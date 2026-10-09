#!/usr/bin/env python3
"""Run the Tonto test suites and print a per-suite *agreement* report.

`ctest` gives a flat pass/fail list.  This driver instead groups the tests by
suite (short, rgbi, long, cx), prints a header above each section, and shows —
in the last columns — how closely each test's output matches its reference
under three criteria:

    exact    byte-for-byte numeric agreement (every printed digit identical)
    lastdig  within +/- K units of the last printed decimal place (default 2),
             for numbers quoted to low precision
    loose    within the relative tolerance (default 0.2%) OR the last-digit
             tolerance -- this is the verdict that decides pass/fail

They are reported in that order, so `loose`, being the combination of the
other two, sits rightmost of the three verdict columns.

The three criteria and their tolerances are exactly those of
`scripts/test.py`; this script simply runs test.py per test, parses the
`AGREEMENT ...` line(s) it prints, and tabulates them by suite.  A test with
several compared output files is scored on its worst file.

Usage
-----
    python3 scripts/suite_report.py --build-dir build
    python3 scripts/suite_report.py -d build-rel --suites short rgbi
    python3 scripts/suite_report.py --rel-tol 1e-3 --last-digit-tol 1

Tolerances (mirror scripts/test.py):
    --rel-tol         loose RELATIVE tolerance   (fraction; default 2e-3 = 0.2%)
    --last-digit-tol  loose LAST-DIGIT tolerance  (units of last place; default 2)
    --abs-tol         absolute near-zero floor    (default 1e-7)

After the suites it runs the SELF-CHECKS: every ctest labelled "selfcheck" in
tests/CMakeLists.txt. They pass or fail on their own, against a known answer or
a rule, and need no stored output. A test or check that cannot run exits 77 and
is reported as SKIP, outside the totals; in CI
pass --skips-are-errors so that a skip fails the run unless it is named by
--allow-skip, because a silent skip is how a check stops being checked.
"""

import argparse
import os
import re
import shlex
import subprocess
import sys

SUITES = ['short', 'hart', 'rgbi', 'long', 'cx']

# Tests with known runner-sensitive numerics that pass the standard loose gate on
# most CPUs but sit close enough to the boundary that a different runner (BLAS /
# eigensolver ordering, FP reassociation) can flip the verdict. Give just these a
# documented wider loose bound so CI does not flicker; the strict gate stays for
# every other test. This is a WORKAROUND, not a fix -- the aim is to remove entries
# by understanding each discrepancy. See TASKS_AND_HISTORY.md "small numerical
# differences". Keys are the test-dir basename.
# Single source of truth: the table lives in test.py, because ctest reaches the
# comparison through test.py with the DEFAULT tolerances and never through this
# file. Keeping a second copy here is how the two paths drift apart.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from test import KNOWN_MARGINAL  # noqa: E402

# "Could not run", as distinct from "ran and disagreed" -- scripts/test.py exits
# with this when a declared input is absent (the pHAR asset, say). CMake pairs it
# with SKIP_RETURN_CODE; this driver runs test.py directly, so it must know it too.
SKIP_EXIT_CODE = 77

# Parse a test.py "AGREEMENT ..." line, e.g.
#   AGREEMENT h2o_rhf_STO-3G   exact=PASS  rel<=0.2%=PASS(max  0%)  \
#             lastdig<=2=PASS(max  0 ulp)  =>  LOOSE=PASS
_ROW = re.compile(
    r'exact=(?P<exact>\w+).*?'
    r'rel<=\S+?=(?P<rel>\w+)\(max\s*(?P<maxrel>[\d.eE+-]+)\s*%\).*?'
    r'lastdig<=\S+?=(?P<ld>\w+)\(max\s*(?P<maxulp>[\d.eE+-]+)\s*ulp\).*?'
    r'LOOSE=(?P<loose>\w+)')


# The job's own CPU accounting, printed by test.py (see job_cpu_seconds there).
# Wall-clock moves with machine load; this does not, so it is what makes two
# suite runs on differently-loaded machines comparable.
_CPU = re.compile(r'^CPUTIME\s+([\d.eE+-]+)')


class _Tee:
    """Write to several streams at once — used to mirror the report to stdout and
    a log file simultaneously.

    Every write is flushed immediately.  A suite takes minutes and each test
    contributes one short line, so without this the report sits in an 8 KB
    buffer and appears all at once when the run ends — which reads exactly like
    a hang.  The cost is nothing: the report is a few dozen lines in total."""
    def __init__(self, *streams):
        self._streams = streams
    def write(self, s):
        for st in self._streams:
            st.write(s)
            st.flush()
    def flush(self):
        for st in self._streams:
            st.flush()


def _save_failure(test_dir, cmd, p, args):
    """Write everything known about a job that failed to run.

    One file per failing test under --failure-dir: the exact command, the exit
    status, and both streams. Nothing is truncated here -- a reader can tail it,
    but a file that was never written cannot be un-truncated.
    """
    d = getattr(args, 'failure_dir', None)
    if not d:
        return
    try:
        os.makedirs(d, exist_ok=True)
        name = os.path.basename(test_dir.rstrip('/')) or 'unnamed'
        with open(os.path.join(d, name + '.log'), 'w') as f:
            f.write('test:    %s\n' % test_dir)
            f.write('command: %s\n' % ' '.join(shlex.quote(c) for c in cmd))
            f.write('exit:    %d\n\n' % p.returncode)
            f.write('----- stdout -----\n%s\n' % (p.stdout or '(empty)'))
            f.write('----- stderr -----\n%s\n' % (p.stderr or '(empty)'))
    except OSError as e:
        # Never let diagnostics break the run that produced them.
        print('  (could not write failure log for %s: %s)' % (test_dir, e))


def score_test(test_py, test_dir, args):
    """Run test.py on one test dir; return an aggregated verdict dict."""
    # per-test tolerance overrides for known runner-sensitive tests
    ov = KNOWN_MARGINAL.get(os.path.basename(test_dir.rstrip('/')), {})
    rel_tol = ov.get('rel_tol', args.rel_tol)
    ld_tol  = ov.get('last_digit_tol', args.last_digit_tol)
    cmd = ['python3', test_py,
           '--test-directory', test_dir,
           '--basis-sets', args.basis_sets,
           '--build-dir', args.build_dir,
           '--log-level=ERROR',
           '--rel-tol', repr(rel_tol),
           '--last-digit-tol', repr(ld_tol),
           '--abs-tol', repr(args.abs_tol)]
    # Without this, `make report` against an MPI build silently ran every job
    # single-rank -- the report looked like an MPI result and was not one.
    if args.mpi:
        cmd += ['--mpi',
                '--mpi-ranks', str(args.mpi_ranks),
                '--mpi-launcher', args.mpi_launcher]
    p = subprocess.run(cmd, capture_output=True, text=True)
    rows = [m for m in (_ROW.search(l) for l in p.stdout.splitlines()
                        if l.startswith('AGREEMENT')) if m]
    cpus = [float(m.group(1)) for m in (_CPU.match(l) for l in p.stdout.splitlines()) if m]
    cpu = sum(cpus) if cpus else None
    if not rows:
        # No comparison happened. Either the test declined to run (SKIP_EXIT_CODE,
        # e.g. a missing large asset), which is not a defect and must not be scored
        # as one, or the job crashed / produced no output, which is.
        if p.returncode == SKIP_EXIT_CODE:
            reason = next((l for l in p.stdout.splitlines()
                           if l.startswith('SKIPPED:')), 'no reason given')
            return {'status': 'SKIP', 'reason': reason.partition('--')[2].strip()
                                                or reason, 'rc': p.returncode,
                    'cpu': cpu}
        status = 'ERROR' if p.returncode != 0 else 'PASS'
        # KEEP THE REASON. This output is already captured above and used to be
        # thrown away here, so an ERROR row said only "ERROR ERROR ERROR - -" and
        # the cause existed nowhere -- not in the log, not in the artefacts (a
        # crashed job writes no .bad). On 2026-08-26 that cost a full CI cycle:
        # eleven MPI tests errored and nothing recorded why, so the failure had
        # to be attributed by test NAME, which is guessing.
        if status == 'ERROR':
            _save_failure(test_dir, cmd, p, args)
        return {'status': status, 'exact': p.returncode == 0,
                'rel': p.returncode == 0, 'ld': p.returncode == 0,
                'loose': p.returncode == 0, 'max_rel': 0.0, 'max_ulp': 0.0,
                'rc': p.returncode, 'cpu': cpu}

    def worst(field):
        return all(m.group(field) == 'PASS' for m in rows)
    return {'status': 'PASS' if p.returncode == 0 else 'FAIL',
            'exact': worst('exact'), 'rel': worst('rel'), 'ld': worst('ld'),
            'loose': p.returncode == 0,
            'max_rel': max(float(m.group('maxrel')) for m in rows),
            'max_ulp': max(float(m.group('maxulp')) for m in rows),
            'rc': p.returncode, 'cpu': cpu}


def yn(ok):
    return 'PASS' if ok else 'FAIL'


_CTEST_NAME = re.compile(r'Test\s+#\d+: (\S+)')


def selfcheck_names(build_dir):
    """The ctests labelled selfcheck in "build_dir", in registration order."""
    p = subprocess.run(['ctest', '-N', '-L', 'selfcheck'], cwd=build_dir,
                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                       universal_newlines=True)
    return [m.group(1) for m in map(_CTEST_NAME.search, p.stdout.splitlines()) if m]


def run_selfcheck(build_dir, name):
    """Run one ctest by its exact "name"; return ('PASS'|'FAIL'|'SKIP', output
    lines). ctest -V prefixes each line of the test's own output with "N: "."""
    p = subprocess.run(['ctest', '-V', '-R', '^%s$' % re.escape(name)], cwd=build_dir,
                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                       universal_newlines=True)
    lines = [re.sub(r'^\d+: ', '', l) for l in p.stdout.splitlines()]
    if any('***Skipped' in l for l in lines):
        return 'SKIP', lines
    if p.returncode == 0 and not any('No tests were found' in l for l in lines):
        return 'PASS', lines
    return 'FAIL', lines


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    root = os.path.dirname(here)
    ap = argparse.ArgumentParser(
        description='Per-suite agreement report for the Tonto test jobs.')
    ap.add_argument('--build-dir', '-d', default=os.path.join(root, 'build'),
                    help='build directory holding tonto, hart and rgbi '
                         '(default: build/)')
    ap.add_argument('--program', '-p', default=None, help=argparse.SUPPRESS)
    ap.add_argument('--tests-dir', '-t', default=os.path.join(root, 'tests'),
                    help='root tests directory (default: tests/)')
    ap.add_argument('--basis-sets', '-b', default=os.path.join(root, 'basis_sets'),
                    help='basis sets directory')
    ap.add_argument('--suites', '-s', nargs='+', default=SUITES, choices=SUITES,
                    help='which suites to run (default: all)')
    ap.add_argument('--rel-tol', type=float, default=2e-3,
                    help='loose RELATIVE tolerance (fraction; default 2e-3 = 0.2%%)')
    ap.add_argument('--last-digit-tol', type=float, default=2.0,
                    help='loose LAST-DIGIT tolerance (units of last place; default 2)')
    ap.add_argument('--abs-tol', type=float, default=1e-7,
                    help='absolute near-zero floor (default 1e-7)')
    ap.add_argument('--mpi', '-m', action='store_true',
                    help='run every job under the MPI launcher (see --mpi-ranks). '
                         'Without this, a report against an MPI-built binary runs '
                         'single-rank and is not an MPI result.')
    ap.add_argument('--mpi-ranks', type=int, default=4,
                    help='MPI ranks per job when --mpi is given (default 4). Sweep '
                         'this: reduction order is rank-count dependent, and -n 1 '
                         'is the control that isolates MPI-build effects from it.')
    ap.add_argument('--mpi-launcher', default='mpirun',
                    help='MPI launcher for --mpi (default mpirun). Must come from '
                         'the same MPI installation the binary was linked against.')
    ap.add_argument('--log', default='tests.log',
                    help='also write the report to this file (default: tests.log in '
                         'the current directory)')
    ap.add_argument('--no-log', action='store_true',
                    help='print to stdout only; do not write a log file')
    ap.add_argument('--failure-dir', default=None,
                    help='write one log per ERRORing test here (command, exit '
                         'status, stdout, stderr). A crashed job produces no .bad '
                         'file, so without this its cause is recorded nowhere.')
    ap.add_argument('--no-selfchecks', action='store_true',
                    help='do not run the self-checks (the ctests labelled selfcheck) '
                         'after the suites')
    ap.add_argument('--skips-are-errors', action='store_true',
                    help='a test or self-check that declines to run (exit 77) '
                         'fails the run unless named by --allow-skip. For CI: on a '
                         'runner everything declared should run.')
    ap.add_argument('--allow-skip', action='append', default=[], metavar='NAME',
                    help='a test directory or a self-check name whose skip is '
                         'expected (repeatable), e.g. '
                         'ammonium_borane_pHAR_C23 when its 167 MB asset is not fetched')
    args = ap.parse_args()

    # Resolve every path to an absolute one *before* anything runs. The
    # test scripts chdir into a scratch work directory, so a relative
    # --basis-sets would be resolved against that directory and silently fail
    # ("could not read the energies"). This is not hypothetical: CI passes
    # `--basis-sets basis_sets` relative, which broke every check the
    # moment they started running from this driver, while local runs and the
    # CMake `report` target -- both of which pass absolute paths -- stayed green.
    # test.py guards the same way for the same reason.
    if args.program is not None:
        ap.error('--program is now --build-dir DIR: the directory holding '
                 'tonto (and hart, rgbi), not the program')
    args.build_dir = os.path.abspath(args.build_dir)
    args.program = os.path.join(args.build_dir, 'tonto')
    args.basis_sets = os.path.abspath(args.basis_sets)
    args.tests_dir = os.path.abspath(args.tests_dir)
    # Absolutised for the same reason as the others: the checks chdir
    # into a scratch directory, so a relative --failure-dir would scatter logs
    # into it and the artefact upload would find nothing.
    if args.failure_dir:
        args.failure_dir = os.path.abspath(args.failure_dir)
    if not os.path.exists(args.program):
        sys.exit('error: program not found: %s' % args.program)
    test_py = os.path.join(here, 'test.py')

    # By default mirror the whole report into tests.log as well as stdout, so a
    # plain run leaves a log behind (matches `ctest >& tests.log` muscle memory).
    logf = None
    if not args.no_log:
        logf = open(args.log, 'w')
        sys.stdout = _Tee(sys.__stdout__, logf)

    relpct = args.rel_tol * 100
    ldk = args.last_digit_tol

    # The name column is sized to the longest name actually being reported,
    # across every suite in this run, so the table is as narrow as the content
    # allows and stays the same width from one suite to the next. Clamped: a
    # very short suite should not collapse the column, and one pathological
    # name should not push the numbers off a terminal.
    _names = [d for suite in args.suites
              for d in (os.listdir(os.path.join(args.tests_dir, suite))
                        if os.path.isdir(os.path.join(args.tests_dir, suite)) else [])]
    NAMEW = max(30, min(54, max((len(n) for n in _names), default=30)))

    # Column order: exact, lastdig, then loose LAST -- loose is the OR of the
    # other two, so it reads naturally as the rightmost of the three verdicts.
    # Verdicts are left-aligned under left-aligned headings, numbers are right
    # aligned under right-aligned ones, so every heading sits over its column.
    COLUMNS = [('test name', NAMEW, 'L'), ('exact', 5, 'L'), ('lastdig', 7, 'L'),
               ('loose', 5, 'L'), ('max rel%', 9, 'R'), ('max LDD', 8, 'R'),
               ('cpu s', 9, 'R')]
    GAP = '  '

    def cells(specs, values):
        """Format values into their columns. One place, so a column added later
        cannot leave a print site behind -- which is how this table came to
        have separators of two different widths."""
        return [('%-*s' if a == 'L' else '%*s') % (w, str(v)[:w])
                for (_, w, a), v in zip(specs, values)]

    def row(values):
        return GAP.join(cells(COLUMNS, values))

    hdr = row([c[0] for c in COLUMNS])
    rule = GAP.join('-' * w for _, w, _ in COLUMNS)
    WIDTH = len(rule)
    grand = {'n': 0, 'exact': 0, 'loose': 0, 'ld': 0, 'err': 0, 'skip': 0}
    widened = []   # known-marginal tests run with a relaxed bound (reported below)
    skipped = []   # (test, reason) for tests that declined to run (reported below)

    print('')
    print('=================================')
    print('Tonto test-suite agreement report')
    print('=================================')
    print('')
    print('Testing program : %s' % args.program)
    print('')
    print('There are three types of agreement:')
    print('. exact   = every digit identical')
    print('. lastdig = within %g units of last-digit place' % (ldk))
    print('. loose   = within %.3g%% OR lastdig' % (relpct))
    print('')
    print('Compared to the reference, we also report:')
    print('. the maximum relative % disagreement (max rel%)')
    print('. the maximim last digit difference   (max LDD )')
    print('')
    print('cpu s is the job\'s OWN reported CPU time, not wall-clock, so two')
    print('runs on differently-loaded machines stay comparable.')

    for suite in args.suites:
        sdir = os.path.join(args.tests_dir, suite)
        if not os.path.isdir(sdir):
            continue
        # A directory is a test if it has a "stdin" (a tonto job file) or an
        # "IO" manifest (which is how an argv-driven job e.g. hart declares
        # its program, arguments and outputs -- it has no stdin at all).
        tests = sorted(d for d in os.listdir(sdir)
                       if os.path.isfile(os.path.join(sdir, d, 'stdin'))
                       or os.path.isfile(os.path.join(sdir, d, 'IO')))
        print('')
        print('SUITE: %s (%d tests)' % (suite, len(tests)))
        print('')
        print(hdr)
        print(rule)
        sub = {'n': 0, 'exact': 0, 'loose': 0, 'ld': 0, 'err': 0, 'skip': 0}
        for t in tests:
            # Print the name *before* running the test, and only then its
            # verdict columns, so a slow test shows as a visibly pending line
            # instead of silence.  Some tests here run for minutes.
            print(cells(COLUMNS[:1], [t])[0] + GAP, end='', flush=True)
            r = score_test(test_py, os.path.join(sdir, t), args)
            # A skipped test is NOT in the denominator: it was never run, so
            # scoring it either way would misreport the build. It is counted
            # and its reason printed below, so the drop in the total is never
            # silent.
            if r['status'] == 'SKIP':
                sub['skip'] += 1
                skipped.append((t, r['reason']))
                print(GAP.join(cells(COLUMNS[1:],
                      ['SKIP', 'SKIP', 'SKIP', '-', '-', '-'])))
                continue
            sub['n'] += 1
            if t in KNOWN_MARGINAL:
                widened.append(t)
            if r['status'] == 'ERROR':
                sub['err'] += 1
                print(GAP.join(cells(COLUMNS[1:],
                      ['ERROR', 'ERROR', 'ERROR', '-', '-',
                       '-' if r.get('cpu') is None else '%.3f' % r['cpu']])))
                continue
            sub['exact'] += r['exact']
            sub['loose'] += r['loose']
            sub['ld'] += r['ld']
            print(GAP.join(cells(COLUMNS[1:],
                  [yn(r['exact']), yn(r['ld']), yn(r['loose']),
                   '%.3g' % r['max_rel'], '%.3g' % r['max_ulp'],
                   '-' if r.get('cpu') is None else '%.3f' % r['cpu']])))
        print(rule)
        print('%s subtotal:  loose %d/%d   (exact %d, lastdig %d%s%s)'
              % (suite, sub['loose'], sub['n'], sub['exact'], sub['ld'],
                 ', ERROR %d' % sub['err'] if sub['err'] else '',
                 ', SKIPPED %d' % sub['skip'] if sub['skip'] else ''))
        for k in grand:
            grand[k] += sub[k]

    print('')
    print('=' * WIDTH)
    print('GRAND TOTAL:  loose %d/%d   (exact %d, lastdig %d%s%s)'
          % (grand['loose'], grand['n'], grand['exact'], grand['ld'],
             ', ERROR %d' % grand['err'] if grand['err'] else '',
             ', SKIPPED %d' % grand['skip'] if grand['skip'] else ''))
    print('=' * WIDTH)
    if skipped:
        print('\nSkipped -- these tests declined to run and are NOT in the totals '
              'above:')
        for t, why in skipped:
            print('  * %-48s %s' % (t, why))
    skips_ok = True
    if args.skips_are_errors:
        unexpected = [t for t, _ in skipped if t not in args.allow_skip]
        if unexpected:
            skips_ok = False
            print('\nERROR: %d skipped test(s) not named by --allow-skip: %s'
                  % (len(unexpected), ', '.join(unexpected)))
    if widened:
        print('\nNote: relaxed loose bound applied to known runner-sensitive tests '
              '(workaround; see TASKS_AND_HISTORY.md "small numerical differences"):')
        for t in widened:
            print('  * %-48s %s' % (t, ', '.join('%s=%g' % kv
                                    for kv in KNOWN_MARGINAL[t].items())))
    # ------------------------------------------------------------------
    # Self-checks: every ctest labelled "selfcheck" in tests/CMakeLists.txt,
    # which is their one list. They pass or fail on their own, against a known
    # answer or a rule, so no stored output is involved and a broken build
    # cannot be blessed into passing them. They run whatever --suites says.
    # ------------------------------------------------------------------
    checks_ok = True
    if not args.no_selfchecks:
        print('')
        print('SELF-CHECKS (pass or fail on their own; no stored output)')
        print('')
        names = selfcheck_names(args.build_dir)
        if not names:
            checks_ok = False
            print('ERROR: no ctest labelled selfcheck in %s' % args.build_dir)
        CHKW = max((len(n) for n in names), default=0)
        for name in names:
            status, output = run_selfcheck(args.build_dir, name)
            if status == 'SKIP':
                why = next((l.split(':', 1)[1].strip() for l in output
                            if l.startswith(('SKIPPED:', 'SKIP:'))), '')
                print('%-*s  %s' % (CHKW, name, 'SKIP' + (' -- ' + why if why else '')))
                if args.skips_are_errors and name not in args.allow_skip:
                    checks_ok = False
                    print('    ERROR: a check that cannot run is an error under '
                          '--skips-are-errors (allow it with --allow-skip %s)' % name)
                continue
            ok = (status == 'PASS')
            print('%-*s  %s' % (CHKW, name, yn(ok)))
            if not ok:
                checks_ok = False
                for line in output[-40:]:
                    print('    %s' % line)

    if logf:
        print('\n(report written to %s)' % os.path.abspath(args.log))
        sys.stdout = sys.__stdout__
        logf.close()
    # Exit non-zero if any test failed the loose (pass-deciding) criterion, if
    # a self-check failed, or if a skip was an error.
    sys.exit(0 if (grand['loose'] == grand['n'] and checks_ok and skips_ok) else 1)


if __name__ == '__main__':
    main()
