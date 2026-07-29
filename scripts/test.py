#!/usr/bin/env python3
import logging
from tempfile import gettempdir
import getpass
from getpass import getuser
import os
from os.path import abspath, join
from itertools import zip_longest
import sys
import shutil
import subprocess
import difflib
import datetime
import time
import re

log = logging.getLogger('test')

prefixes_to_ignore = [
    'Wall-clock', 'CPU time', 
    'Version', 'Platform', 'Timer', 'Build-date',
    # Build provenance stamped into the banner (CMakeLists.txt -> macros.in ->
    # molecule.main.foo). Deliberately ignored: it legitimately differs between
    # machines, and the whole point is that it be visible in stdout without
    # breaking reference comparisons.
    'Compiler', 'LAPACK',
    'Warning', 'https', 'www', 'Peter', 'Daniel', 'Dylan',
    'WARNINGS', 'Look above', 'time taken for',
    '_audit_creation_date', 
    '_audit_creation_method', 
    '_QCr_software_version',
    '_QCr_software_platform',
    '_QCr_software_build_date'
    ]

suffixes_to_ignore = [ '---', '___', '===' ]

test_categories = ['short', 'cx', 'long', 'geminal', 'relativistic']

def is_junk(line):
    return (any(map(line.startswith, prefixes_to_ignore)) or
            any(map(line.startswith, suffixes_to_ignore)) or
            line.strip() == '')


def isclose(a, b, abs_tol=0.0, rel_tol=0.0):
    return (abs(a-b) <= max(rel_tol * max(abs(a), abs(b)), abs_tol))

def get_lines(filename):
    """Read all lines from a file, returning a
    list of line contents

    Arguments:
    filename -- name/path of the file to read
    """
    lines = []
    with open(filename) as f:
        lines = []
        junk_lines = []
        for i, line in enumerate(f):
            if not is_junk(line.strip()):
                lines.append(line)
            else:
                junk_lines.append(i)
    log.debug('Ignored junk lines: %s', junk_lines)
    return lines


def is_float(s):
    """Check if a string may be converted into a float

    Arguments:
    s -- string to be converted"""
    try:
        float(s)
        return True
    except ValueError:
        return False

def equivalent(s1, s2, **kwargs):
    if is_float(s1) and is_float(s2):
        return isclose(float(s1), float(s2), **kwargs)
    else:
        return s1 == s2


def num_decimals(s):
    """Number of digits after the decimal point in the number as PRINTED,
    i.e. the place value of its last significant digit (10**-num_decimals).
    Handles a trailing exponent (1.23e-4 -> 6 decimals)."""
    s = s.strip().lstrip('+-')
    m = re.match(r'^\d*\.?(\d*)(?:[eE]([+-]?\d+))?$', s)
    if not m:
        return 0
    dec = len(m.group(1) or '')
    if m.group(2):
        dec -= int(m.group(2))
    return dec


def token_agreement(a_str, b_str, rel_tol, abs_tol, last_digit_tol):
    """Agreement of one numeric token pair (b_str = reference value).
    Returns a dict of verdicts + metrics, or None if the pair is non-numeric.

      exact  : values are exactly equal
      rel_ok : |a-b| <= rel_tol*max(|a|,|b|)          (loose relative, e.g. 0.01%)
      ld_ok  : |a-b| <= last_digit_tol * 10**-d(b)    (a couple of last places)
      loose  : rel_ok OR ld_ok  (OR within abs_tol, for near-zero values)
    """
    if not (is_float(a_str) and is_float(b_str)):
        return None
    a, b = float(a_str), float(b_str)
    diff = abs(a - b)
    denom = max(abs(a), abs(b))
    rel = 0.0 if denom == 0 else diff / denom
    ulp = 10.0 ** (-num_decimals(b_str))
    ulp_dev = 0.0 if diff == 0 else (diff / ulp if ulp > 0 else float('inf'))
    near_zero = diff <= abs_tol
    rel_ok = (diff <= rel_tol * denom) or near_zero
    ld_ok = (diff <= last_digit_tol * ulp) or near_zero
    return {
        'exact': (a == b),
        'rel': rel, 'ulp': ulp_dev,
        'rel_ok': rel_ok, 'ld_ok': ld_ok, 'loose_ok': rel_ok or ld_ok,
    }


def agreement_report(lines1, lines2, rel_tol, abs_tol, last_digit_tol):
    """Compare two filtered line lists across all three criteria at once.
    Pairs +/- lines from difflib.ndiff, then compares them token-by-token.
    Returns a result dict with per-criterion verdicts and worst-case metrics."""
    diff = list(difflib.ndiff(lines1, lines2))
    del1 = [x for x in diff if x.startswith('-')]
    del2 = [x for x in diff if x.startswith('+')]
    res = {
        'exact': True, 'rel_pass': True, 'ld_pass': True, 'loose_pass': True,
        'n_num': 0, 'n_struct': 0,
        'max_rel': 0.0, 'max_ulp': 0.0, 'worst_rel': None, 'worst_ulp': None,
        'diff_text': ''.join(a + b for a, b in zip(del1, del2)),
    }
    for l1, l2 in zip_longest(del1, del2, fillvalue=''):
        t1 = l1.strip('+- ').split()
        t2 = l2.strip('+- ').split()
        if len(t1) != len(t2):
            # differing token counts: a structural/alignment mismatch, not a
            # numeric-tolerance question. Fails every criterion.
            res['exact'] = res['rel_pass'] = res['ld_pass'] = res['loose_pass'] = False
            res['n_struct'] += 1
            continue
        for a, b in zip(t1, t2):
            ag = token_agreement(a, b, rel_tol, abs_tol, last_digit_tol)
            if ag is None:                       # non-numeric tokens must match
                if a != b:
                    res['exact'] = res['rel_pass'] = res['ld_pass'] = res['loose_pass'] = False
                    res['n_struct'] += 1
                continue
            res['n_num'] += 1
            if not ag['exact']:   res['exact'] = False
            if not ag['rel_ok']:  res['rel_pass'] = False
            if not ag['ld_ok']:   res['ld_pass'] = False
            if not ag['loose_ok']: res['loose_pass'] = False
            if ag['rel'] > res['max_rel']:
                res['max_rel'], res['worst_rel'] = ag['rel'], (a, b)
            if ag['ulp'] > res['max_ulp']:
                res['max_ulp'], res['worst_ulp'] = ag['ulp'], (a, b)
    return res


def format_agreement(name, res, rel_tol, last_digit_tol):
    """A single self-describing columnar line summarising the degree of
    agreement under each criterion, plus optional detail notes."""
    yn = lambda ok: 'PASS' if ok else 'FAIL'
    row = ('AGREEMENT %-44s  exact=%-4s  rel<=%.3g%%=%-4s(max %7.3g%%)  '
           'lastdig<=%g=%-4s(max %7.3g ulp)  =>  LOOSE=%-4s'
           % (name[:44], yn(res['exact']),
              rel_tol * 100, yn(res['rel_pass']), res['max_rel'] * 100,
              last_digit_tol, yn(res['ld_pass']), res['max_ulp'],
              yn(res['loose_pass'])))
    notes = ''
    if not res['exact'] and res['worst_rel']:
        notes += ('    worst relative : %s vs %s  (%.3g%%)\n'
                  % (res['worst_rel'][1], res['worst_rel'][0], res['max_rel'] * 100))
    if not res['exact'] and res['worst_ulp']:
        notes += ('    worst last-digit: %s vs %s  (%.3g ulp)\n'
                  % (res['worst_ulp'][1], res['worst_ulp'][0], res['max_ulp']))
    if res['n_struct']:
        notes += ('    %d structural/alignment mismatch(es) (non-numeric or token-count) '
                  '-- separate from numeric tolerance\n' % res['n_struct'])
    return row, notes


def diff_sbf(file1, file2, args): 
    """Find the differences between 2 cxs files using sbftool
    """
    verbosity = 1
    retcode = subprocess.check_call([args.sbftool, '-vc', file1, file2])
    log.debug('sbftool returned: %s', retcode)
    return (retcode == 0)


def is_sbf(filename):
    """Check if a file is a SBF by reading the header"""
    with open(filename, 'rb') as f:
        if f.read(3) == b'SBF':
            return True
    return False

def diff_files(file1, file2, args, print_diffs=True):
    """Compare two output files under all three agreement criteria (exact,
    loose-relative, loose-last-digit), print a columnar agreement report, and
    return the LOOSE verdict (which drives the pass/exit status). SBF files are
    delegated to sbftool as a single binary verdict."""
    name = os.path.basename(args.test_directory.rstrip('/')) or os.path.basename(file2)
    if is_sbf(file1) and is_sbf(file2):
        log.debug('Diffing with sbftool')
        ok = diff_sbf(file1, file2, args)
        res = {'exact': ok, 'rel_pass': ok, 'ld_pass': ok, 'loose_pass': ok,
               'n_num': 0, 'n_struct': 0 if ok else 1,
               'max_rel': 0.0, 'max_ulp': 0.0, 'worst_rel': None, 'worst_ulp': None,
               'diff_text': ''}
    else:
        lines1 = get_lines(file1)
        lines2 = get_lines(file2)
        res = agreement_report(lines1, lines2, args.rel_tol, args.abs_tol,
                               args.last_digit_tol)
        if print_diffs and res['diff_text']:
            log.info('Diff:\n%s', res['diff_text'])

    row, notes = format_agreement(name, res, args.rel_tol, args.last_digit_tol)
    sys.stdout.write(row + '\n')
    if notes:
        sys.stdout.write(notes)
    sys.stdout.flush()

    if not res['loose_pass']:
        log.debug('Found (loose) differences in %s and %s', file1, file2)
    return res['loose_pass']


class working_directory:
    """ Context manager for temporarily changing the current working directory. """
    old_directory = None

    def __init__(self, directory, create=False):
        if directory:
            self.directory = os.path.expanduser(directory)
            if not os.path.exists(self.directory) and create:
                os.makedirs(self.directory)
        else:
            self.directory = None


    def __enter__(self):
        self.old_directory = os.getcwd()
        if self.directory:
            log.debug('cwd: %s', self.directory)
            os.chdir(self.directory)

    def __exit__(self, exc_type, exc_val, exc_tb):
        os.chdir(self.old_directory)

def temp_test_dir(testname, subdir='tonto-tests'):
    d = join(gettempdir(), subdir + '-' + getpass.getuser())
    name = join(d, testname)
    log.debug('temp_test_dir = %s', name)
    return name

def parse_IO_file(path):
    io_files = {
        'input': set(['stdin']),
        'output': set(['stdout']),
        'delete': set(),
    }

    if os.path.exists(path):
        with open(path) as f:
            for line in f:
                tokens = line.split(':')
                io_files[tokens[0].strip()].add(tokens[1].strip())
    return io_files


def compare_outputs(f1, f2, args):
    if args.compare_program:
        return (subprocess.check_call([args.compare_program, f1, f2]) == 0)
    else:
        log.debug('Using builtin diffing or sbftool')
        d = diff_files(f1, f2, args)
        log.debug('diff_files returned: %s', d)
        return d


def run_test(args, test_dir, io_files):
    env = dict(os.environ)
    env['TONTO_BASIS_SET_DIRECTORY'] = args.basis_sets
    kwargs = {
        'shell': False,
        'universal_newlines': True,
        'env': env,
    }
    if args.mpi:
        prog = ['mpirun', '-n', '4', args.program]
    else:
        prog = [args.program]

    timings = {}
    exec_dir = temp_test_dir(os.path.basename(test_dir.rstrip('/')))
    timings['start'] = time.time() 
    with working_directory(exec_dir, create=True):
        for path in io_files['input']:
            shutil.copy(abspath(join(test_dir, path)), '.')
        timings['cp_input'] = time.time() - timings['start']

        log.debug('Running program %s', ' '.join(prog))
        retcode = subprocess.check_call(prog, **kwargs)
        completed = (retcode == 0)

        timings['tonto'] = time.time() - sum(t for t in timings.values())
        files_equivalent = []

        if completed:
            log.debug('Outputs to check %s', io_files['output'])

            for path in io_files['output']:
                canonical = abspath(join(test_dir, path))
                log.debug('Comparing %s to %s', path, canonical)
                d = compare_outputs(canonical, path, args)
                log.debug('Same file: %s', d)
                files_equivalent.append(d)
        timings['diffs'] = time.time() - sum(t for t in timings.values())
        success = completed and all(files_equivalent)

        for path, equivalent in zip(io_files['output'], files_equivalent):
            log.debug('%s: %s', path, 'GOOD' if equivalent else 'BAD')
            if equivalent:
                """ shutil.copy(abspath(join('.', path)),
                        abspath(join(test_dir, path + '.good')))
                """
            else:
                shutil.copy(abspath(join('.', path)),
                        abspath(join(test_dir, path + '.bad')))
        timings['cp_output'] = time.time() - sum(t for t in timings.values())
        log.debug('Time spent:')
        for k, v in timings.items():
            if k != 'start':
                log.debug('%s: \t %f s', k, v)
    return success

def main():
    """Show the differences between two test files
    """
    import argparse
    import os
    parser = argparse.ArgumentParser()
    parser.add_argument('--program', '-p', default='./tonto',
                        help='Program to use to run the test jobs i.e. tonto')
    parser.add_argument('--test-directory', '-t', default='.',
                        help='Directory in which tests are located')
    parser.add_argument('--compare-program', '-c', default=None,
                        help='diff style program to compare outputs')
    parser.add_argument('--log-level', default='ERROR',
                        help='Log level for running tests')
    parser.add_argument('--basis-sets', '-b', default='.',
                        help='Basis sets directory')
    parser.add_argument('--sbftool', default='../../external/sbf/src/sbftool',
                        help='Location of sbftool')
    parser.add_argument('--mpi', '-m', default=False, action='store_true',
                        help='Test with mpirun')
    parser.add_argument('--abs-tol', type=float, default=1e-7,
                        help='Absolute tolerance (near-zero floor) for numerical differences')
    parser.add_argument('--rel-tol', type=float, default=2e-3,
                        help='Loose RELATIVE tolerance (fraction; default 2e-3 = 0.2%%)')
    parser.add_argument('--last-digit-tol', type=float, default=2.0,
                        help='Loose LAST-DIGIT tolerance: allowed units of the '
                             'last printed decimal place (default 2). A number '
                             'passes loose if it is within rel-tol OR last-digit-tol.')
    args = parser.parse_args()
    # Resolve all paths to absolute up front: run_test() chdir's into a temp
    # directory before copying inputs / reading the basis sets, so a *relative*
    # --test-directory or --basis-sets would be resolved against the temp dir
    # and vanish (doubling the path). Must be absolutised while cwd is still the
    # invocation dir.
    args.sbftool = os.path.abspath(args.sbftool)
    args.test_directory = os.path.abspath(args.test_directory)
    args.basis_sets = os.path.abspath(args.basis_sets)
    logging.basicConfig(level=args.log_level)
    io_files = parse_IO_file(join(args.test_directory,'IO'))
    if run_test(args, args.test_directory, io_files):
        sys.exit(0)
    else:
        sys.exit(1)

if __name__ == '__main__':
    main()
