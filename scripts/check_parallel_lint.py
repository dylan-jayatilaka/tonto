#!/usr/bin/env python3
"""Invariant test: two MPI hazards that are invisible on inspection.

Both come out of milestone 4 (see docs/MPI.md and DEFERRED.md). Each is a
silent wrong answer or a hang, and neither is visible in a serial run -- which
is every run anyone does day to day.

CHECK 1 -- a collective inside a `parallel do` body.

  The translator emits LOCK_PARALLEL_DO as the first statement *inside* a
  `parallel do`, and DO_IN_PARALLEL is false while that lock is held. Every
  reduction macro is gated on DO_IN_PARALLEL, so a PARALLEL_SUM written in the
  loop body is DEAD CODE THAT LOOKS CORRECT. Four such sites in
  molecule.grid.foo each returned 1/n_ranks of the answer.

  Broadcasts and barriers fail differently and worse: they are gated on
  is_parallel alone, so they DO execute -- but the surrounding loop body runs a
  different number of times on different ranks, so the ranks enter a different
  number of collectives and the job hangs or gives MPI_ERR_TRUNCATE.

  The intent -- "MPI on the outside", no interior collectives -- is right and
  standard. The enforcement was invisible. This is the enforcement.

CHECK 2 -- a raw write/read on a `.unit` outside the I/O layer.

  FILE and TEXTFILE guard their I/O with IO_IS_ALLOWED so only the master
  writes. Code that reaches around them with `write(x.unit,...)` does not. The
  loud version of this crashed DWGN_lamaGOET_NBO_file_47 at >=2 ranks on a
  negative unit number. The SILENT version is worse: an unguarded write to a
  non-redirected stdout uses preconnected unit 6, which is valid on every rank,
  so it interleaves output from all ranks instead of failing. Inspection cannot
  find those; this can.

Caveat, stated plainly: this is a heuristic over source text, not a parse. It
keys on Foo's three-space indentation to find block extents, so unusual
formatting could fool it either way. Use the ALLOWED tables below if a
legitimate case appears -- with a reason you would defend in review.

  usage:  python3 check_parallel_lint.py [foofiles-dir]

Exits 0 if clean, 1 on any violation.
"""

import os
import re
import sys

# Collectives. Reductions are gated on DO_IN_PARALLEL and so are silently
# skipped inside a parallel do; broadcasts and barriers are gated on
# is_parallel and so desynchronise instead. Both are wrong in a loop body.
COLLECTIVES = re.compile(
    r'\b(PARALLEL_SUM|PARALLEL_VECTOR_SUM|PARALLEL_SYMMETRIC_SUM'
    r'|PARALLEL_SYMMETRIC_SUM_23|PARALLEL_BROADCAST|PARALLEL_BARRIER)\b')

PARALLEL_DO = re.compile(r'^(\s*)parallel\s+do\b')

# `write(unit=x.unit,...)`, `write(x.unit,...)`, and the read forms.
RAW_IO = re.compile(r'\b(write|read)\s*\(\s*(unit\s*=\s*)?[A-Za-z_][\w.%]*\.unit\b')

# The I/O layer itself: these files ARE the guard, so they must touch .unit.
IO_LAYER = {'file.foo', 'textfile.foo', 'buffer.foo'}

# What counts as "this is master-only, so a raw write is fine". IO_IS_ALLOWED is
# the macro FILE/TEXTFILE use; is_master_processor is the same test spelled out.
GUARD = re.compile(r'\bif\s*\(.*?(IO_IS_ALLOWED|is_master_processor).*?\)')
GUARD_BLOCK = re.compile(
    r'^(\s*)if\s*\(.*?(IO_IS_ALLOWED|is_master_processor).*?\)\s*then\s*$')

# Collectives allowed inside a parallel do, as "file.foo:line-ish": reason.
ALLOWED_COLLECTIVE = {
    # (none -- the four molecule.grid.foo sites were fixed in milestone 4)
}

# Raw .unit I/O allowed outside the I/O layer, as "file.foo:procedure": reason.
ALLOWED_RAW_IO = {
    # (none)
}


def strip_noncode(line):
    """Remove string literals and any trailing comment."""
    out, quote = [], None
    for ch in line:
        if quote:
            if ch == quote:
                quote = None
        elif ch in '"\'':
            quote = ch
        elif ch == '!':
            break
        else:
            out.append(ch)
    return ''.join(out)


def guarded_lines(lines):
    """Return the set of line numbers that sit under a master-only guard.

    Two shapes, both used in the sources:

        if (IO_IS_ALLOWED) then     <- block; everything more-indented is guarded
           write(stdout.unit,...)
        end

        if (IO_IS_ALLOWED) write(stdout.unit,...)      <- one-liner

    Without this the check reports every already-fixed site, and a lint that
    cries wolf is worse than no lint.
    """
    guarded = set()
    for i, raw in enumerate(lines):
        code = strip_noncode(raw)
        m = GUARD_BLOCK.match(code)
        if m:
            indent = len(m.group(1))
            for j in range(i + 1, len(lines)):
                inner = strip_noncode(lines[j])
                if not inner.strip():
                    continue
                here = len(inner) - len(inner.lstrip())
                if here <= indent:
                    break
                guarded.add(j + 1)
        elif GUARD.search(code):
            guarded.add(i + 1)          # one-liner guards its own line
    return guarded


def parallel_do_bodies(lines):
    """Yield (header_line_no, body_line_no, text) for lines inside a parallel do.

    Foo is indentation-scoped at three spaces, so a `parallel do` body is every
    following line indented further than the header, up to the matching `end`.
    Blank and comment-only lines never close a block.
    """
    for i, raw in enumerate(lines):
        m = PARALLEL_DO.match(strip_noncode(raw))
        if not m:
            continue
        indent = len(m.group(1))
        for j in range(i + 1, len(lines)):
            code = strip_noncode(lines[j])
            if not code.strip():
                continue
            here = len(code) - len(code.lstrip())
            if here <= indent:
                break            # dedented out of the loop body
            yield i + 1, j + 1, code


def main():
    root = sys.argv[1] if len(sys.argv) > 1 else os.path.join(
        os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'foofiles')
    if not os.path.isdir(root):
        print('no such directory: %s' % root, file=sys.stderr)
        return 2

    bad_collective, bad_io = [], []
    n_files = n_loops = 0

    for name in sorted(os.listdir(root)):
        if not name.endswith('.foo'):
            continue
        path = os.path.join(root, name)
        n_files += 1
        with open(path, encoding='utf-8', errors='replace') as f:
            lines = f.read().splitlines()

        seen_headers = set()
        for hdr, lineno, code in parallel_do_bodies(lines):
            seen_headers.add(hdr)
            m = COLLECTIVES.search(code)
            if m and '%s:%d' % (name, lineno) not in ALLOWED_COLLECTIVE:
                bad_collective.append(
                    (name, lineno, hdr, m.group(1), code.strip()))
        n_loops += len(seen_headers)

        if name not in IO_LAYER:
            guarded = guarded_lines(lines)
            for k, raw in enumerate(lines, start=1):
                code = strip_noncode(raw)
                m = RAW_IO.search(code)
                if m and k not in guarded \
                        and '%s:%d' % (name, k) not in ALLOWED_RAW_IO:
                    bad_io.append((name, k, code.strip()))

    status = 0

    if bad_collective:
        status = 1
        print('FAIL: collective inside a `parallel do` body '
              '(%d)' % len(bad_collective))
        print('      A reduction there is silently skipped; a broadcast or '
              'barrier there desynchronises the ranks.')
        print('      For a reduction, use the loop clause instead -- '
              '`parallel do i = 1,n reduce(x)` -- which the translator lowers '
              'to a PARALLEL_SUM after UNLOCK_PARALLEL_DO, the only place it '
              'can be written correctly.')
        for name, lineno, hdr, what, code in bad_collective:
            print('  %s:%d  %s  (loop opened at line %d)'
                  % (name, lineno, what, hdr))
            print('      %s' % code)

    if bad_io:
        status = 1
        print('FAIL: raw write/read on a `.unit` outside the I/O layer '
              '(%d)' % len(bad_io))
        print('      FILE/TEXTFILE guard I/O with IO_IS_ALLOWED; reaching '
              'around them writes from every rank.')
        for name, lineno, code in bad_io:
            print('  %s:%d' % (name, lineno))
            print('      %s' % code)

    status |= check_collective_gates()

    if status == 0:
        print('parallel lint OK: %d files, %d `parallel do` loops, '
              'no interior collectives, no UNGUARDED raw .unit I/O outside %s, '
              'collective gates correct'
              % (n_files, n_loops, '/'.join(sorted(IO_LAYER))))
    return status


# Whether a COLLECTIVE executes must never depend on state that can differ
# between ranks. `is_parallel` is identical everywhere; the parallel-do lock is
# NOT -- it is set by executing a loop body, which a rank given zero iterations
# never does. Gating a broadcast or barrier on the lock therefore lets different
# ranks skip different collectives, offsetting the streams until some later pair
# mismatches (observed: a 1-integer receive against a 256-character send,
# MPI_ERR_TRUNCATE, in four CIF tests -- CLAUDE.md milestone 7).
#
# Reductions are the deliberate exception: they combine rank-partitioned partial
# results, so with the lock held there is nothing to combine and skipping is
# correct. macros.in carries a comment explaining the asymmetry; this check stops
# the two gates being "tidied" back into agreement, which is exactly how the bug
# would return -- and it would return silently, because the shipped -Ofast build
# does not expose it.
GATE_RULES = {
    # macro name          : (must contain, must NOT contain)
    'PARALLEL_BROADCAST0':    ('is_parallel', 'work_is_shared'),
    'PARALLEL_BROADCAST_IO0': ('is_parallel', 'work_is_shared'),
    'PARALLEL_BARRIER0':      ('is_parallel', 'work_is_shared'),
    'PARALLEL_SUM0':             ('work_is_shared', None),
    'PARALLEL_VECTOR_SUM0':      ('work_is_shared', None),
    'PARALLEL_SYMMETRIC_SUM0':   ('work_is_shared', None),
    'PARALLEL_SYMMETRIC_SUM_230':('work_is_shared', None),
}


def check_collective_gates(path=None):
    """Assert the MPI definitions of the collective macros use the right gate."""
    if path is None:
        path = os.path.join(os.path.dirname(os.path.dirname(
            os.path.abspath(__file__))), 'include', 'macros.in')
    bad, seen = [], set()
    for line in open(path, encoding='utf-8', errors='replace'):
        m = re.match(r'#\s*define\s+(PARALLEL_[A-Z_0-9]+)\s*(\([^)]*\))?\s+(.*\S)\s*$', line)
        if not m:
            continue
        name, body = m.group(1), m.group(3).lower()
        if name not in GATE_RULES or 'call ' not in body:
            continue          # the serial block defines these as empty: skip
        seen.add(name)
        need, forbid = GATE_RULES[name]
        if need not in body:
            bad.append((name, 'must be gated on %s' % need, m.group(3)))
        elif forbid and forbid in body:
            bad.append((name, 'must NOT be gated on %s (rank-local state)' % forbid,
                        m.group(3)))
    missing = sorted(set(GATE_RULES) - seen)
    if bad or missing:
        print('FAIL: collective macro gated on the wrong state (%d)' % (len(bad) + len(missing)))
        print('      A collective must not be gated on rank-local state such as the')
        print('      parallel-do lock; a reduction must be. See macros.in.')
        for name, why, body in bad:
            print('  %s: %s' % (name, why))
            print('      %s' % body)
        for name in missing:
            print('  %s: no MPI definition found (renamed? then update this check)' % name)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
