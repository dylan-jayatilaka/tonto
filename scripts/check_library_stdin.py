#!/usr/bin/env python3
"""Invariant test: a Foo procedure that takes arguments must not touch `stdin`.

Tonto has two kinds of procedure that read the job file:

  * a **keyword handler** takes no arguments and reads *all* its operands from
    `stdin`, including the name of the thing to act on. It is reachable only
    from a `select case` in a `process_keyword` routine. This is fine.

  * a **library routine** takes its operands as arguments and is called
    directly, including from the argv-driven drivers in `runfiles/` (`hart`,
    `rgbi`, ...). It must never touch `stdin`: those programs have no job file.

Merging the two roles is what broke `hart`. `MOLECULE.READ:read_archive(name,
genre)` took arguments *and* peeked at `stdin` for a trailing `normalise`
qualifier, so it dereferenced an unallocated TEXTFILE in a program that never
created one -- and in `tonto` it read whatever line the job file happened to be
on. See DEFERRED.md, "Keyword parsing must not leak into library routines".

That was the only instance, so this check starts green. Its job is to keep the
rule true: any *new* procedure that takes arguments and mentions `stdin` fails.

Caveat, stated plainly: this is a heuristic over source text, not a parse. It
keys on Foo's layout -- procedures start at three-space indent, their bodies are
indented further -- so an unusual formatting could fool it either way. Use
ALLOWED below if a legitimate case ever appears, with a reason.

  usage:  python3 check_library_stdin.py [foofiles-dir]

Exits 0 if clean, 1 if any procedure violates the rule.
"""

import os
import re
import sys

# Procedures allowed to break the rule, as "file.foo:procedure": reason.
# Add an entry only with a reason you would defend in review.
ALLOWED = {
    # Correct by construction: it creates stdin itself when there is none,
    # rather than assuming a caller made one. This is the pattern to copy if a
    # library routine genuinely needs the TEXTFILE parser.
    'vec{basis}.foo:read_library_data':
        'creates stdin when absent, then restores it',

    # Known dual-role readers: an optional name argument, falling back to
    # stdin when it is absent. Safe *only* because every driver passes the
    # name (run_rgbi.foo:202-203 does). Fragile -- see DEFERRED.md.
    'molecule.read.foo:read_molden_file':      'present(file_name) guard; drivers pass it',
    'molecule.read.foo:read_tonto_FChk_file':  'present(name) guard; drivers pass it',
    'molecule.read.foo:read_g09_FChk_file':    'present(name) guard; drivers pass it',
    'molecule.put.foo:put_florian_wfn_file':   'UNGUARDED stdin read; see DEFERRED.md',
}

# Keyword dispatchers. These take the keyword as an argument and read its
# value from stdin -- that is the whole point of a "keyword=" handler, so
# taking an argument does not make them library routines.
DISPATCHERS = {'process_keyword', 'process_list_keyword', 'read_keywords',
               'process_options'}

# A procedure header: exactly three spaces, a lower-case name, optional
# (args), optional "result (x)", optional ":: attrs". Macros (ENSURE, DIE_IF)
# are upper case, so requiring a lower-case initial excludes them.
HEADER = re.compile(
    r'^   ([a-z_][A-Za-z_0-9]*)'      # name
    r'(\([^)]*\))?'                    # optional (args)
    r'(\s+result\s*\([^)]*\))?'        # optional result (x)
    r'\s*(:::?.*)?$'                   # optional :: / ::: attributes
)

# Declaration lines inside a body can look like a header ("self :: IN"), so
# skip the ones that are obviously declarations of known pseudo-variables.
NOT_A_PROC = {'self', 'res', 'result'}


def strip_noncode(line):
    """Remove string literals and any trailing comment.

    Both matter: "stdin" appears inside help text and inside format strings,
    and a bare comment mentioning stdin is not a call.
    """
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


def procedures(path):
    """Yield (name, has_args, start_line, body_lines) for each procedure."""
    with open(path, encoding='utf-8', errors='replace') as f:
        lines = f.read().splitlines()

    current = None
    for i, line in enumerate(lines, start=1):
        stripped = line.lstrip()
        if stripped.startswith('!'):
            continue
        m = HEADER.match(line)
        if m and m.group(1) not in NOT_A_PROC:
            # A header ends the previous procedure and starts a new one.
            if current:
                yield current
            current = [m.group(1), bool(m.group(2)), i, []]
        elif current:
            current[3].append((i, line))
    if current:
        yield current


def main():
    root = sys.argv[1] if len(sys.argv) > 1 else os.path.join(
        os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'foofiles')
    if not os.path.isdir(root):
        print('no such directory: %s' % root, file=sys.stderr)
        return 2

    violations = []
    n_proc = n_arg_proc = 0

    for name in sorted(os.listdir(root)):
        if not name.endswith('.foo'):
            continue
        path = os.path.join(root, name)
        for proc, has_args, start, body in procedures(path):
            n_proc += 1
            if not has_args:
                continue          # keyword handler: reading stdin is its job
            if proc in DISPATCHERS:
                continue          # its argument *is* the keyword
            n_arg_proc += 1
            key = '%s:%s' % (name, proc)
            if key in ALLOWED:
                continue
            for lineno, text in body:
                if re.search(r'\bstdin\b', strip_noncode(text)):
                    violations.append((name, lineno, proc, text.strip()))

    if violations:
        print('FAIL: procedures that take arguments must not touch stdin')
        print('      (they are called by argv-driven programs, which have no')
        print('       job file -- see DEFERRED.md). Move the parsing into a')
        print('       keyword handler and pass the result as an argument.')
        print('')
        for f, lineno, proc, text in violations:
            print('  %s:%d  in %s' % (f, lineno, proc))
            print('      %s' % text[:100])
        return 1

    print('library stdin OK: %d procedures, %d take arguments, none touch stdin'
          % (n_proc, n_arg_proc))
    return 0


if __name__ == '__main__':
    sys.exit(main())
