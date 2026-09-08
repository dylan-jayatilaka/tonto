#!/usr/bin/env python3
"""Invariant test: every DFT functional name Tonto accepts must be implemented.

This is the accepted-name against implemented-name cross-check of
docs/DFT_STANDARDISATION.md section 12, and it closes the stated limit of the
section 5 fix.

WHAT SECTION 5 FIXED, AND WHAT IT DID NOT. An unrecognised functional name used
to contribute nothing at exit 0. SCF_DATA:set_exchange_functional and
set_correlation_functional now carry a live `case default; UNKNOWN(...)`, so a
name arriving through input or through set_* dies loudly. That covers every path
a user can reach. It does NOT cover the reverse direction, which is how gill96
survived for years: a name the SETTER blesses but no DISPATCHER implements is
accepted, validated, and then silently computes nothing. Validation cannot catch
that, because from the setter's point of view the name is perfectly good.

The six dispatcher `case default` lines cannot be enabled to catch it either --
they are PURE, and UNKNOWN is a DIE that expands to an allocate, which a pure
procedure may not do. That is why this check is a source scan and not a runtime
guard. See docs/DFT_STANDARDISATION.md section 5.

THE INVARIANT. Six `select case` blocks name functionals, and they must agree
exactly:

  accepted     SCF_DATA:set_exchange_functional     -- what input is allowed
               SCF_DATA:set_correlation_functional      to ask for, as a union
  implemented  DFT_FUNCTIONAL:new_r_energy_density  -- what actually computes
               DFT_FUNCTIONAL:new_r_potential           something; a name absent
               DFT_FUNCTIONAL:new_u_energy_density      here is a silent no-op
               DFT_FUNCTIONAL:new_u_potential
  classified   DFT_FUNCTIONAL:is_GGA_functional     -- a name absent here is
               DFT_FUNCTIONAL:is_LDA_functional         quietly answered FALSE

Exchange and correlation names share all four dispatchers and both queries, so
the setters are checked as a union: it is the two setters TOGETHER that define
the vocabulary. Which of the two accepts a given name is a separate question,
and one the setters already answer at runtime.

A missing case is a defect in either direction. Absent from a dispatcher, the
name computes nothing (gill96, and blyp before the fix). Absent from a setter,
the machinery carries a functional no input can select -- dead code that reads
as a supported feature.

WHY A SCAN AND NOT A RUN. A runtime check would have to know the whole
vocabulary to enumerate it, which is the very thing being checked. The scan
reads the vocabulary out of the source, so a name added to one block and
forgotten in another is caught by construction rather than by someone thinking
to test it.

Caveat, stated plainly: this is a heuristic over source text, not a parse. It
keys on Foo's three-space indentation to find block extents. Trailing blanks
inside the case labels are insignificant to Fortran and are stripped here, which
is why `case("vwn5    ")` and `case("vwn5   ")` compare equal.

  usage:  python3 check_functional_names.py [foofiles-dir]

Exits 0 if the six blocks agree, 1 otherwise.
"""

import os
import re
import sys

# The six blocks, as (file, procedure, role). Role groups them for the report.
BLOCKS = [
    ('scf_data.foo',       'set_correlation_functional', 'accepted'),
    ('scf_data.foo',       'set_exchange_functional',    'accepted'),
    ('dft_functional.foo', 'new_r_energy_density',       'implemented'),
    ('dft_functional.foo', 'new_r_potential',            'implemented'),
    ('dft_functional.foo', 'new_u_energy_density',       'implemented'),
    ('dft_functional.foo', 'new_u_potential',            'implemented'),
    ('dft_functional.foo', 'is_GGA_functional',          'classified'),
    ('dft_functional.foo', 'is_LDA_functional',          'classified'),
]

CASE_RE = re.compile(r'^\s*case\s*\(', re.IGNORECASE)
SELECT_RE = re.compile(r'^(\s*)select\s+case\s*\(', re.IGNORECASE)
STRING_RE = re.compile(r'"([^"]*)"')


def case_labels(lines, proc):
    """The quoted labels of the first `select case` block inside procedure `proc`.

    Returns None if the procedure or its select block cannot be found -- which
    is itself a failure, since a renamed procedure silently empties this check.
    """
    start = None
    head = re.compile(r'^   %s\s*[({:]' % re.escape(proc))
    for i, line in enumerate(lines):
        if head.match(line):
            start = i
            break
    if start is None:
        return None

    # The select block: from its `select case` to the `end` at the same indent.
    labels, indent = [], None
    for line in lines[start:]:
        stripped = line.lstrip()
        if indent is None:
            m = SELECT_RE.match(line)
            if m:
                indent = m.group(1)
            elif re.match(r'^   end\s*$', line):
                return None          # procedure ended with no select block
            continue
        if stripped.startswith('!'):
            continue                 # a commented-out case is not a case
        if line.rstrip() == indent + 'end':
            return labels
        if CASE_RE.match(line):
            # Trailing blanks are insignificant to Fortran's character compare.
            labels.extend(s.strip() for s in STRING_RE.findall(line))
    return None                      # unterminated select block


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    foodir = sys.argv[1] if len(sys.argv) > 1 else \
        os.path.join(os.path.dirname(here), 'foofiles')

    found, missing_blocks = {}, []
    cache = {}
    for fname, proc, role in BLOCKS:
        path = os.path.join(foodir, fname)
        if path not in cache:
            if not os.path.exists(path):
                print('FAIL: %s not found under %s' % (fname, foodir))
                return 1
            cache[path] = open(path, encoding='utf-8',
                               errors='replace').read().splitlines()
        labels = case_labels(cache[path], proc)
        if labels is None:
            missing_blocks.append('%s:%s' % (fname, proc))
        else:
            found[(fname, proc, role)] = set(labels)

    if missing_blocks:
        print('FAIL: %d functional-name block(s) could not be read' % len(missing_blocks))
        print('      Renamed or restructured? Then update BLOCKS in this script --')
        print('      a block this check cannot find is a block it cannot check.')
        for b in missing_blocks:
            print('  %s' % b)
        return 1

    # The vocabulary is the union of what the two setters accept.
    vocabulary = set()
    for (fname, proc, role), labels in found.items():
        if role == 'accepted':
            vocabulary |= labels

    bad = []
    for (fname, proc, role), labels in sorted(found.items()):
        if role == 'accepted':
            continue
        for name in sorted(vocabulary - labels):
            bad.append((name, '%s:%s' % (fname, proc), role,
                        'accepted but not %s' % role))
        for name in sorted(labels - vocabulary):
            bad.append((name, '%s:%s' % (fname, proc), role,
                        '%s but no setter accepts it' % role))

    if bad:
        print('FAIL: %d functional name(s) disagree across the six blocks' % len(bad))
        print('      A name a setter accepts but a dispatcher does not implement')
        print('      computes NOTHING, silently, at exit 0 -- that was gill96.')
        print('      See docs/DFT_STANDARDISATION.md sections 5 and 12.')
        for name, where, _role, why in bad:
            print('  %-10s %-45s %s' % ('"%s"' % name, where, why))
        return 1

    print('  functional names agree  vocabulary = %d names, %d blocks  ok'
          % (len(vocabulary), len(found)))
    print('OK -- every accepted DFT functional name is implemented and classified')
    return 0


if __name__ == '__main__':
    sys.exit(main())
