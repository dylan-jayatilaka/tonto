#!/usr/bin/env python3
"""Check the layout of Tonto's printed labels and headings, in the .foo sources.

CHECK 1 -- every `.show("label", value)` label ends in "=".

  TEXTFILE:show turns a label's trailing spaces and "=" into a row of dots,
  so labels of the same length line up:

      stdout.show("Total energy       =", E)      ->  Total energy ........ -76.0

  A label without "=" gets a short " .." instead, which lines up with
  nothing. A label that is a sentence, with the value following the words,
  must say so with dots=FALSE:

      stdout.show("The SCF energy is ", E, dots=FALSE)

CHECK 2 -- a heading's "====" rule is as long as the heading.

      stdout.text("====================")
      stdout.text("Coulomb metric table")
      stdout.text("====================")

  For a box of several lines between two rules, the rules match the longest.

Only literal labels are checked; a label built at run time cannot be.
Usage: check_show_labels.py <directory> [<directory> ...]
Prints each problem as file:line and exits 1 if there are any.
"""

import os
import re
import sys

SHOW = re.compile(r'\b[\w%.]*\.show\(\s*"([^"]*)"')
DOTS_OFF = re.compile(r'dots\s*=\s*FALSE', re.I)
RULE = re.compile(r'^\s*(\w+)\.text\("(=+)"\)\s*$')
TEXT = re.compile(r'^\s*(\w+)\.text\("([^"]*)"\)\s*$')


def statements(lines):
    """Yield (line number, text) with continuation lines joined."""
    i = 0
    while i < len(lines):
        start, text = i, lines[i]
        while text.rstrip().endswith('&') and i + 1 < len(lines):
            i += 1
            text = text.rstrip()[:-1] + ' ' + lines[i].strip().lstrip('&')
        yield start + 1, text
        i += 1


def check_labels(path, lines, problems):
    for number, text in statements(lines):
        if text.lstrip().startswith('!'):
            continue
        for m in SHOW.finditer(text):
            label = m.group(1)
            if label.rstrip().endswith('='):
                continue
            if DOTS_OFF.search(text[m.end():]):
                continue
            problems.append('%s:%d: show label does not end in "=" (and no dots=FALSE): "%s"'
                            % (path, number, label))


def check_rules(path, lines, problems):
    rules = [(i, m) for i, m in ((i, RULE.match(l)) for i, l in enumerate(lines)) if m]
    used = set()
    # A box: two rules on the same file, at most four text lines apart
    for (i, a), (j, b) in zip(rules, rules[1:]):
        if a.group(1) != b.group(1) or not 1 < j - i <= 5:
            continue
        inside = [TEXT.match(l) for l in lines[i + 1:j]]
        if not all(inside):
            continue
        width = max(len(t.group(2).rstrip()) for t in inside)
        for k, r in ((i, a), (j, b)):
            if len(r.group(2)) != width:
                problems.append('%s:%d: rule is %d long, the heading %d'
                                % (path, k + 1, len(r.group(2)), width))
        used.update((i, j))
    # A lone rule under a heading, or failing that, over one
    for i, r in rules:
        if i in used:
            continue
        for k in (i - 1, i + 1):
            if not 0 <= k < len(lines):
                continue
            t = TEXT.match(lines[k])
            if t and t.group(1) == r.group(1) and t.group(2).strip() and not t.group(2).startswith('='):
                if len(r.group(2)) != len(t.group(2).rstrip()):
                    problems.append('%s:%d: rule is %d long, the heading %d'
                                    % (path, i + 1, len(r.group(2)), len(t.group(2).rstrip())))
                break


def main(dirs):
    problems = []
    for d in dirs:
        for name in sorted(os.listdir(d)):
            if not name.endswith('.foo'):
                continue
            path = os.path.join(d, name)
            with open(path, errors='replace') as f:
                lines = f.read().split('\n')
            check_labels(path, lines, problems)
            check_rules(path, lines, problems)
    for p in problems:
        print(p)
    print('%d problem(s)' % len(problems))
    return 1 if problems else 0


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(2)
    sys.exit(main(sys.argv[1:]))
