#!/usr/bin/env python3
"""Normalise Foo-source indentation to multiples of 3 spaces.

Light-touch, structure-preserving:
  * Each *structural* line's leading-space count is snapped to the nearest
    multiple of 3 (n%3==1 -> n-1, n%3==2 -> n+1). On-grid lines are untouched,
    so existing nesting choices (nested `do` at the same level, un-indented
    `case`) are preserved automatically.
  * Continuation lines (those following a line that ends with `&`) and blank
    lines are left exactly as-is, so data tables / wrapped expressions keep
    their alignment.
  * A lone `contains` is forced to column 0.
  * womersley.foo is skipped entirely (per maintainer).

Usage: python3 scripts/reindent_foo.py [file.foo ...]   (default: all foofiles/*.foo)
"""
import glob, os, re, sys

SKIP = {"womersley.foo"}


def snap(n):
    r = n % 3
    if r == 0:
        return n
    return n - 1 if r == 1 else n + 1


def reindent(path):
    with open(path, "r", errors="surrogateescape") as fh:
        text = fh.read()
    nl_end = text.endswith("\n")
    lines = text.split("\n")
    if nl_end:
        lines = lines[:-1]  # drop the empty element after the final newline

    out = []
    prev_continues = False  # previous emitted code line ended with '&'
    changed = 0
    for line in lines:
        stripped = line.strip()
        if stripped == "":
            out.append(line)
            # blank line does not change continuation state
            continue

        sp = len(line) - len(line.lstrip(" "))
        content = line[sp:]

        if prev_continues:
            new = line                       # continuation: leave untouched
        elif stripped.lower() == "contains":
            new = content                    # force column 0
        else:
            new = " " * snap(sp) + content

        if new != line:
            changed += 1
        out.append(new)
        prev_continues = stripped.endswith("&")

    new_text = "\n".join(out) + ("\n" if nl_end else "")
    if new_text != text:
        with open(path, "w", errors="surrogateescape") as fh:
            fh.write(new_text)
    return changed


def main():
    args = sys.argv[1:]
    if args:
        files = args
    else:
        d = os.path.join(os.path.dirname(__file__), "..", "foofiles")
        files = sorted(glob.glob(os.path.join(d, "*.foo")))
    total = 0
    touched = 0
    for f in files:
        if os.path.basename(f) in SKIP:
            continue
        c = reindent(f)
        if c:
            touched += 1
            total += c
            print(f"{c:6d}  {os.path.relpath(f)}")
    print(f"--- {total} lines reindented across {touched} files ---")


if __name__ == "__main__":
    main()
