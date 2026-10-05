#!/usr/bin/env python3
"""Check the user-facing documents (README.md and docs/*.md other than TASK_*
and the history and developer pages) for what belongs only in working
documents -- dates, commit hashes, status notes, investigation stories -- and
for LaTeX in forms GitHub draws wrongly. Exit status 1 if anything is found."""
import re, sys, pathlib

ROOT = pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else pathlib.Path(__file__).resolve().parent.parent)
EXEMPT = {"PROJECT_HISTORY.md", "TONTO_DEVELOPER_INFO.md", "TONTO_REPOSITORY_BRANCHES.md"}

CHECKS = [
    (r"\b20\d\d-\d\d-\d\d\b", "a date"),
    (r"`[0-9a-f]{7,12}`", "a commit hash"),
    (r"\b(Status|STATUS)\b\s*[:(]", "a status note"),
    (r"(?i)working document", "a working-document note"),
    (r"(?i)\b(was tried|we tried|measured, not assumed|turned out|it was found)\b", "an investigation story"),
    (r"\\tag\{", "\\tag (draws as a column in Chrome and Brave; use \\qquad (n))"),
    (r"(?<![`$])\$\$", "$$ display math (use a ```math block)"),
    (r"(?<![`$\\])\$(?![`$])[^$\n]+?(?<!`)\$(?![$])", "$...$ inline math (use $`...`$)"),
]

def user_facing():
    yield ROOT / "README.md"
    for p in sorted((ROOT / "docs").glob("*.md")):
        if p.name.startswith("TASK_") or p.name in EXEMPT:
            continue
        yield p

def strip_code(line):
    # Inline code spans are not checked, except $`...`$ math, which is kept.
    return re.sub(r"(?<!\$)`[^`]*`(?!\$)", "", line)

bad = 0
for path in user_facing():
    fence = False
    for n, line in enumerate(path.read_text().splitlines(), 1):
        if line.strip().startswith("```"):
            fence = not fence
            continue
        if fence:
            continue
        text = strip_code(line)
        for pattern, what in CHECKS:
            if re.search(pattern, text):
                print(f"{path.relative_to(ROOT)}:{n}: {what}: {line.strip()[:100]}")
                bad += 1
print(f"{bad} problem(s) in user-facing documents")
sys.exit(1 if bad else 0)
