#!/bin/sh
# Invariant test: foofiles/ tracks .foo sources and nothing else.
#
# Running a build inside foofiles/ leaves the translator's output next to the
# sources it came from -- .F90/.int/.use, binary .mod, CMakeFiles/ and the
# linked executables. That happened, and 1554 such files reached master before
# anyone noticed. The size was the smaller problem: a stale foofiles/crystal.F90
# beside foofiles/crystal.foo is exactly the confusion CLAUDE.md SS8 exists to
# prevent, because the generated Fortran belongs in the build tree and is never
# what you should be reading or editing.
#
# .gitignore now covers those paths, but an ignore rule is easy to defeat with
# `git add -f` and easy to break by renaming a target. This asserts the end
# state instead of the mechanism.
#
#   usage:  sh check_foofiles_clean.sh <source-dir>
#
# Exits 0 if foofiles/ is clean, 1 otherwise.

SRC="$1"

if [ -z "$SRC" ]; then
    echo "usage: sh check_foofiles_clean.sh <source-dir>" >&2
    exit 2
fi

if [ ! -d "$SRC/foofiles" ]; then
    echo "check_foofiles_clean: no $SRC/foofiles directory" >&2
    exit 2
fi

# Not a git checkout (an unpacked release tarball, say). Nothing to assert about
# the index, and failing here would redden a legitimate build.
if ! git -C "$SRC" rev-parse --git-dir >/dev/null 2>&1; then
    echo "check_foofiles_clean: not a git checkout, skipping"
    exit 0
fi

strays=$(git -C "$SRC" ls-files foofiles/ | grep -v '\.foo$')

if [ -n "$strays" ]; then
    n=$(printf '%s\n' "$strays" | wc -l)
    echo "check_foofiles_clean: FAIL -- $n tracked file(s) under foofiles/ are not .foo" >&2
    echo "" >&2
    printf '%s\n' "$strays" | head -20 >&2
    if [ "$n" -gt 20 ]; then echo "  ... and $((n - 20)) more" >&2; fi
    echo "" >&2
    echo "foofiles/ holds .foo sources only. Generated Fortran belongs in the build" >&2
    echo "tree -- see CLAUDE.md SS8. To fix:" >&2
    echo "" >&2
    echo "    git rm -r --cached \$(git ls-files foofiles/ | grep -v '\.foo\$')" >&2
    echo "" >&2
    echo "then check .gitignore covers them, and delete them from the working tree" >&2
    echo "with 'git clean -n -d foofiles/' (look first) and 'git clean -f -d foofiles/'." >&2
    exit 1
fi

echo "check_foofiles_clean: OK -- $(git -C "$SRC" ls-files foofiles/ | wc -l) tracked files, all .foo"
exit 0
