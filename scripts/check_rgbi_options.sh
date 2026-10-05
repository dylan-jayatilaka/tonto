#!/bin/sh
# Invariant test: rgbi's documented options must match its accepted options.
#
# rgbi is command-line driven, so its `--help` text is its interface
# documentation, and nothing else keeps that text in step with the
# `select case (option)` block in runfiles/run_rgbi.foo. Same shape as
# check_hart_options.sh: two sets from two independent places, compared, and
# the documented failure modes exercised, each of which must exit non-zero.
#
#   documented -- "   --name" headings in the live `rgbi --help` output
#   accepted   -- uncommented `case ("name")` labels in runfiles/run_rgbi.foo
#
#   usage:  sh check_rgbi_options.sh <rgbi-executable> <run_rgbi.foo>
#
# Exits 0 if everything agrees, 1 otherwise.

RGBI="$1"
SOURCE="$2"

if [ -z "$RGBI" ] || [ -z "$SOURCE" ]; then
    echo "usage: sh check_rgbi_options.sh <rgbi> <run_rgbi.foo>" >&2
    exit 2
fi

# Absolutise first: the failure-mode section cd's into a scratch directory.
abspath() {
    case "$1" in
        /*) printf '%s\n' "$1" ;;
        *)  printf '%s\n' "$PWD/$1" ;;
    esac
}
RGBI=$(abspath "$RGBI")
SOURCE=$(abspath "$SOURCE")
if [ ! -x "$RGBI" ];   then echo "not executable: $RGBI" >&2; exit 2; fi
if [ ! -f "$SOURCE" ]; then echo "no such source: $SOURCE" >&2; exit 2; fi

WORK=${TMPDIR:-/tmp}/rgbi_options.$$
mkdir -p "$WORK" || exit 2
trap 'rm -rf "$WORK"' EXIT

status=0

# ---------------------------------------------------------------- help output

"$RGBI" --help > "$WORK/help.txt" 2>&1
rc=$?
if [ $rc -ne 0 ]; then
    echo "FAIL: 'rgbi --help' exited $rc, expected 0"
    sed -n '1,20p' "$WORK/help.txt"
    exit 1
fi

for section in SYNOPSIS EXPLANATION REFERENCES OPTIONS; do
    if ! grep -q "^[[:space:]]*$section" "$WORK/help.txt"; then
        echo "FAIL: 'rgbi --help' has no $section section"
        status=1
    fi
done

# The installed command is rgbi; run_rgbi is the CMake target and must not
# leak into what a user reads.
if grep -q "run_rgbi" "$WORK/help.txt"; then
    echo "FAIL: 'rgbi --help' names the program run_rgbi"
    status=1
fi

# Any option heading still spelled with a single dash is a migration regression.
if grep -E '^[[:space:]]*-[a-zA-Z][a-zA-Z0-9-]*([[:space:]]|$)' "$WORK/help.txt" > "$WORK/single_dash.txt"; then
    echo "FAIL: single-dash option headings in --help (must be '--name'):"
    sed 's/^/     /' "$WORK/single_dash.txt"
    status=1
fi

# --------------------------------------------------------------- the two sets

sed -n 's/^[[:space:]]*--\([a-zA-Z0-9][a-zA-Z0-9-]*\).*/\1/p' "$WORK/help.txt" \
    | sort -u > "$WORK/documented.txt"

# Accepted: uncommented `case ("name")` labels, trailing blanks trimmed. The
# `select case (tail)` block's labels (fchk, molden) are file extensions, not
# options, and are removed.
grep -E '^[[:space:]]*case[[:space:]]*\("' "$SOURCE" \
    | sed -n 's/^[[:space:]]*case[[:space:]]*("\([^"]*\)").*/\1/p' \
    | sed 's/[[:space:]]*$//' \
    | grep -E '^[a-zA-Z0-9][a-zA-Z0-9-]*$' \
    | grep -v -E '^(fchk|molden)$' \
    | sort -u > "$WORK/accepted.txt"

if ! diff -u "$WORK/documented.txt" "$WORK/accepted.txt" > "$WORK/optdiff.txt"; then
    echo "FAIL: rgbi's --help and its accepted options disagree."
    echo "      (-) documented but not accepted, (+) accepted but not documented:"
    sed -n '4,$p' "$WORK/optdiff.txt" | grep -E '^[-+]' | sed 's/^/     /'
    status=1
fi

n=$(wc -l < "$WORK/accepted.txt" | tr -d ' ')
if [ "$n" -lt 3 ]; then
    echo "FAIL: only $n options extracted from $SOURCE -- the parser above is broken"
    status=1
fi

# ------------------------------------------------------------- failure modes

cd "$WORK" || exit 2
: > empty.fchk
: > notawavefunction.txt

check_fails() {
    desc="$1"; shift
    out=$("$RGBI" "$@" 2>&1)
    rc=$?
    if [ $rc -eq 0 ]; then
        echo "FAIL: $desc -- expected non-zero exit, got 0"
        echo "$out" | sed -n '1,6p' | sed 's/^/     /'
        status=1
    fi
}

check_fails "no arguments"
check_fails "two arguments"          empty.fchk empty.fchk
check_fails "wrong file extension"   notawavefunction.txt
check_fails "unknown option"         --nosuchoption x empty.fchk
check_fails "single-dash option"     -ci-labels empty.fchk

# The single-dash rejection must name the long replacement, not just fail.
out=$("$RGBI" -ci-labels empty.fchk 2>&1)
if ! echo "$out" | grep -q -- "--ci-labels"; then
    echo "FAIL: '-ci-labels' rejection does not suggest '--ci-labels'"
    echo "$out" | sed -n '1,6p' | sed 's/^/     /'
    status=1
fi

if [ $status -eq 0 ]; then
    echo "rgbi options OK: $n options, --help and code agree, failures exit non-zero"
fi
exit $status
