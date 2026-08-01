#!/bin/sh
# Invariant test: hart's documented options must match its accepted options.
#
# hart is entirely command-line driven, so its `--help` text *is* its interface
# documentation. Nothing keeps that text in step with the `select case (option)`
# block in runfiles/run_har.foo -- and it had already drifted: `--disk-sfs` was
# documented in full while its case label sat commented out, so using the
# documented option killed the run with "unknown option". A stored reference
# would not have caught it either, since the help text and the case block would
# simply have been blessed together.
#
# This check needs no reference output and cannot be silently blessed. It
# compares two sets extracted from two independent places:
#
#   documented -- "   --name" headings in the live `hart --help` output
#   accepted   -- uncommented `case ("name")` labels in runfiles/run_har.foo
#
# and additionally exercises the documented failure modes, each of which must
# print its message *and* exit non-zero.
#
#   usage:  sh check_hart_options.sh <hart-executable> <run_har.foo> <basis-set-dir>
#
# Exits 0 if everything agrees, 1 otherwise.

HART="$1"
SOURCE="$2"
BASIS="$3"

if [ -z "$HART" ] || [ -z "$SOURCE" ] || [ -z "$BASIS" ]; then
    echo "usage: sh check_hart_options.sh <hart> <run_har.foo> <basis-set-dir>" >&2
    exit 2
fi

# Absolutise before anything, because the failure-mode section below cd's into a
# scratch directory: a relative path such as "build/hart" would stop resolving
# there and every check would "fail" with `not found`. ctest passes absolute
# paths so this only bites a hand-run -- which is exactly when it misleads most.
# scripts/test.py guards the same way, for the same reason.
abspath() {
    case "$1" in
        /*) printf '%s\n' "$1" ;;
        *)  printf '%s\n' "$PWD/$1" ;;
    esac
}
HART=$(abspath "$HART")
SOURCE=$(abspath "$SOURCE")
BASIS=$(abspath "$BASIS")
if [ ! -x "$HART" ];   then echo "not executable: $HART" >&2; exit 2; fi
if [ ! -f "$SOURCE" ]; then echo "no such source: $SOURCE" >&2; exit 2; fi
if [ ! -d "$BASIS" ];  then echo "no basis-set dir: $BASIS" >&2; exit 2; fi

WORK=${TMPDIR:-/tmp}/hart_options.$$
mkdir -p "$WORK" || exit 2
trap 'rm -rf "$WORK"' EXIT

TONTO_BASIS_SET_DIRECTORY="$BASIS"
export TONTO_BASIS_SET_DIRECTORY

status=0

# ---------------------------------------------------------------- help output

"$HART" --help > "$WORK/help.txt" 2>&1
rc=$?
if [ $rc -ne 0 ]; then
    echo "FAIL: 'hart --help' exited $rc, expected 0"
    sed -n '1,20p' "$WORK/help.txt"
    exit 1
fi

for section in SYNOPSIS EXPLANATION RESTRICTIONS OPTIONS; do
    if ! grep -q "^$section" "$WORK/help.txt"; then
        echo "FAIL: 'hart --help' has no $section section"
        status=1
    fi
done

# Any option heading still spelled with a single dash is a migration regression.
# Leading whitespace is matched loosely: whether the runtime prefixes a margin
# column to a text line is a TEXTFILE style setting, not something to depend on.
if grep -E '^[[:space:]]*-[a-zA-Z][a-zA-Z0-9-]*([[:space:]]|$)' "$WORK/help.txt" > "$WORK/single_dash.txt"; then
    echo "FAIL: single-dash option headings in --help (must be '--name'):"
    sed 's/^/     /' "$WORK/single_dash.txt"
    status=1
fi

# --------------------------------------------------------------- the two sets

# Documented: "--name ..." option headings, indented, in the OPTIONS section.
sed -n 's/^[[:space:]]*--\([a-zA-Z0-9][a-zA-Z0-9-]*\).*/\1/p' "$WORK/help.txt" \
    | sort -u > "$WORK/documented.txt"

# Accepted: uncommented `case ("name   ")` labels. Foo pads the labels to a
# common width so the assignments line up, hence the trailing-blank trim. The
# leading-`!` filter is what keeps the deliberately frozen options (--charge,
# --mult, --ldtol, --scf-guess, --anharm, --wavelength) out of both sets: they
# are commented out in the code and in the help alike.
grep -E '^[[:space:]]*case[[:space:]]*\("' "$SOURCE" \
    | sed -n 's/^[[:space:]]*case[[:space:]]*("\([^"]*\)").*/\1/p' \
    | sed 's/[[:space:]]*$//' \
    | grep -E '^[a-zA-Z0-9][a-zA-Z0-9-]*$' \
    | sort -u > "$WORK/accepted_all.txt"

# run_har.foo has a second, unrelated `select case (guess)` block; its labels
# are not options.
grep -v -E '^(density|mos)$' "$WORK/accepted_all.txt" > "$WORK/accepted.txt"

if ! diff -u "$WORK/documented.txt" "$WORK/accepted.txt" > "$WORK/optdiff.txt"; then
    echo "FAIL: hart's --help and its accepted options disagree."
    echo "      (-) documented but not accepted, (+) accepted but not documented:"
    sed -n '4,$p' "$WORK/optdiff.txt" | grep -E '^[-+]' | sed 's/^/     /'
    status=1
fi

n=$(wc -l < "$WORK/accepted.txt" | tr -d ' ')
if [ "$n" -lt 10 ]; then
    echo "FAIL: only $n options extracted from $SOURCE -- the parser above is broken"
    status=1
fi

# ------------------------------------------------------------- failure modes
#
# Each must exit non-zero. Before SYSTEM.die was changed to `stop 1`, every one
# of these died with a message and still returned 0, which is precisely why a
# broken hart could not be detected by any harness.

cd "$WORK" || exit 2
: > empty.cif
: > notacif.txt

check_fails() {
    desc="$1"; shift
    out=$("$HART" "$@" 2>&1)
    rc=$?
    if [ $rc -eq 0 ]; then
        echo "FAIL: $desc -- expected non-zero exit, got 0"
        echo "$out" | sed -n '1,6p' | sed 's/^/     /'
        status=1
    fi
}

check_fails "no arguments"
check_fails "two arguments"          empty.cif empty.cif
check_fails "non-cif argument"       notacif.txt
check_fails "unknown option"         --nosuchoption x empty.cif
check_fails "unknown basis"          --basis NOSUCHBASIS empty.cif
check_fails "single-dash option"     -basis STO-3G empty.cif

# The single-dash rejection must name the long replacement, not just fail.
out=$("$HART" -basis STO-3G empty.cif 2>&1)
if ! echo "$out" | grep -q -- "--basis"; then
    echo "FAIL: '-basis' rejection does not suggest '--basis'"
    echo "$out" | sed -n '1,6p' | sed 's/^/     /'
    status=1
fi

if [ $status -eq 0 ]; then
    echo "hart options OK: $n options, --help and code agree, failures exit non-zero"
fi
exit $status
