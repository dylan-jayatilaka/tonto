#!/bin/sh
# Regression test: the MPI reduction machinery must work, and must not depend
# on the rank count.
#
# run_mpi_pi computes pi by Riemann integration:  pi = \int_0^1 4/(1+x^2) dx.
# It is the smallest program that exercises the whole macro surface every
# parallel routine in Tonto depends on -- one `parallel do` and one
# PARALLEL_SUM -- against an answer that is known in advance and, crucially,
# must be the SAME at every rank count.
#
# This is a self-validating invariant: it needs no reference output, so unlike
# the reference-diff suite it cannot be silently blessed by regenerating
# references on a broken build.
#
# It exists because nothing in the suite asserted that MPI worked at all. When
# this was written, four reductions in molecule.grid.foo were dead code -- the
# translator emits LOCK_PARALLEL_DO as the first statement *inside* a
# `parallel do`, and DO_IN_PARALLEL is false while that lock is held, so a
# PARALLEL_SUM written in the loop body never executes. Every rank silently
# kept only its own 1/n_ranks of the terms. A wrong-but-plausible number came
# out and no test noticed. See DEFERRED.md.
#
# Distinguishing the two failure modes matters:
#   - wrong at EVERY rank count (including -n 1) -> the reduction or the
#     integration itself is broken.
#   - right at -n 1 but wrong at -n 2/-n 4, or the rank counts disagree with
#     each other -> the reduction is not combining partial sums correctly.
# The reference suite cannot tell these apart; this can.
#
#   usage:  sh check_mpi_pi.sh <run_mpi_pi-executable> [mpi-launcher] [ranks...]
#
#   e.g.    sh check_mpi_pi.sh build-mpi/run_mpi_pi mpirun 1 2 4
#
# Exits 0 if every rank count agrees with pi and with the others, 1 otherwise.

PI_EXE="$1"
shift 2>/dev/null
LAUNCHER="${1:-mpirun}"
shift 2>/dev/null
RANKS="$*"
[ -n "$RANKS" ] || RANKS="1 2 4"

if [ -z "$PI_EXE" ]; then
    echo "usage: sh check_mpi_pi.sh <run_mpi_pi> [launcher] [ranks...]" >&2
    exit 2
fi
if [ ! -x "$PI_EXE" ]; then echo "not executable: $PI_EXE" >&2; exit 2; fi

# Oversubscription. This is a CORRECTNESS check on a tiny Riemann sum, not a
# benchmark, so asking for more ranks than cores is exactly what we want -- but
# Open MPI 5 REFUSES to do it by default and exits non-zero without running the
# program at all. That is what made this check fail in CI at -n 4 on every run
# from the day it was added: 1 and 2 ranks agreed with pi to 13 digits and 4
# never started, which reads like a broken reduction and is not one.
#
# Probed rather than hard-coded, because the flag is Open MPI's; MPICH
# oversubscribes without being asked and would reject it.
OVERSUB=""
if "$LAUNCHER" --version 2>&1 | grep -qi "open mpi"; then
    OVERSUB="--oversubscribe"
fi

# Absolute, because we cd into a scratch directory below.
case "$PI_EXE" in /*) ;; *) PI_EXE="$PWD/$PI_EXE" ;; esac

WORK=${TMPDIR:-/tmp}/mpi_pi.$$
mkdir -p "$WORK" || exit 2
trap 'rm -rf "$WORK"' EXIT

# run_mpi_pi uses 1e8 intervals of the midpoint rule, whose discretisation error
# is ~h^2/24 ~ 4e-18 -- far below double precision. So the only error that can
# show up here is floating-point summation order, and any *correct* build must
# land within a few ulp of pi regardless of how the sum was partitioned.
# 1e-9 is many orders looser than reduction-order noise yet still catches a
# reduction that dropped whole ranks' contributions (which would be wrong by a
# factor of ~n_ranks, not by 1e-9).
PI_REF=3.14159265358979
TOL=0.000000001

# extract_pi <output-file>
#
# run_mpi_pi does stdout.show("Pi = ",pi), but TEXTFILE.show renders a label by
# DOT-PADDING it to a fixed width, so the line actually reads
#     Pi ..... 3.14159265358979
# not "Pi = ...". Match a leading `Pi`, then any run of separator characters
# (dots, spaces, `=`), then the number.
#
# Tonto also writes to a file literally named `stdout` in the working directory
# rather than to the process's stdout, so the caller tries both.
extract_pi() {
    sed -n 's/^ *Pi[ .=]*\([-0-9][0-9.EeDd+-]*\).*/\1/p' "$1" 2>/dev/null | tail -1
}

status=0
values=""

for n in $RANKS; do
    RUNDIR="$WORK/n$n"
    mkdir -p "$RUNDIR" || exit 2
    ( cd "$RUNDIR" && "$LAUNCHER" -n "$n" $OVERSUB "$PI_EXE" > console.out 2>&1 )
    rc=$?

    if [ $rc -ne 0 ]; then
        echo "FAIL mpi_pi   -n $n: launcher exited $rc"
        sed -n '1,12p' "$RUNDIR/console.out" 2>/dev/null | sed 's/^/     /'
        status=1
        continue
    fi

    val=$(extract_pi "$RUNDIR/console.out")
    [ -n "$val" ] || val=$(extract_pi "$RUNDIR/stdout")

    if [ -z "$val" ]; then
        echo "FAIL mpi_pi   -n $n: no 'Pi = ' line in output"
        sed -n '1,12p' "$RUNDIR/console.out" 2>/dev/null | sed 's/^/     /'
        status=1
        continue
    fi

    # Fortran may print D exponents; awk wants E.
    val=$(echo "$val" | tr 'Dd' 'Ee')

    if ! echo "$val $PI_REF $TOL" | awk '{exit (($1-$2)^2 <= $3*$3) ? 0 : 1}'; then
        echo "FAIL mpi_pi   -n $n: got $val, expected $PI_REF (tol $TOL)"
        echo "     a reduction that drops ranks is wrong by roughly a factor of n_ranks"
        status=1
        continue
    fi

    echo "ok   mpi_pi   -n $n: $val"
    values="$values $val"
done

# Rank-count invariance: every rank count must agree with every other. This is
# the part that catches a reduction which is *plausible* but rank-dependent.
first=""
for v in $values; do
    if [ -z "$first" ]; then first="$v"; continue; fi
    if ! echo "$v $first $TOL" | awk '{exit (($1-$2)^2 <= $3*$3) ? 0 : 1}'; then
        echo "FAIL mpi_pi   rank-count dependence: $v vs $first (tol $TOL)"
        status=1
    fi
done

if [ $status -eq 0 ]; then
    echo "ok   mpi_pi   all rank counts ($RANKS) agree with pi and with each other"
fi

exit $status
