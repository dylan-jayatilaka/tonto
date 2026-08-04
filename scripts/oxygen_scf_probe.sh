#!/bin/sh
# Oxygen-atom SCF probe: print the energy decomposition at truncated iteration
# counts, so T / V_eN / V_ee can be compared term-by-term across platforms.
#
#   usage:  sh oxprobe.sh /path/to/tonto /path/to/tonto-repo
#
# Iteration 1 is the most diagnostic: the density is just the H_core
# eigenvectors, so T and V_eN there read out the one-electron integrals.
# The virial ratio -V/T must be ~2.000 at convergence for a correct result.

TONTO="$1"
REPO="$2"

if [ -z "$TONTO" ] || [ -z "$REPO" ]; then
    echo "usage: sh oxprobe.sh /path/to/tonto /path/to/tonto-repo" >&2
    exit 1
fi
if [ ! -x "$TONTO" ]; then echo "not executable: $TONTO" >&2; exit 1; fi

SRC="$REPO/tests/short/oxygen_atom_uhf_cc-pVDZ/stdin"
if [ ! -f "$SRC" ]; then echo "no such input: $SRC" >&2; exit 1; fi

echo "binary : $TONTO"
echo "repo   : $REPO"
echo
printf '%-10s %-12s %-12s %-12s %-12s %s\n' iters E V_ee V_eN T -V/T

for n in 1 2 3 100; do
    d=/tmp/oxprobe.$$/it$n
    mkdir -p "$d" || exit 1
    cd "$d" || exit 1

    # Insert max_iterations just after the scfdata block opens.
    awk -v n="$n" '
        { print }
        /scfdata=[ \t]*\{/ && !ins { print "      max_iterations= " n; ins = 1 }
    ' "$SRC" > stdin

    TONTO_BASIS_SET_DIRECTORY="$REPO/basis_sets" "$TONTO" > console.out 2>&1

    if [ ! -f stdout ]; then
        printf '%-10s %s\n' "$n" "NO OUTPUT -- see $d/console.out"
        continue
    fi

    E=$(  awk '/Total energy/            {v=$NF} END{print v}' stdout)
    VEE=$(awk '/Electron repulsion/      {v=$NF} END{print v}' stdout)
    VEN=$(awk '/Nuclear attraction/      {v=$NF} END{print v}' stdout)
    T=$(  awk '/Kinetic energy/          {v=$NF} END{print v}' stdout)
    VT=$( awk '/Virial ratio/            {v=$NF} END{print v}' stdout)

    printf '%-10s %-12s %-12s %-12s %-12s %s\n' "$n" "$E" "$VEE" "$VEN" "$T" "$VT"
done

echo
echo "(run dirs under /tmp/oxprobe.$$)"
