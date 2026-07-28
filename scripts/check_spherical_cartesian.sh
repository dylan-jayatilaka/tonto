#!/bin/sh
# Regression test: spherical and cartesian bases must agree below d functions.
#
# For a basis containing only s and p functions the spherical-harmonic and
# cartesian basis sets are *mathematically identical* (they first differ at d,
# 6 cartesian vs 5 spherical components). But `use_spherical_basis` selects
# different two-electron code inside make_r_Fock_mx:
#
#     use_spherical_basis= T  ->  make_r_JK_direct
#     use_spherical_basis= F  ->  make_r_JK_engine
#
# so the two settings must produce the same energy on an s/p-only basis. This is
# a self-validating invariant: it needs no reference output and cannot be
# silently blessed by regenerating references on a broken build.
#
# It exists because exactly this invariant was violated for months without being
# noticed: gfortran 14.3 on arm64 macOS miscompiled shell1quartet.F90 at -O3, so
# the engine path returned two-electron energies that were slightly too small.
# Everything downstream inherited it -- the oxygen atom converged ~2.8 Ha *below*
# the variational limit -- yet nothing in the suite compared the two paths.
# See ANTLR4_DEFERRED.md, "verify the macOS build".
#
#   usage:  sh check_spherical_cartesian.sh <tonto-executable> <basis-set-dir>
#
# Exits 0 if every case agrees, 1 otherwise.

TONTO="$1"
BASIS="$2"

if [ -z "$TONTO" ] || [ -z "$BASIS" ]; then
    echo "usage: sh check_spherical_cartesian.sh <tonto> <basis-set-dir>" >&2
    exit 2
fi
if [ ! -x "$TONTO" ];  then echo "not executable: $TONTO" >&2; exit 2; fi
if [ ! -d "$BASIS" ];  then echo "no basis-set dir: $BASIS" >&2; exit 2; fi

WORK=${TMPDIR:-/tmp}/sph_cart.$$
mkdir -p "$WORK" || exit 2
trap 'rm -rf "$WORK"' EXIT

# Agreement threshold. The two runs solve an identical problem, so they should
# match to essentially every printed digit; 1e-6 Hartree is far tighter than any
# real discrepancy yet loose enough not to trip on last-digit noise.
TOL=0.000001

status=0

# name  Z  multiplicity  basis   (s/p-only basis sets only -- no d functions!)
run_case () {
    name=$1; Z=$2; mult=$3; basis=$4
    for sph in T F; do
        d="$WORK/$name.$sph"
        mkdir -p "$d" || exit 2
        cd "$d" || exit 2
        cat > stdin <<EOF
{
   name= ${name}_${sph}
   output_style_options= { real_precision= 10 }
   use_spherical_basis= $sph
   basis_name= $basis
   charge= 0
   multiplicity= $mult
   atoms= {
      keys= { label= { units= angstrom } pos= }
      data= {
         $Z      0.000000    0.000000    0.000000
      }
   }
   scfdata= {
      initial_density= core
      kind=            uhf
      direct=          on
      convergence= 0.00001
      diis= { convergence_tolerance= 0.00001 }
      output= NO
      output_results= YES
   }
   scf
   delete_scf_archives
}
EOF
        TONTO_BASIS_SET_DIRECTORY="$BASIS" "$TONTO" > console.out 2>&1
        if [ ! -f stdout ]; then
            echo "FAIL $name: no output for use_spherical_basis=$sph (see $d)"
            status=1
            return
        fi
    done

    eT=$(awk '/Total energy/       {v=$NF} END{print v}' "$WORK/$name.T/stdout")
    eF=$(awk '/Total energy/       {v=$NF} END{print v}' "$WORK/$name.F/stdout")
    vT=$(awk '/Electron repulsion/ {v=$NF} END{print v}' "$WORK/$name.T/stdout")
    vF=$(awk '/Electron repulsion/ {v=$NF} END{print v}' "$WORK/$name.F/stdout")

    if [ -z "$eT" ] || [ -z "$eF" ]; then
        echo "FAIL $name: could not read the energies (see $WORK/$name.*)"
        status=1
        return
    fi

    ok=$(awk -v a="$eT" -v b="$eF" -v t="$TOL" \
             'BEGIN { d = a - b; if (d < 0) d = -d; print (d <= t) ? "yes" : "no" }')

    if [ "$ok" = yes ]; then
        printf 'ok   %-12s spherical=%s cartesian=%s\n' "$name" "$eT" "$eF"
    else
        printf 'FAIL %-12s spherical=%s cartesian=%s   (V_ee %s vs %s)\n' \
               "$name" "$eT" "$eF" "$vT" "$vF"
        echo "     The two settings solve an identical problem on an s/p-only basis,"
        echo "     so this points at the two-electron code (make_r_JK_engine vs"
        echo "     make_r_JK_direct) or at its miscompilation. See ANTLR4_DEFERRED.md."
        status=1
    fi
}

echo "spherical vs cartesian agreement (s/p-only bases)"

run_case Be  4 1 STO-3G     # two s shells   -- the minimal failing case
run_case O   8 3 STO-3G     # s and p shells
run_case Ne 10 1 STO-3G     # s and p, closed shell
run_case N   7 4 3-21G      # s and p, split valence

exit $status
