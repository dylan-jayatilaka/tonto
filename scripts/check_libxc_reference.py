#!/usr/bin/env python3
"""Check Tonto's libxc functionals in a whole SCF, without a blessed reference.

Three kinds of check, on water and its cation in cc-pVDZ:

  1. PBE, PBE0 and r2SCAN energies against PySCF. The numbers below are typed in from
     PySCF 2.14, which shares no code with Tonto's grid, SCF or exact exchange.
     Tonto's finest grid is used; the tolerance is the one check_dft_reference.py
     uses against g09.
  2. A functional evaluated by libxc gives the same energy as the same
     functional hand-written in Tonto, and the one keyword dft_functional=
     gives the same energy as the two keywords it stands for.
  3. Inputs that must be refused are refused: the one keyword together with
     either of the two, an unknown name, and a functional that needs what
     Tonto does not have.

  usage:  python3 check_libxc_reference.py <tonto-exe> <basis-sets-dir>

Exits 0 if every check passes, 1 otherwise, and 77 (skipped) if this Tonto was
built without libxc.
"""

import os
import subprocess
import sys
import tempfile

# PySCF 2.14, cc-pVDZ with Cartesian d functions (Tonto's default), grid level 9
# without pruning, conv_tol 1e-11. Keys: (charge, multiplicity, kind, functional).
PYSCF = {
    (0, 1, "rks", "pbe"):  -76.335028993,
    (0, 1, "rks", "pbe0"): -76.339811822,
    (0, 1, "rks", "r2scan"): -76.382397984,
    (1, 2, "uks", "pbe"):  -75.883203639,
    (1, 2, "uks", "pbe0"): -75.891279686,
    (1, 2, "uks", "r2scan"): -75.934178684,
}
TOL_PYSCF = 5.0e-6

# The same functional two ways: ((charge, multiplicity, kind), lines, lines).
X = "      dft_exchange_functional= %s\n      dft_correlation_functional= %s\n"
F = "      dft_functional= %s\n"
SAME = [
    ((0, 1, "rks"), X % ("becke88", "lyp"),     X % ("gga_x_b88", "gga_c_lyp")),
    ((0, 1, "rks"), X % ("becke88", "lyp"),     F % "blyp"),
    ((0, 1, "rks"), X % ("b3lypgx", "b3lypgc"), F % "hyb_gga_xc_b3lyp"),
    ((0, 1, "rks"), X % ("b3lypgx", "b3lypgc"), F % "b3lyp"),
    ((0, 1, "rks"), X % ("b3lypgx", "b3lypgc"), F % "b3lyp-gaussian"),
    ((1, 2, "uks"), X % ("b3lypgx", "b3lypgc"), F % "hyb_gga_xc_b3lyp"),
    ((1, 2, "uks"), X % ("b3lypx", "b3lypc"),   F % "b3lyp-orca"),
]
TOL_SAME = 1.0e-8

# Inputs Tonto must stop on.
REFUSED = [
    F % "pbe" + "      dft_exchange_functional= slater\n",
    "      dft_correlation_functional= lyp\n" + F % "pbe",
    F % "nonsense",
    F % "gga_x_nonsense",
    F % "hyb_gga_xc_cam_b3lyp",
]

JOB = """{{
   name= ref
   output_style_options= {{ real_precision= 12 }}
   basis_name= cc-pVDZ
   charge= {charge}
   multiplicity= {mult}
   atoms= {{
      keys= {{ label= {{ units= angstrom }} pos= }}
      data= {{
        O -1.769142 -0.076181 0.000000
        H -2.065745 0.837492 0.000000
        H -0.809034 0.001317 0.000000
      }}
   }}
   becke_grid= {{ set_defaults  accuracy= {grid} }}
   scfdata= {{
      initial_density= promolecule
      kind= {kind}
{functional}      convergence= 0.00000001
      diis= {{ convergence_tolerance= 0.0001 }}
      output= NO
      output_results= YES
   }}
   scf
   delete_scf_archives
}}
"""


def run(exe, basis, workdir, tag, charge, mult, kind, functional, grid):
    """Run one job; return the exit code and the total energy, or None."""
    d = os.path.join(workdir, tag)
    os.makedirs(d, exist_ok=True)
    inp = os.path.join(d, "stdin")
    with open(inp, "w") as f:
        f.write(JOB.format(charge=charge, mult=mult, kind=kind,
                           functional=functional, grid=grid))
    out = os.path.join(d, "stdout")
    rc = subprocess.call([exe, "--input", inp, "--output", out], cwd=d,
                         env=dict(os.environ, TONTO_BASIS_SET_DIRECTORY=basis),
                         stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    e = None
    if os.path.exists(out):
        with open(out, errors="replace") as f:
            for line in f:
                if "Total energy" in line:
                    try:
                        e = float(line.split()[-1])
                    except ValueError:
                        pass
                    break
    return rc, e


def main():
    if len(sys.argv) != 3:
        sys.stderr.write(__doc__)
        return 2
    exe, basis = os.path.abspath(sys.argv[1]), os.path.abspath(sys.argv[2])
    bad = []

    with tempfile.TemporaryDirectory() as work:

        # A Tonto without libxc refuses a libxc name: nothing to check then.
        rc, e = run(exe, basis, work, "probe", 0, 1, "rks", F % "pbe", "low")
        if rc != 0 or e is None:
            print("SKIPPED -- this Tonto was built without libxc")
            return 77

        print("%-6s %-5s %-6s %18s %16s %10s" %
              ("CHARGE", "KIND", "FUNC", "TONTO", "PySCF", "|diff|"))
        for (charge, mult, kind, name), ref in sorted(PYSCF.items()):
            rc, e = run(exe, basis, work, "%s_%s" % (kind, name),
                        charge, mult, kind, F % name, "best")
            if rc != 0 or e is None:
                bad.append("%s %s: job failed" % (kind, name))
                continue
            d = abs(e - ref)
            print("%-6d %-5s %-6s %18.10f %16.9f %10.1e" %
                  (charge, kind, name, e, ref, d))
            if d > TOL_PYSCF:
                bad.append("%s %s: |Tonto - PySCF| = %.1e exceeds %.1e"
                           % (kind, name, d, TOL_PYSCF))

        print()
        for i, ((charge, mult, kind), one, two) in enumerate(SAME):
            _, e1 = run(exe, basis, work, "same%da" % i, charge, mult, kind, one, "high")
            _, e2 = run(exe, basis, work, "same%db" % i, charge, mult, kind, two, "high")
            label = " = ".join(" ".join(s.split()) for s in (one, two))
            if e1 is None or e2 is None:
                bad.append("job failed: %s" % label)
                continue
            d = abs(e1 - e2)
            print("%-5s %10.1e  %s" % (kind, d, label))
            if d > TOL_SAME:
                bad.append("energies differ by %.1e: %s" % (d, label))

        print()
        for i, functional in enumerate(REFUSED):
            rc, e = run(exe, basis, work, "refused%d" % i, 0, 1, "rks", functional, "low")
            label = " ".join(functional.split())
            print("%-8s %s" % ("refused" if rc != 0 else "ACCEPTED", label))
            if rc == 0:
                bad.append("accepted, but should be refused: %s" % label)

    print()
    if bad:
        print("FAILED -- %d check(s):" % len(bad))
        for b in bad:
            print("  " + b)
        return 1
    print("OK -- libxc functionals agree with PySCF (< %.1e) and with the "
          "hand-written ones (< %.1e)" % (TOL_PYSCF, TOL_SAME))
    return 0


if __name__ == "__main__":
    sys.exit(main())
