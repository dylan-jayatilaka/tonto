#!/usr/bin/env python3
"""Invariant test: a single atom must run, under every initial guess.

Two properties, neither of which needs a blessed reference:

  1. **It must not crash.** Every combination below must exit 0.
  2. **The converged energy must not depend on the initial guess.** A guess is
     a starting point; if `promolecule` and `core` converge to different
     numbers, one of them is not converged, or they found different SCF
     solutions. Either is worth knowing.

WHY THIS EXISTS. On 2026-08-13 a single atom with `initial_density=
promolecule` -- the default -- **segfaulted in a release build**. H, He and Li
all died, closed shell and open shell alike. The cause was not subtle once
found: a promolecule guess superposes atomic ANO densities, and
`MOLECULE.SCF:make_ANOs_and_interpolators` deliberately returns early for one
atom ("if (.n_atom<=1) return"), so there were no ANOs to superpose.
`MOLECULE.RHO:add_ANO_densities` guards this with
ENSURE(.has_all_ANO_matrices,"no ANOs") -- which reports cleanly in a DEBUG
build and is COMPILED OUT in release, leaving a segfault.

It survived because **no test in the suite ran a single atom**. That is a
COVERAGE gap, not a tolerance gap: no reblessing, tighter tolerance or grid
convergence would ever have found it, because the job never produced a number
to compare. A smoke matrix is the right shape of test for that, and it
generalises the bug instead of pinning the one case that happened to be tried.

Cheap by construction: STO-3G, tiny grid, a fraction of a second each.

  usage:  python3 check_single_atom_scf.py <tonto-exe> <basis-sets-dir>

Exits 0 if every job runs and the guesses agree, 1 otherwise.
"""

import os
import subprocess
import sys
import tempfile

# Z, multiplicity, label. He is here on purpose: the defect was NOT open-shell
# specific, and a closed-shell single atom is the case one would least expect
# to break.
ATOMS = [(1, 2, "H"), (2, 1, "He"), (3, 2, "Li")]
GUESSES = ["promolecule", "core"]
KINDS = ["uhf", "uks"]

# The guess must not move the converged answer by more than this. CALIBRATED
# FROM MEASUREMENT, not from principle: on 2026-08-13 all twelve combinations
# below gave promolecule and core energies identical to every printed digit
# (real_precision= 10), i.e. a difference below 1e-10. 1e-8 is therefore two
# orders of headroom over what was observed, while still being four orders
# tighter than anything that could hide a convergence to a different SCF
# solution. Tighten it if it proves stable; loosen it only with a measurement.
TOL = 1.0e-8

JOB = """{{
   name= atom
   output_style_options= {{ real_precision= 10 }}
   basis_name= STO-3G
   charge= 0
   multiplicity= {mult}
   atoms= {{
      keys= {{ label= {{ units= angstrom }} pos= }}
      data= {{
         {Z}   0.0  0.0  0.0
      }}
   }}
   becke_grid= {{ set_defaults  accuracy= low }}
   scfdata= {{
      initial_density= {guess}
      kind= {kind}
      dft_exchange_functional= slater
      dft_correlation_functional= vwn5
      convergence= 0.00001
      diis= {{ convergence_tolerance= 0.0001 }}
      output= NO
      output_results= YES
   }}
   scf
   delete_scf_archives
}}
"""


def run(exe, basis, workdir, Z, mult, guess, kind):
    """Run one job; return (exit code, total energy or None)."""
    d = os.path.join(workdir, "%d_%s_%s" % (Z, guess, kind))
    os.makedirs(d, exist_ok=True)
    inp = os.path.join(d, "stdin")
    with open(inp, "w") as f:
        f.write(JOB.format(Z=Z, mult=mult, guess=guess, kind=kind))
    env = dict(os.environ, TONTO_BASIS_SET_DIRECTORY=basis)
    out = os.path.join(d, "stdout")
    rc = subprocess.call([exe, "--input", inp, "--output", out],
                         cwd=d, env=env,
                         stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    energy = None
    if os.path.exists(out):
        with open(out, errors="replace") as f:
            for line in f:
                if "Total energy" in line:
                    try:
                        energy = float(line.split()[-1])
                    except ValueError:
                        pass
                    break
    return rc, energy


def main():
    if len(sys.argv) != 3:
        sys.stderr.write(__doc__)
        return 2
    exe, basis = os.path.abspath(sys.argv[1]), os.path.abspath(sys.argv[2])
    if not os.path.exists(exe):
        sys.stderr.write("no such executable: %s\n" % exe)
        return 2

    failures = []
    workdir = tempfile.mkdtemp(prefix="single-atom-scf-")

    for Z, mult, label in ATOMS:
        for kind in KINDS:
            energies = {}
            for guess in GUESSES:
                rc, e = run(exe, basis, workdir, Z, mult, guess, kind)
                status = "ok" if rc == 0 and e is not None else "FAIL"
                print("  %-4s %-4s %-12s exit=%-4d %-18s %s"
                      % (label, kind, guess, rc,
                         ("%.10f" % e) if e is not None else "(no energy)",
                         status))
                if rc != 0:
                    failures.append("%s/%s/%s exited %d%s"
                                    % (label, kind, guess, rc,
                                       " (SEGFAULT)" if rc in (139, -11) else ""))
                elif e is None:
                    failures.append("%s/%s/%s produced no total energy"
                                    % (label, kind, guess))
                else:
                    energies[guess] = e
            if len(energies) == len(GUESSES):
                lo, hi = min(energies.values()), max(energies.values())
                if hi - lo > TOL:
                    failures.append(
                        "%s/%s: the initial guess changed the converged energy "
                        "by %.3e (> %.1e): %s"
                        % (label, kind, hi - lo, TOL,
                           ", ".join("%s=%.10f" % kv for kv in sorted(energies.items()))))

    print()
    if failures:
        print("FAILED -- %d problem(s):" % len(failures))
        for f in failures:
            print("  - %s" % f)
        return 1
    print("OK -- every single-atom job ran, and the initial guess did not move "
          "the converged energy by more than %.1e" % TOL)
    return 0


if __name__ == "__main__":
    sys.exit(main())
