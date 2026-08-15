#!/usr/bin/env python3
"""Absolute accuracy against INDEPENDENT reference codes -- not blessed output.

Every other test in this suite compares Tonto against Tonto: a reference file
records what Tonto DID, and reblessing moves it whenever Tonto moves. That can
only ever answer "did anything change?". It cannot answer "is it right?", and
it is blind to an error that was already present when the reference was
blessed.

That blindness is not hypothetical. Until 2026-08-13 every DFT reference in this
suite encoded a ~1e-5 error caused by `rho_cutoff` truncating the density tail.
The suite was green throughout, for years, because both sides of every
comparison carried the same error.

The numbers below are typed in from g09, which shares no source with Tonto. They
do not move when Tonto moves. If Tonto drifts away from them the test fails, and
the only way to "fix" that is to edit a constant by hand -- a deliberate act
somebody has to justify, not a --bless run.

WHY OPEN SHELL IS HERE. The `rks == uks` property in check_dft_invariants.py is
CLOSED shell, so it is structurally blind to anything that breaks only at
zeta != 0. All three VWN defects found on 2026-08-13/14 did exactly that: the
"V0b = V0a" slip, the chain-rule grouping, and VWN3 evaluating VWN_G at ZERO
instead of zeta, which left the unrestricted VWN3 potential with NO spin
dependence at all. An external open-shell anchor is the only check that catches
that class.

WHY accuracy= best, AND WHY THIS IS IN `long`. Measured 2026-08-14, the same
cases at coarser grids:

    case                     medium      high        best
    H2O+ uks slater+vwn5     -2.17e-05   +5.06e-06   +1.46e-06
    H2O  rks BLYP            -3.13e-06   +9.81e-06   +1.63e-06

A cheap grid does not work: at `high` the deviations already exceed the 5e-06
tolerance, and the sign flips between grids, so the comparison must be on the
absolute difference. Only `best` brings everything under 1.7e-06. That costs
about 10 s per DFT case, hence `long` rather than `short`. Running this blunt
would defeat its purpose -- at a 2e-05 tolerance it would still catch the VWN
slip (6.3e-04) but miss the single-precision literals (2.4e-07) entirely.

  usage:  python3 check_dft_reference.py <tonto-exe> <basis-sets-dir>

Exits 0 if every case is within tolerance, 1 otherwise.
"""

import os
import subprocess
import sys
import tempfile

# --- reference values -------------------------------------------------------
# ALL FROM g09, NOT FROM TONTO. cc-pVDZ, 6D (cartesian d, matching Tonto's
# use_spherical_basis default of FALSE), SCF=(Tight,Conver=10),
# Int(Grid=199974). The grid was verified converged: for BLYP on neutral water,
# FineGrid / UltraFine / 199974 / 250974 give -76.4002380205 / -76.4002384784 /
# -76.4002385321 / -76.4002385317, i.e. stable to 4e-10.
#
# The neutral-water HFS value is corroborated by ORCA 6.1.1 independently
# (agreeing with g09 to 2.3e-08 in spherical harmonics), so this is two codes,
# not one. Beware two traps if re-deriving these: ORCA silently defaults to the
# RI-J approximation (use NoRI), and Gaussian defaults to SPHERICAL d for
# cc-pVDZ, so 6D must be forced to match Tonto.
#
#   (charge, multiplicity, kind, exchange, correlation) -> g09 energy
REFERENCE = {
    ("H2O",  0, 1, "rhf", "",        ""):     -76.0232731206,
    ("H2O",  0, 1, "rks", "slater",  "none"): -75.1948035483,
    ("H2O",  0, 1, "rks", "becke88", "none"): -76.0608370448,
    ("H2O",  0, 1, "rks", "becke88", "lyp"):  -76.4002385321,
    ("H2O",  0, 1, "rks", "slater",  "vwn5"): -75.8578720891,
    ("H2O+", 1, 2, "uhf", "",        ""):     -75.6327038760,
    ("H2O+", 1, 2, "uks", "slater",  "none"): -74.7945774565,
    ("H2O+", 1, 2, "uks", "slater",  "vwn5"): -75.3977985806,
    ("H2O+", 1, 2, "uks", "slater",  "vwn3"): -75.5735204288,
}

# DFT cases carry Tonto's residual grid error against g09's grid -- a consistent
# ~1.5e-06 at accuracy= best, which is a property of the grid, not a defect.
# See docs/DFT_STANDARDISATION.md section 6b.
TOL_DFT = 5.0e-6
# HF uses no grid at all, so it should agree to round-off. Measured 1.2e-10
# (neutral) and 2.0e-10 (cation). A tight bound here catches a basis, integral
# or SCF regression that the DFT rows would swamp.
TOL_HF = 1.0e-8

JOB = """{{
   name= ref
   output_style_options= {{ real_precision= 12 }}
   basis_name= cc-pVDZ
   charge= {charge}
   multiplicity= {mult}
   atoms= {{
      keys= {{ label= {{ units= angstrom }} pos= }}
      data= {{
         8     -0.028260    0.000029    0.000000
         1      0.599917    0.767685    0.000000
         1      0.599941   -0.767609    0.000000
      }}
   }}
   becke_grid= {{ set_defaults  accuracy= best }}
   scfdata= {{
      initial_density= promolecule
      kind= {kind}
{functionals}      convergence= 0.00000001
      diis= {{ convergence_tolerance= 0.0001 }}
      output= NO
      output_results= YES
   }}
   scf
   delete_scf_archives
}}
"""


def run(exe, basis, workdir, tag, charge, mult, kind, exch, corr):
    d = os.path.join(workdir, tag)
    os.makedirs(d, exist_ok=True)
    functionals = ""
    if exch:
        functionals = ("      dft_exchange_functional= %s\n"
                       "      dft_correlation_functional= %s\n" % (exch, corr))
    inp = os.path.join(d, "stdin")
    with open(inp, "w") as f:
        f.write(JOB.format(charge=charge, mult=mult, kind=kind,
                           functionals=functionals))
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
    if not os.path.exists(exe):
        sys.stderr.write("no such executable: %s\n" % exe)
        return 2
    workdir = tempfile.mkdtemp(prefix="dft-reference-")
    bad = []
    print("  %-6s %-5s %-16s %-20s %-20s %-11s" %
          ("SYSTEM", "KIND", "FUNCTIONAL", "TONTO", "g09", "|diff|"))
    for i, (key, ref) in enumerate(sorted(REFERENCE.items())):
        name, charge, mult, kind, exch, corr = key
        tol = TOL_HF if kind in ("rhf", "uhf") else TOL_DFT
        rc, e = run(exe, basis, workdir, "c%02d" % i, charge, mult, kind, exch, corr)
        label = ("%s+%s" % (exch, corr)) if exch else "--"
        if rc != 0 or e is None:
            print("  %-6s %-5s %-16s exit=%d, no energy" % (name, kind, label, rc))
            bad.append("%s/%s/%s failed to run (exit %d)" % (name, kind, label, rc))
            continue
        d = abs(e - ref)
        ok = d < tol
        print("  %-6s %-5s %-16s %-20.12f %-20.10f %-11.3e %s"
              % (name, kind, label, e, ref, d, "ok" if ok else "FAIL"))
        if not ok:
            bad.append("%s %s %s: |Tonto - g09| = %.3e exceeds %.1e "
                       "(Tonto %.12f, g09 %.10f)"
                       % (name, kind, label, d, tol, e, ref))
    print()
    if bad:
        print("FAILED -- %d case(s) outside tolerance:" % len(bad))
        for b in bad:
            print("  - %s" % b)
        print("\n  These references come from g09, NOT from Tonto. A failure here")
        print("  means Tonto has moved away from an independently correct answer.")
        print("  Do NOT 'fix' it by editing the constants without establishing why.")
        return 1
    print("OK -- all %d cases agree with g09 (DFT < %.1e, HF < %.1e)"
          % (len(REFERENCE), TOL_DFT, TOL_HF))
    return 0


if __name__ == "__main__":
    sys.exit(main())
