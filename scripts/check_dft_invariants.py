#!/usr/bin/env python3
"""Invariant tests for the DFT machinery -- properties, not blessed numbers.

Every check here was written by working BACKWARDS from a defect found on
2026-08-12/13, asking "what is the cheapest property that would have caught
this?". None needs a reference output, so none can be blessed away.

  property                              would have caught
  ------------------------------------  ------------------------------------
  1  the grid responds to accuracy=      MOLECULE.SET:initialize_DFT_grids
                                         destroyed and recreated the BECKE_GRID,
                                         so the whole becke_grid= { } block was
                                         INERT and every DFT run used defaults
                                         while put_basics echoed back whatever
                                         had been asked for. Seven accuracies
                                         gave a bit-identical energy.
  2  LDA is invariant to rho_cutoff,     rho_cutoff defaulted to 1e-6, which is
     GGA is not                          a systematic 1e-5 bias in every GGA and
                                         none at all in LDA -- because
                                         x = |grad rho|/rho^(4/3) GROWS in the
                                         tail the cutoff truncates. It was the
                                         long-standing discrepancy against g09.
                                         This check encodes that asymmetry.
  3  closed shell: rks == uks            TWO defects at once. "V0b(i) = V0a(i)"
                                         in the unrestricted VWN potentials gave
                                         beta twice the correlation potential
                                         plus the exchange potential; and 14
                                         single-precision literals sat in the
                                         unrestricted LYP and B3LYP routines
                                         while their restricted twins had d0.
  4  a bogus functional name is fatal    an unrecognised name contributed
                                         NOTHING, silently, at exit 0: "blyp"
                                         gave -67.7092 against -76.4002. And
                                         "gill96" was blessed as valid in three
                                         places and implemented nowhere.
  6  b3lypx == b3lypgx exchange           b3lypx selected B3LYP's exchange
                                         routine but never set
                                         .using_hybrid_exchange, so the 0.2*E_HF
                                         third never entered the Fock matrix.
                                         1.81 Hartree, exit 0. NO test uses the
                                         name, so nothing else guards this.
  5  a late use_spherical_basis= is      the bases are resolved when the atoms
     fatal                               are read, so setting it after the
                                         atoms= block was silently ignored --
                                         25 cartesian basis functions instead of
                                         24 spherical, 1.6e-3 Hartree, exit 0.
  7  a bigger grid does not make the     BECKE_GRID:prune_grid discarded every
     answer worse                        grid point whose WEIGHT was below
                                         basis_fn_cutoff (1e-10). The innermost
                                         shells of a heavy atom have weights of
                                         1e-14, so the core was thrown away, and
                                         a finer grid threw away more of it:
                                         pruning_scheme= none was 80x WORSE than
                                         the pruned grid. The 1.5e-6 floor
                                         against g09 was this.
  8  basis_fn_cutoff is inert at best    the same defect, seen from the other
                                         side: 1e-10 vs 1e-14 moved the energy
                                         by 1.0e-6.
  9  partition_scheme= is not inert,     the DFT path called the Stratmann-
     and a bogus name is fatal           Scuseria builder unconditionally, so
                                         partition_scheme= becke gave a
                                         bit-identical energy. Becke is the
                                         default since 2026-09-11; the check
                                         asks for SS explicitly.

WHY PROPERTIES AND NOT REFERENCES. A blessed reference records what Tonto DID;
these record what must be TRUE. Every defect above was invisible to the
reference suite -- several because both sides of a comparison were consistently
wrong, and the grid one because the suite contains no test that varies a grid.
The 0.2%-relative gate in scripts/test.py is +/-0.15 Hartree at these energies,
some 15,000 times coarser than the effects here.

THRESHOLDS ARE CALIBRATED FROM MEASUREMENT, not from principle. The values
observed on 2026-08-13 (STO-3G water) are recorded beside each one.

  usage:  python3 check_dft_invariants.py <tonto-exe> <basis-sets-dir>

Exits 0 if every property holds, 1 otherwise.
"""

import os
import subprocess
import sys
import tempfile

# --- calibrated thresholds -------------------------------------------------
# Changing the grid MUST change the answer. Observed: 3.82e-05 between
# accuracy= low and high. A generous floor: anything above noise proves the
# becke_grid block is not inert.
GRID_MIN_RESPONSE = 1.0e-7
# b3lypx and b3lypgx select the SAME exchange routines, so with the same
# correlation they must agree to the last bit. Observed after the 2026-09-08 fix:
# exactly 0.0. Before it, 1.81 Hartree -- the whole of B3LYP's exact exchange.
BX_TOL = 1.0e-10
# An LDA functional must be INSENSITIVE to rho_cutoff. Observed: exactly 0.0 --
# the restricted LDA routines are vectorised expressions with no cutoff at all.
LDA_CUTOFF_MAX = 1.0e-10
# A GGA functional MUST be sensitive to it. Observed: 2.11e-05 for becke88
# between rho_cutoff 1e-6 and 1e-12.
GGA_CUTOFF_MIN = 1.0e-7
# Closed shell, rks vs uks. Observed maximum 5.92e-10 (becke88), which is plain
# SCF noise; 1e-8 is ~17x headroom. The defects this catches were 6.3e-4 and
# 2.4e-7, i.e. four and one orders above the threshold respectively.
RU_TOL = 1.0e-8
# Enlarging the grid at accuracy= best must not move the energy by more than
# ordinary grid convergence. Observed after the 2026-09-10 fix (STO-3G water,
# becke88+lyp): pruning_scheme= none 6.0e-7, n_radial_pts= 100 2.1e-7. Before
# it, 1.3e-4 and 2.4e-6 respectively on cc-pVDZ; the defect is 65x the bound.
BIGGER_GRID_MAX = 2.0e-6
# basis_fn_cutoff must not move the energy at best. Observed: 1.0e-12.
BF_CUTOFF_MAX = 1.0e-8
# The partition scheme must change the energy on a coarse grid. Observed:
# 3.2e-5 between stratmann_scuseria and becke at accuracy= low.
PARTITION_MIN_RESPONSE = 1.0e-8

JOB = """{{
   name= h2o
   output_style_options= {{ real_precision= 12 }}
   basis_name= STO-3G
   charge= 0
   multiplicity= 1
   atoms= {{
      keys= {{ label= {{ units= angstrom }} pos= }}
      data= {{
         8     -0.028260    0.000029    0.000000
         1      0.599917    0.767685    0.000000
         1      0.599941   -0.767609    0.000000
      }}
   }}
{spherical}   becke_grid= {{ set_defaults  accuracy= {acc}  rho_cutoff= {cut} {grid} }}
   scfdata= {{
      initial_density= promolecule
      kind= {kind}
      dft_exchange_functional= {exch}
      dft_correlation_functional= {corr}
      convergence= 0.00000001
      diis= {{ convergence_tolerance= 0.0001 }}
      output= NO
      output_results= YES
   }}
   scf
   delete_scf_archives
}}
"""


class Runner:
    def __init__(self, exe, basis):
        self.exe, self.basis = exe, basis
        self.dir = tempfile.mkdtemp(prefix="dft-invariants-")
        self.n = 0

    def __call__(self, acc="low", cut="1.0e-10", kind="rks", exch="slater",
                 corr="none", spherical="", grid=""):
        """Run one job; return (exit code, total energy or None)."""
        self.n += 1
        d = os.path.join(self.dir, "job%03d" % self.n)
        os.makedirs(d, exist_ok=True)
        inp = os.path.join(d, "stdin")
        with open(inp, "w") as f:
            f.write(JOB.format(acc=acc, cut=cut, kind=kind, exch=exch,
                               corr=corr, spherical=spherical, grid=grid))
        out = os.path.join(d, "stdout")
        rc = subprocess.call(
            [self.exe, "--input", inp, "--output", out], cwd=d,
            env=dict(os.environ, TONTO_BASIS_SET_DIRECTORY=self.basis),
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


def energy(run, **kw):
    """Run and insist on a number -- a crash here is itself a failure."""
    rc, e = run(**kw)
    if rc != 0 or e is None:
        raise RuntimeError("job failed (exit %d) for %s" % (rc, kw))
    return e


def main():
    if len(sys.argv) != 3:
        sys.stderr.write(__doc__)
        return 2
    exe, basis = os.path.abspath(sys.argv[1]), os.path.abspath(sys.argv[2])
    if not os.path.exists(exe):
        sys.stderr.write("no such executable: %s\n" % exe)
        return 2
    run = Runner(exe, basis)
    bad = []

    try:
        # 1 -- the grid must respond to accuracy=
        lo = energy(run, acc="low", exch="becke88", corr="lyp")
        hi = energy(run, acc="high", exch="becke88", corr="lyp")
        d = abs(hi - lo)
        ok = d > GRID_MIN_RESPONSE
        print("  1  grid responds to accuracy=      |low-high| = %.3e  %s"
              % (d, "ok" if ok else "FAIL"))
        if not ok:
            bad.append("changing accuracy= did not change the energy (%.3e <= %.1e)"
                       " -- is the becke_grid block being discarded again?"
                       % (d, GRID_MIN_RESPONSE))

        # 2 -- LDA insensitive to rho_cutoff, GGA sensitive
        for exch, lim, want_big in (("slater", LDA_CUTOFF_MAX, False),
                                    ("becke88", GGA_CUTOFF_MIN, True)):
            a = energy(run, acc="high", cut="1.0e-6", exch=exch)
            b = energy(run, acc="high", cut="1.0e-12", exch=exch)
            d = abs(a - b)
            ok = (d > lim) if want_big else (d < lim)
            print("  2  %-7s vs rho_cutoff           |diff|     = %.3e  %s"
                  % (exch, d, "ok" if ok else "FAIL"))
            if not ok and want_big:
                bad.append("%s (a GGA) is insensitive to rho_cutoff (%.3e <= %.1e)"
                           " -- the tail is being discarded, or the cutoff is"
                           " not reaching the functional" % (exch, d, lim))
            elif not ok:
                bad.append("%s (an LDA) moved with rho_cutoff (%.3e >= %.1e)"
                           " -- an LDA integrand dies as rho^(4/3) and should"
                           " not care" % (exch, d, lim))

        # 3 -- closed shell: rks must equal uks
        for exch, corr in (("slater", "none"), ("becke88", "none"),
                           ("none", "vwn5"), ("none", "lyp"),
                           ("becke88", "lyp"), ("b3lypx", "b3lypc")):
            r = energy(run, kind="rks", exch=exch, corr=corr)
            u = energy(run, kind="uks", exch=exch, corr=corr)
            d = abs(u - r)
            ok = d < RU_TOL
            print("  3  rks == uks  %-16s |rks-uks|  = %.3e  %s"
                  % (exch + "+" + corr, d, "ok" if ok else "FAIL"))
            if not ok:
                bad.append("%s+%s: closed-shell uks differs from rks by %.3e"
                           " (>= %.1e). For equal spin densities they are the"
                           " same calculation" % (exch, corr, d, RU_TOL))

        # 4 -- an unrecognised functional name must be fatal
        rc, _ = run(exch="blyp", corr="lyp")
        ok = rc != 0
        print("  4  bogus functional name is fatal  exit       = %-9d %s"
              % (rc, "ok" if ok else "FAIL"))
        if not ok:
            bad.append("an unrecognised functional name exited 0 -- it is being"
                       " silently ignored, which produces a number that is not"
                       " the requested calculation")

        # 5 -- use_spherical_basis= after atoms= must be fatal, not ignored
        rc, _ = run(spherical="   use_spherical_basis= TRUE\n")
        ok = rc != 0
        print("  5  late use_spherical_basis fatal  exit       = %-9d %s"
              % (rc, "ok" if ok else "FAIL"))
        if not ok:
            bad.append("use_spherical_basis= set after the atoms= block exited 0"
                       " -- the bases are already resolved, so it is being"
                       " silently ignored")
        # 6 -- b3lypx and b3lypgx must give the SAME exchange
        bx = energy(run, exch="b3lypx",  corr="none")
        bg = energy(run, exch="b3lypgx", corr="none")
        d = abs(bx - bg)
        ok = d < BX_TOL
        print("  6  b3lypx == b3lypgx exchange     |diff|     = %.3e  %s"
              % (d, "ok" if ok else "FAIL"))
        if not ok:
            bad.append("b3lypx and b3lypgx differ by %.3e (>= %.1e). They select"
                       " the SAME exchange routine, which computes only"
                       " 0.08*E_LDA + 0.72*E_GGA -- B3LYP's remaining 0.2*E_HF"
                       " comes from the Fock matrix, gated on"
                       " .using_hybrid_exchange. If they differ, one of the two"
                       " names is not setting that flag and is silently dropping"
                       " the exact exchange" % (d, BX_TOL))

        # 7 -- a bigger grid must not make the answer worse
        best = energy(run, acc="best", exch="becke88", corr="lyp")
        for label, grid in (("pruning_scheme= none", "pruning_scheme= none"),
                            ("n_radial_pts= 100", "n_radial_pts= 100")):
            e = energy(run, acc="best", exch="becke88", corr="lyp", grid=grid)
            d = abs(e - best)
            ok = d < BIGGER_GRID_MAX
            print("  7  best vs %-21s |diff|     = %.3e  %s"
                  % (label, d, "ok" if ok else "FAIL"))
            if not ok:
                bad.append("enlarging the grid (%s) moved the best energy by %.3e"
                           " (>= %.1e) -- are grid points being discarded by a"
                           " weight threshold again?" % (label, d, BIGGER_GRID_MAX))

        # 8 -- basis_fn_cutoff must be inert at best
        e = energy(run, acc="best", exch="becke88", corr="lyp",
                   grid="basis_fn_cutoff= 1e-14")
        d = abs(e - best)
        ok = d < BF_CUTOFF_MAX
        print("  8  basis_fn_cutoff inert at best   |diff|     = %.3e  %s"
              % (d, "ok" if ok else "FAIL"))
        if not ok:
            bad.append("basis_fn_cutoff 1e-10 vs 1e-14 moved the best energy by"
                       " %.3e (>= %.1e)" % (d, BF_CUTOFF_MAX))

        # 9 -- partition_scheme= must do something, and a bogus name is fatal
        ss = energy(run, acc="low", exch="becke88", corr="lyp",
                    grid="partition_scheme= stratmann_scuseria")
        bk = energy(run, acc="low", exch="becke88", corr="lyp")
        d = abs(bk - ss)
        ok = d > PARTITION_MIN_RESPONSE
        print("  9  partition_scheme= responds      |SS-becke| = %.3e  %s"
              % (d, "ok" if ok else "FAIL"))
        if not ok:
            bad.append("partition_scheme= becke gave the same energy as"
                       " stratmann_scuseria (%.3e <= %.1e) -- the keyword is"
                       " inert on the DFT path again" % (d, PARTITION_MIN_RESPONSE))
        rc, _ = run(grid="partition_scheme= bekce")
        ok = rc != 0
        print("  9  bogus partition name is fatal   exit       = %-9d %s"
              % (rc, "ok" if ok else "FAIL"))
        if not ok:
            bad.append("an unrecognised partition_scheme= exited 0 -- it is"
                       " being silently ignored")

    except RuntimeError as exc:
        bad.append(str(exc))

    print()
    if bad:
        print("FAILED -- %d property/properties violated:" % len(bad))
        for b in bad:
            print("  - %s" % b)
        return 1
    print("OK -- all nine DFT invariants hold (%d jobs)" % run.n)
    return 0


if __name__ == "__main__":
    sys.exit(main())
