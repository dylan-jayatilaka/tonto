# Plan: tune the DFT grid levels to 1e-3 … 1e-7 hartree

## Context

The five named grid levels should each give a known error in a GGA energy:
`very_low` 1e-3, `low` 1e-4, `medium` 1e-5, `high` 1e-6, `very_high` 1e-7 hartree.
They do not now. `very_low` is ten times better than it need be (it wastes points), `low` misses
on HCl, `medium` misses on HCl, Rn and karrikinolide, and for heavy atoms the levels hardly differ.
The cause is in `BECKE_GRID:set_accuracy` (`foofiles/becke_grid.foo:523`): a level sets only the
hydrogen radial count and three angular orders. The extra radial points per period
(`no_of_radial_pts`, line 1185) and the pruning rule are the same at every level.

This is step 1 of `docs/TASK_HAR_STANDARDISATION.md` section 7, and the entry
*Tune the DFT grid levels* in `TASKS_AND_HISTORY.md` (line 4390). It ends in a re-bless.

## What the stored papers say (literature check, done)

In `~/Dropbox/manuscripts/`: Treutler 1995, Mura 1996, Gill 1993 (SG-1), Murray 1993, Lindh 2001,
Krack 1998, Dasgupta 2017 (SG-2, SG-3), Helmich-Paris 2021, Stratmann 1996, Becke 1988,
Laqua 2018, Gräfenstein 2007.

- **No published ladder goes a decade per level down to 1e-7.** Treutler–Ahlrichs grids 1–5 span
  3e-4 to 3e-6 (in the electron count). SG-1 aims at 3e-4, Mura–Knowles the same. So the levels
  must be measured; the papers give the starting values and the test molecules.
- **Radial points** (Mura–Knowles Table I, the mapping Tonto uses): 30 → 50 points gains two
  decades, 50 → 70 nearly two more. Their 20/25/30 for periods 1/2/3 gives 2.5e-4, 30/35/40 gives
  2e-5. Expect roughly 8 to 10 more points per decade, and +5 per period for periods 1 to 3.
- **Angular order sets the floor** (Treutler–Ahlrichs Table IV, neopentane): 110 points 1.8e-3,
  194 points 2.7e-4, 302 and 434 points about 1e-5, 852 points 1e-6. So expect about 110 / 194–302 /
  302–434 / 590–770 / 974–1202 points for the five levels. The present `very_high` (770) will
  probably not reach 1e-7.
- **Pruning by shell number limits the top levels.** Tonto's own water ladder
  (`docs/TASK_DFT_STANDARDISATION.md:676`) puts the residual at `best` at 5e-7 from the L5/L11
  pruning. SG-1 to SG-3 prune by radius. `high` and `very_high` need a milder rule or none.
- **Hard cases named in the papers**, all cheap: neopentane, P4, LiF/LiH, Cr(CO)6, TiCl4.
- **Laqua, Kussmann and Ochsenfeld 2018** (`Laqua_2018_JCP_149_p204111.pdf`) is the closest
  published model for a ladder. Seven grids, Treutler–Ahlrichs radial mapping, the same
  three-zone pruning by shell number that Tonto uses (inner third, next sixth, outer half), but
  **the pruned orders rise with the level** where Tonto holds them at 14 and 50 points:

  | grid | radial points (H–Ne) | angular points, inner / middle / outer | points per C atom |
  |---|---|---|---|
  | g1 | 35 | 14 / 50 / 110 | 2,586 |
  | g2 | 40 | 26 / 74 / 194 | 5,056 |
  | g3 | 50 | 38 / 110 / 302 | 9,564 |
  | g4 | 55 | 50 / 194 / 434 | 15,526 |
  | g5 | 60 | 50 / 194 / 590 | 21,330 |
  | g6 | 70 | 86 / 302 / 974 | 40,838 |
  | g7 | 80 | 110 / 434 / 1454 | 68,770 |

  Heavier periods get 5, 10, 20, 25 and 30 more radial points (the last for Cs on). Their errors
  are PBE/def2-TZVP against an unpruned 150-radial, 2030-angular grid, on the G2 molecules and on
  weakly bound dimers (S22 at five distances, L7) — the same functional, basis and kind of
  reference as this plan. The errors are in their figures 5 and 7 to 9, not in a table; read them
  off the PDF when the scans exist. So step 4 of the method scans the inner and middle orders as
  well as the choice of scheme, starting from this table. They also report that Becke's
  partition gives a ragged energy curve for a weakly bound dimer (argon dimer, their figure 1)
  and cure it by changing the partition at long range. The water dimer in the test set, with one
  stretched geometry added, shows whether Tonto has that problem; changing the partition is not
  part of this task.
- **Gräfenstein and Cremer 2007** (`Grafenstein_2007_JCP_127_p164113.pdf`) is narrower than its
  title suggested: a way to add radial points in the valence region only, tested on the argon
  dimer with a meta-GGA, where 100 augmented points match 250 ordinary ones. It gives no grid
  ladder and no pruning rule to adopt. It bears on r2SCAN, which is reported here but is not a
  target.

## Test systems

Small, H to Zn (plus Br), chosen for spread of electronegativity and bonding. PBE, all-electron
def2-TZVP with spherical functions (def2-SVP for the first scans).

| Period | Systems | What each covers |
|---|---|---|
| 1–2 | H2O, N2, LiF, F2, CH3 radical (open shell), benzene, neopentane, water dimer | polar, triple bond, ionic, lone pairs, radical, aromatic, crowded C–H, hydrogen bond |
| 3 | HCl, NaCl, SiH4, P4, SF6 | the present worst case, ionic, electropositive centre, strained cage, hypervalent |
| 4, s and p | KF, HBr | ionic K; 4p |
| 4, 3d | TiCl4, Cr(CO)6, Ni(CO)4, Zn(CH3)2, CuCl, ScO (doublet), Mn atom (sextet) | d0, carbonyl back-bonding, d10, organometallic, late metal halide, open-shell oxide, high-spin atom |

22 systems; the largest are 13 to 17 atoms. Geometries: experimental or PySCF-optimised, typed
into one file with their source. If an open-shell 3d case will not converge it is dropped and noted.

Afterwards, as checks and not as calibration data: the seven heavy systems already scripted
(HF, HCl, HBr, HI, Xe, Hg, Rn, `x2c-SVPall`, `~/tonto-runs/2026-10-12_heavy_grid`), karrikinolide
(17 atoms, error against size), and one urea Hirshfeld atom refinement per level.

## Method

**The error is measured on one fixed density.** For each system, converge once, keep the density,
and evaluate the XC energy $`E_{xc}`$ and the electron count on each trial grid. The Coulomb,
kinetic and nuclear terms are analytic, so the change in $`E_{xc}`$ is the grid error and nothing
else. The reference is Tonto itself on a very large unpruned grid, shown converged by a second,
larger one agreeing to 1e-8. This avoids comparing codes: open-shell energies sit 3e-7 from ORCA
and PySCF for an unknown reason, which would swamp a 1e-7 target. PySCF
(`~/tonto-runs/2026-10-11_x2c/venv`) checks four closed-shell references only.

**Each ingredient is scanned alone, the others held converged**, so the result is a set of curves,
not a search:

1. **Cutoffs first.** `partition_cutoff` (1e-6), `rho_cutoff`, `basis_fn_cutoff`,
   `prune_rho_cutoff` have unmeasured biases (`types.foo:3108`). Tighten each in turn on six
   systems. Any that costs more than 1e-8 is tightened, at the top levels only if it costs time.
2. **Radial scan.** Angular order 59, no pruning. Vary the radial count of one period at a time.
   Gives error against radial points for periods 1, 2, 3 and 4.
3. **Angular scan.** Radial converged, no pruning. Vary the heavy-atom order and the hydrogen
   order separately over every Lebedev order from 11 to 71.
4. **Pruning.** At each level's radial and angular values, measure what `treutler_ahlrichs`,
   `adaptive` and `none` cost in error and save in points, and scan the inner and middle orders
   of the three-zone rule from Laqua's table. `put_grid_shell_errors`
   (`molecule.rho.foo:1257`) shows which shells can take a lower order.
5. **Assemble.** For each level take the cheapest values whose errors sum to half the target on
   the worst system. Record points per atom and XC time.
6. **Confirm with full SCF runs**: 22 systems × 5 levels, PBE/def2-TZVP, against a full SCF on the
   reference grid. Then B3LYP and PBE0 on six systems, r2SCAN reported but not a target, three
   systems rotated to a general orientation, and the checks listed above.

**How a level passes** (my proposal; say if you want it otherwise): the largest absolute error in
the total energy over the 22 systems is below the target. Error per atom is tabulated beside it,
and karrikinolide shows how it grows with size.

**Results are generated by one driver.** `~/tonto-runs/2026-10-13_grid_levels/` holds
`systems.py` (geometries, charge, multiplicity), `make_jobs.py`, `run_all.sh` (one detached script,
8 jobs at a time, one log) and `tables.py`, built from the scripts in
`~/tonto-runs/2026-10-12_heavy_grid` and `2026-10-11_r2scan_grid`. Every evaluation is one row of
`results.csv`: system, stage, grid settings, points, $`E_{xc}`$, electron count, energy, CPU
seconds. `tables.py` writes the tables and the error-against-points plots from that file alone.
Energies are printed to 10 decimals (`real_precision= 10`), SCF converged to 1e-9.

## Code changes (`foofiles/`, in order)

1. **A keyword that prints $`E_{xc}`$ and the electron count of the stored density on the present
   grid, with no Coulomb build** (`MOLECULE.MAIN` keyword calling the existing batched XC builder
   in `molecule.rho.foo`/`molecule.fock.foo`). Without it each evaluation costs two Fock builds.
   It also serves the open-shell 3e-7 item, whose next step is this same comparison.
2. **Radial points by period as data**: a vector component in `BECKE_GRID` (`types.foo:3002`)
   read by `no_of_radial_pts`, with an input keyword, replacing `n_extra_pts_per_shell` and the
   fixed 9/20/32. Add it to `set_defaults` and `set_settings_from` (`becke_grid.foo:270, 302`).
   Needed for step 2 of the method, and it is the final design.
3. **After calibration:** `set_accuracy` sets, per level, the radial vector, the angular orders
   and the pruning scheme. Periods 5 and 6 keep the 2026-10-12 values at `medium` and get scaled
   values at the other levels, checked on the seven heavy systems.
4. Update the accuracy comments (`types.foo:3121`, `becke_grid.foo:1186`), `docs/RUNNING_TONTO.md:79`,
   `docs/RUNNING_HART.md:77`, `docs/TONTO_KNOWN_ISSUES.md:62`, and the thresholds in
   `scripts/check_dft_invariants.py`.

Changes 1 and 2 leave every energy unchanged and can be merged before the calibration.

## Time

Tonto is serial; the Mac has 12 cores, so 8 jobs run at once. The 3d timings are estimates — the
first run times Cr(CO)6/def2-TZVP, and the plan is rescaled if it is far off.

| Step | Work | Elapsed |
|---|---|---|
| Code changes 1 and 2, debug build, short tests | — | half a day |
| Driver scripts, geometries, 22 converged densities | 22 SCF runs | 2 hours |
| Cutoffs, radial, angular, pruning scans | about 2,500 XC evaluations, 1 s to 2 min each | 3 to 5 hours, unattended |
| Assemble the levels, code change 3 | — | 2 hours |
| Confirmation | about 200 SCF runs, the largest about 30 min each on the reference grid | overnight |
| Docs, re-bless on achari2, full suite | DFT and Hirshfeld tests and all 5 `hart` tests change | half a day |

About three working days, of which under one day is computing.

## Decisions for Dylan

1. The pass rule above (largest error over the set, not error per atom).
2. `extreme` and `best`: I propose keeping `best` as the reference grid used here and dropping
   `extreme`; decided after the tables exist.
3. The COSX defaults name `very_low` and `high`, so they move with the levels. The
   `h2o_rhf_def2-SVP_RIJCOSX` error is re-measured, and the COSX grids pinned to explicit numbers if
   it grows.
4. The re-bless on achari2 and every build and test run are asked for when reached.

## Verification

- Code changes 1 and 2: debug build, `ctest -L short`; every energy unchanged.
- Reference grid: two sizes agree to 1e-8 for all 22 systems; four agree with PySCF to 1e-7.
- Final table: 22 systems × 5 levels, each level's largest error below its target, each level
  cheaper than the next; heavy seven, karrikinolide, rotation and urea refinement tabulated.
- `ctest` on `short long hart` (106 tests) after the re-bless; `scripts/check_docs.py` on the pages.
- Log in a new `docs/TASK_GRID_LEVELS.md`; the entry in `TASKS_AND_HISTORY.md` points to it.
