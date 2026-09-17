# SCF and integral speed: benchmark data

**A working document** for the *Speed up the SCF and the integrals* item in `DEFERRED.md`.
Holds the measurements; the reasoning and plan are in `DEFERRED.md`.

## Karrikinolide grid ladder, 2026-09-11

BLYP/6-31G(d), 17 atoms, geometry and basis from the g09 checkpoint in
`tests/long/karrikinolide_blyp_6-31G(d)_Salvador_properties/`; promolecule guess,
`convergence= 1e-8`, `diis= { convergence_tolerance= 1e-4 }`; release build `06079254`
plus the Becke-default change, gfortran-14, one core, wall time by `/usr/bin/time`.
Raw inputs, outputs and timings: `~/tonto_runs/karrikinolide_grid_ladder_2026-09-11/`
(not in git).

| `accuracy=` | Stratmann-Scuseria | time | Becke (Becke adjustment) | time |
|---|---|---|---|---|
| `low` | −533.954518687 | 121 s | −533.954662894 | 120 s |
| `medium` | −533.954544224 | 157 s | −533.954503626 | 181 s |
| `high` | −533.954570020 | 226 s | −533.954514681 | 260 s |
| `best` | −533.954520631 | 1412 s | −533.954520090 | 1776 s |

g09 reference, `BLYP/6-31G(d) 6D SCF=(Tight,Conver=10) Int(Grid=199974)`: **−533.954519147**
(22 cycles). Differences from it: SS +4.6e-07 / −2.5e-05 / −5.1e-05 / −1.5e-06; Becke
−1.44e-04 / +1.55e-05 / +4.5e-06 / −9.4e-07 for `low` / `medium` / `high` / `best`. Cost
relative to `low`: 1.5×, 2.2×, 15×. Dylan's decision 2026-09-11: `medium` stays the default;
adaptive pruning is to reduce cost before `high` is reconsidered.

For comparison the water ladder (3 atoms, cc-pVDZ) is in `DFT_STANDARDISATION.md` §6b: there
the grid dominates and Becke is faster; here the SCF dominates and SS's screening is worth
about 15%.

## Where the time goes: SCF phase timers, 2026-09-11

The SCF now accumulates CPU seconds per phase (`SCF_DATA.time_*`, a `Time` column in the
iteration table, a summary after the results). Karrikinolide BLYP/6-31G(d), Becke partition,
`accuracy= medium`, 14 iterations, one core, release build:

| phase | CPU s | share |
|---|---|---|
| XC matrix and energy (grid) | 161.7 | 67% |
| Fock build, J and K | 66.4 | 28% |
| Initial guess (promolecule) | 12.0 | 5% |
| MO update (diagonalisation) | 0.43 | 0.2% |
| DIIS extrapolation | 0.19 | 0.1% |
| One-electron matrices, density, grid construction | < 0.05 | |
| total | 240 | wall 230 s |

Per iteration: 13-19 CPU s, of which about 11.5 s is XC and 4.7 s is J and K.

So on a DFT job the exchange-correlation quadrature, not the two-electron integrals, is
the target: two thirds of the time. The J and K build is the other third. Everything else is
noise. Water (cc-pVDZ, `medium`) shows the same split: XC 64%, guess 28%, J and K 7%.

The XC phase is `MOLECULE.FOCK:add_GGA_XC_mx` and what it calls: the atom grids
(`make_rho_becke_atom_grid`: basis functions and their gradients on the grid, the density and
its gradient), the functional evaluated twice per batch (energy density and potential
separately, `DFT_STANDARDISATION.md` §9), and the matrix contraction over shell pairs and
points. Which of those dominates is the next measurement (gprof).

## gprof flat profile of the same job, 2026-09-11

`-pg -g` release build in a separate worktree (`~/github/tonto-prof`, build dir `prof/`),
karrikinolide `medium`, 15 SCF iterations. gprof attributes 139 of the 240 CPU s (sampling
and inlining); the proportions are what matter:

| routine | self s | share of sampled | calls |
|---|---|---|---|
| `MOLECULE.FOCK:add_GGA_XC_mx` (inner: shell-pair × point contraction) | 46.2 | 33% | 255 |
| `MOLECULE.RHO:make_rho_becke_atom_grid` (basis functions and gradients on the grid, density) | 45.1 | 32% | 255 |
| `RYS:get_weights` | 9.2 | 6.6% | 613 M |
| `SHELL1QUARTET:form_esfs` | 8.1 | 5.8% | 5.4 M |
| `SHELL1QUARTET:make_esfs` (dispatcher) | 6.2 | 4.5% | 35 M |
| `BECKE_GRID:set_unique_atoms` | 3.3 | 2.4% | 259 |
| `SHELL1QUARTET:set_cd_new` | 3.2 | 2.3% | 38 M |
| all other `make_esfs_*` specialisations together | ~7 | 5% | |
| `RYS:create` / `destroy`, `MAT{REAL}:create` / `destroy` | ~2.5 | 2% | 40 M / 105 M |

Reading it:

- **The XC quadrature is two routines, equal in cost**, and both scale with the number of
  grid points: evaluating the basis functions and their gradients on every point of every
  atom grid on every iteration (255 = 15 iterations × 17 atoms), and contracting the weighted
  potential with the shell pairs over the points. Halving the points halves two thirds of the
  run. **Adaptive pruning is therefore the first speed item as well as the accuracy item.**
- **3.3 s is labelled `set_unique_atoms`, 259 calls**, but the call graph hangs it under
  `make_partitioned_grid`, which does not call it; with `-Ofast` the label is almost
  certainly the inlined Becke partition (`apply_partition` / `partition_B_B_adj`, which do
  not appear at all). Either way it is per-atom-grid, per-iteration work that depends only
  on the geometry: the partitioned atom grids (points and weights) are rebuilt on every
  iteration and could be built once. A free 2%, and more once pruning makes the grid
  construction itself smarter.
- **The two-electron integrals are 22% in total.** The Rys roots are 6.6% of the run, a third
  of the ERI cost; a perfect vectorisation of them saves at most that. The `make_esfs`
  dispatch and `set_cd_new` (per-quartet pair set-up, 38 M calls) are another 7% between
  them, which is what class batching would remove. `RYS:create` is called 40 M times — once
  per shell quartet — and `MAT{REAL}:create` 105 M times: allocation churn inside the quartet
  loop, small but pure waste.
- Diagonalisation, DIIS and the density matrix do not appear.

**Revised order for the plan:** (1) done, these tables; (2) the free ones; (3) wake the
delta-density Fock build (saves J/K work on late iterations, up to ~25% of the run); (4)
adaptive pruning, which attacks the 67%; (5) class batching and the Rys vectorisation, which
together address perhaps 10-15%.

## Step 2, 2026-09-11: the free ones, and what they turned out to be worth

- **`.max_I` across iterations.** The survey said the Schwarz bounds were rebuilt in every
  Fock build. Reading it properly: `initialize_SCF` already builds them once (after its own
  first Fock build) and `cleanup_scf` destroys them, so inside an SCF they were cached and
  only that first build and the non-SCF Fock builds (properties, HAR) paid an extra (ab|ab)
  pass. The JK drivers now never destroy `.max_I`; the geometry-change routines in
  `MOLECULE.SET` do. Correct, and nearly free -- but worth a few tenths of a second here,
  not the iteration-per-pass the plan assumed.
- **Caching the partitioned atom grids** (the 2.4%) is deferred. The cheap version caches
  the basis-function values on the pruned grid per atom, which is a quarter of a gigabyte on
  karrikinolide and grows with the molecule; the right version keeps only a point-index map
  from the unscaled grid through `compress_zeros` and `prune_grid`, which is a change to the
  grid-construction routines that adaptive pruning rewrites anyway. Do it there.

## Step 3, 2026-09-12: the delta-density Fock build, and what it exposed

**Three latent defects in `MOLECULE.BASE:make_SCF_density_mx`**, all found by switching the
delta build on for the first time:

1. Nothing allocated `.delta_density_mx`, so `use_delta_build= TRUE` (the default) had never
   built anything incrementally. `initialize_SCF` now allocates it when the flag is on, and
   `cleanup_scf` destroys it.
2. The "old" density was saved *after* the density matrix had been destroyed and re-created,
   so the saved copy was whatever the fresh allocation contained. The first increment worked
   by allocator luck; the second was garbage (+147 Hartree on water). Now saved first.
3. The old density was destroyed at the end of every call, so the next call's damping test
   ("is there an old density?") always failed: **damping has never actually been applied**,
   although the table said `*Damping on`. It is now kept, compressed, and damping runs for
   iterations below `damp_finish` as designed.

**Measured on karrikinolide, `medium`, Becke, damping now live:**

| run | iterations | J and K CPU s | final energy − full build | late-iteration ΔE |
|---|---|---|---|---|
| full builds (reference) | 14 | 46.6 | 0 | monotonic to 1e-9 |
| delta, normal cutoffs (1e-9) | 18 | 46.4 | +4e-9 | wobbles ±1e-6 |
| delta, damping off | 20 | 58.7 | −7e-7 | wobbles ±1e-6 |
| delta, full rebuild every 3 | 19 | 68.5 | −1e-10 | wobbles ±3e-7 |
| delta, cutoffs 1e-12 | 15 | 60.4 | −1e-8 | monotonic |

The increments are noisy at the 1e-6 level under the normal Schwarz cutoffs (the test is
against an absolute cutoff, and a small ΔP puts far more quartets under it), and DIIS then
chases that noise for extra iterations. Tightening the cutoffs to 1e-12 restores clean
convergence but admits as many quartets as a full build. **Dylan's decision: `use_delta_build`
defaults to FALSE**; the machinery is correct and opt-in. A ΔP-scaled screening (Ahmadi and
Almlöf) is the version that would pay, and it belongs after adaptive pruning, since J and K
are a quarter of the run here and the XC quadrature two thirds.

`set_using_direct_SCF` used to force `using_delta_build= TRUE`; removed.

## g09 on the same job, for scale

g09, BLYP/6-31G(d), 6D, `SCF=(Tight,Conver=10)`, its default FineGrid, one core:
**49 s wall, 23 cycles**, energy −533.954537963, i.e. 1.9e-5 from its own converged
(199974-point) value. Tonto at `medium`/Becke: 177 s, 14 iterations, 1.6e-5 from the same
converged value. At matched accuracy g09 is 3.6× faster on the job and about 6× faster per
iteration (2.1 s against 12.6 s). Since Tonto's iteration is two thirds XC quadrature on a
grid about the size of FineGrid, the per-iteration gap is mostly in the XC evaluation itself,
not the number of points: the per-point cost of the basis-function evaluation and the
contraction. That narrows the target further.

## Stage B, 2026-09-13: the per-shell angular error, measured

`put_grid_shell_errors` (a keyword; run after `scf` on a restricted density) integrates,
for every atom and every radial shell of its grid, the shell's contribution to the electron
count and to the Dirac exchange energy at the Lebedev order the pruning scheme assigns and at
L = 5, 11, 17, 23, 29, against L = 59, Becke-partitioned like the real grid. Tables and the
summariser `shell_summary.py` (minimal order per shell for a target error) are in
`~/tonto_runs/grid_shell_errors_2026-09-13/`.

Water, BLYP/cc-pVDZ, Becke partition. "Oracle" means the order chosen per shell from the
converged density, i.e. the best any rule can do:

| grid | points used | Σ per-shell error, used | oracle at 1e-7 per shell | oracle at 1e-8 |
|---|---|---|---|---|
| `medium` (30/35 radial, L23/L29) | 12490 | 4.7e-07 | 6772 pts (54%), 1.2e-06 | 11622 (93%), 1.2e-07 |
| `best` (65/70 radial, L59/L71) | 142492 | 5.3e-08 | 12454 pts (9%), 2.3e-06 | 22608 (16%), 2.0e-07 |

What the shell profiles say, the same for O and H (both scale factor 5):

- **Inner, r < 0.25 bohr: L5 is exact to 1e-9.** Treutler-Ahlrichs' inner third is right.
- **0.25 to 0.7 bohr: L11 is exact to 1e-8.** Also right.
- **0.7 to 3 bohr, the bonding shells: the order must rise to L23-L29**, and for 1e-8 per
  shell on oxygen to L59 between 1.5 and 2.4 bohr. This is where `medium` (L29) leaves
  1e-7 per shell and where all of its residual against g09 lives; `best`'s L71 there is
  1e-15, i.e. 6 orders more than needed.
- **Beyond 3 bohr the order falls again**: L17 to 5 bohr, L11 to 6, L5 to 7.
- **Beyond about 7 bohr the shells contribute nothing** (|X| < 1e-8 at any order): 3 of
  35 shells at `medium`, 10 of 70 at `best` -- 8% and 25% of the points -- exist because
  `basis_fn_cutoff` = 1e-10 puts `r_max` at 9-12 bohr.

So the shape is SG-1's five zones, stated in bohr, with the bonding zone needing *more*
than today's `medium` and both ends needing far less. The oracle profiles above are the
target for stage D's rule; stage C (drop points by promolecule density) takes the far end.

Karrikinolide at `medium` (17 atoms, 84590 points), the same diagnostic:

| target per shell | oracle points | of today's | summed error |
|---|---|---|---|
| 1e-5 | 21976 | 26% | 7.3e-04 |
| 1e-6 | 42838 | 51% | 7.8e-05 |
| 1e-7 | 96550 | 114% | 6.2e-06 |
| 1e-8 | 141470 | 167% | 5.7e-07 |
| today's scheme | 84590 | 100% | ~5e-05 |

The profiles: every atom is over-resolved inside 0.7 bohr and beyond 4 bohr, the last
three shells contribute nothing, and the carbons need L59 in their bonding shells (0.9 to
2.6 bohr; the C–C and C=O bonds have more angular structure than water's O–H) for 1e-7 per
shell, where `medium` gives them L29. Oxygen and hydrogen need less there. So a zone rule
with the full order concentrated in the bonding zone and low orders at both ends delivers
**today's `high` accuracy at about `medium` cost** (114% of the points against 220% of the
time for `high`) -- the target Dylan set for reconsidering the default. The zone edges in
bohr are the same for O and H in water and for O and H in karrikinolide; the carbon bonding
zone reaches further out (to 2.6 bohr against 1.9 for H), i.e. the outer edge tracks the
bond length, and the inner edges do not move.

## Stage C, 2026-09-14: pruning the molecular grid by the promolecule density

`BECKE_GRID.prune_rho_cutoff` (default 1e-12, keyword `prune_rho_cutoff=`, zero keeps every
point): `MOLECULE.RHO:make_XC_grid` drops a point when the sum of the spherical atomic
densities of its atom's `overlapping_atom` list is below it. The atomic interpolators are made
silently in `initialize_SCF`, after the guess. The core is never touched, and nothing is ever
thresholded on the weight. `show_timings= TRUE` now also prints the point counts before and
after pruning.

Water, BLYP/cc-pVDZ, `medium`, Becke; unpruned 11854 points, −76.400238578082:

| `prune_rho_cutoff` | points dropped | energy change |
|---|---|---|
| 1e-12 (default) | 3.3% | 0 |
| 1e-11 | 3.3% | 0 |
| 1e-10 | 3.3% | 0 |
| 1e-9 | 5.9% | 1.3e-10 |
| 1e-8 | 6.6% | 7.8e-09 |
| 1e-7 | 7.7% | 7.9e-08 |
| 1e-6 | 11.6% | 1.3e-06 |

Karrikinolide, `medium`, Becke, one core, run back to back:

| run | points | energy | XC CPU s | J/K CPU s | wall |
|---|---|---|---|---|---|
| `prune_rho_cutoff= 0` | 78926 | −533.954503625952 | 120.0 | 44.3 | 166 s |
| default 1e-12 | 77594 | −533.954503625954 | 122.1 | 44.3 | 168 s |

**Exact, and worth almost nothing.** The cut is inert up to the functional's own
`rho_cutoff` (1e-10), and pruning becomes lossy only above it, as the tail analysis predicted.
But at a safe threshold it removes 2-3% of the points: `basis_fn_cutoff` already truncates
each atom's radial grid, and the density at the truncation radius is still above 1e-12. The
stage B estimate of 8% counted shells whose *contribution* is below 1e-8, which is a different
and much looser test. No measurable time is saved. The saving has to come from stage D, the
angular orders.

**Dylan's decision, 2026-09-14: keep it on at 1e-12.** It is exact to rounding and costs
atomic SCFs at SCF start only for DFT jobs without a promolecule guess.

## Stage D, 2026-09-14: `pruning_scheme= adaptive`, the angular order by radius

The rule, its calibration and the energies against g09 are in `DFT_STANDARDISATION.md` §6c.
Karrikinolide, BLYP/6-31G(d), Becke, one core, CPU seconds from `show_timings`:

| grid | points | XC s | J/K s | guess s | wall |
|---|---|---|---|---|---|
| `medium` treutler_ahlrichs | 77594 | 125.5 | 46.1 | 9.8 | 173 s |
| `medium` adaptive | 61089 | 92.9 | 44.5 | 7.3 | 139 s |
| `high` treutler_ahlrichs | 125472 | 209.8 | 45.1 | 15.1 | 257 s |
| `high` adaptive | 162934 | 296.8 | 45.0 | 20.9 | 343 s |

The XC time follows the point count (1.5-1.8 ms per point per SCF); J and K do not move. At
`medium` the saving is a quarter of the XC time for no change in the energy. At `high` the L59
bonding zone costs more than it buys. The next saving is stage E, the batched XC loop.

## perf profile and the no-copy change, 2026-09-14

`perf record -g` (and `--call-graph dwarf` for callers) on karrikinolide, adaptive `medium`:

| routine | share |
|---|---|
| `MOLECULE.RHO:make_rho_becke_atom_grid` (restricted GGA) | 22.3% |
| `MOLECULE.FOCK:add_GGA_XC_mx` (inner) | 20.8% |
| `memmove` (libc), 8.8% from each of the two above | 15.4% |
| `malloc`, `free` | 4.9% |
| ERI: `get_weights` 6.0, `form_esfs` 5.3, `make_esfs` 4.3, `set_cd_new` 2.6, J engines 2.4, `make_esfs_*` ~6 | ~27% |
| `exp` (libm) | 2.3% |
| BLAS/LAPACK (reference netlib) | 0.1% |

A fifth of the run was copying and allocating inside the shell-pair loops of the two XC
routines: allocatable assignments `ga0 = bf_grd0(sa)[:,a]` (eight per basis-function pair),
`skipb = bf_skip(sb).element` and `DD = D(fa:la,fb:lb)` per shell pair, and `pi/pj/pn`
created and destroyed per shell pair. gfortran does not elide these. Reading the grids in
place and allocating the maps once per atom grid:

| | energy | XC CPU s | J/K CPU s | wall |
|---|---|---|---|---|
| before | −533.954503508086 | 92.9 | 44.5 | 139 s |
| no-copy | −533.954503508086 | 75.0 | 46.8 | 123 s |

Bit-identical, XC −19%. What is left of the XC cost is the per-shell-pair mask merge and
index maps over every point of the atom grid, and the pair-by-pair contraction: stage E.

## No-grid J/K profiles, 2026-09-15

Karrikinolide RHF (no XC), promolecule guess, convergence 1e-8, `perf record -g`, release
`build/` at `4b37c9da`. Inputs and `perf.data` in `~/tonto_runs/rys_profile_2026-09-15/`.

| basis | energy | wall | J/K share of SCF |
|---|---|---|---|
| 6-31G(d) | −530.980810596157 | 51 s | 95.6% |
| cc-pVTZ | −531.169266412927 | 1245 s | 97.4% |

| routine (share of run) | 6-31G(d) | cc-pVTZ |
|---|---|---|
| `RYS:get_weights` + its `exp` | ~19% | ~10% |
| `SHELL1QUARTET:form_esfs` | 12.0% | 24.3% |
| `make_esfs` (low-l specialisations inlined) | 10.3% | 4.5% |
| J/K engine (`make_r_JK_engine`, both levels) | 11.8% | 14.0% |
| `malloc` + `free` | ~9% | ~6.5% |
| `set_cd_new` | 5.4% | 3.4% |

- Two thirds of the 6-31G(d) Rys calls come from the low-l routines inlined into `make_esfs`.
- With f functions `form_esfs` dominates; 10.7% of the run reaches it from `make_esfs_XX`,
  the generic path for l sums of 3 or more on both sides. The engine's own cost is the K
  contraction loop. `get_weights6` (6 or more roots) is 0.3%.
- The `malloc` calls come from `set_shell1q_cd_from` (inlined `set_cd_new` and the
  whole-`SHELL1` copies), `make_r_fock_mx` and the engine's work vectors.

**Allocation hoisting.** Each step timed side by side with the previous binary on 6-31G(d),
under the same background build load, so only the ratios are meaningful:

| step | energy | J/K CPU s | `malloc`+`free` |
|---|---|---|---|
| baseline | −530.980810596157 | 73.85 | 8.3% |
| 1a: reused pair arrays and `SHELL1` storage | −530.980810596157 | 70.68 | 4.0% |
| 1a (second pairing) | −530.980810596157 | 80.54 | 4.3% |
| 2: fixed-size `RYS`, no per-call `RYS` allocation | −530.980810596155 | 77.29 | 3.2% |

| 2 + RMS code, switched off (`rys-rms`) | −530.980810596155 | 52.07 | 2.8% |
| 1c: `ERI_SCRATCH` sized by a dry run | −530.980810596155 | 49.17 | 0.2% |

Step 1a is bit-identical and −4.3%; step 2 is −4.0% on top and moves the energy by 2e-12,
presumably from changed vectorisation of the fixed-size weight arrays. The remaining
allocations are the `make_esfs_*` 2-D integral arrays and the engine work vectors.

The last two rows are a separate pairing, run with no background build, so their times
compare with each other only. Step 1c moves those arrays into one `ERI_SCRATCH`, sized
before the quartet loop from the shell pairs' l sums and primitive counts: J/K −5.6%, energy
unchanged, allocation down to 0.2% of the run. The per-quartet size check costs 0.6%.

On cc-pVTZ, the same pairing: energy −531.169266412929 for both, J/K 1203.49 → 1120.31 CPU s
(−6.9%), `malloc`+`free` about 3.8% → 0.5%. With the work arrays passed at fixed shapes the
compiler folds `form_esfs` into the generic routines, so the contraction now shows under
`make_esfs_XX` (15% of the run) and its siblings.

**Every builder on `ERI_SCRATCH`** (J-only, open-shell, and the `make_ERI` direct, nosym and
CIS builders), run side by side with the `rys-rms` binary, all eight jobs at once:

| job | energy (both) | before | after |
|---|---|---|---|
| BLYP 6-31G(d) `medium`, J-only engine | −533.954503625982 | J 59.86 s, wall 92.0 s | J 56.34 s (−5.9%), wall 88.9 s |
| RHF 6-31G(d), J+K engine | −530.980810596155 | J/K 67.56 s | J/K 62.65 s (−7.3%) |
| short UHF water cation, open-shell engine | stdout identical | 0.21 s | 0.21 s |
| short spherical cc-pVTZ water, `make_ERI` path | stdout identical | 2.80 s | 2.37 s (−15%) |

The spherical gain is mostly `change_to_spherical` no longer copying the whole
spherical-harmonic table on every quartet.

**Reduced multiplication scheme** (`form_esfs_rms2`, `scfdata= { use_rms_esfs= }`), same
binary, off and on side by side:

| basis | energy (off and on) | J/K CPU s off | J/K CPU s on | contraction share off / on |
|---|---|---|---|---|
| 6-31G(d) | −530.980810596155 | 51.01 | 51.72 (+1.4%) | 13.1% / 14.3% |
| cc-pVTZ | −531.169266412929 | 1196.07 | 1256.32 (+5.0%) | 25.9% / 29.5% |

Exact, and slower in both. It saves one multiply per shared `Ix*Iy` column but still takes one
dot product of length n_sum per `(e,f)` component pair, and adds a stored product vector and
scattered writes. The hot instructions are the vectorised sums, not the index lookups.

## Tonto against g09 and ORCA, 2026-09-16

Karrikinolide, 17 atoms, geometry from the same g09 checkpoint as every run above. One core
each, run one at a time on an idle machine. Tonto is the `rys-1c` binary. Convergence 1e-8
with a promolecule guess; g09 `SCF=(Tight,Conver=10)`; ORCA `TightSCF`. Inputs, outputs and
timings in `~/tonto_runs/vs_g09_orca_2026-09-16/`.

**Read the timings with the accuracy caveat below** — at Tonto's default cutoffs these runs are
not accuracy-matched to the other two codes.

Cartesian comparison (g09 forced to `6D 10F` to match Tonto's default):

| job | Tonto | g09 |
|---|---|---|
| RHF/6-31G(d), 177 fn | 43.8 s | 6.9 s |
| BLYP/6-31G(d) | 60.5 s | 38.6 s |
| RHF/cc-pVTZ, 475 fn | 857 s | 907 s |
| BLYP/cc-pVTZ | 810 s | 591 s |

Spherical comparison (ORCA has no cartesian option; Tonto `use_spherical_basis= TRUE`):

| job | Tonto | ORCA exact | ORCA default | g09 |
|---|---|---|---|---|
| RHF/6-31G(d), 166 fn | 54.2 s | 42.8 s | 42.8 s | — |
| BLYP/6-31G(d) | 79.0 s | 54.4 s | 17.0 s | — |
| RHF/cc-pVTZ, 414 fn | 1158 s | 780 s | 726 s | — |
| BLYP/cc-pVTZ | 1471 s | 707 s | 50 s | — |
| RHF/def2-SVP | 54.6 s | 42.4 s | 43.2 s | 8.5 s |
| BLYP/def2-SVP | 81.7 s | 53.9 s | 17.2 s | 46.3 s |
| RHF/def2-TZVP | 900 s | 604 s | 604 s | 785 s |
| BLYP/def2-TZVP | 1089 s | 635 s | 47 s | 510 s |

The def2 rows are spherical in all three codes, g09 at its default 5D 7F: the def2 sets are
defined for spherical harmonics, so a cartesian def2 run compares something nobody uses.

**What the table says.** The six-fold gap at 6-31G(d) is a small-basis effect and not worth
chasing — g09 optimises that case heavily. At triple zeta Tonto is level with g09 on cartesian
RHF (857 against 907 s) and 1.15x on def2-TZVP. The weak case is **DFT on a spherical basis**:
2.1x g09 at def2-TZVP and 2.1x ORCA at cc-pVTZ, because the spherical path cannot reach the J
engine at all. That is what the `rys-sph` branch addresses.

**ORCA's default is density fitting, not better integrals.** For BLYP it uses RI-J with the
`def2/J` auxiliary set (`SPLIT-RIJ`), which is where all of its speed comes from: 17.0 s against
54.4 s for its own exact run at 6-31G(d), 50 s against 707 s at cc-pVTZ. It costs 3.9e-4 Eh at
6-31G(d) and 4.0e-4 at cc-pVTZ, and the fitted energy is *below* the exact one, so it is not
variational. For Hartree-Fock, ORCA used no approximation: `NoRI` and the default gave the same
energy to every digit and the same time to 50 ms. ORCA and g09 both do incremental Fock builds;
Tonto's delta build exists and is switched off.

### The ~1e-6 energy difference was Tonto's screening, not a defect

g09 and ORCA agree with each other to about 4e-9 on the def2 RHF jobs. Tonto differed from that
pair by 8.1e-7 at def2-SVP and 5.6e-7 at def2-TZVP, **and the sign flipped between them** — the
signature of discarded contributions rather than a missing term.

With every cutoff at 1e-15 (`eri_schwarz_cutoff=`, `eri_j_density_cutoff=`,
`eri_k_density_cutoff=`, `eri_bf_overlap_cutoff=`, `eri_primitive_pair_cutoff=`), RHF/6-31G(d):

| | default cutoffs | at 1e-15 | reference |
|---|---|---|---|
| cartesian | −530.980810596 | **−530.980808227** | g09 −530.980808229 |
| spherical | −530.979037749 | **−530.979034991** | ORCA −530.979034988 |

Agreement improves from ~2.5e-6 to **1.8e-9 against g09 and 2.7e-9 against ORCA**, which is the
level those two agree with each other. So the integrals, the basis handling and the spherical
transform are all sound; the defaults are simply loose — `ERI_primitive_pair_cutoff` is
`TOL(6)`, the other three `TOL(9)` (`types.foo`).

**The cost is about +48%** on RHF/6-31G(d): 43.8 → 65.0 s cartesian, 54.2 → 80.4 s spherical.

**So the timings above understate the gap at matched accuracy.** Tonto's default run trades
roughly a microhartree for about a third of the run time; g09 at `SCF=Tight` and ORCA at
`TightSCF` are not making that trade. A like-for-like RHF/cc-pVTZ figure would be nearer 1270 s
than 857 s. No default has been changed.

ORCA's remaining 3.2e-7 on the nuclear repulsion was its Angstrom-to-bohr conversion constant:
given the geometry in bohr (`! ... Bohrs`), its V_NN is 576.09404496423485 against Tonto's
576.094044964235, identical to fourteen digits.

### How much of it is each cutoff

RHF/6-31G(d) cartesian, `rys-1c` binary, one at a time from the defaults (primitive pair
`TOL(6)`, the other three `TOL(9)`). g09 gives −530.980808229.

| setting | energy | from g09 | wall |
|---|---|---|---|
| defaults | −530.980810596 | 2.4e-6 | 43.8 s |
| primitive pair 1e-9 | −530.980808198 | 3.1e-8 | 46.7 s (+7%) |
| primitive pair 1e-12 | −530.980808197 | 3.2e-8 | 49.8 s (+14%) |
| primitive pair + Schwarz + J/K density, all 1e-12 | −530.980808227 | **1.8e-9** | 57.2 s (+31%) |
| all five at 1e-15 | −530.980808227 | 1.8e-9 | 65.0 s (+48%) |

**The primitive-pair cutoff is nearly the whole story, and it is cheap to fix.** Moving it alone
from 1e-6 to 1e-9 removes 99% of the error — 2.4e-6 down to 3.1e-8 — for 7% in time. Tightening
it further buys nothing on its own; the residual 3e-8 is the Schwarz and density cutoffs, and
clearing that costs 31%. Beyond 1e-12 nothing changes but the clock.

That is the shape a named accuracy level wants: a cheap `low` at 1e-9 primitive-pair (1e-8
accuracy for 7%), and `medium`/`high` tightening all four together for 1e-9 accuracy at 31%.

## The spherical path on the cartesian engine, 2026-09-16

Branch `rys-sph` (`505fb888`), measured against `rys-1c` back to back on an idle machine.
Karrikinolide RHF/cc-pVTZ spherical, 414 basis functions, f functions throughout:

| path | energy | wall | J and K |
|---|---|---|---|
| direct builder (`rys-1c`) | −531.166917814557 | 1231 s | 1220.6 s, 97.6% |
| cartesian engine (`rys-sph`) | −531.166917812066 | **868 s** | 856.9 s, 97.4% |

**−30%, and the energies agree to 2.5e-12** — the residual being the screening decisions, which
are now taken on cartesian Schwarz bounds. Water cc-pVTZ spherical (d and f) is identical to all
twelve printed decimals.

The spherical run now costs the same as the cartesian one (868 against 857 s), which is what the
algebra says it should: the same cartesian integrals, minus the per-quartet rotation and copies.
Against ORCA's exact spherical run (780 s) Tonto goes from 1.48x to 1.11x.

Note the direct path measured 1231 s here against 1158 s earlier the same day on the same
binary and input — about 6% of run-to-run spread on this machine, so only the side-by-side
pairing is meaningful.

## Named ERI accuracy levels, 2026-09-16

`eri_accuracy=` sets the primitive-pair, Schwarz and J/K density cutoffs together; the four
levels come straight from the scan above. Karrikinolide RHF/6-31G(d) cartesian, one core,
g09 gives −530.980808229:

| `eri_accuracy=` | four cutoffs | energy | from g09 | cpu |
|---|---|---|---|---|
| *(no keyword)* | 1e-6, 1e-9, 1e-9, 1e-9 | −530.980810596152 | 2.4e-6 | 42.0 s |
| `very_low` | 1e-6, 1e-9, 1e-9, 1e-9 | −530.980810596152 | 2.4e-6 | 45.1 s |
| `low` | 1e-9, 1e-9, 1e-9, 1e-9 | −530.980808198255 | 3.1e-8 | 47.4 s |
| `medium` | all 1e-12 | −530.980808227204 | **1.8e-9** | 57.9 s |
| `high` | all 1e-15 | −530.980808227207 | 1.8e-9 | 64.9 s |

`very_low` reproduces the default energy to every printed digit, which is what it is for: the
levels change nothing until one is asked for. The three tightened rows reproduce the
one-at-a-time scan above exactly.

`escalate_eri_accuracy= TRUE` runs the damped iterations at `very_low` and the rest at the level
asked for. With `medium` that gives −530.980808227196, 8e-12 from the unescalated run and two
orders inside the level's own accuracy, in 53.3 s against 57.9 s -- about 8%. Two of the
thirteen iterations are damped, so that is the size of saving the schedule can give.

An x-ray constrained SCF is floored at `medium`, in `SCF_DATA:effective_ERI_accuracy`. The floor
applies only once a level has been asked for, so existing XCW jobs are untouched; when it bites,
the options block says so with an `ERI accuracy in force` line.

## Where the J/K time goes, by basis, 2026-09-16

`perf record --call-graph=dwarf` on karrikinolide RHF, cartesian, one core, the `develop` binary
after the `rys-1c`/`rys-sph` merges. Both profiles and their `perf.data` are kept in
`~/tonto_runs/scf_profiles_2026-09-16/`. 6-31G(d) is 41 s and has d functions; cc-pVTZ is 930 CPU
s and has f throughout.

| symbol | 6-31G(d) | cc-pVTZ |
|---|---|---|
| `make_esfs_*` family, all members | **40.5%** | **51.1%** |
| `RYS:get_weights` | 17.5% | 8.7% |
| `make_esfs_with` (generic, high l) | 14.4% | 6.3% |
| `make_esfs_xx` | 2.7% | 15.6% |
| `make_esfs_dx` / `_xd` / `_px` / `_xp` | 4.5 / 4.2 / 4.2 / 3.6% | 6.8 / 6.0 / 5.0 / 4.5% |
| `make_r_JK_engine` body (`_1`) | 9.5% | 12.9% |
| `VEC{REAL}:to_product_of` | 2.9% | 4.4% |
| `transfer_l_*` family | 3.1% | 6.3% |
| `exp` from libm | 6.4% | 2.4% |
| allocator: `malloc`, `free`, `memmove`, `memcpy` | **1.1%** | **1.0%** |

**The allocator is gone.** One percent in both, which is what stage 1c bought and is why moving
the `transfer_l_*` work arrays onto `ERI_SCRATCH` was dropped: the transfer family's remaining
3-6% is its own arithmetic, not allocation, and hoisting buffers would not touch it.

**The shape changes with the basis, and that decides what to vectorise.** At 6-31G(d) the roots
dominate -- `get_weights` alone is 17.5%, and `make_esfs_with`, the generic high-l routine that
calls it, another 14.4%. At cc-pVTZ the roots fall to 8.7% while `make_esfs_xx` climbs from 2.7%
to 15.6%: with f functions the 2D integral construction and the contraction outweigh the roots,
which is what the earlier no-grid profiles said.

So **T-range binning of the roots is the 6-31G(d) lever and class batching is the cc-pVTZ one**,
and the `make_esfs_*` family is the common target at 40-51% of the run in both. A pilot should
take one member, restructure it, and measure on both bases -- a change tuned on d functions alone
would be optimising an 8.7% line at triple zeta.

## Rys step 3, 2026-09-16: vectorising the roots within a shell quartet

Branch `rys-vec`. The 1 and 2 root fits were written a second time as slice kernels, one leaf
per T range so a contiguous slice of X runs as one straight loop (`RYS:get_weights1_t2_n`,
`get_weights2_t2_n`); `RYS:get_weights_t2(n,X,root,weight)` is the vector entry, and the
eleven low-l `make_esfs_*` routines with 1 or 2 roots (`ps_psss` ... `pd_pspp`) gather every
primitive quartet's X in a pre-pass and make one call. The scalar trees are untouched;
`runfiles/run_rys.foo` checks the two copies agree and times both. Everything below is
karrikinolide RHF, one core; runs and `perf.data` in `~/tonto_runs/rys_vec_2026-09-16/`.

**The kernels themselves are fast.** `run_rys`, idle machine, ns per X, scalar tree against
vector call, all X in one T range: 1 root 23.6 → 7.4 at batch 81; 2 roots 31.9 → 10.5. Three
to four times, from batch 9 upwards. Agreement with the scalar path is 4.4e-16 (one ulp of
`-Ofast` reassociation); the scalar path's output is bitwise identical to `develop` on a
16 000-point dump.

**The batches are not in one range.** Counters compiled in for one run (never in a real
build), 6-31G(d), X evaluated per SCF by the 2 root fit:

| batch size | 1 | 2-9 | 10-81 | 82-1296 |
|---|---|---|---|---|
| X in same-range batches | 1.5 M | 11.0 M | 24.4 M | 12.2 M |
| X in mixed-range batches | -- | 34.3 M | 99.7 M | 33.7 M |

77% of the 217 M X sit in batches that straddle a T range; the 1 root fit is 72%. Batches of
one are under 1%. A min/max test that sends a mixed batch to the scalar path therefore
vectorises about a fifth of the work.

**Grouping by range inside the call** (label each X, count, gather into per-range slices, run,
scatter back) is 1.6x the scalar path on a representative mixed batch (X on [1,15), four
polynomial ranges) at 81 X and above, and *slower* at 9 X, where the labelling and scatter
cost more than the leaf. It is switched on only for mixed batches of 32 or more
(`RYS::group_min`).

**Whole-job result: flat.** Three binaries run concurrently, three repeats, J and K CPU s:

| repeat | `develop` | min/max only | min/max + grouping ≥ 32 |
|---|---|---|---|
| 1 | 60.37 | 60.06 | 60.28 |
| 2 | 62.71 | 63.17 | 64.70 |
| 3 | 64.00 | 63.97 | 64.36 |
| mean | 62.4 | 62.4 | 63.1 |

Energies −530.980810596152 / ...151 / ...157. Single pairings earlier the same evening had
given −4.3%, +2.6% and +1.4% for the same three: **the pair-to-pair spread of one binary is
±2%, and a single side-by-side pair cannot resolve a change of that size.** cc-pVTZ, one pair,
min/max binary: 1108.3 → 1091.3 s (−1.5%), energy within 1e-11.

`perf` on the vectorised binary shows the Rys symbols' share falling from 24% to 21% of the
run while the clock does not move, and three concurrent copies of either binary each run about
15% slower than one alone. Both say the J/K build is not bound by the roots' arithmetic on
this machine: memory traffic through the 2-D integral buffers is the more likely limit, and
that is what class batching would have to address, not the roots.

**What this does not say.** One 17-atom molecule. A larger system moves the X distribution
outward and screens more quartets away, so the same-range fraction and the batch shape can
both differ; the counters above are the measurement to repeat before generalising.

**One defect found by the way.** `get_weights_t2` is reached with n = 0 -- a quartet whose
primitive pairs were all screened out. Silent in release (`minval` of an empty array, an empty
loop); a debug build's `ENSURE` stopped on it. It returns at once now.

## The ERI default moved to `low`, 2026-09-16

`ERI_primitive_pair_cutoff` 1e-6 → 1e-9 (`types.foo`), `ERI_accuracy` default `"low"`;
`very_low` now names the old cutoffs and reproduces the older references. Cost on karrikinolide
RHF/6-31G(d): +7% for 2.4e-6 → 3.1e-8 from g09 (the scan above). Dylan's decision.

Full rebuild, `scripts/suite_report.py`:

| suite | loose | last-digit | exact | moved by the cutoff | not the cutoff |
|---|---|---|---|---|---|
| `short` | 56/56 | 55 | 53 | `nhfcl_rhf_DZP_unit_cell_refractive_indices` (0.36% on one value, one unit) | `h2o_rhf_cc-pVDZ_tdhf` (known runner-sensitive, relaxed bound); `4AP_rhf_STO-3G_read_fchk_and_cif_z_p_2` (ESD suffix `(5)`→`(6)` on four U_eq, reference from another kernel -- no ERIs in the job) |
| `long` | 32/32 | 32 | 27 | `cyclazine_rhf_cc-pVDZ_VMO_canonicalization`, `quartz_NN_HAR_L0/L1_rhf_def2-SVP`, `so2_rhf_DZP_anharmonic_cluster_charge_XWR`, `urea_rblyp_3-21G_generate_SF_stats` -- each one unit in the last printed digit (e.g. SO₂ E_e −656.105 → −656.104) | -- |

**No reference was re-blessed.** `scripts/test.py --bless` adopts a produced file only when it
fails the loose gate, and every one of these passes it -- the harness encodes the project's
convention that loose is the gate and last-digit exact failures are tolerated, not blessed away.
The six shifts are recorded here instead. Water-sized jobs cannot see the cutoff at all (three close atoms leave almost no primitive pair below 1e-6), which is why `short` barely
moved.

Suite timings from this run are not quoted: the agreement report for `short` was run while the
`long` ctest was still going, so the after-column is contaminated. A clean comparison needs no
second binary -- `eri_accuracy= very_low` in the input reproduces the old cutoffs on the same
executable.

## The zinc-finger benchmark, 2026-09-17

[Zn(SCH3)2(imidazole)2], 29 atoms, 152 electrons, closed shell; hand-built geometry (DEFERRED,
*Benchmark molecule*). One core, one job at a time, `%mem=2GB` / `%maxcore 2000`. g09 at
`SCF=(Tight,Conver=10)`, 6D 10F for 6-31G(d) and 5D 7F for def2; ORCA `TightSCF`, spherical
always, `NoRI` for "exact". Tonto `develop` `7d2c236a` at the `low` default, promolecule guess.
Runs in `~/tonto_runs/vs_g09_orca_znfinger_2026-09-17/`, tabulated by `collect.py`.

| job | g09 energy | g09 wall s | ORCA exact energy | ORCA exact s | ORCA default s |
|---|---|---|---|---|---|
| RHF/6-31G(d) | −3101.474631730 (cart) | 69.6 | −3101.464459344 (sph) | 161.9 | 160.2 |
| BLYP/6-31G(d) | −3107.616344890 (cart) | 95.8 | −3107.585179071 (sph) | 199.4 | 41.5 |
| RHF/def2-SVP | −3100.927836570 | 113.6 | −3100.927836555 | 135.7 | 136.6 |
| BLYP/def2-SVP | −3107.067669050 | 114.6 | −3107.067632439 | 159.3 | 33.3 |
| RHF/def2-TZVP | −3102.025942750 | 1365.2 | −3102.025942739 | 1566.0 | 1578.5 |
| BLYP/def2-TZVP | −3108.214409100 | 652.9 | −3108.214407078 | 1378.6 | 84.7 |

g09 and ORCA agree to 1-2e-8 on def2 RHF and to 2e-6 (TZVP) to 4e-5 (SVP) on def2 BLYP, where their grids differ. ORCA's
BLYP default is RI-J, 0.75e-3 to 0.87e-3 Eh below its exact energy.

**Tonto, cartesian 6-31G(d)** (like-for-like with g09):

| job | energy | wall s | J/K CPU s |
|---|---|---|---|
| RHF, J/K engine | −3101.474629556 (2.2e-6 from g09) | 178.9 | 173.7 |
| BLYP, J engine | −3107.616217177 (1.3e-4 from g09, grid) | 280.2 | 215.6 |
| BLYP, pair-list J | −3107.616219403 | 241.5 | 190.7 (−11.6%) |
| BLYP `high`, J engine | −3107.616219469 | 371.1 | 319.6 |
| BLYP `high`, pair-list J | −3107.616219469 | 428.8 | 378.4 |

So on this molecule Tonto is 2.6x g09 for RHF/6-31G(d) and 2.9x for BLYP (2.5x with the pair
list). **At `low` the pair-list energy is 6.6e-8 from `high`, the engine's 2.3e-6** -- the
engine's shell-quartet screening loses 35 times more on this molecule than the count-scaled
primitive screening, and the list is also 11.6% faster. At `high` the list is 18% slower: the
count-scaled cutoff is very tight there. So neither `low` row is accuracy-matched to the other;
an engine run at `medium` would place it.

**Tonto, spherical RHF/6-31G(d), core guess**: −3101.464456724 (2.6e-6 from ORCA), 345.1 s wall
(338.8 s J/K) against ORCA's 161.9 s: 2.1x. With the promolecule guess the same job never
converged, and the spherical def2-SVP RHF job did not converge even from the core guess
(DEFERRED, Correctness).

**Tonto, cartesian def2** -- a self-comparison only: a cartesian def2 basis has more functions than
the spherical one g09 and ORCA use, so the energies are not comparable with theirs.

| job | energy | wall s | J/K CPU s |
|---|---|---|---|
| RHF/def2-SVP, J/K engine | −3100.976345540 | 156.9 | 151.3 |
| BLYP/def2-SVP, J engine | −3107.106181806 | 196.5 | 137.4 |
| BLYP/def2-SVP, pair-list J | −3107.106183488 | 189.0 | 132.3 (−3.7%) |
| RHF/def2-TZVP, J/K engine | −3102.031243033 | 1564.8 | 1526.9 |
| BLYP/def2-TZVP, J engine | −3108.233152525 | 1648.1 | 1388.3 |
| BLYP/def2-TZVP, pair-list J | −3108.233153632 | 1477.1 | 1207.3 (−13.0%) |

The pair-list BLYP energies sit 1.7e-6 (SVP) and 1.1e-6 (TZVP) below the engine's -- the size and
direction of the engine's 2.3e-6 `low` error at 6-31G(d), so probably the same screening loss; no
`high` run was made for def2 to confirm it. One timing each, so the SVP difference is inside the noise; the TZVP one is not.
RHF/def2-TZVP costs about 1.1x the BLYP J build, the same ratio as at def2-SVP.

## The K share: pair-list J beside the combined engine, 2026-09-17

Karrikinolide RHF at `low`, promolecule guess. `off` is the `develop` combined J/K engine; `on` sets
`use_gaussian_pair_J= TRUE`, so `make_r_JK_engine` takes J from the pair list and runs its quartet
loop for K only. Three pairs, the two runs of each pair concurrent on cores 1 and 3. J/K CPU s.
Runs in `~/tonto_runs/k_share_2026-09-17/`.

| basis | off | on | on/off | energy on − off |
|---|---|---|---|---|
| 6-31G(d) | 53.8 55.9 57.0, mean 55.5 | 93.8 94.5 98.8, mean 95.7 | 1.72 | +1.9e-6 |
| cc-pVTZ | 1104.7 1069.5 1073.5, mean 1082.5 | 1590.0 1558.1 1559.9, mean 1569.3 | 1.45 | −3.3e-6 |

The engine's K loop costs about what the combined build costs, so J from the combined engine is
nearly free and the separate J pass is pure overhead. For HF and hybrids only K matters. RHF water
6-31G(d) at `high` gives identical energies on and off, so the energy shifts at `low` are the two
screenings differing; `high` references for karrikinolide are queued (`queue.sh`, `queue.log`).

`high` reference, 6-31G(d): −530.980808227207 off, −530.980808227198 on (140.7 s J/K on, 76.3 s
off). At `low` the engine is 2.37e-6 below it and the pair list 4.7e-7: the list's primitive
screening is five times more accurate here, as on the zinc finger.

### Where the RHF build spends its time (flat `perf record`, 6-31G(d), `low`)

Karrikinolide RHF/6-31G(d), combined engine, 47.0 s J/K under `perf`
(`~/tonto_runs/k_share_2026-09-17/perf_6-31Gd/perf.data`), symbols grouped:

| share | what |
|---|---|
| 59.5% | integral generation: `make_esfs*`, `RYS:*`, `exp` |
| 14.5% | quartet set-up and memory: `set_cd_new`, `set_reusing_storage`, `set_shell2_indices_from`, `malloc` |
| 13.6% | J and K digestion: `make_r_JK_engine*` |
| 3.1% | transfer: `transfer_*` |
| 9.0% | the rest |

The same at cc-pVTZ (1047.7 s J/K under `perf`, `perf_cc-pVTZ/perf.data`): generation 61.7%,
digestion 15.8%, set-up and memory 7.8%, transfer 6.3%, the rest 8.0%.

So generation and per-quartet set-up are 70-75% of the build at both basis sets, and digestion about
one seventh: the condition set for K from the pair list (worth it only if generation dominates K) is
met.

`high` reference at cc-pVTZ, switch off: −531.169262524774 (2179 s J/K). At `low` the engine
(−531.169266412952) is 3.9e-6 below it and pair-list J with engine K (−531.169263118355) is 5.9e-7
below: the list screens better here too, as at 6-31G(d).

### ORCA's approximate exchange on the zinc finger, RHF/def2-TZVP

One core, against ORCA `NoRI` −3102.025942739 in 1566 s wall:

| method | energy | error / Eh | wall / s |
|---|---|---|---|
| `RIJCOSX` | −3102.026644041 | −7.0e-4 | 499 |
| `RIJK` | −3102.025484906 | +4.6e-4 | 421 |

Both are three to four times faster than the exact build, at errors of the size RI-J makes for BLYP.

## RI-J: density-fitted J on the primitive pair list, 2026-09-17

Branch `ri-j`. `auxiliary_basis_name= def2-universal-jfit` and `scfdata= { use_RI_J= TRUE }`; pure DFT
only. Direct: each J build makes two passes over the three-centre integrals (auxiliary primitive |
primitive pair), d = (Q|P), V c = d by a Cholesky factor made once per SCF, J = (ab|Q) c. The
auxiliary functions are always spherical. Runs in `~/tonto_runs/ri_j_2026-09-17/`, one core, one job
at a time.

**Correctness, against ORCA 6.1.1 with `def2/J`.** The fitting error E(RI-J) − E(exact) is compared,
because the two codes' DFT grids differ (3.6e-6 Eh on water) and that cancels in the difference.

| job, spherical, BLYP, `high` | Tonto exact | Tonto RI-J | Tonto error | ORCA error |
|---|---|---|---|---|
| water def2-SVP | −76.3369284357 | −76.3370150493 | −8.6614e-5 | −8.6614e-5 |
| karrikinolide def2-SVP | −533.567022653898 | −533.567470685761 | −4.48032e-4 | −4.48022e-4 |
| karrikinolide def2-TZVP | −534.174358453943 | −534.174715902972 | −3.57449e-4 | −3.57452e-4 |

Cartesian water against g09 `BLYP/def2SVP/W06 6D 10F Int=UltraFine`: g09 −76.3386375800 exact,
−76.3387315359 fitted, error −9.40e-5; Tonto −76.3386407081 and −76.3387336665, error −9.30e-5.
They agree to 1e-6, not to 1e-8 as with ORCA; g09's fit may differ in detail (not pursued). g09
keeps the fitting functions pure, "W06 (5D, 7F)", 71 of them, under `6D 10F`, as Tonto does.

Water: UKS RI-J equals RKS to 2e-10; debug and release builds agree to
every printed digit. The exact rows must be at `high`: at `low` the engine's exact karrikinolide
def2-SVP energy is 3.6e-7 off, which at first looked like an RI-J discrepancy.

**Screening.** RI-J at `low` against `high`: water 4e-10, karrikinolide def2-SVP 1.8e-8, def2-TZVP
4.9e-8.

**Speed at `low`, J/K CPU s.** "Before" is the best exact J on the same input (pair list for the zinc
finger, engine for karrikinolide).

| job | before | RI-J | ratio | whole job, RI-J | fitting error (exact at `low`) |
|---|---|---|---|---|---|
| karrikinolide def2-SVP, spherical | 58.9 | 6.9 | 8.6 | 31 s | −4.477e-4 |
| karrikinolide def2-TZVP, spherical | 999.8 | 24.3 | 41 | 120 s | −3.580e-4 |
| zinc finger def2-SVP, cartesian | 132.3 | 21.0 | 6.3 | 81 s | −9.51e-4 |
| zinc finger def2-TZVP, cartesian | 1207.3 | 68.6 | 17.6 | 337 s | −8.17e-4 |

The karrikinolide "before" rows are the 2026-09-16 `develop` runs. On this branch's build the exact
engine at `high` takes 756.9 s for def2-TZVP and 65.4 s for def2-SVP, against RI-J's 28.5 s and 9.8 s
at `high`: 27 and 6.7 times.

ORCA's fitting errors for the zinc finger are −8.74e-4 and −7.53e-4, in a spherical basis, so not
comparable digit for digit with these cartesian rows.

**Where the time goes now**, zinc finger def2-TZVP with RI-J (338.9 CPU s): XC matrix and energy
214.9 (63%), J 68.6 (20%), initial guess 22.1, diagonalisation 19.0, DIIS 8.9. ORCA's whole job is
85 s. The XC quadrature is the next bottleneck.
