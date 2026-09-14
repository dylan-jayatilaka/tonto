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
