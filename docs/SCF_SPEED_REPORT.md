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
