# Plan: one framework for Tonto's integration grids, XC and COSX

A working document (`CLAUDE.md` §1): plan, decisions and log. The findings for a reader are in
`docs/REPORT_ON_GRID_OPTIMIZATION.md`. Register row: *Grid optimisation: the DFT levels and the
COSX grids*. Drivers: `scripts/grid_optimization/` (README there). Results store:
`~/Dropbox/tonto_data/grid_optimization/`. Run folder: `~/tonto-runs/2026-10-13_grid_levels/`.

This document replaces two: the plan for the DFT grid levels (`docs/TASK_GRID_LEVELS.md`, renamed
to this one) and the record of the COSX grids (`docs/TASK_COSX_GRIDS.md`, now Appendix A).

## 1. Decisions (Dylan, 2026-10-14)

1. **One framework, two tables.** The XC grid and the COSX grid are built by the same code from
   the same kind of parameters, calibrated by the same scripts on the same systems. Each has its
   own table of parameters, because each integrand has its own error.
2. **The grid partition is the Salvador atom with promolecule radii (`tfvp`).** Its fuzzy
   boundaries run through the density minima between bonded atoms, so the bulk of each atom's
   density lies inside its own cell, and the cut falls where the integrand is smallest and
   flattest. The radii come from the promolecule, so no SCF is needed first and no table of atomic
   radii is stored. (Dylan first said `tfva`, then chose `tfvp` for these reasons.)
3. **All 22 test systems, throughout.** Extended later with new hard cases as they are met.
4. **The new keywords stay** (`put_xc_energy_on_grid`, `n_extra_radial_pts=`), and go into
   `docs/RUNNING_TONTO.md` when the levels are set.
5. **The work must survive stops.** Every evaluation is one row of a durable store, keyed so
   that a restart, a new system or a code change computes only what is missing.
6. **Bugs found on the way are fixed** as they are found, each logged in §8.

## 2. The framework

A grid is built atom by atom. For atom $`A`$ of element $`Z`$ in period $`p`$:

- **Radial points:** $`n_r(p) = n_r^{\rm H} + \Delta n_r(p)`$, the hydrogen count plus the extra
  points of the period (`n_radial_pts=`, `n_extra_radial_pts=`). Per element later if the data
  ask for it.
- **Angular order by zone:** the radial range is cut into zones at multiples of an atomic radius
  $`R_Z`$, and each zone has its own Lebedev order $`L_k(Z)`$. Three zones today (Tonto's pruning by
  shell number, inner 14 points, middle 50); four in ORCA's COSX grids; the zone boundaries and
  orders become data.
- **Partition:** each atom's grid is multiplied by its partition weight $`w_A(\mathbf{r})`$, with
  $`\sum_A w_A(\mathbf{r}) = 1`$ at every point. Today Becke's; to be Salvador's with promolecule
  radii (§1, item 2).
- **Pruning by the promolecule density** (`prune_rho_cutoff=`), which costs under 1e-10 hartree.

What differs between the two grids is the **integrand** and the **target**:

| | XC grid | COSX grid |
|---|---|---|
| integrand | $`e_{xc}(\rho,\nabla\rho)`$, the XC energy density | the exchange integrand of COSX (§2.1) |
| measured as | $`E_{xc}`$ on a fixed density | the exchange energy on a fixed density, against exact |
| target | 1e-3, 1e-4, 1e-5, 1e-6, 1e-7 hartree for `very_low` to `very_high` | 0.1 mEh for the final grid; the iteration grid only needs to converge |
| keyword | `put_xc_energy_on_grid` | to write (§5, code change 5) |
| shell diagnostic | `put_grid_shell_errors` | `put_cosx_shell_errors` |

### 2.1 The COSX integrand

COSX makes the exchange matrix as

```math
K_{\mu\nu} = \sum_g w_g\, \chi_\mu(\mathbf{r}_g) \sum_{\lambda\sigma} A_{\nu\sigma}(\mathbf{r}_g)\, D_{\lambda\sigma}\, \chi_\lambda(\mathbf{r}_g), \qquad
A_{\nu\sigma}(\mathbf{r}) = \int \frac{\chi_\nu(\mathbf{r}')\,\chi_\sigma(\mathbf{r}')}{|\mathbf{r}-\mathbf{r}'|}\, d\mathbf{r}' , \qquad (1)
```

with $`\chi_\mu`$ the basis functions, $`D`$ the density matrix, and $`\mathbf{r}_g`$, $`w_g`$ the grid
points and weights. The exchange energy is $`E_x = -\frac14 \mathrm{tr}\,(DKD)`$ for a closed shell,
and its grid error is the grid error of the exchange energy density

```math
e_x(\mathbf{r}) = -\frac14 \int \frac{|\rho_1(\mathbf{r},\mathbf{r}')|^2}{|\mathbf{r}-\mathbf{r}'|}\, d\mathbf{r}' , \qquad
\rho_1(\mathbf{r},\mathbf{r}') = \sum_{\lambda\sigma} D_{\lambda\sigma}\, \chi_\lambda(\mathbf{r})\, \chi_\sigma(\mathbf{r}') , \qquad (2)
```

the outer integral of which is done on the grid. Its angular structure comes from products of
basis functions, up to twice the highest angular momentum in the basis, in the valence zone. That
is why a calibration on C, H and O failed on Zn and S (Appendix A).

### 2.2 Why the partition matters, and why Salvador's with promolecule radii

The partition weight of a point changes fastest near the cell boundary, so the boundary adds
angular structure of its own to the integrand on each atom's grid. Becke's cell boundary, even
with the atomic-size adjustment, cuts through the valence region of the larger atom of a polar
bond. The Salvador boundary with promolecule radii passes through the minimum of the promolecule
density along the bond, close to the bond critical point of the real density, where the
integrand is small and nearly flat along the bond. So the same
angular order should give a smaller error. Ring and cage critical points (benzene, P4) are where
three or more cells meet; those are the places to look first.

**Radii.** The `tfvp` radii come from the promolecule density, which exists before the SCF, so
the grid is fixed for the whole SCF and the same in every job on the same geometry. (`tfva` takes
them from the molecular density, which would need an SCF first and a grid that changes with it.)

**Cost.** `make_Salvador_cell_fn` is $`N^2`$ in the number of atoms near a point, as Becke's is.
To measure: grid build time against Becke's on neopentane and Cr(CO)6.

## 3. The test systems

`scripts/grid_optimization/systems.py`: 22 systems, H to Zn and Br, plus a stretched water dimer
as an extra check. PBE, def2-SVP for the scans, def2-TZVP to confirm; spherical functions.

| Period | Systems | What each covers |
|---|---|---|
| 1–2 | H2O, N2, LiF, F2, CH3 radical (open shell), benzene, neopentane, water dimer | polar, triple bond, ionic, lone pairs, radical, aromatic, crowded C–H, hydrogen bond |
| 3 | HCl, NaCl, SiH4, P4, SF6 | the old worst case, ionic, electropositive centre, strained cage, hypervalent |
| 4, s and p | KF, HBr | ionic K; 4p |
| 4, 3d | TiCl4, Cr(CO)6, Ni(CO)4, Zn(CH3)2, CuCl, ScO (doublet), Mn atom (sextet) | d0, carbonyl back-bonding, d10, organometallic, late metal halide, open-shell oxide, high-spin atom |

Checks, not calibration data: the seven heavy systems (HF, HCl, HBr, HI, Xe, Hg, Rn,
`x2c-SVPall`, `~/tonto-runs/2026-10-12_heavy_grid`), karrikinolide (error against size), one urea
Hirshfeld atom refinement per level, and for COSX the nine jobs of Appendix A §A.4.

## 4. Method

**One fixed density per system.** Converge once on the `best` grid, keep the density matrix, and
evaluate the integrand on each trial grid. The other energy terms are analytic, so the change is
the grid error and nothing else. The reference is Tonto on a large unpruned grid (grid A:
radial 100, 110, 120, 140 for periods 1 to 4, Lebedev order 101), shown converged by a larger one
(grid B: 140, 155, 170, 200, order 131). On water A and B agree to 1e-10 hartree.

**Stages** (`scan.py`), each ingredient varied alone with the others at the reference:

1. `ref`: grids A and B.
2. `cutoffs`: `partition_cutoff`, `rho_cutoff`, `basis_fn_cutoff`, `prune_rho_cutoff` tightened
   in turn on grid A.
3. `radial`: angular order 59, no pruning; the count of one period varied, 15 up to the reference.
4. `angular`: reference radial counts, no pruning; the non-H order and the H order varied
   separately over the Lebedev orders 11 to 71.
5. `levels`: the seven named levels as they now stand, against grid A.
6. **Shell tables, by atom** (replaces the comparison of three pruning schemes): on every atom of
   every system, `put_grid_shell_errors` gives the angular error of each radial shell at orders 5
   to 29 against 59. Gathered into the store as needed order against $`r/R_Z`$, by element and
   bonding. This is the library of atoms: from it the zone boundaries and orders of each element
   are read off, for the XC integrand and, with `put_cosx_shell_errors`, for COSX.
7. **Assemble** each level: the cheapest radial counts and zone orders whose errors sum to half
   the target on the worst system. Points per atom and time per evaluation recorded.
8. **Confirm** with full SCF runs, 22 systems × 5 levels, PBE/def2-TZVP, against a full SCF on
   grid A; then B3LYP and PBE0 on six systems, r2SCAN reported but not a target, three systems
   turned to a general orientation, and the checks of §3.
9. **COSX:** the same stages with the COSX integrand (code change 5), the target 0.1 mEh for the
   final grid; then a small iteration grid that only has to converge the SCF.

**The partition and the stages.** Stages 1 to 5 are run first with Becke's partition (as the
code stands), then again with Salvador's when code change 4 is in. The store keys each evaluation
by its grid block, which names the partition, so both sets stay and can be compared directly. The
radial counts are expected to carry over; the angular orders near the boundaries are what the
partition changes.

**How a level passes** (proposal): the largest absolute error in the total energy over the 22
systems is below the target. Error per atom is tabulated beside it.

## 5. Code changes (`foofiles/`, in order)

1. Done. `put_xc_energy_on_grid` (`MOLECULE.FOCK`, keyword in `MOLECULE.MAIN`): rebuilds the grid
   from the present settings and prints $`E_{xc}`$, the electron count and the points for the
   stored density. The batched XC builders accumulate the electron count in `SCF_data.XC_n_electrons`.
2. Done. `BECKE_GRID.n_extra_radial_pts`, seven numbers by period, keyword `n_extra_radial_pts=`.
   `set_n_extra_pts_per_shell(n)` fills it with the old rule, so every default is unchanged.
3. Zone orders as data: per element, the zone boundaries in units of $`R_Z`$ and the Lebedev order
   of each zone, read by `apply_pruning_scheme` as a new scheme. Replaces the fixed 14 and 50.
4. `partition_scheme= salvador` for the grid: a case in `BECKE_GRID:partition` calling
   `make_Salvador_cell_fn`, with the promolecule radii (`MOLECULE.RHO:make_Salvador_radii_promolecule`)
   set on the grid before `set_grid_data`.
5. `put_cosx_energy_on_grid`: the exchange energy of the stored density with the present COSX
   grid, and the exact one once, to give the COSX grid error from a fixed density.
6. After calibration: `set_accuracy` sets, per level, the radial vector, the zone orders and the
   partition; the COSX defaults name their own tables. Periods 5 and 6 keep the heavy-atom values
   of 2026-10-12 at `medium` and get scaled values at the other levels, checked on the seven heavy
   systems. Then the printed grid summary shows the seven radial numbers.
7. Comments and pages: `types.foo:3121`, `becke_grid.foo:1186`, `docs/RUNNING_TONTO.md`,
   `docs/RUNNING_HART.md`, `docs/TONTO_KNOWN_ISSUES.md`, `scripts/check_dft_invariants.py`.

Changes 1 to 5 leave every default energy unchanged and can be merged before the calibration.
Change 6 moves most DFT references: a re-bless on achari2.

## 6. What carries over from the work begun on 2026-10-13

Everything up to stage 5 stands: the 22 systems and their densities, the two code changes, the
store, and the radial, angular, cutoff and level scans with Becke's partition. They are the Becke
column of the comparison in §4 and the radial calibration, which the partition hardly touches.
What changes is stage 6 onwards: the pruning is calibrated from shell tables by atom instead of
by trying three fixed schemes, and with the Salvador partition.

## 7. Restarting and extending

`scripts/grid_optimization/README.md` has the commands. In short: the store
(`~/Dropbox/tonto_data/grid_optimization/results_<basis>.csv`) is the record; a run folder can be
deleted and made again. `scan.py` writes only the evaluations missing from the store, and
`run_scans.sh` skips finished outputs. A new system is a line in `systems.py`. A code change
that alters what a grid block means bumps `GRID_CODE` in `store.py`.

Batch jobs need `OPENBLAS_NUM_THREADS=1` **and** `OMP_NUM_THREADS=1`: without the second, each
`release` job (OpenBLAS) took two cores and eight jobs ran 80 times slower.

## 8. What the stored papers say about XC grids

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

## 9. Log

**2026-10-14.** Dylan: test on all 22 systems, throughout; keep the new keywords; then merge the
COSX grid work into this item; the grid partition is to be the Salvador atom with promolecule
radii (`tfvp`).

- Code changes 1 and 2 built (debug and release); `ctest -L short` in debug: 86 of 86 pass, so
  the defaults are unchanged. On water `put_xc_energy_on_grid` reproduces the SCF's $`E_{xc}`$ to
  every printed digit; grids A and B agree to 1e-10 hartree.
- SCFs, PBE/def2-SVP on the `best` grid: all 23 converged. Ti, Cr and Ni need damping and level
  shifting in the atomic SCF behind the starting density (otherwise *atomic SCF for the ANOs did
  not converge*). ZnMe2 and SF6 stopped at 100 iterations with a DIIS error of 5e-9 and 8e-9
  against a tolerance of 1e-9; the tolerance is now 1e-8.
- **Bug, fixed:** `ARCHIVE:read` of an unrestricted `OPMATRIX` or `OPVECTOR` asked for the genres
  `"a"` and `"b"`, which neither type has, so nothing was allocated and a release build crashed
  reading into it. Every restart from an unrestricted archive was affected. Now `"u"`.
- **Bug, open:** for KF, the water dimer, ScO and TiCl4 the SCF printed nothing (no table, no
  convergence line) unless `output= TRUE` was given in `scfdata=`; it ran and converged. Seen
  after the ANOs are made; the guess SCF's output setting seems to leak into the parent's.
  Entry in `TASKS_AND_HISTORY.md`, *Correctness*.
- First finding: KF on the `best` grid has an electron count 1.1e-3 short. Period 4 radial counts
  are the suspect; the radial scan will say.
- The scans ran at first with two threads per job (OpenMP in OpenBLAS); fixed with
  `OMP_NUM_THREADS=1` (§7). The drivers moved into the repository and the store to Dropbox.
- **First Becke scans, all 23 systems, grid code `g1`** (1,500 evaluations, about 8 minutes on
  8 cores). Grids A and B agree to 2e-6 hartree at worst (KF), most to 1e-9. The present levels
  are far from the targets: the largest error over the 22 is 1.1e-2 at `very_low` (TiCl4),
  1.3e-3 at `low`, 5.4e-4 at `medium` (SF6), 1.5e-4 at `high`, 3.8e-5 at `very_high` (LiF). The
  worst systems are the period-3 and 3d ones (SF6, TiCl4, NiCO4, KF, NaCl) and neopentane.
  Of the cutoffs only `basis_fn_cutoff` matters, by up to 4e-6 (KF), and only through the bug
  below.
- **Open: the electron count on the unpruned reference grid is short** for systems with an
  electropositive atom: by 8.6e-4 (KF), 2.3e-4 (ScO), 1.9e-4 (LiF), 1.3e-5 (TiCl4), 8.5e-6 (Cr(CO)6).
  Its effect on $`E_{xc}`$ is small (cutoffs moved KF's by 4e-6), but it bounds how well these
  systems can be referenced. Found so far, on KF:
  - not the number of points: grid B, and 300 radial points, give the same count;
  - not the radial mapping (`kind= treutler_ahlrichs`, `becke` the same), nor the XC batching (one
    batch the same), nor the atom order (F first the same);
  - not the atoms alone: a lone K+ ion integrates to 18.0000000000;
  - partly the cutoffs: all at 1e-16 take it from 8.6e-4 to 3.3e-4;
  - it depends on the partition: Becke 8.6e-4 (3.3e-4 with tight cutoffs), no size adjustment
    9.2e-4, Treutler–Ahlrichs 3.9e-4, Stratmann–Scuseria 9.1e-4, Salvador 1.8e-3.
  - **Ruled out by trying it:** letting each atom's grid reach every atom's basis functions
    (`BECKE_GRID:r_max_for_atom`); it changed neither the points nor the count, and was reverted.
  So something drops part of the density where two atoms' cells meet. Next: the electron count
  by atom and shell (`put_grid_shell_errors`) for KF.
- **Bug, fixed: a missing archive was read as zeros.** `ARCHIVE:read` checked that the file
  exists with `ENSURE`, which a release build drops; the open then created an empty file and the
  read gave zeros. Now `DIE_IF`.
- **Bug, fixed: a job named after an element lost its archives.** The Mn atom job was named `Mn`;
  `MOLECULE.SET:delete_atom_SCF_archives` deletes `<symbol>.*` for each kind of atom, so it deleted
  the job's own density and MOs. It now skips a symbol equal to the job's name.
- **Bug, fixed (debug only):** `MOLECULE.RHO:make_Salvador_radii_pro` required the stored density
  matrix, though it uses only the one passed in; a debug build would stop before the first SCF.
- **Bug, fixed:** the keyword `integrate_ed_numerically` crashed (release) after `read_archive`:
  it evaluates the density from the natural orbitals, which did not exist, and its `ENSURE` is
  dropped in release. It now makes them. On KF it gives 27.976 electrons on a `best` grid with
  default cutoffs; it drops points by `partition_cutoff` (268,000 of 295,000), so it is not an
  independent reference for the electron count.
- Code change 4 written: `partition_scheme= salvador` (`BECKE_GRID:partition_Salvador`, the grid's
  own `partition_chi`, made by `MOLECULE.RHO:make_grid_partition_chi` from the promolecule radii
  in `set_up_becke_grid`, which is no longer `PURE`). `make_Salvador_cell_fn` takes the size
  adjustments as an optional argument. Building.
- Code change 4 built (debug and release). `MOLECULE.SCF:make_HA_info` makes the ANOs before the
  grid when the partition is Salvador's; a direct call from `MOLECULE.RHO` would have closed a
  cycle between the two modules. One atom alone gets `chi` = 1. Debug `ctest -L short` 86/86.
- **Salvador scans done** (1,586 evaluations). Present levels, largest error over the 22:
  2.9e-3, 1.7e-3, 4.8e-4, 2.1e-4, 4.2e-5 for `very_low` to `very_high` (Becke: 1.1e-2, 1.3e-3,
  5.4e-4, 1.5e-4, 3.8e-5). Salvador cuts the errors of the period-3 and 3d systems several times
  at the low levels, and raises those of water, the water dimer and neopentane: with promolecule
  radii the O–H boundary sits near H, so O's grid takes most of the bond.
- **Radial points for each target** (`radial_needs.py`, KF, LiF and ScO left out): for 1e-6,
  Becke needs 35, 60, 95, 110 points in periods 1 to 4, Salvador 50, 75, 60, 70. So Salvador
  saves radial points on the heavier atoms and costs them on H and on O bonded to H. KF, LiF and
  ScO never settle below 1e-6, consistent with their electron-count shortfall.
- **For Dylan:** the O–H cost of `tfvp`. Options: (a) keep `tfvp` and give H and its partner more
  points near the boundary (the shell tables will say where); (b) a size adjustment that moves
  the X–H boundary towards X; (c) Becke for X–H pairs and Salvador elsewhere. (a) is the natural
  first step; the shell tables come next in any case.
- **Dylan's decision:** try scaling the Salvador size adjustment for H, and an ionic partition
  for the ionic systems. New grid keyword `partition_h_chi_scale=` (`BECKE_GRID.partition_H_chi_scale`,
  default 1): $`\chi(\mathrm{H},X)`$ is multiplied by $`s`$ and $`\chi(X,\mathrm{H})`$ divided by it, so the
  boundary moves away from H; applied in `MOLECULE.RHO:make_grid_partition_chi`. Scanned at
  $`s`$ = 1.5, 2, 3, 4 on the systems with H. The ionic partition needs no code: `scfdata=
  { initial_atomic_states= { ... } }` already sets the charge and multiplicity of each kind of atom
  for the atomic SCFs behind the promolecule (`systems.py`, `IONIC`: Li+F-, Na+Cl-, K+F-, Cu+Cl-,
  Sc+O-). A store row now has a `variant` (here `ionic`), part of its key, since the ionic
  promolecule changes the grid without changing the grid block.
- **The ionic promolecule does not help** (LiF, NaCl, KF; Salvador partition). Errors at the five
  levels are the same as with neutral atoms or slightly worse, and the electron-count shortfall on
  grid A grows: LiF 7.2e-4 to 3.9e-3, KF 1.8e-3 to 2.4e-3, NaCl 2.7e-5 to 5.0e-5. Neutral Salvador
  is already far better than Becke on these: KF at `very_low` 5e-5 against 3e-3. Recommendation:
  drop it. CuCl and ScO could not be tried: the atomic SCF of Cu+ and Sc+ does not converge, even
  with damping and level shifting kept on to iteration 60 (Cu+ UHF: DIIS error stuck at 3.17 after
  200 iterations). A defect of the atomic SCF.
- **pFON for the atomic SCF (Dylan's suggestion).** The guess SCF did not inherit the pFON
  settings; `MOLECULE.SET:set_SCF_guess_defaults_from` now passes `use_pfon=` and its three
  parameters on, as it does damping and level shift (off by default, so nothing changes). With
  pFON on (3000 K, 50 K per iteration) Cu+ and Sc+ still do not converge. pFON is not the cure: the
  Cu+ UHF settles at -1635.616 hartree, about 3 hartree above the Hartree-Fock energy of Cu+ (near
  -1638.7, from memory, unchecked), with the DIIS error fixed at 3.17. That is a core-excited
  state reached from the starting guess, which smoothing the occupations cannot leave. A start from
  the neutral atom's orbitals is the likely fix; logged in `TASKS_AND_HISTORY.md`, not pursued here.
- **My first H-factor run was wrong and its rows were removed from the store.** It scaled $`\chi`$
  for every H–heavy pair, neighbours or not; non-neighbours have $`\chi = 1`$, so at $`s = 4`$ an H
  cell swallowed distant carbons and errors reached 1.4 hartree. Now only Salvador neighbours are
  scaled. Rerun at $`s`$ = 1.25, 1.5, 2, 3, 4.
- **H factor, neighbours only: no value of $`s`$ makes Salvador as good as Becke on the systems
  with H.** Largest error over the nine, `very_low` to `very_high`: Becke 2.4e-3, 2.8e-4, 1.8e-4,
  9.4e-6, 8.0e-7; Salvador 2.9e-3, 1.7e-3, 4.7e-4, 2.1e-4, 4.2e-5; best factor, $`s`$ = 1.5: 2.5e-3,
  4.1e-4, 3.7e-4, 1.1e-4, 1.7e-5. From $`s`$ = 2 SiH4 breaks: its promolecule boundary already sits
  towards Si, and a uniform factor drives it into the Si atom.
- **Why Salvador loses on X–H bonds** (angular scans, radial at the reference): the error is in
  the heavy atom's angular order, not hydrogen's. With Salvador, neopentane's C order must reach 71
  for 1e-6 (59 gives 3.6e-6), against 35 with Becke; SiH4 and water the same. The small H cell
  hands the outer part of the H density to the heavy atom, where it is an off-centre lump that
  needs high angular orders. Varying the H order changes little under either partition.
- **Proposal for Dylan:** Salvador everywhere except H–heavy neighbour pairs, whose boundary is
  placed by Becke's size adjustment (Bragg–Slater radii) rather than by a uniform factor.
- **Dylan: try the mixed partition.** New grid keyword `partition_h_becke=` (`BECKE_GRID:Becke_boundary_chi`):
  the Salvador $`\chi`$ that puts an H–X boundary exactly where Becke's size-adjusted partition puts
  it ($`c`$ the ratio of Bragg–Slater radii, $`w = (c-1)/(c+1)`$, $`a = w/(w^2-1)`$ held in
  $`[-\tfrac12,\tfrac12]`$, Becke's boundary at $`u = (1-\sqrt{1+4a^2})/(2a)`$, so
  $`\chi = (1+u)/(1-u)`$). First applied to H–heavy **neighbours** only. Largest error over the nine
  with H, `very_low` to `very_high`: 2.2e-3, 9.3e-4, 3.8e-4, 1.1e-4, 9.6e-6 (Becke 2.4e-3, 2.8e-4,
  1.8e-4, 9.4e-6, 8.0e-7). Eight of the nine are now as good as Becke, some better (water
  `very_low` 2.9e-5 against 1.0e-4); neopentane alone is worse (1.1e-4 against 2.9e-6 at `high`).
  Its H atoms also face non-bonded C at 2.1 Å, which Salvador leaves unadjusted ($`\chi = 1`$) where
  Becke adjusts every pair. So the rule was widened to **every** H–heavy pair, and the
  neighbours-only rows were removed from the store.
- **Every H–heavy pair: worse for neopentane, not better** (1.1e-4 to 2.7e-4 at `high`; Becke 2.9e-6).
  Largest over the nine with H: 1.0e-3, 1.7e-3, 1.2e-4, 2.7e-4, 3.4e-5. So neopentane's trouble is
  not its non-bonded H–C pairs. Dylan: keep the bonded-only rule; its rows removed again and rerun.
- **Hypothesis: the sharpness of the cell boundary.** Salvador's step polynomial is applied 4
  times, Becke's 3; a sharper boundary puts more angular structure into each atom's piece, and
  neopentane has the most boundaries per atom. New grid keyword `salvador_stiffness=`
  (`BECKE_GRID.Salvador_stiffness`, default 4), for the grid partition only; `Salvador_step` takes
  the stiffness as an argument, and the property partitions keep 4. Dylan: run it. Scanned at 4, 3
  and 2 with the bonded-only mixed partition.
- **Stiffness result** (mixed partition, bonded H pairs). Largest error over the 22, `very_low` to
  `very_high`: stiffness 4: 2.7e-3, 1.6e-3, 4.8e-4, 1.7e-4, 9.9e-6; 3: 3.8e-3, 2.0e-3, 2.8e-4,
  9.7e-5, 1.4e-5; 2: 6.6e-2, 2.3e-2, 6.9e-3, 3.4e-3, 8.6e-4 (Becke 1.1e-2, 1.3e-3, 5.4e-4,
  1.5e-4, 3.8e-5). Neopentane was the sharpness: at 3 its `high` error falls from 1.1e-4 to 2.1e-5
  and `very_high` from 9.6e-6 to 1.2e-6 (Becke 8.0e-7). Stiffness 3 has the best worst case at
  `medium` and `high`, but beats Becke on only 4 of 22 systems at `high` (stiffness 4: 10 of 22);
  the systems without H do slightly better at 4. Stiffness 2 fails everywhere and needs far more
  radial points: a soft boundary spreads each piece out where its radial grid is coarse. So the
  boundary sharpness trades angular points (too sharp) against radial points (too soft). Radial
  points for 1e-6, periods 1 to 4: stiffness 4: 55, 75, 60, 70; stiffness 3: 40, 100, 70, 70;
  Becke 35, 60, 95, 110.
- **Proposal:** carry three candidates (Becke; mixed, stiffness 4; mixed, stiffness 3) through the
  shell tables and the assembly of the levels, and choose on points per atom at equal accuracy;
  errors at today's levels, which were built for Becke, cannot settle it.
- **Dylan, on the candidates:** carry all three, for diversity; uneasy with the mixed model's ad
  hoc rule (H is small and its density is swamped or polarised by its bonded neighbour), so the
  mixing deserves further study. The test set is small; widen it later, once the behaviour is
  understood, not now. Work committed to branch `grid-optimization` (not merged; merging to
  `develop` needs a good CI).
- **The period-2 radial bump at stiffness 3 is the carbonyls**, not hydrogen: Cr(CO)6 needs 100
  points and Ni(CO)4 95 for 1e-6, every other period-2 system 45 to 60. Their error alternates
  in sign and decays slowly with the count, which points to the metal's density in the C and O
  cells far from their nuclei, where the radial grid is sparse; a softer boundary gives those
  cells more of it (Becke 60, stiffness 4 75, stiffness 3 100).
- **Shell tables** (`shells.py`; store `shells_def2-SVP.csv`, 34,950 rows): `put_grid_shell_errors`
  now compares orders 5 to 71 against 101 (was 5 to 29 against 59) and sets the grid up itself, so
  it works with the Salvador partition; `partition_Salvador` stops with a message (`STOP_IF`)
  where it crashed if the grid was not set up. Order needed for every shell within 1e-7 of L = 101,
  largest over each element, zones in units of the Bragg–Slater radius $`R`$: inside $`0.5R`$ 5 to 17
  everywhere; the peak is at 1 to 2 $`R`$, where Becke needs 101 on Na, Ti, Ni and 71 on Li, K, S, Cr,
  mixed at stiffness 4 at most 53 on the metals (Cr 71 just inside), stiffness 3 between, with O at
  101; beyond 3 $`R`$ low again, except H in the mixed partition (47, 41) and C (Becke 71). Table in
  `~/tonto-runs/2026-10-13_grid_levels/shells_needs_1e-7.md`. A per-element zone scheme is well
  supported.

## Appendix A. The COSX grid record (formerly `docs/TASK_COSX_GRIDS.md`)

The plan and log for adopting the published standard grids in Tonto, for COSX first: SG-0 (Chien
and Gill), SG-1 (Gill, Johnson and Pople), and SG-2 and SG-3 (Dasgupta and Herbert). Register row:
*Adopt published element-dependent grids, for COSX and XC*. Branch: `sg-grids`.

### A.1. Why these grids

Tonto's own COSX grid calibration covered C, H and O and failed on Zn and S. ORCA's COSX grids
(Helmich-Paris, de Souza, Neese and Izsák, *J. Chem. Phys.* 155, 104109, 2021) are the natural
target, but their parameters are not published: the paper gives the recipe (Treutler–Ahlrichs M3
radial mapping with Gauss–Chebyshev quadrature of the second kind, a five-region pruning in
multiples of the Clementi radius, Lebedev orders from its Table I, Becke weights, 32 fitted
parameters per grid) and says the data are available from the authors on request. Its supplement
holds the Rys-root procedure, parallel timings, the training set and coordinates only. The four SG
grids are published in full, so they can be implemented now. They were made for DFT, not for the
COSX integrand, so their COSX error has to be measured, not assumed. If the ORCA parameters are
obtained later, they go in as further named grids by the same route.

**The target (Dylan, 2026-10-05):** a COSX grid error under 0.1 mEh on the test molecules. COSX
itself is about 0.7 mEh from exact on the zinc finger.

### A.2. The recipes

In Tonto: `becke_grid= { kind= sg-0 }` and likewise `sg-1`, `sg-2`, `sg-3`; the same in
`cosx_grid= { }` and `cosx_final_grid= { }`. A standard grid is a whole recipe, so choosing one
turns off the atomic scale factors and the separate hydrogen angular grid, and sets the pruning.
All four use Becke's partition, as Tonto already has.

| grid | radial points | radial scheme | angular points, at most | elements with a published partition |
|---|---|---|---|---|
| SG-0 | 23 (H to F), 26 (Na to Cl) | MultiExp | 170 | H, Li to F, Na to Cl |
| SG-1 | 50 | Euler–Maclaurin | 194 | H to Ar |
| SG-2 | 75 | DE2 | 302 | H, Li to F, Na to Cl |
| SG-3 | 99 | DE2 | 590 | H, Li to F, Na to Cl |

#### 2.1 SG-1

Source: P. M. W. Gill, B. G. Johnson and J. A. Pople, *Chem. Phys. Lett.* 209, 506 (1993), in
`~/Dropbox/manuscripts`.

**Radial quadrature: Euler–Maclaurin** (Murray, Handy and Laming), with $`N = 50`$ points. For
$`\int_0^\infty f(r)\, r^2\, dr`$ the radii $`r_i`$ and weights $`w_i`$, $`i = 1 \ldots N`$, are

```math
r_i = \frac{R\, i^2}{(N+1-i)^2}, \qquad w_i = \frac{2 R^3 (N+1)\, i^5}{(N+1-i)^7} , \qquad (1)
```

with $`R`$ the atomic radius: where the radial density of the outermost orbital peaks by Slater's
rules, $`R = n^{*2}/Z_\mathrm{eff}`$ bohr, with $`n^*`$ Slater's effective principal quantum number
and $`Z_\mathrm{eff}`$ the screened nuclear charge. The paper's Table 1 lists $`R`$ for H to Ar;
Tonto uses those values, and they follow from the rule to the four figures printed.

**Angular grids.** Four spheres of radius $`a_1 R \ldots a_4 R`$ divide the atom into five zones
with 6, 38, 86, 194 and 86 Lebedev points, from the nucleus outward:

| elements | $`a_1`$ | $`a_2`$ | $`a_3`$ | $`a_4`$ | shells in each zone | points |
|---|---|---|---|---|---|---|
| H, He | 0.2500 | 0.5000 | 1.0000 | 4.5000 | 16, 5, 4, 9, 16 | 3752 |
| Li to Ne | 0.1667 | 0.5000 | 0.9000 | 3.5000 | 14, 7, 3, 9, 17 | 3816 |
| Na to Ar | 0.1000 | 0.4000 | 0.8000 | 2.5000 | 12, 7, 5, 7, 19 | 3760 |

Because $`r_i/R`$ does not depend on $`R`$, every element of a row has the same shells in each zone.
The counts agree with Psi4's, which were set to match Q-Chem. One shell of H and He sits exactly on
a boundary ($`i = 17`$, $`r/R = 0.25`$); it belongs to the outer zone, as Q-Chem has it.

**Beyond Ar** the paper gives nothing. Tonto uses the unpruned 50 × 194 grid, with $`R`$ from
Slater's rules and the subshells filled in Madelung order (no exceptions for Cr, Cu and the like).
Slater gave no effective quantum number for the seventh shell; the sixth shell's 4.2 is used. This
is our choice, not a published one.

#### 2.2 SG-0

Sources: S.-H. Chien and P. M. W. Gill, *J. Comput. Chem.* 27, 730 (2006), which is **not** in
`~/Dropbox/manuscripts`: its Table 1 was taken from Psi4's source (`libfock/cubature.cc`), which
transcribes it. P. M. W. Gill and S.-H. Chien, *J. Comput. Chem.* 24, 732 (2003) for the radial
scheme, which is in the folder.

**Radial quadrature: MultiExp.** Let $`x_i`$ and $`a_i`$ be the points and weights of the
$`N`$-point Gaussian quadrature on $`[0,1]`$ for the weight function $`\ln^2 x`$. Then for
$`\int_0^\infty f(r)\, r^2\, dr`$

```math
r_i = -R \ln x_i, \qquad w_i = \frac{R^3 a_i}{x_i} , \qquad (2)
```

with $`R`$ the element's radius from the table below. $`N`$ is 23 for H to F and 26 for Na to Cl.
The two rules are stored in `set_MultiExp_radial_grid`; they were computed in 150-digit arithmetic
from the moments $`2/(k+1)^3`$ and reproduce the paper's Table 1 for $`N`$ = 2, 3 and 20.

**Angular grids.** As for SG-2 below, a partition per element. "6×6 18×3" means the six innermost
shells have 6 points and the next three have 18. The 18-point rule is not a Lebedev grid: it is the
vertices and edge midpoints of an octahedron (Abramowitz and Stegun, p. 894), `as0018` in `LEBEDEV`.

| element | $`R`$ / bohr | partition | points |
|---|---|---|---|
| H | 1.30 | 6×6 18×3 26×1 38×1 74×1 110×1 146×6 86×1 50×1 38×1 18×1 | 1406 |
| Li | 1.95 | 6×6 18×3 26×1 38×1 74×1 110×1 146×6 86×1 50×1 38×1 18×1 | 1406 |
| Be | 2.20 | 6×4 18×2 26×1 38×2 74×1 86×1 110×2 146×5 50×1 38×1 18×1 6×2 | 1390 |
| B | 1.45 | 6×4 26×4 38×3 86×3 146×6 38×1 6×2 | 1426 |
| C | 1.20 | 6×6 18×2 26×1 38×2 50×2 86×1 110×1 146×1 170×2 146×2 86×1 38×1 18×1 | 1390 |
| N | 1.10 | 6×6 18×3 26×1 38×2 74×2 110×1 170×2 146×3 86×1 50×2 | 1414 |
| O | 1.10 | 6×5 18×1 26×2 38×1 50×4 86×1 110×5 86×1 50×1 38×1 6×1 | 1154 |
| F | 1.20 | 6×4 38×2 50×4 74×2 110×2 146×2 110×2 86×3 50×1 6×1 | 1494 |
| Na | 2.30 | 6×6 18×2 26×3 38×1 50×2 110×8 74×2 6×2 | 1328 |
| Mg | 2.20 | 6×5 18×2 26×2 38×2 50×2 74×1 110×2 146×4 110×1 86×1 38×2 18×1 6×1 | 1468 |
| Al | 2.10 | 6×6 18×2 26×1 38×2 50×2 74×1 86×1 146×2 170×2 110×2 86×1 74×1 26×1 18×1 6×1 | 1496 |
| Si | 1.30 | 6×5 18×4 38×4 50×3 74×1 110×2 146×1 170×3 86×1 50×1 6×1 | 1496 |
| P | 1.30 | 6×5 18×4 38×4 50×3 74×1 110×2 146×1 170×3 86×1 50×1 6×1 | 1496 |
| S | 1.10 | 6×4 18×1 26×8 38×2 50×1 74×2 110×1 170×3 146×1 110×1 50×1 6×1 | 1456 |
| Cl | 1.45 | 6×4 18×7 26×2 38×2 50×1 74×1 110×2 170×3 146×1 110×1 86×1 6×1 | 1480 |

The Mg total is computed from the partition; Psi4 notes that the paper prints 1492.

**Other elements** (He, Ne, Ar and everything beyond Cl) use SG-1, as in the paper.

**Q-Chem's own SG-0 differs:** its manual says SG-0 was re-optimised for Q-Chem 3.0. What is here
is the published grid.

#### 2.3 SG-2 and SG-3

Sources: S. Dasgupta and J. M. Herbert, *J. Comput. Chem.* 38, 869 (2017), Table 1 and
eqs. 6–9; M. Mitani, *Theor. Chem. Acc.* 130, 645 (2011); M. Mitani and Y. Yoshioka, *Theor.
Chem. Acc.* 131, 1169 (2012). All three are in `~/Dropbox/manuscripts`.

**Radial quadrature: DE2.** With $`x_i = i h`$, the radii and weights for $`\int_0^\infty f(r)\, r^2\, dr`$ are

```math
r_i = \exp(\alpha x_i - e^{-x_i}), \qquad w_i = h\, \exp(3\alpha x_i - 3 e^{-x_i})\, (\alpha + e^{-x_i}) , \qquad (3)
```

with $`\alpha`$ the element's scaling factor from the table below ($`a`$ in the papers). Mitani keeps
the points with $`r`$ from $`10^{-7}`$ to $`10R`$ bohr, $`R`$ the mean radius of the atom's outermost
valence orbital in Hartree–Fock (1.5 bohr for H; 1.084786 for F and 1.842024 for Cl are quoted
in the 2011 paper). So for $`n_r`$ points: solve $`r(x_{\min}) = 10^{-7}`$ and $`r(x_{\max}) = 10R`$,
and take $`h = (x_{\max} - x_{\min})/(n_r - 1)`$. $`n_r`$ is 75 for SG-2 and 99 for SG-3.

**Angular grids and pruning.** Lebedev grids; each radial shell, counted from the nucleus
outward, has the order given in the table. "6×35 110×12" means the 35 innermost shells have 6
points each and the next 12 have 110.

**Partition.** Becke's, as Tonto already has.

| element | SG-2 partition | α | points | SG-3 partition | α | points |
|---|---|---|---|---|---|---|
| H | 6×35 110×12 302×16 86×7 26×5 | 2.6 | 7094 | 6×45 110×16 590×21 194×10 50×7 | 2.7 | 16710 |
| Li | 6×35 110×12 302×17 86×7 50×4 | 3.2 | 7466 | 6×46 110×16 590×22 146×9 50×6 | 3.0 | 16630 |
| Be | 6×35 110×12 302×17 86×7 50×4 | 2.4 | 7466 | 6×42 86×6 110×14 590×22 194×3 146×6 50×6 | 2.4 | 17046 |
| B | 6×35 110×12 302×17 146×7 26×4 | 2.4 | 7790 | 6×42 86×6 110×14 590×22 194×9 50×6 | 2.4 | 17334 |
| C | 6×35 110×12 302×17 146×7 26×4 | 2.2 | 7790 | 6×46 146×16 590×22 302×1 194×2 146×6 86×6 | 2.4 | 17674 |
| N | 6×35 110×12 302×17 86×7 26×4 | 2.2 | 7370 | 6×40 110×18 590×24 146×11 50×6 | 2.4 | 18286 |
| O | 6×30 110×14 302×18 146×8 50×5 | 2.2 | 8574 | 6×40 110×14 194×2 302×2 590×24 302×1 194×1 146×8 50×7 | 2.6 | 18946 |
| F | 6×26 110×16 302×19 110×8 50×6 | 2.2 | 8834 | 6×35 110×17 194×4 590×25 194×2 110×8 50×8 | 2.1 | 19274 |
| Na | 6×35 110×12 302×17 86×7 50×4 | 3.2 | 7466 | 6×46 110×16 590×22 146×9 50×6 | 3.2 | 16630 |
| Mg | 6×35 110×12 302×17 86×7 50×4 | 2.4 | 7466 | 6×48 110×15 590×20 146×7 50×9 | 2.6 | 15210 |
| Al | 6×32 110×15 302×17 146×7 86×4 | 2.5 | 8342 | 6×42 86×6 110×14 590×22 194×3 146×6 50×6 | 2.6 | 17046 |
| Si | 6×32 110×15 302×17 146×7 50×4 | 2.3 | 8198 | 6×42 86×6 110×14 590×22 194×9 50×6 | 2.8 | 17334 |
| P | 6×30 110×14 302×17 146×7 38×7 | 2.5 | 8142 | 6×35 86×1 110×18 194×4 590×25 194×2 146×8 50×6 | 2.4 | 19658 |
| S | 6×30 110×14 302×17 146×7 38×7 | 2.5 | 8142 | 6×35 86×1 110×18 194×4 590×25 194×2 146×8 50×6 | 2.4 | 19658 |
| Cl | 6×26 110×16 302×19 110×8 50×6 | 2.5 | 8834 | 6×35 110×17 194×4 590×25 194×2 110×8 50×8 | 2.6 | 19274 |

The table was transcribed by script from the paper's Table 1. Every row's shells add to 75
(SG-2) or 99 (SG-3), and the point totals here are computed from the partitions. **Two totals
differ from the paper's:** Mg SG-3 is 15210 here against 16532 printed, and Si SG-2 is 8198 against
8342 printed, which is the Al value. The Q-Chem manual lists no per-element numbers and the OSTI
copy of the paper has the same table, so this could not be settled; the partitions are used, since
their shells add up.

**Other elements** (the rare gases, and everything beyond Cl) use an unpruned 75 × 302 or
99 × 590 Euler–Maclaurin grid, as the paper says, with the SG-1 radius of §2.1.

**The radial range is an assumption.** The paper gives eq. (3) but not where the sum starts and
stops. Tonto uses Mitani's range, $`10^{-7}`$ to $`10R`$ bohr, with $`R`$ the Hartree–Fock mean
radius of the outermost valence orbital: H 1.5, Li 3.873661, Be 2.6494, B 2.2048, C 1.7145,
N 1.4096, O 1.2322, F 1.084786, Na 4.208762, Mg 3.2529, Al 3.4339, Si 2.7521, P 2.3225, S 2.0607,
Cl 1.842024.
Mitani quotes the H, Li, Na, F and Cl values; the rest are the Roothaan–Hartree–Fock values as
remembered, not checked against a table. The shells of a partition are counted from the nucleus,
so a different range would put every angular zone at different radii.

### A.3. What was checked

Runs: `achari2:~/tonto_runs/sg_grids_2026-10-05/` (inputs, outputs, `table.py`, `md.py`), with
the release build of branch `sg-grids` in `achari2:~/github/tonto-sg`.

- **Point counts.** For H to Cl, K, Zn and Br, and He, Ne, Ar, every atom's point count on each of
  the four grids equals the reference table made independently in Python from the published
  tables (`hyd/`). The SG-0 counts equal Psi4's and the SG-1 counts Q-Chem's.
- **Radial grids.** He, Ne and Ar atoms integrate to their electron number to $`10^{-7}`$ or better
  on every grid. The MultiExp rules reproduce the paper's table.
- **Electron counts of the hydrides** (RHF/def2-SVP; the error in electrons). SG-0: up to
  $`1.2\times10^{-3}`$ (SiH4). SG-1: mostly a few $`10^{-5}`$, up to $`9\times10^{-4}`$ (NaH).
  SG-2: up to $`1.3\times10^{-4}`$ (HCl). SG-3: mostly under $`10^{-5}`$, up to $`6\times10^{-5}`$
  (KH). For water Tonto's `high` grid, with fewer points than SG-2, gives $`10^{-7}`$.
- **The pruning, not the radial grid, sets the SG-3 error.** Water: $`1.0\times10^{-5}`$ with the
  published partition, $`7\times10^{-7}`$ with 590 points on every shell.
- **Tests.** `short/hcl_rhf_def2-SVP_SG_grids`, `short/hbr_rhf_def2-SVP_SG_grids` and
  `short/h2o_rhf_def2-SVP_RIJCOSX_SG-0`, blessed on achari2 with the reference build. The whole
  suite passes there, 179 of 179 with the two usual skips.

### A.4. The COSX error of the SG grids

RHF with RI-J (def2-universal-jfit). The error is the energy with COSX minus the energy of the same
job with exact exchange, so the RI-J error cancels. Each job names two grids: the one used in the
SCF iterations and the one used for the final energy. Karrikinolide is C8H8O3, thiotepa C6H12N3PS,
the zinc finger a Zn(SCH3)2(imidazole)2 model in a Cartesian basis; the others use spherical
functions.

**Error in µEh** (the target is 100):

| iterations / final energy | water SVP | CFCl3 SVP | CFCl3 TZVP | thiotepa SVP | thiotepa TZVP | karrikinolide SVP | karrikinolide TZVP | zinc finger SVP | zinc finger TZVP |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| very_low / high (default) | -0.0 | 8.9 | 8.7 | -7.2 | 4.9 | 3.8 | -1.6 | 26.3 | 6.2 |
| SG-0 / high | -0.1 | 8.1 | 8.5 | -7.7 | 5.0 | 3.5 | -1.6 | 4.6 | -5.2 |
| SG-1 / high | -0.1 | 8.2 | 8.4 | -9.5 | 4.2 | 2.9 | -1.8 | 3.2 |  |
| SG-0 / SG-0 | 59.1 | 2604.7 | 3268.0 | -3147.7 | -1600.9 | -3.2 | -45.5 | -2199.3 |  |
| SG-0 / SG-1 | -2.8 | 321.1 | 90.3 | 16.6 | -68.5 | 52.2 | -14.7 | -154.0 | -436.3 |
| SG-0 / SG-2 | 7.8 | 767.9 | -186.5 | 6.0 | -34.4 | -2.8 | 3.2 | -170.6 |  |
| SG-1 / SG-2 | 7.8 | 768.0 | -186.8 | 4.2 | -35.1 | -3.5 | 3.0 | -171.9 | 98.8 |
| SG-1 / SG-3 | 4.3 | 809.6 | -175.9 | 14.5 | 21.3 | 11.4 | -0.5 | -51.4 |  |
| SG-0 / SG-1, no size adjustment | -6.3 | -285.3 | 135.0 | -65.5 | 66.4 | 12.0 | 1.4 | -415.7 |  |
| SG-0 / SG-2, no size adjustment | -3.8 | -16.5 | 46.8 | -69.7 | 24.4 | -19.3 | 6.1 | 63.9 |  |
| SG-0 / SG-3, no size adjustment | -0.8 | -71.3 | 82.2 | 14.3 | -8.7 | 12.0 | -5.2 | 176.2 |  |

"No size adjustment" is `partition_scaling_scheme= none` on the final grid: Becke's partition
without his correction for atoms of different size, which Tonto otherwise applies.

**Grid points:**

| grid | water SVP | CFCl3 SVP | CFCl3 TZVP | thiotepa SVP | thiotepa TZVP | karrikinolide SVP | karrikinolide TZVP | zinc finger SVP | zinc finger TZVP |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| very_low | 3181 | 9123 | 9450 | 28725 | 29045 | 23476 | 23801 | 37800 | 38236 |
| SG-0 | 3951 | 7290 | 7290 | 32344 | 32344 | 22977 | 22977 | 46190 |  |
| SG-1 | 8379 | 13435 | 13854 | 63682 | 65374 | 47301 | 48704 | 84636 |  |
| high | 19014 | 45209 | 46713 | 165299 | 167562 | 127029 | 130219 | 214566 | 216932 |
| SG-2 | 22350 | 41597 | 42070 | 167733 | 168254 | 128163 | 128926 | 220096 |  |
| SG-3 | 51616 | 92645 | 93215 | 394496 | 395600 | 293153 | 294194 | 520608 |  |

What the numbers say:

1. **The final-energy grid sets the error.** Changing only the iteration grid moves the energy by
   under 2 µEh, except on the zinc finger (point 3).
2. **The SG grids are poor final-energy grids for COSX.** SG-1, SG-2 and SG-3 miss the target on
   CFCl3 and on the zinc finger, and where they meet it they are 2 to 20 times worse than `high`,
   which has as many points as SG-2. More points do not help: SG-3 is no better than SG-2. On
   CFCl3 def2-SVP the unpruned 99 × 590 grid gives −1.7 µEh against +810 for SG-3, so it is the
   published pruning that fails; it was made for the density, and the COSX integrand is rougher.
   Dropping the size adjustment changes the errors but does not make them reliably small.
3. **SG-0 is a sound iteration grid, but no better than `very_low`.** With `high` for the final
   energy it matches the default everywhere. On the zinc finger in def2-SVP it cuts the error from
   26 to 5 µEh, but in def2-TZVP the two are the same size (6 and −5). It has from 20 % fewer to
   22 % more points than `very_low`, and the time per iteration follows the points (below).
4. **SG-1 as the iteration grid** costs twice the points of SG-0 for no gain.
5. **SG-0 for the final energy is useless** (errors of mEh), as expected of so small a grid.

**Clean timings** (one job at a time on achari2; the other runs shared the machine, so their
times are not quoted):

| job | grids | iterations | seconds per iteration | wall time |
|---|---|---:|---:|---:|
| karrikinolide SVP | very_low / high | 22 | 6.2 | 2:52.25 |
| karrikinolide SVP | SG-0 / high | 22 | 6.1 | 2:50.14 |
| thiotepa SVP | very_low / high | 18 | 10.2 | 4:07.95 |
| thiotepa SVP | SG-0 / high | 18 | 11.6 | 4:35.13 |
| zinc finger SVP | very_low / high | 20 | 18.4 | 7:58.81 |
| zinc finger SVP | SG-0 / high | 20 | 21.9 | 9:12.63 |
| karrikinolide TZVP | very_low / high | 23 | 23.6 | 11:20.35 |
| karrikinolide TZVP | SG-0 / high | 23 | 23.4 | 11:14.62 |

### A.5. Where this leaves the item

**The SG grids are not being taken further (Dylan, 2026-10-06).** The code stays: the four kinds
are in `BECKE_GRID`, off by default, tested, for any element, and cost nothing where they are.

- **For COSX the present defaults stay.** No SG grid is more accurate than `high` for the final
  energy, and SG-0 is neither faster nor reliably more accurate than `very_low` for the
  iterations.
- **Dropped:** trying the SG grids for the DFT quadrature, and with it the three questions for
  John Herbert. They are kept here in case the grids are ever revisited: the DE2 radial range
  (§2.3), which is assumed; the Mg and Si point totals, which disagree with the printed
  partitions; and whether Q-Chem applies an atomic size adjustment to its Becke weights.
- **ORCA's COSX grids are no longer waited for** (Dylan, 2026-10-13): no reply to the email of
  2026-10-06. Our own are to be calibrated; section 5a.
- **Small loose ends.** The CIF items `_QCr_Becke_grid_n_pts_for_row_1` to `_3` print the counts
  of H, He and Li for an SG kind. The 21-molecule job that hung was a Rys-root bug, since fixed
  (`TASKS_AND_HISTORY.md`).

### A.5a. Our own grids: what the talk gives (2026-10-13)

Dylan, 2026-10-13: stop waiting for ORCA's parameters; no reply since 2026-10-06, and ORCA's
licence rules out reading them from the program. Calibrate our own. Source for the recipe beyond
the paper: the recording of B. Helmich-Paris, "Revision of COSX", ORCA user meeting 2020, day 3
(`~/tonto-runs/cosx_talk/ORCAUM2020-Day3-4_RevisionOfCOSX_HelmichParis.mp4`, 28.5 minutes). The
slides were read from frames (tool `frames` in that folder; frames in `scan/` and `grid/`). Only
the slides were read, not the sound. The grid part is three slides, from 21:10 to 23:30.

**What the slides say.**
- The grid error is minimised as a function of **32 parameters**: "4 grid pruning regions x 6
  rows of the PSE + 8 more parameters". So one number for each of four radial regions for each of
  six rows of the periodic table, 24, and eight more that the slide does not name.
- The function minimised is the product of the mean energy error and the mean number of grid
  points, $`f_{\rm err}(\theta_1 \dots \theta_{32}) = \overline{\Delta E}\;\overline{n_{\rm grid}}`$. So accuracy
  and cost are traded in one number, not accuracy at a fixed size.
- The minimiser is differential evolution.
- Training set: 351 atoms and molecules in different spin states, charges and bonding
  situations, from G2, G3/99, G3/05 and S22, plus "a customized test set with transition metal
  atoms/molecules", which the slide does not specify. So their training set cannot be
  reproduced exactly from the talk.
- Test: GMTKN55 and MOR41. The slide of results gives the total mean absolute deviation in
  kcal/mol for the old grids (default, GRIDX6, GRIDX8) and the new (NG1, NG2, NG3) with SVP,
  TZVP and their DKH forms, with a dashed line at 0.1 kcal/mol: NG2 and NG3 are under it in all
  four, NG1 is at 0.1 to 0.4.
- For TDDFT a much smaller exchange grid is enough, 20 % of the SCF grid.

**The picture of the four regions** (credited to B. de Souza). A radial function of one atom,
not named, peaking near 4.1 bohr, over r from 0 to 30 bohr, with four shaded bands. Read off the
frame: region 1 from 0 to 1.7 bohr, region 2 from 1.7 to 2.9, region 3 from 2.9 to 5.8, region 4
beyond. Region 3, which holds the peak, is shaded darkest, regions 2 and 4 less, region 1 least.
So the densest angular grid sits on the valence maximum, with coarser ones inside and outside,
as in SG-1 and its relatives; whether the boundaries are fixed lengths or scale with the atom
the slide does not say (the paper speaks of Clementi radii).

**What it does not give.** No numbers for any grid: not the radial counts, not the boundaries
for each row, not the Lebedev order in each region, not what the other eight parameters are.

**What follows for our own calibration.**
- The form: four radial regions for each row, a Lebedev grid in each, boundaries as multiples of
  an atomic radius. That is what `BECKE_GRID` already does for `sg-1` to `sg-3`, so the new
  grids are new tables, not new code.
- The eight further parameters are probably the radial counts or their dependence on the row,
  and the scheme's scale factors; to be read from the paper again with this in mind.
- The target: mean error times mean number of points, three grids of increasing size.
- Their training set is far larger than ours can be at first. Start with the systems chosen for
  the DFT grid levels and the two molecules already used for COSX, and test on the nine jobs of
  section 4; widen it if the fitted grids do not carry over.
- Differential evolution over 32 parameters needs many thousands of evaluations of every
  training system. With exchange from a fixed density, one evaluation is one K build, so this
  is feasible for small systems only; the fit should use fixed densities and small molecules.

### A.6. Log

- 2026-10-05. SG-2 and SG-3 transcribed and coded on `sg-grids`; not built.
- 2026-10-06. Dylan: no further work on the SG grids; XC trial and the Herbert questions dropped.
- 2026-10-05/06. SG-0 and SG-1 added; every element covered; built in debug, release and
  reference on achari2. The first release build failed on `DIE_IF` inside `PURE` routines, which
  debug accepts; they became `ENSURE`. Checks and COSX measurements as above.
