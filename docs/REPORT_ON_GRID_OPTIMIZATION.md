# Integration grids for DFT and COSX

Tonto integrates two quantities numerically on a grid of points around each atom: the
exchange-correlation (XC) energy of a DFT calculation, and the exchange matrix when exchange is
made by COSX (chain-of-spheres exchange). This page describes how both grids are built, how their
error is measured, and what the measurements give. One set of code and one set of parameters of
the same form serve both; each integrand has its own values.

## 1. How a grid is built

The molecular grid is a sum of atomic grids. For atom $`A`$, of element $`Z`$ in period $`p`$ of
the periodic table:

- **Radial points.** $`n_r(p) = n_r^{\mathrm{H}} + \Delta n_r(p)`$: the count for hydrogen,
  `n_radial_pts=`, plus the extra points of period $`p`$, `n_extra_radial_pts=` (seven numbers, one
  for each period). The radial points are placed by the Mura–Knowles mapping.
- **Angular points.** On each radial shell, a Lebedev grid that integrates spherical harmonics
  exactly up to order $`L`$. Shells near the nucleus and far out need a lower order than shells in
  the valence region; giving each zone its own order is called pruning.
- **The partition.** Each atom's grid is multiplied by a weight $`w_A(\mathbf{r})`$ with
  $`\sum_A w_A(\mathbf{r}) = 1`$ at every point $`\mathbf{r}`$, so that the atomic grids together
  integrate the whole molecule once (`partition_scheme=`).
- **Pruning by the promolecule.** Points where the promolecule density, the sum of the free-atom
  densities, is below `prune_rho_cutoff=` are dropped.

The named levels, `accuracy= very_low` to `best`, set these together.

## 2. How the error is measured

The error of a grid is measured on one fixed, converged density. For each test system an SCF is
converged once; then the integrand is evaluated on each trial grid from that same density matrix.
All other energy terms are computed analytically, so the change from grid to grid is the grid
error alone. The reference is the same integrand on a large unpruned grid (grid A); a larger one
(grid B) shows that the reference is converged.

For the XC grid, the keyword

```
put_xc_energy_on_grid
```

after a `becke_grid= { }` block builds the grid as the block sets it and prints the XC energy
$`E_{xc}`$, the number of electrons integrated on the grid, and the number of points, for the
stored density matrix. It can follow any number of `becke_grid= { }` blocks in one job.

The targets for the XC grid, for the error in a GGA energy:

| level | `very_low` | `low` | `medium` | `high` | `very_high` |
|---|---|---|---|---|---|
| target /hartree | 1e-3 | 1e-4 | 1e-5 | 1e-6 | 1e-7 |

A level passes if the largest error over the test systems is below its target.

## 3. The COSX integrand

COSX makes the exchange matrix with one analytic and one numerical integration,

```math
K_{\mu\nu} = \sum_g w_g\, \chi_\mu(\mathbf{r}_g) \sum_{\lambda\sigma} A_{\nu\sigma}(\mathbf{r}_g)\, D_{\lambda\sigma}\, \chi_\lambda(\mathbf{r}_g), \qquad
A_{\nu\sigma}(\mathbf{r}) = \int \frac{\chi_\nu(\mathbf{r}')\,\chi_\sigma(\mathbf{r}')}{|\mathbf{r}-\mathbf{r}'|}\, d\mathbf{r}' , \qquad (1)
```

where $`\chi_\mu`$ are the basis functions, $`D`$ is the density matrix, and $`\mathbf{r}_g`$ and
$`w_g`$ are the grid points and weights. Its grid error is that of the exchange energy density

```math
e_x(\mathbf{r}) = -\frac14 \int \frac{|\rho_1(\mathbf{r},\mathbf{r}')|^2}{|\mathbf{r}-\mathbf{r}'|}\, d\mathbf{r}' , \qquad
\rho_1(\mathbf{r},\mathbf{r}') = \sum_{\lambda\sigma} D_{\lambda\sigma}\, \chi_\lambda(\mathbf{r})\, \chi_\sigma(\mathbf{r}') , \qquad (2)
```

for a closed shell, whose integral over $`\mathbf{r}`$ is the exchange energy. Here $`\rho_1`$ is
the one-particle density matrix in space. $`e_x`$ has more angular structure than the density
does: it is built from products of basis functions, up to twice the highest angular momentum in
the basis. So a COSX grid needs its own angular orders, but the same form of grid. The target for
the COSX grid used for the final energy is 0.1 mEh; the grid used in the SCF iterations only has
to bring the SCF to the same answer.

## 4. The partition

Tonto's grids use Becke's partition by default: smoothed Voronoi cells, adjusted for atoms of
different size. The alternative described here is the Salvador atom with promolecule radii
(`tfvp`, the same cell function as `partition_model= tfvp`). Its boundary between two bonded atoms
passes through the minimum of the promolecule density along the bond. That point lies close to
the bond critical point of the real density, where the density is small and nearly flat along
the bond. A partition weight changes fastest near its cell boundary, so the boundary adds angular
structure to each atom's piece of the integrand; placed where the integrand is small, it adds
less. The radii come from the promolecule, so the grid is fixed before the SCF begins and needs no
stored atomic radii.

## 5. The test systems

PBE with def2-SVP for the scans and def2-TZVP to confirm, spherical functions:

| period | systems | what each covers |
|---|---|---|
| 1–2 | H2O, N2, LiF, F2, CH3 radical, benzene, neopentane, water dimer | polar, triple bond, ionic, lone pairs, open shell, aromatic, crowded C–H, hydrogen bond |
| 3 | HCl, NaCl, SiH4, P4, SF6 | polar, ionic, electropositive centre, strained cage, hypervalent |
| 4, s and p | KF, HBr | ionic K; 4p |
| 4, 3d | TiCl4, Cr(CO)6, Ni(CO)4, Zn(CH3)2, CuCl, ScO, Mn atom | d0, back-bonding, d10, organometallic, metal halide, open-shell oxide, high-spin atom |

The geometries are experimental bond lengths in ideal symmetry; the water dimer is the S22
geometry. A stretched water dimer is added as a check on how smoothly a weakly bound pair is
integrated.

## 6. Running the measurements

The drivers are in `scripts/grid_optimization/`; its README gives the commands. Each evaluation
is stored as one row of a results file, keyed by the system, its geometry, the basis set and the
exact grid settings. A run can be stopped and started again, or extended with a new system, and
only what is missing is computed.

Batch jobs must run single-threaded: set both `OPENBLAS_NUM_THREADS=1` and `OMP_NUM_THREADS=1`.
With the second unset, a `release` build linked to OpenBLAS uses two cores per job, and eight
jobs on twelve cores run some 80 times slower.

## 7. Results

All results are PBE/def2-SVP on fixed densities, for the 22 systems of §5.

### 7.1 The reference grid

Grid A is unpruned, with 100, 110, 120 and 140 radial points for periods 1 to 4 and Lebedev
order 101; grid B has 140, 155, 170 and 200 points and order 131. For 19 of the 22 systems the two
give the same $`E_{xc}`$ to 3e-8 hartree or better, and grid A integrates the electron count to
1e-7 or better.

Three systems with an electropositive atom bonded to an electronegative one, KF, LiF and ScO,
are the exception. With Becke's partition, the electron count on grid A is short by 8.6e-4,
1.9e-4 and 2.3e-4 respectively; with Salvador's, by 1.8e-3, 7.2e-4 and 7.6e-4. A lone K$`^+`$ ion
integrates exactly, more points do not change the count, and the shortfall depends on the
partition, so it arises where the two atoms' cells meet. Its effect on $`E_{xc}`$ is a few
microhartree. Until its cause is found these three systems cannot be used to set the two
tightest levels.

### 7.2 The present levels

The largest error in $`E_{xc}`$ over the 22 systems, against grid A, for the levels as they now
stand:

| partition | `very_low` | `low` | `medium` | `high` | `very_high` | `extreme` | `best` |
|---|---|---|---|---|---|---|---|
| Becke | 1.1e-2 | 1.3e-3 | 5.4e-4 | 1.5e-4 | 3.8e-5 | 1.1e-5 | 6.7e-6 |
| Salvador, promolecule radii | 2.9e-3 | 1.7e-3 | 4.8e-4 | 2.1e-4 | 4.2e-5 | 1.2e-5 | 1.0e-5 |
| target | 1e-3 | 1e-4 | 1e-5 | 1e-6 | 1e-7 | | |

No level meets its target with either partition. The worst systems with Becke's partition are
the period-3 and transition-metal ones (TiCl4, SF6, NiCO4, KF, NaCl) and neopentane. Salvador's
partition reduces the errors of those systems several times over at the lower levels, and
increases those of molecules with O–H and C–H bonds (water, the water dimer, neopentane).

### 7.3 Radial points

Angular order 59 throughout, no pruning, the radial count of one period varied while the
others are held at the reference. The smallest count at which the error of every system stays
below the target for every larger count, leaving out KF, LiF and ScO (§7.1):

| period | partition | 1e-3 | 1e-4 | 1e-5 | 1e-6 | 1e-7 |
|---|---|---|---|---|---|---|
| 1 | Becke | 15 | 15 | 25 | 35 | 50 |
| 1 | Salvador | 15 | 30 | 35 | 50 | 65 |
| 2 | Becke | 25 | 35 | 45 | 60 | 100 |
| 2 | Salvador | 30 | 45 | 55 | 75 | 95 |
| 3 | Becke | 35 | 55 | 75 | 95 | over 115 |
| 3 | Salvador | 30 | 45 | 50 | 60 | 115 |
| 4 | Becke | 40 | 50 | 70 | 110 | over 135 |
| 4 | Salvador | 40 | 45 | 60 | 70 | 90 |

Salvador's partition needs fewer radial points for the heavier atoms, and more for hydrogen and
for oxygen bonded to hydrogen. With the promolecule radii the boundary between O and H lies
close to H, so O's grid integrates most of the O–H bond and the hydrogen's density outside its
small cell.

## 8. References

- A. D. Becke, *J. Chem. Phys.* 88, 2547 (1988): the partition.
- M. E. Mura and P. J. Knowles, *J. Chem. Phys.* 104, 9848 (1996): the radial mapping.
- O. Treutler and R. Ahlrichs, *J. Chem. Phys.* 102, 346 (1995): pruning and angular orders.
- P. Salvador and E. Ramos-Cordoba, *J. Chem. Phys.* 139, 071103 (2013): the Salvador atom.
- T. Laqua, J. Kussmann and C. Ochsenfeld, *J. Chem. Phys.* 149, 204111 (2018): pruned orders that
  rise with the level.
- B. Helmich-Paris, B. de Souza, F. Neese and R. Izsák, *J. Chem. Phys.* 155, 104109 (2021):
  COSX grids.
