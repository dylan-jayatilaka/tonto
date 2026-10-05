# Published grids for COSX and XC: SG-0, SG-1, SG-2 and SG-3

The plan and log for adopting the published standard grids in Tonto, for COSX first: SG-0 (Chien
and Gill), SG-1 (Gill, Johnson and Pople), and SG-2 and SG-3 (Dasgupta and Herbert). Register row:
*Adopt published element-dependent grids, for COSX and XC*. Branch: `sg-grids`.

## 1. Why these grids

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

## 2. The recipes

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

### 2.1 SG-1

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

### 2.2 SG-0

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

### 2.3 SG-2 and SG-3

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

## 3. What was checked

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

## 4. The COSX error of the SG grids

RHF with RI-J (def2-universal-jfit). The error is the energy with COSX minus the energy of the same
job with exact exchange, so the RI-J error cancels. Each job names two grids: the one used in the
SCF iterations and the one used for the final energy. Karrikinolide is C8H8O3, thiotepa C6H12N3PS,
the zinc finger a Zn(SCH3)2(imidazole)2 model in a Cartesian basis; the others use spherical
functions.

**Error in µEh** (the target is 100):

ERRTABLE

"No size adjustment" is `partition_scaling_scheme= none` on the final grid: Becke's partition
without his correction for atoms of different size, which Tonto otherwise applies.

**Grid points:**

PTSTABLE

What the numbers say:

1. **The final-energy grid sets the error.** Changing only the iteration grid moves the energy by
   under 2 µEh, except on the zinc finger (point 3).
2. **The SG grids are poor final-energy grids for COSX.** SG-1, SG-2 and SG-3 miss the target on
   CFCl3 and on the zinc finger, and where they meet it they are 2 to 20 times worse than `high`,
   which has as many points as SG-2. More points do not help: SG-3 is no better than SG-2. On
   CFCl3 def2-SVP the unpruned 99 × 590 grid gives −1.7 µEh against +810 for SG-3, so it is the
   published pruning that fails; it was made for the density, and the COSX integrand is rougher.
   Dropping the size adjustment changes the errors but does not make them reliably small.
3. **SG-0 is a good iteration grid.** With `high` for the final energy it matches the default
   everywhere, and on the zinc finger it cuts the error from 26 to 5 µEh: `very_low` leaves a
   poorer converged density there. It has from 20 % fewer to 22 % more points than `very_low`.
4. **SG-1 as the iteration grid** costs twice the points of SG-0 for no gain.

**Clean timings** (one job at a time on achari2):

TIMETABLE

## 5. Where this leaves the item

- The four grids are in `BECKE_GRID`, off by default, tested, for any element.
- **For COSX the present defaults stay.** The one candidate change is `cosx_grid= { kind= sg-0 }`
  for the iterations: Dylan's decision, and by his rule nothing becomes a default without full
  testing of its effect on HAR.
- **XC is the natural use of these grids** and is not done: the same four kinds for the DFT
  quadrature, against Tonto's `high`, on the molecules above.
- **Not settled:** the DE2 radial range (§2.3); the Mg and Si totals; whether Q-Chem applies an
  atomic size adjustment to its Becke weights. All three are questions for John Herbert. The
  ORCA COSX grid parameters are still to be asked of the authors.
- **Small loose ends.** The CIF items `_QCr_Becke_grid_n_pts_for_row_1` to `_3` print the counts
  of H, He and Li for an SG kind. A job with 21 separate molecules 15 Å apart hung in
  *Making gaussian ANO data* (`hydrides_all/`); not looked into.

## 6. Log

- 2026-10-05. SG-2 and SG-3 transcribed and coded on `sg-grids`; not built.
- 2026-10-05/06. SG-0 and SG-1 added; every element covered; built in debug, release and
  reference on achari2. The first release build failed on `DIE_IF` inside `PURE` routines, which
  debug accepts; they became `ENSURE`. Checks and COSX measurements as above.
