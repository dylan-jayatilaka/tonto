# Published grids for COSX and XC: SG-2 and SG-3

The plan and log for adopting the SG-2 and SG-3 integration grids of Dasgupta and Herbert in
Tonto, for COSX first. Register row: *Adopt published element-dependent grids, for COSX and XC*.

## 1. Why these grids

Tonto's own COSX grid calibration covered C, H and O and failed on Zn and S. ORCA's COSX grids
(Helmich-Paris, de Souza, Neese and Izsák, *J. Chem. Phys.* 155, 104109, 2021) are the natural
target, but their parameters are not published: the paper gives the recipe (Treutler–Ahlrichs M3
radial mapping with Gauss–Chebyshev quadrature of the second kind, a five-region pruning in
multiples of the Clementi radius, Lebedev orders from its Table I, Becke weights, 32 fitted
parameters per grid) and says the data are available from the authors on request. Its supplement
holds the Rys-root procedure, parallel timings, the training set and coordinates only. SG-2 and
SG-3 are published in full, so they can be implemented now. They were made for DFT, not for the
COSX integrand, so their COSX error has to be measured, not assumed. If the ORCA parameters are
obtained later, they go in as further named grids by the same route.

## 2. The recipe

Sources: S. Dasgupta and J. M. Herbert, *J. Comput. Chem.* 38, 869 (2017), Table 1 and
eqs. 6–9; M. Mitani, *Theor. Chem. Acc.* 130, 645 (2011); M. Mitani and Y. Yoshioka, *Theor.
Chem. Acc.* 131, 1169 (2012). All three are in `~/Dropbox/manuscripts`.

**Radial quadrature: DE2.** With $`x_i = i h`$, the radii and weights for $`\int_0^\infty f(r)\, r^2\, dr`$ are

```math
r_i = \exp(\alpha x_i - e^{-x_i}), \qquad w_i = h\, \exp(3\alpha x_i - 3 e^{-x_i})\, (\alpha + e^{-x_i}) ,
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
(SG-2) or 99 (SG-3), and the point totals here are computed from the partitions.

**Other elements** (the rare gases, and everything beyond Cl) use an unpruned (75, 302) or
(99, 590) Euler–Maclaurin–Lebedev grid in the paper.

## 3. Open points

- **Two rows disagree with the paper's printed totals.** Mg SG-3 gives 15210 points against
  16532 printed, and Si SG-2 gives 8198 against 8342 (the Al value). One of partition or total is
  misprinted in each. Until settled against Q-Chem, Mg and Si use the unpruned grid.
- **The radial range.** The SG paper does not say how Q-Chem truncates the DE2 sum; the
  $`10^{-7}`$ to $`10R`$ range above is Mitani's, and is assumed.
- **R for C, N, O and the rest.** Not listed in the papers. To be computed as the Hartree–Fock
  mean radius of the outermost valence orbital, checked against the quoted F and Cl values.
- **Elements beyond Cl:** the Euler–Maclaurin radial scheme and its radii need checking against
  what Tonto has before zinc can be run on these grids.

## 4. Plan

1. `BECKE_GRID`: a `de2` radial scheme (`set_DE2_radial_grid`) and `sg-2` / `sg-3` pruning
   schemes keyed on the atomic number, beside `mura_knowles` and `treutler_ahlrichs`. Nothing
   changes by default.
2. Checks, in order: each atom's point count equals the table's; the density of H, C, N, O
   atoms and of water integrates to the electron count; water def2-SVP RI-J/COSX against the
   exact energy and ORCA's value (`short/h2o_rhf_def2-SVP_RIJCOSX`); karrikinolide def2-TZVP
   against exact, with `put_cosx_shell_errors`.
3. Compare cost and error with Tonto's present COSX grids on the same molecules, and on the
   7-molecule urea cluster in def2-TZVP, where COSX takes 40 % of its time in the grid potentials.
4. Then XC: the same grids for the DFT quadrature, against Tonto's `high`.

## 5. Log

Nothing built yet.
