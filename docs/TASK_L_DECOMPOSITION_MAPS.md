# Angular (l) decomposition of density maps

The plan for putting the angular decomposition of electron densities into Tonto, beside the
total, deformation and residual density maps that already follow an X-ray refinement.
Opened 2026-10-06 (Dylan, with Santosh Panjikar). Nothing is coded yet.

**The theory is not repeated in full here.** It is in
`~/Dropbox/tex/conference_talks/2026_ED_angular_decomp/angular_decomp.pdf` (source
`angular_decomp.tex`), with every formula derived and checked numerically by
`check_formulas.py` in the same folder. Read that first. The formulas needed for coding are
collected in section 2 below, in its notation.

## 0. One thing to settle with Dylan before starting

Dylan gave the order of work as *"version 3, then Version 1 - Hirshfeld, and finally the orbital
version"*. The document has four pieces, and "version 3" could mean two of them:

| piece | where in the document | what it is |
|---|---|---|
| A | section 4, "Version 2" | the field of local moments $`M_{lm}(\mathbf r)`$: maps over the cell |
| B | section 3.1 | the expansion about one chosen centre (the Bessel formula) |
| C | section 3.2, "Version 1 - Hirshfeld" | Hirshfeld atoms expanded about their nuclei; the true decomposition |
| D | section 3.3, "the orbital version" | the same for the square root of each atom; populations $`n^A_l`$ |

C and D are unambiguous and come second and third. "Version 3" is either **A** (a slip for
"version 2") or **B** (section **3**.1, which makes the order simply 3.1, 3.2, 3.3). **Ask.** The
plan below is written so that either works: step 1 (shared machinery) serves all four, B is the
exact reference that C is tested against in any case, and A is independent of the rest.

## 1. What exists, and where the new work attaches

**Maps Tonto makes now** (`plot_grid= { kind= ... }`, then `plot`):

| kind | made from | routine |
|---|---|---|
| `electron_density` | the wavefunction, at any points | `MOLECULE.GRID:make_ED_grid` |
| `deformation_density` | wavefunction minus promolecule | `MOLECULE.GRID:make_def_ED_grid` |
| `hirshfeld_density`, `stockholder_density` | wavefunction times a stockholder weight | `MOLECULE.GRID:make_Hirshfeld_density_grid`, `make_stockholder_density_grid` |
| `residual_density` | Fourier sum of $`F_{\rm exp} - F_{\rm pred}`$ with model phases | `CRYSTAL:make_residual_density_grid` (any points), `make_residual_density_cell` (whole cell) |

There is **no total Fourier map**: nothing makes the density of the cell from $`F_{\rm exp}`$ (or
$`F_{\rm calc}`$) with $`F_{000}`$ on the absolute scale. Pieces A and B need one, so it is the
first thing to add, and it is a map people will want in its own right.

**The plot dispatcher** is in `foofiles/molecule.grid.foo`: three `select case` tables over the
plot kind (`cubify_m` near line 40, `plot_function_m` near line 102, and `make_prop_values` near
line 180). A new kind is added to all three. `MOLECULE.PLOT:set_up_for_plot` and the table near
`molecule.plot.foo:560-760` say what each kind needs set up first (Becke grid, ANOs,
interpolators); a new kind needs a line there too.

**Pieces already written that this work reuses:**

- `FOURIER_SUMS:exp_ikr_sums` (`foofiles/fourier_sums.foo`): the fast sum of
  $`w_i e^{i\mathbf k\cdot\mathbf r_i}`$ with Tonto's own vectorised sin and cos. The file's header
  says why it is written as it is and which compiler flags it needs; do not change those.
- `FOURIER_SUMS:make_solid_harmonics`: $`r^l Y_{lm}(\hat{\mathbf r})`$ for real orthonormal
  harmonics, all $`l \le l_{\max}`$, at a list of points. Private; make it public.
  The order of $`m`$ is that of `GAUSSIAN_DATA:spherical_harmonics_for`: **check it against the
  document's $`m = -1, 0, 1 \to y, z, x`$ before trusting any $`m`$ label.**
- `MOLECULE.RHO:make_stockholder_atom_weight(grid,a,pts)`: Hirshfeld's weight of atom `a` at any
  points. It honours `exphar_power=` through `stockholder_exponent`.
- `MOLECULE.RHO:make_sph_avgd_SA_ED_grid` and its callers (`make_sph_TFVA_atom_FFs`, the
  `sph-exphar` path): the spherical average of an atom in the molecule, on the radial shells of
  its Becke grid with a Lebedev grid on each. **This is the $`l = 0`$ case of piece C.** Piece C
  is the same loop with $`Y_{lm}`$ in the angular sum. Start from this code.
- `BECKE_GRID:radial_shell_for_atom`, `angular_grid_for_atom`, `LEBEDEV:set_l`: the shells and
  angular grids.
- `INTERPOLATOR`: for radial functions tabulated on shells, when a filtered density is plotted.

**The other maps can use the fast sin and cos.** `CRYSTAL:make_residual_density_grid` (the
any-points version) computes `exp(val)*dF(r)` with a complex exponential inside a double loop
over points and reflections. That is version A of `scripts/ff_loop_bench.f90`, 19 to 35 ns per
term; `FOURIER_SUMS` does the same sum in 3 to 10. It is step 1 below. The whole-cell version
(`make_residual_density_cell_n`) already tabulates one-dimensional phases and is not the slow one.

## 2. The formulas to code

Notation as in the document. The cell density is

```math
\rho_{\rm cell}(\mathbf r) = \frac{1}{V_{\rm cell}} \sum_{n=1}^{N_{\rm refl}} F_n e^{i\alpha_n}\, e^{-i\mathbf q_n\cdot\mathbf r} , \qquad (1)
```

with $`F_n e^{i\alpha_n}`$ the structure factor of reflection $`n`$, $`\mathbf q_n`$ its scattering
vector ($`2\pi`$ times the reciprocal lattice vector), and $`V_{\rm cell}`$ the cell volume. The sum
runs over the whole sphere of reflections, both Friedel mates and $`F_{000}`$ included.
$`Y_{lm}`$ are real harmonics normalised to one over the unit sphere; $`j_l`$ is the spherical
Bessel function; a hat marks a unit vector.

**A. The field of local moments,** for a Gaussian window of width $`\sigma`$:

```math
M_{lm}(\mathbf r) = \frac{1}{V_{\rm cell}} \sum_{n} (-i\sigma^2)^l\; q_n^l\, Y_{lm}(\hat{\mathbf q}_n)\; e^{-\sigma^2 q_n^2/2}\; F_n e^{i\alpha_n}\; e^{-i\mathbf q_n\cdot\mathbf r} . \qquad (2)
```

It is equation (1) with each structure factor multiplied by a number, so it uses the same
series routine. $`l = 0`$ is $`1/\sqrt{4\pi}`$ times the density blurred by $`\sigma`$; $`l = 1`$ is
$`\sqrt{3/4\pi}\,\sigma^2`$ times its gradient. The useful scaled form is $`M_{lm}/\sigma^l`$, in the
units of the density.

**B. About one centre** $`\mathbf c`$, at distance $`s`$ from it:

```math
\rho_{lm}(s;\mathbf c) = \frac{4\pi\,(-i)^l}{V_{\rm cell}} \sum_{n} F_n e^{i\alpha_n}\, e^{-i\mathbf q_n\cdot\mathbf c}\; Y_{lm}(\hat{\mathbf q}_n)\; j_l(q_n s) . \qquad (3)
```

Exact, and costly: one sum over all reflections for each $`l`$, $`m`$ and $`s`$. Its use here is as
the reference for the quadrature of C.

**C. Hirshfeld atoms.** With $`w_A(\mathbf r)`$ the weight of atom $`A`$ at $`\mathbf c_A`$, and any
density $`\rho`$:

```math
\rho^A_{lm}(s) = \oint Y_{lm}(\hat{\mathbf s})\; w_A(\mathbf c_A + s\hat{\mathbf s})\; \rho(\mathbf c_A + s\hat{\mathbf s})\; d\hat{\mathbf s}, \qquad
\rho(\mathbf r) = \sum_A \sum_{lm} \rho^A_{lm}(|\mathbf r - \mathbf c_A|)\, Y_{lm}(\widehat{\mathbf r - \mathbf c_A}) . \qquad (4)
```

The integral is over directions $`\hat{\mathbf s}`$, done by a Lebedev grid on each radial shell.
Truncating the second sum at $`l \le L`$ is the filter.

**D. The amplitude.** The same with $`\varphi_A = \sqrt{w_A\,\rho}`$ in place of $`w_A\rho`$:

```math
\varphi^A_{lm}(s) = \oint Y_{lm}(\hat{\mathbf s})\; \sqrt{w_A\,\rho}\,\big|_{\mathbf c_A + s\hat{\mathbf s}}\; d\hat{\mathbf s}, \qquad
n^A_l = \sum_{m=-l}^{l} \int_0^\infty \varphi^A_{lm}(s)^2\, s^2\, ds, \qquad \sum_l n^A_l = N_A . \qquad (5)
```

$`n^A_l`$ is the number of the atom's electrons of angular character $`l`$, and $`N_A`$ its electron
count. The pitfalls (cross terms, not orbital occupations, a free N atom gives $`n_1 = 0`$) are in
section 3.3 of the document and must go into the output's heading, briefly.

## 3. Which density

Two densities can be analysed, and the pieces suit them differently.

- **The wavefunction density** (static, the molecule or cluster of the HAR). Available at any
  point from `make_ED_grid`, exact, never negative, no $`F_{000}`$ problem. **C and D start here:**
  the angular quadrature needs the density at points on spheres about atoms, which is exactly what
  `make_ED_grid` gives. A does not apply to it directly (it has no structure factors as a cell
  density until `F_calc` is made).
- **The Fourier density** of equation (1), from $`F_{\rm exp}`$ with the model's phases, or from
  $`F_{\rm calc}`$. This is the experimental, thermally smeared density of the crystal. **A and B
  live here.** C can use it too, with the density at the quadrature points from the series
  routine; D can only where the map is not negative, which a measured map is in places, so D on
  measured data comes last and may not be worth doing.

A keyword chooses: `density_source= wavefunction | f_exp | f_calc` (names to agree).

## 4. Steps

Each step ends with something that can be checked. Debug build for the code, release for
numbers; `tests/long/urea_rhf_STO-3G_HAR` (4 s) is the working job.

**Step 1. `CELL_MAP`, the series routine, and the total Fourier map.** *(serves A, B, C)*

- Make the type `CELL_MAP` of section 5 and move the residual map onto it, changing no number.

- `FOURIER_SUMS`: a routine for equation (1) at a list of points with complex coefficients,
  `res(p) = Re sum_n coeff(n) exp(-i q_n . r_p)`, built on the same inner loop as `exp_ikr_sums`
  (reflections outside, points inside, `sin_cos`). Two calls of `exp_ikr_sums` with the roles of
  points and k swapped, one for the real and one for the imaginary part of the coefficient, would
  do; a dedicated routine avoids the two passes.
- `CRYSTAL:make_residual_density_grid` calls it instead of its own loop.
  **Check:** `long/L_alanine_minmax_residual_density_map` and
  `long/YLID_IAM_plus_anomalous_residual_density` unchanged; time the map before and after.
- `CRYSTAL`: the coefficient set for a total map, beside
  `DIFFRACTION_DATA.SET:make_symop_generated_dF_a_v2` which makes it for the residual: $`F_{\rm exp}`$
  (scale and extinction removed, as the residual does) or $`F_{\rm calc}`$ with the model phase,
  expanded by symmetry, plus $`F_{000}`$ = the electrons in the cell.
- `cell_map= { kind= f_exp }` and `kind= f_calc`, plotted by `plot_grid= { kind= cell_map }`
  (names to agree).
  **Check:** the map integrates to the electrons in the cell; near an atom at rest it resembles
  `electron_density` blurred by the ADPs; the `f_exp` map minus the `f_calc` map equals
  `residual_density`.

**Step 2 (piece A). The field maps.**

- Multiply the coefficients of step 1 by the factor in equation (2); `make_solid_harmonics` gives
  $`q^l Y_{lm}`$ for all reflections at once. Then the series routine.
- Keywords in the `cell_map=` block: `l_value=`, `m_value=` and `window_width=` (a length), and
  `m_value= all` for $`(\sum_m M_{lm}^2)^{1/2}/\sigma^l`$, which does not depend on the axes and
  is the map to look at first.
- **Check:** $`l = 0`$ equals the density map made with coefficients blurred by $`\sigma`$; the three
  $`l = 1`$ maps equal a finite-difference gradient of it times $`\sqrt{3/4\pi}\,\sigma^2`$; moving the
  cell origin (a second job with all atoms shifted) moves the map and does not change it.

**Step 3 (piece B). The one-centre reference.**

- A routine for equation (3): radial functions about a point for $`l \le L`$ on a list of radii.
  Spherical Bessel functions by upward recurrence from $`j_0`$, $`j_1`$ (fine for $`l \le 6`$ at the
  arguments met here; for $`q s < l`$ use the series). Not a plot kind: a table, and the reference
  for step 4.
- **Check:** against `check_formulas.py` on its synthetic density, to the digits it prints.

**Step 4 (piece C). Hirshfeld atoms.**

- `MOLECULE.RHO`: `make_atom_lm_radial_functions(a,l_max,r,f)`, the loop of
  `make_sph_avgd_SA_ED_grid` with $`Y_{lm}`$ in the angular sum, giving `f(shell, lm)` on the atom's
  radial shells. The Lebedev order on each shell must integrate $`Y_{lm}`$ times a density of
  comparable angular content: at least $`2 l_{\max} + 2`$, more near bonds. Do **not** inherit a
  pruned COSX or SG angular grid; set the order explicitly.
- Output 1, a table per atom: the charge in each $`l`$ shell is zero for $`l > 0`$, so print
  $`\int |\rho^A_{lm}| s^2 ds`$ or the power $`\sum_m \int (\rho^A_{lm})^2 s^2 ds`$, and the radial
  functions to a file for plotting.
- Output 2, plot kinds: `l_filtered_density` with `l_max=` (equation 4 truncated) and
  `l_component_density` with `l_value=` (one $`l`$, summed over $`m`$ and atoms). Radial functions
  are interpolated between shells (`INTERPOLATOR`).
- **Checks:** (i) $`l = 0`$ reproduces the spherical Hirshfeld atom (`sph-exphar` with
  `exphar_power= 1`) and its charge; (ii) `l_filtered_density` tends to `electron_density` as
  `l_max` rises, and the remainder at `l_max= 4` is tabulated; (iii) with the weight set to one and
  the Fourier density, it reproduces step 3.

**Step 5 (piece D). The amplitude.**

- The same routine with a switch that takes the square root of $`w_A\rho`$ at each point before the
  angular sum. Populations $`n^A_l`$ by the radial quadrature.
- Keyword `put_atom_l_populations` (name to agree): a table of $`n^A_l`$, $`l = 0 \ldots l_{\max}`$, for
  each atom, the sum, and $`N_A`$ beside it.
- **Checks:** $`\sum_l n^A_l = N_A`$ to the quadrature's accuracy; a single N atom and a Ne atom
  give $`n_1 = 0`$; urea: do the O lone pairs and the planar skeleton show in $`n_1`$ and $`n_2`$, and
  how large are they? That last number decides whether the method is useful, and nobody knows it
  yet.

**Step 6. Tests and a user page.** A short test for each of steps 1, 2, 4 and 5, blessed on
achari2 with the reference build. Then `docs/` gets a user-facing page (what the maps are, the
keywords, the pitfalls), and this file is deleted.

## 5. Where the code goes: a new type, `CELL_MAP`

**Dylan's proposal (2026-10-06), adopted:** a new type `CELL_MAP` holds a density of the cell as
its Fourier coefficients, with the settings of the maps made from it, and the Fourier-synthesis
procedures now scattered over three modules move into it.

**Why.** In the first draft of this plan the new settings (`l_value=`, `m_value=`, `l_max=`,
`window_width=`, `density_source=`) became components of `PLOT_GRID`. That type already has 72
components and is about the *geometry* of a plot: where the points are. What is being added is
about the *function* plotted. And the residual map's machinery is spread out today:

| routine | where it is now |
|---|---|
| `make_residual_density_grid`, `make_residual_density_cell`, `make_residual_density_cell_n` | `CRYSTAL` |
| `set_Fourier_multiplicities` | `CRYSTAL` |
| `make_symop_generated_dF_a_v2` (the symmetry expansion and the coefficient) | `DIFFRACTION_DATA.SET` |
| `get_minmax_residual_density`, `_p` (which drive a `PLOT_GRID` to get a minimum and maximum) | `MOLECULE.RHO` |

**What `CELL_MAP` holds** (a plain record; component descriptions in `types.foo` one line each):

- the coefficients: Miller indices (three `VEC{INT}`) and a `VEC{CPX}` of coefficients, already
  expanded over the whole sphere, on the absolute scale, divided by the cell volume;
- the reciprocal cell matrix, to turn indices into scattering vectors, and the cell volume;
- `kind`: which coefficients (`f_exp`, `f_calc`, `residual`);
- the filter settings: `l_value`, `m_value`, `window_width`.

**What it does not hold:** a `CRYSTAL`, a `MOLECULE`, atoms or a `PLOT_GRID`. It is given what it
needs as arguments. That keeps it out of the containment tangle that `docs/TASK_CRYSTAL_HOIST.md`
is about, and makes it the kind of flat record the re-engineering wants.

**Its procedures:**

| procedure | does |
|---|---|
| `set_coefficients(h,k,l,coeff,cell)` | takes the expanded coefficients |
| `apply_blur(width)`, `apply_l_moment(l,m,width)` | multiply the coefficients by the factor of equation (2) |
| `make_values_at(values,pts)` | equation (1) at any points, through `FOURIER_SUMS` |
| `make_values_on_cell(values,nx,ny,nz)` | the whole cell, with one-dimensional phase tables |
| `make_lm_radial_functions(f,centre,radii,l_max)` | equation (3), the one-centre reference |
| `put_minmax(...)`, `integral` | the minimum, maximum and rms now made in `MOLECULE.RHO`, and the electron count |

`CRYSTAL` keeps one routine that fills a `CELL_MAP` from its reflections (the symmetry expansion,
multiplicities, $`F_{000}`$, scale), because that needs the space group and the data.
`make_residual_density_grid` and its fellows become three-line callers of it, then go.

**What stays outside `CELL_MAP`:**

| what | module |
|---|---|
| the series kernel, public `make_solid_harmonics`, spherical Bessel functions | `FOURIER_SUMS` (plain procedures, no object) |
| per-atom radial functions and populations (pieces C and D): they need atoms and weights | `MOLECULE.RHO`, beside `make_sph_avgd_SA_ED_grid`; for a Fourier density they ask a `CELL_MAP` for values at their quadrature points |
| new plot kinds | the three tables in `MOLECULE.GRID`, and `MOLECULE.PLOT:set_up_for_plot` |
| `l_max=`, `density_source=` for pieces C and D, and `put_atom_l_populations` | `MOLECULE.MAIN` keywords |

**Keywords.** A `cell_map= { }` block in `MOLECULE.MAIN`, read by `CELL_MAP:read_keywords`:
`kind=`, `l_value=`, `m_value=`, `window_width=`. `plot_grid= { kind= cell_map }` then plots
whatever the block describes; `residual_density` stays as a plot kind, for existing inputs.

**Order of work.** Make the type and move the residual map onto it *first*, as step 1, with the
two residual-map tests as the check that nothing moved. The new maps are then additions to a
type that is already tested.

**Cautions.** A new type means an edit to `types.foo`, which every module depends on: a full
rebuild. Check each component name against Fortran's case rule and against the macro names in
`include/macros.in`. `k` and `l` as Miller-index names clash with nothing here, but `l` the Miller
index and `l` the angular momentum must not meet in one routine: call the indices `h1,h2,h3`.

## 6. Traps

- **Case-only name clashes** (CLAUDE.md rule 1) are everywhere in this subject: `l` and `L`, `m`
  and `M`, `Y` and `y`, `r` and `R`, `s` and `S`. Use `lm`, `l_max`, `rad`, `Ylm`, and check each
  routine's locals before compiling.
- **`PURE` and `DIE`:** a check that must fire in release cannot sit in a `PURE` routine
  (it cost a release build on the SG grids). Use `ENSURE` inside, and a `DIE` in a non-`PURE` caller.
- **The order of $`m`$** in `make_solid_harmonics` is `GAUSSIAN_DATA`'s, not necessarily
  $`-l \ldots l`$. Print the $`l = 1`$ harmonics at $`(1,0,0)`$, $`(0,1,0)`$, $`(0,0,1)`$ once and write
  the answer in the routine header.
- **The sign of the exponent.** Equation (1) has $`e^{-i\mathbf q\cdot\mathbf r}`$ and the factor
  $`(-i)^l`$ goes with it. `CRYSTAL:make_residual_density_grid` uses `exp(-2 pi i h.x)`, the same
  sign; `exp_ikr_sums` computes $`e^{+i\mathbf k\cdot\mathbf r}`$. Get the $`l = 1`$ gradient check of
  step 2 to pass before anything else: it fixes the sign.
- **$`F_{000}`$ and the scale.** A map without them is not a density, and the square root of step 5
  has no meaning for it.
- **Friedel mates.** The residual map uses multiplicity factors
  (`set_Fourier_multiplicities`) to cover the half of reciprocal space not stored. Equation (2)
  for odd $`l`$ changes sign between $`\mathbf q`$ and $`-\mathbf q`$, so the factor must be applied to
  each generated reflection with its own direction, before any doubling.
- **Thermal smearing.** The Fourier density is smeared by the ADPs and the wavefunction density is
  not. Their $`l`$ components differ for that reason alone; do not compare them as if they should
  agree.

## 7. Decisions for Dylan

1. What "version 3" is (section 0).
2. The keyword names: the block `cell_map= { kind= l_value= m_value= window_width= }`, and
   `l_filtered_density`, `l_component_density`, `put_atom_l_populations`, `l_max=`,
   `density_source=`.
3. Whether C and D start on the wavefunction density, as recommended in section 3.
4. The default `l_max` (suggest 4) and window width (suggest 0.5 Å).
5. Whether the radial functions are wanted as gnuplot files, as the fit plots are.

## 8. Log

- 2026-10-06. Theory written and checked (`angular_decomp.pdf`). Plan written; then revised to put
  the Fourier maps in a new type `CELL_MAP` (Dylan). Found on the way:
  the earlier derivation's components depend on the cell origin; the any-points residual map uses a
  complex exponential in its inner loop.
