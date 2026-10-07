# Angular (l) decomposition of density maps

The plan for putting the angular decomposition of electron densities into Tonto, beside the
total, deformation and residual density maps that already follow an X-ray refinement.
Opened 2026-10-06 (Dylan, with Santosh Panjikar). Nothing is coded yet.

**The theory is not repeated in full here.** It is in
`~/Dropbox/tex/conference_talks/2026_ED_angular_decomp/angular_decomp.pdf` (source
`angular_decomp.tex`), with every formula derived and checked numerically by
`check_formulas.py` in the same folder. Read that first. The formulas needed for coding are
collected in section 2 below, in its notation.

## 0. The order of work, and the names (Dylan, 2026-10-06)

The document has four pieces. Dylan's order is **A, then C, then D**; B is not a product, only the
exact reference that C is tested against.

| piece | where in the document | what it is | name of the map |
|---|---|---|---|
| A | section 4, "Version 2" | the field of local moments $`M_{lm}(\mathbf r)`$: maps over the cell | `local_moment` |
| B | section 3.1 | the expansion about one chosen centre (the Bessel formula) | none: a test reference |
| C | section 3.2 | Hirshfeld atoms expanded about their nuclei; the true decomposition | `angular_hirshfeld` |
| D | section 3.3 | the same for the square root of each atom; populations $`n^A_l`$ | `hirshfeld_amplitude` |

All three names are Dylan's. The table of populations for D is printed by the keyword
`put_ha_populations`, routine `put_HA_populations`: keywords are lower case, and in routine names
the abbreviation HA (Hirshfeld amplitude) is written in capitals, as `ADP` and `ED` are.

**Output (Dylan).** Every map is written as a Gaussian cube file, as Tonto's other maps are, to be
viewed in VESTA. So each of the three is a plot kind that fills the points of a `PLOT_GRID` and
goes out through the existing writer (`plot_format= gaussian.cube`, or `cell.cube` for a whole
cell); no new file format. For `local_moment` that is one cube file for each $`l, m`$ asked for.
For `angular_hirshfeld` and `hirshfeld_amplitude` the cube holds the density rebuilt from the
atoms' radial functions up to `l_max=`, or one $`l`$ alone, on the plot's points.

**The norm over m comes first (Dylan: "most important").** Nobody should have to look at every
$`m`$ component to begin with. For each $`l`$ the default output is the one quantity that does not
depend on the axes, and a single $`m`$ component is made only when `m_value=` asks for it:

| map | default for each $`l`$ | with `m_value=` |
|---|---|---|
| `local_moment` | the norm $`\big(\sum_m M_{lm}(\mathbf r)^2\big)^{1/2}/\sigma^l`$: one cube file per $`l`$ | the signed component $`M_{lm}/\sigma^l`$ |
| `angular_hirshfeld` | the density of that $`l`$, $`\sum_A\sum_m \rho^A_{lm}\,Y_{lm}`$, which is already independent of the axes | one $`m`$ alone, in the frame given |
| `hirshfeld_amplitude` | the populations $`n^A_l`$, which are sums over $`m`$ | the share of one $`m`$ |

So the first thing to code and to look at, in every step, is the default column. The $`m`$
components need a choice of axes (a local frame on each atom, or the crystal's), which can wait.

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

**Step 2 (piece A). The field maps: `local_moment`.**

- Multiply the coefficients of step 1 by the factor in equation (2); `make_solid_harmonics` gives
  $`q^l Y_{lm}`$ for all reflections at once. Then the series routine.
- Keywords in the `cell_map=` block: `l_value=` and `window_width=` (a length). With no `m_value=`
  the map is the norm over $`m`$ (section 0); `m_value=` gives one signed component. Code the norm
  first: it needs the $`2l+1`$ component maps internally, squared and added point by point.
- **Check:** $`l = 0`$ equals the density map made with coefficients blurred by $`\sigma`$; the three
  $`l = 1`$ maps equal a finite-difference gradient of it times $`\sqrt{3/4\pi}\,\sigma^2`$; moving the
  cell origin (a second job with all atoms shifted) moves the map and does not change it.

**Step 3 (piece B). The one-centre reference** (no keyword; needed to test step 4).

- A routine for equation (3): radial functions about a point for $`l \le L`$ on a list of radii.
  Spherical Bessel functions by upward recurrence from $`j_0`$, $`j_1`$ (fine for $`l \le 6`$ at the
  arguments met here; for $`q s < l`$ use the series). Not a plot kind: a table, and the reference
  for step 4.
- **Check:** against `check_formulas.py` on its synthetic density, to the digits it prints.

**Step 4 (piece C). Hirshfeld atoms: `angular_hirshfeld`.**

- `MOLECULE.RHO`: `make_atom_lm_radial_functions(a,l_max,r,f)`, the loop of
  `make_sph_avgd_SA_ED_grid` with $`Y_{lm}`$ in the angular sum, giving `f(shell, lm)` on the atom's
  radial shells. The Lebedev order on each shell must integrate $`Y_{lm}`$ times a density of
  comparable angular content: at least $`2 l_{\max} + 2`$, more near bonds. Do **not** inherit a
  pruned COSX or SG angular grid; set the order explicitly.
- Output 1, a table per atom: the charge in each $`l`$ shell is zero for $`l > 0`$, so print
  $`\int |\rho^A_{lm}| s^2 ds`$ or the power $`\sum_m \int (\rho^A_{lm})^2 s^2 ds`$, and the radial
  functions to a file for plotting.
- Output 2, plot kind `angular_hirshfeld`: with `l_max=` it is equation 4 truncated (the filtered
  density); with `l_value=` it is one $`l`$ alone, summed over $`m`$ and atoms. Radial functions
  are interpolated between shells (`INTERPOLATOR`).
- **Checks:** (i) $`l = 0`$ reproduces the spherical Hirshfeld atom (`sph-exphar` with
  `exphar_power= 1`) and its charge; (ii) `angular_hirshfeld` with `l_max=` tends to `electron_density` as
  `l_max` rises, and the remainder at `l_max= 4` is tabulated; (iii) with the weight set to one and
  the Fourier density, it reproduces step 3.

**Step 5 (piece D). The amplitude: `hirshfeld_amplitude`.**

- The same routine with a switch that takes the square root of $`w_A\rho`$ at each point before the
  angular sum. Populations $`n^A_l`$ by the radial quadrature.
- Keyword `put_ha_populations`: a table of $`n^A_l`$, $`l = 0 \ldots l_{\max}`$, for
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
| `set_coefficients(h1,h2,h3,coeff,cell)` | takes the expanded coefficients |
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
| `l_max=`, `density_source=` for pieces C and D, and `put_ha_populations` | `MOLECULE.MAIN` keywords |

**Keywords, as coded.** A `cell_map= { }` block in `MOLECULE.MAIN`, read by
`CELL_MAP:read_keywords`: `kind= residual | f_exp | f_calc` is the density; `l_value=` turns the
map into the local moment of that $`l`$ (the norm over $`m`$, or one signed component with
`m_value=`), with `window_width=` a length (default 0.5 Å); `l_value=` resets `m_value`, and a negative `l_value` gives the plain density again. `plot_grid= { kind= cell_map }` then
plots whatever the block describes; `put_cell_map` prints the settings, the reflection count and
the electron count; `residual_density` stays as a plot kind, for existing inputs. The block keeps
its settings between plots, so a sequence of maps changes one keyword at a time. The $`l = 0`$
map keeps its sign (a residual is signed); the plan's name `local_moment` is the map's name in
prose, not a keyword.

**Which plot kinds go into `CELL_MAP`, and how it is called.** Only the maps that are Fourier
sums of structure factors: `residual`, `f_exp`, `f_calc`, and `local_moment` made from any of
them. The hundred or so other kinds in `MOLECULE.GRID` (orbitals, the electron density of the
wavefunction, ELF, potentials) are evaluated from basis functions at points and stay where they
are; so do `angular_hirshfeld` and `hirshfeld_amplitude`, which need atoms. The names are the
same at both levels, so nothing a user types changes:

- *by keyword:* `MOLECULE.GRID`'s tables keep a one-line case for each Fourier kind
  (`residual_density` as now, and the new ones), which fills the `CELL_MAP` and calls it; inside,
  `CELL_MAP` branches on its own `kind` with a `select case`;
- *by direct call:* after `CRYSTAL` has filled it, any routine may call `apply_blur`,
  `apply_l_moment`, `make_values_at` and the rest directly, as `get_minmax_residual_density`
  will.

**Order of work.** Make the type and move the residual map onto it *first*, as step 1, with the
two residual-map tests as the check that nothing moved. The new maps are then additions to a
type that is already tested.

**Wiring a new type in** (found on checking the plan against the code):

- A plain procedure of `FOURIER_SUMS` is called `FOURIER_SUMS:name(...)` from another module, with
  one colon; `::` is the within-module form and fails to link.
- `CMakeLists.txt` lists every module twice by name, the `.foo` in `FOO_SRC` and the generated `.F90`
  in the library's sources; `cell_map` goes into both, beside `fourier_sums`. Miss the second and the
  build fails with "Cannot open module file 'cell_map_module.mod'".
- `types.foo`: `type CELL_MAP` before `type CRYSTAL`, and `cell_map :: CELL_MAP@` in `MOLECULE`
  beside `plot_grid`. `MOLECULE.SET:destroy_ptr_part` destroys it.
- The residual routines have **five** callers, all of which must give the same numbers after the
  move: the plot kind in `MOLECULE.GRID`; `MOLECULE.RHO:make_residual_density_grid` (which
  converts to electrons per Å³) and `get_minmax_residual_density_p`; `MOLECULE.PUT:make_residual_density_cell`
  and `put_ED_refinement_plots`, which writes the `*.residual_density_map,cell.cube` that
  `long/YLID_IAM_plus_anomalous_residual_density` compares.
- `make_symop_generated_dF_a_v2` is two things in one: the per-reflection coefficient
  (the residual's $`(|F_o| - |F_c|) e^{i\alpha_c}`$ on absolute scale) and the expansion of any
  coefficient over the symmetry-generated reflections with the Friedel and site-symmetry factors.
  Split it: `make_symop_generated_coefficients(coeff_out,g1,g2,g3,spacegroup,mult,coeff_in)` keeps
  the expansion, and each map kind supplies its own `coeff_in`. The arithmetic is the same, so no
  number changes; the `f_exp` and `f_calc` maps then need no second copy of the expansion.
- `get_minmax_residual_density_p` stays in `MOLECULE.RHO`: it drives `.plot_grid` and prints.
  Only the statistics of a whole-cell map (minimum, maximum and rms without double-counting the
  cell faces) move into `CELL_MAP`.

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
- **The order of $`m`$** in `make_solid_harmonics` is Molden's, $`0, +1, -1, +2, -2, \ldots`$ within
  each $`l`$, with $`m>0`$ cosine-like and $`m<0`$ sine-like: for $`l = 1`$ the columns are $`z, x, y`$.
  (The first draft of this note read `into_std_S_order` as saying the opposite; it was settled by
  the parity of the three $`l = 1`$ maps under a screw axis of L-alanine, then by their correlation
  of +0.999 with the finite-difference gradient along the matching axis, both signs positive.)
  `CELL_MAP:make_moment_at` maps the usual $`m = -l \ldots l`$ onto those columns, so `m_value=`
  means what the document means, $`-1, 0, 1 \to y, z, x`$. The norm over $`m`$ depends on neither
  order nor sign, which is one more reason to code it first.
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
- **No module cycles.** The submodules of `MOLECULE` are separate Fortran modules, and `use` must
  not form a cycle: `MOLECULE.SCF` uses `MOLECULE.RHO`, so a routine in `MOLECULE.RHO` cannot call
  `MOLECULE.SCF:make_atom_partition_info`. Make prints `Circular ... dependency dropped`, compiles
  against a stale module, and fails with "Mismatch in components of derived type"; the message
  names the wrong thing. Drivers that need set-up go in `MOLECULE.MAIN`.
- **No fast Bessel code is needed** (Dylan asked, 2026-10-06). The one-centre sum costs one
  $`j_l`$ per reflection per radius, $`4\times10^5`$ evaluations for urea and a few milliseconds;
  it is the exact reference and a small-job tool, and the atoms of step 4 go by quadrature, with
  no Bessel function at all. `FOURIER_SUMS:spherical_bessel` sits beside `sin_cos`, which is where
  a vectorised version would go if a use ever needed one.
- **Thermal smearing.** The Fourier density is smeared by the ADPs and the wavefunction density is
  not. Their $`l`$ components differ for that reason alone; do not compare them as if they should
  agree.

## 7. Decisions for Dylan

1. The name for piece D: `hirshfeld_amplitude`, or another (section 0).
2. The remaining keyword names: the block `cell_map= { kind= l_value= m_value= window_width= }`
   with `kind= local_moment` for piece A, and `put_ha_populations`, `l_max=`,
   `density_source=`.
3. Whether C and D start on the wavefunction density, as recommended in section 3.
4. The default `l_max` (suggest 4) and window width (suggest 0.5 Å).
5. The maps go out as cube files (settled). Whether the atoms' radial functions are *also*
   wanted as tables or gnuplot files.

## 8. Log

- 2026-10-06. Theory written and checked (`angular_decomp.pdf`). Plan written; then revised to put
  the Fourier maps in a new type `CELL_MAP` (Dylan). Found on the way:
  the earlier derivation's components depend on the cell origin; the any-points residual map uses a
  complex exponential in its inner loop.
- 2026-10-06, later. Plan checked against the code before coding: every routine it names exists.
  Added the wiring list in section 5 (CMake list, `MOLECULE` component, five callers, the split of
  `make_symop_generated_dF_a_v2`), and the $`m`$ order from `into_std_S_order` to section 6.
- 2026-10-06, evening. **Step 1 done** on the Mac, debug and release. The residual map now goes
  through `CELL_MAP`; `long/L_alanine_minmax_residual_density_map` and
  `long/YLID_IAM_plus_anomalous_residual_density` pass unchanged, and the alanine Max/Min/RMS agree
  to every printed digit in the debug build too. The total maps `f_calc` and `f_exp` (keyword
  `cell_map= { kind= }`, plot kind `cell_map`) on L-alanine integrate to 192.00 electrons on a
  17 x 29 x 17 grid, the residual to 0.0001, and `f_exp - f_calc - residual` is within the cubes'
  four printed decimals. On the test's 15 x 27 x 15 grid the integral was 187.3: a grid of N
  points including both faces is periodic on N-1, and the k = +-26 reflections fold into the sum.
  The timing of the any-points residual map before and after was not measured. Two more wiring
  facts went into section 5 (the second CMake list; `FOURIER_SUMS:name` with one colon from
  another module).
- 2026-10-06, night. **Step 2 coded** (`make_moment_at`; `l_value=` resets `m_value`, a negative
  `l_value` is the plain density; `put_cell_map`). Checks on L-alanine, f_calc, window 0.5 Å,
  0.5-bohr grid, debug build: the $`l = 0`$ map times $`\sqrt{4\pi}`$ integrates to 192.000; the
  $`l = 1`$ norm is the root sum of squares of the three components to 1e-5; each component
  correlates +0.999 with $`\sqrt{3/4\pi}\,\sigma`$ times the central-difference gradient of the
  blurred density along its axis, ratio 1.05 from the coarse grid: on a 0.25-bohr grid in release
  the correlation is +0.99996 and the largest difference 2% of the map's maximum, as central
  differences allow. The $`m`$ order was wrong in the first version of section 6 and is corrected
  there. Step 2 merged as `1a38d81d`.
- 2026-10-06, night. **Urea, the test Dylan asked for.** HAR at B3LYP/def2-TZVP on the data of
  `tests/long/urea_rhf_STO-3G_HAR` converges in 1.5 min on the Mac (GoF 2.55), then maps of the
  cell at 0.2 bohr (53 x 53 x 45): both total maps integrate to 64.000 electrons. The local
  moments from F_exp (model phases) against F_calc, window 0.5 Å, on the whole cell: $`l = 1`$
  norm maxima 0.641 and 0.642 e/Å³, rms difference 0.0013, correlation 0.99998; $`l = 2`$ norm
  maxima 1.310 and 1.306, rms difference 0.0015, correlation 0.99998; the density itself 49.29
  and 49.34 at the heavy nuclei, rms difference 0.022, correlation 0.99991. On the molecular
  plane $`y = x + 1/2`$ the $`l = 1`$ norm is zero on each nucleus and rings it; the $`l = 2`$
  norm peaks on the nuclei and along C=O. The pictures are gnuplot slices
  (`slice_plane.py`, in the job directory); VESTA has no command-line rendering, so cubes are
  opened in it by hand. The test `long/urea_rks_B3LYP_def2-TZVP_HAR_cell_maps` runs the same job
  at 0.4 bohr and compares `stdout` and the two $`l = 1`$ cubes; blessed on achari2. The same
  moments of the `residual` map (`kind= residual`, the $`l`$-resolved residual) were also made:
  the raw residual is -0.23 to +0.14 e/Å³ with rms 0.022, and through the 0.5 Å window its
  $`l = 0, 1, 2`$ parts have rms 0.0013, 0.0023 and 0.0035 -- a smooth, mostly negative
  background with a positive band across the N-H region. The window is wide for residual
  features; a narrower one (0.25 Å) is the thing to try when the residual is the object.
- 2026-10-06, late. **Window default is 0.25 Å (Dylan).** **Step 3 done and verified.**
  `CELL_MAP:make_radial_functions` (equation 3; `FOURIER_SUMS:spherical_bessel`, upward recurrence
  above $`l_{\max}`$, Miller's downward recurrence below) and the keyword
  `put_cell_map_radial_functions`, with `l_max=`, `centre=` (a length), `centre_fractional=`,
  `n_radii=`, `radius_max=` in the `cell_map=` block. Checked on urea's refined model about O and
  C against an independent Python sum over the whole sphere built from the `.fcf` ($`A, B`$ per
  unique reflection) and the `.fco` symmetry operations (`check_radial.py` in the job directory),
  which itself agrees with an angular quadrature of the series to five decimals: every
  $`\rho_{lm}(s)`$, $`l \le 2`$, $`s = 0.1`$ to 2 Å, agrees to about 1e-4 relative, the level of
  the four figures the `.fcf` prints. A first comparison showed the $`l = 1`$ component about O
  off by 0.7% at 0.1 Å: the centre had been typed rounded to 2.7878 Å (exact 2.78784), and that
  column is the gradient at the nucleus, where the curvature is ~5e3 e/Å⁵; with the same centre
  the two agree. `centre_fractional=` avoids that; it needs the cell, so `read_cell_map` sets it
  from the crystal before reading the block.
- 2026-10-06, late. **Step 4 (and the populations of step 5) coded and checked.**
  `MOLECULE.RHO:make_atom_lm_radial_functions(f,radii,a,l_max,l_lebedev,amplitude,weighted,use_cell_map)`
  is the angular quadrature about a nucleus on a Lebedev grid of explicit order (`max(2 l_max+2, 29)`,
  never a pruned grid); the driver `MOLECULE.MAIN:put_angular_Hirshfeld_atoms` prints, for each
  unique fragment atom on its Becke radial shells, the radial functions, the power
  $`P_l = \sum_m \int f_{lm}^2 s^2 ds`$ and the electron count; keywords
  `put_angular_hirshfeld_atoms` and `put_ha_populations` (the amplitude, $`\sqrt{w_A\rho}`$), with
  `l_max=`, `density_source= wavefunction | cell_map` and `atom_weight= hirshfeld | none` in the
  `cell_map=` block. The driver had to live in `MOLECULE.MAIN`: see the module-cycle trap in
  section 6.
  *Check (iii)*, urea, the Fourier density of the model with no weight, quadrature against the exact
  Bessel sums at the same Becke radii (`check_step4.py`): within 1.5 Å of every nucleus the largest
  difference is 0.004 to 0.009 e/Å³ on values up to 177 (5e-5 relative, the `.fcf`'s four figures);
  beyond that the sphere runs through neighbouring nuclei and the 302-point grid cannot integrate a
  50 e/Å³ spike -- the quadrature's limit, irrelevant once the Hirshfeld weight is on. Two earlier
  false alarms were the check's: the Python $`j_2`$ lost its digits at $`x < 0.05`$ (now a series),
  and radii were being read from three printed decimals (now five).
  *The populations*, urea Hirshfeld atoms of the B3LYP/def2-TZVP wavefunction, $`l \le 2`$:
  O 8.363, 0.0064, 0.0023 (sum 8.372, electron count 8.376); N 7.125, 0.0015, 0.0044 (7.131; 7.143);
  C 5.817, 0.0010, 0.0079 (5.826; 5.842); H 0.848, 0.0155, 0.0012 (0.865; 0.866) and 0.866, 0.0135,
  0.0014 (0.881; 0.882). The sums approach the counts from below, the remainder being $`l > 2`$.
  So the answer to the question of section 4: **the amplitude of a Hirshfeld atom is spherical to
  better than 0.2% of its electrons** -- the O lone pairs and the planar skeleton do not show in
  $`n_1, n_2`$ at any size; the only visible non-sphericity is the hydrogens' $`n_1`$ of 0.014-0.016
  e, the bond polarisation. Whether that makes the measure useless or a clean statement is for
  Dylan. The *density's* power $`P_l`$ for the unweighted cell density about an atom is large for
  $`l = 1, 2`$ (neighbours), as it should be.
  **Not done:** the plot kinds `angular_hirshfeld` and `hirshfeld_amplitude` (the rebuilt density
  on a grid from interpolated radial functions, section 4 output 2) and the per-atom radial
  functions to a file for plotting; the Lebedev order is fixed, not raised near bonds.
  **Also in this commit:** `CIF`'s loop reader stops at a `;` text field (the data-set CIFs end
  their reflection loop with one), and `read_cell_map` sets the cell before reading the block.
- 2026-10-06, night. **Glycyl-L-alanine** (Dylan: for the morning, with the TVFA standardisation
  set), `~/Dropbox/tonto_data/xray_neutron_set/gly_L_ala_150K_xray_Capelli2014.cif`, the first of
  that set read by Tonto: a working CIF is block 1's header and atoms plus block 2's merged
  reflection loop, LF line endings (the memory note on the set has the three things to know). HAR
  at B3LYP/def2-TZVP, 20 atoms, 2532 reflections: 29 min on the Mac, GoF 1.34, residual -0.19 to
  +0.16 e/Å³, rms 0.045. Maps at 0.2 bohr (71 x 91 x 93), window 0.25 Å: both total maps
  integrate to 312.000 electrons; F_exp (model phases) against F_calc: $`l = 1`$ norm maxima
  1.980 and 1.989, rms difference 0.0060, correlation 0.99987; $`l = 2`$ norm 3.191 and 3.193,
  rms 0.0043, correlation 0.99996; the residual's $`l = 1, 2`$ parts reach 0.020 and 0.024 (rms
  0.009), five times urea's through the narrower window. The cubes, the output and the working
  CIF are in `~/Dropbox/tonto_data/cell_maps/gly_L_ala/`, urea's in `.../urea/`.
- 2026-10-07, morning. **The plot kinds `angular_hirshfeld` and `hirshfeld_amplitude`** (section
  4 output 2, step 5's amplitude map) and `atoms= { ... }`, merged as `932a3ba4`. *Check (ii)*
  on urea (all atoms, cell at 0.3 bohr, against `electron_density` of the same wavefunction): rms
  remainder 0.49, 0.43, 0.33, 0.12, 0.12 % of the density's rms at $`L = 0, 1, 2, 4, 6`$; the
  largest point remainder 0.18 of 183 e/bohr³ on a nucleus at every $`L \ge 1`$, the
  interpolation floor. The single-$`l`$ path agrees with $`L2 - L1`$ to 1e-5 everywhere but the
  four nuclear grid points, where the cube's five printed figures of a 180 e/bohr³ value are 1e-3
  coarse; and reading `l_max= 6` (which raises `GAUSSIAN_DATA`'s tables) leaves the $`l \le 2`$
  maps unchanged to the last digit. `atoms=` is a brace-delimited list, as `TEXTFILE:read_all`
  reads; an unbraced list swallowed the next keyword. Check (i), $`l = 0`$ against
  `sph-exphar`, was not run: $`L = 0`$'s 0.49 % remainder is the same statement.
  **The report** `docs/REPORT_ON_L_DECOMPOSITION_MAPS.md` is written (Dylan: theory of the three
  maps from the TeX, figures in `docs/images/`, the table of checks, code, keywords).
- 2026-10-07, afternoon. **Deformation kinds, sharpening, multipoles** (Dylan's three
  refinements). `kind= deformation_calc | deformation_exp` subtract the promolecule, the
  spherical atoms of the current method and basis (`use_IAM_ITC_FFs= FALSE`, the HAR's own
  route) assembled by `CRYSTAL:make_F_calc` at the model's positions and ADPs, stored in the map
  by `MOLECULE.MAIN:set_cell_map_promolecule` when the block is read (the only module above both
  `HAR` and `RHO`). `sharpen_u=` divides a mean ADP out, allowed while below the window squared.
  The atom tables print the multipole moments $`\int f_{lm} s^{l+2} ds`$. A latent bug found on
  the way: `set_coefficients` called `destroy_ptr_part`, which would have wiped `atoms=` (and
  the promolecule) on every map; it now drops only the coefficient arrays. Results on urea and
  gly-L-ala are in the report's section 6 (urea: experiment against model for the deformation
  density 0.93, its moments 0.97-0.98; gly-L-ala 0.70 and 0.51-0.92, its residual being twice
  urea's); the cubes are beside the others in `~/Dropbox/tonto_data/cell_maps/`. The Hirshfeld-atom
  populations of gly-L-ala (a second job, 19 min, output `gly_L_ala_hirshfeld_atoms.stdout`
  beside the cubes) say the same as urea's: $`n_0`$ carries all but 0.1-0.3% of every atom --
  O 8.43, 8.41, 8.28; N 7.04, 6.91; C 5.85-6.07; the ammonium H 0.80, the amide H 0.88, C-H
  0.93-0.99 -- and the only $`n_1`$ above 0.01 e are the N-H hydrogens' (0.011-0.018), the C-H
  at 0.005-0.009 and the carbonyl O at 0.008-0.009; $`n_2`$ is below 0.003 except the carbonyl
  C's 0.01.
