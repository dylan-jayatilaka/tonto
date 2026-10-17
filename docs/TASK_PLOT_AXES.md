# TASK: a shared frame for plots and cell maps, and cube files VESTA can use

A working document: the plan, what was done, what was checked, and what is left.
The user-facing result is in `docs/REPORT_ON_L_DECOMPOSITION_MAPS.md`, sections 2.1, 3 and 4.

## 1. What was asked (2026-10-15)

Dylan, looking at the urea $`l = 0, 1, 2`$ maps in VESTA, asked for three things:

1. the $`m`$ components of the local moments, the radial functions and the Hirshfeld tables in a
   local frame: a centre atom, $`x`$ along a bond, $`y`$ along a second bond made perpendicular,
   $`z`$ orthogonal to both, as `PLOT_GRID` already does for plots; and that the axis code be
   shared between `PLOT_GRID` and `CELL_MAP` rather than written twice;
2. the cube writer fixed so that VESTA's cell is the true cell;
3. cube files that list the complete molecules of the cell, so that VESTA draws whole
   molecules and not fragments, and if possible a box around those molecules.

## 2. Decisions

- **No Wigner matrices.** Every $`m`$ component is a Fourier series whose coefficient carries
  the solid harmonic of the scattering vector. Rotating the frame is evaluating the harmonic at
  the components of $`\mathbf q`$ along the new axes; the phase factor is untouched. One extra
  matrix product in each of the three routines. The norm over $`m`$ is unchanged, which is the
  check.
- **A component type, not `get_from`.** `get_from` copies a routine's text into each module
  that asks for it and needs the same fields declared in both types; a block of state with its
  own keywords is better as one type held by both. `PLOT_AXES` holds the centre, the centre
  atom or atoms, the six axis atoms, the three axis vectors and their flags; it reads the
  keywords, completes the frame, and gives the rotation matrix. `PLOT_GRID` keeps its widths and
  origin, which the old axis setters mixed with the frame.
- **The completion rule.** A given $`z`$ is kept and $`x`$, $`y`$ made orthogonal to it;
  otherwise $`x`$ is kept and $`y`$, $`z`$ made orthogonal to it. This is what the old
  `PLOT_GRID` code converged to in the common cases. When both $`y`$ and $`z`$ are given, $`z`$
  wins; the old code let the last one read win.
- **Atom indices are resolved late.** `PLOT_AXES` holds no atoms. `PLOT_GRID` resolves at read
  time, as before, since its widths need the vector. `CELL_MAP` holds no atoms by design, so
  `MOLECULE.MAIN:set_cell_map_axes` resolves against the molecule's atoms before every use. An
  explicit `centre=` or `x_axis=` forgets the atoms that gave it.
- **Cube atom lists.** `plot_atoms= box | unit_cell | molecules` in `plot_grid= { }`. `box` is
  the old behaviour: the molecule's atoms inside the box, which for a cell map dropped the
  atoms of the asymmetric unit outside the cell (urea's second hydrogen site). `molecules` is
  the unit cell atoms with every bonded atom in neighbouring cells added until no bond leads
  out, from `CRYSTAL.unit_cell_connection`. `use_molecules_as_bbox` boxes that list along the
  current axes with `box_margin=` to spare; the density is a Fourier series and can be
  evaluated on any box.
- **`cell.cube` drops the repeated plane.** The grid on a cell has both ends of each axis;
  VESTA takes the cell edge as the number of points times the spacing, so the old file drew
  urea 1.9 % too large. The ordinary `gaussian.cube` is left alone, since Gaussian's own
  convention is not periodic.

## 3. What was done

Branch `plot-axes`, from `grid-optimization`.

| file | change |
|---|---|
| `foofiles/plot_axes.foo`, `types.foo` | the new type and module |
| `foofiles/plot_grid.foo`, `types.foo` | the fifteen frame fields replaced by `axes :: PLOT_AXES`; the setters and readers forward to it; `complete_axes` in place of the three `set_xyz_axes_from_*`; `plot_atoms=`, `use_molecules_as_bbox`, `box_margin=`, `set_bbox_around` |
| `foofiles/cell_map.foo`, `types.foo` | `centre` replaced by `axes`; the frame keywords forwarded; the harmonics of `make_moment_at` and `make_radial_functions` taken in the frame |
| `foofiles/molecule.main.foo`, `molecule.rho.foo` | `MOLECULE.RHO:set_cell_map_axes` before every use of the block (it lives in RHO, not MAIN: MAIN uses RHO, and a call the other way is the submodule cycle of the developer page); the Hirshfeld quadrature takes the rotation matrix; the molecules box in `read_plot_grid` |
| `foofiles/molecule.grid.foo`, `molecule.put.foo` | the centre atom now at `plot_grid.axes.center_atom`; the cube atom list chosen in `dump_plot_grid` |
| `foofiles/molecule.plot.foo`, `cell_map.foo` | `m_maps= norm | all | auto`; with axes given a `plot` of a local moment writes every component (`_m<m>` after the label) and the norm from their squares under the label (`do_cell_map_component_plots`) |
| `foofiles/crystal.foo` | `create_whole_molecule_atom_list` |
| `foofiles/archive.foo` | `write_gaussian_cube` takes `all_atoms`; `write_gaussian_cell` writes one point fewer per axis |
| `CMakeLists.txt` | `plot_axes.foo` in `FOO_SRC` and `plot_axes.F90` in `FORTRAN_SRC`: a new module goes in both lists, or it is translated but never compiled and the first module that uses it fails with "Cannot open module file" |

## 4. Checks

Urea, B3LYP/def2-TZVP HAR, maps of $`F_{\rm exp}`$ on the cell at 0.4 bohr, window 0.5 Å, frame
$`x`$ from N2 to O and $`y`$ from N2 toward N1 (the indices meant C, but the molecule's atom
list is the CIF's eight atoms, O, N1, N2, C, H1 to H4; the check holds in any frame). Job and
cubes: `~/Dropbox/tonto_data/cell_maps/urea/axes_check/`.

| # | what was compared with what | measure | result |
|---|---|---|---|
| 1 | the $`l = 1`$ and $`l = 2`$ norms made on their own (`m_maps= norm`), against the norm written with the components (the root of the sum of their squares) | largest difference over 16,767 points | 0 for both $`l`$ |
| 2 | the same norms against the root sum of squares recomputed from the component cube files | largest difference | 1.2e-5 and 5.0e-5, the five printed decimals |
| 3 | the $`l = 1`$, $`m = 1`$ component in the molecular frame against the one in the cartesian frame | largest difference, largest value | 0.70, 0.62: the frames differ as they must |
| 4 | the radial functions about N2 with $`z`$ out of the plane | the odd columns $`(1,0)`$, $`(2,-1)`$, $`(2,1)`$ | zero at every radius, as the mirror plane requires |
| 5 | the atoms in the cube headers | count | 16 on the cell by default; 64 with `plot_atoms= molecules`, eight complete molecules |

The urea cell-map test of the long suite, run against the new binary, fails as expected and
for no other reason: its two reference cubes have 4 atoms in the header where the new ones
have 16 (the "16 vs 4" line of the comparison), and its stdout has the new `m maps` line and
the frame block; the energies differ at 1e-6, which is the LKO cutoff now on by default
(handover of 2026-10-15), not this work. Re-bless on achari2 with the DFT references.

## 5. Log

**2026-10-16, overnight.** Dylan asked for the Hirshfeld-atom maps summed over the molecule,
comparable to the local-moment maps. Added: `make_rebuilt_atoms_grid` takes the frame of the
`cell_map` axes and one `m_value`, and the atoms are expanded from the Fourier density when
`density_source= cell_map`; `set_up_for_plot` resolves the axes and makes the cell map for the
kinds `angular_hirshfeld` and `hirshfeld_amplitude`; the all-components plot loop covers those
kinds, writing the sum over m under the plain label (a norm for a local moment); the rebuilt
maps are now in e/Å³ (and its square root for the amplitude) like the cell maps. Checks on
urea: the mirror-forbidden components ($`l=1, m=0`$; $`l=2, m=\pm1`$) are zero to 1e-14, each
$`l`$ map is the sum of its components to 5e-5, the $`l=0`$ piece peaks at 51 e/Å³ like the
density. Jobs: `*_hirshfeld_maps.stdin` beside the cubes, 22 cubes a molecule (`ha_*` the
density pieces, `amp_*` the amplitudes). Rendered with `render_like.py`, which copies the tile
rotation of the chosen session onto new maps. **Finding:** the summed pieces are dominated by
concentric shells around the atoms at the neighbour distances, intramolecular and
intermolecular: a Hirshfeld atom holds a sliver of each neighbouring core, and its expansion
truncated at $`l \le 2`$ spreads that spike into a shell; the square root of the amplitude
makes the shells larger still (a sliver $`w_A \rho`$ of 1 e/Å³ at a core of 100 is an amplitude
of 1). The pictures are therefore not comparable with the local-moment maps as they stand; a
radial cutoff per atom, a higher $`l_{\max}`$, or weights that fall faster are the options, for
Dylan to decide. The three sets of cubes and the panels are on Dropbox; nothing was added to
the manuscript.

**2026-10-16, later the same night: two causes of the shells, one fix still wrong.**
(1) The ribbing Dylan saw was series-termination ripple: the atoms were expanded from the raw
Fourier density. Fixed and committed: `CELL_MAP:make_blurred_values_at`, the density through
the window, is now the source when `density_source= cell_map`, so the atoms compare with the
local moments. (2) The shells are mostly the weights: `make_stockholder_atom_weight` sums the
denominator over `.atom`, the molecule's own eight atoms, so in the gap between molecules
urea's atoms claim the neighbours' density. A crystal weight was written,
`MOLECULE.RHO:make_crystal_stockholder_weight` (denominator over the unit-cell atoms translated
through $`\pm 2`$ cells within 16 bohr, each with the free density of a molecule atom of the
same element), and is used in `make_atom_lm_radial_functions` for the cell-map source. **It is
wrong and uncommitted:** on urea the summed $`l = 0`$ piece is 13 times the windowed density at
the median grid point and 100 times at worst (`urea_f_exp_l0.cube` times $`\sqrt{4\pi}`$ is the
reference on the same grid), so the weights sum far above one. Suspects, unchecked: the
interpolators of the non-unique atoms (`.atom` is the CIF's eight; the partition may build
interpolators only for the five unique ones, which would make the numerator garbage for N2,
H2, H4), or the frame of `unit_cell_atom` positions. Next: print $`\sum_A w_A`$ over all
crystal atoms at a few points, or reuse the cluster weights HAR already uses
(`oc-hirshfeld`). No Hirshfeld maps were stored on Dropbox from this run; the earlier
`ha_*`/`amp_*` cubes and panels for urea, made with the molecule-only weights and the raw
density, are still there and are not to be used.

**2026-10-15.** Plan agreed with Dylan; code written. Seven build passes to a clean build:
the new module in only one of CMake's two source lists; a stale module file on the dropped
circular edge; `set_cell_map_axes` first put in MAIN and called from RHO, the documented
submodule cycle; a second caller of the Hirshfeld quadrature. The checks of section 4 passed
at the first run. Dylan then asked that every $`m`$ component be written when axes are given,
with the norm as a by-product (`m_maps=`), and that the urea cubes on Dropbox be regenerated in
the molecular frame; the old cubes there were deleted. Later the same evening Dylan asked for
double the resolution and a box around one molecule only: `plot_atoms= molecule` and
`use_molecule_as_bbox` were added (the molecule containing the plot's centre atom), and all
three molecules, urea, L-alanine and gly-L-ala, were run that way at 0.1 bohr, 36 cubes each,
the earlier sets deleted. The maps are drawn in ChimeraX, installed that evening, by the three
scripts of `scripts/cell_map_panels/` (a panel of the eleven F_exp maps with one camera, an
autosave of the view, a renderer for the panel and the single images); the urea figure went
into the IUCrJ manuscript. A second set, `urea_f_exp_molecules*`,
is the $`F_{\rm exp}`$ maps on a box around the eight complete molecules of the cell
(`plot_atoms= molecules`, `use_molecules_as_bbox`, `box_margin= 1 angstrom`): 99 by 99 by 117
points, 64 atoms in the header, 15 MB a file. The notes for Santosh were also placed in the
IUCr journals template, `~/Dropbox/tex/conference_talks/2026_ED_angular_decomp_iucrj/`.

**2026-10-16.** Three additions to the cell maps, all uncommitted at the time of writing.

*Riesz density maps* (`cell_map= { moment_kind= riesz }`): each term of the series multiplied by
$`(-i)^l Y_{lm}(\hat{\mathbf q})`$ alone, no window and no $`q^l`$; $`F_{000}`$ kept for
$`l = 0`$ only. Suggested by Santosh Panjikar, first called Panjikar maps, renamed by Dylan to
the mathematical name. Checked on urea: the $`l = 0`$ map equals $`\rho/\sqrt{4\pi}`$ to the
cube's printed precision (4e-5 relative), and each norm equals the root of the summed squares
of its components (2e-5). For $`l = 2`$ the map is $`-1/4\pi`$ times the electronic field
gradient.

*Missing reflections* (`cell_map= { fill_missing= TRUE }`). The gly-L-ala Hirshfeld populations
were absurd (O 6 to 9, H up to 3.8, $`\sum n_0`$ 70 of 78). Ruled out, in order: the crystal
weights (probe: about 1 at each nucleus, cluster complete, 575 atoms within 20 bohr as the cell
volume predicts); series-termination ripple (0.21 and 0.25 A windows change nothing; urea cut to
the gly resolution, 0.767 /A, gives correct populations); the measured data (F_calc gives the
same failure); Friedel mates (removing them changes nothing); the weights against the density
(wavefunction density with crystal weights is correct, 76.6). Cause: the gly-L-ala list lacks
39 reflections within its resolution, eight of them strong and below 0.23 /A ((1 0 1), (0 0 2),
(0 2 0), (0 2 1), (2 1 0), (1 2 2), (2 3 0), (3 2 0)). Adding them by hand with zero weight gave
$`\sum n_0`$ 76.2, total 77.8. The keyword does this in code: CRYSTAL:make_missing_reflections
finds them; MOLECULE.MAIN:make_filled_reflections values them with the refinement's form factors
(HAR, or IAM after an IAM refinement) when the block is read; MOLECULE.RHO:make_cell_map appends
them while a map is made. L-alanine lacks three low-angle ones, (0 5 1), (3 2 0), (3 0 2); urea
none below 0.60 /A. A fast reproduction of the gly fault: IAM refinement plus an STO-3G SCF,
one minute (`tmp/iam_pop_gly` in the session job directory).

*Plane maps* through `plot_format= gnuplot.contour` work for the cell maps unchanged: urea
$`|M_1|`$ on the molecular plane, linear levels, ChimeraX colours, in
`~/Dropbox/tonto_data/cell_maps/urea/plane/`. Tonto's linear scale is always symmetric about
zero, so a norm map uses half the colour bar.
