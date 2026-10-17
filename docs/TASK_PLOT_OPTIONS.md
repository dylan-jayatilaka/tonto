# TASK: options and data apart, for plots and cell maps

A working document: the plan for separating the options a user types from the objects that
hold the information a job needs, for plots and for maps from structure factors. Agreed with
Dylan on 2026-10-15, to be worked from 2026-10-16, on branch `plot-axes` before it is merged, so
that the input syntax changes once. The frame work that came first is in
`docs/TASK_PLOT_AXES.md`.

## 1. The diagnosis

Three types each hold two kinds of thing.

| type | the options a user types | the information a job needs |
|---|---|---|
| `PLOT_GRID` (61 fields) | the plot kind, orbital indices, bond atoms, label, format, units, max and min, the contour styling, which atoms a cube lists | the axes, widths, point counts, spacing, origin and box |
| `CELL_MAP` | the kind of map, $`l`$, $`m`$, the window, the sharpening, the frame, the radii, the atom-table settings | the Fourier coefficients of a cell density, the cell matrices, the volume |
| `SCF_DATA` | every SCF option | the state of one SCF |

The same pattern misnames the first group: `SCF_DATA` is not data, it is options. The input
follows the code, so a map needs `plot_grid= { }`, `cell_map= { }` and `plot`, and nothing in
those names says which block holds what.

## 2. The decision

Options and data get separate types, and the options are called what they are.

| options block, read from input | verb | the object the verb makes or fills |
|---|---|---|
| `scf_options= { }` (`scfdata= { }` kept as the old spelling, 92 inputs) | `scf` | the SCF state |
| `plot_options= { }` | `make_plot` (`plot` kept, 6 inputs) | the values on a `PLOT_GRID`, written to a file |
| `cell_map_options= { }` | `make_cell_map` | a `CELL_MAP`: the coefficients, and the maps and tables asked for |
| `plot_grid= { }` | none; it is geometry | a `PLOT_GRID`: a `PLOT_AXES`, widths, points, spacing, origin, box |

The names are long and unambiguous, which matters for a person and for a model reading an
input. The blocks are not nested: `plot_grid= { }` stays a top-level block and
`plot_options= { }` sits beside it, so an old input needs only its plot keywords moved from the
first block to the second. The data objects are light: `PLOT_AXES`, `PLOT_GRID` and
`CELL_MAP` hold only what the job needs, and `CELL_MAP` loses every option.

The tables of the angular decomposition keep their `put_` verbs
(`put_cell_map_radial_functions`, `put_angular_hirshfeld_atoms`, `put_ha_populations`) and
read `cell_map_options= { }`, as outputs do.

**Why now.** When `CRYSTAL` is hoisted out of `MOLECULE` (`docs/TASK_CRYSTAL_HOIST.md`),
`CELL_MAP_OPTIONS`, `CELL_MAP` and `make_cell_map` move to `CRYSTAL` whole, because the
structure factors, the cell and the symmetry are there; `PLOT_OPTIONS` and the wavefunction
kinds stay with `MOLECULE`; `PLOT_AXES` and `PLOT_GRID` are shared geometry. Of the 111 plot
kinds, only `residual_density` (already a cell map), `cell_map` and `angular_hirshfeld` with
`density_source= cell_map` need the crystal; the promolecule, spherical-atom and Stockholder
kinds need only atoms. So after the split one kind straddles, and it stays with the molecule,
taking its density from the crystal's cell map when told to.

## 3. The shared geometry, and `ISOSURFACE`

`ISOSURFACE` holds its own `PLOT_GRID` and uses only its geometry: the point counts and
spacings, the widths, the box, `make_points`, and the marching-cubes resizing. It reads the
block with `plot_grid= { }` inside `isosurface= { }`. It sets the plot kind only to name its
files. So `PLOT_GRID` as geometry alone is exactly what the isosurface code already treats it
as, and the split costs it nothing: the kind it names moves to the isosurface's own options.
`PLOT_GRID` is then a grid of points in a box with a frame, used by plots, cell maps and
isosurfaces alike.

## 4. The work, in order

1. `PLOT_OPTIONS` (`plot_options.foo`, `types.foo`): the plot fields moved out of `PLOT_GRID`
   with their readers; `set_up_for_plot` becomes its validation, run by `make_plot` before
   anything is computed. `PLOT_GRID` keeps the geometry and its readers. `plot_grid= { }`
   accepts the old plot keywords for one release, forwarding them with a warning, so the 29
   inputs can be edited at leisure; or they are edited at once and the warning is not needed.
   Dylan to choose.
2. `CELL_MAP_OPTIONS` (`cell_map_options.foo`): the option fields moved out of `CELL_MAP`;
   `CELL_MAP` keeps the coefficients and the evaluation routines, which take the options as
   arguments. `cell_map_options= { }` and `make_cell_map` in `MOLECULE.MAIN`; the one input that
   uses `cell_map= { }` edited.
3. `SCF_DATA` renamed `SCF_OPTIONS` with `scf_options= { }`, `scfdata= { }` kept as a spelling.
   Mechanical, but it touches many files; last, and only if the first two went well.
4. Inputs, references and documents: the 29 plot inputs, `docs/RUNNING_TONTO.md`,
   `docs/REPORT_ON_L_DECOMPOSITION_MAPS.md` section 3, the test references that print the
   blocks, blessed on achari2 together with the DFT and cell-map references already waiting.

Estimate: two days for 1 and 2 with their inputs and documents; a third for 3.

## 5. Log

**2026-10-15.** Diagnosis and decision, in conversation; this page written. The branch
`plot-axes` is merged first with the frame work as it stands, so the options split starts from
a clean build.
