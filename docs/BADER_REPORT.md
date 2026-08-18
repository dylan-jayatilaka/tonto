# Bader basin analysis: the `archive/Bader` port

**Ported 2026-08-18 from tag `archive/Bader`** (28 commits, Dylan Jayatilaka 19,
Max Davidson 9, 2018-08 – 2019-02; +1907/−791 across seven files). This is the
largest capability that was still stranded on an archive tag.

**Status: compiles, runs, and does not yet give a usable answer.** The objective was
to get the branch's work onto `develop` in a form that builds and whose gross errors
have been found — not to establish that it computes the right answer. Two errors were
found by running it, and they are characterised below: the basin count is wildly
grid-dependent (1 basin for water on one grid, 13942 on another), and voxel volumes
are summed per grid point but sized per grid interval. Neither is a defect of the
port; both are in the algorithm as the branch left it. There is no Bader test of any
kind in `tests/`, and none ever existed, so nothing here is compared against a
reference.

## What was ported

The serial half. The branch splits cleanly in two, and only one half was taken.

| New procedure | File | What it does |
|---|---|---|
| `MOLECULE.PROP:get_Bader_basins` | `molecule.prop.foo` | Entry point. Builds the electron density on the isosurface plot grid with a one-point zero border, then drives the two routines below. Keyword `get_bader_basins`. |
| `MOLECULE.PROP:assign_Bader_basins(ED,dv)` | `molecule.prop.foo` | Assigns every interior grid point to a basin by steepest ascent over the 26 neighbours — the on-grid method of Henkelman et al. (2006). Paths terminate on a maximum or on an already-claimed point. |
| `MOLECULE.PROP:put_Bader_basin_info` | `molecule.prop.foo` | Per-basin volume and electron count as a `TABLE_COLUMN` table, then triangulates. |
| `ISOSURFACE:prepare_Bader_grid` | `isosurface.foo` | Grid set-up. Unlike `prepare_grid` it does **not** call `set_for_marching_cubes`. |
| `ISOSURFACE:cubify_Bader(basin,n_basin)` | `isosurface.foo` | Marching cubes over the basin index array, one surface per basin, all appended to the same point and face lists. |
| `ISOSURFACE:update_4_slab(p,f,slice,basin)` | `isosurface.foo` | Slab loader for the basin index array. Sibling of `update_4_slab_m`, which loads function values. |
| `MARCHINGCUBE:set_vertex_info(p,f,r)` | `marchingcube.foo` | Sets `.vertex_fn_value` to ZERO inside basin `r` and ONE outside it. |
| `MARCHINGCUBE:interpolate_Bader_faces` | `marchingcube.foo` | Interpolates edge vertex *positions* only. |
| `MARCHINGCUBE:interpolate_Bader_edge_info` | `marchingcube.foo` | The interpolation itself, private. |
| `PLOT_GRID:volume`, `PLOT_GRID:pixel_volume` | `plot_grid.foo` | Plot box volume and voxel volume. |

**How the triangulation works, since it is not obvious.** A basin index is a step
function, so there is no isosurface to bisect. The port turns membership into a
two-valued field — ZERO inside basin `r`, ONE outside — and sets the cube's
isovalue to `HALF`. The existing `case_number` then sets a bit for each vertex
inside the basin, and the existing `set_triangulation_info` and
`set_triangle_vertex_info` work unchanged. Only the edge interpolation had to be
new, and only because gradients, hessians and curvatures are meaningless for a
step function; with values of exactly ZERO and ONE the interpolated point always
lands on the edge midpoint, so the surface falls on the voxel boundary, which is
what a grid-based basin boundary is.

## What was deliberately not ported

**The parallel half.** Max Davidson's linked-list rework of
`cubes_to_basin_parallel` is mid-debug: its convergence loop is replaced by a
hard-coded three iterations, and every merge branch leaks the node it just
allocated. Recorded in `DEFERRED.md` under *"Parallelise the Bader basin search"*,
with the specific defects and the recovery commands, because the idea is sound and
worth returning to. `develop`'s existing `cubes_to_basin_parallel` is untouched.

**`PARALLEL:gather`.** The branch adds a `gather` template with 19 type
instantiations. `develop` already has one (`parallel.foo:4827` onwards), added
independently and better: `root` is a required argument rather than an optional one
passed straight to `MPI_GATHER` regardless of whether it is present.

**The `types.foo` changes.** `basin_at_vertex` was added to `CAPPING_SQUARE` and
`MARCHINGCUBE` and never read anywhere on the branch — the code reuses
`value_at_vertex` instead. `BADER`, `LINKED_LIST_MAT_INT` and `HEAD_MAT_INT` exist
only for the parallel half. The `DEFAULT(0)` initialisers the branch adds are
already on `develop`. So the port needs **no** `types.foo` change at all.

**`edge_vertex_index :: VEC{INT}(0:12)` → `(0:11)`, and the disabling of
`divide_cubes_small_map`.** The branch narrowed this member by one element
(`89429b41`, *"Corrected 12 -> 11 dimensioning error"*), which broke
`small_map` — whose only use of element 12 is a re-use counter — and so commented
the whole 358-line `divide_cubes_small_map` out and added *"DISABLED ... has wrong
reference to edge_vertex_index(12)"* to the `small_map` doc comment in `types.foo`.
That is a regression in an unrelated code path and was not taken.

**Three dead procedures.** `MARCHINGCUBE:set_case_info(r)` and
`MARCHINGCUBE:case_number(r)` are defined on the branch and called from nowhere,
and are wrong as written: `r` is declared `REAL` and compared with `.equals(r)`
against a vertex value the branch itself sets to ZERO or ONE, so for any basin
index other than 0 or 1 no bit would ever be set. `cubify_Bader` calls the
argument-less `set_case_info`, which is correct. `MARCHINGCUBE:put_minimal` and
`put_positional_info_min` are debug scaffolding with no caller; the second prints
`.edge_vertex_value`, which the Bader path never sets.

**The commented-out `ENSURE` in `plot_grid.foo`.** The branch disables
`ENSURE(.x_or_y_or_z_axes_defined,...)` in `set_bbox_with_current_axes`. That is a
symptom of the Bader job not defining plot axes, not a fix; the assertion is left
in place.

## Changes made during the port, and why

The branch predates the current tree by seven years and the API moved underneath
it. Every change below is mechanical unless noted.

| Branch | Now | Note |
|---|---|---|
| `:::` procedure attributes | `::` | The `3ca1e53d` migration (2026-07-09). |
| `value_at_vertex` | `vertex_fn_value` | Member renamed. |
| `MAT3{VEC_{INT}}`, `VEC{VEC_{INT}}` | `MAT3{EVEC{INT}}` | Type renamed. |
| `.GRID:make_density_grid_r` | `.make_ED_grid_r` | Procedure renamed. |
| `MAT3{INT}*`, `VEC{REAL}*` locals | `MAT3{INT}@`, `VEC{REAL}@` | Pointers became allocatables throughout. |
| `.associated` | `.allocated` | Follows from the above. |
| `info(x,y,2)[0:] = ...` | `info(x,y,2).element = ...` | Matches the live `cubify` at `isosurface.foo:1166`. |
| `.iso_value` set to `0.5d0` | `HALF` | `include/macros.in`. |
| `basin = ZERO`, `n_basin = ZERO` on `INT` | `= 0` | Assigning the REAL macro to an integer. |
| `f1,f2 :: INT` reading `.vertex_fn_value` | `:: REAL` | The branch declared the interpolation end-values as integers. Compiles either way; the truncation would have put every vertex at ZERO. |

Four further departures are judgement calls rather than renames:

- **`.saved.isosurface` is gone.** The branch does `.saved.isosurface => .isosurface`
  so that a `self :: IN` routine can mutate an isosurface through the alias. `.saved`
  is now `MOLECULE@` and `.isosurface` is `ISOSURFACE@`, so `=>` no longer compiles
  and `=` would copy. The three new routines are declared `self :: INOUT` and use
  `.isosurface` directly, which is what the aliasing was working around.
- **`cubes_to_basin(ED,dv)` was renamed `assign_Bader_basins`.** The branch *replaced*
  `develop`'s `cubes_to_basin(grid,nx,ny,nz,gridlength,gridvector,CP,EDS,VOL)` with a
  two-argument routine of the same name. Keeping both under one name would have given
  a generic with two unrelated implementations — the trap `CLAUDE.md` §8 warns about.
  The old routine and `get_Bader_regions` are untouched and still reachable by the
  `get_bader_regions` keyword, so the two algorithms can be run against each other.
- **`prepare_Bader_grid` sets `.del_z` from `.plot_grid.del_z`**, where both the
  branch and `prepare_grid` set it from `.plot_grid.del_x`. In `prepare_grid` that is
  guarded by `ENSURE(.plot_grid.width.has_all_equal,...)`; `prepare_Bader_grid` has no
  such guard, so copying `del_x` would be wrong on a non-cubic grid.
- **Dead locals dropped.** `interpolate_Bader_edge_info` declared `g1,g2,G,h1,h2,H,S,
  UV,n,u,v,L` and used none of them; `cubify_Bader` declared `capping_number,
  face_number, b, cap_square, oblique, do_capping, region`, likewise unused;
  `get_Bader_basins_sing` declared `CP, EDS, VOL, ng`; `put_Bader_basin_info`
  declared `zfs, tx, ty, tz` for a commented-out zero-flux-surface block.

## A pre-existing defect this port sits next to

`MARCHINGCUBE:set_left_info`, `set_front_info` and `set_below_info` declare their
argument `VEC{INT}(0:12)` — thirteen elements — while `cubify` creates the `info`
array with `[0,11]` and passes `info(...).element`, which has twelve. `cubify_Bader`
reproduces the same pattern, because it is the pattern the live code uses
(`isosurface.foo:1106`). No read is actually out of bounds: the three routines only
read elements 1, 5, 9 and 10. It is a declared-shape overstatement, harmless in a
release build, and it is the thing the branch was trying to address when it
narrowed the member to `(0:11)` and broke `small_map`. If it is fixed, fix the
declaration to match the twelve elements that are passed — do not narrow
`MARCHINGCUBE.edge_vertex_index`, which legitimately has thirteen.

## How to run it

There is no test. A job needs an isosurface with a plot grid, then the keyword:

```
isosurface= { ... plot_grid= { ... } ... }
get_bader_basins
```

## What running it actually does — two defects, both measured

The port was exercised on water, RHF/STO-3G, `use_bbox_with_shape_axes`. It runs to
completion and exits 0. It does not produce a usable answer, for two reasons that
are in the ported algorithm rather than in the port.

### 1. The basin count is wildly grid-dependent

| grid | box scale | basins found | electrons |
|---|---|---|---|
| 21 × 19 × 15 | 0.75 | **1** | 12.22 |
| 41-point request | 2.0 | **13942** | 12.28 |

Water has three nuclei and should give three basins. Neither number is close.

The two failures have different causes and both are inherent to the on-grid method
as written. On the tight box, every path runs uphill to the oxygen: at 0.46 bohr
spacing no grid point sits close enough to a hydrogen nucleus for the cusp to
register as a local maximum, so there are no hydrogen basins to find. On the wide
box the opposite happens — in the outer region the density is flat to the precision
of the arithmetic, so every neighbour gives `Grad` of exactly zero, `EG_max` stays
`ZERO`, the point is its own maximum, and each such point becomes its own basin.
That is where 13942 comes from.

So the method needs a grid fine enough to resolve every nuclear cusp and tight
enough to exclude flat density, and nothing in the code checks for or reports
either condition. A density floor below which a point is not seeded, and a check
that the basin count is commensurate with the number of nuclei, are the obvious
guards; neither is a mechanical addition, so neither was made.

### 2. Voxel volumes are summed per point but sized per interval

`sum(VOL)` counts one `dv` for each of the n_x·n_y·n_z grid **points**, while
`PLOT_GRID:pixel_volume` is the box volume divided by the
(n_x−1)(n_y−1)(n_z−1) grid **intervals**. Volumes and electron counts are therefore
overstated by ∏ n_i/(n_i−1) — 18.75% on the 21 × 19 × 15 grid above, and 44% on an
11-point grid. Measured on the tight box:

```
Volume of grid ......... 578.231543   <- sum(VOL), i.e. 5985 points x dv
Volume of plot box ..... 486.931826   <- 9.177179 x 8.259461 x 6.424025
```

That is the whole systematic error in the electron count: 12.220067 / 1.1875 =
**10.29** against a true 10, the residual 2.9% being grid coarseness and the 0.75
box truncation. So the density integration is roughly right; the voxel bookkeeping
is not.

Both volumes are now printed side by side, so the discrepancy is visible in the
output rather than inferred. Fixing it means choosing an integration rule —
trapezoidal weights on the boundary points, or defining the voxel volume from the
point count — which is a scientific decision and was left alone.

### What was checked and is *not* wrong

An earlier reading of these numbers suspected memory corruption: a grid point the
search never reaches keeps basin index 0, and `EDS`/`VOL` start at 1, so a zero
index would write outside them silently. A count was added and it reports
**`Unassigned points = 0`** on every grid tried — every point does get assigned, so
that write never happens. The guard and the count were kept, because the failure
would be silent if it ever did.

The erratic totals that prompted the suspicion (9.12, 9.31, 39.08, 12.22 electrons
across n = 5, 9, 11, 21) are fully explained by defect 2 compounding with coarse
sampling of the oxygen cusp, not by corruption. The grid is not cubic —
`n_points= 21` gives 21 × 19 × 15 — which is what made the point counts look
irregular.

## What is needed next, in order

1. **Decide whether the on-grid method is the right one.** Defect 1 is not a bug to
   patch: a nearest-neighbour steepest ascent on a plot grid cannot find a cusp it
   does not sample. Henkelman's later near-grid and weight methods exist for
   exactly this. This is the question to settle before any further work.
2. **Fix the voxel bookkeeping** (defect 2). Isolated, and the instrument to check
   it against is now in the output.
3. **Then a reference and a test job**, so the path runs in CI — it currently does
   not run anywhere. Blessing a reference is Dylan's call.
4. **Then the triangulation.** The branch's last commit says *"Plots reasonable
   Bader surfaces, but further work still necessary"*. `cubify_Bader` was never
   reached in any of the runs above with a believable basin set, so it is compiled
   and unexercised. Note also that capping is not done, so a basin cut by the plot
   grid boundary gives an open surface.
5. **Then parallelise** — see `DEFERRED.md`.
