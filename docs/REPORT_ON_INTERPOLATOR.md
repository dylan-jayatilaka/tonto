# The atomic density table: `INTERPOLATOR`

A Hirshfeld weight, a promolecule density or a stockholder surface needs the spherical density
of each atom at millions of points. Tonto evaluates each kind of atom once, on a table against
the distance from the nucleus, and looks the table up after that. This page says what the
table is, how it is looked up without a search, how each kind of atom fills it, which keywords
touch it, and where the routines are.

## 1. The table

The table is indexed by the squared distance $`q = r^2`$, which is what the Cartesian
differences give without a square root. Its points are evenly spaced inside each power of two
of $`q`$: the interval $`[2^e, 2^{e+1})`$ holds $`m`$ cells, so the $`j`$-th point of that
octave is

```math
q_{e,j} = 2^{e}\left(1 + \frac{j}{m}\right), \qquad j = 0, \ldots, m-1 , \qquad (1)
```

with $`m`$ = `octave_cells`, 1024 by default. The first point is $`q = 0`$; the octaves run from
the one holding $`r_{\min}^2`$ ($`r_{\min}`$ = $`10^{-6}`$ bohr) to the one holding
`table_length`$`^2`$ (30 bohr); and the table ends early, at the first point where the function
falls below `table_eps`, the function being taken to decrease from there on. For an atom that is
about 48,000 points and 0.4 MB. Near the nucleus the spacing is a millionth of a bohr and in the
tail a hundredth, so the core of a heavy atom and its tail are both resolved, where an even
spacing in $`r`$ or $`r^2`$ resolves one or the other. With 1024 cells the table reproduces the
density of a free atom to about $`10^{-6}`$ relative inside 3 bohr and to a few $`10^{-5}`$ in
the tail, where the density is $`10^{-10}`$.

The value at a point $`q`$ is interpolated linearly between its two neighbours, so a
looked-up value lies between them; below the first tabulated distance the value at zero is
used, and beyond the last, zero.

**No search.** The cell a point lies in is read from the floating-point representation of
$`q`$ itself. Fortran's `exponent(q)` gives $`e`$ and `fraction(q)` gives $`q/2^{e}`$ in
$`[1/2, 1)`$, so

```math
t = \left(2\,\mathrm{fraction}(q) - 1\right) m, \qquad k = \lfloor t \rfloor, \qquad
\beta = t - k , \qquad (2)
```

and the point is $`k`$ cells into octave $`e`$: its index in the table is
$`(e - e_0)\,m + k + 2`$, with $`e_0`$ the first octave, and the value is
$`(1-\beta)\, v_{k} + \beta\, v_{k+1}`$. There is no logarithm, no bisection and no table of
breakpoints to walk: a lookup is two intrinsic calls, an integer multiply and one linear
interpolation. The points of (1) are made the same way, by `set_exponent`, so the two agree
exactly.

## 2. How a table is made

The table does not evaluate the function itself, so it can be made in pure code and holds
no function pointer: the caller asks for the points, evaluates its density at their square
roots, hands the values back, and finalises:

```
.interpolator.set_octave_points          ! .data_point = the q of (1)
rho = density at sqrt(.interpolator.data_point)
.interpolator.set_data_values(rho)
.interpolator.finalize                   ! ends the table where rho < table_eps
```

| atom | who fills it | the density |
|---|---|---|
| Gaussian basis (the ANO density after an atomic SCF) | `ATOM:make_interpolator`, `make_octave_interpolator` | `ATOM:make_ED_grid_r` along the x axis from the nucleus, in batches of 4096 points |
| Slater basis | `SLATERBASIS:make_interpolator` | `VEC{SLATERSHELL}:densities_at_radii` |
| Coppens basis | `COPPENSBASIS:make_interpolator` | `VEC{COPPENSORBITAL}:densities_at_radii` |
| spherically averaged Hirshfeld atoms, bonded density atoms | `MOLECULE.HAR:make_SAHA`, `make_averaged_BDAs` | the average of the Hirshfeld-weighted molecular density over a Lebedev shell at each point |

`MOLECULE.RHO:make_interpolator_template` makes the template every atom's table is copied from,
with `table_eps` set to the cluster's atom density cutoff; `make_ANO_interpolators`,
`make_slater_interpolators` and `make_coppens_interpolators` fill one table for each atom kind
and copy it to the atoms of that kind. The averaged atoms use a coarser table, 64 cells to the
octave from 0.01 bohr, since every point costs a spherical shell of density evaluations; the
ANO table is their start.

## 3. Lookups

| call | what it does |
|---|---|
| `values_for(q,values,fac)` | sets `values` to the table's values at the squared distances `q`, times `fac` if present |
| `add_values_for(q,values,fac)` | adds them to `values` instead |
| `value_for(q)` | one value |

`ATOM:make_interpolated_density` and `add_interpolated_density`, and the Slater and Coppens
`make_interpolated_ED_grid` and `add_interpolated_density`, form the squared distances from a
set of points and call these; `ATOM:make_ED_grid` chooses between the table and the direct
evaluation by `use_interpolator`. `use_interpolators= NO` in a job turns every table off.

## 4. Keywords

```
interpolator= {
   table_eps=     1.0d-10   ! the table ends where the density falls below this
   table_length=  30        ! the largest distance tabulated, bohr
   octave_cells=  1024      ! cells in each power of two of the squared distance
   put                      ! print the table
}
```

The block sets the template the atoms' tables are made from. The keywords of the earlier
kinds of table — `interpolation_method=`, `domain_mapping=`, `domain_scale_factor=`,
`range_mapping=`, `table_spacing=`, `table_origin=`, `n_data=`, `y1_initial=`, `y1_final=` —
are read and ignored, so an input that sets them, as CrystalExplorer's do, still runs.
`put_interpolator_list` prints the tables of the atoms named on its line, or of all of them.

## 5. The cluster radius

A cluster made for a Hirshfeld surface must hold every atom whose density reaches the
surface. `VEC{ATOM}:max_density_cutoff_radius` gives, over the atoms, the distance at which the
atom density first falls below the cluster's `atom_density_cutoff`, stepping out 0.02 bohr at a
time to 30 bohr at most (`ATOM`, `SLATERBASIS` and `COPPENSBASIS:density_cutoff_radius`); it
evaluates the density directly and does not need a table.

## 6. Lines between atoms

`put_ed_between_atoms A B [n]`, `put_sa_ed_between_atoms A B [n]`,
`put_sa_cell_fn_between_atoms A B [n]` and `put_spherical_sa_ed_from_atom A [r_max [n]]`
write the density, the Salvador atom density or the Salvador cell function along the line from
atom `A` to atom `B` at `n` evenly spaced points (101 by default) as gnuplot data and plot files.
`MOLECULE.RHO:make_ED_line_between_atoms` and its three siblings return the points and the
values; `MOLECULE.MISC:put_line_gnuplot` writes them.

## 7. Where it is implemented

| piece | procedure |
|---|---|
| the points of (1) | `INTERPOLATOR:set_octave_points` |
| the values, and the end of the table | `INTERPOLATOR:set_data_values`, `finalize` |
| the lookup (2) | `INTERPOLATOR:octave_values_for`; `values_for`, `add_values_for`, `value_for` |
| the settings and the keywords | `INTERPOLATOR:set_table_eps`, `set_table_length`, `set_octave_cells`, `read_keywords` |
| printing | `INTERPOLATOR:put`, `put_gnuplot` |
| the template | `MOLECULE.RHO:make_interpolator_template` |
| the tables of the atom kinds | `MOLECULE.RHO:make_ANO_interpolators`, `make_slater_interpolators`, `make_coppens_interpolators` |
| filling a table | `ATOM:make_octave_interpolator`; `SLATERBASIS:make_interpolator`; `COPPENSBASIS:make_interpolator` |
| the averaged atoms | `MOLECULE.HAR:make_SAHA`, `make_spherically_averaged_HA`, `make_averaged_BDAs` |
| the density from a table | `ATOM:make_interpolated_density`, `add_interpolated_density`; `SLATERBASIS` and `COPPENSBASIS:make_interpolated_ED_grid`, `add_interpolated_density`, `density_at_radius` |
| the cluster radius | `VEC{ATOM}:max_density_cutoff_radius`; `ATOM`, `SLATERBASIS`, `COPPENSBASIS:density_cutoff_radius` |
| the lines between atoms | `MOLECULE.RHO:make_ED_line_between_atoms`, `make_SA_ED_line_between_atoms`, `make_SA_cell_fn_line_between_atoms`, `make_sph_SA_line_from_atom`; `MOLECULE.MISC:put_line_gnuplot` |

## 8. What is not done

- **One table for each kind of atom.** The table is copied to every atom of a kind, 0.4 MB
  each; holding it once and pointing to it belongs with the lightening of the derived types.
- **The averaged atoms have no test**, and their coarser table (64 cells) is a choice, not a
  measurement.
- **A heavy-atom structure.** The effect of the table on a Hirshfeld weight and a structure
  factor beyond Kr waits for a data set with such an atom.
- **A line plot along a `plot_grid` line** (`y_width= 0`, `z_width= 0`) writes only part of
  its points; not looked into.
