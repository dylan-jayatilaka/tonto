# The atomic density table: `INTERPOLATOR`

Hirshfeld weights, promolecule densities and stockholder surfaces need the spherical density of
each atom at millions of points. Tonto evaluates each kind of atom once, on a table against the
distance from the nucleus, and looks the table up after that. This page says how the table is
laid out, how a point is found in it without a search, what is assumed about the function, and
how accurate it is.

## 1. The model: linear in $`q = r^2`$ on a grid that doubles

The function $`f(r)`$ is tabulated against the squared distance $`q = r^2`$, which the Cartesian
differences give without a square root. Nothing is fitted: the function is evaluated at the table
points, and between two neighbouring points it is taken as linear in $`q`$,

```math
f(q) \approx (1-\beta)\, f(q_k) + \beta\, f(q_{k+1}), \qquad \beta = \frac{q - q_k}{q_{k+1} - q_k} . \qquad (1)
```

What makes the table suit an atom is where the points are. Each power of two of $`q`$, an
*octave*, is cut into the same number $`m`$ of equal cells (`octave_cells`, 1024 by default). The
octave $`[2^{e-1}, 2^{e})`$ has the points

```math
q_{e,j} = 2^{e-1}\left(1 + \frac{j}{m}\right), \qquad j = 0, 1, \ldots, m-1 . \qquad (2)
```

So the spacing is a fixed fraction of the distance itself: $`\Delta q/q`$ lies between $`1/2m`$ and
$`1/m`$, and $`\Delta r/r`$ is about half that. With $`m = 1024`$ the points are $`10^{-9}`$ bohr
apart at $`r = 10^{-6}`$ bohr, $`10^{-3}`$ bohr apart at 1 bohr, and $`0.03`$ bohr apart at 30 bohr.
The core of a heavy atom, which changes on a scale of hundredths of a bohr, and the tail, which
changes on a scale of a bohr, are both resolved by one table, where an even spacing in $`r`$ or
$`q`$ resolves one or the other.

## 2. Finding a point with Fortran's floating-point intrinsics

A double-precision number is stored as a binary fraction and a power of two,
$`q = f \times 2^{e}`$ with $`\tfrac12 \le f < 1`$. Fortran reads and writes these two parts directly:

| intrinsic | returns | example, $`q = 3`$ |
|---|---|---|
| `exponent(q)` | the power $`e`$ | 2, since $`3 = 0.75 \times 2^2`$ |
| `fraction(q)` | the fraction $`f`$ | 0.75 |
| `set_exponent(x,e)` | `fraction(x)` $`\times 2^{e}`$ | `set_exponent(0.75,2)` = 3 |

The exponent of $`q`$ *is* its octave in equation (2), and the fraction says where in the octave it
lies. Writing $`e_0`$ for the exponent of the first octave, the lookup is

```math
t = \big(2\,\mathrm{fraction}(q) - 1\big)\, m, \qquad k = \lfloor t \rfloor, \qquad \beta = t - k, \qquad
\text{index} = \big(\mathrm{exponent}(q) - e_0\big)\, m + k + 2 , \qquad (3)
```

the 2 allowing for the point $`q = 0`$ at the head of the table, and the value is equation (1). For
$`q = 3`$ with $`m = 1024`$: $`t = (1.5 - 1) \times 1024 = 512`$, so $`q`$ lies exactly on the 512th point
of the octave $`[2, 4)`$. There is no search, no logarithm and no list of breakpoints: a lookup is
two intrinsic calls, an integer multiply and one linear interpolation, the same cost wherever the
point lies. The points of equation (2) are made with `set_exponent(1/2 + j/(2m), e)`, the inverse of
the lookup, so the two agree to the last bit.

## 3. What is assumed about the function

- **It depends on the distance only.** The table is one-dimensional; it holds a spherical atom.
- **It falls to zero.** The table ends at the first point where the function drops below
  `table_eps` ($`10^{-10}`$ by default), and beyond the end the value is zero. A function that dips
  below `table_eps` and rises again would be cut off at the dip.
- **It is finite at the nucleus.** Below the first tabulated distance, $`r_{\min} = 10^{-6}`$ bohr,
  the value at $`q = 0`$ is used.

Nothing else is assumed: the table is not specific to exponentials, and serves any spherical
function that decays, such as the averaged Hirshfeld atoms and the Slater and Coppens densities.

## 4. Accuracy and size

The error of linear interpolation in a cell of width $`\Delta q`$ is at most
$`\tfrac18 (\Delta q)^2 \max|f''(q)|`$, so it falls by four when $`m`$ doubles. With $`m = 1024`$ the table
reproduces the density of a free atom to about $`10^{-6}`$ relative inside 3 bohr and to a few
$`10^{-5}`$ in the tail, where the density is near $`10^{-10}`$. A table of one atom, from
$`10^{-6}`$ to 30 bohr, is about 48,000 points, 0.4 MB.

## 5. How a table is made and used

The table does not evaluate the function itself, so it can be made in pure code and holds no
function pointer. The caller asks for the points, evaluates its density at their square roots,
hands the values back, and finalises:

```
.interpolator.set_octave_points          ! .data_point = the q of equation (2)
rho = density at sqrt(.interpolator.data_point)
.interpolator.set_data_values(rho)
.interpolator.finalize                   ! ends the table where rho < table_eps
```

`values_for(q,values,fac)` sets `values` to the table at the squared distances `q`, times `fac` if
present; `add_values_for` adds them instead; `value_for(q)` gives one value. One table is made for
each kind of atom (`MOLECULE.RHO:make_ANO_interpolators`, `make_slater_interpolators`,
`make_coppens_interpolators`, from the template `make_interpolator_template`) and copied to the
atoms of that kind.

```
interpolator= {
   table_eps=     1.0d-10   ! the table ends where the function falls below this
   table_length=  30        ! the largest distance tabulated, bohr
   octave_cells=  1024      ! cells in each power of two of the squared distance
   put                      ! print the table
}
```

The keywords of the earlier kinds of table (`interpolation_method=`, `domain_mapping=`,
`range_mapping=`, `table_spacing=` and the like) are read and ignored, so inputs that set them, as
CrystalExplorer's do, still run. `use_interpolators= NO` in a job evaluates every density directly.

| piece | procedure |
|---|---|
| the points, equation (2) | `INTERPOLATOR:set_octave_points` |
| the values, and the end of the table | `INTERPOLATOR:set_data_values`, `finalize` |
| the lookup, equation (3) | `INTERPOLATOR:octave_values_for`, called by `values_for`, `add_values_for`, `value_for` |
| filling a table | `ATOM:make_octave_interpolator`, `SLATERBASIS:make_interpolator`, `COPPENSBASIS:make_interpolator`, `MOLECULE.HAR:make_SAHA`, `make_averaged_BDAs` |
| the density from a table | `ATOM:make_interpolated_density`, `add_interpolated_density`; the Slater and Coppens `make_interpolated_ED_grid`, `add_interpolated_density` |

## 6. Not done

- **One table for each atom.** The table is copied to every atom of a kind; holding it once and
  pointing to it belongs with the lightening of the derived types.
- **The averaged atoms** use a coarser table, 64 cells to the octave from 0.01 bohr, since every
  point costs a spherical shell of density evaluations; that choice is not measured, and they have
  no test.
- **A heavy-atom structure.** The effect of the table on a Hirshfeld weight beyond Kr waits for a
  data set with such an atom.
