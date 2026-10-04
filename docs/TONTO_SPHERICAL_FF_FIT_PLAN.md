# Plan: fitting spherical atomic form factors to a few Gaussians

A working document (see `CLAUDE.md` §1). It plans the fit of Tonto's spherical atom form
factors — `sph-tfva`, `sph-tfvp`, `sph-tfvh` — to the International Tables form, for use
in protein refinement programs, and is deleted when the item closes.

## 1. What is wanted

A protein refinement needs a form factor for every atom type at millions of reflections.
Tonto's spherically averaged Salvador atoms (`partition_model= oc-sph-tfva`, `oc-sph-tfvp`,
`oc-sph-tfvh`) give such a form factor, f(s), as a sum over the atom's radial grid
(`MOLECULE.RHO:make_sph_avgd_SA_ED_grid`, then `FOURIER_SUMS:sinc_kr_sums`). That is too
slow to call per reflection, so f(s) is to be fitted once to the form every protein program
already reads,

    f(s) = sum_{i=1..n} a_i exp(-b_i s^2) + c,      s = sin(theta)/lambda in 1/Angstrom,

with n = 4 by default (the International Tables Vol. C Table 6.1.1.4 form, nine numbers per
atom type; f(0) = sum a_i + c is the electron count). The fitted a, b, c are written out in
the format of the target program; the first target is BUSTER.

Decisions taken (Dylan, 2026-10-05):

- Fit in **reciprocal space**: the quantity that must be right is f(s), not the density.
- The fit range is an input; the **default is s = 0 to 2 Å⁻¹**. (0.4 Å resolution is
  s = 1.25 Å⁻¹; the IT tables themselves go to 2.0.)
- The fit is **nonlinear** (a_i, b_i and c all free), because only a few Gaussians may be
  used. The linear even-tempered fit of `oc-ri` uses 14–35 Gaussians per element and is not
  the target, though it is a useful check (§5).
- The **starting values** are the International Tables coefficients of the element,
  already in Tonto (`ATOM:HF_n0_form_factor_coeff`, with the SDS and HF hydrogen sets).
- The minimiser is **gnuplot's `fit`** (Levenberg–Marquardt), run from Tonto through
  `SYSTEM_COMMAND`, which also draws the difference plot f(s) − fit(s).
- The output format is an **option**; a plain table of a, b, c is the default.

## 2. The pieces, and where they go

Lowest module that can hold each piece, as the project prefers.

### 2a. f(s) on an s grid — `MOLECULE.RHO`

`make_sph_atom_FF_curve(f, s, c, s_max, n_s)`: for unique atom `c`, the spherically
averaged Salvador density on the radial grid (`make_sph_avgd_SA_ED_grid`, already weighted
by 4πr²dr), then `sinc_kr_sums` at k = 4π s (with s in Å⁻¹ converted to bohr⁻¹) for `n_s`
evenly spaced s in [0, s_max]. Default `n_s` = 201, so a step of 0.01 Å⁻¹ at the default
range. Any s costs one sinc sum, so the grid is free to choose; the Mura–Knowles radial
points are only the quadrature behind it.

Which radii are used follows `partition_model=` as it does now: `oc-sph-tfva` the molecular
density minimum, `oc-sph-tfvp` the promolecule minimum, `oc-sph-tfvh` the equal-density
point (`uses_promolecule_Salvador_radii`, `uses_Hirshfeld_Salvador_radii`).

### 2b. The fit — a new small module `GAUSSIAN_FF_FIT` (selfless procedures)

Takes `s(:)`, `f(:)`, the start `a0(0:n)`, `b0(0:n)` (index 0 is the constant), the range
and a file stem; returns `a`, `b`, their esds, and the fit's rms and maximum deviation.
Steps:

1. Write `<stem>.dat`: two columns, s and f(s), for s within the range.
2. Write `<stem>.gnuplot`:

   ```
   f(x) = a1*exp(-b1*x**2) + a2*exp(-b2*x**2) + a3*exp(-b3*x**2) + a4*exp(-b4*x**2) + c
   a1 = ...; b1 = ...; ...; c = ...          # the International Tables start
   set fit quiet; set fit errorvariables; set fit limit 1e-10; set fit maxiter 2000
   set fit logfile '<stem>.fit.log'
   fit [0:<s_max>] f(x) '<stem>.dat' via a1,b1,a2,b2,a3,b3,a4,b4,c
   set print '<stem>.coeffs'
   print a1, a1_err, b1, b1_err, ..., c, c_err, FIT_WSSR, FIT_NDF
   set terminal pngcairo size 900,600; set output '<stem>.png'
   plot '<stem>.dat' using 1:($2-f($1)) with lines title 'f(s) - fit'
   ```

3. Run `gnuplot '<stem>.gnuplot'` through `SYSTEM_COMMAND:execute`, synchronously, on the
   master rank only, and treat a failure the way `plot_with_gnuplot` does: a missing gnuplot
   is reported and the job goes on; the data and script are still on disk.
4. Read `<stem>.coeffs` back with `TEXTFILE` into `a`, `b`, the esds, and the residual.
5. Check: f(0) against the electron count (sum a_i + c); the maximum deviation against a
   tolerance (§4); refuse a negative a_i or b_i with a `WARN` and a note in the output — IT
   fits are all-positive and the protein programs may assume it.

Why a module of its own: the fitter knows nothing about atoms or molecules, and the same
code will serve any other curve Tonto wants in this form (an electron scattering factor,
later). gnuplot's `fit` is used rather than writing a Levenberg–Marquardt in Foo because the
problem is small, the plot comes with it, and the project already depends on gnuplot for its
pictures.

### 2c. The driver — `MOLECULE.RHO`, keyword in `MOLECULE.MAIN`

`fit_sph_atom_FFs`: for each unique atom type (by element, or by atom if the user asks —
in a crystal two carbons in different surroundings have different Salvador atoms; the
protein use wants one curve per element, averaged over the atoms of that element in the
molecule, so both are offered and the per-element average is the default), make the curve
(2a), fit it (2b), print a table of a, b, c, their esds, f(0), rms and maximum deviation,
and the start it came from, and write the chosen output format (2d).

Keywords, in a block:

```
fit_sph_atom_FFs= {
   s_max=            2.0        ! 1/Angstrom; the fit range is [0, s_max]
   n_gaussians=      4
   n_points=         201
   per_atom=         NO         ! one curve per element (default) or per atom
   output_format=    table      ! table | buster | ...
   file_stem=        <name>.sph_ff
}
```

`partition_model=` must be one of the three `sph-` models; anything else is a `DIE`
with the list.

### 2d. The output writers — `GAUSSIAN_FF_FIT`

One procedure per format, chosen by `output_format=`. `table`: element, n, a_1 … a_n, b_1
… b_n, c, one line per element, with a header saying the range and the model. `buster`:
**format not yet known** — the public BUSTER manual's file-formats page and FAQ say nothing
about scattering tables; it needs an example file from a BUSTER installation or the keyword
from Global Phasing. Until it arrives the writer is a stub that `DIE`s with that message.

## 3. The fit itself: what to expect

- Four Gaussians plus a constant fit the IT free-atom curves to about 0.001 e over 0–2 Å⁻¹
  (Table 6.1.1.4's own quoted accuracy). A Salvador atom in a molecule differs from the free
  atom mostly at low s (charge transfer and the bonding density), so the IT start is close
  and the fit should converge in tens of iterations.
- The b_i are strongly correlated and the problem is ill-conditioned if two exponents
  approach each other: gnuplot reports this as a singular matrix or by a huge esd. The
  guard is the start: with IT values the exponents stay apart. If a fit does fail, retry
  from the IT values of the neighbouring element, then with n − 1 Gaussians, and say so.
- A charged atom (a Salvador atom is, by about ±0.5–2 e in urea: research document §5) has
  f(0) ≠ Z; that is the point of the exercise and is carried by c and the a_i together.
  Hydrogen's c is very small in the IT form; let it float.
- Weighting: unweighted least squares on an even s grid. The protein data are concentrated
  at low s, but the fit is good enough everywhere that weighting is not needed; revisit if
  the maximum deviation sits at high s and matters.

## 4. Checks and tests

- **Free-atom check:** fit the IAM form factor of each element, generated from the IT
  coefficients themselves on the same s grid, and recover the coefficients to 1e-4 — the
  fitter and the file round trip are then known to work. A `short` ctest.
- **Urea, `sph-tfvh`:** fit C, N, O, H; compare the curves and the fitted a, b, c with the
  free-atom ones; the plot shows where the molecule differs from the free atom. A `long`
  ctest, with the coefficient table in its reference.
- Tolerance on the maximum deviation: 0.005 e over the range as a `WARN`, reported always.
- gnuplot absent: the job must finish; the test suite skips the fit tests when
  `gnuplot` is not on `PATH`, as `rgbi_doctor_selftest` skips today.

## 5. Later

- The linear even-tempered fit (`oc-ri` machinery, L = 0) as an independent check of the
  curve at the level of 1e-4 e, and as the fallback when the nonlinear fit will not converge.
- Electron scattering factors (Mott–Bethe from the same curve) for cryo-EM, same fitter.
- The BUSTER writer, when the format is known; then phenix/REFMAC/SHELXL as asked for.
- Averaging over atoms of an element across several molecules (a library of residues).

## 6. Order of work

1. `sph-tfvh` wired in (branch `sph-tfvh`, 2026-10-05) and checked on urea.
2. `GAUSSIAN_FF_FIT` with the free-atom round-trip test.
3. `make_sph_atom_FF_curve` and the driver keyword; urea test; the `table` writer.
4. The BUSTER writer, when the format is in hand.
