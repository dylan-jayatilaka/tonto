# Spherically averaged atom models, their form factors, and the fits behind them

A protein refinement wants one form factor for each kind of atom, evaluated at millions of
reflections. Tonto makes such a form factor from a spherically averaged atom taken out of the
molecular density, fits it to the few Gaussians every refinement program reads, and writes
the result in the program's format. This page says what the models are, how the form factor
and the fit are made, what each output format looks like, where the least-squares machinery
behind the structure refinement lives, and what is still open. How the spherical models fare
in a refinement against the aspherical ones is in
[Variants of the Salvador atom model](REPORT_ON_SALVADOR_MODELS.md).

## 1. The models

A spherically averaged atom model (SAM) is an aspherical atom of the molecule, averaged over
angle about its nucleus. The aspherical atoms available are the Salvador atom (a topological
fuzzy Voronoi cell atom) in three variants that differ in where the boundary between two bonded
atoms is put, and the exponential Hirshfeld atom of Chodkiewicz and Woźniak. Their spherical
averages are chosen with `partition_model=`:

| `partition_model=` | the atom averaged |
|---|---|
| `tfvc-l0` | Salvador atom, boundaries from the atomic radii |
| `tfvc-p-l0` | Salvador atom, boundaries from the promolecule density |
| `tfvc-h-l0` | Salvador atom, boundaries where the two atoms' densities are equal |
| `sph-exphar` | exponential Hirshfeld atom |

The `oc-` prefix (`oc-tfvc-h-l0` and so on) makes the atom from the density of the molecule in
its crystal environment, with cluster charges, as the Hirshfeld atoms of a refinement are.

The average is a sum over the atom's radial grid: `MOLECULE.RHO:make_sph_avgd_SA_ED_grid`
gives the averaged density on the radial points, and its transform is the sum of
`FOURIER_SUMS:sinc_kr_sums`. Two older spherical models are iterated rather than taken once:
the spherically averaged Hirshfeld atoms of `make_spherically_averaged_HAs` (the iterative
stockholder atoms of Lillestolen and Wheatley) and the bonded density atoms of
`make_spherically_averaged_BDAs`; both are kept as atomic density tables (`INTERPOLATOR`) and
refined by spherical averaging over Lebedev shells until the tables stop changing.

## 2. The form factor

The form factor of the averaged atom is a function of $`s = \sin\theta/\lambda`$ alone,

```math
f(s) = 4\pi \int_0^\infty \rho(r)\,\frac{\sin(4\pi s r)}{4\pi s r}\, r^2\, dr , \qquad (1)
```

with $`\rho(r)`$ the averaged density. `MOLECULE.RHO:make_sph_atom_FF_curve` evaluates it at
`FF_fit_n_points` evenly spaced points from 0 to `FF_fit_s_max` (in 1/Å), for the model in
force; $`f(0)`$ is the atom's electron count.

## 3. The fit to Gaussians

The curve is fitted to the form of the International Tables, Vol. C, Table 6.1.1.4,

```math
f(s) \approx \sum_{i=1}^{n} a_i\, e^{-b_i s^2} + c , \qquad (2)
```

with $`n = 4`$, nine numbers for an atom type. The fit is nonlinear in the $`b_i`$ and made in
the square roots of all nine, so every coefficient comes back positive as the Tables' are. The
start is the International Tables entry of the element (`ATOM:IT_form_factor_coeffs`). The
minimiser is gnuplot's `fit` (Levenberg–Marquardt): `GAUSSIAN_FF_FIT:fit` writes the data and a
script, runs gnuplot through `SYSTEM_COMMAND`, and reads the coefficients and their esds back
from a file the script prints. A fit that stops on a singular matrix, which happens when two
Gaussians fall together, is retried from the same start with the exponents scaled by 0.7, 1.4,
0.5 and 2. The same script draws $`f(s)`$ minus the fit. For atom `X` of job `name` the files are
`name.X.sph_ff.dat`, `.gnuplot`, `.fit.log`, `.coeffs` and `.png`.

The rms and the largest deviation of the fit over the range are printed for every atom
(`GAUSSIAN_FF_FIT:deviations`). Four Gaussians reach 0.005 e on hydrogen and oxygen and about
0.02 e on carbon and nitrogen: the residual is a slow ripple along $`s`$ from the hard edge of
the Salvador cell, which no sum of positive Gaussians follows; a softer edge
(`sph-exphar`) halves it. The detail is in `docs/TASK_SPHERICAL_FF_FIT.md`, section 8.

A round trip on the carbon International Tables curve, from a start perturbed by 10%, recovers
all nine coefficients to $`10^{-4}`$: `GAUSSIAN_FF_FIT:check_round_trip`, the ctest
`form_factor_gaussian_fit_round_trip`, skipped where gnuplot is not installed.

## 4. The keyword, and the output formats

```
crystal= {
   xray_data= {
      partition_model= oc-tfvc-h-l0
      FF_fit_s_max=       2.0      ! 1/Angstrom; the fit range is [0, s_max]
      FF_fit_n_gaussians= 4        ! only 4 so far: the start is the International Tables entry
      FF_fit_n_points=    201
      FF_fit_format=      shelx    ! shelx | table | buster
   }
}
...
scf
HAR_refinement
fit_sph_atom_ffs
```

`fit_sph_atom_ffs` fits every unique atom of the fragment (atoms related by symmetry share a
curve) and writes the coefficients to `name.sph_ff.<format>`, one entry for each atom. The
coefficients themselves are not compared by the tests, only the fit's quality: a degenerate pair
of Gaussians can split differently from one machine to the next.

| format | what is written | writer |
|---|---|---|
| `table` | one line for each atom: label, $`n`$, $`a_1 \ldots a_n`$, $`b_1 \ldots b_n`$, $`c`$, after a comment with the fit's quality and esds | `GAUSSIAN_FF_FIT:put_table_line` |
| `shelx` | one `SFAC` instruction for each atom, `SFAC label a1 b1 a2 b2 a3 b3 a4 b4 c f' f" mu r wt` with $`f'`$, $`f''`$, $`\mu`$, $`r`$ and the weight written as 0 for the user to fill in; a `REM` line before it carries the quality and esds | `GAUSSIAN_FF_FIT:put_SHELX_SFAC` |
| `buster` | two files. `name.sph_ff.buster`: `FORMFACTOR` lines in TNT format, `FORMFACTOR type a1 b1 a2 b2 a3 b3 a4 b4 c`, the pair with the largest $`b`$ first, for the elements present (from the International Tables) and then for each unique atom under its own type; and `name.sph_ff.pdb`: the cell (`CRYST1`, `SCALE`) and every atom as a `HETATM` record whose element-and-charge field is extended to column 82 with the type of its unique atom, as `C+0.1` | `GAUSSIAN_FF_FIT:put_BUSTER_FORMFACTOR`, `BUSTER_type_label`, `PDB_atom_record` |

**BUSTER.** The format rides on BUSTER's partial-charge feature: an atom gets its own form
factor by having its own type, the element symbol followed by a sign, a digit, a point and a
digit in columns 79–82. Tonto gives the types out in the order `+0.1`, `-0.1`, `+0.2`,
`-0.2`, … within each element, 198 at most, and stops with a message if an element has more
unique atoms than that; the label is a label, not a charge. Atoms of one unique kind share a
type. In the PDB record the residue name is the atom's `compound_id` or `UNL`, the chain its
`asym_id` or `A`, the residue number its `compound_sequence_id` or 1, $`B = 8\pi^2 U_{eq}`$ from the
ADP tensor's trace or from $`U_{iso}`$, and the coordinates are Cartesian in Tonto's frame
(x along a, y along c* × a, z along c*), which is the PDB frame. BUSTER is told of the file with
`refine FormfactorDat=/path/to/name.sph_ff.buster`. The atom-to-type table is printed in the
output. The exponent convention, $`b`$ multiplying $`s^2`$ in Å², is that of the International
Tables and of the carbon line Global Phasing supplied; it has not been confirmed with them.

**SHELXL.** `SFAC` with the nine coefficients is SHELXL's own scattering-factor instruction; the
atom's label is used as the type, since every atom has its own curve.

**PHENIX.** Not written. phenix.refine takes its scattering factors from cctbx's tables, which
hold the same $`n`$-Gaussian form; how a custom table is supplied to a refinement has to be
settled before a writer is worth writing.

## 5. The least squares behind a refinement

A structure refinement is a least-squares fit of the structure factors, and the form-factor fit
above is a least-squares fit of a curve; the first has its own solver in Tonto, the second
borrows gnuplot's. The refinement's solver is in three layers.

`LEAST_SQUARES` holds the refined parameters $`p`$ of a fit. The model vector $`X`$ (for a
structure refinement, the positions and ADPs of the atoms) is made from them by

```math
X = X_{\mathrm{offset}} + J\, p , \qquad (3)
```

with $`J = \partial X/\partial p`$ the Jacobian: one parameter for each free component is
$`J = 1`$; an isotropic ADP, a site-symmetric atom or a rigid-body model is a different $`J`$,
and the solver does not care which. `DIFFRACTION_DATA.SET:solve_normal_equations` makes the
normal matrix and right-hand side from the derivatives $`\partial F/\partial X`$ of the
structure factors, the weights $`w = 1/\sigma^2`$ and the residuals $`r`$,

```math
A = (D J)^T w\, (D J), \qquad b = (D J)^T w\, r, \qquad D = \partial F/\partial X , \qquad (4)
```

and hands them to `LEAST_SQUARES:solve_normal_equations`, which keeps the shift, the covariance
$`A^{-1}`$ (or $`(A+W)^{-1}`$ with restraints), the effective number of parameters, and what the
near-zero filter found. The solve itself is `MAT{REAL}:solve_ill_linear_equations_v1`: the
normal matrix is diagonalised, eigenvalues below `near_0_tol` or negative are left out, and the
pseudo-inverse is built from the rest; the near-zero eigenvectors and their inverse participation
ratios say which parameters the data fail to separate. A restrained fit,
$`\chi^2 + (p - p_0)^T W (p - p_0)`$, goes through `solve_restrained_linear_equations` to the
same routine.

The diagonalisation is `MAT{REAL}:diagonalize_QR`: Householder tridiagonalisation followed by
the implicit symmetric QR algorithm with the Wilkinson shift, Golub and van Loan's Algorithms
8.3.1 to 8.3.3, with the Householder vector of their Algorithm 5.1.1 and the Givens rotation of
5.1.3. It is pure, so it can be called where LAPACK's `dsyev` cannot. On a normal matrix of 322
parameters it takes about a seventh of the time of the Jacobi method and a third more than
netlib's `dsyev`. Each eigenvalue is accurate to about machine epsilon times the largest, as
with `dsyev`; the Jacobi method keeps relative accuracy for the small eigenvalues of a matrix
whose eigenvalues span many orders of magnitude, and `diagonalize_Jacobi` is still what the
3 × 3 ADP and quadrupole tensors use. The known-answer check is the ctest
`symmetric_eigensolver_known_answer` (`MAT{REAL}:check_QR_eigensolver`, program
`run_eigensolver`), which also prints the time of the three solvers on a 322 × 322 normal
matrix.

## 6. Where it is implemented

| piece | procedure |
|---|---|
| the averaged density of a `sph-` atom on its radial grid | `MOLECULE.RHO:make_sph_avgd_SA_ED_grid` |
| the transform (1) on an $`s`$ grid | `MOLECULE.RHO:make_sph_atom_FF_curve`, `FOURIER_SUMS:sinc_kr_sums` |
| the iterated spherical Hirshfeld and bonded density atoms | `MOLECULE.HAR:make_SAHA`, `make_spherically_averaged_HA`, `make_averaged_BDAs` |
| the International Tables start | `ATOM:IT_form_factor_coeffs` |
| the fit (2) through gnuplot, and its files | `GAUSSIAN_FF_FIT:fit`, `put_data_file`, `put_script` |
| the fitted form and its deviations | `GAUSSIAN_FF_FIT:gaussian_sum`, `deviations` |
| the driver and the keywords | `MOLECULE.MAIN:fit_sph_atom_FFs`; `DIFFRACTION_DATA.READ:read_FF_fit_*` |
| the three formats | `GAUSSIAN_FF_FIT:put_table_line`, `put_SHELX_SFAC`, `put_BUSTER_FORMFACTOR`, `BUSTER_type_label`, `PDB_atom_record` |
| the PDB header | `CRYSTAL:put_pdb` |
| the refined parameters and the Jacobian (3) | `LEAST_SQUARES:set_jacobian`, `set_parameters_from`, `make_X_shift`, `make_X_covariance` |
| the normal equations (4) | `DIFFRACTION_DATA.SET:solve_normal_equations` |
| the solve, the filter and the covariance | `LEAST_SQUARES:solve_normal_equations`, `MAT{REAL}:solve_ill_linear_equations_v1`, `solve_restrained_linear_equations` |
| the eigensolver | `MAT{REAL}:diagonalize_QR`, `tridiagonalize`, `diagonalize_tridiagonal_QR`, `Givens_rotation`; `VEC{REAL}:Householder_vector` |
| the checks | `MAT{REAL}:check_QR_eigensolver`, `check_restrained_solve`, `GAUSSIAN_FF_FIT:check_round_trip` |

Tests: `long/urea_rhf_STO-3G_sph-TFVH_FF_fit` (SHELX) and `…_FF_fit_buster` (BUSTER, which
compares the PDB file as well as the output).

## 7. What is not done

- **The fit through gnuplot.** gnuplot must be installed, the data go through the disk, and
  the fit is a black box with its own stopping rule. Tonto has the normal-equation solver of
  section 5; a Levenberg–Marquardt fit of (2) needs only the derivatives of $`f`$ with respect
  to $`a_i`$, $`b_i`$ and $`c`$ and a damping on the diagonal, and would make the fit pure and
  testable. It would also take the two-stage fit of the task document: non-negative least
  squares over a fixed logarithmic grid of exponents to find how many Gaussians the curve
  needs and where, then the nonlinear polish.
- **The residual.** 0.02 e on carbon and nitrogen comes from the atom's edge, not from the
  fitting; a softer edge is a model question, settled with the refinement results.
- **One curve for each element.** The fit is one for each unique atom; a protein wants one for
  each element or residue type, averaged over the atoms of that kind, and across several
  molecules.
- **More Gaussians.** Only four, because the start is the International Tables entry.
- **PHENIX**, and the exponent convention of BUSTER to confirm with Global Phasing.
- **The near-zero filter and graded matrices.** The eigensolver's absolute accuracy is set by
  the largest eigenvalue; a normal matrix mixes positions, ADPs and a scale factor, so its
  eigenvalues span orders of magnitude. Scaling the matrix by its diagonal before the filter
  would make the criterion unit-free and the small eigenvalues more accurate, at the price of
  changing the effective number of parameters and the esds; a decision, not yet taken.

## References

- *International Tables for Crystallography*, Vol. C, Table 6.1.1.4: the Gaussian form (2) and
  the coefficients used as the start.
- G. H. Golub and C. F. Van Loan, *Matrix Computations*, 3rd ed. (1996), sections 5.1 and 8.3.
- T. C. Lillestolen and R. J. Wheatley, *Chem. Commun.* (2008) 5909: the iterative stockholder
  atoms.
- The TNT `FORMFACTOR` record: <https://www.uoxray.uoregon.edu/tnt/manual/node23.html#320>;
  BUSTER's partial charges: <https://www.globalphasing.com/buster/ReleaseNotes/ReleaseNotes-BUSTER_snapshot_20240710.txt>.
- [Variants of the Salvador atom model](REPORT_ON_SALVADOR_MODELS.md): the models in
  refinement.
