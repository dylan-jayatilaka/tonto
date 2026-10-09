# The X-ray constrained SCF: convergence and the choice of lambda

A working document. Opened 2026-10-07 on branch `xcw-wandering`. The findings are in
`docs/REPORT_ON_XCW_CONVERGENCE.md`; this page holds the plan, the decisions owed and the log.

## 1. State

Done on the branch, all in the Mac release build on the ammonia restart job
(`tests/long/nh3_x-ray-constrained-rhf-cluster-charge_cc-pVTZ_restart`):

- The instability explained and its size calculated (report sections 3 and 4).
- Density damping made real, in the two SCF loops only, and the fifteen references it moves
  redone on achari2 in the reference build. Branch `damping-repair` is superseded by
  `develop` and can be deleted.
- The effective number of parameters, the criteria for lambda, the leave-one-out sum and the
  three-point TIH extrapolation, printed per lambda and in the SCF results.
- The cure: the Newton step with the constraint curvature, `use_stiffness_correction= TRUE`.
  Converges in about ten iterations at every lambda tried, up to 40, with no damping and
  no DIIS; the plain SCF with damping and DIIS needed 251 iterations at 0.14 and failed above.

## 2. Plan for the cure, and what is left of it

The cure as implemented is the first of three stages.

1. **Done: the explicit form.** At every iteration the structure factor derivatives over all
   occupied-virtual rotations are rebuilt (one pass over the shell pairs, like a structure
   factor evaluation, plus a transformation costing reflections times basis size squared times
   virtuals), the gain matrix is formed and diagonalised (reflections cubed), and one extra
   constraint matrix is built. For ammonia that is cheap. For a molecule with hundreds of
   basis functions and thousands of reflections the derivative array is the problem: it is
   reflections times occupied times virtual reals, and the transformation dominates.
2. **Withdrawn: reuse across iterations.** Tried on 2026-10-08 as a rebuild period of 5, 10
   and 100 iterations, at lambda 0.4 on ammonia, where remaking every iteration converges in
   10: all three diverge. A stored $`\mathbf{B}`$ refers to the orbitals it was made in, and
   contracting it with the gradient in the current orbitals mismatches the occupied-virtual
   indices. Taking the gradient in the stored orbitals instead also diverges: in that basis
   the off-diagonal block of the current Fock matrix measures the rotation already made since
   the matrix was stored, not the error left, so the correction is large and wrong after the
   first big step. Transforming $`\mathbf{B}`$ to the current orbitals needs the occupied-occupied
   and virtual-virtual blocks of every derivative, which were never stored and would cost more
   than $`\mathbf{B}`$ itself. So the derivatives are remade every iteration, and the scaling
   route is stage 3. What can be reused is a reflection-space object: the leading eigenvectors
   of $`\mathbf{G}`$ change slowly, and can deflate the conjugate gradient solve below.
3. **Done, first form: matrix-free** (Dylan, 2026-10-08: the diagonalisation of $`\mathbf{G}`$
   is the problem for anything large; the update only ever contracts $`\mathbf{G}`$ with
   $`\mathbf{B}`$ on either side, so it need not exist). Implemented as `make_B_times`,
   `make_BT_times`, `apply_stiffness_operator`, `solve_stiffness_system` (plain conjugate
   gradients) and `make_constraint_stiffness_estimates` (Hutchinson for $`p_{\mathrm{eff}}`$
   and the leverages, power iteration for the largest gain), switched by
   `use_matrix_free_stiffness=`. Checked against the explicit route on ammonia, report
   section 9: same iterations, energies, largest gain, $`p_{\mathrm{eff}}`$ to 0.3% and GCV;
   the leverages are too rough from 20 samples, so the leave-one-out sum and Cook's distances
   are not usable in this form yet; 10 to 28 conjugate gradient iterations per SCF iteration;
   twenty times the CPU of the explicit route on a molecule this small. **Done in this stage:**
   deflation of the conjugate gradients by the leading eigenvectors of $`\mathbf{G}`$ from
   Lanczos (`stiffness_deflation=`), which on ammonia cuts the count from 10 and 28 to 3 and
   10 with 20 vectors; a diagonal preconditioner was tried first and doubles the count (an
   estimated diagonal from four sign vectors: 10 to 14 and 28 to 80; the exact diagonal, built
   in batches: 10 to 12 and 28 to 52), the matrix being far from diagonal; it is kept as
   `stiffness_preconditioner= diagonal`, off by default. **Left:** whether the Lanczos vectors
   can be carried from one lambda to the next as a start, and deflation on by default; a usable leverage estimator
   (more samples, or probing by colouring); an automatic choice between the two forms by the
   size of $`\mathbf{B}`$; a timing on a real case where $`\mathbf{B}`$ does not fit. The design
   as planned:
   - Never store the derivative array. $`\mathbf{B}\mathbf{v}`$ for an occupied-virtual vector
     $`\mathbf{v}`$ is the vector of structure factors of the symmetrised transition density
     $`\mathbf{c}_{\mathrm{occ}}\,\mathbf{v}\,\mathbf{c}_{\mathrm{vir}}^{T}`$, scaled per
     reflection by $`2\alpha_k/\sigma_k`$ and by $`1/\sqrt{\Delta_{ia}}`$ on the way in: one
     structure factor evaluation, `make_X_SFs` on a density matrix that is not the SCF one.
     $`\mathbf{B}^{T}\mathbf{w}`$ for a reflection vector $`\mathbf{w}`$ is the occupied-virtual
     block of `make_r_constraint(C, resid=w)`, scaled by $`1/\sqrt{\Delta}`$: one constraint
     build. Both exist; what is new is a routine that applies $`\mathbf{1}+\lambda\mathbf{G}`$ to a
     reflection vector by calling them in turn.
   - Solve $`(\mathbf{1}+\lambda\mathbf{G})\mathbf{y} = \delta\mathbf{r}`$ by preconditioned
     conjugate gradients with the diagonal $`1+\lambda G_{kk}`$, where
     $`G_{kk} = \mu\sum_{ia}B_{k,ia}^2`$ comes from one pass over the shell pairs that squares
     each reflection's derivative row as it is made, without storing it. The solve tolerance
     can follow the DIIS error: the correction need not be more accurate than the step.
   - $`p_{\mathrm{eff}} = \mathrm{tr}\,\mathbf{H}`$ by Hutchinson's estimator,
     $`\frac{1}{m}\sum_j \mathbf{z}_j^{T}\mathbf{H}\mathbf{z}_j`$ with random $`\pm 1`$ vectors
     $`\mathbf{z}_j`$ and $`\mathbf{H}\mathbf{z} = \mathbf{z} - (\mathbf{1}+\lambda\mathbf{G})^{-1}\mathbf{z}`$,
     one solve each; a few tens give a few percent, which is all $`p_{\mathrm{eff}}`$ needs.
     The leverages $`H_{kk}`$ by the diagonal estimator of Bekas, Kokiopoulou and Saad (2007),
     the same solves averaged component-wise. The alternative is the Lanczos quadrature of
     Golub and Meurant, *Matrices, Moments and Quadrature with Applications* (2010), which is
     in the manuscripts folder: it gives $`\sum_j f(\gamma_j)`$ for any $`f`$, here
     $`f(\gamma) = \lambda\gamma/(1+\lambda\gamma)`$, from the same products, with error bounds.
     A Davidson solver for the largest few eigenvalues is not enough on its own, since at
     lambda 2 on ammonia 34 of 88 directions contribute and the tail matters; it would serve
     for $`\gamma_{\max}`$ and the damping limit of equation (6).
   - Keep the explicit $`\mathbf{G}`$ and its eigenproblem for the full stiffness report when
     the reflection count is a few thousand at most, where it costs seconds.
   - A power series in $`\lambda\mathbf{G}`$ for the inverse is not an option: it diverges
     where $`\lambda\gamma_{\max} > 1`$, which is exactly where the cure is needed.
   This is the form for large molecules and for MPI, where the two products parallelise as
   the structure factors and the constraint build already do.
4. **Next: the products on the Hirshfeld-atom route** (Dylan, 2026-10-09: cases with
   $`10^5`$ reflections). With the two-centre partition one structure factor evaluation costs
   shell pairs times unique scattering vectors, which at $`10^5`$ reflections and 1000 functions
   is hours per product on one core; the plain XCW needs two per iteration and the correction
   ten to twenty more, so that partition is out at that size whatever the solver. The
   Hirshfeld-atom partition puts the density on a grid once, grid points times basis size
   squared, and makes atom-centred transforms, reflections times atoms times the atomic grid,
   with no pair sum; HAR already runs it at $`10^5`$ reflections. The two products exist on
   that route: $`\mathbf{B}\mathbf{v}`$ is the Hirshfeld-atom structure factors of a transition
   density, and $`\mathbf{B}^{T}\mathbf{w}`$ the occupied-virtual block of
   `make_H_r_constraint` built from a reflection vector. With them the matrix-free correction,
   Lanczos deflation and the trace estimator carry over unchanged. Also the HA-XCW of
   Davidson, Grabowsky and Jayatilaka (2022), so the version that matters.
   **Better still, the fitted Hirshfeld atoms, `partition_model= oc-ri`** (Dylan, 2026-10-09).
   The fit is linear in the density with a fixed metric, so every structure factor is a sum
   over auxiliary functions of analytic transforms, and the reflection count drops out of the
   grid term: $`\mathbf{B}\mathbf{v}`$ is one grid pass to partition and fit the transition
   density, then reflections times auxiliary functions; $`\mathbf{B}^{T}\mathbf{w}`$ contracts the
   reflection vector into the auxiliary space first, reflections times auxiliary functions,
   then one grid pass that builds the constraint matrix like an exchange-correlation matrix.
   At $`10^5`$ reflections and 5000 auxiliary functions that is $`5 \times 10^{8}`$ against
   $`10^{11}`$ per atom for the numerical transforms. The fitting error is irrelevant to the
   correction, a preconditioner, and consistent for the statistics when the XCW itself uses
   the fitted model. This is the form to write: the two products on the `oc-ri` route,
   checked on urea against the two-centre results.

Also left, in this order (Dylan, 2026-10-09: the fitted Hirshfeld atoms first, the line
search after):

- **Stage 4 above**, the products on the `oc-ri` route; with it the Hirshfeld-atom partition
  models generally. Unrestricted wavefunctions after that.
- A test job with the correction on, blessed on achari2, once the damping references are redone.
- The coupled orbital response in the gain matrix, if the 8% error on the leave-one-out
  residuals matters for anything.
- Whether the correction should be the default for constrained SCF. It changes no converged
  result, only the path, so it moves every constrained reference.
- A debug build, MPI, and anything on achari2.
- **The drift at large lambda** (report section 9): on urea the iteration can visit a higher
  stationary point of $`E + \lambda\,\mathrm{GoF}^2`$ and sit there, in STO-3G from lambda
  0.035 and in def2-SVP near 0.9, where lambda times the largest gain is about 2000 and the
  count of iterations has already grown. A line search on the functional along the corrected
  step is the candidate safeguard; it comes after stage 4, since it does not change where
  the method can be used, only how safely.
- Whether the correction and `stiffness_deflation=` should be on by default for constrained
  SCF; the correction is restricted wavefunctions and two-centre partition models only.

## 3. Decisions owed

- **Which criterion chooses lambda: decided** (Dylan, 2026-10-08). The sigmas are unreliable in
  scale but useful relatively, so GCV or the sigma-free AIC; GCV is the default for its
  crystallographic heritage, `lambda_criterion=` switches. Still open: whether the scan should
  stop itself at the criterion's minimum, and a data set with believable sigmas to see the
  two agree.
- **Whether `use_stiffness_correction` and deflation become the defaults** for constrained
  SCF. Not blocking; best decided once the fitted-atom form exists, since that is the form
  people will use. It changes no converged result, only the path, but moves every
  constrained reference.

## 4. Where the runs are

`~/tonto_runs/xcw_2026-10-09/`, with a README: the ammonia scans and single-lambda jobs, the
held-out refits, the urea scans in STO-3G, def2-SVP and def2-TZVP, and the urea controls.
Each directory has its `stdin` and `stdout`, the per-lambda `urea,lambda=*.ffn` files, and
the wavefunction at every lambda: `urea.MOs,lambda=*,r`, `urea.MO_energies,lambda=*,r` and
`urea.density_mx,lambda=*,r`. Deformation densities can be made from these without reruns.

To continue a scan from a stored lambda, copy that lambda's `MOs` and `MO_energies` files to
`urea.MOs,r` and `urea.MO_energies,r` and use `initial_mos= r`. Not `initial_density= r`: that
reads the density and then diagonalises the Fock matrix built from it without the constraint,
which undoes most of the fit (def2-TZVP, lambda 0.06: GoF back to 4.26 instead of 1.54).

## 5. Log

- 2026-10-07. Reproduced on the Mac release build. Found damping dead (water, two damping
  factors, identical tables; then markers in `MOLECULE.BASE:make_SCF_density_mx` showing the
  old density never allocated). Commit `9156e72e` of 2026-09-13 had kept the old density
  between calls, which works only while the incremental build is on. Repaired. Damping-only
  scan and the stiffness routine; 23.0% predicted, 23% converges and 24% diverges. Held-out
  checks of the leave-one-out formula on two reflections: 7 to 8% low. Short and long suites
  with the repair: 117 of 130; the twelve changed tests looked at one by one.
- 2026-10-08. The effective number of parameters, AIC, BIC, GCV, leave-one-out and the TIH
  three-point formula; the keyword moved into `scfdata=`; the statistics added to the SCF
  results block. Read Davidson, Grabowsky and Jayatilaka (2022) part II and Parsons *et al.*
  (2012); the relations are report section 8.
- 2026-10-08. The cure. A first version inferred the fixed point from a probe diagonalisation
  and settled on wrong fixed points at every lambda: far from convergence the plain step is
  nothing like linear. Rewritten as the Newton step with the constraint curvature, applied
  through shifted residuals in the constraint build, with the gain matrix remade every
  iteration. The first version also missed the initial Fock matrix, which is built before the
  loop; fixed. Results, Mac release build, ammonia restart job, DIIS off, damping off:

  | lambda | iterations with the correction | plain SCF, damping and DIIS |
  |---|---|---|
  | 0.012 | 9 | 17 |
  | 0.14 | 11 | 251 |
  | 0.20 | 11 | not converged in 300 |
  | 0.40 | 10 | blows up |

  With damping at 15% for three iterations and DIIS on top, 15 to 17: the damping only slows
  it. Same converged energies and GoF as the plain SCF where that converges.
- 2026-10-08. Lambda scans with the correction to 0.4, then to 4, every point converged. The
  earlier statement that the leave-one-out minimum is near 0.15 came from the unconverged
  points at 0.14 and 0.16 and was wrong; the scan results are in the report. A scan started
  at lambda 4 straight from the lambda 0.012 density diverged: the step is a linearisation,
  and lambda has to be stepped up.
- 2026-10-09. The def2-TZVP scan: steps of 0.05 from lambda 0 fail from the first step
  (kept as `urea_tzvp_step0.05_failed`); steps of 0.005 converge at every point to 0.06, 5 to
  10 iterations, GoF 4.11 to 1.54, 18 minutes. The continuation to 0.5 first started from the
  promolecule, then from `initial_density= r`, both wrong (kept as
  `urea_tzvp2_from_promolecule_failed`); now from `initial_mos= r`, starting at GoF 1.54 with
  zero gradient. The def2-SVP continuations from 0.06 and 0.5 had also started from the
  promolecule.
- 2026-10-09. Stage 4 begun: the matrix-free products on the Hirshfeld route, checked by
  w.(B v) = (B^T w).v, exact on oc-hirshfeld for vectors with the crystal's site symmetry,
  0.02 to 2% off on oc-ri because the existing XCW builds its constraint from unfitted grid
  sums. The correction then overshot because its extra constraint matrix was always built by
  the two-centre routine; fixed, after which oc-hirshfeld and oc-ri follow the two-centre
  route iteration by iteration. Each Hirshfeld iteration costs several grid passes: slow.
- 2026-10-09. Scaling to $`10^5`$ reflections thought through at Dylan's request: the
  structure factor evaluation of the two-centre partition is the wall, not the gain matrix;
  stage 4 above.
- 2026-10-09. The def2-TZVP urea scan, 0 to 1 in steps of 0.05, launched in the runs folder.
- 2026-10-09. Urea with def2-SVP, 80 basis functions, GoF 4.43 before fitting, largest gain
  1730: every lambda from 0 to 0.8 converges with the correction alone, 5 to 12 iterations,
  GoF down to 1.56, p_eff 63 of 817, every criterion still falling. At 0.9 it fails as the
  minimal basis did at 0.035, after a step of 0.1; a jump straight from the promolecule to
  0.8 fails too, where a jump to 0.5 does not. With steps of 0.05: 0.55 to 0.85 converge in 8
  to 36 iterations, 0.9 sits 60 iterations near a GoF 18.9 state, 0.95 and 1.0 converge again
  from there, 55 and 9 iterations, GoF 1.52, p_eff 66, criteria still falling. So the
  wandering is a transient visit to a higher stationary point, not a limit on lambda, and a
  line search on the functional is the safeguard to write. Report section 9.
- 2026-10-08. Urea, STO-3G, 817 reflections, at Dylan's request: scans in steps of 0.01 and
  0.005, segments from 0.030, with the level shift off, on throughout, at 1, 3 and 10, with
  DIIS off, and with damping of 50% and 70% throughout; the plain SCF as control. Results in
  the report. On the way: the level shift sits in the virtual orbital energies, so the
  statistics used shifted gaps while it was on; `orbital_gap` now takes it off for the
  statistics at convergence and keeps it for the correction.
- 2026-10-08. Deflation by Lanczos vectors, numbers in the report; the diagonal preconditioner
  before it, both tried on the matrix-free ammonia jobs at lambda 0.012 and 0.4.
- 2026-10-08. Stage 3, the matrix-free form, implemented and checked against the explicit
  route on ammonia at lambda 0.012 and 0.4; numbers in the report. A first build failed on an
  integer constant too large for the default kind in the sign generator; Lehmer's generator
  with modulus 65537 instead.
- 2026-10-08. Stage 2 tried and withdrawn, see the plan. The keyword, the stored orbitals and
  the period logic were removed again.
- 2026-10-08. `lambda_criterion=` keyword, GCV default; the chosen criterion's value and the
  lambda of its smallest value so far are printed with the statistics.
- 2026-10-08. Third run on achari2 after the blessing, every suite: 181 of 181.
- 2026-10-08. References on achari2. The first run of the suites there, with the damping
  repair as first written, failed 22 of 181 tests, among them the formamide interaction
  energies: the repair had damped every density made while the iteration count was small,
  including the orthogonalised promolecule of the energy decomposition, which was mixed with
  the density in memory. Damping is now applied only where the two SCF loops ask for it, and
  the iteration-0 density is no longer damped. Second run: 15 failures, all iteration tables
  or esd-level HAR and hart numbers (up to 0.6%), blessed. The show_labels failure was three
  heading underlines of the wrong length, fixed. Cook's distance added to the stiffness
  report at Dylan's request. Third run launched to confirm 181 of 181.
