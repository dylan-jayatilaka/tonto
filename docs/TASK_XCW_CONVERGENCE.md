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

Also left:

- Unrestricted wavefunctions and the Hirshfeld-atom partition models, in both the stiffness
  routine and the correction. The Hirshfeld-atom constraint has its own derivative routine.
- A test job with the correction on, blessed on achari2, once the damping references are redone.
- The coupled orbital response in the gain matrix, if the 8% error on the leave-one-out
  residuals matters for anything.
- Whether the correction should be the default for constrained SCF. It changes no converged
  result, only the path, so it moves every constrained reference.
- A debug build, MPI, and anything on achari2.
- **Urea beyond lambda 0.03 in STO-3G, and beyond about 0.85 in def2-SVP** (report section
  9): more than one stationary point of $`E + \lambda\,\mathrm{GoF}^2`$, the first reached not
  the minimum, and no scheme converging. In def2-SVP the limit sits at lambda times the
  largest gain of about 2000, where ammonia also needs lambda stepped rather than jumped.
  Needs the landscape understood, not more solver changes: the omitted residual term of the
  Hessian and the two-electron response. A line search on $`E + \lambda\,\mathrm{GoF}^2`$ along
  the corrected step would at least stop the drift to a higher stationary point.
- Whether the correction and `stiffness_deflation=` should be on by default for constrained
  SCF; the correction is restricted wavefunctions and two-centre partition models only.

## 3. Decisions owed

- **Which criterion chooses lambda: decided** (Dylan, 2026-10-08). The sigmas are unreliable in
  scale but useful relatively, so GCV or the sigma-free AIC; GCV is the default for its
  crystallographic heritage, `lambda_criterion=` switches. Still open: whether the scan should
  stop itself at the criterion's minimum, and a data set with believable sigmas to see the
  two agree.
- **Whether `use_stiffness_correction` becomes the default** for constrained SCF.

## 4. Log

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
