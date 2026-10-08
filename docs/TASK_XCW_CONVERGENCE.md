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
2. **Next: reuse across iterations.** The gain matrix changes slowly once the orbitals settle.
   Build it at the first iteration of each lambda and again only when the DIIS error has
   fallen by a factor of ten, or every fifth iteration. The gradient contraction each
   iteration then costs reflections times occupied times virtual, which is small.
3. **Later: matrix-free.** Never store the derivatives. The two products needed, $`B\tilde g`$
   and $`B^{T}v`$, are a structure factor derivative contracted with a rotation and a constraint
   matrix built from a residual vector, both of which the code already does; solve
   $`(1+\lambda G)y = \delta r`$ by conjugate gradients, which needs a handful of those pairs.
   This is the form for large molecules and for MPI.

Also left:

- Unrestricted wavefunctions and the Hirshfeld-atom partition models, in both the stiffness
  routine and the correction. The Hirshfeld-atom constraint has its own derivative routine.
- A test job with the correction on, blessed on achari2, once the damping references are redone.
- The coupled orbital response in the gain matrix, if the 8% error on the leave-one-out
  residuals matters for anything.
- Whether the correction should be the default for constrained SCF. It changes no converged
  result, only the path, so it moves every constrained reference.
- A debug build, MPI, and anything on achari2.

## 3. Decisions owed

- **Which criterion chooses lambda.** See the report, section 9, for where each criterion puts
  its minimum on ammonia. The sigmas of that data set look too large, which is why the
  criteria that trust them disagree with the sigma-free ones. A data set with believable sigmas
  is needed before a rule is set.
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
- 2026-10-08. References on achari2. The first run of the suites there, with the damping
  repair as first written, failed 22 of 181 tests, among them the formamide interaction
  energies: the repair had damped every density made while the iteration count was small,
  including the orthogonalised promolecule of the energy decomposition, which was mixed with
  the density in memory. Damping is now applied only where the two SCF loops ask for it, and
  the iteration-0 density is no longer damped. Second run: 15 failures, all iteration tables
  or esd-level HAR and hart numbers (up to 0.6%), blessed. The show_labels failure was three
  heading underlines of the wrong length, fixed. Cook's distance added to the stiffness
  report at Dylan's request. Third run launched to confirm 181 of 181.
