# The extinction correction: analysis and reactivation plan

**Opened 2026-08-22.** The secondary-extinction correction in `DIFFRACTION_DATA` has not
been exercised by any test since **2016-10-02**, when commit `a78f2955` turned
`optimise_extinction=` from `true` to `FALSE` in the two X-ray-constrained-wavefunction
jobs that used it. It has compiled ever since, and been reached by nothing.

**Verdict: it can be reactivated.** The amplitude (F) path is structurally intact —
including, unexpectedly, the derivative of `F_pred` through the extinction factor, which
the least squares needs and which is correct. What has to be repaired first is a set of
eight defects, every one of them silent: a gradient with the wrong sign for half the
reflections of a centrosymmetric structure, an intensity path that returns uninitialised
memory, and a parameter count that stops updating part way through an XWR run.

**A large part of the work already exists.** Lorraine Malaspina implemented the SHELXL
form of the correction, the whole intensity path, and an esd for the extinction
parameter on `origin/Lolo_CP2K` on 2026-08-17. See §2.

A related but separate task was agreed at the same time and is written up on its own, in
`docs/GOF_NOT_CHI2.md`: the quantity the code calls `chi2` is a GoF², and the refinement
tables should report GoF rather than its square. It is kept apart from this work
deliberately — the rename is broad and reaches files that have nothing to do with
diffraction. Only its table change needs references reblessed, and that can share this
work's reblessing pass if the two happen to land together.

## 1. How it works

Extinction is **not** a member of the position/ADP (pADP) parameter vector. It is
optimised separately, together with the scale factor, every time the structure factors
are remade:

```
CRYSTAL:make_F_predicted_from            crystal.foo:4001
  DIFFRACTION_DATA.SET:make_F_predicted  diffraction_data.set.foo:1804
    .update_n_param                                          :1835
    .get_F_optimum_parameters                                :1838
      .optimize_F_extinction_factor                          :1286
        .optimize_F_scale_factor            (starting guess) :1114
        VEC{REAL}:min_BFGS(chi2F, d_chi2F, p)  vec{real}.foo:2048
    F_pred = |F_calc| * .F_scale_and_extn_correction          :1841
```

`p(1)` is the scale factor, `p(2)` the extinction factor. The model is Larson's, applied
to the amplitude:

```
y(n) = [1 + eps * |F_calc(n)|^2 * A(theta_n)] ^ (-1/4)
A    = (1 + cos^2 2theta) / (1 + cos 2theta sin 2theta)     diffraction_data.inq.foo:508
```

`VEC{REAL}:min_BFGS` is hard-wired to take a `DIFFRACTION_DATA` as its argument, and this
is its only caller anywhere in the library. Reactivating extinction reactivates that
routine too.

### What is already right

- **`d_F_pred_dX` differentiates through the extinction factor.** `inq.foo:817` forms
  `dX = 1 - (1/2) y^4 (eps A) |F_calc|^2`, which is exactly `d(|F| y)/d|F| / y`. It also
  carries the derivative of the analytic scale factor. `eps` is held constant with
  respect to the pADPs, which is the correct first-order treatment for a parameter
  minimised separately at every cycle; the header says so.
- **The XCW gradient carries it.** `MOLECULE.SCF` at `:2343`, `:2441` and `:2571` builds
  the constraint contribution with `alpha = .crystal.xray_data.F_scale_and_extn_correction`,
  so extinction enters the constrained Fock matrix, under the same constant-`alpha`
  approximation.
- **Residual maps divide it out.** `make_phased_dF_a`/`_b` and
  `make_symop_generated_dF_a_v2` divide by the combined `s*y`.
- **`hart` already exposes it.** `--extinction t` reaches
  `set_refine_extinction` at `run_har.foo:848`. The input keywords
  `optimise_extinction=` / `optimize_extinction=` / `refine_extinction=` all reach
  `read_refine_extinction`.
- **A sane guard exists.** With no experimental data, `set.foo:700` turns both
  `refine_scale` and `refine_extinction` off.

### Evidence it once ran

`tests/nh3_x-ray-constrained-rhf_cc-pVTZ/stdout` at `554235e9` (the Tonto 3.2 import)
prints

```
Using extinction .................. T
Optimize extinction ............... T
Secondary extinction factor ....... 0.0001
```

so the F path produced a finite, plausible number. Seven test jobs carried
`optimise_extinction= true` at that commit; all were switched off between 2016 and 2019,
in commits whose messages address other subjects.

### No test covers it today

Two jobs still say `optimise_extinction= true` —
`tests/short/nh2cn_b3lyp_cc-pVTZ_g94_fchk_to_SF_stl_limit` and
`tests/long/nh2cn_b3lyp_cc-pVTZ_g94_fchk_to_structure_factors` — but neither reads
experimental data. `make_F_predicted` therefore takes its `NOT .have_F_exp` branch and
the extinction code is never entered. **The setting in those two jobs is inert, and the
suite's coverage of extinction is zero.**

## 2. Prior work on `origin/Lolo_CP2K`

| Commit | Date | Contents |
|---|---|---|
| `58f4a23d` | 2026-08-17 | "corrected extinction shelx style". Replaces `extinction_angle_part` with SHELXL eq (62); fixes the `Re(F^3)` gradient bug; implements both intensity correction routines; implements `d_chi2I_d_ext`; corrects `d_chi2I_d_scale`; writes `_refine_ls_extinction_coef` to the CIF. |
| `3e29ca20` | 2026-08-17 | "added exti error". Adds `F_extinction_factor_esd` / `I_extinction_factor_esd` — the inverse 2×2 Gauss-Newton normal matrix for (scale, extinction), scaled by the reduced GoF² — a new `extinction_factor_esd` component, and its output in the CIF and the statistics blocks. |

The mathematics in both was checked line by line and is correct: the intensity
correction is the square of the amplitude correction, `d_chi2I_d_ext` matches
`dy_I/deps = -(1/2) y_I^3 I A`, and `res = sqrt(chi2 * n11/det)` is the (2,2) element of
the inverted 2×2 normal matrix.

**It is a hand-port, not a merge.** The branch forks at `8ee220bc` (2026-03-12) and is 12
commits long, but `develop` is 622 commits ahead of that point, so the branch predates
both `3ca1e53d` (`:::` → `::`) and `4cd995df` (submodule call auto-resolution). Every
hunk uses the old dialect. The changes are small enough to transcribe by hand; the
alternative route through the `foo-old-syntax` tag described in
`docs/TONTO_REPOSITORY_BRANCHES.md` is not worth the ceremony for three files.

Two commits on that branch, `b8f63c49` and `6f7fa8cf`, are unrelated to extinction and
are not part of this port. The `foofiles/tree{str}.foo` hunk in `3e29ca20` adds the new
component's initialiser and must come across with it.

## 3. Defect register

The "Loud?" column follows the convention of `docs/TONTO_AND_MPI.md`. The silent rows
are the dangerous ones.

| # | Site | Defect | Loud? |
|---|---|---|---|
| 1 | `diffraction_data.inq.foo:722` | `d_chi2F_d_ext` uses `REALIFY(F_calc*F_calc*F_calc)` = Re(F³) where the derivative requires \|F\|³. Wrong **sign** for every reflection with negative real `F_calc` — roughly half of a centrosymmetric dataset. This is the gradient that drives `eps`. | no |
| 2 | `diffraction_data.inq.foo:403`, `:472` | `I_scale_and_extn_correction` never assigns `res` when `eps /= 0`; the guarding `DIE` is commented out at `:418` and `:502`. Intensity refinement with extinction reads uninitialised memory. | no |
| 3 | `diffraction_data.inq.foo:740` | `d_chi2I_d_ext` returns `ZERO * p(1)`. With #2, an intensity refinement with extinction runs to completion and reports a converged answer. `set.foo:1100–1101` has both guarding `DIE`s commented out too. | no |
| 4 | `diffraction_data.inq.foo:685` | `d_chi2I_d_scale` returns `FOUR*tmp/(N-Np)`; the derivative is `TWO*tmp/(p(1)*(N-Np))`. Wrong with extinction off as well. | no |
| 5 | `diffraction_data.read.foo:2523` | Testing `_refine_ls_extinction_method`, the code reads `_refine_ls_number_parameters` into `sval`. A CIF that says `none` therefore never turns extinction off. Copy-paste slip. | no |
| 6 | `diffraction_data.put.foo:1485` | The extinction diagnostic plot sets its "no extinction" curve to `F_pred/ext`, where `ext` is the combined `s*y`. It should divide by `y` alone; as written the curve is `\|F_calc\|`, low by the scale factor. | no |
| 7 | `diffraction_data.set.foo:1494` | `update_n_param` accumulates. See §4. | no |
| 8 | `diffraction_data.set.foo:1590` + `:1499` | The XCW `+1` for lambda is discarded in any XWR run. See §4. | no |

### Robustness, not defects

| Site | Issue |
|---|---|
| `set.foo:1286`, `:1315` | The BFGS is unconstrained and starts from `p(2) = ZERO` on every cycle. Nothing prevents `eps` going negative, and `1 + eps\|F\|^2 A <= 0` yields `sqrt(sqrt(negative))` = NaN. Clamp at zero, or reparameterise; and start from the current `.extinction_factor` rather than from zero. |
| `vec{real}.foo:2166` | `DIE_IF(fail,"Exceeded allowed iterations")` is commented out, so a failed minimisation returns its last iterate quietly. This routine's only caller is the extinction optimiser. |
| `set.foo:1079`, `:1511` | Incompatibility with multiple scale factors is enforced by `ENSURE`, which compiles to nothing in a release build. It has to be a `DIE`. Note that the *vector* form `F_scale_and_extn_correction(scale_factors, eps)` does apply extinction, while `optimize_F_extinction_factor` optimises only a single scale — so in release the combination runs and is wrong. |
| `types.foo:3518` | `wavelength` defaults to `-ONE`. `set_d_and_theta` then produces negative theta and a corrupted angular factor with no diagnostic. Under the SHELXL form it is worse: `lambda^3 < 0` gives NaN. Extinction must require a wavelength. |
| `set.foo:252` | `set_n_param` asserts `ENSURE(val>1,"n_param must be non negative")` — rejects a legitimate `n_param = 1`, and the message does not describe the test. |

## 4. The parameter count and the goodness of fit

The accounting exists and is correct in principle. `update_n_param`
(`diffraction_data.set.foo:1494`) adds

- `+2` when `refine_extinction` — scale and extinction;
- `+1` when only `refine_scale`;
- `+1` when `.XCW` — lambda.

So switching extinction on adds exactly **one** parameter over a scale-only refinement,
which is what is wanted. `refine_scale` need not also be set: `optimize_F_extinction_factor`
optimises the scale itself, and the `+2` accounts for both.

`n_param` is not only a reported statistic. It divides the GoF², and
`SCF_DATA:set_penalty` (`scf_data.foo:126`) assigns `.penalty = crystal.xray_data.chi2`,
so **an error in `n_param` changes the converged lambda of an XCW, not just a printed
number.** It also scales the covariance matrix, at `set.foo:2483`.

Two things are wrong with it.

### 4a. The XCW `+1` is lost after a structure refinement

`update_fit_info` sets `.n_param_manually_set = TRUE` when the fit finishes
(`set.foo:1590`), and the whole body of `update_n_param` is gated on
`NOT .n_param_manually_set` (`:1499`). In an XWR — refine, then constrain — the HAR fit
finishes first, so `MOLECULE.SCF`'s `set_XCW(TRUE)` (`molecule.scf.foo:1794`, commented
"add one to n_param") has no effect at all.

Measured, not inferred. In `tests/long/so2_rhf_DZP_anharmonic_cluster_charge_XWR/stdout`:

| Line | Stage | `N_p` |
|---|---|---|
| 618 | after the structure refinement | 27 |
| 971 | after the XCW | 27 |

The `+1` for lambda never arrives. A *pure* XCW gets it right —
`tests/long/nh3_x-ray-constrained-rhf_cc-pVTZ/stdout:664` reports `N_p = 2`, being scale
plus lambda — because no fit ran to freeze the flag. **This defect is live today, with
extinction switched off.** With extinction on it compounds: the XWR would carry the
`+2` from the HAR and still lose the lambda.

`initialize_fit_data` clears the flag again at `:1371`, which is why a `hart` run that
reads `_refine_ls_number_parameters` from its input CIF is not affected — the fit
un-freezes it. A pure XCW reading such a CIF *is* affected, since nothing clears it.

**Open question for a decision, not a defect:** what should `N_p` be in the XCW stage of
an XWR — frozen at the refinement count, or recomputed as scale (+ extinction) + lambda?
The two give different lambdas.

### 4b. `update_n_param` accumulates within a fit iteration

During a fit, `refine_structure OR fit_structure` is true, so the `.n_param = 0` reset at
`:1505` is skipped — but the increments below it are not. Every call therefore adds
another `+1` (or `+2`).

It does not run away: `solve_normal_equations` reassigns
`.n_param = n_p - .n_0 - .near_0` at `:2477` before calling `update_n_param_and_chi2`, so
each iteration starts clean, and the printed value is post-solve and stable —
`tests/hart/urea_hart_STO-3G/urea.out` shows `27` on all five refinement iterations. But
the GoF² that `make_F_predicted` computes *before* the solve uses a count one too high.
On urea that is `817-27` against `817-28`, an error of 0.13 %. With extinction on it
becomes two parameters.

## 5. Which angular factor — a decision

Tonto's `A = (1+cos²2θ)/(1+cos2θ·sin2θ)` has no `lambda^3` and no `1/sin 2θ`, so it does
not grow at low angle, where extinction is strongest. Its denominator is
`1 + (1/2) sin 4θ`, which matches no published form that could be identified, and the
code cannot settle the question: `docs/CCTBX_INTO_TONTO.md:461` names Tonto's model
"Larson-type" without giving it, and Jayatilaka & Dittrich (2008), *Acta Cryst.* **A64**,
383–393 writes only `F_j^c = s X_j(eps,|F_j|) |F_j|` with `X_j` attributed to Larson
(1970). **Resolving this needs the primary source: Larson, A. C., in *Crystallographic
Computing*, ed. F. R. Ahmed (Copenhagen: Munksgaard, 1970), pp. 291–294.**

The alternative, already implemented on `Lolo_CP2K`, is the SHELXL empirical correction,
equation (62) of Bourhis, Dolomanov, Gildea, Howard & Puschmann (2015), *Acta Cryst.*
**A71**, 59–75 — the olex2.refine specification paper, whose equation numbering
`docs/CCTBX_INTO_TONTO.md` follows throughout:

```
F_c' = F_c [1 + 0.001 x F_c^2 lambda^3 / sin 2theta] ^ (-1/4)
```

`x` is the refined extinction parameter. The paper notes that this is the same
correction SHELXL uses, and that per the SHELXL documentation it is "close to the work
of Becker & Coppens (1974) but not identical".

Adopting it removes the obstacle recorded at `docs/CCTBX_INTO_TONTO.md:461` — that
Tonto and olex2.refine use different functions, so no external comparison is meaningful
until one of them is ported. The cost is that `eps` changes meaning, and any historical
value (such as the 0.0001 above) becomes uninterpretable. Adopting it also introduces
the `1/sin 2θ` pole, which needs the wavelength guard of §3 and a low-angle cutoff.

This is a scientific decision, taken before the port rather than during it.

## 6. Port status (2026-08-22, in progress — nothing committed)

Decisions taken by Dylan on 2026-08-22:

- **Use the SHELXL formula**, eq (62) of §5.
- **`N_p` stays frozen at the refinement count.** No term is added for the XCW
  Lagrange multiplier, so a `lambda = 0` XCW reproduces the GoF of the HAR it starts
  from. This is the behaviour Davidson *et al.* (2022) §2 requires: the effective number
  of wavefunction parameters is `N_param(0) = 0`, and `lambda` is a Lagrange multiplier
  rather than a least-squares parameter.

**Done in the working tree.** All five changed files translate without error
(`FooToFortran`, checked individually); **not yet compiled and not yet run.**

| File | Change |
|---|---|
| `types.foo` | new `extinction_factor_esd` and `n_param_structure` components |
| `diffraction_data.inq.foo` | SHELXL `extinction_angle_part`; both `I_scale_and_extn_correction` routines implemented; `d_chi2F_d_ext` `Re(F³)` → `\|F\|³`; `d_chi2I_d_scale` `FOUR` → `TWO/p(1)`; `d_chi2I_d_ext` implemented; `F_`/`I_extinction_factor_esd` added |
| `diffraction_data.set.foo` | esd lifecycle; `ENSURE` → `DIE_IF` for multiple scale factors; live wavelength check; `get_F_`/`get_I_optimum_parameters` made impure so those `DIE`s fire in release; `update_n_param` rewritten |
| `diffraction_data.put.foo` | `_refine_ls_extinction_coef` with esd; method now `SHELXL`; esd in the statistics blocks; extinction plot divides by `y`, not `s*y` |
| `diffraction_data.read.foo` | CIF `_refine_ls_extinction_method` read fixed (defect 5) |

Two findings from the port itself, both worth keeping:

- **`get_F_optimum_parameters :: PUREp`** (`set.foo:1073`) — `PUREp` is not a macro and
  appears nowhere else. The translator **silently drops** the unknown attribute, so the
  routine has been impure by accident. It is now impure deliberately. That the
  translator accepts an unrecognised procedure attribute is a separate matter, and is
  the same class as milestone 8: a construct that parses but does not mean what it says.
- **A `DIE` cannot live inside a `PURE` routine.** In a release build `USE_PRECONDITIONS`
  is undefined, so `PURE` expands to `pure`, while `USE_ERROR_MANAGEMENT` *is* defined,
  so `DIE`/`DIE_IF` are live. Only `ENSURE`/`WARN` may sit inside `PURE`. The hard
  wavelength check therefore sits at the impure entry points, with an `ENSURE` left in
  `extinction_angle_part` for debug builds.

**Done since.** A negative extinction factor is unphysical, and it makes
`1 + x|F|²A` negative, which has no real fourth root, so the correction becomes
not-a-number. Dylan's decision: test for it where the minimiser returns, not inside the
correction, because the minimiser knows nothing about the meaning of its parameters. An
`ENSURE` in both `optimize_F_` and `optimize_I_extinction_factor` reports it in a debug
build and costs nothing in a release build.

Also done, both at Dylan's request: reading a CIF, `_refine_ls_extinction_method` now
switches the correction **on** as well as off, and `_refine_ls_extinction_coef` is read
back, so a job restarted from a CIF continues from the extinction factor already found
rather than refitting it from zero.

**Also done: the XCW constraint gradient.** The three routines that build the constraint
contribution to the Fock matrix multiplied the derivative of `|F_calc|` by the
scale-and-extinction factor alone. Holding the scale factor and the extinction parameter
fixed is *not* an approximation — both minimise the same quantity being differentiated,
so their chain-rule terms are exactly zero. What was dropped is different: the
per-reflection factor also depends on `|F_calc|`, and so on the density matrix. That term
does not vanish, and the least squares has always included it as the `dX` term of
`d_F_pred_dX`. It matters because the gradient is added to the Fock matrix, so a wrong
gradient converges to a different wavefunction rather than merely taking longer. With
extinction off the missing factor is exactly one. New inquiry routine
`d_F_pred_d_F_abs` supplies it and answers the no-extinction case itself, so no caller
needs a test and nothing is added inside the shell-pair loop.

`MOLECULE.SCF:make_pnd_constraint` needs the same treatment but was left alone: neutron
fitting is commented out of `make_constraint_data`, so the routine is unreachable. It
also reads `n_par` without ever assigning it.

**Setting the switch part way through a job.** `tests/long/quartz_NN_HAR_L1_rhf_def2-SVP`
refines twice, once without the correction and once with it, so a second `xray_data=`
block sits between the two `fraghar_refinement` calls. This does **not** rebuild anything.
`MOLECULE.MAIN:read_crystal` and `CRYSTAL:read_xray_data` both create only if the object is
absent and then read keywords into the existing one, so the reflections, the fragments and
the refined structure all carry over — demonstrated by the first refinement reproducing
its previously blessed numbers to every digit.

There is one side effect worth knowing. `CRYSTAL:read_xray_data` ends by calling
`DIFFRACTION_DATA.SET:update` (`crystal.foo:594`), which re-runs `set_d_and_theta`, the
equivalence factors, and the pruning and sorting over the reflections already held. That
is idempotent for a job that only flips a switch, and it is *necessary* when a keyword
changed something structural such as `stl_limit` or `hkl_range`, so it is left alone. But
a re-entered block is not free, and a job that re-enters one after pruning should know
that pruning runs again.

*(A top-level `optimise_extinction=` keyword on `MOLECULE` was added to avoid the nesting
and then reverted: it duplicated a keyword that already sat in the right place, and put a
diffraction-data setting into a namespace where it does not belong.)*

**Still to do.**

1. `min_BFGS`'s commented-out `DIE_IF` on non-convergence (`vec{real}.foo:2166`).
2. Build, and run the tests of step 4 below.

**Expected reference changes, beyond any job that switches extinction on.**

- A **pure** XCW loses the `+1` for `lambda`. `tests/long/nh3_x-ray-constrained-rhf_cc-pVTZ`
  moves from `N_p = 2` to `N_p = 1`, which changes its GoF and its converged `lambda`.
  An **XWR** is unaffected, because `N_p` was already frozen there.
- Any job whose GoF² was computed between a structure-factor rebuild and the following
  solve of the normal equations moves by the accumulation of §4b — of order 0.1%.

## 6. Plan

1. **Decide** the angular factor (§5) and the XWR parameter count (§4a).
2. **Port** `58f4a23d` and `3e29ca20` from `origin/Lolo_CP2K` by hand, translating
   `:::` → `::` and dropping the `.INQ:` / `.SET:` qualifiers. This closes defects 1–4
   and adds the extinction esd and `_refine_ls_extinction_coef`.
3. **Fix** the remaining register: 5, 6, 7, 8, and the robustness items — the negative-`eps`
   guard, the BFGS warm start, the restored `DIE_IF` on non-convergence, `ENSURE` → `DIE`
   for multiple scale factors, and the wavelength precondition.
4. **Test.** The suite has no extinction coverage at all, and this must not stay true.
   Cheapest first: `hart --extinction t` on `tests/hart/urea_hart_STO-3G/urea_init.cif`,
   about 5 s. Then an XCW job with extinction on — `nh3_x-ray-constrained-rhf_cc-pVTZ`
   is the natural choice, being where it last ran. A regression test must assert that
   two different `eps` give *different* GoF², so the correction cannot silently become
   inert again, as it did for ten years.
5. **Rebless** the affected references. Extinction changes numbers wherever it is
   switched on; the `n_param` fixes of §4 change GoF² wherever an XWR runs.
6. **Then the Akaike information criterion for choosing `lambda`** (Dylan, 2026-08-22).
   He has wanted to select the XCW Lagrange multiplier by an information criterion
   rather than by a target GoF, and how to do it is an open question. It is placed here
   deliberately: after extinction is built, tested and covered by a test job, and before
   the naming work below. It is related to the parameter count of §4a — an information
   criterion needs a count of parameters, and the whole difficulty is that the effective
   number contributed by the wavefunction is not known at intermediate `lambda`.
7. `docs/GOF_NOT_CHI2.md` is independent. If it lands in the same window, its table
   change can share step 5's reblessing pass; nothing requires it to.

---

# Appendix A — choosing lambda by an information criterion

**Opened 2026-08-23**, at Dylan's request, as step 6 of the plan above. Nothing here is
implemented. It is written down so the next session starts from the reasoning rather than
from the question.

## A1. Why cross-validation was spotty

Cross-validation has been tried and the free-set statistic jumped around. Two causes, one
of them quantitative and decisive.

**Too few free reflections.** If the model is right, the free-set statistic is a sum of
`m` squared standardised residuals, so it follows a chi-squared distribution on about `m`
degrees of freedom. Its mean is `m` and its variance is `2m`, so its **relative standard
error is `sqrt(2/m)`**. For urea, five percent of 817 reflections is 41, giving 22%. To
tell apart two values of lambda whose free residual differs by two percent, you would need
about five thousand free reflections. A single five-percent split of a small-molecule
dataset cannot locate a minimum. This is not a defect in the implementation; the statistic
simply is not precise enough.

**The split is not stratified.** `CRYSTAL:set_r_free_reflections` (`crystal.foo:220`) draws
one uniform random number per reflection and puts it in the free set if the number falls
below the percentage. Two consequences follow.

- The free-set size is not fixed. It is binomial, so it varies from run to run by about
  `sqrt(m)` — seven reflections in the urea case.
- The split is not balanced across resolution or intensity. A handful of strong low-angle
  reflections dominate the weighted residual. Whether two or five of them land in the free
  set changes the free statistic substantially, and that is decided by chance.

Sorting the reflections by resolution, and within a shell by intensity, then taking every
`k`-th, removes most of the second problem at no cost.

**A nuance worth keeping.** The split is drawn once when the crystal is set up, so a scan
over lambda within a single job uses the same free reflections throughout. The *curve* of
free residual against lambda should therefore be reasonably smooth even when the estimate
is imprecise; what moves from split to split is where its minimum sits. If the curve itself
was jagged within one job, the cause lies elsewhere — most likely the SCF convergence
tolerance at each lambda, or the free set being redrawn.

**One hypothesis checked and rejected.** `crystal.foo:202` copies the whole diffraction-data
object to make the free one, parameter count included, which would divide the free
statistic by `(m - N_p)` instead of `m`. It does not: `VEC{REFLECTION}:put_F_free_stats`
uses `F_chi2`, which divides by `(m - 1)`. The normalisation is right to within a fraction
of a percent.

## A2. Dylan's question: does cross-validation need several reserved sets

Yes, and this is the standard fix. **k-fold cross-validation** partitions the reflections
into `k` groups, holds each out in turn, and adds up the residuals of the held-out
predictions. Every reflection is predicted exactly once, by a model that did not see it,
so the statistic rests on all `N` reflections rather than on `N/20`. The relative standard
error falls from `sqrt(2/m)` to about `sqrt(2/N)` — for urea, from 22% to 5%.

The cost is `k` complete scans over lambda instead of one. The runs are independent.

What Tonto would need for this is small: a way to say which fold is being held out, and a
seed, so that the partition is reproducible and can be stepped through. Today the split is
a single unseeded random draw with no way to request a different one.

**This is Brunger's own position, not a departure from it.** He calls the procedure
*complete cross-validation* and defines it in equations (13) and (14) of Brunger (1997),
*Methods in Enzymology* **277**, 366: partition the data into `t` disjoint test sets of
equal size, refit with each omitted in turn, and add the held-out residuals. His reason is
exactly the one above — "Because of the small size of the test set, the individual test
residuals show large statistical fluctuations. Therefore, complete cross-validation must
be applied to obtain statistically significant results." He goes further and repeats the
whole procedure with twenty different partitionings to get a mean and a standard deviation.

## A2a. Why it works for macromolecules, checked against the source

The received explanation — that cross-validation works in macromolecular crystallography
because there are so many reflections — is correct, and Brunger quantifies it. In the same
chapter: "the standard deviation of the free R value is approximately given by
`R_free/sqrt(n)`, where `n` is the number of reflections in the test set." That is a
relative precision of `1/sqrt(n)`, the same inverse-square-root behaviour as the
`sqrt(2/m)` of A1, which applies to a chi-squared rather than to an R value.

Put against real dataset sizes, with a five percent test set:

| Case | Reflections | Test set | Relative s.e. |
|---|---|---|---|
| urea, `tests/hart/urea_hart_STO-3G` | 817 | 40 | 22% (chi2), 16% (R_free) |
| small-molecule charge density | 6000 | 300 | 8% (chi2), 6% (R_free) |
| protein at 2.0 A | 25000 | 1250 | 4.0% (chi2), 2.8% (R_free) |
| protein at 1.5 A | 60000 | 3000 | 2.6% (chi2), 1.8% (R_free) |

So the received explanation is right, but it is worth being precise about the size of the
effect: the advantage is a factor of five or six in precision, not a difference in kind.
Brunger's recommendation of a ten percent test set was reached from model calculations,
and he notes that the standard deviation also worsens with resolution, a 2.5% test set at
1.8 A behaving like a 10% set at 3 A because the two hold about the same number of
reflections.

**A second reason, which is about the signal rather than the noise.** Macromolecular
refinement has few observations per parameter, so overfitting is a large effect and easy
to see. Ordinary small-molecule refinement has 800 reflections against 27 parameters, so
there is very little overfitting to detect: a small signal measured with a noisy ruler.
The XCW sits between the two. Its wavefunction has many parameters, so the effect being
looked for is much larger than in an ordinary small-molecule refinement, which is a point
in favour of cross-validation here even though the datasets are small.

## A3. The Akaike criterion

AIC is `2k - 2 ln L`, with `k` the number of parameters and `L` the maximised likelihood.
The sigmas are measured rather than estimated, so the errors are Gaussian with known
variances and

    -2 ln L  =  sum_i (F_obs,i - F_pred,i)^2 / sigma_i^2  +  constant

which is the true chi-squared, equal to `(N_refl - N_p)` times the GoF-squared. Hence

    AIC  =  sum_i F_z,i^2  +  2 k_eff  +  constant

The first term falls as lambda rises. The second rises. The minimum is the chosen lambda.
Note that this is one of the few places where the genuine chi-squared, not the GoF-squared,
is the quantity wanted — see `docs/GOF_NOT_CHI2.md`.

Everything turns on `k_eff`, and it cannot be a count of wavefunction parameters. There are
more of those than there are reflections, and at lambda = 0 the data influences none of
them. The replacement is the **effective number of parameters**, defined by how closely the
fit tracks the data:

    k_eff  =  sum over reflections of   d F_pred,i / d F_obs,i

Nudge one observed structure factor. If the prediction for that reflection follows the
whole way, that reflection has consumed one parameter. If it does not move, none. Add up.
The quantity is a real number, not an integer, and it is zero at lambda = 0, which is what
Davidson *et al.* (2022) section 2 says it must be.

## A4. The analytic route, and why it is a CPHF calculation

Write `C` for the wavefunction parameters — orbital rotation parameters — and let the XCW
make stationary

    J(C; F_obs)  =  E_QM(C)  +  lambda * GoF2(C; F_obs)

(the sign convention is the code's; only derivatives with respect to `C` matter below, so
the target value that lambda enforces drops out).

The converged wavefunction `C*(F_obs)` satisfies the stationarity condition

    g(C, F_obs)  =  dJ/dC  =  0

Differentiate that identity with respect to one observed structure factor `F_obs,i`. The
condition holds for every `F_obs`, so its total derivative is zero:

    (d2J/dC dC) (dC*/dF_obs,i)  +  d2J/dC dF_obs,i  =  0

Write `H` for the first factor and `b_i` for the second:

    H    =  d2E_QM/dC dC  +  lambda * d2 GoF2/dC dC
    b_i  =  lambda * d2 GoF2/dC dF_obs,i

so that

    dC*/dF_obs,i  =  -H^-1 b_i

`H` is the electronic Hessian — the same orbital-rotation Hessian that appears in a
stability analysis — plus lambda times the Hessian of the fit term. Solving a linear system
in `H` for a perturbation of the Hamiltonian is exactly a coupled-perturbed Hartree-Fock
calculation. There is one right-hand side per reflection, which is why the direct form is
out of reach: a few thousand CPHF solutions per lambda.

**It collapses to something much better.** Write `v_i = d F_pred,i / dC`. Then

    GoF2          =  (1/(N-N_p)) sum_j (F_pred,j - F_obs,j)^2 / sigma_j^2
    d GoF2/dC     =  (2/(N-N_p)) sum_j (F_pred,j - F_obs,j) v_j / sigma_j^2
    b_i           =  -lambda (2/(N-N_p)) v_i / sigma_i^2

and the sensitivity of one prediction to its own observation is

    d F_pred,i / d F_obs,i  =  v_i^T (dC*/dF_obs,i)
                            =  lambda (2/(N-N_p)) v_i^T H^-1 v_i / sigma_i^2

Summing over reflections turns the sum of quadratic forms into a trace:

    k_eff  =  trace[ H^-1 * lambda * B ],    B = (2/(N-N_p)) sum_i v_i v_i^T / sigma_i^2

`B` is the Gauss-Newton part of the Hessian of the fit term — the usual normal matrix,
formed by dropping the term containing residuals times second derivatives. So with `A` the
electronic Hessian,

    k_eff  =  trace[ (A + lambda B)^-1 lambda B ]

This is the ridge-regression formula for effective degrees of freedom, with the quantum
mechanical energy playing the part of the penalty. It behaves correctly at both ends. At
lambda = 0 it is zero. As lambda grows without bound it tends to the rank of `B`, which is
the smaller of the number of reflections and the number of wavefunction parameters. Both
limits are what Davidson *et al.* state.

**What it would cost.** Only the trace is needed, not the matrix. The trace of a matrix you
can apply but not form is estimated by applying it to random vectors of plus and minus ones
and averaging — `trace(M) = expectation of z^T M z`. Each sample needs one solve with
`(A + lambda B)`, so twenty or so samples replace thousands. Two of the three ingredients
already exist in some form: `v_i` is essentially what the constraint routines build, and
`B` acting on a vector is a contraction Tonto can already do. The missing piece is the
electronic Hessian `A` acting on a vector, which is new work.

## A5. The Monte Carlo route, which needs no Hessian

The same quantity can be measured rather than derived. The definition used above has an
equivalent statistical form,

    k_eff  =  sum_i covariance(F_pred,i, F_obs,i) / sigma_i^2

The two agree for any fitting rule that is deterministic and differentiable, by Stein's
lemma. The covariance form suggests an estimator that treats the whole XCW as a black box.

Draw a vector `z` of independent standard normal numbers. Perturb the data by

    F_obs  ->  F_obs + epsilon * sigma * z

element by element, rerun the XCW at the same lambda, and form

    k_hat  =  (1/epsilon) * sum_i  z_i * [ F_pred,i(perturbed) - F_pred,i ] / sigma_i

To first order the change in prediction is `sum_j (dF_pred,i/dF_obs,j) epsilon sigma_j z_j`,
and since the expectation of `z_i z_j` is one when `i = j` and zero otherwise, the
expectation of `k_hat` is the sum of the diagonal sensitivities, which is `k_eff`. Average
over independent draws of `z`; the error falls as one over the square root of the number of
draws, so twenty to fifty draws give a usable figure.

**Three practical points.**

The step size trades bias against noise. Bias grows with `epsilon`, because the response
stops being linear. Noise grows as `1/epsilon`, because the measured change shrinks while
the numerical noise in `F_pred` does not. A perturbation of about one sigma is the usual
starting point, and the result should be checked for stability by halving and doubling it.

Numerical noise must be controlled, or it will swamp the signal. Start every perturbed run
from the unperturbed converged wavefunction and use the same convergence tolerance, so that
the two runs follow nearly the same path and their common error cancels in the difference.
Without this the SCF tolerance sets the smallest measurable `k_eff`.

Tonto already has the perturbation. `F_sigma_noise=` is a live keyword, documented as noise
added to `F_exp` and `F_sigma` in units of `F_sigma`. Whether it does precisely what is
wanted needs checking — in particular whether it perturbs `F_sigma` as well, which this
estimator does not want — but the mechanism exists, and the rest of the estimator is
arithmetic on quantities already printed.

## A6. Underestimated sigmas: which methods care

Dylan's question, 2026-08-23: the Monte Carlo estimator uses sigmas, so does it inherit
AIC's weakness? It does not. The two roles sigma plays have to be separated.

**The estimate of `k_eff` is immune.** The perturbation is `epsilon sigma z` and the
estimator divides by `epsilon sigma`, so

    k_hat  =  sum_i sum_j  z_i z_j (sigma_j/sigma_i) J_ij,   J_ij = dF_pred,i/dF_obs,j

The expectation of `z_i z_j` is one when `i = j` and zero otherwise, so only the diagonal
survives and the sigma ratio there is exactly one. The result is `sum_i J_ii` whatever the
sigmas were. Scaling them all by ten changes nothing. The variance of the estimator
changes; its mean does not.

**AIC as a whole is not immune, and the size of the effect is alarming.** The criterion
balances `sum_i F_z,i^2` against `2 k_eff`. The first term uses sigma and the second does
not. If the true errors are `c` times the stated ones, the first is inflated by `c^2` while
the penalty is untouched, so minimising it is the same as minimising the correct residual
with the penalty divided by `c^2`. Urea's GoF is 7. Were that misfit entirely
underestimated sigmas, the complexity penalty would be about fifty times too weak, AIC
would effectively ignore it, and it would choose the largest lambda offered without
signalling anything.

**The standard repair.** Treat the error scale as unknown and estimate it with the model:

    AIC  =  N ln( sum_i F_z,i^2 / N )  +  2 (k_eff + 1)

Scaling every sigma by `c` shifts the first term by `-2 N ln c`, the same at every lambda,
so differences in AIC are unaffected. This form is immune to a *common* error in the
sigmas. It is not immune to sigmas that are wrong in a way varying with resolution or
intensity, which is the more realistic failure.

**Cross-validation is invariant without needing a repair.** Multiplying every sigma by a
constant multiplies the held-out residual by a constant, so the curve moves bodily up or
down and the lambda at which it turns does not move. Better still, cross-validation need
not use sigmas at all: Brunger's free R value, equation (15) of the 1997 chapter, is a sum
of `|F_obs - k F_calc|` over a sum of `|F_obs|`, with no weights in it.

**Two refinements to "look for where it starts to rise".** A single test set is enough to
see the *shape*, because the same reflections are used at every lambda, so the curve is
smooth even when its level is imprecise; what moves between splits is *where* the turn
lies. And near any turning point the curve is flat by construction, so the minimum is
located to within roughly the noise divided by the curvature. Averaging over folds attacks
the numerator, which is the argument for complete cross-validation all over again.

**The weakness cross-validation does have here.** The held-out reflections are not
independent of the working set. The model links them — atomicity in an ordinary
refinement, and in the XCW a single wavefunction tying every reflection to every other — so
a reflection set aside is partly predictable from those kept, and the held-out residual is
optimistic. Brunger acknowledged this for proteins, where non-crystallographic symmetry
makes it acute. For *choosing* lambda it matters less than it appears, since a bias varying
slowly with lambda largely cancels in the comparison, but the resulting free residual is
not an honest estimate of prediction error and should not be quoted as one.

## A7. Why AIC still discriminates once the sigmas are rescaled

Dylan's objection, 2026-08-23, and it goes to the heart of the method: *"you mentioned
scaling the sigmas by a certain factor: but how does this make sense, since in normal least
squares the chi2 would then become equal to one. Why does it make sense in AIC?"*

He is right that it becomes one. The maximum-likelihood estimate of the scale is
`c^2 = RSS/N`, so after rescaling the chi-squared is `N` for **every** model, by
construction. The comparison therefore cannot be a comparison of chi-squared values.

It is not. Work the likelihood through. With errors distributed as `N(0, c^2 sigma^2)` and
`c` estimated from the data,

    -2 ln L  =  N ln(2 pi c^2)  +  2 sum_i ln sigma_i  +  RSS / c^2

Minimising over `c^2` gives `c^2 = RSS/N`, and substituting it back collapses everything to

    -2 ln L_max  =  N ln(RSS)  +  constant

where the constant does not depend on the model. Hence

    AIC  =  N ln(RSS)  +  2 (k_eff + 1)  +  constant

**The fit term is the logarithm of the residual, not the residual.** What is compared is
not how well each model fits in absolute terms — that has been made unmeasurable, on
purpose — but *how small an error level each model needs in order to explain the data*. A
better model requires a smaller assumed noise. The extra `+1` in the penalty is that
estimated noise level, counted as the parameter it is.

Two checks that this is sane.

**It reduces to the ordinary form when the sigmas are right.** Reduce the residual by a
fraction `f`. The gain in the new form is `N ln(1/(1-f))`, which is about `N f`. In the
known-sigma form the gain is `f * RSS`. If the sigmas are correct then `RSS` is about `N`
and the two agree. They differ by the factor `RSS/N`, which is the GoF-squared — precisely
the `c^2` of A6, and about fifty for urea.

**It gives up something real, and should.** With the scale estimated you can no longer say
a model fits *well*, only that it fits *better* than another. That looks like a loss but it
is honest: if the noise level is unknown, absolute goodness of fit is not a measurable
thing. It also matches the situation actually at hand, where the GoF comes out at 3 or 7
and nobody believes the true value is 1.

This is the cleanest way to see why cross-validation appeals. It obtains the same
invariance without having to write down a likelihood at all.

## A8. Cautions, and a recommendation

AIC assumes the number of parameters is small compared with the number of observations. If
`k_eff` becomes an appreciable fraction of the reflection count, the corrected form
`AICc = AIC + 2k(k+1)/(N-k-1)` should be used instead.

**Suggested order.** Fix the free-set selection first — stratify it and make it seeded and
selectable — and try k-fold cross-validation, which needs no parameter count and no new
theory. It is a small change to `CRYSTAL:set_r_free_reflections` plus reviving the free-set
calls in `MOLECULE.SCF:make_constraint_data`, which are commented out at
`molecule.scf.foo:2141`. Then treat AIC as the more principled goal, entering by the Monte
Carlo estimator of A5 rather than the Hessian of A4, because it needs no new derivative
code. The analytic route is worth building only if the Monte Carlo estimate turns out to be
too noisy or too slow.

## A9. The exchange that produced this appendix

Dylan's questions, kept close to verbatim because they drove the analysis, each with the
short form of the answer and a pointer to where it is worked out.

**"I don't fully understand how it determines the number of parameters. Would it require
differentiating the effective Fock matrix again, to produce a hessian?"**

Yes for the analytic route, and it is exactly a coupled-perturbed Hartree-Fock problem —
differentiate the stationarity condition, and the Hessian of the constrained functional
appears as the matrix to invert. But the sum over reflections collapses to
`trace[(A + lambda B)^-1 lambda B]`, and only the trace is needed, which a handful of
random probe vectors will estimate. There is also a Monte Carlo route that needs no
Hessian at all. Sections A4 and A5.

**"We did try cross validation, and the chi2_free values were very spotty - perhaps because
we did not have enough reflections or because we did not select evenly across resolution or
intensity? Wouldn't cross validation still require multiple sets of reserved reflections to
be sure?"**

Both suggested causes are real, and the third point is decisive. The free statistic on `m`
reflections has relative standard error `sqrt(2/m)`, which is 22% for a five percent split
of urea's 817 reflections. The selection is an unstratified uniform draw, so the test set
is neither of fixed size nor balanced. And yes, several reserved sets are needed — that is
Brunger's own position, which he calls complete cross-validation and says is *required*
when the test set is small. Sections A1 and A2.

**"I was told that, in macromolecular crystallography, cross validation works because there
were so many reflections."**

Correct, and Brunger quantifies it: the standard deviation of the free R value is about
`R_free/sqrt(n)`. Five percent of a protein dataset is one to three thousand reflections
against urea's forty, so the precision improves by a factor of five or six. A difference of
degree, not of kind. Section A2a.

**"I could never understand Brungers argument. Or others."**

Worth knowing that the 1992 paper does not derive the test-set size; the precision estimate
`R_free/sqrt(n)` is stated empirically in the 1997 chapter, and the ten percent
recommendation came from model calculations. The argument is heuristic, and the one part
that is not — that a single small test set fluctuates too much to be significant — is the
part that recommends complete cross-validation.

**"I see that the monte carlo method involves sigmas. Does it also suffer from the same
issue, that AIC does, regarding underestimated sigmas? In that case cross validation may be
the best? Since one only looks for the point where the chi2_free starts to diverge or
increase, on average ... right?"**

The Monte Carlo estimate of `k_eff` does not suffer: the perturbation and the normalisation
use the same sigma, so it cancels exactly. AIC as a whole does suffer, by a factor of the
GoF-squared. And yes to the last point: a common sigma error moves the cross-validation
curve bodily and leaves the turning point where it was, so looking for where the held-out
residual starts to rise is the robust procedure. Two refinements — a single split shows the
right shape but not the right location, and a flat minimum is located only to within the
noise divided by the curvature. Section A6.

**"You mentioned scaling the sigmas by a certain factor: but how does this make sense, since
in normal least squares the chi2 would then become equal to one. Why does it make sense in
AIC?"**

Because the criterion is then `N ln(RSS)`, not `RSS`. The rescaled chi-squared is indeed one
for every model; what differs between models is the error level each one needs to explain
the data. Section A7.

