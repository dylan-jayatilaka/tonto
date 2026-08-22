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
`docs/REPOSITORY_BRANCHES.md` is not worth the ceremony for three files.

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
