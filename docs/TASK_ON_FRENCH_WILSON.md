# Weak reflections: refinement on F², and French–Wilson amplitudes

**Opened 2026-11-27 (Dylan). Shelved the same day: LATER on the task register.** The plan and
the discussion behind it, written for whoever picks this up. It follows on from the weak-reflection
work for the TFVC paper (`docs/TASK_ON_TFVC_REFINEMENT.md`; runs in
`~/tonto-runs/2026-11-27_cutoff_test/`).

## 1. Decisions

1. **Refinement against F² becomes Tonto's default**, with I and σ(I) as measured, every
   reflection kept, negative intensities included. This is what SHELXL and olex2.refine do.
2. **French–Wilson amplitudes are an option**, used in two places only:
   - when a job asks for `refine_F= TRUE`;
   - for map coefficients (residual and difference densities), where an amplitude is needed.
3. **Order of work:** merge equivalents by default (Blessing), move the zeroing of negative
   intensities out of the reader, then F² refinement and reblessing, and French–Wilson last.
   The merge and the zeroing are separate items on the register.

## 2. Why: what Tonto does now, and the case against it

- Tonto refines against |F| by default (`refine_F= TRUE`). `refine_F= FALSE` stops with
  *"not implemented: no F2 parameter shifts"* (`CRYSTAL:get_parameter_shifts`, `crystal.foo`).
- At read time a negative intensity is set to zero and its σ(I) is enlarged by |I|
  (`REFLECTION:set_I_sigma` → `reset_I_exp_I_sigma`); `derive_Fs_from_Is` then gives F and σ(F).
- Earlier Hirshfeld atom refinements cut weak data: Capelli et al. (2014) kept F > 3σ(F);
  Davidson et al. (2022, Part II, p. 400) removed |F| < 4σ(F). The YQ28 jobs still use
  `f_sigma_cutoff= 4.0`, which rejects 9923 of 31457 reflections.

Both practices raise the weak structure factors. A cutoff on the *measured* F/σ(F) drops the weak
reflections whose intensity fell low by chance and keeps those whose intensity rose by chance.
Hirshfeld & Rabinovich (1973) showed that excluding weak reflections biases the scale factor and
the ADPs. Setting a negative intensity to zero raises its |F| in the same direction. A negative
measured intensity is a valid observation of F², and only a target in F² uses it as one.

## 3. Refinement on F²

Least squares on I with weights $`w = 1/\sigma^2(I)`$ is the exact likelihood when the error
in I is Gaussian, which it is to a good approximation after integration. No prior, no
transformation of the data, no rejection. The derivative follows from what Tonto already has:

```math
\frac{\partial |F|^2}{\partial p} = 2\,\mathrm{Re}\!\left(F^{*}\,\frac{\partial F}{\partial p}\right)
\qquad (1)
```

where $`F`$ is the predicted structure factor (with scale and extinction) and $`p`$ any refined
parameter.

**What exists already.**
- `DIFFRACTION_DATA.SET:solve_normal_equations` dispatches on `.refine_F`; a commented
  `solve_I_normal_equations` is next to it.
- Lorraine Malaspina's branch `origin/Lolo_CP2K` (2026-08-17) has an intensity path, with the
  SHELXL extinction correction (`docs/TASK_EXTINCTION_CORRECTION.md`, §2). Look there before
  writing anything.
- `get_parameter_shifts_I` does not exist; the DIE stands in for it.

**What has to be done.** `get_parameter_shifts_I` from Eq. (1); the weights (1/σ²(I), and the
SHELXL two-parameter scheme as an option); the statistics reported with an F² target: GoF on F²
first, then wR2 on F² and R1 on F, as SHELXL reports them; the default switched; every test that
refines reblessed on achari2.

## 4. French–Wilson (1978)

**Idea.** Estimate |F| and σ(|F|) for each reflection as the posterior mean and spread, given
the measured I and σ(I), and a prior from Wilson statistics. Write $`J = |F|^2`$ for the true
intensity, $`\Sigma`$ for the Wilson mean intensity at that resolution (times the multiplicity
factor $`\varepsilon`$ of the reflection), and assume a Gaussian error in I:

```math
p(J \mid I) \;\propto\; \exp\!\left[-\frac{(I-J)^2}{2\sigma^2}\right] p(J), \qquad J \ge 0
\qquad (2)
```

The Wilson priors are

```math
p_{\mathrm{acentric}}(J) = \frac{1}{\Sigma}\,e^{-J/\Sigma},
\qquad
p_{\mathrm{centric}}(J) = \frac{1}{\sqrt{2\pi\Sigma J}}\,e^{-J/2\Sigma}
\qquad (3)
```

so the posterior is a Gaussian in J, cut off at J = 0, centred at $`I-\sigma^2/\Sigma`$ for
an acentric reflection and at $`I-\sigma^2/2\Sigma`$ for a centric one, where the centric case
also carries the factor $`J^{-1/2}`$. The estimates are moments of it:

```math
\langle |F| \rangle = \langle J^{1/2} \rangle,
\qquad
\sigma^2(|F|) = \langle J \rangle - \langle J^{1/2} \rangle^2
\qquad (4)
```

**The "tricky integrals".** Moments of a truncated Gaussian with half-integer powers of J are
parabolic cylinder functions. Some are closed forms in erfc (⟨J⟩ acentric, ⟨|F|⟩ centric); the
others are not. French & Wilson tabulated them against $`h = (I-\sigma^2/\Sigma)/\sigma`$. Today
each is a one-dimensional numerical integral per reflection, at negligible cost. Two cares:
scale the integrand by its largest value before integrating, so that it does not underflow when
h is very negative; and treat reflections with h far below about −4 as outliers, as French &
Wilson and ctruncate do (threshold quoted from memory; check the paper).

**Where it goes.** The inputs live in three places, so the code has three layers:

| What | Needs | Where |
|---|---|---|
| Posterior \|F\| and σ(\|F\|) of one reflection | I, σ(I), Σ, ε, centric flag | `REFLECTION`, a pure routine beside `derive_Fs_from_Is` |
| Σ by resolution shell: smoothed shell means of I/ε | sin θ/λ of every reflection | `VEC{REFLECTION}`, which holds `stl` |
| ε (operations that leave h unchanged) and the centric test (an operation that sends h to −h) | the space group | `CRYSTAL`; `crystal.foo` ~12009 has a related centric check |

`CRYSTAL` calls it after the merge, and only when `refine_F= TRUE` or a map needs amplitudes.

**Caveat.** The French–Wilson |F| is a posterior mean. It pulls each weak reflection towards the
Wilson average, so it is unbiased averaged over many structures but not reflection by reflection.
For ADPs and densities the F² target of §3 is the cleaner one; French–Wilson is the fair way to
refine on |F| when that is wanted.

## 5. Maps

Refinement maps everywhere use the model phases. Small-molecule work uses
$`F_\mathrm{obs}\,e^{i\varphi_c}`$ and $`(F_\mathrm{obs}-F_\mathrm{calc})\,e^{i\varphi_c}`$.
Protein work uses the σA-weighted $`(2mF_\mathrm{obs}-DF_\mathrm{calc})\,e^{i\varphi_c}`$,
where the figure of merit m down-weights reflections with uncertain phases and D scales the
calculated structure factor for the model's errors, against "model bias". Either way a value of
$`F_\mathrm{obs}`$ is needed for every reflection. For Tonto's residual density
$`\sqrt{\max(I,0)}`$ is the simple choice and French–Wilson the careful one. At small-molecule
data quality the choice changes only the noise level of the map.

## 6. Why macromolecular refinement uses F (summary of the discussion)

1. **Maps need amplitudes.** A protein model is built step by step from maps, so the data must
   exist as amplitudes, weak and negative ones included.
2. **The model's error dominates the measurement error.** The maximum-likelihood targets (Read
   1986, σA; Pannu & Read 1996; Murshudov et al. 1997, Refmac; phenix.refine) model F_obs about
   F_calc with a Rice distribution whose width comes from the missing and misplaced parts of the
   model. That is written in |F|; the measurement error is added to the width as a correction.
   For a nearly complete small-molecule model the scatter is mostly measurement error, Gaussian
   in I, so least squares on F² is the right likelihood.
3. **Weak data.** Protein data run to where the signal fades, so many reflections are weak or
   negative, and there are few observations per parameter.
4. **History.** PROLSQ, TNT and X-PLOR refined on F; TRUNCATE outputs F; MTZ files and deposits
   carry F.

Intensity targets matter where the measurement error is comparable to the model error: weak data
near the resolution limit, and small differences between intensities such as Bijvoet differences
in phasing. That is where Read & McCoy's (2016) intensity likelihood came from. The main
refinement programs now offer intensity targets.

## 7. Alternatives considered

- **Sivia & David (1994):** a flat prior on |F| ≥ 0 in place of Wilson's, giving approximate
  closed forms and no Σ to estimate. Quoted from memory; check `~/Dropbox/manuscripts` before
  citing.
- **Intensity likelihoods (Read & McCoy 2016):** for phasing; for refinement on good
  small-molecule data, least squares on F² already does the same job.

## 8. References

All from memory except French1978, Hirshfeld1973, Capelli2014 and Davidson2022b, which are in the
TFVC paper's `references.bib`.

- French, S. & Wilson, K. (1978). Acta Cryst. A34, 517–525.
- Hirshfeld, F. L. & Rabinovich, D. (1973). Acta Cryst. A29, 510–513.
- Sivia, D. S. & David, W. I. F. (1994). Acta Cryst. A50, 703–714.
- Read, R. J. (1986). Acta Cryst. A42, 140–149.
- Pannu, N. S. & Read, R. J. (1996). Acta Cryst. A52, 659–668.
- Murshudov, G. N., Vagin, A. A. & Dodson, E. J. (1997). Acta Cryst. D53, 240–255.
- Read, R. J. & McCoy, A. J. (2016). Acta Cryst. D72, 375–387.
- Blessing, R. H. (1997). J. Appl. Cryst. 30, 421–436 (merging; for the merge item).
