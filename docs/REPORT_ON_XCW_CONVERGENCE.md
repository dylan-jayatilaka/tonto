# The X-ray constrained SCF: why it wanders, how stiff the data make it, and how to choose lambda

## 1. Summary

1. **The blow-up is a linear instability of the plain SCF step.** The constraint term makes
   the Fock matrix respond very strongly to a change in the density. Above a certain lambda an
   undamped step overshoots by more than it corrects, and each iteration is worse than the
   last by a fixed factor.
2. **That factor can be calculated before running anything**, from the orbitals, the orbital
   energies, and the reflections with their sigmas (section 3). For the ammonia restart job at
   lambda = 0.012 it predicts that a step mixing in more than 23.0% of the new density diverges.
   Measured: 23% converges, 24% diverges.
3. **No single reflection is to blame** in ammonia (section 5). The stiffness is spread over the
   strong low-angle reflections, all of which have F/sigma above 100.
4. **The same matrix gives the effective number of fitted parameters, the AIC, BIC and
   generalised cross-validation criteria for lambda, and a leave-one-out cross-validation from
   one converged run** (section 7). Checked against two real held-out refits: 7 to 8% low.
   Its diagonal is the leverage of crystallographic least squares (section 8).
5. **The cure is the Newton step with the constraint curvature included** (section 9). It
   needs no damping, converges in about ten iterations at every lambda from 0.012 to 4 on
   ammonia, and leaves the converged wavefunction unchanged: it alters only the path.
6. **With the cure the lambda scan runs out to where the criteria turn.** On ammonia the
   leave-one-out sum has its minimum at lambda 2.0 and generalised cross-validation at 3.2,
   where 34 to 36 of the 88 reflections' worth of parameters are in use; AIC with the sigmas
   trusted puts it at 0.08 and BIC at 0.024.

## 2. The instability

The job is `tests/long/nh3_x-ray-constrained-rhf-cluster-charge_cc-pVTZ_restart`: ammonia,
RHF/cc-pVTZ, 88 reflections, lambda = 0.012 then 0.016, started from a converged density. With
`output= YES` the first lambda shows:

```
 Iter  Lambda     GoF    Energy   <MO|M0>
    0  0.0120    2.53  -56.2167    1.0000   *Damping on
    1  0.0120    9.56  -55.9161    0.9575
    2  0.0120   35.99  -52.9207    0.6053
    3  0.0120   54.76  -27.0562    0.0049   *Damping was off
    4  0.0120   27.61  -50.8326    0.2870   *DIIS starts saving now
   ...
   10  0.0120   49.16  -14.6622    0.0001
   ...
   31  0.0120    0.84  -56.2039    0.9985
```

The GoF grows by a factor of about 3.8 per iteration from the first step. DIIS, which starts at
iteration 4, eventually recovers. The final answer is right.

The table says damping is on, but no density damping was applied: the routine that makes the
density only damped when an old density already existed, and only kept one when damping was
already on. Damping now mixes with the density it is given on entry, and every SCF with the
default settings takes a different path to the same answer.

The other three constrained test jobs show no such excursion: their GoF and energy move
smoothly to convergence at every lambda. They use much smaller multipliers, at most 0.0003
for `nh3_x-ray-constrained-rhf_cc-pVTZ` and its `_extinction` twin and 0.001 for the urea UHF
job, and at those values the overshoot of section 3 is far too small to matter.

## 3. Theory

All symbols are defined here.

- $`D`$ is the density matrix and $`E(\mathbf{D})`$ the Hartree-Fock energy.
- $`F_k`$ is the calculated structure factor of reflection $`k`$, $`F_k^{\mathrm{obs}}`$ the
  observed magnitude and $`\sigma_k`$ its standard uncertainty.
- $`\alpha`$ is the scale factor, refitted at every iteration.
- $`N_{\mathrm{refl}}`$ is the number of reflections and $`p`$ the number of fitted parameters.
- $`\lambda`$ is the multiplier.

The constrained SCF makes stationary

```math
L(\mathbf{D}) = E(\mathbf{D}) + \lambda\,\mathrm{GoF}^2(\mathbf{D}), \qquad
\mathrm{GoF}^2 = \frac{1}{N_{\mathrm{refl}}-p}\sum_k \frac{\left(\alpha |F_k| - F_k^{\mathrm{obs}}\right)^2}{\sigma_k^2}
\qquad (1)
```

so the matrix that is diagonalised is the Fock matrix plus $`\lambda\mathbf{C}`$, with

```math
\mathbf{C} = \frac{\partial\,\mathrm{GoF}^2}{\partial\mathbf{D}}
  = \frac{2}{N_{\mathrm{refl}}-p}\sum_k \frac{\alpha\left(\alpha |F_k| - F_k^{\mathrm{obs}}\right)}{\sigma_k^2}\,\mathbf{A}_k ,
\qquad \mathbf{A}_k = \frac{\partial |F_k|}{\partial\mathbf{D}}
\qquad (2)
```

This is what `MOLECULE.SCF:make_r_constraint` builds; $`\mathbf{A}_k`$ is the Fourier transform of a pair
of basis functions at the scattering vector of reflection $`k`$, projected on the phase of
$`F_k`$.

**One SCF step, linearised.** Suppose the input density is off the converged one by
$`\delta\mathbf{D}`$. Each $`|F_k|`$ is off by $`\mathrm{tr}\,(\mathbf{A}_k\,\delta\mathbf{D})`$, so by equation (2) the
constraint matrix is off by

```math
\lambda\,\delta\mathbf{C} = \lambda\,\frac{2\alpha^2}{N_{\mathrm{refl}}-p}\sum_k \frac{\mathbf{A}_k\;\mathrm{tr}\,(\mathbf{A}_k\,\delta\mathbf{D})}{\sigma_k^2}
\qquad (3)
```

Diagonalising with this error in the Fock matrix gives an output density with an error of its
own. To first order, and leaving out the change in the two-electron part of the Fock matrix, a
perturbation $`\mathbf{V}`$ mixes occupied orbital $`i`$ with virtual orbital $`a`$ by
$`-V_{ia}/(\varepsilon_a-\varepsilon_i)`$, where $`\varepsilon`$ are the orbital energies. For a
closed shell this changes the structure factor of reflection $`l`$ by

```math
\mathrm{tr}\,(\mathbf{A}_l\,\delta\mathbf{D}^{\mathrm{out}}) = -4\sum_{ia}\frac{(\mathbf{A}_l)_{ia}\,V_{ia}}{\varepsilon_a-\varepsilon_i}
\qquad (4)
```

Write the error in each structure factor in units of its sigma,
$`e_k = \alpha\,\mathrm{tr}\,(\mathbf{A}_k\,\delta\mathbf{D})/\sigma_k`$. Putting (3) into (4):

```math
\mathbf{e}^{\mathrm{out}} = -\lambda\,\mathbf{G}\,\mathbf{e}^{\mathrm{in}}, \qquad
\mathbf{G} = \frac{2}{N_{\mathrm{refl}}-p}\,\mathbf{B} \mathbf{B}^{T}, \qquad
B_{k,ia} = \frac{2\alpha\,(\mathbf{A}_k)_{ia}}{\sigma_k\sqrt{\varepsilon_a-\varepsilon_i}}
\qquad (5)
```

$`\mathbf{G}`$ is an $`N_{\mathrm{refl}} \times N_{\mathrm{refl}}`$ symmetric matrix with no negative eigenvalues. Call it the gain
matrix, and its largest eigenvalue $`\gamma`$. The minus sign is the overshoot: an error comes
back reversed and $`\lambda\gamma`$ times larger.

**The refitted scale.** Because $`\alpha`$ is refitted each step, an error in the structure
factors that is proportional to the structure factors themselves costs nothing. That error is
the vector $`\mathbf{u}`$ in the space of reflections whose $`k`$-th component is $`\alpha|F_k|/\sigma_k`$,
normalised to unit length; it is projected out of $`\mathbf{B}`$, row by row, before $`\mathbf{G}`$ is formed.
This lowers $`\gamma`$ for ammonia from 1071 to 642.

**The condition for convergence.** A step that mixes a fraction $`x`$ of the new density with
$`1-x`$ of the old multiplies the error along the stiffest direction by
$`1 - x\,(1+\lambda\gamma)`$. It shrinks only if

```math
\mathbf{x} < \frac{2}{1+\lambda\gamma}
\qquad (6)
```

With no damping, $`x = 1`$, the step fails once $`\lambda > 1/\gamma`$.

**It does not depend on the scale of the sigmas.** If every sigma is multiplied by $`s`$, the
lambda that gives the same wavefunction is multiplied by $`s^2`$ and $`\gamma`$ is divided by
$`s^2`$. The product $`\lambda\gamma`$ measures how many times stiffer the data term is than the
wavefunction's own resistance to change. It is a property of how hard the fit is pushed.

## 4. The check against measurement

`MOLECULE.SCF:put_constraint_stiffness` (`put_constraint_stiffness= TRUE` in the `scfdata=`
block) computes $`\mathbf{G}`$ from equation (5) and prints its largest eigenvalues, the limit of equation (6),
and the reflections with the largest diagonal elements. RHF only. For the ammonia job, from the
orbitals converged at lambda = 0.012:

| | |
|---|---|
| Largest gain per unit lambda, $`\gamma`$ | 642.5 |
| Sum of all gains per unit lambda | 2524 |
| Lambda where an undamped step fails, $`1/\gamma`$ | 0.00156 |
| Gain at lambda = 0.012 | 7.71 |
| Largest stable fraction of new density at 0.012 | 0.230 |

Measured with density damping only, DIIS switched off, and lambda = 0.012 throughout:

| Fraction of new density | Result |
|---|---|
| 0.50 | diverges, energy to -36 |
| 0.30 | diverges |
| 0.26 | diverges |
| 0.24 | diverges |
| 0.23 | converges, error falls by 1.3% per iteration |
| 0.22 | converges in about 90 iterations |
| 0.20 | converges |
| 0.15, 0.10, 0.05 | converge, more slowly |

At 0.23 the measured decay of 0.987 per iteration gives $`\lambda\gamma = 7.64`$ from
$`1 - x(1+\lambda\gamma)`$; the calculation gives 7.71. So leaving out the two-electron response
costs about 1% here.

The three quiet test jobs have lambda at most 0.0003, a gain of 0.2, far inside the limit.

## 5. Which reflections make it stiff

The diagonal element $`G_{kk}`$ is the gain reflection $`k`$ would give alone. The top of the
list for ammonia, per unit lambda:

| h k l | sin(theta)/lambda | F_exp | sigma | F/sigma | (F_pred - F_exp)/sigma | own gain | share of stiffest direction |
|---|---|---|---|---|---|---|---|
| 0 4 0 | 0.206 | 9.40 | 0.061 | 154 | -1.24 | 182 | 0.14 |
| 3 0 -2 | 0.186 | 6.12 | 0.056 | 109 | -1.94 | 169 | 0.01 |
| 0 -3 2 | 0.186 | 6.49 | 0.052 | 125 | 0.45 | 169 | 0.08 |
| -1 2 -1 | 0.126 | 8.40 | 0.057 | 147 | -1.26 | 144 | 0.04 |
| 0 3 1 | 0.163 | 7.89 | 0.057 | 138 | 2.58 | 126 | 0.00 |
| -2 0 -4 | 0.231 | 6.42 | 0.051 | 126 | -1.05 | 105 | 0.12 |
| -1 -3 3 | 0.225 | 8.11 | 0.055 | 147 | 0.16 | 87 | 0.09 |
| 1 0 2 | 0.115 | 20.94 | 0.119 | 176 | -1.03 | 87 | 0.04 |
| 1 2 0 | 0.115 | 2.86 | 0.084 | 34 | 1.59 | 77 | 0.01 |

- **No one reflection dominates.** The largest own gain is 182 against a total of 2524, and no
  reflection has more than 14% of the stiffest direction.
- **The stiff ones are the low-angle reflections with small absolute sigma**, about 0.05 to 0.06,
  whatever their size. The own gain goes as $`1/\sigma_k^2`$ times how easily the valence density
  moves that structure factor.
- **A reflection worth a second look has a large leverage and a large residual at once**: it
  can move the wavefunction a long way and is asking to. The measure of that is Cook's
  distance, $`D_k = r_k^2 H_{kk}/\big(p_{\mathrm{eff}}(1-H_{kk})^2\big)`$, with $`H_{kk}`$ the
  leverage of section 7 and $`r_k`$ the standardised residual: how far the whole fit moves
  when reflection $`k`$ is left out, in units of its own uncertainty. The usual threshold is
  $`4/N_{\mathrm{refl}}`$. For ammonia at lambda 0.012, 10 of the 88 reflections are above it, led by (0 3 1)
  at 0.68 and (3 0 -2) at 0.51 against a threshold of 0.045; 61 are below an eighth of it.
  The report prints the ten largest and a histogram in multiples of $`4/N_{\mathrm{refl}}`$.
- Since the result does not depend on the overall scale of the sigmas (section 3), this
  diagnostic can find a reflection whose sigma is too small *relative to the others*, not a set
  of sigmas that are all too small.

## 6. Damping and DIIS

Measured on the two-lambda job:

| Setting | Iterations at 0.012, 0.016 | Worst energy on the way |
|---|---|---|
| in test: mix 50% for 3 iterations, DIIS from 4 | 30, 20 | -29.6 |
| mix 15% for 3 iterations, DIIS saving from 1 | 18, 19 | -56.2013 |
| mix 15% for 6 iterations, DIIS from 6 | 19, 20 | -56.2013 |
| no damping, DIIS saving from 1 | 21, 21 | -56.03 |
| mix 15% throughout, with DIIS | 61, 54 | -56.2013 |

All reach GoF 0.78 and -56.2023. So strong damping for the first few steps, then DIIS, removes
the excursion. Damping kept on under DIIS only slows it.

The exact cure, which needs no damping at all, is in section 9.

## 7. The effective number of parameters, and the choice of lambda

**The definition.** In a restrained least-squares fit the effective number of parameters is
the trace of the hat matrix $`\mathbf{H}`$, the matrix that maps the observations to the fitted values.
Its diagonal element $`H_{kk}`$ says how far the prediction for reflection $`k`$ follows its own
observation: all the way for a freely fitted point, not at all for a point the model cannot
reach. It is the quantity that reduces to the ordinary parameter count for an unrestrained fit,
and to zero at lambda = 0, as Davidson *et al.* (2022) require.

**The hat matrix of the XCW.** Section 3 gave what one SCF step does to an error in the
structure factors. The same linearisation gives what a change in the observations does to the
converged structure factors. Move the observations by $`\delta\mathbf{o}`$, in units of their sigmas,
with the scale direction removed as before. The residuals in equation (2) change by
$`\mathbf{e} - \delta\mathbf{o}`$, where $`\mathbf{e}`$ is the change in the predictions, so the converged response
satisfies $`\mathbf{e} = -\lambda \mathbf{G}\,(\mathbf{e} - \delta\mathbf{o})`$, by equation (5), and

```math
\mathbf{e} = \mathbf{H}\,\delta\mathbf{o}, \qquad \mathbf{H} = \lambda \mathbf{G}\,(\mathbf{1}+\lambda \mathbf{G})^{-1}
\qquad (7)
```

$`\mathbf{H}`$ has the eigenvectors of $`\mathbf{G}`$ and eigenvalues $`\lambda\gamma_j/(\mathbf{1}+\lambda\gamma_j)`$. The
effective number of parameters is its trace, plus the $`p`$ parameters of the scale and
extinction model, which were projected out of $`\mathbf{G}`$:

```math
p_{\mathrm{eff}}(\lambda) = p + \sum_j \frac{\lambda\gamma_j}{1+\lambda\gamma_j}
\qquad (8)
```

Each direction in reflection space contributes between 0 and 1, and is half fitted when
$`\lambda\gamma_j = 1`$. So $`p_{\mathrm{eff}}`$ is $`p`$ at lambda = 0 and climbs as lambda
switches directions on, stiffest first. The undamped SCF begins to fail at
$`\lambda\gamma_{\max} = 1`$ (equation 6), which is where the first direction passes one half:
the instability and the first fitted parameter are the same event.

This is the trace formula of Appendix A of `docs/TASK_EXTINCTION_CORRECTION.md`, with the
Hessian of the energy replaced by its uncoupled form, the orbital energy differences. That
costs about 1% on the largest eigenvalue for ammonia (section 4), and it neglects a term
proportional to the residuals, which matters only where the fit is poor.

**The criteria.** Write $`r_k = (\alpha|F_k| - F_k^{\mathrm{obs}})/\sigma_k`$ for the
standardised residuals, $`\chi^2 = \sum_k r_k^2`$, and $`N_{\mathrm{refl}}`$ for the number of reflections.
Each criterion is a penalised misfit, to be minimised over lambda: $`\chi^2`$ falls as lambda
rises and $`p_{\mathrm{eff}}`$ rises.

```math
\mathrm{AIC} = \chi^2 + 2\,p_{\mathrm{eff}}, \qquad
\mathrm{BIC} = \chi^2 + p_{\mathrm{eff}}\ln N_{\mathrm{refl}}
\qquad (9)
```

Both assume the sigmas are correct: if every sigma is too small by a factor $`s`$,
$`\chi^2`$ is too large by $`s^2`$ while the penalty is not, and both pick too large a lambda.
With GoF values of 3 to 7 that is a real risk. Two forms do not depend on the overall scale of
the sigmas, because the variance scale is treated as a fitted quantity:

```math
\mathrm{AIC}_{\sigma} = N_{\mathrm{refl}}\ln\frac{\chi^2}{N_{\mathrm{refl}}} + 2\,p_{\mathrm{eff}}, \qquad
\mathrm{GCV} = \frac{N_{\mathrm{refl}}\chi^2}{(N_{\mathrm{refl}}-p_{\mathrm{eff}})^2}
\qquad (10)
```

GCV is generalised cross-validation, Golub, Heath and Wahba (1979), *Technometrics* **21**,
215. These two are the ones to use: the sigmas of a diffraction experiment are unreliable in
scale but useful in their relative values, and both criteria use only the relative values.
GCV is the default, for its crystallographic heritage, and the keyword `lambda_criterion=`
switches to the sigma-free AIC; the other three are kept for comparison. BIC penalises harder
than AIC and picks a smaller lambda.

**Leave-one-out without refitting.** A fit is called a linear smoother when its predictions are
a fixed linear map of the observations, $`\hat{\mathbf{o}} = \mathbf{H} \mathbf{o}`$, with $`\mathbf{H}`$ not depending on $`\mathbf{o}`$;
ordinary and ridge least squares are, and so is the linearised XCW by equation (7). For such a
fit the residual of reflection $`k`$ when it is left out is

```math
r_k^{(-k)} = \frac{r_k}{1 - H_{kk}}
\qquad (11)
```

This is the leaving-out-one lemma of Craven and Wahba, in the form given by Golub, Heath and
Wahba, *Technometrics* **21**, 215 (1979), their equation (2.3). The derivation is short.
Refit with reflection $`k`$ removed, and call its prediction $`\hat o_k^{(-k)}`$. If the
observation $`o_k`$ is now replaced by $`\hat o_k^{(-k)}`$ and the fit redone with all $`N_{\mathrm{refl}}`$
reflections, nothing changes: the extra point sits exactly on the fit and contributes no
residual, so the minimiser is the same. The prediction for $`k`$ is therefore the same too.
Linearity then gives $`\hat o_k^{(-k)} = \hat o_k + H_{kk}(\hat o_k^{(-k)} - o_k)`$, and
rearranging, $`o_k - \hat o_k^{(-k)} = (o_k - \hat o_k)/(1-H_{kk})`$, which is equation (11)
in units of sigma. It is exact for the linearised model, so a full leave-one-out
cross-validation at each lambda comes from one converged run:

```math
\mathrm{LOO} = \sum_k \left(\frac{r_k}{1-H_{kk}}\right)^2
\qquad (12)
```

This is the free R value of Brünger, *Nature* **355**, 472 (1992), taken to its limit: every
reflection is its own test set, which is the complete cross-validation he recommends when the
test set is small (*Methods in Enzymology* **277**, 366 (1997)), and it needs no partition
of the data. It should still be checked once against a real held-out refit. A reflection with a large
$`H_{kk}`$ and a large residual at once is the one worth a second look: it can move the
wavefunction a long way and is asking to.

**A lambda scan on ammonia.** The restart job, damping at 15% for three iterations, DIIS from
the first, three scans joined (step 0.0005 to 0.004, then 0.004 to 0.04, then 0.02 to 0.2). $`N_{\mathrm{refl}} = 88`$, $`p = 1`$. GoF is $`\sqrt{\chi^2/(N_{\mathrm{refl}}-1)}`$.

| lambda | $`\lambda\gamma_{\max}`$ | $`p_{\mathrm{eff}}`$ | $`\chi^2`$ | GoF | AIC | BIC | AIC$`_\sigma`$ | GCV | LOO | iterations |
|---|---|---|---|---|---|---|---|---|---|---|
| 0 | 0 | 1.0 | 851.9 | 3.13 | 853.9 | 856.4 | 201.8 | 9.90 | 851.9 | |
| 0.001 | 0.64 | 2.9 | 407.1 | 2.16 | 413.0 | 420.3 | 140.7 | 4.95 | 483.7 | |
| 0.002 | 1.28 | 4.3 | 256.5 | 1.72 | 265.0 | 275.5 | 102.7 | 3.22 | 338.4 | |
| 0.004 | 2.57 | 6.1 | 145.1 | 1.29 | 157.2 | 172.2 | 56.1 | 1.90 | 215.4 | |
| 0.008 | 5.14 | 8.4 | 82.1 | 0.97 | 98.9 | 119.6 | 10.6 | 1.14 | 134.7 | |
| 0.012 | 7.71 | 9.9 | 61.9 | 0.84 | 81.7 | 106.3 | -11.1 | 0.893 | 106.3 | |
| 0.016 | 10.3 | 11.1 | 52.3 | 0.78 | 74.4 | 101.9 | -23.7 | 0.777 | 92.6 | |
| 0.020 | 12.8 | 12.0 | 46.6 | 0.73 | 70.7 | 100.5 | -31.9 | 0.711 | 84.9 | |
| 0.024 | 15.4 | 12.8 | 42.8 | 0.70 | 68.5 | **100.3** | -37.7 | 0.667 | 80.1 | |
| 0.028 | 18.0 | 13.5 | 40.1 | 0.68 | 67.1 | 100.6 | -42.2 | 0.636 | 76.9 | |
| 0.040 | 25.7 | 15.2 | 34.9 | 0.63 | 65.2 | 102.7 | -51.1 | 0.578 | 71.7 | 49 |
| 0.060 | 38.4 | 17.1 | 30.2 | 0.59 | 64.3 | 106.6 | -60.0 | 0.528 | 68.5 | 58 |
| 0.080 | 51.2 | 18.4 | 27.4 | 0.56 | **64.25** | 109.9 | -65.9 | 0.498 | 67.2 | 77 |
| 0.100 | 63.9 | 19.5 | 25.4 | 0.54 | 64.4 | 112.7 | -70.4 | 0.476 | 66.5 | 91 |
| 0.120 | 76.6 | 20.4 | 23.9 | 0.52 | 64.6 | 115.1 | -74.1 | 0.459 | 66.0 | 128 |
| 0.140 | 89.3 | 21.1 | 22.6 | 0.51 | 64.8 | 117.1 | -77.3 | 0.445 | **65.65** | 251 |
| 0.160 | 101.9 | 21.8 | 21.8 | 0.50 | 65.3 | 119.2 | -79.4 | 0.436 | 65.66 | not converged in 300 |

At 0.18 the SCF is still not converged after 300 iterations and at 0.2 it has blown up, so
the row for 0.16 is the last one to trust and even it is not fully converged.

What the scan says:

- **Where the criteria put the minimum.** BIC at 0.024, AIC at 0.08, leave-one-out at about
  0.15, GCV and AIC$`_\sigma`$ beyond 0.16. The two that trust the sigmas disagree with each
  other by a factor of three, and with the sigma-free ones by more. This is the expected
  behaviour of a penalty that is fixed against a misfit whose scale is uncertain, not a defect
  in any of them. The leave-one-out sum is the one with a clear, if shallow, minimum.
- **The sigmas of this data set look too large, not too small**: GoF falls below 1 at lambda
  0.008, with only 8 effective parameters out of 88. That is the opposite of the GoF 3 to 7
  worry in section 7, and it is why AIC and BIC come out so differently here.
- **The fit keeps paying for its parameters a long way out.** Each extra effective parameter
  is bought for less and less $`\chi^2`$, but $`\chi^2`$ per parameter stays above 1 until about
  lambda 0.1. The effective parameter count climbs slowly: 22 of 88 at lambda 0.16, with
  $`\lambda\gamma_{\max} = 102`$. The stiff directions saturate early and the rest are switched
  on one by one.
- **The plain SCF with damping and DIIS cannot reach the region the sigma-free criteria point
  at.** Iterations rise from 49 at lambda 0.04 to 251 at 0.14 and the SCF fails above that,
  with the stable fraction of equation (6) down to 2%. The cure of section 9 removes this limit.
- **The leave-one-out sum from one converged run at each lambda** replaces a k-fold
  cross-validation, to the accuracy checked next.

**The leave-one-out formula checked against a real held-out refit.** Ammonia restart job,
lambda = 0.012. One reflection at a time was held out by giving it a sigma of
1.0 (weight 300 times smaller than its neighbours, so it still gets a predicted structure
factor), the job was run again, and the prediction was compared with the observation using the
original sigma.

| Held out | residual in full fit | $`H_{kk}`$ | equation (11) | refit |
|---|---|---|---|---|
| (0 3 1) | 2.58 | 0.383 | 4.18 | 4.55 |
| (3 0 -2) | -1.94 | 0.434 | -3.43 | -3.67 |

The formula gives the right size and sign and is 7 to 8% low on both. Both errors have the
same sign, which points at the uncoupled orbital response underestimating $`H_{kk}`$ rather
than at noise; the full coupled response, or the residual term left out, would be the next
refinement if the 8% matters. For choosing lambda it does not.


## 8. Leverage, and the halting methods of Davidson, Grabowsky and Jayatilaka

**Leverage.** In crystallographic least squares the leverage of an observation is the diagonal
element of the projection matrix $`\mathbf{P} = \mathbf{A}(\mathbf{A}^{T}\mathbf{W}\mathbf{A})^{-1}\mathbf{A}^{T}\mathbf{W}`$, with $`\mathbf{A}`$ the design matrix
and $`\mathbf{W}`$ the weights: it says how far the calculated value of that observation follows its
observed value, lies between 0 and 1, and sums to the number of parameters (Parsons, Wagner,
Presly, Wood and Cooper, *J. Appl. Cryst.* **45**, 417 (2012), after Prince). Restraints enter
as extra rows of $`\mathbf{A}`$ and get leverages of their own.

The hat matrix of equation (7) is the same object for the XCW. Writing $`\mu = (N_{\mathrm{refl}}-p)/2\lambda`$,

```math
\mathbf{H} = \lambda \mathbf{G}\,(\mathbf{1}+\lambda \mathbf{G})^{-1} = \mathbf{B}\,(\mathbf{B}^{T}\mathbf{B} + \mu\,\mathbf{1})^{-1}\mathbf{B}^{T}
\qquad (13)
```

which is the projection matrix of a least-squares fit with design matrix $`\mathbf{B}`$, whose rows are
the reflections in units of their sigmas and whose columns are the occupied-virtual orbital
rotations scaled by the inverse root of their orbital energy gap, restrained by a ridge of
strength $`\mu`$. The energy is the restraint: its uncoupled Hessian, the orbital energy
differences, becomes the identity after the scaling, so the XCW is ridge regression in the
sense of Hoerl and Kennard, and the ridge parameter is $`(N_{\mathrm{refl}}-p)/2\lambda`$. So:

- $`H_{kk}`$ is the leverage of reflection $`k`$, and its trace is the number of parameters the
  data determine. The normalised leverage of Parsons *et al.* is $`H_{kk}`$ divided by
  $`(p_{\mathrm{eff}}-p)/N_{\mathrm{refl}}`$.
- The reflections of high leverage in ammonia are the strong low-angle ones with small absolute
  sigma (section 5), where for the alanine refinement of Parsons *et al.* they are the
  moderately weak reflections. The difference is in what is being fitted: the XCW adjusts the
  valence density, which moves the low-angle structure factors most.
- The parameter-wise sensitivities $`T_{ij}`$ of Parsons *et al.*, the influence of observation
  $`i`$ on parameter $`j`$, are here row $`i`$ of $`\mathbf{B}`$ times column $`j`$ of
  $`(\mathbf{B}^{T}\mathbf{B}+\mu)^{-1}`$, with the orbital rotations as parameters. They are not printed: a
  single rotation is not a quantity anyone asks about. The sensitivity of a density feature, a
  bond charge or an atomic charge, would be the useful form, and is one further contraction.
- Cook's distance, which Merli and co-workers use to find outliers in refinement, is the
  $`D_k`$ of section 5, printed for every reflection, listed for the ten largest, and binned.

**The halting methods of Davidson, Grabowsky and Jayatilaka**, *Acta Cryst.* **B78**, 397
(2022), section 2, take the GoF against lambda curve as their only input, and halt the scan at
a lambda read off it. Three are given:

1. A power function, $`\mathrm{GoF}^2 = \mathbf{A}\lambda^{\mathbf{B}}`$, fitted to the scan, halted at
   $`\lambda = 1`$, where $`\mathrm{GoF}^2 = \mathbf{A}`$. The argument for 1 is that there the
   effective forces of the energy and of the fit are equal; the authors call it not very strong.
2. The asymptotic form of Tozer, Ingamells and Handy, equation (14) below, fitted to the
   last three (TIH3) or six (TIH6) converged points, with $`D`$ read as the error term and
   the halting value $`\lambda_{\mathrm{TIH}}`$ the lambda where the two correction terms are
   equal.
3. The smallest halting lambda over several data sets of the same crystal.

```math
\mathrm{GoF}^2 = D + E\lambda^{-2} + F\lambda^{-4}, \qquad
\lambda_{\mathrm{TIH}} = |F/D|^{1/4}
\qquad (14)
```

In every case the scan ends where convergence fails, and that point, $`\lambda_{\max}`$, is used
both as the upper limit of the fit and, in the third method, as a halting value in its own
right. Three comments follow from the present work.

- **The convergence failure is a property of the solver, not of the data.** It sets in where
  $`\lambda\gamma_{\max}`$ passes the value the solver can hold (1 for an undamped step,
  about 100 for damping plus DIIS, equation 6), and $`\gamma_{\max}`$ scales as one over sigma
  squared. That is exactly the behaviour of Table 2 of the paper, where scaling two sigmas by
  $`\eta`$ moves $`\lambda_{\max}`$: the two reflections are the stiffest ones. With the cure
  of section 9 there is no $`\lambda_{\max}`$, and the third method, and the choice of fitting
  range in the other two, lose their anchor.
- **The three-point formula is a local extrapolation, and its answer moves with the points it
  is given.** For ammonia the three points at lambda 0.032, 0.036 and 0.040 give
  $`D = 0.31`$ and $`\lambda_{\mathrm{TIH3}} = 0.019`$; the points at 0.32, 0.36 and 0.40
  give $`D = 0.14`$ and 0.20; the points at 3.2, 3.6 and 4.0 give $`D = 0.09`$ and 1.6. The
  extrapolated error term keeps falling because the curve is not in its asymptotic region at
  any of these, and $`F`$ is poorly determined by three close points, as the authors note for
  their TIH3 fits. The leave-one-out minimum is at 2.0 and the AIC minimum at 0.08 (section
  9). The formula is printed beside the other statistics so the two can be compared on every
  job.
- **The criteria of section 7 answer a different question.** The halting methods ask where the
  fit stops improving on its own curve. AIC, GCV and the leave-one-out sum ask where it stops
  improving *prediction* of reflections it has not seen, with the number of parameters counted.
  Only the second kind can say that a lambda is too large.

## 9. The cure: the Newton step with the constraint curvature

**The plain step.** Every iteration of the constrained SCF diagonalises the effective Fock
matrix

```math
\mathbf{F}_{\mathrm{eff}}(\mathbf{D}) = \mathbf{F}(\mathbf{D}) + \lambda\,\mathbf{C}(\mathbf{r}), \qquad
\mathbf{C}(\mathbf{r}) = \frac{2}{N_{\mathrm{refl}}-p}\sum_k \frac{\alpha_k}{\sigma_k}\, r_k\, \mathbf{A}_k
\qquad (15)
```

where $`\mathbf{F}(\mathbf{D})`$ is the ordinary Fock matrix, $`r_k = (\alpha|F_k| - F_k^{\mathrm{obs}})/\sigma_k`$
are the standardised residuals of the density $`D`$, and $`\mathbf{C}(\mathbf{r})`$ is equation (2) written in
terms of them: a linear function of the vector $`\mathbf{r}`$. Write $`\mathbf{g}`$ for the occupied-virtual
block of $`\mathbf{F}_{\mathrm{eff}}(\mathbf{D})`$ in the current orbitals, the orbital gradient, and
$`\Delta_{ia} = \varepsilon_a - \varepsilon_i`$ for the orbital energy differences. A change of the
occupied orbitals is a rotation, new orbital coefficients $`\mathbf{c}\,e^{\boldsymbol{\boldsymbol{\kappa}}}`$ from the old $`c`$,
with $`\boldsymbol{\kappa}`$ antisymmetric,
whose independent elements $`\kappa_{ia}`$ mix occupied orbital $`i`$ with virtual orbital
$`a`$. By first-order perturbation theory the diagonalisation rotates by

```math
\kappa_{ia} = -\frac{g_{ia}}{\Delta_{ia}}
\qquad (16)
```

and moves each residual, to the same order, by

```math
\delta r_k = \sum_{ia}\frac{\partial r_k}{\partial\kappa_{ia}}\,\kappa_{ia}
           = -\sum_{ia} 4\,\frac{\alpha_k}{\sigma_k}\,(\mathbf{A}_k)_{ia}\,\frac{g_{ia}}{\Delta_{ia}}
           = -2\,(\mathbf{B}\tilde{\mathbf{g}})_k,
\qquad \tilde g_{ia} = \frac{g_{ia}}{\sqrt{\Delta_{ia}}}
\qquad (17)
```

with the $`\mathbf{B}`$ of equation (5), since $`\partial|F_k|/\partial\kappa_{ia} = 4(\mathbf{A}_k)_{ia}`$ for a
closed shell. Equation (17) is the move the plain step makes in the space of reflections, and
it is the overshoot of section 3 seen from the other side.

**The Newton step.** Expand $`L = E + \lambda\,\mathrm{GoF}^2`$ to second order in the
rotations. For a closed shell the energy's gradient is $`4g_{ia}`$ and its Hessian, in the
uncoupled approximation that keeps only the orbital energy differences, is $`4\Delta_{ia}`$ on
the diagonal. The fit term's Hessian follows from $`\mathrm{GoF}^2 = \frac{1}{N_{\mathrm{refl}}-p}\sum_k r_k^2`$
by differentiating twice and keeping only the products of first derivatives, the Gauss-Newton
form, since the term with the second derivative of $`r_k`$ is weighted by the residual and
vanishes at a perfect fit:

```math
\frac{\partial^2 L}{\partial\kappa_{ia}\,\partial\kappa_{jb}}
 = 4\Delta_{ia}\,\delta_{ia,jb}
 + \lambda\,\frac{2}{N_{\mathrm{refl}}-p}\sum_k \frac{\partial r_k}{\partial\kappa_{ia}}\frac{\partial r_k}{\partial\kappa_{jb}}
 = 4\sqrt{\Delta_{ia}}\left(\mathbf{1} + \mu \mathbf{B}^{T}\mathbf{B}\right)_{ia,jb}\sqrt{\Delta_{jb}}
\qquad (18)
```

with $`\mu = 2\lambda/(N_{\mathrm{refl}}-p)`$, using $`\partial r_k/\partial\kappa_{ia} = 4(\alpha_k/\sigma_k)(\mathbf{A}_k)_{ia}
= 2\sqrt{\Delta_{ia}}\,B_{k,ia}`$ from the definition of $`\mathbf{B}`$ in equation (5). The Newton step
$`\boldsymbol{\kappa} = -(\partial^2 L)^{-1}\,\partial L`$ is then, in the scaled variables
$`x_{ia} = \sqrt{\Delta_{ia}}\,\kappa_{ia}`$,

```math
\mathbf{x} = -\left(\mathbf{1} + \mu \mathbf{B}^{T}\mathbf{B}\right)^{-1}\tilde{\mathbf{g}}
\qquad (19)
```

The matrix to invert has the size of the number of orbital rotations, but its second part has
rank $`N_{\mathrm{refl}}`$, the number of reflections, so the Woodbury identity (Henderson and Searle, *SIAM
Review* **23**, 53 (1981)),

```math
\left(\mathbf{1} + \mu \mathbf{B}^{T}\mathbf{B}\right)^{-1} = \mathbf{1} - \mu \mathbf{B}^{T}\left(\mathbf{1} + \mu \mathbf{B}\mathbf{B}^{T}\right)^{-1}\mathbf{B}
\qquad (20)
```

moves the inverse into the space of reflections. Its special case
$`(\mathbf{1} + \mu \mathbf{B}^{T}\mathbf{B})^{-1}\mathbf{B}^{T} = \mathbf{B}^{T}(\mathbf{1} + \mu \mathbf{B}\mathbf{B}^{T})^{-1}`$ is the push-through identity used
for equation (13). Since $`\mu \mathbf{B}\mathbf{B}^{T} = \lambda \mathbf{G}`$,

```math
\mathbf{x} = -\tilde{\mathbf{g}} + \mu\,\mathbf{B}^{T}(\mathbf{1}+\lambda \mathbf{G})^{-1}\mathbf{B}\tilde{\mathbf{g}}
\qquad (21)
```

the plain step with its reflection-visible part reduced.

**The matrix that is diagonalised.** Equation (21) is not applied as a rotation. Instead,
consider diagonalising the effective Fock matrix with a second constraint term added,

```math
\mathbf{F}_{\mathrm{eff}}(\mathbf{D}) + \lambda\,\mathbf{C}(\mathbf{y}) = \mathbf{F}(\mathbf{D}) + \lambda\,\mathbf{C}(\mathbf{r} + \mathbf{y})
\qquad (22)
```

where $`\mathbf{y}`$ is a vector in the space of reflections and $`\mathbf{C}(\mathbf{y})`$ is equation (15) built from
$`\mathbf{y}`$ in place of $`\mathbf{r}`$, which is allowed because $`C`$ is linear in its argument. Its orbital
gradient is $`\mathbf{g} + \lambda\,\mathbf{C}(\mathbf{y})_{ia}`$, and in scaled form the added part is

```math
\frac{\lambda\,\mathbf{C}(\mathbf{y})_{ia}}{\sqrt{\Delta_{ia}}}
 = \frac{2\lambda}{N_{\mathrm{refl}}-p}\sum_k \frac{\alpha_k}{\sigma_k}\,\frac{(\mathbf{A}_k)_{ia}}{\sqrt{\Delta_{ia}}}\,y_k
 = \frac{\mu}{2}\,(\mathbf{B}^{T}\mathbf{y})_{ia}
\qquad (23)
```

so by equation (16) the plain step of the matrix (22) is $`\mathbf{x} = -\tilde{\mathbf{g}} - \frac{\mu}{2}\mathbf{B}^{T}\mathbf{y}`$.
This is the Newton step (21) when

```math
\mathbf{y} = -2\,(\mathbf{1}+\lambda \mathbf{G})^{-1}\mathbf{B}\tilde{\mathbf{g}} = (\mathbf{1}+\lambda \mathbf{G})^{-1}\,\delta\mathbf{r}
\qquad (24)
```

with $`\delta\mathbf{r}`$ the plain-step move of equation (17). So $`\mathbf{y}`$ is that move with each stiff
direction reduced by its own factor $`1/(\mathbf{1}+\lambda\gamma_j)`$, the fraction that cancels its
overshoot, and the cure is: at every iteration, after the constraint matrix is added, compute
$`\mathbf{y}`$ from the gradient and add $`\lambda \mathbf{C}(\mathbf{y})`$ to the Fock matrix before it is diagonalised.
Nothing else in the SCF changes, and DIIS takes care of what the reflections cannot see. At
the converged wavefunction $`\mathbf{g} = 0`$, so $`\mathbf{y} = 0`$ and the fixed point is the ordinary one.

A probe step, that is a trial diagonalisation whose result is used to infer the fixed point,
does not work: far from convergence at large lambda the plain step is so far outside the linear
regime that its result carries no information, and the iteration settles on a wrong fixed point.
Equation (24) uses the gradient, which is linear by construction.

**What the method is, in plain terms.** The plain SCF step is a Newton step on the energy
alone: it knows the curvature of the energy, through the orbital energy gaps, and nothing of
the curvature of the fit term. At large lambda the fit term's curvature is the larger, so the
plain step overshoots in every direction the data see. The corrected step is the Newton step
with both curvatures. Because the fit term involves only $`N_{\mathrm{refl}}`$ reflections, its curvature has
rank $`N_{\mathrm{refl}}`$, and the Woodbury identity turns the Newton step into the plain step plus a
correction that lives entirely in the $`N_{\mathrm{refl}}`$-dimensional space of reflections. That is equation
(20). Nothing about the orbitals or the integrals changes; the correction enters as a second
constraint matrix built from the vector $`\mathbf{y}`$.

**The converged wavefunction is unchanged.** The constraint matrix is exactly linear in the
residual vector it is built from, so the fixed points of the corrected iteration are the
orbitals in which $`\tilde{\mathbf{g}} - \mu \mathbf{B}^{T}(\mathbf{1}+\lambda \mathbf{G})^{-1}\mathbf{B}\tilde{\mathbf{g}} = 0`$, and
by equation (20) that is $`(\mathbf{1} + \mu \mathbf{B}^{T}\mathbf{B})^{-1}\tilde{\mathbf{g}} = 0`$, which holds
only when $`\tilde{\mathbf{g}} = 0`$: the ordinary XCW stationarity condition. The approximations in
$`\mathbf{G}`$, the uncoupled response and the neglected residual term, change the preconditioner and
so the path, never the fixed point. Measured: the same energies and GoF as the plain SCF at
every lambda where the plain SCF converges.

**Orthonormality.** The orbitals are still produced by diagonalising a symmetric matrix in the
orthonormalised basis, as in every iteration of every SCF in the code: the correction only
adds the symmetric matrix $`\lambda \mathbf{C}(\mathbf{y})`$ to the Fock matrix before the diagonalisation. So
the orbitals are orthonormal exactly, not to first order. The Newton step of equation (19)
is a description of what that diagonalisation does to first order, not a rotation that is
applied.

**Is it a change of the sigmas?** Not of individual reflections. In the eigenbasis of $`\mathbf{G}`$
the move of the residuals is reduced direction by direction, by $`1/(\mathbf{1}+\lambda\gamma_j)`$:
a stiff direction is usually a combination of several strong low-angle reflections. If
$`\mathbf{G}`$ were diagonal in the reflections the step would be the one obtained with every sigma
enlarged by the factor $`(\mathbf{1}+\lambda\gamma_k)^{1/2}`$, for the update only. Either way the quantity being
made stationary, and the sigmas in it, are untouched.

**Not only for the XCW.** The structure is a penalty term that is a sum of squares over a
small number of quantities linear in the density: a few hundred or thousand reflections against
tens of thousands of orbital rotations. Any such term has a low-rank curvature and the same
cure, with its own $`\mathbf{B}`$: a wavefunction fitted to magnetic structure factors or to Compton
profiles, a density fitted to a reference density on a grid, an SCF with several density
constraints. The ordinary two-electron part of the SCF Hessian is not low rank, which is why
it is handled by DIIS and level shifts and not by this.

**Cost, as implemented.** Each iteration remakes the structure factor derivatives over the
occupied-virtual rotations from the current orbitals, which is one pass over the shell pairs,
the same work as a constraint build, plus a transformation costing reflections times the
square of the basis size times the number of virtuals; forms the gain matrix, reflections
squared times occupied-virtual pairs; diagonalises it, reflections cubed; and builds one more
constraint matrix. For ammonia, 88 reflections and 80 basis functions, the whole job with the
correction and no damping takes less CPU time than the plain SCF with damping and DIIS,
because it needs half the iterations. For a molecule with hundreds of basis functions and
thousands of reflections the derivative array and the eigenproblem would dominate an
iteration, and neither is needed. The correction only ever uses $`\mathbf{G}`$ in the solve
$`(\mathbf{1}+\lambda\mathbf{G})\mathbf{y} = \delta\mathbf{r}`$ of equation (24), and
$`\mathbf{G} = \mu\mathbf{B}\mathbf{B}^{T}`$ acts on a vector through two products the code already
has: $`\mathbf{B}\mathbf{v}`$ is the set of structure factors of the transition density made
from an occupied-virtual vector $`\mathbf{v}`$, scaled by $`2\alpha_k/\sigma_k`$, and $`\mathbf{B}^{T}\mathbf{w}`$
is the occupied-virtual block of the constraint matrix built from a reflection vector
$`\mathbf{w}`$. Conjugate gradients on equation (24) then needs one of each per iteration and
converges in a few tens of iterations, with the diagonal $`1+\lambda G_{kk}`$ as
preconditioner. A power series in $`\lambda\mathbf{G}`$ would not do, since it diverges exactly
where the cure is needed, $`\lambda\gamma_{\max} > 1`$. The effective number of parameters is
the trace of $`\mathbf{H}`$, which the same solves give by Hutchinson's estimator, the average of
$`\mathbf{z}^{T}\mathbf{H}\mathbf{z}`$ over random sign vectors $`\mathbf{z}`$, or by the Lanczos
quadrature of Golub and Meurant, which gives $`\sum_j f(\gamma_j)`$ for any $`f`$ from the
same matrix-vector products; the leverages $`H_{kk}`$ come the same way, a diagonal estimator in
place of the trace. The eigenvalue problem is only needed for the full stiffness report, and
only for a few thousand reflections, where it is cheap.

**The matrix-free form, as implemented** (`use_matrix_free_stiffness= TRUE`): the two products
are a structure factor evaluation of a symmetrised transition density and a constraint build
from a reflection vector; the solve is conjugate gradients without a preconditioner, stopped at
a relative residual of $`10^{-4}`$; $`p_{\mathrm{eff}}`$ and the leverages come from 20 sign
vectors; the largest gain from power iteration. Against the explicit route on ammonia:

| | explicit | matrix-free |
|---|---|---|
| iterations at lambda 0.012, 0.4 | 9, 10 | 9, 10 |
| energy and GoF | same | same |
| largest gain per unit lambda at 0.4 | 633.631 | 633.630 |
| $`p_{\mathrm{eff}}`$ at 0.4 | 26.13 | 26.05 |
| GCV at 0.4 | 0.3576 | 0.3568 |
| leave-one-out sum at 0.4 | 60.9 | 88.5 |
| conjugate gradient iterations per SCF iteration at 0.012, 0.4 | | 10, 28 |
| CPU time of the job at 0.4 | 4 s | 86 s |

The correction, $`p_{\mathrm{eff}}`$, GCV and the sigma-free AIC come out the same. The
leverages do not: 20 sign vectors give each $`H_{kk}`$ only to about a tenth, and the
leave-one-out residual $`r_k/(1-H_{kk})`$ magnifies that where $`H_{kk}`$ is near 1, so the
leave-one-out sum and Cook's distances from the matrix-free route need many more samples or a
better estimator and should not be read as they stand. On a molecule this small the explicit
route is twenty times cheaper, since one pass over the shell pairs makes every derivative at
once while each conjugate gradient iteration costs a structure factor evaluation and a
constraint build; the matrix-free form is for the case where the derivatives cannot be stored.
Its cost per SCF iteration is the conjugate gradient count times those two, and the count
grows with $`\lambda\gamma_{\max}`$. A diagonal preconditioner does not bring it down: with the
exact diagonal of $`\mathbf{G}`$ the count on ammonia goes from 10 to 12 at lambda 0.012 and
from 28 to 52 at 0.4, because the stiff directions of $`\mathbf{G}`$ are combinations of many
strong reflections and the matrix is nowhere near diagonal. What fits this matrix is
deflation by its leading eigenvectors, carried from one iteration to the next; that is the
open item in `docs/TASK_XCW_CONVERGENCE.md`.

**A limit.** The step is a linearisation about the current orbitals. From the converged
lambda 0.012 density the correction takes a jump to lambda 0.4, where $`\lambda\gamma_{\max}`$
is 253, in ten iterations; a jump straight to lambda 4, where it is 2500, diverges. Stepping
lambda up from 0.4 to 4 in steps of 0.4 converges at every point in about ten iterations.

**Results.** The ammonia restart job, no damping, no DIIS, the correction alone, against the
plain SCF with its best damping and DIIS settings (section 6):

| lambda | lambda times largest gain | iterations with the correction | plain SCF with damping and DIIS |
|---|---|---|---|
| 0.012 | 7.7 | 9 | 17 |
| 0.14 | 89 | 11 | 251 |
| 0.20 | 127 | 11 | not converged in 300 |
| 0.40 | 253 | 10 | diverges |

The converged energies and GoF agree with the plain SCF wherever that converges. The first
corrected step already lands close: at lambda 0.14 the GoF goes from 2.53 to 0.54 in one
iteration, where the plain step took it to 52.8 and the overlap with the starting orbitals to
0.0035. Adding damping at 15% for three iterations and DIIS on top gives 15 to 17 iterations:
the damping only slows it. The iteration count no longer depends on lambda.

With the correction the lambda scan of section 7 continues past the old failure point:

| lambda | $`\lambda\gamma_{\max}`$ | $`p_{\mathrm{eff}}`$ | $`\chi^2`$ | GoF | AIC | BIC | AIC$`_\sigma`$ | GCV | LOO | iterations |
|---|---|---|---|---|---|---|---|---|---|---|
| 0.04 | 25.7 | 15.2 | 34.9 | 0.63 | 65.2 | 102.7 | -51.1 | 0.578 | 71.7 | 10 |
| 0.08 | 51.2 | 18.4 | 27.4 | 0.56 | **64.25** | 109.9 | -65.9 | 0.498 | 67.2 | 10 |
| 0.12 | 76.6 | 20.4 | 23.9 | 0.52 | 64.6 | 115.1 | -74.1 | 0.459 | 66.0 | 10 |
| 0.16 | 102 | 21.8 | 21.6 | 0.50 | 65.1 | 119.0 | -80.1 | 0.433 | 65.3 | 10 |
| 0.20 | 127 | 22.8 | 20.0 | 0.48 | 65.6 | 122.1 | -84.9 | 0.413 | 64.6 | 10 |
| 0.40 | 253 | 26.1 | 15.6 | 0.42 | 67.8 | 132.5 | -100.2 | 0.358 | 60.9 | 10 |
| 0.80 | 504 | 29.4 | 12.4 | 0.38 | 71.2 | 144.0 | -113.8 | 0.317 | 54.8 | |
| 1.2 | 752 | 31.3 | 11.1 | 0.36 | 73.6 | 151.1 | -119.8 | 0.303 | 51.3 | |
| 1.6 | 998 | 32.6 | 10.4 | 0.35 | 75.5 | 156.2 | -123.0 | 0.297 | 49.7 | |
| 2.0 | 1243 | 33.6 | 9.9 | 0.34 | 77.1 | 160.3 | -125.0 | 0.2945 | **49.10** | |
| 2.4 | 1486 | 34.4 | 9.6 | 0.33 | 78.4 | 163.6 | -126.4 | 0.2932 | 49.12 | |
| 2.8 | 1729 | 35.1 | 9.3 | 0.33 | 79.5 | 166.4 | -127.5 | 0.2927 | 49.5 | |
| 3.2 | 1972 | 35.7 | 9.1 | 0.32 | 80.4 | 168.8 | -128.3 | **0.2926** | 50.1 | |
| 3.6 | 2216 | 36.2 | 8.9 | 0.32 | 81.3 | 171.0 | -129.0 | 0.2927 | 50.9 | |
| 4.0 | 2462 | 36.7 | 8.8 | 0.32 | 82.1 | 172.9 | -129.5 | 0.2930 | 51.8 | |

Every point converged. Where the criteria put the minimum on ammonia: BIC at 0.024, AIC at
0.08, the leave-one-out sum at 2.0, generalised cross-validation at 3.2, and the sigma-free
AIC still falling at 4. The two that trust the sigmas stop two orders of magnitude earlier
than the two that do not, which is what section 7 predicts for a data set whose sigmas are
too large: GoF is below 1 from lambda 0.008 on. At the leave-one-out minimum 34 of the 88
reflections' worth of parameters are in use and the GoF is 0.34. A data set with believable
sigmas is needed before a rule about where to stop is set; which criterion to read is
decided: GCV, or the sigma-free AIC.

## 10. Keywords

In the `scfdata=` block:

- `put_constraint_stiffness= TRUE` prints, at every converged lambda, the largest gains, the
  stable damping fraction, the effective number of parameters, the four criteria of equations
  (9) and (10), the leave-one-out sum and the three-point extrapolation of section 8, for
  every reflection its own gain, its leverage, its leave-one-out residual and its Cook's
  distance, the ten largest Cook's distances and a histogram of them. The effective
  number of parameters, the sigma-free AIC, the GCV, the leave-one-out sum and the three-point
  lambda are also printed with the structure factor statistics in the SCF results.
- `use_stiffness_correction= TRUE` switches on the correction of section 9.
- `use_matrix_free_stiffness= TRUE` uses the matrix-free form of section 9 for the correction
  and the statistics; `stiffness_cg_tolerance= 1e-4` is its conjugate gradient stopping
  residual, relative, `stiffness_samples= 20` the number of sign vectors in the trace
  estimator, and `stiffness_preconditioner= none` or `diagonal` the preconditioner, the
  diagonal being made exactly in batches once per lambda and slower on ammonia. The own gains, the share of the stiffest mode and the eigenvalues are not made in
  this form, and the leverages are estimates.
- `lambda_criterion= gcv` chooses the criterion whose value and running minimum over the
  lambda scan are printed with the statistics: `gcv` (the default), `aic_sigma`, `aic`,
  `bic` or `loo`. The lambda with the smallest value so far is the optimum lambda.

Both are for restricted wavefunctions and the two-centre partition models.
