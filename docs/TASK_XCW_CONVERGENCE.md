# Why the X-ray constrained SCF wanders and blows up

A working document. Opened 2026-10-07 on branch `xcw-wandering`. It replaces the entry
*Known-flaky: x-ray-constrained SCF convergence wanders violently* in `TASKS_AND_HISTORY.md`,
which recorded the symptom and three leads.

## 1. Summary

1. **Density damping has never been applied, in any SCF.** `MOLECULE.BASE:make_SCF_density_mx`
   switched damping on only if an old density matrix already existed, and saved the old density
   only if damping was on or an incremental Fock build was in use. With the incremental build off
   (the default since 2026-09-13) neither ever happens. The iteration table still printed
   `*Damping on`. A water SCF gives the same iterations for `damp_factor= 0.2` and `0.9`.
2. **The blow-up is a plain linear instability of the SCF step**, of the kind called charge
   sloshing in metals. The constraint term makes the Fock matrix respond very strongly to a change
   in the density. Above a certain lambda an undamped step overshoots by more than it corrects,
   and each iteration is worse than the last by a fixed factor.
3. **That factor can be calculated before running anything**, from the orbitals, the orbital
   energies, and the reflections with their sigmas (section 3). For the ammonia restart job at
   lambda = 0.012 it predicts that a step mixing in more than 23.0% of the new density diverges.
   Measured: 23% converges, 24% diverges.
4. **No single reflection is to blame** in ammonia (section 5). The stiffness is spread over the
   strong low-angle reflections, all of which have F/sigma above 100.
5. **Nothing is merged and no default is changed.** The damping repair changes the path of every
   SCF that uses the default damping, so it is a decision (section 7).

## 2. What was seen

The job is `tests/long/nh3_x-ray-constrained-rhf-cluster-charge_cc-pVTZ_restart`: ammonia,
RHF/cc-pVTZ, 88 reflections, lambda = 0.012 then 0.016, started from a converged density. Run in
the Mac release build with `output= YES`:

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

The GoF grows by a factor of about 3.8 per iteration from the first step, while the table says
damping is on. DIIS, which starts at iteration 4, eventually recovers. The final answer is right.

The three test jobs with lambda up to 0.0003 (`nh3_x-ray-constrained-rhf_cc-pVTZ`, its
`_extinction` twin, and the urea UHF job at 0.001) show nothing of the kind.

## 3. Theory

All symbols are defined here.

- $`D`$ is the density matrix and $`E(D)`$ the Hartree-Fock energy.
- $`F_k`$ is the calculated structure factor of reflection $`k`$, $`F_k^{\mathrm{obs}}`$ the
  observed magnitude and $`\sigma_k`$ its standard uncertainty.
- $`\alpha`$ is the scale factor, refitted at every iteration.
- $`N`$ is the number of reflections and $`p`$ the number of fitted parameters.
- $`\lambda`$ is the multiplier.

The constrained SCF makes stationary

```math
L(D) = E(D) + \lambda\,\mathrm{GoF}^2(D), \qquad
\mathrm{GoF}^2 = \frac{1}{N-p}\sum_k \frac{\left(\alpha |F_k| - F_k^{\mathrm{obs}}\right)^2}{\sigma_k^2}
\qquad (1)
```

so the matrix that is diagonalised is the Fock matrix plus $`\lambda C`$, with

```math
C = \frac{\partial\,\mathrm{GoF}^2}{\partial D}
  = \frac{2}{N-p}\sum_k \frac{\alpha\left(\alpha |F_k| - F_k^{\mathrm{obs}}\right)}{\sigma_k^2}\,A_k ,
\qquad A_k = \frac{\partial |F_k|}{\partial D}
\qquad (2)
```

This is what `MOLECULE.SCF:make_r_constraint` builds; $`A_k`$ is the Fourier transform of a pair
of basis functions at the scattering vector of reflection $`k`$, projected on the phase of
$`F_k`$.

**One SCF step, linearised.** Suppose the input density is off the converged one by
$`\delta D`$. Each $`|F_k|`$ is off by $`\mathrm{tr}\,(A_k\,\delta D)`$, so by equation (2) the
constraint matrix is off by

```math
\lambda\,\delta C = \lambda\,\frac{2\alpha^2}{N-p}\sum_k \frac{A_k\;\mathrm{tr}\,(A_k\,\delta D)}{\sigma_k^2}
\qquad (3)
```

Diagonalising with this error in the Fock matrix gives an output density with an error of its
own. To first order, and leaving out the change in the two-electron part of the Fock matrix, a
perturbation $`V`$ mixes occupied orbital $`i`$ with virtual orbital $`a`$ by
$`-V_{ia}/(\varepsilon_a-\varepsilon_i)`$, where $`\varepsilon`$ are the orbital energies. For a
closed shell this changes the structure factor of reflection $`l`$ by

```math
\mathrm{tr}\,(A_l\,\delta D^{\mathrm{out}}) = -4\sum_{ia}\frac{(A_l)_{ia}\,V_{ia}}{\varepsilon_a-\varepsilon_i}
\qquad (4)
```

Write the error in each structure factor in units of its sigma,
$`e_k = \alpha\,\mathrm{tr}\,(A_k\,\delta D)/\sigma_k`$. Putting (3) into (4):

```math
e^{\mathrm{out}} = -\lambda\,G\,e^{\mathrm{in}}, \qquad
G = \frac{2}{N-p}\,B B^{T}, \qquad
B_{k,ia} = \frac{2\alpha\,(A_k)_{ia}}{\sigma_k\sqrt{\varepsilon_a-\varepsilon_i}}
\qquad (5)
```

$`G`$ is an $`N \times N`$ symmetric matrix with no negative eigenvalues. Call it the gain
matrix, and its largest eigenvalue $`\gamma`$. The minus sign is the overshoot: an error comes
back reversed and $`\lambda\gamma`$ times larger.

**The refitted scale.** Because $`\alpha`$ is refitted each step, an error in the structure
factors that is proportional to the structure factors themselves costs nothing. So the direction
$`u_k \propto \alpha |F_k|/\sigma_k`$ is projected out of $`B`$ before $`G`$ is formed. This
lowers $`\gamma`$ for ammonia from 1071 to 642.

**The condition for convergence.** A step that mixes a fraction $`x`$ of the new density with
$`1-x`$ of the old multiplies the error along the stiffest direction by
$`1 - x\,(1+\lambda\gamma)`$. It shrinks only if

```math
x < \frac{2}{1+\lambda\gamma}
\qquad (6)
```

With no damping, $`x = 1`$, the step fails once $`\lambda > 1/\gamma`$.

**It does not depend on the scale of the sigmas.** If every sigma is multiplied by $`s`$, the
lambda that gives the same wavefunction is multiplied by $`s^2`$ and $`\gamma`$ is divided by
$`s^2`$. The product $`\lambda\gamma`$ measures how many times stiffer the data term is than the
wavefunction's own resistance to change. It is a property of how hard the fit is pushed.

## 4. The check

`MOLECULE.SCF:put_constraint_stiffness` (keyword `put_constraint_stiffness`, after `scf`)
computes $`G`$ from equation (5) and prints its largest eigenvalues, the limit of equation (6),
and the reflections with the largest diagonal elements. RHF only. For the ammonia job, from the
orbitals converged at lambda = 0.012:

| | |
|---|---|
| Largest gain per unit lambda, $`\gamma`$ | 642.5 |
| Sum of all gains per unit lambda | 2524 |
| Lambda where an undamped step fails, $`1/\gamma`$ | 0.00156 |
| Gain at lambda = 0.012 | 7.71 |
| Largest stable fraction of new density at 0.012 | 0.230 |

The measurement used the repaired damping, DIIS switched off, and lambda = 0.012 throughout:

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

Dylan's question (2026-10-07): is there a clue here about unreasonable reflections or sigmas?

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
- **A reflection worth a second look has a large own gain and a large residual at once**: it
  can move the wavefunction a long way and is asking to. Here that is (0 3 1), 2.58 sigma out,
  and (3 0 -2), 1.94 out. The product of the two is the analogue of Cook's distance in ordinary
  least squares. Not yet printed as a column.
- Since the result does not depend on the overall scale of the sigmas (section 3), this
  diagnostic can find a reflection whose sigma is too small *relative to the others*, not a set
  of sigmas that are all too small.

## 6. What cures it

Measured on the original two-lambda job, with the damping repair:

| Setting | Iterations at 0.012, 0.016 | Worst energy on the way |
|---|---|---|
| as blessed: mix 50% for 3 iterations, DIIS from 4 | 30, 20 | -29.6 |
| mix 15% for 3 iterations, DIIS saving from 1 | 18, 19 | -56.2013 |
| mix 15% for 6 iterations, DIIS from 6 | 19, 20 | -56.2013 |
| no damping, DIIS saving from 1 | 21, 21 | -56.03 |
| mix 15% throughout, with DIIS | 61, 54 | -56.2013 |

All reach GoF 0.78 and -56.2023. So strong damping for the first few steps, then DIIS, removes
the excursion. Damping kept on under DIIS only slows it.

Three ways to build this in, cheapest first:

1. **Choose the mix from equation (6) automatically.** Compute $`\gamma`$ once per job, or once
   per lambda, and damp the first few iterations with $`x = 1/(1+\lambda\gamma)`$, half the limit.
   Small change. The memory for $`A_k`$ is $`N`$ times the square of the basis size, which is
   too much for a large molecule with many reflections, so it would need the largest eigenvalue
   by power iteration instead, using the existing structure-factor and constraint routines.
2. **Start DIIS at once in a constrained SCF**, and make that the default for `xray_*` kinds.
   Almost no code. Does not remove the first bad steps as surely as damping.
3. **Undo the overshoot exactly.** The stiff part of the response lives in the space of
   reflections, which is small. Given the change in density from a plain step, the correction is
   one solve with $`1+\lambda G`$ in that space and one extra constraint build. The constrained
   SCF should then converge about as fast as an unconstrained one. This is the proper cure and a
   real piece of work.

### The third cure in detail

Split the error in the density into the part the reflections can see and the part they cannot.
Along eigenvector $`j`$ of $`G`$, with eigenvalue $`\gamma_j`$, a plain step returns the error
multiplied by $`-\lambda\gamma_j`$ (equation 5). So if the plain step changed the structure
factors by $`s`$ (in units of sigma, scale direction removed), the error it started with was
$`-(1+\lambda G)^{-1} s`$, and the step that lands on the answer is

```math
s^{\mathrm{right}} = (1+\lambda G)^{-1}\, s
\qquad (7)
```

In words: each stiff direction is mixed in with its own fraction $`1/(1+\lambda\gamma_j)`$,
which is exactly the fraction that cancels its overshoot, and everything the reflections do not
see is taken in full. Uniform damping must use the fraction of the stiffest direction for all of
them, which is why it is slow.

One iteration would be:

1. The plain step as now, giving the output density and its structure factors.
2. $`s`$, from the structure factors of the output and input densities. No new integrals.
3. Solve $`(1+\lambda G)\,t = \lambda G\,s`$ for $`t`$, the overshoot. $`G`$ is $`N \times N`$.
4. Build the potential $`V = \frac{2}{N-p}\sum_k t_k\,\alpha A_k/\sigma_k`$, which is one more call
   of the routine that builds the constraint matrix, with $`t`$ in place of the residuals.
5. Take $`V`$ off the Fock matrix and diagonalise again; or apply the first-order orbital
   response to $`V`$. Then DIIS as usual, which is left with only the ordinary two-electron part.

Cost: one extra constraint build and one extra diagonalisation per iteration. $`G`$ is built once
per lambda. For a large molecule, where storing every $`A_k`$ is too much, step 3 is solved by
conjugate gradients instead, each product with $`G`$ being one constraint build and one
structure-factor evaluation.

Expected result: about as many iterations as an unconstrained SCF, at any lambda. Not yet
tried. What could spoil it: the two-electron response left out of $`G`$ (1% here, larger for a
small-gap system), and orbitals far from converged, where the linearisation is poor; a few
damped steps first would cover the second.

## 7. Decisions for Dylan

- **Repair damping everywhere, or only for constrained SCF?** The repair is two lines in
  `MOLECULE.BASE:make_SCF_density_mx`. With it, every SCF that leaves damping at its default
  (on, 50%, three iterations) takes a different path. Final energies agree to convergence, but
  iteration counts and anything printed per iteration change, so references move. What the short
  and long suites do with the repair is in section 8.
- **Which cure from section 6.**
- **Whether to keep `put_constraint_stiffness`** as a user keyword, and whether to add the
  leverage-times-residual column.

## 8. Log

- 2026-10-07. Reproduced on the Mac release build. Found damping dead (water, two damping
  factors, identical tables; then markers in `make_SCF_density_mx` showing the old density never
  allocated). Commit `9156e72e` of 2026-09-13 had kept the old density between calls, which works
  only while the incremental build is on. Repaired on the branch. Damping-only scan and the
  stiffness routine as above.
- 2026-10-07. Short and long suites, Mac release build, with the damping repair: 117 of 130.
  Before the repair the short suite was 81 of 82 (`show_labels` only); the long suite was not
  run before it on this machine, so its five failures are not all known to be new. Failing
  besides `show_labels`: short `carbon_atom_uhf_cc-pVDZ_ANO_aoc`, `h2o+_uks_B3LYPG_def2-SVP`,
  `h2o_rhf_cc-pVDZ_tdhf`, `h2o_rhf_def2-SVP_RIJCOSX`, `h2o_rhf_def2-SVP_RIJCOSX_SG-0`,
  `nh3_rhf_DZP_ED_grid`, `oxygen_atom_uks_BLYP_ANO_fon`; long
  `cn-hcn_dimer_rhf_cc-pVDZ_various_E_decompositions`, `gly_ala_fragHAR_rhf_STO-3G`,
  `urea_rhf_STO-3G_HAR_TLS_cluster`, `urea_rhf_STO-3G_HAR_TLS_soft_modes`,
  `urea_rks_B3LYP_def2-TZVP_HAR_cell_maps`. Not looked at one by one. The five X-ray constrained
  jobs pass.
- 2026-10-07, later. Dylan: repair damping for every SCF. Branch `damping-repair` (off `develop`,
  the two-line repair only) is pushed. Looked at the twelve changed tests one by one in the Mac
  release build: the seven short ones agree with their references in the final energy and
  differ only in the iteration table; the five long ones move by 0.2 to 0.25% (2.7% in one small
  number) in quantities that follow a loosely converged SCF. **Still to do: build
  `damping-repair` in the reference build on achari2, run every suite, check each failure is of
  those two kinds, bless, merge to `develop`.** The session could not reach achari2: the
  permission system refused the ssh.
- Not done: UHF and Hirshfeld-atom constraints in the stiffness routine; the urea job; a debug
  build; anything on achari2.
