# Estimating the XCW λ instead of scanning for it

**Status: not checked in. A proposal for discussion (Dylan, 2026-08-10), parked
while the workshop proceeds with the ordinary decadic scan.**

The question that prompted it: *is there a theoretical way to estimate the rough
size of λ to try first, so you can get around scanning for it?*

The answer appears to be yes, and it costs nothing — the one number you need
falls out of the unconstrained SCF you already run. Everything below is checked
against the urea runs of 2026-08-10 rather than asserted; the checks are shown
so they can be disputed.

---

## 1. What λ is

X-ray constrained wavefunction fitting minimises

$$L[\Psi] = E[\Psi] + \lambda\left(\chi^2[\Psi] - \Delta\right)$$

so λ is a **Lagrange multiplier**, and at the constrained solution it takes the
value

$$\boxed{\ \lambda = -\frac{dE}{d\chi^2}\ }$$

That is not a heuristic, it is the stationarity condition. λ is an **exchange
rate**: the number of hartree you pay per unit of χ² you buy. It is the slope of
the E-against-χ² curve traced out as the constraint is tightened.

Two consequences follow immediately, and both matter more than the formula:

- **λ is not dimensionless and has no natural size.** It carries units of
  energy per unit χ². Ask what "λ = 0.01" means and the only honest answer is
  "it depends on your σ".
- **λ therefore cannot transfer between datasets.** χ² is a sum of
  $(\Delta F/\sigma)^2$, so a dataset with smaller σ has a steeper χ² surface,
  and the same λ pulls correspondingly harder. This is not a subtlety; it is a
  factor of hundreds in practice (§4).

## 2. The identity, checked

From the urea XCW run at RHF/def2-SVP, convergence 0.001 / 0.01, on the
HAR-refined geometry — the converged point for each λ:

| λ | χ² (GoF²) | E / hartree |
|---|---|---|
| 0.001 | 15.84 | −168.129790 |
| 0.002 | 14.00 | −168.127161 |
| 0.003 | 12.91 | −168.124468 |

Finite differences against the midpoint λ of each interval:

| interval | ΔE / hartree | Δχ² | −ΔE/Δχ² | midpoint λ | ratio |
|---|---|---|---|---|---|
| 0.001 → 0.002 | +0.002629 | −1.84 | 0.001429 | 0.0015 | 0.95 |
| 0.002 → 0.003 | +0.002693 | −1.09 | 0.002471 | 0.0025 | 0.99 |

The identity holds to 1–5%, the residual being the second-order curvature that a
midpoint finite difference does not capture. **This is also a check on the
code**: the multiplier Tonto applies really is the slope of the curve it
produces.

A third, independent point from the decadic run — λ = 0.0001 (χ² = 19.50,
E = −168.131457) to λ = 0.001 (χ² = 15.83, E = −168.129782) — gives
−ΔE/Δχ² = 4.6 × 10⁻⁴, which sits between the two λ values as a mean-value
theorem requires it to.

## 3. The estimate

Rearranged, the identity is a *prediction* rather than a diagnosis:

$$\lambda \;\approx\; \frac{\Delta E_{\text{acceptable}}}{\Delta \chi^2_{\text{wanted}}}$$

Both numbers are known before any scanning:

- **χ²(λ=0)** is free. It is the goodness of fit of the *unconstrained*
  wavefunction, printed by the ordinary SCF that every XCW job runs first.
- **Δχ² wanted** is at most χ²(0) − 1, since χ² = 1 is a perfect fit within the
  stated errors and there is nothing below it to buy.
- **ΔE acceptable** is a judgement, but a bounded one. XCW is recovering
  electron correlation and crystal-environment effects that the isolated-molecule
  Hartree–Fock wavefunction lacks, so the energy it is reasonable to give up is
  some small fraction of the correlation energy — say 0.5–2% of ~1 hartree for a
  molecule of urea's size, i.e. 5–20 mhartree.

**Worked, for urea.** χ²(0) = 19.8, so Δχ² ≈ 19. With ΔE between 0.005 and
0.02 hartree:

$$\lambda \approx \frac{0.005}{19} = 2.6\times10^{-4}
\quad\text{to}\quad
\frac{0.02}{19} = 1.1\times10^{-3}$$

**The 10⁻³ decade — which is exactly what the decadic scan found**, at a cost of
zero extra SCF cycles.

## 4. Why nothing transfers between datasets

χ² ≈ 1 + (systematic misfit)/σ̄², so χ²(0) rises as the data get more precise,
and by the estimate λ ∝ 1/χ²(0). Two datasets from this workshop:

| | mean σ² | χ²(0) | usable λ |
|---|---|---|---|
| NH₃ (exercise 1) | 4.4 × 10⁻² | 1.07 | ~20× urea's |
| urea (exercise 2) | 2.1 × 10⁻⁴ | 19.8 | ~10⁻³ |

Urea's data is two hundred times more precise, and its χ² surface is
correspondingly steeper. **The precision of the data sets the scale of λ.** A λ
that is gentle on one crystal will annihilate the wavefunction on another, which
is precisely what was observed: λ = 0.01 took urea's ⟨MO|M0⟩ overlap from 1.000
to 0.000056 and its energy from −168.1 to −127.8 hartree.

## 5. What this would change

The scan does not disappear, it changes role — from a **search** to a
**confirmation**:

1. Run the unconstrained SCF. Read χ²(0).
2. Estimate λ ≈ ΔE/χ²(0). Start there.
3. Run one decade either side to confirm you are in the right one.

Step 3 is worth keeping in a *teaching* document regardless of the estimate,
because watching one decade do nothing and the next destroy the wavefunction is
what makes the scale memorable.

## 6. Open questions — the reasons this is parked, not adopted

1. **ΔE_acceptable is still a judgement.** The estimate converts "guess λ" into
   "guess the energy you will spend", which is a better-posed question but not a
   closed one. Is there a principled choice — a fixed fraction of the
   correlation energy, or a criterion based on when ⟨MO|M0⟩ starts to fall
   steeply?
2. **Only tested on two datasets**, one of which (NH₃) has not had its λ range
   measured at all — the 20× is a prediction, not an observation. **That is the
   obvious next experiment**, and it is cheap: NH₃ runs in seconds.
3. **The linear estimate ignores curvature.** dE/dχ² is not constant — it grew
   by 70% between the first and second intervals above. So the estimate predicts
   the *decade* reliably and the value only loosely. That may be all anyone
   needs.
4. **Is there a divergence threshold in the same terms?** λ = 0.01 did not
   merely overshoot, it broke the SCF. Whether the point at which the
   constrained Fock operator stops being usable can also be predicted from
   χ²(0) is unknown, and would be worth more than the estimate itself — it is
   the failure a user actually hits.

## 7. A small feature this suggests

Tonto's λ keywords are `initial_lambda=`, `lambda_step=`, `lambda_max=`, and
`lambda_step` is **additive only** (`foofiles/scf_data.foo`, three `case`
labels). So a decadic scan has to be written as one `scfdata` block per decade.

Two candidates, neither implemented:

- **`lambda_factor=`** — a multiplicative step, so a decadic scan is one block.
- **`target_chi2=`** — the real prize: let the code find the λ that reaches a
  requested χ², which is what a user actually wants and is exactly what §1 says
  is well defined.

---

*Written 2026-08-10 from the urea runs of that day. Deliberately not committed to
`antlr4`; it will reach `wip/sauce` through the Stop-hook snapshot, so it cannot
be lost, but it is not part of the workshop deliverable.*
