# Anomalous dispersion, Bijvoet pairs, and the residual density map

**A working document.** It records what was reviewed, what was measured, and what was
decided, and it should be deleted into the user-facing pages once the open items below
close. See `CLAUDE.md` §1.

The subject is how Tonto handles the anomalous dispersion terms f′ and f″ — in the
refinement, and in the Fourier synthesis behind the residual and deformation density
maps — and a wrong-answer bug found there.

---

## 1. Kanghyun Chu's review

Kanghyun Chu (Bern) read the relevant code in October 2025, answering a question from
Hans-Beat Bürgi about merging Friedel pairs and symmetry equivalents. His findings,
summarised; everything in this section was confirmed against the source in September 2026.

### Preprocessing of the reflection list

Reading `crystal= { ... }` calls `MOLECULE.MAIN:read_crystal`, which reaches
`CRYSTAL:remove_equivalents`. The rule is simple and it is **not** merging:

> If there are any symmetry-equivalent reflections, only the **first** in the list is
> kept and the rest are discarded. There is no averaging.

Tonto says so at run time ("This isn't allowed so I will keep only the first equivalent,
and purge the rest… If you don't like this then first merge your data"). The routines that
would average — `merge_equivalents`, `merge_ordered_equivalents`,
`make_list_of_Bijvoet_classes` — are all commented out, together with their keywords.

The consequence that matters: **Friedel pairs are not removed for non-centrosymmetric
systems**, because −h is not a symmetry equivalent of h in a group without inversion. For
centrosymmetric groups the list does become Friedel-pair-free.

### Refinement

The least squares fits `F_corr` against `F_pred`: `F_corr` is the observation corrected for
anomalous dispersion, `F_pred` is `F_calc` carrying the scale and extinction corrections.

### The residual density path

```
put_minmax_residual_density              (keyword)
  MOLECULE.SCF:get_minmax_residual_density
    CRYSTAL:make_residual_density_cell
      DIFFRACTION_DATA.SET:make_symop_generated_dF_a_v2
```

For each reflection the Fourier coefficient is built by giving the observed magnitude the
model's phase, applying the scale and extinction correction

```
ext        = scale_factor / (val)^(1/4)
val        = 1 + extinction_factor · |F_calc|² · angle_part
angle_part = (1 + cos²2θ) / (1 + cos2θ · sin2θ)
```

and subtracting `F_calc`. In practice `extinction_factor = 0` and `ext = scale_factor`,
because the extinction optimisation is not called by default.

All symmetry operations are then applied to the Miller indices and the coefficient assigned
to each generated index, with the fractional translation's phase factor included. For
reflections at special positions in reciprocal space, repeated indices are allowed and
divided by the site symmetry factor: for `P4` and `hkl = [0,0,l]`, four copies of `[0,0,l]`
enter the synthesis with `dF` reduced by four.

### The Fourier synthesis, with and without a mate

Where a reflection has no Friedel mate in the list, the missing complex-conjugate partner
is supplied arithmetically:

```
Δρ_hkl = Re( dF_hkl · e^{-ik·r} + dF_hkl · e^{-ik·r} ) = 2 · Re( dF_hkl · e^{-ik·r} )
```

Where both members are present, each contributes on its own:

```
Δρ_hkl = Re( dF_hkl · e^{-ik·r} + dF_-h-k-l · e^{+ik·r} )
```

and `dF_-h-k-l` need not be the conjugate of `dF_hkl`, because of experimental noise and
imperfect dispersion correction. That distinction is the whole point: doubling is only
valid when the conjugate half is genuinely absent.

### The defect: unmerged Bijvoet pairs are counted twice

Kanghyun found this on xylitol, `P2₁2₁2₁`. The data run `0 ≤ h ≤ 20`, `0 ≤ k ≤ 21`,
`-21 ≤ l ≤ 21`, and contain no Friedel pairs — but they do contain **Bijvoet** pairs such
as `(h,k,+l)` and `(h,k,−l)`. These are neither symmetry related by the two-fold screws nor
Friedel mates, so both survive `remove_equivalents` and both are doubled.

Expanding `(h,k,l)` gives `(h,−k,−l)`, `(−h,k,−l)`, `(−h,−k,l)`, and doubling each assumes
`(−h,−k,−l)`, `(−h,k,l)`, `(h,−k,l)`, `(h,k,−l)` are absent. Expanding `(h,k,−l)` produces
exactly that set. **Every shared Fourier component is therefore counted twice, and the
residual density is overestimated.**

His conclusion: the residual density calculation is correct *unless* Bijvoet pairs are
involved, and the refinement is largely unaffected — a case with both `(hkl)` and `(hk−l)`
present behaves like one with `(hkl)` alone at twice the weight.

### One corner case he had already fixed

`f9fb26bc` handles a *different* Bijvoet situation: a non-inversion operator generating
`(−h,−k,−l)` from `(h,k,l)` — for example a two-fold along z acting on `[h,k,0]`. That is
counted into the site symmetry factor `ss`, and was tested on `Fd-3m` (diamond). It is
correct and untouched by anything below.

---

## 2. What was measured, 2026-09-04

### The Friedel count is right

`VEC{REFLECTION}:n_unmatched_Friedel_pairs` was suspected of reporting every reflection.
It does not; it is correct, and the suspicious equality is what merged data looks like.
Verified independently of Tonto by a script over the raw `.hkl`, on the one shipped
dataset where the answer is non-trivial:

| `L_alanine_IAM_scale_factor_test` | Tonto | independent count |
|---|---|---|
| reflections kept (`f_sigma_cutoff= 4.0`) | 772 | 772 |
| without a Friedel mate | 192 | 192 |

The four rejected reflections account for the number exactly: `(-6,-2,-6)` and `(6,2,6)`
are a Friedel pair dropped together, `(0,7,1)` and `(4,5,0)` were unpaired, taking 194 to
192. And `gly_ala_100K.hkl` contains **zero** reflections whose mate is also present, so
2514 of 2514 is correct arithmetic on merged data.

What is wrong there is only the *line*: `Fridel` is misspelled, it counts reflections
without a mate rather than pairs, and on merged data it restates `N_r` in a way that reads
like a fault. Held for a batched output re-bless.

### The double count is live in the shipped tests — and how NOT to count it

**A first attempt at counting this was wrong, and the error is worth recording**, because it
is easy to repeat. Asking "is any symmetry equivalent of −h in the list?" without excluding
−h that lands on **h itself** counts Kanghyun's `f9fb26bc` case as a defect. In
`P2₁2₁2₁`, `diag(-1,-1,1)` maps `(0,2,0)` to itself, so the reflection is its own Bijvoet
partner — which the site symmetry factor already handles, correctly. Counting those gave
L-alanine 613 false positives and predicted a change that did not happen.

The right question is whether some *other* reflection in the list is a Bijvoet partner:

| test | genuine cross-reflection Bijvoet | self-partner only (already handled) |
|---|---|---|
| `YLID_IAM_plus_anomalous_residual_density` | **1704 of 2104** | 396 |
| `L_cysteine_IAM_R_min_max_residuals` | **1614 of 1979** | 344 |
| `L_alanine_minmax_residual_density_map` | **0** | 613 |
| `nh3_x-ray-constrained-rhf-cluster-charge_cc-pVTZ` | 4 of 88 | 35 |

### The fix

`CRYSTAL:get_all_Friedel_pairs` asked "is `(−h,−k,−l)` literally in the list?" The question
it must ask is "is any symmetry equivalent of −h in the list, **belonging to a different
reflection**?" — the Bijvoet mate. `CRYSTAL:set_Fourier_multiplicities` now asks that, and
replaces `get_all_Friedel_pairs` at both call sites in `crystal.foo`.

Two things it deliberately does not change. Centrosymmetric groups keep `mult = 2`
throughout, because every reflection's own symops generate −h and the site symmetry factor
handles it. And a reflection mapped to −h by one of its *own* symops is left at 2, for the
same reason — that is Kanghyun's `f9fb26bc` case, and `if (all(g==h)) cycle` is what
protects it.

It keys each reflection into one integer, sorts once and bisects, because the naive test is
O(n²·n_seitz) and that is minutes on a large data set. `get_all_Friedel_pairs` survives as
the diagnostic it always was.

### What the fix changes, measured

Release build, gfortran-14, against the stored references. Only the residual-density
Maximum / Minimum / RMS moved anywhere; nothing else in any output changed.

| test | Bijvoet | dispersion | Maximum | Minimum | RMS |
|---|---|---|---|---|---|
| `YLID_…_residual_density` | 81% | **none** | 0.2952 → **0.1689** | −0.2917 → **−0.1669** | 0.0590 → **0.0341** |
| `L_cysteine_IAM_R_min_max_residuals` | 82% | yes | 0.4985 → 0.2910 | −0.3973 → −0.2372 | 0.0737 → 0.0452 |
| `yq28_anharm_disp_H_U_iso_IAM_refinement` | — | yes | 0.9108 → 0.8564 | −1.6066 → −1.6195 | 0.1223 → **0.1245** |
| `L_alanine_minmax_residual_density_map` | **0** | no | unchanged, 0 ulp | | |
| `urea_hart_STO-3G` | — | — | unchanged, 0 ulp | | |

**The two changes separate cleanly, and each has its own signature.**

*YLID isolates the Bijvoet fix.* It sets `correct_dispersion= no`, and `set_F_disp` is called
only inside `if (.xray_data.INQ:correct_dispersion)` (`crystal.foo:4065`), so `F_disp` keeps
its `DEFAULT(ZERO)` and `F_phase_without_disp` is identical to the old phase. Its change is
the doubling correction and nothing else: a **uniform factor of 1.75** on all three
statistics, tracking the 81% of reflections that were doubled.

*yq28 isolates the dispersion port.* Small, non-uniform, and the **RMS goes up** — which a
doubling correction cannot do, since it can only scale the map down.

*L-alanine is the negative control.* Zero genuine Bijvoet pairs, so the fix must leave it
alone, and it does — to the last ulp. **It should stay that way**: it is now the regression
test for the self-partner case, and a future "fix" that makes it move has broken
`f9fb26bc`.

**The scientific consequence.** Residual densities have been overstated by roughly 1.75× for
non-centrosymmetric structures whose data retain Bijvoet pairs. For YLID the maximum residual
falls from 0.295 to 0.169 e Å⁻³ — the difference between a map that appears to hold
unmodelled features and one that does not.

---

## 3. The dispersion question

### The defaults

```
add_dispersion_to_F_calc      :: BIN, readonly  DEFAULT(FALSE)
remove_dispersion_from_F_exp  :: BIN, readonly  DEFAULT(FALSE)
```

Neither is set by HAR or XCW. `F_corr` is reset to `F_exp` at the top of every refinement
cycle, so with both flags off **`F_corr` is `F_exp`** and the refinement fits uncorrected
data. The branch that would make them differ carries the comment *"WARNING: this is
untested & fails"*.

### Why removal belongs on F_exp, not on F_calc

Agreed with Dylan, 2026-09-04. A deformation map is a difference of **electron densities**:
the calculated density against the best estimate of the experimental one. f′ and f″ describe
resonant scattering by core electrons and are not part of any static charge distribution, so
they must come out of the *observation*, or ρ_exp is not a density at all.

Putting dispersion into `F_calc` instead gives a clean **residual** — the anomalous part is
explained and cancels in the difference — but leaves ρ_exp itself contaminated. That answers
"what does my model fail to explain", which is a different question from "what is the
experimental density". Only the second is what a deformation map is for.

### What the code was doing

`make_symop_generated_dF_a_v2` subtracted `F_disp` from `F_exp` **and** from `F_calc`,
immediately before differencing them. Kanghyun flagged this in the source ("F_disp will
cancel each other out and do nothing"), and he is right: on the magnitudes the two
subtractions cancel identically.

The sharper problem is the **phase**. The routine took its phase from `F_calc` *including*
dispersion, so the complex observation it built was contaminated before either subtraction
happened — and subtracting the same quantity from both sides of a difference cannot correct
a phase.

### The correct conventions already existed, as dead code

`DIFFRACTION_DATA.SET:make_phased_dF_a` made three deliberate choices, all of them the ones
the live routine got wrong:

```
phase = REFLECTION:F_phase_without_disp(.reflections)
dF    = (.reflections.F_corr - .reflections.F_pred) * phase / ext
```

`F_corr` rather than `F_exp`; `F_pred` rather than raw `F_calc`; and a phase explicitly
without dispersion. Nothing called it. Its three call sites in `crystal.foo` were all
commented out.

Both phase accessors are correct, for different jobs, and both were coded: `F_phase`
(with dispersion) is the best estimate of the *true* phase, which is what you need in order
to subtract a complex `F_disp` from a magnitude; `F_phase_without_disp` is what the
difference map needs.

### What was done — and what was reverted

`make_phased_dF_a`'s structure was ported into `make_symop_generated_dF_a_v2` and the two
cancelling `F_disp` lines deleted. The superseded routines then went: `make_phased_dF_a`,
`make_phased_dF_b` (no callers anywhere), and `CRYSTAL:make_residual_density_cell_{c,g,m,p}`,
four commented-out variants that were their only callers — per Dylan, experiments in speed and
efficiency, these calculations being slow.

They were superseded, not merely unused: they take no spacegroup, produce `dF` of length
`n_refl` with no symmetry expansion, and apply a blanket `fac = TWO/volume` under the comment
*"Factor 2 assumes Friedel pairs merged and removed"*. That blanket factor is the ancestor of
the bug in §2 — the rewrite made it per-reflection and then asked the wrong question.

**The port turns out to be almost entirely a no-op, which is worth knowing.** `F_pred` is a
real magnitude, `|F_calc|` times scale and extinction, so

```
old:  |F_obs|·e^{iφ}/ext − F_calc     =  (|F_obs|/ext − |F_calc|)·e^{iφ}      φ  = arg(F_calc)
new:  (F_corr − F_pred)·phase/ext     =  (|F_obs|/ext − |F_calc|)·e^{iφ′}     φ′ = arg(F_calc − F_disp)
```

are the same expression apart from the phase, and `F_corr` is `F_exp` unless
`remove_dispersion_from_F_exp` is set. So the only substantive change was `φ → φ′`.

**That phase change was reverted on 2026-09-04, deliberately.** With
`remove_dispersion_from_F_exp` off — its default — `F_corr` still carries the anomalous signal
while the phase would no longer, giving a half-corrected map that neither convention intends.
`make_phased_dF_a` was coherent only because its `F_corr` was meant to be corrected too. The
two must be switched together, so `F_phase` stays for now and the comment at the site says why.

Measured, and it is what made the attribution clean: with the phase reverted,
`yq28_anharm_disp_H_U_iso_IAM_refinement` returns **exactly** to its original numbers, so its
whole change had been the phase; and `L_cysteine` is unaffected, so the phase changed nothing
measurable there. The re-bless is therefore single-cause — the Bijvoet fix alone.

`REFLECTION:F_phase_without_disp` survives, unused for now, because it is what the eventual
coherent change needs.

### DEFECT: `remove_dispersion_from_F_exp` is accepted, reported, and not honoured

Found 2026-09-04, testing the 2020 comment *"WARNING: this is untested & fails"* rather than
repeating it. **It does not crash — but the comment was right about the outcome**, and
"fails" most likely meant "fails to do the right thing". `remove_dispersion_from_f_exp= yes` on
`L_cysteine_IAM_R_min_max_residuals` runs to exit 0 and reports

```
Correct dispersion? ............ T
Add dispersion into F_calc ..... F
Remove dispersion frm F_exp .... T
```

and then produces output **identical to `correct_dispersion= yes`** — 12 lines differ in the
whole file, and all of them are the keyword echo, the three flag lines, and the version and
timing lines. Every number is the same: R(F) 0.0198, GoF 2.0913, residual density
0.2910 / −0.2372 / 0.0452.

Two conventions cannot legitimately agree to the last digit. The cause is one gate, in
`CRYSTAL:make_F_calc_from`:

```
! Include dispersion?
if (.xray_data.INQ:correct_dispersion) then      ! = add OR remove
   .get_dispersion_correction(Fa)
   .xray_data.reflections.set_F_disp(Fa)
   Fc = Fc + Fa                                  ! added under BOTH conventions
end
```

`set_F_disp` correctly belongs there — both conventions need `F_disp` computed — but
`Fc = Fc + Fa` should be gated on `add_dispersion_to_F_calc` alone. The removal itself lives
only in `make_F_calc_derivs` (`crystal.foo:4129`, `:4185`), the derivative path, so choosing
the XD/SHELX convention silently gives the other one.

The machinery around it is sound: the keyword exists, `set_remove_disp_from_F_exp` assigns the
right member, and the two readers guard each other with mutual `DIE_IF`s so both cannot be set.
It is the gate alone.

**Not fixed here**, deliberately: it is a separate change needing its own re-bless, and one was
already in flight. The source comment at both sites now records what is measured instead of the
2020 guess.

### How to remove dispersion from F_exp, as well as it can be done

There is no phase-free way. `F_exp` is a magnitude, subtracting a complex quantity needs a
phase, and the best available is the model's, so the correction is necessarily
model-dependent. `REFLECTION:remove_anom_from_F_exp` does exactly the right thing:

```
F_corr = |  |F_obs| · e^{iφ_full}  −  F_disp · scale  |
```

Borrow the phase from the full model, subtract the modelled dispersion, take the magnitude.
Iterated with the refinement it improves as the model does.

**Bijvoet averaging is the only phase-free alternative, and it buys less than it appears
to.** f″ produces the Bijvoet difference and averaging cancels it to first order; f′ is real,
shifts f₀ → f₀ + f′ identically for h and −h, and survives averaging untouched. So averaging
removes half the problem and discards the anomalous information to do it.

---

## 4. Open items

1. **Fix the gate so `remove_dispersion_from_F_exp` is honoured** — `Fc = Fc + Fa` in
   `CRYSTAL:make_F_calc_from` must depend on `add_dispersion_to_F_calc`, not on
   `correct_dispersion`. Then `F_corr` finally differs from `F_exp`, the phase can be switched
   to `F_phase_without_disp` in the same change, and the pair becomes coherent. Only then does
   it make sense to ask whether removal should be the default for deformation and residual
   density work. Needs its own re-bless.
2. **The `# of unmatched Fridel pairs` line** — misspelled, misnamed, and uninformative on
   merged data. Batched with other output changes, since any of them re-blesses ~35
   references.
3. **The `YLID_IAM_plus_anomalous_residual_density` name.** The job sets
   `correct_dispersion= no`, so f′ and f″ are supplied and applied to nothing: the anomalous
   signal is left in the observations and shows in the map. That is the defensible way to
   look at anomalous scatterers deliberately, and it is what the job does — but the name
   reads as though dispersion were being added.
4. **Merging.** `make_list_of_Bijvoet_classes` and the two `merge_*` routines are still
   commented out. Tonto cannot merge; it can only discard.

## 5. References

- Meurer *et al.*, *Refinement of anomalous dispersion correction parameters in
  single-crystal structure determinations*, IUCrJ (2022) —
  https://pmc.ncbi.nlm.nih.gov/articles/PMC9438505/ — f′ and f″ can be refined rather than
  fixed, and residual electron densities are strongly affected when the treatment is wrong.
- *Validation of experimental charge-density refinement strategies: when do we overfit?*,
  IUCrJ (2017) — https://journals.iucr.org/m/issues/2017/04/00/lc5072/index.html — Friedel
  pairs must be kept together when data are split, being not independent.
