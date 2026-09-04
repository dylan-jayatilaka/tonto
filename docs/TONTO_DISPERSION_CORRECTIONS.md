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

### Which convention Tonto uses — OVERTURNED, and this is the decision

**Dylan, 2026-09-04, and this is the ruling:** *"in Tonto we should not (often) remove
anomalous from F_exp. The idea is to always correct F_calc to include the effect, and match
F_exp."* So `add_dispersion_to_F_calc` is the convention, `remove_dispersion_from_F_exp`
stays the rarely-used alternative, and **neither is to become a new default**.

This overturns what this section said earlier the same day. That earlier argument is kept
below because it is the reason removal exists at all, not because it won:

> A deformation map is a difference of **electron densities**: the calculated density against
> the best estimate of the experimental one. f′ and f″ describe resonant scattering by core
> electrons and are not part of any static charge distribution, so on that reading they must
> come out of the *observation*. Putting dispersion into `F_calc` instead gives a clean
> **residual** — the anomalous part is explained and cancels in the difference — but leaves
> ρ_exp itself contaminated.

The practical consequence is that the open item below shrinks: the fallback a mis-honoured
`remove_dispersion_from_F_exp` currently gives you is the convention Tonto wants anyway.

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

### DEFECT: `remove_dispersion_from_F_exp` is honoured only when refining

**The 2026-09-04 morning entry that stood here was wrong, and the way it was wrong is worth
keeping.** It reported that `remove_dispersion_from_f_exp= yes` on
`L_cysteine_IAM_R_min_max_residuals` gives output identical to `correct_dispersion= yes`, and
concluded from that identity that the removal never happens. The identity is real. It proves
nothing.

**`L_cysteine` has no dispersion to correct.** Its CIF declares

```
_atom_type_scat_dispersion_real
_atom_type_scat_dispersion_imag
'C' 'C' 0.0000 0.0000   ... and the same for H, N, O and S
```

and nothing else in the job supplies coefficients, so `F_disp` is identically zero. The
checked-in reference prints the table of zeros itself, at `stdout:176`. Measured on the
release build at `real_precision= 10`, three variants of that job — `correct_dispersion= yes`,
`remove_dispersion_from_f_exp= yes`, and `correct_dispersion= no` — are **byte-identical in
every number**; only the keyword echo, the flag lines and the timestamps differ. A job in
which "off" agrees with both conventions to ten decimals cannot tell them apart, so it is not
evidence about either.

**The lesson.** Before comparing two dispersion conventions, check that the job has non-zero
f′ and f″. Only two shipped tests do: `yq28_anharm_disp_H_U_iso_IAM_refinement` (S with
f′ = 2, f″ = 1, set by a `dispersion_coefficients=` block after `process_CIF`) and
`YLID_IAM_plus_anomalous_residual_density` (coefficients supplied, `correct_dispersion= no`).
**yq28 is the test bed**; the run comparing add / remove / off on it was started and not
finished, and is the first thing the next session should do.

**What the gate actually is, read from the source.** `CRYSTAL:make_F_calc_from`
(`crystal.foo:4065`) adds dispersion into `F_calc` whenever `correct_dispersion` is true —
add OR remove. That is **not** a simple wrong gate, and the one-line change proposed in
`DEFERRED.md` would break the other convention:

- `REFLECTION:remove_anom_from_F_exp` takes `phase = .F_phase = F_calc/|F_calc|`. Removal
  needs the *full* model phase, because subtracting a complex `F_disp` from a magnitude has
  no phase-free form (see the next subsection).
- `REFLECTION:remove_anom_from_F_calc` then does `F_calc = F_calc - F_disp`.

Both require `F_calc` to be carrying dispersion at that moment. Gate the addition on
`add_dispersion_to_F_calc` alone and, under the remove convention, the phase silently becomes
the dispersion-free one and `F_disp` is subtracted from an `F_calc` that never had it.

So the sequence in `CRYSTAL:make_F_calc_derivs` — add, optimise the scale, remove from
`F_exp` to make `F_corr`, remove from `F_calc`, re-optimise — is already the coherent
XD/SHELX recipe, and the addition it starts from is deliberate.

**The genuine gap is narrower: the removal lives only in the derivative path.** It is in
`make_F_calc_derivs` (`crystal.foo:4133` and `:4191`) and nowhere else, so a job that sets
`remove_dispersion_from_f_exp=` and does **not** refine never reaches it: it gets dispersion
added into `F_calc`, keeps `F_corr = F_exp`, and reports removal as on. Whether the removal
bites during a refinement is exactly what the unfinished yq28 run would say.

Given the ruling above, the consequence is mild — the convention such a job silently falls
back to is the one Tonto wants — but it is still a flag that reports itself as honoured and
is not.

**Two smaller things seen while reading, neither measured:**

- `CRYSTAL:F_exp_scaled_corrected` (`crystal.foo:3775`) is gated on
  `add_dispersion_to_F_calc` although what it does is *remove* dispersion from `F_exp`, and
  it subtracts `abs(F_disp)` from the magnitude rather than projecting the complex quantity
  onto the model phase. It feeds the `.fcf`/`.fco` and CIF reflection tables
  (`crystal.foo:8628`, `:8755`, `:8870`), not the refinement.
- `ATOM:has_tabular_dispersion_for` tests `abs(element_xray_dispersion(Z)) >= ZERO`, which is
  true for every element. Harmless today: its only call sites are commented out.

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

1. **Finish the yq28 measurement first.** Run
   `yq28_anharm_disp_H_U_iso_IAM_refinement` at `real_precision= 10` with
   `correct_dispersion= TRUE`, with `remove_dispersion_from_f_exp= TRUE`, and with
   `correct_dispersion= FALSE`, and diff the three. That says whether the removal bites in a
   refinement, and it is the only shipped job with non-zero f′ and f″ that refines. Do not
   repeat the `L_cysteine` comparison: it has none. **Do not apply the one-line gate change
   in `DEFERRED.md`** — see the defect section above for why it breaks the removal path.
   The remaining defect, once measured, is that the removal exists only in
   `CRYSTAL:make_F_calc_derivs`, so a non-refining job gets the add convention while
   reporting removal. Whatever the fix, `add_dispersion_to_F_calc` stays the convention and
   neither flag becomes a default — the ruling in §3. Needs its own re-bless.
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
