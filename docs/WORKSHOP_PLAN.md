# The workshop document — the plan

Written 2026-08-10. This is stage 3 of `docs/PLOT_PLAN.md` ("WORKSHOP docs, own
conversation"), and the last piece of the material lost with achari2 (see
`RECOVERY.md`). Like `PLOT_PLAN.md`, this file is pushed after every step so the
plan survives a lost context or a lost machine.

## The model to copy

Found. It is **not** in this repository and not on the `lamaGOET` branch — it is

    github.com/dylan-jayatilaka/lamaGOET   branch macos-qt-fixes   workshop/WORKSHOP.md

358 lines, adapted from Lorraine Malaspina's 2024 lab notes. A local copy is in
this session's scratchpad. What we take from it:

- **Audience is a user, not a developer.** No translator, no MPI, no Foo.
- **Shape**: why bother → how it works → things to know before you start →
  install → worked examples with a *results table containing `?`* the reader
  fills in → where the files end up → where to get the software → references.
- **The `?` table is the teaching device.** Published numbers in the left
  columns, blanks for the reader's own run.
- **Prose explains the science first, the software second.**

What is different here: that document drives lamaGOET's Qt GUI, ours drives
**Tonto directly** — `hart` for exercise 1, `tonto` with a job file for 2 and 3.
So the "fill in the HAR tab" screenshots become **stdin files, printed in full**.

## The exercises Dylan asked for

| # | Exercise | Program |
|---|---|---|
| 1 | HAR refinement of **NH₃** | `hart` (argv-driven) |
| 2 | HAR refinement of **urea or SO₂**, then an **RGBI analysis** | `tonto` |
| 3 | **XCW refinement** on exercise 2's HAR-refined geometry — an X-ray *wavefunction* refinement | `tonto` |

Fixed for all three, by instruction:

- method **RHF**, basis **def2-SVP**
- convergence **0.001** on the energy, **0.01** on the DIIS
- **no** `output_style_options` lines
- **no** `output_results=` lines (they exist only to trim output for testing)
- exercise 3: **no cluster charges**; lambda **0.01, 0.02, 0.03**, then continue
  with `initial_density= restricted` to lambda **0.1, 0.2, 0.3**

## Exercise 2 — urea or SO₂? (measured, not guessed)

Both are in `tests/`. Timed on `sauce` with the current `release/tonto`, same
settings for both — RHF/def2-SVP, HAR, no cluster charges, 0.001/0.01:

| | reflections | ASU | wall clock | R(F) after HAR |
|---|---|---|---|---|
| SO₂ (from `tests/long/so2_rhf_DZP_anharmonic_*`) | 1097 | S + 2×O | **8.0 s** | 0.0406 |
| urea (from `tests/hart/urea_hart_STO-3G`) | 817 | C,O,N,2×H | **23.8 s** | **0.0185** |

**Answer to Dylan's question: yes, SO₂ is quicker — 8 s against 24 s. Use urea
anyway.** Three reasons, and the third is the one that decides it:

1. **24 s is not slow.** The whole point of the comparison was to avoid a
   workshop exercise nobody can finish; neither of these is that.
2. **SO₂ refines badly under the agreed settings.** Stripped of the anharmonic
   4th-order ADPs on S and the cluster charges — neither of which we want in a
   first workshop — its R(F) *rises* from 0.0295 to 0.0406. Urea's falls to
   0.0185 (SHELX IAM is 0.0253). An exercise whose numbers get worse teaches
   the wrong lesson.
3. **Urea carries exercise 3, and exercise 3 is the point.** Locating hydrogen
   is *one* reason to do HAR, and the visible one — urea gives two N–H
   distances to set against neutron values, SO₂ gives none. But it is not the
   main reason, and the document must not say it is (Dylan, 2026-08-10):

   > HAR gives a **wavefunction for the system at that geometry**, which can be
   > further fitted (via XCW) and from which properties can be obtained —
   > properties consistent with a density that has been fitted to X-ray
   > diffraction data.

   That is what makes the three exercises one story rather than three: HAR
   produces the wavefunction, XCW constrains it against the data, and what
   comes out is a density you can compute properties from and still call
   experimental. The RGBI analysis in exercise 2 is the first such property.
   So the **"Why bother" section leads with the wavefunction**, and hydrogen
   appears as the check you can see with your own eyes — the reverse of the
   lamaGOET document's emphasis.

Urea is also a single self-contained CIF with its 817 reflections embedded —
one file for the participant to copy — whereas SO₂ needs a CIF plus a separate
`xd_F.hkl`. Both runs emitted all four fit plots.

**Decided: exercise 2 is urea.**

## Granular steps

Each step ends in a commit and a push.

| # | Step | State |
|---|---|---|
| 0 | Locate the reference WORKSHOP | ✅ done |
| 1 | Time urea vs SO₂; pick exercise 2's molecule | ✅ done — urea |
| 2 | Write this plan, push it | ✅ done |
| 3 | Draft the **three stdin decks** and show Dylan *before* running them | ✅ drafted, in `examples/` — awaiting review |
| 4 | Run exercise 1 (`hart`, NH₃) — capture the numbers | ⬜ |
| 5 | Run exercise 2 (HAR + RGBI) — capture numbers, the four HAR plots, dial diagrams | ⬜ |
| 6 | Run exercise 3 (XCW, six lambdas) — build the λ table | ⬜ |
| 7 | Write `workshop/WORKSHOP.md` around the captured output | ⬜ |
| 8 | Commit the images under `docs/images/workshop/` | ⬜ |
| 9 | Link it from `README.md` and the docs index in `CLAUDE.md` §7 | ⬜ |

## The XWR table (step 6)

Dylan's specification, verbatim in effect: one row per lambda, columns

| λ | final GoF² | energy | ⟨MO\|M0⟩ |
|---|---|---|---|

where ⟨MO|M0⟩ is the overlap of the *final* constrained orbitals with the
*original* (λ=0) ones. **Open question**: whether Tonto already prints this
overlap, or whether it must be computed from the archived MOs. To be resolved in
step 6 — if it is not printed, the honest options are to add it or to state
plainly that the column is computed post hoc.

## Pictures (steps 5 and 8)

Two families, both already working as of `d00e26b6`:

1. **The HAR fit plots** — `<job>.QQ_plot.png`, `<job>.F_z_vs_stl.png`,
   `<job>.F_z_vs_F_exp.png`, `<job>.Delta_F_vs_stl.png`. Tonto draws these
   itself via gnuplot at the end of a HAR (stage 2/2b of `PLOT_PLAN.md`).
   Confirmed emitted by the SO₂ timing run above.
2. **RGBI dial diagrams** — the TeX/`chemfig` half of the RGBI pipeline
   (`docs/RUNNING_RGBI.md`). Needs a TeX Live with `chemfig` plus ghostscript;
   the labelled-structure half additionally needs Open Babel and mol2chemfig.

Both go in the document as images, not as instructions to go and look at a file.

## Open questions raised by drafting the decks (step 3)

1. **`hart` cannot set the SCF energy convergence.** Exercises 2 and 3 set
   `convergence= 0.001` in `scfdata`; `hart` exposes `--dtol` (DIIS) but has no
   equivalent for the energy. So exercise 1 cannot be made to agree with the
   other two on the 0.001 figure. Either `hart` gains the option, or the
   document says plainly that it runs at hart's internal default.
2. **Urea's wavelength was never in `urea_init.cif`.** The archive CIF from the
   timing run came out with `_diffrn_radiation_wavelength -0.529177` — that is
   −1 bohr, i.e. the unset default, printed in Angstrom. Harmless here
   (extinction off, no dispersion), but it means the reported θ_max is
   meaningless. The decks now set `wavelength= 0.3173 angstrom` explicitly.
   Worth a look at why an unset wavelength prints as a negative number instead
   of failing.
3. **`_reflns_d_resolution_low` and `_high` are swapped** in the archive CIF
   relative to the input CIF (0.3475 vs 4.6860). Cosmetic, but it is in a file
   users are told to deposit.

## Exercise 3: what lambda urea's data will actually take (step 6, measured)

The requested ladder — 0.01, 0.02, 0.03 then 0.1, 0.2, 0.3 — **does not run on
this dataset.** This was established by experiment, not inference, and the
evidence is worth keeping because the reason is physical rather than a bug.

**λ = 0.01 from a cold start destroys the wavefunction.** GoF² goes 19.8 → 1254
→ 23335 within three iterations, the energy from −168.1 to −44, and the ⟨MO|M0⟩
overlap to 0.000000. Two candidate causes were tested and eliminated:

- *Wrong reflections?* No. `urea.hkl` and `urea_init.cif` were compared
  reflection by reflection — the same 817 Birkedal reflections, the CIF copy
  merely rounded to 3 dp.
- *Missing setup?* Partly. Adding a Becke grid and a `refine_hirshfeld_atoms`
  to settle the scale factor improved the starting point (GoF² 25.6 → 19.8,
  first-step gradient 2.70 → 1.64) but did not stop the divergence.

**The cause is that urea's data is extremely precise.** The effective mean σ²
is 2.1 × 10⁻⁴, against 4.4 × 10⁻² for the ammonia data in exercise 1 — a factor
of 200. The XCW gradient carries λ × dχ²/dP, so the same λ is a two-hundred-fold
stronger pull here. λ is not a dimensionless knob that transfers between
datasets.

**What does work**, walking up in steps of 0.001. **Caveat on these numbers:
they were produced with convergence 10⁻⁵ / 10⁻⁵, not the 0.001 / 0.01 Dylan
specified.** The tight setting was a *diagnostic* — the only working urea XCW in
the repo (`tests/long/urea_x-ray-constrained-uhf_STO-3G_plus_ELF_plot`) uses
10⁻⁵, so tightening it isolated whether the divergence came from the tolerance
or from λ. It came from λ: the loose setting converges just as well up to 0.003
and diverges at 0.01 just the same. The table is being regenerated at 0.001 /
0.01 for the document; the shape will not change but the last digits will.

| λ | GoF² | energy / hartree | ⟨MO\|M0⟩ | iterations |
|---|---|---|---|---|
| 0.001 | 15.79 | −168.129750 | 0.999567 | 14 |
| 0.002 | 14.00 | −168.127152 | 0.998901 | 17 |
| 0.003 | 12.91 | −168.124470 | 0.998222 | 20 |
| 0.004 | 12.15 | −168.121839 | 0.997572 | 24 |
| 0.005 | 11.58 | −168.119291 | 0.996964 | 28 |
| 0.010 | **diverged** | — | 0.000000 | killed at 13 |

Monotonic in all three columns, which is exactly what XCW should do: χ² falls,
the energy rises above its variational minimum, and the orbitals rotate away
from the unconstrained ones. **This is the table the document should carry**,
and it is a better teaching object than the requested one because the reader
can see the trade-off being bought.

Note λ = 0.010 was seeded from a *converged* λ = 0.005 and still blew up, so
this is not merely a cold-start problem. A finer approach (0.006, 0.007 …) might
reach it; untested, because of the time budget below.

### Time budget

Dylan's limit is 2–3 minutes per job. The tight-convergence ladder above
**exceeded 10 minutes** and was killed after 5 converged λ points plus 13
iterations of the sixth. The cost is per XCW iteration — each needs 817
structure factors from a Hirshfeld partitioning — and the iteration count climbs
with λ (14 → 28 across the ladder).

Two savings applied on Dylan's instruction, both of which also simplify the
decks: the explicit `becke_grid= { accuracy= high }` is gone (the default is
good enough, and `high` was expensive), and the `show_refinement_output=` /
`show_refinement_results=` lines are gone from all three decks. Whether the
requested loose convergence (0.001 / 0.01) brings the ladder inside the budget
without reintroducing the divergence is being measured now.

### Every table here is provisional

Agreed with Dylan, 2026-08-10: **all results tables must be regenerated once the
final parameter set settles**, and not patched piecemeal before then. That
includes exercise 1's and exercise 2's, not only the λ table — the decks have
changed since those runs (the `show_refinement_*` lines and the `becke_grid`
block are gone), so their numbers are from inputs that no longer exist as
written.

The parameter set is settled when these are fixed and measured together:

1. the λ values urea's data will actually take, at 0.001 / 0.01 convergence;
2. whether the job fits the 2–3 minute budget, and what is dropped if not;
3. whether `refine_hirshfeld_atoms` stays in the deck (it is what settles the
   scale factor, but it is also the single most expensive step).

Until then, treat every number in this file and in `workshop/WORKSHOP.md` as indicative
of shape, not of value.

---

## Observations worth keeping (2026-08-10)

**An XCW fit commonly gets worse before it settles, and the cause is unknown.**
Dylan's observation, confirmed here on urea. The λ = 0.001 trace goes
GoF² 11.18 → 9.32 → 11.01 → 10.13 → 9.42 → 9.43: down, back up past its
starting point, down again, settle. At λ = 0.0001 the same deck shows no wobble
at all (11.14 → 10.85 → 10.87).

In this run the swing coincides with the converger changing gear — damping and
level-shifting come off at iteration 3, which is where DIIS starts
extrapolating. That is a correlation in a single trace and **not** a
demonstrated cause; it is written into `workshop/WORKSHOP.md` as an observation, with the
cause explicitly left open. Worth a proper experiment some time: hold damping on
throughout, or start DIIS at iteration 0, and see whether the wobble follows the
converger or the constraint.

**The deck must not be named after the file it reads.** `refine_hirshfeld_atoms`
writes `<name>.HBB.cif2`, so `name= urea` in exercise 3 overwrote `urea.HBB.cif2`
— its own input, exercise 2's output. Running the lab twice would silently start
from the previous run's geometry. Fixed by `name= urea_xcw`; the input is now
provably untouched by a run (checked with `git status` after one).

---

## Still owed: SO₂ as an ADDITIONAL exercise (agreed 2026-08-24)

Not a replacement for urea. The measured rejection above still stands and is the
*reason* this is a separate exercise: under the plain workshop settings SO₂'s
R(F) **rises** 0.0295 → 0.0406 while urea's falls to 0.0185.

**So give SO₂ the settings it actually needs, and make that the lesson** — some
structures are not served by the default recipe:

- Keep `refine_4th_order_for_atoms= { S }` (anharmonic ADPs on sulfur) and the
  cluster charges. Source data: `tests/long/so2_rhf_DZP_anharmonic_consistent_cluster_charge_HAR/{stdin,xd_F.hkl}`.
  Note SO₂ needs a CIF **and** a separate `xd_F.hkl`, unlike urea's single CIF.
- Basis `def2-SVP`, as the other exercises.
- Append a `robydata=` / `roby_analysis` block for the bond indices.

**The chemical punchline urea has not got:** sulfur is **not** hypervalent. The
Roby-Gould bond order comes out ≈1.7, not 2, with a large ionic component —
reproducing Grabowsky *et al.*, *Angew. Chem. Int. Ed.* **2012**, *51*, 6776
(`docs/Grabowsky_2012_Angewandte_12_p6776-6779.pdf` on the stranded branch; cite
by DOI otherwise). `tests/rgbi/` has no SO₂ case, so one could be added.

**Shape:** `examples/5-so2-har/` following the established deck pattern (a `!`
comment block giving the `cd` + run line, then the job), and a `## Exercise 5` in
`workshop/WORKSHOP.md` matching the house layout exactly — prose → `**In a
terminal**, type:` → runtime → `### The input file` → `### What you should get`
(table with a `?` column) → `### The four diagnostic plots` → `### The bond
indices` → `### Things to try next` → `---`. Update the overview table at the top.
Images `workshop/images/so2.{QQ_plot,F_z_vs_stl,F_z_vs_F_exp,Delta_F_vs_stl}.png`
plus `so2.rgbi-{structure,dials-all}.png`.

**Every number must be re-measured**, not transcribed. Earlier SO₂ figures came
from an 8 August build of a branch that predates the extinction merge, and the
plots are job-named PNGs now, not `stdout.*.pdf`.
