# The workshop document — the plan

Written 2026-08-10. This is stage 3 of `docs/PLOT_PLAN.md` ("WORKSHOP docs, own
conversation"), and the last piece of the material lost with achari2 (see
`RECOVERY.md`). Like `PLOT_PLAN.md`, this file is pushed after every step so the
plan survives a lost context or a lost machine.

## The model to copy

Found. It is **not** in this repository and not on the `lamaGOET` branch — it is

    github.com/dylan-jayatilaka/lamaGOET   branch macos-qt-fixes   docs/WORKSHOP.md

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
3. **SO₂ has no hydrogen.** Locating hydrogen is the whole point of HAR. Urea
   gives two N–H distances to set against neutron values; SO₂ gives none.

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
| 3 | Draft the **three stdin decks** and show Dylan *before* running them | ⬜ |
| 4 | Run exercise 1 (`hart`, NH₃) — capture the numbers | ⬜ |
| 5 | Run exercise 2 (HAR + RGBI) — capture numbers, the four HAR plots, dial diagrams | ⬜ |
| 6 | Run exercise 3 (XCW, six lambdas) — build the λ table | ⬜ |
| 7 | Write `docs/WORKSHOP.md` around the captured output | ⬜ |
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
