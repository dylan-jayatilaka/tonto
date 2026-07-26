# SAM branch analysis

**Date:** 2026-07-26. **Question:** what does the `SAM` branch actually change,
beyond cosmetic edits and attribute/intent churn — and how much is already in `antlr4`?

**Method.** Diff SAM against **its own merge-base** with `antlr4` (`b878c31b`, 2025-10-24)
to isolate SAM's own edits from the 300+ `antlr4` commits since; then check each real change
against `antlr4`'s current tree. `-w` (whitespace-ignored) counts confirmed reformatting is a
negligible share everywhere — the bulk is *identifier renaming*, which `-w` cannot collapse.

**Topology.** SAM = 4 commits on top of `b878c31b`, 26 files, ~3,900/3,900 lines. Self-described
WIP: *"Started gaussian IAM & SAM & becke_grid refactor, unchecked"* → *"Foo compiles, fortran
may not"* → *"Fortran compiles under debug"* → *"Renaming vars… open shell & integration errors
under -fast"*.

## Bottom line

The vast majority of SAM is a **rename hygiene pass + attribute/intent tidying**, much of it
already done independently on `antlr4`. The genuine semantic delta is small, **unfinished**, and
sits on a **form-factor lineage that diverged from `antlr4`** — so it is not a clean merge.

## The real semantic content (everything else is cosmetic)

| # | Change | Files | In antlr4? | State |
|---|--------|-------|-----------|-------|
| 1 | **`ADP_model` consolidation** — collapse `partition_model` + `temperature_factor_model` + 3 dead booleans (`use_Voronoi_atoms`, `use_KM_atoms`, `use_zero_overlap`) into one `ADP_model` string; ~7 new procs (`set_`/`read_`/`docu_ADP_model`, `ADP_model_is_one_centred`, `read_thermal_smearing_model` shim); new `DIFFRACTION_DATA` fields `ADP_model`, `use_sphericized_FFs`; new SF dispatch cases `tc-coppens`/`tc-stewart`/`oc-crystal23` | crystal, diffraction_data.{read,set,put,inq}, molecule.{scf,xtal,har,ce} | **No** | Real & coherent, but **WIP**: dangling `set_partition_model` call breaks `run_har.foo`; `use_sphericized_FFs` field never wired; inconsistent `ENSURE` whitelists |
| 2 | **`get_sphericized_HA_FFs`** — the namesake spherical-atom-model form-factor routine (~330 lines) | molecule.scf | **No** | ⚠️ **Hollow placeholder** — a verbatim clone of the aspherical `get_Hirshfeld_atom_FFs` with no sphericization logic (key steps commented out). Not usable. |
| 3 | **Gaussian-IAM hook** — honour each atom's `use_IAM_ITC_FFs` instead of forcing tabulated ITC form factors | molecule.scf | **No** | Real, small |
| 4 | **Re-enable isotropic-H ADP refinement** (`refine_H_U_iso` → `set_isotropic_H_ADP`) | molecule.har | **No** | Real, one line |
| 5 | **Drop `tc-mulliken` partition** option (+ ENSURE guards) | shell2, molecule.xtal | **Conflicts** | `antlr4` commented the whole partition block out instead — incompatible resolution |

**The cosmetic/attribute majority** (≈95%+): `point`/`weight`→`pts`/`wts`, `ft`→`FT`, `l`→`L`,
`gen_oh`→`gen_Oh` renames; attribute-column reformatting; `! NOT PURE` bookkeeping; and edits
*inside already-commented-out code* (e.g. crystal.foo ~85%, molecule.misc/prop). **Zero grid
data changed, zero new grids, zero algorithm change** in lebedev/quadrature/shell2.

- Already in `antlr4` (convergent renames — nothing to port): lebedev `pts/wts` (full),
  becke-grid member renames, `weight_is_0`→`wt_is_0`, spherical_harmonic accessors.
- Not in `antlr4`, trivial if wanted: `make_ft`→`make_FT` case rename; quadrature procedure-name
  shortenings (`set_points_and_weights`→`set_pts_and_wts`, …).

## Why it's not a merge

`antlr4` and SAM carry **different form-factor lineages** — `antlr4` uses
`get_Hirshfeld_atom_FFs_disk` and has **no** `make_stockholder_atom_grid*` at all. SAM's SCF/HAR
edits are written against a structure `antlr4` no longer has, so they would not drop in cleanly
even setting aside the WIP breakage.

## Recommendation

- **No wholesale merge** — it imports known-broken WIP onto a divergent lineage.
- **Take by hand only if wanted (tiny):** the isotropic-H re-enable (#4); the quadrature
  procedure-name completions.
- **`ADP_model` (#1) is the only substantial feature** — treat it as *"decide whether to rename
  the user-facing config keys, then reimplement deliberately on `antlr4`"*, not a cherry-pick.
- **Drop #2 (hollow) and #5 (conflicting).**

SAM currently holds 4 unique commits, so it is **not** in the safe-to-delete set until this work
is extracted or explicitly abandoned. Nothing here blocks `antlr4`; nothing is urgent.
