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
| 3 | **Gaussian-IAM hook** — honour each atom's `use_IAM_ITC_FFs` instead of forcing tabulated ITC form factors | molecule.scf | **Already in antlr4** | Superseded. On antlr4 the routine lives in `molecule.rho` as `make_unique_IAM_atom_FFs` (not `molecule.scf:get_unique_IAM_atom_SFs`); it already dropped the forced `set_use_IAM_ITC_FFs(TRUE)` wrapper (commit `1e625c1dd`, 2026-03-29), `atom.foo:make_FT` dispatches on the per-atom flag, and `molecule.har` wires the density-matrix path when the flag is FALSE. SAM's edit is a strict subset — nothing to port. |
| 4 | **Re-enable isotropic-H ADP refinement** (`refine_H_U_iso` → `set_isotropic_H_ADP`) | molecule.har | **No → done** | Applied by hand on antlr4 (2026-07-27): uncommented the guarded call at `molecule.har.foo:1491`. No-op unless `refine_H_U_iso= TRUE` (defaults FALSE). |
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
- **`ADP_model` (#1)** was investigated in depth (see Appendix A) and the redesign was **rejected**:
  the current `antlr4` `partition_model` enumeration already encodes the correct `(partition, ADP)`
  pairs — **keep it as-is**. Only the test-neutral `oc-sph-tvfa`→`oc-sph-tfva` typo fix was taken.
- **Drop #2 (hollow) and #5 (conflicting).**

SAM currently holds 4 unique commits, so it is **not** in the safe-to-delete set until this work
is extracted or explicitly abandoned. Nothing here blocks `antlr4`; nothing is urgent.

---

# Appendix A — `SF_model`: a clean single-axis redesign on `antlr4`

> **⛔ WITHDRAWN (2026-07-28) — do not implement.** The whole scheme below rests on a false
> premise: that every `tc` model can share the Tanaka partition, so the value's suffix is free to
> mean the ADP model. **That is wrong.** `tc-coppens` and `tc-stewart` legitimately use the
> **Mulliken** density partition (`½(fa+fb)`) with their *own distinct* two-centre ADP formulas
> (`make_temperature_factors`, `shell2.foo:2519-2543`); only `tc-tanaka` uses the Tanaka partition.
> So `partition` and `ADP` are genuinely **independent** in the `tc` case, and the current
> `antlr4` `partition_model` enumeration already encodes the correct `(partition, ADP)` pairs
> (`oc-*` = one free axis, ADP ≡ Debye–Waller; `tc-coppens|stewart` = Mulliken + own ADP;
> `tc-tanaka` = Tanaka + Tanaka; `none` = neither). **Conclusion: keep the current `antlr4` scheme
> as-is.** The earlier "swapped names / hollow stubs" read was a mistake (it looked only at the
> partition dispatch and missed the temperature dispatch, where coppens/stewart/tanaka differ).
> The only change taken from this analysis is the test-neutral `oc-sph-tvfa`→`oc-sph-tfva` typo fix
> (`molecule.scf.foo:215`). Text below is retained only as a reasoning trail.

**Date:** 2026-07-28. This supersedes item #1's "reimplement `ADP_model` deliberately" note.
It is the agreed **spec** (not yet code) for replacing the tangled `partition_model` /
`temperature_factor_model` pair with one honestly-named field. Written with the domain author.

## The insight

There are conceptually **three** variables — but they are **not independent**, so only **one**
needs to be stored:

1. `ADP_model_kind` — `oc` (one-centre) or `tc` (two-centre). *The prefix.*
2. `partition_model` — how the density is divided into atoms.
3. `ADP_model` — the thermal-smearing (Debye–Waller / two-centre) model.

The two kinds are **asymmetric** in which of vars 2/3 the value's suffix carries:

- **`oc`:** partition varies (Hirshfeld, Salvador, …); the ADP model is **always Debye–Waller**.
  → the **suffix is the partition**, ADP is fixed.
- **`tc`:** in full generality both partition (`mulliken`|`tanaka`) *and* ADP
  (`coppens`|`stewart`|`tanaka`) are free — a genuine 2-D space that a single suffix cannot encode.

**Domain simplification (locked in):** *deactivate the two-centre Mulliken partition.* Then every
`tc` model uses the **`tanaka` partition**, collapsing var 2 to a constant — so the suffix is free
to mean the **ADP model**. One stored field suffices again.

`temperature_factor_model` is already dead on `antlr4` (field + all uses commented out); the
one/two-centre distinction is already driven off the partition string's prefix. So this is
cleanup toward what the live code already does — not new machinery.

## The one stored field: `SF_model`

Named `SF_model` because its value selects *how the density becomes structure factors* — it
honestly spans both parts, unlike "partition_model" (which names only the suffix) or "ADP_model"
(which names only the derived kind). The other three variables are **derived accessors**, never
stored.

### Canonical value set + derivation

| `SF_model` | `ADP_model_kind` | `partition_model` (derived) | `ADP_model` (derived) | status |
|---|---|---|---|---|
| `oc-hirshfeld`   | oc | hirshfeld   | Debye–Waller | live |
| `oc-g-hirshfeld` | oc | g-hirshfeld | Debye–Waller | live |
| `oc-salvador`    | oc | salvador    | Debye–Waller | live |
| `oc-g-salvador`  | oc | g-salvador  | Debye–Waller | live |
| `oc-sph-tfva`    | oc | sph-tfva    | Debye–Waller | live *(after typo fix, below)* |
| `oc-crystal23`   | oc | crystal23   | Debye–Waller | live |
| `oc-tanaka`      | oc | tanaka      | Debye–Waller | **commented out** — revive later (needs `make_X_SFs_TAR`) |
| `tc-coppens`     | tc | tanaka      | coppens      | live |
| `tc-stewart`     | tc | tanaka      | stewart      | live |
| `tc-tanaka`      | tc | tanaka      | tanaka       | live |
| `tc-mulliken`    | tc | mulliken    | —            | **commented placeholder** — revives the 2-centre Mulliken partition |
| `static`         | —  | none        | none         | live — no partition, no smearing |

**Derivation rule** (single source of truth = the prefix):
- `ADP_model_kind` = the `oc`/`tc` prefix (`static` ⇒ neither).
- `partition_model` = **oc:** the suffix · **tc:** `tanaka` (fixed) · **static:** `none`.
- `ADP_model`       = **oc:** `Debye-Waller` (fixed) · **tc:** the suffix · **static:** `none`.

## Accessibility — verified in the tree (2026-07-28)

Every live pathway already exists; nothing new to invent.

- **oc form-factor routines** all defined in `molecule.rho.foo`: `make_Hirshfeld_atom_FFs`
  (4554), `make_gHirshfeld_atom_FFs` (4874), `make_Salvador_atom_FFs` (4659),
  `make_gSalvador_atom_FFs` (4983), `make_sph_TFVA_atom_FFs` (4758), `make_C23_Hirshfeld_atom_FFs`
  (5081). Reached via `make_X_SFs` → `make_X_SFs_HAR`.
- **tc path** reached in **both** plain forward SF calc (`make_X_SFs` → `make_X_SFs_RF` →
  `make_ft` @ `molecule.xtal.foo:496/560` → `make_FT_pair` → `make_partition_factors` +
  `make_temperature_factors`) **and** constrained-wavefunction / PND
  (`make_r_constraint` / `make_gc_constraint` / `make_pnd_constraint` → `make_FT_pair`). **Not** HAR.
  *(Corrects an earlier note that put `make_partition_factors` only in the constraint builders — a
  grep artefact.)*
- **`make_temperature_factors`** (`shell2.foo:2491`) already implements `coppens`, `stewart`,
  `tanaka`, `none`. **`make_Tanaka_partition`** exists (`shell2.foo:4774`) — so "all `tc` use the
  tanaka partition" is a direct edit.
- **`static`** = existing `none`→`part=ONE`,`TF=ONE` path; a value rename only.

## Edit plan (per file)

1. **`types.foo`** — rename `DIFFRACTION_DATA%partition_model` → `SF_model`
   (`STR, readonly DEFAULT("oc-hirshfeld")`); delete the (already-commented) `temperature_factor_model`;
   add reserved `use_sphericized_FFs :: BIN, readonly DEFAULT(FALSE)`.
2. **`diffraction_data.inq.foo`** — add the three derived accessors `ADP_model_kind`,
   `partition_model`, `ADP_model` implementing the rule above.
3. **`diffraction_data.set.foo`** — `set_SF_model(val)` with an `ENSURE` whitelist **generated from
   the live value set** (so whitelist ≡ dispatch, the bug SAM hit); retarget the existing
   `tfva`/`sph-tfva` alias block; add `set_use_sphericized_FFs`.
4. **`diffraction_data.read.foo`** — `sf_model=` primary keyword; keep `partition_model=` as an
   alias → `read_SF_model`; `temperature_factor_model=` / `thermal_smearing_model=` remain a
   deprecation shim; canonicalise `none`→`static`; add `use_sphericized_ffs=` + `docu_SF_model`
   (list the value set; mark `use_sphericized_FFs` **"reserved — not yet implemented"**).
5. **`crystal.foo`** — rename the stored accessor to `SF_model`; add delegating
   `ADP_model_kind` / `partition_model` / `ADP_model`; point `uses_Hirshfeld_atoms` etc. at the
   derived `partition_model`.
6. **`molecule.scf.foo`** — `make_X_SFs` dispatch (201) keys on `SF_model` (oc→HAR, tc→RF,
   static→RF); **fix the typo `oc-sph-tvfa` → `oc-sph-tfva`** (`scf:215`) so the reader's canonical
   value actually matches a case. **Keep the printout byte-identical** — print the `SF_model` value
   under the existing `Density partition model ...` label; **do not** add/remove echo lines (see
   *Test impact* below — any stdout text change fails the loose comparator).
7. **`molecule.xtal.foo`** — `make_FT_pair` (957) passes the derived **`ADP_model`** (not the raw
   string) to `make_temperature_factors`; `make_partition_factors` (1048) switches on derived
   `partition_model`: `case ("none")`, `case ("tanaka") make_Tanaka_partition`, plus a **commented**
   `! case ("mulliken"); part = HALF*(fa+fb)` placeholder. The old `tc-coppens`/`tc-stewart` →
   `HALF*(fa+fb)` ("Use Mulliken") lines are deleted.
8. **`shell2.foo`** — `make_temperature_factors` (2491) switches on `ADP_model` with cases
   `none`/`coppens`/`stewart`/`tanaka` (rename off the `tc-` prefixes).

**Implementation nuance:** `oc` dispatches (rho FF selector @ 4543, scf HAR routing) may key on
`SF_model` directly — for `oc`, `SF_model` *is* the partition selector — so only the `tc`
partition/temperature dispatches consume the derived `partition_model`/`ADP_model`. This minimises
churn while keeping the semantics above.

## Test impact (`tests/`)

The loose comparator (`scripts/test.py`) compares only differing lines, token by token: numeric
tokens get the tolerance, but **non-numeric tokens must match exactly** and a **differing token
count fails**. So any changed/added/removed non-junk stdout line is a hard fail — keyword and echo
churn count as much as numbers.

- **23 tests set the keyword**; values used are `oc-hirshfeld` (majority), `tc-tanaka` (×2),
  `tc-stewart` (×4). None use `none`, `sph-tfva`, or `tc-mulliken`.
- **Zero-churn for the ~19 `oc-hirshfeld`/`tc-tanaka` tests** — *by design*: keep `partition_model=`
  as an input **alias** (no `stdin` edits; the `keyword found --> partition_model=` echo is
  unchanged), and keep the `Density partition model ...` echo **byte-identical** (print the
  `SF_model` value, add/remove no lines). For these tests `SF_model` ≡ the old value.
- **`none`→`static` and the `tvfa` typo fix are test-neutral** — no test exercises those paths.
- ⚠️ **Non-minor: the 4 `tc-stewart` tests** (`nh2cn …structure_factor_range`, `…SF_stl_limit`,
  `…structure_factors`; `urea_x-ray-constrained-uhf…ELF_plot`). The scheme changes `tc-stewart`'s
  partition **Mulliken `HALF*(fa+fb)` → Tanaka**, a genuine **numerical** change (SFs, R-factors,
  χ²). Their reference `stdout` must be **regenerated and physically validated**, not text-patched.
  **2 of the 4 are in `tests/short/` (the CI suite)** → CI is red until they are regenerated.

## Locked decisions

- **`tc-mulliken`** — kept only as a **commented placeholder** in `make_partition_factors`; not a
  live value.
- **`oc-tanaka`** — left **commented out**; revive later with `make_X_SFs_TAR` (out of scope now).
- **`oc-sph-tvfa`/`oc-sph-tfva` typo** — **fixed** in this pass.
- **`use_sphericized_FFs`** — switch + keywords added, but **no consumer** and **no**
  `get_sphericized_HA_FFs` (SAM's hollow clone is *not* ported); documented as reserved.
- Not adopted from SAM: renaming the field to `ADP_model` (it names only the derived kind), and the
  `get_sphericized_HA_FFs` procedure.

---

# Appendix B — how `partition_model` actually works on `antlr4` (the current, correct scheme)

**Date:** 2026-07-28. **This is the live reference** (Appendix A is withdrawn). The current
`antlr4` `partition_model` field is correct — **keep it as-is**. Each value is a valid, distinct
`(density partition, ADP/thermal-smearing)` pair:

| `partition_model=` | density partition | ADP / thermal model | notes |
|---|---|---|---|
| `oc-hirshfeld`   | Hirshfeld            | Debye–Waller | |
| `oc-g-hirshfeld` | grid Hirshfeld       | Debye–Waller | |
| `oc-salvador`    | Salvador (TFVA)      | Debye–Waller | |
| `oc-g-salvador`  | grid Salvador        | Debye–Waller | |
| `oc-sph-tfva`    | spherical TFVA       | Debye–Waller | *(dispatch typo `tvfa` fixed 2026-07-28)* |
| `oc-crystal23`   | CRYSTAL23 density    | Debye–Waller | |
| `oc-tanaka`      | Tanaka (one-centre)  | Debye–Waller | **reserved** — commented (`make_X_SFs_TAR`) |
| `tc-coppens`     | **Mulliken** `½(fa+fb)` | Coppens `½(e^Ta+e^Tb)` | |
| `tc-stewart`     | **Mulliken** `½(fa+fb)` | Stewart `exp(-½·g·S(Ua+Ub)S)`, bond-length `g` | |
| `tc-tanaka`      | Tanaka               | Tanaka | |
| `none`           | none                 | none (static, unpartitioned) | |

**The two regimes:**
- **`oc-*` (one-centre):** partition varies, ADP is *always* Debye–Waller — so the two axes
  collapse to one free choice. Path: `make_X_SFs` → `make_X_SFs_HAR`; per-scheme static form
  factors from the `molecule.rho.foo:4543` dispatch. **HAR never touches
  `make_partition_factors`/`make_temperature_factors`.**
- **`tc-*` (two-centre):** partition and ADP are **independent**; the enumeration lists the valid
  pairs. Path: `make_X_SFs` → `make_X_SFs_RF` (forward SF) *and* the constraint builders
  (`make_r_constraint`/`make_gc_constraint`/`make_pnd_constraint`, constrained-wavefunction / PND).
  Partition dispatch = `make_partition_factors` (`molecule.xtal.foo:1048`); ADP dispatch =
  `make_temperature_factors` (`shell2.foo:2514`).

The `! Use Mulliken` comment on `tc-coppens`/`tc-stewart` in `make_partition_factors` is **correct
documentation** — those two-centre ADP models legitimately use the Mulliken density partition —
**not** a stub or mislabel. This is the point that dissolved the whole `SF_model` confusion.
