# Working notes — automatic call resolution + DCE

Companion to `CALL_RESOLUTION_TASK.md` (the handoff brief). This file tracks what
has actually been done/discovered, phase by phase. Started 2026-07-09.

## Environment note

The ANTLR jar is at `external/antlr-4.13.2-complete.jar`, **not** the
`/usr/local/lib/...` path documented in `CLAUDE.md` §8 / `build_translator.sh`.
Build with `ANTLR_JAR="$PWD/external/antlr-4.13.2-complete.jar" scripts/build_translator.sh`.

Regenerating all files is slow: the translator runs **one file per JVM** (~1.5 s
startup × 257 foo files ⇒ ~7 min per full regen). Phase A needs many before/after
regens for the zero-diff check ⇒ a **batch mode** (process many `--foo`, or a
`--foo-dir`, in one JVM) is worth adding first. See plan.

## Phase 0 — feasibility confirmation (done)

### Collision scan (`scripts/collision_scan.py`)
Confirms the brief exactly: **2** classes split across submodules
(`DIFFRACTION_DATA`: 0 collisions; `MOLECULE`: **6** collisions). The 6:
`make_ED_grid`, `make_Fock_mx`, `make_r_overlap_inverse_sqrt`,
`make_r_overlap_sqrt`, `make_stockholder_atom_weight`, `test_plot_info`.
8232 distinct proc names seen. Must report **0** after renames.

### Qualified-call inventory (refines the brief's rough counts)
Shape-classified over `foofiles/*.foo` (excludes decls & array sections):

| Form | Count | Notes |
|---|---|---|
| `.SUBMOD:proc` (self, generic) | 1708 | dot + uppercase submod + `:` |
| `.SUBMOD::proc` (self, non-generic) | 5 | |
| `.MAIN:proc` (self→main module) | 16 | |
| `.:proc` / `.::proc` (same submodule) | 557 / 2 | |
| `recv.SUBMOD:proc` (receiver form) | 410 | e.g. `.mol.SCF:merge_group_MOs` |
| `recv.SUBMOD::proc` | 4 | |
| **submodule-qualified subtotal** | **~2702** | |
| `TYPE:proc` (module-qualified, generic) | 1082 | e.g. `VEC{REAL}:sum_elements` |
| `TYPE::proc` (module-qualified, non-generic) | 1242 | e.g. `TEXTFILE::destroy` |
| **TYPE-qualified subtotal** | **~2324** | brief's rough "~5178" was ~2.2× high (array-section / get_from false positives, as it warned) |
| **GRAND TOTAL to convert** | **~5026** | |

Top TYPE qualifiers: `GAUSSIAN_DATA` (1442!), `OBJECT` (285), `SHELL1QUARTET`,
`ATOM`, `VEC{REAL}`, `VEC{ATOM}`, `REAL`, `ISOSURFACE`, …

## Existing translator infrastructure (already resolves qualified calls)

The registry-based resolution **already exists** — Phase A mostly reuses it:
- `buildSubmethodTable` (~L176): `base type → method → {defining submodule Fortran
  modules}`. Only records submodule files (dotted module header). `MOLECULE.MAIN`
  IS recorded ⇒ `MOLECULE_MAIN_MODULE`.
- `trailerCallModule` (~L2198): named submod → verbatim `<TYPE>_<SUB>_MODULE`;
  `.:`/`.MAIN:` → **registry lookup** to the defining submodule (prefers current
  so `recordUse` self-skips). This is the auto-resolution path bare `.proc` will use.
- `recordCall` (~L2165): bare `.proc` on self → registry lookup; unambiguous once
  the 6 collisions are renamed.
- `buildSelflessMethods` (~L228), function-return-type tracking, `buildGlobalTable`.

## Zero-diff RISKS discovered (things that break "strip qualifier ⇒ identical Fortran")

1. **Generic `:` vs non-generic `::` is textually load-bearing.** `.SET:proc`
   emits `proc_` (generic interface); `.SET::proc` emits `proc` (specific). The
   bare `.proc` self path (L1840) *always* emits `sel + "_"`. So the ~11 `::` dot
   sites + **1242 `TYPE::` sites** cannot merely drop the qualifier — the
   translator would emit the generic name and change the text. **Decision needed:**
   keep a non-generic marker in the bare notation (e.g. `.::proc` retained) vs
   have the translator emit specific when no generic interface exists.
2. **Selfless procs.** `.SET:proc` on a `selfless` target passes no `self`; the
   bare `.proc` self path (L1840) always appends `self`. Must consult
   `selflessProcs`/`selflessGlobal` in the bare path too.
3. **`.MAIN:` path inconsistency.** `submoduleModule("MAIN")` → `MOLECULE_MODULE`
   but `trailerCallModule(...,"MAIN",...)` → registry (`MOLECULE_MAIN_MODULE`).
   Confirm which the 16 `.MAIN:` sites actually hit and that bare-form resolution
   matches it.
4. **`recv.SUBMOD:` receiver forms (414).** Resolution keys off the receiver's
   inferred foo type (`curType`); must confirm type inference is complete enough
   that bare `recv.proc` resolves to the same module.

## Baseline snapshot (done)
Frozen (read-only) at `<scratchpad>/baseline/` — all 257 `foofiles/`+`runfiles/`
regenerated with the CURRENT (unmodified) translator. **257/257 OK, 0 failures**;
733 outputs (257 `.F90`, 238 `.int`, 238 `.use`). This is Phase-A snapshot #1
(pre-rename). 5 pre-existing ANTLR error-recovery lines on stderr (translator
recovered, exit 0) — deterministic, so they cancel in before/after diffs. Not our
bug; out of scope for Phase A.

## Decisions (maintainer, 2026-07-09)
- **Q1 generic vs `::`: KEEP A SPECIFIC MARKER.** Strip only the qualifier NAME,
  keep the colon-count. Target notation:
  `.SET:proc`→`.proc`, `.SET::proc`→`.::proc`,
  `TYPE:proc(x,…)`→`x.proc(…)`, `TYPE::proc(x,…)`→`x.::proc(…)`.
  ⇒ generic-vs-specific choice preserved ⇒ strict byte-level zero-diff holds.
- **Q2 collisions: CONSOLIDATE dups, RENAME distinct.** Confirmed table (2026-07-09):

  | Collision | Action | Keep | Delete/rename |
  |---|---|---|---|
  | `make_r_overlap_inverse_sqrt` | consolidate | INTS | delete BASE def (base.foo:2798) |
  | `make_r_overlap_sqrt` | consolidate | INTS | delete BASE def (base.foo:2778) |
  | `test_plot_info` | consolidate | PLOT | delete MAIN def (main.foo:2131) |
  | `make_ED_grid` | rename | GRID `make_ED_grid` | RHO → `make_ED_grid_at_pts` |
  | `make_Fock_mx` | rename | FOCK `make_Fock_mx` | SCF → `make_scf_Fock_mx` |
  | `make_stockholder_atom_weight` | rename | RHO | HAR → `make_stockholder_atom_weight_kappa` |

## Progress log
- **A0 — batch mode: DONE & validated.** Added `--foo-dir`/`--foo-list` to
  `FooToFortran` (registries built once; per-file emission unchanged). Extracted
  `translateOne`. Helper: `scripts/regen_all.sh <out-dir>`. Full regen = one JVM,
  ~5:00 wall for 257 files. **Zero-diff vs locked baseline: 0 files differ,
  identical file set, 257/257 OK.** ✓
- **A1 — collision resolution: DEBUG BUILD VERIFIED** (after cycle fix). Clean
  `debug/` build compiles + links (`debug/tonto`, exit 0, tonto+hart). Clean link ⇒
  no A1 rename missed a caller. Release build for fast tests in progress.
- **A1 source edits** (below) applied per the edit map.
  `collision_scan → 0` ✓. Consolidations: deleted BASE `make_r_overlap_sqrt` +
  `make_r_overlap_inverse_sqrt` (INTS copies kept; repointed 6 `.BASE:` callers →
  `.INTS:`); deleted MAIN `test_plot_info` (PLOT kept; only leftover ref is a
  commented case line at main.foo:578 — harmless). Renames: SCF `make_Fock_mx` →
  `make_scf_Fock_mx` (def + 11 live `.:` + 2 `.SCF:` in main + comments; `.FOCK:`
  untouched); HAR `make_stockholder_atom_weight` → `_kappa` (def + `.:` self +
  `.HAR:`; `.RHO:`/`_int`/`_acc` untouched); RHO `make_ED_grid` → `make_ED_grid_at_pts`
  (def + `.:` self in rho + `.RHO:` in har; GRID copy, `_r`/`_u`/`_r_v2` variants,
  and all `.atom(x).make_ED_grid` ATOM receiver-calls untouched).
  **Regen-diff vs baseline: PASS.** 16 output files differ, ALL `molecule.*` (the
  7 edited sources + their .int/.use); prop/grid/cp/other modules untouched.
  Spot-checked diffs are exactly the intended changes:
  - `molecule.base.use`: dropped `MAT_REAL to_{inverse_,}sqrt_of_` (internal to the
    deleted overlap procs); added `use MOLECULE_INTS_MODULE, only:
    make_r_overlap_{sqrt,inverse_sqrt}_` for the 6 repointed callers.
  - `molecule.scf.int`: `make_Fock_mx_` interface → `make_scf_Fock_mx_`.
  - `molecule.main.use`: dropped every `use` pulled in only by the deleted
    `test_plot_info`; `make_Fock_mx_` → `make_scf_Fock_mx_`.
  Pre-A1 release binary preserved at `<scratchpad>/tonto_preA1_release` (baseline
  for classifying any test failure as pre-existing vs introduced).
  Pending: debug build link + `tests/` suite under loose criterion.

## GOTCHA — stale `.mod` in incremental build dirs
First `debug/` build (incremental over a prior-session state) failed:
`Fatal Error: Mismatch in components of derived type 'molecule_type' ... expecting
'partition_info_made', but got 'atomic_moments_made'` at `molecule.ints.F90.o`.
NOT an A1 regression: `atomic_moments_made` is gone from ALL current sources (an
earlier session renamed it → `partition_info_made`), and 17 `.mod` files in `debug/`
predated this build. `molecule.ints` compiled against a **stale `.mod`** carrying the
old `molecule_type` layout. Fix: `make clean` then rebuild (gfortran `.mod` files are
gzipped, so a partial/incremental regen leaves inconsistent module layouts).
**Always clean-build `debug/` and `release/` for this task.**

## CYCLE — A1 consolidation direction reversed (BASE↔INTS)
The maintainer's "keep INTS" for `make_r_overlap_sqrt`/`make_r_overlap_inverse_sqrt`
created an **illegal circular `use`**: `MOLECULE_INTS → MOLECULE_BASE` pre-existed
(INTS uses `expectation_`, `make_max_S_for_shell_pairs_`), and repointing BASE's
callers to INTS added `MOLECULE_BASE → MOLECULE_INTS`. gfortran clean-build failed:
`Cannot open module file 'molecule_base_module.mod'`. Cycle-check confirmed exactly
one SCC: `MOLECULE_BASE ↔ MOLECULE_INTS` (baseline was acyclic).
**Fix: keep the procs in BASE, delete from INTS** (forced by acyclicity). Safe
because baseline BASE uses none of SCF/PROP/INTS, and SCF/PROP already use BASE
(10/7 edges) ⇒ repointing external `.INTS:make_r_overlap_inverse_sqrt` callers
(scf ×4, prop ×1) → `.BASE:` adds ZERO new edges. `make_r_overlap_sqrt` is
base-internal only. Kept BASE's existing bodies (proven-compilable in the pre-A1
build; the maintainer's PURE preference can't be honored without re-introducing the
cycle — `make_r_overlap_sqrt` loses the `PURE` attr, behaviour identical).
Post-fix cycle-check: **ACYCLIC** (165 modules, 1403 edges). collision_scan still 0.
Interim guard: `<scratchpad>/cycle_check.py <outdir>` (Tarjan SCC over emitted .use
graph) — prototype for the Phase-A step-4 in-translator check to port to Java.

## Verification strategy (per maintainer)
Debug = correctness gate (compiles + links; ENSURE checks live). Release = fast
test-suite run. So: clean debug build (verify link) → clean release build → full
`tests/` suite via `scripts/compare_test_outputs.py -p release/tonto` (loose
criterion). Compare pass/fail against pre-A1 baseline
(`<scratchpad>/tonto_preA1_release`) to prove "unchanged".

## A2 — auto-resolve bare calls + cycle guard (IN PROGRESS)
- **A2 step 1 — cycle-check in translator: DONE & validated.** Added
  `reportUseCycles` (iterative Tarjan SCC over the emitted `use` graph) + graph
  collection in the batch driver (`translateOne` now returns the emitter). Fires
  only in batch mode (`--foo-dir`/multi-`--foo`) — a cycle needs the whole module
  set; per-file CMake builds can't see it (gfortran stays the backstop there).
  On a cycle: prints the chain + offending edges and exits 1 (so `regen_all.sh`
  fails loudly). Unit-tested (2-node, 3-node SCCs, leaf-sink `TYPES` ignored,
  acyclic chain → 0). Full batch on real sources: **acyclic, 257/257, zero-diff
  vs A1 snapshot** (post-pass is output-neutral). Python `cycle_check.py` retained
  as an independent cross-check.
- **A1 TEST VALIDATION: PASS (unchanged).** Post-A1 release suite: GRAND TOTAL
  loose **121/124**. The 3 loose failures — `cyclazine…tddft`, `urea_ccsd…Salvador`,
  `gly_ala_fragHAR` — ALL also fail on the pre-A1 binary (targeted rerun, both
  exit 1) and are maintainer-confirmed known-problematic. **A1 introduced zero
  regressions.**
- **A2 step 2 — bare-call resolution: IN PROGRESS.** Pilot (DIFFRACTION_DATA)
  revealed the **selfless gap** (risk #2): source `DIFFRACTION_DATA.READ:docu_X(die)`
  (docu_* are `selfless`, local to the READ submodule) — the type-qualified path
  drops self, but the bare dot-method path appended it, emitting the illegal
  `docu_X_(DIFFRACTION_DATA,die)`. **Fix applied:** the two self-call bare paths
  (`.proc` dot-method L~1951, `.SUBMOD:`/`.::proc` on self L~1929) now honor
  `selfless` (set `pendingNoRecv`, pass no self) — mirroring the existing
  type-qualified path. Also learned the naive `.SUBMOD:`→`.` sed is wrong for
  `TYPE.SUBMOD:proc`: must strip the `TYPE.` too. Converter drafted
  (`convert_submod.sh`). Validating: full regen zero-diff (selfless fix must not
  change current qualified output) → then diffraction conversion zero-diff.

## A2 step 2 (details)
**Selfless-fix iterations (zero-diff-driven):**
1. First cut used `selflessProcs || selflessGlobal` → full regen changed **21 files**
   (false positives: `chemical_symbol_(self)`→`chemical_symbol_`, `destroy_data`,
   `show`, …). `selflessGlobal` is name-based across ALL modules → drops self for
   same-named normal methods. Reverted to **local `selflessProcs` only**.
2. Local-only still changed **3 files** (atom, crystal, vec_atom): these names are
   **overloaded** — some overloads `selfless`, some not (`chemical_symbol` no-arg
   self + `chemical_symbol(Z,A)` selfless; `make_pos_covariance_mx`; `has_N_connections`).
   The bare no-arg call targets the non-selfless overload. Fix: `selflessProcs` now
   holds a name only if **EVERY** overload is selfless (pre-pass counts selfless vs
   total overloads per name). Doesn't affect the type-qualified path L~2053 (dominated
   by `selflessGlobal`, a superset). → previously-affected 21 files all **zero-diff**.
3. Note: local-only means a genuinely-cross-submodule bare selfless call wouldn't drop
   self — but none exist in current sources (they'd miscompile), and the diffraction
   `docu_*` are local. Revisit if MOLECULE needs cross-submodule selfless.

Converter `convert_submod.sh`: strips `CLASS.SUBMOD::`→`.::`, `CLASS.SUBMOD:`→`.`,
`.SUBMOD::`→`.::`, `.SUBMOD:`→`.` (TYPE-prefixed first). `TYPE:proc` (module-qualified,
receiver=first-arg) deferred — needs arg-aware rewrite `TYPE:proc(x,..)`→`x.proc(..)`.
Applied to the 4 diffraction files; validating full-regen zero-diff (a4 vs a2).

**By-name procedure-reference gap (risk: functional-interface args).** Source
`VEC{REAL}:min_BFGS(self, DIFFRACTION_DATA.INQ::chi2F, DIFFRACTION_DATA.INQ::d_chi2F,…)`
passes `chi2F`/`d_chi2F` (public functions in INQ) BY NAME. Converting to `.::chi2F`
hit the callHead `.::proc` path, which emitted a call `chi2F(self)`. Fix: callHead
`.::proc` now emits the bare specific name (by-name, + recordUse) when it is **not
immediately called AND in value/argument position** (`!statementPos`). The
`!statementPos` guard is essential: `.TD::do_r_CIS_S0_IC` as a STATEMENT is a no-arg
call (`call do_r_CIS_S0_IC(self)`), which the first cut broke (regressed
molecule.td.F90). After the guard: molecule.td zero-diff restored, diffraction
set.F90 by-name preserved (`min_BFGS_(self,chi2F,d_chi2F,…)`).

**DIFFRACTION_DATA pilot result:** all 4 submodule files convert to bare form with
**0 non-comment diffs**; only cosmetic comment-text diffs remain (`.READ:proc`→`.proc`
inside `! case(...)` lines — behaviorally irrelevant; keeps comments consistent with
code). Translator fixes (selfless all-overloads-local, `.::` by-name) proven
output-neutral on all non-diffraction files. Final full-regen check: a6.

**A6 RESULT — DIFFRACTION_DATA PILOT COMPLETE.** Full regen 257/257, acyclic.
NON-diffraction differing = **0**; diffraction NON-COMMENT diffs = **0**; only 2
diffraction files (read/set) have cosmetic comment-text diffs. The A2 mechanism
(translator selfless/by-name handling + `convert_submod.sh`) is proven on a full
submodule-split class. Generated Fortran behaviorally byte-identical ⇒ binary
identical ⇒ no rebuild needed to prove the pilot.

**Module-aware selfless registry: DONE (validating a7).** Added
`buildSelflessByModule` → `Map<fortranModule, {proc names selfless in ALL overloads}>`
(same line-scan + module-naming as buildSubmethodTable; skips templates). Threaded
through main/translateOne/ModuleEmitter. New helpers: `callModule(fooType,method)`
(extracted from recordCall — the module a `.method` resolves to) and
`isSelflessCall(mod,method)` (local `selflessProcs` OR the resolved module's
all-overloads-selfless set). Both bare-path selfless checks now use it → handles
CROSS-submodule selfless (needed for MOLECULE) without false-dropping same-named
normal methods. a7 = full regen vs a6 (must be 0: module-aware ≡ local on current
sources).

**MOLECULE submodule conversion: DONE (a8).** Converted all 19 molecule.*.foo
self-forms (2732 sites: `MOLECULE.SUBMOD:`→`.`, `.SUBMOD:`→`.`, `.:`→`.`, `.SUBMOD::`
/`MOLECULE.SUBMOD::`→`.::`; receiver forms `.mol.SCF:proc`→`.mol.proc` fall out of the
same `.SUBMOD:`→`.` rule and resolve via the receiver's type). Full regen a8 vs a7:
**NON-molecule differing = 0; molecule NON-COMMENT diffs = 0; acyclic**; 25 files with
cosmetic comment-only diffs. Receiver forms + cross-submodule selfless (module-aware
registry) + `.::` by-name all reproduce the qualified output exactly.

⇒ **All `.SUBMOD:` / `.:` / `TYPE.SUBMOD:` submodule-qualified call syntax is now
eliminated** for both DIFFRACTION_DATA and MOLECULE, with behaviorally byte-identical
Fortran. Binary would be identical ⇒ tests unchanged.

**TYPE:proc — maintainer scope = METHOD CALLS ONLY.** GAUSSIAN_DATA is a *namespace*
(public module vars `nx/ny/nz/index_p1/…` + selfless fns), NOT a method receiver —
`GAUSSIAN_DATA::nx` etc. stay qualified (converting would need a global public-symbol
registry + codebase-wide ambiguity handling; deferred). Convert only true
self-passing method calls `TYPE:proc(x,rest)`→`x.proc(rest)`.

Converter `convert_typeproc.py` (paren-aware): builds `methodsByModule` (non-selfless,
non-template proc headers after `contains`), converts `QUAL:proc(` only if proc is a
method of QUAL's module; extracts the first arg (matched-paren) as receiver; keeps
`::`→`.::`. EXCLUDES: namespace vars / selfless / module-var indexing / get_from /
**expression first-args** (`.weight*val` — receiver-promotion adds parens, non-zero-diff;
left qualified). Pilot (adaptive_quadrature): 9 converted, 6 expr-receiver left, 0
non-comment diff. Applied to all foofiles (~30 files: shell1quartet 62, vec{atom} 55,
isosurface 37, molecule.fock 28, …), idempotent.

**TYPE:proc method-call conversion: REVERTED (unsafe to mechanize).** Full regen a9
FAILED validation: the in-translator cycle-check fired
(`INT→STR→VEC_BIN→VEC_INT→VEC_STR→INT`) plus ~14 files had non-comment diffs. Root
cause (maintainer-confirmed): **`elemental` methods applied to arrays**. In Fortran
`x%stl` ≡ `x(:)%stl` — an elemental component slice over a derived-type array; and
`STR::lower_cased(allowed)` with `allowed : VEC{STR}` is the `elemental` `lower_cased`
applied per element. The converter assumed first-arg type == qualifier, which is FALSE
here: the method lives on the ELEMENT type (STR) but the receiver is `VEC{STR}`, so
`allowed.::lower_cased` mis-resolved to VEC_STR_MODULE → bogus `use` → the cycle. Also
component-name procs (`x.stl`→`x%stl`, benign but non-byte-identical). A safe converter
needs real type inference (detect elemental + array receiver, resolve against element
type) — beyond a mechanical transform. Reverted cleanly (git-checkout pure-typeproc
files; git-checkout + re-apply submodule + re-apply A1 renames for the 8 dual
molecule/diffraction files). **Recovery validated: full regen a10 vs a8 = 0 files
differing (incl comments), acyclic.**

## FINAL STATE (this commit)
All `.SUBMOD:`/`.:`/`.MAIN:`/`recv.SUBMOD:`/`TYPE.SUBMOD:` submodule-qualified call
syntax eliminated for DIFFRACTION_DATA + MOLECULE; behaviorally byte-identical Fortran
(only cosmetic comment-text diffs). Translator gained: batch mode (`--foo-dir`),
in-translator cycle check (Tarjan), module-aware selfless registry, `.::` by-name
handling. A1 collisions resolved (0). Milestone-3 tests unchanged (loose 121/124, all
3 failures pre-existing/known). `TYPE:proc` module-namespace (GAUSSIAN_DATA::) and
method calls left qualified (out of safe mechanical scope). Grammar NOT yet tightened
(still accepts old forms — deliberate, so the old syntax still parses). Finding: the bare/dot
  forms (`.proc`, `.::proc`, `recv.proc`, `recv.::proc`) are the translator's
  fundamental call mechanism and mostly already resolve via the registry. Plan:
  pilot-convert `DIFFRACTION_DATA` submodules (0 collisions, self-contained) to
  bare form, regen, diff vs snapshot; catalog + fix gaps (selfless self-passing,
  `.MAIN:`, receiver-type inference), then scale to MOLECULE + `TYPE:proc`.

## A1 precise edit map (call sites to repoint)

**Caution:** `make_ED_grid` is ALSO an `ATOM` method (`coppensbasis.foo`,
`slaterbasis.foo`); `.atom(x).make_ED_grid(...)` are receiver-calls on atoms —
**do NOT touch**. Only MOLECULE self-calls matter.

- `make_r_overlap_inverse_sqrt`: delete base.foo def; repoint `.BASE:` callers
  (base.foo:2488,2603,2642) → `.INTS:`. (INTS callers in scf/prop unchanged.)
  Bodies byte-identical ✓.
- `make_r_overlap_sqrt`: delete base.foo def; repoint `.BASE:` callers
  (base.foo:2474,2590,2635) → `.INTS:`. INTS copy is `PURE`+explicit self; args
  compatible (1 arg).
- `test_plot_info`: delete main.foo def (2131). Only ref in MAIN is a COMMENTED
  case (main.foo:578) — no live MAIN caller. PLOT copy kept.
- `make_Fock_mx` → SCF def (scf.foo:1550) renamed `make_scf_Fock_mx`. Repoint
  SCF-internal `.:make_Fock_mx` (scf.foo:1696,1738,1776,1829,2065,4670,4708,4727,
  4763,4810,6758) and `.SCF:make_Fock_mx` (main.foo:313,314). LEAVE
  `.:make_Fock_mx_guess_MOs`, `.FOCK:make_Fock_mx` (FOCK copy), and `!`-commented.
- `make_stockholder_atom_weight` → HAR def (har.foo:2495) renamed `_kappa`.
  Repoint `.:make_stockholder_atom_weight(...,kappa,...)` in har.foo (2448) and
  `.HAR:make_stockholder_atom_weight` (misc.foo:4177). LEAVE `.RHO:`/rho-internal
  `.:` (RHO copy) and the `_int`/`_acc` variants.

Post-A1 gates: `collision_scan → 0`; no live caller references a deleted/renamed
target; batch regen + build + `tests/` green.
