# Task brief — automatic submodule call resolution + dead-code elimination

Handoff document for a **fresh conversation**. Goal: eliminate the ugly
`.SUBMOD:proc` / `.:proc` dot-qualified call syntax from the `.foo` sources by
having the ANTLR4 translator resolve submodule calls automatically, then use the
resulting call graph to strip untranslated dead procedures. Read this cold; it
captures the plan, the feasibility data, the validation strategy, and the
gotchas.

## 1. Motivation

The legacy `foo.pl` translator never built an AST, so authors had to hand-qualify
cross-submodule calls: `.SET:set_charge(...)`, `.SCF:make_fock(...)`,
`.:proc` / `.::proc` (same submodule), `.MAIN:proc`. With the ANTLR4 parse tree
this qualification can be **inferred**: maintain a registry of which procedures
each submodule provides (plus function return types, already tracked), and let a
bare `.proc(...)` resolve to the right `MOLECULE_XXX_MODULE` on its own.

Bonus: once the translator has the full call graph, it can do **dead-code
elimination** — omit from translation every Fortran procedure never actually
reached from a program entry point, likely cutting compile time.

## 2. Invariants / milestones

- **Milestone 3 is the anchor and must stay green throughout.** The `tests/`
  suite (via `scripts/test.py`, loose criterion rel ≤ 0.2% OR last-digit ≤ 2)
  must pass — or at minimum be *unchanged* — after every phase. This is the
  behavioural guardrail that licenses the refactor.
- **Milestone 2 is redefined** to: the new canonical `.F90` output = the current
  translator output, with (a) submodule calls auto-resolved and (b) dead code
  removed. Byte-equality with `release/` is explicitly abandoned for this work.
- **Freeze a baseline first.** Before any change, snapshot the current
  translator output (regenerate all `foofiles/` + `runfiles/` into a reference
  dir). Phase A is validated as *output-identical* to that snapshot; Phase B as a
  *strict subtraction* of it.

## 3. Decouple into two phases (different risk profiles)

### Phase A — auto-resolve submodule calls (drop `.SUBMOD:` / `.:`)

Low risk. It is a **source-notation change that must produce byte-identical
Fortran**, so validate exactly like the `:::`→`::` change: regenerate all files
before/after and assert **zero diff** in the generated `.F90`/`.int`/`.use`, then
build + run the suite.

Steps:
1. Build/extend the per-class submodule→procedure registry (see §5 — partial
   infrastructure already exists in `buildSubmoduleProcRegistry`).
2. Make a bare `.proc(...)` resolve through that registry to the correct
   `MOLECULE_XXX_MODULE` (emit the right `use`), preserving the existing
   generic (`:`) vs non-generic (`::`) and selfless/self-passing behaviour.
3. Keep the grammar **accepting** the old `.SUBMOD:` forms during the transition
   so sources can be converted incrementally and diffed at each step; tighten the
   grammar to reject them only at the very end.
4. Convert the `.foo` sources: strip the `SUBMOD:` qualifier (and `.:`/`.::`) to
   the bare `.proc` form.

### Phase B — dead-code elimination

Separate, higher risk. Do it only after Phase A is green.

- Build the reachability graph from **all** roots: every `runfiles/run_*.foo`
  main program, and transitively everything they call.
- **Treat indirect edges as live**: procedures passed as arguments
  (`routinal`/`functional` interface args, §8 of the language doc), generic
  dispatch, and anything reached only through `get_from` template expansion or
  CPP macros. A naive "who-calls-whom-by-name" graph WILL miss these and delete
  live code → link failure or test regression.
- Omit only provably-unreachable procedures. Validate by: full build links +
  full suite passes + measure the compile-time delta (don't assume it's large).
- Removing procedures also changes `.int`/`.use` emission — regenerate those
  consistently.

## 4. Feasibility scan results (already run — `scripts/collision_scan.py`)

Only **two** classes are split across submodules:

| Class | Submodules | Cross-submodule name collisions |
|---|---|---|
| `DIFFRACTION_DATA` | INQ, PUT, READ, SET | **0** |
| `MOLECULE` | BASE, CE, CP, FOCK, GRID, HAR, INQ, INTS, MAIN, MISC, PLOT, PROP, PUT, READ, RHO, SCF, SET, TD, XTAL | **6** |

**Resolution rule (maintainer):** colliding names must be **made unique to the
parent class** (rename in the `.foo` source). The 6 `MOLECULE` collisions:

| Name | Defined in | Signatures |
|---|---|---|
| `make_ED_grid` | GRID `(grid,pt)`, RHO `(grid,pts)` | differ |
| `make_Fock_mx` | FOCK `(F,P,core,r12,xc)`, SCF `(core,r12)` | differ |
| `make_r_overlap_inverse_sqrt` | BASE `(S_inv)`, INTS `(S_inv)` | **identical** — check for a true duplicate to consolidate |
| `make_r_overlap_sqrt` | BASE `(S_sqrt)`, INTS `(S_sqrt)` | **identical** — check for a true duplicate |
| `make_stockholder_atom_weight` | HAR `(grid,a,kappa,pt)`, RHO `(grid,a,pts)` + `(grid,a,pts,overlapping_atom)` | differ |
| `test_plot_info` | MAIN (no args), PLOT (no args) | **identical** — check for a true duplicate |

For the three identical-signature pairs, first check whether one is a genuine
duplicate (delete/consolidate) vs two distinct procedures that happen to share a
name (rename). Re-run `scripts/collision_scan.py` after renaming — it must report
**0 collisions** before the grammar is tightened.

> Note: the scan skips ALL-CAPS names (CPP macros like `ENSURE`, `DIE_IF`) — those
> are not procedures; they were false positives in the first pass.

## 5. Scope + existing infrastructure

Qualified call sites in `foofiles/` (to convert in Phase A):

- `.SUBMOD:proc` (generic, single colon): **~2634**
- `.SUBMOD::proc` (non-generic, double colon): **9**
- `.:proc` / `.::proc` (same submodule): **~688**
- `TYPE:proc` / `TYPE::proc` explicit module-qualified (e.g. `TEXTFILE:destroy`,
  `STR:proc`): ~5178 — mostly a *separate* concern; decide whether these are in
  scope (they name a module explicitly and already resolve). Bulk is in
  `molecule.main`, `molecule.scf`, `diffraction_data.read`, `molecule.grid`,
  `molecule.fock`, `molecule.prop`.

Translator infrastructure that already exists (in `foogrammar/FooToFortran.java`):
- **`buildSubmoduleProcRegistry`** (~line 180): a line-scan building
  `class → procname → {defining modules}` — the seed of the Phase-A registry.
  (This is the same scan whose `::` regex was updated in the `:::`→`::` work.)
- **`buildSelflessMethods`** (~line 226): selfless-target detection (must keep
  working — see the language doc's §11 selfless call-resolution note).
- **Function return types** are already tracked (used for expression typing) —
  reuse for resolving function calls.
- **`buildGlobalTable`** (~line 125): global vars incl. stdin/stdout/stderr types.

## 6. Validation checklist

- [ ] Phase A: regenerate-all before/after ⇒ **zero diff** in `.F90`/`.int`/`.use`
      (the intended source-notation change must not alter emitted Fortran).
- [ ] `scripts/collision_scan.py` reports **0** collisions after renames.
- [ ] Debug + release builds compile and link.
- [ ] `tests/` suite unchanged under the loose criterion (`make report` /
      `scripts/compare_test_outputs.py`).
- [ ] Phase B: every removed procedure provably unreachable; suite still green;
      compile-time delta measured and recorded.

## 7. Pointers

- Language doc: `docs/FOO_GRAMMAR_DOCUMENTATION.md` — §6 (submodules), §8 (generic
  interfaces / `.int`), §11 (dot-method + submodule-qualified calls, selfless
  note).
- Build/run of the translator: `CLAUDE.md` §8 and `scripts/build_translator.sh`.
- Test report: `scripts/compare_test_outputs.py` (writes `tests.log`), or
  `make report` in a build dir.
- Collision scan: `scripts/collision_scan.py`.
