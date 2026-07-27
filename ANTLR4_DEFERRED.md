# ANTLR4 translator — deferred minor issues

Tracked for later attention once the full debug build compiles. None of these
block compilation; they are correctness-of-match or robustness refinements.

## Cleanup: normalise procedure-name CASE across definition and call sites

**Goal (Dylan):** find every procedure whose **definition case differs from its call-site
case** (or where call sites disagree among themselves) and make them consistent. Foo/Fortran
are case-insensitive so these compile and run fine, but the inconsistency is annoying and
trips case-sensitive tooling.

**Why it matters (concrete):** discovered during phase B (dead-code elimination). `textfile.foo`
defines `reset_IO_status` (upper `IO`) but `vec{basis}.foo` calls it as `stdin.reset_io_status`
(lower). The dead-code analysis keyed its call-graph nodes case-sensitively, so the call didn't
match the definition and the procedure was wrongly pruned — a latent, silent trap. (Worked
around in the translator by lower-casing the method part of every graph node via `node()`;
this cleanup would remove the underlying inconsistency in the *sources*.)

**How to tackle (parse-tree driven, reuse phase-B infra):** the translator already walks every
`ProcDef` (definitions) and every `PostfixContext` (calls). Add a `--case-report` mode that
records, per lower-cased procedure name, the **set of distinct spellings** seen across its
definition header and all call sites; flag any name with >1 spelling, listing file:line of each
variant. Then normalise — the definition's spelling is the natural canonical form — and rewrite
the call sites (a targeted, parse-tree-driven edit like `--add-self-intent`, NOT a blind sed,
so commented-out and string-literal occurrences are left alone). Related: [[submodule-call-autoresolution-done]]
already hit a case bug in the submodule registry (commit 627db872); this is the same family.

## Deferred: eliminate explicit `TYPE:proc` calls (out of scope so far)

**What:** the submodule-call cleanup (`4cd995df`) auto-resolved `.SUBMOD:proc` etc., but
explicit **type-qualified** calls `TYPE:proc` / `TYPE::proc` (e.g. `GAUSSIAN_DATA:...`,
`STR:...` for namespace access + method calls) were left **qualified on purpose**.

**Why it's not just a mechanical `TYPE:proc(x)` → `x.proc` rewrite:** that transform is
**unsafe** and was tried and reverted. An **elemental** method invoked on a `VEC{T}` array
receiver resolves against the *receiver's* type (`VEC{T}`), not the element type `T`, so
`x.proc` can bind a different (array-level) overload than `T:proc` intended — this introduced
a `use` cycle. See memory `typeproc-elemental-array-hazard`. A correct elimination needs
**type-aware** resolution that respects elemental/array-receiver semantics (and the
GAUSSIAN_DATA namespace-access case), not a blind receiver swap. Low priority — the explicit
form compiles and runs fine; this is a consistency/readability cleanup, not a correctness bug.

## DONE: phase B — per-executable dead-code elimination

**Goal (Dylan):** eliminate code dead for a specific executable (e.g. `run_molecule`/`tonto`),
in a separate build dir, without affecting the other executables or the normal build.

**Delivered** in `FooToFortran.java` + `CMakeLists.txt`:
- `--call-graph-report` → Graphviz `call_graph.dot` / `module_use.dot` (submodules collapsed to
  parent) / `submodule_use.dot`; `--dead-code-report <root.foo>` → per-module live/dead TSV;
  `--purge-dead-code <root.foo>` → two-pass emit dropping unreachable procs. CMake exposes the
  `callgraphs` target and `-DPURGE_DEAD_CODE=<stem>` (separate build tree). See README §7b, CLAUDE.md §8.
- Reachability = BFS from the root program's entry calls over a call graph captured by
  piggybacking on the real call-resolution. `TYPES`/`SYSTEM` (wholesale-`use`) never pruned.

**Validated:** `-DPURGE_DEAD_CODE=run_molecule` release build compiles 0-error, ~32% of the
~7600 procedures dropped, binary 33→25 MB, and ctest is **121/124 — identical to the full build**
(same 3 known-bad). Three reachability-analysis bugs were caught only by the compile+test gate,
each a call form that bypassed the `use`-based capture: (1) same-module `::proc`/bare-selfless
calls (fixed: `recordSelfCall`); (2) case-sensitive node keys (`reset_IO_status` vs
`reset_io_status`; fixed: `node()` lower-cases the method part — motivates the case-cleanup goal
above). CPP-macro-hidden calls all target `SYSTEM` (always kept), so no macro-root class exists.

## DONE: simplify the DOT call-graph output

**Goal (Dylan):** reduce the complexity of the graphs from `--call-graph-report` (phase B).
`call_graph.dot` is a **procedure-level** graph — ~7600 nodes / ~24k edges — too dense to read
as a single image. `module_use.dot` (921 edges) was legible-ish but still a hairball.

**Delivered** as `scripts/simplify_callgraph.py` (a post-processor over `module_use.dot`, not a
translator change — the DOT already carries every `use` edge):
- **Aggregate** — fold module families into one coloured node: `NUMBERS` (INT/REAL/CPX),
  `ARRAYS` (VEC/MAT of primitives), `SHELLS`, `GAUSSIANS`, `MAPS`, `ISOSURFACES`; and re-point
  `VEC{T}`/`MAT{T}` over a derived type to that element's module (`VEC_ATOM`→`ATOM`), surfacing
  real deps like `MOLECULE→ATOM`. Dropped dead `BREAKDOWN_DATA`, `MULTI_T_ADP`.
- **Ambient** (`--simplify`) — hide the 7 universal utilities that take 62% of all edges
  (`NUMBERS ARRAYS STR BIN TEXTFILE BUFFER TABLE_COLUMN`). 139/921 → **50 nodes / 114 edges**.
- **`--module NAME`** — documentation ego-graph of one module's direct dependencies
  (`--reverse` dependents, `--both`); no `concentrate`, so every direct edge shows.

**Key findings, recorded in `docs/CALL_GRAPHS.md`:** the "aggregate vs ambient" distinction
(merge-and-keep vs hide); `concentrate=true` is *lossy* (drops a direct edge parallel to a
longer path — e.g. `ATOM→INTERPOLATOR`), so the doc mode avoids it; and Graphviz has **no** edge
hops/bridges and is already hierarchical, so fewer edges beats a different engine. README §7b
points to the tool + doc. (The procedure-level `call_graph.dot` remains dense — a module-level
*call* graph in `writeDotFiles` is still a possible future refinement, but not needed now.)

## Future task: introduce Fortran-2008 `submodule` constructs

**Goal (Dylan):** use real Fortran-2008 `submodule` where appropriate. **Concept clash to note
first:** a Foo "submodule" (e.g. `molecule.base.foo` → `module MOLECULE.BASE`) currently
translates to a **separate, standalone Fortran module** `MOLECULE_BASE_MODULE`, `use`d like any
other — NOT an F2008 `submodule`. F2008 `submodule (PARENT) NAME` would instead let the 19
`MOLECULE.*` pieces share one parent interface and break the `use`-graph coupling (a submodule
sees its ancestor's specification without a `use`, and changing a submodule body doesn't force
recompilation of the parent's users). Investigate whether mapping Foo submodules onto F2008
submodules simplifies the emitted module graph and compile-time dependencies. Touches
`emit()`/`buildUseFile()`/`buildInterfaceFile()` and the module-naming scheme.

## Future task: test the MPI parallel build

**Goal (Dylan):** verify the MPI build works and its tests pass. Build flags exist
(CLAUDE.md §4: `-DCMAKE_Fortran_COMPILER=mpifort … -DMPI=1`, optional `-DNO_ERROR_MANAGEMENT`);
`scripts/test.py` has a `--mpi` path (`mpirun -n 4`), wired via `WITH_MPI` in `tests/CMakeLists.txt`.
Status is **unverified** for the ANTLR4 translator output. Start by building MPI and running
`ctest` under it; expect the parallel macros (`PARALLEL_DO_*`, `PARALLEL_SUM`, `broadcast_` — all
`SYSTEM`/`tonto`-targeted, see `macros.in`) to be the surface area. Compare against a non-MPI run.

## Future task: verify the macOS build (Apple Silicon / Tahoe)

**Goal (Dylan):** confirm whether Tonto builds and passes tests on current macOS, on
**Apple Silicon (M2) with macOS Tahoe (26)** — to be done in a **separate session on a real
Mac** (the main dev box is Linux; macOS cannot be tested from it). A March-2026 README note
claimed *"many failures on the Apple M2 with Tahoe 26.3 — not recommended"*, but Dylan says
that is **very old** and likely stale, so it must be re-checked rather than trusted. Build via
Homebrew (`brew install gcc cmake openjdk python3 gnuplot`; BLAS/LAPACK come from
`Accelerate.framework`), then run `make report`. **Feed the result back into the docs:** update
the README macOS line (currently softened to "via Homebrew; Linux/WSL is the reference platform")
and the `Building on MacOS` wiki page to say what actually works — pass → "supported", or list the
specific failures if any remain.

## DONE: continuous integration (GitHub Actions, loose gate)

**Goal (user):** bring back automated CI so every push builds the ANTLR4 translator,
compiles `tonto`, and runs the test suite. (The old Travis badge was defunct.)

**Delivered** as `.github/workflows/ci.yml` — green as of `99dc3a1c` (2026-07-27):
- **Provider:** GitHub Actions (Travis's OSS offering is dead). Triggers on push/PR to
  `antlr4`, `master`, `release`.
- **Pipeline:** checkout `--recursive` → install gfortran-14 + JDK (for the translator's
  `javac`; the ANTLR jar auto-downloads via CMake) + BLAS/LAPACK/python3 → `cmake` +
  `cmake --build -j2` (bounded: one JVM per `.foo` is memory-heavy on a shared runner) →
  short suite via `scripts/compare_test_outputs.py`.
- **Gate:** the **loose** criterion (rel ≤ 0.2% OR last-digit ≤ 2). The agreement table is
  echoed to the run's Job Summary and uploaded as `tests.log`; a self-diagnosing `Diagnostics`
  step (`if: always`) reports toolchain/binary/one-raw-test on red runs. README carries the badge.
- **Two bugs the first green run flushed out** (both fixed): `scripts/test.py` resolved a
  *relative* `--test-directory`/`--basis-sets` after `chdir`ing into the temp run dir → doubled
  path → 100%-fail (fixed by absolutising up front, `036ecaec`); and the Kanghyun `keyword_echo`
  lines added un-reblessed stdout → 44/51 (dropped both echoes, `99dc3a1c`).
- **Not done (deliberate):** debug/release matrix and gfortran-version matrix — single release
  job for now; jar/parser caching not yet added (build is tolerable at ~20 min).

## Editor: improve vim highlighting of Foo and vim integration

**Goal (user):** improve the vim editing experience for `.foo` sources — better syntax
highlighting and tighter editor integration. The repo already ships some vim support
(`.vim/filetype.vim` maps `*.foo` and `macros` to a `foo` filetype; `scripts/fix_tags.pl` and
`scripts/cscope_setup` support ctags/cscope navigation — kept for exactly this reason).

**To investigate / do:**
- **Syntax file:** review/extend the `foo` syntax highlighting to cover the current language —
  reverse declarations (`var :: TYPE`), parameterized types (`VEC{T}`, `MAT{T}`…), pointer/
  allocatable suffixes (`*`, `@`), procedure headers with `:::` attributes (`PURE`,
  `ELEMENTAL`, `get_from(...)`, `selfless`), `KEY?` template placeholders, the constants
  (`TRUE`/`FALSE`/`ZERO`/`ONE`/`NULL`), and comments (`!`). Confirm whether a `syntax/foo.vim`
  exists and is up to date, or author one.
- **Indentation:** Foo uses 3-space indentation to mark scope (closed by `end`); an
  `indent/foo.vim` that follows this would help.
- **Navigation:** verify `scripts/cscope_setup` + `scripts/fix_tags.pl` still produce usable
  tags/cscope indexes for `foofiles/` and `runfiles/`, and document the workflow.
- **Integration niceties (optional):** a command/`makeprg` to translate the current `.foo`
  with `FooToFortran` and jump to errors; folding on scope; matchit for `... end` blocks.

## DONE: explicit `self` intent via self-modification analysis (plan B)

**Goal (user):** make `self`'s intent explicit in the `.foo` sources where it is
currently implicit. The first attempt used a blanket rule (subroutine → INOUT,
function → IN); it did **not** compile — read-only subroutines given INOUT reject a
const `self` from their (often inherited) callers, and some *functions actually
modify self* (memoisers, lazy readers), so `self :: IN` was rejected. Dylan had
assumed all functions are PURE, which several are not.

**Resolved with "Option 2" below** — a **self-modification analysis** in the
translator (`FooToFortran --add-self-intent`, parse-tree driven). Rule:
- subroutine → **INOUT** iff it modifies self, else **IN**;
- function → **IN** iff pure (does not modify self); a self-modifying function is
  left implicit and **flagged impure** (see `self_intent_analysis/impure_functions.tsv`).

"Modifies self" = direct write (`self%x = …`), a self-method call that transitively
modifies self (fixpoint, seeded with create/destroy/nullify), a call to a method whose
`self` is *declared* INOUT/OUT, or an input read into a self component
(`stdin.read(.label)`, `.SCF_DIIS.read_keywords`, `.atom(a).set_flag`).

**Applied + validated:** 135 `self :: IN|INOUT` decls (58 IN / 75 INOUT) across 47
foofiles, plus 2 genuinely-wrong hand-written `INOUT`→`IN` corrections on read-only
`MAT{REAL}` `_LAPACK` helpers. A clean **release** build compiles 0-error and the full
`ctest` suite is **121/124** — the same three deferred failures below, no regressions.

**Follow-on (deferred, Dylan's proposal):** mark the impure procs `IMPURE` in the
`.foo` and declare the rest `PURE`. Purity is compiler-enforced (a `pure` proc calling a
non-pure one is an error), so it self-validates. Impure = {modifies an arg or self} ∪
{does I/O} — so put/dump/show/read are impure regardless of self. The
`impure_functions.tsv` (modifies-self + OUT/INOUT-arg functions) is the seed list; add
I/O-call detection when tackling it.

## Deferred: small numerical differences (longstanding) — drill down

Several tests differ from their references only by small numerical amounts — 3rd–4th
significant figure, or a last-digit wobble on a near-zero value. They pass the loose gate
(rel ≤ 0.2% OR last-digit ≤ 2), but some sit close enough to the boundary that a different
runner/CPU (BLAS / eigensolver ordering, FP reassociation) flips the verdict — this is the
**CI flake** seen on GitHub Actions (same binary, pass on one runner, fail on the next).
**Dylan wants to drill down** on each and fix the root cause, not merely tolerate them.
Examples so far:

| Test | Difference | Likely cause |
|------|-----------|--------------|
| `urea_ccsd_pob-TZVP_Salvador_properties` | Salvador charges/dipoles ~0.5% (`0.1984`→`0.1974`, `-0.3959`→`-0.3956`), 3rd–4th sig-fig | grid integration / partition numerics; longstanding, pre-ANTLR |
| `h2o_rhf_cc-pVDZ_tdhf` | TDHF response, rel ~0.12% — just under the 0.2% gate (ulp already ~10) | time-dependent HF response; eigensolver / BLAS ordering across runners |
| `nh3_rhf_DZP_HAR` | a near-zero value: ~10% *relative* but ~1 ulp — passes only via the last-digit bound | relative metric amplified near zero (cf. the `ylid` case) |

**Stopgap in place (CI stability):** `scripts/compare_test_outputs.py` carries a documented
`KNOWN_MARGINAL` table that widens the loose bound for just these tests (`tdhf`: rel ≤ 0.5%;
`nh3_rhf_DZP_HAR`: last-digit ≤ 4) so the badge stops flickering, while the strict 0.2% / 2 gate
stays for every other test — and the report prints a footnote naming which tests were relaxed.
It is a **workaround, not a fix**: the goal is to *remove* entries from that table by
understanding each discrepancy. (Salvador is not in the table; its ~0.5% is accepted with the
reference pinned to the produced values.)

NOTE (Salvador): verify this is NOT the moments-staleness knock-on from setting `.atomic_moments_made`
(the flag now suppresses moment re-making that release always did). If a targeted
`.atomic_moments_made = FALSE` reset after SCF convergence restores the reference values, it
IS the knock-on and should be fixed rather than accepted. See memory `debug-ensure-vs-release`.

## DONE (release): the 3 remaining test-suite failures (milestone 3)

**Resolved on release — verified 2026-07-17: a `gfortran-14` release build is 124/124 (`ctest`
exit 0), up from 121/124.** All three former failures now pass, fixed at the source / in the
references by Dylan (commits `b3b50dd2` "Fixed no. of doubles test error", `d9dffb3f`,
`dee5cac9` "Corrected Salvador test", `50988e87` "All short & long tests passing on laptop"):

1. **`cyclazine_rhf_cc-pVDZ_tddft_state_selection`** — was a single-line `No. of doubles`
   diff (`24355` ref vs `22797`). **Not** an `-O0` boundary artifact after all: it was a
   real **evaluation-order bug in the source**. `foofiles/td_data.foo` computed
   `n = .no_of_doubles` *after* printing the doubles-window block; commit `b3b50dd2` moved the
   assignment *before* the block, so the printed count is now correct and stable. (Also fixed
   two typos in adjacent `stdout.text` lines.)
2. **`gly_ala_fragHAR_rhf_STO-3G`** — table column-width / alignment shift; reference updated
   (`50988e87`). Passes (73 s).
3. **`urea_ccsd_pob-TZVP_Salvador_properties`** — the longstanding Salvador grid/partition
   numeric difference; the reference was updated to the release-produced values (`dee5cac9`),
   so the release build now matches exactly. **NB:** this "resolves" it only for `-O3`; the
   ~0.5% `-O0` difference is unchanged and now shows up as a **debug-only** failure — it has
   moved into the debug section below, not disappeared. The `atomic_moments_made` knock-on
   question (see the Salvador section above) is still unverified.

Two other cases seen only under the *strict* (exact) sweep also loose-pass and are benign:
`h2o_rhf_cc-pVDZ_tdhf` (one TDHF state differs in the last digits) and
`cyclazine_rhf_cc-pVDZ_VMO_canonicalization` (~1e-4; original archives lost, regenerated).
A threshold-driven "loose pass" gate (candidate for CI, above) absorbs all of these.

What remains for milestone 3 is therefore **not** the release suite (green) but the **debug**
suite (119/124 — next section) and wiring the release gate into CI.

## Deferred: debug-build (`-O0`) test failures — floating-point boundary artifacts

> **FIXED & COMMITTED — `process_CSD_cif` (#113) fragment-offset `int()` flip (2026-07-26).**
> The worst debug failure — the `Fragment offset` `int()` boundary flip (`0.999.../1.000...` truncating
> to different integers under `-O0` vs `-O3`, cascading a whole lattice-vector shift, 189% rel) — is
> **fixed at the source**. Root cause: `int(mean_column_vector)` truncates toward zero with its knife-edge
> exactly on the integers, where a fragment centre can land. Fix: a **toward-zero nudge**,
> `int(pos*(ONE-TOL(8)))`, at all **6** offset sites (`cluster.foo` 132/163/529, `crystal.foo`
> 979/10329/10363). This shrinks the centre by `1e-8` (≫ the ~1e-14 `-O0/-O3` reassociation gap,
> ≪ the 1e-6 coordinate resolution) so both optimisation levels resolve the boundary to the **same**
> integer, while every non-boundary fragment keeps its exact `int()` value.
>
> Rejected alternative: `nint()` (round-to-nearest). It also makes `-O0`==`-O3` but changes the
> *recentering convention* for **every** fragment (e.g. mean 0.616: `int→0` but `nint→1`), churning
> ~624 lines across many cluster tests. The nudge is surgical.
>
> **Verified:** both trees rebuilt 0-error; on `process_CSD_cif` **release output == debug output**
> (0 substantive diff — the `-O0/-O3` disagreement is gone) and release is deterministic run-to-run.
> The `stdout` reference was re-blessed from the release build: it changed only by **one boundary
> fragment** (offset `0 1 0`→`0 0 0`, a pure lattice relabelling; `iodos.cxc` is unchanged — the offset
> cancels in absolute coords). No other test moves. Committed (source `cluster.foo`, `crystal.foo`;
> reference `process_CSD_cif/stdout`).
>
> This fix does **not** address the other 4 debug failures (47, 64, 87, 91) — the small-FP / esd-token /
> convergence-divergence cases in the table below, still open.

**Context (re-measured 2026-07-17).** A clean **debug** build (`gfortran-14`, `-O0 -g`, ENSURE
preconditions live) compiles 0-error and runs the full suite at **119/124** (up from 116/124
on 2026-07-15 — the same-day release is 124/124). Every remaining failure was checked against
the **release** binary (`-O3`), and **release reproduces the reference exactly** for all of
them. So **none is a translator bug or a crash** — the debug build ran every job to completion
with no ENSURE aborts. The *only* variable is optimisation level: `-O3` FP contraction /
reassociation produces sub-ulp numeric differences that, at boundary cases, **flip a discrete
decision** in the source. Debug's real job — surfacing crashes and precondition violations —
passed clean.

**Changes since 2026-07-15 (net +3):** two ADP-label cases (`L_alanine` #65, `YLID` #69) were
cleared by Dylan adding `show_IAM_output=FALSE` / `show_IAM_results=TRUE` to their `stdin` —
this suppresses the per-cycle ADP tables where the near-equal `Uxx`/`Uyy` label flips, while
keeping the final refined values compared (exactly the "suppress the line, keep the meaning"
approach below). `cyclazine` #5 was fixed at the source (see the DONE section above). Against
that, `urea…Salvador` #47 **entered** the debug failure list: its reference was updated to the
`-O3` values, so `-O0` now differs from the reference by the longstanding ~0.5% grid amount.
Net: 8 − 4 (65, 69, 5, 72) + 1 (47 gained) = 5 failures; #113 then fixed at the source
(nudge, above) = **4 failures** (47, 64, 87, 91).

**Proven mechanism** (was `#113`, now fixed — kept as the canonical illustration of the class).
`foofiles/cluster.foo:132` — `.fragment_offset = int(crystal.fragment_geometry.mean_column_vector)`.
`int()` truncates the fragment-centre mean toward zero; when that mean sits on a unit-cell boundary,
`-O3` yields e.g. `1.0000001 → 1` while `-O0` yields `0.9999999 → 0`. Both are crystallographically
valid (differ by a lattice vector) but print differently. The site now carries the `*(ONE-TOL(8))`
nudge (above), closing this instance; the remaining ADP-label and count cases below are the same
class (selecting/counting among near-equal or near-threshold values).

**Current debug failures (4; #113 fixed 2026-07-26, others measured 2026-07-17):**

| # | Test (category) | Substantive diff (ref → debug) | Class |
|---|-----------------|--------------------------------|-------|
| 64 | `ylid` (rgbi) | last bond-analysis columns drift ±0.1 (e.g. `74.02`→`73.90`); worst is `0.04`→`0.05` = 20% *relative* on a near-zero value | FP noise amplified by relative metric near zero |
| 87 | `urea_rhf_DZP_consistent-cluster-charge_HAF` (long) | 1-ulp last digit (`-349.2012`→`-349.2013`) + one column 1 char wider | last-digit rounding + auto-width threshold |
| 91 | `yq28_H_U_iso_IAM_refinement` (long) | identity matrix width `1.0000`→`1.000` (numbers identical); **also** two extra lines `Warning … crystal data already defined!` / `xray_data is already defined` not in the reference — release does not print them, so investigate before assuming FP (may be an ENSURE/precondition path live only in debug, or a junk-filter gap) | auto-width threshold + unexplained debug-only warnings |
| 47 | `urea_ccsd_pob-TZVP_Salvador_properties` (short/long) | Salvador charges `0.1984`→`0.1974`, `-0.3959`→`-0.3956`, dipoles ~3rd–4th sig-fig (~0.5%) | longstanding grid/partition numerics (see Salvador section); reference now pinned to `-O3` |

**Cleared since 2026-07-15** (kept for the record): #65 `L_alanine` and #69 `YLID` ADP-label
flips (fixed via `show_IAM_output=FALSE` in their `stdin`); #5 `cyclazine` No.-of-doubles
(source fix `b3b50dd2`); #72 `gly_ala_fragHAR` (now passes debug too); #113 `process_CSD_cif`
fragment-offset `int()` flip (source fix — `*(ONE-TOL(8))` nudge, 2026-07-26; see the block above).

**Goal (Dylan): make the *debug* tests pass**, probably by **suppressing the offending
output line(s)** in the comparison — but not so much that the test loses meaning. Notes on the
options, to think through:

- **Targeted output suppression (Dylan's lead).** Each failure is one or a few identifiable
  lines: the `Fragment offset` line, the ADP component *label* token, the `No. of doubles`
  count, the auto-width columns. Adding these to `prefixes_to_ignore` (or a per-test ignore
  list) in `scripts/test.py` makes debug green while keeping the numeric substance compared.
  Risk: suppressing a *label* or a *count* removes a genuinely meaningful field — prefer
  suppressing only the specific line, per-test, not the whole table.
- **`test.py` near-zero `abs_tol`.** Only helps the pure-numeric near-zero case (`ylid`
  `0.04` vs `0.05`). Does **not** fix the discrete label/offset/count flips (those are text).
- **Source hardening (higher value, more invasive).** Replace knife-edge `int()` /
  component-selection / auto-width thresholds with a small-epsilon-tolerant form so `-O0` and
  `-O3` agree at boundaries. This has real **portability** value — a boundary that flips
  between `-O0` and `-O3` could also flip between compilers/platforms even in *release* — but
  it edits hand-written scientific `.foo` and must be done per-site (start `cluster.foo:132`).

To reproduce: clean `gfortran-14` debug build (`-DCMAKE_BUILD_TYPE=debug`), `ctest`, then
`diff tests/<suite>/<test>/stdout tests/<suite>/<test>/stdout.bad` for each failure (the loose
harness writes `stdout.bad` on a fail). The five listed above are all that remain as of
2026-07-17; the raw diffs from the original 2026-07-15 run lived in that session's scratchpad
(`debug_analysis/`) and are not preserved across sessions.
