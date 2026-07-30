# CLAUDE.md

Durable, project-wide context for Claude Code, read at the start of every session.
Stable facts only (build, test, layout, conventions). Per-task specs belong in a
separate document.

## 1. What this project is

**Tonto** is a quantum chemistry / crystallography package. Its scientific code is written
in **Foo**, a custom object-oriented preprocessor language that is translated to modern
Fortran (95 / 2003+) and then compiled.

- Foo sources live in `foofiles/` (`*.foo`). Maintainer: Dylan Jayatilaka.
- Legacy translator: `foo.pl` (Perl) — the reference behaviour the ANTLR4 translator was built
  to reproduce. **Both `foo.pl` and its frozen reference output have since been removed**; the
  ANTLR4 translator now drives the build and the task is **complete** (validated by build +
  `ctest` on Linux and CI — see §2). The working-tree `release/` and `debug/` directories are
  ordinary **out-of-source CMake build trees** (untracked, regenerable); `debug/` is currently
  out of date. There is **no** reference-snapshot directory to preserve.
- Executables: `build/tonto` (main program), `build/hart` (standalone Hirshfeld atom
  refinement; `hart -help`).
- Run scripts: `runfiles/`. Test jobs: `tests/`.

**Translator output.** For each `module.foo` the translator emits three files:
- `module.F90` — the Fortran source.
- `module.int` — generic interfaces for the module.
- `module.use` — procedures pulled in from dependent modules.

The `.int` and `.use` files are `#include`d into the `.F90` by the C preprocessor **at
compile time**. So the translator output is **pre-CPP**: macros (`include/macros.in`) and
`#include`s are left intact for the Fortran build to expand.

## 2. Current task — the `antlr4` branch

Replace `foo.pl` with an ANTLR4-based Foo→Fortran translator that reproduces the legacy
output. Two deliverables:

1. A correct ANTLR4 grammar — `foogrammar/Foo.g4`.
2. A translator — `foogrammar/FooToFortran.java` — whose Fortran matches `foo.pl`'s.

Directory roles:

| Path | Role |
|------|------|
| `build/`, `release/`, `debug/` | Local out-of-source CMake build trees (untracked, regenerable). `debug/` is currently out of date. |
| `external/antlr4` | ANTLR4 itself (git submodule). |

*(Historical: during development the translator's output was written to `antlr4-release/` and
compared file-by-file against a frozen `foo.pl` reference snapshot. Both the snapshot and `foo.pl`
are gone — the `release/` name is now just a build tree; validation is build + `ctest`.)*

**Translation rules `foo.pl` applies** (the behaviour to match): reverse declarations
(`var :: TYPE` → `TYPE :: var`), module renaming (`str.foo` → `STR_MODULE`), procedure-header
transformation, type parameterization, and C-style macro expansion (`include/macros.in`).
`foo.pl` runs in two passes — pass 1 analyses signatures/interfaces/symbols, pass 2 generates
code.

**Status** (2026-07-27): **all three milestones done.** The ANTLR4 translator **works and drives
the build** — it parses every `foofiles/` file (submodules included) and emits equivalent,
compilable Fortran. A release `tonto` built from its output passes **124/124** `ctest` locally
under the loose criterion, and **GitHub Actions CI is green** (short suite 51/51; badge in
README; green as of `99dc3a1c`). Only the debug (`-O0`) build has 4 longstanding
FP-boundary/structural failures (not translator bugs — see `ANTLR4_DEFERRED.md`). Phase B
(per-executable dead-code elimination + call/use-graph export) is done (§8, commit `860922ea`);
the DOT graphs now have a `--simplify`/`--module` readability tool (`scripts/simplify_callgraph.py`,
`docs/CALL_GRAPHS.md`).

## 3. The Foo language (summary)

Full details in the companion docs (§7).

- **Reverse declarations:** `varname :: TYPE` (e.g. `i :: INT`, `matrix :: MAT{REAL}`).
- **Primitive types:** `INT`, `REAL` (double precision), `CPX`, `BIN` (logical), `STR`.
- **Parameterized array types** with `{...}`: `VEC{T}`, `MAT{T}`, `MAT3{T}` … `MAT7{T}`;
  nestable (`VEC{VEC{REAL}}`). Dimensions/params with `(...)`: `STR(len=256)`,
  `MAT{REAL}(3,4)`, `VEC{STR}(len=1,6)`.
- **Pointer / allocatable suffixes:** `INT*` (pointer), `VEC{REAL}@` (allocatable).
- **Procedures:** `name(args) result (res) ::: ATTRS`. Attributes after `:::` include `PURE`,
  `ELEMENTAL`, `get_from(MODULE, ...)`.
- **Variable attributes** (comma-separated, after the type): `IN`, `OUT`, `INOUT`, `PRIVATE`,
  `READONLY`, `POINTER`, `TARGET`, `SAVE`, `ALLOCATABLE`, `OPTIONAL`.
- **Modules:** `module NAME … contains … end`; generic `interface NAME … end` blocks.
- **Submodules:** a large class may be split across files. `molecule.base.foo` declares
  `module MOLECULE.BASE`, a submodule of `MOLECULE` (file-name head = lower-case type name).
  Submodule-qualified calls put the submodule before a colon: `.SET:proc` (generic) /
  `.SET::proc` (non-generic); `.:proc` / `.::proc` within the same submodule; `.MAIN:proc`
  for the main module. Explicit calls pass `self`, e.g. `STR:proc(self,…)` /
  `STR::proc(self,…)`. (See §9 — the grammar does not yet implement this.)
- **Control flow:** `if/else if/else … end`, `select case … end`, `do … end`.
- **Comments:** `!` to end of line. **Constants:** `TRUE`, `FALSE`, `ZERO`, `ONE`, `NULL`.
- Case-insensitive keywords; identifier case preserved. `;` separates statements on one line.
- **Indentation is 3 spaces** and marks a new scope block, closed by an `end` keyword.

## 4. Building

CMake, out-of-source. Toolchain (`make`, `perl`, `gfortran-14`, `blas`, `lapack`, `python3`,
`gnuplot`) is already installed.

```bash
mkdir build && cd build
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release
make -j
```

Other build types: `debug`, `release-static`, and MPI (`-DCMAKE_Fortran_COMPILER=mpifort …
-DMPI=1`, optionally `-DNO_ERROR_MANAGEMENT`).

## 5. Validation

The `antlr4` translator task is **complete**; validation is now **build + `ctest`**:

- Build a `release` tree and run `ctest` — but, like `make`, **ask before launching a long
  build/test run** (§8). Use the loose criterion in `scripts/test.py` (rel ≤ 0.2% OR
  last-digit ≤ 2) as the pass/fail gate, not exact match.
- Green on Linux and GitHub Actions CI (short suite 51/51); full release suite 124/124 locally.
  The debug (`-O0`) build has 4 longstanding FP-boundary/structural failures (see
  `ANTLR4_DEFERRED.md`) — not translator bugs.
- *(Historical, no longer applicable: the translator's `*.F90`/`*.int`/`*.use` output was once
  compared file-by-file — equivalent, not byte-exact — against a `foo.pl` reference snapshot.
  Both that snapshot and `foo.pl` are gone. The output is pre-C-preprocessor: macros /
  `#include`s are expanded by the Fortran compile, see §1.)*

## 6. Conventions & gotchas

- Edit `.foo` sources in `foofiles/`, never the generated Fortran.
- During a normal build, generated Fortran lands in the build tree (e.g. `build/`, `release/`);
  do not hand-edit it — edit the `.foo` sources instead.
- `external/*` are git submodules (sbf, lapack-release, antlr4); clone with `--recursive`.
- Note that the files can be translated independently *provided* the `types.foo` file
which defines all the derived types is processed first. The legacy translator uses
two passes through the module file but it is not clear whether ANTLR4 needs two passes
once the Parse tree is generated.

## 7. Reference docs in this repo

- `docs/FOO_GRAMMAR_DOCUMENTATION.md` — full language description and Foo→Fortran conversion rules.
- `README.md` — install/build/test/run instructions.
- Project wiki — building on macOS/Windows, how to run tonto (linked from `README.md`).

## 8. Working agreement

- Plan before coding; don't run `make` / `ctest` without asking.

### Debugging and instrumenting Foo code — do it in a DEBUG build

Learned the hard way (2026-07-30: five wasted release rebuilds). **Add probes in a `debug`
build, not `release`.** `DEBUG_FLAGS` defines `USE_PRECONDITIONS`, which in `include/macros.in`:

- **`#undef PURE`** — so a probe can go inside a `PURE` routine. In release, `PURE` is real and
  any `stdout.show`/`flush` there fails to compile (often with a *misleading* "no specific
  subroutine for the generic `flush_`" rather than a purity error).
- **activates `WARN` / `WARN_IF`** — these are gated on `USE_PRECONDITIONS`, *not*
  `USE_ERROR_MANAGEMENT`, so they compile to **nothing** in release. `DIE`/`DIE_IF` are gated on
  `USE_ERROR_MANAGEMENT` and *are* live in release. So: a check that must fire in production has
  to be a `DIE`, not a `WARN`.
- adds `-fcheck=bounds`, useful given how easy overloading makes an arity/shape slip.

Keep the debug test job **quick** (e.g. `tests/long/urea_rhf_STO-3G_HAR` is ~4 s) so the
edit-build-run loop stays usable.

**Two further traps, both from Foo's overloading — it makes the code pleasant to *use* and hard
to *track*:**

1. **Confirm the path executes before analysing it.** A name match is not the overload that
   runs. `put_CIF`, `make_CIF_esds`, `set_pADP_errors_to`, `put_ADP2_errors_to` and
   `LS_structure_fit` all exist in several versions, and reading the wrong one wastes a rebuild.
   Print a bare marker first; only then instrument.
2. **Generic imports are per-module and inferred from observed calls.** The translator emits each
   module's `use … only:` list from the calls it finds, so `stdout.show("x",<expr>)` with an
   argument type that module has not used before gives "no specific subroutine for the generic
   `show_`" — it cannot resolve e.g. `count(...)` to the INT overload. **Assign to a declared
   variable first**, then show that.

*Also note:* the `shell1quartet.F90` `-O2` pin (arm64 macOS miscompilation workaround, §2) is
currently applied in **every** build type, so in a debug build that one file is compiled `-O2`
while everything else is `-O0`. Harmless for correctness but it hampers debugging that file;
worth gating on the release configs if it gets in the way.

**Translator build/run (confirmed).** Helper script: `scripts/build_translator.sh`.

```bash
# Generate the ANTLR parser + compile the translator (outputs under build/translator/):
scripts/build_translator.sh

# Build and translate one module into antlr4-release/:
scripts/build_translator.sh foofiles/irrep.foo

# Equivalent manual invocation:
JAR=$PWD/external/antlr-4.13.2-complete.jar   # absolute (a later step cd's into foogrammar/); override with $ANTLR_JAR
( cd foogrammar && java -cp "$JAR" org.antlr.v4.Tool -visitor -o ../build/translator/gen Foo.g4 )
javac -cp "$JAR" -d build/translator/classes build/translator/gen/*.java foogrammar/FooToFortran.java
java -cp "$JAR:build/translator/classes" FooToFortran \
     --types foofiles/types.foo --foo foofiles/irrep.foo --out-dir antlr4-release
```

`FooToFortran` writes `<stem>.F90`, `<stem>.int`, `<stem>.use` (stem maps `vec{real}.foo`
→ `vec_real`) into `antlr4-release/`. `types.foo` must be passed so the derived-type table is
built first (§6). (This single-module path is a dev/debug aid; the normal build is via CMake, §4.)

**Analysis modes (phase B — call graph / dead-code elimination).** `FooToFortran` also has
read-only analysis and a purge mode, all built on a cross-module call graph it derives by
piggybacking on the real call-resolution (`recordUse`/`recordSelfCall`, captured per
procedure). Call-graph nodes are `MODULE:method` with the method part lower-cased (Foo
preserves identifier case but Fortran is case-insensitive — a case mismatch would otherwise
miss the edge; see the `node()` helper).

```bash
# DOT graphs (no root needed) + dead-code report (root needed); shares one graph build:
java -cp "$JAR:build/translator/classes" FooToFortran --types foofiles/types.foo \
     --dead-code-report runfiles/run_molecule.foo --call-graph-report --out-dir <dir>
# Purge: emit only procedures reachable from run_molecule into <dir>:
java -cp "$JAR:build/translator/classes" FooToFortran --types foofiles/types.foo \
     --purge-dead-code runfiles/run_molecule.foo --out-dir <dir>
```
Flags: `--call-graph-report` (writes `call_graph.dot`, `module_use.dot`, `submodule_use.dot`);
`--dead-code-report <root.foo>` (per-module live/dead TSV; needs a root); `--purge-dead-code
<root.foo>` (two-pass: build graph → re-emit with dead procs dropped at the `emitProc` choke
point). CMake exposes these as the `callgraphs` target and the `-DPURGE_DEAD_CODE=<stem>`
option (a **separate** build tree — purge is per-executable). Wholesale-`use` modules
(`TYPES`/`SYSTEM`) are never pruned. Validated: a `-DPURGE_DEAD_CODE=run_molecule` release
build compiles clean (~32% of procedures dropped, binary 33→25 MB) and passes the same
loose ctest suite as the full build.

## 9. Milestones & open items

**Milestones**

1. ✅ **DONE.** `foogrammar/Foo.g4` parses **every** file in `foofiles/` without error —
   including the submodule files (`molecule.*`, `diffraction_data.*`).
2. ✅ **DONE.** `foogrammar/FooToFortran.java` emits `.F90` / `.int` / `.use` that are
   **equivalent** (compilable, same behaviour) to the legacy `foo.pl` output (since removed).
3. ✅ **DONE — A translator-built binary passing the loose suite, automated in CI.** A `tonto`
   compiled from the ANTLR4-generated Fortran runs the short suite under `scripts/test.py`'s
   **loose** comparison (rel ≤ 0.2% OR last-digit ≤ 2, plus junk-line filtering) and passes
   **51/51** in **GitHub Actions** (green as of `99dc3a1c`, 2026-07-27; `.github/workflows/ci.yml`,
   README badge). The full release suite is **124/124** loose locally. Residual: the debug (`-O0`)
   build has 4 longstanding FP-boundary/structural failures (#47/#64/#87/#91) that are not
   translator bugs and are documented in `ANTLR4_DEFERRED.md`; CI runs the short release suite.

**Open items** (future directions; details in `ANTLR4_DEFERRED.md`)

- **Grammar still ACCEPTS the old submodule call forms** (`.SET:proc`, `.MAIN:proc`, `STR::proc`)
  even though they are now auto-resolved away in the sources; not tightened (harmless).
- **README/wiki reorganisation** (in progress, 2026-07-27) — split responsibilities: README =
  build + verify/test only; `docs/` = code-tracking dev references; wiki = user guides. Default
  build should be `release` (not `fast`); retire event-specific blocks to the wiki.
- Future tasks (own conversations): a module-level *call* graph in `writeDotFiles` (the
  `--simplify`/`--module` **use**-graph tooling is DONE — `scripts/simplify_callgraph.py`,
  `docs/CALL_GRAPHS.md`); introduce Fortran-2008 `submodule` constructs; test the MPI parallel
  build; boilerplate doc comments; and (long-term) a possible move off Fortran.

> Submodules ARE implemented (dotted headers + colon call forms parse & auto-resolve; commit
> `4cd995df`), and translator build/run commands are recorded in §8 — both former open items done.
