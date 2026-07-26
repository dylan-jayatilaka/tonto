# CLAUDE.md

Durable, project-wide context for Claude Code, read at the start of every session.
Stable facts only (build, test, layout, conventions). Per-task specs belong in a
separate document.

## 1. What this project is

**Tonto** is a quantum chemistry / crystallography package. Its scientific code is written
in **Foo**, a custom object-oriented preprocessor language that is translated to modern
Fortran (95 / 2003+) and then compiled.

- Foo sources live in `foofiles/` (`*.foo`). Maintainer: Dylan Jayatilaka.
- Legacy translator: `foo.pl` (Perl) — the reference behaviour to reproduce. The
  script itself has been removed from the repo now that the ANTLR4 translator
  drives the build; its frozen output survives in `release/`, which remains the
  reference snapshot to match.
- Executables: `build/tonto` (main program), `build/hart` (standalone Hirshfeld atom
  refinement; `hart -help`).
- Run scripts: `runfiles/`. Test jobs: `tests/`.

**Translator output.** For each `module.foo` the translator emits three files:
- `module.F90` — the Fortran source.
- `module.int` — generic interfaces for the module.
- `module.use` — procedures pulled in from dependent modules.

The `.int` and `.use` files are `#include`d into the `.F90` by the C preprocessor **at
compile time**. So the translator output — and the `release/` reference — is **pre-CPP**:
macros (`include/macros.in`) and `#include`s are left intact for the Fortran build to expand.

## 2. Current task — the `antlr4` branch

Replace `foo.pl` with an ANTLR4-based Foo→Fortran translator that reproduces the legacy
output. Two deliverables:

1. A correct ANTLR4 grammar — `foogrammar/Foo.g4`.
2. A translator — `foogrammar/FooToFortran.java` — whose Fortran matches `foo.pl`'s.

Directory roles:

| Path | Role |
|------|------|
| `release/` | Reference Fortran produced by `foo.pl` — the target to reproduce. |
| `antlr4-release/` | Output of the new ANTLR4 translator — compared against `release/`. |
| `external/antlr4` | ANTLR4 itself (git submodule). |

**Translation rules `foo.pl` applies** (the behaviour to match): reverse declarations
(`var :: TYPE` → `TYPE :: var`), module renaming (`str.foo` → `STR_MODULE`), procedure-header
transformation, type parameterization, and C-style macro expansion (`include/macros.in`).
`foo.pl` runs in two passes — pass 1 analyses signatures/interfaces/symbols, pass 2 generates
code.

**Status** (2026-07-15): the ANTLR4 translator **works and drives the build**. Milestones 1 & 2
are done — it parses every `foofiles/` file (submodules included) and emits equivalent,
compilable Fortran. A release `tonto` built from its output passes **121/124** `ctest` under the
loose criterion (the 3 known-bad are longstanding, not translator bugs — see
`ANTLR4_DEFERRED.md`). Milestone 3 (fully-green tests, automated in CI) is what remains — a
hodgepodge of minor issues, not core translator work. Phase B (per-executable dead-code
elimination + call/use-graph export) is also done (§8, commit `860922ea`).

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

## 5. Validation (for the `antlr4` task)

- Generate the `*.F90`, `*.int` and `*.use` files with the new translator
  (`foogrammar/FooToFortran.java`) into `antlr4-release/`, and compare them against the
  reference files in `release/` produced by `foo.pl`.
- The bar is **equivalent, compilable Fortran — not a byte-exact match.**
- The target is **every** generated file, not only the examples named in the docs
  (`str`, `bin`, `int`, `real`, `atom`, `basis`, `molecule.*`); those were produced by an
  earlier Claude attempt whose context was lost.
- The reference files are **pre-C-preprocessor** (see §1); macro / `#include` expansion
  happens during the Fortran compile, which is **not** part of this task.
- **Running `ctest` is now in scope** (it is milestone 3 — see §9) but, like `make`, ask
  before launching a long build/test run (§8). Use the loose criterion in `scripts/test.py`
  (rel ≤ 0.2% OR last-digit ≤ 2) as the pass/fail gate, not exact match.

## 6. Conventions & gotchas

- Edit `.foo` sources in `foofiles/`, never the generated Fortran.
- During a normal build, generated Fortran lands in `build/`; do not hand-edit it. (`release/`
  and `antlr4-release/` are the reference vs. new-translator snapshots used for this task —
  see §2.)
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

**Translator build/run (confirmed).** Helper script: `scripts/build_translator.sh`.

```bash
# Generate the ANTLR parser + compile the translator (outputs under build/translator/):
scripts/build_translator.sh

# Build and translate one module into antlr4-release/:
scripts/build_translator.sh foofiles/irrep.foo

# Equivalent manual invocation:
JAR=external/antlr-4.13.2-complete.jar   # bundled in the repo; override with $ANTLR_JAR
( cd foogrammar && java -cp "$JAR" org.antlr.v4.Tool -visitor -o ../build/translator/gen Foo.g4 )
javac -cp "$JAR" -d build/translator/classes build/translator/gen/*.java foogrammar/FooToFortran.java
java -cp "$JAR:build/translator/classes" FooToFortran \
     --types foofiles/types.foo --foo foofiles/irrep.foo --out-dir antlr4-release
```

`FooToFortran` writes `<stem>.F90`, `<stem>.int`, `<stem>.use` (stem maps `vec{real}.foo`
→ `vec_real`). Compare against `release/` (whitespace-insensitive; the bar is equivalent,
not byte-exact). `types.foo` must be passed so the derived-type table is built first (§6).

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
121/124 ctest as the full build.

## 9. Milestones & open items

**Milestones**

1. ✅ **DONE.** `foogrammar/Foo.g4` parses **every** file in `foofiles/` without error —
   including the submodule files (`molecule.*`, `diffraction_data.*`).
2. ✅ **DONE.** `foogrammar/FooToFortran.java` emits `.F90` / `.int` / `.use` that are
   **equivalent** (compilable, same behaviour) to the reference in `release/`.
3. **IN PROGRESS — A fully-green test suite on a translator-built binary, automated in CI.** A `tonto`
   compiled from the ANTLR4-generated Fortran runs `tests/` (`ctest`) and reproduces each
   reference `stdout` under `scripts/test.py`'s **loose** comparison (rel ≤ 0.2% OR
   last-digit ≤ 2, plus junk-line filtering). "Passing binaries" means **passing in CI under
   that loose script** — not exact match. Current state: **121/124** on release. What remains
   is a hodgepodge of minor issues, all enumerated in `ANTLR4_DEFERRED.md`: the 3 longstanding
   known-bad tests, the debug-build (`-O0`) FP-boundary artifacts (suppress-or-tolerate), and
   the harness junk-filter gaps. Then wire it to CI (GitHub Actions — Travis's OSS offering is
   defunct; see the CI section in `ANTLR4_DEFERRED.md`) so every push runs the loose gate.

**Open items** (all milestone-3 polish + future directions; details in `ANTLR4_DEFERRED.md`)

- **Milestone 3 to fully green + CI** — clear the deferred hodgepodge (3 known-bad tests,
  debug `-O0` FP-boundary artifacts, harness junk-filter gaps), then a GitHub Actions loose gate.
- **Grammar still ACCEPTS the old submodule call forms** (`.SET:proc`, `.MAIN:proc`, `STR::proc`)
  even though they are now auto-resolved away in the sources; not tightened (harmless).
- Future tasks (own conversations): simplify the DOT call-graph output; introduce Fortran-2008
  `submodule` constructs; test the MPI parallel build; boilerplate doc comments; and (long-term)
  a possible move off Fortran. See `ANTLR4_DEFERRED.md` for the first three.

> Submodules ARE implemented (dotted headers + colon call forms parse & auto-resolve; commit
> `4cd995df`), and translator build/run commands are recorded in §8 — both former open items done.
