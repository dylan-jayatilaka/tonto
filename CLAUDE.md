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
  refinement; `hart --help` — see `docs/RUNNING_HART.md`).
- **All programs take GNU long options only** (`--input`, `--basis`, `--help`, …).
  Single-dash spellings were removed; `COMMAND_LINE.process_options` rejects one
  with a message naming the `--name` to use instead.
- Run scripts: `runfiles/`. Test jobs: `tests/`.

**Translator output.** For each `module.foo` the translator emits three files:
- `module.F90` — the Fortran source.
- `module.int` — generic interfaces for the module.
- `module.use` — procedures pulled in from dependent modules.

The `.int` and `.use` files are `#include`d into the `.F90` by the C preprocessor **at
compile time**. So the translator output is **pre-CPP**: macros (`include/macros.in`) and
`#include`s are left intact for the Fortran build to expand.

## 1a. Branching model (adopted 2026-08-11)

Conventional, and now actually followed:

| Branch | Role |
|---|---|
| **`master`** | The stable branch. What the CI badges track, what tags are cut from, what a user clones. Documentation fixes may land here directly; code should arrive by merge. |
| **`develop`** | The integration branch. Work lands here first and is merged to `master` when green. |
| feature branches | Short-lived, merged and deleted. |
| **tags** `v*` | Releases. A tag builds `tonto-linux-x86_64.tar.gz` and `tonto-windows-x86_64.zip` and publishes them (`.github/workflows/release.yml`). |

The old `antlr4` branch was deleted on 2026-08-11, its work being fully merged; `release`
was renamed `develop`, because a long-lived branch called "release" that is *less* stable
than master inverts what the name means everywhere else. **Dylan edits `master` directly
(README and docs), so fetch and merge `origin/master` before pushing.**

## 2. How the translator came to be — background, not current work

Replace `foo.pl` with an ANTLR4-based Foo→Fortran translator that reproduces the legacy
output. **This task is complete** (see *Status* below); the section is kept because the
milestone history explains why the code looks as it does. Two deliverables:

1. A correct ANTLR4 grammar — `foogrammar/Foo.g4`.
2. A translator — `foogrammar/FooToFortran.java` — whose Fortran matches `foo.pl`'s.

Directory roles:

| Path | Role |
|------|------|
| `build/`, `release/`, `debug/` | Local out-of-source CMake build trees (untracked, regenerable). `debug/` is currently out of date. |
| `external/antlr-4.13.2-complete.jar` | ANTLR4 itself — a release jar, **not** a submodule. |

*(Historical: during development the translator's output was written to `antlr4-release/` and
compared file-by-file against a frozen `foo.pl` reference snapshot. Both the snapshot and `foo.pl`
are gone — the `release/` name is now just a build tree; validation is build + `ctest`.)*

**Translation rules `foo.pl` applies** (the behaviour to match): reverse declarations
(`var :: TYPE` → `TYPE :: var`), module renaming (`str.foo` → `STR_MODULE`), procedure-header
transformation, type parameterization, and C-style macro expansion (`include/macros.in`).
`foo.pl` runs in two passes — pass 1 analyses signatures/interfaces/symbols, pass 2 generates
code.

**Status** (2026-08-05). Read this before the milestone list; the detail below is easy to get
lost in.

**The original task is complete.** The ANTLR4 translator replaced `foo.pl` and drives the build:
it parses every `foofiles/` file, submodules included, and emits equivalent, compilable Fortran.
A release `tonto` built from its output passes the full local suite under the loose criterion and
GitHub Actions CI is green. `foo.pl` and its reference snapshot are gone.

**What followed was not translation but repair.** Having a translator that could be trusted made
it possible to go after things that had been wrong for years, and the pattern was consistent:
**almost every defect found was silent.** It produced a wrong number, or no number, with no
diagnostic — and none was found by reading the code. All of them fell to measurement: tracing
broadcasts to per-rank files, counting them between markers, bisecting compiler flags.

The substantive gains, in rough order of value:

- **`hart` works.** The standalone Hirshfeld-atom-refinement program died on every real run and
  exited 0 while doing it. It now runs, has a test suite in CI, an option set reconciled with its
  documentation, and handles crystals with several molecules in the asymmetric unit (fragHAR) —
  in serial *and* under MPI, reproducing the serial reference digit for digit. See `docs/RUNNING_HART.md`.
- **MPI was characterised and largely repaired.** The first MPI build ever configured for this
  project. Eight reductions were silently returning `1/n_ranks` of the answer. A per-rank I/O
  flag's setter assigned the wrong member, so the whole mechanism was dead code that looked live.
  Collectives were gated on rank-local state, so different ranks entered different collectives.
  See `docs/TONTO_AND_MPI.md`, which carries a defect register with a **"Loud?"** column — the *silent* rows
  are the dangerous ones.
- **Whole classes closed, not just instances.** `data` statements were parsed and silently
  discarded; now they are emitted, and any construct that parses but emits nothing is a **build
  failure**. Reductions get a `parallel do … reduce(x)` clause, so the one place a reduction can
  be written incorrectly no longer exists. Two lints run in CI, including one that cannot be
  blessed away and one that guards a fix against being "tidied" back into the bug.
- **A test that never tested.** An X-ray-constrained-wavefunction job silently ate its own `scf`
  keyword and finished in 40 ms; its blessed reference contained no SCF at all. It now runs.

**What is open**, and why it is worth knowing before starting: the parallel-do lock (milestone 6
part 3), a gcc `-O2` tail-call interaction that is worked around but not root-caused (milestone 7),
hoisting `CRYSTAL` out of `MOLECULE` (October), and — long term — the case for re-engineering in a
language with first-class parallelism, argued from the evidence above rather than from taste.

**The working lesson, if you read nothing else:** in this codebase, *inspection does not work and
measurement does*. `docs/TONTO_DEVELOPER.md` §1a records the recipes — trace to per-rank files, never a
shared stream; count events between markers; and confirm a code path executes before analysing it.

## 3. The Foo language (summary)

Full details in the companion docs (§7).

- **Reverse declarations:** `varname :: TYPE` (e.g. `i :: INT`, `matrix :: MAT{REAL}`).
- **Primitive types:** `INT`, `REAL` (double precision), `CPX`, `BIN` (logical), `STR`.
- **Parameterized array types** with `{...}`: `VEC{T}`, `MAT{T}`, `MAT3{T}` … `MAT7{T}`;
  nestable (`VEC{VEC{REAL}}`). Dimensions/params with `(...)`: `STR(len=256)`,
  `MAT{REAL}(3,4)`, `VEC{STR}(len=1,6)`.
- **Pointer / allocatable suffixes:** `INT*` (pointer), `VEC{REAL}@` (allocatable).
- **Procedures:** `name(args) result (res) :: ATTRS`. Attributes after `::` include `PURE`,
  `ELEMENTAL`, `leaky`, `private`, `get_from(MODULE, ...)`. It was `:::` until **`3ca1e53d`**
  (2026-07-09, *"foo: replace ::: procedure-attribute separator with :: everywhere"*), which
  moved to `::` for consistency with Fortran's attribute separator — 184 files, the grammar and
  the translator included. This line was a straggler from that migration, corrected 2026-08-02
  along with three others: **both** ctags procedure regexes and a dead `:::` operator rule in
  the vim syntax files. The ctags effect was measured, not assumed — the old rule still matched
  headers carrying **no** attributes, via its `( *$)` branch, but missed every header with
  `:: leaky`, `:: PURE`, `:: private`, `get_from(...)` and so on: **1285 procedure tags across
  `foofiles/` instead of 12757, i.e. 90% missing.**
  **The tag `foo-old-syntax` (`ae306e1d`, the migration's parent) marks the last commit in the
  old dialect** — and the only point where old-syntax Foo still builds and tests with the current
  toolchain (`foo.pl` already gone, CMake already invoking `FooToFortran`, CI already running
  `ctest -L short`). It is the bridge for porting archived work: merge an `archive/*` branch
  there, where only the semantic drift conflicts, then replay the `:::`→`::` change. See
  `docs/REPOSITORY_BRANCHES.md` and §7 of `docs/FOO_GRAMMAR_DOCUMENTATION.md`.
- **`PURE` vs `pure` — the case matters.** Upper-case `PURE`/`ELEMENTAL` are **macros**
  (`include/macros.in`), `#undef`'d to nothing under `USE_PRECONDITIONS` and under `MPI`.
  Lower-case `pure` is passed through as the **literal Fortran keyword** and stays pure in every
  build. So a routine containing `ENSURE`, `DIE`, `WARN` or any other call that writes `tonto`
  must be declared `PURE`, never `pure` — otherwise it compiles in release (where `ENSURE`
  vanishes) and **fails only in a debug or MPI build**, with gfortran's misleading *"There is no
  specific subroutine for the generic `ensure_`"* rather than a purity error. Cost the debug CI
  a red badge on 2026-08-02; see the note at `PARALLEL:reduction_is_allowed`.
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
-DMPI=1`). The MPI must be built with the **same** Fortran compiler — Tonto does `USE mpi` and
`.mod` files are compiler-version specific; configure now checks and stops with a clear message.
`-DMPI=1` is a hard requirement: if MPI is not found, configure fails rather than silently
producing a serial binary. See `docs/TONTO_AND_MPI.md`.

*(`-DNO_ERROR_MANAGEMENT` was documented here but is a **no-op** — the symbol appears nowhere in
`CMakeLists.txt`, `cmake/*.cmake` or `include/macros.in`. Every optimised build type
unconditionally defines the positive `USE_ERROR_MANAGEMENT`, so there is no documented way to
turn error management off. Removed rather than implemented.)*

**WSL is a supported build host.** `cmake/WSL.cmake` (included at the top of
`CMakeLists.txt`, a no-op everywhere else) strips `/mnt/*` off `PATH` before any tool
search — otherwise `find_package(Java)` resolves to a Windows `java.exe`, which cannot read
Linux paths and needs `;` classpath separators while the translator is invoked with `:` —
and hard-errors on a `/mnt/c` build tree or CRLF sources. `-DTONTO_WSL_STRICT=OFF` downgrades
those to warnings. `scripts/wsl_doctor.sh` is the user-facing preflight;
`scripts/wsl_selftest.sh` asserts every guard on an ordinary Linux box (no Windows needed) and
runs on every push via `.github/workflows/ci-wsl.yml`. Details in `docs/BUILDING_ON_WINDOWS.md`.

## 5. Validation

The translator task is **complete**; validation is now **build + `ctest`**:

- Build a `release` tree and run `ctest` — but, like `make`, **ask before launching a long
  build/test run** (§8). Use the loose criterion in `scripts/test.py` (rel ≤ 0.2% OR
  last-digit ≤ 2) as the pass/fail gate, not exact match.
- Green on Linux and GitHub Actions CI (short suite 51/51); full release suite 124/124 locally.
  The debug (`-O0`) build has 4 longstanding FP-boundary/structural failures (see
  `DEFERRED.md`) — not translator bugs.
- *(Historical, no longer applicable: the translator's `*.F90`/`*.int`/`*.use` output was once
  compared file-by-file — equivalent, not byte-exact — against a `foo.pl` reference snapshot.
  Both that snapshot and `foo.pl` are gone. The output is pre-C-preprocessor: macros /
  `#include`s are expanded by the Fortran compile, see §1.)*

## 6. Conventions & gotchas

- Edit `.foo` sources in `foofiles/`, never the generated Fortran.
- During a normal build, generated Fortran lands in the build tree (e.g. `build/`, `release/`);
  do not hand-edit it — edit the `.foo` sources instead.
- `external/lapack-release` is the **only** git submodule; clone with `--recursive`. ANTLR4 is
  **not** a submodule — it is a release jar, `external/antlr-4.13.2-complete.jar`. The `sbf`
  submodule (Peter Spackman's) was **removed 2026-08-11**: nothing in `CMakeLists.txt` or
  `cmake/*.cmake` ever referenced it, so it was neither compiled nor linked; its only live
  consumer, `datafile.foo`, was commented out of the CMake source list; and no test manifest
  names a `.sbf` file. The dead source went with it in a second commit — `datafile.foo`, the
  commented serialize/deserialize blocks, and `scripts/test.py`'s `diff_sbf`/`is_sbf`/
  `--sbftool` (whose default path pointed into the submodule). `foofiles/` now contains no
  `sbf` reference at all. See `docs/REPOSITORY_BRANCHES.md`.
- Note that the files can be translated independently *provided* the `types.foo` file
which defines all the derived types is processed first. The legacy translator uses
two passes through the module file but it is not clear whether ANTLR4 needs two passes
once the Parse tree is generated.

## 6a. The two script directories

`rgbi-scripts/` is **installed** — `make-rgbi-pic` and `make-rgbi-dials` into `bin`, the
`.tex`/`.sty` templates into `share/tonto/rgbi-scripts`. `scripts/` is **not**: test
harness, invariant checks, lints, doctors, and `scripts/docker/` (moved there 2026-08-11;
it proves `docs/INSTALLING_RGBI.md`'s package list from a bare `ubuntu:24.04`). That is the
boundary — installed versus not, rather than subject matter.

The drivers in `rgbi-scripts/` deliberately have **no `.sh` extension**: they are commands
on `PATH`, named as commands are. `scripts/*.sh` keep theirs because they are invoked by
path. The one inconsistency is `rgbi_doctor.sh`, which is installed into `bin` with its
extension; see `DEFERRED.md`. Each directory has a `README.md` saying this.

## 7. Reference docs in this repo

**Everything is in this repository.** The GitHub wiki was retired on 2026-08-05 and its nine
pages migrated into `docs/` — it was superfluous, hard to maintain, and worst of all *not
versioned with the code it described*, so it could rot silently. Do not add documentation there.

- `README.md` — the leader page: what Tonto is, a quickstart, the documentation index, and what
  each CI badge means. Deliberately short; detail belongs in `docs/`.
- Building — one **self-contained** page per platform, each covering prerequisites, build,
  tests, other build types and MPI for that platform: `docs/BUILDING_ON_LINUX.md`,
  `docs/BUILDING_ON_MACOS.md`, `docs/BUILDING_ON_WINDOWS.md`. There is deliberately no
  shared build page — `BUILDING_TONTO.md` was split out and then deleted (2026-08-10),
  because a chooser page plus per-platform pages still made a reader hop.
- `docs/DOCUMENTATION.md` — the documentation index, linked from the README. The README
  itself is deliberately short: badges, what Tonto is, three build links, the workshop,
  this index, contact. Anything explanatory belongs here or in `docs/`.
- `docs/RUNNING_TONTO.md` — running Tonto: input/output conventions (`stdin`/`stdout`/`IO`), practical set-up.
- `docs/TONTO_LIBRARY_STRUCTURE.md` — source and executable layout, and the module structure picture.
- `docs/RUNNING_HART.md` — the `hart` program: what it hard-codes, its full `--option` reference, how
  it is tested (`tests/hart/`, the `program:`/`args:` IO keys, the invariant check), and its
  remaining milestones.
- `docs/RUNNING_RGBI.md` — the `rgbi` program, the two-halved picture pipeline, the LaTeX traps
  (two `chemfig`s, one of which fails silently), and how it is tested.
- `docs/INSTALLING_RGBI.md` — participant-facing install guide. Linux is tested by
  `scripts/docker/rgbi.Dockerfile` in CI; macOS is untested by hand and probed weekly by
  `ci-rgbi-macos.yml`.
- `docs/TONTO_DEVELOPER.md` — developer reference; §1a is **writing parallel (MPI) code in Foo**, eight
  pitfalls and the trace recipes that found them.
- `docs/FOO_GRAMMAR_DOCUMENTATION.md` — full language description and Foo→Fortran conversion rules.
- `docs/TONTO_AND_MPI.md` — the parallel build, its numeric characterisation, and the defect register.
- `docs/BUILDING_ON_WINDOWS.md` — the four WSL-specific traps, the CMake guards, and how they are tested.
- `docs/TONTO_CONTINUOUS_INTEGRATION.md` — the CI workflows, how to trigger one manually, and how to read a run.
- `docs/MAKING_CALL_GRAPHS.md` — call/use graphs and dead-code elimination.
- `docs/EDITING_TONTO_WITH_VIM.md` — vim set-up: tags, folding, completion.
- `DEFERRED.md` — project-wide deferred issues (was `ANTLR4_DEFERRED.md`).

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

0. **Tracing an overload? Read the `.int` file first.** For each generic, the
   generated `<module>.int` in the build tree lists the candidate specific
   procedures under their *distinct* translator-assigned names:

   ```fortran
   interface put_ADP2_errors_to_
      module procedure put_ADP2_errors_to_0
      module procedure put_ADP2_errors_to_1
   end interface
   ```

   This is the fastest way to learn how many overloads a name has and what they are
   called in the generated Fortran. It does **not** say which one a given call site
   resolves to, nor what `_0`/`_1` mean — for that, either open the definitions, or
   put a `DIE` in the suspect routine and build with `-fbacktrace`, which names the
   specific procedure *and* its callers in one run. Six consecutive mis-traces of
   `put_ADP2_errors_to` (2026-07-30) were spent inferring by hand what these two
   steps answer directly. See §3 of `docs/TONTO_DEVELOPER.md`.

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

**`hart` build/run (confirmed).** `run_har` is built by the ordinary `make` (it is *not*
`EXCLUDE_FROM_ALL`), so `build/hart` appears alongside `build/tonto`. A quick end-to-end job,
~5 s — the same urea structure the `tests/hart/` suite uses:

```bash
mkdir -p /tmp/hart && cd /tmp/hart
cp <repo>/tests/hart/urea_hart_STO-3G/urea_init.cif .
TONTO_BASIS_SET_DIRECTORY=<repo>/basis_sets \
  <repo>/build/hart --job urea --basis STO-3G --grid-accuracy low urea_init.cif
# -> urea.out (log) and urea.archive.cif (refined coords + ADPs with esds)
ctest -L hart      # the suite + the options invariant check
```

Full option reference and testing notes: `docs/RUNNING_HART.md`.

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
   translator bugs and are documented in `DEFERRED.md`; CI runs the short release suite.

**Milestones 4 and 5 — the remaining work on this project** (agreed 2026-07-31). These two are
independent and can run in parallel; milestone 5 is the more important, and should be **planned
before any code is written**, most likely in its own conversation (`/clear`).

4. ✅ **DONE (2026-08-01) — MPI parallel build + numeric characterisation.** First MPI build ever
   configured for this project. Outcome: **MPI at 1 rank reproduces serial exactly**; only
   `h2o_rhf_cc-pVDZ_tdhf` shows rank-count drift (non-monotonic, already `KNOWN_MARGINAL`); and
   `-ffast-math` moves the numbers more than MPI does. **One blocker**: `DWGN_lamaGOET_NBO_file_47`
   crashed at ≥2 ranks on a negative-unit I/O error, **now fixed** (raw unguarded writes in
   `put_NBO_file_47`), so the short suite is 50/51 under MPI, the same as serial. MPI is still
   unaudited for `plot_grid`/`archive` raw I/O and for HAR's `parallel_write`. Eight MPI wrong-answer bugs were found and fixed on the way (see milestone 6).
   Full report: `docs/TONTO_AND_MPI.md`. Build with
   `-DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_C_COMPILER=mpicc -DMPI=1` and compare against the
   serial references with the usual loose gate. (`-DCMAKE_CXX_COMPILER=mpicxx` was in this
   recipe but is **ignored** — `project()` enables `Fortran C` only; `-DNO_ERROR_MANAGEMENT` was
   a **no-op**. Both removed.) **The MPI must be built with the same Fortran compiler**, since
   Tonto does `USE mpi`. Details, and the list of MPI defects found, in `docs/TONTO_AND_MPI.md`. **Expect numeric drift**: reduction order varies with rank count, and
   some of it is genuine UB — per Dylan, "numerics might go off — no worries, we'll check". The
   deliverable is a *characterisation* (which tests drift, by how much, and whether the drift is
   rank-count dependent), not necessarily a green suite. Untested since before the ANTLR4 work.

5. ✅ **DONE (2026-08-03) — `hart`: verify, test, document, and make it work with `fragHAR`.**
   See **`docs/RUNNING_HART.md`**, which is now the authoritative document for the program.
   - ✅ *confirm the program actually works, and fix what does not.* It did not: every real run
     died at once (`std_err` was created but never opened, so the `close_and_delete` that
     follows hit "not an existing file"), **and exited 0 while doing so** — `SYSTEM.die` ended
     in a bare `stop`. Both fixed; `stop 1` now applies to every `DIE`/`DIE_IF` in Tonto.
   - ✅ *devise a testing method and add test jobs.* `tests/hart/`, label `hart`, in CI. The
     `IO` manifest gained optional `program:` / `args:` keys, so an argv-driven program can be
     tested by the same harness as a `stdin` job file. Plus `scripts/check_hart_options.sh`, an
     invariant check comparing `--help` against the option `case` labels — it cannot be blessed.
   - ✅ *correct its options, calls and documentation so they match reality.* `--disk-sfs` was
     documented but its case label was commented out; `--dtol` was parsed and validated but
     never used; `.cif2` was rejected though the message said it was required for restart;
     `extreme` was accepted but undocumented. All reconciled, and the whole option set moved to
     GNU `--long` form (which is what took `tonto`'s `-i`/`-o`/`-b`/`-h`/`-v` with it).
   - ✅ *make it work **seamlessly with `fragHAR`***, i.e. crystals with more than one molecule
     in the asymmetric unit — **milestone H1 in `docs/RUNNING_HART.md`**. **Serial is DONE
     (2026-08-02)**: `hart` counts the atom groups and calls `fragHAR_refinement` when there is
     more than one, with new `--mmcif`, `--group-charges '{ 1 -1 }'`,
     `--group-multiplicities`, `--wavelength` and `--residual-cube` options, and it reproduces
     `tests/long/gly_ala_fragHAR_rhf_STO-3G` **to every digit that reference prints** (R(F)
     0.0324, GoF 3.3535, N_r 2514, N_p 181). Gated by `tests/hart/gly_ala_hart_STO-3G` (60 s).
     It was a hookup, not a repair: fragHAR itself was broken 2020-01-23 by `f0d7cfd3` and
     fixed 2026-06-01 by `d840e322`. **Parallel is DONE too (2026-08-03)**: `mpirun -n 2 hart`
     runs fragHAR to exit 0 and reproduces the serial reference digit for digit (R(F) 0.032423,
     GoF 3.353475; 1586 lines vs 1586, differing only in banner/timing), with the ranks in exact
     lockstep for 2,421,451 broadcasts. Three defects had to go first, the last two of which are
     the generalisable ones: (i) `SYSTEM:set_per_rank_IO_allowed` assigned `.keyword_echo`, so
     the flag could **never be set** and the whole per-rank-I/O mechanism was dead code that
     looked live — longstanding, not from the rename; (ii) the mode was scoped *inside* the
     fragment loop body instead of around it, so bookkeeping broadcasts resumed while ranks were
     on different fragments; (iii) **after a per-rank region the ranks' object state diverges by
     design**, and `put_atom_group_mols` branched on it (`if (.becke_grid.allocated) …` — master
     42 broadcasts, rank 1 zero), which desynced them; it is now non-collective. Because
     TEXTFILE bookkeeping is collective, *printing more on one rank is itself a collective
     mismatch*. Recorded as pitfall 8 in `docs/TONTO_DEVELOPER.md` §1a, with the per-rank-file trace
     recipe that found it after three wrong readings of the code. Still open, both minor:
     `--group-charges-file` for proteins, and the `use_disk_SFs`→`use_disk_FFs` rename. Note
     `fragment_SCF_para`'s scheduler changes shape above 2 ranks, so any parallel fragHAR test
     must pin a rank count. All in `docs/RUNNING_HART.md` §6.
     *(Unrelated to fragHAR but fixed the same day: the non-fragHAR **disk** form-factor path,
     `hart --disk-sfs`, which had never worked — six defects — now does, and is gated by
     `tests/hart/urea_hart_STO-3G_disk_ffs`, the first test ever to execute `make_LS_mx`.)*

6. 🔶 **MOSTLY DONE — make MPI reductions safe by construction** (agreed 2026-08-01). Part 1
   (the `reduce` clause) done 2026-08-04, part 4 (the lint) 2026-08-03, part 2 implemented and
   then **withdrawn** because its premise was wrong. **Only part 3 remains** — the parallel-do
   lock's three defects — and it carries a live caveat: the naive fix silently disables work
   distribution, so it is only worth doing if lock-gated behaviour is actually wanted. Milestone 4 uncovered a class of silent wrong-answer bugs with a single root
   cause: the translator emits `LOCK_PARALLEL_DO` as the first statement *inside* a `parallel do`,
   and `WORK_IS_SHARED` is false while that lock is held, so a `PARALLEL_SUM` written in the loop
   body is **dead code that looks correct**. Four such sites in `molecule.grid.foo` each returned
   `1/n_ranks` of the answer. The intent ("MPI on the outside", no interior collectives) is right
   and standard; the enforcement is invisible, so — per Dylan — "the programmer has to hold the
   call sequence in their head not to make bugs". Agreed fix, all four parts:
   - ✅ **DONE (2026-08-04) — `parallel do … reduce(x)`**, lowered by `FooToFortran` to emit
     `PARALLEL_SUM` after `UNLOCK_PARALLEL_DO`. Removes the failure mode entirely: the
     reduction can no longer be written in the one place it is silently dead. Correct under
     nesting too — an inner loop never acquires the lock, so its `UNLOCK` does not release it,
     `WORK_IS_SHARED` stays false and the reduction is skipped, which is right because that
     rank was given the full range. `reduce` is **not** a reserved word (it is already an
     identifier in `becke_grid.foo`); it is matched as a plain name and checked in `emitDo`.
     Sum only — `PARALLEL_VECTOR_SUM`/`PARALLEL_SYMMETRIC_SUM` stay hand-written.
     **17 of the 23 existing sites converted** (the other 6 are `PARALLEL_SYMMETRIC_SUM`);
     14 of the 17 produced **byte-identical** generated Fortran and the other 3 differ only by
     dropping a redundant `if (WORK_IS_SHARED)` wrapper, so the lowering provably reproduces
     what was hand-written. Serial suite unchanged: 50/51 loose, exact 45, lastdig 48.
   - ❌ **Abort on a suppressed reduction** under `USE_PRECONDITIONS` — implemented, then
     **WITHDRAWN 2026-08-03: the premise is wrong.** A reduction reached while a parallel-do lock
     is held is *also* the intended nesting pattern (an inner `parallel do` + reduction in a
     routine called from an outer one runs serially over its full range per rank, so skipping the
     reduction is correct). `shell1quartet.foo` alone has 17 such loops and the check aborted
     every debug MPI run. It cannot be a `WARN` either — those sites fire per shell-quartet. The
     **lint** is the right enforcement: the real bug is *lexical* containment, which it detects
     precisely. Full reasoning in `DEFERRED.md`.
   - ⬜ **Fix the parallel-do lock — three defects, one mechanism** (design agreed 2026-08-03,
     full write-up in `DEFERRED.md`): (a) recursion clears an outer lock — depth-count it;
     (b) it assumes routine names are unique, which overloads break in principle (currently
     holds, since the translator suffixes them); (c) `LOCK_PARALLEL_DO` is emitted inside the
     loop body. **Correction: this is only worth fixing if lock-gated behaviour is wanted**, and
     the naive fix (move the emission) *silently disables distribution*, because
     `PARALLEL_DO_START`/`_STRIDE` consult the same flag before entry. Doing it safely means
     hoisting the bounds into temporaries, then locking — a translator change. Keep the
     holder's **name** alongside the depth: it is what names the offending routine in every
     diagnostic.
   - ✅ **Lint** for any `PARALLEL_*` macro lexically inside a `parallel do` body, and
     for any raw `write(`/`read(` on a `*.unit` expression outside `file.foo`/`textfile.foo`/
     `buffer.foo`. The second catches the raw-I/O class that crashed `DWGN_lamaGOET_NBO_file_47`
     -- and, more importantly, the *silent* variant of it: an unguarded write to a
     non-redirected stdout uses preconnected unit 6, valid on every rank, so it interleaves
     output instead of failing. Inspection alone cannot find those; a lint can.
     Done as `scripts/check_parallel_lint.py` — a **source scan**, not a translator pass,
     following `check_library_stdin.py`: registered as ctest `parallel_lint` (label `short`, so
     in CI) and as an invariant line in `make report`. Guard-aware (a site inside
     `if (IO_IS_ALLOWED)` is not reported — without that it flagged 48 correct sites). Clean over
     184 files and 74 `parallel do` loops. It complements the abort rather than replacing it: the
     lint sees only what is *lexically* inside a loop, the abort catches a reduction reached
     through a **call** from inside one.

   Sequenced *after* milestone 4's characterisation, because changing the lowering mid-flight
   would confound the numbers. Full design in `DEFERRED.md`, "MPI: defects found during
   milestone 4".

7. 🔶 **WORKED AROUND (2026-08-05), root cause not fully established.** Bisected to a single
   gcc flag: **`-foptimize-sibling-calls`** — of the 45 flags `-O2` enables over `-O1`, the only
   one whose removal fixes all four tests. A tail call tears down the caller's frame before
   jumping, and *any* statement after a call stops it being a tail call — which is precisely why
   a `write` probe and `-fcheck=all` both made the bug vanish: observing it removed the
   optimisation causing it. It is an interaction, not one bad pass: `-O1` plus all 45 flags
   passes, and `-O3`/`-Ofast` pass too, so the **shipped release build was never affected**.
   `CMakeLists.txt` now pins `textfile.F90` to `-fno-optimize-sibling-calls` (nil cost, restores
   the `-O2` control build). **Open:** which tail call, and whether this is a gcc bug or latent
   UB that tail calls merely expose — needs a reduced test case before reporting upstream.
   Earlier status, kept because the reasoning matters:
   Re-verified 2026-08-04 on achari2 (Linux) against current `master`: **all four tests still
   abort at `-n 2`** in the `-O2 -fno-fast-math` build with `MPI_ERR_TRUNCATE`, exit 15, with the
   gate fix confirmed present in that build; `-n 1` passes exactly. One real cause was found and
   fixed (`e3ef5906`, a collective gated on rank-local state — that stays fixed); the remainder is
   **localised to a codegen bug in `textfile.F90` at `-O2`**, with controls: pinning that one file
   to `-O1` makes all four tests pass (3/3) and removing the pin fails again (2/2); a single
   `write` probe inside `TEXTFILE:look_for_item` masks it (5/5); `-fcheck=all` masks it *and
   reports nothing*, so it is not a source-level out-of-bounds. The desync is one surplus integer
   broadcast on rank 0 after 8,481 matching ones. A `.record`-divergence hypothesis was tested and
   **rejected** (it matches on both ranks). Workaround ready but uncommitted (pin the file, as
   `types.F90` and `shell1quartet.F90` already are). Open: which `-O2` pass (bisect left running
   on achari2, `/tmp/m7bisect.log`), whether `-Ofast` is safe or merely lucky, and a minimal
   reproducer before blaming gcc. Full detail in `docs/TONTO_AND_MPI.md` Finding 6. Four CIF-reading tests (`c9o9h8_read_cif_IT_group_9`,
   `maleate_read_CIF_H_double_bond_{new,old}_BLs`, `urea_lamaGOET_grown_CIF`) aborted at ≥2 ranks
   with a mismatched `MPI_Bcast` in `-O2 -fno-fast-math` while passing at `-Ofast`.
   **It was never undefined behaviour** — that was inferred from the symptom and is wrong.
   `PARALLEL_BROADCAST` was gated on `WORK_IS_SHARED`, which includes the parallel-do lock, and
   **the lock is rank-local**: it is set by executing a loop body, which a rank given zero
   iterations never does. Two ranks could therefore disagree about entering a broadcast; MPI pairs
   collectives by issue order, so one skipped broadcast offsets the streams and the next pair
   mismatches (1-integer receive vs 256-character send). Optimisation level only changed whether
   the ranks happened to diverge. Nothing was uninitialised — the `-fcheck=bounds`/`-finit-*` plan
   would have found nothing. Fix: gate broadcasts and barriers on `is_parallel` alone, keep
   reductions on `WORK_IS_SHARED`. Rule: **whether a collective executes must never depend on
   state that can differ between ranks.** Because the shipped `-Ofast` build hides this class, no
   test can catch a regression, so `scripts/check_parallel_lint.py` now audits the gates in
   `macros.in` directly (verified to fail against the pre-fix definition) and runs in CI.
   **Verification gap, still open:** `e3ef5906` verified *three* of the four tests on achari2
   (Linux) at `-O2`, `-n 2`. The fourth, and a re-run of all four against current `master`, are
   outstanding — see `docs/TONTO_AND_MPI.md` Finding 6.

8. ✅ **DONE (2026-08-04) — Translator: `data` statements at program scope were silently
   dropped.** Root cause was one line in `emitBodyList`: `if (b.localDecl() == null &&
   b.stmt() == null) continue;  // blank / unhandled`. A `dataStmt` has both null, so it fell
   through the crack — the variable was still declared, so the Fortran compiled and simply ran
   uninitialised. Module scope was never affected (`emitModule` handles `data` separately),
   which is why the library's 171 `data` statements were always fine.
   **The audit it asked for was done, and the answer is reassuring**: comparing every source
   `data` against the generated output found *no* dropped statement anywhere in `foofiles/`
   (the 5 apparent mismatches are variables *named* `data`, e.g. `data :: VEC{REAL}, IN`), so
   no built binary was ever wrong. The only casualty was `runfiles/run_csq.foo` (3 statements),
   which is not in the build. The `hart` workarounds in `run_har.foo`/`run_sf.foo`/
   `run_sf_derivs.foo` are left alone — they work and are now tested.
   **The class is now closed, not just the instance**: the `continue` is gone. `dataStmt` is
   emitted; `implicitStmt` and `useStmt` in a body are skipped *deliberately*, each with a
   comment saying why (the translator emits its own `implicit none`; the module's `.use`
   include already covers `use TYPES` in `VEC{REAL}:min_BFGS`); and **anything else throws**,
   so a construct that parses but emits nothing is now a build failure rather than a silent
   wrong answer. Verified: 184/184 files translate, output byte-identical to before.

9. ✅ **DONE (`ffce26bd`) — `write_archive` swallowed the following keyword, and one test had
   never run its SCF.** `MOLECULE.PUT:put_archive` tested `stdin.buffer.n_items==2`, but
   `n_items` counts the whole line *including* the keyword — and `write_archive density_mx` is
   already 2 items, so the optional third word (`normalise`) was always sought and **the next
   line's first word was eaten instead**. Now `==3` (the third item is genuinely optional:
   objects such as `density_mx` know their own genre). `MOLECULE.READ:read_archive` had the
   identical bug, fixed during milestone 5; this was its twin. The same commit also fixed a
   copy-paste slip whereby the British spelling `normalise` was silently ignored here while
   `read_archive` accepted it.
   **The consequence was the part that mattered**, and it is repaired:
   `tests/long/nh3_x-ray-constrained-rhf-cluster-charge_cc-pVTZ_restart` ate its own `scf`
   keyword, ran in 40 ms, and its checked-in reference contained **zero** lines mentioning "SCF"
   in 635 lines — the test had never done the science its name claims. The reference was
   regenerated in the same commit: **947 lines, 12 s, SCF present**, and it passes *exactly*
   (0% deviation, 0 ulp) as of 2026-08-05. Blast radius was exactly that one job file, the only
   one in `tests/` using `write_archive`.
   **One open question for a scientist, not a bug:** the job asks for two lambda values
   (`initial_lambda= 0.012`, `lambda_step= 0.004`, `lambda_max= 0.016`) and the IO manifest
   deletes archives for both `lambda=0.012` and `lambda=0.016`, so both evidently ran — but the
   reference prints a single `SCF results` block and a single `Total energy` (−56.2023, R(F)
   0.0099, GoF 0.777). With `output= NO, output_results= YES` that may be correct; whether the
   results of *each* lambda should be reported has not been confirmed.

**Open items** (future directions; details in `DEFERRED.md`)

- **Grammar still ACCEPTS the old submodule call forms** (`.SET:proc`, `.MAIN:proc`, `STR::proc`)
  even though they are now auto-resolved away in the sources; not tightened (harmless).
- **README/wiki reorganisation** (in progress, 2026-07-27) — split responsibilities: README =
  build + verify/test only; `docs/` = code-tracking dev references; wiki = user guides. Default
  build should be `release` (not `fast`); retire event-specific blocks to the wiki.
- **Relocate the fragment machinery: a `CRYSTAL` should contain several `MOLECULE`s** (Dylan,
  2026-08-03). Today a `MOLECULE` holds a `CRYSTAL` *and* holds `.mol(g)`, a set of `MOLECULE`s,
  which forces `MOLECULE.SCF:fragment_scf` to call back into `MOLECULE.SCF:scf` — the 12-node
  call cycle that makes the parallel-do lock unsafe in the one routine where it matters. Moving
  `fragment_SCF` onto `CRYSTAL` dissolves the cycle, the recursion defect, and the need for a
  cloned `subfrag_SCF`, and puts the decision to distribute work over fragments in the container
  where it belongs. Full argument in `DEFERRED.md`.
- **LONG TERM — re-engineer in a language with first-class parallelism** (Dylan, 2026-08-05;
  **not now**, and a bigger task than hoisting `CRYSTAL`). The case for it has been built by
  evidence, not preference. Tonto's parallelism is MPI bolted on through C macros, and the
  failure modes found in the last week were all *invisible*: eight reductions that silently
  returned `1/n_ranks` of the answer; a per-rank I/O flag whose setter assigned the wrong member,
  so the mechanism was dead code that looked live; collectives gated on rank-local state, so
  different ranks entered different collectives; `data` statements parsed and silently discarded;
  and now a `-O2`-only codegen interaction in the I/O layer that desynchronises the ranks and
  **disappears the moment you instrument it**. Each was found only by tracing, none by reading.
  A language where reductions, collectives and data placement are checked constructs rather than
  macro expansions removes these classes by construction instead of by lint. Sequence: finish
  what is in flight, then hoist `CRYSTAL` (October), then consider this.

- Future tasks (own conversations): a module-level *call* graph in `writeDotFiles` (the
  `--simplify`/`--module` **use**-graph tooling is DONE — `scripts/simplify_callgraph.py`,
  `docs/MAKING_CALL_GRAPHS.md`); introduce Fortran-2008 `submodule` constructs; test the MPI parallel
  build; boilerplate doc comments; and (long-term) a possible move off Fortran. (Testing the MPI build
  is now milestone 4 above.)

> Submodules ARE implemented (dotted headers + colon call forms parse & auto-resolve; commit
> `4cd995df`), and translator build/run commands are recorded in §8 — both former open items done.
