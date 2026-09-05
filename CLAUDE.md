# CLAUDE.md

Durable, project-wide context for Claude Code, read at the start of every session.
**Stable facts only** — build, test, layout, conventions. Live work is in `DEFERRED.md`;
the story of how the build and translator came to be is in `docs/PROJECT_HISTORY.md`.

## 1. How to write for this project

Read this first. It is the rule most often broken, and breaking it is expensive: a slab
of explanatory text is cheap for a model to write and costly for a person to read, and it
crowds out the direction of the project.

**Three kinds of writing, and they do not mix.**

| Where | Audience | Rule |
|---|---|---|
| **Source** — `foofiles/*.foo`, `scripts/`, CMake | whoever edits the line | Say what the code does, and why if it is not obvious. Nothing else. |
| **`docs/` and `README.md`** | a person, once | Brief. Facts to know, never how they were found. |
| **Working documents** — `DEFERRED.md`, `docs/PROJECT_HISTORY.md`, `docs/TONTO_AND_MPI.md`, `docs/TONTO_DEVELOPER_INFO.md`, and the per-item reports and plans | the next session | Free to be long. What was measured, what was ruled out, what was decided and why. |

**Which `docs/` files are working documents?** Only those covering an item still in flight —
today `TONTO_AND_MPI.md`, `TONTO_DEVELOPER_INFO.md`, `PROJECT_HISTORY.md`, `DFT_STANDARDISATION.md`,
`EXTINCTION_REPORT.md`, `GFORTRAN16_*.md`, `CCTBX_INTO_TONTO.md`, `GOF_NOT_CHI2.md`,
`TONTO_DISPERSION_CORRECTIONS.md`, and the
`*_REPORT.md` files. **They are deleted when their item closes**, and their durable residue
moves into the user-facing pages. Everything else in `docs/` is user-facing.

**Specific rules for user-facing pages** (`README.md`, `docs/BUILDING_*`, `docs/RUNNING_*`,
`docs/INSTALLING_*`, `docs/DOCUMENTATION.md`, `docs/FOO_*`, `docs/TONTO_LIBRARY_STRUCTURE.md`,
`docs/TONTO_CALL_GRAPHS.md`, `docs/TONTO_EDITING_WITH_VIM.md`, `docs/TONTO_CONTINUOUS_INTEGRATION.md`):

- **No dates, no commit hashes, no run numbers.** If a sentence needs one, it is history.
- **No "measured, not assumed", no "this was tried and rejected", no symptom stories.**
  State the fact; the investigation goes in a working document.
- **No parenthetical corrections of earlier text.** Fix the text.
- **A pitfall is one or two lines**, plus a link to the document that carries the detail.
- **Adding to a page? Check whether something can come out.** These pages should not grow
  monotonically.

**In the source, three further specifics:**

- **Procedure header documentation** may be longer, and is the right place for an
  explanation a caller genuinely needs. Use that latitude sparingly.
- **Type component descriptions** (`types.foo`) stay **very brief** — a line, ideally.
- A **pitfall** that would cause the next person to reintroduce the bug may be noted, in
  one or two lines, pointing at the document that carries the detail.

The test: if a comment explains the *bug*, it belongs in a document. If it explains the
*code*, it belongs in the code — in as few lines as will do.

**Commit messages are exempt.** They are versioned with the change, read by developers on
purpose, and verbose is right there.

## 2. Current focus

Nothing is in flight. The ordered list of next actions, and the reasoning behind each, is
the **handover section at the top of `DEFERRED.md`** — read it before starting work. In
outline: finish the CI platform table (macOS badges, WSL-MPI), then the three untracked
debug failures, then the open MPI and DFT items.

**Do not move `DEFERRED.md`'s contents into this file.** This file answers *how does the
project work*; `DEFERRED.md` answers *what are we doing now*. Merging them loses both.

## 3. What this project is

**Tonto** is a quantum chemistry / crystallography package. Its scientific code is written
in **Foo**, a custom object-oriented preprocessor language that is translated to modern
Fortran (95 / 2003+) and then compiled.

- Foo sources live in `foofiles/` (`*.foo`). Maintainer: Dylan Jayatilaka.
- The translator is `foogrammar/FooToFortran.java`, driven by the grammar `foogrammar/Foo.g4`
  and by ANTLR4 (`external/antlr-4.13.2-complete.jar`, a release jar, not a submodule). It
  replaced a Perl translator, `foo.pl`, which no longer exists — see `docs/PROJECT_HISTORY.md`.
- `build/`, `release/` and `debug/` are ordinary out-of-source CMake build trees: untracked and
  regenerable.
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

## 4. Branching model

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


## 5. The Foo language (summary)

Full details in `docs/FOO_GRAMMAR_DOCUMENTATION.md`.

- **Reverse declarations:** `varname :: TYPE` (e.g. `i :: INT`, `matrix :: MAT{REAL}`).
- **Primitive types:** `INT`, `REAL` (double precision), `CPX`, `BIN` (logical), `STR`.
- **Parameterized array types** with `{...}`: `VEC{T}`, `MAT{T}`, `MAT3{T}` … `MAT7{T}`;
  nestable (`VEC{VEC{REAL}}`). Dimensions/params with `(...)`: `STR(len=256)`,
  `MAT{REAL}(3,4)`, `VEC{STR}(len=1,6)`.
- **Pointer / allocatable suffixes:** `INT*` (pointer), `VEC{REAL}@` (allocatable).
- **Procedures:** `name(args) result (res) :: ATTRS`. Attributes after `::` include `PURE`,
  `ELEMENTAL`, `leaky`, `private`, `get_from(MODULE, ...)`. The separator was `:::` in the old
  dialect; the tag **`foo-old-syntax`** marks the last commit written in it, and is the bridge
  for porting archived work — merge an `archive/*` branch there, then replay the `:::`→`::`
  change. See `docs/TONTO_REPOSITORY_BRANCHES.md`.
- **`PURE` vs `pure` — the case matters.** Upper-case `PURE`/`ELEMENTAL` are **macros**
  (`include/macros.in`), `#undef`'d to nothing under `USE_PRECONDITIONS` and under `MPI`.
  Lower-case `pure` is passed through as the **literal Fortran keyword** and stays pure in every
  build. So a routine containing `ENSURE`, `DIE`, `WARN` or any other call that writes `tonto`
  must be declared `PURE`, never `pure` — otherwise it compiles in release (where `ENSURE`
  vanishes) and **fails only in a debug or MPI build**, with gfortran's misleading *"There is no
  specific subroutine for the generic `ensure_`"* rather than a purity error. Cost the debug CI
  a red badge on 2026-08-02; see the note at `PARALLEL:reduction_is_allowed`.
- **Never reach a `WARN`/`ENSURE` through a line continuation.** The sibling trap, and it
  fails the *other* way round — in **release**, not debug. Under an optimised build
  `WARN(X)` expands to a **comment** (`! Warning message: X`), so

  ```
  if (cond) &
     WARN("...")
  ```

  leaves a bare `if (cond)` with no statement: *"Syntax error in IF-clause"*. It compiles
  happily in debug, where the macro is real. Use **`WARN_IF(cond,"...")`**, which exists for
  exactly this, or the block form `if (cond) then / WARN(...) / end` — an empty block body is
  legal, a continued `if` with nothing after it is not. Every `WARN` in `foofiles/` uses one of
  those two. Found 2026-08-23 by the first release build of the `Lolo_CP2K` port; the
  same rule applies to `WARN_IF`, `ENSURE` and any other macro that vanishes in some build.
- **Variable attributes** (comma-separated, after the type): `IN`, `OUT`, `INOUT`, `PRIVATE`,
  `READONLY`, `POINTER`, `TARGET`, `SAVE`, `ALLOCATABLE`, `OPTIONAL`.
- **Modules:** `module NAME … contains … end`; generic `interface NAME … end` blocks.
- **Submodules:** a large class may be split across files. `molecule.base.foo` declares
  `module MOLECULE.BASE`, a submodule of `MOLECULE` (file-name head = lower-case type name).
  Submodule-qualified calls put the submodule before a colon: `.SET:proc` (generic) /
  `.SET::proc` (non-generic); `.:proc` / `.::proc` within the same submodule; `.MAIN:proc`
  for the main module. Explicit calls pass `self`, e.g. `STR:proc(self,…)` /
  `STR::proc(self,…)`. (See §12 — the grammar still accepts the older call forms.)
- **Control flow:** `if/else if/else … end`, `select case … end`, `do … end`.
- **Comments:** `!` to end of line. **Constants:** `TRUE`, `FALSE`, `ZERO`, `ONE`, `NULL`.
- Case-insensitive keywords; identifier case preserved. `;` separates statements on one line.
- **Indentation is 3 spaces** and marks a new scope block, closed by an `end` keyword.


## 6. Building

CMake, out-of-source. Toolchain (`make`, `perl`, `gfortran-14`, `blas`, `lapack`, `python3`,
`gnuplot`) is already installed.

```bash
mkdir build && cd build
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release
make -j
```

**The project standard compiler is `gfortran-14`.** Do not move to 16 until
[GCC PR 127197](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=127197) — `-fcheck=bounds`
miscompiled — is fixed. Everything on Tonto's side is done and is preserved on
`develop-gfortran-16`; merge that branch and flip `FC_VERSION` rather than redoing it.
A 16 release build is numerically free on Linux and macOS; a 16 debug build works but has
no array bounds checking, which is the reason to wait. Detail:
`docs/GFORTRAN16_GCC_BUG.md`, `docs/GFORTRAN16_DEBUG_CRASH.md`.

**No toolchain PPA in the build or in CI.** `ppa:ubuntu-toolchain-r/test` is used only where a
compiler newer than the distribution's is genuinely required — today `ci-mpi.yml` alone — and
goes as soon as that compiler reaches the archive. A build whose output is compared against
stored references must not silently change where its packages come from: the PPA carries a
newer *minor* release of gfortran-14 than the Ubuntu archive, which is enough to turn the
reference build red. `ci.yml` records the exact package builds of `gfortran`, `libblas-dev`
and `liblapack-dev` on every run.

Other build types: `debug`, `release-static`, and MPI (`-DCMAKE_Fortran_COMPILER=mpifort
-DMPI=1`). **The MPI must be built with the same Fortran compiler** — Tonto does `USE mpi` and
`.mod` files are compiler-version specific; configure checks this and stops. `-DMPI=1` is a hard
requirement: if MPI is not found, configure fails rather than silently producing a serial
binary. See `docs/TONTO_AND_MPI.md`.

**WSL is a supported build host.** `cmake/WSL.cmake` (a no-op elsewhere) strips `/mnt/*` off
`PATH` before any tool search, so `find_package(Java)` cannot resolve to a Windows `java.exe`,
and hard-errors on a `/mnt/c` build tree or CRLF sources. `-DTONTO_WSL_STRICT=OFF` downgrades
those to warnings. `scripts/wsl_doctor.sh` is the user-facing preflight;
`scripts/wsl_selftest.sh` asserts every guard on an ordinary Linux box. Details in
`docs/BUILDING_ON_WINDOWS.md`.

## 7. Validation

Build a `release` tree and run `ctest` — but, like `make`, **ask before launching a long
build/test run** (§11). The pass/fail gate is the **loose** criterion in `scripts/test.py`
(rel ≤ 0.2% OR last-digit ≤ 2), not exact match.

**Quote a score with the suites it counted**, or it cannot be compared with the next one:
`short long hart` is **90** tests (55 + 32 + 3) and is what `ci-full-suite.yml` runs; all four
ctest-registered suites (`short long cx rgbi`) are **132**. The last full-suite run at
gfortran-14 was **88/89 loose, 77 exact**, the 89th a deliberate skip — taken before
`yq28_anharm_disp_remove_from_F_exp` was added, so it is a score out of 89. The debug (`-O0`) build
has longstanding FP-boundary and structural failures listed in `DEFERRED.md` — not translator
bugs.

## 8. Conventions & gotchas

- Edit `.foo` sources in `foofiles/`, never the generated Fortran.
- During a normal build, generated Fortran lands in the build tree (e.g. `build/`, `release/`);
  do not hand-edit it — edit the `.foo` sources instead.
- `external/lapack-release` is the **only** git submodule; clone with `--recursive`. ANTLR4 is
  **not** a submodule — it is a release jar, `external/antlr-4.13.2-complete.jar`.
- Files can be translated independently **provided `types.foo`, which defines every derived
  type, is processed first**.

## 9. The two script directories

`rgbi-scripts/` is **installed** — `make-rgbi-pic` and `make-rgbi-dials` into `bin`, the
`.tex`/`.sty` templates into `share/tonto/rgbi-scripts`. `scripts/` is **not**: test harness,
invariant checks, lints, doctors, and `scripts/docker/`. That is the boundary — installed
versus not, rather than subject matter.

The drivers in `rgbi-scripts/` deliberately have **no `.sh` extension**: they are commands
on `PATH`, named as commands are. `scripts/*.sh` keep theirs because they are invoked by
path. The one inconsistency is `rgbi_doctor.sh`, which is installed into `bin` with its
extension; see `DEFERRED.md`. Each directory has a `README.md` saying this.


## 10. Reference docs in this repo

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
- `docs/TONTO_DEVELOPER_INFO.md` — developer reference; §1a is **writing parallel (MPI) code in Foo**, eight
  pitfalls and the trace recipes that found them; §1b is **build and test traps** — the stale
  translation when a `.foo` is edited mid-build, why the loose gate passes visibly wrong output,
  and how `scripts/test.py` actually compares.
- `docs/FOO_GRAMMAR_DOCUMENTATION.md` — full language description and Foo→Fortran conversion rules.
- `docs/TONTO_AND_MPI.md` — the parallel build, its numeric characterisation, and the defect register.
- `docs/BADER_REPORT.md` — the `archive/Bader` port (2026-08-18): the ten procedures that landed, what
  was deliberately left on the tag, and the two defects found by running it — a basin count that swings
  from 1 to 13942 with the grid, and voxel volumes summed per point but sized per interval.
- `docs/TEACHING_MP2.md` — the MP2 teaching lab ported from `archive/Teaching`: `run_mp2` and
  `run_mp2_exercise`, both `EXCLUDE_FROM_ALL`, and the validation showing `run_mp2` reproduces the
  library `mp2` keyword to twelve decimals once the frozen-core active space matches.
- `docs/EXTINCTION_REPORT.md` — the secondary-extinction correction: dormant since 2016-10-02, its
  eight silent defects, Lorraine Malaspina's prior work on `origin/Lolo_CP2K`, and the reactivation
  plan. Includes the two decisions that must be taken first — Larson's angular factor or SHELXL
  eq (62), and what `N_p` should be in the XCW stage of an XWR.
- `docs/GOF_NOT_CHI2.md` — the quantity called `chi2` throughout the code is a GoF²; the rename, and
  reporting GoF rather than its square in the refinement tables. Kept separate from the extinction
  work on purpose.
- `docs/DFT_STANDARDISATION.md` — milestone 10: the DFT machinery, its three silent defects, the
  functional-interface analysis, and the libxc plan.
- `docs/BUILDING_ON_WINDOWS.md` — the four WSL-specific traps, the CMake guards, and how they are tested.
- `docs/TONTO_CONTINUOUS_INTEGRATION.md` — the CI workflows, how to trigger one manually, and how to read a run.
- `docs/TONTO_CALL_GRAPHS.md` — call/use graphs and dead-code elimination.
- `docs/TONTO_EDITING_WITH_VIM.md` — vim set-up: tags, folding, completion.
- `DEFERRED.md` — the live work: the handover section at the top, then every open issue by
  theme, then an archive of closed ones. **A working document** (§1).
- `docs/PROJECT_HISTORY.md` — why the ANTLR4 translator exists and what the twelve milestones
  found. Background, not current work. **A working document** (§1).


## 11. Working agreement

- Plan before coding; don't run `make` / `ctest` without asking.
- **A parallel build is safe, and `-j` is the right thing to use.** `FOO_TRANSLATOR_XMX` caps
  every per-file translator JVM at `512m`, and `FOO_ANALYSIS_XMX` gives the whole-library modes
  `2g`. Scale `-j` to free memory at roughly 512 MB per job. Two rules follow:
  - **Do not "fix" a memory problem by dropping to `-j1`.** The cap is the lever.
  - **Never remove or raise the cap** without measuring. `-j` limits process count and `-l`
    triggers on load average; neither knows anything about memory. The reasoning is in
    `CMakeLists.txt` above `FOO_TRANSLATOR_XMX`.

### Debugging and instrumenting Foo code — do it in a DEBUG build

`DEBUG_FLAGS` defines `USE_PRECONDITIONS`, which in `include/macros.in`:

- **`#undef`s `PURE`**, so a probe can go inside a `PURE` routine. In release, `PURE` is real and
  a `stdout.show`/`flush` there fails to compile — usually with a misleading "no specific
  subroutine for the generic `flush_`" rather than a purity error.
- **activates `WARN` / `WARN_IF`**, which are gated on `USE_PRECONDITIONS` and so compile to
  nothing in release. `DIE`/`DIE_IF` are gated on `USE_ERROR_MANAGEMENT` and *are* live in
  release. A check that must fire in production has to be a `DIE`.
- adds `-fcheck=bounds`.

Keep the debug test job quick (`tests/long/urea_rhf_STO-3G_HAR` is ~4 s) so the edit-build-run
loop stays usable.

**Three traps from Foo's overloading**, which makes the code pleasant to use and hard to track:

1. **Tracing an overload? Read the `.int` file first.** For each generic, the generated
   `<module>.int` in the build tree lists the candidate specifics under their distinct
   translator-assigned names (`put_ADP2_errors_to_0`, `_1`, …). It does not say which one a call
   site resolves to: for that, put a `DIE` in the suspect routine and build with `-fbacktrace`,
   which names the specific procedure *and* its callers in one run. See
   `docs/TONTO_DEVELOPER_INFO.md` §3.
2. **Confirm the path executes before analysing it.** A name match is not the overload that runs
   — `put_CIF`, `make_CIF_esds`, `set_pADP_errors_to`, `put_ADP2_errors_to` and
   `LS_structure_fit` all exist in several versions. Print a bare marker first.
3. **Generic imports are per-module and inferred from observed calls.** `stdout.show("x",<expr>)`
   with an argument type that module has not used before gives "no specific subroutine for the
   generic `show_`". Assign to a declared variable first, then show that.

*Also note:* the `shell1quartet.F90` `-O2` pin (arm64 macOS workaround, §6) applies in every
build type, so in a debug build that one file is compiled `-O2`. Harmless for correctness, but
it hampers debugging that file.

### `hart` build/run

`run_har` is built by the ordinary `make`, so `build/hart` appears alongside `build/tonto`.
A ~5 s end-to-end job:

```bash
mkdir -p /tmp/hart && cd /tmp/hart
cp <repo>/tests/hart/urea_hart_STO-3G/urea_init.cif .
TONTO_BASIS_SET_DIRECTORY=<repo>/basis_sets \
  <repo>/build/hart --job urea --basis STO-3G --grid-accuracy low urea_init.cif
ctest -L hart      # the suite + the options invariant check
```

Full option reference: `docs/RUNNING_HART.md`.

### Translator build/run

Helper script: `scripts/build_translator.sh`.

```bash
scripts/build_translator.sh                    # generate parser + compile translator
scripts/build_translator.sh foofiles/irrep.foo # translate one module into antlr4-release/

# Equivalent manual invocation:
JAR=$PWD/external/antlr-4.13.2-complete.jar   # absolute; override with $ANTLR_JAR
( cd foogrammar && java -cp "$JAR" org.antlr.v4.Tool -visitor -o ../build/translator/gen Foo.g4 )
javac -cp "$JAR" -d build/translator/classes build/translator/gen/*.java foogrammar/FooToFortran.java
java -cp "$JAR:build/translator/classes" FooToFortran \
     --types foofiles/types.foo --foo foofiles/irrep.foo --out-dir antlr4-release
```

`FooToFortran` writes `<stem>.F90`, `<stem>.int`, `<stem>.use` (stem maps `vec{real}.foo` →
`vec_real`). `types.foo` must be passed so the derived-type table is built first (§8). This
single-module path is a dev aid; the normal build is via CMake.

### Analysis modes — call graph and dead-code elimination

```bash
# DOT graphs (no root needed) + dead-code report (root needed); shares one graph build:
java -cp "$JAR:build/translator/classes" FooToFortran --types foofiles/types.foo \
     --dead-code-report runfiles/run_molecule.foo --call-graph-report --out-dir <dir>
# Purge: emit only procedures reachable from run_molecule into <dir>:
java -cp "$JAR:build/translator/classes" FooToFortran --types foofiles/types.foo \
     --purge-dead-code runfiles/run_molecule.foo --out-dir <dir>
```

CMake exposes these as the `callgraphs` target and the `-DPURGE_DEAD_CODE=<stem>` option (a
**separate** build tree — purge is per-executable). Wholesale-`use` modules (`TYPES`/`SYSTEM`)
are never pruned. A purged release build compiles clean, drops ~32% of procedures, and passes
the same loose suite as the full build.

## 12. Open items

Future directions; details in `DEFERRED.md`.

- **Grammar still accepts the old submodule call forms** (`.SET:proc`, `.MAIN:proc`,
  `STR::proc`) even though the sources no longer use them. Not tightened; harmless.
- **Relocate the fragment machinery: a `CRYSTAL` should contain several `MOLECULE`s.** Today a
  `MOLECULE` holds a `CRYSTAL` *and* holds `.mol(g)`, which forces
  `MOLECULE.SCF:fragment_scf` to call back into `MOLECULE.SCF:scf` — the call cycle that makes
  the parallel-do lock unsafe where it matters. Moving `fragment_SCF` onto `CRYSTAL` dissolves
  the cycle and the recursion defect.
- **Long term — re-engineer in a language with first-class parallelism.** Not now, and a bigger
  task than hoisting `CRYSTAL`. The case is built on evidence: every parallelism defect found so
  far was invisible to inspection. A language where reductions and collectives are checked
  constructs removes these classes by construction instead of by lint.
- A module-level *call* graph in `writeDotFiles`; Fortran-2008 `submodule` constructs;
  boilerplate doc comments.
