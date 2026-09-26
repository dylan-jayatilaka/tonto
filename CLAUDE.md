# CLAUDE.md

Durable, project-wide context for Claude Code, read at the start of every session.
**Stable facts only** — build, test, layout, conventions. Live work is in `TASKS_AND_HISTORY.md`;
the story of how the build and translator came to be is in `docs/PROJECT_HISTORY.md`.

## 1. How to write for this project

Read this first. It is the rule most often broken, and breaking it is expensive: a slab
of explanatory text is cheap for a model to write and costly for a person to read, and it
crowds out the direction of the project.

**Plain language, everywhere.** Dylan is a scientific programmer, not a software engineer,
and Tonto's users are scientists. Write for them — in documents, in code comments, in commit
messages and in conversation. Use a jargon word only when no plain word will do, and then say
what it means the first time. Say what happens rather than naming the practice:

| Not this | This |
|---|---|
| the suite is gated on the loose criterion | a test passes if every number agrees with its reference to 0.2%, or to within 2 in the last digit |
| a red badge means a regression | a failed test means Tonto's results have changed |
| not a flag difference | not a difference in compiler settings |
| `WARN` is gated on `USE_PRECONDITIONS` | `WARN` exists only when `USE_PRECONDITIONS` is defined |

One idea per sentence. If a sentence has to be read twice, rewrite it.

**Three kinds of writing, and they do not mix.**

| Where | Audience | Rule |
|---|---|---|
| **Source** — `foofiles/*.foo`, `scripts/`, CMake | whoever edits the line | Say what the code does, and why if it is not obvious. Nothing else. |
| **`docs/` and `README.md`** | a person, once | Brief. Facts to know, never how they were found. |
| **Working documents** — `TASKS_AND_HISTORY.md`, `docs/PROJECT_HISTORY.md`, `docs/TONTO_AND_MPI.md`, `docs/TONTO_DEVELOPER_INFO.md`, and the per-item reports and plans | the next session | Free to be long. What was measured, what was ruled out, what was decided and why. |

**Which `docs/` files are working documents?** Only those covering an item still in flight —
today `TONTO_AND_MPI.md`, `TONTO_DEVELOPER_INFO.md`, `PROJECT_HISTORY.md`, `DFT_STANDARDISATION.md`,
`EXTINCTION_REPORT.md`, `GFORTRAN16_*.md`, `TONTO_SCF_SPEED_UP.md`, `CCTBX_INTO_TONTO.md`, `GOF_NOT_CHI2.md`,
`TONTO_DISPERSION_CORRECTIONS.md`, `TONTO_RI_FITTING_PLAN.md`, and the
`*_REPORT.md` and `*_PLAN.md` files. **They are deleted when their item closes**, and their durable residue
moves into the user-facing pages. Everything else in `docs/` is user-facing.

**Specific rules for user-facing pages** (`README.md`, `docs/BUILDING_*`, `docs/RUNNING_*`,
`docs/INSTALLING_*`, `docs/DOCUMENTATION.md`, `docs/FOO_*`, `docs/TONTO_BLESSING_TESTS.md`,
`docs/TONTO_LIBRARY_STRUCTURE.md`,
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

The ordered list of next actions, and the reasoning behind each, is the **handover section
at the top of `TASKS_AND_HISTORY.md`** — read it before starting work. In outline: the SCF speed-up —
the primitive pair list for J, density-fitted J (RI-J) and COSX for the exchange of HF and
hybrids are merged, all three options off by default; next is a grid made for COSX, to be
planned first. The CI platform table, the three untracked debug failures and the open MPI and
DFT items are parked behind that.

**Do not move `TASKS_AND_HISTORY.md`'s contents into this file.** This file answers *how does the
project work*; `TASKS_AND_HISTORY.md` answers *what are we doing now*. Merging them loses both.

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
  `STR::proc(self,…)`. The grammar still accepts these older call forms for backward
  compatibility, though no source uses them; see `docs/FOO_GRAMMAR_DOCUMENTATION.md`.
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

**The project standard compiler is `gfortran-14`.** Do not move to 16 yet. An Ubuntu
`gfortran-16` package at 16.1.0 or later is **necessary but not sufficient** — macOS has
16.1.0 and its *debug* build still segfaults with `-fcheck=bounds` already omitted, so the
blocker is not only the bounds bug; see `docs/GFORTRAN16_DEBUG_CRASH.md`. The
`-fcheck=bounds` miscompilation
([GCC PR 127197](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=127197), a duplicate of
PR 124661) is fixed upstream but the shipped `16-20260322` snapshot predates the fix.
Everything on Tonto's side is done and is preserved on `develop-gfortran-16`; merge that
branch and flip `FC_VERSION` rather than redoing it.
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
build/test run** (§11). A test passes by the **loose** criterion in `scripts/test.py`
(every number within 0.2%, or within 2 in the last printed digit), not by exact match.

**Quote a score with the suites it counted**, or it cannot be compared with the next one:
`short long hart` is **106** tests (68 + 33 + 5, by `ctest -N -L`) and is what
`ci-full-suite.yml` runs; the four suites `short long cx rgbi` are **146**. The last full-suite run at
gfortran-14 was **88/89 loose, 77 exact**, the 89th a deliberate skip — taken before
`yq28_anharm_disp_remove_from_F_exp` was added, so it is a score out of 89. The debug (`-O0`) build
has longstanding FP-boundary and structural failures listed in `TASKS_AND_HISTORY.md` — not translator
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
extension; see `TASKS_AND_HISTORY.md`. Each directory has a `README.md` saying this.


## 10. Reference docs in this repo

**Everything is in this repository.** The GitHub wiki was retired on 2026-08-05 and its nine
pages migrated into `docs/` — it was superfluous, hard to maintain, and worst of all *not
versioned with the code it described*, so it could rot silently. Do not add documentation there.

The index is `docs/DOCUMENTATION.md`, linked from the README; one self-contained building
page per platform (`docs/BUILDING_ON_{LINUX,MACOS,WINDOWS}.md`); `docs/RUNNING_TONTO.md`,
`docs/RUNNING_HART.md` and `docs/RUNNING_RGBI.md` for the three programs;
`docs/TONTO_DEVELOPER_INFO.md` for the developer reference (§1a MPI pitfalls, §1b build and
test traps, §1c profiling and timing); `docs/FOO_GRAMMAR_DOCUMENTATION.md` for the language.
`TASKS_AND_HISTORY.md` is the live work and `docs/PROJECT_HISTORY.md` the background — both working
documents (§1). The `*_REPORT.md` and per-item pages in `docs/` are working documents too.

## 11. Working agreement

- Plan before coding; don't run `make` / `ctest` without asking.
- **Write in plain language** (§1) — chat, documents, comments and commit messages alike.
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
- **activates `WARN` / `WARN_IF`**, which exist only when `USE_PRECONDITIONS` is defined and so
  compile to nothing in release. `DIE`/`DIE_IF` exist when `USE_ERROR_MANAGEMENT` is defined, and so *are* live in
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

### `hart`, the translator and the analysis modes

`hart` is built by the ordinary `make` and appears next to `build/tonto`; a 5-second job and
the full option reference are in `docs/RUNNING_HART.md`. The translator is built and run by
`scripts/build_translator.sh` (no argument: build; a `.foo` path: translate that one module
into `antlr4-release/`), and `types.foo` is always passed first (§8). The call-graph and
dead-code modes (`--call-graph-report`, `--dead-code-report`, `--purge-dead-code`; CMake
`callgraphs` target and `-DPURGE_DEAD_CODE=<stem>` in a separate build tree) are in
`docs/TONTO_DEVELOPER_INFO.md` §1. Wholesale-`use` modules are never pruned.

## 12. Open items

Future directions; details in `TASKS_AND_HISTORY.md`.

- **Relocate the fragment machinery: a `CRYSTAL` should contain several `MOLECULE`s.** Today a
  `MOLECULE` holds a `CRYSTAL` *and* holds `.mol(g)`, which forces
  `MOLECULE.SCF:fragment_scf` to call back into `MOLECULE.SCF:scf` — the call cycle that makes
  the parallel-do lock unsafe where it matters. Moving `fragment_SCF` onto `CRYSTAL` dissolves
  the cycle and the recursion defect.
- **Re-engineering: flatten the object model, and move to a language with first-class
  parallelism.** Two arguments, and they converge on Julia. *Parallelism:* every defect found so
  far was invisible to inspection, and a language where reductions and collectives are checked
  constructs removes those classes by construction instead of by lint. *Data model:* the derived
  types behave more like persistent data records than objects, and the deep hierarchy is a large
  part of why the code is inefficient. **The strategy is to destructure inside Foo first** —
  nothing in Fortran mandates the OO paradigm, it was imposed by the Foo layer, so it can be
  undone there progressively, making the eventual migration a translation rather than a redesign.
  Hoisting `CRYSTAL` is the first step. Begins December 2026 or early 2027; see `TASKS_AND_HISTORY.md`.
- A module-level *call* graph in `writeDotFiles`; Fortran-2008 `submodule` constructs;
  boilerplate doc comments.
