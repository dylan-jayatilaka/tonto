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
  refinement; `hart --help` — see `docs/HART.md`).
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
FP-boundary/structural failures (not translator bugs — see `DEFERRED.md`). Phase B
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
-DMPI=1`). The MPI must be built with the **same** Fortran compiler — Tonto does `USE mpi` and
`.mod` files are compiler-version specific; configure now checks and stops with a clear message.
`-DMPI=1` is a hard requirement: if MPI is not found, configure fails rather than silently
producing a serial binary. See `docs/MPI.md`.

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
runs on every push via `.github/workflows/ci-wsl.yml`. Details in `docs/BUILD_WSL.md`.

## 5. Validation

The `antlr4` translator task is **complete**; validation is now **build + `ctest`**:

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
- `external/*` are git submodules (sbf, lapack-release, antlr4); clone with `--recursive`.
- Note that the files can be translated independently *provided* the `types.foo` file
which defines all the derived types is processed first. The legacy translator uses
two passes through the module file but it is not clear whether ANTLR4 needs two passes
once the Parse tree is generated.

## 7. Reference docs in this repo

- `docs/FOO_GRAMMAR_DOCUMENTATION.md` — full language description and Foo→Fortran conversion rules.
- `docs/BUILD_WSL.md` — building under WSL: the four WSL-specific traps, the CMake guards, and how they are tested.
- `docs/CI.md` — the three CI workflows, how to trigger one manually, and how to read a run.
- `docs/HART.md` — the `hart` program: what it hard-codes, its full `--option` reference, how
  it is tested (`tests/hart/`, the `program:`/`args:` IO keys, the invariant check), and its
  remaining milestones.
- `DEFERRED.md` — project-wide deferred issues (was `ANTLR4_DEFERRED.md`).
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
   steps answer directly. See §3 of `docs/DEVELOPER.md`.

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

Full option reference and testing notes: `docs/HART.md`.

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
   Full report: `docs/MPI.md`. Build with
   `-DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_C_COMPILER=mpicc -DMPI=1` and compare against the
   serial references with the usual loose gate. (`-DCMAKE_CXX_COMPILER=mpicxx` was in this
   recipe but is **ignored** — `project()` enables `Fortran C` only; `-DNO_ERROR_MANAGEMENT` was
   a **no-op**. Both removed.) **The MPI must be built with the same Fortran compiler**, since
   Tonto does `USE mpi`. Details, and the list of MPI defects found, in `docs/MPI.md`. **Expect numeric drift**: reduction order varies with rank count, and
   some of it is genuine UB — per Dylan, "numerics might go off — no worries, we'll check". The
   deliverable is a *characterisation* (which tests drift, by how much, and whether the drift is
   rank-count dependent), not necessarily a green suite. Untested since before the ANTLR4 work.

5. 🔶 **`hart` — verify, test, document, and make it work with `fragHAR`.** Everything except
   the `fragHAR` part is **DONE**; see **`docs/HART.md`**, which is now the authoritative
   document for the program.
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
   - ⬜ *make it work **seamlessly with `fragHAR`***, i.e. crystals with more than one molecule
     in the asymmetric unit — **milestone H1 in `docs/HART.md`**, the remaining work. `hart`
     calls only `HAR_refinement`. (`tests/long/gly_ala_fragHAR_rhf_STO-3G` exercises `fragHAR`
     through `tonto`, and Dylan's `gaussian-IAM` branch carries a commit "fragHAR fixed,
     gly_ala test and others need to be modified/checked" — read that before starting.)

6. ⬜ **URGENT, next after milestone 4 — make MPI reductions safe by construction** (agreed
   2026-08-01). Milestone 4 uncovered a class of silent wrong-answer bugs with a single root
   cause: the translator emits `LOCK_PARALLEL_DO` as the first statement *inside* a `parallel do`,
   and `DO_IN_PARALLEL` is false while that lock is held, so a `PARALLEL_SUM` written in the loop
   body is **dead code that looks correct**. Four such sites in `molecule.grid.foo` each returned
   `1/n_ranks` of the answer. The intent ("MPI on the outside", no interior collectives) is right
   and standard; the enforcement is invisible, so — per Dylan — "the programmer has to hold the
   call sequence in their head not to make bugs". Agreed fix, all four parts:
   - **`parallel do … reduce(x)`**, lowered by `FooToFortran` to emit the reduction after
     `UNLOCK_PARALLEL_DO` (i.e. OpenMP's `reduction(+:x)`). Removes the failure mode entirely.
   - **Abort on a suppressed reduction** under `USE_PRECONDITIONS` — do this first, it is two
     lines and independent of the grammar work.
   - **Depth-count the lock** so a recursive inner return cannot release an outer lock; restore
     the `ENSURE` at `parallel.foo:308`.
   - **Translator lint** for any `PARALLEL_*` macro lexically inside a `parallel do` body, and
     for any raw `write(`/`read(` on a `*.unit` expression outside `file.foo`/`textfile.foo`/
     `buffer.foo`. The second catches the raw-I/O class that crashed `DWGN_lamaGOET_NBO_file_47`
     -- and, more importantly, the *silent* variant of it: an unguarded write to a
     non-redirected stdout uses preconnected unit 6, valid on every rank, so it interleaves
     output instead of failing. Inspection alone cannot find those; a lint can.

   Sequenced *after* milestone 4's characterisation, because changing the lowering mid-flight
   would confound the numbers. Full design in `DEFERRED.md`, "MPI: defects found during
   milestone 4".

7. ⬜ **Diagnose and fix the `-O2`-only MPI undefined behaviour** (found 2026-08-02). Four
   CIF-reading tests (`c9o9h8_read_cif_IT_group_9`, `maleate_read_CIF_H_double_bond_{new,old}_BLs`,
   `urea_lamaGOET_grown_CIF`) abort at ≥2 ranks with a **mismatched `MPI_Bcast`** in the
   `-O2 -fno-fast-math` build, while the *same test on the same machine passes at `-Ofast`*, and
   none fail on macOS arm64. A collective mismatch that moves with optimisation level and platform
   is undefined behaviour. **`-Ofast` hides it**, so the shipped configuration is the one where it
   is invisible, not the one where it is absent — which is why this is a milestone, not a deferred
   note. Diagnosis in progress with a Linux `-O2 + -fcheck=bounds` build and an `-O0 + -finit-*`
   poisoning build. See `docs/MPI.md` Finding 6.

8. ⬜ **Translator: `data` statements at program scope are silently dropped.** Found while making
   `hart` work. The declaration still compiles and the variable is simply left uninitialised — a
   silently-wrong-answer bug with no diagnostic, in the same family as the MPI dead reductions.
   Currently worked around in three runfiles; **the translator is the real fix**, and until it is
   fixed any `data` statement anywhere in a program unit is a trap. Deserves a translator-level
   audit for other constructs that are parsed and then quietly discarded.

9. ⬜ **`write_archive` swallows the following keyword, and one test has never run its SCF.**
   `molecule.put.foo:674` tests `stdin.buffer.n_items==2`, but `n_items` counts the whole line
   *including* the keyword — and `write_archive density_mx` is already 2 items, so the optional
   third word (`normalise`) is always sought and the **next line's first word is eaten instead**.
   The correct test is `==3` (per Dylan: the third item is genuinely optional, since objects such
   as `density_mx` know their own genre). `MOLECULE.READ:read_archive` had the identical bug and
   was fixed during milestone 5; this is its twin.
   The consequence is the part that matters: `tests/long/nh3_x-ray-constrained-rhf-cluster-charge_cc-pVTZ_restart`
   eats its own `scf` keyword, runs in 40 ms, and its checked-in reference contains **zero lines
   mentioning "SCF"** in 635 lines — the test has never done the science its name claims. Blast
   radius is exactly that one job file (it is the only one in `tests/` using `write_archive`).
   Fixing the off-by-one will rewrite that reference wholesale, so **regenerate it and inspect it
   as science, not as a diff**. Sequenced after the MPI work.

**Open items** (future directions; details in `DEFERRED.md`)

- **Grammar still ACCEPTS the old submodule call forms** (`.SET:proc`, `.MAIN:proc`, `STR::proc`)
  even though they are now auto-resolved away in the sources; not tightened (harmless).
- **README/wiki reorganisation** (in progress, 2026-07-27) — split responsibilities: README =
  build + verify/test only; `docs/` = code-tracking dev references; wiki = user guides. Default
  build should be `release` (not `fast`); retire event-specific blocks to the wiki.
- Future tasks (own conversations): a module-level *call* graph in `writeDotFiles` (the
  `--simplify`/`--module` **use**-graph tooling is DONE — `scripts/simplify_callgraph.py`,
  `docs/CALL_GRAPHS.md`); introduce Fortran-2008 `submodule` constructs; test the MPI parallel
  build; boilerplate doc comments; and (long-term) a possible move off Fortran. (Testing the MPI build
  is now milestone 4 above.)

> Submodules ARE implemented (dotted headers + colon call forms parse & auto-resolve; commit
> `4cd995df`), and translator build/run commands are recorded in §8 — both former open items done.
