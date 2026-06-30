# CLAUDE.md

Durable, project-wide context for Claude Code, read at the start of every session.
Stable facts only (build, test, layout, conventions). Per-task specs belong in a
separate document.

## 1. What this project is

**Tonto** is a quantum chemistry / crystallography package. Its scientific code is written
in **Foo**, a custom object-oriented preprocessor language that is translated to modern
Fortran (95 / 2003+) and then compiled.

- Foo sources live in `foofiles/` (`*.foo`). Maintainer: Dylan Jayatilaka.
- Legacy translator: `scripts/foo.pl` (Perl) — the reference behaviour to reproduce.
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

**Status** (per commit `7bbf20a4`, verify before trusting): the translator is "untested, not
working". Refinement work (g-Hirshfeld / g-Salvador) is reported working (`48e30ac2`,
`53d1bc96`).

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
- **Do not run the test jobs (`ctest`).**

> The exact commands to build/run the translator are not yet recorded here. The CMake build
> drives `foo.pl`, so `CMakeLists.txt` shows how generation is wired up — derive the
> equivalent invocation for `FooToFortran.java` and record it in §8 once confirmed.

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

- `FOO_GRAMMAR_DOCUMENTATION.md` — full language description and Foo→Fortran conversion rules.
- `FOO_GRAMMAR_VALIDATION.md` — worked `.foo` examples mapped to grammar rules.
- `FOO_QUICK_REFERENCE.md` — concise syntax cheat-sheet.
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
JAR=/usr/local/lib/antlr-4.13.2-complete.jar
( cd foogrammar && java -cp "$JAR" org.antlr.v4.Tool -visitor -o ../build/translator/gen Foo.g4 )
javac -cp "$JAR" -d build/translator/classes build/translator/gen/*.java foogrammar/FooToFortran.java
java -cp "$JAR:build/translator/classes" FooToFortran \
     --types foofiles/types.foo --foo foofiles/irrep.foo --out-dir antlr4-release
```

`FooToFortran` writes `<stem>.F90`, `<stem>.int`, `<stem>.use` (stem maps `vec{real}.foo`
→ `vec_real`). Compare against `release/` (whitespace-insensitive; the bar is equivalent,
not byte-exact). `types.foo` must be passed so the derived-type table is built first (§6).

## 9. Milestones & open items

**Milestones**

1. `foogrammar/Foo.g4` parses **every** file in `foofiles/` without error — including the
   submodule files (`molecule.*`, `diffraction_data.*`).
2. `foogrammar/FooToFortran.java` emits `.F90` / `.int` / `.use` into `antlr4-release/` that are
   **equivalent** (compilable, same behaviour) to the reference in `release/`.

> Confirm milestone wording — your note was cut off at "There are two milestones. release/".

**Open items**

- **Grammar does not yet implement submodules**, although the docs describe them (§3). The
  `moduleDef` rule is `MODULE IDENTIFIER`, but `IDENTIFIER` excludes `.`, so a dotted header
  like `module MOLECULE.BASE` will not parse; and the call rules (`callLike` / `path`) have no
  colon form, so `.SET:proc`, `.MAIN:proc`, `STR::proc` etc. are not recognised. This blocks
  milestone 1 and must be added.
- **Translator build/run commands** are not yet recorded — derive from `CMakeLists.txt` (which
  wires up `foo.pl`) and record the `FooToFortran.java` equivalent in §8.
