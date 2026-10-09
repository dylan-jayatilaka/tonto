# Task: each build type a named intent (Reproducible builds, stage 1)

Working document: plan and log for stage 1 of the register item *Reproducible builds: the four
build intents, and a pinned reference platform*. Stage 2 (a pinned reference image) is not
planned here; it waits until a re-bless has to happen away from achari2 and the CI runner.

## 1. The goal

A user should answer one question, *which of these am I?*, and not three separate ones about
architecture tuning, fast-math and the BLAS. Each build type sets all three, and says what it
chose when you configure.

| Build type | Purpose | Optimisation | Architecture | BLAS / LAPACK |
|---|---|---|---|---|
| `debug` | find bugs | no optimisation, `-g`, bounds checks, `ENSURE`/`WARN` live | generic | whatever is found |
| `release` (default) | portable and fast | `-O3` | generic | system BLAS (whatever is found) |
| `fast` | all-out speed on this machine | `-Ofast` plus extras | native | OpenBLAS |
| `reference` | reproducible, for blessing and CI | `-O3 -fno-fast-math` | generic | netlib, required |

**MPI is a second axis, not a fifth row.** `-DMPI=1` goes with any of the four, and the
documentation shows it as something you add, not a separate type.

**An explicit setting wins**, except in `reference`. A build type only sets *defaults*. If you
pass `-DTONTO_ARCH_FLAG=...` or `-DBLA_VENDOR=...` yourself, that is used, and the configure summary
says so. A `reference` build stops instead, because it would no longer be one.

`release` and `reference` compile identical code (findings 11-13); they differ only in how strictly
the BLAS and the architecture are chosen.

`release-static` stays as it is: the `release` intent, linked statically against the bundled
LAPACK, for the published binaries.

## 2. What exists today

Read from `cmake/SetFortranFlags.cmake` and `CMakeLists.txt`.

| Type | gfortran flags | Arch | BLAS | What the cache says |
|---|---|---|---|---|
| `debug` | `-Wall -g -fbacktrace -fcheck=bounds -DUSE_PRECONDITIONS` | per `TONTO_ARCH_FLAG` (default none) | found | `DEBUG` |
| `release` | `-Ofast` | same | found; OpenBLAS preferred on macOS | `RELEASE` |
| `reference` | `-O2 -fno-fast-math` | same | found; OpenBLAS preferred on macOS | **`RELEASE`** (wrong) |
| `fast` | `-Ofast -faggressive-loop-optimizations -fstrict-aliasing` | same | found | **`TESTING`** (wrong) |
| `testing` | debug flags | same | found | `TESTING` |
| `release-static` | `-Ofast -static`, bundled LAPACK forced | same | bundled 3.8.0 | `RELEASE` |

Findings that shape the plan:

1. **The macOS CI `reference` builds link Accelerate's LAPACK 3.2.1** (2009). The runner has no
   Homebrew OpenBLAS, so `CMakeLists.txt` falls back to Accelerate with a warning; the
   2026-10-06 `ci-macos.yml` log shows `Provenance: compiler GNU 14.4.0, LAPACK 3.2.1`.
   Accelerate is the library that flipped `h2o_rhf_6-31G(d)_normal_mode_analysis` on its own.
   A `reference` build must refuse this instead of warning.
2. **On this Mac, `reference` links Homebrew OpenBLAS**, kernel pinned to `ARMV8` by
   `scripts/test.py` and `tests/CMakeLists.txt`. Homebrew's netlib `lapack` 3.12.1 is also
   installed here (keg-only, `/opt/homebrew/opt/lapack`).
3. **Linux `reference` already links netlib** (Ubuntu `libblas-dev`/`liblapack-dev` 3.12.0), on
   achari2 and on the `ubuntu-24.04` runner. So on Linux the stored references are already
   netlib, and nothing in this task should change a Linux `reference` build's flags or libraries.
4. **The cache records `RELEASE` for a `reference` tree and `TESTING` for a `fast` tree**
   (`SetFortranFlags.cmake`, the `REFERENCE` and `FAST` branches). Anyone checking
   `CMakeCache.txt` to see what they built is misled.
5. **Pitfall for the fix to finding 4.** The arm64 workaround `-fno-schedule-insns` is appended
   to `CMAKE_Fortran_FLAGS_<CONFIG>` for a fixed list of configs, so that it lands after `-O2`.
   `REFERENCE` is not in that list. Today that is harmless because the cache says `RELEASE`; the
   moment it says `REFERENCE`, an arm64 reference build would lose the workaround. Add
   `REFERENCE` and `FAST` to the list in the same change.
6. **BLAS is chosen before the flags.** `find_package(LAPACK)` runs at `CMakeLists.txt` ~line 150,
   `include(SetFortranFlags)` at ~277. The intent has to be read from `CMAKE_BUILD_TYPE` early,
   before the BLAS search.
7. **`fast` does not imply native tuning or OpenBLAS.** It only adds two flags to `-Ofast`.
8. **`testing` is used nowhere** (no workflow, script or page). It is `debug` under another name.
9. **`fourier_sums.F90` is compiled `-O3 -fno-fast-math` in every non-debug build,** `reference`
   included. That is IEEE-safe, and the stored references were made with it, so it stays; the
   documentation should not claim `reference` is `-O2` for every file. (Likewise the arm64
   `shell1quartet.F90` pin and the `textfile.F90` `-fno-optimize-sibling-calls`.)
10. **The building pages are wrong about CI.** `docs/BUILDING_ON_LINUX.md` and
    `docs/BUILDING_ON_MACOS.md` say `release` is "what CI runs and what the reference outputs were
    blessed with". Every comparing workflow builds `reference`, and the blessing page says so.
11. **No `release` or `reference` build compiles with the flags it names.** CMake's own default
    `CMAKE_Fortran_FLAGS_RELEASE` (`-O3`, plus `-DNDEBUG -O3` on Linux) is appended after ours,
    because the cache says `RELEASE`, and with gcc the last `-O` wins. A later `-O3` also undoes
    everything `-Ofast` adds except `-fno-math-errno` (checked with `-cpp -dM`: `__FAST_MATH__`
    disappears, and a sum that `-Ofast` reassociates comes out as at `-O3`). So `release` is in
    effect `-O3` without fast-math, and `reference` is `-O3 -fno-fast-math`. Only `fast` is a true
    `-Ofast`, because its cache entry says `TESTING`, which has no CMake default flags.
12. **A reconfigure turns a `reference` tree into a `release` tree.** The `REFERENCE` branch
    writes `RELEASE` into the cache with `FORCE`, so when `make` re-runs CMake the tree takes the
    `RELEASE` branch. achari2's bless tree `~/github/tonto-rebless/reference` has base flags
    `-Ofast` for this reason.
13. **Findings 11 and 12 cancel, so the references are sound.** The assembly gcc generates for
    `vec_real.F90`, `shell2.F90` and `mat_real.F90` is identical under achari2's line
    (`-Ofast ... -O3 -DNDEBUG -O3`) and a fresh `reference` line (`-O2 -fno-fast-math ... -O3`), and
    differs from both a true `-Ofast` and a true `-O2`. Every stored reference was therefore blessed
    at an effective `-O3`, fast-math off, and `release` today builds the same code.
14. **The `-O2` control build of `docs/TASK_MPI.md`** (the `textfile.F90`
    `-foptimize-sibling-calls` pin) cannot have been a CMake `reference` tree, which was `-O3`; it
    must have been flags passed by hand. Nothing to change, but the comment in `CMakeLists.txt`
    calls it "the -O2 -fno-fast-math control build" as if it were `reference`.

## 3. The change

### 3.1 CMake

- **Read the intent once, early.** At the top of the BLAS section of `CMakeLists.txt`, uppercase
  `CMAKE_BUILD_TYPE` into one variable (`TONTO_INTENT`, defaulting to `RELEASE`), and use it both
  there and in `SetFortranFlags.cmake`, instead of the two separate `toupper`s (`TEMP` and `BT`).
- **Defaults per intent, applied only when not set by the user:**
  - `fast`: `TONTO_ARCH_FLAG=auto`; `BLA_VENDOR=OpenBLAS`. If OpenBLAS is not found, stop with a
    message naming the package to install (`libopenblas-dev`, `brew install openblas`), or say how to
    build `fast` without it (`-DBLA_VENDOR=Generic`).
  - `reference`: `TONTO_ARCH_FLAG=none`; netlib. On macOS add `/opt/homebrew/opt/lapack` (or
    `/usr/local/opt/lapack`) to `CMAKE_PREFIX_PATH` and search `BLA_VENDOR=Generic` there, so neither
    OpenBLAS nor `/usr/lib/libblas.dylib` (Accelerate) is picked up.
  - `release`, `debug`: unchanged (generic arch; on macOS OpenBLAS preferred over Accelerate).
- **`reference` refuses a library that is not netlib.** After the search, if the linked
  libraries name `openblas`, `Accelerate`, `mkl` or `veclib`, stop with a message. A reference
  build that quietly is not one is the failure this task exists to remove.
- **Configure prints one summary line**, e.g.
  `Build type reference: -O2 -fno-fast-math, no arch tuning, LAPACK 3.12.1 (netlib, /opt/homebrew/opt/lapack)`,
  with `(set by you)` after any part the user overrode.
- **The cache keeps the type as typed**, with no `FORCE` rewrite, so a reconfigure rebuilds the
  same intent (finding 12). `CMAKE_Fortran_FLAGS_<intent>` is set to the workaround flags alone,
  which both removes CMake's default `-O3` (finding 11) and keeps the workaround after every `-O`
  (finding 5).
- **Remove `testing`.** It is unused; the error message lists the remaining types.
- Fix the help strings on the `CMAKE_BUILD_TYPE` cache entries, which list different choices in
  each branch.

### 3.2 CI

- `ci-macos.yml`, `ci-full-suite-macos.yml` and `ci-macos-mpi.yml` build `reference`; add
  `brew install lapack` to their install step. With the new check they would otherwise stop at
  configure, which is the right outcome.
- No Linux workflow changes. The Linux `reference` build must come out with **identical
  `flags.make` and the same linked libraries** before and after.

### 3.3 Documentation (the larger half)

- `docs/BUILDING_ON_LINUX.md`, `docs/BUILDING_ON_MACOS.md`, `docs/BUILDING_ON_WINDOWS.md`: replace
  the *Other build types* table with the four intents (one line each: who it is for, what it
  sets), a line saying MPI is added to any of them, and a line saying explicit `-D` settings win.
  Fix the claim that CI runs `release` (finding 10).
- `README.md`: a short version of the table where building is introduced, linking to the
  building pages.
- `docs/TONTO_BLESSING_TESTS.md`: the `reference` section states that it requires netlib and
  stops otherwise, and how to get netlib on macOS.
- `docs/BUILDING_WITH_MPI.md`: already says MPI goes with any build type; check it matches.
- `CLAUDE.md` §6: the list of build types.

## 4. Verification

On this Mac, each step in its own build tree, one at a time:

1. Configure all four types; check `CMakeCache.txt` (`CMAKE_BUILD_TYPE`), `flags.make` for one
   ordinary file and for `shell1quartet.F90`, and the linked LAPACK in the configure log.
2. `reference` with OpenBLAS forced (`-DBLA_VENDOR=OpenBLAS`): configure must stop.
3. Build `reference` (netlib 3.12.1) and run `ctest -L short`, then `short long hart`. The
   comparison is against the Linux references, so this is the first time the Mac runs the suite
   on the same kind of library as the references. Record the score with its suites.

On achari2, in a worktree:

4. Configure `reference` before and after the change; `diff` the two `flags.make` trees and the
   linked libraries. They must be identical, so no re-bless is needed there.

In CI:

5. Dispatch `macOS-release` on the branch and read its `Build type` line: it must name
   `/opt/homebrew/opt/lapack`, not Accelerate's 3.2.1.

## 5. What this moves

- **Linux references: nothing.** The generated code and the libraries are unchanged.
- **macOS CI results**: from Accelerate 3.2.1 to netlib 3.12.0, the version the references were blessed with. The stored references are
  Linux-blessed, so there is nothing macOS-specific to re-bless; but tests that passed on the Mac
  by luck or failed by Accelerate may change status. That is the useful outcome.
- **`fast` users**: they get native tuning and OpenBLAS, and different last digits, which is what
  `fast` says it is for.

## 6. Decisions (Dylan, 2026-10-09)

1. `reference` links the **system's netlib** (Ubuntu 3.12.0; Homebrew `lapack`, whose library also
   reports 3.12.0), not the bundled 3.8.0. No Linux reference moves.
2. `fast` without OpenBLAS **stops**, naming the package; `-DBLA_VENDOR=Generic` overrides.
3. `testing` is **removed**.
4. The build type is **stamped in the run banner**, as a separate commit.
5. `reference` is **`-O3 -fno-fast-math`**, which is what every stored reference was in effect
   blessed with (finding 13), not a true `-O2`. No re-bless.
6. `release` stays **`-O3`**, fast-math off, as it has in effect always been; `-Ofast` stays in
   `fast`.
7. OpenBLAS is **not** installed on achari2: it would become the system default BLAS there and
   switch every existing binary to it at run time.

## 7. Log

- 2026-10-09: plan written. Findings 1-10 from reading the CMake files, the workflows and the
  `ci-macos.yml` log of 2026-10-06.
- 2026-10-09: findings 11-14. A trailing CMake `-O3` cancelled both `-Ofast` and `-O2`; a reconfigure
  turned a `reference` tree into `release` (reproduced: one `cmake .` on a fresh reference tree gives
  base flags `-Ofast`). Assembly comparison, old line against new, for `vec_real`, `shell2`,
  `mat_real`, `molecule.scf` and `scf_data` on the Mac, and `vec_real`, `shell2`, `mat_real`,
  `molecule.scf` and `shell1quartet` on achari2 (bless-tree line, CI line, new `reference`, new
  `release`): identical in every case.
- 2026-10-09: implemented (sections 3.1-3.3). Configured on the Mac: all five types, cache keeps the
  type as typed, a reconfigure keeps `reference`; `reference` links `/opt/homebrew/opt/lapack`
  (3.12.0); refusals fire for `reference` with `BLA_VENDOR=OpenBLAS`, with `TONTO_ARCH_FLAG=auto`, and
  for `testing`. On achari2 (worktree `~/github/tonto-intents`): `reference` and `release` link
  `/usr/lib/x86_64-linux-gnu/lib{lapack,blas}.so` (netlib, 3.12.0); `fast` stops, no OpenBLAS there.
  Not yet done: a build and test run on the Mac's new netlib `reference`; CI on the branch.
