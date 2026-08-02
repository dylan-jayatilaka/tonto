# Tonto

[![CI (Linux-release)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci.yml/badge.svg)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci.yml)
[![CI (Linux-debug)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-debug.yml/badge.svg)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-debug.yml)
[![CI (WSL-release)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-wsl.yml/badge.svg)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-wsl.yml)
[![CI (WSL-debug)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-wsl-debug.yml/badge.svg)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-wsl-debug.yml)
[![CI (Linux-MPI)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-mpi.yml/badge.svg)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-mpi.yml)

Tonto is a quantum chemistry and crystallography package, with a focus on
Hirshfeld atom refinement, structure factor calculation, and
X-ray wavefunction refinement.

The code is written in **Foo** — an
object-oriented preprocessor language translated to modern Fortran.

**This README covers producing a `tonto` binary on Ubuntu/Linux**

Everything else lives elsewhere:

  - Compiling tonto on Mac and Linux, running Tonto, and science how-tos on the [**wiki**](https://github.com/dylan-jayatilaka/tonto/wiki)
  
  - Developer references in [**`docs/`**](docs/).

---

## 1. Prerequisites

On **Ubuntu/Debian Linux** (the assumed, best-supported platform):

```
sudo apt install make default-jdk gfortran libblas-dev liblapack-dev python3 gnuplot git
```

- `default-jdk` provides `java`/`javac` for the ANTLR4 `foo`→Fortran translator.
  The ANTLR jar itself is downloaded automatically on the first `cmake` run
  (internet needed for that one configure).
- `gnuplot` is for graphs; `python3` runs the test harness.
- Optional: `graphviz` (for the developer call-graphs), and for the parallel
  build: `sudo apt install openmpi-bin libopenmpi-dev`.

**macOS** → [Building on macOS](https://github.com/dylan-jayatilaka/tonto/wiki/Building-on-MacOS)
(via Homebrew; Linux/WSL is the reference platform). &nbsp;
**Windows** → untested natively; **WSL** is the easy path, and is covered by CI.
Follow [**`docs/BUILD_WSL.md`**](docs/BUILD_WSL.md) rather than the Linux steps
below: WSL adds four traps (a Windows JDK on the interop `PATH`, building on
`/mnt/c`, CRLF line endings, and the OOM killer) that `cmake` now detects and
explains. Run `scripts/wsl_doctor.sh` first.

## 2. Get the code

```
git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
cd tonto
git checkout release        # the tested branch — recommended
```

(`--recursive` pulls the submodules. To keep several branches side by side,
clone into a named folder: `git clone --recursive … tonto-release`.)

## 3. Build

Tonto builds **out of source**: make a build directory, configure it once with
`cmake`, then `make`. Pick a **build type** — this is the one real choice, and
`release` is the tested default. Copy-paste the recipe you want:

**Release** — optimised, tested, what CI runs. Use this unless you have a reason not to.
```
mkdir build && cd build
cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=release
make -j4
```

**Debug** — `-O0` with runtime checks and error messages; use it to diagnose a crash.
```
mkdir debug && cd debug
cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=debug
make -j4
```

**Fast** — aggressive optimisation; faster, but may perturb the last printed digits.
```
cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=fast
```

**Static** — a self-contained binary for redistribution (larger).
```
cmake .. -DCMAKE_BUILD_TYPE=release-static
```

**MPI (parallel)** — for production runs. **Validate the results yourself** before
trusting them: see `docs/MPI.md` for what is known to differ from a serial run.
```
cmake .. -DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_C_COMPILER=mpicc \
         -DCMAKE_BUILD_TYPE=fast -DMPI=1
```

Use `-DCMAKE_BUILD_TYPE=release` instead if you intend to **compare against the
reference outputs**: the references were blessed at `release`, and `fast` adds
`-faggressive-loop-optimizations -fstrict-aliasing` on top of `-Ofast`, so
differences from it are not evidence about MPI.

**The MPI must have been built with the same Fortran compiler you are using.**
Tonto does `USE mpi`, and Fortran `.mod` files are compiler-version specific, so
an MPI packaged against a different gcc fails with *"Cannot read module file …
created by a different version of GNU Fortran"*. Configure now checks this and
stops with a clear message rather than failing deep in the build. If they do not
match, build MPI against your compiler, e.g.

```
./configure --prefix=$HOME/opt/openmpi-gf14 FC=gfortran-14 CC=clang CXX=clang++
```

and put its `bin` first on `PATH`. Note also that `-DMPI=1` is now a hard
requirement: if MPI cannot be found, configure fails instead of quietly
producing a serial binary.

**On a cluster / supercomputer:** environments vary too much to script. Load
your compiler and MPI modules first, then use the recipe above, overriding the
compiler if needed:  `-DCMAKE_Fortran_COMPILER=<your ftn wrapper>`. The three
knobs that matter are the **compiler**, the **build type**, and **`-DMPI=1`**.

> **About `-j`.** Translation runs one JVM per `.foo` file, which is
> memory-heavy. A bare `make -j` (unbounded) can thrash the machine — cap it:
> `make -j4 -l8` (≤ 4 jobs, pause while load > 8). Lower `-j` first if a build stalls.

When it finishes, your binaries are in the build dir: **`build/tonto`** (the main
program) and **`build/hart`** (standalone Hirshfeld atom refinement — `hart --help`;
see [`docs/HART.md`](docs/HART.md) for the full option reference and how it is tested).
The full source/executable layout is on the
[wiki](https://github.com/dylan-jayatilaka/tonto/wiki).

> **Options are GNU long options.** Every Tonto program takes `--name` only —
> `tonto --input job.txt`, `hart --basis STO-3G`. The old single-dash spellings
> (`-i`, `-o`, `-b`, `-help`, `-basis`, …) were removed; a program that is given
> one now says which `--name` to use instead.

## 4. Verify — run the tests

From the build directory, the quickest check:

```
ctest
```

Two correct builds can differ in the last printed digits (compiler, BLAS, grid
ordering), so a bare `ctest` shows *pseudo-failures*. For a clear verdict, use
the **agreement report** instead:

```
make report        # runs every suite, writes a grouped table to tests.log
```

It reports each test under three criteria — **exact** (every digit identical),
**loose** (within 0.2 % *or* ±2 in the last digit — **this decides pass/fail**),
and **last-digit** — plus the worst deviations:

```
SUITE: short   (51 tests)
test                                    exact  loose  lastdig   max rel%    max ulp
h2o_rhf_STO-3G                          PASS   PASS   PASS             0          0
h2o_rhf_6-31G(d)_normal_mode_analysis   FAIL   PASS   FAIL        0.0017          3
...
short subtotal:  loose 51/51   (exact 48, lastdig 49)
```

Tolerances are options on `scripts/suite_report.py` / `scripts/test.py`
(`--rel-tol`, `--last-digit-tol`, `--abs-tol`); `--suites short rgbi` selects a
subset. To inspect one failure, compare the reference and `.bad` output in
`tests/<suite>/<job>/`:  `vimdiff stdout stdout.bad`.

**The CI badges** at the top link to GitHub Actions; open the latest run to see
this same agreement table on its summary page, and download the `tests.log`
artifact. Green means the short suite passed the loose gate.
[**`docs/CI.md`**](docs/CI.md) covers what each workflow runs, how to start one by
hand (`gh workflow run …` or the Actions tab), and how to read the result.

They are not all the same kind of badge:

- **CI (Linux-release)** is the one that must be green. It is the gate.
- **CI (Linux-debug)** carries four longstanding `-O0` floating-point/structural
  failures that are not code defects (see `DEFERRED.md`), so its suite step is
  informational.
- **CI (Linux-MPI)** does not run on every push — Ubuntu's Open MPI is built
  against gcc-13 while the project standardises on gfortran-14, and `USE mpi`
  makes `.mod` files compiler-version specific, so the workflow builds Open MPI
  from source and is therefore cached, scheduled weekly, and triggered only by
  MPI-relevant paths. Its gate is the π rank-invariance check
  (`scripts/check_mpi_pi.sh`), not the suite, whose step is `continue-on-error`
  while the defect register in `docs/MPI.md` still has open rows. So a red MPI
  badge means the build broke or π stopped being rank-count independent — both
  real.

## 5. Help, bugs, contributing

Email **dylan.jayatilaka@gmail.com** (I am slow to reply — you may have better
luck via people who know me).

- **Running Tonto, tutorials, workshops** → the [wiki](https://github.com/dylan-jayatilaka/tonto/wiki).
- **Developer references** (source layout, the ANTLR4 translator, call-graphs &
  dead-code tools, pushing to GitHub) → [`docs/`](docs/) — see
  [`docs/DEVELOPER.md`](docs/DEVELOPER.md) and [`docs/CALL_GRAPHS.md`](docs/CALL_GRAPHS.md).
