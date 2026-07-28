# Tonto

[![CI](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci.yml/badge.svg)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci.yml)

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
**Windows** → untested natively; **WSL** (then follow the Linux steps) is the
easy path. See [Building on Windows](https://github.com/dylan-jayatilaka/tonto/wiki/Building-on-Windows).

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

**MPI (parallel)** — for production runs. **Validate the results yourself** before trusting them.
```
cmake .. -DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_C_COMPILER=mpicc \
         -DCMAKE_CXX_COMPILER=mpicxx -DCMAKE_BUILD_TYPE=fast -DMPI=1
# add -DNO_ERROR_MANAGEMENT for extra speed
```

**On a cluster / supercomputer:** environments vary too much to script. Load
your compiler and MPI modules first, then use the recipe above, overriding the
compiler if needed:  `-DCMAKE_Fortran_COMPILER=<your ftn wrapper>`. The three
knobs that matter are the **compiler**, the **build type**, and **`-DMPI=1`**.

> **About `-j`.** Translation runs one JVM per `.foo` file, which is
> memory-heavy. A bare `make -j` (unbounded) can thrash the machine — cap it:
> `make -j4 -l8` (≤ 4 jobs, pause while load > 8). Lower `-j` first if a build stalls.

When it finishes, your binaries are in the build dir: **`build/tonto`** (the main
program) and **`build/hart`** (standalone Hirshfeld atom refinement — `hart -help`).
The full source/executable layout is on the
[wiki](https://github.com/dylan-jayatilaka/tonto/wiki).

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

**The CI badge** at the top links to GitHub Actions; open the latest run to see
this same agreement table on its summary page, and download the `tests.log`
artifact. Green means the short suite passed the loose gate.

## 5. Help, bugs, contributing

Email **dylan.jayatilaka@gmail.com** (I am slow to reply — you may have better
luck via people who know me).

- **Running Tonto, tutorials, workshops** → the [wiki](https://github.com/dylan-jayatilaka/tonto/wiki).
- **Developer references** (source layout, the ANTLR4 translator, call-graphs &
  dead-code tools, pushing to GitHub) → [`docs/`](docs/) — see
  [`docs/DEVELOPER.md`](docs/DEVELOPER.md) and [`docs/CALL_GRAPHS.md`](docs/CALL_GRAPHS.md).
