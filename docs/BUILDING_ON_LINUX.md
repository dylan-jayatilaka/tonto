# Building Tonto on Linux

Ubuntu/Debian, the assumed and best-supported platform. Everything here is one
pass from a clean machine to a tested binary. Other platforms:
[macOS](BUILDING_ON_MACOS.md), [Windows/WSL](BUILDING_ON_WINDOWS.md).

Build types other than `release`, parallel (MPI) builds and clusters are common
to all platforms and live in [`BUILDING_TONTO.md`](BUILDING_TONTO.md).

---

## 1. Install the prerequisites

```bash
sudo apt install make cmake default-jdk gfortran-14 libblas-dev liblapack-dev \
                 python3 gnuplot git
```

- **`gfortran-14`, not the distro default `gfortran`.** Ubuntu 24.04's plain
  `gfortran` is version 13. The project standardises on **14**: that is what CI
  builds with, what the reference outputs in `tests/` were blessed with, and —
  because Tonto does `USE mpi` and Fortran `.mod` files are compiler-version
  specific — what any MPI you use must also have been built with. Version 13
  will generally compile, but do not report a numeric difference against the
  references without first checking on 14.
- `default-jdk` provides `java`/`javac` for the ANTLR4 `foo`→Fortran translator.
  The ANTLR jar itself is downloaded automatically on the first `cmake` run
  (internet needed for that one configure).
- **`gnuplot` (6.0 or later) is needed at *run* time, not build time.** Tonto
  runs it for you: at the end of a refinement it writes each diagnostic plot as
  a data file *and* a gnuplot script, then invokes `gnuplot` to render a `.png`.
  Without it the job still completes and both the data and the scripts are
  still written; you get a warning naming the command to run by hand, and no
  pictures.
- `python3` runs the test harness.
- Optional: `graphviz` for the developer call-graphs;
  `openmpi-bin libopenmpi-dev` for a parallel build.

## 2. Get the source code

```bash
git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
cd tonto
git checkout release        # the tested branch — recommended
```

`--recursive` pulls the submodules. To keep several branches side by side,
clone into a named folder: `git clone --recursive … tonto-release`.

## 3. Configure and build

Tonto builds **out of source**: make a build directory, configure it once, then
`make`.

```bash
mkdir build && cd build
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release
make -j4
```

> **About `-j`.** Translation runs one JVM per `.foo` file, which is
> memory-heavy. A bare `make -j` (unbounded) can thrash the machine — cap it:
> `make -j4 -l8` (at most 4 jobs, pause while load > 8). Lower `-j` first if a
> build stalls.

You now have **`build/tonto`** (the main program) and **`build/hart`**
(standalone Hirshfeld atom refinement — `hart --help`; see
[`RUNNING_HART.md`](RUNNING_HART.md)).

## 4. Run the tests

```bash
ctest -L short        # about a minute
ctest                 # the full suite
```

The comparison is deliberately loose — relative difference ≤ 0.2%, or last
printed digit within 2 — because the references were blessed on one compiler
and one machine.

---

## Where to go next

| | |
|---|---|
| Other build types (debug, fast, static), MPI, clusters | [`BUILDING_TONTO.md`](BUILDING_TONTO.md) |
| What a parallel build does and does not reproduce | [`TONTO_AND_MPI.md`](TONTO_AND_MPI.md) |
| Running Tonto | [`RUNNING_TONTO.md`](RUNNING_TONTO.md) |
| Source and executable layout | [`TONTO_LIBRARY_STRUCTURE.md`](TONTO_LIBRARY_STRUCTURE.md) |
