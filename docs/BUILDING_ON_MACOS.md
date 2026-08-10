# Building Tonto on macOS

The same toolchain as Linux, installed with [Homebrew](https://brew.sh) instead
of `apt`. Everything here is one pass from a clean machine to a tested binary.
Other platforms: [Linux](BUILDING_ON_LINUX.md),
[Windows/WSL](BUILDING_ON_WINDOWS.md).

Build types other than `release`, parallel (MPI) builds and clusters are common
to all platforms and live in [`BUILDING_TONTO.md`](BUILDING_TONTO.md).

---

## 1. Install the prerequisites, with Homebrew

First Apple's command-line tools (`git`, `make`, a C compiler):

```bash
xcode-select --install
```

Then the rest:

```bash
brew install gcc cmake openjdk python3 gnuplot
```

- `gcc` provides **`gfortran`**. Homebrew's `gcc` formula currently gives
  `gfortran-14`, which is the version this project standardises on.
- `openjdk` provides **`java`/`javac`** for the ANTLR4 `foo`→Fortran
  translator. If the build cannot find `javac`, add Homebrew's openjdk to your
  `PATH` as `brew` instructs — on Apple Silicon:
  ```bash
  echo 'export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"' >> ~/.zshrc
  ```
- **No BLAS or LAPACK to install** — macOS provides them through
  `Accelerate.framework`.
- **`gnuplot` is needed at *run* time, not build time**, to render the
  diagnostic plots a refinement writes. Without it the job still completes and
  the data files and gnuplot scripts are still written; you get a warning and
  no pictures.
- Optional parallel build: `brew install open-mpi` — but see the
  compiler-matching warning in [`BUILDING_TONTO.md`](BUILDING_TONTO.md). A
  Homebrew Open MPI built against a different gcc will not work.

## 2. Get the source code

```bash
git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
cd tonto
git checkout release        # the tested branch — recommended
```

`--recursive` pulls the submodules.

## 3. Configure and build

Tonto builds **out of source**: make a build directory, configure it once, then
`make`.

```bash
mkdir build && cd build
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release
make -j4
```

If `gfortran-14` is not on your `PATH` under that exact name, point CMake at
what Homebrew installed — `ls $(brew --prefix gcc)/bin/gfortran*` will show it.

> **About `-j`.** Translation runs one JVM per `.foo` file, which is
> memory-heavy. A bare `make -j` (unbounded) can thrash the machine — cap it:
> `make -j4`, and lower it first if a build stalls.

You now have **`build/tonto`** and **`build/hart`** (`hart --help`; see
[`RUNNING_HART.md`](RUNNING_HART.md)).

## 4. Run the tests

```bash
ctest -L short        # about a minute
ctest                 # the full suite
```

macOS shows tiny last-digit differences in a few tests. The comparison is
deliberately loose — relative difference ≤ 0.2%, or last printed digit within
2 — and counts those as passes.

## One macOS-specific oddity: the arm64 compiler pin

**arm64.** `shell1quartet.F90` is pinned to `-O2 -fno-schedule-insns` on arm64
macOS, working around a gfortran miscompilation of the two-electron integral
code. `CMakeLists.txt` explains it at the pin. Nothing to do; it is mentioned
so the odd flag in the build log is not a mystery.

---

## Where to go next

| | |
|---|---|
| Other build types (debug, fast, static), MPI, clusters | [`BUILDING_TONTO.md`](BUILDING_TONTO.md) |
| What a parallel build does and does not reproduce | [`TONTO_AND_MPI.md`](TONTO_AND_MPI.md) |
| Running Tonto | [`RUNNING_TONTO.md`](RUNNING_TONTO.md) |
| Source and executable layout | [`TONTO_LIBRARY_STRUCTURE.md`](TONTO_LIBRARY_STRUCTURE.md) |
