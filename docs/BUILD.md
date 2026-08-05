# Building Tonto

Everything needed to produce a working `tonto` (and `hart`) binary, on all three
supported platforms. This page is the single source of truth for building — the
README only carries a three-command quickstart and points here.

Related: [`BUILD_WSL.md`](BUILD_WSL.md) for the WSL-specific traps and how they
are guarded, [`MPI.md`](MPI.md) for what a parallel build does and does not
reproduce, [`CI.md`](CI.md) for what the automated builds do.

---

## 1. Prerequisites

On **Ubuntu/Debian Linux** (the assumed, best-supported platform):

```
sudo apt install make default-jdk gfortran-14 libblas-dev liblapack-dev python3 gnuplot git
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
- `gnuplot` is for graphs; `python3` runs the test harness.
- Optional: `graphviz` (for the developer call-graphs), and for the parallel
  build: `sudo apt install openmpi-bin libopenmpi-dev`.

**macOS** → §4 below (via Homebrew). **Windows** → §5 below (via WSL2).

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
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release
make -j4
```

**Debug** — `-O0` with runtime checks and error messages; use it to diagnose a crash.
```
mkdir debug && cd debug
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=debug
make -j4
```

**Fast** — aggressive optimisation; faster, but may perturb the last printed digits.
```
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=fast
```

**Static** — a self-contained binary for redistribution (larger).
```
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release-static
```

**MPI (parallel)** — for production runs. **Validate the results yourself** before
trusting them: see [`MPI.md`](MPI.md) for what is known to differ from a serial run.
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
./configure --prefix=$HOME/opt/openmpi-gf14 FC=gfortran-14 CC=gcc-14
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
see [`docs/HART.md`](HART.md) for the full option reference and how it is tested).
The full source/executable layout is in [`LAYOUT.md`](LAYOUT.md).

> **Options are GNU long options.** Every Tonto program takes `--name` only —
> `tonto --input job.txt`, `hart --basis STO-3G`. The old single-dash spellings
> (`-i`, `-o`, `-b`, `-help`, `-basis`, …) were removed; a program that is given
> one now says which `--name` to use instead.


---

## 4. macOS

macOS needs the same toolchain, installed with [Homebrew](https://brew.sh)
instead of `apt`. Once the tools are present, **clone, build and test are
identical to Linux** — use §2 and §3 above.

First Apple's command-line tools (`git`, `make`, a C compiler):

```
xcode-select --install
```

Then the rest:

```
brew install gcc cmake openjdk python3 gnuplot
```

- `gcc` provides **`gfortran`** (Homebrew's `gcc` formula currently gives
  `gfortran-14`, which is the version this project standardises on).
- `openjdk` provides **`java`/`javac`** for the ANTLR4 translator. If the build
  cannot find `javac`, add Homebrew's openjdk to your `PATH` as it instructs —
  on Apple Silicon:
  `echo 'export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"' >> ~/.zshrc`
- **No BLAS/LAPACK to install** — macOS provides them via `Accelerate.framework`.
- Optional parallel build: `brew install open-mpi`, but see the compiler-matching
  warning in §3 — a Homebrew Open MPI built against a different gcc will not work.

macOS shows tiny last-digit differences in a few tests. The loose criterion (see the README quickstart) counts those as passes.

> **arm64 note.** `shell1quartet.F90` is pinned to `-O2 -fno-schedule-insns` on
> arm64 macOS, working around a gfortran miscompilation of the two-electron
> integral code. `CMakeLists.txt` explains it at the pin.

## 5. Windows

Use **WSL2** (Windows Subsystem for Linux) — a real Ubuntu, then follow §1–§3
unchanged. Native Windows builds (MinGW/MSYS2, cross-compilation) are **not
tested** with the ANTLR4 translator.

1. In **PowerShell as Administrator**:
   ```
   wsl --install
   ```
   Reboot if prompted, launch **Ubuntu** from the Start menu, set a
   username/password. (Microsoft's
   [WSL install guide](https://learn.microsoft.com/windows/wsl/install).)

2. In the Ubuntu shell, follow §1–§3 exactly as for Linux.

**Keep the repository inside the Linux filesystem** (e.g. `~/tonto`), *not* under
`/mnt/c/...` — builds there are far slower, and `cmake` will refuse. Edit from
Windows via VS Code's WSL remote or the `\\wsl$\` path.

WSL adds four traps — a Windows `java.exe` on the interop `PATH`, a build tree on
`/mnt/c`, CRLF line endings, and the OOM killer arriving because translation
starts one JVM per `.foo` file. `cmake/WSL.cmake` detects all four and explains
them; run `scripts/wsl_doctor.sh` first. Full detail in
[`BUILD_WSL.md`](BUILD_WSL.md).
