# Building Tonto

**Go straight to your platform.** Each of the three is self-contained: from a
clean machine to a tested `tonto` and `hart`, without reading the other two.

| Platform | |
|---|---|
| **Linux** (Ubuntu/Debian) | [`BUILDING_ON_LINUX.md`](BUILDING_ON_LINUX.md) |
| **macOS** (Homebrew) | [`BUILDING_ON_MACOS.md`](BUILDING_ON_MACOS.md) |
| **Windows** (WSL2) | [`BUILDING_ON_WINDOWS.md`](BUILDING_ON_WINDOWS.md) |

What follows applies to all three, and none of it is needed for an ordinary
`release` build.

---

## Build types: release, debug, fast and static

Tonto builds **out of source**, and the build type is the one real choice.
Configure a separate directory for each type you want to keep.

| Type | What it is for |
|---|---|
| `release` | Optimised and tested; what CI runs and what the reference outputs were blessed with. **Use this unless you have a reason not to.** |
| `debug` | `-O0`, runtime checks, error messages. Use it to diagnose a crash. |
| `fast` | Aggressive optimisation. Faster, but may perturb the last printed digits. |
| `release-static` | A self-contained binary for redistribution. Larger. |

```bash
mkdir debug && cd debug
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=debug
make -j4
```

Substitute any type above for `debug`, in its own directory.

## Parallel (MPI) builds, on any platform with an MPI library

Parallel is a build option rather than a platform: the flags below apply
wherever an MPI is installed. It is tested, and in CI, on **Linux**; on WSL and
macOS it is expected to work but is **untested**.

**Validate parallel results before trusting them** —
[`TONTO_AND_MPI.md`](TONTO_AND_MPI.md) records what a parallel run does and does
not reproduce.

```bash
cmake .. -DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_C_COMPILER=mpicc \
         -DCMAKE_BUILD_TYPE=fast -DMPI=1
```

Use `-DCMAKE_BUILD_TYPE=release` instead if you intend to **compare against the
reference outputs**: they were blessed at `release`, and `fast` adds
`-faggressive-loop-optimizations -fstrict-aliasing` on top of `-Ofast`, so a
difference from it is not evidence about MPI.

**The MPI must have been built with the same Fortran compiler you are using.**
Tonto does `USE mpi`, and Fortran `.mod` files are compiler-version specific, so
an MPI packaged against a different gcc fails with *"Cannot read module file …
created by a different version of GNU Fortran"*. Configure checks this and stops
with a clear message rather than failing deep in the build. If they do not
match, build MPI against your compiler:

```bash
./configure --prefix=$HOME/opt/openmpi-gf14 FC=gfortran-14 CC=gcc-14
```

and put its `bin` first on `PATH`. `-DMPI=1` is a hard requirement: if MPI
cannot be found, configure fails rather than quietly producing a serial binary.

**On a cluster or supercomputer**, environments vary too much to script. Load
your compiler and MPI modules first, then use the recipe above, overriding the
compiler if needed with `-DCMAKE_Fortran_COMPILER=<your ftn wrapper>`. The three
knobs that matter are the **compiler**, the **build type** and **`-DMPI=1`**.

## What the build produces, and where it lands

**`build/tonto`** (the main program) and **`build/hart`** (standalone Hirshfeld
atom refinement — `hart --help`; see [`RUNNING_HART.md`](RUNNING_HART.md) for
the option reference and how it is tested). The full source and executable
layout is in [`TONTO_LIBRARY_STRUCTURE.md`](TONTO_LIBRARY_STRUCTURE.md).

> **Options are GNU long options.** Every Tonto program takes `--name` only —
> `tonto --input job.txt`, `hart --basis STO-3G`. The old single-dash spellings
> (`-i`, `-o`, `-b`, `-help`, `-basis`, …) were removed; a program given one now
> says which `--name` to use instead.
