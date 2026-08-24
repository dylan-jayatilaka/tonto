# The gfortran-16 debug crash — handover

**Status 2026-08-24: not fixed.** Characterised well enough to hand over cold.
This page is self-contained; `DEFERRED.md` (§ "PARTLY DIAGNOSED (2026-08-24)")
carries the same findings woven into the longer record.

## The one-paragraph version

A **gfortran-16 debug** build of Tonto segfaults on any job that runs an SCF,
on **both** arm64 macOS and x86_64 Linux. gfortran-14 debug is fine, and
gfortran-16 **release** is fine on both platforms. The fault is a bad read in the
pointgroup machinery while building the promolecule initial guess — but the crash
site **differs between platforms**, and the construct will not reproduce in
isolation, which together point at *memory corruption laid down earlier* rather
than a logic error where it dies. **Chasing the crash site is chasing the
victim; the job is to find the write.**

## Reproduce it in two minutes

```bash
mkdir b && cd b
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-16 -DCMAKE_BUILD_TYPE=debug
make -j6                                  # ~20 min
mkdir -p /tmp/t && cd /tmp/t
cp <repo>/tests/short/h2o_rhf_STO-3G/{stdin,IO} .
TONTO_BASIS_SET_DIRECTORY=<repo>/basis_sets <repo>/b/tonto ; echo "exit=$?"
# expect 139
```

Control: the same with `gfortran-14` gives exit 0.

## What is established, and how

| Fact | How it was established |
|---|---|
| Only with an SCF | a CIF-processing job with no SCF is fine |
| gfortran-16 only | gfortran-14 debug, same commit, same flags, same machine: exit 0 |
| **Both platforms** | achari2 (Ubuntu 24.04 x86_64) segfaults identically. The old note said "only on arm64"; that was untested |
| Crash, Linux | `POINTGROUP:make_character_table`, `pointgroup.foo:1018`, at `i=1, n=1`, Oh pointgroup, order 48 |
| Crash, macOS | `MOLECULE.BASE:make_pg_image_of_shell`, `KERN_INVALID_ADDRESS at 0xd9` |
| Read side is sound | gdb: `ubound(.irrep(1).mx)` = (1,1,48), `ubound(.irrep(4).mx)` = (3,3,48) — both correct |
| Reached from | `scf → initialize_scf → get_initial_guess → make_promolecule_density_mx → make_anos → make_anos_for_atom` |

**The two crash sites are different procedures.** A deterministic logic error
would fault in the same place on both platforms. Two different innocent reads
dying in the same neighbourhood is what heap corruption looks like — the memory
layout decides which one dies first.

## Ruled out — do not spend time here again

| Hypothesis | How it died |
|---|---|
| `-mtune=native` in debug (absent from release, which uses `-mcpu=apple-m2`) | rebuilt with `-DTONTO_ARCH_FLAG=none`: still 139 |
| The `shell1quartet.F90` `-O2` pin | recompiled at `-O0` and relinked: still crashes (earlier note) |
| Stack exhaustion | still 139 with `ulimit -s 65520`, the hard ceiling, as well as the 8 MB default |
| Keyword-named components (`character`, `dimension` in `IRREP`) | renamed to `chi`/`dim`, rebuilt, rerun: still 139. Renamed anyway as hygiene (`fce350ca`) |
| The crash-site construct itself | two standalone reduced cases — an allocatable array of a derived type with an allocatable vector component, allocated in a loop then written, first bare and then nested inside an outer type so the access is `self%irrep(i)%chi(n)` exactly as in Tonto — **both run to exit 0 under gfortran-14 AND gfortran-16** |
| `VEC{OBJECT}` passing an unallocated allocatable | genuine UB, found by `-fcheck=all`, and **fixed** (`d8b94cbf`). The crash is unchanged |

## Traps that cost time — read before repeating

- **`-fcheck=all` does not catch this fault.** It catches an *earlier* violation
  during keyword reading, long before the SCF. That one is now fixed; the crash
  remains.
- **AddressSanitizer on arm64 macOS is useless here.** ASan is genuinely linked
  (`libasan.8.dylib`, 48 `__asan` symbols) but emits **no report** and turns the
  SIGSEGV into a SIGBUS. That is a failure to instrument, **not** a clean bill of
  health. Try it on Linux instead.
- **gdb cannot print the component formerly called `character`** — its Fortran
  parser takes the name for the keyword. (Now renamed to `chi`, so this is moot.)
- **macOS gives no line numbers.** `lldb` will not attach and `-fbacktrace`
  prints raw addresses. The only symbolication is macOS's own crash report,
  `~/Library/Logs/DiagnosticReports/tonto-*.ips` — a JSON payload after the first
  line. **Linux gives file and line for free.**
- **`dim` is reserved in Foo** as the array-size accessor. Renaming DIIS's
  `dimension` method to `dim` turned `d = .dim` into `d = size(self)` on a
  non-array and broke the build. IRREP's `dim` is safe only because it is a
  component on a scalar.

## Do this next, in this order

1. **Sanitizer on Linux**, on achari2. Conventional toolchain, ASan reliable,
   symbolic backtraces free. This is the one thing most likely to name the write.
   `valgrind` there is the fallback and would name it directly.
2. **Bisect the guess.** Change `initial_density=` away from `promolecule` and
   see whether the crash survives. Cheap, untried, and it establishes whether the
   damage predates `make_anos_for_atom` or is laid down inside it.
3. Only then go back to reading code.

## Working material

On this Mac (build trees are regenerable; `/tmp` is not durable):

| | |
|---|---|
| `build-gf16-debug/` | gfortran-16 debug — reproduces, exit 139 |
| `build-gf14-debug/` | gfortran-14 debug — the control, exit 0 |
| `build-macos/` | gfortran-14 release — for regression checks, `ctest -L short` |
| `build-gf16-notune/`, `build-gf16-check/`, `build-gf16-asan/` | the refuted experiments |

On achari2: `/tmp/tonto-gf16/{rel16,dbg16}` (gfortran-16 release and debug).
Note `/tmp` there is not durable either — rebuild from the branch.

Branch: **`macos-and-so2`**, pushed. Two fixes on it are independent of this
crash and worth landing regardless: the `plot_grid` purity fix (`97d8b0e9`,
which had `ci-debug.yml` red since 2026-08-23) and the `VEC{OBJECT}` allocatable
fix (`d8b94cbf`).
