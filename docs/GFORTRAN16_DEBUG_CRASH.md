# The gfortran-16 debug crash — found

**Status 2026-09-03: root cause established, worked around, and FILED upstream as GCC PR 127197**
(<https://gcc.gnu.org/bugzilla/show_bug.cgi?id=127197>).
`TASKS_AND_HISTORY.md` carries the same finding woven into the longer record.

**A migration to gfortran-16 was made and REVERTED on 2026-08-27.** It was made accepting a
debug build with no array bounds checking; it was reverted hours later when the local gate found
a *second*, worse defect — a gfortran-16 debug build fails **34 of 71** short tests where a
gfortran-14 debug build of the same code passes exactly. So the bounds-check bug described here
is **not the only reason** to avoid 16 for debug builds; see `TASKS_AND_HISTORY.md` for the second. The
migration is preserved on the branch `develop-gfortran-16`. Release builds on 16 remain fine.

## The one-paragraph version

A **gfortran-16 debug** build of Tonto segfaults on any job that runs an SCF, on
both arm64 macOS and x86_64 Linux, while gfortran-14 debug and gfortran-16
release are both fine. **The cause is a code-generation bug in gfortran 16's
`-fcheck=bounds` itself, not a defect in Tonto and not memory corruption.** For a
bounds-checked subscript reached through an allocatable component chain, gfortran
16 copies the array descriptor into one stack temporary and emits the check
reading *another* temporary that is only written later in the same statement. The
check therefore consults uninitialised stack memory: it either faults on a
nonsense address or reports a bounds violation that is not real. Tonto's build
now omits `-fcheck=bounds` on gfortran 16 and up.

## The mechanism

The statement that dies on Linux is `POINTGROUP:make_character_table`:

```fortran
self%irrep(i)%chi(n) = trace_(self%irrep(i)%mx(:,:,n))
```

At `-O0 -fcheck=bounds`, gfortran 16 emits (x86_64, offsets from `%rbp`):

```
+609 … +721   copy the SELF%IRREP descriptor  ->  -0x100 … -0xc8
+734          mov -0xc0(%rbp),%rsi      <-- reads the NEXT temporary, never written
+741          mov -0xb8(%rbp),%rcx
+773          mov (%rcx),%rcx           <-- SIGSEGV: address 0x22a0000038a
...
+1176         mov %rcx,-0xc0(%rbp)      <-- the only write, 400 bytes further on
```

The address at `+773` is `base + (offset + i)*184 + 168`, i.e. the third
dimension's lower bound of `irrep(i)%mx` — an ordinary bounds check. It faults
because `base` and `offset` were read from a temporary the compiler had not filled
in yet. The garbage picked up on the failing run was 1,1,2,3,3,1,1,2,3,3: the Oh
irrep dimensions, left on the stack by an earlier call.

Compare the *preceding* statement in the same procedure, `create_(…%chi,…)`,
which is compiled correctly: it writes its descriptor copy to `-0x80` and reads
`-0x80`.

## Evidence

| Fact | How it was established |
|---|---|
| The flag is the trigger | `-fcheck=bounds` is debug-only (`cmake/SetFortranFlags.cmake`). No check, no faulty temporary — which is why gfortran-16 **release** was always fine |
| Read before write | the only write to those two slots is at `+1176`; every use before it reads them uninitialised. Full disassembly of the procedure, not a sample |
| Reproducer | `scripts/gfortran_bounds_bug.f90`, 97 lines, no Tonto: gfortran-14 fine both ways, gfortran-16 fine at `-O0`, **SIGSEGV at `-O0 -fcheck=bounds`** on x86_64 |
| The two crash sites are two statements, not two victims | the macOS site, `MOLECULE.BASE:make_pg_image_of_shell`, contains `.pointgroup.mx(:,:,n)` — the identical construct: a bounds-checked variable subscript on an allocatable array component reached through an allocatable component |
| Confirmed in Tonto | recompiling **`pointgroup.F90` alone** without `-fcheck=bounds` and relinking removes the segfault. The run reaches "Making gaussian ANO data …" and then hits the next site of the same shape, `atom.F90:7058` (`self%NOs%r(:,n)`) |
| … and the next failure is the same bug wearing a different hat | there the garbage descriptor does not fault; it makes the check *report* `Index '1' of dimension 2 … outside of expected range (0:0)`. Bounds of `(0:0)` read out of an uninitialised descriptor |

## What this retires

Three entries in the old "ruled out" table were misread, and the conclusion drawn
from them — heap corruption — was wrong. Recorded so the reasoning is not
repeated:

- **"The two crash sites are different procedures, so it must be corruption."**
  They are two *statements* of the same shape, each independently miscompiled.
  Nothing was corrupt.
- **"The construct will not reproduce in isolation."** The reduced cases were
  built without the descriptor-temporary trigger. The construct reproduces
  perfectly once the bounds check is present *and* the target is x86_64.
- **"AddressSanitizer on macOS emits no report."** It never would. This is a
  stack-slot ordering error inside compiler-generated code, not a heap access
  ASan instruments.

Two entries stand: `-mtune=native` and the `shell1quartet.F90` `-O2` pin are
genuinely irrelevant. The `VEC{OBJECT}` unallocated-allocatable fix (`d8b94cbf`)
was a real conformance defect and worth landing, but was never related to this.

## What Tonto does about it

`cmake/SetFortranFlags.cmake` omits `-fcheck=bounds` from `DEBUG_FLAGS` when the
GNU Fortran version is 16 or newer, and says so at configure time. Everything
else about a debug build is unchanged: `-O0`, `-g`, `-fbacktrace`,
`USE_PRECONDITIONS`, the `ENSURE`/`WARN` machinery. Only the compiler's own array
bounds checking is lost, and only on 16.

```bash
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-16 -DCMAKE_BUILD_TYPE=debug
# -- gfortran 16.1.0: omitting -fcheck=bounds from DEBUG (compiler bug -- ...)
```

Re-enable with `-DTONTO_FORCE_FCHECK_BOUNDS=ON` to retest once GCC fixes it. Use
**gfortran-14 for debug work that needs bounds checking**; it is correct and
keeps the flag.

## Testing a compiler

```bash
scripts/check_gfortran_bounds_bug.py gfortran-16     # exit 1 if affected
```

It compiles the reproducer with and without the flag and runs both: correct
without and failing with is the bug, since a check may add a diagnostic but must
never change the answer.

**A pass is evidence, not proof.** What the bad code does depends on what happens
to be on the stack, and the reduced case only provokes the faulty temporary on
x86_64 — gfortran 16 on arm64 compiles *the reproducer* correctly while still
crashing Tonto at `make_pg_image_of_shell`. Trust a failure; do not use a pass to
clear a compiler whose debug builds are failing. Check the generated code by hand
instead, for the signature above.

## 16.1.0 is available on macOS, and a debug build still segfaults -- with the flag already off

Measured 2026-09-24 on arm64 macOS, Homebrew GCC 16.1.0 (`gfortran-16`), against gfortran-14 on the
same machine and the same job, `tests/short/h2o_rhf_cc-pVDZ` run with no command-line options:

| build | result |
|---|---|
| gfortran-14, debug | **clean, 0.54 s** |
| gfortran-16.1.0, debug, `-fcheck=bounds` **omitted** (the current default on 16) | **SIGSEGV, 0.11 s** |
| gfortran-16.1.0, debug, `-DTONTO_FORCE_FCHECK_BOUNDS=ON` | SIGSEGV |

**Two conclusions, and the second is the awkward one.**

1. **The reproducer passes at 16.1.0** (`scripts/check_gfortran_bounds_bug.py gfortran-16`: correct
   both with and without the flag). As this page already warns, a pass on arm64 is not proof --
   so that alone settles nothing.
2. **This crash is not the bounds bug.** The criterion on this page is "correct without the flag and
   failing with it"; here it fails *both* ways. So **dropping `-fcheck=bounds` does not make a
   gfortran-16 debug build usable**, and whatever is wrong is either a second compiler defect or a
   Tonto defect that only 16 exposes.

**Consequence for the move to 16.** `CLAUDE.md` §6 says to wait for Ubuntu to reach "16.1.0 or
later", on the reasoning that PR 127197's fix landed before 16.1.0. That threshold is necessary and
**not sufficient**: 16.1.0 is in hand on one platform and debug still does not work. The release
build remains unaffected -- it carries no `-fcheck` -- so the *release* half of the migration is
still as described.

**Not yet established, and needed before anyone rewrites the guidance:** the crash site. `atos`
misattributes it on this binary -- three distinct frames resolved to one line, in a routine the job
never reaches -- so the two runs gave inconsistent answers and neither is quoted here. This wants an
`lldb` session on the 16 debug binary, not symbol arithmetic. Also unknown: whether x86_64 behaves
the same, since the Linux box has only the pre-fix `16.0.1` PPA snapshot.

**Method note.** Reaching this took two false starts worth remembering. Passing `--input` to a debug
binary aborts instantly at `vec_str.F90:251` ("Different CHARACTER lengths (1024/256)") -- the
separate `--`-option defect -- so a debug job must rely on the default `stdin`/`stdout` in its
directory. And `tests/long/urea_rhf_STO-3G_HAR`, which `CLAUDE.md` §11 recommends as the quick debug
job, does **not** run clean in a gfortran-14 debug build here either: it dies after 6.5 s in the
error handler itself, "Attempt to DEALLOCATE unallocated 'tonto_bug'" at `system.F90:481`, which
masks whatever the real error was. Use a job with a known-clean baseline, or the comparison
establishes nothing.

## Still open

- **Reported to GCC as PR 127197** on 2026-09-03, with a five-version bisection (12, 13, 14
  and 15 all correct; 16.0.1 segfaults) — see `docs/GFORTRAN16_GCC_BUG.md`. Awaiting a
  maintainer. The duplicate search over *resolved* bugs and `16 Regression` remains
  outstanding and has to be done by hand: sourceware blocks scripted access.
- **gfortran 15 is untested** — neither machine has it. The gate is therefore on
  `>= 16`, which is what was measured. If 15 turns out to be affected, lower it.
- ~~A full gfortran-16 debug build has not been run end-to-end.~~ **Done
  2026-08-25, and it passes.** A whole-tree gfortran-16 debug build on arm64
  macOS compiles clean, runs `h2o_rhf_STO-3G` to **exit 0** (it gave 139 before)
  with `Total energy -74.9658`, and takes `ctest -L short` **62/62**.
- **Which arm64 construct makes the temporary.** The reduced case does not
  provoke it there, so the macOS crash site has not been examined at machine
  level. It is not needed for the fix, but it would make the upstream report
  cover both targets.

## Versions

| | |
|---|---|
| Affected | GCC 16.1.0 (Homebrew, arm64 macOS) and 16.0.1 20260315 trunk r16-8100 (Ubuntu 24.04, x86_64) |
| Not affected | GCC 14.2.0 (Ubuntu), 14.3.0 (Homebrew) |
| Untested | GCC 15 |
