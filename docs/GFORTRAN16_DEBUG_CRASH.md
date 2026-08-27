# The gfortran-16 debug crash — found

**Status 2026-08-27: root cause established, worked around, account requested upstream.**
`DEFERRED.md` carries the same finding woven into the longer record.

**The project migrated to gfortran-16 anyway, on 2026-08-27**, accepting a debug build with no
array bounds checking rather than waiting on a fix of unknown date — Tonto is almost entirely
dynamically allocated Fortran, so overruns are expected to be rare. See `CLAUDE.md` §4. This
document therefore describes a *live* condition of every debug build, not a historical one.

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

## Still open

- **Not reported to GCC**, and the blocker is an account, not the report. The draft is
  finished in `docs/GFORTRAN16_GCC_BUG.md`; Bugzilla sign-up is restricted and goes through
  <gcc-bugzilla-account-request@gcc.gnu.org>, where a request was sent **2026-08-27**. The
  duplicate search over *resolved* bugs and `16 Regression` is still outstanding and, like the
  filing, has to be done by hand — sourceware blocks scripted access.
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
