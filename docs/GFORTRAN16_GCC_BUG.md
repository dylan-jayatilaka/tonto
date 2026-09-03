# GCC bug report — `-fcheck=bounds` reads an uninitialised descriptor temporary

**FILED 2026-09-03 as GCC PR 127197** — <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=127197>
(product `gcc`, component `fortran`). Tonto's workaround and the background:
`docs/GFORTRAN16_DEBUG_CRASH.md`.

This page is now the **record of what was reported**, not a to-do. Keep it in step with the
Bugzilla thread: if upstream asks for more, add the answer here as well as there.

## The bisection, measured 2026-09-03 on sauce (x86_64, Ubuntu 24.04)

Every major compiler on the box, same source, `-O0 -g -fcheck=bounds`:

| compiler | result |
|---|---|
| 12.5.0 | OK |
| 13.4.0 | OK |
| 14.3.0 | OK |
| 15.2.0 | OK |
| 16.0.1 20260322 (experimental) [trunk `r16-8246-g569ace1fa50`] | **SIGSEGV** |

and gfortran-16 **without** `-fcheck=bounds` passes, so the check itself is the trigger. That
is what makes it a clean *16 regression* rather than "16 looks broken" — the draft had 15.x
down as untested, and both 15 and 16 turned out to be installed already.

**Method note.** Ubuntu ships gfortran 12 through 16 side by side, so this cost one loop and
about a minute. It was left undone for a week because nobody checked what was on the machine.
When a report needs a version range, run the range.

## The duplicate search (2026-08-25, and still worth repeating)

Bugzilla quicksearch `product:gcc component:fortran fcheck bounds` returns 22 open bugs; the
near ones are all *missing-diagnostic* reports — `-fcheck=bounds` failing to catch something
(44744, 82243, 111339, 34740, 39772, 81095) — or an ICE (83953). None is wrong code *generated
by* the check. Ruled out as the same bug: commit **r16-2249** *"fortran: Amend descriptor bounds
init if unallocated"* (PR fortran/108889), which looks close by title but is about suppressing
uninitialised warnings in reallocation-on-assignment and touches
`gfc_alloc_allocatable_for_assignment`, not the bounds-check path.

**Sourceware refuses scripted access** — plain `curl` gets a 429 and the Anubis anti-bot layer
blocks the rest, WebFetch included — so any further search has to be done by hand in a
logged-in browser.

Bugzilla fields as filed: **Product** gcc · **Component** fortran · **Severity** normal ·
**Keywords** `wrong-code` · **Known to fail** 16.0.1 · **Known to work** 12.5.0, 13.4.0,
14.3.0, 15.2.0.

---

## What was reported


**Summary:** `[16 Regression] -fcheck=bounds generates a check that reads an
uninitialised stack temporary, causing a segfault or a bogus bounds violation`

**Target:** `x86_64-linux-gnu` (reproduces); `aarch64-apple-darwin` (miscompiles
similar code in a larger program, but not the reduced case below)

**Versions:**

* fails: 16.0.1 20260315 (experimental) [trunk r16-8100-g3aca3bae8ee], Ubuntu 24.04
* fails: 16.1.0 (Homebrew) — in a larger program, on aarch64
* works: 14.2.0 (Ubuntu), 14.3.0 (Homebrew)
* 15.x not tested

## Description

At `-O0 -fcheck=bounds`, for an array reference reached through an allocatable
component chain, the generated bounds check reads the array descriptor from a
stack temporary that is only written *later in the same statement*. The check
therefore uses uninitialised memory. Depending on what is on the stack it either
faults on a nonsense address or reports a bounds violation that is not real.

Adding `-fcheck=bounds` should be able to add a diagnostic; it should never change
whether a conforming program runs.

## Reproducer

Attached as `gfortran_bounds_bug.f90` (also in the Tonto tree at
`scripts/gfortran_bounds_bug.f90`). The essential statement is

```fortran
self%irrep(i)%chi(n) = trace_(self%irrep(i)%mx(:,:,n))
```

where `self` is `type(POINTGROUP_TYPE), intent(inout)` holding an allocatable
array component `irrep(:)` whose element type has allocatable components `chi(:)`
and `mx(:,:,:)`.

```
$ gfortran-16 -O0 -g                  -o t gfortran_bounds_bug.f90 && ./t
 OK: chi(1,1) =   1.0000000000000000

$ gfortran-16 -O0 -g -fcheck=bounds   -o t gfortran_bounds_bug.f90 && ./t
Program received signal SIGSEGV: Segmentation fault - invalid memory reference.
#3  0x... in __pg_m_MOD_make_character_table
    at gfortran_bounds_bug.f90:66

$ gfortran-14 -O0 -g -fcheck=bounds   -o t gfortran_bounds_bug.f90 && ./t
 OK: chi(1,1) =   1.0000000000000000
```

## Generated code

x86_64, `-O0 -fcheck=bounds`, offsets from `%rbp`. The descriptor of `self%irrep`
is copied to `-0x100 … -0xc8`, and the check then reads `-0xc0`/`-0xb8`:

```
    ; copy SELF%IRREP descriptor into the temporary at -0x100
+609    mov  -0x108(%rbp),%rax          ; self
+616    mov  0x2f0(%rax),%rcx           ; irrep.base_addr
+623    mov  0x2f8(%rax),%rbx           ; irrep.offset
+630    mov  %rcx,-0x100(%rbp)
+637    mov  %rbx,-0xf8(%rbp)
 ...                                    ; dtype, span, bounds
+714    mov  %rax,-0xd0(%rbp)
+721    mov  %rdx,-0xc8(%rbp)

    ; bounds check for mx(:,:,n) -- but reading the NEXT temporary
+728    mov  -0x28(%rbp),%eax           ; n
+731    movslq %eax,%rdx
+734    mov  -0xc0(%rbp),%rsi           ; <-- uninitialised
+741    mov  -0xb8(%rbp),%rcx           ; <-- uninitialised
+748    mov  -0x24(%rbp),%eax           ; i
+753    add  %rax,%rcx
+756    imul $0xb8,%rcx,%rcx            ; sizeof(IRREP_TYPE)
+763    add  %rsi,%rcx
+766    add  $0xa8,%rcx                 ; mx descriptor, dim[2].lower_bound
+773    mov  (%rcx),%rcx                ; <-- SIGSEGV
+776    cmp  %rcx,%rdx
+779    jge  ...

    ; the temporary at -0xc0 is initialised only here
+1155   mov  -0x108(%rbp),%rdx
+1162   mov  0x2f0(%rdx),%rcx
+1169   mov  0x2f8(%rdx),%rbx
+1176   mov  %rcx,-0xc0(%rbp)
+1183   mov  %rbx,-0xb8(%rbp)
```

Two descriptor temporaries are created for the same statement. The first is
filled at `+630` and the second at `+1176`, but the check emitted at `+734` — well
before `+1176` — refers to the second. The preceding statement in the same
procedure (`call create_(self%irrep(i)%chi,self%order)`) is compiled correctly:
it writes its copy to `-0x80` and reads `-0x80`.

## Second symptom, same cause

Where the uninitialised slot happens to hold small values rather than an
unmapped address, the check does not fault; it succeeds in reading bounds of
`(0:0)` out of the garbage descriptor and reports a violation that is not there:

```
Fortran runtime error: Index '1' of dimension 2 of array 'self...%r'
outside of expected range (0:0)
```

This is arguably the worse symptom, since it looks like a genuine finding.

## Notes

* `-O0` only is enough; no optimisation is involved.
* Found in Tonto (<https://github.com/dylan-jayatilaka/tonto>), where it makes
  every gfortran-16 debug build crash on any SCF job, at several different
  statements of this shape. Release builds, which carry no `-fcheck`, are
  unaffected.
* On aarch64-apple-darwin the reduced case above is compiled correctly — no
  descriptor temporaries are created for it — while the same construct elsewhere
  in the larger program still crashes. The reduced case therefore demonstrates
  the bug on x86_64 only.
