# Draft GCC bug report — `-fcheck=bounds` reads an uninitialised descriptor temporary

Not yet filed. Check <https://gcc.gnu.org/bugzilla/> for an existing report
first; search terms: *fcheck bounds descriptor temporary uninitialised
allocatable component*. Background and Tonto's workaround:
`docs/GFORTRAN16_DEBUG_CRASH.md`.

Bugzilla fields: **Product** gcc · **Component** fortran · **Severity** normal
(wrong code) · **Keywords** `wrong-code`, `ice-on-valid-code` does not apply ·
**Known to fail** 16.0.1, 16.1.0 · **Known to work** 14.2.0, 14.3.0.

---

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
