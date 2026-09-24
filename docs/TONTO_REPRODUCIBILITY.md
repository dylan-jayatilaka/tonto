# Reproducing Tonto's numbers, and blessing references

Two different questions, often confused:

- **"Is my build sane?"** — run `ctest`. The loose criterion (relative difference ≤ 0.2%, or last
  printed digit within 2) is there precisely so a build on your compiler, your CPU and your BLAS
  can pass against references made elsewhere. Build however you like, including every
  optimisation.
- **"Can I make a reference other people will reproduce?"** — a much stricter question, and the
  rest of this page.

## What the physics does, and what the suite compares

Across macOS/arm64/OpenBLAS, Linux/x86-64/netlib BLAS, and two minor releases of gfortran-14, the
converged science agrees to about **five significant figures** — for the gly_ala fragment HAR,
R(F) 0.032422 against 0.032421 and GoF 3.354131 against 3.354027.

What does *not* reproduce is a small number of **ill-conditioned derived quantities**, where one
unit in the last bit becomes a percentage:

| quantity | why it is unstable |
|---|---|
| ADP principal axis ratio | `maxeval/mineval`, and for a non-positive-definite ADP `mineval` is negative and near zero |
| some per-shell ratios in the refinement tables | 1 ulp in `2.7586` versus `2.7587` becomes 1.3% in a ratio derived from it |
| a torsion angle near a planar bond | 0 or 180 depending on the last bit |
| refinement iteration counts | decided by parameter shifts of 1-6% of an esd, which is noise |

So most reference failures are not physics failures. Before investigating one, check whether it is
one of these; `DEFERRED.md` carries the measured detail.

## The four things that change the last bits

1. **Architecture tuning.** `-march=native` / `-mtune=native` bake the build host's instruction set
   into the binary, so two machines with the same compiler and the same BLAS still differ.
   **Tonto therefore defaults to no architecture tuning.** Opt in for speed:

   ```bash
   cmake .. -DTONTO_ARCH_FLAG=auto        # tune for this machine
   cmake .. -DTONTO_ARCH_FLAG="-march=znver3"   # or for a named target
   ```

2. **Fast-math.** `RELEASE` uses `-Ofast`, which implies `-ffast-math` and permits floating-point
   reassociation. Each compiler release reassociates and vectorises differently, so an `-Ofast`
   build drifts between compiler versions even on identical hardware.

3. **The BLAS.** Netlib reference BLAS/LAPACK is plain Fortran with no runtime CPU dispatch, so it
   is deterministic. **OpenBLAS selects kernels from the detected microarchitecture**, which
   reintroduces machine dependence — and a container does not help, because a container shares the
   host CPU and `CPUID` is not virtualised.

4. **The compiler version itself.** A different *minor* release of gfortran-14 is enough; see
   `CLAUDE.md` §6.

## The `REFERENCE` build type

```bash
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=reference
```

`-O2 -fno-fast-math`, no architecture tuning. Slower than `RELEASE` and deliberately not what
users build. With netlib BLAS, the only remaining input is the compiler version.

## Blessing references

```bash
export CTEST_OUTPUT_ON_FAILURE=1
ctest                       # see what moved, and why
ctest ... --bless           # via scripts/test.py; see its --bless options
```

**Read every diff before blessing.** `test.py` refuses a bless that shrinks the output by more than
`--bless-min-line-ratio`, but nothing stops it from blessing a real regression that happens to look
like noise. The failure modes to separate are: an added output line (structural, harmless), an
ill-conditioned derived quantity (see the table above), and a genuine numerical change.

**Bless on the platform the badges gate on.** `DEFERRED.md` records references being blessed on
macOS and then having to be moved to Linux. Check the `Platform:` and `Compiler:` banner inside a
reference before assuming your machine can reproduce it.
