# Running the tests, and blessing references

Two different questions:

- **Is my build sane?** Run the tests. The pass criterion is loose on purpose — relative
  difference ≤ 0.2%, *or* the last printed digit within 2 — so a build on your compiler, your CPU
  and your BLAS can pass against references made on someone else's. Build with every optimisation
  you like.
- **Can I make a reference other people will reproduce?** Much stricter, and the rest of this page.

## Running them

```bash
export CTEST_OUTPUT_ON_FAILURE=1   # so a failure says why, not just that it failed
ctest -L short                     # about a minute
ctest                              # everything
make report                        # every test, pass or fail, tabulated into tests.log
```

## A failure is not always a bug

Most cross-platform failures are not physics. The converged science reproduces to about five
significant figures across macOS/arm64/OpenBLAS and Linux/x86-64/netlib. What does not reproduce is
a handful of **derived quantities that divide by something near zero**, where one unit in the last
bit becomes a percentage:

| quantity | why it has no stable value |
|---|---|
| ADP principal axis ratio | `maxeval/mineval`, and `mineval` is negative and near zero for a non-positive-definite ADP |
| some per-shell ratios in the refinement tables | 1 ulp in the value they derive from becomes >1% in the ratio |
| a torsion angle across a planar bond | 0 or 180 depending on the last bit |
| refinement iteration counts | decided by parameter shifts of a few percent of an esd, which is noise |

Check whether a failure is one of these before investigating it. Measured cases are in
`TASKS_AND_HISTORY.md`.

## What changes the last bits

1. **Architecture tuning.** `-march=native` / `-mtune=native` bake the build host's instruction set
   into the binary, so two machines with the same compiler and BLAS still differ. Only a `fast`
   build tunes by default; any other type can opt in:

   ```bash
   cmake .. -DTONTO_ARCH_FLAG=auto                # tune for this machine
   cmake .. -DTONTO_ARCH_FLAG="-march=znver3"     # or a named target
   ```

2. **Fast-math.** `fast` uses `-Ofast`, which lets the compiler change the order of
   floating-point operations. Different compiler versions choose different orders, so the last
   digits change between compiler versions, even on the same machine. `release` and `reference`
   do not use it.

3. **The BLAS.** The reference (netlib) BLAS and LAPACK run the same code on every processor,
   so they give the same results everywhere. OpenBLAS chooses different code for different
   processors, so its results depend on the machine — and running in a container does not help,
   because a container uses the machine's own processor.

4. **The compiler version**, including a different *minor* release of the same major version.

## The `reference` build type

```bash
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=reference
```

`-O3 -fno-fast-math`, no architecture tuning, and the netlib BLAS and LAPACK. The only remaining
input is the compiler version. Configure stops if it would get anything else: a tuning flag,
another BLAS, or a system `libblas.so` that points at OpenBLAS. On macOS, `brew install lapack`
first.

**Bless with this, and note CI builds it too** — every workflow that compares against references
uses `reference`, so what CI checks and what you blessed are the same thing. It compiles the same
code as `release`; the two differ only in the BLAS. `release` remains the build for actual work,
and `release-static` is what the published binaries use.

## Blessing, by hand

One test at a time, from the build tree:

```bash
python3 ../scripts/test.py --bless \
        --build-dir      . \
        --test-directory ../tests/long/gly_ala_fragHAR_rhf_STO-3G \
        --basis-sets     ../basis_sets \
        --log-level=WARNING
```

Drop `--bless` to see the agreement line without touching anything. A `hart` or `rgbi` test takes
the same `--build-dir`: its `IO` manifest names the program, which is run from that directory.
A new test has no reference yet: it fails with `NO REFERENCE` and leaves `stdout.bad`, and the
same command with `--bless` adopts the output as its first reference.

Four rules:

- **Read every diff first.** `--bless` refuses output that shrinks by more than
  `--bless-min-line-ratio`, and nothing else protects you. Separate the three cases: an added output
  line (structural, harmless), one of the unstable quantities above, and a genuine numerical change.
- **Bless on the platform the badges test on** (Linux, gfortran-14, reference BLAS). A reference
  output records `Platform:` and `Compiler:` at the top — read them before assuming your machine
  can reproduce it.
- **One reason at a time.** Two changes blessed together cannot be told apart afterwards.
- **`--bless-anyway` overrides the shrink guard.** If you need it, you probably have a broken build.
