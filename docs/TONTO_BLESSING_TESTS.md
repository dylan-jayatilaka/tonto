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
`DEFERRED.md`.

## What changes the last bits

1. **Architecture tuning.** `-march=native` / `-mtune=native` bake the build host's instruction set
   into the binary, so two machines with the same compiler and BLAS still differ. Tonto therefore
   defaults to **no** tuning. Opt in for speed:

   ```bash
   cmake .. -DTONTO_ARCH_FLAG=auto                # tune for this machine
   cmake .. -DTONTO_ARCH_FLAG="-march=znver3"     # or a named target
   ```

2. **Fast-math.** `release` uses `-Ofast`, which permits floating-point reassociation. Compilers
   reassociate differently from release to release, so `-Ofast` drifts between compiler versions on
   the same hardware.

3. **The BLAS.** Netlib reference BLAS/LAPACK has no runtime CPU dispatch, so it is deterministic.
   OpenBLAS picks kernels from the detected microarchitecture, which reintroduces machine
   dependence — and a container does not help, because a container shares the host CPU.

4. **The compiler version**, including a different *minor* release of the same major version.

## The `reference` build type

```bash
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=reference
```

`-O2 -fno-fast-math`, no architecture tuning. Slower than `release`, and not what users build. With
netlib BLAS the only remaining input is the compiler version. Bless with this.

## Blessing, by hand

One test at a time, from the build tree:

```bash
python3 ../scripts/test.py --bless \
        --program        ./tonto \
        --test-directory ../tests/long/gly_ala_fragHAR_rhf_STO-3G \
        --basis-sets     ../basis_sets \
        --log-level=WARNING
```

Drop `--bless` to see the agreement line without touching anything. For a `hart` or `rgbi` test,
`--program` still points at `tonto`: the test's `IO` manifest names the real program, resolved as a
sibling.

Four rules:

- **Read every diff first.** `--bless` refuses output that shrinks by more than
  `--bless-min-line-ratio`, and nothing else protects you. Separate the three cases: an added output
  line (structural, harmless), one of the unstable quantities above, and a genuine numerical change.
- **Bless on the platform the badges gate on.** A reference carries `Platform:` and `Compiler:` in
  its banner — read them before assuming your machine can reproduce it.
- **One reason at a time.** Two changes blessed together cannot be told apart afterwards.
- **`--bless-anyway` overrides the shrink guard.** If you need it, you probably have a broken build.
