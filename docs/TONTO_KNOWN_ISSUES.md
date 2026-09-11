# Known issues and limits

What Tonto does not do, or does wrongly, that you may meet in ordinary use. One or two
lines each, with a pointer to the document that carries the detail.

**This is not a bug tracker.** `DEFERRED.md` is the live register of work, and it holds
the reasoning, the measurements and the plans. This page answers a narrower question:
*I hit something odd — is it known?*

A defect marked **silent** produces a wrong answer with no error and exit 0. Those are
the ones worth reading.

## Input

- **Any single input line is limited to 256 characters.** This applies to job files and
  to CIFs, which share one line buffer. An over-long line is refused and named, so it
  will not be silently truncated.
- **A long path must not be typed into a job file.** `basis_directory=` inside a job file
  is bounded by that 256-character line limit. Paths given through
  `TONTO_BASIS_SET_DIRECTORY`, `--basis-library` or `hart --basis-dir` are limited to 1024
  characters instead, which is the route to use for a deeply nested directory — on WSL,
  for instance.
- **A second `xray_data=` block does not restore the unpruned reflections.** Successive
  prunings compound.

## Numerical accuracy and platforms

- **The pass criterion for the test suite is loose**, not exact: 0.2% relative or two
  units in the last digit. A test can pass with visibly different output. See
  `TONTO_DEVELOPER_INFO.md` §1b.
- **A few tests differ on macOS only**, against references that are correct. The cause is
  which LAPACK is linked, not the science.
- **Column widths in printed tables can differ between builds** with every number
  identical. The comparator splits into tokens, so whitespace is invisible to it; a raw
  `diff` will show the drift and it is not a failure.
- **gfortran 16 is not the supported compiler.** A release build is fine, but a debug
  build has no array bounds checking, because `-fcheck=bounds` is miscompiled. Use
  gfortran 14. See `GFORTRAN16_GCC_BUG.md`.
- **Debug (`-O0`) builds have longstanding floating-point boundary failures** in the test
  suite. They are not translator bugs.

## Refinement

- **X-ray-constrained SCF convergence wanders**, sometimes violently. Well known, never
  diagnosed.
- **Extinction has eight defects, all silent.** Notably the gradient that drives the
  extinction parameter uses `Re(F³)` where it needs `|F|³`, so its sign is wrong for
  roughly half of a centrosymmetric dataset; and an *intensity* refinement with extinction
  reads uninitialised memory yet runs to completion and reports a converged answer. The
  optimiser is also unconstrained, so a negative extinction parameter can produce NaN.
  Full register in `EXTINCTION_REPORT.md` §3.
- **A CIF saying `_refine_ls_extinction_method none` does not turn extinction off.** The
  code tests the wrong CIF item. **Silent.**
- **Residual density is overestimated when unmerged Bijvoet pairs are present**, because
  every shared Fourier component is counted twice. The refinement itself is largely
  unaffected. See `TONTO_DISPERSION_CORRECTIONS.md`.
- **The quantity named `chi2` throughout the output is a goodness-of-fit squared**, not a
  chi-squared. See `GOF_NOT_CHI2.md`.

## DFT

- **DFT energies at the default grid (Becke partition, `accuracy= medium`) sit about
  5×10⁻⁸ from an independent reference.** `partition_scheme= stratmann_scuseria` is ten
  times further off on the same grid and its accuracy is not monotonic in `accuracy=`.
  See `DFT_STANDARDISATION.md` §6b.
- **The exchange–correlation energy is never reported separately.** `V_ee` lumps it in
  with the Coulomb term. See `DFT_STANDARDISATION.md`.

## Bader and QTAIM

- **The basin search is grid-dependent to the point of being unusable.** On water it
  gives 1 basin on a tight grid and 13942 on a wide one, where the answer is 3. Nothing
  in the code checks for or reports this. See `BADER_REPORT.md`.
- **Voxel volumes are summed per point but sized per interval.**
- **The basin search is serial**, even in an MPI build.

## Parallel (MPI)

- **A parallel run is not simply a faster serial run.** Read `TONTO_AND_MPI.md` before
  trusting one, and its defect register for what is still open.
- **`move_to_record` desynchronises the ranks** when a file is re-read, because the
  collective count is taken from a rank-local record number. It aborts rather than
  producing a wrong number, but only at two ranks or more.
- **Two out-of-bounds reads remain open** — one in the fragment RMA path, one in QTAIM at
  a single rank. The fragment one is **silent**.
- **Random-number seeds are not cloned across ranks.**

## Plotting

- **The pictures need gnuplot on your `PATH`.** Without it the plot data and the gnuplot
  script are still written, and a warning names the script so you can draw it yourself;
  only the `.png` is missing.
