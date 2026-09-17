# Speeding up Tonto's SCF: method, results and what did not work

For someone arriving fresh. It explains how Tonto builds the Fock matrix today, what has been
changed to make it faster, what was tried and abandoned, and what is still open.

Three documents cover this work, and they do different jobs:

| document | what it holds |
|---|---|
| **this page** | the method as it stands, and the lessons -- read first |
| `docs/SCF_SPEED_REPORT.md` | the measurements: every table, in the order they were taken |
| `DEFERRED.md`, *Speed up the SCF*, *Primitive-batched J and K* | the task register: decisions, open items, next steps |

All timings below are one core, karrikinolide (C8H6O3, 17 atoms) unless the zinc finger
[Zn(SCH3)2(imidazole)2] is named. Timings on this laptop vary by 2-6% between runs of the same binary, so a comparison
is only trusted when it is repeated, with the two binaries run at the same time.

---

## 1. Where the time goes

An SCF iteration builds a Fock matrix. Three parts cost anything:

- **J and K**, the Coulomb and exchange matrices, from the two-electron integrals. For
  Hartree-Fock this is 95-97% of the run.
- **The XC matrix**, for DFT, from a numerical quadrature on a molecular grid. Before the work
  described here it was two thirds of a BLYP run; after it, J is the larger part again.
- Everything else -- one-electron integrals, diagonalisation, DIIS, the density matrix -- is
  under 1%.

So the work has two strands, the grid and the integrals.

---

## 2. The XC quadrature (done)

What Tonto does now, in order of the pipeline:

1. **The molecular grid is built once per SCF**, not per iteration.
2. **Becke partitioning, `accuracy= medium`**, is the default grid. It is 1.5-6.7e-8 Eh from
   g09 on the reference DFT cases.
3. **Points where the promolecule density is below 1e-12 are dropped** (`prune_rho_cutoff=`).
   Exact, but it removes only 2-3% of points, because the basis-function cutoff already
   truncates each atom's grid.
4. **The quadrature is batched** (`use_batched_xc=`, on by default): points are grouped into
   spatial boxes of up to 512; for each box only the significant basis functions are evaluated,
   into flat arrays; the density and the XC matrix are then two matrix products (`dgemm`).
   Karrikinolide BLYP: XC 73.4 → 16.9 CPU s, energy within 1.5e-11.
5. **`pruning_scheme= adaptive`** is available but not the default. It sets the angular order
   per radial shell by distance in bohr: 20% faster at `medium` for the same energy, but at
   `high` it costs more than it gains.

The per-shell error measurements that the adaptive scheme is calibrated from are in
`SCF_SPEED_REPORT.md`, *Stage B*; the grid standardisation itself in `DFT_STANDARDISATION.md`.

---

## 3. How J and K are built

### 3.1 The integral pipeline

Tonto uses **Rys quadrature** with the **horizontal recurrence (the "transfer relation") applied
after contraction**. For one shell quartet (ab|cd):

1. **Set up the shell pairs.** For each primitive pair of a and b: the exponent sum ζ, the
   product centre P, the offset from P to the centre of the higher-l shell, and the prefactor
   (contraction coefficients times the gaussian-product exponential). Geometry-free parts come
   from `SHELL1PAIR`, precomputed per pair of basis shells; the geometry-dependent parts are
   recomputed per quartet (`SHELL1QUARTET:set_ab_new`, `set_cd_new`).
2. **For each primitive quartet** (a primitive pair on each side): X = ρ|PQ|², the Rys roots
   and weights for X, then for each root the three **2-D integral tiles** Ix, Iy, Iz by the
   vertical recurrences. A tile is at most (l_ab+1) × (l_cd+1), so tiny.
3. **Contract**: the (e0|f0) integrals, where e and f run over the cartesian components from
   l_max to l_sum on each side, are Σ over primitives and roots of Ix·Iy·Iz. These are called
   `esfs` in the code.
4. **Transfer** to the real (ab|cd) components. The transfer coefficients depend only on the
   distance AB, not on exponents, which is why doing it *after* contraction saves so much.

`SHELL1QUARTET` holds hand-specialised versions of steps 2-3 (`make_esfs_*`) for the common
angular momentum classes, and generic ones (`make_esfs_Xs` ... `make_esfs_XX`) for the rest.

### 3.2 The J engine and K

- **J** does not need the (ab|cd) integrals at all. The density is taken backwards through the
  transfer relation once per SCF iteration, giving a density in the (e0| space for each shell
  pair (`MOLECULE.FOCK:reverse_transfer`). J is then contracted directly from `esfs`, and taken
  forward through the transfer at the end. This is the **J engine**.
- **K** needs the transferred (ab|cd), because the density couples functions from different
  shell pairs. It is digested quartet by quartet.
- Only unique quartets are computed, with the usual permutational factors.

### 3.3 Spherical bases

A spherical basis builds J and K with the **cartesian engine**: the density is expanded to the
cartesian basis (P_cart = U P Uᵀ), J and K are built, and contracted back (Uᵀ J U). This is exact
and removes the per-quartet rotation: karrikinolide RHF/cc-pVTZ spherical 1231 → 868 s.

### 3.4 Screening

Four cutoffs decide what is computed:

| cutoff | what it drops |
|---|---|
| `ERI_primitive_pair_cutoff` | a primitive pair whose gaussian-product exponential is below it (only when the quartet's four atoms differ) |
| `ERI_Schwarz_cutoff` | a shell quartet whose Schwarz bound, sqrt((ab|ab)(cd|cd)) times the largest density element, is below it |
| `ERI_J_density_cutoff`, `ERI_K_density_cutoff` | density-weighted tests in the engine |

They are set together by **`eri_accuracy=`**:

| level | primitive pair | the other three | karrikinolide RHF/6-31G(d), from g09 | cost |
|---|---|---|---|---|
| `very_low` | 1e-6 | 1e-9 | 2.4e-6 | 1.00 |
| **`low` (default)** | 1e-9 | 1e-9 | 3.1e-8 | 1.07 |
| `medium` | 1e-12 | 1e-12 | 1.8e-9 | 1.31 |
| `high` | 1e-15 | 1e-15 | 1.8e-9 | 1.48 |

The primitive-pair cutoff alone was 99% of the old error. `escalate_eri_accuracy= TRUE` runs the
damped early iterations at `very_low`. An x-ray constrained SCF is held at `medium` or tighter
once a level is given.

**Trap:** once `eri_accuracy=` is given, the level's cutoffs are re-applied at every iteration,
so an explicit `eri_schwarz_cutoff=` or similar in the same input is silently overwritten. Do not
combine them until this is fixed (`DEFERRED.md`).

### 3.5 Memory handling

All work arrays for the quartet loop come from one `ERI_SCRATCH`, sized before the loop for the
largest quartet the basis can form. The allocator went from 8% of the run to 1%.

---

## 4. Early contraction: the loop-order question

This is the question that motivated the current direction, so it is set out in full.

**Two ways to arrange the loops.** For a shell quartet with many primitives, the (e0|f0)
integrals are a sum over primitive quartets and roots of Ix(xe,xf)·Iy(ye,yf)·Iz(ze,zf).
The loops can be nested either way round:

```
Today (SHELL1QUARTET)                     Primitive index outermost (GAUSSIAN4's order)

for each primitive quartet, root i:       esfs = 0
   build tiles into Ixa(i,:,:),           for each primitive quartet, root i:
   Iya(i,:,:), Iza(i,:,:)                    build the three small tiles Ix, Iy, Iz
for each component pair (e,f):               for each component pair (e,f):
   esfs(e,f) = sum over i of                    esfs(e,f) += Ix(xe,xf)*Iy(ye,yf)*Iz(ze,zf)
     Ixa(i,xe,xf)*Iya(i,ye,yf)*Iza(i,ze,zf)
```

Both do the same arithmetic. The left-hand form fills a large buffer of tiles for every primitive
quartet and then reads long columns of it once per component pair -- 1296 times for (dd|dd). The
right-hand form reads each small tile once and keeps everything in the fastest cache.

**The lineage.** Tonto's older `GAUSSIAN4`/`SHELL4` code used the right-hand order: the formula for
one primitive quartet, with the contraction loops outside. `SHELL1QUARTET` changed two things: it
moved the transfer to after the contraction, which is the real gain, and it made the primitive
index the innermost long loop, so that the compiler could vectorise it -- which is where the
buffers came from.

**The hypothesis was that the buffers limit the speed** ("memory traffic"): the roots' share of the
profile fell when they were vectorised, but the run time did not move, and three copies running
at once each ran 15% slower than one alone.

**The test and its answer: the buffers are not the limit.** `make_esfs_XX` was rewritten in the
right-hand order and timed against the original, three repeats each:

| | 6-31G(d) J/K | cc-pVTZ J/K | energy |
|---|---|---|---|
| left-hand (today) | 53.5 s | 1076 s | |
| right-hand | 54.5 s (+1.9%) | 1107 s (+2.9%) | same to 1e-14 |

The cache-miss rates measured beforehand were low too (3-4% of level-1 loads). The long contiguous
sum over primitives is exactly what the compiler vectorises well, and reading small tiles through
index lookups is not. So on a CPU the loop order is not a lever, and the rewrite was not kept.

**What does help** is changing *what* the long contiguous loop runs over: not the primitives of one
quartet, but a large batch of primitive quartets drawn from many shell quartets at once. That is
section 5, and it is also the shape a GPU needs.

---

## 5. The primitive pair list and the batched J kernel (in progress)

On branch `esfs-order`, behind `scfdata= { use_gaussian_pair_J= TRUE }`, off by default. J only.

### 5.1 The idea

- **Store the geometry-dependent pair data once**, not per quartet. The surviving primitive pairs
  number only 1-4 × 10⁴ for these molecules (a few MB), even at triple zeta. The primitive quartets
  (4 × 10⁷ upwards) are never stored: storing them would be tens of GB, and reading them back
  would be slower than recomputing.
- **Group the pairs by class** (l_max, l_min), and loop over two class blocks at a time. There is no
  shell-quartet object and no per-quartet set-up.
- **For one pair k, batch all its partners j and all roots** into contiguous columns (at most 256
  entries): the X values and roots in one call, the recurrence coefficients as vectors, the tiles as
  `I(p,e,f)` with the batch index p first, and the contraction with the (e0|-space density in the
  same pass. The long vectorised loop now runs over the batch.
- The transfer relations and the J engine's reverse/forward transfer are unchanged.

`GAUSSIAN_PAIR_LIST` (`foofiles/gaussian_pair_list.foo`) holds the flat arrays and the kernel;
`MOLECULE.FOCK:make_gaussian_pair_list` builds it.

### 5.2 Screening at the primitive level

Two lessons, both learned the hard way:

- **Bounds must be taken in the AO basis.** A Schwarz bound computed from (e0|e0) integrals does not
  bound the real integrals, because the forward transfer amplifies. The first version lost 8e-7 Eh
  at cc-pVTZ. Each pair's bound is now the AO bound of its shell pair (`max_I`), scaled by that
  primitive's share of the shell pair's (e0|-space bound -- valid because a shell pair's primitives
  share the same transfer coefficients -- and the density maximum is taken from the AO density.
- **Many small skips add up.** Skipping individual primitive quartets drops far more terms than
  skipping whole shell quartets. The cutoff for a pair of pairs is therefore divided by the number
  of primitive pairs kept on both sides, so the total dropped from one shell quartet stays below the
  shell-level cutoff.

Pairs within a class are sorted by decreasing bound, so the partner loop stops at the first failure.

### 5.3 Results so far

BLYP, J/K CPU seconds, one job at a time, against the `develop` J engine:

| molecule, basis, level | engine | pair list | energy, engine / list, distance from `high` |
|---|---|---|---|
| karrikinolide 6-31G(d), `low` | 44.0 | 43.9 | 2.4e-11 / 2.2e-11 |
| karrikinolide cc-pVTZ, `low` | 767 | **598 (−22%)** | 2.3e-8 / 3.6e-9 |
| karrikinolide cc-pVTZ, `high` | 1210 | **1032 (−15%)** | identical to 1e-14 |
| zinc finger 6-31G(d), `low` | 215.6 | **190.7 (−12%)** | 2.3e-6 / 6.6e-8 |
| zinc finger 6-31G(d), `high` | 319.6 | 378.4 (+18%) | identical |

So at the default level the pair list is as fast or faster and more accurate. At `high` it is
faster at triple zeta and slower at 6-31G(d), where the count-scaled cutoff becomes very tight.

### 5.4 Still to do

- **K.** The exchange index pattern couples different shell pairs, so it cannot be contracted in the
  (e0| space like J. The options are to feed the existing quartet digestion from the list, or a
  different algorithm.
- **Tune the count-scaled cutoff.** Without the scaling the list was 10% faster than the engine at
  6-31G(d) and slightly less accurate.
- **Batch across pairs k**, not just within one k: the GPU shape.
- **Shared exponents.** Basis sets that share exponents across shells (6-31G's sp shells, cc-pVTZ's
  general contraction) halve the 6-31G(d) pair list if the tiles are shared; not yet exploited.
- **MPI over class blocks.**
- `short` and `long` before merging.

---

## 5a. RI-J: density fitting of J for pure DFT (branch `ri-j`)

`auxiliary_basis_name= def2-universal-jfit` with `scfdata= { use_RI_J= TRUE }`. Off by default,
because it changes the energy by 1e-4 to 1e-3 Eh; stops with a message for HF and hybrids, where a
separate J gains nothing (section 7).

- The density is fitted in an auxiliary basis with the Coulomb metric: d_Q = (Q|rho), V c = d,
  J_ab = sum_Q (ab|Q) c_Q. The fitting error in the energy is one-signed (negative) and matches
  ORCA's with the same auxiliary basis to 1e-8 Eh.
- **The three-centre integrals come from the pair-list kernel.** An auxiliary primitive is a
  one-centre "pair" of class (L,0), so `GAUSSIAN_PAIR_LIST:add_to_RI_J` is the batched J kernel with
  one side taken from a second list. Nothing is stored: two passes per J build. There are only a
  few hundred auxiliary primitives against 1-4 x 10^4 orbital pairs, so a pass is about 10^7
  primitive integrals where the exact J does 10^9.
- The metric's Cholesky factor is made once per SCF (`MOLECULE.FOCK:initialize_RI_J`). Auxiliary
  functions are always spherical, whatever the orbital basis: cartesian f and g shells carry
  lower-l contaminants that make the metric nearly singular.
- **Result:** J is 6-40 times faster (zinc finger def2-TZVP 1207 -> 69 s, karrikinolide def2-TZVP
  1000 -> 24 s), and the XC quadrature becomes the largest part of the job.

Still to do: automatic auxiliary bases for basis sets that have none (pob-TZVP, so HAR); the effect
of the fitted density on structure factors; MPI; `short` and `long` before merging.

---

## 5b. COSX: the exchange matrix by quadrature (branch `cosx`)

`scfdata= { use_COSX= TRUE }`, for HF and hybrids, closed shell so far; with `use_RI_J= TRUE` it is
what ORCA calls `RIJCOSX`. Off by default: the exact J and K routes are untouched and remain the
route for work that must not be approximate.

- **The method.** One electron is integrated on a grid, the other analytically:
  K(m,n) = sum over grid points g of w(g) X(g,m) sum_s A(g;n,s) F(g,s), then made symmetric. X are the
  basis function values, F = X P, and A(g;n,s) is the potential at point g of the product of
  functions n and s.
- **The potentials come from the pair-list kernel.** A grid point is the limit of an auxiliary s
  primitive of infinite exponent in the RI-J kernel, so
  `GAUSSIAN_PAIR_LIST:make_point_potentials` is that kernel with no recursion on the point's side:
  1-D tiles, all the points of a batch in one contiguous loop. It returns (es| integrals; the
  shell pair's transfer, written out once per build as a matrix, turns them into (ab|.
- **The grid side is the XC batch machinery** (section 2): box batches of up to 512 points with
  their significant shells, F and K as two matrix products per batch
  (`MOLECULE.FOCK:make_r_K_COSX_on`). It works in the cartesian functions, so a spherical basis
  goes through U P U^T and back, as in 3.3.
- **Its own grid**, `cosx_grid_accuracy=` (default `very_low`), made by `initialize_COSX`, so HF
  needs no DFT grid and a hybrid keeps its XC grid.
- **Overlap fitting** (`use_cosx_overlap_fitting=`, on): the quadrature index of K is multiplied by
  S S_num^-1, S_num being the overlap matrix by quadrature on the same grid. It cuts the error of
  the coarsest grid thirteen-fold.
- **The final energy comes from a finer grid** (`cosx_final_grid_accuracy=`, default `high`): one
  more Fock build for the converged density, orbitals unchanged. The grid error of K falls slowly
  and unsteadily with the grid (karrikinolide: 1.5e-5, 2.9e-5, -1.5e-5, 2.0e-6 Eh from `very_low`
  to `high`), but the energy is stationary, so the coarse SCF grid costs second order only.
- **Screening**: on each batch a shell pair is skipped when its Schwarz bound times the largest F
  of its two shells there is below the Schwarz cutoff. It gains little on a 17-atom molecule,
  where every shell is linked to every batch through the density.
- **Result so far:** the COSX error is 2e-8 Eh for water and 3e-6 for karrikinolide, ORCA's being
  4e-7 and 2e-6; it is a hundred times smaller than the RI-J error it travels with. J and K, RI-J
  plus COSX against the exact engine: karrikinolide def2-TZVP 343 s against 757-892 s; zinc finger
  def2-TZVP 1006 s against 1527 s, and 676 s with OpenBLAS, where ORCA's whole `RIJCOSX` job is
  499 s. **At def2-SVP it loses** (zinc finger 285 s against 151 s), as it does in ORCA: the exact
  K is cheap in a small basis, and COSX costs points times shell pairs whatever the basis.
  Numbers: `SCF_SPEED_REPORT.md`, *COSX*.
- **Where the time goes** (karrikinolide def2-TZVP): the point-potential kernel 26%, the transfer
  and the contraction with F 20%, `dgemm` with the reference BLAS 18%, the Rys roots 17%. The
  final build on `high` is a quarter to a third of the whole, having seven times the points.

Still to do: a grid made for this integrand (the error falls with the angular order in the
bonding region, not with the radial points, so the named XC levels are a poor ladder for it);
unrestricted and hybrid DFT; timings in triplicate; MPI; HAR; `long` before merging.

---

## 6. What was tried and did not pay

Each is one line here; the numbers are in `SCF_SPEED_REPORT.md` under the heading named.

| attempt | outcome | section in the report |
|---|---|---|
| Incremental (delta-density) Fock builds | Correct after three latent defects were fixed, but the increments are noisy under absolute Schwarz cutoffs and DIIS chases the noise: no saving. Off by default. A ΔP-scaled screening would be the version to try. | *Step 3* |
| Loose integrals until partly converged (g09's two-pass) | Became `escalate_eri_accuracy=`; worth about 8%, because only two iterations are damped. | *Named ERI accuracy levels* |
| Reduced multiplication scheme for the contraction (`use_rms_esfs=`) | Exact but 1.4-5% slower: it adds stored products and scattered writes to a loop that was already vectorised. | *No-grid J/K profiles* |
| Vectorising the Rys roots within one quartet | The root kernels are 3-4× faster per X, but 77% of X fall in batches that straddle a T range, and the whole job did not move. | *Rys step 3* |
| Moving the `transfer_l_*` work arrays onto `ERI_SCRATCH` | Skipped: the allocator was already 1% of the run. | *Where the J/K time goes* |
| Primitive index outermost in `make_esfs_XX` | 2-3% slower (section 4). | `DEFERRED.md`, step 0 |
| Sharing primitives across generally contracted shells at cc-pVTZ | Not worth it at the shell-quartet level: the sharing is confined to the s shells, 1.44×, and the contraction coefficients differ. Revisited in the pair list (5.4). | `DEFERRED.md` |
| Tighter or looser screening of COSX on a 17-atom molecule | Nothing to skip: every shell pair matters on every batch. 1e-7 for 1e-9 saves 3% and costs 5e-6 Eh. | *COSX* |
| A coarser grid than `very_low` for the COSX iterations | A third less time, but 6.7e-5 Eh out after the final build, against 2.9e-6. | *COSX* |
| Promolecule-density pruning of the grid | Exact, kept, but saves almost nothing. | *Stage C* |
| Adaptive angular pruning at `high` | Costs more than `treutler_ahlrichs` there. Kept as an option. | *Stage D* |

---

## 7. Where Tonto stands against g09 and ORCA

Wall time, one core, default settings of each code (so not accuracy-matched: Tonto's `low` and the
others' defaults differ by up to 1e-6 Eh):

| job | Tonto | g09 | ORCA exact |
|---|---|---|---|
| karrikinolide RHF/6-31G(d), cartesian | 43.8 s | 6.9 s | -- |
| karrikinolide RHF/cc-pVTZ, cartesian | 857 s | 907 s | -- |
| karrikinolide RHF/cc-pVTZ, spherical | 868 s | -- | 780 s |
| karrikinolide RHF/def2-TZVP (spherical, before 3.3) | 900 s | 785 s | 604 s |
| zinc finger RHF/6-31G(d), cartesian | 179 s | 70 s | -- |
| zinc finger BLYP/6-31G(d), cartesian | 280 s (242 s pair list) | 96 s | -- |

At triple zeta Tonto is close; at double zeta g09 is several times faster. ORCA's default BLYP
uses density fitting (RI-J), which is where its large DFT speed comes from; it moves the energy by
about 8e-4 Eh.

---

## 8. Traps for the next person

- **Foo identifiers are case-sensitive, Fortran's are not.** `JJ` and `jj`, `M` and `m` in one
  routine compile to the same name. The compiler catches it, with a confusing message.
- **`eri_accuracy=` overwrites explicit cutoffs every iteration** (section 3.4).
- **Transfer-space quantities are not AO bounds** (section 5.2).
- **One pair of timings cannot resolve 3%.** Repeat three times, and run the candidates at the same
  time.
- **On this laptop, long background jobs can be killed by the session's memory guard** even though
  Tonto uses under 100 MB; run them detached and one at a time.
- **Spherical-basis SCF on the zinc finger does not converge**, and Tonto prints the unconverged
  energy without a warning. Cartesian is fine. A separate SCF-convergence problem, possibly
  low-lying states needing fractional occupation, not an integral defect (`DEFERRED.md`,
  *Correctness*).
