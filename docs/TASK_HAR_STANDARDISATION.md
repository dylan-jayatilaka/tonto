# Standardising the density behind Hirshfeld atom refinement

The plan and log for settling one robust protocol for the electron density used in refinement
— atoms, basis, method, crystal environment and partition — that works across the periodic
table. Rule for the order of work (Dylan, 2026-10-05): **the things which fix the refinements
are done first.** No new ADP-model features until the protocol is frozen; the ADP studies are
then rerun under it. Reruns are scripted (`~/tonto-runs/2026-10-05_mode_fitting`).

## 1. Why now

On urea, fine conclusions changed with the quality of the density: the NH₂ wag amplitudes
refine to zero in def2-SVP with cluster charges and to clearly nonzero values on the def2-TZVP
cluster (`docs/REPORT_ON_MODE_FITTING.md`). Measured effect of each choice on a urea refinement:

| choice | effect on GoF | effect on N–H |
|---|---|---|
| basis, def2-SVP to def2-TZVP | 0.25–0.40 | up to 0.006 Å |
| method, RHF to B3LYP | 0.5–0.7 | about 0.01 Å |
| crystal environment: none, cluster charges, 7-molecule cluster | 0.1–0.2 | 0.01–0.02 Å |
| partition, Hirshfeld or TFVA | about 0.2 | 0.01–0.04 Å |
| ADP model: free, TLS, soft modes | 0.05–0.2 | under 0.01 Å |
| spherical-atom model: rotational average or ensemble | 0.002–0.003 | no printed digit |
| exact exchange or RI-J with COSX | 0.0004 | under 0.001 Å |
| Cartesian or spherical functions | 0.07–0.13 in def2-SVP, at most 0.01 in def2-TZVP; spherical the lower | up to 0.004 Å, under one esd |
| contracted or uncontracted core | not measured; Dylan is testing it | |

## 2. Decisions taken

All Dylan's, 2026-10-05, unless marked.

1. **Basis sets: the def2 family, with spherical functions.** def2 was designed for spherical
   functions and is newer; the Pople bases are dropped, and with them the Cartesian-d question.
   Every study from now on sets `use_spherical_basis= TRUE`. Making it Tonto's default means a
   full re-bless of the test references; not yet decided.
2. **The spherical atoms: the default stays.** An unrestricted SCF with the molecule's own
   method (UHF, or UKS with the same functional), then the average over all rotations
   (`ATOM:spherically_average`). That average of one component of the ground term is the density
   of the equal mixture of the term's components: the spherical ground-state atom. `ano_kind=
   aoc` and `fon` stay available and off.
3. **No relaxation of the orbitals in the averaged field.** The spherically averaged atom is
   unphysical anyway, and relaxing in its field does not capture what happens to an atom in a
   molecule, where the core contracts. So the missing relaxation is not a defect to repair. One
   number is on record: on urea the ensemble (relaxed) atoms change GoF by 0.002–0.003.
4. **Functionals:** RHF, BLYP and B3LYP are good choices. PBE0 and a SCAN-type functional are to
   be added, being the most used now; range-separated hybrids, which Tonto lacks, later.
5. **One partition, robust across the periodic table,** is to be chosen, on the nine-structure
   X-ray and neutron set (§5).
6. **Disordered structures are left out** of test sets (ice VI was removed).

## 3. The spherical-atom study (first task)

The September numerical fault is fixed: open-shell atoms depended on the machine, because the
old average over the cube group used wrong rotation matrices for spherical d, f and g
functions and cannot make d–d products spherical. Since 2026-09-27 the average is exact (a
quadrature over Euler angles, exact to L = 2 l_max), and water's starting energy and the
Hirshfeld moments are identical under two BLAS kernels. Left to check:

1. Each atom's SCF reaches its ground term: H to Ar, then K to Kr, energies against published
   atomic values. (The zinc guess defect was one such failure, since fixed.)
2. Release against debug builds, and the Mac against achari2.
3. def2 atoms with spherical d and f functions, also inside a cluster made by `create_cluster`
   (which kept Cartesian functions until branch `cluster-spherical`, 2026-10-05).
4. The DFT ensemble atoms (`fon`) did not converge a BLYP refinement in 100 cycles: note only,
   since the option stays off.
5. **Dropped for now (Dylan, 2026-10-12).** It was: after scalar X2C (§6a), one heavy atom, to see
   whether the rotational average and the ensemble atoms (`ano_kind= aoc`, `fon`) still give the
   same refinement. It would be expensive, because the ensemble options need extending first (next
   sentences), and decision 3 of §2 already says the missing relaxation is not a defect to repair.
   The default atoms stand for heavy atoms too. This was the only place the
   question of the default atoms stayed open; the register row *Spherical atoms by the right
   ensemble* was closed on 2026-10-11 and folded in here. The ensemble options handle one open
   shell and stop at Kr, so they would need extending first: Roothaan's coupling constants for
   two open shells (Cr, Cu, the 4d and 5d rows, the lanthanides), and the Aufbau table beyond Kr.
   The theory for one open shell is in the comment under
   `MOLECULE.FOCK:make_AOC_effective_Fock_mx`. **Owed with this step: a user-facing page on how
   Tonto makes its atoms** (the default average, `ano_kind=`, what each handles), which does not
   exist; until then the keyword help and that comment are the only description.

## 4. Basis sets across the periodic table

- **def2-SVP, def2-TZVP, def2-TZVPP: H to Kr** in `basis_sets/`. Beyond Kr the published def2
  sets replace the core by an effective core potential, which is no use here: X-rays scatter
  from the core.
- **x2c-SVPall, x2c-TZVPall, x2c-TZVPPall: H to Rn**, all-electron, in `basis_sets/` (Pollak and
  Weigend, *J. Chem. Theory Comput.* 13, 3696, 2017; the same family as def2). All 86 elements
  agree with the Basis Set Exchange copies in every exponent and coefficient. They are made for a scalar-relativistic Hamiltonian
  (X2C), which Tonto has as `relativity_kind= x2c` (§6a). To be tested: x2c-TZVPall with X2C
  against def2-TZVP on light atoms, where the two should agree, and on one 4d or 5d compound.
- **All-electron families that cover the table**, from the Basis Set Exchange metadata
  (2026-10-05; sets with no core potential on any element, and how far from H they are complete):

  | family | complete to | kind |
  |---|---|---|
  | x2c-SV(P)all, SVPall, TZVPall, TZVPPall, QZVPall, QZVPPall (each also `-s`, `-2c`) | Rn | segmented; the def2 design |
  | ANO-R, ANO-R0 to R3 | Rn | generally contracted |
  | ANO-RCC and its VDZP/VTZP/VQZP cuts | Cm | generally contracted |
  | ANO-DK3 | Lr | generally contracted |
  | jorge-DZP, TZP and -DKH forms (QZP to Xe only) | Lr | segmented |
  | Dyall v/cv/ae at 2z, 3z, 4z | Og | uncontracted |
  | HGBS, AHGBS (Lehtola) | Og | uncontracted |
  | UGBS | Th, gaps to Lr | uncontracted |
  | Sapporo DZP/TZP/QZP and -2012 | Xe | segmented |
  | pcseg-n, pc-n | Kr | segmented |

  **The partner of def2 beyond Kr is the x2c family** (in Tonto's library under the
  literature names since 2026-10-05; they were `x2c-SVP`, `x2c-TZVP`, `x2c-TZVPP`). Beyond Rn only the jorge
  sets (segmented) and Dyall or HGBS (uncontracted) remain. The uncontracted families are the
  systematic way to carry an uncontracted-core protocol across the table, at a cost in size.
- Auxiliary sets for RI-J: `def2-universal-jfit` covers H to Rn; whether it suits the x2c
  orbital sets needs checking.
- **The core.** A contracted core cannot follow the contraction of the core density on bonding.
  If X-ray data at high angle see that contraction, an uncontracted core will fit better and
  remove high-angle outliers. def2-TZVP is (11s6p2d1f)/[5s3p2d1f] on C, N, O; uncontracting gives
  1.40 times the functions (urea 168 to 236, Cartesian). Cheaper variants: uncontract only the
  tight s and p contractions on the heavy atoms, or a core-valence set. Dylan is testing this.

## 5. The test set

`~/Dropbox/tonto_data/xray_neutron_set/`: nine ordered molecular crystals from Chodkiewicz and
Woźniak, *IUCrJ* 12, 74 (2025), each CIF holding the measured reflections, which Tonto reads
directly. The neutron structures are still to be fetched (no CSD codes are given; the sources
are in that folder's README). All nine contain only H, C, N and O, so a test beyond the first
row needs further structures.

## 6. Functionals and libxc

Tonto has Slater exchange, Becke88, LYP and B3LYP, and its functional code takes the density and
its gradient only. The three steps depend on each other:

1. **libxc as the functional engine** (register row *libxc as the DFT functional engine*). The
   unrestricted forms come with it, which the atoms need.
2. **PBE0** then needs nothing more: it is a gradient functional with 25 % exact exchange.
3. **r2SCAN** (the regularised SCAN; SCAN itself is very sensitive to the grid) is a meta-GGA: it
   needs the kinetic energy density on the grid and a new term in the Fock matrix, for restricted
   and unrestricted cases. That is the larger step, and it rests on step 1.

Range-separated hybrids need the attenuated Coulomb integrals in the exact and COSX code: later.

## 6a. Relativity: scalar X2C (Dylan, 2026-10-11)

**Status, 2026-10-12: done, on `develop`** (pull request 47) (steps 1 to 3 below; what was done and measured
is at the end of this section). `relativity_kind= x2c`; user page `docs/RUNNING_TONTO.md`,
*Relativity*. Still open: a first refinement beyond Kr (the heavy-atom test of the default atoms,
§3 step 5, was dropped on 2026-10-12).

**Decisions (Dylan, 2026-10-11):**

- Proceed with X2C. Rescue what can be rescued from the old IOTC code, rewrite the rest.
- **Scalar X2C only, for now.**
- **No picture-change correction for the density.** It is not required.
- **Spin–orbit coupling is likely not required for the density either.** It is required for the
  current density, which belongs to a project not yet recorded and too far ahead to plan now.
- **The atoms stay as they are** with X2C: the unrestricted SCF on the ground state, then the
  average over rotations (decision 2 of §2). No `aoc`, no `fon`.

What was found on 2026-10-11. `molecule.rel.foo` was removed in commit `fcbd5a8a` ("Removed
molecule.rel .nomo .misc for now"). The calls to it in `molecule.scf.foo` (`make_r_DKH1_core_mx`,
`make_r_DKH2_core_mx`, `make_r_IOTC_core_mx`, `set_gc_IOTC_core_mx`) are commented out, and the
`select case` on `relativity_kind` falls to its default, the non-relativistic core matrix, with no
message. The picture-change-corrected structure factors (`make_FT_IOTC`, `make_IOTC_FT_PCE_off`,
`build_da_IOTC_ft` in `molecule.xtal.foo`) are commented out too.

X2C and IOTC decouple the same one-electron Dirac matrix exactly; X2C does it in one step of
matrix algebra and is Dyall's normalised elimination of the small component plus a
renormalisation (Iliaš and Saue, *J. Chem. Phys.* 126, 064102, 2007). With $`S`$, $`T`$, $`V`$
the overlap, kinetic and nuclear attraction matrices, $`W`$ the matrix of
$`\mathbf{p}\cdot V\mathbf{p}`$ and $`c`$ the speed of light, solve once, in the uncontracted
basis,

```math
\begin{pmatrix} V & T \\ T & W/4c^2 - T \end{pmatrix}
\begin{pmatrix} C_L \\ C_S \end{pmatrix} =
\begin{pmatrix} S & 0 \\ 0 & T/2c^2 \end{pmatrix}
\begin{pmatrix} C_L \\ C_S \end{pmatrix} E \qquad (1)
```

keep the positive-energy solutions (large and small parts $`C_L`$, $`C_S`$, energies $`E`$), and form

```math
X = C_S C_L^{-1}, \quad \tilde S = S + X^\dagger T X/2c^2, \quad
R = S^{-1/2}\,(S^{-1/2}\tilde S S^{-1/2})^{-1/2}\,S^{1/2} \qquad (2)
```

```math
h_+ = R^\dagger\,[\,V + TX + X^\dagger T + X^\dagger (W/4c^2 - T) X\,]\,R \qquad (3)
```

$`h_+`$ replaces $`T+V`$ as the core matrix, and is then contracted. Nothing else in a restricted
or unrestricted SCF changes.

Steps:

1. **Stop the silent fall-through now**: `relativity_kind=` other than none stops with a message
   until a relativistic core matrix exists.
2. **Rescue** from the history (`git show fcbd5a8a^:foofiles/molecule.rel.foo`): the
   $`\mathbf{p}\cdot V\mathbf{p}`$ integrals, the handling of the uncontracted basis, and a
   finite-nucleus model if one is there.
3. **Write** the scalar X2C core matrix from equations (1) to (3). Check: one-electron ions
   against the Dirac energies; a heavy closed-shell atom against published X2C or Dirac–Fock
   energies and against PySCF's `x2c`; light atoms against def2 results.
4. **Not planned: a picture-change correction** (decision above). Bučinský, Jayatilaka and
   Grabowsky (*J. Phys. Chem. A* 120, 6650, 2016) found picture change and spin–orbit coupling
   negligible for the structure factors of HgPh₂ and BiPh₃, and scalar relativity and electron
   correlation important at low and medium angle. The old picture-change code in
   `molecule.xtal.foo` is left commented out.
5. **Not planned: spin–orbit coupling** (decision above). For the record, it would need the
   spin-dependent part of $`W`$, complex two-component orbitals (Tonto's `gchf` kind for
   Hartree–Fock; nothing yet for DFT), and a two-electron spin–orbit correction.

**What was done (2026-10-11, branch `x2c`).**

- `foofiles/molecule.rel.foo` is new and small: the four primitive matrices, the contraction
  matrix, and `make_r_X2C_core_mx`. It is one case in `MOLECULE.SCF:make_r_core_mx`. Rescued
  from the old file: the loop over primitive shell pairs, and the $`\mathbf{p}\cdot V\mathbf{p}`$
  integrals (`GAUSSIAN2:make_pVp_ints`, from the commented-out `make_DKH_ints` without its
  spin–orbit parts). The old IOTC algebra and its bookkeeping for repeated exponents were not used.
- **Repeated primitives.** A primitive that appears in several shells of an atom (same angular
  momentum, same exponent) is used once (`make_primitive_shells`). The contraction matrix is
  built from the stored coefficients and each column scaled to the norm the program gives that
  basis function; the routine stops if the contracted primitive overlap is not the molecule's
  overlap matrix to 1e-8. That check covers Cartesian component norms and the order of spherical
  components without the routine having to know either.
- **The algebra is done in Cholesky bases, and that matters.** The first version orthonormalised
  with the eigenvectors of $`S`$ and then diagonalised $`T`$. It agreed with PySCF to 1e-8 hartree
  on every library basis, but was wrong by 6e-7 (Ne⁹⁺) and 1.1e-6 hartree (Hg⁷⁹⁺) in an
  even-tempered s set with exponents from 0.016 to 1.7e10: the eigenvalues of $`T`$ span 13
  orders of magnitude, and an eigenvector routine gives the small ones an absolute error set by
  the largest. With $`S = L_S L_S^\dagger`$ and $`T = L_T L_T^\dagger`$ (Cholesky), the Dirac
  matrix is $`L_S^{-1} V L_S^{-\dagger}`$, $`\sqrt2\,c\,L_S^{-1}L_T`$ and
  $`\tfrac12 L_T^{-1} W L_T^{-\dagger} - 2c^2`$, the metric is the unit matrix, and the error
  is 1e-11. The prototype that found this is `~/tonto-runs/2026-10-11_x2c/proto.py`.
- **Self-check in every run:** $`h_+`$ must reproduce the positive Dirac energies to 1e-8
  relative, or the job stops.
- **Nuclei.** Points by default. `nucleus_model= gaussian` gives the Gaussian charges of Visscher
  and Dyall (the table `gaussian_nucleus_eta` in `gaussian2.foo`, identical to PySCF's exponents
  for H, Br, Kr, Hg and Rn). The attraction and $`\mathbf{p}\cdot V\mathbf{p}`$ integrals take
  the nuclear exponent $`\eta`$ as an optional argument: with $`p`$ the sum of the two Gaussian
  exponents and $`f = \eta/(p+\eta)`$, the Rys argument is scaled by $`f`$, a root $`t^2`$ gives
  the Gaussian exponent $`p f t^2/(1 - f t^2)`$, and the integral is scaled by $`\sqrt f`$. The old
  finite-nucleus routine (`make_finite_nuclear_attraction_ints`) failed an array bounds check and
  was removed; before this work `nucleus_model= finite` gave a nuclear attraction matrix of zero,
  with no message. A Gaussian nucleus is available only under X2C, because the contracted
  attraction matrix (`SHELL2:get_nuc`) knows only points.
- **Stops instead of silent wrong answers:** `relativity_kind=` other than `none`, `x2c`, `pauli`,
  `zora`; a nucleus model other than point without X2C; X2C with general complex orbitals.
- **Speed of light:** `SPEED_OF_LIGHT_AU` was 137.03599 and is 137.035999177 (CODATA 2022).
- **Output:** under X2C the energy summary prints one number, the one-electron energy, in place
  of the nuclear attraction energy, the kinetic energy and the virial ratio.
- **Not needed:** no energy-gradient job is reachable from the input (`optimise_geometry` is
  commented out), so there is nothing to guard. Cluster charges and an electric field are added
  to the core matrix after the transformation, untransformed.

**What was measured** (PySCF 2.14, spin-free X2C, same basis, $`c`$ = 137.035999177, spherical
`x2c-SVPall` unless said; hartree; Hartree–Fock; runs in `~/tonto-runs/2026-10-11_x2c`, PySCF in `venv` there).

| System | Tonto | PySCF |
|---|---|---|
| Ne | -128.51206173 | -128.512061727 |
| Kr | -2787.70616450 | -2787.706164503 |
| Xe | -7442.86712300 | -7442.867122998 |
| Hg | -19622.39695888 | -19622.396958891 |
| Rn | -23560.96241364 | -23560.962413653 |
| HBr, 1.414 Å | -2604.57465099 | -2604.574650994 |
| HI, 1.609 Å | -7112.65502747 | -7112.655027467 |
| Hg⁷⁹⁺ (UHF) | -3531.26744210 | -3531.267442103 |
| Br atom (UHF) | -2603.97313216 | -2603.973132161 |
| Kr, Cartesian | -2787.71086791 | -2787.710867914 |
| HBr, Cartesian | -2604.57855931 | -2604.578559312 |
| H₂O, cc-pVDZ (repeated primitives) | -76.07156215 | -76.071562149 |
| Hg, x2c-TZVPall | -19623.42960918 | -19623.429609193 |
| Kr, Gaussian nucleus | -2787.68716175 | -2787.687161752 |
| Hg, Gaussian nucleus | -19618.92472122 | -19618.924721233 |
| Rn, Gaussian nucleus | -23554.62526761 | -23554.625267627 |
| HBr, Gaussian nuclei | -2604.55856114 | -2604.558561141 |
| H₂O, x2c-TZVPPall (f on O, d on H) | -76.10943979 | -76.10943979 |
| CHFClBr, x2c-TZVPall (five centres, no symmetry) | -3203.40084166 | -3203.40084179 |
| iodobenzene, x2c-TZVPall (276 functions) | -7343.14802981 | -7343.14803111 |

- Without X2C the same eight systems of the first block agree with PySCF to 1e-8 as well, so the
  basis sets are the same in the two programs. The X2C correction is -0.14 (Ne), -53 (Kr),
  -428 (Xe), -3702 (Hg) and -5151 hartree (Rn) in this basis.
- **One-electron ions,** 41 even-tempered s functions (exponents $`2^{34}`$ to $`2^{-6}`$), point
  nucleus. Tonto: Ne⁹⁺ -50.06674181, Hg⁷⁹⁺ -3532.19050050. The same matrix problem solved in
  40-digit arithmetic (`ref_mpmath.py`): -50.066741806 and -3532.190500498. The analytic Dirac
  energies $`c^2(\sqrt{1-(Z/c)^2}-1)`$ are -50.066742017 and -3532.192092935; the remainder is the
  basis.
- **DFT.** PBE on HBr differs from PySCF by 7.7e-4 hartree with X2C and by the same 7.7e-4
  without it, at Tonto's default grid; at `accuracy= very_high` both differences are 1.0e-5.
  X2C is right with DFT; **the default grid costs 8e-4 hartree on a bromine atom**, to be kept in
  mind when energies of heavy atoms are compared (a refinement was not tested).
- The gaussian nucleus raises the energy by 0.019 (Kr), 3.47 (Hg) and 6.34 hartree (Rn).
- **The last two rows differ by more than 1e-8, and not because of X2C.** Without X2C, CHFClBr
  differs from PySCF by 1.4e-7 and with it by 1.3e-7; iodobenzene by 7.9e-7 and 1.3e-6. That is
  the cutoff on the two-electron integrals in a direct SCF (Tonto at its default, PySCF at 1e-15
  for CHFClBr and its default for iodobenzene).
- **Cost.** Iodobenzene with X2C, release build, one core: 5 min 56 s for the whole job, 20
  iterations; PySCF took 7 min. The transformation itself is not noticeable at this size.
- **A stall that is not X2C's.** Iodobenzene with x2c-TZVPall has one overlap eigenvalue of
  9.1e-6, just under the default `linear_dependence_tol` of 1e-5. The function is then filtered
  out, the energy converges, but the DIIS error stays at 6.9e-5 (7.3e-3 without X2C) and the SCF
  runs to its iteration limit. With `linear_dependence_tol= 1e-7` it converges in 20 iterations.
  The convergence test does not allow for the filtered functions; an existing defect, entered in
  `TASKS_AND_HISTORY.md` under *Correctness*.
- **The tests print no iteration table** (`output= NO`). With the `core` guess, which is also what
  `promolecule` gives for a single atom, krypton's guess orbitals come from a nearly degenerate
  shell: the starting energy differs between builds by up to 5e-4 hartree and the last digits of
  the converged gradient with it, though the final energy agrees to 1e-8. The Mac debug build
  failed the two Kr tests against the achari2 reference for that reason until the table was
  left out.

**Open after this.**

1. ~~The heavy-atom test of the default atoms~~ dropped for now, 2026-10-12 (§3, step 5).
2. A first refinement beyond Kr with `x2c-TZVPall` and X2C, and its cost: the transformation is
   one eigenproblem of twice the number of primitives, and the primitive integrals are a serial
   loop over shell pairs and nuclei. If a real structure is slow, do the transformation atom by
   atom (the "DLU" approximation of Peng and Reiher) and run the integral loop in parallel.
3. Done 2026-10-12: `hart --relativity x2c`, and the X2C tests pass under MPI on 2 processes.
4. The atomic density tables near a heavy nucleus (`TASKS_AND_HISTORY.md`, *Correctness*).

## 7. Order of work

1. The spherical-atom study (§3).
2. The core question (§4), and Cartesian against spherical from the reruns.
3. libxc, PBE0, then r2SCAN (§6).
3a. Scalar X2C (§6a), needed before any structure beyond Kr.
4. On the nine structures against neutron: method, basis, environment and partition; choose the
   partition; freeze the protocol and name it.
5. Then the ADP-model studies, rerun under the protocol.

Speed work (SG grids, COSX; `docs/TASK_COSX_GRIDS.md` on branch `sg-grids`) goes on beside this:
it does not change results.

## 8. Log

- 2026-10-12 (later): pull request 48 merged. Decisions (Dylan): the aoc/fon heavy-atom test is
  dropped for now (§3 step 5); `hart` keeps the 4d and 5d metals and is to move to spherical
  functions (a task, with a re-bless); the DFT grid levels are to be tuned to 1e-3, 1e-4 and 1e-5
  hartree for `very_low`, `low` and `medium` (a science task; they are not there now).
- 2026-10-12: X2C merged (pull request 47). The four tasks it left are done on branch
  `heavy-atom-fixes` (`TASKS_AND_HISTORY.md`, archive, *the four tasks left by the X2C work*):
  `hart --relativity x2c` with the x2c basis sets and atoms to Rn; the SCF converges when a basis
  function is filtered for linear dependence; the default DFT grid is good to about 1e-5 hartree
  up to Rn (it was 7.7e-4 for HBr, wrong by 9 hartree for HI, 2.2e-2 for Hg). **Before a heavy
  atom is refined:** the atomic density tables go negative near a heavy nucleus (*Correctness*),
  and a data set with an atom beyond Kr is needed.
- 2026-10-11 (evening): scalar X2C written and checked, branch `x2c` (§6a): 17 systems against
  PySCF to 1e-8 hartree, one-electron ions against a 40-digit solution, Gaussian nuclei. Suite,
  Mac release and achari2 reference: the 184 earlier tests pass and the three new X2C tests
  pass (blessed on achari2); those three also pass in the Mac debug build. The three silent fall-throughs (`relativity_kind= iotc/dkh`, `nucleus_model= finite`) now stop.
- 2026-10-11: libxc, PBE0, r2SCAN and the keyword `dft_functional=` are on branch `libxc`
  (`docs/TASK_DFT_STANDARDISATION.md` §11), in a build with `-DLIBXC=ON`. r2SCAN's energy needs a
  much finer grid than PBE's: at the default `accuracy= medium` its error is 2.3e-4 hartree for
  water against 3e-6 for PBE. **A refinement does not feel it:** on urea the grid moves the GoF by
  0.003 to 0.004 with r2SCAN and 0.005 to 0.007 with PBE, and no bond or U_iso by more than a
  quarter of an esd. The default grid stands. Isolated urea, def2-TZVP, free ADPs: GoF 2.606 with
  PBE and 2.511 with r2SCAN; def2-SVP: 2.672 and 2.700 (B3LYP 2.662). Merged to `develop` by
  pull request 46. Found that no relativistic Hamiltonian runs (§6a);
  decision to go to X2C.

- 2026-10-05: the x2c sets renamed to their literature names (`x2c-SVPall`, `x2c-TZVPall`,
  `x2c-TZVPPall`), after checking all 86 elements of each against the Basis Set Exchange; headers
  rewritten with the reference. Water RHF with `x2c-SVPall`, spherical: -75.955014.
- 2026-10-06: the spherical reruns are in (74 refinements, all converged). Cartesian against
  spherical, urea RHF, isolated molecule: the def2-SVP GoF falls by 0.07–0.13 with spherical
  functions (free ADPs, Hirshfeld: 3.304 to 3.179), def2-TZVP by at most 0.010 (2.935 to 2.925);
  no bond moves by more than one esd. The table of section 1 now quotes the spherical numbers,
  and every urea table of `docs/REPORT_ON_MODE_FITTING.md` is rebuilt from them. New there: B3LYP
  on the def2-SVP 7-molecule cluster (GoF 2.381 free) and def2-TZVP with cluster charges (RHF
  2.620, B3LYP 2.038 -- the best fit so far, a little better than the def2-TZVP cluster's 2.079).
  Runs: `~/tonto-runs/2026-10-05_mode_fitting/cc/sph`, tables by `cc/sph_tables.py`.
- 2026-10-05: document written. Running: the spherical-function reruns of every urea refinement
  (74 jobs) after a four-SCF timing test, see the handover in `TASKS_AND_HISTORY.md`.
