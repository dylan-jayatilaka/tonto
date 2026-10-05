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
| basis, def2-SVP to def2-TZVP | 0.4–0.5 | up to 0.006 Å |
| method, RHF to B3LYP | 0.6–0.8 | about 0.01 Å |
| crystal environment: none, cluster charges, 7-molecule cluster | 0.1–0.2 | 0.01–0.02 Å |
| partition, Hirshfeld or TFVA | about 0.2 | 0.01–0.04 Å |
| ADP model: free, TLS, soft modes | 0.05–0.2 | under 0.01 Å |
| spherical-atom model: rotational average or ensemble | 0.002–0.003 | no printed digit |
| exact exchange or RI-J with COSX | 0.0004 | under 0.001 Å |
| Cartesian or spherical functions | being measured (the spherical reruns of 2026-10-05) | |
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

## 4. Basis sets across the periodic table

- **def2-SVP, def2-TZVP, def2-TZVPP: H to Kr** in `basis_sets/`. Beyond Kr the published def2
  sets replace the core by an effective core potential, which is no use here: X-rays scatter
  from the core.
- **x2c-SVPall, x2c-TZVPall, x2c-TZVPPall: H to Rn**, all-electron, in `basis_sets/` (Pollak and
  Weigend, *J. Chem. Theory Comput.* 13, 3696, 2017; the same family as def2). All 86 elements
  agree with the Basis Set Exchange copies in every exponent and coefficient. They are made for a scalar-relativistic Hamiltonian;
  Tonto has IOTC and DKH (`relativity_kind=`). To be tested: x2c-TZVPall with IOTC against def2-TZVP
  on light atoms, where the two should agree, and on one 4d or 5d compound.
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

## 7. Order of work

1. The spherical-atom study (§3).
2. The core question (§4), and Cartesian against spherical from the reruns.
3. libxc, PBE0, then r2SCAN (§6).
4. On the nine structures against neutron: method, basis, environment and partition; choose the
   partition; freeze the protocol and name it.
5. Then the ADP-model studies, rerun under the protocol.

Speed work (SG grids, COSX; `docs/TASK_COSX_GRIDS.md` on branch `sg-grids`) goes on beside this:
it does not change results.

## 8. Log

- 2026-10-05: the x2c sets renamed to their literature names (`x2c-SVPall`, `x2c-TZVPall`,
  `x2c-TZVPPall`), after checking all 86 elements of each against the Basis Set Exchange; headers
  rewritten with the reference. Water RHF with `x2c-SVPall`, spherical: -75.955014.
- 2026-10-05: document written. Running: the spherical-function reruns of every urea refinement
  (74 jobs) after a four-SCF timing test, see the handover in `TASKS_AND_HISTORY.md`.
