# Plan: spherical atoms by the right ensemble

A working document (see `CLAUDE.md` §1). It plans the atomic SCF that gives a spherical
atom *by construction* — the average of configuration for Hartree–Fock, held fractional
occupations for DFT — in place of the post-facto spherical average of an open-shell
atom's density, and is deleted when the item closes.

## 1. What is wanted, and what is not

Every ANO atom behind the promolecule guess, the Hirshfeld and Salvador partitions and
their HAR form factors comes from an atomic SCF on a one-atom molecule, whose density is
then averaged over rotations (`ATOM:spherically_average`). For an open-shell atom that
density is a *rotational average of one state*, which is not the same thing as the
density of the spherically symmetric ensemble: the energy functional was never
stationary for the averaged density, and for HF the exchange of the averaged density is
not the average of the exchange.

The right objects (Dylan, 2026-10-03/05):

- **HF: the average of configuration** (Slater; Roothaan's open-shell theory with the
  coupling constants of the configuration average). A wavefunction method should use
  the ensemble of determinants, not a fractional density.
- **DFT: fractional occupations held to convergence**, n/g on each spin-orbital of the
  open shell, spin-averaged so the density is spherical in space and spin. The KS
  functional of a fractional density is the ensemble functional, so this is "natural".

Decisions:

- **Off by default.** `ano_kind= average` keeps today's post-facto average; no HAR
  reference moves until the implications are tested in full.
- **No split of the Fock code from `MOLECULE`** now. The two-electron builders already
  take any density and return J and K; the ensemble needs one new *assembler* on top of
  them, not a copy. The split — an SCF object owning the matrices and builders over a
  `VEC{ATOM}` — stays with the re-engineering item, where it is the same job as the
  CRYSTAL hoist. So the one-atom `MOLECULE` inside `make_ANOs_for_atom` stays; what
  changes is that its SCF can never recurse (its guess is the core Hamiltonian, named)
  and that it can be asked for an ensemble.
- The light design must be shaped so the assembler moves unchanged when the split comes.

## 2. The theory, in the form the code uses

Closed shells with density D_c (occupation 2) and one open shell of N_o orbitals holding
n electrons, occupation 2f each with f = n/(2N_o), density D_o. Write G(D) = J(D) − ½K(D)
and E₂[D] = ½ tr[D G(D)].

**Average of configuration (HF).** Averaging the two-electron energy over every
determinant of the configuration gives Roothaan's open-shell form with the coupling
constants a = b = 2N_o(n−1)/(n(2N_o−1)) (a = b = 1 for a closed shell, 0 for one
electron). With D = D_c + D_o,

    E_AOC = tr[D h] + E₂[D] − (1 − a) E₂[D_o],

i.e. the RHF energy of the fractional density minus (1−a) times the open shell's
self-interaction. The two Fock operators are F_c = h + G(D) and F̃_o = F_c − (1−a) G(D_o);
stationarity needs (F_c)_cv = 0, (F̃_o)_ov = 0 and (F_c − f F̃_o)_co = 0. One effective
Fock matrix in the MO basis — cc, cv, vv blocks from F_c; oo, ov blocks from F̃_o; the co
block (F_c − f F̃_o)/(1−f) — is Hermitian, has these conditions as its off-diagonal
blocks, and goes back to the AO basis as S C F_eff Cᵀ S, exactly as `make_ro_Fock_mx`
does for high-spin ROHF. Diagonalising it, DIIS on FDS − SDF, damping and level shift
then all work unchanged. The energy is not ½ tr[D(h + F_eff)]; the difference is kept as
`SCF_DATA.ensemble_energy_correction` and added where the RHF energy is formed.

**Held fractional occupations (DFT).** D = D_c + D_o with the same f, and the ordinary
KS Fock and energy of D. Nothing but the density changes.

The open shell is the Aufbau one: the orbitals N_c+1 … N_c+N_o in energy order after
each diagonalisation, with N_c = (n_e − n)/2. For the first three rows that is the
valence s or p shell; for Sc–Zn the 3d shell under 4s². Cr and Cu (3d⁵4s¹, 3d¹⁰4s¹) have
two open shells and fall back to the post-facto average, with a note.

## 3. The pieces, and where they go

- **`ATOM`**: `ground_state_open_shell(Z, l, n)` — the Aufbau open shell for Z ≤ 36
  (l = −1 where the rule does not give one open shell); `AOC_coupling(N_o, n)`.
- **`SCF_DATA`**: `ANO_kind` (`ano_kind= average | aoc | fon`), and the ensemble in
  force — `using_open_shell_ensemble`, `n_open_orbitals`, `n_open_electrons`,
  `AOC_coupling`, `ensemble_energy_correction`.
- **`MOLECULE.SCF:make_ANOs_for_atom`**: after the one-atom molecule is made, if
  `ano_kind` is not `average` and the atom has one open shell, set the SCF kind (`rhf`
  for `aoc`; `rks` with the parent's functional for `fon`, which needs a DFT parent), the
  ensemble, no delta build; the atomic SCF then runs as it does now, and the result is
  still spherically averaged afterwards (a no-op on an ensemble density, kept for the
  closed-shell and fall-back cases).
- **`MOLECULE.BASE:make_SCF_density_mx`**: in the restricted branch, when the ensemble is
  on, D = 2 Σ_closed + 2f Σ_open from the current MOs (`make_ensemble_r_density_mx`),
  and the singlet check is skipped (an open-shell atom keeps its own multiplicity, which
  the ensemble density does not use).
- **`MOLECULE.FOCK:make_AOC_effective_Fock_mx`**: called at the end of `make_scf_Fock_mx`
  when the ensemble is on and the kind is HF: forms D_o from the MOs, G(D_o) by a second
  two-electron build, F̃_o, the effective Fock matrix, and the energy correction.
- **`MOLECULE.SCF:SCF_electronic_energy`**: the correction added for `rhf` and `rks`.
- The "Table of atomic SCF calculation" gains the atomic energy, so a job shows what
  the ensemble gave.

## 4. Checks and tests

- H (s¹): a = 0, so the AOC atom is the bare-nucleus 1s — the exact HF hydrogen.
  Closed shells: the ensemble is plain RHF/RKS, same energy.
- C, N, O in cc-pVDZ: E_AOC against the literature average-of-configuration energies
  (Fischer), and above the high-spin ROHF energy of the ground term.
- `fon` with BLYP on the same atoms: converges; density spherical; energy above the
  spin-polarised ground state.
- Tests: `short/carbon_atom_uhf_cc-pVDZ_ANO_aoc` and `short/oxygen_atom_uks_BLYP_ANO_fon`
  (a one-atom job with `guess_output= TRUE`, so the atomic SCF is in the stdout), blessed
  on achari2.
- **Urea HAR, `ano_kind= aoc` and `fon` against `average`**, def2-SVP and def2-TZVP, same
  table as the research document's: this is the test of the implications Dylan asked for
  before any default changes.

## 5. Later

- Two open shells (Cr, Cu, and excited configurations): Roothaan's general coupling.
- The split: this assembler and the J/K builders move to the SCF object together.
