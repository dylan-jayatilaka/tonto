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

## 4a. What was measured (2026-10-05, branch `atom-scf`)

Atoms in cc-pVDZ, the atomic SCF behind the ANOs (`make_anos`, `guess_output= TRUE`):

| atom | `average` (UHF, then averaged) | `aoc` | difference | expected |
|---|---|---|---|---|
| H | −0.499278 | −0.499278 | 0 | a = 0: the UHF 1s exactly |
| C | −37.686669 | −37.653277 | +0.033 | 3F₂ of p², ≈ 0.03 with HF-sized F₂ |
| N | −54.391354 | −54.282559 | +0.109 | 9F₂ of p³, ≈ 0.11 |
| O | −74.792301 | −74.745756 | +0.047 | 3F₂ of p⁴, ≈ 0.047 |
| F | −99.375328 | −99.371151 | +0.004 | p⁵ has one term: UHF's spin contamination |

(The Slater relations: for p² and p⁴ the configuration average lies 3F₂ above the ³P
term, for p³ 9F₂ above ⁴S, with F₂ the Slater integral F²/25; the experimental ¹D–³P and
²D–⁴S splittings give F₂, and HF overestimates them by about 30 %.) So the average of
configuration is implemented right. `fon` with BLYP: C −37.789, O −74.990, converged in
6 and 10 iterations.

Urea HAR (`tests/long/urea_rhf_STO-3G_HAR` with the basis changed), the test Dylan asked
for before any default moves:

| | R(F) | GoF | cycles | O=C /Å | N–H1 /Å | N–H3 /Å | U_iso H1, H3 /Å² |
|---|---|---|---|---|---|---|---|
| RHF def2-SVP, `average` | 0.0181 | 3.3041 | 5 | 1.2558(4) | 1.028(5) | 0.986(6) | 0.054(4), 0.048(3) |
| RHF def2-SVP, `aoc` | 0.0181 | 3.3069 | 5 | 1.2558(4) | 1.028(5) | 0.986(6) | 0.054(4), 0.048(3) |
| RHF def2-TZVP, `average` | 0.0167 | 2.9352 | 5 | 1.2560(4) | 1.025(4) | 0.989(5) | 0.054(3), 0.046(2) |
| RHF def2-TZVP, `aoc` | 0.0167 | 2.9370 | 5 | 1.2560(4) | 1.025(4) | 0.988(5) | 0.054(3), 0.046(2) |
| BLYP def2-SVP, `average` | 0.0159 | 2.6734 | 22 | 1.2561(3) | 1.013(4) | 0.990(5) | 0.046(3), 0.044(2) |
| BLYP def2-SVP, `fon` | 0.0159 | 2.6643 | 100, not converged | 1.2561(3) | 1.013(4) | 0.991(5) | 0.047(3), 0.045(2) |

- **The ensemble atom changes the HAR by nothing visible**: GoF by 0.002–0.003, no bond
  or ADP by a printed digit. The Hirshfeld weight is a ratio of free-atom densities, and
  the spherical average of the UHF atom and the average-of-configuration atom differ
  too little in that ratio to matter. In STO-3G they do not differ at all (one radial
  function per shell), so `long/urea_rhf_STO-3G_HAR_ANO_aoc` reproduces the Hirshfeld
  test's R and GoF exactly and checks only that the path runs.
- The BLYP refinement with `fon` atoms did not converge in 100 cycles where `average`
  took 22; the parameters it circles are the `average` ones within one esd. A BLYP HAR
  on this job is slow to settle either way (22 cycles against 5 for RHF), which is a
  separate thing to look at.
- Conclusion for the default: there is no numerical reason to change it, and no harm
  in leaving `aoc` available. The scientific point stands — the ensemble atom is the
  stationary object — but on urea it is not a different answer.

## 5. Later

- Two open shells (Cr, Cu, and excited configurations): Roothaan's general coupling.
- The split: this assembler and the J/K builders move to the SCF object together.
