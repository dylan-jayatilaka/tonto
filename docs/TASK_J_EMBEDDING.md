# TASK: Coulomb embedding by the density of the nearest neighbours

Working document, opened 2026-10-24. Branch `j-embedding`, worktree `~/tonto-wt/j-embedding`.
The method, the keywords, the code table, the checks done and the literature are in
[`REPORT_ON_CRYSTAL_EMBEDDING.md`](REPORT_ON_CRYSTAL_EMBEDDING.md); this page keeps the plan, the
log and what is still open.

## Status (2026-11-17)

| stage | state |
|---|---|
| 1. neighbours, rotated density matrices | done, `ed0862d0`; checks in `~/tonto-runs/2026-11-13_j_embedding_stage1` |
| 2. Coulomb integrals, nuclei, removal from the cluster charges, `embedding_radius=` | done, `3ef3dcd0`; checks in `~/tonto-runs/2026-11-13_j_embedding_stage2` |
| 2b. Schwarz screening, `use_embedding_schwarz_test=` | done, `eaa20647`, `1561bbbc`; `~/tonto-runs/2026-11-13_j_embedding_schwarz` |
| 3. update in the self-consistent loop | planned below, in progress |
| 4. refinements against neutron | open |
| fragment HAR | open; needs the TFVC path in fragment HAR first (it segfaults, `~/tonto-runs/2026-11-13_fraghar_tfvc_test`) |

Decided on the way (Dylan, 2026-11-12 to 13): whole molecules only, so the atoms and groups
levels below are not implemented; the source molecule is an argument of the integral routine so
fragment HAR can reuse it; no supermolecule HAR for ion pairs or hydrates (8-hydroxyquinolinium
hydrogen maleate, N-acetyl-L-4-hydroxyproline monohydrate wait for embedded fragment HAR).

## Stage 3: a self-consistent embedding

**What is wrong now (confirmed 2026-11-17).** `make_core_mx` builds `.embedding_mx` only if it
does not exist, and nothing destroys it. A probe line showed it is built at SCF iteration 0, so
from the starting density: the promolecule with `initial_density= promolecule`. It is then kept
for the rest of that SCF and for every later SCF of a HAR, even after the atoms move. On urea
(RHF/def2-SVP, 3.5 Å, cluster charges to 8 Å) the stored matrix gives
$`\mathrm{tr}\,(V P) = -0.65330`$ with the final density, against $`-0.65823`$ for the matrix made from
the final density: 4.9 mEh, 0.75 %. Run in `~/tonto-runs/2026-11-13_j_embedding_schwarz/urea_probe`.
The cluster charges, by contrast, are remade from the current density at every Fock build.

**Aim.** At convergence the neighbours' density must be the converged density of the molecule,
rotated, to a stated tolerance, just as the cluster charges are the converged charges and
dipoles. And each SCF must start again from the atoms where they now are.

**Design: a full rebuild whenever the density has moved by more than a tolerance.** Keep, with
the embedding matrix, the density matrix $`P_{\mathrm{emb}}`$ it was made from. At every Fock
build (which with self-consistent cluster charges already remakes the core matrix), rebuild the
embedding when

```math
\max_{\mu\nu} |P_{\mu\nu} - P_{\mathrm{emb},\mu\nu}| > \tau \qquad (1)
```

and set $`P_{\mathrm{emb}} = P`$. Here $`P`$ is the current density matrix and $`\tau`$ the new
keyword `embedding_update_tolerance=`. The SCF needs no extra convergence test: a rebuild changes
the core matrix, so the next iteration's energy and density change and the SCF goes on; when it
stops, the last density used for the embedding is within $`\tau`$ of the final one.

At the start of every SCF the embedding matrix and $`P_{\mathrm{emb}}`$ are destroyed, so the first
Fock build makes them afresh at the present atom positions. In a HAR this first build uses the
previous cycle's converged density, which is close, so later cycles should need one or two
rebuilds; the first SCF, from the promolecule, perhaps five to eight.

**Rejected: an incremental update** $`V[P] = V[P_{\mathrm{emb}}] + V[P - P_{\mathrm{emb}}]`$ every
iteration, screened by the small change. It is cheaper per iteration, but the same idea for the
molecule's own Fock matrix (`using_delta_build`) was switched off because the increments are noisy
at the $`10^{-6}`$ level and cost iterations (`docs/TASK_SCF_SPEEDUP_DATA.md`).
**Rejected: an outer loop** (converge, rebuild, reconverge). It needs a second convergence loop
round the SCF, and does the same rebuilds as (1) but with each one waiting for a full convergence.

**The default for** $`\tau`$ is to be chosen by measurement, not assumed: run urea and L-alanine at
$`\tau = 10^{-2}, 10^{-3}, 10^{-4}, 10^{-5}`$ and tabulate the number of rebuilds, the time and the
error of the final embedding, $`\mathrm{tr}\,(V_{\mathrm{stored}} P) - \sum_n (E_J + E_N)`$, the
second term from `put_embedding_neighbours`. Choose the largest $`\tau`$ that keeps that error
below the SCF's own energy convergence. My guess is $`10^{-4}`$.

**Code.**

| piece | where |
|---|---|
| `embedding_density :: MAT{REAL}@`, the $`P_{\mathrm{emb}}`$ of (1) | `types.foo`, MOLECULE, next to `embedding_mx` |
| `embedding_update_tolerance :: REAL`, the $`\tau`$ of (1), and its keyword | `types.foo`, SCF_DATA; `scf_data.foo` |
| `n_embedding_builds :: INT`, counted per SCF and printed with the SCF results | `types.foo`, SCF_DATA |
| the test (1) and the rebuild, in place of "build once" | `MOLECULE.SCF:make_core_mx` |
| destroy both at the start of each SCF | `MOLECULE.SCF:scf` (the place to be found exactly) |
| print the error of the final embedding | `MOLECULE.SCF:put_embedding_neighbours` |

**Checks.**

1. The probe comparison above: with the update, the stored matrix must give the same
   $`\mathrm{tr}\,(V P)`$ as one made from the final density, to within the error that $`\tau`$ allows.
2. Consistency: $`\tau = 0`$ (a rebuild at every Fock build) and the chosen default must give the
   same converged energy and density to the SCF convergence.
3. A moved atom: two SCFs in one job with an atom moved between them; the second must rebuild
   from the new positions (its neighbours table must change).
4. A short urea HAR with the embedding, TFVC, B3LYP/def2-SVP: it must converge, and the number of
   rebuilds per cycle and the time per cycle are recorded.
5. The short test suite, which should not change, since the embedding is off by default.

The old checks 1 and 2 below (the explicit cluster, the far limit) remain to do after this.

**A note on the energy.** With neighbours that are copies of the molecule, the SCF energy counts
the whole interaction $`\mathrm{tr}\,(V^{\mathrm{emb}} P)`$, twice the share of one molecule in the
pair interaction of a lattice energy. HAR uses only the density, so this does not matter for
refinement, but the printed energy is not a lattice energy.

Open for fragment HAR: `ERI_SCRATCH:set_sizes_for` is called once for each molecule's own shell
pairs, which covers src = self but not a mixed quartet whose bra and ket come from two bases with
different largest pairs; size for the mixed quartets before reusing `add_r_embedding_mx`.

## Goal

Give the molecule in a HAR the Coulomb field of its nearest neighbours as they actually are, a
smeared density plus nuclei, and not as point charges. The neighbours are symmetry copies of
the molecule itself, so their density comes from its own density matrix, and it updates as the
SCF proceeds. The far field stays with the cluster charges. There is no exchange and no Pauli
repulsion in this task; they are the subject of the last section.

## Why

- Brux et al. (2025), Struct. Dyn. 12, 054101, improve HAR of amino acids with ORCA's CPCM
  continuum (water). A continuum has no direction and its cavity radii are fitted to hydration
  free energies, not to the polarisation of the density; Dylan judges it too rough.
- Cluster charges (`use_SC_cluster_charges`, 8 Å) lower the GoF of urea by about 10 % and match
  the explicit 7-molecule cluster at about 1/40 of its cost (`~/tonto-runs/2026-10-22_urea_cluster`,
  `2026-10-23_soft_walls`). What they miss is short range: at a hydrogen bond, 1.8 to 2.0 Å,
  the field of a smeared density with its nucleus differs from that of a point charge and
  dipole, because the two densities overlap (penetration).
- The explicit cluster gets this right but runs the SCF of every molecule. The embedding needs
  the SCF of the central molecule only.

## The method

In [`REPORT_ON_CRYSTAL_EMBEDDING.md`](REPORT_ON_CRYSTAL_EMBEDDING.md).

## Where it goes in the code

In [`REPORT_ON_CRYSTAL_EMBEDDING.md`](REPORT_ON_CRYSTAL_EMBEDDING.md).

## Checks

1. **Exactness.** Make the explicit urea cluster's density matrix from rotated and translated
   copies of the central $`P`$, and its Coulomb matrix with the ordinary builder on the cluster.
   Its central block must equal $`J[P] + V^{\mathrm{emb}}`$ (whole molecules) to integral
   accuracy. This tests the mapping, the rotation and the integrals at once.
2. **Far limit.** For a fragment moved far away, $`V^{\mathrm{emb}}`$ must approach the field of
   its cluster charges.
3. **Rotation.** A space group with rotations (L-alanine, $`P2_12_12_1`$), not only translations.

Done instead of check 1 so far: the rotated density against the molecule's at carried-back
points, the nuclear term against the molecule's potential at the neighbour's nuclei (exact), and
the electronic term against the same interaction on the DFT grid ($`2\times10^{-6}`$), on urea and
L-alanine; check 3 is covered by L-alanine. Checks 1 and 2 are still to do.

## Tests against experiment

Urea and L-alanine, cluster charges as in the benchmark, B3LYP and r2SCAN, def2-SVP and def2-TZVP,
TFVC and TPFVC at the benchmark's wall sharpness, five ways:

| surroundings | |
|---|---|
| isolated | from the 2026-10-23 batches |
| cluster charges | from the 2026-10-23 batches |
| cluster charges + partner atoms | new |
| cluster charges + partner groups | new |
| cluster charges + whole neighbour molecules | new |
| explicit cluster | `2026-10-22_urea_cluster`, L-alanine 9 molecules |

Compare GoF, bonds to hydrogen and hydrogen S12 against neutron, per wavefunction (never pooled),
and against the explicit cluster. Then xylitol and Gly–Ala if the gain is clear.

## Stages and effort

1. Fragment selection, shell copies and the rotated blocks; check 1 by hand on urea: 1 day.
2. `make_r_J_embedding`, the nuclei, removal from the cluster charges, keywords: 1 day.
3. The update in the self-consistent loop; checks 2 and 3; a short test: 1 day.
4. The refinements above, on achari2 or the Mac overnight.

## Later: exchange and orthogonality

In [`REPORT_ON_CRYSTAL_EMBEDDING.md`](REPORT_ON_CRYSTAL_EMBEDDING.md).

