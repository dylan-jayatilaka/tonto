# TASK: Coulomb embedding by the density of the nearest neighbours

Working document, opened 2026-10-24. Branch `j-embedding`, worktree `~/tonto-wt/j-embedding`.
The method, the keywords, the code table, the checks done and the literature are in
[`REPORT_ON_CRYSTAL_EMBEDDING.md`](REPORT_ON_CRYSTAL_EMBEDDING.md); this page keeps the plan, the
log and what is still open.

## Status (2026-11-16)

| stage | state |
|---|---|
| 1. neighbours, rotated density matrices | done, `ed0862d0`; checks in `~/tonto-runs/2026-11-13_j_embedding_stage1` |
| 2. Coulomb integrals, nuclei, removal from the cluster charges, `embedding_radius=` | done, `3ef3dcd0`; checks in `~/tonto-runs/2026-11-13_j_embedding_stage2` |
| 3. update in the self-consistent loop | open; see below |
| 4. refinements against neutron | open |
| fragment HAR | open; needs the TFVC path in fragment HAR first (it segfaults, `~/tonto-runs/2026-11-13_fraghar_tfvc_test`) |

Decided on the way (Dylan, 2026-11-12 to 13): whole molecules only, so the atoms and groups
levels below are not implemented; the source molecule is an argument of the integral routine so
fragment HAR can reuse it; no supermolecule HAR for ion pairs or hydrates (8-hydroxyquinolinium
hydrogen maleate, N-acetyl-L-4-hydroxyproline monohydrate wait for embedded fragment HAR).

Stage 3, the design question: the self-consistent cluster charges are remade at every SCF
iteration (`make_core_mx` is called after each Fock build). Remaking the embedding as often costs
about ten Fock builds per iteration. Proposed: an outer loop inside the SCF. Make the embedding
from the current density, converge the SCF, remake the embedding from the converged density,
continue from the converged orbitals, and stop when the largest change of the embedding matrix
falls below a tolerance. In HAR the outer loop restarts from the previous cycle's density, so
after the first cycle it should take one or two passes.

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

