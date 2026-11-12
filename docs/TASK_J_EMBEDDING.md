# TASK: Coulomb embedding by the density of the nearest neighbours

Working document, opened 2026-10-24. No branch yet.

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

The core matrix of the central molecule gains one term,

```math
F_{\mu\nu} = h_{\mu\nu} + V^{\mathrm{cc}}_{\mu\nu} + V^{\mathrm{emb}}_{\mu\nu} + G_{\mu\nu}[P] \qquad (1)
```

where $`\mu,\nu`$ label basis functions of the central molecule, $`h`$ is the usual core
matrix, $`V^{\mathrm{cc}}`$ the cluster-charge matrix, $`G[P]`$ the two-electron part from the
molecule's own density matrix $`P`$, and

```math
V^{\mathrm{emb}}_{\mu\nu} = \sum_{n} \Big[ \sum_{\lambda\sigma \in n} (\mu\nu|\lambda\sigma)\, P^{(n)}_{\lambda\sigma}
 \;-\; \sum_{A \in n} Z_A \,\langle \mu | \, |\mathbf r - \mathbf R_A|^{-1} | \nu \rangle \Big] \qquad (2)
```

Here $`n`$ runs over the near fragments, $`\lambda,\sigma`$ over the basis functions placed on
the atoms of fragment $`n`$, $`(\mu\nu|\lambda\sigma)`$ is the two-electron Coulomb integral,
$`Z_A`$ and $`\mathbf R_A`$ are the charge and position of nucleus $`A`$ of the fragment, and
$`P^{(n)}`$ is the fragment's density matrix:

```math
P^{(n)} = T(R_n)\, P_{nn}\, T(R_n)^{\mathsf T} \qquad (3)
```

where $`P_{nn}`$ is the block of $`P`$ for the parent atoms of the fragment, and $`T(R_n)`$ is
the block-diagonal matrix that rotates each shell of basis functions by the rotation part
$`R_n`$ of the symmetry operation that makes the fragment from its parent atoms. The
translation part only moves the shell centres.

Since $`P^{(n)}`$ is held fixed during an SCF, $`V^{\mathrm{emb}}`$ is a one-electron operator
for the central molecule and is added to the core matrix, beside $`V^{\mathrm{cc}}`$. It is
remade from the latest $`P`$ wherever the self-consistent cluster charges are remade.

The atoms of the near fragments are removed from the cluster charges, so no atom is counted twice.

### Three levels of fragment

| level | fragment | net charge of the fragment |
|---|---|---|
| atoms | each hydrogen-bond partner atom, diagonal block of $`P`$ only | Mulliken charge: basis-dependent, not what the cluster charges use |
| groups | the donor or acceptor group (O–H, N–H, C=O), with the off-diagonal blocks inside it | Mulliken charge of the group |
| molecules | every molecule with an atom within a set distance of the central molecule | zero |

Whole molecules are the clean choice (neutral, no cut through bonds); atoms and groups are the
cheap ones. Which level is enough is the main question this task answers.

### Cost

For $`N`$ basis functions per molecule and $`m`$ neighbour molecules, the integrals of
Eq. (2) number about $`\tfrac12 N^2 \times \tfrac12 m N^2`$ before screening, about
$`1.5\,N^4`$ for $`m = 6`$, against $`\tfrac18 N^4`$ for one Fock build of the molecule and
$`\tfrac18 (7N)^4 \approx 300\,N^4`$ per iteration for the explicit 7-molecule cluster. They are
made once per update of $`P^{(n)}`$, not once per SCF iteration. Schwarz screening removes most of
them, since most neighbour functions are far from most central ones. Expect the cost of a few
extra SCF iterations; measure it.

## Where it goes in the code

- **Core matrix:** `molecule.scf.foo`, the routine that makes `.core_mx`, beside
  `make_cluster_charge_mx(Z)`. Same shape: create a matrix, fill it, add it, destroy it.
- **The neighbours:** `CLUSTER` made `within_radius` with `defragment` FALSE for atoms and
  groups, TRUE for whole molecules. `make_non_fragment_mu_charges` already maps each cluster atom
  to its parent atom and symmetry operation (it rotates the atomic dipoles); reuse that mapping.
  `molecule_for_atom` gives the whole molecules.
- **Rotating the block:** `rotate_density_mx_r` in `molecule.base.foo` rotates a density matrix
  shell by shell with `GAUSSIAN_DATA:make_gaussian_xyz_matrices`; the fragment version applies
  the same matrices to the block $`P_{nn}`$ only.
- **The integrals:** a new `make_r_J_embedding(V)` in `molecule.fock.foo`, a trimmed copy of
  `make_r_J_nosym`: the outer loop over the central molecule's shell pairs, the inner loop over the
  fragment's shell pairs (shells copied from the parent atoms, placed at the fragment atom
  positions), no exchange, no permutational factors between the two sets. `SHELL1QUARTET` takes
  shells at any centre.
- **The nuclei:** the point-charge matrix code of `make_cluster_charge_mx`, with charges $`Z_A`$.
- **Keywords** in `scfdata= { }`, names to be settled: `embedding_fragments= none | atoms |
  groups | molecules` (default `none`), `embedding_radius=` (distance from any atom of the
  molecule, default perhaps 3 Å for atoms and groups).

Mind the module-use cycles among the MOLECULE submodules (`docs/TASK_MTZ_READER.md`, the
`molecule.fock` episode): put the new builder in FOCK and call it from SCF, which already uses FOCK.

## Checks

1. **Exactness.** Make the explicit urea cluster's density matrix from rotated and translated
   copies of the central $`P`$, and its Coulomb matrix with the ordinary builder on the cluster.
   Its central block must equal $`J[P] + V^{\mathrm{emb}}`$ (whole molecules) to integral
   accuracy. This tests the mapping, the rotation and the integrals at once.
2. **Far limit.** For a fragment moved far away, $`V^{\mathrm{emb}}`$ must approach the field of
   its cluster charges.
3. **Rotation.** A space group with rotations (L-alanine, $`P2_12_12_1`$), not only translations.

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

With whole-molecule fragments the next step is a complete crystal SCF in the manner of Shukla,
Dolg, Fulde and Stoll (Chem. Phys. Lett. 262, 213 (1996); Phys. Rev. B 57, 1471 (1998); in
`~/Dropbox/manuscripts/`): the neighbours' exchange (exact integrals or COSX for a hybrid or HF;
the exchange-correlation of the total density on the grid for DFT) and orthogonality of the
central orbitals to the neighbours' occupied orbitals. Shukla imposes orthogonality by a
level-shift projector, $`\lambda \sum_k |\gamma_k\rangle\langle\gamma_k|`$ over the
neighbours' occupied orbitals with $`\lambda`$ of $`10^3`$ to $`10^4`$ hartree, and adds basis
functions on the neighbours to carry the orthogonalisation tails. The alternative is to project
the neighbours' occupied orbitals out of the central basis functions, which gives the tails
directly but makes some functions nearly dependent, to be removed by filtering small eigenvalues
of the overlap.

**Read Manby, Stella, Goodpaster and Miller, J. Chem. Theory Comput. 8, 2564 (2012) first**
(`Manby_2012_JCTC_8_p2564-8.pdf`), and Goodpaster et al., J. Chem. Phys. 140, 18A507 (2014), on
the basis. Their projection-based embedding is Shukla's projector,
$`f^A = h + J[\gamma_A+\gamma_B] + v_{xc}[\gamma_A+\gamma_B] + \mu P^B`$, with the practical
choices worked out, including a correction that removes nearly all dependence on $`\mu`$.
Dylan's view: if they have worked out the projection step, it is better than eigenvalue filtering.
