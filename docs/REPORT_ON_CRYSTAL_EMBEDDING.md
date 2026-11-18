# Embedding the molecule of a HAR in its crystal

In a Hirshfeld atom refinement the wavefunction is made for one molecule. The crystal around it
enters as self-consistent point charges and dipoles on the atoms of the molecules within a set
radius (`use_SC_cluster_charges=`, `cluster_radius=`). Point charges give the far field well. At a
hydrogen bond, 1.8 to 2.0 Å from the molecule, they do not: there the neighbour's density overlaps
the molecule's, and its field is not that of a point charge and dipole. This page describes the
Coulomb embedding, which replaces the charges of the nearest neighbours by their density and
nuclei, and the later step, which adds their exchange and keeps the molecule's orbitals
orthogonal to theirs.

Units are atomic units; vectors are columns; ᵀ is the transpose.


## 1. The Coulomb embedding

The Fock matrix of the molecule gains one term,

```math
F_{\mu\nu} = h_{\mu\nu} + V^{\mathrm{cc}}_{\mu\nu} + V^{\mathrm{emb}}_{\mu\nu} + G_{\mu\nu}[P] \qquad (1)
```

where $`\mu,\nu`$ label the basis functions of the molecule, $`h`$ is the core matrix,
$`V^{\mathrm{cc}}`$ the matrix of the cluster charges, $`P`$ the molecule's density matrix and
$`G[P]`$ its two-electron part. The embedding matrix is

```math
V^{\mathrm{emb}}_{\mu\nu} = \sum_{n} \Big[ \sum_{\lambda\sigma \in n} (\mu\nu|\lambda\sigma)\, P^{(n)}_{\lambda\sigma}
 \;-\; \sum_{A \in n} Z_A \,\langle \mu | \, |\mathbf{r} - \mathbf{y}_A|^{-1} | \nu \rangle \Big] \qquad (2)
```

Here $`n`$ runs over the neighbour molecules, $`\lambda,\sigma`$ over the basis functions on the
atoms of neighbour $`n`$, $`(\mu\nu|\lambda\sigma)`$ is the two-electron Coulomb integral, and
$`Z_A`$ and $`\mathbf{y}_A`$ are the charge and position of nucleus $`A`$ of the neighbour.

**The neighbours** are the symmetry copies of the molecule, other than the molecule itself, with
an atom within the distance $`r_{\mathrm{emb}}`$ (`embedding_radius=`) of an atom of the
molecule. The selection follows the shape of the molecule: it is a shell of roughly constant
thickness around it. A copy that several symmetry operations make, as for a molecule on a special
position, is taken once. Copy $`n`$ is made by a rotation $`R_n`$ and a shift $`\mathbf{t}_n`$,
both Cartesian, and its atom $`a`$ is at

```math
\mathbf{y}_a = R_n\, \mathbf{x}_a + \mathbf{t}_n \qquad (3)
```

with $`\mathbf{x}_a`$ the position of atom $`a`$ of the molecule, so its atoms are in the
molecule's order.

**Their density** is the molecule's own, carried into place:

```math
P^{(n)} = T(R_n)\, P\, T(R_n)^{\mathsf T} \qquad (4)
```

where $`T(R_n)`$ is block diagonal, each block rotating the Cartesian functions of one shell by
$`R_n`$. The shift moves only the centres of the shells.

**The cluster charges** of the atoms of the neighbours are removed, so no atom enters twice. The
neighbours are neutral molecules, so the charges that remain carry the far field as before.

$`V^{\mathrm{emb}}`$ is added to the core matrix. It is a Coulomb operator only: the neighbours'
electrons repel the molecule's, but there is no exchange with them and nothing stops the
molecule's orbitals from spreading into their region. Exchange and orthogonality are §3.

**Self-consistency.** The neighbours' density follows the molecule's as the SCF proceeds, as
the cluster charges do. With $`V^{\mathrm{emb}}`$ is kept the density matrix $`P_{\mathrm{emb}}`$ it
was made from, and at each Fock build $`V^{\mathrm{emb}}`$ is made again from the current $`P`$ when
any element of $`P - P_{\mathrm{emb}}`$ exceeds $`\tau`$ (`embedding_update_tolerance=`) in size.
When the SCF stops, the neighbours' density is the converged one to within $`\tau`$. Each SCF
starts with a fresh build, so in a refinement the neighbours follow the atoms as they move.

| $`\tau`$ | builds, urea | builds, L-alanine | error, urea / Eh | error, L-alanine / Eh |
|---|---|---|---|---|
| 0 (every Fock build) | 13 | 15 | 0 | 0 |
| $`10^{-5}`$ (default) | 10 | 11 | $`2\times10^{-6}`$ | $`7\times10^{-6}`$ |
| $`10^{-4}`$ | 8 | 9 | $`4\times10^{-5}`$ | $`9\times10^{-5}`$ |
| $`10^{-3}`$ | 7 | 8 | $`4\times10^{-5}`$ | $`3\times10^{-4}`$ |
| $`10^{-2}`$ | 5 | 6 | $`3\times10^{-4}`$ | $`2\times10^{-3}`$ |

The error is $`\mathrm{tr}\,(V^{\mathrm{emb}} P)`$ of the stored matrix less that of one made from the
converged density; RHF/def2-SVP from the promolecule, $`r_{\mathrm{emb}}`$ = 3.5 Å, cluster charges
to 8 Å, SCF converged to $`10^{-6}`$ Eh.

**Cost.** For $`N`$ basis functions per molecule and $`m`$ neighbours there are about
$`\tfrac{1}{2}N^2 \times \tfrac{1}{2}mN^2`$ integrals, against $`\tfrac{1}{8}N^4`$ for one Fock build
of the molecule: about twelve Fock builds for $`m = 6`$, before screening.

**Screening.** An integral block is skipped by the Schwarz test when

```math
\sqrt{(\mu\nu|\mu\nu)}\;\sqrt{(\lambda\sigma|\lambda\sigma)}\;\max\,|P^{(n)}_{\lambda\sigma} + P^{(n)}_{\sigma\lambda}| < \epsilon \qquad (5)
```

with the maximum taken over the functions of the shell pair, and $`\epsilon`$ the cutoff
`ERI_Schwarz_cutoff=` ($`10^{-9}`$). The bounds $`(\lambda\sigma|\lambda\sigma)`$ are made for each
neighbour at its own atom positions, because a rotation mixes the functions of a p or d shell and
the molecule's bounds do not bound the rotated ones. The test removes pairs whose functions barely
overlap; it does not become stronger as a neighbour moves away, since the Coulomb interaction of
two distant pairs falls off only as the inverse distance. `use_embedding_schwarz_test= FALSE`
computes every integral.


## 2. Fragment HAR

In fragment HAR the asymmetric unit holds several molecules or ions, the fragments, each with
its own SCF (`kind= fragment-rhf`, `atom_groups=`, `fraghar_refinement`). With
`embedding_radius=` or cluster charges in `scfdata=`, each fragment has its surroundings in the
crystal. The Fock matrix of fragment $`g`$ is (1) with

- $`V^{\mathrm{emb}}`$ summed over the copies of every fragment $`h`$, its own and the others,
  with an atom within $`r_{\mathrm{emb}}`$ of an atom of $`g`$, the fragment $`g`$ itself left
  out. Another fragment of the same asymmetric unit is a neighbour by the identity operation. In
  (2) the basis functions $`\lambda,\sigma`$, the nuclei and the density matrix are those of
  fragment $`h`$, rotated by (4) with the shells of $`h`$;
- $`V^{\mathrm{cc}}`$ from the charges and dipoles of every other atom of the crystal within
  `cluster_radius=` of $`g`$, each atom with the charge and dipole it has in its own fragment.

Either part can be used alone.

**Self-consistency.** The surroundings of a fragment depend on the densities of all the
fragments, so the fragment SCFs are repeated in passes. Let $`\Delta_h`$ be the largest change
of a density matrix element of fragment $`h`$ in one of its SCFs. Fragment $`g`$ has its
surroundings made again, and its SCF repeated, when for some fragment $`h`$, $`g`$ included,

```math
\sum \Delta_h > \tau \qquad (6)
```

the sum being over the SCFs of $`h`$ since the surroundings of $`g`$ were last made, and $`\tau`$
being `embedding_update_tolerance=`. The passes end when no fragment needs repeating. Then every
fragment has surroundings made from densities within $`\tau`$ of the final ones. The file
`<name>.fragment_SCF_passes` lists, for each pass, the number of fragment SCFs done and the
largest $`\Delta_h`$. In a refinement the passes of each cycle start from the densities of the
cycle before.

| crystal, RHF | fragments | surroundings | passes from the promolecule | passes in the later refinement cycles |
|---|---|---|---|---|
| ammonia, DZP | 1 | density to 3.5 Å, charges to 8 Å | 5 | 4, 3, 2, 2, 1 |
| N-acetyl-L-4-hydroxyproline monohydrate, def2-SVP | 2 | charges to 8 Å | 7 | 6, 4, 4, 3, 2, 1 |
| N-acetyl-L-4-hydroxyproline monohydrate, def2-SVP | 2 | density to 3.5 Å, charges to 8 Å | 7 | 6, 4, 3, 2, 2 |
| 8-hydroxyquinolinium hydrogen maleate, def2-SVP | 2 ions | density to 3.5 Å, charges to 8 Å | 8 | 6, 4, 3, 2, 1 |

Each pass makes one embedding matrix for each fragment, which is most of its cost. A refinement
with the embedding takes about 2.7 times as long as one with cluster charges alone.

**Limits.** Fragments with capping atoms (`use_Ryde_capping=`, the nearest-neighbour fragments
of a network solid) have no surroundings: the job stops. So does a parallel run. The fragment
SCFs must be restricted.


## 3. Exchange and orthogonality

With whole molecules as neighbours, the next step is an SCF of the molecule in the full field of
its neighbours: their Coulomb field, their exchange, and orthogonality of the molecule's occupied
orbitals to theirs. Shukla, Dolg, Stoll and Fulde (1996) gave this for crystals. The Fock operator
of the central cluster contains the Coulomb and exchange operators of the environment, and a
projection operator

```math
\hat{P}^{\mathrm{env}} = \lambda \sum_{k} |\gamma_k\rangle\langle\gamma_k| \qquad (7)
```

over the occupied orbitals $`\gamma_k`$ of the environment, with $`\lambda`$ of $`10^3`$ to
$`10^4`$ hartree, which pushes any component of the cluster's orbitals along the environment's
occupied orbitals far up in energy. Functions placed on the environment atoms carry the tails
that orthogonality adds to the cluster's orbitals.

Manby, Stella, Goodpaster and Miller (2012) use the same projector in their projection-based
embedding,

```math
\hat{f}^{A} = \hat{h} + \hat{J}[\gamma^A + \gamma^B] + \hat{v}_{\mathrm{xc}}[\gamma^A + \gamma^B]
 + \mu\, \hat{P}^{B} \qquad (8)
```

where $`\gamma^A`$ and $`\gamma^B`$ are the density matrices of the embedded subsystem and its
environment, $`\hat{J}`$ the Coulomb operator, $`\hat{v}_{\mathrm{xc}}`$ the exchange-correlation
potential of the total density, and $`\hat{P}^{B}`$ the projector onto the environment's occupied
orbitals. They give a first-order correction that removes nearly all of the dependence on
$`\mu`$, and Goodpaster et al. (2014) treat the truncation of the basis. Huzinaga's operator
(Huzinaga and Cantu 1971) is the parameter-free alternative. For a hybrid functional or
Hartree–Fock the exchange part of (8) is the exact exchange with the neighbours, by integrals or
by COSX.

The projector keeps the whole basis. The alternative, projecting the neighbours' occupied
orbitals out of the molecule's basis functions, gives the orthogonality tails directly but makes
some of the projected functions nearly linearly dependent, and they must be removed by filtering
small eigenvalues of the overlap matrix. The level shift of (7) removes nothing: components along
the neighbours' orbitals are only raised in energy, and the error of a finite shift is what the
first-order correction of Manby et al. estimates.


## 4. What has been done elsewhere

| subject | what exists |
|---|---|
| Embedded-cluster Hartree–Fock of crystals with exchange and projection | Shukla, Dolg, Stoll and Fulde (1996), lithium hydride; Shukla, Dolg, Fulde and Stoll (1998), LiF and LiCl, through Wannier functions; Shukla, Dolg and Stoll (1998), polyethylene; correlated calculations on these wavefunctions (Shukla et al. 1999). Ionic and covalent solids, not molecular crystals |
| Correlation in solids by increments | Stoll (1992), diamond; Stoll, Paulus and Fulde (2005) |
| Embedded-cluster codes for solids | EMBED (Pisani and coworkers) |
| Projection-based embedding | Manby et al. (2012); Goodpaster et al. (2014); review by Lee, Welborn, Manby and Miller (2019). Periodic environments: Libisch, Marsman, Burgdörfer and Kresse (2017); Chulhai and Goodpaster (2018), with the Huzinaga operator; applications to surfaces and bulk solids, not to molecular crystals |
| Frozen-density embedding | Wesolowski and Warshel (1993); periodic subsystem DFT, Genova, Ceresoli and Pavanello (2014), and the eQE code (2017), applied to a pentacene supercell; a frozen periodic host at the cost of a gas-phase calculation, Sharma and Sierka (arXiv 2026), on surfaces and solvents |
| Molecular crystals in fields of self-consistent charges | QM/MM with self-consistent electrostatic embedding, Bjornsson and Bühl (2012); monomers and dimers in self-consistent atomic charges and dipoles for MP2 and beyond, Hirata (2008), solid formic acid |
| The crystal in HAR | Self-consistent cluster charges and dipoles (Jayatilaka and Dittrich 2008; Capelli et al. 2014); a cluster of multipoles (Chodkiewicz, Woińska and Woźniak 2020); the neighbours as frozen extremely localized molecular orbitals, QM/ELMO (Wieduwilt, Macetti and Genoni 2021); ESP-derived charges in one or more layers (Landeros-Rivera and Kleemiss 2026); explicit clusters of molecules |
| Periodic wavefunctions in HAR | Projector-augmented waves (Ruth, Herbst-Irmer and Stalke 2022); a periodic Gaussian-basis calculation with CRYSTAL (Chu, Jayatilaka et al. 2026) |

Of the HAR schemes, QM/ELMO is the closest to §3: the neighbours enter as frozen localized
orbitals, with their Coulomb and exchange fields and orthogonality to them. It needs ELMOs
transferred from a library or made for each neighbour. The Coulomb embedding of §1 takes the
neighbours' density from the molecule's own density matrix, so it needs nothing else and stays
self-consistent with the molecule. A Coulomb embedding of this kind, with the neighbours' density
rotated from the molecule's own, has not been used in HAR or, as far as we know, for molecular
crystals elsewhere; nor has projection-based embedding in the sense of (7) and (8).

Manby et al. (2012) cite Stoll's incremental scheme and Huzinaga's operator, but not the work of
Shukla, Dolg, Stoll and Fulde, which used the same level-shift projector sixteen years earlier.


## 5. Keywords

In `scfdata=`:

| keyword | default | meaning |
|---|---|---|
| `embedding_radius=` | 0 (off) | the neighbours of §1 are the copies with an atom within this distance of an atom of the molecule; takes units, e.g. `3.5 angstrom` |
| `embedding_update_tolerance=` | $`10^{-5}`$ | remake the embedding when an element of the density matrix has moved by more than this since it was made (§1); 0 remakes it at every Fock build |
| `use_embedding_schwarz_test=` | `TRUE` | skip the embedding integrals that the Schwarz test (5) shows to be negligible; `FALSE` computes them all |
| `use_SC_cluster_charges=` | `FALSE` | must be `TRUE`: the embedding replaces the charges of the neighbours' atoms |
| `cluster_radius=` | 6 Å | must reach every atom of every neighbour; the job stops if one is missing |

The embedding works with dipole cluster charges only (not `use_qq_cluster_charges=`) and with
Cartesian basis functions only (not `use_spherical_basis=`), since only Cartesian shells are
rotated in (4).

In a fragment SCF (§2) the same keywords apply to every fragment, with two differences:
`use_SC_cluster_charges=` and `embedding_radius=` may each be used without the other, and the
SCFs may start from the promolecule. For DFT fragments give `kind= fragment-rks`, the
`dft_functional=` and `use_default_guess_options= FALSE`; without the last the fragments are
done at Hartree–Fock.

On the molecule:

| keyword | meaning |
|---|---|
| `put_embedding_neighbours` | lists the neighbours (symmetry operation, lattice shift, shortest distance) and checks each: its rotated density against the molecule's at the carried-back points, its nuclear term against the molecule's electronic potential at its nuclei, and its electronic term against the same interaction on the DFT grid (§7) |
| `put_fragment_neighbours` | after a fragment SCF: lists the neighbours of each fragment, each with the interaction of the fragment's electrons with its electrons, $`E_J`$, and with its nuclei, $`E_N`$; then the sum of $`E_J`$ over the copies for each pair of fragments, a table that must be symmetric (§7) |


## 6. Example input

```
{
   name= urea
   basis_name= def2-SVP
   charge= 0
   multiplicity= 1
   CIF= { file_name= urea.cif }
   process_CIF
   crystal= { xray_data= { partition_model= oc-tfvc } }
   scfdata= {
      initial_density=        promolecule
      kind=                   rks
      dft_functional=         b3lyp
      use_SC_cluster_charges= TRUE
      cluster_radius=         8 angstrom
      embedding_radius=       3.5 angstrom
   }
   scf
   put_embedding_neighbours
   HAR_refinement
}
```


A fragment HAR of a monohydrate, the water being atoms 5, 25 and 26:

```
{
   name= NAC
   basis_name= def2-SVP
   CIF= { file_name= NAC_monohydrate.cif }
   process_CIF
   atom_groups= {
      keys= { name= atom_indices= }
      data= {
         nac   { 1 2 3 4 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 }
         water { 5 25 26 }
      }
   }
   scfdata= {
      kind=                   fragment-rhf
      initial_density=        promolecule
      use_SC_cluster_charges= TRUE
      cluster_radius=         8 angstrom
      embedding_radius=       3.5 angstrom
   }
   fraghar_refinement
}
```


## 7. Checks

`put_embedding_neighbours` on urea ($`P\bar{4}2_1m`$, the molecule on a special position) and
L-alanine ($`P2_12_12_1`$), RHF/def2-SVP, $`r_{\mathrm{emb}}`$ = 3.5 Å:

| | urea | L-alanine |
|---|---|---|
| neighbours within 3.5 Å | 10 | 12 |
| neighbours within 2.5 Å | 6 | 8 |
| largest density difference, translated copies | $`8\times10^{-15}`$ | $`2\times10^{-14}`$ |
| largest density difference, rotated copies | $`1\times10^{-12}`$ | $`3\times10^{-12}`$ |
| nuclear term against the molecule's potential at the nuclei | equal to every printed digit | equal to every printed digit |
| electronic term against the DFT grid, relative | $`2\times10^{-6}`$ | $`2\times10^{-6}`$ |

The density differences are taken at six points about each atom of each neighbour, where the
largest density is 1.6 to 1.7. Within 2.5 Å the neighbours are those of the explicit clusters of
7 and 9 molecules used for these crystals. Copies related by symmetry give equal interaction
energies.

`put_embedding_neighbours` also prints $`\mathrm{tr}\,(V^{\mathrm{emb}} P)`$ for the stored matrix and
for one made from the present density, the error of the table in §1. In a two-cycle refinement of
ammonia (`tests/long/nh3_rhf_DZP_HAR_J-embedding`), in which the H atom moves by 0.03 Å, the two
agree to $`6\times10^{-7}`$ Eh: the embedding follows the atoms.


**Fragments.** Three checks of §2, RHF/DZP on ammonia ($`P2_13`$), $`r_{\mathrm{emb}}`$ = 3.5 Å,
cluster charges to 8 Å, all tolerances $`10^{-9}`$:

| check | result |
|---|---|
| the molecule as the one fragment of a fragment SCF, against the molecule's own SCF of §1 | −56.37683994 Eh both ways |
| ammonia described in the subgroup $`P2_1`$, where two molecules are independent, as two fragments | −56.37683994 Eh for each fragment |
| the same: the sum of $`E_J`$ over the copies of fragment 2 about fragment 1, and of 1 about 2 | 116.523313 Eh both, equal to $`10^{-14}`$ |

For N-acetyl-L-4-hydroxyproline monohydrate (RHF/def2-SVP, 3.5 Å) the molecule has ten copies
of itself and seven of the water as neighbours, and the water seven copies of the molecule. Each
of the seven molecule–water pairs has the same $`E_J`$ in both lists, and the two sums agree to
$`6\times10^{-8}`$ Eh.

Refined to convergence at the default tolerances, the one-fragment refinement gives GoF 0.85145
and the molecule's 0.85142. Without surroundings the two refinements agree to eight digits.
The tests are `tests/long/nh3_rhf_DZP_fragHAR_J-embedding` and
`tests/long/nh3_P21_rhf_DZP_fragment_SCF_J-embedding`.


## 8. Where it is implemented

| piece | procedure |
|---|---|
| the keywords | `SCF_DATA:set_embedding_radius`, `set_embedding_update_tolerance`, `set_using_embedding_Schwarz_test`, and their `read_` routines |
| the neighbours, $`R_n`$ and $`\mathbf{t}_n`$ of (3) | `MOLECULE.SCF:make_embedding_neighbours` |
| whether two copies coincide | `MOLECULE.SCF:same_atom_sets` |
| $`P^{(n)}`$ of (4) | `MOLECULE.BASE:rotate_density_mx_r` |
| one neighbour's term of (2), from a source molecule, its density matrix and its atom positions, with the test (5) | `MOLECULE.FOCK:add_r_embedding_mx` |
| the sum over the neighbours | `MOLECULE.SCF:make_r_embedding_mx` |
| removal of the neighbours' atoms from the cluster charges | `MOLECULE.SCF:remove_embedded_cluster_charges`, called from `make_cluster_charge_mx` |
| $`V^{\mathrm{emb}}`$ in the core matrix, kept in `.embedding_mx` with $`P_{\mathrm{emb}}`$ in `.embedding_density`, and the test for remaking it | `MOLECULE.SCF:make_r_core_mx` |
| the fresh build at the start of each SCF | `MOLECULE.SCF:initialize_SCF` |
| the checks of §7 | `MOLECULE.SCF:put_embedding_neighbours` |
| the copies of another molecule as neighbours | the optional argument `src` of `make_embedding_neighbours`, `add_r_embedding_of` and `remove_embedded_cluster_charges` |
| the potential of the surroundings of fragment $`g`$, kept in `.mol(g).external_potential_mx` and added to the fragment's core matrix | `MOLECULE.SCF:make_fragment_surroundings`; `make_r_core_mx` |
| each fragment's atomic charges and dipoles, copied to the atoms of the whole structure | `MOLECULE.SCF:put_fragment_moments_to_atoms` |
| the passes and the test (6) | `MOLECULE.SCF:fragment_SCF_embedded`, called from `fragment_SCF` |
| the fragment checks of §7 | `MOLECULE.SCF:put_fragment_neighbours` |


## References

- Bjornsson R., Bühl M., *J. Chem. Theory Comput.* **8**, 498 (2012).
- Capelli S. C., Bürgi H.-B., Dittrich B., Grabowsky S., Jayatilaka D., *IUCrJ* **1**, 361 (2014).
- Chodkiewicz M. L., Woińska M., Woźniak K., *IUCrJ* **7**, 1199 (2020).
- Chu K., Jayatilaka D., Malaspina L. A., Genoni A., Cametti G., Mebs S., Lentz D., Bürgi H.-B., Churakov S. V., Grabowsky S., *J. Phys. Chem. Lett.* **17**, 3170 (2026).
- Chulhai D. V., Goodpaster J. D., *J. Chem. Theory Comput.* **14**, 1928 (2018).
- Genova A., Ceresoli D., Pavanello M., *J. Chem. Phys.* **141**, 174101 (2014).
- Genova A., Ceresoli D., Krishtal A., Andreussi O., DiStasio R. A., Pavanello M., *Int. J. Quantum Chem.* **117**, e25401 (2017).
- Goodpaster J. D., Barnes T. A., Manby F. R., Miller T. F., *J. Chem. Phys.* **140**, 18A507 (2014).
- Hirata S., *J. Chem. Phys.* **129**, 204104 (2008).
- Huzinaga S., Cantu A. A., *J. Chem. Phys.* **55**, 5543 (1971).
- Jayatilaka D., Dittrich B., *Acta Cryst.* A**64**, 383 (2008).
- Landeros-Rivera B., Kleemiss F., *J. Appl. Cryst.* **59**, 1467 (2026).
- Lee S. J. R., Welborn M., Manby F. R., Miller T. F., *Acc. Chem. Res.* **52**, 1359 (2019).
- Libisch F., Marsman M., Burgdörfer J., Kresse G., *J. Chem. Phys.* **147**, 034110 (2017).
- Manby F. R., Stella M., Goodpaster J. D., Miller T. F., *J. Chem. Theory Comput.* **8**, 2564 (2012).
- Pisani C. (ed.), *Quantum-Mechanical Ab-initio Calculation of the Properties of Crystalline Materials*, Lecture Notes in Chemistry **67**, Springer (1996).
- Ruth P. N., Herbst-Irmer R., Stalke D., *IUCrJ* **9**, 286 (2022).
- Sharma M., Sierka M., arXiv:2611.12399 (2026).
- Shukla A., Dolg M., Stoll H., Fulde P., *Chem. Phys. Lett.* **262**, 213 (1996).
- Shukla A., Dolg M., Fulde P., Stoll H., *Phys. Rev. B* **57**, 1471 (1998).
- Shukla A., Dolg M., Stoll H., *Chem. Phys. Lett.* **294**, 126 (1998).
- Shukla A., Dolg M., Fulde P., Stoll H., *Phys. Rev. B* **60**, 5211 (1999).
- Stoll H., *Phys. Rev. B* **46**, 6700 (1992).
- Stoll H., Paulus B., Fulde P., *J. Chem. Phys.* **123**, 144108 (2005).
- Wesolowski T. A., Warshel A., *J. Phys. Chem.* **97**, 8050 (1993).
- Wieduwilt E. K., Macetti G., Genoni A., *J. Phys. Chem. Lett.* **12**, 463 (2021).
