# Counting each pair of atoms once: pair stabilisers in two-centre structure factors

Tonto calculates the wavefunction of a fragment of the crystal, usually one molecule, and
builds the crystal's structure factors from it by applying the space-group operations. A
fragment generally has symmetry of its own, so the operations produce some parts of the crystal
more than once, and each part must be given a weight that undoes this. For a density divided
into atoms the weight belongs to the atom, and is one over the number of operations that leave
the atom in place. For a density written as products of basis functions on two atoms
(`partition_model= tc-...`), it belongs to the **pair of atoms**. This page gives the weight,
shows where it comes from, and says how Tonto finds it.

Notation is that of `docs/REPORT_ON_MODE_FITTING.md`, §6: basis functions $`\chi_\mu`$ on atom $`A`$
and $`\chi_\nu`$ on atom $`B`$, density matrix $`D_{\mu\nu}`$, scattering vector $`\mathbf{k}`$.


## 1. The density as a sum over pairs of atoms

The density of the crystal is a sum over pairs of basis functions. Collect the terms by the
atoms the functions are on:

```math
\rho(\mathbf{r}) = \sum_{(A,B)} \rho_{AB}(\mathbf{r}), \qquad
\rho_{AB}(\mathbf{r}) = \sum_{\mu \text{ on } A}\ \sum_{\nu \text{ on } B} D_{\mu\nu}\, \chi_\mu(\mathbf{r} - \mathbf{R}_A)\, \chi_\nu(\mathbf{r} - \mathbf{R}_B) . \qquad (1)
```

The sum is over **ordered pairs** $`(A,B)`$ of atoms of the crystal, each atom at a definite
position; $`(A,A)`$ is the part of the density from functions on one atom. Because $`D`$ is
symmetric, $`\rho_{AB} = \rho_{BA}`$. Write $`F_{AB}(\mathbf{k})`$ for the Fourier transform of $`\rho_{AB}`$,
with its thermal smearing.


## 2. What a symmetry operation does to a pair

A space-group operation $`g = \{R_g \mid \mathbf{t}_g\}`$ takes the point $`\mathbf{r}`$ to
$`g\mathbf{r} = R_g \mathbf{r} + \mathbf{t}_g`$, a rotation $`R_g`$ followed by a translation $`\mathbf{t}_g`$. It takes atom $`A`$
to an atom $`gA`$ of the crystal, and so it takes the pair $`(A,B)`$ to the pair $`(gA, gB)`$: **one
operation, with one translation, acts on both atoms.**

If the wavefunction and the thermal motion have the symmetry of the crystal, the density of the
image pair is the density of the pair, moved by $`g`$: $`\rho_{gA\,gB}(\mathbf{r}) = \rho_{AB}(g^{-1}\mathbf{r})`$. Its
Fourier transform follows by changing the variable of integration to $`\mathbf{s} = g^{-1}\mathbf{r}`$:

```math
\begin{aligned}
F_{gA\,gB}(\mathbf{k}) &= \int \rho_{AB}(g^{-1}\mathbf{r})\, e^{i\mathbf{k}\cdot\mathbf{r}}\, d\mathbf{r} \\
 &= \int \rho_{AB}(\mathbf{s})\, e^{i\mathbf{k}\cdot(R_g\mathbf{s} + \mathbf{t}_g)}\, d\mathbf{s} && \text{with } \mathbf{r} = R_g\mathbf{s} + \mathbf{t}_g \\
 &= e^{i\mathbf{k}\cdot\mathbf{t}_g} \int \rho_{AB}(\mathbf{s})\, e^{i(R_g^{\mathsf T}\mathbf{k})\cdot\mathbf{s}}\, d\mathbf{s} && \text{since } \mathbf{k}\cdot R_g\mathbf{s} = (R_g^{\mathsf T}\mathbf{k})\cdot\mathbf{s} \\
 &= e^{i\mathbf{k}\cdot\mathbf{t}_g}\, F_{AB}(R_g^{\mathsf T}\mathbf{k}) .
\end{aligned}
\qquad (2)
```

So the structure factor of any image of a pair comes from that of the pair itself, at a rotated
scattering vector and with a phase.


## 3. Orbits and stabilisers

Let $`G`$ be the space group and $`T`$ its lattice translations. Tonto holds one operation for each
element of $`G/T`$: the $`N_G`$ operations of the space group that are not related by a lattice
translation.

- The **orbit** of a pair is the set of all its images $`(gA, gB)`$, $`g`$ in $`G`$. Every pair of the
  crystal is in exactly one orbit.
- The **stabiliser** of a pair is the set of operations that leave both of its atoms where they
  are,

```math
S_{AB} = \{\, g \in G : gA = A \text{ and } gB = B \,\} = S_A \cap S_B , \qquad (3)
```

  the operations common to the site-symmetry groups $`S_A`$ and $`S_B`$ of the two atoms. $`|S_{AB}|`$
  is the number of operations in it. For $`A = B`$ it is the site-symmetry group of the atom.

Two operations $`g`$ and $`g'`$ give the same image of the pair exactly when $`g' = g s`$ with $`s`$ in
$`S_{AB}`$. So as $`g`$ runs over the $`N_G`$ operations of $`G/T`$, the image $`(gA,gB)`$ runs over the
members of the orbit that lie in one unit cell, and reaches each of them $`|S_{AB}|`$ times. There
are $`N_G/|S_{AB}|`$ of them.


## 4. The structure factor

The structure factor of one unit cell is the sum of $`F_p`$ over the pairs $`p`$ in the cell. Take
the pairs orbit by orbit, with one **representative** pair $`(A,B)`$ chosen from each orbit:

```math
\begin{aligned}
F(\mathbf{k}) &= \sum_{\text{orbits}}\ \sum_{p \text{ in the orbit, in the cell}} F_p(\mathbf{k}) \\
 &= \sum_{\text{orbits}}\ \frac{1}{|S_{AB}|} \sum_{g \in G/T} F_{gA\,gB}(\mathbf{k}) && \text{each member is reached } |S_{AB}| \text{ times} \\
 &= \sum_{\text{orbits}}\ \frac{1}{|S_{AB}|} \sum_{g \in G/T} e^{i\mathbf{k}\cdot\mathbf{t}_g}\, F_{AB}(R_g^{\mathsf T}\mathbf{k}) && \text{by (2)} \\
 &= \sum_{g \in G/T} e^{i\mathbf{k}\cdot\mathbf{t}_g}\ \underbrace{\sum_{\text{orbits}} \frac{1}{|S_{AB}|}\, F_{AB}(R_g^{\mathsf T}\mathbf{k})}_{\text{made from the fragment}} .
\end{aligned}
\qquad (4)
```

The last line is what is computed: the structure factor of the fragment, each pair of atoms
weighted by one over the order of its stabiliser, at the rotated scattering vectors, summed over
the operations with their phases. For $`A = B`$ the weight is one over the site symmetry of the
atom, which is the weight an atom has when the density is divided into atoms.

**Nothing has to be made symmetric first.** The operations of $`S_{AB}`$ are among those summed in
(4), and with the factor $`1/|S_{AB}|`$ they average $`F_{AB}`$ over its stabiliser.

**Pairs in either order.** Since $`\rho_{AB} = \rho_{BA}`$, Tonto takes the two orders together: the
unordered pair $`\{A,B\}`$ carries $`F_{AB} + F_{BA} = 2F_{AB}`$ when $`A \ne B`$. Its stabiliser is then
the set of operations that leave the two atoms in place *or exchange them*, and (4) holds with
that stabiliser and the orbits of unordered pairs.


## 5. A fragment holding several pairs of one orbit

A fragment usually holds more than one pair from an orbit: the two C–N pairs of urea are images
of each other under the molecule's own symmetry. Suppose the fragment holds $`m_{AB}`$ pairs of the
orbit of $`(A,B)`$. Each of them, summed over $`G/T`$, gives the whole orbit $`|S_{AB}|`$ times, by §3.
So the fragment can be used as it stands, every pair with the weight

```math
f_{AB} = \frac{1}{m_{AB}\, |S_{AB}|} = \frac{1}{n_{AB}} , \qquad (5)
```

where $`n_{AB} = m_{AB}|S_{AB}|`$ is the number of (operation, fragment pair) that land on $`(A,B)`$.
That number is counted directly: for each operation $`g`$ and each fragment atom $`C`$ whose image
$`gC`$ is $`A`$ after a lattice translation $`\mathbf{L}`$, every fragment atom $`D`$ whose image
$`gD + \mathbf{L}`$, *with the same* $`\mathbf{L}`$, is a fragment atom $`B`$ adds one to $`n_{AB}`$.

Tonto offers both forms:

| | weight of a fragment pair | pairs with a weight |
|---|---|---|
| all the pairs (the default) | $`1/n_{AB}`$ | every pair |
| the petite list, `use_pair_petite_list= TRUE` | $`m_{AB}/n_{AB} = 1/\lvert S_{AB}\rvert`$ for the first pair of each orbit in the fragment, zero for the rest | one of each orbit |

They give the same structure factor when the fragment's wavefunction has the symmetry assumed
in §2. The petite list does less work, by about the order of the fragment's symmetry group.


## 6. The petite list

Keeping one member of each set of symmetry-equivalent quantities, with a weight, is the method
of Dacre (1970) and Elder (1973) for the two-electron integrals of a molecule with point-group
symmetry, in the form given by Dupuis and King (1977). They call the set of representatives the
**petite list**, and the matrix made from it alone a **skeleton** matrix; the true Fock matrix is
recovered from the skeleton by symmetrising it, which is a sum over the operations of the group.
Here the objects are pairs of atoms instead of blocks of integrals, the weight is one over the
order of the pair's stabiliser, and the sum over the operations is the one in (4) that the
structure factor needs anyway.

One consequence carries over. A quantity made from the petite list is a skeleton until it has
been summed over the group. The structure factor is so summed. The derivative of the structure
factors with respect to the density matrix, which the X-ray constrained SCF adds to the Fock
matrix, is a matrix over basis functions, and from the petite list it would be a skeleton: its
blocks for the pairs left off the list would be zero. Symmetrising it needs, for each operation
of the fragment's symmetry group, the matrix that says how the basis functions transform: which
atom goes to which, and how the p, d and higher functions on it turn. Tonto does not do this, so
**the petite list is not for the X-ray constrained SCF**, which stops if it is set. The default,
all the pairs, is correct there with no further step.


## 7. The earlier scheme: repetition factors

Before this, Tonto shared each product between its two atoms (Jayatilaka and Grimwood, 2001,
§4.3). Each fragment atom $`A`$ has a **repetition factor** $`n_A`$, the number of (operation, fragment
atom) that land on $`A`$ with lattice translations ignored, and a product of a primitive on $`A`$
with a primitive on $`B`$ was given the weight

```math
\frac{w_A}{n_A} + \frac{w_B}{n_B} , \qquad (6)
```

with $`w_A`$ and $`w_B`$ the fractions $`\alpha/(\alpha+\beta)`$ and $`\beta/(\alpha+\beta)`$ of the two exponents. It is still
used when repetition factors are set by hand (`use_manual_repetition_factors= TRUE`).

**For one whole molecule (6) and (5) are equal.** Let the fragment be a molecule whose own
symmetry group in the crystal has $`n_s`$ operations. An operation that takes a pair of atoms of
the molecule to another pair of atoms of the molecule takes the molecule to itself, so it is one
of the $`n_s`$; and each of the $`n_s`$ lands every pair on a pair of the fragment. So $`n_{AB} = n_s`$
for every pair. In the same way $`n_A = n_s`$ for every atom, and (6) is
$`w_A/n_s + w_B/n_s = 1/n_s`$, because $`w_A + w_B = 1`$.

**For a cluster of molecules they differ.**

- A pair with one atom in each of two molecules is in an orbit of its own, whose size is not
  set by the repetition factors of its atoms.
- For such a pair the same lattice translation must act on both atoms. Counting each atom by
  itself, modulo the lattice, loses that.


## 8. A check on urea

Urea is a whole molecule on a site of symmetry mm2, four operations, so by §7 the old and the new
weights must agree, and the petite list must agree with all the pairs. A `tc-tls` refinement
(RHF/def2-SVP, 817 reflections, 17 parameters; `docs/REPORT_ON_MODE_FITTING.md`, §6):

| weights | pairs with a weight | sum of the weights | GoF | R(F) |
|---|---|---|---|---|
| (6), repetition factors | | | 3.6577 | 0.0186 |
| (5), all the pairs | 64 | 16 | 3.6577 | 0.0186 |
| (5), the petite list | 37 | 16 | 3.6577 | 0.0186 |

There are $`8\times 8 = 64`$ ordered pairs of the eight atoms, and the weights sum to $`64/4 = 16`$,
the number of ordered pairs for each operation of the molecule's symmetry. In the petite list
the pair of oxygen with itself has weight 1/4, oxygen with one nitrogen 1/2, and the two
nitrogens with each other 1/4: the last is left in place by the identity and one mirror, and
its atoms are exchanged by the twofold axis and the other mirror.


## 9. Keywords

In `crystal=`:

| keyword | default | meaning |
|---|---|---|
| `use_pair_petite_list=` | `FALSE` | `TRUE`: one pair of each orbit, weight $`1/\lvert S_{AB}\rvert`$; `FALSE`: every pair, weight $`1/n_{AB}`$ (§5) |
| `put_fragment_pair_weights` | | prints the weight of each pair of fragment atoms, how many are not zero, and their sum |
| `use_manual_repetition_factors=` | `FALSE` | `TRUE` keeps the weights (6) with the repetition factors read by `repetition_factors=` |

The weights are used by every two-centre model: `tc-tls`, `tc-tanaka`, `tc-stewart` and
`tc-coppens`.


## 10. Where it is implemented

| piece | procedure |
|---|---|
| $`n_{AB}`$, $`m_{AB}`$ and the weights (5) | `CRYSTAL:make_fragment_pair_weights` |
| the printout | `CRYSTAL:put_fragment_pair_weights` |
| the weight given to a pair of shells | `MOLECULE.XTAL:make_partition_factors` |
| the weights (6), for manual repetition factors | `CRYSTAL:make_repetition_factors`, `SHELL2:make_Tanaka_partition` |
| the sum over the operations in (4) | `SPACEGROUP:sum_unique_sf` |


## References

- P. D. Dacre, *Chem. Phys. Lett.* 7, 47 (1970); M. Elder, *Int. J. Quantum Chem.* 7, 75 (1973):
  symmetry-equivalent two-electron integrals.
- M. Dupuis and H. F. King, *Int. J. Quantum Chem.* 11, 613 (1977): the petite list and the
  skeleton Fock matrix.
- D. Jayatilaka and D. J. Grimwood, *Acta Cryst.* A57, 76 (2001): repetition factors.
