# Variants of the Salvador atom model

Hirshfeld atom refinement needs the molecular density divided into atoms. This page compares
ways of doing that, on urea and on YLID: the Hirshfeld atom, the Salvador atom (a topological
fuzzy Voronoi atom), three variants of the Salvador atom that differ in where the boundary
between two bonded atoms is put, the exponential Hirshfeld atom of Chodkiewicz and Woźniak, and
the spherical average of each. The main findings:

- **The aspherical models fit alike.** Hirshfeld fits best; the Salvador family and the
  exponential Hirshfeld atom are within 0.2 in GoF of it and give the same heavy-atom geometry.
- **They differ in the polar N–H bonds**, by up to 0.04 Å, and not in C–H bonds.
- **A spherical atom works only if it is large enough to hold its own bonding density.** The
  spherical Salvador atom does not converge; with equal-density boundaries (`sph-tfvh`), or as a
  spherical exponential Hirshfeld atom (`sph-exphar`), it fits as well as the independent atom
  model and puts the hydrogens within 0.02 Å of the neutron positions, where the independent
  atom model is 0.1 Å short.

All refinements here use Cartesian basis functions.

## 1. The models

Let $`\rho(\mathbf r)`$ be the molecular density, and $`\rho^0_A(r)`$ the spherical density of the
free atom $`A`$. Every model writes the density of atom $`A`$ in the molecule as
$`\rho_A(\mathbf r) = w_A(\mathbf r)\,\rho(\mathbf r)`$ with weights $`w_A`$ that add to one.

**Hirshfeld.** The weight is the atom's share of the promolecule, the sum of the free atoms:

```math
w_A(\mathbf r) = \frac{\rho^0_A(\mathbf r)}{\sum_B \rho^0_B(\mathbf r)} . \qquad (1)
```

**Exponential Hirshfeld** (`exphar`; Chodkiewicz and Woźniak, 2025). Every free-atom density
is raised to a power $`n`$:

```math
w_A(\mathbf r) = \frac{\rho^0_A(\mathbf r)^n}{\sum_B \rho^0_B(\mathbf r)^n} . \qquad (2)
```

$`n = 1`$ is Hirshfeld; a larger $`n`$ makes the atoms overlap less. The authors recommend $`n = 2`$.

**Salvador** (`tfva`). The weight is Salvador's cell function, a Becke-like fuzzy Voronoi cell
whose faces are moved along each bond. For a bonded pair $`A`$–$`B`$ the face cuts the bond at a
distance $`R(A.B)`$ from $`A`$, and the model is fixed by the rule for that point. Mayer and
Salvador put it at the minimum of the molecular density along the bond.

**Salvador with promolecule radii** (`tfvp`). The same, with the minimum taken on the
promolecule density. The partition then depends on the geometry alone, not on the wavefunction.

**Salvador with equal-density radii** (`tfvh`). The boundary is where the two free atoms'
densities are equal, $`\rho^0_A = \rho^0_B`$ on the bond. There the Hirshfeld weight of the pair
alone, $`\rho^0_A/(\rho^0_A + \rho^0_B)`$, is one half, so the cell function and the pair's
Hirshfeld weight agree on where the atoms meet. The point always exists and is unique, because
one density falls and the other rises along the bond. The half-weight surface of the full
promolecule would not do: on most bonds those surfaces of $`A`$ and $`B`$ do not touch.

**The spherical average** (`sph-tfva`, `sph-tfvp`, `sph-tfvh`, `sph-exphar`). The atom's density
is averaged over each sphere about its nucleus,

```math
\bar\rho_A(r) = \frac{1}{4\pi} \oint w_A(\mathbf r)\,\rho(\mathbf r)\, d\Omega , \qquad (3)
```

with $`d\Omega`$ the element of solid angle, and the form factor at scattering vector
$`\mathbf k`$, for a nucleus at $`\mathbf r_A`$, is

```math
f_A(\mathbf k) = e^{i \mathbf k\cdot\mathbf r_A} \int_0^\infty 4\pi r^2\, \bar\rho_A(r)\, \frac{\sin kr}{kr}\, dr . \qquad (4)
```

It keeps the atom's charge and the radial shape of its density, taken from the wavefunction,
and loses everything aspherical: bonding density and lone pairs.

| `partition_model=` | weight | boundary on a bond | spherical average |
|---|---|---|---|
| `oc-hirshfeld` | (1) | | no |
| `exphar`, with `exphar_power=` $`n`$ (default 2) | (2) | | no |
| `oc-salvador` | cell function | minimum of the molecular density | no |
| `tfvp` | cell function | minimum of the promolecule density | no |
| `tfvh` | cell function | equal free-atom densities | no |
| `sph-exphar`, `sph-tfva`, `sph-tfvp`, `sph-tfvh` | as the model named | | yes |

The independent atom model (IAM) is the comparison throughout: International Tables form factors
for the neutral heavy atoms and the Stewart–Davidson–Simpson bonded-atom form factor for H.

## 2. Where the boundaries fall

Urea, RHF/def2-SVP, at the geometry refined with Salvador atoms. $`R(A.B)`$ is the distance from
$`A`$ to its boundary with $`B`$; the first column is the bond length.

| bond | bond length /Å | $`R(A.B)`$, molecule /Å | $`R(A.B)`$, promolecule /Å | ratio |
|---|---|---|---|---|
| O–C, from O | 1.2557 | 0.8425 | 0.8159 | 1.033 |
| N–C, from N | 1.3406 | 0.9022 | 0.8037 | 1.123 |
| N–H1, from N | 1.0383 | 0.8119 | 0.7854 | 1.034 |
| N–H3, from N | 1.0261 | 0.7911 | 0.7785 | 1.016 |

The promolecule moves the C–N boundary about 0.10 Å toward N and the others by 2–3 %.

Urea, CCSD/pob-TZVP density at the CIF geometry, all three rules:

| bond | bond length /Å | $`R(A.B)`$, molecule /Å | promolecule /Å | equal density /Å |
|---|---|---|---|---|
| C–O, from C | 1.2537 | 0.4375 | 0.4616 | 0.5674 |
| C–N, from C | 1.3400 | 0.4985 | 0.5679 | 0.6346 |
| N–H1, from N | 1.0077 | 0.8576 | 0.8268 | 0.7398 |
| N–H2, from N | 0.9930 | 0.8421 | 0.8193 | 0.7340 |

The equal-density point is well away from the density minimum, toward the more electronegative
atom: the carbon atom grows by 0.13 and 0.14 Å along C–O and C–N, and the hydrogen's part of the
N–H bond grows from 0.15 to 0.27 Å. So `tfvh` has the largest hydrogens and the largest carbon
of the three.

**Charges** from the same density, nucleus included:

| atom | Hirshfeld | Salvador | `tfvp` | `tfvh` |
|---|---|---|---|---|
| C | +0.198 | +2.167 | +1.695 | +0.908 |
| O | −0.396 | −1.189 | −1.118 | −0.740 |
| N | −0.137 | −1.804 | −1.469 | −0.801 |
| H1 (cis) | +0.127 | +0.672 | +0.596 | +0.364 |
| H2 | +0.109 | +0.643 | +0.585 | +0.353 |

The Salvador atoms carry charges of the size QTAIM atoms do, the density minimum being close to
the zero-flux surface along a bond. `tfvh` is about half-way between Salvador and Hirshfeld.

## 3. Urea: the aspherical models

Hirshfeld atom refinement of urea against the 123 K X-ray data (Birkedal et al., 2004; 817
reflections), RHF, isolated molecule, with only `partition_model=` changed. The neutron bond
lengths are those of Swaminathan, Craven and McMullan (1984) at 123 K, as tabulated by Wall
(2016), without esds: N–H1 1.006, N–H3 1.000 and C=O 1.257 Å.

| | Hirshfeld | Salvador | `tfvp` | `tfvh` |
|---|---|---|---|---|
| **def2-SVP** | | | | |
| R(F) | 0.0181 | 0.0190 | 0.0188 | 0.0186 |
| GoF | 3.30 | 3.54 | 3.48 | 3.46 |
| cycles | 5 | 8 | 7 | 7 |
| O=C /Å | 1.2558(4) | 1.2557(4) | 1.2555(4) | 1.2556(4) |
| N–C /Å | 1.3413(3) | 1.3406(3) | 1.3408(3) | 1.3410(3) |
| N–H1 /Å | 1.028(5) | 1.038(5) | 1.036(4) | 1.034(4) |
| N–H3 /Å | 0.986(6) | 1.026(5) | 1.023(4) | 1.018(4) |
| U_iso O /Å² | 0.01512(7) | 0.01517(8) | 0.01516(8) | 0.01515(7) |
| U_iso N /Å² | 0.02218(8) | 0.02214(9) | 0.02212(9) | 0.02211(9) |
| U_iso C /Å² | 0.01213(7) | 0.01219(8) | 0.01217(8) | 0.01216(8) |
| U_iso H1 /Å² | 0.054(4) | 0.050(4) | 0.048(3) | 0.046(3) |
| U_iso H3 /Å² | 0.048(3) | 0.042(2) | 0.041(2) | 0.040(2) |
| **def2-TZVP** | | | | |
| R(F) | 0.0167 | 0.0170 | 0.0170 | 0.0169 |
| GoF | 2.94 | 3.10 | 3.07 | 3.06 |
| cycles | 5 | 8 | 7 | 8 |
| O=C /Å | 1.2560(4) | 1.2560(4) | 1.2558(4) | 1.2559(4) |
| N–C /Å | 1.3413(3) | 1.3405(3) | 1.3408(3) | 1.3409(3) |
| N–H1 /Å | 1.025(4) | 1.033(4) | 1.032(4) | 1.029(3) |
| N–H3 /Å | 0.989(5) | 1.017(4) | 1.017(4) | 1.013(3) |
| U_iso O /Å² | 0.01513(6) | 0.01516(7) | 0.01516(7) | 0.01515(7) |
| U_iso N /Å² | 0.02227(7) | 0.02224(8) | 0.02222(8) | 0.02221(8) |
| U_iso C /Å² | 0.01210(7) | 0.01215(7) | 0.01213(7) | 0.01212(7) |
| U_iso H1 /Å² | 0.054(3) | 0.049(3) | 0.048(3) | 0.046(3) |
| U_iso H3 /Å² | 0.046(2) | 0.0402(19) | 0.0400(19) | 0.0392(18) |
| time /s | 53 | 77 | 73 | 77 |

- **The three Salvador variants form a sequence,** Salvador, `tfvp`, `tfvh`, along which the
  N–H bonds shorten (def2-TZVP: N–H3 1.017, 1.017, 1.013 Å), the fit improves a little (GoF
  3.10, 3.07, 3.06), and every step is toward Hirshfeld.
- **`tfvp` reproduces the Salvador refinement,** every heavy-atom parameter within one esd,
  although its C–N boundary is 0.1 Å away. The partition can be fixed by the geometry alone at
  no cost.
- **Hirshfeld fits best** (GoF 2.94 in def2-TZVP). Its N–H3 is 0.011 Å short of neutron where
  every Salvador variant is long; averaged over the two bonds `tfvh` and Hirshfeld are about
  equally far from neutron (0.018 and 0.015 Å).
- **The heavy atoms do not depend on the model:** bonds within 0.0008 Å and ADPs within one or
  two esds across the four.
- **The hydrogen U_iso falls along the sequence,** 0.054, 0.050, 0.048, 0.046 Å² for H1 in
  def2-SVP. A larger hydrogen atom in the partition leaves less of the bond density to be
  modelled as hydrogen motion. The hydrogen esds are smallest with `tfvh`, 0.003 Å.
- **A Salvador refinement costs about 1.5 times a Hirshfeld one** on this job.

**The exponential Hirshfeld atom**, for several powers $`n`$:

| urea | $`n`$ | R(F) | GoF | cycles | N–H1 /Å | N–H3 /Å | O=C /Å | U_iso H1, H3 /Å² |
|---|---|---|---|---|---|---|---|---|
| def2-SVP, Hirshfeld | 1 | 0.0181 | 3.30 | 5 | 1.028(5) | 0.986(6) | 1.2558(4) | 0.054(4), 0.048(3) |
| def2-SVP, `exphar` | 1.5 | 0.0182 | 3.35 | 5 | 1.031(4) | 1.004(5) | 1.2557(4) | 0.050(4), 0.044(2) |
| def2-SVP, `exphar` | 2 | 0.0183 | 3.37 | 6 | 1.032(4) | 1.010(4) | 1.2557(4) | 0.049(3), 0.043(2) |
| def2-SVP, `exphar` | 3 | 0.0185 | 3.40 | 8 | 1.034(4) | 1.015(4) | 1.2556(4) | 0.048(3), 0.042(2) |
| def2-SVP, `sph-exphar` | 2 | 0.0283 | 5.55 | 4 | 1.020(8) | 0.989(8) | 1.2582(7) | 0.044(5), 0.046(4) |
| def2-TZVP, Hirshfeld | 1 | 0.0167 | 2.94 | 5 | 1.025(4) | 0.989(5) | 1.2560(4) | 0.054(3), 0.046(2) |
| def2-TZVP, `exphar` | 1.5 | 0.0167 | 2.97 | 5 | 1.026(4) | 1.003(4) | 1.2559(4) | 0.050(3), 0.042(2) |
| def2-TZVP, `exphar` | 2 | 0.0167 | 2.99 | 6 | 1.027(3) | 1.007(4) | 1.2559(4) | 0.049(3), 0.041(2) |
| def2-TZVP, `exphar` | 3 | 0.0168 | 3.01 | 8 | 1.029(3) | 1.011(3) | 1.2559(4) | 0.048(3), 0.040(2) |
| def2-TZVP, `sph-exphar` | 2 | 0.0282 | 5.38 | 4 | 1.010(7) | 0.981(8) | 1.2581(7) | 0.045(5), 0.045(4) |
| neutron | | | | | 1.006 | 1.000 | 1.257 | |

- **N–H3 lengthens steadily with $`n`$** (def2-TZVP: 0.989, 1.003, 1.007, 1.011 Å for $`n`$ = 1,
  1.5, 2, 3), crossing the neutron value between 1.5 and 2. N–H1 moves by 0.004 Å. This is what
  Chodkiewicz and Woźniak report: polar X–H bonds lengthen with $`n`$.
- **The fit hardly changes:** R(F) is the same and GoF rises by 0.05. The hydrogen esds fall
  from 0.004–0.005 to 0.003 Å and the hydrogen U_iso by 10 %. Each step in $`n`$ costs a cycle
  or two.
- **`exphar` with $`n = 2`$ lies between Hirshfeld and `tfvh`** on every hydrogen quantity
  (N–H3: 0.989, 1.007, 1.013, 1.017 Å for Hirshfeld, `exphar`, `tfvh`, Salvador in def2-TZVP),
  at 1.1 times the cost of Hirshfeld.

## 4. Urea: the spherical models

**Which atoms can be made spherical.** Mixing the models by element, with the Salvador atom:

| urea | STO-3G | def2-SVP |
|---|---|---|
| Salvador | R 0.038, converges (7 cycles) | R 0.0190, GoF 3.54, converges (8) |
| spherical H, aspherical C/N/O | R 0.041, converges (5) | R 0.0209, GoF 3.86, converges (6) |
| `sph-tfva`, all spherical | H1 runs away, R = 1 | R 0.044, GoF 11.6, never converges |
| `sph-tfva`, H with isotropic U | reaches R 0.0533, then oscillates | |
| `sph-tfva`, H fixed | R 0.096 | |

Spherical hydrogens are harmless: the Salvador hydrogen is nearly spherical already. Spherical
C, N and O are not: without their bonding density the fit rebuilds it by moving the hydrogens
and inflating their U. The oscillations are between two states of the outer loop of SCF and
fit; in def2-SVP H1 moves 0.034 bohr back and forth.

**The size of the atom decides.** `sph-tfva` does not converge in any basis. `sph-tfvp`
converges, with GoF 8.16 in def2-SVP. `sph-tfvh` gives 5.87. That is the order of growing carbon
and hydrogen atoms (§2): the more of the bond density lies inside the atom being averaged, the
less the averaging loses.

`sph-tfvh` in three basis sets; every run converges in 4–5 cycles:

| urea | STO-3G | def2-SVP | def2-TZVP |
|---|---|---|---|
| R(F) | 0.0447 | 0.0297 | 0.0294 |
| GoF | 8.52 | 5.87 | 5.65 |
| cycles | 4 | 5 | 4 |
| O=C /Å | 1.2571(11) | 1.2570(7) | 1.2570(7) |
| N–H1 /Å | 1.050(12) | 1.033(9) | 1.022(8) |
| N–H3 /Å | 0.994(13) | 1.001(10) | 0.993(9) |
| U_iso H1 /Å² | 0.063(9) | 0.046(5) | 0.047(5) |
| U_iso H3 /Å² | 0.064(7) | 0.050(5) | 0.050(4) |
| time /s | 1.5 | 7 | 50 |

**Against the independent atom model:**

| model | N–H1 /Å | N–H3 /Å | R(F) | GoF |
|---|---|---|---|---|
| IAM (SDS H) | 0.905(10) | 0.888(12) | 0.0284 | 6.50 |
| `sph-tfvp`, STO-3G | 1.048(15) | 0.944(15) | 0.0455 | 10.5 |
| `sph-tfvp`, def2-SVP | 1.069(12) | 0.981(15) | 0.0341 | 8.16 |
| Hirshfeld, def2-SVP | 1.028(5) | 0.986(6) | 0.0181 | 3.30 |
| Salvador, def2-SVP | 1.038(5) | 1.026(5) | 0.0190 | 3.54 |
| `tfvp`, def2-SVP | 1.036(4) | 1.023(4) | 0.0188 | 3.48 |

- **The IAM shortens N–H by 0.1 Å; the spherical models do not.** A spherical average of the
  molecular density about the hydrogen nucleus includes the density drawn into the bond; a
  free-atom hydrogen does not.
- **A lower R is not a better geometry.** The IAM fits better than `sph-tfvp` (R 0.028 against
  0.034) by moving the hydrogens and adjusting the U, which is how it gets the bonds wrong.
- **`sph-tfvh` and `sph-exphar` fit as well as the IAM** (R 0.0294 and 0.0282 against 0.0284 in
  def2-TZVP, next section) with the hydrogens near the neutron positions. They are still
  spherical models: GoF 5.4–5.7 against 3.0 for the aspherical ones, and hydrogen esds twice as
  large.
- **The two err in opposite directions on N–H3.** `sph-exphar` is best of any model on N–H1
  (+0.004 Å) and 0.019 Å short on N–H3; `sph-tfvh` is +0.016 and −0.007 Å, and gives the neutron
  C=O length. Either can supply the form factors for the Gaussian fit of `fit_sph_atom_ffs`.

## 5. Urea: every model against neutron

| model | basis | N–H1 /Å | N–H3 /Å | O=C /Å | R(F) | GoF |
|---|---|---|---|---|---|---|
| **neutron, 123 K** | | **1.006** | **1.000** | **1.257** | | |
| IAM (International Tables; SDS H) | | 0.905(10) | 0.888(12) | 1.2583(8) | 0.0284 | 6.50 |
| Hirshfeld | def2-TZVP | 1.025(4) | 0.989(5) | 1.2560(4) | 0.0167 | 2.94 |
| `exphar` n=2 | def2-TZVP | 1.027(3) | 1.007(4) | 1.2559(4) | 0.0167 | 2.99 |
| Salvador | def2-TZVP | 1.033(4) | 1.017(4) | 1.2560(4) | 0.0170 | 3.10 |
| `tfvp` | def2-TZVP | 1.032(4) | 1.017(4) | 1.2558(4) | 0.0170 | 3.07 |
| `tfvh` | def2-TZVP | 1.029(3) | 1.013(3) | 1.2559(4) | 0.0169 | 3.06 |
| `sph-tfvh` | def2-TZVP | 1.022(8) | 0.993(9) | 1.2570(7) | 0.0294 | 5.65 |
| `sph-exphar` n=2 | def2-TZVP | 1.010(7) | 0.981(8) | 1.2581(7) | 0.0282 | 5.38 |
| `sph-tfvp` | def2-TZVP | 1.045(10) | 0.977(12) | 1.2553(9) | 0.0316 | 7.06 |
| `sph-tfva` | def2-TZVP | 1.11(2) | 0.954(19) | 1.2518(14) | 0.0422 | 11.0 (no convergence) |
| Hirshfeld | def2-SVP | 1.028(5) | 0.986(6) | 1.2558(4) | 0.0181 | 3.30 |
| `exphar` n=2 | def2-SVP | 1.032(4) | 1.010(4) | 1.2557(4) | 0.0183 | 3.37 |
| Salvador | def2-SVP | 1.038(5) | 1.026(5) | 1.2557(4) | 0.0190 | 3.54 |
| `tfvp` | def2-SVP | 1.036(4) | 1.023(4) | 1.2555(4) | 0.0188 | 3.48 |
| `tfvh` | def2-SVP | 1.034(4) | 1.018(4) | 1.2556(4) | 0.0186 | 3.46 |
| `sph-tfvh` | def2-SVP | 1.033(9) | 1.001(10) | 1.2570(7) | 0.0297 | 5.87 |
| `sph-exphar` n=2 | def2-SVP | 1.020(8) | 0.989(8) | 1.2582(7) | 0.0283 | 5.55 |
| `sph-tfvp` | def2-SVP | 1.069(12) | 0.981(15) | 1.2545(10) | 0.0341 | 8.16 |
| `sph-tfvh` | STO-3G | 1.050(12) | 0.994(13) | 1.2571(11) | 0.0447 | 8.52 |
| `sph-tfvp` | STO-3G | 1.048(15) | 0.944(15) | 1.2542(13) | 0.0455 | 10.5 |
| `sph-tfva` | def2-SVP | does not converge (oscillates) | | | 0.044 | 11.6 |
| `sph-tfva` | STO-3G | does not converge (H1 runs away) | | | 1.0 | |

Differences from the neutron values, in Å:

| model | N–H1 | N–H3 |
|---|---|---|
| IAM | −0.101 | −0.112 |
| Hirshfeld, def2-TZVP | +0.019 | −0.011 |
| `exphar` n=2, def2-TZVP | +0.021 | +0.007 |
| `sph-exphar` n=2, def2-TZVP | +0.004 | −0.019 |
| Salvador, def2-TZVP | +0.027 | +0.017 |
| `tfvp`, def2-TZVP | +0.026 | +0.017 |
| `tfvh`, def2-TZVP | +0.023 | +0.013 |
| `sph-tfvh`, def2-TZVP | +0.016 | −0.007 |
| `sph-tfvp`, def2-TZVP | +0.039 | −0.023 |
| Hirshfeld, def2-SVP | +0.022 | −0.014 |
| `exphar` n=2, def2-SVP | +0.026 | +0.010 |
| `sph-exphar` n=2, def2-SVP | +0.014 | −0.011 |
| Salvador, def2-SVP | +0.032 | +0.026 |
| `tfvp`, def2-SVP | +0.030 | +0.023 |
| `tfvh`, def2-SVP | +0.028 | +0.018 |
| `sph-tfvh`, def2-SVP | +0.027 | +0.001 |
| `sph-tfvp`, def2-SVP | +0.063 | −0.019 |

- **Hirshfeld is closest among the aspherical models** (def2-TZVP within 0.019 Å). The Salvador
  variants are 0.02–0.03 Å long, `tfvh` the least; `exphar` with $`n = 2`$ is 0.021 and 0.007 Å
  long.
- **def2-TZVP moves every aspherical model a few mÅ toward neutron** and lowers R(F) by about
  0.002.
- **C=O is 3 esds short with Hirshfeld,** 1.2558(4) against 1.257 Å; `sph-tfvh` gives 1.2570(7)
  in both basis sets.
- These are isolated-molecule densities in small basis sets. The comparison between models at
  the same basis is the point, not the absolute values; with a crystal environment and B3LYP
  the Hirshfeld N–H bonds come within 0.005 Å of neutron
  ([`REPORT_ON_MODE_FITTING.md`](REPORT_ON_MODE_FITTING.md)).

## 6. YLID: a crystal with C–H bonds only

Is the N–H disagreement between Hirshfeld and the Salvador family a property of the partitions,
or of the polar, hydrogen-bonded N–H? YLID (2-dimethylsulfuranylidene-1,3-indanedione,
C₁₁H₁₀O₂S) has ten C–H bonds, six methyl and four aromatic, and no N–H or O–H. Cu Kα data on
F², `f_sigma_cutoff= 4`, isotropic hydrogens, RHF, isolated molecule. The IAM on the same data
gives R(F) 0.0275 and GoF 8.54. The neutron averages are 1.083 Å for aromatic C–H and 1.077 Å
for methyl C–H (Allen and Bruno, 2010).

def2-SVP:

| | Hirshfeld | Salvador | `tfvp` | `tfvh` |
|---|---|---|---|---|
| R(F) | 0.0231 | 0.0232 | 0.0233 | 0.0233 |
| GoF | 7.250 | 7.284 | 7.287 | 7.284 |
| cycles | 4 | 6 | 6 | 6 |
| time /min | 7.4 | 11.9 | 12.4 | 12.1 |
| S1–C8 /Å | 1.7096(15) | 1.7098(15) | 1.7098(15) | 1.7098(15) |
| O1–C1 /Å | 1.2293(17) | 1.2279(16) | 1.2281(16) | 1.2290(17) |
| C4–H5 (arom.) | 1.09(2) | 1.088(14) | 1.095(15) | 1.097(16) |
| C5–H7 (arom.) | 1.11(2) | 1.102(15) | 1.113(15) | 1.117(16) |
| C6–H4 (arom.) | 1.08(2) | 1.071(14) | 1.083(15) | 1.088(15) |
| C11–H9 (arom.) | 1.09(2) | 1.086(12) | 1.101(13) | 1.105(14) |
| C9–H1 (methyl) | 1.08(2) | 1.072(15) | 1.074(16) | 1.075(16) |
| C9–H3 | 1.08(2) | 1.083(15) | 1.097(16) | 1.102(16) |
| C9–H10 | 1.09(2) | 1.085(14) | 1.094(15) | 1.097(15) |
| C10–H2 | 1.08(2) | 1.069(14) | 1.078(15) | 1.081(15) |
| C10–H6 | 1.10(3) | 1.096(17) | 1.103(17) | 1.108(18) |
| C10–H8 | 1.11(2) | 1.106(13) | 1.112(14) | 1.113(14) |
| mean C–H /Å | 1.091 | 1.086 | 1.095 | 1.098 |
| mean H esd /Å | 0.021 | 0.014 | 0.015 | 0.016 |
| U_iso H, range /Å² | 0.037–0.056 | 0.033–0.049 | 0.033–0.049 | 0.034–0.050 |
| mean U_iso H esd /Å² | 0.006 | 0.004 | 0.004 | 0.004 |

def2-TZVP:

| | Hirshfeld | Salvador | `tfvp` | `tfvh` |
|---|---|---|---|---|
| R(F) | 0.0228 | 0.0229 | 0.0229 | 0.0229 |
| GoF | 7.185 | 7.232 | 7.230 | 7.226 |
| cycles | 4 | 7 | 6 | 6 |
| time /min | 36 | 51 | 47 | 45 |
| S1–C8 /Å | 1.7102(15) | 1.7104(15) | 1.7104(15) | 1.7104(15) |
| O1–C1 /Å | 1.2298(17) | 1.2284(16) | 1.2287(16) | 1.2295(16) |
| C4–H5 (arom.) | 1.09(2) | 1.088(15) | 1.093(15) | 1.094(15) |
| C5–H7 (arom.) | 1.11(2) | 1.109(15) | 1.116(15) | 1.117(16) |
| C6–H4 (arom.) | 1.08(2) | 1.078(14) | 1.087(15) | 1.090(15) |
| C11–H9 (arom.) | 1.09(2) | 1.092(13) | 1.101(13) | 1.103(14) |
| C9–H1 (methyl) | 1.08(2) | 1.071(15) | 1.073(16) | 1.074(16) |
| C9–H3 | 1.08(2) | 1.087(15) | 1.097(16) | 1.100(16) |
| C9–H10 | 1.09(2) | 1.089(14) | 1.095(15) | 1.097(15) |
| C10–H2 | 1.08(2) | 1.072(14) | 1.078(15) | 1.079(15) |
| C10–H6 | 1.10(3) | 1.098(17) | 1.104(18) | 1.107(18) |
| C10–H8 | 1.11(2) | 1.103(13) | 1.108(14) | 1.109(14) |
| mean C–H /Å | 1.091 | 1.089 | 1.095 | 1.097 |
| mean H esd /Å | 0.021 | 0.015 | 0.015 | 0.015 |
| U_iso H, range /Å² | 0.040–0.057 | 0.035–0.050 | 0.035–0.050 | 0.036–0.051 |
| mean U_iso H esd /Å² | 0.006 | 0.004 | 0.004 | 0.004 |

- **The N–H effect does not appear in C–H.** In def2-SVP the Salvador mean C–H is 0.005 Å
  shorter than Hirshfeld's and `tfvh`'s 0.007 Å longer, and every bond agrees between the models
  within one esd. The urea disagreement belongs to the polar N–H bond.
- **The models cannot be ranked on these hydrogens.** All four are 0.01–0.02 Å long against the
  neutron means, with esds of 0.015–0.02 Å: Cu Kα data to $`\sin\theta/\lambda \approx 0.6`$ Å⁻¹,
  with $`\theta`$ the Bragg angle and $`\lambda`$ the wavelength, do not place them well.
- **The fits are the same:** R(F) within 0.0002 and GoF within 0.05. Hirshfeld converges in 4
  cycles against 6 or 7, in 60–80 % of the time.
- **The Salvador family gives smaller hydrogen esds,** about 30 % smaller for position and
  U_iso, and U_iso values about 15 % smaller, as in urea. The heavy atoms are identical.
- **The larger basis changes nothing in the comparison.**

## 7. What is not done

- The spherical models on YLID and on a larger molecule.
- `sph-exphar` with $`n`$ below 2: a softer atom should suit the Gaussian form-factor fit.
- A crystal of nearly spherical atoms (an ionic solid, a simple metal, a rare-gas solid), where
  `sph-tfva` might work.
- "Spherical hydrogens only" as a model of its own.
- These refinements with spherical basis functions and a crystal environment.

## References

- F. H. Allen and I. J. Bruno, *Acta Cryst.* B66, 380 (2010).
- H. Birkedal, D. Madsen, R. H. Mathiesen, K. Knudsen, H.-P. Weber, P. Pattison and
  D. Schwarzenbach, *Acta Cryst.* A60, 371 (2004).
- M. L. Chodkiewicz and K. Woźniak, *IUCrJ* 12, 74 (2025).
- I. Mayer and P. Salvador, *Chem. Phys. Lett.* 383, 368 (2004); P. Salvador and E. Ramos-Cordoba,
  *J. Chem. Phys.* 139, 071103 (2013).
- S. Swaminathan, B. M. Craven and R. K. McMullan, *Acta Cryst.* B40, 300 (1984).
- M. E. Wall, *IUCrJ* 3, 237 (2016).
