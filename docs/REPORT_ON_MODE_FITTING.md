# Fitting ADPs with rigid-body motion and internal modes

Atomic displacement parameters (ADPs) are usually refined atom by atom: six numbers per atom,
with nothing tying the atoms together. In a molecular crystal the atoms move together. The
molecule translates and librates as a whole (rigid-body motion, T, L, S), and it vibrates
internally. Each internal vibration is a normal mode of the molecule. The stiff modes (bond
stretches and bends) are well described by a calculated Hessian and need no refinement. The
soft modes (torsions, wags) are where the crystal matters.

Tonto refines the rigid-body motion directly against the structure factors, with the stiff
internal modes added from a Hessian and the amplitudes of the softest modes refined. This page
gives the equations, the keywords, where the code is, and what it gives on urea.

Units: Tonto works in atomic units (bohr, bohr², electron masses, hartree, ħ = 1) and prints
ADPs in Å², librations in degrees², and frequencies in cm⁻¹. Vectors are columns; ᵀ is the
transpose; ⟨ ⟩ is the thermal average, the average over the motion of the atoms in the crystal.


## 1. The model, from the start

### 1.1 What an ADP is

Atom $`i`$ vibrates about its mean position. Its displacement $`\mathbf{u}_i`$ is the vector from the mean
position to where it is at a given moment. The **anisotropic displacement parameter (ADP)** of
the atom is the 3 × 3 matrix of mean-square displacements

```math
U_i = \langle \mathbf{u}_i \mathbf{u}_i^{\mathsf T} \rangle . \qquad (1)
```

Its diagonal elements are the mean-square displacements along x, y and z; the off-diagonal ones
say how the motion along one axis correlates with another. U_i is symmetric, so it has six
independent elements. $`U_{\rm iso} = (U_{xx} + U_{yy} + U_{zz})/3`$ is its isotropic average.

The diffraction experiment sees $`U_i`$ through the structure factor. For a reflection with
scattering vector $`\mathbf{k}`$ ($`|\mathbf{k}| = 4\pi \sin\theta / \lambda_X`$, $`\lambda_X`$ the X-ray wavelength),
atom $`i`$ at mean position $`\mathbf{r}_i`$ scatters with its form factor $`f_i(\mathbf{k})`$ times the
**Debye–Waller factor**:

```math
F(\mathbf{k}) = \sum_i f_i(\mathbf{k})\, e^{i\mathbf{k}\cdot\mathbf{r}_i}\, e^{-\frac12 \mathbf{k}^{\mathsf T} U_i \mathbf{k}} . \qquad (2)
```

This holds when the displacement has a Gaussian distribution, true for harmonic motion.

### 1.2 Correlated motion

In a molecule the atoms do not move independently. Suppose that, at any moment, every
displacement is a linear function of a few **generalised coordinates** $`\mathbf{v} = (v_1, \dots, v_m)`$:

```math
\mathbf{u}_i = B_i \mathbf{v}, \qquad B_i \text{ a } 3\times m \text{ matrix fixed by the geometry.}
```

Putting this in (1) gives

```math
U_i = B_i \langle \mathbf{v}\mathbf{v}^{\mathsf T}\rangle B_i^{\mathsf T} = B_i\, \Sigma\, B_i^{\mathsf T}, \qquad \Sigma = \langle \mathbf{v}\mathbf{v}^{\mathsf T}\rangle . \qquad (3)
```

So the ADPs of all $`N`$ atoms ($`6N`$ numbers) follow from $`\Sigma`$, the $`m\times m`$ covariance of the
generalised coordinates ($`m(m+1)/2`$ numbers). This is the whole idea: refine $`\Sigma`$ instead of the $`U_i`$.

### 1.3 Rigid-body motion: $`\mathbf{t}`$ and $`\boldsymbol{\lambda}`$

The simplest correlated motion is the molecule moving as a rigid body, by a translation and a
rotation.

**The translation** $`\mathbf{t}`$ is a vector: every atom moves by the same $`\mathbf{t}`$.

**The rotation** is a rotation by a small angle $`\varphi`$ (in radians) about an axis through a fixed
**origin**, with unit vector $`\mathbf{n}`$ along the axis. An atom at position $`\mathbf{r}_i`$ from the
origin moves to $`R\,\mathbf{r}_i`$, where $`R`$ is the rotation matrix. For small $`\varphi`$, to first order,

```math
R\,\mathbf{r}_i = \mathbf{r}_i + \varphi\, \mathbf{n} \times \mathbf{r}_i ,
```

so the displacement is $`\varphi\,\mathbf{n}\times\mathbf{r}_i`$. Define the **rotation vector** (or
libration vector)

```math
\boldsymbol{\lambda} = \varphi\, \mathbf{n} :
```

its direction is the rotation axis, its length the angle in radians. The displacement from the
rotation is then $`\boldsymbol{\lambda}\times\mathbf{r}_i`$, linear in $`\boldsymbol{\lambda}`$.

Together, the rigid-body displacement of atom $`i`$ is

```math
\mathbf{u}_i = \mathbf{t} + \boldsymbol{\lambda} \times \mathbf{r}_i . \qquad (4)
```

The cross product is a matrix acting on $`\boldsymbol{\lambda}`$: $`\boldsymbol{\lambda}\times\mathbf{r}_i = A_i \boldsymbol{\lambda}`$ with

```math
A_i = \begin{pmatrix} 0 & z_i & -y_i \\ -z_i & 0 & x_i \\ y_i & -x_i & 0 \end{pmatrix}, \qquad \mathbf{r}_i = (x_i, y_i, z_i) \text{ from the origin.}
```

So (4) is of the form of §1.2 with six generalised coordinates $`\mathbf{v} = (\mathbf{t}, \boldsymbol{\lambda})`$ and
$`B_i = [\,1 \mid A_i\,]`$, a $`3\times 6`$ matrix ($`1`$ is the $`3\times 3`$ unit matrix).

The first-order step drops terms of order $`\varphi^2`$. These bend the paths of the atoms into arcs and
make bond lengths from a refinement appear slightly short (the libration correction); that
correction is not applied here.

### 1.4 T, L and S

The covariance $`\Sigma = \langle \mathbf{v}\mathbf{v}^{\mathsf T}\rangle`$ of $`\mathbf{v} = (\mathbf{t}, \boldsymbol{\lambda})`$ is $`6\times 6`$. Its $`3\times 3`$ blocks are the
conventional rigid-body tensors:

```math
\Sigma = \begin{pmatrix} T & S^{\mathsf T} \\ S & L \end{pmatrix}, \qquad
T = \langle \mathbf{t}\mathbf{t}^{\mathsf T}\rangle, \quad
L = \langle \boldsymbol{\lambda}\boldsymbol{\lambda}^{\mathsf T}\rangle, \quad
S = \langle \boldsymbol{\lambda}\mathbf{t}^{\mathsf T}\rangle ,
```

the translation (Å²), the libration (rad², printed in deg²), and the correlation of rotation with
translation (Å rad). Putting $`B_i = [\,1 \mid A_i\,]`$ into (3) and multiplying out the blocks gives
the **Schomaker–Trueblood** formula

```math
U_i^{\rm TLS} = T + A_i L A_i^{\mathsf T} + A_i S + S^{\mathsf T} A_i^{\mathsf T} . \qquad (5)
```

- **The origin** is the fragment's centre of mass. $`T`$ and $`S`$ depend on where it is; $`L`$ does not.
- **$`\mathrm{tr}\, S`$ is not determined.** Adding the same number $`c`$ to the three diagonal elements of
  $`S`$ adds $`c\,(A_i + A_i^{\mathsf T})`$ to every $`U_i`$, and that is zero because $`A_i`$ is antisymmetric. No
  measurement can fix it. Tonto removes that direction, which makes $`\mathrm{tr}\, S = 0`$. $`\Sigma`$ has
  21 independent elements, so 20 remain.
- **Site symmetry.** If the molecule sits on a special position, $`\Sigma`$ must carry the site symmetry.
  Tonto imposes it through the atoms:
  - each atom that is a symmetry image of another must get the symmetry-transformed U;
  - each atom on a special position must have a U unchanged by its site-symmetry operations.

  These are linear conditions on $`\Sigma`$, and the allowed $`\Sigma`$ are their solutions. Urea on its mm2 site
  keeps 8 of the 20.

### 1.5 Internal motion: normal modes

The molecule also vibrates internally. Let $`\mathbf{x}`$ be the $`3N`$ Cartesian displacements of all the
atoms from their equilibrium positions. Near a minimum of the energy $`E`$ the potential is
harmonic,

```math
E = E_0 + \tfrac12\, \mathbf{x}^{\mathsf T} H\, \mathbf{x}, \qquad H_{ab} = \frac{\partial^2 E}{\partial x_a\, \partial x_b},
```

where $`H`$ is the **Hessian**, the $`3N\times 3N`$ matrix of force constants (hartree/bohr²).

Let $`m_i`$ be the mass of atom $`i`$, and $`M`$ the $`3N\times 3N`$ diagonal matrix holding each atom's mass
three times. In **mass-weighted coordinates** $`\mathbf{q} = M^{1/2}\mathbf{x}`$ the kinetic energy is
$`\tfrac12 |\dot{\mathbf{q}}|^2`$, and the motion separates into independent oscillators, the **normal modes**:

1. **Mass-weight the Hessian:** $`H' = M^{-1/2} H M^{-1/2}`$.
2. **Remove the rigid-body motion.** For an isolated molecule the three translations and three
   rotations cost no energy; they are not vibrations. Build the six mass-weighted rigid-body
   displacement vectors ($`\sqrt{m_i}\,\mathbf{e}`$ for a translation along unit vector $`\mathbf{e}`$,
   $`\sqrt{m_i}\,\mathbf{e}\times\mathbf{r}_i`$ for a rotation about it), orthonormalise them, and project them out:
   $`H'' = P H' P`$ with $`P = 1 - \sum_{\mathbf{v}} \mathbf{v}\mathbf{v}^{\mathsf T}`$ over those six vectors $`\mathbf{v}`$. This imposes the
   **Eckart–Sayvetz conditions**: the remaining modes carry no net linear or angular momentum,
   so they are purely internal. The six become exact zero modes (five for a linear molecule).
3. **Diagonalise $`H''`$.** Each eigenvector $`\mathbf{l}_k`$ (a $`3N`$-vector) is a mode's shape; its
   eigenvalue is $`\omega_k^2`$, with $`\omega_k`$ the mode's angular frequency. $`\mathbf{l}_{ik}`$ denotes the
   three components of $`\mathbf{l}_k`$ on atom $`i`$.

In terms of the mode amplitudes $`Q_k`$ (the **normal coordinates**), atom $`i`$ moves by

```math
\mathbf{u}_i = \sum_k \mathbf{l}_{ik}\, Q_k / \sqrt{m_i},
```

which is again of the form of §1.2. Each mode is a harmonic oscillator. In quantum mechanics its
mean-square amplitude at temperature $`\Theta`$ is ($`\hbar = 1`$; $`k_B`$ Boltzmann's constant)

```math
\langle Q_k^2\rangle = \frac{1}{2\omega_k} \coth\!\left(\frac{\omega_k}{2 k_B \Theta}\right) . \qquad (6)
```

At $`\Theta = 0`$ this is $`1/(2\omega_k)`$, the zero-point motion; at high $`\Theta`$ it tends to
$`k_B\Theta/\omega_k^2`$, the classical value. Different modes are uncorrelated, so (1) gives

```math
U_i^{\rm modes} = \sum_k \mathbf{l}_{ik} \mathbf{l}_{ik}^{\mathsf T}\, \langle Q_k^2\rangle / m_i . \qquad (7)
```

**Stiff and soft modes.** A stiff mode (high ω, bond stretches and bends) is hardly affected by
the crystal, and (6) from a calculated Hessian is good. A soft mode (low ω, torsions and wags)
is sensitive to the crystal environment and to anharmonicity, and its amplitude should come from
the data. Tonto splits the modes at a cutoff $`\omega_c`$ (200 cm⁻¹ by default). $`U_i^{\rm high}`$ is (7)
summed over the stiff modes only:

```math
U_i^{\rm high} = \sum_{\omega_k > \omega_c} \mathbf{l}_{ik} \mathbf{l}_{ik}^{\mathsf T}\, \langle Q_k^2\rangle / m_i . \qquad (8)
```

A mode with $`\omega_k^2 \le 0`$ (an imaginary frequency) counts as soft. A Hessian taken at the crystal
geometry, which is not an energy minimum, can have such modes.

### 1.6 The full model

Adding the rigid-body motion (4) and the stiff internal modes, taken as uncorrelated, gives the
ADP of atom *i* as

```math
U_i = U_i^{\rm high} + B_i\, \Sigma\, B_i^{\mathsf T} , \qquad (9)
```

with $`\Sigma`$ refined and $`U_i^{\rm high}`$ fixed by the Hessian.

### 1.7 Soft modes

The soft modes left out of (8) can be put back with refined amplitudes. Take the $`K`$ softest
modes, imaginary ones first. Write $`\mathbf{d}_{ik} = \mathbf{l}_{ik}/\sqrt{m_i}`$ for the displacement of atom $`i`$
in mode $`k`$ per unit $`Q_k`$, and $`a_k = \langle Q_k^2\rangle`$ for the mode's mean-square amplitude. Each soft
mode adds one generalised coordinate $`Q_k`$ to $`\mathbf{v}`$ and one column $`\mathbf{d}_{ik}`$ to $`B_i`$. The modes
are taken as uncorrelated with each other and with the rigid-body motion, so only the amplitudes
enter, and (9) becomes

```math
U_i = U_i^{\rm high} + B_i\, \Sigma\, B_i^{\mathsf T} + \sum_{k=1}^{K} a_k\, \mathbf{d}_{ik}\mathbf{d}_{ik}^{\mathsf T} , \qquad (10)
```

with $`\Sigma`$ the rigid-body covariance of §1.4 and $`a_1 \dots a_K`$ refined. A soft mode must have
$`\omega_k`$ below the cutoff $`\omega_c`$ of (8), or it would be counted twice; raising the cutoff moves
more modes from $`U^{\rm high}`$ into the refinement. The cutoff is compared with the scaled frequency
(§1.9). Set it between the $`K`$-th and the next mode: a mode below the cutoff that is not among
the $`K`$ refined is in neither term of (10).

**The harmonic amplitude** $`a_k^0`$ is (6) at the mode's frequency. An imaginary mode has none.

**The implied frequency** of a refined $`a_k`$ is the $`\omega`$ for which (6) gives $`a_k`$. It says what
frequency the refined amplitude corresponds to, and is printed beside it.

**A soft mode can repeat the rigid-body motion.** Only the pattern $`\mathbf{d}_{ik}\mathbf{d}_{ik}^{\mathsf T}`$ over the
atoms reaches the data. If that pattern is a combination of the rigid-body patterns and the
other soft modes, the data cannot tell them apart, and the mode adds no parameter. Tonto finds
such modes as it finds $`\mathrm{tr}\, S`$ (§1.4), and drops them. In urea, a planar molecule, the
second out-of-plane NH₂ wag is a combination of the first wag and the libration about the
in-plane axes, so two soft modes refine as one.

### 1.8 Correlations, and keeping Σ positive

Equation (10) takes each soft mode as independent of the rigid-body motion and of the other soft
modes. They need not be: a torsion can move with the libration about the same axis, and the data
can correct the Hessian's mode shapes by mixing two nearly degenerate soft modes. Both are put in
by making the soft-mode amplitudes $`Q_k`$ generalised coordinates in their own right, as in §1.2:

```math
\mathbf{v} = (\mathbf{t}, \boldsymbol{\lambda}, Q_1, \dots, Q_K), \qquad
B_i = [\,1 \mid A_i \mid \mathbf{d}_{i1} \cdots \mathbf{d}_{iK}\,], \qquad
U_i = U_i^{\rm high} + B_i\, \Sigma\, B_i^{\mathsf T} , \qquad (11)
```

with $`\Sigma = \langle \mathbf{v}\mathbf{v}^{\mathsf T}\rangle`$ now $`(6+K)\times(6+K)`$:

```math
\Sigma = \begin{pmatrix} T & S^{\mathsf T} & C_t^{\mathsf T} \\ S & L & C_\lambda^{\mathsf T} \\ C_t & C_\lambda & \Sigma_Q \end{pmatrix} .
```

$`C_t`$ and $`C_\lambda`$ ($`K\times 3`$) are the correlations of each mode with the translation and the
libration. $`\Sigma_Q`$ ($`K\times K`$) has the amplitudes $`a_k = \langle Q_k^2\rangle`$ on its diagonal and the
mixing of the modes off it. With $`C_t`$, $`C_\lambda`$ and the off-diagonal $`\Sigma_Q`$ zero, (11) is (10).

**The correlation coefficient** of coordinates $`p`$ and $`q`$ is
$`\rho_{pq} = \Sigma_{pq} / (\Sigma_{pp}\Sigma_{qq})^{1/2}`$, between −1 and 1. The cross terms are
restrained toward zero through it (§2.4).

**Σ must be positive semidefinite**: $`\mathbf{w}^{\mathsf T}\Sigma\,\mathbf{w} \ge 0`$ for every vector $`\mathbf{w}`$, since
$`\mathbf{w}^{\mathsf T}\Sigma\,\mathbf{w} = \langle (\mathbf{w}\cdot\mathbf{v})^2\rangle`$ is a mean-square displacement. A negative soft-mode
amplitude breaks it. So does any negative eigenvalue of Σ. §2.4 says how this is kept.

### 1.9 Frequency scaling

Calculated harmonic frequencies are too high, so (8) is too small. The frequencies can be
multiplied by a scale factor before (6) is used. The default is the value of Scott and Radom
(1996) for the method, when Tonto made the Hessian itself:

| method | scale factor |
|---|---|
| HF | 0.8953 |
| B3LYP | 0.9614 |
| BLYP | 0.9945 |
| other | 1 |

They were fitted for the 6-31G(d) basis and are applied whatever the basis. For a Hessian read
in from a file, the method is unknown and the default is 1.

### 1.10 A Hessian by finite differences

Without an external program, Tonto makes the Hessian from its own SCF energies by central
differences with step $`h`$ (0.01 bohr by default). Writing $`E(+a)`$ for the energy with coordinate $`a`$
moved by $`+h`$, and so on:

```math
H_{aa} = \frac{E(+a) + E(-a) - 2E_0}{h^2}, \qquad
H_{ab} = \frac{E(+a,+b) - E(+a,-b) - E(-a,+b) + E(-a,-b)}{4h^2} .
```

**Translational invariance.** Moving the whole molecule does not change E, so each atom's block
row of H sums to zero over the atoms. Tonto uses this to get the last atom's rows, so N atoms
need $`2n + 2n(n-1)`$ SCFs with $`n = 3(N-1)`$. It holds at any geometry, but not with cluster charges
or an applied field; then every coordinate is differenced.

The **gradient** $`\partial E/\partial x_a`$ comes from the same energies, $`[E(+a) - E(-a)]/2h`$, and is printed.
Rotational invariance gives a similar sum rule only where the gradient is zero. At a geometry
that is not stationary, such as a crystal geometry, the rotations are not exact zero modes of H,
and the projection in §1.5 step 2 is an approximation.


## 2. Refining against the structure factors

### 2.1 What is minimised

For each measured reflection the refinement compares the observed structure factor amplitude
$`F_{\rm obs}`$ with the calculated one, $`F_{\rm calc}`$ from (2), with weight $`w = 1/\sigma^2`$, $`\sigma`$ the
measurement's standard uncertainty. With $`\Delta F = F_{\rm obs} - F_{\rm calc}`$, it minimises

```math
\chi^2 = \sum_{\rm reflections} w\, \Delta F^2 .
```

Two numbers summarise the fit. **GoF** (goodness of fit) counts the parameters:

```math
\mathrm{GoF} = \left( \frac{\chi^2}{N_{\rm refl} - N_{\rm param}} \right)^{1/2},
```

and is 1 for a model that fits to within the measurement errors.
$`R(F) = \sum|\Delta F| \,/\, \sum|F_{\rm obs}|`$ does
not count the parameters or use the weights. **Compare models by GoF, not R**: R can hide a
worse fit. Whether a model with more parameters is significantly better is decided by
Hamilton's test.

### 2.2 Parameters and the Jacobian

The refinement's parameters $`\mathbf{p}`$ are kept explicitly. The model vector $`\mathbf{X}`$, the positions and
ADPs of all the atoms, is made from them by

```math
\mathbf{X} = \mathbf{X}_0 + J\,\mathbf{p}, \qquad J = \partial\mathbf{X}/\partial\mathbf{p} \text{ (the Jacobian).} \qquad (13)
```

- **Free ADPs:** J has one unit column per refined component of X.
- **TLS model:** each atom keeps its three position columns. Its six ADP columns are replaced
  by columns shared by all the atoms, one per allowed $`\Sigma`$ parameter: by (9),
  $`\partial U_i/\partial\Sigma = \partial(B_i\Sigma B_i^{\mathsf T})/\partial\Sigma`$. $`\mathbf{X}_0`$ holds $`U^{\rm high}`$ on the ADP rows.
- **Soft modes:** one more shared column per mode, $`\partial U_i/\partial a_k = \mathbf{d}_{ik}\mathbf{d}_{ik}^{\mathsf T}`$ by (10).
  With the correlations, one column per element of the larger Σ of (11).

### 2.3 Solving

From the derivatives $`\partial F_{\rm calc}/\partial\mathbf{X}`$, the change in $`\mathbf{p}`$ that best lowers $`\chi^2`$ to first
order solves the **normal equations**:

```math
D = \frac{\partial F_{\rm calc}}{\partial \mathbf{X}}\, J, \qquad A = D^{\mathsf T} W D, \qquad \mathbf{b} = D^{\mathsf T} W\, \Delta\mathbf{F}, \qquad A\, \Delta\mathbf{p} = \mathbf{b},
```

with $`W`$ the diagonal matrix of the weights. Eigenvalues of $`A`$ near zero (directions the data do
not determine) are filtered out. The covariance of $`\mathbf{p}`$ is $`C = A^{-1}`$ scaled by GoF², and the esd of
each parameter is the square root of its diagonal element. The covariance of $`\mathbf{X}`$ follows as
$`J C J^{\mathsf T}`$, which gives every atomic $`U`$ an esd even though only $`\Sigma`$ and the $`a_k`$ are refined. Each
refinement cycle starts by putting the ADPs on the model: $`\Sigma`$ is fitted to the current
$`U - U^{\rm high}`$, and $`U`$ is set to $`U^{\rm high} + B\,\Sigma\,B^{\mathsf T}`$.

### 2.4 Restraints

Some parameters are held toward a target instead of being left free. Each restraint is a linear
function $`g_j`$ of the elements of Σ, held toward a target $`g_j^0`$ with an uncertainty $`\sigma_j`$. The
refinement minimises

```math
\chi^2 + \sum_j \frac{(g_j - g_j^0)^2}{\sigma_j^2} . \qquad (14)
```

There are three kinds. Each uses a **scale** $`c_p`$ for coordinate $`p`$ of $`\mathbf{v}`$: its diagonal element
$`\Sigma_{pp}`$ for a translation or libration (at least 1/1000 of the largest diagonal element of
$`T`$ or $`L`$), and for a soft mode the harmonic amplitude (6) at its $`|\omega_k|`$.

- **Soft-mode amplitudes**, $`g = a_k`$, held toward the harmonic amplitude $`a_k^0`$ with uncertainty
  $`\sigma_k = f_a\, a_k^0`$. Here $`a_k^0`$ is (6) evaluated at the mode's harmonic frequency $`\omega_k`$: the
  restraint is on the amplitude, the quantity the ADPs are linear in, and $`\omega_k`$ fixes its target.
  $`f_a`$ is `soft_mode_restraint=`, 0.5 by default, one number for all modes. An imaginary mode
  has no $`a_k^0`$ and is not restrained.
- **Cross terms** (§1.8), $`g = \Sigma_{pq}`$, held toward 0 with uncertainty
  $`\sigma_{pq} = f_\rho\, (c_p c_q)^{1/2}`$. $`f_\rho`$ is `correlation_restraint=`, 0.5 by default, one number
  for all pairs. Since $`(c_p c_q)^{1/2}`$ is the largest value $`|\Sigma_{pq}|`$ can have, this is the same as
  holding the correlation coefficient $`\rho_{pq}`$ toward 0 with uncertainty $`f_\rho`$.
- **Positivity.** At the start of each refinement cycle Σ is diagonalised. For each eigenvector
  $`\mathbf{w}`$ with a negative eigenvalue, $`g = \mathbf{w}^{\mathsf T}\Sigma\,\mathbf{w}`$ is held toward 0 with uncertainty
  $`\sigma_w = \sum_p w_p^2 c_p / 100`$. This holds Σ on the boundary $`\Sigma \ge 0`$ while the data push
  against it. When they stop pushing, the eigenvalue comes out positive and the restraint is
  dropped at the next cycle. `use_positive_tls_sigma=` switches it, on by default.

**What a restraint means.** A restraint is an extra observation, the same as a bond-length
restraint in a conventional refinement: alongside the reflections, the fit is told "this
quantity is $`g^0`$, give or take $`\sigma`$". If the reflections determine the quantity much better than
$`\sigma`$, they win and the restraint hardly matters. If they hardly determine it, the restraint
decides it, and the quantity stays near $`g^0`$ instead of drifting to wherever the noise takes it.

For the cross terms the target is 0: the model of (10), with the motions independent, is the
starting assumption, and the data must show a correlation before the refinement accepts one.
Each cross term adds $`(\Sigma_{pq}/\sigma_{pq})^2 = (\rho_{pq}/f_\rho)^2`$ to $`\chi^2`$. With $`f_\rho = 0.5`$ that is 1 at $`\rho = 0.5`$ and 4 at
$`\rho = 1`$. It pulls smoothly toward 0, more strongly the larger $`\rho`$; there is no threshold. The
term is small, which is what "weak" means. Whether it matters depends on how well the data fix
$`\rho`$. If they give $`\rho`$ with an esd of 0.06, moving it 0.1 away from the data's value costs
$`(0.1/0.06)^2 \approx 2.8`$ in $`\chi^2`$, against about 0.2 gained in the restraint, so the data decide.
Only a correlation the data barely determine is held near 0. The value 0.5 is a choice; §5 shows
how the results depend on it.

**How the restraints stay linear.** The restraint is on $`\Sigma_{pq}`$, which is linear in the parameters,
not on $`\rho_{pq}`$, which is not. Its uncertainty $`\sigma_{pq}`$ uses the scales $`c_p`$ and $`c_q`$ of the current Σ, and these are
fixed for the cycle and updated before the next. So within a cycle every restraint is an exact
linear observation and nothing is linearised. At convergence it holds $`\rho_{pq}`$ toward 0 with
uncertainty $`f_\rho`$; the change of the scales with the parameters is not differentiated, which
for a weak restraint makes no practical difference. The positivity restraint works the same way:
the eigenvector $`\mathbf{w}`$ is fixed for the cycle, and $`\mathbf{w}^{\mathsf T}\Sigma\,\mathbf{w}`$ is linear in Σ.

**Why positivity is a restraint and not Σ = C Cᵀ.** Writing Σ as $`C C^{\mathsf T}`$ with $`C`$ lower triangular
(its Cholesky factor) and refining $`C`$ makes Σ positive semidefinite by construction. Tonto does
not do this, for three reasons.
- **Site symmetry.** The symmetry conditions of §1.4 are linear in Σ, so the allowed Σ are found
  as the null space of a matrix. In $`C`$ they are quadratic, and there is no such simple answer.
- **The boundary at $`C = 0`$.** With no correlations Σ_Q is diagonal and the Cholesky form is $`a_k = c_k^2`$.
  Then $`\partial a_k/\partial c_k = 2c_k`$ is zero at $`c_k = 0`$, so the normal matrix is singular exactly
  where the data push an amplitude to zero, and the esd there is infinite. The restraint keeps the
  parameter linear and gives a finite esd on the boundary.
- **Linearity.** Σ linear in the parameters keeps the Jacobian of §2.2 constant, and the
  restraints and esds of this section unchanged.

In matrix form: the elements of Σ are a linear function of the parameters, $`\mathbf{s} = Q\,\mathbf{p}`$,
because the parameters are the combinations of them that survive §1.4 and §1.7. Each $`g_j`$ is
$`\mathbf{r}_j\cdot\mathbf{s}`$ for a fixed row $`\mathbf{r}_j`$. Stack the rows $`\mathbf{r}_j Q/\sigma_j`$ into a matrix $`G`$. Then the
added term has matrix $`R = G^{\mathsf T} G`$ in the parameters, and the normal equations of §2.3 become

```math
(A + R)\, \Delta\mathbf{p} = \mathbf{b} + G^{\mathsf T} \mathbf{e}, \qquad e_j = (g_j^0 - g_j)/\sigma_j ,
```

and the covariance is $`(A + R)^{-1}`$. A restrained parameter is only partly fitted to the data. The
**effective number of parameters** counts how much:

```math
p_{\rm eff} = \mathrm{tr}\left[(A + R)^{-1} A\right] ,
```

which is the number of parameters when $`R = 0`$, and less than it when the restraints bind. A
direction held at zero by the positivity restraint counts as almost no parameter.

### 2.5 How many parameters the data support

A model with more parameters always fits at least as well. Three measures say whether the
improvement is worth the parameters. Let $`n`$ be the number of reflections and $`p`$ the
number of parameters ($`p_{\rm eff}`$ for a restrained fit).

- **Akaike's information criterion**, $`\mathrm{AIC} = \chi^2 + 2p`$.
- **The Bayesian information criterion**, $`\mathrm{BIC} = \chi^2 + p \ln n`$. It charges more per
  parameter than AIC once $`n > 7`$.

  For both, the model with the smaller value is preferred.
- **Hamilton's test**, below.

**Hamilton's test.** Model $`a`$ has $`p_a`$ parameters and model $`b`$ adds $`k`$, so $`p_b = p_a + k`$.
The ratio of their weighted R factors is $`\mathcal R = (\chi^2_a / \chi^2_b)^{1/2}`$. Model $`b`$ is
significantly better, at significance level $`\alpha`$, when

```math
\mathcal R > \left[ 1 + \frac{k}{n - p_b}\, F_{k,\, n-p_b,\, \alpha} \right]^{1/2} , \qquad (15)
```

with $`F_{k,\,n-p_b,\,\alpha}`$ the point of the F distribution exceeded with probability $`\alpha`$.
This page uses $`\alpha = 0.005`$.

Tonto prints $`n`$, $`p_{\rm eff}`$, $`\chi^2`$, GoF, AIC and BIC after a TLS refinement; the Hamilton
ratio is formed from the $`\chi^2`$ of two runs.


## 3. Keywords

On the molecule (top level):

| keyword | default | meaning |
|---|---|---|
| `force_constants= { ... }` | | the Cartesian Hessian as 3N × 3N numbers, atomic units |
| `read_g09_fchk_file` | | reads the Hessian from a Gaussian checkpoint, if it holds one |
| `fd_hessian_step=` | 0.01 | step for `make_fd_hessian`, bohr |
| `make_fd_hessian` | | the Hessian by finite differences of SCF energies (after `scf`) |
| `normal_mode_analysis` | | the normal modes, rigid-body motion projected out |
| `put_normal_modes` | | prints the Hessian and the modes |
| `internal_adp_temperature=` | 0 | the temperature Θ in (6), Kelvin; 0 gives zero-point motion |
| `soft_mode_cutoff=` | 200 | modes below this, cm⁻¹, are soft |
| `frequency_scale_factor=` | see §1.9 | scales the frequencies in (6) |
| `make_internal_adps` | | makes U^high, eq. (8), and the soft modes of (10) |
| `put_internal_adps` | | prints U^high and how many modes are soft, imaginary and stiff |

In `xray_data=`:

| keyword | default | meaning |
|---|---|---|
| `adp_model=` | `free` | `free`: refine each atom's ADP; `tls`: the model (10), HAR only |
| `n_soft_modes=` | 0 | $`K`$ in (10): how many of the softest modes have refined amplitudes |
| `soft_mode_restraint=` | 0.5 | $`f_a`$ in §2.4: the amplitude restraint's $`\sigma`$ as a fraction of the harmonic amplitude |
| `use_soft_mode_correlations=` | `FALSE` | `TRUE` refines the cross terms of (11) |
| `correlation_restraint=` | 0.5 | $`f_\rho`$ in §2.4: the uncertainty of a correlation coefficient held toward 0 |
| `use_positive_tls_sigma=` | `TRUE` | holds Σ positive semidefinite (§2.4) |

A TLS Hirshfeld atom refinement with U^high from a Hessian, and the four softest modes refined:

```
   CIF= { file_name= urea.cif }
   process_CIF

   force_constants= { ... }          ! or: scf, then make_fd_hessian
   normal_mode_analysis
   internal_adp_temperature= 123
   frequency_scale_factor= 0.8953    ! RHF/6-31G(d) Hessian
   soft_mode_cutoff= 600             ! the four softest modes, after scaling
   make_internal_adps
   put_internal_adps

   crystal= {
      xray_data= {
         partition_model= oc-hirshfeld
         adp_model= tls
         n_soft_modes= 4
         ...
      }
   }
   scfdata= { ... }
   scf
   HAR_refinement
```

**On an explicit cluster.** The density can come from a cluster of whole molecules around the
central one, made by `create_cluster`. The TLS body is then the central molecule. Its stiff-mode
ADPs and soft modes are made from its own Hessian, so `make_internal_adps` goes before the
cluster is made; the other molecules' ADPs follow by symmetry:

```
   make_internal_adps
   cluster= {
      generation_method= within_radius
      radius= 2.5 Angstrom              ! urea: the six hydrogen-bonded neighbours
      defragment= true
      make_info
   }
   create_cluster
```

The tests `tests/long/urea_rhf_STO-3G_HAR_TLS`, `urea_rhf_STO-3G_HAR_TLS_soft_modes`,
`urea_rhf_STO-3G_HAR_TLS_soft_mode_correlations` and `urea_rhf_STO-3G_HAR_TLS_cluster` are
complete examples.


## 4. Where it is implemented

Each piece of the model, and the Tonto procedure that does it.

| piece | procedure |
|---|---|
| rigid-body vectors, mass-weighted, orthonormal | `VEC{ATOM}:make_rigid_body_modes` |
| P M P, projecting vectors out of a matrix | `MAT{REAL}:project_out_vectors` |
| normal modes with the projection | `MOLECULE.PROP:normal_mode_analysis` |
| Hessian by finite differences | `MOLECULE.PROP:make_FD_hessian` |
| U^high, eq. (8); its printout | `MOLECULE.PROP:make_internal_ADPs`, `put_internal_ADPs` |
| frequency scale factor | `MOLECULE.PROP:harmonic_frequency_scale` |
| U^high handed to the refinement | `MOLECULE.PROP:set_crystal_U_high`, `CRYSTAL:set_fragment_U_high` |
| ∂U_i/∂Σ for one atom | `CRYSTAL:TLS_response` |
| the elements of Σ, in order | `CRYSTAL:TLS_sigma_pairs` |
| allowed Σ and $`a_k`$, J columns, tr S, origin | `CRYSTAL:make_TLS_jacobian_columns` |
| p and X₀; ADPs put on the model | `CRYSTAL:set_refinement_parameters` |
| soft modes chosen, $`\mathbf{d}_{ik}`$ and $`a_k^0`$ | `MOLECULE.PROP:make_soft_modes`, `CRYSTAL:set_soft_modes` |
| restraints (14) | `CRYSTAL:set_TLS_restraints`, `MAT{REAL}:solve_restrained_linear_equations` |
| T, L, S with esds | `CRYSTAL:put_TLS_results` |
| refined amplitudes, implied frequencies | `CRYSTAL:put_soft_mode_results`, `CRYSTAL:frequency_for_amplitude` |
| correlation coefficients | `CRYSTAL:put_soft_mode_correlations` |
| the TLS body: the central molecule of a cluster | `CRYSTAL:n_TLS_atoms`, `MOLECULE.PROP:set_crystal_U_high` |
| $`p_{\rm eff}`$, AIC, BIC | `LEAST_SQUARES:solve_normal_equations`, `CRYSTAL:put_model_selection` |
| explicit parameters, eq. (13); the solve | `LEAST_SQUARES` (`least_squares.foo`) |


## 5. Results

### Water

RHF/6-31G(d) Hessian from Gaussian. After the projection the six rigid-body modes are exactly
zero. The internal frequencies are 1826.4, 4070.3 and 4188.5 cm⁻¹, against Gaussian's 1826.6,
4070.6 and 4188.8; Tonto uses average atomic masses and Gaussian isotopic ones. At 0 K the
hydrogen U^high has principal values 0.0043 and 0.0038 Å² in the molecular plane and zero out of
it, where water has no internal mode. The O–H stretch alone gives ħ/2μω ≈ 0.0046 Å².

### Urea: stiff-mode ADPs

RHF/6-31G(d) Hessian by finite differences at the HAR geometry, 123 K, frequencies unscaled:

| atom | U^high, U_iso /Å² |
|---|---|
| O | 0.0005 |
| N | 0.0006 |
| C | 0.0010 |
| H1, H2 (cis to O) | 0.0117 |
| H3, H4 | 0.0067 |

The crystal geometry is planar, while gas-phase urea has pyramidal NH₂ groups. So the two NH₂
wags are imaginary (432i and 164i cm⁻¹) and are left out of U^high as soft modes. The gradient
there is 0.093 Eh/bohr. RHF frequencies are about 10 % high, so these U^high are about 10 % low.

### Urea: the refinements compared

Hirshfeld atom refinement against the 123 K X-ray data (817 reflections), with RHF
densities in the def2-SVP and def2-TZVP basis sets. There are two partitions of the density
into atoms: Hirshfeld (`oc-hirshfeld`) and the topological fuzzy Voronoi atoms of Salvador,
TFVA (`oc-salvador`). There are three ADP models:
- free: six parameters per atom;
- TLS with $`U^{\rm high} = 0`$: the rigid-body model (5) alone;
- TLS + U^high: the model (9), with $`U^{\rm high}`$ as above.

Every refinement also has the positions and a scale factor. The neutron bond lengths are from
Swaminathan, Craven and McMullan (1984).

For each refinement: the number of parameters, the fit, the N–H and C=O bond lengths, and the
hydrogens' U_iso.

| basis | partition | ADP model | parameters | GoF | R(F) | N–H1 /Å | N–H3 /Å | C=O /Å | U_iso H1 / H3 /Å² |
|---|---|---|---|---|---|---|---|---|---|
| def2-SVP | Hirshfeld | free | 27 | 3.304 | 0.0181 | 1.028(5) | 0.986(6) | 1.2558(4) | 0.054(4) / 0.048(3) |
| def2-SVP | Hirshfeld | TLS, U^high = 0 | 17 | 3.837 | 0.0184 | 1.025(5) | 0.994(5) | 1.2562(5) | 0.0334(15) / 0.0329(13) |
| def2-SVP | Hirshfeld | TLS + U^high | 17 | 3.508 | 0.0180 | 1.028(5) | 0.994(5) | 1.2559(4) | 0.0447(15) / 0.0399(13) |
| def2-SVP | TFVA | free | 27 | 3.541 | 0.0190 | 1.038(5) | 1.026(5) | 1.2557(4) | 0.050(4) / 0.042(2) |
| def2-SVP | TFVA | TLS, U^high = 0 | 17 | 3.883 | 0.0184 | 1.034(5) | 1.036(4) | 1.2564(5) | 0.0337(14) / 0.0332(12) |
| def2-SVP | TFVA | TLS + U^high | 17 | 3.641 | 0.0187 | 1.039(5) | 1.029(4) | 1.2560(4) | 0.0455(14) / 0.0398(13) |
| def2-TZVP | Hirshfeld | free | 27 | 2.935 | 0.0167 | 1.025(4) | 0.989(5) | 1.2560(4) | 0.054(3) / 0.046(2) |
| def2-TZVP | Hirshfeld | TLS, U^high = 0 | 17 | 3.462 | 0.0169 | 1.022(4) | 0.993(5) | 1.2564(4) | 0.0343(13) / 0.0326(11) |
| def2-TZVP | Hirshfeld | TLS + U^high | 17 | 3.095 | 0.0164 | 1.025(4) | 0.993(5) | 1.2561(4) | 0.0457(13) / 0.0395(11) |
| def2-TZVP | TFVA | free | 27 | 3.099 | 0.0170 | 1.033(4) | 1.017(4) | 1.2560(4) | 0.049(3) / 0.0402(19) |
| def2-TZVP | TFVA | TLS, U^high = 0 | 17 | 3.446 | 0.0166 | 1.028(4) | 1.025(4) | 1.2567(4) | 0.0347(12) / 0.0326(10) |
| def2-TZVP | TFVA | TLS + U^high | 17 | 3.169 | 0.0168 | 1.033(4) | 1.020(4) | 1.2563(4) | 0.0466(12) / 0.0391(10) |
| neutron | | | | | | 1.006 | 1.000 | | |

- **def2-TZVP fits better than def2-SVP** in every partition and ADP model, by 0.37–0.47 in GoF.
- **Hirshfeld fits better than TFVA** with free ADPs and with TLS + U^high. With TLS alone the
  two are about equal.
- **The stiff modes always help:** TLS + U^high has a lower GoF than TLS alone in all four
  basis and partition pairs.
- **Free ADPs fit significantly better than TLS + U^high** in all four. The Hamilton ratios (15)
  for the ten extra parameters are 1.068, 1.035, 1.061 and 1.029, against 1.016 needed at
  $`\alpha = 0.005`$.
- **N–H bond lengths depend on the partition, not the ADP model.** TFVA makes them 0.01–0.04 Å
  longer than Hirshfeld does, and further from neutron. The ADP model moves no N–H bond by more
  than 0.01 Å.
- **R(F) would mislead.** It ranks TLS + U^high level with free ADPs (0.0180 against 0.0181 for
  def2-SVP, Hirshfeld), where GoF and Hamilton's test say free is significantly better.

### Urea: ADPs against neutron

The neutron ADPs of urea at 123 K (Swaminathan, Craven and McMullan, 1984), scaled to the X-ray
data, are tabulated by Jayatilaka and Dittrich (2008, Table 6). The cell is tetragonal, so its
axes and Tonto's Cartesian axes coincide and the tensors compare component by component. Two
measures per atom. The first is the ratio of U_iso to the neutron U_iso, for the size. The second
is the similarity index of Whitten and Spackman (2006),

```math
S_{12} = 100 \left[ 1 - \frac{2^{3/2}\, \det(U_1^{-1} U_2^{-1})^{1/4}}{\det(U_1^{-1} + U_2^{-1})^{1/2}} \right],
```

which is 0 for identical displacement ellipsoids and grows as their shapes and orientations part.

For each refinement of the table above, its GoF and both measures for the two independent
hydrogens, H1 and H3.

| basis | partition | ADP model | GoF | H1: U_iso ratio | H1: S12 | H3: U_iso ratio | H3: S12 |
|---|---|---|---|---|---|---|---|
| def2-SVP | Hirshfeld | free | 3.304 | 1.47 | 8.05 | 1.44 | 7.06 |
| def2-SVP | Hirshfeld | TLS, U^high = 0 | 3.837 | 0.91 | 1.85 | 0.99 | 2.86 |
| def2-SVP | Hirshfeld | TLS + U^high | 3.508 | 1.22 | 0.77 | 1.20 | 0.50 |
| def2-SVP | TFVA | free | 3.541 | 1.37 | 4.23 | 1.26 | 0.97 |
| def2-SVP | TFVA | TLS, U^high = 0 | 3.883 | 0.92 | 1.68 | 1.00 | 2.76 |
| def2-SVP | TFVA | TLS + U^high | 3.641 | 1.24 | 0.88 | 1.20 | 0.48 |
| def2-TZVP | Hirshfeld | free | 2.935 | 1.48 | 6.13 | 1.37 | 4.74 |
| def2-TZVP | Hirshfeld | TLS, U^high = 0 | 3.462 | 0.94 | 1.84 | 0.98 | 2.88 |
| def2-TZVP | Hirshfeld | TLS + U^high | 3.095 | 1.25 | 0.87 | 1.18 | 0.45 |
| def2-TZVP | TFVA | free | 3.099 | 1.35 | 2.84 | 1.21 | 0.70 |
| def2-TZVP | TFVA | TLS, U^high = 0 | 3.446 | 0.95 | 1.81 | 0.98 | 2.86 |
| def2-TZVP | TFVA | TLS + U^high | 3.169 | 1.28 | 1.00 | 1.17 | 0.40 |

O, N and C agree with neutron in every refinement: U_iso ratios 0.99–1.01, S12 at most 0.03.

- **The ADP model decides the hydrogen ADPs**; the basis and partition matter much less.
- **Free hydrogens are 21–48 % too large.** Their shapes are poor with Hirshfeld (S12 4.7–8.1)
  and better with TFVA (S12 0.7–4.2).
- **TLS + U^high gives the best hydrogen shapes**, S12 0.4–1.0 in every case, with sizes 17–28 %
  too large.
- **TLS alone** gets the hydrogen sizes within 9 %, but the shapes are worse (S12 1.7–2.9).

So the model that fits the data best, free ADPs, gives the worst hydrogen ADPs.

### Urea: soft modes

The modes of the RHF/6-31G(d) Hessian at the crystal geometry, softest first, with the
molecular plane the mirror plane normal to (1, −1, 0):

| mode | ω /cm⁻¹ | motion | share of the motion in C, N and O |
|---|---|---|---|
| 1 | 432i | NH₂ wag out of the plane, the two groups opposite | 0.12 |
| 2 | 164i | NH₂ wag out of the plane, the two groups together | 0.08 |
| 3 | 559 | in-plane N–C–N bend | 0.65 |
| 4 | 645 | in-plane C=O bend | 0.83 |

The share is the fraction of the mode's mass-weighted motion $`\sum_i |\mathbf{l}_{ik}|^2`$ carried by
the heavy atoms. The hydrogens move furthest in every mode, being lightest, so the share, not the
size of the displacements, says what kind of motion a mode is.

Hirshfeld partition, the model (10) with the $`K`$ softest modes refined, `soft_mode_restraint=`
0.5. The cutoff is 200 cm⁻¹ for $`K \le 2`$, 600 for $`K = 3`$ and 700 for $`K = 4`$, so modes 3 and 4
move from $`U^{\rm high}`$ into the refinement. $`K = 0`$ is TLS + U^high above.

For each $`K`$: the parameter count, the effective count, the fit, AIC and BIC, and the
hydrogen ADPs against neutron (U_iso ratio, then S12).

| basis | $`K`$ | parameters | $`p_{\rm eff}`$ | GoF | $`\chi^2`$ | AIC | BIC | H1: U_iso ratio, S12 | H3: U_iso ratio, S12 |
|---|---|---|---|---|---|---|---|---|---|
| def2-SVP | 0 | 17 | 17.00 | 3.508 | 9843 | 9877 | 9957 | 1.22, 0.77 | 1.20, 0.50 |
| def2-SVP | 1 | 18 | 18.00 | 3.481 | 9680 | 9716 | 9801 | 1.39, 1.54 | 1.31, 1.10 |
| def2-SVP | 2 | 18 | 18.00 | 3.481 | 9680 | 9716 | 9801 | 1.39, 1.54 | 1.31, 1.10 |
| def2-SVP | 3 | 19 | 18.94 | 3.471 | 9615 | 9652 | 9742 | 1.37, 1.41 | 1.32, 1.18 |
| def2-SVP | 4 | 20 | 19.71 | 3.428 | 9367 | 9407 | 9499 | 1.38, 1.61 | 1.32, 1.09 |
| def2-SVP | free | 27 | 27 | 3.304 | 8624 | 8678 | 8806 | 1.47, 8.05 | 1.44, 7.06 |
| def2-TZVP | 0 | 17 | 17.00 | 3.095 | 7664 | 7698 | 7778 | 1.25, 0.87 | 1.18, 0.45 |
| def2-TZVP | 1 | 18 | 18.00 | 3.069 | 7527 | 7563 | 7648 | 1.41, 1.64 | 1.29, 0.97 |
| def2-TZVP | 2 | 18 | 18.00 | 3.069 | 7527 | 7563 | 7648 | 1.41, 1.64 | 1.29, 0.97 |
| def2-TZVP | 3 | 19 | 18.94 | 3.065 | 7496 | 7534 | 7623 | 1.39, 1.55 | 1.29, 1.02 |
| def2-TZVP | 4 | 20 | 19.71 | 3.028 | 7308 | 7347 | 7440 | 1.39, 1.75 | 1.30, 0.97 |
| def2-TZVP | free | 27 | 27 | 2.935 | 6806 | 6860 | 6987 | 1.48, 6.13 | 1.37, 4.74 |

Hamilton's test (15) at $`\alpha = 0.005`$, each model against the one before it:

| basis | from | to | added parameters | ratio $`\mathcal R`$ | needed | significant |
|---|---|---|---|---|---|---|
| def2-SVP | K = 0 | K = 1 | 1 | 1.0084 | 1.0049 | yes |
| def2-SVP | K = 2 | K = 3 | 1 | 1.0034 | 1.0050 | no |
| def2-SVP | K = 3 | K = 4 | 1 | 1.0131 | 1.0050 | yes |
| def2-SVP | K = 4 | free | 7 | 1.0422 | 1.0129 | yes |
| def2-TZVP | K = 0 | K = 1 | 1 | 1.0090 | 1.0049 | yes |
| def2-TZVP | K = 2 | K = 3 | 1 | 1.0021 | 1.0050 | no |
| def2-TZVP | K = 3 | K = 4 | 1 | 1.0128 | 1.0050 | yes |
| def2-TZVP | K = 4 | free | 7 | 1.0362 | 1.0129 | yes |

The refined amplitudes at $`K = 4`$, in atomic units ($`m_e^{1/2}`$ bohr)². An imaginary mode has no
harmonic amplitude.

| basis | mode | ω /cm⁻¹ | harmonic $`a_k^0`$ | refined $`a_k`$ | implied ω /cm⁻¹ |
|---|---|---|---|---|---|
| def2-SVP | 1 | 432i | | 157(54) | 700 |
| def2-SVP | 2 | 164i | | 243(76) | 456 |
| def2-SVP | 3 | 559 | 197 | 207(96) | 532 |
| def2-SVP | 4 | 645 | 170 | 737(135) | 187 |
| def2-TZVP | 1 | 432i | | 130(47) | 843 |
| def2-TZVP | 2 | 164i | | 218(65) | 506 |
| def2-TZVP | 3 | 559 | 197 | 231(84) | 478 |
| def2-TZVP | 4 | 645 | 170 | 657(118) | 202 |

- **Two wags refine as one.** $`K = 2`$ gives the same fit and parameter count as $`K = 1`$: the
  second wag is a combination of the first and the in-plane libration (§1.7). Both amplitudes
  are printed, but only one combination of them is determined.
- **The wags are needed.** The first wag improves the fit significantly in both basis sets.
- **The in-plane bends differ.** Mode 3 is not determined by the data: it refines to 10(84)
  at $`K = 3`$ and 207(96) at $`K = 4`$ (def2-SVP), against a harmonic 197, and does not improve the
  fit. Mode 4 refines to about four times its harmonic amplitude, an implied frequency near
  200 cm⁻¹ instead of 645, and improves the fit significantly.
- **AIC and BIC agree with Hamilton's test.** Neither rises as modes are added. Both are lowest for free
  ADPs, which are still significantly better than $`K = 4`$.
- **The better fit gives worse hydrogen ADPs.** Each soft mode added moves the hydrogens further
  from neutron. The U_iso ratios rise from 1.18–1.25 to 1.29–1.41, and S12 from 0.45–0.87 to
  0.97–1.75.

So the refined soft modes, like free ADPs, lower GoF by making the hydrogens larger, not by
making them more like neutron. The next two sections test the soft modes against the crystal
environment and against the crystal's own vibrations.

### Urea: the crystal environment

The density so far is that of an isolated molecule. The crystal around a molecule polarises its
density, most of all at the hydrogens, which make the hydrogen bonds. Two ways to include it:

- **cluster charges:** point charges and dipoles at the atoms of the surrounding molecules
  within 8 Å, made self-consistently from the molecule's own density (`use_SC_cluster_charges=`);
- **an explicit cluster:** the central molecule and the six it hydrogen-bonds to, all computed
  quantum mechanically (`create_cluster` with `radius= 2.5 Angstrom`). The Hirshfeld atoms of the
  central molecule are taken from the density of the whole cluster.

All refinements here use def2-SVP and the Hirshfeld partition. B3LYP is the Gaussian form of the
functional (`b3lypgx`, `b3lypgc`).

For each method and environment, free ADPs and TLS + U^high: the fit, the N–H bond lengths, and
the hydrogen ADPs against neutron (U_iso ratio, then S12).

| method | environment | ADP model | parameters | GoF | N–H1 /Å | N–H3 /Å | H1: U_iso ratio, S12 | H3: U_iso ratio, S12 |
|---|---|---|---|---|---|---|---|---|
| RHF | isolated | free | 27 | 3.304 | 1.028(5) | 0.986(6) | 1.47, 8.05 | 1.44, 7.06 |
| RHF | isolated | TLS + U^high | 17 | 3.508 | 1.028(5) | 0.994(5) | 1.22, 0.77 | 1.20, 0.50 |
| RHF | cluster charges | free | 27 | 3.307 | 1.017(5) | 0.999(5) | 1.29, 4.48 | 1.26, 3.37 |
| RHF | cluster charges | TLS + U^high | 17 | 3.383 | 1.014(5) | 1.005(5) | 1.24, 0.75 | 1.19, 0.48 |
| RHF | 7-molecule cluster | free | 27 | 3.217 | 1.023(5) | 0.997(5) | 1.29, 3.06 | 1.25, 3.88 |
| RHF | 7-molecule cluster | TLS + U^high | 17 | 3.284 | 1.020(5) | 1.004(5) | 1.24, 0.83 | 1.18, 0.45 |
| B3LYP | isolated | free | 27 | 2.709 | 1.018(4) | 0.992(5) | 1.30, 3.81 | 1.33, 4.53 |
| B3LYP | isolated | TLS + U^high | 17 | 2.833 | 1.020(4) | 0.995(4) | 1.23, 0.78 | 1.19, 0.48 |
| B3LYP | cluster charges | free | 27 | 2.551 | 1.007(4) | 1.001(4) | 1.15, 1.96 | 1.18, 1.68 |
| B3LYP | cluster charges | TLS + U^high | 17 | 2.596 | 1.006(4) | 1.005(4) | 1.24, 0.78 | 1.19, 0.50 |
| neutron | | | | | 1.006 | 1.000 | 1, 0 | 1, 0 |

- **The environment brings the N–H bonds to neutron.** B3LYP with cluster charges gives N–H1
  1.007(4) and N–H3 1.001(4) Å against neutron 1.006 and 1.000 Å.
- **B3LYP fits better than RHF**, by 0.6–0.8 in GoF, isolated and with cluster charges.
- **The environment brings the free hydrogen ADPs toward neutron.** S12 falls from 3.8–8.1 for the
  isolated molecule to 1.7–2.0 with B3LYP and cluster charges.
- **TLS + U^high gives nearly the same hydrogen ADPs in every case**: U_iso ratios 1.18–1.24,
  S12 0.45–0.83. They depend on the motion model, not on the density.
- **The gap between free and TLS + U^high narrows.** The Hamilton ratio (15) for the ten extra
  parameters falls from 1.068 (RHF, isolated) to 1.024 (B3LYP, cluster charges), against 1.016
  needed. Free ADPs are still significantly better.
- **RI-J with COSX** reproduces the exact 7-molecule refinements to 0.0004 in GoF and every
  bond to 0.001 Å; on this cluster it is slower than the exact method.

**The soft modes in the crystal environment.** The same sequence of $`K`$ soft modes, with Σ held
positive (§2.4). Each entry is GoF, with $`p_{\rm eff}`$ in brackets.

| method | environment | K = 0 | K = 1 | K = 2 | K = 3 | K = 4 | free |
|---|---|---|---|---|---|---|---|
| RHF | isolated | 3.508 (17.0) | 3.481 (18.0) | 3.481 (18.0) | 3.471 (18.9) | 3.428 (19.7) | 3.304 (27) |
| RHF | cluster charges | 3.383 (17.0) | 3.385 (17.0) | 3.385 (17.0) | 3.382 (18.0) | 3.358 (18.7) | 3.307 (27) |
| RHF | 7-molecule cluster | 3.284 (17.0) | 3.286 (18.0) | 3.286 (17.0) | 3.287 (18.9) | 3.264 (18.7) | 3.217 (27) |
| B3LYP | isolated | 2.833 (17.0) | 2.828 (18.0) | 2.828 (18.0) | 2.802 (18.0) | 2.795 (18.8) | 2.709 (27) |
| B3LYP | cluster charges | 2.596 (17.0) | 2.598 (17.0) | 2.599 (17.0) | 2.577 (17.0) | 2.578 (17.9) | 2.551 (27) |

- **In the crystal environment the NH₂ wags are not wanted.** With cluster charges their refined
  amplitudes are zero or would be negative. The positivity restraint holds them at zero, and
  $`p_{\rm eff}`$ stays at 17. Hamilton's test finds no improvement from $`K = 0`$ to $`K = 2`$.
- This does not mean the wags are still in the crystal. The crystal's own NH₂ modes have the
  amplitudes the isolated-molecule refinements found (next section but one). Why the refinements
  with cluster charges do not use the wags is open.
- **An in-plane bend is still wanted:** mode 4 with RHF, mode 3 with B3LYP. Each improves the fit
  significantly by Hamilton's test at $`\alpha = 0.005`$.

### Urea: correlations

The same refinements with the soft modes' correlations refined as well (§1.8).

| method | environment | K | parameters | $`p_{\rm eff}`$ | GoF | H1: U_iso ratio, S12 | H3: U_iso ratio, S12 |
|---|---|---|---|---|---|---|---|
| RHF | isolated | 3 | 20 | 19.6 | 3.412 | 1.39, 2.10 | 1.32, 1.24 |
| RHF | cluster charges | 3 | 20 | 19.6 | 3.359 | 1.24, 1.17 | 1.19, 0.56 |
| RHF | 7-molecule cluster | 3 | 20 | 19.6 | 3.267 | 1.26, 1.25 | 1.19, 0.56 |
| B3LYP | isolated | 3 | 20 | 19.6 | 2.790 | 1.29, 1.12 | 1.25, 0.76 |
| B3LYP | cluster charges | 1 | 18 | 18.0 | 2.591 | 1.18, 0.50 | 1.14, 0.31 |
| B3LYP | cluster charges | 3 | 20 | 19.6 | 2.574 | 1.16, 0.45 | 1.14, 0.33 |
| B3LYP | cluster charges | 4 | 23 | 21.2 | 2.578 | 1.17, 0.42 | 1.14, 0.37 |

- **Three soft modes with their correlations are the best motion model.** In every environment
  $`K = 3`$ with correlations is significantly better than $`K = 2`$; $`K = 4`$ is not better
  than $`K = 3`$.
- **B3LYP with cluster charges and $`K = 3`$ with correlations comes closest to free ADPs:** GoF
  2.574 with 20 parameters against 2.551 with 27. The Hamilton ratio is 1.0134 against 1.0129
  needed, so free ADPs are only just significantly better. Its hydrogen ADPs are the best of any
  refinement here: U_iso ratios 1.14–1.16, S12 0.33–0.45.
- **What the correlations say.** With one soft mode, the first NH₂ wag, B3LYP with cluster
  charges refines the wag to a positive amplitude, 95(28), correlated with libration about the
  C=O axis by $`\rho = -0.49(15)`$. The same ADPs without the correlation need a negative
  amplitude. The hydrogens move out of the plane less than libration alone would carry them,
  as expected when they are held by hydrogen bonds.
- **The correlation restraint does not matter here.** With $`K = 3`$, widths
  $`f_\rho = 0.25, 0.5, 1, 2`$ and 10 give the same GoF to 0.0001 and the same correlations
  to the printed digit. The data determine every correlation to an esd of 0.03–0.11.

### Urea: the crystal's own vibrations

The vibrations of crystalline urea were measured by inelastic neutron scattering (INS) and
calculated by periodic DFT by Johnson, Parlinski, Natkaniec and Hudson (2003). In the crystal:

- the lattice modes, the motion of whole molecules, reach 159 cm⁻¹ measured and about
  200 cm⁻¹ calculated; above them there are only internal modes;
- the NH₂ torsions, wags and rocks lie in two broad INS bands near 480 and 670 cm⁻¹, pushed up
  by the hydrogen bonds; for the planar isolated molecule the wags are imaginary;
- no internal mode lies below about 416 cm⁻¹.

The harmonic amplitude (6) at 123 K is 230 at 480 cm⁻¹ and 164 at 670 cm⁻¹, in atomic units.

For the wags and the C=O bend: the frequency in the isolated molecule, where the crystal has its
modes, and the amplitude refined for the isolated molecule with the frequency it implies.

| mode | isolated molecule, RHF/6-31G(d) /cm⁻¹ | crystal /cm⁻¹ | refined amplitude, isolated molecule, $`K = 4`$ (implied /cm⁻¹) |
|---|---|---|---|
| NH₂ wags, 1 and 2 | 432i, 164i | NH₂ bands near 480 and 670 | def2-SVP: 157(54) and 243(76) (700, 456); def2-TZVP: 130(47) and 218(65) (843, 506) |
| C=O bend, 4 | 645 | all internal modes above 416 | def2-SVP: 737(135) (187); def2-TZVP: 657(118) (202) |

- **The wag amplitudes refined for the isolated molecule are those of the crystal's NH₂ modes.**
  Their implied frequencies, 456–843 cm⁻¹, bracket the two INS bands.
- **The large C=O-bend amplitude is not that bend's vibration.** It implies about 200 cm⁻¹, where
  the crystal has lattice modes only. It stands for motion the model otherwise lacks.
- **A better restraint target for the wags** is the crystal's harmonic amplitude, from an INS
  frequency or a periodic Hessian, instead of no restraint at all (§6).

### Urea: the rigid-body tensors

The rigid-body motion from def2-SVP, Hirshfeld, TLS + U^high, about the centre of mass (8
parameters on the mm2 site). Rows of $`S`$ are components of $`\boldsymbol{\lambda}`$, columns of $`\mathbf{t}`$.

```math
T = \begin{pmatrix} 0.01402(7) & -0.00045(9) & 0 \\ -0.00045(9) & 0.01402(7) & 0 \\ 0 & 0 & 0.00591(4) \end{pmatrix} \text{Å}^2, \qquad
L = \begin{pmatrix} 19.8(17) & 12.2(17) & 0 \\ 12.2(17) & 19.8(17) & 0 \\ 0 & 0 & 44(4) \end{pmatrix} \text{deg}^2,
```

```math
S = \begin{pmatrix} -0.18(2) & 0.12(2) & 0 \\ -0.12(2) & 0.18(2) & 0 \\ 0 & 0 & 0 \end{pmatrix} \text{Å deg}.
```

$`L`$ has eigenvalues 7.6, 32 and 44 deg², the largest about the C=O axis; the rms libration is 5.3°.
$`S`$ has zero trace.


## 6. What is not done yet

- Restraining the NH₂ wags toward the crystal's harmonic amplitudes, from INS frequencies or a
  periodic Hessian, in place of leaving them free.
- B3LYP on the 7-molecule cluster, and the def2-TZVP basis for the crystal-environment tables.
- A molecule with a methyl torsion, where the first soft mode should be the torsion.
- T, L and S reported at the centre of reaction as well as at the centre of mass.
- An ORCA Hessian reader.
- One TLS group per fragment only; no segmented (attached-group) TLS.


## References

- V. Schomaker and K. N. Trueblood, *Acta Cryst.* B24, 63 (1968): rigid-body motion, T, L, S.
- A. P. Scott and L. Radom, *J. Phys. Chem.* 100, 16502 (1996): harmonic frequency scale factors.
- W. C. Hamilton, *Acta Cryst.* 18, 502 (1965): significance tests on the crystallographic R factor.
- S. Swaminathan, B. M. Craven and R. K. McMullan, *Acta Cryst.* B40, 300 (1984): urea, neutron, 123 K.
- D. Jayatilaka and B. Dittrich, *Acta Cryst.* A64, 383 (2008): Hirshfeld atom refinement;
  Table 6 gives the urea neutron ADPs used here.
- A. E. Whitten and M. A. Spackman, *Acta Cryst.* B62, 875 (2006): the S12 similarity index.
- M. R. Johnson, K. Parlinski, I. Natkaniec and B. S. Hudson, *Chem. Phys.* 291, 53 (2003): INS and
  periodic DFT of crystalline urea.
