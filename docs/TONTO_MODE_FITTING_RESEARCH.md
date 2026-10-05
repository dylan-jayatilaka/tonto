# Fitting ADPs with rigid-body motion and internal modes

Atomic displacement parameters (ADPs) are usually refined atom by atom: six numbers per atom,
with nothing tying the atoms together. In a molecular crystal the atoms move together. The
molecule translates and librates as a whole (rigid-body motion, T, L, S), and it vibrates
internally. Each internal vibration is a normal mode of the molecule. The stiff modes (bond
stretches and bends) are well described by a calculated Hessian and need no refinement. The
soft modes (torsions, wags) are where the crystal matters.

Tonto refines the rigid-body motion directly against the structure factors, with the stiff
internal modes added from a Hessian. This page gives the equations, the keywords, where the
code is, and what it gives on urea. The soft modes are not yet refined; see the last section.

Units: Tonto works in atomic units (bohr, bohr², electron masses, hartree, ħ = 1) and prints
ADPs in Å², librations in degrees², and frequencies in cm⁻¹. Vectors are columns; ᵀ is the
transpose; ⟨ ⟩ is the thermal average, the average over the motion of the atoms in the crystal.


## 1. The model, from the start

### 1.1 What an ADP is

Atom $i$ vibrates about its mean position. Its displacement $\mathbf{u}_i$ is the vector from the mean
position to where it is at a given moment. The **anisotropic displacement parameter (ADP)** of
the atom is the 3 × 3 matrix of mean-square displacements

$$U_i = \langle \mathbf{u}_i \mathbf{u}_i^{\mathsf T} \rangle . \tag{1}$$

Its diagonal elements are the mean-square displacements along x, y and z; the off-diagonal ones
say how the motion along one axis correlates with another. U_i is symmetric, so it has six
independent elements. $U_{\rm iso} = (U_{xx} + U_{yy} + U_{zz})/3$ is its isotropic average.

The diffraction experiment sees $U_i$ through the structure factor. For a reflection with
scattering vector $\mathbf{k}$ ($|\mathbf{k}| = 4\pi \sin\theta / \lambda_X$, $\lambda_X$ the X-ray wavelength),
atom $i$ at mean position $\mathbf{r}_i$ scatters with its form factor $f_i(\mathbf{k})$ times the
**Debye–Waller factor**:

$$F(\mathbf{k}) = \sum_i f_i(\mathbf{k})\, e^{i\mathbf{k}\cdot\mathbf{r}_i}\, e^{-\frac12 \mathbf{k}^{\mathsf T} U_i \mathbf{k}} . \tag{2}$$

This holds when the displacement has a Gaussian distribution, true for harmonic motion.

### 1.2 Correlated motion

In a molecule the atoms do not move independently. Suppose that, at any moment, every
displacement is a linear function of a few **generalised coordinates** $\mathbf{v} = (v_1, \dots, v_m)$:

$$\mathbf{u}_i = B_i \mathbf{v}, \qquad B_i \text{ a } 3\times m \text{ matrix fixed by the geometry.}$$

Putting this in (1) gives

$$U_i = B_i \langle \mathbf{v}\mathbf{v}^{\mathsf T}\rangle B_i^{\mathsf T} = B_i\, \Sigma\, B_i^{\mathsf T}, \qquad \Sigma = \langle \mathbf{v}\mathbf{v}^{\mathsf T}\rangle . \tag{3}$$

So the ADPs of all $N$ atoms ($6N$ numbers) follow from $\Sigma$, the $m\times m$ covariance of the
generalised coordinates ($m(m+1)/2$ numbers). This is the whole idea: refine $\Sigma$ instead of the $U_i$.

### 1.3 Rigid-body motion: $\mathbf{t}$ and $\boldsymbol{\lambda}$

The simplest correlated motion is the molecule moving as a rigid body, by a translation and a
rotation.

- **The translation** $\mathbf{t}$ is a vector: every atom moves by the same $\mathbf{t}$.
- **The rotation** is a rotation by a small angle $\varphi$ (in radians) about an axis through a fixed
  **origin**, with unit vector $\mathbf{n}$ along the axis. An atom at position $\mathbf{r}_i$ from the
  origin moves to $R\,\mathbf{r}_i$, where $R$ is the rotation matrix. For small $\varphi$, to first order,

  $$R\,\mathbf{r}_i = \mathbf{r}_i + \varphi\, \mathbf{n} \times \mathbf{r}_i ,$$

  so the displacement is $\varphi\,\mathbf{n}\times\mathbf{r}_i$. Define the **rotation vector** (or
  libration vector)

  $$\boldsymbol{\lambda} = \varphi\, \mathbf{n} :$$

  its direction is the rotation axis, its length the angle in radians. The displacement from the
  rotation is then $\boldsymbol{\lambda}\times\mathbf{r}_i$, linear in $\boldsymbol{\lambda}$.

Together, the rigid-body displacement of atom $i$ is

$$\mathbf{u}_i = \mathbf{t} + \boldsymbol{\lambda} \times \mathbf{r}_i . \tag{4}$$

The cross product is a matrix acting on $\boldsymbol{\lambda}$: $\boldsymbol{\lambda}\times\mathbf{r}_i = A_i \boldsymbol{\lambda}$ with

$$A_i = \begin{pmatrix} 0 & z_i & -y_i \\ -z_i & 0 & x_i \\ y_i & -x_i & 0 \end{pmatrix}, \qquad \mathbf{r}_i = (x_i, y_i, z_i) \text{ from the origin.}$$

So (4) is of the form of §1.2 with six generalised coordinates $\mathbf{v} = (\mathbf{t}, \boldsymbol{\lambda})$ and
$B_i = [\,1 \mid A_i\,]$, a $3\times 6$ matrix ($1$ is the $3\times 3$ unit matrix).

The first-order step drops terms of order $\varphi^2$. These bend the paths of the atoms into arcs and
make bond lengths from a refinement appear slightly short (the libration correction); that
correction is not applied here.

### 1.4 T, L and S

The covariance $\Sigma = \langle \mathbf{v}\mathbf{v}^{\mathsf T}\rangle$ of $\mathbf{v} = (\mathbf{t}, \boldsymbol{\lambda})$ is $6\times 6$. Its $3\times 3$ blocks are the
conventional rigid-body tensors:

$$\Sigma = \begin{pmatrix} T & S^{\mathsf T} \\ S & L \end{pmatrix}, \qquad
T = \langle \mathbf{t}\mathbf{t}^{\mathsf T}\rangle, \quad
L = \langle \boldsymbol{\lambda}\boldsymbol{\lambda}^{\mathsf T}\rangle, \quad
S = \langle \boldsymbol{\lambda}\mathbf{t}^{\mathsf T}\rangle ,$$

the translation (Å²), the libration (rad², printed in deg²), and the correlation of rotation with
translation (Å rad). Putting $B_i = [\,1 \mid A_i\,]$ into (3) and multiplying out the blocks gives
the **Schomaker–Trueblood** formula

$$U_i^{\rm TLS} = T + A_i L A_i^{\mathsf T} + A_i S + S^{\mathsf T} A_i^{\mathsf T} . \tag{5}$$

- **The origin** is the fragment's centre of mass. $T$ and $S$ depend on where it is; $L$ does not.
- **$\operatorname{tr} S$ is not determined.** Adding the same number $c$ to the three diagonal elements of
  $S$ adds $c\,(A_i + A_i^{\mathsf T})$ to every $U_i$, and that is zero because $A_i$ is antisymmetric. No
  measurement can fix it. Tonto removes that direction, which makes $\operatorname{tr} S = 0$. $\Sigma$ has
  21 independent elements, so 20 remain.
- **Site symmetry.** If the molecule sits on a special position, $\Sigma$ must carry the site symmetry.
  Tonto imposes it through the atoms:
  - each atom that is a symmetry image of another must get the symmetry-transformed U;
  - each atom on a special position must have a U unchanged by its site-symmetry operations.

  These are linear conditions on $\Sigma$, and the allowed $\Sigma$ are their solutions. Urea on its mm2 site
  keeps 8 of the 20.

### 1.5 Internal motion: normal modes

The molecule also vibrates internally. Let $\mathbf{x}$ be the $3N$ Cartesian displacements of all the
atoms from their equilibrium positions. Near a minimum of the energy $E$ the potential is
harmonic,

$$E = E_0 + \tfrac12\, \mathbf{x}^{\mathsf T} H\, \mathbf{x}, \qquad H_{ab} = \frac{\partial^2 E}{\partial x_a\, \partial x_b},$$

where $H$ is the **Hessian**, the $3N\times 3N$ matrix of force constants (hartree/bohr²).

Let $m_i$ be the mass of atom $i$, and $M$ the $3N\times 3N$ diagonal matrix holding each atom's mass
three times. In **mass-weighted coordinates** $\mathbf{q} = M^{1/2}\mathbf{x}$ the kinetic energy is
$\tfrac12 |\dot{\mathbf{q}}|^2$, and the motion separates into independent oscillators, the **normal modes**:

1. **Mass-weight the Hessian:** $H' = M^{-1/2} H M^{-1/2}$.
2. **Remove the rigid-body motion.** For an isolated molecule the three translations and three
   rotations cost no energy; they are not vibrations. Build the six mass-weighted rigid-body
   displacement vectors ($\sqrt{m_i}\,\mathbf{e}$ for a translation along unit vector $\mathbf{e}$,
   $\sqrt{m_i}\,\mathbf{e}\times\mathbf{r}_i$ for a rotation about it), orthonormalise them, and project them out:
   $H'' = P H' P$ with $P = 1 - \sum_{\mathbf{v}} \mathbf{v}\mathbf{v}^{\mathsf T}$ over those six vectors $\mathbf{v}$. This imposes the
   **Eckart–Sayvetz conditions**: the remaining modes carry no net linear or angular momentum,
   so they are purely internal. The six become exact zero modes (five for a linear molecule).
3. **Diagonalise $H''$.** Each eigenvector $\mathbf{l}_k$ (a $3N$-vector) is a mode's shape; its
   eigenvalue is $\omega_k^2$, with $\omega_k$ the mode's angular frequency. $\mathbf{l}_{ik}$ denotes the
   three components of $\mathbf{l}_k$ on atom $i$.

In terms of the mode amplitudes $Q_k$ (the **normal coordinates**), atom $i$ moves by

$$\mathbf{u}_i = \sum_k \mathbf{l}_{ik}\, Q_k / \sqrt{m_i},$$

which is again of the form of §1.2. Each mode is a harmonic oscillator. In quantum mechanics its
mean-square amplitude at temperature $\Theta$ is ($\hbar = 1$; $k_B$ Boltzmann's constant)

$$\langle Q_k^2\rangle = \frac{1}{2\omega_k} \coth\!\left(\frac{\omega_k}{2 k_B \Theta}\right) . \tag{6}$$

At $\Theta = 0$ this is $1/(2\omega_k)$, the zero-point motion; at high $\Theta$ it tends to
$k_B\Theta/\omega_k^2$, the classical value. Different modes are uncorrelated, so (1) gives

$$U_i^{\rm modes} = \sum_k \mathbf{l}_{ik} \mathbf{l}_{ik}^{\mathsf T}\, \langle Q_k^2\rangle / m_i . \tag{7}$$

**Stiff and soft modes.** A stiff mode (high ω, bond stretches and bends) is hardly affected by
the crystal, and (6) from a calculated Hessian is good. A soft mode (low ω, torsions and wags)
is sensitive to the crystal environment and to anharmonicity, and its amplitude should come from
the data. Tonto splits the modes at a cutoff $\omega_c$ (200 cm⁻¹ by default). $U_i^{\rm high}$ is (7)
summed over the stiff modes only:

$$U_i^{\rm high} = \sum_{\omega_k > \omega_c} \mathbf{l}_{ik} \mathbf{l}_{ik}^{\mathsf T}\, \langle Q_k^2\rangle / m_i . \tag{8}$$

A mode with $\omega_k^2 \le 0$ (an imaginary frequency) counts as soft. A Hessian taken at the crystal
geometry, which is not an energy minimum, can have such modes.

### 1.6 The full model

Adding the rigid-body motion (4) and the stiff internal modes, taken as uncorrelated, gives the
ADP of atom *i* as

$$U_i = U_i^{\rm high} + B_i\, \Sigma\, B_i^{\mathsf T} , \tag{9}$$

with $\Sigma$ refined and $U_i^{\rm high}$ fixed by the Hessian. The soft modes are not yet
in the model (§6).

### 1.7 Frequency scaling

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

### 1.8 A Hessian by finite differences

Without an external program, Tonto makes the Hessian from its own SCF energies by central
differences with step $h$ (0.01 bohr by default). Writing $E(+a)$ for the energy with coordinate $a$
moved by $+h$, and so on:

$$H_{aa} = \frac{E(+a) + E(-a) - 2E_0}{h^2}, \qquad
H_{ab} = \frac{E(+a,+b) - E(+a,-b) - E(-a,+b) + E(-a,-b)}{4h^2} .$$

**Translational invariance.** Moving the whole molecule does not change E, so each atom's block
row of H sums to zero over the atoms. Tonto uses this to get the last atom's rows, so N atoms
need $2n + 2n(n-1)$ SCFs with $n = 3(N-1)$. It holds at any geometry, but not with cluster charges
or an applied field; then every coordinate is differenced.

The **gradient** $\partial E/\partial x_a$ comes from the same energies, $[E(+a) - E(-a)]/2h$, and is printed.
Rotational invariance gives a similar sum rule only where the gradient is zero. At a geometry
that is not stationary, such as a crystal geometry, the rotations are not exact zero modes of H,
and the projection in §1.5 step 2 is an approximation.


## 2. Refining against the structure factors

### 2.1 What is minimised

For each measured reflection the refinement compares the observed structure factor amplitude
$F_{\rm obs}$ with the calculated one, $F_{\rm calc}$ from (2), with weight $w = 1/\sigma^2$, $\sigma$ the
measurement's standard uncertainty. With $\Delta F = F_{\rm obs} - F_{\rm calc}$, it minimises

$$\chi^2 = \sum_{\rm reflections} w\, \Delta F^2 .$$

Two numbers summarise the fit. **GoF** (goodness of fit) counts the parameters:

$$\mathrm{GoF} = \left( \frac{\chi^2}{N_{\rm refl} - N_{\rm param}} \right)^{1/2},$$

and is 1 for a model that fits to within the measurement errors.
$R(F) = \sum|\Delta F| \,/\, \sum|F_{\rm obs}|$ does
not count the parameters or use the weights. **Compare models by GoF, not R**: R can hide a
worse fit. Whether a model with more parameters is significantly better is decided by
Hamilton's test.

### 2.2 Parameters and the Jacobian

The refinement's parameters $\mathbf{p}$ are kept explicitly. The model vector $\mathbf{X}$, the positions and
ADPs of all the atoms, is made from them by

$$\mathbf{X} = \mathbf{X}_0 + J\,\mathbf{p}, \qquad J = \partial\mathbf{X}/\partial\mathbf{p} \text{ (the Jacobian).} \tag{10}$$

- **Free ADPs:** J has one unit column per refined component of X.
- **TLS model:** each atom keeps its three position columns. Its six ADP columns are replaced
  by columns shared by all the atoms, one per allowed $\Sigma$ parameter: by (9),
  $\partial U_i/\partial\Sigma = \partial(B_i\Sigma B_i^{\mathsf T})/\partial\Sigma$. $\mathbf{X}_0$ holds $U^{\rm high}$ on the ADP rows.

### 2.3 Solving

From the derivatives $\partial F_{\rm calc}/\partial\mathbf{X}$, the change in $\mathbf{p}$ that best lowers $\chi^2$ to first
order solves the **normal equations**:

$$D = \frac{\partial F_{\rm calc}}{\partial \mathbf{X}}\, J, \qquad A = D^{\mathsf T} W D, \qquad \mathbf{b} = D^{\mathsf T} W\, \Delta\mathbf{F}, \qquad A\, \Delta\mathbf{p} = \mathbf{b},$$

with $W$ the diagonal matrix of the weights. Eigenvalues of $A$ near zero (directions the data do
not determine) are filtered out. The covariance of $\mathbf{p}$ is $C = A^{-1}$ scaled by GoF², and the esd of
each parameter is the square root of its diagonal element. The covariance of $\mathbf{X}$ follows as
$J C J^{\mathsf T}$, which gives every atomic $U$ an esd even though only $\Sigma$ is refined. Each
refinement cycle starts by putting the ADPs on the model: $\Sigma$ is fitted to the current
$U - U^{\rm high}$, and $U$ is set to $U^{\rm high} + B\,\Sigma\,B^{\mathsf T}$.


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
| `frequency_scale_factor=` | see §1.7 | scales the frequencies in (6) |
| `make_internal_adps` | | makes U^high, eq. (8) |
| `put_internal_adps` | | prints U^high and how many modes are soft, imaginary and stiff |

In `xray_data=`:

| keyword | default | meaning |
|---|---|---|
| `adp_model=` | `free` | `free`: refine each atom's ADP; `tls`: the model (9), HAR only |

A TLS Hirshfeld atom refinement with U^high from a Hessian:

```
   CIF= { file_name= urea.cif }
   process_CIF

   force_constants= { ... }          ! or: scf, then make_fd_hessian
   normal_mode_analysis
   internal_adp_temperature= 123
   frequency_scale_factor= 0.8953    ! RHF/6-31G(d) Hessian
   make_internal_adps
   put_internal_adps

   crystal= {
      xray_data= {
         partition_model= oc-hirshfeld
         adp_model= tls
         ...
      }
   }
   scfdata= { ... }
   scf
   HAR_refinement
```

The test `tests/long/urea_rhf_STO-3G_HAR_TLS` is a complete example.


## 4. Where it is implemented

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
| allowed Σ, J columns, tr S, origin | `CRYSTAL:make_TLS_jacobian_columns` |
| p and X₀; ADPs put on the model | `CRYSTAL:set_refinement_parameters` |
| T, L, S with esds | `CRYSTAL:put_TLS_results` |
| explicit parameters, eq. (10); the solve | `LEAST_SQUARES` (`least_squares.foo`) |


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
wags are imaginary (432i and 208i cm⁻¹) and are left out of U^high as soft modes. The gradient
there is 0.093 Eh/bohr. RHF frequencies are about 10 % high, so these U^high are about 10 % low.

### Urea: TLS refinement

Hirshfeld atom refinement, def2-SVP, 123 K data, U^high as above:

| ADP model | parameters | GoF | R(F) | N–H1 /Å | N–H3 /Å | U_iso H1 / H3 /Å² |
|---|---|---|---|---|---|---|
| free | 27 | 3.30 | 0.0181 | 1.028(5) | 0.986(6) | 0.054(4) / 0.048(3) |
| TLS, U^high = 0 | 17 | 3.84 | 0.0184 | 1.025(5) | 0.994(5) | 0.0334(15) / 0.0329(13) |
| TLS + U^high | 17 | 3.51 | 0.0180 | 1.028(5) | 0.994(5) | 0.0447(15) / 0.0399(13) |
| neutron | | | | 1.006 | 1.000 | |

- **The stiff modes matter:** adding U^high lowers GoF from 3.84 to 3.51.
- **TLS + U^high still fits worse than free ADPs:** GoF 3.51 against 3.30. Whether that
  difference is significant is for a Hamilton test; it is not yet printed.
- **The hydrogen ADPs are short of the free ones** by about 0.01 Å². That is where the two NH₂
  wags belong, the soft modes this model does not yet refine.
- **The ADP model hardly moves the hydrogen positions:** N–H3 shifts 0.008 Å toward neutron,
  N–H1 not at all.

### Urea: ADPs against neutron

The neutron ADPs of urea at 123 K (Swaminathan, Craven and McMullan, 1984), scaled to the X-ray
data, are tabulated by Jayatilaka and Dittrich (2008, Table 6). The cell is tetragonal, so its
axes and Tonto's Cartesian axes coincide and the tensors compare component by component. Two
measures per atom:
- the ratio of U_iso to the neutron U_iso, for the size;
- the similarity index of Whitten and Spackman (2006),

  $$S_{12} = 100 \left[ 1 - \frac{2^{3/2}\, \det(U_1^{-1} U_2^{-1})^{1/4}}{\det(U_1^{-1} + U_2^{-1})^{1/2}} \right],$$

  which is 0 for identical displacement ellipsoids and grows as their shapes and orientations
  part.

| ADP model | GoF | H1: U_iso ratio | H1: S12 | H3: U_iso ratio | H3: S12 |
|---|---|---|---|---|---|
| free | 3.30 | 1.47 | 8.05 | 1.44 | 7.06 |
| TLS, U^high = 0 | 3.84 | 0.91 | 1.85 | 0.99 | 2.86 |
| TLS + U^high | 3.51 | 1.22 | 0.77 | 1.20 | 0.50 |

O, N and C agree with neutron in every model: U_iso ratios 0.99–1.01, S12 at most 0.03.

- **Free HAR hydrogens are about 45 % too large and the wrong shape**, as found before for HAR
  of urea with an isolated-molecule density.
- **TLS + U^high gives the best hydrogen shapes by far**, S12 ten times smaller than free, but
  sizes about 20 % too large.
- **TLS alone** gets the hydrogen size about right, but the shape is worse.

That TLS + U^high hydrogens are too large is not yet understood. The missing soft modes would
make them larger still, and unscaled RHF frequencies make U^high smaller, not larger. Possible
causes are the scaling of the neutron data to the X-ray data, and correlation between the
rigid-body and the internal motion, which the model takes as independent.

### Urea: the rigid-body tensors

The rigid-body motion, about the centre of mass (8 parameters on the mm2 site):

    T /Å²:      0.01402(7)   -0.00045(9)   0                L /deg²:   19.8(17)   12.2(17)   0
               -0.00045(9)    0.01402(7)   0                           12.2(17)   19.8(17)   0
                0             0            0.00591(4)                   0          0         44(4)

    S /(Å deg): -0.18(2)  0.12(2)  0;   -0.12(2)  0.18(2)  0;   0  0  0      (rows λ, columns t)

L has eigenvalues 7.6, 32 and 44 deg², the largest about the C=O axis; the rms libration is 5.3°.
S has zero trace.


## 6. What is not done yet

- **Soft modes.** Refining the amplitudes of the softest internal modes (including any
  imaginary ones) one at a time, held toward their harmonic values, with AIC, BIC and the
  Hamilton test printed at each step to say how many the data support.
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
