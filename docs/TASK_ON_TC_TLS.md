# Two-centre thermal smearing from rigid-body motion and normal modes (`tc-tls`) — theory

A working document. It develops the theory of a new model, `tc-tls`, before any code is written.
Notation follows `docs/REPORT_ON_MODE_FITTING.md` (called *the report* below), whose equations
(1)–(11) are used without repeating them. Sources: Dylan's notes `~/Dropbox/tex/idea/atom_pair_pdf.tex`
(the atom-pair distribution) and `adp5.tex` (ADPs from normal modes and the coth factor). The
user-facing form of §2–§5 is §6 of the report.

Units are atomic units throughout. Vectors are columns, ᵀ is the transpose, ⟨ ⟩ is the thermal
average.


## 1. The idea in one paragraph

Hirshfeld atom refinement cuts the molecular density into atoms and smears each atom with its
own ADP. The cut is an approximation: the density is a sum over *pairs* of basis functions, and
a product of functions on two different atoms sits between them and moves with both. To smear
it one needs to know how the two atoms move *together*, which a set of independent ADPs does not
say. The TLS and soft-mode model of the report does say it: it gives the whole covariance of
the atomic displacements, not just each atom's own block. So with that model no partition is
needed. Each product of two Gaussians is smeared by the mean-square displacement of its own
centre, which follows from the same refined parameters. The stiff modes come from the Hessian
with the coth factor, exactly as now; the rigid-body motion and the soft modes are refined,
exactly as now. No parameter is added.


## 2. The covariance between two atoms

The report's model is that the displacement of atom $`A`$ is linear in a set of generalised
coordinates. Collect *all* of them in one vector: the translation $`\mathbf{t}`$, the rotation
vector $`\boldsymbol{\lambda}`$, the refined soft-mode amplitudes $`Q_1 \dots Q_K`$, and the stiff-mode
amplitudes $`Q_k`$ with $`\omega_k > \omega_c`$:

```math
\mathbf{u}_A = \bar B_A\, \bar{\mathbf{v}}, \qquad
\bar{\mathbf{v}} = (\mathbf{t}, \boldsymbol{\lambda}, Q_1, \dots, Q_K, Q_{\rm stiff}), \qquad
\bar B_A = [\,1 \mid A_A \mid \mathbf{d}_{A1} \cdots \mathbf{d}_{AK} \mid \mathbf{d}_{A,\rm stiff}\,] . \qquad (1)
```

Here $`A_A`$ is the cross-product matrix of the report's (4), built from the position of atom $`A`$
relative to the origin, and $`\mathbf{d}_{Ak} = \mathbf{l}_{Ak}/\sqrt{m_A}`$ is the displacement of atom $`A`$ per
unit amplitude of mode $`k`$. The covariance of $`\bar{\mathbf{v}}`$ is

```math
\bar\Sigma = \langle \bar{\mathbf{v}} \bar{\mathbf{v}}^{\mathsf T} \rangle =
\begin{pmatrix} \Sigma & 0 \\ 0 & \mathrm{diag}(a_k^0) \end{pmatrix}, \qquad
a_k^0 = \frac{1}{2\omega_k} \coth\!\left(\frac{\omega_k}{2 k_B \Theta}\right) , \qquad (2)
```

with $`\Sigma`$ the refined $`(6+K)\times(6+K)`$ matrix of the report's (11), and the stiff block fixed
by the Hessian at the temperature $`\Theta`$ of the data (the report's (6)).

The **two-centre ADP** of atoms $`A`$ and $`B`$ is the $`3\times 3`$ matrix

```math
U_{AB} = \langle \mathbf{u}_A \mathbf{u}_B^{\mathsf T} \rangle = \bar B_A\, \bar\Sigma\, \bar B_B^{\mathsf T}
       = B_A\, \Sigma\, B_B^{\mathsf T} + \sum_{\omega_k > \omega_c} a_k^0\, \mathbf{d}_{Ak} \mathbf{d}_{Bk}^{\mathsf T} . \qquad (3)
```

$`U_{AA}`$ is the ordinary ADP $`U_A`$, the report's (11). $`U_{AB}`$ is not symmetric, and
$`U_{BA} = U_{AB}^{\mathsf T}`$. The second term of (3) is the two-centre version of $`U^{\rm high}`$; call it
$`U^{\rm high}_{AB}`$. Everything in (3) is already available: $`\Sigma`$ is what `adp_model= tls`
refines, and the $`\mathbf{d}_{Ak}`$ and $`a_k^0`$ are what `make_internal_adps` uses for the diagonal blocks.

**On the coth factor in the urea work.** It was used. Every urea job set
`internal_adp_temperature= 123`, the temperature of the X-ray and neutron data, and both
$`U^{\rm high}`$ and the harmonic targets of the soft-mode restraints were made from (2) at that
temperature. The keyword's default is 0, which gives zero-point motion only.


## 3. A product of two Gaussians under displacement

The density is

```math
\rho(\mathbf{r}) = \sum_{ab} D_{ab}\, \chi_a(\mathbf{r} - \mathbf{R}_A)\, \chi_b(\mathbf{r} - \mathbf{R}_B) , \qquad (4)
```

where $`\chi_a`$ is a basis function on atom $`A`$ at $`\mathbf{R}_A`$, $`\chi_b`$ one on atom $`B`$ at $`\mathbf{R}_B`$, and
$`D_{ab}`$ the density matrix. Take one primitive from each, with exponents $`\alpha`$ and $`\beta`$. Define

```math
\gamma = \alpha + \beta, \qquad w_a = \alpha/\gamma, \qquad w_b = \beta/\gamma, \qquad
\mu = \alpha\beta/\gamma, \qquad \mathbf{R} = \mathbf{R}_A - \mathbf{R}_B .
```

The Gaussian product theorem says the product of the two radial parts is one Gaussian of
exponent $`\gamma`$ at the point $`\mathbf{P} = w_a \mathbf{R}_A + w_b \mathbf{R}_B`$ on the line between the atoms, times a
constant $`e^{-\mu |\mathbf{R}|^2}`$. The angular parts are polynomials in $`\mathbf{r} - \mathbf{R}_A = (\mathbf{r} - \mathbf{P}) - w_b \mathbf{R}`$
and $`\mathbf{r} - \mathbf{R}_B = (\mathbf{r} - \mathbf{P}) + w_a \mathbf{R}`$.

Now displace the atoms by $`\mathbf{u}_A`$ and $`\mathbf{u}_B`$. Two combinations matter:

```math
\mathbf{u}_P = w_a \mathbf{u}_A + w_b \mathbf{u}_B , \qquad \mathbf{d} = \mathbf{u}_A - \mathbf{u}_B . \qquad (5)
```

$`\mathbf{u}_P`$ is the displacement of the product's centre; $`\mathbf{d}`$ is the relative displacement of
the two atoms. The displaced product is, exactly,

```math
\chi_a \chi_b\,(\mathbf{r}) = \chi_{ab}(\mathbf{r} - \mathbf{P} - \mathbf{u}_P \,;\, \mathbf{R} + \mathbf{d}) , \qquad (6)
```

where $`\chi_{ab}(\mathbf{s}; \mathbf{R})`$ is the product written about its own centre. So **the product moves
bodily with $`\mathbf{u}_P`$, and changes size and shape only through $`\mathbf{d}`$.** Its Fourier transform at
scattering vector $`\mathbf{k}`$ is

```math
\int \chi_a \chi_b\, e^{i\mathbf{k}\cdot\mathbf{r}}\, d\mathbf{r}
   = e^{i\mathbf{k}\cdot\mathbf{P}}\; e^{i\mathbf{k}\cdot\mathbf{u}_P}\; \tilde\chi_{ab}(\mathbf{k}; \mathbf{R} + \mathbf{d}) , \qquad (7)
```

with $`\tilde\chi_{ab}`$ the transform of $`\chi_{ab}`$. The covariances of the two combinations follow
from (3):

```math
U^P_{ab} = \langle \mathbf{u}_P \mathbf{u}_P^{\mathsf T} \rangle = w_a^2\, U_A + w_b^2\, U_B + w_a w_b\,(U_{AB} + U_{AB}^{\mathsf T}) , \qquad (8)
```

```math
V_{AB} = \langle \mathbf{d}\, \mathbf{d}^{\mathsf T} \rangle = U_A + U_B - U_{AB} - U_{AB}^{\mathsf T} , \qquad (9)
```

```math
C_{ab} = \langle \mathbf{d}\, \mathbf{u}_P^{\mathsf T} \rangle = w_a U_A - w_b U_B + w_b U_{AB} - w_a U_{AB}^{\mathsf T} . \qquad (10)
```


## 4. The model: each product is smeared by the motion of its centre

**Approximation.** In the thermal average of (7), keep the dependence on $`\mathbf{u}_P`$ and drop the
dependence on $`\mathbf{d}`$: the product keeps the size and shape it has at the mean geometry, and
the density matrix is the one calculated there. This is the rigid-atom approximation of Hirshfeld
atom refinement, applied to each product instead of each atom. What it leaves out is in §7.

Since $`\mathbf{u}_P`$ is Gaussian with covariance (8), $`\langle e^{i\mathbf{k}\cdot\mathbf{u}_P}\rangle = e^{-\frac12 \mathbf{k}^{\mathsf T} U^P_{ab} \mathbf{k}}`$,
and the structure factor is

```math
F(\mathbf{k}) = \sum_{ab} D_{ab} \sum_{\alpha\beta} c_\alpha c_\beta\;
     e^{i\mathbf{k}\cdot\mathbf{P}}\; \tilde\chi_{ab}(\mathbf{k}; \mathbf{R})\; e^{-\frac12 \mathbf{k}^{\mathsf T} U^P_{ab} \mathbf{k}} , \qquad (11)
```

the inner sum over the primitives of the two contracted functions, with contraction coefficients
$`c_\alpha`$ and $`c_\beta`$. The sum over $`ab`$ runs over every pair, one-centre and two-centre, with $`a`$ in
the unit cell. There is no partition function in (11).

**The centre as a pseudo-atom.** Put (1) into (5). Because $`w_a + w_b = 1`$,

```math
\mathbf{u}_P = \bar B_P\, \bar{\mathbf{v}}, \qquad \bar B_P = w_a \bar B_A + w_b \bar B_B , \qquad
U^P_{ab} = \bar B_P\, \bar\Sigma\, \bar B_P^{\mathsf T} . \qquad (12)
```

Column by column:

- the translation column of $`\bar B_P`$ is the unit matrix;
- the rotation column is $`w_a A_A + w_b A_B`$, and since $`A`$ is linear in the position, this is the
  cross-product matrix of the point $`\mathbf{P}`$ itself;
- each mode column is $`\mathbf{d}_{Pk} = w_a \mathbf{d}_{Ak} + w_b \mathbf{d}_{Bk}`$, the mode's displacement
  interpolated to $`\mathbf{P}`$.

So (12) is the report's (11) for a fictitious atom at $`\mathbf{P}`$:

```math
U^P_{ab} = U^{{\rm high},P}_{ab} + B_P\, \Sigma\, B_P^{\mathsf T}, \qquad
U^{{\rm high},P}_{ab} = \sum_{\omega_k > \omega_c} a_k^0\, \mathbf{d}_{Pk} \mathbf{d}_{Pk}^{\mathsf T} . \qquad (13)
```

The rigid-body part of $`U^P_{ab}`$ is the Schomaker–Trueblood ADP of the point $`\mathbf{P}`$: the overlap
density rides on the rigid body where it sits. The stiff-mode part is the user's "two-centre ADPs
from the normal modes, used as is". The soft-mode part is "their two-centre amplitudes smear the
overlap density". All three come from one formula.

**Consequences.**

- *No new parameters.* The refined parameters are $`\Sigma`$ and the positions, as in `adp_model= tls`.
- *The derivatives have the same form as now.* $`\partial F/\partial \Sigma_{pq}`$ is (11) with each term multiplied
  by $`-\tfrac12 (\bar B_P^{\mathsf T}\mathbf{k})_p (\bar B_P^{\mathsf T}\mathbf{k})_q`$, the same expression the atom-based TLS
  model uses with $`B_A`$.
- *One-centre products are unchanged.* For $`A = B`$, $`\mathbf{P} = \mathbf{R}_A`$ and $`U^P_{ab} = U_A`$.
- *The temperature factor depends on the exponents*, through $`w_a`$ and $`w_b`$, so it is made per
  pair of primitives, as `SHELL2:make_Tanaka_TFs` already does.


## 5. Relation to the two-centre models already in Tonto

Substituting (9) into (8) gives an identity:

```math
U^P_{ab} = \underbrace{w_a U_A + w_b U_B}_{\text{Tanaka}} \; - \; w_a w_b\, V_{AB} . \qquad (14)
```

- **`tc-tanaka`** uses the first term alone. It is exact when the two atoms have no relative
  motion ($`V_{AB} = 0`$), and otherwise smears the overlap density too much, by $`w_a w_b V_{AB}`$.
  A bond stretch moves the two atoms in opposite directions and leaves the overlap density
  nearly still; (14) takes that out, Tanaka does not.
- **`tc-stewart`** puts every product at the midpoint, $`w_a = w_b = \tfrac12`$. Its "bonded" case,
  $`\tfrac12(U_A + U_B)`$, is (8) with $`U_{AB}`$ such that $`V_{AB} = 0`$. Its "non-bonded" case,
  $`\tfrac14(U_A + U_B)`$, is (8) with $`U_{AB} = 0`$, independent motion. `tc-tls` has these two as
  limits and gets the correlation from the model instead of a distance test.
- **`tc-coppens`** averages the two atoms' temperature factors and corresponds to no $`U_{AB}`$.

None of the three can be used for refinement of ADPs in a consistent way, because $`U_{AB}`$ is
not determined by $`U_A`$ and $`U_B`$. In `tc-tls` it is, by (3).


## 6. Positions, symmetry, and pairs between molecules

**Position derivatives.** In (11) the position of atom $`A`$ enters through the phase $`e^{i\mathbf{k}\cdot\mathbf{P}}`$,
through $`\tilde\chi_{ab}(\mathbf{k}; \mathbf{R})`$, and through $`D`$. The first gives

```math
\frac{\partial F}{\partial \mathbf{R}_A} \approx i\mathbf{k} \sum_{ab,\; a \in A} w_a\, (\text{term } ab \text{ of (11)}) \;+\; (\text{the same with } w_b \text{ for } b \in A). \qquad (15)
```

This is the rigid-atom derivative $`i\mathbf{k} F_A`$ of Hirshfeld atom refinement with an effective
atomic structure factor: each product is shared between its two atoms in the ratio of the
exponents. The derivative of $`\tilde\chi_{ab}`$ with respect to $`\mathbf{R}`$ is analytic and can be added. The
derivative of $`D`$ is left to the repeated SCF, as in Hirshfeld atom refinement.

**Symmetry.** A pair related to a pair in the asymmetric unit by a symmetry operation with rotation
matrix $`S`$ has $`U^P \to S\, U^P S^{\mathsf T}`$, as for an atom. The site-symmetry conditions on $`\Sigma`$ are those
of the report's §1.4, unchanged.

**Pairs between molecules.** (3) gives $`U_{AB}`$ only when $`A`$ and $`B`$ belong to the same rigid body.
For a product of functions on two different molecules the model has no correlation. The
consistent choice is $`U_{AB} = 0`$, so $`U^P_{ab} = w_a^2 U_A + w_b^2 U_B`$. These products are small. A
lattice-dynamical model would supply the true value; that is outside this task.


## 7. What the approximation leaves out

There is one identity behind every correction. Let $`\mathbf{x}`$ be the $`3N`$ displacements of all atoms,
Gaussian with covariance $`U`$ (the full matrix with blocks $`U_{AB}`$), and let $`\mathbf{c}`$ be the
$`3N`$-vector with $`w_a\mathbf{k}`$ on atom $`A`$, $`w_b\mathbf{k}`$ on atom $`B`$ and zero elsewhere, so that
$`\mathbf{k}\cdot\mathbf{u}_P = \mathbf{c}\cdot\mathbf{x}`$. Then for any function $`f`$ of the displacements,

```math
\langle f(\mathbf{x})\, e^{i\mathbf{c}\cdot\mathbf{x}} \rangle = e^{-\frac12 \mathbf{c}^{\mathsf T} U \mathbf{c}}\; \langle f(\mathbf{x} + i\,U\mathbf{c}) \rangle . \qquad (16)
```

The first factor is the temperature factor of (11), since $`\mathbf{c}^{\mathsf T} U \mathbf{c} = \mathbf{k}^{\mathsf T} U^P_{ab} \mathbf{k}`$. The
second is the average of $`f`$ with every displacement shifted by an imaginary amount. With
$`f = D_{ab}(\mathbf{x})\, \tilde\chi_{ab}(\mathbf{k}; \mathbf{R} + \mathbf{d})`$ this is the exact thermal average of one term. The model
of §4 replaces the second factor by $`f(0)`$. Expanding it to second order,

```math
\langle f(\mathbf{x} + i\,U\mathbf{c}) \rangle = f(0) + i\,(\nabla f)^{\mathsf T} U \mathbf{c}
   + \tfrac12\, \mathrm{tr}\,\!\big[ \nabla\nabla f\, ( U - U\mathbf{c}\mathbf{c}^{\mathsf T} U ) \big] + \dots , \qquad (17)
```

with $`\nabla f`$ and $`\nabla\nabla f`$ the first and second derivatives of $`f`$ at the mean geometry. The first-order
term is what `atom_pair_pdf.tex` calls a p-type distribution. For the part of $`f`$ that depends
only on $`\mathbf{d}`$, $`(\nabla f)^{\mathsf T} U \mathbf{c} = (\partial f/\partial \mathbf{d})^{\mathsf T} C_{ab}\, \mathbf{k}`$, with $`C_{ab}`$ of (10).

**The exact result for two s functions and a constant density matrix.** Here
$`f = e^{-\mu |\mathbf{R} + \mathbf{d}|^2}`$ times constants, and (16) is a Gaussian integral:

```math
\left\langle e^{-\mu|\mathbf{R}+\mathbf{d}|^2}\, e^{i\mathbf{k}\cdot\mathbf{u}_P} \right\rangle
  = e^{-\frac12 \mathbf{k}^{\mathsf T} U^P_{ab} \mathbf{k}}\;
    \frac{ \exp\!\big[ -\mu\, \mathbf{R}_c^{\mathsf T} (1 + 2\mu V_{AB})^{-1} \mathbf{R}_c \big] }{ \det(1 + 2\mu V_{AB})^{1/2} } , \qquad
  \mathbf{R}_c = \mathbf{R} + i\, C_{ab}\, \mathbf{k} . \qquad (18)
```

Both (11)'s temperature factor and (18) were checked against a direct average over four million
random displacements; they agree to the sampling error. Reading (18):

- At $`\mathbf{k} = 0`$ it is the thermally averaged overlap, not the overlap at the mean geometry. With a
  constant $`D`$ the number of electrons $`\sum_{ab} D_{ab} \langle S_{ab} \rangle`$ is then not conserved. The
  true $`D`$ changes with geometry so that it is. So **(18) must not be used without the derivative
  of $`D`$**; the two changes are of the same order and partly cancel.
- The term linear in $`\mathbf{k}`$ shifts the product's centre: geometries where the atoms are closer
  overlap more and weigh more.
- The term quadratic in $`\mathbf{k}`$ reduces the smearing by $`2\mu\, C_{ab}^{\mathsf T} (1+2\mu V_{AB})^{-1} C_{ab}`$.

**The opposite limit: constant pair populations.** If instead $`D_{ab}`$ changes so that
$`D_{ab} S_{ab}`$ stays fixed, then for s functions $`f`$ is constant and (11) is exact. The model of §4
lies between this limit and (18). The truth needs $`\partial D/\partial \mathbf{x}`$, one coupled-perturbed calculation
or a finite difference along each mode; the finite-difference Hessian already visits those
geometries. A cheaper estimate (Dylan, 2026-10-12) is the uncoupled sum-over-states formula of
`docs/REPORT_ON_XCW_CONVERGENCE.md`, equation (4). For a moved atom the perturbation has two parts,
because the basis moves: with $`F^x`$ and $`S^x`$ the derivatives of the Fock and overlap matrices in
the molecular orbitals, occupied orbital $`i`$ gains
$`-(F^x_{ai} - \varepsilon_i S^x_{ai})/(\varepsilon_a - \varepsilon_i)`$ of each virtual orbital $`a`$, and loses
$`\tfrac12 S^x_{ji}`$ of each occupied orbital $`j`$ to stay orthonormal. Leaving out the change of
the two-electron part of $`F^x`$ with the density is what makes it uncoupled.

**Size.** For a valence pair, $`\mu \approx 0.25`$ bohr⁻² and $`|\mathbf{R}| \approx 2.5`$ bohr. A stretch at 123 K
has $`V_{AB} \approx 0.002`$ Å² along the bond between heavy atoms and 0.006 Å² for a bond to hydrogen,
with bends several times larger across it. The $`\mathbf{k}`$-independent factor of (18) then differs from
one by about 0.5 % for a heavy-atom pair and about 1 % for a bond to hydrogen. These are
estimates to be measured, not results.

**Rotation of the product.** Libration turns each product about its own centre as well as moving
the centre. The turning is first order in $`\boldsymbol{\lambda}`$ and correlated with $`\mathbf{u}_P`$, so by (17) it gives a
first-order term. Hirshfeld atom refinement leaves out the same term for the aspherical part of
each atom. It is left out here too.


## 8. What to decide before coding

1. *Decided (Dylan, 2026-10-12): yes, fixed D to begin with.* **Is §4 the model?** That is: product centres as pseudo-atoms, equations (11)–(13), shape and
   density matrix fixed at the mean geometry. The alternatives are to add the $`\mathbf{R}`$-dependence of
   $`\tilde\chi_{ab}`$ through (17), which without $`\partial D/\partial\mathbf{x}`$ breaks the electron count, or to do both.
2. **Pairs between molecules:** $`U_{AB} = 0`$, as in §6. *Decided (Dylan, 2026-10-12): yes.*
3. **Position derivatives.** *Decided (Dylan, 2026-10-12): the phase term (15) alone; the
   derivative of the product $`\chi_{ab}`$ is ignored.*
4. **The name.** *Decided (Dylan, 2026-10-12):* `partition_model= tc-tls` implies `adp_model= tls`,
   for now.

**First test of the theory, needing little code:** with $`\Sigma`$ and the positions held at a converged
`adp_model= tls` Hirshfeld refinement of urea, compute $`F`$ from (11) and compare the GoF with the
Hirshfeld one, and with `tc-tanaka` using the same ADPs. By (14) the difference between the last
two is the relative motion alone.


## References

- J. R. Michael and T. Koritsanszky, *J. Math. Chem.* 53, 250 (2015), "Validation of convolution
  approximation to the thermal-average electron density"
  (`~/Dropbox/manuscripts/Michael_2015_J_Math_Chem_53_p250-9.pdf`). Found 2026-10-12 after the
  theory here was written (Dylan: "Michael has derived some of these equations"). Their eq. (10)
  is our (8): the exact ADP of a product of two primitive GTOs, with the off-diagonal blocks
  $`U_{AB}`$; implemented in their program Denprop. Their $`U`$ is from the internal normal modes of
  an isolated molecule (coth amplitudes, their eq. (5)), not refined; they smear the density, not
  structure factors, and do not refine. Formamide (B3LYP/6-311G**) and S8 (MP2/cc-pVTZ) at 23 K:
  dropping $`U_{AB}`$ changes the smeared density by at most 0.015 e/A^3 (at N); the smeared density
  agrees closely at the bond critical points with the mean of 5e5 static densities at sampled
  geometries -- the average our stiff-mode correction estimates with two points per mode. They
  say earlier work (Tanaka; Stewart; Coppens; Jayatilaka and Grimwood) approximated the product's
  ADP "without providing the exact solution". What is new here is taking $`U_{AB}`$ from a TLS plus
  soft-mode model refined against the data, the derivatives, and the refinement.
- D. Jayatilaka and D. J. Grimwood, *Acta Cryst.* A57, 76 (2001), §4.2: the two-centre temperature
  factors in Tonto.
- R. F. Stewart, *J. Chem. Phys.* 51, 4569 (1969); P. Coppens, T. V. Willoughby and L. N. Csonka,
  *Acta Cryst.* A27, 248 (1971); K. Tanaka, *Acta Cryst.* A44, 1002 (1988): the earlier two-centre models.
- V. Schomaker and K. N. Trueblood, *Acta Cryst.* B24, 63 (1968): T, L and S.


## 9. Plan for the code

### What exists (read 2026-10-12, nothing built)

- **The two-centre path makes F only.** `MOLECULE.SCF:make_X_SFs_RF` → `MOLECULE.XTAL:make_FT` loops
  over the shell pairs of the fragment's atoms and calls `make_FT_pair`, which gets a partition
  factor and a temperature factor per pair of primitives (`make_partition_factors`,
  `SHELL2:make_temperature_factors`) and hands both to `SHELL2:make_FT`. Symmetry is applied by
  rotating the k points, one block per unique symmetry operation, and summing with
  `SPACEGROUP:sum_unique_sf`. Atoms shared between images are divided by their repetition factor.
  It is tested for F and for the X-ray constrained SCF; no test refines a structure with it.
- **The refinement sees per-atom derivatives only.** `CRYSTAL:make_unique_sf_derivs` makes
  $`\partial F/\partial X`$ for the nine position and ADP numbers of each atom, and
  `DIFFRACTION_DATA.SET:solve_normal_equations` multiplies by the Jacobian `.xray_data.ls.jacobian`
  to reach the TLS parameters. Nothing makes $`U_{AB}`$ for two different atoms.
- **$`\Sigma`$ is not stored.** `put_TLS_results` and `set_TLS_restraints` each rebuild it as
  `TLS_basis` times the parameters.
- **The stiff modes reach the crystal only as the summed `fragment_U_high`.** The mode vectors stay
  on the molecule (`normal_mode_eigenvectors`).
- **Three `select case` blocks have no default** and would do nothing, silently, for a new model:
  `make_X_SFs` (`molecule.scf.foo`), `make_partition_factors` (`molecule.xtal.foo`),
  `SHELL2:make_temperature_factors`. `HAR_refinement` stops unless the model is one-centre.
- `MOLECULE.XTAL:make_FT_deriv_U` is an old two-centre ADP derivative with no caller, hardwired to
  Stewart and inconsistent between its two versions. It is not used here; delete it in step 2.

### The one formula the derivatives need

Because the temperature factor of (11) is $`\exp(-\tfrac12 \mathbf{k}^{\mathsf T} U^P_{ab}\mathbf{k})`$ with $`U^P_{ab}`$ of (8),
every derivative with respect to an element of $`U_A`$, $`U_B`$ or $`U_{AB}`$ is $`k_i k_j`$ times a
scalar. For the atom pair $`(A,B)`$ define three sums over the terms of (11) whose functions are on
those two atoms, each term weighted per pair of primitives:

```math
S^{AA}_{AB}(\mathbf{k}) = \sum w_a^2\,(\text{term}), \qquad
S^{BB}_{AB}(\mathbf{k}) = \sum w_b^2\,(\text{term}), \qquad
S^{AB}_{AB}(\mathbf{k}) = \sum w_a w_b\,(\text{term}) . \qquad (19)
```

With $`\mathbf{a} = B_A^{\mathsf T}\mathbf{k}`$ and $`\mathbf{b} = B_B^{\mathsf T}\mathbf{k}`$ (vectors over the generalised coordinates), the
derivative with respect to $`\Sigma_{pq}`$ is

```math
\frac{\partial F}{\partial \Sigma_{pq}} = -\tfrac12 \sum_{A \le B}
   \big[ S^{AA}_{AB}\, a_p a_q + S^{BB}_{AB}\, b_p b_q + S^{AB}_{AB}\,(a_p b_q + b_p a_q) \big] , \qquad (20)
```

and the position derivative (15) needs one more sum per atom, with weight $`w_a`$. A weight per pair
of primitives is exactly what the `partition` argument of `SHELL2:make_FT` already carries, so no
new integral code is needed: the same routine is called with different weights.

### Steps, each with its check

**Step 1. F from (11), no refinement.**

1. `MOLECULE.PROP`: make the two-centre stiff-mode ADPs $`U^{\rm high}_{AB}`$ of (3), a
   `MAT4{REAL}(3,3,n,n)`, in the routine that makes `stiff_mode_ADPs` now (one loop, the diagonal is
   the existing array). Hand it to the crystal with `set_crystal_U_high`.
2. `CRYSTAL`: one routine that returns $`\Sigma`$ (factored out of `put_TLS_results` and
   `set_TLS_restraints`), one that returns the $`3\times(6+K)`$ matrix $`B_A`$ (factored out of
   `TLS_response`), and `two_center_ADP(a,b)` giving (3); zero if either atom is outside the TLS
   body.
3. `SHELL2:make_TLS_TFs(TF,k_pts,Ua,Ub,Uab)`: (8) per pair of primitives, laid out as
   `make_Tanaka_TFs`. The partition factor is Tanaka's, which shares each product between its two
   atoms in the ratio of the exponents, as (15) does.
4. `tc-tls` added to the options, to the three `select case` blocks, and to the keyword's
   documentation; reading it sets `adp_model= tls`. The blocks get no `case default`: a blank
   `partition_model` reaches them today and does nothing, and stopping there could break a job
   that works.

Checks: (a) with $`U_{AB}`$ set so that $`V_{AB} = 0`$, F equals `tc-tanaka`'s; (b) a one-atom
"molecule" gives the one-centre F; (c) **the urea comparison**: at a converged `adp_model= tls`
Hirshfeld refinement, F by `oc-hirshfeld`, `tc-tanaka` and `tc-tls` with the same parameters; GoF
first.

**Step 2. Derivatives and refinement.**

1. `MOLECULE.XTAL:make_FT_and_derivs`: the shell-pair loop of `make_FT`, also accumulating the
   sums (19) per atom pair and the position sum per atom.
2. `CRYSTAL`: from those sums, $`\partial F/\partial X`$ for the positions (the ADP rows zero) and (20) contracted
   with `TLS_basis` for the TLS parameters; `solve_normal_equations` takes the second as columns
   added after the Jacobian product.
3. `MOLECULE.HAR`: a fit loop for two-centre models. Each cycle recomputes F and the derivatives
   through the shell pairs, because there are no static atomic form factors to shift. The stop in
   `HAR_refinement` allows `tc-tls`.

Checks: (a) every derivative column against a finite difference of F, on urea STO-3G; this is
where the sharing between symmetry-equivalent atoms of the fragment and the repetition factors
will show if wrong; (b) the refinement converges and its GoF, bond lengths and hydrogen ADPs go in
the report beside the Hirshfeld ones; (c) new test `long/urea_rhf_STO-3G_TC_TLS`.

**Not in the first version:** soft-mode correlations beyond what $`\Sigma`$ already holds need
nothing extra; unrestricted and complex density matrices; a TLS body inside a cluster; the terms
of §7.

### Log

**2026-10-12.** Step 1 written on branch `tc-tls` (`b43cb188`), not built. It needs a TLS model
already refined with a one-centre partition; `make_two_center_ADPs` stops otherwise. The input for
check (c) is `~/tonto-runs/2026-10-12_tc_tls/step1/stdin`: the urea STO-3G TLS test, then
`make_structure_factors` with `oc-hirshfeld`, `tc-tanaka` and `tc-tls` in turn. Not yet known:
whether re-reading `partition_model=` inside `crystal=` keeps the refined TLS model.

**2026-10-12, step 1 built and run.** Debug and release both compile; the debug run (bounds
checking on) goes through all three models without a stop. Re-reading `partition_model=` inside
`crystal= { xray_data= { } }` keeps the refined TLS model. Urea, RHF/STO-3G, 817 reflections,
parameters from the converged `adp_model= tls` Hirshfeld refinement, the scale factor re-fitted
for each model (release build; `put_structure_factor_stats` after each `make_structure_factors`):

| smearing | GoF | R(F) | Rw(F) | scale |
|---|---|---|---|---|
| `oc-hirshfeld` | 7.1801 | 0.0387 | 0.0299 | 0.9812 |
| `tc-tanaka` | 7.2709 | 0.0389 | 0.0303 | 0.9814 |
| `tc-tls` | 7.2552 | 0.0388 | 0.0302 | 0.9811 |

The Hirshfeld row reproduces the refinement's own GoF. The positions and $`\Sigma`$ were refined
for Hirshfeld, so the two-centre rows are at a disadvantage until step 2 refines them. `tc-tls`
is below `tc-tanaka`, the direction (14) predicts, but by little, and STO-3G is too poor a basis
to say more. Checks (a) and (b) of step 1 not done. Runs in
`~/tonto-runs/2026-10-12_tc_tls/step1/{debug,release}`.

**2026-10-12, the same at def2-SVP** (Dylan: STO-3G is too poor). Same input with the basis
changed, so the Hessian is the one in the STO-3G test input; release build, 11 s.

| smearing | GoF | R(F) | Rw(F) | scale |
|---|---|---|---|---|
| `oc-hirshfeld` | 3.4866 | 0.0180 | 0.0145 | 1.0002 |
| `tc-tanaka` | 3.6561 | 0.0184 | 0.0152 | 1.0007 |
| `tc-tls` | 3.6461 | 0.0183 | 0.0152 | 1.0002 |

The same order as STO-3G, and the same caution: only the Hirshfeld row is refined. Run in
`~/tonto-runs/2026-10-12_tc_tls/step1_svp`.

**2026-10-12, step 2 built and run** (branch `tc-tls`). Written: `SHELL2`-level moments
(`MOLECULE.XTAL:make_FT_pair_moments`, `make_FT_and_TLS_derivs`),
`CRYSTAL:get_two_center_parameter_shifts`, the fit loop `MOLECULE.HAR:LS_fit_two_center` (the
crystal's `LS_structure_fit` split into `start_structure_fit`, `apply_parameter_shifts`,
`finish_fit_cycle`, `finish_structure_fit`, used by both), direct parameter derivatives in
`solve_normal_equations`, the keyword `check_two_center_derivs`, and `make_FT_deriv_U` deleted.
Only $`s_0`$, $`s_1`$, $`s_2`$ are needed per shell pair (weights 1, $`w_a`$, $`w_a^2`$), since
$`w_b = 1 - w_a`$. Debug and release build; the refinement runs and converges.

*The derivative check* (urea, def2-SVP): the $`\Sigma`$ derivatives agree with central differences
to 1.9e-8 of the largest. The position number is 13 % (9 % at STO-3G): the finite difference
moves each atom with its basis functions, which changes the size and shape of every product at
fixed $`D`$; the analytic derivative moves only the centres. So 13 % is the size of the term the
refinement leaves out.

*That term matters.* Urea, RHF/def2-SVP, 817 reflections, Hessian RHF/def2-SVP from ORCA at the
Hirshfeld-refined geometry (`scripts/orca_hessian_to_tonto.py`), frequencies scaled by 0.8953:

| what | GoF (17 parameters) | R(F) |
|---|---|---|
| `oc-hirshfeld`, refined | 3.4854 | 0.0181 |
| `tc-tls` at the Hirshfeld parameters | 3.6451 | 0.0183 |
| `tc-tls`, only the TLS parameters refined from there | 3.6377 | 0.0185 |
| `tc-tls`, everything refined from the starting CIF | 3.6577 | 0.0186 |

The third row is printed as 3.6197 with 9 parameters; 3.6377 is the same chi^2 counted with 17.
Refining only $`\Sigma`$ lowers the GoF steadily, as exact derivatives must. Refining the positions
too ends *higher* than the Hirshfeld geometry gives. With hydrogen positions refined and the heavy
atoms held, the first shift is in H3 z and the GoF rises at once, so it is mostly the hydrogens.
The fit ends where $`J^{\mathsf T} W r = 0`$ for the approximate $`J`$, which is not the minimum of
the GoF. Open: what the position derivative should be (§10, to be decided). With the earlier
RHF/6-31G(d) Hessian the four numbers are 3.4866, 3.6461, 3.6388, 3.6591: the Hessian hardly
matters here. Runs in `~/tonto-runs/2026-10-12_tc_tls/{step2,diag_adps_only,svp_orca,orca_hess}`.
