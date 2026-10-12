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

- D. Jayatilaka and D. J. Grimwood, *Acta Cryst.* A57, 76 (2001), §4.2: the two-centre temperature
  factors in Tonto.
- R. F. Stewart, *J. Chem. Phys.* 51, 4569 (1969); P. Coppens, T. V. Willoughby and L. N. Csonka,
  *Acta Cryst.* A27, 248 (1971); K. Tanaka, *Acta Cryst.* A44, 1002 (1988): the earlier two-centre models.
- V. Schomaker and K. N. Trueblood, *Acta Cryst.* B24, 63 (1968): T, L and S.
