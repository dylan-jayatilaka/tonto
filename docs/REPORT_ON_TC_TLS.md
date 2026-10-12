# Smearing the density between two atoms: `tc-tls`

Hirshfeld atom refinement cuts the molecular density into atoms and smears each atom with its
own ADP. The cut is an approximation. The density is a sum over pairs of basis functions, and a
product of functions on two different atoms sits between them and moves with both. To smear it
one must know how the two atoms move together, which independent ADPs do not say. The model of
rigid-body motion and internal modes in `docs/REPORT_ON_MODE_FITTING.md` does say it, so with
that model no cut is needed. This page gives the equations, the keywords, where the code is, and
what it gives on urea.

That report is the starting point, and its notation is used here. Its equations are cited as
(M1), (M2), and so on, and its sections as §M1, §M1.4, and so on. Units: atomic units; vectors
are columns; ᵀ is the transpose; ⟨ ⟩ is the thermal average.


## 1. The covariance between two atoms

Collect every generalised coordinate in one vector: the rigid-body motion and refined soft modes
of (M11), and the amplitudes $`Q_k`$ of the stiff modes. For atom $`A`$,

```math
\mathbf{u}_A = \bar B_A\, \bar{\mathbf{v}}, \qquad
\bar{\mathbf{v}} = (\mathbf{t}, \boldsymbol{\lambda}, Q_1, \dots, Q_K, Q_{\rm stiff}), \qquad
\bar B_A = [\,B_A \mid \mathbf{d}_{A,\rm stiff}\,] , \qquad (1)
```

with $`B_A`$ the matrix of (M11) and one column $`\mathbf{d}_{Ak}`$ (§M1.7) for each stiff mode. The covariance
of $`\bar{\mathbf{v}}`$ has two blocks: the refined $`\Sigma`$ of (M11), and a diagonal block holding the harmonic
amplitude (M6) of each stiff mode at the temperature of the data,

```math
\bar\Sigma = \langle \bar{\mathbf{v}}\bar{\mathbf{v}}^{\mathsf T}\rangle = \begin{pmatrix} \Sigma & 0 \\ 0 & \mathrm{diag}\,\langle Q_k^2\rangle \end{pmatrix} .
```

The zeros say that the stiff modes are taken as uncorrelated with the rigid-body motion and the
soft modes, and with each other. The **two-centre ADP** of atoms
$`A`$ and $`B`$ is then

```math
U_{AB} = \langle \mathbf{u}_A \mathbf{u}_B^{\mathsf T} \rangle
       = B_A\, \Sigma\, B_B^{\mathsf T} + \sum_{\omega_k > \omega_c} \langle Q_k^2\rangle\, \mathbf{d}_{Ak} \mathbf{d}_{Bk}^{\mathsf T} . \qquad (2)
```

$`U_{AA}`$ is the ADP of atom $`A`$, (M11). $`U_{AB}`$ is not symmetric, and $`U_{BA} = U_{AB}^{\mathsf T}`$. It needs no
parameter beyond those already in the model.

## 2. Basis functions, primitives and their products

The electron density is a sum over pairs of basis functions,

```math
\rho(\mathbf{r}) = \sum_{\mu\nu} D_{\mu\nu}\, \chi_\mu(\mathbf{r} - \mathbf{R}_A)\, \chi_\nu(\mathbf{r} - \mathbf{R}_B) , \qquad (3)
```

where $`\chi_\mu`$ is a basis function on atom $`A`$ at position $`\mathbf{R}_A`$, $`\chi_\nu`$ one on atom $`B`$ at
$`\mathbf{R}_B`$, and $`D`$ is the **density matrix** from the SCF: $`D_{\mu\nu}`$ says how much of the product
$`\chi_\mu\chi_\nu`$ is in the density. Each basis function is a fixed sum (a contraction) of
**primitive** Gaussians $`\chi_{\mu a}`$, a polynomial times a Gaussian with exponent $`\alpha_{\mu a}`$:

```math
\chi_\mu(\mathbf{r}) = \sum_a c_{\mu a}\, \chi_{\mu a}(\mathbf{r}), \qquad
\chi_{\mu a}(\mathbf{r}) = x^{l} y^{m} z^{n}\, e^{-\alpha_{\mu a} r^2}, \qquad \mathbf{r} = (x, y, z) , \qquad (4)
```

with $`c_{\mu a}`$ the contraction coefficients, which include the normalisation, and $`l, m, n`$ the
powers that make the function s, p, d and so on. Write $`\chi_{\mu a\,\nu b}`$ for the product of
a primitive on $`A`$ with a primitive on $`B`$. Then (3) is a sum of such products:

```math
\chi_{\mu a\,\nu b}(\mathbf{r}) = \chi_{\mu a}(\mathbf{r} - \mathbf{R}_A)\, \chi_{\nu b}(\mathbf{r} - \mathbf{R}_B), \qquad
\rho(\mathbf{r}) = \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} c_{\mu a} c_{\nu b}\; \chi_{\mu a\,\nu b}(\mathbf{r}) . \qquad (5)
```

Everything below is done for one product $`\chi_{\mu a\,\nu b}`$ and then summed as in (5).

**The product is a Gaussian about its own centre.** For the product $`\chi_{\mu a\,\nu b}`$ write $`\alpha`$
for the exponent $`\alpha_{\mu a}`$ of its primitive on $`A`$ and $`\beta`$ for the exponent of its primitive
on $`B`$, and define

```math
\gamma = \alpha + \beta, \quad
w_A = \frac{\alpha}{\gamma}, \quad w_B = \frac{\beta}{\gamma}, \quad
\xi = \frac{\alpha\beta}{\gamma}, \quad
\mathbf{R} = \mathbf{R}_A - \mathbf{R}_B, \quad \mathbf{P} = w_A \mathbf{R}_A + w_B \mathbf{R}_B . \qquad (6)
```

$`\alpha`$, $`\beta`$, $`\gamma`$, $`w_A`$, $`w_B`$, $`\xi`$ and $`\mathbf{P}`$ belong to the pair of primitives $`\mu a, \nu b`$; the indices
are left off them to keep the formulas readable. $`w_A + w_B = 1`$, and $`\mathbf{P}`$ is a point on the
line between the atoms. Completing the square in the two exponents gives the Gaussian product
theorem,

```math
\alpha |\mathbf{r} - \mathbf{R}_A|^2 + \beta |\mathbf{r} - \mathbf{R}_B|^2
   = \gamma\, |\mathbf{r} - \mathbf{P}|^2 + \xi\, |\mathbf{R}|^2 , \qquad (7)
```

and the two polynomials are functions of $`\mathbf{r} - \mathbf{R}_A = (\mathbf{r} - \mathbf{P}) - w_B \mathbf{R}`$ and
$`\mathbf{r} - \mathbf{R}_B = (\mathbf{r} - \mathbf{P}) + w_A \mathbf{R}`$. So the product depends on $`\mathbf{r}`$ only through
$`\mathbf{s} = \mathbf{r} - \mathbf{P}`$, the position measured from its centre. Call the product written that way
$`\bar\chi_{\mu a\,\nu b}`$:

```math
\chi_{\mu a\,\nu b}(\mathbf{r}) = \bar\chi_{\mu a\,\nu b}(\mathbf{r} - \mathbf{P};\, \mathbf{R}), \qquad
\bar\chi_{\mu a\,\nu b}(\mathbf{s}; \mathbf{R}) = \chi^{\rm poly}_{\mu a}(\mathbf{s} - w_B \mathbf{R})\; \chi^{\rm poly}_{\nu b}(\mathbf{s} + w_A \mathbf{R})\; e^{-\xi |\mathbf{R}|^2}\, e^{-\gamma s^2} , \qquad (8)
```

where $`\chi^{\rm poly}`$ is the polynomial $`x^l y^m z^n`$ of (4). The centre $`\mathbf{P}`$ says where the
product is; the vector $`\mathbf{R}`$ between the atoms sets its size, through $`e^{-\xi|\mathbf{R}|^2}`$, and
its shape, through the polynomials.

**Its Fourier transform.** At scattering vector $`\mathbf{k}`$, changing the variable of integration
from $`\mathbf{r}`$ to $`\mathbf{s}`$,

```math
\int \chi_{\mu a\,\nu b}(\mathbf{r})\, e^{i\mathbf{k}\cdot\mathbf{r}}\, d\mathbf{r}
   = e^{i\mathbf{k}\cdot\mathbf{P}}\; \tilde\chi_{\mu a\,\nu b}(\mathbf{k}; \mathbf{R}), \qquad
\tilde\chi_{\mu a\,\nu b}(\mathbf{k}; \mathbf{R}) = \int \bar\chi_{\mu a\,\nu b}(\mathbf{s}; \mathbf{R})\, e^{i\mathbf{k}\cdot\mathbf{s}}\, d\mathbf{s} . \qquad (9)
```

For two s functions, for example,
$`\tilde\chi_{\mu a\,\nu b} = (\pi/\gamma)^{3/2}\, e^{-\xi|\mathbf{R}|^2}\, e^{-k^2/4\gamma}`$. With (5), the structure
factor of the molecule at rest is
$`\sum_{\mu\nu} D_{\mu\nu} \sum_{ab} c_{\mu a} c_{\nu b}\, e^{i\mathbf{k}\cdot\mathbf{P}}\, \tilde\chi_{\mu a\,\nu b}(\mathbf{k}; \mathbf{R})`$.

**Under displacement.** Move atom $`A`$ by $`\mathbf{u}_A`$ and atom $`B`$ by $`\mathbf{u}_B`$, the basis functions
moving with their atoms. In (6), $`\mathbf{P}`$ becomes $`\mathbf{P} + \mathbf{u}_P`$ and $`\mathbf{R}`$ becomes
$`\mathbf{R} + \mathbf{u}_A - \mathbf{u}_B`$, with

```math
\mathbf{u}_P = w_A \mathbf{u}_A + w_B \mathbf{u}_B . \qquad (10)
```

So, from (9), the Fourier transform of the displaced product is exactly

```math
e^{i\mathbf{k}\cdot\mathbf{P}}\; e^{i\mathbf{k}\cdot\mathbf{u}_P}\; \tilde\chi_{\mu a\,\nu b}(\mathbf{k};\, \mathbf{R} + \mathbf{u}_A - \mathbf{u}_B) . \qquad (11)
```

The displacement does two things. The centre of the product moves by $`\mathbf{u}_P`$, which gives the
phase $`e^{i\mathbf{k}\cdot\mathbf{u}_P}`$. And the vector between the atoms changes by the relative displacement
$`\mathbf{u}_A - \mathbf{u}_B`$, which changes $`\tilde\chi_{\mu a\,\nu b}`$: in (8) that vector sets how large the
product is, through $`e^{-\xi|\mathbf{R}|^2}`$, and enters the two polynomials.

## 3. The model

**Each product is smeared by the motion of its centre.** In the thermal average of (11) the
factor $`e^{i\mathbf{k}\cdot\mathbf{u}_P}`$ is averaged. Two other things in a term of (5) change when the
atoms move, and both are held at their values at the mean geometry and taken out of the average:
the transform $`\tilde\chi_{\mu a\,\nu b}`$, whose argument $`\mathbf{R} + \mathbf{u}_A - \mathbf{u}_B`$ in (11) is replaced by
$`\mathbf{R}`$, and the density matrix element $`D_{\mu\nu}`$. This is the rigid-atom approximation of Hirshfeld atom refinement,
made for each product instead of each atom.

The average is done as for an atom (§M1.1). The displacements are Gaussian with zero mean, so
$`x = \mathbf{k}\cdot\mathbf{u}_P`$ is a Gaussian variable with zero mean, and for such a variable
$`\langle e^{ix}\rangle = e^{-\langle x^2\rangle/2}`$. Its mean square is
$`\langle x^2\rangle = \mathbf{k}^{\mathsf T} \langle \mathbf{u}_P \mathbf{u}_P^{\mathsf T}\rangle\, \mathbf{k}`$, so

```math
\langle e^{i\mathbf{k}\cdot\mathbf{u}_P} \rangle = e^{-\frac12 \mathbf{k}^{\mathsf T} U^P_{\mu a\,\nu b}\, \mathbf{k}}, \qquad
U^P_{\mu a\,\nu b} = \langle \mathbf{u}_P \mathbf{u}_P^{\mathsf T} \rangle . \qquad (12)
```

Putting (10) into $`\langle \mathbf{u}_P \mathbf{u}_P^{\mathsf T}\rangle`$ and multiplying out, with $`U_A = \langle\mathbf{u}_A\mathbf{u}_A^{\mathsf T}\rangle`$
and $`U_{AB} = \langle\mathbf{u}_A\mathbf{u}_B^{\mathsf T}\rangle`$ of (2),

```math
U^P_{\mu a\,\nu b} = w_A^2\, U_A + w_B^2\, U_B + w_A w_B\,(U_{AB} + U_{AB}^{\mathsf T}) . \qquad (13)
```

It depends on the pair of primitives through $`w_A`$ and $`w_B`$. Equation (13) is equation (10) of
Michael and Koritsanszky (2015), who give it as the exact ADP of a product of two primitive
Gaussians when the covariance of the nuclear displacements is known. They took that covariance
from the internal normal modes of an isolated molecule, with the coth amplitudes (M6), and
smeared the density itself; here the covariance comes from (2), with the rigid-body motion and
the soft modes refined against the data. The structure factor is then

```math
\begin{aligned}
F(\mathbf{k}) &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} c_{\mu a} c_{\nu b}\;
     e^{i\mathbf{k}\cdot\mathbf{P}}\; \tilde\chi_{\mu a\,\nu b}(\mathbf{k}; \mathbf{R})\; e^{-\frac12 \mathbf{k}^{\mathsf T} U^P_{\mu a\,\nu b} \mathbf{k}} \\
   &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} t_{\mu a\,\nu b}(\mathbf{k}), \qquad
   t_{\mu a\,\nu b}(\mathbf{k}) = c_{\mu a} c_{\nu b}\;
     e^{i\mathbf{k}\cdot\mathbf{P}}\; \tilde\chi_{\mu a\,\nu b}(\mathbf{k}; \mathbf{R})\; e^{-\frac12 \mathbf{k}^{\mathsf T} U^P_{\mu a\,\nu b} \mathbf{k}} .
\end{aligned}
\qquad (14)
```

The second line names one term, $`t_{\mu a\,\nu b}`$, for use below. No partition into atoms appears
in (14). When (14) is made from one molecule and summed over the space-group operations, each
pair of atoms has a weight that counts it once; that is in `docs/REPORT_ON_PAIR_STABILIZERS.md`.

**The centre is a pseudo-atom.** Putting (1) into (10), and using $`w_A + w_B = 1`$,

```math
\mathbf{u}_P = \bar B_P\, \bar{\mathbf{v}}, \qquad \bar B_P = w_A \bar B_A + w_B \bar B_B , \qquad
U^P_{\mu a\,\nu b} = \bar B_P\, \bar\Sigma\, \bar B_P^{\mathsf T} , \qquad (15)
```

with $`\bar\Sigma = \langle\bar{\mathbf{v}}\bar{\mathbf{v}}^{\mathsf T}\rangle`$. This is (M11) for a fictitious atom at $`\mathbf{P}`$. Its
rigid-body part is the Schomaker–Trueblood ADP (M5) of the point $`\mathbf{P}`$, because $`A_i`$ is linear in
the position, so $`w_A A_A + w_B A_B`$ is the matrix $`A`$ of the point $`\mathbf{P}`$. Its mode part uses
each mode's displacement interpolated to $`\mathbf{P}`$, $`\mathbf{d}_{Pk} = w_A \mathbf{d}_{Ak} + w_B \mathbf{d}_{Bk}`$, for the
stiff modes with the amplitude (M6) and for the soft modes with the refined one. Three things
follow:

- The refined parameters are those of §M1: $`\Sigma`$ and the positions. None is added.
- For two functions on the same atom, $`\mathbf{P} = \mathbf{R}_A`$ and $`U^P_{\mu a\,\nu b} = U_A`$.
- For functions on atoms of two different molecules the model gives no correlation, and
  $`U_{AB} = 0`$ is used.

**Relation to earlier two-centre models.** Write
$`V_{AB} = \langle (\mathbf{u}_A - \mathbf{u}_B)(\mathbf{u}_A - \mathbf{u}_B)^{\mathsf T}\rangle = U_A + U_B - U_{AB} - U_{AB}^{\mathsf T}`$ for the
mean-square relative displacement of the two atoms. Since $`w_A^2 = w_A - w_A w_B`$ and
$`w_B^2 = w_B - w_A w_B`$, (13) is

```math
U^P_{\mu a\,\nu b} = w_A U_A + w_B U_B - w_A w_B\, V_{AB} . \qquad (16)
```

The first two terms are Tanaka's model (`tc-tanaka`). It is exact when the atoms have no relative
motion, and otherwise smears the product too much: a bond stretch moves the two atoms in
opposite directions and leaves the density between them nearly still. Stewart's model
(`tc-stewart`) puts every product at the midpoint, and its two cases are $`V_{AB} = 0`$ for bonded
atoms and $`U_{AB} = 0`$ for the rest. Those models cannot give $`U_{AB}`$ from $`U_A`$ and $`U_B`$; here it
comes from (2).

**How much the correlation matters, and how good the model is.** Michael and Koritsanszky (2015)
tested both on formamide at 23 K, internal modes only. Leaving out $`U_{AB}`$ changed the smeared
density by at most 0.015 e/Å³, at the nitrogen nucleus. Against the average of 500 000 static
densities, each calculated at a geometry drawn from the distribution of the normal modes, the
smeared density (13) gave nearly the same values at the bond critical points. That average is
the quantity §6 estimates with two geometries for each stiff mode.

## 4. What the model leaves out

Let $`\mathbf{x}`$ be the $`3N`$ displacements of all the atoms, and
$`f(\mathbf{x}) = D_{\mu\nu}\, \tilde\chi_{\mu a\,\nu b}(\mathbf{k};\, \mathbf{R} + \mathbf{u}_A - \mathbf{u}_B)`$ the part of one term of (5)
that the model holds fixed, as a function of them; $`D_{\mu\nu}`$ depends on $`\mathbf{x}`$ too. The exact
average of that term is

```math
\langle f(\mathbf{x})\, e^{i\mathbf{k}\cdot\mathbf{u}_P} \rangle
   = e^{-\frac12 \mathbf{k}^{\mathsf T} U^P_{\mu a\,\nu b} \mathbf{k}}\; \langle f(\mathbf{x} + i\,U\mathbf{c}) \rangle , \qquad (17)
```

where $`U`$ is the $`3N\times 3N`$ matrix with blocks $`U_{AB}`$, and $`\mathbf{c}`$ is the $`3N`$-vector holding
$`w_A\mathbf{k}`$ on atom $`A`$, $`w_B\mathbf{k}`$ on atom $`B`$ and zero elsewhere, so that $`\mathbf{k}\cdot\mathbf{u}_P = \mathbf{c}\cdot\mathbf{x}`$.
It follows from completing the square in the Gaussian average. The approximation of §3
replaces the last factor by $`f(0)`$. What that leaves out is:

- the change of $`\tilde\chi_{\mu a\,\nu b}`$ with the relative displacement $`\mathbf{u}_A - \mathbf{u}_B`$;
- the change of the density matrix with the geometry;
- the turning of each product about its own centre when the molecule librates.

The first two must be taken together: with the first alone the number of electrons is not
conserved.

The change of the density matrix can be estimated by first-order perturbation theory without
solving coupled equations, the sum over orbitals used for the X-ray constrained SCF
(`docs/REPORT_ON_XCW_CONVERGENCE.md`, equation (4)). There the perturbation is a change of the
Fock matrix; for a moved atom it is the change of the Fock matrix and of the overlap matrix,
because the basis functions move with the atom. Tonto instead does an SCF at each displaced
geometry: for the position derivatives in §5, and for the stiff modes in §6.

## 5. Derivatives for the refinement

The refinement changes $`\Sigma`$ and the atom positions (§M2.2), so it needs the derivative of the
structure factor (14) with respect to each element $`\Sigma_{pq}`$ and each position $`\mathbf{R}_C`$. Both are
found by differentiating (14) term by term.

**With respect to $`\Sigma`$.** Two facts are used. First, by (15) and the blocks of §M1,

```math
U^P_{\mu a\,\nu b} = \bar B_P\, \bar\Sigma\, \bar B_P^{\mathsf T}
   = B_P\, \Sigma\, B_P^{\mathsf T} + \sum_{\omega_k > \omega_c} \langle Q_k^2\rangle\, \mathbf{d}_{Pk} \mathbf{d}_{Pk}^{\mathsf T}, \qquad
B_P = w_A B_A + w_B B_B , \qquad (18)
```

of which only the first term contains $`\Sigma`$. Second, $`\Sigma`$ is symmetric, and its element
$`\Sigma_{pq}`$ with $`p \ne q`$ stands for the two equal elements $`\Sigma_{pq}`$ and $`\Sigma_{qp}`$. Then,
differentiating (14),

```math
\begin{aligned}
\frac{\partial F}{\partial \Sigma_{pq}}
 &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} c_{\mu a} c_{\nu b}\; e^{i\mathbf{k}\cdot\mathbf{P}}\; \tilde\chi_{\mu a\,\nu b}(\mathbf{k}; \mathbf{R})\;
    \frac{\partial}{\partial \Sigma_{pq}} \Big( e^{-\frac12 \mathbf{k}^{\mathsf T} U^P_{\mu a\,\nu b} \mathbf{k}} \Big) \\
 &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab}
    \underbrace{ c_{\mu a} c_{\nu b}\; e^{i\mathbf{k}\cdot\mathbf{P}}\; \tilde\chi_{\mu a\,\nu b}(\mathbf{k}; \mathbf{R})\; e^{-\frac12 \mathbf{k}^{\mathsf T} U^P_{\mu a\,\nu b} \mathbf{k}} }_{t_{\mu a\,\nu b}\ \text{of (14)}}
    \Big( -\tfrac12\, \frac{\partial}{\partial \Sigma_{pq}} \big( \mathbf{k}^{\mathsf T} U^P_{\mu a\,\nu b}\, \mathbf{k} \big) \Big) \\
 &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} t_{\mu a\,\nu b}
    \Big( -\tfrac12\, \frac{\partial}{\partial \Sigma_{pq}} \big( \mathbf{k}^{\mathsf T} B_P\, \Sigma\, B_P^{\mathsf T} \mathbf{k} \big) \Big)
    && \text{by (18)} \\
 &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} t_{\mu a\,\nu b}
    \Big( -\tfrac12\, \frac{\partial}{\partial \Sigma_{pq}} \big( \mathbf{h}_P^{\mathsf T}\, \Sigma\, \mathbf{h}_P \big) \Big)
    && \text{with } \mathbf{h}_P = B_P^{\mathsf T}\mathbf{k} \\
 &= -\frac{2-\delta_{pq}}{2} \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} t_{\mu a\,\nu b}\; h_{P,p}\, h_{P,q}
    && \text{since } \frac{\partial ( \mathbf{h}^{\mathsf T} \Sigma\, \mathbf{h} )}{\partial \Sigma_{pq}} = (2-\delta_{pq})\, h_p h_q \\
 &= -\frac{2-\delta_{pq}}{2} \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} t_{\mu a\,\nu b}\;
    ( w_A h_{A,p} + w_B h_{B,p} )( w_A h_{A,q} + w_B h_{B,q} )
    && \text{with } \mathbf{h}_P = w_A \mathbf{h}_A + w_B \mathbf{h}_B,\ \ \mathbf{h}_A = B_A^{\mathsf T}\mathbf{k} \\
 &= -\frac{2-\delta_{pq}}{2} \sum_{\mu\nu} \Big[\,
      h_{A,p} h_{A,q} \Big( D_{\mu\nu} \sum_{ab} w_A^2\; t_{\mu a\,\nu b} \Big)
    + h_{B,p} h_{B,q} \Big( D_{\mu\nu} \sum_{ab} (1-w_A)^2\; t_{\mu a\,\nu b} \Big) \\
 &\qquad\qquad\qquad\quad
    + ( h_{A,p} h_{B,q} + h_{B,p} h_{A,q} ) \Big( D_{\mu\nu} \sum_{ab} w_A (1-w_A)\; t_{\mu a\,\nu b} \Big) \Big] .
\end{aligned}
\qquad (19)
```

Here $`\delta_{pq}`$ is the Kronecker delta, one if $`p = q`$ and zero otherwise. The last step uses
two things: $`\mathbf{h}_A`$ and $`\mathbf{h}_B`$ belong to the atoms and are the same for every
primitive, so they come outside the sum over $`a, b`$; and $`w_B = 1 - w_A`$. The three sums in round
brackets are all made of

```math
s^{\mu\nu}_n(\mathbf{k}) = D_{\mu\nu} \sum_{ab} w_A^{\,n}\; t_{\mu a\,\nu b}, \qquad n = 0, 1, 2 , \qquad (20)
```

since $`(1-w_A)^2 = 1 - 2w_A + w_A^2`$ and $`w_A(1-w_A) = w_A - w_A^2`$. $`s^{\mu\nu}_0`$ is the contribution of
the pair $`\mu\nu`$ to $`F`$ itself, $`F = \sum_{\mu\nu} s^{\mu\nu}_0`$. So

```math
\frac{\partial F}{\partial \Sigma_{pq}} = -\frac{2-\delta_{pq}}{2} \sum_{\mu\nu}
   \Big[ s^{\mu\nu}_2\, h_{A,p}\, h_{A,q} + (s^{\mu\nu}_0 - 2 s^{\mu\nu}_1 + s^{\mu\nu}_2)\, h_{B,p}\, h_{B,q}
       + (s^{\mu\nu}_1 - s^{\mu\nu}_2)\,( h_{A,p}\, h_{B,q} + h_{B,p}\, h_{A,q} ) \Big] , \qquad (21)
```

where $`A`$ is the atom of $`\mu`$ and $`B`$ the atom of $`\nu`$. An atom outside the rigid body does not
move with $`\Sigma`$, and has $`\mathbf{h} = 0`$.

**With respect to the position of an atom.** A position enters a term of (14) in three places:
the centre $`\mathbf{P}`$, the vector $`\mathbf{R}`$ in $`\tilde\chi_{\mu a\,\nu b}`$, and the density matrix element $`D_{\mu\nu}`$.
Tonto offers three derivatives, which keep one, two or all three of these
(`tc_position_derivs=`):

- `center`. Only $`\mathbf{P}`$ is moved. By (6), $`\partial\mathbf{P}/\partial\mathbf{R}_A = w_A`$ and $`\partial\mathbf{P}/\partial\mathbf{R}_B = w_B = 1 - w_A`$.
  For atom $`C`$, a term depends on $`\mathbf{R}_C`$ if $`\mu`$ is on $`C`$, or $`\nu`$ is, or both:

```math
\begin{aligned}
\frac{\partial F}{\partial \mathbf{R}_C}
 &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} c_{\mu a} c_{\nu b}\; \tilde\chi_{\mu a\,\nu b}(\mathbf{k}; \mathbf{R})\; e^{-\frac12 \mathbf{k}^{\mathsf T} U^P_{\mu a\,\nu b} \mathbf{k}}\;
    \frac{\partial}{\partial \mathbf{R}_C} \Big( e^{i\mathbf{k}\cdot\mathbf{P}} \Big) \\
 &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} t_{\mu a\,\nu b}\;\, i\mathbf{k}\, \frac{\partial \mathbf{P}}{\partial \mathbf{R}_C} \\
 &= i\mathbf{k} \Big[ \sum_{\mu\nu,\ \mu \text{ on } C} D_{\mu\nu} \sum_{ab} w_A\; t_{\mu a\,\nu b}
              + \sum_{\mu\nu,\ \nu \text{ on } C} D_{\mu\nu} \sum_{ab} (1 - w_A)\; t_{\mu a\,\nu b} \Big] \\
 &= i\mathbf{k} \Big[ \sum_{\mu\nu,\ \mu \text{ on } C} s^{\mu\nu}_1 + \sum_{\mu\nu,\ \nu \text{ on } C} ( s^{\mu\nu}_0 - s^{\mu\nu}_1 ) \Big] .
\end{aligned}
\qquad (22)
```

- `fixed-dm`. $`\mathbf{P}`$ and $`\mathbf{R}`$ are moved: the basis functions move with the atom, and the density
  matrix is held. By central differences of (14), step 0.01 bohr.
- `scf`. All three: as `fixed-dm`, with an SCF at each displaced geometry, started from the
  undisplaced orbitals, so that the density matrix follows the atom.

In all three a unique atom moves together with its symmetry images in the molecule. The
derivatives of `fixed-dm` and `scf` are made once for each fit with a given density matrix.

## 6. The stiff modes averaged numerically

§3 made an approximation: two quantities that change when the atoms move were held at their
values at the mean geometry. This section removes that approximation for the stiff modes, one
mode at a time, by doing the average numerically (`tc_integrate_stiff_modes=`).

**What changes when one mode moves.** Take one stiff mode $`k`$ and give its normal coordinate the
value $`Q`$, all other motion aside for the moment. Atom $`A`$ is displaced by $`Q\,\mathbf{d}_{Ak}`$ (§M1.7).
By (11), with $`\mathbf{u}_A = Q\,\mathbf{d}_{Ak}`$ and $`\mathbf{u}_B = Q\,\mathbf{d}_{Bk}`$, the product $`\chi_{\mu a\,\nu b}`$ contributes

```math
D_{\mu\nu}(Q)\; c_{\mu a} c_{\nu b}\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   e^{iQ\,\mathbf{k}\cdot\mathbf{d}_{Pk}}\;
   \tilde\chi_{\mu a\,\nu b}\big(\mathbf{k};\, \mathbf{R} + Q(\mathbf{d}_{Ak} - \mathbf{d}_{Bk})\big), \qquad
\mathbf{d}_{Pk} = w_A \mathbf{d}_{Ak} + w_B \mathbf{d}_{Bk} . \qquad (23)
```

Three things in (23) depend on $`Q`$:

1. the phase $`e^{iQ\,\mathbf{k}\cdot\mathbf{d}_{Pk}}`$, because the centre of the product moves by $`Q\,\mathbf{d}_{Pk}`$;
2. the transform $`\tilde\chi_{\mu a\,\nu b}`$, because the vector between the two atoms changes from $`\mathbf{R}`$
   to $`\mathbf{R} + Q(\mathbf{d}_{Ak} - \mathbf{d}_{Bk})`$. In (8) that vector enters through the factor $`e^{-\xi|\mathbf{R}|^2}`$,
   which sets how large the product is, and through the two polynomials;
3. the density matrix element $`D_{\mu\nu}(Q)`$, because the wavefunction at the displaced geometry is
   not the one at the mean geometry.

**Two structure factors as functions of $`Q`$.** The other motion smears each product as before.
Let $`W^{(k)}_{\mu a\,\nu b}`$ be the temperature factor of (14) with the term of mode $`k`$ taken out of
$`U^P_{\mu a\,\nu b}`$, (18). $`F^{\rm exact}_k(Q)`$ keeps all three dependences of (23); $`F^{\rm approx}_k(Q)`$
keeps only the phase, as §3 does:

```math
\begin{aligned}
W^{(k)}_{\mu a\,\nu b} &= \exp\!\Big[ -\tfrac12\, \mathbf{k}^{\mathsf T} \big( U^P_{\mu a\,\nu b} - \langle Q_k^2\rangle\, \mathbf{d}_{Pk}\mathbf{d}_{Pk}^{\mathsf T} \big)\, \mathbf{k} \Big] , \\
F^{\rm exact}_k(Q) &= \sum_{\mu\nu} D_{\mu\nu}(Q) \sum_{ab} c_{\mu a} c_{\nu b}\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   e^{iQ\,\mathbf{k}\cdot\mathbf{d}_{Pk}}\;
   \tilde\chi_{\mu a\,\nu b}\big(\mathbf{k};\, \mathbf{R} + Q(\mathbf{d}_{Ak} - \mathbf{d}_{Bk})\big)\; W^{(k)}_{\mu a\,\nu b} , \\
F^{\rm approx}_k(Q) &= \sum_{\mu\nu} D_{\mu\nu}(0) \sum_{ab} c_{\mu a} c_{\nu b}\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   e^{iQ\,\mathbf{k}\cdot\mathbf{d}_{Pk}}\;
   \tilde\chi_{\mu a\,\nu b}(\mathbf{k};\, \mathbf{R})\; W^{(k)}_{\mu a\,\nu b} .
\end{aligned}
\qquad (24)
```

The two are equal at $`Q = 0`$.

**The average of the approximate one is the structure factor of §3.** $`Q`$ has a Gaussian
distribution with zero mean and mean square $`\langle Q_k^2\rangle`$, (M6), so
$`\langle e^{iQ\theta}\rangle = e^{-\frac12 \langle Q_k^2\rangle \theta^2}`$ for any number $`\theta`$, as in (12). Averaging
$`F^{\rm approx}_k`$ of (24) over $`Q`$:

```math
\begin{aligned}
\langle F^{\rm approx}_k \rangle
 &= \sum_{\mu\nu} D_{\mu\nu}(0) \sum_{ab} c_{\mu a} c_{\nu b}\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   \tilde\chi_{\mu a\,\nu b}(\mathbf{k};\, \mathbf{R})\; W^{(k)}_{\mu a\,\nu b}\; \big\langle e^{iQ\,\mathbf{k}\cdot\mathbf{d}_{Pk}} \big\rangle
   && \text{only the phase depends on } Q \\
 &= \sum_{\mu\nu} D_{\mu\nu}(0) \sum_{ab} c_{\mu a} c_{\nu b}\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   \tilde\chi_{\mu a\,\nu b}(\mathbf{k};\, \mathbf{R})\; W^{(k)}_{\mu a\,\nu b}\; e^{-\frac12 \langle Q_k^2\rangle (\mathbf{k}\cdot\mathbf{d}_{Pk})^2}
   && \text{with } \theta = \mathbf{k}\cdot\mathbf{d}_{Pk} \\
 &= \sum_{\mu\nu} D_{\mu\nu}(0) \sum_{ab} c_{\mu a} c_{\nu b}\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   \tilde\chi_{\mu a\,\nu b}(\mathbf{k};\, \mathbf{R})\; e^{-\frac12 \mathbf{k}^{\mathsf T} U^P_{\mu a\,\nu b} \mathbf{k}}
   && \text{by the definition of } W^{(k)}_{\mu a\,\nu b} \\
 &= F \text{ of (14)} .
\end{aligned}
\qquad (25)
```

The average of $`F^{\rm exact}_k`$ cannot be done by formula, because $`D_{\mu\nu}(Q)`$ is known only by
doing an SCF at each $`Q`$. It is done by three points.

**The three-point rule.** For any function $`g(Q)`$ of a Gaussian variable with zero mean and mean
square $`\langle Q_k^2\rangle`$, the three-point Gauss–Hermite rule is

```math
\langle g \rangle \approx \tfrac23\, g(0) + \tfrac16\, g(+s_k) + \tfrac16\, g(-s_k), \qquad
s_k = \sqrt{3\,\langle Q_k^2\rangle} , \qquad (26)
```

exact when $`g`$ is a polynomial of degree five or less.

**The error of §3 for mode $`k`$.** It is the difference of the two averages, $`\Delta_k`$. Applying
the rule (26) to the difference $`g(Q) = F^{\rm exact}_k(Q) - F^{\rm approx}_k(Q)`$:

```math
\begin{aligned}
\Delta_k &= \langle F^{\rm exact}_k \rangle - \langle F^{\rm approx}_k \rangle
          = \big\langle\, F^{\rm exact}_k(Q) - F^{\rm approx}_k(Q) \,\big\rangle \\
 &\approx \tfrac23 \big[ F^{\rm exact}_k(0) - F^{\rm approx}_k(0) \big]
        + \tfrac16 \big[ F^{\rm exact}_k(s_k) - F^{\rm approx}_k(s_k) \big]
        + \tfrac16 \big[ F^{\rm exact}_k(-s_k) - F^{\rm approx}_k(-s_k) \big]
   && \text{by (26)} \\
 &= \tfrac16 \big[ F^{\rm exact}_k(s_k) + F^{\rm exact}_k(-s_k) \big]
  - \tfrac16 \big[ F^{\rm approx}_k(s_k) + F^{\rm approx}_k(-s_k) \big]
   && \text{the two are equal at } Q = 0 \\
 &= \tfrac16 \big[ F^{\rm exact}_k(s_k) + F^{\rm exact}_k(-s_k) \big]
  - \tfrac16 \sum_{\mu\nu} D_{\mu\nu}(0) \sum_{ab} c_{\mu a} c_{\nu b}\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   \tilde\chi_{\mu a\,\nu b}(\mathbf{k}; \mathbf{R})\; W^{(k)}_{\mu a\,\nu b}\;
   \big( e^{i s_k \mathbf{k}\cdot\mathbf{d}_{Pk}} + e^{-i s_k \mathbf{k}\cdot\mathbf{d}_{Pk}} \big)
   && \text{by (24)} \\
 &= \tfrac16 \big[ \underbrace{F^{\rm exact}_k(s_k)}_{F_k^{+}} + \underbrace{F^{\rm exact}_k(-s_k)}_{F_k^{-}} \big]
  - \tfrac13 \underbrace{ \sum_{\mu\nu} D_{\mu\nu}(0) \sum_{ab} c_{\mu a} c_{\nu b}\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   \tilde\chi_{\mu a\,\nu b}(\mathbf{k}; \mathbf{R})\; W^{(k)}_{\mu a\,\nu b}\;
   \cos( s_k\, \mathbf{k}\cdot\mathbf{d}_{Pk} ) }_{F_k^{\cos}}
   && e^{i\phi} + e^{-i\phi} = 2\cos\phi \\
 &= \tfrac16 \big( F_k^{+} + F_k^{-} \big) - \tfrac13\, F_k^{\cos} .
\end{aligned}
\qquad (27)
```

**How the three are made.**

- $`F_k^{+}`$: move every atom $`A`$ by $`+s_k\,\mathbf{d}_{Ak}`$, do an SCF there, and make the structure factor
  from (14) with the new positions, the new density matrix, and $`W^{(k)}`$ as the temperature
  factor. Moving the atoms with their basis functions puts in the phase and the change of
  $`\tilde\chi_{\mu a\,\nu b}`$; the SCF puts in $`D_{\mu\nu}(s_k)`$. That is $`F^{\rm exact}_k(s_k)`$ of (24).
- $`F_k^{-}`$: the same with $`-s_k`$.
- $`F_k^{\cos}`$: at the mean geometry with its own density matrix, the structure factor (14) with
  $`W^{(k)}`$ as the temperature factor and each product of primitives multiplied by
  $`\cos(s_k\,\mathbf{k}\cdot\mathbf{d}_{Pk})`$.

**The correction.** Adding the stiff modes,

```math
\Delta F = \sum_{\omega_k > \omega_c} \Delta_k = \sum_{\omega_k > \omega_c} \Big[ \tfrac16 \big( F_k^{+} + F_k^{-} \big) - \tfrac13\, F_k^{\cos} \Big] , \qquad (28)
```

and the structure factor used in the refinement is (14) plus $`\Delta F`$.

- **Why the difference is integrated, not $`F^{\rm exact}_k`$ itself.** The phase in (24) varies
  quickly with $`Q`$ at high angle, where three points are too few; (25) does that part exactly.
  The difference is small and varies slowly, which is what the rule (26) is good for.
- **Why the modes are added.** Each $`\Delta_k`$ is small, so the effect of one on another is small
  twice over and is left out.
- **Cost and limits.** Two SCFs for each stiff mode. $`\Delta F`$ is made once for each fit with a
  given density matrix, and held fixed while the parameters shift. It is for a single molecule,
  with the harmonic amplitudes (M6). The rigid-body motion and the refined soft modes are averaged
  separately, in §7.

## 7. The rigid-body motion averaged numerically

The average of §6 can be applied to the rigid-body motion and the refined soft modes as well
(`tc_integrate_rigid_body=`). The motion is first written in coordinates that are uncorrelated,
as the stiff modes are; then each is averaged by (27).

**Uncorrelated coordinates.** $`\Sigma`$ is symmetric, so it has orthonormal eigenvectors
$`\mathbf{e}_j`$ with eigenvalues $`\lambda_j`$, $`\Sigma\,\mathbf{e}_j = \lambda_j \mathbf{e}_j`$. Write the generalised coordinates
$`\mathbf{v}`$ of (M11) in terms of them, $`\mathbf{v} = \sum_j c_j \mathbf{e}_j`$ with $`c_j = \mathbf{e}_j^{\mathsf T}\mathbf{v}`$. The $`c_j`$ are the
**principal coordinates** of the motion:

```math
\begin{aligned}
\langle c_i c_j \rangle &= \mathbf{e}_i^{\mathsf T}\, \langle \mathbf{v}\mathbf{v}^{\mathsf T} \rangle\, \mathbf{e}_j && \text{since } c_j = \mathbf{e}_j^{\mathsf T}\mathbf{v} \\
 &= \mathbf{e}_i^{\mathsf T}\, \Sigma\, \mathbf{e}_j && \Sigma = \langle \mathbf{v}\mathbf{v}^{\mathsf T}\rangle \\
 &= \lambda_j\, \mathbf{e}_i^{\mathsf T} \mathbf{e}_j && \Sigma\,\mathbf{e}_j = \lambda_j \mathbf{e}_j \\
 &= \lambda_j\, \delta_{ij} && \text{the eigenvectors are orthonormal.}
\end{aligned}
\qquad (29)
```

So different principal coordinates are uncorrelated, and $`c_j`$ has mean square $`\lambda_j`$. The
displacement of atom $`A`$, to first order, is

```math
\mathbf{u}_A = B_A \mathbf{v} = \sum_j c_j\, B_A \mathbf{e}_j = \sum_j c_j\, \mathbf{d}_{Aj}, \qquad \mathbf{d}_{Aj} = B_A \mathbf{e}_j , \qquad (30)
```

which has the form of the normal modes of §M1.5: $`\mathbf{d}_{Aj}`$ is the displacement of atom $`A`$ per unit
of $`c_j`$. In (2) the rigid-body term is then
$`B_A \Sigma B_B^{\mathsf T} = \sum_j \lambda_j\, \mathbf{d}_{Aj}\mathbf{d}_{Bj}^{\mathsf T}`$, the same form as the stiff-mode term. So §6
applies to each principal coordinate as it stands, with $`\mathbf{d}_{Aj}`$ for $`\mathbf{d}_{Ak}`$, $`\lambda_j`$ for
$`\langle Q_k^2\rangle`$, and $`s_j = \sqrt{3\lambda_j}`$.

**The displaced geometry is a true turn.** One thing is different. An eigenvector has a
translation part, a rotation part and a soft-mode part,
$`\mathbf{e}_j = (\boldsymbol{\tau}_j, \boldsymbol{\omega}_j, q_{j1} \dots q_{jK})`$. At the value $`c`$ of the coordinate the molecule has
been turned through the angle $`|c\,\boldsymbol{\omega}_j|`$ about the axis along $`\boldsymbol{\omega}_j`$ through the origin $`\mathbf{O}`$
of §M1.4. (30) replaces that turn by its first-order displacement $`c\,\boldsymbol{\omega}_j \times (\mathbf{R}_A - \mathbf{O})`$,
(M4). For $`F^{\rm exact}_j`$ the atoms are put where the turn really takes them:

```math
\mathbf{R}_A(c) = \mathbf{O} + \mathsf{R}(c\,\boldsymbol{\omega}_j)\,(\mathbf{R}_A - \mathbf{O}) + c\,\boldsymbol{\tau}_j + c \sum_{k=1}^{K} q_{jk}\, \mathbf{d}_{Ak} , \qquad (31)
```

where $`\mathsf{R}(\boldsymbol{\theta})`$ is the rotation by the angle $`\theta = |\boldsymbol{\theta}|`$ about the unit vector
$`\mathbf{n} = \boldsymbol{\theta}/\theta`$, which takes a vector $`\mathbf{r}`$ to
$`\mathbf{r}\cos\theta + (\mathbf{n}\times\mathbf{r})\sin\theta + \mathbf{n}\,(\mathbf{n}\cdot\mathbf{r})(1-\cos\theta)`$. For a small angle (31) is
$`\mathbf{R}_A + c\,\mathbf{d}_{Aj}`$.

**The correction.** With the atoms at $`\mathbf{R}_A(\pm s_j)`$, an SCF there, and the structure factor
made from (14) with $`W^{(j)}`$ of (24) as the temperature factor, $`F_j^{+}`$ and $`F_j^{-}`$ are found as
in §6. $`F_j^{\cos}`$ is (27)'s, with $`\mathbf{d}_{Pj} = w_A \mathbf{d}_{Aj} + w_B \mathbf{d}_{Bj}`$: it moves each product's
centre in a straight line and turns nothing. Then

```math
\Delta F^{\rm rb} = \sum_j \Big[ \tfrac16 \big( F_j^{+} + F_j^{-} \big) - \tfrac13\, F_j^{\cos} \Big] , \qquad (32)
```

summed over the principal coordinates, and the structure factor used in the refinement is (14)
plus $`\Delta F`$ of (28) plus $`\Delta F^{\rm rb}`$.

- **What the difference holds.** When the molecule turns, the wavefunction turns with it, so a
  product of basis functions turns about its own centre, and its centre moves on an arc.
  $`F_j^{\cos}`$ has neither. For a product that is a round Gaussian, turning about its centre
  changes nothing; for one with lobes, such as a p function on nitrogen pointing at a hydrogen,
  it carries the lobe through a larger arc than the centre.
- **A pure translation gives nothing.** If $`\boldsymbol{\omega}_j = 0`$ and there are no soft modes, moving the
  molecule changes only the phases, which $`F_j^{\cos}`$ has exactly; $`F_j^{+} + F_j^{-} = 2F_j^{\cos}`$.
- **It follows the refinement.** $`\Sigma`$ is refined, so $`\Delta F^{\rm rb}`$ is made at the start of each
  fit from the $`\Sigma`$ of that moment and held fixed during it. The stiff-mode $`\Delta F`$ does not
  change in this way, because its amplitudes come from the Hessian.
- **Cost.** Two SCFs for each principal coordinate, twelve for a rigid body without soft modes.
  The SCF is done for convenience: turning a molecule turns its density matrix and no more.

## 8. Keywords

In `xray_data=`:

| keyword | default | meaning |
|---|---|---|
| `partition_model= tc-tls` | | the model (14); sets `adp_model= tls` |
| `tc_position_derivs=` | `center` | the position derivative: `center` (22), `fixed-dm` or `scf` (§5) |
| `tc_integrate_stiff_modes=` | `FALSE` | `TRUE` adds $`\Delta F`$ of (28) |
| `tc_integrate_rigid_body=` | `FALSE` | `TRUE` adds $`\Delta F^{\rm rb}`$ of (32) |

On the molecule:

| keyword | meaning |
|---|---|
| `check_two_center_derivs` | compares (21) and (22) with central differences of (14) and prints the largest disagreement; for (22) that is the size of what `center` leaves out |

A Hessian from ORCA is turned into `force_constants=` by `scripts/orca_hessian_to_tonto.py`.

## 9. Where it is implemented

| piece | procedure |
|---|---|
| the stiff modes, $`\mathbf{d}_{Ak}`$ and $`\langle Q_k^2\rangle`$ | `MOLECULE.PROP:make_stiff_modes` |
| the stiff-mode part of (2) for every atom pair | `MOLECULE.PROP:make_internal_ADPs`, `CRYSTAL:set_fragment_U_high_pair` |
| $`B_A`$ of (1) | `CRYSTAL:TLS_displacement_mx` |
| $`\Sigma`$ from the atoms' ADPs | `CRYSTAL:make_TLS_sigma_vector` |
| $`U_{AB}`$, (2), for every atom pair | `CRYSTAL:make_two_center_ADPs` |
| the temperature factor of (14) with (13), per pair of primitives | `SHELL2:make_TLS_TFs` |
| the temperature factors of a shell pair, any two-centre model | `MOLECULE.XTAL:make_pair_TFs` |
| $`s_0`$, $`s_1`$, $`s_2`$ of (20) for a shell pair | `MOLECULE.XTAL:make_FT_pair_moments` |
| $`F`$ with (21) and the sums of (22) | `MOLECULE.XTAL:make_FT_and_TLS_derivs` |
| the rotation between an atom and its image | `CRYSTAL:image_rotation` |
| the shifts from those derivatives | `CRYSTAL:get_two_center_parameter_shifts` |
| derivatives taken directly with respect to refined parameters | `DIFFRACTION_DATA.SET:solve_normal_equations` |
| position derivatives by differences, `fixed-dm` and `scf` | `MOLECULE.HAR:make_two_center_position_derivs` |
| the cosine factor in (27) | `SHELL2:scale_TFs_by_shift` |
| the average of one uncorrelated coordinate, (27) | `MOLECULE.HAR:make_mode_SF_correction` |
| $`\Delta F`$, (28) | `MOLECULE.HAR:make_stiff_mode_SF_correction` |
| the principal coordinates of $`\Sigma`$, (29) and (30) | `CRYSTAL:make_TLS_principal_motions` |
| $`\Delta F^{\rm rb}`$, (32), with the turn (31) | `MOLECULE.HAR:make_rigid_body_SF_correction` |
| the fit loop | `MOLECULE.HAR:LS_fit_two_center` |
| the parts of a fit shared with Hirshfeld atom refinement | `CRYSTAL:start_structure_fit`, `apply_parameter_shifts`, `finish_fit_cycle`, `finish_structure_fit` |
| an SCF made ready at a new geometry | `MOLECULE.HAR:prepare_SCF_at_new_geometry` |
| the check of the derivatives | `MOLECULE.HAR:check_two_center_derivs` |


## 10. Results: urea

`partition_model= tc-tls` against Hirshfeld atoms with the same TLS model for
the ADPs. RHF/def2-SVP on the isolated molecule; 817 reflections at 123 K; 17 parameters; stiff
modes from an RHF/def2-SVP Hessian, frequencies scaled by 0.8953; no soft modes refined.

| refinement | GoF | R(F) | N–H1 /Å | N–H3 /Å |
|---|---|---|---|---|
| Hirshfeld atoms | 3.4854 | 0.0181 | 1.028 | 0.994 |
| `tc-tls`, position derivative `center` | 3.6577 | 0.0186 | 1.024 | 0.977 |
| `tc-tls`, position derivative `fixed-dm` | 3.6681 | 0.0183 | 1.023 | 1.013 |
| `tc-tls`, position derivative `scf` | 3.6292 | 0.0183 | 1.023 | 0.994 |
| `tc-tls`, `center`, stiff modes averaged (§6) | 3.6349 | 0.0186 | 1.024 | 0.980 |
| `tc-tls`, `scf`, stiff modes averaged (§6) | 3.6115 | 0.0183 | 1.023 | 0.995 |

The three with `scf` or the averaging have the SCF converged to 1e-7, the others to 1e-4.

- **Hirshfeld atoms fit best.** The best `tc-tls` refinement is 0.126 higher in GoF.
- **The position derivative matters, and only `scf` is right.** With `tc-tls` and the TLS
  parameters alone refined at the Hirshfeld geometry the GoF is 3.6377. `center` and `fixed-dm` end
  above that, `scf` below. `fixed-dm`, which moves the basis functions and holds the density
  matrix, is worse than `center`, which moves neither.
- **N–H3 follows the position derivative**: 0.977, 1.013 and 0.994 Å. With `scf` it is the
  Hirshfeld value. N–H1 is the same in all.
- **Averaging the stiff modes numerically lowers the GoF by about 0.02**, with either derivative.
- **The correlation between atoms is a small effect.** At the Hirshfeld parameters, and with the
  RHF/6-31G(d) Hessian of the earlier sections, `tc-tls` gives GoF 3.6461 and `tc-tanaka`, which
  leaves the correlation out, 3.6561.
- **The check of the derivatives** (`check_two_center_derivs`): those with respect to $`\Sigma`$
  agree with central differences to 2 parts in 10⁸; what `center` leaves out of the position
  derivatives is 13 % of the largest.
- **Time**, one core: 1 minute for `center`, 9 for `scf`, 10 for the averaging with `center`, 19
  with `scf`.


## References

- V. Schomaker and K. N. Trueblood, *Acta Cryst.* B24, 63 (1968): rigid-body motion, T, L, S.
- R. F. Stewart, *J. Chem. Phys.* 51, 4569 (1969), and K. Tanaka, *Acta Cryst.* A44, 1002 (1988):
  two-centre temperature factors.
- D. Jayatilaka and D. J. Grimwood, *Acta Cryst.* A57, 76 (2001): the two-centre models in Tonto.
- J. R. Michael and T. Koritsanszky, *J. Math. Chem.* 53, 250 (2015): the ADP of a product of two
  Gaussians from the full covariance of the nuclear displacements, tested against an average
  over static densities.
