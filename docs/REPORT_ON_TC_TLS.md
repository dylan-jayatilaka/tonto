# Smearing the density between two atoms: `tc-tls`

Hirshfeld atom refinement cuts the molecular density into atoms and smears each atom with its
own ADP. The cut is an approximation. The density is a sum over pairs of basis functions, and a
product of functions on two different atoms sits between them and moves with both. To smear it
one must know how the two atoms move together, which independent ADPs do not say. The model of
rigid-body motion and internal modes in `docs/REPORT_ON_MODE_FITTING.md` does say it, so with
that model no cut is needed. This page gives the equations, the keywords, where the code is, and
what it gives on urea.

That report is the starting point. Equations of that report are cited here as (M1), (M2), and so
on, and sections of that report as §M1, §M1.4, and so on. One thing is written differently: the
atom a vector or matrix belongs to is a superscript here, $`U^A`$ and $`\mathbf{u}^A`$ where that report
writes $`U_i`$ and $`\mathbf{u}_i`$, so that subscripts are free for components and for the labels of modes
and primitives. Units: atomic units; vectors
are columns; † is the transpose (every vector and matrix here is real), where that report writes
ᵀ; ⟨ ⟩ is the thermal average.


## 1. The covariance between two atoms

Collect every generalised coordinate in one vector: the rigid-body motion and refined soft modes
of (M11), and the amplitudes $`Q_k`$ of the stiff modes. For atom $`A`$,

```math
\mathbf{u}^A = \bar B^A\, \bar{\mathbf{v}}, \qquad
\bar{\mathbf{v}} = (\mathbf{t}, \boldsymbol{\lambda}, Q_1, \dots, Q_K, Q_{\rm stiff}), \qquad
\bar B^A = [\,B^A \mid \mathbf{d}^A_{\rm stiff}\,] , \qquad (1)
```

with $`B^A`$ the matrix of (M11) and one column $`\mathbf{d}^A_k`$ (§M1.7) for each stiff mode. The covariance
of $`\bar{\mathbf{v}}`$ has two blocks: the refined $`\Sigma`$ of (M11), and a diagonal block holding the harmonic
amplitude (M6) of each stiff mode at the temperature of the data,

```math
\bar\Sigma = \langle \bar{\mathbf{v}}\bar{\mathbf{v}}^{\dagger}\rangle = \begin{pmatrix} \Sigma & 0 \\ 0 & \mathrm{diag}\,\langle Q_k^2\rangle \end{pmatrix} .
```

The zeros say that the stiff modes are taken as uncorrelated with the rigid-body motion and the
soft modes, and with each other. The **two-centre ADP** of atoms
$`A`$ and $`B`$ is then

```math
U^{AB} = \langle \mathbf{u}^A \mathbf{u}^{B\dagger} \rangle
       = B^A\, \Sigma\, B^{B\dagger} + \sum_{\omega_k > \omega_c} \langle Q_k^2\rangle\, \mathbf{d}^A_k \mathbf{d}^{B\dagger}_k . \qquad (2)
```

The block $`U^{AA}`$ is the ADP of atom $`A`$, (M11), written $`U^A`$. The matrix $`U^{AB}`$ is not
symmetric, and $`U^{BA} = U^{AB\dagger}`$. The two-centre ADPs need no parameter beyond those already
in the rigid-body and mode model.

## 2. Gaussian basis functions and products of two of them

**The density.** The electron density is a sum over pairs of basis functions,

```math
\rho(\mathbf{r}) = \sum_{\mu\nu} D_{\mu\nu}\, G_\mu(\mathbf{r} - \mathbf{R}^A)\, G_\nu(\mathbf{r} - \mathbf{R}^B) . \qquad (3)
```

Here $`G_\mu`$ is a basis function centred on atom $`A`$, whose nucleus is at $`\mathbf{R}^A`$, and $`G_\nu`$ is a
basis function centred on atom $`B`$, at $`\mathbf{R}^B`$. $`D`$ is the **density matrix** from the SCF: the
element $`D_{\mu\nu}`$ says how much of the product $`G_\mu G_\nu`$ is in the density.

**Contracted and primitive Gaussians.** Each basis function $`G_\mu`$ is a **contracted Gaussian**:
a fixed sum of **primitive Gaussians** $`g^\mu_a`$, labelled by $`a`$, with contraction coefficients
$`c^\mu_a`$, which include the normalisation. A primitive Gaussian is a polynomial times a Gaussian
with exponent $`\alpha^\mu_a`$. With the position written by its components, $`\mathbf{r} = (r_1, r_2, r_3)`$,

```math
G_\mu(\mathbf{r}) = \sum_a c^\mu_a\, g^\mu_a(\mathbf{r}), \qquad
g^\mu_a(\mathbf{r}) = \mathbf{r}^{\mathbf{l}^\mu}\, e^{-\alpha^\mu_a |\mathbf{r}|^2}, \qquad
\mathbf{r}^{\mathbf{l}^\mu} = r_1^{\,l^\mu_1}\, r_2^{\,l^\mu_2}\, r_3^{\,l^\mu_3} . \qquad (4)
```

The three whole numbers $`\mathbf{l}^\mu = (l^\mu_1, l^\mu_2, l^\mu_3)`$ are the powers of the polynomial part.
They are the same for every primitive of $`G_\mu`$, and they make the basis function s, p, d and so
on. The primitives of $`G_\nu`$ are $`g^\nu_b`$, labelled by $`b`$, with coefficients $`c^\nu_b`$, exponents
$`\beta^\nu_b`$ and powers $`\mathbf{l}^\nu`$.

**The Gaussian product.** Write $`g^{\mu\nu}_{ab}`$ for the product of a primitive of $`G_\mu`$ on atom
$`A`$ with a primitive of $`G_\nu`$ on atom $`B`$, each centred on its own atom. Putting (4) into (3)
writes the density as a sum of these Gaussian products:

```math
g^{\mu\nu}_{ab}(\mathbf{r}) = g^\mu_a(\mathbf{r} - \mathbf{R}^A)\, g^\nu_b(\mathbf{r} - \mathbf{R}^B), \qquad
\rho(\mathbf{r}) = \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} c^\mu_a c^\nu_b\; g^{\mu\nu}_{ab}(\mathbf{r}) . \qquad (5)
```

Everything below is done for one Gaussian product $`g^{\mu\nu}_{ab}`$, and then summed as in (5).

**Quantities that belong to one Gaussian product.** For the Gaussian product $`g^{\mu\nu}_{ab}`$, write
$`\alpha`$ for the exponent $`\alpha^\mu_a`$ of its primitive on atom $`A`$ and $`\beta`$ for the exponent
$`\beta^\nu_b`$ of its primitive on atom $`B`$. Four numbers follow from the two exponents,

```math
\gamma = \alpha + \beta, \qquad
w_A = \frac{\alpha}{\gamma}, \qquad w_B = \frac{\beta}{\gamma}, \qquad
\xi = \frac{\alpha\beta}{\gamma} , \qquad (6)
```

and two vectors follow from the positions of the two atoms,

```math
\mathbf{R} = \mathbf{R}^A - \mathbf{R}^B, \qquad \mathbf{P} = w_A \mathbf{R}^A + w_B \mathbf{R}^B . \qquad (7)
```

$`\mathbf{R}`$ is the vector from atom $`B`$ to atom $`A`$. $`\mathbf{P}`$ is the **centre of the Gaussian product**:
because $`w_A + w_B = 1`$, the point $`\mathbf{P}`$ lies on the line between the two atoms, nearer the atom
whose primitive has the larger exponent. The numbers $`\alpha`$, $`\beta`$, $`\gamma`$, $`w_A`$, $`w_B`$, $`\xi`$ and
the point $`\mathbf{P}`$ are different for each Gaussian product; the labels $`\mu, \nu, a, b`$ are left
off them to keep the formulas readable.

**The Gaussian product written about its own centre.** Measure the position from the centre,
$`\mathbf{s} = \mathbf{r} - \mathbf{P}`$. Then the Gaussian product is a function of $`\mathbf{s}`$ and of the vector $`\mathbf{R}`$
between the atoms, and of nothing else:

```math
g^{\mu\nu}_{ab}(\mathbf{r}) = \bar g^{\mu\nu}_{ab}(\mathbf{r} - \mathbf{P};\, \mathbf{R}), \qquad
\bar g^{\mu\nu}_{ab}(\mathbf{s}; \mathbf{R}) = (\mathbf{s} - w_B \mathbf{R})^{\mathbf{l}^\mu}\; (\mathbf{s} + w_A \mathbf{R})^{\mathbf{l}^\nu}\; e^{-\xi |\mathbf{R}|^2}\, e^{-\gamma |\mathbf{s}|^2} . \qquad (8)
```

Equation (8) is the **Gaussian product theorem**. Equation (8) holds for two reasons. In the exponents of the
two primitives, completing the square gives

```math
\alpha\, |\mathbf{r} - \mathbf{R}^A|^2 + \beta\, |\mathbf{r} - \mathbf{R}^B|^2
   = \gamma\, |\mathbf{r} - \mathbf{P}|^2 + \xi\, |\mathbf{R}|^2 = \gamma\, |\mathbf{s}|^2 + \xi\, |\mathbf{R}|^2 , \qquad (9)
```

which gives the two exponentials of (8). And in the polynomial parts, by (7),
$`\mathbf{r} - \mathbf{R}^A = \mathbf{s} - w_B \mathbf{R}`$ and $`\mathbf{r} - \mathbf{R}^B = \mathbf{s} + w_A \mathbf{R}`$, which gives the two powers of (8).

Equation (8) separates two things. The centre $`\mathbf{P}`$ says where the Gaussian product is. The
vector $`\mathbf{R}`$ between the atoms sets how large the Gaussian product is, through the factor
$`e^{-\xi|\mathbf{R}|^2}`$, and enters the two polynomial parts.

**Fourier transform of the Gaussian product.** At scattering vector $`\mathbf{k}`$, changing the variable
of integration from $`\mathbf{r}`$ to $`\mathbf{s} = \mathbf{r} - \mathbf{P}`$ in (8),

```math
\int g^{\mu\nu}_{ab}(\mathbf{r})\, e^{i\mathbf{k}\cdot\mathbf{r}}\, d\mathbf{r}
   = e^{i\mathbf{k}\cdot\mathbf{P}}\; \tilde g^{\mu\nu}_{ab}(\mathbf{k}; \mathbf{R}), \qquad
\tilde g^{\mu\nu}_{ab}(\mathbf{k}; \mathbf{R}) = \int \bar g^{\mu\nu}_{ab}(\mathbf{s}; \mathbf{R})\, e^{i\mathbf{k}\cdot\mathbf{s}}\, d\mathbf{s} . \qquad (10)
```

The centre $`\mathbf{P}`$ appears only in the phase $`e^{i\mathbf{k}\cdot\mathbf{P}}`$; the transform
$`\tilde g^{\mu\nu}_{ab}`$ depends on the two atoms only through $`\mathbf{R}`$. Tonto evaluates the transform by
the method of McMurchie and Davidson (1978). Along each axis the two polynomial parts of (8)
times the Gaussian are written as a sum of derivatives of the Gaussian $`e^{-\gamma s^2}`$ with
respect to its centre, with coefficients $`E_t`$ that follow from $`\mathbf{R}`$ and the exponents by
recursion. The Fourier transform of the $`t`$-th derivative is the transform of the Gaussian
times $`(ik)^t`$, so each axis gives a polynomial in the component of $`\mathbf{k}`$ along that axis, and
the three axes multiply. For two s functions, for example,
$`\tilde g^{\mu\nu}_{ab} = (\pi/\gamma)^{3/2}\, e^{-\xi|\mathbf{R}|^2}\, e^{-|\mathbf{k}|^2/4\gamma}`$. With (5), the structure
factor of the molecule with its atoms at rest is
$`\sum_{\mu\nu} D_{\mu\nu} \sum_{ab} c^\mu_a c^\nu_b\, e^{i\mathbf{k}\cdot\mathbf{P}}\, \tilde g^{\mu\nu}_{ab}(\mathbf{k}; \mathbf{R})`$.

**The Gaussian product when its two atoms are displaced.** Now let atom $`A`$ be displaced from
$`\mathbf{R}^A`$ by $`\mathbf{u}^A`$ and atom $`B`$ from $`\mathbf{R}^B`$ by $`\mathbf{u}^B`$, each primitive Gaussian moving with the
atom on which it is centred. In (7), the centre $`\mathbf{P}`$ becomes $`\mathbf{P} + \mathbf{u}^P`$ and the vector
$`\mathbf{R}`$ becomes $`\mathbf{R} + \mathbf{u}^A - \mathbf{u}^B`$, where

```math
\mathbf{u}^P = w_A \mathbf{u}^A + w_B \mathbf{u}^B \qquad (11)
```

is the displacement of the centre of the Gaussian product. Putting the displaced centre and the
displaced vector into (10), the Fourier transform of the displaced Gaussian product is exactly

```math
e^{i\mathbf{k}\cdot\mathbf{P}}\; e^{i\mathbf{k}\cdot\mathbf{u}^P}\; \tilde g^{\mu\nu}_{ab}(\mathbf{k};\, \mathbf{R} + \mathbf{u}^A - \mathbf{u}^B) . \qquad (12)
```

Displacing the two atoms therefore does two things to the Gaussian product. The centre moves by
$`\mathbf{u}^P`$, which multiplies the Fourier transform by the phase $`e^{i\mathbf{k}\cdot\mathbf{u}^P}`$. And the vector
between the atoms changes by the relative displacement $`\mathbf{u}^A - \mathbf{u}^B`$, which changes the
transform $`\tilde g^{\mu\nu}_{ab}`$ itself: by (8), that vector sets how large the Gaussian product is
and enters the two polynomial parts.

## 3. The TLS models

**Each Gaussian product is smeared by the motion of its own centre.** In the thermal average of
(12) the factor $`e^{i\mathbf{k}\cdot\mathbf{u}^P}`$ is averaged. Two other things in a term of (5) change when the
atoms move, and both are held at their values at the mean geometry and taken out of the average:
the transform $`\tilde g^{\mu\nu}_{ab}`$, whose argument $`\mathbf{R} + \mathbf{u}^A - \mathbf{u}^B`$ in (12) is replaced by
$`\mathbf{R}`$, and the density matrix element $`D_{\mu\nu}`$. Holding these two fixed is the rigid-atom
approximation of Hirshfeld atom refinement, made for each Gaussian product instead of each atom.

The average of the phase is done as for an atom (§M1.1). The displacements of the atoms have a
Gaussian distribution with zero mean, so the number $`x = \mathbf{k}\cdot\mathbf{u}^P`$ is a Gaussian variable
with zero mean, and for such a variable $`\langle e^{ix}\rangle = e^{-\langle x^2\rangle/2}`$. The mean square
of $`x`$ is
$`\langle x^2\rangle = \mathbf{k}^{\dagger} \langle \mathbf{u}^P \mathbf{u}^{P\dagger}\rangle\, \mathbf{k}`$, so

```math
\langle e^{i\mathbf{k}\cdot\mathbf{u}^P} \rangle = e^{-\frac12 \mathbf{k}^{\dagger} U^P\, \mathbf{k}}, \qquad
U^P = \langle \mathbf{u}^P \mathbf{u}^{P\dagger} \rangle . \qquad (13)
```

Putting (11) into $`\langle \mathbf{u}^P \mathbf{u}^{P\dagger}\rangle`$ and multiplying out, with $`U^A = \langle\mathbf{u}^A\mathbf{u}^{A\dagger}\rangle`$
and $`U^{AB} = \langle\mathbf{u}^A\mathbf{u}^{B\dagger}\rangle`$ of (2),

```math
U^P = w_A^2\, U^A + w_B^2\, U^B + w_A w_B\,(U^{AB} + U^{AB\dagger}) . \qquad (14)
```

$`U^P`$ is the **ADP of the Gaussian product**: the mean-square displacement of its centre. $`U^P`$
is different for each Gaussian product $`g^{\mu\nu}_{ab}`$, through $`w_A`$ and $`w_B`$; like $`\mathbf{P}`$, it
is written without the labels $`\mu, \nu, a, b`$. Equation (14) is equation (10) of
Michael and Koritsanszky (2015), who give it as the exact ADP of a product of two primitive
Gaussians when the covariance of the nuclear displacements is known. They took that covariance
from the internal normal modes of an isolated molecule, with the coth amplitudes (M6), and
smeared the density itself; here the covariance comes from (2), with the rigid-body motion and
the soft modes refined against the data. The structure factor is then

```math
\begin{aligned}
F(\mathbf{k}) &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} c^\mu_a c^\nu_b\;
     e^{i\mathbf{k}\cdot\mathbf{P}}\; \tilde g^{\mu\nu}_{ab}(\mathbf{k}; \mathbf{R})\; e^{-\frac12 \mathbf{k}^{\dagger} U^P \mathbf{k}} \\
   &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} t^{\mu\nu}_{ab}(\mathbf{k}), \qquad
   t^{\mu\nu}_{ab}(\mathbf{k}) = c^\mu_a c^\nu_b\;
     e^{i\mathbf{k}\cdot\mathbf{P}}\; \tilde g^{\mu\nu}_{ab}(\mathbf{k}; \mathbf{R})\; e^{-\frac12 \mathbf{k}^{\dagger} U^P \mathbf{k}} .
\end{aligned}
\qquad (15)
```

The second line names one term, $`t^{\mu\nu}_{ab}`$, for use below. No partition into atoms appears
in (15). When (15) is made from one molecule and summed over the space-group operations, each
pair of atoms has a weight that counts it once; that is in `docs/REPORT_ON_PAIR_STABILIZERS.md`.

**The centre is a pseudo-atom.** Putting (1) into (11), and using $`w_A + w_B = 1`$,

```math
\mathbf{u}^P = \bar B^P\, \bar{\mathbf{v}}, \qquad \bar B^P = w_A \bar B^A + w_B \bar B^B , \qquad
U^P = \bar B^P\, \bar\Sigma\, \bar B^{P\dagger} , \qquad (16)
```

with $`\bar\Sigma = \langle\bar{\mathbf{v}}\bar{\mathbf{v}}^{\dagger}\rangle`$. Equation (16) is (M11) for a fictitious atom at
$`\mathbf{P}`$. The rigid-body part of $`U^P`$ is the Schomaker–Trueblood ADP (M5) of the point $`\mathbf{P}`$,
because the matrix $`A_i`$ of (M4) is linear in the position, so $`w_A A^A + w_B A^B`$ is the matrix
$`A`$ of the point $`\mathbf{P}`$. The mode part of $`U^P`$ uses each mode's displacement interpolated to
$`\mathbf{P}`$, $`\mathbf{d}^P_k = w_A \mathbf{d}^A_k + w_B \mathbf{d}^B_k`$, for the
stiff modes with the amplitude (M6) and for the soft modes with the refined one. Three things
follow:

- The refined parameters are those of §M1: $`\Sigma`$ and the positions. None is added.
- For two functions on the same atom, $`\mathbf{P} = \mathbf{R}^A`$ and $`U^P = U^A`$.
- For functions on atoms of two different molecules the rigid-body and mode model gives no
  correlation, and
  $`U^{AB} = 0`$ is used.

**Relation to earlier two-centre models.** Write
$`V^{AB} = \langle (\mathbf{u}^A - \mathbf{u}^B)(\mathbf{u}^A - \mathbf{u}^B)^{\dagger}\rangle = U^A + U^B - U^{AB} - U^{AB\dagger}`$ for the
mean-square relative displacement of the two atoms. Since $`w_A^2 = w_A - w_A w_B`$ and
$`w_B^2 = w_B - w_A w_B`$, (14) is

```math
U^P = w_A U^A + w_B U^B - w_A w_B\, V^{AB} . \qquad (17)
```

The first two terms are Tanaka's model (`tc-tanaka`). Tanaka's model is exact when the two atoms
have no relative motion, and otherwise smears the Gaussian product too much: a bond stretch moves the two atoms in
opposite directions and leaves the density between them nearly still. Stewart's model
(`tc-stewart`) puts every Gaussian product at the midpoint, and its two cases are $`V^{AB} = 0`$ for bonded
atoms and $`U^{AB} = 0`$ for the rest. Those models cannot give $`U^{AB}`$ from $`U^A`$ and $`U^B`$; here it
comes from (2).

**How much the correlation matters, and how good the approximation is.** Michael and Koritsanszky (2015)
tested both on formamide at 23 K, internal modes only. Leaving out $`U^{AB}`$ changed the smeared
density by at most 0.015 e/Å³, at the nitrogen nucleus. Against the average of 500 000 static
densities, each calculated at a geometry drawn from the distribution of the normal modes, the
smeared density (14) gave nearly the same values at the bond critical points. That average is
the quantity §6 estimates with two geometries for each stiff mode.

## 4. What the approximation of §3 leaves out

Let $`\mathbf{x}`$ be the $`3N`$ displacements of all the atoms, and
$`f(\mathbf{x}) = D_{\mu\nu}\, \tilde g^{\mu\nu}_{ab}(\mathbf{k};\, \mathbf{R} + \mathbf{u}^A - \mathbf{u}^B)`$ the part of one term of (5)
that §3 holds fixed, as a function of those displacements; $`D_{\mu\nu}`$ depends on $`\mathbf{x}`$ too. The exact
average of that term is

```math
\langle f(\mathbf{x})\, e^{i\mathbf{k}\cdot\mathbf{u}^P} \rangle
   = e^{-\frac12 \mathbf{k}^{\dagger} U^P \mathbf{k}}\; \langle f(\mathbf{x} + i\,U\mathbf{c}) \rangle , \qquad (18)
```

where $`U`$ is the $`3N\times 3N`$ matrix with blocks $`U^{AB}`$, and $`\mathbf{c}`$ is the $`3N`$-vector holding
$`w_A\mathbf{k}`$ on atom $`A`$, $`w_B\mathbf{k}`$ on atom $`B`$ and zero elsewhere, so that $`\mathbf{k}\cdot\mathbf{u}^P = \mathbf{c}\cdot\mathbf{x}`$.
Equation (18) follows from completing the square in the Gaussian average. The approximation of §3
replaces the last factor by $`f(0)`$. What that leaves out is:

- the change of $`\tilde g^{\mu\nu}_{ab}`$ with the relative displacement $`\mathbf{u}^A - \mathbf{u}^B`$;
- the change of the density matrix with the geometry;
- the turning of each Gaussian product about its own centre when the molecule librates.

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
structure factor (15) with respect to each element $`\Sigma_{pq}`$ and each position $`\mathbf{R}^C`$. Both are
found by differentiating (15) term by term.

**With respect to $`\Sigma`$.** Two facts are used. First, by (16) and the blocks of §1,

```math
U^P = \bar B^P\, \bar\Sigma\, \bar B^{P\dagger}
   = B^P\, \Sigma\, B^{P\dagger} + \sum_{\omega_k > \omega_c} \langle Q_k^2\rangle\, \mathbf{d}^P_k \mathbf{d}^{P\dagger}_k, \qquad
B^P = w_A B^A + w_B B^B , \qquad (19)
```

of which only the first term contains $`\Sigma`$. Second, $`\Sigma`$ is symmetric: its entries
$`\Sigma_{pq}`$ and $`\Sigma_{qp}`$ are equal, so for $`p \ne q`$ the two are one independent parameter, not
two. That parameter is called $`\Sigma_{pq}`$ with $`p \le q`$, and changing it changes both entries of
the matrix together. Then, differentiating (15),

```math
\begin{aligned}
\frac{\partial F}{\partial \Sigma_{pq}}
 &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} c^\mu_a c^\nu_b\; e^{i\mathbf{k}\cdot\mathbf{P}}\; \tilde g^{\mu\nu}_{ab}(\mathbf{k}; \mathbf{R})\;
    \frac{\partial}{\partial \Sigma_{pq}} \Big( e^{-\frac12 \mathbf{k}^{\dagger} U^P \mathbf{k}} \Big) \\
 &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab}
    \underbrace{ c^\mu_a c^\nu_b\; e^{i\mathbf{k}\cdot\mathbf{P}}\; \tilde g^{\mu\nu}_{ab}(\mathbf{k}; \mathbf{R})\; e^{-\frac12 \mathbf{k}^{\dagger} U^P \mathbf{k}} }_{t^{\mu\nu}_{ab}\ \text{of (15)}}
    \Big( -\tfrac12\, \frac{\partial}{\partial \Sigma_{pq}} \big( \mathbf{k}^{\dagger} U^P\, \mathbf{k} \big) \Big) \\
 &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} t^{\mu\nu}_{ab}
    \Big( -\tfrac12\, \frac{\partial}{\partial \Sigma_{pq}} \big( \mathbf{k}^{\dagger} B^P\, \Sigma\, B^{P\dagger} \mathbf{k} \big) \Big)
    && \text{by (19)} \\
 &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} t^{\mu\nu}_{ab}
    \Big( -\tfrac12\, \frac{\partial}{\partial \Sigma_{pq}} \big( \mathbf{h}^{P\dagger}\, \Sigma\, \mathbf{h}^P \big) \Big)
    && \text{with } \mathbf{h}^P = B^{P\dagger}\mathbf{k} \\
 &= -\frac{2-\delta_{pq}}{2} \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} t^{\mu\nu}_{ab}\; h^P_p\, h^P_q
    && \text{since } \frac{\partial ( \mathbf{h}^{\dagger} \Sigma\, \mathbf{h} )}{\partial \Sigma_{pq}} = (2-\delta_{pq})\, h_p h_q \\
 &= -\frac{2-\delta_{pq}}{2} \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} t^{\mu\nu}_{ab}\;
    ( w_A h^A_p + w_B h^B_p )( w_A h^A_q + w_B h^B_q )
    && \text{with } \mathbf{h}^P = w_A \mathbf{h}^A + w_B \mathbf{h}^B,\ \ \mathbf{h}^A = B^{A\dagger}\mathbf{k} \\
 &= -\frac{2-\delta_{pq}}{2} \sum_{\mu\nu} \Big[\,
      h^A_p h^A_q \Big( D_{\mu\nu} \sum_{ab} w_A^2\; t^{\mu\nu}_{ab} \Big)
    + h^B_p h^B_q \Big( D_{\mu\nu} \sum_{ab} (1-w_A)^2\; t^{\mu\nu}_{ab} \Big) \\
 &\qquad\qquad\qquad\quad
    + ( h^A_p h^B_q + h^B_p h^A_q ) \Big( D_{\mu\nu} \sum_{ab} w_A (1-w_A)\; t^{\mu\nu}_{ab} \Big) \Big] .
\end{aligned}
\qquad (20)
```

Here $`\delta_{pq}`$ is the Kronecker delta, one if $`p = q`$ and zero otherwise. The factor
$`2-\delta_{pq}`$ is the count of entries the parameter sets: in
$`\mathbf{h}^{\dagger}\Sigma\,\mathbf{h} = \sum_{pq} h_p \Sigma_{pq} h_q`$ the parameter $`\Sigma_{pq}`$ with $`p \ne q`$ appears
twice, as $`h_p \Sigma_{pq} h_q`$ and as $`h_q \Sigma_{qp} h_p`$, and a diagonal one appears once. The last step uses
two things: $`\mathbf{h}^A`$ and $`\mathbf{h}^B`$ belong to the atoms and are the same for every
primitive, so they come outside the sum over $`a, b`$; and $`w_B = 1 - w_A`$. The three sums in round
brackets are all made of

```math
s^{\mu\nu}_n(\mathbf{k}) = D_{\mu\nu} \sum_{ab} w_A^{\,n}\; t^{\mu\nu}_{ab}, \qquad n = 0, 1, 2 , \qquad (21)
```

since $`(1-w_A)^2 = 1 - 2w_A + w_A^2`$ and $`w_A(1-w_A) = w_A - w_A^2`$. $`s^{\mu\nu}_0`$ is the contribution of
the pair $`\mu\nu`$ to $`F`$ itself, $`F = \sum_{\mu\nu} s^{\mu\nu}_0`$. So

```math
\frac{\partial F}{\partial \Sigma_{pq}} = -\frac{2-\delta_{pq}}{2} \sum_{\mu\nu}
   \Big[ s^{\mu\nu}_2\, h^A_p\, h^A_q + (s^{\mu\nu}_0 - 2 s^{\mu\nu}_1 + s^{\mu\nu}_2)\, h^B_p\, h^B_q
       + (s^{\mu\nu}_1 - s^{\mu\nu}_2)\,( h^A_p\, h^B_q + h^B_p\, h^A_q ) \Big] , \qquad (22)
```

where $`A`$ is the atom of $`\mu`$ and $`B`$ the atom of $`\nu`$. An atom outside the rigid body does not
move with $`\Sigma`$, and has $`\mathbf{h} = 0`$.

**With respect to the position of an atom.** A position enters a term of (15) in three places:
the centre $`\mathbf{P}`$, the vector $`\mathbf{R}`$ in $`\tilde g^{\mu\nu}_{ab}`$, and the density matrix element $`D_{\mu\nu}`$.
Tonto offers three derivatives, which keep one, two or all three of these
(`tc_position_derivs=`):

- `center`. Only $`\mathbf{P}`$ is moved. By (7), $`\partial\mathbf{P}/\partial\mathbf{R}^A = w_A`$ and $`\partial\mathbf{P}/\partial\mathbf{R}^B = w_B = 1 - w_A`$.
  For atom $`C`$, a term depends on $`\mathbf{R}^C`$ if $`\mu`$ is on $`C`$, or $`\nu`$ is, or both:

```math
\begin{aligned}
\frac{\partial F}{\partial \mathbf{R}^C}
 &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} c^\mu_a c^\nu_b\; \tilde g^{\mu\nu}_{ab}(\mathbf{k}; \mathbf{R})\; e^{-\frac12 \mathbf{k}^{\dagger} U^P \mathbf{k}}\;
    \frac{\partial}{\partial \mathbf{R}^C} \Big( e^{i\mathbf{k}\cdot\mathbf{P}} \Big) \\
 &= \sum_{\mu\nu} D_{\mu\nu} \sum_{ab} t^{\mu\nu}_{ab}\;\, i\mathbf{k}\, \frac{\partial \mathbf{P}}{\partial \mathbf{R}^C} \\
 &= i\mathbf{k} \Big[ \sum_{\mu\nu,\ \mu \text{ on } C} D_{\mu\nu} \sum_{ab} w_A\; t^{\mu\nu}_{ab}
              + \sum_{\mu\nu,\ \nu \text{ on } C} D_{\mu\nu} \sum_{ab} (1 - w_A)\; t^{\mu\nu}_{ab} \Big] \\
 &= i\mathbf{k} \Big[ \sum_{\mu\nu,\ \mu \text{ on } C} s^{\mu\nu}_1 + \sum_{\mu\nu,\ \nu \text{ on } C} ( s^{\mu\nu}_0 - s^{\mu\nu}_1 ) \Big] .
\end{aligned}
\qquad (23)
```

- `fixed-dm`. $`\mathbf{P}`$ and $`\mathbf{R}`$ are moved: the basis functions move with the atom, and the density
  matrix is held. By central differences of (15), step 0.01 bohr.
- `scf`. All three: as `fixed-dm`, with an SCF at each displaced geometry, started from the
  undisplaced orbitals, so that the density matrix follows the atom.

In all three a unique atom moves together with its symmetry images in the molecule. The
derivatives of `fixed-dm` and `scf` are made once for each fit with a given density matrix.

## 6 to 8. The averages beyond first order

Sections 6, 7 and 8, with equations (24) to (45), are in `docs/REPORT_ON_TC_TLS_AVERAGES.md`.

- **§6, the stiff modes averaged numerically** (`tc_integrate_stiff_modes=`): the structure
  factor is made at geometries displaced along each stiff mode, with an SCF at each, and the
  difference from what (15) gives is added as a correction, $`\Delta F`$ of (29).
- **§7, the rigid-body motion averaged numerically** (`tc_integrate_rigid_body=`): the same along
  each principal coordinate of $`\Sigma`$, the molecule being turned exactly; the correction is
  $`\Delta F^{\rm rb}`$ of (33).
- **§8, the rigid-body motion averaged by formula** (`tc_analytic_rigid_body=`): each term of (15)
  is averaged over the rotation to second order, (43), all six coordinates together, with no
  displaced geometry and no SCF; the derivatives with respect to $`\Sigma`$ gain (45).

## 9. Keywords

In `xray_data=`:

| keyword | default | meaning |
|---|---|---|
| `partition_model= tc-tls` | | the structure factor (15); sets `adp_model= tls` |
| `tc_position_derivs=` | `center` | the position derivative: `center` (23), `fixed-dm` or `scf` (§5) |
| `tc_integrate_stiff_modes=` | `FALSE` | `TRUE` adds $`\Delta F`$ of (29) |
| `tc_integrate_rigid_body=` | `FALSE` | `TRUE` adds $`\Delta F^{\rm rb}`$ of (33) |
| `tc_analytic_rigid_body=` | `FALSE` | `TRUE` averages the rigid-body motion by (43), for a whole molecule; not with `tc_integrate_rigid_body=` |
| `tc_quadrature_points=` | 3 | the number of points of the rule in §6 and §7: odd, 3 or more |

On the molecule:

| keyword | meaning |
|---|---|
| `check_two_center_derivs` | compares the derivatives with respect to $`\Sigma`$, (22) with (45), and to the positions, (23), with central differences of the structure factor and prints the largest disagreement; for (23) that is the size of what `center` leaves out |
| `check_two_center_average` | compares the average by formula, (43), with the numerical one, (33), at the refined $`\Sigma`$ |

A Hessian from ORCA is turned into `force_constants=` by `scripts/orca_hessian_to_tonto.py`.

## 10. Where it is implemented

| piece | procedure |
|---|---|
| the stiff modes, $`\mathbf{d}^A_k`$ and $`\langle Q_k^2\rangle`$ | `MOLECULE.PROP:make_stiff_modes` |
| the stiff-mode part of (2) for every atom pair | `MOLECULE.PROP:make_internal_ADPs`, `CRYSTAL:set_fragment_U_high_pair` |
| $`B^A`$ of (1) | `CRYSTAL:TLS_displacement_mx` |
| $`\Sigma`$ from the atoms' ADPs | `CRYSTAL:make_TLS_sigma_vector` |
| $`U^{AB}`$, (2), for every atom pair | `CRYSTAL:make_two_center_ADPs` |
| the phase, the Gaussian factor and the temperature factor of (15) with (14), for each pair of primitives | `SHELL2:make_TLS_prefactors` |
| the polynomials of the transform (10) along each axis, and their derivatives | `SHELL2:make_e_coeff`, `SHELL2:make_ft_polynomials` |
| $`\eta`$ of (41), $`\mathbf{q}`$ and $`Y`$ of (43), and the derivatives of a term with respect to $`\mathbf{y}`$ and $`L`$ | `SHELL2:make_turn_factors` |
| the transform of a shell pair, for every pair of basis functions or traced with the density matrix | `SHELL2:make_FT_TLS`, `SHELL2:make_FT_TLS_moments` |
| $`L`$ and $`\langle\boldsymbol{\lambda}\mathbf{u}^{A\dagger}\rangle`$ of (40) | `CRYSTAL:make_two_center_ADPs` |
| $`s_0`$, $`s_1`$, $`s_2`$ of (21) for a shell pair | `MOLECULE.XTAL:make_FT_pair_TLS_moments` |
| (22) and (45) for all the pairs of one atom with the others | `MOLECULE.XTAL:add_TLS_sigma_derivs` |
| $`F`$ with (22) and the sums of (23) | `MOLECULE.XTAL:make_FT_and_TLS_derivs` |
| the rotation between an atom and its image | `CRYSTAL:image_rotation` |
| the shifts from those derivatives | `CRYSTAL:get_two_center_parameter_shifts` |
| derivatives taken directly with respect to refined parameters | `DIFFRACTION_DATA.SET:solve_normal_equations` |
| position derivatives by differences, `fixed-dm` and `scf` | `MOLECULE.HAR:make_two_center_position_derivs` |
| the cosine factor in (28) | `SHELL2:scale_prefactors_by_shift` |
| the average of one uncorrelated coordinate, (28) | `MOLECULE.HAR:make_mode_SF_correction` |
| $`\Delta F`$, (29) | `MOLECULE.HAR:make_stiff_mode_SF_correction` |
| the principal coordinates of $`\Sigma`$, (30) and (31) | `CRYSTAL:make_TLS_principal_motions` |
| $`\Delta F^{\rm rb}`$, (33), with the turn (32) | `MOLECULE.HAR:make_rigid_body_SF_correction` |
| the fit loop | `MOLECULE.HAR:LS_fit_two_center` |
| the parts of a fit shared with Hirshfeld atom refinement | `CRYSTAL:start_structure_fit`, `apply_parameter_shifts`, `finish_fit_cycle`, `finish_structure_fit` |
| an SCF made ready at a new geometry | `MOLECULE.HAR:prepare_SCF_at_new_geometry` |
| the check of the derivatives | `MOLECULE.HAR:check_two_center_derivs` |
| the check of (43) against (33) | `MOLECULE.HAR:check_two_center_average` |


## 11. Results

All refinements use the TLS model with the stiff modes from a Hessian and no soft modes refined,
RHF wavefunctions of the isolated molecule, and the `center` position derivative unless it is
named. The Hessian is RHF in the same basis set, from ORCA, at the geometry of the Hirshfeld
atom refinement, with frequencies scaled by 0.8953. The def2-TZVP and gly-L-ala calculations
use RI-J and COSX.

### Urea

817 reflections at 123 K; 17 parameters. Goodness of fit:

| refinement | def2-SVP | def2-TZVP |
|---|---|---|
| Hirshfeld atoms | 3.4855 | 3.0570 |
| `tc-tls` | 3.6578 | 3.2369 |
| `tc-tls`, rigid-body motion averaged (§7), 3 points | 3.4876 | 3.2403 |
| `tc-tls`, rigid-body motion averaged, 5 points | 3.4493 | 3.1201 |
| `tc-tls`, rigid-body motion averaged, 7 points | 3.4483 | 3.1150 |
| `tc-tls`, rigid-body motion averaged, 9 points | 3.4483 | 3.1148 |
| `tc-tls`, rigid-body motion averaged, 11 points | 3.4483 | 3.1148 |
| `tc-tls`, rigid-body motion averaged by formula (§8) | 3.3337 | 2.9503 |
| `tc-tls`, rigid-body motion and stiff modes (§6) averaged, 3 points | 3.4606 | 3.2795 |
| `tc-tls`, rigid-body motion and stiff modes averaged, 5 points | 3.4227 | 3.1353 |

- **Without the averages `tc-tls` fits worse than Hirshfeld atoms, by 0.17 to 0.18 in GoF at both
  basis sets.**
- **Averaging the rigid-body motion moves the fit toward Hirshfeld atoms.** Urea librates by
  about 7° root mean square about the C=O bond. A Gaussian product with lobes must turn with the
  molecule, and without §7 it does not. Hirshfeld atoms do better there because density near a
  hydrogen is given to the hydrogen, with its large ADP.
- **Three points are too few for the rigid-body average; seven are enough.** The GoF settles at
  3.4483 at def2-SVP and 3.1148 at def2-TZVP, the same to four figures with 9 and 11 points.
  Five points are within 0.005 of that; three are out by 0.04 at def2-SVP and by 0.13 at
  def2-TZVP, where they show no gain at all. The outer points of the three-point rule are at 13°,
  where moving on an arc and moving on a straight line differ by a phase of order one at high
  angle.
- **With the numerical rigid-body average converged, `tc-tls` gains 0.21 in GoF at def2-SVP and
  0.12 at def2-TZVP.** That puts it 0.04 below Hirshfeld atoms at def2-SVP and 0.06 above at
  def2-TZVP.
- **Averaged by formula, `tc-tls` fits better than Hirshfeld atoms at both basis sets**: by 0.15
  in GoF at def2-SVP and by 0.11 at def2-TZVP, with the same 17 parameters. It gains 0.32 and
  0.29 on `tc-tls` without the average.
- **The formula and the numerical average agree at low angle and differ at high angle.** At the
  refined $`\Sigma`$, def2-SVP, the two corrections to the structure factors have the same
  root-mean-square size, 0.013 electrons, and differ by 0.003, a fifth of it. For the 136
  reflections with $`|\mathbf{k}|`$ below 2 bohr⁻¹ the correction is 0.025 and the difference
  0.001, a twentieth. The numerical average takes the principal coordinates one at a time (§8).
  For one product of two p functions, where the true average over all three rotations can be
  done numerically, the one-at-a-time average is out by 1 to 4 % of the correction up to
  2 bohr⁻¹ and by up to 40 % at 4 to 6 bohr⁻¹; the formula is within 2 to 4 % throughout.
- **Averaging the stiff modes helps at def2-SVP and hurts at def2-TZVP.** At def2-TZVP the stiff
  modes include an out-of-plane NH₂ mode at 274 cm⁻¹ in which a hydrogen moves by 0.2 Å root
  mean square; a straight-line displacement that large stretches the N–H bonds. At def2-SVP that
  mode is below the cutoff and is not among the stiff modes.

Position derivatives and bond lengths, def2-SVP, with the SCF converged to 1e-4 for the first
four rows and to 1e-7 for the rest:

| refinement | GoF | R(F) | N–H1 /Å | N–H3 /Å | C=O /Å | C–N /Å |
|---|---|---|---|---|---|---|
| Hirshfeld atoms | 3.4854 | 0.0181 | 1.028 | 0.994 | 1.2559 | 1.3411 |
| `tc-tls`, `center` | 3.6577 | 0.0186 | 1.024 | 0.977 | 1.2560 | 1.3412 |
| `tc-tls`, `fixed-dm` | 3.6681 | 0.0183 | 1.023 | 1.013 | 1.2561 | 1.3413 |
| `tc-tls`, `scf` | 3.6292 | 0.0183 | 1.023 | 0.994 | 1.2562 | 1.3413 |
| `tc-tls`, `center`, rigid body averaged, 3 points | 3.4876 | 0.0183 | 1.031 | 0.991 | 1.2620 | 1.3495 |
| `tc-tls`, `center`, rigid body and stiff modes averaged, 3 points | 3.4606 | 0.0183 | 1.032 | 0.993 | 1.2619 | 1.3495 |
| `tc-tls`, `center`, rigid body averaged by formula | 3.3337 | 0.0169 | 1.030 | 0.988 | 1.2635 | 1.3482 |

- **Only the `scf` position derivative is right.** With `tc-tls` and the TLS parameters alone
  refined at the Hirshfeld geometry the GoF is 3.6377; `center` and `fixed-dm` end above that,
  `scf` below. `fixed-dm`, which moves the basis functions and holds the density matrix, is worse
  than `center`, which moves neither. N–H3 follows the derivative: 0.977, 1.013 and 0.994 Å.
- **Averaging the rigid-body motion lengthens the bonds between the heavy atoms**: C=O by 0.006 Å
  and C–N by 0.008 Å numerically, and by 0.008 and 0.007 Å by formula. In §7 and §8 the atoms
  move on arcs, so the refined positions are those of the
  turning molecule itself; a refinement that moves atoms in straight lines places them closer
  together. The lengthening is the libration correction to a bond length, here part of the
  structure-factor model.
- **The correlation between atoms is a small effect.** At the Hirshfeld parameters, and with an
  RHF/6-31G(d) Hessian, `tc-tls` gives GoF 3.6461 and `tc-tanaka`, which leaves the correlation
  out, 3.6561.
- **The check of the derivatives** (`check_two_center_derivs`): those with respect to $`\Sigma`$
  agree with central differences to 2 parts in 10⁸, with and without the average by formula;
  what `center` leaves out of the position derivatives is 13 % of the largest. Without (45)
  the derivatives with respect to $`\Sigma`$ would be out by 34 % of the largest when the formula
  is on, and the refinement then stops at GoF 3.3586 instead of 3.3337.
- **Time**, def2-SVP, one core, for the whole job with its SCFs: 19 seconds for `center`, and
  86 seconds with the rigid-body average by formula. The numerical rigid-body average with
  seven points takes 12 minutes.

### Glycyl-L-alanine

2531 reflections at 150 K (Capelli et al., 2014); 81 parameters, 20 of them for T, L and S. The
molecule is treated as one rigid body. Five internal modes are below the cutoff of 200 cm⁻¹ at
def2-SVP, at 77, 125, 130, 154 and 166 cm⁻¹ after scaling; the methyl torsion is spread over the
last two. They are neither among the stiff modes nor refined.

| refinement | def2-SVP GoF | def2-SVP R(F) | def2-TZVP GoF | def2-TZVP R(F) |
|---|---|---|---|---|
| Hirshfeld atoms | 3.5940 | 0.0353 | 3.5575 | 0.0344 |
| `tc-tls` | 3.6131 | 0.0354 | 3.5750 | 0.0347 |
| `tc-tls`, rigid-body motion averaged, 3 points | 3.6054 | 0.0354 | 3.5689 | 0.0347 |
| `tc-tls`, rigid-body motion averaged, 5 points | 3.6039 | 0.0354 | | |
| `tc-tls`, rigid-body motion averaged by formula | 3.6042 | 0.0354 | | |

- **The two smearings differ much less than in urea**: `tc-tls` is 0.02 above Hirshfeld atoms in
  GoF at both basis sets, against 0.17.
- **Averaging the rigid-body motion recovers a third to a half of that**: 0.006 of 0.018 at
  def2-TZVP with three points, 0.009 of 0.019 at def2-SVP with five. At def2-SVP three and five
  points agree to 0.002: a larger molecule librates less, and the rule is adequate.
- **The formula gives the same as the numerical average here**, 3.6042 against 3.6039 at
  def2-SVP, and `tc-tls` stays 0.010 above Hirshfeld atoms. The gain over Hirshfeld atoms
  found for urea is not found for this molecule.

## 12. Urea: against the multi-temperature analysis of Capelli, Förtsch and Bürgi

Capelli, Förtsch and Bürgi (2000) fitted a dynamical model to the neutron ADPs of urea at 12, 60
and 123 K: molecular translations and librations, one out-of-plane coordinate of the NH₂ groups
coupled to them, and a temperature-independent part for the stiff internal modes. The
refinements here use one X-ray data set at 123 K and a calculated Hessian for the stiff modes.
The two can be compared at 123 K.

**Axes.** Those of their paper, with the origin at the centre of mass: $`x`$ along the C=O bond,
$`y`$ in the molecular plane, $`z`$ normal to the plane. $`L_{11}`$ is then the libration about the
C=O bond.

**The rigid-body tensors at 123 K.** Their model w1, the one they prefer, and the segmented
rigid-body analysis of Swaminathan, Craven and McMullan (1984) that they quote, are from their
Table 8. $`L`$ is in rad², $`T`$ in Å², $`S`$ in rad Å; the sign of $`S`$ depends on the direction chosen
for the axes, so its size is given.

| | Capelli et al., model w1 | Swaminathan et al. | def2-SVP, Hirshfeld atoms | def2-SVP, `tc-tls`, both averages | def2-TZVP, Hirshfeld atoms | def2-TZVP, `tc-tls`, both averages |
|---|---|---|---|---|---|---|
| in the plane | | | | | | |
| $`L_{33}`$ | 0.0021(1) | 0.0019 | 0.00232(5) | 0.00233(5) | 0.00235(5) | 0.00237(5) |
| $`T_{11}`$ | 0.0050(2) | 0.0067 | 0.00582(4) | 0.00581(4) | 0.00573(4) | 0.00571(4) |
| $`T_{22}`$ | 0.0132(3) | 0.0148 | 0.01347(7) | 0.01342(7) | 0.01337(7) | 0.01329(7) |
| $`\lvert S_{32}\rvert`$ | 0.0011(1) | 0.0011 | 0.00105(3) | 0.00103(3) | 0.00109(3) | 0.00108(3) |
| out of the plane | | | | | | |
| $`L_{11}`$ | 0.0149(3) | 0.0150 | 0.0134(12) | 0.0140(9) | 0.0177(9) | 0.0189(9) |
| $`L_{22}`$ | 0.0038(3) | 0.0080 | 0.0097(9) | 0.0088(9) | 0.0055(9) | 0.0043(9) |
| $`T_{33}`$ | 0.0136(4) | 0.0150 | 0.01435(15) | 0.01436(14) | 0.01411(13) | 0.01414(13) |
| $`\lvert S_{23}\rvert`$ | 0.0016(2) | 0.0042 | 0.0052(7) | 0.0045(7) | 0.0023(7) | 0.0014(5) |

The `tc-tls` columns have the rigid-body motion and the stiff modes averaged with five points
(§6, §7). Esds are in brackets; the axes are the principal axes of inertia, in which Tonto prints
these tensors.

- **The in-plane motion agrees**: $`L_{33}`$, $`T_{22}`$ and $`S_{32}`$ are within 0.0003 of model w1 in
  every column, and are determined as sharply from one X-ray data set as theirs from three
  neutron temperatures. $`T_{11}`$ lies between model w1 and Swaminathan et al.
- **The out-of-plane librations are less well determined**, with esds near 0.001, and they
  depend on the basis set through the cutoff. At def2-SVP no out-of-plane NH₂ mode is in the
  model, and $`L_{22}`$ and $`S_{23}`$ agree with the rigid-body analysis of Swaminathan et al. At
  def2-TZVP one such mode, at 274 cm⁻¹, is among the stiff modes, and $`L_{22}`$ and $`S_{23}`$ move to
  within two esds of model w1, which has an out-of-plane NH₂ coordinate; $`L_{11}`$ then comes out
  larger than theirs, by three to four esds.
- **Hirshfeld atoms and `tc-tls` give the same tensors within one or two esds.**

**The temperature-independent part.** Their $`\varepsilon`$ tensors are refined, one for the hydrogens
and one for the other atoms (their Table 5, model w1); their Table 7 gives values calculated
from spectroscopic force fields and from an ab initio calculation. Here the same quantity is
$`U^{A}_{\rm high}`$ of (M8), from the calculated Hessian with frequencies scaled by 0.8953, at 123 K.
In units of 10⁻⁴ Å²; for a hydrogen, along the N–H bond, across it in the molecular plane, and
out of the plane:

| | Capelli et al., refined | their Table 7, calculated | def2-SVP | def2-TZVP |
|---|---|---|---|---|
| H, along N–H | 66(2) | 53 to 54 | 52 to 58 | 53 to 58 |
| H, in the plane across N–H | 120(3) | 134 to 142 | 142 to 149 | 140 to 144 |
| H, out of the plane | 263(6) | 240 to 265 | 25 and 195 | 221 and 266 |
| C, N, O, in the plane | 13(1) to 15(1) | 7 to 9 | 7 to 10 | 8 to 10 |
| C, N, O, out of the plane | 16(2) | 2 to 16 | 1 to 14 | 2 to 15 |

- **In the plane the calculated stiff-mode ADPs agree with their calculated values**, and with
  their refined ones to about 15 %.
- **Out of the plane the hydrogens depend on which modes count as stiff.** At def2-SVP two
  out-of-plane NH₂ modes are below the cutoff of 200 cm⁻¹ (one imaginary, one at 169 cm⁻¹ after
  scaling), and are in neither the stiff part nor the refinement; one hydrogen then has only 25.
  At def2-TZVP one of them is at 274 cm⁻¹, counts as stiff, and both hydrogens come out near
  their values.

## References

- H. B. Bürgi and S. C. Capelli, *Acta Cryst.* A56, 403 (2000), and S. C. Capelli, M. Förtsch and
  H. B. Bürgi, *Acta Cryst.* A56, 413 (2000): molecular motion from multi-temperature ADPs; urea.
- S. Swaminathan, B. M. Craven and R. K. McMullan, *Acta Cryst.* B40, 300 (1984): urea, neutron
  diffraction at 12, 60 and 123 K.
- V. Schomaker and K. N. Trueblood, *Acta Cryst.* B24, 63 (1968): rigid-body motion, T, L, S.
- R. F. Stewart, *J. Chem. Phys.* 51, 4569 (1969), and K. Tanaka, *Acta Cryst.* A44, 1002 (1988):
  two-centre temperature factors.
- D. Jayatilaka and D. J. Grimwood, *Acta Cryst.* A57, 76 (2001): the two-centre models in Tonto.
- L. E. McMurchie and E. R. Davidson, *J. Comput. Phys.* 26, 218 (1978): the expansion of a
  product of Cartesian Gaussians used for its Fourier transform.
- J. R. Michael and T. Koritsanszky, *J. Math. Chem.* 53, 250 (2015): the ADP of a product of two
  Gaussians from the full covariance of the nuclear displacements, tested against an average
  over static densities.
