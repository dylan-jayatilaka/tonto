# Smearing the density between two atoms: the averages beyond first order

`docs/REPORT_ON_TC_TLS.md` smears each product of two Gaussian basis functions by the motion of
its own centre. That is a first-order average: it leaves out the change of the product itself
when its two atoms move, and the turning of the product when the molecule turns. This page
gives three ways of putting those back: a numerical average over each stiff internal mode, a
numerical average over the rigid-body motion, and an average over the rigid-body motion by
formula.

This page continues that report. Its sections are numbered 6 to 8 and its equations (24) to
(45); sections 1 to 5 and equations (1) to (23) are in that report and are cited here by their
numbers, as are its later sections, 9 to 12, which hold the keywords, the list of procedures and
the results. Equations and sections of `docs/REPORT_ON_MODE_FITTING.md` are cited as (M1), §M1
and so on. Units: atomic units; vectors are columns; † is the transpose; ⟨ ⟩ is the thermal
average.

## 6. The stiff modes averaged numerically

§3 made an approximation: two quantities that change when the atoms move were held at their
values at the mean geometry. This section removes that approximation for the stiff modes, one
mode at a time, by doing the average numerically (`tc_integrate_stiff_modes=`).

**What changes when one mode moves.** Take one stiff mode $`k`$ and give its normal coordinate the
value $`Q`$, all other motion aside for the moment. Atom $`A`$ is displaced by $`Q\,\mathbf{d}^A_k`$ (§M1.7).
By (12), with $`\mathbf{u}^A = Q\,\mathbf{d}^A_k`$ and $`\mathbf{u}^B = Q\,\mathbf{d}^B_k`$, the Gaussian product $`g^{\mu\nu}_{ab}`$ contributes

```math
D_{\mu\nu}(Q)\; c^\mu_a c^\nu_b\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   e^{iQ\,\mathbf{k}\cdot\mathbf{d}^P_k}\;
   \tilde g^{\mu\nu}_{ab}\big(\mathbf{k};\, \mathbf{R} + Q(\mathbf{d}^A_k - \mathbf{d}^B_k)\big), \qquad
\mathbf{d}^P_k = w_A \mathbf{d}^A_k + w_B \mathbf{d}^B_k . \qquad (24)
```

Three things in (24) depend on $`Q`$:

1. the phase $`e^{iQ\,\mathbf{k}\cdot\mathbf{d}^P_k}`$, because the centre of the Gaussian product moves by $`Q\,\mathbf{d}^P_k`$;
2. the transform $`\tilde g^{\mu\nu}_{ab}`$, because the vector between the two atoms changes from $`\mathbf{R}`$
   to $`\mathbf{R} + Q(\mathbf{d}^A_k - \mathbf{d}^B_k)`$. In (8) that vector enters through the factor $`e^{-\xi|\mathbf{R}|^2}`$,
   which sets how large the product is, and through the two polynomials;
3. the density matrix element $`D_{\mu\nu}(Q)`$, because the wavefunction at the displaced geometry is
   not the one at the mean geometry.

**Two structure factors as functions of $`Q`$.** The other motion smears each Gaussian product as before.
Let $`W^{P(k)}`$ be the temperature factor of (15) with the term of mode $`k`$ taken out of
$`U^P`$, (19). $`F^{\rm exact}_k(Q)`$ keeps all three dependences of (24); $`F^{\rm approx}_k(Q)`$
keeps only the phase, as §3 does:

```math
\begin{aligned}
W^{P(k)} &= \exp\!\Big[ -\tfrac12\, \mathbf{k}^{\dagger} \big( U^P - \langle Q_k^2\rangle\, \mathbf{d}^P_k\mathbf{d}^{P\dagger}_k \big)\, \mathbf{k} \Big] , \\
F^{\rm exact}_k(Q) &= \sum_{\mu\nu} D_{\mu\nu}(Q) \sum_{ab} c^\mu_a c^\nu_b\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   e^{iQ\,\mathbf{k}\cdot\mathbf{d}^P_k}\;
   \tilde g^{\mu\nu}_{ab}\big(\mathbf{k};\, \mathbf{R} + Q(\mathbf{d}^A_k - \mathbf{d}^B_k)\big)\; W^{P(k)} , \\
F^{\rm approx}_k(Q) &= \sum_{\mu\nu} D_{\mu\nu}(0) \sum_{ab} c^\mu_a c^\nu_b\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   e^{iQ\,\mathbf{k}\cdot\mathbf{d}^P_k}\;
   \tilde g^{\mu\nu}_{ab}(\mathbf{k};\, \mathbf{R})\; W^{P(k)} .
\end{aligned}
\qquad (25)
```

The two are equal at $`Q = 0`$.

**The average of the approximate one is the structure factor of §3.** $`Q`$ has a Gaussian
distribution with zero mean and mean square $`\langle Q_k^2\rangle`$, (M6), so
$`\langle e^{iQ\theta}\rangle = e^{-\frac12 \langle Q_k^2\rangle \theta^2}`$ for any number $`\theta`$, as in (13). Averaging
$`F^{\rm approx}_k`$ of (25) over $`Q`$:

```math
\begin{aligned}
\langle F^{\rm approx}_k \rangle
 &= \sum_{\mu\nu} D_{\mu\nu}(0) \sum_{ab} c^\mu_a c^\nu_b\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   \tilde g^{\mu\nu}_{ab}(\mathbf{k};\, \mathbf{R})\; W^{P(k)}\; \big\langle e^{iQ\,\mathbf{k}\cdot\mathbf{d}^P_k} \big\rangle
   && \text{only the phase depends on } Q \\
 &= \sum_{\mu\nu} D_{\mu\nu}(0) \sum_{ab} c^\mu_a c^\nu_b\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   \tilde g^{\mu\nu}_{ab}(\mathbf{k};\, \mathbf{R})\; W^{P(k)}\; e^{-\frac12 \langle Q_k^2\rangle (\mathbf{k}\cdot\mathbf{d}^P_k)^2}
   && \text{with } \theta = \mathbf{k}\cdot\mathbf{d}^P_k \\
 &= \sum_{\mu\nu} D_{\mu\nu}(0) \sum_{ab} c^\mu_a c^\nu_b\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   \tilde g^{\mu\nu}_{ab}(\mathbf{k};\, \mathbf{R})\; e^{-\frac12 \mathbf{k}^{\dagger} U^P \mathbf{k}}
   && \text{by the definition of } W^{P(k)} \\
 &= F \text{ of (15)} .
\end{aligned}
\qquad (26)
```

The average of $`F^{\rm exact}_k`$ cannot be done by formula, because $`D_{\mu\nu}(Q)`$ is known only by
doing an SCF at each $`Q`$. That average is done numerically, with three points.

**The three-point rule.** For any function $`g(Q)`$ of a Gaussian variable with zero mean and mean
square $`\langle Q_k^2\rangle`$, the three-point Gauss–Hermite rule is

```math
\langle g \rangle \approx \tfrac23\, g(0) + \tfrac16\, g(+s_k) + \tfrac16\, g(-s_k), \qquad
s_k = \big( 3\,\langle Q_k^2\rangle \big)^{1/2} , \qquad (27)
```

exact when $`g`$ is a polynomial of degree five or less.

**The error of §3 for mode $`k`$.** The error is the difference of the two averages, $`\Delta_k`$. Applying
the rule (27) to the difference $`g(Q) = F^{\rm exact}_k(Q) - F^{\rm approx}_k(Q)`$:

```math
\begin{aligned}
\Delta_k &= \langle F^{\rm exact}_k \rangle - \langle F^{\rm approx}_k \rangle
          = \big\langle\, F^{\rm exact}_k(Q) - F^{\rm approx}_k(Q) \,\big\rangle \\
 &\approx \tfrac23 \big[ F^{\rm exact}_k(0) - F^{\rm approx}_k(0) \big]
        + \tfrac16 \big[ F^{\rm exact}_k(s_k) - F^{\rm approx}_k(s_k) \big]
        + \tfrac16 \big[ F^{\rm exact}_k(-s_k) - F^{\rm approx}_k(-s_k) \big]
   && \text{by (27)} \\
 &= \tfrac16 \big[ F^{\rm exact}_k(s_k) + F^{\rm exact}_k(-s_k) \big]
  - \tfrac16 \big[ F^{\rm approx}_k(s_k) + F^{\rm approx}_k(-s_k) \big]
   && \text{the two are equal at } Q = 0 \\
 &= \tfrac16 \big[ F^{\rm exact}_k(s_k) + F^{\rm exact}_k(-s_k) \big]
  - \tfrac16 \sum_{\mu\nu} D_{\mu\nu}(0) \sum_{ab} c^\mu_a c^\nu_b\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   \tilde g^{\mu\nu}_{ab}(\mathbf{k}; \mathbf{R})\; W^{P(k)}\;
   \big( e^{i s_k \mathbf{k}\cdot\mathbf{d}^P_k} + e^{-i s_k \mathbf{k}\cdot\mathbf{d}^P_k} \big)
   && \text{by (25)} \\
 &= \tfrac16 \big[ \underbrace{F^{\rm exact}_k(s_k)}_{F_k^{+}} + \underbrace{F^{\rm exact}_k(-s_k)}_{F_k^{-}} \big]
  - \tfrac13 \underbrace{ \sum_{\mu\nu} D_{\mu\nu}(0) \sum_{ab} c^\mu_a c^\nu_b\; e^{i\mathbf{k}\cdot\mathbf{P}}\;
   \tilde g^{\mu\nu}_{ab}(\mathbf{k}; \mathbf{R})\; W^{P(k)}\;
   \cos( s_k\, \mathbf{k}\cdot\mathbf{d}^P_k ) }_{F_k^{\cos}}
   && e^{i\phi} + e^{-i\phi} = 2\cos\phi \\
 &= \tfrac16 \big( F_k^{+} + F_k^{-} \big) - \tfrac13\, F_k^{\cos} .
\end{aligned}
\qquad (28)
```

**How the three are made.**

- $`F_k^{+}`$: move every atom $`A`$ by $`+s_k\,\mathbf{d}^A_k`$, do an SCF there, and make the structure factor
  from (15) with the new positions, the new density matrix, and $`W^{P(k)}`$ as the temperature
  factor. Moving the atoms with their basis functions puts in the phase and the change of
  $`\tilde g^{\mu\nu}_{ab}`$; the SCF puts in $`D_{\mu\nu}(s_k)`$. That is $`F^{\rm exact}_k(s_k)`$ of (25).
- $`F_k^{-}`$: the same with $`-s_k`$.
- $`F_k^{\cos}`$: at the mean geometry with its own density matrix, the structure factor (15) with
  $`W^{P(k)}`$ as the temperature factor and each Gaussian product multiplied by
  $`\cos(s_k\,\mathbf{k}\cdot\mathbf{d}^P_k)`$.

**The correction.** Adding the stiff modes,

```math
\Delta F = \sum_{\omega_k > \omega_c} \Delta_k = \sum_{\omega_k > \omega_c} \Big[ \tfrac16 \big( F_k^{+} + F_k^{-} \big) - \tfrac13\, F_k^{\cos} \Big] , \qquad (29)
```

and the structure factor used in the refinement is (15) plus $`\Delta F`$.

- **Why the difference is integrated, not $`F^{\rm exact}_k`$ itself.** The phase in (25) varies
  quickly with $`Q`$ at high angle, where three points are too few; (26) does that part exactly.
  The difference is small and varies slowly, which is what the rule (27) is good for.
- **Why the modes are added.** Each $`\Delta_k`$ is small, so the effect of one on another is small
  twice over and is left out.
- **Cost and limits.** Two SCFs for each stiff mode. $`\Delta F`$ is made once for each fit with a
  given density matrix, and held fixed while the parameters shift. The correction is for a single molecule,
  with the harmonic amplitudes (M6). The rigid-body motion and the refined soft modes are averaged
  separately, in §7.

## 7. The rigid-body motion averaged numerically

The average of §6 can be applied to the rigid-body motion and the refined soft modes as well
(`tc_integrate_rigid_body=`). The motion is first written in coordinates that are uncorrelated,
as the stiff modes are; then each is averaged by (28).

**Uncorrelated coordinates.** $`\Sigma`$ is symmetric, so it has orthonormal eigenvectors
$`\mathbf{e}_j`$ with eigenvalues $`\lambda_j`$, $`\Sigma\,\mathbf{e}_j = \lambda_j \mathbf{e}_j`$. Write the generalised coordinates
$`\mathbf{v}`$ of (M11) in terms of them, $`\mathbf{v} = \sum_j c_j \mathbf{e}_j`$ with $`c_j = \mathbf{e}_j^{\dagger}\mathbf{v}`$. The $`c_j`$ are the
**principal coordinates** of the motion:

```math
\begin{aligned}
\langle c_i c_j \rangle &= \mathbf{e}_i^{\dagger}\, \langle \mathbf{v}\mathbf{v}^{\dagger} \rangle\, \mathbf{e}_j && \text{since } c_j = \mathbf{e}_j^{\dagger}\mathbf{v} \\
 &= \mathbf{e}_i^{\dagger}\, \Sigma\, \mathbf{e}_j && \Sigma = \langle \mathbf{v}\mathbf{v}^{\dagger}\rangle \\
 &= \lambda_j\, \mathbf{e}_i^{\dagger} \mathbf{e}_j && \Sigma\,\mathbf{e}_j = \lambda_j \mathbf{e}_j \\
 &= \lambda_j\, \delta_{ij} && \text{the eigenvectors are orthonormal.}
\end{aligned}
\qquad (30)
```

So different principal coordinates are uncorrelated, and $`c_j`$ has mean square $`\lambda_j`$. The
displacement of atom $`A`$, to first order, is

```math
\mathbf{u}^A = B^A \mathbf{v} = \sum_j c_j\, B^A \mathbf{e}_j = \sum_j c_j\, \mathbf{d}^A_j, \qquad \mathbf{d}^A_j = B^A \mathbf{e}_j , \qquad (31)
```

which has the form of the normal modes of §M1.5: $`\mathbf{d}^A_j`$ is the displacement of atom $`A`$ per unit
of $`c_j`$. In (2) the rigid-body term is then
$`B^A \Sigma B^{B\dagger} = \sum_j \lambda_j\, \mathbf{d}^A_j\mathbf{d}^{B\dagger}_j`$, the same form as the stiff-mode term. So §6
applies to each principal coordinate as it stands, with $`\mathbf{d}^A_j`$ for $`\mathbf{d}^A_k`$, $`\lambda_j`$ for
$`\langle Q_k^2\rangle`$, and $`s_j = (3\lambda_j)^{1/2}`$.

**The displaced geometry is a true turn.** One thing is different. An eigenvector has a
translation part, a rotation part and a soft-mode part,
$`\mathbf{e}_j = (\boldsymbol{\tau}_j, \boldsymbol{\omega}_j, q_{j1} \dots q_{jK})`$. At the value $`c`$ of the coordinate the molecule has
been turned through the angle $`|c\,\boldsymbol{\omega}_j|`$ about the axis along $`\boldsymbol{\omega}_j`$ through the origin $`\mathbf{O}`$
of §M1.4. (31) replaces that turn by its first-order displacement $`c\,\boldsymbol{\omega}_j \times (\mathbf{R}^A - \mathbf{O})`$,
(M4). For $`F^{\rm exact}_j`$ the atoms are put where the turn really takes them:

```math
\mathbf{R}^A(c) = \mathbf{O} + \mathsf{R}(c\,\boldsymbol{\omega}_j)\,(\mathbf{R}^A - \mathbf{O}) + c\,\boldsymbol{\tau}_j + c \sum_{k=1}^{K} q_{jk}\, \mathbf{d}^A_k , \qquad (32)
```

where $`\mathsf{R}(\boldsymbol{\theta})`$ is the rotation by the angle $`\theta = |\boldsymbol{\theta}|`$ about the unit vector
$`\mathbf{n} = \boldsymbol{\theta}/\theta`$, which takes a vector $`\mathbf{r}`$ to
$`\mathbf{r}\cos\theta + (\mathbf{n}\times\mathbf{r})\sin\theta + \mathbf{n}\,(\mathbf{n}\cdot\mathbf{r})(1-\cos\theta)`$. For a small angle (32) is
$`\mathbf{R}^A + c\,\mathbf{d}^A_j`$.

**The correction.** With the atoms at $`\mathbf{R}^A(\pm s_j)`$, an SCF there, and the structure factor
made from (15) with $`W^{P(j)}`$ of (25) as the temperature factor, $`F_j^{+}`$ and $`F_j^{-}`$ are found as
in §6. $`F_j^{\cos}`$ is (28)'s, with $`\mathbf{d}^P_j = w_A \mathbf{d}^A_j + w_B \mathbf{d}^B_j`$: it moves each Gaussian product's
centre in a straight line and turns nothing. Then

```math
\Delta F^{\rm rb} = \sum_j \Big[ \tfrac16 \big( F_j^{+} + F_j^{-} \big) - \tfrac13\, F_j^{\cos} \Big] , \qquad (33)
```

summed over the principal coordinates, and the structure factor used in the refinement is (15)
plus $`\Delta F`$ of (29) plus $`\Delta F^{\rm rb}`$.

- **What the difference holds.** When the molecule turns, the wavefunction turns with it, so a
  product of basis functions turns about its own centre, and its centre moves on an arc.
  $`F_j^{\cos}`$ has neither. For a Gaussian product that is round, turning about its centre
  changes nothing; for one with lobes, such as a p function on nitrogen pointing at a hydrogen,
  it carries the lobe through a larger arc than the centre.
- **A pure translation gives nothing.** If $`\boldsymbol{\omega}_j = 0`$ and there are no soft modes, moving the
  molecule changes only the phases, which $`F_j^{\cos}`$ has exactly; $`F_j^{+} + F_j^{-} = 2F_j^{\cos}`$.
- **The correction follows the refinement.** $`\Sigma`$ is refined, so $`\Delta F^{\rm rb}`$ is made at the start of each
  fit from the $`\Sigma`$ of that moment and held fixed during it. The stiff-mode $`\Delta F`$ does not
  change in this way, because its amplitudes come from the Hessian.
- **Cost.** Two SCFs for each principal coordinate, twelve for a rigid body without soft modes.
  The SCF is done for convenience: turning a molecule turns its density matrix and no more.

## 8. The rigid-body motion averaged by formula

A rigid motion of the molecule needs no new wavefunction: the density turns and moves with the
molecule. So the average over the rigid-body motion can be written down for all six coordinates
together, with no displaced geometries and no SCF (`tc_analytic_rigid_body=`). It is done here
to second order in the rotation.

**One Gaussian product in a turned molecule.** Let the molecule be turned by the rotation
$`\mathsf{R}(\boldsymbol{\lambda})`$ of (32) about the origin $`\mathbf{O}`$ of §M1.4, and then moved by $`\mathbf{t}`$. A point of the
molecule at $`\mathbf{y}`$ goes to

```math
\mathbf{r} = \mathsf{R}\,(\mathbf{y} - \mathbf{O}) + \mathbf{O} + \mathbf{t} , \qquad (34)
```

and the density there is that of the unmoved molecule at $`\mathbf{y}`$. Let $`\mathbf{p} = \mathbf{P} - \mathbf{O}`$ be the centre
of a Gaussian product from the origin. The transform of the moved product is

```math
\begin{aligned}
\int g^{\mu\nu}_{ab}(\mathbf{y})\, e^{i\mathbf{k}\cdot\mathbf{r}}\, d\mathbf{y}
 &= e^{i\mathbf{k}\cdot(\mathbf{O}+\mathbf{t})} \int g^{\mu\nu}_{ab}(\mathbf{y})\, e^{i(\mathsf{R}^{\dagger}\mathbf{k})\cdot(\mathbf{y}-\mathbf{O})}\, d\mathbf{y}
    && \text{by (34), and } \mathbf{k}\cdot\mathsf{R}\mathbf{z} = (\mathsf{R}^{\dagger}\mathbf{k})\cdot\mathbf{z} \\
 &= e^{i\mathbf{k}\cdot(\mathbf{O}+\mathbf{t})}\; e^{i(\mathsf{R}^{\dagger}\mathbf{k})\cdot\mathbf{p}}\; \tilde g^{\mu\nu}_{ab}(\mathsf{R}^{\dagger}\mathbf{k}; \mathbf{R})
    && \text{by (10) at } \mathsf{R}^{\dagger}\mathbf{k} \\
 &= e^{i\mathbf{k}\cdot\mathbf{O}}\; e^{i\mathbf{k}\cdot\mathbf{t}}\; e^{i\mathbf{k}\cdot\mathsf{R}\mathbf{p}}\; \tilde g^{\mu\nu}_{ab}(\mathsf{R}^{\dagger}\mathbf{k}; \mathbf{R}) .
\end{aligned}
\qquad (35)
```

Nothing is recalculated: the transform is that of the unmoved product, at the turned scattering
vector. Write it as its Gaussian factor times the rest,
$`\tilde g^{\mu\nu}_{ab}(\mathbf{k}; \mathbf{R}) = e^{-|\mathbf{k}|^2/4\gamma}\, \mathcal{E}(\mathbf{k})`$, where $`\mathcal{E}`$ is the polynomial in the
components of $`\mathbf{k}`$ of §2, with the constants. A rotation does not change $`|\mathbf{k}|`$, so the
Gaussian factor stays, and what has to be averaged is

```math
\mathcal{A}(\mathbf{k}) = \Big\langle e^{i\mathbf{k}\cdot\mathbf{t}}\; e^{i\mathbf{k}\cdot\mathsf{R}(\boldsymbol{\lambda})\mathbf{p}}\; \mathcal{E}\big(\mathsf{R}(\boldsymbol{\lambda})^{\dagger}\mathbf{k}\big) \Big\rangle \qquad (36)
```

over $`\mathbf{v} = (\mathbf{t}, \boldsymbol{\lambda})`$, Gaussian with zero mean and covariance $`\Sigma`$.

**The phase and the polynomial to second order.** For a small rotation
$`\mathsf{R}(\boldsymbol{\lambda})\mathbf{p} = \mathbf{p} + \boldsymbol{\lambda}\times\mathbf{p} + \tfrac12 \boldsymbol{\lambda}\times(\boldsymbol{\lambda}\times\mathbf{p})`$, and the same with
$`-\boldsymbol{\lambda}`$ and $`\mathbf{k}`$ gives $`\mathsf{R}^{\dagger}\mathbf{k}`$. Then

```math
\mathbf{k}\cdot\mathsf{R}(\boldsymbol{\lambda})\mathbf{p} = \mathbf{k}\cdot\mathbf{p} + (\mathbf{p}\times\mathbf{k})\cdot\boldsymbol{\lambda} + \tfrac12\, \boldsymbol{\lambda}^{\dagger} M \boldsymbol{\lambda}, \qquad
M = \tfrac12\big(\mathbf{k}\mathbf{p}^{\dagger} + \mathbf{p}\mathbf{k}^{\dagger}\big) - (\mathbf{k}\cdot\mathbf{p})\, 1 , \qquad (37)
```

```math
\mathcal{E}\big(\mathsf{R}(\boldsymbol{\lambda})^{\dagger}\mathbf{k}\big) = \mathcal{E} + (\mathbf{g}\times\mathbf{k})\cdot\boldsymbol{\lambda} + \boldsymbol{\lambda}^{\dagger} K \boldsymbol{\lambda}, \qquad
K = \tfrac12\Big[ \tfrac12\big(\mathbf{g}\mathbf{k}^{\dagger} + \mathbf{k}\mathbf{g}^{\dagger}\big) - (\mathbf{g}\cdot\mathbf{k})\, 1 \Big] + \tfrac12\, X^{\dagger} H X . \qquad (38)
```

Here $`\mathbf{g}`$ and $`H`$ are the first and second derivatives of $`\mathcal{E}`$ with respect to $`\mathbf{k}`$, taken at
$`\mathbf{k}`$, and $`X`$ is the matrix of the cross product with $`\mathbf{k}`$, $`X\mathbf{z} = \mathbf{k}\times\mathbf{z}`$. In (37) the term
linear in $`\boldsymbol{\lambda}`$ is the first-order displacement of the centre, already in $`\mathbf{u}^P`$ of (11), and
$`M`$ is the curvature of the centre's path. In (38) the terms in $`\boldsymbol{\lambda}`$ are the turning of the
product about its own centre.

**The average.** Put (37) and (38) into (36). With the column $`\mathbf{h}^P = B^{P\dagger}\mathbf{k}`$ of §5, the terms of
the phase that are linear in $`\mathbf{v}`$ are $`\mathbf{h}^P\cdot\mathbf{v}`$, so

```math
\mathcal{A}(\mathbf{k}) = e^{i\mathbf{k}\cdot\mathbf{p}}\; \Big\langle \exp\!\big( i\,\mathbf{h}^P\cdot\mathbf{v} + \tfrac{i}{2}\, \boldsymbol{\lambda}^{\dagger} M \boldsymbol{\lambda} \big)\;
   \big( \mathcal{E} + (\mathbf{g}\times\mathbf{k})\cdot\boldsymbol{\lambda} + \boldsymbol{\lambda}^{\dagger} K \boldsymbol{\lambda} \big) \Big\rangle . \qquad (39)
```

The exponent is quadratic in $`\mathbf{v}`$ and $`\mathbf{v}`$ is Gaussian, so (39) is a Gaussian integral, and is
done by completing the square. Only two blocks of $`\Sigma`$ beyond $`U^P`$ are needed: the
covariance of the rotation vector with itself and with the displacement of the centre,

```math
L = \langle \boldsymbol{\lambda}\boldsymbol{\lambda}^{\dagger} \rangle, \qquad
\mathbf{y} = \langle \boldsymbol{\lambda}\, \mathbf{u}^{P\dagger} \rangle\, \mathbf{k}, \qquad
D = (1 - iLM)^{-1} , \qquad (40)
```

with $`D`$ a complex $`3\times 3`$ matrix. From here on some quantities are complex; † on them is
still the transpose, with no complex conjugate. The result has three parts:

```math
\Big\langle \exp\!\big( i\,\mathbf{h}^P\cdot\mathbf{v} + \tfrac{i}{2}\, \boldsymbol{\lambda}^{\dagger} M \boldsymbol{\lambda} \big) \Big\rangle
   = e^{-\frac12 \mathbf{h}^{P\dagger} \Sigma\, \mathbf{h}^P}\; \eta, \qquad
\eta = |1 - iLM|^{-1/2}\; \exp\!\big( -\tfrac{i}{2}\, \mathbf{y}^{\dagger} M D\, \mathbf{y} \big) ; \qquad (41)
```

```math
\langle \boldsymbol{\lambda} \rangle_w = i D \mathbf{y}, \qquad
\langle \boldsymbol{\lambda}\boldsymbol{\lambda}^{\dagger} \rangle_w = W = D L - (D\mathbf{y})(D\mathbf{y})^{\dagger} ; \qquad (42)
```

```math
\mathcal{A}(\mathbf{k}) = e^{i\mathbf{k}\cdot\mathbf{p}}\; e^{-\frac12 \mathbf{h}^{P\dagger} \Sigma\, \mathbf{h}^P}\; \eta\;
   \Big[ \mathcal{E} + \mathbf{g}\cdot\mathbf{q} + \sum_{jl} H_{jl}\, Y_{jl} \Big], \qquad
\mathbf{q} = i\,\mathbf{k}\times(D\mathbf{y}) + \tfrac12\big[ W\mathbf{k} - (\mathrm{tr}\,W)\,\mathbf{k} \big], \qquad
Y = \tfrac12\, X W X^{\dagger} . \qquad (43)
```

In (41) the first factor is the rigid-body part of the temperature factor of (13), and $`\eta`$
is what the curvature of the path adds. In (42) $`\langle\;\rangle_w`$ is the average with the
complex weight of (41), divided by (41): the weight acts on powers of $`\boldsymbol{\lambda}`$ as a Gaussian
with a complex mean and covariance. (43) follows from (39) with (41) and (42), the products
with $`\mathbf{g}`$ and $`H`$ having been collected.

**The structure factor** is therefore (15) with each term $`t^{\mu\nu}_{ab}`$ multiplied by $`\eta`$ and its
polynomial $`\mathcal{E}`$ replaced by the square bracket of (43). The stiff modes keep their part of the
temperature factor.

**The derivatives with respect to $`\Sigma`$.** $`\Sigma`$ now enters a term in three ways: through the
temperature factor, as before; through $`\mathbf{y}`$; and through $`L`$. The first gives (22) again,
with the averaged terms in the sums (21). For the other two, number the coordinates of $`\mathbf{v}`$
so that $`\lambda_m`$ is coordinate $`3+m`$. Then, by (40),

```math
y_m = \sum_q \Sigma_{3+m,\,q}\; h^P_q , \qquad L_{mn} = \Sigma_{3+m,\,3+n} , \qquad (44)
```

and the chain rule adds to (22)

```math
\sum_{\mu\nu} D_{\mu\nu} \sum_{ab} \Big[ \sum_m \frac{\partial t^{\mu\nu}_{ab}}{\partial y_m}\, \frac{\partial y_m}{\partial \Sigma_{pq}}
   + \sum_{mn} \frac{\partial t^{\mu\nu}_{ab}}{\partial L_{mn}}\, \frac{\partial L_{mn}}{\partial \Sigma_{pq}} \Big] . \qquad (45)
```

By (44) $`\partial y_m/\partial\Sigma_{pq}`$ is $`h^P_q`$ if $`p = 3+m`$, plus $`h^P_p`$ if $`q = 3+m`$ and $`q \ne p`$,
and $`h^P = w_A h^A + (1-w_A) h^B`$ is linear in $`w_A`$; so the derivatives of the terms with
respect to $`\mathbf{y}`$ are summed over the primitives with the weights 1 and $`w_A`$, as in (21), and
those with respect to $`L`$ with the weight 1. The derivatives of one term with respect to $`\mathbf{y}`$
and $`L`$ are found by working back through (43), (42), (41) and (40) from the term to $`\mathbf{y}`$ and
$`L`$, one step at a time.

- **With $`L = 0`$ it is (15)**: then $`D = 1`$, $`\eta = 1`$, $`\mathbf{y} = 0`$ and $`W = 0`$.
- **The turning does not average to nothing**, although $`\langle\boldsymbol{\lambda}\rangle = 0`$: a turn is correlated
  with the displacement of the centre, which is what $`\mathbf{y}`$ holds.
- **For a product of two s functions** $`\mathcal{E}`$ does not depend on $`\mathbf{k}`$, and only $`\eta`$ is left.
- **All six coordinates are averaged together.** The numerical average of §7 takes the principal
  coordinates one at a time. That leaves out, at second order, the products of two different
  coordinates in (37) and (38), which do not average to zero with the complex weight. At low
  angle the two averages agree; at high angle the formula is the more accurate (§11).
- **Cost.** A complex $`3\times 3`$ inverse for each Gaussian product at each reflection, and the
  first and second derivatives of the polynomials. No SCF and no displaced geometry.
