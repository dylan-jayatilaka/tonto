# A sum of Gaussians for 1/r: where it wins

The Coulomb kernel $`1/r`$ can be written, to any chosen accuracy, as a short sum of Gaussians.
This page says what that does and does not buy in a Gaussian-basis code: it is a loss wherever
integrals are evaluated analytically between Gaussian functions, and a gain wherever the Coulomb
operator is *applied to a function* held on a grid — which is where the multiresolution, plane-wave
and tensor-hypercontraction methods live. It also answers a related question: whether a Gaussian
kernel would make the overlap-fitted quadrature of the chain-of-spheres exchange (COSX) exact. It
would not.

## 1. The expansion

Start from the identity

```math
\frac{1}{r} = \frac{2}{\sqrt{\pi}} \int_{-\infty}^{\infty} \exp\!\left(-r^{2} e^{2s} + s\right) ds , \qquad (1)
```

and apply the trapezoid rule in $`s`$ with step $`h`$. The result is

```math
\frac{1}{r} \approx \sum_{k=1}^{M} c_k\, e^{-t_k r^{2}}, \qquad t_k = e^{2 s_k}, \qquad c_k = \frac{2h}{\sqrt{\pi}}\, e^{s_k}, \qquad (2)
```

with the exponents $`t_k`$ on a logarithmic ladder. The integrand of (1) is analytic in a strip of
half-width $`\pi/4`$, so the error of the trapezoid rule falls as $`\exp(-\pi^{2}/2h)`$: the relative
error is uniform in $`r`$, oscillates in $`\log r`$, and halving it costs a fixed number of extra
terms. Beylkin and Monzón's reduction then removes about half the terms without loss. For a
relative accuracy $`\varepsilon`$ over $`r`$ from 0.01 to 50 bohr, the range that matters between
Gaussian charge distributions in a molecule:

| $`\varepsilon`$ | terms, trapezoid | terms after reduction, about |
|---|---:|---:|
| $`10^{-4}`$ | 37 | 20 |
| $`10^{-6}`$ | 68 | 35 |
| $`10^{-8}`$ | 107 | 55 |
| $`10^{-10}`$ | 156 | 80 |

The count grows as $`\log(1/\varepsilon)\,[\log(r_{\max}/r_{\min}) + \log(1/\varepsilon)]`$ and
depends weakly on the range: Harrison and co-workers quote 52 Gaussians for $`10^{-8}`$ on
$`[10^{-5}, 1]`$, Beylkin and Monzón 89 for $`[10^{-9}, 1]`$. Tables of best approximations are
Braess and Hackbusch's. In the language of Beylkin and Mohlenkamp, (2) is a *separated
representation* of the Coulomb operator of rank $`M`$: each term is a product of a function of $`x`$,
one of $`y`$ and one of $`z`$, because $`e^{-t|\mathbf r_1 - \mathbf r_2|^2}`$ factorises over the
Cartesian directions and $`1/r`$ never does.

## 2. What it does to analytic integrals: a loss

For a Gaussian pair of exponent $`p`$ at centre $`\mathbf P`$, one term of the kernel gives a
potential that is again a Gaussian,

```math
\int e^{-p|\mathbf r_1 - \mathbf P|^{2}}\, e^{-t|\mathbf r_1 - \mathbf r_2|^{2}}\, d\mathbf r_1
 = \left(\frac{\pi}{p+t}\right)^{3/2} \exp\!\left(-\frac{pt}{p+t}\,|\mathbf r_2 - \mathbf P|^{2}\right) , \qquad (3)
```

and the two-electron integral between two $`s`$ pairs, exponents $`p`$ and $`q`$ at $`\mathbf P`$
and $`\mathbf Q`$, is elementary — the Gaussian-geminal integral, with no Boys function:

```math
\iint e^{-p|\mathbf r_1-\mathbf P|^2} e^{-q|\mathbf r_2-\mathbf Q|^2} e^{-t|\mathbf r_1-\mathbf r_2|^2}\, d\mathbf r_1 d\mathbf r_2
 = \frac{\pi^{3}}{(pq+pt+qt)^{3/2}} \exp\!\left(-\frac{pqt}{pq+pt+qt}\,|\mathbf P-\mathbf Q|^{2}\right) . \qquad (4)
```

Higher angular momentum follows by the usual recursions. The exact integral costs one Boys
function, or in Rys form one to five quadrature roots; the expanded integral costs one exponential
per term, so 35 to 55 terms for $`10^{-6}`$ to $`10^{-8}`$. For four-index integrals, and equally
for the three-index pair potentials that COSX needs, the expansion is **10 to 50 times slower**.
The Boys function is the exact resummation of the ladder; Rys quadrature is already the compact
form, and nothing is gained by unrolling it.

## 3. What it does to an operator: separability

The gain appears when the Coulomb operator acts on a function $`f(\mathbf r)`$ held on a grid, that
is, when the potential $`\int f(\mathbf r')/|\mathbf r - \mathbf r'|\, d\mathbf r'`$ is wanted
rather than a matrix element between Gaussians. Two properties of (2) matter.

**Each term is a separable convolution.** On a grid of $`n^{3}`$ points a general convolution costs
$`n^{6}`$ operations; a Gaussian convolution is three one-dimensional convolutions, one per axis,
and costs $`3n^{4}`$. With $`M`$ terms the Coulomb potential of $`f`$ costs $`3Mn^{4}`$, which for
$`n = 100`$ and $`M = 40`$ is $`10^{10}`$ operations. This is the fast Gauss transform of Greengard
and Strain applied term by term.

**The ladder separates scales.** Tight terms (large $`t_k`$) have a potential that reaches only a
short distance, so their contribution is local and screens to linear scaling. Diffuse terms (small
$`t_k`$) give a potential so smooth that a coarse grid, or a reciprocal-space sum, carries it
exactly. Ewald's split of $`1/r`$ into $`\mathrm{erfc}(\sqrt\omega\, r)/r`$, summed in real space,
and $`\mathrm{erf}(\sqrt\omega\, r)/r`$, summed in reciprocal space, is this ladder with two bands;
the Gaussian sum is Ewald at every scale at once. In a periodic system the diffuse end of the
ladder is what the reciprocal-space sum does, and a uniform grid with a fast Fourier transform
integrates it without error because it is band-limited.

## 4. Where it wins

### 4.1 Multiresolution: MADNESS

Harrison, Fann, Yanai, Gan and Beylkin hold orbitals and densities in an adaptive multiwavelet
basis and apply the Coulomb operator in the separated form (2): a rank-$`M`$ sum of separable
convolutions, each applied cheaply at every level of the multiresolution tree. The adaptivity puts
fine boxes near the nuclei, so the method is all-electron and converges the energy to a set
precision with no basis set, routinely to microhartree. The price is a large prefactor, and
Hartree–Fock exchange, which needs the operator applied to every occupied orbital pair (Yanai and
co-workers), is the expensive step there too. This is the clearest case of the method winning:
there is no Gaussian basis, so there are no analytic integrals to compete with, and the separated
operator is what makes the approach feasible at all.

### 4.2 The Coulomb matrix on grids

The Gaussian-and-plane-wave method of Lippert, Hutter and Parrinello, and Quickstep in CP2K, put
the density on a uniform grid and solve Poisson's equation by Fourier transform — the diffuse end
of the ladder done in reciprocal space, with the sharp core density handled separately (the
augmented variant). Füsti-Molnár and Pulay's Fourier-transform Coulomb method makes the same split
in a molecular code: a smooth part of the density goes to the grid, the sharp part stays analytic.
Both are linear scaling for the Coulomb matrix. In a code with density-fitted J, as here, this
end of the problem is already cheap, and the gain would appear only for systems far larger than a
seven-molecule cluster.

### 4.3 Tensor hypercontraction: the form of $`Z`$

Tensor hypercontraction (THC; Hohenstein, Parrish and Martínez) writes the whole four-index tensor
through a grid. With the collocation matrix $`X_{\mu g} = \chi_\mu(\mathbf r_g)`$ of the basis
functions at $`N_g`$ points,

```math
(\mu\nu|\lambda\sigma) \approx \sum_{g g'} X_{\mu g} X_{\nu g}\, Z_{g g'}\, X_{\lambda g'} X_{\sigma g'} , \qquad (5)
```

so every product of two basis functions is represented by its values on the grid, and $`Z`$ is a
kernel between grid points. The least-squares form (Parrish, Hohenstein, Martínez and Sherrill)
chooses $`Z`$ to minimise the squared error of (5) over all four indices, which gives

```math
Z = S^{-1} E\, S^{-1}, \qquad S_{g g'} = \Big[\sum_\mu X_{\mu g} X_{\mu g'}\Big]^{2}, \qquad
E_{g g'} = \sum_{\mu\nu\lambda\sigma} X_{\mu g} X_{\nu g}\, (\mu\nu|\lambda\sigma)\, X_{\lambda g'} X_{\sigma g'} . \qquad (6)
```

$`S`$ is the grid's own overlap of pair products, and $`E`$ is the exact Coulomb interaction of the
pair densities concentrated at $`g`$ and $`g'`$; in practice $`E`$ is formed through density
fitting. The interpolative form (ISDF; Lu and Ying, and for molecules Lee, Lin and Head-Gordon)
reaches the same structure from the other end: it finds interpolating functions $`\zeta_g(\mathbf r)`$
such that $`\chi_\mu(\mathbf r)\chi_\nu(\mathbf r) \approx \sum_g \zeta_g(\mathbf r) X_{\mu g} X_{\nu g}`$
for every pair, picks the points $`\mathbf r_g`$ by a pivoted QR factorisation of the pair matrix,
and sets

```math
Z_{g g'} = \iint \zeta_g(\mathbf r_1)\, \frac{1}{|\mathbf r_1 - \mathbf r_2|}\, \zeta_{g'}(\mathbf r_2)\, d\mathbf r_1 d\mathbf r_2 . \qquad (7)
```

Here the Gaussian ladder earns its place: with $`\zeta_g`$ on a uniform grid, (7) is $`M`$
separable convolutions, exact for the model kernel, with no auxiliary basis and no four-index
integrals. The number of points is a small multiple of the basis size, against several hundred per
basis function in a COSX grid, because the interpolation, not a quadrature, carries the accuracy.

Exchange then costs two matrix products and one elementwise product,

```math
K_{\mu\nu} = \sum_{g g'} X_{\mu g}\, \big(X^{T} P X\big)_{g g'}\, Z_{g g'}\, X_{\nu g'} , \qquad (8)
```

that is $`O(N_g^{2} N + N_g N^{2})`$ per build for $`N`$ basis functions. For $`N = 1000`$ and
$`N_g = 10\,000`$ that is about $`10^{11}`$ operations, tens of seconds on one core; forming $`Z`$
once per geometry is the larger cost, and whether the whole wins over COSX depends on that
prefactor. The method's largest wins are in correlated theory: with (5) the MP2, CC2 and
coupled-cluster tensors factorise, and Martínez and co-workers reduce CC2 to quartic scaling.

### 4.4 COSX, pseudospectral methods and overlap fitting, in the same language

COSX (Neese, Wennmohs, Hansen and Becker) is (8) with one of the two grid sums done analytically:

```math
K_{\mu\nu} = \sum_g w_g\, X_{\mu g} \sum_{\lambda\sigma} P_{\lambda\sigma} X_{\sigma g}\, A_{\lambda\nu}(\mathbf r_g),
\qquad A_{\lambda\nu}(\mathbf r_g) = \int \frac{\chi_\lambda(\mathbf r)\chi_\nu(\mathbf r)}{|\mathbf r - \mathbf r_g|}\, d\mathbf r , \qquad (9)
```

with $`w_g`$ the grid weights. The pair potentials $`A`$ play the part of $`\sum_{g'} Z_{gg'} X_{\lambda g'} X_{\nu g'}`$,
exactly; what is approximate is the remaining quadrature over $`g`$. Overlap fitting (Izsák and
Neese) replaces $`w_g X_{\mu g}`$ by $`\big(S\,S_{\mathrm{num}}^{-1} X\big)_{\mu g}`$, with
$`S`$ the exact overlap matrix and $`S_{\mathrm{num}} = X W X^{T}`$ its quadrature, so that the
quadrature is exact for every product of two basis functions. That is the $`S^{-1}`$ factor of
(6) applied on one side only, which is why the fitted $`K`$ is not symmetric and is symmetrised.
Friesner's pseudospectral method is the same construction with *dealiasing functions* added to the
fitting set, extra functions that span part of what the potential term reaches outside the
basis; Izsák, Neese and Klopper tried the complementary auxiliary basis of explicitly correlated
theory in the same role, and found the best scheme to be a generalisation of overlap fitting.

Against this background the question of a Gaussian kernel answers itself. By (3) the pair
potential under a Gaussian kernel is a sum of Gaussians at the pair centres with exponents
$`p\,t_k/(p+t_k)`$, broader than any basis product. It is smooth, but it is not in the space the
overlap fit makes exact, so the quadrature error is unchanged; the exact potential
$`\mathrm{erf}(\sqrt p\, r)/r`$ was never rough either. The fit could be made exact by adding those
Gaussians to the fitting set, but there are pairs × terms of them, and compressing that set is
density fitting under another name.

## 5. Energy-based physics with a modified Hamiltonian

Using the kernel (2) consistently, in every integral, gives a model Hamiltonian, and the
self-consistent field is variational for it. With $`\Delta k(r)`$ the difference between the model
kernel and $`1/r`$, $`\rho`$ the density and $`\gamma`$ the one-particle density matrix, the energy
error is first order,

```math
\Delta E \approx \frac{1}{2} \iint \Big[\rho(\mathbf r_1)\rho(\mathbf r_2) - \frac{1}{2}|\gamma(\mathbf r_1,\mathbf r_2)|^{2}\Big]\, \Delta k(r_{12})\, d\mathbf r_1 d\mathbf r_2 , \qquad (10)
```

so about $`\varepsilon`$ times the electron repulsion energy, which is some hundreds of hartree for
a molecule of a few dozen atoms: $`\varepsilon = 10^{-6}`$, about 35 terms, gives absolute errors of
a tenth of a millihartree to a millihartree, and the equioscillating error of (2) cancels part of
that. Energy differences carry the error only in the part of the repulsion energy that changes,
and the density error is smaller still. This is a sound footing for thermochemistry, for
geometries and for densities, and it is the footing MADNESS stands on. In a periodic system the
same kernel splits naturally: the tight terms in real space with finite lattice sums, the diffuse
terms in reciprocal space, so that the one expansion serves both the molecule and the crystal.

What it does not change is the cost of a Gaussian-basis code, where every two-electron quantity
is an analytic integral and §2 applies. The expansion pays only when the code is reorganised
around applying the operator — §4.1, §4.2 and the construction of $`Z`$ in §4.3.

## 6. Periodic molecular systems

In a molecular crystal the Coulomb matrix needs the lattice sum and gets it from Ewald, which is
the two-band ladder. Exchange is different: the density matrix of an insulator decays
exponentially, within a few ångström for a molecular crystal, so the $`1/r`$ tail in $`K`$ is cut
off by $`\gamma`$ and no reciprocal-space sum is needed for it. The difficulty a grid has with
exchange is therefore at middle range — pair potentials integrated on points several bohr from
their own nucleus, within the decay length of $`\gamma`$ but where density-tuned grids have pruned
their angular points — and a lattice sum does not touch it. What does is a range split: the
short-range $`\mathrm{erfc}`$ part of the kernel in COSX, where the pair potentials are compact and
the quadrature error local, and the long-range $`\mathrm{erf}`$ part for the orbital-pair densities
on a uniform grid, where it is exact. That is the structure of exchange in plane-wave codes
(Gygi and Baldereschi; Spencer and Alavi) and of the truncated-Coulomb exchange of CP2K (Guidon,
Hutter and VandeVondele).

## 7. Bearing on Tonto

- Density-fitted J and the Rys integrals stay as they are; a Gaussian kernel would slow both.
- COSX's error is a quadrature error on a potential that is already smooth, and a Gaussian kernel
  does not reduce it. Grids made for the exchange integrand (Helmich-Paris, de Souza, Neese and
  Izsák) do. For Hirshfeld atom refinement the deciding number is that RI-J with COSX reproduces
  exact refinements of a seven-molecule cluster to 0.0004 in the goodness of fit, below anything the
  refinement can see.
- If exchange becomes the limit for larger clusters, the one idea here worth a planned trial is
  tensor hypercontraction in its interpolative form, with $`Z`$ built by the ladder: fewer points by
  an order of magnitude, an interpolation error in place of a quadrature error, and a once-per-
  geometry setup whose cost decides the matter.

## References

- G. Beylkin and L. Monzón, *Appl. Comput. Harmon. Anal.* 19, 17 (2005); 28, 131 (2010).
- G. Beylkin and M. J. Mohlenkamp, *Proc. Natl. Acad. Sci. USA* 99, 10246 (2002).
- D. Braess and W. Hackbusch, *IMA J. Numer. Anal.* 25, 685 (2005).
- L. Greengard and J. Strain, *SIAM J. Sci. Stat. Comput.* 12, 79 (1991).
- R. J. Harrison, G. I. Fann, T. Yanai, Z. Gan and G. Beylkin, *J. Chem. Phys.* 121, 11587 (2004);
  T. Yanai, G. I. Fann, Z. Gan, R. J. Harrison and G. Beylkin, *J. Chem. Phys.* 121, 6680 (2004).
- G. Lippert, J. Hutter and M. Parrinello, *Mol. Phys.* 92, 477 (1997); *Theor. Chem. Acc.* 103, 124 (1999);
  J. VandeVondele et al., *Comput. Phys. Commun.* 167, 103 (2005).
- L. Füsti-Molnár and P. Pulay, *J. Chem. Phys.* 117, 7827 (2002).
- E. G. Hohenstein, R. M. Parrish and T. J. Martínez, *J. Chem. Phys.* 137, 044103 (2012);
  R. M. Parrish, E. G. Hohenstein, T. J. Martínez and C. D. Sherrill, *J. Chem. Phys.* 137, 224106 (2012);
  E. G. Hohenstein et al., *J. Chem. Phys.* 138, 124111 (2013).
- J. Lu and L. Ying, *J. Comput. Phys.* 302, 329 (2015); J. Lee, L. Lin and M. Head-Gordon,
  *J. Chem. Theory Comput.* 16, 243 (2020).
- F. Neese, F. Wennmohs, A. Hansen and U. Becker, *Chem. Phys.* 356, 98 (2009); R. Izsák and F. Neese,
  *J. Chem. Phys.* 135, 144105 (2011); R. Izsák, F. Neese and W. Klopper, *J. Chem. Phys.* 139, 094111 (2013);
  B. Helmich-Paris, B. de Souza, F. Neese and R. Izsák, *J. Chem. Phys.* 155, 104109 (2021).
- R. A. Friesner, *Chem. Phys. Lett.* 116, 39 (1985); M. N. Ringnalda, M. Belhadj and R. A. Friesner,
  *J. Chem. Phys.* 93, 3397 (1990).
- F. Gygi and A. Baldereschi, *Phys. Rev. B* 34, 4405 (1986); J. Spencer and A. Alavi, *Phys. Rev. B* 77,
  193110 (2008); M. Guidon, J. Hutter and J. VandeVondele, *J. Chem. Theory Comput.* 5, 3010 (2009).
