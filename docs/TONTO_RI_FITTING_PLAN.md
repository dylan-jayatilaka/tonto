# Aspherical form factors by RI density fitting — plan

**Working document** (CLAUDE.md §1): delete it when the item closes, moving whatever is durable
into the user-facing pages. Live status is the entry in `TASKS_AND_HISTORY.md`, *Aspherical form factors by
RI density fitting*.

**Status 2026-09-27: stage (a) written, not yet built or run**, on branch `oc-ri`:
`partition_model= oc-ri`, with `RI_l_max=` and `RI_exponent_ratio=`. How it works is set out
in [How the fitted Hirshfeld atom works](#how-the-fitted-hirshfeld-atom-works) below, written to
be read on its own; the rest of the document is the record of how the plan was reached.

**Two stages (Dylan, 2026-09-27).**

- **(a) An option: fitted Hirshfeld atoms.** For each Hirshfeld atom, fit `w_a rho` in functions
  on that atom only, with the **overlap** metric, and take its form factors from the fit. Useful
  in its own right as a partition option, and it answers the first question: is the fit accurate
  enough? Measure the fitted form factors against the quadrature ones over all reflections, and
  time both.
  - **Auxiliary functions: an even-tempered set** on each atom (exponents in geometric
    progression, up to some L), not a specialised basis -- so it works for any basis, pob-TZVP
    included, where no RI auxiliary set exists.
  - **No Gaussian products.** Each auxiliary function sits on one centre, so the fitted density,
    its value anywhere, and its transform are sums of single Gaussians -- much cheaper than the
    molecular density, which needs products of basis-function pairs. The right-hand side
    `b_P = integral chi_P w_a rho` still needs `rho` on the Becke grid once per fit.
  - For functions on one centre the overlap metric is analytic and block-diagonal in (l,m).
  - Risk to find out first: `w_a rho` has a cusp, and a tail toward the neighbours; functions
    on one centre may need a high L to fit it.
- **(b) Use it in refinement.** The design-matrix derivatives from the same coefficients; the
  effect on refined parameters and their esds; the reflection-weighted metric (section 3);
  per-molecule fits; tests. Only after (a) shows the fit is good enough.

**Literature check, 2026-09-27: a published scheme exists.** Chodkiewicz et al., *Transferable
Hirshfeld atom model for rapid evaluation of aspherical atomic form factors*, IUCrJ 11 (2024)
[lt5065](https://journals.iucr.org/m/issues/2024/02/00/lt5065/index.html). They expand each
Hirshfeld atom as `rho_a(r) = sum_lm R_lm(r) Y_lm(r^)`, get `R_lm` by projection on the atom's
angular grid (no fit), and take form factors by a Fourier-Bessel transform of each `R_lm`. Refined
against unexpanded HAR, max |Delta R(X-H)| is 3.9 / 2.9 / 1.6 / 0.95 mA at L_max 4 / 5 / 6 / 7;
L_max = 7 is their choice. Their speed-up is quoted for whole calculations, not the form-factor step:
"about 50 times" on the larger systems, but their crambin example (642 atoms, 68 s against ~17 min
on 12 cores) is 15x. The method is essentially that of Koritsanszky, Volkov and Coppens (projection
of stockholder atoms onto spherical harmonics). Two consequences for stage (a):
- **L_max must be about 7.** An even-tempered Gaussian set then has ~20 x 64 = 1280 functions per
  atom, so the grid right-hand side `n_pt x n_aux` is about half the cost of the present
  `n_pt x n_k` sum at gly_ala size. The Gaussian route saves little until `n_k` is large.
- **With the overlap metric, the Gaussian fit is this projection with the radial part fitted.**
  The `Y_lm` are orthogonal, so the metric splits by (l,m), and each block fits `R_lm` in Gaussians.
  On the same grid, `b_P` can be taken from the projected `R_lm` on the radial points instead of
  from all points: the same numbers, `n_rad x n_exp` per (l,m) instead of `n_pt x n_exp`. So
  projecting first makes the Gaussian fit cheaper, and shows directly, per l, where a Gaussian set
  fails (cusp, tail). `FOURIER_SUMS:sinc_kr_sums` is already the l = 0 Fourier-Bessel transform.
  Cost estimates per gly_ala atom (arithmetic, not measured) are in section 1a.
The grid points come from radial shells times a Lebedev sphere (`BECKE_GRID:make_atom_grid`), so
grouping points by distance from the nucleus gives the shells the projection needs, pruned or not.

**Re-measured 2026-09-27** (macOS `sample`, release build of `develop`, same job): gly_ala fragHAR
now takes **16.3 s against 66 s**, and the form-factor sum (`FOURIER_SUMS:exp_ikr_sums`) is **33%**
of the samples, against 81.6% in section 1. Fock builds are 23%, the LS normal equations about 9%.
So for a job of this size (2514 reflections) the fit can save at most about a third; the case for
it as a speed-up now rests on large reflection sets, where the n_k x n_pt sum still dominates.
Stage (a) stands as an option on its own merits.

**The proposal (Dylan).** Expand the Hirshfeld atomic densities using the RI machinery already in
the tree rather than writing a fresh multipole/Bessel transform. It must be a **density** fit, not
the Coulomb (potential) fit that regular RI-J does.

Distinct from the *"effect of the fitted density on structure factors"* owed in the RI-J entry of
`TASKS_AND_HISTORY.md`: that concerns structure factors computed from an RI-J **SCF** density; this concerns
fitting `w_a rho` itself so that its Fourier transform becomes analytic.


## How the fitted Hirshfeld atom works

This section is written for a reader who knows what a Hirshfeld atom and a form factor are, and
nothing else about this work. It describes `partition_model= oc-ri`.

### What is computed today

A Hirshfeld atom *a* is the molecular density $\rho$ times the stockholder weight $w_a$. Its
form factor at a scattering vector $\mathbf k$ (with $2\pi$ included, so $|\mathbf k| = 4\pi
\sin\theta/\lambda$) is its Fourier transform, and Tonto evaluates it on the atom's Becke grid,
points $\mathbf r_i$ with weights $W_i$:

$$
f_a(\mathbf k) = \int w_a(\mathbf r)\,\rho(\mathbf r)\,e^{i\mathbf k\cdot\mathbf r}\,d^3r
\;\approx\; \sum_i \rho_{a,i}\, e^{i\mathbf k\cdot\mathbf r_i},
\qquad \rho_{a,i} = W_i\, w_a(\mathbf r_i)\,\rho(\mathbf r_i).
$$

That is one sine and one cosine for every reflection and every grid point: $n_k \times n_{pt}$ of
them per atom, about 23 million for a gly_ala atom (2514 reflections, about 9000 points). It is the
largest single cost of a HAR job.

### The idea

Fit the atom's density in a few hundred functions whose Fourier transforms are known exactly. The
grid is then used once per atom to find the fit, and the form factors come from a formula. No
sine or cosine over the grid is needed.

### The fitting functions

Every function sits on the nucleus of atom *a*, and coordinates are measured from it. Each is a
Gaussian radial part times a real spherical harmonic:

$$
\chi_{nlm}(\mathbf r) = g_{nl}(r)\,Y_{lm}(\hat{\mathbf r}),
\qquad g_{nl}(r) = N_{nl}\, r^l e^{-\alpha_n r^2},
\qquad N_{nl} = \left[\frac{2\,(2\alpha_n)^{l+3/2}}{\Gamma(l+3/2)}\right]^{1/2}.
$$

- The $Y_{lm}$ are the real spherical harmonics, normalised so that $\int Y_{lm}Y_{l'm'}\,d\Omega =
  \delta_{ll'}\delta_{mm'}$. They run from $l = 0$ to $L$ = `RI_l_max=`, which is 7 by default:
  $(L+1)^2 = 64$ angular functions.
- $N_{nl}$ makes $\int_0^\infty g_{nl}^2\, r^2\,dr = 1$.
- The exponents $\alpha_n$ are **even-tempered**: they start at twice the largest basis exponent on
  the atom and are divided by `RI_exponent_ratio=` (2 by default) each time, until they reach the
  smallest basis exponent. The range covers the density, whose tightest part has twice the largest
  basis exponent. This needs no special auxiliary basis, so it works with any orbital basis.

A carbon atom in def2-TZVP gets about 19 exponents, so about $19 \times 64 \approx 1200$ functions.

### The fit

The fitted density is $\tilde\rho_a = \sum_{nlm} c_{nlm}\,\chi_{nlm}$. The coefficients minimise
the squared error integrated over all space, the **overlap norm**:

$$
\int \bigl(\rho_a - \tilde\rho_a\bigr)^2\, d^3r \;\;\text{is least}
\quad\Longleftrightarrow\quad
\mathbf S\,\mathbf c = \mathbf b,
\qquad S_{PQ} = \int \chi_P\,\chi_Q\,d^3r,
\qquad b_P = \int \chi_P\,\rho_a\,d^3r .
$$

**Why this norm.** Parseval's theorem says the same number measures the error in the form factors,
weighted equally at every $\mathbf k$:

$$
\int \bigl|f_a(\mathbf k) - \tilde f_a(\mathbf k)\bigr|^2\, d^3k
= (2\pi)^3 \int \bigl(\rho_a - \tilde\rho_a\bigr)^2\, d^3r .
$$

So the fit is as good at high resolution as at low. The Coulomb norm used for RI-J in the SCF
weights the error by $1/k^2$, which makes the fit worst at high resolution, where a charge-density
refinement needs it most. That is why this fit does not reuse the RI-J metric.

### The overlap matrix is small and exact

The angular integral of two harmonics is zero unless $l = l'$ and $m = m'$. So $\mathbf S$ falls
apart into one small block for each $l$, the same for every $m$:

$$
S_{nlm,\,n'l'm'} = \delta_{ll'}\,\delta_{mm'}\; s^{(l)}_{nn'},
\qquad
s^{(l)}_{nn'} = \int_0^\infty g_{nl}\,g_{n'l}\,r^2\,dr
= \left(\frac{2\sqrt{\alpha_n\alpha_{n'}}}{\alpha_n + \alpha_{n'}}\right)^{l+3/2}.
$$

With about 20 exponents, that is eight $20\times 20$ matrices for $L = 7$. Each is factored once
by Cholesky ($s^{(l)} = \mathbf L\mathbf L^{\mathsf T}$) and used for all $2l+1$ values of $m$.
How well conditioned they are depends only on the exponent ratio: a ratio near 1 makes neighbouring
functions nearly the same, and the matrix nearly singular.

### The right-hand side, and the spherical-harmonic projection inside it

$b$ is the only step that needs the grid:

$$
b_{nlm} \approx \sum_i \rho_{a,i}\; g_{nl}(r_i)\; Y_{lm}(\hat{\mathbf r}_i).
$$

The Becke grid of an atom is a set of radial shells, each a Lebedev sphere of directions: point *i*
is shell *j* at radius $R_j$, direction $\hat{\mathbf q}$. Summing over the directions of each shell
first:

$$
b_{nlm} = \sum_j g_{nl}(R_j)\; P_{lm}(j),
\qquad
P_{lm}(j) = \sum_{\hat{\mathbf q}\,\in\,\text{shell } j} \rho_{a,j\hat{\mathbf q}}\; Y_{lm}(\hat{\mathbf q}).
$$

$P_{lm}(j)$ is the **projection of the atom onto spherical harmonics**. It is the radial function
$\rho_{lm}(R_j) = \int \rho_a(R_j\hat{\mathbf q})\,Y_{lm}(\hat{\mathbf q})\,d\Omega$ times the
shell's radial weight. This is the expansion of Koritsanszky, Volkov and Coppens, and of Chodkiewicz
et al. (2024). Grouping by shell reorders the same sum, so it gives exactly the same $b$.

Seen this way, the fit for each $(l,m)$ is a one-dimensional least-squares fit of the radial
function $\rho_{lm}(r)$ by the Gaussians $g_{nl}(r)$, with weight $r^2\,dr$. The published
method uses $\rho_{lm}$ itself, on the radial grid; this one replaces it by a Gaussian fit. With a
complete set of Gaussians the two would agree.

Two ways to compute $b$:

| | work per atom | gly_ala atom |
|---|---|---|
| point by point (what the code does now) | $n_{pt} \times n_\alpha$ exponentials, $n_{pt} \times n_\alpha (L+1)^2$ multiply-adds | ~11 M |
| projection first, then the radial sums | $n_{pt} \times (L+1)^2$, then $n_{shell} \times n_\alpha (L+1)^2$ | ~0.6 M |

The second is about 20 lines more code. It also shows, for each $l$, where a set of Gaussians fails
to fit: at the nuclear cusp, or in the tail.

### The transform of the fit is a formula

A Gaussian times a spherical harmonic transforms into the same harmonic, now in the direction of
$\mathbf k$:

$$
\int e^{i\mathbf k\cdot\mathbf r}\; r^l Y_{lm}(\hat{\mathbf r})\, e^{-\alpha r^2}\, d^3r
= \left(\frac{\pi}{\alpha}\right)^{3/2}
\left(\frac{i}{2\alpha}\right)^{l}
k^l\, Y_{lm}(\hat{\mathbf k})\; e^{-k^2/4\alpha}.
$$

For $l = 0$ this is the familiar transform of a Gaussian. Each power of $\mathbf r$ brings
down a factor $i\mathbf k/2\alpha$, which gives the rest. So, putting the atom back at its position
$\mathbf r_a$:

$$
\tilde f_a(\mathbf k) = e^{i\mathbf k\cdot\mathbf r_a}
\sum_{l=0}^{L} i^l \sum_{m=-l}^{l} k^l\,Y_{lm}(\hat{\mathbf k})
\sum_n c_{nlm}\, N_{nl}\left(\frac{\pi}{\alpha_n}\right)^{3/2}(2\alpha_n)^{-l}\, e^{-k^2/4\alpha_n}.
$$

Per reflection this needs $n_\alpha$ exponentials, which are shared by every $l$ and $m$, and one
sine and cosine for the phase. The rest is multiplication and addition.

### Cost

Arithmetic estimates for one gly_ala atom, not yet measured:

| step | estimate |
|---|---|
| today's sum over the grid | 70–230 ms |
| $b$, point by point | ~10 ms |
| $b$ by projection | ~1 ms |
| overlap matrices and Cholesky | microseconds |
| the analytic transform | ~3 ms |

The density on the grid has to be computed whichever way the form factors are taken, so it is left
out.

### Where it is in the code

| piece | procedure |
|---|---|
| fit and transform | `FOURIER_SUMS:fitted_exp_ikr_sums` |
| spherical harmonics at points and at $\mathbf k$ | `FOURIER_SUMS:make_solid_harmonics`, from `GAUSSIAN_DATA:spherical_harmonics_for`, normalised exactly by `make_normalised_harmonics` |
| the exponents | `MOLECULE.RHO:make_RI_exponents` |
| the choice between the sum over points and the fit | `MOLECULE.RHO:make_atom_FFs_on_grid`, called by the three Hirshfeld form-factor routines in `MOLECULE.RHO` and by `MOLECULE.HAR:make_LS_mx` |
| spherical harmonics up to `RI_l_max` | `GAUSSIAN_DATA:set_indices`, called from `MOLECULE.SCF:make_atom_partition_info` |

### What is still to be found out

- How closely $\tilde f_a$ matches the quadrature $f_a$, by resolution shell, and how that depends
  on `RI_l_max=` and `RI_exponent_ratio=`.
- The effect on refined parameters and their esds, which is what matters in the end. The
  published expansion needed $L = 7$ to keep bonds to hydrogen within 1 mÅ of unexpanded HAR.
- Whether the Gaussians fit the nuclear cusp well enough at the default exponent range.


## 1. Why — the measurement

First profile of a fragHAR job: 2026-09-24, macOS `sample`, release build,
`tests/long/gly_ala_fragHAR_rhf_STO-3G`, 4965 samples over a 66 s run.

| | share |
|---|---|
| libm `sin` / `cos` / `exp` / `cexp`, all from one line | 81.6% |
| Fock build (Rys, `shell1quartet`) + grid density | 8.0% |
| LS normal equations solve | ~2% |
| all disk I/O, archive writes and `open` included | <=2.2% |

The line is `foofiles/molecule.rho.foo:6120`, in the `do k / do i` loop of
`MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk`:

```
do k = 1,n_k          ! 2514 reflections
   do i = 1,n_pt      ! ~9000 grid points for this atom
      kr = k1*pt(i,1)+k2*pt(i,2)+k3*pt(i,3)
      sf = sf + rho(i)*exp(IMAGIFY(kr))
   end
end
```

`n_k x n_pt` complex exponentials per atom. Three call sites of that loop are 77.4% of all samples.

**Two conclusions that shaped the plan.** The ASF quadrature *is* the cost of a fragHAR job, so
RI-J and COSX cannot fix it — the whole Fock build is 8%, and an infinitely fast SCF saves 8%.
Neither is disk the cost: the `per_rank_write` subtree is 4 samples of 4965, against a guess that
it might be significant.


## 1a. Cost estimates per atom, gly_ala (2026-09-27, arithmetic only, not measured)

Assumed: `n_pt` ~ 9000 (about 45 radial shells x 194 Lebedev points, `l_angular_grid` 23);
`n_k` = 2514; L_max = 7, so 64 (l,m); 20 even-tempered exponents, so 1280 Gaussians. Evaluating
the density on the grid is the same for every route and is left out.

| step | work | estimate |
|---|---|---|
| today: `exp_ikr_sums` | 22.6 M sin/cos pairs | 70-230 ms |
| projection onto `Y_lm` | 64 `Y_lm` per point, 0.6 M multiply-adds | ~1 ms |
| Gaussian `b_P` directly on the grid | 0.2 M `exp`, 11.5 M multiply-adds | ~10 ms |
| Gaussian `b_P` from the projection | 64 x 45 x 20 = 58 k | negligible |
| overlap metric and Cholesky | 8 blocks of 20 x 20, closed form | microseconds |
| Fourier-Bessel transform | 113 k `j_l` sets, 7.2 M multiply-adds | 5-10 ms |
| Gaussian transform | 0.4 k x 8 radial factors per k, 3.2 M multiply-adds | ~3 ms |

Both routes are 10x or more below today's sum, and differ from each other by a few ms. Speed does not
choose between them; accuracy per basis choice, and what the functions are wanted for, do.


## 2. The scheme

For each Hirshfeld atom `a`, fit the Hirshfeld atomic density in an atom-centred auxiliary basis:

```
rho_a(r) = w_a(r) rho(r)  ~  sum_P c_P^a chi_P(r)
```

The transform is then analytic and linear in the coefficients:

```
f_a(k) = sum_P c_P^a chi~_P(k)
```

Per atom: `n_aux x n_pt` to build the right-hand side on the grid, an `O(n_aux^2)` solve, then
`n_aux x n_k` analytic transform in which only about `n_shell_aux x n_k` exponentials appear — the
radial factor `exp(-k^2 / 4 alpha)` depends on the shell, not on the component. The `n_k x n_pt`
transcendentals disappear.

**The win grows with the reflection count**, which is the regime that matters. Grid work goes from
`n_k x n_pt` to `n_aux x n_pt`, a factor `n_k / n_aux`: gly_ala is `n_k` = 2514 against a few
hundred auxiliary functions per fragment, so 6-12x; a protein at 1e5 reflections is 1e2-1e3x. And
`w_a rho` is localised on atom `a`, so a local auxiliary basis keeps `n_aux` small.

**The right-hand side stays on the grid.** `w_a` is a ratio of promolecule densities, not a
Gaussian, so `b_P = integral chi_P w_a rho` cannot be an analytic integral. That is affordable —
it is `n_aux x n_pt`, not `n_k x n_pt` — but the Becke grid does not go away and grid accuracy
still matters.

**Per atom first, per molecule later** (Dylan, 2026-09-26). The fit is per Hirshfeld atom to begin
with; the X-ray work will later want one fit of the whole molecular density. Per atom, the metric
is small (a few hundred functions on one centre) and cheap. Per molecule it is one metric over every
auxiliary function -- AuCarbene4-sized, 3012 x 3012 for 104 atoms in univ-JFIT -- and then building
it, transforming it and factoring it all matter: on achari2 (netlib BLAS) the Cholesky factor of
that size is 3.7 s, the Coulomb integrals 0.5 s, the spherical transform 0.1 s (section 4).


## 3. The metric is the whole point

The fit minimises a norm of `Delta rho = rho_a - rho~_a`. Because the Coulomb kernel is
`4 pi / k^2` in Fourier space, **the choice of metric is the choice of which reflections the fit is
accurate for.** This is why it must not be the RI-J metric.

| metric | minimises | consequence |
|---|---|---|
| **Coulomb** (RI-J's) | `integral abs(Delta rho~(k))^2 / k^2 dk` | errors at large `k` weighted *down* by `1/k^2` — least accurate exactly at high resolution, where a charge-density refinement needs it most. Wrong here. |
| **Overlap** | `integral abs(Delta rho~(k))^2 dk` | uniform in `k`. One new metric builder; everything else reused. |
| **Reflection-weighted** | `sum_hkl w_hkl abs(Delta f_a(k_hkl))^2` | optimal for the quantity actually being refined. |

The third deserves attention because it is cheaper than it looks. Its normal matrix

```
A_PQ = sum_k w_k conjg(chi~_P(k)) chi~_Q(k)
```

is symmetric positive-definite and depends only on the geometry and the hkl list — **not on the
density** — so it is built and factored **once per geometry** and reused across every SCF and LS
iteration, exactly as `.RI_metric_factor` is today. Building it is `n_aux^2 x n_k`, which is the
reason to amortise it, not a reason to avoid it.

All three metrics are SPD, so the existing factorisation and solve are reused unchanged whichever
is chosen.


## 4. How RI-J solves for the coefficients today: Cholesky, not LU

Asked during the 2026-09-24 discussion, so recorded here.

`MOLECULE.FOCK:initialize_RI_J` calls `make_RI_metric`, which builds the Coulomb metric over the
auxiliary pair list in cartesian functions and transforms it to spherical ones a shell pair at a
time (`make_spherical_metric`), and then `.to_cholesky_factor` — once per geometry. (Until
2026-09-26 the transform was two dense matrix products: 240 of 244 s on AuCarbene4. A per-molecule
overlap metric must be transformed the same blockwise way.) Each iteration then does one `.RI_metric_factor.solve_cholesky_equation(ds,cs)`
(`:2084`), a forward/back substitution, `O(n_aux^2)`.

**Keep Cholesky.** It is the right tool for an SPD metric: half the work of LU, and no pivoting.

One caveat carries a risk for this work: overlap metrics are less well conditioned than Coulomb
ones, so a near-singular auxiliary set could make a bare Cholesky fail where RI-J's does not. A
pivoted or eigenvalue-truncated fallback may be owed, and the condition number should be
**reported** rather than discovered.


## 5. What already exists

This is mostly assembly, which is the argument for this route over a fresh multipole-Bessel
implementation.

| piece | where | note |
|---|---|---|
| auxiliary basis reading, pair list | `MOLECULE.FOCK:resolve_auxiliary_bases`, `make_auxiliary_pair_list` | each auxiliary primitive is already a **one-centre (L,0) pair** |
| analytic Gaussian transform | `SHELL2:make_ft_static` / `make_ft_c` / `make_ft_component`, `foofiles/shell2.foo:493` | computes the ordinary molecular structure factors, and a one-centre (L,0) pair is exactly what it takes — so **no new integral code** for the transform of the fitted density |
| Cholesky factor and solve | `MAT{REAL}:to_cholesky_factor`, `solve_cholesky_equation` | reused unchanged |
| auxiliary values on the Becke grid | `SHELL1:make_grid` | for the right-hand side |
| metric setup | `MOLECULE.FOCK:make_RI_metric`, `make_spherical_metric` | the auxiliary basis, pair list and shell-pair spherical transform are the same for any metric; give it a choice of metric, since only the integral step differs |

**New work**: the metric builder (`GAUSSIAN_PAIR_LIST:make_coulomb_metric` is the template), the
grid right-hand side `b_P`, and the contraction of `c_P` with the analytic transform.


## 6. What must be measured before it is believed

- **Fit error on the reflection set, per metric.** The honest comparison is fit error against
  *quadrature* error at equal cost, not against exact — the present numbers carry quadrature error
  of their own.
- **Auxiliary basis adequacy** for `w_a rho`, which has a nuclear cusp and kinks where the weight
  function turns over. Whether a `def2-universal-jfit`-shaped set is flexible enough is open, and
  auxiliary bases for the crystal bases (pob-TZVP) do not exist at all — already owed for RI-J.
- **The effect on refined parameters and their esds**, not just on `f_a`. This feeds the LS design
  matrix, so a smooth systematic fit error is more dangerous than a noisy one.
- **That the ASF derivatives** for the design matrix come out analytically from the same
  coefficients, as they should.


## 7. Three cheap wins in the present loop, independent of all this

Worth taking first; they are small and they do not depend on any of the above.

1. **`exp(IMAGIFY(kr))` -> two real accumulators with `cos` / `sin`.** Tried 2026-09-24 and
   **reverted** (`dace267a`): performance-neutral — 33.27 s against 32.68 s, inside the noise — and
   it perturbs the last digits of every HAR job, so it was not worth a re-bless on its own. The loop
   exists in seven copies (six in `molecule.rho.foo`, one in `molecule.har.foo`); factor them into
   one routine before restructuring.

   **The prediction was wrong, and how it was wrong is the lesson.** It was forecast at 12.6% by
   attributing `exp` samples to a `cexp` parent in the `sample` call tree. That attribution is not
   trustworthy: libm symbol resolution in these profiles is partial — many frames come back as
   `??? (in libsystem_m.dylib) load address ... + 0x1170`, and "cexp" even appears to call itself —
   so samples inside one libm helper get charged to whichever exported symbol precedes them. Apple's
   `cexp` evidently already fast-paths a zero real part, and computing `sin` and `cos` separately
   costs about what one `cexp` did. **Trust wall-clock for libm-bound loops, not per-symbol shares
   from `sample`.**
2. **The loop is a matrix product**, `kr = k_pts . transpose(pt)`. The cost is the `n_k x n_pt`
   cosines and sines, not the dot products, so the gain depends on vectorising `cos`/`sin` over a
   tile (glibc's vector library under `-Ofast` on Linux; nothing equivalent in gfortran on macOS).
   Time a vectorised tile on its own first. Blocked over `k` tiles — a full
   `n_k x n_pt` is 180 MB at gly_ala size — giving one DGEMM, a vectorised `cos`/`sin` over the
   tile, then one DGEMV against `rho`.
3. **Prune on `abs(rho*wt)`, not on the weight alone.** A point that contributes nothing still
   costs a full `k` loop, and the saving multiplies against all `n_k`.
