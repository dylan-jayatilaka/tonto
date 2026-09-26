# Aspherical form factors by RI density fitting — plan

**Working document** (CLAUDE.md §1): delete it when the item closes, moving whatever is durable
into the user-facing pages. Live status is the entry in `TASKS_AND_HISTORY.md`, *Aspherical form factors by
RI density fitting*.

**Status 2026-09-24: sketch only.** Nothing is implemented. Dylan's proposal, recorded with the
profile that motivates it and a survey of what the tree already has. To be worked out in detail
later.

**The proposal (Dylan).** Expand the Hirshfeld atomic densities using the RI machinery already in
the tree rather than writing a fresh multipole/Bessel transform. It must be a **density** fit, not
the Coulomb (potential) fit that regular RI-J does.

Distinct from the *"effect of the fitted density on structure factors"* owed in the RI-J entry of
`TASKS_AND_HISTORY.md`: that concerns structure factors computed from an RI-J **SCF** density; this concerns
fitting `w_a rho` itself so that its Fourier transform becomes analytic.


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
