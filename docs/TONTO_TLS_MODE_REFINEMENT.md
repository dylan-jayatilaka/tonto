# Rigid-body and soft-mode ADP refinement against the diffraction data — plan

**Working document** (CLAUDE.md §1): delete it when the item closes, moving whatever is durable
into the user-facing pages. Live status is the entry in `TASKS_AND_HISTORY.md`, *ADPs as rigid-body
motion plus soft modes, refined against F*.

**Status 2026-10-03: step 0 is built, on branch `ls-jacobian`** (`36f2a0eb`): the restrained solver
(0a) and the Jacobian (0b), with isotropic hydrogens and special positions as its first uses. Mac
suites: 158 tests, the 11 expected failures (esds, N_p, GoF², iteration counts), re-bless on achari2
pending. Quartz L1's Si position esds went from 0.009–0.018 to 0.00003–0.00008; urea's isotropic-H
fit from 16 iterations to 4. Written from a discussion between Dylan and Claude on 2026-10-02/03,
recorded in §8.

**A design decision taken in step 0, which the plan below predates:** site symmetry is imposed as a
*constraint* through J, not as a restraint with a large weight. The condition is linear and
homogeneous (R δ = δ, R U Rᵀ = U for every stabiliser operation), so the symmetric subspace is a
linear space and the stabiliser average is a projector onto it; a basis of that subspace as J's
columns is exact, needs no weight, and gives zero variance to the fixed directions because they
have none. A restraint is for a preference, not a symmetry (§3.8 table still lists the restraint
form, which remains available). Also found: today's filter did not catch quartz's four forbidden
Si directions, so N_p was 19 and 20 where it is now 15 and 16.


## 1. What this is for

In a HAR the anisotropic displacement parameters (ADPs) of a molecule in a crystal are refined as
six free numbers per atom. Most of the motion they describe is not six numbers per atom: it is the
whole molecule translating and librating in its lattice site, plus a few soft internal motions
(torsions, ring puckers), plus stiff internal vibrations that are nearly all zero-point motion and
can be computed. The plan is to refine the ADPs in those terms, directly against the structure
factors:

- **Rigid-body motion**: the T, L and S tensors of Schomaker and Trueblood, 20 parameters per
  rigid group.
- **Soft internal modes**: a few mode shapes from a Hessian, with their thermal amplitudes (and
  their mixing with each other and with the rigid-body motion) refined.
- **Stiff internal modes**: computed once from the Hessian and held fixed.

What it buys: far fewer parameters (§3.5); hydrogen ADPs that mean something, because the six
free numbers per hydrogen are the weakest part of any X-ray refinement; parameters that are
physical quantities (libration amplitudes, torsional amplitudes) instead of 6N tensor components;
and a refinement that is optimal for everything at once, where the usual TLS fit to already refined
ADPs is not (§3.6).

The Hessian is **imported**, from Gaussian (already supported through the checkpoint reader and the
`force_constants=` keyword) or ORCA (reader to write). Tonto's own normal-mode code is reused and
extended (§4). The nuclear-wavefunction ideas discussed earlier are not part of this plan.


## 2. Notation

Cartesian throughout. N atoms, masses m_i. Wilson, Decius and Cross: mass-weighted Cartesian
displacements q = l Q, so the Cartesian displacement of atom i in normal coordinate Q_k is
x_i = l_ik Q_k / √m_i.

A set of m **generalised coordinates** Q (6 rigid-body plus k internal) moves atom i by

    u_i = B_i Q,            B_i a 3 × m matrix,

and if Q has thermal covariance Σ = ⟨Q Qᵀ⟩ (m × m, symmetric, positive semidefinite) then

    U_i = ⟨u_i u_iᵀ⟩ = B_i Σ B_iᵀ.                                        (1)

Everything below is a choice of the columns of B_i and of what in Σ is refined, fixed or
restrained. For the rigid-body columns B is geometry; for the internal columns B_i = l_i/√m_i.


## 3. Theory

### 3.1 Rigid-body motion: T, L and S

A rigid body displaces atom i by a translation t and a small rotation λ about an origin:

    u_i = t + λ × r_i = t + A_i λ,     A_i = [[0, z_i, −y_i], [−z_i, 0, x_i], [y_i, −x_i, 0]],

with r_i = (x_i, y_i, z_i) measured from the origin. So the rigid-body block of B_i is [1 | A_i],
and with

    T = ⟨t tᵀ⟩  (Å²),   L = ⟨λ λᵀ⟩  (rad²),   S = ⟨λ tᵀ⟩  (Å rad),

equation (1) gives the Schomaker–Trueblood formula

    U_i = T + A_i L A_iᵀ + A_i S + Sᵀ A_iᵀ.                                 (2)

Three facts about (2) that the refinement has to respect:

- **The trace of S is not determined.** Adding c to the three diagonal elements of S adds
  c (A_i + A_iᵀ) = 0 to every U_i, because A_i is antisymmetric. This is a property of the second
  moments of any rigid body, not of the scattering model, and no Bragg experiment can see it.
  Nothing wanted depends on it either (U_i, mean positions, libration corrections to bond lengths).
  Convention: tr S = 0. So 21 parameters become 20.
- **T and S depend on the origin; L does not.** Fix the origin. The usual choice is the one that
  makes S symmetric, the "centre of reaction". Refine about the centre of mass, report at the centre
  of reaction.
- **Degenerate geometries.** A linear molecule has no libration about its axis. Small or nearly
  linear molecules determine L poorly, and L and T trade off. A planar molecule is fine.

Everything else about the TLS fit is ordinary linear least squares: 6N equations in 20 unknowns,
well posed for any non-linear molecule with four or more atoms not all near one axis.

### 3.2 Internal modes from a Hessian

Given the Cartesian Hessian H (3N × 3N, atomic units) at the molecule's geometry:

1. Mass-weight: H' = M^(−1/2) H M^(−1/2), M = diag(m_i, m_i, m_i).
2. Build the six rigid-body vectors in mass-weighted coordinates: √m_i ê_α for the translations,
   √m_i ê_α × (r_i − r_com) for the rotations; orthonormalise them (Gram–Schmidt).
3. Project them out: P = 1 − Σ_v v vᵀ, H'' = P H' P. This imposes the Eckart–Sayvetz conditions:
   the remaining modes carry no net linear or angular momentum.
4. Diagonalise H'': six eigenvalues are zero (a check on the input), the rest are ω_k², with
   eigenvectors l_k.

The thermal mean-square amplitude of mode k at temperature T is

    ⟨Q_k²⟩ = (ħ / 2ω_k) coth(ħω_k / 2k_B T),                               (3)

and its contribution to atom i's ADP is l_ik l_ikᵀ ⟨Q_k²⟩ / m_i. In atomic units (ħ = 1) this is
exactly `MULTI_T_ADP:amplitude`.

**At one temperature, amplitude and frequency are the same parameter**: (3) is monotonic in ω_k,
so refining ⟨Q_k²⟩ or ω_k is a choice of variable. They separate only with data at several
temperatures: if the refined ⟨Q_k²⟩ follows (3) with one ω_k, the mode is harmonic with that
frequency; if not, it is anharmonic or mixed with the lattice. (Bürgi and Capelli did exactly this
with multi-temperature data.)

**Soft and stiff.** Split the 3N−6 modes at a frequency ω_c (about 200 cm⁻¹ to start; §3.6 decides
the number of soft modes from the data). The stiff ones are computed once and fixed:

    U_i^high = Σ_{ω_k > ω_c} l_ik l_ikᵀ ⟨Q_k²⟩ / m_i.                       (4)

Above about 500 cm⁻¹, coth → 1 and (4) is zero-point motion, nearly independent of temperature.
Its size, for pure hydrogen motion at 3000 cm⁻¹ (stretch) and 1300 cm⁻¹ (bends):
about 0.006 Å² along the bond and 0.013 Å² across it. That is a third of a typical hydrogen ADP
and cannot be left out. For heavy atoms it is 0.001–0.003 Å², about an esd. This is what SHADE
adds to a TLS fit to get hydrogen ADPs. A harmonic frequency scale factor (about 0.96 for B3LYP) can
be applied to the stiff modes; it is a second-order effect here.

The Hessian from a gas-phase program is at the gas-phase geometry. Transport the Cartesian mode
vectors to the crystal geometry atom by atom (the atom order must match), and build the rigid-body
vectors and the projection at the crystal geometry. Mode shapes are much less sensitive to the
level of theory and to geometry than frequencies, which is the reason for fixing shapes and
refining amplitudes.

### 3.3 The full model: one covariance matrix

Take m = 6 + k generalised coordinates: the rigid-body six and the k softest internal modes. Then
U_i = U_i^high + B_i Σ B_iᵀ, with Σ partitioned

         ⎡ T    Sᵀ   C_tᵀ ⎤
    Σ =  ⎢ S    L    C_λᵀ ⎥
         ⎣ C_t  C_λ  Σ_int ⎦

- T, L, S: as in §3.1 (20 parameters after tr S = 0).
- Σ_int (k × k): diagonal entries are the soft-mode amplitudes ⟨Q_k²⟩; off-diagonal entries are
  the mixing between soft modes. Near-degenerate soft modes are exactly the ones a Hessian gets
  least right, so refining the mixing lets the data correct the mode shapes within the soft
  subspace, which is the only sense in which "refining the form of the modes" is determined by the
  data. Refining the 3N components of each shape is not; it is free U again.
- C_t, C_λ (k × 3 each): correlation of each soft mode with translation and libration. Physical:
  a torsion couples to the libration about the same axis.

Σ must be positive semidefinite (§3.7 says how), and there may be exact null directions besides
tr S, for instance a soft mode that coincides at every atom with a rigid-body field; those are
handled as tr S is.

Parameter count: m(m+1)/2 − 1 for the full Σ; 20 + k if only the soft-mode amplitudes are refined
(Σ_int diagonal, C_t = C_λ = 0).

### 3.4 Refining against the structure factors

The structure factor depends on U_i through the Debye–Waller factor, and Tonto already forms
∂F/∂U_i for every atom. The model's derivatives are the chain rule:

    ∂F/∂Σ_pq = Σ_i Σ_ab (∂F/∂U_i,ab) (∂U_i,ab/∂Σ_pq),     ∂U_i/∂Σ_pq = sym(B_i,·p B_i,·qᵀ),

where the second factor is a constant matrix for fixed geometry. In matrix form: if dF is the
present n_refl × n_pADP derivative matrix, the model's is dF · J, with J the constant
n_pADP × n_model Jacobian (identity on the positional parameters, the B_i products on the ADP
rows). B_i depends weakly on the positions being refined; the Gauss–Newton iteration takes care of
that. Parameters refined together: positions, Σ, scale, extinction, as now. Fix tr S = 0 and the
origin, or the normal matrix is singular.

### 3.5 How many parameters this is

The point of comparison is the 6N free U_ij refined today (9N parameters with positions; the
positions are the same in both models and are left out). Full Σ against 6N:

| N atoms | 6N  | k_max with full Σ | parameters at k_max | 20 + k, amplitudes only, k_max |
|---------|-----|-------------------|---------------------|--------------------------------|
| 5       | 30  | 1                 | 27                  | 10                             |
| 10      | 60  | 4                 | 54                  | 40                             |
| 15      | 90  | 7                 | 90                  | 70                             |
| 20      | 120 | 9                 | 119                 | 100                            |
| 30      | 180 | 12                | 170                 | 160                            |
| 50      | 300 | 18                | 299                 | 280                            |

For a typical 20-atom molecule, nine soft modes with full mixing is still fewer parameters than
today's free refinement; in practice k will be 2–4. The table shows only that the model is never
more parametrised than the present one; the real observation count is the number of reflections.
If hydrogens are refined isotropically today, the 6N column shrinks by 5 per hydrogen and the
conclusion holds.

### 3.6 Choosing the number of soft modes: Hamilton, AIC and BIC

Adding a mode always lowers the weighted residual χ² = Σ_h w_h (F_obs,h − F_calc,h)². The question
is whether it lowers it by more than noise would. Three standard answers, all for the same Gaussian
least-squares likelihood (−2 ln L = χ² + constant, when the σ_h are known):

- **AIC** (Akaike): choose the model with the smallest χ² + 2p, where p is the number of refined
  parameters. It estimates how well the model would predict new data. Rule: one more parameter must
  lower χ² by more than 2.
- **BIC** (Schwarz): smallest χ² + p ln n, n the number of reflections. It approximates the
  posterior probability of the model. For 1000–8000 reflections ln n is 7–9, so BIC is stricter
  than AIC and leans toward fewer modes.
- **Hamilton's R-factor ratio test**: for the model with fewer parameters, ℛ = wR₁/wR₂ ≥ 1; the extra
  b parameters are justified at significance α if ℛ exceeds ℛ_{b,n−p,α} = √(1 + b F_{b,n−p,α}/(n−p)).
  It is the F test in R-factor clothing. For one parameter at α = 0.05 and large n−p, F ≈ 3.84, so
  Hamilton sits between AIC (2) and BIC (ln n).

Procedure: add the next lowest mode, refine, compute the criterion, stop when it stops improving.
Nested models only (each model contains the one before). With restraints (§3.7) p must be the
*effective* number of parameters, tr[A(A+W)⁻¹], not the count. Tonto's GoF² is χ²/(n − p) and should
use the same p; see `docs/GOF_NOT_CHI2.md`.

A later refinement of this: make all 3N−6 modes candidates, fix every amplitude at its harmonic
value, and refine only the *excess* amplitudes with an L1 penalty (basis pursuit). Sparsity in the
excess is the right prior, since most modes are stiff and well predicted. It needs ⟨Q²⟩ ≥ 0 (non-
negative sparse regression) and, with mixing, a positive-semidefinite low-rank fit with a nuclear-
norm penalty. Tonto's Gauss–Newton loop does not solve L1 problems; the practical route is
iteratively reweighted ridge, which §3.7 provides. Not for the first version.

### 3.7 Why the two-step method is worse, and why not TLS plus free U

**Two-step (refine U_ij, then fit T, L, S to them)** is Schomaker and Trueblood's original
procedure and is what THMA and PLATON do. It treats the refined U_ij as data, equal weighted or
weighted by their esds, and drops their correlations with each other, with the positions and with
the scale. If the second step used the full covariance matrix of the U_ij the two methods would
agree to first order (Gauss–Markov); nobody does that. The direct refinement weights everything by
the diffraction data and gives T, L and S honest esds. It also forbids non-positive-definite U for
weak scatterers. The two-step method keeps one role: as the diagnostic. A free refinement followed
by a TLS fit shows atom by atom where the rigid-body model fails (a methyl, a phenyl); the direct
constrained refinement hides that in the R factor and the residual density, and the positions
absorb part of the error. So: free refinement first to decide the segments and soft modes,
constrained refinement for the final parameters, Hamilton or AIC between them.

**TLS plus free U together**, letting the eigenvalue filter remove what is undetermined, does not
work, and the reason is that the degeneracy is exact, not small. Write U_i = U_i^high + A_i p + ΔU_i
(p the TLS parameters, ΔU_i free). For any δ,

    p → p + δ,    ΔU_i → ΔU_i − A_i δ

leaves every U_i and every structure factor unchanged. The normal matrix has a 20-dimensional null
space whatever the data. Filtering gives the right total U_i (what a free refinement gives) but the
split between p and ΔU is set by the pseudo-inverse, which picks the shift with no component along
the null vectors; those mix T (Å²), L (rad²), S and U, so the split depends on the units and the
starting values. T, L, S would be meaningless, and nothing is gained for the ADPs. What makes
"rigid where the data cannot say otherwise, free where they can" well posed is a *penalty* on ΔU,
not a filter: that is the restraint of §3.8, and it is how macromolecular codes do TLS with
residual B factors.

### 3.8 The eigenvalue filter, and its generalisation to restraints

**What the filter does.** Each least-squares cycle solves the normal equations

    A δ = b,    A = Dᵀ w D,   b = Dᵀ w (F_obs − F_calc),

for the parameter shifts δ, D being the derivative matrix. `MAT{REAL}:solve_ill_linear_equations_v1`
diagonalises A = V diag(a_j) Vᵀ and forms the pseudo-inverse

    A⁺ = Σ_{a_j > tol} v_j v_jᵀ / a_j,     δ = A⁺ b,

with the directions v_j whose eigenvalue is at or below `near_0_tol` (10⁻³; also any negative one)
left out. The count of dropped directions is subtracted from the parameter count, and the covariance
matrix of the parameters is GoF² A⁺.

Three consequences:

1. **Along a dropped direction the parameters do not move**, and their variance is reported as
   zero. For an *exact* null direction (tr S; a symmetry-breaking combination of a special-position
   atom's parameters) that is right. For a merely *poorly determined* direction (L against T in a
   nearly planar molecule; a soft-mode cross term the data barely see) the parameters are silently
   frozen at their starting values with no esd.
2. **Which directions are dropped depends on the units of the parameters**, since the threshold is
   applied to eigenvalues of A, and positions (bohr), U (bohr²), L (rad²) scale A differently. The
   same model in different units drops different directions.
3. **Special positions.** `CRYSTAL:stabilize_asym_atom_shifts` averages each special-position
   atom's shift over its site stabiliser *after* the solve, so the shift is symmetric. The
   covariance matrix is not symmetrised, and the symmetry-breaking directions are either dropped
   (variance 0) or, if the data leave them a small eigenvalue, kept with a large variance. The
   quartz L1 test's IO note records the result: the refined Si position esd is 0.009 Å against
   0.00014 Å for the general-position oxygen, with the bond length right to 0.0003 Å and only the
   uncertainty wrong.

**The restraint.** Minimise instead

    S(p) = Σ_h w_h (F_obs,h − F_calc,h(p))² + (p − p₀)ᵀ W (p − p₀),          (5)

where p₀ is a prior value for each parameter and W a symmetric non-negative weight matrix (zero
rows for parameters with no prior). Linearising F_calc(p + δ) ≈ F_calc + D δ gives the normal
equations

    (A + W) δ = b + W (p₀ − p),                                             (6)

with covariance (A + W)⁻¹ (the Bayesian one; the frequentist covariance of the estimator is
(A+W)⁻¹ A (A+W)⁻¹, and both should be printed while this is new) and effective number of
parameters

    p_eff = tr[(A + W)⁻¹ A],                                                (7)

which replaces p in GoF², AIC, BIC and Hamilton. Reading: (5) is −2 ln of the posterior with a
Gaussian prior of mean p₀ and covariance W⁻¹. A well-determined parameter ignores its prior
(a_j ≫ w_j); a poorly determined one follows it (a_j ≪ w_j); nothing is frozen silently, nothing has
zero variance, and the choice is made per parameter in the parameter's own units.

**The filter is a limiting case of (6):** W infinite along the dropped eigenvectors of A and zero
elsewhere, with p₀ the current value. The generalisation is to make W finite and to choose it on
physical grounds:

| use | p₀ | W |
|-----|----|---|
| soft-mode amplitudes | harmonic value from the Hessian, eq. (3) | 1/σ² with σ a fraction (say 50%) of the harmonic value |
| soft-mode cross terms (C_t, C_λ, off-diagonal Σ_int) | 0 | weak, from the same σ and the TLS scale |
| T, L, S | none | 0 |
| tr S | 0 | large (replaces the filter for this one direction) |
| residual free ΔU_i (if ever used) | 0 | 1/σ_U² with σ_U about 0.002 Å² |
| special positions | the symmetrised value P p | w (1 − P)ᵀ(1 − P), w large; P the stabiliser average |
| ridge / L1 by reweighting | current value | λ I, or λ/|p_j| per parameter, iterated |

The last-but-one row replaces `stabilize_asym_atom_shifts`: the shift comes out symmetric from
the solve itself, the symmetry-breaking directions have variance 1/w → 0, and the symmetric ones
have proper, finite esds. That is the proposed cure for the quartz Si esd, to be verified on that
test. With W = 0 everywhere (6) reduces to today's solve, so the whole suite is the regression test
for step 0 in §5. Keep the eigenvalue filter as a fallback for exact null directions that no
restraint names.

**Positive semidefiniteness of Σ.** Not a restraint but a constraint. Cheapest: after each cycle,
diagonalise Σ and set negative eigenvalues to zero (projection onto the PSD cone). Cleaner but more
nonlinear: refine a Cholesky factor, Σ = C Cᵀ. Start with the projection.


## 4. What exists in Tonto

Found 2026-10-03 by reading the code; file:line are approximate.

**Least squares.** `CRYSTAL:LS_structure_fit(ff,output,results)` (`crystal.foo:4574`) does one
rigid-atom cycle: `initialize_fit_data`, pADP vector from the asymmetric-unit atoms, then
`get_parameter_shifts_F(ff)` (`:4814`), which calls `make_F_calc_derivs(dFc,ff)` (`:4241`; builds
dF/dX for positions and ADPs through `make_unique_sf_derivs` and `SPACEGROUP:sum_unique_sf_derivs`),
`DIFFRACTION_DATA.INQ:d_F_abs_dX`, and `DIFFRACTION_DATA.SET:solve_normal_equations(dFdX)`
(`diffraction_data.set.foo:2259`), which forms weights and residuals and calls the three-argument
`solve_normal_equations(dF,sig,del)` (`:2465`): builds A and b, calls
`MAT{REAL}:solve_ill_linear_equations_v1`, sets `n_param_structure = n_p − n_0 − near_0`, GoF²,
covariance, `update_fit_esds`, and caps the shift. Then `stabilize_asym_atom_shifts`
(`crystal.foo:5358`) symmetrises special-position shifts, and `set_asym_from_ufrag_shifts` applies
them.

**Parameter vector.** `ATOM:put_pADP_vector_to` (`atom.foo:2538`): positions 1–3, U 4–9 in the order
xx, yy, zz, xy, xz, yz, then third-order (10–19) and fourth-order (20–34) anharmonic terms when
tagged. `VEC{ATOM}:no_of_pADPs`, `no_of_pADPs_up_to_atom`, `get_atom_for_pADP_index` index into it.
Isotropic hydrogens: `VEC{ATOM}:set_isotropic_H_ADP`. Covariance → ADP esds:
`ATOM:set_pADP_errors_to(covariance_mx,H_U_iso)`.

**Hessian input.** `MOLECULE.PROP:read_force_constants` (`force_constants=` keyword): a flat vector
in atomic units, either triangle. The Gaussian checkpoint reader (`molecule.read.foo:1846`) fills
`.force_constants` from "Cartesian Force Constants". No ORCA `.hess` reader.

**Normal modes.** `MOLECULE.PROP:normal_mode_analysis` mass-weights `.force_constants` with
`VEC{ATOM}:displacement_mass_vector` and diagonalises; it does **not** project out the rigid-body
motions, so the six "zero" modes come out mixed with the softest internal ones. `put_normal_modes`
prints frequencies in cm⁻¹. Test `short/h2o_rhf_6-31G(d)_normal_mode_analysis` exercises it.
`MOLECULE` also carries `phi3_force_constants` and `phi4_force_constants` (unused here).

**`MULTI_T_ADP`** (`multi_t_adp.foo`, 2800 lines, Dylan 2008 and 2021): fits normal-mode shapes,
frequencies and Grüneisen parameters to ADPs from several CIFs at several temperatures, with
starting modes from translations, librations, local X–H modes (Bürgi's ε tensors) and random
internal vectors. **It is not in the build** (not in `CMakeLists.txt`; `scripts/simplify_callgraph.py`
drops it as dead) and would not compile: `PUIRE` at `:2116`, `PUREj` at `:2737`. It fits to ADPs,
not to F, and refines mode shapes, which §3.3 argues against. Worth salvaging, as code or as
reference: `initialize_translations`, `initialize_librations` (the rigid-body fields, §3.1),
`VEC{ATOM}:initialize_local_H_modes` (still live, in `vec{atom}.foo:778`), `amplitude` (eq. 3),
and the `MULTI_T_ADP` type's layout. Leave the module dead.

**Isotropic hydrogens** (`refine_h_u_iso= YES`) are done inside the same 9-parameter block
(`crystal.foo:6231`): the three diagonal U columns of the derivative matrix are set *identical*, each
to the full dF/dU_iso, and the three off-diagonal columns to zero. So every isotropic hydrogen puts
five exact null directions into the normal matrix (urea STO-3G HAR: 25 near-zero eigenvalues with
two isotropic H against 19 with anisotropic), the filter drops them, and the pseudo-inverse spreads
the U_iso shift equally over the three diagonal components. Each therefore moves by **one third** of
the Gauss–Newton step, and since `ATOM:set_isotropic_ADP` then takes the mean of the trace, U_iso
moves by one third per iteration and converges geometrically with ratio 2/3. **Measured 2026-10-03**
on `long/urea_rhf_STO-3G_HAR`: the first fit needs 16 least-squares iterations with isotropic H
against 3 with anisotropic H, the limiting parameter being H1's U every time, and
(2/3)^16 = 0.0015 is exactly the ratio of the first to the converged shift/esd (6.49 → 0.01). The
final U_iso is right; only the cost is wrong. The esd is right too, by a compensating trick:
`ATOM:set_pADP_errors_to` takes the U_iso esd as the *sum* of the three component esds ("this is
correct believe it or not"), and it is, because each component's variance is one ninth of the true
one. The component esds printed in the ADP table are one third of the truth. All of this disappears
when an isotropic hydrogen is one parameter with one column, which is the Jacobian J of §3.4 with a
9 → 4 block per such atom; so J is built in step 0 (below) with this as its first use.

**Geometry helpers on `VEC{ATOM}`:** `center_of_mass`, `move_origin_to_center_of_mass`,
`make_inertia_tensor`, `make_inertial_axes`, `displacement_mass_vector`, `make_connection_table`.
`MAT{REAL}:schmidt_orthonormalize`, `solve_symmetric_eigenproblem`, `diagonalize_Jacobi`.


## 5. Design and plan

**Rule for placement:** put each piece on the lowest module that has what it needs, so others can
use it. The TLS algebra needs only positions, so it is not a `CRYSTAL` method.

### Where the code goes

| piece | module | why there |
|-------|--------|-----------|
| restrained solve, eq. (6)–(7): `solve_restrained_linear_equations(rhs, W, prior_shift, ans, covariance, n_eff)` | `MAT{REAL}` | pure linear algebra; beside `solve_ill_linear_equations_v1` |
| projecting a set of vectors out of a symmetric matrix | `MAT{REAL}` | used by the Eckart step; general |
| rigid-body fields `make_rigid_body_modes(B, origin)`: the [1 ∣ A_i] columns, optionally mass-weighted | `VEC{ATOM}` | needs positions and masses only; serves `MULTI_T_ADP` too if revived |
| Eckart projection and the soft/stiff split: `make_eckart_modes(l, omega)`; `make_internal_ADPs(omega_c, T, U_high)`; ORCA `.hess` reader | `MOLECULE.PROP` | owns `.force_constants` and the normal modes already |
| the model itself: a new type `MODE_ADP` with its module `mode_adp.foo`: B (3N × m), Σ, origin, mode labels and frequencies, U_high, temperature, prior Σ₀ and weights; `make_U`, the constant Jacobian `d_U_d_Sigma`, to/from the parameter vector, PSD projection, `put` (T, L, S at the centre of reaction, L eigenvalues in degrees², rms libration, mode amplitudes against their harmonic values) | new, depending on `VEC{ATOM}` and `MAT{REAL}` only | like `MULTI_T_ADP` but small, and independent of diffraction |
| wiring: `adp_model=` keyword (`free`, `tls`, `tls+modes`), the Jacobian applied to dF (`dF · J`), Σ parameters in the refined vector, propagation of cov(Σ) to the atomic U esds (J cov Jᵀ), the per-step AIC/BIC/Hamilton line | `CRYSTAL` and `DIFFRACTION_DATA` | that is where the fit lives |

### Procedures changed or added

- `MAT{REAL}`: **new** `solve_restrained_linear_equations`; **new** `project_out_vectors`.
- `DIFFRACTION_DATA.SET:solve_normal_equations(dF,sig,del)`: take W and p₀ − p; call the new
  solver; `n_param_structure` becomes p_eff (eq. 7); covariance from (A+W)⁻¹. With W = 0 the result
  is today's to rounding.
- `DIFFRACTION_DATA` type: restraint weights and priors; `adp_model`; the model-selection numbers.
- `CRYSTAL:get_parameter_shifts_F` (both forms): multiply dF by J when `adp_model /= free`.
- `CRYSTAL:LS_structure_fit`: build the model's parameter vector; after the solve, map Σ shifts back
  to atomic U through J; esds through J cov Jᵀ.
- `CRYSTAL:stabilize_asym_atom_shifts`: unchanged at first; made redundant by the symmetry restraint
  row once that is verified on quartz, then removed.
- `VEC{ATOM}`: **new** `make_rigid_body_modes`.
- `MOLECULE.PROP`: `normal_mode_analysis` gains the Eckart projection (keep the old behaviour behind
  the existing keyword until the one test is re-blessed); **new** `make_internal_ADPs`; **new**
  `read_orca_hessian`.
- `MODE_ADP`: **new** type and module; `types.foo` entry.
- `MULTI_T_ADP`: untouched.

### Steps, each with its check

0. **DONE on `ls-jacobian`. Restraints in the solver** (independent of everything else). `MAT{REAL}` solver, the W and p₀
   plumbing in `DIFFRACTION_DATA.SET`, p_eff. Check: with W = 0 the full suite is unchanged;
   with the special-position restraint, quartz L1 gives the same Si position and a finite, sensible
   Si esd (the IO note says what to expect), and no other number moves beyond tolerance.
   **0b. The Jacobian J**, dF · J, with isotropic hydrogens as its first use: one U_iso parameter per
   such atom instead of three identical columns and three zero ones. Checks: the urea HAR above
   converges in about 3 iterations instead of 16, to the same U_iso and the same U_iso esd (now a
   plain square root of a variance, the sum trick retired), and the component esds in the ADP table
   become consistent with it. The 25 near-zero eigenvalues drop to 19. Tests with isotropic H need a
   re-bless if any printed digit moves.
1. **Hessian in, modes out.** ORCA reader; Eckart projection; six zero frequencies as the check;
   `make_internal_ADPs`. Check against the literature: urea's hydrogen U^high about 0.01–0.02 Å²
   (SHADE's values); heavy atoms 0.001–0.003 Å².
2. **TLS only, against F.** `MODE_ADP` with the six rigid-body columns; J; refine positions and
   T, L, S with U^high fixed. Checks: on a rigid molecule (urea), compare T, L, S with a PLATON/THMA
   fit to the free-refinement ADPs after subtracting U^high: same to within the esds; wR and GoF²
   against the free refinement by Hamilton. Hydrogen ADPs against neutron where there are neutron
   data (the ten structures listed in `docs/TONTO_RI_FITTING_PLAN.md`).
3. **Soft modes, one at a time.** Amplitudes only (Σ_int diagonal, no cross terms), restrained to
   the harmonic value; AIC/BIC/Hamilton printed each step. Check on a molecule with a torsion
   (gly_ala, or any HAR test with a methyl): the first mode added should be the torsion, and its
   amplitude should exceed the harmonic one.
4. **Full Σ.** Cross terms and mixing; PSD projection; the restraint table of §3.8.
5. **Later, not in this plan's first pass:** L1 by reweighting; multi-temperature data (frequency
   against amplitude; Grüneisen, where `MULTI_T_ADP` is the reference); segmented TLS by bond
   graph (Trueblood–Dunitz attached rigid groups); anharmonic torsions by a one-dimensional Boltzmann
   sum in the periodic potential, refining the barrier instead of the amplitude; TLS into the CIF.

### Tests

Existing: `long/quartz_NN_HAR_L1_rhf_def2-SVP` (step 0), `short/h2o_rhf_6-31G(d)_normal_mode_analysis`
(step 1; will be re-blessed when the projection goes in), the urea and gly_ala HARs (steps 2–3).
New: one TLS-only HAR and one TLS+modes HAR with a committed Hessian file, blessed on achari2.


## 6. What the first version does not do

- Average the structure factor over the curved path of a libration (Johnson's correction; the
  apparent bond shortening). Second order in the libration amplitude; add when librations exceed
  a few degrees.
- Treat anharmonic torsions; they are Gaussian in the first version.
- Refine mode shapes. Only amplitudes and their mixing within the soft subspace.
- Fit to several temperatures at once.
- Read a Hessian in any format but Tonto's own flat vector, Gaussian's checkpoint and ORCA's `.hess`.


## 7. Open questions

- The frequency cut ω_c between soft and stiff, and whether the stiff set should get one refined
  scale factor.
- Whether to refine Σ about the centre of mass and report at the centre of reaction, or refine the
  origin too (it is determined by the data to the extent S is).
- How to handle a molecule on a special position: T, L, S must carry the site symmetry, which the
  symmetry restraint of §3.8 can impose on the Σ parameters as it does on atomic ones.
- Several molecules in the asymmetric unit: one Σ each, with the Hessian of each, or of the whole
  asymmetric unit.
- Whether the harmonic frequency scale factor belongs here or in the user's Hessian program.


## 8. Record of the discussion (2026-10-02/03)

Kept so the reasoning is not lost; the plan above is the residue.

**Starting point.** Dylan proposed a vibrational Hartree treatment of the nuclei with the Einstein
approximation (each nucleus in a single-centre basis), minimising the Helmholtz free energy at
finite T with the electrons at T = 0. Two things came out of examining it:

- *A mean-field nucleus moves in a frozen electron cloud.* By Gauss's law the restoring force
  constant from the atom's own core is k ≈ (4π/3) Z ρₑ(0):

  | | ρₑ(0) / bohr⁻³ | Z | k, frozen cloud / Eh bohr⁻² | k, BO surface / Eh bohr⁻² | ratio |
  |---|---|---|---|---|---|
  | C | ≈ 120 | 6 | ≈ 3000 | ≈ 0.3 (a C–C stretch) | ~10⁴ |
  | H | ≈ 0.4 | 1 | ≈ 2 | ≈ 0.3–0.5 (an X–H stretch) | ~5 |

  On the Born–Oppenheimer surface the core moves with the nucleus and contributes no curvature;
  mean field freezes it. Carbon would vibrate 100 times too fast and its U would be 100 times too
  small. This is the known failure of mean-field nuclear–electronic orbital methods, which are used
  for protons only and then with an electron–proton correlation functional. So the nuclear part of
  the scheme was kept and the potential replaced by the BO surface.
- *With the BO surface, a single-atom Einstein model is still wrong for a molecular crystal.*
  Displacing one atom with its neighbours fixed stretches bonds (stiff); most of a room-temperature
  ADP comes from the six soft external modes (30–150 cm⁻¹), which an independent-atom potential
  cannot describe. Hence rigid-body motion plus internal modes.

**Rotations.** Free rigid-rotor wavefunctions (Dylan's first suggestion) are the basis for a gas;
in a crystal the molecule librates through a few degrees in a steep well, so the three rotations
and three translations form a 6D oscillator whose second moments are T, L, S, with (3) giving the
Boltzmann sum in closed form. The anharmonic case that matters is the curved path of a libration.

**Redundancy of 3N Cartesian plus 6 external coordinates.** Dylan proposed scaling the thermal
energy by the ratio of excess coordinates as a first approximation. Not needed: project the six
rigid-body vectors out of the Hessian (Eckart conditions) and 3N−6 + 6 = 3N exactly. Scaling would
change every U by 3N/(3N+6) (17% for N = 10) while the real error lives in the six soft directions
and is a factor of several; and modes do not share thermal energy evenly (external ≈ k_BT each,
internal mostly zero-point).

**Why fit directly against F rather than to refined ADPs** (Dylan's objection to the two-step
method as "not optimal for all parameters at once"): §3.7. Agreed.

**TLS and free U together with the eigenvalue filter** (Dylan): exactly degenerate, §3.7; the fix is
a penalty, §3.8. Fitting tr S by the filter is fine, and is the one case where the filter equals a
constraint.

**Is a Hessian needed for TLS?** No: the six rigid-body fields are geometry. The Hessian enters only
through the internal modes, §3.2.

**How restrictive is TLS plus positions?** Too restrictive for any molecule with a soft torsion. The
ladder: (1) TLS; (2) segmented TLS; (3) TLS plus fixed internal modes; (4) TLS plus refined soft-mode
amplitudes (Bürgi–Capelli normal-mode refinement); (5) TLS plus penalised free ΔU. The plan goes to
(4), with (5) as the safety net.

**Amplitude or frequency; form of the modes; L1; the stiff residue**: §3.2, §3.3, §3.6. Dylan knows
AIC and not BIC; §3.6 explains both and Hamilton. Dylan chose: fixed internal U's; options 1 and 2
(full or cheap Hessian) as the starting points; mode at a time.

**Notation**: B is M^(−1/2) l with the rigid-body columns added (Wilson, Decius and Cross), §2.

**Correction made during the discussion.** Claude said `LS_structure_fit` has site-symmetry
constraints on U; it has none in the normal matrix. Special-position shifts are symmetrised after
the solve by `stabilize_asym_atom_shifts`, and ill-determined directions are handled by the
eigenvalue filter. §3.8 and §4 state what the code does.


## 9. References

- V. Schomaker and K. N. Trueblood, *Acta Cryst.* B24, 63 (1968). TLS; the trace of S.
- J. D. Dunitz, V. Schomaker and K. N. Trueblood, *J. Phys. Chem.* 92, 856 (1988). Interpretation
  of ADPs; attached rigid groups (THMA).
- H.-B. Bürgi and S. C. Capelli, *Acta Cryst.* A56, 403 (2000); S. C. Capelli, M. Förtsch and
  H.-B. Bürgi, *Acta Cryst.* A56, 413 (2000). Normal-mode refinement against multi-temperature ADPs.
- A. Ø. Madsen, *J. Appl. Cryst.* 39, 757 (2006). SHADE: hydrogen ADPs from TLS plus internal modes.
- A. A. Hoser and A. Ø. Madsen, *Acta Cryst.* A72, 206 (2016); A73, 102 (2017). Dynamic quantum
  crystallography: lattice-dynamical models fitted to ADPs, with HAR.
- M. D. Winn, M. N. Isupov and G. N. Murshudov, *Acta Cryst.* D57, 122 (2001). TLS refined directly
  against the data, with residual B factors.
- W. C. Hamilton, *Acta Cryst.* 18, 502 (1965). The R-factor ratio test.
- E. B. Wilson, J. C. Decius and P. C. Cross, *Molecular Vibrations* (1955). Normal coordinates,
  Eckart conditions, the l and L matrices.
- C. K. Johnson, in *Crystallographic Computing* (1970). Curvilinear (libration) corrections.
