# Atomic electron-density response — plan

Working document (CLAUDE.md §1). The equations, the uses and the literature are in
[`REPORT_ON_ATOMIC_ED_RESPONSE.md`](REPORT_ON_ATOMIC_ED_RESPONSE.md), cited here as (R1), (R2), …
Delete this page when the item closes; move what lasts into the report.

**Opened 2026-10-22** at Dylan's request: the response of each atom's density to nuclear motion
(and to an electric field), for (a) thermal smearing beyond the rigid atom, a simpler and fuller
`tc-tls`; (b) charge-flux polarisable force fields; (c) refractive indices and the vibrational
dielectric response; each with an atom-by-atom interpretation. Nothing is implemented yet. The
SAM paper comes first; this item starts after it.

## What the literature search found (2026-10-22)

Two searches, one on response theory and atomic interpretation, one on crystallographic thermal
models, force fields and optical properties. Local PDFs in `~/Dropbox/manuscripts/` were read
first; the rest from abstracts and Crossref. Summary of the gaps, which are what make this worth
doing:

1. No one has partitioned the analytic $`\partial\rho/\partial R`$ (CPHF/CPKS) into real-space
   atoms (Hirshfeld, ISA, MBIS, Becke, TFVC, or even QTAIM) and analysed the atomic density
   changes as functions, or split them into rigid following plus deformation. The QTAIM
   charge–charge flux–dipole flux work (Haiduke & Bruns 2005 onwards) uses finite differences of
   populations and stops at the dipole. Hirshfeld enters only through charges and dipoles
   (Richter et al. 2021, AC/DC).
2. No analytic $`\partial^2\rho/\partial Q^2`$ has been used for a vibrationally averaged density or
   structure factors of a molecular crystal. Closest: Michael & Koritsanszky (J. Math. Chem. 53, 250,
   2015) — exact two-centre *rigid* convolution plus ensemble averages for formamide and S8, with
   BCP properties agreeing within ~10%, H maxima washed out; and Nery et al. (arXiv:2608.27361,
   2026) — perturbative thermally averaged valence density of silicon, no partition, charge moves
   into the bonds. Hoser & Madsen's review (IUCrJ 12, 421, 2025) does not mention deforming atoms.
3. Auxiliary-basis response for nuclear perturbations exists only as a speed-up: RI-J Hessians
   (Deglmann, May, Furche, Ahlrichs, CPL 384, 103, 2004 — reference confirmed), RIJCOSX Hessians
   (Bykov et al. 2015), auxiliary density perturbation theory (Flores-Moreno & Köster 2008;
   Delgado-Venegas et al. 2016, moving auxiliary functions). Never analysed or partitioned.
4. Distributed multipole response exists only for electric fields (Stone; Ángyán et al. 1994 CPHF +
   QTAIM; Misquitta & Stone ISA-Pol; Krishtal et al. 2006 Hirshfeld). Nothing for nuclei.
5. Optical properties of molecular crystals in the crystallographic literature are entirely
   electronic (Whitten, Jayatilaka & Spackman 2006; Jayatilaka et al. 2009; PolaBer); the
   vibrational part with an atomic interpretation is not done.

Unresolved references, not to be cited without a check: "Choi & Kertesz" (charge–charge flux
overlap; the local `Choi_1999_Angew` PDF is unrelated); SIBFA's charge-flux treatment; Reis,
Papadopoulos & Munn on crystal susceptibilities. The Michael & Koritsanszky J. Math. Chem. paper is
2015 (online 2014). Ruysink & Vos 1974 is ethyne, not ethylene.

## What Tonto has (checked 2026-10-22)

- `MOLECULE.CP`: `solve_CPHF_equations(U,rhs)` for any number of perturbations, RHF only, J and K
  built in the AO basis, DIIS; `make_U_electric_dipole`; `make_perturbed_densities(D,U,MO,no)`;
  `make_SCF_atomic_polarisability` divides the perturbed densities into stockholder atoms on the
  Becke grid (charge and dipole polarisabilities); unit-cell polarisability, hyperpolarisability
  and refractive index (`put_UC_refractive_index`). No exchange–correlation kernel, so no CPKS.
- Derivative integrals, untested and unused: `GAUSSIAN2:make_S_1st_deriv_ints`,
  `make_T_1st_deriv_ints`, `make_NA_1st_deriv_ints`; `SHELL2` wrappers of each;
  `GAUSSIAN4:make_ERI_derivatives` and `SHELL4:make_ERI_derivatives` (MAT5 output, centres A, B, C,
  D by translational invariance for the fourth).
- `MOLECULE.MISC`: a commented-out HF energy gradient (`HF_energy_gradient`, `energy_gradient`,
  `add_S_gradient_term`, `add_T_gradient_term`, `add_NA_gradient_term`, `add_2e_gradient_term`,
  `make_e_weighted_density_mx`, `make_fd_gradient`). Dylan: "old and wrong". It reads the density
  from an archive and uses old call forms; treat it as a sketch.
- `tc-tls`: SCF at displaced geometries (`make_two_center_position_derivs(...,redo_SCF)`, the
  `scf` derivative kind; §6 stiff-mode averages), the mode vectors $`\mathbf d^k_A`$ and
  $`\langle Q_k^2\rangle`$ (`REPORT_ON_MODE_FITTING.md`).
- TFVC atoms, their harmonic expansion (`make_sph_avgd_SA_ED_grid` with `rho_lm`), the LKO cutoff.

## Stages

Each stage ends with a short test in `tests/short` (NH3 or H2O, a few seconds), and is merged on
its own.

### Stage 1 — the response by finite differences, partitioned (no new integrals)

The cheapest way to get every analysis working, and the reference for stage 4.

1. For each nuclear coordinate $`x`$ (or each normal mode), SCF at $`\pm h`$ (default 0.005 bohr),
   started from the reference orbitals, as `tc-tls` does. $`\rho^{(x)}`$ by central difference on
   the grid; TFVC weights at each geometry, so (R4) is automatic.
2. Rigid following and deformation (R5); atomic charge and dipole fluxes (R6); the harmonic
   components of $`\delta\rho_A^{(x)}`$ to $`l_{\max}`$.
3. Checks: translation sum rule (R3) on the grid; $`\sum_A q_A^{(x)} = 0`$; the polar tensor (R7)
   against the finite-difference molecular dipole derivative; Hirshfeld against TFVC.
4. Output: a table per coordinate (charge flux, dipole flux, deformation norm per atom), and
   cube files of $`\delta\rho_A^{(k)}`$ for VESTA.
5. Second derivatives along a mode from the same SCFs: $`\rho^{(kk)}`$ by the three-point formula.

Cost: $`2\times 3N`$ SCFs (or $`2\times`$ modes). Fine for NH3, urea, gly-ala.

### Stage 2 — the electric-field response on TFVC atoms

1. Generalise `make_SCF_atomic_polarisability` from the stockholder weight to the partition in
   force (Hirshfeld or TFVC, LKO on), and from charge and dipole to the harmonic components of the
   atomic response to $`l_{\max}`$.
2. Checks: atomic polarisabilities sum to the molecular one (with the charge-flux term); finite
   field against CPHF.
3. Refractive index from TFVC atomic polarisabilities through the existing unit-cell machinery.

### Stage 3 — the Hartree–Fock gradient, to validate the derivative integrals

1. Revive the gradient in `MOLECULE.MISC` in current Foo and house style, using the density
   matrix in memory, not the archive.
2. Test each term against a finite difference of its own energy term (the old code's
   `make_fd_gradient` pattern): overlap (energy-weighted density), kinetic, nuclear attraction
   (including Hellmann–Feynman), two-electron (`make_ERI_derivatives`), nuclear repulsion.
3. Total gradient against a finite difference of the SCF energy, NH3 and urea, STO-3G and
   def2-SVP, spherical and Cartesian shells. Fix integrals as found.

This is the stage most likely to find bugs; nothing else depends on the integrals until it passes.

### Stage 4 — CPHF for nuclear displacements (RHF)

1. Right-hand side per coordinate: the skeleton Fock derivative
   $`F^{(x)} = h^{(x)} + G^{(x)}(D)`$ with the derivative integrals of stage 3, minus
   $`S^{(x)}`$ terms; $`U^{(x)}_{\rm oo} = -\tfrac12 S^{(x)}_{\rm oo}`$.
2. Solve with `solve_CPHF_equations` for all $`3N`$ at once (it already takes many perturbations;
   check its storage at $`3N`$ for urea, 24 perturbations).
3. $`D^{(x)}`$ from $`U^{(x)}`$, then $`\rho^{(x)}`$ by (R1), including the following-basis term
   (basis-function gradients on the grid already exist for DFT).
4. Checks: against stage 1 point by point; the translation sum rule (R3) analytically; the dipole
   derivative against finite differences; and the Hessian against finite differences of the
   stage-3 gradient, as an overall check.

DFT: CPKS needs the exchange–correlation kernel on the grid; not planned until RHF is done. For
DFT densities, stage 1 serves.

### Stage 5 — second derivatives and the thermal average

1. $`\rho^{(kk)}`$ along each mode by a finite difference of analytic first-order responses
   (one CPHF at $`\pm h`$ along the mode), or from stage 1.
2. The averaged form factor (R8) for each TFVC atom: the convolution term, the first-order
   deformation paired with the atom's own motion, and the second-order deformation. The modes and
   $`\langle Q_k^2\rangle`$ from the mode model.
3. Checks against `tc-tls` §6 (stiff modes averaged numerically with an SCF at each displacement)
   on urea: the same physics, so the structure factors should agree to second order.
4. A `partition_model=` for refinement, with the corrections fitted to the $`s^l`$ Gaussians of
   the SAM paper's form-factor fits.

### Stage 6 — force-field and optical outputs

1. Charge, charge flux, dipole flux and atomic polarisability tables for a force field, in a
   plain format (one atom per line, internal-coordinate fluxes optional).
2. Mode effective charges and the vibrational dielectric tensor (R9), split by atom; added to the
   electronic part from stage 2.
3. IR intensities from (R7) against the analytic dipole derivative, as the check.

### Stage 7 (optional) — an auxiliary or one-centre representation of the response

Carry $`\delta\rho_A^{(x)}`$ in the TFVC harmonic expansion with fitted radial Gaussians, or in an
auxiliary basis as in auxiliary density perturbation theory, if the cost of stage 4 on a grid
becomes the bottleneck.

## Decisions for Dylan

1. RHF first for the analytic route (stages 3–4), DFT through stage 1 only until then?
2. TFVC as the partition throughout, Hirshfeld kept only for comparison?
3. Test systems: NH3 (short tests), urea and gly-ala (as for the paper)?
4. Stage order: 1 and 2 first (no new integrals, quick results and a paper of their own), or 3
   first to settle the integrals early?
