# Stopping the X-ray constrained fit where its energy matches the crystal

A science task, **to be planned** (Dylan, 2026-10-10). The facts and the derivation are in
`docs/REPORT_ON_XCW_CONVERGENCE.md`, section 8.6. This page records the discussion that led to
it, with Dylan's questions, what was computed on the way, and the steps a plan would have to
cover.

## 1. The idea

The statistical criteria for lambda (leave-one-out, GCV, AIC, BIC and their sigma-free forms)
either do not turn on urea or turn where the fitted wavefunction's energy has risen by 0.5 to
1 hartree, 14 to 27 times the lattice energy. A physical stopping rule instead: the energy rise
$`\Delta E`$ of the XCW, evaluated with the free-molecule Hamiltonian, should equal the
molecule's own deformation energy in the crystal, which to second order equals minus its
polarisation energy (report equations 27 to 29). Estimate that energy independently, before
fitting, and stop the scan where $`\Delta E`$ reaches it. The rule uses no sigmas, and since
$`\Delta E`$ rises steadily with lambda it gives one lambda.

## 2. The discussion, tidied

**Where it started.** The urea def2-TZVP scan (`~/tonto_runs/xcw_2026-10-09/urea_tzvp4`,
lambda 2 to 6 in steps of 0.1) finished with no leave-one-out minimum. The sum was 2236 at 6
and falling by about 7 per step, the fall shrinking by about 0.13 per step.

> *Dylan: How much is the GoF now, and how much will it be, when?*

GoF 1.083 at lambda 6. Fitting the per-step changes from 4.8 to 6.0 with straight lines gives
the leave-one-out minimum near lambda 11 to 12 and $`\chi^2`$ levelling near 11, at
GoF about 1.05. So the rest of the scan buys about 0.03 in GoF.

> *Dylan: What is the change in energy, from minimum?!*

The energy rises a steady 16.8 mE_h per step of 0.1, with no sign of slowing: 0.50 hartree at
lambda 2, 0.84 at 4, 1.17 at 6, and an extrapolated 2.0 to 2.2 hartree at 11 to 12.

> *Dylan: For context, what is the estimated sublimation energy of the crystal, is it measured?*

Measured. Urea: sublimation enthalpy 93.8 kJ/mol at 298 K, the average of the measurements;
lattice energy 102.5 kJ/mol = 39.0 mE_h with vibrations removed (Reilly and Tkatchenko 2013,
Table IV; Otero-de-la-Roza and Johnson 2012 give 99.4). Ammonia: 29.8 and 37.2 kJ/mol
(14.2 mE_h). Both papers are in `~/Dropbox/manuscripts/`. Every statistical minimum on urea costs
14 to 30 lattice energies; the extrapolated def2-TZVP leave-one-out minimum would cost about 55.

> *Dylan: Plot at the BIC minimum. Note the BIC minimums for the other bases. Do you have any
> other scale-free minima? Is there a way to estimate a tolerable rise from the binding energy,
> or some other way?*

The BIC and BIC$`_\sigma`$ minima are in the report's criteria table (section 8.2). The scale-free
AIC has no minimum on any urea set: its penalty, 2 per parameter, is weaker than BIC$`_\sigma`$'s
$`\ln N \approx 6.7`$, so it always stops later. For a tolerable rise, the binding energy itself
is no bound. What bounds the rise is the polarisation, which is part of the binding.

> *Dylan: But we are observing an energy* increase *whereas the polarization energy is an energy*
> decrease *-- how to relate the two? Except as a defined heuristic?*

They are two parts of the same energy, and the relation is exact to second order, not a
heuristic. Report equations 27 to 29: the deformation energy
$`\Delta E_\mathrm{def} = \langle\Psi|H_0|\Psi\rangle - E_0 \ge 0`$ is a rise, by the variational
principle for $`H_0`$. The polarisation energy $`E_\mathrm{pol} \le 0`$ is a fall, by the
variational principle for $`H_0+V`$. To second order in $`V`$, $`\Delta E_\mathrm{def} =
-E_\mathrm{pol}`$, because the interaction gains twice what the distortion costs. Beyond second
order, $`\Delta E_\mathrm{def}`$ is still bounded by the gain in interaction. The XCW's
$`\Delta E`$ is exactly a $`\Delta E_\mathrm{def}`$: the free-molecule energy of a distorted
wavefunction.

> *Dylan: I know how. One can estimate the polarization energy from calculated or assumed atomic
> polarisabilities, or perhaps the XDM model, and then the calculation can stop when the
> polarization energy of the fitted density matches that calculated from the unperturbed
> lambda = 0 wavefunction. Any comment?*

Agreed, and the identity above is what makes it principled. Comments:

1. **Make the induction self-consistent.** Compute the field from the neighbours' multipoles
   and let the induced dipoles polarise each other (Applequist, or Thole with damping). Urea's
   dipole is strongly enhanced in the crystal, so a field from the lambda = 0 moments alone
   would underestimate.
2. **Point polarisabilities give a lower bound.** They miss the compression of the density by
   the neighbours' exchange repulsion, which also raises $`\langle H_0\rangle`$. PIXEL- or
   CE-B3LYP-type decompositions estimate that term separately.
3. **Correlation is probably the larger correction.** The data come from a correlated density,
   and the Hartree-Fock energy of a correlated density lies above $`E_0`$. That is second order
   in the density change, and plausibly tens of mE_h for urea. A legitimate Hartree-Fock XCW pays
   for it too, so the target is $`-E_\mathrm{pol}`$ plus that term. It is computable: the
   Hartree-Fock energy of the relaxed MP2 or CCSD density at lambda 0.
4. **The rule gives one answer.** $`\Delta E`$ rises monotonically with lambda.
5. **Where it lands.** At a target of a few tens of mE_h it stops at lambda 0.005 to 0.01, with
   GoF² 4 to 8. So most of what the statistical criteria fit is not crystal polarisation.
6. **A cross-check already exists in Tonto.** The self-consistent cluster-charge SCF gives
   $`\Delta E_\mathrm{def}`$ directly, at the Hartree-Fock level, anisotropy included. It is
   computed in section 3 and calibrates the cheaper polarisability route.

## 3. What was computed (2026-10-10)

Self-consistent cluster charges, Hirshfeld charges and dipoles within 8 Å, RHF, urea crystal
of the test job, in `~/tonto_runs/xcw_2026-10-09/embed/def2-{SVP,TZVP}_sc2` (a free SCF, then
the embedded one restarted from its orbitals):

| basis | $`E_0`$ | in the field | $`V_{cN}`$ | $`\langle H_0\rangle`$ | $`\Delta E_\mathrm{def}`$ |
|---|---|---|---|---|---|
| def2-SVP | -223.825356 | -224.470684 | -0.659557 | -223.811127 | 14.2 mE_h |
| def2-TZVP | -224.082702 | -224.757236 | -0.692443 | -224.064793 | 17.9 mE_h |

The check that total = $`\langle H_0\rangle + V_{cN}`$ holds exactly. The initial-guess block of
the embedded run evaluates the free density in the field: -223.825356 - 0.489965 =
-224.315321, as printed. The XCW reaches these $`\Delta E`$ at lambda 0.005 (def2-SVP, GoF² 7.9,
$`p_\mathrm{eff}`$ 16) and about 0.008 (def2-TZVP, GoF² about 4.1, $`p_\mathrm{eff}`$ about 27).

**Two pitfalls found on the way.**
- **Cluster charges are silently zero under `partition_model= tc-stewart`.** The cluster
  charges come from the Hirshfeld moments, which that partition never makes. The job runs, sets
  $`V_{cN}`$ to 0 and returns the free energy. The first two embedding attempts
  (`embed/*_sc`) did exactly that. This should be a `DIE`; see TASKS_AND_HISTORY.md.
- **The embedded SCF must restart from converged orbitals** (`initial_MOs= restricted`), as
  the tests `nh3_rhf_DZP_consistent-cluster-charges` and `urea_rhf_DZP_consistent-cluster-charge_HAF`
  do. From the promolecule the charges were not applied in the one case tried; that run also had
  `tc-stewart`, so this one is not separated from the first.

The ammonia scans already start from an embedded wavefunction (`use_SC_cluster_charges=` in
`scanA`), so their $`\Delta E`$ is measured from a different reference and is not in the report.

## 4. What a plan must cover

1. **Sensitivity of the cluster-charge estimate**: radius 8 to 12 Å; charges only against
   charges and dipoles against `qq`; whether a damped short-range field changes 14 to 18
   mE_h much.
2. **The polarisability route**: atomic polarisabilities scaled by Hirshfeld volumes
   (Tkatchenko-Scheffler, or XDM), Thole-damped induced dipoles iterated in the field of the
   lambda = 0 multipoles. Compare with step 1.
3. **The correlation term**: the Hartree-Fock energy of a correlated density at lambda 0. First
   check what Tonto can make, then urea def2-SVP and def2-TZVP.
4. **The exchange-compression term**: an estimate from an explicit-neighbour cluster, or from a
   decomposition scheme. Least clear how to do; plan last.
5. **The decisive comparison**: the XCW difference density at the energy-matched lambda,
   against the embedded-minus-free density. If they look alike, the rule is picking out the
   crystal's effect. If not, the fit at that lambda is already absorbing something else.
6. **Ammonia** with a free lambda = 0 reference, so it can join the table.
7. **Literature check.** The identity is textbook (Stone 2013, eq. 2.3.22; the Hylleraas
   functional). Using it as a target for an XCW's energy rise was not found in Jayatilaka 2001,
   Grimwood 2001, Dos Santos 2014, Ernst 2017 and 2020, the Genoni 2018 review or Krawczuk 2014,
   but that was a keyword search. Read the Genoni and Macchi crystal-field papers before
   calling it new.
8. **If it holds**: a `lambda_criterion= deformation_energy` with the target as input or
   computed, beside the statistical criteria.

## 5. Where things are

- Scans: `~/tonto_runs/xcw_2026-10-09/` (README there); the def2-TZVP scan to lambda 6 is
  `urea_tzvp4`, the BIC-minimum plot job `plots/tzvp_4.0`.
- Embedding runs: `~/tonto_runs/xcw_2026-10-09/embed/`.
- Criteria from scan stdouts, BIC$`_\sigma`$ and energies included, merged over several scan
  folders, with each criterion's minimum: `plots/criteria.py N dir1 dir2 ...`, for example
  `python3 plots/criteria.py 817 urea_tzvp urea_tzvp3 urea_tzvp4`.
