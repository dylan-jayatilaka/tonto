# TASK: the hydrogen correction to the spherical Salvador atom (`sph-tfvc+h`)

Working document. The user-facing description is in `docs/REPORT_ON_SALVADOR_MODELS.md` §1,
and the results will go into its §4.

## 1. The idea

A hydrogen atom's density has no core, and the maximum of its partitioned density
$`w_H\rho`$ sits along the bond, not at the nucleus. A spherical average about the nucleus
therefore puts the atom's peak on a shell and smears it. The correction (suggested by a
colleague, and close to Sheldrick's old idea of refining a hydrogen at its density peak)
is to average about the maximum instead, keep the vector $`\boldsymbol\delta`$ from nucleus to
maximum, and correct the position after each refinement step. The hope is to cure the
non-convergence of the all-spherical model (`sph-tfvc`), whose hydrogens run away or
oscillate (report §4).

Started 2026-10-20 on branch `sph-tfvc-h`, off `develop` at 4211c2b8. The same branch renames
the model from TFVA to TFVC (topological fuzzy Voronoi *cell*): the keywords `tfva`,
`sph-tfva` and `oc-sph-tfva` became `tfvc`, `sph-tfvc` and `oc-sph-tfvc`, and
`make_sph_TFVA_atom_FFs` became `make_sph_TFVC_atom_FFs`. The old spellings are not
accepted; no test or runfile used them.

## 2. Step 0: where the maximum is (2026-10-20)

Urea at the CIF geometry, RHF, `oc-salvador` radii, with the existing
`put_ed_between_atoms`, `put_sa_cell_fn_between_atoms` and `put_sa_ed_between_atoms`
keywords on the two N–H bonds (H1→N1, H3→N1; 201 points). The maximum of $`w_H\rho`$
along the bond, measured from the H nucleus:

| basis | bond | R(N–H) / bohr | max at / bohr | / Å | $`w_H\rho`$ at nucleus | at max | cell boundary ($`w_H = 1/2`$) / bohr |
|---|---|---|---|---|---|---|---|
| STO-3G | H1–N1 | 1.948 | 0.175 | 0.093 | 0.314 | 0.332 | 0.575 |
| STO-3G | H3–N1 | 1.791 | 0.224 | 0.118 | 0.352 | 0.381 | 0.546 |
| def2-SVP | H1–N1 | 1.948 | 0.097 | 0.052 | 0.338 | 0.351 | 0.428 |
| def2-SVP | H3–N1 | 1.791 | 0.099 | 0.052 | 0.407 | 0.425 | 0.412 |

So the maximum is interior to the cell (the boundary is two to three times further out), a
line search along the bond finds it, and the offset is of the order of the famous 0.1 Å
X–H shortening of the independent atom model. Inputs and outputs:
`/private/tmp/claude-503/-Users-dylan-tonto/1fa0faa2-5478-49d7-80d4-e5f8ec36fd37/scratchpad/step0_*`
(session scratch; the numbers above are the record).

A caution from the report's §4: with spherical hydrogens and aspherical C, N, O the urea
refinement already converges; it is the spherical heavy atoms that make `sph-tfvc` diverge.
The correction may therefore not cure `sph-tfvc` on its own. It is still the right centre for a
spherical hydrogen.

## 3. What was built

- `ATOM.sph_center_offset` (bohr, default zero) and `ATOM:set_sph_center_offset`.
- `MOLECULE.RHO:make_H_center_offsets`: for every H, the line of $`w_H\rho`$ from the nucleus to
  its nearest Salvador neighbour (`SA_ED_between_atoms`, 201 points), the maximum refined by
  a parabola through the three points about it, the offset stored and a table printed.
  Called from `make_atom_partition_info` after `make_SA_info`, so it runs at the start of every
  refinement cycle on the current wavefunction and geometry.
- The centre `position + sph_center_offset` in `make_sph_avgd_SA_ED_v1`,
  `make_sph_avgd_HA_ED_v1`, `make_sph_TFVC_atom_FFs` and `make_sph_HA_atom_FFs` (the shell
  centre and the phase). The offset is zero unless a `+h` model is in force, so the other
  models are unchanged.
- Keywords `tfvc+h` → `oc-tfvc+h` and `sph-tfvc+h` → `oc-sph-tfvc+h`, with case rows in
  `molecule.scf.foo` (partition info, `make_X_SFs`) and `molecule.rho.foo` (form factors, the
  FF-fit curve).
- Why no separate "correct the position" step: the offset is constant through the LS step,
  so $`\partial F/\partial\mathbf r_H = \partial F/\partial\mathbf r_m`$ and the LS shift of the
  nucleus is the shift of the averaged atom. The nucleus stays the refined parameter and the CIF
  carries nuclear positions.
- `tfvc+h` (aspherical) refines identically to `oc-salvador`: the centre does not enter a rigid
  aspherical atom's structure factor. It exists to print the offsets and as a check.
- Test: `tests/long/urea_rhf_STO-3G_sph-TFVC+H_HAR`, the STO-3G case where `sph-tfvc` runs away.

### The dipolar model, `l1-tfvc`, and the stiffness keyword (added the same day)

Dylan's conclusion on seeing the `+h` argument: a dipolar correction applied to *every* atom is
the uniform model; the H offset is an ad hoc version of it with a historical resonance with
Sheldrick's trick. So:

- `FOURIER_SUMS:j1_kr_sums`: the $`j_1(kr)`$ transform of shell vectors.
- `make_sph_avgd_SA_ED_v1` and `make_sph_avgd_SA_ED_grid` take an optional `avg1` / `N1`, the
  $`l = 1`$ vector of each shell, $`\sum_j w_j W_j \rho_j \hat{\mathbf r}_j`$ over the Lebedev points.
- `make_sph_TFVC_atom_FFs` adds $`3i\,\hat{\mathbf k}\cdot\mathbf D(k)`$ to the spherical factor
  for `oc-l1-tfvc` (report §1, eq. 6). The FF-fit curve stays $`l = 0`$.
- `becke_grid= { salvador_stiffness= k }`, default 4, `BECKE_GRID:Salvador_step` loops `k`
  times. Printed after the Salvador radii table.

Sign check still to do: at the starting geometry `l1-tfvc` should give a lower R than
`sph-tfvc` (0.054 at STO-3G, report §4) and approach `oc-salvador` (0.038); a sign error would
make it worse than `sph-tfvc`.

## 4. Log

- 2026-10-20: plan agreed; step 0 measured; rename and model coded; first build clean.
- 2026-10-20, first results (urea, STO-3G, Mac release build):
  - `sph-tfvc+h`: cycle 1 offsets 0.093 Å (H1, H2) and 0.116 Å (H3, H4), as in step 0; the
    first LS fit ends at R 0.987, H3 at 0.51 Å from N1; "refinement stopped: GoF has
    increased". The same runaway as `sph-tfvc` (rerun as a control: R 0.9996). The H offset
    alone does not cure the spherical model — consistent with report §4, where it is the
    spherical heavy atoms that drive the hydrogens.
  - `tfvc+h` against `oc-salvador`: every number identical (R 0.0379, GoF 7.04, converged);
    only the offset tables and timings differ. The machinery is right.
  - The regression test is therefore `tests/long/urea_rhf_STO-3G_TFVC+H_HAR` (`oc-tfvc+h`),
    not the spherical job.
- 2026-10-20, second build (the `+l1` model and `salvador_stiffness=`), Dylan asked for
  def2-SVP as the minimum basis to judge on. Results are in report §4 (the table "The hydrogen
  correction and the dipolar term"). In short: `sph-tfvc+h` fails at def2-SVP too (H3 to 0.39 Å
  from N1 by cycle 3, 100 cycles); `l1-tfvc` converges, R 0.0327, GoF 7.71, hydrogens
  short and hot. Sign check passed: `+l1` is better than `sph-tfvc` in both bases, not worse.
  STO-3G `+l1`: R 0.0437, GoF 8.74; with stiffness 2: R 0.0404, GoF 7.78.
  Runs: `scratchpad/svp_*` and `scratchpad/urea_*` of this session (the table is the record).
- 2026-10-20, third build: the general multipole. Dylan renamed the family `l0-tfvc` (=
  `sph-tfvc`), `l1-tfvc`, `l2-tfvc`, `l3-tfvc`; `FOURIER_SUMS:multipole_kr_sums` and
  `spherical_bessels`, `DIFFRACTION_DATA.TFVC_l_max`, harmonics from `make_solid_harmonics`
  (orthonormal real $`Y_{lm}`$, so $`\rho_{lm} = 4\pi\sum_j w_j W_j \rho_j Y_{lm}`$ and
  $`f = \sum i^l Y_{lm}(\hat k)\sum_i \mathrm{wts}_i \rho_{lm}(r_i) j_l(kr_i)`$). Checks: the
  general code at $`l = 1`$ reproduces the hand-written dipole run to every printed digit in
  both bases; the sequence $`l = 1, 2, 3`$ improves R, GoF, both N–H lengths and both U_iso
  monotonically towards the Salvador atom (report §4 table). def2-SVP `l3-tfvc`: R 0.0216,
  GoF 4.12, N–H 1.025 / 0.985 Å (Salvador 0.0190, 3.54, 1.038 / 1.026). Test added:
  `tests/long/urea_rhf_STO-3G_l1-TFVC_HAR` (reference to be blessed on achari2).
- Not done: `l4` and beyond (the case tables stop at 3; a numeric suffix would need parsing);
  `l<n>` variants of `tfvp`/`tfvh`/`exphar` radii; the FF-fit writer stays $`l = 0`$.
- 2026-10-20, evening, after the merge: Dylan asked for results on glycyl-L-alanine and
  L-alanine ("L_glycine" in his message; the data set has no glycine) as well as urea, at
  def2-TZVP as well as def2-SVP, and with B3LYP and PBE0 as well as RHF, in a new page
  `docs/REPORT_ON_LN_TFVC.md`. Ninety refinements: `~/tonto-runs/2026-10-20_ln_tfvc/` (inputs
  from `gen.py`, runner `run_batch.sh`/`run_one.sh`, `summ2.py` → `results.csv`, `tables.py` →
  `tables.md`, `assemble.sh` builds the report from `head.md` + `tables.md` + `tail.md`). The
  gly-L-ala jobs ran on the Mac (8 at a time, 20–26 min each at def2-TZVP), urea and L-alanine on
  achari2 (6 at a time, `~/tonto-runs/2026-10-20_ln_tfvc` there too). Every job exit 0; the
  spherical atom hit the 25-cycle cap on all six urea jobs and "GoF rose" on three L-alanine
  ones. The urea working CIF is the test suite's (817 reflections); the amino-acid CIFs are the
  working copies from `~/tonto-runs/2026-10-18_overnight_maps/`. The model text and the urea
  table moved from `REPORT_ON_SALVADOR_MODELS.md` to the new page, which that page now points to.
- Earlier note, now done: the general multipole, `sph-tfvc+l2` and beyond — `FOURIER_SUMS:make_solid_
  harmonics(Y,pts,l_max)` already gives real harmonics at points, so each shell projects onto
  $`Y_{lm}`$ and transforms with $`j_l`$: $`f = \sum_l 4\pi i^l \sum_m Y_{lm}(\hat k)\int 4\pi r^2
  \rho_{lm}(r) j_l(kr)\,dr`$. Check the harmonics' normalisation first.
- 2026-10-22, the centroid models (branch `sph-tfvc-centroid`, c48e746b): `sph-tfvc+c` centres
  every atom's spherical average on its TFVC electron centroid $`\boldsymbol\delta_A=-\boldsymbol\mu_A/N_A`$,
  `sph-tfvc+hc` only the H atoms (`MOLECULE.RHO:make_centroid_offsets`). Motivation: the
  density maximum of `+h` is a Gaussian-basis artefact (the exact density has a cusp and falls
  monotonically from H to the bond minimum), and the centroid removes the atomic dipole.
  - Zero-ADP urea RHF/def2-SVP against `tc-stewart` (Σ|ΔF|/Σ|F|): TFVC(0) 2.19%, TFVC(1) 2.18%,
    `+hc` 2.38%, `+c` **17.4%** (rms |ΔF| 0.80 e above sinθ/λ 0.9). Moving a heavy atom's
    spherical density moves its core; offsets O, N 0.03 Å, C 0.004 Å are enough to wreck high angle.
  - The TFVC H centroid lies **away** from the bonded atom, 0.16–0.17 Å: the TFVC wall is at the
    density minimum, ~0.42 bohr from H on N–H, so the H atom is cut off on the bond side. The H
    dipole is parallel to H→N (cos 0.999), i.e. electrons displaced outward. Not a sign error.
  - Refinements, the 18 TFVC(0) cases with LKO (`~/tonto-runs/2026-10-22_sph_tfvc_centroid/`):
    `+c` converges sensibly in 5 of 18, `+hc` in 10 (TFVC(0): 9); four DFT/SVP jobs end in
    SCF failure or GoF near 100–240 with collapsed H. GoF is worse than TFVC(0) in nearly every
    case. X–H come out **shorter** than TFVC(0) (L-alanine 0.96–0.99 Å against 1.05–1.08; full
    atom 1.07–1.08): with the H density pushed outward the nucleus moves in to compensate.
  - Conclusion: the centroid does not cure the spherical atom, and is worse than the nucleus as
    centre. TFVC(1) remains the smallest model that converges everywhere.
- 2026-10-22, Dylan: the idea has not worked; the code for `+h`, `+c` and `+hc` is removed
  (`make_H_center_offsets`, `make_centroid_offsets`, `ATOM.sph_center_offset`), with the test
  `urea_rhf_STO-3G_TFVC+H_HAR`. The findings go to the SAM paper's Supporting Information for
  Martí. The multipole keywords are renamed to match the paper: `tfvc-l0` … `tfvc-l3`
  (internal `oc-tfvc-l1` … `oc-tfvc-l3`; `tfvc-l0` is `oc-sph-tfvc`); the old `l<n>-tfvc`
  spellings are not accepted.

## 5. Open

- `hart` is always `oc-hirshfeld`; no change there.
- The BUSTER / FF-fit writer: with `sph-tfvc+h` the fitted H form factor belongs at the
  shifted position, and the writer puts out the nucleus. Not done.
- `sph-tfvh+h`, `sph-tfvp+h`, `sph-exphar+h`: one table row each if wanted.
