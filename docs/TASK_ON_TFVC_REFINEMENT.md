# TASK: Hirshfeld-type refinement with TFVC atoms, and the SAM paper

Working document (see `CLAUDE.md` §1). It records the TFVC refinement work of October 2026 and
the paper written from it with Martí Gimferrer. It subsumes `TASK_SPH_TFVC_H.md`; that page's
full log is in git history (last version on `tfvc-names`, before this page). The user-facing
pages are `docs/REPORT_ON_SALVADOR_MODELS.md` (the models) and `docs/REPORT_ON_LN_TFVC.md` (the
multipole family). The full results live in the paper, not in a repo report.

## 1. The question

What is the best standard refinement within the rigid, independently vibrating atom
approximation? It is tested against neutron structures, with the TFVC atom (topological fuzzy
Voronoi cell; Salvador and Ramos-Cordoba 2013) as the partition. The atom has a wall on each
bond. The variables are:

| dimension | values |
|---|---|
| where the wall sits | density minimum (TFVC), electrostatic-potential minimum (TFVC(V)) |
| wall stiffness $`k`$ (`salvador_stiffness=`) | 1, 2, 4 (default 4) |
| method | RHF, PBE0, B3LYP; r2SCAN run but not yet in the paper |
| basis | def2-SVP, def2-TZVP |
| surroundings | isolated molecule, self-consistent cluster charges (8 Å), explicit cluster |
| angular order of the atom | full, TFVC($`l_{\max}`$) for $`l_{\max}`$ = 0–3 (TFVC(0) = SAM, the spherical atom) |
| crystals | urea (123 K), L-alanine (23 K), xylitol (122 K), Gly-L-Ala (12 K) |
| measures | GoF first; X–H and heavy-atom bond lengths minus neutron; S12 of the H ADPs (neutron ADPs put on the X-ray scale by Blessing's additive $`\Delta U`$) |

Also measured: the non-rigidity $`\varepsilon`$ of each atom and its charge flux $`N' = dN/dR`$,
as the wall moves along a bond. These ask whether the partitioned atom follows its nucleus
rigidly, as the refinement model assumes.

Rule: results are reported per wavefunction and never averaged across wavefunctions. Rows are
grouped by basis (def2-SVP, then def2-TZVP), each in the order RHF, PBE0, B3LYP.

## 2. Where the paper is

- **ShareLaTeX (the master copy):** https://sharelatex.gwdg.de/project/69c2e232e9ef6367effccae2,
  project `2026-SAM-Project`. Files: `main.tex`, `SI.tex` (a separate document, S-numbered),
  `SI_neutron.tex` (`\input` by `SI.tex`), `references.bib`, `FIGURES/`. Target journal: Acta
  Cryst. A. Dylan and Martí edit it live, so re-read the exact text before every edit.
- **There is no local copy of the whole paper.** Every piece written here was generated in a
  run folder (below) and pasted in through the browser. The local sources are:

| piece | local source |
|---|---|
| neutron SI section, `tab:neutron`, `fig:neutron`, `fig:neutronk` | `~/tonto-runs/2026-10-23_neutron_summary/` (`SI_neutron_section.tex`, `upload/SI_neutron.tex`, `fig_neutron_*.gp`) |
| `fig:nonrigidity` | `~/tonto-runs/2026-10-22_wall_scan/fig_nonrigidity.gp`, `FIGURES/` |
| wall positions, `tab:radiiT`, `tab:radiiV` (main), `tab:SradiiT/V` (SI) | `~/tonto-runs/2026-10-25_tfvc_radii/` (`main_radii_section.tex`, `SI_radii.tex`) |
| X–H scan figures for the SI | `~/tonto-runs/2026-10-24_xh_scans/FIGURES/fig_xh_*` |
| introduction's new bib entries | `~/tonto-runs/2026-10-27_introduction/new_references.bib` |
| multipole convergence `tab:lconv`, SI Tables S1–S6 | `docs/REPORT_ON_LN_TFVC.md` |

- Editing recipe (Claude in Chrome): the editor is CodeMirror 6.
  `document.querySelector('.cm-content').cmView.view` gives the view, and
  `view.dispatch({changes:[{from,to,insert}]})` makes the edit. Find the anchor text and
  dispatch in the same JavaScript call. Keep each call short, because long ones drop the
  extension. Pass long LaTeX in pieces.

## 3. Calculation files

Every folder has a `README.md` or `README.txt`. Each batch folder also holds its own
`tonto_bin`, `tonto_source_uncommitted.diff`, `done.log` (exit code and time per job) and
`compare.py` (against neutron).

| folder (`~/tonto-runs/`) | what |
|---|---|
| `2026-10-20_ln_tfvc` | 90 refinements of the multipole family TFVC(0–3) and full TFVC; urea, L-alanine, Gly-L-Ala × RHF/PBE0/B3LYP × def2-SVP/TZVP; no LKO cutoff |
| `2026-10-22_ln_tfvc_lko` | the same with the LKO cutoff (Laqua 2018) on |
| `2026-10-22_sph_tfvc_check` | zero-ADP check: full TFVC agrees with `tc-stewart` to 2e-5 on a fine grid |
| `2026-10-22_sph_tfvc_centroid` | the failed centroid-centred spherical atoms (`+c`, `+hc`) |
| `2026-10-22_tpfvc` | first TFVC(V) refinements, urea |
| `2026-10-22_urea_radii` | urea wall positions, isolated molecule, 9 wavefunctions (`walls.json`) |
| `2026-10-22_urea_cluster` | urea, explicit 7-molecule cluster |
| `2026-10-22_wall_scan` | urea wall scans, RHF/PBE0/B3LYP def2-SVP; `fig_nonrigidity.gp` |
| `2026-10-23_soft_walls` | urea benchmark: isolated and cluster charges; TFVC/TFVC(V) × $`k`$ 1, 2, 4 × 6 wavefunctions |
| `2026-10-23_l_alanine` | L-alanine benchmark: isolated, cluster charges, explicit cluster (`alanine_cluster`, failed, §6) |
| `2026-10-23_xylitol` | xylitol benchmark, cluster charges; one-cycle exact vs RIJCOSX timing |
| `2026-10-23_gly_ala` | Gly-L-Ala benchmark, cluster charges, 12 K (coordinates inverted to the L enantiomer) |
| `2026-10-23_neutron_summary` | `collect.py` → `deviations.csv` (every bond and H for every run); `prepare.py`; paper figures |
| `2026-10-23_wall_scans` | wall scans of all bonds, four crystals, B3LYP/def2-TZVP, $`k`$ 1–4 |
| `2026-10-24_r2scan` | 72 refinements, the benchmark with r2SCAN in place of B3LYP; `compare_*.out`, `timing.out`; fine-grid and tight-SCF checks on urea |
| `2026-10-24_xh_scans` | X–H wall scans for the missing wavefunctions of urea and L-alanine (all done); `fig_xh.py` |
| `2026-10-25_tfvc_radii` | wall positions, 4 crystals × 6 wavefunctions, cluster charges |
| `2026-10-27_introduction` | bib entries for the new introduction |
| `2026-10-29_rigidity_test` | `test.py`: does the lower-$`\varepsilon`$ wall refine closer to neutron? |

X-ray and neutron CIFs: `~/Dropbox/tonto_data/xray_neutron_set/`. The Chodkiewicz X-ray CIFs hide
their reflections in `_iucr_refine_fcf_details`, so append the loop before use. The references
cited are in `~/Dropbox/manuscripts/` (Medvedev 2017, Brux 2025, Chodkiewicz 2025, …).

## 4. Code

### Keywords

| keyword | where it goes | what it does |
|---|---|---|
| `partition_model= tfvc` (also `salvador`, `oc-salvador`) | `crystal= { xray_data= }` | full TFVC atom, wall at the density minimum |
| `partition_model= tfvc-v` (also `tpfvc`, `oc-tpfvc`) | same | wall at the minimum of the molecular electrostatic potential |
| `partition_model= tfvc-p`, `tfvc-h` (also `tfvp`, `tfvh`) | same | wall at the promolecule density minimum / where the two atoms' spherical densities are equal |
| `partition_model= tfvc-l0` … `tfvc-l3` (`tfvc-l0` = `sph-tfvc`) | same | TFVC atom expanded in harmonics about its nucleus up to $`l_{\max}`$ |
| `partition_model= tfvc-p-l0`, `tfvc-h-l0` | same | spherical versions of the two above |
| `salvador_stiffness= k` | `becke_grid= { }` | wall stiffness, the number of Becke smoothing steps; default 4 |
| `lko_cutoff= r` | `becke_grid= { }` | LKO cutoff for the grid and the TFVC atom; default 5 bohr |
| `make_salvador_info` | molecule | the TFVC radii and atom data |
| `put_salvador_radii` | molecule | radii at the density and potential minima, every neighbour pair |
| `put_tfvc_wall_scans` | molecule | $`\varepsilon`$ and $`N'`$ against wall position, every bonded pair, $`k`$ = 1–4 |
| `put_tfvc_wall_scans_xh` | molecule | the same for X–H bonds only (worktree `~/tonto-wt/wall-scans-xh`, uncommitted) |
| `fit_sph_atom_ffs` | molecule | Gaussian fit of the spherical form factors, $`l = 0`$ only (`docs/TASK_SPHERICAL_FF_FIT.md`) |

### Where it is implemented

| what | file and routine |
|---|---|
| keyword spellings and their standard names, `TFVC_l_max` | `foofiles/diffraction_data.set.foo`, `set_partition_model` |
| which partitions need the TFVC data; structure-factor route | `foofiles/molecule.scf.foo` (two case tables) |
| form factors: full atom, spherical and multipole atoms | `foofiles/molecule.rho.foo`, `make_Salvador_atom_FFs`, `make_sph_TFVC_atom_FFs`, `make_sph_avgd_SA_ED_v1`, `make_sph_avgd_SA_ED_grid` |
| radial transforms $`j_l(kr)`$ and harmonics | `foofiles/fourier_sums.foo`, `multipole_kr_sums`, `spherical_bessels`, `make_solid_harmonics` |
| radii at the density minimum and at the potential minimum | `make_Salvador_radii`, `make_Salvador_radii_ESP`, `ESP_minimum_for`, `put_Salvador_radii`, `make_SA_info`: `molecule.grid.foo` on `develop`; still in `molecule.rho.foo` on `tfvc-names` until it is merged with `develop` |
| potential on a grid, used for the V wall | `foofiles/molecule.grid.foo`, `make_EP_grid_r` |
| wall stiffness and the wall step | `foofiles/becke_grid.foo`, `read_Salvador_stiffness`, `Salvador_step` |
| wall scans, $`\varepsilon`$, $`N'`$ | `foofiles/molecule.har.foo`, `put_TFVC_wall_scans` and helpers |
| keyword dispatch | `foofiles/molecule.main.foo` |

### Branches

| branch | state |
|---|---|
| `develop` | has TFVC, TFVC(V) as `oc-tpfvc`, `tfvc-l0`–`l3`, `salvador_stiffness=`, LKO, and the fix of the RHO↔GRID use cycle (881babfa, fast-forwarded 2026-11-11 after a clean build, `ctest -L short` 88/88 and the three Salvador/TPFVC tests on the Mac) |
| `tfvc-response` | adds `put_tfvc_wall_scans` (318c8dde), not merged |
| `tfvc-names` | 318c8dde plus the renames (ce0b3dbc, pushed): `oc-tpfvc` → `oc-tfvc-v`, `tfvp`/`tfvh` → `tfvc-p`/`tfvc-h`, docs updated; the old spellings still accepted. Not merged |
| `fix-rho-grid-cycle` (worktree `~/tonto-wt/fix-rho-grid-cycle`) | merged into `develop`; the branch and worktree can be deleted |
| worktree `~/tonto-wt/wall-scans-xh` | **uncommitted** `put_tfvc_wall_scans_xh`; its own build log says FAILED (the scans used a build made earlier) |

Merging `develop` into `tfvc-names` conflicts only on comments in two of the moved routines.
GitHub CI cannot check any of this until the account lock is lifted (GitHub Support ticket
#4988445; see the watch list of the task register).

## 5. What was found

Numbers are in the run folders and the paper; this is the gist.

- **The spherical atom TFVC(0) fails.** The hydrogens run away or oscillate because the
  heavy atoms' bonding density is missing. Moving the centre does not help: the H density
  maximum (`+h`) and the electron centroid (`+c`, `+hc`) were both tried, and that code was
  removed. The multipole family converges and improves monotonically with $`l_{\max}`$.
  Urea def2-SVP TFVC(3) gives GoF 4.12, against 3.54 for the full atom.
- **Wall positions** (`tab:radiiT/V`):
  - On N–H and O–H the density wall is 0.16–0.24 Å from H and the potential wall 0.35–0.38 Å,
    a gap of 0.12–0.19 Å.
  - On C–H the two walls nearly coincide, at 0.39–0.42 Å.
  - The density wall moves 0.02–0.04 Å with the wavefunction; the potential wall moves
    ≤ 0.002 Å.
  - The density minimum is found to $`R/1000`$.
- **Benchmark against neutron:**
  - Cluster charges lower the GoF by about 10% and match the explicit urea cluster at about
    1/40 of the cost.
  - Urea: TFVC(V) at $`k`$ = 2 gives the best GoF and N–H.
  - L-alanine: the surroundings barely matter, and TFVC(V) makes NH₃⁺ about 0.02 Å short.
- **Across stiffness, def2-TZVP:**
  - The TFVC GoF falls steadily from $`k`$ = 4 to 1 for every crystal and method; urea B3LYP
    goes 2.189, 2.117, 2.096.
  - The TFVC X–H mean error stays flat at 0.010, 0.008 and 0.010 Å.
  - The TFVC(V) error grows: 0.018, 0.023 and 0.027 Å.
- **Does non-rigidity pick the better wall?** (B3LYP/def2-TZVP, polar X–H)
  - At $`k`$ = 1 the lower-$`\varepsilon`$ wall gives the closer bond length on 13 of 14 bonds.
  - At $`k`$ = 2 and 4 it is chance (5 and 6 of 14).
  - C–H bonds cannot discriminate. For S12 it is chance at every $`k`$.
  - So $`\varepsilon`$ predicts the GoF across stiffness, but not reliably which wall gives
    the better hydrogens. One wavefunction so far.
- **r2SCAN** (72 runs): urea def2-TZVP $`k`$ = 4 GoF is 2.131 for TFVC and 2.108 for TFVC(V),
  close to PBE0 (2.122, 2.105) and better than B3LYP (2.189, 2.157). With exact integrals in
  Tonto it is not cheaper than B3LYP (0.9–1.3×). Brux's saving comes from ORCA's RI-J.
- **Density quality** (Medvedev 2017, maximum normalised errors of atomic densities): PBE0
  1.82, SCAN 2.11, B3LYP 2.12. r2SCAN is not SCAN.

## 6. To do

### In the paper

1. Check that the new Introduction is in `main.tex` (search for "in effect the first test of
   how stiff"); the tab stuck at "Loading…" after the insertion.
2. Add the six entries of `new_references.bib` to `references.bib`.
3. Upload `SI_radii.tex` and add `\input{SI_radii}` after `\input{SI_neutron}` in `SI.tex`.
4. Delete the old urea-only walls section (`tab:walls`), if Dylan agrees.
5. X–H scans: the PBE0 runs are finished.
   - Run `fig_xh.py wf urea` and `fig_xh.py wf L_alanine`.
   - Upload all `fig_xh_*.pdf` and add an SI section with them.
   - Write the X–H summary in "The non-rigidity of the atoms".
6. Run `test.py` for the other five wavefunctions, then write the non-rigidity-as-predictor
   result and the across-$`k`$ result. These bear on the abstract's claim that the most rigid
   atoms have soft walls near the potential minima. The default $`k`$ = 4 is the stiffest
   tested, and the GoF prefers $`k`$ = 1.
7. Open questions for Dylan:
   - Cite expHAR (Chodkiewicz 2025) in the abstract?
   - Does NoSpherA2 (Kleemiss 2021) really offer TFVC?
   - Fill the blue placeholder for the TFVC paper with Florian.
8. Rename `oc-tpfvc` to TFVC(V) everywhere in the paper's text, as decided.

### The matrix of tests

| | urea | L-alanine | xylitol | Gly-L-Ala |
|---|---|---|---|---|
| isolated, RHF/PBE0/B3LYP × SVP/TZVP × 2 walls × $`k`$ 1, 2, 4 | done | done | – | – |
| cluster charges, same | done | done | done | done |
| explicit cluster | done (7 molecules) | **failed**: B3LYP/SVP SCF does not converge (DIIS error 0.045 after 300 cycles), both walls, $`k`$ = 2 | – | – |
| r2SCAN, isolated and cluster charges | done, not in paper | done, not in paper | done, not in paper | done, not in paper |
| wall positions, 6 wavefunctions | done | done | done | done |
| wall scans, all bonds, B3LYP/TZVP | done | done | done | done |
| X–H scans, 6 wavefunctions | done | done | – | – |
| multipole TFVC($`l_{\max}`$) | done ($`k`$ = 4, density wall) | done | – | done |

9. **r2SCAN into the paper**, since Brux et al. recommend it. Add it as a fourth method in every
   table and figure, per wavefunction (`tables.py`, `collect.py`/`prepare.py`, the gnuplot
   files). It also needs wall positions and X–H scans, which are not yet run.
10. **The L-alanine explicit cluster** (the zwitterion test, which decides whether cluster
    charges are enough). Retry with a better start: cluster-charge orbitals, level shift or
    damping, more cycles. Otherwise say in the paper that it could not be converged.
11. The multipole convergence tables used TFVC at $`k`$ = 4 with the density wall. Redo them
    with the model the benchmark chooses.

### The Gaussian-fit refinements — not started

The point of the Gaussian fits is to make the method practical: fitted form factors make the
structure factors fast, with an FFT for large structures, which is the new idea in the
paper. The fits themselves have been studied:
- The $`s^l`$ Gaussian form is used.
- The criterion is 0.001 e absolute per component.
- The exponents are even-tempered and shared per atom.
- These were done on Hirshfeld atoms, urea RHF/def2-SVP; `tonto-lm-form-factor-fits` memory.
- `fit_sph_atom_ffs` exists for $`l = 0`$.
- They are written up in the paper, with Tables S4–S6.

**No refinement with fitted form factors has been run.** Steps:
- Redo the fits on TFVC atoms with the chosen wall and $`k`$.
- Extend `GAUSSIAN_FF_FIT` to $`l > 0`$ with the $`s^l`$ prefactor.
- Refine with the fitted form factors in def2-SVP/TZVP, B3LYP, PBE0 and r2SCAN.
- Compare GoF, bond lengths and S12 with the exact-form-factor benchmark.
- Then time it, and try the FFT.

Later, Dylan's suggestion: benchmark RIJCOSX for accuracy and speed on the explicit clusters,
together with the Gaussian form factors.

### Code

12. Done 2026-11-11: `fix-rho-grid-cycle` merged to `develop`.
13. `tfvc-names`: merge `develop` in (comment conflicts only), bless the changed outputs on
    achari2 (never on the Mac), then merge to `develop`.
14. Merge `tfvc-response` (the wall scans) and decide whether `put_tfvc_wall_scans_xh` is kept.
    If it is, fix its build.
15. Not done from the old task: $`l \ge 4`$; `l<n>` variants of `tfvc-p`/`tfvc-h`/`exphar`; the
    form-factor writer stays $`l = 0`$.
