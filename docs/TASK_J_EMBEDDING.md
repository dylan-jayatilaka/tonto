# TASK: Coulomb embedding by the density of the nearest neighbours

Working document, opened 2026-10-24. Branch `j-embedding`, worktree `~/tonto-wt/j-embedding`.
The method, the keywords, the code table, the checks done and the literature are in
[`REPORT_ON_CRYSTAL_EMBEDDING.md`](REPORT_ON_CRYSTAL_EMBEDDING.md); this page keeps the plan, the
log and what is still open.

## Status (2026-11-18)

| stage | state |
|---|---|
| 1. neighbours, rotated density matrices | done, `ed0862d0`; checks in `~/tonto-runs/2026-11-13_j_embedding_stage1` |
| 2. Coulomb integrals, nuclei, removal from the cluster charges, `embedding_radius=` | done, `3ef3dcd0`; checks in `~/tonto-runs/2026-11-13_j_embedding_stage2` |
| 2b. Schwarz screening, `use_embedding_schwarz_test=` | done, `eaa20647`, `1561bbbc`; `~/tonto-runs/2026-11-13_j_embedding_schwarz` |
| 3. update in the self-consistent loop, `embedding_update_tolerance=` | done 2026-11-17, `c71a4a20`; `~/tonto-runs/2026-11-17_j_embedding_stage3`; test `tests/long/nh3_rhf_DZP_HAR_J-embedding` |
| 4. refinements against neutron | open |
| 5. fragment HAR | coded and checked 2026-11-18, `366926cc`, `3188e064` and the commit after; `~/tonto-runs/2026-11-17_fraghar_embedding`; two new tests wait for their references (achari2) |

Decided on the way (Dylan, 2026-11-12 to 13): whole molecules only, so the atoms and groups
levels below are not implemented; the source molecule is an argument of the integral routine so
fragment HAR can reuse it; no supermolecule HAR for ion pairs or hydrates (8-hydroxyquinolinium
hydrogen maleate, N-acetyl-L-4-hydroxyproline monohydrate wait for embedded fragment HAR).

## Stage 3: a self-consistent embedding

**Outcome (2026-11-17).** Implemented as designed below; the test (1) sits in
`MOLECULE.SCF:make_r_core_mx` (not `make_core_mx`, which calls it), the fresh build in
`initialize_SCF`. Tolerance series, RHF/def2-SVP from the promolecule, 3.5 Å, cluster charges to
8 Å, SCF to 1e-6 (errors in Eh, against the τ = 0 run, which rebuilds at every Fock build):

| τ | urea builds | stored-matrix error | ΔE | L-Ala builds | stored-matrix error | ΔE | L-Ala job time |
|---|---|---|---|---|---|---|---|
| 0 | 13 | 0 | 0 | 15 | 0 | 0 | 388 s |
| 1e-5 | 10 | 2e-6 | −3e-6 | 11 | 7e-6 | −9e-6 | 334 s |
| 1e-4 | 8 | 3.5e-5 | −5.9e-5 | 9 | 8.5e-5 | +1.2e-4 | 310 s |
| 1e-3 | 7 | 3.8e-5 | +1.0e-5 | 8 | 3.2e-4 | +4.5e-4 | 297 s |
| 1e-2 | 5 | 3.3e-4 | +9.7e-4 | 6 | 1.6e-3 | +2.2e-3 | 273 s |

Job times include about two minutes (L-Ala) of `put_embedding_neighbours` grid checks. Urea is not
monotonic between 1e-4 and 1e-3 (sign cancellation); L-alanine, the zwitterion, is monotonic and
about ten times more sensitive. Default chosen: **τ = 1e-5**, the largest value keeping the error
below 1e-5 Eh, the SCF convergence the HAR jobs use. The SCF needed no extra convergence test:
the iteration tables end smoothly. Moved-atom check (check 3 below): a two-cycle NH3 HAR, H moved
0.03 Å, stored and fresh embedding agree to 6e-7 Eh (`nh3_fullprec`). The NH3 test takes 5 s and
its refined positions differ from `nh3_rhf-consistent-cluster-charge_DZP_HAR`.

**What is wrong now (confirmed 2026-11-17).** `make_core_mx` builds `.embedding_mx` only if it
does not exist, and nothing destroys it. A probe line showed it is built at SCF iteration 0, so
from the starting density: the promolecule with `initial_density= promolecule`. It is then kept
for the rest of that SCF and for every later SCF of a HAR, even after the atoms move. On urea
(RHF/def2-SVP, 3.5 Å, cluster charges to 8 Å) the stored matrix gives
$`\mathrm{tr}\,(V P) = -0.65330`$ with the final density, against $`-0.65823`$ for the matrix made from
the final density: 4.9 mEh, 0.75 %. Run in `~/tonto-runs/2026-11-13_j_embedding_schwarz/urea_probe`.
The cluster charges, by contrast, are remade from the current density at every Fock build.

**Aim.** At convergence the neighbours' density must be the converged density of the molecule,
rotated, to a stated tolerance, just as the cluster charges are the converged charges and
dipoles. And each SCF must start again from the atoms where they now are.

**Design: a full rebuild whenever the density has moved by more than a tolerance.** Keep, with
the embedding matrix, the density matrix $`P_{\mathrm{emb}}`$ it was made from. At every Fock
build (which with self-consistent cluster charges already remakes the core matrix), rebuild the
embedding when

```math
\max_{\mu\nu} |P_{\mu\nu} - P_{\mathrm{emb},\mu\nu}| > \tau \qquad (1)
```

and set $`P_{\mathrm{emb}} = P`$. Here $`P`$ is the current density matrix and $`\tau`$ the new
keyword `embedding_update_tolerance=`. The SCF needs no extra convergence test: a rebuild changes
the core matrix, so the next iteration's energy and density change and the SCF goes on; when it
stops, the last density used for the embedding is within $`\tau`$ of the final one.

At the start of every SCF the embedding matrix and $`P_{\mathrm{emb}}`$ are destroyed, so the first
Fock build makes them afresh at the present atom positions. In a HAR this first build uses the
previous cycle's converged density, which is close, so later cycles should need one or two
rebuilds; the first SCF, from the promolecule, perhaps five to eight.

**Rejected: an incremental update** $`V[P] = V[P_{\mathrm{emb}}] + V[P - P_{\mathrm{emb}}]`$ every
iteration, screened by the small change. It is cheaper per iteration, but the same idea for the
molecule's own Fock matrix (`using_delta_build`) was switched off because the increments are noisy
at the $`10^{-6}`$ level and cost iterations (`docs/TASK_SCF_SPEEDUP_DATA.md`).
**Rejected: an outer loop** (converge, rebuild, reconverge). It needs a second convergence loop
round the SCF, and does the same rebuilds as (1) but with each one waiting for a full convergence.

**The default for** $`\tau`$ is to be chosen by measurement, not assumed: run urea and L-alanine at
$`\tau = 10^{-2}, 10^{-3}, 10^{-4}, 10^{-5}`$ and tabulate the number of rebuilds, the time and the
error of the final embedding, $`\mathrm{tr}\,(V_{\mathrm{stored}} P) - \sum_n (E_J + E_N)`$, the
second term from `put_embedding_neighbours`. Choose the largest $`\tau`$ that keeps that error
below the SCF's own energy convergence. My guess is $`10^{-4}`$.

**Code.**

| piece | where |
|---|---|
| `embedding_density :: MAT{REAL}@`, the $`P_{\mathrm{emb}}`$ of (1) | `types.foo`, MOLECULE, next to `embedding_mx` |
| `embedding_update_tolerance :: REAL`, the $`\tau`$ of (1), and its keyword | `types.foo`, SCF_DATA; `scf_data.foo` |
| `n_embedding_builds :: INT`, counted per SCF and printed with the SCF results | `types.foo`, SCF_DATA |
| the test (1) and the rebuild, in place of "build once" | `MOLECULE.SCF:make_core_mx` |
| destroy both at the start of each SCF | `MOLECULE.SCF:scf` (the place to be found exactly) |
| print the error of the final embedding | `MOLECULE.SCF:put_embedding_neighbours` |

**Checks.**

1. The probe comparison above: with the update, the stored matrix must give the same
   $`\mathrm{tr}\,(V P)`$ as one made from the final density, to within the error that $`\tau`$ allows.
2. Consistency: $`\tau = 0`$ (a rebuild at every Fock build) and the chosen default must give the
   same converged energy and density to the SCF convergence.
3. A moved atom: two SCFs in one job with an atom moved between them; the second must rebuild
   from the new positions (its neighbours table must change).
4. A short urea HAR with the embedding, TFVC, B3LYP/def2-SVP: it must converge, and the number of
   rebuilds per cycle and the time per cycle are recorded.
5. The short test suite, which should not change, since the embedding is off by default.

The old checks 1 and 2 below (the explicit cluster, the far limit) remain to do after this.

**A note on the energy.** With neighbours that are copies of the molecule, the SCF energy counts
the whole interaction $`\mathrm{tr}\,(V^{\mathrm{emb}} P)`$, twice the share of one molecule in the
pair interaction of a lattice energy. HAR uses only the density, so this does not matter for
refinement, but the printed energy is not a lattice energy.

## Stage 5: the embedding in fragment HAR

### Outcome (2026-11-18)

**What the code questions turned up.**

- The fragments did not inherit `embedding_radius=`, nor the cluster charges, nor anything else
  but a few convergence options: `set_SCF_guess_defaults_from` gives each `.mol(g)` default SCF
  options unless `use_default_guess_options= FALSE`. So every fragment HAR so far has used
  isolated fragments, whatever the `scfdata=` said about cluster charges.
- Had the cluster charges reached a fragment, they would have been wrong: `make_atomic_moments`
  rebuilds the fragment's crystal from the fragment's own atoms, so its asymmetric unit, and
  its cluster, hold copies of that fragment only.
- Fragment HAR does not repeat the fragment SCFs until they agree. It does one SCF per fragment
  per refinement cycle.
- `update_atom_groups` deallocates and remakes every `.mol(g)` each refinement cycle. The
  fragment densities survive only as the archive files on disk, which is what `restart` reads.

**Step 0, the segfault.** Not a TFVC bug: fragment HAR had no TFVC code at all.
`fragHAR_refinement` called `make_atom_partition_info` on the whole structure, which for a TFVC
model looks for the cell walls on a density the whole structure does not have (the ENSURE
"no density_mx" in a debug build, a segfault in release). And `get_Hirshfeld_atom_FFs_disk` used
Hirshfeld weights whatever the model. Now the whole structure makes only the grid and the ANOs,
and each fragment makes its own walls (`MOLECULE.GRID:make_SA_walls`) and TFVC form factors.
Gly-Ala, two capped fragments, RHF/STO-3G, release: GoF 3.3495 with `oc-tfvc`, 3.3535 with
`oc-hirshfeld`. The spherical TFVC models stop with a message. In a debug build the Gly-Ala
STO-3G job stops at "no minima found between the atoms" for an N–H bond: with STO-3G the density
along that bond has no minimum (`Mayer_Salvador_radius_for`). That is the basis set. The urea
one-fragment exit 1 of 2026-11-13 was "no binary archive" (`ARCHIVE:read_5`), not looked at
further: the NH3 one-fragment jobs below run.

**The design that was built**, which departs from steps 1 to 5 below in one way: the fragment
does nothing itself. The molecule that owns the fragments makes one matrix for fragment $`g`$,
the potential of all its surroundings, and the fragment's SCF adds it to its core matrix
(`.external_potential_mx`). It holds

- near, the copies of every fragment $`h`$, $`g`$ included, with an atom within
  $`r_{\mathrm{emb}}`$ of $`g`$, by density and nuclei (steps 1 to 3 as planned);
- far, with cluster charges on, every other atom within `cluster_radius=`, by the charge and
  dipole its own fragment gave it, on a copy of the crystal that holds all the atoms (step 4).

Step 5 became an outer loop: `fragment_SCF_embedded` repeats the fragment SCFs in passes. With
$`\Delta_h`$ the largest change of a density matrix element of fragment $`h`$ in its last SCF, a
fragment $`g`$ is done again when

```math
\sum_{\text{SCFs of } h \text{ since the surroundings of } g \text{ were made}} \Delta_h > \tau \qquad (2)
```

for any $`h`$, $`g`$ itself included, with $`\tau`$ = `embedding_update_tolerance=`. The sum bounds
how far the density of $`h`$ has moved. The passes stop when no fragment needs doing. The copies
of $`g`$ itself are updated by the same loop, not inside its SCF as in stage 3: that needs the
fragment to know its crystal, and the passes are needed for the other fragments anyway. Stage 3
rejected an outer loop for one molecule; here there is no way round it.

**Checks.**

| check | result |
|---|---|
| (a) NH3 as one fragment against the stage 3 molecule, RHF/DZP, 3.5 Å, charges to 8 Å, all tolerances $`10^{-9}`$ | SCF energy −56.37683994 Eh by both routes, equal to all 8 digits printed |
| (a) the same, refined to convergence at the default tolerances | GoF 0.85145 (fragment) against 0.85142 (molecule); 0.85144503 against 0.85141749 with tolerances $`10^{-8}`$ |
| the same without surroundings | fragment and molecule refinements agree to all 8 digits (GoF 0.87830647) |
| new: NH3 in the subgroup $`P2_1`$, two independent molecules as two fragments, tolerances $`10^{-9}`$ | both fragments −56.37683994 Eh, the one-fragment energy; this tests the cross-fragment code exactly |
| (c) pair sums of $`E_J`$, same job | (1,2) and (2,1) both 116.523313, difference $`10^{-14}`$; the three sums add to the one-fragment 174.784974 |
| (b), (c) NAC monohydrate | within 3.5 Å the NAC has 10 NAC copies and 7 water copies, the water 7 NAC copies and no water; the seven NAC–water pairs appear in both lists with equal $`E_J`$; pair sums 647.175608 Eh both ways, difference $`6\times10^{-8}`$ (the integral cutoff is $`10^{-9}`$ per integral); shortest contact 1.63 Å (NAC–NAC), 1.65 Å (NAC–water) |
| ten ctest jobs matching `J-embedding`, `fragHAR`, `consistent-cluster`, `quartz_NN`, release, Mac | pass |

The two-cycle GoF of the one-fragment NH3 job (0.853) differs from the stage 3 test's (0.851).
Run to convergence they agree, so the difference is in the starting point, not the method. My
reading, not checked: the stage 3 job makes its first form factors after an SCF that starts
from the isolated molecule's density, the fragment job after the passes have converged.

**NAC monohydrate, RHF/def2-SVP, Hirshfeld atoms** (`~/tonto-runs/2026-11-17_fraghar_embedding`,
README there). Fragment SCFs at the X-ray geometry, passes to $`\tau = 10^{-5}`$:

| surroundings | passes | job time | NAC energy / Eh | water energy / Eh |
|---|---|---|---|---|
| none | 1 | 78 s | −624.968067 | −75.961864 |
| cluster charges to 8 Å | 7 | 380 s | −625.323030 | −76.108499 |
| density within 3.5 Å, charges to 8 Å | 7 | 925 s | −625.320493 | −76.071404 |

The energies count the fragment's electrons in the field of the surroundings but not its nuclei
in that field, so they are not interaction energies. Each pass shrinks the density change four
to five times (ten times for NH3).

**Fragment HARs against neutron** (same folder; `compare_neutron.py`, outputs `compare_nac.out`
and `compare_hqm.out`). def2-SVP, Hirshfeld atoms, charges to 8 Å, density to 3.5 Å, all converged.
$`\Delta`$ is the refined bond length less the neutron one, in Å, averaged over the bonds named;
S12 is the Whitten–Spackman measure for the hydrogen ADPs (×100, smaller is better), the neutron
ADPs shifted by one overall tensor. Per wavefunction, not pooled.

NAC monohydrate (4 O–H, 9 C–H bonds; X-ray and neutron at 9 K):

| wavefunction | surroundings | GoF | R(F) | mean Δ(O–H) | mean Δ(C–H) | mean abs Δ(X–H) | S12 H | time / s | passes in each cycle |
|---|---|---|---|---|---|---|---|---|---|
| RHF | none | 1.5703 | 0.0271 | −0.027 | −0.015 | 0.019 | 8.3 | 338 | |
| RHF | charges | 1.5716 | 0.0270 | −0.008 | −0.010 | 0.013 | 11.5 | 1367 | 7 6 4 4 3 2 1 |
| RHF | density + charges | 1.5728 | 0.0270 | −0.015 | −0.010 | 0.015 | 12.4 | 3079 | 7 6 4 3 2 2 |
| B3LYP | none | 1.5257 | 0.0267 | −0.038 | −0.020 | 0.026 | 6.6 | 401 | |
| B3LYP | charges | 1.5155 | 0.0265 | −0.021 | −0.017 | 0.019 | 6.3 | 1126 | 7 5 4 3 2 |
| B3LYP | density + charges | 1.5175 | 0.0265 | −0.029 | −0.017 | 0.021 | 7.0 | 3205 | 7 6 4 3 2 2 |

8-hydroxyquinolinium hydrogen maleate (O–H, N–H and the acid O–H as the three polar bonds, 8 C–H;
X-ray 15 K, neutron 12 K; no isolated-ion runs, as decided on 2026-11-12):

| wavefunction | surroundings | GoF | R(F) | mean Δ(polar X–H) | mean Δ(C–H) | mean abs Δ(X–H) | S12 H | time / s | passes in each cycle |
|---|---|---|---|---|---|---|---|---|---|
| RHF | charges | 1.6694 | 0.0337 | +0.017 | −0.001 | 0.011 | 6.3 | 1113 | 8 6 4 3 2 1 |
| RHF | density + charges | 1.6686 | 0.0337 | +0.009 | −0.001 | 0.008 | 6.0 | 2661 | 8 6 4 3 2 1 |
| B3LYP | charges | 1.5819 | 0.0329 | −0.012 | −0.007 | 0.008 | 6.9 | 1011 | 8 5 4 2 2 |
| B3LYP | density + charges | 1.5817 | 0.0329 | −0.019 | −0.007 | 0.011 | 6.9 | 2588 | 8 6 4 3 2 |

What the numbers say, and no more: with Hirshfeld atoms and def2-SVP, replacing the charges of the
neighbours within 3.5 Å by their density changes the GoF by 0.002 or less in all four pairs, and
moves the polar X–H bonds by about 0.01 Å, towards neutron in one pair (8HQ, RHF) and away in
the other three. Cluster charges against isolated fragments is the larger step (NAC, B3LYP: GoF
1.526 to 1.516, O–H from −0.038 to −0.021 Å). So on these two crystals the Coulomb embedding
shows no gain over cluster charges. Not yet tried here: TFVC atoms, a larger basis, other radii.
The embedded jobs take about 2.7 times as long as the cluster-charge ones.

Gly-Ala with TFVC atoms (step 0): the GoF 3.3495 quoted above is where that refinement stopped at
its cycle limit ("too many iterations"); the Hirshfeld run converged. Why the TFVC run does not
converge with STO-3G is not looked at.

**Still open in stage 5.**

- The references of the two new tests, on achari2: `tests/long/nh3_rhf_DZP_fragHAR_J-embedding`
  (one fragment, two cycles) and `tests/long/nh3_P21_rhf_DZP_fragment_SCF_J-embedding` (two
  fragments, SCF and `put_fragment_neighbours`). Both run in a debug build on the Mac (10 s each).
  Check (d) asked for an embedded Gly-Ala variant; Gly-Ala's fragments are capped residues of one
  molecule, which have no crystal surroundings of their own, so the code stops for capped fragments.
- **A trap for DFT fragments, older than this work.** `kind= fragment-rks` with
  `dft_functional= b3lyp` runs the fragments at **RHF** unless `use_default_guess_options= FALSE`
  is also given: only then do the fragments copy the `scfdata=`. On the two-fragment NH3 job the
  fragment files say `SCF kind rhf` and give the RHF energy (−56.376840) without it, and
  `SCF kind rks`, −56.736389 Eh for both fragments, with it (B3LYP/DZP, with the surroundings,
  6 passes; `nh3_P21_b3lyp` and `nh3_P21_b3lyp_default_options` in the run folder). In `set_SCF_guess_defaults_from` and
  `set_molecule_from_atom_group` the branches that would copy the options test for a kind
  beginning `fraghar`, a name no SCF kind has any more. Proposed fix, for Dylan to decide since it
  changes what existing fragment jobs do: test for `fragment-` there, and give the fragments the
  kind that follows the prefix. Either way the fragments converge to `guess_convergence=`
  ($`10^{-8}`$ in the energy), not to `convergence=`.
- Parallel runs: `fragment_SCF_embedded` stops under MPI.
- Fewer passes: each pass costs one embedding build per fragment, most of the time. An
  extrapolation over the passes (DIIS on the fragment densities) is the obvious next step.
- 8-hydroxyquinolinium hydrogen maleate; then the refinements against neutron, per wavefunction.

### The plan as written on 2026-11-17

Aim: embedded fragment HAR for crystals with several molecules or ions in the asymmetric unit.
First case: NAC monohydrate, two fragments (NAC and water), Lübben 2014, X-ray and neutron at
9 K, `~/Dropbox/tonto_data/xray_neutron_set/NAC_monohydrate_9K_{xray,neutron}_Lubben2014.cif`.
Then 8-hydroxyquinolinium hydrogen maleate (Malaspina, same folder), an ion pair. Rough effort
about six hours of work plus runs, the least certain part being step 0.

**How fragment HAR runs today.** `MOLECULE.SCF:fragment_SCF_norm` (and `_para`) loops over the
groups `.atom_group(g)`. Each fragment is its own MOLECULE `.mol(g)`, with its own `crystal`
(from `update_fragment_info` with that fragment's atoms) and its own `SCF_data`, and gets its own
`usual_SCF`. With `restart` the fragment SCFs of later refinement cycles start from their
previous MOs. So if `embedding_radius=` reached a fragment today, `make_embedding_neighbours`
would apply the symmetry operations to that fragment's own atoms only: each fragment would be
embedded in copies of itself, and the other fragments would stay as cluster charges. Not tested;
first find out whether `.mol(g).SCF_data` inherits the keyword at all.

**Step 0. The fragment HAR + TFVC segfault.** `~/tonto-runs/2026-11-13_fraghar_tfvc_test`:
`fraghar_oc-salvador` (urea as one fragment) and `glyala_fraghar_oc-salvador` exit 139;
`glyala_fraghar_oc-hirshfeld` runs, `fraghar_oc-hirshfeld` exits 1. Debug build, `-fbacktrace`,
start from the Gly-Ala case (it isolates TFVC from the urea one-fragment problem). The urea
exit 1 needs its own look: a one-fragment fragment HAR is check (a) below.

**Step 1. Neighbours across fragments.** For fragment $`g`$, the neighbours are the copies
$`(h, R_n, \mathbf{t}_n)`$ of every fragment $`h`$, its own included, with an atom within
$`r_{\mathrm{emb}}`$ of an atom of $`g`$, except $`g`$ itself. The other fragments of the same
asymmetric unit are neighbours by the identity and zero shift. Generalise
`make_embedding_neighbours` to take the source atoms (fragment $`h`$'s) and return $`h`$ with each
copy; `same_atom_sets` as now, per $`h`$.

**Step 2. The source density.** The neighbour's density is $`T(R_n) P_h T(R_n)^{\mathsf T}`$ with
fragment $`h`$'s density matrix and its own shells: call `rotate_density_mx_r` on `.mol(h)` and
`add_r_embedding_mx(V, .mol(h), P, y)`, which already takes the source molecule. The Schwarz
bounds of the source are made inside the routine, so they carry over.

**Step 3. Integral workspace for mixed quartets.** `ERI_SCRATCH:set_sizes_for` is called once for
each molecule's own shell pairs, which covers src = self but not a quartet whose bra and ket
come from two bases with different largest pairs; size it for the mixed quartets.

**Step 4. Cluster charges.** `remove_embedded_cluster_charges` matches the atoms of copies of the
molecule itself. Generalise it to take the list of embedded positions from step 1. Check first
how a fragment's cluster charges are made: `CLUSTER:make_non_fragment_mu_charges` takes each
atom's charge and dipole from its asymmetric-unit atom, so which fragment's Hirshfeld moments
reach the other fragments' atoms is to be confirmed in the code.

**Step 5. Updating.** Split the embedding of fragment $`g`$ into the part from copies of itself,
updated during its own SCF as in stage 3, and the part from the other fragments, whose densities
change only between fragment SCFs. Keep one $`P_{\mathrm{emb}}`$ per source fragment; at the start
of $`g`$'s SCF rebuild the other-fragment part when any source density has moved by more than
$`\tau`$. Find out whether fragment HAR already repeats the fragment SCFs until they agree with
each other (through the cluster charges); if not, the refinement cycles do it.

**Checks.**
- (a) Urea, or L-alanine, as a one-fragment fragment HAR with the embedding must reproduce the
  stage 3 HAR to the SCF convergence.
- (b) NAC monohydrate: `put_embedding_neighbours` (generalised) must list the water copies among
  the NAC's neighbours and the NAC copies among the water's, with the density, nuclear and grid
  checks as now.
- (c) Symmetry of the pair interaction: with converged densities, the interaction of $`g`$ with a
  copy of $`h`$ must equal that of $`h`$ with the inverse copy of $`g`$.
- (d) A test job: `tests/long/gly_ala_fragHAR_rhf_STO-3G` exists; add an embedded variant, two
  refinement cycles, as for `nh3_rhf_DZP_HAR_J-embedding`.

Then the refinements of NAC monohydrate against its neutron structure, per wavefunction.

## Goal

Give the molecule in a HAR the Coulomb field of its nearest neighbours as they actually are, a
smeared density plus nuclei, and not as point charges. The neighbours are symmetry copies of
the molecule itself, so their density comes from its own density matrix, and it updates as the
SCF proceeds. The far field stays with the cluster charges. There is no exchange and no Pauli
repulsion in this task; they are the subject of the last section.

## Why

- Brux et al. (2025), Struct. Dyn. 12, 054101, improve HAR of amino acids with ORCA's CPCM
  continuum (water). A continuum has no direction and its cavity radii are fitted to hydration
  free energies, not to the polarisation of the density; Dylan judges it too rough.
- Cluster charges (`use_SC_cluster_charges`, 8 Å) lower the GoF of urea by about 10 % and match
  the explicit 7-molecule cluster at about 1/40 of its cost (`~/tonto-runs/2026-10-22_urea_cluster`,
  `2026-10-23_soft_walls`). What they miss is short range: at a hydrogen bond, 1.8 to 2.0 Å,
  the field of a smeared density with its nucleus differs from that of a point charge and
  dipole, because the two densities overlap (penetration).
- The explicit cluster gets this right but runs the SCF of every molecule. The embedding needs
  the SCF of the central molecule only.

## The method

In [`REPORT_ON_CRYSTAL_EMBEDDING.md`](REPORT_ON_CRYSTAL_EMBEDDING.md).

## Where it goes in the code

In [`REPORT_ON_CRYSTAL_EMBEDDING.md`](REPORT_ON_CRYSTAL_EMBEDDING.md).

## Checks

1. **Exactness.** Make the explicit urea cluster's density matrix from rotated and translated
   copies of the central $`P`$, and its Coulomb matrix with the ordinary builder on the cluster.
   Its central block must equal $`J[P] + V^{\mathrm{emb}}`$ (whole molecules) to integral
   accuracy. This tests the mapping, the rotation and the integrals at once.
2. **Far limit.** For a fragment moved far away, $`V^{\mathrm{emb}}`$ must approach the field of
   its cluster charges.
3. **Rotation.** A space group with rotations (L-alanine, $`P2_12_12_1`$), not only translations.

Done instead of check 1 so far: the rotated density against the molecule's at carried-back
points, the nuclear term against the molecule's potential at the neighbour's nuclei (exact), and
the electronic term against the same interaction on the DFT grid ($`2\times10^{-6}`$), on urea and
L-alanine; check 3 is covered by L-alanine. Checks 1 and 2 are still to do.

## Tests against experiment

Urea and L-alanine, cluster charges as in the benchmark, B3LYP and r2SCAN, def2-SVP and def2-TZVP,
TFVC and TPFVC at the benchmark's wall sharpness, five ways:

| surroundings | |
|---|---|
| isolated | from the 2026-10-23 batches |
| cluster charges | from the 2026-10-23 batches |
| cluster charges + partner atoms | new |
| cluster charges + partner groups | new |
| cluster charges + whole neighbour molecules | new |
| explicit cluster | `2026-10-22_urea_cluster`, L-alanine 9 molecules |

Compare GoF, bonds to hydrogen and hydrogen S12 against neutron, per wavefunction (never pooled),
and against the explicit cluster. Then xylitol and Gly–Ala if the gain is clear.

## Stages and effort

1. Fragment selection, shell copies and the rotated blocks; check 1 by hand on urea: 1 day.
2. `make_r_J_embedding`, the nuclei, removal from the cluster charges, keywords: 1 day.
3. The update in the self-consistent loop; checks 2 and 3; a short test: 1 day.
4. The refinements above, on achari2 or the Mac overnight.

## Later: exchange and orthogonality

In [`REPORT_ON_CRYSTAL_EMBEDDING.md`](REPORT_ON_CRYSTAL_EMBEDDING.md).

