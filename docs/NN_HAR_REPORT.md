# Nearest-neighbour HAR on a covalent network solid

Reactivation of `archive/nn-har`, and the first independent reproduction of
chapter 5 of Max Davidson's thesis. Work done 2026-08-15; commit `16a91ce1`.

Everything below was measured on this machine, not inferred from the code.
Where a claim is an inference it says so.

## 1. What the capability is

Hirshfeld atom refinement needs an aspherical form factor for every atom, and
that means a molecule to compute it from. In a covalent network solid there is
no molecule: quartz is one infinite bonded lattice. `fragHAR` handles crystals
with several molecules in the asymmetric unit, but it cannot handle a structure
with no molecular boundary at all.

Nearest-neighbour fragmentation supplies the missing boundary. Around each
asymmetric unit atom it follows bonds out `NN_level` shells, then replaces the
shell beyond with capping hydrogens. The form factor is taken for the central
atom only; everything else in the fragment is there to provide the chemical
environment.

Three new keywords on `diffraction_data`:

| keyword | default | meaning |
|---|---|---|
| `use_NN=` | `FALSE` | switch the scheme on |
| `NN_level=` | 1 | how many shells before capping. Implies `use_NN= TRUE` |
| `use_NN_capping=` | `TRUE` | `FALSE` leaves the outer shell bare |

The work is `MOLECULE.BASE:set_NN_capped_groups`, a third branch of
`update_atom_groups` alongside Ryde capping and explicit atom indices.

## 2. Provenance, and why this was not a straight replay

`archive/nn-har` is four commits, Feb 2023, by Max Davidson. Its dates matter:

```
2020-01-23  f0d7cfd3   fragHAR broken           <- ancestor of the branch
2023-02-01  89fa30e7   branch point
2023-02-03  452787fd   Implemented NN-HAR, currently not working
2023-02-03  5831e28e   Added NN Working, but underwhelming results
2023-02-08  5b37f915   automated level switch, H bond normalisation
2023-02-23  fd956388   Corrected fragHAR to work with DFT
2026-06-01  d840e322   fragHAR fixed            <- ABSENT from the branch
```

Every NN commit sits inside the six-year window when fragHAR was broken, and
the fix lands three years after the branch ends. Max was patching fragHAR
himself as he went. **NN-HAR and a working fragHAR have never coexisted until
now.** That was the main reason to doubt the thesis numbers, and §5 reports
what the doubt was worth.

Consequently the port follows `develop`'s post-fix idiom rather than the
branch's: names and basis set assigned *before*
`update_atom_and_basis_info_no_grp`, and `set_minimal_copy` in place of the
deleted `create_copy` / `make_fragment_data`. Dropped from the branch: a debug
probe in `molecule.tad.foo`; the widening of `ATOM:covalent_radii_ccdc` to
`public`, unnecessary because caps are relabelled H before placement so
`r_cov(A) + r_cov(H)` falls out of the existing elemental accessor; and
`mol.atom.nullify_bases`, which no longer exists and was redundant.

## 3. Defects fixed on the way

Three in `cluster.foo`, none introduced by this feature, all live before it:

- **`make_asym_occupation_list` wrote out of bounds.** It created a one-element
  occupation list and then indexed it by the *unit cell* atom `u`. Fires
  whenever `u /= 1`; quartz has 9 unit cell atoms.
- **That routine's attribute was misspelt `PURTE`.** It parsed, was silently
  ignored, and the routine had never been pure.
- **`do_NN_defragment` stopped a shell early**, leaving its growth loop through
  `nc_atoms = nc_atoms + 1; exit`, and never set the cluster bookkeeping
  (`n_atoms`, `molecule_for_atom`, `is_fragment_atom`, `info_made`) that
  `create_atom_list` requires.

One introduced by the port and fixed before commit, recorded because the shape
recurs: `set_NN_capped_groups` first grew fragments in the molecule's
`.cluster` member. Growing a fragment mutates the cluster, so on exit it held
the *last* fragment's atoms against a crystal describing a different fragment,
and `CLUSTER:put` indexed `.crystal.fragment_atom` with cluster atom indices --
a SEGFAULT 600 lines of output later, in a routine with nothing to do with
fragmentation. The cluster is now local to the routine. **Nothing was wrong at
the point of the mistake.**

## 4. Tests, in two tiers

Fragment construction and refinement fail independently, so they are tested
independently. This is why the SEGFAULT above was diagnosable: the short tests
stayed green through it, correctly, because fragmentation was never the broken
part.

### Short -- fragment construction only (4 tests, 0.41 s total)

No SCF, no reflections, no refinement. `tests/short/quartz_NN_fragments_*`.

The assertion is partly in the **file name**: `put_atom_group_mols` writes each
fragment as `group-<n>-<formula>`, so a wrong fragmentation leaves the declared
output missing and the test fails loudly rather than comparing wrong numbers.
Verified to fail: perturbing `NN_level` 1->2 against level 1 references trips
both the missing file and the stdout atom count (21 against 9).

Quartz makes this sharp because the expected answers are published. Davidson
Fig. 5.2 names the level 1 fragments outright:

| | Si-centred | O-centred |
|---|---|---|
| L0+H | `H4Si` silane | `H2O` water |
| **L1+H** | `H4O4Si` **orthosilicic acid** | `H6OSi2` **silyloxysilane** |
| L2+H | `H12O4Si5` | `H6O7Si2` |
| L1 uncapped | `O4Si` | `OSi2` |
| L2 uncapped | `O4Si5` | `O7Si2` |

All five reproduce. Geometry checks out too: Si-O at 1.6093 and 1.6137 A --
alpha-quartz's two distinct bonds -- with O-H caps at 0.9100 A = 0.68 + 0.23
and Si-H caps at 1.4300 A = 1.20 + 0.23, i.e. on the internuclear vector at the
summed CCDC covalent radii.

Formulae are alphabetical, not Hill notation: `VEC{ATOM}:chemical_formula`
sorts symbols and stops. Quartz has no carbon so the conventions agree here,
but they diverge whenever carbon is present with an element sorting between C
and H -- Hill CH3Cl against Tonto's CClH3.

### Long -- the refinement (2 tests, 6.8 s and 19.9 s)

`tests/long/quartz_NN_HAR_L{0,1}_rhf_def2-SVP`. RHF/def2-SVP, matching the
thesis basis. L1 costs 3x L0 because fragments grow from 5/3 atoms to 9/9 and
the SCF runs per fragment per cycle. Anyone adding **L2** should expect minutes,
not seconds -- the Si fragment reaches 21 atoms.

## 5. The thesis reproduces

This was in genuine doubt, per §2. It should not have been.

| | L0+H ours | L0+H thesis | L1+H ours | L1+H thesis |
|---|---|---|---|---|
| R(F) / R1 | 0.0213 | 0.0306 | **0.0120** | 0.0127 |
| GoF^2 | 77.37 | 103.02 | **9.84** | 10.61 |
| r(Si-O) /A | 1.61(4) | 1.611(15) | **1.609(8)** | 1.610(6) |
| U_iso(Si) /A^2 | 0.003(4) | 0.0028(14) | **0.0029(7)** | 0.0029(5) |
| U_iso(O) /A^2 | 0.00564(10) | 0.00575(11) | **0.00566(4)** | 0.00567(4) |

At L1+H the structural parameters agree to the digits printed. R and GoF^2 sit
5-7% apart, which is what a different data reduction should give -- 1009
reflections here against the thesis's 1134. The L0->L1 collapse reproduces too:
77->10 here, 103->11 there.

**Davidson's uncomfortable finding reproduces as well.** The thesis IAM gives
GoF^2 7.235; this L1+H HAR gives 9.84. HAR is still *worse* than the
independent atom model on quartz. Whatever the 2020-2026 fragHAR breakage
affected, it was not this chapter.

L0's R(F) is the weakest agreement anywhere in either job -- 30% apart against
5-7% at L1 -- and is **not** accounted for by the reduction difference. L0 is
the pathological model so it may be nothing. Unexplained, and left unexplained
rather than blessed away.

### Data

`quartz_init.cif` + `HKLdata.quartz`, from `Quartz1_CD.cif` (Olex2/SHELXL,
16 Jan 2024; Ag Ka, 100 K; measured at Bern by Yaser Balmohammadi, provided by
Simon Grabowsky) with its 162822 embedded reflections stripped -- the original
is 169885 lines.

**It is not the thesis reduction.** Thesis: N_meas 168430, N_uniq 1134,
(sin th/lambda)max 1.46 A^-1. This: N_meas 162822, N_uniq 1748, theta_max
51.958 deg giving 1.404 A^-1. Same crystal, different reduction. Dylan is
chasing the thesis reduction from Max or Simon; **if it arrives these
references should be re-blessed against it.** Worth asking at the same time
which Tonto Max actually ran: the thesis says version 24.04.06, but the branch
ends 2023-02-23.

The thesis PDF contains no data: `pdfdetach` reports 0 embedded files, and
appendices A/B/C support chapters 2-4 only. Chapter 5 has no supporting
information at all.

## 6. Bond lengths, and why no improvement can be demonstrated

| | r(Si-O) /A | su /A |
|---|---|---|
| **Experiment** (SHELX IAM, *same* data) | **1.60925(15)**, **1.61365(13)** | 0.00015 |
| Thesis IAM (Tonto) | 1.6(3) | 0.3 |
| Thesis L1+H | 1.610(6) | 0.006 |
| **Ours L1+H** | **1.609(8)** | 0.008 |

The **values are excellent** -- 1.609 against an experimental 1.60925, agreeing
to 0.0003 A. Every model lands on 1.609-1.611.

**The uncertainties are not.** Tonto's HAR su is 0.008 A against SHELX's
0.00015 A on identical data: fifty times wider. The thesis reads "every model
is within error bars of every other model" as the models being alike. At that
width every model is inside every other model's error bar *by construction*.
The comparison has no power to resolve anything, and **the question "does HAR
improve bond lengths" is currently unanswerable in Tonto for a structure with
special positions** -- not because HAR fails to improve them, but because the
uncertainty hides any improvement.

Tonto also reports **one** Si-O bond where quartz has **two**. SHELX lists
1.60925(15) and 1.61365(13) as distinct; the archive CIF prints a single
`Si1 . O1 . 1.609(8)`. The fragment geometry carries both -- the short tests
show 1.6093 and 1.6137 -- so the information exists and is not reaching the
CIF. The 0.0044 A splitting is 30 sigma on SHELX's sus and invisible on
Tonto's.

### The ADPs are the point, and they are where the damage is

It would be easy to conclude from the above that quartz is a weak test bed
because HAR's famous benefit is on X-H bonds and quartz has no hydrogens except
artificial, unrefined caps. **That conclusion is wrong.** HAR does not only
place hydrogens: it returns *atomic displacement parameters in quantitative
agreement with neutron diffraction*, and those ADPs carry real physical
information -- vibrational, hence phonon information, and information about
disorder. They are a scientific deliverable in their own right, not a nuisance
parameter, and quartz has perfectly good ADPs for both its atoms. So quartz
**is** a legitimate test bed. It is testing the ADPs.

Which makes §7 worse, not better, because the su inflation lands squarely on
that deliverable:

| | U_iso /A^2 | su | relative |
|---|---|---|---|
| Si, ours L1+H HAR | 0.0029(7) | 0.0007 | **24%** |
| Si, SHELX IAM, same data | 0.00314(1) | 0.00001 | 0.3% |
| O, ours L1+H HAR | 0.00566(4) | 0.00004 | 0.7% |
| O, SHELX IAM, same data | 0.00572(2) | 0.00002 | 0.3% |

The general-position oxygen is within a factor of two of SHELX and perfectly
usable. The special-position silicon carries a **24% uncertainty on its U_iso**,
seventy times SHELX's. No comparison against a neutron ADP is possible at that
width, and neither is any phonon or disorder inference. The central value,
0.0029 against 0.00314, is fine. The uncertainty is what destroys the
measurement -- and it is not a HAR problem, it is the special-position
parameterisation of §7.

## 7. Where the inflated sus come from -- measured, and not what was assumed

The first explanation offered for §6 was a rank-deficient pseudo-inverse:
Tonto refines all coordinates and projects onto the invariant subspace
afterwards, patching the parameter count by counting near-zero eigenvalues.
**That explanation is wrong, and measurement is what killed it.** Running L1+H
with `show_near_0_eigenvectors= TRUE` gives, in every cycle:

```
near_0_tol ................ 0.1000E-02
near_0 .................... 0
```

The filter never fires. All 18 eigenvalues are non-zero:

```
smallest   0.4197E+05   0.5144E+05   0.4798E+06   0.1068E+07
largest    0.2805E+10   0.3268E+10
```

Full rank, no filtering, no pseudo-inverse. But a **condition number of about
78000**, and sqrt(78000) ~ 280, so sus may legitimately differ by up to ~280x
between stiff and soft eigendirections. The measured Si-vs-O su ratio sits
inside that:

```
Si1   -1.304(9)     2.259(5)     3.600(9)      su ~ 0.009 A
O1    -0.77606(14)  3.64199(14)  2.96977(13)   su ~ 0.00014 A
```

A **64x difference between two atoms in one refinement** -- and this comparison
is internal to a single Tonto run, so it carries no cross-program confound.

The silicon is the one on a 2-fold axis (`_atom_site_site_symmetry_order` 2,
refinement flag `S` in the source CIF). The general-position oxygen is fine;
its su is comparable to SHELX's. So the *cause* is special positions, but the
*mechanism* is an ill-conditioned full-rank system, not a singular one: the
symmetry-constrained directions are ones the data cannot see, and refining them
anyway leaves them soft rather than exactly flat.

### What follows for a fix

- **Symmetry-unique parameter refinement is the correct fix, and the only clean
  one.** Refining only the independent parameters removes the soft directions
  from the parameter space rather than suppressing them numerically, leaving a
  well-conditioned system whose inverse gives correct sus directly.
  cctbx-style reparameterisation and explicit constraint projection
  (`B^T A B`, solve reduced, propagate covariance back through `B`) are
  equivalent; the choice is implementation, not principle.
- **Eigenvalue filtering is not the culprit and removing it would change
  nothing.** The soft eigenvalues are ~4e4 against a tolerance of 1e-3 --
  eight orders of magnitude from being caught. Keep the filter as a safety net
  for genuine degeneracy. What should change is that it stops being needed: with
  explicit constraints a filter hit becomes a diagnostic rather than routine.
- **`N_p` is overcounted.** 19 = 18 structural + 1 scale, i.e. 9 per atom for
  both. Silicon on a 2-fold axis should contribute about 5 -- one free
  positional parameter and four independent ADP components. Since
  `GoF^2 = chi^2/(N_r - N_p)` with `N_r = 1009`, that is a 0.4% effect here:
  negligible for quartz, wrong in principle, and material for a structure with
  more atoms on special positions.

## 8. Other defects found, not fixed here

- **`refine_F= FALSE` computed no shifts, silently.** The F2 branch of
  `CRYSTAL:get_parameter_shifts` was commented out and `get_parameter_shifts_I`
  does not exist anywhere in the tree, so both overloads fell through their
  `if`. A zero shift reads as convergence, so the refinement reported success
  having refined nothing. Now a `DIE`. Implementing the F2 path properly is a
  separate job; the machinery beneath it -- `d_I_pred_dX`,
  `optimize_I_scale_factor`, `I_r_factor`, and a commented
  `solve_I_normal_equations` -- does already exist. Found by a peer session and
  verified here.
- **`U_iso` derivatives make the normal matrix singular by construction.**
  `molecule.har.foo:1300` writes three identical columns
  (`sf_d(k,4) = sf_d(k,5) = sf_d(k,6) = -sf2`), giving rank deficiency 2 in
  that block, which the pseudo-inverse then absorbs. **This did not affect the
  numbers above** -- quartz refined anisotropically and never entered that
  branch -- but it would bite any isotropic refinement. Found by a peer session
  and verified here.
- **Output defects left visible in the blessed references, deliberately**:
  `Rw(F2)` prints `NaN`, `R_sigma(F2)` prints `0.0000`, and every reflection is
  reported as an unmatched Friedel pair. The misspelling "Fridel" is in the
  source.

## 9. Open

- Re-bless against the thesis's own 1134-reflection reduction if it arrives.
- Explain L0's R(F) discrepancy (§5).
- Symmetry-unique parameter refinement (§7) -- the one that would make both the
  bond-length and the ADP questions answerable. The ADP one is the more
  valuable: a 24% su on U_iso(Si) puts any comparison with neutron, and any
  phonon or disorder inference, out of reach.
- `L1+H` at def2-TZVP, where the thesis reports R1 0.0125 / GoF^2 10.086, and
  BLYP/def2-TZVP where it reports its best result (0.0090 / 4.892). Note the
  thesis DFT numbers were taken with a Becke-grid workaround for a bug since
  fixed (milestone 10), so the HF rows are the sounder reference.
