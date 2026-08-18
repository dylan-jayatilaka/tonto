# Repository branches: what is live, what was archived, and how to recover it

On 2026-08-11 the branch list was reduced from twenty-one branches to five;
sixteen were removed. No commit was deleted. Fifteen of the sixteen are preserved
as annotated `archive/*` tags on `origin`, and any of them can be restored to a
working branch with one command. The sixteenth needed no tag, because its tip was
already an ancestor of `master`.

This page records what each branch held, who wrote it, why it was archived, and
why the archived work cannot be merged into `master` — only ported.

## Branches that remain live

| Branch | Role |
|---|---|
| **`master`** | The stable branch. What the CI badges track and what a user clones. |
| **`develop`** | The integration branch. Work lands here and is merged to `master` when green. |
| **`Nice-branch`** | Not our work. See below. |
| **`Lolo_CP2K`** | Active work by Lorraine A. — CP2K periodic density, selectable periodic stockholder model, SHELX extinction. Last commit 2026-08-03. Left as a branch deliberately. |
| **`gh-pages`** | The Jekyll site, `CNAME` → `dylan-jayatilaka.github.io`. Untouched. |

## Work that is not ours: `Nice-branch`

**`Nice-branch` is the work of Patrick Cassam-Chenaï's group in Nice, and is not
touched by any cleanup of this repository.** It covers electronic and vibrational
quantum chemistry method development, a research programme entirely separate from
the quantum crystallography work on every other branch.

It is by far the largest branch — 252 commits from cassam (156), Thovinarn (49),
davideaccomasso (44) and Patrick Cassam-Chenaï (3), spanning 2017 to 2025-07,
adding roughly 35,000 lines of library code and its own test suite under
`tests/geminal/`. The code is disjoint from `master`: `pauli_block_geminals.foo`
(33,953 lines), `generalized_seniority_conf.foo`, `molecule.gem.foo`,
`geminal_mf_scheme.foo` and `geminal_mf_spectrum.foo` exist on that branch and
nowhere else.

**Nothing on `Nice-branch` was archived, tagged, pruned or altered.** It remains a
branch, at the same commit it has always been at. Collaborators pushing to it or
pulling from it will see no change whatsoever.

## Archived branches and their recovery tags

Each row is recoverable with the command in the next section. Every tag carries
an annotation describing the branch in more detail than this table.

### Scientific work, unmerged

| Tag | Author(s) | Dates | What it holds |
|---|---|---|---|
| `archive/Bader` | Dylan Jayatilaka (19), Max Davidson (9) | 2018-12 – 2019-02 | Bader basin analysis and isosurface triangulation: `cubify_Bader`, `get_Bader_basins_para`/`_sing`, `interpolate_Bader_edge_info`, `interpolate_Bader_faces`, `prepare_Bader_grid`, `put_Bader_basin_info`, plus marching-cube changes and a `PARALLEL` gather. `master` carries only `get_Bader_regions`, so this is genuinely unmerged. |
| `archive/release-td-old` | Dylan Jayatilaka (13), Kanghyun Chu (4) | 2025-08 – 2025-09 | Time-dependent and CIS work. `td_data.foo` reworked, M=0 singlet detector, `S_list` array, MS=0 option in CIS, MGS/Householder orthonormalisation in Davidson, and a major `symmetric_reflect` bug fix. **All seven TD/CIS commits are already on `develop`, which has since gone further; the `symmetric_reflect` fix included. Nothing to port back** — assessed 2026-08-17, see the porting note below. What is still stranded is the earlier half: two breakdown commits and `e22e2569`, an additive extension to Kang's form-factor symmetrization. |
| `archive/bond-energy` | Dylan Jayatilaka (11) | 2020-09 – 2020-10 | Roby bond-energy analysis. `roby.foo` +1478 lines: `Eshared` partitioning for E^DE, exact energy-density method, deformation energies, group populations. |
| `archive/release-pHAR-broken` | Kanghyun Chu (8) | 2025-04 | **TEST RESCUED 2026-08-16.** Held the only ammonia-borane pHAR test in existence. It is now on `develop` as `tests/long/ammonium_borane_pHAR_C23`, and it PASSES — reproducing the 2025-04 reference digit for digit. The 167 MB CRYSTAL23 wavefunction stays on this tag and is fetched on demand; the test skips without it. See the porting note below. Still unported from these 8 commits: the form-factor symmetrisation residual tables. |
| `archive/nn-har` | Max Davidson (3), Dylan Jayatilaka (1) | 2023-02 | **NEAREST-NEIGHBOUR** Hirshfeld atom refinement -- cluster selection by connectivity for PERIODIC NETWORK SOLIDS, with hydrogen capping, automated level switching and H-bond length normalisation. (An earlier version of this table said "neural-network", which is wrong and led to the branch being dismissed once; see the porting note below.) |
| `archive/libxc` | Peter Spackman (4) | 2017-08 | Optional libxc dependency: `cmake/FindLibxc.cmake`, `dft_functional.foo` wiring, B3LYP via libxc. |
| `archive/Teaching` | Max Davidson (5), Dylan Jayatilaka (3) | 2019-06 – 2019-10 | MP2 teaching lab: `run_mp2.foo`, `run_exercise.foo`, a lab-specific `CMakeLists.txt`, and two student PDFs. |
| `archive/lamaGOET` | Lorraine A. (2) | 2021-08 – 2022-03 | `put_unit_cell_geometry_cartesian` and an xyz writer with fractional coordinates. 129 added lines. |
| `archive/energies-breakdown2` | Sam Thompson, Dylan Jayatilaka (2) | 2024-02 – 2024-03 | Energy breakdown, `breakdown_data.foo`. Incomplete — the final commit message records it failing in the polarisation term. |
| `archive/lorraine` | Lorraine A. (1) | 2018-08 | `molecule.prop.foo` changes. Not an ancestor of `lamaGOET` or `Lolo_CP2K`; separate, earlier work. |
| `archive/kanghyun` | Kanghyun Chu (1) | 2026-03 | Keyword-echo tidy: removes empty lines from the echoing section. 8 changed lines. |

### Redundant branches, archived for safety

These four held nothing that is not already on `master` or on another branch.
They were tagged anyway, so that nothing anywhere became unreachable.

| Tag | Why it was redundant |
|---|---|
| `archive/wip-sauce` | A machine-to-machine bridge snapshot. Its one change — release packaging `bin/` → `build/` — is already on `master` at `release.yml:65-67` and `:131-132`. The rest of its diff against `master` is reversion: old `antlr4`/`release` branch lists and the pre-move `docker/` path. |
| `archive/cubes_to_basin` | Max Davidson's 2018-06-28 prototype, 50 added lines. `master` already carries both `cubes_to_basin` and `cubes_to_basin_parallel`, and `Bader` continued this line from 2018-08-16 with a linked-list and parallel implementation. |
| `archive/energies-breakdown` | **The name is misleading: the branch contains no energy-breakdown code.** It is 2017 `master` plus a ten-line README addition. The real work is on `energies-breakdown2`. |
| `archive/dylan-jayatilaka-patch-1` | One installation-command line, on the pre-rewrite README — the one with the Travis badge and the Erice 2025 workshop block. `master` has since rewritten that file wholesale. |

`plots-and-system-command` was deleted without a tag, because its tip commit
`a268b8bc` is an ancestor of `master`. Its content is already in the main line.

### The pHAR test: an open dependency, now resolved (2026-08-16)

`archive/release-pHAR-broken` is the authoritative copy of the ammonia-borane
pHAR test: `release-td-old` shares four of its commits but lacks
`Crystal23_InputFiles.zip`.

It was recorded here as blocked, because `GenerateXML.XML` is a 134-byte Git LFS
pointer rather than the 167 MB object — `.gitattributes` was lost before the
branch tip. **That block is lifted.** The object is still on GitHub and
verifiable, the test is on `develop` as
`tests/long/ammonium_borane_pHAR_C23`, and it passes, reproducing the 2025-04
reference digit for digit. Full account in the porting notes above.

**The tag remains the home of the asset, deliberately.** `develop` and `master`
carry no LFS objects and must not start: committing the pointer with a
`.gitattributes` that tracks it would make every clone pull 167 MB.
`scripts/fetch_phar_asset.sh` pulls it from this tag on request, and the test
skips without it. So the dependency is not removed — it is made **opt-in**,
which is the only arrangement that keeps a public clone cheap.

## State of the recovery effort, and what is left (2026-08-18)

Nine of the fifteen tags are closed. Of the six that held substantial unmerged work,
three are now resolved:

| tag | outcome |
|---|---|
| `archive/nn-har` | **ported** 2026-08-16 (`16a91ce1`) — six tests, thesis reproduced |
| `archive/release-pHAR-broken` | **test rescued** 2026-08-16 (`8ea8c988`); its symmetrization extension ported 2026-08-18 |
| `archive/release-td-old` | **nothing to port** — the TD/CIS half is already on `develop`, which is ahead |
| `archive/Bader` | **serial half ported** 2026-08-18 — compiles and runs; two measured defects; parallel half deferred. See `docs/BADER_REPORT.md` |

Two remain, in the order worth taking them:

| tag | size | note |
|---|---|---|
| `archive/bond-energy` | 11 commits, +1756/−217 | Roby bond energies, `roby.foo` +1478 lines. Self-contained. |
| `archive/Teaching` | 8 commits, +152/−87 | MP2 teaching lab and two student PDFs. Small, no library risk. |

`archive/energies-breakdown2` is left alone: its own final commit records it failing in the
polarisation term.

**Two things to do before reading any of these**, both learned the hard way here:

1. **`git cherry develop archive/<tag>`** first. `release-td-old` was recommended as the
   next rescue on the strength of a commit titled *"Major bug fix in `symmetric_reflect`!"*
   which was already on `develop` under a different hash. One command settles it.
2. **Check what the branch's code depends on before porting it.** Every one of these
   predates the `:::` → `::` migration (tag `foo-old-syntax` is the bridge) and the API has
   moved underneath them; see the two sections at the end of this document.

## Recovering an archived branch

One command, from any clone that has fetched tags:

```bash
git fetch --tags
git branch Bader archive/Bader      # or: git checkout -b Bader archive/Bader
```

The restored branch is byte-identical to what was deleted. To read a single file
without restoring anything:

```bash
git show archive/Bader:foofiles/isosurface.foo
git log --oneline master..archive/Bader
```

To see why a branch was archived, read its tag annotation:

```bash
git tag -n99 -l 'archive/*'         # all of them
git show archive/Bader              # one, with its commit
```

## What a collaborator with an older clone will see

Nothing is removed from anyone's clone. Local branches and local commits are
untouched, on every machine.

| The collaborator runs | What happens |
|---|---|
| Nothing | Their local branch and its commits are unaffected. |
| `git fetch` | The `archive/*` tags arrive automatically. `origin/<branch>` **still appears** in `git branch -r` — git does not prune stale remote refs by default. |
| `git fetch --prune` | `origin/<branch>` disappears. The tags remain. |
| A fresh `git clone` | No archived branches; all `archive/*` tags present, with full history. |

Two consequences worth knowing.

**The change is quiet.** Because `git fetch` does not prune, someone who fetches
will still see the old branch listed and may conclude nothing has changed. They
have to run `git fetch --prune` to see the new state.

**Pushing a local copy resurrects the branch.** A collaborator holding a local
`Bader` who runs `git push origin Bader` recreates it on the remote as a new
branch. Nothing is lost when this happens, but the branch list refills. If the
work is finished, delete the local copy; if it is not, say so and it can stay a
branch, as `Lolo_CP2K` has.

## Porting notes: what was assessed, and what was found

The four smallest branches were examined in detail on 2026-08-11, and `libxc` on
2026-08-12. Most of what looked like low-hanging fruit turned out to be already
fixed, obsolete, or worse than what `master` now has. Recorded so the assessment
is not repeated.

| Branch | Finding |
|---|---|
| `archive/lamaGOET` | **`put_unit_cell_geometry_cartesian` was ported** — see below. Its second routine, `write_xyz_file_xtal14`, was **not**: it is a degraded fork of `put_xyz_file`, which has since moved to `molecule.put.foo` and improved. The branch version writes `.crystal.asymmetric_unit_geometry` — **fractional** coordinates, verified at `crystal.foo:3771` where they are converted with `matmul(.unit_cell.direct_mx,…)` — with no unit conversion, while its own comment claims cartesian axes. It also omits the xyz comment line, making the file malformed, and uses `TEXTFILE*` and `stdin.buffer_exhausted`, both gone from `master` (the latter commented out at `textfile.foo:2001`). If XTAL14 output is wanted, add an option to `put_xyz_file`. |
| `archive/kanghyun` | Nothing to port. The `oisn't` → `isn't` typo was **already fixed on `master`** independently. Commenting out `stdout.flush` in `object.foo` was a workaround for stray blank lines in the keyword echo; the real cause — `TEXTFILE:flush` emitting the margin twice — was root-caused and fixed on 2026-08-03 (see `DEFERRED.md`), so the workaround is obsolete and treats the symptom. Only the two-line CIF/job-name echo is live, and it was judged not worth the output change. |
| `archive/lorraine` | **Skip.** It modifies `cubes_to_basin` and its driver rather than adding anything, and `master` has independently evolved both `cubes_to_basin` and `cubes_to_basin_parallel` since. A merge into live code, not a graft. |
| `archive/release-td-old` | **The TD/CIS half is already merged; `develop` is ahead of it.** Assessed 2026-08-17. `git cherry develop archive/release-td-old` marks all seven TD/CIS commits as upstream, including `ac8b2af4` *"Major bug fix in `symmetric_reflect`!"*, which is on `develop` as `e28bd649` — the same patch, the same minute, differing only in blob hashes and line offsets. `develop` then continued past the branch: the Mazur one-double correction, the 2D `Vs` array in CIS, `read_molden_NOs` reinstated, and the TDHF and cyclazine test fixes, through `b3b50dd2` (2026-07-16), all after the branch's last TD commit (2025-09-04). So the branch holds the **older** copy of this code and nothing in it should be ported back. What remains stranded is the other half — the two breakdown commits and `e22e2569`, discussed below. Details of the `symmetric_reflect` bug and its surviving twin are in the next section. |
| `archive/nn-har` | **PORTED 2026-08-16 — `16a91ce1` on `develop`. This row previously said the branch was BLOCKED ON A TEST CASE and that porting needed "a network-solid structure with diffraction data to refine against — a scientific input, not something that can be synthesised". That input existed all along, in `~/Dropbox/Quartz/`:** the Bern quartz measurement (Balmohammadi, via Grabowsky; Ag Kα, 100 K), and Max Davidson's thesis chapter 5 as the expected answers. NN is **nearest neighbour** — connectivity-based cluster selection with H-capping, which is how HAR is done on an extended solid where no molecule can be isolated. The port took the branch's NN logic but **not** its group construction, which predates the 2026 fragHAR repair `d840e322` by three years — all four of its commits sit inside the window when fragHAR was broken (`f0d7cfd3`, 2020-01-23 → 2026-06-01), and Max was patching fragHAR himself as he went. Its `fd956388` fragHAR/DFT fix was dropped as superseded, as this row predicted. **Six tests, in two tiers**: four short ones build fragments only (37 ms each, no SCF, no reflections) and assert the formulae Davidson Fig. 5.2 names — orthosilicic acid around the Si, silyloxysilane around the O; two long ones do the refinement (7 s and 21 s). **The thesis reproduces**: L1+H gives R(F) 0.0120 / GoF² 9.84 against 0.0127 / 10.61, with r(Si–O) and both U_iso agreeing to the digits printed — and Davidson's uncomfortable finding reproduces too, IAM GoF² 7.235 beating HAR's 9.84. Three live `cluster.foo` defects were fixed on the way, including an out-of-bounds write in `make_asym_occupation_list` and a `PURTE` attribute that had never been `PURE`. The "I AM LAZY" comment this row flagged is handled. Full record: `docs/NN_HAR_REPORT.md`. |
| `archive/release-pHAR-broken` | **TEST RESCUED AND PASSING 2026-08-16.** This branch was recorded as the one archive with an open dependency: a 167 MB CRYSTAL23 wavefunction stored as a **134-byte Git LFS pointer**, because `.gitattributes` was lost before the branch tip. `DEFERRED.md` called checking whether that object was still retrievable *"FIRST STEP, and it decides everything below"*, and it had never been run because `git-lfs` was not installed. **It is retrievable.** Finding out needed no `git-lfs` at all — the LFS protocol is plain HTTP, so a `curl` against GitHub's batch API returns a signed download URL rather than an error; the object downloads to 174,978,609 bytes whose sha256 matches the oid exactly. **The test then passed on current `develop`**, reproducing the 2025-04 reference digit for digit: R(F) 0.005188, N_r 20, N_p 11, GoF² 0.631422, scale 0.979852, *"Structure fit converged."* So pHAR — which ships in the library with `MOLECULE.CE:phar_defragment` live and nothing testing it — is now known to work. **One line had to be ported, not worked around**: the job died on `unknown option: thermal_smearing_model=`, a keyword removed by `acb7af0b` *"in favour of deriving the info from partition_model"*; the job already sets `partition_model= oc-crystal23`, which carries it. **The asset is deliberately NOT committed.** Restoring a `.gitattributes` and porting the pointer — which the old note suggested — would make every clone pull 167 MB, since LFS smudges the checked-out ref automatically. `develop` and `master` carry **zero** LFS objects (`git lfs ls-files develop`), and must stay that way; the asset lives only on this tag and `scripts/fetch_phar_asset.sh` pulls it on request, verifying sha256 and deleting on mismatch. The test **skips** without it, printing how to get it. `stdout` was re-blessed: the numbers were unchanged, but the old reference echoed the removed keyword and carried a "Form factor asymmetry" section this build no longer prints. Runtime 3 m 14 s. **Not** rescued: the form-factor symmetrisation residual tables in the same 8 commits. |
| `archive/Bader` | **SERIAL HALF PORTED 2026-08-18.** The largest capability left on any tag, and the port needed no `types.foo` change at all: the branch's three new types and its `basin_at_vertex` members serve only the parallel half or are never read. Ten new procedures across `molecule.prop.foo`, `isosurface.foo`, `marchingcube.foo` and `plot_grid.foo`, reached by a **new keyword `get_bader_basins`** so that `get_Bader_regions` and the older `cubes_to_basin` are untouched and the two algorithms can be compared. **It compiles clean, runs to exit 0, and does not yet give a usable answer** — and both reasons were found by running it, not by reading it. The basin count is wildly grid-dependent: 1 basin for water on a 21 × 19 × 15 box, **13942** on a wider 41-point box, where flat outer density makes every point its own maximum. And `sum(VOL)` counts one voxel per grid *point* while `pixel_volume` divides by grid *intervals*, overstating volumes and electron counts by ∏ n_i/(n_i−1) — 18.75% on that grid; correcting for it gives 10.29 electrons against a true 10, which is what confirms the density integration itself is roughly right. Four things were deliberately not taken: the parallel linked-list rework (mid-debug — its convergence loop is a hard-coded three iterations and every merge branch leaks; see `DEFERRED.md`), the `PARALLEL:gather` template (`develop`'s is better — `root` required rather than an optional passed straight to `MPI_GATHER`), the narrowing of `MARCHINGCUBE.edge_vertex_index` from `(0:12)` to `(0:11)` **and** the disabling of the 358-line `divide_cubes_small_map` that it forced, and three dead procedures the branch defined and never called. Full account: `docs/BADER_REPORT.md`. |
| `archive/libxc` | **Do not port as it stands; it is a prototype.** It is the most valuable of the small branches — a capability rather than a printout — and its two hardest judgement calls are correct. But it wires one of the four functional dispatch routines, and that one dereferences absent arguments on exactly the functionals it added. Assessed 2026-08-12; full findings below. |

### What was ported, and how

`CRYSTAL:put_unit_cell_geometry_cartesian` — the unit cell geometry printed in
cartesian coordinates and Ångström, rather than the fractional coordinates of
its sibling `put_unit_cell_geometry`. The port needed exactly two mechanical
changes, and every other symbol it uses still exists:

- `.associated` → `.allocated` (five `ENSURE`s)
- `.unit_cell.direct_matrix` → `.unit_cell.direct_mx` — a rename; the old name
  is gone from `master`

Four unused locals (`fac1`, `fac2`, `fac3`, `cartesian`) and some commented-out
alternatives were dropped. It is reached by the **new keyword
`put_unit_cell_geometry_cart`** rather than being called from `put_crystal` as
on the branch — deliberately, so that no existing test reference changes and
nothing has to be reblessed.

### `archive/release-td-old` in detail — the `symmetric_reflect` bug, and its surviving twin

The branch was recommended as the next rescue on the strength of `ac8b2af4`,
*"Major bug fix in `symmetric_reflect`!"*, on the reasoning that a bug fix
stranded on an archive tag means the live code still has the bug. **That
reasoning was sound and the premise was wrong**: the commit is on `develop` as
`e28bd649`, and `git blame` puts it on the live lines. The lesson is that a
branch commit must be checked against the tree by patch, not by whether the
branch was ever merged — `git cherry` answers it in one command.

The bug itself is worth recording, because it recurs. The lower-triangle branch
of `MAT{INTRINSIC}:symmetric_reflect` read:

```foo
do j = 1,.dim1
do i = 1,i-1              ! bound names the loop variable it controls
   self(j,i) = self(i,j)  ! same direction as the upper branch
```

Two defects in three lines: the inner trip count is computed from an undefined
`i`, and the assignment sets the *upper* triangle, which is what the other
branch already does. The fix corrected both.

**One copy survived, and was fixed on 2026-08-17**: `MAT3{REAL}:symmetric_reflect_23`
carried the same three lines, unamended, at `mat3{real}.foo:236`. It could not
fire — its only callers, `molecule.fock.foo:739` and `:903`, call it bare, so
`upper` defaults to `TRUE`, and no call site anywhere in `foofiles/` or
`runfiles/` passes `set_lower`. The same is true of the `MAT{INTRINSIC}` version,
so that 2025 fix was itself pre-emptive. Both were landmines rather than wrong
answers: the first caller to ask for the lower triangle would have got an
undefined loop bound. No test reference changes, since no test reaches the path.

The defect class is greppable, which is how the survivor was found:

```bash
grep -rnP 'do\s+([A-Za-z_]\w*)\s*=\s*[^,\n]+,\s*\1\s*[-+]' foofiles/
```

That pattern — a `do` whose bound names its own loop variable — now returns
nothing across `foofiles/`.

### Kang's form-factor symmetrization: mostly merged, one extension is not

`e22e2569` *"Added Kang residual symmetrization updated"* (2025-04-25) is one of
the commits still stranded on `release-td-old`, but it is an **update** to work
that is already live. On `develop`:

| | state |
|---|---|
| `SPACEGROUP:symmetrize_unique_SFs(sf,stabilizer,refl,diff)` | present, called from `CRYSTAL:symmetrize_FFs` |
| `asymmetric_FF_symmetrization_rss` and its "Form factor asymmetry" table | present — `crystal.foo:7716`, headed *"By Kang. 10.Mar.2025"*, printed by five HAR test references |
| `.crystal.asymmetric_unit_geometry.destroy` in `molecule.read.foo` | present, at line 1275 |

What the 2025-04-25 commit adds on top is the **maximum** residual beside the
root-sum-square: `asymmetric_FF_symmetrization_rmax` and
`..._rmax_hkl`, reported with the reflection that produced it, which needs three
`OUT` arguments on `symmetrize_unique_SFs` instead of one. That is the
"form-factor symmetrisation residual tables" listed as unported from the pHAR
branch. It is additive and self-contained.

Two riders in the same commit should **not** be taken with it. One moves
`put_CIFs`/`put_tonto_fcf_XCW`/`put_olex_fcf_XCW`/`put_xd_fco_XCW` out of the
`if (.crystal.xray_data.do_residual_cube)` guard in the XCW lambda loop, so every
XCW run would write those files; `develop` keeps them inside the guard, and with
`output=FALSE` rather than the branch's `TRUE`. The other is a comment noting
that `.reflections(n).stl` is printed in Bohr⁻¹.

**Open question, not a claim.** The rescued pHAR job does not print the
"Form factor asymmetry" table, although the 2025-04 reference it reproduces
digit for digit did — noted in `tests/long/ammonium_borane_pHAR_C23/IO`. The
table is gated on `.asymmetric_FF_symmetrization_rss.allocated`, which is set
only by `CRYSTAL:symmetrize_FFs`, whose only caller is `molecule.har.foo:612`.
So the pHAR path appears not to symmetrize its form factors while the HAR path
does. The numbers being unchanged says the difference is not affecting this
structure's result, but whether pHAR *should* symmetrize has not been asked of
anyone who would know.

### `archive/libxc` in detail — what is worth keeping, and what blocks a port

Peter Spackman, four commits, August 2017, about 150 lines: `cmake/FindLibxc.cmake`,
a `-DLIBXC=ON` option, a `USING_LIBXC` macro set in `cmake/SetFortranFlags.cmake`,
an `XCFUNC` wrapper type in `types.foo`, and an `#ifdef` fork inside
`DFT_FUNCTIONAL:new_r_energy_density`. The SBF submodule bump in the first commit
was incidental and is now moot.

**Two judgement calls the branch got right**, and they are the ones a
reimplementation is most likely to get wrong:

- **The energy convention.** Tonto's `E` is the functional *divided by the
  density* — `new_r_LDA_x_energy_density` computes `-(3/4)(3/pi)^(1/3) rho^(1/3)`,
  which is the energy per particle, not an energy density. libxc's
  `xc_f03_lda_exc` / `xc_f03_gga_exc` return exactly that quantity, so the
  branch's `E = E + EXC` is dimensionally consistent. A reimplementation that
  assumes `E` is an energy density is wrong by a factor of rho.
- **The VWN variant mapping.** `b3lypx` is mapped to `XC_HYB_GGA_XC_B3LYP5`
  (VWN5) and `b3lypgx` to `XC_HYB_GGA_XC_B3LYP` (VWN_RPA, i.e. VWN3). That
  matches Tonto's own `b3lypc`→VWN5 and `b3lypgc`→VWN3 split.

**Six reasons it cannot be merged as written**, in order of severity:

1. **Only the energy is wired; the potential is not.** The SCF is driven through
   `new_r_potential` (`molecule.fock.foo:5066`), which the branch does not touch,
   and neither unrestricted routine is touched either. A `-DLIBXC=ON` build would
   converge on Tonto's own functionals and then report an energy from libxc's —
   silently inconsistent and non-variational whenever the two differ. Wiring the
   potential is not a small addition: libxc returns `vrho` and `vsigma`, which
   still have to be assembled into the `V0` / `Vx,Vy,Vz` form Tonto expects.
2. **An absent-optional dereference on every LDA run.** `sigma = Nx*Nx + Ny*Ny +
   Nz*Nz` is evaluated unconditionally, above the `select case`, but `Nx,Ny,Nz`
   are `optional` and the routine's own `ENSURE` permits them absent when
   `is_LDA_functional(name)` — true for `slater`, `xalpha`, `vwn5` and `vwn3`,
   which are four of the cases the branch routes to libxc.
3. **`sigma` is never created or destroyed.** It is declared `VEC{REAL}@` with no
   `.create`; Fortran 2003 assignment auto-allocates, so it works by accident,
   but there is no `.destroy` and Tonto's memory accounting never sees it.
4. **The `b3lypc` no-op is correct only in pairs.** `exch` and `corr` are
   independent input keywords (`molecule.fock.foo:5027-5028`). Routing `b3lypx`
   to the whole B3LYP XC functional and making `b3lypc` do nothing is right only
   for that exact pair: `exch=b3lypx, corr=lyp` double-counts correlation, and
   `exch=b3lypx` alone acquires correlation that was not asked for.
5. **It no longer parses.** `XCFUNC` is raw Fortran inside a Foo `type` block
   (`type(xc_f03_func_t) xc_func`, with no `::`). `Foo.g4` makes a `typeDef` body
   `(varDecl | NEWLINE)*`, and `varDecl` requires `name :: TYPE`. The line-based
   `foo.pl` accepted this in 2017; the ANTLR4 grammar does not. The type's
   `xc_info` member is also declared and never used.
6. **Lowercase `use`.** `types.foo` documents the convention — a capital `USE`
   stops the preprocessor treating the module as a build dependency — and the
   branch writes lowercase `use xc_f03_lib_m` in both files.

**How the branch's code stands against a current libxc** — measured on 2026-08-12
against the packaged `libxc-dev` 5.2.3, by compiling the branch's exact call
pattern rather than by reading release notes:

- **The `xc_f03` interface is stable, and the branch's calls are the right shape.**
  `xc_f03_func_init`, `xc_f03_func_end`, `xc_f03_lda_exc` and `xc_f03_gga_exc` all
  exist in 5.2.3 with the same names and argument order, and all seven functional
  constants the branch uses (`XC_LDA_X`, `XC_LDA_C_VWN`, `XC_LDA_C_VWN_3`,
  `XC_GGA_X_B88`, `XC_GGA_C_LYP`, `XC_HYB_GGA_XC_B3LYP`, `XC_HYB_GGA_XC_B3LYP5`)
  are present and absent from `xc_funcs_removed.h`. Upstream describes the
  `libxcf03` API as unchanged for about eight years.
- **One genuine break: `np` became `size_t` in libxc 5.0.0.** Tonto's `INT` is
  `integer(4)` (`include/macros.in:86`, `INT_KIND=4`), so passing `N0.dim`
  produces *"Type mismatch in argument 'np': passed INTEGER(4) to INTEGER(8)"* at
  every call site. Converting to `integer(c_size_t)` is the only change the
  branch's calls need — with that one edit the whole pattern compiles clean.
  This is a loud, compile-time failure, so it is a nuisance rather than a risk.
- **On libxc 7.0.0 and later a second `use` is required.** The functional
  constants were split out into `xc_f03_funcs_m`, so `use xc_f03_lib_m` alone no
  longer defines them.
- **`FindLibxc.cmake` will misreport on libxc 6 and later.** It runs three
  `find_library` calls and appends each result to `LIBXC_LIBRARIES` **without
  checking any of them succeeded**. libxc 5.0.0 turned `libxcf90` into a duplicate
  of `libxcf03` and 6.0.0 removed that duplicate, so from 6.0.0 the `xcf90` search
  fails, `LIBXC_F90_LIBRARY-NOTFOUND` is appended to the link line, and
  `find_package_handle_standard_args` still sees a non-empty `LIBXC_LIBRARIES` and
  reports the package found. Configure succeeds; the link then fails.

**A toolchain blocker independent of all of the above.** Ubuntu's `libxc-dev`
5.2.3 ships `xc_f03_lib_m.mod` built by **gfortran-15**, and Tonto builds with
**gfortran-14**, which refuses it outright: *"Cannot read module file … created by
a different version of GNU Fortran"*. This is the same constraint already
documented for MPI in `docs/TONTO_AND_MPI.md` — a Fortran `.mod` is compiler-version
specific. Using the distro package therefore means moving Tonto to gfortran-15;
staying on gfortran-14 means building libxc from source with it. Whichever is
chosen has to be decided before any code is written, and enforced in CMake with
the same kind of check the MPI compiler match already gets.

### The SBF question — raised and settled on 2026-08-11: the submodule is gone

`archive/libxc` bumps the `external/sbf` submodule in the same commit that adds
libxc. The two are unrelated: `cmake/FindLibxc.cmake` makes no reference to SBF,
and the bump was incidental. `master` is at a **newer** SBF commit than the
branch, so that part of the branch is obsolete either way.

More to the point, **SBF was already all but gone from Tonto** when this was
investigated:

- It was a submodule, from `https://github.com/peterspackman/sbf`.
- **Nothing in `CMakeLists.txt` or `cmake/*.cmake` referenced it**, so it was
  neither compiled nor linked.
- Of 137 mentions across `foofiles/`, 97 were on commented-out lines. Only three
  were live: two in `datafile.foo`, which was **commented out of the CMake source
  list**, and one vestigial `sbf_file_name :: STR` member of `ATOM_GROUP` in
  `types.foo` whose only readers were themselves commented out.

Two further things were checked before removing it, because neither is obvious
from the build files:

- `scripts/test.py` carries `diff_sbf`, `is_sbf` and a `--sbftool` option, but
  **no test `IO` manifest anywhere mentions a `.sbf` file**, so that path never
  triggers. It also shells out to an external `sbftool` binary, which the
  submodule never built.
- `tests/samuel/sucacb_energies_breakdown/stdin` names two `.sbf` files, but that
  directory holds only `stdin` and `stdout` — **no `IO` manifest**, so it is not a
  registered test and does not run.

**All of it was removed on 2026-08-11**, in two commits — the submodule first,
then the dead source it left behind. `external/lapack-release` is now the only
submodule; ANTLR4 is a release jar rather than a submodule, contrary to what
`CLAUDE.md` used to say.

The source cleanup deleted `foofiles/datafile.foo` and its commented-out line in
the CMake source list, the `sbf_file_name` member and the commented `type
DATAFILE` and `use sbf` in `types.foo`, three commented routines in
`atom_group.foo`, the commented `read_sbf`/serialize/`serialize_isosurface_sbf`
blocks in `molecule.read.foo`, `molecule.put.foo` and `molecule.ce.foo`, and the
`test_dump_file` scratch routine in `molecule.main.foo`. `scripts/test.py` lost
`diff_sbf`, `is_sbf` and the `--sbftool` option, whose default path pointed into
the deleted submodule.

Comments that merely *mentioned* SBF inside live routines were reworded rather
than deleted, since removing the whole line would have taken working
documentation with it — `breakdown_data.foo:176`, and a stale `read_wfn_file`
doc line in `molecule.read.foo` that advertised reading "a tonto .sbf file" when
the body had only ever handled `.fchk` and `.molden`.

**One reference is deliberately left**: `tests/samuel/sucacb_energies_breakdown/`
names two `.sbf` files in its `stdin`. It is test *data* for a job that has no
`IO` manifest and is registered with no `CMakeLists`, so it does not run; it is
left alone rather than edited, along with the rest of that unregistered
directory.

## Why archived work cannot be merged into `master`, only ported

The short statement is that a merge is technically possible and practically
worthless. It is worth being precise about which part of the problem is which,
because the visible cause is not the largest one.

### The visible half: the Foo procedure-attribute separator changed

Commit `3ca1e53d` (2026-07-09) replaced the `:::` procedure-attribute separator
with `::` across 184 files. `master` now contains no `:::` at all; every archived
branch is written entirely in the older form.

| Branch | `:::` lines in `foofiles/` | Branch | `:::` lines |
|---|---|---|---|
| `kanghyun` | 11,575 | `libxc` | 7,675 |
| `release-td-old` | 11,544 | `cubes_to_basin` | 7,734 |
| `release-pHAR-broken` | 10,046 | `lorraine` | 7,749 |
| `bond-energy` | 9,780 | `Bader` | 8,125 |
| `nn-har` | 9,733 | `master` | **0** |
| `lamaGOET` | 9,702 | | |

Taken alone this would be a nuisance rather than a barrier: the substitution is
mechanical, and could be applied to a branch before merging it.

**There is a tag for this.** `foo-old-syntax` (`ae306e1d`, the migration's
parent) is the last commit in the old dialect, and the only point in the history
where old-syntax Foo can still be built and tested with the current toolchain —
`foo.pl` was already gone, `CMakeLists.txt` already invoked `FooToFortran`, and
CI already ran `ctest -L short`. Porting is easier from there than from `master`,
because the dialect matches and only the semantic drift below has to be resolved:

```bash
git checkout -b port-lamaGOET foo-old-syntax
git merge archive/lamaGOET      # same dialect; build and test here
# then replay the ::: -> :: migration on the result
```

### The larger half: the library API migrated underneath the branches

Trial-merging four branches with `git merge-tree` produces a conflict in **every
file each branch touches** — including `lamaGOET`, which is two commits and 129
added lines. That branch's single conflict is the whole problem in miniature:

```
<<<<<<< origin/master
      if (.fragment_geometry.allocated)        .put_fragment_data
      if (.unit_cell_geometry.allocated)       .put_unit_cell_geometry
=======
      if (.fragment_geometry.associated)        .put_fragment_data
      if (.unit_cell_geometry.associated)       .put_unit_cell_geometry
      if (.unit_cell_geometry.associated)       .put_unit_cell_geometry_cartesian
>>>>>>> origin/lamaGOET
```

This is not a `:::` conflict. It is `.allocated` against `.associated` — a
pointer-to-allocatable migration carried across the whole library:

| | `.allocated` | `.associated` |
|---|---|---|
| `master` | 3114 | 30 |
| `lamaGOET` | 1206 | 2695 |

A near-total inversion. The branch's genuine contribution in that hunk is a single
line, `put_unit_cell_geometry_cartesian`; everything else in the conflict is churn
that happened around it.

Other migrations the archived branches predate:

- **GNU long options only.** `command_line.foo` carries 25 `--` option lines on
  `master` against 2 on `Bader`; the single-dash spellings were removed.
- **`parallel do … reduce(x)`.** 17 sites on `master`, a construct that did not
  exist when any archived branch was written.
- **Submodule splits.** Large classes such as `MOLECULE` were divided across
  `molecule.*.foo` files.
- **`PURE` against `pure`.** The case now carries meaning that it did not before.

### The conclusion

Git will produce a merged tree on request. That tree is textually merged and
dialectally incoherent: the branch side is written in a language variant the
translator no longer accepts, against an API that no longer exists. A person has
to rewrite each hunk by hand, which is porting, not merging.

Porting from an `archive/*` tag is therefore exactly as easy as porting from the
branch would have been. Nothing was made harder by archiving; the branch list
simply stopped advertising twenty pieces of work as though they were pending
merges, when none of them were.
