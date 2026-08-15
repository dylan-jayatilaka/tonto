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
| `archive/release-td-old` | Dylan Jayatilaka (13), Kanghyun Chu (4) | 2025-08 – 2025-09 | Time-dependent and CIS work. `td_data.foo` reworked, M=0 singlet detector, `S_list` array, MS=0 option in CIS, MGS/Householder orthonormalisation in Davidson, and a major `symmetric_reflect` bug fix. |
| `archive/bond-energy` | Dylan Jayatilaka (11) | 2020-09 – 2020-10 | Roby bond-energy analysis. `roby.foo` +1478 lines: `Eshared` partitioning for E^DE, exact energy-density method, deformation energies, group populations. |
| `archive/release-pHAR-broken` | Kanghyun Chu (8) | 2025-04 | **Referenced by `DEFERRED.md`.** The only ammonia-borane pHAR test in existence, plus form-factor symmetrisation residual tables. See the note below. |
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

### The pHAR test is the one archive with an open dependency

`DEFERRED.md`, *"Reinstate the ammonia-borane pHAR test"*, depends on
`archive/release-pHAR-broken`. That tag is the authoritative copy:
`release-td-old` shares four of its commits but lacks `Crystal23_InputFiles.zip`.

The test remains blocked for the reason recorded there — `GenerateXML.XML` is a
134-byte Git LFS pointer rather than the 167 MB object, because `.gitattributes`
was lost before the branch tip. Archiving the branch does not change that, and
does not make it worse: the pointer and every other file are preserved in the tag.

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
| `archive/nn-har` | **RE-ASSESSED 2026-08-15: worth porting, and master already carries half of it as DEAD CODE.** NN is **nearest neighbour**, not neural network -- `types.foo` says so plainly: *"Determines the number of nearest neighbour to use for periodic network solids. Default is 1. Everything beyond this number is set as Hydrogen atoms."* This is connectivity-based cluster selection with H-capping, which is how HAR is done on an extended solid where no molecule can be isolated. **`CLUSTER:do_NN_defragment` is LIVE on master** (`cluster.foo:1167`) **but has no live caller**: its call sites are commented out at `cluster.foo:450` and `molecule.base.foo:1776`, and the whole of `make_NN_capped_groups` is commented out at `molecule.base.foo:1753`. The three `CRYSTAL` keys that switch it on -- `NN_level`, `use_NN`, `use_NN_capping` -- were never brought across at all. So somebody ported the machinery and not the wiring, and the branch holds the wiring. Its `fd956388` also carries a fragHAR/DFT fix that IS superseded (a `scf_kind(1:7)=="fraghar"` prefix test, now `CRYSTAL:do_fragHAR`, `crystal.foo:3073`), so that part can be dropped. Caveats: old `:::` dialect, and an acknowledged gap in its own comment -- *"NOTE ERRORS SHOULD ALSO BE SET TO ZERO HERE, BUT I AM LAZY MD 10/02/2023"*. |
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
