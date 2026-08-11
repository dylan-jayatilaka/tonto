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
| `archive/nn-har` | Max Davidson (3), Dylan Jayatilaka (1) | 2023-02 | Neural-network Hirshfeld atom refinement, with automated level switching and H-bond length normalisation. The author's own commit message records the results as underwhelming. |
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
