# Porting the rest of `Lolo_CP2K`

> **EXECUTED 2026-08-23 on branch `Lolo2`, commit `70a7a085`.** This page is
> kept as the record of what was *planned*. What actually happened — including
> the one thing this plan got wrong, that `912b32b2` carries the whole
> `oc-observed` model and not merely dialect churn — is in
> **`docs/LOLO_CP2K_PORT.md`**, which is written for Lorraine and is the page
> to read first.

**Written 2026-08-23, no code ported yet.** The two extinction commits from this branch
are already on `develop` — see `docs/EXTINCTION_REPORT.md`. This plan covers everything
else on it.

Work on branch **`Lolo2`**, cut from `develop` at the extinction merge.

**`Lolo_CP2K` is live and must not be touched.** It is Lorraine Malaspina's active
development, still receiving commits. Everything below is a hand-port *from* it onto
`develop`; nothing is pushed to it, rebased, merged into it, or deleted. It stays live for
as long as she wishes.

**Decided by Dylan, 2026-08-23: we port, she does not.** Lorraine has no knowledge of how
far Tonto has moved in the last two months — 622 commits past her fork point, including
the two dialect migrations of §3 — so asking her to port would mean asking her to learn a
codebase that changed under her. The work is ours. This also settles the question the
first version of this plan left open about duplicated effort: there is none.

## 1. The branch inventory, for the record

| Branch | Last commit | Status |
|---|---|---|
| `master` | 2026-08-16, Dylan | stable |
| `develop` | 2026-08-23 | integration |
| `Nice-branch` | 2026-08-12, cassam | **not ours** — Cassam-Chenaï's group in Nice |
| `Lolo_CP2K` | 2026-08-21, Lorraine A. Malaspina | live, the subject of this plan |
| `wip/sauce` | 2026-08-23, Dylan | a one-commit snapshot of the extinction `Infinity` fix, **already on `develop`** in `4ad76d98`. Nothing to port. |
| `gh-pages` | 2014-06-28 | the Jekyll site |
| `extinction-reactivation` | — | **deleted 2026-08-23**, locally and on the remote, once merged into `develop` at `89dbacef`. A short-lived feature branch that had served its purpose. |
| `Lolo2` | 2026-08-23 | this port |

So apart from `Nice-branch` there is one live branch carrying unported work: `Lolo_CP2K`.

## 2. Excluded, and why

Two commits are pure dialect churn and must **not** be ported.

| Commit | Size | What it is |
|---|---|---|
| `6f7fa8cf` "Restore legacy Foo build compatibility" | 187 files, +43867/−61928 | Converts `::` procedure attributes back to `:::` across the tree, so the branch builds with the legacy toolchain. The exact reverse of `3ca1e53d`. |
| `912b32b2` "trying to merge the observed density stuff…" | 185 files, +62350/−43877 | The mirror image — pulling newer state into the branch. Its real content was the SHELX extinction, already ported, plus the observed-density work that later commits refine. |

These two are why a cherry-pick will not work and why the whole port is by hand.

## 3. The dialect problem

`Lolo_CP2K` forks from `8ee220bc` (2026-03-12). `develop` is 622 commits past that point,
including two migrations the branch predates:

- `3ca1e53d` — `:::` → `::` for procedure attributes;
- `4cd995df` — submodule call qualifiers auto-resolved, so `.INQ:foo` and `.SET:foo`
  become plain `.foo` in most positions.

Every hunk therefore needs translating as it lands. This is the same exercise as the
extinction port, which went smoothly: read the diff, understand the change, retype it
against the current source, translate the file with `FooToFortran` to confirm it parses.

## 4. What is to be ported

Ten commits, about 840 lines of real change, in four groups.

### Group A — CRYSTAL23 and CP2K imported densities (five commits, ~110 lines)

| Commit | Files | What it does |
|---|---|---|
| `9ccdacf1` | `molecule.read.foo`, `molecule.scf.foo`, `types.foo` | **The silent one.** The XML holds a density matrix but no basis exponents or contraction coefficients, so Tonto paired a 46×46 CRYSTAL matrix with the 36-function POB-TZVP basis named in `basis_name` and reconstructed a density that could never be valid. Reads the XML's central-cell overlap matrix into a new `CRYSTAL23.central_overlap_mx`, and adds `DIE_IF(.n_bf/=.crystal23.n_bf, …)`. |
| `fe66ca5b` | `molecule.read.foo`, `molecule.scf.foo` | Makes the overlap read optional (`look_for(…,found=found)`) and drops the hard `ENSURE`, so files without that block still work. Supersedes part of `9ccdacf1`; port them together. |
| `67c8e2dd` | `molecule.read.foo` | CP2K bridge XML shares Crystal23's container but uses opposite direct-lattice labels, and reports atoms in `[0,1)` while its periodic AO matrices use centred images. Detects `CP2K_TONTO_PERIODIC_DENSITY` explicitly and maps positions to `[-1/2,1/2)`. |
| `a8ea1092` | `molecule.main.foo` | Two keyword aliases: `cp2k_periodic_file_name=` and `process_cif_and_cp2k_data`. **Note `develop` already has `process_cif_and_cp2k_data`** (`molecule.main.foo:378`), so check before adding. |
| `b8f63c49` | `molecule.har.foo` | Imported-density refinements have no molecular MOs, so a missing `.density_mx` must not fall back to a molecular SCF. Introduces `external_density = observed OR partition_model=="oc-crystal23"` at three decision points, twice over. |

### Group B — periodic Hirshfeld (three commits, ~180 lines)

| Commit | Files | What it does |
|---|---|---|
| `4661a8a3` | 7 files incl. `molecule.rho.foo`, `types.foo`, the `diffraction_data` readers | A selectable periodic stockholder model — a new setting plus 100 lines of `molecule.rho.foo`. The largest behavioural addition after the merging work. |
| `0673ad05` | `molecule.scf.foo`, `tests/benchmark_c23_fourier_kernel.f90` | Optimises the periodic Hirshfeld Fourier kernel, with a standalone benchmark. The benchmark is a bare `.f90` outside the ctest tree; decide whether it belongs in `tests/` at all. |
| `a62eb998` | `molecule.scf.foo` | A one-atom fragment of a periodic `oc-observed` crystal *does* need a Hirshfeld denominator, because neighbouring cell images contribute. Removes the `if (.n_atom<=1) return` shortcut for that case. |

### Group C — robustness (one commit, 6 lines)

`a35ed64a` — imported periodic densities define no molecular SCF block, but the Hirshfeld
reference atoms still need atomic-SCF defaults when ANOs are generated. Adds
`if (.SCF_data.deallocated) .set_SCF_defaults`. Small and self-contained; port first as a
warm-up.

### Group D — reflection merging and model-based pruning (one commit, 512 lines)

`e2a401ef`, pushed 2026-08-21. The largest and the most consequential.

Lorraine's own description: *"if a symmetry equivalent was in the data, Tonto was not
merging them at all — it silently kept the first and discarded the rest."* The commit
merges after every partitioning, SHELX style, and moves the pruning of systematic absences
and zero `F_calc` onto **the model** rather than the IAM, so that reflection 222 is not
removed from a HAR merely because the IAM says it is absent.

Touches `crystal.foo`, `diffraction_data.read.foo`, `diffraction_data.set.foo` (417 lines),
`molecule.har.foo`, `types.foo`.

**This overlaps directly with open work of ours.** `DEFERRED.md` carries *"Pruning compounds
across repeated `update` calls"*: `.reflection0` is no longer clobbered, but pruning still
edits `.reflections` in place, and restoring the working set from the pristine copy aborts
with a heap error because downstream state is sized against the reflection array. Since
`e2a401ef` rewrites much of the same region, **read it before doing any more work on that
deferred item** — it may resolve it, may conflict with it, or may make the restore
tractable by rebuilding the state that currently breaks.

## 5. Suggested order

1. `a35ed64a` — six lines, unrelated to anything else. Confirms the porting loop works.
2. Group A as one unit, in commit order `9ccdacf1` → `fe66ca5b` → `67c8e2dd` → `a8ea1092`
   → `b8f63c49`. `fe66ca5b` partially undoes `9ccdacf1`, so port the pair together rather
   than landing the strict version first.
3. `a62eb998`, then `4661a8a3`, then `0673ad05`.
4. `e2a401ef` last and on its own, with the deferred pruning item open beside it.

## 6. Testing — the real obstacle

**There is no CRYSTAL23 or CP2K test that runs.** The only C23 job in the tree is
`tests/crystal23/ammonium_borane_pHAR_C23`, whose 167 MB wavefunction is deliberately not
committed: it lives on the `archive/release-pHAR-broken` tag and `scripts/fetch_phar_asset.sh`
pulls it on request, with the test skipping when it is absent. So groups A, B and C would
land with no automated cover at all.

Two things are needed and neither can be synthesised:

- **A small CRYSTAL23 XML** — ideally one where the old code picked the wrong basis, so the
  `9ccdacf1` check is shown to fire. That is the test that proves the silent bug is closed.
- **A small CP2K bridge XML**, to cover `67c8e2dd`'s lattice convention.

Ask Lorraine. She has both, and without them this port is unverifiable.

Group D is different and better placed: it changes reflection handling, which every
existing HAR test exercises. Expect reference changes wherever symmetry equivalents are
present in the data, and treat any test whose reflection count moves as evidence rather
than as a failure.

## 7. Settled, and the one thing still open

**Settled 2026-08-23.** We port rather than Lorraine; the destination is `develop`, by way
of `Lolo2`; and `Lolo_CP2K` stays live and untouched.

**Still open, and it is the only thing that can block verification: the test data.** Groups
A, B and C touch code paths that nothing in `tests/` exercises. Two small files are needed
and neither can be synthesised:

- a small **CRYSTAL23 XML**, ideally one where the old code chose the wrong basis, so the
  `9ccdacf1` check is shown to fire rather than merely being present;
- a small **CP2K bridge XML**, to cover the lattice convention of `67c8e2dd`.

Lorraine has both. Not porting does not stop her supplying them, and this is the one place
where asking her is unavoidable — the alternative is landing three groups of changes on
`develop` with no cover at all, which the register in `docs/TONTO_AND_MPI.md` and the DFT
milestone both argue against.

Group D needs no new asset. It changes reflection handling, which every existing HAR test
already exercises.
