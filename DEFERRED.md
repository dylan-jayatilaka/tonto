# Deferred issues

Tracked for later attention. None of these block compilation; they are
correctness-of-match or robustness refinements.

*(This file began as `ANTLR4_DEFERRED.md`, a list of translator loose ends. It
now covers the whole project, so it was renamed.)*

> **Organisation.** Two halves. Everything above the archive is **live**; everything in
> [Archive](#done-resolved-and-closed-archive) is **finished** — done, resolved, withdrawn or
> won't-do — and is kept rather than deleted, so the reasoning stays searchable. Live entries
> are grouped by theme within that first half.
>
> The split had drifted and was re-sorted on 2026-08-26: eight finished sections were still
> filed under live themes (the two MPI-CI `DONE`s, the `TEXTFILE:flush` root cause, the
> withdrawn suppressed-reduction abort, the dropped-`data`-statements fix, the macOS RGBI
> badge, the `bond-energy` won't-do, and the keyword-parsing survey), and one live list
> (*RGBI: known defects*) was stranded inside the archive. **43 live, 17 archived.**
> If you add an entry, put it under a theme; when it closes, move it down — a `DONE` heading
> in a live section is how the drift starts.

| Theme | What lives there |
|-------|------------------|
| [Correctness](#correctness--open-bugs-that-give-wrong-answers) | Open bugs that give wrong answers, including ones with no diagnostic |
| [MPI](#mpi) | The milestone-4 defect register, the `parallel do` lock, the `MPI_Bcast` desync, architecture options |
| [Build system and toolchain](#build-system-and-toolchain) | `get_from` dependency trap, macro pruning, `types.foo` split, OpenBLAS |
| [Test suite and numerics](#test-suite-and-numerics) | Small numerical differences, `-O0` failures, NaN/negative ESDs |
| [Translator and the Foo language](#translator-and-the-foo-language) | Dropped `data` statements, name-case normalisation, F2008 submodules |
| [hart](#hart) | Remaining hart items and un-migrated runfiles |
| [RGBI and the picture toolchain](#rgbi-and-the-picture-toolchain) | Folding the five-ecosystem picture pipeline into one integrated codebase |
| [Tooling and editor support](#tooling-and-editor-support) | vim highlighting and integration |
| [Platform-specific](#platform-specific) | macOS/Apple Silicon, gfortran-16 |
| [Archive](#done-resolved-and-closed-archive) | Done, resolved, and won't-do — kept for the reasoning |

**Highest-priority open items**, if you are looking for where to start:
**macOS in CI** (feasible and free — the repo is public — and it is the only thing that can
guard the two arm64 `-O2`/`-O3` compiler pins, which today are guarded by nothing and whose
failure mode is wrong numbers, not crashes; see *Platform-specific*),
NaN and negative ESDs from the least-squares variance-covariance matrix, the MPI items
behind milestones 6 and 7, and **pHAR ships untested** — its only test is stranded on a branch
behind a 167 MB Git LFS asset (see *"Reinstate the ammonia-borane pHAR test"*). (`data` statements silently dropped: **fixed 2026-08-04** — see
below; the audit found no library file was ever affected.)

---

# Housekeeping

## Trim the oversized source comments once the debugging settles (Dylan, 2026-08-18)

**Status: open, deliberately deferred.** Several routines now carry long explanatory
comments written during debugging — how a defect was found, which compiler disagreed,
what the symptom looked like. `CLAUDE.md` §7a is the rule going forward: source comments
stay brief and explain the *code*, while the investigation belongs in a markdown file.
The existing slabs are **worth keeping for now**, because we are in the thick of the work
they describe and the context is still live.

When this settles, sweep them: keep the sentence that stops the bug being reintroduced,
move the rest into `DEFERRED.md` or the relevant `docs/` report, and leave a pointer.
Known offenders, all recent:

- `crystal.foo`, `CRYSTAL:put_asymmetric_FF_symmetrization_rss` — the noise-floor comment
  (the compiler-to-compiler numbers belong in a document).
- `spacegroup.foo`, `SPACEGROUP:symmetrize_unique_SFs` — the double-root explanation.
- `real.foo`, `REAL:get_dp_de_le` — three stacked notes on negative, NaN and tiny esds.
- `molecule.har.foo` and `crystal.foo` around the fragHAR and disk-FF repairs.

A useful check while sweeping: a comment longer than the code it describes is a candidate,
and a comment that names a compiler version, a date or a test name almost certainly is.

## The energy breakdown is inert on `develop` — present, switched off, driver missing (2026-08-18)

Found while closing the branch-recovery effort. `breakdown_data.foo` is **in the tree and
does nothing**, and it is disconnected in three independent places, so no single switch
turns it back on:

| piece | state on `develop` |
|---|---|
| `foofiles/breakdown_data.foo` | present, 1589 lines, 165 procedures |
| `type BREAKDOWN_DATA` in `types.foo` | present, line 7771 |
| `CMakeLists.txt` source lists | **commented out in both places** (`:378` and `:495`) — never compiled |
| keyword `put_energies_breakdown=` | **commented out**, `molecule.main.foo:407` |
| `MOLECULE.PROP:put_energies_breakdown` | **does not exist anywhere in `foofiles/`** |
| `breakdown_data` member on `MOLECULE` | absent |

So the commented-out keyword calls a procedure that is not there, and the module it
would need is not built. This is the same shape as `datafile.foo` before the SBF
removal: a file that looks like a live capability and is not one. It is not a
wrong-answer bug — nothing can reach it — but it is 1589 lines of source that no build
has compiled, so nothing has checked it still translates.

**Most of the code is already here.** The newest version of `breakdown_data.foo` on any
ref is on `archive/release-td-old` at 1593 lines — **four lines** more than `develop`'s.
So the bulk landed; only the wiring did not:

| what is missing | where it is |
|---|---|
| `MOLECULE.PROP:put_energies_breakdown`, the driver | `archive/release-td-old`, `molecule.prop.foo:6561` |
| the live keyword line (`.PROP:put_energies_breakdown`) | same tag, `molecule.main.foo:389` |
| the two `CMakeLists.txt` uncommentings | same tag, commit `7c71e672` |
| `MAT{REAL}` / `MAT{CPX}` / `OPMATRIX` helpers it uses | same tag, commit `74f33e26` |

**It is known not to work, and that is the real obstacle.** Two people have tried:

- `archive/energies-breakdown2` — Sam Thompson, `4b9bb359` *"Got tonto compiling"*
  (2024-03-07), then Dylan `7190faa0` *"Got a few numbers from breadown, failing in
  polarisation bit"* (2024-03-11).
- `archive/release-td-old` — Dylan again, `37a3875d` *"Fixed up more of Spackman's
  breakdown code"* (2025-04-25), `7c71e672` and `74f33e26` (2025-08-27).

So porting the wiring is a small, bounded job — an hour, and it would compile. Making the
numbers right is the actual work, it is a **scientific** question about the polarisation
term rather than a translation one, and the last two attempts stopped there. **Do not
port the wiring without deciding to finish the physics**, or the result is a reachable
keyword that produces a wrong energy decomposition — strictly worse than the present
state, where it cannot be reached at all.

The cheap alternative, if nobody intends to finish it: delete `breakdown_data.foo` and the
dead keyword line from `develop` and leave the work on its two tags, as was done for
`datafile.foo`. That is a decision for Dylan, not a default.

## Searched for in-core CCSD: it is not in this repository (2026-08-18)

Dylan recalled *"some work on in-core CCSD"* and asked for it to be found and extracted.
**There is no coupled-cluster solver anywhere in the repository.** Recorded so the
search is not repeated.

What was searched, all on 2026-08-18:

| where | how | result |
|---|---|---|
| `develop` library and runfiles | `grep -i ccsd`, and for `cc`/`ccd`/`cisd`/`lccd`/`cepa`/`mp3`/`mp4` procedure headers, and for `amplitude`/`t1`/`t2`/`doubles`/`coupled cluster` | nothing |
| every branch and tag (15 archive tags, 5 branches, all remotes) | `git grep -l -i ccsd` over `foofiles/*` and `runfiles/*` | nothing |
| commit messages, all refs | `git log --all -i --grep=ccsd --grep='coupled cluster'` | three hits, all about *structure factors* — see below |
| 131 dangling commits | `git fsck --no-reflogs`, then `git grep -i` over all of them for `ccsd`/`coupled.cluster` and for CC procedure headers | nothing |
| added filenames, all history | `git log --all --diff-filter=A --name-only` filtered for `cc`/`cluster`/`corr` | only `docs/CCTBX_INTO_TONTO.md`, which is cctbx, unrelated |

Every `ccsd` string in the tree is one of three innocent things: a **Gaussian**
CCSD density matrix read from an fchk file (`tests/{short,long}/urea_ccsd_pob-TZVP_*`,
whose job files say *"This fchk file has the CCSD density matrix in it"*), a
literature citation inside `basis_sets/TZP-DKH`, or documentation referring to those
tests. The three commits — `ed585a29` *"added ability to calculate CCSD structure
factors"*, `765463f2`, `306ec7f4` — add the ability to compute structure factors
**from** an externally-computed CC density, not to compute one. The only CC plumbing
in the library is `MOLECULE.READ:read_gX_CC_dm`, one line of fchk reading.

**What almost certainly is the memory.** The phrase "in core" appears in Tonto against
**MP2**, not CCSD, and four times: `MOLECULE.MISC:make_r_mp2`, `make_chem_mp2`,
`make_u_mp2` and `make_gc_mp2` are each documented *"make ... MP2 in core"*, and
`make_r_mp2` prints *"This is a toy implementation with integrals stored in memory"*.
Restricted, chemists'-notation, unrestricted and general-complex variants. All four are
already on `develop`, none is on any archive tag, and nothing needs extracting.
`make_r_mp2` was validated on 2026-08-18 as part of the `archive/Teaching` port — it
agrees with a hand-written MP2 to twelve decimals; see `docs/TEACHING_MP2.md`.

### The old Subversion repository was searched too, and it is not there either

Dylan then recalled the pre-GitHub history: a Subversion repository on SourceForge,
project name forgotten. **It is `tonto-chem`** — <https://sourceforge.net/projects/tonto-chem/>,
SVN at `svn://svn.code.sf.net/p/tonto-chem/code/`, browsable over HTTPS at
`https://svn.code.sf.net/p/tonto-chem/code/`. It is still live, at **revision 4411**,
spanning **1999-06-28 to 2014-06-21** — five years of history that predates the GitHub
repository and is in no way contained in it.

No `svn` client is installed here and none is needed: the whole log comes back from a
single DeltaV `REPORT` over HTTPS, and files can be fetched with plain `curl`.

```bash
# the entire 4411-revision log, authors and changed paths included, in one request
cat > logreq.xml <<'XML'
<?xml version="1.0" encoding="utf-8"?>
<S:log-report xmlns:S="svn:">
<S:start-revision>4411</S:start-revision><S:end-revision>1</S:end-revision>
<S:discover-changed-paths/><S:all-revprops/><S:path></S:path>
</S:log-report>
XML
curl -s -X REPORT -H "Content-Type: text/xml" -H "Depth: 0" --data-binary @logreq.xml   "https://svn.code.sf.net/p/tonto-chem/code/!svn/bc/4411/" -o svnlog.xml
```

What was checked, and the result in every case is **nothing**:

| check | scope | result |
|---|---|---|
| commit messages | all 4411 revisions | 6 hits, none a CCSD implementation: reading CC/MP2/MP3 densities from fchk files, the `cc-pVTZ` basis, CCSD **CIF/MIF** files, and `destroy ex,cc in copy` |
| changed paths | every add/modify/replace/delete in all 4411 revisions, 14 branches and 4 tags | no path ever named for CCSD; the only `cc`-ish matches are `cluster.foo` (the crystal cluster) and `cc-pVDZ` test directories |
| file contents, newest line | all **166** `.foo` files of `branches/tonto-3.2` | zero `ccsd`, zero `coupled cluster`, zero amplitude names |
| file contents, other line | all **165** `.foo` files of `trunk/tonto` | the same |
| the manual | `documentation/tonto.docbook` and `tonto.xml`, ~445 kB each | zero `ccsd` |
| authors | all 22 | listed below; none committed a correlation method beyond MP2 |

The 22 SVN committers, for the record, since they are not recoverable from the git
repository: `reaper` (1800, 1999–2005), `dylan_` (1475, 2005–2014), `dylan` (612,
1999–2005), `tonto` (144), `chris` (52, Roby work), `cassam` (51), `durhammike` (48),
`bucinsky` (40), `grimwreaper` (22, platforms and BLACS/ScaLAPACK), `magdalos` (19,
2013, HAR least squares and ADP errors), `awhitton` (14, 2003–2004, PND and
`diffraction_data`), `jjripsnorter` (12), `josh146` (9), `bdittric` (9),
`shadowadler` (3, fingerprints), `birger` (3, anomalous dispersion), `smeeton1` (2),
`vongrabow` (2, ELI-D), `mimisue` (1, vapour keyword), `root` (1), `josh` (1),
`skw` (1, Roby).

**Conclusion: Dylan's own guess was right — it was never committed.** The work exists in
neither repository, so there is nothing to extract. If a copy survives it is outside
version control: an unpushed clone on `sauce` or `achari2` (`git log --all --oneline`
there settles it in one command), a student's own machine or thesis, or an email
attachment. Absent that, the in-core MP2 family is the nearest thing that exists and it
is already on `develop`.

# Correctness — open bugs that give wrong answers

## ADP standard uncertainties are transformed element-wise on axis change (2026-08-16)

`ATOM:change_ADP2_axis_system_to` (`atom.foo:3733`) converts the ADP *errors*
between Cartesian and crystal axes by running the tensor congruence over the
standard deviations themselves:

```foo
.put_ADP2_errors_to(ADP)
ADP.change_basis_using(cell.direct_U_mx)   ! M sigma M^T -- on sigmas
.set_ADP2_errors_to(ADP)
```

That is invalid whatever the basis convention. The conversion is a congruence,
`U' = M U M^T`, so each `U'_ij` is a linear combination of **all six** `U_kl`
and its variance needs the full 6x6 covariance. Running it over sigmas forms
*signed* combinations of standard deviations, so terms cancel and an entry can
come out negative. The routine already carried `! ERROR: To fix later` twice.
The docstring admits it: *"the errors are transformed too, linearly … (this is
wrong, but in the absence of any covariance we do it)"*.

**Scope — smaller than it first appears, and this took three tries to pin
down.** There are TWO CIF ADP writers:

| path | esds from | correct? |
|---|---|---|
| `crystal.foo:8207` `put_CIF_ADP2(…,esd)` | `CRYSTAL:make_CIF_esds` | **yes** |
| `crystal.foo:8135` `put_CIF_ADP2(…)` | the atom's own `.pADP_errors` | no |

`make_CIF_esds` already does the right thing — it builds the induced 6x6 map
with `GAUSSIAN_DATA:symmetric_tensor_2_product_mx` and applies it as a
quadratic form to the covariance. So the claim that this "affects every
anisotropic ADP esd Tonto writes to a CIF" is **wrong**; it affects the
no-covariance writer, which is the `put_cif` path exercised by
`tests/short/urea_lamaGOET_grown_CIF`.

**A numerical claim that was made and is RETRACTED.** An independent
calculation appeared to show the CIF esds 38x too large on one component and
14x too small on another. That comparison was invalid: it assumed
`U' = M U M^T` with its own packing of the 6-vector, whereas Tonto's convention
per `symmetric_tensor_2_product_mx` is `D' = R^T D R` with the off-diagonal
doubling handled inside the packed form. Reproducing the ADP *values* validated
the 3x3 tensor map but said nothing about the packing, which is where the
factors of two live. **The size of the error is therefore NOT known.** Anyone
picking this up should measure it properly, against Tonto's own convention.

### The decision, and why it was parked

Dylan, 2026-08-16: **the ADP errors must be removed, not transformed and not
zeroed** — *"Leaving them allocated holding possible rubbish is not good, for
accidental later use."* Absent means "not available", which is true; zero means
"known exactly", which is false and would be divided by in every shift-on-esd
test.

An implementation was written and **reverted**, for a reason worth recording:

> **`ENSURE` does not enforce anything in a release build.** The recommendation
> in `docs/CCTBX_INTO_TONTO.md` §10 says destroying the errors is safe because
> *"the existing ENSUREs then catch any consumer that needs them, loudly and at
> the point of use"*. `ENSURE` is gated on `USE_PRECONDITIONS`, which is off in
> every optimised build, so those guards compile to nothing. Destroying
> `pADP_errors` turned a wrong-number bug into a **SIGSEGV** in
> `urea_lamaGOET_grown_CIF` — `put_CIF_ADP2_cryst` requires the array
> unconditionally. A consumer that must fail in production needs a `DIE`.

The debug build named it in one run (`ATOM:put_ADP2_errors_to_1 ... no
pADP_errors`, via `put_CIF_ADP2_cryst`), which is the recipe in
`docs/TONTO_DEVELOPER_INFO.md` §1a working exactly as advertised.

Making "absent" actually representable then means guarding **five** CIF
writers — 44 `_esu` column headers and 5 value/error table pairs — because
`pADP_errors` is one vector holding positions, U_iso and ADPs together, so
destroying it removes the coordinate esds too. That is a change to CIF output
shape in five places, each needing its own re-bless, and it is a different and
much larger job than fixing the transform.

### What to do

1. Fix `change_ADP2_axis_system_to` to propagate exactly, `V' = T V T^T`, when
   a covariance is available. `ATOM` already carries one — `covariance_mx`
   (`types.foo:2602`), stored by `set_pADP_errors_to` and already used as
   `.covariance_mx(4:9,4:9)` at `atom.foo:1578` — so no plumbing is needed.
   Build `T` by pushing each packed unit tensor through the same routine used
   on the tensor itself, so the convention cannot drift.
2. Destroy the errors when there is no covariance, per the decision above.
3. Guard the five writers so the `_esu` columns are omitted rather than filled.
   Re-bless the affected references; `urea_lamaGOET_grown_CIF` at minimum.
4. Better: consider splitting `pADP_errors`, or giving it a validity flag, so
   only the ADP block goes absent and the coordinate writers are untouched.
   This belongs with the parameter-descriptor migration in
   `docs/CCTBX_INTO_TONTO.md` §6 rather than as a separate change.

Related, and not fixed either: `molecule.har.foo:1300` implements `U_iso` by
writing three identical derivative columns (`sf_d(k,4) = sf_d(k,5) =
sf_d(k,6) = -sf2`), making the normal matrix singular by construction and
letting the pseudo-inverse absorb it. Verified present. It did **not** affect
the quartz results in `docs/NN_HAR_REPORT.md`, which refined anisotropically
and never entered that branch, but it would bite any isotropic refinement.

## Pruning compounds across repeated `update` calls (Dylan, 2026-08-23)

**Half fixed 2026-08-23. The other half is not a one-liner — a naive attempt
corrupts the heap.**

Found while making `tests/long/quartz_NN_HAR_L1_rhf_def2-SVP` refine twice, once
without the extinction correction and once with it, which puts a second
`xray_data=` block in the middle of a job.

`DIFFRACTION_DATA.SET:update` (`diffraction_data.set.foo:664`) does, in order:

```
.reflection0 = .reflections     ! "keep original reflections"
.reflection0.set_d_and_theta(...)
.reflections.set_d_and_theta(...)
...
.prune_reflections              ! operates on .reflections, in place
.sort_reflections
```

`.reflection0` is the pristine copy that ought to exist, and `types.foo:3502`
declares it as the original input structure factors before pruning. It is not
the cross-validation machinery — that is `CRYSTAL.xray_r_free_data`, a separate
`DIFFRACTION_DATA` holding the held-out reflections. It is maintained properly,
receiving `set_d_and_theta`, the equivalence factors and any `exp_scale_factor`
scaling alongside `.reflections`. But **the only place it is ever read is the
`show_rejects` diagnostic** (`diffraction_data.set.foo:727`), which prints the
list before and after pruning. Nothing refits from it and nothing prunes from it.

**Why it matters, in Dylan's words.** Re-running the pruning is legitimate: the
geometry has changed, so which reflections deserve to be rejected can change
with it. That is exactly why it must run against the *original* data. Pruning
the survivors of an earlier pruning can only ever remove more, so a reflection
rejected on the starting geometry can never come back once the model has
improved, even if it now fits perfectly well. The first pruning is done on the
worst model the job will ever have, and its verdict is currently permanent.

**Fixed: the copy is no longer clobbered.** It was refreshed from `.reflections`
on *every* call, so the second `update` overwrote it with the already-pruned set
and the original data was gone for the rest of the job. It is now taken once:

```
if (.reflection0.deallocated) .reflection0 = .reflections
```

No behaviour changes except that `show_rejects` now shows the true original.

**Open: pruning still works on `.reflections` in place**, so successive prunings
compound. The obvious fix — restore the working set from the pristine copy
before pruning again —

```
.reflections.destroy
.reflections = .reflection0
```

**was tried and aborts the quartz job** with `malloc(): invalid size (unsorted)`
during the second refinement. Both components are `VEC{REFLECTION}@`, i.e.
allocatable, and `REFLECTION` has no pointer components, so the assignment
itself is a legitimate deep copy. The corruption therefore comes from downstream
state that is tied to the reflection array and does not survive its wholesale
replacement — the calculated and predicted structure factors, the group
assignments, and whatever the fragment and refinement machinery sizes against
`.reflections.dim`. Replacing the array mid-job invalidates all of it.

So the remaining work is not the assignment; it is establishing what must be
rebuilt after a re-prune and rebuilding it. That is why this stays deferred.

**Scope.** Only jobs that call `update` more than once are affected, which in
`tests/` is the quartz job alone, and it prunes nothing — 1009 reflections in
both refinements. Nothing currently checked in gives a wrong answer. The defect
is latent, and becomes live the moment a job with cutoffs re-enters the block.

## DFT: three silent defects — see `docs/DFT_STANDARDISATION.md`

Found 2026-08-12 by measurement on `tests/short/h2o_blyp_cc-pVDZ`. The full
record, with the evidence, the fix, and the plan, is
**`docs/DFT_STANDARDISATION.md`** (milestone 10 in `CLAUDE.md`). Summarised here
only so this register stays complete:

| Defect | Effect | Status |
|---|---|---|
| `MOLECULE.SET:initialize_DFT_grids` destroyed and recreated the `BECKE_GRID` | **every** user grid setting discarded; all DFT ran at default `accuracy= "low"` while `put_basics` echoed the requested settings back | **FIXED** 2026-08-12 |
| `rho_cutoff` defaults to 10⁻⁶ | **the long-standing systematic error against g09.** Cross-validation isolated it: HF agrees to 1.2e-10 and Slater to 4.6e-7, but B88 differs by 9.9e-6 — because `x = \|∇ρ\|/ρ^(4/3)` *grows* in the tail the cutoff truncates. Lowering it to 10⁻¹⁰ collapses the full-BLYP gap **300-fold**, from 1.03e-5 to 3.5e-8, and is **free** — timed, no trend at any accuracy. Each derivative order costs another ρ^(-1/3), so meta-GGAs would be far worse | OPEN — change the default; batch the `types.foo` comment onto the next cascade |
| **RESOLVED 2026-08-14: three causes** (was: "open-shell DFT off by 1.5e-5, cause unknown") | (1) `pruning_scheme= jayatilaka2`, a confound introduced during the investigation -- removed for ROBUSTNESS, not average accuracy: it was actually better closed-shell (3.5e-8) but -1.5e-5 on an open-shell case where every alternative was within 1.6e-6. (2) the VWN5 potential grouped the chain rule wrongly. (3) the VWN3 potential evaluated `VWN_G`/`VWN_dG` at **ZERO instead of zeta**, so it had NO SPIN DEPENDENCE AT ALL. After all three: slater +1.44e-6, +vwn5 +1.455e-6, +vwn3 +1.511e-6 against g09 -- correlation now adds nothing of its own. Tonto's default grid sits ~1.5e-6 from g09; use 5e-6 for any external-reference test. See `docs/DFT_STANDARDISATION.md` section 6a | **FIXED** |
| **The grid needs far too many points for its accuracy** | At `accuracy= best` (65 radial, L71) every DFT case is ~1.5e-6 from g09, while the two HF cases -- which use no grid -- agree to 1e-10. The seven DFT numbers span only 1.44-1.63e-6 across different functionals, charges and spin treatments, so it is a GRID OFFSET, not functional error. g09 reaches 5e-10 of its converged answer on FineGrid (75,302), a broadly comparable grid. Something in the quadrature (partition weights, radial mapping, normalisation) is likely wrong; a rewrite of the grid construction should be considered. Sets the floor for `dft_reference`'s 5e-6 tolerance. See `docs/DFT_STANDARDISATION.md` section 6b | OPEN — not for now |
| `use_spherical_basis=` after the `atoms=` block | silently ignored — 25 basis functions instead of 24, 1.6e-3 Hartree, exit 0, no diagnostic | OPEN |
| Eight `case default; UNKNOWN(...)` lines commented out | an unrecognised functional name silently contributes nothing — `blyp` gives −67.7092 instead of −76.4002, exit 0 | OPEN |
| `gill96` blessed in three places, implemented nowhere | accepted name that computes nothing, indistinguishable from a typo | OPEN |
| `MOLECULE.SCF:put_SCF_energy` has no callers and mislabels its output | the XC energy is never reported, so none of the above is visible | OPEN |

**Two consequences that will surface elsewhere.** Every checked-in DFT reference
was produced with the default grid rather than the one its own input requests, so
they must be reblessed against converged numbers once the grid fix lands. And the
existing suite cannot detect any of these, because no test varies the grid and
every test spells its functional correctly.

## Command line: `command_arguments` silently truncates (and is never read)

**Found 2026-08-02** while sizing the `hart --group-charge-spin` option (milestone H1).
`COMMAND_LINE` has **two** independent 256-character limits, and Fortran truncates a fixed-length
CHARACTER assignment **silently** in both cases:

1. **Per token.** `item`, `option`, `option_value` and `arg` are all `VEC{STR}@`, i.e. `STR_SIZE`
   = 256 per element. A single quoted argument longer than that -- e.g.
   `--group-charge-spin "1 0 1 2 -1 1 ..."` for a protein -- is cut off with no error.
2. **The whole command line.** `command_arguments :: STR` is **one** 256-character string that
   `command_line.foo:134` appends *every* token to:
   `command_arguments = trim(command_arguments)//" "//trim(token.to_quoted_str)//" "`.
   That overflows after roughly a dozen tokens on **any** command line.

**The saving grace, and the fix.** `command_arguments` is **never read** -- `grep` finds no
consumer in `foofiles/` or `runfiles/`. It is written and discarded, so the truncation is
currently harmless. The honest fix is to **delete the field**, not widen it. (`put_command_optarg`
prints `command_optarg`, a `VEC{STR}` built per option, which is unaffected.)

**Design consequence for `hart --group-charge-spin`** (H1): do not take one quoted blob. Two
things keep every token short:
- make the option **repeatable** -- `--group-charge-spin 12 -1 1 --group-charge-spin 47 1 1`;
- make it an **exceptions list**, defaulting all other groups to `{0 1}`. The tonto keyword being
  mirrored already works this way and says so in its name: `atom_groups= { keys={charge=}
  altered_data= {...} }` -- *altered* data. For a protein nearly every residue is neutral singlet;
  only Asp/Glu, Lys/Arg and metal ions deviate, so a 300-residue structure needs a dozen entries.

For genuinely large cases, fall back to `--group-charge-spin-file <file>`, one `r C M` per line --
which also makes the setup reproducible and version-controllable, unlike a shell command.

**NOTE `COMMAND_LINE` does not currently support repeated options**: `has_option` /
`value_for_option` return the *first* match. Repeatability needs a small addition there.

## Command line: two latent bugs that only a debug build sees

Both were pre-existing in `COMMAND_LINE:process_options` and both are now fixed;
recorded because they are a *class* of defect this codebase is prone to — a real
precondition that `USE_PRECONDITIONS` gates away, so release ships the violation.

- **`VEC{STR}:append` with a mismatched CHARACTER length.** `append` is
  `self = [self,value]`, an array constructor, which requires `value` to have the
  same length as the array element. `process_options` appended `trim(token(2:))`
  and the literal `"none"` into a `VEC{STR}(len=256)`. Illegal Fortran; release
  does not check it, but `-fcheck` aborts — so **every option, in every program,
  killed a debug build**. Fixed by going through a full-width `STR` first.
- **`put_command_optarg` overflowed the output buffer.** It asked for a column
  `width=STR_SIZE` while `BSTR_SIZE == STR_SIZE`, so the 2-character margin put
  the cursor past the end and tripped `ENSURE(.item_end+len(string)<=BSTR_SIZE)`.

Worth a sweep for other `append(trim(...))` / `append("literal")` calls on
`VEC{STR}`, and for other `width=STR_SIZE` uses, on the same reasoning.

## Deferred: `std_err` writes into the *input* file (hard-coded unit collision)

**Found 2026-07-29** while instrumenting `table_column.foo` to chase the zero(error) problem.
A few `std_err.show(...)` calls added for debugging did not appear in any error log — they were
**appended to the `stdin` file**, which corrupted the input while it was still being read and
killed the job with:

```
Error in TEXTFILE:read_line_bad_EOF ... unexpected end of file
File name = stdin   Line number = 35
```

The input file grew from 55 to 101 lines during the run. Reproducible.

**Mechanism — partly established, not fully pinned.** The three units are distinct
(`TEXTFILE_STDIN_UNIT` 5, `TEXTFILE_STDOUT_UNIT` 6, `TEXTFILE_STDERR_UNIT` 7, `include/macros.in`).
But `create_std_err` (`textfile.foo`) never *opens* anything — it allocates the object and
claims the hard-coded unit:

```fortran
std_err.name   = "stderr"
std_err.action = "write"
std_err.unit   = tonto.std_err_unit      ! = 7, hard-coded
```

Fortran does not pre-connect unit 7, and Tonto hands out units dynamically when it opens files,
so the likely story is that unit 7 already belonged to another open file (the input) and the
write simply went there. That has not been proved — worth confirming with an `inquire` on the
unit at the point of the write.

**Why it matters more than it looks:** a diagnostic channel that silently destroys the input is
a trap exactly when someone is debugging, and it fails in a way that looks like a *parse* error
in the user's input rather than an I/O bug. It cost real time here.

**Suggested fix:** have `create_std_err` claim its unit the same way every other file does
(via the allocator, checking `unit_used`) instead of assuming a fixed number, or open the file
properly. Note `stdout`/`stdin` share the same hard-coded pattern and may be latently exposed
to the same collision.

**Naming note:** the object was renamed `stderr` → `std_err` (matching the existing `std_time`,
`std_name`, `std_output` family) because the Unix name implied Unix behaviour it does not have.
The *file* it writes is still called `stderr`, so test directories and `IO` manifests are
unaffected. The CPP macro `TEXTFILE_STDERR_UNIT` was deliberately left alone: it is one of a
trio with `TEXTFILE_STDIN_UNIT`/`TEXTFILE_STDOUT_UNIT`, and renaming just one would look odd.
Renaming `stdin`/`stdout` likewise is ~12,500 call sites across 81 foofiles but — importantly —
**zero** test churn, since the file names are set separately. Deferred as cosmetic.

---

# MPI

## Parallelise the Bader basin search (Dylan, 2026-08-18)

`MOLECULE.PROP:assign_Bader_basins`, ported from `archive/Bader` on 2026-08-18, is
serial. It is also the expensive part of the calculation: a steepest-ascent walk from
every interior grid point, each step scanning 26 neighbours, and it prints its own
`cpu_time` for exactly that reason. On any grid worth using it dominates.

The work is embarrassingly parallel in principle — each starting point's path is
independent — but the basins are not: a path stops as soon as it lands on a point
another path already claimed, so the assignment is order-dependent, and two ranks
walking neighbouring slabs will disagree about which basin a shared path belongs to.
That merge is the whole difficulty, and it is why the existing
`cubes_to_basin_parallel` is structured as a decompose-then-reconcile loop over
slab boundaries rather than a plain `parallel do`.

**There is prior art on the archive tag, and it was deliberately not ported.** Max
Davidson reworked `cubes_to_basin_parallel` on `archive/Bader` (9 commits,
2018-08 – 2019-02) to replace the boundary-plane rescan with a linked list of paths
that terminate on a partition boundary — `LINKED_LIST_MAT_INT` / `HEAD_MAT_INT` /
`BADER` in `types.foo`, plus a `PARALLEL:gather` template. His commit messages record
it as an improvement ("Times for parallel cube to basin now comparable to base
implementation"), and the idea is sound: reconcile only the paths that can cross a
boundary instead of sweeping the boundary plane.

It was left on the tag because the code is mid-debug, in two specific ways:

- **The convergence loop is disabled.** `do while (changes EQV TRUE)` was replaced by
  `do ii = 1,3! while (changes EQV TRUE)`. Basin labels propagate one partition per
  pass, so three passes is correct only up to four ranks and silently wrong above it.
- **Every merge branch leaks.** Each does `allocate(tmp); tmp => head.head`, discarding
  the node it just allocated on every pass of the outer loop. It also uses raw
  `allocate`/`nullify` rather than Tonto's `create`/`destroy`, so the memory accounting
  never sees the list.

Recovering it:

```bash
git show archive/Bader:foofiles/molecule.prop.foo   # cubes_to_basin_parallel
git show archive/Bader:foofiles/types.foo           # the three list types
git show archive/Bader:foofiles/parallel.foo        # gather -- superseded, see below
```

Two things not to take from the tag. `PARALLEL:gather` is already on `develop` in a
better form (`parallel.foo:4827` onwards — `root` required rather than optional, and a
`recvtmp` staging buffer); the branch version passes the *optional* `root` straight to
`MPI_GATHER` whether or not it is present. And the branch's `MAT3{VEC_{INT}}` spelling
is now `MAT3{EVEC{INT}}`.

**Before writing any of it, get a reference.** There is no Bader test of any kind —
serial or parallel — so there is nothing to compare a parallel result against. The
serial path needs a blessed reference first; see `docs/BADER_REPORT.md`.

## MPI: defects found during milestone 4 (2026-08-01)

Found while building Tonto with MPI for the first time — **no MPI build had ever been
configured**, in CI or in any local build tree, so none of this had been exercised. Items marked
**FIXED** were repaired as part of milestone 4 because they produce wrong answers and would have
made the numeric characterisation meaningless; the rest are recorded with evidence.

### The root cause behind most of these: the `parallel do` lock is invisible

`FooToFortran` lowers `parallel do` to `PARALLEL_DO_START/STRIDE` bounds plus a
`LOCK_PARALLEL_DO(tag)` emitted as **the first statement inside the loop body**, with
`UNLOCK_PARALLEL_DO(tag)` after `end do`. `work_is_shared` is
`.is_parallel AND .parallel_do_lock==" "` (`parallel.foo:287`), so **inside a `parallel do` body
`WORK_IS_SHARED` is always FALSE**, and every `PARALLEL_*` macro — each of which expands to
`if (WORK_IS_SHARED0) call …` — is a silent no-op there.

The intent (Dylan) is sound and standard: keep MPI "on the outside", parallelise coarsely, and
prevent interior collectives, which would otherwise deadlock on mismatched trip counts. The
*mechanism* is the problem:

- **Suppression is silent.** A reduction that never ran is indistinguishable from one that did.
- **It answers a dynamic question when the need is lexical.** `WORK_IS_SHARED` means "no outer
  loop is active"; what a reduction site needs to know is "am I inside the body or after it".
- **The unlock is name-matched, not depth-counted.** `unlock_parallel_do` clears the lock
  whenever the name matches (`parallel.foo:321`), so a recursive routine's inner return releases
  the outer loop's lock. The `ENSURE` that would catch recursion is commented out at
  `parallel.foo:308`.

Correctness therefore depends on the programmer holding the whole dynamic call sequence in their
head. As Dylan put it: *"the programmer has to hold the call sequence in their head not to make
bugs."* That is a design defect, not a discipline problem — the constraint is invisible at the
point where it matters.

> **DECIDED (Dylan, 2026-08-01): adopt all four of the following. This is URGENT and is the next
> piece of work after milestone 4's characterisation completes.** It is sequenced after, not
> during, because changing the `parallel do` lowering mid-characterisation would confound the
> numbers it is meant to produce. Tracked as milestone 6 in `CLAUDE.md` §9.

**1. Move the reduction into the loop construct (the main fix).**

```foo
parallel do q = 1,.n_shell_pairs reduce(grid)
   ...
end
```

lowering to the existing macros with the reduction emitted in the one place it is correct:

```fortran
do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
LOCK_PARALLEL_DO("MODULE:proc")
   ...
end do
UNLOCK_PARALLEL_DO("MODULE:proc")
if (WORK_IS_SHARED) then
   PARALLEL_SUM(grid)
end if
```

This is exactly OpenMP's `reduction(+:x)`, and it removes the failure mode entirely: the
programmer no longer chooses *where* the reduction goes, so it cannot be written inside the body,
inside a conditional, or forgotten. Every wrong-answer bug in the next section becomes impossible
by construction.

Implementation notes:
- `FooToFortran.java` already owns this lowering (it emits the `PARALLEL_DO_START/STRIDE` bounds
  and the `LOCK`/`UNLOCK` pair), so this is a grammar addition plus a few lines of emission.
- `Foo.g4` needs `reduce(` *ident-list* `)` as an optional suffix on the `parallel do` loop head.
- Support the variants actually used: `PARALLEL_SUM` (default), `PARALLEL_SYMMETRIC_SUM` and
  `PARALLEL_SYMMETRIC_SUM_23`. Suggested spelling: `reduce(x)`, `reduce_symmetric(m)`,
  `reduce_symmetric_23(m)` — or a single `reduce(x, kind=symmetric_23)`.
- **Replicated contributions need care.** Two of the fixed routines have a serial loop feeding
  the same array as the parallel one; the reduction would count it `n_ranks` times. The
  `is_master_processor` guard used in the fix is the pattern — the migration must check each site
  rather than mechanically adding `reduce(...)`.
- Migration: convert the ~88 existing `parallel do` sites incrementally. The old form keeps
  working, so this need not be atomic.

**2. Make suppression loud in debug.** Under `USE_PRECONDITIONS`, have `PARALLEL_SUM` and friends
abort when the lock is held. A reduction inside a locked region is *always* a bug, never a
legitimate no-op. Two lines in `include/macros.in`, and it converts silent wrong answers into
immediate aborts naming the routine. Do this one **first** — it is the cheapest, it is
independent of the grammar work, and it turns the remaining unfixed sites into loud failures
rather than quiet ones.

**3. Depth-count the lock.** Replace the name-matched `parallel_do_lock` string with a depth
counter, or have `lock_parallel_do` return the previous value for `unlock_parallel_do` to
restore. Then a recursive routine's inner return can no longer release the outer loop's lock.
Restore the recursion `ENSURE` at `parallel.foo:308` once this holds.

**4. Lint it in the translator.** Phase B already has the parse tree and a cross-module call
graph; flag any `PARALLEL_*` macro appearing lexically inside a `parallel do` body. This catches
hand-written sites that have not yet been migrated to `reduce(...)`, and it would have found all
four `molecule.grid.foo` bugs statically, without running anything.

Items 1 and 2 together would have prevented every wrong-answer bug listed below.

### Wrong answers under MPI — FIXED

- **FIXED. Four dead or missing reductions in `foofiles/molecule.grid.foo`.** In
  `make_electronic_pot_grid_r`, `make_mixed_ESP_grid_r` and `make_scm_ESP_grid_r` the
  `PARALLEL_SUM(grid)` sat *inside* its own `parallel do` (and, in two cases, additionally inside
  a rank-dependent `if`), so it never executed and every rank kept only its own `1/n_ranks` of
  the terms. `make_multipole_ESP_grid_r` had no reduction at all. Hoisted past the `end`.
  Two of them also have a **serial, replicated** multipole loop feeding the same `grid`, which a
  plain `ALLREDUCE` would have counted `n_ranks` times; those are now guarded with
  `tonto.is_master_processor`, which is a no-op in a serial build. Had the reductions inside the
  rank-dependent conditionals ever run, they would have been collectives in a rank-dependent
  branch — a guaranteed hang.
- **FIXED. Heap buffer overflow in `PARALLEL_SYMMETRIC_SUM_23`** (`foofiles/parallel.foo:571`).
  It sums "the lower half of the second two indices" but sized its triangle buffer from
  `mat.dim1`. Callers pass `MAT3{REAL}(n_states,n_bf,n_bf)`, so it wrote `n_bf*(n_bf+1)/2`
  doubles into a buffer of `n_states*(n_states+1)/2`, and `MPI_ALLREDUCE` then read past it.
  The `ENSURE` also checked `dim1==dim2`, i.e. asserted `n_states==n_bf`, firing spuriously in
  any `USE_PRECONDITIONS` build. Now `dim = mat.dim2` and `ENSURE(mat.dim2==mat.dim3)`.
- **FIXED. Three missing CIS/TDHF reductions in `foofiles/molecule.fock.foo`.** `r_CIS_S1_AV`
  had a `parallel do` and no reduction of any kind, leaving `F` a per-rank partial;
  `r_CIS_S0_AV` reduced `F` but not `K`, then computed `F = TWO*F - K` mixing a reduced quantity
  with an unreduced one; `u_CIS_AV` never reduced its `K` OUT argument. All three now use plain
  `PARALLEL_SUM` — not the symmetric variant, since these arrays are filled in both triangles.
  All are no-ops in a serial build.

### Architecture options for the MPI layer (considered 2026-08-02, deferred)

Discussed and deliberately **not** done now -- recorded so the reasoning is not lost. Sequenced
after MPI is working, per Dylan: MPI fully working (+ CI) -> hart -> tidies.

**(a) Split `parallel.foo` into its own repository, consumed as a submodule.** Dylan's original
idea: a type-generic MPI layer is genuinely reusable, and no good equivalent was available when it
was written. **Assessment: buys little, costs coupling.** `get_from` is *source-level inlining*,
not linking -- `parallel.foo` produces no `.F90` and no module, its procedures are textually
donated into `SYSTEM` at translation time, and the translator needs `types.foo`, `parallel.foo`
and `system.foo` **together** to resolve the templates. So it can never be a separately *compiled*
library while it remains a donor. A submodule would supply the source, but gains nothing over a
subdirectory while losing the ability to change the three files atomically. And what an outside
user could consume is the *generated Fortran* -- a build artifact they could not regenerate
without the Foo toolchain, i.e. a fork of a snapshot.

**(b) Promote `parallel.foo` from `get_from` donor to a real module.** This is the part of (a)
worth having, and it can be done entirely in-tree. A real module compiles to its own `.F90`,
appears in `FOO_SRC`, and therefore **acquires a dependency edge** -- fixing the stale-build trap
above structurally rather than by convention. **Effort: a couple of days with real testing, not an
afternoon.** Measured 2026-08-02:
- **1241** `get_from(PARALLEL:...)` donation lines in `system.foo` to remove;
- `SYSTEM`'s type in `types.foo` currently **duplicates** PARALLEL's components (`is_parallel`,
  `processor_rank`, `n_processors`, ... under a comment reading "inherited from PARALLEL"); these
  become one `parallel :: PARALLEL` component;
- **126 external call sites across 12+ files** (`archive`, `buffer`, `command_line`, `crystal`,
  `file`, `molecule.{ce,grid,misc,prop,put,scf}`, `plot_grid`, ...) where `.is_parallel` becomes
  `.parallel.is_parallel` and so on;
- every iteration touches `types.foo`, so every iteration is a full cascade;
- and it cannot be validated serially -- the point is MPI behaviour, so each pass needs an MPI
  build and the CIF tests.

**(c) Spike `mpi_f08` assumed-rank to see how much of the layer is now redundant.**
`parallel.foo` is 9865 lines of which **1175 are `get_from` instantiations** -- the cross-product
of type x rank, written out, because a Fortran dummy has fixed type and rank. MPI-3.0 (2012) added
`TYPE(*), DIMENSION(..)`, and the Open MPI we built confirms support
(`F08 assumed rank syntax... yes`).

**But this does NOT make the layer redundant, and the earlier claim in this file that it did was
too strong.** Inside a `TYPE(*), DIMENSION(..)` procedure you can do almost nothing with the
argument -- no indexing, no assignment, no type enquiry; you can only pass it to C. So `mpi_f08`
still requires the **caller** to supply count and datatype:
`call MPI_Bcast(x, size(x), MPI_DOUBLE_PRECISION, ...)`. Tonto's layer *derives* both from the
argument type (`LEN?=>size(buffer)*len(buffer)`, `MPI_TYPE?=>MPI_CHARACTER`), so the caller writes
`PARALLEL_BROADCAST(x,root)` and cannot get them wrong. Assumed rank solves the **plumbing**; the
templates solve the **inference**, and the inference is the part that prevents wrong-count and
wrong-datatype bugs. Worth a spike on `broadcast` alone to see how many of the 1175 collapse --
but go in expecting to keep the inference layer.

### BUILD-SYSTEM TRAP: `get_from` donors are invisible to the dependency graph

Per Dylan: "always been an issue." Recorded now with a concrete demonstration and a fix.

**The mechanism.** Each translation step declares (`CMakeLists.txt:764`):

```cmake
DEPENDS ${foopath} ${FOO_TYPES_FILE} ${ANTLR4_TRANSLATOR_STAMP}
```

i.e. its own `.foo`, `types.foo`, and the translator -- and **nothing else**. But the translator
follows `get_from(...)` and inlines *donor* files. Those donors are not in the DEPENDS list, so
**editing a donor rebuilds nothing**: you get a silently stale binary, no warning, and a test run
that appears to exercise your change and does not.

That also explains the asymmetry everyone notices: editing `types.foo` cascades to the entire
build (it is an explicit dependency of every translation) while editing a donor does nothing at
all.

**Worked example, 2026-08-02.** `foofiles/parallel.foo` is 9857 lines containing the *entire* MPI
layer, is a pure `get_from` donor into `SYSTEM`, generates **no `parallel.F90` whatsoever**, and
appears nowhere in `CMakeLists.txt`. An experiment to re-enable Florian's `MPI_BARRIER` (see
above) edited it, ran `cmake --build`, got `rc=0` in under a second, and tested a binary that
still had the old code. Caught only because the build log was 11 lines and the generated
`system.F90` still showed 25 commented barriers. Forcing it needs `touch foofiles/system.foo`.

**This is very likely how the barrier came to be disabled unnoticed in 2021** (`42040312`): a
two-line commit touching `parallel.foo` would have changed no compiled output for whoever made it.

**The fix, in order of preference:**

1. **Depfiles.** `FooToFortran` already knows every file it reads -- it has to, to resolve
   `get_from`. Have it emit a `.d` alongside each `.F90` and pass CMake's `DEPFILE` option on the
   `add_custom_command` (CMake >= 3.20 with Make/Ninja). Precise, automatic, and self-maintaining
   as donors change.
2. **Compute the donor set at configure time** -- scan the `.foo` files for `get_from(` and add
   the referenced donors to that file's `DEPENDS`. No translator change, but the scan must track
   the grammar.
3. **Blunt fallback**: add every `foofiles/*.foo` to every `DEPENDS`. Always correct, but then any
   edit rebuilds everything -- i.e. permanently what `types.foo` does today.

Until this is fixed: **after editing any donor, `touch` a file that is a real dependency** (its
consumer, or `types.foo`) or you will test a stale binary.

### Known-flaky: x-ray-constrained SCF convergence wanders violently (long-standing)

Per Dylan this instability is **well known and has never been diagnosed**. Recorded here now that
there is finally an executing test to observe it with:
`tests/long/nh3_x-ray-constrained-rhf-cluster-charge_cc-pVTZ_restart` (which had never actually
run its SCF -- see milestone 9). With `output= YES` the first lambda shows:

```
Iter  Lambda    GoF2    Energy     Delta   <MO|M0>
   0  0.0120    6.49  -56.2167   ...       1.0000
   1  0.0120   95.01  -55.9091   ...       0.9565
   2  0.0120 1325.99  -52.7894   ...       0.5942
   3  0.0120 3000.65  -26.4060   ...       0.0039   *Damping was off
   4  0.0120  766.26  -51.0366   ...       0.3086   *DIIS starts saving now
```

The energy excursions to **-26.4 Ha** -- some 30 Ha above the true value -- and the overlap with
the reference MOs collapses to 0.004, i.e. the wavefunction is essentially destroyed, before DIIS
drags it back to a sane -56.2023. The final answer is fine; the path is not.

**Why it matters beyond aesthetics.** The blessed reference now encodes that trajectory. A
last-digit difference early on can send another machine down a different path, or fail to recover
at all, so this test is a strong candidate for future cross-platform flakiness. If it goes red on
Linux or in CI, suspect this before suspecting a real regression.

**Leads worth trying**, in rough order of cheapness:
- The excursion coincides exactly with `*Damping was off` at iteration 3. Damping is being
  switched off while the wavefunction is still far from converged; the level-shift is off from
  iteration 0. Check the damping/level-shift switch-off criteria for a constrained SCF -- they are
  probably tuned for an ordinary SCF where GoF2 plays no part in the objective.
- The objective is `E + lambda*GoF2`, and GoF2 reaches 3000 while E is ~-56. The gradient is then
  dominated entirely by the fit term, which is exactly when a plain DIIS extrapolation misbehaves.
- Compare against a run that starts at lambda=0 (not a restart): if the wandering is absent there,
  the restart seed is starting the fit too far from its own optimum.

### PENDING: apply at the next cascade rebuild — types.foo M0s comments

Deliberately **not** applied yet: `types.foo` is included everywhere, so editing it forces a full
recompile, and this is documentation only. Apply it the next time a cascade is needed anyway.
People jump to the type component to find out what a field means, so this belongs there rather
than only in `MOLECULE.SCF`. Exact text (replacing the existing one-line comments):

```foo
     M0s :: OPMATRIX@
     ! The unfitted reference MOs, for overlap_with_M0s.
     ! NOT necessarily the lambda=0 MOs: a RESTART starts at lambda>0 and seeds
     ! these with the MOs at the restart lambda. Set in MOLECULE.SCF.
```

```foo
     overlap_with_M0s :: REAL  DEFAULT(ONE)
     ! Overlap of the current lambda>0 MOs with the reference M0s.
     ! For a RESTART the reference is this job's starting point, not lambda=0,
     ! so the value is not comparable across a restart boundary. See M0s.
```

### Two findings from the MPI_Bcast hunt (2026-08-02)

- **`STR_SIZE` and `BSTR_SIZE` are both 256** (`include/macros.in:57-58`). `BSTR` is meant to be
  the "big string" holding a whole text-file line, while `STR` holds a single token or program
  variable -- but there is currently **no distinction at all**. Consequences: any input line
  longer than 256 characters is silently truncated (CIF looped lines can be long), and the
  distinction the code appears to draw between the two is fiction. Decide whether BSTR should be
  genuinely larger, or whether the two names should be collapsed. Per Dylan, 256 may even be too
  large for STR.

- **Somebody hit the mismatched-`MPI_Bcast` bug before and their workaround is commented out.**
  In the broadcast template itself (`foofiles/parallel.foo`, in `broadcast(buffer,root)`):

  ```foo
  ! Florian here: Added this Barrier to make sure that not BCAST interferes with a
  ! different kind leading to str and Int in asynchronous running MPI process to
  ! screw up communication
  !call MPI_BARRIER(MPI_COMM_WORLD,.mpi_error)
  ```

  "BCAST interferes with a different kind leading to str and Int" is *exactly* the failure
  diagnosed in `docs/TONTO_AND_MPI.md` Finding 6: a 256-character STR broadcast pairing with a 1-integer
  INT broadcast, giving `MPI_ERR_TRUNCATE`. This is independent evidence that the desynchronisation
  **predates all of the milestone 4 work**.

  **Do not simply reinstate the barrier.** MPI matches collectives on a communicator in *program
  order* -- `MPI_Bcast` carries no tag, so order is the only thing pairing them. `MPI_ERR_TRUNCATE`
  therefore proves the ranks issue genuinely different *sequences* of collectives, and a barrier
  cannot repair a different sequence: it is one more collective that must itself match, so it
  generally converts the truncation into a deadlock. It would also force a full synchronisation on
  every single broadcast. Treat the comment as a dated bug report, not as a fix.

  **UPDATE 2026-08-02: the barrier was TESTED and it WORKS. The argument above is wrong.**
  Re-enabling it (verified compiled in: 26 live `MPI_BARRIER` calls in the generated `system.F90`,
  up from 1) makes all three failing CIF tests **pass**. That refutes the ordering argument: if the
  collective *streams* differed in length a barrier would have **deadlocked**, since it adds one
  element to each stream and cannot realign them. Passing means every rank reaches the same
  collectives, and the fault is **timing-sensitive**, not a sequence mismatch.

  Ruled out: non-blocking operations. `ibroadcast` is defined but **never called** anywhere, so
  there is no in-flight message to collide with a later collective.

  **Prime suspect, and it is the milestone-6 mechanism again.** `PARALLEL_BROADCAST` is not an
  unconditional call (`include/macros.in:234`):

  ```c
  #define PARALLEL_BROADCAST0(X,Y)   if (WORK_IS_SHARED0) call broadcast_(tonto,X,Y)
  ```

  Every broadcast is gated on `WORK_IS_SHARED` = `.is_parallel AND .parallel_do_lock==" "`, so a
  broadcast inside a held lock is **silently skipped** -- exactly as the dead reductions were. If
  two ranks ever disagree about the lock state, one performs a collective the other skips, and from
  that point the streams are offset by one -- precisely `MPI_ERR_TRUNCATE` on the next pair.
  A plausible route to disagreement: the translator emits `LOCK_PARALLEL_DO` as the **first
  statement inside** the loop body, so a rank given **zero iterations** by the cyclic distribution
  never takes the lock while ranks with work do. **Not yet verified** -- this is the next thing to
  test, and if it holds then milestone 6 fixes milestone 7 as well.

  **Interim options.** The barrier is a working stopgap but costs a full synchronisation on every
  broadcast. The better fix (Dylan) is to make the failure unrepresentable: have
  `TEXTFILE:read_line_external` issue **one** collective instead of two -- carry the status in a
  reserved leading byte of the string, or broadcast a small `VEC{INT}` and the payload only when a
  line exists. A single collective per call cannot be mispaired, whatever the underlying cause.

  **CONFIRMED 2026-08-02 — the lock hypothesis is right, and the fix is one line.** Changing the
  broadcast gate from the lock to `is_parallel` alone:

  ```c
  -#define PARALLEL_BROADCAST0(X,Y)   if (WORK_IS_SHARED0)   call broadcast_(tonto,X,Y)
  +#define PARALLEL_BROADCAST0(X,Y)   if (tonto%is_parallel) call broadcast_(tonto,X,Y)
  ```

  makes **all three CIF tests pass** at `-n 2`, with `-n 1` unaffected. The new gate was verified
  present in the generated `macros` *before* the run -- after the earlier barrier experiment
  silently tested a stale binary -- and the tree was reverted afterwards.

  **The principle, stated once:** *whether a collective executes must never depend on state that
  can differ between ranks.* `parallel_do_lock` is rank-local -- set by executing a loop body,
  which a rank given zero iterations never does -- so gating a collective on it is unsound.
  `is_parallel` is identical everywhere.

  **Reductions and broadcasts have opposite semantics under that lock**, yet share the gate: a
  reduction combines rank-partitioned partials, so inside a `parallel do` (where the inner work is
  serial per rank) skipping it is *correct*; a broadcast replicates master-only data, so skipping
  it is *catastrophic*. That single conflation explains both the eight dead reductions and this
  desync -- **milestone 7 collapses into milestone 6.**

  Preferred over the barrier, which costs a full synchronisation on every broadcast and masks the
  cause rather than removing it. Dylan's one-collective change to `read_line_external` is still
  worth doing as belt-and-braces, since it makes the failure unrepresentable.

### Not yet fixed — recorded with evidence

- **FIXED (2026-08-02) — mismatched `MPI_Bcast` in the CIF-reading path, `-O2` only.** Root
  cause was the broadcast gate (see the confirmed fix above): `PARALLEL_BROADCAST` was gated on
  `WORK_IS_SHARED`, so a broadcast inside a held parallel-do lock was silently skipped, and ranks
  that disagreed about the lock offset their collective streams. Now gated on `is_parallel`
  alone. All three tested CIF jobs pass at `-n 2`. Original report follows.

  ~~UNDIAGNOSED, HIGH PRIORITY:~~
  On Linux x86_64, four CIF tests (`c9o9h8_read_cif_IT_group_9`,
  `maleate_read_CIF_H_double_bond_{new,old}_BLs`, `urea_lamaGOET_grown_CIF`) abort at >=2 ranks
  with *"An error occurred in MPI_Bcast ... MPI_ERRORS_ARE_FATAL"* in the
  `-O2 -fno-fast-math` build, while the **same test on the same machine passes at `-Ofast`**.
  They do not fail on macOS arm64 at all. A collective mismatch that moves with the optimisation
  level and the platform is undefined behaviour -- an uninitialised value or out-of-bounds read
  feeding a broadcast length, or the branch deciding whether a rank reaches the collective.
  Diagnose with a Linux debug MPI build (`-fcheck=bounds -finit-integer=-999999
  -finit-real=snan`). NOTE the shipped `-Ofast` configuration is the one that HIDES this.


- **FIXED (2026-08-02) — `SYSTEM:initialize` seeds and guarded collectives.** Three faults that
  were load-bearing on each other, which is why the note below said "fix both together or neither":
  (i) the seed routine ran *before* `parallel_initialize`, so `master_rank` and `processor_rank`
  were both still `DEFAULT(0)` and **every rank believed it was master**; (ii) consequently every
  rank seeded from its own `system_clock`, so seeds were never cloned despite the routine's name,
  its comment, and an explicit `! NOTE: cloned for each processor!`; (iii) both `PARALLEL_BROADCAST`
  calls sat *inside* `if (.is_master_processor)`, i.e. collectives only the root would reach.
  Fault (i) masked (iii): with `is_parallel` false the broadcasts were no-ops.

  Fixed together. The fix is *smaller* than the bug, because one collective should never have
  existed: `random_seed(size=n)` reports a property of the compiled RNG and is identical on every
  rank, so it is now simply called everywhere and its broadcast **deleted** rather than moved. Only
  the `seed` broadcast was hoisted out of the guard. Net: one fewer collective than before.

  Serial suite unchanged (50/51, exact 45). **Not yet demonstrated under MPI** -- in a serial build
  `is_master_processor` is TRUE and the broadcast compiles away, so the changed lines are never
  exercised. Testable via R-free selection (`crystal.foo:246`), which is seeded from this: under
  `-n 2` every rank should now make the *same* selection. Still clock-seeded, so still not
  reproducible run-to-run; that part remains open below.

  Original note follows.

- ~~**Latent deadlock in `SYSTEM:initialize`**~~ (`foofiles/system.foo:259-261`).
  `initialize_cloned_random_seed` is called *before* `parallel_initialize`, so `is_parallel` is
  still false and every rank seeds itself from its own `system_clock` — the seeds are **not**
  cloned, contrary to the routine's own comment. Worse, both `PARALLEL_BROADCAST` calls
  (`:275-278`, `:284-305`) sit **inside** `if (.is_master_processor)` guards — a collective in a
  rank-0-only branch. This is harmless today only because `WORK_IS_SHARED` is false that early;
  reordering the two lines (the obvious "fix") deadlocks in `SYSTEM:create`, with `n` undefined
  on non-master ranks at `allocate(seed(n))`. Fix both together or neither.
  Knock-on: R-free reflection selection (`crystal.foo:242`) is seeded from the clock and is
  therefore not reproducible run-to-run, MPI or not.
- **FIXED (2026-08-02) — a rank dying used to hang the job.** The entry used to read
  *"`MPI_ABORT` is commented out"*, which understates it: the error paths were actively calling
  **`MPI_FINALIZE`**, which is **collective**. One rank dying called it alone while its peers kept
  running, so the job hung rather than failing. Now the three error paths (`die` and *both*
  `unknown` overloads) call `MPI_ABORT`, which is unilateral by design and terminates every rank
  in the communicator. Guarded on `.is_parallel`; the normal shutdown in `SYSTEM:finalize` still
  uses `MPI_FINALIZE`, which is correct there.

  Three things had to be true for the old commented line to work and none were, which is probably
  why it was disabled: **`MPI_ABORT` was never imported** in `system.foo` or `parallel.foo`, so
  uncommenting could not compile; the commented call passed **`.mpi_status`**
  (a `VEC{INT}(MPI_STATUS_SIZE)`) where `MPI_ABORT` wants the integer `ierror`; and there were
  **three** affected routines, with the comment present at only one.

  Serial suite unchanged (50/51, exact 45) -- the code is inside `#ifdef MPI` and additionally
  guarded, so a serial build never reaches it.
- **DECISION NEEDED DURING THE fragHAR WORK — `make_LS_mx` writes the same file from every rank.**
  `foofiles/molecule.har.foo:1346` calls `arch.per_rank_write(sf_u)` (renamed from
  `parallel_write`, 2026-08-02), which is deliberately unguarded by `IO_IS_ALLOWED`.

  **Exact context**, traced 2026-08-02:

  ```
  MOLECULE.HAR:make_LS_mx(d_Fa)                          molecule.har.foo:725
    do f = 1,.crystal.n_fragment_atoms                                  :799   <- SERIAL
      do u = 1,n_unique                                                 :875   <- SERIAL
        c = .crystal.unique_frag_atom(u)                                :915
        arch.create(trim(.atom(c).tag)//"-SFs")                        :1344
        arch.per_rank_write(sf_u)                                      :1346
  ```

  Both enclosing loops are serial, so every rank walks every `u`, `c` takes the **same value on
  every rank at the same time**, and the filename is therefore identical across ranks. Two faults
  stack: all ranks write one file concurrently (no locking, no offsets -- corruption, and no
  filesystem prevents it), and `file.foo:134` only *opens* under `IO_IS_ALLOWED` while
  `per_rank_IO_allowed` is never set on this path, so non-master ranks write to the master's
  broadcast unit which they never opened.

  **Update (2026-08-03) — it was never set on *any* path.** `SYSTEM:set_per_rank_IO_allowed`
  assigned `.keyword_echo` instead of `.per_rank_IO_allowed`, so the flag could not be set at
  all, the escape hatch in `SYSTEM:IO_is_allowed` was unreachable dead code, and all ~10 call
  sites in `molecule.scf.foo` were silent no-ops that toggled an unrelated flag. Longstanding —
  `set_parallel_IO_allowed` had the identical body before the rename. Fixed, and the mechanism
  now works: see `docs/RUNNING_HART.md` (milestone H1) and `docs/TONTO_AND_MPI.md` §5. This also means every
  earlier statement in this file of the form "per-rank I/O is enabled here" described an
  intention, not a behaviour.

  **Reachable ONLY via fragHAR:** `fragHAR_refinement` (`:52`) is the sole caller of
  `set_use_disk_SFs(TRUE)`; `LS_fit` (`:562`) then takes the `LS_fit_HAs_disk` branch, which
  calls `make_LS_mx` (`:643`). No job file turns it on -- `gly_ala_fragHAR`'s `stdin:40` has
  `use_disk_SFs=` **commented out** -- but `:52` forces it TRUE regardless, so *every* fragHAR
  run takes the disk path whatever the keyword says. The code's own comment at `:631` reads
  *"NOTE: routine make_LS_mx is very long! Is it working? Rewrite?"*.

  **Serial coverage now exists (2026-08-02), and only via gly_ala.** Two jobs reach it, both
  fragHAR: `tests/long/gly_ala_fragHAR_rhf_STO-3G` (tonto) and the new
  `tests/hart/gly_ala_hart_STO-3G` (hart, label `hart`, therefore **in CI**). Both pass, so the
  serial disk path is exercised and correct; what remains untested is the *parallel* one, which
  is where the `per_rank_write` defect lives.

  **DIAGNOSED 2026-08-02. It was never a regression — the parallelisation was never finished.**
  `git log -L` on the loop header says so in its own commit messages: `b2c53834` *"Working
  monolithic LS fit routine for **future** parallelism"*, `15c2aee3` *"Needs to be tested before
  **parallelisation can begin**"*, then `bfd30b95` *"Progress made to lower memory, parallelised
  HAR routine"* — which introduced `do u = 1,n_unique` as a **plain** `do`. The
  `per_rank_write` was copied into it from `MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk`
  (`molecule.rho.foo:5386`), which does the same thing inside a **`parallel do u`** and is
  therefore correct — distinct `u` → distinct atom → distinct `<tag>-SFs` filename. The
  `parallel` keyword never arrived at the copy.

  **Three sites, one correct and two not:**

  | site | loop | verdict |
  |---|---|---|
  | `molecule.rho.foo:5437` in `get_Hirshfeld_atom_FFs_disk` | **`parallel do u`** (`:5386`) | correct — each rank owns its filenames |
  | `molecule.har.foo:1346` in `make_LS_mx` | plain `do u` (`:875`, extends to `:1408`) | **every rank writes every file, concurrently** |
  | `crystal.foo:4961` in `shift_update_ff(dF)` | plain `do u` (`:4932`) | same, and it is a **read-modify-write** (`arch.read` at `:4946`) of the shared file |

  There is a commented-out fourth copy at `molecule.har.foo:958-962`, inside the same
  `make_LS_mx` loop, which is why the routine appears to compute `sf_u` twice.

  **A second defect, and this one affects the CORRECT site too: there is no barrier between the
  distributed writes and the collective reads.** `PARALLEL:unlock_parallel_do`
  (`parallel.foo`) only clears a rank-local string — a `parallel do` has **no** implicit
  barrier. So after `get_Hirshfeld_atom_FFs_disk` distributes the writes, nothing stops rank 0
  reaching `crystal.foo:4946` / `:5590` / `:6216` — which read `<tag>-SFs` on *every* rank — for
  an atom another rank has not written yet. On a fast shared filesystem this usually works. It
  is a race, not a correctness argument, and it is the likely explanation for "it was working in
  the past". *(The whole scheme also assumes a shared filesystem, which is worth stating
  somewhere it can be found.)*

  **What makes the correct site correct — and exactly where that argument stops.** Atom labels
  are unique, so `<tag>-SFs` is a distinct file per atom. Under `parallel do u` each rank owns
  distinct `u`, hence distinct atoms, hence distinct file names: the ranks write *simultaneously
  but to different files*, which needs no locking and no MPI-IO. That is the whole safety
  argument, and it is sound.

  It does **not** extend to a serial loop. There the collision is not between different atoms,
  it is every rank writing *the same* atom's file at the same instant — unique labels cannot
  help, because all the ranks are on the same label.

  **Separate, and real: disk crowding** (Dylan). Even in the correct case, N ranks each opening,
  writing and closing their own small file at the same moment contends on the filesystem —
  metadata operations especially, and on a network filesystem (NFS, Lustre) far more than on a
  local disk. A gly_ala-sized job writes 20 such files; a protein writes hundreds per LS cycle.
  So the parallel version can be *correct* and still be slower than the serial one. Worth
  measuring before assuming `parallel do` is an optimisation — and a further argument for not
  writing the files at all where they are only going to be recomputed (see the "written and then
  never read" item below).

  **Fix status:**
  - ✅ **`make_LS_mx` → master-only `write`** (the `IO_IS_ALLOWED`-guarded `ARCHIVE:write`),
    *not* `parallel do`. Done 2026-08-02. The loop cannot simply be parallelised: `sf_e`
    accumulates across `u` (`:1310`) and `make_F_predicted_from(sf_e)` is called **inside** the
    loop (`:1355`), so parallelising needs a reduction and a restructure. Since the loop is
    serial every rank computes the same `sf_u`, so the master's file is complete and correct.
    `ARCHIVE:write` is the safe path: `FILE:open_for` guards the `open` and then broadcasts
    `.unit` and `.io_status` so the `DIE_IF` stays collective, and `FILE:write`/`close` are
    guarded with `.record` kept in step on every rank. Serial behaviour is unchanged by
    construction (`IO_IS_ALLOWED` is always true there) and `ctest -L hart` is 3/3.
  - ✅ **`shift_update_ff(dF)` → master-only `write`** (`crystal.foo:4905`, serial `do u` at
    `:4932`). Done 2026-08-02. Worse than `make_LS_mx` because it is a **read-modify-write**:
    the read is `FILE:read`, which is master-only and **broadcasts** the values, so every rank
    already held master's copy and then all of them wrote it back over each other. The read side
    was already master-authoritative; the write side now agrees. `parallel do u` was available
    (the per-atom work is independent, and the commented-out `parallel_read` shows it was the
    intent) but master-only is the conservative choice, matches `make_LS_mx`, and avoids the
    disk-crowding question entirely.
  - ✅ **An explicit barrier** after the distributed write phase, before the first collective
    read. Done 2026-08-02. New `PARALLEL_BARRIER` macro in `include/macros.in`, defined in all
    four blocks of the family and gated on **`is_parallel` alone** — never `WORK_IS_SHARED` —
    for the same reason as `PARALLEL_BROADCAST`: whether a collective executes must not depend
    on rank-local state, or ranks disagree about how many collectives to enter and the job
    hangs. Compiles to nothing in a serial build and is a no-op at one rank.

    **Placement corrected the same day, after an error.** It was first put at the end of
    `MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk`. That routine has **two kinds of caller**:

    | caller | collective? |
    |---|---|
    | `MOLECULE.SCF:make_X_SFs_HAR_disk` | yes — every rank, same count |
    | the `do g` loop in `MOLECULE.RHO:get_Hirshfeld_atom_FFs` | yes |
    | the three per-fragment calls in `MOLECULE.SCF:fragment_SCF_norm` / `_para` | **NO — rank-local** |

    On the rank-local paths rank *r* calls it only for the fragments it owns, and in
    `fragment_SCF_para`'s >2-rank master/worker branch the master `cycle`s and never calls it at
    all — so the barrier would have executed a different number of times per rank and
    **deadlocked**. Exactly the milestone-6 hazard, and a reminder that the rule applies to the
    **call site** as much as to the macro. Moved to the two collective sites:
    `make_X_SFs_HAR_disk` (the non-fragHAR disk path) and the end of `MOLECULE.SCF:fragment_SCF`
    (which every rank reaches, and which covers both fragment branches). Nothing caught this
    because nothing runs parallel fragHAR — see `fragment_SCF_para` below.

  **Only one live `per_rank_write` call site now remains in the tree** — `molecule.rho.foo:5437`,
  inside the `parallel do u`, which is the correct use. Register row 1 is closed.

  **Two naming defects found while adding that coverage** — fold them into the
  `use_disk_SFs`→`use_disk_FFs` rename rather than spending a separate pass:

  - **`hart --disk-sfs` does not do what its name says.** It is wired to
    `set_use_text_SFs` (`run_har.foo:805`), and that setter assigns **both**
    `.use_text_SFs` and `.use_disk_SFs` (`diffraction_data.set.foo:611-618`). So the option
    really means "disk form factors, written as **ascii**", and there is no way to ask `hart`
    for the binary form at all. Either rename the option to `--text-ffs` or give it a value
    (`binary`/`text`/`off`).
  - **The files are named `C1-SFs.unknown`.** Wrong noun (they are atomic **form factors**) and
    an `.unknown` extension, which suggests the archive genre is never set on this path. A
    gly_ala run drops 20 of them, 3.2 MB.

### INVESTIGATED, DO NOT "FIX": `get_Hirshfeld_atom_FFs_for_atom` recomputes rather than reads

The observation is right — `MOLECULE.RHO:get_Hirshfeld_atom_FFs_for_atom` (`molecule.rho.foo`)
contains **no archive access of any kind**. It rebuilds everything: `becke_grid.make_atom_grid`,
`apply_stockholder_atom_weight`, `becke_grid.prune_grid`, `make_ED_grid_r_v2` and the Fourier
sum — apparently the work `get_Hirshfeld_atom_FFs_disk` already did and wrote to `<tag>-SFs`.
Its caller even says so (`molecule.har.foo:713`): *"in iteration 0, these were already
calculated as part of make_X_SFs ... double work!"*.

**But making it read the file would break its only caller.** Checked 2026-08-02:

- The sole caller of `get_Hirshfeld_atom_FFs_for_atom` is
  `MOLECULE.HAR:get_derivative_F_calc_for_atom`, and the sole caller of *that* is
  **`runfiles/run_sf_derivs.foo:748`** — not `tonto`, not `hart`, and nothing on the HAR path,
  which uses `make_LS_mx` / `LS_fit_HAs_memory` instead.
- `run_sf_derivs` is **`EXCLUDE_FROM_ALL`** (`CMakeLists.txt:918`): translated and compilable,
  never built by default, exercised by no test.
- **It never runs a disk-SF pass.** No `use_disk_SFs`, no `HAR_refinement`, no `LS_fit`, no
  `get_Hirshfeld_atom_FFs_disk` anywhere in the file — it does an SCF and goes straight to
  `get_derivative_F_calc_for_atom`. So no `<tag>-SFs` file exists when it runs, and a read
  would `DIE` on a missing file.

So the recomputation is **necessary there**, and the "double work!" comment describes a
situation that no longer arises on that path. There is also **no performance win available
here**: the routine does not execute in any tested configuration, so it is not why
`gly_ala_fragHAR` takes ~60 s. (That claim was made and withdrawn the same day; the real cost
has not been measured — see the test-speed item below, and measure before optimising.)

**It did turn up a real bug, though — FIXED 2026-08-02.** Comparing the expressions showed
`make_LS_mx` was **omitting the site occupancy** from the aspherical form factors. Per Dylan it
should be included: it is 1 for a non-disordered small molecule, "definitely not" for a protein.

| route | expression | occupancy |
|---|---|---|
| in core — `make_Hirshfeld_atom_FFs`, `make_Salvador_atom_FFs`, `make_sph_TFVA_atom_FFs` | `ff = sf * s2 * .atom(c).site_occupancy` | yes |
| disk, fragHAR — `get_Hirshfeld_atom_FFs_disk` | `s2 = .crystal.fragment_atom(c).site_occupancy/sc` | yes |
| recompute — `get_Hirshfeld_atom_FFs_for_atom` | `ff = sf * s2 * .atom(c).site_occupancy` | yes |
| **disk, non-fragHAR — `make_LS_mx`** | `s2 = ONE/sc` | **NO** |

So the same structure refined with `use_disk_SFs` on and off gave different structure factors
whenever an occupancy was not 1. `sf_u` feeds `sf_e` and the derivatives `sf_d` as well as the
`<tag>-SFs` file, so it was the refinement that was wrong, not merely the stored factors.

The correct routine's own comment names the culprit: *"Also include the site occupancy factor
**(Dont forget this for the non-fragHAR routine)**"*. It was forgotten. Fixed by giving
`make_LS_mx` the same `s2`, indexed the same way (`.crystal.fragment_atom(c)`) since the two
write the same files and must agree.

**Latent, and fixed before it was ever exercised** — three independent reasons:
1. Every CIF in `tests/` that does a refinement has **occupancy exactly 1**, so the factor was
   1 and changed nothing. The only partial occupancies in the suite are `0.5` in six
   `tests/cx/` jobs (`mo7c.cif`, `acetoacetanilide.cif`), and **no `cx` job refines** — they
   are Hirshfeld-surface jobs (`generation_method= for_hirshfeld_surface`, `CX_surface=`).
2. `make_LS_mx` is reached by **no test at all**: it needs `use_disk_SFs` on a *non*-fragHAR
   refinement, no job file enables that, and fragHAR routes to `LS_fit_fragHAR_disk` instead.
3. The path **segfaults anyway** — see the next item.

The k-points at least were never in doubt: `CRYSTAL:make_unique_X_SF_k_pts` is a thin wrapper
over the same `reflections.make_unique_SF_k_pts` that `make_LS_mx` calls directly.

### `hart --disk-sfs t` — WORKS as of 2026-08-02 (was: segfault, then infinite loop)

The non-fragHAR disk path had **five** defects. All are fixed; `--disk-sfs t` now runs to
completion and agrees with the in-core path:

| | in core | on disk |
|---|---|---|
| R(F) | 0.037992 | 0.038220 |
| R(F2) | 0.045405 | 0.045540 |
| Rw(F) | 0.029097 | 0.029145 |
| N_r / N_p | 817 / 27 | 817 / 27 |
| GoF | 7.037680 | 7.049176 |
| Scale factor | 0.981084 | 0.981037 |

~5 minutes wall clock against 7.7 s in core (`urea`, release, macOS arm64).

**The five defects, all in `MOLECULE.HAR:make_LS_mx` / `LS_fit_HAs_disk` / `do_LS_refinement`:**

1. **`d_Fa` declared `OUT` instead of `INOUT`** → `intent(out)` deallocates an allocatable on
   entry, so the caller's `create` was dead code and an unallocated array reached
   `d_F_abs_dX`'s non-allocatable dummy. SIGSEGV.
2. **`update_fit_info` never called** → `.fit_finished` was tested but nothing ever set it.
   Infinite loop.
3. **`initialize_fit_data` called per LS iteration** (it sat in `make_LS_mx`, which is inside
   the loop) → re-created `X_fit`/`X_fit0` and reset `chi2_fit0 = huge()` every pass.
4. **`X_fit` re-seeded from the atoms once per ATOM per iteration** — `put_pADP_vector_to`
   sat inside `make_LS_mx`'s `do u` loop. **X_fit is authoritative and the atoms follow it**
   (Dylan), so re-seeding it mid-fit discards the refinement.
5. **`.atom` never refreshed from `.crystal.fragment_atom` inside the loop** — the missing link.
   The chain is `X_fit` → `asymmetric_unit_atom` (`set_pADP_vector_to`) → `fragment_atom`
   (`set_frag_from_asym_pADPs`) → **`.atom`** → `make_LS_mx`, which builds the next design
   matrix from `.atom(c).position`. Without the last step the design matrix was rebuilt from the
   *original* coordinates every iteration. The requirement was written down two lines above the
   gap — *">>>fragment_atom must be the same as molecule.atom"* — but nothing did it. The
   in-core path gets away without it because it computes form factors once before the fit;
   the disk path rebuilds every iteration, so it needs the refresh every iteration.

Defects 3–5 only became visible in that order: fixing 2 exposed 3+4, and fixing 3+4 alone
*froze* the fit (the bogus re-seeding was the only thing making the numbers move) until 5 closed
the chain.

**Structural cause, and the part that was merged.** `make_LS_mx`'s preamble was a hand-inlined
copy of `CRYSTAL:initialize_fit_data`, and `LS_fit_HAs_disk` a hand-rolled copy of
`CRYSTAL:LS_structure_fit`'s loop. The preamble is now **one call** to the original, which also
removed a sixth defect — the copy assigned `refine_3rd/4th_order_for_atom` to local allocatables
(undefined when unallocated; `lldb` caught `EXC_BAD_ACCESS`), where the original passes them as
arguments. `make_LS_mx` now does one job: build the design matrix, which is the only thing that
genuinely differs between the two paths.

**Still open here:**

- **It stops on "chi2 has increased", not convergence**, and the fit oscillates
  (chi2 49.726 → 49.696 → 49.693 → 49.694 → 49.701 → …, 264 fit rows). The answer is right to
  ~0.6% on R(F) but the convergence behaviour is poor. Not investigated.
- **`LS_structure_fit` prunes inside its loop (`crystal.foo:4336`); this loop does not.** Adding
  it segfaults after ~10 iterations — pruning changes `reflections.dim` mid-fit and something
  downstream is still sized for the old count.
- ✅ **Test added: `tests/hart/urea_hart_STO-3G_disk_ffs`** (label `hart`, therefore in CI) —
  the first coverage `make_LS_mx` has ever had. **22 s**, via the new `hart --max-iterations`.
  Capping at 3 both cuts the job from ~5 min and gives a *better* answer (R(F) 0.037995 against
  the in-core 0.037992, versus 0.038220 uncapped), because the uncapped fit oscillates away from
  the optimum before stopping. The reference therefore records a deliberately unconverged fit,
  with `WARNING: refinement stopped: too many iterations.` in it.
- **`--grid-accuracy very_low` SEGFAULTS on this path** (rc=139 after 99 s), which is why the
  test uses `low`. Not investigated — it is the obvious next thing to look at, since a coarse
  grid crashing where a fine one does not suggests an array sized from the wrong grid dimension.

### New: `hart --max-iterations <n>`, and `DIFFRACTION_DATA:set_max_iterations`

Added 2026-08-02, prompted by needing a cheap disk-FF test (Dylan: *"just set max_it very low,
no convergence, but at least some numbers are shown correctly, exercising the code"*).

There was a `max_iterations=` job-file keyword but **no setter** — the reader writes the readonly
field directly from inside the module — so an argv-driven program could not bound a run at all.
`set_max_iterations` now exists; it caps **both** loops, since `too_many_fit_iterations` and
`too_many_ref_iterations` are both measured against `max_iterations`.

Validation is done in `run_har.foo` with `DIE_IF`, not in the setter: the keyword reader's
`max_iterations > min_iterations` check is an `ENSURE` and so compiles away in every optimised
build a user has. The floor is 3, because `min_iterations` defaults to 2.

*(Dylan's other suggestion — skip the initial SCFs by reading stored MOs per fragment — was not
needed here: urea is one molecule and its SCF is seconds out of the 22. It remains the relevant
lever for `gly_ala_fragHAR`, where the fragment SCFs are the cost; see the test-speed item.)*

### `fragment_SCF_para` — how it works, and what is rough about it

Documented in the routine header 2026-08-02 (it previously had a three-line comment identical to
the serial version's). Summary, so this is findable from here:

Two strategies, chosen on rank count, and they are **different algorithms** — which is why
results are not comparable between them and any parallel fragHAR test must pin a rank count.
At exactly 2 ranks it is a plain `parallel do g` (static cyclic). Above 2 it is a dynamic
master/worker pool: fragments sorted **largest-first** by basis-function count
(longest-processing-time-first), then drawn off a shared counter in the master's memory using
**one-sided MPI RMA** — `MPI_Win_lock_exclusive` + `MPI_Get_accumulate`, i.e. an atomic
fetch-and-add. The master hosts the window and **does no science**, which is the scheme's cost
and precisely why 2 ranks take the other branch.

`setup_p_loop` also calls `lock_parallel_do("no")`, which is what makes any `parallel do` inside
a fragment run serially on its owning rank — the "MPI on the outside" rule.

Assignment order does **not** perturb the numbers: each fragment's form factors go to its own
`<tag>-SFs` archive keyed by atom tag, not into a shared accumulator.

**Rough edges, none fixed:**

- `PARALLEL:next_p_loop_index` evaluates `.p_loop_list(.p_loop_index)` **before** the caller's
  exit test — and its own comment insists it be called first — so the final draw indexes one
  past the end. Read and discarded in an optimised build; a `-fcheck=bounds` MPI build should
  abort there. **This is the most likely thing to bite first when parallel fragHAR is tried.**
- `p_loop_list` is cached on `tonto` and only rebuilt when deallocated, so two different fragment
  sets within one run would reuse a stale list.
- `setup_p_loop(w,lb=1,ub=.atom_group.dim)` but the list is created with `.mol.dim`. Equal today
  (one `mol` per group); a mismatch would index out of range.
- **The long-term shape is sub-communicators** (`MPI_Comm_split`): give each fragment a *group*
  of ranks, parallelise within a fragment as well as across them, and stop donating a whole rank
  to scheduling. That would dissolve both branches. Agreed with Dylan as the eventual direction,
  not now.

### `HAR_refinement` does not remake the atom groups between cycles

Raised by Dylan while fixing the above: once `molecule.atom` is remade, the atom groups and
their fragments — capped residues, separated molecules — must be remade for the next refinement
cycle, since the integrals and SCF depend on them.

`fragHAR_refinement` does this: `.update_atom_groups` at `molecule.har.foo:106`, inside its LS
cycle. **`HAR_refinement` does not call it at all.** Its outer cycle does redo
`.promolecule_SCF` and `.scf` at the new geometry, so integrals and the wavefunction are
current — but if a structure ever reaches `HAR_refinement` with `.atom_group` allocated, the
group sub-molecules would keep the pre-shift coordinates. Harmless for the single-fragment case
that path is normally used for, which is presumably why it has never bitten. Worth either
adding the call or asserting that groups are absent on that path.

### Deferred: `fragHAR_refinement` forces disk form factors ON, unconditionally

`molecule.har.foo:52` is `.crystal.xray_data.set_use_disk_SFs(TRUE)` with the comment
*"Use disk SFs for now ..."*, and it is the **sole** caller. So the `use_disk_SFs=` keyword
does nothing for a fragHAR job — `gly_ala_fragHAR`'s `stdin:40` has it commented out and the
disk path runs anyway. Per Dylan this was a deliberate design feature, most likely to bound
memory (the in-core path holds `MAT{CPX}(n_refl, n_pADPs)` for every atom at once).

Worth revisiting once the parallel defects above are fixed:

- Is the memory saving still needed at today's structure sizes and today's machines?
- If it is, it should be a *choice* — honour the keyword, and default it on only when the
  in-core requirement exceeds some threshold.
- "for now" has lasted since `bfd30b95`.

### Deferred: `gly_ala_fragHAR` is slow (~60 s) — speed it up for CI and `long`

Two jobs run it, `tests/long/gly_ala_fragHAR_rhf_STO-3G` (tonto) and
`tests/hart/gly_ala_hart_STO-3G` (hart, and therefore **in CI**). 60 s is the single largest
item in the `hart` label, and Dylan wants it cheaper.

**Measure first.** The obvious suspect — `get_Hirshfeld_atom_FFs_for_atom` recomputing what is
already on disk — was checked and **does not execute on this path at all** (item above). Nothing
has actually been profiled, so the next step is a profile, not a guess. A `gprof`/`perf` run on
the hart job would settle it in one go; the plausible costs are the per-atom Becke grid work in
`make_LS_mx`'s `do u` loop, the fragment SCFs, and the ~10 LS cycles.

Candidates, once there is a measurement to justify one:

1. **Start from stored MOs** (`initial_MOs`) rather than the promolecule guess, checking in the
   converged archives. Cuts the fragment SCF, but the checked-in binaries then have to be kept
   in step with the basis set and the code, which is a maintenance cost the other tests do not
   carry.
2. Drop the residual cube — **already done** for the hart test (`--residual-cube f`), and worth
   doing in the tonto job too.
3. A coarser `--grid-accuracy` for the test, if the refinement statistics survive it; the point
   of the test is that fragHAR runs and reproduces, not that the grid is converged.

Any of these rewrites the reference, so re-bless deliberately and read the result as science.

  **Deliberately NOT fixed now**, because the right fix depends on intent and no test exercises
  the branch, so a wrong choice would be invisible:
  - if that `u` loop is *meant* to be serial -> change to the ordinary `write`, which is
    `IO_IS_ALLOWED`-guarded, so only the master writes. One word, no serial behaviour change.
  - if it is *meant* to be parallel -> make it `parallel do u`, at which point `per_rank_write`
    becomes correct (each rank owns distinct `u`, hence distinct filenames) **and** you get the
    speedup the `! WRITE SF_U TO DISK HERE TO SAVE TIME` comment is reaching for. Bigger: needs
    the reduction question answered for whatever `make_LS_mx` accumulates.

  `MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk` (`molecule.rho.foo:5437`) makes the identical call
  correctly, from inside a `parallel do` -- copy that pattern if going parallel.
- **`fragment_SCF_para` RMA work queue** (`foofiles/parallel.foo:6400`, driven from
  `molecule.scf.foo:5871`): `g = .p_loop_list(.p_loop_index)` is evaluated before the caller's
  exit test, so the terminating fetch indexes past the end of `p_loop_list` on every worker,
  every run. The master also reads its own window buffer outside any access epoch (no
  `MPI_WIN_FLUSH`/`MPI_WIN_SYNC` anywhere) while workers accumulate into it. Additionally the
  algorithm *changes shape* at `n_processors > 2`, so `-n 2` and `-n 4` use different schedulers
  and different rank→fragment maps.
- **PARTLY FIXED (2026-08-02) — QTAIM basin decomposition** (`MOLECULE.PROP:cubes_to_basin_parallel`,
  `foofiles/molecule.prop.foo`). Register entry **verified accurate**, unlike several others.

  **Done — the two cheap parts.** (a) The mid-run `tonto.finalize` is **removed**: it called
  `MPI_FINALIZE` from inside a property calculation, so MPI was shut down for the rest of the job
  and every later MPI call was invalid. That did not fail locally, it poisoned everything
  downstream. Finalisation belongs to the program. (b) Two `DIE_IF` guards added, because the
  decomposition **cannot work below 2 ranks**: at `nprocs==1`, `mx = nx` and the master does
  `ED(2:mx+2,...) = grid(1:mx+1,...)`, reading `grid` one element past its first extent (it is
  `MAT3{REAL}` with first extent `nx`), then unconditionally `sendrecv`s to `processor_rank+1`
  — rank 1, which does not exist. A second guard rejects `nprocs > nx`, where slabs have zero
  width. Refusing beats corrupting.

  Note this is only reachable in an **MPI build** — the call site picks `cubes_to_basin` under
  `#else` of `#ifdef MPI` — but `mpirun -n 1` on an MPI build *does* reach it, which is precisely
  the broken case and a very ordinary way to run.

  **Still open — the decomposition itself.** It is a hand-rolled 1-D chain: master sends only
  "hi", middle ranks both, the last only "lo". Two of the three allocation branches are
  **identical** (`ED.create(px+2,...)` in both), so the `else if` earns nothing at setup. The
  proper fix is `MPI_Cart_create` / `MPI_Cart_shift`, which returns `MPI_PROC_NULL` for
  off-the-end neighbours — making each `sendrecv` a no-op at the ends and letting the `nprocs==1`
  case fall out for free, rather than being guarded against. That would also let the guards above
  be removed. Narrow (QTAIM only) but genuinely broken rather than merely fragile.
- **FIXED (2026-08-01). `DWGN_lamaGOET_NBO_file_47` aborted at every rank count above 1.**
  Reproduced at `-n 2` and `-n 4`, at both `-Ofast` and `-O2 -fno-fast-math`:
  *"Fortran runtime error: Unit number is negative and unit was not already opened with
  OPEN(NEWUNIT=...)"*. Root cause is `foofiles/file.foo:134-146` — only the master executes the
  `open(... newunit=.unit ...)` (it is wrapped in `if (IO_IS_ALLOWED)`), and the following
  `PARALLEL_BROADCAST(.unit,tonto.master_processor)` then hands the master's negative `newunit`
  value to every rank. Non-master ranks hold a unit they never opened, so any *unguarded* I/O on
  it fails. The job dies during the `scfdata` block just after `read_g09_fchk_file`, having
  already written its archives. A debug MPI build pinned it to `MOLECULE.PUT:put_NBO_file_47`,
  which calls `stdout.redirect(...)` then makes **thirteen raw Fortran `write(stdout.unit,...)`
  calls** that bypass the TEXTFILE API and its guarding. Fixed by guarding them; it is write-only
  output so no broadcast is needed. Passes at -n 2 and -n 4 bit-exact, unchanged serially.

  **The failure mode is the real lesson.** It crashed loudly only because a *redirected* TEXTFILE
  holds a negative `newunit`. An unguarded raw write to a **non-redirected** stdout uses
  `TEXTFILE_STD_OUT_UNIT` (6), a valid preconnected unit on every rank, and would **silently
  interleave** output instead of crashing. So the sentinel idea is weaker than it first looks:
  the negative `newunit` already acts as one, and it cannot help the unit-6 case at all (nor can
  a static sentinel, since `fragment_SCF_para` deliberately enables `per_rank_IO_allowed`).
  Remaining unguarded raw-I/O sites: `plot_grid.foo:2280` (`read(textfile.unit,*)` on every rank,
  and the result needs broadcasting too) and `archive.foo:2687/2712/2763` (VAPOR/stream/VTK
  writers; each rank opens the *same filename* with its own `newunit`, so silent corruption
  rather than a crash). The durable fix is a **translator lint** for `write(`/`read(` on any
  `*.unit` outside `file.foo`/`textfile.foo`/`buffer.foo` -- static, cheap, and it would have
  found every one of these without running anything. Added to milestone 6. See `docs/TONTO_AND_MPI.md`.
- **`parallel_sum` clobbers `val` even when the optional `sum` is supplied**
  (`foofiles/parallel.foo:458`): an unconditional `val = tmp` after the `if (present(sum))`
  branch, violating the "give me the sum, leave `val` alone" contract.
- **Build-system inconsistencies.** `CMakeLists.txt` assigns `CMAKE_Fortran_FLAGS` from
  `MPI_Fortran_FLAGS`, which modern `FindMPI` never sets — it evaluates to the empty string *and*
  wipes any user-supplied flags (survivable only because `SetFortranFlags` rebuilds them). The
  executables link `${MPI_LIBRARIES}` (the deprecated **C** libraries) while the `tonto` library
  links `${MPI_Fortran_LIBRARIES}`. The MSMPI branch references `${msmpi-linux-home}`, which is
  **defined nowhere**, and sets `CMAKE_C_FLAGS` from `${CMAKE_CXX_FLAGS}` (copy-paste).
- **Dead MPI runfiles.** `run_mpi_matmul.foo` uses `myid` uninitialised and deadlocks at `-n 1`;
  `run_mpi_pi_io.foo` is referenced nowhere in `CMakeLists.txt`. `run_mpi_test` and
  `run_mpi_test_complete` are built and installed but **assert nothing** — `test_parallel` prints
  `"MPI failed to …"` and still exits 0.
- **`#ifdef MPI` strips `PURE`/`ELEMENTAL` from the entire codebase** (`include/macros.in:256`).
  Necessary today — MPI calls are impure and some `PURE` routines contain `PARALLEL_SUM` (e.g.
  `shell1quartet.foo:898`) — but it is a sledgehammer: it costs the optimiser common-subexpression
  elimination, loop-invariant hoisting and `elemental` vectorisation across *all* code, and it is
  a large part of why an MPI build differs numerically from serial even at one rank. Only
  routines that transitively reach a `PARALLEL_*` macro need it. The translator's phase-B call
  graph (`--call-graph-report`) can compute that set exactly.

---

# Build system and toolchain

## Future task: split `types.foo` into several modules (parallel compilation)

**Goal (Dylan):** `types.F90` is the slowest single compile in the build and it is a
**serial bottleneck** — everything `use`s it, so nothing else can start until it finishes.
Split `types.foo` into several independent modules so they compile in parallel under `-j`.

**Why it is slow — measured (2026-07-28, M2 Pro / Tahoe, gfortran-14):**

| Stage | Time |
|---|---|
| parse + `.mod` generation (`-fsyntax-only`) | 0.01 s |
| `-O0` (codegen only) | 29 s |
| `-O1` | 45 s |
| full release flags (`-Ofast … -O2`) | 839 s (~14 min) |

The front end is *instantaneous*, so the ~90 derived-type definitions themselves cost nothing.
The cost is **codegen**: those types have ~585 allocatable/pointer components, and gfortran
auto-generates a deep-copy helper per type (`__copy_types_module_<TYPE>`) — 167 text symbols and
**8.9 MB of `__TEXT`** from 5.7 k lines containing *zero* user-written procedures. Optimising
that generated boilerplate is what costs the four minutes.

**Interim fix already applied:** `types.F90` is compiled at `-O1`
(`set_source_files_properties` in `CMakeLists.txt`), 839 s → 45 s (~18x), no runtime cost worth
measuring (the helpers are memcpy-shaped). The serial-bottleneck problem remains.

**Note — F2008 `submodule` does NOT help here** (cf. the submodule task above): type
definitions are part of a module's *interface* and cannot live in a submodule, and the
`__copy_*` helpers are generated where the types are defined. Splitting into several **real
modules** is what parallelises; submodules only avoid recompilation cascades.

**Care needed:** the split must respect the derived-type dependency order (types with
components of other derived types), and every `use TYPES_MODULE` site plus the translator's
`.use`-file generation must follow. Check whether the translator can emit the split
automatically from one `types.foo` rather than requiring the source be broken up by hand.

## libxc as the DFT functional engine — see `docs/DFT_STANDARDISATION.md`

Superseded by milestone 10. The decision, the verified API facts, the compiler
constraint, and the seven-point list of what a real implementation must cover are
all in **`docs/DFT_STANDARDISATION.md` §8**, alongside the interface analysis
(§7) that determines how much work it is.

The one thing worth repeating here, because it is the most likely thing for a
reimplementation to get wrong: **Tonto's `E` is the functional divided by the
density** — energy per particle, exactly what libxc's `_exc` entry points return.
Read `new_r_LDA_x_energy_density`, which computes `-(3/4)(3/pi)^(1/3) rho^(1/3)`.
Assuming `E` is an energy density is wrong by a factor of rho.

`archive/libxc` (Peter Spackman, 2017) is a prototype and **must not be merged**;
it is assessed in `docs/TONTO_REPOSITORY_BRANCHES.md`.

## Milestone 6, partial: the suppressed-reduction abort and the parallel lint (2026-08-02)

Two of milestone 6's four parts are done. Both are cheap, independent of the grammar work, and
together they would have caught **every** MPI defect found on 2026-08-02, including one I
introduced myself.

**1. Abort on a suppressed reduction.** The reduction macros now call
`SYSTEM:reduction_is_allowed(name)` (from `parallel.foo`) instead of testing `WORK_IS_SHARED`
inline. It returns the same value, and under `USE_PRECONDITIONS` `ENSURE`s that a FALSE result
is not caused by a parallel-do lock:

```foo
res = .work_is_shared
ENSURE(res OR NOT .is_parallel, trim(name)//" is inside a `parallel do`: the reduction is
                                 SKIPPED there, so it is dead code ...")
```

A function rather than an `if/then/else` in the macro deliberately: the macro keeps its
single-statement `if (...) call ...` shape, so an existing `if (cond) PARALLEL_SUM(x)` call site
cannot break. Debug-only by construction — `ENSURE` compiles to a comment without
`USE_PRECONDITIONS`, so optimised builds behave exactly as before and pay nothing. `pure` is
safe for the same reason: wherever `ENSURE` is live, `PURE` has been `#undef`'d.

**NOT YET EXERCISED.** In a non-MPI build the reduction macros expand to nothing, so
`reduction_is_allowed` is compiled but never called. It is inert in every serial run, which
means the serial suites can neither break it nor validate it. **The first debug MPI build is
what tests it** — and if it fires, that is a finding, not a bug in the check.

**2. `scripts/check_parallel_lint.py`**, registered as `ctest` test `parallel_lint` (label
`short`, so in CI) and as an invariant line in `make report`. Two checks:

- **no collective inside a `parallel do` body** — reductions are silently skipped there;
  broadcasts and barriers execute but a different number of times per rank and hang.
- **no raw `write`/`read` on a `.unit`** outside `file.foo`/`textfile.foo`/`buffer.foo`.

It is **guard-aware**: a site inside `if (IO_IS_ALLOWED)` or `if (... is_master_processor ...)`,
block or one-liner, is not reported. Without that it flagged 48 already-correct sites, and a
lint that cries wolf gets ignored. Self-tested against a synthetic file containing one violation
of each kind plus two correctly-guarded writes: it reports exactly the two violations.

Current state: **clean** — 184 files, 74 `parallel do` loops, no interior collectives, no
unguarded raw `.unit` I/O.

The two checks are complementary and neither subsumes the other. The lint is static and sees
only what is lexically inside a loop; the abort is dynamic and catches a reduction reached
through a **call** from inside a parallel do, which no source scan can find.

**Still open in milestone 6:** `parallel do … reduce(x)` (grammar + translator), and
depth-counting the parallel-do lock so a recursive inner return cannot release an outer lock
(restoring the `ENSURE` at `parallel.foo:308`).

## PRIORITY: `hart` has never run under MPI — MPI_ERR_TRUNCATE at 2 ranks

Found 2026-08-03 while attempting milestone 5's last step (parallel fragHAR at 2 ranks). **Not a
regression** — no argv-driven program has ever been run under MPI.

Evidence, on a local gfortran-14 Open MPI build (`~/opt/openmpi-gf14`, `-Ofast`, `-DMPI=1`,
`hart` confirmed linked against `libmpi`):

| what | result |
|---|---|
| `run_mpi_pi` at 1, 2, 4, 8 ranks | OK — all agree with π and with each other |
| `tonto` (stdin-driven) at 2 ranks | OK — exit 0, no truncation |
| `hart --version` / `--help` / no args at 2 ranks | OK — no truncation |
| `hart … <cif>` at 2 ranks | **`MPI_ERR_TRUNCATE` in `MPI_Bcast`**, reported by rank 1 |
| `hart --basis NOSUCH <cif>` at 2 ranks | same — so it fails **before** any CIF is read |

So the reductions are sound and `tonto` is fine; the **argv-driven path** breaks, as soon as a
`<cif-file>` argument is supplied. `MPI_ERR_TRUNCATE` on a broadcast means the ranks are at
*different* broadcasts — the collective streams have gone out of step.

**The bisect brackets it.** `--help`, `--version` and no-args all `stop` before
`run_har.foo:203`; supplying an argument is what first reaches the `std_err`
close / destroy / re-create / `set_name` / `open_for` sequence there.

**A concrete defect of the right shape in that path.** `FILE:close_and_delete` sets
`.io_status` **inside** an `IO_IS_ALLOWED` guard and then tests it on every rank with no
broadcast in between — while its sibling `FILE:close` does broadcast it:

| routine | guarded operation | broadcast afterwards? |
|---|---|---|
| `FILE:close` | `inquire` | **yes** — `PARALLEL_BROADCAST(.io_status,…)` |
| `FILE:close_and_delete` | `close(status="delete")` | **no**, then `DIE_IF(.io_status/=0,…)` |

On a non-master rank `.io_status` keeps whatever it held, so that `DIE_IF` is decided on
rank-local state. This is the milestone-6 rule one level down: not a collective gated on
rank-local state, but a **branch** taken on it, inside a routine whose branches contain
collectives.

**LOCALISED 2026-08-03 to a single call.** Method — *bracketing by phase*, which is bisection
with printed landmarks rather than a debugger, and is worth reusing for any collective desync:

1. A temporary trace in `PARALLEL:broadcast` (the template, so all 25 type instantiations get
   it) printing the element count of every `MPI_BCAST` to stderr.
2. Run under `mpirun --tag-output -n 2`, split the stream by rank, and diff. `MPI_ERR_TRUNCATE`
   **is** a length mismatch, so the first index where the two length sequences differ is the
   divergence.
3. Then temporary `PHASE` markers at known points in `run_har.foo`, each printing the running
   broadcast count, to find which interval contains that index.

Result. The two ranks are **identical for the first 126 broadcasts** and diverge inside one call:

| marker | rank 0 | rank 1 |
|---|---|---|
| after stdin created | 5 | 5 |
| after `process_options` | 30 | 30 |
| after the `std_err` dance | 36 | 36 |
| before the option loop | 36 | 36 |
| before the banner | 106 | 106 |
| **immediately before the `put`** | **126** | **126** |
| died | 163 (+37) | 129 (+3) |

So the desync is **inside `stdout.put(.command_optarg,by_column=TRUE,left=TRUE,width=STR_SIZE-8)`**
in `COMMAND_LINE:put_command_optarg` — i.e. *echoing the command line*. Master performs **37**
broadcasts there and rank 1 performs **3**. At the divergent index master sends a `STR` (256)
while rank 1 is at a scalar (1), which is the truncation.

## `hart` NOW WORKS UNDER MPI (2026-08-03) — and reproduces serial exactly

After the `TEXTFILE:flush` fix below, plus removing an over-strict runtime check (next item),
`hart` runs to completion at 2 ranks and agrees with a serial run **digit for digit** — and from
*different builds*, release-serial against debug-MPI, which makes the agreement stronger:

| | serial (release) | MPI, 2 ranks (debug) |
|---|---|---|
| R(F) | 0.037995 | 0.037995 |
| N_r / N_p | 817 / 27 | 817 / 27 |
| GoF^2 | 49.536697 | 49.536697 |
| GoF | 7.038231 | 7.038231 |

**fragHAR at 2 ranks still fails**, but far later — 884 lines in, past the CIF, the atom groups
and the ANO data, at *"Making F_pred ..."* — with a **second, distinct** `MPI_ERR_TRUNCATE` in
the fragment path.

**CONFIRMED 2026-08-03 — it is output inside `fragment_SCF_para`'s `parallel do g`.** Traced with
phase markers and a per-broadcast counter. The ranks are in *perfect lockstep* right up to the
loop, and diverge the instant they take different fragments:

| | rank 0 | rank 1 |
|---|---|---|
| entering `fragment_SCF` | 127,197 | 127,197 |
| before `parallel do g` | 141,907 | 141,907 |
| **fragment taken** | **g=1** | **g=2** |
| died | 146,858 | 146,815 |

So each rank runs a whole SCF on *its own* fragment, and that work performs collectives — chiefly
`BUFFER:put_str`, which broadcasts `.string` (256 bytes) and `.item_end` on **every token
written**, outside the `IO_IS_ALLOWED` guard. Different fragments produce different amounts of
output, hence different numbers of collectives, hence `MPI_ERR_TRUNCATE`.

**PROPOSED FIX (Dylan, 2026-08-03): make `per_rank_IO_allowed` also switch off the broadcasts.**

The switch already exists and is already used in the right places — `fragment_SCF_para` wraps its
disk-FF writes in `set_per_rank_IO_allowed(TRUE/FALSE)`. And `SYSTEM:IO_is_allowed`
(`system.foo:6734`, which overrides the simpler `PARALLEL` version and is the one the
`IO_IS_ALLOWED` macro calls) honours it:

```foo
if (.per_rank_IO_allowed) then
   res = TRUE                                        ! every rank may do I/O
else
   res = .is_master_processor OR (NOT .is_parallel)
end
```

But it only removes the master-only **guard**. The **broadcasts** stay unconditional — the same
half-measure as `FILE:per_rank_write`, which also removes the guard and not the synchronisation.
That is why per-rank file I/O has never actually worked.

The fix is to give the flag its full meaning: when it is set, the ranks are *deliberately* doing
different I/O, so the bookkeeping broadcasts must be skipped.

```
#define PARALLEL_BROADCAST_IO(X,Y)   if (tonto%is_parallel .AND. .NOT. tonto%per_rank_IO_allowed) \
                                        call broadcast_(tonto,X,Y)
```

Apply it **only** to I/O bookkeeping — `.unit`, `.io_status`, `.record`, `exists`/`is_open`, and
`BUFFER:put_str`'s buffer and cursor — and leave genuine data broadcasts (`FILE:read`'s payload,
`plot_grid`'s `pt`, the random seed) alone.

**Why it is safe**, which is the part worth checking: with the broadcasts disabled inside the
region there is nothing collective left to mismatch, so a rank that takes zero iterations simply
does nothing and no peer waits on it. The flag itself must be set and cleared *outside* the
parallel loop so every rank agrees on the mode.

**Refinement (Dylan): one flag conflates two permissions.** `per_rank_IO_allowed` currently means
both "write my own files" *and* "write to the shared log" — so enabling it for a fragment loop
also lets every rank interleave `stdout`. They should be separate:

| flag | meaning | broadcasts | who writes |
|---|---|---|---|
| *(default)* | shared file, master only | yes | master |
| `per_rank_self_IO_allowed` | each rank writes its **own** files | **no** | every rank |
| `per_rank_shared_IO_allowed` | every rank may write the **shared** log | yes | every rank (interleaved, deliberate) |

**The deeper form is per-FILE, not global**: a `FILE`/`TEXTFILE` is intrinsically either shared
(master writes, state broadcast) or per-rank (each rank owns a copy, nothing broadcast). Marking
the file removes the need for any global mode and for setting/clearing it around regions — the
archive is per-rank, `stdout` is shared, and each behaves correctly wherever it is used. Two
global flags are a cheaper approximation and a reasonable first step.

**No translator change**, unlike every other option considered. It also deletes most of the
~147,000 broadcasts a fragHAR run performs, so it is a scaling fix as much as a correctness one.

**This is structural, not a bug at a line.** Coarse-grained parallelism over fragments cannot
work while writing output is a collective operation. Compile-time purity (see the `PURE` design
above) cannot help either: a fragment SCF legitimately writes archives and output. The two
options remain (1) make output non-collective inside a parallel region, or (2) suppress or
serialise it there.

**Note the scale while deciding:** 141,907 broadcasts occur *before the fragment loop even
starts* — a HAR job spends ~142k collectives producing text. Option (1) would remove nearly all
of them, so it is a performance fix as much as a correctness one. That is the known register-row territory (the per-fragment,
**rank-local** calls in `fragment_SCF_norm`/`_para`), and it is a separate hunt. The method that
found the first one is recorded below and applies unchanged.

## Root cause behind several of these: `MOLECULE` contains `MOLECULE`s (Dylan, 2026-08-03)

The fragment machinery is a **module/problem mapping** problem, and most of the parallel defects
above are downstream of it.

Today a `MOLECULE` holds a `CRYSTAL` *and* holds `.mol(g)` — a set of `MOLECULE`s. So
`MOLECULE.SCF:fragment_scf` must call back into `MOLECULE.SCF:scf`, and that is **why** the
12-node cycle exists:

```
fragment_scf_para -> usual_scf -> initialize_scf -> get_initial_guess -> ...
   -> do_atom_group_scf -> scf -> fragment_scf -> fragment_scf_para
```

It is not an accident of the initial-guess path; it is forced by a type containing instances of
itself.

**Put the fragments where the physics puts them** — a `CRYSTAL` containing several `MOLECULE`s,
with `fragment_SCF` as a `CRYSTAL` method. Then `CRYSTAL:fragment_SCF` calls `MOLECULE:scf`,
which calls nothing back, and:

- **the cycle disappears**, and with it the recursion defect in the parallel-do lock;
- the `subfrag_SCF` clone becomes unnecessary;
- depth counting loses its only *live* justification (the cycle was the sole `parallel do`
  routine inside any cycle — 26 such routines, 8 cycles);
- the parallel decision moves to the right place. Distributing work over fragments is a property
  of the **container**. Today it is made inside a `MOLECULE` method that is simultaneously "do an
  SCF on me" and "do SCFs on my children" — which is why `fragment_SCF_para` carries
  `per_rank_IO_allowed` toggling, per-fragment archives and a work scheduler: container concerns
  in a molecule's clothes.

Large refactor, and the right one. Worth noting as the destination even if the interim fixes
above are taken first, so they are understood as interim.

## Design (2026-08-03): let MPI keep `PURE`, so the compiler forbids I/O in parallel regions

Dylan's proposal, and it is a good one: if output inside a parallel region desynchronises the
ranks, then routines reachable from one should be `pure` — and the *compiler* should enforce it,
rather than the programmer remembering.

Today `include/macros.in` `#undef`s `PURE` under **`MPI`**, so an MPI build has no purity checking
at all. The cost of stopping that, measured:

| | count |
|---|---|
| routines declared `PURE`/`ELEMENTAL` | **4122** |
| routines containing parallel constructs | 93 |
| **both — would have to drop `PURE`** | **64** |

So 64 routines lose a `PURE` they were never entitled to (they contain `parallel do` or a
reduction, which mutate `tonto`), and **4058 keep compiler-enforced purity under MPI**. The first
entry in that list is `buffer.foo:put_str` — declared `PURE` *and* broadcasting, which is exactly
the routine desynchronising fragHAR. It is not pure and the compiler would say so.

**Debuggability is not lost**, because there are *two independent* `#undef PURE` in `macros.in` —
one in the `MPI` block, one in the `USE_PRECONDITIONS` block. Removing only the MPI one gives:

| build | `PURE` | effect |
|---|---|---|
| MPI release | **on** | compiler forbids I/O in 4058 routines |
| MPI debug | off (via `USE_PRECONDITIONS`) | probes still allowed |

**Limit, stated plainly: this cannot fix fragHAR.** `fragment_SCF_para`'s `parallel do g` runs a
whole SCF per fragment — archives, output, allocation — so those routines can never be pure.
Compile-time purity covers **fine-grained** parallelism (the `shell1quartet` integral layer) and
cannot cover **coarse-grained** parallelism over fragments. For that there are two honest
options, and one must be chosen:

1. make output **non-collective** inside a parallel region — i.e. stop broadcasting the buffer
   when the ranks are doing different work; or
2. **suppress or serialise** output there.

Option 1 is the deeper fix and would also delete most of the ~147k broadcasts a fragHAR run
currently performs (see "output costs two collectives per token").

## Design (2026-08-03): fix the parallel-do lock — depth AND name, and lock outside the loop

Agreed with Dylan while diagnosing the fragHAR desync. Three separate defects in one mechanism.

**Today's lock is a single string, "first wins":**

```foo
lock_parallel_do(name)
   if (.parallel_do_lock==name) return                    ! already ours
   if (.parallel_do_lock==" ") .parallel_do_lock = name   ! take only if free
unlock_parallel_do(name)
   if (.parallel_do_lock==name) .parallel_do_lock = " "   ! release only if it matches
```

**1. Nesting is fine; RECURSION is not.** An inner `parallel do` cannot steal the lock and its
unlock cannot release the outer one, so ordinary nesting — which today proved is *pervasive*,
17 loops in `shell1quartet.foo` alone — is safe. But if a routine's own `parallel do` is
re-entered recursively, the inner `lock("A")` is a no-op and the inner `unlock("A")` **matches
and clears the lock while the outer loop is still running**. The code knows: there is a disabled
`ENSURE(name/=.parallel_do_lock,"recursive parallel routines not allowed")` at `parallel.foo:310`
and the comment says *"It is currently an error if the routine is recursive."*

**2. It assumes routine names are unique** — its own comment warns so, and overloads are the
obvious way that breaks (Dylan). *Currently it holds*: checked across the whole generated tree,
every `LOCK_PARALLEL_DO` name is distinct, because the translator suffixes overloads
(`CRYSTAL:make_unique_sf_0`, `_1`). But correctness resting on a naming convention fails silently
the day the convention shifts.

**3. `LOCK_PARALLEL_DO` is emitted INSIDE the loop body** — and the reason that matters is *not*
the one first recorded here. **Correction (2026-08-03):** the original claim was that a rank
given zero iterations never locks, so the ranks disagree about being in a parallel region. That
is harmless on its own: the lock is only consulted by code *inside* the body, which such a rank
never executes, and the trailing `UNLOCK` no-ops on a clear lock.

The real point is that **`work_is_shared` is doing two different jobs**:

| question | asked by | when | must answer |
|---|---|---|---|
| "should **this** loop distribute?" | `PARALLEL_DO_START` / `_STRIDE` | *before* entry | yes |
| "am I **inside** a parallel region?" | inner loops, reductions, I/O | *during* the body | yes |

They are the *opposite* states of one flag. Locking before the loop answers the second correctly
and the first **wrongly** — and silently: `parallel_do_start` returns `f` and `parallel_do_stride`
returns `s`, so every rank runs the full range. The loop is still correct, merely **not
distributed**. A pure performance regression with no error, which is the worst kind. *Verified by
reading both routines; this is why the naive "move the emission" fix must not be applied.*

The two questions are separable **in time** — they are asked at different moments — and collide
only because Fortran evaluates `do` bounds after the point where one would want to lock. Hoisting
resolves it:

```fortran
lo = PARALLEL_DO_START(1,1)   ! asked while depth == 0 -> distributes
hi = n
st = PARALLEL_DO_STRIDE(1)
LOCK_PARALLEL_DO("...")       ! now depth == 1
do i = lo, hi, st             ! answers correctly throughout the body
```

so hoisting is not a workaround for a Fortran limitation; it makes explicit a sequencing the
current lowering leaves implicit. It needs the translator to emit and declare temporaries.

**Only do this if lock-gated behaviour is wanted** (e.g. making output non-collective inside a
parallel region by testing the lock). On its own it buys nothing, and done naively it costs the
parallelism.

**FINDING 2026-08-03 — depth counting is NOT a small self-contained change, and the defect it
fixes is not live.** Two facts, both checked:

1. **`LOCK_PARALLEL_DO` is the first statement *inside* the loop body, so it executes once per
   iteration.** A depth counter would increment N times and decrement once, leaving the depth
   stuck at N-1. The present name-based scheme survives only because it is *idempotent*
   (`if (.parallel_do_lock==name) return`). So **depth counting requires lock-before-loop**,
   which requires the hoisted bounds below — it is the same piece of work, not a separate one.
2. ~~No recursive routine contains a `parallel do`.~~ **WRONG — corrected 2026-08-03 (Dylan).**
   That check looked only for the `recursive` *attribute*, i.e. **direct** recursion. The SCF
   re-enters itself **indirectly**, through the initial guess. Confirmed from the translator's own
   `--call-graph-report`: `MOLECULE_SCF_MODULE:fragment_scf_para` sits inside a **12-node call
   cycle**:

   ```
   fragment_scf_para -> usual_scf -> initialize_scf -> get_initial_guess
     -> get_initial_density_mx -> make_atom_group_guess_mos -> atom_group_scf
     -> do_atom_group_scf -> scf -> fragment_scf -> fragment_scf_para
   ```

   It is the **only** `parallel do` routine in any cycle (26 such routines, 8 cycles in the whole
   graph) — and it is precisely the routine at the centre of the fragHAR failure. With the
   name-based lock the outer `fragment_scf_para` takes the lock, the inner one's `lock` is a
   same-name no-op, and the inner one's **`UNLOCK` clears the lock while the outer loop is still
   running**. The outer body then believes it is not in a parallel region: inner loops begin
   distributing and reductions begin firing.

   *Caveat:* this is static reachability. Whether the cycle is taken depends on the initial-guess
   setting (`make_atom_group_guess_mos` is not on the promolecule path). It should be confirmed
   dynamically — a counter in `lock_parallel_do` would settle it — but it is live enough to
   matter, and it is in the failing path.

**Dylan's `get_from` answer to the duplication objection.** A clone (`subfrag_SCF`) breaks the
name collision and so fixes the cycle with no translator work — and my objection that clones
drift (as `make_LS_mx` did) does **not** apply if the clone is generated:
`subfrag_SCF :: get_from(MOLECULE.SCF:fragment_SCF)` produces it from a single definition, so it
cannot diverge, and the translator tags its lock with the new name automatically. The inner call
site must then be pointed at the clone. It is a point fix rather than a mechanism fix — another
cycle would need another clone — but it is honest and cheap. **Measure first**: confirm the cycle
is actually taken at runtime (a counter in `lock_parallel_do`) before cloning anything, since
`make_atom_group_guess_mos` is not on the promolecule path.

**So the priority is raised, but the cost is unchanged**: depth counting still cannot be done
while `LOCK_PARALLEL_DO` sits inside the loop body (fact 1), so it remains coupled to the
translator change. The per-rank-I/O work is still the smaller, more certain win.

**Proposed fix, all three at once:**

- **Depth counter for correctness, name for diagnostics — keep both.**
  `lock`: `depth = depth+1`, and record the name *only* on the 0→1 transition.
  `unlock`: `depth = depth-1`, and clear the name on 1→0.
  `WORK_IS_SHARED` = `is_parallel AND depth==0`.
  Recursion becomes correct; the name-uniqueness assumption **disappears**, because unlock no
  longer matches on name; and the disabled `ENSURE` can be *deleted* rather than restored, since
  recursion becomes legal.
- **Keeping the name is not cosmetic.** It is the outer routine's identity in every diagnostic —
  it is the only reason `MOLECULE.FOCK:make_u_JK_engine` was identified on 2026-08-03, from a
  message that would otherwise have said "a reduction, somewhere".
- **Emit `LOCK` before the loop and `UNLOCK` after** (translator change), so the lock state is
  identical on every rank regardless of iteration count. This is the most valuable of the three
  and the only one that needs `FooToFortran`.

## A related trap found the same way: `--fos 0`

`hart --fos 0` (used by `tests/hart/gly_ala_hart_STO-3G` to disable F/sigma pruning) called
`DIFFRACTION_DATA:set_F_sigma_cutoff(0)`. That routine `ENSURE`s `val>ZERO` and says so in its
own comment — *"make sure zero is not entered ... just leave it off!"* — while the field defaults
to `-TWO` and every consumer tests `> ZERO`. So passing 0 worked **only in release**, where the
`ENSURE` compiles away, and aborted in any debug build. Fixed in `run_har.foo`: `--fos 0` now
means *do not call the setter*, which is what the library asks for.

Worth noting as a pattern: this is the second defect today that was invisible in release and
obvious in debug. A debug MPI build is cheap insurance.

**Superseded — `hart` at 2 ranks dies mid-SCF** (exit 1, an `MPI_ABORT` from a `DIE`) after
getting through options, CIF, Becke grid and into the first SCF. That is a *different* bug and a
far better place to be. Diagnosis is hampered by the next item.

### FIXED (2026-08-03): `DIE` messages were LOST under MPI — and fixing it found two more bugs

`SYSTEM:die` wrote its message only inside `if (.IO_is_allowed)`, i.e. **on the master alone**.
So when a *non-master* rank died — the usual case for an MPI-only defect — nothing was printed
anywhere: not stdout, not stderr, not `<job>.err`. The job aborted in silence.

Fixed at all three `MPI_ABORT` sites (`die` and both `unknown` overloads): before aborting, each
rank writes its own message straight to Fortran's preconnected **stderr (unit 0)**, tagged with
the rank.

Two deliberate choices there:
- **Unit 0, not `.std_err_unit`** — unit 0 exists on every rank, whereas the `std_err` file is
  opened only on the master and its unit number is merely broadcast.
- **No `flush` of the TEXTFILE** — `TEXTFILE:flush` contains `PARALLEL_BROADCAST`s, so flushing
  from the single dying rank while its peers run on would be a collective entered by one rank,
  i.e. a hang. **The error path must not use collectives.**

**It paid for itself immediately.** The first run after the fix named two real defects that had
been invisible:

1. `SYSTEM:reduction_is_allowed ... a PARALLEL_* reduction is inside a parallel do` — the
   milestone-6 abort firing on a **genuine dead reduction** in the SCF path. Note the static
   lint reports *nothing* lexically inside a `parallel do`, and a one-level call scan finds
   nothing either, so this is reached through a **deeper call chain** — precisely the case the
   lint cannot see and the runtime abort can. The two checks are not redundant.
2. `CIF:find_looped_item ... CIF file has not been opened` on rank 1 — rank-local file state.

The abort message now also names the **locking routine**, via `trim(.parallel_do_lock)`, since
`LOCK_PARALLEL_DO` records which routine holds the lock — that is exactly "which `parallel do`
am I inside", which is what you need to fix it.

*(Correction: an earlier note here claimed `ENSURE` messages must be plain literals because
`message` is fixed-length. That was wrong — it generates `STR(len=*)`, and concatenation is
fine. The debug-build break was **entirely** the lowercase `pure` vs the `PURE` macro; two
changes were made at once and both were credited. The claim has been removed from CLAUDE.md.)*

### superseded note: `DIE` messages are LOST under MPI

When a `DIE` fires under MPI the message never appears — not on stdout, not on stderr, not in
`<job>.err`. `SYSTEM:die` writes to the `std_err` TEXTFILE, which is *buffered*, and then calls
`MPI_ABORT`, which kills the job before anything is flushed. So a parallel job that dies tells
you **nothing at all** about why.

That is its own defect and worth fixing before more MPI debugging: flush `std_err` (and stdout)
immediately before `MPI_ABORT` in `SYSTEM:die`. It would have saved most of the effort above.

### How it was found

**The `256 1` pair is `BUFFER:put_str`** (`buffer.foo:505-528`). It broadcasts `.string`
(a `BSTR`, 256 bytes) and then `.item_end` (an `INT`, 1) — exactly the repeating pair in the
trace. So the whole output path is built on these, and the two ranks call `put_str` a
**different number of times** while writing the command-line echo:

```
rank 0:  256 1 | 256 1 1 1 | 256 1 256 1 256 1 1 1 | ...   (37)
rank 1:  256 1   1                                          (3, then aborts)
```

They agree on the first `put_str`, then master starts a second one while rank 1 has moved on to
a scalar. **Who calls `put_str` a different number of times is the one remaining question.**
Note the broadcasts themselves are correctly *outside* the `IO_IS_ALLOWED` guard; what is inside
it is the buffer update and the `ENSURE` — so the collective is fine and the *control flow*
reaching it is not.

### Separately, and arguably worse: output costs two collectives per token

`BUFFER:put_str` broadcasts a **256-byte string plus an integer on every call** — i.e. every
token written to any output buffer is two MPI collectives. That is why printing a banner
accounts for 126 broadcasts before `hart` has done any science at all. It is correct, but it
makes all output a synchronisation point and will not scale. The obvious question for whoever
picks this up: does the buffer need broadcasting at all on a *write* path, where non-master
ranks discard the result? (On a *read* path it clearly does.) Splitting the read and write cases
would remove nearly all of them.

The instrumentation was reverted; the tree is clean. Re-applying it is ~10 minutes and the
recipe is above.

**Earlier attempt, TESTED AND NOT THE CAUSE.** The broadcast was added (`file.foo`, `close_and_delete`
now matches `close`), the MPI tree rebuilt, and `hart` at 2 ranks fails **identically** —
`MPI_ERR_TRUNCATE` in `MPI_Bcast` reported by rank 1, at the same point, `urea.out` still
stopping after 29 lines at the option echo. **Do not re-try this fix.**

The fix was kept anyway: it is correct on its own terms, it removes a genuine rank-local branch,
and it is verified serially (release rebuild, short suite 50/51 unchanged, all four invariant
checks pass, `ctest -L hart` 4/4).

So the desync is elsewhere in the same window — between the option echo and the basis-validation
`DIE_IF`s. **Next step is instrumentation, not inspection**: the debug MPI tree
(`build-mpi-debug`, `~/opt/openmpi-gf14`) has `USE_PRECONDITIONS` and `-fbacktrace`, and the job
is seconds long, so a marker printed per rank before each collective in that window will find it
in one run. Inspection has now failed twice here.

**Also found:** `hart`'s early-exit paths return **non-zero under MPI** — `hart --version` at 2
ranks exits 1, because `stop` runs on one rank without `MPI_FINALIZE` on the others. Harmless
serially, wrong for any harness, and a separate small fix.

## Deferred: prune dead and stale macros in `include/macros.in`

**Audited 2026-07-29.** Of **377** macros defined, **145 are never used in any `.foo` file**.
Fewer macros is better for maintenance (Dylan), but they are not all the same kind of thing and
should not be deleted with one sweep:

**(a) Genuinely dead — delete.** `TONTO_SET_STDERR` / `TONTO_SET_STDERR0` is the clearest case:
unused *and* it expands to `set_error_output_file_(tonto,X)` / `SYSTEM_set_error_output_file`,
a routine that **does not exist anywhere in `foofiles/`**. It would fail to compile the moment
anyone used it.

> **CORRECTION 2026-08-02 — the rest of this list was WRONG and acting on it would have broken
> the build.** The 2026-07-29 audit grepped only `foofiles/`, and so was blind to two other
> sources of use. Re-measured:
>
> | macro | audit said | actually |
> |---|---|---|
> | `TONTO_CREATE` | unused | **50 runfiles**, 6 generated `.F90` |
> | `TONTO_DESTROY` | unused | **48 runfiles**, 6 generated `.F90` |
> | `PARALLEL_DO_START` / `_STRIDE` | unused | **emitted by the translator**, 10 generated `.F90` |
> | `LOCK_PARALLEL_DO` / `UNLOCK_PARALLEL_DO` | unused | **emitted by the translator**, 10 generated `.F90` |
> | `PARALLEL_VECTOR_SUM` | unused | genuinely unused — the only true positive |
>
> `TONTO_CREATE`/`TONTO_DESTROY` live in `runfiles/`, which the audit did not scan. The
> `PARALLEL_DO_*` and `*_PARALLEL_DO` family can **never** appear in a `.foo` file by
> construction: `FooToFortran` writes them into the generated Fortran when it lowers
> `parallel do`. Any macro audit must cover **three** sources -- `foofiles/`, `runfiles/`, and
> what the translator emits -- or it will report build-breaking false positives.

**(b) Stale defaults — the real hazard.** Macros that look like the tunable default for
something but no longer drive anything, because the code sets its own value. Editing one has no
effect, which is a trap. Worse, at least one has *drifted*:

```
macros.in:629   ROBY_ZERO_ANGLE_CUTOFF   TOL(2)
roby.foo:176    .zero_angle_cutoff = TOL(2)*RADIAN_PER_DEGREE     <- not the same value
```

**These are safe to delete: the value has migrated into the type component's `DEFAULT(...)`
in `types.foo`** (Dylan — confirmed: `types.foo` carries 1103 `DEFAULT(...)` declarations).
The documentation is not lost by deleting the macro; it now lives on the component that owns
it, which is the better place. Checked one by one:

| macro | macro value | `DEFAULT` in `types.foo` | |
|---|---|---|---|
| `ROBY_OUTPUT_THETA_INFO` | `TRUE` | `TRUE` | matches |
| `ROBY_ANALYZE_ALL_ATOM_PAIRS` | `FALSE` | `FALSE` | matches |
| `QUADRATURE_ACCURACY` | `TOL(6)` | `TOL(6)` | matches |
| `QUADRATURE_MAXIT` | `10` | `10` | matches |
| `ISOSURFACE_ISO_VALUE` | `ONE` | `ONE` | matches |
| `TEXTFILE_MARGIN_WIDTH` | `0` | `0` | matches |
| `TEXTFILE_SPACING` | `2` | `2` | matches |
| `TEXTFILE_INT_WIDTH` | `8` | `8` | matches |
| `ISOSURFACE_TABLE_LENGTH/SPACING/EPS` | `30.0d0` / `0.02d0` / `TOL(9)` | `INTERPOLATOR_TABLE_*` | re-homed to the owning module |
| `TEXTFILE_COMMENT_CHARS` / `_QUOTE_CHARS` | `"!#"` / `"'"""` | `BUFFER_COMMENT_CHARS` / `BUFFER_QUOTE_CHARS` | re-homed |
| **`ROBY_ZERO_ANGLE_CUTOFF`** | `TOL(2)` | `TOL(2)*RADIAN_PER_DEGREE` | **DIVERGED** |
| **`PLOT_GRID_PLOT_FORMAT`** | `"gnuplot.contour"` | `" "` | **DIVERGED** |
| `DIIS_ERROR_TEMP_CUTOFF`, `FILE_BUFFER_LENGTH` | `TOL(2)`, `1024` | none found | check individually |

The two **DIVERGED** entries are the argument for doing this sooner rather than later: they are
not merely dead but *wrong*. Anyone "tidying up" by wiring the code to `ROBY_ZERO_ANGLE_CUTOFF`
would silently drop the degrees→radians conversion.

Remaining unchecked in this class: `ADAPTIVE_QUADRATURE_ACCURACY`, `QUADRATURE_EPS`, the other
`ISOSURFACE_*`/`PLOT_GRID_*`/`TEXTFILE_*` entries, `MULTI_T_ADP_TOL_0`,
`REAL_MAX_DECIMAL_PLACES`, `BASIS_LIBRARY_ENV_NAME`, `TONTO_REPOSITORY_BASIS_DIRECTORY` — same
method: find the component, compare its `DEFAULT` with the macro, delete if superseded.

**(c) Unused but arguably intentional API — leave alone.** Tonto is a library, and these are
language surface a future `.foo` could legitimately use: the kind/size families (`INT_1_SIZE`,
`REAL_16_SIZE`, …), the alternate scalar types (`INT_1/2/4/8`, `REAL_4/8/16`, `CPX_4/8/16`,
`CHR`, `BSTR`), `MAT6`/`MAT7`, and the physical-constant set (`AVOGADROS_NUMBER`, `BOHR_SI`,
`KCAL_PER_HARTREE`, …). Same reasoning as keeping the `set_width_automatically` fix.

**Caveat on the audit:** it counted uses in `foofiles/*.foo` and inside `macros.in` only. A
macro could in principle be referenced from another `include/` file or by the build, so confirm
before deleting any individual one.

## Deferred: adopt OpenBLAS consistently (single-threaded) on Linux and WSL

**Decision (2026-07-30): not now.** Do the Mac/Linux numerical comparison first. The intended
end state is **OpenBLAS, pinned to one thread, on every platform** — matching what macOS already
does — but getting there means **redoing the reference outputs completely**, so it must not be
started in the middle of other work.

### What is actually happening today

Measured, not inferred — `ldd release/tonto` links `libblas.so.3` / `liblapack.so.3`, which on
Debian/Ubuntu are `update-alternatives` symlinks. On the development box exactly one alternative
is registered (priority 10, from `libblas3` 3.12.1): the **netlib reference BLAS**. OpenBLAS is
not installed. The WSL CI runner resolves the same way (`Found BLAS:
/usr/lib/x86_64-linux-gnu/libblas.so`, LAPACK 3.12.0).

| Platform | Selection | Verdict |
|----------|-----------|---------|
| macOS | `CMakeLists.txt:110` explicitly prefers Homebrew OpenBLAS over Accelerate, and warns on fallback — because Accelerate's Fortran LAPACK is frozen at 3.2.1 (2009) and on its own flipped `short/h2o_rhf_6-31G(d)_normal_mode_analysis` | Handled |
| Linux | No `BLA_VENDOR`; bare `find_package(LAPACK)` takes whatever the distro alternative points at. README installs `libblas-dev liblapack-dev` = reference | **Suboptimal** |
| WSL | Same as Linux; `docs/BUILDING_ON_WINDOWS.md` installs the same reference packages | **Suboptimal** |

So the platform that got careful attention is macOS, while the two reference platforms silently
get the slowest BLAS. That is backwards.

Note the two axes are independent: **LAPACK version** (3.12.x here — modern, so the `ILAVER`
probe stays quiet) is a correctness/algorithms question; **BLAS implementation** (reference) is a
speed one.

### How much is actually on the table

Less than it first appears, and the bigger lever is elsewhere. Tonto's LAPACK surface is narrow —
`dsyev`, `zheev`, `zhpev`, `dgesv`, `dgetrf`/`dgetri`, **all in `mat{real}.foo` / `mat{cpx}.foo`**.
There is **not one explicit `dgemm` call**. Matrix multiplication goes through the Fortran
`matmul` intrinsic instead: **375 call sites across 35 files**. By default gfortran services
`matmul` from libgfortran's own blocked routine, never touching BLAS.

So swapping in OpenBLAS alone accelerates only the eigensolves and linear solves. The flag that
would route `matmul` to `dgemm` is **`-fexternal-blas`** (with `-fblas-matmul-limit=<n>` as the
size threshold); it is not set anywhere in `cmake/SetFortranFlags.cmake`. That is plausibly the
larger win — and a larger numerical perturbation.

Also unquantified: how much of a typical HAR/SCF run is linear algebra at all, versus Tonto's own
two-electron integral code. Benchmark before believing any of this is worth it.

### Why this forces a full re-bless

Both changes alter summation order, so last digits move; the loose gate (rel ≤ 0.2 % OR
last-digit ≤ 2) exists precisely for this, and the macOS case above shows a swap *can* flip a
test outright.

The sharper hazard is **threading**. Reference BLAS is single-threaded; OpenBLAS is not, and its
results vary with thread *count*, because the blocking — and hence the reduction order — changes.
Every stored reference in `tests/` was generated against single-threaded reference BLAS. Adopting
OpenBLAS without pinning threads would produce run-to-run last-digit noise indistinguishable from
regressions. Hence the decision above: **one thread**, via `OPENBLAS_NUM_THREADS=1`, set in the
harness (`scripts/test.py` / `scripts/suite_report.py`) so it cannot be forgotten. Multithreaded
OpenBLAS would also oversubscribe cores in MPI builds.

### Suggested order when this is picked up

1. Finish the Mac/Linux numerical comparison **first** — it is the baseline everything else is
   measured against, and it must not be perturbed mid-flight.
2. Benchmark `libopenblas-dev` alone (threads pinned to 1) on one representative HAR job. Record
   before/after wall-clock **and** the agreement table.
3. Separately benchmark `-fexternal-blas`, same job, same pinning.
4. Only then decide whether the measured speedup justifies re-blessing every reference. If it
   does, re-bless in one deliberate commit across all platforms at once, so Linux, macOS and WSL
   share a single BLAS story.
5. Update `README.md`, `docs/BUILDING_ON_WINDOWS.md` and `scripts/wsl_doctor.sh` together — they currently
   tell users to install the reference packages.

---

---

# Test suite and numerics

## Reinstate the ammonia-borane pHAR test — blocked by a 167 MB asset (Dylan, 2026-08-05)

**pHAR code ships in the library but nothing tests it.** `MOLECULE.CE:phar_defragment` is live
(`molecule.main.foo` dispatches `phar_defragment`, added by Kang 2025-03-13), and
`crystal.foo`, `molecule.har.foo` and `molecule.rho.foo` all carry pHAR-specific branches. There
is **no pHAR test in `tests/`**.

One exists, on a branch: `tests/long/ammonium_borane_pHAR_C23/`, on
**`origin/release-pHAR-broken`** (tip `18712bd6`, 2025-04-29, kanghyun-chu).

**What is there, and what is missing:**

| file | size | |
|---|---|---|
| `stdin` | 946 B | the job |
| `stdout` | 269 KB | the blessed reference |
| `B6H6_grown.cif` | 7 KB | input |
| `tonto_data_on_F_20rfl.hkl` | 16 KB | input |
| `Crystal23_InputFiles.zip` | 12 KB | the Crystal23 inputs that *generate* the XML |
| **`GenerateXML.XML`** | **134 B** | **a Git LFS pointer, not the file** |

The `IO` manifest lists `GenerateXML.XML` as a required input, so **the test cannot run**. The
pointer says:

```
version https://git-lfs.github.com/spec/v1
oid sha256:1c5c24f0903c1b8667e3f8aa41ba1b2a550a49370b22db422a37d7a1f093a8ee
size 174978609
```

i.e. the real asset is **167 MB**.

**FIRST STEP, and it decides everything below: is that LFS object still retrievable?** This was
*not* checked — `git-lfs` is not installed on the machine this was investigated from. One
command answers it:

```bash
sudo apt install git-lfs          # or: brew install git-lfs
cd <a scratch clone of tonto>
git lfs ls-files -l origin/release-pHAR-broken     # does GitHub still know the oid?
git lfs fetch origin release-pHAR-broken           # does it actually come down?
```

- **If it downloads**, the fix is small: restore a `.gitattributes` that tracks
  `tests/long/**/*.XML`, port the one test directory onto `release`, and decide whether a
  167 MB clone cost is acceptable (option 4 below says it probably is not).
- **If it 404s or the quota is exhausted**, the object is gone and options 1–3 are the only
  real ones — regenerate, fetch on demand, or shrink the case.

**How the deposit went wrong** is legible in the branch's own history:

```
0f70beae  Activated git lfs and added .gitattributes files but NOT YET large XML
69d6e806  Added large XML file
...
18712bd6  The Crystal23 input files for generating the GenerateXML.XML are included
```

LFS was enabled and `.gitattributes` added, then the XML committed — but **`.gitattributes` does
not exist at the branch tip**. So the tracking configuration was lost while the pointer file
stayed behind, and a plain `git clone` gets a 134-byte text file where a 167 MB XML should be.
Whether the LFS object is still retrievable from GitHub was **not** checked (git-lfs is not
installed on this machine) — check that first, since it decides which option below is viable.

**Options, in rough order of preference:**

1. **Regenerate rather than store.** `Crystal23_InputFiles.zip` (12 KB) is committed precisely so
   the XML can be rebuilt — that is what `18712bd6` was for. Needs Crystal23, which is
   proprietary, so this makes the test reproducible only for those who have it.
2. **Fetch on demand**, the way the ANTLR jar already is at configure time: host the XML
   somewhere durable and have the test skip cleanly when it is absent.
3. **Shrink the case.** 167 MB for a test asset is the real problem; a smaller cell, fewer
   reflections, or a trimmed XML would make this an ordinary test.
4. **Proper LFS** — restore `.gitattributes` and re-push the object. Note GitHub's free LFS quota
   is 1 GB storage *and* 1 GB/month bandwidth; a 167 MB asset cloned a handful of times exhausts
   it, which is a poor fit for a public repository.

**Note the branch is 631 commits behind `release` and 8 ahead**, so this is a port of one test
directory, not a merge. Those 8 commits also contain unrelated work (form-factor symmetrisation
tables with RMS and MAX residuals) that may or may not already be in `release` — check before
cherry-picking.

---

### ANSWERED AND UNBLOCKED (2026-08-16): the asset is alive, and the test PASSES

**The first step above was finally run, and the answer is yes.** The LFS object is still on
GitHub, retrievable, and byte-exact.

It did not need `git-lfs` to find out. The LFS protocol is plain HTTP, so the batch API
answers it directly — useful whenever `git-lfs` is missing:

```bash
curl -s -X POST \
  -H "Accept: application/vnd.git-lfs+json" -H "Content-Type: application/vnd.git-lfs+json" \
  -d '{"operation":"download","transfers":["basic"],"objects":[
       {"oid":"1c5c24f0903c1b8667e3f8aa41ba1b2a550a49370b22db422a37d7a1f093a8ee",
        "size":174978609}]}' \
  https://github.com/dylan-jayatilaka/tonto.git/info/lfs/objects/batch
```

It returns a signed `download.href` (1 h expiry) rather than an error, i.e. the object is
present and the quota is not exhausted. Downloading it gives 174 978 609 bytes whose sha256
matches the oid exactly, opening with
`<CREATOR name="CRYSTAL" version="1.0.0"/> <TITLE> Ammonium_closo-hexaborane(6)_pHAR </TITLE>`.

#### How to pull the file and run the test

`git-lfs` is installed as of 2026-08-16. Work in a **separate worktree**, so the main tree and
its tests cannot be disturbed:

```bash
# 1. an isolated checkout of the archived branch (shares the object store, no second clone)
git worktree add -b wip/phar ~/github/tonto-phar archive/release-pHAR-broken

# 2. pull the 167 MB asset. Note .gitattributes is ABSENT at the tip, but the pointer is in
#    the index, so git-lfs still resolves it -- `git lfs ls-files -l` lists the oid
cd ~/github/tonto-phar
git lfs pull --include "tests/long/ammonium_borane_pHAR_C23/GenerateXML.XML"
git lfs status        # expect: LFS: 1c5c24f -> File: 1c5c24f

# 3. run it against the CURRENT binary, not a build of the 2025-04 branch. That branch
#    predates the ANTLR4 translator and foo.pl is gone, so it will not build; the point is
#    to reinstate the TEST on develop, and the test is data plus a job file
mkdir -p /tmp/phar && cd /tmp/phar
cp ~/github/tonto-phar/tests/long/ammonium_borane_pHAR_C23/{stdin,B6H6_grown.cif,tonto_data_on_F_20rfl.hkl,GenerateXML.XML} .
sed -i '/thermal_smearing_model=/d' stdin      # see below -- obsolete keyword
TONTO_BASIS_SET_DIRECTORY=<repo>/basis_sets <repo>/build/tonto
```

**One edit to the job file is required.** As committed it dies with

```
Error in DIFFRACTION_DATA.READ:process_keyword ... unknown option: thermal_smearing_model=
```

`thermal_smearing_model=` was removed by `acb7af0b`, *"Removed 'temperature_factor_model' and a
few others, **in favour of deriving the info from partition_model**"* — the whole machinery is
commented out, keyword, reader, setter, `CRYSTAL` accessor and its `molecule.scf.foo` consumers.
The job already sets `partition_model= oc-crystal23`, which now carries that information
(confirmed by Dylan, 2026-08-16), so **deleting the line is the correct port**, not a workaround.

#### Result: exact reproduction, 3 m 41 s

With that one line removed, the job runs to completion on current `develop` and reproduces the
2025-04 reference **digit for digit**:

| | this run | reference |
|---|---|---|
| `Structure fit converged.` | yes | yes |
| R(F) | 0.005188 | 0.005188 |
| N_r | 20 | 20 |
| N_p | 11 | 11 |
| GoF² | 0.631422 | 0.631422 |
| GoF | 0.794621 | 0.794621 |
| scale factor | 0.979852 | 0.979852 |

So **pHAR still works**, and the code that ships untested is now known to be correct on this
case. That was the whole point of the entry.

#### What remains, and it is small

The reference is 2928 lines against this run's 971. The difference is **not numerical**: the
`thermal_smearing_model=` keyword echo (removed), a *"crystal data already defined!"* warning,
and a **"Form factor asymmetry"** diagnostic section the current build no longer prints. So
reinstating the test needs its `stdout` re-blessed — legitimate, given a year of intervening
development, but a blessing decision.

Remaining work to land it, with **option 2 (fetch on demand) chosen by Dylan**:

1. Port `tests/long/ammonium_borane_pHAR_C23/` to `develop`, keeping the 134-byte **pointer**
   rather than the asset — a 167 MB file in a public repo is what option 4 rightly rejects.
2. Restore a `.gitattributes` tracking `tests/long/**/*.XML`, which is the file whose loss
   caused all of this.
3. **The test must SKIP, not fail, when the asset is absent** — otherwise every clone without
   the object goes red. `scripts/test.py` has no skip mechanism today; ctest's
   `SKIP_RETURN_CODE` is the natural hook. This is the only real piece of work left.
4. Re-bless `stdout` from a current run.
5. Note the runtime: 3 m 41 s, so `long`, and comfortably the slowest test in the suite.


## Deferred: small numerical differences (longstanding) — drill down

Several tests differ from their references only by small numerical amounts — 3rd–4th
significant figure, or a last-digit wobble on a near-zero value. They pass the loose gate
(rel ≤ 0.2% OR last-digit ≤ 2), but some sit close enough to the boundary that a different
runner/CPU (BLAS / eigensolver ordering, FP reassociation) flips the verdict — this is the
**CI flake** seen on GitHub Actions (same binary, pass on one runner, fail on the next).
**Dylan wants to drill down** on each and fix the root cause, not merely tolerate them.

### The XCW `Penalty in F` drift, found while reblessing (2026-08-26)

**Filed here at Dylan's direction**, alongside the other small numerical differences:
they look alike and may well have *unrelated* causes, but one register is the place to
see them together.

Reblessing the six `long` XCW/HAR references after the extinction/GoF merge, almost
every changed number was accounted for. `N_p` went 2 → 1 — the milestone 11 decision to
stop counting the XCW Lagrange multiplier — and that one change propagates through the
whole per-shell `GOF` column, each shell scaling by `sqrt((n_refl-1)/(n_refl-2))`. Every
other column (`R(F)`, `Rw(F)`, `R(F2)`, `Rw(F2)`, `F_exp/F_pred`, `# of refl`) is
**byte-identical**. Hundreds of differing lines, one parameter.

**One number is not accounted for.** In `nh3_x-ray-constrained-rhf_cc-pVTZ`:

| | old | new | ratio |
|---|---|---|---|
| `Penalty in F` = `GoF^2(N_p)` | 7.557 | 7.491 | 1.00881 |

With `N_r` = 88, a pure `N_p` 2 → 1 change would give `87/86` = **1.01163** if
`GoF^2 = chi^2/(N_r - N_p)`. Observed is 1.00881, leaving **~0.28% unexplained**.

Caveats, so the next person does not over-read this:

- **That formula is assumed, not verified.** `Penalty in F` may not be
  `chi^2/(N_r - N_p)` at all; if it is not, there is nothing to explain here and the
  first job is to read what the code actually computes.
- **It is not platform drift.** macOS/gfortran-14 and Linux/gfortran-16 both print
  7.491 to every digit — checked directly, not inferred.
- **The per-shell residuals are probably print precision, not signal.** Each shell ratio
  falls 0.06–0.21% short of the pure-`N_p` prediction, but the references carry only 4
  significant figures, which alone accounts for ~0.09%. The `Penalty` residual is on
  values printed to the same precision and is three times larger, so that one is real.

### Two more for the same register

- **`quartz_NN_HAR_L0`/`L1` fail on macOS only — and their references are correct.**
  This was first written up here as two more stale references. It is not: reblessing on
  Linux/gfortran-14 (2026-08-26) left both files **byte-unchanged**, because they already
  agree there `exact=PASS, max 0%`. They fail on arm64 macOS and nowhere else, which puts
  them beside `ylid` above rather than with the XCW reblessing.

  **It is not a crash and not a debug-build artefact**: exit 0, full 50 kB of output, job
  completes, gfortran-14 **release**. Measured 2026-08-26.

  **What actually fails is row ORDER in a sorted reflection list, not the numbers.**
  Matched by Miller index instead of by line position, the two runs list the **same 54
  reflections** and the largest per-reflection difference is **0.158%** — inside the 0.2%
  gate. But 4 rows of 160 come out in a different order, because two sort keys sit
  **0.103% apart** and sub-0.2% platform drift is enough to flip them:

  | | reflection | key |
  |---|---|---|
  | Linux, row 21 | `(-8, 8, 10)` | −6.6098 |
  | macOS, row 21 | `(-5, 9, 11)` | −6.6166 |

  `scripts/test.py` compares line by line, so one transposed row pairs unrelated fields and
  reports `0.7196 vs 0.0000` — the **100%** in the ctest log. Nothing is 100% wrong.

  *(Two earlier explanations of this in the git history are wrong and should not be
  believed: that these were stale references, and that a changed ESD digit count shifted a
  column width. Both were inferred from a truncated diff. The reflections were compared by
  index only after the second one failed to hold up.)*

  **Where the order comes from:** `DIFFRACTION_DATA.PUT:put_N_worst_reflections`, which does
  `F_z.quick_sort(ind,decreasing_order=TRUE)` and prints `.reflections(ind(1:n))`.

  **A tie-break on `(h,k,l)` will NOT fix this — do not try it.** That was proposed here and
  is wrong: the keys are not tied, their true order reverses. Measured for the pair that
  swaps:

  | | `(-8, 8, 10)` | `(-5, 9, 11)` | gap |
  |---|---|---|---|
  | Linux | −6.6098 | −6.6106 | 0.0008 |
  | macOS | −6.6168 | −6.6166 | 0.0002 |

  Drift of ~0.001 on values of ~6.6 exceeds a gap of 0.0008, so the comparison genuinely
  reverses. A secondary key only helps for *equal* primaries. More generally: **any total
  order on a noisy continuous key can flip**, so no sort refinement fixes this class.

  **Decision 2026-08-26 (Dylan): leave it, record it.** The two fixes that would work, with
  what each costs, so the choice does not have to be re-derived:

  - *Normalise in `scripts/test.py`* — sort the rows of the block on both sides before
    comparing. Output and references unchanged; but it is a shared-harness change touching
    every test, and it masks genuine row-ordering regressions in this table.
  - *Canonical order in the Foo source* — keep `F_z` for **selecting** the worst `n`, print
    them ordered by `(h,k,l)`. Platform-stable; but a crystallographer loses worst-first
    reading order, and three references need reblessing on Linux.

  **Consequence of leaving it, and it is the reason this is written down:** this is **not a
  macOS problem**. Any machine whose BLAS or FP ordering moves `F_z` by ~0.01% can transpose
  those rows — a different Linux CI runner included. It is a latent flake wearing a
  platform-specific mask, like `ylid` above. If `quartz_NN_HAR_L0`/`L1` ever go red on
  Linux, read this entry before believing the number.

  **Do not bless these on a Mac.** It would bake macOS numbers into a Linux-gated project
  and leave Linux passing on under a third of the tolerance. Generating references on
  Linux/gfortran-14 is what exposed that they needed no reblessing at all.
- **`urea_ccsd_pob-TZVP_Salvador_properties` (2.99%) and `h2o_rhf_cc-pVDZ_tdhf` (0.231%)
  both PASS in a gfortran-16 debug build** while failing in gfortran-14 release on the same
  Mac. Two variables differ between those builds — compiler *and* optimisation — so this
  isolates nothing by itself. The cheap next experiment is a gfortran-14 **debug** build: if
  they pass there too, the culprit is `-Ofast`, not the compiler. Note `ci-macos.yml`'s
  header attributes the first to macOS; that attribution is at least incomplete.

### Lower priority: `ylid` (rgbi) — vdW contact indices differ on macOS

**Status 2026-07-29:** fails on **macOS only** (3.85% max rel); **passes on Linux** against the
current reference, with both platforms on gfortran-16 and LAPACK 3.12.0.

**Where the difference lives:** splitting the diff at the *"Roby-Gould bond indices: VDW
interactions only"* boundary gives **14 of 17** differing hunks inside the vdW section and only
3 outside it (the largest of those being a Roby population, `C1 8.95` vs `8.96`). So the
van der Waals contacts — non-bonded pairs at 2.3–3.3 Å, enabled by `analyze_vdw_atom_pairs= T`
— carry most of the instability, as Dylan expected.

> **SUPERSEDED 2026-08-02.** This was written when vdW analysis was still ON. It is now **OFF**:
> Dylan turned it off in `99b1b535` (2026-07-31, *"tests(ylid): turn vdW contact analysis off"*),
> and `tests/rgbi/ylid/stdin:26` reads `analyze_vdw_atom_pairs= F`. The test's own comment records
> the reasoning -- the vdW section was the whole of the macOS/Linux disagreement, and this test
> exists to exercise Roby bond indices, which do not need it. The argument below was not
> persuasive at the time and is kept only for the timing data (vdW ON ~46 s, OFF ~59 s), which
> remains an unexplained oddity: turning work *off* made the job *slower*.

**Considered and NOT adopted at the time: turning the vdW pairs off and re-blessing.** Two reasons:

1. **It would break Linux.** ylid currently *passes* there, so a re-bless would enshrine macOS
   numbers and flip the platform that is presently correct.
2. **It does not speed the test up** — the opposite. Measured interleaved, three rounds:
   vdW **ON** 45.9 / 46.7 / 46.3 s, **OFF** 59.5 / 59.5 / 59.1 s, i.e. ~13 s *slower* with
   fewer output lines (607 vs 660). Whatever `analyze_vdw_atom_pairs= F` does, it is not
   "skip work" — it selects a different and more expensive set of pairs. That oddity is worth
   understanding on its own; a flag that costs time when disabled is a bug smell.

It would also drop coverage of the vdW code path, and the 3 non-vdW hunks would likely remain.

**Also ruled out:** tightening the guess-SCF convergence. Settings from `convergence= 1e-3`
(default) down to `1e-6` leave both the runtime (~46 s throughout) and the disagreement
essentially unchanged (108 → 106 differing lines); `1e-8` makes the atomic SCF fail to converge
outright, so `make_ANOs` DIEs and the job aborts — which is the DIE working as intended, but it
also produced a *shorter* output that briefly looked like a speed-up and a perfect match. Beware
that trap when timing truncated runs.

**Open:** where the 46 s actually goes (convergence is not the lever), and why the vdW indices
are the platform-sensitive part.

### NOT STARTED: `# of unmatched Fridel pairs` reports *every* reflection (and is misspelled)

Found during the H1 fragHAR archaeology (2026-08-02, `docs/RUNNING_HART.md` §6). In
`tests/long/gly_ala_fragHAR_rhf_STO-3G/stdout` the refinement-results block reads:

```
# of reflections,    N_r .......... 2514
# of unmatched Fridel pairs ....... 2514
```

i.e. **all 2514 reflections are counted as unmatched Friedel pairs** — a suspicious equality
rather than an obviously wrong number, so it may be a correct-but-uninformative diagnostic (this
is a centrosymmetric-in-projection dataset with no anomalous signal to pair up) or a real
miscount. Nothing downstream is known to consume it, so it is a reporting question, not a
correctness one, until shown otherwise.

Three things to settle together:

1. Whether the count is right, and what it should be for a dataset with no Friedel mates.
2. **The line is misspelled** — `Fridel` should be `Friedel`. Reference-visible, so it needs a
   re-bless of every stdout carrying it; batch it with other cosmetic output fixes rather than
   spending a re-bless on one word.
3. It is **new since 2019**, arriving together with a `Using single scale factor ...... T` line.
   The numeric `Scale factor ...... 0.9768` is still printed directly underneath, so nothing was
   lost — the block simply gained two lines.

Not a regression in the science: the 2019 and 2026 refinements agree to 4 significant figures
(table in `docs/RUNNING_HART.md` §6). Deferred until after H1.

### PRIORITY, NOT STARTED: NaN and negative ESDs from the least-squares variance-covariance matrix

**This is the live thread — pick it up here.** Two impossible esd values are confirmed by
instrumentation, in two different tests. An esd is a square root: it can be neither negative
nor NaN. Per Dylan the source is the **least squares — the variance-covariance matrix is not
right**, either a genuine error in its construction or UB.

**Evidence (both from probes inserted in `TABLE_COLUMN:set_values_and_errors`, since removed):**

| test | finding |
|---|---|
| `short/urea_lamaGOET_grown_CIF` | ADP U13/U23 columns: `e_neg = 2`, `e_zero = 2` of 5 rows — two **negative** esds |
| `long/urea_rhf_STO-3G_HAR` | one ADP column: **`e_nan = 1`**, with `prec_out = 5` (i.e. the column precision was normal — the *data* is bad, not the formatting) |
| `long/gly_ala_fragHAR_rhf_STO-3G` | headline statistic **`Rw(F2) ....... NaN`** — and it is NaN in the 2019 `ecb593e9` output too (`docs/RUNNING_HART.md` §6), so this predates every change under investigation. Note `Rw(F)` beside it is fine (0.0334), so whatever poisons the F² weighting does not touch the F one |

The `e_nan` probe result also killed the competing explanation that the column precision was
simply small (`max_dp=1` would give `dp=2` innocently); `prec_out` was 5 in every column.

**What has been fixed — formatting only, NOT the numbers.** `REAL:get_dp_de_le` now guards with
`NOT (abs_error>ZERO)`, which catches zero *and* NaN in one test (every comparison against NaN
is false), and takes `abs()` for the negative case. Previously these reached `log10()`, which is
undefined for both, and under `-ffast-math` rendered as 6 decimal places on Linux/x86 and 2 on
macOS/arm64 **from identical input** — the whole of the longstanding "zero(error) problem".
`TABLE_COLUMN:set_values_and_errors` now emits a `WARN` when a column contains a NaN or negative
esd, deliberately rather than silently printing `(0)`, because a NaN esd rendered as `(0)` is a
wrong number presented as a confident one.

**So the output is now well-defined and platform-independent; the esds are still wrong.**

#### LOCALISED (2026-07-30): the bad esd sits on a symmetry-constrained-to-zero ADP component

Found by consumer-side probing in `TABLE_COLUMN:set_values_and_errors` under the **gfortran-14
debug build** (`-DUSE_PRECONDITIONS`, so `PURE` is off and `WARN` is live — see §8 of
`CLAUDE.md`). Static tracing failed six times on this path; printing from the consumer
succeeded on the first try. Prefer that direction here.

The probe dumped the offending column in full (`long/urea_rhf_STO-3G_HAR`):

```
column heading = [U_xy]    subheading = []      n rows = 5   NaN count = 1
e (esds)  :  1  0.0000   2  0.0000   3  NaN     4  0.0028   5  0.0027
r (values):  1  0.0000   2  0.0000   3  0.0000  4  0.0000   5  0.0000
```

**Every data value in the column is exactly zero.** `U_xy` in that axis system is
**symmetry-constrained to zero** for all five atoms of urea. So the *value* is fixed at zero,
and it is only its *error* that is broken: two atoms get an exact `0`, two get a plausible
`~0.003`, and one (row 3, carbon) gets NaN.

This re-frames the bug. The esd of a symmetry-zero component should be identically zero, and
it is being computed instead as the square root of a quantity that ought to be exactly zero
but lands slightly **negative** — NaN for one atom, exact zero for the others, i.e. pure
round-off noise about zero decided the outcome per atom.

**Corroboration that the covariance matrix itself is fine.** Probes A, B and E on
`.xray_data.covariance_mx` (before *and* after the transform) reported **zero** NaNs and zero
negatives, and the `sqrt` guard added in `ATOM:set_pADP_errors_to` **never fired**. So the
defect is *not* in the least-squares covariance as first assumed — it is introduced when the
ADP errors are transformed into the axis system in which `U_xy` vanishes. Dylan called this:
"The transformation, linear or non-linear, are implicated."

Independent support from Tonto's own output: the *other* ADP table in the same run prints
**non-zero** `U_xy` for the same five atoms (`0.0007, -0.0122, 0.0002, -0.039, -0.001`).
`U_xy` is zero only *after* the transformation — exactly where a transformed variance can
land on `-1e-19` instead of `0`. That table also carries an `NPD` (non-positive-definite)
flag per atom, which reads `F` for all five: the ADP matrices themselves are positive
definite, so it really is their transformed *errors* that go bad.

#### ROOT CAUSE FOUND AND FIXED (2026-07-30) — `long/urea_rhf_STO-3G_HAR`

Probing the pre-`sqrt` variances in `VEC{ATOM}:get_ADP2s_in_ADP2_principal_axes_in`, scaled by
1e20 because `stdout.show`'s 4-dp default hid everything (the real variances are ~1e-6, so at
the default format *every* value printed as `0.0000` — rescale before concluding anything):

| atom | component | variance |
|---|---|---|
| 1 (O) | `U_xy` | **+4.0e-23** |
| 3 (C) | `U_xy` | **-1.3e-23** |
| 1, 3 | `U_xz`, `U_yz` | exactly 0 |
| any | `U_xx`, `U_yy`, `U_zz` | ~1e-6 |

The bad variance is **seventeen orders of magnitude** below the genuine ones on the same row.
Oxygen's noise landed positive (`sqrt` → 6e-12, printed `0.000000(0)`); carbon's landed
negative (`sqrt` → NaN, printed `0.00000(0)`). Same computation, same magnitude, opposite sign
— the platform decides. That is the entire defect.

**Why those components are special, stated correctly.** In this frame each atom's ADP tensor has
been diagonalized, so the off-diagonal *values* are zero by construction. Their *variances* are
**not** required to be zero — they measure how well determined the principal-axis *directions*
are. In urea, O and C sit on the 2mm axis, so their axes are symmetry-fixed and the true
variance is zero (hence the round-off); N1 lies on a mirror plane and keeps orientational
freedom, so its off-diagonal variance is a real 3.8e-7 (~18% of its diagonal ones) and its
`0.000000(8)` esd is **physically meaningful**. Any fix must keep N1's and kill O's and C's.

**The fix** (in both `get_ADP2s_in_ADP2_principal_axes_in` and `get_ADP2s_in_new_axes_in`):

```
tol = 1.0d-9*maxval(abs(dU))
DIE_IF(any(dU<-tol),"negative variance in the rotated ADP2 covariance matrix")
where (dU<tol) dU = ZERO
dU = sqrt(dU)
```

The clamp is **symmetric** — it zeroes round-off-level *positive* variances too, so the printed
esd no longer depends on which side of zero the noise fell, i.e. it is platform-independent.
That also removes the last of the "zeros not aligned" formatting difference: O and C now both
print `0.00000(0)`, while N1 correctly keeps `0.000000(8)`. The relative tolerance does the
discriminating. Both routines lost their `PURE` attribute, because `DIE` is live in release
(`USE_ERROR_MANAGEMENT`) where `PURE` is real; they run once per printed table, so the cost is nil.

**Second bug found in the same place (independently spotted by Dylan).** In
`get_ADP2s_in_new_axes_in` — the routine already carrying `! WARNING: PROBLEM WITH THIS ROUTINE?
Dylan 25/11/2024` — the loop wrote `rcm.put_diagonal_to(dU(n,1:6))` where `n` is set to `.dim`
*before* the loop and never incremented. So every iteration overwrote the **last** row and rows
`1..dim-1` were never assigned at all, then square-rooted as uninitialised heap. The tell is the
inconsistency within one loop: `U` is indexed by `a`, `dU` by `n`. The sibling principal-axes
routine gets away with `n` only because it does `n = 0` / `n = n+1`. Fixed to `dU(a,1:6)`.

**Still open — a second, independent site.** `short/urea_lamaGOET_grown_CIF` still emits 4
`negative esd` warnings, and they come from the **`put_cif`** path, not from the ADP2
transformation fixed here (they appear immediately after `keyword found --> put_cif`). Same class
of defect, different route; note the earlier finding that `make_CIF_esds` is never called for
these tests, so the CIF esds come from the atom's stored `.pADP_errors`. Pick that up next.

**Naming hazard noted (Dylan):** the local `rcm` in these routines is a *rotated covariance
matrix* (it is the result of `rotated_U2_covariance_mx_for_atom`), but `rcm` conventionally reads
as *reciprocal cell matrix*. 30 occurrences across the ADP2/3/4 analogues; worth renaming to
`rcov`, but not folded into this correctness fix.

**(Superseded) Next step:** instrument the ADP error transformation into that axis system (the producer of
the table headed `#  ID  U_xx  U_yy  U_zz  U_xy  U_xz  U_yz` with the `/A^2` subheading, which
in the run's `stdout` immediately follows the probe output). Print the pre-`sqrt` quantity for
each component; expect a small negative for the symmetry-zero ones. The fix is then
two-legged: clamp at the transformation (`sqrt(max(v,ZERO))`) *and* trap at creation with a
`DIE` — note `WARN` is debug-only, so a release build would still print `(0)` silently
(see the `USE_ERROR_MANAGEMENT` note above).

**Open question — the warning is debug-only.** `WARN`/`WARN_IF` are gated on
`USE_PRECONDITIONS` (`macros.in:281`), which release builds do **not** define (only
`USE_ERROR_MANAGEMENT`, which gates `DIE`). So the new NaN/negative-esd warning **compiles to
nothing in a release build**: a production run silently prints `(0)` for a NaN esd. That is
defensible for a programmer precondition, but this is a statement about the **validity of
numbers written into a CIF** — possibly a published one. Options if that is judged wrong:
promote it to something always-on (not `WARN`), which changes output and forces a re-bless; or
leave it debug-only and rely on the upstream fix. Decision deferred, deliberately.

**ROOT CAUSE FOUND (2026-07-29) — `sqrt` of a negative variance.** `atom.foo:2144`
`set_pADP_errors_to(covariance_mx,H_U_iso)`:

```fortran
covariance_mx.put_diagonal_to(dX)
dX = sqrt(dX)          ! <- unguarded: negative variance -> NaN
```

A variance cannot be negative, but if the least-squares variance-covariance matrix is **not
positive definite** its diagonal can come out slightly negative, and `sqrt` then returns NaN.
That NaN flows into `.pADP_errors`, into the ADP esds, into the CIF, and was printed as `(0)` —
a meaningless number wearing the clothes of a confident one. The two symptoms are one mechanism:
NaN where the `sqrt` is applied, plain negative esds where it is not.

**Guarded (symptom only):** `dX = sqrt(max(dX,ZERO))` plus a `WARN_IF(any(dX<ZERO),...)`, so the
esd becomes a visible zero rather than a silent NaN. **This does not fix anything scientific** —
a negative variance still means the refinement's v-cov matrix is wrong.

**AGREED DIRECTION (Dylan): the check belongs at the least-squares / covariance stage, as a
`DIE`, not a warning here.** Two reasons it is the right call:

- **Severity.** A non-positive-definite v-cov matrix means the refinement did not produce a
  usable error model. Continuing yields esds that are not merely imprecise but meaningless, and
  they end up in a CIF that may be published. Failing loudly is correct.
- **Visibility.** `WARN`/`WARN_IF` are gated on `USE_PRECONDITIONS` (`macros.in:281`), which
  **release builds do not define** — so any warning is a no-op in production. `DIE` is gated on
  `USE_ERROR_MANAGEMENT`, which release *does* define. So a `DIE` actually fires where it
  matters; a `WARN` never would.

So the guard added in `atom.foo` should be regarded as a stop-gap at the wrong layer. The real
check is upstream, where the covariance matrix is formed — test positive-definiteness (or at
minimum a non-negative diagonal) there and `DIE` with a message naming the refinement.

**Still open — the actual question: why is the matrix not positive definite?** Candidates: a
singular or near-singular normal-equations matrix being inverted; an over-parameterised
refinement (more parameters than the data supports); accumulated rounding in the inversion; or a
genuine bug in the accumulation. A debug build with `-fcheck=all -ffpe-trap=invalid,zero,overflow`
should trap at creation. Note `-ffast-math` is on in release, so NaN behaviour there is not
dependable.

**The earlier trace below was of the WRONG BRANCH — kept as a caution.** `crystal.foo:7842-7844`
chooses between two `put_CIF` overloads, and these tests take the one *without* a covariance
matrix, so `make_CIF_esds` is never called for them (verified: a probe there produced no output
at all in either test). The esds come from the atom's stored `.pADP_errors` instead. The mapped
path below is real code, but it is not the path these failures take.

**THE (WRONG-BRANCH) PATH, kept for reference.** Traced 2026-07-29 (static reading, no probe yet):

```
least squares  ->  .xray_data.covariance_mx            (MAT{REAL}, the v-cov matrix)
                        |
crystal.foo:8231        |  per-atom diagonal block, transformed:
                        |  covariance_mx(af:al,af:al).back_transform_to(C(af:al,af:al),T(1:n,1:n))
                        v
crystal.foo:8236   C.put_diagonal_to(esd)               <- esd = DIAGONAL of the transformed v-cov
                        |
crystal.foo:8038   atom.put_CIF_ADP2(...,esd)
vec{atom}.foo:1002 put_ADP2_errors_to(dU,fac,esd)       <- pure copy: dU(a,k) = esd(k+3+…)
                        |                                   then dU = fac*fac*dU
                        v
                   TABLE_COLUMN:set_values_and_errors(r,e)  <- where e_nan / e_neg were measured
```

**Note what `esd` actually is: the diagonal of a covariance matrix.** There is no `sqrt` in this
path — `put_diagonal_to` takes the diagonal straight out of `C`. So:

- a **negative** esd means the transformed covariance diagonal is negative, i.e. `C` is **not
  positive semi-definite** — either `covariance_mx` itself is not, or `back_transform_to`
  is losing it. This is Dylan's diagnosis, structurally confirmed.
- a **NaN** esd means a NaN is already in `covariance_mx`, or is produced by the transform.

`T` is built from `.unit_cell.inverse_mx` and the reciprocal matrices via
`GAUSSIAN_DATA:symmetric_tensor_{2,3,4}_product_mx` (`crystal.foo:8208-8221`); it is 34x34,
zeroed, with only the used sub-blocks filled, so an atom whose `no_of_pADPs` reaches into an
unfilled region would transform through zero rows — that yields zeros, not NaN, but is worth
checking while there.

**The next probe (one run localises it):** print, per atom block, the counts of NaN and negative
entries in `covariance_mx(af:al,af:al)` **before** the transform and in `C(af:al,af:al)`
**after** it, in `make_CIF_esds` (`crystal.foo:8179`). That says immediately whether the defect
is inherited from the least squares or manufactured by `back_transform_to`. Use `stdout` (NOT
`std_err` — see the unit-collision item), and the NaN test `x/=x` worked under the current
release flags.

**Then:** if it is inherited, chase `covariance_mx` back into the least-squares/normal-equations
inversion; a debug build with `-fcheck=all -ffpe-trap=invalid,zero,overflow` should trap where
the NaN is *created* rather than where it is printed. Remember `-ffast-math` is on in release,
so NaN behaviour there is not dependable.

**Older notes on where to look (superseded by the map above):**

1. Is the NaN/negative already present in the covariance matrix **diagonal**, or introduced by
   the transformation into the ADP basis? Print the diagonal before and after.
2. If it is in the diagonal — is it a `sqrt` of a negative, a `0/0`, or an uninitialised
   element? A debug build with `-fcheck=all -ffpe-trap=invalid,zero,overflow` should trap at the
   point of creation rather than at the point of printing.
3. Note `-ffast-math` is in force in release builds, so NaN-producing operations are *not*
   guaranteed to behave predictably; the debug build is the right place to chase this.

**Caveat on the NaN test itself:** `e/=e` is the standard NaN check but `-ffast-math` permits
the compiler to assume no NaNs and fold it away. It demonstrably *worked* with the current flags
(the probe found the NaN), but do not rely on it silently — verify it still detects when flags
change. The guard in `get_dp_de_le` uses `NOT (abs_error>ZERO)` partly for this reason.

### Superseded detail: negative ESDs (kept for the record)

**Found 2026-07-29** while tracking down `urea_lamaGOET_grown_CIF`. Instrumenting the ADP
columns showed that, of the five atoms, **two carry tiny *negative* esds** in the U13/U23
columns (`e_neg = 2`, `e_tiny = 2`, alongside two that are exactly zero):

```
DBG prec ....... 5      DBG e_neg ...... 2      DBG e_zero ..... 2
```

An esd is a square root and cannot be negative. **Dylan's reading: this comes from the least
squares — the variance-covariance matrix is not right, either a genuine error in its
construction or undefined behaviour.** Noise pushing a diagonal element slightly below zero
would do it, but that should be understood rather than clamped.

The *formatting* consequence has been fixed (`REAL:get_dp_de_le` now takes `abs(error)` before
`log10`, which was previously invalid for a negative argument and gave platform-dependent
decimal counts — 6 on Linux/x86, 2 on macOS/arm64). **That fix makes the output well-defined;
it does not make the esds correct.** The underlying negative variance is still there and is the
thing to chase.

Where to start: the ADP esds reach the CIF writer via `put_ADP2_errors_to`, which scales values
taken from `.xray_data.covariance_mx`. Worth checking whether the negatives are present in the
covariance matrix diagonal itself or are introduced by the transformation into the ADP basis,
and whether a debug build with `-fcheck=all`/`-ffpe-trap` catches anything in that path.

Examples so far:

| Test | Difference | Likely cause |
|------|-----------|--------------|
| `urea_ccsd_pob-TZVP_Salvador_properties` | Salvador charges/dipoles ~0.5% (`0.1984`→`0.1974`, `-0.3959`→`-0.3956`), 3rd–4th sig-fig | grid integration / partition numerics; longstanding, pre-ANTLR |
| `h2o_rhf_cc-pVDZ_tdhf` | TDHF response, rel ~0.12% — just under the 0.2% gate (ulp already ~10) | time-dependent HF response; eigensolver / BLAS ordering across runners |
| `nh3_rhf_DZP_HAR` | a near-zero value: ~10% *relative* but ~1 ulp — passes only via the last-digit bound | relative metric amplified near zero (cf. the `ylid` case) |

**Stopgap in place (CI stability):** `scripts/suite_report.py` carries a documented
`KNOWN_MARGINAL` table that widens the loose bound for just these tests (`tdhf`: rel ≤ 0.5%;
`nh3_rhf_DZP_HAR`: last-digit ≤ 4) so the badge stops flickering, while the strict 0.2% / 2 gate
stays for every other test — and the report prints a footnote naming which tests were relaxed.
It is a **workaround, not a fix**: the goal is to *remove* entries from that table by
understanding each discrepancy. (Salvador is not in the table; its ~0.5% is accepted with the
reference pinned to the produced values.)

NOTE (Salvador): verify this is NOT the moments-staleness knock-on from setting `.atomic_moments_made`
(the flag now suppresses moment re-making that release always did). If a targeted
`.atomic_moments_made = FALSE` reset after SCF convergence restores the reference values, it
IS the knock-on and should be fixed rather than accepted. See memory `debug-ensure-vs-release`.

## Deferred: debug-build (`-O0`) test failures — floating-point boundary artifacts

> **FIXED & COMMITTED — `process_CSD_cif` (#113) fragment-offset `int()` flip (2026-07-26).**
> The worst debug failure — the `Fragment offset` `int()` boundary flip (`0.999.../1.000...` truncating
> to different integers under `-O0` vs `-O3`, cascading a whole lattice-vector shift, 189% rel) — is
> **fixed at the source**. Root cause: `int(mean_column_vector)` truncates toward zero with its knife-edge
> exactly on the integers, where a fragment centre can land. Fix: a **toward-zero nudge**,
> `int(pos*(ONE-TOL(8)))`, at all **6** offset sites (`cluster.foo` 132/163/529, `crystal.foo`
> 979/10329/10363). This shrinks the centre by `1e-8` (≫ the ~1e-14 `-O0/-O3` reassociation gap,
> ≪ the 1e-6 coordinate resolution) so both optimisation levels resolve the boundary to the **same**
> integer, while every non-boundary fragment keeps its exact `int()` value.
>
> Rejected alternative: `nint()` (round-to-nearest). It also makes `-O0`==`-O3` but changes the
> *recentering convention* for **every** fragment (e.g. mean 0.616: `int→0` but `nint→1`), churning
> ~624 lines across many cluster tests. The nudge is surgical.
>
> **Verified:** both trees rebuilt 0-error; on `process_CSD_cif` **release output == debug output**
> (0 substantive diff — the `-O0/-O3` disagreement is gone) and release is deterministic run-to-run.
> The `stdout` reference was re-blessed from the release build: it changed only by **one boundary
> fragment** (offset `0 1 0`→`0 0 0`, a pure lattice relabelling; `iodos.cxc` is unchanged — the offset
> cancels in absolute coords). No other test moves. Committed (source `cluster.foo`, `crystal.foo`;
> reference `process_CSD_cif/stdout`).
>
> This fix does **not** address the other 4 debug failures (47, 64, 87, 91) — the small-FP / esd-token /
> convergence-divergence cases in the table below, still open.

**Context (re-measured 2026-07-17).** A clean **debug** build (`gfortran-14`, `-O0 -g`, ENSURE
preconditions live) compiles 0-error and runs the full suite at **119/124** (up from 116/124
on 2026-07-15 — the same-day release is 124/124). Every remaining failure was checked against
the **release** binary (`-O3`), and **release reproduces the reference exactly** for all of
them. So **none is a translator bug or a crash** — the debug build ran every job to completion
with no ENSURE aborts. The *only* variable is optimisation level: `-O3` FP contraction /
reassociation produces sub-ulp numeric differences that, at boundary cases, **flip a discrete
decision** in the source. Debug's real job — surfacing crashes and precondition violations —
passed clean.

**Changes since 2026-07-15 (net +3):** two ADP-label cases (`L_alanine` #65, `YLID` #69) were
cleared by Dylan adding `show_IAM_output=FALSE` / `show_IAM_results=TRUE` to their `stdin` —
this suppresses the per-cycle ADP tables where the near-equal `Uxx`/`Uyy` label flips, while
keeping the final refined values compared (exactly the "suppress the line, keep the meaning"
approach below). `cyclazine` #5 was fixed at the source (see the DONE section above). Against
that, `urea…Salvador` #47 **entered** the debug failure list: its reference was updated to the
`-O3` values, so `-O0` now differs from the reference by the longstanding ~0.5% grid amount.
Net: 8 − 4 (65, 69, 5, 72) + 1 (47 gained) = 5 failures; #113 then fixed at the source
(nudge, above) = **4 failures** (47, 64, 87, 91).

**Proven mechanism** (was `#113`, now fixed — kept as the canonical illustration of the class).
`foofiles/cluster.foo:132` — `.fragment_offset = int(crystal.fragment_geometry.mean_column_vector)`.
`int()` truncates the fragment-centre mean toward zero; when that mean sits on a unit-cell boundary,
`-O3` yields e.g. `1.0000001 → 1` while `-O0` yields `0.9999999 → 0`. Both are crystallographically
valid (differ by a lattice vector) but print differently. The site now carries the `*(ONE-TOL(8))`
nudge (above), closing this instance; the remaining ADP-label and count cases below are the same
class (selecting/counting among near-equal or near-threshold values).

**Current debug failures (4; #113 fixed 2026-07-26, others measured 2026-07-17):**

| # | Test (category) | Substantive diff (ref → debug) | Class |
|---|-----------------|--------------------------------|-------|
| 64 | `ylid` (rgbi) | last bond-analysis columns drift ±0.1 (e.g. `74.02`→`73.90`); worst is `0.04`→`0.05` = 20% *relative* on a near-zero value | FP noise amplified by relative metric near zero |
| 87 | `urea_rhf_DZP_consistent-cluster-charge_HAF` (long) | 1-ulp last digit (`-349.2012`→`-349.2013`) + one column 1 char wider | last-digit rounding + auto-width threshold |
| 91 | `yq28_H_U_iso_IAM_refinement` (long) | identity matrix width `1.0000`→`1.000` (numbers identical); **also** two extra lines `Warning … crystal data already defined!` / `xray_data is already defined` not in the reference — release does not print them, so investigate before assuming FP (may be an ENSURE/precondition path live only in debug, or a junk-filter gap) | auto-width threshold + unexplained debug-only warnings |
| 47 | `urea_ccsd_pob-TZVP_Salvador_properties` (short/long) | Salvador charges `0.1984`→`0.1974`, `-0.3959`→`-0.3956`, dipoles ~3rd–4th sig-fig (~0.5%) | longstanding grid/partition numerics (see Salvador section); reference now pinned to `-O3` |

**Cleared since 2026-07-15** (kept for the record): #65 `L_alanine` and #69 `YLID` ADP-label
flips (fixed via `show_IAM_output=FALSE` in their `stdin`); #5 `cyclazine` No.-of-doubles
(source fix `b3b50dd2`); #72 `gly_ala_fragHAR` (now passes debug too); #113 `process_CSD_cif`
fragment-offset `int()` flip (source fix — `*(ONE-TOL(8))` nudge, 2026-07-26; see the block above).

**Goal (Dylan): make the *debug* tests pass**, probably by **suppressing the offending
output line(s)** in the comparison — but not so much that the test loses meaning. Notes on the
options, to think through:

- **Targeted output suppression (Dylan's lead).** Each failure is one or a few identifiable
  lines: the `Fragment offset` line, the ADP component *label* token, the `No. of doubles`
  count, the auto-width columns. Adding these to `prefixes_to_ignore` (or a per-test ignore
  list) in `scripts/test.py` makes debug green while keeping the numeric substance compared.
  Risk: suppressing a *label* or a *count* removes a genuinely meaningful field — prefer
  suppressing only the specific line, per-test, not the whole table.
- **`test.py` near-zero `abs_tol`.** Only helps the pure-numeric near-zero case (`ylid`
  `0.04` vs `0.05`). Does **not** fix the discrete label/offset/count flips (those are text).
- **Source hardening (higher value, more invasive).** Replace knife-edge `int()` /
  component-selection / auto-width thresholds with a small-epsilon-tolerant form so `-O0` and
  `-O3` agree at boundaries. This has real **portability** value — a boundary that flips
  between `-O0` and `-O3` could also flip between compilers/platforms even in *release* — but
  it edits hand-written scientific `.foo` and must be done per-site (start `cluster.foo:132`).

To reproduce: clean `gfortran-14` debug build (`-DCMAKE_BUILD_TYPE=debug`), `ctest`, then
`diff tests/<suite>/<test>/stdout tests/<suite>/<test>/stdout.bad` for each failure (the loose
harness writes `stdout.bad` on a fail). The five listed above are all that remain as of
2026-07-17; the raw diffs from the original 2026-07-15 run lived in that session's scratchpad
(`debug_analysis/`) and are not preserved across sessions.

---

# Translator and the Foo language

## Cleanup: normalise procedure-name CASE across definition and call sites

**Goal (Dylan):** find every procedure whose **definition case differs from its call-site
case** (or where call sites disagree among themselves) and make them consistent. Foo/Fortran
are case-insensitive so these compile and run fine, but the inconsistency is annoying and
trips case-sensitive tooling.

**Why it matters (concrete):** discovered during phase B (dead-code elimination). `textfile.foo`
defines `reset_IO_status` (upper `IO`) but `vec{basis}.foo` calls it as `stdin.reset_io_status`
(lower). The dead-code analysis keyed its call-graph nodes case-sensitively, so the call didn't
match the definition and the procedure was wrongly pruned — a latent, silent trap. (Worked
around in the translator by lower-casing the method part of every graph node via `node()`;
this cleanup would remove the underlying inconsistency in the *sources*.)

**How to tackle (parse-tree driven, reuse phase-B infra):** the translator already walks every
`ProcDef` (definitions) and every `PostfixContext` (calls). Add a `--case-report` mode that
records, per lower-cased procedure name, the **set of distinct spellings** seen across its
definition header and all call sites; flag any name with >1 spelling, listing file:line of each
variant. Then normalise — the definition's spelling is the natural canonical form — and rewrite
the call sites (a targeted, parse-tree-driven edit like `--add-self-intent`, NOT a blind sed,
so commented-out and string-literal occurrences are left alone). Related: [[submodule-call-autoresolution-done]]
already hit a case bug in the submodule registry (commit 627db872); this is the same family.

## Future task: introduce Fortran-2008 `submodule` constructs

**Goal (Dylan):** use real Fortran-2008 `submodule` where appropriate. **Concept clash to note
first:** a Foo "submodule" (e.g. `molecule.base.foo` → `module MOLECULE.BASE`) currently
translates to a **separate, standalone Fortran module** `MOLECULE_BASE_MODULE`, `use`d like any
other — NOT an F2008 `submodule`. F2008 `submodule (PARENT) NAME` would instead let the 19
`MOLECULE.*` pieces share one parent interface and break the `use`-graph coupling (a submodule
sees its ancestor's specification without a `use`, and changing a submodule body doesn't force
recompilation of the parent's users). Investigate whether mapping Foo submodules onto F2008
submodules simplifies the emitted module graph and compile-time dependencies. Touches
`emit()`/`buildUseFile()`/`buildInterfaceFile()` and the module-naming scheme.

## Column-0 `#ifdef`/`#endif` inside a program body — benign parser diagnostic

**Date:** 2026-07-28. A release build prints, at ~98%:

```
[ 98%] Generating run_mpi_test_complete.F90
line 234:0 extraneous input '<EOF>' expecting {END, USE, 'interface', ... }
```

**Cause.** `runfiles/run_mpi_test_complete.foo` (and `run_mpi_test.foo`) wrap their executable
body in a **column-0** `#ifdef MPI ... #endif` that sits between the indented `program` body and
the final `end program`. Foo block scoping is **indentation-based**; the grammar passes
`#include`/macros through pre-CPP but does **not** model column-0 `#if*`/`#endif` directives, so
the indent/dedent tracking can't cleanly match the closing `end program` and the parser reports a
spurious EOF expectation at the true end of file.

**Impact — benign.** ANTLR error-recovers and still emits valid Fortran (`run_mpi_test_complete.F90`
ends correctly and the executable links). Only these two MPI test harnesses use the pattern; no
`foofiles/` module, `tonto`/`hart`, or `ctest` is affected. Not a regression — the files predate
current work.

**Fix (deferred).** Teach the Foo lexer to treat CPP directive lines
(`#if`/`#ifdef`/`#ifndef`/`#else`/`#elif`/`#endif`) as passthrough that does not perturb
indentation/scoping. Low priority. (Separately, 2026-07-28: `run_mpi_test_complete.foo` had its
variable declarations moved *inside* the `#ifdef MPI` so they don't trigger unused-variable
warnings in a non-MPI debug build; this does not remove the parser diagnostic above.)

---

# hart

## hart — deferred items

The `hart` work (milestone 5) left these alone deliberately. `docs/RUNNING_HART.md` is
the authoritative document; these are the items with no owner yet.

- **fragHAR support (milestone H1).** `hart` only ever calls `HAR_refinement`
  (`runfiles/run_har.foo`), never `fragHAR_refinement`, and has no
  atom-group/per-fragment-charge path, so crystals with more than one molecule
  in the asymmetric unit cannot be refined with it.
  `tests/long/gly_ala_fragHAR_rhf_STO-3G` exercises fragHAR through `tonto` and
  is the acceptance test for this. **It is a hookup, not a repair** — fragHAR
  itself works in `tonto` today and reproduces the last known-good 2019 output
  to 4 significant figures (archaeology and table in `docs/RUNNING_HART.md` §6). It was
  broken 2020-01-23 (`f0d7cfd3`) and fixed by `d840e322`, which came in with the
  `hart` work. Do **not** try to revive `.cif.use_fragments` — that flag is dead
  and `.crystal.data.refine_fragments` superseded it correctly.
- **Frozen options.** `--charge`, `--mult`, `--ldtol`, `--scf-guess`,
  `--anharm`, `--wavelength` and `--4th-order-only` are commented out in both
  the `select case` block and the help text of `run_har.foo`. They are kept in
  step deliberately — the invariant check (`scripts/check_hart_options.sh`)
  compares only the uncommented labels against the live `--help` output, so a
  half-revived option is caught. Reviving one means uncommenting *both* halves.
- **Hard-coded `stdout.*` scratch names.** A HAR job writes seven plot files
  whose names ignore the job name entirely: `stdout.F_z_vs_stl`,
  `stdout.Delta_F_vs_stl`, `stdout.F_z_vs_F_exp`, `stdout.Delta_F_pred_z_vs_F_pred`,
  `stdout.Delta_F_pred_z_vs_stl` (`foofiles/diffraction_data.put.foo`), and
  `stdout.QQ_plot_with_hkl`, `stdout.QQ_plot.gunplot`
  (`foofiles/vec{reflection}.foo`). Two runs in one directory overwrite each
  other's plots. They should derive from `<job>`. Also `.gunplot` is a typo for
  `.gnuplot` — the file's own header says "Gnuplot input file".
- **`.cif2` restart round-tripping is untested.** `hart` now accepts a `.cif2`
  input (it previously rejected one while its own message said it was
  required for restart), but nothing exercises the write-then-restart cycle.
- **`--scf-guess` needs rewriting, not restoring.** The block that used to
  implement it in `run_har.foo` was fatal — see the comment there. It ran
  unconditionally (`guess` is always `"mos"`), and
  `SCF_DATA:set_initial_density("restricted")` does not mean "guess a restricted
  density", it rewrites `.initial_density` to `"r"`, i.e. *read* one from an SCF
  archive. On a first run no archive exists, so the SCF read garbage. It also
  threw away the promolecule guess set on the line above, and its two branches
  were swapped against their own names. Reviving the option means writing it
  correctly against `SCF_DATA`'s actual semantics.
- **`put_archive` / `read_archive` are asymmetric about the normalise
  qualifier.** After the fix below, `put_archive <name> normalise` takes it
  inline on the keyword line, while reading needs the separate
  `read_archive_and_normalise` keyword. Both work; they just do not look alike.
  Unify if that grates — either give `put` its own
  `put_archive_and_normalise`, or let `read_archive` take the inline qualifier
  by relaxing its `ENSURE(n_line_items==3)` to accept a fourth item.

## Runfiles that are not built, and were not migrated to `--options`

The single-dash option removal covered every `runfiles/*.foo` that CMake
actually translates. These eight have **no `add_executable` target and are not
in the translated source list** in `CMakeLists.txt`, so they are never compiled
and a mistake in them would not be caught by any build:

`run_xtal.foo`, `run_cif_to_surface.foo`, `run_dnc.foo`,
`run_metal_fingerprints.foo`, `run_command_line.foo`,
`run_fix_pre_v4088_cif.foo`, `run_compare_data.foo`,
`run_compare_3d_pair_data.foo`.

Several still carry single-dash `case` labels (e.g. `run_xtal.foo` is a near
copy of `run_molecule.foo` with `case("i","-input")`), which `COMMAND_LINE`
can no longer deliver. Either revive them — translate, build, migrate — or
delete them. `run_cif_to_surface.foo` was the CrystalExplorer entry point;
CrystalExplorer no longer uses tonto.

---

# RGBI and the picture toolchain

## LONG TERM: fold the picture toolchain into one integrated codebase (Dylan, 2026-08-09)

**The ask, in his words:** *"review and rewrite different tools based on these
ecosystems, into one integrated codebase"*, prompted by finding the RGBI
requirements *"extremely convoluted"*. They are, and the convolution is real
rather than a failure of setup — worth stating plainly, because the instinct is
to blame oneself for it.

**What is actually in the chain**, and what each piece contributes:

| Piece | Ecosystem | What it does that nothing else does |
|---|---|---|
| `obabel --gen2d` | C++ | Generates a **2D depiction layout** from 3D coordinates |
| `mol2chemfig` | Python (port of an abandoned Perl tool) | Turns a `.mol` into `chemfig` markup |
| **Indigo** | C++ library, Python binding | Molecule parsing / aromaticity perception, for the above |
| `chemfig` + TikZ | TeX | Draws it |
| `pdflatex`, `pdfcrop`, `gs` | TeX + PostScript | Renders and crops |
| two `bash` scripts | shell | Glue, with no error handling until 2026-08-09 |

Five ecosystems, two of them effectively unmaintained, held together by shell.
The failure modes measured on 2026-08-09 are the predictable consequence: a
`pipx` venv silently orphaned by an OS Python upgrade; `command -v` reporting a
program that cannot execute; every LaTeX run redirected to `/dev/null`; and the
whole pipeline exiting **0** having drawn nothing.

**The observation that makes consolidation tractable.** Tonto already knows the
molecule, the bonding, the atom labels and every index it wants to print. The
*only* thing it does not have is a **2D depiction layout**. So this is not
"rewrite five tools" — it is "acquire one algorithm, then emit the picture
directly", which is exactly the move stage 2 of `docs/PLOT_PLAN.md` already made
for the HAR plots: Tonto computed the label placement itself instead of hoping
gnuplot had an algorithm for it (it does not).

**And half of it is already done, which is the proof of concept.** The dial
diagrams are TikZ emitted straight from Tonto's own numbers — no Open Babel, no
Indigo, no mol2chemfig — and they reproduce the committed reference PDFs exactly.
That half of the toolchain has *already* been integrated, and it is the half
that never breaks.

**Options, in increasing order of ambition:**

- **A — emit `chemfig`/TikZ directly from Tonto.** Drops Open Babel, Indigo and
  mol2chemfig; keeps LaTeX, which is doing real typographic work and is the one
  dependency a chemist already has. Needs a 2D layout in Foo. *Recommended
  starting point.*
- **B — emit SVG directly.** Drops LaTeX too, so the picture appears with no
  external program at all. Loses chemfig's typography and its ring/bond
  conventions, which are not trivial to reproduce.
- **C — keep the pipeline, replace the bash glue with one program.** Cheapest,
  and removes the silent-failure class, but leaves five ecosystems to install.
  This is roughly what R1-R3 of the 2026-08-09 restoration achieve already.

**Measure before choosing** (CLAUDE.md §2). The open question is how good
`obabel --gen2d` actually is on the molecules that matter, because that sets the
bar option A has to clear. Take the `tests/rgbi/` set, render each layout, and
look. A ring-template plus force-directed refinement is a well-understood
algorithm and is likely enough for drug-like molecules; if it is not, option A
is much more expensive than it looks.

**Sequence:** after the workshop. The doctor (`scripts/rgbi_doctor.sh`) and the
container (`scripts/docker/rgbi.Dockerfile`) make the status quo survivable in the
meantime — which is the point of doing them first. Related in spirit to the
CLAUDE.md long-term item on re-engineering in a language with first-class
parallelism: both are cases of replacing "invisible failure held together by
glue" with "checked constructs".

### The open questions, which must be researched before any of it is decided

Dylan, 2026-08-09: *"It's a complex decision that needs discussion and research
and planning."* Agreed — the layer to replace is genuinely not obvious, and the
instinct to blame the oldest tool is not evidence. Recorded as questions rather
than as a plan:

1. **Is there a good programmatic alternative to `chemfig` at all?** Dylan likes
   it and is not aware of one. The candidates to actually evaluate are RDKit's
   drawing code, Indigo's own renderer (already installed here, and never used
   for drawing), OpenSMILES-based web renderers, and plain hand-emitted SVG.
   The bar is high: chemfig's ring/bond conventions and its typography are the
   part a chemist notices.
2. **Is Indigo maintained?** Open question, and material: it is a C++ toolkit
   with a Python binding, and it arrives here only as a *dependency of*
   `mol2chemfigPy3`. Check release cadence and the state of Apple-silicon
   wheels before building anything further on it.
3. **Which end goes first?** Two defensible answers, and they point opposite
   ways. Replace the *front* (Open Babel + Indigo + mol2chemfig) and keep
   chemfig — Tonto emits chemfig markup itself, needing only a 2D layout
   algorithm. Or replace the *back* (chemfig + LaTeX) and keep the chemistry
   toolkits — Tonto emits SVG and no TeX is needed at all. The front is the
   fragile half in practice; the back is the half with no alternative in sight.
4. **How good is `obabel --gen2d` really?** This is the measurement that decides
   question 3: it sets the bar any replacement layout has to clear. Render every
   `tests/rgbi/` case and look.

### Smaller, and self-contained: the dial grid's column count is hard-coded

`ROBY:put_dial_table_do_H` / `_no_H` (`foofiles/roby.foo:7090`, `7143`) hard-code
**four columns** in three places each — `ceiling(0.25d0*n_bonds)`, `min(l+4,...)`
and `\begin{longtable}{cccc}`. Four dials need about 520pt; `article`'s default
`\textwidth` is about 345pt, so the fourth column fell off the page and `pdfcrop`
cut it. That is now worked around in `rgbi-dial-header.tex` by giving the page a
large canvas (the crop means paper size does not survive into the result), which
fixes the clipping with no rebuild.

The proper fix makes the *table* fit the *page* instead: emit the dials as a
flowed sequence of boxes separated by breakable glue, and let LaTeX fit as many
per line as the width allows and break pages itself. That deletes the column
count entirely rather than choosing a better one — no width probing, no
arithmetic in Foo, and it adapts to any dial scale or paper size. Costs a
`roby.foo` rebuild and re-blessing the committed reference PDFs, which is why it
is here and not done.

Dylan's reason for wanting the grid at all, worth preserving: *"to have a quick
scan to see if there were any unusual bonds — so separate files would be no good
for this kind of visual survey."* Any redesign has to keep the one-page survey.

---

## RGBI: known defects and rough edges

Moved out of `docs/RUNNING_RGBI.md` on 2026-08-10, when the user-facing pages
were cleared of developer material. None is fixed.

**In the program**

- **`rgbi --help` calls the program `run_rgbi`**, which is the CMake target name
  rather than what gets installed, and points at a `./rgbi-script` folder, which
  is spelled `rgbi-scripts`. Both cosmetic. `hart` has an invariant check
  comparing `--help` against its option `case` labels
  (`scripts/check_hart_options.sh`); `rgbi` has three options and no such check.
- `CMakeLists.txt:883` pins a file to `-O2` because **"rgbi/BN's Roby
  populations were wrong"** at other optimisation levels. Read that comment
  before touching optimisation flags for this program. No test guards it — see
  the macOS-in-CI item below.

**In the pictures**

- **The dial grid's column count is hard-coded to four**, in three places per
  routine (`ROBY:put_dial_table_do_H`, `foofiles/roby.foo:7090`). Four dials
  need ~520 pt and `article`'s default `\textwidth` is ~345 pt, so the fourth
  column fell off the page and `pdfcrop` cut it — visible in the committed
  reference PDFs. Worked around in `rgbi-dial-header.tex` by giving the page a
  large canvas.
- Nothing compares the reference PDFs automatically; they are eyeball targets.

**Reference material not in the repository**

The Grabowsky chapter PDF is deliberately not checked in — 1.1 MB of binary. On
`sauce` it is at `~/rgbi-reference/Jayatilaka_2025_Grabowsky_chapter.pdf`.

---

# Tooling and editor support

## Editor: improve vim highlighting of Foo and vim integration

**Goal (user):** improve the vim editing experience for `.foo` sources — better syntax
highlighting and tighter editor integration. The repo already ships some vim support
(`.vim/filetype.vim` maps `*.foo` and `macros` to a `foo` filetype; `scripts/fix_tags.pl` and
`scripts/cscope_setup` support ctags/cscope navigation — kept for exactly this reason).

**To investigate / do:**
- **Syntax file:** review/extend the `foo` syntax highlighting to cover the current language —
  reverse declarations (`var :: TYPE`), parameterized types (`VEC{T}`, `MAT{T}`…), pointer/
  allocatable suffixes (`*`, `@`), procedure headers with `::` attributes (`PURE`,
  `ELEMENTAL`, `get_from(...)`, `selfless`), `KEY?` template placeholders, the constants
  (`TRUE`/`FALSE`/`ZERO`/`ONE`/`NULL`), and comments (`!`). Confirm whether a `syntax/foo.vim`
  exists and is up to date, or author one.
- **Indentation:** Foo uses 3-space indentation to mark scope (closed by `end`); an
  `indent/foo.vim` that follows this would help.
- **Navigation:** verify `scripts/cscope_setup` + `scripts/fix_tags.pl` still produce usable
  tags/cscope indexes for `foofiles/` and `runfiles/`, and document the workflow.
- **Integration niceties (optional):** a command/`makeprg` to translate the current `.foo`
  with `FooToFortran` and jump to errors; folding on scope; matchit for `... end` blocks.

---

# Platform-specific

## OPEN: long paths to the basis sets fail -- STR is 256 characters

**Reported for the Windows `tonto.exe` and `hart.exe` (untested binaries from
the release workflow), suspected to be a forward/backslash problem. IT IS NOT.
Reproduced on LINUX on 2026-08-14 with a long path, so no Windows box is needed
to work on it.**

    path length = 544
    short path : exit=0   -74.963358
    long path  : exit=1   Error in VEC{BASIS}:read_library_data ...
                          no library basis set

### Cause

`STR_SIZE` is 256 (`include/macros.in:57`), so a Foo `STR` is
`character(256)`. The basis directory is read as

```foo
call get_environment_variable(BASIS_LIBRARY_ENV_NAME, basdir, status=i)
if (i/=0) basdir = " "
```

and the Fortran standard sets `status = -1` when the value is **too long for
the variable supplied**. So a path over 256 characters is truncated, reports
-1, is then blanked by that line, and the code falls back to `./basis_sets` --
which usually does not exist, giving "no library basis set" with no mention of
a path problem.

Three call sites, i.e. every program that takes a basis directory:
`runfiles/run_har.foo:763`, `runfiles/run_molecule.foo:132`,
`runfiles/run_xtal.foo:122`.

Windows is where it surfaced because paths there are routinely deeper
(`C:\Users\...\AppData\...`), not because of anything Windows-specific. There
is no path-separator handling anywhere in `foofiles/`, and none is needed:
Windows accepts forward slashes in its file APIs.

### History: BSTR was 8192, and shrinking it was reasonable

`git blame` settles this. Peter Spackman introduced `BSTR` in Dec 2015
(`542350e7`) with `STR_SIZE 512` and **`BSTR_SIZE 8192`** -- a genuine "big
string". Both were cut to 256 on 2024-11-25 (`f2c47977`, "Tidied TEXTFILE,
slurp stdin? ...").

**That was defensible.** `BSTR_SIZE` is the JOB-FILE LINE BUFFER:
`buffer.foo:258` declares `item :: STR(len=BSTR_SIZE)` and `buffer.foo:23`
warns "The buffer string is of length BSTR_SIZE". Shrinking an 8192-character
per-line buffer while moving to slurping stdin is exactly the right call, and
256 is ample for a job-file line.

So do **not** move paths onto `BSTR`: that is the buffer's size, reduced
deliberately. The error was that ONE constant was serving two requirements that
differ by an order of magnitude.

### The real scope: COMMAND_LINE is worse than the environment variable

    command           :: STR   ! character(256)
    command_arguments :: STR   ! EVERY argument, concatenated into one string
    command_optarg    :: VEC{STR}@

`command_arguments` accumulates all arguments joined together
(`command_line.foo:134`), so it overflows well before any single path does; and
`command_optarg` being `VEC{STR}` caps each individual option value -- including
`--basis <path>` -- at 256. So the command-line route truncates independently of
`TONTO_BASIS_SET_DIRECTORY`, which is why the report named both programs.

### Two things to fix, and they are separate

**1. Give paths and the command line their own size.** A `PATH_SIZE` (4096
matches Linux `PATH_MAX`; Windows extended paths reach 32767), used by
`COMMAND_LINE`'s `command`, `command_arguments` and `command_optarg`, and by the
three `get_environment_variable` sites. `BUFFER` keeps `BSTR_SIZE` at 256,
untouched. Raising `STR_SIZE` globally is the blunt alternative and would cost
memory across the many `STR` members in `types.foo` for a problem in a handful
of places.

**2. Truncation must FAIL LOUDLY.** Independently of any size,

    if (i/=0) basdir = " "

treats "the value was truncated" (-1) exactly like "the variable is not set"
(1), silently falling back to `./basis_sets`. Whatever size is chosen, a path
that does not fit must DIE naming the variable and the length. This is the same
silent-fallback shape as the DFT defects above, and it is why the failure
surfaced as "no library basis set" with no mention of a path at all.

### Testing

The reproduction is a shell script away and needs no Windows: build a directory
tree deeper than 256 characters, point `TONTO_BASIS_SET_DIRECTORY` at it, and
assert the job still runs. That belongs in the suite alongside
`check_single_atom_scf.py` -- it is the same kind of coverage gap, in that no
existing test uses a long path.

Worth noting in `docs/BUILDING_ON_WINDOWS.md` too, since that page collects the
Windows traps and this is the one users will hit first.



## HIGH PRIORITY: put macOS in CI, with a badge (Dylan, 2026-08-09)

**It is feasible, and it is free.** `dylan-jayatilaka/tonto` is a **public**
repository, and GitHub gives public repositories unlimited standard-runner
minutes — macOS included, where a private repo would be billed at 10×. So the
only cost is wall-clock. This was established while adding
`.github/workflows/ci-rgbi-macos.yml`, which already runs the RGBI picture
toolchain on `macos-14`; the same machinery extends to Tonto itself.

**Why it is high priority rather than nice-to-have.** `macos-14` runners are
**arm64**, which is exactly the platform where `shell1quartet.F90` is
miscompiled. That miscompilation cost 36 tests (82/124 → 118/124 once the file
was pinned, `a3ec1b07`), and produced *wrong numbers rather than crashes* — the
diverging boron atomic SCF and rgbi/BN's populations were symptoms of it, not
independent defects. **That pin is currently guarded by nothing.** Delete it, or
have a compiler upgrade change what the flag means, and the suite goes quietly
wrong on every Mac while Linux CI stays green. A macOS job is the only thing
that can see it. Note `CMakeLists.txt:883` carries a second such pin for the
same reason (rgbi/BN's Roby populations), likewise unguarded.

**What it needs:** `brew install gcc cmake openjdk` (the translator needs a JDK;
BLAS/LAPACK already default to Homebrew OpenBLAS on macOS rather than
Accelerate, whose LAPACK is frozen at 3.2.1 from 2009). Expect 40-70 minutes for
a build, which is why the suite should be `ctest -L short`, not all 124.

**Do not expect green on the first run, and do not bless it away.** At the last
real-Mac measurement (2026-07-28, M2 Pro, macOS 26.5.2) the full suite was
118/124 with **6 residual failures**, and the leading explanation — non-unique
eigenvectors within degenerate eigenspaces, so anything using individual
eigenvectors rather than the subspace projector is ill-defined — is still a
hypothesis with an unexecuted next step (relink against Accelerate from the
intact `.o` files and A/B the `theta` tables). So:

1. Start it as a **scheduled, non-gating** workflow, exactly like
   `ci-rgbi-macos.yml`: separate file, no `push` trigger, no
   `continue-on-error`. A red Mac badge is information.
2. Use it to gather repeated evidence on the 6 failures, which one Mac session
   could not.
3. **Only then** add the badge to `README.md` and promote it to a gate.

The stale README line ("many failures on the Apple M2 — not recommended") should
be corrected at the same time; it is wrong about the build, which was always
clean.

## UNVERIFIED: the `textfile.foo` MPI fix has not seen the serial suite (2026-08-26)

**Do this before the `textfile.foo` change goes anywhere near `master`.** Full detail, the
exact commands and the baseline: `docs/TONTO_AND_MPI.md` Finding 7, "VERIFICATION STILL OWED".

`move_to_record_external` and the two record movers were changed to position the file on the
I/O rank and broadcast the result. `foofiles/textfile.foo` is on the path of **every file read
in Tonto**, so the change is exercised by essentially every job, serial included -- but only
**three** have been run: the MPI reproducer at `-n 2` (still fails, `MPI_ERR_TRUNCATE` -- the
fix removed an amplifier, not the origin), and `-n 1` plus serial on
`urea_read_and_process_CIF` alone, which pass exactly.

Baseline to match, from `CLAUDE.md` §5: **124/124** loose on the full release suite locally,
**51/51** short in CI. Pass `--failure-dir` so an ERRORing job records its cause.

Why it is not merely paperwork: the edit moved a `DIE_IF` out of a loop into its caller and
added an `exit` on `.IO_status/=0` -- a control-flow path that did not exist before. A loop
whose terminating check moved is exactly the kind of change that turns a loud failure into a
quiet one. The expectation is that serial behaviour is unchanged, because the new guard is a
no-op in a serial build; **that expectation is the thing to test, not to assert.**

## Transpose and thin the CI badge table (Dylan, 2026-08-26)

Do this as its own piece of work, in its own session. It is a README/docs change only --
no workflow edits -- so it needs no build and no CI run.

**Transpose.** The README table is platforms as rows, build types as columns. Swap them:
build types as **rows**, platforms as **columns**. Transposing does not reduce the cell
count -- 3 platforms x 4 build types is 12 either way -- but it fixes the growth
direction. Platforms are stable at three (Linux, Windows/WSL, macOS) and have not changed
in years; **build types are what keeps growing** -- release, debug, MPI, and now MPI-debug
(see the entry above), with `release-static` and `fast` also existing. A markdown table
grows badly sideways: a fourth column makes the README scroll horizontally on GitHub and
is unreadable on a phone. A fourth row costs nothing.

**Thin, which is the part that actually matters.** A badge means *"a live signal worth
watching"*. Four of the nine current cells are not that: the three macOS cells say
"not running yet" (the workflows are not on `master`, so `schedule:` never fires),
WSL-MPI is a dash, and **WSL-debug carries a badge and has never had a green run**. Twelve
badges would be mostly grey, and a wall of grey teaches a reader to ignore all of them --
including Linux-release, which is the gate and the one that must never be ignored.

So split the two questions the one table is currently being asked:

- **`README.md`** -- "is Tonto healthy?" A small table of badges that are genuinely live
  signals. Linux-release above all.
- **`docs/TONTO_CONTINUOUS_INTEGRATION.md`** -- "what is covered where?" The full
  platform x build-type matrix including the not-yet-running cells, where completeness is
  the point and a grey cell is informative rather than noise.

**Do not** simply delete the unbadged rows: "macOS release is not tested in CI" is a fact a
reader deserves, it just does not deserve README real estate. And keep the existing rule
that a badge and its workflow triggers are changed together -- a badge pointing at a
workflow that never runs is how WSL-debug got into its current state.

## Add a PARALLEL DEBUG build to CI, on every platform (Dylan, 2026-08-26)

There is a debug job and an MPI job on Linux, and neither is a **debug MPI** job. Nothing
in CI, on any platform, builds `-DMPI=1 -DCMAKE_BUILD_TYPE=debug`. That is the gap this
entry closes.

**Why, with the evidence that prompted it.** Chasing the deterministic
`urea_read_and_process_CIF` desync (Finding 7 in `docs/TONTO_AND_MPI.md`), the release MPI
build reported:

```
Error on rank 1: TEXTFILE:move_to_next_record ... error opening new file urea.cif
```

— a misleading message (that routine opens nothing) about damage **41 broadcasts downstream**
of the divergence. The *debug* MPI build, first run, reported:

```
Error on rank 1: CIF:move_to_end_of_data ... CIF file has not been opened
```

which names the divergent state at the site. The difference is `ENSURE`, which is gated on
`USE_PRECONDITIONS` and compiled out of every optimised build. Six CI cycles on release
produced less than one local debug run. **Preconditions are the instrument that makes an MPI
desync legible, and no CI job compiles them together with MPI.**

**Scope.** `ci-mpi.yml` (Linux) first — it already builds Open MPI from source and caches it,
so a second job reusing that cache is cheap. Then macOS, where `mpifort` and `gfortran` come
matched from Homebrew and no source build is needed. WSL last, and only if the WSL-release and
WSL-debug jobs are green first.

**Shape it like `ci-debug.yml`, not like a suite run.** Build, then a couple of fast jobs at
`-n 2` to prove the binary executes and the ranks stay in step. A full numeric suite under
debug MPI would be slow and would inherit the `-O0` failures already in `DEFERRED.md`.

**Three cautions.**

- **`ENSURE` can itself contain a collective.** `TEXTFILE:is_open` performs a
  `PARALLEL_BROADCAST_IO`, so `ENSURE(.file.is_open,...)` broadcasts. A precondition evaluated
  on some ranks and not others *is* a desync — so a debug MPI build can fail in ways release
  never would. Expect to triage that before the job is green, and do not read a debug-only
  failure as proof of a release bug.
- **On gfortran-16 the debug build omits `-fcheck=bounds`** (the compiler bug in
  `docs/GFORTRAN16_DEBUG_CRASH.md`), so it checks less than a gfortran-14 debug build. Pin the
  job to gfortran-14 if bounds checking is the point; use 16 if matching the MPI toolchain is.
- **Do not badge it until it has been green once.** `ci-wsl-debug.yml` has a badge and has
  never had a green run.

Related: the badge table cannot absorb another column — see the note in that entry.

## Future task: verify the macOS build (Apple Silicon / Tahoe)

**Goal (Dylan):** confirm whether Tonto builds and passes tests on current macOS, on
**Apple Silicon (M2) with macOS Tahoe (26)** — to be done in a **separate session on a real
Mac** (the main dev box is Linux; macOS cannot be tested from it). A March-2026 README note
claimed *"many failures on the Apple M2 with Tahoe 26.3 — not recommended"*, but Dylan says
that is **very old** and likely stale, so it must be re-checked rather than trusted. Build via
Homebrew (`brew install gcc cmake openjdk python3 gnuplot`; BLAS/LAPACK come from
`Accelerate.framework`), then run `make report`. **Feed the result back into the docs:** update
the README macOS line (currently softened to "via Homebrew; Linux/WSL is the reference platform")
and the `Building on MacOS` wiki page to say what actually works — pass → "supported", or list the
specific failures if any remain.

**RESOLVED (2026-07-28, session on the real Mac: M2 Pro, macOS 26.5.2 Tahoe, Darwin 25,
gfortran-14 / Homebrew GCC 14.3.0, CMake 4.3.3, Java 26).**

**Outcome: the macOS failures were a compiler miscompilation, not a Tonto bug.** gfortran 14.3
on arm64 miscompiles `shell1quartet.F90` (the two-electron integral code) at `-O3`. Pinning
that one file to `-O2 -fno-schedule-insns` (`CMakeLists.txt`, commit `a3ec1b07`) took the suite
from **82/124 to 118/124**:

| Suite | before | after |
|---|---|---|
| short | 33/51 | 49/51 |
| rgbi | 1/13 | 12/13 |
| long | 18/28 | 25/28 |
| cx | 30/32 | 32/32 |
| **total** | **82/124** | **118/124** |

The build itself was always clean, so the March-2026 README note ("many failures on the Apple
M2 — not recommended") is stale in both respects. **README and wiki still NOT updated** —
Dylan's standing instruction; and the 6 residual failures should be understood first (they are
mostly the longstanding small-difference cases already tracked below, plus one structural diff
in `urea_lamaGOET_grown_CIF` with max rel 0 / LDD 0).

The investigation history is kept below because the false trails are worth not re-walking.
**Note especially:** the diverging boron atomic SCF that dominates the "atomic SCF" section
above was a *symptom* of this miscompilation, not an independent defect — with the compiler
workaround in place boron converges and rgbi/BN reproduces the reference exactly. The `DIE`
added in `make_ANOs` is still worth keeping as a safety net, but its motivating example is gone.

- **The build is clean.** Full translate + compile + link, zero errors; only benign warnings
  (clang deployment-version override ×149, a `-F` flag not valid for Fortran, 2 macro
  redefinitions). So the 2026-03 "not recommended" note is stale *as regards building*.
- **`make report`: 82/124 loose** (exact 73, lastdig 79) — short 33/51, rgbi 1/13, long 18/28,
  cx 30/32. Baseline artefacts kept in `/tmp/tonto-mac-baseline/` (tests.log, build log,
  all 44 `stdout.bad`).
- **What passes:** geometry/CIF processing and Hirshfeld surfaces (cx 30/32), and plain SCF
  energies (`h2o_rhf_cc-pVDZ`, `blyp`, `rks_B3LYPG`, `xalpha`, `aug-cc-pVDZ`) — all *exact*.
- **What fails:** downstream *property* evaluation — 1e properties, structure factors / HAR,
  cluster charges, Roby bond indices, polarisabilities/TDHF, Salvador properties.
- **Ruled out (with evidence, not argument):**
  - *Not* the ANO occupancy threshold (`atom.foo`, `count(.NAO_occupations>=1/14)`): scanning
    `occupied_ano_cutoff=` over 0.02–0.30 changes BN not at all (the marginal occupation is
    ~0.3, nowhere near 1/14).
  - *Not* uninitialised heap: identical results under `MallocScribble=1 MallocPreScribble=1`.
  - *Not mainly the LAPACK version.* Accelerate's Fortran LAPACK is frozen at **3.2.1 (2009)**
    (confirmed via `ilaver`); switching to Homebrew OpenBLAS (**3.12.0**) flipped exactly
    **one** short test (33/51 → 34/51) and **zero** rgbi (1/13 either way). Worth doing as
    hygiene — now the macOS default, see `CMakeLists.txt` — but it is not the cause.
- **Leading hypothesis — degenerate eigenspaces.** In rgbi/BN the Roby angle table has an
  exactly degenerate pair (states 7 and 8, both `theta = 68.7340°` — the π_x/π_y pair).
  Eigenvectors within a degenerate eigenspace are **not unique**: any orthogonal mixing is a
  valid eigenbasis, and different LAPACKs return different mixtures. Anything downstream that
  uses individual eigenvectors, rather than the projector onto the whole subspace, is then
  **basis-dependent, i.e. ill-defined** — which explains differences far too large for
  round-off. Strongly supported by the rgbi pattern: every failing molecule is linear/highly
  symmetric (BN, C2, CN-, CO, F2, N2, NF, O2, Ni-carbonyl — all with degenerate π), while the
  **only exact pass is `CHFCl`, the one molecule with no symmetry and no degeneracy**. Three
  environments give three different answers for BN's B population: Linux 6.48, Accelerate 6.16,
  OpenBLAS 6.34.
  **Next step:** relink an Accelerate binary from the same objects (`.o` files are intact) and
  A/B the `output_theta_info= YES` tables. If the *thetas* (eigenvalues, invariant) agree while
  the populations differ, non-invariance under degenerate mixing is confirmed — and the fix is
  in the Roby code, not the build.
- **Still uncontrolled:** `-Ofast` implies `-ffast-math`; and the `release/` tree used here
  carried a **stale cached `-O2`** (a fresh configure gives `-O3`), so opt level differed from
  the Linux reference too. A strict-FP rebuild (`-O2 -fno-fast-math -ffp-contract=off`) is the
  clean way to settle the FP-flags question.
- **Also note:** a `1e_properties` diff shows *total* molecular moments agreeing to 7 digits
  while *atomic partitioned* charges/dipoles differ by 15–17% — totals right, partitioning
  wrong. Consistent with the same "non-invariant use of an arbitrary basis" theme.

### RESOLVED: the oxygen atom converged to a variationally impossible energy

The single best probe found so far, because it is one atom, runs in seconds, and its
correctness can be judged from physics alone rather than by diffing against a reference.

Run `tests/short/oxygen_atom_uhf_cc-pVDZ/stdin` **verbatim**:

| | Total energy |
|---|---|
| Linux reference (`tests/short/.../stdout`) | **−74.7923** |
| this Mac, OpenBLAS 3.12.0 | **−77.6178** |
| this Mac, Accelerate 3.2.1 | **−77.6178** |

Both report "converged". The O atom UHF limit is ≈ −74.81, so **−77.62 lies ~2.8 Ha *below* the
variational limit — impossible for a correct HF calculation.** This is not a tolerance or
round-off effect and not a LAPACK effect (both libraries agree exactly); it is a Mac↔Linux
difference in the SCF itself, so the prime suspects are code generation and `-Ofast`
(`-ffast-math`, FMA contraction, denormal flush-to-zero).

The same signature appears in every standalone atom tried on this Mac (B, C, N, Al, Si all come
out 2–6 Ha too low), and in the *guess* SCF inside BN (its N atom gives −55.7673 where the HF
limit is ≈ −54.40). So it may well underlie a large share of the 42 macOS failures — including
the atomic-partitioning discrepancies — and it is upstream of the ANO/Roby problem rather than
caused by it.

#### Localised: two-electron integrals between *different shells*

Bisection done 2026-07-28 against the Linux reference box (`achari2`, the very machine that
produced the reference outputs — its kernel `6.8.0-88-generic` matches their `Platform:` line).

**Ruled out, each with a direct test:**

| Suspect | Result |
|---|---|
| FP flags | **No** — rebuilt `-O2 -fno-fast-math -ffp-contract=off`: identical (−77.6178, virial 1.9574) |
| LAPACK | **No** — Accelerate 3.2.1 and OpenBLAS 3.12.0 agree exactly |
| Platform `#ifdef`s | **No** — `GNU_gfortran_on_Darwin` is defined but never used anywhere |
| Basis setup / normalisation | **No** — overlap matrix **bit-identical** to Linux, diagonal exactly 1.0 |
| One-electron integrals | **No** — T and V_eN **bit-identical** to Linux (see below) |
| ERI screening | **No** — setting all six `eri_*_cutoff` to 1e-20 changes nothing |
| Initial guess / tolerances | **No** — see the table above |

**Where it actually is.** `V_ee` is not computed directly; `molecule.scf.foo:1868` obtains it *by
subtraction* (`energy − V_NN − V_eN − V_charge − T`), so a `V_ee` discrepancy *is* a total-energy
discrepancy. With V_NN = 0 for an atom and T, V_eN identical, the error is entirely in the
two-electron part of `.SCF_energy`.

**Minimal reproducer — Be atom, STO-3G, milliseconds:**

| Atom (STO-3G) | shells | Mac V_ee | Linux V_ee | T (both) |
|---|---|---|---|---|
| He | one s | 1.055713 | 1.055713 | 2.823526 |
| **Be** | **two s (1s,2s)** | **4.538672** | **4.875821** | 14.844185 |
| Ne | s + p | 54.602059 | 55.508976 | 125.562006 |
| O | s + p | 28.349706 | 29.075205 | 73.444959 |

A **single** shell is exact; **two distinct shells** diverge. So it is not an angular-momentum
(p/d) problem — it is two-electron integrals spanning different shells. The Mac value is always
*too small*, as if contributions are lost.

*Why the densities still agree:* in a minimal basis these atoms have every orbital occupied
(Be: 2 functions / 4 electrons), so the density is fixed by the basis regardless of the Fock
matrix. That is why T and V_eN match while the energy does not — it does **not** imply the
Fock matrix is correct.

**Next step:** dump the individual two-electron integrals for Be/STO-3G on both platforms and
diff them — there are only ~6 unique values. `put_fock_matrix` / `put_density_matrix` print
empty under direct SCF, so this needs a temporary print inserted in the ERI path
(`shell1quartet.foo`, `shell2.foo`, `gaussian4.foo`). Since the one-electron code shares the
same Gaussian machinery and is provably correct, the fault is specific to the ERI routines.

**Repro harness:** `scripts/oxygen_scf_probe.sh <tonto> <repo>` prints the energy decomposition
at truncated iteration counts on either platform.

#### Final: compiler miscompilation of `shell1quartet.F90`

Localised by swapping a single object file: with `shell1quartet.F90` compiled at `-O2` and
everything else untouched, every probe matches Linux exactly (oxygen E/V_ee/V_eN/T/virial;
Be V_ee 4.875821; BN's Roby populations identical to the reference). The path mattered because
only the *engine* ERI route is affected — `use_spherical_basis= T` selects `make_r_JK_direct`
and was an accidental working workaround.

**Evidence it is a GCC bug rather than UB in our source** (not proof — see caveats):

- The generated Fortran is **bit-identical** to Linux's (same md5, 11263 lines), so the
  translator is not implicated.
- Compiles clean under `-Wall -Wextra -Waliasing`: **no warnings at all**.
- Runs clean under `-fcheck=all -finit-real=snan -finit-integer=-999999` with
  `-ffpe-trap=invalid,zero,overflow` armed in the main program: no bounds violations, no
  runtime errors, no traps.
- The one concrete UB candidate — `Jab`/`Jcd` aliasing when `ab == cd`, both `INTENT(INOUT)` —
  is **properly guarded**: every branch in which `same` can be true writes `Jab` only and never
  references `Jcd` (`if (same) ... Jab = Jab + TWO*factor*ev`). Conforming.

*Caveats:* the instrumentation covers this file only (bounds checking is per compilation unit),
`-fcheck` cannot detect argument aliasing at all, and — importantly — the checks necessarily run
on a *correctly* compiled build, since adding them changes codegen enough to make the answer
right. So this shows the source is well-defined when compiled conservatively; it is not a proof
that the failing binary contained no UB.

**No minimal `-O3` workaround exists** (tried, failed): the trigger is not any of the 13 passes
`-O3` adds over `-O2`, and disabling all 13 still gives the wrong answer; nor is it the
vectoriser (`-fno-tree-vectorize`, `-fvect-cost-model=very-cheap`), inlining
(`-fno-inline-functions`, `--param max-inline-insns-auto`), `-fno-ipa-cp-clone`, or
`-fno-strict-aliasing`. There appear to be **two interacting triggers**:

| config | result |
|---|---|
| strict FP, `-O2` | wrong; `-fno-schedule-insns` fixes it |
| fast-math, `-O2` | correct, even with scheduling on |
| fast-math, `-O3` | wrong; `-fno-schedule-insns` does NOT fix it |

Hence the level is pinned rather than a pass disabled. Both switches are applied because that
is the configuration the 118/124 run actually verified.

**Not fixed in GCC 16 (tested 2026-07-29).** A full build with `gfortran-16` (Homebrew GCC
16.1.0) and `-DTONTO_SKIP_ARM64_WORKAROUNDS=ON` compiles cleanly (zero errors) and then
reproduces the miscompilation **exactly** — the invariant check fails with values identical to
GCC 14.3's, digit for digit:

| case | correct | GCC 14.3 | GCC 16.1 |
|---|---|---|---|
| Be | −14.3518804762 | −14.6890291293 | −14.6890291293 |
| O | −73.8041502333 | −74.5296496920 | −74.5296496920 |
| Ne | −126.6045249968 | −127.5114422775 | −127.5114422775 |
| N | −54.1053903978 | −55.0573624742 | −55.0573624742 |

Consequences:

- **Do not version-gate the workaround** — it is needed on both 14.3 and 16.1.
- **Beware the default compiler.** Homebrew's main `gcc` formula is now 16.1.0, so plain
  `gfortran` on this Mac *is* GCC 16; only CLAUDE.md's documented
  `-DCMAKE_Fortran_COMPILER=gfortran-14` pins 14. Both hit the bug, and the workaround covers
  any GNU compiler on arm64 Apple, but a developer's "default" build is not the documented one.
- **The two versions producing bit-identical wrong answers is worth weighing.** It is what you
  would expect from a persistent target-specific backend bug, but equally from source UB that
  both versions exploit the same way, so it does not settle the question — it only shows the
  behaviour is deterministic rather than a random codegen accident.

**Compiler versions in play (they were never harmonised, which hid this):** Linux `release/`
(the reference binary) used **plain `gfortran` = GCC 13.3.0**, Linux `build-rel/` and GitHub CI
use **gfortran-14**, and this Mac used **14.3 and now 16.1**. The committed reference `stdout`
files predate all of it and do not record what produced them — which is why the run banner now
stamps compiler and LAPACK version (`CMakeLists.txt` → `macros.in` → `molecule.main.foo`,
ignored by `scripts/test.py`).

**Left to do:**
- Decide the harmonisation: pick one reference compiler across Mac, Linux and CI. Worth doing
  with data — check whether GCC 16 reproduces the *same suite numbers* before switching, since
  otherwise the choice is between re-blessing references and staying put.
- Measure the runtime cost of `-O2` on this file (it is the ERI hot path). If negligible, stop.
- Consider a GCC bug report. The ingredients are unusually strong: bit-identical source, two
  platforms disagreeing, and a self-validating oracle (an SCF energy below the variational
  limit, virial 1.957 vs 2.000). Would need reducing to a minimal test case first.
- Re-test whether the *global* `-fno-schedule-insns` in `cmake/SetFortranFlags.cmake` is still
  needed now that the file is pinned — currently kept only because it was in the verified run.
- **Add a regression test**: for an s/p-only basis, `use_spherical_basis= T` and `F` must agree
  (the two bases are mathematically identical below d functions). That single invariant would
  have caught this immediately, on one machine, with no reference file.

## DIAGNOSED (2026-08-25): gfortran-**16** DEBUG builds SEGFAULT — BOTH platforms

> **Authoritative page: [`docs/GFORTRAN16_DEBUG_CRASH.md`](docs/GFORTRAN16_DEBUG_CRASH.md)**
> — cause, machine-level evidence, reproducer, workaround, and what is still owed.
> Draft upstream report: [`docs/GFORTRAN16_GCC_BUG.md`](docs/GFORTRAN16_GCC_BUG.md).

**RESOLVED 2026-08-25 — it is a gfortran 16 compiler bug in `-fcheck=bounds`, not a
Tonto defect and not memory corruption.** For a bounds-checked subscript reached
through an allocatable component chain, gfortran 16 copies the array descriptor into
one stack temporary and emits the check reading *another* temporary that is only
written later in the same statement. The check reads uninitialised stack memory, and
either faults on a nonsense address or reports a bounds violation that is not real.
`-fcheck=bounds` is debug-only, which is exactly why gfortran-16 **release** was
always fine.

Found by disassembling `POINTGROUP:make_character_table` and observing that the only
*write* to the two slots the faulting check reads is 400 bytes further on. Reduced to
`scripts/gfortran_bounds_bug.f90` (97 lines, no Tonto): gfortran-14 fine both ways,
gfortran-16 fine at `-O0`, SIGSEGV at `-O0 -fcheck=bounds` on x86_64. Confirmed in
Tonto by recompiling `pointgroup.F90` alone without the flag — the segfault goes, the
run reaches "Making gaussian ANO data …", and the next site of the same shape
(`atom.F90:7058`, `self%NOs%r(:,n)`) then reports a *false* violation, bounds `(0:0)`
read out of the garbage descriptor.

**Fix:** `cmake/SetFortranFlags.cmake` omits `-fcheck=bounds` from `DEBUG_FLAGS` on GNU
Fortran 16 and up, announcing it at configure time; `-DTONTO_FORCE_FCHECK_BOUNDS=ON`
puts it back for retesting. `scripts/check_gfortran_bounds_bug.py <compiler>` tests a
compiler. gfortran 15 is untested — neither machine has it — so the gate is on `>= 16`,
which is what was measured.

**This retires the heap-corruption hypothesis below, and the reasoning that produced
it.** The two crash sites are two *statements* of the same shape, each independently
miscompiled — not two innocent victims of one corrupting write. The reduced cases
"failed to reproduce" because they were built without the descriptor-temporary
trigger. ASan on macOS was never going to report a stack-slot ordering error inside
compiler-generated code. The `VEC{OBJECT}` fix (`d8b94cbf`) was a real conformance
defect worth landing, but was never related to this.

**Confirmed end-to-end the same day.** A whole-tree gfortran-16 debug build on arm64
macOS compiles clean, runs `h2o_rhf_STO-3G` to **exit 0** (139 before) with
`Total energy -74.9658`, and takes `ctest -L short` **62/62**.

**Still owed:** filing the upstream report — it needs a Bugzilla login and there is
no account. The draft and the duplicate search are in `docs/GFORTRAN16_GCC_BUG.md`.

**A side observation from that run, not chased.** The two `short` tests that fail in
the gfortran-14 **release** build on this Mac — `urea_ccsd_pob-TZVP_Salvador_properties`
(2.99%) and `h2o_rhf_cc-pVDZ_tdhf` (0.231%) — both **pass** in the gfortran-16 debug
build. `ci-macos.yml` attributes the first to macOS; this says it is not intrinsic to
the platform or the test. Two variables differ between those builds, compiler *and*
optimisation, so it does not isolate a cause. The cheap experiment is a gfortran-14
**debug** build: if they pass there too, the culprit is `-Ofast`, not the compiler.

---

*Historical record from 2026-08-24 and earlier follows. Its conclusion is wrong; it is
kept because the measurements are sound and the reasoning is instructive.*

**Use `gfortran-14` for debug builds, on any platform.** `gfortran-16` release is fine on both
platforms; its *debug* build segfaults on **both** arm64 macOS and x86_64 Linux — the "only on
arm64" in the original note was an untested assumption, disproved 2026-08-24 (see progress note
below).

| build | any SCF job |
|---|---|
| macOS, gfortran-**16**, debug | **SIGSEGV** (exit 139, ~2 s) |
| macOS, gfortran-**14**, debug | **runs to completion** — verified `h2o_rhf_STO-3G` and `urea_rhf_STO-3G_HAR` |
| macOS, gfortran-16, release | fine (119/124) |
| Linux, gfortran-16, release | fine (123/124) |

**NOT a regression, and not caused by our changes** — I first wrote this up as a regression on the
strength of the note below saying the debug build "ran every job to completion" on 2026-07-17.
That note does **not record its platform** (it says only `gfortran-14, -O0 -g`), and since all work
before 2026-07-28 was on the Linux box, it was almost certainly Linux. **Dylan spotted this.**
So arm64-macOS debug was simply an **untested configuration**, and gfortran-14 debug works there
today — which rules out today's commits entirely.

*Consequence for the compiler migration:* the earlier verdict "the gfortran-16 switch is
numerically free" was established for **release only**, on both platforms. It does **not** extend
to debug on arm64. Standardising on 16 therefore needs this caveat, or the crash fixed.

**Symptoms** (gfortran-16.1.0, `-DCMAKE_BUILD_TYPE=debug`, tree `debug/`):

| test | result |
|---|---|
| `short/h2o_rhf_STO-3G` (plain SCF) | **SIGSEGV**, exit 139, ~2 s |
| `long/urea_rhf_STO-3G_HAR` | **SIGSEGV**, exit 139 |
| `short/nh3_rhf_DZP_HAR` | **SIGSEGV**, exit 139 |
| `short/urea_read_and_process_CIF` (no SCF) | exit 0, fine |

So it correlates with running an SCF, not with the ADP/esd code.

**Ruled out:**
- **Stack overflow** — raising `ulimit -s` from 8 MB to 64 MB does not help.
- **The `shell1quartet.F90` `-O2` pin** — that per-source option applies in *every* build type, so
  in debug that one file is `-O2` while the rest is `-O0`. Recompiling it at `-O0` and relinking
  still segfaults. (The pin *should* still be gated to release configs for debuggability — see
  CLAUDE.md §8 — but it is not the cause.)

**Not yet diagnosed.** `lldb` cannot attach in this environment ("attached to process, but could
not pause execution"), and `-fbacktrace` prints raw addresses only, so there is no symbolic
backtrace yet. Options, cheapest first:

1. **Compiler-specific — ANSWERED: yes.** gfortran-14 debug runs the same jobs to completion on
   the same commit, so the fault is in gfortran-16's debug (`-O0`) codegen on arm64.
2. To localise it: rebuild the gfortran-16 debug tree with `-fcheck=all` (currently only
   `-fcheck=bounds`) — `-fcheck=pointer` may turn the segfault into a Fortran runtime error
   naming a line. Note `lldb` cannot attach in this environment, and `-fbacktrace` yields raw
   addresses only, so a symbolic backtrace needs either `atos` with the load slide or a
   different debugger.
3. Worth checking whether a Linux gfortran-16 debug build crashes too (build started; result
   pending) — that tells us whether it is arm64-specific or general to gfortran-16.
**Progress 2026-08-24, on a real arm64 Mac.** Four things established, two of
which contradict what was written above.

**a. It was masked by a separate bug that stopped the build entirely.** Before any
segfault could happen, gfortran-16 debug failed to *compile*: `PLOT_GRID:volume`
and `pixel_volume` (added by `cc530a34`, the serial Bader port) were declared
lower-case `pure` while calling `PURE` routines. Not compiler-specific -- CI
(Linux-debug) had been red on it since 2026-08-23, five runs, and gfortran-14
rejects the same file here. Fixed in `97d8b0e9`; the rule is now written up in
`docs/FOO_GRAMMAR_DOCUMENTATION.md` ("PURE is contagious upward").

**b. A symbolic backtrace IS obtainable**, contradicting "there is no symbolic
backtrace yet" above. `lldb` still cannot attach and `-fbacktrace` still prints
raw addresses, but **macOS writes its own symbolicated crash report** to
`~/Library/Logs/DiagnosticReports/tonto-*.ips`. Parse the JSON payload:

```
EXC_BAD_ACCESS, KERN_INVALID_ADDRESS at 0x00000000000000d9
  make_pg_image_of_shell  <- crash
  symmetrize_1 / symmetrize_0
  make_anos_for_atom / make_anos / make_anos_and_interpolators
  make_promolecule_density_mx / make_promol_guess_mos
  get_initial_density_mx / get_initial_guess / initialize_scf
  usual_scf / scf
```

`0xd9` is a field access off a null base. This is why only jobs with an SCF
crash: the fault is in building the **promolecule initial guess**.

**c. It is NOT the architecture-tuning flag.** This was the leading hypothesis --
debug applies `-mtune=native` (via `HOST_FLAG`) while release deliberately avoids
`native` in favour of `-mcpu=apple-m2`, an asymmetry nobody had pointed at.
Rebuilt with `-DTONTO_ARCH_FLAG=none`, which removes it: **still exit 139**.
Refuted. (The asymmetry is still worth removing -- a debug build should carry no
CPU tuning at all -- but it is not this bug.)

**d. Control, clean.** Same commit, same flags, same machine:
gfortran-14 debug runs `h2o_rhf_STO-3G` to **exit 0**; gfortran-16 debug gives
**exit 139**.

**e. It is NOT arm64-specific, and it is NOT a macOS problem.** This is the
question item 3 below left pending, now answered: a gfortran-16 **debug** build
on achari2 (Ubuntu 24.04, x86_64) segfaults on the same job, exit 139. The entry
above said "only (as far as tested) on arm64"; that was untested, not true.

**f. Linux gives the backtrace macOS cannot**, with line numbers, for free:

```
#3  make_character_table   at pointgroup.F90:1066
#4  update                 at pointgroup.F90:324
#5  create_1               at pointgroup.F90:71
#6  make_anos_for_atom     at molecule.scf.F90:5779
#7  make_anos / make_anos_and_interpolators / make_promolecule_density_mx
```

`pointgroup.foo:1018`, inside `POINTGROUP:make_character_table`:

```foo
   do i = 1, .n_irrep
      .irrep(i).character.create(.order)
      do n = 1, .order
         .irrep(i).character(n) = .irrep(i).mx(:,:,n).trace   ! <- here
```

So it reads `.irrep(i).mx` for an irrep whose `mx` is **not allocated**. The
macOS crash is the same class one level out -- `make_pg_image_of_shell` reading
`.pointgroup.mx(:,:,n)`, `KERN_INVALID_ADDRESS at 0xd9`, a field access off a
null base -- and both are reached from `make_anos_for_atom`, building the
promolecule guess for a single-atom molecule.

**So the working conclusion is that this is a latent Tonto bug, not a gfortran-16
codegen bug**: unallocated pointgroup data that gfortran-14 tolerates and
gfortran-16 dereferences. That is a much better prognosis than a compiler bug --
it can be fixed here. It is not yet proven; what is proven is the crash site, on
two platforms, and that the arch flag is not involved.

**Do the diagnosis on Linux, not on macOS.** `-fbacktrace` there symbolicates
with file and line for nothing, while macOS needs the crash report in
`~/Library/Logs/DiagnosticReports/` and gives no line numbers. The Mac was the
harder machine to work on and was not the necessary one.

**g. Narrowed to the assignment TARGET, with a concrete hypothesis (2026-08-24, gdb on achari2).**
Runtime values at the fault:

| | |
|---|---|
| crash line | `pointgroup.foo:1018` — `.irrep(i).character(n) = .irrep(i).mx(:,:,n).trace` |
| `i`, `n` | **1, 1** — the very first irrep and first operation |
| pointgroup | `oh`, `order` 48, `n_irrep` 10 |
| `ubound(.irrep(1).mx)` | `(1,1,48)` — correct for a 1-D irrep |
| `ubound(.irrep(4).mx)` | `(3,3,48)` — correct for a 3-D irrep |

So `mx` is allocated correctly and the READ side is sound. (An earlier reading of
gdb's output suggested every irrep was 1x1x48; that was a misreading of gdb's
`<repeats>` display, and it is wrong.) By elimination the fault is the WRITE
target, `.irrep(i).character`, which is allocated on the line immediately above
by `.create(.order)`.

**It could not be inspected directly**: `print self%irrep(1)%character` gives
*"A syntax error in expression, near `character'"* — gdb's Fortran parser treats
`character` as a keyword. Which points at the hypothesis.

**Hypothesis to test next: the component names.** `types.foo:3196` declares

```foo
   type IRREP
     label     :: STR(len=4)
     dimension :: INT          ! <- Fortran keyword
     character :: VEC{REAL}@   ! <- Fortran keyword, and the allocatable that fails
```

Both `character` and `dimension` are Fortran keywords used as component names.
That is legal Fortran and has worked for years, but it is exactly the sort of
thing a new front-end regresses on, and the failing component is the one that is
both keyword-named *and* allocatable. gdb's parser already chokes on it.

**REFUTED, same day.** The experiment was run: `character` renamed to `chi`
throughout (4 references and the declaration — the blast radius is only
`types.foo`, `irrep.foo` and `pointgroup.foo`), gfortran-16 debug rebuilt,
`h2o_rhf_STO-3G` rerun. **Still exit 139.** The keyword-named component is not
the cause, and the rename has been reverted rather than left in the tree, since
it fixes nothing and would muddy the record. gdb's parser error was a red
herring about gdb, not about the compiler.

(The names are still poor Fortran — `character` and `dimension` are both
keywords, and `chi`/`dim` would be better, `chi` being the standard symbol for a
group character. Worth doing as hygiene, on its own, but it is not this bug.)

**h. The construct itself does NOT reproduce it (2026-08-24).** Two standalone
reduced cases were written and run under both compilers: (1) an allocatable array
of a derived type with an allocatable vector component, allocated
element-by-element in a loop then written through; (2) the same, with the array
itself a component of an outer derived type, so the access is
`self%irrep(i)%chi(n)` exactly as in Tonto. **Both compile and run to exit 0
under gfortran-14 and gfortran-16.** So the shape at the crash site is not, by
itself, the trigger.

**That is the most useful negative result so far, because of what it implies: the
crash site is probably not the bug site.** A fault that will not reproduce when
the construct is isolated, but is reliable inside the full program on two
platforms, looks like **earlier memory corruption landing somewhere fatal** --
and gfortran-14 and -16 simply lay memory out differently, so the same corruption
is benign under one and fatal under the other. That would explain every
observation at once: compiler-dependent, platform-independent, reproducible in
situ, not reproducible in isolation, and a read side that checks out while the
write target does not.

**The leading candidate for that earlier corruption is already in hand.** The
`-fcheck=all` run stops long before the SCF, during keyword reading, at
*"Allocatable actual argument 'tmp' is not allocated"* -- `VEC{OBJECT}:read_keys`
and `:clear_keys`:

```foo
   read_keys :: leaky
      self :: allocatable, INOUT
      tmp :: OBJECT@          ! declared, never created
      ...
      tmp.read_keys           ! called on an unallocated allocatable
```

That is undefined behaviour on every compiler; the uninstrumented run merely
survives it. The template is inherited by 43 files.

**The experiment was run (2026-08-24), and the `tmp` bug is fixed but was NOT the
cause.** Four sites in the *allocatable* copy of the template now do
`tmp.create` / use / `tmp.destroy`, which is the pattern two sibling procedures
in the same file already used. (The *pointer* copy below the `! Old` marker
deliberately `nullify`s and is left alone -- the checker's complaint named an
allocatable.) Result: the `-fcheck=all` violation is gone, and the run gets
further -- but it still ends in **SIGSEGV, same site, same faulting address
`0xd9`**. `-fcheck=all` does not catch the fault itself.

**A further clue, and it strengthens the corruption hypothesis: the crash site is
not the same on the two platforms.**

| platform | crash |
|---|---|
| Linux x86_64 | `POINTGROUP:make_character_table`, `pointgroup.foo:1018` |
| macOS arm64 | `MOLECULE.BASE:make_pg_image_of_shell`, `KERN_INVALID_ADDRESS at 0xd9` |

Both are in the pointgroup machinery reached from `make_anos_for_atom`, but they
are *different procedures*. A single deterministic logic error would fault in the
same place on both. Two different faults in the same neighbourhood, neither
reproducible when the construct is isolated, is what heap corruption looks like:
whatever is damaged, the layout decides which innocent read dies first.

**The sanitizer was tried on macOS and is INCONCLUSIVE — read this before
repeating it.** A full `-fsanitize=address -fno-omit-frame-pointer` gfortran-16
debug build was made and run. ASan is genuinely active (`libasan.8.dylib` linked,
48 undefined `__asan` symbols), but:

- it produced **no ASan report at all**, and
- the process died with **SIGBUS (138)** rather than the usual SIGSEGV (139).

A sanitizer that changes the signal but issues no diagnostic has not cleared the
program; it has failed to instrument usefully. gfortran + ASan on arm64 macOS is
the suspect, not the absence of a heap bug. **Do not read this as "ASan found
nothing".**

Stack exhaustion was re-tested at the same time and is genuinely ruled out: the
plain gfortran-16 debug build still exits 139 with the stack at the hard ceiling
(`ulimit -s 65520`), as well as at the 8 MB default. That confirms the original
note.

**Next, and it should be done on Linux.** achari2 has gfortran-16 and a
conventional toolchain where ASan is reliable; the same build there is the
obvious repeat, and Linux already proved better for this bug once (it gives
symbolic backtraces with line numbers for free, which macOS does not). Failing
that, `valgrind` on Linux would name the offending write directly. The bisect
option — changing `initial_density=` away from `promolecule` to establish whether
the damage predates `make_anos_for_atom` — is cheap and still untried.

**Also done and NOT the fix:** the IRREP components `character` and `dimension`
were renamed to `chi` and `dim` (they are Fortran keywords and were poor names).
It builds clean and the crash is unchanged, so it is hygiene, not a fix. Note
`dim` is **reserved in Foo** as the array-size accessor -- renaming DIIS's
`dimension` method to `dim` turned `d = .dim` into `d = size(self)` on a
non-array and broke the build, so DIIS keeps its name.

**Still open, and the next step.** `-fcheck=all` does turn a crash into a named
error, but **not this one** -- it stops earlier, at
`vec_atom.F90:2018`, with *"Allocatable actual argument 'tmp' is not allocated"*,
during keyword reading and long before the SCF. That is a real defect in its own
right (`VEC{OBJECT}:read_keys` and `:clear_keys` declare `tmp :: OBJECT@`, never
create it, and then call a method on it; the template is inherited by 43 files),
but it is benign in practice -- the uninstrumented run gets past it and dies
later somewhere else. So it is **not established** that it causes the segfault.
To get there: fix or suppress the `tmp` violation, then re-run under
`-fcheck=all` and see what it names next.

4. **Also wanted (Dylan): a DEBUG CI job.** Debug is the configuration whose job is to catch
   crashes and precondition violations, yet nothing checks it — CI builds release only, and the
   debug status here was two weeks stale, which is why this went unnoticed. Design notes: the 4
   known debug failures (47, 64, 87, 91) must be recorded as *expected* or the job is red from
   day one and gets ignored; and `-O0` is slow, so the `short` suite (or a few representative
   jobs — one SCF, one HAR, one CIF-processing) is the sensible scope.

---

# Done, resolved and closed (archive)

## DONE (2026-08-02): test registration — stale `DEPENDS run_test`

**Removed.** `grep -c "DEPENDS run_test" tests/CMakeLists.txt` is now 0; it went with the hart
work's rewrite of `add_all_tests`.

`tests/CMakeLists.txt` used to set `DEPENDS run_test` on every registered test.
`DEPENDS` names a *test*, and no test called `run_test` has ever existed — the
name belongs to a Python function in `scripts/test.py`. It was a silent no-op
and has been removed. Mentioned here in case the intent was a real dependency
on the executable being built (which `add_test` does not express; that is what
the `report` target's `add_dependencies` is for).

## Closed (won't-do): eliminate explicit `TYPE:proc` calls — at its practical limit

**What:** the submodule-call cleanup (`4cd995df`) auto-resolved `.SUBMOD:proc` etc., but
explicit **type-qualified** calls `TYPE:proc` / `TYPE::proc` (e.g. `GAUSSIAN_DATA:...`,
`STR:...` for namespace access + method calls) were left **qualified on purpose**.

**Why it's not just a mechanical `TYPE:proc(x)` → `x.proc` rewrite:** that transform is
**unsafe** and was tried and reverted. An **elemental** method invoked on a `VEC{T}` array
receiver resolves against the *receiver's* type (`VEC{T}`), not the element type `T`, so
`x.proc` can bind a different (array-level) overload than `T:proc` intended — this introduced
a `use` cycle. See memory `typeproc-elemental-array-hazard`. A correct elimination needs
**type-aware** resolution that respects elemental/array-receiver semantics (and the
GAUSSIAN_DATA namespace-access case), not a blind receiver swap. Low priority — the explicit
form compiles and runs fine; this is a consistency/readability cleanup, not a correctness bug.

**Analysis tool built — `--type-qualified-call-report` (read-only).** Rather than rewrite
blindly again, `FooToFortran` now classifies every site by asking the *real* resolver two
questions — "where does `TYPE:proc` resolve today?" vs. "where would `arg1.proc(...)` go?" —
and only calls a site **SAFE** when both answers exist and agree. Writes
`type_qualified_calls.tsv` (per-site verdict + why). New helpers `buildElementalByModule`,
`runTypeQualifiedCallReport`, `classifyTypeQualifiedCall`, `isLonePostfixArg`. Emission is
untouched; it just walks the modules and throws the Fortran away (~11 min single-JVM full walk).

**Verdict: this task is at its practical limit — it cannot be cheaply or completely done.**
Report over **2068** sites (2026-07-29):

| class | count | % | convertible? |
|---|---|---|---|
| NAMESPACE (989 = `GAUSSIAN_DATA::…`) | 1002 | 48.5% | no — data access, no receiver to promote |
| MODULE_MISMATCH | 506 | 24.5% | no — would bind a *different* module |
| EXPR_RECEIVER | 232 | 11.2% | no — receiver needs added parens |
| ELEMENTAL_HAZARD | 99 | 4.8% | no — the exact trap that sank the reverted attempt |
| **SAFE** | **154** | **7.4%** | **yes — the only candidates** |
| UNKNOWN / BY_NAME / COMPONENT_COLLISION / NO_RECEIVER | 75 | 3.6% | no — untypeable or non-call |

Only **7.4%** is mechanically convertible; the other **~92%** each fail for a *structural*
reason (namespace access with no receiver, genuine cross-module dispatch, expression receiver,
or elemental/array-receiver semantics) that no blind rewrite can satisfy. Converting the 154
SAFE sites would be a large, file-spanning diff for a cosmetic gain while **leaving the bulk of
the explicit forms in place anyway** — so the readability payoff never actually lands. The
explicit `TYPE:proc` form compiles, runs, and is unambiguous. **Recommendation: stop here.**
Keep the report as the durable evidence and the work-list should anyone ever want the SAFE
subset; treat full elimination as *not worth doing*, not merely *not yet done*.

## DONE: phase B — per-executable dead-code elimination

**Goal (Dylan):** eliminate code dead for a specific executable (e.g. `run_molecule`/`tonto`),
in a separate build dir, without affecting the other executables or the normal build.

**Delivered** in `FooToFortran.java` + `CMakeLists.txt`:
- `--call-graph-report` → Graphviz `call_graph.dot` / `module_use.dot` (submodules collapsed to
  parent) / `submodule_use.dot`; `--dead-code-report <root.foo>` → per-module live/dead TSV;
  `--purge-dead-code <root.foo>` → two-pass emit dropping unreachable procs. CMake exposes the
  `callgraphs` target and `-DPURGE_DEAD_CODE=<stem>` (separate build tree). See README §7b, CLAUDE.md §8.
- Reachability = BFS from the root program's entry calls over a call graph captured by
  piggybacking on the real call-resolution. `TYPES`/`SYSTEM` (wholesale-`use`) never pruned.

**Validated:** `-DPURGE_DEAD_CODE=run_molecule` release build compiles 0-error, ~32% of the
~7600 procedures dropped, binary 33→25 MB, and ctest is **121/124 — identical to the full build**
(same 3 known-bad). Three reachability-analysis bugs were caught only by the compile+test gate,
each a call form that bypassed the `use`-based capture: (1) same-module `::proc`/bare-selfless
calls (fixed: `recordSelfCall`); (2) case-sensitive node keys (`reset_IO_status` vs
`reset_io_status`; fixed: `node()` lower-cases the method part — motivates the case-cleanup goal
above). CPP-macro-hidden calls all target `SYSTEM` (always kept), so no macro-root class exists.

## DONE: simplify the DOT call-graph output

**Goal (Dylan):** reduce the complexity of the graphs from `--call-graph-report` (phase B).
`call_graph.dot` is a **procedure-level** graph — ~7600 nodes / ~24k edges — too dense to read
as a single image. `module_use.dot` (921 edges) was legible-ish but still a hairball.

**Delivered** as `scripts/simplify_callgraph.py` (a post-processor over `module_use.dot`, not a
translator change — the DOT already carries every `use` edge):
- **Aggregate** — fold module families into one coloured node: `NUMBERS` (INT/REAL/CPX),
  `ARRAYS` (VEC/MAT of primitives), `SHELLS`, `GAUSSIANS`, `MAPS`, `ISOSURFACES`; and re-point
  `VEC{T}`/`MAT{T}` over a derived type to that element's module (`VEC_ATOM`→`ATOM`), surfacing
  real deps like `MOLECULE→ATOM`. Dropped dead `BREAKDOWN_DATA`, `MULTI_T_ADP`.
- **Ambient** (`--simplify`) — hide the 7 universal utilities that take 62% of all edges
  (`NUMBERS ARRAYS STR BIN TEXTFILE BUFFER TABLE_COLUMN`). 139/921 → **50 nodes / 114 edges**.
- **`--module NAME`** — documentation ego-graph of one module's direct dependencies
  (`--reverse` dependents, `--both`); no `concentrate`, so every direct edge shows.

**Key findings, recorded in `docs/TONTO_CALL_GRAPHS.md`:** the "aggregate vs ambient" distinction
(merge-and-keep vs hide); `concentrate=true` is *lossy* (drops a direct edge parallel to a
longer path — e.g. `ATOM→INTERPOLATOR`), so the doc mode avoids it; and Graphviz has **no** edge
hops/bridges and is already hierarchical, so fewer edges beats a different engine. README §7b
points to the tool + doc. (The procedure-level `call_graph.dot` remains dense — a module-level
*call* graph in `writeDotFiles` is still a possible future refinement, but not needed now.)

## RESOLVED (was: atomic (guess) SCF fails for partly-filled degenerate shells)

**There was no SCF instability.** The diverging boron atomic SCF found on 2026-07-28 was a
*symptom* of the gfortran miscompilation of `shell1quartet.F90` (see "verify the macOS build"
below): the two-electron integrals were wrong, so the atomic SCF was iterating on a corrupted
Fock matrix. With the compiler workaround in place boron converges normally, the `DIE` does not
fire, and rgbi/BN reproduces the Linux reference **exactly**. Nothing here needs fixing in the
SCF or in the physics.

**Kept, because they stand on their own merits:**

- **`make_ANOs` DIEs on a non-converged atomic SCF** (`molecule.scf.foo`). It previously
  collected the per-atom `converged` flag, printed it only in verbose mode, and carried on —
  so ANOs built from a garbage iterate silently corrupted every quantity derived from the
  atomic projectors while molecular totals still looked plausible. Refusing to continue is
  right regardless of what caused the non-convergence, and it is exactly the check that would
  have surfaced this class of fault sooner.
- **The widened guess-SCF whitelist** (`molecule.set.foo`). Only `output`, `initial_MOs`,
  `initial_density`, `convergence`, the DIIS tolerance and `relativity_kind` were copied from
  the parent, so `max_iterations`/damping/level-shift given in the input were silently
  discarded and a failing guess SCF could not be rescued from the input file at all.

**Retired follow-ups.** Two items were queued here — calibrating "sensible" guess-SCF recovery
options, and adding a tiny symmetry-breaking charge to make the atomic SCF deterministic. Both
were motivated purely by the phantom instability and are **no longer needed**. Worth keeping
from that discussion, though: the textbook remedy of spherically-averaged *fractional*
occupations does **not** apply here, because UHF requires integer occupancy of spin orbitals —
so if a genuine degeneracy problem ever does appear, the degeneracy must be *lifted*, not
averaged over.

**The cautionary tale.** Every hypothesis below was tested against the real code and refuted,
one after another. In hindsight they all failed for the same reason: they were searching for a
*physics* cause of a *compiler* artefact. The tell was there early — the same input gave
different answers on different platforms — and it deserved more weight than it got.

| Hypothesis | Verdict |
|---|---|
| Boron's atomic SCF diverges; ANOs built from it anyway | **Observed** (100 iters, −6…−69 Ha, "DIIS stuck", final −65 Ha for an atom whose energy is ≈ −24.5) |
| That causes BN's platform-dependent Roby numbers | **Demonstrated by intervention** — forcing convergence moved B 5.35/3.40 → 6.50/6.63, collapsed the cross-library spread 1.95 → 0.13, landing on the Linux reference 6.48 |
| It explains the other rgbi failures | **Refuted** — only BN fails to converge; C2/CN-/CO/F2/N2/NF/O2 converge with bit-identical atomic energies across LAPACKs and still deviate |
| "Partly-filled degenerate shell" predicts which atoms fail | **Refuted** — carbon 2p² is partly-filled and degenerate, and converges |
| Low nuclear charge (B, Z=5) | **Not supported** — standalone UHF SCFs *converge* for B(5), C(6), N(7), Al(13), Si(14). But see the oxygen-atom finding below: standalone atomic energies on this Mac are themselves wrong, so these runs test convergence only, not correctness |
| Bad initial guess | **Refuted** — `initial_density=` core / fock / promolecule / progroup all give identical results and all fail (verified the option reaches the SCF: "Kind of initial density ... promolecule") |
| Loose guess tolerances (1e-3 vs 1e-5) | **Refuted** — standalone boron converges at every combination of `convergence`/DIIS tolerance |
| Wrong atom setup in the guess | **Refuted** — guess reports B, charge 0, multiplicity 2, 5 e⁻ (3α/2β), all correct |

**Do not trust the numbers in that table as physics.** Everything above was measured on
*miscompiled* binaries, so quantities like the "two converged boron solutions 0.57 Ha apart"
(−24.3378 vs −23.7663) are artefacts of wrong two-electron integrals, not evidence of an SCF
pathology or of basin-hopping. They are kept only as a record of what was tried.

One observation from that work is still true and worth knowing, since it is a property of the
defaults rather than of the bug: the guess SCF withdraws **all** stabilisation at exactly the
iteration DIIS takes over —

```
level shift 0.30, quits at 3   |   damping 0.50, quits at 3   |   DIIS extrapolates from 3
```

That is fine when the first three iterations settle, and unforgiving when they do not. It did
not cause this failure, but it is a thin margin for any genuinely hard atom.

### Retired: (a) sensible guess-SCF recovery options, (b) symmetry-breaking charge

Both were queued to tame an instability that turned out not to exist. The `DIE` message no
longer prescribes any values (it names the relevant keywords only), so nothing extreme is
enshrined, and the tiny-charge idea has no problem left to solve. The `SCF_DATA` machinery it
would have used (`.cluster_charges`, `.cluster_charge_positions`,
`set_using_cluster_charges()`) is noted here only in case a genuine degeneracy problem ever
does turn up.

**Related defect, still open and unrelated to the above:** a `DIE` leaves the process **exit
status 0**. `scripts/test.py` uses `subprocess.check_call`, so a hard error is not detected as
a run failure -- only the output diff catches it. Worth fixing so failures are unambiguous to
CI.

## DONE (2026-08-01/02): test the MPI parallel build

**Milestone 4 complete.** First MPI build ever configured for this project; full characterisation
in `docs/TONTO_AND_MPI.md`, defects in the MPI section above. Headline: MPI at 1 rank reproduces serial
exactly on two platforms, rank-count drift is confined to one already-known-marginal test, and
`-ffast-math` moves the numbers more than MPI does.

**Goal (Dylan):** verify the MPI build works and its tests pass. Build flags exist
(CLAUDE.md §4: `-DCMAKE_Fortran_COMPILER=mpifort … -DMPI=1`, optional `-DNO_ERROR_MANAGEMENT`);
`scripts/test.py` has a `--mpi` path (`mpirun -n 4`), wired via `WITH_MPI` in `tests/CMakeLists.txt`.
Status is **unverified** for the ANTLR4 translator output. Start by building MPI and running
`ctest` under it; expect the parallel macros (`PARALLEL_DO_*`, `PARALLEL_SUM`, `broadcast_` — all
`SYSTEM`/`tonto`-targeted, see `macros.in`) to be the surface area. Compare against a non-MPI run.

## DONE: continuous integration (GitHub Actions, loose gate)

**Goal (user):** bring back automated CI so every push builds the ANTLR4 translator,
compiles `tonto`, and runs the test suite. (The old Travis badge was defunct.)

**Delivered** as `.github/workflows/ci.yml` — green as of `99dc3a1c` (2026-07-27):
- **Provider:** GitHub Actions (Travis's OSS offering is dead). Triggers on push/PR to
  `antlr4`, `master`, `release`.
- **Pipeline:** checkout `--recursive` → install gfortran-14 + JDK (for the translator's
  `javac`; the ANTLR jar auto-downloads via CMake) + BLAS/LAPACK/python3 → `cmake` +
  `cmake --build -j2` (bounded: one JVM per `.foo` is memory-heavy on a shared runner) →
  short suite via `scripts/suite_report.py`.
- **Gate:** the **loose** criterion (rel ≤ 0.2% OR last-digit ≤ 2). The agreement table is
  echoed to the run's Job Summary and uploaded as `tests.log`; a self-diagnosing `Diagnostics`
  step (`if: always`) reports toolchain/binary/one-raw-test on red runs. README carries the badge.
- **Two bugs the first green run flushed out** (both fixed): `scripts/test.py` resolved a
  *relative* `--test-directory`/`--basis-sets` after `chdir`ing into the temp run dir → doubled
  path → 100%-fail (fixed by absolutising up front, `036ecaec`); and the Kanghyun `keyword_echo`
  lines added un-reblessed stdout → 44/51 (dropped both echoes, `99dc3a1c`).
- **Not done (deliberate):** debug/release matrix and gfortran-version matrix — single release
  job for now; jar/parser caching not yet added (build is tolerable at ~20 min).

## DONE: explicit `self` intent via self-modification analysis (plan B)

**Goal (user):** make `self`'s intent explicit in the `.foo` sources where it is
currently implicit. The first attempt used a blanket rule (subroutine → INOUT,
function → IN); it did **not** compile — read-only subroutines given INOUT reject a
const `self` from their (often inherited) callers, and some *functions actually
modify self* (memoisers, lazy readers), so `self :: IN` was rejected. Dylan had
assumed all functions are PURE, which several are not.

**Resolved with "Option 2" below** — a **self-modification analysis** in the
translator (`FooToFortran --add-self-intent`, parse-tree driven). Rule:
- subroutine → **INOUT** iff it modifies self, else **IN**;
- function → **IN** iff pure (does not modify self); a self-modifying function is
  left implicit and **flagged impure** (see `self_intent_analysis/impure_functions.tsv`).

"Modifies self" = direct write (`self%x = …`), a self-method call that transitively
modifies self (fixpoint, seeded with create/destroy/nullify), a call to a method whose
`self` is *declared* INOUT/OUT, or an input read into a self component
(`stdin.read(.label)`, `.SCF_DIIS.read_keywords`, `.atom(a).set_flag`).

**Applied + validated:** 135 `self :: IN|INOUT` decls (58 IN / 75 INOUT) across 47
foofiles, plus 2 genuinely-wrong hand-written `INOUT`→`IN` corrections on read-only
`MAT{REAL}` `_LAPACK` helpers. A clean **release** build compiles 0-error and the full
`ctest` suite is **121/124** — the same three deferred failures below, no regressions.

**Follow-on (deferred, Dylan's proposal):** mark the impure procs `IMPURE` in the
`.foo` and declare the rest `PURE`. Purity is compiler-enforced (a `pure` proc calling a
non-pure one is an error), so it self-validates. Impure = {modifies an arg or self} ∪
{does I/O} — so put/dump/show/read are impure regardless of self. The
`impure_functions.tsv` (modifies-self + OUT/INOUT-arg functions) is the seed list; add
I/O-call detection when tackling it.

## DONE (release): the 3 remaining test-suite failures (milestone 3)

**Resolved on release — verified 2026-07-17: a `gfortran-14` release build is 124/124 (`ctest`
exit 0), up from 121/124.** All three former failures now pass, fixed at the source / in the
references by Dylan (commits `b3b50dd2` "Fixed no. of doubles test error", `d9dffb3f`,
`dee5cac9` "Corrected Salvador test", `50988e87` "All short & long tests passing on laptop"):

1. **`cyclazine_rhf_cc-pVDZ_tddft_state_selection`** — was a single-line `No. of doubles`
   diff (`24355` ref vs `22797`). **Not** an `-O0` boundary artifact after all: it was a
   real **evaluation-order bug in the source**. `foofiles/td_data.foo` computed
   `n = .no_of_doubles` *after* printing the doubles-window block; commit `b3b50dd2` moved the
   assignment *before* the block, so the printed count is now correct and stable. (Also fixed
   two typos in adjacent `stdout.text` lines.)
2. **`gly_ala_fragHAR_rhf_STO-3G`** — table column-width / alignment shift; reference updated
   (`50988e87`). Passes (73 s).
3. **`urea_ccsd_pob-TZVP_Salvador_properties`** — the longstanding Salvador grid/partition
   numeric difference; the reference was updated to the release-produced values (`dee5cac9`),
   so the release build now matches exactly. **NB:** this "resolves" it only for `-O3`; the
   ~0.5% `-O0` difference is unchanged and now shows up as a **debug-only** failure — it has
   moved into the debug section below, not disappeared. The `atomic_moments_made` knock-on
   question (see the Salvador section above) is still unverified.

Two other cases seen only under the *strict* (exact) sweep also loose-pass and are benign:
`h2o_rhf_cc-pVDZ_tdhf` (one TDHF state differs in the last digits) and
`cyclazine_rhf_cc-pVDZ_VMO_canonicalization` (~1e-4; original archives lost, regenerated).
A threshold-driven "loose pass" gate (candidate for CI, above) absorbs all of these.

What remains for milestone 3 is therefore **not** the release suite (green) but the **debug**
suite (119/124 — next section) and wiring the release gate into CI.

---

## `archive/bond-energy` will not be ported (Dylan, 2026-08-18)

**Decided, not merely postponed.** The Roby bond-energy work on `archive/bond-energy`
(Dylan Jayatilaka, 11 commits, 2020-09 – 2020-10) stays on its tag. Nothing on
`develop` depends on it and nothing was removed to make this decision.

What it holds, so the size of the thing is on record: `roby.foo` +1478 lines --
`Eshared` partitioning for E^DE, an exact energy-density method, deformation
energies and group populations -- plus `types.foo` +127, `atom.foo` +123,
`vec{atom}.foo` +122, and smaller changes to `molecule.prop.foo`,
`molecule.scf.foo`, `opmatrix.foo`, `vec{int}.foo` and `CMakeLists.txt`.
Eleven files, +1756/−217, all unmerged (`git cherry develop archive/bond-energy`
marks all 11 commits `+`).

Two things a future reader should know before reopening it, both established
2026-08-18:

- **The starting position is better than it looks.** Unlike the Bader port there is
  already a regression net: `tests/rgbi/karrikinolide_blyp_6-31G(d)_Roby_bond_index`
  and its `_g09_` variant exercise the live Roby path. And the two helper modules the
  tag adds, `vec{emat{intrinsic}}.foo` and `vec{emat{int}}.foo`, have since landed on
  `develop` independently, so that part of the work is already done.
- **The difficulty is drift, not translation.** `roby.foo` is 7490 lines on `develop`
  against 8463 on the tag, and it has been modified on `develop` since 2020. This is
  a graft into evolved code, not a clean apply, and it needs a `types.foo` change --
  which the Bader port did not.

Recover it with `git branch bond-energy archive/bond-energy`. See
`docs/TONTO_REPOSITORY_BRANCHES.md`.

## Keyword parsing must not leak into library routines (fixed; survey kept)

`MOLECULE.READ:read_archive(name,genre)` is a library routine — it takes its
operands as arguments and is called from `molecule.scf.foo` (×3),
`molecule.main.foo` (×2) and `run_sf_derivs.foo`. It nonetheless reached for the
global `stdin` to look for a trailing `normalise` qualifier:

```foo
   if (stdin.buffer.n_items==2) stdin.read(normalize)
```

Two bugs in one line:

- **It segfaults in any program without a `stdin`.** `hart` has no job file, so
  this dereferenced an unallocated `TEXTFILE` on the second HAR cycle — the
  first time the refinement re-reads its density archive.
- **In `tonto` it was dead, and could corrupt parsing.** `TEXTFILE:n_line_items`
  *is* `buffer.n_items`, and the keyword handler `ENSURE`s `n_line_items==3`, so
  a line of 3 items can never satisfy `==2`: no job file could ever switch
  normalisation on, and every `if (do_norm) …unnormalize(…)` branch was
  unreachable. Meanwhile a *programmatic* caller inherited whatever line the job
  file was on, and on a 2-item line would silently consume one of its tokens.

Fixed: the qualifier is now an optional argument, and `read_archive_and_normalise`
is its keyword — so the feature is reachable for the first time.

**Survey — this was the only instance.** Six places in the library peek at the
line and then read a variable number of items:

| Site | Procedure | Shape | Reached from |
|---|---|---|---|
| `molecule.har.foo:1797` | `make_spherically_averaged_HAs` | no args | 1 `case` line |
| `molecule.misc.foo:2040` | `put_interpolator_list` | no args | 1 `case` line |
| `molecule.misc.foo:5022` | `put_spherical_SA_ED_from_atom` | no args | 1 `case` line |
| `diffraction_data.put.foo:783` | `put_worst_reflections` | no args | 1 `case` line |
| `molecule.put.foo:674` | `put_archive` | no args | 1 `case` line |
| `molecule.read.foo` | `read_archive(name,genre)` | **took args** | 6 programmatic sites |

The five no-argument ones are genuine keyword handlers: they read *all* their
operands from `stdin`, including the name of the thing to act on, and a driver
never calls them. That pattern is fine. The breakage came only from merging the
parse role and the do role into one procedure, which happened once.

**The rule, for anyone adding a driver:** a procedure that takes arguments must
not touch `stdin`. `scripts/check_library_stdin.py` enforces it (ctest name
`library_stdin`, label `short`; also run by `make report` and CI).

### DONE (2026-08-02): `write_archive` swallowed the next keyword

Found while testing `read_archive_and_normalise`. `MOLECULE.PUT:put_archive` has
the same qualifier peek, with an off-by-one:

```foo
   if (stdin.buffer.n_items==2) stdin.read(normalize)     ! molecule.put.foo
```

`n_items` counts the **whole line including the keyword** — that is why
`read_archive`'s handler asserts `n_line_items==3` for
`read_archive <name> <genre>`. So a plain `write_archive MOs` is exactly 2 items,
the peek always fires, runs off the end of the line and **consumes the next
keyword** as its qualifier. The test should be `==3`.

Demonstrated: a job file with `write_archive MOs` followed by
`read_archive MOs restricted` dies with "unknown option: mos" — the
`read_archive` keyword was eaten, leaving its operands to be read as keywords.

**The one test that uses it is hollow — investigate this first.**
`tests/long/nh3_x-ray-constrained-rhf-cluster-charge_cc-pVTZ_restart/stdin:196`
reads:

```
   read_ascii_archive density_mx r
   write_archive density_mx

   ! Now to the SCF
   scf
```

`write_archive density_mx` is two items, so the peek fires and swallows the very
next token — **`scf`**. The reader resumes at `delete_scf_archives`. Evidence
from the blessed reference, not inference:

- `grep -c SCF stdout` → **0**. Not one mention, though the job sets
  `output_results= YES`.
- `Wall-clock time taken for job "nh3" is , 40 milliseconds.`

An X-ray-constrained RHF cluster-charge SCF in cc-pVTZ does not run in 40 ms.
**The test performs no SCF at all**, and its reference was blessed in that
state, so it passes by faithfully reproducing the skip. Whatever coverage its
name promises — X-ray constrained wavefunctions, cluster charges, restart — it
does not provide.

**Why it is not fixed here.** Correcting the off-by-one makes `scf` execute, so
the job would then do the science it was written to do and its reference must be
re-blessed against completely different output. That is a deliberate decision
about a long-suite test, not a drive-by in an options branch.

**Blast radius, surveyed:** one test and one line.
`grep -rn '^[[:space:]]*write_archive' tests/ runfiles/` returns exactly the nh3
line above and nothing else, and `n_items==2) stdin.read` now matches only
`molecule.put.foo:674`. So the fix is a one-character edit (`2` → `3`) plus
re-blessing one reference — but that reference will change completely, because
the job will start doing an SCF it has never done.

(The `normalise`/`normalize` spelling fix applied to that same line is
behaviour-neutral until this off-by-one is fixed, since the token it compares is
currently a stray keyword either way.)

### Four more of the same shape, allow-listed rather than fixed

Writing that check turned these up. They are **not broken today**, so they are
recorded rather than changed, but they are the same merged-role pattern and the
allow-list in the script names each one:

- `molecule.read.foo:read_molden_file`, `read_tonto_FChk_file`,
  `read_g09_FChk_file` — take an optional name and fall back to
  `stdin.buffer.exhausted` when it is absent. Safe only because every driver
  passes the name (`run_rgbi.foo:202-203` does). Call one without the argument
  from a program that has no `stdin` and it dereferences an unallocated
  `TEXTFILE`, exactly as `read_archive` did.
- `molecule.put.foo:put_florian_wfn_file` — **unguarded**:
  `if (NOT stdin.buffer.exhausted) stdin.read(name)` with no `present()` test at
  all. This one would crash in any driver that reaches it. Nothing does today.

The fix in each case is the one applied to `read_archive`: move the fallback
into the keyword handler and let the library routine take a plain argument.

`vec{basis}.foo:read_library_data` is allow-listed for the opposite reason — it
is *correct*. It creates `stdin` itself when there is none and restores it
afterwards. Copy that pattern if a library routine genuinely needs the parser.

(`put_archive` looks like a counter-example to a plain grep — 10 further call
sites — but those are a *different overload*, `put_archive(item,name,…)`, which
takes arguments and never touches `stdin`. Read the `.int` file rather than
grepping by name; see CLAUDE.md §8.)

## DONE (2026-08-03): the MPI CI is GREEN — first time ever

Run #8 (`29f8dcea`) concluded **success**, after the `--oversubscribe` fix below. Every step
passed, including the two that matter:

| step | result |
|---|---|
| Assert the binary really is MPI-linked | success |
| **MPI invariant — pi is rank-count independent (GATING)** | **success** |
| Short suite under MPI at 2 ranks (informational) | **success** |

Runs 1–7 had all failed. Note the informational short suite passed too, which is more than the
gate required — so `tonto` at 2 ranks agrees with the serial references across the short suite in
CI, not just locally.

## DONE (2026-08-02): the MPI CI failure was the launcher, not the reductions

**The reductions are correct.** This entry previously said "something in the reduction path is
still wrong" — that was wrong, and it was written before the log had been read. Correcting it in
full, because the mistaken version was pushed.

`ci-mpi.yml` had failed on every run since it was added, always on the gating step *"MPI
invariant — pi is rank-count independent"*. The log says:

```
ok   mpi_pi   -n 1: 3.141592653589362
ok   mpi_pi   -n 2: 3.141592653589390
FAIL mpi_pi   -n 4: launcher exited 1
```

So π was right at 1 and 2 ranks, agreeing with each other to **13 significant digits** — i.e.
`PARALLEL_SUM` works. At 4 ranks the program **never ran**: Open MPI 5 refuses to start more
ranks than there are slots and exits non-zero, and a GitHub runner does not have 4.

The workflow already knew this in one place and not the other: the toolchain-verification step
uses `--oversubscribe` (`ci-mpi.yml:114`), and the suite step is pinned to `-n 2` with the
comment *"the runner has 4 vCPUs and the launcher would oversubscribe"* — but the π check was
invoked with `1 2 4` and `check_mpi_pi.sh` launched without the flag.

**Fixed** in `scripts/check_mpi_pi.sh`: probe the launcher (`--version | grep -qi "open mpi"`)
and add `--oversubscribe` when it is Open MPI. Probed rather than hard-coded because the flag is
Open MPI's; MPICH oversubscribes unasked and would reject it. Oversubscribing is exactly right
here — this is a correctness check on a tiny Riemann sum, not a benchmark.

Verified locally on a 12-core machine, where `-n 16` is a genuine oversubscribe:

```
ok   mpi_pi   -n 1: 3.141592653589362      <- identical to CI
ok   mpi_pi   -n 2: 3.141592653589390      <- identical to CI
ok   mpi_pi   -n 4: 3.141592653590147
ok   mpi_pi   -n 16: 3.141592653589789
ok   mpi_pi   all rank counts (1 2 4 16) agree with pi and with each other
```

and the flag is demonstrably the cause: `mpirun -n 16` alone exits 1 without running anything,
`mpirun -n 16 --oversubscribe` exits 0.

**What this changes.** Parallel fragHAR is **not** blocked by a broken reduction, which is what
the red CI appeared to say. The reduction path is sound at 1, 2, 4 and 16 ranks. The remaining
`fragment_SCF_para` concerns stand on their own merits — the two-strategy split and the
`next_p_loop_index` off-by-one — and are not evidence of anything deeper.

**Lesson worth keeping:** the check reported `launcher exited 1`, which is not a numerical
failure at all, and four consecutive runs were read as "the reductions are broken" without
anyone opening the log. `scripts/check_mpi_pi.sh` distinguishes the cases in its own output; the
diagnosis just has to read it.

## ROOT CAUSE FOUND AND FIXED (2026-08-03): `TEXTFILE:flush` put the margin twice on master

```foo
      if (IO_IS_ALLOWED) then
         write(unit=.unit,...) trim(.buffer.string)
         ...
         .clear_and_put_margin        ! <-- MASTER ONLY
         .record = .record + 1
      end

      PARALLEL_BROADCAST(.IO_status,...)
      PARALLEL_BROADCAST(.record,...)

      .clear_and_put_margin           ! <-- every rank
```

`clear_and_put_margin` ends in `.buffer.put(...)` → `BUFFER:put_str`, which **broadcasts**
`.string` (256 bytes) and `.item_end`. So the master entered **one extra pair of collectives per
flush**, the ranks fell out of step, and the next collective failed with `MPI_ERR_TRUNCATE`.

**Why it hid for years:** `clear_and_put_margin` *returns early* when `.style.margin_width==0`,
doing no collective at all — and that is the normal case. It only bites once something sets a
margin. `COMMAND_LINE:put_command_optarg` calls `set_margin_width(2)`, which is why `hart` died
exactly there, why the first 126 broadcasts matched perfectly (banner, margin 0), and why
`tonto` was never affected.

**Fix:** delete the call inside the guard; the one after the broadcasts already covers every
rank. Verified — `hart` at 2 ranks goes from dying at 30 lines of output to **758**, and the
truncation is gone. Serially unaffected: release rebuilt, short suite 50/51 unchanged, all four
invariant checks pass, `ctest -L hart` 4/4.

## WITHDRAWN (2026-08-03): the runtime "abort on a suppressed reduction"

Milestone 6 part 2 was implemented, and then **removed the next day, because its premise is
wrong**. It assumed a reduction reached while a parallel-do lock is held is always a bug. It is
not — it is *also* the intended nesting pattern:

```foo
SHELL1QUARTET:make_esfs_ss_0000(v11)      ! called from inside
   parallel do k = 1,.ab_n_gaussian_pairs !   MOLECULE.FOCK:make_u_JK_engine's
   ...                                    !   own `parallel do`
   PARALLEL_SUM(v11)                      ! correctly placed after ITS loop
```

With the outer lock held, the inner `parallel do` runs **serially over its full range on every
rank**, so each rank already holds the complete `v11` and skipping the reduction is **correct**.
That is "MPI on the outside" working as designed. `shell1quartet.foo` alone has 17 such loops,
all reached from outer loops in the Fock build, so the check aborted every debug MPI run almost
immediately — it killed `hart`.

Nor can it be downgraded to a `WARN`: these sites are per shell-quartet, so the warning would
fire millions of times.

**What survives is the lint**, and it is the right tool: the actual bug is a reduction *lexically
inside* a `parallel do` body (the four `molecule.grid.foo` sites), which
`scripts/check_parallel_lint.py` detects statically and precisely. The dynamic check could not
distinguish the bug from the design, and the static one does not need to.

*(An earlier note claimed the two checks were complementary — the lint for lexical cases, the
abort for reductions reached through a call. The call case turns out to be the legitimate one,
so there is nothing for the abort to add.)*

**Milestone 6 is therefore: part 4 (lint) done, part 2 withdrawn with reasons, parts 1
(`reduce(x)`) and 3 (depth-counted lock) still open.**

## Translator: `data` statements at `program` scope are silently dropped — FIXED 2026-08-04

**This one causes silently wrong answers, and it cost a day.** The ANTLR4
translator emits `data` statements for module-scope variables (`atom.foo` 21/21,
`colour.foo` 44/44, `becke_grid.foo` 6/6 survive) but **drops them entirely from a
`program` unit**. `runfiles/run_har.foo` had two; `build/run_har.F90` had none.

The declaration is still emitted, so it compiles clean and the variable is simply
uninitialised. In `hart` that meant `allowed_bases` and `grid_levels` were garbage,
so **every** basis name — including the program's own default `def2-SVP` — failed
`is_one_of` with "unknown basis". Nothing warned.

Worked around by assigning the arrays in the executable part instead
(`run_har.foo`, `run_sf.foo`, `run_sf_derivs.foo`). **The translator is the real
bug.** Until it is fixed, either emit the `data`, or make the translator *reject*
what it cannot translate — silently discarding a statement is the worst option.

`runfiles/run_csq.foo` still has program-scope `data` and is not in the translated
source list; it will hit this if revived.

### Resolution (2026-08-04)

**Both** halves of the recommendation above were implemented — the `data` is emitted,
*and* the translator now rejects what it cannot translate.

Root cause was a single line in `FooToFortran.emitBodyList`:

```java
if (b.localDecl() == null && b.stmt() == null) continue;   // blank / unhandled
```

`procBody` has seven alternatives; a `dataStmt` has both `localDecl` and `stmt` null,
so it fell through this crack. The comment even said "unhandled".

Now: `dataStmt` is emitted; `implicitStmt` and `useStmt` inside a body are skipped
**deliberately**, each with a comment giving the reason (the translator emits its own
`implicit none`, and the module's `<stem>.use` include already provides the `use TYPES`
in `VEC{REAL}:min_BFGS` — emitting either in place would not compile); and **every other
alternative throws**. A construct that parses but produces no output is now a build
failure, which is the property this entry asked for.

**The audit found the blast radius was smaller than feared.** Comparing every source
`data` statement against the generated Fortran across all 184 files found no dropped
statement anywhere in `foofiles/`. Five files looked like mismatches and all five are
variables *named* `data` (`data :: VEC{REAL}, IN`, `data = data(keep)`), not statements.
So no built binary was ever wrong: the library has no procedure-scope `data`, and the
only affected program, `run_csq.foo`, is not built. `run_csq` now translates with all
three of its `data` statements intact.

The `run_har.foo` / `run_sf.foo` / `run_sf_derivs.foo` workarounds are **left in place**:
they work, `hart` is now tested around them, and reverting tested code to restore a
`data` statement buys nothing.

Related: `none` is a reserved word in `Foo.g4`, so it cannot be used as a variable
name. The parse error it produces (`mismatched input 'none'`) points at the *end*
of the file, not at the offending line, which makes it hard to find.

## FIXED 2026-08-24: the macOS RGBI badge was red over one bad package name

`ci-rgbi-macos.yml` failed at **"BasicTeX, then tlmgr BY ABSOLUTE PATH"** from
2026-08-05. This entry used to say the cause was unknown, because the Mac that
last had the toolchain working was behind a firewall and the logs had expired.
Both premises turned out to be false: the Mac came back, and the log of run
**32456863540** (2026-08-21) was still retrievable. It says:

```
tlmgr install: package longtable not present in repository.
```

That is the whole fault. **There is no TeX Live package called `longtable`** --
`longtable.sty` ships inside `tools`, which BasicTeX already carries. Every
other package in the list was fine: `geometry`, `pgf` and `xcolor` were already
present, and `chemfig`, `pdfcrop` and `simplekv` installed cleanly (4/4). tlmgr
still returned 1 for the one name it could not resolve, and that killed the step.

None of the three candidate causes guessed here previously was right. The
absolute-path `tlmgr` trap, which this entry speculated about, was real but had
already been fixed on 2026-08-05 and was not the failure.

Fixed by dropping `longtable` from the install list, plus a `kpsewhich
longtable.sty` in the same step so the claim "already satisfied" is asserted
rather than trusted.

**The lesson is the cheap one.** The diagnosis took a single `gh run view
--log-failed` and no Mac at all -- the entry's own instruction ("anyone with a
Mac should read a FRESH log") was half right: the fresh log was the answer, the
Mac was not needed. Logs are retained 90 days; this one was read on day 3.


# `hart`: development history

Moved out of `docs/RUNNING_HART.md` on 2026-08-10, when the user-facing pages
were cleared of developer material. Kept verbatim.

## 4a. What was actually wrong with it

Recorded because "unverified" undersold it. `hart` was not *nearly* working —
**five independent defects** each made it useless, stacked so that fixing one
only revealed the next. Anything that runs an untested program should expect
this shape.

1. **Died before doing any work.** `std_err` was created but never opened, and
   the very next thing `hart` does is `close_and_delete` it — which dies on a
   file that does not exist.
2. **Reported success while dying.** `SYSTEM.die` ended in a bare `stop`, which
   exits 0. Every `DIE` in Tonto did. No harness could have detected any of
   this from an exit code, which is a large part of why it went unnoticed.
3. **Rejected every basis set, including its own default.** The translator
   silently drops `data` statements at `program` scope, so `allowed_bases` and
   `grid_levels` were uninitialised. See `DEFERRED.md` — the translator is the
   real bug and it is a silently-wrong-answer class.
4. **Segfaulted in the SCF.** It set the promolecule guess and then immediately
   overwrote it with `set_initial_density(spinorbitals)`, which means *read a
   density from an archive*, not *guess one*. No archive exists on a first run.
5. **Segfaulted on the second refinement cycle.** `hart` never created `stdin`,
   and `MOLECULE.READ:read_archive` consults `stdin.buffer` for a `normalise`
   qualifier — dereferencing an unallocated `TEXTFILE` the first time the
   refinement re-read its density archive.

Two further latent bugs in `COMMAND_LINE` (documented in `DEFERRED.md`) meant no
Tonto program could parse a command line at all under a debug build, which is
why the debug diagnosis had to be unblocked before it could be used.

**Correctness evidence.** `hart` is not merely running — driving `tonto` with
`hart`'s exact configuration produces a `urea.archive.cif` that is
**digit-for-digit identical**, every coordinate and every esd:

```
N1  0.14498(8)  0.64498(8)  0.17869(12)  0.02071(18)  Uani
```

So `hart` reproduces the validated `tonto` HAR path exactly on this structure,
rather than merely producing plausible numbers.


## 6. Milestones

**H1 — fragHAR support. SERIAL DONE (2026-08-02); PARALLEL DONE (2026-08-03).**

**H1 was a hookup, not a repair.** fragHAR works in `tonto` today: `tests/long/
gly_ala_fragHAR_rhf_STO-3G` converges and reproduces the last known-good 2019 output to the
printed precision (evidence in *"Archaeology"* below). So H1 was confined to `hart`'s own
argv-to-`fragHAR_refinement` path -- no science to reconstruct, no `tonto` regression to chase.

### What was built, and the acceptance evidence

`hart` now refines gly-L-ala as two capped, oppositely-charged residues and reproduces the
`tonto` reference **to every digit the reference prints**:

| | `tonto` reference (4 dp) | `hart` |
|---|---|---|
| R(F) | 0.0324 | 0.032423 |
| R(F2) | 0.0687 | 0.068659 |
| Rw(F) | 0.0334 | 0.033403 |
| R_sigma(F) | 0.0160 | 0.015952 |
| # of reflections, N_r | 2514 | 2514 |
| # of fit parameters, N_p | 181 | 181 |
| GoF (N_p) | 3.3535 | 3.353475 |
| Effective (mean) sigma^2 | 0.0253 | 0.025275 |
| Scale factor | 0.9768 | 0.976826 |

Registered as `tests/hart/gly_ala_hart_STO-3G` (label `hart`, 60 s), whose `IO` manifest
records why each non-default option is there. The whole invocation is:

```
hart --job glyala --basis STO-3G --grid-accuracy low --mmcif t --fos 0 \
     --residual-cube f --wavelength 0.59960 --group-charges '{ 1 -1 }' \
     --std-f2 gly_ala_100K_F2.hkl gly_ala_100K.cif
```

The pieces:

- **`--mmcif`** sets `CIF:set_is_mmCIF(TRUE)` *before* `process_CIF` (it selects the
  `_atom_site.` item names and brings in the compound sequence ids) and turns on
  `use_Ryde_capping`. No separate `--use-Ryde-capping` option: splitting a covalent chain
  without capping leaves dangling bonds, so there is no second choice to offer.
- **`--group-charges` / `--group-multiplicities`** take a **braced** list, one entry per group,
  read by the new `COMMAND_LINE:int_vec_for_option`. That builds a one-line internal `TEXTFILE`
  and hands it to `TEXTFILE:read_all`, i.e. the same reader the `atom_group_charges= { 1 -1 }`
  job-file keyword uses -- same syntax, same integer parsing. The brace check is a `DIE_IF` in
  `int_vec_for_option` rather than being left to `read_all`, whose own brace checks are
  `ENSURE`s and so compile away in every optimised build. The list length is then checked
  against the real number of atom groups, because a silently-short list would refine a
  zwitterion as neutral fragments and converge to a plausible wrong answer.
- **The whole list is one command-line token** and must be quoted. `process_options` binds
  exactly one token as an option value, so a bare `--group-charges { 1 -1 }` takes `{` as the
  value and sends `-1` to the single-dash rejection, which then advises `--1`.
- **Charges are written to `MOLECULE.atom_group_charges` as well as onto the groups.** That is
  the copy that survives: `set_connected_atom_groups` re-applies it every time the groups are
  rebuilt, and `fragHAR_refinement` rebuilds them on every LS cycle (`molecule.har.foo:106`).
- **`fragHAR` is chosen by the data, not by an option** -- more than one atom group means more
  than one molecule or capped residue, which is exactly what `HAR_refinement` cannot do. The
  SCF kind becomes `fragment-rhf`/`fragment-rks`, damping (0.99) and level shifting (100) are
  switched on to match the `tonto` job, and the whole-of-asymmetric-unit `scf` call is skipped
  -- with several fragments there is no single molecule to converge, and `fragHAR_refinement`
  does the fragment SCF itself. (The `tonto` job has its `scf` keyword commented out for the
  same reason.)
- **`--cluster-radius` with fragments is a `DIE`**, not a silent no-op: self-consistent cluster
  charges are a whole-molecule idea the fragment SCF does not use.

Two library changes came with it, both of which were gaps rather than bugs in `hart`:

- **`MOLECULE.SET:set_molecule_from_atom_group` now honours a group's spin multiplicity.** It
  set `mol.spin_multiplicity = mol.default_spin_multiplicity` unconditionally, so a multiplicity
  given for a group made from the **connection table** -- the ordinary several-molecules case --
  was read, stored in `spin_multiplicity_set`, and then overwritten. The Ryde-capped path
  (`molecule.base.foo:1871`) already got this right; the two now agree.
- **`DIFFRACTION_DATA.SET:set_do_residual_cube`.** There was a `do_residual_cube=` keyword but
  no setter, so an argv-driven program could not turn the cube off -- and it is the largest
  file a refinement writes (9 MB for a dipeptide, 8.9 of the 16 MB this job produced).

Two input-side notes:

- **`--fos 0`.** `hart` defaults the F/sigma cutoff to 3 but `DIFFRACTION_DATA` defaults it to
  off, and the `tonto` job leaves it off. With pruning on, `N_r` would not be 2514. Zero
  disables it (the test in `diffraction_data.set.foo` is `> ZERO`), so no source change was
  needed -- but it is a real trap for anyone comparing `hart` against a `tonto` job.
- **`hart` does not read `tonto`'s keyword hkl format**, only SHELX and free-form. The test's
  `gly_ala_100K_F2.hkl` is the tonto-format file reduced to `h k l F2 sigma` columns; the exact
  `awk` line is recorded in the test's `IO` manifest. Column 4 of the original is junk, which is
  the part worth not getting wrong.

### What broke, when, and when it was fixed

fragHAR was **broken in `tonto` from 2020-01-23 until 2026-06-01**, and the git history pins
both ends. On the first date the predicate deciding "are we refining fragments" was moved off
the CIF:

```foo
-  res = .cif.is_mmCIF AND .cif.use_fragments      ! f0d7cfd3, 2020-01-23
+  res = .crystal.data.refine_fragments
```

with companion commits `bcdcdfb0` ("removed use_fragments cif check") and `59d46d13`. The same
day, `e8ecf99e` records *"Only four tests failing now; fragHAR one of them"*, and `tests/long/
gly_ala_fragHAR_rhf_STO-3G/stdin` lost its `use_fragments= YES` line. It stayed broken for over
six years (`fafce805`, `a0b9da8b`, Feb 2021: *"fraghar still broken"*).

**`.cif.use_fragments` is now a dead flag**: `cif.foo` still has the setter (`:214`), the keyword
(`:291`) and the type field (`types.foo:822`), but **nothing reads it**. Worth deleting, but it is
not what broke fragHAR and it is not what fixed it.

It was **repaired by `d840e322`** (*"fragHAR fixed, gly_ala test and others need to be
modified/checked"*), which arrived from Dylan's `gaussian-IAM` branch alongside the `hart` work
and is live on `antlr4`. The reference `tests/long/gly_ala_fragHAR_rhf_STO-3G/stdout` was
re-blessed at that point and records a genuine converged refinement.

The method paper is Bergmann, Davidson & Jayatilaka, *IUCrJ* **7** (2020) (`fc5039`).

### How grouping actually works (verified, not assumed)

Two mechanisms, and they compose rather than competing:

| mechanism | groups from | role |
|---|---|---|
| `MOLECULE.SET:set_connected_atom_groups` (`molecule.set.foo:1051`) | **connectivity** (`.atom.set_connected_groups`) | general; separate molecules in the asymmetric unit fall straight out. **No capping gate.** |
| `MOLECULE.BASE:set_Ryde_capped_groups` (`molecule.base.foo:1705`) | `compound_sequence_id` (mmCIF residue labels) | *additionally* splits one covalent chain into residues, each with a half-residue cap. Correctly gated on `use_Ryde_capping`. |

So capping is not an alternative path -- it is a refinement applied *within* a bonded chain,
because connectivity alone would lump a whole protein into a single group.

### What a fragment molecule inherits

`MOLECULE.SET:set_molecule_from_atom_group` (`molecule.set.foo:937`):

```foo
mol.charge            = .atom_group(g).charge          ! taken from the group
mol.spin_multiplicity = mol.default_spin_multiplicity  ! DERIVED -- group field IGNORED
mol.set_SCF_guess_defaults_from(.SCF_data)             ! SCF kind cloned from the parent
```

So the SCF kind is **cloned**, not specified -- `hart` does not need a `fragment-rhf` option.
`ATOM_GROUP` already carries `charge :: INT DEFAULT(0)` and
`spin_multiplicity :: INT DEFAULT(1)`, matching the proposed CLI defaults exactly. **But the
multiplicity field is not honoured**: it is overwritten by `default_spin_multiplicity`. The unused
`spin_multiplicity_set :: BIN` flag is presumably what should gate that. Honouring `M` therefore
needs code; `C` works today.

### The CLI (agreed with Dylan)

- **`--mmcif`** -> `m.cif.set_is_mmCIF(TRUE)` before `process_CIF`. The setter exists
  (`cif.foo:196`); `hart` simply never calls it. It should turn Ryde capping on **internally**:
  splitting a covalent chain without capping leaves dangling bonds, so there is no second
  possibility and no `--use-Ryde-capping` option is warranted. (The "mmCIF but treat each chain as
  one group" case is plain HAR, reachable without `--mmcif`.)
- **`--group-charge-spin r C M`** -- **repeatable**, and an **exceptions list**: everything not
  named defaults to `{0 1}`. This mirrors the tonto keyword it replaces, whose name already says
  so: `atom_groups= { keys={charge=} altered_data= {...} }`. Keeps every token well under the
  256-character `STR` limit (see the COMMAND_LINE entry in `DEFERRED.md`); a 300-residue protein
  needs a dozen entries, not 300. `--group-charge-spin-file <file>` as the fallback for large
  cases -- also reproducible and version-controllable, unlike a shell line.
- **`COMMAND_LINE` does not yet support repeated options** (`has_option`/`value_for_option` return
  the first match). Small addition needed.
- Call `fragHAR_refinement` instead of `HAR_refinement` when there is more than one group.

### Order of work

1. ✅ **Serial.** `gly_ala` reproduces through `hart` against the `tonto` reference; see the
   table above. Done 2026-08-02.
2. ⬜ **Parallel is blocked on two open MPI register rows**, both of which fragHAR reaches:
   `fragHAR_refinement` sets `use_disk_SFs(TRUE)` (`molecule.har.foo:52`), which routes through
   `LS_fit_HAs_disk` -> **`make_LS_mx`** -- the same-file-from-every-rank `per_rank_write`
   (register row 1) -- and its SCF goes through **`fragment_SCF_para`** (row 4), whose scheduler
   *changes shape above 2 ranks*, so results are not comparable between `-n 2` and `-n 4` by
   construction. Any parallel fragHAR test must therefore pin a rank count.
3. ⬜ **Rename `use_disk_SFs` -> `use_disk_FFs`** (Dylan): they are atomic **form factors**, not
   structure factors. Cosmetic but it removes a standing confusion, and it touches the same code.
   The files the serial run leaves behind make the case by themselves: 20 of them, named
   `C1-SFs.unknown`, `H10-SFs.unknown`, ... -- wrong noun *and* an `.unknown` extension, which
   suggests the archive genre is not being set either. Worth fixing in the same pass.
4. ⬜ **`--group-charges-file`, for proteins.** The braced list is right for a handful of groups
   and wrong for 300 residues. A file is also reproducible and version-controllable, which a
   shell line is not. Agreed with Dylan as the follow-up, not part of serial H1.

### Archaeology — SETTLED (2026-08-02)

The question was whether a working fragHAR reference exists to aim at, or whether the science
would have to be reconstructed from the paper. It exists, and it is the **current** reference.

Method: extract `tests/long/gly_ala_fragHAR_rhf_STO-3G/stdout` at `ecb593e9` (2019-10-28,
*"Fixed gly-ala fraghar-rhf test"* -- the last commit before the 2020 break) and compare with
today's. No build was needed; `ecb593e9` predates the ANTLR4 translator and would have required
the removed `foo.pl` plus a `cmake_minimum_required` local CMake 4.3.3 rejects.

Both say `No. of atom groups .... 2` and both say `Structure refinement converged.`

| quantity | 2019 (`ecb593e9`) | today | agrees |
|---|---|---|---|
| R(F) | 0.032430 | 0.0324 | yes |
| R(F2) | 0.068683 | 0.0687 | yes |
| Rw(F) | 0.033411 | 0.0334 | yes |
| R_sigma(F) | 0.015952 | 0.0160 | yes |
| R_sigma(F2) | 0.000332 | 0.0003 | yes |
| Effective (mean) sigma^2 | 0.025276 | 0.0253 | yes |
| # of reflections, N_r | 2514 | 2514 | yes |
| # of fit parameters, N_p | 181 | 181 | yes |
| chi^2 / GoF^2 (N_p) | 11.251429 | 11.2462 | 4 s.f. |
| Goodness of fit / GoF (N_p) | 3.354315 | 3.3535 | 4 s.f. |

Today prints 4 dp because the job now sets `real_precision= 4`; the only genuine drift is GoF,
3.3543 -> 3.3535, about **2 parts in 10^4** across six years -- well inside the loose gate. The
line count differs (949 -> 1257) from added per-cycle output, and the labels were renamed
(`chi^2` -> `GoF^2`, `Goodness of fit` -> `GoF`), which is why a naive grep for the 2019 spelling
finds nothing today and briefly suggested the refinement had stopped happening. It had not.

**Conclusion: `tests/long/gly_ala_fragHAR_rhf_STO-3G/stdout` is a sound acceptance target.**
Re-enabling `.cif.use_fragments` is not required and should not be attempted --
`refine_fragments` superseded it correctly.

Two observations recorded rather than acted on, both present in 2019 as well as today, so
neither is a regression and neither blocks H1:

- **`Rw(F2) ....... NaN`** in both. Same root as the NaN/negative-esd item in `DEFERRED.md`
  (least-squares variance-covariance matrix).
- **`# of unmatched Fridel pairs ....... 2514`** -- new since 2019, and it reports *every*
  reflection as unmatched, having displaced 2019's `Scale factor ...... 0.976789` with
  `Using single scale factor ...... T`. Either a benign diagnostic or a real miscount; also
  misspelled (Friedel). Filed in `DEFERRED.md`, to investigate after H1.

### fragHAR under MPI — DONE (2026-08-03)

`mpirun -n 2 hart ... --group-charges '{ 1 -1 }'` now completes with exit 0 and reproduces the
serial reference: **R(F) 0.032423, GoF 3.353475**, and the second block **0.089265 / 12.034623**
-- digit for digit, every reported statistic, 1586 lines against 1586. The only textual
differences are the banner (version, build date, timers), one `WARN` line that is live in the
debug build and compiles away in the release build that blessed the reference, and one
`Making gaussian ANO data ...` progress line that is skipped because
`make_ANOs_and_interpolators` returns early when `.has_all_ANO_matrices`.

Getting there took three defects, in increasing order of subtlety.

**1. `SYSTEM:set_per_rank_IO_allowed` assigned the wrong member.** It set `.keyword_echo`, not
`.per_rank_IO_allowed`. Longstanding -- `set_parallel_IO_allowed` had the identical body before
the rename -- so the escape hatch in `SYSTEM:IO_is_allowed` had **never once been reachable**,
every "let each rank do its own I/O" call site in `molecule.scf.foo` was a silent no-op, and
non-master ranks simply dropped their writes. Symptom: rank 1 ran the ALA fragment, wrote its
28 KB log, and produced no `2-ALA.density_mx,r` at all. Nothing warned.

**2. The per-rank mode was scoped to the wrong region.** It was switched on and then off again
*inside* the fragment loop body, so the I/O bookkeeping broadcasts resumed while the two ranks
were on different fragments. It has to be set once, outside the loop, on every rank -- including
the >2-rank master, which schedules and `cycle`s without ever entering the body.

**3. Object state diverges permanently after a per-rank loop, and output branches on it.**
This is the one worth remembering. Each rank builds grids and densities only for the fragment
*it* ran, so `.mol(g).becke_grid` is allocated on one rank and not on another. `MOLECULE:put`
branches on exactly that (`if (.becke_grid.allocated) .put_becke_grid`), so the ranks issue
different numbers of TEXTFILE bookkeeping broadcasts. Measured: after `put: cluster done`,
master issued 42 broadcasts and rank 1 issued **zero**. The ranks then fell out of step and a
later collective paired with the wrong one -- surfacing, misleadingly, as rank 1 dying in
`TEXTFILE:close` with *"not an existing file!"*, because its `.exists` broadcast received
another rank's payload.

The fix is to stop `MOLECULE.PUT:put_atom_group_mols` being collective at all: it is pure
output, so it switches per-rank I/O on (which makes the TEXTFILE bookkeeping non-collective)
and lets the master write alone, restoring the caller's mode afterwards.

**The general rule this establishes:** *after* a per-rank region, the ranks' object graphs are
deliberately different. Any later shared-mode code that branches on allocation status, array
extent, or convergence flags of that per-rank data will desync. Either resynchronise the state,
or keep the code non-collective. Recorded in `docs/TONTO_DEVELOPER_INFO.md` §1a and `docs/TONTO_AND_MPI.md`.

**How it was found.** By tracing, not by reading -- three consecutive readings of the code
pointed at the wrong routine. A `write` at the single `MPI_BCAST` choke point in
`parallel.foo`, logging `(datatype, count)` to a per-rank `fort.7<rank>` file (Fortran
auto-connects the unit, so the two streams cannot interleave), plus positional `TAG` markers.
Diffing the two streams gives the exact call where they part; segmenting the counts between
tags names the routine. That recipe is in `docs/TONTO_DEVELOPER_INFO.md` §1a.

**H2 — revive the frozen options.** `--charge`, `--mult`, `--ldtol`,
`--scf-guess`, `--anharm`, `--wavelength` and `--4th-order-only` are commented
out in both the `select case` block and the help text. Reviving one means
uncommenting both halves; the invariant check compares only uncommented labels,
so a half-revived option is caught.

**H3 — ✅ DONE (2026-08-09).** The plot names now come from the job.
`MOLECULE.SCF` passes its `.name` to `CRYSTAL:put_correction_data`, which heads
every plot file with it; an unnamed job (`MOLECULE.name` still `"unknown"`)
falls back to `stdout`, so nothing that worked before changed name. Nothing in
`tests/` compares a plot file — the 16 references are all `delete:` lines, and
`scripts/test.py:314` records `delete:` as *"recorded but unused"* — so this
could not break a test, and did not.

**H4 — test the `.cif2` restart round trip.**

Tracked alongside the rest of the project's deferred work in `DEFERRED.md`.

---

## The Windows executables have never been run on Windows

**Status: open, 2026-08-10. Deliberately carried into the IUCr Calgary lab.**

`tonto.exe` and `hart.exe` are cross-compiled from Linux with MinGW-w64
(`cmake/mingw-w64-toolchain.cmake`, the `windows-exe` job of
`.github/workflows/release.yml`) and shipped in
`tonto-<version>-windows-x86_64.zip`. Native Windows has never been a tested
platform for Tonto, and no one has run these binaries on a Windows machine.

**What is known.** They link as `PE32+ executable (console) x86-64`, they are
statically linked so no DLLs travel with them, and under wine on a Linux runner
`tonto.exe --version` prints the version and `hart.exe` reproduces workshop
exercise 1 exactly: R(F) = 0.010071, the reference value. That is the whole of
the evidence.

**Why that is not enough.** Wine is not Windows. The parts most likely to
differ are the ones wine emulates rather than implements: path handling for the
`--basis-dir` argument, console I/O buffering in `TEXTFILE`, the `system()` call
Tonto makes to run gnuplot, and file deletion at the end of a job.

**How it will be resolved.** Live, in the lab, by whoever brings a Windows
laptop. If a binary misbehaves, the fallback is in the same section of
`workshop/WORKSHOP.md`: WSL plus the Linux tarball, which is tested in CI on
every push to `release` and `master`.

**If it works**, the WSL instructions can be demoted to a footnote for Windows
users. **If it does not**, the fix is to reproduce the failure under wine
first — the cross-build is cheap to iterate on — and only then to blame
Windows.

---

## Two local `ctest` failures that CI does not see: zero-esd formatting

**Status: understood, not fixed, 2026-08-10.**

A full local `ctest` on `release/` gives 131/133. The two failures are
`tests/hart/urea_hart_STO-3G_disk_ffs` and
`tests/long/gly_ala_fragHAR_rhf_STO-3G`, and both report the same thing:

```
exact=FAIL  rel<=0.2%=FAIL(max 0%)  lastdig<=2=FAIL(max 0 ulp)  =>  LOOSE=FAIL
    1 structural/alignment mismatch(es)
```

**Zero numeric difference, zero ulp.** The mismatch is in the torsion-angle
table, where a zero angle carries a zero esd:

```
reference (blessed on Darwin-25.5.0):    O---C--N1--H1     0.000000(0)
this machine (Linux):                    O---C--N1--H1   0.00000000(1)
```

The value is 0 either way. What differs is how many digits the esd printer
emits when the esd is at the denormal edge — 0 on the machine that blessed the
reference, 1 in the last place here — and that changes the token alignment, so
`test.py`'s structural check fires while every numeric check passes.

**It is not a regression.** `gly_ala_fragHAR_rhf_STO-3G` runs `tonto`, a binary
in that build tree older than the day's only source change (`--defragment` in
`runfiles/run_har.foo`, which builds `hart`). And **Linux CI does not see it**:
the same `urea_hart_STO-3G_disk_ffs` reports `FAIL PASS PASS` there — exact
fails, loose passes — in a green 54/54 run.

**What is worth doing, when there is time.** Either rebless the two references
on Linux, or make the esd printer's digit count independent of a zero esd,
which is the real fix: a zero esd should not be formatted by the same
digit-counting rule as a measured one. Until then the two tests fail only on
machines whose formatting differs from the Mac that blessed them.

**Correction, 2026-08-10, same day.** Reblessing
`tests/hart/urea_hart_STO-3G_disk_ffs` on this machine **made Linux CI red**,
and the reblessed reference has been reverted.

The variable is not the operating system, as the Darwin banner on the old
reference suggested. It is the **compiler version**: this machine has
GNU 14.3.0 and both the CI runner and the machine that blessed the reference
have 14.2.0. So the odd one out is here, and blessing from here breaks
everybody else. With the original reference, CI reports the test as
`exact=FAIL, loose=PASS` and is green; with mine, it fails the loose gate on a
structural mismatch of zero numeric difference.

The `gly_ala_fragHAR_rhf_STO-3G` rebless is kept: that difference was an extra
keyword-echo line, not a formatting one, and the test is in the `long` suite,
which CI does not run.

**So the item stands, with a sharper statement of it:** the esd printer's digit
count for a zero esd varies between gfortran point releases, and any reference
containing a zero esd is therefore only valid for the compiler that blessed it.
The fix is in the printer, not in the references.

### UPDATE 2026-08-18 (LATER, AND IT SUPERSEDES THE SECTION BELOW): the fix was WRONG, and the whole class is now a DECISION TO LIVE WITH IT

Read this before the section that follows, which is left in place only because
its reasoning is instructive about how the mistake was made.

**The esd fix was wrong and is reverted (`e522e1ce` reverts `26d9166a`).** The
premise -- "a `dp` exceeding `max_dp` means the esd is too small to print" --
is false: `dp = min(abs(floor(log10(err))),max_dp) + 1`, so `dp = max_dp+1` is
the NORMAL outcome for any esd at or below the clamp. The change therefore
destroyed real esds on every compiler:

    reference   1   N  7.0  1.0811(3)   ...  0.03615(19)
    with fix    1   N  7.0  1.081(0)    ...  0.036(0)

**How it survived local testing, which is the part worth remembering:** it was
verified against the ctest pass/fail gate rather than against the diff. The
gate is loose by design (0.03615 -> 0.036 is 0.4%, and that test carries a
relaxed last-digit tolerance), so `nh3_rhf_DZP_HAR` reported "Passed" while its
output was visibly wrong. CI found it in one run. **Check the diff, not the
gate**, when changing anything that formats output.

**What the harness actually does, measured in `scripts/test.py`, because three
separate discussions have guessed at it:**

- Tokenising is `.split()`, so **pure whitespace/column-width drift is
  invisible to every criterion**. It costs nothing in ctest or `make report`;
  it is a `vimdiff` irritant only.
- `value(esd)` is **not** compared as a string. `token_agreement` calls
  `split_value_esd` and applies the normal tolerances to the value and
  last-digit slack to the esd. So `180.00000000(1)` vs `180.0000000(0)` PASSES
  loose -- confirmed on CI, where `urea_hart_STO-3G_disk_ffs` came back
  loose PASS at 0.00569% / 0.43 ulp.
- A **token-count** difference fails every criterion including loose. This is
  how width drift can still bite: these tables are dense enough that one extra
  character merges `O1` with the value beside it, dropping a token. That, not
  the digits, is the "structural mismatch of zero numeric difference" that
  reddened CI on 2026-08-10.

**DECISION (Dylan, 2026-08-18): stop fixing this class; live with it.** The
formatting work is worth human legibility and the rare token-merge hard-fail,
not correctness -- the harness already absorbs esd drift. Do not spend more
time on the printer.

**If it is ever revisited, the honest fix is upstream and is NOT a print
threshold** (Dylan): a parameter fixed by site symmetry should get an exactly
zero esd BY CONSTRUCTION -- constrained out of the normal matrix rather than
refined and then filtered by eigenvalue. Then the zero is exact on every
machine with no tolerance to justify. This is the same root as the open NaN and
negative-esd item elsewhere in this file: parameters that should never have
been in the variance-covariance matrix at all.

**Separately, and this one IS fixed:** the "Form factor asymmetry" table was
printing floating-point noise. For a molecular HAR the asymmetry is ~1e-15 and
the reflection at which the maximum occurs is the argmax of that noise, so the
same urea job gave 0.818E-15 at (0,5,2) here and 0.843E-15 at (-1,-1,-3) on CI
-- 100-167% relative disagreement, which is what actually reddened CI on
2026-08-18 (NOT the formatting). Now floored at `FF_ASYMMETRY_TOL = TOL(12)`
in `include/macros.in`: below that the value prints as an exact zero and its
meaningless hkl is dropped, and the columns print **one significant digit**,
since the table is a diagnostic and only the order of magnitude matters. Real
asymmetry is untouched -- pHAR runs 1e-9 to 5e-8.

Note the irony worth keeping: the old double-square-root bug HID this. A fourth
root compressed 1e-15 noise up to ~1e-8 and printed it to one digit, which was
stable across machines by accident. Fixing the mathematics exposed the noise
the wrong formula had been concealing.

### UPDATE 2026-08-18: half of this is fixed; the other half is open and named

**Fixed and pushed (`26d9166a`): the esd digit count.** `REAL:get_dp_de_le`
clamps the error's decimal place at `max_dp` and then emitted an error digit at
`dp = max_dp+1` anyway, so an esd of about 1e-8 out of a covariance matrix
printed `180.00000000(1)` under gfortran 14.2.0 and `180.0000000(0)` under
14.3.0 — the value being an exact zero on one compiler and denormal-scale noise
on the other, taking different branches. The fix needs no threshold: after the
1-digit reduction, `dp > max_dp` means the esd needed more decimals than the
caller allows, so it prints as zero. **It should converge 14.2.0 onto the
14.3.0 output**, which is what the references blessed in `d7d2caaa` contain.

**UNVERIFIED AT THE TIME OF WRITING.** Locally this is a no-op — 14.3.0 already
produced the zeros — so the proof must come from CI, which runs 14.2.0. The
check is simply: are `nh3_rhf_DZP_HAR` (label `short`) and `urea_hart_STO-3G`
(label `hart`) green on GitHub for `26d9166a` or later? **If they are red, the
diagnosis is wrong and those two references, plus
`nh3_rhf-consistent-cluster-charge_DZP_HAR`, must be reverted to their
pre-`d7d2caaa` contents** — they were blessed from this 14.3.0 box deliberately
(Dylan, 2026-08-18) to get the corrected asymmetry table in, accepting the risk.
Also left deliberately unblessed: `urea_hart_STO-3G_disk_ffs`, which after the
fix agrees loosely on its first file and differs by 0.43 ulp on the second
(fewer printed decimals). Bless it only once CI is known good.

**Open, and NOT the same bug: column widths drift by one character.** In the
*cartesian* ADP table only, every entry is byte-identical between compilers but
the gap between two columns moves by one space:

```
-  1   S  0.01561(7)  0.01747(8)  0.02029(8)   0.00000(4)  0.00000(3)   0.00000(8)
+  1   S  0.01561(7)  0.01747(8)  0.02029(8)   0.00000(4)   0.00000(3)   0.00000(8)
```

Seen in `so2_rhf_DZP_anharmonic_cluster_charge_XWR` (widens) and both
`nh3_*_HAR` tests (narrows). What has been **ruled out** by comparing the
references character by character, so nobody repeats it:

- Not the values — no printed entry differs.
- Not all ADP tables — the wide `U_xx…Axis` table with esds is identical across
  compilers. Only the cartesian one moves.
- Not a missing guard in the width path: `REAL:get_sl_cb` derives the width from
  the same `get_dp_de_le` call that produces the text, so the two cannot
  disagree about an entry. Per-entry arithmetic gives 11 characters on both.

That leaves the column-level maximum — `max_cb`, the padding that aligns closing
braces — as the only place a difference can enter with no entry changing. Start
there.

**The measurement that was in flight when this was written:** a full
`gfortran-13` build in a scratch tree (13, 15 and 16 are all installed here;
there is no 14.2.0 on this machine, which is why the esd half could not be
verified locally). If 13 or 15 reproduces the width drift against 14.3.0, that
is a local reproducer and the bisect can proceed without CI. **If all of them
agree, the drift is not the compiler at all** — it would then be something about
the machine that blessed the reference, and the hunt moves elsewhere. Either
outcome is worth knowing; run it and record which.

---

## Exercise 4 should use Tonto's own contour plotting, not a hand-rolled script

**Status: open, 2026-08-11.** Dylan's observation, and he is right.

`workshop/WORKSHOP.md` exercise 4 writes the grid with `plot_format= gnuplot`
and draws it with `examples/4-urea-deformation/deformation.gnuplot`, which
computes its own logarithmic contour ladder. Tonto already has that facility:

```
   plot_format= gnuplot.contour
   contour_scale= log            ! the default
   contour_min_value= -3.0       ! log10 of the smallest contour
   contour_increment=  0.5       ! half a decade
   contour_max_value=  0.0       ! log10 of the largest
```

which makes Tonto write three files per plot — `*.contour_data`,
`*.bond_data` and a `*.commands` gnuplot script — and the script draws the map
with a **logarithmic colour bar labelled in powers of ten**, positive and
negative, and the bonds overlaid. Verified working: it reports

```
Minimum +ve log10 contour =  -3.0
Maximum +ve log10 contour =   0.0
Contour increment         =   0.5
No. of contours           =   7
Total no. of contours     =  15
```

**Why it was not adopted before the lab.** Two things need sorting first, and
neither is deep:

1. The generated script targets `xterm` and then `postscript eps`. Rendering
   to PNG means overriding both terminal lines, and under `pngcairo` the
   result comes out black — the script's colours assume the EPS terminal's
   white ground, and `background rgb 'white'` alone does not fix it.
2. The difference map (constrained minus unconstrained) is made by subtracting
   two plain grids. The contour format writes a different file, so keeping
   that picture means either a second pair of plots in plain format or
   subtracting in the contour data.

**The right end state** is exercise 4 using `gnuplot.contour` and the workshop
telling the reader to run `gnuplot -persist <job>,<kind>,gnuplot.commands`,
with no hand-written script at all.

---

## `rgbi_doctor.sh` is installed as a command but named as a script

**Status: open, 2026-08-11.** Cosmetic, and a rename with call sites.

`CMakeLists.txt` installs three programs into `bin`: `make-rgbi-pic`,
`make-rgbi-dials` and `scripts/rgbi_doctor.sh`. The first two are named as
commands should be, with no extension; the third carries `.sh` into `bin` and
lives in the directory that is otherwise *not* installed.

If it is a user-facing command it should be `rgbi-doctor` in `rgbi-scripts/`.
That means updating both drivers (which call it for
`--print-template-dir` and the preflight), `ctest`'s `rgbi_doctor_selftest`,
`scripts/rgbi_selftest.sh`, the CI workflows, and four documents — so it is
worth doing deliberately, not as a side effect.
