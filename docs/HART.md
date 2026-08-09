# `hart` — standalone Hirshfeld atom refinement

`hart` is a command-line program that performs a Hirshfeld atom refinement (HAR)
on a crystal structure in a CIF file. It is built from `runfiles/run_har.foo`
and links the same Tonto library as `tonto`; CMake names the target `run_har`
and the executable `hart`.

```
hart --job urea --basis STO-3G --grid-accuracy low urea_init.cif
```

Everything `hart` does is also reachable from a `tonto` job file. `hart` exists
so that a HAR can be driven from a script or a GUI without writing one.

---

## 1. How `hart` differs from a `tonto` HAR job

`hart` is not a general interface to the library — it is one workflow with most
of the choices already made. Compare `tests/long/urea_rhf_STO-3G_HAR/stdin`
(the `tonto` route) with what `hart` hard-codes:

| Setting | `hart` | `tonto` job file |
|---|---|---|
| Partition model | `oc-hirshfeld`, always | `partition_model=` keyword |
| Data kind | `x-ray`, always | `data_kind=` keyword |
| Charge / multiplicity | 0 / 1, always | `charge=` / `multiplicity=` |
| Initial density | promolecule, always | `initial_density=` keyword |
| SCF convergence | `TOL(3)` = 1e-3, always | `convergence=` keyword |
| DIIS convergence | `--dtol`, default 1e-2 | `diis= { convergence_tolerance= }` |
| Scale factor | not optimised | optimised in the `tonto` HAR path |
| Refinement | `HAR_refinement` | any, incl. `fragHAR_refinement` |

These are settings, not different science. Give a `tonto` job the same ones and
the two agree **digit for digit**, esds included — that is how `hart`'s output
was validated (§4a). But `tests/long/urea_rhf_STO-3G_HAR` is *not* one of those
jobs: it optimises the scale factor and uses tighter tolerances, so `hart` will
not reproduce that particular reference, and `tests/hart/` carries its own.

More than one molecule (or capped residue) in the asymmetric unit **is**
supported, as of milestone H1: `hart` counts the atom groups and calls
`fragHAR_refinement` instead of `HAR_refinement` when there is more than one.
Serial only for now — see §6.

## 2. Options

All options are GNU long options — `--name`. A single-dash spelling is
rejected with a message naming the replacement:

```
$ hart -basis STO-3G urea.cif
Error in COMMAND_LINE:process_options ... options must be given in GNU long
form, use '--basis' instead of '-basis'
```

This changed in the `--long-options` migration, which removed single-dash
options from **every** Tonto program at once (`tonto` lost `-i`/`-o`/`-b` and
`-h`/`-v` in the same change). There are deliberately **no single-letter
aliases**: no `-b` for `--basis`, no `-g` for `--grid-accuracy`, not even `-h`
for `--help`. Option names are self-documenting or they are not documented.

`hart <cif-file>` takes exactly one non-optional argument: a `.cif` or `.cif2`
file. A `.cif2` file is what a previous `hart` run writes and what a restart
reads.

| Option | Value | Default | Effect |
|---|---|---|---|
| `--basis` | basis-set name | `def2-SVP` | One of `STO-3G`, `def2-SVP`, `def2-TZVP`, `def2-TZVPP`, `cc-pVDZ`, `cc-pVTZ`, `cc-pVQZ`. A file of that name must exist in the basis directory. |
| `--basis-dir` | directory | `$TONTO_BASIS_SET_DIRECTORY`, else `./basis_sets` | Where the basis-set library lives. |
| `--cluster-radius` | Å | 0 | Model the potential of whole molecules within this radius by Hirshfeld charges and dipoles. Maximum 10. |
| `--complete-mol` | `t`/`f` | `t` | Complete molecules for the cluster point charges. Set `f` for network compounds. |
| `--dispersion` | quoted list | none | Dispersion coefficients, e.g. `--dispersion 'S 0.04370 0.04026 Cl 0.05457 0.05224'`. |
| `--disk-sfs` | `t`/`f` | `f` | Write the aspherical atomic structure factors to disk. |
| `--dtol` | real | 0.01 | DIIS convergence tolerance. Must lie between 1e-5 and 0.1. |
| `--extinction` | `t`/`f` | `f` | Correct extinction via the Larson formula. |
| `--fos` | ratio | 3 | Reject reflections with F/sigma below this. |
| `--fzcut` | z-score | off | Prune reflections whose `(F_pred-F_expt)/sigma` exceeds this. Needs a CIF from a previous HAR. |
| `--group-charges` | braced list | all 0 | Charge on each atom group, in group order, e.g. `--group-charges '{ 1 -1 }'`. One entry per group; `hart` stops if the count differs. Quote it: the whole list is one option value. |
| `--group-multiplicities` | braced list | all 1 | Spin multiplicity 2S+1 of each atom group, same form and same one-per-group rule. |
| `--grid-accuracy` | level | `low` | `very_low`, `low`, `medium`, `high`, `very_high` or `extreme`. |
| `--h-adps` | `t`/`f` | `t` | Refine the H-atom ADPs. |
| `--h-iso` | `t`/`f` | `f` | Set isotropic H ADPs and refine isotropically. |
| `--h-pos` | `t`/`f` | `t` | Refine H-atom positions. |
| `--help` | — | — | Print the full documentation and exit 0. |
| `--job` | name | head of the CIF name | Names the output file `<job>.out` and the other intermediates. |
| `--mmcif` | `t`/`f` | `f` | Read the CIF as an mmCIF (`_atom_site.` names, residue labels). Residues are then Ryde-capped and refined by fragHAR. |
| `--residual-cube` | `t`/`f` | `t` | Write a residual-density cube over the unit cell. By far the largest file a refinement produces (9 MB for a dipeptide). |
| `--scf` | `rhf`/`rks` | `rhf` | The SCF wavefunction. |
| `--shelx-f`, `--shelx-f2` | file | — | Reflections in SHELX fixed format `(I4,I4,I4,8F,8F)`. |
| `--std-f`, `--std-f2` | file | — | Reflections in free, whitespace-separated format. |
| `--wavelength` | Å | from the CIF | X-ray wavelength. Needed only when the CIF has no `_diffrn_radiation_wavelength`; it defines each reflection's Bragg angle. |
| `--version` | — | — | Print the version and exit 0. |

**Restrictions**, all enforced with a message: zero overall charge; an even
number of electrons; every symmetry-unique atom present in the fragment; no
disorder; elements up to Z=36 only; no transition metals (Z=21–30).

**Exit status.** `--help` and `--version` exit 0. Every failure — a usage
error, an unknown option, an unreadable basis, any `DIE` inside the library —
exits **non-zero**. This was not true before: `SYSTEM.die` ended in a bare
`stop`, which exits 0, so `hart` reported success while dying. That is why the
program could sit broken and untested for so long — no harness could see it.

## 3. Output files

For `--job urea`:

| File | Contents |
|---|---|
| `urea.out` | The main log: banner, options echoed back, SCF, refinement cycles, statistics. |
| `urea.err` | Error stream. Empty on a clean run. |
| `urea.archive.cif` | **The result.** Refined fractional coordinates and ADPs with esds, in a standard asymmetric-unit CIF. |
| `urea.cartesian.cif2`, `urea.HBB.cif2`, `urea.fractional.cif1` | Enhanced Tonto CIFs; the `.cif2` is what a restart needs. |
| `urea.archive.fcf`, `urea.archive.fco`, `urea.fcf6` | Structure-factor files. |
| `stdout.*` (seven files) | Gnuplot input for the residual and Q-Q plots. **Their names ignore `--job`** — see §6. |

## 4. How `hart` is tested

`hart` had no test of any kind until the `tests/hart/` suite was added. The
harness (`scripts/test.py`) was built for `tonto`, which reads a file called
`stdin` and writes one called `stdout`; `hart` reads neither and names its own
output. Two `IO`-manifest keys bridge the gap:

```
program: hart
args:    --job urea --basis STO-3G --grid-accuracy low urea_init.cif
input:   urea_init.cif
output:  urea.out
output:  urea.archive.cif
```

- `program:` is resolved as a **sibling of `--program`**, so `hart` is found in
  whichever build tree is under test without any extra path being configured.
- `args:` is split shell-style and appended. There is **no token substitution**
  in it, deliberately: `hart` echoes its own command line into `<job>.out`, so
  an absolute `--basis-dir` would bake a machine-specific path into the
  reference. The `TONTO_BASIS_SET_DIRECTORY` fallback removes the need — the
  harness already exports it.
- A manifest with a `program:` key does **not** get the `stdin`/`stdout`
  defaults, since they would both be wrong.

`urea.archive.cif` is the reference that matters. It carries the refined
coordinates and ADPs with esds in `0.02071(18)` form, which is exactly what
`test.py`'s dedicated `value(esd)` comparator handles — so the test compares
the scientific result, not just log text.

**The invariant check** (`scripts/check_hart_options.sh`, ctest name
`hart_options`, label `hart`) compares two independently derived sets: the
option headings in the live `hart --help` output, and the uncommented
`case ("…")` labels in `runfiles/run_har.foo`. Any name in one and not the
other fails the test. It also asserts every documented option is spelled `--`,
and that each documented failure mode exits non-zero.

This exists because the help and the code *had* drifted: `--disk-sfs` was
documented in full while its `case` label sat commented out, so using the
documented option killed the run with "unknown option". A stored reference
would not have caught that — help text and code would have been blessed
together. This check cannot be blessed.

Run it all with:

```bash
ctest -L hart          # the suite
make report            # the per-suite agreement table, hart included
```

CI runs `--suites short hart`.

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

## 5. Known defects

- The `stdout.*` plot files ignore `--job`, so two runs in one directory
  overwrite each other's plots. (The `stdout.QQ_plot.gunplot` typo that used to
  sit alongside this was fixed on 2026-08-09 with the gnuplot plot work.)
- `<job>.err` is left behind on a clean run rather than deleted, and a run that
  stops early (`--help`, a usage error) leaves a stray file called `stderr`.
  `tonto` does the same, which is why so many `IO` manifests list `delete: stderr`.
- The `.cif2` restart cycle is accepted but untested end to end.
- The refinement reports **19 near-zero eigenvalues out of 27 parameters** on the
  urea test job. That is the normal matrix being close to singular; it is
  reproduced exactly by `tonto` with the same settings, so it is not a `hart`
  defect, but it is worth understanding before trusting esds from a job with
  more parameters.

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
or keep the code non-collective. Recorded in `docs/DEVELOPER.md` §1a and `docs/MPI.md`.

**How it was found.** By tracing, not by reading -- three consecutive readings of the code
pointed at the wrong routine. A `write` at the single `MPI_BCAST` choke point in
`parallel.foo`, logging `(datatype, count)` to a per-rank `fort.7<rank>` file (Fortran
auto-connects the unit, so the two streams cannot interleave), plus positional `TAG` markers.
Diffing the two streams gives the exact call where they part; segmenting the counts between
tags names the routine. That recipe is in `docs/DEVELOPER.md` §1a.

**H2 — revive the frozen options.** `--charge`, `--mult`, `--ldtol`,
`--scf-guess`, `--anharm`, `--wavelength` and `--4th-order-only` are commented
out in both the `select case` block and the help text. Reviving one means
uncommenting both halves; the invariant check compares only uncommented labels,
so a half-revived option is caught.

**H3 — derive the `stdout.*` scratch names from `<job>`.** In
`foofiles/diffraction_data.put.foo` and `foofiles/vec{reflection}.foo`; it
affects `tonto` HAR jobs too. (The `.gunplot` half of this item is **done** —
renamed to `.gnuplot` on 2026-08-09.)

**H4 — test the `.cif2` restart round trip.**

Tracked alongside the rest of the project's deferred work in `DEFERRED.md`.
