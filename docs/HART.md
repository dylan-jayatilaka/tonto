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

The consequence worth knowing: `hart` cannot refine a structure with more than
one molecule in the asymmetric unit, because that needs `fragHAR_refinement`
and `hart` never calls it. See §6, milestone H1.

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
| `--grid-accuracy` | level | `low` | `very_low`, `low`, `medium`, `high`, `very_high` or `extreme`. |
| `--h-adps` | `t`/`f` | `t` | Refine the H-atom ADPs. |
| `--h-iso` | `t`/`f` | `f` | Set isotropic H ADPs and refine isotropically. |
| `--h-pos` | `t`/`f` | `t` | Refine H-atom positions. |
| `--help` | — | — | Print the full documentation and exit 0. |
| `--job` | name | head of the CIF name | Names the output file `<job>.out` and the other intermediates. |
| `--scf` | `rhf`/`rks` | `rhf` | The SCF wavefunction. |
| `--shelx-f`, `--shelx-f2` | file | — | Reflections in SHELX fixed format `(I4,I4,I4,8F,8F)`. |
| `--std-f`, `--std-f2` | file | — | Reflections in free, whitespace-separated format. |
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
  overwrite each other's plots. One of them, `stdout.QQ_plot.gunplot`, is a
  typo for `.gnuplot` — its own header says "Gnuplot input file".
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

**H1 — fragHAR support.** The one that matters. `hart` calls only
`HAR_refinement`, never `fragHAR_refinement`, and has no atom-group or
per-fragment-charge path, so a crystal with more than one molecule in the
asymmetric unit cannot be refined. `tests/long/gly_ala_fragHAR_rhf_STO-3G`
exercises fragHAR through `tonto` and is the acceptance test. Dylan's
`gaussian-IAM` branch has a commit "fragHAR fixed, gly_ala test and others need
to be modified/checked" — read it first.

**H2 — revive the frozen options.** `--charge`, `--mult`, `--ldtol`,
`--scf-guess`, `--anharm`, `--wavelength` and `--4th-order-only` are commented
out in both the `select case` block and the help text. Reviving one means
uncommenting both halves; the invariant check compares only uncommented labels,
so a half-revived option is caught.

**H3 — derive the `stdout.*` scratch names from `<job>`,** and fix the
`.gunplot` typo. In `foofiles/diffraction_data.put.foo` and
`foofiles/vec{reflection}.foo`; it affects `tonto` HAR jobs too.

**H4 — test the `.cif2` restart round trip.**

Tracked alongside the rest of the project's deferred work in `DEFERRED.md`.
