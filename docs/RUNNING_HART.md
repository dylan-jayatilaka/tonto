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

These are settings, not different science: give a `tonto` job the same ones and
the two agree digit for digit, esds included. But
`tests/long/urea_rhf_STO-3G_HAR` is *not* one of those jobs: it optimises the scale factor and uses tighter tolerances, so `hart` will
not reproduce that particular reference, and `tests/hart/` carries its own.

More than one molecule, or capped residue, in the asymmetric unit **is**
supported: `hart` counts the atom groups and calls `fragHAR_refinement` instead
of `HAR_refinement` when there is more than one. This works in serial and under
MPI.

## 2. Options

All options are GNU long options — `--name`. A single-dash spelling is
rejected with a message naming the replacement:

```
$ hart -basis STO-3G urea.cif
Error in COMMAND_LINE:process_options ... options must be given in GNU long
form, use '--basis' instead of '-basis'
```

There are **no single-letter aliases**: no `-b` for `--basis`, not even `-h`
for `--help`.

`hart <cif-file>` takes exactly one non-optional argument: a `.cif` or `.cif2`
file. A `.cif2` file is what a previous `hart` run writes and what a restart
reads.

| Option | Value | Default | Effect |
|---|---|---|---|
| `--basis` | basis-set name | `def2-SVP` | One of `STO-3G`, `def2-SVP`, `def2-TZVP`, `def2-TZVPP`, `cc-pVDZ`, `cc-pVTZ`, `cc-pVQZ`. A file of that name must exist in the basis directory. |
| `--basis-dir` | directory | `$TONTO_BASIS_SET_DIRECTORY`, else `./basis_sets` | Where the basis-set library lives. |
| `--cluster-radius` | Å | 0 | Model the potential of whole molecules within this radius by Hirshfeld charges and dipoles. Maximum 10. |
| `--defragment` | `t`/`f` | `t` | Apply the crystal symmetry to grow each fragment in the CIF into whole molecules, before anything is refined. A CIF holds the asymmetric unit — a quarter of a molecule for urea — and HAR runs a quantum calculation on whatever fragment it is handed, so refining an incomplete molecule converges to a meaningless answer. **Set `f` for a network solid** (diamond, silica, a coordination polymer): there is no whole molecule to complete, the growth has no stopping point, and the run will not terminate. Since the default is `t`, a network solid *must* turn it off. |
| `--complete-mol` | `t`/`f` | `t` | An accepted spelling of `--defragment`, kept because existing scripts write it. Identical in effect. |
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
exits **non-zero**.

## 3. Output files

For `--job urea`:

| File | Contents |
|---|---|
| `urea.out` | The main log: banner, options echoed back, SCF, refinement cycles, statistics. |
| `urea.err` | Error stream. Empty on a clean run. |
| `urea.archive.cif` | **The result.** Refined fractional coordinates and ADPs with esds, in a standard asymmetric-unit CIF. |
| `urea.cartesian.cif2`, `urea.HBB.cif2`, `urea.fractional.cif1` | Enhanced Tonto CIFs; the `.cif2` is what a restart needs. |
| `urea.archive.fcf`, `urea.archive.fco`, `urea.fcf6` | Structure-factor files. |
| `urea.QQ_plot*`, `urea.F_z_*`, `urea.Delta_F_*` | Data, gnuplot script and `.png` for each diagnostic plot. |

## 4. How `hart` is tested

The suite is `tests/hart/`, label `hart`. The harness (`scripts/test.py`) was
built for `tonto`, which reads `stdin` and writes `stdout`; `hart` reads neither
and names its own output, so two `IO`-manifest keys bridge the gap:

```
program: hart
args:    --job urea --basis STO-3G --grid-accuracy low urea_init.cif
input:   urea_init.cif
output:  urea.out
output:  urea.archive.cif
```

`program:` is resolved as a sibling of `--program`, so `hart` is found in
whichever build tree is under test. `args:` is split shell-style and appended.
The reference that matters is `urea.archive.cif`: it carries the refined
coordinates and ADPs as `0.02071(18)`, which `test.py` compares with its
`value(esd)` comparator, so the test checks the scientific result rather than
log text.

**The invariant check** (`scripts/check_hart_options.sh`, ctest name
`hart_options`, label `hart`) compares the option headings in the live
`hart --help` output with the `case ("…")` labels in `runfiles/run_har.foo`.
Any name in one and not the other fails the test, which is what keeps this
page's option table honest.

Run it all with:

```bash
ctest -L hart          # the suite
make report            # the per-suite agreement table, hart included
```

CI runs `--suites short hart`.

## 5. Known rough edges

- `<job>.err` is left behind on a clean run rather than deleted, and a run that
  stops early (`--help`, a usage error) leaves a stray file called `stderr`.
  `tonto` does the same, which is why so many `IO` manifests list `delete: stderr`.
- The `.cif2` restart cycle is accepted but untested end to end.
- The refinement reports **19 near-zero eigenvalues out of 27 parameters** on the
  urea test job. That is the normal matrix being close to singular; it is
  reproduced exactly by `tonto` with the same settings, so it is not a `hart`
  defect, but it is worth understanding before trusting esds from a job with
  more parameters.

Development history — what was wrong with `hart` before it worked, the fragHAR
milestones and the MPI work — is in [`../DEFERRED.md`](../DEFERRED.md).
