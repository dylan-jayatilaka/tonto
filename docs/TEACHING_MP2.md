# The MP2 teaching lab

**Ported 2026-08-18 from tag `archive/Teaching`** (Max Davidson 5 commits, Dylan
Jayatilaka 3, June–October 2019). Two programs, neither built by default:

| Program | Build with | What it is |
|---|---|---|
| `runfiles/run_mp2.foo` | `make run_mp2` | A working restricted MP2 program, ~40 lines of actual physics. Validated against the library. |
| `runfiles/run_mp2_exercise.foo` | `make run_mp2_exercise` | The same program with the correlation energy left blank, in five graded exercises. |

Both are `EXCLUDE_FROM_ALL` in `CMakeLists.txt`, deliberately: they are teaching
material rather than part of the shipped product, an ordinary `make` does not build
them, and no test references either. `run_mp2` was already registered that way
before this port; `run_mp2_exercise` was added alongside it.

## Validation

The point of a teaching program is that the answer it produces is right, so this was
checked rather than assumed. Tonto computes MP2 itself
(`MOLECULE.MISC:make_r_mp2`, keyword `mp2`), which makes an internal reference
available and means no literature value is needed.

Water at the RHF/STO-3G geometry of
`tests/short/h2o_rhf_cc-pVDZ_electron_density_plot`, SCF converged to 1e-7, printed
with `real_precision= 12`. Both numbers come from a single run of `run_mp2`, whose
job file also carries the `mp2` keyword, so the orbitals are by construction
identical:

| | E2 | E(total) |
|---|---|---|
| library `mp2` keyword | −0.038839812482 | −75.004633512884 |
| `run_mp2`, frozen core | −0.038839812482 | −75.004633512884 |
| `run_mp2`, all electron | −0.038936921469 | −75.004730621870 |

**Exact to every printed digit.** Reproduce it with:

```bash
make run_mp2
mkdir -p /tmp/mp2 && cd /tmp/mp2      # the job file must be named "stdin"
# ... write the job, including "scf" and "mp2" ...
TONTO_BASIS_SET_DIRECTORY=<repo>/basis_sets <repo>/build/run_mp2
```

### Why there are two columns

The library restricts the active space, and says so: `make_r_mp2` prints *"The active
space is restricted to minic the gaussian program"* and calls
`MOLECULE.MISC:set_MO_limits`, which picks the first active occupied orbital and the
last active virtual from hard-coded energy windows (`Emin = -5.0`, `Emax = 100.0`,
`Edel = 0.1`). For water/STO-3G that gives `fa = 2`, `lr = 7` — the oxygen 1s is
frozen, every virtual is kept.

`run_mp2` therefore accumulates two sums in the one loop: `e2` over all occupied
orbitals, and `e2_fc` skipping the first. The frozen-core column is the one that
matches the library, and the all-electron column is slightly lower because
correlating the core recovers a little more. Both are printed, which makes the
comparison against the library keyword a one-line check rather than an exercise in
matching conventions.

This is also exercise 5 in the worksheet: run `mp2`, notice the two do not agree,
work out why.

## Three defects fixed on the way

`runfiles/run_mp2.foo` was already on `develop` — it had been carried along by the
pointer removal and the `stderr` → `std_err` rename without anyone running it. It did
not work, in three independent ways.

1. **It could not read a basis set.** It called `m.MAIN:run` with no argument, where
   `run_molecule.foo` passes `basis_library_dir`. Since the program has no
   command-line option handling, the `TONTO_BASIS_SET_DIRECTORY` environment variable
   was the only possible source and was never consulted. The failure was
   badly misleading: the basis is resolved as the `atoms=` block closes, so the error
   named the closing brace —

   ```
   File name   = stdin
   Line number =   14
   File buffer =    }
   Cursor ----------^
   ```

   — on a job that plain `tonto` accepts without complaint. It now reads the
   environment variable.

2. **The correlation formula was wrong.** It read

   ```
   e2 = e2 + (TWO*v(i,a,j,b)**2 - (v(i,a,j,b)-v(i,b,j,a))**2)/(...)
   ```

   Writing v₁ = (ia|jb) and v₂ = (ib|ja), that numerator expands to
   v₁² + 2v₁v₂ − v₂², where the correct one (Szabo & Ostlund eq. 6.74) is
   v₁(2v₁ − v₂) = 2v₁² − v₁v₂. A trailing `e2 = HALF*e2` then halved the result,
   which does not belong with this numerator either.

3. **The loops silently froze the core.** They ran `do i = 2,no` and `do j = 2,no`
   with no comment saying why. That happens to be what the library does, so it was
   probably deliberate — but nothing recorded it, and combined with defect 2 the
   agreement it was presumably aiming for could not have been observed.

## What was not taken from the tag

- **The tag's `CMakeLists.txt`.** It is a lab-specific fork — 116 changed lines
  reshaping the build around the teaching exercise. `develop`'s build already
  registers `run_mp2` correctly; only the new `run_mp2_exercise` target was added.
- **`FortranBasics.pdf` (2.1 MB) and `How_to_use_a_command_line.pdf` (250 KB).**
  Student handouts, and the reason the tag's diff is mostly binary. They stay on the
  tag, on the same principle as the pHAR wavefunction: `develop` and `master` should
  not carry large binaries that every clone must pull. Retrieve them with
  `git show archive/Teaching:FortranBasics.pdf > FortranBasics.pdf`.
- **`run_exercise.foo`.** Thirty-two lines of which twenty-six are the licence
  header; the body is `TONTO_CREATE` followed by `TONTO_DESTROY`. It is a blank
  program skeleton with no teaching content, and `run_mp2_exercise.foo` subsumes it.

## A note on the naming

The tag's own `run_mp2.foo` was the **worksheet** — its correlation loop is commented
out for the student to fill in — while `develop`'s file of the same name was the
attempted solution. Porting the tag's file over `develop`'s would therefore have
replaced a working program with a blank one. So the tag's worksheet became
`run_mp2_exercise.foo` and the working program keeps the plain name, which is also
the naming Dylan proposed for `run_exercise.foo`.

**Keep the two files in step.** The exercise is the working program with four blocks
blanked out; if the physics in `run_mp2.foo` changes, the worksheet's exercise 3 needs
the same change.

## Neither program takes command-line options

`run_mp2` and `run_mp2_exercise` read a file named `stdin` in the current directory
and write one named `stdout`. They do **not** accept `--input` / `--output`, unlike
every shipped program (see `CLAUDE.md` §1). Passing `--input foo` is silently ignored,
which is confusing enough to be worth stating in the header of both files. Adding real
option handling would mean duplicating the hundred-odd lines `run_molecule.foo` spends
on it; for a teaching program run from a scratch directory it is not obviously worth
it, but it is the first thing to fix if these are used in anger.
