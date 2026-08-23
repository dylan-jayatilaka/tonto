# The `Lolo_CP2K` port — what we did, and what it means for your tests

**For Lorraine Malaspina.** Written 2026-08-23, when the work was done.

This document says what happened to your `Lolo_CP2K` work, exactly which of
your keywords and routines we had to change and why, what we did not bring
over, and — the part that needs you — how to add tests for it.

The plan we worked from is `docs/LOLO_CP2K_PORT_PLAN.md`. This document is the
record of what actually happened, which is not quite the same thing.

---

## 1. The short version

Your branch `Lolo_CP2K` is untouched. Nothing was pushed to it, merged into it,
rebased onto it, or deleted. It stays exactly as you left it, for as long as you
want it.

We copied your work by hand onto a new branch called `Lolo2`, which will be
merged into `develop`. We did the copying rather than asking you to, because
Tonto has moved 622 commits since you branched, including two changes to the
language itself. Asking you to port would have meant asking you to learn a
codebase that had changed underneath you.

Almost all of your code went across unchanged in meaning. Where we had to change
something, section 3 says what and why.

**The one thing we could not do is test it.** Nothing in Tonto's test suite
runs any of this code. Section 5 explains what is needed and how to make it.

---

## 2. What went across

Ten of your commits were in scope. Here is what happened to each.

| Your commit | What it does | Outcome |
|---|---|---|
| `a35ed64a` | Atomic-SCF defaults when the density is imported | ported |
| `9ccdacf1` | Keep the XML overlap matrix; check the basis matches | ported |
| `fe66ca5b` | Make that overlap read optional, loosen to `TOL(4)` | ported |
| `67c8e2dd` | Detect CP2K files; centre atoms, reverse lattice labels | ported |
| `a8ea1092` | `cp2k_periodic_file_name=` and `process_cif_and_cp2k_data` | **already there** |
| `b8f63c49` | Do not start a molecular SCF for an imported density | ported in part |
| `4661a8a3` | Selectable periodic stockholder model | ported in part |
| `0673ad05` | Faster Fourier kernel for the periodic form factors | ported |
| `a62eb998` | One-atom fragment needs a Hirshfeld denominator | **not ported** |
| `e2a401ef` | SHELXL merging; prune on the model, not on the IAM | ported |

Two of your commits were left out on purpose, and they are the two big ones that
convert the whole tree between old and new Foo syntax: `6f7fa8cf` and
`912b32b2`. They are not science, they are dialect, and they are the reason none
of this could be cherry-picked.

### The thing the plan got wrong, and you should know about

`912b32b2` is described in the plan as dialect churn. It is not only that. It
also carries the **entire `oc-observed` partition model** — the regularised
experimental-density work. That model does not exist anywhere else, so it is not
on `develop` and it is not in this port.

That has three consequences:

- `a62eb998` only ever applies to `oc-observed`, so there was nothing here for
  it to change. It is not ported. It is not lost — it is still on your branch,
  and it will apply the moment the observed-density model itself is brought over.
- `b8f63c49` and `4661a8a3` each had a part that referred to `oc-observed`.
  Those parts were left out; the CRYSTAL23 parts went across in full.
- Bringing `oc-observed` over is a separate piece of work that nobody has
  scheduled. If it matters to you, please say so — it changes the priority.

---

## 3. What we changed in your code, and why

Nothing here changes what your code computes. These are all either renames that
Tonto made while you were away, or one small correctness change.

### 3.1 The language changed twice while you were branched

- Procedure attributes are written after `::` now, not `:::`. Every one of your
  procedure headers was retyped.
- Calls no longer need the submodule name. Your `.SET:prune_reflections`,
  `.INQ:have_I_exp` and `.SCF:make_X_SFs` became plain `.prune_reflections`,
  `.have_I_exp` and `.make_X_SFs`. Tonto works out the submodule itself now.

### 3.2 Routines that had been renamed or moved

| You called | It is now called | Where it lives now |
|---|---|---|
| `make_HA_inputs` | `make_HA_info` | `molecule.scf.foo`, with a different argument list |
| `get_C23_Hirshfeld_atom_FFs` | `make_C23_Hirshfeld_atom_FFs` | moved to `molecule.rho.foo` |

Your changes to those two routines went into their new homes.

### 3.3 One correctness change we made deliberately

Your `set_merg_code` was declared `pure`, in lower case. It contains an
`ENSURE`, and `ENSURE` writes a message, so it is not pure. In an optimised
build `ENSURE` disappears and the routine compiles; in a debug or MPI build it
does not, and the compiler's message is misleading — it complains that there is
no specific subroutine for the generic `ensure_`, not that the routine is
impure. We changed it to upper-case `PURE`, which is a Tonto macro that becomes
`pure` only in builds where `ENSURE` is compiled away.

This is a real trap and it has cost us a red build before. In short: **a routine
containing `ENSURE`, `DIE` or `WARN` must be `PURE`, never `pure`.** Your
`set_stockholder_model` already had this right.

### 3.4 One routine we left out

`make_periodic_stockholder_atom_weight` — the short wrapper that passes
`.atom` to `make_periodic_stockholder_atom_weight_from` — exists only to serve
the observed-density path, which is not here. The real routine,
`make_periodic_stockholder_atom_weight_from`, is ported in full and is what the
CRYSTAL23 code calls. When the observed-density work arrives, the wrapper is
four lines.

### 3.5 Your keywords: all of them survive, unchanged

Every keyword you added is spelled exactly as you wrote it, and behaves as you
wrote it.

| Keyword | Where | Values | Default |
|---|---|---|---|
| `merg=`, `merg_code=` | inside `xray_data= { }` | 0 to 4 | 2 |
| `stockholder_model=` | inside `xray_data= { }` | `cluster`, `periodic` | `cluster` |
| `cp2k_periodic_file_name=` | top level | a file name | — |
| `process_cif_and_cp2k_data` | top level | — | — |

`use_equivalents=` still works and still parses, but it is now a compatibility
alias: `TRUE` means `merg_code= 0`, `FALSE` means `merg_code= 2`. Old input
files keep working. Its `?` documentation says so.

Two other things that will show up in output:

- `F_calc_cutoff` now defaults to `TOL(8)` rather than `TOL(3)`, because it has
  become a numerical-zero test applied after the aspherical model has run,
  rather than a way of removing systematic absences beforehand. That is your
  change and it is the point of `e2a401ef`.
- `prune_reflections` is now split into `prune_observation_reflections` and
  `prune_calculated_reflections`, with `prune_reflections` calling both. Again
  yours. Anything in Tonto that used to call `prune_reflections` after
  calculating structure factors now calls `prune_calculated_reflections`.

---

## 4. Two things we would like you to look at

Neither is a defect. Both are decisions of yours that we ported faithfully and
would rather you confirmed than have us quietly change.

**The Becke grid is forced to `extreme`.** In `9ccdacf1` the integration grid
for an imported periodic density is promoted to `accuracy= extreme` unless the
user asked for `extreme` or `best`. We understand why: a periodic density needs
a denser grid than the molecular default, especially for core-sensitive data.
But it does mean that a user who writes `accuracy= high` gets `extreme` without
being told. Tonto has just spent a month removing a bug of exactly that
shape — a `becke_grid` block whose settings were silently discarded, which was
the reason DFT energies were hard to reproduce (`docs/DFT_STANDARDISATION.md`).
We would suggest either saying so in the output, or making it a floor the user
can lower deliberately. Your call; tell us which and we will do it.

**`reflection0` versus `unmerged_reflections`.** You introduced
`unmerged_reflections` as the untouched master copy, and redefined
`reflection0` as "the complete merged set, kept for reporting". Just before
your work landed we had made a separate fix so that `reflection0` was taken
only once and could not be overwritten by a second `xray_data=` block. Your
design supersedes that: the pristine data is now in `unmerged_reflections`,
which is the right place for it, so we removed our guard and followed you.
We think that is correct, but you are the one who knows what `reflection0` is
for now.

---

## 5. Testing — the part that needs you

### 5.1 Where things stand

**No test in Tonto exercises any of this code.** Not the CRYSTAL23 import, not
the CP2K import, not the periodic stockholder model. There is exactly one job
in the tree that reads a CRYSTAL23 wavefunction, and it does not run unless
somebody deliberately fetches a 167 MB file first.

That matters more here than it would in most codebases. Nearly every defect
found in Tonto this year was **silent** — it produced a wrong number, or no
number, with no error message, and none of them was found by reading the code.
Your own `9ccdacf1` is exactly this kind: Tonto was pairing a 46×46 density
matrix from CRYSTAL with a 36-function basis and reconstructing a density that
could not possibly be right, and said nothing. The check you added turns that
into an error message. **But nothing yet proves the check fires**, and a check
nobody has seen fire is not much better than no check.

So the single most useful thing you can give us is a small CRYSTAL23 file where
the old code picked the wrong basis, and a small CP2K file. Section 5.4 says how
to make them.

### 5.2 Where the tests live now

They live in **`tests/crystal23/`**. We made that directory as part of this
work — previously the one job sat in `tests/long/` among the ordinary molecular
tests, which was not a helpful place for it.

```
tests/crystal23/
   CMakeLists.txt
   ammonium_borane_pHAR_C23/
      stdin                     the Tonto job
      IO                        the manifest: what goes in, what is compared
      stdout                    the blessed reference output
      B6H6_grown.cif            the structure
      tonto_data_on_F_20rfl.hkl the reflections
      Crystal23_InputFiles.zip  how the wavefunction was made (12 KB)
      GenerateXML.XML           the wavefunction -- 167 MB, NOT committed
```

Jobs in this directory carry two labels, `crystal23` and `long`, so:

```bash
ctest -L crystal23     # just these
ctest -L long          # the long suite, which still includes them
```

Put your new jobs in `tests/crystal23/`. Anything named `*.XML` or `*.xml` in a
subdirectory there is ignored by git automatically, so you cannot accidentally
commit a huge wavefunction.

### 5.3 How the existing job works

The Tonto job itself, `stdin`, is ordinary except for three lines:

```
   basis_name= pob-TZVP-rev2       ! MUST be the basis CRYSTAL used
   c23_XML_file_name= GenerateXML.XML
   process_cif_and_c23_xml
```

and then, inside `crystal= { xray_data= { ... } }`:

```
   partition_model= oc-crystal23
```

The basis name is the part that bites. The XML contains a density matrix but no
basis exponents and no contraction coefficients, so Tonto has to be told which
basis to rebuild the density with, and it must be **exactly** the one CRYSTAL
used. For this job that is `pob-TZVP-rev2`: 22 atoms in the primitive cell
(2 N, 6 B, 14 H), giving 2×18 + 6×18 + 14×6 = 228 spherical basis functions,
which is what the XML declares. Your `9ccdacf1` check compares those two numbers
and stops if they differ.

The `IO` file is the test manifest. It has five possible keys:

| Key | Meaning |
|---|---|
| `input:` | a file to copy into the run directory (repeat as needed) |
| `output:` | a file to compare against the reference (repeat as needed) |
| `delete:` | recorded, currently unused |
| `program:` | run this instead of `tonto`, e.g. `hart` |
| `args:` | the command line for that program |
| `skip-hint:` | printed when the test is skipped, saying how to fix it |

A job with no `program:` line is a plain Tonto job, so `stdin` and `stdout` are
added for you. Comments start with `!`. A mistyped key is an error, not a
silent omission — that is deliberate, because a dropped `output:` line would
make the test pass while comparing nothing.

If a declared `input:` file is missing, the test does not fail: it **skips**,
prints the `skip-hint:` and exits 77, and ctest reports "Skipped" rather than
reddening the suite. That is how a test can depend on a file too big to commit.

### 5.4 How the big XML is made

The recipe is committed, in `Crystal23_InputFiles.zip` (12 KB). It contains
three files, and they are worth understanding because **your tests will be made
the same way**.

**Step one — the periodic SCF.** `Ammonium_closo-hexaborane(6)_pHAR.d12` is an
ordinary CRYSTAL23 input deck:

```
Ammonium_closo-hexaborane(6)_pHAR
CRYSTAL
0 0 0
225                                  <- space group F m -3 m
9.0998                               <- cell edge, Angstrom
       4                             <- 4 asymmetric unit atoms
       7        0.250000  0.250000  0.250000     N
       5        0.365199  0.500000  0.500000     B
       1        0.245953  0.500000  0.500000     H
       1        0.302645  0.197355  0.197355     H
KEEPSYMM
NOSHIFT
BASISSET
pob-TZVP-rev2                        <- the basis Tonto must be told about
DFT
b3lyp
END
SHRINK
6 6
TOLDEE
7
END
```

Running this produces the converged wavefunction, saved as a `.f9` file. The
job output, `Ammonium_closo-hexaborane(6)_pHAR.out`, is in the zip too, so you
can see what it should look like.

**Step two — dump the matrices.** `GenerateXML.d3` is a CRYSTAL *properties*
deck, and it contains exactly one keyword:

```
CRYAPI_OUT
```

That is what writes the XML: the cell vectors, the atoms in Cartesian
coordinates, the number of basis functions, the list of neighbouring cells, and
then the density and overlap matrix for each of those cells. That last part is
why the file is 167 MB for a 22-atom cell.

The properties step reads the `.f9` from step one and is what gives the file its
name, `GenerateXML.XML`.

> **One thing we could not verify.** We do not have CRYSTAL23 on these machines,
> so we could not run either step. The exact runner commands — we believe
> `runcry23` for the `.d12` and `runprop23` for the `.d3`, and that the `.XML`
> comes out named after the `.d3` — are read off the file names and the job
> output, not tested. If you correct us, we will fix this section.

**What Tonto reads out of the XML.** If you are producing one from anything
other than CRYSTAL, these are the tags that have to be there:

```
CELL_VECTOR_A / _B / _C          cell vectors, bohr
NUMBER_OF_ATOMS
CARTESIAN_COORDINATES, ATOM.n    atomic symbol, number, position
NUMBER_OF_ATOMIC_ORBITALS        basis functions per primitive cell
INTEGER_VECTORS_INFO, IVDL.n     the neighbouring cells
DIRECT_DENSITY_MATRIX__IVDL.n    density matrix for each of them
DIRECT_OVERLAP_MATRIX__IVDL.1    central-cell overlap -- OPTIONAL
```

The overlap block is the one you made optional in `fe66ca5b`. When it is
present, Tonto builds the same overlap matrix from its own basis and compares
the two; a disagreement above `TOL(4)` stops the run with a message telling the
user to fix `basis_name`. When it is absent — which is the CP2K case — only the
basis-function count is checked.

A CP2K file is recognised by the tag `CP2K_TONTO_PERIODIC_DENSITY` appearing
anywhere in it. When Tonto sees that tag it does two extra things, both from
your `67c8e2dd`: it moves the atoms into the centred cell that the periodic
matrices assume, and it reverses the sign of each lattice label, because CP2K's
P(R) is CRYSTAL's P(−R).

### 5.5 Why the big file is not committed, and what to do instead

`GenerateXML.XML` is 174,978,609 bytes. It is deliberately not in the
repository, and it should stay that way. Committing it — or committing a Git
LFS pointer together with a `.gitattributes` that tracks it — makes **every**
clone of Tonto pull 167 MB, because LFS fetches the checked-out revision
automatically for anyone who has git-lfs installed. GitHub's free allowance is
1 GB of storage and 1 GB per month of transfer, so a handful of clones would
exhaust it.

So the file lives on an archive tag, `archive/release-pHAR-broken`, and a script
fetches it on request:

```bash
scripts/fetch_phar_asset.sh
```

It fetches the file, checks its SHA256 against the recorded value, and deletes
it if it does not match — a truncated file is worse than no file, because the
test would then run and produce numbers nobody could trust. Without the file the
test skips and prints that command.

**For your tests, please do the same.** Commit the job, the `IO`, the CIF, the
reflections, the reference output, and the small zip of CRYSTAL input decks.
Do not commit the wavefunction. If you tell us where a wavefunction is, we will
add a fetch step for it. And please keep committing the input decks — they are
12 KB and they are the only reason the 167 MB file could ever be regenerated.

### 5.6 What we would like, concretely

1. **A small CRYSTAL23 XML, ideally one where the basis really is wrong** —
   a case where the old code silently used the wrong basis. That is the test
   that proves `9ccdacf1` works, rather than merely that it compiles.
2. **A small CP2K file**, to cover the lattice convention in `67c8e2dd`.
3. **A job using `stockholder_model= periodic`**, so the two settings can be
   compared against each other. It does not need to be a converged refinement;
   two different answers from two settings is already a real test.
4. **Any data with symmetry equivalents in it.** `e2a401ef` is the change with
   the widest reach and the easiest to test: before it, Tonto kept the first
   equivalent and silently discarded the rest. A file with equivalents in it
   demonstrates that directly.

Small is genuinely better than realistic. A test that takes four seconds gets
run; the one we have takes three minutes and forty seconds and is the slowest in
the suite.

---

## 6. What still has to happen at our end

Being honest about the state of it:

- **The ported code has not been compiled.** Every changed file was put through
  the Foo-to-Fortran translator and parses, and both source lints pass, but no
  binary has been built from it yet and no test has been run.
- **`e2a401ef` will change existing results.** It changes how reflections are
  merged and pruned for every refinement, not only periodic ones. Any test
  whose reflection count moves is evidence that the change is working, not a
  failure — but each one has to be looked at rather than blessed.
- **The existing CRYSTAL23 test will change too**, in two ways: the grid is
  promoted to `extreme` (section 4), and pruning now happens on the model
  rather than the IAM. Its reference output will need regenerating and its
  three-minute runtime will grow.

None of that needs anything from you. The test files in section 5.6 do.

---

## 7. Where to look

| For | Read |
|---|---|
| The plan this work followed | `docs/LOLO_CP2K_PORT_PLAN.md` |
| Which branch is what, and why | `docs/REPOSITORY_BRANCHES.md` |
| Building Tonto on Linux | `docs/BUILDING_ON_LINUX.md` |
| The Foo language | `docs/FOO_GRAMMAR_DOCUMENTATION.md` |
| Traps when writing or debugging Foo | `docs/TONTO_DEVELOPER.md` |
| Why user settings must not be overridden | `docs/DFT_STANDARDISATION.md` |
