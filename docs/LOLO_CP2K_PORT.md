# The `Lolo_CP2K` port — what we did, and what it means for your tests

**For Lorraine Malaspina.** Written 2026-08-23, when the work was done.

This document says what happened to your `Lolo_CP2K` work, exactly which of
your keywords and routines we had to change and why, what we did not bring
over, and — the part that needs you — how to add tests for it.

There was a plan before the work started. It has been deleted: it was
superseded by what actually happened, and in one important respect it was
wrong — it treated `912b32b2` as pure syntax churn, when that commit also
carries the whole `oc-observed` model. A plan nobody should follow again is
better removed than left lying about. This document replaces it.

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
something, section 3 says what and why. There is exactly one place where we
overruled a decision of yours rather than asking first — the forced
`accuracy= extreme` grid — and section 4.1 gives the reasoning in full, along
with the one line of input that gets the same grid deliberately. Section 4.3
records a defect we found in shared code and deliberately did **not** fix,
because fixing it moves test results and that is your call, not ours.

**Everything on your branch is now across**, `oc-observed` included, and it
builds and runs. What we could not do is test the *periodic* half of it:
nothing in Tonto's test suite reads a CRYSTAL23 or CP2K wavefunction, or
exercises `oc-observed`. Your reflection-merging work is a different story —
it changed ten tests immediately, and section 6.1 has the numbers. Section 6
explains what is still needed and how to make it.

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
| `a62eb998` | One-atom fragment needs a Hirshfeld denominator | ported |
| `e2a401ef` | SHELXL merging; prune on the model, not on the IAM | ported |

Two of your commits were not replayed as commits, and they are the two big ones
that convert the whole tree between old and new Foo syntax: `6f7fa8cf` and
`912b32b2`. The syntax half of them is not science, and it is the reason none of
this could be cherry-picked. But `912b32b2` is not *only* syntax, which is the
trap described next.

### `912b32b2` was not only syntax, and that nearly cost you a whole model

Our plan wrote that commit off as dialect churn. It is that — but it also
carries the **entire `oc-observed` partition model**, the regularised
experimental-density work, which exists nowhere else in your history. Had we
followed the plan, that model would have been silently dropped, and the two
later commits that refine it would have looked like they applied to nothing.

**It is ported.** `oc-observed` builds atomic form factors from the data
rather than from a wavefunction — a neutral IAM prior plus a regularized,
model-phased experimental residual, Hirshfeld-partitioned onto each atom and
deconvolved of harmonic thermal motion, with the residual damped both by
`observed_density_shrinkage` and by F²/(F²+σ²). With it came `a62eb998`, which
until then had nothing to apply to.

How we know nothing else was missed: we compared every procedure, type member
and input keyword across three points — your branch tip, your fork point
`8ee220bc`, and here. The three-way comparison is what separates what you
**added** from what Tonto has since **renamed or removed**, which a plain
two-way diff cannot do. That list is now empty.

Our reading of how the method works, and four things about it worth knowing, are
in section 5. Please correct us there — it is your method, and we reconstructed
it from the code.

One more thing worth saying here, because it shapes section 6: **your branch
carries no test jobs.** Measured against your fork point, the only file you add
under `tests/` is the Fourier-kernel benchmark of section 3.4. So the CRYSTAL23
and CP2K files we ask for at the end really do have to come from you
separately — they are not sitting on the branch waiting to be ported, and we
looked.

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

### 3.4 Your Fourier-kernel benchmark

`tests/benchmark_c23_fourier_kernel.f90`, which came with `0673ad05`, is not in
this port, and that is a decision rather than an oversight.

It is a good program and we are not throwing it away — it stays on your branch
and this note says where to find it. But it is a standalone Fortran
reimplementation of both kernels, so what it checks is that
exp(i·kr) equals cos(kr) + i·sin(kr), which was never in doubt. It does not
touch `molecule.rho.foo`, so it could not catch the one thing that might
genuinely have gone wrong in porting `0673ad05`: somebody mistyping your change
into Tonto itself. Its other half is a timing comparison, which is
machine-dependent and would be a flaky test.

If you would like it kept in the tree as a development tool — built on request,
not run by `ctest` — say so and we will add it that way.

### 3.5 Two routines renamed to match their twins

`get_observed_Hirshfeld_atom_FFs` is now **`make_observed_Hirshfeld_atom_FFs`**,
and lives in `molecule.rho.foo` beside `make_C23_Hirshfeld_atom_FFs` rather than
in `molecule.scf.foo`. That is not our invention: Tonto had already renamed and
moved the CRYSTAL23 twin the same way, and leaving one of a matched pair behind
would be worse than moving both.

Two more renames you will meet if you read the ported code:
`get_unique_IAM_atom_SFs` is now `make_unique_IAM_atom_FFs`, and
`BECKE_GRID:make_grid(pt,wt,a)` is now `make_atom_grid(pts,wts,a)`. Both are
Tonto's renames, not ours.

### 3.6 Your keywords: all of them survive, unchanged

Every keyword you added is spelled exactly as you wrote it, and behaves as you
wrote it.

| Keyword | Where | Values | Default |
|---|---|---|---|
| `merg=`, `merg_code=` | inside `xray_data= { }` | 0 to 4 | 2 |
| `stockholder_model=` | inside `xray_data= { }` | `cluster`, `periodic` | `cluster` |
| `cp2k_periodic_file_name=` | top level | a file name | — |
| `process_cif_and_cp2k_data` | top level | — | — |
| `observed_density_shrinkage=` | inside `xray_data= { }` | ≥ 0 and < 1 | 0.5 |
| `observed_density_min_TF=` | inside `xray_data= { }` | > 0 and ≤ 1 | 0.1 |
| `observed_zero_phase_sign=` | inside `xray_data= { }` | −1, 0 or +1 | 0 |

`partition_model=` accepts `observed` and `oc-crystal23`/`crystal23` as you
wrote them, and `observed` still normalises to `oc-observed`.

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

## 4. One change we did not keep, and three to look at

### 4.1 The forced `accuracy= extreme` grid — REMOVED

This is the only place where we overruled you rather than asking, so it gets
the full reasoning.

`9ccdacf1` promoted the integration grid for an imported periodic density to
`accuracy= extreme`, unless the user had already asked for `extreme` or `best`:

```
if (NOT .becke_grid.accuracy.is_one_of(["extreme","best   "])) then
   .becke_grid.set_accuracy("extreme")
end
```

**Dylan asked for this to be taken out, and we agree.** It is commented out in
`MOLECULE.RHO:make_C23_Hirshfeld_atom_FFs`, with a note pointing here.

Why: it silently overrides the user. Somebody who writes
`becke_grid= { accuracy= high }` gets `extreme`, is not told, and cannot find
out from the output why the job is slow or why the number moved. Tonto has just
spent a month removing a defect of exactly that shape — `initialize_DFT_grids`
destroyed and recreated the `BECKE_GRID`, so **every** DFT run silently used the
type defaults while the echo went on reporting what the user had asked for. That
one cost years of difficulty reproducing DFT energies, and it is written up in
`docs/DFT_STANDARDISATION.md`. Putting a second override of the same kind into
the periodic path, three weeks after removing the first, is not a trade we are
willing to make — however good the reason for wanting the denser grid.

**Your underlying point stands, and nothing is lost.** An imported periodic
density really does want a denser grid than the molecular default, especially
for core-sensitive data. The fix is to ask for it in the job file, where it is
visible, echoed, and can be changed:

```
   becke_grid= { accuracy= extreme }
```

Put that at the top level of the job, alongside `basis_name=`, before the
`crystal= { }` block. `set_defaults` first is a good habit if you want to be
sure of what you are starting from:

```
   becke_grid= {
      set_defaults
      accuracy= extreme
      put_basics          ! echoes the grid actually in use
   }
```

The accuracies, cheapest first, are `very_low`, `sg-1`, `low`, `medium`,
`high`, `very_high`, `extreme`, `best`. `extreme` is what your code was
forcing, so writing it explicitly reproduces your behaviour exactly.

**What we kept from the same hunk.** You also changed `.becke_grid.create` to
`if (.becke_grid.deallocated) .becke_grid.create`. That part is right and it
stays — without it, an unconditional `create` wipes a `becke_grid=` block the
user has just written, which is the `initialize_DFT_grids` bug all over again.
So your change is what makes the input block above work at all.

**One consequence for testing.** Because the grid is no longer forced, the
existing job `tests/crystal23/ammonium_borane_pHAR_C23` runs on the default
grid. If a periodic refinement needs a denser one to be trustworthy, that
should be *demonstrated* — the same job at two accuracies, giving two
different answers — rather than compiled in. That would be a good test, and it
is cheap.

### 4.2 `reflection0` versus `unmerged_reflections`

This one we did keep, and would just like you to confirm. You introduced
`unmerged_reflections` as the untouched master copy, and redefined
`reflection0` as "the complete merged set, kept for reporting". Just before
your work landed we had made a separate fix so that `reflection0` was taken
only once and could not be overwritten by a second `xray_data=` block. Your
design supersedes that: the pristine data is now in `unmerged_reflections`,
which is the right place for it, so we removed our guard and followed you.
We think that is correct, but you are the one who knows what `reflection0` is
for now.

### 4.3 An unguarded phase in the residual map — recorded, not fixed

Found while porting `oc-observed`, and left for you.

`DIFFRACTION_DATA.SET:make_symop_generated_dF_a_v2` computes the phase of each
residual coefficient as `F_calc/abs(F_calc)`, with no guard. So:

- a reflection whose `F_calc` is **exactly zero** gives 0/0, that is a NaN;
- one whose `F_calc` is merely **tiny** gives a phase that is numerical noise,
  which is then attached to a full-sized `F_exp`.

Both poison the residual density map without saying anything. Your phased
routine guards exactly this, with `abs(...) > .F_calc_cutoff`, and contributes
zero instead — which we think is right.

**We did not apply that guard to the shared routine, on purpose.** Its only two
callers are `make_residual_density_grid` and `make_residual_density_cell_n`,
and **nine test references print residual output**. Guarding it would move
those numbers, and deciding whether the new numbers are the correct ones is a
scientific judgement about your own data, not a porting decision. So:

**As it stands, the defect is still there, and fixing it will change the test
suite.** That is the honest position. Nothing is hidden: there is a note beside
the code saying so, and this section.

One measurement, so the size of it is known rather than guessed: no NaN from
this routine appears in any current reference. The three `NaN` strings in the
suite are all `Rw(F2)`, an unrelated quantity computed when there is no
intensity data. So exact zeros do not arise in what we test today, and the
realistic exposure is the tiny-`F_calc` case.

One trap if you do fix it. The guard tests against `F_calc_cutoff`, whose
default your own `e2a401ef` changed from `TOL(3)` to `TOL(8)` — so the two
commits couple through one number. And `F_calc_cutoff` is documented as
"negative means unused"; a negative value makes the guard always true and
restores the division by zero. A guard on `abs(F_calc) > ZERO` as well would
close that.
### 4.4 Systematic absences are now pruned even at `merg_code= 0` — OPEN

This one is not a defect either, but it is the change with the widest reach in
the whole port, and it is the only thing still failing in the test suite. It is
left that way on purpose, so it is visible rather than blessed away.

`apply_merg` calls `prune_systematic_absences` **before** it looks at
`merg_code`, so true space-group absences are removed even when the user asked
for no merging at all. Your comment explains why, and we agree with the
principle: SHELXL rejects true absences independently of any merging or
observation threshold, and a reflection that is zero by symmetry carries no
information.

**What it does to real jobs.** Two short tests generate a reflection list to an
`stl_limit` in space group **P b c a**, whose three glide planes require k even
in 0kl, l even in h0l, and h even in hk0:

```
Space-group systematic absences pruned
No. of observations before pruning ..... 1604
No. of systematic absences .............  247
No. of observations kept ............... 1357
```

247 of 1604, about 15%, which is the right order for Pbca. We checked the
count against the reflection conditions rather than assuming it.

**The part we would like you to look at.** Both those jobs set
`use_equivalents= YES`, which now maps to `merg_code= 0`, and the run duly
prints *"MERG code (observations retained unmerged) = 0"* — and then removes
247 reflections anyway. Someone who writes `use_equivalents= YES` is asking for
their list to be left alone, and it is not.

We think the pruning is right and the *reporting* is what misleads. Two cheap
options, and it is your call which:

- say so in the `merg_code=` documentation — that absences are always removed,
  whatever the code, because they are not observations;
- or separate the two ideas properly, so absence rejection has its own switch
  and `merg_code` governs only merging.

Until it is decided, the two tests stay red. Everything else in the short suite
passes: 60 of 62.
---

## 5. The `oc-observed` method, as we read it

We had to understand this to port it, and understanding is where mistakes hide.
So here is our reading, for you to correct. **Nothing below is a defect
report** — three of the four are properties of the method rather than of your
code, and the fourth is a question nobody has measured.

### 5.1 What we think it does

Aspherical atomic form factors, obtained from the data instead of from a
wavefunction:

1. A neutral IAM prior supplies the phases, the electron count, and the Fourier
   coefficients of the reflections nobody measured.
2. The residual ΔF = F_exp·(model phase) − F_calc is the part of the data the
   spherical model fails to explain.
3. It is damped twice — by `observed_density_shrinkage`, and by
   F²/(F²+σ²) so a noisy weak reflection counts for less.
4. Fourier-transformed to the Becke grid, then Hirshfeld-partitioned onto atoms.
5. Divided by the atom's temperature factor exp(−½h·U·h), floored at
   `observed_density_min_TF`, to turn a *dynamic* residual into a *static*
   form factor.
6. Added to the IAM prior.

We checked that the loop really closes: `MOLECULE.HAR:LS_fit_HAs_memory` rebuilds
the form factors every refinement cycle, and the temperature factor comes from
`.atom(c).put_ADP2_vector_to(adp2)` — the ADPs as currently refined. So the
deconvolution does follow the refinement, as your comment says.

The phase handling is neat and we nearly missed it: `phase_seed` is taken from
`.reflections(i).F_calc` *before* the IAM prior overwrites it, so it holds the
**previous cycle's aspherical** phases. The phased routine prefers those and
falls back to the IAM phase only where the previous model had none.

### 5.2 The ADPs lag by one cycle

The least squares refines ADPs against form factors that were deconvolved using
the *previous* cycle's ADPs. At a true fixed point that lag vanishes. But
convergence is tested on parameter shifts (`tol_for_shift_on_esd`), not on the
form factors — so the refinement can stop while the deconvolution is still one
step behind.

Whether that matters depends on how far the form factors still move at the point
the refinement stops, and nobody has measured it. It would be easy to: print the
largest change in `ff` between the last two cycles.

### 5.3 The shrinkage factor is doing two jobs, and only one is written down

Your `ENSURE` justifies shrinkage < 1 by circularity, and that is right: at 1
the whole residual returns, F_calc equals F_exp by construction, and the
refinement is an identity rather than a fit.

But it is also the **damping that makes this fixed-point iteration a
contraction**. Undamped, there is no reason the loop should converge at all.
That is a second and independent argument for shrinkage < 1, and we could not
find it stated anywhere. If it is deliberate, it deserves a line in the
`docu_` text, because it tells a user what happens as they push shrinkage
towards one: not merely "more circular", but "less likely to converge".

### 5.4 `observed_density_min_TF` biases, it does not only limit

Dividing by exp(−½h·U·h) amplifies noise at high angle, which is why the floor
exists — at the default 0.1 the amplification is capped at tenfold.

The thing to be aware of is that below the floor the deconvolution is no longer
merely *noisy*, it is *wrong*: the recovered static form factor is out by a
factor T/`min_TF`, and there is a kink at the reflection where the exponent
passes about 2.3. It is a sensible engineering choice, but it is a systematic
distortion of the high-angle data rather than noise suppression, and the size of
it depends on the ADPs — so it varies from structure to structure and from atom
to atom.

### 5.5 One approximation inherent to the method

The residual density near atom *a* contains thermal smearing contributed by its
*neighbours*, whose ADPs are different. Hirshfeld partitioning hands atom *a* a
share of that, which is then divided by atom *a*'s temperature factor.

This is standard for experimental-form-factor work and we are not suggesting an
alternative. We mention it only so that it is written down somewhere, because it
sets a floor on what the method can be expected to deliver for a structure with
strongly contrasting ADPs — a heavy atom beside a hydrogen, say.

---

## 6. Testing — the part that needs you

### 6.1 Where things stand

**Your merging work is tested. The periodic half is not.**

`e2a401ef` reached the suite at once. Ten short tests changed the moment it
landed, and two of them show your headline claim working: `nh3_IAM_ITC` and
`nh3_IAM_gaussian` both go from 88 to 86 reflections — exactly as before — but
the pair is now *averaged* rather than one of them discarded, moving GoF from
1.360 to 1.387 and from 1.268 to 1.288. Eight references were regenerated. Two
are deliberately left failing; that is section 4.4.

Nothing, however, exercises the CRYSTAL23 import, the CP2K import, the periodic
stockholder model, or `oc-observed`. There is exactly one job in the tree that
reads a CRYSTAL23 wavefunction, and it does not run unless somebody
deliberately fetches a 167 MB file first. **`oc-observed` has never been
executed at all** — roughly 300 lines of new numerical code that compiles and
that nothing has ever run.

That matters more here than it would in most codebases. Nearly every defect
found in Tonto this year was **silent** — it produced a wrong number, or no
number, with no error message, and none of them was found by reading the code.
Your own `9ccdacf1` is exactly this kind: Tonto was pairing a 46×46 density
matrix from CRYSTAL with a 36-function basis and reconstructing a density that
could not possibly be right, and said nothing. The check you added turns that
into an error message. **But nothing yet proves the check fires**, and a check
nobody has seen fire is not much better than no check.

So the single most useful thing you can give us is a small CRYSTAL23 file where
the old code picked the wrong basis, and a small CP2K file. Section 6.4 says how
to make them.

### 6.2 Where the tests live now

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

### 6.3 How the existing job works

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

The `IO` file is the test manifest. It recognises six keys:

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

### 6.4 How the big XML is made

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

### 6.5 Why the big file is not committed, and what to do instead

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

### 6.6 What we would like, concretely

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

## 7. What still has to happen at our end

Being honest about the state of it:

- **It builds.** A release `tonto` and `hart` compile clean from the ported
  sources, with no new warnings, and both source lints pass. Two release-only
  compile errors had to be fixed first, and one of them was in your code: a
  `WARN` reached by a line continuation, `if (cond) & WARN(...)`. `WARN`
  expands to a *comment* in a release build, so that form leaves a bare
  `if (cond)` and will not compile — in release only, never in debug. It is now
  `WARN_IF`, which takes the condition as an argument and exists for this.
  Worth knowing for your own branch, where the same line is still present.
- **The long suite has not run**, nor MPI, nor a debug build. The long suite is
  where every HAR, fragHAR, extinction and quartz test lives, so it is the
  obvious next thing and it may well move more references.
- **`e2a401ef` will change existing results.** It changes how reflections are
  merged and pruned for every refinement, not only periodic ones. Any test
  whose reflection count moves is evidence that the change is working, not a
  failure — but each one has to be looked at rather than blessed.
- **The short suite is at 60/62.** Eight references were regenerated: six that
  differed only by the new MERG banner lines, and the two IAM jobs where
  equivalents are now genuinely averaged instead of discarded — which moved
  `nh3_IAM_ITC` from GoF 1.360 to 1.387 and `nh3_IAM_gaussian` from 1.268 to
  1.288, on the same 88 → 86 reflections. That is your fix, visible. The two
  still red are the Pbca pair of section 4.4, deliberately not blessed.
- **The existing CRYSTAL23 test will change too**, because pruning now happens
  on the model rather than on the IAM. Its reference output will need
  regenerating. It will *not* slow down, because the forced `extreme` grid was
  removed (section 4.1); had it stayed, the three-minute runtime would have
  grown considerably.

None of that needs anything from you. The test files in section 6.6 do.

---

## 8. Where to look

| For | Read |
|---|---|
| Which branch is what, and why | `docs/REPOSITORY_BRANCHES.md` |
| Building Tonto on Linux | `docs/BUILDING_ON_LINUX.md` |
| The Foo language | `docs/FOO_GRAMMAR_DOCUMENTATION.md` |
| Traps when writing or debugging Foo | `docs/TONTO_DEVELOPER.md` |
| Why user settings must not be overridden | `docs/DFT_STANDARDISATION.md` |
