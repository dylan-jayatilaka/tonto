# Running `rgbi` — Roby-Gould bond indices, and the pictures

`rgbi` is the third installed executable, alongside `tonto` and `hart`
(`runfiles/run_rgbi.foo`, `OUTPUT_NAME rgbi`). It computes Roby-Gould bond
indices from a Gaussian `.FChk` or a `.molden` file, and writes the LaTeX
fragments from which two pictures are drawn: a **labelled molecular structure**
and a page of **dial diagrams**.

The science is in the Grabowsky chapter, Jayatilaka (2025), and in the two
papers the program itself cites: Gould *et al.* (2008) *Theor. Chem. Acc.* **199**
119-270, and Alhameedi *et al.* (2018) *Int. J. Quantum Chem.* e25603.

*(The chapter PDF is deliberately **not** checked in — 1.1 MB of binary that a
weak link cannot push. On `sauce` it is at
`~/rgbi-reference/Jayatilaka_2025_Grabowsky_chapter.pdf`.)*

For installing the picture tools see **`docs/INSTALLING_RGBI.md`**; for checking
an installation, run `scripts/rgbi_doctor.sh`. This document is the developer
reference.

---

## 1. The pipeline

Measured on 2026-08-09, not inferred — a real job was run and each stage
watched (CLAUDE.md §2).

```
                    ┌─────────────────────────────┐
                    │  rgbi <file>                │   or a tonto job file
                    │  (or tonto + roby_analysis) │   with robydata= { … }
                    └──────────────┬──────────────┘
                                   │
                                   ▼
                    ┌─────────────────────────────┐
                    │  rgbi-*.tex  fragments       │
                    │  geometry.xyz                │
                    └──────┬───────────────┬──────┘
                           │               │
          ┌────────────────┘               └────────────────┐
          ▼                                                 ▼
┌───────────────────────┐                    ┌──────────────────────────┐
│  make-rgbi-dials      │                    │  make-rgbi-pic           │
│                       │                    │                          │
│  LaTeX only:          │                    │  obabel --gen2d          │
│    chemfig, tikz      │                    │      ↓  .mol             │
│    pdfcrop + gs       │                    │  mol2chemfig  [Indigo]   │
│                       │                    │      ↓  chemfig .tex     │
│                       │                    │  pdflatex ×2 + pdfcrop   │
└───────────┬───────────┘                    └────────────┬─────────────┘
            ▼                                             ▼
  rgbi-dial-table±H.pdf                        rgbi-mol-structure±H.pdf
```

**The two halves are independent, and that matters.** Only the structure
picture needs the awkward software. The dial diagrams come out of a stock TeX
Live and nothing else — so "RGBI does not work on this machine" is too coarse a
statement to act on, and a workshop participant who cannot install Open Babel
can still draw half the pictures:

```bash
scripts/rgbi_doctor.sh --dials-only
make-rgbi-dials --do-H
```

## 2. The `rgbi` command

```
rgbi [ options ] <file.FChk> | <file.molden>
```

| Option | Meaning |
|---|---|
| `--groups '{ { 1 4 6 } { 2 3 5 } }'` | Compute indices between two *groups* of atoms rather than between individual atoms. Default: every pair close enough to be bonded by the Cambridge Structural Database criteria. |
| `--ci-labels` | Print the covalent/ionic index pair above and below each bond, instead of the index and its % covalency. Affects what `make-rgbi-pic` draws. |
| `--help` | Full documentation, including the method summary and references. |

Output goes to `stdout` in the working directory, plus the `.tex` fragments of
§3. There is **no** `--basis` option and none is needed: the wavefunction comes
from the input file. `TONTO_BASIS_SET_DIRECTORY` is still read, as everywhere.

### Both routes work, and agree

`rgbi` and an ordinary `tonto` job produce the same fragments — verified on N2,
byte-identical. Use whichever suits:

```bash
rgbi N2.molden                     # argv-driven
tonto --input stdin                # job file with robydata= { … } + roby_analysis
```

One difference to know: `rgbi` leaves `output_theta_info` at its default (**on**),
whereas most `tests/rgbi/*/stdin` set it **off** — so the argv route gives you
the dial fragments and those test jobs do not.

## 3. What Tonto writes, and when

All from `ROBY:bond_analysis` in `foofiles/roby.foo`, via `stdout.redirect`:

| File | Gate |
|---|---|
| `rgbi-bondtable+H.tex`, `rgbi-bondtable-H.tex` | always |
| `rgbi-atom-labels+H.tex`, `rgbi-atom-labels-H.tex` | always |
| `rgbi-bondtable+H+vdw.tex` | `analyze_vdw_atom_pairs=` |
| `rgbi-dial-figures.tex` | `output_theta_info=` (default **YES**) |
| `rgbi-dial-table+H.tex`, `rgbi-dial-table-H.tex` | `output_theta_info=` |
| `<n>+H.tex`, `<n>-H.tex`, one per bond | `output_theta_info=` |
| `rgbi-data-<A>-<B>` | separate `put` |
| `geometry.xyz` | always (`.atom.put_xyz_file`) |

**The per-bond dial files are named after the bond number alone** — `1+H.tex`,
`2+H.tex`, … with no `rgbi-` prefix, and the dial table pulls them in with
`\input{1+H}`. They look like junk in a directory listing and are not.

## 4. Drawing the pictures

**Tonto draws them for you** at the end of a Roby analysis, if the tools are
installed — that is, if `make-rgbi-pic` is on your `PATH`. You get the
heavy-atom picture, and the `+H` one as well when the molecule has hydrogens.
Nothing to ask for and nothing to remember.

If the script is *not* installed, Tonto does nothing and says nothing: a machine
without the picture tools behaves exactly as it did before. If it *is* installed
and fails, that is reported once and the job carries on — the `.tex` fragments
and `geometry.xyz` are on disk either way, so you lose the convenience and
nothing else. The same contract as the post-HAR gnuplot plots
(`SYSTEM:call_gnuplot`).

To draw them yourself, or to redraw with different options:

```bash
make-rgbi-pic   --do-H       # structure + dials, hydrogens kept
make-rgbi-dials --do-H       # dials only (needs no Open Babel/Indigo/mol2chemfig)
```

**Neither needs a wavefunction file.** Open Babel derives the 2D depiction from
coordinates alone — it perceives the bonding itself and gets nothing extra from
a `.molden` or `.FChk` — and Tonto already writes `geometry.xyz` on every run,
so that is the default input. Verified on ylid (24 atoms): the `.mol` built from
`geometry.xyz` is identical to the one built from `ylid.molden`, 14 heavy atoms
and 15 bonds either way. `--molden` and `--fchk` remain for existing habits.

### `make-rgbi-pic`

| Option | Meaning |
|---|---|
| `--xyz <file>` | Lay out from an `.xyz` file. **Default: `geometry.xyz`.** |
| `--molden <file>`, `--fchk <file>` | Lay out from a wavefunction file instead. |
| `--do-H`, `--no-H` | Keep or delete hydrogens. Default: delete. Picks `+H` or `-H` fragments and output names. |
| `--do-ci`, `--no-ci` | Print c/i indices instead of % covalency. Pair with `rgbi --ci-labels`. |
| `--do-xyz` | Let `mol2chemfig` recompute the 2D coordinates. |
| `--do-tonto` | Run `tonto` first, to make the fragments. |
| `--skip-doctor` | Skip the dependency preflight. |
| `--quiet`, `--help` | |

### `make-rgbi-dials`

`--do-H`/`--no-H`, `--do-tonto`, `--skip-doctor`, `--quiet`, `--help` — same
meanings. It asks the doctor with `--dials-only`, so a missing Open Babel or
mol2chemfig will not stop it.

### Where the templates come from

`scripts/rgbi_doctor.sh --print-template-dir` is the **single** implementation
of the search, and both scripts ask it rather than keeping a copy that can
drift. In order: `$TONTO_RGBI_SCRIPT_DIRECTORY`, a git checkout's
`rgbi-scripts/`, an installed `<prefix>/share/tonto/rgbi-scripts`, and finally
`~/bin` — which is where they *used* to come from, unconditionally, so that
`rgbi-scripts/` in the repository was authoritative on no machine but the
author's.

## 5. Two traps in the LaTeX, both of which have cost time

### The two `chemfig`s are both needed — one fails loudly, one silently

`rgbi-mol-structure.tex` loads `chemfig` twice, apparently. It has to.

- `mol2chemfig.sty` does `\input{cf-pastebin.tex}` — a **vendored chemfig
  v1.2d from December 2015**, with the comment *"load this directly, don't mess
  with chemfig.sty"*. Remove it and the run dies at once:
  `Command \setbondstyle undefined`.
- `\usepackage{chemfig}` loads the **modern system chemfig**. Remove it and
  `pdflatex` **exits 0 with no error** — and draws the aromatic ring circle
  oversized and overlapping its own bonds, with the skeleton compressed. A
  wrong picture with no diagnostic.

Measured on ylid, 2026-08-09, against `tests/rgbi/ylid/rgbi-mol-structure+H.pdf`.
It was deleted once as redundant and had to be restored; there is now a note in
the template itself, where the deletion would be made.

### `pdflatex` must run twice

The bond labels are placed by TikZ node references resolved through the `.aux`
file, so after a single pass they float off to the side of the molecule in a
cluster. That looks exactly like a label-placement bug and is not one. Both
scripts run it twice.

## 6. Testing

`ctest -L rgbi` — 13 jobs, comparing `stdout` against blessed references. They
exercise the **numbers**, not the pictures.

For the pictures, 32 reference PDFs are committed across 11 of the 13
directories: three for each diatomic and five for `ylid`. Nothing compares them
automatically; they are visual targets.

- **Fast loop:** a diatomic (N2, BN, CO). Seconds, and still exercises both
  halves.
- **The gate:** `ylid`. The only case with hydrogens, so the only one that
  exercises the `+H`/`-H` split, the atom labelling and a multi-bond dial table.
  Its job takes about 2 minutes; drawing takes about 15 seconds.

Note that a picture run needs `output_theta_info= YES`, which most of those
`stdin` files turn off.

`ctest -R rgbi_doctor_selftest` (label `short`, so it is in CI) checks the
doctor still catches things. The doctor itself is deliberately not a ctest: CI
has none of the arcane software and would be permanently red. The install list
is covered separately by `docker/rgbi.Dockerfile` and
`.github/workflows/ci-rgbi.yml`.

## 7. Known defects and rough edges, not fixed

Kept together here rather than scattered through the sections above, so the
reference part stays readable.

**In the program**

- **`rgbi --help` calls the program `run_rgbi`**, which is the CMake target
  name, not what gets installed; and it points at a `./rgbi-script` folder,
  which is spelled `rgbi-scripts`. Both cosmetic. `hart` has an invariant check
  comparing `--help` against its option `case` labels
  (`scripts/check_hart_options.sh`); `rgbi` has three options and no such check.
- `CMakeLists.txt:883` pins a file to `-O2` because **"rgbi/BN's Roby
  populations were wrong"** at other optimisation levels. Read that comment
  before touching optimisation flags for this program. It is guarded by no test
  — see the macOS-in-CI item in `DEFERRED.md`.

**In the pictures**

- **The dial grid's column count is hard-coded to four** in three places per
  routine (`ROBY:put_dial_table_do_H`, `foofiles/roby.foo:7090`). Four dials
  need ~520 pt and `article`'s default `\textwidth` is ~345 pt, so the fourth
  column fell off the page and `pdfcrop` cut it — visible in the committed
  reference PDFs too. Worked around in `rgbi-dial-header.tex` by giving the page
  a large canvas; the proper fix is in `DEFERRED.md`.
- Nothing compares the reference PDFs automatically; they are eyeball targets.
