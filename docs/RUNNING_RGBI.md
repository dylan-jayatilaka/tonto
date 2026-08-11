# Running `rgbi` — Roby-Gould bond indices and their pictures

`rgbi` is the third installed executable, alongside `tonto` and `hart`. It
computes Roby-Gould bond indices from a Gaussian `.FChk` or a `.molden` file,
and writes the LaTeX fragments for two pictures: a **labelled molecular
structure** and a page of **dial diagrams**.

The method is described in Gould *et al.* (2008), *Theor. Chem. Acc.* **119**,
275–290, and Alhameedi *et al.* (2018), *Int. J. Quantum Chem.* e25603, and in
the Grabowsky chapter, Jayatilaka (2025).

To install the picture tools see [`INSTALLING_RGBI.md`](INSTALLING_RGBI.md); to
check an installation, run `scripts/rgbi_doctor.sh`.

---

## 1. The pipeline

```
                    ┌─────────────────────────────┐
                    │  rgbi <file>                │   or a tonto job file
                    │  (or tonto + roby_analysis) │   with robydata= { … }
                    └──────────────┬──────────────┘
                                   │
                                   ▼
                    ┌─────────────────────────────┐
                    │  rgbi-*.tex  fragments      │
                    │  geometry.xyz               │
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

**The two halves are independent.** Only the structure picture needs Open Babel
and mol2chemfig; the dial diagrams need a stock TeX Live and nothing else. If
you cannot install the awkward half, you can still draw the dials:

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
| `--ci-labels` | Print the covalent/ionic index pair above and below each bond, instead of the index and its % covalency. Changes what `make-rgbi-pic` draws. |
| `--help` | Full documentation, including the method summary and references. |

Output goes to `stdout` in the working directory, plus the `.tex` fragments of
§3. There is no `--basis` option and none is needed: the wavefunction comes from
the input file.

### Running it through `tonto` instead

`rgbi` and an ordinary `tonto` job produce the same fragments. Use whichever
suits:

```bash
rgbi N2.molden                     # argv-driven
tonto --input stdin                # job file with robydata= { … } + roby_analysis
```

One difference: `rgbi` leaves `output_theta_info` at its default (**on**),
whereas most `tests/rgbi/*/stdin` set it **off** — so the argv route gives you
the dial fragments and those test jobs do not.

## 3. The files Tonto writes

All from `ROBY:bond_analysis`:

| File | Written when |
|---|---|
| `rgbi-bondtable+H.tex`, `rgbi-bondtable-H.tex` | always |
| `rgbi-atom-labels+H.tex`, `rgbi-atom-labels-H.tex` | always |
| `rgbi-bondtable+H+vdw.tex` | `analyze_vdw_atom_pairs=` |
| `rgbi-dial-figures.tex` | `output_theta_info=` (default **YES**) |
| `rgbi-dial-table+H.tex`, `rgbi-dial-table-H.tex` | `output_theta_info=` |
| `<n>+H.tex`, `<n>-H.tex`, one per bond | `output_theta_info=` |
| `rgbi-data-<A>-<B>` | separate `put` |
| `geometry.xyz` | always |

The per-bond dial files are named after the bond number alone — `1+H.tex`,
`2+H.tex`, … with no `rgbi-` prefix — and the dial table pulls them in with
`\input{1+H}`. They look like junk in a directory listing and are not.

## 4. Drawing the pictures

**Tonto draws them for you** at the end of a Roby analysis, if `make-rgbi-pic`
is on your `PATH`: the heavy-atom picture, and the `+H` one as well when the
molecule has hydrogens.

If the script is not installed, Tonto says nothing and the job is unaffected. If
it is installed and fails, that is reported once and the job carries on; the
`.tex` fragments and `geometry.xyz` are written either way.

To draw them yourself, or to redraw with different options:

```bash
make-rgbi-pic   --do-H       # structure + dials, hydrogens kept
make-rgbi-dials --do-H       # dials only (no Open Babel/Indigo/mol2chemfig)
```

Neither needs a wavefunction file. Open Babel derives the 2D depiction from
coordinates alone, and Tonto writes `geometry.xyz` on every run, so that is the
default input. `--molden` and `--fchk` remain for existing habits.

### `make-rgbi-pic`

| Option | Meaning |
|---|---|
| `--xyz <file>` | Lay out from an `.xyz` file. **Default: `geometry.xyz`.** |
| `--molden <file>`, `--fchk <file>` | Lay out from a wavefunction file instead. |
| `--do-H`, `--no-H` | Keep or delete hydrogens. Default: delete. Picks the `+H` or `-H` fragments and output names. |
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

`scripts/rgbi_doctor.sh --print-template-dir` is the single implementation of
the search, and both scripts ask it. In order: `$TONTO_RGBI_SCRIPT_DIRECTORY`,
a git checkout's `rgbi-scripts/`, an installed
`<prefix>/share/tonto/rgbi-scripts`, then `~/bin`.

## 5. Two LaTeX traps

### Both `chemfig` loads are needed, and one fails silently

`rgbi-mol-structure.tex` appears to load `chemfig` twice. It has to.

- `mol2chemfig.sty` does `\input{cf-pastebin.tex}` — a vendored chemfig v1.2d
  from 2015. Remove it and the run dies at once with
  `Command \setbondstyle undefined`.
- `\usepackage{chemfig}` loads the modern system chemfig. Remove it and
  `pdflatex` **exits 0 with no error**, drawing the aromatic ring circle
  oversized and overlapping its own bonds, with the skeleton compressed.

A note in the template itself says so, where the deletion would be made.

### `pdflatex` must run twice

The bond labels are placed by TikZ node references resolved through the `.aux`
file. After a single pass they cluster off to the side of the molecule, which
looks like a label-placement bug and is not. Both scripts run it twice.

## 6. Testing

`ctest -L rgbi` — 13 jobs, comparing `stdout` against blessed references. They
exercise the numbers, not the pictures.

For the pictures, 32 reference PDFs are committed across 11 of the 13
directories. Nothing compares them automatically; they are visual targets.

- **Fast loop:** a diatomic (N2, BN, CO). Seconds, and exercises both halves.
- **The gate:** `ylid` — the only case with hydrogens, so the only one that
  exercises the `+H`/`-H` split, the atom labelling and a multi-bond dial table.
  About 2 minutes to run, 15 seconds to draw.

A picture run needs `output_theta_info= YES`, which most of those `stdin` files
turn off.

`ctest -R rgbi_doctor_selftest` checks the doctor still catches things. The
doctor itself is not a ctest: CI has none of the arcane software. The install
list is covered by `scripts/docker/rgbi.Dockerfile` and `.github/workflows/ci-rgbi.yml`.

Known defects and rough edges are in [`../DEFERRED.md`](../DEFERRED.md).
