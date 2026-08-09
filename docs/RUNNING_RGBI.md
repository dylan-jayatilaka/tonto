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

For installing the pieces, see **`docs/INSTALLING_RGBI.md`**. This document is
the developer reference.

---

## 1. The pipeline, and the fact that it has two independent halves

Measured on 2026-08-09, not inferred — a real N2 job was run and each stage
watched (CLAUDE.md §2). **The two pictures have very different dependencies**,
and this had not been written down anywhere:

| Picture | Produced by | Needs |
|---|---|---|
| **Dial diagrams** (`rgbi-dial-table{+,-}H.pdf`) | `make-rgbi-dials` | `pdflatex`, `pdfcrop`, and the LaTeX packages `chemfig`, `tikz`, `xcolor`, `longtable` |
| **Molecular structure** (`rgbi-mol-structure{+,-}H.pdf`) | `make-rgbi-pic` | all of the above **plus** `obabel`, python **Indigo**, and **`mol2chemfig`** |

Only the structure picture needs the awkward software. The dial diagrams come
out of a stock TeX Live and nothing else — which is worth knowing before anyone
concludes that "RGBI does not work on this machine".

The flow:

```
   rgbi / tonto  ──▶  rgbi-*.tex fragments  ──┬──▶  make-rgbi-dials  ──▶  rgbi-dial-table{+,-}H.pdf
   (ROBY:bond_analysis)                       │
                                              └──▶  make-rgbi-pic
                                                      obabel  (.molden/.fchk ──▶ .mol, 2D coords)
                                                      mol2chemfig (.mol ──▶ chemfig .tex)   [uses Indigo]
                                                      pdflatex + pdfcrop
                                                                     ──▶  rgbi-mol-structure{+,-}H.pdf
```

## 2. What Tonto writes, and when

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

Two notes on that table.

- **The per-bond dial files are named after the bond number alone** — `1+H.tex`,
  `2+H.tex`, … with no `rgbi-` prefix, and `rgbi-dial-table+H.tex` pulls them in
  with `\input{1+H}`. Easy to mistake for junk, easy to collide with anything
  else in the directory called `1+H.tex`. Left as is for now: renaming them
  changes what every `\input` line says.
- **Most `tests/rgbi/*/stdin` set `output_theta_info= NO`**, so the suite as it
  stands does *not* produce the dial files. A job used to gate the pictures has
  to turn it on.

## 3. Baseline measurement, 2026-08-09

The starting point this restoration was written against. `tests/rgbi/N2`, with
`output_theta_info= YES`, against `release/tonto`:

- **Tonto's half is healthy.** All eight `.tex` fragments were written, exit 0.
- **`make-rgbi-dials` is healthy.** `rgbi-dial-table+H.pdf` was produced and is
  **identical to the committed reference** `tests/rgbi/N2/rgbi-dial-table-H.pdf`
  — τ(N1-N2) = 2.88, covalent 2.88, ionic 0.00.
- **`make-rgbi-pic` fails, and exits 0 while doing it.** In full:

  ```
  rm: cannot remove 'rgbi-structure.tex': No such file or directory
  1 molecule converted                                   <- obabel fine
  mol2chemfig: .../mol2chemfigpy3/bin/python: bad interpreter
  !!! Error: Input file `rgbi-mol-structure.pdf' not found!
  mv: cannot stat 'rgbi-mol-structure-crop.pdf'
  EXIT=0
  ```

  `mol2chemfig` is installed but unrunnable — its pipx virtual-environment
  interpreter has been removed from under it, which `command -v mol2chemfig`
  cannot see. It leaves a **zero-byte** `rgbi-structure.tex`, `pdflatex` then
  fails, and because every LaTeX run in the script is redirected to
  `/dev/null` the only symptom is a picture that silently does not appear —
  or, worse, the *previous* run's picture left standing.

This is why `scripts/rgbi_doctor.sh` exists, and why it tests that
`mol2chemfig` **executes** rather than that it is present.

## 4. The reference pictures in `tests/rgbi/`

32 PDFs are committed across 11 of the 13 test directories: three for each
diatomic (`rgbi-mol-structure.pdf`, `rgbi-mol-structure-H.pdf`,
`rgbi-dial-table-H.pdf`) and five for `ylid`, which also has the `+H` forms.
They were made by hand and are the visual targets.

Use a **diatomic** (N2, BN, CO) for the fast edit-run loop — seconds, and it
still exercises both halves. Use **ylid** as the gate: it is the only case with
H atoms, so it is the only one that exercises the `+H`/`-H` split, the atom
labelling and a multi-bond dial table.

## 5. The two `chemfig`s are both needed — one fails loudly, one silently

`rgbi-mol-structure.tex` loads `chemfig` twice, apparently. It has to, and the
reason is worth knowing because the redundant-looking one was deleted once and
had to be restored.

- `mol2chemfig.sty` does `\input{cf-pastebin.tex}` — a **vendored chemfig
  v1.2d from December 2015**, with the comment *"load this directly, don't mess
  with chemfig.sty"*. Remove it and the run dies at once:
  `Command \setbondstyle undefined`.
- `\usepackage{chemfig}` loads the **modern system chemfig**. Remove it and
  `pdflatex` **exits 0 with no error** — and draws the aromatic ring circle
  oversized and overlapping its own bonds, with the whole skeleton compressed.
  A wrong picture with no diagnostic.

Measured on ylid, 2026-08-09, against `tests/rgbi/ylid/rgbi-mol-structure+H.pdf`.

**And `pdflatex` must run twice.** The bond labels are placed by TikZ node
references resolved through the `.aux` file, so after a single pass they float
off to the side of the molecule in a cluster. That looks exactly like a
placement bug and is not one — it is a missing second pass. Both scripts
already run `pdflatex` twice for this reason.

## 6. Known defects, not yet fixed

- `foofiles/crystal.foo` "free" plots (see `docs/PLOT_PLAN.md`) — unrelated to
  RGBI but in the same neighbourhood.
- `CMakeLists.txt:883` pins a file to `-O2` because **"rgbi/BN's Roby
  populations were wrong"** at other optimisation levels. Read that comment
  before touching optimisation flags for this program.
- `~/bin/make-rgbi-molden` on Dylan's machine is a third, older script,
  hard-coded to a file called `test.molden` and printing `iest.molden not
  found` when it is absent. It is superseded by `make-rgbi-pic --molden` and is
  deliberately **not** brought into the repository.
