# Workshop answers

Answers to the questions in [WORKSHOP.md](WORKSHOP.md). Have a guess first:
the file names are chosen to be guessable.

---

## Exercise 1 — what each output file is for

A two-second `hart` run on 88 reflections leaves about thirty files behind.
Almost all are results rather than debris: a refinement produces a structure, a
set of calculated structure factors, a wavefunction, a residual density and
diagnostic plots, each in a format some other program expects.

Everything is named `<job>.something`, where `<job>` is what you passed to
`--job`. That is the whole naming scheme, and it is why two jobs can share a
directory without overwriting each other.

### The two files you read

| File | What it is |
|---|---|
| `nh3.out` | The log. Everything the program said. `--job` names it; `--output` moves it. |
| `nh3.err` | The error stream. **Empty is good.** A failed run leaves its complaint here. |

### The refined structure

| File | What it is |
|---|---|
| `nh3.archive.cif` | The refined structure as an ordinary CIF: fractional coordinates and ADPs for the **asymmetric unit**, with esds. This is the file you deposit. |
| `nh3.HBB.cif2` | The same refinement, but the **whole fragment** in Cartesian coordinates, with ADPs also given in the inertial and principal-axis frames, and their esds. CIF2 format. This is the one a quantum calculation wants — exercise 3 reads urea's. Named for Hans-Beat Bürgi, whose convention it follows. |
| `nh3.cartesian.cif2` | The geometry in Cartesian coordinates, CIF2. |
| `nh3.fractional.cif1` | The geometry in fractional coordinates, CIF1. |

**Why so many?** "The structure" means different things to different readers. A
database wants the asymmetric unit in fractional coordinates; a quantum
chemistry program wants a complete molecule in Cartesians; a
displacement-parameter analysis wants the ADPs in a molecular frame. Converting
between them by hand invites a factor of 2π or a transposed matrix.

### The calculated structure factors

| File | What it is |
|---|---|
| `nh3.archive.fcf` | *F*<sub>calc</sub> and *F*<sub>obs</sub> in CIF format — the file validation tools such as checkCIF read. |
| `nh3.fcf6` | The same reflection data in SHELX `LIST 6` FCF layout. |
| `nh3.archive.fco` | The same again, in the layout XD expects. |

Three files, one content, three formats — because the downstream programs have
never agreed on one.

### The wavefunction

| File | What it is |
|---|---|
| `nh3.MOs,r` | The molecular orbital coefficients. |
| `nh3.MO_energies,r` | The orbital energies. |
| `nh3.density_mx,r` | The density matrix. |

These are Tonto **archive** files: `<job>.<what>,<genre>`, and the `,r` means
*restricted* — one spatial orbital per electron pair. An unrestricted
calculation writes `,a` and `,b` instead.

These are what makes HAR more than a way of moving hydrogens: a refinement that
ends in a wavefunction can be asked questions afterwards, which is what
exercises 2 and 3 do. A `tonto` job reads them back with `read_archive`.
Exercises 2 and 3 end with `delete_scf_archives`, so you will not find them
there — they are large, and the analysis has already run.

### The residual density

| File | What it is |
|---|---|
| `nh3.residual_density,cell.cube` | ρ<sub>obs</sub> − ρ<sub>calc</sub> on a grid over the unit cell, in Gaussian `.cube` format — readable by VMD, PyMOL, VESTA, Avogadro and most viewers. |

The name is `<job>.<what>,<region>.<format>`. This is where you look for what
the model has missed: bonding density, a wrongly placed hydrogen, an
unmodelled disorder.

### The diagnostic plots

Four plots, four files each:

| Suffix | What it is |
|---|---|
| *(no suffix)*, e.g. `nh3.F_z_vs_stl` | The data — plain columns of numbers, plus *h k l* so you can identify a point. |
| `.labels` | Where each outlier's (*h k l*) is written on the plot, and its leader line. |
| `.gnuplot` | The gnuplot script that draws it. **Edit and re-run this** to change the plot; nothing needs recompiling. |
| `.png` | The picture, produced by running gnuplot on the script. |

The four plots themselves are described in WORKSHOP.md: `QQ_plot` (with its
data file historically called `QQ_plot_with_hkl`), `F_z_vs_stl`,
`F_z_vs_F_exp` and `Delta_F_vs_stl`.

| File | What it is |
|---|---|
| `fit.log` | **Not Tonto's.** It is *gnuplot's* own log, written when the QQ-plot script fits its straight line. Harmless, and easy to mistake for a refinement log. |

### Why there are no scratch files

Nothing here is a scratch file or a temporary integral. Every file is either a
result in a format another program reads, or a plot you can redraw: a
refinement is finished when its results are in the formats the rest of
crystallography uses.
