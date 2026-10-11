# Running Tonto

## Running a job

1. Make an input file called `stdin`, or copy one from `tests/`.
2. Run `tonto` in that directory. It writes its output to `stdout`.

`tonto` takes no file arguments. Its options, all GNU long options:

| Option | Effect |
|---|---|
| `--input <file>` | Read the job from `<file>` instead of `stdin` |
| `--output <file>` | Write the output to `<file>` instead of `stdout` |
| `--basis-library <dir>` | Where the basis-set files are, if not `$TONTO_BASIS_SET_DIRECTORY` |
| `--version` | Print the version and stop |
| `--help` | Print the usage and stop |

The single-dash spellings (`-i`, `-o`, `-b`, `-help`) were removed; a program
given one says which `--name` to use instead. `hart` has its own, larger option
set — see [`RUNNING_HART.md`](RUNNING_HART.md).

## Example inputs

`tests/` is the example collection, one directory per job:

- The input file is always `stdin`.
- The reference output is always `stdout`.
- Auxiliary data files sit alongside.
- `IO` lists the auxiliary files the job reads, and the temporary files to
  delete afterwards. For an argv-driven program such as `hart` it also carries
  `program:` and `args:` keys.

## Where to keep the executable and the basis sets

The executable can go anywhere. Two things help:

- Tonto needs the **`basis_sets`** folder — in the same place, or named in the
  input file, or given by `--basis-library`, or by the
  `TONTO_BASIS_SET_DIRECTORY` environment variable. A path longer than 256
  characters must use one of the latter two: a path named *in the input file*
  arrives on an input line, and an input line is limited to 256 characters.
- A **symbolic link** named `tonto` pointing into the build directory means
  every job picks up the latest build after a recompile.
- For long jobs, copy the executable to a descriptive name —
  `tonto.oxalic-acid` — so concurrent runs can be told apart in `ps`.

## Density functionals

A DFT job has `kind= rks` or `kind= uks` in `scfdata= { }`, and names its
functional there in one of two ways.

**One keyword for the whole functional**, `dft_functional=`:

| Name | Functional | Needs libxc |
|---|---|---|
| `lda` | Slater exchange, VWN correlation (functional V) | no |
| `xalpha` | Xα exchange | no |
| `blyp` | Becke 88 exchange, LYP correlation | no |
| `b3lyp-gaussian`, or `b3lyp` | B3LYP as in Gaussian, NWChem and Psi4 | no |
| `b3lyp-orca` | B3LYP as in ORCA, Turbomole and GAMESS | no |
| `pbe` | PBE | yes |
| `pbe0` | PBE0, with 25 % exact exchange | yes |
| `r2scan` | r2SCAN | yes |
| any libxc name | e.g. `hyb_gga_xc_b3lyp`, `hyb_mgga_xc_r2scan0` | yes |

**Two keywords**, `dft_exchange_functional=` and `dft_correlation_functional=`.
Each takes one of Tonto's own names (`slater`, `xalpha`, `becke88`, `b3lypx`,
`b3lypgx`; `vwn5`, `vwn3`, `lyp`, `b3lypc`, `b3lypgc`; `none`) or a libxc name
such as `gga_x_pbe` and `gga_c_pbe`. The two keywords cannot be given together
with `dft_functional=`.

- **The two B3LYP forms differ** in their local correlation, by 37 millihartree
  in the energy of a water molecule. Say which one was used.
- **A libxc name** is the functional's name in the
  [libxc list](https://libxc.gitlab.io/functionals/), in lower case. Tonto must
  be built with `-DLIBXC=ON`; otherwise the name is refused.
- **Not available:** range-separated hybrids, functionals with non-local
  correlation, and functionals that need the Laplacian of the density.
- **r2SCAN energies need a finer grid than the default.** The energy error for
  water at the default `accuracy= medium` is 2e-4 hartree, against 3e-6 for PBE;
  use `becke_grid= { set_defaults accuracy= very_high }` or finer when energies
  are compared. A structure refinement does not need it: on urea the grid
  changes the goodness of fit by 0.004 and no bond length by a printed digit.

## The programs

| Path | Program |
|------|---------|
| `build/tonto` | the main program |
| `build/hart` | standalone Hirshfeld atom refinement — [`RUNNING_HART.md`](RUNNING_HART.md) |

Smaller test and utility programs are built alongside them.
