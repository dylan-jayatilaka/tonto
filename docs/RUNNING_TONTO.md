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
- **The default grid,** `accuracy= medium`, gives a GGA energy to about 1e-5
  hartree for molecules of any element up to Rn. From K on an atom has many
  more radial points than a light one (75, 130 and 190 in periods 4, 5 and 6,
  against 35 to 40), so a heavy atom costs several light ones.
- **r2SCAN energies need a finer grid than the default.** The energy error for
  water at the default `accuracy= medium` is 2e-4 hartree, against 3e-6 for PBE;
  use `becke_grid= { set_defaults accuracy= very_high }` or finer when energies
  are compared. A structure refinement does not need it: on urea the grid
  changes the goodness of fit by 0.004 and no bond length by a printed digit.

## Relativity

Beyond krypton the inner electrons move fast enough that a non-relativistic
calculation gives the wrong core density. `relativity_kind= x2c` in
`scfdata= { }` replaces the kinetic energy and nuclear attraction by the
scalar-relativistic X2C (exact two-component) one-electron Hamiltonian. Nothing
else in the job changes, and it works with every restricted and unrestricted
`kind=`, Hartree–Fock or DFT.

```
basis_name= x2c-TZVPall
scfdata= {
   kind= rks
   dft_functional= pbe
   relativity_kind= x2c
}
```

- **Use a basis set made for it.** `x2c-SVPall`, `x2c-TZVPall` and
  `x2c-TZVPPall` cover H to Rn with all electrons. A basis set contracted for a
  non-relativistic calculation, such as def2 or cc-pVXZ, is too stiff in the
  core for a heavy atom.
- **The atoms use it too.** The atomic calculations behind the promolecule and
  the Hirshfeld atoms are made with the same Hamiltonian as the molecule.
- **What it leaves out:** spin–orbit coupling; the relativistic correction to
  the electron repulsion; and the picture-change correction of the density,
  which matters only very near a heavy nucleus.
- **The nuclei are points** unless `nucleus_model= gaussian` is given, outside
  `scfdata= { }`. Each nucleus is then a Gaussian charge of the size tabulated
  by Visscher and Dyall, *At. Data Nucl. Data Tables* **67**, 207 (1997). It
  raises the energy of a mercury atom by 3.5 hartree, and is available only
  with X2C.
- **The printed energies** give the one-electron energy as one number, because
  the kinetic and nuclear attraction energies are not separate under X2C.
- **The cost** is one matrix diagonalisation of twice the number of primitive
  Gaussians, made once before the SCF.

The equations, how they relate to Dyall's elimination of the small component, the
routines and the checks are in [`REPORT_ON_RELATIVITY.md`](REPORT_ON_RELATIVITY.md).

## The programs

| Path | Program |
|------|---------|
| `build/tonto` | the main program |
| `build/hart` | standalone Hirshfeld atom refinement — [`RUNNING_HART.md`](RUNNING_HART.md) |

Smaller test and utility programs are built alongside them.
