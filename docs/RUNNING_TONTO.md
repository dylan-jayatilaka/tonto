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
  `TONTO_BASIS_SET_DIRECTORY` environment variable.
- A **symbolic link** named `tonto` pointing into the build directory means
  every job picks up the latest build after a recompile.
- For long jobs, copy the executable to a descriptive name —
  `tonto.oxalic-acid` — so concurrent runs can be told apart in `ps`.

## The programs

| Path | Program |
|------|---------|
| `build/tonto` | the main program |
| `build/hart` | standalone Hirshfeld atom refinement — [`RUNNING_HART.md`](RUNNING_HART.md) |

Smaller test and utility programs are built alongside them.
