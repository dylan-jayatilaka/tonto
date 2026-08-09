# Running Tonto

Migrated from the project wiki (2026-08-05) so that it is versioned with the
code it describes.

## Example input

The `tests/` folder is the example collection — one directory per job.

- Input files are always called **`stdin`**.
- Reference output files are always called **`stdout`**.
- Some jobs need auxiliary input data files, which sit alongside.
- The file called **`IO`** lists the auxiliary files needed as input, and the
  temporary files produced during the run that should be deleted afterwards.
  For an argv-driven program such as `hart` it also carries `program:` and
  `args:` keys — see [`RUNNING_HART.md`](RUNNING_HART.md).

## Running it

1. Make an input file called `stdin`, or copy one you like from `tests/`.
2. Run the `tonto` executable from your build directory.

That is the whole thing.

> **Options are GNU long options.** Every Tonto program takes `--name` only —
> `tonto --input job.txt`, `hart --basis STO-3G`. The old single-dash spellings
> (`-i`, `-o`, `-b`, `-help`, `-basis`, …) were removed; a program given one now
> says which `--name` to use instead.

## Practical set-up

You can copy the executable wherever you like.

- You will generally also need the **`basis_sets`** folder in the same place, or
  else tell Tonto where it is — either in the `stdin` input file, or via the
  `TONTO_BASIS_SET_DIRECTORY` environment variable.
- A **symbolic link** named `tonto` pointing at the build directory's binary
  means every job picks up the latest build after a recompile.
- For long jobs, give the program a descriptive name —
  `tonto.this-is-for-oxalic-acid` — so concurrent runs can be told apart in
  `ps` and in job listings.

## What the programs are

| Path | Program |
|------|---------|
| `build/tonto` | the main program |
| `build/hart` | standalone Hirshfeld atom refinement (`hart --help`; see [`RUNNING_HART.md`](RUNNING_HART.md)) |

Smaller test and utility programs are built alongside them.
