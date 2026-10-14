# Grid optimisation: the drivers

Scripts for measuring the error of Tonto's integration grids — the DFT exchange-correlation grid,
and the COSX exchange grid — on a set of test systems. The plan is
`docs/TASK_ON_GRID_OPTIMIZATION.md`; the findings are `docs/REPORT_ON_GRID_OPTIMIZATION.md`.

Each measurement integrates one quantity on one grid for one fixed, converged density, so the
change from grid to grid is the grid error and nothing else.

## Where things are

| what | where |
|---|---|
| these scripts, and the test systems (`systems.py`) | here, versioned with the code |
| the results store, one CSV file per basis set | `$GRID_STORE`, default `~/Dropbox/tonto_data/grid_optimization` |
| a run folder: the binary, densities, inputs and outputs | any directory; the scripts are run from inside it |

The store is the record. A run folder can be deleted once `collect.py` has run: everything in it
can be made again.

## Running

From inside a run folder, with `S` this directory:

```
$S/install_binary.sh                    # copies release/tonto here as tonto.grid, with its commit
python3 $S/make_jobs.py def2-SVP best   # one SCF input per system: <basis>/<system>/stdin
$S/run_scf.sh def2-SVP                  # the SCFs, 8 at a time; skips converged ones
python3 $S/scan.py def2-SVP ref cutoffs radial angular levels
$S/run_scans.sh def2-SVP                # the scans, 8 at a time, then collect.py
python3 $S/tables.py def2-SVP           # tables from the store alone
python3 $S/radial_needs.py def2-SVP     # radial points each period needs for each target
```

Options to `scan.py` and `tables.py`, after the basis:

| option | effect |
|---|---|
| `partition=salvador` | grids with `partition_scheme= salvador`; files `scan_<stage>_salvador.*` |
| `hchi=S` | adds `partition_h_chi_scale= S`: the Salvador boundary between H and a heavier atom moves away from H |
| `variant=ionic` | the promolecule is built from the ions in `systems.IONIC` (`initial_atomic_states=`); only those systems |
| `systems=A,B,...` | only these systems (`scan.py`) |

All variants live side by side in the store. A row's `variant` names a change outside the grid
block, and is part of its key.

## Stopping, restarting, extending

- **Stop:** kill `run_scans.sh` and its `tonto.grid` jobs.
- **Restart:** `collect.py`, then `scan.py` and `run_scans.sh` again. `scan.py` leaves out every
  evaluation already in the store, and `run_scans.sh` skips any output that finished.
- **A new test system:** add it to `systems.py` and run the sequence. Only it is computed.
- **A changed geometry:** its hash changes, so it is computed afresh; the old rows stay.
- **A code change that alters what a grid block means:** bump `GRID_CODE` in `store.py`. The old
  rows stay but no longer count as done.

A row's key is system, geometry hash, basis, the grid block exactly as given to Tonto,
`GRID_CODE`, and the integrand when it is not the XC energy.

## Stages

| stage | what is varied | error measured against |
|---|---|---|
| `ref` | two large unpruned grids, A and B | each other |
| `cutoffs` | each cutoff tightened in turn, on grid A | the default cutoffs |
| `radial` | the radial count of one period; angular order 59, no pruning | the reference count |
| `angular` | the non-H order, then the H order; reference radial counts, no pruning | order 101 |
| `levels` | the seven named accuracy levels | grid A |

`tables_lib.py` holds the lookups into the store that the analysis scripts share. A value is found
by its grid block, so a grid measured once, under any stage, serves every stage that uses it.

## Notes for test systems

- The atomic SCF behind the promolecule guess needs damping and level shifting for Ti, Cr and
  Ni; `make_jobs.py` adds them for those elements.
- `make_jobs.py` sets `output= TRUE` in `scfdata=`: without it some SCFs print nothing.
