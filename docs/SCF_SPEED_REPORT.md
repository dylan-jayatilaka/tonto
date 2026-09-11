# SCF and integral speed: benchmark data

**A working document** for the *Speed up the SCF and the integrals* item in `DEFERRED.md`.
Holds the measurements; the reasoning and plan are in `DEFERRED.md`.

## Karrikinolide grid ladder, 2026-09-11

BLYP/6-31G(d), 17 atoms, geometry and basis from the g09 checkpoint in
`tests/long/karrikinolide_blyp_6-31G(d)_Salvador_properties/`; promolecule guess,
`convergence= 1e-8`, `diis= { convergence_tolerance= 1e-4 }`; release build `06079254`
plus the Becke-default change, gfortran-14, one core, wall time by `/usr/bin/time`.
Raw inputs, outputs and timings: `~/tonto_runs/karrikinolide_grid_ladder_2026-09-11/`
(not in git).

| `accuracy=` | Stratmann-Scuseria | time | Becke (Becke adjustment) | time |
|---|---|---|---|---|
| `low` | −533.954518687 | 121 s | −533.954662894 | 120 s |
| `medium` | −533.954544224 | 157 s | −533.954503626 | 181 s |
| `high` | −533.954570020 | 226 s | −533.954514681 | 260 s |
| `best` | −533.954520631 | 1412 s | −533.954520090 | 1776 s |

g09 reference, `BLYP/6-31G(d) 6D SCF=(Tight,Conver=10) Int(Grid=199974)`: **−533.954519147**
(22 cycles). Differences from it: SS +4.6e-07 / −2.5e-05 / −5.1e-05 / −1.5e-06; Becke
−1.44e-04 / +1.55e-05 / +4.5e-06 / −9.4e-07 for `low` / `medium` / `high` / `best`. Cost
relative to `low`: 1.5×, 2.2×, 15×. Dylan's decision 2026-09-11: `medium` stays the default;
adaptive pruning is to reduce cost before `high` is reconsidered.

For comparison the water ladder (3 atoms, cc-pVDZ) is in `DFT_STANDARDISATION.md` §6b: there
the grid dominates and Becke is faster; here the SCF dominates and SS's screening is worth
about 15%.
