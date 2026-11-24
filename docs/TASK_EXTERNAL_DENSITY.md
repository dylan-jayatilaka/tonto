# TASK: refinement with a density from an external program (ORCA, Psi4)

Working document. Started 2026-11-23 on branch `external-density` (from `develop` at `a1bf01b8`).

## Why

The TFVC paper asks whether r2SCAN is justified over Hartree-Fock for HAR, but nothing in the
study is more accurate than either. A correlated density (relaxed MP2, CCSD) is the reference.
Tonto has no MP2 or CCSD density. ORCA (installed, `~/bin/orca_6_1_1`) gives relaxed RI-MP2
densities; Psi4 (installed, `micromamba run -n psi4`) gives relaxed DF-CCSD densities through
its analytic gradient.

Measured cost per density, one core, def2-TZVP (scratchpad timings, 2026-11-23):

| molecule | basis functions | ORCA RI-MP2 relaxed | Psi4 DF-CCSD relaxed |
|---|---:|---|---|
| urea | 148 | 33 s | 272 s |
| L-alanine | 228 | 86 s | 2099 s |
| Gly-L-Ala | 370 | 227 s | about 10 h (scaled) |
| xylitol | 382 | 233 s | about 13 h (scaled) |

A refinement needs one density per cycle, about 12 cycles.

## Design

At each refinement cycle `.SCF` is replaced, when `scfdata= { external_program= ... }` is set,
by: write the geometry (and the cluster charges, if used) for the external program; run it;
read back its density matrix **into Tonto's own basis and atom list**.

The atoms and the basis must stay Tonto's own. `read_molden_file` rebuilds the atom list and
the basis from the file, which loses the atom labels and the ADPs of a refinement, so it
cannot be used inside HAR. Only the density matrix is imported.

The density is moved into Tonto's basis-function order and normalisation by a small driver
script, run as one external command. The driver runs ORCA or Psi4, converts the density, and
writes it as a Tonto ASCII density-matrix archive, which the SCF reads as its initial density.

The mapping (order, sign and normalisation of the spherical functions of each shell) is found
and checked from the overlap matrix: Tonto's `put_overlap_matrix` against Psi4's `S` for the
same molecule and basis. The overlap matrix fixes the mapping independently of any SCF.

Cluster charges: the charges are recomputed after each external density from that density
(the TFVC or Hirshfeld charges, as Tonto does now) and passed to the next cycle, one cycle
behind. They converge as the refinement does.

## Checks, in order

1. Overlap matrix: Psi4's S mapped into Tonto's order equals Tonto's S to 1e-10.
2. HF density: a Psi4 HF density with exact integrals (`scf_type pk`), mapped, equals Tonto's
   own HF density to SCF convergence; and a Tonto SCF started from it converges in one cycle
   at the same energy.
3. HF refinement through the driver reproduces Tonto's own HF refinement (urea, def2-SVP).
4. Then MP2 (ORCA) and CCSD (Psi4) refinements of urea and L-alanine.

## Notes

- Psi4's `molden()` needs the alpha density in the MO basis, or the occupations come out
  doubled or wrong (found while testing the molden route).
- ORCA writes `[5D]`, Psi4 `[5D7F]`; Tonto's molden reader accepts both since `709f7c18`.

## Findings

- **Tonto uses Cartesian d and f functions by default** (urea def2-TZVP: 168 functions, against
  148 spherical). Psi4 is therefore run with `puream false`, which gives the same functions.
- **Step 1 done (water, def2-SVP and def2-TZVP):** with Cartesian functions the two programs have
  the same shells in the same order, the same normalisation of every component, and differ only
  in the order of the components within a shell:

  | shell | Tonto | Psi4 |
  |---|---|---|
  | d | xx yy zz xy xz yz | xx xy xz yy yz zz |
  | f | xxx yyy zzz xxy xxz xyy yyz xzz yzz xyz | xxx xxy xxz xyy xyz xzz yyy yyz yzz zzz |

  Psi4's overlap matrix, so reordered, equals Tonto's to 2e-10 (the printed precision). A Psi4
  density matrix moves into Tonto's basis by the same reordering of rows and columns, with no
  scale factors.
- **Step 2 done (water, def2-SVP, RHF):** Psi4 with `puream false`, `scf_type pk`, its alpha
  density doubled and reordered as in step 1, equals Tonto's own converged density to 1.4e-10
  (Tonto's `density_mx,r` archive is the total density, trace(PS) = 10). Written as a Tonto archive
  and read with `initial_density= restricted`, it gives a DIIS gradient of 2e-10 at iteration 0, and
  Tonto stops after one cycle at -75.9622444320 Eh, equal to Psi4's energy to every printed digit.
- **Tonto's binary archive** `<name>.density_mx,r` is one Fortran sequential unformatted record:
  a 4-byte length, the n x n matrix in column order as 8-byte reals, the 4-byte length again.
- `put_overlap_matrix` must be preceded by `make_overlap_matrix`; otherwise, in a release build,
  it prints column numbers without end (the check that the matrix exists is compiled out).

## Log

- 2026-11-23: plan written; step 1 (overlap matrix) started.
- 2026-11-24: step 1 done (table above). Next: step 2, the HF density.
- 2026-11-24: step 2 done (scratchpad `hf2/`: `psi_hf.py`, the archive writer). Next: step 3, the
  driver script and an HF refinement of urea through it, against Tonto's own.
