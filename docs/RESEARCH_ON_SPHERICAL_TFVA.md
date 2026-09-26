# Research: spherically averaged Salvador atoms (`sph-tfva`)

A working document (see `CLAUDE.md` §1). It records what was measured about the
`partition_model= sph-tfva` model, and is deleted when the item closes; its lasting
residue goes into the keyword help and the user pages.

## The model

A Salvador atom (a topological fuzzy Voronoi atom, TFVA) is the molecular density times
the Salvador cell function W_c(r) of atom c. The `sph-tfva` model averages that atomic
density over spheres centred on the nucleus, and uses the average in place of the
atom:

- ρ̄_c(r) = (1/4π) ∮ W_c ρ dΩ, on each radial shell of the atom's Becke grid;
- f_c(k) = Σ_i 4π r_i² w_i ρ̄_c(r_i) sin(k r_i)/(k r_i), times the position phase
  exp(i k·r_c).

What it keeps: each atom's charge and the radial shape of its density (so charge transfer
and contraction or expansion, much as a kappa-refined spherical atom, but taken from the
wavefunction). What it loses: everything aspherical -- bonding density, lone pairs.
So it should fit worse than Salvador and better than the ordinary spherical-atom model.

Code: `MOLECULE.RHO:make_sph_TFVA_atom_FFs`, `make_sph_avgd_SA_ED_grid`,
`make_sph_avgd_SA_ED_v1`; the transform is `FOURIER_SUMS:sinc_kr_sums`.

## Bugs found and fixed

The option had never been run. Three bugs, all fixed on branch `fourier-sums`:

1. The sin(kr)/kr sum was computed and never used; the form factors were the position
   phase alone.
2. The averaged density was weighted by the bare radial weights from
   `BECKE_GRID:radial_grid_for_atom`, missing 4πr² (and s³ for scaled atomic grids).
   The radii and weights now come from `radial_shell_for_atom`.
3. The average used a fixed 25 × 50 Gauss–Legendre grid on every shell. It now uses each
   shell's Lebedev grid from `BECKE_GRID`; `make_sph_avgd_SA_ED_v1`, written for this
   and never called, placed its points at radius r² and was rewritten.

Found on the way, not specific to this model: `BECKE_GRID:make_Salvador_cell_fn` checked
`pts.dim1` where it means `pts.dim2`. The check exists only in debug builds, so every
debug run using Salvador atoms stopped there.

## Checks that the model is computed correctly

Urea, STO-3G (`tests/long/urea_rhf_STO-3G_HAR` with `partition_model=` changed).

- Electron counts Σ 4πr²w ρ̄: 9.1497, 8.2163, 3.4540, 0.6985, 0.7832 -- the Salvador
  populations to four decimals, with both the old and the Lebedev average.
- Form factors against the full Salvador ones, computed in the same run: largest
  difference 0.05–0.31 per atom, against values up to 8. At the smallest |k| 8.19
  against 8.15; at the largest (≈ 1.4 Å⁻¹) 1.036 against 1.042. The difference is the
  aspherical part the average removes.
- The first least-squares fit starts at R = 0.054, against 0.039 for Salvador.

## What happens in a refinement

| urea | STO-3G | def2-SVP |
|---|---|---|
| Salvador | R 0.038, converges (7 cycles) | R 0.0190, GoF 3.54, converges (8) |
| spherical H, aspherical C/N/O | R 0.041, converges (5) | R 0.0209, GoF 3.86, converges (6) |
| `sph-tfva`, all spherical | H1 runs away, R = 1 | R 0.044, GoF 11.6, never converges |
| `sph-tfva`, H with isotropic U | reaches R 0.0533, then flip-flops for ever | -- |
| `sph-tfva`, H fixed | R 0.096 | -- |

The mixed rows used a temporary build; the model choice by element is not in the code.
A run with the reverse mix (aspherical H, spherical C/N/O) moved H1 by 0.79 bohr in its
first cycle and then crashed; the crash may be the temporary code and was not checked.

The flip-flops are two-state oscillations of the outer SCF-and-fit loop: in STO-3G with
isotropic H, H3's U_iso alternates between two values 0.0018 Å² apart (shift/esd 0.14,
against a convergence test of 0.01); in def2-SVP H1 moves 0.034 bohr back and forth.

## Conclusion so far

Spherical averaging is harmless for hydrogen -- the Salvador H is nearly spherical -- and
harmful for C, N and O: without their bonding density, the fit tries to rebuild it by
moving the hydrogens and inflating their U. The first explanation offered, that the
hydrogens were the problem, was wrong; the mixed-model runs decided it.

No case has yet been found where the all-spherical model works. The keyword help marks it
experimental.

## Open

- A crystal of nearly spherical atoms, where the model might work: ionic (NaCl, MgO),
  a simple metal, a noble-gas solid.
- One or two more molecules.
- Whether "spherical H only" should become a model of its own.
- The second "Structure refinement results" block prints a higher R than the first for
  every model (Salvador 0.038 → 0.042, `sph-tfva` 0.053 → 0.084): find what it is.
- `MOLECULE.HAR:make_LS_mx` builds Hirshfeld form factors whatever the partition model.
