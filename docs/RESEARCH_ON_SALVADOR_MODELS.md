# Research: variants of the Salvador atom model

Variants of the Salvador (TFVA) partition, measured on urea: `sph-tfva`, the
spherically averaged Salvador atom; `tfvp`, the Salvador atom with its radii taken
from the promolecule; and `sph-tfvp`, both at once.

# 1. Spherically averaged Salvador atoms (`sph-tfva`)

A working document (see `CLAUDE.md` §1). It records what was measured about these
models, and is deleted when the item closes; its lasting residue goes into the keyword
help and the user pages.

### The model

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

### Bugs found and fixed

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

### Checks that the model is computed correctly

Urea, STO-3G (`tests/long/urea_rhf_STO-3G_HAR` with `partition_model=` changed).

- Electron counts Σ 4πr²w ρ̄: 9.1497, 8.2163, 3.4540, 0.6985, 0.7832 -- the Salvador
  populations to four decimals, with both the old and the Lebedev average.
- Form factors against the full Salvador ones, computed in the same run: largest
  difference 0.05–0.31 per atom, against values up to 8. At the smallest |k| 8.19
  against 8.15; at the largest (≈ 1.4 Å⁻¹) 1.036 against 1.042. The difference is the
  aspherical part the average removes.
- The first least-squares fit starts at R = 0.054, against 0.039 for Salvador.

### What happens in a refinement

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

### Conclusion so far

Spherical averaging is harmless for hydrogen -- the Salvador H is nearly spherical -- and
harmful for C, N and O: without their bonding density, the fit tries to rebuild it by
moving the hydrogens and inflating their U. The first explanation offered, that the
hydrogens were the problem, was wrong; the mixed-model runs decided it.

No case has yet been found where the all-spherical model works. The keyword help marks it
experimental.

### Open

- A crystal of nearly spherical atoms, where the model might work: ionic (NaCl, MgO),
  a simple metal, a noble-gas solid.
- One or two more molecules.
- Whether "spherical H only" should become a model of its own.
- The second "Structure refinement results" block prints a higher R than the first for
  every model (Salvador 0.038 → 0.042, `sph-tfva` 0.053 → 0.084): find what it is.
- `MOLECULE.HAR:make_LS_mx` builds Hirshfeld form factors whatever the partition model.

# 2. Salvador atoms with promolecule radii (`tfvp`)

### The model

A Salvador atom's cell function uses the Mayer–Salvador radius R(A.B) for each bonded
pair: the minimum of the density along the A–B line. `tfvp` (topological fuzzy Voronoi
proatom) finds those minima on the **promolecule** density -- the sum of the atoms'
ANO densities -- instead of the molecular density. Everything else is the Salvador
model: the atom is still the molecular density times the cell function.

The point: the partition then depends only on the geometry, not on the wavefunction,
so it is the same for every method and basis at a given geometry.

Code: `MOLECULE.RHO:make_Salvador_radii_promolecule`, chosen in `make_Salvador_radii`
when `partition_model= tfvp`; branch `tfvp`.

### How far the promolecule moves the radii

Urea, def2-SVP, at the geometry refined with Salvador atoms (its radii table). R(A.B) is the distance from A to the boundary
with B.

| bond | R(A–B) /Å | R(A.B) molecule /Å | R(A.B) promolecule /Å | ratio |
|---|---|---|---|---|
| O–C, from O | 1.2557 | 0.8425 | 0.8159 | 1.033 |
| N–C, from N | 1.3406 | 0.9022 | 0.8037 | 1.123 |
| N–H1, from N | 1.0383 | 0.8119 | 0.7854 | 1.034 |
| N–H3, from N | 1.0261 | 0.7911 | 0.7785 | 1.016 |

The C–N boundary moves about 0.10 Å toward N; the others by 2–3%. STO-3G is similar
(C–N 14%).

### Urea HAR, def2-SVP: Hirshfeld, Salvador and `tfvp` side by side

`tests/long/urea_rhf_STO-3G_HAR` with the basis set to def2-SVP and only
`partition_model=` changed. All three converge.

| | Hirshfeld | Salvador | `tfvp` |
|---|---|---|---|
| R(F) | 0.0181 | 0.0190 | 0.0188 |
| GoF | 3.30 | 3.54 | 3.48 |
| cycles | 5 | 8 | 7 |
| O=C /Å | 1.2558(4) | 1.2557(4) | 1.2555(4) |
| N–C /Å | 1.3413(3) | 1.3406(3) | 1.3408(3) |
| N–H1 /Å | 1.028(5) | 1.038(5) | 1.036(4) |
| N–H3 /Å | 0.986(6) | 1.026(5) | 1.023(4) |
| U_iso O /Å² | 0.01512(7) | 0.01517(8) | 0.01516(8) |
| U_iso N /Å² | 0.02218(8) | 0.02214(9) | 0.02212(9) |
| U_iso C /Å² | 0.01213(7) | 0.01219(8) | 0.01217(8) |
| U_iso H1 /Å² | 0.054(4) | 0.050(4) | 0.048(3) |
| U_iso H3 /Å² | 0.048(3) | 0.042(2) | 0.041(2) |

In STO-3G: R 0.0379, 0.0379, 0.0381; GoF 7.04 for all three.

What the table says:

- `tfvp` reproduces the Salvador refinement closely: every heavy-atom parameter within
  one esd, the N–H bonds within 0.003 Å, and a slightly better fit (GoF 3.48 against
  3.54), although the C–N boundary moved 0.1 Å.
- Both Salvador variants give N–H bonds 0.01–0.04 Å longer than Hirshfeld, and N–H3
  most of all (1.023–1.026 against 0.986 Å). Hirshfeld fits best here.
- The heavy-atom ADPs hardly depend on the model; the hydrogen ADPs do.

### Open

- A reference for the N–H bonds (neutron data for urea) to say which model is right.
- A larger molecule, and one without hydrogen-bond donors.
- The test `long/urea_rhf_STO-3G_TFVP_HAR`, to be blessed on the Linux reference host.

# 3. Spherically averaged, with promolecule radii (`sph-tfvp`)

`sph-tfva`'s form factors with `tfvp`'s radii. Expected, like `sph-tfva`, not to
converge. **It converges**, in both bases:

| urea | `sph-tfva` | `sph-tfvp` | `tfvp` (aspherical) |
|---|---|---|---|
| STO-3G | H1 runs away, R = 1 | R 0.0455, GoF 10.5, 4 cycles | R 0.0381, GoF 7.04 |
| def2-SVP | never converges; R 0.044, GoF 11.6 | R 0.0341, GoF 8.16, 6 cycles | R 0.0188, GoF 3.48 |

def2-SVP, `sph-tfvp` against `tfvp`: O=C 1.2545(10) against 1.2555(4) Å; N–C 1.3351(8)
against 1.3408(3); N–H1 1.069(12) against 1.036(4); N–H3 0.981(15) against 1.023(4).
U_iso O 0.01479(18), N 0.0217(2), C 0.01219(19), H1 0.060(9), H3 0.059(7) Å².

So the promolecule radii are enough to make the spherical model stable, though it still
fits twice as badly as the aspherical ones, its esds are two to three times larger, and
its N–H bonds scatter by ±0.04 Å about the aspherical values. Why the promolecule radii
stabilise it is not known: they move the C–N boundary 0.1 Å toward N, which gives the
spherical C more of the bond density; that is a guess, not measured.

### Against the independent atom model (IAM)

`IAM_refinement` on the same urea data. Tonto's IAM uses International Tables form factors
for the neutral heavy atoms and Stewart–Davidson–Simpson bonded-atom form factors for H.

| model | N–H1 /Å | N–H3 /Å | R(F) | GoF |
|---|---|---|---|---|
| IAM (SDS H) | 0.905(10) | 0.888(12) | 0.0284 | 6.50 |
| `sph-tfvp`, STO-3G | 1.048(15) | 0.944(15) | 0.0455 | 10.5 |
| `sph-tfvp`, def2-SVP | 1.069(12) | 0.981(15) | 0.0341 | 8.16 |
| Hirshfeld, def2-SVP | 1.028(5) | 0.986(6) | 0.0181 | 3.30 |
| Salvador, def2-SVP | 1.038(5) | 1.026(5) | 0.0190 | 3.54 |
| `tfvp`, def2-SVP | 1.036(4) | 1.023(4) | 0.0188 | 3.48 |

IAM: O=C 1.2583(8), N–C 1.3386(7) Å; U_iso O 0.01565(15), N 0.02322(17), C 0.01244(15),
H1 0.055(6), H3 0.044(5) Å².

- IAM shortens N–H by about 0.1 Å, as it always does. `sph-tfvp` does not: its N–H bonds
  are near the aspherical models', with esds two to three times larger and more scatter.
  A spherical average of the *molecular* density about the H nucleus already includes the
  density drawn into the bond; a free-atom H does not.
- IAM nonetheless fits better than `sph-tfvp` (R 0.028 against 0.034), and better than the
  STO-3G HARs: it absorbs the bonding density by moving the H atoms and adjusting the U, which
  is how it gets the bonds wrong. A lower R is not a better geometry here.
- To say which N–H is right needs the neutron values for urea (thought to be about
  1.00–1.01 Å; to be checked against the published structure).
