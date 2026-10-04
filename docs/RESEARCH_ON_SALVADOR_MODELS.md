# Research: variants of the Salvador atom model

Variants of the Salvador (TFVA) partition, measured on urea: `sph-tfva`, the
spherically averaged Salvador atom; `tfvp`, the Salvador atom with its radii taken
from the promolecule; `sph-tfvp`, both at once; and `tfvh`, the Salvador atom with
each boundary where the two atoms' spherical densities are equal; and `sph-tfvh`, the
spherical atom with those radii. Sections 1–5 are urea; section 6 is YLID, a crystal with
C–H bonds only; section 7 is `sph-tfvh`.

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
- Related published work: Chodkiewicz & Woźniak, "Towards improved accuracy of Hirshfeld
  atom refinement with an alternative electron density partition", IUCrJ (2025) -- an
  "exponential Hirshfeld" partition with an exponent n that reduces atomic overlap (n = 1 is
  Hirshfeld). Read before going further with partition variants.

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

# 4. All models against the neutron structure

Urea, 123 K. The X-ray data appear to be Birkedal et al.'s 123 K synchrotron set
(Acta Cryst. A60, 371, 2004; sin θ/λ to 1.44 Å⁻¹). The neutron reference is
Swaminathan, Craven & McMullan, Acta Cryst. B40, 300 (1984), at 123 K, as tabulated by
Wall, IUCrJ 3, 237 (2016), Table 4. In our files H1 is the hydrogen on the O side (cis);
that it matches the neutron H1 has not been checked -- the two neutron values differ by
only 0.006 Å, so the comparison does not depend on it.

| model | basis | N–H1 /Å | N–H3 /Å | O=C /Å | R(F) | GoF |
|---|---|---|---|---|---|---|
| **neutron, 123 K** | | **1.006** | **1.000** | **1.257** | | |
| IAM (International Tables; SDS H) | -- | 0.905(10) | 0.888(12) | 1.2583(8) | 0.0284 | 6.50 |
| Hirshfeld | def2-TZVP | 1.025(4) | 0.989(5) | 1.2560(4) | 0.0167 | 2.94 |
| Salvador | def2-TZVP | 1.033(4) | 1.017(4) | 1.2560(4) | 0.0170 | 3.10 |
| `tfvp` | def2-TZVP | 1.032(4) | 1.017(4) | 1.2558(4) | 0.0170 | 3.07 |
| `tfvh` | def2-TZVP | 1.029(3) | 1.013(3) | 1.2559(4) | 0.0169 | 3.06 |
| `sph-tfvh` | def2-TZVP | 1.022(8) | 0.993(9) | 1.2570(7) | 0.0294 | 5.65 |
| `sph-tfvp` | def2-TZVP | 1.045(10) | 0.977(12) | 1.2553(9) | 0.0316 | 7.06 |
| `sph-tfva` | def2-TZVP | 1.11(2) | 0.954(19) | 1.2518(14) | 0.0422 | 11.0 (no convergence) |
| Hirshfeld | def2-SVP | 1.028(5) | 0.986(6) | 1.2558(4) | 0.0181 | 3.30 |
| Salvador | def2-SVP | 1.038(5) | 1.026(5) | 1.2557(4) | 0.0190 | 3.54 |
| `tfvp` | def2-SVP | 1.036(4) | 1.023(4) | 1.2555(4) | 0.0188 | 3.48 |
| `tfvh` | def2-SVP | 1.034(4) | 1.018(4) | 1.2556(4) | 0.0186 | 3.46 |
| `sph-tfvh` | def2-SVP | 1.033(9) | 1.001(10) | 1.2570(7) | 0.0297 | 5.87 |
| `sph-tfvp` | def2-SVP | 1.069(12) | 0.981(15) | 1.2545(10) | 0.0341 | 8.16 |
| `sph-tfvh` | STO-3G | 1.050(12) | 0.994(13) | 1.2571(11) | 0.0447 | 8.52 |
| `sph-tfvp` | STO-3G | 1.048(15) | 0.944(15) | 1.2542(13) | 0.0455 | 10.5 |
| `sph-tfva` | def2-SVP | does not converge (flip-flops) | | | 0.044 | 11.6 |
| `sph-tfva` | STO-3G | does not converge (H1 runs away) | | | 1.0 | -- |

Differences from the neutron values, N–H1 / N–H3, in Å:

| model | N–H1 | N–H3 |
|---|---|---|
| IAM | −0.101 | −0.112 |
| Hirshfeld, def2-TZVP | +0.019 | −0.011 |
| Salvador, def2-TZVP | +0.027 | +0.017 |
| `tfvp`, def2-TZVP | +0.026 | +0.017 |
| `tfvh`, def2-TZVP | +0.023 | +0.013 |
| `sph-tfvh`, def2-TZVP | +0.016 | −0.007 |
| `sph-tfvp`, def2-TZVP | +0.039 | −0.023 |
| Hirshfeld, def2-SVP | +0.022 | −0.014 |
| Salvador, def2-SVP | +0.032 | +0.026 |
| `tfvp`, def2-SVP | +0.030 | +0.023 |
| `tfvh`, def2-SVP | +0.028 | +0.018 |
| `sph-tfvh`, def2-SVP | +0.027 | +0.001 |
| `sph-tfvp`, def2-SVP | +0.063 | −0.019 |

- Hirshfeld is closest (def2-TZVP within 0.019 Å); the Salvador variants are 0.02–0.03 Å
  long, `tfvh` the least so of them (0.023 and 0.013 Å); `sph-tfvp` is within 0.04–0.06 Å but uneven; IAM is 0.1 Å short.
- def2-SVP -> def2-TZVP moves every aspherical model a few mÅ toward the neutron values and
  lowers R by about 0.002. `sph-tfva` still does not converge in def2-TZVP (stopped after 100
  cycles, 9.6 min against about 1 min for the others).
- O=C is close in every model but not within the X-ray esds: against 1.257 Å, Hirshfeld
  1.2558(4) is 3 esds short, `sph-tfvp` 1.2545(10) 2.5 short, IAM 1.2583(8) 1.6 long.
  `sph-tfvh` gives 1.2570(7) at both bases, the one model on the neutron value.
- **`sph-tfvh` is the spherical model that works** (§7): closest of all models to the
  neutron N–H3 at def2-TZVP, within 0.016 Å on N–H1, R(F) 0.0294 against the IAM's 0.0284,
  where `sph-tfvp` and `sph-tfva` are far worse or do not converge.
- **The neutron values are quoted without esds** (Wall 2016 gives none), and it is not known
  here whether they are the raw values or those corrected for thermal motion, which the
  original paper also gives. Both are in Swaminathan, Craven & McMullan (1984), not
  consulted. The X-ray values here are uncorrected.
- These are small basis sets and an isolated-molecule wavefunction with cluster charges;
  published HAR on these data reaches a few mÅ with larger bases. The comparison between
  models at the same basis is the point here, not the absolute values.

# 5. Salvador atoms with equal-density radii (`tfvh`)

### The model

`tfvh` (topological fuzzy Voronoi Hirshfeld) is the Salvador model with a different
rule for the boundary on each bonded pair A–B. Instead of the density minimum, the
boundary is the point on the A–B line where the two atoms' spherical ANO densities are
equal, ρ_A(r) = ρ_B(r). At that point the **pairwise** Hirshfeld weight
ρ_A/(ρ_A + ρ_B) is exactly ½, so the Salvador cell function and the pair's Hirshfeld
weight agree on where the two atoms meet. As in `tfvp`, the radii depend only on the
geometry and the free atoms, not on the wavefunction; the atom is still the molecular
density times the cell function.

Only the pair form makes sense. The point where the *full-promolecule* Hirshfeld weight
of A reaches ½ exists only on bonds where no third atom contributes; on most bonds the
half-weight surfaces of A and B do not touch, so there is no boundary to find. The
pair form always has exactly one root, because ρ_A falls and ρ_B rises monotonically
along the line.

Code: `MOLECULE.RHO:make_Salvador_radii_hirshfeld` and `Hirshfeld_half_radius_for`
(a bisection on the A–B line, 40 steps); the radii table in a `Salvador_properties`
job gains an `R(A.B) eq-dens` column. Keyword `partition_model= tfvh` (or `oc-tfvh`);
branch `tfvh`; test `long/urea_rhf_STO-3G_TFVH_HAR`.

### Where the equal-density point falls

Urea, from `short/urea_ccsd_pob-TZVP_Salvador_properties` (CCSD density, pob-TZVP,
the CIF geometry). R(A.B) is the distance from A to the boundary with B.

| bond | R(A–B) /Å | R(A.B) molecule /Å | R(A.B) promolecule /Å | R(A.B) equal density /Å |
|---|---|---|---|---|
| C–O, from C | 1.2537 | 0.4375 | 0.4616 | 0.5674 |
| C–N, from C | 1.3400 | 0.4985 | 0.5679 | 0.6346 |
| N–H1, from N | 1.0077 | 0.8576 | 0.8268 | 0.7398 |
| N–H2, from N | 0.9930 | 0.8421 | 0.8193 | 0.7340 |

The equal-density point sits well away from the density minimum, and in a consistent
direction: toward the more electronegative atom for C–O and C–N (the C atom grows by
0.13 and 0.14 Å), and toward N for N–H (the H atom grows from 0.15 Å of the bond at
the minimum to 0.27 Å). So `tfvh` hydrogens are the largest of the three Salvador
variants, and `tfvh` carbon the largest carbon.

### Urea HAR, def2-SVP and def2-TZVP

Same jobs as §2 (`tests/long/urea_rhf_STO-3G_HAR` with the basis changed and only
`partition_model=` varied). All converge.

| | Hirshfeld | Salvador | `tfvp` | `tfvh` |
|---|---|---|---|---|
| **def2-SVP** | | | | |
| R(F) | 0.0181 | 0.0190 | 0.0188 | 0.0186 |
| GoF | 3.30 | 3.54 | 3.48 | 3.46 |
| cycles | 5 | 8 | 7 | 7 |
| O=C /Å | 1.2558(4) | 1.2557(4) | 1.2555(4) | 1.2556(4) |
| N–C /Å | 1.3413(3) | 1.3406(3) | 1.3408(3) | 1.3410(3) |
| N–H1 /Å | 1.028(5) | 1.038(5) | 1.036(4) | 1.034(4) |
| N–H3 /Å | 0.986(6) | 1.026(5) | 1.023(4) | 1.018(4) |
| U_iso O /Å² | 0.01512(7) | 0.01517(8) | 0.01516(8) | 0.01515(7) |
| U_iso N /Å² | 0.02218(8) | 0.02214(9) | 0.02212(9) | 0.02211(9) |
| U_iso C /Å² | 0.01213(7) | 0.01219(8) | 0.01217(8) | 0.01216(8) |
| U_iso H1 /Å² | 0.054(4) | 0.050(4) | 0.048(3) | 0.046(3) |
| U_iso H3 /Å² | 0.048(3) | 0.042(2) | 0.041(2) | 0.040(2) |
| **def2-TZVP** | | | | |
| R(F) | 0.0167 | 0.0170 | 0.0170 | 0.0169 |
| GoF | 2.94 | 3.10 | 3.07 | 3.06 |
| cycles | 5 | 8 | 7 | 8 |
| O=C /Å | 1.2560(4) | 1.2560(4) | 1.2558(4) | 1.2559(4) |
| N–C /Å | 1.3413(3) | 1.3405(3) | 1.3408(3) | 1.3409(3) |
| N–H1 /Å | 1.025(4) | 1.033(4) | 1.032(4) | 1.029(3) |
| N–H3 /Å | 0.989(5) | 1.017(4) | 1.017(4) | 1.013(3) |
| U_iso O /Å² | 0.01513(6) | 0.01516(7) | 0.01516(7) | 0.01515(7) |
| U_iso N /Å² | 0.02227(7) | 0.02224(8) | 0.02222(8) | 0.02221(8) |
| U_iso C /Å² | 0.01210(7) | 0.01215(7) | 0.01213(7) | 0.01212(7) |
| U_iso H1 /Å² | 0.054(3) | 0.049(3) | 0.048(3) | 0.046(3) |
| U_iso H3 /Å² | 0.046(2) | 0.0402(19) | 0.0400(19) | 0.0392(18) |
| time /s | 53 | 77 | 73 | 77 |

What the table says:

- The three Salvador variants form a sequence, Salvador → `tfvp` → `tfvh`, in which the
  N–H bonds shorten (def2-TZVP: N–H3 1.017 → 1.017 → 1.013 Å) and the fit improves a
  little (GoF 3.10 → 3.07 → 3.06), and every step moves toward Hirshfeld. `tfvh` is the
  closest of the three to the neutron bonds (+0.023 and +0.013 Å at def2-TZVP, §4) and
  has the smallest hydrogen esds (0.003 Å).
- Hirshfeld still fits best (GoF 2.94) and is still closest to neutron for N–H1, but its
  N–H3 is 0.011 Å *short* where every Salvador variant is long. Averaged over the two
  bonds, `tfvh` (+0.018) and Hirshfeld (|0.015|) are about equally far from neutron.
- The heavy-atom bonds and ADPs barely depend on the model: within 0.0008 Å and one or
  two esds across the four columns.
- The hydrogen U_iso falls along the same sequence, 0.054 → 0.050 → 0.048 → 0.046 Å² for
  H1 at def2-SVP: a larger hydrogen atom in the partition means less of the bond density
  is modelled as hydrogen motion.
- The cost is that of a Salvador refinement (about 1.5 times Hirshfeld on this job);
  the bisection for the radii is negligible.

### Charges by partition, and their stability

Urea, the CCSD/pob-TZVP density of `short/urea_ccsd_pob-TZVP_Salvador_properties`, with the
partition model set by `crystal= { xray_data= { partition_model= ... } }` (no data needed).
Charges include the nucleus.

| atom | Hirshfeld | Salvador | `tfvp` | `tfvh` |
|---|---|---|---|---|
| C | +0.198 | +2.167 | +1.695 | +0.908 |
| O | −0.396 | −1.189 | −1.118 | −0.740 |
| N | −0.137 | −1.804 | −1.469 | −0.801 |
| H1 (cis) | +0.127 | +0.672 | +0.596 | +0.364 |
| H2 | +0.109 | +0.643 | +0.585 | +0.353 |

The Salvador atoms carry charges of the size the QTAIM atoms do (the density minimum is close to
the zero-flux surface along the bond), and `tfvh` sits about half-way between Salvador and
Hirshfeld, which is what its larger C and H atoms imply.

Every number in the table, and every dipole and quadrupole of all four partitions, is the same
to the four printed decimals under the `neoversen1` and `armv8` OpenBLAS kernels on the Mac.
The Hirshfeld moments used to move by 3–4e-4 between kernels; that went with the exact
spherical average of the ANO atoms, so none of the partitions is more stable than another
any more.

### Open

- The same comparison on a molecule without hydrogen-bond donors, where the N–H
  disagreement between Hirshfeld and the Salvador family should disappear if it is a
  partition effect and not a model-of-the-crystal effect.
- Whether the equal-density boundary should use the *pair* of spherical atoms, as here,
  or the pair after the promolecule's charge transfer -- which would move the C–O and
  C–N boundaries back toward C.
- Blessing `long/urea_rhf_STO-3G_TFVH_HAR` and the two `Salvador_properties` tests
  (new column) on the Linux reference host.

# 6. YLID: the four models on a crystal with no hydrogen-bond donors

The question from §5: is the N–H disagreement between Hirshfeld and the Salvador family a
partition effect, or does it come from the hydrogen-bonded environment? YLID
(2-dimethylsulfuranylidene-1,3-indanedione, C₁₁H₁₀O₂S, 24 atoms) has ten C–H bonds — six
methyl, four aromatic — and no N–H or O–H.

The job is `long/YLID_IAM_plus_anomalous_residual_density` made into a HAR: the same CIF,
Cu Kα F² data, `f_sigma_cutoff= 4`, `refine_H_U_iso= YES` (hydrogen isotropic), no
dispersion correction and no anharmonic sulfur, with the `scfdata=` block and
`HAR_refinement` of the urea job and only `partition_model=` varied. RHF, no cluster
charges. The IAM on the same data gives R(F) 0.0275, GoF 8.54.

### def2-SVP

| | Hirshfeld | Salvador | `tfvp` | `tfvh` |
|---|---|---|---|---|
| R(F) | 0.0231 | 0.0232 | 0.0233 | 0.0233 |
| GoF | 7.250 | 7.284 | 7.287 | 7.284 |
| cycles | 4 | 6 | 6 | 6 |
| time /min | 7.4 | 11.9 | 12.4 | 12.1 |
| S1–C8 /Å | 1.7096(15) | 1.7098(15) | 1.7098(15) | 1.7098(15) |
| O1–C1 /Å | 1.2293(17) | 1.2279(16) | 1.2281(16) | 1.2290(17) |
| C4–H5 (arom.) | 1.09(2) | 1.088(14) | 1.095(15) | 1.097(16) |
| C5–H7 (arom.) | 1.11(2) | 1.102(15) | 1.113(15) | 1.117(16) |
| C6–H4 (arom.) | 1.08(2) | 1.071(14) | 1.083(15) | 1.088(15) |
| C11–H9 (arom.) | 1.09(2) | 1.086(12) | 1.101(13) | 1.105(14) |
| C9–H1 (methyl) | 1.08(2) | 1.072(15) | 1.074(16) | 1.075(16) |
| C9–H3 | 1.08(2) | 1.083(15) | 1.097(16) | 1.102(16) |
| C9–H10 | 1.09(2) | 1.085(14) | 1.094(15) | 1.097(15) |
| C10–H2 | 1.08(2) | 1.069(14) | 1.078(15) | 1.081(15) |
| C10–H6 | 1.10(3) | 1.096(17) | 1.103(17) | 1.108(18) |
| C10–H8 | 1.11(2) | 1.106(13) | 1.112(14) | 1.113(14) |
| mean C–H /Å | 1.091 | 1.086 | 1.095 | 1.098 |
| mean H esd /Å | 0.021 | 0.014 | 0.015 | 0.016 |
| U_iso H, range /Å² | 0.037–0.056 | 0.033–0.049 | 0.033–0.049 | 0.034–0.050 |
| mean U_iso H esd /Å² | 0.006 | 0.004 | 0.004 | 0.004 |

Neutron averages for comparison (Allen & Bruno, Acta Cryst. B66, 380, 2010; quoted from memory, check): aromatic C–H
1.083, methyl C–H 1.077 Å.

What the table says:

- **The N–H effect does not appear in C–H.** In urea every Salvador variant gave N–H
  0.01–0.04 Å *longer* than Hirshfeld. Here the Salvador mean C–H is 0.005 Å *shorter* than
  Hirshfeld's, `tfvh` is 0.007 Å longer, and every bond agrees between the models within one
  esd. So the urea disagreement belongs to the polar, hydrogen-bonded N–H, not to the
  partitions as such.
- All four models are 0.01–0.02 Å long against the neutron means, with esds of 0.015–0.02 Å:
  Cu Kα data to sin θ/λ ≈ 0.6 Å⁻¹ do not place these hydrogens well, and the models cannot be
  ranked on them.
- The fits are indistinguishable: R(F) within 0.0002, GoF within 0.04. Hirshfeld converges in
  4 cycles against 6, and in 60% of the time.
- As in urea, the Salvador family gives hydrogen esds about 30% smaller than Hirshfeld's,
  for both position and U_iso, and U_iso values about 15% smaller. The heavy atoms are
  identical.

### def2-TZVP

| | Hirshfeld | Salvador | `tfvp` | `tfvh` |
|---|---|---|---|---|
| R(F) | 0.0228 | 0.0229 | 0.0229 | 0.0229 |
| GoF | 7.185 | 7.232 | 7.230 | 7.226 |
| cycles | 4 | 7 | 6 | 6 |
| time /min | 36 | 51 | 47 | 45 |
| S1–C8 /Å | 1.7102(15) | 1.7104(15) | 1.7104(15) | 1.7104(15) |
| O1–C1 /Å | 1.2298(17) | 1.2284(16) | 1.2287(16) | 1.2295(16) |
| C4–H5 (arom.) | 1.09(2) | 1.088(15) | 1.093(15) | 1.094(15) |
| C5–H7 (arom.) | 1.11(2) | 1.109(15) | 1.116(15) | 1.117(16) |
| C6–H4 (arom.) | 1.08(2) | 1.078(14) | 1.087(15) | 1.090(15) |
| C11–H9 (arom.) | 1.09(2) | 1.092(13) | 1.101(13) | 1.103(14) |
| C9–H1 (methyl) | 1.08(2) | 1.071(15) | 1.073(16) | 1.074(16) |
| C9–H3 | 1.08(2) | 1.087(15) | 1.097(16) | 1.100(16) |
| C9–H10 | 1.09(2) | 1.089(14) | 1.095(15) | 1.097(15) |
| C10–H2 | 1.08(2) | 1.072(14) | 1.078(15) | 1.079(15) |
| C10–H6 | 1.10(3) | 1.098(17) | 1.104(18) | 1.107(18) |
| C10–H8 | 1.11(2) | 1.103(13) | 1.108(14) | 1.109(14) |
| mean C–H /Å | 1.091 | 1.089 | 1.095 | 1.097 |
| mean H esd /Å | 0.021 | 0.015 | 0.015 | 0.015 |
| U_iso H, range /Å² | 0.040–0.057 | 0.035–0.050 | 0.035–0.050 | 0.036–0.051 |
| mean U_iso H esd /Å² | 0.006 | 0.004 | 0.004 | 0.004 |

The larger basis changes nothing in the comparison: the four models agree with each other
as they did at def2-SVP, the Salvador mean C–H moves 0.003 Å toward Hirshfeld's, and
R(F) falls by 0.0003 for all four.

The times are for plain RHF with exact integrals (no RI-J, no COSX), eight jobs sharing one
Mac, and OpenBLAS left free to use several threads; they compare with each other and not
with anything else. About 12 of the minutes of every def2-TZVP job went on making the
ANO data for sulfur, before the SCF began -- the ANO atomic SCF for a third-row atom in a
triple-zeta basis is slow and worth a look.

# 7. Spherically averaged, with equal-density radii (`sph-tfvh`)

### The model

`sph-tfvh` is the spherical average of the `tfvh` atom (§5): the Salvador cell function
with each pair boundary where the two atoms' spherical densities are equal, then averaged
over angles as in `sph-tfva` (§1). `partition_model= sph-tfvh` (or `oc-sph-tfvh`); branch
`sph-tfvh`, one dispatch case at each site that has `oc-sph-tfvp`.

### Urea

The same job as §1–§5 (`tests/long/urea_rhf_STO-3G_HAR`, basis and `partition_model=`
changed). Every run converges, in 4–5 cycles.

| urea | STO-3G | def2-SVP | def2-TZVP |
|---|---|---|---|
| R(F) | 0.0447 | 0.0297 | 0.0294 |
| GoF | 8.52 | 5.87 | 5.65 |
| cycles | 4 | 5 | 4 |
| O=C /Å | 1.2571(11) | 1.2570(7) | 1.2570(7) |
| N–H1 /Å | 1.050(12) | 1.033(9) | 1.022(8) |
| N–H3 /Å | 0.994(13) | 1.001(10) | 0.993(9) |
| U_iso H1 /Å² | 0.063(9) | 0.046(5) | 0.047(5) |
| U_iso H3 /Å² | 0.064(7) | 0.050(5) | 0.050(4) |
| time /s | 1.5 | 7 | 50 |

Against the other spherical models at def2-SVP: `sph-tfva` never converges (R 0.044,
GoF 11.6), `sph-tfvp` R 0.0341, GoF 8.16, N–H1 1.069(12), N–H3 0.981(15). Against the
neutron values (1.006, 1.000, 1.257): `sph-tfvh` at def2-TZVP is +0.016, −0.007 and 0.000 Å.

What the numbers say:

- The spherical atom fails (§1, §3) because C, N and O lose their bonding density and
  the fit rebuilds it by moving the hydrogens. `sph-tfvh` fails least because its carbon
  and hydrogens are the largest of the family (§5): more of the bond density is inside
  the atom that is being averaged, so less is lost in the averaging. The sequence
  `sph-tfva` → `sph-tfvp` → `sph-tfvh` is the sequence of growing C and H atoms, and GoF
  falls 11.6 → 8.2 → 5.9.
- It is still a spherical model: R(F) 0.0294 against 0.0169 for the aspherical `tfvh`,
  and GoF 5.65 against 3.06, and the hydrogen esds are twice as large. The IAM gives
  R 0.0284, GoF 6.50 with N–H 0.1 Å short: `sph-tfvh` fits as well as the IAM and puts
  the hydrogens where the neutrons do.
- This is the model whose form factors the Gaussian fit of
  `docs/TONTO_SPHERICAL_FF_FIT_PLAN.md` is for.

### Open

- A test: `long/urea_rhf_STO-3G_sph-TFVH_HAR`, to be blessed on achari2, once the model is
  judged worth keeping (it is the first spherical model that is).
- YLID and a larger molecule with `sph-tfvh`.
- Whether hydrogen should stay aspherical (§1 found spherical H harmless) — not needed now
  that the all-spherical model converges.
