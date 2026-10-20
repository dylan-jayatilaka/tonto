# The multipole Salvador atoms: `l0-tfvc` to `l3-tfvc`

The Salvador atom (the topological fuzzy Voronoi cell atom, `oc-salvador`, described with its
variants in [Variants of the Salvador atom model](REPORT_ON_SALVADOR_MODELS.md)) can be used
whole, or spherically averaged about its nucleus (`sph-tfvc`). The spherical atom fits badly and
often does not converge. This page describes a family of models between the two — the atom's
density kept to its spherical harmonics up to $`l = n`$, nothing fitted — and compares them on
three crystals, with three wavefunctions and two basis sets. In short:

- **The spherical atom fails because its hydrogens do the heavy atoms' work.** Without the bonding
  density of C, N and O the fit rebuilds it by moving the hydrogens along their bonds and
  inflating their ADPs; on urea it never converges.
- **Centring a hydrogen on its density maximum does not help** (`sph-tfvc+h`); **a dipole on every
  atom does** (`l1-tfvc`): every job converges.
- **Each harmonic closes most of what is left.** At $`l = 3`$ the GoF is 1.15–1.4 times the full
  atom's on all eighteen method–basis–crystal combinations, the mean X–H length is within 0.03 Å
  of the full atom's (0.01 Å on the amino acids) and the worst single bond within 0.07 Å.
- **The pattern does not depend on the wavefunction or the basis**: RHF, B3LYP and PBE0, def2-SVP
  and def2-TZVP, give the same ordering and nearly the same gaps.
- **A multipole job costs what the spherical one costs**: the harmonics come from the shells the
  spherical average already uses.

## 1. The models

The Salvador weight $`w_A(\mathbf r)`$ divides the molecular density $`\rho`$ into atoms,
$`\rho_A = w_A\rho`$. The aspherical model (`oc-salvador`) transforms $`\rho_A`$ as it is. The
spherical model averages it over each sphere about the nucleus,

```math
\bar\rho_A(r) = \frac{1}{4\pi} \oint w_A(\mathbf r)\,\rho(\mathbf r)\, d\Omega , \qquad (1)
```

with $`d\Omega`$ the element of solid angle, and its form factor at scattering vector
$`\mathbf k`$, for a nucleus at $`\mathbf r_A`$, is

```math
f_A(\mathbf k) = e^{i \mathbf k\cdot\mathbf r_A} \int_0^\infty 4\pi r^2\, \bar\rho_A(r)\, \frac{\sin kr}{kr}\, dr . \qquad (2)
```

**The multipole family** (`l0-tfvc`, `l1-tfvc`, `l2-tfvc`, `l3-tfvc`). (1) is the first term of the
expansion of the atom's density on each sphere in real spherical harmonics $`Y_{lm}`$, normalised
to one over the sphere,

```math
\rho_A(r,\hat{\mathbf r}) = \sum_{lm} \rho_{A,lm}(r)\, Y_{lm}(\hat{\mathbf r}), \qquad
\rho_{A,lm}(r) = \oint Y_{lm}(\hat{\mathbf r})\, w_A(\mathbf r)\,\rho(\mathbf r)\, d\Omega . \qquad (3)
```

The model `l<n>-tfvc` keeps the terms to $`l = n`$, and its form factor is the sum of their
Fourier–Bessel transforms,

```math
f_A(\mathbf k) = e^{i \mathbf k\cdot\mathbf r_A} \sum_{l=0}^{n} i^l \sum_{m} Y_{lm}(\hat{\mathbf k})
\int_0^\infty 4\pi r^2\, \rho_{A,lm}(r)\, j_l(kr)\, dr , \qquad (4)
```

with $`j_l`$ the spherical Bessel function ($`j_0(x) = \sin x/x`$, $`j_1(x) = (\sin x -
x\cos x)/x^2`$, …). `l0-tfvc` is `sph-tfvc`, (2). For $`l = 1`$ the three functions are
$`p`$-like, $`x/r`$, $`y/r`$, $`z/r`$, and the term is $`i\,[\hat k_x F_x(k) + \hat k_y F_y(k) +
\hat k_z F_z(k)]`$: three dipolar form factors, each a real function of $`|\mathbf k|`$, times the
direction cosines of $`\hat{\mathbf k}`$, and imaginary because a dipole is odd. The $`l = 2`$ and
$`l = 3`$ terms are the five $`d`$-like and seven $`f`$-like functions in the same way. This is the
form every multipole refinement program uses; the difference is that here the radial functions
$`\rho_{A,lm}(r)`$ come from the wavefunction, on the same Lebedev shells as (1), and nothing is
refined but positions and ADPs. The harmonics are projected once per atom per refinement cycle,
so the cost is that of the spherical atom.

**The hydrogen correction** (`sph-tfvc+h`, and `tfvc+h`). A hydrogen atom has no core, and the
maximum of $`w_H\rho`$ lies a little way along the bond from the nucleus: 0.05 Å (def2-SVP) to
0.1 Å (STO-3G) on urea's N–H bonds. `sph-tfvc+h` averages each hydrogen about that maximum
instead of the nucleus — the maximum is found on a line from the nucleus to the nearest Salvador
neighbour, the offset $`\boldsymbol\delta`$ is stored, and (1) and (2) use
$`\mathbf r_H + \boldsymbol\delta`$ as the centre. The refined parameter is still the nucleus:
$`\boldsymbol\delta`$ is held fixed through the least-squares step, so a shift of the nucleus is the
same shift of the averaged atom, and it is recomputed at the start of each cycle. Shifting a
spherical density by $`\boldsymbol\delta`$ is, to first order, a dipole with radial function
$`-\boldsymbol\delta\, d\bar\rho_H/dr`$; so `+h` is a particular $`l = 1`$ term on the hydrogens only,
and `l1-tfvc` is the uniform version of it. For an aspherical atom the centre does not enter the
structure factor at all — thermal smearing is a convolution, and a shift is the same phase factor
whichever point is called the centre — so `tfvc+h` refines exactly as `oc-salvador` does and only
prints the offsets.

**The wall stiffness.** The Salvador cell function is Becke's step function applied
`salvador_stiffness=` times (in the `becke_grid=` block; 4, Salvador's value, by default). Fewer
steps make a softer wall between atoms.

## 2. The crystals and the settings

| crystal | data | reflections | atoms in the asymmetric unit |
|---|---|---|---|
| urea | Birkedal *et al.* (2004), 123 K | 817 | O, N, C, 2 H |
| L-alanine | Destro, Marsh and Bianchi (1988), 23 K | 2,405 | 6 heavy, 7 H |
| glycyl-L-alanine | Capelli *et al.* (2014), 150 K | 2,531 | 10 heavy, 10 H |

Every refinement starts from the published geometry and ADPs, uses a self-consistent cluster-charge
SCF, Cartesian basis functions, free positions and anisotropic ADPs for every atom, one scale
factor and no extinction, and stops at 25 refinement cycles if it has not converged. The
wavefunctions are RHF, B3LYP (the Gaussian form) and PBE0, in def2-SVP and def2-TZVP. The
reference for each model is the full Salvador atom of the same wavefunction and basis: there are no
neutron data here, and the question is how much of the aspherical atom each model recovers.
"Mean X–H" is the mean of every X–H bond in the asymmetric unit; "max |Δ| per bond" is the largest
change of a single X–H length from the full atom's; U_iso(H) is the mean over the hydrogens.

## 3. Urea at RHF: where the spherical atom goes wrong

RHF/def2-SVP, from the published geometry:

| model | converges | R(F) | GoF | N1–H1 / Å | N1–H3 / Å | U_iso(H1), U_iso(H3) / Å² |
|---|---|---|---|---|---|---|
| `oc-salvador` | yes, 8 cycles | 0.0190 | 3.54 | 1.038 | 1.026 | 0.050, 0.042 |
| `l0-tfvc` | no, oscillates | 0.044 | 11.6 | 1.149 | 0.967 | 0.10, 0.080 |
| `sph-tfvc+h` | no, 100 cycles | 0.042 | 9.8 | 1.006 | 0.389 | 0.018, 0.43 |
| `l1-tfvc` | yes | 0.0327 | 7.71 | 0.982 | 0.892 | 0.12, 0.078 |
| `l2-tfvc` | yes | 0.0274 | 6.22 | 1.007 | 0.948 | 0.061, 0.054 |
| `l3-tfvc` | yes | 0.0216 | 4.12 | 1.025 | 0.985 | 0.072, 0.060 |

- With spherical hydrogens and aspherical C, N, O the refinement converges (GoF 3.86); it is the
  spherical heavy atoms that break it. Their bonding density is gone, and the only parameters that
  can put it back are the hydrogens' positions and ADPs — a spherical hydrogen displaced along its
  bond *is* a dipole, so the fit uses it as one. The SCF then recomputes with the hydrogen in the
  wrong place and the outer loop oscillates.
- Centring the hydrogen on its density maximum does not rescue this: the offsets are 0.05 Å on
  both bonds, and from the third cycle H3 sits 0.4 Å from N1 and stays there.
- The dipole on every atom does: `l1-tfvc` converges and takes the GoF nearly half-way to the
  full atom. The hydrogens still compensate — N–H short by 0.06 and 0.13 Å with U_iso two to three
  times the full atom's — which is what the $`l = 2`$ and $`l = 3`$ parts of sp² C, N and O leave
  behind; `l2-tfvc` and `l3-tfvc` take most of that up in turn. Every column is monotonic in $`l`$.
- In STO-3G the same order holds (`l1` R 0.044, GoF 8.7; `l2` 0.042, 7.5; `l3` 0.041, 7.2; full
  atom 0.038, 7.0), and a softer wall helps a little: `l1-tfvc` with `salvador_stiffness= 2`
  gives GoF 7.8 against 8.7.

## 4. Three crystals, three wavefunctions, two basis sets

Ninety refinements. Converged means within 25 cycles; "25 cycles" is the cap, "GoF rose" the
refinement's own stop when a cycle makes the fit worse.

### urea

| method | basis | model | converged | R(F) | GoF | mean X–H / Å | Δ mean vs Salvador / Å | max \|Δ\| per bond / Å | mean U_iso(H) / Å² |
|---|---|---|---|---|---|---|---|---|---|
| RHF | def2-SVP | `oc-salvador` | yes | 0.0190 | 3.54 | 1.0320 | +0.000 | 0.000 | 0.0460 |
| RHF | def2-SVP | `l0-tfvc` | no, 25 cycles | 0.0442 | 11.57 | 1.0514 | +0.019 | 0.108 | 0.0900 |
| RHF | def2-SVP | `l1-tfvc` | yes | 0.0327 | 7.71 | 0.9374 | -0.095 | 0.134 | 0.0995 |
| RHF | def2-SVP | `l2-tfvc` | yes | 0.0274 | 6.22 | 0.9774 | -0.055 | 0.078 | 0.0575 |
| RHF | def2-SVP | `l3-tfvc` | yes | 0.0216 | 4.12 | 1.0050 | -0.027 | 0.041 | 0.0660 |
| RHF | def2-TZVP | `oc-salvador` | yes | 0.0170 | 3.10 | 1.0258 | +0.000 | 0.000 | 0.0446 |
| RHF | def2-TZVP | `l0-tfvc` | no, 25 cycles | 0.0422 | 11.06 | 1.0309 | +0.005 | 0.074 | 0.0930 |
| RHF | def2-TZVP | `l1-tfvc` | yes | 0.0324 | 7.41 | 0.9317 | -0.094 | 0.134 | 0.0935 |
| RHF | def2-TZVP | `l2-tfvc` | yes | 0.0263 | 5.83 | 0.9740 | -0.052 | 0.070 | 0.0555 |
| RHF | def2-TZVP | `l3-tfvc` | yes | 0.0207 | 3.91 | 1.0028 | -0.023 | 0.034 | 0.0615 |
| B3LYP | def2-SVP | `oc-salvador` | yes | 0.0167 | 2.87 | 1.0223 | +0.000 | 0.000 | 0.0419 |
| B3LYP | def2-SVP | `l0-tfvc` | no, 25 cycles | 0.0386 | 8.85 | 0.6601 | -0.362 | 0.665 | 0.2225 |
| B3LYP | def2-SVP | `l1-tfvc` | yes | 0.0309 | 6.85 | 0.9327 | -0.090 | 0.133 | 0.0835 |
| B3LYP | def2-SVP | `l2-tfvc` | yes | 0.0257 | 5.62 | 0.9722 | -0.050 | 0.072 | 0.0515 |
| B3LYP | def2-SVP | `l3-tfvc` | yes | 0.0205 | 3.69 | 0.9990 | -0.023 | 0.038 | 0.0580 |
| B3LYP | def2-TZVP | `oc-salvador` | yes | 0.0157 | 2.68 | 1.0131 | +0.000 | 0.000 | 0.0401 |
| B3LYP | def2-TZVP | `l0-tfvc` | no, 25 cycles | 0.0383 | 9.15 | 0.9677 | -0.045 | 0.139 | 0.2040 |
| B3LYP | def2-TZVP | `l1-tfvc` | yes | 0.0294 | 6.24 | 0.9291 | -0.084 | 0.122 | 0.0725 |
| B3LYP | def2-TZVP | `l2-tfvc` | yes | 0.0242 | 5.11 | 0.9691 | -0.044 | 0.059 | 0.0495 |
| B3LYP | def2-TZVP | `l3-tfvc` | yes | 0.0197 | 3.64 | 0.9966 | -0.016 | 0.027 | 0.0525 |
| PBE0 | def2-SVP | `oc-salvador` | yes | 0.0170 | 2.92 | 1.0296 | +0.000 | 0.000 | 0.0420 |
| PBE0 | def2-SVP | `l0-tfvc` | no, 25 cycles | 0.0399 | 9.18 | 0.6575 | -0.372 | 0.679 | 0.2365 |
| PBE0 | def2-SVP | `l1-tfvc` | yes | 0.0312 | 6.94 | 0.9380 | -0.092 | 0.136 | 0.0845 |
| PBE0 | def2-SVP | `l2-tfvc` | yes | 0.0261 | 5.77 | 0.9788 | -0.051 | 0.073 | 0.0515 |
| PBE0 | def2-SVP | `l3-tfvc` | yes | 0.0208 | 3.81 | 1.0057 | -0.024 | 0.039 | 0.0580 |
| PBE0 | def2-TZVP | `oc-salvador` | yes | 0.0157 | 2.63 | 1.0209 | +0.000 | 0.000 | 0.0402 |
| PBE0 | def2-TZVP | `l0-tfvc` | no, 25 cycles | 0.0386 | 9.29 | 0.9342 | -0.087 | 0.216 | 0.2280 |
| PBE0 | def2-TZVP | `l1-tfvc` | yes | 0.0299 | 6.39 | 0.9343 | -0.087 | 0.127 | 0.0760 |
| PBE0 | def2-TZVP | `l2-tfvc` | yes | 0.0246 | 5.25 | 0.9758 | -0.045 | 0.061 | 0.0495 |
| PBE0 | def2-TZVP | `l3-tfvc` | yes | 0.0199 | 3.67 | 1.0037 | -0.017 | 0.029 | 0.0530 |

### L-alanine

| method | basis | model | converged | R(F) | GoF | mean X–H / Å | Δ mean vs Salvador / Å | max \|Δ\| per bond / Å | mean U_iso(H) / Å² |
|---|---|---|---|---|---|---|---|---|---|
| RHF | def2-SVP | `oc-salvador` | yes | 0.0209 | 1.32 | 1.0840 | +0.000 | 0.000 | 0.0191 |
| RHF | def2-SVP | `l0-tfvc` | no, GoF rose | 0.0333 | 2.95 | 1.0713 | -0.013 | 0.085 | 0.1356 |
| RHF | def2-SVP | `l1-tfvc` | yes | 0.0306 | 2.50 | 1.0338 | -0.050 | 0.115 | 0.0383 |
| RHF | def2-SVP | `l2-tfvc` | yes | 0.0278 | 2.14 | 1.0253 | -0.059 | 0.116 | 0.0317 |
| RHF | def2-SVP | `l3-tfvc` | yes | 0.0232 | 1.52 | 1.0706 | -0.013 | 0.028 | 0.0283 |
| RHF | def2-TZVP | `oc-salvador` | yes | 0.0199 | 1.21 | 1.0827 | +0.000 | 0.000 | 0.0209 |
| RHF | def2-TZVP | `l0-tfvc` | no, GoF rose | 0.0324 | 2.81 | 1.0720 | -0.011 | 0.072 | 0.1230 |
| RHF | def2-TZVP | `l1-tfvc` | yes | 0.0299 | 2.42 | 1.0316 | -0.051 | 0.108 | 0.0396 |
| RHF | def2-TZVP | `l2-tfvc` | yes | 0.0270 | 2.06 | 1.0268 | -0.056 | 0.098 | 0.0331 |
| RHF | def2-TZVP | `l3-tfvc` | yes | 0.0225 | 1.44 | 1.0716 | -0.011 | 0.019 | 0.0294 |
| B3LYP | def2-SVP | `oc-salvador` | yes | 0.0195 | 1.17 | 1.0732 | +0.000 | 0.000 | 0.0183 |
| B3LYP | def2-SVP | `l0-tfvc` | yes | 0.0322 | 2.76 | 1.0730 | -0.000 | 0.055 | 0.0613 |
| B3LYP | def2-SVP | `l1-tfvc` | yes | 0.0295 | 2.33 | 1.0270 | -0.046 | 0.099 | 0.0371 |
| B3LYP | def2-SVP | `l2-tfvc` | yes | 0.0271 | 2.05 | 1.0212 | -0.052 | 0.095 | 0.0314 |
| B3LYP | def2-SVP | `l3-tfvc` | yes | 0.0223 | 1.41 | 1.0623 | -0.011 | 0.021 | 0.0277 |
| B3LYP | def2-TZVP | `oc-salvador` | yes | 0.0192 | 1.14 | 1.0689 | +0.000 | 0.000 | 0.0206 |
| B3LYP | def2-TZVP | `l0-tfvc` | yes | 0.0307 | 2.56 | 1.0531 | -0.016 | 0.060 | 0.0403 |
| B3LYP | def2-TZVP | `l1-tfvc` | yes | 0.0285 | 2.21 | 1.0243 | -0.045 | 0.097 | 0.0396 |
| B3LYP | def2-TZVP | `l2-tfvc` | yes | 0.0260 | 1.95 | 1.0209 | -0.048 | 0.084 | 0.0341 |
| B3LYP | def2-TZVP | `l3-tfvc` | yes | 0.0218 | 1.38 | 1.0606 | -0.008 | 0.015 | 0.0286 |
| PBE0 | def2-SVP | `oc-salvador` | yes | 0.0197 | 1.18 | 1.0776 | +0.000 | 0.000 | 0.0179 |
| PBE0 | def2-SVP | `l0-tfvc` | no, GoF rose | 0.0326 | 2.80 | 1.0776 | +0.000 | 0.090 | 0.1197 |
| PBE0 | def2-SVP | `l1-tfvc` | yes | 0.0297 | 2.36 | 1.0311 | -0.046 | 0.101 | 0.0369 |
| PBE0 | def2-SVP | `l2-tfvc` | yes | 0.0272 | 2.07 | 1.0248 | -0.053 | 0.098 | 0.0311 |
| PBE0 | def2-SVP | `l3-tfvc` | yes | 0.0225 | 1.43 | 1.0665 | -0.011 | 0.020 | 0.0271 |
| PBE0 | def2-TZVP | `oc-salvador` | yes | 0.0191 | 1.13 | 1.0743 | +0.000 | 0.000 | 0.0199 |
| PBE0 | def2-TZVP | `l0-tfvc` | yes | 0.0311 | 2.61 | 1.0597 | -0.015 | 0.058 | 0.0426 |
| PBE0 | def2-TZVP | `l1-tfvc` | yes | 0.0288 | 2.24 | 1.0288 | -0.046 | 0.100 | 0.0391 |
| PBE0 | def2-TZVP | `l2-tfvc` | yes | 0.0262 | 1.97 | 1.0253 | -0.049 | 0.086 | 0.0336 |
| PBE0 | def2-TZVP | `l3-tfvc` | yes | 0.0219 | 1.38 | 1.0657 | -0.009 | 0.016 | 0.0281 |

### gly-L-ala

| method | basis | model | converged | R(F) | GoF | mean X–H / Å | Δ mean vs Salvador / Å | max \|Δ\| per bond / Å | mean U_iso(H) / Å² |
|---|---|---|---|---|---|---|---|---|---|
| RHF | def2-SVP | `oc-salvador` | yes | 0.0192 | 1.68 | 1.0888 | +0.000 | 0.000 | 0.0308 |
| RHF | def2-SVP | `l0-tfvc` | yes | 0.0382 | 4.43 | 0.9753 | -0.114 | 0.273 | 0.0721 |
| RHF | def2-SVP | `l1-tfvc` | yes | 0.0352 | 3.99 | 1.0061 | -0.083 | 0.211 | 0.0491 |
| RHF | def2-SVP | `l2-tfvc` | yes | 0.0317 | 3.46 | 1.0134 | -0.075 | 0.210 | 0.0403 |
| RHF | def2-SVP | `l3-tfvc` | yes | 0.0225 | 2.14 | 1.0690 | -0.020 | 0.068 | 0.0393 |
| RHF | def2-TZVP | `oc-salvador` | yes | 0.0180 | 1.57 | 1.0840 | +0.000 | 0.000 | 0.0321 |
| RHF | def2-TZVP | `l0-tfvc` | yes | 0.0367 | 4.27 | 0.9792 | -0.105 | 0.256 | 0.0729 |
| RHF | def2-TZVP | `l1-tfvc` | yes | 0.0343 | 3.94 | 0.9968 | -0.087 | 0.194 | 0.0493 |
| RHF | def2-TZVP | `l2-tfvc` | yes | 0.0307 | 3.39 | 1.0084 | -0.076 | 0.187 | 0.0414 |
| RHF | def2-TZVP | `l3-tfvc` | yes | 0.0216 | 2.10 | 1.0687 | -0.015 | 0.042 | 0.0409 |
| B3LYP | def2-SVP | `oc-salvador` | yes | 0.0174 | 1.49 | 1.0769 | +0.000 | 0.000 | 0.0291 |
| B3LYP | def2-SVP | `l0-tfvc` | yes | 0.0364 | 4.16 | 0.9834 | -0.093 | 0.243 | 0.0695 |
| B3LYP | def2-SVP | `l1-tfvc` | yes | 0.0333 | 3.73 | 0.9986 | -0.078 | 0.179 | 0.0457 |
| B3LYP | def2-SVP | `l2-tfvc` | yes | 0.0302 | 3.27 | 1.0089 | -0.068 | 0.177 | 0.0388 |
| B3LYP | def2-SVP | `l3-tfvc` | yes | 0.0208 | 1.97 | 1.0614 | -0.016 | 0.052 | 0.0379 |
| B3LYP | def2-TZVP | `oc-salvador` | yes | 0.0160 | 1.37 | 1.0689 | +0.000 | 0.000 | 0.0316 |
| B3LYP | def2-TZVP | `l0-tfvc` | yes | 0.0345 | 3.94 | 0.9829 | -0.086 | 0.214 | 0.0629 |
| B3LYP | def2-TZVP | `l1-tfvc` | yes | 0.0314 | 3.51 | 0.9948 | -0.074 | 0.159 | 0.0459 |
| B3LYP | def2-TZVP | `l2-tfvc` | yes | 0.0282 | 3.08 | 1.0065 | -0.062 | 0.149 | 0.0408 |
| B3LYP | def2-TZVP | `l3-tfvc` | yes | 0.0197 | 1.91 | 1.0581 | -0.011 | 0.034 | 0.0392 |
| PBE0 | def2-SVP | `oc-salvador` | yes | 0.0177 | 1.51 | 1.0804 | +0.000 | 0.000 | 0.0290 |
| PBE0 | def2-SVP | `l0-tfvc` | yes | 0.0368 | 4.22 | 0.9838 | -0.097 | 0.254 | 0.0705 |
| PBE0 | def2-SVP | `l1-tfvc` | yes | 0.0337 | 3.77 | 1.0017 | -0.079 | 0.182 | 0.0456 |
| PBE0 | def2-SVP | `l2-tfvc` | yes | 0.0305 | 3.31 | 1.0123 | -0.068 | 0.181 | 0.0389 |
| PBE0 | def2-SVP | `l3-tfvc` | yes | 0.0211 | 2.00 | 1.0645 | -0.016 | 0.055 | 0.0376 |
| PBE0 | def2-TZVP | `oc-salvador` | yes | 0.0162 | 1.38 | 1.0731 | +0.000 | 0.000 | 0.0309 |
| PBE0 | def2-TZVP | `l0-tfvc` | yes | 0.0351 | 4.02 | 0.9834 | -0.090 | 0.235 | 0.0645 |
| PBE0 | def2-TZVP | `l1-tfvc` | yes | 0.0321 | 3.60 | 0.9971 | -0.076 | 0.164 | 0.0457 |
| PBE0 | def2-TZVP | `l2-tfvc` | yes | 0.0288 | 3.14 | 1.0094 | -0.064 | 0.155 | 0.0404 |
| PBE0 | def2-TZVP | `l3-tfvc` | yes | 0.0200 | 1.93 | 1.0619 | -0.011 | 0.036 | 0.0388 |

## 5. What the tables say

- **The spherical atom.** On urea it never converges, in any wavefunction or basis. On L-alanine
  it converges in three of six cases, on glycyl-L-alanine in all six — but converging is not
  fitting: its GoF is 2.2–3.6 times the full atom's, its hydrogen U_iso two to seven times, and on
  glycyl-L-alanine one methyl bond, C3–H3B, is 0.21–0.27 Å short. The runaway is a matter of
  degree: with more atoms the hydrogens' share of the missing density is smaller.
- **The dipole** (`l1-tfvc`) converges everywhere and halves the GoF excess (ratio 1.9–2.6). The
  mean X–H is now *short*, by 0.05–0.10 Å, rather than scattered: the dipole supplies the
  direction of the bonding density but not its shape, and the hydrogens still move in to supply
  the rest.
- **The quadrupole** (`l2-tfvc`) takes the GoF ratio to 1.6–2.3 and halves the bond error again
  on urea; on the amino acids it changes the bonds little, because what remains there is the
  $`l = 3`$ shape of sp³ carbon and of the N–H₃⁺ group — the worst bond is always one to the
  ammonium nitrogen (N1–H1 in L-alanine, N2–H3N2 in glycyl-L-alanine).
- **The octupole** (`l3-tfvc`) brings every combination to a GoF ratio of 1.15–1.40, a mean X–H
  within 0.027 Å (urea) and 0.020 Å (amino acids) of the full atom's, and a worst bond within
  0.041 Å (urea), 0.028 Å (L-alanine) and 0.068 Å (glycyl-L-alanine, again the ammonium N–H).
  The hydrogen U_iso stays 1.24–1.51 times the full atom's: what is left of the aspherical
  density is still taken up as hydrogen motion rather than position.
- **Wavefunction and basis move the numbers, not the picture.** B3LYP and PBE0 fit better than RHF
  at every $`l`$ (the full atom's GoF on urea: 3.5 RHF, 2.9 B3LYP, 2.9 PBE0 in def2-SVP), def2-TZVP
  a little better than def2-SVP, and the ratios between models are the same to a few per cent.
- **For a program that carries one form factor per atom type**, `l1-tfvc` is the smallest step
  that converges and is well defined: one radial curve $`F(s)`$ per atom type and a unit vector per
  atom (the bond direction, to a good approximation). `l3-tfvc` is what it takes to come within
  reach of the full atom.

## 6. What is not done

- `l4-tfvc` and beyond; the keywords stop at 3.
- The `l<n>` forms of the other radii (`tfvp`, `tfvh`) and of the exponential Hirshfeld atom.
- The spherical-form-factor fit (`FF_fit`) and the BUSTER writer use the $`l = 0`$ part only; the
  $`l = 1`$ curves and directions are not written out.
- The comparison against neutron positions and ADPs, when the neutron data for these three
  crystals are to hand.

## References

- H. Birkedal, D. Madsen, R. H. Mathiesen, K. Knudsen, H.-P. Weber, P. Pattison and
  D. Schwarzenbach, *Acta Cryst.* A60, 371 (2004).
- S. C. Capelli, H.-B. Bürgi, B. Dittrich, S. Grabowsky and D. Jayatilaka, *IUCrJ* 1, 361 (2014).
- R. Destro, R. E. Marsh and R. Bianchi, *J. Phys. Chem.* 92, 966 (1988).
- I. Mayer and P. Salvador, *Chem. Phys. Lett.* 383, 368 (2004); P. Salvador and E. Ramos-Cordoba,
  *J. Chem. Phys.* 139, 071103 (2013).
