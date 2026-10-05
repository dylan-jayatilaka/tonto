# Fitting ADPs with rigid-body motion and internal modes

Atomic displacement parameters (ADPs) are usually refined atom by atom: six numbers per atom,
with nothing tying the atoms together. In a molecular crystal the atoms move together. The
molecule translates and librates as a whole (rigid-body motion, T, L, S), and it vibrates
internally. Each internal vibration is a normal mode of the molecule. The stiff modes (bond
stretches and bends) are well described by a calculated Hessian and need no refinement. The
soft modes (torsions, wags) are where the crystal matters.

Tonto refines the rigid-body motion directly against the structure factors, with the stiff
internal modes added from a Hessian. This page gives the equations, the keywords, where the
code is, and what it gives on urea. The soft modes are not yet refined; see the last section.

Units: Tonto works in atomic units (bohr, bohr², electron masses, hartree) and prints ADPs in
Å², librations in degrees², and frequencies in cm⁻¹.


## 1. The model

The ADP of atom *i* is the sum of a rigid-body part and a stiff-internal-mode part:

    U_i = U_i^high + B_i Σ B_iᵀ                                                    (1)

### Rigid-body motion

A rigid body moves atom *i* by a translation **t** and a small rotation **λ** about an origin:

    u_i = t + λ × r_i = B_i v,     B_i = [ 1 | A_i ],     v = (t, λ),

    A_i = [[ 0,  z_i, −y_i ],
           [−z_i,  0,   x_i ],
           [ y_i, −x_i,  0  ]],

with **r**_i = (x_i, y_i, z_i) measured from the origin. Σ = ⟨v vᵀ⟩ is the 6 × 6 covariance of
the rigid-body motion, and its blocks are the familiar T, L and S:

    Σ = [[ T, Sᵀ ],        T = ⟨t tᵀ⟩  (Å²),   L = ⟨λ λᵀ⟩  (rad²),   S = ⟨λ tᵀ⟩  (Å rad).
         [ S, L  ]]

Writing out B_i Σ B_iᵀ gives the Schomaker–Trueblood formula

    U_i^TLS = T + A_i L A_iᵀ + A_i S + Sᵀ A_iᵀ.                                    (2)

- **The origin** is the fragment's centre of mass. T and S depend on it; L does not.
- **tr S is not determined.** Adding the same amount to the three diagonal elements of S adds
  nothing to any U_i, since A_i is antisymmetric. Tonto drops that direction, which makes
  tr S = 0. Σ has 21 independent elements, so 20 remain.
- **Site symmetry.** If the molecule sits on a special position, Σ must carry the site
  symmetry. Tonto imposes it through the atoms:
  - Each atom that is a symmetry image of another must get the symmetry-transformed U.
  - Each atom on a special position must have a U unchanged by its site-symmetry operations.

  These are linear conditions on Σ, and the allowed Σ are the solutions. Urea on its mm2 site
  keeps 8 of the 20.

### Stiff internal modes

From the Cartesian Hessian H at the molecule's geometry:

1. **Mass-weight:** H' = M^(−1/2) H M^(−1/2).
2. **Project out the rigid-body motion** (the Eckart–Sayvetz conditions): H'' = P H' P, with
   P = 1 − Σ_v v vᵀ over the six mass-weighted translations and rotations, orthonormalised.
   These six become exact zero modes, five for a linear molecule.
3. **Diagonalise:** the eigenvalues are ω_k², the eigenvectors **l**_k.

Mode k at temperature T has mean-square amplitude

    ⟨Q_k²⟩ = coth(ω_k / 2 k_B T) / (2 ω_k)      (atomic units; 1/(2ω_k) at T = 0),     (3)

and adds **l**_ik **l**_ikᵀ ⟨Q_k²⟩ / m_i to the U of atom *i*. U_i^high in (1) is the sum over
the **stiff** modes, those above a cutoff frequency (200 cm⁻¹ by default):

    U_i^high = Σ_{ω_k > ω_c} l_ik l_ikᵀ ⟨Q_k²⟩ / m_i.                              (4)

The soft modes are left out of U^high. A mode with an imaginary frequency counts as soft,
because a Hessian taken at the crystal geometry, which is not a minimum, can have such modes.
In the full model the data must determine their amplitudes.

**Frequency scaling.** Calculated harmonic frequencies are too high, so U^high is too low. The
frequencies in (4) can be multiplied by a scale factor. The default is the value of Scott and
Radom (1996) for the method, when Tonto made the Hessian itself:

| method | scale factor |
|---|---|
| HF | 0.8953 |
| B3LYP | 0.9614 |
| BLYP | 0.9945 |
| other | 1 |

They were fitted for the 6-31G(d) basis and are applied whatever the basis. For a Hessian read
in from a file, the method is unknown and the default is 1.

### A Hessian by finite differences

Without an external program, Tonto makes the Hessian from its own SCF energies by central
differences with step h (0.01 bohr by default):

    H_ii = [E(+i) + E(−i) − 2 E₀] / h²
    H_ij = [E(+i,+j) − E(+i,−j) − E(−i,+j) + E(−i,−j)] / (4 h²)

**Translational invariance** says each atom's block row of H sums to zero over the atoms. Tonto
uses it to get the last atom's rows, so the Hessian of N atoms needs 2n + 2n(n−1) SCFs with
n = 3(N−1). This holds at any geometry, but not with cluster charges or an applied field; then
every coordinate is differenced.

The **gradient** comes free from the same energies and is printed. At a geometry that is not
stationary, such as a crystal geometry, the rotations are not exact zero modes of H. The
projection in step 2 is then an approximation.


## 2. Refining against the structure factors

The refinement's parameters p are kept explicitly. The model vector X (every atom's position
and ADP) is made from them by

    X = X₀ + J p,        J = ∂X/∂p.                                                (5)

- **Free ADPs:** J has one unit column per refined component.
- **TLS model:** each atom keeps its three position columns. Its six ADP columns are replaced
  by columns shared by all atoms, one per allowed Σ parameter. The fixed part X₀ holds
  U^high on the ADP rows.

From the derivatives of the structure factors with respect to X, the normal equations are
formed for p:

    A = (∂F/∂X J)ᵀ w (∂F/∂X J),     b = (∂F/∂X J)ᵀ w ΔF,     A Δp = b.

Near-zero eigenvalues of A are filtered out as for any refinement. The covariance of p is
C = A⁻¹ scaled by GoF². That of the atoms follows as J C Jᵀ, which gives every atomic U an esd
even though only Σ is refined. Each refinement cycle starts by putting the ADPs on the model:
Σ is fitted to the current U − U^high, and U is set to U^high + B Σ Bᵀ.

**Compare models by GoF, not R.** GoF counts the parameters; R can hide a worse fit.


## 3. Keywords

On the molecule (top level):

| keyword | default | meaning |
|---|---|---|
| `force_constants= { ... }` | | the Cartesian Hessian as 3N × 3N numbers, atomic units |
| `read_g09_fchk_file` | | reads the Hessian from a Gaussian checkpoint, if it holds one |
| `fd_hessian_step=` | 0.01 | step for `make_fd_hessian`, bohr |
| `make_fd_hessian` | | the Hessian by finite differences of SCF energies (after `scf`) |
| `normal_mode_analysis` | | the normal modes, rigid-body motion projected out |
| `put_normal_modes` | | prints the Hessian and the modes |
| `internal_adp_temperature=` | 0 | temperature for (3), Kelvin; 0 gives zero-point motion |
| `soft_mode_cutoff=` | 200 | modes below this, cm⁻¹, are soft |
| `frequency_scale_factor=` | see above | scales the frequencies in (4) |
| `make_internal_adps` | | makes U^high, eq. (4) |
| `put_internal_adps` | | prints U^high and how many modes are soft, imaginary and stiff |

In `xray_data=`:

| keyword | default | meaning |
|---|---|---|
| `adp_model=` | `free` | `free`: refine each atom's ADP; `tls`: the model (1), HAR only |

A TLS Hirshfeld atom refinement with U^high from a Hessian:

```
   CIF= { file_name= urea.cif }
   process_CIF

   force_constants= { ... }          ! or: scf, then make_fd_hessian
   normal_mode_analysis
   internal_adp_temperature= 123
   frequency_scale_factor= 0.8953    ! RHF/6-31G(d) Hessian
   make_internal_adps
   put_internal_adps

   crystal= {
      xray_data= {
         partition_model= oc-hirshfeld
         adp_model= tls
         ...
      }
   }
   scfdata= { ... }
   scf
   HAR_refinement
```

The test `tests/long/urea_rhf_STO-3G_HAR_TLS` is a complete example.


## 4. Where it is implemented

| piece | procedure |
|---|---|
| rigid-body vectors, mass-weighted, orthonormal | `VEC{ATOM}:make_rigid_body_modes` |
| P M P, projecting vectors out of a matrix | `MAT{REAL}:project_out_vectors` |
| normal modes with the projection | `MOLECULE.PROP:normal_mode_analysis` |
| Hessian by finite differences | `MOLECULE.PROP:make_FD_hessian` |
| U^high, eq. (4); its printout | `MOLECULE.PROP:make_internal_ADPs`, `put_internal_ADPs` |
| frequency scale factor | `MOLECULE.PROP:harmonic_frequency_scale` |
| U^high handed to the refinement | `MOLECULE.PROP:set_crystal_U_high`, `CRYSTAL:set_fragment_U_high` |
| ∂U_i/∂Σ for one atom | `CRYSTAL:TLS_response` |
| allowed Σ, J columns, tr S, origin | `CRYSTAL:make_TLS_jacobian_columns` |
| p and X₀; ADPs put on the model | `CRYSTAL:set_refinement_parameters` |
| T, L, S with esds | `CRYSTAL:put_TLS_results` |
| explicit parameters, eq. (5); the solve | `LEAST_SQUARES` (`least_squares.foo`) |


## 5. Results

### Water

RHF/6-31G(d) Hessian from Gaussian. After the projection the six rigid-body modes are exactly
zero. The internal frequencies are 1826.4, 4070.3 and 4188.5 cm⁻¹, against Gaussian's 1826.6,
4070.6 and 4188.8; Tonto uses average atomic masses and Gaussian isotopic ones. At 0 K the
hydrogen U^high has principal values 0.0043 and 0.0038 Å² in the molecular plane and zero out of
it, where water has no internal mode. The O–H stretch alone gives ħ/2μω ≈ 0.0046 Å².

### Urea: stiff-mode ADPs

RHF/6-31G(d) Hessian by finite differences at the HAR geometry, 123 K, frequencies unscaled:

| atom | U^high, U_iso /Å² |
|---|---|
| O | 0.0005 |
| N | 0.0006 |
| C | 0.0010 |
| H1, H2 (cis to O) | 0.0117 |
| H3, H4 | 0.0067 |

The crystal geometry is planar, while gas-phase urea has pyramidal NH₂ groups. So the two NH₂
wags are imaginary (432i and 208i cm⁻¹) and are left out of U^high as soft modes. The gradient
there is 0.093 Eh/bohr. RHF frequencies are about 10 % high, so these U^high are about 10 % low.

### Urea: TLS refinement

Hirshfeld atom refinement, def2-SVP, 123 K data, U^high as above:

| ADP model | parameters | GoF | R(F) | N–H1 /Å | N–H3 /Å | U_iso H1 / H3 /Å² |
|---|---|---|---|---|---|---|
| free | 27 | 3.30 | 0.0181 | 1.028(5) | 0.986(6) | 0.054(4) / 0.048(3) |
| TLS, U^high = 0 | 17 | 3.84 | 0.0184 | 1.025(5) | 0.994(5) | 0.0334(15) / 0.0329(13) |
| TLS + U^high | 17 | 3.51 | 0.0180 | 1.028(5) | 0.994(5) | 0.0447(15) / 0.0399(13) |
| neutron | | | | 1.006 | 1.000 | |

- **The stiff modes matter:** adding U^high lowers GoF from 3.84 to 3.51.
- **TLS + U^high still fits worse than free ADPs:** GoF 3.51 against 3.30. Whether that
  difference is significant is for a Hamilton test; it is not yet printed.
- **The hydrogen ADPs are short of the free ones** by about 0.01 Å². That is where the two NH₂
  wags belong, the soft modes this model does not yet refine.
- **The ADP model hardly moves the hydrogen positions:** N–H3 shifts 0.008 Å toward neutron,
  N–H1 not at all.

The rigid-body motion, about the centre of mass (8 parameters on the mm2 site):

    T /Å²:      0.01402(7)   -0.00045(9)   0                L /deg²:   19.8(17)   12.2(17)   0
               -0.00045(9)    0.01402(7)   0                           12.2(17)   19.8(17)   0
                0             0            0.00591(4)                   0          0         44(4)

    S /(Å deg): -0.18(2)  0.12(2)  0;   -0.12(2)  0.18(2)  0;   0  0  0      (rows λ, columns t)

L has eigenvalues 7.6, 32 and 44 deg², the largest about the C=O axis; the rms libration is 5.3°.
S has zero trace.


## 6. What is not done yet

- **Soft modes.** Refining the amplitudes of the softest internal modes (including any
  imaginary ones) one at a time, held toward their harmonic values, with AIC, BIC and the
  Hamilton test printed at each step to say how many the data support.
- T, L and S reported at the centre of reaction as well as at the centre of mass.
- An ORCA Hessian reader.
- One TLS group per fragment only; no segmented (attached-group) TLS.


## References

- V. Schomaker and K. N. Trueblood, *Acta Cryst.* B24, 63 (1968): rigid-body motion, T, L, S.
- A. P. Scott and L. Radom, *J. Phys. Chem.* 100, 16502 (1996): harmonic frequency scale factors.
- W. C. Hamilton, *Acta Cryst.* 18, 502 (1965): significance tests on the crystallographic R factor.
