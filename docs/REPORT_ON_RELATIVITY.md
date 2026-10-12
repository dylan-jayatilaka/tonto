# Relativity in Tonto: the scalar X2C Hamiltonian

## 1. Summary

1. **Tonto has one relativistic Hamiltonian that runs: scalar X2C**, the exact two-component
   one-electron Hamiltonian without spin–orbit coupling. `relativity_kind= x2c` puts it in place
   of the kinetic energy plus nuclear attraction. Nothing else in a restricted or unrestricted
   SCF changes, Hartree–Fock or DFT.
2. **It is Dyall's normalised elimination of the small component, solved in one step and
   renormalised** (section 3). The one-electron Dirac equation is solved once, in the
   primitive basis, before the SCF.
3. **It agrees with PySCF's spin-free X2C to 1e-8 hartree** on atoms from Ne to Rn and on small
   molecules, and with a 40-digit solution of the same equations for one-electron ions
   (section 6).
4. **The atoms use it too**: the atomic calculations behind the promolecule and the Hirshfeld
   atoms are made with the same Hamiltonian as the molecule.
5. **What it leaves out:** spin–orbit coupling, the relativistic correction to the electron
   repulsion, and the picture-change correction of the density (section 7).
6. **The nuclei are points, or Gaussian charges** with `nucleus_model= gaussian`.
7. **Use a basis set made for it**: `x2c-SVPall`, `x2c-TZVPall` or `x2c-TZVPPall`, which cover
   H to Rn with all electrons.

## 2. The equations

Take the primitive Gaussians of the basis set, each used once. Between them let $`S`$ be the
overlap matrix, $`T`$ the kinetic energy matrix, $`V`$ the nuclear attraction matrix, and $`W`$
the matrix of $`\mathbf{p}\cdot V\mathbf{p}`$, where $`\mathbf{p} = -i\nabla`$ is the momentum:

```math
W_{ab} = \langle \nabla \chi_a |\, V \,| \nabla \chi_b \rangle \qquad (1)
```

with $`\chi_a`$ a primitive function. Let $`c`$ be the speed of light, 137.035999177 atomic
units. The one-electron Dirac equation without spin–orbit coupling is then

```math
\begin{pmatrix} V & T \\ T & W/4c^2 - T \end{pmatrix}
\begin{pmatrix} C_L \\ C_S \end{pmatrix} =
\begin{pmatrix} S & 0 \\ 0 & T/2c^2 \end{pmatrix}
\begin{pmatrix} C_L \\ C_S \end{pmatrix} E \qquad (2)
```

For $`n`$ primitives it has $`2n`$ solutions. The $`n`$ with the higher energies $`E`$, measured
from the rest energy $`c^2`$, are the electronic ones; the columns of $`C_L`$ and $`C_S`$ are
their large and small components. From them

```math
X = C_S C_L^{-1} \qquad (3)
```

```math
\tilde S = S + \frac{1}{2c^2} X^\dagger T X, \qquad
\tilde L = V + TX + X^\dagger T + X^\dagger \left( \frac{W}{4c^2} - T \right) X \qquad (4)
```

```math
R = S^{-1/2}\,\left( S^{-1/2}\,\tilde S\, S^{-1/2} \right)^{-1/2} S^{1/2}, \qquad
h_+ = R^\dagger \tilde L R \qquad (5)
```

$`X`$ gives the small component of any electronic solution from its large component. $`\tilde S`$
is the norm of such a solution and $`\tilde L`$ its energy, both written in terms of the large
component alone. $`R`$ changes the large component into a two-component function with the
ordinary norm $`S`$. The matrix $`h_+`$, contracted to the basis functions, is the core matrix
of the SCF. Its eigenvalues are exactly the electronic energies $`E`$ of equation (2); as
$`c \to \infty`$, $`X \to 1`$ and $`h_+ \to T + V`$.

## 3. How this relates to Dyall

**The small-component basis.** Dyall's *modified Dirac equation* writes the small component
$`\psi_S`$ through a function $`\phi_L`$ of the same kind as the large component,
$`\psi_S = (\boldsymbol\sigma\cdot\mathbf{p}/2c)\,\phi_L`$, with $`\boldsymbol\sigma`$ the Pauli
matrices. Expanding the large component and $`\phi_L`$ in the same basis gives equation (2):
that substitution is where the metric $`T/2c^2`$ and the matrix $`W`$ come from. $`W`$ is the
spin-free part of $`(\boldsymbol\sigma\cdot\mathbf{p})V(\boldsymbol\sigma\cdot\mathbf{p})`$;
the part left out is the spin–orbit coupling.

**The elimination.** Dyall's *normalised elimination of the small component* (NESC) replaces
equation (2) by an equation for the large component only,

```math
\tilde L\, C_L = \tilde S\, C_L\, E \qquad (6)
```

with $`\tilde L`$ and $`\tilde S`$ of equation (4). It is exact when $`X`$ is exact. In Dyall's
method $`X`$ and the orbitals are found together, by iteration.

**What X2C adds.** Two things. $`X`$ is taken once, from the solutions of the one-electron
equation (2), by equation (3), with no iteration. And equation (6), which has the unusual
metric $`\tilde S`$, is turned by $`R`$ into an ordinary one-component problem with metric
$`S`$, equation (5), so that the rest of a non-relativistic program can be used unchanged. With
the electron repulsion then added in its non-relativistic form, this is the X2C method with a
one-electron decoupling.

References: K. G. Dyall, *J. Chem. Phys.* **106**, 9618 (1997); W. Kutzelnigg and W. Liu,
*J. Chem. Phys.* **123**, 241102 (2005); M. Iliaš and T. Saue, *J. Chem. Phys.* **126**, 064102
(2007); W. Liu and D. Peng, *J. Chem. Phys.* **131**, 031104 (2009).

## 4. Keywords

| keyword | where | values | meaning |
|---|---|---|---|
| `relativity_kind=` | in `scfdata= { }` | `none` (default), `x2c`, `pauli`, `zora` | `x2c` is the Hamiltonian of this report. `pauli` and `zora` are older one-electron terms for general complex orbitals. Any other value stops the job. |
| `nucleus_model=` | at the top level of the molecule | `point` (default), `gaussian` | `gaussian` makes each nucleus a Gaussian charge with the exponent tabulated by Visscher and Dyall, *At. Data Nucl. Data Tables* **67**, 207 (1997). It needs `relativity_kind= x2c`. |
| `basis_name=` | at the top level | `x2c-SVPall`, `x2c-TZVPall`, `x2c-TZVPPall` | The all-electron sets of Pollak and Weigend, *J. Chem. Theory Comput.* **13**, 3696 (2017), H to Rn. |

In `hart`:

| option | values | default |
|---|---|---|
| `--relativity` | `none`, `x2c` | `x2c` if the basis name begins with `x2c-`, otherwise `none` |
| `--nucleus` | `point`, `gaussian` | `point` |

A job:

```
basis_name= x2c-TZVPall
use_spherical_basis= TRUE

scfdata= {
   kind= rks
   dft_functional= pbe
   relativity_kind= x2c
}
```

Under X2C the energy summary prints the one-electron energy as one number. The kinetic and
nuclear attraction energies, and the virial ratio, are not defined separately.

## 5. Where it is implemented

| piece | procedure |
|---|---|
| the choice of core matrix | `MOLECULE.SCF:make_r_core_mx` |
| the list of different primitive shells; a primitive repeated in several shells of an atom is used once | `MOLECULE.REL:make_primitive_shells`, `no_of_primitive_functions` |
| $`S`$, $`T`$, $`V`$ and $`W`$ between the primitives, for point or Gaussian nuclei | `MOLECULE.REL:make_primitive_X2C_ints` |
| the integrals (1) of $`\mathbf{p}\cdot(1/r)\mathbf{p}`$ for one pair of primitives | `GAUSSIAN2:make_pVp_ints`, `make_normalized_pVp_ints` |
| the nuclear attraction for a Gaussian nucleus | `GAUSSIAN2:make_nuclear_attraction_ints`, with the nuclear exponent as an optional argument; the exponents from `GAUSSIAN2:nucleus_eta` |
| the contraction from primitives to basis functions, checked against the molecule's overlap matrix | `MOLECULE.REL:make_primitive_contraction_mx` |
| equations (2) to (5), and the contraction of $`h_+`$ | `MOLECULE.REL:make_r_X2C_core_mx` |
| a triangular solve, for the Cholesky factors | `MAT{REAL}:solve_lower_triangular_equations` |
| the energy summary under X2C | `MOLECULE.SCF:update_SCF_data_energies`, `SCF_DATA:put_energies` |
| the keywords | `SCF_DATA:set_relativity_kind`, `MOLECULE.MAIN:read_nucleus_model`, and `runfiles/run_har.foo` for `hart` |

Three things to know when reading the code.

- **The algebra is done in Cholesky bases.** With $`S = L_S L_S^\dagger`$ and
  $`T = L_T L_T^\dagger`$, equation (2) becomes an ordinary symmetric eigenproblem whose blocks
  are $`L_S^{-1} V L_S^{-\dagger}`$, $`\sqrt2\,c\,L_S^{-1} L_T`$ and
  $`\tfrac12 L_T^{-1} W L_T^{-\dagger} - 2c^2`$. In that basis $`\tilde S = 1 + X^\dagger X`$ and
  $`R = \tilde S^{-1/2}`$.
- **The routine checks itself.** The eigenvalues of $`h_+`$ must equal the electronic energies
  of equation (2), and the contracted primitive overlap must equal the molecule's overlap
  matrix; otherwise the job stops.
- **A Gaussian nucleus** of exponent $`\eta`$ has the potential
  $`\mathrm{erf}(\sqrt\eta\, r)/r`$. With $`p`$ the sum of the two primitive exponents and
  $`f = \eta/(p+\eta)`$, the point-nucleus integral is changed in three places: the Rys argument
  is scaled by $`f`$, a Rys root $`t^2`$ gives the Gaussian exponent $`p f t^2/(1 - f t^2)`$, and
  the result is scaled by $`\sqrt f`$.

## 6. The checks the code passed

The reference is PySCF 2.14, spin-free X2C, with the same basis set and the same speed of
light. Energies in hartree; Hartree–Fock and spherical `x2c-SVPall` unless said.

| system | Tonto | PySCF |
|---|---|---|
| Ne | -128.51206173 | -128.512061727 |
| Kr | -2787.70616450 | -2787.706164503 |
| Xe | -7442.86712300 | -7442.867122998 |
| Hg | -19622.39695888 | -19622.396958891 |
| Rn | -23560.96241364 | -23560.962413653 |
| HBr | -2604.57465099 | -2604.574650994 |
| HI | -7112.65502747 | -7112.655027467 |
| Br atom, unrestricted | -2603.97313216 | -2603.973132161 |
| Kr, Cartesian functions | -2787.71086791 | -2787.710867914 |
| H₂O, cc-pVDZ (repeated primitives) | -76.07156215 | -76.071562149 |
| H₂O, x2c-TZVPPall (f functions) | -76.10943979 | -76.10943979 |
| Hg, x2c-TZVPall | -19623.42960918 | -19623.429609193 |
| Kr, Gaussian nucleus | -2787.68716175 | -2787.687161752 |
| Hg, Gaussian nucleus | -19618.92472122 | -19618.924721233 |
| Rn, Gaussian nucleus | -23554.62526761 | -23554.625267627 |
| iodobenzene, x2c-TZVPall, `eri_accuracy= high` | -7343.14803111 | -7343.14803111 |

Other checks:

| what was compared with what | result |
|---|---|
| one-electron Ne⁹⁺ and Hg⁷⁹⁺, 41 even-tempered s functions, against equation (2) solved in 40-digit arithmetic | -50.06674181 and -3532.19050050; agree to 1e-8 |
| the same against the analytic Dirac energy $`c^2(\sqrt{1-(Z/c)^2}-1)`$ | 2e-7 and 1.6e-3 above it, which is the basis set |
| PBE on HBr and HI, X2C, against PySCF on a very fine grid | 7.5e-6 and 1.5e-6 at the default grid |
| `hart` on urea, `x2c-SVPall` with X2C against def2-SVP | GoF 3.209 against 3.217 |

The X2C correction to the Hartree–Fock energy in `x2c-SVPall` is -0.14 hartree for Ne, -53 for
Kr, -428 for Xe, -3702 for Hg and -5151 for Rn. A Gaussian nucleus raises the energy by 0.019
hartree for Kr, 3.47 for Hg and 6.34 for Rn.

## 7. What is left out

- **Spin–orbit coupling.** It needs the spin-dependent part of $`W`$, complex two-component
  orbitals, and a two-electron correction.
- **The relativistic correction to the electron repulsion.** $`X`$ and $`R`$ come from the
  one-electron problem and the repulsion is added unchanged.
- **The picture-change correction.** The X2C orbitals are not the large component of the Dirac
  solution but a transformed function. The density made from them in the ordinary way differs
  from the true density very near a heavy nucleus. Bučinský, Jayatilaka and Grabowsky,
  *J. Phys. Chem. A* **120**, 6650 (2016), found the effect on structure factors negligible for
  HgPh₂ and BiPh₃.
- **Energy gradients.**
- **General complex orbitals.**

## 8. Pitfalls

- **A basis set contracted for a non-relativistic calculation** (def2, cc-pVXZ) is too stiff in
  the core of a heavy atom. Use the x2c sets, and use the x2c sets only with X2C.
- **The speed of light must match** when energies are compared with another program:
  1e-7 in $`c`$ is 4e-4 hartree in a mercury atom.
- **The cost** is one eigenproblem of twice the number of primitives, made once. It is not
  noticeable for a molecule of a dozen atoms.
- **Diagonalising $`T`$ is not safe.** With exponents from 0.01 to 1e10 its eigenvalues span 13
  orders of magnitude and six figures of the energy are lost; the Cholesky factors lose none.
