# How each atom's electron density responds when the nuclei move

When a molecule vibrates, its electron density changes. Part of the change is each atom's density
being carried along with its nucleus. The rest is a real deformation: charge flows between atoms,
and each atom's density changes shape. This page sets out how to compute that response, how to
divide it into atoms, and three uses for it:

- **thermal smearing** of the density beyond the rigid-atom (convolution) model used in every
  Hirshfeld atom refinement and TLS model;
- **charge-flux and polarisable force fields**, whose parameters are the atoms' charge and dipole
  fluxes;
- **optical and dielectric properties** of crystals, where the nuclear response gives the
  vibrational contribution, with an atom-by-atom interpretation.

The response to an electric field, which gives atomic polarisabilities, uses the same machinery
and is treated alongside. The plan for Tonto is in [`TASK_ON_ATOMIC_ED_RESPONSE.md`](TASK_ON_ATOMIC_ED_RESPONSE.md).

Units are atomic units. A perturbation is written $`x`$: a Cartesian coordinate of one nucleus, a
normal coordinate $`Q_k`$, or a component of an electric field $`\mathcal E`$. A superscript
$`(x)`$ is the derivative with respect to it at the reference geometry and zero field.


## 1. The response of the molecular density

The density in a basis of atom-centred functions $`\chi_\mu`$ with density matrix $`D_{\mu\nu}`$ is
$`\rho(\mathbf r) = \sum_{\mu\nu} D_{\mu\nu}\,\chi_\mu(\mathbf r)\,\chi_\nu(\mathbf r)`$. Its first
derivative has two parts,

```math
\rho^{(x)}(\mathbf r) = \sum_{\mu\nu} D^{(x)}_{\mu\nu}\,\chi_\mu\chi_\nu
  + \sum_{\mu\nu} D_{\mu\nu}\,\bigl(\chi^{(x)}_\mu\chi_\nu + \chi_\mu\chi^{(x)}_\nu\bigr) . \qquad (1)
```

- **The relaxation term**, with $`D^{(x)}`$, comes from the coupled-perturbed SCF equations (CPHF
  for Hartree–Fock, CPKS for Kohn–Sham). For an electric field it is the whole response. For a
  nuclear displacement its occupied–occupied block is fixed by the derivative of the overlap
  matrix, $`-\tfrac12 S^{(x)}`$, and its virtual–occupied block is the solution of the CPHF
  equations.
- **The following-basis term**, with $`\chi^{(x)}_\mu`$, exists only for nuclear displacements:
  the basis functions sit on the nuclei and move with them.

Only the sum is physical; the split depends on the basis. The equations are those of an analytic
Hessian (Gerratt and Mills 1968; Pople et al. 1979). All $`3N`$ responses are needed, $`N`$ being
the number of atoms, so the Z-vector shortcut of Handy and Schaefer (1984) does not help. Solving
them with an auxiliary basis for the Coulomb (and exchange) terms is routine: RI-J Hessians
(Deglmann et al. 2004), RIJCOSX Hessians (Bykov et al. 2015), and auxiliary density perturbation
theory, in which the response density itself is carried by auxiliary coefficients (Flores-Moreno
and Köster 2008; Delgado-Venegas et al. 2016).

In density-functional language the response is the linear response function $`\chi(\mathbf r,\mathbf r')`$
acting on the derivative of the nuclear potential $`v`$,

```math
\rho^{(x)}(\mathbf r) = \int \chi(\mathbf r,\mathbf r')\, v^{(x)}(\mathbf r')\, d\mathbf r' , \qquad (2)
```

the object behind the nuclear reactivity indices of Cohen, Ganduglia-Pirovano and Kudrnovský
(1994). In solids the same object, for a phonon, is the central quantity of density-functional
perturbation theory (Baroni et al. 2001).

**Translation sum rule.** Moving every nucleus by the same amount moves the whole density, so

```math
\sum_{B} \frac{\partial \rho}{\partial \mathbf R_B}(\mathbf r) = -\nabla \rho(\mathbf r) , \qquad (3)
```

where $`\mathbf R_B`$ is the position of nucleus $`B`$. It is exact, and a check on any
implementation.

**Second derivatives.** A thermal average to second order in the displacements needs
$`\rho^{(kk)} = \partial^2\rho/\partial Q_k^2`$ along each normal mode $`k`$. Second-order CPHF
gives it analytically; in practice it is a finite difference of first-order responses along each
mode.


## 2. Dividing the response into atoms

A real-space partition gives atom $`A`$ the density $`\rho_A = w_A\rho`$, with weights $`w_A(\mathbf r)`$
that sum to one everywhere: Hirshfeld's stockholder weights, or the fuzzy Voronoi cells of the
TFVC atom (Salvador and Ramos-Cordoba 2013). The weights depend on the nuclear positions, so

```math
\rho_A^{(x)} = w_A\,\rho^{(x)} + w_A^{(x)}\,\rho . \qquad (4)
```

The atoms' responses sum to the molecular response, because the $`w_A^{(x)}`$ sum to zero.

**Rigid following and deformation.** If atom $`A`$'s density were rigid, moving its nucleus by
$`\delta\mathbf R_A`$ would change it by $`-\delta\mathbf R_A\cdot\nabla\rho_A`$, and moving other
nuclei would not change it at all. The deformation is what is left. For a normal mode $`k`$ in
which nucleus $`A`$ moves by $`\mathbf d^k_A`$ per unit $`Q_k`$,

```math
\delta\rho_A^{(k)} = \rho_A^{(k)} + \mathbf d^k_A\cdot\nabla\rho_A . \qquad (5)
```

The rigid-atom (convolution) model of thermal smearing is the statement $`\delta\rho_A^{(k)} = 0`$.
The size and shape of $`\delta\rho_A^{(k)}`$ is therefore a direct test of it, atom by atom and
mode by mode.

**Atomic multipoles of the response.** Expanding $`\delta\rho_A^{(x)}`$ about its nucleus in the
harmonics $`C_{lm}`$, as the TFVC($`l_{\max}`$) atoms are, gives the response by angular character.
Its $`l = 0`$ integral is the charge flux and its $`l = 1`$ moment the dipole flux,

```math
q_A^{(x)} = -\int \rho_A^{(x)}\, d\mathbf r , \qquad
\mathbf m_A^{(x)} = -\int (\mathbf r - \mathbf R_A)\,\rho_A^{(x)}\, d\mathbf r + N_A\,\mathbf R_A^{(x)} , \qquad (6)
```

where $`N_A = \int\rho_A\,d\mathbf r`$ is the atom's electron population, $`q_A`$ its net charge
(nuclear charge minus $`N_A`$), and $`\mathbf m_A = -\int(\mathbf r-\mathbf R_A)\,\rho_A\,d\mathbf r`$ its
electronic dipole about its own nucleus. Since the charges sum to the molecular charge,
$`\sum_A q_A^{(x)} = 0`$. Higher $`l`$ give the quadrupole flux and beyond, which no published
model has used.

**The atomic polar tensor.** The molecular dipole is $`\mathbf p = \sum_A (q_A\mathbf R_A + \mathbf m_A)`$.
Its derivative with respect to the position of nucleus $`B`$ is the atomic polar tensor of $`B`$,
and it splits into a charge, a charge flux and a dipole flux,

```math
\frac{\partial \mathbf p}{\partial \mathbf R_B}
 = q_B\,\mathbf 1 + \sum_A \mathbf R_A \otimes \frac{\partial q_A}{\partial \mathbf R_B}
 + \sum_A \frac{\partial \mathbf m_A}{\partial \mathbf R_B} . \qquad (7)
```

This is the charge–charge flux–dipole flux model of Haiduke and Bruns (2005), there with QTAIM
atoms. The left side is known exactly from the analytic dipole derivative, so (7) is a check on
the partitioned responses.


## 3. Uses

### 3.1 Thermal smearing beyond the rigid atom

Write each nucleus's displacement as a sum over harmonic normal modes, $`\mathbf u_A = \sum_k \mathbf d^k_A Q_k`$,
with $`\langle Q_k Q_l\rangle = \delta_{kl}\langle Q_k^2\rangle`$ and
$`\langle Q_k^2\rangle = (2\omega_k)^{-1}\coth(\omega_k/2k_BT)`$ for mass-weighted coordinates of
frequency $`\omega_k`$ at temperature $`T`$. The atom's density at a displaced geometry, in a frame
that moves with its nucleus, is
$`\rho_A + \sum_k Q_k\,\delta\rho_A^{(k)} + \tfrac12\sum_{kl} Q_kQ_l\,\delta\rho_A^{(kl)}`$. Averaging
its Fourier transform over the Gaussian distribution of displacements gives, to second order in
the amplitudes,

```math
\bar f_A(\mathbf k) = e^{-\frac12 \mathbf k^{\dagger} U_A \mathbf k}\,
 \Bigl[\, f_A(\mathbf k)
 + i \sum_k \langle Q_k^2\rangle\,(\mathbf k\cdot\mathbf d^k_A)\,\delta f_A^{(k)}(\mathbf k)
 + \tfrac12 \sum_k \langle Q_k^2\rangle\,\delta f_A^{(kk)}(\mathbf k) \Bigr] , \qquad (8)
```

where $`f_A`$, $`\delta f_A^{(k)}`$ and $`\delta f_A^{(kk)}`$ are the Fourier transforms of $`\rho_A`$,
$`\delta\rho_A^{(k)}`$ and $`\delta\rho_A^{(kk)}`$, $`U_A = \sum_k\langle Q_k^2\rangle\,\mathbf d^k_A\mathbf d^{k\dagger}_A`$
is the atom's ADP, and $`\mathbf k`$ is the scattering vector ($`|\mathbf k| = 4\pi\sin\theta/\lambda`$).

- The first term is the convolution model.
- The second term pairs the first-order deformation with the motion of the atom's own nucleus in
  the same mode. It is of the same order as the third, and is easily overlooked.
- Every correction is one-centre. It needs no products of basis functions on two atoms, unlike the
  two-centre smearing of [`tc-tls`](REPORT_ON_TC_TLS.md) and of Michael and Koritsanszky (2015),
  and it includes the relaxation of the density matrix, which both of those leave out.
- With the corrections expanded in harmonics and fitted to the $`s^l`$-weighted Gaussians of the
  TFVC form factors, the thermally averaged atom costs no more in a refinement than the static
  one.

### 3.2 Charge flux and polarisable force fields

The charges, charge fluxes and dipole fluxes of (6) and (7), and the atomic polarisabilities from
the electric-field response, are the parameters of a polarisable force field with charge flux
(Dinur and Hagler 1995; the AMOEBA+ charge flux of Liu, Piquemal and Ren 2020). In published work
they are fitted to dipole surfaces or taken as finite differences of partitioned charges. Here they
are analytic, for one partition used throughout, and they carry the term from the moving weights
in (4), which the finite-difference and fitted approaches do not separate.

### 3.3 Optical and dielectric properties

The electronic part of a crystal's dielectric response comes from the electric-field response:
molecular or atomic polarisabilities placed in the crystal with Lorentz-factor tensors give
refractive indices (Whitten, Jayatilaka and Spackman 2006; Jayatilaka et al. 2009). The
vibrational part comes from the nuclear response. Each normal mode $`k`$ has an effective charge
$`\mathbf Z_k = \sum_B (\partial\mathbf p/\partial\mathbf R_B)\,\mathbf d^k_B`$, and its contribution to
the static dielectric tensor is

```math
\Delta\varepsilon = \frac{4\pi}{V} \sum_k \frac{\mathbf Z_k \mathbf Z_k^{\dagger}}{\omega_k^2} , \qquad (9)
```

with $`V`$ the cell volume. With (7), each mode's contribution splits into atomic charges, charge
fluxes and dipole fluxes, so the infrared and static dielectric response of a molecular crystal
can be read atom by atom.


## 4. What has been done elsewhere

| subject | what exists | what does not |
|---|---|---|
| Response of the density to nuclei | Analytic in every Hessian code; in solids for phonons (DFPT); with an auxiliary basis as a speed-up (RI-J, RIJCOSX, auxiliary density perturbation theory) | Analysis of the response density itself; it is formed and discarded |
| Atomic interpretation | Charge, charge flux and dipole flux from QTAIM by finite differences of atomic populations (Haiduke and Bruns 2005 and later); Hirshfeld charges and dipoles in the same analysis (Richter et al. 2021); APT partitions (King, Mast; Gussoni, Castiglioni, Zerbi; Cioslowski; Milani and Castiglioni) | A real-space partition of the analytic $`\rho^{(x)}`$; the split into rigid following and deformation; anything beyond the dipole |
| Electric-field response by atoms | Distributed polarisabilities (Stone 1985; Ángyán et al. 1994 with CPHF and QTAIM; Misquitta and Stone, ISA-Pol); Hirshfeld polarisabilities (Krishtal et al. 2006); crystal polarisabilities and refractive indices (Krawczuk, Macchi et al., PolaBer) | — |
| Thermal smearing | The rigid-atom convolution in all HAR, TAAM and normal-mode refinement (Hoser and Madsen; Butkiewicz et al. 2025); temperature factors for density between atoms (Scheringer 1977); exact two-centre rigid convolution (Michael and Koritsanszky 2015); ensemble averages over sampled geometries for small molecules (the same authors); a perturbative calculation of the thermally averaged density of silicon (Nery et al., arXiv 2026) | Partitioned response densities used to correct the convolution model in molecular crystals |
| Force fields | Charge flux fitted to dipole surfaces (AMOEBA+); geometry-dependent charges (Dinur and Hagler); machine-learned QTAIM multipoles (FFLUX) | Analytic charge and dipole fluxes on Hirshfeld or TFVC atoms |
| Optical properties | Electronic: CPHF refractive indices from X-ray constrained wavefunctions; PolaBer. Vibrational: Born charges and the IR dielectric function in periodic codes | An atom-resolved vibrational contribution for molecular crystals |

The partition matters for thermal smearing. Chodkiewicz and Woźniak (2025) show that, with
overlapping Hirshfeld atoms, errors in the ADPs of an atom bonded to hydrogen are partly taken up
by the hydrogen's ADPs; how much of a bond's density moves with which nucleus is decided by the
partition. The deformation $`\delta\rho_A^{(k)}`$ of (5) measures it directly.


## 5. What Tonto has

- **Coupled-perturbed Hartree–Fock for electric fields** (`MOLECULE.CP`): the solver
  `solve_CPHF_equations` takes any number of perturbations; RHF only, with no exchange–correlation
  kernel. The perturbed densities are divided into Hirshfeld atoms to give atomic charge and dipole
  polarisabilities (`make_SCF_atomic_polarisability`), and the molecular polarisabilities give
  unit-cell polarisabilities and refractive indices (`put_UC_refractive_index`).
- **First-derivative integrals** for overlap, kinetic energy and nuclear attraction
  (`GAUSSIAN2`, `SHELL2`: `make_S_1st_deriv_ints`, `make_T_1st_deriv_ints`, `make_NA_1st_deriv_ints`)
  and for electron repulsion (`GAUSSIAN4`, `SHELL4`: `make_ERI_derivatives`). None is used by any
  calculation, and none is tested.
- **A Hartree–Fock energy gradient** that assembles those integrals (`MOLECULE.MISC`), entirely
  commented out and known to be wrong. Each term has a finite-difference check beside it.
- **The density at displaced geometries** from `tc-tls`, which runs an SCF at each displacement
  for its derivatives and its averages over the stiff modes
  ([`REPORT_ON_TC_TLS_AVERAGES.md`](REPORT_ON_TC_TLS_AVERAGES.md)). Finite differences of these
  densities give $`\rho^{(x)}`$ and $`\rho^{(kk)}`$ without any new integrals.
- **The TFVC atoms and their harmonic expansion** (`make_sph_avgd_SA_ED_grid`), with the LKO cutoff
  that makes the cell weights cost linear in the size of the system.


## References

- Ángyán J. G., Jansen G., Loos M., Hättig C., Hess B. A., *Chem. Phys. Lett.* **219**, 267 (1994).
- Baroni S., de Gironcoli S., Dal Corso A., Giannozzi P., *Rev. Mod. Phys.* **73**, 515 (2001).
- Butkiewicz H., Chodkiewicz M., Madsen A. Ø., Hoser A. A., *IUCrJ* **12**, 123 (2025).
- Bykov D., Petrenko T., Izsák R., Kossmann S., Becker U., Valeev E., Neese F., *Mol. Phys.* **113**, 1961 (2015).
- Chodkiewicz M., Woźniak K., *IUCrJ* **12**, 74 (2025).
- Cohen M. H., Ganduglia-Pirovano M. V., Kudrnovský J., *J. Chem. Phys.* **101**, 8988 (1994).
- Cioslowski J., *J. Am. Chem. Soc.* **111**, 8333 (1989).
- Deglmann P., May K., Furche F., Ahlrichs R., *Chem. Phys. Lett.* **384**, 103 (2004).
- Delgado-Venegas R. I., Mejía-Rodríguez D., Flores-Moreno R., Calaminici P., Köster A. M., *J. Chem. Phys.* **145**, 224103 (2016).
- Dinur U., Hagler A. T., *J. Comput. Chem.* **16**, 154 (1995).
- Flores-Moreno R., Köster A. M., *J. Chem. Phys.* **128**, 134105 (2008).
- Gerratt J., Mills I. M., *J. Chem. Phys.* **49**, 1719 (1968).
- Gussoni M., Castiglioni C., Zerbi G., *J. Chem. Phys.* **80**, 1377 (1984).
- Haiduke R. L. A., Bruns R. E., *J. Phys. Chem. A* **109**, 2680 (2005).
- Handy N. C., Schaefer H. F., *J. Chem. Phys.* **81**, 5031 (1984).
- Hoser A. A., Madsen A. Ø., *Acta Cryst. A* **72**, 206 (2016); *IUCrJ* **12**, 421 (2025).
- Jayatilaka D., Munshi P., Turner M. J., Howard J. A. K., Spackman M. A., *Phys. Chem. Chem. Phys.* **11**, 7209 (2009).
- King W. T., Mast G. B., Blanchette P. P., *J. Chem. Phys.* **56**, 4440 (1972).
- Krawczuk A., Pérez D., Macchi P., *J. Appl. Cryst.* **47**, 1452 (2014).
- Krishtal A., Senet P., Yang M., Van Alsenoy C., *J. Chem. Phys.* **125**, 034312 (2006).
- Liu C., Piquemal J.-P., Ren P., *J. Phys. Chem. Lett.* **11**, 419 (2020).
- Michael J. R., Koritsanszky T., *J. Math. Chem.* **53**, 250 (2015).
- Milani A., Castiglioni C., *J. Mol. Struct. THEOCHEM* **955**, 158 (2010).
- Misquitta A. J., Stone A. J., *J. Chem. Phys.* **124**, 024111 (2006); *Theor. Chem. Acc.* **137**, 153 (2018).
- Nery J. P., Gupta R., Hellman O., Allen P. B., Verstraete M. J., arXiv:2608.27361 (2026), preprint.
- Pople J. A., Krishnan R., Schlegel H. B., Binkley J. S., *Int. J. Quantum Chem. Symp.* **13**, 225 (1979).
- Richter W. E., Duarte L. J., Vidal L. N., Bruns R. E., *J. Phys. Chem. A* **125**, 3219 (2021).
- Salvador P., Ramos-Cordoba E., *J. Chem. Phys.* **139**, 071103 (2013).
- Scheringer C., *Acta Cryst. A* **33**, 426 and 430 (1977).
- Stone A. J., *Mol. Phys.* **56**, 1065 (1985).
- Whitten A. E., Jayatilaka D., Spackman M. A., *J. Chem. Phys.* **125**, 174505 (2006).
