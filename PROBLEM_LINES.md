# Problematic lines — two debug-build crashes (array / size mismatches)

(Written for Dylan — I could not send email from this machine: no mail transport
installed and no email tool available. Open/copy from here, or I can commit+push it
so it reaches your home machine via `git pull`.)

Both crashes are debug-only `ENSURE`/precondition aborts (release compiles them out).

---

## 1. `h2o_rks_BVWN5_cc-pVDZ`  —  "Nx missing"  (FIXED)

Runtime error:
    Error in DFT_FUNCTIONAL:new_r_potential ... Nx missing

The ENSURE that fired:
    foofiles/dft_functional.foo:265   ENSURE(present(Nx) EQV NOT .is_LDA_functional(name),"Nx missing")

Siblings with the same over-strict form:
    foofiles/dft_functional.foo:220   (new_r_energy_density)   "Nx, Ny, Nz missing"
    foofiles/dft_functional.foo:302   (new_u_energy_density)   "Nxa missing"
    foofiles/dft_functional.foo:303   (new_u_energy_density)   "Nxb missing"

Root cause — the caller passes the density gradient Nx to BOTH exchange and
correlation, unconditionally:
    foofiles/molecule.fock.foo:5041   if (exch/="none") add.new_r_potential(exch,V0,N0,Vx,Vy,Vz,Nx,Ny,Nz)
    foofiles/molecule.fock.foo:5042   if (corr/="none") add.new_r_potential(corr,V0,N0,Vx,Vy,Vz,Nx,Ny,Nz)
    foofiles/molecule.fock.foo:5002-5003  (same for new_r_energy_density)

For BVWN5 = Becke88 (GGA exchange) + VWN5 (LDA correlation): Nx IS computed (needed
for Becke88) and is also handed to VWN5. The LDA branch ignores it:
    foofiles/dft_functional.foo:279   case ("vwn5   "); .new_r_VWN5_c_potential(V0,N0)   ! Nx unused

So `present(Nx)` is TRUE while `is_LDA_functional("vwn5")` is TRUE — the biconditional
fails even though passing Nx to an LDA functional is harmless.

FIX (applied): relax the biconditional to "GGA requires Nx; LDA tolerates it":
    ENSURE(.is_LDA_functional(name) OR present(Nx), "...")
Lines 220, 265, 302, 303. Test now passes.

---

## 2. `h2o_rhf_cc-pVDZ_dipole_polarisabilities`  —  "incompatible sizes"  (NOT yet fixed)

Runtime error:
    Error in MAT{REAL}:back_transform_to_2 ... incompatible sizes
    (stdin keyword: put_scf_dipole_hyperpolarisability)

The ENSUREs that fired are inside `back_transform_to(new,L,R)`  (new = L·self·R†),
the `back_transform_to_2` overload:
    foofiles/mat{intrinsic}.foo:4231   ENSURE(.dim2==R.dim2,"incompatible sizes")
    foofiles/mat{intrinsic}.foo:4232   ENSURE(.dim1==L.dim2,"incompatible sizes")
    foofiles/mat{intrinsic}.foo:4233   ENSURE(new.dim2==R.dim1,"incompatible sizes")
    foofiles/mat{intrinsic}.foo:4234   ENSURE(new.dim1==L.dim1,"incompatible sizes")

Reached via the CPHF hyperpolarisability path:
    keyword put_scf_dipole_hyperpolarisability
      -> molecule.main.foo:463  .CP:put_SCF_dipole_hyperpolarisa
      -> molecule.cp.foo:1090   put_SCF_dipole_hyperpolarisa
      -> molecule.cp.foo:1099   .:make_SCF_dipole_hyperpol
      -> molecule.cp.foo:1028   make_SCF_dipole_hyperpol

Candidate call sites (all `back_transform_to(new,L,R)` in the CPHF machinery):
    foofiles/molecule.cp.foo:1446   (make_perturbed_densities)  U(:,:,i).back_transform_to(D(:,:,i),MO(:,no+1:),MO(:,1:no))
    foofiles/molecule.cp.foo:1644   (solve_CPHF_equations)      U(:,:,n).back_transform_to(U_ao,MOv,MOo)
    foofiles/molecule.cp.foo:1810   (add_A_times_U)             U(:,:,n).back_transform_to(W,MOv,MOo)

The plain *polarisability* part completed (it also uses solve_CPHF_equations /
add_A_times_U), so the crash is most likely the hyperpolarisability-specific
    molecule.cp.foo:1446  (make_perturbed_densities)
Still to confirm the exact line via backtrace, and to determine whether it is a
translator array-slice issue (MO(:,no+1:) / MO(:,1:no)) or a source dimension
convention. Unlike #1, this looks like a genuine dimension mismatch, so it may also
be wrong (silently) in release.
