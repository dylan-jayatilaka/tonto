!==================================================================
!
! MOLECULE.CP: Coupled-perturbed (CP) procedures
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1996
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation; either
! version 2 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Library General Public License for more details.
!
! You should have received a copy of the GNU Library General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA  02111-1307, USA.
!==================================================================
module MOLECULE_CP_MODULE

#  include "macros"

   include "molecule.cp.use"

   implicit none

   include "molecule.cp.int"


contains

!  ================================
!  Very bad dipole polarisabilities
!  ================================

   subroutine put_Sylvain_Csizmadia_tensors(self)
   ! Put out the Sylvain-Csizmadia polarisability tensors.  This
   ! routine will read the value of the Unsold denominator.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: P,Dx,Dy,Dz
      MAT(REAL,:,:), allocatable :: Qxx,Qyy,Qzz
      MAT(REAL,:,:), allocatable :: Qxy,Qxz,Qyz
      MAT(REAL,:,:), allocatable :: Oxxx,Oyyy,Ozzz
      MAT(REAL,:,:), allocatable :: Oxxy,Oxxz,Oyyx
      MAT(REAL,:,:), allocatable :: Oyyz,Ozzx,Ozzy,Oxyz
      MAT(REAL,3,3) :: alpha,evec,mu2
      MAT3(REAL,3,3,3) :: beta,mu3
      VEC(REAL,3) :: bvec,eval,mu1
      VEC(REAL,10) :: b10vec
      INT :: n,x,y,z,i,u,v,w
      REAL :: delta,dm1
      REAL :: fac1,fac2
      REAL :: fnm1,fnm2
      REAL :: val1,val2,val3
   ENSURE(allocated(self%density_mx),"MOLECULE.CP:put_Sylvain_Csizmadia_tensors ... no density matrix")
   ENSURE(allocated(self%MOs),"MOLECULE.CP:put_Sylvain_Csizmadia_tensors ... no molecular orbitals")
   ENSURE(allocated(self%MOs%r),"MOLECULE.CP:put_Sylvain_Csizmadia_tensors ... no restricted MO'ss")
   ENSURE(self%basis_info_made,"MOLECULE.CP:put_Sylvain_Csizmadia_tensors ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.CP:put_Sylvain_Csizmadia_tensors ... no atom info")
      call flush_(stdout)
      call text_(stdout,"========================================")
      call text_(stdout,"Sylvian-Csizmadia polarisability tensors")
      call text_(stdout,"========================================")

      ! Input Unsold denominator
   ! stdin.read(delta)

      ! 1/N factors ...
      fnm1 = (ONE/self%n_a)
      fnm2 = fnm1*fnm1

      ! Calculate Unsold denominator
      dm1 = ZERO
      do i = 1,self%n_a
         dm1 = dm1+ONE/self%MO_energies%r(i)
      end do
      dm1 = -fnm1*dm1
      fac1 = FOUR*dm1
      fac2 = TWELVE*dm1*dm1

      ! Print Unsold denominator
      delta = ONE/dm1
      call flush_(stdout)
      call show_(stdout,"Unsold denominator/a.u. =",delta)

      ! Allocate space for moment matrix elements, and get them
      n = self%n_bf
      call create_(Dx,n,n); call create_(Dy,n,n); call create_(Dz,n,n)
      call create_(Qxx,n,n); call create_(Qyy,n,n); call create_(Qzz,n,n)
      call create_(Qxy,n,n); call create_(Qxz,n,n); call create_(Qyz,n,n)
      call create_(Oxxx,n,n); call create_(Oyyy,n,n); call create_(Ozzz,n,n)
      call create_(Oxxy,n,n); call create_(Oxxz,n,n)
      call create_(Oyyx,n,n); call create_(Oyyz,n,n)
      call create_(Ozzx,n,n); call create_(Ozzy,n,n)
      call create_(Oxyz,n,n)
      call make_dipole_matrices_(self,Dx,Dy,Dz)
      call make_quadrupole_matrices_(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz)
      call make_octupole_matrices_(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz)

      ! Get the half-occupied density matrix
      call make_r_density_mx_(self)
      call create_(P,self%n_bf,self%n_bf)
      P = HALF*self%density_mx%r

      ! Set the component indices
      x = 1
      y = 2
      z = 3

      ! Moments
      mu1(x) = trace_product_with_(P,Dx)
      mu1(y) = trace_product_with_(P,Dy)
      mu1(z) = trace_product_with_(P,Dz)
      mu2(x,x) = trace_product_with_(P,Qxx)
      mu2(y,y) = trace_product_with_(P,Qyy)
      mu2(z,z) = trace_product_with_(P,Qzz)
      mu2(y,x) = trace_product_with_(P,Qxy)
      mu2(z,x) = trace_product_with_(P,Qxz)
      mu2(z,y) = trace_product_with_(P,Qyz)
      mu2(x,y) = mu2(y,x)
      mu2(x,z) = mu2(z,x)
      mu2(y,z) = mu2(z,y)
      mu3(x,x,x) = trace_product_with_(P,Oxxx)
      mu3(y,y,y) = trace_product_with_(P,Oyyy)
      mu3(z,z,z) = trace_product_with_(P,Ozzz)
      mu3(y,x,x) = trace_product_with_(P,Oxxy)
      mu3(z,x,x) = trace_product_with_(P,Oxxz)
      mu3(y,y,x) = trace_product_with_(P,Oyyx)
      mu3(z,y,x) = trace_product_with_(P,Oxyz)
      mu3(z,z,x) = trace_product_with_(P,Ozzx)
      mu3(z,y,y) = trace_product_with_(P,Oyyz)
      mu3(z,z,y) = trace_product_with_(P,Ozzy)
      call make_symmetric_(mu3)

      ! Evaluate the polarisability
      alpha(x,x) = mu2(x,x)-trace_product_with_(P,Dx,P,Dx)
      alpha(y,y) = mu2(y,y)-trace_product_with_(P,Dy,P,Dy)
      alpha(z,z) = mu2(z,z)-trace_product_with_(P,Dz,P,Dz)
      alpha(y,x) = mu2(x,y)-trace_product_with_(P,Dy,P,Dx)
      alpha(z,x) = mu2(x,z)-trace_product_with_(P,Dz,P,Dx)
      alpha(z,y) = mu2(y,z)-trace_product_with_(P,Dz,P,Dy)
      call symmetric_reflect_(alpha)
      alpha = fac1*alpha
      call flush_(stdout)
      call text_(stdout,"================================")
      call text_(stdout,"Sylvain-Csizmadia polarisability")
      call text_(stdout,"================================")
      call flush_(stdout)
      call put_(stdout,alpha)
      call flush_(stdout)
      call show_(stdout,"Mean polarisability =",THIRD*(alpha(x,x)+alpha(y,y)+alpha(z,z)))
      call flush_(stdout)
      call text_(stdout,"Principal values (au) :")
      call flush_(stdout)
      call solve_symmetric_eigenproblem_(alpha,eval,evec)
      call put_(stdout,eval)
      call flush_(stdout)
      call text_(stdout,"Principal directions :")
      call flush_(stdout)
      call put_(stdout,evec)

!      ! Whitten-Spackman-Jayatilaka polarisability
!      alpha1.create(3,3,.n_a);      alpha1 = ZERO
!      alpha2.create(3,3,.n_a,.n_a); alpha2 = ZERO
!      Pi.create(.n_bf,.n_bf)
!      Tx.create(.n_bf,.n_bf,.n_a)
!      Ty.create(.n_bf,.n_bf,.n_a)
!      Tz.create(.n_bf,.n_bf,.n_a)
!      do i = 1,.n_a
!         MOi => .MOs.r(:,i:i)
!         Pi.to_product_of(MOi,MOi,transpose_b=TRUE)
!         Tx(:,:,i).to_product_of(Pi,Dx)
!         Ty(:,:,i).to_product_of(Pi,Dy)
!         Tz(:,:,i).to_product_of(Pi,Dz)
!      end
!      do i = 1,.n_a
!         MOi => .MOs.r(:,i:i)
!         Pi.to_product_of(MOi,MOi,transpose_b=TRUE)
!         alpha1(x,x,i) = Pi.trace_product_with(Qxx) - Tx(:,:,i).trace_product_with(Tx(:,:,i))
!         alpha1(y,y,i) = Pi.trace_product_with(Qyy) - Ty(:,:,i).trace_product_with(Ty(:,:,i))
!         alpha1(z,z,i) = Pi.trace_product_with(Qzz) - Tz(:,:,i).trace_product_with(Tz(:,:,i))
!         alpha1(y,x,i) = Pi.trace_product_with(Qxy) - Ty(:,:,i).trace_product_with(Tx(:,:,i))
!         alpha1(z,x,i) = Pi.trace_product_with(Qxz) - Tz(:,:,i).trace_product_with(Tx(:,:,i))
!         alpha1(z,y,i) = Pi.trace_product_with(Qyz) - Tz(:,:,i).trace_product_with(Ty(:,:,i))
!         alpha1(:,:,i) = -FOUR*alpha1(:,:,i)/(.MO_energies.r(i))
!         do j = 1,(i-1)
!             alpha2(x,x,i,j) = -Tx(:,:,i).trace_product_with(Tx(:,:,j)) - Tx(:,:,j).trace_product_with(Tx(:,:,i))
!             alpha2(y,y,i,j) = -Ty(:,:,i).trace_product_with(Ty(:,:,j)) - Ty(:,:,j).trace_product_with(Ty(:,:,i))
!             alpha2(z,z,i,j) = -Tz(:,:,i).trace_product_with(Tz(:,:,j)) - Tz(:,:,j).trace_product_with(Tz(:,:,i))
!             alpha2(y,x,i,j) = -Ty(:,:,i).trace_product_with(Tx(:,:,j)) - Ty(:,:,j).trace_product_with(Tx(:,:,i))
!             alpha2(z,x,i,j) = -Tz(:,:,i).trace_product_with(Tx(:,:,j)) - Tz(:,:,j).trace_product_with(Tx(:,:,i))
!             alpha2(z,y,i,j) = -Tz(:,:,i).trace_product_with(Ty(:,:,j)) - Tz(:,:,j).trace_product_with(Ty(:,:,i))
!             alpha2(:,:,i,j) = -TWO*alpha2(:,:,i,j)*(ONE/.MO_energies.r(i) + ONE/.MO_energies.r(j))
!         end
!      end
!      Tz.destroy; Ty.destroy; Tx.destroy
!      Pi.destroy
!
!      stdout.flush
!      alpha = ZERO
!      do i = 1,.n_a
!         alpha = alpha + alpha1(:,:,i)
!         do j = 1,(i-1)
!            alpha = alpha + alpha2(:,:,i,j)
!         end
!      end
!      alpha.symmetric_reflect
!
!      stdout.flush
!      stdout.text("==========================================")
!      stdout.text("Whitten-Spackman-Jayatilaka polarisability")
!      stdout.text("==========================================")
!      stdout.flush
!      stdout.put(alpha)
!      stdout.flush
!      stdout.show("Mean polarisability =",THIRD*(alpha(x,x)+alpha(y,y)+alpha(z,z)))
!
!      stdout.flush
!      stdout.text("Principal values (au) :")
!      stdout.flush
!      alpha.solve_symmetric_eigenproblem(eigenvalues,eigenvectors)
!      stdout.put(eigenvalues)
!      stdout.flush
!      stdout.text("Principal directions :")
!      stdout.flush
!      stdout.put(eigenvectors)
!
!      stdout.flush
!      stdout.text("WSJ first polarisability (au):")
!      stdout.flush
!      stdout.put(alpha)
!      stdout.flush
!      stdout.show("Mean polarisability =",THIRD*(alpha(x,x)+alpha(y,y)+alpha(z,z)))
!
!      alpha2.destroy
!      alpha1.destroy
!

!                  + P.trace_product_with(Dx,P,Dx,P,Dx)

!                  + P.trace_product_with(Dy,P,Dy,P,Dy)

!                  + P.trace_product_with(Dz,P,Dz,P,Dz)

!                  - P.trace_product_with(Dy,P,Qxx) + P.trace_product_with(Dy,P,Dx,P,Dx)

!                  - P.trace_product_with(Dz,P,Qxx) + P.trace_product_with(Dz,P,Dx,P,Dx)

!                  - P.trace_product_with(Dx,P,Qyy) + P.trace_product_with(Dy,P,Dy,P,Dx)

!                  + P.trace_product_with(Dz,P,Dy,P,Dx)

!                  - P.trace_product_with(Dx,P,Qzz) + P.trace_product_with(Dz,P,Dz,P,Dx)

!                  - P.trace_product_with(Dz,P,Qyy) + P.trace_product_with(Dz,P,Dy,P,Dy)

!                  - P.trace_product_with(Dy,P,Qzz) + P.trace_product_with(Dz,P,Dz,P,Dy)
!      beta.make_symmetric
!
!      fac = 12.0d0/(delta*delta)
!      beta = fac*beta
!
!      stdout.flush
!      stdout.text("WSJ Dipole hyperpolarisability (au):")
!      stdout.flush
!      stdout.put(beta)
!
!      stdout.flush
!      stdout.text("Dipole hyperpolarisability - unique values (au):")
!      stdout.flush
!      stdout.text("The order is xxx,xxy,xyy,yyy,xxz,xyz,yyz,xzz,yzz,zzz")
!      stdout.flush
!
!      b10vec( 1) = beta(1,1,1)
!      b10vec( 2) = beta(1,1,2)
!      b10vec( 3) = beta(1,2,2)
!      b10vec( 4) = beta(2,2,2)
!      b10vec( 5) = beta(1,1,3)
!      b10vec( 6) = beta(1,2,3)
!      b10vec( 7) = beta(2,2,3)
!      b10vec( 8) = beta(1,3,3)
!      b10vec( 9) = beta(2,3,3)
!      b10vec(10) = beta(3,3,3)
!
!      stdout.put(b10vec)
!
!      stdout.flush
!      stdout.text("Vector dipole hyperpolarisability (au):")
!      stdout.flush
!      beta.set_vector_hyperpolarisability(bvec)
!
!      stdout.put(bvec)

      ! =====================
      ! Jayatilaka-Jha-Munshi
      ! =====================
      do u = 1,3
         do v = 1,3
            alpha(u,v) = mu2(u,v)-fnm1*mu1(u)*mu1(v)
         end do
      end do
      alpha = fac1*alpha
      do u = 1,3
         do v = 1,3
            do w = 1,3
               val1 = mu3(u,v,w)
               val2 = mu1(u)*mu2(v,w)+mu1(v)*mu2(w,u)+mu1(w)*mu2(u,v)
               val3 = mu1(u)*mu1(v)*mu1(w)
               beta(u,v,w) = val1-fnm1*val2+TWO*fnm2*val3
            end do
         end do
      end do
      beta = fac2*beta
      call flush_(stdout)
      call text_(stdout,"==========================================")
      call text_(stdout,"Jayatilaka-Jha-Munshi (JJM) polarisability")
      call text_(stdout,"==========================================")
      call flush_(stdout)
      call text_(stdout,"mu1:")
      call put_(stdout,mu1)
      call flush_(stdout)
      call text_(stdout,"mu2:")
      call put_(stdout,mu2)
      call flush_(stdout)
      call text_(stdout,"JJM first polarisability:")
      call flush_(stdout)
      call flush_(stdout)
      call put_(stdout,alpha)
      call flush_(stdout)
      call show_(stdout,"JJM mean polarisability =",THIRD*(alpha(x,x)+alpha(y,y)+alpha(z,z)))
      call flush_(stdout)
      call text_(stdout,"JJM Dipole hyperpolarisability (JJM):")
      call flush_(stdout)
      call put_(stdout,beta)
      call flush_(stdout)
      call text_(stdout,"JJM Dipole hyperpolarisability - unique values:")
      call flush_(stdout)
      call text_(stdout,"The order is xxx,xxy,xyy,yyy,xxz,xyz,yyz,xzz,yzz,zzz")
      call flush_(stdout)
      b10vec(1) = beta(1,1,1)
      b10vec(2) = beta(1,1,2)
      b10vec(3) = beta(1,2,2)
      b10vec(4) = beta(2,2,2)
      b10vec(5) = beta(1,1,3)
      b10vec(6) = beta(1,2,3)
      b10vec(7) = beta(2,2,3)
      b10vec(8) = beta(1,3,3)
      b10vec(9) = beta(2,3,3)
      b10vec(10) = beta(3,3,3)
      call put_(stdout,b10vec)
      call flush_(stdout)
      call text_(stdout,"JJM Vector dipole hyperpolarisability:")
      call flush_(stdout)
      call set_vector_hyperpolarisability_(beta,bvec)
      call put_(stdout,bvec)
      call flush_(stdout)
      call show_(stdout,"JJM |beta| =",norm_(bvec))
      call destroy_(P)
      call destroy_(Oxyz)
      call destroy_(Ozzy); call destroy_(Ozzx)
      call destroy_(Oyyz); call destroy_(Oyyx)
      call destroy_(Oxxz); call destroy_(Oxxy)
      call destroy_(Ozzz); call destroy_(Oyyy); call destroy_(Oxxx)
      call destroy_(Qyz); call destroy_(Qxz); call destroy_(Qxy)
      call destroy_(Qzz); call destroy_(Qyy); call destroy_(Qxx)
      call destroy_(Dz); call destroy_(Dy); call destroy_(Dx)
   end subroutine

   subroutine put_SOS_dipole_polarisability(self)
   ! Calculate the dipole polarisability using the sum-over-states
   ! (SOS) formula.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), allocatable :: D
      VEC(REAL,:), allocatable :: e
      VEC(REAL,3) :: eval
      MAT(REAL,3,3) :: alpha,evec
      INT :: l,m,a,i
      REAL :: val
   ENSURE(allocated(self%MOs),"MOLECULE.CP:put_SOS_dipole_polarisability ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.CP:put_SOS_dipole_polarisability ... no restricted MO's")
   ENSURE(allocated(self%SCF_data),"MOLECULE.CP:put_SOS_dipole_polarisability ... no scf data")
   ENSURE(self%SCF_data%SCF_kind=="rhf","MOLECULE.CP:put_SOS_dipole_polarisability ... SOS polarisability only for RHF case")
   ENSURE(self%basis_info_made,"MOLECULE.CP:put_SOS_dipole_polarisability ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.CP:put_SOS_dipole_polarisability ... no atom info")
      e = self%MO_energies%r

      ! Get the dipole moment matrices in the AO basis.
      call create_(D,self%n_bf,self%n_bf,3)
      call make_dipole_matrices_(self,D(:,:,1),D(:,:,2),D(:,:,3))

      ! Change dipole matrices from AO -> MO basis
      call change_basis_using_(D(:,:,1),self%MOs%r)
      call change_basis_using_(D(:,:,2),self%MOs%r)
      call change_basis_using_(D(:,:,3),self%MOs%r)

      ! Do the calculation
      alpha = ZERO
      do l = 1,3
         do m = 1,l
            val = ZERO
            do a = self%n_a+1,self%n_bf
               do i = 1,self%n_a
                  val = val+(D(a,i,l)*D(a,i,m))/(e(a)-e(i))
               end do
            end do
            alpha(l,m) = val
         end do
      end do
      call destroy_(D)
      alpha = FOUR*alpha
      call symmetric_reflect_(alpha)
      call destroy_(self%dipole_polarisability)
      call create_(self%dipole_polarisability,3,3)  ! leaky here
      self%dipole_polarisability = alpha
      call flush_(stdout)
      call text_(stdout,"=====================================")
      call text_(stdout,"Sum-over-states dipole polarisability")
      call text_(stdout,"=====================================")
      call flush_(stdout)
      call text_(stdout,". In atomic units")
      call flush_(stdout)
      call put_(stdout,alpha)
      call flush_(stdout)
      call show_(stdout,"Mean polarisability =",THIRD*(alpha(1,1)+alpha(2,2)+alpha(3,3)))
      call flush_(stdout)
      call text_(stdout,"Principal values:")
      call flush_(stdout)
      call solve_symmetric_eigenproblem_(alpha,eval,evec)
      call put_(stdout,eval)
      call flush_(stdout)
      call text_(stdout,"Principal directions:")
      call flush_(stdout)
      call put_(stdout,evec)
   end subroutine


!  =================================
!  Unit-cell dipole polarizabilities
!  =================================

   subroutine put_UC_dipole_polarisability(self)
   ! Calculate the unit cell dipole polarisability using Coupled
   ! Perturbed Hartree-Fock theory. It assumes that the crystal data
   ! is present, and that the polarisabilities for each molecules can
   ! be added up.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,3,3) :: alpha
   ENSURE(allocated(self%dipole_polarisability),"MOLECULE.CP:put_UC_dipole_polarisability ... no dipole_polarisability")
   ENSURE(allocated(self%atom),"MOLECULE.CP:put_UC_dipole_polarisability ... no atom data")
   ENSURE(allocated(self%crystal),"MOLECULE.CP:put_UC_dipole_polarisability ... no crystal data")

      ! Make it
      call make_UC_dipole_polarisability_(self,alpha)

      ! Print the polarisability
      call flush_(stdout)
      call text_(stdout,"===============================")
      call text_(stdout,"Unit cell dipole polarisability")
      call text_(stdout,"===============================")
      call put_dipole_polarisability_(self,alpha)
   end subroutine

   subroutine make_UC_dipole_polarisability(self,uc_alpha)
   ! Calculate the unit cell dipole polarisability using Coupled
   ! Perturbed Hartree-Fock theory. It assumes that the crystal data
   ! is present, and that the polarisabilities for each molecules can
   ! be added up.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,3,3), OUT :: uc_alpha
   ENSURE(allocated(self%dipole_polarisability),"MOLECULE.CP:make_UC_dipole_polarisability ... no dipole_polarisability")
   ENSURE(allocated(self%atom),"MOLECULE.CP:make_UC_dipole_polarisability ... no atom data")
   ENSURE(allocated(self%crystal),"MOLECULE.CP:make_UC_dipole_polarisability ... no crystal data")
   ENSURE(all(self%crystal%frag_atom_for_asym_atom/=0),"MOLECULE.CP:make_UC_dipole_polarisability ... some asym atoms not found")

      ! Molecular polarisability
      uc_alpha = self%dipole_polarisability

      ! Make the unit cell cluster
      call destroy_(self%cluster)
      call create_(self%cluster)
      call set_defaults_(self%cluster,self%crystal)
      call set_generation_method_(self%cluster,"unit_cell")
      call set_defragment_(self%cluster,FALSE)
      call set_radius_(self%cluster,ZERO)
      call update_info_(self%cluster)  ! NOT PURE
      call add_UC_tensors_(self%cluster,uc_alpha)
   end subroutine

   subroutine put_UC_dipole_hyperpolar(self)
   ! Calculate the unit cell dipole hyperpolarisability using Coupled
   ! Perturbed Hartree-Fock theory. It assumes that the crystal data
   ! is present, and that the polarisabilities for each molecules can
   ! be added up.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,3,3,3) :: beta
      VEC(REAL,3) :: bvec
   ENSURE(allocated(self%dipole_hyperpolarisability),"MOLECULE.CP:put_UC_dipole_hyperpolar ... no dipole_hyperpolarisability")
   ENSURE(allocated(self%atom),"MOLECULE.CP:put_UC_dipole_hyperpolar ... no atom data")
   ENSURE(allocated(self%crystal),"MOLECULE.CP:put_UC_dipole_hyperpolar ... no crystal data")
   ENSURE(all(self%crystal%frag_atom_for_asym_atom/=0),"MOLECULE.CP:put_UC_dipole_hyperpolar ... some asym atoms not found")
      beta = self%dipole_hyperpolarisability
      call destroy_(self%cluster)
      call update_fragment_info_(self%crystal,self%atom)
      call create_(self%cluster)
      call set_defaults_(self%cluster,self%crystal)
      call set_generation_method_(self%cluster,"unit_cell")
      call set_defragment_(self%cluster,FALSE)
      call set_radius_(self%cluster,ZERO)
      call update_info_(self%cluster)
      call add_UC_tensors_(self%cluster,beta)
      call flush_(stdout)
      call text_(stdout,"====================================")
      call text_(stdout,"Unit cell dipole hyperpolarisability")
      call text_(stdout,"====================================")
      call flush_(stdout)
      call text_(stdout,". In atomic units")
      call flush_(stdout)
      call put_(stdout,beta)
      call set_vector_hyperpolarisability_(beta,bvec)
      call flush_(stdout)
      call text_(stdout,"Vector dipole hyperpolarisability:")
      call flush_(stdout)
      call put_(stdout,bvec)
   end subroutine


!  ============================
!  Unit-cell refractive indices
!  ============================

   subroutine put_UC_refractive_index(self)
   ! Calculate the unit cell refractive index from the atomic charge
   ! and dipole polarisabilities.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,3,3) :: chi1
      MAT3(REAL,3,3,3) :: chi2
   ENSURE(allocated(self%dipole_polarisability),"MOLECULE.CP:put_UC_refractive_index ... no dipole_polarisability")
   ENSURE(allocated(self%atom),"MOLECULE.CP:put_UC_refractive_index ... no atom data")
   ENSURE(allocated(self%crystal),"MOLECULE.CP:put_UC_refractive_index ... no crystal data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.CP:put_UC_refractive_index ... no crystal fragment info")
   ENSURE(all(self%crystal%frag_atom_for_asym_atom/=0),"MOLECULE.CP:put_UC_refractive_index ... some asym atoms not found")

      ! Make sure asymmetric unit is generated from the fragment
      ! (Also it assigns atomic charges, polarisabilities to asymmetric unit)
      call destroy_asymmetric_unit_(self%crystal)
      call update_fragment_info_(self%crystal,self%atom)
      DIE_IF(NOT allocated(self%crystal%asymmetric_unit_atom),"MOLECULE.CP:put_UC_refractive_index ... missing asym atoms")

      ! Make the cluster
      call destroy_(self%cluster)
      call create_(self%cluster)
      call set_defaults_(self%cluster,self%crystal)
      call update_info_(self%cluster)  ! leaky here
      call flush_(stdout)
      call text_(stdout,"=================================================")
      call text_(stdout,"Unit cell susceptibilities and refractive indices")
      call text_(stdout,"=================================================")
      call flush_(stdout)
      call text_(stdout,"=== Naive method (no local field effects) ===")
      call make_chi1_naive_(self%cluster,chi1,self%dipole_polarisability)
      call put_refractive_indices_(self,chi1)
      call flush_(stdout)
      call text_(stdout,"=== ALFFA method (single site per molecule, at COM) ===")
      call make_chi1_ALFFA_(self%cluster,chi1,self%dipole_polarisability)
      call put_refractive_indices_(self,chi1)
      call flush_(stdout)
      call text_(stdout,"=== RLFTn method (multiple sites per molecule) ===")
      call make_chi1_RLFTn_(self%cluster,chi1,self%dipole_polarisability)
      call put_refractive_indices_(self,chi1)
      call flush_(stdout)
      call text_(stdout,"=== Spackman's RLFTn method (L tensor averaged over sites) ===")
      call make_chi1_RLFTn_av_L_(self%cluster,chi1,self%dipole_polarisability)
      call put_refractive_indices_(self,chi1)
      call flush_(stdout)
      call text_(stdout,"======================================")
      call text_(stdout,"Unit cell second susceptibility chi(2)")
      call text_(stdout,"======================================")
      call make_chi2_RLFTn_av_L_(self%cluster,chi2,self%dipole_hyperpolarisability,self%dipole_polarisability)
      call put_chi2(chi2,chi1)
   end subroutine

   subroutine put_UC_chi2(self)
   ! This code was ressurected and modified from the svn repository
   !  extracted at {2007-07-01} ... It is supposed to represent
   ! Munshi's results
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,3,3) :: alpha
      MAT(REAL,3,3) :: uc_alpha
      MAT(REAL,3,3) :: chi1
      MAT(REAL,3,3) :: chi1_naive
      MAT(REAL,3,3) :: chi1_com
      MAT(REAL,3,3) :: chi1_nonH
      MAT(REAL,3,3) :: chi1_nc
      MAT(REAL,3,3) :: chi1_do
      MAT3(REAL,3,3,3) :: beta
      MAT3(REAL,3,3,3) :: chi2_nonH
   ENSURE(self%partition_info_made,"MOLECULE.CP:put_UC_chi2 ... no atom moments")
   ENSURE(self%atomic_polarisabilities_made,"MOLECULE.CP:put_UC_chi2 ... no atomic polarisabilities")
   ENSURE(allocated(self%dipole_polarisability),"MOLECULE.CP:put_UC_chi2 ... no dipole_polarisability")
   ENSURE(allocated(self%dipole_hyperpolarisability),"MOLECULE.CP:put_UC_chi2 ... no dipole_hyperpolarisability")
   ENSURE(allocated(self%atom),"MOLECULE.CP:put_UC_chi2 ... no atom data")
   ENSURE(allocated(self%crystal),"MOLECULE.CP:put_UC_chi2 ... no crystal data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.CP:put_UC_chi2 ... no crystal fragment info")
   ENSURE(all(self%crystal%frag_atom_for_asym_atom/=0),"MOLECULE.CP:put_UC_chi2 ... some asym atoms not found")

      ! Get molecular alpha and beta
      alpha = self%dipole_polarisability
      beta = self%dipole_hyperpolarisability

      ! Make the susceptibilities
      call destroy_(self%cluster)
      call create_(self%cluster)
      call set_defaults_(self%cluster,self%crystal)
      call update_info_(self%cluster)  ! leaky here
      call make_susceptibility2_(self%cluster,alpha,beta,uc_alpha,chi1_naive,chi1_com,chi1_nonH,chi2_nonH,chi1_nc,chi1_do,chi1)
      call flush_(stdout)
      call text_(stdout,"===========================================")
      call text_(stdout,"Unit cell first and second susceptibilities")
      call text_(stdout,"===========================================")
      call flush_(stdout)
      call text_(stdout,". This code uses a distrubted polarizability formalism")
      call text_(stdout,". It was used to produce unlublished results by P. Munshi in 2007")
      call flush_(stdout)
      call text_(stdout,"== Unit cell alpha==")
      call flush_(stdout)
      call put_(stdout,uc_alpha)
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"=== Naive method (no local field effects) ===")
      call put_refractive_indices_(self,chi1_naive)
      call flush_(stdout)
      call text_(stdout,"=== ALFFA method (single site per molecule, at COM) ===")
      call put_refractive_indices_(self,chi1_com)
      call flush_(stdout)
      call text_(stdout,"=== RLFTn method (multiple sites per molecule) ===")
      call put_refractive_indices_(self,chi1_nonH)
      call flush_(stdout)
      call text_(stdout,"=== chi2 RLFTn method (multiple sites per molecule) ===")
      call put_chi2(chi2_nonH,chi1_nonH)

!      stdout.flush
!      stdout.text("Unit cell distributed no charge susceptibility ====(DAP-q)====")
!      .put_refractive_indices(chi1_nc)
!
!      stdout.flush
!      stdout.text("Unit cell distributed dipole only susceptibility ====(DAD)======")
!      .put_refractive_indices(chi1_do)
!
!      stdout.flush
!      stdout.text("Unit cell distributed susceptibility ====(DAP)======")
!      .put_refractive_indices(chi1)
   end subroutine

   subroutine put_refractive_indices(self,chi1)
   ! Put out the permittivity and unit cell refractive index
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,3,3), OUT :: chi1
      VEC(REAL,3) :: eval
      MAT(REAL,3,3) :: eps,evec

      ! Print out the susceptibility
      call flush_(stdout)
      call text_(stdout,"chi1:")
      call flush_(stdout)
      call put_(stdout,chi1)
      call flush_(stdout)

      ! Work out the permittivity
      call to_unit_mx_(eps)
      eps = eps+chi1

      ! Get the optic axes
      call solve_symmetric_eigenproblem_(eps,eval,evec)

      ! Print out the refractive indices
      call flush_(stdout)
      call text_(stdout,"Refractive indices")
      call flush_(stdout)
      call put_(stdout,sqrt(eval))

      ! Optic axes ...
      call flush_(stdout)
      call text_(stdout,"Principal directions (columns):")
      call flush_(stdout)
      call put_(stdout,evec)
      if (allocated(self%crystal)) then
         call flush_(stdout)
         call text_(stdout,"Principal directions in crystal axis system:")
         call flush_(stdout)
         call change_basis_using_(evec,self%crystal%unit_cell%inverse_mx,self%crystal%unit_cell%direct_mx)
         call put_(stdout,evec)
      end if
   end subroutine

   subroutine put_chi2(chi2,chi1)
   ! Put out the chi2
      MAT3(REAL,3,3,3), OUT :: chi2
      MAT(REAL,3,3), OUT :: chi1
      VEC(REAL,10) :: v10
      VEC(REAL,3) :: v3
      VEC(REAL,3) :: eval
      MAT(REAL,3,3) :: evec
      MAT(REAL,3,3) :: eps
      MAT3(REAL,3,3,3) :: beta

      ! Print out the 2nd susceptibility
      call flush_(stdout)
      call text_(stdout,"== chi2 in the xyz frame ==")
      call flush_(stdout)
      call text_(stdout,". Atomic units are used")
      call text_(stdout,". Element order is: xxx,yxx,yyx,yyy,zxx,zyx,zyy,zzx,zzy,zzz")
      call flush_(stdout)
      call put_(stdout,chi2)
      call compress_to_pyramid_(chi2,v10)
      call flush_(stdout)
      call text_(stdout,"Independent chi2 elements:")
      call put_(stdout,v10)
      call set_vector_hyperpolarisability_(chi2,v3)
      call flush_(stdout)
      call text_(stdout,"Vector componenets of chi2:")
      call flush_(stdout)
      call put_(stdout,v3)
      call flush_(stdout)
      call show_(stdout,"|chi2| = ",norm_(v3))

      ! Work out the permittivity
      call to_unit_mx_(eps)
      eps = eps+chi1

      ! Get the optic axes
      call solve_symmetric_eigenproblem_(eps,eval,evec)

      ! Beta in the optic axis frame
      beta = chi2
      call change_basis_using_(beta,evec)

      ! chi2 in the optic axis frame
      call flush_(stdout)
      call text_(stdout,"== chi2 in the optic axis frame ==")
      call flush_(stdout)
      call put_(stdout,chi2)
      call compress_to_pyramid_(chi2,v10)
      call flush_(stdout)
      call text_(stdout,"Independent chi2 elements:")
      call text_(stdout,"The order of elements is: xxx,yxx,yyx,yyy,zxx,zyx,zyy,zzx,zzy,zzz")
      call put_(stdout,v10)
      call compress_to_pyramid_(chi2,v10)
      call flush_(stdout)
      call text_(stdout,"chi2 in the optic axis frame - as vector:")
      call put_(stdout,v10)
      call set_vector_hyperpolarisability_(chi2,v3)
      call flush_(stdout)
      call text_(stdout,"Vector componenets of chi2:")
      call flush_(stdout)
      call put_(stdout,v3)
      call flush_(stdout)
      call show_(stdout,"|chi2| = ",norm_(v3))
   end subroutine


!  =======================
!  Dipole polarizabilities
!  =======================

   subroutine make_SCF_dipole_polarisability(self)
   ! Make the SCF electric dipole polarisability
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,3,3) :: alpha
      MAT3(REAL,:,:,:), allocatable :: U,D_mo
      INT :: nv,no,l,m,a,i
      REAL :: val

      ! Make the U matrices if needed
      if (NOT allocated(self%U_electric_dipole)) call make_U_electric_dipole_(self)  ! NOT PURE
      U = self%U_electric_dipole

      ! Get the dipole moment matrices in the AO basis.
      no = self%n_a
      nv = self%n_bf-self%n_a
      call create_(D_mo,nv,no,3)
      call get_VO_MO_dipole_matrices_(self,D_mo)
      D_mo = -D_mo

      ! Calculate polarisability using U and D ...
      alpha = ZERO
      do l = 1,3
         do m = 1,l
            val = ZERO
            do a = 1,nv
               do i = 1,no
                  val = val+D_mo(a,i,l)*U(a,i,m)
               end do
            end do
            alpha(l,m) = val
         end do
      end do
      alpha = FOUR*alpha
      call symmetric_reflect_(alpha)

      ! Assign
      self%dipole_polarisability = alpha

      ! Clean
      call destroy_(D_mo)
   end subroutine

   subroutine put_SCF_dipole_polarisability(self)
   ! Put the SCF polarisability
      type(MOLECULE_TYPE), INOUT :: self

      ! Make the polarisability .....
      call make_SCF_dipole_polarisability_(self)  ! INOUT

      ! Print out the answer
      call flush_(stdout)
      call text_(stdout,"=========================")
      call text_(stdout,"SCF dipole polarizability")
      call text_(stdout,"=========================")
      call put_dipole_polarisability_(self,self%dipole_polarisability)

      ! Biggest contributions
      call put_big_alpha_terms_(self)
   end subroutine

   subroutine put_big_alpha_terms(self)
   ! Put the main contributions to the SCF polarisability
   ! Tidied from mjt === dylan
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), allocatable :: U,D_mo
      VEC(REAL,:), allocatable :: alpha_bar,alpha_abs
      VEC(INT,:), allocatable :: indices
      INT :: a,i,ai,l,nv,no
      REAL :: val

      ! Make the U matrices/dipole integrals if needed
      if (NOT allocated(self%U_electric_dipole)) call make_U_electric_dipole_(self)
      U = self%U_electric_dipole

      ! Get the virtual-occupied MO dipole moment matrices
      no = self%n_a
      nv = self%n_bf-no
      call create_(D_mo,nv,no,3)
      call get_VO_MO_dipole_matrices_(self,D_mo)
      D_mo = -D_mo

      ! Get the individual orbital contributions to the
      ! mean polarisabilitity
      call create_(alpha_bar,nv*no)
      ai = 0
      do i = 1,no
         do a = 1,nv
            val = ZERO
            do l = 1,3
               val = val+D_mo(a,i,l)*U(a,i,l)
            end do
            ai = ai+1
            alpha_bar(ai) = val
         end do
      end do
      alpha_bar = THIRD*FOUR*alpha_bar
      call destroy_(D_mo)

      ! Get largest contributions in "indices"
      call create_(indices,nv*no)
      call create_(alpha_abs,nv*no)
      alpha_abs = abs(alpha_bar)
      call quick_sort_(alpha_abs,indices,decreasing_order=TRUE)

      ! Print out the largest contributions
      call flush_(stdout)
      call text_(stdout,"Mean dipole polarisability, top orbital-pair contributions:")
      call flush_(stdout)
      call show_(stdout,"Check: mean polarisability =",sum_elements_(alpha_bar))
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=1)
      call put_(stdout,"Rank",int_width=TRUE)
      call put_(stdout,"a",int_width=TRUE)
      call put_(stdout,"i",int_width=TRUE)
      call put_(stdout,"alpha_ai")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=1)
      do l = 1,5  ! take the top 5 results
         ai = indices(l)  ! get the index
         a = mod(ai,nv)
         i = (ai-1)/nv+1
         call put_(stdout,l)
         call put_(stdout,a)
         call put_(stdout,i)
         call put_(stdout,alpha_bar(ai))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=3,real_fields=1)
      call destroy_(alpha_abs)
      call destroy_(indices)
      call destroy_(alpha_bar)
   end subroutine

   subroutine put_dipole_polarisability(self,alpha)
   ! Put the dipole polarisability
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,3,3), IN :: alpha
      VEC(REAL,3) :: eval
      MAT(REAL,3,3) :: evec
      REAL :: mean

      ! Print out the answer
      call flush_(stdout)
      call text_(stdout,". In atomic units")
      call put_(stdout,alpha)
      call flush_(stdout)
      mean = THIRD*(alpha(1,1)+alpha(2,2)+alpha(3,3))
      call show_(stdout,"Mean polarisability =",mean)

      ! Get principal axes
      call solve_symmetric_eigenproblem_(alpha,eval,evec)

      ! Put in descending order
      call reverse_order_(eval)
      call reverse_column_order_(evec)

      ! Put eigenvalues
      call flush_(stdout)
      call text_(stdout,"Principal values (au) :")
      call flush_(stdout)
      call put_(stdout,eval)

      ! Optic axes ...
      call flush_(stdout)
      call text_(stdout,"Principal directions (columns):")
      call flush_(stdout)
      call put_(stdout,evec)
      if (allocated(self%crystal)) then
         call flush_(stdout)
         call text_(stdout,"Principal directions in crystal axis system :")
         call flush_(stdout)
         call change_basis_using_(evec,self%crystal%unit_cell%inverse_mx,self%crystal%unit_cell%direct_mx)
         call put_(stdout,evec)
      end if
   end subroutine


!  ============================
!  Dipole hyperpolarizabilities
!  ============================

   subroutine make_SCF_dipole_hyperpol(self)
   ! Calculate scf dipole hyperpolarisability
   ! Taken from Colwell et al, CPL 210, p. 261 (1993)
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,3,3,3) :: beta
      MAT3(REAL,:,:,:), allocatable :: U,F
      MAT(REAL,:,:), allocatable :: xi_vv,xi_oo
      INT :: no,nv,l,m,n
      REAL :: val,val1,val2

      ! Make the U matrices/dipole integrals if needed
      if (NOT allocated(self%U_electric_dipole)) call make_U_electric_dipole_(self)
      U = self%U_electric_dipole

      ! Make F(k). We don't need the U term in eqn (29)
      ! See Colwell et al, CPL 210, p. 261 (1993)
      call create_(F,self%n_bf,self%n_bf,3)
      call get_MO_dipole_matrices_(self,F)
      call add_A_times_U_(self,F,U)

      ! Make beta
      no = self%n_a
      nv = self%n_bf-no
      call create_(xi_oo,no,no)
      call create_(xi_vv,nv,nv)
      beta = ZERO
      do m = 1,3
         do n = 1,3
            call to_product_of_(xi_vv,U(:,:,m),U(:,:,n),transpose_b=TRUE)
            call plus_product_of_(xi_vv,U(:,:,n),U(:,:,m),transpose_b=TRUE)
            call to_product_of_(xi_oo,U(:,:,m),U(:,:,n),transpose_a=TRUE)
            call plus_product_of_(xi_oo,U(:,:,n),U(:,:,m),transpose_a=TRUE)
            do l = 1,3
               val1 = TWO*trace_product_with_(F(1:no,1:no,l),xi_oo)
               val2 = -TWO*trace_product_with_(F(no+1:,no+1:,l),xi_vv)
               val = val1+val2
               beta(l,m,n) = beta(l,m,n)+val
               beta(m,n,l) = beta(m,n,l)+val
               beta(n,l,m) = beta(n,l,m)+val
            end do
         end do
      end do

      ! Create dipole hyperpolarisability
      call destroy_(self%dipole_hyperpolarisability)
      call create_(self%dipole_hyperpolarisability,3,3,3)  ! leaky here
      self%dipole_hyperpolarisability = beta

      ! Clean
      call destroy_(xi_vv)
      call destroy_(xi_oo)
      call destroy_(F)
   end subroutine

   subroutine put_SCF_dipole_hyperpolarisa(self)
   ! Calculate the polarisability using Coupled Perturbed 
   ! Hartree-Fock theory.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,3,3,3) :: beta
      VEC(REAL,3) :: bvec

      ! Do it .....
      call make_SCF_dipole_hyperpol_(self)
      beta = self%dipole_hyperpolarisability
      call set_vector_hyperpolarisability_(beta,bvec)

      ! Print the results
      call flush_(stdout)
      call text_(stdout,"==========================")
      call text_(stdout,"Dipole hyperpolarisability")
      call text_(stdout,"==========================")
      call flush_(stdout)
      call text_(stdout,". In atomic units")
      call put_(stdout,beta)
      call flush_(stdout)
      call text_(stdout,"Vector dipole hyperpolarisability (au):")
      call flush_(stdout)
      call put_(stdout,bvec)
      call flush_(stdout)
      call show_(stdout," |beta| = ",norm_(bvec))
   end subroutine


!  =======================
!  Atomic polarizabilities
!  =======================

   subroutine make_SCF_atomic_polarisability(self)
   ! Calculate the polarisability using Coupled Perturbed
   ! Hartree-Fock theory.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), allocatable :: D
      MAT3(REAL,:,:,:), allocatable :: U
      MAT(REAL,:,:), allocatable :: pt,NO
      VEC(REAL,:), allocatable :: wt,Wai,rhoi,occ
      VEC(REAL,3) :: pos
      INT :: n_pt,i,a
   ENSURE(allocated(self%MOs),"MOLECULE.CP:make_SCF_atomic_polarisability ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.CP:make_SCF_atomic_polarisability ... no restricted MO's")

      ! Make the U matrices/dipole integrals if needed
      if (NOT allocated(self%U_electric_dipole)) call make_U_electric_dipole_(self)
      U = self%U_electric_dipole

      ! Make the AO perturbed density matrices
      call create_(D,self%n_bf,self%n_bf,3)
      call make_perturbed_densities_(self,D,U,self%MOs%r,self%n_a)

      ! Ensure ANO's and becke_grid made (leaky)
      call make_atom_partition_info_(self)

      ! Make the becke grid integration grid (pt,wt)
      call make_grid_(self%becke_grid,pt,wt)  ! <<<<<<<< leaky

      ! Integrate the polarisabilities
      n_pt = size(pt,1)
      call create_(NO,self%n_bf,self%n_bf)
      call create_(occ,self%n_bf)
      call create_(Wai,n_pt)
      call create_(rhoi,n_pt)

      ! Make overlap matrix (for NO's)
      call make_overlap_mx_(self)  ! NOT PURE
      do i = 1,3
         call make_r_NOs_(self,NO,occ,D(:,:,i))  ! NOT PURE
         call make_ED_grid_r_v1_(self,rhoi,pt,NO,occ)
         rhoi = rhoi*wt
         do a = 1,self%n_atom
            call make_stockholder_atom_weight_(self,Wai,a,pt,self%overlapping_atoms_for_atom(a)%element)
            Wai = -rhoi*Wai
            pos = self%atom(a)%position
            self%atom(a)%charge_polarisability(i) = sum(Wai)
            self%atom(a)%dipole_polarisability(1,i) = sum(Wai*(pt(:,1)-pos(1)))
            self%atom(a)%dipole_polarisability(2,i) = sum(Wai*(pt(:,2)-pos(2)))
            self%atom(a)%dipole_polarisability(3,i) = sum(Wai*(pt(:,3)-pos(3)))
         end do
      end do

      ! Clean
      call destroy_(rhoi); call destroy_(Wai)
      call destroy_(occ); call destroy_(NO)
      call destroy_(wt); call destroy_(pt)
      call destroy_(D)
      self%atomic_polarisabilities_made = TRUE
   end subroutine

   subroutine put_SCF_atomic_polarisability(self)
   ! Calculate the polarisability using Coupled Perturbed 
   ! Hartree-Fock theory.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,3,3) :: dpol,alpha,alpha_q,alpha_m
      VEC(REAL,3) :: qpol
      INT :: a
      REAL :: asym
   ENSURE(allocated(self%MOs),"MOLECULE.CP:put_SCF_atomic_polarisability ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.CP:put_SCF_atomic_polarisability ... no restricted MO's")

      ! Make it
      if (NOT self%atomic_polarisabilities_made) then
         call make_SCF_atomic_polarisability_(self)
      end if

      ! Write results in au
      alpha_q = ZERO
      call flush_(stdout)
      call text_(stdout,"=================================")
      call text_(stdout,"Hirshfeld atomic polarisabilities")
      call text_(stdout,"=================================")
      call flush_(stdout)
      call text_(stdout,". In atomic units")
      call flush_(stdout)
      call text_(stdout,"Monopole polarisabiities:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=3)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"q_x")
      call put_(stdout,"q_y")
      call put_(stdout,"q_z")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=3)
      do a = 1,self%n_atom
         call put_(stdout,self%atom(a)%label,int_width=TRUE)
         qpol = self%atom(a)%charge_polarisability
         call put_(stdout,qpol(1))
         call put_(stdout,qpol(2))
         call put_(stdout,qpol(3))
         call flush_(stdout)
         alpha_q(:,1) = alpha_q(:,1)+self%atom(a)%position*qpol(1)
         alpha_q(:,2) = alpha_q(:,2)+self%atom(a)%position*qpol(2)
         alpha_q(:,3) = alpha_q(:,3)+self%atom(a)%position*qpol(3)
      end do
      call dash_(stdout,int_fields=1,real_fields=3)
      call flush_(stdout)

      ! Write results in au
      alpha_m = ZERO
      asym = ZERO
      call flush_(stdout)
      call text_(stdout,"Dipole polarisabiities:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=3)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"a_:x")
      call put_(stdout,"a_:y")
      call put_(stdout,"a_:z")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=3)
      do a = 1,self%n_atom
         call put_(stdout,self%atom(a)%label,int_width=TRUE)
         dpol = self%atom(a)%dipole_polarisability
         call put_(stdout,dpol(1,1))
         call put_(stdout,dpol(1,2))
         call put_(stdout,dpol(1,3))
         call flush_(stdout)
         call tab_(stdout,int_fields=1)
         call put_(stdout,dpol(2,1))
         call put_(stdout,dpol(2,2))
         call put_(stdout,dpol(2,3))
         call flush_(stdout)
         call tab_(stdout,int_fields=1)
         call put_(stdout,dpol(3,1))
         call put_(stdout,dpol(3,2))
         call put_(stdout,dpol(3,3))
         asym = max(asym,abs(dpol(1,2)-dpol(2,1)))
         asym = max(asym,abs(dpol(1,3)-dpol(3,1)))
         asym = max(asym,abs(dpol(2,3)-dpol(3,2)))
         call flush_(stdout)
         alpha_m = alpha_m+dpol
      end do
      call dash_(stdout,int_fields=1,real_fields=3)
      call flush_(stdout)
      call show_(stdout,"Maximum asymmetry in polarisabilities =",asym)
      call flush_(stdout)
      call text_(stdout,"Contribution to molecular polarisability from atomic charges (au):")
      call put_(stdout,alpha_q)
      call flush_(stdout)
      call text_(stdout,"Contribution to molecular polarisability from atomic dipoles (au):")
      call put_(stdout,alpha_m)
      call flush_(stdout)
      call text_(stdout,"Molecular polarisability (au):")
      alpha = alpha_q+alpha_m
      call put_(stdout,alpha)
   end subroutine


!  ====
!  CPHF
!  ====

   subroutine make_U_electric_dipole(self)
   ! Calculate U matrices for electric dipole perturbations using
   ! Coupled Perturbed Hartree-Fock theory. 
   ! NOTE: restricted case only so far.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), allocatable :: U,D_mo
      INT :: no,nv
   ENSURE(allocated(self%overlap_mx),"MOLECULE.CP:make_U_electric_dipole ... no overlap_mx")

      ! Dimensions
      no = self%n_a
      nv = self%n_bf-no

      ! U matrices. Leaky
      call create_(U,nv,no,3)

      ! Virtual-occupied dipole moment matrices 
      ! - in the MO basis
      call create_(D_mo,nv,no,3)
      call get_VO_MO_dipole_matrices_(self,D_mo)
      D_mo = -D_mo

      ! Solve the CPHF equations
      call set_CPHF_DIIS_(self)
      call solve_CPHF_equations_(self,U,D_mo)  ! NOT PURE
      call destroy_(D_mo)

      ! Assign U matrices
      self%U_electric_dipole = U
   end subroutine

   subroutine make_U2_electric_dipole(self)
   ! Calculate the second order U matrices for electric dipole
   ! perturbations using Coupled Perturbed Hartree-Fock theory. 
   ! NOTE: restricted case only so far.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), allocatable :: U,U2,rhs,Doo,Dvv
      MAT(REAL,:,:), allocatable :: xi,D,W,J,K,MOv
      INT :: no,nv,l,m,p
   ENSURE(allocated(self%U_electric_dipole),"MOLECULE.CP:make_U2_electric_dipole ... no first order U matrices!")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.CP:make_U2_electric_dipole ... no overlap_mx")

      ! Dimensions
      no = self%n_a
      nv = self%n_bf-no

      ! U2 matrices. Leaky
      call create_(U2,nv,no,3)

      ! Assign U matrices, MO's
      U = self%U_electric_dipole

      ! RHS of CPHF equations
      call create_(rhs,nv,no,6)

      ! Get the virtual-occupied dipole moment matrices in the MO basis
      call create_(Doo,no,no,3)
      call create_(Dvv,nv,nv,3)
      call get_OO_VV_MO_dipole_matrices_(self,Doo,Dvv)

      ! Virtual MOs
      MOv = self%MOs%r(:,no+1:)
      call create_(D,self%n_bf,self%n_bf)
      call create_(W,self%n_bf,self%n_bf)
      call create_(J,self%n_bf,self%n_bf)
      call create_(K,self%n_bf,self%n_bf)

      ! Make the second-order RHS
      p = 0
      do l = 1,3
         do m = 1,l
            p = p+1

         ! xi-oo -> AO basis
            call create_(xi,no,no)
            call to_product_of_(xi,U(:,:,l),U(:,:,m),transpose_a=TRUE)
            call plus_product_of_(xi,U(:,:,m),U(:,:,l),transpose_a=TRUE)
            call back_transform_to_(xi,W,MOv)
            call destroy_(xi)

         ! xi-vv -> AO basis
            call create_(xi,nv,nv)
            call to_product_of_(xi,U(:,:,l),U(:,:,m),transpose_b=TRUE)
            call plus_product_of_(xi,U(:,:,m),U(:,:,l),transpose_b=TRUE)
            call back_transform_to_(xi,D,MOv)
            call destroy_(xi)
            D = HALF*(D-W)
            call symmetrize_(D)

         ! A times xi -> AO basis -> MO basis -> W
            call make_r_JK_engine_(self,J,K,D)
            D = FOUR*J-TWO*K

         ! Assemble rhs
            call change_basis_to_(D,rhs(:,:,p),MOv,MOv)
            call plus_scaled_product_of_(rhs(:,:,p),U(:,:,l),Doo(:,:,m),fac=-ONE)
            call plus_scaled_product_of_(rhs(:,:,p),U(:,:,m),Doo(:,:,l),fac=-ONE)
            call plus_scaled_product_of_(rhs(:,:,p),Dvv(:,:,l),U(:,:,m),fac=-ONE)
            call plus_scaled_product_of_(rhs(:,:,p),Dvv(:,:,m),U(:,:,l),fac=-ONE)
         end do
      end do

      ! Clean
      call destroy_(K)
      call destroy_(J)
      call destroy_(W)
      call destroy_(D)
      call destroy_(Dvv)
      call destroy_(Doo)

      ! Solve the CPHF equations
      call set_CPHF_DIIS_(self)
      call solve_CPHF_equations_(self,U2,rhs)  ! NOT PURE

      ! Assign U matrices
      self%U2_electric_dipole = U2
   end subroutine

   PURE subroutine make_perturbed_densities(self,D,U,MO,no)
   ! Make the perturbed densities "D" from the "U" matrices by back
   ! transforming with the "MO" from occupied orbital "no".
      type(MOLECULE_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: D
      MAT3(REAL,:,:,:), IN :: U
      MAT(REAL,:,:), IN :: MO
      INT, optional, IN :: no
      INT :: i
   ENSURE(size(D,3)==size(U,3),"MOLECULE.CP:make_perturbed_densities ... incompatible D and U")
   ENSURE(size(D,1)==self%n_bf AND size(D,2)==self%n_bf,"MOLECULE.CP:make_perturbed_densities ... wrong size, D")
   ENSURE(is_square_(MO) AND size(MO,1)==self%n_bf,"MOLECULE.CP:make_perturbed_densities ... wrong size, MO")
      if (present(no)) then
         ENSURE(no>0,"MOLECULE.CP:make_perturbed_densities ... n_occ must be positive")
         ENSURE(no<=self%n_bf,"MOLECULE.CP:make_perturbed_densities ... n_occ must not exceed n_bf")
         do i = 1,size(U,3)
            call back_transform_to_(U(:,:,i),D(:,:,i),MO(:,no+1:),MO(:,1:no))
            call symmetrize_(D(:,:,i))
         end do
      else
         do i = 1,size(U,3)
            call back_transform_to_(U(:,:,i),D(:,:,i),MO)
            call symmetrize_(D(:,:,i))
         end do
      end if
      D = FOUR*D
   end subroutine

   PURE subroutine get_MO_dipole_matrices(self,D_mo)
   ! Get the MO dipole matrices
      type(MOLECULE_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: D_mo
   ENSURE(all(shape(D_mo)==[self%n_bf,self%n_bf,3]),"MOLECULE.CP:get_MO_dipole_matrices ... wrong shape, D_mo")
   ENSURE(allocated(self%MOs),"MOLECULE.CP:get_MO_dipole_matrices ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.CP:get_MO_dipole_matrices ... no restricted MO's")

      ! Get the dipole moment matrices in the AO basis.
      call make_dipole_matrices_(self,D_mo(:,:,1),D_mo(:,:,2),D_mo(:,:,3))

      ! Change dipole matrices from AO -> MO basis
      call change_basis_using_(D_mo(:,:,1),self%MOs%r)
      call change_basis_using_(D_mo(:,:,2),self%MOs%r)
      call change_basis_using_(D_mo(:,:,3),self%MOs%r)
   end subroutine

   PURE subroutine get_VO_MO_dipole_matrices(self,D_mo)
   ! Get the virtual-occupied MO dipole matrices
      type(MOLECULE_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: D_mo
      MAT3(REAL,:,:,:), allocatable :: D_ao
      MAT(REAL,:,:), allocatable :: MOv,MOo
      INT :: no
   ENSURE(all(shape(D_mo)==[(self%n_bf-self%n_a),self%n_a,3]),"MOLECULE.CP:get_VO_MO_dipole_matrices ... wrong shape, D_mo")
   ENSURE(allocated(self%MOs),"MOLECULE.CP:get_VO_MO_dipole_matrices ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.CP:get_VO_MO_dipole_matrices ... no restricted MO's")

      ! Get the dipole moment matrices in the AO basis.
      call create_(D_ao,self%n_bf,self%n_bf,3)
      call make_dipole_matrices_(self,D_ao(:,:,1),D_ao(:,:,2),D_ao(:,:,3))

      ! MO's
      no = self%n_a
      MOv = self%MOs%r(:,no+1:)
      MOo = self%MOs%r(:,1:no)

      ! Change dipole matrices from AO -> MO basis
      call change_basis_to_(D_ao(:,:,1),D_mo(:,:,1),MOv,MOo)
      call change_basis_to_(D_ao(:,:,2),D_mo(:,:,2),MOv,MOo)
      call change_basis_to_(D_ao(:,:,3),D_mo(:,:,3),MOv,MOo)
      call destroy_(D_ao)
   end subroutine

   PURE subroutine get_OO_VV_MO_dipole_matrices(self,Doo,Dvv)
   ! Get the virtual-occupied MO dipole matrices
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), OUT :: Doo
      MAT3(REAL,:,:,:), OUT :: Dvv
      MAT3(REAL,:,:,:), allocatable :: D_ao
      MAT(REAL,:,:), allocatable :: MOv,MOo
      INT :: no
   ENSURE(allocated(self%MOs),"MOLECULE.CP:get_OO_VV_MO_dipole_matrices ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.CP:get_OO_VV_MO_dipole_matrices ... no restricted MO's")

      ! Get the dipole moment matrices in the AO basis.
      call create_(D_ao,self%n_bf,self%n_bf,3)
      call make_dipole_matrices_(self,D_ao(:,:,1),D_ao(:,:,2),D_ao(:,:,3))

      ! Change dipole matrices from AO -> MO basis
      no = self%n_a
      MOv = self%MOs%r(:,no+1:)
      MOo = self%MOs%r(:,1:no)
      call change_basis_to_(D_ao(:,:,1),Doo(:,:,1),MOo)
      call change_basis_to_(D_ao(:,:,2),Doo(:,:,2),MOo)
      call change_basis_to_(D_ao(:,:,3),Doo(:,:,3),MOo)
      call change_basis_to_(D_ao(:,:,1),Dvv(:,:,1),MOv)
      call change_basis_to_(D_ao(:,:,2),Dvv(:,:,2),MOv)
      call change_basis_to_(D_ao(:,:,3),Dvv(:,:,3),MOv)
      call destroy_(D_ao)
   end subroutine

   PURE subroutine set_CPHF_DIIS(self)
   ! Set up the DIIS archives for a CPHF calculations.
      type(MOLECULE_TYPE), INOUT :: self
      REAL :: tol
   ENSURE(allocated(self%SCF_data),"MOLECULE.CP:set_CPHF_DIIS ... no scf data")

      ! Set up DIIS
      tol = self%SCF_data%SCF_DIIS%convergence_tolerance
      call set_defaults_(self%SCF_data%SCF_DIIS)
      call set_save_iteration_(self%SCF_data%SCF_DIIS,0)
      call set_start_iteration_(self%SCF_data%SCF_DIIS,2)
      call set_convergence_tolerance_(self%SCF_data%SCF_DIIS,tol)
   end subroutine

   subroutine solve_CPHF_equations(self,U,rhs)
   ! Solve the coupled-perturbed Hartree-Fock (CPHF) equations. The
   ! result is the derivative MO coefficients "U" (virtual-occupied
   ! block) and "rhs" is the right hand side of the CPHF equations
   ! (virtual-occupied block). The convergence tolerance and the
   ! number of iterations are controlled via the SCF_data block, since
   ! this is a self consistent procedure.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), INOUT :: U
      MAT3(REAL,:,:,:), IN :: rhs
      VEC(REAL,:), allocatable :: Ev,Eo
      VEC(REAL,:), allocatable :: par,err
      MAT3(REAL,:,:,:), allocatable :: U_vo
      MAT(REAL,:,:), allocatable :: U_ao,J,K,W
      MAT(REAL,:,:), allocatable :: MOv,MOo
      INT :: no,nv,n_pert,n_vo,n
      REAL :: tol0,test,delta
      BIN :: use_DIIS
      type(ARCHIVE_TYPE) :: arch
   ENSURE(allocated(self%SCF_data),"MOLECULE.CP:solve_CPHF_equations ... no scf data")
   ENSURE(allocated(self%MOs),"MOLECULE.CP:solve_CPHF_equations ... no MOs")
   ENSURE(allocated(self%MOs%r),"MOLECULE.CP:solve_CPHF_equations ... no restricted MOs")
   ENSURE(size(U,1)==(self%n_bf-self%n_a),"MOLECULE.CP:solve_CPHF_equations ... wrong size, U, dimension 1")
   ENSURE(size(U,2)==self%n_a,"MOLECULE.CP:solve_CPHF_equations ... wrong size, U, dimension 2")
   ENSURE(size(rhs,1)==(self%n_bf-self%n_a),"MOLECULE.CP:solve_CPHF_equations ... wrong size, rhs, dimension 1")
   ENSURE(size(rhs,2)==self%n_a,"MOLECULE.CP:solve_CPHF_equations ... wrong size, rhs dimension 2")
   ENSURE(size(U,3)==size(rhs,3),"MOLECULE.CP:solve_CPHF_equations ... incompatible sizes, U and rhs")

      ! Dimensions
      no = self%n_a
      nv = self%n_bf-self%n_a
      n_vo = nv*no
      n_pert = size(U,3)
      tol0 = TOL(6)

      ! Assign MO's, orbital ranges
      MOv = self%MOs%r(:,no+1:)
      MOo = self%MOs%r(:,1:no)
      Ev = self%MO_energies%r(no+1:)
      Eo = self%MO_energies%r(1:no)

      ! Read/initialize U's
      call initialize_Us_(self,U,rhs,Ev,Eo,tol0)

      ! Allocate temporaries
      call create_(U_vo,nv,no,n_pert)
      call create_(U_ao,self%n_bf,self%n_bf)
      call create_(W,nv,no)
      call create_(J,self%n_bf,self%n_bf)
      call create_(K,self%n_bf,self%n_bf)
      call create_(par,n_vo)
      call create_(err,n_vo)

      ! Use DIIS?
      use_DIIS = self%SCF_data%using_CP_DIIS

      ! Set initial convergence data/DIIS
      call update_CP_difference_(self%SCF_data,ZERO)
      call set_DIIS_error_(self%SCF_data,ZERO)
      if (use_DIIS) call reset_CP_(self%SCF_data,n_pert)

      ! Banner & tabular output
      call put_CP_banner_(self%SCF_data)
      call put_CP_options_(self%SCF_data)
      call set_CP_table_(self%SCF_data)
      call put_CP_table_head_(self%SCF_data)
      call put_CP_table_body_and_footer_(self%SCF_data)

      ! Begin iterations
      do
         U_vo = U
         test = ZERO
         delta = ZERO

         ! For each perturbation, update U(:,:,n)
         do n = 1,n_pert

            ! U(MO) -> U(AO)
            call back_transform_to_(U(:,:,n),U_ao,MOv,MOo)
            call symmetrize_(U_ao)

            ! Make A.U (AO) -> A.U (MO) = W
            call make_r_JK_engine_(self,J,K,U_ao)
            U_ao = FOUR*J-TWO*K
            call remove_dependence_from_r_(self,U_ao)
            call change_basis_to_(U_ao,W,MOv,MOo)

            ! DIIS extrapolation on AU (MO)
            if (use_DIIS) then
               par = reshape(W,[n_vo])
               call plus_difference_product_with_(W,U(:,:,n),Ev,Eo,tol0)  ! W <- H
               err = reshape(W-rhs(:,:,n),[n_vo])  ! H - rhs
               call extrapolate_(self%SCF_data%CP_DIIS(n),par,err,n_vo)
               W = reshape(par,[nv,no])  ! W <- AU
               test = max(test,self%SCF_data%CP_DIIS(n)%error_length)
            end if

            ! Update U, U <- (rhs - W)/D
            W = rhs(:,:,n)-W
            U(:,:,n) = ZERO
            call plus_difference_divisor_with_(U(:,:,n),W,Ev,Eo,tol0)  ! W <- H
         end do

         ! Delta is max |diff|
         delta = max(delta,maxval(abs(U-U_vo)))

         ! Update data
         call update_iteration_(self%SCF_data)
         call update_DIIS_from_CP_DIIS_(self%SCF_data)
         call update_CP_difference_(self%SCF_data,delta)
         call set_error_length_(self%SCF_data%SCF_DIIS,test)
         call set_DIIS_error_(self%SCF_data,test)

         ! Put table
         call put_CP_table_body_and_footer_(self%SCF_data,flush=TRUE)

         ! Write archive for restart
         call set_(arch,self%name,"U_electric_dipole",genre="r")
         call write_(arch,U)
         if (SCF_done_(self%SCF_data)) exit
      end do  ! iterative cycle

      ! Clean-up
      call destroy_ptr_part_(self%SCF_data%SCF_DIIS)
      call destroy_(self%SCF_data%CP_DIIS)
      call destroy_(err)
      call destroy_(par)
      call destroy_(K)
      call destroy_(J)
      call destroy_(W)
      call destroy_(U_ao)
      call destroy_(U_vo)
   end subroutine

   subroutine initialize_Us(self,U,rhs,Ev,Eo,tol0)
   ! Initialize the U matrices
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), OUT :: U
      MAT3(REAL,:,:,:), IN :: rhs
      VEC(REAL,:), IN :: Ev
      VEC(REAL,:), IN :: Eo
      REAL, IN :: tol0
      type(ARCHIVE_TYPE) :: arch
      INT :: n
      if (self%SCF_data%initial_Us=="r") then

         ! Read or calculate initial approximation for U
         call flush_(stdout)
         call text_(stdout,"As requested from SCF_data, reading U's from disk ...")
         call set_(arch,self%name,"U_electric_dipole",genre="r")
         call read_(arch,U)
      else

         ! Set SOS initial guess
         U = ZERO
         do n = 1,size(U,3)
            call plus_difference_divisor_with_(U(:,:,n),rhs(:,:,n),Ev,Eo,tol0)
         end do
      end if
   end subroutine


!  ========
!  A matrix
!  ========

!   make_A_times_U(AU,U) :: private
!   ! Make the full A times U matrix in the MO basis
!      self :: INOUT
!      AU :: MAT3{REAL}, OUT
!      U  :: MAT3{REAL}, IN
!
!   ENSURE(.SCF_data.allocated,"no scf data")
!   ENSURE(.basis_info_made,"no basis info")
!   ENSURE(.atom.allocated,"no atom info")
!   ENSURE(U.dim1==(.n_bf-.n_a),"wrong size, U, dimension 1")
!   ENSURE(U.dim2==.n_a,"wrong size, rhs, dimension 2")
!   ENSURE(AU.dim1==.n_bf,"wrong size, AU")
!   ENSURE(AU.dim2==.n_bf,"wrong size, AU")
!   ENSURE(U.dim3==AU.dim3,"incompatible sizes, U and AU")
!
!      U_ao,J,K :: MAT{REAL}@
!      MOv, MOo :: MAT{REAL}@
!      n :: INT
!
!      U_ao.create(.n_bf,.n_bf)
!      J.create(.n_bf,.n_bf)
!      K.create(.n_bf,.n_bf)
!
!      MOv = .MOs.r(:,.n_a+1:)
!      MOo = .MOs.r(:,:.n_a) 
!
!      do n = 1,U.dim3
!         U(:,:,n).back_transform_to(U_ao,Mov,MOo)
!         U_ao.symmetrize                   ! U -> AO basis
!         .make_r_JK_direct(J,K,U_ao)  ! A times U (INOUT)
!         AU(:,:,n) = FOUR*J - TWO*K
!         AU(:,:,n).change_basis_using(.MOs.r)
!      end
!
!      ! Clean
!      K.destroy
!      J.destroy
!      U_ao.destroy
!
!,   end

   subroutine add_A_times_U(self,AU,U)
   ! Add the full A matrix times the full U matrix to "AU". "U" is the
   ! virtual-occupied block of the full U matrix.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), OUT :: AU
      MAT3(REAL,:,:,:), IN :: U
      MAT(REAL,:,:), allocatable :: J,K,W
      MAT(REAL,:,:), allocatable :: MOv,MOo
      INT :: n
   ENSURE(allocated(self%SCF_data),"MOLECULE.CP:add_A_times_U ... no scf data")
   ENSURE(self%basis_info_made,"MOLECULE.CP:add_A_times_U ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.CP:add_A_times_U ... no atom info")
   ENSURE(size(U,1)==(self%n_bf-self%n_a),"MOLECULE.CP:add_A_times_U ... wrong size, U, dimension 1")
   ENSURE(size(U,2)==self%n_a,"MOLECULE.CP:add_A_times_U ... wrong size, rhs, dimension 2")
   ENSURE(size(AU,1)==self%n_bf,"MOLECULE.CP:add_A_times_U ... wrong size, AU")
   ENSURE(size(AU,2)==self%n_bf,"MOLECULE.CP:add_A_times_U ... wrong size, AU")
   ENSURE(size(U,3)==size(AU,3),"MOLECULE.CP:add_A_times_U ... incompatible sizes, U and AU")
      call create_(J,self%n_bf,self%n_bf)
      call create_(K,self%n_bf,self%n_bf)
      call create_(W,self%n_bf,self%n_bf)
      MOv = self%MOs%r(:,self%n_a+1:)
      MOo = self%MOs%r(:,1:self%n_a)  ! NB: explicit "1:" — the open-lower form ":.n_a" is
                               ! mistranslated to ".n_a:" by both foo.pl and antlr4
      do n = 1,size(U,3)
         call back_transform_to_(U(:,:,n),W,MOv,MOo)
         call symmetrize_(W)  ! U -> AO basis
         call make_r_JK_engine_(self,J,K,W)
         W = FOUR*J-TWO*K  ! W = A times U
         call change_basis_using_(W,self%MOs%r)
         AU(:,:,n) = AU(:,:,n)+W
      end do

      ! Clean
      call destroy_(W)
      call destroy_(K)
      call destroy_(J)
   end subroutine

end module
