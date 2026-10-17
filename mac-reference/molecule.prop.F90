!==================================================================
!
! MOLECULE.PROP: An object representation of a molecule.
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
module MOLECULE_PROP_MODULE

#  include "macros"

   include "molecule.prop.use"

   implicit none

   include "molecule.prop.int"


contains

!  ===========
!  Frequencies
!  ===========

   subroutine read_force_constants(self)
   ! Read the force constants as a flat vector in atomic units.
   ! Since it is a symmetric matrix the order, row or column major,
   ! does not matter.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: fc
      call read_all_(stdin,fc)
      call set_force_constants_(self,fc)
      call destroy_(fc)
   end subroutine

   subroutine make_FD_hessian(self)
   ! Make .force_constants, the Cartesian Hessian, by central differences
   ! of SCF energies with step .FD_hessian_step (bohr):
   !    H_ii = (E(+i) + E(-i) - 2 E0)/h^2
   !    H_ij = (E(+i+j) - E(+i-j) - E(-i+j) + E(-i-j))/(4 h^2)
   ! The SCF must have been done at the reference geometry; its options
   ! are used for every displaced SCF, each starting from the density of
   ! the one before. Translational invariance -- each atom's block row of
   ! H sums to zero over the atoms -- gives the last atom's rows, unless
   ! cluster charges or an applied field break it. The gradient comes from
   ! the same energies and is printed: away from a stationary point the
   ! rotational sum rule does not hold, and the Eckart projection in
   ! normal_mode_analysis is then an approximation.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: geom0,Ep,Em,grad
      MAT(REAL,:,:), allocatable :: FC
      REAL :: E0,h,epp,epm,emp,emm
      INT :: n3n,n,i,j,a,b,n_scf
      BIN :: invariant,out
   ENSURE(allocated(self%atom),"MOLECULE.PROP:make_FD_hessian ... no atoms")
   ENSURE(allocated(self%SCF_data),"MOLECULE.PROP:make_FD_hessian ... no SCF data: do an SCF first")
   ENSURE(self%FD_hessian_step>ZERO,"MOLECULE.PROP:make_FD_hessian ... fd_hessian_step= must be positive")
      n3n = 3*size(self%atom)
      h = self%FD_hessian_step
      E0 = self%SCF_data%energy

      ! Coordinates differenced: all, or all but the last atom's
      invariant = NOT self%SCF_data%using_cluster_charges AND all(self%E_field==ZERO)
      n = n3n
      if (invariant AND size(self%atom)>1) n = n3n-3
      call create_(geom0,n3n)
      call put_coordinate_vector_to_(self%atom,geom0)
      out = self%SCF_data%output
      call set_output_(self%SCF_data,FALSE)

      ! Single displacements: the diagonal and the gradient
      call create_(Ep,n)
      call create_(Em,n)
      call create_(grad,n)
      call create_(FC,n3n,n3n)
      FC = ZERO
      do i = 1,n
         Ep(i) = FD_hessian_energy_(self,geom0,i,1,0,0)
         Em(i) = FD_hessian_energy_(self,geom0,i,-1,0,0)
         FC(i,i) = (Ep(i)+Em(i)-TWO*E0)/(h*h)
         grad(i) = (Ep(i)-Em(i))/(TWO*h)
      end do
      n_scf = 2*n

      ! Double displacements: the off-diagonal elements
      do i = 1,n
         do j = i+1,n
            epp = FD_hessian_energy_(self,geom0,i,1,j,1)
            epm = FD_hessian_energy_(self,geom0,i,1,j,-1)
            emp = FD_hessian_energy_(self,geom0,i,-1,j,1)
            emm = FD_hessian_energy_(self,geom0,i,-1,j,-1)
            FC(i,j) = (epp-epm-emp+emm)/(FOUR*h*h)
            FC(j,i) = FC(i,j)
         end do
      end do
      n_scf = n_scf+4*n*(n-1)/2

      ! The last atom, by translational invariance
      if (n<n3n) then
         do i = 1,n
            do b = 1,3
               FC(i,n+b) = -sum(FC(i,b:n:3))
               FC(n+b,i) = FC(i,n+b)
            end do
         end do
         do a = 1,3
            do b = 1,3
               FC(n+a,n+b) = -sum(FC(n+a,b:n:3))
            end do
         end do
      end if

      ! Back to the reference geometry and its SCF
      call FD_hessian_move_to_(self,geom0)
      call SCF_(self)
      call set_output_(self%SCF_data,out)
      call destroy_(self%force_constants)
      self%force_constants = FC
      self%FD_hessian_scale = SCF_frequency_scale_(self)
      call flush_(stdout)
      call text_(stdout,"=======================================")
      call text_(stdout,"Cartesian Hessian by finite differences")
      call text_(stdout,"=======================================")
      call flush_(stdout)
      call show_(stdout,"Step /bohr                      =",h)
      call show_(stdout,"No. of SCFs                     =",n_scf)
      call show_(stdout,"Translational invariance used?  =",n<n3n)
      call show_(stdout,"Literature frequency scale      =",self%FD_hessian_scale)
      call show_(stdout,"Largest gradient /(Eh/bohr)     =",maxval(abs(grad)))
      call show_(stdout,"RMS gradient /(Eh/bohr)         =",sqrt(sum(grad*grad)/n))
      if (maxval(abs(grad))>TOL(3)) then
         call text_(stdout,"The geometry is not a stationary point: the rotational modes")
         call text_(stdout,"are not exact zero modes, and their projection is approximate.")
      end if
      call flush_(stdout)
      call destroy_(FC)
      call destroy_(grad)
      call destroy_(Em)
      call destroy_(Ep)
      call destroy_(geom0)
   end subroutine

   function FD_hessian_energy(self,geom0,i,si,j,sj) result (res)
   ! The SCF energy with coordinate "i" displaced by "si" steps of
   ! .FD_hessian_step from "geom0", and coordinate "j" by "sj" (none if
   ! j is 0).
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: geom0
      INT, IN :: i,si,j,sj
      REAL :: res
      VEC(REAL,:), allocatable :: g
      g = geom0
      g(i) = g(i)+si*self%FD_hessian_step
      if (j>0) g(j) = g(j)+sj*self%FD_hessian_step
      call FD_hessian_move_to_(self,g)
      call SCF_(self)
      res = self%SCF_data%energy
      call destroy_(g)
   end function

   subroutine FD_hessian_move_to(self,g)
   ! Put the atoms at "g" and clear what depends on the geometry, as a
   ! HAR cycle does; the next SCF starts from the current density.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: g
      call set_positions_from_vector_(self%atom,g)
      if (allocated(self%MOs)) call symmetrically_orthonormalize_(self,self%MOs)
      call delete_SCF_archives_(self,keep_DM=TRUE)
      call destroy_SCF_results_(self,keep_MOs=TRUE)
      call destroy_SCF_integrals_(self)
      self%partition_info_made = FALSE
      call set_initial_density_(self%SCF_data,spinorbital_genre_(self%SCF_data))
   end subroutine

   subroutine normal_mode_analysis(self)
   ! Make the normal modes: the eigenvectors of the mass-weighted force
   ! constants, with the rigid-body motions projected out first (the
   ! Eckart-Sayvetz conditions), so the translations and rotations are
   ! exactly zero modes and the internal modes are free of them.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: n3n,i,j,n_rb
      MAT(REAL,:,:), allocatable :: fc,B
      VEC(REAL,:), allocatable :: rm
      REAL :: rmi,rmj
   ENSURE(allocated(self%force_constants),"MOLECULE.PROP:normal_mode_analysis ... no force constants")
   ENSURE(allocated(self%atom),"MOLECULE.PROP:normal_mode_analysis ... no atoms")
      n3n = 3*size(self%atom)

      ! Clean
      call destroy_(self%normal_mode_eigenvectors)
      call destroy_(self%normal_mode_eigenvalues)

      ! Allocate
      call create_(self%normal_mode_eigenvalues,n3n)
      call create_(self%normal_mode_eigenvectors,n3n,n3n)
      call create_(fc,n3n,n3n)
      call create_(rm,n3n)

      ! Masses in au ... *not* AMU
      rm = displacement_mass_vector_(self%atom)*MASS_OF_ELECTRON_PER_AMU
      rm = ONE/sqrt(rm)

      ! Mass weight
      do i = 1,n3n
         rmi = rm(i)
         do j = 1,n3n
            rmj = rm(j)
            fc(i,j) = rmi*self%force_constants(i,j)*rmj
         end do
      end do

      ! Project out the rigid-body motions
      call create_(B,n3n,6)
      call make_rigid_body_modes_(self%atom,B,n_rb)
      call project_out_vectors_(fc,B(:,1:n_rb))

      ! Get modes
      call solve_symmetric_eigenproblem_(fc,self%normal_mode_eigenvalues,self%normal_mode_eigenvectors)

      ! The rigid-body modes are degenerate at zero, so the eigensolver
      ! returns any rotation of them, differing between platforms. Put the
      ! rigid-body vectors themselves in their place, with eigenvalue zero.
      j = 0
      do i = 1,n3n
         if (sum(matmul(transpose(B(:,1:n_rb)),self%normal_mode_eigenvectors(:,i))**2)<=HALF) cycle
         j = j+1
         if (j>n_rb) exit
         self%normal_mode_eigenvectors(:,i) = B(:,j)
         self%normal_mode_eigenvalues(i) = ZERO
      end do
      call destroy_(B)

      ! Clean
      call destroy_(rm)
      call destroy_(fc)
   end subroutine

   PURE function harmonic_frequency_scale(self) result (res)
   ! The factor the harmonic frequencies are scaled by in the internal-mode
   ! ADPs: .frequency_scale_factor if it was set (it is negative when not);
   ! else, for a Hessian made by make_FD_hessian, the literature value for
   ! its method (.FD_hessian_scale); else 1, since the method behind a
   ! Hessian that was read in is not known.
      type(MOLECULE_TYPE), IN :: self
      REAL :: res
      res = self%frequency_scale_factor
      if (res>ZERO) return
      res = self%FD_hessian_scale
      if (res>ZERO) return
      res = ONE
   end function

   PURE function SCF_frequency_scale(self) result (res)
   ! The harmonic frequency scale factor for the SCF method, from Scott and
   ! Radom, J. Phys. Chem. 100, 16502 (1996), for the 6-31G(d) basis: HF
   ! 0.8953, B3LYP 0.9614, BLYP 0.9945; 1 for anything else.
      type(MOLECULE_TYPE), IN :: self
      REAL :: res
      res = ONE
      if (NOT allocated(self%SCF_data)) return
      select case (self%SCF_data%SCF_kind)
      case ("rhf ","uhf ","rohf")
         res = 0.8953d0
      case ("rks ","uks ")
         if (self%SCF_data%DFT_exchange_functional(1:5)=="b3lyp") then; res = 0.9614d0
         else if (self%SCF_data%DFT_exchange_functional=="becke88" AND self%SCF_data%DFT_correlation_functional=="lyp") then; res = 0.9945d0
         end if
      end select
   end function

   subroutine make_internal_ADPs(self)
   ! Make .stiff_mode_ADPs, the part of each atom's ADP from the stiff
   ! internal modes: those above .soft_mode_cutoff (cm^-1). The harmonic
   ! frequencies are first scaled by harmonic_frequency_scale. Mode k with
   ! mass-weighted vector l_k and frequency w_k adds to atom a
   !    U_a += l_ak l_ak^T <Q_k^2> / m_a,  <Q_k^2> = coth(w_k/2kT) / (2 w_k)
   ! in atomic units, at temperature .internal_ADP_temperature (zero-point
   ! motion only at T = 0). The rigid-body modes are recognised by their
   ! overlap with make_rigid_body_modes and left out. A mode with a zero or
   ! negative eigenvalue, which a Hessian away from a minimum gives, is a
   ! soft mode: the refinement must find its amplitude. See
   ! docs/TASK_ON_MODE_FITTING.md section 3.2.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: B,l
      VEC(REAL,3) :: la
      REAL :: w,kT,Q2,ma,rb,scale
      INT :: n3n,n_rb,k,a,i,j
   ENSURE(allocated(self%atom),"MOLECULE.PROP:make_internal_ADPs ... no atoms")
      if (NOT allocated(self%normal_mode_eigenvectors)) call normal_mode_analysis_(self)
      n3n = 3*size(self%atom)
      kT = self%internal_ADP_temperature/KELVIN_PER_HARTREE
      scale = harmonic_frequency_scale_(self)
      call create_(B,n3n,6)
      call make_rigid_body_modes_(self%atom,B,n_rb)
      l = self%normal_mode_eigenvectors
      call destroy_(self%stiff_mode_ADPs)
      call create_(self%stiff_mode_ADPs,3,3,size(self%atom))
      self%stiff_mode_ADPs = ZERO
      do k = 1,n3n

         ! Rigid-body, non-positive and soft modes are not stiff modes
         rb = ZERO
         do j = 1,n_rb
            rb = rb+dot_product(B(:,j),l(:,k))**2
         end do
         if (rb>HALF) cycle
         if (self%normal_mode_eigenvalues(k)<=ZERO) cycle
         w = scale*sqrt(self%normal_mode_eigenvalues(k))
         if (w*WAVENUMBER_PER_HARTREE<=self%soft_mode_cutoff) cycle

         ! Mean-square amplitude of the mode
         Q2 = HALF/w
         if (kT>ZERO) Q2 = Q2/tanh(HALF*w/kT)
         do a = 1,size(self%atom)
            ma = self%atom(a)%mass*MASS_OF_ELECTRON_PER_AMU
            la = l(3*a-2:3*a,k)
            do i = 1,3
               do j = 1,3
                  self%stiff_mode_ADPs(i,j,a) = self%stiff_mode_ADPs(i,j,a)+la(i)*la(j)*Q2/ma
               end do
            end do
         end do
      end do
      call destroy_(l)
      call destroy_(B)
   end subroutine

   subroutine set_crystal_U_high(self)
   ! Give the crystal the stiff-mode ADPs of its TLS body, the fixed part of
   ! a TLS refinement, and the .crystal.xray_data.n_soft_modes softest modes
   ! it is to refine. For a cluster made by create_cluster they come from
   ! the molecule it was made from (.saved), the central molecule, which is
   ! the TLS body. Without stiff-mode ADPs U_high is zero, and says so.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: V
      VEC(REAL,:), allocatable :: a0,freq
      INT :: n_s
      REAL :: T
   ENSURE(allocated(self%crystal),"MOLECULE.PROP:set_crystal_U_high ... no crystal")
      n_s = self%crystal%xray_data%n_soft_modes
      if (allocated(self%stiff_mode_ADPs)) then
         DIE_IF(size(self%stiff_mode_ADPs,3)/=n_TLS_atoms_(self%crystal),"MOLECULE.PROP:set_crystal_U_high ... stiff_mode_ADPs and the TLS body differ in number of atoms")
         call set_fragment_U_high_(self%crystal,self%stiff_mode_ADPs)
         if (n_s>0) call make_soft_modes_(self,n_s,V,a0,freq)
         T = self%internal_ADP_temperature
      else if (allocated(self%saved)) then
         DIE_IF(NOT allocated(self%saved%stiff_mode_ADPs),"MOLECULE.PROP:set_crystal_U_high ... TLS on a cluster needs make_internal_adps before create_cluster")
         DIE_IF(size(self%saved%stiff_mode_ADPs,3)/=n_TLS_atoms_(self%crystal),"MOLECULE.PROP:set_crystal_U_high ... stiff_mode_ADPs and the TLS body differ in number of atoms")
         call set_fragment_U_high_(self%crystal,self%saved%stiff_mode_ADPs)
         if (n_s>0) call make_soft_modes_(self%saved,n_s,V,a0,freq)
         T = self%saved%internal_ADP_temperature
      else
         call flush_(stdout)
         call text_(stdout,"TLS: no stiff-mode ADPs (make_internal_adps), so U_high is zero")
         call flush_(stdout)
         DIE_IF(n_s>0,"MOLECULE.PROP:set_crystal_U_high ... soft modes need make_internal_adps")
      end if
      if (n_s>0) then
         call set_soft_modes_(self%crystal,V,a0,freq,T)
         call destroy_(freq); call destroy_(a0); call destroy_(V)
      end if
   end subroutine

   subroutine make_soft_modes(self,n,V,a0,freq)
   ! Make the "n" softest internal modes -- imaginary ones first, then by
   ! increasing frequency -- as "V", the displacement of each atom per unit
   ! normal coordinate, l/sqrt(m); "a0" each mode's harmonic <Q^2> (eq. (6)
   ! of docs/REPORT_ON_MODE_FITTING.md, frequencies scaled as for
   ! U_high; negative for an imaginary mode); and "freq" its frequency in
   ! cm^-1 (negative for imaginary).
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: n
      MAT(REAL,:,:), allocatable, OUT :: V
      VEC(REAL,:), allocatable, OUT :: a0,freq
      MAT(REAL,:,:), allocatable :: B
      VEC(INT,:), allocatable :: soft
      REAL :: w,kT,scale,rb,ma
      INT :: n3n,n_rb,n_s,k,j,a
   DIE_IF(NOT allocated(self%normal_mode_eigenvectors),"MOLECULE.PROP:make_soft_modes ... soft modes need the normal modes: normal_mode_analysis")
      n3n = 3*size(self%atom)
      call create_(B,n3n,6)
      call make_rigid_body_modes_(self%atom,B,n_rb)
      scale = harmonic_frequency_scale_(self)
      kT = self%internal_ADP_temperature/KELVIN_PER_HARTREE

      ! The soft modes, in eigenvalue order (imaginary ones come first)
      call create_(soft,n3n)
      n_s = 0
      do k = 1,n3n
         rb = ZERO
         do j = 1,n_rb
            rb = rb+dot_product(B(:,j),self%normal_mode_eigenvectors(:,k))**2
         end do
         if (rb>HALF) cycle
         if (self%normal_mode_eigenvalues(k)>ZERO) then
            w = scale*sqrt(self%normal_mode_eigenvalues(k))
            if (w*WAVENUMBER_PER_HARTREE>self%soft_mode_cutoff) cycle
         end if
         n_s = n_s+1
         soft(n_s) = k
      end do
      DIE_IF(n_s<n,"MOLECULE.PROP:make_soft_modes ... fewer soft modes than n_soft_modes=: raise soft_mode_cutoff=")
      call create_(V,n3n,n)
      call create_(a0,n)
      call create_(freq,n)
      do j = 1,n
         k = soft(j)
         do a = 1,size(self%atom)
            ma = self%atom(a)%mass*MASS_OF_ELECTRON_PER_AMU
            V(3*a-2:3*a,j) = self%normal_mode_eigenvectors(3*a-2:3*a,k)/sqrt(ma)
         end do
         if (self%normal_mode_eigenvalues(k)>ZERO) then
            w = scale*sqrt(self%normal_mode_eigenvalues(k))
            a0(j) = HALF/w
            if (kT>ZERO) a0(j) = a0(j)/tanh(HALF*w/kT)
            freq(j) = w*WAVENUMBER_PER_HARTREE
         else
            a0(j) = -ONE
            freq(j) = -scale*sqrt(-self%normal_mode_eigenvalues(k))*WAVENUMBER_PER_HARTREE
         end if
      end do
      call destroy_(soft); call destroy_(B)
   end subroutine

   subroutine put_internal_ADPs(self)
   ! Put the stiff-mode ADPs of make_internal_ADPs, in Angstrom^2, and the
   ! internal modes sorted into soft and stiff.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: B
      MAT(REAL,3,3) :: U
      REAL :: w,rb,scale
      INT :: n3n,n_rb,n_soft,n_stiff,n_imag,k,j,a
   ENSURE(allocated(self%stiff_mode_ADPs),"MOLECULE.PROP:put_internal_ADPs ... no stiff-mode ADPs: use make_internal_ADPs")
      n3n = 3*size(self%atom)
      call create_(B,n3n,6)
      call make_rigid_body_modes_(self%atom,B,n_rb)

      ! Sort the modes as make_internal_ADPs does
      scale = harmonic_frequency_scale_(self)
      n_soft = 0; n_stiff = 0; n_imag = 0
      do k = 1,n3n
         rb = ZERO
         do j = 1,n_rb
            rb = rb+dot_product(B(:,j),self%normal_mode_eigenvectors(:,k))**2
         end do
         if (rb>HALF) cycle
         if (self%normal_mode_eigenvalues(k)<=ZERO) then
            n_imag = n_imag+1
            n_soft = n_soft+1
            cycle
         end if
         w = scale*sqrt(self%normal_mode_eigenvalues(k))*WAVENUMBER_PER_HARTREE
         if (w<=self%soft_mode_cutoff) then; n_soft = n_soft+1
         else; n_stiff = n_stiff+1
         end if
      end do
      call destroy_(B)
      call flush_(stdout)
      call text_(stdout,"==================================")
      call text_(stdout,"ADPs from the stiff internal modes")
      call text_(stdout,"==================================")
      call flush_(stdout)
      call text_(stdout,". U_high of each atom, from the internal normal modes above the")
      call text_(stdout,"  soft-mode cutoff; the soft modes, including any imaginary ones,")
      call text_(stdout,"  and the rigid-body motion are left for the refinement.")
      call flush_(stdout)
      call show_(stdout,"Temperature /K                  =",self%internal_ADP_temperature)
      call show_(stdout,"Soft-mode cutoff /cm^-1         =",self%soft_mode_cutoff)
      call show_(stdout,"Frequency scale factor          =",scale)
      call show_(stdout,"No. of rigid-body modes         =",n_rb)
      call show_(stdout,"No. of soft internal modes      =",n_soft)
      call show_(stdout,"   of which imaginary           =",n_imag)
      call show_(stdout,"No. of stiff internal modes     =",n_stiff)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=7,width=4)
      call put_(stdout,"#",int_width=TRUE)
      call put_(stdout,"ID",width=4)
      call put_(stdout,"U_xx")
      call put_(stdout,"U_yy")
      call put_(stdout,"U_zz")
      call put_(stdout,"U_xy")
      call put_(stdout,"U_xz")
      call put_(stdout,"U_yz")
      call put_(stdout,"U_iso")
      call flush_(stdout)
      call put_(stdout," ",int_width=TRUE)
      call put_(stdout," ",width=4)
      call put_(stdout,"/A^2")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=7,width=4)
      do a = 1,size(self%atom)
         U = self%stiff_mode_ADPs(:,:,a)*ANGSTROM_PER_BOHR*ANGSTROM_PER_BOHR
         call put_(stdout,a)
         call put_(stdout,trim(self%atom(a)%label),width=4)
         call put_(stdout,U(1,1))
         call put_(stdout,U(2,2))
         call put_(stdout,U(3,3))
         call put_(stdout,U(1,2))
         call put_(stdout,U(1,3))
         call put_(stdout,U(2,3))
         call put_(stdout,THIRD*(U(1,1)+U(2,2)+U(3,3)))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=7,width=4)
   end subroutine

   subroutine put_normal_modes(self)
   ! Put the force constants and normal modes
      type(MOLECULE_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: eval
      VEC(STR(len=STR_SIZE),:), allocatable :: ewav
      INT :: n3n,i,f,l
   ENSURE(allocated(self%normal_mode_eigenvectors),"MOLECULE.PROP:put_normal_modes ... no normal mode eigenvectors")
   ENSURE(allocated(self%normal_mode_eigenvalues),"MOLECULE.PROP:put_normal_modes ... no normal mode eigenvalues")
      call flush_(stdout)
      call text_(stdout,"=========================")
      call text_(stdout,"Cartesian force constants")
      call text_(stdout,"=========================")
      call flush_(stdout)
      n3n = 3*size(self%atom)
      call show_(stdout,"No. of atoms        =",n3n/3)
      call show_(stdout,"No. of displacement =",n3n)
      call flush_(stdout)
      call put_(stdout,self%force_constants)
      call flush_(stdout)
      call text_(stdout,"Mass vector:")
      call flush_(stdout)
      call put_(stdout,self%atom(:)%mass)
      call flush_(stdout)
      call text_(stdout,"========================")
      call text_(stdout,"Normal mode eigenvectors")
      call text_(stdout,"========================")
      call flush_(stdout)
      call text_(stdout,". Eigenvalues are in cm^-1")
      call text_(stdout,". Eigenvectors in mass-weighted atomic units (not AMUs)")
      call flush_(stdout)
      call show_(stdout,"cm^-1 per Hartree     =",WAVENUMBER_PER_HARTREE)
      call show_(stdout,"Electron mass per AMU =",MASS_OF_ELECTRON_PER_AMU)
      call flush_(stdout)

      ! Table headings
      call create_(ewav,n3n)
      call create_(eval,n3n)
      eval = self%normal_mode_eigenvalues
      eval = sqrt(abs(eval))*WAVENUMBER_PER_HARTREE
      ewav = to_str_(eval,stdout%style%real_fortran_fmt,stdout%style%real_width,stdout%style%real_precision)
      call destroy_(eval)

      ! The table, ten modes at a time to stay within the line length
      do f = 1,n3n,10
         l = min(f+9,n3n)
         call create_(table,l-f+1)
         do i = f,l
            call set_heading_(table(i-f+1),trim(ewav(i)))
            call set_values_(table(i-f+1),self%normal_mode_eigenvectors(:,i))
         end do
         call put_(table)
         call destroy_(table)
      end do
      call destroy_(ewav)
   end subroutine

   subroutine put_dnc_displaced_geometries(self)
   ! Put the dimensionless normal coordinate displced geometries
   ! used for evaluating the phi constants force constants.
   ! See Schneider and Thiel (1989) CPL 157 p. 367.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: qtox
      VEC(REAL,:), allocatable :: m12,g12
      VEC(REAL,:), allocatable :: geom0,delta
      INT :: n_modes,n3n,f,l,i,s
      REAL :: step
   ENSURE(allocated(self%atom),"MOLECULE.PROP:put_dnc_displaced_geometries ... no atoms")
   ENSURE(size(self%atom)==1,"MOLECULE.PROP:put_dnc_displaced_geometries ... must have more than one atom")

      ! Ensure modes are there
      if (NOT allocated(self%normal_mode_eigenvectors)) call normal_mode_analysis_(self)

      ! Step size
      step = self%phi_step_size

      ! Starting mode & no of modes
      n3n = 3*size(self%atom)
      l = n3n
      f = 7
      if (is_linear_(self%atom)) f = 6
      n_modes = l-f+1

      ! Make the dimensionless normal coordinates
      call create_(qtox,n3n,n_modes)
      call create_(m12,n3n)
      call create_(g12,n_modes)
      g12 = ONE/sqrt(sqrt(self%normal_mode_eigenvalues(f:l)))
      m12 = displacement_mass_vector_(self%atom)*MASS_OF_ELECTRON_PER_AMU
      m12 = ONE/sqrt(m12)
      call change_basis_to_(self%normal_mode_eigenvectors,qtox,m12,g12)
      call destroy_(m12)
      call destroy_(g12)
      call flush_(stdout)
      call text_(stdout,"================================")
      call text_(stdout,"Dimensionless normal coordinates")
      call text_(stdout,"================================")
      call flush_(stdout)
      call text_(stdout,"The columns correspoind to the non-zero modes")
      call text_(stdout,"assuming that the geometry is a minimum")
      call flush_(stdout)
      call show_(stdout,"No. of atoms        =",n3n/3)
      call show_(stdout,"No. of displacement =",n3n)
      call show_(stdout,"No. of modes        =",n_modes)
      call flush_(stdout)
      call put_(stdout,qtox)
      call flush_(stdout)
      call text_(stdout,"========================================================")
      call text_(stdout,"Displaced normal coordinate geometries for Phi constants")
      call text_(stdout,"========================================================")
      call create_(geom0,n3n)
      call create_(delta,n3n)
      call flush_(stdout)
      call text_(stdout,"Undisplaced (reference) geometry")
      call put_coordinate_vector_to_(self%atom,geom0)
      call put_coordinates_(self%atom)
      do i = f,l
         do s = -1,1,2
            call flush_(stdout)
            call show_(stdout,"Displacement mode      = ",i)
            call show_(stdout,"Displacement mode sign = ",s)
            delta = geom0+s*step*qtox(:,i)
            call set_positions_from_vector_(self%atom,delta)
            call put_coordinates_(self%atom)
         end do
      end do

      ! Clean
      call destroy_(delta)
      call destroy_(geom0)
   end subroutine


!  =======================
!  One electron properties
!  =======================

   subroutine put_1e_properties(self)
   ! Put all the available one elctron properties
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%density_mx),"MOLECULE.PROP:put_1e_properties ... no density")
   ENSURE(allocated(self%atom),"MOLECULE.PROP:put_1e_properties ... no atoms")
   ENSURE(allocated(self%basis),"MOLECULE.PROP:put_1e_properties ... no basis sets specified")

      ! Make the density matrix (leaky)
      call make_r_density_mx_(self)

      ! Mulliken analysis (leaky)
      call put_Mulliken_populations_(self)
      call put_Mayer_bond_indices_(self)

      ! Moments
      call put_moments_(self)

      ! Atomic charges & moments
      call make_atom_partition_info_(self)
      call put_HA_moments_(self)  ! Wrong

      ! Fields and field gradients
      call put_E_field_at_nuclei_(self)
      call put_EFG_at_nuclei_(self)

      ! Clean up leak here
      call delete_atom_SCF_archives_(self)
   end subroutine


!   put_1e_properties_with_KM :: leaky
!   ! Put all the available one elctron properties
!      self :: INOUT
!
!   ENSURE(.density_mx.allocated,"no density")
!   ENSURE(.atom.allocated,"no atoms")
!   ENSURE(.basis.allocated,"no basis sets specified")
!
!      ! Make the density matrix (leaky)
!      .make_r_density_mx
!
!      ! Mulliken analysis (leaky)
!      .put_Mulliken_populations
!
!      ! Moments
!      .put_dipole
!      .put_quadrupole
!      .put_octupole
!
!      ! New charges
!      .put_KM_atom_multipoles
!
!      ! Atomic charges & moments
!      .put_HA_moments
!
!      ! Fields and field gradients
!      .put_E_field_at_nuclei
!      .put_EFG_at_nuclei
!
!      ! Standard plots
!
!      ! Clean up leak here
!      .delete_atom_SCF_archives
!
!   end

   subroutine put_1e_properties_for_groups(self)
   ! Put all the one electron properties for each group
      type(MOLECULE_TYPE), INOUT :: self
      INT :: g
   ENSURE(allocated(self%density_mx),"MOLECULE.PROP:put_1e_properties_for_groups ... no density")
   ENSURE(allocated(self%atom),"MOLECULE.PROP:put_1e_properties_for_groups ... no atoms")
   ENSURE(allocated(self%basis),"MOLECULE.PROP:put_1e_properties_for_groups ... no basis sets specified")
   ENSURE(allocated(self%atom_group),"MOLECULE.PROP:put_1e_properties_for_groups ... no .atom_group")
   ENSURE(allocated(self%mol),"MOLECULE.PROP:put_1e_properties_for_groups ... no .mol")
   ENSURE(size(self%mol)==size(self%atom_group),"MOLECULE.PROP:put_1e_properties_for_groups ... wrong size, .mol")
      call make_ANOs_and_interpolators_(self)

   ! .update_atom_groups
      do g = 1,size(self%atom_group)
         call flush_(stdout)
         call text_(stdout,"=======================")
         call text_(stdout,"Properties for group "//trim(to_str_(g)))
         call text_(stdout,"=======================")
         call flush_(stdout)
         call put_1e_properties_(self%mol(g))
      end do
   end subroutine


!  Mayer bond indices

   subroutine put_Mayer_bond_indices(self)
   ! Write out the Mayer bond indices
      type(MOLECULE_TYPE), INOUT :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: no_of_bonds,n_kind,k1,k2,n_bonds,i
      INT :: a,b,fa,la,fb,lb
      VEC(REAL,:), allocatable :: PD,Q,Z,V,F
      MAT(REAL,:,:), allocatable :: PS,BO
      MAT(INT,:,:), allocatable :: pair
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols,labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash,dsh1
      VEC(REAL,:), allocatable :: dist,bAB
      BIN :: has_unique_labels
   ENSURE(allocated(self%density_mx),"MOLECULE.PROP:put_Mayer_bond_indices ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.PROP:put_Mayer_bond_indices ... only for restricted densities")

      ! Anything to do?
      if (size(self%atom)<2) return
      no_of_bonds = no_of_bonds_(self%atom,ROBY_BOND_RANGE_FACTOR)
      if (no_of_bonds<1) return
      if (allocated(self%SCF_data)) then
         if (spinorbital_genre_(self%SCF_data)/="r ") then
            return  ! Do not do for anything but restricted for now
         end if
      end if

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self%atom)
      DIE_IF(NOT has_unique_labels,"MOLECULE.PROP:put_Mayer_bond_indices ... atoms are not uniquely labelled!")

      ! Overlap matrix (leaky)
      call make_overlap_mx_(self)

      ! Restricted Mulliken matrix
      call create_(PS,self%n_bf,self%n_bf)
      call to_product_of_(PS,self%density_mx%r,self%overlap_mx)

      ! Mulliken diagonal populations
      call create_(PD,self%n_bf)
      call put_diagonal_to_(PS,PD)

      ! Bond orders etc.
      call create_(BO,self%n_atom,self%n_atom)
      call create_(Q,self%n_atom)
      call create_(Z,self%n_atom)
      call create_(V,self%n_atom)
      call create_(F,self%n_atom)
      do a = 1,self%n_atom
         fa = self%first_basis_fn_for_atom(a)
         la = self%last_basis_fn_for_atom(a)

         ! Mullken population & charge
         Q(a) = sum(PD(fa:la))
         Z(a) = -Q(a)+self%atom(a)%nuclear_charge

         ! Bond orders
         do b = 1,self%n_atom
            fb = self%first_basis_fn_for_atom(b)
            lb = self%last_basis_fn_for_atom(b)
            BO(a,b) = trace_product_with_(PS(fa:la,fb:lb),PS(fb:lb,fa:la))
         end do

         ! Valences
         V(a) = TWO*Q(a)-BO(a,a)

         ! Make free valences
         F(a) = V(a)
         do b = 1,self%n_atom
            if (b/=a) then
               F(a) = F(a)-BO(a,b)
            end if
         end do
      end do

      ! Title
      call flush_(stdout)
      call text_(stdout,"=========================================")
      call text_(stdout,"Mayer populations, valences & bond orders")
      call text_(stdout,"=========================================")
      call flush_(stdout)
      call text_(stdout,". These bond indices and valences are defined in:")
      call text_(stdout,"  Mayer (1986) Int. J. QUantum Chem. 29 p 477-83.")
      call flush_(stdout)
      call text_(stdout,". Indices appear only for atom pairs which are connected")
      call text_(stdout,"  according to the Cambridge Structural Database (CSD).")
      call flush_(stdout)
      call show_(stdout,"No. of bonds =",no_of_bonds)
      call flush_(stdout)

      ! Table
      call create_(table,5)
      call set_heading_(table(1),"Atom")
      call set_width_from_(table(1),chemical_symbols_(self%atom))
      call set_heading_(table(2),"Mulliken")
      call set_subhead_(table(2),"Pop")
      call set_heading_(table(3),"Charge")
      call set_heading_(table(4),"  Mayer")
      call set_subhead_(table(4),"Valence")
      call set_heading_(table(5),"   Free")
      call set_subhead_(table(5),"Valence")

      ! Set data
      call create_(labels,self%n_atom,1)
      labels(:,1) = self%atom(:)%tag
      call set_values_(table(1),labels(:,1))
      call set_values_(table(2),Q)
      call set_values_(table(3),Z)
      call set_values_(table(4),V)
      call set_values_(table(5),F)

      ! Put
      call put_(table,label_rows=FALSE)

      ! Clean
      call destroy_(labels)
      call destroy_(table)

      ! Put bond indices
      ! Get atom Z list
      call make_atom_Z_list_(self%atom,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self%atom(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      call create_(table,8)

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),chemical_symbols_(self%atom))
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"--")
      call set_width_(table(2),2)
      call set_heading_(table(3),"B")
      call set_width_(table(3),table(1)%width)
      call set_left_justify_(table(3),TRUE)
      call set_heading_(table(4),"A")
      call set_width_from_(table(4),self%atom(:)%tag)
      call set_column_spacing_(table(5),0)
      call set_heading_(table(5),"--")
      call set_width_(table(5),2)
      call set_heading_(table(6),"B")
      call set_width_(table(6),table(4)%width)
      call set_left_justify_(table(6),TRUE)
      call set_heading_(table(7),"R(A--B)")
      call set_subheading_(table(7),"/Angs.")
      call set_width_from_(table(7),ONE)
      call set_heading_(table(8)," Bond")
      call set_subhead_(table(8),"order")
      call set_width_from_(table(8),TEN)

      ! Put out the headings
      call flush_(stdout)
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = k1,n_kind

         ! Get the bonded atoms per kind
            call get_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,ROBY_BOND_RANGE_FACTOR)
            dist = dist*ANGSTROM_PER_BOHR

         ! No of bonds in this class
            n_bonds = size(dist)
            if (n_bonds==0) then
               call destroy_(dist)
               call destroy_(pair)
               cycle
            end if

         ! Element symbols
            call create_(symbols,n_bonds,2)
            symbols = " "
            symbols(1,1) = chemical_symbol_(self%atom(Z_list(k1)%element(1)))
            symbols(1,2) = chemical_symbol_(self%atom(Z_list(k2)%element(1)))

         ! Dashes
            call create_(dash,n_bonds)
            call create_(dsh1,n_bonds)
            dash = "--"
            dsh1 = "  "
            dsh1(1) = "--"

         ! Labels
            call create_(labels,n_bonds,2)
            labels(:,1) = self%atom(pair(:,1))%tag
            labels(:,2) = self%atom(pair(:,2))%tag

         ! Populations & indices
            call create_(bAB,n_bonds)
            do i = 1,n_bonds
               bAB(i) = BO(pair(i,1),pair(i,2))
            end do

         ! Set table data
            call set_values_(table(1),symbols(:,1))
            call set_values_(table(2),dsh1)
            call set_values_(table(3),symbols(:,2))
            call set_values_(table(4),labels(:,1))
            call set_values_(table(5),dash)
            call set_values_(table(6),labels(:,2))
            call set_values_(table(7),dist)
            call set_values_(table(8),bAB)

         ! Put body
            call flush_(stdout)
            call put_body_(table,label_rows=FALSE)

         ! Clean
            call clear_columns_(table)
            call destroy_(bAB)
            call destroy_(labels)
            call destroy_(dsh1)
            call destroy_(dash)
            call destroy_(symbols)
            call destroy_(dist)
            call destroy_(pair)
         end do
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
      call destroy_(F)
      call destroy_(V)
      call destroy_(Q)
      call destroy_(BO)
      call destroy_(PD)
      call destroy_(PS)
   end subroutine


!  Mulliken populations

   subroutine put_Mulliken_populations_0(self)
   ! Output Mulliken population matrix.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: mulliken_mx
      MAT(REAL,:,:), allocatable :: DM,S2
      VEC(REAL,:), allocatable :: diagonals
      MAT(CPX,:,:), allocatable :: mulliken_mx2
      VEC(CPX,:), allocatable :: diagonals2
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:put_Mulliken_populations_0 ... no overlap matrix")
   ENSURE(allocated(self%density_mx),"MOLECULE.PROP:put_Mulliken_populations_0 ... no density matrix")

      ! Create space
      call create_(mulliken_mx,self%n_bf,self%n_bf)
      call create_(diagonals,self%n_bf)

      ! General complex case (LBxxx)
      if (is_allocated_with_genre_(self%density_mx,"gc")) then

         ! these objects are twice as large as usually
         call create_(S2,2*self%n_bf,2*self%n_bf)
         call create_(mulliken_mx2,2*self%n_bf,2*self%n_bf)
         call create_(diagonals2,2*self%n_bf)
         mulliken_mx2 = (ZERO,ZERO)
         diagonals2 = (ZERO,ZERO)
         S2 = ZERO
         call set_aa_block_to_(S2,self%overlap_mx)
         call set_bb_block_to_(S2,self%overlap_mx)
         call flush_(stdout)
         call text_(stdout,"   =========================")
         call text_(stdout,"   GCHF Mulliken populations")
         call text_(stdout,"   =========================")

         ! fill the general_complex mulliken matrix and diagonals
         call to_product_of_(mulliken_mx2,self%density_mx%gc,S2)
         call put_diagonal_to_(mulliken_mx2,diagonals2)

         ! Make use of the restricted mulliken matrix
         diagonals = REALIFY(diagonals2(1:self%n_bf))+REALIFY(diagonals2(self%n_bf+1:2*self%n_bf))
         mulliken_mx = REALIFY(mulliken_mx2(1:self%n_bf,1:self%n_bf))+REALIFY(mulliken_mx2(self%n_bf+1:2*self%n_bf,self%n_bf+1:2*self%n_bf))

         ! Put the atomic populations
         call put_Mulliken_populations_(self,mulliken_mx,diagonals)
         call destroy_(mulliken_mx2)
         call destroy_(diagonals2)
         call destroy_(S2)
         call flush_(stdout)
         call text_(stdout,"   ==============================")
         call text_(stdout,"   GCHF Mulliken spin populations")
         call text_(stdout,"   ==============================")

         ! Make GC mulliken matrix and diagonals
         call create_(DM,self%n_bf,self%n_bf)
         DM = ZERO
         call make_r_Sz_density_mx_(self,DM)
         mulliken_mx = ZERO
         diagonals = ZERO
         call to_product_of_(mulliken_mx,DM,self%overlap_mx)
         call put_diagonal_to_(mulliken_mx,diagonals)
         ! Put the atomic ao sz spin populations
         call put_Mulliken_spins_(self,mulliken_mx,diagonals)
         call destroy_(DM)

      ! Unrestricted case
      else if (is_allocated_with_genre_(self%density_mx,"u ")) then

         ! Temporary
         call create_(DM,self%n_bf,self%n_bf)

         ! Restricted density matrix
         DM = self%density_mx%a+self%density_mx%b

         ! Normal mulliken population
         call to_product_of_(mulliken_mx,DM,self%overlap_mx)
         call put_diagonal_to_(mulliken_mx,diagonals)

         ! Put the populations
         call put_Mulliken_populations_(self,mulliken_mx,diagonals)

         ! Unpaired density matrix
         DM = self%density_mx%a-self%density_mx%b
         call to_product_of_(mulliken_mx,DM,self%overlap_mx)
         call put_diagonal_to_(mulliken_mx,diagonals)

         ! Put the spin populations
         call put_Mulliken_spins_(self,mulliken_mx,diagonals)

         ! Clean up
         call destroy_(DM)

      ! Restricted case
      else if (is_allocated_with_genre_(self%density_mx,"r ")) then

         ! Make the restricted mulliken matrix
         call to_product_of_(mulliken_mx,self%density_mx%r,self%overlap_mx)
         call put_diagonal_to_(mulliken_mx,diagonals)

         ! Put the atomic populations
         call put_Mulliken_populations_(self,mulliken_mx,diagonals)
      else
         DIE("MOLECULE.PROP:put_Mulliken_populations_0 ... Mulliken populations not yet implemented for this case")
      end if

      ! Clean up
      call destroy_(mulliken_mx)
      call destroy_(diagonals)
   end subroutine

   subroutine put_Mulliken_populations_1(self,mulliken_mx,diagonals)
   ! Output the Mulliken populations
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), IN :: mulliken_mx
      VEC(REAL,:), IN :: diagonals
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: tags,lmom
      VEC(REAL,:), allocatable :: pops,chrg
      INT :: off,a,b,c,fa,la,n_bf

      ! Atomic data
      call create_(pops,self%n_atom)
      call create_(chrg,self%n_atom)

      ! Basis-fn data
      call create_(tags,self%n_bf)
      call create_(lmom,self%n_bf)
      off = 0
      do a = 1,self%n_atom

         ! Mulliken population & charge
         fa = self%first_basis_fn_for_atom(a)
         la = self%last_basis_fn_for_atom(a)
         pops(a) = sum(diagonals(fa:la))
         chrg(a) = self%atom(a)%nuclear_charge-pops(a)

         ! Symbols for each basis fn.
         do b = 1,self%atom(a)%basis%n_shell
            n_bf = self%atom(a)%basis%shell(b)%n_bf
            do c = 1,n_bf
               tags(off+c) = self%atom(a)%tag
               lmom(off+c) = self%atom(a)%basis%shell(b)%l_chr
            end do
            off = off+n_bf
         end do
      end do

      ! Title
      call flush_(stdout)
      call text_(stdout,"===================================")
      call text_(stdout,"Mulliken basis-function populations")
      call text_(stdout,"===================================")
      call flush_(stdout)
      call show_(stdout,"Trace of the Mulliken matrix =",trace_(mulliken_mx))
      call flush_(stdout)

      ! Create table
      call create_(table,3)
      call set_heading_(table(1),"Atom")
      call set_width_from_(table(1),chemical_symbols_(self%atom))
      call set_heading_(table(2),"Basis")
      call set_subhead_(table(2),"func.")
      call set_heading_(table(3),"Mulliken")
      call set_subhead_(table(3),"    Pop.")

      ! Set data
      call set_values_(table(1),tags)
      call set_values_(table(2),lmom)
      call set_values_(table(3),diagonals)

      ! Put
      call put_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(lmom)
      call destroy_(tags)
      call flush_(stdout)
      call text_(stdout,"===========================")
      call text_(stdout,"Mulliken atomic populations")
      call text_(stdout,"===========================")
      call flush_(stdout)
      call show_(stdout,"Sum of the Mulliken populations =",sum(pops))
      call show_(stdout,"Sum of the Mulliken charges     =",sum(chrg))
      call flush_(stdout)

      ! Create table
      call create_(table,3)
      call set_heading_(table(1),"Atom")
      call set_width_from_(table(1),chemical_symbols_(self%atom))
      call set_heading_(table(2),"Mulliken")
      call set_subhead_(table(2),"    Pop.")
      call set_heading_(table(3),"Mulliken")
      call set_subhead_(table(3),"  charge")

      ! Set data
      call create_(tags,self%n_atom)
      tags = self%atom(:)%tag
      call set_values_(table(1),tags)
      call set_values_(table(2),pops)
      call set_values_(table(3),chrg)

      ! Put
      call put_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(tags)
      call destroy_(chrg)
      call destroy_(pops)
   end subroutine

   subroutine put_Mulliken_spins(self,mulliken_mx,diagonals)
   ! Outputs the mulliken populations to the output file
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), IN :: mulliken_mx
      VEC(REAL,:), IN :: diagonals

   ! ENSURE(NOT .use_spherical_basis,"not implemented")
      VEC(REAL,:), allocatable :: atom_pop
      REAL :: pop
      INT :: o_count,a,b,c,n_comp

      ! Print out the spins for every basis function
      call flush_(stdout)
      call text_(stdout,"Mulliken spin population analysis: ")
      call dash_(stdout,int_fields=2,real_fields=1)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Type",int_width=TRUE)
      call put_(stdout,"Spin pop")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=1)
      o_count = 1
      call create_(atom_pop,self%n_atom)
      atom_pop = ZERO
      do a = 1,self%n_atom
         do b = 1,self%atom(a)%basis%n_shell
            if (self%use_spherical_basis) then
               n_comp = self%atom(a)%basis%shell(b)%n_sph
            else
               n_comp = self%atom(a)%basis%shell(b)%n_comp
            end if
            do c = 1,n_comp
               call put_(stdout,self%atom(a)%label,int_width=TRUE)
               call put_(stdout,self%atom(a)%basis%shell(b)%l_chr,int_width=TRUE)
               pop = diagonals(c+o_count-1)
               call put_(stdout,pop)
               atom_pop(a) = atom_pop(a)+pop
               call flush_(stdout)
            end do
            o_count = o_count+n_comp
         end do
      end do
      call dash_(stdout,int_fields=2,real_fields=1)
      call flush_(stdout)
      call show_(stdout,"Trace of the Mulliken spin matrix =",trace_(mulliken_mx))

      ! Print out the spins
      call flush_(stdout)
      call text_(stdout,"Atomic spin populations: ")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Spin")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      do a = 1,self%n_atom
         call put_(stdout,self%atom(a)%label,int_width=TRUE)
         call put_(stdout,atom_pop(a))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=1)

      ! Clean up
      call destroy_(atom_pop)
      call flush_(stdout)
   end subroutine

   subroutine get_gchf_na_nb(self)
   ! alpha and beta populations of occupied GCHF spinorbitals
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: S
      INT :: i,k,l
      CPX :: na,nb,atot,btot,II
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_gchf_na_nb ... no overlap matrix")
      S = self%overlap_mx
      II = (ZERO,ONE)
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout," GCHF na nb orbital populations ")
      call dash_(stdout,int_fields=1,real_fields=4)
      call put_(stdout,"i",int_width=TRUE)
      call put_(stdout,"na")
      call put_(stdout,"  ")
      call put_(stdout,"nb")
      call put_(stdout,"  ")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      atot = (ZERO,ZERO)
      btot = (ZERO,ZERO)
      do i = 1,self%n_e
         na = (ZERO,ZERO)
         nb = (ZERO,ZERO)
         do k = 1,self%n_bf
            do l = 1,self%n_bf

            ! sigma z overlap
               na = na+conjg(self%MOs%gc(k,i))*S(k,l)*(self%MOs%r(l,i))
               nb = nb+conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*(self%MOs%r(l+self%n_bf,i))
            end do
         end do
         call put_(stdout,i)
         call put_(stdout,na)
         call put_(stdout,nb)
         call flush_(stdout)
         atot = atot+na
         btot = btot+nb
      end do
      call dash_(stdout,int_fields=1,real_fields=4)
      call put_(stdout,"total:",int_width=TRUE)
      call put_(stdout,atot)
      call put_(stdout,btot)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
   end subroutine


!  Spin expectation values

   subroutine get_uhf_sx_sy_sz(self)
   ! evaluation of expectation values of sigma_x, sigma_y and sigma_z
   ! n_x=0, n_y= and n_z= number of open shell electrons
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: S
      MAT(REAL,:,:), allocatable, target :: orbs
      VEC(REAL,:), PTR :: CMOi
      INT :: i,k,l
      REAL :: z,x,y
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_uhf_sx_sy_sz ... no overlap matrix")
      S = self%overlap_mx
      call create_(orbs,2*self%n_bf,self%n_e)
      orbs = ZERO
      do i = 1,self%n_a
         orbs(1:self%n_bf,i) = self%MOs%a(:,i)
      end do
      do i = 1,self%n_b
         orbs(self%n_bf+1:2*self%n_bf,self%n_a+i) = self%MOs%b(:,i)
      end do
      x = ZERO; y = ZERO; z = ZERO
      do i = 1,self%n_e
         CMOi => orbs(:,i)
         do k = 1,self%n_bf
            do l = 1,self%n_bf

            ! sigma x overlap
               x = x+(CMOi(k+self%n_bf))*S(k,l)*(CMOi(l))
               x = x+(CMOi(k))*S(k,l)*(CMOi(l+self%n_bf))
            ! sigma y overlap
               y = y+(CMOi(k+self%n_bf))*S(k,l)*CMOi(l)
               y = y-(CMOi(k))*S(k,l)*CMOi(l+self%n_bf)
            ! sigma z overlap
               z = z+(CMOi(k))*S(k,l)*(CMOi(l))
               z = z-(CMOi(k+self%n_bf))*S(k,l)*(CMOi(l+self%n_bf))
            end do
         end do
      end do
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"UHF expectation values of Pauli spin matrices,")
      call text_(stdout,"n_x=0, n_y=0 and n_z= number of open shell electrons:")
      call show_(stdout,"n_x = <sigma_x> =",x)
      call show_(stdout,"n_y = <sigma_y> =",y)
      call show_(stdout,"n_z = <sigma_z> =",z)
      call flush_(stdout)
      call destroy_(orbs)
   end subroutine

   subroutine get_gchf_sx_sy_sz(self)
   ! evaluation of expectation values of sigma_x, sigma_y and sigma_z
   ! n_x=0, n_y= and n_z= number of open shell electrons
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: S
      INT :: i,k,l
      CPX :: z,x,y,II
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_gchf_sx_sy_sz ... no overlap matrix")
      S = self%overlap_mx
      II = (ZERO,ONE)
      x = (ZERO,ZERO)
      y = (ZERO,ZERO)
      z = (ZERO,ZERO)
      do i = 1,self%n_e
         do k = 1,self%n_bf
            do l = 1,self%n_bf

         ! sigma x overlap
               x = x+conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*self%MOs%r(l,i)
               x = x+conjg(self%MOs%gc(k,i))*S(k,l)*self%MOs%r(l+self%n_bf,i)
         ! sigma y overlap
               y = y+conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*self%MOs%r(l,i)
               y = y-conjg(self%MOs%gc(k,i))*S(k,l)*self%MOs%r(l+self%n_bf,i)
         ! sigma z overlap
               z = z+conjg(self%MOs%gc(k,i))*S(k,l)*self%MOs%r(l,i)
               z = z-conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*self%MOs%r(l+self%n_bf,i)
            end do
         end do
      end do
      call flush_(stdout)
      call text_(stdout,"GCHF expectation values of Pauli spin matrices:")
      call show_(stdout,"n_x = <sigma_x> =",x)
      call show_(stdout,"n_y = <sigma_y> =",y)
      call show_(stdout,"n_z = <sigma_z> =",z)
      call flush_(stdout)
   end subroutine

   subroutine get_uhf_s2(self)
   ! s2 uhf expectation values 
   ! evaluated directly for the x, y and z components
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: S
      MAT(REAL,:,:), allocatable :: sx,sy,sz
      MAT(REAL,:,:), allocatable, target :: orbs
      VEC(REAL,:), PTR :: CMOi,CMOj
      INT :: i,j,k,l
      REAL :: x,y,z
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_uhf_s2 ... no overlap matrix")
      S = self%overlap_mx
      call create_(sx,self%n_e,self%n_e)
      call create_(sy,self%n_e,self%n_e)
      call create_(sz,self%n_e,self%n_e)
      call create_(orbs,2*self%n_bf,self%n_e)
      orbs = ZERO
      do i = 1,self%n_a
         orbs(1:self%n_bf,i) = self%MOs%a(:,i)
      end do
      do i = 1,self%n_b
         orbs(self%n_bf+1:2*self%n_bf,self%n_a+i) = self%MOs%b(:,i)
      end do
      sx = ZERO
      sy = ZERO
      sz = ZERO
      do i = 1,self%n_e
         do j = 1,self%n_e
            CMOi => orbs(:,i)
            CMOj => orbs(:,j)
            do k = 1,self%n_bf
               do l = 1,self%n_bf

            ! sigma x density 
                  sx(i,j) = sx(i,j)+(CMOi(k+self%n_bf))*S(k,l)*(CMOj(l))
                  sx(i,j) = sx(i,j)+(CMOi(k))*S(k,l)*(CMOj(l+self%n_bf))
            ! sigma y density
                  sy(i,j) = sy(i,j)+(CMOi(k+self%n_bf))*S(k,l)*CMOj(l)
                  sy(i,j) = sy(i,j)-(CMOi(k))*S(k,l)*CMOj(l+self%n_bf)
            ! sigma z density
                  sz(i,j) = sz(i,j)+(CMOi(k))*S(k,l)*(CMOj(l))
                  sz(i,j) = sz(i,j)-(CMOi(k+self%n_bf))*S(k,l)*(CMOj(l+self%n_bf))
               end do
            end do
         end do
      end do
      x = ZERO
      y = ZERO
      z = ZERO
      do i = 1,self%n_e
         do j = i+1,self%n_e
            x = x-sx(i,j)*sx(j,i)*TWO+sx(i,i)*sx(j,j)*TWO
            y = y+sy(i,j)*sy(j,i)*TWO-sy(i,i)*sy(j,j)*TWO  ! i*i=-1
            z = z-sz(i,j)*sz(j,i)*TWO+sz(i,i)*sz(j,j)*TWO
         end do
      end do
      call flush_(stdout)
      call text_(stdout,"S^2 = Sx^2 + Sy^2 + Sz^2")
      call show_(stdout,"Sx^2 =",(self%n_e+x)/FOUR)
      call show_(stdout,"Sy^2 =",(self%n_e+y)/FOUR)
      call show_(stdout,"Sz^2 =",(self%n_e+z)/FOUR)
      call show_(stdout,"S^2  =",(THREE*self%n_e+x+y+z)/FOUR)
      call flush_(stdout)
      call destroy_(sx)
      call destroy_(sy)
      call destroy_(sz)
   end subroutine

   subroutine get_gchf_s2(self)
   ! S2 gchf expectation value evaluated via Sx^2 + Sy^2 + Sz^2 contributions
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: S
      MAT(CPX,:,:), allocatable :: sx,sy,sz
      INT :: i,j,k,l
      CPX :: x,y,z
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_gchf_s2 ... no overlap matrix")
      S = self%overlap_mx
      call create_(sx,self%n_e,self%n_e)
      call create_(sy,self%n_e,self%n_e)
      call create_(sz,self%n_e,self%n_e)
      call flush_(stdout)
      sx = (ZERO,ZERO)
      sy = (ZERO,ZERO)
      sz = (ZERO,ZERO)
      do i = 1,self%n_e
         do j = 1,self%n_e
            do k = 1,self%n_bf
               do l = 1,self%n_bf

            ! sigma x squared density
                  sx(i,j) = sx(i,j)+conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*(self%MOs%r(l,j))
                  sx(i,j) = sx(i,j)+conjg(self%MOs%gc(k,i))*S(k,l)*(self%MOs%r(l+self%n_bf,j))
            ! sigma y density
                  sy(i,j) = sy(i,j)+conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*self%MOs%r(l,j)
                  sy(i,j) = sy(i,j)-conjg(self%MOs%gc(k,i))*S(k,l)*self%MOs%r(l+self%n_bf,j)
            ! sigma z density
                  sz(i,j) = sz(i,j)+conjg(self%MOs%gc(k,i))*S(k,l)*(self%MOs%r(l,j))
                  sz(i,j) = sz(i,j)-conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*(self%MOs%r(l+self%n_bf,j))
               end do
            end do
         end do
      end do
      x = (ZERO,ZERO)
      y = (ZERO,ZERO)
      z = (ZERO,ZERO)
      do i = 1,self%n_e
         do j = i+1,self%n_e
            x = x-sx(i,j)*sx(j,i)*TWO+sx(i,i)*sx(j,j)*TWO
            y = y+sy(i,j)*sy(j,i)*TWO-sy(i,i)*sy(j,j)*TWO  ! i*i=-1
            z = z-sz(i,j)*sz(j,i)*TWO+sz(i,i)*sz(j,j)*TWO
         end do
      end do
      call flush_(stdout)
      call text_(stdout,"S^2 = Sx^2 + Sy^2 + Sz^2")
      call show_(stdout,"Sx^2 =",(self%n_e+x)/FOUR)
      call show_(stdout,"Sy^2 =",(self%n_e+y)/FOUR)
      call show_(stdout,"Sz^2 =",(self%n_e+z)/FOUR)
      call show_(stdout,"S^2  =",(THREE*self%n_e+x+y+z)/FOUR)
      call flush_(stdout)
      call destroy_(sx)
      call destroy_(sy)
      call destroy_(sz)
   end subroutine

   subroutine get_uhf_s2_orig(self)
   ! s2 uhf expectation value via the overlap of alpha and beta orbs.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: S
      MAT(REAL,:,:), allocatable :: aa,bb,ab,ba
      MAT(REAL,:,:), allocatable, target :: orbs
      VEC(REAL,:), PTR :: CMOi,CMOj
      INT :: i,j,k,l
      REAL :: x,y,z,na,nb
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_uhf_s2_orig ... no overlap matrix")
      S = self%overlap_mx
      call create_(aa,self%n_e,self%n_e); call create_(bb,self%n_e,self%n_e)
      call create_(ba,self%n_e,self%n_e); call create_(ab,self%n_e,self%n_e)
      call flush_(stdout)
      call text_(stdout,"S^2 = (S+S- + S-S+)/2 + Sz^2")
      aa = ZERO
      ab = ZERO
      ba = ZERO
      bb = ZERO
      call create_(orbs,2*self%n_bf,self%n_e)
      orbs = ZERO
      do i = 1,self%n_a
         orbs(1:self%n_bf,i) = self%MOs%a(:,i)
      end do
      do i = 1,self%n_b
         orbs(self%n_bf+1:2*self%n_bf,self%n_a+i) = self%MOs%b(:,i)
      end do
      do i = 1,self%n_e
         do j = 1,self%n_e
            CMOi => orbs(:,i)
            CMOj => orbs(:,j)
            do k = 1,self%n_bf
               do l = 1,self%n_bf
                  ba(i,j) = ba(i,j)+(CMOi(k+self%n_bf))*S(k,l)*(CMOj(l))
                  ab(i,j) = ab(i,j)+(CMOi(k))*S(k,l)*(CMOj(l+self%n_bf))
                  aa(i,j) = aa(i,j)+(CMOi(k))*S(k,l)*(CMOj(l))
                  bb(i,j) = bb(i,j)+(CMOi(k+self%n_bf))*S(k,l)*(CMOj(l+self%n_bf))
               end do
            end do
         end do
      end do
      na = ZERO
      nb = ZERO
      do i = 1,self%n_e
         CMOi => orbs(:,i)
         do k = 1,self%n_bf
            do l = 1,self%n_bf
               na = na+(CMOi(k))*S(k,l)*(CMOi(l))
               nb = nb+(CMOi(k+self%n_bf))*S(k,l)*(CMOi(l+self%n_bf))
            end do
         end do
      end do
      call text_(stdout,"alpha (na) and beta (nb) populations:")
      call show_(stdout,"na =",na)
      call show_(stdout,"nb =",nb)
      x = ZERO
      y = ZERO
      z = ZERO
      do i = 1,self%n_e
         do j = 1,self%n_e
            x = x+ab(i,i)*ba(j,j)-ab(i,j)*ba(j,i)
            y = y+ba(i,i)*ab(j,j)-ba(i,j)*ab(j,i)
            z = z+(aa(i,j)-bb(i,j))*(aa(j,i)-bb(j,i))
         end do
      end do
      call show_(stdout,"S2 exact           =",(na-nb)*(na-nb+TWO)/FOUR)
      call show_(stdout,"S+S-/2             =",(na+x)/TWO)
      call show_(stdout,"S-S+/2             =",(nb+y)/TWO)
      call show_(stdout,"Sz^2               =",((na-nb)**2+self%n_e-z)/FOUR)
      call show_(stdout,"S2                 =",((na-nb)**2+self%n_e-z)/FOUR+(na+x+nb+y)/TWO)
      call show_(stdout,"spin_contamination =",nb+(x+y)/TWO+(self%n_e-z)/FOUR)
      call destroy_(aa); call destroy_(bb)
      call destroy_(ab); call destroy_(ba)
      call destroy_(orbs)
   end subroutine

   subroutine get_gchf_s2_orig(self)
   ! S2 gchf expectation value evaluated via S+-,  S-+ and Sz^2 contributions
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: S
      MAT(CPX,:,:), allocatable :: aa,bb,ab,ba
      INT :: i,j,k,l
      CPX :: x,y,z,na,nb
      REAL :: S2,S2e
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_gchf_s2_orig ... no overlap matrix")
      S = self%overlap_mx
      call create_(aa,self%n_e,self%n_e); call create_(bb,self%n_e,self%n_e)
      call create_(ba,self%n_e,self%n_e); call create_(ab,self%n_e,self%n_e)
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"S^2 = (S+S- + S-S+)/2 + Sz^2")
      aa = (ZERO,ZERO)
      ab = (ZERO,ZERO)
      ba = (ZERO,ZERO)
      bb = (ZERO,ZERO)
      do i = 1,self%n_e
         do j = 1,self%n_e
            do k = 1,self%n_bf
               do l = 1,self%n_bf
                  ba(i,j) = ba(i,j)+conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*(self%MOs%r(l,j))
                  ab(i,j) = ab(i,j)+conjg(self%MOs%gc(k,i))*S(k,l)*(self%MOs%r(l+self%n_bf,j))
                  aa(i,j) = aa(i,j)+conjg(self%MOs%gc(k,i))*S(k,l)*(self%MOs%r(l,j))
                  bb(i,j) = bb(i,j)+conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*(self%MOs%r(l+self%n_bf,j))
               end do
            end do
         end do
      end do
      na = (ZERO,ZERO)
      nb = (ZERO,ZERO)
      do i = 1,self%n_e
         do k = 1,self%n_bf
            do l = 1,self%n_bf
               na = na+conjg(self%MOs%gc(k,i))*S(k,l)*(self%MOs%r(l,i))
               nb = nb+conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*(self%MOs%r(l+self%n_bf,i))
            end do
         end do
      end do
      call text_(stdout,"alpha (na) and beta (nb) populations:")
      call show_(stdout,"na =",na)
      call show_(stdout,"nb =",nb)
      x = (ZERO,ZERO)
      y = (ZERO,ZERO)
      z = (ZERO,ZERO)
      do i = 1,self%n_e
         do j = 1,self%n_e
            x = x+ab(i,i)*ba(j,j)-ab(i,j)*ba(j,i)
            y = y+ba(i,i)*ab(j,j)-ba(i,j)*ab(j,i)
            z = z+(aa(i,j)-bb(i,j))*(aa(j,i)-bb(j,i))
         end do
      end do
      S2e = (self%n_a-self%n_b)*(self%n_a-self%n_b+TWO)/FOUR
      S2 = REALIFY(((na-nb)**2+self%n_e-z)/FOUR+(na+x+nb+y)/TWO)
      call show_(stdout,"S2 exact           =",(self%n_a-self%n_b)*(self%n_a-self%n_b+TWO)/FOUR)
      call show_(stdout,"S+S-/2             =",(na+x)/TWO)
      call show_(stdout,"S-S+/2             =",(nb+y)/TWO)
      call show_(stdout,"Sz^2               =",((na-nb)**2+self%n_e-z)/FOUR)
      call show_(stdout,"S2                 =",((na-nb)**2+self%n_e-z)/FOUR+(na+x+nb+y)/TWO)
      call show_(stdout,"spin_contamination =",S2-S2e)
      call destroy_(aa); call destroy_(bb)
      call destroy_(ab); call destroy_(ba)
   end subroutine

   subroutine get_gchf_s2_contributions(self)
   ! S2 gchf expectation value decomposition into various 
   ! contribution and matrix whose eigenvectors give 
   ! directions of optimal collinearity 
   ! see P. Cassam-Chena\"{\i}, Theor Chem Acc (2015) 134:125
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: S
      MAT(CPX,:,:), allocatable :: aa,bb,ab,ba
      MAT(CPX,:,:), allocatable :: sxmat,symat,szmat
      MAT(CPX,:,:), allocatable :: amat
      INT :: i,j,k,l
      CPX :: x,y,z,na,nb
      CPX :: ii,sx,sy,sz
      CPX :: sxsx,sxsy,sxsz
      CPX :: sysx,sysy,sysz
      CPX :: szsx,szsy,szsz
      REAL :: S2,S2e
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_gchf_s2_contributions ... no overlap matrix")
      S = self%overlap_mx
      ii = (ZERO,ONE)
      call create_(amat,3,3)
      call create_(aa,self%n_e,self%n_e); call create_(bb,self%n_e,self%n_e)
      call create_(ba,self%n_e,self%n_e); call create_(ab,self%n_e,self%n_e)
      call create_(sxmat,self%n_e,self%n_e); call create_(symat,self%n_e,self%n_e); call create_(szmat,self%n_e,self%n_e)
      aa = (ZERO,ZERO)
      ab = (ZERO,ZERO)
      ba = (ZERO,ZERO)
      bb = (ZERO,ZERO)
      sxmat = (ZERO,ZERO)
      symat = (ZERO,ZERO)
      szmat = (ZERO,ZERO)
      do i = 1,self%n_e
         do j = 1,self%n_e
            do k = 1,self%n_bf
               do l = 1,self%n_bf
                  ba(i,j) = ba(i,j)+conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*(self%MOs%r(l,j))
                  ab(i,j) = ab(i,j)+conjg(self%MOs%gc(k,i))*S(k,l)*(self%MOs%r(l+self%n_bf,j))
                  aa(i,j) = aa(i,j)+conjg(self%MOs%gc(k,i))*S(k,l)*(self%MOs%r(l,j))
                  bb(i,j) = bb(i,j)+conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*(self%MOs%r(l+self%n_bf,j))
               end do
            end do
            sxmat(i,j) = sxmat(i,j)+ab(i,j)+ba(i,j)
            symat(i,j) = symat(i,j)+ab(i,j)-ba(i,j)
            szmat(i,j) = szmat(i,j)+aa(i,j)-bb(i,j)
         end do
      end do
      sxmat = sxmat/TWO
      symat = symat/TWO/ii
      szmat = szmat/TWO
      na = (ZERO,ZERO)
      nb = (ZERO,ZERO)
      do i = 1,self%n_e
         na = na+aa(i,i)
         nb = nb+bb(i,i)
      end do
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"alpha (na) and beta (nb) populations:")
      call show_(stdout,"na =",na)
      call show_(stdout,"nb =",nb)
      amat = ZERO
      amat(1,1) = self%n_e/FOUR
      amat(2,2) = self%n_e/FOUR
      amat(3,3) = self%n_e/FOUR
      x = (ZERO,ZERO)
      y = (ZERO,ZERO)
      z = (ZERO,ZERO)
      sx = (ZERO,ZERO)
      sy = (ZERO,ZERO)
      sz = (ZERO,ZERO)
      sxsx = (ZERO,ZERO)
      sxsy = (ZERO,ZERO)
      sxsz = (ZERO,ZERO)
      sysx = (ZERO,ZERO)
      sysy = (ZERO,ZERO)
      sysz = (ZERO,ZERO)
      szsx = (ZERO,ZERO)
      szsy = (ZERO,ZERO)
      szsz = (ZERO,ZERO)
      do i = 1,self%n_e
         do j = 1,self%n_e
            x = x+ab(i,i)*ba(j,j)
            y = y-ba(i,j)*ab(j,i)
            z = z+(aa(i,j)-bb(i,j))*(aa(j,i)-bb(j,i))
            amat(1,1) = amat(1,1)-conjg(sxmat(i,j))*sxmat(i,j)
            amat(2,2) = amat(2,2)-conjg(symat(i,j))*symat(i,j)
            amat(3,3) = amat(3,3)-conjg(szmat(i,j))*szmat(i,j)
            amat(1,2) = amat(1,2)-conjg(sxmat(i,j))*symat(i,j)
            amat(2,1) = amat(2,1)-conjg(symat(i,j))*sxmat(i,j)
            amat(1,3) = amat(1,3)-conjg(sxmat(i,j))*szmat(i,j)
            amat(3,1) = amat(3,1)-conjg(szmat(i,j))*sxmat(i,j)
            amat(2,3) = amat(2,3)-conjg(symat(i,j))*szmat(i,j)
            amat(3,2) = amat(3,2)-conjg(szmat(i,j))*symat(i,j)
            sxsy = sxsy+sxmat(i,i)*symat(j,j)-conjg(sxmat(i,j))*symat(i,j)
            sysx = sysx+symat(i,i)*sxmat(j,j)-conjg(symat(i,j))*sxmat(i,j)
            sxsz = sxsz+sxmat(i,i)*szmat(j,j)-conjg(sxmat(i,j))*szmat(i,j)
            szsx = szsx+szmat(i,i)*sxmat(j,j)-conjg(szmat(i,j))*sxmat(i,j)
            sysz = sysz+symat(i,i)*szmat(j,j)-conjg(symat(i,j))*szmat(i,j)
            szsy = szsy+szmat(i,i)*symat(j,j)-conjg(szmat(i,j))*symat(i,j)
         end do
         sxsy = sxsy+ii/TWO*szmat(i,i)
         sysx = sysx-ii/TWO*szmat(i,i)
         sxsz = sxsz-ii/TWO*symat(i,i)
         szsx = szsx+ii/TWO*symat(i,i)
         sysz = sysz+ii/TWO*sxmat(i,i)
         szsy = szsy-ii/TWO*sxmat(i,i)
      end do
      do i = 1,self%n_e
         sx = sx+sxmat(i,i)
         sy = sy+symat(i,i)
         sz = sz+szmat(i,i)
         do j = i+1,self%n_e
            sxsx = sxsx+sxmat(i,i)*sxmat(j,j)-sxmat(i,j)*sxmat(j,i)
            sysy = sysy+symat(i,i)*symat(j,j)-symat(i,j)*symat(j,i)
            szsz = szsz+szmat(i,i)*szmat(j,j)-szmat(i,j)*szmat(j,i)
         end do
      end do
      sxsx = sxsx*TWO+self%n_e/FOUR
      sysy = sysy*TWO+self%n_e/FOUR
      szsz = szsz*TWO+self%n_e/FOUR
      S2e = (self%n_a-self%n_b)*(self%n_a-self%n_b+TWO)/FOUR
      S2 = REALIFY(((na-nb)**2+self%n_e-z)/FOUR+(na+TWO*x+nb+TWO*y)/TWO)
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"Expectation values of spin operator components and their products:")
   !  stdout.show(" x                ",x)
   !  stdout.show(" y                ",y)
   !  stdout.show(" z                ",z)
      call show_(stdout,"Sx     =",sx)
      call show_(stdout,"Sy     =",sy)
      call show_(stdout,"Sz     =",sz)
      call show_(stdout,"SxSx   =",sxsx)
      call show_(stdout,"SxSy   =",sxsy)
      call show_(stdout,"SxSz   =",sxsz)
      call show_(stdout,"SySx   =",sysx)
      call show_(stdout,"SySy   =",sysy)
      call show_(stdout,"SySz   =",sysz)
      call show_(stdout,"SzSx   =",szsx)
      call show_(stdout,"SzSy   =",szsy)
      call show_(stdout,"SzSz   =",szsz)
      call show_(stdout,"S+S-/2 =",(na+x+y)/TWO)
      call show_(stdout,"S-S+/2 =",(nb+x+y)/TWO)
   !  stdout.show("(na/2-nb/2)^2     ",((na-nb)**2)/FOUR)  
   !  stdout.show("Sz^2              ",((na-nb)**2 +.n_e - z)/FOUR)  
   !  stdout.show("S2 exact          ",(.n_a -.n_b)*(.n_a-.n_b+TWO)/FOUR)
   !  stdout.show("S2                ",((na-nb)**2 + .n_e - z)/FOUR + (na + TWO*x + nb + TWO*y)/TWO)  
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"S^2 expectation value contributions:")
      call show_(stdout,"(na-nb)(na-nb+2)/4 =",(na-nb)*(na-nb+2)/FOUR)
   !  stdout.show("(.n_e - z)/4      ",(.n_e - z)/FOUR)  
      call show_(stdout,"z-noncollinearity =",(self%n_e-z)/FOUR)
   !  stdout.show(" x                ",x)  
      call show_(stdout,"x,y-nonperpendicularity =",x)
   !  stdout.show("spin_contamination",S2-S2e)
   !  stdout.show("(nb+y)            ",(nb+y))  
      call show_(stdout,"spin-contamination =",(nb+y))
      call show_(stdout,"<S^2>              =",sxsx+sysy+szsz)
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"A-matrix (for reorientation in an optimal collinearity direction")
      call put_(stdout,amat)
      call destroy_(aa); call destroy_(bb)
      call destroy_(ab); call destroy_(ba)
      call destroy_(amat)
      call destroy_(sxmat); call destroy_(symat); call destroy_(szmat)
   end subroutine

   subroutine get_uhf_kramers(self)
   !  analyse uhf kramers pairs 
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: S
      MAT(REAL,:,:), allocatable :: kramersA
      MAT(REAL,:,:), allocatable :: kramersB
      INT :: i,j,k,l
      REAL :: X1,X2,X3,X4
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_uhf_kramers ... no overlap matrix")
      call create_(kramersA,self%n_b,self%n_a)
      call create_(kramersB,self%n_a,self%n_b)
      kramersA = ZERO; kramersB = ZERO
      X1 = ZERO; X2 = ZERO; X3 = ZERO; X4 = ZERO
      S = self%overlap_mx
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"Kramers pairs analysis - UHF case:")
      call flush_(stdout)
      call text_(stdout,"Overlaps of individual MOs:")

      ! Table head
      call dash_(stdout,int_fields=2,real_fields=2)
      call put_(stdout,"i",int_width=TRUE)
      call put_(stdout,"j",int_width=TRUE)
      call put_(stdout,"|<bj|ai>|")
      call put_(stdout,"|<ai|bj>|")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=2)
      do i = 1,self%n_a
         do j = 1,self%n_b
            do k = 1,self%n_bf
               do l = 1,self%n_bf
                  kramersA(j,i) = kramersA(j,i)+self%MOs%b(k,j)*S(k,l)*self%MOs%a(l,i)
                  kramersB(i,j) = kramersB(i,j)-self%MOs%a(k,i)*S(k,l)*self%MOs%b(l,j)
               end do
            end do
            if (abs(kramersB(i,j))>0.0001 OR abs(kramersA(j,i))>0.0001) then
               call put_(stdout,i)
               call put_(stdout,j)
               call put_(stdout,abs(kramersA(j,i)))
               call put_(stdout,abs(kramersB(i,j)))
               call flush_(stdout)
            end if
         end do
      end do

      ! end table
      call dash_(stdout,int_fields=2,real_fields=2)
      call flush_(stdout)
      call text_(stdout,"Overlaps of one alpha MO with all the other beta MOs:")

      ! table head
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"i",int_width=TRUE)
      call put_(stdout,"sumj <ai|bj>^2")
!     stdout.put("<sum(ai)|bj>")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      do i = 1,self%n_a
         X1 = ZERO; X2 = ZERO
         do j = 1,self%n_b
            X1 = X1+kramersA(j,i)*kramersA(j,i)
            X2 = X2+kramersB(i,j)*kramersB(i,j)
            X3 = X3+kramersA(j,i)*kramersA(j,i)
            X4 = X4+kramersB(i,j)*kramersB(i,j)
         end do
         call put_(stdout,i)
         call put_(stdout,X1)
      ! stdout.put(X2)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"sum",int_width=TRUE)
      call put_(stdout,X3)
   ! stdout.put(X4)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      call destroy_(kramersA)
      call destroy_(kramersB)
   end subroutine

   subroutine get_gchf_kramers(self)
   !  analyse kramers pairs 
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: S
      MAT(CPX,:,:), allocatable :: kramers
      INT :: i,j,k,l
      CPX :: X1,X2,X3,X4,II
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_gchf_kramers ... no overlap matrix")
      call create_(kramers,self%n_e,self%n_e)
      kramers = (ZERO,ZERO)
      II = (ZERO,ONE)
      X1 = (ZERO,ZERO); X2 = (ZERO,ZERO); X3 = (ZERO,ZERO); X4 = (ZERO,ZERO)
      S = self%overlap_mx
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"Kramers pairs analysis - GCHF case:")
      call flush_(stdout)

      ! the phase is random - hence bad for testing purposes
      call text_(stdout,"Overlaps of individual MOs:")

      ! table head
      call dash_(stdout,int_fields=2,real_fields=2)
      call put_(stdout,"i",int_width=TRUE)
      call put_(stdout,"j",int_width=TRUE)
      call put_(stdout,"|<i|Kj>|")
      call put_(stdout,"|<j|Ki>|")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=2)
      do i = 1,self%n_e
         do j = i,self%n_e
            do k = 1,self%n_bf
               do l = 1,self%n_bf
                  kramers(i,j) = kramers(i,j)-conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*self%MOs%r(l,j)
                  kramers(i,j) = kramers(i,j)+conjg(self%MOs%gc(k,i))*S(k,l)*self%MOs%r(l+self%n_bf,j)
                  kramers(j,i) = kramers(j,i)-conjg(self%MOs%gc(k+self%n_bf,j))*S(k,l)*self%MOs%r(l,i)
                  kramers(j,i) = kramers(j,i)+conjg(self%MOs%gc(k,j))*S(k,l)*self%MOs%r(l+self%n_bf,i)
               end do
            end do
            if (abs(kramers(i,j))>0.0001 OR abs(kramers(j,i))>0.0001) then
               call put_(stdout,i)
               call put_(stdout,j)
               call put_(stdout,abs(kramers(i,j)))
               call put_(stdout,abs(kramers(j,i)))
               call flush_(stdout)
            end if
         end do
      end do
      call dash_(stdout,int_fields=2,real_fields=2)
      call flush_(stdout)
      call text_(stdout,"Overlaps of MOi with all the other Kramres MOj:")
      call dash_(stdout,int_fields=1,real_fields=2)
      call put_(stdout,"i",int_width=TRUE)
      call put_(stdout,"sumj |<i|Kj>|^2")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      do i = 1,self%n_e
         X1 = (ZERO,ZERO)
         do j = 1,self%n_e
            X1 = X1+kramers(i,j)*conjg(kramers(i,j))
         end do
         call put_(stdout,i)
         call put_(stdout,abs(X1))
         call flush_(stdout)
         X3 = X3+X1
      end do
      call dash_(stdout,int_fields=1,real_fields=2)
      call put_(stdout,"sum",int_width=TRUE)
      call put_(stdout,X3)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      call show_(stdout," Number of Kramers pairs                            =",X3/TWO)
      call show_(stdout," Spin contamination <up|down> ~ analog of UHF <i,j> =",self%n_b-X3/TWO)
      call destroy_(kramers)
   end subroutine

   subroutine get_uhf_kramers_2(self)
   !  analyse uhf kramers pairs 
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: S
      MAT(REAL,:,:), allocatable :: kramersA
      MAT(REAL,:,:), allocatable :: kramersB
      INT :: i,j,k,l,lb,la
      REAL :: X1,X2,X3,X4
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_uhf_kramers_2 ... no overlap matrix")
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"Kramers pairs analysis - UHF case:")
      call text_(stdout,"This routine is using more beta orbitals, to identify your Kramers pairs (open shells):")
      call text_(stdout,"Note: You need the orbital energies !!!!")
      lb = self%n_a+(self%n_a-self%n_b)*2  ! instead of .n_b
      la = self%n_a  ! .n_a
      call create_(kramersA,lb,la)
      call create_(kramersB,la,lb)
      kramersA = ZERO; kramersB = ZERO
      X1 = ZERO; X2 = ZERO; X3 = ZERO; X4 = ZERO
      S = self%overlap_mx
      call flush_(stdout)
      call text_(stdout,"Overlaps of individual MOs:")

      ! table head
      call dash_(stdout,int_fields=2,real_fields=4)
      call put_(stdout,"j",int_width=TRUE)
      call put_(stdout,"i",int_width=TRUE)
      call put_(stdout,"|<bj|ai>|")
      call put_(stdout,"|<ai|bj>|")
      call put_(stdout,"eai")
      call put_(stdout,"ebj")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=4)
      do j = 1,lb  ! instead of .n_b
         do i = 1,la
            do k = 1,self%n_bf
               do l = 1,self%n_bf
                  kramersA(j,i) = kramersA(j,i)+self%MOs%b(k,j)*S(k,l)*self%MOs%a(l,i)
                  kramersB(i,j) = kramersB(i,j)-self%MOs%a(k,i)*S(k,l)*self%MOs%b(l,j)
               end do
            end do
            if (abs(kramersB(i,j))>0.005 OR abs(kramersA(j,i))>0.005) then
               call put_(stdout,j)
               call put_(stdout,i)
               call put_(stdout,abs(kramersA(j,i)))
               call put_(stdout,abs(kramersB(i,j)))
               call put_(stdout,self%MO_energies%a(i))
               call put_(stdout,self%MO_energies%b(i))
               call flush_(stdout)
            end if
         end do
      end do
      call dash_(stdout,int_fields=2,real_fields=4)
      call flush_(stdout)
      call text_(stdout,"Overalps of one MOb with all the other MOa:")
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"j",int_width=TRUE)
      call put_(stdout,"sumi <bj|ai>^2")
   ! stdout.put("<sum(ai)|bj>")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
   ! stdout.text("j_b / Sab.Sba_j / Sba.Sab_j")
      do j = 1,lb  ! instead of .n_b
         X1 = ZERO; X2 = ZERO
         do i = 1,self%n_a
            X1 = X1+kramersA(j,i)*kramersA(j,i)
            X2 = X2+kramersB(i,j)*kramersB(i,j)
            X3 = X3+kramersA(j,i)*kramersA(j,i)
         end do
         call put_(stdout,j)
         call put_(stdout,X1)
      ! stdout.put(X2)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=1)

   ! stdout.show("sum",X3)
      call put_(stdout,"sum",int_width=TRUE)
      call put_(stdout,X3)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      X3 = ZERO

   ! stdout.text("j_b / Sab.Sba_j / Sba.Sab_j")
      do j = 1,self%n_b  ! instead of .n_b
         X1 = ZERO; X2 = ZERO
         do i = 1,la
            X3 = X3+kramersA(j,i)*kramersA(j,i)
         end do
      end do

   ! stdout.show("sum*",X3)
      call put_(stdout,"sum*",int_width=TRUE)
      call put_(stdout,X3)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      call text_(stdout,"*sum of only the first nb lines in the table")
      call destroy_(kramersA)
      call destroy_(kramersB)
   end subroutine

   subroutine get_gchf_kramers_2(self)
   !  analyse kramers pairs 
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: S
      MAT(CPX,:,:), allocatable :: kramers
      INT :: i,j,k,l,li,lj
      CPX :: X1,X2,X3,X4,II
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_gchf_kramers_2 ... no overlap matrix")
      li = self%n_e
      lj = self%n_e+(self%n_a-self%n_b)*2
      call create_(kramers,lj,lj)
      kramers = (ZERO,ZERO)
      II = (ZERO,ONE)
      X1 = (ZERO,ZERO); X2 = (ZERO,ZERO)
      X3 = (ZERO,ZERO); X4 = (ZERO,ZERO)
      S = self%overlap_mx
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"Kramers pairs analysis - GCHF case:")
      call text_(stdout,"This routine is using more kramers orbitals, to identify your Kramers pairs (open shells):")
      ! the phase is random - hence bad for testing purposes
      ! table head
      call dash_(stdout,int_fields=2,real_fields=1)
      call put_(stdout,"j",int_width=TRUE)
      call put_(stdout,"i",int_width=TRUE)
      call put_(stdout,"|<i|Kj>|")
   ! stdout.put("  ")
   ! stdout.put("<Kj|i>")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=1)
      do j = 1,lj
         do i = 1,li
            do k = 1,self%n_bf
               do l = 1,self%n_bf
                  kramers(i,j) = kramers(i,j)-conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*self%MOs%r(l,j)
                  kramers(i,j) = kramers(i,j)+conjg(self%MOs%gc(k,i))*S(k,l)*self%MOs%r(l+self%n_bf,j)
               end do
            end do
            if (abs(kramers(i,j))>0.0001 OR abs(kramers(j,i))>0.0001) then
               call put_(stdout,j)
               call put_(stdout,i)
               call put_(stdout,abs(kramers(i,j)))
               call flush_(stdout)
            end if
         end do
      end do
      call dash_(stdout,int_fields=2,real_fields=1)
      call flush_(stdout)
      call text_(stdout,"Overlaps of Kramers MOj with all the other MOi:")
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"j",int_width=TRUE)
      call put_(stdout,"sumj |<i|Kj>|^2")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      do j = 1,lj
         X1 = (ZERO,ZERO)  !;X2=(ZERO,ZERO)
         do i = 1,li
            X1 = X1+kramers(i,j)*conjg(kramers(i,j))
         end do
         call put_(stdout,j)
         call put_(stdout,abs(X1))
         call flush_(stdout)
         X3 = X3+X1
      end do
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"sum",int_width=TRUE)
      call put_(stdout,X3)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      X3 = (ZERO,ZERO)
      do i = 1,self%n_e
         X1 = (ZERO,ZERO)  !;X2=(ZERO,ZERO)
         do j = 1,self%n_e
            X1 = X1+kramers(i,j)*conjg(kramers(i,j))
         end do
         X3 = X3+X1
      end do
      call put_(stdout,"sum*",int_width=TRUE)
      call put_(stdout,X3)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      call text_(stdout,"*sum for n_e electrons only")
      call destroy_(kramers)
   end subroutine

   subroutine get_gchf_jz(self)
   ! s, l and j eigenvalues not finished  at all
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: S
      MAT(CPX,:,:), allocatable :: S2,Lz2
      MAT(REAL,:,:), allocatable :: Lx,Ly,Lz
      CPX :: II,jzi
      INT :: i,k,l
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:get_gchf_jz ... no overlap matrix")
   ENSURE(is_allocated_with_genre_(self%MOs,"gc"),"MOLECULE.PROP:get_gchf_jz ... no gchf mos'")
      call create_(Lx,self%n_bf,self%n_bf)
      call create_(Ly,self%n_bf,self%n_bf)
      call create_(Lz,self%n_bf,self%n_bf)
      call to_product_of_(self%density_mx%gc,self%MOs%r(:,1:self%n_e),self%MOs%r(:,1:self%n_e),transpose_b=TRUE)
      II = (ZERO,ONE)
      Lx = ZERO
      Ly = ZERO
      Lz = ZERO
      call make_L_matrices_(self,Lx,Ly,Lz)
      call destroy_(Lx)
      call destroy_(Ly)
      S = self%overlap_mx
      call create_(S2,2*self%n_bf,2*self%n_bf)
      call create_(Lz2,2*self%n_bf,2*self%n_bf)
      Lz2 = (ZERO,ZERO)
      S2 = (ZERO,ZERO)
      call set_aa_block_to_(S2,S,HALF)
      call set_bb_block_to_(S2,S,-HALF)
      call set_aa_block_to_(Lz2,Lz,-II)
      call set_bb_block_to_(Lz2,Lz,-II)
      call destroy_(Lz)
      Lz2 = Lz2+S2
      call destroy_(S2)
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"Numerical Jz evaluation for each orbital")
      ! table head
      call dash_(stdout,int_fields=1,real_fields=2)
      call put_(stdout,"i",int_width=TRUE)
      call put_(stdout,"jzi")
      call put_(stdout," ")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      do i = 1,self%n_e
         jzi = (ZERO,ZERO)
         do k = 1,2*self%n_bf
            do l = 1,2*self%n_bf
               jzi = jzi+conjg(self%MOs%gc(k,i))*Lz2(k,l)*self%MOs%r(l,i)
            end do
         end do
         call put_(stdout,i)
         call put_(stdout,jzi)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=2)
      call destroy_(Lz2)
   end subroutine

   pure subroutine get_kramers_pairs_0(self,kramers)
   !  get overlap of kramers pairs,
   !  i.e. the spatial overlap between alpha and beta spinors
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: kramers

   ! ENSURE(.overlap_mx.allocated,"no overlap matrix")
      MAT(REAL,:,:), allocatable :: S
      INT :: i,j,k,l
      S = self%overlap_mx
      kramers = ZERO
      do i = 1,self%n_a
         do j = 1,self%n_b
            do k = 1,self%n_bf
               do l = 1,self%n_bf
                  kramers(i,j) = kramers(i,j)-self%MOs%a(k,i)*S(k,l)*self%MOs%b(l,j)
               end do
            end do
         end do
      end do
   end subroutine

   pure subroutine get_kramers_pairs_1(self,kramers)
   !  get the overlap of kramers pairs
      type(MOLECULE_TYPE), IN :: self
      MAT(CPX,:,:), OUT :: kramers

   ! ENSURE(.overlap_mx.allocated,"no overlap matrix")
      MAT(REAL,:,:), allocatable :: S
      INT :: i,j,k,l
      S = self%overlap_mx
      kramers = (ZERO,ZERO)
      do i = 1,self%n_e
         do j = i,self%n_e
            do k = 1,self%n_bf
               do l = 1,self%n_bf
                  kramers(i,j) = kramers(i,j)-conjg(self%MOs%gc(k+self%n_bf,i))*S(k,l)*self%MOs%r(l,j)
                  kramers(i,j) = kramers(i,j)+conjg(self%MOs%gc(k,i))*S(k,l)*self%MOs%r(l+self%n_bf,j)
                  kramers(j,i) = kramers(j,i)-conjg(self%MOs%gc(k+self%n_bf,j))*S(k,l)*self%MOs%r(l,i)
                  kramers(j,i) = kramers(j,i)+conjg(self%MOs%gc(k,j))*S(k,l)*self%MOs%r(l+self%n_bf,i)
               end do
            end do
         end do
      end do
   end subroutine


!   put_mulliken_ku_populations :: leaky
!   ! sets the kramers unrestricted mulliken spin population matrix
!   ! we use the unpaired electron density concept, which accounts for
!   ! the total spin
!      mulliken_mx, density :: MAT{REAL}*
!      diagonals :: VEC{REAL}*
!      S2,PSPS,S :: MAT{REAL}*
!      mulliken_mx2,DS2,PSPS2 :: MAT{CPX}*
!      mulliken_ku_mx2 :: MAT{CPX}*
!      CMO_ku :: MAT{CPX}*
!      CMO :: MAT{CPX}@
!      diagonals2 :: VEC{CPX}*
!!      i,j,k,l,m,n :: INT
!      Oij :: CPX
!
!      ! Create space
!      mulliken_mx.create(.n_bf, .n_bf)
!      PSPS.create(.n_bf, .n_bf)
!      diagonals.create(.n_bf)
!
!      ! Overlap matrix
!      .make_overlap_mx ! leaky
!      S => .overlap_mx
!      ! Unrestricted case
!      if (.density_mx.is_allocated_with_genre("u ")) then
!
!         ! Temporary
!         density.create(.n_bf,.n_bf)
!
!         ! Restricted density matrix
!         density = .density_mx.a + .density_mx.b
!
!         ! KU mulliken population D= 2PS - PSPS from Staroverov&Davidson CPL 330 (2000) 161-168
!         mulliken_mx = matmul(density,.overlap_mx)
!         PSPS = matmul(mulliken_mx,mulliken_mx)
!         mulliken_mx = TWO*mulliken_mx - PSPS
!         mulliken_mx.put_diagonal_to(diagonals)
!         
!         stdout.flush
!         stdout.flush
!         stdout.text(" Kramers unrestricted spin case!")
!         stdout.text("      KU = 2PS - PS*PS ")
!         ! Put the populations
!         .put_mulliken_spins(mulliken_mx, diagonals)
!
!         ! Clean up
!         density.destroy
!
!       elseif (.density_mx.is_allocated_with_genre("gc")) then
!
!        DIE("Mulliken ku populations not yet implemented for this case")
! 
!         S2.create(2*.n_bf,2*.n_bf)
!         DS2.create(2*.n_bf,2*.n_bf)
!         CMO_ku.create(2*.n_bf,.n_e)
!         CMO = .MOs.gc(:,1:.n_e)
!         Oij=(ZERO,ZERO)
!         PSPS2.create(2*.n_bf,2*.n_bf)
!         mulliken_mx2.create(2*.n_bf,2*.n_bf)
!         mulliken_ku_mx2.create(2*.n_bf,2*.n_bf)
!         diagonals2.create(2*.n_bf)
!         diagonals2.destroy
!         mulliken_mx2.destroy
!         mulliken_ku_mx2.destroy
!         S2.destroy
!         CMO_ku.destroy
!         DS2.destroy
!         PSPS2.destroy
!      else
!        
!        DIE("Mulliken ku populations not yet implemented for this case")
!
!      end
!
!      ! Clean up
!      mulliken_mx.destroy
!      PSPS.destroy
!      diagonals.destroy
!
!   end

! Field gradient

   subroutine put_E_field_at_nuclei(self)
   ! Put out the electric field at every nuclear position
      type(MOLECULE_TYPE), INOUT :: self
      type(CLUSTER_TYPE), allocatable :: cluster
      MAT(REAL,:,:), allocatable :: XField
      MAT(REAL,:,:), allocatable :: EField
      MAT(REAL,:,:), allocatable :: NField
      MAT(REAL,:,:), allocatable :: MField
      MAT(REAL,:,:), allocatable :: LField
      MAT(REAL,:,:), allocatable :: coords
      VEC(STR(len=STR_SIZE),:), allocatable :: alabel
      VEC(STR(len=STR_SIZE),:), allocatable :: flabel
      VEC(STR(len=STR_SIZE),3) :: axis
      VEC(REAL,3) :: r,d,F
      VEC(REAL,:), allocatable :: charge
      REAL :: mol_rms
      REAL :: LF_rms
      REAL :: xtal_rms
      REAL :: d3
      INT :: a,i,n
      INT :: n_charge
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      axis = ["x","y","z"]
      call create_(EField,3,self%n_atom)
      call create_(NField,3,self%n_atom)
      call create_(MField,3,self%n_atom)
      EField = electronic_E_field_at_nuclei_(self)
      NField = nuclear_E_field_at_nuclei_(self%atom)
      MField = EField+NField  ! Total for molecule
      call head_(stdout,"Electric fields at nuclei")
      call text_(stdout,". Contributions from the electron density (ED) and nuclei are shown.")
      call text_(stdout,". Results in AU.")
      call flush_(stdout)

      ! Make column data
      call create_(alabel,3*self%n_atom)
      call create_(flabel,3*self%n_atom)
      alabel = " "
      mol_rms = ZERO
      n = 0
      do a = 1,self%n_atom
         do i = 1,3
            n = n+1
            if (i==1) alabel(n) = self%atom(a)%tag
            flabel(n) = axis(i)
         end do
         mol_rms = mol_rms+dot_product(MField(:,a),MField(:,a))
      end do

      ! RMS values
      mol_rms = sqrt(mol_rms/self%n_atom)

      ! Table
      call create_(table,5)
      call set_heading_(table(1),"Atom")
      call set_heading_(table(2),"E_i")
      call set_heading_(table(3),"Electrons")
      call set_heading_(table(4),"Nuclei")
      call set_heading_(table(5),"Molecule")
      call set_subhead_(table(5),"total")

      ! Set table coulmn values
      call set_values_(table(1),alabel)
      call set_values_(table(2),flabel)
      call set_values_(table(3),reshape(EField,[3*self%n_atom]))
      call set_values_(table(4),reshape(NField,[3*self%n_atom]))
      call set_values_(table(5),reshape(MField,[3*self%n_atom]))

      ! Put
      call put_(table,label_rows=FALSE)
      call destroy_(table)
      call flush_(stdout)
      call show_(stdout,"RMS (total E field) =",mol_rms)

      ! Add crystal field?
      if (allocated(self%crystal)) then

         ! Table
         call head_(stdout,"Electric fields at nuclei - including crystal environment")
         call text_(stdout,". Calculated using charge and dipole Lorentz field tensors,")
         call text_(stdout,"  from an Ewald sum of  Hirshfeld charges and dipoles situated")
         call text_(stdout,"  at every unit cell atom site")

         ! Make atomic charges and dipoles
         if (NOT self%partition_info_made) then
            call make_atom_partition_info_(self)
         end if

         ! Set up the cluster
         call create_(XField,3,self%n_atom)
         call create_(LField,3,self%n_atom)
         call create_(cluster)
         call set_defaults_(cluster,self%crystal)
         coords = coordinates_(self%atom)
         call set_fragment_geometry_(cluster,coords,cartesian=TRUE)
         call destroy_(coords)
         call set_generation_method_(cluster,"within_radius")
         call set_defragment_(cluster,TRUE)
         call set_radius_(cluster,TEN)
         if (allocated(self%SCF_data)) then
            call set_radius_(cluster,self%SCF_data%cluster_radius)
         end if
         call update_info_(cluster)  ! leaky

         ! Make Lorentz field ... does not need full cluster
         call make_Lorentz_fields_(cluster,LField)

         ! Make Xtal electric field
         XField = MField+LField

         ! RMS values
         mol_rms = ZERO
         LF_rms = ZERO
         xtal_rms = ZERO
         do a = 1,self%n_atom
            mol_rms = mol_rms+dot_product(MField(:,a),MField(:,a))
            LF_rms = LF_rms+dot_product(LField(:,a),LField(:,a))
            xtal_rms = xtal_rms+dot_product(XField(:,a),XField(:,a))
         end do
         mol_rms = sqrt(mol_rms/self%n_atom)
         LF_rms = sqrt(LF_rms/self%n_atom)
         xtal_rms = sqrt(xtal_rms/self%n_atom)

         ! Table headings
         call create_(table,5)
         call set_heading_(table(1),"Atom")
         call set_heading_(table(2),"E_i")
         call set_heading_(table(3),"Central")
         call set_subhead_(table(3),"Molecule")
         call set_heading_(table(4),"From the")
         call set_subhead_(table(4),"Surrounds")
         call set_heading_(table(5),"Crystal")
         call set_subhead_(table(5),"total")

         ! Set new table coulmn values
         call set_values_(table(1),alabel)
         call set_values_(table(2),flabel)
         call set_values_(table(3),reshape(MField,[3*self%n_atom]))
         call set_values_(table(4),reshape(LField,[3*self%n_atom]))
         call set_values_(table(5),reshape(XField,[3*self%n_atom]))

         ! Put
         call put_(table,label_rows=FALSE)
         call destroy_(table)
         call flush_(stdout)
         call show_(stdout,"RMS (Molecular   E field) =",mol_rms)
         call show_(stdout,"RMS (Environment E field) =",LF_rms)
         call show_(stdout,"RMS (Crystal     E field) =",xtal_rms)

         ! Table
         call head_(stdout,"Electric fields at nuclei - including crystal environment")
         call text_(stdout,". Calculated using Hirshfeld point charges and dipoles by")
         call text_(stdout,"  direct summation over all molecules in a cluster surrounding")
         call text_(stdout,"  the central ones, such that any surrounding molecule has an")
         call text_(stdout,"  atom within the cutoff radius")
         call flush_(stdout)
         call show_(stdout,"Cluster radius in au =",cluster%radius)
         call flush_(stdout)

         ! Make the cluster charges
         n_charge = cluster%n_atoms-cluster%n_fragment_atoms
         n_charge = 3*n_charge
      ! n_charge = 2*n_charge
      ! n_charge = n_charges
         call create_(charge,n_charge)
         call create_(coords,3,n_charge)
         call make_non_fragment_mu_charges_(cluster,coords,charge)
      ! cluster.make_non_fragment_positions2(coords,charge)
      ! cluster.make_non_fragment_positions3(coords,charge)

         ! Get the point charge electric fields
         LField = ZERO
         do a = 1,self%n_atom
            r = self%atom(a)%position
            F = ZERO
            do i = 1,n_charge
               d = r-coords(:,i)
               d3 = norm2(d)
               d3 = d3*d3*d3
               F = F+charge(i)*d/d3
            end do
            LField(:,a) = F
         end do

         ! Make Xtal electric field
         XField = MField+LField

         ! RMS values
         mol_rms = ZERO
         LF_rms = ZERO
         xtal_rms = ZERO
         do a = 1,self%n_atom
            mol_rms = mol_rms+dot_product(MField(:,a),MField(:,a))
            LF_rms = LF_rms+dot_product(LField(:,a),LField(:,a))
            xtal_rms = xtal_rms+dot_product(XField(:,a),XField(:,a))
         end do
         mol_rms = sqrt(mol_rms/self%n_atom)
         LF_rms = sqrt(LF_rms/self%n_atom)
         xtal_rms = sqrt(xtal_rms/self%n_atom)

         ! Table headings
         call create_(table,5)
         call set_heading_(table(1),"Atom")
         call set_heading_(table(2),"E_i")
         call set_heading_(table(3),"Central")
         call set_subhead_(table(3),"Molecule")
         call set_heading_(table(4),"From the")
         call set_subhead_(table(4),"Surrounds")
         call set_heading_(table(5),"Crystal")
         call set_subhead_(table(5),"total")

         ! Set new table coulmn values
         call set_values_(table(1),alabel)
         call set_values_(table(2),flabel)
         call set_values_(table(3),reshape(MField,[3*self%n_atom]))
         call set_values_(table(4),reshape(LField,[3*self%n_atom]))
         call set_values_(table(5),reshape(XField,[3*self%n_atom]))

         ! Put
         call put_(table,label_rows=FALSE)
         call destroy_(table)
         call flush_(stdout)
         call show_(stdout,"RMS (Molecular   E field) =",mol_rms)
         call show_(stdout,"RMS (Environment E field) =",LF_rms)
         call show_(stdout,"RMS (Crystal     E field) =",xtal_rms)

         ! Clean up
         call destroy_(coords)
         call destroy_(charge)
         call destroy_(cluster)
         call destroy_(LField)
         call destroy_(XField)
      end if

      ! Clean
      call destroy_(flabel)
      call destroy_(alabel)
      call destroy_(NField)
      call destroy_(MField)
      call destroy_(EField)
   end subroutine

   subroutine put_EFG_at_nuclei(self)
   ! Put out the electric field gradient at every nuclear position
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: EField
      MAT(REAL,:,:), allocatable :: NField
      MAT(REAL,:,:), allocatable :: MField
      VEC(STR(len=STR_SIZE),:), allocatable :: alabel
      VEC(STR(len=STR_SIZE),:), allocatable :: flabel
      VEC(STR(len=STR_SIZE),6) :: axis
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: a,i,n
      axis = ["xx","yy","zz","xy","xz","yz"]
      call create_(EField,3,self%n_atom)
      call create_(NField,3,self%n_atom)
      call create_(MField,3,self%n_atom)
      Efield = electronic_EFG_at_nuclei_(self)
      NField = nuclear_EFG_at_nuclei_(self%atom)
      MField = EField+NField  ! Total for molecule
      call head_(stdout,"Electric field gradient (EFG) at nuclei")
      call text_(stdout,". Results in AU.")
      call flush_(stdout)

      ! Make column data
      call create_(alabel,6*self%n_atom)
      call create_(flabel,6*self%n_atom)
      alabel = " "
      n = 0
      do a = 1,self%n_atom
         do i = 1,6
            n = n+1
            if (i==1) alabel(n) = self%atom(a)%tag
            flabel(n) = axis(i)
         end do
      end do

      ! Table
      call create_(table,5)
      call set_heading_(table(1),"Atom")
      call set_heading_(table(2),"E_ij")
      call set_heading_(table(3),"Electrons")
      call set_heading_(table(4),"Nuclei")
      call set_heading_(table(5),"Molecule")
      call set_subhead_(table(5),"total")

      ! Set table coulmn values
      call set_values_(table(1),alabel)
      call set_values_(table(2),flabel)
      call set_values_(table(3),reshape(EField,[6*self%n_atom]))
      call set_values_(table(4),reshape(NField,[6*self%n_atom]))
      call set_values_(table(5),reshape(MField,[6*self%n_atom]))

      ! Put
      call put_(table,label_rows=FALSE)
      call destroy_(table)

      ! Clean
      call destroy_(flabel)
      call destroy_(alabel)
      call destroy_(NField)
      call destroy_(MField)
      call destroy_(EField)
   end subroutine

   subroutine spin_contamination(self)
   ! Put spin contamination for the UHF calculations.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: MSC,S
      REAL :: SC,S2
      INT :: na,nb,i,j,k,l
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:spin_contamination ... no overlap matrix")
   ENSURE(allocated(self%MOs%a),"MOLECULE.PROP:spin_contamination ... no alpha orbitals")
   ENSURE(allocated(self%MOs%b),"MOLECULE.PROP:spin_contamination ... no beta orbitals")
      S = self%overlap_mx
      na = no_of_alpha_electrons_(self)
      nb = no_of_beta_electrons_(self)
      call create_(MSC,na,nb)
      MSC = ZERO
      SC = ZERO
      do i = 1,na
         do j = 1,nb
            do k = 1,self%n_bf
               do l = 1,self%n_bf
                  MSC(i,j) = MSC(i,j)+self%MOs%b(k,j)*S(k,l)*self%MOs%a(l,i)
               end do
            end do
            SC = SC+MSC(i,j)**2
         end do
      end do
      call destroy_(MSC)
      S2 = (na-nb)*ONE/TWO*((na-nb)*ONE/TWO+ONE)
      call flush_(stdout)
      call show_(stdout,"    THEORETICAL S**2   =",S2)
      call show_(stdout,"    UHF S**2           =",S2+nb-SC)
      call show_(stdout,"    SPIN CONTAMINATION =",nb-SC)
      call flush_(stdout)
   end subroutine


!  Structure factor populations

!   put_mulliken_sf_populations 
!   ! Atomic contributions to structure factors.
!   ! Makes the mulliken population analysis of structure factors,
!   ! You need to have the structure factors real valued, else this is CRAP!
!      self :: INOUT
!
!      density,k_pts :: MAT{REAL}*
!      mulliken_mx :: MAT{CPX}*
!      diagonals :: VEC{CPX}*
!      S2 :: MAT{CPX}*
!      mulliken_mx2 :: MAT{CPX}*
!      diagonals2 :: VEC{CPX}*
!      MM :: MAT{REAL}*
!      GG,ft_hkl,buco :: MAT{CPX}*
!      n,m,k_max,n_ref :: INT
!      sf :: CPX
!      sfn :: MAT{CPX}*
!      Fc :: MAT{CPX}*
!
!      .crystal.assign_xray
!
!      Fc.create(.crystal.n_refl,.n_atom); Fc=(ZERO,ZERO)
!      sfn.create(.crystal.n_unique_SF_k_pts,.n_atom); sfn=(ZERO,ZERO)
!      k_pts.create(.crystal.n_unique_SF_k_pts,3); k_pts=ZERO
!      .crystal.xray_data.reflections.make_unique_SF_k_pts(k_pts,.crystal.spacegroup,.crystal.unit_cell)
!
!      stdout.flush
!      stdout.text("NOTE: We are in put_mulliken_sf_populations!")
!      stdout.text("NOTE: You need to have the structure factors real valued!")
!      stdout.text("NOTE: So as, the cmpx atomic contributions cancel out!")
!      stdout.flush
!
!      .make_r_density_mx
!
!      ! Create space
!      mulliken_mx.create(.n_bf, .n_bf)
!      diagonals.create(.n_bf)
!
!      MM.create(.n_prim,.n_bf)
!      .make_normalized_contraction_mx(MM)
!
!      ft_hkl.create(.n_bf,.n_bf)
!      GG.create(.n_prim,.n_prim)
!      buco.create(.n_bf,.n_prim)
!      
!      k_max = size(k_pts,1)
!      n_ref = size(Fc,1)
!      sfn=ZERO
!
!      parallel do n = 1,k_max
!
!         ! here it comes. point by point the structure factors will be on
!         ! and on calculated, the code is slow if many sfs are present
!         .make_primitive_ft_mx(GG,k_pts(n,1),k_pts(n,2),k_pts(n,3))
!         buco.to_product_of(MM,GG,transpose_a=TRUE)
!         ft_hkl.to_product_of(buco,MM)
!         .make_part_and_therm_smear(ft_hkl,k_pts(n,:))
!
!      ! general complex case (LBxxx)
!      if (.density_mx.is_allocated_with_genre("gc")) then
!
!         ! these objects are twice as large as usually
!         S2.create(2*.n_bf,2*.n_bf)
!         mulliken_mx2.create(2*.n_bf,2*.n_bf)
!         diagonals2.create(2*.n_bf)
!
!         mulliken_mx2 = (ZERO,ZERO)
!         diagonals2 = (ZERO,ZERO)
!         S2 = (ZERO,ZERO)
!         S2.set_aa_block_to(ft_hkl)
!         S2.set_bb_block_to(ft_hkl)
!
!         ! fill the general_complex mulliken matrix and diagonals
!         mulliken_mx2.to_product_of(.density_mx.gc,S2)
!         mulliken_mx2.put_diagonal_to(diagonals2)
!
!         ! Make use of the restricted mulliken matrix
!         diagonals = REALIFY(diagonals2(1:.n_bf)) + REALIFY(diagonals2(.n_bf+1:2*.n_bf))

!                           REALIFY(mulliken_mx2(.n_bf+1:2*.n_bf,.n_bf+1:2*.n_bf))
!
!         ! Put the atomic populations
!         .put_mulliken_sf_populations(sfn(n,:),diagonals)
!
!         mulliken_mx2.destroy
!         diagonals2.destroy
!         S2.destroy
!
!         density.destroy
!
!      ! Unrestricted case
!      else if (.density_mx.is_allocated_with_genre("u ")) then
!
!         ! Temporary
!         density.create(.n_bf,.n_bf)
!
!         ! Restricted density matrix
!         density = .density_mx.a + .density_mx.b
!
!         ! Normal mulliken population
!         mulliken_mx.to_product_of(density,ft_hkl)
!         mulliken_mx.put_diagonal_to(diagonals)
!         
!         ! get the raw sf
!         .make_FT_test_1(sf,density,k_pts(n,:))
!         ! Put the populations
!         .put_mulliken_sf_populations(sfn(n,:),diagonals)
!
!         ! Clean up
!         density.destroy
!
!      ! Restricted case
!      else if (.density_mx.is_allocated_with_genre("r ")) then
!         
!         ! Temporary
!         density.create(.n_bf,.n_bf)
!
!         ! Make the restricted mulliken matrix
!         mulliken_mx.to_product_of(.density_mx.r,ft_hkl)
!         mulliken_mx.put_diagonal_to(diagonals)
!         
!         density=.density_mx.r
!         ! get the raw sf
!         .make_ft_test_1(sf,density,k_pts(n,:))
!
!         ! Put the atomic populations
!         .put_mulliken_sf_populations(sfn(n,:),diagonals)
!
!         ! Clean up
!         density.destroy
!         
!      else
!
!        DIE("Mulliken populations not yet implemented for this case")
!
!      end if
!
!     end
!
!     if (WORK_IS_SHARED) then
!        PARALLEL_SUM(sfn)
!     end if
!
!      ! lets build the sf out of the unique data for all atoms 
!      do n=1,.n_atom
!         .crystal.spacegroup.sum_unique_sf(Fc(:,n),sfn(:,n),.crystal.xray_data.reflections)
!      end do
!
!      stdout.flush
!      stdout.flush
!      stdout.put("Mulliken populations in structure factors:")
!      stdout.flush
!      stdout.put("(One line for one structure factor, real and imaginary part shown for each atom)")
!      stdout.flush
!      stdout.dash(int_fields=1, real_fields=2*.n_atom)
!      stdout.put("#", int_width = TRUE)
!      do n=1,.n_atom
!         stdout.put(.atom(n).label)
!         stdout.put(" ")
!      end do
!      stdout.flush 
!      stdout.dash(int_fields=1, real_fields=2*.n_atom)
!
!      ! output
!      do n=1,n_ref
!         stdout.put(n)
!         do m=1,.n_atom
!            stdout.put(Fc(n,m))
!         end do
!         stdout.flush
!      end do
!      stdout.dash(int_fields=1, real_fields=2*.n_atom)
!
!      ! Clean up
!      mulliken_mx.destroy
!      diagonals.destroy
!      ft_hkl.destroy
!      k_pts.destroy
!      MM.destroy
!      GG.destroy
!      buco.destroy
!      sfn.destroy
!      Fc.destroy
!
!   end

!   put_mulliken_sf_populations(sfn,diagonals)
!   ! makes the mulliken sf populations 
!      self :: IN
!      sfn :: VEC{CPX}, OUT
!      diagonals :: VEC{CPX}, IN
!
!      atom_pop :: VEC{CPX}@
!      pop :: CPX
!      o_count, a, b, c, n_comp :: INT
!
!      o_count = 1
!      atom_pop.create(.n_atom)
!      atom_pop = ZERO
!
!      do a = 1, .n_atom
!      do b = 1, .atom(a).basis.n_shell
!
!         if (.use_spherical_basis) then
!            n_comp = .atom(a).basis.shell(b).n_sph
!         else
!            n_comp = .atom(a).basis.shell(b).n_comp
!         end if
!
!         do c = 1, n_comp
!            pop = diagonals(c + o_count - 1)
!            atom_pop(a) = atom_pop(a) + pop
!         end
!
!         o_count = o_count + n_comp
!
!      end
!      end
!
!      do a = 1,.n_atom
!         sfn(a) = atom_pop(a)
!      end
!
!      atom_pop.destroy
!
!   end

!  =================
!  Structure factors
!  =================

   subroutine put_X_SF_statistics(self)
   ! Put the X-ray structure factors, with summary statistics 
   ! if possible
      type(MOLECULE_TYPE), INOUT :: self
   DIE_IF(NOT allocated(self%atom),"MOLECULE.PROP:put_X_SF_statistics ... no atom list")
   DIE_IF(NOT self%basis_info_made,"MOLECULE.PROP:put_X_SF_statistics ... no basis info")
   DIE_IF(NOT allocated(self%crystal),"MOLECULE.PROP:put_X_SF_statistics ... no crystal")
   DIE_IF(NOT allocated(self%crystal%xray_data),"MOLECULE.PROP:put_X_SF_statistics ... no crystal data")
      if (have_F_exp_(self%crystal%xray_data)) then

         ! Remake F_pred
         call make_F_predicted_(self%crystal%xray_data)

         ! Show stats & data analysis
         call flush_(stdout)
         call text_(stdout,"============================")
         call text_(stdout,"X-ray diffraction statistics")
         call text_(stdout,"============================")
         call put_F_statistics_(self%crystal%xray_data)
      end if
   end subroutine

   subroutine put_X_SFs(self)
   ! Put the X-ray structure factors, with summary statistics if
   ! possible
      type(MOLECULE_TYPE), INOUT :: self
   DIE_IF(NOT allocated(self%atom),"MOLECULE.PROP:put_X_SFs ... no atom list")
   DIE_IF(NOT self%basis_info_made,"MOLECULE.PROP:put_X_SFs ... no basis info")
   DIE_IF(NOT allocated(self%crystal),"MOLECULE.PROP:put_X_SFs ... no crystal")
   DIE_IF(NOT allocated(self%crystal%xray_data),"MOLECULE.PROP:put_X_SFs ... no crystal data")
      if (have_F_exp_(self%crystal%xray_data)) then

         ! Show stats & data analysis
         call flush_(stdout)
         call text_(stdout,"============================")
         call text_(stdout,"X-ray diffraction statistics")
         call text_(stdout,"============================")
         call put_F_statistics_(self%crystal%xray_data)
         call put_X_data_analysis_(self%crystal,job=self%name)
      end if
      call put_X_reflection_data_(self%crystal)
   end subroutine


!   put_N_SFs :: leaky
!   ! Put the PND (Sz) structure factors, with summary statistics
!   ! if possible
!      self :: INOUT
!
!   DIE_IF(NOT .atom.allocated,"no atom list")
!   DIE_IF(NOT .basis_info_made,"no basis info")
!   DIE_IF(NOT .crystal.allocated,"no crystal")
!   DIE_IF(NOT .crystal.xray_data.allocated,"no crystal data")
!   DIE_IF(NOT .crystal.xray_data.have_F_exp, "no crystal F_exp data")
!   DIE_IF(NOT .crystal.xray_data.have_F_pred,"no crystal F_pred data")
!
!      if (.crystal.xray_data.have_F_exp) then
!
!         ! Remake F_pred
!         .crystal.xray_data.make_F_predicted
!
!         ! Show stats & data analysis
!         stdout.flush
!         stdout.text("==============")
!         stdout.text("PND statistics")
!         stdout.text("==============")
!         .crystal.xray_data.put_F_statistics
!         .crystal.put_X_data_analysis
!
!      else
!
!         .crystal.put_N_reflection_data
!
!      end
!
!   end

!  ==============
!  Coulomb energy
!  ==============

   subroutine put_Coulomb_energy(self)
   ! Print out the Coulomb energy for the current density matrix
   ! NOTE: ths turns off any adiabatic constant and Fermi-Amaldi
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: J,K
      REAL :: E_J,E_K
   ENSURE(allocated(self%density_mx),"MOLECULE.PROP:put_Coulomb_energy ... no density matrix")

      ! Make restricted density matrix
      call make_r_density_mx_(self)

      ! Make the J matrix
      call set_ee_adiabatic_constant_(self%SCF_data,ONE)
      call set_Fermi_Amaldi_(self%SCF_data,FALSE)
      call create_(J,self%n_bf,self%n_bf)
      call create_(K,self%n_bf,self%n_bf)
      call make_r_JK_engine_(self,J,K,self%density_mx%r)

      ! Energy
      E_J = HALF*trace_product_with_(J,self%density_mx%r)
      E_K = -QUARTER*trace_product_with_(K,self%density_mx%r)

      ! Output
      call flush_(stdout)
      call show_(stdout,"Coulomb  energy = ",E_J)
      call show_(stdout,"Exchange energy = ",E_K)
      call destroy_(K)
      call destroy_(J)
   end subroutine


!  ===================
!  Pairwise SCF energy
!  ===================

   subroutine put_SCF_energy_in_MO_pairs(self)
   ! Analyze the SCF energy into MO pairs.
   ! NOTE: the .density_matrix is destroyed; but .MOs remain.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: core_energy
      MAT(REAL,:,:), allocatable :: pair_energy
      REAL :: energy
      BIN :: direct
      INT :: i,j
      call create_(core_energy,self%n_a)
      call create_(pair_energy,self%n_a,self%n_a)
      core_energy = ZERO
      pair_energy = ZERO
      direct = self%SCF_data%using_direct_SCF
      call set_using_direct_SCF_(self%SCF_data,TRUE)
      do i = 1,self%n_a
         call make_SCF_density_mx_(self,i)
         call make_Fock_mx_(self,self%density_mx,self%fock_mx,core=TRUE,r12=FALSE)
         core_energy(i) = SCF_electronic_energy_(self,self%density_mx,core=TRUE)
         do j = 1,i
            call make_SCF_density_mx_(self,j)
            call make_Fock_mx_(self,self%density_mx,self%fock_mx,core=FALSE,r12=TRUE)
            energy = SCF_electronic_energy_(self,self%density_mx,core=FALSE)
            if (i==j) then; pair_energy(i,j) = energy
            else; pair_energy(i,j) = TWO*energy
            end if
         end do
      end do
      call destroy_(self%density_mx)
      call set_using_direct_SCF_(self%SCF_data,direct)
      call flush_(stdout)
      call text_(stdout,"SCF MO pair energy decomposition:")
      energy = sum(core_energy)+sum(transfer(pair_energy,[ONE]))+nuclear_repulsion_energy_(self%atom)
      call show_(stdout,"SCF energy =",energy)
      call text_(stdout,"Core energies:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"MO_i",int_width=TRUE)
      call put_(stdout,"Core energy")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      do i = 1,self%n_a
         call put_(stdout,i)
         call put_(stdout,core_energy(i))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=1)
      call flush_(stdout)
      call text_(stdout,"Orbital pair energies:")
      call dash_(stdout,int_fields=2,real_fields=1)
      call put_(stdout,"MO_i",int_width=TRUE)
      call put_(stdout,"MO_j",int_width=TRUE)
      call put_(stdout,"Pair energy")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=1)
      do i = 1,self%n_a
         do j = 1,i
            call put_(stdout,i)
            call put_(stdout,j)
            call put_(stdout,pair_energy(i,j))
            call flush_(stdout)
         end do
      end do
      call dash_(stdout,int_fields=2,real_fields=1)
      call flush_(stdout)
   end subroutine

   subroutine put_SCF_energy_as_MO_parts(self)
   ! Analyze the SCF energy into MO contributions
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: MO_energy
      MAT(REAL,:,:), allocatable :: H
      INT :: i
   ENSURE(allocated(self%MOs),"MOLECULE.PROP:put_SCF_energy_as_MO_parts ... no MO's")
   ENSURE(allocated(self%MO_energies),"MOLECULE.PROP:put_SCF_energy_as_MO_parts ... no MO energies")
   ENSURE(allocated(self%density_mx),"MOLECULE.PROP:put_SCF_energy_as_MO_parts ... no density matrix")
   ENSURE(allocated(self%SCF_data),"MOLECULE.PROP:put_SCF_energy_as_MO_parts ... no SCF_data")
   ENSURE(is_one_of_(self%SCF_data%SCF_kind,["rhf","rks"]),"MOLECULE.PROP:put_SCF_energy_as_MO_parts ... must be rhf/rks")
      call create_(MO_energy,self%n_a)
      call create_(H,self%n_bf,self%n_bf)
      call make_core_mx_(self)
      call change_basis_to_(self%core_mx%r,H,self%MOs%r)
      do i = 1,self%n_a
         MO_energy(i) = H(i,i)+self%MO_energies%r(i)
      end do
      call flush_(stdout)
      call text_(stdout,"SCF MO energy decomposition:")
      call show_(stdout,"SCF electronic energy =",sum(MO_energy))
      call text_(stdout,"MO energies:")
      call flush_(stdout)
      call put_(stdout,MO_energy)
      call destroy_(H)
      call destroy_(MO_energy)
   end subroutine


!  ========================
!  SCF Energy decomposition
!  ========================

   subroutine put_AO_energy_partition(self)
   ! Put out the AO energy partition
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: Y
      call create_(Y,self%n_bf,self%n_bf)
      call to_unit_mx_(Y)
      call put_energy_partition_(self,Y)
      call destroy_(Y)
   end subroutine

   subroutine put_SAO_energy_partition(self)
   ! Put out the symmetric AO energy partition
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: Smh
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PROP:put_SAO_energy_partition ... no overlap matrix")
      call create_(Smh,self%n_bf,self%n_bf)
      call make_r_overlap_inverse_sqrt_(self,Smh)
      call put_energy_partition_(self,Smh)
      call destroy_(Smh)
   end subroutine

   subroutine put_MO_energy_partition(self)
   ! Put out the MO energy partition. NOTE: the MO's must be ordered
   ! contiguously, first group 1 then group 2 MO's, AND the atoms in
   ! each group must be contiguous in the .atom list, first group 1
   ! then group 2. This is because the atom_group_AO_subspace_set
   ! routine is used, and by abuse of its function with the above
   ! cobstraints, it will do the right thing and copy quadrants.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%MOs),"MOLECULE.PROP:put_MO_energy_partition ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.PROP:put_MO_energy_partition ... no restricted MO's")
      call put_energy_partition_(self,self%MOs%r)
   end subroutine

   subroutine put_energy_partition(self,Y)
   ! Put out the energy decomposition specified in the .atom_group
   ! array.  The inverse of matrix "Y" is used to back transform the
   ! density matrix (which is taken from disk). The back transformed
   ! density matrix is then partitioned, and transformed back in order
   ! to evaluate the interaction energies between the partitioned
   ! systems.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: Y
      MAT(REAL,:,:), allocatable :: P,PP
      MAT(REAL,:,:), allocatable :: W,T,Z,ZZ,C,K
      MAT(REAL,:,:), allocatable :: X,E_T,E_Z,EE
      MAT3(REAL,:,:,:), allocatable :: E_ZZ
      MAT4(REAL,:,:,:,:), allocatable :: E_C,E_K
      VEC(INT,:), allocatable :: gr1,gr2
      INT :: n_group,g,h,i,j
      REAL :: fac
      STR(len=STR_SIZE) :: unit
      type(ARCHIVE_TYPE) :: arch
      REAL :: kinetic_energy,nuclear_attraction,nuclear_repulsion
      REAL :: coulomb_repulsion,net_coulomb,bicentric_exchange
      REAL :: exchange_attraction,total_interaction
   ENSURE(allocated(self%atom_group),"MOLECULE.PROP:put_energy_partition ... no atom group information")
   ENSURE(size(self%atom_group)==2,"MOLECULE.PROP:put_energy_partition ... must have only 2 groups")
   ENSURE(size(Y,1)==self%n_bf,"MOLECULE.PROP:put_energy_partition ... incorrect size for Y array")
      call set_(arch,self%name,"density_mx","r")
      ENSURE(exists_(arch),"MOLECULE.PROP:put_energy_partition ... no density matrix saved on disk")
      call create_(P,self%n_bf,self%n_bf)
      call create_(PP,self%n_bf,self%n_bf)
      call read_(arch,P)
      call create_(W,self%n_bf,self%n_bf)
      call create_(T,self%n_bf,self%n_bf)
      call create_(Z,self%n_bf,self%n_bf)
      call create_(X,self%n_bf,self%n_bf)
      call to_inverse_of_(X,Y)
      call make_kinetic_energy_mx_(self,T)
      call make_nuclear_attraction_mx_(self,Z)
      n_group = size(self%atom_group)
      call create_(E_T,n_group,n_group); E_T = ZERO
      call create_(E_Z,n_group,n_group); E_Z = ZERO
      call create_(E_C,n_group,n_group,n_group,n_group); E_C = ZERO
      call create_(E_K,n_group,n_group,n_group,n_group); E_K = ZERO
      call create_(E_ZZ,n_group,n_group,n_group); E_ZZ = ZERO
      call create_(ZZ,self%n_bf,self%n_bf)
      PP = P
      call back_transform_using_(PP,X)
      call destroy_(X)
      do i = 1,n_group
         do j = 1,i
            W = ZERO
            call atom_group_AO_subspace_set_(self,W,PP,i,j)
            if (i/=j) call atom_group_AO_subspace_set_(self,W,PP,j,i)
            call back_transform_using_(W,Y)
            E_T(i,j) = trace_product_with_(T,W)
            E_Z(i,j) = trace_product_with_(Z,W)
            do g = 1,n_group
               call make_nuclear_attraction_mx_(self,ZZ,self%atom_group(g)%atom_index)
               E_ZZ(i,j,g) = trace_product_with_(ZZ,W)
            end do
         end do
      end do
      call destroy_(ZZ)
      call create_(C,self%n_bf,self%n_bf)
      call create_(K,self%n_bf,self%n_bf)
      do g = 1,n_group
         do h = 1,g
            W = ZERO
            call atom_group_AO_subspace_set_(self,W,PP,g,h)
            if (g/=h) call atom_group_AO_subspace_set_(self,W,PP,h,g)
            call back_transform_using_(W,Y)
            call make_r_JK_nosym_(self,C,K,W)
            do i = 1,n_group
               do j = 1,i
                  W = ZERO
                  call atom_group_AO_subspace_set_(self,W,PP,i,j)
                  if (i/=j) call atom_group_AO_subspace_set_(self,W,PP,j,i)
                  call back_transform_using_(W,Y)
                  E_C(i,j,g,h) = trace_product_with_(C,W)
                  E_K(i,j,g,h) = -trace_product_with_(K,W)
               end do
            end do
         end do
      end do
      call destroy_(K)
      call destroy_(C)
      E_C = HALF*E_C
      E_K = HALF*HALF*E_K
      call create_(EE,n_group,n_group)
      call text_(stdout," ")
      call text_(stdout,"Energies obtained with the current density matrix:")
      call text_(stdout," ")
      fac = sum(E_T+E_Z)+sum(E_C+E_K)
      call show_(stdout,"SCF electronic energy =",fac)
      call show_(stdout,"SCF energy            =",fac+nuclear_repulsion_energy_(self%atom))
      call show_(stdout,"Kinetic energy        =",sum(E_T))
      unit = "kcal/mol"
      fac = conversion_factor_(unit)
      E_T = fac*E_T
      E_Z = fac*E_Z
      E_C = fac*E_C
      E_K = fac*E_K
      E_ZZ = fac*E_ZZ
      call text_(stdout," ")
      call text_(stdout,"Energy decomposition in kcal/mol ...")
      call text_(stdout," ")
      call text_(stdout,"Kinetic interaction terms")
      call put_(stdout,E_T)
      call text_(stdout," ")
      call text_(stdout,"Nuclear attraction interaction terms")
      call put_(stdout,E_Z)
      call text_(stdout," ")
      call text_(stdout,"Nuclear attraction interaction terms ONLY for group 1 nuclei")
      call put_(stdout,E_ZZ(:,:,1))
      call text_(stdout," ")
      call text_(stdout,"Nuclear attraction interaction terms ONLY for group 2 nuclei")
      call put_(stdout,E_ZZ(:,:,2))
      gr1 = self%atom_group(1)%atom_index
      gr2 = self%atom_group(2)%atom_index
      EE(1,1) = fac*nuclear_repulsion_energy_(self%atom(gr1))
      EE(2,2) = fac*nuclear_repulsion_energy_(self%atom(gr2))
      nuclear_repulsion = fac*nuclear_repulsion_energy_(self%atom)-EE(1,1)-EE(2,2)
      EE(2,1) = nuclear_repulsion
      EE(1,2) = ZERO
      call text_(stdout," ")
      call text_(stdout,"Nuclear nuclear repulsion")
      call put_(stdout,EE)
      EE(1,1) = E_C(1,1,1,1)
      EE(2,1) = E_C(2,2,1,1)
      EE(1,2) = E_C(1,1,2,2)
      EE(2,2) = E_C(2,2,2,2)
      call text_(stdout," ")
      call text_(stdout,"Diagonal Coulomb repulsion interaction terms")
      call put_(stdout,EE)
      EE(1,1) = E_C(1,2,1,1)
      EE(2,1) = E_C(2,1,1,1)
      EE(1,2) = E_C(1,2,2,2)
      EE(2,2) = E_C(2,1,2,2)
      call text_(stdout," ")
      call text_(stdout,"Semi diagonal Coulomb repulsion interaction terms")
      call put_(stdout,EE)
      EE(1,1) = E_C(1,2,1,2)
      EE(2,1) = E_C(2,1,1,2)
      EE(1,2) = E_C(1,2,2,1)
      EE(2,2) = E_C(2,1,2,1)
      call text_(stdout," ")
      call text_(stdout,"Off diagonal Coulomb repulsion interaction terms")
      call put_(stdout,EE)
      call text_(stdout," ")
      call text_(stdout,"All Coulomb repulsion interaction terms")
      call put_(stdout,E_C)
      EE(1,1) = E_K(1,1,1,1)
      EE(2,1) = E_K(2,2,1,1)
      EE(1,2) = E_K(1,1,2,2)
      EE(2,2) = E_K(2,2,2,2)
      call text_(stdout," ")
      call text_(stdout,"Diagonal exchange interaction terms")
      call put_(stdout,EE)
      EE(1,1) = E_K(1,2,1,1)
      EE(2,1) = E_K(2,1,1,1)
      EE(1,2) = E_K(1,2,2,2)
      EE(2,2) = E_K(2,1,2,2)
      call text_(stdout," ")
      call text_(stdout,"Semi diagonal exchange interaction terms")
      call put_(stdout,EE)
      EE(1,1) = E_K(1,2,1,2)
      EE(2,1) = E_K(2,1,1,2)
      EE(1,2) = E_K(1,2,2,1)
      EE(2,2) = E_K(2,1,2,1)
      call text_(stdout," ")
      call text_(stdout,"Off diagonal exchange interaction terms")
      call put_(stdout,EE)
      call text_(stdout," ")
      call text_(stdout,"All exchange interaction terms")
      call put_(stdout,E_K)
      kinetic_energy = E_T(2,1)
      nuclear_attraction = sum(E_Z)-E_ZZ(1,1,1)-E_ZZ(2,2,2)
      coulomb_repulsion = sum(E_C)-E_C(1,1,1,1)-E_C(2,2,2,2)
      net_coulomb = nuclear_attraction+nuclear_repulsion+coulomb_repulsion
      bicentric_exchange = E_K(1,1,2,2)+E_K(2,2,1,1)
      exchange_attraction = sum(E_K)-E_K(1,1,1,1)-E_K(2,2,2,2)
      total_interaction = kinetic_energy+net_coulomb+exchange_attraction
      call text_(stdout," ")
      call show_(stdout,"Electron-nuclear attraction =",nuclear_attraction)
      call show_(stdout,"Nuclear-nuclear repulsion   =",nuclear_repulsion)
      call show_(stdout,"Electron coulomb repulsion  =",coulomb_repulsion)
      call text_(stdout," ")
      call text_(stdout,"Interaction energies for "//trim(self%name)//":")
      call text_(stdout," ")
      call show_(stdout,"Kinetic energy              =",kinetic_energy)
      call show_(stdout,"Exchange energy             =",exchange_attraction)
      call show_(stdout,"Kinetic + Exhange           =",kinetic_energy+exchange_attraction)
      call show_(stdout,"Bicentric Exchange          =",bicentric_exchange)
      call show_(stdout,"Net Coulomb energy          =",net_coulomb)
      call show_(stdout,"Net electronic en           =",nuclear_attraction+coulomb_repulsion)
      call show_(stdout,"Total int. energy           =",total_interaction)
      kinetic_energy = E_T(1,1)
      exchange_attraction = E_K(1,1,1,1)
      net_coulomb = E_ZZ(1,1,1)+fac*nuclear_repulsion_energy_(self%atom(gr1))+E_C(1,1,1,1)
      total_interaction = kinetic_energy+net_coulomb+exchange_attraction
      call text_(stdout," ")
      call text_(stdout,"Monomer 1 energies:")
      call text_(stdout," ")
      call show_(stdout,"Kinetic energy 1            =",kinetic_energy)
      call show_(stdout,"Exchange energy 1           =",exchange_attraction)
      call show_(stdout,"Kinetic+Exhange 1           =",kinetic_energy+exchange_attraction)
      call show_(stdout,"Net Coulomb 1               =",net_coulomb)
      call show_(stdout,"Total 1                     =",total_interaction)
      kinetic_energy = E_T(2,2)
      exchange_attraction = E_K(2,2,2,2)
      net_coulomb = E_ZZ(2,2,2)+fac*nuclear_repulsion_energy_(self%atom(gr2))+E_C(2,2,2,2)
      total_interaction = kinetic_energy+net_coulomb+exchange_attraction
      call text_(stdout," ")
      call text_(stdout,"Monomer 2 energies:")
      call text_(stdout," ")
      call show_(stdout,"Kinetic energy 2            =",kinetic_energy)
      call show_(stdout,"Exchange energy 2           =",exchange_attraction)
      call show_(stdout,"Kinetic+Exhange 2           =",kinetic_energy+exchange_attraction)
      call show_(stdout,"Net Coulomb 2               =",net_coulomb)
      call show_(stdout,"Total 2                     =",total_interaction)
      call destroy_(EE)
      call destroy_(E_ZZ)
      call destroy_(E_K)
      call destroy_(E_C)
      call destroy_(E_Z)
      call destroy_(E_T)
      call destroy_(Z)
      call destroy_(T)
      call destroy_(W)
      call destroy_(PP)
      call destroy_(P)
   end subroutine

   subroutine put_Roby_smo_energy_partition(self)
   ! Put out the roby energy decomposition specified in the
   ! .atom_group array based on symmetrically orthonormalized 
   ! monomer molecular (SOMMO) fragments.  A supermolecule 
   ! density matrix must exist on disk.
      type(MOLECULE_TYPE), INOUT :: self
      call put_Roby_energy_partition_(self,symorthonormalize=TRUE)
   end subroutine

   subroutine put_Roby_mmo_energy_partition(self)
   ! Put out the roby energy decomposition specified in the
   ! .atom_group array based on monomer molecular (MMO) fragments. 
   ! A supermolecue density matrix must exist on disk.
      type(MOLECULE_TYPE), INOUT :: self
      call put_Roby_energy_partition_(self,symorthonormalize=FALSE)
   end subroutine

   subroutine put_Roby_energy_partition(self,symorthonormalize)
   ! Calculate and put out the roby energy decomposition using groups
   ! specified in the .atom_group array.  If "symorthonormalize" is
   ! TRUE, then the occupied monomer molecular orbitals are
   ! symmetrically orthonormalized in order to define the fragments.
   ! NOTE: The density matrix defining the supermolecule is read in
   ! from the disk, so an SCF calculation or a "make_group_density_mx"
   ! calculation must be done before this routine is called.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, IN :: symorthonormalize
      STR(len=STR_SIZE) :: unit
      type(ARCHIVE_TYPE) :: arch
      REAL :: E_T,E_T_A,E_T_B,E_Z,E_Z_A,E_Z_B
      REAL :: E0_A,E0_B,E0_T_A,E0_T_B,E0_Z_A,E0_Z_B
      REAL :: E_C,E_C_A,E_C_B,E_K,E_K_A,E_K_B,E_N,E_N_A,E_N_B
      REAL :: E0_C_A,E0_C_B,E0_K_A,E0_K_B
      REAL :: E_kin,E_att,E_rep,E_coul,E_net_coul,E_ex,E_int,E_prom,energy,fac
      MAT(REAL,:,:), allocatable :: P,P_A,P_B,D,T,Z,C,K
      VEC(INT,:), allocatable :: gr1,gr2
      INT :: n,f,l
   ENSURE(allocated(self%atom_group),"MOLECULE.PROP:put_Roby_energy_partition ... no atom group information")
   ENSURE(size(self%atom_group)==2,"MOLECULE.PROP:put_Roby_energy_partition ... must have only 2 groups")

      ! Units
      unit = "kcal/mol"
      fac = conversion_factor_(unit)

      ! Make the ANOs: atom groups need it (leaky)
      call make_ANOs_and_interpolators_(self)

      ! Make the atom_group(:).mol's
   ! .make_atom_group_info

      ! Make sure ANO data and atom_group(:).mol SCF_data is up-to-date (leaky)
      call update_group_crystal_and_ANOs_(self)

      ! Density matrix exists?
      call set_(arch,self%name,"density_mx","r")
      ENSURE(exists_(arch),"MOLECULE.PROP:put_Roby_energy_partition ... no density matrix")

      ! Read density from disk
      call create_(P,self%n_bf,self%n_bf)
      call read_(arch,P)

      ! Make the monomer density and MO's
      call do_atom_group_SCF_(self,MOs=TRUE)

      ! Make the group energies
      call make_group_energies_(self,1,E0_A,E0_T_A,E0_Z_A,E0_C_A,E0_K_A,fac)
      call make_group_energies_(self,2,E0_B,E0_T_B,E0_Z_B,E0_C_B,E0_K_B,fac)

      ! Symorthonormalize the occupied monomer MO's
      if (symorthonormalize) then
         call symorthonormalize_occupied_MOs_(self)
      else
         call text_(stdout," ")
         call text_(stdout,"WARNING: no symorthonormalisation of occupied MO's")
      end if

      ! Do the energy decomposition
      call create_(P_A,self%n_bf,self%n_bf)
      call create_(P_B,self%n_bf,self%n_bf)
      call create_(D,self%n_bf,self%n_bf)
      call make_overlap_mx_(self)
      call change_basis_to_(P,P_A,self%overlap_mx)
      P_B = P_A
      n = index_of_first_zero_value_(self%NO_occ_nos%r(1:))-1
      call to_product_of_(D,self%MOs%r(:,1:n),self%MOs%r(:,1:n),transpose_b=TRUE)
      call change_basis_using_(P_A,D)
      f = index_of_first_nonzero_value_(self%NO_occ_nos%r(n+1:))
      f = n+f
      l = index_of_first_zero_value_(self%NO_occ_nos%r(f:))-1
      l = f+l-1
      call to_product_of_(D,self%MOs%r(:,f:l),self%MOs%r(:,f:l),transpose_b=TRUE)
      call change_basis_using_(P_B,D)
      call destroy_(D)
      call create_(T,self%n_bf,self%n_bf)
      call make_kinetic_energy_mx_(self,T)
      E_T = trace_product_with_(T,P)*fac
      E_T_A = trace_product_with_(T,P_A)*fac
      E_T_B = trace_product_with_(T,P_B)*fac
      call destroy_(T)
      call create_(Z,self%n_bf,self%n_bf)
      call make_nuclear_attraction_mx_(self,Z)
      E_Z = trace_product_with_(Z,P)*fac
      call make_nuclear_attraction_mx_(self,Z,self%atom_group(1)%atom_index)
      E_Z_A = trace_product_with_(Z,P_A)*fac
      call make_nuclear_attraction_mx_(self,Z,self%atom_group(2)%atom_index)
      E_Z_B = trace_product_with_(Z,P_B)*fac
      call destroy_(Z)
      call create_(C,self%n_bf,self%n_bf)
      call create_(K,self%n_bf,self%n_bf)
      call make_r_JK_nosym_(self,C,K,P)
      E_C = HALF*trace_product_with_(C,P)*fac
      E_K = -QUARTER*trace_product_with_(K,P)*fac
      call make_r_JK_nosym_(self,C,K,P_A)
      E_C_A = HALF*trace_product_with_(C,P_A)*fac
      E_K_A = -QUARTER*trace_product_with_(K,P_A)*fac
      call make_r_JK_nosym_(self,C,K,P_B)
      E_C_B = HALF*trace_product_with_(C,P_B)*fac
      E_K_B = -QUARTER*trace_product_with_(K,P_B)*fac
      call destroy_(K)
      call destroy_(C)
      call destroy_(P_B)
      call destroy_(P_A)
      call destroy_(P)
      gr1 = self%atom_group(1)%atom_index
      gr2 = self%atom_group(2)%atom_index
      E_N = fac*nuclear_repulsion_energy_(self%atom)
      E_N_A = nuclear_repulsion_energy_(self%atom(gr1))*fac
      E_N_B = nuclear_repulsion_energy_(self%atom(gr2))*fac
      call text_(stdout," ")
      call text_(stdout," ")
      call text_(stdout,"Supermolecule energies:")
      call text_(stdout," ")
      energy = E_T+E_Z+E_C+E_K
      call show_(stdout,"SCF electronic energy =",(energy/fac))
      call show_(stdout,"... in kcal/mol       =",energy)
      call show_(stdout,"SCF energy            =",(energy/fac)+nuclear_repulsion_energy_(self%atom))
      call show_(stdout,"... in kcal/mol       =",energy+fac*nuclear_repulsion_energy_(self%atom))
      call show_(stdout,"Kinetic energy        =",E_T)
      call show_(stdout,"... in kcal/mol       =",(E_T/fac))
      call text_(stdout," ")
      call text_(stdout,"Roby interaction energy decomposition (kcal/mol):")
      E_kin = E_T-E_T_A-E_T_B
      E_ex = E_K-E_K_A-E_K_B
      E_att = E_Z-E_Z_A-E_Z_B
      E_rep = E_N-E_N_A-E_N_B
      E_coul = E_C-E_C_A-E_C_B
      E_net_coul = E_att+E_rep+E_coul
      E_int = E_kin+E_net_coul+E_ex
      call put_Roby_energy_parts_(E_kin,E_ex,E_coul,E_att,E_rep,E_int)
      call text_(stdout," ")
      call text_(stdout,"Interacting Monomer 1 energies (kcal/mol):")
      call text_(stdout," ")
      E_int = E_T_A+(E_Z_A+E_N_A+E_C_A)+E_K_A
      E_prom = E_int-self%mol(1)%SCF_data%energy*fac
      call put_Roby_energy_parts_(E_T_A,E_K_A,E_C_A,E_Z_A,E_N_A,E_int,E_prom)
      call text_(stdout," ")
      call text_(stdout,"Isolated Monomer 1 energies (kcal/mol):")
      call text_(stdout," ")
      call show_(stdout,"Monomer 1 energy   =",self%mol(1)%SCF_data%energy)
      call show_(stdout,"... in k/cal mol   =",self%mol(1)%SCF_data%energy*fac)
      call text_(stdout," ")
      call put_Roby_energy_parts_(E0_T_A,E0_K_A,E0_C_A,E0_Z_A,E_N_A)
      call text_(stdout," ")
      call text_(stdout,"Changes in monomer 1 energy contributions (kcal/mol):")
      E_kin = E_T_A-E0_T_A
      E_ex = E_K_A-E0_K_A
      E_coul = E_C_A-E0_C_A
      E_att = E_Z_A-E0_Z_A  ! nuclear attraction
      E_rep = ZERO  ! change in nuclear repulsion is zero
      E_net_coul = E_att+E_rep+E_coul
      E_prom = E_kin+E_net_coul+E_ex
      call put_Roby_energy_parts_(E_kin,E_ex,E_coul,E_att,E_rep,E_prom=E_prom)
      call text_(stdout," ")
      call text_(stdout,"Interacting Monomer 2 energies (kcal/mol):")
      call text_(stdout," ")
      E_int = E_T_B+(E_Z_B+E_N_B+E_C_B)+E_K_B
      E_prom = E_int-self%mol(2)%SCF_data%energy*fac
      call put_Roby_energy_parts_(E_T_B,E_K_B,E_C_B,E_Z_B,E_N_B,E_int,E_prom)
      call text_(stdout," ")
      call text_(stdout,"Isolated Monomer 2 energies (kcal/mol):")
      call text_(stdout," ")
      call show_(stdout,"Monomer 2 energy   =",self%mol(2)%SCF_data%energy)
      call show_(stdout,"... in k/cal mol   =",self%mol(2)%SCF_data%energy*fac)
      call text_(stdout," ")
      call put_Roby_energy_parts_(E0_T_B,E0_K_B,E0_C_B,E0_Z_B,E_N_B)
      call text_(stdout," ")
      call text_(stdout,"Changes in monomer 2 energy contributions (kcal/mol):")
      E_kin = E_T_B-E0_T_B
      E_ex = E_K_B-E0_K_B
      E_coul = E_C_B-E0_C_B
      E_att = E_Z_B-E0_Z_B  ! nuclear attraction
      E_rep = ZERO  ! change in nuclear repulsion is zero
      E_net_coul = E_att+E_rep+E_coul
      E_prom = E_kin+E_net_coul+E_ex
      call put_Roby_energy_parts_(E_kin,E_ex,E_coul,E_att,E_rep,E_prom=E_prom)
   end subroutine

   subroutine put_Roby_energy_parts(E_T,E_K,E_C,E_Z,E_N,E_int,E_prom)
   ! Put of the Roby energy parts
      REAL, IN :: E_T,E_K,E_C,E_Z,E_N
      REAL, optional, IN :: E_int,E_prom
      call flush_(stdout)
      call show_(stdout,"Kinetic energy       =",E_T)
      call show_(stdout,"Exchange energy      =",E_K)
      call show_(stdout,"Kinetic + Exhange    =",E_T+E_K)
      call show_(stdout,"Nuclear attraction   =",E_Z)
      call show_(stdout,"Nuclear repulsion    =",E_N)
      call show_(stdout,"Electronic repulsion =",E_C)
      call show_(stdout,"Net coulomb energy   =",E_Z+E_N+E_C)
      if (present(E_int)) call show_(stdout,"Total int. energy    =",E_int)
      if (present(E_prom)) call show_(stdout,"Promotion energy     =",E_prom)
   end subroutine


!  ====================
!  Energy decomposition
!  ====================

!   put_energy_decomposition
!   ! Put out the energy decomposition specified in the .atom_group array
!      T,Z,C,K,P,E_T,E_Z,E_C,E_K :: MAT{REAL}@
!      n_group,i,j,ia,ib,a,b, fa,la,fb,lb :: INT
!      fac :: REAL
!      unit :: STR
!      arch1 :: ARCHIVE
!   ENSURE(.atom_group.allocated,"no atom group information")
!      arch1.set(.name,"density_mx,restricted")
!   ENSURE(arch1.exists,"no density matrix")
!      P.create(.n_bf,.n_bf)
!      arch1.read(P)
!      .density_mx.create("restricted")
!      .density_mx.r = P
!      T.create(.n_bf,.n_bf); .get_kinetic_energy_mx(T)
!      Z.create(.n_bf,.n_bf); .get_attraction_nuclear_mx(Z)
!      C.create(.n_bf,.n_bf)
!      K.create(.n_bf,.n_bf); .make_r_JK_direct(C,K,P)
!      .make_r_fock(direct=TRUE,core=FALSE,r12=TRUE)
!      n_group = size(.atom_group)
!      E_T.create(n_group,n_group); E_T = ZERO
!      E_Z.create(n_group,n_group); E_Z = ZERO
!      E_C.create(n_group,n_group); E_C = ZERO
!      E_K.create(n_group,n_group); E_K = ZERO
!      do i = 1,n_group
!      do j = 1,n_group
!      do ia = 1,size(.atom_group(i).element)
!      do ib = 1,size(.atom_group(j).element)
!         a = .atom_group(i).element(ia)
!         b = .atom_group(j).element(ib)
!         fa = .first_basis_fn_for_atom(a)
!         la = .last_basis_fn_for_atom(a)
!         fb = .first_basis_fn_for_atom(b)
!         lb = .last_basis_fn_for_atom(b)
!         fac = ONE
!       ! if (i/=j) fac = TWO
!         E_T(i,j) = E_T(i,j) + fac*T(fa:la,fb:lb).trace_product_with(P(fb:lb,fa:la))
!         E_Z(i,j) = E_Z(i,j) + fac*Z(fa:la,fb:lb).trace_product_with(P(fb:lb,fa:la))
!         E_C(i,j) = E_C(i,j) + fac*C(fa:la,fb:lb).trace_product_with(P(fb:lb,fa:la))
!         E_K(i,j) = E_K(i,j) - fac*K(fa:la,fb:lb).trace_product_with(P(fb:lb,fa:la))
!      end
!      end
!      end
!      end
!      E_C = HALF*E_C
!      E_K = HALF*HALF*E_K
!      stdout.text(" ")
!      stdout.text("F:")
!      stdout.put(.fock_mx.r)
!      stdout.text(" ")
!      stdout.text("C-K/2:")
!      stdout.put(C-HALF*K)
!      stdout.text(" ")
!      stdout.text("Energy decomposition in AU ...")
!      stdout.text(" ")
!      stdout.text("Kinetic interaction terms")
!      stdout.put(E_T)
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms")
!      stdout.put(E_Z)
!      stdout.text(" ")
!      stdout.text("Coulomb repulsion interaction terms")
!      stdout.put(E_C)
!      stdout.text(" ")
!      stdout.text("Exchange interaction terms")
!      stdout.put(E_K)
!      stdout.text(" ")
!      fac = sum(E_T+E_Z+E_C+E_K)
!      stdout.show("SCF electronic energy =",fac)
!      stdout.show("SCF energy            =",fac+.atom.nuclear_repulsion_energy)
!      stdout.show("Kinetic energy        =",sum(E_T))
!      !
!      unit = "kcal/mol"
!      fac = unit.conversion_factor
!      E_T = fac*E_T
!      E_Z = fac*E_Z
!      E_C = fac*E_C
!      E_K = fac*E_K
!      stdout.text(" ")
!      stdout.text("Energy decomposition in kcal/mol ...")
!      stdout.text(" ")
!      stdout.text("Kinetic interaction terms")
!      stdout.put(E_T)
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms")
!      stdout.put(E_Z)
!      stdout.text(" ")
!      stdout.text("Coulomb repulsion interaction terms")
!      stdout.put(E_C)
!      stdout.text(" ")
!      stdout.text("Exchange interaction terms")
!      stdout.put(E_K)
!      stdout.text(" ")
!      E_K.destroy
!      E_C.destroy
!      E_Z.destroy
!      E_T.destroy
!      K.destroy
!      C.destroy
!      Z.destroy
!      T.destroy
!   end
!
!   put_energy_partition
!   ! Put out the energy decomposition specified in the .atom_group array
!   ! This routine calculates quadricentric contributions which are
!   ! comparable to the Fischer-Kollmar decomposition.
!      T,Z,ZZ,C,K,P,W,E_T,E_Z, EE :: MAT{REAL}@
!      E_ZZ :: MAT3{REAL}@
!      E_C,E_K :: MAT4{REAL}@
!      n_group,x,y,i,j :: INT
!      fac :: REAL
!      unit :: STR
!      arch1 :: ARCHIVE
!      kinetic_energy,nuclear_attraction,nuclear_repulsion :: REAL
!      coulomb_repulsion,net_coulomb,bicentric_exchange :: REAL
!      exchange_attraction,total_interaction :: REAL
!   ENSURE(.atom_group.allocated,"no atom group information")
!      arch1.set(.name,"density_mx,restricted")
!   ENSURE(arch1.exists,"no density matrix")
!      P.create(.n_bf,.n_bf)
!      arch1.read(P)
!      W.create(.n_bf,.n_bf)
!      T.create(.n_bf,.n_bf); .get_kinetic_energy_mx(T)
!      Z.create(.n_bf,.n_bf); .get_attraction_nuclear_mx(Z)
!      n_group = size(.atom_group)
!      E_T.create(n_group,n_group); E_T = ZERO
!      E_Z.create(n_group,n_group); E_Z = ZERO
!      E_C.create(n_group,n_group,n_group,n_group); E_C = ZERO
!      E_K.create(n_group,n_group,n_group,n_group); E_K = ZERO
!      E_ZZ.create(n_group,n_group,n_group); E_ZZ = ZERO
!      ZZ.create(.n_bf,.n_bf)
!         do i = 1,n_group
!         do j = 1,i
!            W = ZERO
!            .atom_group_AO_subspace_set(W,P,i,j)

!            .atom_group_AO_subspace_set(W,P,j,i)
!            E_T(i,j) = T.trace_product_with(W)
!            E_Z(i,j) = Z.trace_product_with(W)
!            do x = 1,n_group
!               .make_nuclear_attraction_mx(ZZ,.atom_group(x).element)
!               E_ZZ(i,j,x) = ZZ.trace_product_with(W)
!            end
!         end
!         end
!      ZZ.destroy
!      C.create(.n_bf,.n_bf)
!      K.create(.n_bf,.n_bf)
!      do x = 1,n_group
!      do y = 1,x
!         .make_r_JK_group(C,K,P,x,y)
!         do i = 1,n_group
!         do j = 1,i
!            W = ZERO
!            .atom_group_AO_subspace_set(W,P,i,j)

!            .atom_group_AO_subspace_set(W,P,j,i)
!            E_C(i,j,x,y) =  C.trace_product_with(W)
!            E_K(i,j,x,y) = -K.trace_product_with(W)
!         end
!         end
!      end
!      end
!      K.destroy
!      C.destroy
!      E_C = HALF*E_C
!      E_K = HALF*HALF*E_K
!      EE.create(n_group,n_group)
!      !
!      stdout.text(" ")
!      fac = sum(E_T+E_Z) + sum(E_C+E_K)
!      stdout.show("SCF electronic energy =",fac)
!      stdout.show("SCF energy            =",fac+.atom.nuclear_repulsion_energy)
!      stdout.show("Kinetic energy        =",sum(E_T))
!!     stdout.text(" ")
!!     stdout.text("Energy decomposition in AU ...")
!!     stdout.text(" ")
!!     stdout.text("Kinetic interaction terms")
!!     stdout.put(E_T)
!!     stdout.text(" ")
!!     stdout.text("Nuclear attraction interaction terms")
!!     stdout.put(E_Z)
!!     stdout.text(" ")
!!     stdout.text("Nuclear attraction interaction terms ONLY for group 1 nuclei")
!!     stdout.put(E_ZZ(:,:,1))
!!     stdout.text(" ")
!!     stdout.text("Nuclear attraction interaction terms ONLY for group 2 nuclei")
!!     stdout.put(E_ZZ(:,:,2))
!!     EE(1,1) = .atom(.atom_group(1).element).nuclear_repulsion_energy
!!     EE(2,2) = .atom(.atom_group(2).element).nuclear_repulsion_energy
!!     EE(2,1) = .atom.nuclear_repulsion_energy - EE(1,1) - EE(2,2)
!!     EE(1,2) = ZERO
!!     stdout.text(" ")
!!     stdout.text("Nuclear nuclear repulsion")
!!     stdout.put(EE)
!!     EE(1,1) = E_C(1,1,1,1)
!!     EE(2,1) = E_C(2,2,1,1)
!!     EE(1,2) = E_C(1,1,2,2)
!!     EE(2,2) = E_C(2,2,2,2)
!!     stdout.text(" ")
!!     stdout.text("Diagonal Coulomb repulsion interaction terms")
!!     stdout.put(EE)
!!     EE(1,1) = E_C(1,2,1,1)
!!     EE(2,1) = E_C(2,1,1,1)
!!     EE(1,2) = E_C(1,2,2,2)
!!     EE(2,2) = E_C(2,1,2,2)
!!     stdout.text(" ")
!!     stdout.text("Semi diagonal Coulomb repulsion interaction terms")
!!     stdout.put(EE)
!!     EE(1,1) = E_C(1,2,1,2)
!!     EE(2,1) = E_C(2,1,1,2)
!!     EE(1,2) = E_C(1,2,2,1)
!!     EE(2,2) = E_C(2,1,2,1)
!!     stdout.text(" ")
!!     stdout.text("Off diagonal Coulomb repulsion interaction terms")
!!     stdout.put(EE)
!!     stdout.text(" ")
!!     stdout.text("All Coulomb repulsion interaction terms")
!!     stdout.put(E_C)
!!     EE(1,1) = E_K(1,1,1,1)
!!     EE(2,1) = E_K(2,2,1,1)
!!     EE(1,2) = E_K(1,1,2,2)
!!     EE(2,2) = E_K(2,2,2,2)
!!     stdout.text(" ")
!!     stdout.text("Diagonal exchange interaction terms")
!!     stdout.put(EE)
!!     EE(1,1) = E_K(1,2,1,1)
!!     EE(2,1) = E_K(2,1,1,1)
!!     EE(1,2) = E_K(1,2,2,2)
!!     EE(2,2) = E_K(2,1,2,2)
!!     stdout.text(" ")
!!     stdout.text("Semi diagonal exchange interaction terms")
!!     stdout.put(EE)
!!     EE(1,1) = E_K(1,2,1,2)
!!     EE(2,1) = E_K(2,1,1,2)
!!     EE(1,2) = E_K(1,2,2,1)
!!     EE(2,2) = E_K(2,1,2,1)
!!     stdout.text(" ")
!!     stdout.text("Off diagonal exchange interaction terms")
!!     stdout.put(EE)
!!     stdout.text(" ")
!!     stdout.text("All exchange interaction terms")
!!     stdout.put(E_K)
!      !
!      unit = "kcal/mol"
!      fac = unit.conversion_factor
!      E_T = fac*E_T
!      E_Z = fac*E_Z
!      E_C = fac*E_C
!      E_K = fac*E_K
!      E_ZZ = fac*E_ZZ
!      stdout.text(" ")
!      stdout.text("Energy decomposition in kcal/mol ...")
!      stdout.text(" ")
!      stdout.text("Kinetic interaction terms")
!      stdout.put(E_T)
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms")
!      stdout.put(E_Z)
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms ONLY for group 1 nuclei")
!      stdout.put(E_ZZ(:,:,1))
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms ONLY for group 2 nuclei")
!      stdout.put(E_ZZ(:,:,2))
!      EE(1,1) = fac*.atom(.atom_group(1).element).nuclear_repulsion_energy
!      EE(2,2) = fac*.atom(.atom_group(2).element).nuclear_repulsion_energy
!      nuclear_repulsion = fac*.atom.nuclear_repulsion_energy - EE(1,1) - EE(2,2)
!      EE(2,1) = nuclear_repulsion
!      EE(1,2) = ZERO
!      stdout.text(" ")
!      stdout.text("Nuclear nuclear repulsion")
!      stdout.put(EE)
!      EE(1,1) = E_C(1,1,1,1)
!      EE(2,1) = E_C(2,2,1,1)
!      EE(1,2) = E_C(1,1,2,2)
!      EE(2,2) = E_C(2,2,2,2)
!      stdout.text(" ")
!      stdout.text("Diagonal Coulomb repulsion interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_C(1,2,1,1)
!      EE(2,1) = E_C(2,1,1,1)
!      EE(1,2) = E_C(1,2,2,2)
!      EE(2,2) = E_C(2,1,2,2)
!      stdout.text(" ")
!      stdout.text("Semi diagonal Coulomb repulsion interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_C(1,2,1,2)
!      EE(2,1) = E_C(2,1,1,2)
!      EE(1,2) = E_C(1,2,2,1)
!      EE(2,2) = E_C(2,1,2,1)
!      stdout.text(" ")
!      stdout.text("Off diagonal Coulomb repulsion interaction terms")
!      stdout.put(EE)
!      stdout.text(" ")
!      stdout.text("All Coulomb repulsion interaction terms")
!      stdout.put(E_C)
!      EE(1,1) = E_K(1,1,1,1)
!      EE(2,1) = E_K(2,2,1,1)
!      EE(1,2) = E_K(1,1,2,2)
!      EE(2,2) = E_K(2,2,2,2)
!      stdout.text(" ")
!      stdout.text("Diagonal exchange interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_K(1,2,1,1)
!      EE(2,1) = E_K(2,1,1,1)
!      EE(1,2) = E_K(1,2,2,2)
!      EE(2,2) = E_K(2,1,2,2)
!      stdout.text(" ")
!      stdout.text("Semi diagonal exchange interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_K(1,2,1,2)
!      EE(2,1) = E_K(2,1,1,2)
!      EE(1,2) = E_K(1,2,2,1)
!      EE(2,2) = E_K(2,1,2,1)
!      stdout.text(" ")
!      stdout.text("Off diagonal exchange interaction terms")
!      stdout.put(EE)
!      stdout.text(" ")
!      stdout.text("All exchange interaction terms")
!      stdout.put(E_K)
!      kinetic_energy = E_T(2,1)
!      nuclear_attraction = sum(E_Z) - E_ZZ(1,1,1) - E_ZZ(2,2,2)
!      coulomb_repulsion = sum(E_C) - E_C(1,1,1,1) - E_C(2,2,2,2)
!      net_coulomb = nuclear_attraction + nuclear_repulsion + coulomb_repulsion
!      bicentric_exchange = E_K(1,1,2,2) + E_K(2,2,1,1)
!      exchange_attraction = sum(E_K) - E_K(1,1,1,1) - E_K(2,2,2,2)
!      total_interaction = kinetic_energy + net_coulomb + exchange_attraction
!      stdout.text(" ")
!      stdout.text("Interaction energies for "//trim(.name)//":")
!      stdout.text(" ")
!      stdout.show("Kinetic energy     =",kinetic_energy)
!      stdout.show("Exchange energy    =",exchange_attraction)
!      stdout.show("Kinetic + Exhange  =",kinetic_energy+exchange_attraction)
!      stdout.show("Bicentric Exchange =",bicentric_exchange)
!      stdout.show("Net Coulomb energy =",net_coulomb)
!      stdout.show("Total int. energy  =",total_interaction)
!      EE.destroy
!      E_ZZ.destroy
!      E_K.destroy
!      E_C.destroy
!      E_Z.destroy
!      E_T.destroy
!      Z.destroy
!      T.destroy
!      W.destroy
!      P.destroy
!   end
!
!   put_ortho_energy_partition
!   ! Put out the energy decomposition specified in the .atom_group array
!   ! This routine calculates quadricentric contributions which are
!   ! comparable to the Fischer-Kollmar decomposition in an orthogonalised
!   ! AO basis set
!      T, Z,ZZ, C :: MAT{REAL}@
!      K, P,PP, W :: MAT{REAL}@
!      E_T,E_Z,EE :: MAT{REAL}@
!      S, Sph,Smh :: MAT{REAL}@
!      E_ZZ :: MAT3{REAL}@
!      E_C,E_K :: MAT4{REAL}@
!      n_group,i,j,g,h, fa,la,fb,lb :: INT
!      fac,fac_ij,fac_xy :: REAL
!      unit :: STR
!      arch1 :: ARCHIVE
!      kinetic_energy,nuclear_attraction,nuclear_repulsion :: REAL
!      coulomb_repulsion,net_coulomb,bicentric_exchange :: REAL
!      exchange_attraction,total_interaction :: REAL
!   ENSURE(.atom_group.allocated,"no atom group information")
!      arch1.set(.name,"density_mx,restricted")
!   ENSURE(arch1.exists,"no density matrix")
!      P.create(.n_bf,.n_bf)
!      PP.create(.n_bf,.n_bf)
!      arch1.read(P)
!      W.create(.n_bf,.n_bf)
!      T.create(.n_bf,.n_bf); .get_kinetic_energy_mx(T)
!      Z.create(.n_bf,.n_bf); .get_attraction_nuclear_mx(Z)
!      S.create(.n_bf,.n_bf); .INTS_overlap_mx(S)
!      Sph.create(.n_bf,.n_bf); Sph.to_sqrt(S)
!      Smh.create(.n_bf,.n_bf); Smh.to_inverse_sqrt(S)
!      n_group = size(.atom_group)
!      E_T.create(n_group,n_group); E_T = ZERO
!      E_Z.create(n_group,n_group); E_Z = ZERO
!      E_C.create(n_group,n_group,n_group,n_group); E_C = ZERO
!      E_K.create(n_group,n_group,n_group,n_group); E_K = ZERO
!      E_ZZ.create(n_group,n_group,n_group); E_ZZ = ZERO
!      ZZ.create(.n_bf,.n_bf)
!      PP = P
!      PP.change_basis_using(Sph)
!         do i = 1,n_group
!         do j = 1,i
!            W = ZERO
!            .atom_group_AO_subspace_set(W,PP,i,j)

!            .atom_group_AO_subspace_set(W,PP,j,i)
!            W.change_basis_using(Smh)
!            E_T(i,j) = T.trace_product_with(W)
!            E_Z(i,j) = Z.trace_product_with(W)
!            do g = 1,n_group
!               .make_nuclear_attraction_mx(ZZ,.atom_group(g).element)
!               E_ZZ(i,j,g) = ZZ.trace_product_with(W)
!            end
!         end
!         end
!      ZZ.destroy
!      C.create(.n_bf,.n_bf)
!      K.create(.n_bf,.n_bf)
!      do g = 1,n_group
!      do h = 1,g
!         W = ZERO
!         .atom_group_AO_subspace_set(W,PP,g,h)

!         .atom_group_AO_subspace_set(W,PP,h,g)
!         W.change_basis_using(Smh)
!         .make_r_JK_nosym(C,K,W)
!         do i = 1,n_group
!         do j = 1,i
!            W = ZERO
!            .atom_group_AO_subspace_set(W,PP,i,j)

!            .atom_group_AO_subspace_set(W,PP,j,i)
!            W.change_basis_using(Smh)
!            E_C(i,j,g,h) =  C.trace_product_with(W)
!            E_K(i,j,g,h) = -K.trace_product_with(W)
!         end
!         end
!      end
!      end
!      K.destroy
!      C.destroy
!      E_C = HALF*E_C
!      E_K = HALF*HALF*E_K
!      EE.create(n_group,n_group)
!      !
!      stdout.text(" ")
!      fac = sum(E_T+E_Z) + sum(E_C+E_K)
!      stdout.show("SCF electronic energy =",fac)
!      stdout.show("SCF energy            =",fac+.atom.nuclear_repulsion_energy)
!      stdout.show("Kinetic energy        =",sum(E_T))
!      !
!      unit = "kcal/mol"
!      fac = unit.conversion_factor
!      E_T = fac*E_T
!      E_Z = fac*E_Z
!      E_C = fac*E_C
!      E_K = fac*E_K
!      E_ZZ = fac*E_ZZ
!      stdout.text(" ")
!      stdout.text("Energy decomposition in kcal/mol ...")
!      stdout.text(" ")
!      stdout.text("Kinetic interaction terms")
!      stdout.put(E_T)
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms")
!      stdout.put(E_Z)
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms ONLY for group 1 nuclei")
!      stdout.put(E_ZZ(:,:,1))
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms ONLY for group 2 nuclei")
!      stdout.put(E_ZZ(:,:,2))
!      EE(1,1) = fac*.atom(.atom_group(1).element).nuclear_repulsion_energy
!      EE(2,2) = fac*.atom(.atom_group(2).element).nuclear_repulsion_energy
!      nuclear_repulsion = fac*.atom.nuclear_repulsion_energy - EE(1,1) - EE(2,2)
!      EE(2,1) = nuclear_repulsion
!      EE(1,2) = ZERO
!      stdout.text(" ")
!      stdout.text("Nuclear nuclear repulsion")
!      stdout.put(EE)
!      EE(1,1) = E_C(1,1,1,1)
!      EE(2,1) = E_C(2,2,1,1)
!      EE(1,2) = E_C(1,1,2,2)
!      EE(2,2) = E_C(2,2,2,2)
!      stdout.text(" ")
!      stdout.text("Diagonal Coulomb repulsion interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_C(1,2,1,1)
!      EE(2,1) = E_C(2,1,1,1)
!      EE(1,2) = E_C(1,2,2,2)
!      EE(2,2) = E_C(2,1,2,2)
!      stdout.text(" ")
!      stdout.text("Semi diagonal Coulomb repulsion interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_C(1,2,1,2)
!      EE(2,1) = E_C(2,1,1,2)
!      EE(1,2) = E_C(1,2,2,1)
!      EE(2,2) = E_C(2,1,2,1)
!      stdout.text(" ")
!      stdout.text("Off diagonal Coulomb repulsion interaction terms")
!      stdout.put(EE)
!      stdout.text(" ")
!      stdout.text("All Coulomb repulsion interaction terms")
!      stdout.put(E_C)
!      EE(1,1) = E_K(1,1,1,1)
!      EE(2,1) = E_K(2,2,1,1)
!      EE(1,2) = E_K(1,1,2,2)
!      EE(2,2) = E_K(2,2,2,2)
!      stdout.text(" ")
!      stdout.text("Diagonal exchange interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_K(1,2,1,1)
!      EE(2,1) = E_K(2,1,1,1)
!      EE(1,2) = E_K(1,2,2,2)
!      EE(2,2) = E_K(2,1,2,2)
!      stdout.text(" ")
!      stdout.text("Semi diagonal exchange interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_K(1,2,1,2)
!      EE(2,1) = E_K(2,1,1,2)
!      EE(1,2) = E_K(1,2,2,1)
!      EE(2,2) = E_K(2,1,2,1)
!      stdout.text(" ")
!      stdout.text("Off diagonal exchange interaction terms")
!      stdout.put(EE)
!      stdout.text(" ")
!      stdout.text("All exchange interaction terms")
!      stdout.put(E_K)
!      kinetic_energy = E_T(2,1)
!      nuclear_attraction = sum(E_Z) - E_ZZ(1,1,1) - E_ZZ(2,2,2)
!      coulomb_repulsion = sum(E_C) - E_C(1,1,1,1) - E_C(2,2,2,2)
!      net_coulomb = nuclear_attraction + nuclear_repulsion + coulomb_repulsion
!      bicentric_exchange = E_K(1,1,2,2) + E_K(2,2,1,1)
!      exchange_attraction = sum(E_K) - E_K(1,1,1,1) - E_K(2,2,2,2)
!      total_interaction = kinetic_energy + net_coulomb + exchange_attraction
!      stdout.text(" ")
!      stdout.text("Interaction energies for "//trim(.name)//":")
!      stdout.text(" ")
!      stdout.show("Kinetic energy     =",kinetic_energy)
!      stdout.show("Exchange energy    =",exchange_attraction)
!      stdout.show("Kinetic + Exhange  =",kinetic_energy+exchange_attraction)
!      stdout.show("Bicentric Exchange =",bicentric_exchange)
!      stdout.show("Net Coulomb energy =",net_coulomb)
!      stdout.show("Total int. energy  =",total_interaction)
!      EE.destroy
!      E_ZZ.destroy
!      E_K.destroy
!      E_C.destroy
!      E_Z.destroy
!      E_T.destroy
!      Smh.destroy
!      Sph.destroy
!      S.destroy
!      Z.destroy
!      T.destroy
!      W.destroy
!      PP.destroy
!      P.destroy
!   end
!
!   make_r_JK_group(J,K,P,row_group,col_group)
!   ! Make the real coulomb matrix "J" and exchange matrix "K" matrix from the
!   ! density matrix "P" which is non-zero only for rows belonging to basis
!   ! functions on atoms in group "row_group" and also columns for basis functions
!   ! on atoms in the "col_group", as specified in ".atom_group".
!     J,K,P :: MAT{REAL}
!     row_group,col_group :: INT
!     PP :: MAT{REAL}@
!     I :: MAT4{REAL}@
!     max_I,max_P :: VEC{REAL}@
!     sh4 :: SHELL4
!     ab,cd,i_a,i_b,i_c,i_d :: INT
!     a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld :: INT
!     I_abcd,factor,cutoff :: REAL
!     skip :: BIN
!   ENSURE(.atom_group.allocated,"no atom group information")
!   ENSURE(row_group<=size(.atom_group),"no such row group")
!   ENSURE(col_group<=size(.atom_group),"no such column group")
!   ENSURE(row_group>0,"row group index must be positive")
!   ENSURE(col_group>0,"column group index must be positive")
!
!     J = ZERO
!     K = ZERO
!     cutoff = .SCF_data.ERI_limit
!     PP.create(.n_bf,.n_bf)
!     PP = ZERO
!     .atom_group_AO_subspace_set(PP,P,row_group,col_group)

!     .atom_group_AO_subspace_set(PP,P,col_group,row_group)
!     max_I.create(.n_shell_pairs)
!     max_P.create(.n_shell_pairs)
!     .make_max_abab_integrals(max_I)
!     .make_max_density_elements(max_P,PP)
!     do ab = 1, .n_shell_pairs
!       .get_shell_pair_indices(ab,i_a,i_b)    ! a & b shell indices.
!       fa = .first_basis_fn_for_shell(i_a)
!       fb = .first_basis_fn_for_shell(i_b)
!       la = .last_basis_fn_for_shell(i_a)
!       lb = .last_basis_fn_for_shell(i_b)
!       .set_shell_quartet_ab(sh4,i_a,i_b)
!       do cd = 1,ab
!         .get_shell_pair_indices(cd,i_c,i_d)  ! c & d shell indices.
!         fc = .first_basis_fn_for_shell(i_c)
!         fd = .first_basis_fn_for_shell(i_d)
!         lc = .last_basis_fn_for_shell(i_c)
!         ld = .last_basis_fn_for_shell(i_d)
!                                        ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
!         skip = .schwarz_inequality_test(cutoff,ab,cd,i_a,i_b,i_c,i_d,max_P,max_I)
!         if (skip) cycle
!                                              ! calculate ab|cd
!         .set_shell_quartet_cd(sh4,i_c,i_d)
!         factor = ONE                         ! Evaluate the integrals'
!         if (i_a==i_b) factor = HALF          ! coincidence factors
!         if (i_c==i_d) factor = HALF * factor
!         if (i_a==i_c AND i_b==i_d) factor = HALF * factor
!         I.create(fa,la,fb,lb,fc,lc,fd,ld)
!         sh4.get_ERI(I)
!           do d = fd,ld
!           do c = fc,lc
!           do b = fb,lb
!           do a = fa,la
!            I_abcd = factor * I(a,b,c,d)
!            J(a,b) = J(a,b) + I_abcd*PP(d,c) ! These are symmetric
!            J(a,b) = J(a,b) + I_abcd*PP(c,d)
!            J(b,a) = J(b,a) + I_abcd*PP(d,c)
!            J(b,a) = J(b,a) + I_abcd*PP(c,d)
!            J(c,d) = J(c,d) + I_abcd*PP(b,a)
!            J(c,d) = J(c,d) + I_abcd*PP(a,b)
!            J(d,c) = J(d,c) + I_abcd*PP(b,a)
!            J(d,c) = J(d,c) + I_abcd*PP(a,b)
!            K(a,d) = K(a,d) + I_abcd*PP(b,c) ! These are asymmetric
!            K(a,c) = K(a,c) + I_abcd*PP(b,d)
!            K(b,c) = K(b,c) + I_abcd*PP(a,d)
!            K(b,d) = K(b,d) + I_abcd*PP(a,c)
!            K(d,a) = K(d,a) + I_abcd*PP(c,b)
!            K(c,a) = K(c,a) + I_abcd*PP(d,b)
!            K(c,b) = K(c,b) + I_abcd*PP(d,a)
!            K(d,b) = K(d,b) + I_abcd*PP(c,a)
!           end
!           end
!           end
!           end
!         I.destroy
!         sh4.destroy_cd
!       end
!       sh4.destroy_ab
!     end
!     max_P.destroy
!     max_I.destroy
!     PP.destroy
!   end

!  =========================
!  Crystal Explorer energies
!  =========================

   subroutine put_group_12_Grimme2006_energy(self)
   ! Calculate the 1-2 dispersion term from Grimme's C6 coefficients
   ! C6 taken from Grimme (2006) J. Comp. Chem.  27(15) p. 1787
      type(MOLECULE_TYPE), target, INOUT :: self
      STR(len=STR_SIZE) :: unit
      REAL :: E_disp,fac
   ENSURE(allocated(self%atom_group),"MOLECULE.PROP:put_group_12_Grimme2006_energy ... no atom group information")
   ENSURE(size(self%atom_group)==2,"MOLECULE.PROP:put_group_12_Grimme2006_energy ... must have only 2 groups")
      E_disp = Grimme06_dispersion_energy_(self%mol(1)%atom,self%mol(2)%atom)

      ! Convert units to kJ mol-1
      unit = "kJ/mol"
      fac = conversion_factor_(unit)
      E_disp = E_disp*fac

      ! Output energies
      call flush_(stdout)
      call text_(stdout,"=========================================")
      call text_(stdout,"Grimme (2006) atom-atom dispersion energy")
      call text_(stdout,"=========================================")
      call flush_(stdout)
      call text_(stdout,". See: Grimme (2006) J. Comp. Chem.  27(15) p. 1787")
      call flush_(stdout)
      call text_(stdout,". E_disp = sum over atoms (C6 / r^6).")
      call flush_(stdout)
      call show_(stdout,"Grimme06 dispersion energy (kJ/mol) = ",E_disp)
   end subroutine

   subroutine put_group_12_Spackman1986_energies(self)
   ! Calculate the 1-2 dispersion and repulsion term from atom-atom potentials
   ! See Spackman M. A. (1986) J. Chem. Phys. 85(11) p6579
      type(MOLECULE_TYPE), target, INOUT :: self
      STR(len=STR_SIZE) :: unit
      REAL :: E_disp,E_rep,fac
   ENSURE(allocated(self%atom_group),"MOLECULE.PROP:put_group_12_Spackman1986_energies ... no atom group information")
   ENSURE(size(self%atom_group)==2,"MOLECULE.PROP:put_group_12_Spackman1986_energies ... must have only 2 groups")
      E_disp = Spackman86_dispersion_energy_(self%mol(1)%atom,self%mol(2)%atom)
      E_rep = Spackman86_repulsion_energy_(self%mol(1)%atom,self%mol(2)%atom)

      ! Convert units to kJ mol-1
      unit = "kJ/mol"
      fac = conversion_factor_(unit)
      E_disp = E_disp*fac
      E_rep = E_rep*fac

      ! Output energies
      call flush_(stdout)
      call text_(stdout,"==============================================")
      call text_(stdout,"Spackman (1986) atom-atom interaction energies")
      call text_(stdout,"==============================================")
      call flush_(stdout)
      call text_(stdout,". See: Spackman (1986) JCP 85 p. 6579")
      call text_(stdout,". Repulsion is an exponential fit to Gordon-Kim")
      call text_(stdout,"  energies, modified for H-bonding and H-H contacts")
      call flush_(stdout)
      call text_(stdout,". Dispersion from C6 London-formula-combined atomic")
      call text_(stdout,"  dipole polarizabilities")
      call flush_(stdout)
      call show_(stdout,"Spackman86 dispersion energy (kJ/mol) = ",E_disp)
      call flush_(stdout)
      call show_(stdout,"Spackman86 repulsion  energy (kJ/mol) = ",E_rep)
   end subroutine

   subroutine put_group_12_polarization_energy(self)
   ! Calculate the 1-2 polarization term
   ! Atomic polarizabilities from Thakkar and Lupinetti (2006)
      type(MOLECULE_TYPE), target, INOUT :: self
      MAT(REAL,:,:), allocatable :: Fe_at_A,Fn_at_A,Ft_at_A
      MAT(REAL,:,:), allocatable :: Fe_at_B,Fn_at_B,Ft_at_B
      VEC(REAL,:), allocatable :: F2_at_A,F2_at_B
      BIN :: specialA,specialB
      REAL :: E_pol,fac
      INT :: n1,n2
      STR(len=STR_SIZE) :: unit
   ENSURE(allocated(self%atom_group),"MOLECULE.PROP:put_group_12_polarization_energy ... no atom group information")
   ENSURE(size(self%atom_group)==2,"MOLECULE.PROP:put_group_12_polarization_energy ... must have only 2 groups")
   ENSURE(allocated(self%mol(1)%density_mx),"MOLECULE.PROP:put_group_12_polarization_energy ... no mol 1 DM!")
   ENSURE(allocated(self%mol(2)%density_mx),"MOLECULE.PROP:put_group_12_polarization_energy ... no mol 2 DM!")
      n1 = self%mol(1)%n_atom
      n2 = self%mol(2)%n_atom

      ! Electric field from B at A
      call create_(Ft_at_A,3,n1)
      call create_(Fn_at_A,3,n1)
      call create_(Fe_at_A,3,n1)
      Fe_at_A = electronic_E_field_at_nuclei_of_(self%mol(2),self%mol(1)%atom)
      Fn_at_A = nuclear_E_field_at_nuclei_of_(self%mol(2)%atom,self%mol(1)%atom)
      Ft_at_A = Fe_at_A+Fn_at_A
      call destroy_(Fe_at_A)
      call destroy_(Fn_at_A)

      ! Electric field from A at B
      call create_(Ft_at_B,3,n2)
      call create_(Fn_at_B,3,n2)
      call create_(Fe_at_B,3,n2)
      Fe_at_B = electronic_E_field_at_nuclei_of_(self%mol(1),self%mol(2)%atom)
      Fn_at_B = nuclear_E_field_at_nuclei_of_(self%mol(1)%atom,self%mol(2)%atom)
      Ft_at_B = Fe_at_B+Fn_at_B
      call destroy_(Fe_at_B)
      call destroy_(Fn_at_B)

      ! Magnitude of the electric field squared
      call create_(F2_at_A,size(Ft_at_A,2))
      call create_(F2_at_B,size(Ft_at_B,2))
      F2_at_A = column_norms_(Ft_at_A)
      F2_at_B = column_norms_(Ft_at_B)
      call destroy_(Ft_at_B)
      call destroy_(Ft_at_A)
      F2_at_A = F2_at_A**2
      F2_at_B = F2_at_B**2

      ! Polarization Energy
      specialA = self%mol(1)%charge/=0 AND n1==1
      specialB = self%mol(2)%charge/=0 AND n2==1
      E_pol = ZERO
      E_pol = E_pol+polarization_energy_(self%mol(1)%atom,F2_at_A,specialA)
      E_pol = E_pol+polarization_energy_(self%mol(2)%atom,F2_at_B,specialB)

      ! Convert units to kJmol-1
      unit = "kJ/mol"
      fac = conversion_factor_(unit)
      E_pol = E_pol*fac

      ! Output energies
      call flush_(stdout)
      call text_(stdout,"=============================")
      call text_(stdout,"Atom-atom polarization energy")
      call text_(stdout,"=============================")
      call flush_(stdout)
      call text_(stdout,". Atomic dipole polarizabilities from Thakkar and Lupinetti (2006)")
      call flush_(stdout)
      call show_(stdout,"Polarization energy (kJ/mol) = ",E_pol)

      ! Tidy up
      call destroy_(F2_at_B)
      call destroy_(F2_at_A)
   end subroutine


   ! There is confusion between "self" & "mol_B"
   ! versus .mol(1) & .mol(2) ... latter seem better to use

   function intermolecular_polarization_energy(self,mol_B) result (res)
   ! Calculate the 1-2 polarization term
   ! Atomic polarizabilities from Thakkar and Lupinetti (2006)
      type(MOLECULE_TYPE), target, INOUT :: self
      type(MOLECULE_TYPE), INOUT :: mol_B
      REAL :: res
! ENSURE(.mol(1).density_mx.allocated ,"no mol 1 DM!")
! ENSURE(.mol(2).density_mx.allocated ,"no mol 2 DM!")
      MAT(REAL,:,:), allocatable :: electronic_at_A,nuclear_at_A,total_at_A
      MAT(REAL,:,:), allocatable :: electronic_at_B,nuclear_at_B,total_at_B
      VEC(REAL,:), allocatable :: F_sq_A,F_sq_B
      BIN :: specialA,specialB
      REAL :: E_pol
      INT :: n2
   ENSURE(allocated(self%atom_group),"MOLECULE.PROP:intermolecular_polarization_energy ... no atom group information")
   ENSURE(size(self%atom_group)==2,"MOLECULE.PROP:intermolecular_polarization_energy ... must have only 2 groups")

      ! Electric field from B at A
      call create_(electronic_at_A,3,self%n_atom)
      call create_(nuclear_at_A,3,self%n_atom)
      call create_(total_at_A,3,self%n_atom)
      electronic_at_A = electronic_E_field_at_nuclei_of_(mol_B,self%atom)
      nuclear_at_A = nuclear_E_field_at_nuclei_of_(mol_B%atom,self%atom)
      total_at_A = electronic_at_A+nuclear_at_A

      ! Electric field from A at B
      n2 = mol_B%n_atom
      call create_(electronic_at_B,3,n2)
      call create_(nuclear_at_B,3,n2)
      call create_(total_at_B,3,n2)
      electronic_at_B = electronic_E_field_at_nuclei_of_(self,mol_B%atom)
      nuclear_at_B = nuclear_E_field_at_nuclei_of_(self%atom,mol_B%atom)
      total_at_B = electronic_at_B+nuclear_at_B

      ! Magnitude of the electric field squared
      call create_(F_sq_A,size(total_at_A,2))
      call create_(F_sq_B,size(total_at_B,2))
      F_sq_A = column_norms_(total_at_A)
      F_sq_A = F_sq_A**2
      F_sq_B = column_norms_(total_at_B)
      F_sq_B = F_sq_B**2

      ! Polarization Energy
      specialA = self%charge/=0 AND self%n_atom==1
      specialB = mol_B%charge/=0 AND n2==1
      E_pol = ZERO
      E_pol = E_pol+polarization_energy_(self%atom,F_sq_A,specialA)
      E_pol = E_pol+polarization_energy_(mol_B%atom,F_sq_B,specialB)
      res = E_pol

      ! Tidy up
      call destroy_(electronic_at_A)
      call destroy_(nuclear_at_A)
      call destroy_(total_at_A)
      call destroy_(electronic_at_B)
      call destroy_(nuclear_at_B)
      call destroy_(total_at_B)
      call destroy_(F_sq_A)
      call destroy_(F_sq_B)
   end function

   function intermolecular_overlap_repulsion(self,mol_B) result (res)
   ! Calculate the exchange-repulsion energy based on the following
   ! overlap formula (see Soderhjelm et al. JCP 124 (2006) eq. 14)
   ! E = S^2 (1 + 20 e ^(-R))
      type(MOLECULE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), INOUT :: mol_B
      REAL :: res
      REAL :: S2
      MAT(REAL,:,:), allocatable :: X,Y
      INT :: n_a,n_b
      type(MOLECULE_TYPE), allocatable :: merged
      MAT(REAL,:,:), allocatable :: MO_a,MO_b
      call create_from_mols_(merged,self,mol_B)
      call merge_group_MOs_(merged)
      call set_output_(merged%SCF_data,FALSE)

      ! Make SCF density matrices
      call make_overlap_mx_(merged)

      ! Overlap between phi_i and phi_j
      ! = sum_{a b} c_{a i} S_{ab} c_{b j}
      S2 = ZERO
      n_a = index_of_first_zero_value_(self%NO_occ_nos%r(1:))-1
      n_b = index_of_first_zero_value_(mol_B%NO_occ_nos%r(1:))-1
      allocate(MO_a(merged%n_bf,n_a))
      allocate(MO_b(merged%n_bf,n_b))
      MO_a = ZERO
      MO_b = ZERO
      MO_a(:self%n_bf,:) = self%MOs%r(:,1:n_a)
      MO_b(self%n_bf+1:,:) = mol_B%MOs%r(:,1:n_b)
      Y = matmul(merged%overlap_mx,MO_b(:,:))
      X = matmul(transpose(MO_a(:,:)),Y)
      S2 = sum(X*X)
      res = 0.92*S2  ! Magic number
      call show_(stdout,"Overlap repulsion =",res)
      call destroy_(merged)
   end function

   function symmetric_intermolecular_qm_energies(self,mol_B,monomer_energies) result (res)
   ! Calculate the group 1-2 interaction energies.
   ! monomer energies is a vector if TWO * -monomer energies
   ! Effectively avoiding calculating the expectations for the
   ! monomers each time
   ! monomer_enegries = [ E_en, E_1e, E_ee, E_ex ]
   ! res              = [ E_coul, E_rep ]
   !!!! IF mol_B and  self are symmetry related !!!!
   ! For simplicity the notations follows the reference:
   ! P. Su & H. Li (2009) JCP 131 p. 014102
   ! It has references to the original work and is explained fairly
   ! clearly, except for their use of "X" for supermolecule (X=AB).
   ! NOTE: make sure the MO's and density matrices for each group exist!
      type(MOLECULE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), INOUT :: mol_B
      VEC(REAL,4), IN :: monomer_energies
      VEC(REAL,2) :: res

      ! Terms regardless of unrestricted/restricted
      REAL :: E_en,E_ke,E_1e,E_nn,E_ee,E_ex,r
      REAL :: E_tot,E_net,E_ABn,E_ABo,E_rep,E_XR
      type(OPMATRIX_TYPE), allocatable :: J,K
      type(OPMATRIX_TYPE), allocatable :: saved_MOs_A
      type(OPMATRIX_TYPE), allocatable :: saved_MOs_B
      REAL :: exchange_factor
      type(MOLECULE_TYPE), allocatable :: merged
      exchange_factor = -HALF

      ! If the molecules are too far apart use multipoles
      ! Set to use where the minimum separation is nearly 8 A
      call get_distance_to_(self%atom,mol_B%atom,r)
      if (r>13.6) then  ! 7.2 Angstroms
         res = ZERO
         res(1) = intermolecular_coulomb_atomic_multipole_energy_(self,mol_B)
         return
      end if

      ! Hacky method to reuse code, should be rewritten
      call create_from_mols_(merged,self,mol_B)
      saved_MOs_A = self%MOs
      saved_MOs_B = mol_B%MOs
      self%MOs = self%NOs
      mol_B%MOs = mol_B%NOs
      call set_SCF_kind_(self%SCF_data,genre_(self%MOs))
      call set_SCF_kind_(mol_B%SCF_data,genre_(mol_B%MOs))
      call merge_group_MOs_(merged)
      call set_output_(merged%SCF_data,FALSE)
      if (is_restricted_(merged%SCF_data)) then
         exchange_factor = -QUARTER
      end if

      ! Make SCF density matrices
      call make_SCF_density_mx_(merged)

      ! Make integrals
      call make_core_mx_(merged)

      ! >> Nuclear attraction energy << 
      call uncompress_(merged%nuclear_attraction_mx)
      E_ABn = ZERO; E_ABo = ZERO;
      E_en = ZERO; E_1e = ZERO;
      E_ex = ZERO; E_nn = ZERO;
      E_ke = ZERO;
      E_en = expectation_(merged,merged%nuclear_attraction_mx)+monomer_energies(1)
      call compress_(merged%nuclear_attraction_mx)

      ! Non-orthogonal & orthogonalized electronic energies

      ! >> One electron interaction energy <<
      E_1e = expectation_(merged,merged%core_mx)
      E_ABn = E_ABn+E_1e
      E_1e = E_1e+monomer_energies(2)
      E_ke = E_1e-E_en
      call create_(J,merged%n_bf,genre_(merged%density_mx))
      call create_(K,merged%n_bf,genre_(merged%density_mx))
      call make_JK_engine_(merged,J,K,merged%density_mx)

      ! Alpha density matrix with beta, beta density matrix with alpha
      E_ee = expectation_(merged,J)*HALF
      E_ABn = E_ABn+E_ee
      E_ee = E_ee+monomer_energies(3)
      E_ex = expectation_(merged,K)*exchange_factor
      E_ABn = E_ABn+E_ex
      E_ex = E_ex+monomer_energies(4)

      ! >> Nuclear-nuclear repulsion energy term <<
      E_nn = group_12_NR_int_energy_(merged)
      ! >> Electron repulsion energy term <<

      ! >> Total <<
      E_tot = E_ke+E_nn+E_en+E_ee+E_ex
      E_net = E_nn+E_en+E_ee

      ! ======================
      ! Now symmorthonormalize
      ! ======================
      call symmorthonormalize_occupied_MOs_(merged)
      call make_SCF_density_mx_(merged)

      ! One electron hamiltonian
      E_ABo = expectation_(merged,merged%core_mx)

      ! Two-electron integrals
      call destroy_(J); call destroy_(K)
      call create_(J,merged%n_bf,genre_(merged%density_mx))
      call create_(K,merged%n_bf,genre_(merged%density_mx))
      call make_JK_engine_(merged,J,K,merged%density_mx)
      E_ABo = E_ABo+HALF*expectation_(merged,J,TRUE)
      E_ABo = E_ABo+exchange_factor*expectation_(merged,K)
      ! Get repulsion energy
      E_rep = E_ABo-E_ABn  ! Accounts for orthogonalization
      E_XR = E_ex+E_rep

      ! Clean up remaining temporaries
      res = [E_net,E_XR]
      if (abs(E_XR)<0.00001) then
         call show_(stdout,"Extremely small repulsion (could have avoided calculation) =",E_XR*KJOULEMOL_PER_HARTREE)
         call show_(stdout,"E_ele      =",E_net*KJOULEMOL_PER_HARTREE)
         call show_(stdout,"separation =",r*ANGSTROM_PER_BOHR)
      end if
      call destroy_(J); call destroy_(K)
      call destroy_(merged%MOs)
      call destroy_(merged%NOs)
      call destroy_(merged%density_mx)
      self%MOs = saved_MOs_A
      mol_B%MOs = saved_MOs_B

      ! need to destroy but don't want to destroy atom_group
      call destroy_(merged)
   end function


   ! Why can't this be called via mol1, mol2?
   ! Code bloat ...

   function intermolecular_qm_energies(self,mol_B) result (res)
   ! Calculate the group 1-2 interaction energies.
   ! For simplicity the notations follows the reference:
   ! res = [ E_coul, E_rep ]
   ! P. Su & H. Li (2009) JCP 131 p. 014102
   ! It has references to the original work and is explained fairly
   ! clearly, except for their use of "X" for supermolecule (X=AB).
   ! NOTE: make sure the MO's and density matrices for each group exist!
      type(MOLECULE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), INOUT :: mol_B
      VEC(REAL,2) :: res
! ENSURE(.mol(1).density_mx.allocated, "no density matrix for atom group 1")
! ENSURE(.mol(2).density_mx.allocated, "no density matrix for atom group 2")
! ENSURE(.mol(1).MOs.allocated , "no molecular orbitals for atom group 1")
! ENSURE(.mol(2).MOs.allocated , "no molecular orbitals for atom group 2")

      ! Terms regardless of unrestricted/restricted
      REAL :: E_en,E_ke,E_1e,E_nn,E_ee,E_ex,r
      REAL :: E_tot,E_net,E_ABn,E_ABo,E_rep,E_XR
      type(OPMATRIX_TYPE), allocatable :: P
      type(OPMATRIX_TYPE), allocatable :: J,J_A,J_B
      type(OPMATRIX_TYPE), allocatable :: K,K_A,K_B
      type(OPMATRIX_TYPE), allocatable :: saved_MOs_A,saved_MOs_B
      VEC(REAL,3) :: exchange_factors
      type(MOLECULE_TYPE), allocatable :: merged
   ENSURE(allocated(self%atom_group),"MOLECULE.PROP:intermolecular_qm_energies ... no atom group information")
   ENSURE(size(self%atom_group)==2,"MOLECULE.PROP:intermolecular_qm_energies ... must have only 2 atom groups")
      exchange_factors = [-HALF,-HALF,-HALF]

      ! If the molecules are too far apart use multipoles
      ! Set to use where the minimum separation is approximately 6.35 Angstrom
      call get_distance_to_(self%atom,mol_B%atom,r)
      if (r>13.6) then  ! 7.2 Angstroms
         res = ZERO
         res(1) = intermolecular_coulomb_atomic_multipole_energy_(self,mol_B)
         return
      end if

      ! Hacky method to reuse code, should be rewritten
      call create_from_mols_(merged,self,mol_B)
      call make_overlap_mx_(self)
      call make_NOs_(self)
      call make_overlap_mx_(mol_B)
      call make_NOs_(mol_B)
      saved_MOs_A = self%MOs
      saved_MOs_B = mol_B%MOs
      self%MOs = self%NOs
      mol_B%MOs = mol_B%NOs
      call set_SCF_kind_(self%SCF_data,genre_(self%MOs))
      call set_SCF_kind_(mol_B%SCF_data,genre_(mol_B%MOs))
      call merge_group_MOs_(merged)
      call set_output_(merged%SCF_data,FALSE)
      if (is_restricted_(merged%SCF_data)) then
         exchange_factors = -QUARTER
      end if
      if (genre_(self%MOs)=="r") exchange_factors(2) = -QUARTER
      if (genre_(mol_B%MOs)=="r") exchange_factors(3) = -QUARTER

      ! Make SCF density matrices
      call make_SCF_density_mx_(merged)
      call make_SCF_density_mx_(self)
      call make_SCF_density_mx_(mol_B)

      ! Make integrals
      call make_core_mx_(merged)
      call make_core_mx_(self)
      call make_core_mx_(mol_B)

      ! >> Nuclear attraction energy << 
      call uncompress_(merged%nuclear_attraction_mx)
      call uncompress_(self%nuclear_attraction_mx)
      call uncompress_(mol_B%nuclear_attraction_mx)
      E_ABn = ZERO; E_ABo = ZERO;
      E_en = ZERO; E_1e = ZERO;
      E_ex = ZERO; E_nn = ZERO;
      E_ke = ZERO;
      E_en = group_12_int_energy_(merged,merged%nuclear_attraction_mx,self%nuclear_attraction_mx,mol_B%nuclear_attraction_mx)
      call compress_(merged%nuclear_attraction_mx)
      call compress_(self%nuclear_attraction_mx)
      call compress_(mol_B%nuclear_attraction_mx)

      ! Non-orthogonal & orthogonalized electronic energies

      ! >> One electron interaction energy <<
      E_1e = group_12_int_energy_(merged,P,self%core_mx,mol_B%core_mx,E_AB=E_ABn)
      E_ke = E_1e-E_en
      call create_(J,merged%n_bf,genre_(merged%density_mx))
      call create_(K,merged%n_bf,genre_(merged%density_mx))
      call create_(J_A,self%n_bf,genre_(self%density_mx))
      call create_(K_A,self%n_bf,genre_(self%density_mx))
      call create_(J_B,mol_B%n_bf,genre_(mol_B%density_mx))
      call create_(K_B,mol_B%n_bf,genre_(mol_B%density_mx))

      ! Two-electron matrices
      call make_JK_engine_(merged,J,K,merged%density_mx)
      call make_JK_engine_(self,J_A,K_A,self%density_mx)
      call make_JK_engine_(mol_B,J_B,K_B,mol_B%density_mx)

      ! Alpha density matrix with beta, beta density matrix with alpha
      E_ee = group_12_int_energy_(merged,J,J_A,J_B,HALF,E_ABn,TRUE)
      E_ex = group_12_exchange_int_energy_(merged,K,K_A,K_B,exchange_factors,E_ABn)
      call destroy_(K_A); call destroy_(K_B)
      call destroy_(J_A); call destroy_(J_B)

      ! >> Nuclear-nuclear repulsion energy term <<
      E_nn = group_12_NR_int_energy_(merged)
      ! >> Electron repulsion energy term <<

      ! >> Total <<
      E_tot = E_ke+E_nn+E_en+E_ee+E_ex
      E_net = E_nn+E_en+E_ee

      ! ======================
      ! Now symmorthonormalize
      ! ======================
      call symmorthonormalize_occupied_MOs_(merged)
      call make_SCF_density_mx_(merged)

      ! One electron hamiltonian
      E_ABo = expectation_(merged,merged%core_mx)

      ! Two-electron integrals
      call destroy_(J); call destroy_(K)
      call create_(J,merged%n_bf,genre_(merged%density_mx))
      call create_(K,merged%n_bf,genre_(merged%density_mx))
      call make_JK_engine_(merged,J,K,merged%density_mx)
      E_ABo = E_ABo+HALF*expectation_(merged,J,TRUE)
      E_ABo = E_ABo+exchange_factors(1)*expectation_(merged,K)

      ! Get repulsion energy
      E_rep = E_ABo-E_ABn  ! Accounts for orthogonalization
      E_XR = E_ex+E_rep

      ! Clean up remaining temporaries
      call destroy_(J); call destroy_(K)

      ! In kJ/mol
      res = [E_net,E_XR]
      self%MOs = saved_MOs_A
      mol_B%MOs = saved_MOs_B

      ! Clean
      call destroy_(self%NOs)
      call destroy_(mol_B%NOs)
      call destroy_(merged)
   end function

   subroutine put_group_12_energies(self)
   ! Calculate the classical and QM group 1-2 interaction energies.
   ! NOTE: make sure the density matrices and MOs for each group
   ! exist e.g. by reading gaussian or by "make_group_density_mx"
      type(MOLECULE_TYPE), INOUT :: self
      call put_group_12_QM_energies_(self)
   end subroutine

   subroutine create_from_mols(self,mol_A,mol_B)
   ! Create a molecule by merging "mol_A" and "mol_B"
      type(MOLECULE_TYPE), allocatable, OUT :: self
      type(MOLECULE_TYPE), target, INOUT :: mol_A
      type(MOLECULE_TYPE), target, INOUT :: mol_B
      INT :: i,n_A,n_B
      call create_(self)
      call set_defaults_(self)  ! NOT PURE
      allocate(self%mol(2))

      ! Is this necessary
      call make_SCF_density_mx_(mol_A)
      call make_SCF_density_mx_(mol_B)
      n_A = size(mol_A%atom)
      n_B = size(mol_B%atom)
      self%atom = mol_A%atom
      call append_(self%atom,mol_B%atom)
      call create_(self%atom_group,2)
      call set_atom_(self%atom_group(1),mol_A%atom)
      self%atom_group(1)%atom_index = [(i,i = 1,n_A)]
      self%mol(1) = mol_A
      call set_atom_(self%atom_group(2),mol_B%atom)
      self%atom_group(2)%atom_index = [(i,i = n_A+1,n_A+n_B)]
      self%mol(2) = mol_B
      call join_atom_groups_(self)
      call update_group_crystal_and_ANOs_(self)
   end subroutine

   subroutine put_group_12_multipole_energies(self)
   ! Calculate the interaction (coulomb) energy with multipoles
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,5) :: E
      REAL :: E_tot
      call make_coulomb_atomic_multipole_energy_(self,E,E_tot)
      call flush_(stdout)
      call text_(stdout,"==================================")
      call text_(stdout,"Atom-atom multipole coulomb energy")
      call text_(stdout,"==================================")
      call flush_(stdout)
      call text_(stdout,"Up to and including octupole-charge/quadrupole-dipole interactions")
      call flush_(stdout)
      call show_(stdout,"Charge-charge                         = ",E(1),right=TRUE)
      call show_(stdout,"Charge-dipole                         = ",E(2),right=TRUE)
      call show_(stdout,"Charge-quadrupole/dipole-dipole       = ",E(3),right=TRUE)
      call show_(stdout,"Charge-octapole/quadrupole-dipole     = ",E(4),right=TRUE)
      call show_(stdout,"quadrupole-quadrupole/dipole-octapole = ",E(5),right=TRUE)
      call show_(stdout,"E_coulomb (multipole)                 = ",E_tot,right=TRUE)
      call show_(stdout,"E_coulomb (multipole) (kJ/mol)        = ",to_units_(E_tot,"kJ/mol"),right=TRUE)
      call text_(stdout,"The following are shown for crystalexplorer")
      call text_(stdout,"the only calculated value is the coulomb term")
      call show_(stdout,"Delta E_repulsion (kJ/mol)            = ",ZERO,right=TRUE)
      call show_(stdout,"Delta E_ee-exchange (kJ/mol)          = ",ZERO,right=TRUE)
      call show_(stdout,"Delta E_exch-rep (kJ/mol)             = ",ZERO,right=TRUE)
      call show_(stdout,"Delta E_coul (kJ/mol)                 = ",to_units_(E_tot,"kJ/mol"),right=TRUE)
      call flush_(stdout)
   end subroutine

   subroutine make_coulomb_multipole_energy(self,E,E_tot)
   ! Make the intermolecular coulomb repulsion energy using
   ! multipoles. Assumes there are two atom groups present.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,5), OUT :: E
      REAL, OUT :: E_tot
      REAL :: q_a,q_b
      VEC(REAL,3) :: m_a,m_b
      VEC(REAL,3) :: coma,comb
      MAT(REAL,3,3) :: Qu_a,Qu_b
      MAT3(REAL,3,3,3) :: O_a,O_b
      type(T_TENSOR_TYPE) :: T
      E = ZERO
      m_a = dipole_moment_(self%mol(1))
      m_b = dipole_moment_(self%mol(2))
      Qu_a = quadrupole_moment_mx_(self%mol(1))
      Qu_b = quadrupole_moment_mx_(self%mol(2))
      O_a = octupole_moment_mx_(self%mol(1))
      O_b = octupole_moment_mx_(self%mol(2))
      coma = center_of_mass_(self%mol(1)%atom)
      comb = center_of_mass_(self%mol(2)%atom)
      q_a = self%mol(1)%charge
      q_b = self%mol(2)%charge
      call create_(T,coma,comb)
      E = interaction_energy_(T,q_a,q_b,m_a,m_b,Qu_a,Qu_b,O_a,O_b)
      E_tot = sum(E)
   end subroutine

   subroutine make_coulomb_atomic_multipole_energy(self,E,E_tot)
   ! ???
      type(MOLECULE_TYPE), target, INOUT :: self
      VEC(REAL,5), OUT :: E
      REAL, OUT :: E_tot
      VEC(REAL,:), allocatable :: charges_a
      VEC(REAL,:), allocatable :: charges_b
      MAT(REAL,:,:), allocatable :: dipoles_a,quadrupoles_a,octupoles_a
      MAT(REAL,:,:), allocatable :: dipoles_b,quadrupoles_b,octupoles_b
      REAL :: q_a,q_b
      VEC(REAL,3) :: m_a,m_b
      MAT(REAL,3,3) :: Qu_a,Qu_b
      MAT3(REAL,3,3,3) :: O_a,O_b
      INT :: i,j,n1,n2
      type(T_TENSOR_TYPE) :: T
      n1 = self%mol(1)%n_atom
      n2 = self%mol(2)%n_atom
      call create_(charges_a,n1)
      call create_(dipoles_a,n1,3)
      call create_(quadrupoles_a,n1,6)
      call create_(octupoles_a,n1,10)
      call create_(charges_b,n2)
      call create_(dipoles_b,n2,3)
      call create_(quadrupoles_b,n2,6)
      call create_(octupoles_b,n2,10)
      call make_atomic_multipoles_(self%mol(1),charges_a,dipoles_a,quadrupoles_a,octupoles_a)
      call make_atomic_multipoles_(self%mol(2),charges_b,dipoles_b,quadrupoles_b,octupoles_b)
      E = ZERO
      do i = 1,n1
         q_a = self%mol(1)%atom(i)%atomic_number-charges_a(i)
         m_a = dipoles_a(i,:)
         call expand_quadrupole_from_(Qu_a,quadrupoles_a(i,:))
         call expand_octupole_from_(O_a,octupoles_a(i,:))
         do j = 1,n2
            q_b = self%mol(2)%atom(j)%atomic_number-charges_b(j)
            m_b = dipoles_b(j,:)
            call expand_quadrupole_from_(Qu_b,quadrupoles_b(j,:))
            call expand_octupole_from_(O_b,octupoles_b(j,:))
            call create_(T,self%mol(1)%atom(i)%position,self%mol(2)%atom(j)%position)
            E = E+interaction_energy_(T,q_a,q_b,m_a,m_b,Qu_a,Qu_b,O_a,O_b)
         end do
      end do
      E_tot = sum(E)
      call destroy_(charges_a); call destroy_(charges_b)
      call destroy_(dipoles_a); call destroy_(dipoles_b)
      call destroy_(quadrupoles_a); call destroy_(charges_b)
      call destroy_(octupoles_a); call destroy_(charges_b)
   end subroutine


   ! Is this used? Why?

   function intermolecular_coulomb_atomic_multipole_energy(self,mol_B) result (E_coul)
   ! Like it says? ...
   ! A & B molecules
      type(MOLECULE_TYPE), IN :: self
      REAL :: E_coul
      type(MOLECULE_TYPE), INOUT :: mol_B
      VEC(REAL,5) :: E
      REAL :: q_a,q_b
      VEC(REAL,3) :: m_a,m_b
      MAT(REAL,3,3) :: Qu_a,Qu_b
      MAT3(REAL,3,3,3) :: O_a,O_b
      INT :: i,j
      type(T_TENSOR_TYPE) :: T
   ENSURE(allocated(self%charges),"MOLECULE.PROP:intermolecular_coulomb_atomic_multipole_energy ... no atomic charge information")
   ENSURE(allocated(self%dipoles),"MOLECULE.PROP:intermolecular_coulomb_atomic_multipole_energy ... no atomic dipole information")
   ENSURE(allocated(self%quadrupoles),"MOLECULE.PROP:intermolecular_coulomb_atomic_multipole_energy ... no atomic quadrupole information")
   ENSURE(allocated(self%octupoles),"MOLECULE.PROP:intermolecular_coulomb_atomic_multipole_energy ... no atomic octupole information")
   ENSURE(allocated(mol_B%charges),"MOLECULE.PROP:intermolecular_coulomb_atomic_multipole_energy ... no atomic charge information")
   ENSURE(allocated(mol_B%dipoles),"MOLECULE.PROP:intermolecular_coulomb_atomic_multipole_energy ... no atomic dipole information")
   ENSURE(allocated(mol_B%quadrupoles),"MOLECULE.PROP:intermolecular_coulomb_atomic_multipole_energy ... no atomic quadrupole information")
   ENSURE(allocated(mol_B%octupoles),"MOLECULE.PROP:intermolecular_coulomb_atomic_multipole_energy ... no atomic octupole information")
      E = ZERO
      do i = 1,self%n_atom
         q_a = self%atom(i)%atomic_number-self%charges(i)
         m_a = self%dipoles(i,:)
         call expand_quadrupole_from_(Qu_a,self%quadrupoles(i,:))
         call expand_octupole_from_(O_a,self%octupoles(i,:))
         do j = 1,mol_B%n_atom
            q_b = mol_B%atom(j)%atomic_number-mol_B%charges(j)
            m_b = mol_B%dipoles(j,:)
            call expand_quadrupole_from_(Qu_b,mol_B%quadrupoles(j,:))
            call expand_octupole_from_(O_b,mol_B%octupoles(j,:))
            call create_(T,self%atom(i)%position,mol_B%atom(j)%position)
            E = E+interaction_energy_(T,q_a,q_b,m_a,m_b,Qu_a,Qu_b,O_a,O_b)
         end do
      end do
      E_coul = sum(E)
   end function

   subroutine put_group_12_QM_energies(self)
   ! Calculate the group 1-2 interaction energies.
   ! For simplicity the notations follows the reference:
   ! P. Su & H. Li (2009) JCP 131 p. 014102
   ! It has references to the original work and is explained fairly
   ! clearly, except for their use of "X" for supermolecule (X=AB).
   ! NOTE: make sure the MO's and density matrices for each group exist!
      type(MOLECULE_TYPE), INOUT :: self

      ! Terms regardless of unrestricted/restricted
      REAL :: E_en,E_ke,E_1e,E_nn,E_ee,E_ex,r
      REAL :: E_tot,E_net,E_ABn,E_ABo,E_rep,E_XR
      type(OPMATRIX_TYPE), allocatable :: J,J_A,J_B
      type(OPMATRIX_TYPE), allocatable :: K,K_A,K_B
      type(OPMATRIX_TYPE), allocatable :: saved_MOs_A,saved_MOs_B
      VEC(REAL,3) :: exchange_factors = [-HALF,-HALF,-HALF]
      VEC(REAL,3) :: a_b
   ENSURE(allocated(self%mol),"MOLECULE.PROP:put_group_12_QM_energies ... no molecules")
   ENSURE(size(self%mol)==2,"MOLECULE.PROP:put_group_12_QM_energies ... must have two molecules")
   ENSURE(allocated(self%mol(1)%density_mx),"MOLECULE.PROP:put_group_12_QM_energies ... no density matrix for atom group 1")
   ENSURE(allocated(self%mol(2)%density_mx),"MOLECULE.PROP:put_group_12_QM_energies ... no density matrix for atom group 2")
   ENSURE(allocated(self%mol(1)%MOs),"MOLECULE.PROP:put_group_12_QM_energies ... no molecular orbitals for atom group 1")
   ENSURE(allocated(self%mol(2)%MOs),"MOLECULE.PROP:put_group_12_QM_energies ... no molecular orbitals for atom group 2")

      ! ======================
      ! Get non orthogonal MOs
      ! ======================

      ! A & B molecules
      a_b = center_of_mass_(self%mol(1)%atom)-center_of_mass_(self%mol(2)%atom)
      r = norm_(a_b)
      ! If the molecules are too far apart use multipoles
      if (to_units_(r,"ANGSTROMS")>12.0d0) then
         call put_group_12_multipole_energies_(self)
         return
      end if
      call make_overlap_mx_(self%mol(1))
      call make_NOs_(self%mol(1))
      call make_overlap_mx_(self%mol(2))
      call make_NOs_(self%mol(2))
      saved_MOs_A = self%mol(1)%MOs
      saved_MOs_B = self%mol(2)%MOs
      self%mol(1)%MOs = self%mol(1)%NOs
      self%mol(2)%MOs = self%mol(2)%NOs
   ! .mol(1).SCF_data.set_SCF_kind(.mol(1).MOs.genre)
   ! .mol(2).SCF_data.set_SCF_kind(.mol(2).MOs.genre)
      call merge_group_MOs_(self)
      call set_output_(self%SCF_data,FALSE)
      if (is_restricted_(self%SCF_data)) then
         exchange_factors = -QUARTER
      end if
      if (genre_(self%mol(1)%MOs)=="r") exchange_factors(2) = -QUARTER
      if (genre_(self%mol(2)%MOs)=="r") exchange_factors(3) = -QUARTER

      ! Make SCF density matrices
      call make_SCF_density_mx_(self)
      call make_SCF_density_mx_(self%mol(1))
      call make_SCF_density_mx_(self%mol(2))

      ! Make integrals
      call make_core_mx_(self)
      call make_core_mx_(self%mol(1))
      call make_core_mx_(self%mol(2))

      ! >> Nuclear attraction energy << 
      call uncompress_(self%nuclear_attraction_mx)
      call uncompress_(self%mol(1)%nuclear_attraction_mx)
      call uncompress_(self%mol(2)%nuclear_attraction_mx)
      E_ABn = ZERO; E_ABo = ZERO;
      E_en = ZERO; E_1e = ZERO;
      E_ex = ZERO; E_nn = ZERO;
      E_ke = ZERO;
      E_en = group_12_int_energy_(self,self%nuclear_attraction_mx,self%mol(1)%nuclear_attraction_mx,self%mol(2)%nuclear_attraction_mx)
      call compress_(self%nuclear_attraction_mx)
      call compress_(self%mol(1)%nuclear_attraction_mx)
      call compress_(self%mol(2)%nuclear_attraction_mx)

      ! Non-orthogonal & orthogonalized electronic energies

      ! >> One electron interaction energy <<
      E_1e = group_12_int_energy_(self,self%core_mx,self%mol(1)%core_mx,self%mol(2)%core_mx,E_AB=E_ABn)
      E_ke = E_1e-E_en
      call create_(J,self%n_bf,genre_(self%density_mx))
      call create_(K,self%n_bf,genre_(self%density_mx))
      call create_(J_A,self%mol(1)%n_bf,genre_(self%mol(1)%density_mx))
      call create_(K_A,self%mol(1)%n_bf,genre_(self%mol(1)%density_mx))
      call create_(J_B,self%mol(2)%n_bf,genre_(self%mol(2)%density_mx))
      call create_(K_B,self%mol(2)%n_bf,genre_(self%mol(2)%density_mx))
      call make_JK_engine_(self,J,K,self%density_mx)
      call make_JK_engine_(self%mol(1),J_A,K_A,self%mol(1)%density_mx)
      call make_JK_engine_(self%mol(2),J_B,K_B,self%mol(2)%density_mx)

      ! Alpha density matrix with beta, beta density matrix with alpha
      E_ee = group_12_int_energy_(self,J,J_A,J_B,HALF,E_ABn,TRUE)
      E_ex = group_12_exchange_int_energy_(self,K,K_A,K_B,exchange_factors,E_ABn)
      call destroy_(K_A); call destroy_(K_B)
      call destroy_(J_A); call destroy_(J_B)

      ! >> Nuclear-nuclear repulsion energy term <<
      E_nn = group_12_NR_int_energy_(self)
      ! >> Electron repulsion energy term <<

      ! >> Total <<
      E_tot = E_ke+E_nn+E_en+E_ee+E_ex
      E_net = E_nn+E_en+E_ee

      ! ======================
      ! Now symmorthonormalize
      ! ======================
      call symmorthonormalize_occupied_MOs_(self)
      call make_SCF_density_mx_(self)

      ! One electron hamiltonian
      E_ABo = expectation_(self,self%core_mx)

      ! Two-electron integrals
      call destroy_(J); call destroy_(K)
      call create_(J,self%n_bf,genre_(self%density_mx))
      call create_(K,self%n_bf,genre_(self%density_mx))
      call make_JK_engine_(self,J,K,self%density_mx)
      E_ABo = E_ABo+HALF*expectation_(self,J,TRUE)
      E_ABo = E_ABo+exchange_factors(1)*expectation_(self,K)

      ! Get repulsion energy
      E_rep = E_ABo-E_ABn  ! Accounts for orthogonalization
      E_XR = E_ex+E_rep

      ! Clean up remaining temporaries
      call destroy_(J); call destroy_(K)
      call flush_(stdout)
      call text_(stdout,"=======================================")
      call text_(stdout,"Quantum mechanical interaction energies")
      call text_(stdout,"=======================================")
      call flush_(stdout)
      call text_(stdout,". E_pro-supermol(non-orthog) = E(Psi) where Psi = Psi_1 Psi_2")
      call text_(stdout,". is the non-orthogonal pro-supermolecule product wavefunction")
      call flush_(stdout)
      call text_(stdout,". E_pro-supermol(orthog) = E(Psi) where Psi = A(Psi_1 Psi_2)")
      call text_(stdout,"  is the antisymmetrized normalized pro-supermolecule wavefunction")
      call flush_(stdout)
      call text_(stdout,". Delta E_X = E_X(Psi) - E_X(Psi_1) - E_X(Psi_2) where the")
      call text_(stdout,"  terms on the right are calculated as  expectation values of")
      call text_(stdout,"  of operators X which are the subterms of the Hamiltonian")
      call flush_(stdout)
      call text_(stdout,". This dcomposition was first defined by Kitaura & Morokuma.")
      call text_(stdout,"  The code follows: P. Su and H. Li (2009) JCP 131 p. 014102.")
      call text_(stdout,"  See there for references, or Stone's 'Intermolecular Forces'.")
      call flush_(stdout)
      call text_(stdout,". NOTE: E_pro-supermol(orthog) is the only QM valid number since")
      call text_(stdout,"  it arises from an antisymmetric wavefunction; however the non-")
      call text_(stdout,"  orthogonal promolecule does produce good electronic densities")
      call text_(stdout,"  for use in X-ray crystallography, so the sub terms which come")
      call text_(stdout,"  from it, produce here, may also be reasonable.")
      call flush_(stdout)
      call text_(stdout,". NOTE: The energies here always apply to determinant orbital")
      call text_(stdout,"  wavefunctions NOT correlated density matrices. However for")
      call text_(stdout,"  correlated wavefunctions the highest occupied natural orbitals")
      call text_(stdout,"  (HONOs) are used instead of the molecular orbitals (MOs).")
      call flush_(stdout)
      call show_(stdout,"Separation (Å)                        = ",to_units_(r,"ANGSTROMS"),right=TRUE)
      call show_(stdout,"1. Delta E_kinetic-energy             = ",E_ke,right=TRUE)
      call show_(stdout,"2. Delta E_nn-repulsion               = ",E_nn,right=TRUE)
      call show_(stdout,"3. Delta E_en-attraction              = ",E_en,right=TRUE)
      call show_(stdout,"4. Delta E_ee-repulsion               = ",E_ee,right=TRUE)
      call show_(stdout,"5. Delta E_ee-exchange                = ",E_ex,right=TRUE)
      call flush_(stdout)
      call show_(stdout,"6. E_pro-supermol(non-orthog)         = ",E_ABn,right=TRUE)
      call show_(stdout,"7. E_pro-supermol(orthog)             = ",E_ABo,right=TRUE)
      call flush_(stdout)
      call show_(stdout,"Delta E         (1+2+3+4+5)           = ",E_tot,right=TRUE)
      call show_(stdout,"Delta E_coulomb   (2+3+4)             = ",E_net,right=TRUE)
      call show_(stdout,"Delta E_repulsion        (-6+7)       = ",E_rep,right=TRUE)
      call show_(stdout,"Delta E_exch-repulsion  (5-6+7)       = ",E_XR,right=TRUE)

      ! In kJ/mol
      E_net = to_units_(E_net,"kJ/mol")
      E_ex = to_units_(E_ex,"kJ/mol")
      E_rep = to_units_(E_rep,"kJ/mol")
      E_XR = to_units_(E_XR,"kJ/mol")
      call flush_(stdout)
      call show_(stdout,"Delta E_coul (kJ/mol)                 = ",E_net,right=TRUE)
      call show_(stdout,"Delta E_ee-exchange (kJ/mol)          = ",E_ex,right=TRUE)
      call show_(stdout,"Delta E_repulsion (kJ/mol)            = ",E_rep,right=TRUE)
      call show_(stdout,"Delta E_exch-rep (kJ/mol)             = ",E_XR,right=TRUE)
      call destroy_(self%mol(1)%NOs)
      call destroy_(self%mol(2)%NOs)
      self%mol(1)%MOs = saved_MOs_A
      self%mol(2)%MOs = saved_MOs_B
   end subroutine


!   put_group_12_QM_energies :: leaky
!   ! Calculate the group 1-2 interaction energies.
!   ! For simplicity the notations follows the reference:
!   ! P. Su & H. Li (2009) JCP 131 p. 014102
!   ! It has references to the original work and is explained fairly
!   ! clearly, except for their use of "X" for supermolecule (X=AB).
!   ! NOTE: make sure the MO's and density matrices for each group exist!
!      self :: INOUT
!
!   ENSURE(.atom_group.allocated,"no atom group information")
!   ENSURE(.atom_group.dim==2 ,"must have only 2 atom groups")
!   ENSURE(.mol(1).density_mx.allocated, "no density matrix for atom group 1")
!   ENSURE(.mol(2).density_mx.allocated, "no density matrix for atom group 2")
!   ENSURE(.mol(1).MOs.allocated , "no molecular orbitals for atom group 1")
!   ENSURE(.mol(2).MOs.allocated , "no molecular orbitals for atom group 2")
!
!      ! Terms regardless of unrestricted/restricted
!      E_en,E_ke,E_1e,E_nn,E_ee,E_ex,r :: REAL
!      E_tot,E_net,E_ABn,E_ABo,E_rep,E_XR :: REAL
!      J,J_A,J_B :: OPMATRIX@
!      K,K_A,K_B :: OPMATRIX@
!      saved_MOs_A, saved_MOs_B :: OPMATRIX@
!      exchange_factors :: VEC{REAL}(3) = [-HALF, -HALF, -HALF]
!      a_b :: VEC{REAL}(3)
!
!
!      ! ======================
!      ! Get non orthogonal MOs
!      ! ======================
!
!      a_b = .mol(1).atom.center_of_mass - .mol(2).atom.center_of_mass
!      r = a_b.norm
!      ! If the molecules are too far apart use multipoles
!      if (r.to_units("ANGSTROMS") > 12.0d0) then
!         .put_group_12_multipole_energies
!         return
!      end
!
!      .mol(1).make_overlap_mx
!      .mol(1).make_NOs
!      .mol(2).make_overlap_mx
!      .mol(2).make_NOs
!
!      saved_MOs_A = .mol(1).MOs
!      saved_MOs_B = .mol(2).MOs
!
!      .mol(1).MOs = .mol(1).NOs
!      .mol(2).MOs = .mol(2).NOs
!    ! .mol(1).SCF_data.set_SCF_kind(.mol(1).MOs.genre)
!    ! .mol(2).SCF_data.set_SCF_kind(.mol(2).MOs.genre)
!
!      .merge_group_MOs
!      .SCF_data.set_output(FALSE)
!      if (.SCF_data.is_restricted) then
!         exchange_factors = -QUARTER
!      end if
!
!      if (.mol(1).MOs.genre == "r") exchange_factors(2) = -QUARTER
!      if (.mol(2).MOs.genre == "r") exchange_factors(3) = -QUARTER
!
!      ! Make SCF density matrices
!             .make_SCF_density_mx
!      .mol(1).make_SCF_density_mx
!      .mol(2).make_SCF_density_mx
!
!      ! Make integrals
!             .make_core_mx
!      .mol(1).make_core_mx
!      .mol(2).make_core_mx
!
!      ! >> Nuclear attraction energy << 
!             .nuclear_attraction_mx.uncompress
!      .mol(1).nuclear_attraction_mx.uncompress
!      .mol(2).nuclear_attraction_mx.uncompress
!
!      E_ABn = ZERO; E_ABo = ZERO; 
!      E_en = ZERO; E_1e = ZERO;
!      E_ex = ZERO; E_nn = ZERO;
!      E_ke = ZERO;
!

!                                  , .mol(2).nuclear_attraction_mx)
!
!             .nuclear_attraction_mx.compress
!      .mol(1).nuclear_attraction_mx.compress
!      .mol(2).nuclear_attraction_mx.compress
!
!      ! Non-orthogonal & orthogonalized electronic energies
!
!      ! >> One electron interaction energy <<
!      E_1e = .group_12_int_energy(.core_mx, .mol(1).core_mx, .mol(2).core_mx, E_AB=E_ABn)
!      E_ke = E_1e - E_en
!
!      J.create(.n_bf, .density_mx.genre)
!      K.create(.n_bf, .density_mx.genre)
!      J_A.create(.mol(1).n_bf, .mol(1).density_mx.genre)
!      K_A.create(.mol(1).n_bf, .mol(1).density_mx.genre)
!      J_B.create(.mol(2).n_bf, .mol(2).density_mx.genre)
!      K_B.create(.mol(2).n_bf, .mol(2).density_mx.genre)
!
!             .make_JK_engine(J  ,K  ,.density_mx)
!      .mol(1).make_JK_engine(J_A,K_A,.mol(1).density_mx)
!      .mol(2).make_JK_engine(J_B,K_B,.mol(1).density_mx)
!
!      ! Alpha density matrix with beta, beta density matrix with alpha
!      E_ee = .group_12_int_energy(J, J_A, J_B, HALF, E_ABn, TRUE)
!      E_ex = .group_12_exchange_int_energy(K, K_A, K_B, exchange_factors, E_ABn)
!
!      K_A.destroy; K_B.destroy
!      J_A.destroy; J_B.destroy  
!
!      ! >> Nuclear-nuclear repulsion energy term <<
!      E_nn = .group_12_NR_int_energy
!      ! >> Electron repulsion energy term <<
!
!      ! >> Total <<
!      E_tot = E_ke + E_nn + E_en + E_ee + E_ex
!      E_net =        E_nn + E_en + E_ee
!
!      ! ======================
!      ! Now symmorthonormalize
!      ! ======================
!
!      .symmorthonormalize_occupied_MOs
!      .make_SCF_density_mx
!
!      ! One electron hamiltonian
!      E_ABo = .expectation(.core_mx)
!
!      ! Two-electron integrals
!      J.destroy; K.destroy
!      J.create(.n_bf, .density_mx.genre)
!      K.create(.n_bf, .density_mx.genre)
!
!      .make_JK_engine(J, K, .density_mx)
!
!      E_ABo = E_ABo +    HALF*.expectation(J, TRUE)
!      E_ABo = E_ABo + exchange_factors(1)*.expectation(K)
!
!      ! Get repulsion energy
!      E_rep = E_ABo - E_ABn ! Accounts for orthogonalization
!      E_XR  = E_ex + E_rep
!
!      ! Clean up remaining temporaries
!      J.destroy; K.destroy
!
!      stdout.flush
!      stdout.text("=======================================")
!      stdout.text("Quantum mechanical interaction energies")
!      stdout.text("=======================================")
!      stdout.flush
!      stdout.text(". E_pro-supermol(non-orthog) = E(Psi) where Psi = Psi_1 Psi_2")
!      stdout.text(". is the non-orthogonal pro-supermolecule product wavefunction")
!      stdout.flush
!      stdout.text(". E_pro-supermol(orthog) = E(Psi) where Psi = A(Psi_1 Psi_2)")
!      stdout.text("  is the antisymmetrized normalized pro-supermolecule wavefunction")
!      stdout.flush
!      stdout.text(". Delta E_X = E_X(Psi) - E_X(Psi_1) - E_X(Psi_2) where the")
!      stdout.text("  terms on the right are calculated as  expectation values of")
!      stdout.text("  of operators X which are the subterms of the Hamiltonian")
!      stdout.flush
!      stdout.text(". This dcomposition was first defined by Kitaura & Morokuma.")
!      stdout.text("  The code follows: P. Su and H. Li (2009) JCP 131 p. 014102.")
!      stdout.text("  See there for references, or Stone's 'Intermolecular Forces'.")
!      stdout.flush
!      stdout.text(". NOTE: E_pro-supermol(orthog) is the only QM valid number since")
!      stdout.text("  it arises from an antisymmetric wavefunction; however the non-")
!      stdout.text("  orthogonal promolecule does produce good electronic densities")
!      stdout.text("  for use in X-ray crystallography, so the sub terms which come")
!      stdout.text("  from it, produce here, may also be reasonable.")
!      stdout.flush
!      stdout.text(". NOTE: The energies here always apply to determinant orbital")
!      stdout.text("  wavefunctions NOT correlated density matrices. However for")
!      stdout.text("  correlated wavefunctions the highest occupied natural orbitals")
!      stdout.text("  (HONOs) are used instead of the molecular orbitals (MOs).")
!
!      stdout.flush
!      stdout.show("Separation (Å)                        = ", r.to_units("ANGSTROMS"), right=TRUE)
!      stdout.show("1. Delta E_kinetic-energy             = ",E_ke ,right=TRUE)
!      stdout.show("2. Delta E_nn-repulsion               = ",E_nn ,right=TRUE)
!      stdout.show("3. Delta E_en-attraction              = ",E_en ,right=TRUE)
!      stdout.show("4. Delta E_ee-repulsion               = ",E_ee,right=TRUE)
!      stdout.show("5. Delta E_ee-exchange                = ",E_ex,right=TRUE)
!      stdout.flush
!      stdout.show("6. E_pro-supermol(non-orthog)         = ",E_ABn,right=TRUE)
!      stdout.show("7. E_pro-supermol(orthog)             = ",E_ABo,right=TRUE)
!      stdout.flush
!      stdout.show("Delta E         (1+2+3+4+5)           = ",E_tot,right=TRUE)
!      stdout.show("Delta E_coulomb   (2+3+4)             = ",E_net,right=TRUE)
!      stdout.show("Delta E_repulsion        (-6+7)       = ",E_rep,right=TRUE)
!      stdout.show("Delta E_exch-repulsion  (5-6+7)       = ",E_XR ,right=TRUE)
!
!      ! In kJ/mol
!      E_net = E_net.to_units("kJ/mol")
!      E_ex  = E_ex.to_units("kJ/mol")
!      E_rep = E_rep.to_units("kJ/mol")
!      E_XR  = E_XR.to_units("kJ/mol")
!
!      stdout.flush
!      stdout.show("Delta E_coul (kJ/mol)                 = ",E_net,right=TRUE)
!      stdout.show("Delta E_ee-exchange (kJ/mol)          = ",E_ex,right=TRUE)
!      stdout.show("Delta E_repulsion (kJ/mol)            = ",E_rep,right=TRUE)
!      stdout.show("Delta E_exch-rep (kJ/mol)             = ",E_XR ,right=TRUE)
!
!      .mol(1).NOs.destroy
!      .mol(2).NOs.destroy
!      .mol(1).MOs = saved_MOs_A
!      .mol(2).MOs = saved_MOs_B
!
!   end

   function group_12_int_energy(self,X,X_A,X_B,fac,E_AB,with_cross_terms) result (res)
   ! Return the group 1-2 interaction energy for one electron
   ! operators for the total system "X" and subsystems "XA" and "XB",
   ! in the supermolecule and subsystem bases, respectively.
   ! If present, "E_AB" is incremented with the expectation of "X".
   ! If present, overall factor "fac" is included.
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), IN :: X
      type(OPMATRIX_TYPE), IN :: X_A
      type(OPMATRIX_TYPE), IN :: X_B
      REAL, optional, IN :: fac
      REAL, optional, INOUT :: E_AB
      BIN, optional :: with_cross_terms
      REAL :: res
      REAL :: E,E_A,E_B
   ENSURE(allocated(self%atom_group),"MOLECULE.PROP:group_12_int_energy ... no atom group information")
   ENSURE(size(self%atom_group)==2,"MOLECULE.PROP:group_12_int_energy ... must have only 2 groups")

      ! Energies
      E = expectation_(self,X,with_cross_terms)
      E_A = expectation_(self%mol(1),X_A,with_cross_terms)
      E_B = expectation_(self%mol(2),X_B,with_cross_terms)

      ! Interaction energy
      res = (E-(E_A+E_B))
      if (present(fac)) res = fac*res
      if (present(E_AB)) then
         if (present(fac)) then; E_AB = E_AB+fac*E
         else; E_AB = E_AB+E
         end if
      end if
   end function

   function group_12_exchange_int_energy(self,X,X_A,X_B,factors,E_AB,with_cross_terms) result (res)
   ! Return the group 1-2 interaction energy for one electron
   ! operators for the total system "X" and subsystems "XA" and "XB",
   ! in the supermolecule and subsystem bases, respectively.
   ! If present, "E_AB" is incremented with the expectation of "X".
   ! If present, overall factor "fac" is included.
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), IN :: X
      type(OPMATRIX_TYPE), IN :: X_A
      type(OPMATRIX_TYPE), IN :: X_B
      VEC(REAL,3), optional, IN :: factors
      REAL, optional, INOUT :: E_AB
      BIN, optional :: with_cross_terms
      REAL :: res
      REAL :: E,E_A,E_B
      VEC(REAL,3) :: fac = [1.0,1.0,1.0]
   ENSURE(allocated(self%atom_group),"MOLECULE.PROP:group_12_exchange_int_energy ... no atom group information")
   ENSURE(size(self%atom_group)==2,"MOLECULE.PROP:group_12_exchange_int_energy ... must have only 2 groups")
      if (present(factors)) fac = factors

      ! Energies
      E = expectation_(self,X,with_cross_terms)
      E_A = expectation_(self%mol(1),X_A,with_cross_terms)
      E_B = expectation_(self%mol(2),X_B,with_cross_terms)

      ! Interaction energy
      res = fac(1)*E-(fac(2)*E_A+fac(3)*E_B)
      if (present(E_AB)) then
         E_AB = E_AB+(E*fac(1))
      end if
   end function

   function group_12_NR_int_energy(self) result (res)
   ! Return the group 1-2 nuclear-repulsion interaction energy.
      type(MOLECULE_TYPE), IN :: self
      REAL :: res
      REAL :: E,E_A,E_B
   ENSURE(allocated(self%atom_group),"MOLECULE.PROP:group_12_NR_int_energy ... no atom group information")
   ENSURE(size(self%atom_group)==2,"MOLECULE.PROP:group_12_NR_int_energy ... must have only 2 groups")
   ENSURE(allocated(self%mol(1)%density_mx),"MOLECULE.PROP:group_12_NR_int_energy ... no mol 1 DM!")
   ENSURE(allocated(self%mol(2)%density_mx),"MOLECULE.PROP:group_12_NR_int_energy ... no mol 2 DM!")

      ! Nuclear-nuclear repulsion energies
      E = nuclear_repulsion_energy_(self%atom)
      E_A = nuclear_repulsion_energy_(self%mol(1)%atom)
      E_B = nuclear_repulsion_energy_(self%mol(2)%atom)

      ! Interaction energy
      res = E-(E_A+E_B)
   end function


!  ==================
!  Atom group setting
!  ==================

   PURE subroutine atom_group_AO_subspace_set(self,P,Q,row_group,col_group)
   ! Set P=Q only for the block whose rows correspond to basis functions
   ! on the atoms in the atom group "row_group" and whose columns are
   ! on the atoms in the atom group "col_group", as given in the
   ! .atom_group array vector.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: P,Q
      INT, IN :: row_group
      INT, IN :: col_group
      INT :: i_a,i_b,a,b,fa,fb,la,lb
   ENSURE(size(P,1)==self%n_bf,"MOLECULE.PROP:atom_group_AO_subspace_set ... wrong size for P")
   ENSURE(size(P,2)==self%n_bf,"MOLECULE.PROP:atom_group_AO_subspace_set ... wrong size for P")
   ENSURE(size(Q,1)==self%n_bf,"MOLECULE.PROP:atom_group_AO_subspace_set ... wrong size for Q")
   ENSURE(size(Q,2)==self%n_bf,"MOLECULE.PROP:atom_group_AO_subspace_set ... wrong size for Q")
   ENSURE(allocated(self%atom_group),"MOLECULE.PROP:atom_group_AO_subspace_set ... no atom group information")
   ENSURE(row_group<=size(self%atom_group),"MOLECULE.PROP:atom_group_AO_subspace_set ... no such row group")
   ENSURE(col_group<=size(self%atom_group),"MOLECULE.PROP:atom_group_AO_subspace_set ... no such column group")
   ENSURE(row_group>0,"MOLECULE.PROP:atom_group_AO_subspace_set ... row group index must be positive")
   ENSURE(col_group>0,"MOLECULE.PROP:atom_group_AO_subspace_set ... column group index must be positive")
      do i_a = 1,size(self%atom_group(row_group)%atom_index)
         do i_b = 1,size(self%atom_group(col_group)%atom_index)
            a = self%atom_group(row_group)%atom_index(i_a)
            b = self%atom_group(col_group)%atom_index(i_b)
            fa = self%first_basis_fn_for_atom(a)
            fb = self%first_basis_fn_for_atom(b)
            la = self%last_basis_fn_for_atom(a)
            lb = self%last_basis_fn_for_atom(b)
            P(fa:la,fb:lb) = Q(fa:la,fb:lb)
         end do
      end do
   end subroutine


!  =====================
!  Roby analysis methods
!  =====================

   subroutine roby_analysis(self)
   ! Do one of the many kind of Roby population analysis
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: allowed_kind
   ENSURE(allocated(self%roby),"MOLECULE.PROP:roby_analysis ... no robydata= supplied")
   ENSURE(allocated(self%density_mx),"MOLECULE.PROP:roby_analysis ... no density")

      ! Ensure restricted density
      call make_r_density_mx_(self)

      ! Check it!
      allowed_kind = genre_(self%density_mx)=="r " OR genre_(self%density_mx)=="u "
      DIE_IF(NOT allowed_kind,"MOLECULE.PROP:roby_analysis ... only restricted or unrestricted densities are allowed")

      ! Set up ANOs/NAOs
      call make_ANOs_and_interpolators_(self)
      call make_NAOs_(self)

      ! If there are atom_groups use them as default groups
   ! if (.roby.atom_group.deallocated) then
   ! if (.atom_group.allocated) then
   ! if (.atom_group.has_atom_indices) then
   !    .roby.atom_group = .atom_group.atom_lists
   ! end
   ! end
   ! end

      ! Set defaults (density_mx, etc.)
      call set_defaults_(self%roby,self)

      ! What to do?
      select case (self%roby%roby_kind)

      case ("atom_bond_analysis       ")
         call atom_bond_analysis_(self%roby)

   ! case("atom_energy_bond_analysis")
   !    DIE_IF(.fock_mx.deallocated,"no fock_mx")
   !    .roby.atom_energy_bond_analysis

   ! case("atom_e1_exact_analysis")
   !    DIE_IF(.fock_mx.deallocated,"no fock_mx")
   !    .roby.atom_E1_exact_analysis

   ! case("atom_e1_krogel_analysis")
   !    DIE_IF(.fock_mx.deallocated,"no fock_mx")
   !    .roby.atom_E1_Krogel_analysis

      case ("atom_hyper_analysis       ")
         call atom_bond_analysis_(self%roby)

      case ("group_bond_analysis      ")
         DIE_IF(NOT allocated(self%roby%atom_group),"MOLECULE.PROP:roby_analysis ... no atom_group specified!")
         call atom_bond_analysis_(self%roby)

      case ("group_hyper_analysis      ")
         DIE_IF(NOT allocated(self%roby%atom_group),"MOLECULE.PROP:roby_analysis ... no atom_group specified!")
         call atom_bond_analysis_(self%roby)

      case default
         allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "atom_bond_analysis       "
         tonto%known_keywords(2) = "atom_hyper_analysis       "
         tonto%known_keywords(3) = "group_bond_analysis      "
         tonto%known_keywords(4) = "group_hyper_analysis      "
         call unknown_(tonto,self%roby%roby_kind,"MOLECULE.PROP:roby_analysis")
         deallocate(tonto%known_keywords)
      end select
   end subroutine


!  =================
!  Testing Functions
!  =================

!   put_KM_atom_multipoles
!   ! Put the KM multipoles
!      self :: INOUT
!
!   ENSURE(.basis_info_made,"no basis info")
!   ENSURE(.atom.allocated,"no atom list")
!   ENSURE(.density_mx.allocated,"no density_mx")
!   ENSURE(.density_mx.r.allocated,"no restricted density_mx")
!   ENSURE(NOT .use_spherical_basis,"not implememnted for spherical")
!
!      charge :: VEC{REAL}@
!      dipole, quadrupole, octupole :: MAT{REAL}@
!      a :: INT
!      dp :: REAL
!
!      charge.create(.n_atom)
!      dipole.create(.n_atom, 3)
!      quadrupole.create(.n_atom, 6)
!      octupole.create(.n_atom, 10)
!      .make_atomic_multipoles(charge,dipole,quadrupole,octupole)
!
!      stdout.flush
!      stdout.text("Koritsanszky-Michael atomic populations: ")
!      stdout.flush
!      stdout.dash(int_fields=1, real_fields=5)
!      stdout.put("Atom", int_width = TRUE)
!      stdout.put("Charge")
!      stdout.put("mu_x")
!      stdout.put("mu_y")
!      stdout.put("mu_z")
!      stdout.put("|mu|")
!      stdout.flush
!      stdout.dash(int_fields=1,real_fields=5)
!      stdout.flush
!      charge(:) = .atom(:).atomic_number - charge(:) ! <<< redefine charge
!      do a = 1,.n_atom
!         stdout.put(.atom(a).label,int_width=TRUE)
!         stdout.put(charge(a))
!         stdout.put(dipole(a,1))
!         stdout.put(dipole(a,2))
!         stdout.put(dipole(a,3))
!         dp = dipole(a,:).norm
!         stdout.put(dp)
!         stdout.flush
!      end
!      stdout.dash(int_fields=1, real_fields=6)
!      stdout.flush
!
!      ! Write results in au
!      stdout.flush
!      stdout.text("Koritsanszky-Michael quadrupole moments (au):")
!      stdout.flush
!      stdout.dash(int_fields=1,real_fields=6)
!      stdout.tab(int_fields=1)
!      stdout.put("Qxx")
!      stdout.put("Qyy")
!      stdout.put("Qzz")
!      stdout.put("Qxy")
!      stdout.put("Qxz")
!      stdout.put("Qyz")
!      stdout.flush
!      stdout.put("Atom",int_width=TRUE)
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.flush
!      stdout.dash(int_fields=1,real_fields=6)
!      do a = 1,.n_atom
!         stdout.put(.atom(a).label,int_width=TRUE)
!         stdout.put(quadrupole(a,1))
!         stdout.put(quadrupole(a,2))
!         stdout.put(quadrupole(a,3))
!         stdout.put(quadrupole(a,4))
!         stdout.put(quadrupole(a,5))
!         stdout.put(quadrupole(a,6))
!         stdout.flush
!      end
!      stdout.dash(int_fields=1,real_fields=6)
!
!      ! Write results in au
!      stdout.flush
!      stdout.text("Koritsanszky-Michael octupole moments (au):")
!      stdout.flush
!      stdout.dash(int_fields=1,real_fields=10)
!      stdout.tab(int_fields=1)
!      stdout.put("Oxxx")
!      stdout.put("Oyyy")
!      stdout.put("Ozzz")
!      stdout.put("Oxxy")
!      stdout.put("Oxxz")
!      stdout.put("Oyyx")
!      stdout.put("Oyyz")
!      stdout.put("Ozzx")
!      stdout.put("Ozzy")
!      stdout.put("Oxyz")
!      stdout.flush
!      stdout.put("Atom",int_width=TRUE)
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.put("/au")
!      stdout.flush
!      stdout.dash(int_fields=1,real_fields=10)
!      do a = 1,.n_atom
!         stdout.put(.atom(a).label,int_width=TRUE)
!         stdout.put(octupole(a,1))
!         stdout.put(octupole(a,2))
!         stdout.put(octupole(a,3))
!         stdout.put(octupole(a,4))
!         stdout.put(octupole(a,5))
!         stdout.put(octupole(a,6))
!         stdout.put(octupole(a,7))
!         stdout.put(octupole(a,8))
!         stdout.put(octupole(a,9))
!         stdout.put(octupole(a,10))
!         stdout.flush
!      end
!      stdout.dash(int_fields=1,real_fields=10)
!
!      quadrupole.destroy
!      charge.destroy
!      dipole.destroy
!
!   end

!  =============
!  Bader methods
!  =============

   subroutine get_Bader_regions(self)
   ! Calculate the electron density and integrate
      type(MOLECULE_TYPE), IN :: self
      INT :: n,nx,ny,nz
      MAT(REAL,:,:), allocatable :: pt
      MAT(REAL,3,3) :: axes
      MAT3(REAL,:,:,:), allocatable :: g3
      MAT3(INT,:,:,:), allocatable :: CP
      VEC(REAL,:), allocatable :: EDS,VOL
      VEC(REAL,:), allocatable :: grid
#ifdef MPI      
      MAT4(INT,:,:,:,:), allocatable :: CP1
   ENSURE(allocated(self%plot_grid),"MOLECULE.PROP:get_Bader_regions ... No plot_grid!")
#endif      
      nx = self%plot_grid%n_x
      ny = self%plot_grid%n_y
      nz = self%plot_grid%n_z
      n = self%plot_grid%n_pt

      ! Make space for grid values and grid points
      call create_(g3,nx,ny,nz)
      call create_(grid,n)
      call create_(pt,n,3)

      ! Make the grid points
      call make_points_(self%plot_grid,pt)

      ! Make the ED grid values
      call make_ED_grid_r_(self,grid,pt)

      ! Clean up
      call destroy_(pt)

      ! Do it
      axes = plot_axes_(self%plot_grid)
      g3 = reshape(grid,[nx,ny,nz])
#ifdef MPI
      call cubes_to_basin_parallel(g3,nx,ny,nz,self%plot_grid%width,axes,CP1,EDS,VOL)
#else
      call cubes_to_basin(g3,nx,ny,nz,self%plot_grid%width,axes,CP,EDS,VOL)
#endif

      ! Clean
      call destroy_(VOL)
      call destroy_(EDS)
      call destroy_(CP)
      call destroy_(g3)
   end subroutine

   subroutine get_Bader_basins(self)
   ! Assign every point of the isosurface plot grid to a Bader basin of
   ! the electron density, report the volume and electron count of each
   ! basin, and triangulate the basin boundaries into .isosurface.
   ! NOTE: this is the ported archive/Bader serial path and is *not*
   ! validated against a reference; see docs/TASK_BADER_BASIN_PORT.md.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: n,nx,ny,nz
      VEC(REAL,:), allocatable :: grid
      MAT3(REAL,:,:,:), allocatable :: g3
      MAT(REAL,:,:), allocatable :: pt
   ENSURE(allocated(self%isosurface),"MOLECULE.PROP:get_Bader_basins ... no isosurface")
      nx = self%isosurface%plot_grid%n_x
      ny = self%isosurface%plot_grid%n_y
      nz = self%isosurface%plot_grid%n_z
      n = self%isosurface%plot_grid%n_pt

      ! Make space for the grid values and grid points
      call create_(g3,nx+2,ny+2,nz+2)
      call create_(grid,n)
      call create_(pt,n,3)

      ! Make the grid points
      call make_points_(self%isosurface%plot_grid,pt)

      ! Make the electron density grid values
      call make_ED_grid_r_(self,grid,pt)

      ! Make the g3 array, which has a zero boundary so the gradient
      ! search never steps off the grid
      g3 = ZERO
      g3(2:nx+1,2:ny+1,2:nz+1) = reshape(grid,[nx,ny,nz])

      ! Clean up
      call destroy_(pt)
      call destroy_(grid)

      ! Do it
      call assign_Bader_basins_(self,g3,pixel_volume_(self%isosurface%plot_grid))
      call destroy_(g3)
   end subroutine

   subroutine assign_Bader_basins(self,ED,dv)
   ! Assign each interior point of the electron density grid "ED" to a
   ! Bader basin by following the steepest ascent to a maximum, using
   ! the on-grid method of Henkelman et al. (2006). "ED" carries a
   ! one-point zero boundary on every face; "dv" is the voxel volume.
   ! Results are reported and triangulated by .put_Bader_basin_info.
   ! NOTE: on the archive/Bader branch this routine was called
   ! cubes_to_basin, replacing the differently-shaped routine of that
   ! name which is still present below.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), IN :: ED
      REAL, IN :: dv
      INT :: n_path,n_basin
      INT :: i,m,b
      INT :: x,y,z
      INT :: nx,ny,nz
      INT :: dx,dy,dz
      INT :: qx,qy,qz
      INT :: mx,my,mz
      INT :: px,py,pz
      MAT(INT,:,:), allocatable :: path
      MAT3(INT,:,:,:), allocatable :: basin
      REAL :: ED_p,ED_q
      REAL :: EG_max,Grad,d
      REAL :: t1,t2,t3

      ! Remove the boundary
      nx = size(ED,1)-2
      ny = size(ED,2)-2
      nz = size(ED,3)-2

      ! Basin index of every grid point, boundary included
      call create_(basin,nx+2,ny+2,nz+2)
      basin = 0
      n_basin = 0
      EG_max = huge(ONE)
      call cpu_time(t1)

      ! Non-boundary grid points
      do z = 2,nz+1
         do y = 2,ny+1
            do x = 2,nx+1

         ! Already assigned a basin?
               if (basin(x,y,z)/=0) cycle

         ! No: start a new path
               call create_(path,3,0)
               n_path = 0

         ! Starting path point
               px = x
               py = y
               pz = z

         ! Follow the maximum gradient
               do

            ! Gradient too small?
                  if (EG_max==ZERO) exit

            ! Landed on a previously found basin?
                  if (basin(px,py,pz)>0) exit

            ! Density at the new path point
                  ED_p = ED(px,py,pz)

            ! Append the new path point to the list
                  n_path = n_path+1
                  call append_column_(path,[px,py,pz])

            ! Loop over the neighbours q of p to find the one giving
            ! the largest gradient
                  m = 0
                  mx = px  ! Max gradient position
                  my = py
                  mz = pz
                  EG_max = ZERO
                  do qz = pz-1,pz+1
                     do qy = py-1,py+1
                        do qx = px-1,px+1
                           m = m+1
                           if (m==14) then

                  ! The central point has d==0
                              if (ZERO>=EG_max) then
                                 mx = qx
                                 my = qy
                                 mz = qz
                                 EG_max = ZERO
                              end if
                           else

                  ! Density at the neighbour q
                              ED_q = ED(qx,qy,qz)

                  ! Point separation
                              dx = qx-px
                              dy = qy-py
                              dz = qz-pz
                              d = dx*dx+dy*dy+dz*dz
                              d = sqrt(d)

                  ! Gradient towards q
                              Grad = (ED_q-ED_p)/d

                  ! A bigger gradient?
                              if (Grad<=EG_max) cycle

                  ! Yes
                              mx = qx
                              my = qy
                              mz = qz
                              EG_max = Grad
                           end if
                        end do
                     end do
                  end do

            ! New path point
                  px = mx
                  py = my
                  pz = mz
               end do

         ! Path ended. Basin index at the last path point?
               b = basin(px,py,pz)

         ! A new basin?
               if (b==0) then
                  n_basin = n_basin+1
                  b = n_basin
               end if

         ! Assign the whole path to basin "b"
               do i = 1,n_path
                  px = path(1,i)
                  py = path(2,i)
                  pz = path(3,i)
                  basin(px,py,pz) = b
               end do
               call destroy_(path)
               EG_max = huge(ONE)
            end do
         end do
      end do
      call cpu_time(t2)
      call show_(stdout,"Elapsed time for gradient search =",t2-t1)

      ! Output
      call put_Bader_basin_info_(self,ED,basin,n_basin,dv)

      ! Timing
      call cpu_time(t3)
      call show_(stdout,"Elapsed time for summation and output =",t3-t2)

      ! Clean
      call destroy_(basin)
   end subroutine

   subroutine put_Bader_basin_info(self,ED,basin,n_basin,dv)
   ! Report the volume and electron count of each Bader basin, then
   ! triangulate the basin boundaries into .isosurface. "ED" and
   ! "basin" both carry a one-point boundary which is ignored.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), IN :: ED
      MAT3(INT,:,:,:), IN :: basin
      INT, IN :: n_basin
      REAL, IN :: dv
      INT :: x,y,z,i,n_unassigned
      INT :: nx,ny,nz
      VEC(REAL,:), allocatable :: EDS,VOL
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
   ENSURE(allocated(self%isosurface),"MOLECULE.PROP:put_Bader_basin_info ... no isosurface")

      ! Limits. Ignore the boundary
      nx = size(ED,1)-2
      ny = size(ED,2)-2
      nz = size(ED,3)-2
      call create_(EDS,n_basin)
      call create_(VOL,n_basin)

      ! Add up the volumes and electron counts. NOTE: this sums one "dv"
      ! per grid *point*, while "dv" is the plot box volume divided by
      ! the number of grid *intervals*, so sum(VOL) exceeds the box
      ! volume by product(n_i/(n_i-1)). Both are printed below so the
      ! discrepancy is visible; see docs/TASK_BADER_BASIN_PORT.md.
      ! A grid point the gradient search never reached has basin index
      ! zero, and must be counted rather than indexed: EDS and VOL start
      ! at one, so a zero index would write outside them.
      EDS = ZERO
      VOL = ZERO
      n_unassigned = 0
      do z = 2,nz+1
         do y = 2,ny+1
            do x = 2,nx+1
               i = basin(x,y,z)
               if (i==0) then
                  n_unassigned = n_unassigned+1
                  cycle
               end if
               EDS(i) = EDS(i)+ED(x,y,z)*dv  ! electrons in this voxel
               VOL(i) = VOL(i)+dv  ! size of the voxel
            end do
         end do
      end do
      call flush_(stdout)
      call text_(stdout,"====================")
      call text_(stdout,"Bader basin analysis")
      call text_(stdout,"====================")
      call flush_(stdout)
      call text_(stdout,"This method uses the on-grid method of Henkelman et al. (2006)")
      call flush_(stdout)
      call show_(stdout,"No. of basins found =",n_basin)
      call show_(stdout,"Voxel volume        =",dv)
      call show_(stdout,"Volume of grid      =",sum(VOL))
      call show_(stdout,"Volume of plot box  =",volume_(self%isosurface%plot_grid))
      call show_(stdout,"No. of electrons    =",sum(EDS))
      call show_(stdout,"Unassigned points   =",n_unassigned)
      call flush_(stdout)
      call text_(stdout,"Data for each Bader basin:")

      ! Make the table
      call create_(table,2)
      call set_heading_(table(1),"Volume")
      call set_subhead_(table(1),"/au")
      call set_values_(table(1),VOL)
      call set_heading_(table(2),"No. of")
      call set_subhead_(table(2),"electrons")
      call set_values_(table(2),EDS)

      ! Body
      call put_(table)

      ! Clean
      call clear_columns_(table)
      call destroy_(table)
      call destroy_(VOL)
      call destroy_(EDS)

      ! Triangulate the basin boundaries
      call prepare_Bader_grid_(self%isosurface)
      call cubify_Bader_(self%isosurface,basin(2:nx+1,2:ny+1,2:nz+1),n_basin)
   end subroutine

   subroutine cubes_to_basin(grid,nx,ny,nz,gridlength,gridvector,CP,EDS,VOL)
   ! A tonto implementation of the cubes to basin code, originally
   ! implemented by Florian from Tobias (spelling).
      INT, IN :: nx,ny,nz
      MAT3(REAL,:,:,:), IN :: grid
      MAT3(INT,:,:,:), allocatable, OUT :: CP
      VEC(REAL,3), IN :: gridlength
      MAT(REAL,3,3), IN :: gridvector
      VEC(REAL,:), allocatable, OUT :: EDS,VOL
      INT :: i,x,y,z,xs,ys,zs,ix,iy,iz,ListMax,iCP
      REAL :: GradMax,distance,MaxVal,dv,t1,t2,t3
      VEC(INT,3) :: Max
      VEC(type(EVEC_INT_TYPE),:), allocatable :: List
      MAT3(REAL,:,:,:), allocatable :: ED
      VEC(REAL,:), allocatable :: Maxima
      call create_(ED,nx+2,ny+2,nz+2)
      call create_(CP,nx+2,ny+2,nz+2)
      call create_(Maxima,1)
      ED = ZERO
      ED(2:nx+1,2:ny+1,2:nz+1) = grid
      CP = ZERO
      iCP = ZERO
      GradMax = 1
      call cpu_time(t1)
      do x = 2,nx+1
         do y = 2,ny+1
            do z = 2,nz+1
               if (CP(x,y,z)==ZERO) then

            ! Only perform if critical direction not known
                  call create_(List,3,0)

            ! Gives the length of the number of points in the
            ! current list
                  ListMax = 0
                  Max(1) = x
                  Max(2) = y
                  Max(3) = z
                  do while (NOT (GradMax==ZERO OR CP(Max(1),Max(2),Max(3))>0))
                     xs = Max(1)
                     ys = Max(2)
                     zs = Max(3)
                     MaxVal = ED(xs,ys,zs)
                     ListMax = ListMax+1

               ! Appends to list if necessary
                     if (size(List(1)%element)<ListMax) then
                        call append_(List(1)%element,xs)
                     else
                        List(1)%element(ListMax) = xs
                     end if
                     if (size(List(2)%element)<ListMax) then
                        call append_(List(2)%element,ys)
                     else
                        List(2)%element(ListMax) = ys
                     end if
                     if (size(List(3)%element)<ListMax) then
                        call append_(List(3)%element,zs)
                     else
                        List(3)%element(ListMax) = zs
                     end if
                     GradMax = 0
                     do ix = xs-1,xs+1
                        do iy = ys-1,ys+1
                           do iz = zs-1,zs+1

                  ! NOTE: Initial version used xs for all distances, if
                  ! errors are occurring consider altering zs and ys.
                              distance = sqrt(((ix-xs)*gridlength(1))**2+((iy-ys)*gridlength(2))**2+((iz-zs)*gridlength(3))**2)
                              if ((ED(ix,iy,iz)-ED(xs,ys,zs))/distance>GradMax) then

                  ! Determines the optimal gradient direction
                                 Max(1) = ix
                                 Max(2) = iy
                                 Max(3) = iz
                                 MaxVal = ED(ix,iy,iz)
                                 GradMax = (ED(ix,iy,iz)-ED(xs,ys,zs))/distance
                              end if
                           end do
                        end do
                     end do
                  end do
                  if (CP(Max(1),Max(2),Max(3))>0) then
                     do i = 1,ListMax
                        CP(List(1)%element(i),List(2)%element(i),List(3)%element(i)) = CP(Max(1),Max(2),Max(3))
                     end do
                  else
                     if (iCP==ZERO) then
                        Maxima(1) = MaxVal
                     else
                        call append_(Maxima,MaxVal)
                     end if
                     iCP = iCP+1
                     do i = 1,ListMax
                        CP(List(1)%element(i),List(2)%element(i),List(3)%element(i)) = iCP
                     end do
                  end if
                  call destroy_(List)
                  GradMax = 1
               end if
            end do
         end do
      end do
      call cpu_time(t2)
      call show_(stdout,"Elapsed time for gradient search =",t2-t1)
      call create_(EDS,iCP)
      call create_(VOL,iCP)
      EDS = ZERO
      VOL = ZERO

      ! Cross product for the volume (or so I am told)
      ! Divide by total number of points! Move to plot grid
      dv = abs(gridvector(1,1)*gridvector(2,2)*gridvector(3,3)-gridvector(3,1)*gridvector(2,2)*gridvector(1,3)+gridvector(1,2)*gridvector(2,3)*gridvector(3,1)-gridvector(3,2)*gridvector(2,3)*gridvector(1,1)+gridvector(1,3)*gridvector(2,1)*gridvector(3,2)-gridvector(3,3)*gridvector(2,1)*gridvector(1,2))

      ! Corrects dv with the gridsize
      dv = dv/(nx-1)/(ny-1)/(nz-1)*gridlength(1)*gridlength(2)*gridlength(3)
      do x = 1,nx+2
         do y = 1,ny+2
            do z = 1,nz+2
               do i = 1,iCP
                  if (i==CP(x,y,z)) then
                     EDS(i) = EDS(i)+ED(x,y,z)*dv  ! electrons in this voxel
                     VOL(i) = VOL(i)+dv  ! size of voxel
                  end if
               end do
            end do
         end do
      end do
      dv = 0
      do i = 1,iCP
         call show_(stdout,"Critical point      =",i)
         call show_(stdout,"Number of electrons =",EDS(i))
         dv = dv+EDS(i)
         call show_(stdout,"Volume of CP =",VOL(i))
      end do
      call show_(stdout,"Total electrons =",dv)
      call destroy_(ED)
      call destroy_(Maxima)
      call cpu_time(t3)
      call show_(stdout,"Elapsed time for summation and output =",t3-t2)
   end subroutine

   subroutine cubes_to_basin_parallel(grid,nx,ny,nz,gridlength,gridvector,CP_master,EDS,VOL)
   ! A parallel tonto implementation of the cubes to basin code, originally
   ! implemented by Florian from Tobias (spelling).
      INT, IN :: nx,ny,nz
      MAT3(REAL,:,:,:), IN :: grid
      VEC(REAL,3), IN :: gridlength
      MAT(REAL,3,3), IN :: gridvector
      MAT4(INT,:,:,:,:), allocatable, OUT :: CP_master
      VEC(REAL,:), allocatable, OUT :: EDS,VOL
      INT :: nprocs,i,ii,px,mx,x,y,z,xs,ys,zs,ix,iy,iz,ListMax,iCP
      REAL :: GradMax,distance,dv,t1,t2,t3,t4
      VEC(INT,3) :: Max,Replace
      MAT(INT,:,:), allocatable :: List
      MAT4(INT,:,:,:,:), allocatable :: CP
      MAT3(REAL,:,:,:), allocatable :: ED
      MAT3(INT,:,:,:), allocatable :: CP_hisend,CP_losend
      MAT3(INT,:,:,:), allocatable :: CP_hirecv,CP_lorecv
      MAT(INT,:,:), allocatable :: CPS
      VEC(REAL,:), allocatable :: Maxima
      BIN :: changes

      ! Establishes the partitions on each processor.
      nprocs = tonto%n_processors

      ! This hand-rolled 1-D slab decomposition CANNOT work at one rank, and an
      ! MPI build run under `mpirun -n 1` reaches exactly that (the serial build
      ! calls cubes_to_basin instead, via the #ifdef MPI at the call site).
      ! At nprocs==1: mx = nx, so the master branch does
      !     ED(2:mx+2,...) = grid(1:mx+1,...)
      ! which reads grid ONE ELEMENT PAST ITS FIRST EXTENT (grid is MAT3{REAL}
      ! with first extent nx); and the halo exchange below unconditionally does
      !     tonto.sendrecv(...,tonto.processor_rank+1)
      ! i.e. sends to rank 1, which does not exist. Refuse rather than corrupt.
      !
      ! Proper fix is MPI_Cart_create/MPI_Cart_shift, which yields MPI_PROC_NULL
      ! for off-the-end neighbours -- making the sendrecv a no-op and the
      ! nprocs==1 case fall out for free. See TASKS_AND_HISTORY.md.
      DIE_IF(nprocs<2,"MOLECULE.PROP:cubes_to_basin_parallel ... QTAIM basin decomposition needs at least 2 MPI ranks; run with -n 2 or more, or use a serial build")
      DIE_IF(nprocs>nx,"MOLECULE.PROP:cubes_to_basin_parallel ... QTAIM basin decomposition needs nprocs <= nx; slabs would have zero width")
      mx = nx-(nprocs-1)*int(nx/nprocs)
      px = int(nx/nprocs)
      if (tonto%processor_rank==tonto%master_rank) then
         call create_(ED,mx+2,ny+2,nz+2)
         call create_(CP,mx+2,ny+2,nz+2,3)
      else if (tonto%processor_rank<nprocs-1) then
         call create_(ED,px+2,ny+2,nz+2)
         call create_(CP,px+2,ny+2,nz+2,3)
      else
         call create_(ED,px+2,ny+2,nz+2)
         call create_(CP,px+2,ny+2,nz+2,3)
      end if

      ! Fills the partitions with the electron density grid.
      ! Boundaries in between processors are filled wiht more ED grid
      ! points, all other boundaries are zeroed.
      if (tonto%processor_rank==tonto%master_rank) then
         ED = ZERO
         ED(2:mx+2,2:ny+1,2:nz+1) = grid(1:mx+1,:,:)
         px = mx
      else if (tonto%processor_rank<nprocs-1) then
         ED = ZERO
         ED(1:px+2,2:ny+1,2:nz+1) = grid(mx+(tonto%processor_rank-1)*px:mx+(tonto%processor_rank)*px+1,:,:)
      else
         ED = ZERO
         ED(1:px+1,2:ny+1,2:nz+1) = grid(mx+(tonto%processor_rank-1)*px:mx+(tonto%processor_rank)*px,:,:)
      end if

      ! Initialises the partition
      CP = ZERO
      GradMax = 1

      ! Used for process timing.
      if (tonto%processor_rank==tonto%master_rank) then
         call cpu_time(t1)
      end if

      ! Enables multiple threads to be set, if that is available on
      ! the hardware
      !call omp_set_num_threads(3)

      ! Cycles through the partition to determine all gradient paths
      ! within this partition.
      do x = 2,px+1
         do y = 2,ny+1
            do z = 2,nz+1

         ! Avoids unnecessary computation in the partition if a
         ! gradient path has already been found.
               if (sum(abs(CP(x,y,z,:)))==ZERO) then

            ! Creates a list for gradient path following. Chnaging the
            ! order of this list will improve the efficiency of this
            ! program.
                  call create_(List,3,0)
            ! Gives the length of the number of points in the
            ! current list
                  ListMax = 0

            ! Initialises the Maximum direction to be the starting
            ! point of the 3x3x3 cube
                  Max(1) = x
                  Max(2) = y
                  Max(3) = z

            ! Follows the gradient path until either it becomes a
            ! previous gradient path (CP > 0) OR a maximum is found
            ! (GradMax = ZERO)
                  do while (NOT (GradMax==ZERO OR sum(abs(CP(Max(1),Max(2),Max(3),:)))>0))

               ! Stores the initial value of this iteration
                     xs = Max(1)
                     ys = Max(2)
                     zs = Max(3)

               ! Gives the list size (Given the access to the dim
               ! function, this actually may be unnecessary)
                     ListMax = ListMax+1

               ! Appends the current point to the list
                     call append_column_(List,[xs,ys,zs])

               ! Leaves the loop, if it is on the x-boundaries, as
               ! this could cause incorrect gradient paths due to a
               ! lack of information
                     if (Max(1)==1) exit
                     if (Max(1)==px+2) exit

               !Re-initialises the gradient
                     GradMax = 0

               ! Loops through all the neighbouring points (3x3x3) to
               ! determine the gradient path
                     do ix = xs-1,xs+1
                        do iy = ys-1,ys+1
                           do iz = zs-1,zs+1

                  ! Calculates the distance in each of the grid
                  ! directions
                              distance = sqrt(((ix-xs)*gridlength(1))**2+((iy-ys)*gridlength(2))**2+((iz-zs)*gridlength(3))**2)

                  ! Separate option at current grid point to avoid
                  ! divide by zero error
                              if (ix==xs AND iy==ys AND iz==zs) then

                     ! Only update if this is the correct direction
                                 if (ZERO>=GradMax) then
                                    Max(1) = ix
                                    Max(2) = iy
                                    Max(3) = iz
                                    GradMax = ZERO
                                 end if

                  ! Only update if this is a steeper gradient to
                  ! follow.
                              else if ((ED(ix,iy,iz)-ED(xs,ys,zs))/distance>GradMax) then
                                 Max(1) = ix
                                 Max(2) = iy
                                 Max(3) = iz
                                 GradMax = (ED(ix,iy,iz)-ED(xs,ys,zs))/distance
                              end if
                           end do
                        end do
                     end do
                  end do

         ! Checks if this is an old or a new gradient path. This is
         ! for old paths
                  if (sum(abs(CP(Max(1),Max(2),Max(3),:)))/=0) then

            ! Update all points on the path to point to the same
            ! maxima on this path
            !$OMP parallel shared(CP) private(i)
            !$OMP do
                     do i = 1,ListMax
                        CP(List(1,i),List(2,i),List(3,i),:) = CP(Max(1),Max(2),Max(3),:)
                     end do
            !$OMP end do
            !$OMP end parallel
         ! New path
                  else

            ! Update all points on the path to point to the same
            ! maxima on this path
            !$OMP parallel shared(CP) private(i)
            !$OMP do
                     do i = 1,ListMax
                        CP(List(1,i),List(2,i),List(3,i),:) = Max(:)
                     end do
            !$OMP end do
            !$OMP end parallel
                  end if

         ! Resets the list for the next iteration
                  call destroy_(List)

         ! Rests the gradient, so it will enter the do while loop at
         ! the next iteration.
                  GradMax = 1
               end if
            end do
         end do
      end do

      ! Prints how long the gradient path following method takes. The
      ! barrier is use to ensure all processors have completed the
      ! job.
      call barrier_(tonto)
      if (tonto%processor_rank==tonto%master_rank) then
         call cpu_time(t2)
         call show_(stdout,"Elapsed time for gradient search =",t2-t1)
      end if

      ! The partition is not used after this, so it is destroyed to
      ! clear memory
      call destroy_(ED)

      ! Gives all processors information about the size of each
      ! partition, for sending and receiving purposes.
      px = int(nx/nprocs)
      mx = nx-(nprocs-1)*int(nx/nprocs)

      ! Create sending and receiving matrices for each processor.
      if (tonto%processor_rank==tonto%master_rank) then
         call create_(CP_hisend,ny+2,nz+2,3)
         call create_(CP_hirecv,ny+2,nz+2,3)
      else if (tonto%processor_rank<nprocs-1) then

         ! Update the critical points to reflect the index in the
         ! entire grid.
         CP(:,:,:,1) = CP(:,:,:,1)+mx+(tonto%processor_rank-1)*px
         call create_(CP_hisend,ny+2,nz+2,3)
         call create_(CP_hirecv,ny+2,nz+2,3)
         call create_(CP_losend,ny+2,nz+2,3)
         call create_(CP_lorecv,ny+2,nz+2,3)
      else

         ! Update the critical points to reflect the index in the
         ! entire grid.
         CP(:,:,:,1) = CP(:,:,:,1)+mx+(tonto%processor_rank-1)*px
         call create_(CP_losend,ny+2,nz+2,3)
         call create_(CP_lorecv,ny+2,nz+2,3)
      end if

      ! Ensures the program enters the loops 
      changes = TRUE

      ! Loops as long as one partition has changed
      do while (changes EQV TRUE)

         ! Initialises the logical to be false, so it only loops again
         ! if "changes" occur later on in the do loop.
         changes = FALSE

         ! Send and receive boundaries between processors.
         if (tonto%processor_rank==tonto%master_rank) then
            CP_hisend = CP(mx+1,:,:,:)
            call sendrecv_(tonto,CP_hisend,CP_hirecv,tonto%processor_rank+1)
         else if (tonto%processor_rank<nprocs-1) then
            CP_hisend = CP(px+1,:,:,:)
            call sendrecv_(tonto,CP_hisend,CP_hirecv,tonto%processor_rank+1)
            CP_losend = CP(2,:,:,:)
            call sendrecv_(tonto,CP_losend,CP_lorecv,tonto%processor_rank-1)
         else
            CP_losend = CP(2,:,:,:)
            call sendrecv_(tonto,CP_losend,CP_lorecv,tonto%processor_rank-1)
         end if

         ! Master instructions
         if (tonto%processor_rank==tonto%master_rank) then

            ! Only perform on the upper boundary
            x = mx+1
            do y = 2,ny+1
               do z = 2,nz+1

               ! Cycles if the critical point is interior to this
               ! boundary, as no merging is required.
                  if (CP(x,y,z,1)<x+1) cycle

               ! Save what values need to be replaced, as if the
               ! current points maxima is updated, it will not be able
               ! to be used as a reference.
                  Replace(:) = CP(x,y,z,:)

               ! Check if the maxima of the point is different from
               ! the maxima of the maxima
                  if (sum(abs(CP(x,y,z,:)-CP_hirecv(Replace(2),Replace(3),:)))/=0) then

                  ! Saves the value of the maxima to replace old
                  ! values.
                     Max(:) = CP_hirecv(y,z,:)

                  ! Cycle through the grid.
                  !$OMP parallel shared(CP) private(xs,ys,zs)
                  !$OMP do
                     do xs = 2,x
                        do ys = 2,ny+1
                           do zs = 2,nz+1

                     ! Cycles if the critical point is interior to this
                     ! boundary, as no merging is required.
                              if (CP(xs,ys,zs,1)<x+1) cycle

                     ! Updates only if the critical point is the same
                     ! as the one in Replace.
                              if (CP(xs,ys,zs,1)==Replace(1) AND CP(xs,ys,zs,2)==Replace(2) AND CP(xs,ys,zs,3)==Replace(3)) then
                                 CP(xs,ys,zs,:) = Max(:)
                              end if
                           end do
                        end do
                     end do
                  !$OMP end do
                  !$OMP end parallel
                  end if
               end do
            end do

            ! See if any other thread had changes that occured at both
            ! boundaries. NOTE: This could be better updated, so it
            ! better reflects where the changes are specific to. This
            ! would ensure a faster process.
            call parallel_or_(tonto,changes)

         ! Instructions for the central processor merges
         else if (tonto%processor_rank<nprocs-1) then

            ! Only perform on the upper boundary
            x = px+1
            do y = 2,ny+1
               do z = 2,nz+1

               ! Cycles if the critical point is interior to this
               ! boundary, as no merging is required.
                  if (CP(x,y,z,1)<mx+tonto%processor_rank*px+2) cycle

               ! Save what values need to be replaced, as if the
               ! current points maxima is updated, it will not be able
               ! to be used as a reference.
                  Replace(:) = CP(x,y,z,:)

               ! Check if the maxima of the point is different from
               ! the maxima of the maxima
                  if (sum(abs(CP(x,y,z,:)-CP_hirecv(Replace(2),Replace(3),:)))/=0) then

                  ! Saves the value of the maxima to replace old
                  ! values.
                     Max(:) = CP_hirecv(y,z,:)

                  ! Cycle through the grid.
                  !$OMP parallel shared(CP) private(xs,ys,zs)
                  !$OMP do
                     do xs = 2,x
                        do ys = 2,ny+1
                           do zs = 2,nz+1

                     ! Cycles if the critical point is interior to this
                     ! boundary, as no merging is required.
                              if (CP(xs,ys,zs,1)<mx+tonto%processor_rank*px+2) cycle

                     ! Updates only if the critical point is the same
                     ! as the one in Replace.
                              if (CP(xs,ys,zs,1)==Replace(1) AND CP(xs,ys,zs,2)==Replace(2) AND CP(xs,ys,zs,3)==Replace(3)) then
                                 CP(xs,ys,zs,:) = Max(:)
                                 if (xs==2) changes = TRUE
                              end if
                           end do
                        end do
                     end do
                  !$OMP end do
                  !$OMP end parallel
                  end if
               end do
            end do

            ! Only perform on the lower boundary
            x = 2
            do y = 2,ny+1
               do z = 2,nz+1

               ! Cycles if the critical point is interior to this
               ! boundary, as no merging is required.
                  if (CP(x,y,z,1)>mx+(tonto%processor_rank-1)*px+1) cycle

               ! Save what values need to be replaced, as if the
               ! current points maxima is updated, it will not be able
               ! to be used as a reference.
                  Replace(:) = CP(x,y,z,:)

               ! Check if the maxima of the point is different from
               ! the maxima of the maxima
                  if (sum(abs(CP(x,y,z,:)-CP_lorecv(Replace(2),Replace(3),:)))/=0) then

                  ! Saves the value of the maxima to replace old
                  ! values.
                     Max(:) = CP_lorecv(y,z,:)

                  ! Cycle through the grid.
                  !$OMP parallel shared(CP) private(xs,ys,zs)
                  !$OMP do
                     do xs = x,px+1
                        do ys = 2,ny+1
                           do zs = 2,nz+1

                     ! Cycles if the critical point is interior to this
                     ! boundary, as no merging is required.
                              if (CP(xs,ys,zs,1)>mx+(tonto%processor_rank-1)*px+1) cycle

                     ! Updates only if the critical point is the same
                     ! as the one in Replace.
                              if (CP(xs,ys,zs,1)==Replace(1) AND CP(xs,ys,zs,2)==Replace(2) AND CP(xs,ys,zs,3)==Replace(3)) then
                                 CP(xs,ys,zs,:) = Max(:)
                                 if (xs==px+1) changes = TRUE
                              end if
                           end do
                        end do
                     end do
                  !$OMP end do
                  !$OMP end parallel
                  end if
               end do
            end do

            ! See if any other thread had changes that occured at both
            ! boundaries. NOTE: This could be better updated, so it
            ! better reflects where the changes are specific to. This
            ! would ensure a faster process.
            call parallel_or_(tonto,changes)

         ! Instructions for the end processor merges
         else

            ! Only perform on the lower boundary
            x = 2
            do y = 2,ny+1
               do z = 2,nz+1

               ! Cycles if the critical point is interior to this
               ! boundary, as no merging is required.
                  if (CP(x,y,z,1)>mx+(tonto%processor_rank-1)*px+1) cycle

               ! Save what values need to be replaced, as if the
               ! current points maxima is updated, it will not be able
               ! to be used as a reference.
                  Replace(:) = CP(x,y,z,:)

               ! Check if the maxima of the point is different from
               ! the maxima of the maxima
                  if (sum(abs(CP(x,y,z,:)-CP_lorecv(Replace(2),Replace(3),:)))/=0) then

                  ! Saves the value of the maxima to replace old
                  ! values.
                     Max(:) = CP_lorecv(y,z,:)

                  ! Cycle through the grid.
                  !$OMP parallel shared(CP) private(xs,ys,zs)
                  !$OMP do
                     do xs = x,px+1
                        do ys = 2,ny+1
                           do zs = 2,nz+1

                     ! Cycles if the critical point is interior to this
                     ! boundary, as no merging is required.
                              if (CP(xs,ys,zs,1)>mx+(tonto%processor_rank-1)*px+1) cycle

                     ! Updates only if the critical point is the same
                     ! as the one in Replace.
                              if (CP(xs,ys,zs,1)==Replace(1) AND CP(xs,ys,zs,2)==Replace(2) AND CP(xs,ys,zs,3)==Replace(3)) then
                                 CP(xs,ys,zs,:) = Max(:)
                              end if
                           end do
                        end do
                     end do
                  !$OMP end do
                  !$OMP end parallel
                  end if
               end do
            end do

            ! See if any other thread had changes that occured at both
            ! boundaries. NOTE: This could be better updated, so it
            ! better reflects where the changes are specific to. This
            ! would ensure a faster process.
            call parallel_or_(tonto,changes)
         end if
      end do

      ! Send all the completed merges to the master and cleanup
      ! send and recv matrices.
      if (tonto%processor_rank==tonto%master_rank) then

         ! Cleanup
         call destroy_(CP_hisend)
         call destroy_(CP_hirecv)

         ! Creates a grid to merge all the critical points
         call create_(CP_master,size(grid,1),ny,nz,3)

         ! Remove the boundary from the index notation
         CP(:,:,:,:) = CP(:,:,:,:)-1

         ! Initialise the grid
         CP_master(1:mx,:,:,:) = ZERO

         ! Assign the master's partition to the grid
         CP_master(1:mx,:,:,:) = CP(2:mx+1,2:ny+1,2:nz+1,:)

         ! Cleanup the partition
         call destroy_(CP)

         ! Receive the partitions from the other processors
         do i = 1,nprocs-1
            call recv_(tonto,CP_master(mx+(i-1)*px+1:mx+i*px,:,:,:),i,i)
         end do
      else if (tonto%processor_rank<nprocs-1) then

         ! Cleanup
         call destroy_(CP_hisend)
         call destroy_(CP_losend)
         call destroy_(CP_hirecv)
         call destroy_(CP_lorecv)

         ! Remove the boundary from the index notation
         CP(:,:,:,:) = CP(:,:,:,:)-1

         ! Send the partition to the master processor for merging
         call send_(tonto,CP(2:px+1,2:ny+1,2:nz+1,:),tonto%master_rank,tonto%processor_rank)
      else

         ! Cleanup
         call destroy_(CP_lorecv)
         call destroy_(CP_losend)

         ! Remove the boundary from the index notation
         CP(:,:,:,:) = CP(:,:,:,:)-1

         ! Send the partition to the master processor for merging
         call send_(tonto,CP(2:px+1,2:ny+1,2:nz+1,:),tonto%master_rank,tonto%processor_rank)
      end if
      if (tonto%processor_rank==tonto%master_rank) then

         ! Prints how long the communication takes. The
         ! barrier is use to ensure all processors have completed the
         ! job.
         if (tonto%processor_rank==tonto%master_rank) then
            call cpu_time(t3)
            call show_(stdout,"Elapsed time for communication =",t3-t2)
         end if

         ! This is the determinant of the grid vectors
         dv = abs(gridvector(1,1)*gridvector(2,2)*gridvector(3,3)-gridvector(3,1)*gridvector(2,2)*gridvector(1,3)+gridvector(1,2)*gridvector(2,3)*gridvector(3,1)-gridvector(3,2)*gridvector(2,3)*gridvector(1,1)+gridvector(1,3)*gridvector(2,1)*gridvector(3,2)-gridvector(3,3)*gridvector(2,1)*gridvector(1,2))

         ! Scales dv to the correct grid size
         dv = dv/(nx-1)/(ny-1)/(nz-1)*gridlength(1)*gridlength(2)*gridlength(3)

         ! Initialise the output variables

         ! Stores each maxima that exists. The index in this matrix is
         ! used in each subsequent vector
         call create_(CPS,3,0)
         ! Stores the some of the electron density across all voxels
         call create_(EDS,0)
         ! Stores the sum of the volume covered by these voxels
         call create_(VOL,0)
         ! Stores the ED value of each maxima. (not currently output,
         ! but can easily be added
         call create_(Maxima,0)
         ! Illustrates the current number of maxima, and the index of
         ! the current maxima.
         iCP = 0

         ! Cycles through all space to sum the volume and ED sum of
         ! the grid for each CP
         do x = 1,size(CP_master,1)
            do y = 1,size(CP_master,2)
               do z = 1,size(CP_master,3)

            ! This is essentially a logical
                  ii = 0

            ! Checks this maxima against already appended maxima 
                  do i = 1,size(CPS,2)
                     if (CP_master(x,y,z,1)==CPS(1,i) AND CP_master(x,y,z,2)==CPS(2,i) AND CP_master(x,y,z,3)==CPS(3,i)) then
                        ii = 1
                        iCP = i
                        exit
                     end if
                  end do

            ! Append the maxima if it is a new maxima, and add the
            ! volume and electron densities to their respective
            ! matrices
                  if (ii==0) then
                     call append_column_(CPS,CP_master(x,y,z,:))
                     call append_(Maxima,grid(CP_master(x,y,z,1),CP_master(x,y,z,2),CP_master(x,y,z,3)))
                     call append_(EDS,grid(x,y,z)*dv)
                     call append_(VOL,dv)
                  else
                     EDS(iCP) = EDS(iCP)+grid(x,y,z)*dv
                     VOL(iCP) = VOL(iCP)+dv
                  end if
               end do
            end do
         end do

         ! Puts the output into the stdout, note this could be made to
         ! look a lot nicer, and to utilise the tonto system
         ! functions. 
         do i = 1,size(EDS)
            call show_(stdout,"CP =",i)
            call put_(stdout,CPS(:,i))
            call put_(stdout,[EDS(i),VOL(i)])
         end do
      end if

      ! Prints how long the summation procedure takes. The
      ! barrier is use to ensure all processors have completed the
      ! job.
      call barrier_(tonto)
      if (tonto%processor_rank==tonto%master_rank) then
         call cpu_time(t4)
         call show_(stdout,"Elapsed time for summation and output =",t4-t3)
      end if

      ! Idles the other processor
      call barrier_(tonto)

      ! Finalises the MPI program
      ! NOTE: tonto.finalize (MPI_FINALIZE) was called here. That shut MPI down
      ! from inside a science routine, so every later MPI call in the job was
      ! invalid -- it did not fail locally, it poisoned everything downstream.
      ! Finalisation belongs to the program, not to a property calculation.
      ! (Removed 2026-08-02.)
   end subroutine

   subroutine put_submolecule(self,i)
   ! Puts the co_ordinates of the atoms and the bonds within ".atom_group(i)" 
   ! to stdout
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: i
      call put_coordinates_(self%mol(i)%atom)
      call put_bond_length_table_(self%mol(i)%atom)
   end subroutine

end module
