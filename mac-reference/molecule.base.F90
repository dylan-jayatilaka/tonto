!===============================================================================
!
! MOLECULE.BASE
!
! An object representation of a molecule.
!
! These are base methods needed by all other submodules.
!
! The submodule hierarchy is shown below.
! . The numbers (if any) indicate (roughly) the number of used
!   procedures from the module at the head of the column. This is
!   useful if you want to decouple the modules even further.
! . The type of routines in each submodule is more-or-less
!   straightforward to understand from the three- or four-letter
!   pneumonic e.g. REL is for relativistic routines.
!
! Updated 2014-12-01
!
! .BASE
!     3 .REL
!    16---24 .GRID
!    14---12---- 1 .XTAL
!     2--- |---- |---- 4 .CE =
!     7--- |---- 1---- |-- | .INTS
!    14--- |---- 5---- |-- |---- 2 .FOCK
!    30--- 5---- 4---- 5-- |----10---- 5 .SCF
!     2--- |---- |---- |-- |---- 1---- 5--- 1 .GEM =     =     =
!    11--- |---- |---- 1-- |----15---- 5---13--- | .PROP =     =
!     4--- |---- 6---- |-- |---- 3---- |--- 2--- |---- | .PLOT =
!     4--- |---- 5---- |-- |---- 4---- 2--- 1--- |---- |---- | .CP
!    12--- |---- 7---- |-- |---- |---- |--- 1--- |---- |---- 1-- | .TAD
!    15--- |---- 5---- 1-- |---- 5---- 6--- 5--- |---- |---- |-- |--- 2 .MISC
!    76--- 3---- 3----13-- 7---- 1---- |---13--- 3----19---- 6-- 9---11----24 .MAIN
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1996
! Copyright (C) Dylan Jayatilaka 2012-
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
!===============================================================================
module MOLECULE_BASE_MODULE

#  include "macros"

   include "molecule.base.use"

   implicit none

   include "molecule.base.int"


contains

!  ============================
!  Axis change and origin shift
!  ============================

   subroutine move_origin_to_center_of_mass(self)
   ! Move the origin to the center of mass
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.BASE:move_origin_to_center_of_mass ... atom list required")
      call move_origin_to_center_of_mass_(self%atom)
      if (allocated(self%becke_grid)) then
         call set_atom_info_(self%becke_grid,self%atom)  ! NOT PURE
         call set_grid_data_(self%becke_grid)
      end if
   end subroutine

   subroutine move_origin_to_center_of_atoms(self)
   ! Move the origin to the center of atoms
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.BASE:move_origin_to_center_of_atoms ... atom list required")
      call move_origin_to_center_of_atoms_(self%atom)
      if (allocated(self%becke_grid)) then
         call set_atom_info_(self%becke_grid,self%atom)  ! NOT PURE
         call set_grid_data_(self%becke_grid)
      end if
   end subroutine

   subroutine move_origin(self)
   ! Move the origin by a user specified vector
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,3) :: shift
   ENSURE(allocated(self%atom),"MOLECULE.BASE:move_origin ... atom list required")
   ENSURE(stdin%buffer%n_items==4 OR stdin%buffer%n_items==5,"MOLECULE.BASE:move_origin ... origin shift needed on same line")
      call read_quantity_(stdin,shift)
      call translate_by_(self%atom,shift)
      if (allocated(self%becke_grid)) then
         call set_atom_info_(self%becke_grid,self%atom)  ! NOT PURE
         call set_grid_data_(self%becke_grid)
      end if
   end subroutine


!  =================
!  Overlapping atoms
!  =================

   PURE subroutine make_max_S_for_shell_pairs(self,max_S)
   ! Make "max_S", the maximum of the overlap integrals between basis
   ! functions in pairs of shells.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: max_S
      INT :: ab,a,b,fa,fb,la,lb
   ENSURE(allocated(self%overlap_mx),"MOLECULE.BASE:make_max_S_for_shell_pairs ... no overlap_mx")
   ENSURE(size(max_S)==self%n_shell_pairs,"MOLECULE.BASE:make_max_S_for_shell_pairs ... wrong size, S_max")
      do ab = 1,self%n_shell_pairs
         a = (1+int(sqrt(EIGHT*ab-SEVEN)))/2
         b = ab-a*(a-1)/2
         fa = self%first_basis_fn_for_shell(a)
         la = self%last_basis_fn_for_shell(a)
         fb = self%first_basis_fn_for_shell(b)
         lb = self%last_basis_fn_for_shell(b)
         max_S(ab) = maxval(abs(self%overlap_mx(fa:la,fb:lb)))
      end do
   end subroutine


!  ========================
!  Contraction Coefficients
!  ========================

   PURE subroutine make_contraction_mx(self,cc_mat)
   ! Returns the matrix of complete contraction coefficients for each basis set
   ! Size of cc_mat is [.n_prim,.n_bf].
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: cc_mat
      INT :: a,b,p,ap,ab
   ENSURE(size(cc_mat,1)==self%n_prim,"MOLECULE.BASE:make_contraction_mx ... wrong shape, cc_mat")
   ENSURE(size(cc_mat,2)==self%n_bf,"MOLECULE.BASE:make_contraction_mx ... wrong shape, cc_mat")
      b = 1
      p = 1
      cc_mat = ZERO
      do a = 1,self%n_atom
         ap = self%atom(a)%basis%n_prim
         ab = self%atom(a)%basis%n_bf
         call make_contraction_mx_(self%atom(a)%basis,cc_mat(p:p+ap-1,b:b+ab-1))
         p = p+ap
         b = b+ab
      end do
   end subroutine

   PURE subroutine make_normalized_contraction_mx(self,cc_mat)
   ! Returns the matrix of complete contraction coefficients for each
   ! basis set. Size of cc_mat is [.n_prim,.n_bf]. Primitive components
   ! are properly normalized.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: cc_mat
      INT :: a,b,p,ap,ab
   ENSURE(size(cc_mat,1)==self%n_prim,"MOLECULE.BASE:make_normalized_contraction_mx ... wrong shape, cc_mat")
   ENSURE(size(cc_mat,2)==self%n_bf,"MOLECULE.BASE:make_normalized_contraction_mx ... wrong shape, cc_mat")
      call renormalize_(self%basis)
      b = 1
      p = 1
      cc_mat = ZERO
      do a = 1,self%n_atom
         ap = self%atom(a)%basis%n_prim
         ab = self%atom(a)%basis%n_bf
         call make_normalized_contraction_mx_(self%atom(a)%basis,cc_mat(p:p+ap-1,b:b+ab-1))
         p = p+ap
         b = b+ab
      end do
      call unnormalize_(self%basis)
   end subroutine


!  ========================
!  Atom AO subspace setting
!  ========================

   PURE subroutine AO_subspace_set(self,A,B,row_atom,col_atom)
   ! Set "A" equal to the AO subspace blocks of "B" specified by the atom
   ! indices in "row_atom" and "col_atom". If either is missing, then copy
   ! the entire row or col, i.e.  A(small) = B(row_atom,col_atom)
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: A
      MAT(REAL,:,:), IN :: B
      VEC(INT,:), optional, IN :: row_atom
      VEC(INT,:), optional, IN :: col_atom
      INT :: n_row_atoms,n_col_atoms,a1,a2
      INT :: i,b_i,f_i,l_i,n_i,j,b_j,f_j,l_j,n_j
   ENSURE(size(B,1)==self%n_bf,"MOLECULE.BASE:AO_subspace_set ... B has wrong shape")
   ENSURE(size(B,2)==self%n_bf,"MOLECULE.BASE:AO_subspace_set ... B has wrong shape")
      if (present(row_atom) AND present(col_atom)) then
         n_row_atoms = size(row_atom)
         n_col_atoms = size(col_atom)
         a1 = no_of_basis_functions_(self%atom(row_atom))
         a2 = no_of_basis_functions_(self%atom(col_atom))
         ENSURE(size(A,1)==a1,"MOLECULE.BASE:AO_subspace_set ... A has wrong shape")
         ENSURE(size(A,2)==a2,"MOLECULE.BASE:AO_subspace_set ... A has wrong shape")
         b_i = 0
         do i = 1,n_row_atoms
            f_i = self%first_basis_fn_for_atom(row_atom(i))
            l_i = self%last_basis_fn_for_atom(row_atom(i))
            n_i = l_i-f_i+1
            b_j = 0
            do j = 1,n_col_atoms
               f_j = self%first_basis_fn_for_atom(col_atom(j))
               l_j = self%last_basis_fn_for_atom(col_atom(j))
               n_j = l_j-f_j+1
               A(b_i+1:b_i+n_i,b_j+1:b_j+n_j) = B(f_i:l_i,f_j:l_j)
               b_j = b_j+n_j
            end do
            b_i = b_i+n_i
         end do
      else if (present(row_atom)) then
         n_row_atoms = size(row_atom)
         a1 = no_of_basis_functions_(self%atom(row_atom))
         a2 = self%n_bf
         ENSURE(size(A,1)==a1,"MOLECULE.BASE:AO_subspace_set ... A has wrong shape")
         ENSURE(size(A,2)==a2,"MOLECULE.BASE:AO_subspace_set ... A has wrong shape")
         b_i = 0
         do i = 1,n_row_atoms
            f_i = self%first_basis_fn_for_atom(row_atom(i))
            l_i = self%last_basis_fn_for_atom(row_atom(i))
            n_i = l_i-f_i+1
            A(b_i+1:b_i+n_i,:) = B(f_i:l_i,:)
            b_i = b_i+n_i
         end do
      else if (present(col_atom)) then
         n_col_atoms = size(col_atom)
         a1 = self%n_bf
         a2 = no_of_basis_functions_(self%atom(col_atom))
         ENSURE(size(A,1)==a1,"MOLECULE.BASE:AO_subspace_set ... A has wrong shape")
         ENSURE(size(A,2)==a2,"MOLECULE.BASE:AO_subspace_set ... A has wrong shape")
         b_j = 0
         do j = 1,n_col_atoms
            f_j = self%first_basis_fn_for_atom(col_atom(j))
            l_j = self%last_basis_fn_for_atom(col_atom(j))
            n_j = l_j-f_j+1
            A(:,b_j+1:b_j+n_j) = B(:,f_j:l_j)
            b_j = b_j+n_j
         end do
      end if
   end subroutine


!  ======================
!  Density matrix methods
!  ======================

   subroutine make_SCF_density_mx_0(self,damp,n_a,method)
   ! Make the density matrix from the molecular orbitals.
   ! . If "damp" is present use it to damp the updated density matrix.
   !   An .old_density_mx must exist for this to work.
   ! . If "n_a" is present it is used as the number of alpha electrons
   !   in embedded scf calculations.
   ! . If "method" is present it is used as the kind of density matrix
   !   to make, otherwise taken from .SCF_data.SCF_kind, or guessed
   !   from the spinorbital_genre of the MO's.
   ! NOTE:
   ! . If .SCF_data.do_delta_build is set and a a .delta_density_mx
   !   is created, then a delta density is made.
   ! . If .SCF_data.apply_pFON a finite temperature density matrix is
   !   made with temperature .SCF_data.temperature_for_pFON (untested)
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: damp
      INT, optional, IN :: n_a
      STR(len=*), optional, IN :: method
      REAL :: T
      BIN :: damping,do_delta_density
      BIN :: use_pFON,use_FON,ensemble
      STR(len=STR_SIZE) :: scf_method,genre
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_SCF_density_mx_0 ... no basis info")
   ENSURE(allocated(self%MOs),"MOLECULE.BASE:make_SCF_density_mx_0 ... no molecular orbitals")
   ENSURE(has_any_genre_(self%MOs),"MOLECULE.BASE:make_SCF_density_mx_0 ... no molecular orbitals")

      ! Determine (or guess) the kind of SCF method to be used.
      if (present(method)) then; scf_method = method
      else if (allocated(self%SCF_data)) then; scf_method = self%SCF_data%SCF_kind
      else; scf_method = guess_SCF_kind_(self%MOs)
      end if

      ! Determine (or guess) the genre of density matrix to be made
      if (allocated(self%SCF_data)) then; genre = spinorbital_genre_(self%SCF_data)
      else; genre = genre_(self%MOs)
      end if

      ! Make the delta density matrix?
      do_delta_density = allocated(self%delta_density_mx)
      if (allocated(self%SCF_data)) do_delta_density = do_delta_density AND do_delta_build_(self%SCF_data)

      ! Determine if density matrix damping is to be used. Only a caller
      ! that asks for it gets it: the density on entry is then the previous
      ! SCF iterate, which is what damping mixes with. The SCF loops pass
      ! damp=.SCF_data.apply_damping; every other caller gets no damping.
      damping = FALSE
      if (present(damp)) damping = damp
      if (damping) damping = allocated(self%density_mx)
      if (damping) damping = has_genre_(self%density_mx,genre)

      ! Determine if pFON should be used
      use_pFON = FALSE
      if (allocated(self%SCF_data)) then
         if (allocated(self%fock_mx)) then
            if (is_allocated_with_genre_(self%fock_mx,genre)) then
               use_pFON = apply_pFON_(self%SCF_data)
            end if
         end if
      end if

      ! Determine if finite temperature FON should be used
      use_FON = FALSE
      if (allocated(self%SCF_data)) use_FON = self%SCF_data%using_FON

      ! An open-shell ensemble atom: held fractional occupations
      ensemble = FALSE
      if (allocated(self%SCF_data)) ensemble = self%SCF_data%using_open_shell_ensemble

      ! Save the old density, for damping and for the delta density. This
      ! must come BEFORE the density matrix is destroyed and re-created
      ! below: it used to come after, so the "old" density was whatever
      ! the fresh allocation happened to contain -- usually the previous
      ! contents, by luck of the allocator, until the second delta build.
      call destroy_(self%old_density_mx)
      if (do_delta_density OR damping) then
         if (allocated(self%density_mx)) then
            if (has_genre_(self%density_mx,genre)) then
               self%old_density_mx = self%density_mx
               call compress_(self%old_density_mx)
            end if
         end if
      end if

      ! Create the density matrix (leaky)
      if (NOT allocated(self%density_mx)) call create_(self%density_mx,self%n_bf)
      call destroy_(self%density_mx,genre)
      call create_(self%density_mx,genre)

      ! Make the density matrix
      select case (scf_method)

      case ("group")
         ENSURE(is_allocated_with_genre_(self%MOs,"r "),"MOLECULE.BASE:make_SCF_density_mx_0 ... no MO's")
         ENSURE(self%spin_multiplicity==1,"MOLECULE.BASE:make_SCF_density_mx_0 ... this is not a singlet state")
         ENSURE(present(n_a),"MOLECULE.BASE:make_SCF_density_mx_0 ... n_a must be specified")
         call to_scaled_product_of_(self%density_mx%r,self%MOs%r(:,1:self%n_a),self%MOs%r(:,1:self%n_a),fac=TWO,transpose_b=TRUE)

      case ("rhf","rks","xray_rks","xray_rhf","noninteracting-group-rhf")
         ENSURE(is_allocated_with_genre_(self%MOs,"r "),"MOLECULE.BASE:make_SCF_density_mx_0 ... no MO's")
         if (ensemble) then
            call make_ensemble_r_density_mx(self%density_mx%r,self%MOs%r,(self%n_e-self%SCF_data%n_open_electrons)/2,self%SCF_data%n_open_orbitals,self%SCF_data%n_open_electrons/(TWO*self%SCF_data%n_open_orbitals))
         else
            ENSURE(self%spin_multiplicity==1,"MOLECULE.BASE:make_SCF_density_mx_0 ... Not a singlet state: "//trim(chemical_formula_(self%atom))//", mult="//trim(to_str_(self%spin_multiplicity)))
            if (use_pFON) then
               T = temperature_for_pFON_(self%SCF_data)
               call make_finite_T_density_mx(self%density_mx%r,self%MOs%r,self%fock_mx%r,T,self%n_a)
               self%density_mx%r = TWO*self%density_mx%r
            else if (use_FON) then
               T = self%SCF_data%temperature
               call make_finite_T_DM_r(self%density_mx%r,self%SCF_data%E_Fermi,self%MOs%r,self%MO_energies%r,T,self%n_a)
               self%density_mx%r = TWO*self%density_mx%r
            else
               call to_scaled_product_of_(self%density_mx%r,self%MOs%r(:,1:self%n_a),self%MOs%r(:,1:self%n_a),fac=TWO,transpose_b=TRUE)
            end if
         end if
         if (scf_method(1:4)=="xray" AND self%SCF_data%stabilize_density) then
            call stabilize_(self,self%density_mx%r)
         end if

      case ("rohf")
         ENSURE(is_allocated_with_genre_(self%MOs,"r "),"MOLECULE.BASE:make_SCF_density_mx_0 ... no MO's")
         call to_product_of_(self%density_mx%a,self%MOs%a(:,1:self%n_a),self%MOs%a(:,1:self%n_a),transpose_b=TRUE)
         call to_product_of_(self%density_mx%b,self%MOs%b(:,1:self%n_b),self%MOs%b(:,1:self%n_b),transpose_b=TRUE)

      case ("uhf","uks","xray_uhf","xray_uks","pnd_uhf","pnd_uks","xray_pnd_uhf","xray_pnd_uks")
         ENSURE(is_allocated_with_genre_(self%MOs,"u "),"MOLECULE.BASE:make_SCF_density_mx_0 ... no MO's")
         if (use_pFON) then
            T = temperature_for_pFON_(self%SCF_data)
            call make_finite_T_density_mx(self%density_mx%a,self%MOs%a,self%fock_mx%a,T,self%n_a)
            call make_finite_T_density_mx(self%density_mx%b,self%MOs%b,self%fock_mx%b,T,self%n_b)
         else if (use_FON) then
            T = self%SCF_data%temperature
            call make_finite_T_DM_u(self%density_mx%a,self%density_mx%b,self%SCF_data%E_Fermi,self%MOs%a,self%MOs%b,self%MO_energies%a,self%MO_energies%b,T,self%n_a,self%n_b)
         else
            call to_product_of_(self%density_mx%a,self%MOs%a(:,1:self%n_a),self%MOs%a(:,1:self%n_a),transpose_b=TRUE)
            call to_product_of_(self%density_mx%b,self%MOs%b(:,1:self%n_b),self%MOs%b(:,1:self%n_b),transpose_b=TRUE)
         end if

      case ("gchf","xray_gchf")
         ENSURE(is_allocated_with_genre_(self%MOs,"gc"),"MOLECULE.BASE:make_SCF_density_mx_0 ... no MO's")
         call to_product_of_(self%density_mx%gc,self%MOs%gc(:,1:self%n_e),self%MOs%gc(:,1:self%n_e),dagger_b=TRUE)

      case default
         DIE("MOLECULE.BASE:make_SCF_density_mx_0 ... unknown SCF kind, "//trim(scf_method))
      end select

      ! Damp the density matrix if required
      if (damping AND allocated(self%old_density_mx)) then
         call uncompress_(self%old_density_mx)
         call damp_(self%density_mx,self%old_density_mx,self%SCF_data%damp_factor)
      end if

      ! Archive density matrix?
   ! do_archive = FALSE
   ! if (do_archive) .put_archive(.density_mx,"density_mx")

      ! Save the delta density matrix for incremental Fock build. The old
      ! density was saved compressed (its square part destroyed), and
      ! plus() works on square parts, so restore it first; the damping
      ! branch above did this, which is why only the delta path crashed.
      if (do_delta_density AND allocated(self%old_density_mx)) then
         call uncompress_(self%old_density_mx)
         self%delta_density_mx = self%old_density_mx
         call scale_by_(self%delta_density_mx,-ONE)
         call plus_(self%delta_density_mx,self%density_mx)
      else

         ! No delta this iteration: leave nothing stale for the Fock build
         if (allocated(self%delta_density_mx)) call destroy_(self%delta_density_mx,genre)
      end if

      ! The old density is kept (compressed) for the next call, so that
      ! damping can see it. It used to be destroyed here, so "damping"
      ! found no old density and never mixed.
      if (allocated(self%old_density_mx)) call compress_(self%old_density_mx)
   end subroutine

   subroutine make_finite_T_density_mx(D,MO,F,T,n_e)
   ! Make the finite temperature density matrix "D" from molecular
   ! orbitals "MO" according to the pFON method with temperature "T"
   ! and number of electrons "n_e" *for a given spin*, as in Rabuck
   ! and Scuseria, JCP 110, 695 (1999).  There must exist a current
   ! fock_mx "F" made from "D" which is used to get the
   ! eigenvalues for the Fermi-Dirac distribution.
      MAT(REAL,:,:), OUT :: D
      MAT(REAL,:,:), IN :: MO,F
      REAL, IN :: T
      INT, IN :: n_e
      MAT(REAL,:,:), allocatable :: W
      VEC(REAL,:), allocatable :: occ
      VEC(INT,:), allocatable :: ind
      REAL :: beta,e_homo,e_lumo,e_fermi,homo_lumo_gap,fac
      INT :: n_orb,i,j,n_frac
   ENSURE(is_square_(F),"MOLECULE.BASE:make_finite_T_density_mx ... F must be square")
   ENSURE(is_same_shape_as_(F,MO),"MOLECULE.BASE:make_finite_T_density_mx ... incompatible F and MO")
   ENSURE(n_e<size(MO,2),"MOLECULE.BASE:make_finite_T_density_mx ... fewer MO's than electrons")
   ENSURE(T>=ZERO,"MOLECULE.BASE:make_finite_T_density_mx ... T must be positive")
      n_orb = size(F,1)
      call create_(occ,n_orb)
      call create_(ind,n_orb)

      ! Make the occupation numbers
      call create_(W,n_orb,n_orb)
      call to_product_of_(W,F,MO)
      do i = 1,n_orb  ! These are energies
         occ(i) = dot_product(MO(:,i),W(:,i))
      end do
      call quick_sort_(occ,ind)  ! "ind" has the energies sorted
      call destroy_(W)
      beta = KELVIN_PER_HARTREE/T
      e_homo = occ(ind(n_e))
      e_lumo = occ(ind(n_e+1))
      e_fermi = HALF*(e_homo+e_lumo)
      homo_lumo_gap = e_lumo-e_homo
      do j = 1,n_orb  ! These are the occupation numbers
         i = ind(j)
         occ(i) = ONE/(ONE+exp(beta*(occ(i)-e_fermi)))
      end do

      ! The number of fractional orbitals on each side of the Fermi level,
      ! never more than there are orbitals
      if (homo_lumo_gap>0.135e0) then; n_frac = 5
      else if (homo_lumo_gap<0.090e0) then; n_frac = 3
      else; n_frac = 4
      end if
      n_frac = min(n_frac,n_e,n_orb-n_e)

      ! Reset the fractional occupancies
      do j = 1,n_orb
         i = ind(j)
         if (j<=n_e-n_frac) then; occ(i) = ONE
         else if (j>n_e+n_frac) then; occ(i) = ZERO
         end if
      end do

      ! Renormalise the fractional occupations so that the window holds
      ! its n_frac electrons and the total stays n_e (Rabuck & Scuseria)
      fac = ZERO
      do j = n_e-n_frac+1,n_e+n_frac
         fac = fac+occ(ind(j))
      end do
      if (fac>ZERO) then
         fac = n_frac/fac
         do j = n_e-n_frac+1,n_e+n_frac
            occ(ind(j)) = fac*occ(ind(j))
         end do
      end if

      ! Form the temperature broadended density
      D = ZERO
      do j = 1,n_e+n_frac
         i = ind(j)
         call plus_scaled_product_of_(D,MO(:,i:i),MO(:,i:i),fac=occ(i),transpose_b=TRUE)
      end do
      call destroy_(ind)
      call destroy_(occ)
   end subroutine

   PURE subroutine make_ensemble_r_density_mx(D,MO,n_closed,n_open,f)
   ! Make the restricted density "D" of an open-shell ensemble from the
   ! orbitals "MO": the first "n_closed" with occupation 2, the next
   ! "n_open" with occupation 2f.
      MAT(REAL,:,:), OUT :: D
      MAT(REAL,:,:), IN :: MO
      INT, IN :: n_closed,n_open
      REAL, IN :: f
   ENSURE(is_square_(D),"MOLECULE.BASE:make_ensemble_r_density_mx ... D must be square")
   ENSURE(n_closed+n_open<=size(MO,2),"MOLECULE.BASE:make_ensemble_r_density_mx ... fewer MO's than shells")
      D = ZERO
      if (n_closed>0) then
         call plus_scaled_product_of_(D,MO(:,1:n_closed),MO(:,1:n_closed),fac=TWO,transpose_b=TRUE)
      end if
      if (n_open>0) then
         call plus_scaled_product_of_(D,MO(:,n_closed+1:n_closed+n_open),MO(:,n_closed+1:n_closed+n_open),fac=TWO*f,transpose_b=TRUE)
      end if
   end subroutine

   subroutine make_finite_T_DM_r(D,E_Fermi,MO,eig,T,n_e)
   ! Make the restricted finite temperature density matrix "D" from
   ! molecular orbitals "MO" according to the FON method with
   ! temperature "T" in Kelvin and number of electrons "n_e" *for a
   ! given spin*, as in Rabuck and Scuseria, JCP 110, 695 (1999).
      MAT(REAL,:,:), OUT :: D
      REAL, OUT :: E_Fermi
      MAT(REAL,:,:), IN :: MO
      VEC(REAL,:), IN :: eig
      REAL, IN :: T
      INT, IN :: n_e
      INT :: i
      REAL :: occ
   ENSURE(is_square_(D),"MOLECULE.BASE:make_finite_T_DM_r ... D must be square")
   ENSURE(size(MO,2)==size(eig),"MOLECULE.BASE:make_finite_T_DM_r ... MO and eig are incomptabile")
   ENSURE(n_e<=size(MO,2),"MOLECULE.BASE:make_finite_T_DM_r ... fewer MO's than electrons")
   ENSURE(T>ZERO,"MOLECULE.BASE:make_finite_T_DM_r ... T must be positive")

      ! Set temperature and eigenvalues
      call set_T_Fermi_(T)
      call set_E_level_(eig)
      call set_n_electrons_(n_e)

      ! Find the Fermi energy
      call find_root_brent_(Fermi_Dirac_excess_fn,x1=eig(1),x2=eig(n_e+1),root=E_Fermi,tol=TOL(6))
      if (TRUE) then
         call show_(stdout,"HOMO      =",eig(n_e))
         call show_(stdout,"LUMO      =",eig(n_e+1))
         call show_(stdout,"E_Fermi   =",E_Fermi)
      end if

      ! Set the Fermi energy
      call set_E_Fermi_(E_Fermi)

      ! Form the temperature broadended density
      D = ZERO
      do i = 1,size(MO,2)
         occ = Fermi_Dirac_fn_(i)
         call plus_scaled_product_of_(D,MO(:,i:i),MO(:,i:i),fac=occ,transpose_b=TRUE)
      end do
   end subroutine

   subroutine make_finite_T_DM_u(Da,Db,E_Fermi,MOa,MOb,Ea,Eb,T,n_a,n_b)
   ! Make the restricted finite temperature density matrix "D" from
   ! molecular orbitals "MO" according to the FON method with
   ! temperature "T" in Kelvin and number of electrons "n_e" *for a
   ! given spin*, as in Rabuck and Scuseria, JCP 110, 695 (1999).
      MAT(REAL,:,:), OUT :: Da,Db
      REAL, OUT :: E_Fermi
      MAT(REAL,:,:), IN :: MOa,MOb
      VEC(REAL,:), IN :: Ea,Eb
      REAL, IN :: T
      INT, IN :: n_a,n_b
      INT :: i
      REAL :: Emin,Emax,occ
   ENSURE(is_square_(Da),"MOLECULE.BASE:make_finite_T_DM_u ... Da must be square")
   ENSURE(is_square_(Db),"MOLECULE.BASE:make_finite_T_DM_u ... Da must be square")
   ENSURE(size(MOa,2)==size(Ea),"MOLECULE.BASE:make_finite_T_DM_u ... MOa and Ea are incomptabile")
   ENSURE(size(MOb,2)==size(Eb),"MOLECULE.BASE:make_finite_T_DM_u ... MOb and Eb are incomptabile")
   ENSURE(n_b<=size(MOb,2),"MOLECULE.BASE:make_finite_T_DM_u ... fewer MOb's than electrons")
   ENSURE(T>ZERO,"MOLECULE.BASE:make_finite_T_DM_u ... T must be positive")

      ! Set temperature and eigenvalues
      call set_T_Fermi_(T)
      call set_E_level_(Ea,Eb)
      call set_n_electrons_(n_a+n_b)

      ! Find the Fermi energy
      Emin = min(Ea(1),Eb(1))
      Emax = max(Ea(n_a),Eb(n_b))
      call find_root_brent_(Fermi_Dirac_excess_fn,x1=Emin,x2=Emax,root=E_Fermi,tol=TOL(6))
      if (TRUE) then
         call show_(stdout,"HOMOa     =",Ea(n_a))
         call show_(stdout,"HOMOb     =",Eb(n_b))
         call show_(stdout,"LUMOa     =",Ea(n_a+1))
         call show_(stdout,"LUMOb     =",Ea(n_b+1))
         call show_(stdout,"E_Fermi   =",E_Fermi)
      end if

      ! Set the Fermi energy
      call set_E_Fermi_(E_Fermi)

      ! Form the temperature broadended density
      Da = ZERO
      do i = 1,size(MOa,2)
         occ = Fermi_Dirac_fn_(i)
         call plus_scaled_product_of_(Da,MOa(:,i:i),MOa(:,i:i),fac=occ,transpose_b=TRUE)
      end do
      Db = ZERO
      do i = 1,size(MOb,2)
         occ = Fermi_Dirac_fn_(n_a+i)
         call plus_scaled_product_of_(Db,MOb(:,i:i),MOb(:,i:i),fac=occ,transpose_b=TRUE)
      end do
   end subroutine

   PURE subroutine make_r_density_mx_0(self)
   ! Make the AO (spin independent) density matrix any existing density
   ! matrix. The result is placed in the "r " part.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_r_density_mx_0 ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_r_density_mx_0 ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.BASE:make_r_density_mx_0 ... no density matrix")

      ! Leaky
      call make_r_density_mx_(self,self%density_mx)
   end subroutine

   PURE subroutine make_r_density_mx_1(self,D)
   ! Make the AO (spin independent) density matrix from the existing
   ! density matrix. The result is placed in the "r " part of
   ! the density matrix.
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), INOUT :: D
      STR(len=STR_SIZE) :: genre
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_r_density_mx_1 ... no basis info")
   ENSURE(has_any_genre_(D),"MOLECULE.BASE:make_r_density_mx_1 ... no density matrix")
      if (allocated(self%SCF_data)) then; genre = spinorbital_genre_(self%SCF_data)
      else; genre = genre_(D)
      end if
      select case (genre)

      case ("r ")
         ! do nothing

      case ("u ")
         D%r = D%a+D%b

      case ("gc")
         D%r = RE(aa_block_(D%gc))+RE(bb_block_(D%gc))
      end select
   end subroutine

   PURE subroutine make_r_density_mx_2(self,P)
   ! Make the AO (spin independent) density matrix "P" from the
   ! existing density matrix.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: P
      call make_ao_RE_density_mx_(self,P)
   end subroutine

   PURE subroutine make_ao_RE_density_mx(self,P)
   ! Make the REAL AO (spin independent) density matrix "P" from the
   ! existing density matrix.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: P
      STR(len=STR_SIZE) :: genre
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_ao_RE_density_mx ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_ao_RE_density_mx ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.BASE:make_ao_RE_density_mx ... no density matrix")
   ENSURE(size(P,1)==self%n_bf,"MOLECULE.BASE:make_ao_RE_density_mx ... P: wrong dim1")
   ENSURE(size(P,2)==self%n_bf,"MOLECULE.BASE:make_ao_RE_density_mx ... P: wrong dim2")
      if (allocated(self%SCF_data)) then; genre = spinorbital_genre_(self%SCF_data)
      else; genre = genre_(self%density_mx)
      end if
      select case (genre)

      case ("r ")
         P = self%density_mx%r

      case ("u ")
         P = self%density_mx%a+self%density_mx%b

      case ("gc")
         P = RE(aa_block_(self%density_mx%gc))+RE(bb_block_(self%density_mx%gc))
      end select
   end subroutine

   PURE subroutine make_ao_IM_density_mx(self,P)
   ! Make the IMAGINARY AO (spin independent) density matrix "P" from
   ! the existing density matrix.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: P
      STR(len=STR_SIZE) :: genre
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_ao_IM_density_mx ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_ao_IM_density_mx ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.BASE:make_ao_IM_density_mx ... no density matrix")
   ENSURE(size(P,1)==self%n_bf,"MOLECULE.BASE:make_ao_IM_density_mx ... P: wrong dim1")
   ENSURE(size(P,2)==self%n_bf,"MOLECULE.BASE:make_ao_IM_density_mx ... P: wrong dim2")
      if (allocated(self%SCF_data)) then; genre = spinorbital_genre_(self%SCF_data)
      else; genre = genre_(self%density_mx)
      end if
      P = ZERO
      if (genre=="r ") return
      if (genre=="u ") return
      select case (genre)

      case ("gc")
         P = IM(aa_block_(self%density_mx%gc))+IM(bb_block_(self%density_mx%gc))
      end select
   end subroutine

   PURE subroutine make_r_Sz_density_mx_0(self)
   ! Make the AO (spin independent) density matrix from the existing
   ! density matrix. The result is placed in the "r " part of
   ! the density matrix. NOTE: factor 1/2 not included.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_r_Sz_density_mx_0 ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_r_Sz_density_mx_0 ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.BASE:make_r_Sz_density_mx_0 ... no density matrix")
      call make_r_Sz_density_mx_(self,self%density_mx)  ! leaky
   end subroutine

   PURE subroutine make_r_Sz_density_mx_1(self,D)
   ! Make the AO (spin independent) density matrix from the existing
   ! density matrix. The result is placed in the "r " part of
   ! the density matrix. NOTE: factor 1/2 not included!
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), allocatable, OUT :: D
      STR(len=STR_SIZE) :: genre
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_r_Sz_density_mx_1 ... no basis info")
   ENSURE(allocated(D),"MOLECULE.BASE:make_r_Sz_density_mx_1 ... no density matrix")
   ENSURE(has_any_genre_(D),"MOLECULE.BASE:make_r_Sz_density_mx_1 ... no density matrix")
      if (allocated(self%SCF_data)) then; genre = spinorbital_genre_(self%SCF_data)
      else; genre = genre_(D)
      end if
      if (genre=="r ") return
      select case (genre)

      case ("u ")
         call destroy_(D,"r ")
         call create_(D,"r ")
         D%r = D%a-D%b
      end select
   end subroutine

   PURE subroutine make_r_Sz_density_mx_2(self,P)
   ! Make the AO (spin independent) density matrix from the existing
   ! density matrix. The result is placed in the "r " part of
   ! the density matrix. NOTE: factor 1/2 not included!
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: P
      STR(len=STR_SIZE) :: genre
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_r_Sz_density_mx_2 ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_r_Sz_density_mx_2 ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.BASE:make_r_Sz_density_mx_2 ... no density matrix")
   ENSURE(size(P,1)==self%n_bf,"MOLECULE.BASE:make_r_Sz_density_mx_2 ... P: wrong dim1")
   ENSURE(size(P,2)==self%n_bf,"MOLECULE.BASE:make_r_Sz_density_mx_2 ... P: wrong dim2")
      if (allocated(self%SCF_data)) then; genre = spinorbital_genre_(self%SCF_data)
      else; genre = genre_(self%density_mx)
      end if
      select case (genre)

      case ("r ")
         ! return

      case ("u ")
         P = self%density_mx%a-self%density_mx%b

      case ("gc")
         P = RE(aa_block_(self%density_mx%gc))-RE(bb_block_(self%density_mx%gc))
      end select
   end subroutine

   PURE subroutine make_c_S_densities(self,S)
   ! Make  the complex spin density matrices
   ! NOTE: factor 1/2 IS included.
      type(MOLECULE_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: S
      STR(len=STR_SIZE) :: genre
      CPX :: I
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_c_S_densities ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_c_S_densities ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.BASE:make_c_S_densities ... no density matrix")
   ENSURE(size(S,1)==self%n_bf,"MOLECULE.BASE:make_c_S_densities ... S: wrong dim1")
   ENSURE(size(S,2)==self%n_bf,"MOLECULE.BASE:make_c_S_densities ... S: wrong dim2")
   ENSURE(size(S,3)==3,"MOLECULE.BASE:make_c_S_densities ... S: wrong dim3")
      if (allocated(self%SCF_data)) then; genre = spinorbital_genre_(self%SCF_data)
      else; genre = genre_(self%density_mx)
      end if
      S = ZERO
      select case (genre)

      case ("r ")
         ! return

      case ("u ")
         S(:,:,3) = self%density_mx%a-self%density_mx%b

      case ("gc")
         I = IMAGIFY(ONE)
         S(:,:,1) = ab_block_(self%density_mx%gc)+ba_block_(self%density_mx%gc)
         S(:,:,2) = I*ab_block_(self%density_mx%gc)-I*ba_block_(self%density_mx%gc)
         S(:,:,3) = aa_block_(self%density_mx%gc)-bb_block_(self%density_mx%gc)
      end select

      ! Factor 1/2
      S = HALF*S
   end subroutine

   PURE subroutine make_RE_c_S_densities(self,S)
   ! Make the REAL part of the complex spin density matrices.
   ! NOTE: factor 1/2 IS included.
      type(MOLECULE_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: S
      STR(len=STR_SIZE) :: genre
      CPX :: I
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_RE_c_S_densities ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_RE_c_S_densities ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.BASE:make_RE_c_S_densities ... no density matrix")
   ENSURE(size(S,1)==self%n_bf,"MOLECULE.BASE:make_RE_c_S_densities ... S: wrong dim1")
   ENSURE(size(S,2)==self%n_bf,"MOLECULE.BASE:make_RE_c_S_densities ... S: wrong dim2")
   ENSURE(size(S,3)==3,"MOLECULE.BASE:make_RE_c_S_densities ... S: wrong dim3")
      if (allocated(self%SCF_data)) then; genre = spinorbital_genre_(self%SCF_data)
      else; genre = genre_(self%density_mx)
      end if
      S = ZERO
      select case (genre)

      case ("r ")
         ! return

      case ("u ")
         S(:,:,3) = self%density_mx%a-self%density_mx%b

      case ("gc")
         I = IMAGIFY(ONE)
         S(:,:,1) = RE(ab_block_(self%density_mx%gc))+RE(ba_block_(self%density_mx%gc))
         S(:,:,2) = RE(I*ab_block_(self%density_mx%gc))-RE(I*ba_block_(self%density_mx%gc))
         S(:,:,3) = RE(aa_block_(self%density_mx%gc))-RE(bb_block_(self%density_mx%gc))
      end select

      ! Factor 1/2
      S = HALF*S
   end subroutine

   PURE subroutine make_ao_IM_spin_densities(self,S)
   ! Make the IMAGINARY part of the AO spin density matrices.
   ! NOTE: factor 1/2 IS included.
      type(MOLECULE_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: S
      STR(len=STR_SIZE) :: genre
      CPX :: I
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_ao_IM_spin_densities ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_ao_IM_spin_densities ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.BASE:make_ao_IM_spin_densities ... no density matrix")
   ENSURE(size(S,1)==self%n_bf,"MOLECULE.BASE:make_ao_IM_spin_densities ... S: wrong dim1")
   ENSURE(size(S,2)==self%n_bf,"MOLECULE.BASE:make_ao_IM_spin_densities ... S: wrong dim2")
   ENSURE(size(S,3)==3,"MOLECULE.BASE:make_ao_IM_spin_densities ... S: wrong dim3")
      if (allocated(self%SCF_data)) then; genre = spinorbital_genre_(self%SCF_data)
      else; genre = genre_(self%density_mx)
      end if
      S = ZERO
      if (genre=="r ") return
      if (genre=="u ") return
      select case (genre)

      case ("gc")
         I = IMAGIFY(ONE)
         S(:,:,1) = IM(ab_block_(self%density_mx%gc))-IM(ba_block_(self%density_mx%gc))
         S(:,:,2) = IM(I*ab_block_(self%density_mx%gc))-IM(I*ba_block_(self%density_mx%gc))
         S(:,:,3) = IM(aa_block_(self%density_mx%gc))-IM(bb_block_(self%density_mx%gc))
      end select

      ! Factor 1/2
      S = HALF*S
   end subroutine

   subroutine make_SCF_density_mx_1(self,n,nb)
   ! Make the density matrix from orbital "n" of the molecular orbitals.
   ! If present, orbital "nb" of the beta molecular orbitals is used.
   ! If either index is not an occupied MO, the density is set to zero.
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: n
      INT, optional, IN :: nb
      STR(len=STR_SIZE) :: genre
      type(ARCHIVE_TYPE) :: arch
      INT :: m
      BIN :: uhf
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_SCF_density_mx_1 ... no basis info")
   ENSURE(allocated(self%MOs),"MOLECULE.BASE:make_SCF_density_mx_1 ... no molecular orbitals")
      uhf = includes_(self%SCF_data%SCF_kind,"u ")
      WARN_IF(present(nb) AND (NOT uhf),"MOLECULE.BASE:make_SCF_density_mx_1 ... nb ignored")
      m = n
      if (present(nb)) m = nb
      genre = spinorbital_genre_(self%SCF_data)
      call destroy_(self%density_mx,genre)
      call create_(self%density_mx,genre)
      select case (self%SCF_data%SCF_kind)

      case ("rhf","rks","xray_rhf","xray_rks","noninteracting-group-rhf")
         ENSURE(self%spin_multiplicity==1,"MOLECULE.BASE:make_SCF_density_mx_1 ... Not a singlet state: "//trim(chemical_formula_(self%atom))//", mult="//trim(to_str_(self%spin_multiplicity)))
         if (n>0 AND n<=self%n_a) then
            call to_scaled_product_of_(self%density_mx%r,self%MOs%r(:,n:n),self%MOs%r(:,n:n),fac=TWO,transpose_b=TRUE)
         else
            self%density_mx%r = ZERO
         end if

      case ("rohf")
         if (n>0 AND n<=self%n_b) then
            call to_product_of_(self%density_mx%a,self%MOs%a(:,n:n),self%MOs%a(:,n:n),transpose_b=TRUE)
            call to_product_of_(self%density_mx%b,self%MOs%b(:,n:n),self%MOs%b(:,n:n),transpose_b=TRUE)
         else if (n>nb AND n<=self%n_a) then
            self%MOs%a(:,n:n) = self%MOs%r(:,n:n)
            call to_product_of_(self%density_mx%a,self%MOs%a(:,n:n),self%MOs%a(:,n:n),transpose_b=TRUE)
            self%density_mx%a = ZERO
            self%density_mx%b = ZERO
         end if

      case ("uhf","uks","xray_uhf","xray_uks","pnd_uhf","pnd_uks","xray_pnd_uhf","xray_pnd_uks")
         if (n>0 AND n<=self%n_a) then
            call to_product_of_(self%density_mx%a,self%MOs%a(:,n:n),self%MOs%a(:,n:n),transpose_b=TRUE)
         else
            self%density_mx%a = ZERO
         end if
         if (m>0 AND m<=self%n_b) then
            call to_product_of_(self%density_mx%b,self%MOs%b(:,m:m),self%MOs%b(:,m:m),transpose_b=TRUE)
         else
            self%density_mx%b = ZERO
         end if

      case ("gchf","xray_gchf")
         if (n>0 AND n<=self%n_e) then
            call to_product_of_(self%density_mx%gc,self%MOs%gc(:,n:n),self%MOs%gc(:,n:n),dagger_b=TRUE)
         else
            self%density_mx%gc = ZERO
         end if

      case default
         DIE("MOLECULE.BASE:make_SCF_density_mx_1 ... unknown kind, "//trim(self%SCF_data%SCF_kind))
      end select

      ! Archive
      call set_(arch,self%name,"density_mx",genre)
      call write_(arch,self%density_mx)
   end subroutine


!  ==========================
!  Set atoms density matrices
!  ========================== 

   subroutine make_aspherical_atoms(self)
   ! Get the aspherical atom density matrix i.e. just copy the 
   ! atom-atom block from the molecular density matrix.  
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), allocatable :: D
      STR(len=STR_SIZE) :: genre
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.BASE:make_aspherical_atoms ... no atoms")
   ENSURE(self%atom_info_made,"MOLECULE.BASE:make_aspherical_atoms ... no atom info")
   ENSURE(allocated(self%basis),"MOLECULE.BASE:make_aspherical_atoms ... no basis set")
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_aspherical_atoms ... no basis_set info")
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_aspherical_atoms ... no density_mx")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.BASE:make_aspherical_atoms ... no density_mx")
      if (has_all_ANO_matrices_(self)) call destroy_ANO_matrices_(self)

      ! Determine (or guess) the genre of density matrix to be made
      if (allocated(self%SCF_data)) then; genre = spinorbital_genre_(self%SCF_data)
      else; genre = genre_(self%MOs)
      end if

      ! Copy existing density matrix
      D = self%density_mx

      ! Make the restricted density matrix
      call make_r_density_mx_(self,D)

      ! Set athe atom densities
      do a = 1,self%n_atom
         call set_density_mx_for_atom_(self,a,D%r)
      end do
      call destroy_(D)
   end subroutine

   subroutine set_density_mx_for_atom(self,a,D)
   ! (ANO) and density matrix data and interpolator for atom "a" from
   ! a given (real) density matrix "D".
      type(MOLECULE_TYPE), INOUT :: self
      INT :: a
      MAT(REAL,:,:), IN :: D
      INT :: f,l
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:set_density_mx_for_atom ... no density_mx")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.BASE:set_density_mx_for_atom ... no density_mx")

      ! Create a-th atoms density matrix
      call destroy_(self%atom(a)%density_mx)
      call create_(self%atom(a)%density_mx,self%atom(a)%basis%n_bf)
      call create_(self%atom(a)%density_mx,genre="r ")

      ! Copy the a-th atom-atom block
      f = self%first_basis_fn_for_atom(a)
      l = self%last_basis_fn_for_atom(a)
      self%atom(a)%density_mx%r = D(f:l,f:l)
      call set_atom_data_made_(self%atom(a),TRUE)
   end subroutine


!  Spherically average atoms in existing density

   subroutine make_spherical_atoms(self)
   ! Get the spherical atom natural orbitals (ANO) data for all
   ! atoms in the molecule.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), allocatable :: D
      STR(len=STR_SIZE) :: genre
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.BASE:make_spherical_atoms ... no atoms")
   ENSURE(self%atom_info_made,"MOLECULE.BASE:make_spherical_atoms ... no atom info")
   ENSURE(allocated(self%basis),"MOLECULE.BASE:make_spherical_atoms ... no basis set")
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_spherical_atoms ... no basis_set info")
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_spherical_atoms ... no density_mx")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.BASE:make_spherical_atoms ... no density_mx")
      if (has_all_atom_data_(self)) return
      if (has_all_ANO_matrices_(self)) call destroy_ANO_matrices_(self)

      ! Determine (or guess) the genre of density matrix to be made
      if (allocated(self%SCF_data)) then; genre = spinorbital_genre_(self%SCF_data)
      else; genre = genre_(self%MOs)
      end if

      ! Copy existing density matrix
      D = self%density_mx

      ! Make the restricted density matrix
      call make_r_density_mx_(self,D)

      ! Spherically average the molecule AO density
      do a = 1,self%n_atom
         call spherically_average_atom_(self,a,D%r)
      end do
      call destroy_(D)
      call set_atom_data_made_(self%atom,TRUE)
   end subroutine

   subroutine spherically_average_atom(self,a,D)
   ! Make the spherically averaged restricted atomic natural orbitals
   ! (ANO) and density matrix data and interpolator for atom "a" from
   ! a given (real) density matrix "D".
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: a
      MAT(REAL,:,:), IN :: D
      INT :: f,l
      type(MOLECULE_TYPE), allocatable :: mol
   ENSURE(NOT allocated(self%atom(a)%NOs),"MOLECULE.BASE:spherically_average_atom ... NOs, atom "//trim(to_str_(a)))
   ENSURE(NOT allocated(self%atom(a)%NO_occ_nos),"MOLECULE.BASE:spherically_average_atom ... occ, atom "//trim(to_str_(a)))
   ENSURE(NOT allocated(self%atom(a)%density_mx),"MOLECULE.BASE:spherically_average_atom ... DM, atom "//trim(to_str_(a)))

      ! Create the molecule
      call create_(mol)
      call set_molecule_from_atom_(self,a,mol)

      ! Get the density matrix
      call create_(mol%density_mx,mol%n_bf)
      call create_(mol%density_mx,genre="r ")
      f = self%first_basis_fn_for_atom(a)
      l = self%last_basis_fn_for_atom(a)
      mol%density_mx%r = D(f:l,f:l)

      ! Spherically average the AO density
      call spherically_average_(mol%atom(1),mol%density_mx%r,self%use_spherical_basis)

      ! Make the natural orbitals
      call make_NOs_(mol)

      ! Clean up the SCF files
      call delete_archive_(mol,"NOs")
      call delete_archive_(mol,"NO_occ_nos")

      ! Destroy the atoms's NO and interpolator info
      call destroy_(self%atom(a)%interpolator)
      call destroy_(self%atom(a)%density_mx)
      call destroy_(self%atom(a)%NO_occ_nos)
      call destroy_(self%atom(a)%NOs)

      ! Copy the results
      self%atom(a)%NOs = mol%NOs
      self%atom(a)%NO_occ_nos = mol%NO_occ_nos
      self%atom(a)%density_mx = mol%density_mx

      ! Make the interpolator
      call make_interpolator_(self%atom(a))

      ! Nullify parts we just copied or can't destroy
      call destroy_ptr_part_(mol%atom)  ! do not destroy ptr part

      ! Safe to destroy
      call destroy_(mol)
   end subroutine


!  =================
!  Expectation value
!  =================

   PURE function expectation_0(self,X) result (res)
   ! Evaluates the expectation value of the matrix of the operator X
   ! with the current density matrix
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), IN :: X
      REAL :: res
      STR(len=STR_SIZE) :: genre
      MAT(REAL,:,:), allocatable :: W
   ENSURE(is_square_(X),"MOLECULE.BASE:expectation_0 ... X operator matrix is not square")
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:expectation_0 ... no density_mx")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.BASE:expectation_0 ... no density_mx genre")
      res = ZERO
      genre = genre_(self%density_mx)
      select case (genre)

      case ("r ")
         ENSURE(size(X,1)==self%n_bf,"MOLECULE.BASE:expectation_0 ... (rhf) wrong size, X")
         res = trace_product_with_(self%density_mx%r,X)

      case ("u ")
         ENSURE(size(X,1)==self%n_bf,"MOLECULE.BASE:expectation_0 ... (uhf) wrong size, X")
         res = trace_product_with_(self%density_mx%a,X)
         res = trace_product_with_(self%density_mx%b,X)+res

      case ("gc")
         ENSURE(size(X,1)==2*self%n_bf,"MOLECULE.BASE:expectation_0 ... (gchf) wrong size, X")
         call create_(W,2*self%n_bf,2*self%n_bf)
         W = RE(self%density_mx%gc)
         res = trace_product_with_(W,X)
         call destroy_(W)
      end select
   end function

   PURE function expectation_1(self,X,with_cross_terms) result (res)
   ! Evaluates the expectation value of the matrix of the opmatrix X
   ! with the current density matrix
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), IN :: X
      BIN, optional, IN :: with_cross_terms
      REAL :: res
      STR(len=STR_SIZE) :: orb_kind
      STR(len=STR_SIZE) :: op_kind
      BIN :: cross_terms
      res = ZERO
      cross_terms = FALSE
      if (present(with_cross_terms)) cross_terms = with_cross_terms
      orb_kind = genre_(self%density_mx)
      op_kind = genre_(X)
      select case (orb_kind)

      case ("r ")
         if (op_kind=="r ") then
            ENSURE(size(X%r,1)==self%n_bf,"MOLECULE.BASE:expectation_1 ... (rhf) wrong size, X")
            res = trace_product_with_(self%density_mx%r,X%r)
         else
            ENSURE(size(X%a,1)==self%n_bf,"MOLECULE.BASE:expectation_1 ... (rhf) wrong size, X")
            res = trace_product_with_(self%density_mx%r,X%a)
            res = trace_product_with_(self%density_mx%r,X%b)+res
         end if

      case ("u ")
         if (op_kind=="r ") then
            ENSURE(size(X%r,1)==size(self%density_mx%a,1),"MOLECULE.BASE:expectation_1 ... (uhf) wrong size, X")
            res = trace_product_with_(self%density_mx%a,X%r)
            res = trace_product_with_(self%density_mx%b,X%r)+res
         else
            ENSURE(size(X%a,1)==size(self%density_mx%a,1),"MOLECULE.BASE:expectation_1 ... (uhf) wrong size, X")
            res = trace_product_with_(self%density_mx%a,X%a)
            res = trace_product_with_(self%density_mx%b,X%b)+res
            if (cross_terms) then
               res = trace_product_with_(self%density_mx%a,X%b)+res
               res = trace_product_with_(self%density_mx%b,X%a)+res
            end if
         end if
      end select
   end function


!  ===========================
!  Cluster generation routines
!  ===========================

   subroutine create_cluster(self)
   ! Create a new "self" by generating a cluster from information
   ! stored in .cluster.  The original molecule replaced by the
   ! *cluster-fragent* and stored in .saved. The new molecule is
   ! created from the cluster geometry.
      type(MOLECULE_TYPE), allocatable, INOUT :: self
   ENSURE(allocated(self%crystal),"MOLECULE.BASE:create_cluster ... no crystal data")
   ENSURE(allocated(self%cluster),"MOLECULE.BASE:create_cluster ... no cluster data")
   ENSURE(self%cluster%info_made,"MOLECULE.BASE:create_cluster ... no cluster data")
      call flush_(stdout)
      call text_(stdout,"Resetting molecule to cluster fragment")

      ! Reset .atom list to the *cluster-fragment*
      call destroy_(self%atom)
      call create_fragment_atom_list_(self%cluster,self%atom)

      ! Finalize basis on new .atom list
      self%atom_info_made = FALSE
      call update_atom_basis_group_info_(self)
      call update_fragment_info_(self%crystal,self%atom)

      ! Message
      call flush_(stdout)
      call text_(stdout,"Saving old molecule; making new cluster")
      call flush_(stdout)

      ! Save this molecule & get a clean new molecule
      call save_(self)

      ! The name for the new cluster
      self%name = trim(self%saved%name)//"_cluster"

      ! If a CIF exists, copy that
      if (allocated(self%saved%cif)) self%cif = self%saved%cif

      ! Saved crystal is the same
      self%crystal = self%saved%crystal

      ! Set .atom list from (saved) cluster
      ! Don't make a new cluster, let the user do it.
      call create_atom_list_(self%saved%cluster,self%atom)

      ! Create basis sets
      if (allocated(self%saved%basis)) self%basis = self%saved%basis
      if (allocated(self%saved%slaterbasis)) self%slaterbasis = self%saved%slaterbasis
      if (allocated(self%saved%coppensbasis)) self%coppensbasis = self%saved%coppensbasis
      self%basis_name = self%saved%basis_name
      self%slaterbasis_name = self%saved%slaterbasis_name
      self%coppensbasis_name = self%saved%coppensbasis_name
      self%auxiliary_basis_name = self%saved%auxiliary_basis_name
      self%use_spherical_basis = self%saved%use_spherical_basis

      ! Finalize atom info
      if (allocated(self%atom)) call destroy_bases_(self%atom)
      self%atom_info_made = FALSE
      call update_atom_basis_group_info_(self)

      ! Redo crystal.atoms
      call update_fragment_info_(self%crystal,self%atom)
   end subroutine

   PURE subroutine destroy_cluster(self)
   ! Destroy a molecule created by the "create_cluster" routine, 
   ! and recover the original molecule from .saved
      type(MOLECULE_TYPE), allocatable, INOUT :: self
   ENSURE(allocated(self%saved),"MOLECULE.BASE:destroy_cluster ... no crystal data")
      call unsave_(self)
   end subroutine


!  =====================
!  Atoms and atom groups
!  =====================

   subroutine update_atom_basis_group_info(self)
   ! Update the atom labels, no of electrons, spin multiplicities,
   ! axis system, crystal fragment info, and bases; remove isosurface.
      type(MOLECULE_TYPE), INOUT :: self
      call update_atom_and_basis_info_no_grp_(self)
      call update_atom_groups_(self)
      self%atom_info_made = TRUE
   end subroutine

   subroutine update_atom_groups(self)
   ! Update the .atom_group information
      type(MOLECULE_TYPE), INOUT :: self
      if (NOT allocated(self%atom)) return
      if (has_cartesian_axes_(self%atom)) then

         ! We have .atom()'s ...
         if (use_Ryde_capping_(self%crystal)) then

            ! Ulf Ryde's capped residues
            ! WARNING: .atom_groups list only (parent) uncapped atoms
            call set_Ryde_capped_groups_(self)  ! NOT PURE
         else if (use_NN_(self%crystal)) then

            ! Nearest-neighbour capped fragments, one per asymmetric
            ! unit atom, for a periodic network solid
            call set_NN_capped_groups_(self)  ! NOT PURE
         else if (has_atom_indices_(self%atom_group)) then

            ! User set atom_indices for each atom_group 
            call set_atom_group_mols_(self)  ! NOT PURE
         else

            ! Make .atom_group's from connection table
            call set_connected_atom_groups_(self)
            call set_atom_group_mols_(self)
         end if
      end if
   end subroutine

   subroutine set_atom_group_mols(self)
   ! Make the ".atom_group.mol" molecules
      type(MOLECULE_TYPE), INOUT :: self
      INT :: g
   ENSURE(allocated(self%atom),"MOLECULE.BASE:set_atom_group_mols ... no atoms!")
      if (NOT allocated(self%atom_group)) return

      ! Regenerate molecules
      if (allocated(self%mol)) deallocate(self%mol)
      allocate(self%mol(size(self%atom_group)))

      ! Check is indices are sensible
      call update_(self%atom_group,self%atom)

      ! Create each group from "self"
      do g = 1,size(self%atom_group)

         ! Create mol & define it
         ! ... including mol.crystal part
         call set_molecule_from_atom_group_(self,g,self%mol(g))  ! NOT PURE

         ! Read the MOs & make the density
         ! This won't work until Tonto can read in its own dump
      ! if (.atom_group(g).MO_file_name/=" ") then
      !     .mol(g).read_archive("MOs","r ")
      !     .mol(g).make_SCF_density_mx
      ! end

         ! Rotate and translate mol
         if (self%atom_group(g)%transform_group) then
            call rotate_by_(self%mol(g),self%atom_group(g)%rotation_mx)
            call translate_by_(self%mol(g)%atom,self%atom_group(g)%shift)
         end if
         call set_group_to_(self%mol(g)%atom,g)
         self%mol(g)%name = self%atom_group(g)%name
      end do

      ! Update atom_group info from self
      ! ANOs done above already.
      call update_group_crystal_and_ANOs_(self,skip_ANOs=TRUE)
   end subroutine

   subroutine join_atom_groups(self)
   ! Delete any existing .atom and .basis and set to the concatenation
   ! of the molecules in .atom_group
      type(MOLECULE_TYPE), target, INOUT :: self
      INT :: g,charge,unpaired_electrons
   ENSURE(allocated(self%atom_group),"MOLECULE.BASE:join_atom_groups ... no .atom_group")
   ENSURE(allocated(self%mol),"MOLECULE.BASE:join_atom_groups ... no .mol")
   ENSURE(size(self%mol)==size(self%atom_group),"MOLECULE.BASE:join_atom_groups ... wrong size, .mol")
      if (allocated(self%atom)) call destroy_(self%atom)
      if (allocated(self%basis)) call destroy_(self%basis)
      charge = 0
      unpaired_electrons = 0

      ! Append remaining ...
      do g = 1,size(self%atom_group)
         DIE_IF(NOT allocated(self%mol(g)%basis),"MOLECULE.BASE:join_atom_groups ... no basis on mol "//trim(to_str_(g)))
         DIE_IF(NOT allocated(self%mol(g)%atom),"MOLECULE.BASE:join_atom_groups ... no atoms in mol "//trim(to_str_(g)))
         charge = charge+self%mol(g)%charge
         unpaired_electrons = unpaired_electrons+self%mol(g)%spin_multiplicity-1
         if (g==1) then

            ! Initial atom/basis list
            self%name = self%atom_group(g)%name
            self%basis = self%mol(g)%basis
            self%atom = self%mol(g)%atom
         else

            ! Append basis & atoms
            self%name = trim(self%name)//"_"//trim(self%atom_group(g)%name)
            call append_(self%basis,self%mol(g)%basis)
            call append_(self%atom,self%mol(g)%atom)
         end if
      end do

      ! Unresolve atomic bases
      call destroy_bases_(self%atom)

      ! Uniquify basis
      call remove_repetitions_(self%basis)

      ! Finally: set atom info & basis info (leaky)
      call set_charge_(self,charge)
      call set_spin_multiplicity_(self,unpaired_electrons+1)

      ! Finalise atom info
      call update_atom_and_basis_info_no_grp_(self)
   end subroutine

   subroutine rotate_group(self,g,axis,angle)
   ! Rotate a group "g" of atoms in the molecule using a matrix defined by its
   ! rotation "axis" and the "angle" of rotation around that axis, which are all
   ! inputted on the command line. The rotation is performed on the coordinates
   ! and the group-block diagonal of the density matrix. NOTE: this can be a
   ! problem for group off-diagonal blocks; this routine works for progroup
   ! densities only.
   ! Checked, 7/2/06; the energy decomposition was checked by manual rotation
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: g
      VEC(REAL,3), IN :: axis
      REAL, IN :: angle
      type(MOLECULE_TYPE), allocatable :: mol
      type(ARCHIVE_TYPE) :: archive
      MAT(REAL,3,3) :: R
      VEC(REAL,3) :: v
      MAT(REAL,:,:), allocatable :: P
      INT :: i,a
   ENSURE(allocated(self%atom_group),"MOLECULE.BASE:rotate_group ... no atom group information")

      ! Clean up different-geometry data
      call delete_SCF_archives_(self)
      call to_3x3_rotation_mx_(R,axis,angle)

      ! Rotate atoms; the line below does not work
      do i = 1,self%atom_group(g)%n_atoms
         a = self%atom_group(g)%atom_index(i)
         v = self%atom(a)%position
         call to_product_of_(self%atom(a)%position,R,v)
         call back_transform_using_(self%atom(a)%ADP_tensor,R)
      end do
      call destroy_(self%MOs)  ! leaky here
      if (NOT allocated(self%density_mx)) return
      ENSURE(allocated(self%density_mx%r),"MOLECULE.BASE:rotate_group ... group rotation for restricted DM only")

      ! Create molecule
      call create_(mol)
      call set_molecule_from_atom_group_(self,g,mol)

      ! Rotate density
      call create_(P,mol%n_bf,mol%n_bf)
      call put_group_density_to_(self,P,g,mol)
      call rotate_density_mx_r_(mol,P,R)
      call set_group_density_from_(self,P,g,mol)
      call destroy_(P)
      call destroy_ptr_part_(mol%atom)  ! do not destroy these
      call destroy_(mol)

      ! Write out the density matrix
      call set_(archive,self%name,"density_mx",genre="r ")
      call write_(archive,self%density_mx)
   end subroutine


!  =====================
!  Ryde fragment capping
!  =====================

   subroutine set_Ryde_capped_groups(self)
   ! Make Ulf Ryde's atom capped groups
   ! The strategy is to make them from the compound_sequence_id's
   ! which are found in an mmCIF, normally ....
      type(MOLECULE_TYPE), INOUT :: self
      INT :: n_comp,g
      VEC(INT,:), allocatable :: IDs
   ENSURE(allocated(self%atom),"MOLECULE.BASE:set_Ryde_capped_groups ... no atom info")
   ENSURE(allocated(self%atoms_bonded_to_atom),"MOLECULE.BASE:set_Ryde_capped_groups ... no connection table")
      if (NOT use_Ryde_capping_(self%crystal)) return

      ! Assign connections ... NOT PURE
      call assign_connection_table_(self%atoms_bonded_to_atom)
      n_comp = no_of_compounds_(self%atom)
      if (NOT allocated(self%atom_group)) then

         ! Make groups from compound_id's
         call create_(self%atom_group,n_comp)
         do g = 1,n_comp
            IDs = self%atom(:)%compound_sequence_id
            call set_atom_index_(self%atom_group(g),indices_of_elements_matching_(IDs,g))
            call set_n_atoms_(self%atom_group(g),size(self%atom_group(g)%atom_index))
         end do
      end if

      ! Check indices are sensible
      call update_(self%atom_group,self%atom)

      ! Destroy the group .mol s
      if (allocated(self%mol)) deallocate(self%mol)
      allocate(self%mol(size(self%atom_group)))

      ! Make Ryde's molecular caps ... in .mol(g)
      do g = 1,size(self%atom_group)
         call set_Ryde_cap_for_group_(self,g)
      end do

      ! Clear connections
      call destroy_connection_table_
   end subroutine


!  ===================================
!  Nearest-neighbour fragment capping
!  ===================================

   subroutine set_NN_capped_groups(self)
   ! Make nearest-neighbour capped atom groups for a periodic network
   ! solid, where no molecule can be isolated at all. One fragment is
   ! made around each asymmetric unit atom by following bonds out
   ! ".crystal.NN_level" shells; the shell beyond that is replaced by
   ! capping hydrogen atoms. Only the central atom of each fragment is
   ! a fragment atom -- it is the one whose form factor is wanted.
   ! For quartz at level 1 this gives orthosilicic acid around the
   ! silicon, and silyloxysilane around the oxygen.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name,label
      REAL :: length
      INT :: n_comp,nc_atoms,n_cap,a,c,i
      VEC(type(EVEC_INT_TYPE),:), allocatable :: connections_for
      type(CLUSTER_TYPE), allocatable :: cluster
   ENSURE(allocated(self%atom),"MOLECULE.BASE:set_NN_capped_groups ... no atom info")
   ENSURE(allocated(self%crystal),"MOLECULE.BASE:set_NN_capped_groups ... no crystal info")
      if (NOT use_NN_(self%crystal)) return
      n_comp = self%crystal%n_asymmetric_unit_atoms

      ! Make one group per asymmetric unit atom
      call destroy_(self%atom_group)
      call create_(self%atom_group,n_comp)
      if (allocated(self%mol)) deallocate(self%mol)
      allocate(self%mol(n_comp))

      ! A cluster to grow the fragments in. This is deliberately LOCAL,
      ! not the ".cluster" member: growing a fragment mutates the
      ! cluster, so using the member would leave it holding the last
      ! fragment's atoms against the crystal's own fragment list, and
      ! MOLECULE:put walks both -- a SEGFAULT in CLUSTER:put, where
      ! .crystal.fragment_atom gets indexed by cluster atom.
      call create_(cluster)
      call set_defaults_(cluster,self%crystal)
      do a = 1,n_comp

         ! Grow "NN_level" shells of bonded neighbours out from
         ! asymmetric unit atom "a". Uncapped, one shell fewer is
         ! asked for: do_NN_defragment always adds the shell beyond
         ! "level", and that is the shell which would be capped.
         call make_asym_occupation_list_(cluster,a)
         if (use_NN_capping_(self%crystal)) then
            call do_NN_defragment_(cluster,cluster%occupation_list,NN_level_(self%crystal),nc_atoms)
         else
            call do_NN_defragment_(cluster,cluster%occupation_list,NN_level_(self%crystal)-1,nc_atoms)
         end if
         call extract_geometry_(cluster,cluster%occupation_list,cluster%geometry)
         call create_atom_list_(cluster,self%mol(a)%atom)
         call set_atom_index_(self%atom_group(a),[a])
         call set_n_atoms_(self%atom_group(a),cluster%n_atoms)

         ! Every atom but the central one is there to provide the
         ! chemical environment, not to have a form factor taken
         call set_is_cap_atom_(self%mol(a)%atom(2:),TRUE)

         ! Those atoms are not refined, so they carry no thermal
         ! motion of their own. Their ADP errors are left alone, being
         ! read only for fragment atoms.
         call set_ADP_tensor_(self%mol(a)%atom(2:),ZERO)

         ! Give the central atom a digit, so that its label stays
         ! distinct from the capping labels made below
         if (index_of_digit_(self%mol(a)%atom(1)%label)==0) then
            label = trim(self%mol(a)%atom(1)%label)//"1"
            call set_label_(self%mol(a)%atom(1),label,skip_mass=TRUE)
         end if
         if (use_NN_capping_(self%crystal)) then

            ! Connections say which atom each cap hangs off, and hence
            ! the direction to place it in
            call make_connection_table_(self%mol(a)%atom,connections_for)
            n_cap = size(self%mol(a)%atom)-nc_atoms
            do i = 1,n_cap

               ! WARNING: AN has to be set after setting unique label
               label = "H-cap"//trim(to_str_(i))
               call set_label_(self%mol(a)%atom(nc_atoms+i),label,skip_mass=TRUE)
               call set_atomic_number_and_mass_(self%mol(a)%atom(nc_atoms+i),"H ")

               ! Put the cap along the internuclear vector of the bond
               ! it replaces, at the sum of the two covalent radii
               c = connections_for(nc_atoms+i)%element(1)
               length = covalent_radius_ccdc_(self%mol(a)%atom(c))+covalent_radius_ccdc_(self%mol(a)%atom(nc_atoms+i))
               call set_bond_length_(self%mol(a)%atom,c,nc_atoms+i,length)
            end do
            call destroy_(connections_for)
         end if

         ! Set .name
         name = "group-"//trim(to_str_(a))//"-"
         name = trim(name)//trim(chemical_formula_(self%mol(a)%atom,with_spaces=FALSE))
         call set_name_(self%mol(a),name)
         call set_name_(self%atom_group(a),name)

         ! Assign basis set -- don't destroy!
         call set_basis_name_(self%mol(a),self%basis_name)  ! NOT PURE

         ! Set charge
         call set_charge_(self%mol(a),self%atom_group(a)%charge)

         ! Make atom info; groups should not contain subgroups!
         self%mol(a)%atom_info_made = FALSE
         call set_group_to_(self%mol(a)%atom,a)
         call update_atom_and_basis_info_no_grp_(self%mol(a))  ! no recursion

         ! Reset multiplicity? NOTE: reset no. of alpha & beta electrons
         if (self%atom_group(a)%spin_multiplicity_set) then
            call set_spin_multiplicity_(self%mol(a),self%atom_group(a)%spin_multiplicity)
         end if

         ! Set guess-type SCF defaults: no output
         ! Will reset SCF_kind based on multiplicity
         call set_SCF_guess_defaults_from_(self%mol(a),self%SCF_data)

         ! Copy the crystal -- destroy it!
         ! Make separate asymm unit with only mol in it later ...
         self%mol(a)%crystal = self%crystal
         call set_minimal_copy_(self%mol(a)%crystal,self%crystal)
         call update_fragment_info_(self%mol(a)%crystal,self%mol(a)%atom)

         ! Don't remake ANO data if there
         call resolve_ANOs_from_(self%mol(a),self)
      end do
      call destroy_(cluster)
   end subroutine

   subroutine set_Ryde_cap_for_group(self,g)
   ! Make a capped fragment for molecule "mol, the atom_group(g).mol
   ! with compound_sequence_id "g", ready for fragHAR calculation.
   ! *This routine will add extra atoms*.
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: g
      STR(len=STR_SIZE) :: name
   ENSURE(allocated(self%atom),"MOLECULE.BASE:set_Ryde_cap_for_group ... no atom info")
   ENSURE(allocated(self%atom_group),"MOLECULE.BASE:set_Ryde_cap_for_group ... no atom_group info")
   ENSURE(allocated(self%mol),"MOLECULE.BASE:set_Ryde_cap_for_group ... no .mol")
   ENSURE(g<=size(self%atom_group),"MOLECULE.BASE:set_Ryde_cap_for_group ... group number g too big")
   ENSURE(g>=1,"MOLECULE.BASE:set_Ryde_cap_for_group ... group number g less than 1!")
   ENSURE(size(self%mol)==size(self%atom_group),"MOLECULE.BASE:set_Ryde_cap_for_group ... wrong size, .mol")

      ! Start with *uncapped* .atom_group(g) atom indices ...
      self%mol(g)%atom = self%atom(self%atom_group(g)%atom_index)

      ! Add cap atoms !!!!!!!!!!!!!!!
      call add_Ryde_cap_for_group_(self,g,self%mol(g))
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! Set .name
      name = id_compound_str_(self%atom(self%atom_group(g)%atom_index(1)))
      if (name==" ") then
         name = "group-"//trim(to_str_(g))//"-"
         name = trim(name)//trim(chemical_formula_(self%mol(g)%atom,with_spaces=FALSE))
      end if
      call set_name_(self%mol(g),name)
      call set_name_(self%atom_group(g),name)

      ! Assign basis set -- don't destroy!
      call set_basis_name_(self%mol(g),self%basis_name)  ! NOT PURE

      ! Set charge
      call set_charge_(self%mol(g),self%atom_group(g)%charge)

      ! Make atom info; groups should not contain subgroups!
      self%mol(g)%atom_info_made = FALSE
      call set_group_to_(self%mol(g)%atom,g)
      call update_atom_and_basis_info_no_grp_(self%mol(g))  ! no recursion
      ! Is the above doing REDUNDANT basis resolution?

      ! Reset multiplicity? NOTE: reset no. of alpha & beta electrons
      if (self%atom_group(g)%spin_multiplicity_set) then
         call set_spin_multiplicity_(self%mol(g),self%atom_group(g)%spin_multiplicity)
      end if

      ! Set guess-type SCF defaults: no output
      ! Will reset SCF_kind based on multiplicity
      call set_SCF_guess_defaults_from_(self%mol(g),self%SCF_data)

      ! Copy the crystal -- destroy it!
      ! Make separate asymm unit with only mol in it later ...
      if (allocated(self%crystal)) then
         self%mol(g)%crystal = self%crystal
         call set_minimal_copy_(self%mol(g)%crystal,self%crystal)
         call update_fragment_info_(self%mol(g)%crystal,self%mol(g)%atom)
      end if

      ! Don't remake ANO data if there
      call resolve_ANOs_from_(self%mol(g),self)
   end subroutine

   PURE subroutine add_Ryde_cap_for_group(self,g,mol)
   ! Add Ryde' caps fragment for molecule the atom_group(g).mol with
   ! compound_sequence_id "g".
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: g
      type(MOLECULE_TYPE), INOUT :: mol
      BIN :: in_residue
      MAT(INT,:,:), allocatable :: bonded_atom
      INT :: n_bonds,g2,b,ap,bp
   ENSURE(allocated(self%atom),"MOLECULE.BASE:add_Ryde_cap_for_group ... no atom info")
   ENSURE(allocated(self%atom_group),"MOLECULE.BASE:add_Ryde_cap_for_group ... no atom_group info")
   ENSURE(g<=size(self%atom_group),"MOLECULE.BASE:add_Ryde_cap_for_group ... group number g too big")
   ENSURE(g>=1,"MOLECULE.BASE:add_Ryde_cap_for_group ... group number g less than 1!")

      ! Residue atom?
      in_residue = is_in_a_residue_(self%atom(self%atom_group(g)%atom_index(1)))
      if (NOT in_residue) return
      do g2 = 1,size(self%atom_group)

         ! Skip group "g" itself
         if (g2==g) cycle

         ! No. of bonds to atom_group "g2"
         n_bonds = n_bonds_between_atom_groups_(self,g,g2)

         ! Any bonds to group "g2"?
         if (n_bonds<=0) cycle

         ! Cap each connected group
         bonded_atom = bonded_atoms_between_groups_(self,g,g2)
         do b = 1,n_bonds

            ! Get atoms from each group
            ap = bonded_atom(1,b)
            bp = bonded_atom(2,b)

            ! Use them to start the Ryde cap
            call add_Ryde_cap_from_atoms_(self,ap,bp,mol)
         end do

         ! Clean
         call destroy_(bonded_atom)
      end do
   end subroutine

   PURE subroutine add_Ryde_cap_from_atoms(self,ap,bp,mol)
   ! Cap residue molecule "mol" starting from atom "ap" in that
   ! residue, which is connected to atom "bp" in another group.
   ! WARNING: This will add extra atoms to atom_group(g1).mol.atom 
   ! but it won't add extra indices into atom_index!
   ! Zhang, D. W. & Zhang, J. Z. H. (2003). J. Chem. Phys., 119(7), 3599-3605.
   ! Soederhjelm, P.& Ryde, U. (2008). J. Phys. Chem. A, 113(3), 617-627
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: ap,bp
      type(MOLECULE_TYPE), INOUT :: mol
      INT :: i,j,cp,cf,dp,last
      VEC(INT,:), allocatable :: bp_bonds,cp_bonds
      REAL :: length
      STR(len=STR_SIZE) :: label
   ENSURE(allocated(self%atom_group),"MOLECULE.BASE:add_Ryde_cap_from_atoms ... no atom_group info")
   ENSURE(allocated(self%atoms_bonded_to_atom),"MOLECULE.BASE:add_Ryde_cap_from_atoms ... no connection table")

      ! Append connected atom bp in atom_group(g2)
      call append_(mol%atom,self%atom(bp))
      last = size(mol%atom)
      call set_cap_atom_(mol%atom(last),TRUE)

      ! Find connections to atom bp ... in SMILES:
      ! -ap-res1 - bp-res2(cp_1-res2)(cp_2-res2) ...
      bp_bonds = self%atoms_bonded_to_atom(bp)%element

      ! Find connections to bp
      do i = 1,size(bp_bonds)

         ! This case cap atom 2
         cp = bp_bonds(i)

         ! Already found atom in residue g
         if (cp==ap) cycle

         ! Append new cap atom 2
         call append_(mol%atom,self%atom(cp))
         last = size(mol%atom)
         call set_cap_atom_(mol%atom(last),TRUE)
         if (self%atom(ap)%atomic_number>1) then

            ! Add caps for all non-H atom ... should be 3
            ! (serging for cap atom 3)
            cp_bonds = self%atoms_bonded_to_atom(cp)%element

            ! Index of cp in atom_group(g) fragment
            cf = last

            ! Find connections to cp
            do j = 1,size(cp_bonds)
               dp = cp_bonds(j)

               ! Already found atom bp
               if (dp==bp) cycle

               ! Append new cap atom, keep its index
               call append_(mol%atom,self%atom(dp))
               last = size(mol%atom)
               call set_cap_atom_(mol%atom(last),TRUE)

               ! Correct label & atomic number for last added atom
               ! WARNING: AN has to be set after setting unique label
               label = "H-"//trim(to_str_(self%atom(dp)%atom_sequence_id))//"-"
               label = trim(label)//trim(compound_id_str_(self%atom(dp)))
               call set_label_(mol%atom(last),label,skip_mass=TRUE)
               call set_atomic_number_and_mass_(mol%atom(last),"H ")
               call set_cap_atom_(mol%atom(last),TRUE)

               ! Reset bond length to last added atom
               length = CIF_CH_BOND_LENGTH*BOHR_PER_ANGSTROM
               call set_bond_length_(mol%atom,cf,last,length)
            end do
         end if
      end do
   end subroutine


!  =================
!  Molecule rotation
!  =================

   PURE subroutine rotate_by(self,R)
   ! Rotate the whole molecule using matrix "R" which specifies how
   ! positions in space are to be transformed i.e. x' = Rx.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: R
   ENSURE(is_square_(R),"MOLECULE.BASE:rotate_by ... rotation matrix is not square")
   ENSURE(size(R,1)==3,"MOLECULE.BASE:rotate_by ... rotation matrix must be 3x3")
      if (is_zero_(R)) return
      call rotate_by_(self%atom,R)
      call rotate_MOs_(self,R)
      call rotate_density_mx_(self,R)
   end subroutine

   PURE subroutine rotate_MOs(self,R)
   ! Rotate the molecular orbital matrix using matrix "R" which
   ! specifies how positions in space are to be transformed
   ! i.e. x' = Rx.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: R
      STR(len=STR_SIZE) :: genre
   ENSURE(is_one_of_(genre_(self%MOs),["r ","u "]),"MOLECULE.BASE:rotate_MOs ... unimplemented rotation")
      if (NOT allocated(self%MOs)) return
      if (is_deallocated_or_no_genre_(self%MOs)) return
      genre = genre_(self%MOs)
      select case (genre)

      case ("r ")
         call rotate_MOs_r_(self,self%MOs%r,R)

      case ("u ")
         call rotate_MOs_r_(self,self%MOs%a,R)
         call rotate_MOs_r_(self,self%MOs%b,R)
      end select
   end subroutine

   PURE subroutine rotate_MOs_r(self,MO,R)
   ! Rotate a restricted molecular orbitals "MO" using matrix "R",
   ! where "R" specifies how positions in space are to be transformed
   ! i.e. x' = Rx.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: MO
      MAT(REAL,:,:), IN :: R
      MAT(REAL,:,:), allocatable :: new
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: tr
      INT :: s,f,l,a
   ENSURE(size(MO,1)==self%n_bf,"MOLECULE.BASE:rotate_MOs_r ... wrong dim1 for MO orbitals")
   ENSURE(is_square_(R),"MOLECULE.BASE:rotate_MOs_r ... rotation matrix R is not square")
   ENSURE(size(R,1)==3,"MOLECULE.BASE:rotate_MOs_r ... rotation matrix R is not 3x3")
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.BASE:rotate_MOs_r ... only for cartesian bases")
      call create_(new,self%n_bf,self%n_bf)
      call make_gaussian_xyz_matrices_(tr,R)
      do s = 1,self%n_shell
         f = self%first_basis_fn_for_shell(s)
         l = self%last_basis_fn_for_shell(s)
         a = self%angular_moment_for_shell(s)
         call to_product_of_(new(f:l,:),tr(a)%element(:,:),MO(f:l,:))
      end do
      MO = new
      call destroy_(tr)
      call destroy_(new)
   end subroutine

   PURE subroutine rotate_density_mx(self,R)
   ! Rotate the resricted density matrix using matrix "R" which
   ! specifies how positions in space are to be transformed
   ! i.e. x' = Rx.
   ! NOTE: the kind is taken from the MO's.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: R
      STR(len=STR_SIZE) :: genre
   ENSURE(is_one_of_(genre_(self%MOs),["r ","u "]),"MOLECULE.BASE:rotate_density_mx ... unimplemented rotation")
      if (NOT allocated(self%MOs)) return
      if (is_deallocated_or_no_genre_(self%MOs)) return
      genre = genre_(self%MOs)
      select case (genre)

      case ("r ")
         call rotate_density_mx_r_(self,self%density_mx%r,R)

      case ("u ")
         call rotate_density_mx_r_(self,self%density_mx%a,R)
         call rotate_density_mx_r_(self,self%density_mx%b,R)
      end select
   end subroutine

   PURE subroutine rotate_density_mx_r(self,P,R)
   ! Rotate the density matrix "P" using matrix "R", where "R" specifies
   ! how positions in space are to be transformed i.e. x' = Rx.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: P
      MAT(REAL,:,:), IN :: R
      MAT(REAL,:,:), allocatable :: new
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: tr
      INT :: s,f,l,a
   ENSURE(is_square_(P),"MOLECULE.BASE:rotate_density_mx_r ... wrong shape for density matrix P")
   ENSURE(size(P,1)==self%n_bf,"MOLECULE.BASE:rotate_density_mx_r ... wrong dim1 for density matrix P")
   ENSURE(is_square_(R),"MOLECULE.BASE:rotate_density_mx_r ... rotation matrix R is not square")
   ENSURE(size(R,1)==3,"MOLECULE.BASE:rotate_density_mx_r ... rotation matrix R is not 3x3")
      call create_(new,self%n_bf,self%n_bf)
      call make_gaussian_xyz_matrices_(tr,R)
      do s = 1,self%n_shell
         f = self%first_basis_fn_for_shell(s)
         l = self%last_basis_fn_for_shell(s)
         a = self%angular_moment_for_shell(s)
         call to_product_of_(new(f:l,:),tr(a)%element(:,:),P(f:l,:))
      end do
      do s = 1,self%n_shell
         f = self%first_basis_fn_for_shell(s)
         l = self%last_basis_fn_for_shell(s)
         a = self%angular_moment_for_shell(s)
         call to_product_of_(P(:,f:l),new(:,f:l),tr(a)%element(:,:),transpose_b=TRUE)
      end do
      call destroy_(tr)
      call destroy_(new)
   end subroutine

   PURE subroutine rotate_fock(self,R)
   ! Rotate the molecular orbital matrix using matrix "R" which
   ! specifies how positions in space are to be transformed
   ! i.e. x' = Rx.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: R
      STR(len=STR_SIZE) :: genre
   ENSURE(is_one_of_(genre_(self%fock_mx),["r ","u "]),"MOLECULE.BASE:rotate_fock ... unimplemented rotation")
      if (NOT allocated(self%fock_mx)) return
      if (is_deallocated_or_no_genre_(self%fock_mx)) return
      genre = genre_(self%fock_mx)
      select case (genre)

      case ("r ")
         call rotate_fock_mx_r_(self,self%fock_mx%r,R)

      case ("u ")
         call rotate_fock_mx_r_(self,self%fock_mx%a,R)
         call rotate_fock_mx_r_(self,self%fock_mx%b,R)
      end select
   end subroutine

   PURE subroutine rotate_fock_mx_r(self,G,R)
   ! Rotate the fock matrix "G" using matrix "R", where "R" specifies
   ! how positions in space are to be transformed i.e. x' = Rx.
   ! NOTE: THIOS IS WRONG ... SEE DM rotation!
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: G
      MAT(REAL,:,:), IN :: R
      MAT(REAL,:,:), allocatable :: new
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: tr
      INT :: s,f,l,a
   ENSURE(is_square_(G),"MOLECULE.BASE:rotate_fock_mx_r ... wrong shape for density matrix P")
   ENSURE(size(G,1)==self%n_bf,"MOLECULE.BASE:rotate_fock_mx_r ... wrong dim1 for density matrix P")
   ENSURE(is_square_(R),"MOLECULE.BASE:rotate_fock_mx_r ... rotation matrix R is not square")
   ENSURE(size(R,1)==3,"MOLECULE.BASE:rotate_fock_mx_r ... rotation matrix R is not 3x3")
      call create_(new,self%n_bf,self%n_bf)
      call make_gaussian_xyz_matrices_(tr,R)
      do s = 1,self%n_shell
         f = self%first_basis_fn_for_shell(s)
         l = self%last_basis_fn_for_shell(s)
         a = self%angular_moment_for_shell(s)
         call to_product_of_(new(f:l,:),tr(a)%element(:,:),G(f:l,:),transpose_b=TRUE)
      end do
      G = new
      call destroy_(tr)
      call destroy_(new)
   end subroutine


!  ==============================
!  Shift/remove linear dependence
!  ==============================

   subroutine shift_dependence_from(self,F)
   ! Level shift the linear dependence in "F"
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), INOUT :: F
      STR(len=STR_SIZE) :: s_kind
   ENSURE(allocated(self%SCF_data),"MOLECULE.BASE:shift_dependence_from ... no SCF_data")

      ! Extrapolate compressed entities
      s_kind = spinorbital_genre_(self%SCF_data)
      select case (s_kind)
      case ("rhf","rks","xray_rhf","xray_rks","rdvpt")
         call shift_dependence_from_r_(self,F%r)
      case ("uhf","uks","rohf","pnd_uhf","pnd_uks","xray_uhf","xray_uks","xray_pnd_uhf","xray_pnd_uks")
         call shift_dependence_from_r_(self,F%a)
         call shift_dependence_from_r_(self,F%b)
      case default
      end select
   end subroutine

   subroutine shift_dependence_from_r(self,F)
   ! Level shift the linear dependence in AO matrix "F" using overlap
   ! eigenvectors "evec" with small eigenvalue "eval".
   ! WARNING: this routine assumes eval's are ordered
   ! smallest to largest.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:) :: F
      INT :: n
      REAL :: tol,shift
   ENSURE(allocated(self%SCF_data),"MOLECULE.BASE:shift_dependence_from_r ... no SCF_data")
   ENSURE(allocated(self%overlap_mx_evecs),"MOLECULE.BASE:shift_dependence_from_r ... no overlap_mx_evecs")
   ENSURE(allocated(self%overlap_mx_evals),"MOLECULE.BASE:shift_dependence_from_r ... no overlap_mx_evals")
   ENSURE(size(F,1)==size(self%overlap_mx_evecs,1),"MOLECULE.BASE:shift_dependence_from_r ... wrong size, F")

      ! Get no of vectors smaller than "tol"
      tol = self%SCF_data%linear_dependence_tol
      n = index_of_first_greater_than_(self%overlap_mx_evals,tol)-1

      ! Decouple and shift the small eigenvectors to high energy
      shift = self%SCF_data%linear_dependence_shift
      call decouple_and_shift_r(F,self%overlap_mx_evecs,n,shift)
   end subroutine

   subroutine decouple_and_shift_r(F,evec,n,shift)
   ! In "F", decouple the first "n" eigenvectors "evec" and shift them
   ! to eigenvalue "shift". NOTE: ensure "evec" is orthonormal.
      MAT(REAL,:,:) :: F
      MAT(REAL,:,:), IN :: evec
      INT, IN :: n
      REAL, IN :: shift
      INT :: d
   ENSURE(is_square_(F),"MOLECULE.BASE:decouple_and_shift_r ... F is not square")

      ! Return if no vectores to shift/decouple
      if (n<=0) return
      d = size(F,1)

      ! Change to the eigenvector basis
      call change_basis_using_(F,evec)

      ! Decouple the vectors
      F(1:n,n+1:d) = ZERO
      F(n+1:d,1:n) = ZERO

      ! Shift the first n eigenvectors
      call set_diagonal_to_(F(1:n,1:n),shift)

      ! Revert from the eigenvector basis
      call back_transform_using_(F,evec)
   end subroutine

   subroutine remove_dependence_from(self,F)
   ! Zero-out the linear dependence in "F"
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: F
      STR(len=STR_SIZE) :: s_kind
   ENSURE(allocated(self%SCF_data),"MOLECULE.BASE:remove_dependence_from ... no SCF_data")

      ! Extrapolate compressed entities
      s_kind = spinorbital_genre_(self%SCF_data)
      select case (s_kind)
      case ("rhf","rks","xray_rhf","xray_rks","rdvpt")
         call remove_dependence_from_r_(self,F%r)
      case ("uhf","uks","rohf","pnd_uhf","pnd_uks","xray_uhf","xray_uks","xray_pnd_uhf","xray_pnd_uks")
         call remove_dependence_from_r_(self,F%a)
         call remove_dependence_from_r_(self,F%b)
      case default
      end select
   end subroutine

   PURE subroutine remove_dependence_from_r(self,F)
   ! In "F", remove the linearly-dependent overlap eigenvectors.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: F
      REAL :: tol
      INT :: n
   ENSURE(allocated(self%SCF_data),"MOLECULE.BASE:remove_dependence_from_r ... no SCF_data")
   ENSURE(allocated(self%overlap_mx_evecs),"MOLECULE.BASE:remove_dependence_from_r ... no overlap_mx_evecs")
   ENSURE(size(F,1)==self%n_bf,"MOLECULE.BASE:remove_dependence_from_r ... wrong size, F")

      ! Get no of vectors smaller than "tol"
      tol = self%SCF_data%linear_dependence_tol
      n = index_of_first_greater_than_(self%overlap_mx_evals,tol)-1

      ! Return if no vectores to shift/decouple
      if (n<=0) return

      ! Change to the eigenvector basis
      call change_basis_using_(F,self%overlap_mx_evecs)

      ! Zero couplings 
      F(:,1:n) = ZERO
      F(1:n,:) = ZERO

      ! Revert from the eigenvector basis
      call back_transform_using_(F,self%overlap_mx_evecs)
   end subroutine


!  ================
!  Natural orbitals
!  ================

   subroutine make_NOs(self,genre)
   ! Make the natural orbitals from the density matrix
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), optional, IN :: genre
      STR(len=STR_SIZE) :: orb_kind
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_NOs ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_NOs ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.BASE:make_NOs ... no density matrix")
      if (present(genre)) then; orb_kind = genre
      else; orb_kind = genre_(self%density_mx)
      end if
      select case (orb_kind)
      case ("r "); call make_r_NOs_(self)
      case ("u "); call make_u_NOs_(self)
      case ("gc"); call make_gc_NOs_(self)
      case default; allocate(tonto%known_keywords(3))
      tonto%known_keywords(1) = "r "
      tonto%known_keywords(2) = "u "
      tonto%known_keywords(3) = "gc"
      call unknown_(tonto,orb_kind,"MOLECULE.BASE:make_NOs")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine make_r_NOs_0(self)
   ! Make the restricted natural orbitals from the density matrix
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_r_NOs_0 ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.BASE:make_r_NOs_0 ... no density matrix")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.BASE:make_r_NOs_0 ... no overlap matrix")

      ! Recreate (leaky)
      call destroy_(self%NOs)
      call create_(self%NOs,self%n_bf,"r ")
      call destroy_(self%NO_occ_nos)
      call create_(self%NO_occ_nos,self%n_bf,"r ")

      ! Make the restricted NO's
      call make_r_NOs_(self,self%NOs%r,self%NO_occ_nos%r,self%density_mx%r)
   end subroutine

   subroutine make_u_NOs(self)
   ! Make the unrestricted natural orbitals from the density matrix
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_u_NOs ... no density matrix")
   ENSURE(allocated(self%density_mx%a),"MOLECULE.BASE:make_u_NOs ... no density matrix")
   ENSURE(allocated(self%density_mx%b),"MOLECULE.BASE:make_u_NOs ... no density matrix")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.BASE:make_u_NOs ... no overlap matrix")

      ! Recreate (leaky)
      call destroy_(self%NOs)
      call create_(self%NOs,self%n_bf,"u ")
      call destroy_(self%NO_occ_nos)
      call create_(self%NO_occ_nos,self%n_bf,"u ")

      ! Make the unrestricted NO's
      call make_r_NOs_(self,self%NOs%a,self%NO_occ_nos%a,self%density_mx%a)
      call make_r_NOs_(self,self%NOs%b,self%NO_occ_nos%b,self%density_mx%b)
   end subroutine

   subroutine make_gc_NOs(self)
   ! Make the general natural orbitals and occupations from the
   ! complex density matrix.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: R
      MAT(CPX,:,:), allocatable :: V,X
   ENSURE(allocated(self%density_mx),"MOLECULE.BASE:make_gc_NOs ... no density matrix")
   ENSURE(allocated(self%density_mx%gc),"MOLECULE.BASE:make_gc_NOs ... no density matrix")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.BASE:make_gc_NOs ... no overlap matrix")

      ! Recreate
      call destroy_(self%NOs)
      call create_(self%NOs,self%n_bf,"gc")
      call destroy_(self%NO_occ_nos)
      call create_(self%NO_occ_nos,self%n_bf,"g")
      call create_(V,2*self%n_bf,2*self%n_bf)
      call create_(R,self%n_bf,self%n_bf)
      call make_r_overlap_sqrt_(self,R)
      V = ZERO
      call set_aa_block_to_(V,R)
      call set_bb_block_to_(V,R)
      call destroy_(R)
      call create_(X,2*self%n_bf,2*self%n_bf)
      X = self%density_mx%gc
      call change_basis_using_(X,V)
      call solve_hermitian_eigenproblem_(X,self%NO_occ_nos%g,V)
      call create_(R,self%n_bf,self%n_bf)
      call make_r_overlap_inverse_sqrt_(self,R)
      X = ZERO
      call set_aa_block_to_(X,R)
      call set_bb_block_to_(X,R)
      call destroy_(R)
      call to_product_of_(self%NOs%gc,X,V)
      self%NO_occ_nos%g = self%NO_occ_nos%g(2*self%n_bf:1:-1)
      self%NOs%gc = self%NOs%gc(:,2*self%n_bf:1:-1)
      call destroy_(X)
      call destroy_(V)
   end subroutine

   subroutine make_E_NOs(self,genre)
   ! Make the energy natural orbitals from the energy density matrix
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), optional, IN :: genre
      STR(len=STR_SIZE) :: orb_kind
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_E_NOs ... no basis info")
   ENSURE(allocated(self%E_density_mx),"MOLECULE.BASE:make_E_NOs ... no density matrix")
   ENSURE(has_any_genre_(self%E_density_mx),"MOLECULE.BASE:make_E_NOs ... no density matrix")
      if (present(genre)) then; orb_kind = genre
      else; orb_kind = genre_(self%E_density_mx)
      end if
      select case (orb_kind)
      case ("r "); call make_r_ENOs_(self)
      case ("u "); call make_u_ENOs_(self)
      case ("gc"); call make_gc_ENOs_(self)
      case default; allocate(tonto%known_keywords(3))
      tonto%known_keywords(1) = "r "
      tonto%known_keywords(2) = "u "
      tonto%known_keywords(3) = "gc"
      call unknown_(tonto,orb_kind,"MOLECULE.BASE:make_E_NOs")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine make_r_ENOs(self)
   ! Make the restricted energy natural orbitals from the energy density matrix
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%E_density_mx),"MOLECULE.BASE:make_r_ENOs ... no E density matrix")
   ENSURE(allocated(self%E_density_mx%r),"MOLECULE.BASE:make_r_ENOs ... no E density matrix")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.BASE:make_r_ENOs ... no overlap matrix")

      ! Recreate (leaky)
      call destroy_(self%E_NO_energies)
      call create_(self%E_NO_energies,self%n_bf,"r ")
      call destroy_(self%E_NOs)
      call create_(self%E_NOs,self%n_bf,"r ")

      ! Make the restricted NO's
      call make_r_NOs_(self,self%E_NOs%r,self%E_NO_energies%r,self%E_density_mx%r)
   end subroutine

   subroutine make_u_ENOs(self)
   ! Make the unrestricted energy natural orbitals from the energy density matrix
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%E_density_mx),"MOLECULE.BASE:make_u_ENOs ... no E density matrix")
   ENSURE(allocated(self%E_density_mx%a),"MOLECULE.BASE:make_u_ENOs ... no E density matrix")
   ENSURE(allocated(self%E_density_mx%b),"MOLECULE.BASE:make_u_ENOs ... no E density matrix")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.BASE:make_u_ENOs ... no overlap matrix")

      ! Recreate (leaky)
      call destroy_(self%E_NO_energies)
      call create_(self%E_NO_energies,self%n_bf,"u ")
      call destroy_(self%E_NOs)
      call create_(self%E_NOs,self%n_bf,"u ")

      ! Make the unrestricted NO's
      call make_r_NOs_(self,self%E_NOs%a,self%E_NO_energies%a,self%E_density_mx%a,reverse_order=FALSE)
      call make_r_NOs_(self,self%E_NOs%b,self%E_NO_energies%b,self%E_density_mx%b,reverse_order=FALSE)
   end subroutine

   subroutine make_gc_ENOs(self)
   ! Make the general energy natural orbitals and eigenvalues from the
   ! general complex energy density matrix.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: R
      MAT(CPX,:,:), allocatable :: V,X
   ENSURE(allocated(self%E_density_mx),"MOLECULE.BASE:make_gc_ENOs ... no E_density matrix")
   ENSURE(allocated(self%E_density_mx%gc),"MOLECULE.BASE:make_gc_ENOs ... no E density matrix")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.BASE:make_gc_ENOs ... no overlap matrix")

      ! Recreate
      call destroy_(self%E_NOs)
      call create_(self%E_NOs,self%n_bf,"gc")
      call destroy_(self%E_NO_energies)
      call create_(self%E_NO_energies,self%n_bf,"g ")
      call create_(V,2*self%n_bf,2*self%n_bf)
      call create_(R,self%n_bf,self%n_bf)
      call make_r_overlap_sqrt_(self,R)
      V = ZERO
      call set_aa_block_to_(V,R)
      call set_bb_block_to_(V,R)
      call destroy_(R)
      call create_(X,2*self%n_bf,2*self%n_bf)
      X = self%E_density_mx%gc
      call change_basis_using_(X,V)
      call solve_hermitian_eigenproblem_(X,self%E_NO_energies%g,V)
      call create_(R,self%n_bf,self%n_bf)
      call make_r_overlap_inverse_sqrt_(self,R)
      X = ZERO
      call set_aa_block_to_(X,R)
      call set_bb_block_to_(X,R)
      call destroy_(R)
      call to_product_of_(self%E_NOs%gc,X,V)
      call destroy_(X)
      call destroy_(V)
   end subroutine

   subroutine make_r_NOs_1(self,NO,occupation,P,reverse_order)
   ! Make the real natural orbitals "NO" and "occupation" numbers from the
   ! density matrix "P"
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:) :: NO,P
      VEC(REAL,:) :: occupation
      BIN, optional, IN :: reverse_order
      BIN :: reverse
      MAT(REAL,:,:), allocatable :: V,X
   ENSURE(allocated(self%overlap_mx),"MOLECULE.BASE:make_r_NOs_1 ... no overlap matrix")
      reverse = TRUE
      if (present(reverse_order)) reverse = reverse_order
      call create_(V,self%n_bf,self%n_bf)
      call make_r_overlap_sqrt_(self,V)
      call create_(X,self%n_bf,self%n_bf)
      call change_basis_to_(P,X,V)
      call solve_symmetric_eigenproblem_(X,occupation,V)
      call make_r_overlap_inverse_sqrt_(self,X)
      call to_product_of_(NO,X,V)
      call destroy_(X)
      call destroy_(V)
      if (reverse) then
         occupation = occupation(self%n_bf:1:-1)
         NO = NO(:,self%n_bf:1:-1)
      end if
   end subroutine

   subroutine make_density_mx_from(self,NO,occupations,genre)
   ! Make the density_mx from the natural orbitals and the
   ! occupation numbers.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), IN :: NO
      type(OPVECTOR_TYPE), IN :: occupations
      STR(len=*), IN, optional :: genre
      STR(len=STR_SIZE) :: genus
      MAT(REAL,:,:), allocatable :: Pr
   ENSURE(self%basis_info_made,"MOLECULE.BASE:make_density_mx_from ... no basis info")
      if (present(genre)) then; genus = genre
      else; genus = genre_(self%NOs)
      end if
      select case (genus)

      case ("r ")
         ENSURE(allocated(NO%r),"MOLECULE.BASE:make_density_mx_from ... no restricted NOs")
         ENSURE(allocated(occupations%r),"MOLECULE.BASE:make_density_mx_from ... no restricted occupations")
         call destroy_(self%density_mx)
         call create_(self%density_mx,self%n_bf,"r ")
         call create_(Pr,self%n_bf,self%n_bf)
         call to_product_with_diagonal_(Pr,NO%r,occupations%r)
         call to_product_of_(self%density_mx%r,Pr,NO%r,transpose_b=TRUE)
         call destroy_(Pr)

      case ("u ")
         ENSURE(allocated(NO%a),"MOLECULE.BASE:make_density_mx_from ... no alpha NOs")
         ENSURE(allocated(NO%b),"MOLECULE.BASE:make_density_mx_from ... no beta NOs")
         ENSURE(allocated(occupations%a),"MOLECULE.BASE:make_density_mx_from ... no alpha occupations")
         ENSURE(allocated(occupations%b),"MOLECULE.BASE:make_density_mx_from ... no beta occupations")
         call destroy_(self%density_mx)
         call create_(self%density_mx,self%n_bf,"u ")
         call create_(Pr,self%n_bf,self%n_bf)
         call to_product_with_diagonal_(Pr,NO%a,occupations%a)
         call to_product_of_(self%density_mx%a,Pr,NO%a,transpose_b=TRUE)
         call to_product_with_diagonal_(Pr,NO%b,occupations%b)
         call to_product_of_(self%density_mx%b,Pr,NO%b,transpose_b=TRUE)
         call destroy_(Pr)

      case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "r "
         tonto%known_keywords(2) = "u "
         call unknown_(tonto,genus,"MOLECULE.BASE:make_density_mx_from")
         deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine make_density_mx_from_NOs(self)
   ! Make the .density_mx from the *NOs* and the
   ! *NO_occ_nos* vector. Only restricted so far.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: genre
      type(ARCHIVE_TYPE) :: arch
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.BASE:make_density_mx_from_NOs ... no occupation numbers")
   ENSURE(allocated(self%NOs),"MOLECULE.BASE:make_density_mx_from_NOs ... no natural orbitals")
      genre = genre_(self%NOs)

      ! Destroy
      call destroy_(self%density_mx,genre)
      call create_(self%density_mx,genre)

      ! Make it
      select case (genre)

      case ("r")
         ENSURE(is_allocated_with_genre_(self%NOs,"r"),"MOLECULE.BASE:make_density_mx_from_NOs ... no restricted NO's")
         call make_DM_from_NOs(self%density_mx%r,self%NOs%r,self%NO_occ_nos%r)

      case ("u")
         ENSURE(is_allocated_with_genre_(self%NOs,"u"),"MOLECULE.BASE:make_density_mx_from_NOs ... no unrestricted NO's")
         call make_DM_from_NOs(self%density_mx%a,self%NOs%a,self%NO_occ_nos%a)
         call make_DM_from_NOs(self%density_mx%b,self%NOs%b,self%NO_occ_nos%b)

      case default
         DIE("MOLECULE.BASE:make_density_mx_from_NOs ... only genre 'r' allowed")
      end select

      ! Archive
      call set_(arch,self%name,"density_mx",genre)
      call write_(arch,self%density_mx)
   end subroutine

   PURE subroutine make_DM_from_NOs(DM,NOs,occs)
   ! Make the density mx "DM" from the "NOs" and their
   ! occupation numbers "occs".
      MAT(REAL,:,:), OUT :: DM
      MAT(REAL,:,:), IN :: NOs
      VEC(REAL,:), IN :: occs
      MAT(REAL,:,:), allocatable :: NO
      VEC(REAL,:), allocatable :: occ
      INT :: dim
      dim = size(occs)
      call create_(NO,dim,dim)
      occ = sqrt(occs)
      call to_product_with_diagonal_(NO,NOs,occ)
      DM = matmul(NO,transpose(NO))
      call destroy_(occ)
      call destroy_(NO)
   end subroutine


!  ===================
!  Overlap matrix sqrt
!  ===================

   subroutine make_r_overlap_sqrt(self,S_sqrt)
   ! Get "S_sqrt", the sqrt overlap matrix
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: S_sqrt
      REAL :: tol
   ENSURE(allocated(self%overlap_mx_evecs),"MOLECULE.BASE:make_r_overlap_sqrt ... no overlap_mx_evecs")
   ENSURE(allocated(self%overlap_mx_evals),"MOLECULE.BASE:make_r_overlap_sqrt ... no overlap_values")
   ENSURE(is_square_(S_sqrt),"MOLECULE.BASE:make_r_overlap_sqrt ... non-square S_qrt")
   ENSURE(size(S_sqrt,1)==size(self%overlap_mx_evecs,1),"MOLECULE.BASE:make_r_overlap_sqrt ... wrong shape for S_sqrt")
      if (allocated(self%SCF_data)) then
         tol = self%SCF_data%linear_dependence_tol
         call to_sqrt_of_(S_sqrt,self%overlap_mx_evals,self%overlap_mx_evecs,tol)
      else
         call to_sqrt_of_(S_sqrt,self%overlap_mx_evals,self%overlap_mx_evecs)
      end if
   end subroutine

   PURE subroutine make_r_overlap_inverse_sqrt(self,S_inv)
   ! Get "S_inv", the inverse sqrt overlap matrix
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: S_inv
      REAL :: tol
   ENSURE(allocated(self%overlap_mx_evecs),"MOLECULE.BASE:make_r_overlap_inverse_sqrt ... no overlap_mx_evecs")
   ENSURE(allocated(self%overlap_mx_evals),"MOLECULE.BASE:make_r_overlap_inverse_sqrt ... no overlap_values")
   ENSURE(is_square_(S_inv),"MOLECULE.BASE:make_r_overlap_inverse_sqrt ... non-square S_inv")
   ENSURE(size(S_inv,1)==size(self%overlap_mx_evecs,1),"MOLECULE.BASE:make_r_overlap_inverse_sqrt ... wrong shape for S_sqrt")
      if (allocated(self%SCF_data)) then
         tol = self%SCF_data%linear_dependence_tol
         call to_inverse_sqrt_of_(S_inv,self%overlap_mx_evals,self%overlap_mx_evecs,tol)
      else
         call to_inverse_sqrt_of_(S_inv,self%overlap_mx_evals,self%overlap_mx_evecs)
      end if
   end subroutine


!  ==========================
!  Natural orbital assignment
!  ==========================

   subroutine assign_NOs_to_MOs(self)
   ! Copy the natural orbitals to be the MOs.
   ! If the MO's are not there, try and read them from disk.
   ! The occupation numbers are the MO ones.
      type(MOLECULE_TYPE), target, INOUT :: self
      type(ARCHIVE_TYPE) :: arch
      STR(len=STR_SIZE) :: genre
      INT :: g
   ENSURE(allocated(self%MOs),"MOLECULE.BASE:assign_NOs_to_MOs ... no molecular orbitals")
   ENSURE(has_any_genre_(self%MOs),"MOLECULE.BASE:assign_NOs_to_MOs ... no molecular orbitals")

      ! Get MOs
      if (allocated(self%MOs)) then

         ! Already there
         genre = genre_(self%MOs)
      else if (allocated(self%SCF_data)) then

         ! Does it exist on disk?
         genre = spinorbital_genre_(self%SCF_data)
         call set_(arch,self%name,"MOs",genre)
         if (exists_(arch)) then
            call create_(self%MOs,self%n_bf)
            call read_(arch,self%MOs)
            call unnormalize_(self%MOs,"1",self%first_basis_fn_for_shell,self%last_basis_fn_for_shell,self%angular_moment_for_shell)
            call close_(arch)
         end if
      end if

      ! Clone the MO's
      if (allocated(self%MOs)) then
         self%NOs = self%MOs
         call create_(self%NO_occ_nos,self%n_bf,genre)
         call set_to_zero_(self%NO_occ_nos)
         call set_SCF_occupations_for_NOs_(self,genre)
      end if
      if (allocated(self%atom_group)) then
         if (allocated(self%mol)) then
            do g = 1,size(self%atom_group)

            ! Should be generalised to read archives like above ...
               if (NOT allocated(self%mol(g)%MOs)) cycle
               genre = genre_(self%mol(g)%MOs)
               self%mol(g)%NOs = self%mol(g)%MOs
               call create_(self%mol(g)%NO_occ_nos,self%mol(g)%n_bf,genre)
               call set_to_zero_(self%mol(g)%NO_occ_nos)
               call set_SCF_occupations_for_NOs_(self%mol(g),genre)
            end do
         end if
      end if
   end subroutine

   subroutine assign_MOs_to_NOs(self)
   ! Copy the MOs to be the same as the natural orbitals
   ! The occupation numbers are the MO ones.
      type(MOLECULE_TYPE), target, INOUT :: self
      STR(len=STR_SIZE) :: genre
      INT :: g

      ! Clone
      if (allocated(self%NOs)) then
         genre = genre_(self%NOs)
         self%MOs = self%NOs
         call create_(self%NO_occ_nos,self%n_bf,genre)
         call set_to_zero_(self%NO_occ_nos)
         call set_SCF_occupations_for_NOs_(self,genre)
      end if
      if (allocated(self%atom_group)) then
         if (allocated(self%mol)) then
            do g = 1,size(self%atom_group)
               if (NOT allocated(self%mol(g)%NOs)) cycle
               genre = genre_(self%NOs)
               self%mol(g)%MOs = self%mol(g)%NOs
               call create_(self%mol(g)%NO_occ_nos,self%n_bf,genre)
               call set_to_zero_(self%mol(g)%NO_occ_nos)
               call set_SCF_occupations_for_NOs_(self%mol(g),genre)
            end do
         end if
      end if
   end subroutine


!  ================
!  Symmetry methods
!  ================

   subroutine symmetrize_0(self,mat,orb_kind)
   ! Symmetrize an opmatrix matrix "mat" and of spinorbital_genre
   ! "orb_kind" (if specified)
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), INOUT :: mat
      STR(len=*), optional, IN :: orb_kind
      STR(len=STR_SIZE) :: itemkind
      itemkind = genre_(mat)
      if (present(orb_kind)) itemkind = orb_kind
      select case (itemkind)
      case ("r ")
         call symmetrize_(self,mat%r)
      case ("u ")
         call symmetrize_(self,mat%a)
         call symmetrize_(self,mat%b)
      case ("gc")
         call symmetrize_(self,aa_block_ptr_(mat%gc))
         call symmetrize_(self,bb_block_ptr_(mat%gc))
         call symmetrize_(self,ab_block_ptr_(mat%gc))
         call symmetrize_(self,ba_block_ptr_(mat%gc))
      end select
   end subroutine

   subroutine symmetrize_1(self,mat)
   ! Symmetrize a matrix
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:) :: mat
      MAT(REAL,:,:), allocatable :: sym,W
      MAT(REAL,:,:), allocatable :: tra,trb
      INT :: n,a,b,i,j,na,nb
      INT :: fa,fb,fi,fj,la,lb,li,lj
      MAT(INT,:,:), allocatable :: image
   ENSURE(self%basis_info_made,"MOLECULE.BASE:symmetrize_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.BASE:symmetrize_1 ... no atom list")
   ENSURE(allocated(self%pointgroup),"MOLECULE.BASE:symmetrize_1 ... no pointgroup")
   ENSURE(is_square_(mat),"MOLECULE.BASE:symmetrize_1 ... non-square matrix")
   ENSURE(size(mat,1)==self%n_bf,"MOLECULE.BASE:symmetrize_1 ... incorrectly dimensioned matrix")

      ! The following code is inherited from MOLECULE.BASE
      call create_(image,self%n_shell,self%pointgroup%order)
      call make_pg_image_of_shell_(self,image)
      call create_(sym,self%n_bf,self%n_bf)
      sym = ZERO
      do n = 1,self%pointgroup%order
         do a = 1,self%n_shell
            tra = xyz_mx_(self%pointgroup,n,self%angular_moment_for_shell(a))
            i = image(a,n)
            fa = self%first_basis_fn_for_shell(a)
            fi = self%first_basis_fn_for_shell(i)
            la = self%last_basis_fn_for_shell(a)
            li = self%last_basis_fn_for_shell(i)
            na = la-fa+1
            do b = 1,self%n_shell
               trb = xyz_mx_(self%pointgroup,n,self%angular_moment_for_shell(b))
               j = image(b,n)
               fb = self%first_basis_fn_for_shell(b)
               fj = self%first_basis_fn_for_shell(j)
               lb = self%last_basis_fn_for_shell(b)
               lj = self%last_basis_fn_for_shell(j)
               nb = lb-fb+1
               call create_(W,na,nb)
               call to_product_of_(W,tra,mat(fa:la,fb:lb))
               call plus_product_of_(sym(fi:li,fj:lj),W,trb,transpose_b=TRUE)
               call destroy_(W)
            end do
         end do
      end do
      mat = sym/REALIFY(self%pointgroup%order)
      call destroy_(sym)
      call destroy_(image)
   end subroutine

   subroutine symmetrize_2(self,mat)
   ! Symmetrize a matrix
      type(MOLECULE_TYPE), IN :: self
      MAT(CPX,:,:) :: mat
      MAT(CPX,:,:), allocatable :: sym,W
      MAT(REAL,:,:), allocatable :: tra,trb
      INT :: n,a,b,i,j,na,nb
      INT :: fa,fb,fi,fj,la,lb,li,lj
      MAT(INT,:,:), allocatable :: image
   ENSURE(self%basis_info_made,"MOLECULE.BASE:symmetrize_2 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.BASE:symmetrize_2 ... no atom list")
   ENSURE(allocated(self%pointgroup),"MOLECULE.BASE:symmetrize_2 ... no pointgroup")
   ENSURE(is_square_(mat),"MOLECULE.BASE:symmetrize_2 ... non-square matrix")
   ENSURE(size(mat,1)==self%n_bf,"MOLECULE.BASE:symmetrize_2 ... incorrectly dimensioned matrix")

      ! The following code is inherited from MOLECULE.BASE
      call create_(image,self%n_shell,self%pointgroup%order)
      call make_pg_image_of_shell_(self,image)
      call create_(sym,self%n_bf,self%n_bf)
      sym = ZERO
      do n = 1,self%pointgroup%order
         do a = 1,self%n_shell
            tra = xyz_mx_(self%pointgroup,n,self%angular_moment_for_shell(a))
            i = image(a,n)
            fa = self%first_basis_fn_for_shell(a)
            fi = self%first_basis_fn_for_shell(i)
            la = self%last_basis_fn_for_shell(a)
            li = self%last_basis_fn_for_shell(i)
            na = la-fa+1
            do b = 1,self%n_shell
               trb = xyz_mx_(self%pointgroup,n,self%angular_moment_for_shell(b))
               j = image(b,n)
               fb = self%first_basis_fn_for_shell(b)
               fj = self%first_basis_fn_for_shell(j)
               lb = self%last_basis_fn_for_shell(b)
               lj = self%last_basis_fn_for_shell(j)
               nb = lb-fb+1
               call create_(W,na,nb)
               call to_product_of_(W,tra,mat(fa:la,fb:lb))
               call plus_product_of_(sym(fi:li,fj:lj),W,trb,transpose_b=TRUE)
               call destroy_(W)
            end do
         end do
      end do
      mat = sym/REALIFY(self%pointgroup%order)
      call destroy_(sym)
      call destroy_(image)
   end subroutine

   PURE subroutine make_pg_image_of_shell(self,res)
   ! Return an array "res(a,n)" which is the image of shell "a" under
   ! pointgroup operation "n"
   ! Size of "res" is [.n_shell,.pointgroup.order]
      type(MOLECULE_TYPE), IN :: self
      MAT(INT,:,:), OUT :: res
      VEC(REAL,3) :: new_pos
      INT :: n,a,aa,as,new_atom,fs
   ENSURE(allocated(self%pointgroup),"MOLECULE.BASE:make_pg_image_of_shell ... no pointgroup")
   ENSURE(size(res,1)==self%n_shell,"MOLECULE.BASE:make_pg_image_of_shell ... wrong dim2, res")
   ENSURE(size(res,2)==self%pointgroup%order,"MOLECULE.BASE:make_pg_image_of_shell ... wrong dime2, res")
      do n = 1,self%pointgroup%order
         do a = 1,self%n_shell
            aa = self%atom_for_shell(a)
            as = self%atom_shell_for_shell(a)
            call to_product_of_(new_pos,self%pointgroup%mx(:,:,n),self%atom(aa)%position)
            new_atom = atom_index_from_position_(self%atom,new_pos)
            fs = self%first_shell_for_atom(new_atom)
            res(a,n) = fs+as-1
         end do
      end do
   end subroutine

   PURE subroutine symmetrize_DM_like_mx_0(self,DM)
   ! Symmetrize a density matrix-like object "DM" using the crystal
   ! seitz operators. 
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: DM
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: tr
      MAT(REAL,:,:), allocatable :: sym,blk
      MAT(REAL,:,:), allocatable :: tra,trb
      MAT(INT,:,:), allocatable :: imshcnt
      INT :: g
      INT :: a,fa,la,ma
      INT :: b,fb,lb,mb
      INT :: i,fi,li
      INT :: j,fj,lj
      MAT3(REAL,:,:,:), allocatable :: symops
      MAT(INT,:,:), allocatable :: image
   ENSURE(self%basis_info_made,"MOLECULE.BASE:symmetrize_DM_like_mx_0 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.BASE:symmetrize_DM_like_mx_0 ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.BASE:symmetrize_DM_like_mx_0 ... No crystal")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.BASE:symmetrize_DM_like_mx_0 ... No crystal")
   ENSURE(is_square_(DM),"MOLECULE.BASE:symmetrize_DM_like_mx_0 ... non-square matrix")
   ENSURE(size(DM,1)==self%n_bf,"MOLECULE.BASE:symmetrize_DM_like_mx_0 ... incorrectly dimensioned matrix")

      ! Symops

      ! The following code is inherited from MOLECULE.BASE
      symops = xyz_seitz_matrices_(self%crystal)

      ! Make image shell map
      call create_(image,self%n_shell,self%crystal%spacegroup%n_seitz)
      call make_sg_image_of_shell_(self,image)

      ! Rotated and accumulated SM coeffcients
      call create_(sym,self%n_bf,self%n_bf)
      sym = DM

   ! stdout.text("DM:")
   ! stdout.put(DM)

      ! Counter for image-shell blocks
      call create_(imshcnt,self%n_shell,self%n_shell)
      imshcnt = 1

      ! Zero lower-triangle symmetry-dependent blocks
      ! Reset image shell block counts
   ! do a = 1,.n_shell
   ! do b = 1,a
   !    
   !    atom_a = .atom_for_shell(a)
   !    atom_b = .atom_for_shell(b)

   !    !  Unique atom block?
   !    ua = any(atom_a==.crystal.unique_frag_atom) 
   !    ub = any(atom_b==.crystal.unique_frag_atom) 

   !    fa = .first_basis_fn_for_shell(a)
   !    la = .last_basis_fn_for_shell(a)
   !    fb = .first_basis_fn_for_shell(b)
   !    lb = .last_basis_fn_for_shell(b)
   !    
   !    if (NOT ua AND NOT ub) then
   !       ! Zero block: At least one of (a,b) is 
   !       ! related to a unique fragment atom.
   !       sym(fa:la,fb:lb) = ZERO
   !       sym(fb:lb,fa:la) = ZERO
   !    else
   !       ! Count block & copy
   !       imshcnt(a,b) = 1
   !       imshcnt(b,a) = 1
   !       sym(fa:la,fb:lb) = DM(fa:la,fb:lb)
   !       sym(fb:lb,fa:la) = DM(fb:lb,fa:la)
   !    end

   ! end
   ! end

      ! Generate Symmetry-dependent blocks
      do a = 1,self%n_shell

      ! ! Only do unique (asym-atom-corresponding) frag atoms
      ! atom_a = .atom_for_shell(a)
      ! ua = any(atom_a==.crystal.unique_frag_atom)
      ! if (NOT ua) cycle
         fa = self%first_basis_fn_for_shell(a)
         la = self%last_basis_fn_for_shell(a)
         ma = self%angular_moment_for_shell(a)
         do b = 1,a

            ! Do any atom for shell b
            fb = self%first_basis_fn_for_shell(b)
            lb = self%last_basis_fn_for_shell(b)
            mb = self%angular_moment_for_shell(b)
            do g = 2,self%crystal%spacegroup%n_seitz
               i = image(a,g)
               j = image(b,g)
               if (i==0 OR j==0) cycle
               fi = self%first_basis_fn_for_shell(i)
               li = self%last_basis_fn_for_shell(i)
               fj = self%first_basis_fn_for_shell(j)
               lj = self%last_basis_fn_for_shell(j)
               call make_gaussian_xyz_matrices_(tr,symops(:,:,g))
               tra = tr(ma)%element(:,:)
               trb = transpose(tr(mb)%element(:,:))
               blk = matmul(matmul(tra,DM(fa:la,fb:lb)),trb)
               sym(fi:li,fj:lj) = sym(fi:li,fj:lj)+blk
               sym(fj:lj,fi:li) = sym(fj:lj,fi:li)+transpose(blk)
               imshcnt(i,j) = imshcnt(i,j)+1
               imshcnt(j,i) = imshcnt(j,i)+1
               call destroy_(tr)
            end do
         end do
      end do

      ! Divide unique blocks by symmetrisation factors
      do a = 1,self%n_shell
         fa = self%first_basis_fn_for_shell(a)
         la = self%last_basis_fn_for_shell(a)
         do b = 1,self%n_shell
            fb = self%first_basis_fn_for_shell(b)
            lb = self%last_basis_fn_for_shell(b)
            if (imshcnt(a,b)>0) then
               sym(fa:la,fb:lb) = sym(fa:la,fb:lb)/imshcnt(a,b)
            end if
         end do
      end do

      ! Reset image shell block counts
      ! for symmetry-dependent blocks
   ! imshcnt = 0
   ! do a = 1,.n_shell

   !    atom_a = .atom_for_shell(a)
   !    ua = any(atom_a==.crystal.unique_frag_atom) 
   !    fa = .first_basis_fn_for_shell(a)
   !    la = .last_basis_fn_for_shell(a)

   !    do b = 1,a
   !    
   !       atom_b = .atom_for_shell(b)
   !       ub = any(atom_b==.crystal.unique_frag_atom) 
   !       fb = .first_basis_fn_for_shell(b)
   !       lb = .last_basis_fn_for_shell(b)
   !      
   !       !  Unique atom block?
   !       if (NOT ua AND NOT ub) then
   !       if (imshcnt(a,b)==0) then
   !          ! Do block this time
   !          imshcnt(a,b) = 1
   !          imshcnt(b,a) = 1
   !          sym(fa:la,fb:lb) = DM(fa:la,fb:lb)
   !          sym(fb:lb,fa:la) = DM(fb:lb,fa:la)
   !       end
   !       end

   !    end

   ! end

   ! ! Symmetrise dependent blocks amonst only themselves
   ! ! This is not ideal: we probably want to copy out from
   ! ! the unique blocks. Maybe fiddle with the image array?
   ! do a = 1,.n_shell

   !    ! Only do unique (asym-atom-corresponding) frag atoms
   !    atom_a = .atom_for_shell(a)
   !    ua = any(atom_a==.crystal.unique_frag_atom)
   !    if (ua) cycle

   !    fa = .first_basis_fn_for_shell(a)
   !    la = .last_basis_fn_for_shell(a)
   !    ma = .angular_moment_for_shell(a)

   !    do b = 1,a

   !       atom_b = .atom_for_shell(b)
   !       ub = any(atom_b==.crystal.unique_frag_atom)
   !       if (ub) cycle

   !       ! Do any atom for shell b
   !       fb = .first_basis_fn_for_shell(b)
   !       lb = .last_basis_fn_for_shell(b)
   !       mb = .angular_moment_for_shell(b)

   !       do g = 2,.crystal.spacegroup.n_seitz

   !          i = image(a,g)
   !          j = image(b,g)

   !          if (i==0 OR j==0) cycle

   !          !  Unique image atom block?
   !          atom_i = .atom_for_shell(i)
   !          atom_j = .atom_for_shell(j)

   !          ui = any(atom_i==.crystal.unique_frag_atom) 
   !          uj = any(atom_j==.crystal.unique_frag_atom) 

   !          if (NOT ui AND NOT uj) then

   !             fi = .first_basis_fn_for_shell(i)
   !             li =  .last_basis_fn_for_shell(i)
   !             fj = .first_basis_fn_for_shell(j)
   !             lj =  .last_basis_fn_for_shell(j)
   !            
   !             GAUSSIAN_DATA:make_gaussian_xyz_matrices(tr,symops(:,:,g))
   !            
   !             tra = tr(ma)[:,:]
   !             trb = transpose(tr(mb)[:,:])
   !             blk = matmul( matmul(tra,DM(fa:la,fb:lb)), trb)
   !            
   !             sym(fi:li,fj:lj) = sym(fi:li,fj:lj) + blk
   !             sym(fj:lj,fi:li) = sym(fj:lj,fi:li) + transpose(blk)
   !            
   !             imshcnt(i,j) = imshcnt(i,j) + 1
   !             imshcnt(j,i) = imshcnt(j,i) + 1
   !            
   !             tr.destroy

   !          end

   !       end

   !    end

   ! end

   ! ! Divide unique blocks by symmetrisation factors
   ! do a = 1,.n_shell
   !    atom_a = .atom_for_shell(a)
   !    ua = any(atom_a==.crystal.unique_frag_atom) 
   !    fa = .first_basis_fn_for_shell(a)
   !    la = .last_basis_fn_for_shell(a)
   !    do b = 1,.n_shell
   !       atom_b = .atom_for_shell(b)
   !       ub = any(atom_b==.crystal.unique_frag_atom) 
   !       fb = .first_basis_fn_for_shell(b)
   !       lb = .last_basis_fn_for_shell(b)
   !       if (NOT ua AND NOT ub) then
   !       if (imshcnt(a,b)>0) then
   !          sym(fa:la,fb:lb) = sym(fa:la,fb:lb)/imshcnt(a,b)
   !       end
   !       end
   !    end
   ! end
      DM = sym

   ! stdout.text("DM 2:")
   ! stdout.put(DM)

      ! Clean
      call destroy_(sym)
      call destroy_(image)
      call destroy_(imshcnt)
      call destroy_(symops)
   end subroutine

   PURE subroutine symmetrize_DM_like_mx_1(self,DM)
   ! Symmetrize a density matrix-like object "DM" using the crystal
   ! seitz operators. 
      type(MOLECULE_TYPE), IN :: self
      MAT(CPX,:,:), INOUT :: DM
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: tr
      MAT(CPX,:,:), allocatable :: sym,blk
      MAT(REAL,:,:), allocatable :: tra,trb
      MAT(INT,:,:), allocatable :: imshcnt
      INT :: g
      INT :: a,fa,la,ma
      INT :: b,fb,lb,mb
      INT :: i,fi,li
      INT :: j,fj,lj
      MAT3(REAL,:,:,:), allocatable :: symops
      MAT(INT,:,:), allocatable :: image
   ENSURE(self%basis_info_made,"MOLECULE.BASE:symmetrize_DM_like_mx_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.BASE:symmetrize_DM_like_mx_1 ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.BASE:symmetrize_DM_like_mx_1 ... No crystal")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.BASE:symmetrize_DM_like_mx_1 ... No crystal")
   ENSURE(is_square_(DM),"MOLECULE.BASE:symmetrize_DM_like_mx_1 ... non-square matrix")
   ENSURE(size(DM,1)==self%n_bf,"MOLECULE.BASE:symmetrize_DM_like_mx_1 ... incorrectly dimensioned matrix")

      ! Symops

      ! The following code is inherited from MOLECULE.BASE
      symops = xyz_seitz_matrices_(self%crystal)

      ! Make image shell map
      call create_(image,self%n_shell,self%crystal%spacegroup%n_seitz)
      call make_sg_image_of_shell_(self,image)

      ! Rotated and accumulated SM coeffcients
      call create_(sym,self%n_bf,self%n_bf)
      sym = DM

   ! stdout.text("DM:")
   ! stdout.put(DM)

      ! Counter for image-shell blocks
      call create_(imshcnt,self%n_shell,self%n_shell)
      imshcnt = 1

      ! Zero lower-triangle symmetry-dependent blocks
      ! Reset image shell block counts
   ! do a = 1,.n_shell
   ! do b = 1,a
   !    
   !    atom_a = .atom_for_shell(a)
   !    atom_b = .atom_for_shell(b)

   !    !  Unique atom block?
   !    ua = any(atom_a==.crystal.unique_frag_atom) 
   !    ub = any(atom_b==.crystal.unique_frag_atom) 

   !    fa = .first_basis_fn_for_shell(a)
   !    la = .last_basis_fn_for_shell(a)
   !    fb = .first_basis_fn_for_shell(b)
   !    lb = .last_basis_fn_for_shell(b)
   !    
   !    if (NOT ua AND NOT ub) then
   !       ! Zero block: At least one of (a,b) is 
   !       ! related to a unique fragment atom.
   !       sym(fa:la,fb:lb) = ZERO
   !       sym(fb:lb,fa:la) = ZERO
   !    else
   !       ! Count block & copy
   !       imshcnt(a,b) = 1
   !       imshcnt(b,a) = 1
   !       sym(fa:la,fb:lb) = DM(fa:la,fb:lb)
   !       sym(fb:lb,fa:la) = DM(fb:lb,fa:la)
   !    end

   ! end
   ! end

      ! Generate Symmetry-dependent blocks
      do a = 1,self%n_shell

      ! ! Only do unique (asym-atom-corresponding) frag atoms
      ! atom_a = .atom_for_shell(a)
      ! ua = any(atom_a==.crystal.unique_frag_atom)
      ! if (NOT ua) cycle
         fa = self%first_basis_fn_for_shell(a)
         la = self%last_basis_fn_for_shell(a)
         ma = self%angular_moment_for_shell(a)
         do b = 1,a

            ! Do any atom for shell b
            fb = self%first_basis_fn_for_shell(b)
            lb = self%last_basis_fn_for_shell(b)
            mb = self%angular_moment_for_shell(b)
            do g = 2,self%crystal%spacegroup%n_seitz
               i = image(a,g)
               j = image(b,g)
               if (i==0 OR j==0) cycle
               fi = self%first_basis_fn_for_shell(i)
               li = self%last_basis_fn_for_shell(i)
               fj = self%first_basis_fn_for_shell(j)
               lj = self%last_basis_fn_for_shell(j)
               call make_gaussian_xyz_matrices_(tr,symops(:,:,g))
               tra = tr(ma)%element(:,:)
               trb = transpose(tr(mb)%element(:,:))
               blk = matmul(matmul(tra,DM(fa:la,fb:lb)),trb)
               sym(fi:li,fj:lj) = sym(fi:li,fj:lj)+blk
               sym(fj:lj,fi:li) = sym(fj:lj,fi:li)+transpose(blk)
               imshcnt(i,j) = imshcnt(i,j)+1
               imshcnt(j,i) = imshcnt(j,i)+1
               call destroy_(tr)
            end do
         end do
      end do

      ! Divide unique blocks by symmetrisation factors
      do a = 1,self%n_shell
         fa = self%first_basis_fn_for_shell(a)
         la = self%last_basis_fn_for_shell(a)
         do b = 1,self%n_shell
            fb = self%first_basis_fn_for_shell(b)
            lb = self%last_basis_fn_for_shell(b)
            if (imshcnt(a,b)>0) then
               sym(fa:la,fb:lb) = sym(fa:la,fb:lb)/imshcnt(a,b)
            end if
         end do
      end do

      ! Reset image shell block counts
      ! for symmetry-dependent blocks
   ! imshcnt = 0
   ! do a = 1,.n_shell

   !    atom_a = .atom_for_shell(a)
   !    ua = any(atom_a==.crystal.unique_frag_atom) 
   !    fa = .first_basis_fn_for_shell(a)
   !    la = .last_basis_fn_for_shell(a)

   !    do b = 1,a
   !    
   !       atom_b = .atom_for_shell(b)
   !       ub = any(atom_b==.crystal.unique_frag_atom) 
   !       fb = .first_basis_fn_for_shell(b)
   !       lb = .last_basis_fn_for_shell(b)
   !      
   !       !  Unique atom block?
   !       if (NOT ua AND NOT ub) then
   !       if (imshcnt(a,b)==0) then
   !          ! Do block this time
   !          imshcnt(a,b) = 1
   !          imshcnt(b,a) = 1
   !          sym(fa:la,fb:lb) = DM(fa:la,fb:lb)
   !          sym(fb:lb,fa:la) = DM(fb:lb,fa:la)
   !       end
   !       end

   !    end

   ! end

   ! ! Symmetrise dependent blocks amonst only themselves
   ! ! This is not ideal: we probably want to copy out from
   ! ! the unique blocks. Maybe fiddle with the image array?
   ! do a = 1,.n_shell

   !    ! Only do unique (asym-atom-corresponding) frag atoms
   !    atom_a = .atom_for_shell(a)
   !    ua = any(atom_a==.crystal.unique_frag_atom)
   !    if (ua) cycle

   !    fa = .first_basis_fn_for_shell(a)
   !    la = .last_basis_fn_for_shell(a)
   !    ma = .angular_moment_for_shell(a)

   !    do b = 1,a

   !       atom_b = .atom_for_shell(b)
   !       ub = any(atom_b==.crystal.unique_frag_atom)
   !       if (ub) cycle

   !       ! Do any atom for shell b
   !       fb = .first_basis_fn_for_shell(b)
   !       lb = .last_basis_fn_for_shell(b)
   !       mb = .angular_moment_for_shell(b)

   !       do g = 2,.crystal.spacegroup.n_seitz

   !          i = image(a,g)
   !          j = image(b,g)

   !          if (i==0 OR j==0) cycle

   !          !  Unique image atom block?
   !          atom_i = .atom_for_shell(i)
   !          atom_j = .atom_for_shell(j)

   !          ui = any(atom_i==.crystal.unique_frag_atom) 
   !          uj = any(atom_j==.crystal.unique_frag_atom) 

   !          if (NOT ui AND NOT uj) then

   !             fi = .first_basis_fn_for_shell(i)
   !             li =  .last_basis_fn_for_shell(i)
   !             fj = .first_basis_fn_for_shell(j)
   !             lj =  .last_basis_fn_for_shell(j)
   !            
   !             GAUSSIAN_DATA:make_gaussian_xyz_matrices(tr,symops(:,:,g))
   !            
   !             tra = tr(ma)[:,:]
   !             trb = transpose(tr(mb)[:,:])
   !             blk = matmul( matmul(tra,DM(fa:la,fb:lb)), trb)
   !            
   !             sym(fi:li,fj:lj) = sym(fi:li,fj:lj) + blk
   !             sym(fj:lj,fi:li) = sym(fj:lj,fi:li) + transpose(blk)
   !            
   !             imshcnt(i,j) = imshcnt(i,j) + 1
   !             imshcnt(j,i) = imshcnt(j,i) + 1
   !            
   !             tr.destroy

   !          end

   !       end

   !    end

   ! end

   ! ! Divide unique blocks by symmetrisation factors
   ! do a = 1,.n_shell
   !    atom_a = .atom_for_shell(a)
   !    ua = any(atom_a==.crystal.unique_frag_atom) 
   !    fa = .first_basis_fn_for_shell(a)
   !    la = .last_basis_fn_for_shell(a)
   !    do b = 1,.n_shell
   !       atom_b = .atom_for_shell(b)
   !       ub = any(atom_b==.crystal.unique_frag_atom) 
   !       fb = .first_basis_fn_for_shell(b)
   !       lb = .last_basis_fn_for_shell(b)
   !       if (NOT ua AND NOT ub) then
   !       if (imshcnt(a,b)>0) then
   !          sym(fa:la,fb:lb) = sym(fa:la,fb:lb)/imshcnt(a,b)
   !       end
   !       end
   !    end
   ! end
      DM = sym

   ! stdout.text("DM 2:")
   ! stdout.put(DM)

      ! Clean
      call destroy_(sym)
      call destroy_(image)
      call destroy_(imshcnt)
      call destroy_(symops)
   end subroutine

   PURE subroutine make_sg_image_of_shell(self,res)
   ! Return an array "res(s,g)" which is the image of shell "s" under
   ! *spacegroup* operation "g" ignoring whole cell translations.
   ! Size of "res" is [.n_shell,.crystal.spacegroup.n_seitz]
      type(MOLECULE_TYPE), IN :: self
      MAT(INT,:,:), OUT :: res
      VEC(REAL,3) :: pg
      INT :: g,s,as,sa,ag,fg
   ENSURE(allocated(self%crystal),"MOLECULE.BASE:make_sg_image_of_shell ... no crystal")
   ENSURE(self%crystal%spacegroup%analyzed,"MOLECULE.BASE:make_sg_image_of_shell ... crystal spacegroup not analyzed")
   ENSURE(size(res,1)==self%n_shell,"MOLECULE.BASE:make_sg_image_of_shell ... wrong dim2, res")
   ENSURE(size(res,2)==self%crystal%spacegroup%n_seitz,"MOLECULE.BASE:make_sg_image_of_shell ... wrong dim2, res")
      res = ZERO
      do g = 1,self%crystal%spacegroup%n_seitz
         do s = 1,self%n_shell

         ! Get transformed atom position
            as = self%atom_for_shell(s)
            pg = self%atom(as)%position
            call transform_xyz_position_(self%crystal,pg,g,to_unit_cell=TRUE)

         ! Get index of rotated atom by its position
            ag = atom_index_from_position_(self%atom,pg)

         ! Assign
            if (ag==0) then
               res(s,g) = 0
            else
               fg = self%first_shell_for_atom(ag)
               sa = self%atom_shell_for_shell(s)
               res(s,g) = fg+sa-1
            end if
         end do
      end do
   end subroutine

   PURE subroutine stabilize_0(self,mat)
   ! Stabilize a matrix
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: mat
      MAT(REAL,:,:), allocatable :: sym,w
      MAT(REAL,:,:), allocatable :: tra,trb
      INT :: n,a,b,i,j,na,nb
      INT :: fa,fb,fi,fj,la,lb,li,lj
      MAT(INT,:,:), allocatable :: image
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.BASE:stabilize_0 ... not implemented")
   ENSURE(self%basis_info_made,"MOLECULE.BASE:stabilize_0 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.BASE:stabilize_0 ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.BASE:stabilize_0 ... no pointgroup")
   ENSURE(is_square_(mat),"MOLECULE.BASE:stabilize_0 ... non-square matrix")
   ENSURE(size(mat,1)==self%n_bf,"MOLECULE.BASE:stabilize_0 ... incorrectly dimensioned matrix")

      ! The following code is inherited from MOLECULE.BASE
      call create_(image,self%n_shell,self%crystal%n_stabilizer_symops)
      call make_image_of_stabilizer_(self,image)
      call create_(sym,self%n_bf,self%n_bf)
      sym = ZERO
      do n = 1,self%crystal%n_stabilizer_symops
         do a = 1,self%n_shell
            tra = xyz_stabilizer_mx_(self%crystal,n,self%angular_moment_for_shell(a))
            i = image(a,n)
            fa = self%first_basis_fn_for_shell(a)
            fi = self%first_basis_fn_for_shell(i)
            la = self%last_basis_fn_for_shell(a)
            li = self%last_basis_fn_for_shell(i)
            na = la-fa+1
            do b = 1,self%n_shell
               trb = xyz_stabilizer_mx_(self%crystal,n,self%angular_moment_for_shell(b))
               j = image(b,n)
               fb = self%first_basis_fn_for_shell(b)
               fj = self%first_basis_fn_for_shell(j)
               lb = self%last_basis_fn_for_shell(b)
               lj = self%last_basis_fn_for_shell(j)
               nb = lb-fb+1
               call create_(w,na,nb)
               call to_product_of_(w,tra,mat(fa:la,fb:lb))
               call plus_product_of_(sym(fi:li,fj:lj),w,trb,transpose_b=TRUE)
               call destroy_(w)
            end do
         end do
      end do
      mat = sym/REALIFY(self%crystal%n_stabilizer_symops)
      call destroy_(sym)
      call destroy_(image)
   end subroutine

   PURE subroutine stabilize_1(self,mat)
   ! Stabilize a matrix
      type(MOLECULE_TYPE), IN :: self
      MAT(CPX,:,:), INOUT :: mat
      MAT(CPX,:,:), allocatable :: sym,w
      MAT(REAL,:,:), allocatable :: tra,trb
      INT :: n,a,b,i,j,na,nb
      INT :: fa,fb,fi,fj,la,lb,li,lj
      MAT(INT,:,:), allocatable :: image
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.BASE:stabilize_1 ... not implemented")
   ENSURE(self%basis_info_made,"MOLECULE.BASE:stabilize_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.BASE:stabilize_1 ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.BASE:stabilize_1 ... no pointgroup")
   ENSURE(is_square_(mat),"MOLECULE.BASE:stabilize_1 ... non-square matrix")
   ENSURE(size(mat,1)==self%n_bf,"MOLECULE.BASE:stabilize_1 ... incorrectly dimensioned matrix")

      ! The following code is inherited from MOLECULE.BASE
      call create_(image,self%n_shell,self%crystal%n_stabilizer_symops)
      call make_image_of_stabilizer_(self,image)
      call create_(sym,self%n_bf,self%n_bf)
      sym = ZERO
      do n = 1,self%crystal%n_stabilizer_symops
         do a = 1,self%n_shell
            tra = xyz_stabilizer_mx_(self%crystal,n,self%angular_moment_for_shell(a))
            i = image(a,n)
            fa = self%first_basis_fn_for_shell(a)
            fi = self%first_basis_fn_for_shell(i)
            la = self%last_basis_fn_for_shell(a)
            li = self%last_basis_fn_for_shell(i)
            na = la-fa+1
            do b = 1,self%n_shell
               trb = xyz_stabilizer_mx_(self%crystal,n,self%angular_moment_for_shell(b))
               j = image(b,n)
               fb = self%first_basis_fn_for_shell(b)
               fj = self%first_basis_fn_for_shell(j)
               lb = self%last_basis_fn_for_shell(b)
               lj = self%last_basis_fn_for_shell(j)
               nb = lb-fb+1
               call create_(w,na,nb)
               call to_product_of_(w,tra,mat(fa:la,fb:lb))
               call plus_product_of_(sym(fi:li,fj:lj),w,trb,transpose_b=TRUE)
               call destroy_(w)
            end do
         end do
      end do
      mat = sym/REALIFY(self%crystal%n_stabilizer_symops)
      call destroy_(sym)
      call destroy_(image)
   end subroutine

   PURE subroutine make_image_of_stabilizer(self,res)
   ! Return an array "res(a,n)" which is the image of shell "a" under
   ! a crystal stabilizer operation "n"
   ! Size of "res" is [.n_shell,.pointgroup.order]
      type(MOLECULE_TYPE), IN :: self
      MAT(INT,:,:), OUT :: res
      VEC(REAL,3) :: new_pos
      INT :: n,a,aa,as,new_atom,fs
   ENSURE(allocated(self%crystal),"MOLECULE.BASE:make_image_of_stabilizer ... no crystal")
   ENSURE(size(res,1)==self%n_shell,"MOLECULE.BASE:make_image_of_stabilizer ... wromg dim1, res")
   ENSURE(size(res,2)==self%crystal%n_stabilizer_symops,"MOLECULE.BASE:make_image_of_stabilizer ... wromg dim2, res")
      do n = 1,self%crystal%n_stabilizer_symops
         do a = 1,self%n_shell
            aa = self%atom_for_shell(a)
            as = self%atom_shell_for_shell(a)
            new_pos = matmul(self%crystal%ptr(:,:,n),self%atom(aa)%position)
            new_atom = atom_index_from_position_(self%atom,new_pos)
            fs = self%first_shell_for_atom(new_atom)
            res(a,n) = fs+as-1
         end do
      end do
   end subroutine

end module
