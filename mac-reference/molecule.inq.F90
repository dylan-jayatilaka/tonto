!===============================================================================
!
! MOLECULE.INQ
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
! Hierarchic order:
!    INQ
!    SET   ...  INQ 20  SET 83
!    PUT   ...  INQ  3  SET  0  PUT 46
!    BASE  ...  INQ  3  SET 18 [PUT  1] BASE 62
!    REL   ... 
!    INTS  ...  INQ  4  SET  16 PUT  0  BASE 32  [REL  3] INTS 14
!    XTAL  ...  INQ  1  SET  20 PUT  0 [BASE  7] [REL 29] INTS  0  XTAL 40
!    GRID  ...  INQ 35  SET  74 PUT 34  BASE  7  [REL 97] INTS  0  XTAL  0  GRID 367
!    FOCK  ...  INQ  0  SET 101 PUT  0  BASE  0  [REL  0] INTS  5  XTAL  0  GRID   4
!    SCF   ...  INQ 21  SET  28 PUT 21  BASE 49  [REL  5] INTS 40  XTAL  7  GRID  10 FOCK  6  SCF 25
!    PROP  ...  INQ 11  SET  11 PUT  2  BASE 43  [REL  0] INTS 40  XTAL  7  GRID  10 FOCK 17  SCF 30
!    CP    ...  INQ  0  SET   0 PUT  0  BASE  3  [REL  0] INTS  8  XTAL  0  GRID   5 FOCK  4  SCF  1 PROP  0PLOT  0
!    PLOT  ...  INQ  0  SET   7 PUT  2  BASE  9  [REL  0] INTS  3 [XTAL  1] GRID  13 FOCK  0  SCF  5 PROP  0  CP  0
!    TAD   ...  INQ  0  SET  11 PUT 12  BASE  2  [REL  0] INTS  0 [XTAL  1] GRID   8 FOCK  0  SCF 36 PROP  0  CP  0 PLOT  0
!    MISC  ...  INQ  2  SET  11 PUT  5  BASE 14  [REL  0] INTS 21 [XTAL  1] GRID   8 FOCK 15  SCF 13 PROP  0  CP  0 PLOT  1 TAD  2
!    MAIN  ...  INQ  0  SET   1 PUT 50  BASE 22  [REL  3] INTS  3  XTAL 14  GRID   2 FOCK  0  SCF 35 PROP 44  CP 16 PLOT 14 TAD 15 MISC 31
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
module MOLECULE_INQ_MODULE

#  include "macros"

   include "molecule.inq.use"

   implicit none

   include "molecule.inq.int"


contains

!  =========
!  Electrons
!  =========

   PURE function no_of_electrons(self) result (res)
   ! Work out and return the number of electrons in the molecule.
   ! NOTE: removed the core electrons, if any.
      type(MOLECULE_TYPE), IN :: self
      INT :: res
   ENSURE(allocated(self%atom),"MOLECULE.INQ:no_of_electrons ... no atom list")
      res = no_of_electrons_(self%atom)-self%charge-sum(self%atom(:)%n_core_electrons)
   end function

   PURE function no_of_alpha_electrons(self) result (res)
   ! Work out and return the number of alpha electrons in the molecule.
      type(MOLECULE_TYPE), IN :: self
      INT :: res
      res = no_of_electrons_(self)-no_of_beta_electrons_(self)
   end function

   PURE function no_of_beta_electrons(self) result (res)
   ! Work out and return the number of beta electrons in the molecule.
   ! Normally the number of beta electrons is smaller than alpha, but
   ! if the atom list is a single explicit (alpha) "El" electron it is
   ! assumed that there is one beta electron surrounding it. This is
   ! to ensure a good initial guess for promolecule SCF guess.
      type(MOLECULE_TYPE), IN :: self
      INT :: res
      INT :: tmp
   ENSURE(allocated(self%atom),"MOLECULE.INQ:no_of_beta_electrons ... no atom list")
      if (size(self%atom)==1 AND self%atom(1)%tag(1:2)=="El") then
         res = 1  ! Assume one beta electron surrounds the "El"
      else
         tmp = no_of_electrons_(self)-no_of_unpaired_electrons_(self)
         res = tmp/2
         ENSURE(is_even_(tmp),"MOLECULE.INQ:no_of_beta_electrons ... Inconsistent multiplicity, non integer # of beta electrons")
      end if
   end function

   PURE function has_valid_no_of_beta_electrons(self) result (res)
   ! Return TRUE if the number of beta electrons is valid based on the
   ! assigned charge and multiplicity i.e. it must come out integral
   ! *except* in the case when the atom list is a single explicit
   ! (alpha) "El" electron - in which case there must be one beta
   ! electron. See above.
      type(MOLECULE_TYPE), IN :: self
      BIN :: res
      INT :: tmp
   ENSURE(allocated(self%atom),"MOLECULE.INQ:has_valid_no_of_beta_electrons ... no atom list")
      if (size(self%atom)==1 AND self%atom(1)%tag(1:2)=="El") then
         tmp = no_of_electrons_(self)
         res = tmp==1  ! must have one beta electron
      else
         tmp = no_of_electrons_(self)-no_of_unpaired_electrons_(self)
         res = is_even_(tmp)
      end if
   end function

   PURE function no_of_unpaired_electrons(self) result (res)
   ! Work out and return the number of unpaired electrons in the molecule
      type(MOLECULE_TYPE), IN :: self
      INT :: res
      res = self%spin_multiplicity-1
      ENSURE(res<=no_of_electrons_(self),"MOLECULE.INQ:no_of_unpaired_electrons ... Wrong multiplicity, not enough electrons")
   end function


!  ==============
!  Atoms & shells
!  ==============

   PURE function no_of_atom_pairs(self) result (res)
   ! Return the number of atom pairs in the molecule
      type(MOLECULE_TYPE), IN :: self
      INT :: res
   ENSURE(allocated(self%atom),"MOLECULE.INQ:no_of_atom_pairs ... no atom list")
      res = self%n_atom*(self%n_atom+1)/2
   end function

   PURE function has_all_shell_info(self) result (res)
   ! Return TRUE if the per-atom interpolators are made for every atom.
      type(MOLECULE_TYPE), IN :: self
      BIN :: res
      res = allocated(self%atom)
      if (res) res = has_all_shell_info_(self%atom)
   end function


!  =========================
!  NOs, ANOs & interpolators
!  =========================

   PURE function no_of_occupied_NOs(self,genre,tol) result (res)
   ! Returns the number of non-zero occupied natural orbitals. For this purpose,
   ! zero is defined to be "tol" if present, or TOL(7) otherwise
      type(MOLECULE_TYPE), IN :: self
      STR(len=*), optional, IN :: genre
      REAL, optional, IN :: tol
      INT :: res
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.INQ:no_of_occupied_NOs ... no occupation numbers")
      res = no_of_occupied_(self%NO_occ_nos,genre,tol)
   end function

   pure function has_all_NAO_matrices(self) result (res)
   ! Return TRUE if the NAOs are made for every atom.
      type(MOLECULE_TYPE), IN :: self
      BIN :: res
      res = allocated(self%atom)
      if (res) res = has_all_NAO_matrices_(self%atom)
   end function

   pure function has_all_ANO_matrices(self) result (res)
   ! Return TRUE if the ANO data is made for every atom.
      type(MOLECULE_TYPE), IN :: self
      BIN :: res
      res = allocated(self%atom)
      if (res) res = has_all_ANO_matrices_(self%atom)
   end function

   pure function has_all_interpolators(self) result (res)
   ! Return TRUE if the per-atom interpolators are made for every atom.
      type(MOLECULE_TYPE), IN :: self
      BIN :: res
      res = allocated(self%atom)
      if (res) res = has_all_interpolators_(self%atom)
   end function

   pure function has_all_atom_data(self) result (res)
   ! Return TRUE if all the atom ANOs and interpolators are made for
   ! every atom.
      type(MOLECULE_TYPE), IN :: self
      BIN :: res
      res = allocated(self%atom)
      if (res) res = has_all_atom_data_(self%atom)
   end function


!  =========
!  SCF & DFT
!  =========

   pure function guess_SCF_method(self) result (res)
   ! Guess the scf method in use.
      type(MOLECULE_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      res = "?"
      if (allocated(self%SCF_data)) then; res = self%SCF_data%SCF_kind
      else if (allocated(self%MOs)) then; res = guess_SCF_kind_(self%MOs)

   ! else;                                      DIE("no way to  guess!")
      end if
   end function

   pure function DFT_grid_info_made(self) result (res)
   ! Return TRUE if the DFT grid information is made
      type(MOLECULE_TYPE), IN :: self
      BIN :: res
      res = allocated(self%becke_grid) AND allocated(self%overlapping_atoms_for_atom)
   end function

   pure function has_Hirshfeld_inputs(self) result (res)
   ! Return TRUE if the inputs for a Hirshfeld calc are there
      type(MOLECULE_TYPE), IN :: self
      BIN :: res
      res = DFT_grid_info_made_(self) AND has_all_ANO_matrices_(self) AND (has_all_interpolators_(self) OR has_all_atom_data_(self))
   end function

   pure function has_Salvador_info(self) result (res)
   ! Return TRUE if the Salvador neighbours & radii are there.
      type(MOLECULE_TYPE), IN :: self
      BIN :: res
      res = allocated(self%Salvador_bonds_for) AND allocated(self%Salvador_radii_for)
   end function


!  =================
!  Overlapping atoms
!  =================

   PURE function are_overlapping_atoms_0(self,a,b) result (res)
   ! Return TRUE if atoms "a" and "b" overlap.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a,b
      BIN :: res
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.INQ:are_overlapping_atoms_0 ... no .overlapping_atoms_for_atom")
      if (a==b) then; res = TRUE
      else; res = any(self%overlapping_atoms_for_atom(a)%element==b)
      end if
   end function

   PURE function are_overlapping_atoms_1(self,ab) result (res)
   ! Return TRUE if atom pair "ab" overlap.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: ab
      BIN :: res
      INT :: a,b
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.INQ:are_overlapping_atoms_1 ... no .overlapping_atoms_for_atom")
      a = (1+int(sqrt(EIGHT*ab-SEVEN)))/2
      b = ab-a*(a-1)/2
      res = are_overlapping_atoms_(self,b,a)
   end function

   PURE function is_overlapping_atom(self,a,list) result (res)
   ! Return TRUE if atom "a" overlaps with *any* atoms in the "list".
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a
      VEC(INT,:), IN :: list
      BIN :: res
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.INQ:is_overlapping_atom ... no .overlapping_atoms_for_atom")
      res = has_elements_common_with_(self%overlapping_atoms_for_atom(a)%element,list)
   end function

   PURE function overlapping_atoms_for(self,list) result (res)
   ! Return the list of atoms which overlaps with those in "list".
      type(MOLECULE_TYPE), IN :: self
      VEC(INT,:), IN :: list
      VEC(INT,:), allocatable :: res
      INT :: a,k
      call create_(res,self%n_atom)
      k = 0
      do a = 1,self%n_atom
         if (NOT is_overlapping_atom_(self,a,list)) cycle
         k = k+1
         res(k) = a
      end do
      call shrink_(res,k)
   end function


!  =================
!  VEC{ATOM} methods
!  =================

   PURE function default_spin_multiplicity(self) result (res)
   ! Return the default multiplicity for a molecule.
      type(MOLECULE_TYPE), IN :: self
      INT :: res
      INT :: n_e
   ENSURE(allocated(self%atom),"MOLECULE.INQ:default_spin_multiplicity ... no atom info")
      n_e = no_of_electrons_(self)
      if (size(self%atom)==1) then

         ! One atom
         if (self%atom(1)%atomic_number==-1) then
            res = 2  ! This is an El electron
         else
            res = ground_state_multiplicity_(self%atom(1),n_e)
         end if
      else

         ! Singlet or doublet
         res = mod(n_e,2)+1
      end if
   end function


!   nuclear_repulsion_energy result (res) :: PURE
!   ! Return the nuclear repulsion energy
!      self :: IN
!      res :: REAL
!
!   ENSURE(.atom.allocated,"atom list required")
!
!      i :: INT
!      SCF_kind :: STR
!
!      SCF_kind = "unknown"
!      if (.SCF_data.allocated) SCF_kind = .SCF_data.SCF_kind
!
!      i = 0
!      res = ZERO
!      select case (SCF_kind)
!      case ("embedded_rhf"); res = .atom.nuclear_repulsion_energy([(i,i=1,.saved.n_atom)])
!      case ("crystal_rhf "); res = .atom.nuclear_repulsion_energy([(i,i=1,.saved.n_atom)])
!      case default;          res = .atom.nuclear_repulsion_energy
!      end
!
!   end

!  ============================
!  Nuclear & electric potential
!  ============================

   pure subroutine nuclear_potential(self,values,pts)
   ! Calculate the nuclear potential "values" of a given set of "pts".
   ! This is useful for numerical integration of nuclear attraction
   ! integrals. Dummay atom charges are included.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: values
      MAT(REAL,:,:), IN :: pts
      INT :: n_pts,n,i
      REAL :: Z_n,r
      VEC(REAL,3) :: pos
      n_pts = size(pts,1)
      values = ZERO
      do n = 1,self%n_atom
         Z_n = self%atom(n)%nuclear_charge
         pos = self%atom(n)%position
         do i = 1,n_pts
            r = distance_to_(pos,pts(i,:))
            if (r>tiny(r)) then
               values(i) = values(i)+Z_n/r
            else
               values(i) = values(i)+huge(ONE)
            end if
         end do
      end do
   end subroutine

   PURE subroutine electric_potential(values,grid,charges,positions)
   ! Calculate the electric potential "values" at a set of "grid" points, coming
   ! from a given set "charges" located at "positions".  This is useful for
   ! numerical integration of the potential.
      VEC(REAL,:), OUT :: values
      VEC(REAL,:), IN :: charges
      MAT(REAL,:,:), IN :: grid
      MAT(REAL,:,:), IN :: positions
      INT :: n,i
      REAL :: q,r
      VEC(REAL,3) :: pos
   ENSURE(size(charges)==size(positions,1),"MOLECULE.INQ:electric_potential ... inconsistent charges and positions arrays")
   ENSURE(size(positions,2)==3,"MOLECULE.INQ:electric_potential ... wrong 2nd dimension, positions array")
   ENSURE(size(values)==size(grid,1),"MOLECULE.INQ:electric_potential ... inconsistent values and grid array")
      values = ZERO
      do n = 1,size(charges)
         q = charges(n)
         pos = positions(n,:)
         do i = 1,size(grid,1)
            r = distance_to_(pos,grid(i,:))
            if (r>tiny(r)) then
               values(i) = values(i)+q/r
            else
               values(i) = values(i)+TEN**6  ! something very large
            end if
         end do
      end do
   end subroutine


!  ================
!  Atom connections
!  ================

   pure function connected(self,a,b) result (res)
   ! Return true if atoms "a" and "b" are connected
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a,b
      BIN :: res
      if (a<=size(self%atoms_bonded_to_atom,1)) then
         res = any(b==self%atoms_bonded_to_atom(a)%element(:))
      else
         res = FALSE
      end if
   end function

   pure function connected_or_identical(self,a,b) result (res)
   ! Return true if atoms "a" and "b" are connected
   ! or if they are the same atom
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a,b
      BIN :: res
      if (a==b) then
         res = TRUE
      else if (a<=size(self%atoms_bonded_to_atom,1)) then
         res = any(b==self%atoms_bonded_to_atom(a)%element(:))
      else
         res = FALSE
      end if
   end function


!  ===========
!  Atom groups
!  ===========

   pure function n_bonds_between_atom_groups(self,g1,g2) result (res)
   ! Return the number of connections between ".atom_group(g1)" and
   ! ".atom_group(g2)". 
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: g1,g2
      INT :: res
      INT :: i,j,a,b
      BIN :: bonded
      res = 0
      residue1: do i = 1,size(self%atom_group(g1)%atom_index)
         a = self%atom_group(g1)%atom_index(i)
         residue2: do j = 1,size(self%atom_group(g2)%atom_index)
            b = self%atom_group(g2)%atom_index(j)
            bonded = has_a_connection_(a,b)
            if (bonded) then
               res = res+1
            end if
         end do residue2
      end do residue1
   end function

   pure function bonded_atoms_between_groups(self,g1,g2) result (res)
   ! Return the number of connections between ".atom_group(g1)" and
   ! ".atom_group(g2)" in a table "res(1:2,n_bonds)". 
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: g1,g2
      MAT(INT,2,n_bonds_between_atom_groups(self,g1,g2)) :: res
      INT :: n
      INT :: i,j
      INT :: a,b
      BIN :: bonded
      n = 0
      residue1: do i = 1,size(self%atom_group(g1)%atom_index)
         a = self%atom_group(g1)%atom_index(i)
         residue2: do j = 1,size(self%atom_group(g2)%atom_index)
            b = self%atom_group(g2)%atom_index(j)
            ! Are a & b bonded?
            bonded = has_a_connection_(a,b)
            ! Store pair of bonded atoms
            if (bonded) then
               n = n+1
               res(1,n) = a
               res(2,n) = b
            end if
         end do residue2
      end do residue1
   end function

   PURE function n_bf_up_to_mol(self,g) result (res)
   ! Return the no. of basis functions up to the "g"-th
   ! molecule .mol(g) assuming they are concatenated.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: g
      INT :: res
      INT :: i
   ENSURE(allocated(self%mol),"MOLECULE.INQ:n_bf_up_to_mol ... no .mol")
   ENSURE(g>0 AND g<=size(self%mol),"MOLECULE.INQ:n_bf_up_to_mol ... group out of range")
      res = 0
      do i = 1,g-1
         res = res+self%mol(i)%n_bf
      end do
   end function


!  ===============
!  ARCHIVE related
!  ===============

   function archive_exists(self,archive_name,genre) result (res)
   ! Return TRUE if the fock matrix exists
      type(MOLECULE_TYPE), IN :: self
      STR(len=*), IN :: archive_name
      STR(len=*), IN :: genre
      BIN :: res
      type(ARCHIVE_TYPE) :: archive
      call set_(archive,self%name,archive_name,genre)
      res = exists_(archive)
   end function

   function archive_doesnt_exist(self,archive_name,genre) result (res)
   ! Return TRUE if the fock matrix exists
      type(MOLECULE_TYPE), IN :: self
      STR(len=*), IN :: archive_name
      STR(len=*), IN :: genre
      BIN :: res
      type(ARCHIVE_TYPE) :: archive
      call set_(archive,self%name,archive_name,genre)
      res = NOT exists_(archive)
   end function


!  ===========
!  CIF related
!  ===========

   PURE function wavefunction_descriptor(self) result (psi)
   ! Get the wavefunction details. Only call this if an SCF
   ! calculation was completed!
      type(MOLECULE_TYPE), IN :: self
      VEC(STR(len=STR_SIZE),19) :: psi
      STR(len=STR_SIZE) :: field
      REAL :: radius
      INT :: L
   ENSURE(allocated(self%SCF_data),"MOLECULE.INQ:wavefunction_descriptor ... no SCF data!")
      psi = "?"
      if (NOT allocated(self%SCF_data)) return

      ! QM method
      psi(1) = "Hartree-Fock"
      if (is_DFT_calculation_(self%SCF_data)) then
         psi(1) = "Kohn-Sham/"//trim(self%SCF_data%DFT_exchange_functional)//trim(self%SCF_data%DFT_correlation_functional)
      end if
      if (self%SCF_data%using_cluster_charges) then

         ! Add crystal field
         field = "CF"
         if (self%SCF_data%using_SC_cluster_charges) field = "SCCF"

         ! Add L
         if (self%SCF_data%using_qq_cluster_charges) then
            field = trim(field)//" L=2"
         else
            field = trim(field)//" L=1"
         end if

         ! Radius
         radius = self%SCF_data%cluster_radius*ANGSTROM_PER_BOHR

         ! All bits
         psi(1) = trim(psi(1))//"/"//trim(field)//" R="//trim(to_str_(radius,"f3.1"))//" A"
      end if

      ! AO kind
      psi(2) = spinorbital_genre_(self%SCF_data)

      ! Basis
      psi(3) = "custom?"
      if (self%basis_name/=" ") psi(3) = trim(self%basis_name)

      ! Basis kind
      psi(4) = "cartesian"
      if (self%use_spherical_basis) psi(4) = "spherical-harmonic"

      ! Basis max L
      L = maximum_basis_set_l_value_(self%basis)
      psi(5) = trim(to_str_(L))

      ! Becke grid
      if (is_DFT_calculation_(self%SCF_data)) then
         if (allocated(self%becke_grid)) then
            psi(6) = trim(self%becke_grid%kind)
            psi(7) = trim(self%becke_grid%accuracy)
            psi(8) = trim(self%becke_grid%pruning_scheme)
            psi(9) = trim(to_str_(self%becke_grid%basis_fn_cutoff,fmt="e6.1"))
            psi(10) = trim(to_str_(self%becke_grid%rho_cutoff,fmt="e6.1"))
            psi(11) = trim(to_str_(self%becke_grid%scale_atomic_grids))
            psi(12) = trim(to_str_(no_of_pts_for_row_(self%becke_grid,1)))
            psi(13) = trim(to_str_(no_of_pts_for_row_(self%becke_grid,2)))
            psi(14) = trim(to_str_(no_of_pts_for_row_(self%becke_grid,3)))
            psi(15) = trim(to_str_(self%becke_grid%n_radial_pts))
            psi(16) = trim(to_str_(self%becke_grid%n_extra_pts_per_shell))
            psi(17) = trim(to_str_(self%becke_grid%l_angular_grid))
            psi(18) = trim(to_str_(self%becke_grid%l_H_angular_grid))
            psi(19) = trim(to_str_(self%becke_grid%reduce_H_angular_grid))
         end if
      end if
   end function

end module
