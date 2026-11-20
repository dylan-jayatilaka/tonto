!===============================================================================
!
! MOLECULE.PUT
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
module MOLECULE_PUT_MODULE

#  include "macros"

   include "molecule.put.use"

! The version, commit and build date (GIT_VERSION ...), remade on every build
#  include "build_stamp"

   implicit none

   include "molecule.put.int"


contains

!  ======
!  Output
!  ======

   subroutine put(self)
   ! Put all the available molecule information on file
      type(MOLECULE_TYPE), INOUT :: self
   DIE_IF(NOT allocated(tonto),"MOLECULE.PUT:put ... no tonto variable")
      call put_basics_(self)

      ! Stop here for low verbosity
      if (tonto%low_verbosity) then
         if (allocated(self%atom)) then
            if (size(self%atom)>100) return
         end if
      end if

      ! Atoms coordinates, basis sets
      if (allocated(self%atom)) call put_atoms_(self)  ! INOUT

      ! Atom groups
      if (allocated(self%atom_group)) call put_atom_groups_(self)

      ! Stop here for proteins
      if (use_Ryde_capping_(self%crystal)) then
         if (allocated(self%atom)) then
            if (size(self%atom)>100) return
         end if
      end if
      if (allocated(self%pointgroup)) call put_pointgroup_(self)
      if (allocated(self%crystal)) call put_crystal_(self)
      if (allocated(self%cluster)) call put_cluster_(self)
      if (allocated(self%becke_grid)) call put_becke_grid_(self)
      if (allocated(self%overlapping_atoms_for_atom) AND allocated(self%atom)) then
         call put_overlapping_atoms_for_atom_(self)
      end if
   end subroutine

   subroutine put_basics(self)
   ! Put basic molecule information
      type(MOLECULE_TYPE), IN :: self
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"====================")
      call text_(stdout,"Molecule information")
      call text_(stdout,"====================")
      call flush_(stdout)
      call show_(stdout,"Name                   =",self%name)
      call flush_(stdout)
      call show_(stdout,"Chemical Formula       =",trim(chemical_formula_(self%atom,with_spaces=TRUE)))
      call show_(stdout,"Molecular weight       =",molecular_weight_(self%atom))
      call flush_(stdout)
      call show_(stdout,"Charge                 =",self%charge)
      call show_(stdout,"Multiplicity           =",self%spin_multiplicity)
      call flush_(stdout)
      call show_(stdout,"No. of atoms           =",self%n_atom)
      call show_(stdout,"No. of electrons       =",self%n_e)
      call show_(stdout,"No. of alpha electrons =",self%n_a)
      call show_(stdout,"No. of beta  electrons =",self%n_b)
      if (has_all_bases_(self%atom)) call flush_(stdout)
      if (self%basis_name/=" ") then
         call show_(stdout,"Gaussian basis name    =",self%basis_name)
         call show_(stdout,"Spherical harmonics?   =",self%use_spherical_basis)
      end if
      if (self%slaterbasis_name/=" ") call show_(stdout,"Slater basis name      =",self%slaterbasis_name)
      if (self%coppensbasis_name/=" ") call show_(stdout,"Coppens basis name     =",self%coppensbasis_name)
      ! Which auxiliary basis a density fit used is part of reproducing it
      if (self%auxiliary_basis_name/=" ") call show_(stdout,"Auxiliary basis name   =",self%auxiliary_basis_name)
      if (NOT is_zero_(self%E_field)) call show_(stdout,"Applied E Field        =",self%E_field(1),self%E_field(2),self%E_field(3))
      if (NOT is_zero_(self%B_field)) call show_(stdout,"Applied B Field        =",self%B_field(1),self%B_field(2),self%B_field(3))
      if (NOT is_zero_(self%gauge_origin)) call show_(stdout,"B field Gauge origin   =",self%gauge_origin)
   end subroutine

   subroutine put_atoms(self)
   ! Output the atom coordinate and basis set information
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_atoms ... no atom list")
      call put_coordinate_info_(self%atom)
      if (allocated(self%basis)) call put_bases_(self)  ! INOUT
   ! if (.coppensbasis.allocated) .put_coppensbases
   ! if (.slaterbasis.allocated)  .put_slaterbases

   ! .atom.put_ADPs
   end subroutine

   subroutine put_atom_coordinates(self)
   ! Output the atom coordinate and basis set information
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_atom_coordinates ... no atom list")
      call put_coordinates_(self%atom)
   end subroutine

   subroutine put_basics_and_atoms(self)
   ! Output minimal atom info without crystal info.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_basics_and_atoms ... no atom list")
      call put_basics_(self)
      call put_atoms_(self)  ! INOUT
      call put_bases_(self)
   end subroutine

   subroutine put_ADPs(self)
   ! Output the ADPs in cartesian axes
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_ADPs ... no atom list")
      call put_ADPs_(self%atom)
   end subroutine

   subroutine put_pointgroup(self)
   ! Output the current pointgroup
      type(MOLECULE_TYPE), IN :: self
      call put_(self%pointgroup)
   end subroutine

   subroutine put_principal_axes(self)
   ! Output the atom principal axes
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_principal_axes ... no atom list")
      call put_principal_axes_(self%atom)
   end subroutine


!  ================
!  Bonds and angles
!  ================

   subroutine put_CCDC_bonded_neighbours(self)
   ! Put the CCDC bonded neighbours for reach atom.
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_CCDC_bonded_neighbours ... no atoms")
      call put_CCDC_bonded_neighbours_(self%atom)
   end subroutine

   subroutine put_bond_length_with_error(self)
   ! Read two atoms, and output the bond length with associated error
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: cm
      INT :: a1,a2,n_f
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_bond_length_with_error ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_bond_length_with_error ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.PUT:put_bond_length_with_error ... no diffraction data")
   ENSURE(allocated(self%crystal%xray_data%covariance_mx),"MOLECULE.PUT:put_bond_length_with_error ... no cov matrix created")

      ! Read the two atoms
      call read_(stdin,a1)
      call read_(stdin,a2)

      ! No. of fragment atoms
      n_f = no_of_pADPs_(self%crystal%fragment_atom)

      ! Set fragment covariance matrix
      call destroy_(self%crystal%xray_data%fragment_covariance_mx)
      call create_(self%crystal%xray_data%fragment_covariance_mx,n_f,n_f)
      call set_frag_from_asym_cov_mx_(self%crystal)  ! INOUT

      ! Extract positional-covariances
      call make_pos_covariance_mx_(self%crystal,cm)

      ! Put the bond length
      call put_bond_length_with_error_(self%atom,a1,a2,cm)

      ! Clean
      call destroy_(cm)
   end subroutine

   subroutine put_bond_angle_with_error(self)
   ! Output the atom coordinate and basis set information
   ! Read three atoms, and output the bond angle with associated error
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: cm
      INT :: a1,a2,a3,n_f
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_bond_angle_with_error ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_bond_angle_with_error ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.PUT:put_bond_angle_with_error ... no diffraction data")
   ENSURE(allocated(self%crystal%xray_data%covariance_mx),"MOLECULE.PUT:put_bond_angle_with_error ... no cov matrix created")

      ! Read the three atoms defining the angle
      call read_(stdin,a1)
      call read_(stdin,a2)
      call read_(stdin,a3)

      ! No. of fragment atoms
      n_f = no_of_pADPs_(self%crystal%fragment_atom)

      ! Set fragment covariance matrix
      call destroy_(self%crystal%xray_data%fragment_covariance_mx)
      call create_(self%crystal%xray_data%fragment_covariance_mx,n_f,n_f)
      call set_frag_from_asym_cov_mx_(self%crystal)  ! INOUT

      ! Extract positional-covariances
      call make_pos_covariance_mx_(self%crystal,cm)

      ! Put the bond angle
      call put_bond_angle_with_error_(self%atom,a1,a2,a3,cm)
      call destroy_(cm)
   end subroutine

   subroutine put_torsion_angle_with_error(self)
   ! Output the atom coordinate and basis set information
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: cm
      INT :: a1,a2,a3,a4,n_f
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_torsion_angle_with_error ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_torsion_angle_with_error ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.PUT:put_torsion_angle_with_error ... no diffraction data")
   ENSURE(allocated(self%crystal%xray_data%covariance_mx),"MOLECULE.PUT:put_torsion_angle_with_error ... no cov matrix created")

      ! Read the three atoms defining the angle
      call read_(stdin,a1)
      call read_(stdin,a2)
      call read_(stdin,a3)
      call read_(stdin,a4)

      ! No. of fragment atoms
      n_f = no_of_pADPs_(self%crystal%fragment_atom)

      ! Set fragment covariance matrix
      call destroy_(self%crystal%xray_data%fragment_covariance_mx)
      call create_(self%crystal%xray_data%fragment_covariance_mx,n_f,n_f)
      call set_frag_from_asym_cov_mx_(self%crystal)  ! INOUT

      ! Extract positional-covariances
      call make_pos_covariance_mx_(self%crystal,cm)

      ! Put the torsion angle
      call put_torsion_angle_with_error_(self%atom,a1,a2,a3,a4,cm)

      ! Clean
      call destroy_(cm)
   end subroutine


!  =====
!  Bases
!  =====

   subroutine put_bases(self)
   ! Output the molecule basis set information, if the bases are all resolved.
      type(MOLECULE_TYPE), INOUT :: self
      if (NOT allocated(self%basis)) return

      ! Put atom basis tags
      call put_bases_(self%atom)
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"===================")
      call text_(stdout,"Gaussian basis sets")
      call text_(stdout,"===================")
      call flush_(stdout)
      call show_(stdout,"Basis name             =",self%basis_name)
      call flush_(stdout)
      if (self%use_spherical_basis) call show_(stdout,"Spherical harmonics?   =",self%use_spherical_basis)
      call show_(stdout,"No. of basis sets      =",self%n_basis)
      call show_(stdout,"No. of shells          =",self%n_shell)
      call show_(stdout,"No. of shell pairs     =",self%n_shell_pairs)
      call show_(stdout,"No. of basis functions =",self%n_bf)
      call show_(stdout,"No. of primitives      =",self%n_prim)
      call flush_(stdout)
      call renormalize_(self%basis)  ! INOUT
      call put_(self%basis)
      call unnormalize_(self%basis)
   end subroutine

   subroutine put_slaterbases(self)
   ! Output the molecule slaterbasis set information, if the bases are
   ! all resolved.
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_slaterbases ... no atom list")
   ENSURE(allocated(self%slaterbasis),"MOLECULE.PUT:put_slaterbases ... no basis set list")
      if (NOT slaterbases_are_resolved_(self%atom)) return
      call flush_(stdout)
      call text_(stdout,"Molecule atom slaterbasis set information:")
      call flush_(stdout)
      call show_(stdout,"No. of basis sets =",size(self%slaterbasis))
      call put_(self%slaterbasis)
   end subroutine

   subroutine put_coppensbases(self)
   ! Output the molecule coppensbasis set information, if the bases are all
   ! resolved.
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_coppensbases ... no atom list")
   ENSURE(allocated(self%coppensbasis),"MOLECULE.PUT:put_coppensbases ... no basis set list")
      if (NOT coppensbases_are_resolved_(self%atom)) return
      call flush_(stdout)
      call text_(stdout,"Molecule atom coppensbasis set information:")
      call flush_(stdout)
      call show_(stdout,"No. of basis sets =",size(self%coppensbasis))
      call show_(stdout,"Maximum N value   =",maximum_basis_n_value_(self%coppensbasis))
      call show_(stdout,"No. of orbitals   =",no_of_orbitals_(self%coppensbasis))
      call show_(stdout,"No. of primitives =",no_of_primitives_(self%coppensbasis))
      call put_(self%coppensbasis)
   end subroutine


!  ======
!  Groups
!  ======

   subroutine put_atom_groups(self)
   ! Output the .atom_group info array
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom_group),"MOLECULE.PUT:put_atom_groups ... no atom group information")
      call put_(self%atom_group)
      call put_atom_group_mols_(self)
   end subroutine

   subroutine put_atom_group_mols(self)
   ! Output each .atom_group.mol info
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name
      INT :: g
      BIN :: saved
   ENSURE(allocated(self%atom_group),"MOLECULE.PUT:put_atom_group_mols ... no .atom_group information")
   ENSURE(allocated(self%mol),"MOLECULE.PUT:put_atom_group_mols ... no .mol information")

      ! This is pure output, and it must NOT be collective.
      !
      ! After a per-rank fragment loop (MOLECULE.SCF:fragment_SCF_para) each
      ! rank has built grids and densities only for the fragment IT ran, so
      ! .mol(g).becke_grid and friends are allocated on one rank and not on
      ! another. MOLECULE:put branches on exactly that, so the ranks issue
      ! different numbers of TEXTFILE bookkeeping broadcasts, fall out of step,
      ! and a later collective pairs with the wrong one -- observed as rank 1
      ! dying in TEXTFILE:close with "not an existing file!" because its
      ! .exists broadcast received another rank's payload.
      !
      ! Per-rank I/O mode makes the TEXTFILE bookkeeping non-collective, so the
      ! master can then do the writing alone. Restore the caller's mode rather
      ! than forcing it FALSE: this may be called from inside a per-rank region.
      saved = tonto%per_rank_IO_allowed
      call set_per_rank_IO_allowed_(tonto,TRUE)
      if (is_master_processor_(tonto)) then

         ! Output fragment info
         do g = 1,size(self%atom_group)

            ! Minimal stdout for fragment
            name = trim(self%atom_group(g)%name)
            call redirect_(stdout,name)
         ! .mol(g).put_basics_and_atoms
            call put_(self%mol(g))  ! INOUT
            call revert_(stdout)

            ! Even more minimal xyz for plotting
            call put_xyz_file_(self%mol(g)%atom,name)
         end do
      end if
      call set_per_rank_IO_allowed_(tonto,saved)
   end subroutine

   subroutine put_overlapping_atoms_for_atom(self)
   ! Out out the .overlapping_atoms_for_atom info array
      type(MOLECULE_TYPE), IN :: self
      INT :: i,j,k
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.PUT:put_overlapping_atoms_for_atom ... no overlapping_atoms_for_atom")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_overlapping_atoms_for_atom ... no atoms")
      call save_style_(stdout)
      call set_int_width_(stdout,4)
      call set_n_fields_per_line_(stdout,25)
      call set_using_array_labels_(stdout,FALSE)
      call text_(stdout," ")
      call text_(stdout,"Overlapping atoms for atom:")
      call text_(stdout," ")
      do i = 1,size(self%atom)
         call put_(stdout,i)
         k = 1
         do j = 1,size(self%overlapping_atoms_for_atom(i)%element)
            k = k+1
            call put_(stdout,self%overlapping_atoms_for_atom(i)%element(j))
            if (mod(k,25)==0) call flush_(stdout)
         end do
         if (mod(k,25)/=0) call flush_(stdout)
      end do
      call unsave_style_(stdout)
   end subroutine

   subroutine put_plot_grid(self)
   ! Output the current grid.
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%plot_grid),"MOLECULE.PUT:put_plot_grid ... no plot grid to output")
      call put_(self%plot_grid)
   end subroutine

   subroutine put_becke_grid(self)
   ! Output the current DFT grid.
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%becke_grid),"MOLECULE.PUT:put_becke_grid ... no Becke integration grid to output")
      call put_(self%becke_grid)
   end subroutine

   subroutine put_crystal(self)
   ! Output the current crystal
      type(MOLECULE_TYPE), INOUT :: self
      call put_(self%crystal)
   end subroutine

   subroutine put_cluster(self)
   ! Put out the cluster information
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%cluster),"MOLECULE.PUT:put_cluster ... no cluster data")
      call put_(self%cluster)
   end subroutine


!  ======================
!  MOs, NOs, and matrices
!  ======================

   subroutine put_MOs(self)
   ! Output the current associated molecular orbitals
      type(MOLECULE_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"Molecular orbitals:")
      call put_(stdout,self%MOs)
   end subroutine

   subroutine put_MOs_and_energies(self)
   ! Output the current associated molecular orbitals and their energies
      type(MOLECULE_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"Molecular orbital energies:")
      call put_(stdout,self%MO_energies)
      call flush_(stdout)
      call text_(stdout,"Molecular orbitals:")
      call put_(stdout,self%MOs)
   end subroutine

   subroutine put_MO_energies(self)
   ! Output the molecular orbital energies
      type(MOLECULE_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"Molecular orbital energies:")
      call put_(stdout,self%MO_energies)
   end subroutine

   subroutine put_NOs(self)
   ! Output the current associated molecular orbitals
      type(MOLECULE_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"NO occupation numbers:")
      call put_(stdout,self%NO_occ_nos)
      call flush_(stdout)
      call text_(stdout,"Natural orbitals:")
      call put_(stdout,self%NOs)
   end subroutine


! Warning: wrt unnormalized basis functions

   subroutine put_density_mx(self)
   ! Output the current associated density matrix
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%density_mx),"MOLECULE.PUT:put_density_mx ... no density matrix")
      call flush_(stdout)
      call text_(stdout,"Density matrix:")
      call put_(stdout,self%density_mx)
   end subroutine

   subroutine put_Fock_mx(self)
   ! Output the current associated fock matrix
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%fock_mx),"MOLECULE.PUT:put_Fock_mx ... no fock matrix")
      call flush_(stdout)
      call text_(stdout,"Fock matrix:")
      call put_(stdout,self%fock_mx)
   end subroutine

   subroutine put_overlap_mx_evals(self)
   ! Calculate the overlap matrix eigenvalues
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(self%basis_info_made,"MOLECULE.PUT:put_overlap_mx_evals ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_overlap_mx_evals ... no atom list")
   ENSURE(bases_are_resolved_(self%atom),"MOLECULE.PUT:put_overlap_mx_evals ... no atom bases")
   ENSURE(bases_are_resolved_(self%atom),"MOLECULE.PUT:put_overlap_mx_evals ... no atom bases")
   ENSURE(allocated(self%overlap_mx_evals),"MOLECULE.PUT:put_overlap_mx_evals ... no overlap_mx_evals: make_overlap_mx.")
      call save_style_(stdout)
      call set_real_fortran_fmt_(stdout,"e")
      call set_real_width_(stdout,20)
      call set_real_precision_(stdout,10)
      call text_(stdout,"Overlap matrix eigenvalues:")
      call put_(stdout,self%overlap_mx_evals)
      call unsave_style_(stdout)
   end subroutine

   subroutine put_overlap_mx(self)
   ! Print out the overlap matrix
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%overlap_mx),"MOLECULE.PUT:put_overlap_mx ... no overlap_mx_evals: make_overlap_mx.")
      call text_(stdout,"Overlap matrix:")
      call put_(stdout,self%overlap_mx)
   end subroutine

   subroutine put_kinetic_energy_mx(self)
   ! Print out the kinetic energy matrix
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%kinetic_energy_mx),"MOLECULE.PUT:put_kinetic_energy_mx ... no kinetic_energy_mx.")
      call text_(stdout,"Kinetic energy matrix:")
      call put_(stdout,self%kinetic_energy_mx)
   end subroutine

   subroutine put_nuclear_attraction_mx(self)
   ! Print out the nuclear attractions matrix
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%nuclear_attraction_mx),"MOLECULE.PUT:put_nuclear_attraction_mx ... no nuclear_attraction_mx.")
      call text_(stdout,"Nuclear attraction matrix:")
      call put_(stdout,self%nuclear_attraction_mx)
   end subroutine


!  =========
!  Archiving
!  =========

   subroutine put_archive_0(self)
   ! Write the archive called "name". The genre is defined by the
   ! object to be written.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: fbf,lbf,ang
      STR(len=STR_SIZE) :: name,normalize
      BIN :: do_norm
      type(ARCHIVE_TYPE) :: arch

      ! Read the archive "name"
      call read_(stdin,name)

      ! Set archive "name"
      call set_(arch,self%name,name)

      ! Second word on line is normalize?
      normalize = " "
      if (stdin%buffer%n_items==3) call read_(stdin,normalize)
      ! Both spellings. The first test used to read "normalize" as well, so
      ! the British spelling was silently ignored here while read_archive
      ! accepted it -- a copy-paste slip, not a deliberate asymmetry.
      do_norm = normalize(1:9)=="normalise" OR normalize(1:9)=="normalize"

      ! Tell them
      if (do_norm) then
         call flush_(stdout)
         call text_(stdout,"Writing normalized "//trim(name))
         call flush_(stdout)
      end if
      fbf = self%first_basis_fn_for_shell
      lbf = self%last_basis_fn_for_shell
      ang = self%angular_moment_for_shell

      ! Write the archive, "genre" from object
      select case (name)

      case ("molecular_orbitals","MOs               ","HF_MOs            ","KS_MOs            ")
         DIE_IF(NOT allocated(self%MOs),"MOLECULE.PUT:put_archive_0 ... no MOs")
         if (do_norm) then
            call renormalize_(self%MOs,"1",fbf,lbf,ang)
            call write_(arch,self%MOs)
            call unnormalize_(self%MOs,"1",fbf,lbf,ang)
         else
            call write_(arch,self%MOs)
         end if

      case ("MO_energies       ","HF_MO_energies    ","KS_MO_energies    ")
         DIE_IF(NOT allocated(self%MO_energies),"MOLECULE.PUT:put_archive_0 ... no MO_energies")
         call write_(arch,self%MO_energies)

      case ("natural_orbitals  ","NOs               ")
         DIE_IF(NOT allocated(self%NOs),"MOLECULE.PUT:put_archive_0 ... no NOs")
         if (do_norm) then
            call renormalize_(self%NOs,"1",fbf,lbf,ang)
            call write_(arch,self%NOs)
            call unnormalize_(self%NOs,"1",fbf,lbf,ang)
         else
            call write_(arch,self%NOs)
         end if

      case ("no_occupation_nos ","NO_occ_nos        ")
         DIE_IF(NOT allocated(self%NO_occ_nos),"MOLECULE.PUT:put_archive_0 ... no NO_occ_nos")
         call write_(arch,self%NO_occ_nos)

      case ("density_matrix    ","density_mx        ","hf_density_matrix ","HF_density_mx     ","ks_density_matrix ","KS_density_mx     ","mp2_density_matrix","MP2_density_mx    ","mp3_density_matrix","MP3_density_mx    ")
         DIE_IF(NOT allocated(self%density_mx),"MOLECULE.PUT:put_archive_0 ... no density_mx")
         if (do_norm) then
            call renormalize_(self%density_mx,fbf,lbf,ang)
            call write_(arch,self%density_mx)
            call unnormalize_(self%density_mx,fbf,lbf,ang)
         else
            call write_(arch,self%density_mx)
         end if

      case ("fock_matrix       ","Fock_mx           ","Kohn-Sham_mx      ")
         DIE_IF(NOT allocated(self%fock_mx),"MOLECULE.PUT:put_archive_0 ... no fock_mx")
         if (do_norm) then
            call renormalize_(self%fock_mx,fbf,lbf,ang)
            call write_(arch,self%fock_mx)
            call unnormalize_(self%fock_mx,fbf,lbf,ang)
         else
            call write_(arch,self%fock_mx)
         end if

      case default
         allocate(tonto%known_keywords(24))
         tonto%known_keywords(1) = "molecular_orbitals"
         tonto%known_keywords(2) = "MOs               "
         tonto%known_keywords(3) = "HF_MOs            "
         tonto%known_keywords(4) = "KS_MOs            "
         tonto%known_keywords(5) = "MO_energies       "
         tonto%known_keywords(6) = "HF_MO_energies    "
         tonto%known_keywords(7) = "KS_MO_energies    "
         tonto%known_keywords(8) = "natural_orbitals  "
         tonto%known_keywords(9) = "NOs               "
         tonto%known_keywords(10) = "no_occupation_nos "
         tonto%known_keywords(11) = "NO_occ_nos        "
         tonto%known_keywords(12) = "density_matrix    "
         tonto%known_keywords(13) = "density_mx        "
         tonto%known_keywords(14) = "hf_density_matrix "
         tonto%known_keywords(15) = "HF_density_mx     "
         tonto%known_keywords(16) = "ks_density_matrix "
         tonto%known_keywords(17) = "KS_density_mx     "
         tonto%known_keywords(18) = "mp2_density_matrix"
         tonto%known_keywords(19) = "MP2_density_mx    "
         tonto%known_keywords(20) = "mp3_density_matrix"
         tonto%known_keywords(21) = "MP3_density_mx    "
         tonto%known_keywords(22) = "fock_matrix       "
         tonto%known_keywords(23) = "Fock_mx           "
         tonto%known_keywords(24) = "Kohn-Sham_mx      "
         call unknown_(tonto,name,"MOLECULE.PUT:put_archive")
         deallocate(tonto%known_keywords)
      end select

      ! Clean up
      call close_(arch)
   end subroutine

   subroutine put_ascii_archive(self)
   ! Write the archive called "name". The genre is defined by the
   ! object to be written.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name
      type(ARCHIVE_TYPE) :: arch

      ! Read the archive "name"
      call read_(stdin,name)

      ! Set the archive "name" and ascii format
      call set_(arch,self%name,name,format="ascii")

      ! Write the archive, "genre" from object
      select case (name)

      case ("molecular_orbitals","MOs               ","HF_MOs            ","KS_MOs            ")

      ! .MOs.renormalize("1",.first_basis_fn_for_shell,.last_basis_fn_for_shell,.angular_moment_for_shell)
         call write_(arch,self%MOs,by_column=TRUE)
      ! .MOs.unnormalize("1",.first_basis_fn_for_shell,.last_basis_fn_for_shell,.angular_moment_for_shell)

      case ("MO_energies       ","HF_MO_energies    ","KS_MO_energies    ")
         call write_(arch,self%MO_energies,by_column=TRUE)

      case ("natural_orbitals  ","NOs               ")
      ! .NOs.renormalize("1",.first_basis_fn_for_shell,.last_basis_fn_for_shell,.angular_moment_for_shell)
         call write_(arch,self%NOs,by_column=TRUE)
      ! .NOs.unnormalize("1",.first_basis_fn_for_shell,.last_basis_fn_for_shell,.angular_moment_for_shell)

      case ("occupation_nos    ","NO_occ_nos        ")
         call write_(arch,self%NO_occ_nos,by_column=TRUE)

      case ("density_matrix    ","density_mx        ","hf_density_matrix ","HF_density_mx     ","ks_density_matrix ","KS_density_mx     ","mp2_density_matrix","MP2_density_mx    ","mp3_density_matrix","MP3_density_mx    ")

      ! .density_mx.renormalize(.first_basis_fn_for_shell,.last_basis_fn_for_shell,.angular_moment_for_shell)
         call write_(arch,self%density_mx,by_column=TRUE)
      ! .density_mx.unnormalize(.first_basis_fn_for_shell,.last_basis_fn_for_shell,.angular_moment_for_shell)

      case ("fock_matrix       ","Fock_mx           ","Kohn-Sham_mx      ")

      ! .fock_mx.renormalize(.first_basis_fn_for_shell,.last_basis_fn_for_shell,.angular_moment_for_shell)
         call write_(arch,self%fock_mx,by_column=TRUE)
      ! .fock_mx.unnormalize(.first_basis_fn_for_shell,.last_basis_fn_for_shell,.angular_moment_for_shell)

      case default
         allocate(tonto%known_keywords(24))
         tonto%known_keywords(1) = "molecular_orbitals"
         tonto%known_keywords(2) = "MOs               "
         tonto%known_keywords(3) = "HF_MOs            "
         tonto%known_keywords(4) = "KS_MOs            "
         tonto%known_keywords(5) = "MO_energies       "
         tonto%known_keywords(6) = "HF_MO_energies    "
         tonto%known_keywords(7) = "KS_MO_energies    "
         tonto%known_keywords(8) = "natural_orbitals  "
         tonto%known_keywords(9) = "NOs               "
         tonto%known_keywords(10) = "occupation_nos    "
         tonto%known_keywords(11) = "NO_occ_nos        "
         tonto%known_keywords(12) = "density_matrix    "
         tonto%known_keywords(13) = "density_mx        "
         tonto%known_keywords(14) = "hf_density_matrix "
         tonto%known_keywords(15) = "HF_density_mx     "
         tonto%known_keywords(16) = "ks_density_matrix "
         tonto%known_keywords(17) = "KS_density_mx     "
         tonto%known_keywords(18) = "mp2_density_matrix"
         tonto%known_keywords(19) = "MP2_density_mx    "
         tonto%known_keywords(20) = "mp3_density_matrix"
         tonto%known_keywords(21) = "MP3_density_mx    "
         tonto%known_keywords(22) = "fock_matrix       "
         tonto%known_keywords(23) = "Fock_mx           "
         tonto%known_keywords(24) = "Kohn-Sham_mx      "
         call unknown_(tonto,name,"MOLECULE.PUT:put_ascii_archive")
         deallocate(tonto%known_keywords)
      end select

      ! Clean up
      call close_(arch)
   end subroutine

   subroutine put_archive_1(self,opmatrix,archive_name,genre,with_lambda)
   ! Save "opmatrix" to an archive on disk with name "archive_name".
   ! The genre is guessed from "opmatrix" unless "genre" is present.
   ! If "with_lambda" is present and TRUE and the archives are not to
   ! be deleted after the scf calculation, then the lambda value is
   ! attached to the stored archive.
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), IN :: opmatrix
      STR(len=*), IN :: archive_name
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: with_lambda
      type(ARCHIVE_TYPE) :: archive
      BIN :: avec_lambda
      avec_lambda = FALSE
      if (present(with_lambda)) avec_lambda = with_lambda
      call set_(archive,self%name,archive_name)
      call write_(archive,opmatrix,genre)
      if (avec_lambda) then
         if (allocated(self%SCF_data)) then
            if (includes_(self%SCF_data%SCF_kind,"xray_") OR includes_(self%SCF_data%SCF_kind,"pnd_")) then

      ! archive.set(.name,trim(archive_name)//",lambda="//trim(.SCF_data.lambda.to_str("f5.3")))
               call set_(archive,self%name,trim(archive_name)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6")))
               call write_(archive,opmatrix,genre)
            end if
         end if
      end if
   end subroutine

   subroutine put_archive_2(self,opvector,archive_name,genre,with_lambda)
   ! Save "opvector" to an archive on disk with name "archive_name".
   ! The genre is guessed from "opmatrix" unless "genre" is present.
   ! If "with_lambda" is present and TRUE and the archives are not to
   ! be deleted after the scf calculation, then the lambda value is
   ! attached to the stored archive.
      type(MOLECULE_TYPE), IN :: self
      type(OPVECTOR_TYPE), IN :: opvector
      STR(len=*), IN :: archive_name
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: with_lambda
      type(ARCHIVE_TYPE) :: archive
      BIN :: avec_lambda
      avec_lambda = FALSE
      if (present(with_lambda)) avec_lambda = with_lambda
      call set_(archive,self%name,archive_name)
      call write_(archive,opvector,genre)
      if (avec_lambda) then
         if (allocated(self%SCF_data)) then
            if (includes_(self%SCF_data%SCF_kind,"xray_") OR includes_(self%SCF_data%SCF_kind,"pnd_")) then

      !   archive.set(.name,trim(archive_name)//",lambda="//trim(.SCF_data.lambda.to_str("f5.3")))
               call set_(archive,self%name,trim(archive_name)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6")))
               call write_(archive,opvector,genre)
            end if
         end if
      end if
   end subroutine

   subroutine put_archive_3(self,item,archive_name,genre,with_lambda)
   ! Save "item" to an archive on disk with name "archive_name".
      type(MOLECULE_TYPE), IN :: self
      REAL, IN :: item
      STR(len=*), IN :: archive_name
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: with_lambda
      type(ARCHIVE_TYPE) :: archive
      BIN :: avec_lambda
      avec_lambda = FALSE
      if (present(with_lambda)) avec_lambda = with_lambda
      call set_(archive,self%name,archive_name)
      call write_(archive,item,genre)
      if (avec_lambda) then
         if (allocated(self%SCF_data)) then
            if (includes_(self%SCF_data%SCF_kind,"xray_") OR includes_(self%SCF_data%SCF_kind,"pnd_")) then

      !   archive.set(.name,trim(archive_name)//",lambda="//trim(.SCF_data.lambda.to_str("f5.3")))
               call set_(archive,self%name,trim(archive_name)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6")))
               call write_(archive,item,genre)
            end if
         end if
      end if
   end subroutine


!  ====
!  VRML
!  ====

   subroutine put_vrml(self)
   ! Output a VRML file for the coordinate geometry
      type(MOLECULE_TYPE), INOUT :: self
      type(TEXTFILE_TYPE), allocatable :: out
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_vrml ... no atom list!")
      call create_(out,trim(self%name)//achar(46)//"wrl")
      call open_for_(out,"write")
      call text_(stdout,"Generating VRML molecule")
      call text_(out,"#VRML V2.0 utf8")
      call text_(out,"NavigationInfo { type "//achar(34)//"EXAMINE"//achar(34)//" }")
      call text_(out,"Viewpoint { ")
      call text_(out,"position 0 0 10")
      call text_(out,"fieldOfView 1")
      call text_(out,"orientation 0 0 1 0")
      call text_(out,'description "camera z"')
      call text_(out,"}")
      call text_(out,"DirectionalLight {")
      call text_(out,"  color 1 1 1")
      call text_(out,"  direction 1 0 0")
      call text_(out,"  intensity 0.4")
      call text_(out,"}")
      call put_vrml_(self%atom,out)
      if (allocated(self%isosurface)) call put_vrml_(self%isosurface,out)  ! INOUT
      call text_(stdout,"done VRML molecule")
   end subroutine


!  =====
!  ANO's
!  =====

   subroutine put_ANOs(self)
   ! Put the ANO data out for all atoms in the molecule.
      type(MOLECULE_TYPE), IN :: self
      INT :: k,a,b
      STR(len=STR_SIZE) :: list
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_ANOs ... no atoms")
      if (NOT has_all_ANO_matrices_(self)) return
      if (self%n_atom_kind<=0) return
      call flush_(stdout)
      call text_(stdout,"========")
      call text_(stdout,"ANO data")
      call text_(stdout,"========")
      do a = 1,self%n_atom

         ! Unique atom?
         if (NOT self%atom(a)%is_unique_kind) cycle
         k = self%atom(a)%kind
         list = trim(to_str_(a))
         do b = a+1,self%n_atom
            if (self%atom(b)%kind/=k) cycle
            list = trim(list)//" "//trim(to_str_(b))
         end do
         list = "ANO's for atoms ... "//trim(list)
         call flush_(stdout)
         call text_(stdout,repeat("=",len_trim(list)))
         call text_(stdout,list)
         call text_(stdout,repeat("=",len_trim(list)))
         call put_ANOs_(self%atom(a))
         call flush_(stdout)
      end do
   end subroutine


!  ==============
!  Atomic moments
!  ==============

   subroutine put_moments(self)
   ! Put out the atomic electric moments
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%density_mx),"MOLECULE.PUT:put_moments ... no density")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_moments ... no atoms")
   ENSURE(allocated(self%basis),"MOLECULE.PUT:put_moments ... no basis sets specified")

      ! Moments
      call put_dipole_(self)
      call put_quadrupole_(self)
      call put_octupole_(self)
   end subroutine

   subroutine put_dipole(self)
   ! Put out the dipole to stdout
      type(MOLECULE_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,3) :: elec
      VEC(REAL,3) :: nucl
      VEC(REAL,3) :: total
      VEC(STR(len=STR_SIZE),3) :: axis
      REAL :: dmag
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_dipole ... no atoms")
      axis = ["x","y","z"]
      elec = electronic_dipole_moment_(self)
      nucl = nuclear_dipole_moment_(self%atom)
      total = elec+nucl
      dmag = sqrt(dot_product(total,total))
      call head_(stdout,"Dipole moments")
      call show_(stdout,"Dipole magnitude/au    =",dmag)
      call show_(stdout,"Dipole magnitude/Debye =",dmag*DEBYE_PER_AU)

      ! Table
      call flush_(stdout)
      call text_(stdout,"Dipole moment components in AU.")
      call flush_(stdout)
      call create_(table,4)
      call set_heading_(table(1),"Axis")
      call set_values_(table(1),axis)
      call set_heading_(table(2),"Electronic")
      call set_values_(table(2),elec)
      call set_heading_(table(3),"Nuclear")
      call set_values_(table(3),nucl)
      call set_heading_(table(4),"Total")
      call set_subhead_(table(4),"/au")
      call set_values_(table(4),total)
      call put_(table,label_rows=FALSE)
      call destroy_(table)

      ! Table in Debye
      call flush_(stdout)
      call text_(stdout,"Dipole moment components in Debye.")
      call flush_(stdout)
      elec = elec*DEBYE_PER_AU
      nucl = nucl*DEBYE_PER_AU
      total = total*DEBYE_PER_AU
      call create_(table,4)
      call set_heading_(table(1),"Axis")
      call set_values_(table(1),axis)
      call set_heading_(table(2),"Electronic")
      call set_values_(table(2),elec)
      call set_heading_(table(3),"Nuclear")
      call set_values_(table(3),nucl)
      call set_heading_(table(4),"Total")
      call set_subhead_(table(4),"/D")
      call set_values_(table(4),total)
      call put_(table,label_rows=FALSE)
      call destroy_(table)
   end subroutine

   subroutine put_quadrupole(self)
   ! Put out the quadrupole to stdout
      type(MOLECULE_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,6) :: elec
      VEC(REAL,6) :: nucl
      VEC(REAL,6) :: total
      VEC(STR(len=STR_SIZE),6) :: axis
      REAL :: fac
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_quadrupole ... no atoms")
      axis = ["xx","yy","zz","xy","xz","yz"]
      elec = electronic_quadrupole_moment_vec_(self)
      nucl = nuclear_quadrupole_moment_vec_(self%atom)
      total = elec+nucl

      ! Table
      call flush_(stdout)
      call text_(stdout,"Qudrupole moment components in AU.")
      call flush_(stdout)
      call create_(table,4)
      call set_heading_(table(1),"Axis")
      call set_values_(table(1),axis)
      call set_heading_(table(2),"Electronic")
      call set_values_(table(2),elec)
      call set_heading_(table(3),"Nuclear")
      call set_values_(table(3),nucl)
      call set_heading_(table(4),"Total")
      call set_subhead_(table(4),"/au")
      call set_values_(table(4),total)
      call put_(table,label_rows=FALSE)
      call destroy_(table)

      ! Table in Debye-Angstrom
      call flush_(stdout)
      call text_(stdout,"Qudrupole moment components in Debye-Angtrom.")
      call flush_(stdout)
      fac = DEBYE_PER_AU*(ANGSTROM_PER_BOHR)
      elec = elec*fac
      nucl = nucl*fac
      total = total*fac
      call create_(table,4)
      call set_heading_(table(1),"Axis")
      call set_values_(table(1),axis)
      call set_heading_(table(2),"Electronic")
      call set_values_(table(2),elec)
      call set_heading_(table(3),"Nuclear")
      call set_values_(table(3),nucl)
      call set_heading_(table(4),"Total")
      call set_subhead_(table(4),"/D A")
      call set_values_(table(4),total)
      call put_(table,label_rows=FALSE)
      call destroy_(table)
   end subroutine

   subroutine put_octupole(self)
   ! Put out the quadrupole to stdout
      type(MOLECULE_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,10) :: elec
      VEC(REAL,10) :: nucl
      VEC(REAL,10) :: total
      VEC(STR(len=STR_SIZE),10) :: axis
      REAL :: fac
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_octupole ... no atoms")
      axis = ["xxx","yyy","zzz","xxy","xxz","yyx","yyz","zzx","zzy","xyz"]
      elec = electronic_octupole_moment_vec_(self)
      nucl = nuclear_octupole_moment_vec_(self%atom)
      total = elec+nucl

      ! Table
      call flush_(stdout)
      call text_(stdout,"Octupole moment components in AU.")
      call flush_(stdout)
      call create_(table,4)
      call set_heading_(table(1),"Axis")
      call set_values_(table(1),axis)
      call set_heading_(table(2),"Electronic")
      call set_values_(table(2),elec)
      call set_heading_(table(3),"Nuclear")
      call set_values_(table(3),nucl)
      call set_heading_(table(4),"Total")
      call set_subhead_(table(4),"/au")
      call set_values_(table(4),total)
      call put_(table,label_rows=FALSE)
      call destroy_(table)

      ! Table in Debye*A^2
      call flush_(stdout)
      call text_(stdout,"Octupole moment components in Debye Angstrom^2.")
      call flush_(stdout)
      fac = DEBYE_PER_AU*(ANGSTROM_PER_BOHR)**2
      elec = elec*fac
      nucl = nucl*fac
      total = total*fac
      call create_(table,4)
      call set_heading_(table(1),"Axis")
      call set_values_(table(1),axis)
      call set_heading_(table(2),"Electronic")
      call set_values_(table(2),elec)
      call set_heading_(table(3),"Nuclear")
      call set_values_(table(3),nucl)
      call set_heading_(table(4),"Total")
      call set_subhead_(table(4),"/D A^2")
      call set_values_(table(4),total)
      call put_(table,label_rows=FALSE)
      call destroy_(table)
   end subroutine

   subroutine put_HA_moments(self)
   ! Put out the atomic charges, dipoles, and quadrupole and moments,
   ! from some partitioning method.  NOTE: The moments should have
   ! been made previously!
      type(MOLECULE_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: pole1m
      VEC(REAL,:), allocatable :: pole2m
      MAT(REAL,:,:), allocatable :: pole1a
      MAT(REAL,:,:), allocatable :: pole2a
      MAT(REAL,3,3) :: pole2
   ! pole2s :: MAT{REAL}(3,3) ! sum
      REAL :: da,dx
   ! qa, qx :: REAL
      INT :: a
   ENSURE(self%basis_info_made,"MOLECULE.PUT:put_HA_moments ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_HA_moments ... no atom list")
   ENSURE(has_dipoles_(self%atom),"MOLECULE.PUT:put_HA_moments ... no atomic dipoles")

      ! Atomic moments & magnitudes

      ! The following code is inherited from MOLECULE.PUT
      call create_(pole1a,size(self%atom),3)
      call create_(pole1m,size(self%atom))
      call create_(pole2a,size(self%atom),6)
      call create_(pole2m,size(self%atom))

      ! Get moments
   ! pole2s = ZERO ! sum
      do a = 1,self%n_atom

         ! Extract dipoles
         pole1a(a,1) = self%atom(a)%dipole(1)  ! mu_A
         pole1a(a,2) = self%atom(a)%dipole(2)
         pole1a(a,3) = self%atom(a)%dipole(3)
         pole1m(a) = norm2(self%atom(a)%dipole)  ! |mu_A|
         ! Extract quadrupoles
         pole2 = self%atom(a)%quadrupole  ! Q_A
         pole2a(a,1) = pole2(1,1)
         pole2a(a,2) = pole2(2,2)
         pole2a(a,3) = pole2(3,3)
         pole2a(a,4) = pole2(1,2)
         pole2a(a,5) = pole2(1,3)
         pole2a(a,6) = pole2(2,3)
   !    pole2s      = pole2s + pole2         ! Sum up Q_A
         pole2 = matmul(pole2,pole2)  ! Q_A^2
         pole2m(a) = sqrt(trace_(pole2))  ! |Q_A|
      end do

      ! Dipole magnitudes by expectation
      da = norm2(sum(pole1a,dim=1))  ! |mu|
      dx = norm2(dipole_moment_(self))

      ! Quadrupole magnitudes by expectation
   ! pole2 = matmul(pole2s,pole2s)           ! Q^2
   ! qa    = sqrt(pole2.trace)               ! |Q|
   ! pole2 = .electronic_quadrupole_moment_mx
   ! pole2 = matmul(pole2,pole2)             ! <Q>^2
   ! qx    = sqrt(pole2.trace)

      ! Results in au
      call head_(stdout,"Hirshfeld charges and moments")
      call text_(stdout,". Atomic charges *include* the nuclear charge.")
      call flush_(stdout)
      call text_(stdout,". Atomic dipoles & quadrupoles are for electrons")
      call text_(stdout,"  only; the nucleus is assumed at the origin.")
      call flush_(stdout)
      call text_(stdout,". All results are in AU.")
      call flush_(stdout)
      call text_(stdout,"  Check the accuracy of numerical integration: see")
      call text_(stdout,"  if the sum of atomic charges is the total charge!")
   ! stdout.flush
   ! stdout.text(". For uncharged species: see if the  magnitude of")
   ! stdout.text("  the atom-summed dipoles is the same as that got")
   ! stdout.text("  by expectation value.")
      call flush_(stdout)
      call show_(stdout,"Sum of atomic charges q_A =",sum(self%atom(:)%charge))
   ! stdout.show("| mu | after sum of mu_A  =",da)
   ! stdout.show("|<mu>| from expectation   =",dx)
   ! stdout.show("| Q |  after sum of Q_A   =",qa)
   ! stdout.show("|<Q>|  from expectation   =",qx)

      ! Table
      call flush_(stdout)
      call text_(stdout,"Atomic charges and electronic dipoles.")
      call flush_(stdout)
      call create_(table,6)
      call set_heading_(table(1),"Atom")
      call set_heading_(table(2),"Charge")
      call set_subhead_(table(2),"q")
      call set_heading_(table(3),"Dipole")
      call set_subhead_(table(3),"mu_x")
      call set_heading_(table(4)," ")
      call set_subhead_(table(4),"mu_y")
      call set_heading_(table(5)," ")
      call set_subhead_(table(5),"mu_z")
      call set_heading_(table(6)," ")
      call set_subhead_(table(6),"|mu|")
      call set_values_(table(1),self%atom(:)%tag)
      call set_values_(table(2),self%atom(:)%charge)
      call set_values_(table(3),pole1a(:,1))
      call set_values_(table(4),pole1a(:,2))
      call set_values_(table(5),pole1a(:,3))
      call set_values_(table(6),pole1m(:))
      call put_(table,label_rows=TRUE)
      call destroy_(table)

      ! Table
      call flush_(stdout)
      call text_(stdout,"Atomic electronic quadrupoles.")
      call flush_(stdout)
      call create_(table,8)
      call set_heading_(table(1),"Atom")
      call set_heading_(table(2),"Quadrupole")
      call set_subhead_(table(2),"Qxx")
      call set_heading_(table(3)," ")
      call set_subhead_(table(3),"Qyy")
      call set_heading_(table(4)," ")
      call set_subhead_(table(4),"Qzz")
      call set_heading_(table(5)," ")
      call set_subhead_(table(5),"Qxy")
      call set_heading_(table(6)," ")
      call set_subhead_(table(6),"Qxz")
      call set_heading_(table(7)," ")
      call set_subhead_(table(7),"Qyz")
      call set_heading_(table(8)," ")
      call set_subhead_(table(8),"|Q|")
      call set_values_(table(1),self%atom(:)%tag)
      call set_values_(table(2),pole2a(:,1))
      call set_values_(table(3),pole2a(:,2))
      call set_values_(table(4),pole2a(:,3))
      call set_values_(table(5),pole2a(:,4))
      call set_values_(table(6),pole2a(:,5))
      call set_values_(table(7),pole2a(:,6))
      call set_values_(table(8),pole2m(:))
      call put_(table,label_rows=TRUE)
      call destroy_(table)

      ! Clean
      call destroy_(pole2m)
      call destroy_(pole2a)
      call destroy_(pole1m)
      call destroy_(pole1a)
   end subroutine

   subroutine put_BA_moments(self)
   ! Put out the atomic charges, dipoles, and quadrupole and moments,
   ! from some partitioning method.  NOTE: The moments should have
   ! been made previously!
      type(MOLECULE_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: pole1m
      VEC(REAL,:), allocatable :: pole2m
      MAT(REAL,:,:), allocatable :: pole1a
      MAT(REAL,:,:), allocatable :: pole2a
      MAT(REAL,3,3) :: pole2
   ! pole2s :: MAT{REAL}(3,3) ! sum
      REAL :: da,dx
   ! qa, qx :: REAL
      INT :: a
   ENSURE(self%basis_info_made,"MOLECULE.PUT:put_BA_moments ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_BA_moments ... no atom list")
   ENSURE(has_dipoles_(self%atom),"MOLECULE.PUT:put_BA_moments ... no atomic dipoles")

      ! Atomic moments & magnitudes

      ! The following code is inherited from MOLECULE.PUT
      call create_(pole1a,size(self%atom),3)
      call create_(pole1m,size(self%atom))
      call create_(pole2a,size(self%atom),6)
      call create_(pole2m,size(self%atom))

      ! Get moments
   ! pole2s = ZERO ! sum
      do a = 1,self%n_atom

         ! Extract dipoles
         pole1a(a,1) = self%atom(a)%dipole(1)  ! mu_A
         pole1a(a,2) = self%atom(a)%dipole(2)
         pole1a(a,3) = self%atom(a)%dipole(3)
         pole1m(a) = norm2(self%atom(a)%dipole)  ! |mu_A|
         ! Extract quadrupoles
         pole2 = self%atom(a)%quadrupole  ! Q_A
         pole2a(a,1) = pole2(1,1)
         pole2a(a,2) = pole2(2,2)
         pole2a(a,3) = pole2(3,3)
         pole2a(a,4) = pole2(1,2)
         pole2a(a,5) = pole2(1,3)
         pole2a(a,6) = pole2(2,3)
   !    pole2s      = pole2s + pole2         ! Sum up Q_A
         pole2 = matmul(pole2,pole2)  ! Q_A^2
         pole2m(a) = sqrt(trace_(pole2))  ! |Q_A|
      end do

      ! Dipole magnitudes by expectation
      da = norm2(sum(pole1a,dim=1))  ! |mu|
      dx = norm2(dipole_moment_(self))

      ! Quadrupole magnitudes by expectation
   ! pole2 = matmul(pole2s,pole2s)           ! Q^2
   ! qa    = sqrt(pole2.trace)               ! |Q|
   ! pole2 = .electronic_quadrupole_moment_mx
   ! pole2 = matmul(pole2,pole2)             ! <Q>^2
   ! qx    = sqrt(pole2.trace)

      ! Results in au
      call head_(stdout,"Becke charges and moments")
      call text_(stdout,". Atomic charges *include* the nuclear charge.")
      call flush_(stdout)
      call text_(stdout,". Atomic dipoles & quadrupoles are for electrons")
      call text_(stdout,"  only; the nucleus is assumed at the origin.")
      call flush_(stdout)
      call text_(stdout,". All results are in AU.")
      call flush_(stdout)
      call text_(stdout,"  Check the accuracy of numerical integration: see")
      call text_(stdout,"  if the sum of atomic charges is the total charge!")
   ! stdout.flush
   ! stdout.text(". For uncharged species: see if the  magnitude of")
   ! stdout.text("  the atom-summed dipoles is the same as that got")
   ! stdout.text("  by expectation value.")
      call flush_(stdout)
      call show_(stdout,"Sum of atomic charges q_A =",sum(self%atom(:)%charge))
   ! stdout.show("| mu | after sum of mu_A  =",da)
   ! stdout.show("|<mu>| from expectation   =",dx)
   ! stdout.show("| Q |  after sum of Q_A   =",qa)
   ! stdout.show("|<Q>|  from expectation   =",qx)

      ! Table
      call flush_(stdout)
      call text_(stdout,"Atomic charges and electronic dipoles.")
      call flush_(stdout)
      call create_(table,6)
      call set_heading_(table(1),"Atom")
      call set_heading_(table(2),"Charge")
      call set_subhead_(table(2),"q")
      call set_heading_(table(3),"Dipole")
      call set_subhead_(table(3),"mu_x")
      call set_heading_(table(4)," ")
      call set_subhead_(table(4),"mu_y")
      call set_heading_(table(5)," ")
      call set_subhead_(table(5),"mu_z")
      call set_heading_(table(6)," ")
      call set_subhead_(table(6),"|mu|")
      call set_values_(table(1),self%atom(:)%tag)
      call set_values_(table(2),self%atom(:)%charge)
      call set_values_(table(3),pole1a(:,1))
      call set_values_(table(4),pole1a(:,2))
      call set_values_(table(5),pole1a(:,3))
      call set_values_(table(6),pole1m(:))
      call put_(table,label_rows=TRUE)
      call destroy_(table)

      ! Table
      call flush_(stdout)
      call text_(stdout,"Atomic electronic quadrupoles.")
      call flush_(stdout)
      call create_(table,8)
      call set_heading_(table(1),"Atom")
      call set_heading_(table(2),"Quadrupole")
      call set_subhead_(table(2),"Qxx")
      call set_heading_(table(3)," ")
      call set_subhead_(table(3),"Qyy")
      call set_heading_(table(4)," ")
      call set_subhead_(table(4),"Qzz")
      call set_heading_(table(5)," ")
      call set_subhead_(table(5),"Qxy")
      call set_heading_(table(6)," ")
      call set_subhead_(table(6),"Qxz")
      call set_heading_(table(7)," ")
      call set_subhead_(table(7),"Qyz")
      call set_heading_(table(8)," ")
      call set_subhead_(table(8),"|Q|")
      call set_values_(table(1),self%atom(:)%tag)
      call set_values_(table(2),pole2a(:,1))
      call set_values_(table(3),pole2a(:,2))
      call set_values_(table(4),pole2a(:,3))
      call set_values_(table(5),pole2a(:,4))
      call set_values_(table(6),pole2a(:,5))
      call set_values_(table(7),pole2a(:,6))
      call set_values_(table(8),pole2m(:))
      call put_(table,label_rows=TRUE)
      call destroy_(table)

      ! Clean
      call destroy_(pole2m)
      call destroy_(pole2a)
      call destroy_(pole1m)
      call destroy_(pole1a)
   end subroutine

   subroutine put_SS_moments(self)
   ! Put out the atomic charges, dipoles, and quadrupole and moments,
   ! from some partitioning method.  NOTE: The moments should have
   ! been made previously!
      type(MOLECULE_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: pole1m
      VEC(REAL,:), allocatable :: pole2m
      MAT(REAL,:,:), allocatable :: pole1a
      MAT(REAL,:,:), allocatable :: pole2a
      MAT(REAL,3,3) :: pole2
   ! pole2s :: MAT{REAL}(3,3) ! sum
      REAL :: da,dx
   ! qa, qx :: REAL
      INT :: a
   ENSURE(self%basis_info_made,"MOLECULE.PUT:put_SS_moments ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_SS_moments ... no atom list")
   ENSURE(has_dipoles_(self%atom),"MOLECULE.PUT:put_SS_moments ... no atomic dipoles")

      ! Atomic moments & magnitudes

      ! The following code is inherited from MOLECULE.PUT
      call create_(pole1a,size(self%atom),3)
      call create_(pole1m,size(self%atom))
      call create_(pole2a,size(self%atom),6)
      call create_(pole2m,size(self%atom))

      ! Get moments
   ! pole2s = ZERO ! sum
      do a = 1,self%n_atom

         ! Extract dipoles
         pole1a(a,1) = self%atom(a)%dipole(1)  ! mu_A
         pole1a(a,2) = self%atom(a)%dipole(2)
         pole1a(a,3) = self%atom(a)%dipole(3)
         pole1m(a) = norm2(self%atom(a)%dipole)  ! |mu_A|
         ! Extract quadrupoles
         pole2 = self%atom(a)%quadrupole  ! Q_A
         pole2a(a,1) = pole2(1,1)
         pole2a(a,2) = pole2(2,2)
         pole2a(a,3) = pole2(3,3)
         pole2a(a,4) = pole2(1,2)
         pole2a(a,5) = pole2(1,3)
         pole2a(a,6) = pole2(2,3)
   !    pole2s      = pole2s + pole2         ! Sum up Q_A
         pole2 = matmul(pole2,pole2)  ! Q_A^2
         pole2m(a) = sqrt(trace_(pole2))  ! |Q_A|
      end do

      ! Dipole magnitudes by expectation
      da = norm2(sum(pole1a,dim=1))  ! |mu|
      dx = norm2(dipole_moment_(self))

      ! Quadrupole magnitudes by expectation
   ! pole2 = matmul(pole2s,pole2s)           ! Q^2
   ! qa    = sqrt(pole2.trace)               ! |Q|
   ! pole2 = .electronic_quadrupole_moment_mx
   ! pole2 = matmul(pole2,pole2)             ! <Q>^2
   ! qx    = sqrt(pole2.trace)

      ! Results in au
      call head_(stdout,"Stratmann charges and moments")
      call text_(stdout,". Atomic charges *include* the nuclear charge.")
      call flush_(stdout)
      call text_(stdout,". Atomic dipoles & quadrupoles are for electrons")
      call text_(stdout,"  only; the nucleus is assumed at the origin.")
      call flush_(stdout)
      call text_(stdout,". All results are in AU.")
      call flush_(stdout)
      call text_(stdout,"  Check the accuracy of numerical integration: see")
      call text_(stdout,"  if the sum of atomic charges is the total charge!")
   ! stdout.flush
   ! stdout.text(". For uncharged species: see if the  magnitude of")
   ! stdout.text("  the atom-summed dipoles is the same as that got")
   ! stdout.text("  by expectation value.")
      call flush_(stdout)
      call show_(stdout,"Sum of atomic charges q_A =",sum(self%atom(:)%charge))
   ! stdout.show("| mu | after sum of mu_A  =",da)
   ! stdout.show("|<mu>| from expectation   =",dx)
   ! stdout.show("| Q |  after sum of Q_A   =",qa)
   ! stdout.show("|<Q>|  from expectation   =",qx)

      ! Table
      call flush_(stdout)
      call text_(stdout,"Atomic charges and electronic dipoles.")
      call flush_(stdout)
      call create_(table,6)
      call set_heading_(table(1),"Atom")
      call set_heading_(table(2),"Charge")
      call set_subhead_(table(2),"q")
      call set_heading_(table(3),"Dipole")
      call set_subhead_(table(3),"mu_x")
      call set_heading_(table(4)," ")
      call set_subhead_(table(4),"mu_y")
      call set_heading_(table(5)," ")
      call set_subhead_(table(5),"mu_z")
      call set_heading_(table(6)," ")
      call set_subhead_(table(6),"|mu|")
      call set_values_(table(1),self%atom(:)%tag)
      call set_values_(table(2),self%atom(:)%charge)
      call set_values_(table(3),pole1a(:,1))
      call set_values_(table(4),pole1a(:,2))
      call set_values_(table(5),pole1a(:,3))
      call set_values_(table(6),pole1m(:))
      call put_(table,label_rows=TRUE)
      call destroy_(table)

      ! Table
      call flush_(stdout)
      call text_(stdout,"Atomic electronic quadrupoles.")
      call flush_(stdout)
      call create_(table,8)
      call set_heading_(table(1),"Atom")
      call set_heading_(table(2),"Quadrupole")
      call set_subhead_(table(2),"Qxx")
      call set_heading_(table(3)," ")
      call set_subhead_(table(3),"Qyy")
      call set_heading_(table(4)," ")
      call set_subhead_(table(4),"Qzz")
      call set_heading_(table(5)," ")
      call set_subhead_(table(5),"Qxy")
      call set_heading_(table(6)," ")
      call set_subhead_(table(6),"Qxz")
      call set_heading_(table(7)," ")
      call set_subhead_(table(7),"Qyz")
      call set_heading_(table(8)," ")
      call set_subhead_(table(8),"|Q|")
      call set_values_(table(1),self%atom(:)%tag)
      call set_values_(table(2),pole2a(:,1))
      call set_values_(table(3),pole2a(:,2))
      call set_values_(table(4),pole2a(:,3))
      call set_values_(table(5),pole2a(:,4))
      call set_values_(table(6),pole2a(:,5))
      call set_values_(table(7),pole2a(:,6))
      call set_values_(table(8),pole2m(:))
      call put_(table,label_rows=TRUE)
      call destroy_(table)

      ! Clean
      call destroy_(pole2m)
      call destroy_(pole2a)
      call destroy_(pole1m)
      call destroy_(pole1a)
   end subroutine

   subroutine put_SA_moments(self)
   ! Put out the atomic charges, dipoles, and quadrupole and moments,
   ! from some partitioning method.  NOTE: The moments should have
   ! been made previously!
      type(MOLECULE_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: pole1m
      VEC(REAL,:), allocatable :: pole2m
      MAT(REAL,:,:), allocatable :: pole1a
      MAT(REAL,:,:), allocatable :: pole2a
      MAT(REAL,3,3) :: pole2
   ! pole2s :: MAT{REAL}(3,3) ! sum
      REAL :: da,dx
   ! qa, qx :: REAL
      INT :: a
   ENSURE(self%basis_info_made,"MOLECULE.PUT:put_SA_moments ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_SA_moments ... no atom list")
   ENSURE(has_dipoles_(self%atom),"MOLECULE.PUT:put_SA_moments ... no atomic dipoles")

      ! Atomic moments & magnitudes

      ! The following code is inherited from MOLECULE.PUT
      call create_(pole1a,size(self%atom),3)
      call create_(pole1m,size(self%atom))
      call create_(pole2a,size(self%atom),6)
      call create_(pole2m,size(self%atom))

      ! Get moments
   ! pole2s = ZERO ! sum
      do a = 1,self%n_atom

         ! Extract dipoles
         pole1a(a,1) = self%atom(a)%dipole(1)  ! mu_A
         pole1a(a,2) = self%atom(a)%dipole(2)
         pole1a(a,3) = self%atom(a)%dipole(3)
         pole1m(a) = norm2(self%atom(a)%dipole)  ! |mu_A|
         ! Extract quadrupoles
         pole2 = self%atom(a)%quadrupole  ! Q_A
         pole2a(a,1) = pole2(1,1)
         pole2a(a,2) = pole2(2,2)
         pole2a(a,3) = pole2(3,3)
         pole2a(a,4) = pole2(1,2)
         pole2a(a,5) = pole2(1,3)
         pole2a(a,6) = pole2(2,3)
   !    pole2s      = pole2s + pole2         ! Sum up Q_A
         pole2 = matmul(pole2,pole2)  ! Q_A^2
         pole2m(a) = sqrt(trace_(pole2))  ! |Q_A|
      end do

      ! Dipole magnitudes by expectation
      da = norm2(sum(pole1a,dim=1))  ! |mu|
      dx = norm2(dipole_moment_(self))

      ! Quadrupole magnitudes by expectation
   ! pole2 = matmul(pole2s,pole2s)           ! Q^2
   ! qa    = sqrt(pole2.trace)               ! |Q|
   ! pole2 = .electronic_quadrupole_moment_mx
   ! pole2 = matmul(pole2,pole2)             ! <Q>^2
   ! qx    = sqrt(pole2.trace)

      ! Results in au
      call head_(stdout,"Salvador charges and moments")
      call text_(stdout,". Atomic charges *include* the nuclear charge.")
      call flush_(stdout)
      call text_(stdout,". Atomic dipoles & quadrupoles are for electrons")
      call text_(stdout,"  only; the nucleus is assumed at the origin.")
      call flush_(stdout)
      call text_(stdout,". All results are in AU.")
      call flush_(stdout)
      call text_(stdout,"  Check the accuracy of numerical integration: see")
      call text_(stdout,"  if the sum of atomic charges is the total charge!")
   ! stdout.flush
   ! stdout.text(". For uncharged species: see if the  magnitude of")
   ! stdout.text("  the atom-summed dipoles is the same as that got")
   ! stdout.text("  by expectation value.")
      call flush_(stdout)
      call show_(stdout,"Sum of atomic charges q_A =",sum(self%atom(:)%charge))
   ! stdout.show("| mu | after sum of mu_A  =",da)
   ! stdout.show("|<mu>| from expectation   =",dx)
   ! stdout.show("| Q |  after sum of Q_A   =",qa)
   ! stdout.show("|<Q>|  from expectation   =",qx)

      ! Table
      call flush_(stdout)
      call text_(stdout,"Atomic charges and electronic dipoles.")
      call flush_(stdout)
      call create_(table,6)
      call set_heading_(table(1),"Atom")
      call set_heading_(table(2),"Charge")
      call set_subhead_(table(2),"q")
      call set_heading_(table(3),"Dipole")
      call set_subhead_(table(3),"mu_x")
      call set_heading_(table(4)," ")
      call set_subhead_(table(4),"mu_y")
      call set_heading_(table(5)," ")
      call set_subhead_(table(5),"mu_z")
      call set_heading_(table(6)," ")
      call set_subhead_(table(6),"|mu|")
      call set_values_(table(1),self%atom(:)%tag)
      call set_values_(table(2),self%atom(:)%charge)
      call set_values_(table(3),pole1a(:,1))
      call set_values_(table(4),pole1a(:,2))
      call set_values_(table(5),pole1a(:,3))
      call set_values_(table(6),pole1m(:))
      call put_(table,label_rows=TRUE)
      call destroy_(table)

      ! Table
      call flush_(stdout)
      call text_(stdout,"Atomic electronic quadrupoles.")
      call flush_(stdout)
      call create_(table,8)
      call set_heading_(table(1),"Atom")
      call set_heading_(table(2),"Quadrupole")
      call set_subhead_(table(2),"Qxx")
      call set_heading_(table(3)," ")
      call set_subhead_(table(3),"Qyy")
      call set_heading_(table(4)," ")
      call set_subhead_(table(4),"Qzz")
      call set_heading_(table(5)," ")
      call set_subhead_(table(5),"Qxy")
      call set_heading_(table(6)," ")
      call set_subhead_(table(6),"Qxz")
      call set_heading_(table(7)," ")
      call set_subhead_(table(7),"Qyz")
      call set_heading_(table(8)," ")
      call set_subhead_(table(8),"|Q|")
      call set_values_(table(1),self%atom(:)%tag)
      call set_values_(table(2),pole2a(:,1))
      call set_values_(table(3),pole2a(:,2))
      call set_values_(table(4),pole2a(:,3))
      call set_values_(table(5),pole2a(:,4))
      call set_values_(table(6),pole2a(:,5))
      call set_values_(table(7),pole2a(:,6))
      call set_values_(table(8),pole2m(:))
      call put_(table,label_rows=TRUE)
      call destroy_(table)

      ! Clean
      call destroy_(pole2m)
      call destroy_(pole2a)
      call destroy_(pole1m)
      call destroy_(pole1a)
   end subroutine


!  =======
!  Put CIF
!  =======

   subroutine put_grown_CIF(self)
   ! Prints out fragment cif file.
   ! Does only cartesian axes, so no axis change and no ESD transform.
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: clean
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_grown_CIF ... No crystal")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_grown_CIF ... No atom")

      ! Make sure CIF is there ...
      clean = FALSE
      if (NOT allocated(self%cif)) then
         clean = TRUE
         call create_(self%cif,trim(self%name)//".cif")
      end if

      ! Cartesian fragment/CIF2
      call set_use_CIF2_(self%cif,TRUE)
      call redirect_(stdout,trim(self%name)//".cartesian.cif2")
      call put_CIF_banner_(self,self%cif%use_CIF2)
      call put_CIF_basics_(self)
      call put_CIF_wavefunction_(self)
      call put_fragment_CIF_(self%crystal,self%cif)
      call revert_(stdout)

      ! Clean up
      if (clean) then
         call destroy_(self%cif)
      end if
   end subroutine

   subroutine put_CIFs(self)
   ! Prints out an asymmetric and fragment cif file.
   ! Does both cartesian and crystal axes.
   ! NOTE: esd's are never transformed on an axis change -- they belong to
   ! the frame they were made in. A writer emits them only into that frame
   ! and omits them otherwise. See "VEC{ATOM}:put_CIF_no_esd_note".
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: clean
      STR(len=STR_SIZE) :: title
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_CIFs ... No crystal")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_CIFs ... No atom")

      ! Make sure CIF is there ...
      clean = FALSE
      if (NOT allocated(self%cif)) then
         clean = TRUE
         call create_(self%cif,trim(self%name)//".cif")
      end if

      ! Cartesian fragment/CIF2
      call set_use_CIF2_(self%cif,TRUE)

      ! Set title
      title = self%name
      if (allocated(self%SCF_data)) then
         if (is_constrained_wavefunction_(self%SCF_data)) then
            title = trim(title)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6"))
         end if
      end if
      if (allocated(self%plot_grid)) then
         if (self%plot_grid%plot_label/=" ") then
            title = trim(title)//","//trim(self%plot_grid%plot_label)
         end if
      end if

      ! Redirect
      call redirect_(stdout,trim(title)//".cartesian.cif2")
      call put_CIF_banner_(self,self%cif%use_CIF2)
      call put_CIF_basics_(self)
      call put_CIF_wavefunction_(self)
      call put_fragment_CIF_(self%crystal,self%cif)
      call revert_(stdout)

      ! Put refection files
      call put_tonto_fcf_(self)
      call put_xd_fco_(self)

      ! Crystal asymmetric-unit/CIF1
      call set_use_CIF2_(self%cif,FALSE)

      ! Redirect
      call redirect_(stdout,trim(title)//".HBB.cif2")
      call put_CIF_banner_(self,self%cif%use_CIF2)
      call put_CIF_basics_(self)
      call put_CIF_wavefunction_(self)
      call put_fragment_CIF_HBB_(self%crystal,self%cif)
      call revert_(stdout)

      ! Put refection files
      call put_tonto_fcf_(self)
      call put_xd_fco_(self)

      ! Crystal asymmetric-unit/CIF1
      call set_use_CIF2_(self%cif,FALSE)

      ! Redirect
      call redirect_(stdout,trim(title)//".archive.cif")
      call put_CIF_banner_(self,self%cif%use_CIF2)
      call put_CIF_basics_(self)
      call put_CIF_wavefunction_(self)
      call change_axis_system_to_(self%crystal%asymmetric_unit_atom,"crystal",self%crystal%unit_cell)
      call put_asymm_unit_CIF_(self%crystal,self%cif)
      call change_axis_system_to_(self%crystal%asymmetric_unit_atom,"cartesian",self%crystal%unit_cell)
      call put_asymm_unit_CIF_bond_info_(self%crystal)  ! Must be cartesian
      call revert_(stdout)

      ! Put other files
!     if (.crystal.xray_data.allocated) then
!     if (.crystal.xray_data.reflections.allocated) then

      ! .put_shelx_hkl3
      ! .put_shelx_hkl4
      ! .put_shelx_fcf5
      call put_shelx_fcf6_(self)

      ! Crystal fragment/CIF1
      call set_use_CIF2_(self%cif,FALSE)

      ! Redirect
      call redirect_(stdout,trim(title)//".fractional.cif1")
      call put_CIF_banner_(self,self%cif%use_CIF2)
      call put_CIF_basics_(self)
      call put_CIF_wavefunction_(self)
      call change_axis_system_to_(self%crystal%fragment_atom,"crystal",self%crystal%unit_cell)
      call put_fragment_CIF_(self%crystal,self%cif)
      call change_axis_system_to_(self%crystal%fragment_atom,"cartesian",self%crystal%unit_cell)
      call put_fragment_CIF_bond_info_(self%crystal)  ! Must be cartesian
      call revert_(stdout)

!     end
!     end

      ! Clean up
      if (clean) then
         call destroy_(self%cif)
      end if
   end subroutine

   subroutine put_CIF_banner(self,CIF2)
   ! Prints out the CIF banner, with "CIF2" format if needed.
      type(MOLECULE_TYPE), IN :: self
      BIN, IN :: CIF2
      STR(len=STR_SIZE) :: date
      if (CIF2) then
         call text_(stdout,"#\#CIF_2.0")
         call flush_(stdout)
      end if
      call text_(stdout,"# ========================")
      call text_(stdout,"# Tonto generated CIF file")
      call text_(stdout,"# ========================")
      call flush_(stdout)
      call text_(stdout,"# NOTE: This is an incomplete CIF file")
      call text_(stdout,"# Manually edit it to pass checkCIF")
      call flush_(stdout)
      call text_(stdout,"data_"//trim(self%name))
      call flush_(stdout)
      date = current_date_YMD_str_()
      call text_(stdout,"_audit_creation_date                "//trim(date))
      call text_(stdout,"_audit_creation_method              'Tonto "//TONTO_VERSION//" github v. "//GIT_VERSION//"'")
      call flush_(stdout)
      call text_(stdout,"# ============")
      call text_(stdout,"# QCr software")
      call text_(stdout,"# ============")
      call flush_(stdout)
      call text_(stdout,"_QCr_software                       'Tonto'")
      call text_(stdout,"_QCr_software_version               '"//TONTO_VERSION//" v. "//GIT_VERSION//"'")
      call text_(stdout,"_QCr_software_platform              '"//TONTO_PLATFORM//"'")
      call text_(stdout,"_QCr_software_build_date            '"//TONTO_BUILD_DATE//"'")
      call text_(stdout,"_QCr_software_website               'https://github.com/dylan-jayatilaka/tonto'")
   end subroutine

   subroutine put_CIF_basics(self)
   ! Put basic molecule information
      type(MOLECULE_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"# ===========")
      call text_(stdout,"# Publication")
      call text_(stdout,"# ===========")
      call flush_(stdout)
      call text_(stdout,"_publ_contact_author")
      call text_(stdout,";")
      call text_(stdout,"    Prof. Dr. Sir Tonto Magnifico")
      call text_(stdout,";")
      call text_(stdout,"_publ_contact_author_email          'tonto.magnifico@gmail.com'")
      call text_(stdout,"_publ_contact_author_phone          '+61 8 6488 3138'")
      call text_(stdout,"_publ_requested_journal             'IUCrJ'")
      call text_(stdout,"_publ_section_title")
      call text_(stdout,";")
      call text_(stdout,"   'Hirshfeld atom-refinement (HAR) for "//trim(self%name)//"'")
      call text_(stdout,"   'X-ray wavefunction refinement (XWR) for "//trim(self%name)//"'")
      call text_(stdout,"   Tonto Magnifico, IUCrJ (XXXX)")
      call text_(stdout,";")
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"    _publ_author_name")
      call text_(stdout,"    'Tonto Magnifico'")
      call flush_(stdout)
      call text_(stdout,"_publ_author_address")
      call text_(stdout,";")
      call text_(stdout,"    Prof. Dr. Sir Tonto Magnifico")
      call text_(stdout,"    1 El Camino Ciberespacio")
      call text_(stdout,"    Tierra Austral del Espiritu Santo")
      call text_(stdout,";")
      call flush_(stdout)
      call text_(stdout,"# ========")
      call text_(stdout,"# Chemical")
      call text_(stdout,"# ========")
      call flush_(stdout)
      call text_(stdout,"_chemical_name_common               "//trim(self%name))
      call text_(stdout,"_chemical_formula_sum               '"//trim(chemical_formula_(self%atom,with_spaces=TRUE))//"'")
      call text_(stdout,"_chemical_formula_moiety            '"//trim(chemical_formula_(self%atom,with_spaces=TRUE))//"'")
      call text_(stdout,"_chemical_formula_weight            "//trim(to_str_(molecular_weight_(self%atom),"f10.4")))
      call text_(stdout,"_chemical_compound_source           'see text'")
      call text_(stdout,"_chemical_absolute_configuration    unk")
   end subroutine

   subroutine put_CIF_wavefunction(self)
   ! Put the wavefunction information out
      type(MOLECULE_TYPE), IN :: self
      VEC(STR(len=STR_SIZE),19) :: psi
      if (NOT allocated(self%SCF_data)) return
      if (NOT allocated(self%basis)) return

      ! Get wavefunction details
      psi = wavefunction_descriptor_(self)
      call flush_(stdout)
      call text_(stdout,"# ==========")
      call text_(stdout,"# QCr method")
      call text_(stdout,"# ==========")
      call flush_(stdout)
      call text_(stdout,"_QCr_calc_method                    '"//trim(psi(1))//"'")
      call text_(stdout,"_QCr_calc_spinorbital_genre          '"//trim(psi(2))//"'")
      call text_(stdout,"_QCr_calc_gaussian_basis_set        '"//trim(psi(3))//"'")
      call text_(stdout,"_QCr_calc_gaussian_basis_set_kind   '"//trim(psi(4))//"'")
      call text_(stdout,"_QCr_calc_basis_max_l               '"//trim(psi(5))//"'")
      call flush_(stdout)
      call text_(stdout,"_QCr_calc_unit                      'See _atom_site_* in tonto.cif2'")
      call text_(stdout,"_QCr_calc_unit_spin_multiplicity    "//trim(to_str_(self%spin_multiplicity)))
      call text_(stdout,"_QCr_calc_unit_n_electrons          "//trim(to_str_(self%n_e)))
      call text_(stdout,"_QCr_calc_unit_n_alpha_electrons    "//trim(to_str_(self%n_a)))
      call text_(stdout,"_QCr_calc_unit_n_beta_electrons     "//trim(to_str_(self%n_b)))
      if (psi(6)/="?") then
         call flush_(stdout)
         call text_(stdout,"_QCr_Becke_grid_kind                "//trim(psi(6)))
         call text_(stdout,"_QCr_Becke_grid_accuracy            "//trim(psi(7)))
         call text_(stdout,"_QCr_Becke_grid_L_angular_grid      "//trim(psi(17)))
         call text_(stdout,"_QCr_Becke_grid_L_H_angular_grid    "//trim(psi(18)))
         call text_(stdout,"_QCr_Becke_grid_reduce_L_H_ang_grid "//trim(psi(19)))
         call text_(stdout,"_QCr_Becke_grid_core_pruning_scheme "//trim(psi(8)))
         call text_(stdout,"_QCr_Becke_grid_scale_H-He_grid     "//trim(psi(11)))
         call text_(stdout,"_QCr_Becke_grid_n_pts_for_row_1     "//trim(psi(12)))
         call text_(stdout,"_QCr_Becke_grid_n_pts_for_row_2     "//trim(psi(13)))
         call text_(stdout,"_QCr_Becke_grid_n_pts_for_row_3     "//trim(psi(14)))
         call text_(stdout,"_QCr_Becke_grid_n_radial_H_pts      "//trim(psi(15)))
         call text_(stdout,"_QCr_Becke_grid_H_pts_per_n_shell   "//trim(psi(16)))
         call text_(stdout,"_QCr_Becke_grid_basis_fn_cutoff     "//trim(psi(9)))
         call text_(stdout,"_QCr_Becke_grid_rho_cutoff          "//trim(psi(10)))
      end if
   end subroutine

   subroutine put_shelx_hkl3(self)
   ! Prints out reflections in shelx hkl3 format
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_shelx_hkl3 ... No atom")
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_shelx_hkl3 ... No crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.PUT:put_shelx_hkl3 ... No crystal diffraction data")
   ENSURE(allocated(self%crystal%xray_data%reflections),"MOLECULE.PUT:put_shelx_hkl3 ... No crystal reflections")

      ! Fragment
      call redirect_(stdout,trim(self%name)//".hkl3")
      call put_shelx_hkl3_(self%crystal%xray_data%reflections)
      call revert_(stdout)
   end subroutine

   subroutine put_shelx_hkl4(self)
   ! Prints out reflections in shelx hkl4 format
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_shelx_hkl4 ... No atom")
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_shelx_hkl4 ... No crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.PUT:put_shelx_hkl4 ... No crystal diffraction data")
   ENSURE(allocated(self%crystal%xray_data%reflections),"MOLECULE.PUT:put_shelx_hkl4 ... No crystal reflections")

      ! Fragment
      call redirect_(stdout,trim(self%name)//".hkl4")
      call put_shelx_hkl4_(self%crystal%xray_data%reflections)
      call revert_(stdout)
   end subroutine

   subroutine put_shelx_fcf5(self)
   ! Prints out just the reflection information plus minimal
   ! extra stuff as in the shelx fcf LIST=5 file.
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: clean
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_shelx_fcf5 ... No crystal")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_shelx_fcf5 ... No atom")

      ! Make sure CIF object is there ...
      clean = FALSE
      if (NOT allocated(self%cif)) then
         clean = TRUE
         call create_(self%cif,trim(self%name)//".fcf5")
      end if

      ! Fragment
      call redirect_(stdout,trim(self%name)//".fcf5")
      call put_CIF_banner_(self,self%cif%use_CIF2)
      call put_shelx_fcf5_(self%crystal)
      call revert_(stdout)

      ! Clean up
      if (clean) then
         call destroy_(self%cif)
      end if
   end subroutine

   subroutine put_shelx_fcf6(self)
   ! Prints out just the reflection information plus minimal
   ! extra stuff as in the shelx fcf LIST=6 file.
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: clean
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_shelx_fcf6 ... No crystal")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_shelx_fcf6 ... No atom")

      ! Make sure CIF object is there ...
      clean = FALSE
      if (NOT allocated(self%cif)) then
         clean = TRUE
         call create_(self%cif,trim(self%name)//".fcf6")
      end if

      ! Fragment
      call redirect_(stdout,trim(self%name)//".fcf6")
      call put_CIF_banner_(self,self%cif%use_CIF2)
      call put_shelx_fcf6_(self%crystal)
      call revert_(stdout)

      ! Clean up
      if (clean) then
         call destroy_(self%cif)
      end if
   end subroutine

   subroutine put_tonto_fcf(self)
   ! Prints out the tonto fcf file
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: clean
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_tonto_fcf ... No crystal")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_tonto_fcf ... No atom")

      ! Make sure CIF object is there ...
      clean = FALSE
      if (NOT allocated(self%cif)) then
         clean = TRUE
         call create_(self%cif,trim(self%name)//".archive.fcf")
      end if

      ! Fragment
      call save_style_(stdout)
      call redirect_(stdout,trim(self%name)//".archive.fcf")
      call put_CIF_banner_(self,self%cif%use_CIF2)
      call put_tonto_fcf_(self%crystal)
      call revert_(stdout)
      call unsave_style_(stdout)

      ! Clean up
      if (clean) then
         call destroy_(self%cif)
      end if
   end subroutine

   subroutine put_tonto_fcf_XCW(self)
   ! Prints out the tonto fcf file
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: clean
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_tonto_fcf_XCW ... No crystal")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_tonto_fcf_XCW ... No atom")

      ! Make sure CIF object is there ...
      clean = FALSE
      if (NOT allocated(self%cif)) then
         clean = TRUE
         if (self%SCF_data%do_outputs) then
            if (NOT exceeded_lambda_max_(self%SCF_data)) then
               call create_(self%cif,trim(self%name)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6"))//".archive.fcf")
            else
               call create_(self%cif,trim(self%name)//".archive.fcf")
            end if
         else
            call create_(self%cif,trim(self%name)//".archive.fcf")
         end if
      end if

      ! Fragment
      call save_style_(stdout)
      if (self%SCF_data%do_outputs) then
         if (NOT exceeded_lambda_max_(self%SCF_data)) then
            call redirect_(stdout,trim(self%name)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6"))//".archive.fcf")
         else
            call redirect_(stdout,trim(self%name)//".archive.fcf")
         end if
      else
         call redirect_(stdout,trim(self%name)//".archive.fcf")
      end if
      call put_CIF_banner_(self,self%cif%use_CIF2)
      call put_tonto_fcf_(self%crystal)
      call revert_(stdout)
      call unsave_style_(stdout)

      ! Clean up
      if (clean) then
         call destroy_(self%cif)
      end if
   end subroutine

   subroutine put_olex_fcf_XCW(self)
   ! Prints out the tonto fcf file
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: clean
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_olex_fcf_XCW ... No crystal")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_olex_fcf_XCW ... No atom")

      ! Make sure CIF object is there ...
      clean = FALSE
      if (NOT allocated(self%cif)) then
         clean = TRUE
         if (self%SCF_data%do_outputs) then
            if (NOT exceeded_lambda_max_(self%SCF_data)) then
               call create_(self%cif,trim(self%name)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6"))//".olex.fcf")
            else
               call create_(self%cif,trim(self%name)//".archive.fcf")
            end if
         else
            call create_(self%cif,trim(self%name)//".archive.fcf")
         end if
      end if

      ! Fragment
      call save_style_(stdout)
      if (self%SCF_data%do_outputs) then
         if (NOT exceeded_lambda_max_(self%SCF_data)) then
            call redirect_(stdout,trim(self%name)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6"))//".olex.fcf")
         else
            call redirect_(stdout,trim(self%name)//".archive.fcf")
         end if
      else
         call redirect_(stdout,trim(self%name)//".archive.fcf")
      end if
      call put_CIF_banner_(self,self%cif%use_CIF2)
      call put_Olex2_fcf_(self%crystal,"'all reflections are used'")
      call revert_(stdout)
      call unsave_style_(stdout)

      ! Clean up
      if (clean) then
         call destroy_(self%cif)
      end if
   end subroutine

   subroutine put_xd_fco(self)
   ! Prints out just the F2 reflection information plus minimal
   ! extra stuff as in the XD fco file.
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: clean
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_xd_fco ... No crystal")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_xd_fco ... No atom")

      ! Make sure CIF object is there ...
      clean = FALSE
      if (NOT allocated(self%cif)) then
         clean = TRUE
         call create_(self%cif,trim(self%name)//".archive.fco")
      end if

      ! Fragment
      call save_style_(stdout)
      call redirect_(stdout,trim(self%name)//".archive.fco")
      call set_int_width_(stdout,5)
      call set_real_precision_(stdout,4)
      call set_real_width_(stdout,13)
      call put_CIF_banner_(self,self%cif%use_CIF2)
      call put_xd_fco_(self%crystal)
      call revert_(stdout)
      call unsave_style_(stdout)

      ! Clean up
      if (clean) then
         call destroy_(self%cif)
      end if
   end subroutine

   subroutine put_xd_fco_XCW(self)
   ! Prints out just the F2 reflection information plus minimal
   ! extra stuff as in the XD fco file.
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: clean
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_xd_fco_XCW ... No crystal")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_xd_fco_XCW ... No atom")

      ! Make sure CIF object is there ...
      clean = FALSE
      if (NOT allocated(self%cif)) then
         clean = TRUE
         if (self%SCF_data%do_outputs) then
            if (NOT exceeded_lambda_max_(self%SCF_data)) then
               call create_(self%cif,trim(self%name)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6"))//".archive.fco")
            else
               call create_(self%cif,trim(self%name)//".archive.fco")
            end if
         else
            call create_(self%cif,trim(self%name)//".archive.fco")
         end if
      end if

      ! Fragment
      call save_style_(stdout)
      if (self%SCF_data%do_outputs) then
         if (NOT exceeded_lambda_max_(self%SCF_data)) then
            call redirect_(stdout,trim(self%name)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6"))//".archive.fco")
         else
            call redirect_(stdout,trim(self%name)//".archive.fco")
         end if
      else
         call redirect_(stdout,trim(self%name)//".archive.fco")
      end if
      call set_int_width_(stdout,5)
      call set_real_precision_(stdout,4)
      call set_real_width_(stdout,13)
      call put_CIF_banner_(self,self%cif%use_CIF2)
      call put_xd_fco_(self%crystal)
      call revert_(stdout)
      call unsave_style_(stdout)

      ! Clean up
      if (clean) then
         call destroy_(self%cif)
      end if
   end subroutine


!  ===
!  PDB
!  ===

   subroutine put_pdb(self)
   ! Prints out a pdb file
      type(MOLECULE_TYPE), IN :: self
      type(TEXTFILE_TYPE), allocatable :: pdbfile
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_pdb ... No crystal")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_pdb ... No atom")
      call create_(pdbfile,"stdout.pdb")
      call open_for_(pdbfile,"write")
      call text_(pdbfile,"HEADER: TONTO generated PDB file")
      call put_pdb_(self%crystal,pdbfile)
      call put_pdb_(self%atom,pdbfile)
      call close_(pdbfile)
      call destroy_(pdbfile)
   end subroutine


!  ===================
!  Atomic form factors
!  ===================

   subroutine put_atomic_form_factors(self)
   ! Put the spherically averaged restricted atomic form factors
   ! for the unique atoms.
      type(MOLECULE_TYPE), INOUT :: self
      REAL :: length,spacing
      BIN :: IAM,val
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.PUT:put_atomic_form_factors ... no atom")
   ENSURE(allocated(self%crystal),"MOLECULE.PUT:put_atomic_form_factors ... no crystal - specify H form factor type")

      ! Type of IAM SF's
      ! If FALSE, the atomic density_mx is used.
      IAM = self%crystal%xray_data%use_IAM_ITC_FFs

      ! Table length
      length = 2.00d0*FOUR*PI*ANGSTROM_PER_BOHR
      spacing = 0.05d0*FOUR*PI*ANGSTROM_PER_BOHR
      do a = 1,self%n_atom

         ! Unique atom?
         if (NOT self%atom(a)%is_unique_kind) cycle

         ! Structure factors
         call flush_(stdout)
         call text_(stdout,"========================")
         call text_(stdout,"Real form factors for "//trim(chemical_symbol_(self%atom(a))))
         call text_(stdout,"========================")
         call flush_(stdout)
         val = self%atom(a)%use_IAM_ITC_FFs
         call set_use_IAM_ITC_FFs_(self%atom(a),IAM)
         call put_FT_along_z_r_(self%atom(a),length,spacing)
         call set_use_IAM_ITC_FFs_(self%atom(a),val)  ! Put back
      end do
   end subroutine


!  ======================================
!  Crystal structure factor residual maps
!  ======================================

!   make_residual_density_map :: leaky
!   ! Work out the residual density map on ".plot_grid". A Gnuplot
!   ! ascii file is generated. zc
!   ENSURE(.plot_grid.allocated, "no grid")
!
!      map :: VEC{REAL}@
!
!      map.create(.plot_grid.n_pt)
!      .make_residual_density_map(map)
!      .dump_plot_grid(map,"residual_density")
!      map.destroy
!
!   end

!   make_residual_density_map(map)
!   ! Make the crystal error "map".
!      map :: VEC{REAL}, OUT
!
!   ENSURE(.plot_grid.allocated, "no grid")
!
!      pt :: MAT{REAL}@
!      n_pt :: INT
!
!      ! Make pts array
!      n_pt = .plot_grid.n_pt
!      pt.create(n_pt,3)
!      .plot_grid.make_points(pt)
!
!      ! Make residual error map
!      .crystal.make_residual_density_cell(map,.plot_grid.n_x,.plot_grid.n_y,.plot_grid.n_z)
!
!      ! Clean
!      pt.destroy
!
!   end

   subroutine make_residual_density_cell_0(self)
   ! Work out the residual map on the unit cell with divisions as
   ! specified in the ".plot_grid" and write to a file.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), allocatable :: map
   ENSURE(allocated(self%plot_grid),"MOLECULE.PUT:make_residual_density_cell_0 ... no grid")
      call create_(map,self%plot_grid%n_pt)
      call make_residual_density_cell_(self,map)
      call dump_plot_grid_(self,map,"residual_density")
      call destroy_(map)
   end subroutine

   PURE subroutine make_residual_density_cell_1(self,map)
   ! Work out the residual "map" on the unit cell with divisions as
   ! specified in the ".plot_grid".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: map
      INT :: nx,ny,nz
   ENSURE(allocated(self%plot_grid),"MOLECULE.PUT:make_residual_density_cell_1 ... no grid")

      ! Points per axis
      nx = self%plot_grid%n_x
      ny = self%plot_grid%n_y
      nz = self%plot_grid%n_z

      ! Make residual error map
      call make_residual_density_cell_(self%crystal,map,nx,ny,nz)
   end subroutine


!  ============================
!  Standard ED refinement plots
!  ============================

   subroutine put_ED_refinement_plots(self)
   ! Evaluate the min and max residual density from the
   ! fitted structure factors, evaluated on a grid 0.1 A
   ! per side. Also evaluate the residuals at the atom
   ! centers and report the worst one.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: map
      MAT(REAL,:,:), allocatable :: pt
      INT :: n_pt
   DIE_IF(NOT allocated(self%atom),"MOLECULE.PUT:put_ED_refinement_plots ... no atoms to fit")
   DIE_IF(NOT allocated(self%crystal),"MOLECULE.PUT:put_ED_refinement_plots ... no crystal")
   DIE_IF(NOT self%crystal%fragment_info_made,"MOLECULE.PUT:put_ED_refinement_plots ... no crystal fragment info")
   DIE_IF(NOT allocated(self%crystal%xray_data),"MOLECULE.PUT:put_ED_refinement_plots ... no crystal xray data")

      ! Set up
      if (NOT allocated(self%plot_grid)) call create_(self%plot_grid)

      ! Use unit cell as bbox if requested
      call set_box_scale_factor_(self%plot_grid,ONE)
      if (is_zero_(self%plot_grid%desired_separation)) call set_desired_separation_(self%plot_grid,0.1*BOHR_PER_ANGSTROM)
      call set_center_to_center_of_(self%plot_grid,self%atom)
      call set_bbox_from_unit_cell_(self%plot_grid,self%crystal%unit_cell,midpt_is_center=FALSE)

      ! Make the residual map
      n_pt = self%plot_grid%n_pt
      call create_(map,n_pt)
      call set_plot_kind_(self%plot_grid,"residual_density")
      call make_points_(self%plot_grid,pt)
      call make_residual_density_cell_(self,map)

      ! Write cube file
      call dump_plot_grid_(self,map,"residual_density",form="gaussian.cube")

      ! Clean
      call destroy_(map)
   end subroutine


!  ==========================
!  Plot printing and deletion
!  ==========================

   subroutine dump_plot_grid(self,grid,name,form)
   ! Dump (write) out the plot "grid" to an appropriate file with
   ! "name", in various formats "form", otherwise (like most things)
   ! taken from the .plot_grid settings.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), INOUT :: grid
      STR(len=*), IN :: name
      STR(len=*), optional, IN :: form
      MAT(REAL,:,:), allocatable :: pos
      VEC(INT,:), allocatable :: valid
      VEC(type(EVEC_INT_TYPE),:), allocatable :: bonds
      type(ARCHIVE_TYPE) :: arch
      STR(len=STR_SIZE) :: title,fmt
      REAL :: fac,z_range
   ENSURE(allocated(self%plot_grid),"MOLECULE.PUT:dump_plot_grid ... no grid")
   ENSURE(allocated(self%atom),"MOLECULE.PUT:dump_plot_grid ... no atoms")
      if (self%plot_grid%plot_units/=" ") then
         fac = conversion_factor_(self%plot_grid%plot_units)
         grid = fac*grid
      end if
      if (self%plot_grid%use_plot_max) then
         where (grid>self%plot_grid%plot_max) grid = self%plot_grid%plot_max
      end if
      if (self%plot_grid%use_plot_min) then
         where (grid<self%plot_grid%plot_min) grid = self%plot_grid%plot_min
      end if

      ! Set title
      title = self%name
      if (allocated(self%SCF_data)) then
         if (is_constrained_wavefunction_(self%SCF_data)) then
            title = trim(title)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6"))
         end if
      end if
      if (allocated(self%plot_grid)) then
         if (self%plot_grid%plot_label/=" ") then
            title = trim(title)//","//trim(self%plot_grid%plot_label)
         end if
      end if

      ! Get plot format
      fmt = self%plot_grid%plot_format
      if (present(form)) fmt = form

      ! Dump all formats if none specified
      if (fmt=="all") then
         call set_(arch,title,name,format="contour")
         call write_contour_a_la_nancy_(arch,grid,self%plot_grid,self%atom)
         call set_(arch,title,name,format="drishti.raw")
         call write_drishti_(arch,grid,self%plot_grid%n_x,self%plot_grid%n_y,self%plot_grid%n_z)
         call set_(arch,title,name,format="gnuplot")
         call write_gnuplot_(arch,grid,self%plot_grid%n_x,self%plot_grid%n_y,self%plot_grid%n_z)
         call set_(arch,title,name,format="gaussian.cube")
         call write_gaussian_cube_(arch,grid,self%plot_grid,self%atom,name)
         call set_(arch,title,name,format="mathematica")
         call write_mathematica_(arch,grid,self%plot_grid%n_x,self%plot_grid%n_y,self%plot_grid%n_z)
         call set_(arch,title,name,format="spackman")
         call write_contour_a_la_spackman_(arch,grid,self%plot_grid,self%atom)
         call set_(arch,title,name,format="vapor.raw")
         call write_vapor_(arch,grid,self%plot_grid%n_x,self%plot_grid%n_y,self%plot_grid%n_z)
         call set_(arch,title,name,format="vtk")
         call write_vtk_(arch,grid,self%plot_grid,name)
         call set_(arch,title,name,format="xdgraph")
         call write_a_la_xdgraph_(arch,grid,self%plot_grid,self%atom)
         call set_(arch,title,name,format="xcrysden.xsf")
         call write_xcrysden_xsf_(arch,grid,self%plot_grid,self%atom,name)
         call set_(arch,title,name,format="cell.cube")
         call write_gaussian_cell_(arch,grid,self%plot_grid,self%crystal%unit_cell_atom,name)
         if (self%plot_grid%n_z==1) then
            call set_(arch,title,name,format="mathematica")
            z_range = ONE
            call get_rotoranged_atom_positions_(self%plot_grid,pos,z_range,valid)
            call make_connection_table_(self%atom(valid),bonds)
            call write_mathematica_(arch,grid,self%plot_grid,self%plot_grid%atom(valid),pos,bonds)
            call destroy_(bonds)
            call destroy_(pos)
            call destroy_(valid)
         end if

      ! Dump specified format
      else
         select case (fmt)

         case ("contour")
            call set_(arch,title,name,format="contour")
            call write_contour_a_la_nancy_(arch,grid,self%plot_grid,self%atom)

         case ("cube")
            call set_(arch,title,name,format="gaussian.cube")
            call write_gaussian_cube_(arch,grid,self%plot_grid,self%atom,name)

         case ("drishti")
            call set_(arch,title,name,format="drishti.raw")
            call write_drishti_(arch,grid,self%plot_grid%n_x,self%plot_grid%n_y,self%plot_grid%n_z)

         case ("gaussian","gaussian.cube")
            call set_(arch,title,name,format="gaussian.cube")
            call write_gaussian_cube_(arch,grid,self%plot_grid,self%atom,name)

         case ("cell.cube")
            call set_(arch,title,name,format="cell.cube")
            call write_gaussian_cell_(arch,grid,self%plot_grid,self%crystal%unit_cell_atom,name)

         case ("gnuplot")
            call set_(arch,title,name,format="gnuplot")
            call write_gnuplot_(arch,grid,self%plot_grid%n_x,self%plot_grid%n_y,self%plot_grid%n_z)

         case ("gnuplot.contour")

            ! Get rotated positions and data
            call make_connection_table_(self%atom,bonds)
            call get_rotated_atom_positions_(self%plot_grid,pos)

            ! Make the gnuplot plot archive
            ! Use all commas so that .eps can be handled by latex
            call set_(arch,title,name,format="gnuplot.contour")
            call set_use_all_commas_(arch,TRUE)
            call write_gnuplot_contour_(arch,grid,self%plot_grid,pos,bonds)

            ! Clean
            call destroy_(pos)
            call destroy_(bonds)

         case ("spackman")
            call set_(arch,title,name,format="spackman")
            call write_contour_a_la_spackman_(arch,grid,self%plot_grid,self%atom)

         case ("mathematica")
            call set_(arch,title,name,format="mathematica")
            call write_mathematica_(arch,grid,self%plot_grid%n_x,self%plot_grid%n_y,self%plot_grid%n_z)
            if (self%plot_grid%n_z==1) then
               call set_(arch,title,name,format="mathematica")
               z_range = ONE
               call get_rotoranged_atom_positions_(self%plot_grid,pos,z_range,valid)
               call make_connection_table_(self%atom(valid),bonds)
               call write_mathematica_(arch,grid,self%plot_grid,self%plot_grid%atom(valid),pos,bonds)
               call destroy_(bonds)
               call destroy_(pos)
               call destroy_(valid)
            end if

         case ("vapor")
            call set_(arch,title,name,format="vapor.raw")
            call write_vapor_(arch,grid,self%plot_grid%n_x,self%plot_grid%n_y,self%plot_grid%n_z)

         case ("vtk")
            call set_(arch,title,name,format="vtk")
            call write_vtk_(arch,grid,self%plot_grid,name)

         case ("xcrysden")
            call set_(arch,title,name,format="xcrysden.xsf")
            call write_xcrysden_xsf_(arch,grid,self%plot_grid,self%atom,name)

         case ("xdgraph")
            call set_(arch,title,name,format="xdgraph")
            call write_a_la_xdgraph_(arch,grid,self%plot_grid,self%atom)

         case default
            allocate(tonto%known_keywords(14))
            tonto%known_keywords(1) = "contour"
            tonto%known_keywords(2) = "cube"
            tonto%known_keywords(3) = "drishti"
            tonto%known_keywords(4) = "gaussian"
            tonto%known_keywords(5) = "gaussian.cube"
            tonto%known_keywords(6) = "cell.cube"
            tonto%known_keywords(7) = "gnuplot"
            tonto%known_keywords(8) = "gnuplot.contour"
            tonto%known_keywords(9) = "spackman"
            tonto%known_keywords(10) = "mathematica"
            tonto%known_keywords(11) = "vapor"
            tonto%known_keywords(12) = "vtk"
            tonto%known_keywords(13) = "xcrysden"
            tonto%known_keywords(14) = "xdgraph"
            call unknown_(tonto,fmt,"MOLECULE.PUT:dump_plot_grid")
            deallocate(tonto%known_keywords)
         end select
      end if
   end subroutine


! Other program processing ...

!  ==============
!  Write XYZ File 
!  ==============

   subroutine put_xyz_file(self)
   ! Prints out an asymmetric-unit xyz file.
   ! cartesian axes.
      type(MOLECULE_TYPE), IN :: self
      STR(len=STR_SIZE) :: name
      type(TEXTFILE_TYPE), allocatable :: xyzfile
      INT :: i
      STR(len=STR_SIZE), parameter :: atomnumber = "(I3)"
      STR(len=STR_SIZE), parameter :: comment = "(' written by tonto')"
      STR(len=STR_SIZE), parameter :: atom_format = "(A5, 3F12.6)"
      name = self%name
      if (NOT exhausted_(stdin%buffer)) call read_(stdin,name)
      call create_(xyzfile,trim(name)//".xyz")
      call open_for_(xyzfile,"write")
      call put_(xyzfile,self%n_atom)
      call flush_(xyzfile)
      call put_(xyzfile,trim(name)//" computed by TONTO "//TONTO_VERSION//" v. "//GIT_VERSION)
      call flush_(xyzfile)
      do i = 1,self%n_atom
         call put_(xyzfile,self%atom(i)%label)
         call put_(xyzfile,self%atom(i)%position(1)*ANGSTROM_PER_BOHR)
         call put_(xyzfile,self%atom(i)%position(2)*ANGSTROM_PER_BOHR)
         call put_(xyzfile,self%atom(i)%position(3)*ANGSTROM_PER_BOHR)
         call flush_(xyzfile)
      end do

      ! Cleanup
      call close_(xyzfile)
      call destroy_(xyzfile)
   end subroutine

   subroutine put_xyz_frag_file(self)
   ! Prints out an fragment xyz file.
   ! cartesian axes.
      type(MOLECULE_TYPE), IN :: self
      STR(len=STR_SIZE) :: name
      type(TEXTFILE_TYPE), allocatable :: xyzfile
      INT :: i
      INT :: n_atom
      STR(len=STR_SIZE), parameter :: atomnumber = "(I3)"
      STR(len=STR_SIZE), parameter :: comment = "(' written by tonto')"
      STR(len=STR_SIZE), parameter :: atom_format = "(A5, 3F12.6)"
      name = self%name
      if (NOT exhausted_(stdin%buffer)) call read_(stdin,name)
      call create_(xyzfile,trim(name)//".xyz")
      call open_for_(xyzfile,"write")
      n_atom = self%crystal%n_fragment_atoms
      call put_(xyzfile,n_atom)
      call flush_(xyzfile)
      call put_(xyzfile,trim(name)//" computed by TONTO "//TONTO_VERSION//" v. "//GIT_VERSION)
      call flush_(xyzfile)
      do i = 1,n_atom
         call put_(xyzfile,self%atom(i)%atomic_number)
!         xyzfile.put(.atom(i).label)
         call put_(xyzfile,self%atom(i)%position(1)*ANGSTROM_PER_BOHR)
         call put_(xyzfile,self%atom(i)%position(2)*ANGSTROM_PER_BOHR)
         call put_(xyzfile,self%atom(i)%position(3)*ANGSTROM_PER_BOHR)
         call flush_(xyzfile)
      end do

      ! Cleanup
      call close_(xyzfile)
      call destroy_(xyzfile)
   end subroutine

   subroutine put_xtal23_xyz_file(self)
   ! Prints out an asymmetric-unit xyz file.
   ! cartesian axes.
      type(MOLECULE_TYPE), IN :: self
      STR(len=STR_SIZE) :: name
      type(TEXTFILE_TYPE), allocatable :: xyzfile
      INT :: i
      INT :: n_atom
      STR(len=STR_SIZE), parameter :: atomnumber = "(I3)"
      STR(len=STR_SIZE), parameter :: comment = "(' written by tonto')"
      STR(len=STR_SIZE), parameter :: atom_format = "(A5, 3F12.6)"
      name = self%name
      if (NOT exhausted_(stdin%buffer)) call read_(stdin,name)
      call create_(xyzfile,trim(name)//".xyz")
      call open_for_(xyzfile,"write")
      n_atom = self%crystal%n_asymmetric_unit_atoms
      call put_(xyzfile,n_atom)
      call flush_(xyzfile)
      call put_(xyzfile,trim(name)//" computed by TONTO "//TONTO_VERSION//" v. "//GIT_VERSION)
      call flush_(xyzfile)
      do i = 1,n_atom

!         xyzfile.put(.atom(i).atomic_number)
         call put_(xyzfile,self%crystal%asymmetric_unit_atom(i)%atomic_number)
         call put_(xyzfile,self%crystal%asymmetric_unit_geometry(1,i))
         call put_(xyzfile,self%crystal%asymmetric_unit_geometry(2,i))
         call put_(xyzfile,self%crystal%asymmetric_unit_geometry(3,i))
         call flush_(xyzfile)
      end do

      ! Cleanup
      call close_(xyzfile)
      call destroy_(xyzfile)
   end subroutine


!  ======================
!  Gaussian chk interface
!  ======================

   subroutine put_tonto_FChk_MO(self)
   ! Write the MO's in a way which can replace g09 fchk file section
      type(MOLECULE_TYPE), IN :: self
      type(ARCHIVE_TYPE) :: arch
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.PUT:put_tonto_FChk_MO ... spherical basis not implemented")
   ENSURE(allocated(self%MOs),"MOLECULE.PUT:put_tonto_FChk_MO ... no MO's")
      call set_(arch,self%name,"gaussian_MO",genre=genre_(self%MOs),format="ascii")
      call write_(arch,self%MOs,by_column=TRUE,ascii=TRUE)
   end subroutine

   subroutine put_tonto_FChk_DM(self)
   ! Write the MO's in a way which can replace g09 fchk file section
      type(MOLECULE_TYPE), IN :: self
      type(ARCHIVE_TYPE) :: arch
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.PUT:put_tonto_FChk_DM ... spherical basis not implemented")
   ENSURE(allocated(self%density_mx),"MOLECULE.PUT:put_tonto_FChk_DM ... no MO's")
      call set_(arch,self%name,"gaussian_DM",genre=genre_(self%density_mx),format="ascii")
      call write_(arch,self%density_mx,by_column=TRUE,ascii=TRUE)
   end subroutine


!  ========================
!  AIM2000/Morphy interface
!  ========================

! Can we get rid of these beow, from this module? --dylan
! The don't fit in PUT

   subroutine put_morphy98_wfn_file(self)
   ! Write a morphy98 .wfn file
      type(MOLECULE_TYPE), IN :: self
      STR(len=STR_SIZE) :: name
      type(TEXTFILE_TYPE), allocatable :: wfnfile
      VEC(INT,:), allocatable :: lvec
      VEC(REAL,:), allocatable :: evec
      MAT(REAL,:,:), allocatable :: dmatrix,cc
      type(SHELL_TYPE), allocatable :: sh
      INT :: i,j,pcount,n_orbitals,a,l,atomn,shelln
      STR(len=STR_SIZE), parameter :: n_vars_format = "(A8, 10X, I5, 15X, I5, 15X, I5, 17X)"
      STR(len=STR_SIZE), parameter :: atom_format = "(A5, I3, '    (CENTRE', 1I3, ') ', 3F12.8, '          ', F5.1)"
      STR(len=STR_SIZE), parameter :: c_assignment_format = "('CENTRE ASSIGNMENTS  ', 20I3)"
      STR(len=STR_SIZE), parameter :: t_assignment_format = "('TYPE ASSIGNMENTS    ', 20I3)"
      STR(len=STR_SIZE), parameter :: exponent_format = "('EXPONENTS ', 5D14.7)"
      STR(len=STR_SIZE), parameter :: mol_title_format = "(1A4, I3, 1A30, F12.8, 1A15, F12.8)"
      STR(len=STR_SIZE), parameter :: coefficient_format = "(5D16.8)"
      STR(len=STR_SIZE), parameter :: e_v_format = "(' THE HF ENERGY = ', F20.12, ' THE VIRIAL(-V/T)=', F13.8)"
   ENSURE(is_allocated_with_genre_(self%NO_occ_nos,"r "),"MOLECULE.PUT:put_morphy98_wfn_file ... No occupation numbers")
   ENSURE(is_allocated_with_genre_(self%MO_energies,"r "),"MOLECULE.PUT:put_morphy98_wfn_file ... No orbital energies")
   ENSURE(is_allocated_with_genre_(self%MOs,"r "),"MOLECULE.PUT:put_morphy98_wfn_file ... No orbitals")
      name = self%name
      if (NOT exhausted_(stdin%buffer)) call read_(stdin,name)
      call create_(wfnfile,trim(name)//".wfn")
      call open_for_(wfnfile,"write")
      if (IO_IS_ALLOWED) then
         if (mod(self%n_e,2)==0) then
            n_orbitals = self%n_e/2
         else
            n_orbitals = (self%n_e+1)/2
         end if
         write(unit=wfnfile%unit,fmt='(a)') trim(name)//" computed by TONTO "//TONTO_VERSION//" v. "//GIT_VERSION
         write(unit=wfnfile%unit,fmt=n_vars_format) "GAUSSIAN",n_orbitals,self%n_prim,self%n_atom
         write(unit=wfnfile%unit,fmt=atom_format) (self%atom(i)%label,i,i,self%atom(i)%position,REALIFY(self%atom(i)%atomic_number),i=1,self%n_atom)
         write(unit=wfnfile%unit,fmt=c_assignment_format) ((j,i=1,no_of_primitives_(self%atom(j)%basis)),j=1,self%n_atom)
         pcount = 1
         call create_(lvec,self%n_prim)
         call create_(evec,self%n_prim)
         do a = 1,self%n_shell
            atomn = self%atom_for_shell(a)
            shelln = self%atom_shell_for_shell(a)
            sh = self%atom(atomn)%basis%shell(shelln)
            l = n_comp_up_to_(sh%l)-sh%n_comp
            do j = 1,sh%n_cc
               if (sh%l==3) then
                  lvec(pcount) = l+1
                  lvec(pcount+1) = l+2
                  lvec(pcount+2) = l+3
                  lvec(pcount+3) = l+4
                  lvec(pcount+4) = l+5
                  lvec(pcount+5) = l+7
                  lvec(pcount+6) = l+6
                  lvec(pcount+7) = l+8
                  lvec(pcount+8) = l+9
                  lvec(pcount+9) = l+10
                  do i = 1,sh%n_comp
                     evec(pcount) = sh%exponent(j)
                     pcount = pcount+1
                  end do
               else
                  do i = 1,sh%n_comp
                     evec(pcount) = sh%exponent(j)
                     lvec(pcount) = l+i
                     pcount = pcount+1
                  end do
               end if
            end do
         end do
         write(unit=wfnfile%unit,fmt=t_assignment_format) lvec
         write(unit=wfnfile%unit,fmt=exponent_format) evec
         call destroy_(lvec)
         call destroy_(evec)
         call create_(dmatrix,self%n_prim,self%n_bf)
         call create_(cc,self%n_prim,self%n_bf)
         call make_contraction_mx_(self,cc)
         call to_product_of_(dmatrix,cc,self%MOs%r)
         do i = 1,n_orbitals
            write(unit=wfnfile%unit,fmt=mol_title_format) "MO  ",i,"OCC NO = ",self%NO_occ_nos%r(i)," ORB. ENERGY = ",self%MO_energies%r(i)
            write(unit=wfnfile%unit,fmt=coefficient_format) dmatrix(:,i)
         end do
         write(unit=wfnfile%unit,fmt="(1A8)") "END DATA"
         if (allocated(self%SCF_data)) then
            write(unit=wfnfile%unit,fmt=e_v_format) self%SCF_data%energy,TWO
         else
            write(unit=wfnfile%unit,fmt=e_v_format) ZERO,TWO
         end if

         ! Clean
         call destroy_(cc)
         call destroy_(dmatrix)
      end if
      call close_(wfnfile)
      call destroy_(wfnfile)
   end subroutine

   subroutine put_aim2000_wfn_file(self)
   ! Writes a proaim2000 .wfn file
      type(MOLECULE_TYPE), IN :: self
      STR(len=STR_SIZE) :: name
      type(TEXTFILE_TYPE), allocatable :: wfnfile
      VEC(INT,:), allocatable :: lvec
      VEC(REAL,:), allocatable :: evec
      MAT(REAL,:,:), allocatable :: dmatrix,cc
      INT :: i,j,pcount,n_orbitals,a,l,atomn,shelln
      type(SHELL_TYPE), allocatable :: sh
      REAL :: occ
      STR(len=STR_SIZE), parameter :: n_vars_format = "(A8, 10X, I5,' MOL ORBITALS', I7,' PRIMITIVES', I9,' NUCLEI')"
      STR(len=STR_SIZE), parameter :: atom_format = "(A5, I3, '    (CENTRE', 1I3, ') ', 3F12.8, '  CHARGE =', F5.1)"
      STR(len=STR_SIZE), parameter :: c_assignment_format = "('CENTRE ASSIGNMENTS  ', 20I3)"
      STR(len=STR_SIZE), parameter :: t_assignment_format = "('TYPE ASSIGNMENTS    ', 20I3)"
      STR(len=STR_SIZE), parameter :: exponent_format = "('EXPONENTS ', 5D14.7)"
      STR(len=STR_SIZE), parameter :: mol_title_format = "('MO  ', I3,'    MO 0.0        OCC NO =',F13.7,'  ORB. ENERGY =',F12.7)"
      STR(len=STR_SIZE), parameter :: coefficient_format = "(5D16.8)"
      STR(len=STR_SIZE), parameter :: e_v_format = "(' THE HF ENERGY = ', F20.12, ' THE VIRIAL(-V/T)=', F13.8)"
   ENSURE(is_allocated_with_genre_(self%MO_energies,"r "),"MOLECULE.PUT:put_aim2000_wfn_file ... No orbital energies")
   ENSURE(is_allocated_with_genre_(self%MOs,"r "),"MOLECULE.PUT:put_aim2000_wfn_file ... No orbitals")
      name = self%name
      if (NOT exhausted_(stdin%buffer)) call read_(stdin,name)
      call create_(wfnfile,trim(name)//".wfn")
      call open_for_(wfnfile,"write")
      if (IO_IS_ALLOWED) then
         if (mod(self%n_e,2)==0) then
            n_orbitals = self%n_e/2
         else
            n_orbitals = (self%n_e+1)/2
         end if
         write(unit=wfnfile%unit,fmt='(a)') trim(name)//" computed by TONTO "//TONTO_VERSION//" v. "//GIT_VERSION
         write(unit=wfnfile%unit,fmt=n_vars_format) "GAUSSIAN",n_orbitals,self%n_prim,self%n_atom
         write(unit=wfnfile%unit,fmt=atom_format) (self%atom(i)%label,i,i,self%atom(i)%position,REALIFY(self%atom(i)%atomic_number),i=1,self%n_atom)
         write(unit=wfnfile%unit,fmt=c_assignment_format) ((j,i=1,no_of_primitives_(self%atom(j)%basis)),j=1,self%n_atom)
         pcount = 1
         call create_(lvec,self%n_prim)
         call create_(evec,self%n_prim)
         do a = 1,self%n_shell
            atomn = self%atom_for_shell(a)
            shelln = self%atom_shell_for_shell(a)
            sh = self%atom(atomn)%basis%shell(shelln)
            l = n_comp_up_to_(sh%l)-sh%n_comp
            do j = 1,sh%n_cc
               if (sh%l==3) then
                  lvec(pcount) = l+1
                  lvec(pcount+1) = l+2
                  lvec(pcount+2) = l+3
                  lvec(pcount+3) = l+4
                  lvec(pcount+4) = l+5
                  lvec(pcount+5) = l+7
                  lvec(pcount+6) = l+6
                  lvec(pcount+7) = l+8
                  lvec(pcount+8) = l+9
                  lvec(pcount+9) = l+10
                  do i = 1,sh%n_comp
                     evec(pcount) = sh%exponent(j)
                     pcount = pcount+1
                  end do
               else
                  do i = 1,sh%n_comp
                     evec(pcount) = sh%exponent(j)
                     lvec(pcount) = l+i
                     pcount = pcount+1
                  end do
               end if
            end do
         end do
         write(unit=wfnfile%unit,fmt=t_assignment_format) lvec
         write(unit=wfnfile%unit,fmt=exponent_format) evec
         call destroy_(lvec)
         call destroy_(evec)
         call create_(dmatrix,self%n_prim,self%n_bf)
         call create_(cc,self%n_prim,self%n_bf)
         call make_contraction_mx_(self,cc)
         call to_product_of_(dmatrix,cc,self%MOs%r)
         do i = 1,n_orbitals
            occ = ZERO
            if (i<=self%n_a) occ = TWO
            write(unit=wfnfile%unit,fmt=mol_title_format) i,occ,self%MO_energies%r(i)
            write(unit=wfnfile%unit,fmt=coefficient_format) dmatrix(:,i)
         end do
         write(unit=wfnfile%unit,fmt="(1A8)") "END DATA"
         if (allocated(self%SCF_data)) then
            write(unit=wfnfile%unit,fmt=e_v_format) self%SCF_data%energy,2.0D00
         else
            write(unit=wfnfile%unit,fmt=e_v_format) ZERO,2.0D00
         end if

         ! Clean
         call destroy_(cc)
         call destroy_(dmatrix)
      end if
      call close_(wfnfile)
      call destroy_(wfnfile)
   end subroutine

   subroutine put_florian_wfn_file(self,with_lambda)
   ! Writes a full wavefunction .ffn file
      type(MOLECULE_TYPE), IN :: self
      BIN, optional, IN :: with_lambda
      STR(len=STR_SIZE) :: name
      type(TEXTFILE_TYPE), allocatable :: wfnfile
      VEC(INT,:), allocatable :: lvec
      VEC(REAL,:), allocatable :: evec
      MAT(REAL,:,:), allocatable :: dmatrix,cc
      type(SHELL_TYPE), allocatable :: sh
      INT :: i,j,pcount,a,l,atomn,shelln
      REAL :: occ
      BIN :: avec_lambda
      STR(len=STR_SIZE), parameter :: n_vars_format = "(A8, 10X, I5,' MOL ORBITALS', I7,' PRIMITIVES', I9,' NUCLEI')"
      STR(len=STR_SIZE), parameter :: atom_format = "(A5, I3, '    (CENTRE', 1I3, ') ', 3F12.8, '  CHARGE =', F5.1)"
      STR(len=STR_SIZE), parameter :: c_assignment_format = "('CENTRE ASSIGNMENTS  ', 20I3)"
      STR(len=STR_SIZE), parameter :: t_assignment_format = "('TYPE ASSIGNMENTS    ', 20I3)"
      STR(len=STR_SIZE), parameter :: exponent_format = "('EXPONENTS ', 5D14.7)"
      STR(len=STR_SIZE), parameter :: mol_title_format = "('MO ', I4,'    MO 0.0        OCC NO =',F13.7,'  ORB. ENERGY =',F12.7)"
      STR(len=STR_SIZE), parameter :: coefficient_format = "(5D16.8)"
      STR(len=STR_SIZE), parameter :: e_v_format = "(' THE HF ENERGY = ', F20.12, ' THE VIRIAL(-V/T)=', F13.8)"
   ENSURE(is_allocated_with_genre_(self%MO_energies,"r "),"MOLECULE.PUT:put_florian_wfn_file ... No orbital energies")
   ENSURE(is_allocated_with_genre_(self%MOs,"r "),"MOLECULE.PUT:put_florian_wfn_file ... No orbitals")
      avec_lambda = FALSE
      if (present(with_lambda)) avec_lambda = with_lambda
      name = self%name
      if (NOT exhausted_(stdin%buffer)) call read_(stdin,name)
      if (avec_lambda) then
         call create_(wfnfile,trim(name)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6"))//".ffn")
      else
         call create_(wfnfile,trim(name)//".ffn")
      end if
      call open_for_(wfnfile,"write")
      if (IO_IS_ALLOWED) then
         write(unit=wfnfile%unit,fmt='(a)') trim(name)//" computed by TONTO "//TONTO_VERSION//" v. "//GIT_VERSION
         write(unit=wfnfile%unit,fmt=n_vars_format) "GAUSSIAN",self%n_bf,self%n_prim,self%n_atom
         write(unit=wfnfile%unit,fmt=atom_format) (self%atom(i)%label,i,i,self%atom(i)%position,REALIFY(self%atom(i)%atomic_number),i=1,self%n_atom)
         write(unit=wfnfile%unit,fmt=c_assignment_format) ((j,i=1,no_of_primitives_(self%atom(j)%basis)),j=1,self%n_atom)
         pcount = 1
         call create_(lvec,self%n_prim)
         call create_(evec,self%n_prim)
         do a = 1,self%n_shell
            atomn = self%atom_for_shell(a)
            shelln = self%atom_shell_for_shell(a)
            sh = self%atom(atomn)%basis%shell(shelln)
            l = n_comp_up_to_(sh%l)-sh%n_comp
            do j = 1,sh%n_cc
               if (sh%l==3) then
                  lvec(pcount) = l+1
                  lvec(pcount+1) = l+2
                  lvec(pcount+2) = l+3
                  lvec(pcount+3) = l+4
                  lvec(pcount+4) = l+5
                  lvec(pcount+5) = l+7
                  lvec(pcount+6) = l+6
                  lvec(pcount+7) = l+8
                  lvec(pcount+8) = l+9
                  lvec(pcount+9) = l+10
                  do i = 1,sh%n_comp
                     evec(pcount) = sh%exponent(j)
                     pcount = pcount+1
                  end do
               else
                  do i = 1,sh%n_comp
                     evec(pcount) = sh%exponent(j)
                     lvec(pcount) = l+i
                     pcount = pcount+1
                  end do
               end if
            end do
         end do
         write(unit=wfnfile%unit,fmt=t_assignment_format) lvec
         write(unit=wfnfile%unit,fmt=exponent_format) evec
         call destroy_(lvec)
         call destroy_(evec)
         call create_(dmatrix,self%n_prim,self%n_bf)
         call create_(cc,self%n_prim,self%n_bf)
         call make_contraction_mx_(self,cc)
         call to_product_of_(dmatrix,cc,self%MOs%r)
         do i = 1,self%n_bf
            occ = ZERO
            if (i<=self%n_a) occ = TWO
            write(unit=wfnfile%unit,fmt=mol_title_format) i,occ,self%MO_energies%r(i)
            write(unit=wfnfile%unit,fmt=coefficient_format) dmatrix(:,i)
         end do
         write(unit=wfnfile%unit,fmt="(1A8)") "END DATA"
         if (allocated(self%SCF_data)) then
            write(unit=wfnfile%unit,fmt=e_v_format) self%SCF_data%energy,2.0D00
         else
            write(unit=wfnfile%unit,fmt=e_v_format) ZERO,2.0D00
         end if

         ! Clean
         call destroy_(cc)
         call destroy_(dmatrix)
      end if
      call close_(wfnfile)
      call destroy_(wfnfile)
   end subroutine


!  ============
!  NBO analysis
!  ============

   subroutine put_NBO_file_47(self)
   ! Output the current associated density matrix
      type(MOLECULE_TYPE), IN :: self
      INT :: i,j,Z
      VEC(REAL,3) :: pos
      VEC(INT,:), allocatable :: ind,fbf,lbf,ang
      type(OPMATRIX_TYPE), allocatable :: mx
      STR(len=STR_SIZE) :: title
   ENSURE(allocated(self%density_mx),"MOLECULE.PUT:put_NBO_file_47 ... no density matrix")

      ! Set title
      title = self%name
      if (allocated(self%SCF_data)) then
         if (is_constrained_wavefunction_(self%SCF_data)) then
            title = trim(title)//",lambda="//trim(to_str_(self%SCF_data%lambda,"f9.6"))
         end if
      end if
      call redirect_(stdout,trim(title)//".NBO,file_47")
!     stdout.redirect(trim(.name)//".NBO,file_47")

      ! NOTE: these are RAW Fortran writes on stdout.unit, reaching around the
      ! TEXTFILE API, so they get none of its IO_IS_ALLOWED guarding. Under MPI
      ! only the master actually opens the redirected file (TEXTFILE.open is
      ! guarded) and its unit is then broadcast, so a non-master rank writing
      ! here dies with "Unit number is negative and unit was not already opened
      ! with OPEN(NEWUNIT=...)". This is write-only output, so the fix is just
      ! the guard -- nothing needs broadcasting. The computations between the
      ! writes stay replicated on every rank, exactly as before.
      if (IO_IS_ALLOWED) then
         write(stdout%unit,"(1x,'$COORD')")
         write(stdout%unit,"(1x,a)") trim(self%name)
      end if
      do i = 1,self%n_atom
         Z = self%atom(i)%atomic_number
         pos = self%atom(i)%atomic_number*ANGSTROM_PER_BOHR
         if (IO_IS_ALLOWED) then
            write(stdout%unit,"(1x,2i5,3f15.6)") Z,Z,pos
         end if
      end do
      if (IO_IS_ALLOWED) then
         write(stdout%unit,"(1x,'$END')")
         write(stdout%unit,"(1x,'$BASIS')")
      end if
      call make_atom_for_basis_fn_in_(self%atom,ind)
      if (IO_IS_ALLOWED) then
         write(stdout%unit,"(1x,' CENTER =',11i6,/,(10x,11i6))") (ind(i),i=1,size(ind))
         write(stdout%unit,"(1x,'$END')")
      end if
      fbf = self%first_basis_fn_for_shell
      lbf = self%last_basis_fn_for_shell
      ang = self%angular_moment_for_shell
      mx = self%density_mx
      call renormalize_(mx,fbf,lbf,ang)
      if (IO_IS_ALLOWED) then
         write(stdout%unit,"(1x,'$DENSITY')")
         write(stdout%unit,"((1x,5e20.12))") ((mx%r(i,j),i=j,self%n_bf),j=1,self%n_bf)
         write(stdout%unit,"(1x,'$END')")
      end if
      mx = self%fock_mx
      call renormalize_(mx,fbf,lbf,ang)
      if (IO_IS_ALLOWED) then
         write(stdout%unit,"(1x,'$FOCK')")
         write(stdout%unit,"((1x,5e20.12))") ((mx%r(i,j),i=j,self%n_bf),j=1,self%n_bf)
         write(stdout%unit,"(1x,'$END')")
      end if
      call destroy_(mx)
      call revert_(stdout)
   end subroutine

end module
