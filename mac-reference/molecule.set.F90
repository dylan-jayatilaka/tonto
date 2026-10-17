!===============================================================================
!
! MOLECULE.SET
!
! Memory allocation, create & destroy, setting, intializing, updating,
! resolving and reading from disk, and deleting serialized things from
! disk.
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
module MOLECULE_SET_MODULE

#  include "macros"

   include "molecule.set.use"

   implicit none

   include "molecule.set.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(MOLECULE_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"MOLECULE.SET:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(MOLECULE_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MOLECULE.SET:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of a molecule object
      type(MOLECULE_TYPE), INOUT :: self

      ! Saved molecule
      call destroy_(self%saved)

      ! Atoms (nullify pointed-to atoms before)
      call destroy_(self%atom)

      ! Atom_groups
      call destroy_atom_group_(self)

      ! Destroy bases and pointed
      call destroy_(self%basis)
      call destroy_(self%slaterbasis)
      call destroy_(self%coppensbasis)
      call destroy_(self%auxiliary_basis)
      call destroy_(self%auxiliary_basis_for_atom)
      call destroy_(self%RI_metric_factor)

      ! Grids, interpolators & plots
      call destroy_(self%plot_grid)
      call destroy_(self%cell_map)
      call destroy_(self%interpolator)
      call destroy_(self%isosurface)
      call destroy_(self%becke_grid)
      call destroy_XC_grid_(self)
      call destroy_COSX_grid_(self)

      ! Crystals and clusters
      call destroy_(self%crystal)
      call destroy_(self%cluster)
      call destroy_(self%cif)
      call destroy_(self%pointgroup)

      ! Basis set & integral info arrays
      call destroy_(self%atom_for_shell)
      call destroy_(self%atom_shell_for_shell)
      call destroy_(self%first_shell_for_atom)
      call destroy_(self%last_shell_for_atom)
      call destroy_(self%basis_shell_for_shell)
      call destroy_(self%first_basis_fn_for_shell)
      call destroy_(self%last_basis_fn_for_shell)
      call destroy_(self%angular_moment_for_shell)
      call destroy_(self%first_basis_fn_for_atom)
      call destroy_(self%last_basis_fn_for_atom)
      call destroy_(self%basis_shell1pair)
      call destroy_(self%max_I)
      call destroy_(self%overlapping_atoms_for_atom)

      ! SCF data
      call destroy_(self%SCF_data)

      ! SCF info
      call destroy_(self%MO_energies)
      call destroy_(self%MOs)
      call destroy_(self%density_mx)
      call destroy_(self%old_density_mx)
      call destroy_(self%delta_density_mx)
      call destroy_(self%NOs)
      call destroy_(self%NO_occ_nos)
      call destroy_(self%overlap_mx)
      call destroy_(self%overlap_mx_evecs)
      call destroy_(self%overlap_mx_evals)
      call destroy_(self%max_S)
      call destroy_(self%max_I)
      call destroy_(self%kinetic_energy_mx)
      call destroy_(self%nuclear_attraction_mx)

      ! SCF intermediates
      call destroy_(self%core_mx)
      call destroy_(self%fock_mx)
      call destroy_(self%fock_2e_mx)
      call destroy_(self%constraint_mx)

      ! Bond analysis
      call destroy_(self%roby)

      ! Localised MO's
      call destroy_(self%localiser)

      ! CPHF and response properties
      call destroy_(self%U_electric_dipole)
      call destroy_(self%dipole_polarisability)
      call destroy_(self%dipole_hyperpolarisability)
      call destroy_(self%U2_electric_dipole)

      ! Force constants
      call destroy_(self%force_constants)
      call destroy_(self%normal_mode_eigenvectors)
      call destroy_(self%normal_mode_eigenvalues)
      call destroy_(self%stiff_mode_ADPs)
      call destroy_(self%phi3_force_constants)
      call destroy_(self%phi4_force_constants)

      ! Atomic multipoles
      call destroy_(self%charges)
      call destroy_(self%dipoles)
      call destroy_(self%octupoles)
      call destroy_(self%quadrupoles)
   end subroutine

   PURE subroutine save(self)
   ! Save everything about "self" in ".saved", and make "self" fresh.
      type(MOLECULE_TYPE), allocatable, INOUT :: self
      type(MOLECULE_TYPE), allocatable :: saved

      ! Keep only one level of saved
      if (allocated(self%saved)) then
         call destroy_(self%saved)
      end if

      ! Save self
      saved = self

      ! New self
      call destroy_(self)
      call create_(self)

      ! Set .saved
   ! .saved = saved
      call move_alloc(saved,self%saved)
   end subroutine

   PURE subroutine unsave(self)
   ! Revert back to a previously saved molecule stored in .saved, and
   ! destroy everything about the current molecule in "self".
   ! WARNING: be careful when setting up self, make sure none of its
   ! pointer parts refer to .saved information.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%saved),"MOLECULE.SET:unsave ... no previous settings")

      ! Restore self
      self = self%saved

      ! Clean up
      call destroy_(self%saved)
   end subroutine


!  ================
!  Specific cleanup
!  ================

   PURE subroutine destroy_atom_group(self)
   ! Destroy the pointer parts of a molecule object
      type(MOLECULE_TYPE), INOUT :: self
      call destroy_(self%atom_group)
      if (allocated(self%mol)) deallocate(self%mol)
   end subroutine

   PURE subroutine destroy_matrices(self)
   ! Destroy the matrices of a molecule object
      type(MOLECULE_TYPE), INOUT :: self
      call destroy_(self%atom_for_shell)
      call destroy_(self%atom_shell_for_shell)
      call destroy_(self%first_shell_for_atom)
      call destroy_(self%last_shell_for_atom)
      call destroy_(self%first_basis_fn_for_shell)
      call destroy_(self%last_basis_fn_for_shell)
      call destroy_(self%angular_moment_for_shell)
      call destroy_(self%first_basis_fn_for_atom)
      call destroy_(self%last_basis_fn_for_atom)
      call destroy_(self%MO_energies)
      call destroy_(self%MOs)
      call destroy_(self%density_mx)
      call destroy_(self%old_density_mx)
      call destroy_(self%delta_density_mx)
      call destroy_(self%NOs)
      call destroy_(self%NO_occ_nos)
      call destroy_(self%overlap_mx)
      call destroy_(self%overlap_mx_evecs)
      call destroy_(self%overlap_mx_evals)
      call destroy_(self%max_S)
      call destroy_(self%max_I)
      call destroy_(self%kinetic_energy_mx)
      call destroy_(self%nuclear_attraction_mx)
      call destroy_(self%core_mx)
      call destroy_(self%fock_mx)
      call destroy_(self%fock_2e_mx)
      call destroy_(self%constraint_mx)

   ! .atom_group.destroy
      call destroy_atom_group_(self)
      call destroy_(self%elmo_connection_table)
   end subroutine

   PURE subroutine destroy_NAO_matrices(self)
   ! Destroy the NAO's for all atoms in the molecule.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.SET:destroy_NAO_matrices ... no atoms")
      call destroy_NAO_matrices_(self%atom)
   end subroutine

   PURE subroutine destroy_ANO_matrices(self)
   ! Destroy the ANO's for all atoms in the molecule.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.SET:destroy_ANO_matrices ... no atoms")
      call destroy_ANO_matrices_(self%atom)
   end subroutine

   PURE subroutine destroy_interpolators(self)
   ! Destroy the (non-unique) interpolators
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.SET:destroy_interpolators ... no atoms")
      call destroy_interpolators_(self%atom)
   end subroutine

   PURE subroutine destroy_atom_data(self)
   ! Destroy the restricted atomic natural orbitals (ANO) data for all
   ! atoms in the molecule.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.SET:destroy_atom_data ... no atoms")
      call destroy_atom_data_(self%atom)
   end subroutine

   PURE subroutine destroy_atom_shell_info(self)
   ! Destroy the atom precomputed shellpair info.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.SET:destroy_atom_shell_info ... no atoms")
      call destroy_shell_info_(self%atom)
   end subroutine


!  SCF

   PURE subroutine destroy_SCF_results(self,keep_MOs,keep_DM)
   ! Destroy the SCF results in memory
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: keep_MOs
      BIN, optional, IN :: keep_DM
      BIN :: delete_MOs
      BIN :: delete_DM
      delete_MOs = TRUE
      if (present(keep_MOs)) delete_MOs = NOT keep_MOs
      delete_DM = TRUE
      if (present(keep_DM)) delete_DM = NOT keep_DM
      if (delete_MOs) call destroy_(self%MOs)
      if (delete_DM) call destroy_(self%density_mx)
      call destroy_(self%MO_energies)
      call destroy_(self%fock_mx)
   end subroutine

   PURE subroutine destroy_tmp_SCF_matrices(self)
   ! Destroy any temporary matrices stored in memory.
      type(MOLECULE_TYPE), INOUT :: self
      call destroy_(self%old_density_mx)
      call destroy_(self%delta_density_mx)
      call destroy_(self%fock_2e_mx)
      call destroy_(self%constraint_mx)
   end subroutine

   PURE subroutine destroy_SCF_integrals(self)
   ! Destroy all SCF-related integrals in memory.
      type(MOLECULE_TYPE), INOUT :: self
      call destroy_(self%max_I)
      call destroy_(self%overlap_mx)
      call destroy_(self%overlap_mx_evecs)
      call destroy_(self%overlap_mx_evals)
      call destroy_(self%kinetic_energy_mx)
      call destroy_(self%nuclear_attraction_mx)
      call destroy_(self%core_mx)
   end subroutine


!  ============
!  Set defaults
!  ============

   subroutine set_defaults(self)
   ! Set up a default molecule
      type(MOLECULE_TYPE), INOUT :: self
      self%name = "unknown"
      self%charge = 0
      self%spin_multiplicity = 1
      self%n_e = 0
      self%n_a = 0
      self%n_b = 0
      self%E_field = ZERO
      self%B_field = ZERO
      self%gauge_origin = ZERO
      self%n_atom = 0
      self%atom_info_made = FALSE
      self%n_atom_kind = 0
      self%n_bf = 0
      self%basis_name = " "
      self%n_basis = 0
      self%basis_l_max = -1
      self%use_spherical_basis = FALSE
      self%n_bf = 0
      self%n_prim = 0
      self%slaterbasis_name = " "
      self%coppensbasis_name = " "
      self%basis_info_made = FALSE
      self%use_interpolators = TRUE
      self%n_shell = 0
      self%n_shell_pairs = 0
      self%n_unique_shells = 0
      self%CX_file_name = " "
      self%CX_uses_angstrom = TRUE
      self%nucleus_model = "point"

      ! Do want to set this always?
      ! e.g. for .make_mol_from_atom(a) methods?
      self%partition_info_made = FALSE
      self%u_HA_moments_made = FALSE
      self%atomic_polarisabilities_made = FALSE
      if (allocated(self%atom)) then
         self%name = chemical_formula_(self%atom)
         self%spin_multiplicity = default_spin_multiplicity_(self)
         ! Set unique atom tags if needed, multiplicity, no. of
         ! electrons; resolve basis sets, set basis-shell info; 
         ! and (possibly) resent kinds
         call update_atom_info_(self)
         call resolve_bases_and_update_(self)  ! NOT PURE
         call update_kinds_(self%atom)
         self%n_atom_kind = maxval(self%atom%kind)
      end if
   end subroutine

   subroutine set_SCF_guess_defaults_from(self,SCF_data)
   ! Set scf  data options for a guess-type SCF defaults
      type(MOLECULE_TYPE), INOUT :: self
      type(SCF_DATA_TYPE), allocatable, IN :: SCF_data

      ! Set the SCF defaults (leaky)
      if (NOT allocated(SCF_data)) then
         call set_SCF_defaults_(self)  ! NOT PURE
         call set_output_(self%SCF_data,FALSE)
      else if (SCF_data%SCF_kind(1:7)=="fraghar") then
         call set_SCF_defaults_(self,SCF_data)  ! leaky
      else if (NOT SCF_data%use_default_guess_options) then
         call set_SCF_defaults_(self,SCF_data)  ! leaky
      else
         call set_SCF_defaults_(self)
         call set_output_(self%SCF_data,FALSE)
      end if

      ! Reset some defaults from SCF_data, if it is there
      if (allocated(SCF_data)) then
         call set_output_(self%SCF_data,SCF_data%guess_output)
         if (SCF_data%initial_MOs/="--using density--") call set_initial_MOs_(self%SCF_data,SCF_data%initial_MOs)
         if (SCF_data%initial_density/="--using MO's--") call set_initial_density_(self%SCF_data,SCF_data%initial_density)
         ! A guess SCF converges to .guess_convergence, NOT to whatever the
         ! parent asked for. These are the atomic calculations behind the
         ! ANOs, the promolecule and the Hirshfeld weights: an internal
         ! reference, not the user's answer, so a job run at convergence=
         ! 1e-4 must not silently get atomic densities good only to 1e-4.
         ! It is settable -- guess_convergence= in scfdata= -- and echoed by
         ! SCF_DATA:put_options under "SCF termination options".
         call set_convergence_(self%SCF_data,SCF_data%guess_convergence)
         call set_guess_convergence_(self%SCF_data,SCF_data%guess_convergence)
         call set_convergence_tolerance_(self%SCF_data%SCF_DIIS,SCF_data%SCF_DIIS%convergence_tolerance)
         call set_relativity_kind_(self%SCF_data,SCF_data%relativity_kind)

         ! Iteration-control and convergence-acceleration options. Without these
         ! a guess SCF that will not converge (e.g. an open-shell atom with a
         ! partly-filled degenerate shell, where damping and level shift both
         ! quit at iteration 3 just as DIIS takes over) cannot be rescued from
         ! the input file at all -- the user's settings were silently discarded.
         ! NOTE on ordering: set_max_iterations ENSUREs val>=.min_iterations and
         ! set_min_iterations ENSUREs .max_iterations>=val, so max must go first
         ! or raising both at once trips the precondition in a debug build. The
         ! guards likewise mirror each setter's ENSUREs, since the parent object
         ! may carry unset (zero) values.
         if (SCF_data%max_iterations>=0) call set_max_iterations_(self%SCF_data,SCF_data%max_iterations)
         if (SCF_data%min_iterations>=0) call set_min_iterations_(self%SCF_data,SCF_data%min_iterations)
         call set_using_damping_(self%SCF_data,SCF_data%using_damping)
         if (SCF_data%damp_factor>ZERO AND SCF_data%damp_factor<=ONE) then
            call set_damp_factor_(self%SCF_data,SCF_data%damp_factor)
         end if
         ! NOTE: set_damp_finish also sets level_shift_finish, so it must precede
         ! set_level_shift_finish below.
         if (SCF_data%damp_finish>0) call set_damp_finish_(self%SCF_data,SCF_data%damp_finish)
         call set_using_level_shift_(self%SCF_data,SCF_data%using_level_shift)
         call set_level_shift_(self%SCF_data,SCF_data%level_shift)
         if (SCF_data%level_shift_finish>0) then
            call set_level_shift_finish_(self%SCF_data,SCF_data%level_shift_finish)
         end if
      end if

      ! Nuclear energy
      self%SCF_data%nuclear_repulsion_energy = nuclear_repulsion_energy_(self%atom)

      ! Finalise
      call update_(self%SCF_data)
   end subroutine

   subroutine set_general_SCF_guess_defaults_from(self,SCF_data)
   ! Set scf  data options for a guess-type SCF defaults
      type(MOLECULE_TYPE), INOUT :: self
      type(SCF_DATA_TYPE), allocatable, IN :: SCF_data

      ! Set the SCF defaults (leaky)
      if (NOT allocated(SCF_data)) then
         call set_SCF_defaults_(self)  ! NOT PURE
         call set_output_(self%SCF_data,FALSE)
      else if (SCF_data%use_default_guess_options) then
         call set_SCF_defaults_(self)
         call set_output_(self%SCF_data,FALSE)
      else
         call set_SCF_defaults_(self,SCF_data)  ! leaky
      end if
      call set_SCF_kind_(self%SCF_data,"ghf")

      ! Reset some defaults from SCF_data, if it is there
      if (allocated(SCF_data)) then
         call set_output_(self%SCF_data,SCF_data%guess_output)
         call set_convergence_(self%SCF_data,SCF_data%convergence)
         call set_convergence_tolerance_(self%SCF_data%SCF_DIIS,SCF_data%SCF_DIIS%convergence_tolerance)
         call set_relativity_kind_(self%SCF_data,SCF_data%relativity_kind)
      end if

      ! Nuclear energy
      self%SCF_data%nuclear_repulsion_energy = nuclear_repulsion_energy_(self%atom)

      ! Finalise
      call update_(self%SCF_data)
   end subroutine

   subroutine set_SCF_defaults(self,SCF_data)
   ! Set up scf defaults for molecule
      type(MOLECULE_TYPE), INOUT :: self
      type(SCF_DATA_TYPE), allocatable, optional, IN :: SCF_data
      if (NOT allocated(self%SCF_data)) then; call create_(self%SCF_data)
      else; call set_defaults_(self%SCF_data)
      end if

      ! Restricted?
      if (self%spin_multiplicity==1) call set_SCF_kind_(self%SCF_data,"rhf")
      if (self%spin_multiplicity/=1) call set_SCF_kind_(self%SCF_data,"uhf")
      if (present(SCF_data)) then
         if (allocated(SCF_data)) then

         ! Set defaults from SCF_data
            self%SCF_data = SCF_data
            call destroy_(self%SCF_data%cluster)
            call destroy_(self%SCF_data%cluster_charges)
            call destroy_(self%SCF_data%cluster_charge_positions)
            if (self%SCF_data%SCF_kind/=" ") then

            ! Process scfkind , make sure it is sensible
            ! Always use restricted
               if (is_DFT_calculation_(self%SCF_data)) then
                  if (self%spin_multiplicity==1) call set_SCF_kind_(self%SCF_data,"rks")
                  if (self%spin_multiplicity/=1) then

               ! DIE("cannot yet do ROKS (for, e.g. atomic SCF)")
                     call set_SCF_kind_(self%SCF_data,"uks")
                  end if
               else
                  if (self%spin_multiplicity==1) call set_SCF_kind_(self%SCF_data,"rhf")
                  if (self%spin_multiplicity/=1) then
                     call set_SCF_kind_(self%SCF_data,"uhf")
               !  .SCF_data.set_using_DIIS(FALSE)
                  end if
               end if
            end if
         end if
      end if
      self%SCF_data%nuclear_repulsion_energy = nuclear_repulsion_energy_(self%atom)
      if (is_DFT_calculation_(self%SCF_data)) then
         call initialize_DFT_grids_(self)  ! NOT PURE
      end if
   end subroutine

   subroutine set_group_becke_grids(self)
   ! Give every fragment .mol(g) the integration-grid settings of self. Each
   ! fragment builds its own grid, for its own atoms, but must integrate on the
   ! settings the user gave the parent.
   !
   ! MUST be called before the fragment grids are built -- .initialize_DFT_grids
   ! and MOLECULE.RHO:set_up_becke_grid create a grid only if there is none, and
   ! keep the settings of one that is already there, so this has to get in
   ! first. Without it a fragment silently integrated at the BECKE_GRID
   ! defaults, and so becke_grid= keywords and hart's --grid-accuracy did
   ! nothing whatever on a fragHAR job.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: g
      if (NOT allocated(self%becke_grid)) return
      if (NOT allocated(self%mol)) return
      do g = 1,size(self%mol)
         if (NOT allocated(self%mol(g)%becke_grid)) call create_(self%mol(g)%becke_grid)
         call set_settings_from_(self%mol(g)%becke_grid,self%becke_grid)
      end do
   end subroutine

   subroutine initialize_DFT_grids(self)
   ! Initialise DFT grids, if not already done so.
   ! NOTE: create only if there is no grid yet. Destroying and recreating
   ! unconditionally discarded everything the "becke_grid= { ... }" input
   ! block had just set -- accuracy, kind, pruning_scheme, the cutoffs --
   ! and silently reverted every DFT run to the type defaults, while
   ! "put_basics" went on echoing the settings the user asked for. The
   ! ".set_grid_data" below rebuilds the grids unconditionally and is
   ! leaky, so a changed accuracy is still picked up and moved atoms are
   ! still handled. Same pattern as MOLECULE.RHO:set_up_becke_grid.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SET:initialize_DFT_grids ... no SCF_data")
      if (NOT allocated(self%becke_grid)) call create_(self%becke_grid)
      call set_atom_info_(self%becke_grid,self%atom)  ! NOT PURE
      call set_grid_data_(self%becke_grid)

      ! The molecular XC grid is built from these, once, on first use
      call destroy_XC_grid_(self)
      call set_overlapping_atoms_for_atom_(self)
   end subroutine

   PURE subroutine destroy_XC_grid(self)
   ! Destroy the molecular DFT grid (MOLECULE.RHO:make_XC_grid); it is
   ! rebuilt on demand. Lives here, not in RHO, because SET calls it and
   ! RHO already depends on SET.
      type(MOLECULE_TYPE), INOUT :: self
      call destroy_XC_batches_(self)
      call destroy_(self%XC_last_pt)
      call destroy_(self%XC_first_pt)
      call destroy_(self%XC_index)
      call destroy_(self%XC_wts)
      call destroy_(self%XC_pts)
   end subroutine

   PURE subroutine destroy_COSX_grid(self)
   ! Destroy the grid for the chain-of-spheres exchange and its batches
   ! (MOLECULE.FOCK:initialize_COSX).
      type(MOLECULE_TYPE), INOUT :: self
      call destroy_(self%COSX_fit)
      call destroy_(self%COSX_batch_last_shell)
      call destroy_(self%COSX_batch_first_shell)
      call destroy_(self%COSX_batch_shell)
      call destroy_(self%COSX_batch_last)
      call destroy_(self%COSX_batch_first)
      call destroy_(self%COSX_batch_pt)
      call destroy_(self%COSX_wts)
      call destroy_(self%COSX_pts)
      call destroy_(self%COSX_grid)
   end subroutine

   PURE subroutine destroy_XC_batches(self)
   ! Destroy the batches of the molecular DFT grid (MOLECULE.RHO:make_XC_batches).
      type(MOLECULE_TYPE), INOUT :: self
      call destroy_(self%XC_batch_last_shell)
      call destroy_(self%XC_batch_first_shell)
      call destroy_(self%XC_batch_shell)
      call destroy_(self%XC_batch_last)
      call destroy_(self%XC_batch_first)
      call destroy_(self%XC_batch_pt)
   end subroutine

   PURE subroutine set_overlapping_atoms_for_atom(self)
   ! Make the list of overlapping atoms for a given atom
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: r_max
      VEC(REAL,3) :: d
      REAL :: r
      INT :: a,b,n_atom_pairs
   ENSURE(allocated(self%becke_grid),"MOLECULE.SET:set_overlapping_atoms_for_atom ... no becke_grid")
      n_atom_pairs = no_of_atom_pairs_(self)

      ! Overlapping atoms list array (the molecular XC grid depends on it)
      call destroy_XC_grid_(self)
      call destroy_(self%overlapping_atoms_for_atom)
      call create_(self%overlapping_atoms_for_atom,self%n_atom)

      ! Make the maximum radii
      call create_(r_max,self%n_atom)
      do a = 1,self%n_atom
         r_max(a) = r_max_(self%atom(a),self%becke_grid%basis_fn_cutoff)
      end do

      ! Make the overlapping atom arrays
      do a = 1,self%n_atom

         ! Atom a overlaps itself
         call append_(self%overlapping_atoms_for_atom(a)%element,a)

         ! Loop over other atoms "b"
         do b = 1,a-1

            ! Distance "r" to atom "b"
            d = self%atom(a)%position-self%atom(b)%position
            r = norm2(d)
            if (r>(r_max(a)+r_max(b))) cycle

            ! Atoms "a" and "b" overlap
            call append_(self%overlapping_atoms_for_atom(a)%element,b)
            call append_(self%overlapping_atoms_for_atom(b)%element,a)
         end do
      end do
      call destroy_(r_max)

      ! Reorder the lists
      do a = 1,self%n_atom
         call sort_(self%overlapping_atoms_for_atom(a)%element)
      end do
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_name(self,val)
   ! Set the molecule name
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: val
   ENSURE(all(element_symbols/=val),"MOLECULE.SET:set_name ... job name must not be an element symbol")
      self%name = val
   end subroutine

   PURE subroutine set_charge(self,val)
   ! Generic set
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%charge = val
   end subroutine

   PURE subroutine set_spin_multiplicity(self,val)
   ! Set the spin multiplicity and no. of alpha & beta electrons
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val/=0,"MOLECULE.SET:set_spin_multiplicity ... cannot have zero multiplicity")
      self%spin_multiplicity = val
      if (allocated(self%atom)) then
         self%n_e = no_of_electrons_(self)
         self%n_a = no_of_alpha_electrons_(self)
         self%n_b = no_of_beta_electrons_(self)
      end if
   end subroutine

   PURE subroutine set_n_a_n_b_and_multiplicity(self,n_a,n_b)
   ! Set the no. of alpha and beta electrons, and then the spin
   ! multiplicity. This is needed only for Molden files!
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: n_a
      INT, IN :: n_b
   ENSURE(n_a>=n_b,"MOLECULE.SET:set_n_a_n_b_and_multiplicity ... n_a can't be smaller than n_b")
      self%n_a = n_a
      self%n_b = n_b
      self%spin_multiplicity = (n_a-n_b)+1
   end subroutine

   PURE subroutine set_SCF_occupations_for_NOs(self,genre)
   ! Set the SCF occupation numbers for the NOs
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: genre
      select case (genre)

      case ("r ")
         self%NO_occ_nos%r(1:self%n_a) = TWO

      case ("u ")
         self%NO_occ_nos%a(1:self%n_a) = ONE
         self%NO_occ_nos%b(1:self%n_b) = ONE

      case ("gc")
         self%NO_occ_nos%g(1:self%n_e) = ONE
      end select
   end subroutine

   PURE subroutine set_force_constants(self,fc)
   ! Set the .force_constants to the flat array "fc".
   ! Since it is a symmetric matrix the order does not matter.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: fc
      INT :: n3n
   ENSURE(allocated(self%atom),"MOLECULE.SET:set_force_constants ... no atom list")
   ENSURE(size(fc)==9*size(self%atom)*size(self%atom),"MOLECULE.SET:set_force_constants ... wrong size, fc")

      ! Create space
      n3n = 3*size(self%atom)
      call destroy_(self%force_constants)
      call create_(self%force_constants,n3n,n3n)

      ! Reshape
      self%force_constants = reshape(fc,[n3n,n3n])

      ! Its method is not known, so no default frequency scale
      self%FD_hessian_scale = -ONE
   end subroutine

   PURE subroutine set_partition_info_made(self,val)
   ! Generic set
      type(MOLECULE_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%partition_info_made = val
   end subroutine

   PURE subroutine set_atom_partition_model(self,val)
   ! Set the the type of atom partition model to use.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: val
      VEC(STR(len=STR_SIZE),:), allocatable :: options
      options = ["oc-crystal23","crystal23   ","oc-hirshfeld","hirshfeld   ","oc-salvador ","salvador    ","tfva        "]
      ENSURE(is_one_of_(val,options),"MOLECULE.SET:set_atom_partition_model ... unknown partition_model, "//trim(val))
      self%atom_partition_model = val
      call to_lower_case_(self%atom_partition_model)

      ! Rename to standard form
      select case (self%atom_partition_model)
      case ("crystal23   "); self%atom_partition_model = "hirshfeld"
      case ("hirshfeld   "); self%atom_partition_model = "hirshfeld"
      case ("salvador    "); self%atom_partition_model = "salvador "
      case ("oc-crystal23"); self%atom_partition_model = "hirshfeld"
      case ("oc-hirshfeld"); self%atom_partition_model = "hirshfeld"
      case ("oc-salvador "); self%atom_partition_model = "salvador "
      case ("tfva        "); self%atom_partition_model = "salvador "
      end select
   end subroutine

   PURE subroutine set_u_HA_moments_made(self,val)
   ! Generic set
      type(MOLECULE_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%u_HA_moments_made = val
   end subroutine

   PURE subroutine set_atomic_polarisabilities_made(self,val)
   ! Generic set
      type(MOLECULE_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%atomic_polarisabilities_made = val
   end subroutine


!  ===================
!  Atoms & atom groups
!  ===================

   PURE subroutine update_atom_info(self)
   ! Set the atom information:
   ! . Unique atom tags and kind indices
   ! . The spin multiplicity
   ! . The number of alpha and beta electrons.
   ! WARNING: .atom.update depends on .atom.basis, so you might need
   !          to call this routine again!
      type(MOLECULE_TYPE), INOUT :: self
      if (NOT allocated(self%atom)) return
   ! if (.atom_info_made) return

      ! Finalize 
      call update_(self%atom)

      ! No. of atoms & no. of atom kinds
      self%n_atom = size(self%atom)
      self%n_atom_kind = maxval(self%atom%kind)

      ! Reset multiplicity?
      if (NOT has_valid_no_of_beta_electrons_(self)) then

      !  WARN("Inconsistent charge and multiplicity")
      !  WARN("Reassigning the multiplicity")
         self%spin_multiplicity = default_spin_multiplicity_(self)
      end if

      ! Now can set # of electrons
      self%n_e = no_of_electrons_(self)
      self%n_a = no_of_alpha_electrons_(self)
      self%n_b = no_of_beta_electrons_(self)

      ! Make connetion table
      call make_connection_table_(self%atom,self%atoms_bonded_to_atom)

   ! .atom_info_made = TRUE
   end subroutine


! How is below different from update_atom_groups?
! It should have a better name ...

   subroutine update_group_crystal_and_ANOs(self,skip_ANOs)
   ! Update .atom_group information (ANOs, crystal, SCF guess ...)
   ! WARNING: atom_group(:).mol.crystal's are clones of .crystal
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: skip_ANOs
      INT :: g
   ENSURE(allocated(self%atom),"MOLECULE.SET:update_group_crystal_and_ANOs ... atom groups must be defined after atoms=")
   ENSURE(allocated(self%atom_group),"MOLECULE.SET:update_group_crystal_and_ANOs ... no atom_group")
   ENSURE(allocated(self%mol),"MOLECULE.SET:update_group_crystal_and_ANOs ... no .mol")
   ENSURE(size(self%mol)==size(self%atom_group),"MOLECULE.SET:update_group_crystal_and_ANOs ... wrong size, .mol")

      ! Below sounds not good?
      if (NOT allocated(self%SCF_data)) then
         call set_SCF_defaults_(self)  ! NOT PURE
         call set_output_(self%SCF_data,FALSE)
      end if
      do g = 1,size(self%atom_group)

         ! Don't remake ANO data if there
         if (present(skip_ANOs)) then
            if (NOT skip_ANOs) then
               call resolve_ANOs_from_(self%mol(g),self,self%atom_group(g)%atom_index)
            end if
         end if

         ! Assign the crystal & gegenerate unit cell atoms
         if (allocated(self%crystal)) then
            self%mol(g)%crystal = self%crystal  ! Should be minimal copy?
            call update_fragment_info_(self%mol(g)%crystal,self%mol(g)%atom)
         end if
      end do
   end subroutine


!  ===========================================
!  Set new molecule from an atom or atom_group
!  ===========================================

   subroutine set_molecule_from_atom(self,n,mol)
   ! Make a fully fledged molecule "mol" from a single atom "n" in self
   ! The new molecule is ready for an SCF calculation.
   ! DANGER: see warnings below
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: n
      type(MOLECULE_TYPE), OUT :: mol
      INT :: i,k
   ENSURE(allocated(self%atom),"MOLECULE.SET:set_molecule_from_atom ... no atom info")
   ENSURE(n<=self%n_atom,"MOLECULE.SET:set_molecule_from_atom ... atom number n too big")

      ! Create the single atom and copy its pointers
      ! WARNING: dont destroy ptr parts of mol.atom
      mol%atom = self%atom(n:n)
      call set_position_(mol%atom(1),[ZERO,ZERO,ZERO])

      ! Set default molecule e.g. the name, atom_info
      ! and default multiplicity ...
      call set_defaults_(mol)  ! NOT PURE

      ! Copy the basis set and set the basis info
      ! WARNING: dont destroy basis of mol
      call resolve_bases_and_update_from_(mol,self)
      call update_atom_info_(mol)

      ! Don't remake ANO data if there
      i = 0
      call resolve_ANOs_from_(mol,self,[(i,i = n,n)])

      ! Set guess-type SCF defaults
      call set_SCF_guess_defaults_from_(mol,self%SCF_data)

      ! Set nondefault atomic default state
      if (allocated(self%SCF_data%initial_atomic_states)) then
         k = self%atom(n)%kind
         call set_charge_(mol,self%SCF_data%initial_atomic_states(k,1))
         call set_spin_multiplicity_(mol,self%SCF_data%initial_atomic_states(k,2))
      end if
   end subroutine

   subroutine set_molecule_from_atom_group(self,g,mol,set_name)
   ! Make a fully fledged molecule "mol" from group "g" of atoms
   ! specified in .atom_group(g).element(:), ready for an SCF
   ! calculation.
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: g
      type(MOLECULE_TYPE), OUT :: mol
      BIN, optional, IN :: set_name
      STR(len=STR_SIZE) :: name
      BIN :: reset_name
   ENSURE(allocated(self%atom),"MOLECULE.SET:set_molecule_from_atom_group ... no atom info")
   ENSURE(allocated(self%atom_group),"MOLECULE.SET:set_molecule_from_atom_group ... no atom_group info")
   ENSURE(g<=size(self%atom_group),"MOLECULE.SET:set_molecule_from_atom_group ... group number g too big")
   ENSURE(g>=1,"MOLECULE.SET:set_molecule_from_atom_group ... group number g less than 1!")
      reset_name = TRUE
      if (present(set_name)) reset_name = set_name

      ! Create the single atom and copy its pointers
      ! WARNING: dont destroy ptr parts of mol.atom
      mol%atom = self%atom(self%atom_group(g)%atom_index)

      ! Set default molecule e.g. the name, atom_info
      ! and default multiplicity ...
      call set_defaults_(mol)  ! NOT PURE

      ! Reset .name after .set_defaults
      name = self%atom_group(g)%name
      if (reset_name) then
         if (name==" ") then
            name = "group-"//trim(to_str_(g))//"-"
            name = trim(name)//trim(chemical_formula_(mol%atom,with_spaces=FALSE))
            call set_name_(mol,name)
            call set_name_(self%atom_group(g),name)
         end if
      end if

      ! Set charge & multiplicity.
      !
      ! An explicitly-set group multiplicity wins over the derived default --
      ! that is what .spin_multiplicity_set is for, and it is how the Ryde
      ! capped path already behaves (MOLECULE.BASE:set_Ryde_cap_for_group).
      ! Until this was added, a multiplicity given for a group made from the
      ! connection table -- the ordinary case of several molecules in the
      ! asymmetric unit -- was read, stored, and then silently overwritten on
      ! the line below.
      mol%charge = self%atom_group(g)%charge
      if (self%atom_group(g)%spin_multiplicity_set) then
         mol%spin_multiplicity = self%atom_group(g)%spin_multiplicity
      else
         mol%spin_multiplicity = default_spin_multiplicity_(mol)
      end if

      ! Copy the basis set and set the basis info
      ! WARNING: dont destroy basis of mol !
      call resolve_bases_and_update_from_(mol,self)
      call update_atom_info_(mol)

      ! Don't remake ANO data if there
      call resolve_ANOs_from_(mol,self,self%atom_group(g)%atom_index)

      ! Make atom info. No recursive groups. Yet?
      mol%atom_info_made = FALSE
      call update_atom_and_basis_info_no_grp_(mol)

      ! Assign the crystal & gegenerate unit cell atoms
      if (allocated(self%crystal)) then
         mol%crystal = self%crystal
         call update_fragment_info_(mol%crystal,mol%atom)
      end if

      ! Set guess-type SCF defaults
      call set_SCF_guess_defaults_from_(mol,self%SCF_data)

      ! Reset SCF_kind for fraghar calculations
      if (allocated(self%SCF_data)) then
         if (self%SCF_data%SCF_kind(1:8)=="fraghar-") then
            call set_SCF_kind_(mol%SCF_data,self%SCF_data%SCF_kind(9:11))
         end if
      end if

      ! Set density matrix from parent?
      ! Restricted only. Do not clobber
      if (allocated(self%density_mx)) then
         if (allocated(self%density_mx%r)) then
            if (NOT allocated(mol%density_mx)) then
               call create_(mol%density_mx,mol%n_bf,"r ")
               call put_group_density_to_(self,mol%density_mx%r,g,mol)
            end if
         end if
      end if
   end subroutine

   subroutine update_atom_and_basis_info_no_grp(self)
   ! Update the atom labels, no. of electrons, spin multiplicities,
   ! axis system, crystal fragment info, and bases; remove isosurface.
      type(MOLECULE_TYPE), INOUT :: self

      ! Set unique atom tags if needed, multiplicity, no. of
      ! electrons; resolve basis sets, set basis-shell info; 
      ! and (possibly) resent kinds
      call update_atom_info_(self)
      call resolve_bases_and_update_(self)
      call update_kinds_(self%atom)
      self%n_atom_kind = maxval(self%atom%kind)

      ! Finalize crystal
      if (allocated(self%crystal)) then
         call resolve_axis_system_(self)
         call update_fragment_info_(self%crystal,self%atom)
      end if

      ! Destroy stored atoms here
      call destroy_(self%isosurface)
      self%atom_info_made = TRUE
   end subroutine


!  ===============
!  Atom group info
!  ===============

   PURE subroutine set_connected_atom_groups(self)
   ! Make .atom_group's from the connection table.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: n_group,g

      ! Set group indices, if possible
      call set_connected_groups_(self%atom)
      if (has_groups_(self%atom)) then

         ! Each .atom() had a .group index
         n_group = no_of_groups_(self%atom)
         if (NOT allocated(self%atom_group)) then
            call create_(self%atom_group,n_group)
         else
            ENSURE(n_group==size(self%atom_group),"MOLECULE.SET:set_connected_atom_groups ... inconsistent .atom_group size")
         end if

         ! Make groups from group index
         do g = 1,size(self%atom_group)
            call set_atom_index_(self%atom_group(g),indices_of_elements_matching_(self%atom(:)%group,g))
            call set_n_atoms_(self%atom_group(g),size(self%atom_group(g)%atom_index))
         end do

         ! Set charges
         if (allocated(self%atom_group_charges)) then
            ENSURE(size(self%atom_group_charges)==size(self%atom_group),"MOLECULE.SET:set_connected_atom_groups ... Wrong size, .atom_group_charges")
            do g = 1,size(self%atom_group)
               call set_charge_(self%atom_group(g),self%atom_group_charges(g))
            end do
         end if
      end if
   end subroutine


!  =============
!  Group density
!  =============

! These can be made much better if the atoms
! in the groups are assumed contigous.

   PURE subroutine put_group_density_to(self,P,g,mol)
   ! Put the atom group "g" in the whole .density matrix into "P".
   ! "mol" is a molecule created from the appropriate atom group "g".
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: P
      INT, IN :: g
      type(MOLECULE_TYPE), IN :: mol
      INT :: a,b,aa,bb,fa,la,fb,lb,faa,laa,fbb,lbb
   ENSURE(allocated(self%atom_group),"MOLECULE.SET:put_group_density_to ... no atom group information")
   ENSURE(g>0,"MOLECULE.SET:put_group_density_to ... group number must be positive")
   ENSURE(g<=size(self%atom_group),"MOLECULE.SET:put_group_density_to ... group number exceeds number of groups")
      P = ZERO
      do a = 1,self%atom_group(g)%n_atoms  ! Copy this atom density
         do b = 1,self%atom_group(g)%n_atoms
            aa = self%atom_group(g)%atom_index(a)
            bb = self%atom_group(g)%atom_index(b)
            faa = self%first_basis_fn_for_atom(aa)
            fbb = self%first_basis_fn_for_atom(bb)
            laa = self%last_basis_fn_for_atom(aa)
            lbb = self%last_basis_fn_for_atom(bb)
            fa = mol%first_basis_fn_for_atom(a)
            fb = mol%first_basis_fn_for_atom(b)
            la = mol%last_basis_fn_for_atom(a)
            lb = mol%last_basis_fn_for_atom(b)
            P(fa:la,fb:lb) = self%density_mx%r(faa:laa,fbb:lbb)
         end do
      end do
   end subroutine

   PURE subroutine set_group_density_from(self,P,g,mol)
   ! Set the atom group "g" density matrix from "P". "mol" must be a
   ! molecule created from the appropriate atom group "g".
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: P
      INT, IN :: g
      type(MOLECULE_TYPE), IN :: mol
      INT :: a,b,aa,bb,fa,la,fb,lb,faa,laa,fbb,lbb
   ENSURE(allocated(self%atom_group),"MOLECULE.SET:set_group_density_from ... no atom group information")
   ENSURE(g>0,"MOLECULE.SET:set_group_density_from ... group number must be positive")
   ENSURE(g<=size(self%atom_group),"MOLECULE.SET:set_group_density_from ... group number exceeds number of groups")
      do a = 1,self%atom_group(g)%n_atoms  ! Copy this atom density
         do b = 1,self%atom_group(g)%n_atoms
            aa = self%atom_group(g)%atom_index(a)
            bb = self%atom_group(g)%atom_index(b)
            fa = mol%first_basis_fn_for_atom(a)
            fb = mol%first_basis_fn_for_atom(b)
            faa = self%first_basis_fn_for_atom(aa)
            fbb = self%first_basis_fn_for_atom(bb)
            la = mol%last_basis_fn_for_atom(a)
            lb = mol%last_basis_fn_for_atom(b)
            laa = self%last_basis_fn_for_atom(aa)
            lbb = self%last_basis_fn_for_atom(bb)
            self%density_mx%r(faa:laa,fbb:lbb) = P(fa:la,fb:lb)
         end do
      end do
   end subroutine

   PURE subroutine set_group_density(self,g,mol)
   ! Set the atom group "g" .density_mx from "P". "mol" must be a
   ! molecule created from the appropriate atom group "g".
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: g
      type(MOLECULE_TYPE), IN :: mol
      INT :: a,b,ga,gb
      INT :: fga,lga,fa,la
      INT :: fgb,lgb,fb,lb
   ENSURE(allocated(self%atom_group),"MOLECULE.SET:set_group_density ... no atom group information")
   ENSURE(g>0,"MOLECULE.SET:set_group_density ... group number must be positive")
   ENSURE(g<=size(self%atom_group),"MOLECULE.SET:set_group_density ... group number exceeds number of groups")
   ENSURE(allocated(self%density_mx),"MOLECULE.SET:set_group_density ... no density_mx")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.SET:set_group_density ... no restricted density_mx")
      do a = 1,self%atom_group(g)%n_atoms
         do b = 1,self%atom_group(g)%n_atoms
            ga = self%atom_group(g)%atom_index(a)
            gb = self%atom_group(g)%atom_index(b)
            fa = mol%first_basis_fn_for_atom(a)
            fga = self%first_basis_fn_for_atom(ga)
            fb = mol%first_basis_fn_for_atom(b)
            fgb = self%first_basis_fn_for_atom(gb)
            la = mol%last_basis_fn_for_atom(a)
            lga = self%last_basis_fn_for_atom(ga)
            lb = mol%last_basis_fn_for_atom(b)
            lgb = self%last_basis_fn_for_atom(gb)
            self%density_mx%r(fga:lga,fgb:lgb) = mol%density_mx%r(fa:la,fb:lb)
         end do
      end do
   end subroutine

   PURE subroutine set_group_MOs(self,g,mol)
   ! Set the atom group "g" .density_mx from "P". "mol" must be a
   ! molecule created from the appropriate atom group "g".
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: g
      type(MOLECULE_TYPE), IN :: mol
      INT :: a,ga,ms,mf,no
      INT :: fga,lga,fa,la
   ENSURE(allocated(self%atom_group),"MOLECULE.SET:set_group_MOs ... no atom group information")
   ENSURE(g>0,"MOLECULE.SET:set_group_MOs ... group number must be positive")
   ENSURE(g<=size(self%atom_group),"MOLECULE.SET:set_group_MOs ... group number exceeds number of groups")
   ENSURE(allocated(self%MOs),"MOLECULE.SET:set_group_MOs ... no MOs")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.SET:set_group_MOs ... no NO_occ_nos")
   ENSURE(allocated(mol%MOs),"MOLECULE.SET:set_group_MOs ... no mol MOs")
   ENSURE(allocated(mol%MOs%r),"MOLECULE.SET:set_group_MOs ... no restricted mol MOs")
      mf = n_bf_up_to_mol_(self,g)
      ms = mf+1
      mf = mf+mol%n_bf
      do a = 1,self%atom_group(g)%n_atoms
         ga = self%atom_group(g)%atom_index(a)
         fa = mol%first_basis_fn_for_atom(a)
         fga = self%first_basis_fn_for_atom(ga)
         la = mol%last_basis_fn_for_atom(a)
         lga = self%last_basis_fn_for_atom(ga)
         self%MOs%r(fga:lga,ms:mf) = mol%MOs%r(fa:la,:)
      end do
      no = mol%n_e/2
      self%NO_occ_nos%r(ms:ms+no-1) = TWO
   end subroutine


!  ============================
!  Bases directory & basis name
!  ============================

   subroutine set_basis_directory(self,directory)
   ! Set the basis directory (for all kinds of bases)
      type(MOLECULE_TYPE), IN :: self
      STR(len=*), IN :: directory
      call set_library_directory_(self%basis,directory)  ! NOT PURE
      call set_library_directory_(self%slaterbasis,directory)
      call set_library_directory_(self%coppensbasis,directory)
   end subroutine

   subroutine set_basis_name(self,name)
   ! Set a gaussian *library* basis set name
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*) :: name

! DIE_IF(any(name==non_gaussian),"basis "//trim(name)//"is not a gaussian basis")

 !        "Coppens          "  ]

      ! Set the basis kind
      self%basis_name = name

      ! Destroy links to previous bases
      if (allocated(self%atom)) call destroy_basis_part_(self%atom)
      call destroy_(self%basis)

      ! Make fresh basis set links (might be leaky)
      call resolve_gaussianbases_(self)  ! NOT PURE
   end subroutine

   subroutine set_slaterbasis_name(self,name)
   ! Set a slater *library* basis set name
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: name

      ! Set the basis kind
      self%slaterbasis_name = name

      ! Destroy links to previous bases
      if (allocated(self%atom)) call destroy_slaterbasis_part_(self%atom)
      call destroy_(self%slaterbasis)

      ! Make fresh basis set links (might be leaky)
      call resolve_slaterbases_(self)  ! NOT PURE
   end subroutine

   subroutine set_coppensbasis_name(self,name)
   ! Set a coppens *library* basis set name
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: name
   ENSURE(name=="Coppens","MOLECULE.SET:set_coppensbasis_name ... basis "//trim(name)//"is not a known coppens basis")

      ! Set the basis kind
      self%coppensbasis_name = name

      ! Destroy links to previous bases
      if (allocated(self%atom)) call destroy_coppensbasis_part_(self%atom)
      call destroy_(self%coppensbasis)

      ! Make fresh basis set links (might be leaky)
      call resolve_coppensbases_(self)  ! NOT PURE
   end subroutine


!  ==================
!  Resolve basis sets
!  ==================

   subroutine resolve_bases_and_update(self)
   ! Match the basis set labels with a basis set, either from a
   ! library or from the user input. The atoms must exist!
      type(MOLECULE_TYPE), INOUT :: self
      if (NOT allocated(self%atom)) return

      ! Read and resolve
      if (NOT bases_are_resolved_(self%atom)) then
         self%basis_info_made = FALSE
         call resolve_coppensbases_(self)  ! NOT PURE
         call resolve_slaterbases_(self)
         call resolve_gaussianbases_(self)
      end if

      ! Update
      call update_basis_info_(self)
   end subroutine

   subroutine resolve_bases_and_update_from(self,mol)
   ! Set the basis data from molecule "mol" assumed to be a parent
   ! "supermolecule" NOTE: atom kinds are *NOT* updated
      type(MOLECULE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol

      ! Copy the basis set and set the basis info
      ! WARNING: dont destroy basis of mol
      self%use_spherical_basis = mol%use_spherical_basis
      if (allocated(mol%basis)) self%basis = mol%basis
      if (allocated(mol%slaterbasis)) self%slaterbasis = mol%slaterbasis
      if (allocated(mol%coppensbasis)) self%coppensbasis = mol%coppensbasis

      ! Do it, but don't update atom.kind's
      call resolve_bases_and_update_(self)  ! NOT PURE
      self%atom_info_made = TRUE  ! Assume kinds ared OK
   end subroutine

   subroutine resolve_gaussianbases(self)
   ! Match the basis set labels for every atom with the actual atomic
   ! basis set nby reading a library basis, if not there.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: library_basis_labels
      INT :: n_unique
      STR(len=PATH_SIZE) :: library
      if (NOT allocated(self%atom)) return
      if (self%n_atom<1) return
      if (bases_are_resolved_(self%atom)) return

      ! Read from a library
      if (self%basis_name/=" ") then

         ! Renormalize basis sets. (The library is in normalized form)
         if (allocated(self%basis)) call renormalize_(self%basis)

         ! Read the basis from the library
         library = library_file_(self%basis,self%basis_name)
         library_basis_labels = library_basis_labels_(self%atom,self%basis_name)
         call read_library_data_(self%basis,library,library_basis_labels,n_unique)  ! NOT PURE
         call destroy_(library_basis_labels)
         if (allocated(self%basis)) call unnormalize_(self%basis)
      end if

      ! Resolve 
      if (allocated(self%basis)) then
         call set_spherical_(self%basis,self%use_spherical_basis)
         call resolve_bases_(self%atom,self%basis,self%basis_name)
         call set_basis_kind_(self%atom,"gaussian")
         self%atom_info_made = FALSE  ! Need new atom kinds
      end if
   end subroutine

   subroutine set_auxiliary_basis_name(self,name)
   ! Set the auxiliary *library* basis set name. The basis sets are
   ! read when first needed, by .resolve_auxiliary_bases.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: name
      self%auxiliary_basis_name = name
      call destroy_(self%auxiliary_basis)
      call destroy_(self%auxiliary_basis_for_atom)
      call destroy_(self%RI_metric_factor)
   end subroutine

   subroutine resolve_auxiliary_bases(self)
   ! Read the auxiliary basis set of every kind of atom from the
   ! library, if not already there, and set .auxiliary_basis_for_atom.
   ! The auxiliary basis sets are always spherical.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      STR(len=STR_SIZE) :: label
      STR(len=PATH_SIZE) :: library
      INT :: a,b
   ENSURE(allocated(self%atom),"MOLECULE.SET:resolve_auxiliary_bases ... no atoms")
   ENSURE(self%auxiliary_basis_name/=" ","MOLECULE.SET:resolve_auxiliary_bases ... no auxiliary_basis_name= given")
      if (allocated(self%auxiliary_basis_for_atom)) return

      ! Read from the library, which is in normalized form
      library = library_file_(self%auxiliary_basis,self%auxiliary_basis_name)
      labels = library_basis_labels_(self%atom,self%auxiliary_basis_name)
      call read_library_data_(self%auxiliary_basis,library,labels)  ! NOT PURE
      call destroy_(labels)
      call unnormalize_(self%auxiliary_basis)
      call set_spherical_(self%auxiliary_basis,TRUE)

      ! Match each atom to its basis
      call create_(self%auxiliary_basis_for_atom,self%n_atom)
      self%auxiliary_basis_for_atom = 0
      do a = 1,self%n_atom
         label = library_basis_label_(self%atom(a),self%auxiliary_basis_name)
         do b = 1,size(self%auxiliary_basis)
            if (NOT same_as_(self%auxiliary_basis(b)%label,label,ignore_case=TRUE)) cycle
            self%auxiliary_basis_for_atom(a) = b
            exit
         end do
         DIE_IF(self%auxiliary_basis_for_atom(a)==0,"MOLECULE.SET:resolve_auxiliary_bases ... no auxiliary basis for atom "//trim(to_str_(a)))
      end do
   end subroutine

   PURE function n_auxiliary_bf(self) result (res)
   ! The number of auxiliary basis functions
      type(MOLECULE_TYPE), IN :: self
      INT :: res
      INT :: a
   ENSURE(allocated(self%auxiliary_basis_for_atom),"MOLECULE.SET:n_auxiliary_bf ... auxiliary bases not resolved")
      res = 0
      do a = 1,self%n_atom
         res = res+self%auxiliary_basis(self%auxiliary_basis_for_atom(a))%n_bf
      end do
   end function

   subroutine resolve_slaterbases_0(self)
   ! Match the basis set labels for every atom with the actual atomic
   ! basis set by reading a library basis set, if not there.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: library_basis_labels
      INT :: n_unique
      STR(len=PATH_SIZE) :: library
      if (NOT allocated(self%atom)) return
      if (self%n_atom<1) return
      if (slaterbases_are_resolved_(self%atom)) return

      ! Read from a library
      if (self%slaterbasis_name/=" ") then

         ! Renormalize basis sets. (The library is in normalized form)
         if (allocated(self%slaterbasis)) call renormalize_(self%slaterbasis)

         ! Read the basis from the library
         library = library_file_(self%slaterbasis,self%slaterbasis_name)
         library_basis_labels = library_basis_labels_(self%atom,self%slaterbasis_name)
         call read_library_data_(self%slaterbasis,library,library_basis_labels,n_unique)
         call destroy_(library_basis_labels)
         if (allocated(self%slaterbasis)) call unnormalize_(self%slaterbasis)
      end if

      ! Resolve
      if (allocated(self%slaterbasis)) then
         call resolve_bases_(self%atom,self%slaterbasis,self%slaterbasis_name)
         call set_basis_kind_(self%atom,"slater")
         self%atom_info_made = FALSE  ! Need new atom kinds
      end if
   end subroutine

   subroutine resolve_slaterbases_1(self,atom)
   ! Match the basis set labels for every atom with the actual atomic
   ! basis set by reading a library basis set, if not there.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: atom
      VEC(STR(len=STR_SIZE),:), allocatable :: library_basis_labels
      INT :: n_unique
      STR(len=PATH_SIZE) :: library
      if (NOT allocated(atom)) return
      if (size(atom)<1) return
      if (slaterbases_are_resolved_(atom)) return

      ! Read from a library
      if (self%slaterbasis_name/=" ") then

         ! Renormalize basis sets. (The library is in normalized form)
         if (allocated(self%slaterbasis)) call renormalize_(self%slaterbasis)

         ! Read the basis from the library
         library = library_file_(self%slaterbasis,self%slaterbasis_name)
         library_basis_labels = library_basis_labels_(atom,self%slaterbasis_name)
         call read_library_data_(self%slaterbasis,library,library_basis_labels,n_unique)
         call destroy_(library_basis_labels)
         if (allocated(self%slaterbasis)) call unnormalize_(self%slaterbasis)
      end if

      ! Resolve
      if (allocated(self%slaterbasis)) then
         call resolve_bases_(atom,self%slaterbasis,self%slaterbasis_name)
         call set_basis_kind_(atom,"slater")
      end if
   end subroutine

   subroutine resolve_coppensbases(self)
   ! Match the basis set labels for every atom with the actual atomic
   ! basis set by reading a library basis set, if not there.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: library_basis_labels
      INT :: n_unique
      STR(len=PATH_SIZE) :: library
      if (NOT allocated(self%atom)) return
      if (self%n_atom<1) return
      if (coppensbases_are_resolved_(self%atom)) return

      ! Read from a library 
      if (self%coppensbasis_name/=" ") then

         ! Renormalize basis sets. (The library is in normalized form)
         if (allocated(self%coppensbasis)) call renormalize_(self%coppensbasis)
         ! Read the basis from the library
         library = library_file_(self%coppensbasis,self%coppensbasis_name)
         library_basis_labels = library_basis_labels_(self%atom,self%coppensbasis_name)
         call read_library_data_(self%coppensbasis,library,library_basis_labels,n_unique)
         call destroy_(library_basis_labels)
         if (allocated(self%coppensbasis)) call unnormalize_(self%coppensbasis)
      end if

      ! Resolve 
      if (allocated(self%coppensbasis)) then
         call resolve_bases_(self%atom,self%coppensbasis,self%coppensbasis_name)
         call set_basis_kind_(self%atom,"coppens")
         self%atom_info_made = FALSE  ! Need new atom kinds
      end if
   end subroutine

   PURE subroutine update_basis_info(self)
   ! Make the basis set information including shell-basis-fn limits
      type(MOLECULE_TYPE), INOUT :: self
      if (NOT allocated(self%basis)) return
      if (NOT allocated(self%atom)) return
      self%n_basis = size(self%basis)
      self%n_bf = no_of_basis_functions_(self%atom)
      self%n_prim = no_of_primitives_(self%atom)
      self%n_shell = no_of_shells_(self%atom)
      self%n_shell_pairs = no_of_shell_pairs_(self%atom)

      ! Make shell-basis-function limts, etc. (leaky)
      call update_shell_info_(self)
      self%basis_info_made = TRUE
      self%atom_info_made = FALSE  ! Since kinds depend on bases
   end subroutine


!  ============
!  Resolve ANOs
!  ============

   PURE subroutine resolve_ANOs_from_0(self,mol,list)
   ! Set ANO data and atom shell info from molecule "mol" from the
   ! sublist "list". Make sure none of these are destroyed!
   ! NOTE: No point to assign unique data, all data is non-unique
      type(MOLECULE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      VEC(INT,:), IN :: list
      BIN :: made_ANO,made_int,made_shl
      INT :: a,b
   ENSURE(allocated(self%atom),"MOLECULE.SET:resolve_ANOs_from_0 ... no atoms")
   ENSURE(self%n_atom_kind>0,"MOLECULE.SET:resolve_ANOs_from_0 ... atoms not finalized")
   ENSURE(allocated(mol%atom),"MOLECULE.SET:resolve_ANOs_from_0 ... mol has no atoms")
   ENSURE(mol%n_atom_kind>0,"MOLECULE.SET:resolve_ANOs_from_0 ... mol atoms not finalized")
   ENSURE(size(list)==size(self%atom),"MOLECULE.SET:resolve_ANOs_from_0 ... inconsistent list length")

      ! Assign ANO orbitals
      made_ANO = has_all_ANO_matrices_(mol)
      if (made_ANO) then
         do a = 1,size(list)
            b = list(a)
            self%atom(a)%density_mx = mol%atom(b)%density_mx
            self%atom(a)%NOs = mol%atom(b)%NOs
            self%atom(a)%NO_occ_nos = mol%atom(b)%NO_occ_nos
            self%atom(a)%AOs = mol%atom(b)%AOs
            call set_energy_(self%atom(a),mol%atom(b)%energy)
         end do
      end if

      ! Assign ANO interpolators
      made_int = has_all_interpolators_(mol)
      if (made_int) then
         do a = 1,size(list)
            b = list(a)
            self%atom(a)%interpolator = mol%atom(b)%interpolator
         end do
      end if

      ! Assign atom shell info
      made_shl = has_all_shell_info_(mol)
      if (made_shl) then
         do a = 1,size(list)
            b = list(a)
            self%atom(a)%first_basis_fn_for_shell = mol%atom(b)%first_basis_fn_for_shell
            self%atom(a)%last_basis_fn_for_shell = mol%atom(b)%last_basis_fn_for_shell
            self%atom(a)%basis_shell1pair = mol%atom(b)%basis_shell1pair
         end do
      end if
   end subroutine


!  Some probs with routine below, use above if you can?

   PURE subroutine resolve_ANOs_from_1(self,mol)
   ! Set ANO data and atom shell info from molecule "mol" by comparing
   ! the atom kinds directly. Make sure none of the assigned ANO data
   ! is destroyed!
      type(MOLECULE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      INT :: a,b
   ENSURE(allocated(self%atom),"MOLECULE.SET:resolve_ANOs_from_1 ... no atoms")
   ENSURE(self%n_atom_kind>0,"MOLECULE.SET:resolve_ANOs_from_1 ... atoms not finalized")
   ENSURE(allocated(mol%atom),"MOLECULE.SET:resolve_ANOs_from_1 ... mol has no atoms")
   ENSURE(mol%n_atom_kind>0,"MOLECULE.SET:resolve_ANOs_from_1 ... mol atoms not finalized")

      ! Assign ANO orbitals
      do a = 1,size(self%atom)
         do b = 1,size(mol%atom)
            if (is_same_kind_as_(self%atom(a),mol%atom(b))) then
               if (has_ANO_matrices_(mol%atom(b))) then
                  self%atom(a)%density_mx = mol%atom(b)%density_mx
                  self%atom(a)%NOs = mol%atom(b)%NOs
                  self%atom(a)%NO_occ_nos = mol%atom(b)%NO_occ_nos
                  self%atom(a)%AOs = mol%atom(b)%AOs
                  call set_energy_(self%atom(a),mol%atom(b)%energy)
                  exit  ! next atom "a"
               end if
            end if
         end do
      end do

      ! Assign ANO interpolators
      do a = 1,size(self%atom)
         do b = 1,size(mol%atom)
            if (is_same_kind_as_(self%atom(a),mol%atom(b))) then
               if (allocated(mol%atom(b)%interpolator)) then
                  self%atom(a)%interpolator = mol%atom(b)%interpolator
                  exit  ! next atom "a"
               end if
            end if
         end do
      end do

      ! Assign atom shell info
      do a = 1,size(self%atom)
         do b = 1,size(mol%atom)
            if (is_same_kind_as_(self%atom(a),mol%atom(b))) then
               if (has_shell_info_(mol%atom(b))) then
                  self%atom(a)%first_basis_fn_for_shell = mol%atom(b)%first_basis_fn_for_shell
                  self%atom(a)%last_basis_fn_for_shell = mol%atom(b)%last_basis_fn_for_shell
                  self%atom(a)%basis_shell1pair = mol%atom(b)%basis_shell1pair
                  exit  ! next atom "a"
               end if
            end if
         end do
      end do
   end subroutine


!  =========================
!  Resolve axes to cartesian
!  =========================

   PURE subroutine resolve_axis_system(self)
   ! Change the axis system to cartesian, if required.
   ! NOTE: Errors (if any) are transformed straightforwardly
      type(MOLECULE_TYPE), INOUT :: self
      if (NOT allocated(self%crystal)) return
      if (NOT allocated(self%atom)) return
      call change_axis_system_to_(self%atom,"cartesian",self%crystal%unit_cell)
   end subroutine

   subroutine change_axis_system_to(self)
   ! Change the axis system.
   ! NOTE: Errors (if any) are transformed straightforwardly
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: axis_system
      if (NOT allocated(self%crystal)) return
      if (NOT allocated(self%atom)) return
      call read_(stdin,axis_system)
      call change_axis_system_to_(self%atom,axis_system,self%crystal%unit_cell)
   end subroutine


!  ============
!  Read routine
!  ============

!   read_CIF_ADPs(cif) :: leaky
!   ! Read atom ADP information from a Crystallographic Information File, "cif".
!      cif :: CIF
!
!   ENSURE(cif.start_of_data>0,"no start of data in CIF")
!   ENSURE(cif.end_of_data>cif.start_of_data,"no end of data in CIF")
!   ENSURE(.crystal.allocated,"must have crystal information")
!
!      if (NOT cif.file.is_open) cif.open
!
!      .atom.read_CIF_ADPs(cif)
!
!      .resolve_axis_system
!
!   end

!  ======================
!  Gaussian chk interface
!  ======================

   PURE subroutine set_basis_from_gX_data(self,l_4_shell,n_cc_4_sh,atom_4_sh,ex_4_prim,cc_4_prim)
   ! Set the basis from gaussian data "XXX_4_sh" are the shell data
   ! flattened over all molecular shells. "XXX_4_prim" are (likewise)
   ! data flattened over molecular primitives.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(INT,:), IN :: l_4_shell,n_cc_4_sh,atom_4_sh
      VEC(REAL,:), IN :: ex_4_prim,cc_4_prim
      INT :: n_basis,n_shell,n_cc
      INT :: a,s,t,f,l,b
      type(BASIS_TYPE), allocatable, target :: basis
      type(SHELL_TYPE), PTR :: sh
      BIN :: same
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.SET:set_basis_from_gX_data ... not implemented")
   ENSURE(allocated(self%atom),"MOLECULE.SET:set_basis_from_gX_data ... no atonm list")
   ENSURE(size(self%atom)==self%n_atom,"MOLECULE.SET:set_basis_from_gX_data ... wrong size, atom list")

      ! Molecular basis (to be appended to)
      n_basis = 0
      call create_(self%basis,n_basis)

      ! Basis fn/shell counters
      l = 0  ! Last primitive, flattened over all atoms
      t = 0  ! Last shell, flattened over all shells

      ! Loop atom "a"
      do a = 1,self%n_atom

         ! Basis for atom "a"
         call create_(basis)
         basis%label = trim(generic_chemical_symbol_(self%atom(a)))//":gaussian"

         ! Set basis label and kind
         call set_basis_label_(self%atom(a),basis%label)
         call set_basis_kind_(self%atom(a),"gaussian")

         ! No. of shells
         n_shell = count(atom_4_sh==a)

         ! Create basis shells
         basis%n_shell = n_shell
         call create_(basis%shell,n_shell)

         ! Set basis shells
         do s = 1,n_shell

            ! This shell
            sh => basis%shell(s)

            ! Set shell L
            call set_l_(sh,l_4_shell(t+s))
            call set_n_comp_(sh)

            ! Set shell n_cc
            n_cc = n_cc_4_sh(t+s)
            sh%n_cc = n_cc

            ! Set shell exponents/contractions
            call create_(sh%exponent,n_cc)
            call create_(sh%contraction,n_cc)
            f = l+1
            l = l+n_cc
            sh%exponent = ex_4_prim(f:l)
            sh%contraction = cc_4_prim(f:l)
         end do

         ! Set no. of bf'd and primitives
         basis%n_bf = no_of_basis_functions_(basis)
         basis%n_prim = no_of_primitives_(basis)
         call unnormalize_(basis)  ! Note this
         t = t+n_shell

         ! Seen basis before?
         same = FALSE
         do b = 1,n_basis
            if (NOT same_as_(self%basis(b),basis)) cycle
            same = TRUE
            exit
         end do

         ! Append only new basis to .basis
         if (NOT same) then
            n_basis = n_basis+1
            call append_(self%basis,basis)
         end if

         ! Clean
         call destroy_(basis)
      end do
   end subroutine


! Should go into GAUSIAN_DATA?

!   swap_from_g94_orbital_order(X,swap)
!   ! Swap the order of f orbitals on matrix "X" after reading a
!   ! gaussian checkpoint file, for "swap" equal to "row" or "1", or
!   ! "coloumn" or "2". Also: remove the intra-shell basis function
!   ! normalisation factors which are not used in Tonto.
!      X :: MAT{REAL}
!      swap :: STR
!
!      s,f,l,ls,fg,lg,i,j :: INT
!      fac :: VEC{REAL}@
!      ff :: VEC{INT}(10) = [ 1, 2, 3, 5, 6, 4, 9, 7, 8, 10 ]
!      gg :: VEC{INT}(15) = [15, 5, 1, 14, 13, 9, 4, 6, 2, 12, 10, 3, 11, 8, 7]
!
!      select case (swap)
!
!      case("row","1")
!
!         do s = 1,.n_shell
!
!            f  = .first_basis_fn_for_shell(s)
!            l  = .last_basis_fn_for_shell(s)
!            ls = .angular_moment_for_shell(s)
!
!            ENSURE(ls<=4,"cannot yet convert order for h shells")
!
!            ! Swap f & g functions
!            select case (ls)
!            case (3); X(f:l,:) = X(f-1+ff,:)
!            case (4); X(f:l,:) = X(f-1+gg,:)
!            end
!
!            ! Normalisation factors
!            if (ls>1) then  
!               lg = GAUSSIAN_DATA:n_comp_up_to(ls-1)
!               fg = lg + 1
!               lg = lg + GAUSSIAN_DATA:n_comp(ls)
!               fac = GAUSSIAN_DATA::unnormalizing_factors(fg:lg)
!               do i = f,l
!                  X(i,:) = X(i,:)*fac(i-f+1)
!               end
!            end
!
!         end
!
!      case("column","2")
!
!         do s = 1,.n_shell
!
!            f  = .first_basis_fn_for_shell(s)
!            l  = .last_basis_fn_for_shell(s)
!            ls = .angular_moment_for_shell(s)
!
!            ENSURE(ls<=4,"cannot yet convert order for h shells")
!
!            ! Swap f & g functions
!            select case (ls)
!            case (3); X(:,f:l) = X(:,f-1+ff)
!            case (4); X(:,f:l) = X(:,f-1+gg)
!            end
!
!            ! Normalisation factors
!            if (ls>1) then  
!               lg = GAUSSIAN_DATA:n_comp_up_to(ls-1)
!               fg = lg + 1
!               lg = lg + GAUSSIAN_DATA:n_comp(ls)
!               fac = GAUSSIAN_DATA::unnormalizing_factors(fg:lg)
!               do j = f,l
!                  X(:,j) = X(:,j)*fac(j-f+1)
!               end
!            end
!
!         end
!
!      case default
!         DIE("unknown swap kind, "//trim(swap))
!
!      end
!
!   end

!   swap_into_g94_orbital_order(X,swap)
!   ! Swap the order of f orbitals on matrix "X" after reading a
!   ! gaussian checkpoint file, for "swap" equal to "row" or "1", or
!   ! "coloumn" or "2". Also: remove the intra-shell basis function
!   ! normalisation factors which are not used in Tonto.
!      X :: MAT{REAL}
!      swap :: STR
!
!      s,f,l,ls,fg,lg,i,j :: INT
!      fac :: VEC{REAL}@
!      ff :: VEC{INT}(10) = [ 1, 2, 3, 5, 6, 4, 9, 7, 8, 10 ]
!      gg :: VEC{INT}(15) = [15, 5, 1, 14, 13, 9, 4, 6, 2, 12, 10, 3, 11, 8, 7]
!
!      select case (swap)
!
!      case("row","1")
!
!         do s = 1,.n_shell
!
!            f  = .first_basis_fn_for_shell(s)
!            l  = .last_basis_fn_for_shell(s)
!            ls = .angular_moment_for_shell(s)
!
!            ENSURE(ls<=4,"cannot yet convert order for h shells")
!
!            ! Normalisation factors
!            if (ls>1) then  
!               lg = GAUSSIAN_DATA:n_comp_up_to(ls-1)
!               fg = lg + 1
!               lg = lg + GAUSSIAN_DATA:n_comp(ls)
!               fac = GAUSSIAN_DATA::unnormalizing_factors(fg:lg)
!               do i = f,l
!                  X(i,:) = X(i,:)/fac(i-f+1)
!               end
!            end
!
!            ! Swap f functions; g functions - no change
!            select case (ls)
!            case (3); X(f-1+ff,:) = X(f:l,:)
!            case (4); X(f-1+gg,:) = X(f:l,:)
!            end
!
!         end
!
!      case("column","2")
!
!         do s = 1,.n_shell
!
!            f  = .first_basis_fn_for_shell(s)
!            l  = .last_basis_fn_for_shell(s)
!            ls = .angular_moment_for_shell(s)
!
!            ENSURE(ls<=4,"cannot yet convert order for h shells")
!
!            ! Normalisation factors
!            if (ls>1) then  
!               lg = GAUSSIAN_DATA:n_comp_up_to(ls-1)
!               fg = lg + 1
!               lg = lg + GAUSSIAN_DATA:n_comp(ls)
!               fac = GAUSSIAN_DATA::unnormalizing_factors(fg:lg)
!               do j = f,l
!                  X(:,j) = X(:,j)/fac(j-f+1)
!               end
!            end
!
!            ! Swap f functions; g functions - no change
!            select case (ls)
!            case (3); X(:,f-1+ff) = X(:,f:l)
!            case (4); X(:,f-1+gg) = X(:,f:l)
!            end
!
!         end
!
!      case default
!         DIE("unknown swap kind, "//trim(swap))
!
!      end
!
!   end

!  =================
!  Atom pair indices
!  =================

   PURE subroutine set_atom_pair_indices_from(self,index,a,b,fa,la,na,fb,lb,nb)
   ! Return the atom indicies "a" and "b" which map to an atom-pair
   ! "index".  Also get "fa", "la" and "fb", "lb", the indices of
   ! first and last basis functions for atoms "a" and "b", and "na"
   ! and "nb", the number of basis functions on those atoms,
   ! respectively. NOTE: "fa" and "fb" are actually the first basis
   ! functions for atoms "a" and "b" *minus 1*.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      INT, OUT :: a,b
      INT, OUT :: fa,la,na
      INT, OUT :: fb,lb,nb
      a = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b = index-a*(a-1)/2
      fa = self%first_basis_fn_for_atom(a)-1
      la = self%last_basis_fn_for_atom(a)
      na = la-fa
      fb = self%first_basis_fn_for_atom(b)-1
      lb = self%last_basis_fn_for_atom(b)
      nb = lb-fb
   end subroutine


!  ==================================
!  VEC{ATOM} shell extraction methods
!  ==================================

! The set routines should be quicker, but I'm not sure.
! When allocatable components are used it may be easier
! to allocate an entire basis set array once and for all.

   PURE subroutine set_shell_from(self,index,shell)
   ! Set basis set "shell" (SHELL type) corresponding to "index"
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      type(SHELL_TYPE), OUT :: shell
      INT :: aa,sa
   ENSURE(allocated(self%atom),"MOLECULE.SET:set_shell_from ... no atoms")
      aa = self%atom_for_shell(index)
      sa = self%atom_shell_for_shell(index)
      shell = self%atom(aa)%basis%shell(sa)
   end subroutine

   PURE subroutine set_shell1_from_0(self,index,shell)
   ! Get basis set "shell" (SHELL1 type) corresponding to "index"
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      type(SHELL1_TYPE), OUT :: shell
      INT :: aa,sa
   ENSURE(allocated(self%atom),"MOLECULE.SET:set_shell1_from_0 ... no atoms")
      aa = self%atom_for_shell(index)
      sa = self%atom_shell_for_shell(index)
      call set_(shell,self%atom(aa)%basis%shell(sa),self%atom(aa)%position)
   end subroutine

   PURE subroutine set_shell1_from_1(self,index,shell,fa,la)
   ! Get the "index"-th "shell" of the basis set with first bf index
   ! "fa", last bf index "la".
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      type(SHELL1_TYPE), OUT :: shell
      INT, OUT :: fa,la
      INT :: aa,sa
   ENSURE(allocated(self%atom),"MOLECULE.SET:set_shell1_from_1 ... no atoms")
      aa = self%atom_for_shell(index)
      sa = self%atom_shell_for_shell(index)
      fa = self%first_basis_fn_for_shell(index)
      la = self%last_basis_fn_for_shell(index)
      call set_(shell,self%atom(aa)%basis%shell(sa),self%atom(aa)%position)
   end subroutine

   PURE subroutine set_shell1_from_2(self,index,shell,fa,la,na)
   ! Get the "index"-th "shell" of the basis set with first bf index
   ! "fa", last bf index "la", and "na" the no. of bf's in the shell.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      type(SHELL1_TYPE), OUT :: shell
      INT, OUT :: fa,la,na
      call set_shell1_from_(self,index,shell,fa,la)
      na = la-fa+1
   end subroutine


!  ==================
!  Shell pair indices
!  ==================

   pure subroutine set_shell2_indices_from_0(self,index,a,b,na,nb)
   ! Return the shell indicies "a" and "b" which map to "index"
   ! Also return the basis function start indices "fa", "la", etc ...
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      INT, OUT :: a,na
      INT, OUT :: b,nb
      INT :: fa,la
      INT :: fb,lb
      a = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b = index-a*(a-1)/2
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      na = la-fa+1
      nb = lb-fb+1
   end subroutine

   pure subroutine set_shell2_indices_from_1(self,index,a,b,na,nb,atom_a,atom_b)
   ! Return the shell indicies "a" and "b" which map to "index"
   ! Also return the basis function start indices "fa", "la", etc ...
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      INT, OUT :: a,na,atom_a
      INT, OUT :: b,nb,atom_b
      INT :: fa,la
      INT :: fb,lb
      a = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b = index-a*(a-1)/2
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      na = la-fa+1
      nb = lb-fb+1
      atom_a = self%atom_for_shell(a)
      atom_b = self%atom_for_shell(b)
   end subroutine

   pure subroutine set_shell2_indices_from_2(self,index,a,b,fa,la,na,fb,lb,nb)
   ! Return the shell indicies "a" and "b" which map to "index"
   ! Also return the basis function start indices "fa", "la", etc ...
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      INT, OUT :: a,fa,la,na
      INT, OUT :: b,fb,lb,nb
      a = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b = index-a*(a-1)/2
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      na = la-fa+1
      nb = lb-fb+1
   end subroutine

   pure subroutine set_shell2_indices_from_3(self,index,a,b,fa,la,na,fb,lb,nb,atom_a,atom_b)
   ! Return the shell indicies "a" and "b" which map to "index"
   ! Also return the basis function start indices "fa", "la", etc ...
   ! Also return the atom indices "atom_a" and "atom_b" for each shell.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      INT, OUT :: a,fa,la,na,atom_a
      INT, OUT :: b,fb,lb,nb,atom_b
      call set_shell2_indices_from_(self,index,a,b,fa,la,na,fb,lb,nb)
      atom_a = self%atom_for_shell(a)
      atom_b = self%atom_for_shell(b)
   end subroutine


! For testing

   pure subroutine set_shell2_indices_from_4(self,a,b,fa,la,na,fb,lb,nb,atom_a,atom_b)
   ! Return the shell indicies "a" and "b" which map to "index"
   ! Also return the basis function start indices "fa", "la", etc ...
   ! Also return the atom indices "atom_a" and "atom_b" for each shell.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a,b
      INT, OUT :: fa,la,na
      INT, OUT :: fb,lb,nb
      INT, OUT :: atom_a,atom_b
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      na = la-fa+1
      nb = lb-fb+1
      atom_a = self%atom_for_shell(a)
      atom_b = self%atom_for_shell(b)
   end subroutine


!  =======================
!  Precomputed shell pairs
!  =======================

   PURE subroutine set_shell2_from_0(self,index,sh2)
   ! Get SHELL2 "sh2" correponding to the pair index "index".
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      type(SHELL2_TYPE), OUT :: sh2
      INT :: a,b,aa,sa,bb,sb
      VEC(REAL,3) :: pa,pb
      a = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b = index-a*(a-1)/2
      sa = self%basis_shell_for_shell(a)
      sb = self%basis_shell_for_shell(b)
      aa = self%atom_for_shell(a)
      bb = self%atom_for_shell(b)
      pa = self%atom(aa)%position
      pb = self%atom(bb)%position
      call set_and_precompute_from_(sh2,self%basis_shell1pair(sa,sb),pa,pb,aa==bb,skip=FALSE)
   end subroutine

   PURE subroutine set_shell2_from_1(self,index,sh2,fa,la,na,fb,lb,nb)
   ! Get the SHELL2 object "sh2" correponding to the pair index "index"
   ! Also return the basis function start indices "fa", "la", etc ...
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      type(SHELL2_TYPE), OUT :: sh2
      INT, OUT :: fa,la,na
      INT, OUT :: fb,lb,nb
      INT :: a,b,aa,sa,bb,sb
      VEC(REAL,3) :: pa,pb
      a = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b = index-a*(a-1)/2
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      na = la-fa+1
      nb = lb-fb+1
      sa = self%basis_shell_for_shell(a)
      sb = self%basis_shell_for_shell(b)
      aa = self%atom_for_shell(a)
      bb = self%atom_for_shell(b)
      pa = self%atom(aa)%position
      pb = self%atom(bb)%position
      call set_and_precompute_from_(sh2,self%basis_shell1pair(sa,sb),pa,pb,aa==bb,skip=FALSE)
   end subroutine

   PURE subroutine set_shell2_from_2(self,index,sh2,fa,la,na,fb,lb,nb,atom_a,atom_b)
   ! Get the SHELL2 object "sh2" correponding to the pair index "index"
   ! Also return the basis function start indices "fa", "la", etc ...
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      type(SHELL2_TYPE), OUT :: sh2
      INT, OUT :: fa,la,na
      INT, OUT :: fb,lb,nb
      INT, OUT :: atom_a,atom_b
      INT :: a,b,sa,sb
      VEC(REAL,3) :: pa,pb
      a = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b = index-a*(a-1)/2
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      na = la-fa+1
      nb = lb-fb+1
      sa = self%basis_shell_for_shell(a)
      sb = self%basis_shell_for_shell(b)
      atom_a = self%atom_for_shell(a)
      atom_b = self%atom_for_shell(b)
      pa = self%atom(atom_a)%position
      pb = self%atom(atom_b)%position
      call set_and_precompute_from_(sh2,self%basis_shell1pair(sa,sb),pa,pb,atom_a==atom_b,FALSE)
   end subroutine

   PURE subroutine set_shell2_from_3(self,index,sh2,a,fa,la,na,b,fb,lb,nb,atom_a,atom_b)
   ! Get the SHELL2 object "sh2" correponding to the pair index "index"
   ! Also return the basis function start indices "fa", "la", etc ...
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      type(SHELL2_TYPE), OUT :: sh2
      INT, OUT :: a,fa,la,na
      INT, OUT :: b,fb,lb,nb
      INT, OUT :: atom_a,atom_b
      INT :: sa,sb
      VEC(REAL,3) :: pa,pb
      a = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b = index-a*(a-1)/2
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      na = la-fa+1
      nb = lb-fb+1
      sa = self%basis_shell_for_shell(a)
      sb = self%basis_shell_for_shell(b)
      atom_a = self%atom_for_shell(a)
      atom_b = self%atom_for_shell(b)
      pa = self%atom(atom_a)%position
      pb = self%atom(atom_b)%position
      call set_and_precompute_from_(sh2,self%basis_shell1pair(sa,sb),pa,pb,atom_a==atom_b,FALSE)
   end subroutine


!  =====================
!  Shell quartet indices
!  =====================

   pure subroutine set_shell4_indices_from(self,index,fa,la,fb,lb,fc,lc,fd,ld)
   ! For the quartet index "index" return the shell indicies "a", "b",
   ! "c" and "d" which map to "index". Also return the basis function
   ! start indices "fa", "la", etc ...
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      INT, OUT :: fa,la,fb,lb
      INT, OUT :: fc,lc,fd,ld
      INT :: a,b,c,d
      INT :: ab,cd
      ab = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      cd = index-ab*(ab-1)/2
      a = (1+int(sqrt(8.0d0*ab-7.0d0)))/2
      b = ab-a*(a-1)/2
      c = (1+int(sqrt(8.0d0*cd-7.0d0)))/2
      d = cd-c*(c-1)/2
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      fc = self%first_basis_fn_for_shell(c)
      fd = self%first_basis_fn_for_shell(d)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      lc = self%last_basis_fn_for_shell(c)
      ld = self%last_basis_fn_for_shell(d)
   end subroutine

   pure subroutine set_shell4_indexes_from(self,index,a,b,c,d,atom_a,atom_b,atom_c,atom_d)
   ! For the quartet index "index" return the shell indicies "a", "b",
   ! "c" and "d" which map to "index" and the corresponding atom
   ! centers. Note different spelling for this routine and the one
   ! above to avoid overload problems.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      INT, OUT :: a,b,c,d
      INT, OUT :: atom_a,atom_b
      INT, OUT :: atom_c,atom_d
      INT :: ab,cd
      ab = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      cd = index-ab*(ab-1)/2
      a = (1+int(sqrt(8.0d0*ab-7.0d0)))/2
      b = ab-a*(a-1)/2
      c = (1+int(sqrt(8.0d0*cd-7.0d0)))/2
      d = cd-c*(c-1)/2
      atom_a = self%atom_for_shell(a)
      atom_b = self%atom_for_shell(b)
      atom_c = self%atom_for_shell(c)
      atom_d = self%atom_for_shell(d)
   end subroutine


!  ==============
!  Shell quartets
!  ==============

   PURE subroutine set_shell4_from_0(self,a,b,c,d,sh4)
   ! Get the SHELL4 object "sh4" correponding to the indicies "a",
   ! "b", "c" and "d".
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a,b,c,d
      type(SHELL4_TYPE), OUT :: sh4
      INT :: aa,bb,cc,dd
      INT :: sa,sb,sc,sd
      aa = self%atom_for_shell(a)
      bb = self%atom_for_shell(b)
      cc = self%atom_for_shell(c)
      dd = self%atom_for_shell(d)
      sa = self%atom_shell_for_shell(a)
      sb = self%atom_shell_for_shell(b)
      sc = self%atom_shell_for_shell(c)
      sd = self%atom_shell_for_shell(d)
      call set_(sh4%a,self%atom(aa)%basis%shell(sa),self%atom(aa)%position)
      call set_(sh4%b,self%atom(bb)%basis%shell(sb),self%atom(bb)%position)
      call set_(sh4%c,self%atom(cc)%basis%shell(sc),self%atom(cc)%position)
      call set_(sh4%d,self%atom(dd)%basis%shell(sd),self%atom(dd)%position)
   end subroutine

   PURE subroutine set_shell4_from_1(self,index,sh4,a,b,c,d)
   ! Get the SHELL4 object "sh4" correponding to the quartet index
   ! "index" Also return the shell indicies "a", "b", "c" and "d"
   ! which map to "index"
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      type(SHELL4_TYPE), OUT :: sh4
      INT, OUT :: a,b,c,d
      INT :: ab,cd
      INT :: aa,bb,cc,dd
      INT :: sa,sb,sc,sd
      ab = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      cd = index-ab*(ab-1)/2
      a = (1+int(sqrt(8.0d0*ab-7.0d0)))/2
      b = ab-a*(a-1)/2
      c = (1+int(sqrt(8.0d0*cd-7.0d0)))/2
      d = cd-c*(c-1)/2
      aa = self%atom_for_shell(a)
      bb = self%atom_for_shell(b)
      cc = self%atom_for_shell(c)
      dd = self%atom_for_shell(d)
      sa = self%atom_shell_for_shell(a)
      sb = self%atom_shell_for_shell(b)
      sc = self%atom_shell_for_shell(c)
      sd = self%atom_shell_for_shell(d)
      call set_(sh4%a,self%atom(aa)%basis%shell(sa),self%atom(aa)%position)
      call set_(sh4%b,self%atom(bb)%basis%shell(sb),self%atom(bb)%position)
      call set_(sh4%c,self%atom(cc)%basis%shell(sc),self%atom(cc)%position)
      call set_(sh4%d,self%atom(dd)%basis%shell(sd),self%atom(dd)%position)
   end subroutine

   PURE subroutine set_shell4_from_2(self,index,sh4,a,b,c,d,fa,la,fb,lb,fc,lc,fd,ld)
   ! Get the SHELL4 object "sh4" correponding to the quartet index
   ! "index" Also return the shell indicies "a", "b", "c" and "d"
   ! which map to "index" Also return the basis function start indices
   ! "fa", "la", etc ...
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      type(SHELL4_TYPE), OUT :: sh4
      INT, OUT :: a,b,c,d
      INT, OUT :: fa,la,fb,lb
      INT, OUT :: fc,lc,fd,ld
      INT :: ab,cd,aa,sa,bb,sb,cc,sc,dd,sd
      ab = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      cd = index-ab*(ab-1)/2
      a = (1+int(sqrt(8.0d0*ab-7.0d0)))/2
      b = ab-a*(a-1)/2
      c = (1+int(sqrt(8.0d0*cd-7.0d0)))/2
      d = cd-c*(c-1)/2
      aa = self%atom_for_shell(a)
      sa = self%atom_shell_for_shell(a)
      bb = self%atom_for_shell(b)
      sb = self%atom_shell_for_shell(b)
      cc = self%atom_for_shell(c)
      sc = self%atom_shell_for_shell(c)
      dd = self%atom_for_shell(d)
      sd = self%atom_shell_for_shell(d)
      call set_(sh4%a,self%atom(aa)%basis%shell(sa),self%atom(aa)%position)
      call set_(sh4%b,self%atom(bb)%basis%shell(sb),self%atom(bb)%position)
      call set_(sh4%c,self%atom(cc)%basis%shell(sc),self%atom(cc)%position)
      call set_(sh4%d,self%atom(dd)%basis%shell(sd),self%atom(dd)%position)
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      fc = self%first_basis_fn_for_shell(c)
      fd = self%first_basis_fn_for_shell(d)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      lc = self%last_basis_fn_for_shell(c)
      ld = self%last_basis_fn_for_shell(d)
   end subroutine

   PURE subroutine set_shell4_from_3(self,index,sh4,a,b,c,d,atom_a,atom_b,atom_c,atom_d)
   ! Get the SHELL4 object "shell" correponding to the quartet index
   ! "index" Also return the shell indicies "a", "b", "c" and "d"
   ! which map to "index" Plus the atoms which the shells are on.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: index
      type(SHELL4_TYPE), OUT :: sh4
      INT, OUT :: a,b,c,d
      INT, OUT :: atom_a,atom_b
      INT, OUT :: atom_c,atom_d
      INT :: ab,cd,sa,sb,sc,sd
      ab = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      cd = index-ab*(ab-1)/2
      a = (1+int(sqrt(8.0d0*ab-7.0d0)))/2
      b = ab-a*(a-1)/2
      c = (1+int(sqrt(8.0d0*cd-7.0d0)))/2
      d = cd-c*(c-1)/2
      atom_a = self%atom_for_shell(a)
      sa = self%atom_shell_for_shell(a)
      atom_b = self%atom_for_shell(b)
      sb = self%atom_shell_for_shell(b)
      atom_c = self%atom_for_shell(c)
      sc = self%atom_shell_for_shell(c)
      atom_d = self%atom_for_shell(d)
      sd = self%atom_shell_for_shell(d)
      call set_(sh4%a,self%atom(atom_a)%basis%shell(sa),self%atom(atom_a)%position)
      call set_(sh4%b,self%atom(atom_b)%basis%shell(sb),self%atom(atom_b)%position)
      call set_(sh4%c,self%atom(atom_c)%basis%shell(sc),self%atom(atom_c)%position)
      call set_(sh4%d,self%atom(atom_d)%basis%shell(sd),self%atom(atom_d)%position)
   end subroutine

   PURE subroutine set_shell4_ab_from(self,a,b,sh4)
   ! Set the a and b parts of the "sh4" SHELL4 object.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a,b
      type(SHELL4_TYPE), OUT :: sh4
      INT :: aa,sa
      INT :: bb,sb
      aa = self%atom_for_shell(a)
      bb = self%atom_for_shell(b)
      sa = self%atom_shell_for_shell(a)
      sb = self%atom_shell_for_shell(b)
      call set_(sh4%a,self%atom(aa)%basis%shell(sa),self%atom(aa)%position)
      call set_(sh4%b,self%atom(bb)%basis%shell(sb),self%atom(bb)%position)
   end subroutine

   PURE subroutine set_shell4_cd_from(self,c,d,sh4)
   ! Set the c and d parts of the "sh4" SHELL4 object.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: c,d
      type(SHELL4_TYPE), OUT :: sh4
      INT :: cc,sc
      INT :: dd,sd
      cc = self%atom_for_shell(c)
      dd = self%atom_for_shell(d)
      sc = self%atom_shell_for_shell(c)
      sd = self%atom_shell_for_shell(d)
      call set_(sh4%c,self%atom(cc)%basis%shell(sc),self%atom(cc)%position)
      call set_(sh4%d,self%atom(dd)%basis%shell(sd),self%atom(dd)%position)
   end subroutine

   PURE subroutine set_shell1q_ab_from(self,a,b,sh1q,skip)
   ! Set the "a" and "b" parts of the "sh1q" SHELL1QUARTET object. 
   ! If "skip" is TRUE then some primitive pairs are ignored, if too
   ! small (this should not be set if any atoms in the qurtet are the
   ! same; if in doubt set skip=FALSE).
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a,b
      type(SHELL1QUARTET_TYPE), INOUT :: sh1q
      BIN, IN :: skip
      INT :: aa,sa
      INT :: bb,sb
      VEC(REAL,3) :: pa,pb
      aa = self%atom_for_shell(a)  !which atom.
      bb = self%atom_for_shell(b)
      sa = self%basis_shell_for_shell(a)  !which shell.
      sb = self%basis_shell_for_shell(b)
      pa = self%atom(aa)%position
      pb = self%atom(bb)%position
      call set_ab_new_(sh1q,self%basis_shell1pair(sa,sb),pa,pb,aa==bb,skip)
   end subroutine

   PURE subroutine set_shell1q_cd_from(self,c,d,sh1q,skip)
   ! Set the "c" and "d" parts of the "sh1q" SHELL1QUARTET object. 
   ! If "skip" is TRUE then some primitive pairs are ignored, if too
   ! small (this should not be set if any atoms in the qurtet are the
   ! same; if in doubt set skip=FALSE).
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: c,d
      type(SHELL1QUARTET_TYPE), INOUT :: sh1q
      BIN, IN :: skip
      INT :: cc,sc
      INT :: dd,sd
      VEC(REAL,3) :: pc,pd
      cc = self%atom_for_shell(c)  !which atom.
      dd = self%atom_for_shell(d)
      sc = self%basis_shell_for_shell(c)  !which shell.
      sd = self%basis_shell_for_shell(d)
      pc = self%atom(cc)%position
      pd = self%atom(dd)%position
      call set_cd_new_(sh1q,self%basis_shell1pair(sc,sd),pc,pd,cc==dd,skip)
   end subroutine


!  ======================
!  Make shell information
!  ======================

   PURE subroutine update_shell_info(self)
   ! Define a vector of atom numbers corresponding to the molecule's
   ! basis set shell numbers; also define a vector of atom shell
   ! numbers corresponding to the molecule basis set shell number; 
   ! and precalculated shell info for integrals.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.SET:update_shell_info ... no atom information")
   ENSURE(bases_are_resolved_(self%atom),"MOLECULE.SET:update_shell_info ... gaussian bases not all resolved")
   ENSURE(self%n_atom>0,"MOLECULE.SET:update_shell_info ... no atoms")
   ENSURE(allocated(self%basis),"MOLECULE.SET:update_shell_info ... no basis set")

      ! Set basis l_max before anything else
      self%basis_l_max = maximum_basis_set_l_value_(self%basis)
   ! GAUSSIAN_DATA:set_indices(.basis_l_max)

      ! Set atom for shell
      call destroy_(self%atom_for_shell)
      call make_atom_for_shell_(self%atom,self%atom_for_shell)
      call destroy_(self%atom_shell_for_shell)
      call make_atom_shell_for_shell_(self%atom,self%atom_shell_for_shell)

      ! Set first/last shell for atom
      call destroy_(self%first_shell_for_atom)
      call destroy_(self%last_shell_for_atom)
      call create_(self%first_shell_for_atom,self%n_atom)
      call create_(self%last_shell_for_atom,self%n_atom)
      call make_shell_for_atom_limits_(self%atom,self%first_shell_for_atom,self%last_shell_for_atom)

      ! Set first/last bf for shell
      call destroy_(self%first_basis_fn_for_shell)
      call destroy_(self%last_basis_fn_for_shell)
      call destroy_(self%angular_moment_for_shell)
      call make_shell_limits_(self%atom,self%first_basis_fn_for_shell,self%last_basis_fn_for_shell,self%angular_moment_for_shell)

      ! The same limits counting cartesian components. The J/K engine is
      ! cartesian only, so a spherical basis builds J and K through these.
      call destroy_(self%first_cartesian_bf_for_shell)
      call destroy_(self%last_cartesian_bf_for_shell)
      call make_cartesian_shell_limits_(self%atom,self%first_cartesian_bf_for_shell,self%last_cartesian_bf_for_shell)
      self%n_bf_cartesian = self%last_cartesian_bf_for_shell(size(self%last_cartesian_bf_for_shell))

      ! Set first/last bf for atom
      call destroy_(self%first_basis_fn_for_atom)
      call destroy_(self%last_basis_fn_for_atom)
      call make_atom_basis_fn_limits_(self%atom,self%first_basis_fn_for_atom,self%last_basis_fn_for_atom)

      ! Shell pointer into flattened .basis vector
      call destroy_(self%basis_shell_for_shell)
      call make_basis_shell_for_shell_(self%atom,self%basis_shell_for_shell,self%basis)

      ! Precalculate non-positional basis-pair information
      self%n_unique_shells = no_of_shells_(self%basis)
      call destroy_(self%basis_shell1pair)
      call create_(self%basis_shell1pair,self%n_unique_shells,self%n_unique_shells)
      call make_precomputed_shellpairs_(self%basis_shell1pair,self%basis)
   end subroutine


!  ===============
!  Atom-shell info
!  ===============

   PURE subroutine update_atom_shell_info(self)
   ! Get the atom shell info for all atoms in the molecule. We make it
   ! only for the unique atoms and pointer copy to the non-unique
   ! atoms, just like for ANO atom information.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: a,b
      VEC(BIN,:), allocatable :: done
      BIN :: first
   ENSURE(allocated(self%atom),"MOLECULE.SET:update_atom_shell_info ... no atoms")
   ENSURE(self%atom_info_made,"MOLECULE.SET:update_atom_shell_info ... no atom info")
   ENSURE(self%basis_info_made,"MOLECULE.SET:update_atom_shell_info ... no basis_set info")
   ENSURE(allocated(self%basis),"MOLECULE.SET:update_atom_shell_info ... no basis set")
   ENSURE(self%n_atom_kind>0,"MOLECULE.SET:update_atom_shell_info ... no atom kinds set")

      ! Already there?
      if (has_all_shell_info_(self)) return
      call create_(done,self%n_atom)
      done = FALSE
      first = TRUE
      do a = 1,self%n_atom

         ! Unique atom?
         if (done(a)) cycle

         ! Make the shell info
         call make_shell_info_(self%atom(a))
         done(a) = TRUE

         ! Copy it to non-unique atoms
         do b = a+1,self%n_atom

            ! Skip?
            if (done(b)) cycle
            if (NOT is_same_kind_as_(self%atom(b),self%atom(a))) cycle
            self%atom(b)%first_basis_fn_for_shell = self%atom(a)%first_basis_fn_for_shell
            self%atom(b)%last_basis_fn_for_shell = self%atom(a)%last_basis_fn_for_shell
            self%atom(b)%angular_moment_for_shell = self%atom(a)%angular_moment_for_shell
            self%atom(b)%basis_shell1pair = self%atom(a)%basis_shell1pair
            done(b) = TRUE
         end do
      end do
      call destroy_(done)
   end subroutine


!  =========
!  Archiving
!  =========

   subroutine delete_archive(self,name)
   ! Delete the archive called "name"
      type(MOLECULE_TYPE), IN :: self
      STR(len=*), IN :: name
      type(ARCHIVE_TYPE) :: archive
      call set_(archive,self%name,name)
      call delete_all_genres_(archive)
   end subroutine


! May be needed by involved non-SCF routines

   recursive subroutine delete_SCF_archives(self,keep_MOs,keep_DM)
   ! Delete any SCF-procedure generated archives stored on disk.
      type(MOLECULE_TYPE), IN :: self
      BIN, optional, IN :: keep_MOs
      BIN, optional, IN :: keep_DM
      type(ARCHIVE_TYPE) :: archive
      BIN :: delete_MOs,delete_DM
      delete_MOs = TRUE
      if (present(keep_MOs)) delete_MOs = NOT keep_MOs
      delete_DM = TRUE
      if (present(keep_DM)) delete_DM = NOT keep_DM
      if (delete_MOs) then
         call set_(archive,self%name,"MOs"); call delete_all_genres_(archive)
         call set_(archive,self%name,"MO_energies"); call delete_all_genres_(archive)
      end if
      if (delete_DM) then
         call set_(archive,self%name,"density_mx"); call delete_all_genres_(archive)
      end if
      call set_(archive,self%name,"Fock_mx"); call delete_all_genres_(archive)
      call set_(archive,self%name,"NOs"); call delete_all_genres_(archive)
      call set_(archive,self%name,"NO_occ_nos"); call delete_all_genres_(archive)
      call set_(archive,self%name,"energy"); call delete_all_genres_(archive)
      call set_(archive,self%name,"promolecule_density_mx"); call delete_all_genres_(archive)
      call set_(archive,self%name,"group_density_mx"); call delete_all_genres_(archive)
      call set_(archive,self%name,"group_MOs"); call delete_all_genres_(archive)
      call set_(archive,self%name,"U_electric_dipole"); call delete_all_genres_(archive)
      call set_(archive,self%name,"U2_electric_dipole"); call delete_all_genres_(archive)
      call set_(archive,self%name,"J_mx"); call delete_all_genres_(archive)
      call set_(archive,self%name,"constraint_mx"); call delete_all_genres_(archive)
      call set_(archive,self%name,"ERI_integrals"); call delete_all_genres_(archive)
      call set_(archive,self%name,"ERI_index"); call delete_all_genres_(archive)

      ! WARNING: These two below are recursive
      call delete_atom_SCF_archives_(self)
      call delete_group_SCF_archives_(self,keep_MOs,keep_DM)
      call delete_lambda_SCF_archives_(self,keep_MOs)
   end subroutine

   subroutine delete_lambda_SCF_archives(self,keep_MOs)
   ! Delete any lambda type archives stored on disk.
      type(MOLECULE_TYPE), IN :: self
      BIN, optional, IN :: keep_MOs
      REAL :: l,l_max
      STR(len=5) :: l_str
      type(ARCHIVE_TYPE) :: archive
      BIN :: delete_MOs
      delete_MOs = TRUE
      if (present(keep_MOs)) delete_MOs = NOT keep_MOs
      if (NOT delete_MOs) return
      if (NOT allocated(self%SCF_data)) return
      if (NOT is_constrained_scf_(self%SCF_data)) return
      l = self%SCF_data%initial_lambda
      l_max = self%SCF_data%lambda_max+l
      do
         if (l>l_max) exit
         l_str = trim(to_str_(l,"f5.3"))
         call set_(archive,self%name,"MOs,lambda="//trim(l_str)); call delete_all_genres_(archive)
         call set_(archive,self%name,"MO_energies,lambda="//trim(l_str)); call delete_all_genres_(archive)
         l = l+self%SCF_data%lambda_step
      end do
   end subroutine

   recursive subroutine delete_atom_SCF_archives(self)
   ! Delete the atom SCF archives, if they exist.
      type(MOLECULE_TYPE), IN :: self
      INT :: a
      type(MOLECULE_TYPE) :: mol
      if (self%n_atom_kind<=0) return
      do a = 1,self%n_atom

         ! Unique atom?
         if (NOT self%atom(a)%is_unique_kind) cycle

         ! Delete
         call set_defaults_(mol)
         mol%name = chemical_symbol_(self%atom(a))
         call delete_SCF_archives_(mol)
      end do
   end subroutine

   recursive subroutine delete_group_SCF_archives(self,keep_MOs,keep_DM)
   ! Delete any group type SCF archives.
      type(MOLECULE_TYPE), IN :: self
      BIN, optional, IN :: keep_MOs
      BIN, optional, IN :: keep_DM
      INT :: g
      if (NOT allocated(self%atom_group)) return
      if (NOT allocated(self%mol)) return
      do g = 1,size(self%atom_group)
         call delete_SCF_archives_(self%mol(g),keep_MOs,keep_DM)
      end do
   end subroutine

   subroutine delete_SCF_MO_archive(self)
   ! Delete the SCF MO archives
      type(MOLECULE_TYPE), IN :: self
      type(ARCHIVE_TYPE) :: archive
      call set_(archive,self%name,"MOs"); call delete_all_genres_(archive)
      call set_(archive,self%name,"MO_energies"); call delete_all_genres_(archive)
   end subroutine


!  ===============
!  Plot grid files
!  ==============

   subroutine delete_plot_grid_0(self,name)
   ! Delete the plot grid called "name".
      type(MOLECULE_TYPE), IN :: self
      STR(len=*), IN :: name
      STR(len=STR_SIZE) :: title
      type(ARCHIVE_TYPE) :: arch

      ! Set title to be molecule name + sub-title from plot
      title = self%name
      if (allocated(self%plot_grid)) then
         if (self%plot_grid%plot_label/=" ") then
            title = trim(self%name)//","//trim(self%plot_grid%plot_label)
         end if
      end if

      ! Delete
      call set_(arch,title,name,format="gnuplot"); call delete_all_genres_(arch)
      call set_(arch,title,name,format="mathematica"); call delete_all_genres_(arch)
      call set_(arch,title,name,format="contour"); call delete_all_genres_(arch)
      call set_(arch,title,name,format="gaussian.cube"); call delete_all_genres_(arch)
      call set_(arch,title,name,format="xdgraph"); call delete_all_genres_(arch)
   end subroutine

   subroutine delete_plot_grid_1(self)
   ! Delete the plot grid whose name is specified on the input line
      type(MOLECULE_TYPE), IN :: self
      STR(len=STR_SIZE) :: name
      STR(len=STR_SIZE) :: title
      type(ARCHIVE_TYPE) :: arch
   DIE_IF(stdin%buffer%n_items/=2,"MOLECULE.SET:delete_plot_grid_1 ... must specify grid to delete in input")
   DIE_IF(stdin%buffer%item_index==2,"MOLECULE.SET:delete_plot_grid_1 ... must be on the last item")
      call read_(stdin,name)

      ! Set title to be molecule name + sub-title from plot
      title = self%name
      if (allocated(self%plot_grid)) then
         if (self%plot_grid%plot_label/=" ") then
            title = trim(self%name)//","//trim(self%plot_grid%plot_label)
         end if
      end if

      ! Delete
      call set_(arch,title,name,format="gnuplot"); call delete_all_genres_(arch)
      call set_(arch,title,name,format="mathematica"); call delete_all_genres_(arch)
      call set_(arch,title,name,format="contour"); call delete_all_genres_(arch)
      call set_(arch,title,name,format="gaussian.cube"); call delete_all_genres_(arch)
      call set_(arch,title,name,format="xdgraph"); call delete_all_genres_(arch)
   end subroutine

end module
