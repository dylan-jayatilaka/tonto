!==================================================================
!
! MOLECULE.HAR: 
!
! Hirshfeld atom refinement, invarioms and related concepts.
!
! Copyright (C) Dylan Jayatilaka, 2012
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
!==============================================================================-
module MOLECULE_HAR_MODULE

#  include "macros"

   include "molecule.har.use"

   implicit none

   include "molecule.har.int"


contains

!  =========================
!  Hirshfeld atom refinement
!  =========================

   subroutine fragHAR_refinement(self)
   ! Do an fragment HAR.
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: results
      BIN :: output
      BIN :: do_ASFs
   DIE_IF(NOT allocated(self%atom),"MOLECULE.HAR:fragHAR_refinement ... no atoms to fit")
   DIE_IF(NOT allocated(self%atom_group),"MOLECULE.HAR:fragHAR_refinement ... no atom_group's")
   DIE_IF(NOT allocated(self%SCF_data),"MOLECULE.HAR:fragHAR_refinement ... no SCF_data")
   DIE_IF(NOT allocated(self%crystal),"MOLECULE.HAR:fragHAR_refinement ... no crystal")
   DIE_IF(NOT allocated(self%crystal%xray_data),"MOLECULE.HAR:fragHAR_refinement ... no crystal xray data")
   DIE_IF(NOT allocated(self%crystal%xray_data%reflections),"MOLECULE.HAR:fragHAR_refinement ... no reflections")

      ! Use disk SFs for now ...
      call set_use_disk_SFs_(self%crystal%xray_data,TRUE)

      ! Output options
      results = self%crystal%xray_data%show_refinement_results
      output = self%crystal%xray_data%show_refinement_output

      ! ANO's and interpolators?
      call make_atom_partition_info_(self)

      ! Initial F_calc's for pruning
      if (NOT have_F_pred_(self%crystal%xray_data%reflections)) then
         if (output) then
            call flush_(stdout)
            call text_(stdout,"Making F_pred ...")
         end if
         do_ASFs = NOT self%SCF_data%restart_skip_asfs
         call fragment_SCF_(self,do_SFs=do_ASFs)
         call make_F_predicted_(self%crystal)
      ! .make_X_SFs
      end if

      ! Prune?
      if (is_F_calc_prunable_(self%crystal%xray_data)) then

         ! Prune
         if (output) then
            call flush_(stdout)
            call text_(stdout,"Pruning reflections with F_pred/F_calc for refinement ...")
         end if
         call prune_xray_data_(self%crystal)
      end if

      ! Initialize message
      if (output) then
         call flush_(stdout)
         call text_(stdout,"Starting refinement ...")
      end if
      call initialize_refinement_data_(self%crystal)

      ! Turn off unwanted printing ...
      call set_refine_fragments_(self%crystal%xray_data,TRUE)
      call set_refine_structure_(self%crystal%xray_data,TRUE)
      call set_fit_structure_(self%crystal%xray_data,FALSE)
      call set_output_(self%SCF_data,FALSE)

      ! Banner, options/switches, initial parameters,
      ! start banner, and iteration table header
      if (output) then
         call put_refinement_header_info_(self%crystal)
      end if
      do  ! ... LS cycle

         ! Remake fragments & do SCF
         if (self%crystal%xray_data%ref_iteration>0) then
            call update_atom_groups_(self)  ! needed?
            call fragment_SCF_(self,restart=TRUE,do_SFs=TRUE)
         end if

         !!!!!!!!!!!!!!!!!!!!!!
         call LS_fit_fragHAR_disk_(self)
         !!!!!!!!!!!!!!!!!!!!!!

         ! Update geometry & errors
         call update_refinement_parameters_(self%crystal%xray_data)
         call update_refinement_ESDs_(self%crystal)

         ! Put the iteration results
         if (output) then
            call put_refinement_table_body_(self%crystal%xray_data)
         end if

         ! Finished?
         call update_refinement_info_(self%crystal%xray_data)
         if (self%crystal%xray_data%refinement_finished) exit
      end do  ! ... LS cycle
      call update_refinement_info_(self%crystal%xray_data)

      ! Table footer
      if (output) then
         call put_footer_(self%crystal%xray_data%ref_table)
      end if

      ! Final results
      if (results) then
         call put_refinement_results_(self%crystal)
      end if

      ! Final bond lengths, angles, torsions, with errors
      if (output) then
         call put_bond_info_with_errors_(self%crystal)
      end if

      ! SCF @ final geometry
      call fragment_SCF_(self,restart=TRUE)
      call set_output_(self%SCF_data,TRUE)  ! put back

      ! Put analysis info ...
      if (self%crystal%xray_data%do_standard_plots) then
         call put_X_data_analysis_(self%crystal,output=TRUE,job=self%name)
      end if

      ! Final results
      call put_correlations_(self%crystal,name=trim(self%name)//".correlation-coefficients")

      ! Put residuals ...
      if (self%crystal%xray_data%do_residual_cube) call get_minmax_residual_density_(self,output=TRUE)

      ! Put final CIF after residuals
      call put_CIFs_(self)

      ! Do standatd plots and properties
      if (self%crystal%xray_data%do_standard_plots) then
      end if
   end subroutine

   subroutine LS_fit_fragHAR_disk(self)
   ! LS fit the Hirshfeld atoms to the calculated structure factors.
   ! Disk version
      type(MOLECULE_TYPE), INOUT :: self

      ! Do a least squares
      call LS_fit_(self,self%crystal%xray_data%show_fit_output,self%crystal%xray_data%show_fit_results)

      ! Put final CIF after residuals
      call put_CIFs_(self)
   end subroutine


!   TAR_refinement :: leaky
!   ! Do a Tanaka atom refinement (TAR) on the X-ray data. 
!      self :: INOUT
!
!   DIE_IF(NOT .atom.allocated, "no atoms to fit")
!   DIE_IF(NOT .SCF_data.allocated,"no SCF_data")
!   DIE_IF(NOT .crystal.allocated, "no crystal")
!   DIE_IF(NOT .crystal.xray_data.allocated,"no crystal xray data")
!   DIE_IF(NOT .crystal.uses_one_center_TF_model,"must use one-center temperature factor model")
!
!      results,output :: BIN
!
!      ! Output options
!      results = .crystal.xray_data.show_refinement_results
!      output  = .crystal.xray_data.show_refinement_output
!      output = TRUE
!
!      ! Initial SCF
!      if (.density_mx.deallocated) then
!
!         if (output) then
!            stdout.flush
!            stdout.text("Initial density matrix for refinement ...")
!         end
!
!         ! Initial promolecule SCF
!         if (.SCF_data.using_cluster_charges) .promolecule_scf
!
!         .scf
!
!      end
!
!      ! Always use initial_density for stability
!      .SCF_data.set_initial_density(.SCF_data.spinorbital_genre)
!
!      ! Initial F_calc's for pruning
!      if (NOT .crystal.xray_data.reflections.have_F_pred) then
!         if (output) then
!            stdout.flush
!            stdout.text("Making F_pred ...")
!         end
!         .make_X_SFs
!      end
!
!      ! Prune?
!      if (.crystal.xray_data.is_F_calc_prunable) then
!         if (output) then
!            stdout.flush
!            stdout.text("Pruning reflections with F_pred/F_calc for refinement ...")
!         end
!         .crystal.xray_data.prune_reflections
!      end
!
!      ! Initialize message
!      if (output) then
!         stdout.flush
!         stdout.text("Starting refinement ...")
!      end
!
!      .crystal.initialize_refinement_data
!
!      ! Turn off unwanted printing ...
!      .crystal.xray_data.set_refine_structure(TRUE)
!      .crystal.xray_data.set_fit_structure(FALSE)
!      .SCF_data.set_output(FALSE)
!
!      ! Banner, options/switches, initial parameters,
!      ! start banner, and iteration table header
!      if (output) then
!         .crystal.put_refinement_header_info
!      end
!
!      ! Refinement cycle 
!      do
!
!         ! SCF
!         if (.crystal.xray_data.ref_iteration>0) then

!            .promolecule_scf
!            .scf
!         end
!
!         !!!!!!!!!!!!!!!!!!!!!
!         .LS_fit_Tanaka_atoms
!         !!!!!!!!!!!!!!!!!!!!!
!
!         ! Update geometry & errors
!         .crystal.xray_data.update_refinement_parameters
!         .crystal.update_refinement_ESDs
!
!         ! Put the iteration results
!         if (output) then
!            .crystal.xray_data.put_refinement_table_body
!         end
!
!         ! Finished?
!         .crystal.xray_data.update_refinement_info
!         if (.crystal.xray_data.refinement_finished) exit
!
!      end ! ... LS cycle
!
!      .crystal.xray_data.update_refinement_info
!
!      ! Table footer
!      if (output) then
!         .crystal.xray_data.ref_table.put_footer
!      end
!
!      ! Final results
!      if (results) then
!         .crystal.put_refinement_results
!      end
!
!      ! Final bond lengths, angles, torsions, with errors
!      if (output) then
!         .crystal.put_bond_info_with_errors
!      end
!
!      ! SCF @ final geometry
!      .scf
!      .SCF_data.set_output(TRUE) ! put back
!
!      ! Put analysis info ...
!      if (.crystal.xray_data.do_standard_plots) then
!         .crystal.put_X_data_analysis(output=TRUE,job=.name)
!      end
!
!      ! Put residuals ...
!      if (.crystal.xray_data.do_residual_cube) then
!         .get_minmax_residual_density(output=TRUE)
!
!      ! Put final CIF after residuals
!      .put_CIFs
!
!      ! Do standatd plots and properties
!      if (.crystal.xray_data.do_standard_plots) then
!      end
!
!   end

!   LS_fit_Tanaka_atoms :: leaky
!   ! LS fit the Hirshfeld atoms to the calculated structure factors.
!   ! In-memory version.
!      self :: INOUT
!
!      ff :: MAT{CPX}@
!
!      ! Hirshfeld-atom structure factors
!      ! NOTE: in iteration 0, these were already calculated
!      ! as part of make_X_SFs ... double work!
!      .get_Tanaka_atom_FFs(ff)
!
!      ! Do a least squares
!      .LS_fit(ff,.crystal.xray_data.show_fit_output,.crystal.xray_data.show_fit_results)
!
!      ! Final results
!      .crystal.put_correlations(name=trim(.name)//".correlation-coefficients")
!
!      ! Put final CIF after residuals
!      .put_CIFs
!
!      ! Clean up
!      ff.destroy
!
!   end

   subroutine HAR_refinement(self)
   ! Do a Hirshfeld atom refinement (HAR) on the X-ray data. 
   ! The wavefunction and form factors are recalculated every 
   ! time the geometry changes, until appropriately converged, 
   ! normally to a maximum relative change of 0.01\sigma
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: results
      BIN :: output
   DIE_IF(NOT allocated(self%atom),"MOLECULE.HAR:HAR_refinement ... no atoms to fit")
   DIE_IF(NOT allocated(self%SCF_data),"MOLECULE.HAR:HAR_refinement ... no SCF_data")
   DIE_IF(NOT allocated(self%crystal),"MOLECULE.HAR:HAR_refinement ... no crystal")
   DIE_IF(NOT allocated(self%crystal%xray_data),"MOLECULE.HAR:HAR_refinement ... no crystal xray data")
   DIE_IF(NOT allocated(self%crystal%xray_data%reflections),"MOLECULE.HAR:HAR_refinement ... no x-ray reflections")
   DIE_IF(NOT using_one_center_TF_model_(self%crystal),"MOLECULE.HAR:HAR_refinement ... must use one-center temperature factor model")

      ! This is a HAR
      call set_refine_structure_(self%crystal%xray_data,TRUE)
      call set_output_(self%SCF_data,FALSE)

      ! Output options
      results = self%crystal%xray_data%show_refinement_results
      output = self%crystal%xray_data%show_refinement_output

      ! Set up atom partition info
      call make_atom_partition_info_(self)

      ! Initial SCF
      if (NOT allocated(self%density_mx)) then
         if (output) then
            call flush_(stdout)
            call text_(stdout,"Initial density matrix for refinement ...")
         end if
         ! Initial promolecule SCF
         if (self%SCF_data%using_cluster_charges) call promolecule_SCF_(self)
         call SCF_(self)  ! ???
      end if

      ! Initial F_calc's for pruning
      if (NOT have_F_pred_(self%crystal%xray_data%reflections)) then
         if (output) then
            call flush_(stdout)
            call text_(stdout,"Making F_pred ...")
         end if
         call make_X_SFs_(self)
      end if

      ! Prune?
      if (is_F_calc_prunable_(self%crystal%xray_data)) then
         if (output) then
            call flush_(stdout)
            call text_(stdout,"Pruning reflections with F_pred/F_calc for refinement ...")
         end if
         call prune_xray_data_(self%crystal)
      end if

      ! Initialize & turn off SCF printing ...
      call initialize_refinement_data_(self%crystal)

      ! Always use initial_density for stability
      call set_initial_density_(self%SCF_data,spinorbital_genre_(self%SCF_data))

      ! A TLS model needs the stiff-mode ADPs
      if (self%crystal%xray_data%ADP_model=="tls") call set_crystal_U_high_(self)

      ! BANNER: options/switches, initial parameters,
      ! iteration banner, and iteration table header
      if (output) then
         call flush_(stdout)
         call text_(stdout,"Starting refinement ...")
         call put_refinement_header_info_(self%crystal)
      end if

      !==========
      ! HAR cycle
      !==========
      do

         ! SCF: skip first one
         if (self%crystal%xray_data%ref_iteration>0) then
            if (self%crystal%xray_data%restart_HAR_with_promolecule) call promolecule_SCF_(self)
            call SCF_(self)  ! ???
         end if

         !=======================
         call LS_fit_Hirshfeld_atoms_(self)
         !=======================

         ! Update geometry & errors
         call update_refinement_parameters_(self%crystal%xray_data)
         call update_refinement_ESDs_(self%crystal)

         ! Put the iteration results
         if (output) then
            call put_refinement_table_body_(self%crystal%xray_data)
         end if

         ! Finished?
         call update_refinement_info_(self%crystal%xray_data)
         if (self%crystal%xray_data%refinement_finished) exit
      end do  ! HAR cycle

      ! Table footer
      if (output) then
         call put_footer_(self%crystal%xray_data%ref_table)
      end if

      ! Final results
      if (results) then
         call put_refinement_results_(self%crystal)
      end if

      ! Final bond lengths, angles, torsions, with errors
      if (output) then
         call put_bond_info_with_errors_(self%crystal)
      end if

      ! SCF @ final geometry
      call SCF_(self)
      call set_output_(self%SCF_data,TRUE)  ! put back

      ! Put analysis info ...
      if (self%crystal%xray_data%do_standard_plots) then
         call put_X_data_analysis_(self%crystal,output=TRUE,job=self%name)
      end if

      ! Put residuals ...
      if (self%crystal%xray_data%do_residual_cube) then
         call get_minmax_residual_density_(self,output=TRUE)
      end if

      ! Put final CIF after residuals
      call put_CIFs_(self)

      ! Do standatd plots and properties
      if (self%crystal%xray_data%do_standard_plots) then
      end if
   end subroutine

   subroutine HA_fit(self)
   ! Do a single least-squares fit with HA form factor
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: results
      BIN :: output

      ! This is a rigid-atom fit
      call set_fit_structure_(self%crystal%xray_data,TRUE)

      ! Output options
      results = self%crystal%xray_data%show_fit_results
      output = self%crystal%xray_data%show_fit_output

      ! ANO's and interpolators?
      call make_atom_partition_info_(self)

      ! Initial SCF
      if (NOT allocated(self%density_mx)) then
         if (output) then
            call flush_(stdout)
            call text_(stdout,"Initial density matrix for refinement ...")
         end if
         ! Initial promolecule SCF
         if (self%SCF_data%using_cluster_charges) call promolecule_SCF_(self)
         call SCF_(self)  ! ???
      end if

      ! Initial F_calc's for pruning
      if (NOT have_F_pred_(self%crystal%xray_data%reflections)) then
         if (output) then
            call flush_(stdout)
            call text_(stdout,"Making F_pred ...")
         end if
         call make_X_SFs_(self)
      end if

      ! Prune?
      if (is_F_calc_prunable_(self%crystal%xray_data)) then
         if (output) then
            call flush_(stdout)
            call text_(stdout,"Pruning reflections with F_pred/F_calc for refinement ...")
         end if
         call prune_xray_data_(self%crystal)
      end if

      ! Do the LSQ fit
      ! Header output appears inside here
      call LS_fit_Hirshfeld_atoms_(self)
   end subroutine

   subroutine LS_fit_Hirshfeld_atoms(self)
   ! LS fit the Hirshfeld atoms to the calculated structure factors.
   ! Cartesian (bohr) coordinates are used because structure factors
   ! are aspherical and calculated from the QM wavefunction.
   ! NOTE: this routine does only one cycle of fitting with the
   ! current density matrix i.e. this is a "rigid atom fit".
   ! NOTE: It's tricky exactly which atom's are used for refinement:
   ! * Structure factors are calculated only for *unique* fragment
   !   atoms since the QM wavefunction refers to the fragment_atoms.
   ! * However, asymmetric unit atoms are used to "store" the updating
   !   geometry; thus unique fragment atom pADPs and shifts are
   !   calculated (and derivatives with respect to the fragment atom
   !   positions and ADPs) but these parameters and shifts are later
   !   transformed back into asymmetric unit.
      type(MOLECULE_TYPE), INOUT :: self
      if (self%crystal%xray_data%use_disk_SFs) then; call LS_fit_HAs_disk_(self)
      else; call LS_fit_HAs_memory_(self)
      end if

      ! Finalise stuff at new geometry ...

      ! Orthonormalize MO's at new geometry
      if (allocated(self%MOs)) then
         if (NOT self%crystal%xray_data%partition_model=="oc-crystal23") then
            if (allocated(self%SCF_data)) then
               if (NOT self%SCF_data%SCF_kind(1:8)=="fragment") then

         ! Orthonormalize MO's at new geometry
                  call symmetrically_orthonormalize_(self,self%MOs)
               end if
            end if
         end if
      end if

      ! Delete stored matrices
      ! NOTE: DM is incorrect, but OK as a guess.
      call delete_SCF_archives_(self,keep_DM=TRUE)
      call destroy_SCF_results_(self,keep_MOs=TRUE)
      call destroy_SCF_integrals_(self)

      ! New geometry, new moments
      self%partition_info_made = FALSE
   end subroutine

   subroutine LS_fit_HAs_memory(self)
   ! LS fit the Hirshfeld atoms to the calculated structure factors.
   ! In-memory version.
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: fitout
      BIN :: fitres
      MAT(CPX,:,:), allocatable :: ff
   DIE_IF(NOT allocated(self%crystal),"MOLECULE.HAR:LS_fit_HAs_memory ... no crystal")

      ! Hirshfeld-atom structure factors
      ! NOTE: in iteration 0, these were already calculated
      !       as part of make_X_SFs ... double work! Oh well.
      !       Store in .xray_data?
      if (self%crystal%xray_data%ref_iteration>=1) call make_atom_partition_info_(self)
      call make_aspherical_atomic_FFs_(self,ff)

      ! Symmetrize form factors "ff" -- kang (2024.Jun.3) 
      ! ... Thank you! --dylan
      call symmetrize_FFs_(self%crystal,ff)

      ! Least squares (maybe with output)
      ! Here, .atom = .crystal.fragment_atom
      fitout = self%crystal%xray_data%show_fit_output
      fitres = self%crystal%xray_data%show_fit_results
      call LS_fit_(self,ff,fitout,fitres)

      ! Put final CIF after residuals
      call put_CIFs_(self)

      ! Clean up
      call destroy_(ff)
   end subroutine


!  Strategy: insert calls, separate out sf_u.
!  End result: one shift update of parameters
!  Then this goes in the normal LSQ cycle in LS_fit
!  NOTE: routine make_LS_mx is very long! Is it working? Rewrite?

   subroutine LS_fit_HAs_disk(self)
   ! LS fit the Hirshfeld atoms to the calculated structure factors.
   ! Low memory version.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: d_Fa

      ! Once-only preamble, mirroring CRYSTAL:LS_structure_fit (crystal.foo:4272
      ! onwards). All four used to run inside make_LS_mx, hence once per LS
      ! iteration -- and the X_fit seeding once per ATOM per iteration, since it
      ! sat inside that routine's `do u` loop.
      call initialize_fit_data_(self%crystal)
      call put_pADP_vector_to_(self%crystal%asymmetric_unit_atom,self%crystal%xray_data%X_fit)
      if (self%crystal%xray_data%refine_structure) self%crystal%xray_data%X_ref = self%crystal%xray_data%X_fit
      call put_fit_header_(self%crystal)
      do

         ! This allocation is REAL, and make_LS_mx relies on it: its d_Fa
         ! dummy is INOUT, so what is created here is what it fills. It was
         ! declared OUT, which deallocates an allocatable on entry and made
         ! this line dead code -- and the job segfaulted. See make_LS_mx.
         call create_(d_Fa,size(self%crystal%xray_data%reflections),no_of_pADPs_(self%crystal%asymmetric_unit_atom))
         call make_LS_mx_(self,d_Fa)
         call do_LS_refinement_(self,d_Fa)
         call destroy_(d_Fa)
         call put_fit_table_body_(self%crystal%xray_data)

         ! Finished?
         !
         ! update_fit_info is what DECIDES that, and it was missing: it sets
         ! .fit_converged / .too_many_fit_iterations, and from them
         ! .fit_finished, and it is also the only thing that advances
         ! .fit_iteration (diffraction_data.set.foo, "fit_iteration =
         ! fit_iteration + 1" at the end). Without it the loop tested a flag
         ! nothing ever set: the fit re-solved iteration 1 forever, reprinting
         ! identical rows -- 599 of them before a timeout, GoF2 49.702984 every
         ! time -- and never converged.
         !
         ! This loop is hand-rolled. The two that work call update_fit_info
         ! immediately before the same test: CRYSTAL:LS_structure_fit
         ! (crystal.foo:4332 and :4472), which is where LS_fit_HAs_MEMORY and
         ! LS_fit_fragHAR_disk both end up. That is why the in-core path and
         ! fragHAR converge and only this one did not.
         call update_fit_info_(self%crystal%xray_data)
         if (self%crystal%xray_data%fit_finished) exit
      end do

      ! Table footer
      call put_footer_(self%crystal%xray_data%fit_table)

      ! Set the fragment covariances from cov matrix
      call set_fragment_esds_(self%crystal)

      ! Print out bond length, angle, torsion tables with errors
      call put_bond_info_with_errors_(self%crystal)

      ! Finally: remake .asymmetric_unit_atom/unit_cell geometry from
      ! .fragment_atom, which *must* be assigned since it must point
      ! to molecule.atom
      call destroy_(self%crystal%asymmetric_unit_atom)
      call destroy_(self%crystal%asymmetric_unit_geometry)
      self%crystal%asymmetric_unit_source = "tonto-ls-structure-fit"
      call update_fragment_info_(self%crystal,self%crystal%fragment_atom)

      ! Set geometry type
      self%crystal%asymmetric_unit_source = "tonto-refinement"

      ! Final results
      call put_correlations_(self%crystal,name=trim(self%name)//".correlation-coefficients")

      ! Put final CIF after residuals
      call put_CIFs_(self)
      if (self%crystal%xray_data%is_IAM_refinement) then
         if (NOT self%crystal%xray_data%use_IAM_ITC_FFs) then

         ! Delete stored matrices
            call delete_SCF_archives_(self,keep_DM=TRUE)
            call destroy_SCF_results_(self,keep_MOs=TRUE)
            call destroy_SCF_integrals_(self)
            if (allocated(self%SCF_data)) then
               if (NOT self%SCF_data%SCF_kind=="mfcc") then

         ! Orthonormalize MO's at new geometry
         ! This makes new overlap integrals ...
                  call symmetrically_orthonormalize_(self,self%MOs)
               end if
            end if
         end if
      end if
   end subroutine

   subroutine get_derivative_F_calc_for_atom(self,a,dFc)
   ! Get "dFc", the derivatives of the complex F_calcs for the
   ! fragment atom corresponding to asymmetric unit atom "a".
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: a
      MAT(CPX,:,:), OUT :: dFc
      VEC(CPX,:), allocatable :: ff
      INT :: n_refl,n_p

      ! Complex structure factor derivatives
      n_refl = size(self%crystal%xray_data%reflections)
      n_p = no_of_pADPs_(self%crystal%asymmetric_unit_atom(a))

      ! Hirshfeld-atom structure factors
      ! NOTE: in iteration 0, these were already calculated
      ! as part of make_X_SFs ... double work!
      call get_Hirshfeld_atom_FFs_for_atom_(self,a,ff)

      ! Do a least squares
      call make_F_calc_derivs_for_atom_(self%crystal,a,dFc,ff)

      ! Clean up
      call destroy_(ff)
   end subroutine

   subroutine make_LS_mx(self,d_Fa)
   ! Calculate the LS matrix Low memory version.
      type(MOLECULE_TYPE), target, INOUT :: self
      ! INOUT, not OUT. The caller allocates this (LS_fit_HAs_disk does, with
      ! exactly the shape used below) and its sibling DO_LS_refinement, which
      ! takes the very same variable from the very same caller, is likewise
      ! declared MAT{REAL}@, INOUT. `OUT` was the slip, and on an ALLOCATABLE
      ! dummy it is not a documentation nicety: Fortran DEALLOCATES an
      ! intent(out) allocatable on entry, so the caller's allocation was thrown
      ! away, nothing here ever allocated it again, and it arrived unallocated
      ! at DIFFRACTION_DATA:d_F_abs_dX -- whose "res" dummy is MAT{REAL}, OUT,
      ! assumed-shape and NOT allocatable. Passing an unallocated allocatable
      ! to such a dummy is undefined; gfortran dereferenced a null array
      ! descriptor and every run that reached here died with SIGSEGV.
      MAT(REAL,:,:), allocatable, INOUT :: d_Fa
      VEC(STR(len=STR_SIZE),:), allocatable :: list
      VEC(STR(len=STR_SIZE),:), allocatable :: tags
      MAT(CPX,:,:), allocatable :: sf_d
      VEC(CPX,:), allocatable :: sf_u
      VEC(CPX,:), allocatable :: sf_e
      VEC(REAL,3) :: rc
      VEC(REAL,3) :: kv
      VEC(REAL,:), allocatable :: wt
      MAT(REAL,:,:), allocatable :: pt
      VEC(REAL,:), allocatable :: rho
      type(ARCHIVE_TYPE), allocatable :: arch
      INT :: n_unique,n_k
      INT :: n_p,n_p_atom
      INT :: c,u,f,base,k
      INT :: a,n_f,n_pt
      MAT(REAL,:,:), allocatable :: k_pts  ! Why?
      MAT(REAL,:,:), allocatable :: k_pt  ! Same?
      MAT(REAL,3,3) :: adp2
      MAT3(REAL,3,3,3) :: adp3
      MAT4(REAL,3,3,3,3) :: adp4
      MAT(CPX,:,:), allocatable :: dFc
      MAT(CPX,:,:), allocatable :: dFc_atom
      MAT(CPX,:,:), allocatable :: dFa_atom
      MAT(CPX,:,:), allocatable :: dFb_atom
      REAL :: rf,cutoff,sc,s2
      REAL :: k1,k2,k3,c2,c3,c4
      REAL :: k1_k1,k2_k2,k3_k3
      REAL :: k1_k2,k1_k3,k2_k3
      REAL :: zero3
      BIN :: spin,has_skip,has_Uiso,is_H_atom
      BIN :: refine_positions,refine_Uiso
      BIN :: refine_positions_only,refine_ADPs_only
      BIN :: refine_ADPs,refine_ADP4s,refine_ADP3s
      CPX :: II,sfh,sf1,sf2,sf4,sf6,sf24
      STR(len=STR_SIZE) :: tag
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.HAR:make_LS_mx ... no data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.HAR:make_LS_mx ... no fragment info")
   ENSURE(allocated(self%atom),"MOLECULE.HAR:make_LS_mx ... no atoms to fit")
   ENSURE(allocated(self%crystal),"MOLECULE.HAR:make_LS_mx ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.HAR:make_LS_mx ... no crystal xray data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.HAR:make_LS_mx ... no crystal fragment info")
   ENSURE(self%basis_info_made,"MOLECULE.HAR:make_LS_mx ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.HAR:make_LS_mx ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.HAR:make_LS_mx ... no density matrix")

      ! Constants
      II = IMAGIFY(ONE)
      n_p = no_of_pADPs_(self%crystal%asymmetric_unit_atom)
      n_f = no_of_pADPs_(self%crystal%fragment_atom)
      n_k = n_unique_SF_k_pts_(self%crystal)

      ! Hirshfeld-atom structure factors
      n_unique = self%crystal%n_unique_frag_atoms

      ! Make sure HA stuff is there
      call make_atom_partition_info_(self)

      ! Is there a nothing list?
      has_skip = allocated(self%crystal%xray_data%refine_nothing_for_atom)
      if (has_skip) then
         tags = self%crystal%fragment_atom%tag
         list = self%crystal%xray_data%refine_nothing_for_atom
         DIE_IF(NOT has_all_elements_common_with_(list,tags),"MOLECULE.HAR:make_LS_mx ... you try to refine nothing for an atom that is not in your list")
      end if

      ! Are there atoms to be refined isotropically?
      has_Uiso = allocated(self%crystal%xray_data%refine_U_iso_for_atom)
      if (has_Uiso) then
         tags = self%crystal%fragment_atom%tag
         list = self%crystal%xray_data%refine_U_iso_for_atom
         DIE_IF(NOT has_all_elements_common_with_(list,tags),"MOLECULE.HAR:make_LS_mx ... you try to refine an atom isotropically that is not in your list")
      end if
      refine_ADPs_only = self%crystal%xray_data%refine_ADPs_only
      refine_positions_only = self%crystal%xray_data%refine_positions_only

      ! Defaults per atom
      if (NOT refine_ADPs_only) then
         refine_positions = TRUE
      else
         refine_positions = FALSE
      end if
      if (NOT refine_positions_only) then
         refine_ADPs = TRUE
      else
         refine_ADPs = FALSE
      end if

      ! Final check to do U_iso this atom?
      refine_Uiso = FALSE
      if (NOT refine_Uiso) then
         if (has_Uiso) then
            if (any(self%crystal%xray_data%refine_U_iso_for_atom==tag)) then
               refine_Uiso = TRUE
            end if
         end if
      end if

      ! Skip derivatives for this atom?
      if (has_skip) then
         if (any(self%crystal%xray_data%refine_nothing_for_atom==tag)) then
            refine_positions = FALSE
            refine_ADPs = FALSE
         end if
      end if

      ! NOTE: the fit is initialised ONCE by LS_fit_HAs_disk before its loop,
      ! via CRYSTAL:initialize_fit_data -- exactly where CRYSTAL:LS_structure_fit
      ! does it. Calling it here meant once per LS iteration, re-creating
      ! X_fit / X_fit0 and resetting GoF2_fit0 to huge() every pass.
      !
      ! make_LS_mx now does one job: build the design matrix. That is the single
      ! thing which genuinely differs between the disk path and the in-core one.

      ! Allocate structure factor array (leaky)
      ! For pne atom
      call create_(dFc,size(self%crystal%xray_data%reflections),n_p)
      call create_(sf_e,n_k)  ! Symmetry generated SF's
      sf_e = ZERO
      do u = 1,n_unique
         refine_Uiso = FALSE
         refine_ADP4s = has_only_ADP4s_and_errors_(self%crystal%fragment_atom(u))
         refine_ADP3s = has_only_ADP3s_and_errors_(self%crystal%fragment_atom(u))

         ! Special cases for H atom
         is_H_atom = self%crystal%fragment_atom(u)%atomic_number==1
         if (is_H_atom) then
            refine_positions = self%crystal%xray_data%refine_H_positions
            refine_ADPs = self%crystal%xray_data%refine_H_ADPs
            refine_Uiso = self%crystal%xray_data%refine_H_U_iso
         end if
         call create_(sf_u,n_k)
         sf_u = ZERO

         ! Symmetry generated K points and SF's

         ! Make list of symmetry non-equivalent symops
         ! and the necessary k points
         call create_(k_pts,n_k,3)
         call make_unique_SF_k_pts_(self%crystal%xray_data%reflections,k_pts,self%crystal%spacegroup,self%crystal%unit_cell)

         ! X-ray or PND?
         spin = FALSE

         ! Density matrix (leaky)
         if (NOT spin) then; call make_r_density_mx_(self)
         else; call make_r_Sz_density_mx_(self)
         end if
         ENSURE(allocated(self%density_mx%r),"MOLECULE.HAR:make_LS_mx ... no restricted DM")

         ! Basis function cutoff
         cutoff = self%becke_grid%basis_fn_cutoff

         ! Max no of becke grid points per atom
         n_pt = max_no_of_pts_per_atom_(self%becke_grid)

         ! Loop over integration atoms "c"
         c = self%crystal%unique_frag_atom(u)
         rc = self%atom(c)%position

         ! Repetition factor for this atom, INCLUDING the site occupancy.
         !
         ! The occupancy was missing here, and this is the routine
         ! MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk warns about in its own
         ! comment -- "Also include the site occupancy factor (Dont forget this
         ! for the non-fragHAR routine)". It was forgotten. Every other route to
         ! an aspherical form factor applies it:
         !
         !   in core   MOLECULE.RHO:make_Hirshfeld_atom_FFs, :make_Salvador_-
         !             atom_FFs, :make_sph_TFVA_atom_FFs   -- sf*s2*occupancy
         !   on disk   MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk (fragHAR)
         !                                                  -- s2 = occupancy/sc
         !   recompute MOLECULE.RHO:get_Hirshfeld_atom_FFs_for_atom
         !                                                  -- sf*s2*occupancy
         !
         ! so the same structure refined with use_disk_SFs on and off gave
         ! different structure factors whenever an occupancy was not 1. Silent:
         ! for a fully occupied small molecule the factor is exactly 1 and
         ! nothing changes, which is every job in tests/. It is disordered
         ! structures and proteins -- per Dylan, "definitely not [one] for
         ! proteins" -- where it is wrong.
         !
         ! sf_u feeds sf_e and the derivatives sf_d as well as the <tag>-SFs
         ! file, so this corrects the refinement, not just the stored factors.
         !
         ! Indexed as .crystal.fragment_atom(c), matching get_Hirshfeld_atom_-
         ! FFs_disk, because the two write the SAME files and must agree.
         sc = self%crystal%frag_atom_sym_factor(c)
         if (NOT is_zero_(sc)) then
            s2 = self%crystal%fragment_atom(c)%site_occupancy/sc

            ! Get the Becke grid for atom "c" (leaky)
            call make_atom_grid_(self%becke_grid,pt,wt,c)
            n_pt = size(pt,1)

            ! Apply stockholder weight function "Wc" for atom "c"
            ! If using interpolatators, then on overlapping atoms
            ! are used ... otherwise nothing is skipped
            call apply_stockholder_atom_weight_(self,wt,c,pt)

            ! Prune the grid with new "wt"
            call prune_grid_(self%becke_grid,pt,wt)

            ! Density for atom "c"
            n_pt = size(pt,1)  ! <-- New n_pt !!!
            call create_(rho,n_pt)
            call make_ED_grid_r_v2_(self,rho,pt,self%density_mx%r,cutoff)

            ! Weight the atom "c" density
            rho = rho*wt

            ! Get aspherical form factor sf_u
            call make_atom_FFs_on_grid_(self,sf_u,k_pts,pt,rho,c)
            sf_u = sf_u*s2

         !  ! Write out sf_u
         !  arch.create(trim(.atom(c).tag)//"-SFs")
         !  if (.crystal.xray_data.use_text_SFs) arch.set_format("ascii")
         !  arch.per_rank_write(sf_u)
         !  arch.destroy

            ! Clean up atom "c" stuff
            call destroy_(rho)
            call destroy_(wt)
            call destroy_(pt)

            ! Clean up
            call destroy_(k_pts)
         end if

         ! NOTE: seeding X_fit from the atoms, copying X_ref and printing the
         ! fit header used to happen HERE -- inside this `do u` loop, so once
         ! per ATOM per LS iteration. X_fit is authoritative, so re-seeding it
         ! from the atoms mid-fit throws away the refinement. They are now done
         ! once in LS_fit_HAs_disk before the loop, where
         ! CRYSTAL:LS_structure_fit has them.

         ! Constants
         ! Fragment atom
         f = self%crystal%unique_frag_atom(u)
         a = self%crystal%asym_atom_for_frag_atom(f)  ! declare
         n_p_atom = no_of_pADPs_(self%crystal%asymmetric_unit_atom(a))

         ! Per u-atom Symmetry generated structure factors
         call create_(sf_d,n_k,n_p_atom)  ! Symmetry generated SF derivatives

         ! Aspherical temperature independent SFs
         ! sf_u = ZERO
         ! HERE

         ! Symmetry generated k points
         call create_(k_pt,n_k,3)
         call make_unique_SF_k_pts_(self%crystal%xray_data%reflections,k_pt,self%crystal%spacegroup,self%crystal%unit_cell)

         ! Refine only 4th's?
         zero3 = ONE
         if (self%crystal%xray_data%refine_4th_order_only) zero3 = ZERO

         ! Initialize
         sf_d = ZERO

         ! This atom's label
         tag = self%crystal%fragment_atom(f)%tag

         ! Read stored SFS !!!!!!!!!!!!
         ! arch.create(trim(tag)//"-SFs")
         ! if (.crystal.xray_data.use_text_SFs) arch.set_format("ascii")
         ! arch.read(sf_u)
         ! arch.destroy
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         ! May need to be initialised. Trying without atm
         ! DIE_IF(NOT adp2.equals(.crystal.fragment_atom(f).ADP_tensor),"ADP2 not equal to ADP_tensor")

         ! Get ADP2s
         call put_ADP2_vector_to_(self%crystal%fragment_atom(f),adp2)

         ! ADP4's !!!!!!!!!!!!!
         if (refine_ADP4s) then

         !!!!!!!!!!!!!!!!!!!!!!

            ! Get ADP tensors
            call put_ADP3_vector_to_(self%crystal%fragment_atom(f),adp3)
            call put_ADP4_vector_to_(self%crystal%fragment_atom(f),adp4)
            do k = 1,n_k

               ! Scattering vector
               k1 = k_pt(k,1)
               k2 = k_pt(k,2)
               k3 = k_pt(k,3)
               kv = [k1,k2,k3]

               ! Component products
               k1_k1 = k1*k1
               k2_k2 = k2*k2
               k3_k3 = k3*k3
               k1_k2 = k1*k2
               k1_k3 = k1*k3
               k2_k3 = k2*k3

               ! U tensor contracted with k vector
               c2 = contract_with_(adp2,kv)*HALF
               c3 = contract_with_(adp3,kv)*SIXTH
               c4 = contract_with_(adp4,kv)*SIXTH*QUARTER

               ! Static structure factor x TF
               ! i.e. dynamic structure factor,
               ! in the harmonic approximation
               sfh = sf_u(k)*exp(-c2)

               ! Multiples of *anharmonic* structure factor
               sf1 = sfh*(ONE-II*c3+c4)
               sf2 = sf1*HALF

               ! SF contribution (anharmonic!)
               sf_e(k) = sf_e(k)+sf1
               if (refine_positions) then
                  sf_d(k,1) = II*k1*sf1
                  sf_d(k,2) = II*k2*sf1
                  sf_d(k,3) = II*k3*sf1
               end if
               if (refine_ADPs) then

                  ! U2 derivatives
                  sf_d(k,4) = -k1_k1*sf2
                  sf_d(k,5) = -k2_k2*sf2
                  sf_d(k,6) = -k3_k3*sf2
                  sf_d(k,7) = -k1_k2*sf1
                  sf_d(k,8) = -k1_k3*sf1
                  sf_d(k,9) = -k2_k3*sf1

                  ! Structure factor (harmonic)
                  sf1 = sfh

                  ! Multiples of *harmonic* sf
                  sf2 = sf1*HALF
                  sf4 = sf1*QUARTER
                  sf6 = sf1*SIXTH
                  sf24 = sf1*SIXTH*QUARTER

                  ! U3 derivatives
                  sf_d(k,10) = -II*k1_k1*k1*sf6*zero3
                  sf_d(k,11) = -II*k2_k2*k2*sf6*zero3
                  sf_d(k,12) = -II*k3_k3*k3*sf6*zero3
                  sf_d(k,13) = -II*k1_k1*k2*sf2*zero3
                  sf_d(k,14) = -II*k1_k1*k3*sf2*zero3
                  sf_d(k,15) = -II*k2_k2*k1*sf2*zero3
                  sf_d(k,16) = -II*k2_k2*k3*sf2*zero3
                  sf_d(k,17) = -II*k3_k3*k1*sf2*zero3
                  sf_d(k,18) = -II*k3_k3*k2*sf2*zero3
                  sf_d(k,19) = -II*k1*k2*k3*sf1*zero3

                  ! U4 derivatives
                  sf_d(k,20) = k1_k1*k1_k1*sf24
                  sf_d(k,21) = k2_k2*k2_k2*sf24
                  sf_d(k,22) = k3_k3*k3_k3*sf24
                  sf_d(k,23) = k1_k1*k1_k2*sf6
                  sf_d(k,24) = k1_k1*k1_k3*sf6
                  sf_d(k,25) = k2_k2*k1_k2*sf6
                  sf_d(k,26) = k2_k2*k2_k3*sf6
                  sf_d(k,27) = k3_k3*k1_k3*sf6
                  sf_d(k,28) = k3_k3*k2_k3*sf6
                  sf_d(k,29) = k1_k1*k2_k2*sf4
                  sf_d(k,30) = k1_k1*k3_k3*sf4
                  sf_d(k,31) = k2_k2*k3_k3*sf4
                  sf_d(k,32) = k1_k1*k2_k3*sf2
                  sf_d(k,33) = k2_k2*k1_k3*sf2
                  sf_d(k,34) = k3_k3*k1_k2*sf2
               end if
            end do

         ! ADP3's !!!!!!!!!!!!!!!!!!
         else if (refine_ADP3s) then

         !!!!!!!!!!!!!!!!!!!!!!!!!!!

            ! Get ADP tensors
            call put_ADP3_vector_to_(self%crystal%fragment_atom(f),adp3)
            do k = 1,n_k

               ! Scattering vector
               k1 = k_pt(k,1)
               k2 = k_pt(k,2)
               k3 = k_pt(k,3)
               kv = [k1,k2,k3]

               ! Component products
               k1_k1 = k1*k1
               k2_k2 = k2*k2
               k3_k3 = k3*k3
               k1_k2 = k1*k2
               k1_k3 = k1*k3
               k2_k3 = k2*k3

               ! U tensor contracted with k vector
               c2 = contract_with_(adp2,kv)*HALF
               c3 = contract_with_(adp3,kv)*SIXTH

               ! Static structure factor x TF
               ! i.e. dynamic structure factor,
               ! in the harmonic approximation
               sfh = sf_u(k)*exp(-c2)

               ! Multiples of *anharmonic* structure factor
               ! -- 3rd order only.
               sf1 = sfh*(ONE-II*c3)
               sf2 = sf1*HALF

               ! SF contribution (anharmonic!)
               sf_e(k) = sf_e(k)+sf1

               !test refine_H_positions Lorraine
               if (refine_positions) then

                  ! Pos derivatives
                  sf_d(k,1) = II*k1*sf1
                  sf_d(k,2) = II*k2*sf1
                  sf_d(k,3) = II*k3*sf1
               end if
               if (refine_ADPs) then

                  ! U2 derivatives
                  sf_d(k,4) = -k1_k1*sf2
                  sf_d(k,5) = -k2_k2*sf2
                  sf_d(k,6) = -k3_k3*sf2
                  sf_d(k,7) = -k1_k2*sf1
                  sf_d(k,8) = -k1_k3*sf1
                  sf_d(k,9) = -k2_k3*sf1

                  ! Structure factor (harmonic!)
                  sf1 = sfh

                  ! Multiples of *harmonic* sf
                  sf2 = sf1*HALF
                  sf6 = sf1*SIXTH

                  ! U3 derivatives
                  sf_d(k,10) = -II*k1_k1*k1*sf6
                  sf_d(k,11) = -II*k2_k2*k2*sf6
                  sf_d(k,12) = -II*k3_k3*k3*sf6
                  sf_d(k,13) = -II*k1_k1*k2*sf2
                  sf_d(k,14) = -II*k1_k1*k3*sf2
                  sf_d(k,15) = -II*k2_k2*k1*sf2
                  sf_d(k,16) = -II*k2_k2*k3*sf2
                  sf_d(k,17) = -II*k3_k3*k1*sf2
                  sf_d(k,18) = -II*k3_k3*k2*sf2
                  sf_d(k,19) = -II*k1*k2*k3*sf1
               end if
            end do

         ! ADP2's !!!!!!!!!!!!!!
         else  ! NO anharmonicity

         !!!!!!!!!!!!!!!!!!!!!!!

            ! This is for isotropic refinement
            if (refine_Uiso) then  ! For U_iso 
               do k = 1,n_k

                  ! Scattering vector
                  k1 = k_pt(k,1)
                  k2 = k_pt(k,2)
                  k3 = k_pt(k,3)
                  kv = [k1,k2,k3]

                  ! Component products
                  k1_k1 = k1*k1
                  k2_k2 = k2*k2
                  k3_k3 = k3*k3
                  k1_k2 = k1*k2
                  k1_k3 = k1*k3
                  k2_k3 = k2*k3

                  ! U tensor contracted with k vector
                  c2 = contract_with_(adp2,kv)*HALF

                  ! Static structure factor x TF
                  ! i.e. dynamic structure factor,
                  ! in the harmonic approximation
                  sfh = sf_u(k)*exp(-c2)

                  ! Multiples *harmonic* structure factor
                  sf1 = sfh
                  sf2 = sf1*HALF*(k1_k1+k2_k2+k3_k3)

                  ! SF contribution (harmonic)
                  sf_e(k) = sf_e(k)+sf1
                  if (refine_positions) then

                     ! Pos derivatives
                     sf_d(k,1) = II*k1*sf1
                     sf_d(k,2) = II*k2*sf1
                     sf_d(k,3) = II*k3*sf1
                  end if
                  if (refine_ADPs) then

                     ! U_iso derivatives
                     sf_d(k,4) = -sf2
                     sf_d(k,5) = -sf2
                     sf_d(k,6) = -sf2
                     sf_d(k,7) = ZERO
                     sf_d(k,8) = ZERO
                     sf_d(k,9) = ZERO
                  end if
               end do

            ! This is an isotropic refinement
            else  ! For ADP2s !!!!!!!!!!!!!!!!!
               do k = 1,n_k

                  ! Scattering vector
                  k1 = k_pt(k,1)
                  k2 = k_pt(k,2)
                  k3 = k_pt(k,3)
                  kv = [k1,k2,k3]

                  ! Component products
                  k1_k1 = k1*k1
                  k2_k2 = k2*k2
                  k3_k3 = k3*k3
                  k1_k2 = k1*k2
                  k1_k3 = k1*k3
                  k2_k3 = k2*k3

                  ! U tensor contracted with k vector
                  c2 = contract_with_(adp2,kv)*HALF

                  ! Static structure factor x TF
                  ! i.e. dynamic structure factor,
                  ! in the harmonic approximation
                  sfh = sf_u(k)*exp(-c2)

                  ! Multiples *harmonic* structure factor
                  sf1 = sfh
                  sf2 = sf1*HALF

                  ! SF contribution (harmonic)
                  sf_e(k) = sf_e(k)+sf1
                  if (refine_positions) then

                     ! Pos derivatives
                     sf_d(k,1) = II*k1*sf1
                     sf_d(k,2) = II*k2*sf1
                     sf_d(k,3) = II*k3*sf1
                  end if
                  if (refine_ADPs) then

                     ! U2 derivatives
                     sf_d(k,4) = -k1_k1*sf2
                     sf_d(k,5) = -k2_k2*sf2
                     sf_d(k,6) = -k3_k3*sf2
                     sf_d(k,7) = -k1_k2*sf1
                     sf_d(k,8) = -k1_k3*sf1
                     sf_d(k,9) = -k2_k3*sf1
                  end if
               end do
            end if
         end if

         ! Clean
         call destroy_(k_pt)  ! KEEP ... Keep what? DJ

         ! WRITE SF_U TO DISK HERE TO SAVE TIME
         ! Write out sf_u
         !
         ! Plain `write`, NOT `per_rank_write`. The enclosing `do u` loop is
         ! SERIAL, so every rank reaches this with the same "u", hence the same
         ! atom "c", hence the same file name -- and per_rank_write is the
         ! ordinary write with the master-only guard REMOVED, so that was every
         ! rank writing one file concurrently, with no locking and no offsets.
         ! See FILE:per_rank_write for the precondition it violates.
         !
         ! Not a regression: this loop was never parallelised. The commit that
         ! introduced it (bfd30b95) followed two that say so outright -- "for
         ! FUTURE parallelism" (b2c53834) and "needs to be tested before
         ! parallelisation can begin" (15c2aee3). The per_rank_write came in
         ! with the loop body, copied from MOLECULE.RHO:get_Hirshfeld_atom_FFs_-
         ! disk, which does the same thing inside a `parallel do u` and is
         ! therefore correct -- there each rank owns distinct u, so distinct
         ! file names. The `parallel` keyword never arrived at this copy.
         !
         ! Master-only is right here, not `parallel do`: sf_e accumulates ACROSS
         ! u (see "sf_e(k) = sf_e(k) + sf1" below) and make_F_predicted_from is
         ! called INSIDE this loop, so parallelising needs a reduction and a
         ! restructure. Since the loop is serial every rank computes the same
         ! sf_u, so the master's file is complete and correct.
         !
         ! ARCHIVE:write is the safe path: FILE:open_for guards the open and
         ! then broadcasts .unit and .io_status, so the DIE_IF stays collective,
         ! and FILE:write/close are guarded with .record kept in step on every
         ! rank.
         call create_(arch,trim(self%atom(c)%tag)//"-SFs")
         if (self%crystal%xray_data%use_text_SFs) call set_format_(arch,"ascii")
         call write_(arch,sf_u)
         call destroy_(arch)
         call destroy_(sf_u)

         ! .crystal.make_unique_sf_derivs(sf_e,sf_d) ! processor 1 finishes here

         ! Make and set F_pred with scale & extinction.
         call reset_F_corr_to_F_exp_(self%crystal%xray_data%reflections)  ! We fit to F_corr
         call make_F_predicted_from_(self%crystal,sf_e)  ! ... and applies the dispersion convention

         ! Base index for unique frag atom
         base = no_of_pADPs_up_to_atom_(self%crystal%fragment_atom(self%crystal%unique_frag_atom),u)

         ! Sum the symmetry-generated SF's to get
         ! the derivative structure factors dFc
         call create_(dFc_atom,size(self%crystal%xray_data%reflections),n_p_atom)
         call sum_unique_sf_derivs_(self%crystal%spacegroup,dFc_atom,sf_d,self%crystal%xray_data%reflections)  ! Check dimensions in here
         call destroy_(sf_d)

         ! Add derivative of anomalous contribution to |SF|, dFc
         ! Only if F_calc has anomalous dispersion added into it.
         if (self%crystal%xray_data%add_dispersion_to_F_calc) then

            ! Size of symmetry generated k points
            ! WARNING: new k_pt array
            n_k = n_symop_generated_SF_k_pts_(self%crystal)
            call create_(k_pts,n_k,3)  ! NEW

            ! Size of anamalous Jacobian
            call create_(dFa_atom,n_k,n_p_atom)
            call create_(dFb_atom,size(self%crystal%xray_data%reflections),n_p)

            ! The full list of symmetry-generated k points
            call make_symop_generated_SF_k_pts_(self%crystal%xray_data%reflections,k_pts,self%crystal%spacegroup,self%crystal%unit_cell)

            ! Note: each atom contribution scales by a site-symmetry factor
            dFa_atom = ZERO
            rf = self%crystal%frag_atom_sym_factor(f)
            if (NOT is_zero_(rf)) then
               base = no_of_pADPs_up_to_atom_(self%crystal%fragment_atom(self%crystal%unique_frag_atom),u)
               call add_derivative_dispn_(self%crystal%fragment_atom(f),dFa_atom,base,k_pts,rf)  ! Get rid of base here
            end if

            ! Add together symmetry contributions
            dFb_atom = ZERO
            call add_full_sf_derivs_d_(self%crystal%spacegroup,dFb_atom,dFa_atom,self%crystal%xray_data%reflections)
            dFc_atom = dFc_atom+dFb_atom

            ! Clean
            call destroy_(dFb_atom)
            call destroy_(dFa_atom)
            call destroy_(k_pts)
         end if

         ! Copy & clean
         dFc(:,base+1:base+n_p_atom) = dFc_atom
         call destroy_(dFc_atom)
      end do
      call destroy_(sf_e)

      ! Get the derivative absolute SF's, F_abs = |F_calc|
      call d_F_abs_dX_(self%crystal%xray_data,d_Fa,dFc)
      call destroy_(dFc)
   end subroutine

   subroutine do_LS_refinement(self,d_Fa)
   ! Performs the least squares refinement
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable, INOUT :: d_Fa
      MAT(REAL,:,:), allocatable :: dF

      ! Get the unique atom shifts, dX (leaky)
      call solve_normal_equations_(self%crystal%xray_data,d_Fa)

      ! Change/store unique frag atom -> asym atom shifts
      ! Change/store the covariance matrix too.
      call set_asym_from_ufrag_shifts_(self%crystal)  ! .xray_data.dX now for asym atoms
      call set_asym_from_ufrag_cov_mx_(self%crystal)

      ! Stop symmetry breaking
      ! NOTE: we should apply this to the gradient *not*
      ! the shift to make sure the shift maintains symmetry
      call stabilize_asym_atom_shifts_(self%crystal)

      ! Store old asym-atom parameter vector.
      ! Add shift dA to get new .xray_data.X_fit
      call update_fit_parameters_(self%crystal%xray_data)
      call set_pADP_vector_to_(self%crystal%asymmetric_unit_atom,self%crystal%xray_data%X_fit)

      ! Set the fit errors e.g. max(dX/esd)
      ! Copy them to the asymmetric unit atoms.
      call update_fit_esds_(self%crystal)
      call set_pADP_errors_to_(self%crystal%asymmetric_unit_atom,self%crystal%xray_data%covariance_mx,self%crystal%xray_data%refine_H_U_iso)

      ! Set ALL fragment_atom positions/ADPs from asym atoms HERE
      ! >>>fragment_atom must be the same as molecule.atom
      call create_(dF,3,self%crystal%n_fragment_atoms)
      call set_frag_from_asym_pADPs_(self%crystal,dF)

      ! Update structure factors, the unique (rigid) frag atom SF's.
      ! Use the frag atom position shifts "dF".
      call shift_update_ff_(self%crystal,dF)
      call destroy_(dF)

      ! Close the chain. X_fit is authoritative and the atoms follow it:
      !
      !   X_fit  --set_pADP_vector_to-->  asymmetric_unit_atom
      !          --set_frag_from_asym_pADPs-->  crystal.fragment_atom
      !          --THIS LINE-->  .atom
      !
      ! and make_LS_mx builds the next design matrix from .atom -- its Becke
      ! grid and Fourier sum read .atom(c).position. Without this the moved
      ! geometry never reached the next iteration, so the design matrix was
      ! rebuilt from the ORIGINAL coordinates every pass.
      !
      ! The requirement was already written down two lines up -- ">>>
      ! fragment_atom must be the same as molecule.atom" -- but nothing did it.
      ! The in-core path gets away without it because CRYSTAL:LS_structure_fit
      ! computes its form factors once, before the fit, and MOLECULE.HAR:LS_fit
      ! does `.atom = .crystal.fragment_atom` after the whole thing. The disk
      ! path rebuilds every iteration, so it needs the refresh every iteration.
      self%atom = self%crystal%fragment_atom
   end subroutine


!  ==============
!  IAM refinement
!  ==============

   subroutine IAM_refinement(self)
   ! Do an IAM refinement. Actually this is just a fit.
   ! Cartesian (bohr) coordinates are used because structure factors
   ! are aspherical and calculated from the QM wavefunction.
   ! NOTE: this routine does only one cycle of fitting with the
   ! current density matrix i.e. this is a "rigid atom fit".
   ! NOTE: It's tricky exactly which atom's are used for refinement:
   ! * Structure factors are calculated only for *unique* fragment
   !   atoms since the QM wavefunction refers to the fragment_atoms.
   ! * However, asymmetric unit atoms are used to "store" the updating
   !   geometry; thus unique fragment atom pADPs and shifts are
   !   calculated (and derivatives with respect to the fragment atom
   !   positions and ADPs) but these parameters and shifts are later
   !   transformed back into asymmetric unit.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable :: ff
   ENSURE(allocated(self%atom),"MOLECULE.HAR:IAM_refinement ... no atoms to fit")
   ENSURE(allocated(self%crystal),"MOLECULE.HAR:IAM_refinement ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.HAR:IAM_refinement ... no crystal xray data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.HAR:IAM_refinement ... no crystal fragment info")

      ! Set so that called routines know
      call set_is_IAM_refinement_(self%crystal%xray_data,TRUE)

      ! Isotropic H's?
      if (self%crystal%xray_data%refine_H_U_iso) call set_isotropic_H_ADP_(self%atom)

      ! Do we need atomic density matrices
      if (NOT self%crystal%xray_data%use_IAM_ITC_FFs) then
         call make_ANOs_and_interpolators_(self)
         call set_use_IAM_ITC_FFs_(self%atom,FALSE)
      end if

      ! Get atom SF's. May IAM gaussian FFs.
      call make_unique_IAM_atom_FFs_(self,ff)

      ! Do a least squares
      call LS_fit_(self,ff,self%crystal%xray_data%show_IAM_output,self%crystal%xray_data%show_IAM_results)

      ! Clean up
      call destroy_(ff)

      ! Correlations
      call put_correlations_(self%crystal,name=trim(self%name)//".correlation-coefficients")

      ! Put residuals ...
      if (self%crystal%xray_data%do_residual_cube) then
         call get_minmax_residual_density_(self,output=TRUE)
      end if

      ! Put final CIF after residuals
      call put_CIFs_(self)

      ! Do standatd plots and properties
      if (self%crystal%xray_data%do_standard_plots) then
      end if
   end subroutine


!  ================================================
!  Least-squares refinement with given form factors
!  ================================================

   subroutine LS_fit_0(self,ff,output,results)
   ! Do a least squares structure refinement with a given set of
   ! static aspherical form factors "ff". Note the dimensions.
   ! The structure factors "ff" update when the geometry shifts.
   ! If "output"  is false, do not print any LS fit output.
   ! If "results" is false, do not print any LS fit results.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), INOUT :: ff
      BIN, IN :: output
      BIN, IN :: results
   ENSURE(allocated(self%atom),"MOLECULE.HAR:LS_fit_0 ... no atoms to fit")
   ENSURE(allocated(self%crystal),"MOLECULE.HAR:LS_fit_0 ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.HAR:LS_fit_0 ... no crystal xray data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.HAR:LS_fit_0 ... no crystal fragment info")
   ENSURE(size(ff,1)==n_unique_SF_k_pts_(self%crystal),"MOLECULE.HAR:LS_fit_0 ... ff: wrong dim1")
   ENSURE(size(ff,2)==self%crystal%n_asymmetric_unit_atoms,"MOLECULE.HAR:LS_fit_0 ... ff: wrong dim2")

      ! Do the fit 
      call LS_structure_fit_(self%crystal,ff,output,results)

      ! Set geometry type
      if (NOT self%crystal%xray_data%partition_model=="oc-crystal23") then

         ! NOTE: For pHAR, .atom is xml_atom and 
         ! .crystal.fragment_atom is for geometry analysis
         self%atom = self%crystal%fragment_atom
         self%crystal%asymmetric_unit_source = "tonto-refinement"
      end if
   end subroutine


   ! Split? Simplifify logic & presentation?

   subroutine LS_fit_1(self,output,results)
   ! Do a least squares structure refinement with a given set of
   ! static aspherical form factors which are on disk.
   ! If "output"  is FALSE, do not print any LS fit output.
   ! If "results" is FALSE, do not print any LS fit results.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, IN :: output
      BIN, IN :: results
   ENSURE(allocated(self%atom),"MOLECULE.HAR:LS_fit_1 ... no atoms to fit")
   ENSURE(allocated(self%crystal),"MOLECULE.HAR:LS_fit_1 ... no crystal")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.HAR:LS_fit_1 ... no crystal fragment info")

      ! Do the fit 
      call LS_structure_fit_(self%crystal,output,results)

      ! Set geometry type
      self%atom = self%crystal%fragment_atom
      self%crystal%asymmetric_unit_source = "tonto-refinement"
   end subroutine


!  ===============
!  Invariom labels
!  ===============

   subroutine put_SMILES_invariom_labels(self)
   ! Analyze the SCF energy into MO contributions
      type(MOLECULE_TYPE), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      VEC(type(EVEC_INT_TYPE),:), allocatable :: invariom_kind
      INT :: n_kind,n_elem,n_dash,k,l
   ENSURE(allocated(self%atom),"MOLECULE.HAR:put_SMILES_invariom_labels ... no atoms")

      ! Get the invariom labels
      labels = smiles_invariom_labels_(self%atom)

      ! Put labels out
      call flush_(stdout)
      call text_(stdout,"SMILES invariom labels:")
      call put_(stdout,labels,left=TRUE)

      ! Make the invariom kind table
      call repetitions_in_(invariom_kind,labels)

      ! Maximum no. of elements
      n_kind = size(invariom_kind)
      n_elem = 0
      do k = 1,n_kind
         n_elem = max(n_elem,size(invariom_kind(k)%element))
      end do

      ! Put invariom kind table out
      call flush_(stdout)
      call show_(stdout,"No. of distinct labels =",size(invariom_kind))
      call flush_(stdout)
      call text_(stdout,"Table of invariom kinds")
      call flush_(stdout)
      n_dash = min(10,n_elem)
      call dash_(stdout,width=5*(n_dash+1))
      call put_(stdout,"Kind",width=5)
      call put_(stdout," #'s",width=5)
      call flush_(stdout)
      call dash_(stdout,width=5*(n_dash+1))
      do k = 1,n_kind
         call put_(stdout,k,width=5)
         n_elem = size(invariom_kind(k)%element)
         call put_(stdout,invariom_kind(k)%element(1),width=5)
         l = 1
         do
            l = l+1
            if (l>n_elem) exit
            if (mod(l,10)==1) then
               call flush_(stdout)
               call tab_(stdout,width=5)
            end if
            call put_(stdout,invariom_kind(k)%element(l),width=5)
         end do
         call flush_(stdout)
      end do
      call dash_(stdout,width=5*(n_dash+1))

      ! Cleanup
      call destroy_(invariom_kind)
      call destroy_(labels)
   end subroutine

   subroutine put_invariom_labels(self)
   ! Analyze the SCF energy into MO contributions
      type(MOLECULE_TYPE), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      VEC(type(EVEC_INT_TYPE),:), allocatable :: invariom_kind
      INT :: n_kind,n_elem,n_dash,k,l
   ENSURE(allocated(self%atom),"MOLECULE.HAR:put_invariom_labels ... no atoms")

      ! Get the invariom labels
      labels = invariom_labels_(self%atom)

      ! Put labels out
      call flush_(stdout)
      call text_(stdout,"Atom names and their invariom labels:")
      do k = 1,size(labels)
         call put_(stdout,k,width=5)
         call put_(stdout,self%atom(k)%label,width=10,left=TRUE)
         call put_(stdout,labels(k),width=25,left=TRUE)
         call flush_(stdout)
      end do
      call flush_(stdout)

      ! Make the invariom kind table
      call repetitions_in_(invariom_kind,labels)

      ! Maximum no. of elements
      n_kind = size(invariom_kind)
      n_elem = 0
      do k = 1,n_kind
         n_elem = max(n_elem,size(invariom_kind(k)%element))
      end do

      ! Put invariom kind table out
      call flush_(stdout)
      call show_(stdout,"No. of distinct labels =",size(invariom_kind))
      call flush_(stdout)
      call text_(stdout,"Table of invariom kinds")
      call flush_(stdout)
      n_dash = min(10,n_elem)
      call dash_(stdout,width=5*(n_dash+1))
      call put_(stdout,"Kind",width=5)
      call put_(stdout," #'s",width=5)
      call flush_(stdout)
      call dash_(stdout,width=5*(n_dash+1))
      do k = 1,n_kind
         call put_(stdout,k,width=5)
         n_elem = size(invariom_kind(k)%element)
         call put_(stdout,invariom_kind(k)%element(1),width=5)
         l = 1
         do
            l = l+1
            if (l>n_elem) exit
            if (mod(l,10)==1) then
               call flush_(stdout)
               call tab_(stdout,width=5)
            end if
            call put_(stdout,invariom_kind(k)%element(l),width=5)
         end do
         call flush_(stdout)
      end do
      call dash_(stdout,width=5*(n_dash+1))

      ! Cleanup
      call destroy_(invariom_kind)
      call destroy_(labels)
   end subroutine

   subroutine put_invariom_label(self)
   ! Analyze the SCF energy into MO contributions
      type(MOLECULE_TYPE), IN :: self
      STR(len=STR_SIZE) :: label
      INT :: i
   ENSURE(allocated(self%atom),"MOLECULE.HAR:put_invariom_label ... no atoms")
      call read_(stdin,i)
      label = invariom_label_of_(self%atom,i)
      call flush_(stdout)
      call show_(stdout,"Invariom label for atom ="//trim(to_str_(i))//" = ",label)
   end subroutine

   subroutine put_next_neighbour_handedness(self)
   ! Put the next neighbour handedness out, for the atom with the
   ! index specified in the input.
      type(MOLECULE_TYPE), IN :: self
      STR(len=STR_SIZE) :: hand
      INT :: i
   ENSURE(allocated(self%atom),"MOLECULE.HAR:put_next_neighbour_handedness ... no atoms")
      call read_(stdin,i)
      hand = next_neighbour_handedness_of_(self%atom,i)
      call flush_(stdout)
      call show_(stdout,"Next-neighbour handedness of atom ="//trim(to_str_(i))//" = ",hand)
   end subroutine


!  ====================================
!  Spherically averaged Hirshfeld atoms
!  ====================================

   subroutine make_iterative_HAs(self)
   ! Make the (iteratively) Hirshfeld atoms a la Wheatly.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: max_iterations
   ENSURE(allocated(self%atom),"MOLECULE.HAR:make_iterative_HAs ... no atoms")
   ENSURE(self%atom_info_made,"MOLECULE.HAR:make_iterative_HAs ... no atom info")
   ENSURE(self%n_atom_kind>0,"MOLECULE.HAR:make_iterative_HAs ... no atom kinds")

      ! Do it
      max_iterations = 250
      call make_SAHA_(self,max_iterations)
   end subroutine

   subroutine make_spherically_averaged_HAs(self)
   ! Make the (iteratively) spherically averaged Hirshfeld atom (SAHA)
   ! density interpolators. You may specify the maximum iterations on
   ! the input line if you want; if not, it is assumed to be 1.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: max_iterations
   ENSURE(allocated(self%atom),"MOLECULE.HAR:make_spherically_averaged_HAs ... no atoms")
   ENSURE(self%atom_info_made,"MOLECULE.HAR:make_spherically_averaged_HAs ... no atom info")
   ENSURE(self%n_atom_kind>0,"MOLECULE.HAR:make_spherically_averaged_HAs ... no atom kinds")

      ! Read max # if iterations
      max_iterations = 1
      if (n_line_items_(stdin)>1) call read_(stdin,max_iterations)

      ! Do it
      call make_SAHA_(self,max_iterations)
   end subroutine

   subroutine make_SAHA(self,max_iterations)
   ! Make the (iteratively) spherically averaged Hirshfeld atom (SAHA)
   ! density interpolators, for every atom. NOTE: these are supposed

   ! Wheatley, Chem. Commun., 2008, 5909–5911.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: max_iterations
      VEC(type(INTERPOLATOR_TYPE),:), allocatable :: interpolator
      VEC(type(EVEC_REAL_TYPE),:), allocatable :: rho_av
      REAL :: max_eps,eps,eps_a
      INT :: max_l,a,iteration
   ENSURE(allocated(self%atom),"MOLECULE.HAR:make_SAHA ... no atoms")
   ENSURE(self%atom_info_made,"MOLECULE.HAR:make_SAHA ... no atom info")
   ENSURE(self%n_atom_kind>0,"MOLECULE.HAR:make_SAHA ... no atom kinds")

      ! Parameters defining the SAHA's
      max_l = 18  ! L-value for for spherical averaging.
      max_eps = TOL(6)  ! Maximum absolute difference for convergence

      ! Switch interpolators on
      if (NOT self%use_interpolators) then
         WARN("MOLECULE.HAR:make_SAHA ... You must be using interpolators!")
         self%use_interpolators = TRUE
      end if

      ! Set default template for atom interpolators
      if (NOT allocated(self%interpolator)) then
         call create_(self%interpolator)
         call set_interpolation_method_(self%interpolator,"linear")
         call set_range_mapping_(self%interpolator,"none")
         call set_domain_mapping_(self%interpolator,"sqrt(x/(1-x))*scale")
         call set_table_length_(self%interpolator,ONE)
         call set_spacing_(self%interpolator,0.01d0)
         call set_table_eps_(self%interpolator,0.001d0)
      end if

      ! Reset ANO interpolators
      call destroy_interpolators_(self)
      call make_atom_partition_info_(self)

      ! Copy ANO interpolators (which become IHA's)
      call create_(interpolator,self%n_atom)
      do a = 1,self%n_atom
         interpolator(a) = self%atom(a)%interpolator
      end do

      ! Reassign fresh (non-ANO) interpolator copy;
      ! Create averaged rho; set extrapolator.
      call destroy_ANO_matrices_(self)
      call create_(rho_av,self%n_atom)
      do a = 1,self%n_atom
         self%atom(a)%interpolator = interpolator(a)
         call create_(rho_av(a)%element,interpolator(a)%n_data)
      end do
      call set_is_unique_kind_(self%atom,FALSE)

      ! Banner
      call flush_(stdout)
      call text_(stdout,"=========================================")
      call text_(stdout,"Make spherically averaged Hirshfeld atoms")
      call text_(stdout,"=========================================")
      call flush_(stdout)
      if (max_iterations==1) then
         call text_(stdout,"This will make a single-iteration average")
      else
         call text_(stdout,"This may make an iteratively spherical averaged")
         call text_(stdout,"Hirshfeld atom (IHA) or ISA as described by these :")
         call text_(stdout,"Lillestolen & Wheatley, Chem. Commun., 2008, 5909–5911.")
         call text_(stdout,"but only if the precedure converges!")
         call flush_(stdout)
         call show_(stdout,"Max. iterations             =",max_iterations)
      end if
      call show_(stdout,"Convergence on density      =",max_eps)
      call show_(stdout,"L max (for spherical avg.)  =",max_l)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"Iter",int_width=TRUE)
      call put_(stdout,"Max change")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)

      ! Begin iterative cycle to get the IHA
      iteration = 0
      do
         iteration = iteration+1

         ! Spherically average & extrapolate, for each atom
         eps = ZERO
         do a = 1,self%n_atom
            call make_spherically_averaged_HA_(self,rho_av(a)%element,eps_a,a,interpolator(a),max_l)
            eps = max(eps,eps_a)
         end do

         ! Update convergence
         call put_(stdout,iteration)
         call put_(stdout,eps)
         call flush_(stdout)

         ! Transfer averaged values to atoms and interpolators
         do a = 1,self%n_atom
            call set_data_values_(interpolator(a),rho_av(a)%element)
            call finalize_(interpolator(a))
         end do

         ! Spherically avereaged Hirshfeld atoms obtained?
         if (eps<max_eps) exit
         if (iteration>max_iterations) exit
      end do

      ! Footer
      call dash_(stdout,int_fields=1,real_fields=1)

      ! Clean up
      call destroy_(rho_av)
   end subroutine

   subroutine make_spherically_averaged_HA(self,rho_av,eps,a,interpolator,l_max)
   ! Make the spherically averaged Hirshfeld atom density "rho_av" for
   ! the atom with index "a". Also return "eps", the maximum
   ! difference between the rho_av values and those stored in the
   ! interpolator data_values. The spacing and radial data values from
   ! the center of atom "a" are taken from the "interpolator". "l_max"
   ! is the maximum l value for the lebedev angular integration grid
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: rho_av
      REAL, OUT :: eps
      INT, IN :: a,l_max
      type(INTERPOLATOR_TYPE) :: interpolator
      MAT(REAL,:,:), allocatable :: pt
      VEC(REAL,:), allocatable :: rho,Wa
      VEC(REAL,3) :: pos
      REAL :: old,r
      INT :: n_pts,n,i
      type(LEBEDEV_TYPE), allocatable :: lebedev
   ENSURE(self%basis_info_made,"MOLECULE.HAR:make_spherically_averaged_HA ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.HAR:make_spherically_averaged_HA ... no atom list")
   ENSURE(has_all_interpolators_(self%atom),"MOLECULE.HAR:make_spherically_averaged_HA ... No interpolators")
   ENSURE(size(rho_av)==interpolator%n_data,"MOLECULE.HAR:make_spherically_averaged_HA ... wrong size, rho_av")
   ENSURE(is_in_range_(a,[1,self%n_atom]),"MOLECULE.HAR:make_spherically_averaged_HA ... a out of range")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.HAR:make_spherically_averaged_HA ... no overlapping_atoms")

      ! Set the spherical grid
      call create_(lebedev)
      call set_l_(lebedev,l_max)

      ! No of quadrature points (on sphere)
      n_pts = lebedev%n_pts

      ! Weight function, molecular density, spherical points
      ! and weights
      call create_(Wa,n_pts)
      call create_(rho,n_pts)
      call create_(pt,n_pts,3)

      ! Atom position
      pos = self%atom(a)%position

      ! Maximum relative difference
      eps = ZERO

   ! Debug
   ! stdout.show("atom =",a)
   ! interpolator.put

   ! stdout.put("r")
   ! stdout.put("rho_av")
   ! stdout.put("old")
   ! stdout.put("%")
   ! stdout.flush

      ! Loop over radial values
      do i = 1,interpolator%n_data

         ! Table value r
         r = interpolator%data_point(i)

         ! Interpolator data valie
         old = interpolator%data_value(i)

         ! Convert to actual r?
         if (interpolator%domain_is_sqrt_x1x) r = sqrt(r/(1-r))*interpolator%domain_scale_factor

         ! For each r, make a spherical shell of points
         do n = 1,n_pts
            pt(n,1) = pos(1)+r*lebedev%pts(n,1)
            pt(n,2) = pos(2)+r*lebedev%pts(n,2)
            pt(n,3) = pos(3)+r*lebedev%pts(n,3)
         end do

         ! Weight function for atom a on the spherical shell
         call make_stockholder_atom_weight_(self,Wa,a,pt,self%overlapping_atoms_for_atom(a)%element)

         ! Molecular density on the spherical shell
         call make_ED_grid_at_pts_(self,rho,pt)

         ! Weighted Hirshfeld atom density on shell
         Wa = Wa*rho*lebedev%wts

         ! Averaged value
         rho_av(i) = sum(Wa)

         ! Average percentage change
         eps = max(eps,abs((rho_av(i)-old)))
      end do

      ! Clean up
      call destroy_(pt)
      call destroy_(rho)
      call destroy_(Wa)
      call destroy_(lebedev)
   end subroutine


!  ====================
!  Bonded density atoms
!  ====================

   subroutine make_averaged_BDAs_0(self)
   ! Make the spherically averaged bond density atom
   ! interpolators for every atom.
      type(MOLECULE_TYPE), INOUT :: self
      type(CLUSTER_TYPE) :: cluster
      VEC(type(INTERPOLATOR_TYPE),:), allocatable :: interpolator
      VEC(type(EVEC_REAL_TYPE),:), allocatable :: rho_av
      REAL :: eps,eps_a
      INT :: a,l_max,iteration
   ENSURE(self%use_interpolators,"MOLECULE.HAR:make_averaged_BDAs_0 ... must be using interpolators")

      ! Set up the template for interpolators
      if (NOT allocated(self%interpolator)) then
         call create_(self%interpolator)
         call set_interpolation_method_(self%interpolator,"cubic_spline")
         call set_range_mapping_(self%interpolator,"none")
         call set_domain_mapping_(self%interpolator,"sqrt(x/(1-x))*scale")
         call set_table_length_(self%interpolator,ONE)
         call set_spacing_(self%interpolator,0.001d0)
         call set_table_eps_(self%interpolator,cluster%atom_density_cutoff)
      end if

      ! Set up grids, atomic densities, and NO's
      call make_atom_partition_info_(self)

      ! List of initial atom interpolators. Leaky
      call create_(interpolator,self%n_atom)
      do a = 1,self%n_atom
         interpolator(a) = self%atom(a)%interpolator
      end do

      ! Destroy atom ANO data and interpolators. Leaky.
      call destroy_ANO_matrices_(self)
      call destroy_interpolators_(self)

      ! The list of new atomic radial densities
      call create_(rho_av,self%n_atom)

      ! Reassign fresh interpolator copy; create averaged rho
      do a = 1,self%n_atom
         self%atom(a)%interpolator = interpolator(a)
         call create_(rho_av(a)%element,interpolator(a)%n_data)
      end do

      ! No of Gauss-Legendre integration points
      l_max = 35

      ! Banner
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"Iter",int_width=TRUE)
      call put_(stdout,"Max change")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)

      ! Begin iterative cycle
      iteration = 0
      do
         iteration = iteration+1
         call show_(stdout,"Iteration =",iteration)

         ! Spherically average each atom
         eps = ZERO
         do a = 1,self%n_atom
            call make_averaged_BDAs_(self,rho_av(a)%element,eps_a,a,interpolator(a),l_max)
            eps = max(eps,eps_a)
         end do

         ! Update convergence
         call put_(stdout,iteration)
         call put_(stdout,eps)
         call flush_(stdout)

         ! Transfer averaged values to atoms and interpolators
         do a = 1,self%n_atom
            call set_data_values_(interpolator(a),rho_av(a)%element)
            call finalize_(interpolator(a))
            self%atom(a)%interpolator = interpolator(a)
         end do

         ! Spherical atoms obtained
         if (eps<TOL(6)) exit
         if (iteration>=1) exit
      end do

      ! Footer
      call dash_(stdout,int_fields=1,real_fields=1)

      ! Copy atom interpolators. Leaky
      do a = 1,self%n_atom
         call destroy_(self%atom(a)%interpolator)
         self%atom(a)%interpolator = interpolator(a)
      end do

      ! Clean up
      call destroy_(rho_av)
      call destroy_(interpolator)
   end subroutine

   subroutine make_averaged_BDAs_1(self,rho_av,eps,a,interpolator,l_max)
   ! Make the spherically averaged Hirshfeld atom density "rho_av" for
   ! the atom with index "a". Also return "eps", the maximum
   ! difference between the rho_av values and those stored in the
   ! interpolator data_values. The spacing and radial data values from
   ! the center of atom "a" are taken from the "interpolator". "l_max"
   ! is the maximum l value for the lebedev angular integration grid
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: rho_av
      REAL, OUT :: eps
      INT, IN :: a,l_max
      type(INTERPOLATOR_TYPE) :: interpolator
      MAT(REAL,:,:), allocatable :: pt
      VEC(REAL,:), allocatable :: rho,Wa,del,pc
      VEC(REAL,3) :: pos
      REAL :: old,r
      INT :: n_pts,n,i
      type(LEBEDEV_TYPE), allocatable :: lebedev
   ENSURE(self%basis_info_made,"MOLECULE.HAR:make_averaged_BDAs_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.HAR:make_averaged_BDAs_1 ... no atom list")
   ENSURE(has_all_interpolators_(self%atom),"MOLECULE.HAR:make_averaged_BDAs_1 ... No interpolators")
   ENSURE(size(rho_av)==interpolator%n_data,"MOLECULE.HAR:make_averaged_BDAs_1 ... wrong size, rho_av")
   ENSURE(is_in_range_(a,[1,self%n_atom]),"MOLECULE.HAR:make_averaged_BDAs_1 ... a out of range")

      ! Set the spherical grid
      call create_(lebedev)
      call set_l_(lebedev,l_max)

      ! No of quadrature points (on sphere)
      n_pts = lebedev%n_pts

      ! Weight function, molecular density, spherical points
      ! and weights
      call create_(Wa,n_pts)
      call create_(rho,n_pts)
      call create_(pt,n_pts,3)
      call create_(del,interpolator%n_data)
      call create_(pc,interpolator%n_data)

      ! Atom position
      pos = self%atom(a)%position

      ! Maximum relative difference
      eps = ZERO
      call show_(stdout,"atom =",a)
      call put_(interpolator)

   ! stdout.put("r")
   ! stdout.put("rho_av")
   ! stdout.put("old")
   ! stdout.put("%")
   ! stdout.flush

      ! Loop over radial values
      do i = 1,interpolator%n_data

         ! Table value r
         r = interpolator%data_point(i)

         ! Interpolator data valie
         old = interpolator%data_value(i)

         ! Convert to actual r?
         if (interpolator%domain_is_sqrt_x1x) r = sqrt(r/(1-r))*interpolator%domain_scale_factor

         ! For each r, make a spherical shell of points
         do n = 1,n_pts
            pt(n,1) = pos(1)+r*lebedev%pts(n,1)
            pt(n,2) = pos(2)+r*lebedev%pts(n,2)
            pt(n,3) = pos(3)+r*lebedev%pts(n,3)
         end do

         ! Weight function for atom a on the spherical shell
         call make_bond_density_atom_grid_(self,Wa,pt,a)

         ! Weighted Hirshfeld atom density on shell
         Wa = Wa*lebedev%wts

         ! Averaged value
         rho_av(i) = sum_elements_(Wa)

         ! Average percentage change
         eps = max(eps,abs((rho_av(i)-old)))
         del(i) = rho_av(i)-old
         pc(i) = del(i)/old
      end do
      call text_(stdout,"del:")
      call put_(stdout,del)
      call text_(stdout,"pc:")
      call put_(stdout,pc)

      ! Clean up
      call destroy_(pc); call destroy_(del)
      call destroy_(pt); call destroy_(rho); call destroy_(Wa)
      call destroy_(lebedev)
   end subroutine

   subroutine put_BDA_charges(self)
   ! Put the BDA charges.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable :: bond
      VEC(type(EVEC_REAL_TYPE),:), allocatable :: bond_charge
      STR(len=STR_SIZE) :: label
      INT :: a,b,c
      BIN :: has_bond
   ENSURE(self%basis_info_made,"MOLECULE.HAR:put_BDA_charges ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.HAR:put_BDA_charges ... no atom list")

      ! Make the charges
      call make_BDA_charges_(self,bond,bond_charge)

      ! Put the table
      call dash_(stdout,real_fields=3)
      call tab_(stdout,real_fields=1)
      call put_(stdout,"Bonded")
      call put_(stdout,"Bond")
      call flush_(stdout)
      call put_(stdout,"Atom")
      call put_(stdout,"Atom")
      call put_(stdout,"Charge")
      call flush_(stdout)
      call dash_(stdout,real_fields=3)
      do a = 1,self%n_atom
         label = trim(self%atom(a)%label)//"("//trim(to_str_(a))//")"
         call put_(stdout,label)
         has_bond = allocated(bond(a)%element)
         if (has_bond) has_bond = size(bond(a)%element)>0
         if (NOT has_bond) then
            call flush_(stdout)
         else
            do b = 1,size(bond(a)%element)
               if (b>1) call tab_(stdout,real_fields=1)
               c = bond(a)%element(b)
               label = trim(self%atom(c)%label)//"("//trim(to_str_(c))//")"
               call put_(stdout,label)
               call put_(stdout,-bond_charge(a)%element(b))
               call flush_(stdout)
            end do
            call tab_(stdout,real_fields=2)
            call put_(stdout,-sum_elements_(bond_charge(a)%element))
            call flush_(stdout)
         end if
      end do
      call dash_(stdout,real_fields=3)

      ! Clean up
      call destroy_(bond)
      call destroy_(bond_charge)
   end subroutine

   subroutine make_BDA_charges(self,bond,bond_charge)
   ! make the bond density atom charges.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable, OUT :: bond
      VEC(type(EVEC_REAL_TYPE),:), allocatable, OUT :: bond_charge
      MAT(REAL,:,:), allocatable :: pt
      VEC(REAL,:), allocatable :: wt,rho_aba,def_density_grid
      VEC(INT,:), allocatable :: bond_density_atom
      INT :: n_pt,a,b
      REAL :: val
   ENSURE(self%basis_info_made,"MOLECULE.HAR:make_BDA_charges ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.HAR:make_BDA_charges ... no atom list")

      ! Set up grids, atomic densities, and NO's
      call make_atom_partition_info_(self)

      ! Array for bonds and bond charges
      call create_(bond,self%n_atom)
      call create_(bond_charge,self%n_atom)

      ! Loop over atoms, a
      do a = 1,self%n_atom

         ! Make becke grid
         call make_grid_(self%becke_grid,pt,wt,[a])
         n_pt = size(pt,1)

         ! Deformation density on grid
         call create_(def_density_grid,n_pt)
         call make_def_ED_grid_(self,def_density_grid,pt)

         ! Bond density on grid
         call create_(rho_aba,n_pt)

         ! bond density atom
         call create_(bond_density_atom,3)
         bond_density_atom(1) = a
         bond_density_atom(3) = a

         ! Loop over atoms b, bonded to a
         do b = 1,self%n_atom
            if (a==b) cycle
            if (NOT bonded_(self%atom,a,b)) cycle

            ! Get the bond density
            bond_density_atom(2) = b
            call make_def_bond_density_grid_(self,rho_aba,pt,bond_density_atom,def_density_grid)

            ! Get weighted integral
            rho_aba = rho_aba*wt

            ! Get charge
            val = sum_elements_(rho_aba)

            ! Store charge
            call append_(bond(a)%element,b)
            call append_(bond_charge(a)%element,val)
         end do

         ! Clean up
         call destroy_(bond_density_atom)
         call destroy_(rho_aba)
         call destroy_(def_density_grid)
         call destroy_(pt)
         call destroy_(wt)
      end do
   end subroutine


!  =======================================
!  Scaled and derivative stockholder grids
!  =======================================

   subroutine make_scaled_stockholder_derivs(self,G,H,kappa,delta,skip_ANOs)
   ! Make the scaled stockholder atom derivatives "G" and hessian "H"
   ! for used in a Newton-Raphson procedure to find the best scale
   ! factors.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: G
      MAT(REAL,:,:), OUT :: H
      VEC(REAL,:), IN :: kappa
      REAL, OUT :: delta
      BIN, optional, IN :: skip_ANOs
      MAT3(REAL,:,:,:), allocatable :: Wa_cd
      MAT(REAL,:,:), allocatable :: pt,Wa_c
      VEC(REAL,:), allocatable :: wt,Wa,G_grid,K_grid,Ga
      REAL :: delta2
      INT :: a,c,d,n_pt
   ENSURE(self%basis_info_made,"MOLECULE.HAR:make_scaled_stockholder_derivs ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.HAR:make_scaled_stockholder_derivs ... no atom list")
   ENSURE(allocated(self%MOs),"MOLECULE.HAR:make_scaled_stockholder_derivs ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.HAR:make_scaled_stockholder_derivs ... no restricted MO's")
   ENSURE(allocated(self%becke_grid),"MOLECULE.HAR:make_scaled_stockholder_derivs ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.HAR:make_scaled_stockholder_derivs ... no finalized becke_grid")
   ENSURE(is_square_(H),"MOLECULE.HAR:make_scaled_stockholder_derivs ... H is not square")
   ENSURE(size(G)==size(H,1),"MOLECULE.HAR:make_scaled_stockholder_derivs ... G and H are inconsistent")

      ! Set up grids, spherical atomic densities, and NO's
      call make_HA_info_(self,skip_ANOs=skip_ANOs)  ! Must be HA

      ! Make the stockholder derivatives D and H
      delta = ZERO
      G = ZERO
      H = ZERO
      do a = 1,self%n_atom

         ! Make integration grids
         call make_grid_(self%becke_grid,pt,wt,[a])
         n_pt = size(pt,1)

         ! Make G and K kinetic densities
         call create_(G_grid,n_pt)
         call create_(K_grid,n_pt)
         call make_G_and_K_kinetic_grids_(self,G_grid,K_grid,pt)

         ! Make Hirshfeld atoms
         call create_(Wa,n_pt)
         call make_stockholder_atom_weight_kappa_(self,Wa,a,kappa,pt)

         ! Make delta kinetic grid
         G_grid = (G_grid-K_grid)*wt
         delta2 = TWO*sum_elements_(G_grid*Wa)
         delta = delta+QUARTER*delta2*delta2
         call destroy_(Wa)
         call destroy_(K_grid)

         ! Stockholder 1st derivatives
         call create_(Ga,self%n_atom)
         call create_(Wa_c,n_pt,self%n_atom)
         call make_d_stockholder_atom_grid_(self,Wa_c,a,kappa,pt)
         do c = 1,self%n_atom
            Ga(c) = sum_elements_(Wa_c(:,c)*G_grid)
         end do
         call destroy_(Wa_c)

         ! Add contribution from 1st derivatives
         G = G+delta2*Ga
         call plus_outer_product_of_(H,Ga,TWO)
         call destroy_(Ga)

         ! Stockholder 2nd derivatives
         call create_(Wa_cd,n_pt,self%n_atom,self%n_atom)
         call make_d2_stockholder_atom_grid_(self,Wa_cd,a,kappa,pt)

         ! Add contribution from 2nd derivatives
         G_grid = delta2*G_grid
         do c = 1,self%n_atom
            do d = 1,self%n_atom
               H(c,d) = H(c,d)+sum_elements_(Wa_cd(:,c,d)*G_grid)
            end do
         end do
         call destroy_(Wa_cd)

         ! Clean
         call destroy_(G_grid)
         call destroy_(wt)
         call destroy_(pt)
      end do
      delta = sqrt(delta)
   end subroutine

   subroutine make_stockholder_atom_weight_kappa(self,grid,a,kappa,pt)
   ! Make the scaled stockholder desity "grid" for the atom with index
   ! "a" and with scale parameters "kappa" using supplied points "pt".
   ! This uses gaussian atomic densities.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      INT, IN :: a
      VEC(REAL,:), IN :: kappa
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: rho_a
      INT :: n_pt,i
   ENSURE(allocated(self%atom),"MOLECULE.HAR:make_stockholder_atom_weight_kappa ... no atoms")
   ENSURE(has_interpolators_any_kind_(self%atom),"MOLECULE.HAR:make_stockholder_atom_weight_kappa ... missing atomic interpolators")
   ENSURE(size(grid)==size(pt,1),"MOLECULE.HAR:make_stockholder_atom_weight_kappa ... wrong size, grid")
   ENSURE(size(kappa)==self%n_atom,"MOLECULE.HAR:make_stockholder_atom_weight_kappa ... wrong size, kappa")

      ! Allocate temporary densities
      n_pt = size(grid)
      call create_(rho_a,n_pt)

      ! Make density for atom "a"
      call make_ED_grid_(self%atom(a),rho_a,pt)

      ! Make total density in "grid"
      grid = kappa(a)*rho_a
      do i = 1,self%n_atom
         if (i==a) cycle
         call add_ED_grid_(self%atom(i),grid,pt,kappa(i))
      end do

      ! Make the stockholder weight
      do i = 1,n_pt
         if (grid(i)>epsilon(ONE)) then; grid(i) = rho_a(i)/grid(i)
         else; grid(i) = ZERO
         end if
      end do
      call destroy_(rho_a)
   end subroutine

   subroutine make_d_stockholder_atom_grid(self,grid,a,kappa,pt)
   ! Make the derivative scaled stockholder desity "grid" for the atom
   ! with index "a" and with scale parameters "kappa" using supplied
   ! points "pt".  This uses gaussian atomic densities.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: grid
      INT, IN :: a
      VEC(REAL,:), IN :: kappa
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: rho_a,rho_c,rho
      INT :: n_pt,i,c
   ENSURE(allocated(self%atom),"MOLECULE.HAR:make_d_stockholder_atom_grid ... no atoms")
   ENSURE(has_interpolators_any_kind_(self%atom),"MOLECULE.HAR:make_d_stockholder_atom_grid ... missing atomic interpolators")
   ENSURE(size(grid,1)==size(pt,1),"MOLECULE.HAR:make_d_stockholder_atom_grid ... wrong dim1, grid")
   ENSURE(size(grid,2)==self%n_atom,"MOLECULE.HAR:make_d_stockholder_atom_grid ... wrong dim2, grid")
   ENSURE(size(kappa)==self%n_atom,"MOLECULE.HAR:make_d_stockholder_atom_grid ... wrong size, kappa")

      ! Allocate temporary densities
      n_pt = size(grid,1)
      call create_(rho_a,n_pt)
      call create_(rho_c,n_pt)
      call create_(rho,n_pt)

      ! Make density for atom "a"
      call make_ED_grid_(self%atom(a),rho_a,pt)

      ! Make total density in "rho" with kappa factors
      rho_a = kappa(a)*rho_a
      rho = rho_a
      do i = 1,self%n_atom
         if (i==a) cycle
         call add_ED_grid_(self%atom(i),rho,pt,kappa(i))
      end do
      where (rho>epsilon(ONE)); rho_a = rho_a/(rho*rho)
      elsewhere; rho_a = ZERO
      end where

      ! Make the derivative stockholder weight
      do c = 1,self%n_atom
         call make_ED_grid_(self%atom(c),rho_c,pt)
         grid(:,c) = -rho_a*rho_c
         if (c/=a) cycle
         where (rho>epsilon(ONE)) grid(:,c) = grid(:,c)+rho_c/rho
      end do

      ! Clean
      call destroy_(rho)
      call destroy_(rho_c); call destroy_(rho_a)
   end subroutine

   subroutine make_d2_stockholder_atom_grid(self,grid,a,kappa,pt)
   ! Make the 2nd derivative scaled stockholder desity "grid" for the
   ! atom with index "a" and with scale parameters "kappa" using
   ! supplied points "pt".  This uses gaussian atomic densities.
      type(MOLECULE_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: grid
      INT, IN :: a
      VEC(REAL,:), IN :: kappa
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: rho_a,rho_c,rho_d,rho
      INT :: n_pt,i,c,d
   ENSURE(allocated(self%atom),"MOLECULE.HAR:make_d2_stockholder_atom_grid ... no atoms")
   ENSURE(has_interpolators_any_kind_(self%atom),"MOLECULE.HAR:make_d2_stockholder_atom_grid ... missing atomic interpolators")
   ENSURE(size(grid,1)==size(pt,1),"MOLECULE.HAR:make_d2_stockholder_atom_grid ... wrong dim1, grid")
   ENSURE(size(grid,2)==self%n_atom,"MOLECULE.HAR:make_d2_stockholder_atom_grid ... wrong dim2, grid")
   ENSURE(size(grid,3)==self%n_atom,"MOLECULE.HAR:make_d2_stockholder_atom_grid ... wrong dim3, grid")
   ENSURE(size(kappa)==self%n_atom,"MOLECULE.HAR:make_d2_stockholder_atom_grid ... wrong size, kappa")

      ! Allocate temporary densities
      n_pt = size(grid,1)
      call create_(rho_a,n_pt)
      call create_(rho_c,n_pt)
      call create_(rho_d,n_pt)
      call create_(rho,n_pt)

      ! Make density for atom "a"
      call make_ED_grid_(self%atom(a),rho_a,pt)

      ! Make total density in "rho" with kappa factors
      rho_a = kappa(a)*rho_a
      rho = rho_a
      do i = 1,self%n_atom
         if (i==a) cycle
         call add_ED_grid_(self%atom(i),rho,pt,kappa(i))
      end do
      where (rho>epsilon(ONE)); rho_a = TWO*rho_a/(rho*rho*rho)
      elsewhere; rho_a = ZERO
      end where
      where (rho>epsilon(ONE)); rho = ONE/(rho*rho)
      elsewhere; rho = ZERO
      end where

      ! Make the derivative stockholder weight
      do c = 1,self%n_atom
         call make_ED_grid_(self%atom(c),rho_c,pt)
         do d = 1,self%n_atom
            call make_ED_grid_(self%atom(d),rho_d,pt)
            grid(:,c,d) = rho_a*rho_c*rho_d
            if (c==a) grid(:,c,d) = grid(:,c,d)-rho_d*rho
            if (d==a) grid(:,c,d) = grid(:,c,d)-rho_c*rho
         end do
      end do

      ! Clean
      call destroy_(rho)
      call destroy_(rho_d); call destroy_(rho_c); call destroy_(rho_a)
   end subroutine

end module
