!====================================================================
!
! DIFFRACTION_DATA: Diffraction data structure for crystals
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1999
! Copyright (C) Dylan Jayatilaka 2023
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
!====================================================================
module DIFFRACTION_DATA_SET_MODULE

#  include "macros"

   include "diffraction_data.set.use"

   implicit none

   include "diffraction_data.set.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(DIFFRACTION_DATA_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"DIFFRACTION_DATA.SET:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(DIFFRACTION_DATA_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"DIFFRACTION_DATA.SET:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Erase all pointer information
      type(DIFFRACTION_DATA_TYPE), INOUT :: self

      ! Reflection related data
      call destroy_reflection_data_(self)
      call destroy_(self%refine_U_iso_for_atom)
      ! Refinement/fit related data
      call destroy_(self%refine_3rd_order_for_atom)
      call destroy_(self%refine_4th_order_for_atom)
      call destroy_(self%refine_atom_parameters)
      call destroy_fit_data_(self)
      call destroy_refinement_data_(self)
   end subroutine

   PURE subroutine destroy_reflection_data(self)
   ! Erase all reflection data
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      if (reflection_data_exists_(self)) then
         self%scale_factor = ONE
         self%exp_scale_factor = -ONE
         self%extinction_factor = ZERO
         self%extinction_factor_esd = -ONE
         call destroy_(self%reflection0)
         call destroy_(self%reflections)
         call destroy_(self%hkl_range)
         call destroy_(self%scale_factors)
      end if
   end subroutine

   PURE subroutine destroy_fit_data(self)
   ! Erase all generated fit arrays
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call destroy_(self%fit_table)
      call destroy_(self%dX)
      call destroy_(self%X_fit)
      call destroy_(self%X_fit0)
      call destroy_(self%fragment_covariance_mx)
      call destroy_(self%correlation_mx)
      call destroy_(self%covariance_mx)
      call destroy_(self%esd)
      call destroy_(self%ls)
   end subroutine

   PURE subroutine destroy_refinement_data(self)
   ! Erase all generated refinement arrays
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call destroy_(self%ref_table)
      call destroy_(self%X_ref)
      call destroy_(self%X_ref0)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self)
   ! Set up a default diffraction_data object
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call destroy_reflection_data_(self)
      call destroy_fit_data_(self)
      call set_fit_data_(self)
      call destroy_refinement_data_(self)
      call set_refinement_data_(self)
   end subroutine

   pure subroutine set_fit_data(self)
   ! Set the defaults internal variables for a fit
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      type(DIFFRACTION_DATA_TYPE) :: std

      ! Refinement/fit iteration data
      self%fit_iteration = std%fit_iteration
      self%GoF2_fit = std%GoF2_fit
      self%GoF2_fit0 = std%GoF2_fit0
      self%max_shift = std%max_shift
      self%max_esd = std%max_esd
      self%max_shift_on_esd = std%max_shift_on_esd
      self%max_shift_on_esd_ind = std%max_shift_on_esd_ind
      self%max_shift_on_esd_par = std%max_shift_on_esd_par
      self%fit_finished = std%fit_finished
      self%fit_converged = std%fit_converged
      self%GoF2_increased = std%GoF2_increased
      self%too_many_fit_iterations = std%too_many_fit_iterations
   end subroutine

   pure subroutine set_refinement_data(self)
   ! Set the defaults internal variables for a refinement
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      type(DIFFRACTION_DATA_TYPE) :: std

      ! Refinement/fit iteration data
      self%ref_iteration = std%ref_iteration
      self%GoF2_ref = std%GoF2_ref
      self%GoF2_ref0 = std%GoF2_ref0
      self%max_refshift = std%max_refshift
      self%max_refesd = std%max_refesd
      self%max_refshift_on_esd = std%max_refshift_on_esd
      self%max_refshift_on_esd_ind = std%max_refshift_on_esd_ind
      self%max_refshift_on_esd_par = std%max_refshift_on_esd_par
      self%refinement_finished = std%refinement_finished
      self%refinement_converged = std%refinement_converged
      self%too_many_ref_iterations = std%too_many_ref_iterations
   end subroutine

   pure subroutine set_add_dispersion_to_F_calc(self,val)
   ! Set whether to correct dispersion in the refinement by adding the
   ! anomalous dispersion into F_calc and calculating the derivatives
   ! of the dispersion correction.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%add_dispersion_to_F_calc = val
   end subroutine

   PURE subroutine set_data_kind(self,val)
   ! Read the data kind of crystal experiment
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
   ENSURE(is_one_of_(val,["x-ray","pnd  "]),"DIFFRACTION_DATA.SET:set_data_kind ... unknown data_kind, "//trim(val))
      self%data_kind = val
      call to_lower_case_(self%data_kind)
   end subroutine

   subroutine set_fcf_file_name(self,val)
   ! Set the name of the fcf file from which to read in a unique set
   ! of reflections.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      type(TEXTFILE_TYPE) :: file
      self%fcf_file_name = val
      call set_name_(file,val)
      DIE_IF(NOT exists_(file),"DIFFRACTION_DATA.SET:set_fcf_file_name ... the specified fcf file does not exist")
   end subroutine

   pure subroutine set_fit_structure(self,val)
   ! Set whether to rigid-fit the structure
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%fit_structure = val

      ! If fitting, you can't be refining
      if (NOT val) self%refine_structure = NOT val
   end subroutine

   pure subroutine set_max_shift_on_esd(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%max_shift_on_esd = val
   end subroutine

   pure subroutine set_max_shift_on_esd_par(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val

      ! The following code is inherited from OBJECT
      self%max_shift_on_esd_par = val
   end subroutine

   pure subroutine set_max_refshift_on_esd(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%max_refshift_on_esd = val
   end subroutine

   pure subroutine set_max_refshift_on_esd_par(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val

      ! The following code is inherited from OBJECT
      self%max_refshift_on_esd_par = val
   end subroutine

   PURE subroutine set_n_data_sets(self,val)
   ! Set the number of data sets in a multiple-data-set refinement.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>1,"DIFFRACTION_DATA.SET:set_n_data_sets ... n_data_sets must be non negative")
      self%n_data_sets = val
   end subroutine

   pure subroutine set_n_param_structure(self,val)
   ! Set the number of refined structural (pADP) parameters
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%n_param_structure = val
   end subroutine

   PURE subroutine set_n_param(self,val)
   ! Set the number of paramters in the model "by hand"
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>1,"DIFFRACTION_DATA.SET:set_n_param ... n_param must be non negative")
      self%n_param = val
      self%n_param_manually_set = TRUE
   end subroutine

   pure subroutine set_n_worst_reflections(self,val)
   ! Set the no. of worst reflections to show
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT, IN :: val
      self%n_worst_reflections = val
   end subroutine

   PURE subroutine set_partition_model(self,val)
   ! Set the partition model used to calculate the structure factors.
   ! The "oc-" models are one-center, atom-based partitionings, and
   ! may be used for structure refinement. The "tc-" models are
   ! two-center, basis-function-based partitionings which use all parts
   ! of the fragment density matrix (with 'repetition factors'), and
   ! may not be used for structure refinement.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      VEC(STR(len=STR_SIZE),:), allocatable :: options
      options = ["none          ","oc-crystal23  ","crystal23     ","oc-g-hirshfeld","g-hirshfeld   ","oc-hirshfeld  ","hirshfeld     ","oc-g-salvador ","g-salvador    ","oc-salvador   ","salvador      ","tfva          ","oc-sph-tfva   ","sph-tfva      ","oc-tfvp       ","tfvp          ","oc-tfvh       ","tfvh          ","oc-sph-tfvp   ","sph-tfvp      ","oc-sph-tfvh   ","sph-tfvh      ","oc-exphar     ","exphar        ","oc-sph-exphar ","sph-exphar    ","oc-ri         ","ri            ","tc-coppens    ","tc-stewart    ","tc-tanaka     "]
      self%partition_model = val
      call to_lower_case_(self%partition_model)
      STOP_IF(NOT is_one_of_(self%partition_model,options),"DIFFRACTION_DATA.SET:set_partition_model ... unknown partition_model")

      ! Rename to standard form
      select case (self%partition_model)
      case ("crystal23  "); self%partition_model = "oc-crystal23"
      case ("g-hirshfeld"); self%partition_model = "oc-g-hirshfeld"
      case ("hirshfeld  "); self%partition_model = "oc-hirshfeld"
      case ("g-salvador "); self%partition_model = "oc-g-salvador"
      case ("salvador   "); self%partition_model = "oc-salvador"
      case ("tfva       "); self%partition_model = "oc-salvador"
      case ("sph-tfva   "); self%partition_model = "oc-sph-tfva"
      case ("tfvp       "); self%partition_model = "oc-tfvp"
      case ("tfvh       "); self%partition_model = "oc-tfvh"
      case ("sph-tfvp   "); self%partition_model = "oc-sph-tfvp"
      case ("sph-tfvh   "); self%partition_model = "oc-sph-tfvh"
      case ("exphar     "); self%partition_model = "oc-exphar"
      case ("sph-exphar "); self%partition_model = "oc-sph-exphar"
      case ("ri         "); self%partition_model = "oc-ri"
      end select

      ! One-center partitions go with one-center TF models
      ! NOTE: for crystal23 density matrices it is assuned you are 
      ! doing an atom-based i.e. OC model

    !    .set_temperature_factor_model("atom-based")
   end subroutine

   pure subroutine set_max_iterations(self,val)
   ! Set the maximum number of iterations, for BOTH loops -- it caps
   ! .too_many_fit_iterations and .too_many_ref_iterations alike.
   !
   ! There was a `max_iterations=` keyword but no setter, so an argv-driven
   ! program could not bound a run at all. Callers must validate: the keyword
   ! reader's `max_iterations > min_iterations` check is an ENSURE and so
   ! compiles away in an optimised build.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%max_iterations = val
   end subroutine

   pure subroutine set_do_residual_cube(self,val)
   ! Set whether to write a residual-density cube at the end of a refinement.
   ! There was a `do_residual_cube=` keyword but no setter, so a program driven
   ! by argv rather than a job file had no way to turn it off -- and the cube is
   ! the largest thing a refinement writes (9 MB for a dipeptide).
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%do_residual_cube = val
   end subroutine

   pure subroutine set_refine_extinction(self,val)
   ! Set whether to correct extinction or not according to the Larson formula
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%refine_extinction = val
      if (NOT val) self%extinction_factor_esd = -ONE
   end subroutine

   PURE subroutine set_extinction_factor(self,val,esd)
   ! Set the secondary extinction factor, and its error "esd" if that is
   ! given. Without an esd the stored one is marked unavailable.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val
      REAL, optional, IN :: esd
   ENSURE(val>=ZERO,"DIFFRACTION_DATA.SET:set_extinction_factor ... negative extinction factor")
      self%extinction_factor = val
      self%extinction_factor_esd = -ONE
      if (present(esd)) self%extinction_factor_esd = esd
   end subroutine

   pure subroutine set_refine_fragments(self,val)
   ! Set whether to refine fragments
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%refine_fragments = val

      ! If refining you can't be fitting
      if (val) self%fit_structure = NOT val
   end subroutine

   pure subroutine set_refine_structure(self,val)
   ! Set whether to refine the structure
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%refine_structure = val

      ! If refining you can't be fitting
      if (val) self%fit_structure = NOT val
   end subroutine

   pure subroutine set_refine_H_positions(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%refine_H_positions = val
   end subroutine

   pure subroutine set_refine_H_ADPs(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%refine_H_ADPs = val
   end subroutine

   pure subroutine set_refine_H_U_iso(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%refine_H_U_iso = val
   end subroutine

   PURE subroutine set_n_soft_modes(self,val)
   ! Set the number of soft modes a TLS model refines.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>=0,"DIFFRACTION_DATA.SET:set_n_soft_modes ... n_soft_modes= must not be negative")
      self%n_soft_modes = val
   end subroutine

   PURE subroutine set_soft_mode_restraint(self,val)
   ! Set the relative sigma of the soft-mode amplitude restraints.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>ZERO,"DIFFRACTION_DATA.SET:set_soft_mode_restraint ... soft_mode_restraint= must be positive")
      self%soft_mode_restraint = val
   end subroutine

   pure subroutine set_use_soft_mode_correlations(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_soft_mode_correlations = val
   end subroutine

   PURE subroutine set_correlation_restraint(self,val)
   ! Set the sigma of the restraint of a TLS correlation coefficient toward 0.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>ZERO,"DIFFRACTION_DATA.SET:set_correlation_restraint ... correlation_restraint= must be positive")
      self%correlation_restraint = val
   end subroutine

   pure subroutine set_use_positive_TLS_sigma(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_positive_TLS_sigma = val
   end subroutine

   PURE subroutine set_ADP_model(self,val)
   ! Set the ADP model: "free" or "tls".
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%ADP_model = val
      call to_lower_case_(self%ADP_model)
      ENSURE(is_one_of_(self%ADP_model,["free","tls "]),"DIFFRACTION_DATA.SET:set_ADP_model ... adp_model= must be free or tls, not "//trim(val))
   end subroutine

   PURE subroutine set_refine_4th_order(self,val)
   ! Read the list of atoms for which fourth-order anharmonicty
   ! parameters are to be refined.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: val
      self%refine_4th_order_for_atom = val
      self%refine_anharmonicity = TRUE
   end subroutine

   pure subroutine set_refine_4th_order_only(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%refine_4th_order_only = val
   end subroutine

   pure subroutine set_refine_anharmonicity(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%refine_anharmonicity = val
   end subroutine

   PURE subroutine set_restart_HAR_with_promolecule(self,val)
   ! Set whether to restart HAR with promolecule every time.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%restart_HAR_with_promolecule = val
   end subroutine

   pure subroutine set_remove_disp_from_F_exp(self,val)
   ! Set whether to correct dispersion in the refinement by removing
   ! the anomalous dispersion from F_exp. This is an "XD_style"
   ! dispersion correction.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%remove_dispersion_from_F_exp = val
   end subroutine


!   set_temperature_factor_model(val) :: PURE
!   ! Read the temperature factor model to use to correct for thermal
!   ! vibration in the calculated structure factors. Use "atom-based"
!   ! or "one-center" for usual ADP based model. Use "none" for static
!   ! structure factors. The remaining options are different
!   ! two-center models, see Jayatilaka & Grimwood (2001) Acta Cryst A.
!   ! NOTE: .partition_model subsumes this ....
!      self :: INOUT
!      val :: STR, IN
!
!   ENSURE(val.is_one_of(["none      ","one-center","atom-based","tc-coppens","tc-stewart","tc-tanaka "]),"unknown data_kind, "//trim(val))
!
!      .temperature_factor_model = val
!      .temperature_factor_model.to_lower_case
!
!   end

   subroutine set_wavelength(self,val)
   ! Read the experimental wavelength
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val
      self%wavelength = val
   end subroutine

   PURE subroutine set_XCW(self,val)
   ! Set if doing an XCW calculation - needed for calculating
   ! n_param, which is incremented by 1 for lambda.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%XCW = val
   end subroutine


!  Cutoffs

   PURE subroutine set_F_sigma_noise(self,val)
   ! Set noise to add to F_exp and F_sigma, in units of F_sigma.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val
      self%F_sigma_noise = val
   end subroutine

   PURE subroutine set_F_sigma_cutoff(self,val)
   ! Set the minimum acceptable value for F_exp/sigma
   ! NOTE: make sure zero is not entered ... just leave it off!
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>=ZERO,"DIFFRACTION_DATA.SET:set_F_sigma_cutoff ... cutoff must not be negative")
      self%F_sigma_cutoff = val
   end subroutine

   PURE subroutine set_I_sigma_cutoff(self,val)
   ! Set the minimum acceptable value for F2_exp/sigma
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>=ZERO,"DIFFRACTION_DATA.SET:set_I_sigma_cutoff ... cutoff must not be negative")
      self%I_sigma_cutoff = val
   end subroutine

   PURE subroutine set_F_z_cutoff(self,val)
   ! Set the F_z_cutoff (in sigma units) for prune_bad_reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>=ZERO,"DIFFRACTION_DATA.SET:set_F_z_cutoff ... cutoff must not be negative")
      self%F_z_cutoff = val
   end subroutine

   PURE subroutine set_I_z_cutoff(self,val)
   ! Set the I_z_cutoff (in sigma units) for prune_bad_reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>=ZERO,"DIFFRACTION_DATA.SET:set_I_z_cutoff ... cutoff must not be negative")
      self%I_z_cutoff = val
   end subroutine

   PURE subroutine set_F_calc_cutoff(self,val)
   ! Set the F_calc_cutoff
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>=ZERO,"DIFFRACTION_DATA.SET:set_F_calc_cutoff ... cutoff must not be negative")
      self%F_calc_cutoff = val
   end subroutine

   PURE subroutine set_F_pred_cutoff(self,val)
   ! Set the F_pred_cutoff
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>=ZERO,"DIFFRACTION_DATA.SET:set_F_pred_cutoff ... cutoff must not be negative")
      self%F_pred_cutoff = val
   end subroutine

   PURE subroutine set_I_pred_cutoff(self,val)
   ! Set the I_pred_cutoff
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>=ZERO,"DIFFRACTION_DATA.SET:set_I_pred_cutoff ... cutoff must not be negative")
      self%I_pred_cutoff = val
   end subroutine


!  Show switches

   pure subroutine set_show_rejects(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%show_rejects = val
   end subroutine

   pure subroutine set_show_refinement_output(self,val)
   ! Set whether to show refinement output
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%show_refinement_output = val
      self%show_refinement_results = val
   end subroutine

   pure subroutine set_show_refinement_results(self,val)
   ! Set whether to show refinmenet statistics results
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%show_refinement_results = val
   end subroutine

   pure subroutine set_show_fit_output(self,val)
   ! Set whether to show fit output
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%show_fit_output = val
      self%show_fit_results = val
   end subroutine

   pure subroutine set_show_fit_results(self,val)
   ! Set whether to show fit statistics results
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%show_fit_results = val
   end subroutine

   pure subroutine set_show_IAM_output(self,val)
   ! Set whether to show IAM output
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%show_IAM_output = val
      self%show_IAM_results = val
   end subroutine

   pure subroutine set_show_IAM_results(self,val)
   ! Set whether to show IAM statistics results
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%show_IAM_results = val
   end subroutine


!   Use switches

   pure subroutine set_use_equivalents(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_equivalents = val
   end subroutine

   pure subroutine set_use_disk_SFs(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_disk_SFs = val
   end subroutine

   pure subroutine set_is_IAM_refinement(self,val)
   ! Set if an IAM refinement. User should not have to do this.
   ! See next option.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%is_IAM_refinement = val
   end subroutine

   pure subroutine set_use_IAM_ITC_FFs(self,val)
   ! Set whether to use IT vol. C atomic form factors (TRUE) or else
   ! gaussian atomic IAM form factors (FALSE). In either case, one is
   ! doing an IAM refinement.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_IAM_ITC_FFs = val

      ! Set the IAM refinenemt switch
      self%is_IAM_refinement = TRUE
   end subroutine

   pure subroutine set_use_Ryde_capping(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_Ryde_capping = val
   end subroutine

   pure subroutine set_use_NN(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_NN = val
   end subroutine

   pure subroutine set_use_NN_capping(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_NN_capping = val
   end subroutine

   pure subroutine set_NN_level(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%NN_level = val
   end subroutine

   pure subroutine set_RI_l_max(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%RI_l_max = val
   end subroutine

   pure subroutine set_RI_exponent_ratio(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%RI_exponent_ratio = val
   end subroutine

   pure subroutine set_expHAR_power(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%expHAR_power = val
   end subroutine

   pure subroutine set_FF_fit_s_max(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%FF_fit_s_max = val
   end subroutine

   pure subroutine set_FF_fit_n_gaussians(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%FF_fit_n_gaussians = val
   end subroutine

   pure subroutine set_FF_fit_n_points(self,val)
   ! Generic set
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%FF_fit_n_points = val
   end subroutine

   pure subroutine set_FF_fit_format(self,val)
   ! Set the output format of the fitted form-factor coefficients.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%FF_fit_format = val
      call to_lower_case_(self%FF_fit_format)
   end subroutine

   subroutine set_use_text_SFs(self,val)
   ! Set whether to use text format structure factors
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      self%use_text_SFs = val
      self%use_disk_SFs = val
   end subroutine


!  ==============
!  Update methods
!  ==============

   subroutine update(self,unit_cell,spacegroup)
   ! Update the crystal information after setting values. If the
   ! "unit_cell" is provided, then that is used to define the
   ! reflection .theta values.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      type(UNIT_CELL_TYPE), IN :: unit_cell
      type(SPACEGROUP_TYPE), IN :: spacegroup
      REAL :: stl_max,sin_theta,lambda,lambda_max
      STR(len=STR_SIZE) :: lam,lam_max,sth
      if (NOT allocated(self%reflections)) then

         ! Set hkl range ?
         if (allocated(self%hkl_range)) call set_hkl_range_(self%reflections,self%hkl_range)

         ! Set hkl up to stl_limit?
         if (self%stl_limit>ZERO) call set_hkl_up_to_stl_limit_(self%reflections,self%stl_limit,unit_cell)
      else

         ! The untainted reflections, taken once; only show_rejects reads
         ! them. A repeated xray_data= block re-prunes the working list in
         ! place, so a rejected reflection stays rejected until the job is
         ! rerun at the final geometry. CRYSTAL:update_xray_data cuts the
         ! form factors on disk down to match.
         if (NOT allocated(self%reflection0)) self%reflection0 = self%reflections

         ! Refuse a wavelength these reflections cannot have been measured at.
         ! .set_d_and_theta clamps sin(theta) to one, which is right for a
         ! reflection a rounding error outside the limiting sphere and wrong
         ! for a wavelength that is simply not this dataset's: the reflection
         ! is handed theta = 90 degrees and the extinction angular factor
         ! blows up. See TASKS_AND_HISTORY.md.
         stl_max = max_stl_(self%reflections,unit_cell)
         if (stl_max*self%wavelength>ONE+DIFFRACTION_DATA_STL_LAMBDA_TOL) then
            sin_theta = stl_max*self%wavelength
            lambda = to_units_(self%wavelength,"angstrom")
            lambda_max = ONE/stl_max
            lambda_max = to_units_(lambda_max,"angstrom")
            lam = adjustl(to_str_(lambda,"f9.6"))
            lam_max = adjustl(to_str_(lambda_max,"f9.6"))
            sth = adjustl(to_str_(sin_theta,"f9.6"))

            ! The literal must stay a literal: the translator prepends the
            ! module and routine name inside it, and cannot do that for a
            ! variable.
            DIE("DIFFRACTION_DATA.SET:update ... wavelength inconsistent with the reflection data: "//trim(lam)//" A given, but these reflections need "//trim(lam_max)//" A or less; the largest sin(theta)/lambda"//" implies sin(theta) = "//trim(sth))
         end if

         ! Set d, stl, theta 
         call set_d_and_theta_(self%reflection0,unit_cell,self%wavelength)
         call set_d_and_theta_(self%reflections,unit_cell,self%wavelength)

         ! Equivalence factors
         call set_equivalence_factors_(self%reflection0,spacegroup)
         call set_equivalence_factors_(self%reflections,spacegroup)

         ! Prune & sort !!!
         call prune_reflections_(self)
         call sort_reflections_(self)
         !!!!!!!!!!!!!!!!!!
         if (have_F_exp_(self)) then
            ENSURE(have_nonzero_F_sigma_(self%reflections),"DIFFRACTION_DATA.SET:update ... no experimental F_sigma's")

            ! Explicit scaling
            if (self%exp_scale_factor>ZERO) then
               call scale_F_exp_(self%reflection0,self%exp_scale_factor)
               call scale_F_exp_(self%reflections,self%exp_scale_factor)
               call scale_F_sigma_(self%reflection0,self%exp_scale_factor)
               call scale_F_sigma_(self%reflections,self%exp_scale_factor)

               ! Turn off next time
               self%exp_scale_factor = -ONE
            end if
         else
            self%refine_scale = FALSE
            self%refine_extinction = FALSE
         end if
      end if
      if (self%show_rejects) then
         if (size(self%reflection0)>size(self%reflections)) then
            call flush_(stdout)
            call text_(stdout,"==========================")
            call text_(stdout,"Reflections before pruning")
            call text_(stdout,"==========================")
            call put_(self%reflection0)
            call flush_(stdout)
            call text_(stdout,"=========================")
            call text_(stdout,"Reflections after pruning")
            call text_(stdout,"=========================")
            call put_(self%reflections)
         end if
      end if
   end subroutine

   subroutine process_fcf_file(self)
   ! Process tge specified fcf_file and read the reflections.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      type(CIF_TYPE), allocatable :: cif
      BIN :: found

      ! Read the fcf file if requested
      if (self%fcf_file_name/=" ") then
         call create_(cif,trim(self%fcf_file_name))
         call open_(cif)
         call find_data_block_(cif,found)
         DIE_IF(NOT found,"DIFFRACTION_DATA.SET:process_fcf_file ... no data block found in your fcf file: "//trim(self%fcf_file_name))
         found = has_smCIF_F_meas_only_(cif)
         if (found) then
            call read_smCIF_F_meas_only_(self%reflections,cif)  ! leaky
         else
            found = has_smCIF_F2_reflections_(cif)
            DIE_IF(NOT found,"DIFFRACTION_DATA.SET:process_fcf_file ... no F or F_squared reflections in your fcf file: "//trim(self%fcf_file_name))
            call read_smCIF_F2_reflections_(self%reflections,cif)  ! leaky
         end if
         call destroy_(cif)
      end if
   end subroutine

   subroutine sort_reflections(self)
   ! I sorted according to one data set that I saw. --Dylan
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n_refl,i,hc
      VEC(INT,:), allocatable :: hi,list,indx
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:sort_reflections ... no reflection data")
      n_refl = size(self%reflections)
      call create_(hi,3)
      call create_(list,n_refl)
      call create_(indx,n_refl)

      ! Sort according to H index code
#     define CLUSTER_H_MAX 127
      do i = 1,n_refl
         hi = Miller_indices_(self%reflections(i))
         hc = CLUSTER_H_CODE(hi(3),hi(2),hi(1))
         list(i) = hc
      end do
#     define CLUSTER_H_MAX 7
      call destroy_(indx)
      call destroy_(list)
      call destroy_(hi)
   end subroutine


! ===================
! Pruning reflections
! ===================

   subroutine prune_reflections(self)
   ! Zero-out negative reflections, adding appropriate error to sigma
   ! If random errors are to be added, process and add error to sigma.
   ! Then prune all the reflections, as requested by cutoffs.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: n_refl,n_prun

      ! Prune small I's and define F's
      if (have_I_exp_(self)) then
         DIE_IF(NOT have_nonzero_I_sigma_(self),"DIFFRACTION_DATA.SET:prune_reflections ... I_exp present but no I_sigma?")
         call prune_small_I_sigmas_(self)  ! < TOL(6)
         ! -I_exp => I_sigma <- I_sigma + |-I_exp| AND I_exp <- 0
      ! .reflections.reset_I_exp_I_sigma
      end if
      if (self%I_sigma_cutoff>ZERO) then
         if (have_I_exp_(self)) then
            call prune_weak_Is_(self)
         else
            WARN("DIFFRACTION_DATA.SET:prune_reflections ... I_sigma_cutoff specified but no I_exp data")
         end if
      end if

      ! Next prune small F's
      if (have_F_exp_(self)) then
         DIE_IF(NOT have_nonzero_F_sigma_(self),"DIFFRACTION_DATA.SET:prune_reflections ... F_exp present but no F_sigma?")
         call prune_small_F_sigmas_(self)  ! < TOL(3)
         ! -F_exp => F_sigma <- F_sigma + |-F_exp| AND F_exp <- 0
      ! .reflections.reset_F_exp_F_sigma
      end if

      ! Add noise
      ! BUG: this runs on every call, so each xray_data= block and each
      ! pre-refinement or in-loop prune adds noise again and inflates
      ! F_sigma again. It should happen once, when the data are read.
      ! See TASKS_AND_HISTORY.md, 2026-09-25 (afternoon).
      if (self%F_sigma_noise/=ZERO AND have_F_exp_(self)) then
         DIE_IF(NOT have_nonzero_F_sigma_(self),"DIFFRACTION_DATA.SET:prune_reflections ... F_exp present but no F_sigma?")
         call add_noise_(self%reflections,self%F_sigma_noise)
      end if

      ! Prune small F_exp/F_sigma
      if (self%F_sigma_cutoff>ZERO) then
         if (have_F_exp_(self) AND have_nonzero_F_sigma_(self)) then
            call prune_weak_Fs_(self)
         else
            WARN("DIFFRACTION_DATA.SET:prune_reflections ... F_sigma_cutoff specified but no F_exp/F_sigma data")
         end if
      end if

      ! NOTE if doing below, do it repeatedly
      ! because the scale factor will change
      if (self%F_z_cutoff>ZERO) then
         if (have_F_exp_(self) AND have_F_pred_(self)) then
            do
               n_refl = size(self%reflections)
            ! Prune
               call prune_bad_F_reflections_(self)
               n_prun = size(self%reflections)
            ! Redo?
               if (n_prun==n_refl) then; exit
               else; call make_F_predicted_(self)
               end if
            end do
         else
            WARN("DIFFRACTION_DATA.SET:prune_reflections ... F_z_cutoff specified but no F_exp/F_pred data")
         end if
      end if

      ! Prune F_calc's which are zero
      if (self%F_calc_cutoff>ZERO) then
         if (have_F_calc_(self)) then
            call prune_zero_F_calcs_(self)
         end if
      end if

      ! Prune F_pred's which are zero
      if (self%F_pred_cutoff>ZERO) then
         if (have_F_pred_(self)) then
            call prune_zero_F_preds_(self)
         else
            WARN("DIFFRACTION_DATA.SET:prune_reflections ... F_pred_cutoff specified but no F_pred data")
         end if
      end if
      if (self%stl_high_cutoff>ZERO) then
         call prune_high_angle_reflections_(self)
      end if
      if (self%stl_low_cutoff>ZERO) then
         call prune_low_angle_reflections_(self)
      end if
   end subroutine

   subroutine prune_bad_F_reflections(self)
   ! This routine eliminates all the reflections with respect to a
   ! rejection test on the reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable :: new_reflections
      VEC(INT,:), allocatable :: good,bad,h1,h2,h3
      VEC(REAL,:), allocatable :: val
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: n_refl,n,i,g,b
      STR(len=STR_SIZE) :: title
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:prune_bad_F_reflections ... No reflection data")

      ! No. of old reflections

      ! The following code is inherited from DIFFRACTION_DATA.SET
      n_refl = size(self%reflections)

      ! Find out how many rejects "n" there are
      n = 0
      do i = 1,n_refl
         if (NOT has_large_abs_F_z_(self%reflections(i),self%F_z_cutoff)) cycle
         n = n+1
      end do

      ! Are there any weak reflections?
      if (n==0) return

      ! Are there any weak reflections?
      if (n==n_refl) then
         DIE("DIFFRACTION_DATA.SET:prune_bad_F_reflections ... All reflection data has been rejected!")
      end if

      ! Create the "good" and "bad" lists and how many of each
      call create_(bad,n)
      call create_(good,n_refl-n)
      g = 0
      b = 0
      do i = 1,n_refl
         if (has_large_abs_F_z_(self%reflections(i),self%F_z_cutoff)) then
            b = b+1
            bad(b) = i
         else
            g = g+1
            good(g) = i
         end if
      end do

      ! Print out
      call flush_(stdout)
      title = "Reflections pruned"
      call text_(stdout,repeat("=",len_trim(title)))
      call text_(stdout,trim(title))
      call text_(stdout,repeat("=",len_trim(title)))

      ! Banner
      call flush_(stdout)
      call show_(stdout,"Rejection criteria      =","has_large_abs_F_z")
      call flush_(stdout)
      call show_(stdout,"No. of reflections      =",n_refl)
      call show_(stdout,"No. of rejects          =",b)
      call show_(stdout,"No. of kept reflections =",g)
      call show_(stdout,"Wavelength (Bohr)       =",self%wavelength)
      call show_(stdout,"Wavelength (Angstrom)   =",self%wavelength*ANGSTROM_PER_BOHR)
      if ("abs_F_z"=="stl") then
         call show_(stdout,"Cutoff (Bohr^-1)        =",self%F_z_cutoff)
         call show_(stdout,"Cutoff (Angstrom^-1)    =",self%F_z_cutoff/ANGSTROM_PER_BOHR)
      else
         call show_(stdout,"Cutoff                  =",self%F_z_cutoff)
      end if
      if (self%show_rejects) then
         call flush_(stdout)
         call text_(stdout,"Rejects:")

         ! Get data
         n_refl = size(bad)
         call create_(h1,n_refl); h1 = self%reflections(bad(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(bad(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(bad(:))%h3
         call create_(val,n_refl); val = abs_F_z_(self%reflections(bad(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"abs_F_z"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
         call flush_(stdout)
         call text_(stdout,"Kept:")

         ! Get data
         n_refl = size(good)
         call create_(h1,n_refl); h1 = self%reflections(good(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(good(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(good(:))%h3
         call create_(val,n_refl); val = abs_F_z_(self%reflections(good(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"abs_F_z"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
      end if

      ! Copy the non-rejects
      call create_(new_reflections,g)
      new_reflections = self%reflections(good)
      self%reflections = new_reflections
      !!!!!!!!!!!!!!!!!!!!!!

      ! Clean
      call destroy_(bad)
      call destroy_(good)
   end subroutine

   subroutine prune_high_angle_reflections(self)
   ! This routine eliminates all the reflections with respect to a
   ! rejection test on the reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable :: new_reflections
      VEC(INT,:), allocatable :: good,bad,h1,h2,h3
      VEC(REAL,:), allocatable :: val
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: n_refl,n,i,g,b
      STR(len=STR_SIZE) :: title
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:prune_high_angle_reflections ... No reflection data")

      ! No. of old reflections

      ! The following code is inherited from DIFFRACTION_DATA.SET
      n_refl = size(self%reflections)

      ! Find out how many rejects "n" there are
      n = 0
      do i = 1,n_refl
         if (NOT has_high_stl_(self%reflections(i),self%stl_high_cutoff)) cycle
         n = n+1
      end do

      ! Are there any weak reflections?
      if (n==0) return

      ! Are there any weak reflections?
      if (n==n_refl) then
         DIE("DIFFRACTION_DATA.SET:prune_high_angle_reflections ... All reflection data has been rejected!")
      end if

      ! Create the "good" and "bad" lists and how many of each
      call create_(bad,n)
      call create_(good,n_refl-n)
      g = 0
      b = 0
      do i = 1,n_refl
         if (has_high_stl_(self%reflections(i),self%stl_high_cutoff)) then
            b = b+1
            bad(b) = i
         else
            g = g+1
            good(g) = i
         end if
      end do

      ! Print out
      call flush_(stdout)
      title = "Reflections pruned"
      call text_(stdout,repeat("=",len_trim(title)))
      call text_(stdout,trim(title))
      call text_(stdout,repeat("=",len_trim(title)))

      ! Banner
      call flush_(stdout)
      call show_(stdout,"Rejection criteria      =","has_high_stl")
      call flush_(stdout)
      call show_(stdout,"No. of reflections      =",n_refl)
      call show_(stdout,"No. of rejects          =",b)
      call show_(stdout,"No. of kept reflections =",g)
      call show_(stdout,"Wavelength (Bohr)       =",self%wavelength)
      call show_(stdout,"Wavelength (Angstrom)   =",self%wavelength*ANGSTROM_PER_BOHR)
      if ("stl"=="stl") then
         call show_(stdout,"Cutoff (Bohr^-1)        =",self%stl_high_cutoff)
         call show_(stdout,"Cutoff (Angstrom^-1)    =",self%stl_high_cutoff/ANGSTROM_PER_BOHR)
      else
         call show_(stdout,"Cutoff                  =",self%stl_high_cutoff)
      end if
      if (self%show_rejects) then
         call flush_(stdout)
         call text_(stdout,"Rejects:")

         ! Get data
         n_refl = size(bad)
         call create_(h1,n_refl); h1 = self%reflections(bad(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(bad(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(bad(:))%h3
         call create_(val,n_refl); val = stl_(self%reflections(bad(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"stl"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
         call flush_(stdout)
         call text_(stdout,"Kept:")

         ! Get data
         n_refl = size(good)
         call create_(h1,n_refl); h1 = self%reflections(good(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(good(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(good(:))%h3
         call create_(val,n_refl); val = stl_(self%reflections(good(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"stl"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
      end if

      ! Copy the non-rejects
      call create_(new_reflections,g)
      new_reflections = self%reflections(good)
      self%reflections = new_reflections
      !!!!!!!!!!!!!!!!!!!!!!

      ! Clean
      call destroy_(bad)
      call destroy_(good)
   end subroutine

   subroutine prune_low_angle_reflections(self)
   ! This routine eliminates all the reflections with respect to a
   ! rejection test on the reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable :: new_reflections
      VEC(INT,:), allocatable :: good,bad,h1,h2,h3
      VEC(REAL,:), allocatable :: val
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: n_refl,n,i,g,b
      STR(len=STR_SIZE) :: title
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:prune_low_angle_reflections ... No reflection data")

      ! No. of old reflections

      ! The following code is inherited from DIFFRACTION_DATA.SET
      n_refl = size(self%reflections)

      ! Find out how many rejects "n" there are
      n = 0
      do i = 1,n_refl
         if (NOT has_low_stl_(self%reflections(i),self%stl_low_cutoff)) cycle
         n = n+1
      end do

      ! Are there any weak reflections?
      if (n==0) return

      ! Are there any weak reflections?
      if (n==n_refl) then
         DIE("DIFFRACTION_DATA.SET:prune_low_angle_reflections ... All reflection data has been rejected!")
      end if

      ! Create the "good" and "bad" lists and how many of each
      call create_(bad,n)
      call create_(good,n_refl-n)
      g = 0
      b = 0
      do i = 1,n_refl
         if (has_low_stl_(self%reflections(i),self%stl_low_cutoff)) then
            b = b+1
            bad(b) = i
         else
            g = g+1
            good(g) = i
         end if
      end do

      ! Print out
      call flush_(stdout)
      title = "Reflections pruned"
      call text_(stdout,repeat("=",len_trim(title)))
      call text_(stdout,trim(title))
      call text_(stdout,repeat("=",len_trim(title)))

      ! Banner
      call flush_(stdout)
      call show_(stdout,"Rejection criteria      =","has_low_stl")
      call flush_(stdout)
      call show_(stdout,"No. of reflections      =",n_refl)
      call show_(stdout,"No. of rejects          =",b)
      call show_(stdout,"No. of kept reflections =",g)
      call show_(stdout,"Wavelength (Bohr)       =",self%wavelength)
      call show_(stdout,"Wavelength (Angstrom)   =",self%wavelength*ANGSTROM_PER_BOHR)
      if ("stl"=="stl") then
         call show_(stdout,"Cutoff (Bohr^-1)        =",self%stl_low_cutoff)
         call show_(stdout,"Cutoff (Angstrom^-1)    =",self%stl_low_cutoff/ANGSTROM_PER_BOHR)
      else
         call show_(stdout,"Cutoff                  =",self%stl_low_cutoff)
      end if
      if (self%show_rejects) then
         call flush_(stdout)
         call text_(stdout,"Rejects:")

         ! Get data
         n_refl = size(bad)
         call create_(h1,n_refl); h1 = self%reflections(bad(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(bad(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(bad(:))%h3
         call create_(val,n_refl); val = stl_(self%reflections(bad(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"stl"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
         call flush_(stdout)
         call text_(stdout,"Kept:")

         ! Get data
         n_refl = size(good)
         call create_(h1,n_refl); h1 = self%reflections(good(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(good(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(good(:))%h3
         call create_(val,n_refl); val = stl_(self%reflections(good(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"stl"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
      end if

      ! Copy the non-rejects
      call create_(new_reflections,g)
      new_reflections = self%reflections(good)
      self%reflections = new_reflections
      !!!!!!!!!!!!!!!!!!!!!!

      ! Clean
      call destroy_(bad)
      call destroy_(good)
   end subroutine

   subroutine prune_weak_Fs(self)
   ! This routine eliminates all the reflections with respect to a
   ! rejection test on the reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable :: new_reflections
      VEC(INT,:), allocatable :: good,bad,h1,h2,h3
      VEC(REAL,:), allocatable :: val
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: n_refl,n,i,g,b
      STR(len=STR_SIZE) :: title
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:prune_weak_Fs ... No reflection data")

      ! No. of old reflections

      ! The following code is inherited from DIFFRACTION_DATA.SET
      n_refl = size(self%reflections)

      ! Find out how many rejects "n" there are
      n = 0
      do i = 1,n_refl
         if (NOT has_weak_F_exp_(self%reflections(i),self%F_sigma_cutoff)) cycle
         n = n+1
      end do

      ! Are there any weak reflections?
      if (n==0) return

      ! Are there any weak reflections?
      if (n==n_refl) then
         DIE("DIFFRACTION_DATA.SET:prune_weak_Fs ... All reflection data has been rejected!")
      end if

      ! Create the "good" and "bad" lists and how many of each
      call create_(bad,n)
      call create_(good,n_refl-n)
      g = 0
      b = 0
      do i = 1,n_refl
         if (has_weak_F_exp_(self%reflections(i),self%F_sigma_cutoff)) then
            b = b+1
            bad(b) = i
         else
            g = g+1
            good(g) = i
         end if
      end do

      ! Print out
      call flush_(stdout)
      title = "Reflections pruned"
      call text_(stdout,repeat("=",len_trim(title)))
      call text_(stdout,trim(title))
      call text_(stdout,repeat("=",len_trim(title)))

      ! Banner
      call flush_(stdout)
      call show_(stdout,"Rejection criteria      =","has_weak_F_exp")
      call flush_(stdout)
      call show_(stdout,"No. of reflections      =",n_refl)
      call show_(stdout,"No. of rejects          =",b)
      call show_(stdout,"No. of kept reflections =",g)
      call show_(stdout,"Wavelength (Bohr)       =",self%wavelength)
      call show_(stdout,"Wavelength (Angstrom)   =",self%wavelength*ANGSTROM_PER_BOHR)
      if ("F_exp_on_sigma"=="stl") then
         call show_(stdout,"Cutoff (Bohr^-1)        =",self%F_sigma_cutoff)
         call show_(stdout,"Cutoff (Angstrom^-1)    =",self%F_sigma_cutoff/ANGSTROM_PER_BOHR)
      else
         call show_(stdout,"Cutoff                  =",self%F_sigma_cutoff)
      end if
      if (self%show_rejects) then
         call flush_(stdout)
         call text_(stdout,"Rejects:")

         ! Get data
         n_refl = size(bad)
         call create_(h1,n_refl); h1 = self%reflections(bad(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(bad(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(bad(:))%h3
         call create_(val,n_refl); val = F_exp_on_sigma_(self%reflections(bad(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"F_exp_on_sigma"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
         call flush_(stdout)
         call text_(stdout,"Kept:")

         ! Get data
         n_refl = size(good)
         call create_(h1,n_refl); h1 = self%reflections(good(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(good(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(good(:))%h3
         call create_(val,n_refl); val = F_exp_on_sigma_(self%reflections(good(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"F_exp_on_sigma"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
      end if

      ! Copy the non-rejects
      call create_(new_reflections,g)
      new_reflections = self%reflections(good)
      self%reflections = new_reflections
      !!!!!!!!!!!!!!!!!!!!!!

      ! Clean
      call destroy_(bad)
      call destroy_(good)
   end subroutine

   subroutine prune_weak_Is(self)
   ! This routine eliminates all the reflections with respect to a
   ! rejection test on the reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable :: new_reflections
      VEC(INT,:), allocatable :: good,bad,h1,h2,h3
      VEC(REAL,:), allocatable :: val
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: n_refl,n,i,g,b
      STR(len=STR_SIZE) :: title
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:prune_weak_Is ... No reflection data")

      ! No. of old reflections

      ! The following code is inherited from DIFFRACTION_DATA.SET
      n_refl = size(self%reflections)

      ! Find out how many rejects "n" there are
      n = 0
      do i = 1,n_refl
         if (NOT has_weak_I_exp_(self%reflections(i),self%I_sigma_cutoff)) cycle
         n = n+1
      end do

      ! Are there any weak reflections?
      if (n==0) return

      ! Are there any weak reflections?
      if (n==n_refl) then
         DIE("DIFFRACTION_DATA.SET:prune_weak_Is ... All reflection data has been rejected!")
      end if

      ! Create the "good" and "bad" lists and how many of each
      call create_(bad,n)
      call create_(good,n_refl-n)
      g = 0
      b = 0
      do i = 1,n_refl
         if (has_weak_I_exp_(self%reflections(i),self%I_sigma_cutoff)) then
            b = b+1
            bad(b) = i
         else
            g = g+1
            good(g) = i
         end if
      end do

      ! Print out
      call flush_(stdout)
      title = "Reflections pruned"
      call text_(stdout,repeat("=",len_trim(title)))
      call text_(stdout,trim(title))
      call text_(stdout,repeat("=",len_trim(title)))

      ! Banner
      call flush_(stdout)
      call show_(stdout,"Rejection criteria      =","has_weak_I_exp")
      call flush_(stdout)
      call show_(stdout,"No. of reflections      =",n_refl)
      call show_(stdout,"No. of rejects          =",b)
      call show_(stdout,"No. of kept reflections =",g)
      call show_(stdout,"Wavelength (Bohr)       =",self%wavelength)
      call show_(stdout,"Wavelength (Angstrom)   =",self%wavelength*ANGSTROM_PER_BOHR)
      if ("I_exp_on_sigma"=="stl") then
         call show_(stdout,"Cutoff (Bohr^-1)        =",self%I_sigma_cutoff)
         call show_(stdout,"Cutoff (Angstrom^-1)    =",self%I_sigma_cutoff/ANGSTROM_PER_BOHR)
      else
         call show_(stdout,"Cutoff                  =",self%I_sigma_cutoff)
      end if
      if (self%show_rejects) then
         call flush_(stdout)
         call text_(stdout,"Rejects:")

         ! Get data
         n_refl = size(bad)
         call create_(h1,n_refl); h1 = self%reflections(bad(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(bad(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(bad(:))%h3
         call create_(val,n_refl); val = I_exp_on_sigma_(self%reflections(bad(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"I_exp_on_sigma"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
         call flush_(stdout)
         call text_(stdout,"Kept:")

         ! Get data
         n_refl = size(good)
         call create_(h1,n_refl); h1 = self%reflections(good(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(good(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(good(:))%h3
         call create_(val,n_refl); val = I_exp_on_sigma_(self%reflections(good(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"I_exp_on_sigma"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
      end if

      ! Copy the non-rejects
      call create_(new_reflections,g)
      new_reflections = self%reflections(good)
      self%reflections = new_reflections
      !!!!!!!!!!!!!!!!!!!!!!

      ! Clean
      call destroy_(bad)
      call destroy_(good)
   end subroutine

   subroutine prune_zero_F_preds(self)
   ! This routine eliminates all the reflections with respect to a
   ! rejection test on the reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable :: new_reflections
      VEC(INT,:), allocatable :: good,bad,h1,h2,h3
      VEC(REAL,:), allocatable :: val
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: n_refl,n,i,g,b
      STR(len=STR_SIZE) :: title
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:prune_zero_F_preds ... No reflection data")

      ! No. of old reflections

      ! The following code is inherited from DIFFRACTION_DATA.SET
      n_refl = size(self%reflections)

      ! Find out how many rejects "n" there are
      n = 0
      do i = 1,n_refl
         if (NOT has_small_F_pred_(self%reflections(i),TOL(3))) cycle
         n = n+1
      end do

      ! Are there any weak reflections?
      if (n==0) return

      ! Are there any weak reflections?
      if (n==n_refl) then
         DIE("DIFFRACTION_DATA.SET:prune_zero_F_preds ... All reflection data has been rejected!")
      end if

      ! Create the "good" and "bad" lists and how many of each
      call create_(bad,n)
      call create_(good,n_refl-n)
      g = 0
      b = 0
      do i = 1,n_refl
         if (has_small_F_pred_(self%reflections(i),TOL(3))) then
            b = b+1
            bad(b) = i
         else
            g = g+1
            good(g) = i
         end if
      end do

      ! Print out
      call flush_(stdout)
      title = "Reflections pruned"
      call text_(stdout,repeat("=",len_trim(title)))
      call text_(stdout,trim(title))
      call text_(stdout,repeat("=",len_trim(title)))

      ! Banner
      call flush_(stdout)
      call show_(stdout,"Rejection criteria      =","has_small_F_pred")
      call flush_(stdout)
      call show_(stdout,"No. of reflections      =",n_refl)
      call show_(stdout,"No. of rejects          =",b)
      call show_(stdout,"No. of kept reflections =",g)
      call show_(stdout,"Wavelength (Bohr)       =",self%wavelength)
      call show_(stdout,"Wavelength (Angstrom)   =",self%wavelength*ANGSTROM_PER_BOHR)
      if ("F_pred"=="stl") then
         call show_(stdout,"Cutoff (Bohr^-1)        =",TOL(3))
         call show_(stdout,"Cutoff (Angstrom^-1)    =",TOL(3)/ANGSTROM_PER_BOHR)
      else
         call show_(stdout,"Cutoff                  =",TOL(3))
      end if
      if (self%show_rejects) then
         call flush_(stdout)
         call text_(stdout,"Rejects:")

         ! Get data
         n_refl = size(bad)
         call create_(h1,n_refl); h1 = self%reflections(bad(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(bad(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(bad(:))%h3
         call create_(val,n_refl); val = F_pred_(self%reflections(bad(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"F_pred"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
         call flush_(stdout)
         call text_(stdout,"Kept:")

         ! Get data
         n_refl = size(good)
         call create_(h1,n_refl); h1 = self%reflections(good(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(good(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(good(:))%h3
         call create_(val,n_refl); val = F_pred_(self%reflections(good(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"F_pred"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
      end if

      ! Copy the non-rejects
      call create_(new_reflections,g)
      new_reflections = self%reflections(good)
      self%reflections = new_reflections
      !!!!!!!!!!!!!!!!!!!!!!

      ! Clean
      call destroy_(bad)
      call destroy_(good)
   end subroutine

   subroutine prune_zero_F_calcs(self)
   ! This routine eliminates all the reflections with respect to a
   ! rejection test on the reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable :: new_reflections
      VEC(INT,:), allocatable :: good,bad,h1,h2,h3
      VEC(REAL,:), allocatable :: val
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: n_refl,n,i,g,b
      STR(len=STR_SIZE) :: title
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:prune_zero_F_calcs ... No reflection data")

      ! No. of old reflections

      ! The following code is inherited from DIFFRACTION_DATA.SET
      n_refl = size(self%reflections)

      ! Find out how many rejects "n" there are
      n = 0
      do i = 1,n_refl
         if (NOT has_zero_F_calc_abs_(self%reflections(i),self%F_calc_cutoff)) cycle
         n = n+1
      end do

      ! Are there any weak reflections?
      if (n==0) return

      ! Are there any weak reflections?
      if (n==n_refl) then
         DIE("DIFFRACTION_DATA.SET:prune_zero_F_calcs ... All reflection data has been rejected!")
      end if

      ! Create the "good" and "bad" lists and how many of each
      call create_(bad,n)
      call create_(good,n_refl-n)
      g = 0
      b = 0
      do i = 1,n_refl
         if (has_zero_F_calc_abs_(self%reflections(i),self%F_calc_cutoff)) then
            b = b+1
            bad(b) = i
         else
            g = g+1
            good(g) = i
         end if
      end do

      ! Print out
      call flush_(stdout)
      title = "Reflections pruned"
      call text_(stdout,repeat("=",len_trim(title)))
      call text_(stdout,trim(title))
      call text_(stdout,repeat("=",len_trim(title)))

      ! Banner
      call flush_(stdout)
      call show_(stdout,"Rejection criteria      =","has_zero_F_calc_abs")
      call flush_(stdout)
      call show_(stdout,"No. of reflections      =",n_refl)
      call show_(stdout,"No. of rejects          =",b)
      call show_(stdout,"No. of kept reflections =",g)
      call show_(stdout,"Wavelength (Bohr)       =",self%wavelength)
      call show_(stdout,"Wavelength (Angstrom)   =",self%wavelength*ANGSTROM_PER_BOHR)
      if ("F_calc_abs"=="stl") then
         call show_(stdout,"Cutoff (Bohr^-1)        =",self%F_calc_cutoff)
         call show_(stdout,"Cutoff (Angstrom^-1)    =",self%F_calc_cutoff/ANGSTROM_PER_BOHR)
      else
         call show_(stdout,"Cutoff                  =",self%F_calc_cutoff)
      end if
      if (self%show_rejects) then
         call flush_(stdout)
         call text_(stdout,"Rejects:")

         ! Get data
         n_refl = size(bad)
         call create_(h1,n_refl); h1 = self%reflections(bad(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(bad(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(bad(:))%h3
         call create_(val,n_refl); val = F_calc_abs_(self%reflections(bad(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"F_calc_abs"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
         call flush_(stdout)
         call text_(stdout,"Kept:")

         ! Get data
         n_refl = size(good)
         call create_(h1,n_refl); h1 = self%reflections(good(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(good(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(good(:))%h3
         call create_(val,n_refl); val = F_calc_abs_(self%reflections(good(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"F_calc_abs"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
      end if

      ! Copy the non-rejects
      call create_(new_reflections,g)
      new_reflections = self%reflections(good)
      self%reflections = new_reflections
      !!!!!!!!!!!!!!!!!!!!!!

      ! Clean
      call destroy_(bad)
      call destroy_(good)
   end subroutine

   subroutine prune_small_F_sigmas(self)
   ! This routine eliminates all the reflections with respect to a
   ! rejection test on the reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable :: new_reflections
      VEC(INT,:), allocatable :: good,bad,h1,h2,h3
      VEC(REAL,:), allocatable :: val
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: n_refl,n,i,g,b
      STR(len=STR_SIZE) :: title
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:prune_small_F_sigmas ... No reflection data")

      ! No. of old reflections

      ! The following code is inherited from DIFFRACTION_DATA.SET
      n_refl = size(self%reflections)

      ! Find out how many rejects "n" there are
      n = 0
      do i = 1,n_refl
         if (NOT has_small_F_sigma_(self%reflections(i),TOL(3))) cycle
         n = n+1
      end do

      ! Are there any weak reflections?
      if (n==0) return

      ! Are there any weak reflections?
      if (n==n_refl) then
         DIE("DIFFRACTION_DATA.SET:prune_small_F_sigmas ... All reflection data has been rejected!")
      end if

      ! Create the "good" and "bad" lists and how many of each
      call create_(bad,n)
      call create_(good,n_refl-n)
      g = 0
      b = 0
      do i = 1,n_refl
         if (has_small_F_sigma_(self%reflections(i),TOL(3))) then
            b = b+1
            bad(b) = i
         else
            g = g+1
            good(g) = i
         end if
      end do

      ! Print out
      call flush_(stdout)
      title = "Reflections pruned"
      call text_(stdout,repeat("=",len_trim(title)))
      call text_(stdout,trim(title))
      call text_(stdout,repeat("=",len_trim(title)))

      ! Banner
      call flush_(stdout)
      call show_(stdout,"Rejection criteria      =","has_small_F_sigma")
      call flush_(stdout)
      call show_(stdout,"No. of reflections      =",n_refl)
      call show_(stdout,"No. of rejects          =",b)
      call show_(stdout,"No. of kept reflections =",g)
      call show_(stdout,"Wavelength (Bohr)       =",self%wavelength)
      call show_(stdout,"Wavelength (Angstrom)   =",self%wavelength*ANGSTROM_PER_BOHR)
      if ("F_sigma"=="stl") then
         call show_(stdout,"Cutoff (Bohr^-1)        =",TOL(3))
         call show_(stdout,"Cutoff (Angstrom^-1)    =",TOL(3)/ANGSTROM_PER_BOHR)
      else
         call show_(stdout,"Cutoff                  =",TOL(3))
      end if
      if (self%show_rejects) then
         call flush_(stdout)
         call text_(stdout,"Rejects:")

         ! Get data
         n_refl = size(bad)
         call create_(h1,n_refl); h1 = self%reflections(bad(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(bad(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(bad(:))%h3
         call create_(val,n_refl); val = F_sigma_(self%reflections(bad(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"F_sigma"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
         call flush_(stdout)
         call text_(stdout,"Kept:")

         ! Get data
         n_refl = size(good)
         call create_(h1,n_refl); h1 = self%reflections(good(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(good(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(good(:))%h3
         call create_(val,n_refl); val = F_sigma_(self%reflections(good(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"F_sigma"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
      end if

      ! Copy the non-rejects
      call create_(new_reflections,g)
      new_reflections = self%reflections(good)
      self%reflections = new_reflections
      !!!!!!!!!!!!!!!!!!!!!!

      ! Clean
      call destroy_(bad)
      call destroy_(good)
   end subroutine

   subroutine prune_small_I_sigmas(self)
   ! This routine eliminates all the reflections with respect to a
   ! rejection test on the reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable :: new_reflections
      VEC(INT,:), allocatable :: good,bad,h1,h2,h3
      VEC(REAL,:), allocatable :: val
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: n_refl,n,i,g,b
      STR(len=STR_SIZE) :: title
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:prune_small_I_sigmas ... No reflection data")

      ! No. of old reflections

      ! The following code is inherited from DIFFRACTION_DATA.SET
      n_refl = size(self%reflections)

      ! Find out how many rejects "n" there are
      n = 0
      do i = 1,n_refl
         if (NOT has_small_I_sigma_(self%reflections(i),TOL(6))) cycle
         n = n+1
      end do

      ! Are there any weak reflections?
      if (n==0) return

      ! Are there any weak reflections?
      if (n==n_refl) then
         DIE("DIFFRACTION_DATA.SET:prune_small_I_sigmas ... All reflection data has been rejected!")
      end if

      ! Create the "good" and "bad" lists and how many of each
      call create_(bad,n)
      call create_(good,n_refl-n)
      g = 0
      b = 0
      do i = 1,n_refl
         if (has_small_I_sigma_(self%reflections(i),TOL(6))) then
            b = b+1
            bad(b) = i
         else
            g = g+1
            good(g) = i
         end if
      end do

      ! Print out
      call flush_(stdout)
      title = "Reflections pruned"
      call text_(stdout,repeat("=",len_trim(title)))
      call text_(stdout,trim(title))
      call text_(stdout,repeat("=",len_trim(title)))

      ! Banner
      call flush_(stdout)
      call show_(stdout,"Rejection criteria      =","has_small_I_sigma")
      call flush_(stdout)
      call show_(stdout,"No. of reflections      =",n_refl)
      call show_(stdout,"No. of rejects          =",b)
      call show_(stdout,"No. of kept reflections =",g)
      call show_(stdout,"Wavelength (Bohr)       =",self%wavelength)
      call show_(stdout,"Wavelength (Angstrom)   =",self%wavelength*ANGSTROM_PER_BOHR)
      if ("I_sigma"=="stl") then
         call show_(stdout,"Cutoff (Bohr^-1)        =",TOL(6))
         call show_(stdout,"Cutoff (Angstrom^-1)    =",TOL(6)/ANGSTROM_PER_BOHR)
      else
         call show_(stdout,"Cutoff                  =",TOL(6))
      end if
      if (self%show_rejects) then
         call flush_(stdout)
         call text_(stdout,"Rejects:")

         ! Get data
         n_refl = size(bad)
         call create_(h1,n_refl); h1 = self%reflections(bad(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(bad(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(bad(:))%h3
         call create_(val,n_refl); val = I_sigma_(self%reflections(bad(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"I_sigma"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
         call flush_(stdout)
         call text_(stdout,"Kept:")

         ! Get data
         n_refl = size(good)
         call create_(h1,n_refl); h1 = self%reflections(good(:))%h1
         call create_(h2,n_refl); h2 = self%reflections(good(:))%h2
         call create_(h3,n_refl); h3 = self%reflections(good(:))%h3
         call create_(val,n_refl); val = I_sigma_(self%reflections(good(:)))

         ! Table
         call create_(table,4)
         call set_heading_(table(1),"h"); call set_values_(table(1),h1)
         call set_heading_(table(2),"k"); call set_values_(table(2),h2)
         call set_heading_(table(3),"l"); call set_values_(table(3),h3)
         call set_heading_(table(4),"I_sigma"); call set_values_(table(4),val)
         call put_(table)

         ! Clean
         call destroy_(table)
         call destroy_(h3)
         call destroy_(h2)
         call destroy_(h1)
         call destroy_(val)
      end if

      ! Copy the non-rejects
      call create_(new_reflections,g)
      new_reflections = self%reflections(good)
      self%reflections = new_reflections
      !!!!!!!!!!!!!!!!!!!!!!

      ! Clean
      call destroy_(bad)
      call destroy_(good)
   end subroutine


! ==============================
! Set scale & extinction factors
! ==============================

   subroutine get_F_optimum_parameters(self)
   ! Get the scale factors, extinction parameters, etc, which minimise
   ! the GoF^2. (To get the corrections corresponding to these
   ! parameters see routine .F_scale_and_extn_correction)
   ! NOTE: not PURE -- the extinction preconditions below must fire in a
   ! release build, and DIE is live there while PURE is a real "pure".
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      if (self%refine_extinction) then
         DIE_IF(use_multiple_scale_factors_(self),"DIFFRACTION_DATA.SET:get_F_optimum_parameters ... extinction needs a single scale factor")
         DIE_IF(self%wavelength<=ZERO,"DIFFRACTION_DATA.SET:get_F_optimum_parameters ... extinction needs a wavelength; set wavelength=")
         call optimize_F_extinction_factor_(self)
      else if (self%refine_scale) then
         if (use_multiple_scale_factors_(self)) then; call optimize_F_scale_factors_(self)
         else; call optimize_F_scale_factor_(self)
         end if
      end if
   end subroutine

   subroutine get_I_optimum_parameters(self)
   ! Get the scale factors, extinction parameters, etc, which minimise
   ! the GoF^2. (To get the corrections corresponding to these
   ! parameters see routine .I_scale_and_extn_correction)
   ! NOTE: not PURE -- see the note on .get_F_optimum_parameters.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      if (self%refine_extinction) then
         DIE_IF(use_multiple_scale_factors_(self),"DIFFRACTION_DATA.SET:get_I_optimum_parameters ... extinction needs a single scale factor")
         DIE_IF(self%wavelength<=ZERO,"DIFFRACTION_DATA.SET:get_I_optimum_parameters ... extinction needs a wavelength; set wavelength=")
         call optimize_I_extinction_factor_(self)
      else if (self%refine_scale) then
         if (use_multiple_scale_factors_(self)) then; call optimize_I_scale_factors_(self)
         else; call optimize_I_scale_factor_(self)
         end if
      end if
   end subroutine

   PURE subroutine optimize_F_scale_factor(self)
   ! Determine the structure factor scale factor to scale the
   ! calculated structure factors .F_calc by in order to minimise the
   ! GoF2 against F_corr, normally F_exp. (But it does not do any
   ! scaling; see .extinction_correction routine for that)
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: top,bot,sg2,F_calc
      INT :: n
      type(REFLECTION_TYPE) :: ref
   ENSURE(have_F_exp_(self),"DIFFRACTION_DATA.SET:optimize_F_scale_factor ... no experimental structure factors")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.SET:optimize_F_scale_factor ... no calculated structure factors")
      top = ZERO
      bot = ZERO
      do n = 1,size(self%reflections)
         ref = self%reflections(n)
         if (self%data_kind=="pnd") then; F_calc = REALIFY(ref%F_calc)
         else; F_calc = abs(ref%F_calc)
         end if
      ! ENSURE(ref.F_sigma/=ZERO,"Structure factor has zero error!")
         sg2 = ONE/(ref%F_sigma*ref%F_sigma)
         top = top+F_calc*ref%F_corr*sg2
         bot = bot+F_calc*F_calc*sg2
      end do
      self%scale_factor = top/bot
   end subroutine

   PURE subroutine optimize_I_scale_factor(self)
   ! Determine the structure factor scale factor to scale the calculated
   ! structure factors .F_calc by in order to minimise the GoF2. (But it does
   ! not do any scaling; see .extinction_correction routine for that)
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: top,bot,sg2,I_calc
      INT :: n,n_refl
      type(REFLECTION_TYPE) :: ref
   ENSURE(have_I_exp_(self),"DIFFRACTION_DATA.SET:optimize_I_scale_factor ... no experimental I's")
   ENSURE(have_I_calc_(self%reflections),"DIFFRACTION_DATA.SET:optimize_I_scale_factor ... no calculated I's")
   ENSURE(self%data_kind/="pnd","DIFFRACTION_DATA.SET:optimize_I_scale_factor ... sorry, noy yet ...")
      n_refl = size(self%reflections)
      top = ZERO
      bot = ZERO
      do n = 1,n_refl
         ref = self%reflections(n)
         I_calc = ref%I_calc
      ! ENSURE(ref.I_sigma/=ZERO,"Structure factor has zero error!")
         sg2 = ONE/(ref%I_sigma*ref%I_sigma)
      ! top = top + I_calc * ref.I_corr * sg2
         top = top+I_calc*ref%I_exp*sg2
         bot = bot+I_calc*I_calc*sg2
      end do
      self%scale_factor = top/bot
   end subroutine

   PURE subroutine optimize_F_scale_factors(self)
   ! Multiple scale factors are determined here ...
   ! Determine the structure factor scale factors to scale the calculated
   ! structure factors .F_calc by in order to minimise the GoF2. (But it does
   ! not do any scaling; see .extinction_correction routine for that)
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: top,bot,F_calc
      INT :: g,n,n_refl,n_group
      type(REFLECTION_TYPE) :: ref
   ENSURE(have_F_exp_(self),"DIFFRACTION_DATA.SET:optimize_F_scale_factors ... no experimental structure factors")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.SET:optimize_F_scale_factors ... no calculated structure factors")
   ENSURE(use_multiple_scale_factors_(self),"DIFFRACTION_DATA.SET:optimize_F_scale_factors ... no reflection group numbers")
      n_group = maxval(self%reflections%group)
      n_refl = size(self%reflections)
      call destroy_(self%scale_factors)
      call create_(self%scale_factors,n_group)
      do g = 1,n_group
         top = ZERO
         bot = ZERO
         do n = 1,n_refl
            ref = self%reflections(n)
            if (ref%group/=g) cycle
            if (self%data_kind=="pnd") then; F_calc = REALIFY(ref%F_calc)
            else; F_calc = abs(ref%F_calc)
            end if

         ! These guys need to be pruned out at the start!
         ! ENSURE(ref.F_sigma/=ZERO,"Structure factor has zero error!")
            top = top+F_calc*ref%F_corr/(ref%F_sigma*ref%F_sigma)
            bot = bot+F_calc*F_calc/(ref%F_sigma*ref%F_sigma)
         end do
         self%scale_factors(g) = top/bot
      end do
   end subroutine

   PURE subroutine optimize_I_scale_factors(self)
   ! Multiple scale factors are determined here ...
   ! Determine I scale factors to scale the calculated
   ! .I_calc by in order to minimise the GoF2I. (But it does
   ! not do any scaling; see .extinction_correction routine for that)
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: top,bot,I_calc
      INT :: g,n,n_refl,n_group
      type(REFLECTION_TYPE) :: ref
   ENSURE(have_I_exp_(self),"DIFFRACTION_DATA.SET:optimize_I_scale_factors ... no experimental I's")
   ENSURE(have_I_calc_(self%reflections),"DIFFRACTION_DATA.SET:optimize_I_scale_factors ... no calculated I's")
   ENSURE(use_multiple_scale_factors_(self),"DIFFRACTION_DATA.SET:optimize_I_scale_factors ... no reflection group numbers")
   ENSURE(self%data_kind/="pnd","DIFFRACTION_DATA.SET:optimize_I_scale_factors ... sorry, noy yet ...")
      n_group = maxval(self%reflections%group)
      n_refl = size(self%reflections)
      call create_(self%scale_factors,n_group)
      do g = 1,n_group
         top = ZERO
         bot = ZERO
         do n = 1,n_refl
            ref = self%reflections(n)
            if (ref%group/=g) cycle
            I_calc = ref%I_calc

         ! These guys need to be pruned out at the start!
         ! ENSURE(ref.I_sigma/=ZERO,"Structure factor has zero error!")

         ! top = top + I_calc * ref.I_corr / (ref.I_sigma * ref.I_sigma)
            top = top+I_calc*ref%I_exp/(ref%I_sigma*ref%I_sigma)
            bot = bot+I_calc*I_calc/(ref%I_sigma*ref%I_sigma)
         end do
         self%scale_factors(g) = top/bot
      end do
   end subroutine

   PURE subroutine optimize_F_extinction_factor(self)
   ! Optimize .scale_factor and .extinction_factor with Larson's method.
   ! NOTE: the corrections are not applied to .F_pred; use the routine
   ! .extinction_corrections to do that.
   ! See: Larson, A. C., in Crystallographic Computin
   !      Ed. Ahmed, F. R. (Copenhagen, Munksgaard 1970), pp. 291-294.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(REAL,2) :: p
      REAL :: GoF2_min

      ! Get starting scale factor
      call optimize_F_scale_factor_(self)

      ! Do a 2-variable minimisation
      ! p(2) is the .extinction_factor
      p(1) = self%scale_factor
      p(2) = ZERO
      call min_BFGS_(self,GoF2F,d_GoF2F,p,GoF2_min,tol=TOL(7),gtol=TOL(7),step=TOL(4))

   ! The minimiser knows nothing about its parameters, so check the
   ! extinction factor here. A negative one is unphysical, and it makes
   ! 1+x|F|^2*A negative, whose fourth root is not a real number.
      ENSURE(p(2)>=ZERO,"DIFFRACTION_DATA.SET:optimize_F_extinction_factor ... negative extinction factor")
      self%scale_factor = p(1)
      self%extinction_factor = p(2)
      self%extinction_factor_esd = F_extinction_factor_esd_(self,p)
   end subroutine

   PURE subroutine optimize_I_extinction_factor(self)
   ! Optimize .scale_factor and .extinction_factor with Larson's method.
   ! NOTE: the corrections are not applied to .F_pred; use the routine
   ! .extinction_corrections to do that.
   ! See: Larson, A. C., in Crystallographic Computing
   !      Ed. Ahmed, F. R. (Copenhagen, Munksgaard 1970), pp. 291-294.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(REAL,2) :: p
      REAL :: GoF2_min

      ! Get starting scale factor
      call optimize_I_scale_factor_(self)

      ! Do a 2-variable minimisation
      ! p(2) is the .extinction_factor
      p(1) = self%scale_factor
      p(2) = ZERO
      call min_BFGS_(self,GoF2I,d_GoF2I,p,GoF2_min,tol=TOL(7),gtol=TOL(7),step=TOL(6))

   ! The minimiser knows nothing about its parameters, so check the
   ! extinction factor here. A negative one is unphysical, and it makes
   ! 1+x|F|^2*A negative, whose fourth root is not a real number.
      ENSURE(p(2)>=ZERO,"DIFFRACTION_DATA.SET:optimize_I_extinction_factor ... negative extinction factor")
      self%scale_factor = p(1)
      self%extinction_factor = p(2)
      self%extinction_factor_esd = I_extinction_factor_esd_(self,p)
   end subroutine


!  ========================================
!  Initialize or update fit/refinement info
!  ========================================

   PURE subroutine initialize_fit_data(self,n_p,n_f,labels)
   ! Create the fit data arrays & initialize if needed.
   ! "n_p" is the number of unique pADP's to refine.
   ! "n_f" is the number of non-unique fragment atom pADP's.
   ! Some of these arrays will already be created if refining.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT, IN :: n_p,n_f
      VEC(STR(len=*),:), IN :: labels

      ! Reset fit data
      call destroy_fit_data_(self)
      call set_fit_data_(self)

      ! Keep the labels, so a near-zero direction can be named
      if (NOT allocated(self%ls)) call create_(self%ls)
      call set_param_labels_(self%ls,labels)

      ! Set fitting
      if (NOT self%refine_structure) call set_fit_structure_(self,TRUE)

      ! Initially GoF^2 is as big as you can get
      self%GoF2_fit0 = huge(ONE)

      ! Set the uique # of parameters. Used for getting the correct
      ! GoF^2 statistic. Note: zero eigenvalues in the least squares
      ! will modify this value.
      self%n_param_structure = n_p
      self%n_param = n_p
      self%n_param_manually_set = FALSE  ! Ignore manually set n_p

      ! Create parameter vectors & shifts (leaky)
      call create_(self%dX,n_p)
      call create_(self%X_fit,n_p)
      call create_(self%X_fit0,n_p)

      ! Create esd's and covariances
      call create_(self%esd,n_p)
   ! .covariance_mx.create(n_p,n_p)
   ! .correlation_mx.create(n_p,n_p)
      call create_(self%fragment_covariance_mx,n_f,n_f)

      ! Set esd's to zero
      self%esd = ZERO

      ! Create the table (leaky)
      call create_(self%fit_table,9)

      ! Fit table headings
      call set_heading_(self%fit_table(1),"Fit")
      call set_subhead_(self%fit_table(1),"Iter")
      call set_heading_(self%fit_table(2),"GoF")
      call set_heading_(self%fit_table(3),"R")
      call set_heading_(self%fit_table(4),"R_w")
      call set_heading_(self%fit_table(5),"Max.")
      call set_subhead_(self%fit_table(5),"Shift")
      call set_sb3head_(self%fit_table(5),"/esd")
      call set_heading_(self%fit_table(6),"Max.")
      call set_subhead_(self%fit_table(6),"Shift")
      call set_sb3head_(self%fit_table(6),"/au")
      call set_heading_(self%fit_table(7),"Max.")
      call set_subhead_(self%fit_table(7),"Shift")
      call set_sb3head_(self%fit_table(7),"param")
      call set_heading_(self%fit_table(8),"No. of")
      call set_subhead_(self%fit_table(8),"params")
      call set_heading_(self%fit_table(9),"No. of")
      call set_subhead_(self%fit_table(9),"eig's")
      call set_sb3head_(self%fit_table(9),"near 0")

      ! Table widths (here because of label width)
      call set_width_from_(self%fit_table(1),self%max_iterations)  ! Iter
      call set_width_from_(self%fit_table(2),TEN**3)  ! GoF
      call set_width_from_(self%fit_table(3),ONE)  ! R
      call set_width_from_(self%fit_table(4),ONE)  ! Rw
      call set_width_from_(self%fit_table(5),-TEN)  ! Max shift/esd
      call set_width_from_(self%fit_table(6),-TEN)  ! Max shift
      call set_width_from_(self%fit_table(7),labels)  ! Max shift par label
      call set_width_(self%fit_table(8),3)  ! n_param
      call set_width_(self%fit_table(9),3)  ! near_0
   end subroutine

   PURE subroutine initialize_refinement_data(self,n_p,labels)
   ! Create the refinement data arrays & initialize if needed
   ! "n_p" is the number of unique pADP's to refine.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT, IN :: n_p
      VEC(STR(len=*),:), IN :: labels

      ! Destroy existing data (leaky); this also zeroes the counters,
      ! so a second refinement in the same job starts clean
      call destroy_refinement_data_(self)
      call set_refinement_data_(self)

      ! Keep the labels, so a near-zero direction can be named
      if (NOT allocated(self%ls)) call create_(self%ls)
      call set_param_labels_(self%ls,labels)

      ! Initially GoF^2 is as big as you can get
      self%GoF2_ref0 = huge(ONE)

      ! Create parameter vectors (leaky)
      call create_(self%X_ref,n_p)
      call create_(self%X_ref0,n_p)

      ! Refinement table headings
      call create_(self%ref_table,11)
      call set_heading_(self%ref_table(1),"Ref.")
      call set_subhead_(self%ref_table(1),"Iter")
      call set_heading_(self%ref_table(2),"No of")
      call set_subhead_(self%ref_table(2),"  Fit")
      call set_sb3head_(self%ref_table(2),"iters")
      call set_heading_(self%ref_table(3),"GoF")
      call set_subhead_(self%ref_table(3),"initial")
      call set_heading_(self%ref_table(4),"GoF")
      call set_subhead_(self%ref_table(4),"final")
      call set_heading_(self%ref_table(5),"R")
      call set_heading_(self%ref_table(6),"R_w")
      call set_heading_(self%ref_table(7),"Max.")
      call set_subhead_(self%ref_table(7),"Shift")
      call set_sb3head_(self%ref_table(7)," /esd")
      call set_heading_(self%ref_table(8),"Max.")
      call set_subhead_(self%ref_table(8),"Shift")
      call set_sb3head_(self%ref_table(8),"/au")
      call set_heading_(self%ref_table(9),"Max.")
      call set_subhead_(self%ref_table(9),"Shift")
      call set_sb3head_(self%ref_table(9),"param")
      call set_heading_(self%ref_table(10),"No. of")
      call set_subhead_(self%ref_table(10),"params")
      call set_heading_(self%ref_table(11),"No. of")
      call set_subhead_(self%ref_table(11),"eig's")
      call set_sb3head_(self%ref_table(11),"near 0")

      ! Table widths (needs to be here because of label width)
      call set_width_from_(self%ref_table(1),self%max_iterations)  ! Iter
      call set_width_from_(self%ref_table(2),self%max_iterations)  ! # of fit iters
      call set_width_from_(self%ref_table(3),TEN**3)  ! GoF initial
      call set_width_from_(self%ref_table(4),TEN**3)  ! GoF final
      call set_width_from_(self%ref_table(5),ONE)  ! R
      call set_width_from_(self%ref_table(6),ONE)  ! Rw
      call set_width_from_(self%ref_table(7),-TEN)  ! Max shift/esd
      call set_width_from_(self%ref_table(8),-TEN)  ! Max shift/esd
      call set_width_from_(self%ref_table(9),labels)  ! Max shift par
      call set_width_(self%ref_table(10),3)  ! n_param
      call set_width_(self%ref_table(11),3)  ! n_param
   end subroutine

   PURE subroutine update_n_param_and_GoF2(self)
   ! Update the number of parameters and recalculate the GoF2
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call update_n_param_(self)
      call update_GoF2_(self)
   end subroutine

   PURE subroutine update_n_param(self)
   ! Update the number of parameters to include the scale and extinction
   ! factors. Always recomputed from .n_param_structure, never incremented
   ! in place: this routine is called once per structure-factor rebuild and
   ! an in-place increment would inflate N_p within a fit iteration.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: n_p

      ! Frozen: set from a CIF, or fixed by a finished refinement
      if (self%n_param_manually_set) return

      ! Structural (pADP) parameters, or none if not refining
      n_p = 0
      if (self%refine_structure OR self%fit_structure) n_p = self%n_param_structure

      ! Scale and extinction
      if (self%refine_extinction) then
         ENSURE(NOT use_multiple_scale_factors_(self),"DIFFRACTION_DATA.SET:update_n_param ... extinction needs a single scale factor")
         n_p = n_p+2
      else if (self%refine_scale) then
         if (use_multiple_scale_factors_(self)) then; n_p = n_p+maxval(self%reflections%group)
         else; n_p = n_p+1
         end if
      end if

      ! NOTE: nothing is added for the XCW Lagrange multiplier lambda. It is
      ! not a least-squares parameter, and the effective number of
      ! wavefunction parameters N_param(lambda) is zero at lambda=0 --
      ! Davidson et al. (2022), Acta Cryst. B78, 312-332, section 2. Holding
      ! N_p at the refinement count is what makes a lambda=0 XCW reproduce
      ! the GoF of the HAR it starts from.
      self%n_param = n_p
   end subroutine

   PURE subroutine update_GoF2(self)
   ! Update the number of parameters and recalculate the GoF2
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: n_r,n_p
   ENSURE(self%n_param>0,"DIFFRACTION_DATA.SET:update_GoF2 ... n_param must be +ve")

      ! Update the GoF2
      n_r = size(self%reflections)-1
      n_p = size(self%reflections)-self%n_param
      self%GoF2 = max(n_r,1)*F_GoF2_(self%reflections)/max(n_p,1)
      self%GoF2_fit = self%GoF2
   end subroutine

   PURE subroutine update_fit_info(self)
   ! Update refinement parameters after fit
      type(DIFFRACTION_DATA_TYPE), INOUT :: self

      ! Converged
      self%fit_converged = abs(self%max_shift_on_esd)<self%tol_for_shift_on_esd AND self%fit_iteration>=self%min_iterations

      ! Florian here: Added TOL(14) due to differences between
      ! machines and also # of CPUs when running tests, as numerical
      ! difference was giving problems, plase tell me if this is
      ! reasonable!!!! (@Dylan and @Peter)
      ! All good, believe you, thanks for the exp. (Dylan)
      self%GoF2_increased = self%GoF2_fit>(self%GoF2_fit0+TOL(14)) AND self%fit_iteration>self%min_iterations

      ! Roll back the parameters is GoF2 increases
      ! WARNING: covariances are not rolled back ...
   ! if (.GoF2_increased) then
   !    .X_fit    = .X_fit0
   !    .GoF2_fit = .GoF2_fit0
   ! end

      ! Too many fit iterations?
      self%too_many_fit_iterations = self%fit_iteration>self%max_iterations

      ! Done?
      self%fit_finished = self%fit_converged OR self%GoF2_increased OR self%too_many_fit_iterations

      ! If refining, store initial & final GoF2
      if (self%refine_structure) then
         if (self%fit_iteration==1) self%GoF2_ref0 = self%GoF2_fit  ! Store initial GoF2
         if (self%fit_finished) self%GoF2_ref = self%GoF2_fit  ! Stoire final GoF2
      end if

      ! Fix the number of parameters
      if (self%fit_finished) self%n_param_manually_set = TRUE
   end subroutine

   PURE subroutine update_fit_parameters(self)
   ! Update i.e. increment the fit parameters
      type(DIFFRACTION_DATA_TYPE), INOUT :: self

      ! Next iteration
      self%fit_iteration = self%fit_iteration+1

      ! Save and increment parameters
      self%X_fit0 = self%X_fit
      self%X_fit = self%X_fit+self%dX
   end subroutine

   PURE subroutine update_refinement_info(self)
   ! Return whether the refinement has finished
      type(DIFFRACTION_DATA_TYPE), INOUT :: self

      ! Has fit finished?
      self%refinement_converged = abs(self%max_refshift_on_esd)<self%tol_for_shift_on_esd AND self%ref_iteration>=self%min_iterations

      ! GoF2 inceased?
      self%GoF2_increased = self%GoF2_ref>(self%GoF2_ref0+TOL(2))

      ! Too many refinement iterations?
      self%too_many_ref_iterations = self%ref_iteration>self%max_iterations
      self%refinement_finished = self%refinement_converged OR self%GoF2_increased OR self%too_many_ref_iterations
   end subroutine

   PURE subroutine update_refinement_parameters(self)
   ! Update the refinement parameters
      type(DIFFRACTION_DATA_TYPE), INOUT :: self

      ! Increment
      self%ref_iteration = self%ref_iteration+1

      ! Transfer parameters
      self%X_ref0 = self%X_ref  ! From previous or 1st iteration
      self%X_ref = self%X_fit  ! This iteration
   end subroutine


!  ===========
!  Update ESDs
!  ===========

   PURE subroutine update_fit_esds(self)
   ! Set the esd's for the current fit.
   ! WARNING: The order of the parameters is not always the same
   ! between calls of this routine. You *must* call this if the
   ! covariance matrix has been transformed from a unique-fragment
   ! atom ordering to asymmetric-unit ordering.
   ! NOTE: Only sensible shifts/ESDs are recorded.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: indmax,p
      REAL :: dXp,val,valmax
   ENSURE(allocated(self%covariance_mx),"DIFFRACTION_DATA.SET:update_fit_esds ... no covariance_mx")
   ENSURE(allocated(self%esd),"DIFFRACTION_DATA.SET:update_fit_esds ... no esd")
   ENSURE(allocated(self%dX),"DIFFRACTION_DATA.SET:update_fit_esds ... no dX")
   ENSURE(size(self%esd)==size(self%covariance_mx,1),"DIFFRACTION_DATA.SET:update_fit_esds ... esd, covariance_mx mismatch")
   ENSURE(size(self%esd)==size(self%dX),"DIFFRACTION_DATA.SET:update_fit_esds ... esd, dX mismatch")

      ! Set variances
      call put_diagonal_to_(self%covariance_mx,self%esd)

   ! stdout.text("variances:")
   ! stdout.put(.esd,by_column=TRUE)

      ! Set ESD's
      self%esd = sqrt(self%esd)

   ! stdout.text("esd:")
   ! stdout.put(.esd,by_column=TRUE)

   ! stdout.text("dX:")
   ! stdout.put(.dX,by_column=TRUE)

   ! W.create(.esd.dim)
   ! where (abs(.esd)<DIFFRACTION_DATA_SMALLEST_SHIFT); W = ZERO
   ! elsewhere;                                         W = .dX/.esd
   ! end
   ! stdout.text("dX/esd:")
   ! stdout.put(W,by_column=TRUE)
   ! W.destroy

      ! Get the largest shift on esd. Pass 1 finds the value.
      valmax = ZERO
      do p = 1,size(self%esd)
         dXp = abs(self%dX(p))
         val = abs(self%esd(p))  ! unnecessary

         ! Ignore symmetry zero's
         ! WARNING: don't know if this is right
         ! Test on dXp doesn't work for anharmonic
         if (val<DIFFRACTION_DATA_SMALLEST_SHIFT) cycle
      ! if (dXp<DIFFRACTION_DATA_SMALLEST_SHIFT) cycle

         ! Shift on esd
         val = dXp/val
         ENSURE(val<DIFFRACTION_DATA_MAX_SHIFT_ON_ESD,"DIFFRACTION_DATA.SET:update_fit_esds ... shift/ESD is too large!")
         if (val>valmax) valmax = val
      end do

      ! Pass 2 takes the LOWEST-INDEXED parameter that reaches that value to
      ! within DIFFRACTION_DATA_SHIFT_TIE_TOL. Symmetry-equivalent parameters
      ! tie here to well below printed precision, and a tie broken by rounding
      ! gives a label that varies with the compiler -- which fails a reference
      ! comparison outright, because a non-numeric token must match exactly.
      ! Two passes rather than a tolerance against a running incumbent: the
      ! latter would let the recorded maximum creep upward by the tolerance at
      ! every step. See macros.in for how the tolerance was calibrated.
      indmax = DIFFRACTION_DATA_INDMAX_INIT
      do p = 1,size(self%esd)
         val = abs(self%esd(p))
         if (val<DIFFRACTION_DATA_SMALLEST_SHIFT) cycle
         val = abs(self%dX(p))/val
         if (val<valmax*(ONE-DIFFRACTION_DATA_SHIFT_TIE_TOL)) cycle
         indmax = p
         exit
      end do

      ! Assign maximum index. Every reported quantity comes from the SAME
      ! parameter, so the row is internally consistent; the value can sit below
      ! valmax, but only by the tie tolerance.
      ENSURE(indmax>0,"DIFFRACTION_DATA.SET:update_fit_esds ... no maximum index found!")
      self%max_shift_on_esd_ind = indmax
      self%max_shift_on_esd = abs(self%dX(indmax))/abs(self%esd(indmax))
      self%max_shift = abs(self%dX(indmax))
      self%max_esd = self%esd(indmax)

      ! Debug
   ! stdout.show("max_shift_on_esd_ind  =",indmax)
   ! stdout.show("max_shift_on_esd      =",valmax)
   end subroutine

   PURE subroutine update_refinement_ESDs(self)
   ! Update the esd's for the current fit.
   ! NOTE: must have updated X_ref and X_ref0
   ! NOTE: Only sensible shifts/ESDs are recorded.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: n_p,indmax,p,n
      REAL :: dXp,val,valmax,valsum
      VEC(REAL,:), allocatable :: dX
   ENSURE(allocated(self%covariance_mx),"DIFFRACTION_DATA.SET:update_refinement_ESDs ... no covariance_mx")
   ENSURE(allocated(self%esd),"DIFFRACTION_DATA.SET:update_refinement_ESDs ... no esd")
   ENSURE(allocated(self%dX),"DIFFRACTION_DATA.SET:update_refinement_ESDs ... no dX")
   ENSURE(size(self%esd)==size(self%covariance_mx,1),"DIFFRACTION_DATA.SET:update_refinement_ESDs ... esd, covariance_mx mismatch")
   ENSURE(size(self%esd)==size(self%dX),"DIFFRACTION_DATA.SET:update_refinement_ESDs ... esd, dX mismatch")
   ENSURE(allocated(self%X_ref),"DIFFRACTION_DATA.SET:update_refinement_ESDs ... no X_ref")
   ENSURE(allocated(self%X_ref0),"DIFFRACTION_DATA.SET:update_refinement_ESDs ... no X_ref0")

      ! Constants
      n_p = size(self%esd)

      ! Change in parameters between refinements
      call create_(dX,n_p)
      dX = self%X_ref-self%X_ref0

      ! Get the location of the maximum shift on esd
      n = 0
      indmax = DIFFRACTION_DATA_INDMAX_INIT
      valmax = ZERO
      valsum = ZERO
      do p = 1,n_p
         dXp = abs(dX(p))
         val = abs(self%esd(p))

         ! Ignore symmetry zero's
         ! Test on dXp mightn't work for anharmonic
         ! WARNING: don't know if this is right
         if (val<DIFFRACTION_DATA_SMALLEST_SHIFT) cycle
      ! if (dXp<DIFFRACTION_DATA_SMALLEST_SHIFT) cycle

         ! Skip ridiculous shift/ESD
         ! Need to add 50 as a macro
         val = dXp/val
      ! ENSURE(val<DIFFRACTION_DATA_MAX_SHIFT_ON_ESD,"shift/ESD is too large!")
         n = n+1
         valsum = valsum+val
         if (val>valmax) valmax = val
      end do

      ! Pass 2 takes the lowest-indexed parameter within the tie tolerance of
      ! the maximum, as update_fit_ESDs does, so that a tie between
      ! symmetry-equivalent parameters does not print a label set by rounding.
      indmax = DIFFRACTION_DATA_INDMAX_INIT
      do p = 1,n_p
         val = abs(self%esd(p))
         if (val<DIFFRACTION_DATA_SMALLEST_SHIFT) cycle
         val = abs(dX(p))/val
         if (val<valmax*(ONE-DIFFRACTION_DATA_SHIFT_TIE_TOL)) cycle
         indmax = p
         exit
      end do

      ! Assign maximum index
      ENSURE(indmax>0,"DIFFRACTION_DATA.SET:update_refinement_ESDs ... no maximum index found!")
      self%max_refshift_on_esd_ind = indmax
      self%ave_refshift_on_esd = valsum/n
      self%max_refshift_on_esd = abs(dX(indmax))/abs(self%esd(indmax))
      self%max_refshift = abs(dX(indmax))
      self%max_refesd = self%esd(indmax)

      ! Debug
   ! stdout.show("max_refshift_on_esd_ind  =",indmax)
   ! stdout.show("max_refshift_on_esd      =",valmax)
   ! .max_refshift_on_esd     = valmax
   ! stdout.text(".esd:")
   ! stdout.put(.esd,by_column=TRUE)

      ! Clean
      call destroy_(dX)
   end subroutine


!  =================
!  Structure factors
!  =================

   subroutine make_F_predicted(self,update_n_param)
   ! Make the predicted magnitude of the structure factors, including
   ! possibly an overall scale factor and extinction correction.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, optional, IN :: update_n_param
      VEC(REAL,:), allocatable :: F_pred
      INT :: n_r,n_p
      BIN :: update
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:make_F_predicted ... no reflection data")
      if (NOT have_F_exp_(self)) then

         ! No experimental data ...
         call create_(F_pred,size(self%reflections))
         if (self%data_kind=="pnd") then; F_pred = REALIFY(self%reflections%F_calc)
         else; F_pred = abs(self%reflections%F_calc)
         end if

         ! Set F_predicted, uscaled
         call set_F_pred_(self%reflections,F_pred)
         call destroy_(F_pred)
      else

         ! Have experimental data ...
         update = TRUE
         if (present(update_n_param)) update = update_n_param

         ! Experimental data exists ...
         if (update) call update_n_param_(self)

         ! Get optimum scale factors, extinction, etc.
         if (self%refine_extinction OR self%refine_scale) call get_F_optimum_parameters_(self)

         ! Make F_predicted; scale_factor multiplies F_calc
         call create_(F_pred,size(self%reflections))
         if (self%data_kind=="pnd") then; F_pred = REALIFY(self%reflections%F_calc)*F_scale_and_extn_correction_(self)
         else; F_pred = abs(self%reflections%F_calc)*F_scale_and_extn_correction_(self)
         end if

         ! Mix in some F_exp
         if (self%SF_mixing_parameter>ZERO) then
            if (have_F_exp_(self)) then
               F_pred = (ONE-self%SF_mixing_parameter)*F_pred+self%SF_mixing_parameter*self%reflections%F_exp
            end if
         end if

         ! Set F_predicted
         call set_F_pred_(self%reflections,F_pred)
         call destroy_(F_pred)

         ! Calculate the GoF^2; reset denominator
         n_r = size(self%reflections)-1
         n_p = size(self%reflections)-self%n_param
         self%GoF2 = max(n_r,1)*F_GoF2_(self%reflections)/max(n_p,1)
         self%GoF2_fit0 = self%GoF2_fit
         self%GoF2_fit = self%GoF2
      end if
   end subroutine

   subroutine make_I_predicted(self,update_n_param)
   ! Make the predicted squared magnitude of the structure factors, including
   ! possibly an overall scale factor and extinction correction.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN, optional, IN :: update_n_param
      VEC(REAL,:), allocatable :: I_pred
      INT :: n_r,n_p
      BIN :: update
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:make_I_predicted ... no reflection data")
   ENSURE(NOT self%data_kind=="pnd","DIFFRACTION_DATA.SET:make_I_predicted ... sorry, no PND yet")
      if (NOT have_F_exp_(self)) then

         ! Set I_predicted
         call set_I_pred_(self%reflections,self%reflections%I_calc)
      else
         update = TRUE
         if (present(update_n_param)) update = update_n_param

         ! Experimental data exists ...
         if (update) call update_n_param_(self)

         ! Get optimum scale factors, extinction, etc.
         if (self%refine_extinction OR self%refine_scale) call get_I_optimum_parameters_(self)

         ! Make I_predicted
         call create_(I_pred,size(self%reflections))
         I_pred = self%reflections%I_calc*I_scale_and_extn_correction_(self)

         ! Mix in some F_exp
         if (self%SF_mixing_parameter>ZERO) then
            if (have_I_exp_(self)) then
               I_pred = (ONE-self%SF_mixing_parameter)*I_pred+self%SF_mixing_parameter*self%reflections%I_exp
            end if
         end if

         ! Set I_predicted
         call set_I_pred_(self%reflections,I_pred)
         call destroy_(I_pred)

         ! Calculate the GoF^2; reset denominator
         n_r = size(self%reflections)-1
         n_p = size(self%reflections)-self%n_param
         self%GoF2 = max(n_r,1)*I_GoF2_(self%reflections)/max(n_p,1)
         self%GoF2_fit0 = self%GoF2_fit
         self%GoF2_fit = self%GoF2
      end if
   end subroutine

   PURE subroutine make_Fourier_coefficients(self,coeff,kind,F_pro,wiener)
   ! Set "coeff(n)" to the Fourier coefficient of reflection "n" for a map
   ! of the given "kind", on absolute scale and with the model's phase:
   !
   !    "residual"          (F_corr - F_pred) exp(i alpha) / ext
   !    "f_exp"              F_corr           exp(i alpha) / ext
   !    "f_calc"             F_pred           exp(i alpha) / ext
   !    "deformation_calc"   F_pred exp(i alpha) / ext - F_pro
   !    "deformation_exp"    F_corr exp(i alpha) / ext - F_pro
   !
   ! "F_pro" are the promolecule structure factors, the spherical atoms at
   ! the model's positions and ADPs, needed for the two deformation kinds.
   ! With "wiener" TRUE each coefficient of an observed kind (residual,
   ! f_exp, deformation_exp) is weighted by F^2/(F^2 + sigma_F^2), which
   ! damps the reflections whose error is of the size of the measurement.
   !
   ! F_corr is the observed |F| (it equals F_exp unless
   ! remove_dispersion_from_F_exp is set), F_pred is the model |F| carrying
   ! the scale and extinction corrections, and "ext" is those corrections,
   ! so dividing by it returns the coefficient to absolute scale. The
   ! residual coefficient is the standard (|F_o| - |F_c|) exp(i alpha_c).
   !
   ! THE PHASE IS DELIBERATELY THE ONE THAT INCLUDES DISPERSION.
   ! F_phase_without_disp is what a residual map free of the anomalous
   ! contribution needs -- but only once F_corr is dispersion-corrected
   ! too, which needs remove_dispersion_from_F_exp. While that flag is off
   ! (its default), F_corr still carries the anomalous signal, so a
   ! dispersion-free phase would give a half-corrected map that neither
   ! convention intends. Switch BOTH together, not one.
   ! See docs/TASK_DISPERSION_CORRECTIONS.md.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(CPX,:), OUT :: coeff
      STR(len=*), IN :: kind
      VEC(CPX,:), optional, IN :: F_pro
      BIN, optional, IN :: wiener
      VEC(REAL,:), allocatable :: ext
      CPX :: phase
      REAL :: F2,s2
      INT :: n
      BIN :: weigh,observed
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:make_Fourier_coefficients ... no reflections")
   ENSURE(size(coeff)==size(self%reflections),"DIFFRACTION_DATA.SET:make_Fourier_coefficients ... coeff: wrong dim")
   ENSURE(present(F_pro) OR (kind/="deformation_calc" AND kind/="deformation_exp"),"DIFFRACTION_DATA.SET:make_Fourier_coefficients ... a deformation kind needs F_pro")
      weigh = FALSE
      if (present(wiener)) weigh = wiener
      observed = kind=="residual" OR kind=="f_exp" OR kind=="deformation_exp"

      ! Per-reflection scale factors & extinction corrections
      call create_(ext,size(self%reflections))
      ext = F_scale_and_extn_correction_(self)
      do n = 1,size(self%reflections)
         phase = F_phase_(self%reflections(n))
         select case (kind)
         case ("residual"); coeff(n) = (self%reflections(n)%F_corr-self%reflections(n)%F_pred)*phase/ext(n)
         case ("f_exp"); coeff(n) = self%reflections(n)%F_corr*phase/ext(n)
         case ("f_calc"); coeff(n) = self%reflections(n)%F_pred*phase/ext(n)
         case ("deformation_calc"); coeff(n) = self%reflections(n)%F_pred*phase/ext(n)-F_pro(n)
         case ("deformation_exp"); coeff(n) = self%reflections(n)%F_corr*phase/ext(n)-F_pro(n)
         case default; coeff(n) = ZERO
         end select

         ! The Wiener weight, for observed coefficients
         if (weigh AND observed) then
            F2 = self%reflections(n)%F_corr**2
            s2 = self%reflections(n)%F_sigma**2
            coeff(n) = coeff(n)*F2/(F2+s2)
         end if
      end do
      call destroy_(ext)
   end subroutine

   PURE subroutine make_symop_generated_coefficients(self,coeff,g1,g2,g3,spacegroup,mult,f)
   ! Expand the per-reflection Fourier coefficients "f" over the whole
   ! sphere of reciprocal space: for each reflection and each symop of
   ! "spacegroup", in that order, set "g1", "g2", "g3" to the symmetry
   ! generated Miller indices and "coeff" to the coefficient with the
   ! symop's translational phase, times the Friedel multiplicity "mult"
   ! (2 when the conjugate half of reciprocal space is absent from the
   ! data, else 1), divided by the number of symops that send a reflection
   ! onto itself or its conjugate, so that no term is counted twice.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(CPX,:), OUT :: coeff
      VEC(INT,:), OUT :: g1,g2,g3
      type(SPACEGROUP_TYPE), IN :: spacegroup
      VEC(INT,:), IN :: mult
      VEC(CPX,:), IN :: f
      MAT(REAL,3,3) :: R
      VEC(REAL,3) :: t
      VEC(INT,3) :: g,h
      INT :: n_refl,n,p0,p,s,ss,Friedel
      CPX :: ci
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.SET:make_symop_generated_coefficients ... no reflections")
   ENSURE(size(coeff)==spacegroup%n_seitz*size(self%reflections),"DIFFRACTION_DATA.SET:make_symop_generated_coefficients ... coeff: wrong dim")
   ENSURE(size(g1)==spacegroup%n_seitz*size(self%reflections),"DIFFRACTION_DATA.SET:make_symop_generated_coefficients ... g1: wrong dim")
   ENSURE(size(g2)==spacegroup%n_seitz*size(self%reflections),"DIFFRACTION_DATA.SET:make_symop_generated_coefficients ... g2: wrong dim")
   ENSURE(size(g3)==spacegroup%n_seitz*size(self%reflections),"DIFFRACTION_DATA.SET:make_symop_generated_coefficients ... g3: wrong dim")
   ENSURE(size(mult)==size(self%reflections),"DIFFRACTION_DATA.SET:make_symop_generated_coefficients ... mult: wrong dim")
   ENSURE(size(f)==size(self%reflections),"DIFFRACTION_DATA.SET:make_symop_generated_coefficients ... f: wrong dim")
      n_refl = size(self%reflections)
      ci = -2*PI*IMAGIFY(ONE)
      p = 0
      do n = 1,n_refl

         ! Unique miller indices
         h = Miller_indices_(self%reflections(n))

         ! Friedel multiplicity
         Friedel = mult(n)

         ! Count the symops that send h onto h or -h. The identity always
         ! counts once; a symop other than the inversion can give -h, e.g.
         ! 2 (0,0,z) acting on [hk0], and that is the Bijvoet pair.
         p0 = p
         ss = 0
         do s = 1,spacegroup%n_seitz

            ! Symop in fractionals
            R = transpose(spacegroup%seitz(1:3,1:3,s))
            t = spacegroup%seitz(1:3,4,s)

            ! Index of symmetry generated reflections
            p = p+1

            ! Symmetry generated miller indices
            g = nint(matmul(R,h))

            ! Count if the same
            if (all(h==g)) ss = ss+1
            if (all(-h==g)) ss = ss+1
            g1(p) = g(1)
            g2(p) = g(2)
            g3(p) = g(3)

            ! The coefficient, with the translation part of the symop
            coeff(p) = f(n)*Friedel
            coeff(p) = coeff(p)*exp(ci*dot_product(h,t))
         end do

         ! Divide by (reciprocal) site symmetry factor
         if (ss>1) coeff(p0+1:p) = coeff(p0+1:p)/REALIFY(ss)
      end do

      ! Divide by the number of data sets
      if (self%n_data_sets>1) then
         coeff = coeff/REALIFY(self%n_data_sets)
      end if
   end subroutine


!   make_symop_generated_dF_a(dF,g1,g2,g3,spacegroup) :: PURE
!   ! Make the full set of symmetry generated reciprocal get phased
!   ! structure factor differences from experimental values
!   ! (without extinction/scale factor correction) for residual map.
!   ! WARNING: reflections ASSUMED UNIQUE here
!      self :: IN
!      dF :: VEC{CPX}, OUT
!      g1,g2,g3 :: VEC{INT}, OUT
!      spacegroup :: SPACEGROUP, IN
!
!   ENSURE(.reflections.allocated,"no reflections")
!   ENSURE(dF.dim1==spacegroup.n_seitz*.reflections.dim,"dF: wrong dim1")
!   ENSURE(g1.dim1==spacegroup.n_seitz*.reflections.dim,"g1: wrong dim1")
!   ENSURE(g2.dim1==spacegroup.n_seitz*.reflections.dim,"g2: wrong dim1")
!   ENSURE(g3.dim1==spacegroup.n_seitz*.reflections.dim,"g3: wrong dim1")
!
!      R :: MAT{REAL}(3,3)
!      t :: VEC{REAL}(3)
!      g,h :: VEC{INT}(3)
!      n_refl,n,p0,p,s, ss :: INT
!      F_calc,F_disp,F_exp,phase,ci :: CPX
!      ext :: VEC{REAL}@
!
!      ! No. of reflection
!      n_refl = .reflections.dim
!
!      ! These are the scale factors & extinction
!      ext.create(n_refl)
!      ext = .F_scale_and_extn_correction
!
!      ! Make the symmetry-generated k points
!      p = 0
!
!      ci = -2*PI*IMAGIFY(ONE)
!
!      do n = 1,n_refl
!
!         ! F_calc phase including dispersion
!         F_calc = .reflections(n).F_calc
!
!         ! Remove dispersion from F_calc & phase
!         F_disp = .reflections(n).F_disp
!         F_calc = F_calc - F_disp    ! Does disp include TF here?
!         phase  = F_calc/abs(F_calc) ! phase without dispersion
!
!         ! Phase and scale F_exp
!         ! WARNING: this uses (potentially) scale factor with
!         ! dispersion included in F_calc .... this is conservative
!         F_exp  = .reflections(n).F_exp*phase/ext(n)
!
!         ! Unique miller indices
!         h(1) = .reflections(n).h
!         h(2) = .reflections(n).k
!         h(3) = .reflections(n).l
!
!         p0 = p
!         ss = 1
!
!         do s = 1,spacegroup.n_seitz
!
!            ! Symop in fractionals
!            R = transpose(spacegroup.seitz(1:3,1:3,s))
!            t = spacegroup.seitz(1:3,4,s) 
!
!            ! Index of symmetry generated reflections
!            p = p + 1
!
!            ! Symmetry generated miller indices
!            g = nint(matmul(R,h))
!
!            ! Count if the same
!            if (all(h==g)) ss = ss + 1
!
!            ! Return
!            g1(p) = g(1)
!            g2(p) = g(2)
!            g3(p) = g(3)
!
!            ! Symmetry generated (Delta F)
!            dF(p) = F_exp - F_calc
!            dF(p) = dF(p)*exp(ci*dot_product(h,t)) ! translation part
!
!         end
!
!         ! Divide by (reciprocal) site symmetry factor
!         if (ss>1) dF(p0+1:p) = dF(p0+1:p)/REALIFY(ss)
!
!      end
!
!      ! Clean
!      ext.destroy
!
!      ! Divide by the number of data sets
!      if (.n_data_sets>1) then
!         dF = dF/REALIFY(.n_data_sets)
!      end
!
!   end

! ================ 
! Normal equations
! ================ 

   subroutine solve_normal_equations_0(self,dFdX)
   ! Get the ".dX" in some parameter vector by solving the normal
   ! equations, given "dFdX", the derivatives of the *absolute*
   ! *unscaled* *unextinction corrected* structure factors |F_calc|
   ! w.r.t. those parameters. 
   !    d |F_calc,i| d
   ! This procedure also sets:
   ! -  ".covariance_mx"
   ! - ".correlation_mx"
   ! - ".near_0",       the no. of LSM eigenvalues nearly equal to
   !                    zero, as determined by ".near_0_tol".
   ! - ".near_0_evals", 
   !   ".near_0_evecs", the eigenvalues and eigenvectors of the 
   !                    LSM nearly equal to zero
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: dFdX
      MAT(REAL,:,:), allocatable :: dF
      VEC(REAL,:), allocatable :: sig,del
      INT :: n_refl,n_p

      ! Constants
      n_refl = size(self%reflections)
      n_p = size(dFdX,2)

      ! LSQ arrays.
      ! In Coppens (1997) equation (4.18) these arrays correspond to:
      ! sig ... the weights w_i, here 1/sigma_i^2
      ! del ... the differences (F_exp,i - s_i*|F_calc,i|) for refine_F
      !     ... the differences (I_exp,i - s_i*|I_calc,i|) for refine_I
      !         where "s_i"
      ! dF  ... the parameter X derivatives s_i* d|F_calc,i|/dX, or
      !             or        X derivatives s_i* d|I_calc,i|/dX, or
      call create_(del,n_refl)
      call create_(sig,n_refl)
      call create_(dF,n_refl,n_p)
      if (self%refine_F) then

         ! Refine on |F_exp|
         call d_F_pred_dX_(self,dF,sig,del,dFdX)
      else

         ! Refine on |I_exp|
         dFdX = TWO*dFdX
         call d_I_pred_dX_(self,dF,sig,del,dFdX)
      end if

      ! Solve the normal equations
      call solve_normal_equations_(self,dF,sig,del)
      call destroy_(dF)
      call destroy_(sig)
      call destroy_(del)
   end subroutine


!   solve_I_normal_equations(dIdX) :: leaky
!   ! Get the ".dX" in some parameter vector by solving the F-normal
!   ! equations, given "dFdX", the derivatives of the square absolute
!   ! structure factors w.r.t. those parameters.
!      self :: INOUT
!      dIdX :: MAT{REAL}, IN
!
!   ENSURE(.have_I_exp, "no experimental structure factors")
!   ENSURE(.reflections.have_I_pred, "no predicted structure factors")
!   ENSURE(.reflections.have_nonzero_I_sigma, "no structure factor errors")
!   ENSURE(dIdX.dim1==.reflections.dim, "wrong size, dFdX")
!   ENSURE(dIdX.dim2==.dX.dim, "incompatible .dX and dIdX")
!
!      A,dI :: MAT{REAL}@
!      rhs,sig,del, eval :: VEC{REAL}@
!      n_refl,n_p,i,j :: INT
!
!      ! Constants
!      n_refl = .reflections.dim
!      n_p    = dIdX.dim2
!
!      ! Arrays
!      A.create(n_p,n_p)
!      rhs.create(n_p)
!      del.create(n_refl)
!      dI.create(n_refl,n_p)
!      sig.create(n_refl)
!
!      ! Make derivatives of I_pred
!      .d_I_pred_dX(dI,sig,del,dIdX)
!
!      ! Checked using FD, setting optimize extinction
!      ! and optimise scale OFF
!    ! stdout.text("dI:")
!    ! stdout.put(dI)
!
!      ! Make the normal matrix
!      do i = 1,n_p
!      do j = 1,n_p
!         A(i,j) = sum(sig*dI(:,i)*dI(:,j))
!      end
!      end
!    ! sig.destroy
!
!      ! Make the rhs
!      do i = 1,n_p
!         rhs(i) = sum(del*dI(:,i))
!      end
!
!    ! dI.destroy
!    ! del.destroy
!
!    ! stdout.text("A:")
!    ! stdout.put(A)
!    ! stdout.text("rhs:")
!    ! stdout.put(rhs)
!
!      ! Solve for the shifts .dX (leaky)

!        ,.correlation_mx)
!
!    ! stdout.text("shifts:")
!    ! stdout.put(.dX)
!    ! stdout.show("Normal equations check =",maxval(abs(matmul(A,.dX)-rhs)))
!    ! stdout.show("Smallest eigenvalue    =",minval(abs(.near_0_evals)))
!    ! stdout.show("near_0_tol             =",.near_0_tol)
!    ! stdout.show("near_0                 =",.near_0)    
!    ! if (.near_0>0) then
!    ! stdout.text("Near_0 eigenvalues:")
!    ! stdout.put(.near_0_evals,by_column=TRUE)
!    ! end
!
!    ! eval.destroy
!
!      ! Get the maximum shift
!      where (abs(.dX)<DIFFRACTION_DATA_SMALLEST_SHIFT) 
!         .dX = ZERO
!      end
!      .max_shift = maxval(abs(.dX))
!
!      ! Set the number of parameters
!      .n_param = n_p - .near_0
!
!      ! Update GoF2
!      .update_n_param_and_GoF2
!
!      ! Set the full covariance matrix
!      .covariance_mx = .GoF2*.covariance_mx
!
!      ! Set the fit errors e.g. max(.dX/esd)
!      ! Parameter ordering is unique fragment atom order.
!      .update_fit_esds
!
!    ! rhs.destroy
!    ! A.destroy
!
!    ! stdout.text("shifts before renormalisation:")
!    ! stdout.put(.dX)
!
!      ! Renormalize shift if too big
!    ! if (.max_shift>.max_allowed_param_shift) then
!    !    fac = .max_allowed_param_shift/.max_shift
!    !    .dX = fac * .dX
!    ! end
!
!    ! stdout.text("shifts after renormalisation:")
!    ! stdout.put(.dX)
!
!   end

   subroutine solve_normal_equations_1(self,dF,sig,del)
   ! Get the ".dX" in some parameter vector by solving the F-normal
   ! equations, given "dF", the derivatives of the absolute structure
   ! factors (or the same squared) w.r.t. those parameters.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: dF
      VEC(REAL,:), IN :: sig
      VEC(REAL,:), IN :: del
      MAT(REAL,:,:), allocatable :: A,dFp
      VEC(REAL,:), allocatable :: rhs,dp
      INT :: n_refl,n_X,n_p,i,j
      REAL :: fac,small
      BIN :: use_J
   ENSURE(size(dF,1)==size(self%reflections),"DIFFRACTION_DATA.SET:solve_normal_equations_1 ... wrong size, dFdX")
   ENSURE(size(dF,2)==size(self%dX),"DIFFRACTION_DATA.SET:solve_normal_equations_1 ... incompatible .dX and dFdX")
   ENSURE(size(sig,1)==size(self%reflections),"DIFFRACTION_DATA.SET:solve_normal_equations_1 ... wrong size, sig")
   ENSURE(size(del,1)==size(self%reflections),"DIFFRACTION_DATA.SET:solve_normal_equations_1 ... wrong size, del")

      ! Constants
      n_refl = size(self%reflections)
      n_X = size(dF,2)

      ! Derivatives with respect to the refined parameters, dF J.
      ! Without a Jacobian every pADP component is a parameter.
      if (NOT allocated(self%ls)) call create_(self%ls)
      use_J = allocated(self%ls%jacobian)
      if (use_J) then
         ENSURE(size(self%ls%jacobian,1)==n_X,"DIFFRACTION_DATA.SET:solve_normal_equations_1 ... wrong dim1, ls.jacobian")
         n_p = size(self%ls%jacobian,2)
         call create_(dFp,n_refl,n_p)
         call to_product_of_(dFp,dF,self%ls%jacobian)
      else
         n_p = n_X
         call create_(dFp,n_refl,n_p)
         dFp = dF
      end if

      ! Arrays
      call create_(A,n_p,n_p)
      call create_(rhs,n_p)

      ! Make the LSQ (normal) matrix
      do i = 1,n_p
         do j = 1,n_p
            A(i,j) = sum(sig*dFp(:,i)*dFp(:,j))
         end do
      end do

      ! Symmetrization is needed to machine precision
      call symmetrize_(A)

      ! Make the rhs
      do i = 1,n_p
         rhs(i) = sum(del*dFp(:,i))
      end do
      call destroy_(dFp)

      ! Solve for the parameter shifts, restrained if weights are set (leaky)
      call create_(dp,n_p)
      call solve_normal_equations_(self%ls,A,rhs,self%near_0_tol,dp)
      call destroy_(rhs)
      call destroy_(A)
      call destroy_(dp)

      ! Back to pADP components: dX = J dp, covariance J C J^T
      call make_X_shift_(self%ls,self%dX)
      call destroy_(self%covariance_mx)
      call make_X_covariance_(self%ls,self%covariance_mx)

      ! Correlation matrix; zero where a parameter has no variance
      call destroy_(self%correlation_mx)
      call create_(self%correlation_mx,n_X,n_X)
      small = tiny(ONE)
      do i = 1,n_X
         do j = 1,n_X
            if (equals_(self%covariance_mx(i,i),ZERO,small)) then; self%correlation_mx(i,j) = ZERO
            else if (equals_(self%covariance_mx(j,j),ZERO,small)) then; self%correlation_mx(i,j) = ZERO
            else; self%correlation_mx(i,j) = self%covariance_mx(i,j)/sqrt(self%covariance_mx(i,i)*self%covariance_mx(j,j))
            end if
         end do
      end do
      if (self%show_near_0_eigenvectors) then
         call text_(stdout,"shifts:")
         call put_(stdout,self%dX)
         call set_real_fortran_fmt_(stdout,"e")
!        stdout.show("Normal equations check =",maxval(abs(matmul(A,.dX)-rhs)))
!        stdout.show("Smallest eigenvalue    =",minval(abs(.near_0_evals)))
         call show_(stdout,"near_0_tol             =",self%near_0_tol)
         call show_(stdout,"near_0                 =",self%ls%near_0)
         call flush_(stdout)
         call set_real_fortran_fmt_(stdout,"f")
         call text_(stdout,"Distribution of values:")
         ! table.put_log_table(eval,.near_0_tol)
         call set_real_fortran_fmt_(stdout,"e")
         call flush_(stdout)
         call text_(stdout,"All eigenvalues:")
         call put_(stdout,self%ls%eigenvalues,by_column=TRUE)
         if (self%ls%near_0>0) then
            call flush_(stdout)
            call text_(stdout,"Near_0 eigenvalues:")
            call put_(stdout,self%ls%near_0_evals,by_column=TRUE)
            call set_real_fortran_fmt_(stdout,"f")
            call flush_(stdout)
            call text_(stdout,"Near_0 eigenvectors:")
            call put_(stdout,self%ls%near_0_evecs)
         end if
      end if

      ! Get the maximum shift
   ! where (abs(.dX)<DIFFRACTION_DATA_SMALLEST_SHIFT) 
   !    .dX = ZERO
   ! end
      self%max_shift = maxval(abs(self%dX))

      ! Set the number of parameters
      self%n_param_structure = n_p-self%ls%n_0-self%ls%near_0
      self%n_param = self%n_param_structure

      ! Update GoF2
      call update_n_param_and_GoF2_(self)

      ! Set the full covariance matrix
      self%covariance_mx = self%GoF2*self%covariance_mx

      ! Set the fit errors e.g. max(.dX/esd)
      ! Parameter ordering is unique fragment atom order.
      call update_fit_esds_(self)

      ! Renormalise shift if too big
      if (self%max_shift>self%max_allowed_param_shift) then
         fac = self%max_allowed_param_shift/self%max_shift
         self%dX = fac*self%dX
      end if
   end subroutine

end module
