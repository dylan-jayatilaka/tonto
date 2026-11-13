!====================================================================
!
! DIFFRACTION_DATA.READ: 
!
! Diffraction data structure for crystals: read & documentation
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
module DIFFRACTION_DATA_READ_MODULE

#  include "macros"

   include "diffraction_data.read.use"

   implicit none

   include "diffraction_data.read.int"


contains

!  ============
!  Read methods
!  ============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","DIFFRACTION_DATA.READ:read_keywords ... expecting open bracket symbol, {")

      ! Read the open braces {

      ! The following code is inherited from OBJECT
      call read_(stdin,word)

      ! Echo keyword if requested
      if (tonto%keyword_echo) then
         call flush_(stdout)
         call text_(stdout,"keyword found --> "//trim(word))
      end if
      do  ! Loop over input keywords

         ! Read the keyword
         call read_(stdin,word)
         DIE_IF(reverted_(stdin),"DIFFRACTION_DATA.READ:read_keywords ... unexpected end of file while reading list keywords")
         call to_lower_case_(word)

         ! Append any lonely = sign
         if (NOT exhausted_(stdin%buffer)) then
            if (next_item_(stdin)=="=") then
               call skip_next_item_(stdin)
               word = trim(word)//"="
            end if
         end if

         ! Echo keyword if requested
         if (tonto%keyword_echo) then
            call flush_(stdout)
            call text_(stdout,"keyword found --> "//trim(word))
         end if

         ! Exit if keyword is closing brace }
         if (word=="}") exit

         ! Process the keyword 
         call process_keyword_(self,word)
      end do
   end subroutine

   subroutine process_keyword(self,keyword)
   ! Process command "keyword". Any required data needed by the
   ! "keyword" is inputted from "stdin".
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      BIN :: die
      word = keyword
      call to_lower_case_(word)
      select case (word)

      case ("}                             ")  ! exit read_loop

      case ("add_dispersion_to_f_calc=     "); call read_add_dispersion_to_F_calc_(self)
      case ("correct_dispersion=           "); call read_add_dispersion_to_F_calc_(self)
      case ("d_low_cutoff=                 "); call read_d_low_cutoff_(self)
      case ("destroy_reflection_data       "); call destroy_reflection_data_(self)
      case ("display_correlation_matrix=   "); call read_show_corrrelation_mx_(self)
      case ("display_near_0_eigenvectors=  "); call read_show_near_0_evecs_(self)
      case ("do_residual_cube=             "); call read_do_residual_cube_(self)
      case ("exp_scale_factor=             "); call read_exp_scale_factor_(self)
      case ("f_calc_cutoff=                "); call read_F_calc_cutoff_(self)
      case ("f_pred_cutoff=                "); call read_F_pred_cutoff_(self)
      case ("f_sigma_cutoff=               "); call read_F_sigma_cutoff_(self)
      case ("f_sigma_noise=                "); call read_F_sigma_noise_(self)
      case ("f_z_cutoff=                   "); call read_F_z_cutoff_(self)
      case ("hkl_range=                    "); call read_hkl_range_(self)
      case ("i_pred_cutoff=                "); call read_I_pred_cutoff_(self)
      case ("i_sigma_cutoff=               "); call read_I_sigma_cutoff_(self)
      case ("i_z_cutoff=                   "); call read_I_z_cutoff_(self)
   ! case ("invariom_database_directory=  "); .read_invariom_directory
   ! case ("invariom_refinement=          "); .read_invariom_refinement
   ! case ("junk=                         "); .read_junk
      case ("kind=                         "); call read_data_kind_(self)
      case ("make_f_predicted              "); call make_F_predicted_(self)
      case ("max_allowed_param_shift=      "); call read_max_allowed_param_shift_(self)
      case ("max_iterations=               "); call read_max_iterations_(self)
      case ("max_prune_iterations=         "); call read_max_prune_iterations_(self)
      case ("min_correlation=              "); call read_min_correlation_(self)
      case ("min_iterations=               "); call read_min_iterations_(self)
   ! case ("nka_t_sequence_no=            "); .read_NKA_T_seq_no
      case ("nn_level=                     "); call read_NN_level_(self)
      case ("n_data_sets=                  "); call read_n_data_sets_(self)
      case ("n_param=                      "); call read_n_param_(self)
      case ("n_worst_reflections=          "); call read_n_worst_reflections_(self)
      case ("near_0_tol=                   "); call read_near_0_tol_(self)
      case ("optimise_extinction=          "); call read_refine_extinction_(self)
      case ("optimize_extinction=          "); call read_refine_extinction_(self)
      case ("optimise_scale_factor=        "); call read_refine_scale_(self)
      case ("optimize_scale_factor=        "); call read_refine_scale_(self)
      case ("optimise_scale_factors=       "); call read_refine_scale_(self)
      case ("optimize_scale_factors=       "); call read_refine_scale_(self)
      case ("partition_model=              "); call read_partition_model_(self)
      case ("exphar_power=                 "); call read_expHAR_power_(self)
      case ("ff_fit_s_max=                 "); call read_FF_fit_s_max_(self)
      case ("ff_fit_n_gaussians=           "); call read_FF_fit_n_gaussians_(self)
      case ("ff_fit_n_points=              "); call read_FF_fit_n_points_(self)
      case ("ff_fit_format=                "); call read_FF_fit_format_(self)
      case ("ri_exponent_ratio=            "); call read_RI_exponent_ratio_(self)
      case ("ri_l_max=                     "); call read_RI_l_max_(self)
   ! case ("prune_reflections             "); .prune_reflections ! Apply automatically?

      !  Put            

      case ("put                           "); call put_(self)
      case ("put_worst_reflections         "); call put_worst_reflections_(self)
      case ("put_sin_theta_on_lambda       "); call put_sin_theta_on_lambda_(self)
      case ("put_wavelength                "); call put_wavelength_(self)

      case ("read_cif                      "); call read_CIF_(self)
      case ("read_fcf_file                 "); call read_fcf_file_(self)
      case ("redirect                      "); call redirect_(self)
      case ("refine_3rd_order_for_atoms=   "); call read_refine_3rd_order_(self)
      case ("refine_4th_order_for_atoms=   "); call read_refine_4th_order_(self)
      case ("refine_4th_order_only=        "); call read_refine_4th_order_only_(self)
      case ("refine_adps_only=             "); call read_refine_ADPs_only_(self)
      case ("refine_anharmonicity=         "); call read_refine_anharmonicity_(self)
      case ("refine_extinction=            "); call read_refine_extinction_(self)
      case ("refine_f=                     "); call read_refine_F_(self)
      case ("refine_h_adps=                "); call read_refine_H_ADPs_(self)
      case ("refine_h_adps_only=           "); call read_refine_H_ADPs_(self)
      case ("refine_h_positions=           "); call read_refine_H_positions_(self)
      case ("refine_h_positions_only=      "); call read_refine_H_positions_(self)
      case ("refine_h_u_iso=               "); call read_refine_H_U_iso_(self)
      case ("adp_model=                    "); call read_ADP_model_(self)
      case ("n_soft_modes=                 "); call read_n_soft_modes_(self)
      case ("soft_mode_restraint=          "); call read_soft_mode_restraint_(self)
      case ("use_soft_mode_correlations=   "); call read_use_soft_mode_correlations_(self)
      case ("correlation_restraint=        "); call read_correlation_restraint_(self)
      case ("use_positive_tls_sigma=       "); call read_use_positive_TLS_sigma_(self)
      case ("refine_nothing_for_atoms=     "); call read_refine_nothing_for_atoms_(self)
      case ("refine_positions_only=        "); call read_refine_positions_only_(self)
      case ("refine_scale_factor=          "); call read_refine_scale_(self)
      case ("refine_scale_factors=         "); call read_refine_scale_(self)
      case ("refine_u_iso_for_atoms=       "); call read_refine_U_iso_for_atoms_(self)
      case ("reflection_data=              "); call read_reflection_data_(self)
      case ("remove_dispersion_from_f_exp= "); call read_remove_disp_from_F_exp_(self)
      case ("restart_har_with_promolecule= "); call read_restart_with_promolecule_(self)
      case ("revert                        "); call revert_(self)
      case ("sf_mixing_parameter=          "); call read_SF_mixing_parameter_(self)
      case ("show_correlation_matrix=      "); call read_show_corrrelation_mx_(self)
      case ("show_fit_output=              "); call read_show_fit_output_(self)
      case ("show_fit_results=             "); call read_show_fit_results_(self)
      case ("show_iam_output=              "); call read_show_IAM_output_(self)
      case ("show_iam_results=             "); call read_show_IAM_results_(self)
      case ("show_near_0_eigenvectors=     "); call read_show_near_0_evecs_(self)
      case ("show_refinement_output=       "); call read_show_ref_output_(self)
      case ("show_refinement_results=      "); call read_show_ref_results_(self)
      case ("show_rejects=                 "); call read_show_rejects_(self)
      case ("stl_high_cutoff=              "); call read_stl_high_cutoff_(self)
      case ("stl_limit=                    "); call read_stl_limit_(self)
      case ("stl_low_cutoff=               "); call read_stl_low_cutoff_(self)
      case ("stop_after_dispersion=        "); call read_stop_after_dispersion_(self)
      case ("synthesize_sigma_i=           "); call read_synthesize_sigma_I_(self)
   ! case ("thermal_smearing_model=       "); .read_temperature_factor_model
      case ("temperature=                  "); call read_temperature_(self)
   ! case ("temperature_factor_model=     "); .read_temperature_factor_model
      case ("tol_for_shift_on_esd=         "); call read_tol_for_shift_on_esd_(self)
      case ("tol_near_0=                   "); call read_near_0_tol_(self)
      case ("use_disk_sfs=                 "); call read_use_disk_SFs_(self)
      case ("use_equivalents=              "); call read_use_equivalents_(self)
      case ("use_iam_itc_ffs=              "); call read_use_IAM_ITC_FFs_(self)
      case ("use_nn=                       "); call read_use_NN_(self)
      case ("use_nn_capping=               "); call read_use_NN_capping_(self)
      case ("use_ryde_capping=             "); call read_use_Ryde_capping_(self)
      case ("use_text_sfs=                 "); call read_use_text_SFs_(self)
      case ("wavelength=                   "); call read_wavelength_(self)

      !  Documentation

      case ("add_dispersion_to_f_calc=?    "); call docu_add_dispersion_to_F_calc_(die)
      case ("correct_dispersion=?          "); call docu_add_dispersion_to_F_calc_(die)
      case ("d_low_cutoff=?                "); call docu_d_low_cutoff_(die)
      case ("destroy_reflection_data?      "); call docu_destroy_reflection_data_(die)
      case ("display_correlation_mx=?      "); call docu_show_corr_matrix_(die)
      case ("display_near_0_eigenvectors=? "); call docu_show_near_0_evecs_(die)
      case ("do_residual_cube=?            "); call docu_do_residual_cube_(die)
      case ("exp_scale_factor=?            "); call docu_exp_scale_factor_(die)
      case ("f_calc_cutoff=?               "); call docu_F_calc_cutoff_(die)
      case ("f_pred_cutoff=?               "); call docu_F_pred_cutoff_(die)
      case ("f_sigma_cutoff=?              "); call docu_F_sigma_cutoff_(die)
      case ("f_sigma_noise=?               "); call docu_F_sigma_noise_(die)
      case ("f_z_cutoff=?                  "); call docu_F_z_cutoff_(die)
      case ("fcf_file=?                    "); call docu_fcf_file_(die)
      case ("hkl_range=?                   "); call docu_hkl_range_(die)
      case ("i_pred_cutoff=?               "); call docu_I_pred_cutoff_(die)
      case ("i_sigma_cutoff=?              "); call docu_I_sigma_cutoff_(die)
      case ("i_z_cutoff=?                  "); call docu_I_z_cutoff_(die)
   ! case ("invariom_database_directory=? "); .docu_invariom_directory(die)
   ! case ("invariom_refinement=?         "); .docu_invariom_refinement(die)
   ! case ("junk=?                        "); .docu_junk(die)
      case ("kind=?                        "); call docu_data_kind_(die)
      case ("make_f_predicted?             "); call docu_make_F_predicted_(die)
      case ("max_allowed_param_shift=?     "); call docu_max_allowed_param_shift_(die)
      case ("max_iterations=?              "); call docu_max_iterations_(die)
      case ("max_prune_iterations=?        "); call docu_max_prune_iterations_(die)
      case ("min_correlation=?             "); call docu_min_correlation_(die)
      case ("min_iterations=?              "); call docu_min_iterations_(die)
   ! case ("nka_t_sequence_no=?           "); .docu_NKA_T_seq_no(die)
      case ("nn_level=?                    "); call docu_NN_level_(die)
      case ("n_data_sets=?                 "); call docu_n_data_sets_(die)
      case ("n_param=?                     "); call docu_n_param_(die)
      case ("n_worst_reflections=?         "); call docu_n_worst_reflections_(die)
      case ("near_0_tol=?                  "); call docu_near_0_tol_(die)
      case ("optimize_scale_factors=?      "); call docu_refine_scale_(die)
      case ("optimise_extinction=?         "); call docu_refine_extinction_(die)
      case ("optimize_extinction=?         "); call docu_refine_extinction_(die)
      case ("optimise_scale_factor=?       "); call docu_refine_scale_(die)
      case ("optimize_scale_factor=?       "); call docu_refine_scale_(die)
      case ("optimise_scale_factors=?      "); call docu_refine_scale_(die)
      case ("refine_extinction=?           "); call docu_refine_extinction_(die)
      case ("refine_scale_factor=?         "); call docu_refine_scale_(die)
      case ("refine_scale_factors=?        "); call docu_refine_scale_(die)
      case ("partition_model=?             "); call docu_partition_model_(die)
      case ("exphar_power=?                "); call docu_expHAR_power_(die)
      case ("ff_fit_s_max=?                "); call docu_FF_fit_(die)
      case ("ff_fit_n_gaussians=?          "); call docu_FF_fit_(die)
      case ("ff_fit_n_points=?             "); call docu_FF_fit_(die)
      case ("ff_fit_format=?               "); call docu_FF_fit_(die)
      case ("ri_exponent_ratio=?           "); call docu_RI_exponent_ratio_(die)
      case ("ri_l_max=?                    "); call docu_RI_l_max_(die)
   ! case ("prune_reflections             "); .prune_reflections ! Apply automatically?(die)
      case ("put?                          "); call docu_put_(die)
      case ("put_worst_reflections?        "); call docu_put_worst_reflections_(die)
      case ("put_sin_theta_on_lambda?      "); call docu_put_sin_theta_on_lambda_(die)
      case ("put_wavelength?               "); call docu_put_wavelength_(die)
      case ("read_cif?                     "); call docu_CIF_(die)
      case ("redirect?                     "); call docu_redirect_(die)
      case ("refine_3rd_order_for_atoms=?  "); call docu_refine_3rd_order_(die)
      case ("refine_4th_order_for_atoms=?  "); call docu_refine_4th_order_(die)
      case ("refine_4th_order_only=?       "); call docu_refine_4th_order_only_(die)
      case ("refine_adps_only=?            "); call docu_refine_ADPs_only_(die)
      case ("refine_anharmonicity=?        "); call docu_refine_anharmonicity_(die)
      case ("refine_f=?                    "); call docu_refine_F_(die)
      case ("refine_h_adps=?               "); call docu_refine_H_ADPs_only_(die)
      case ("refine_h_adps_only=?          "); call docu_refine_H_ADPs_only_(die)
      case ("refine_h_positions=?          "); call docu_refine_H_positions_only_(die)
      case ("refine_h_positions_only=?     "); call docu_refine_H_positions_only_(die)
      case ("adp_model=?                   "); call docu_ADP_model_(die)
      case ("refine_h_u_iso=?              "); call docu_refine_H_U_iso_(die)
      case ("refine_nothing_for_atoms=?    "); call docu_refine_nothing_for_atoms_(die)
      case ("refine_positions_only=?       "); call docu_refine_positions_only_(die)
      case ("refine_u_iso_for_atoms=?      "); call docu_refine_U_iso_for_atoms_(die)
      case ("reflection_data=?             "); call docu_reflection_data_(die)
      case ("remove_dispersion_from_f_exp=?"); call docu_remove_disp_from_F_exp_(die)
      case ("restart_har_with_promolecule=?"); call docu_restart_with_promolecule_(die)
      case ("revert?                       "); call docu_revert_(die)
      case ("sf_mixing_parameter=?         "); call docu_SF_mixing_parameter_(die)
      case ("show_fit_output=?             "); call docu_show_fit_output_(die)
      case ("show_fit_results=?            "); call docu_show_fit_results_(die)
      case ("show_iam_output=?             "); call docu_show_IAM_output_(die)
      case ("show_iam_results=?            "); call docu_show_IAM_results_(die)
      case ("show_refinement_output=?      "); call docu_show_ref_output_(die)
      case ("show_refinement_results=?     "); call docu_show_ref_results_(die)
      case ("show_rejects=?                "); call docu_show_rejects_(die)
      case ("stl_high_cutoff=?             "); call docu_stl_high_cutoff_(die)
      case ("stl_low_cutoff=?              "); call docu_stl_low_cutoff_(die)
      case ("stl_limit=?                   "); call docu_stl_limit_(die)
   ! case ("stop_after_dispersion=?       "); .docu_stop_after_dispersion(die)
   ! case ("synthesize_sigma_i=?          "); .docu_synthesize_sigma_I(die)
   ! case ("thermal_smearing_model=?      "); .docu_temperature_factor_model(die)
   ! case ("temperature=?                 "); .docu_temperature(die)
   ! case ("temperature_factor_model=?    "); .docu_temperature_factor_model(die)
      case ("tol_for_shift_on_esd=?        "); call docu_tol_for_shift_on_esd_(die)
      case ("tol_near_0=?                  "); call docu_near_0_tol_(die)
      case ("use_disk_sfs=?                "); call docu_use_disk_SFs_(die)
      case ("use_equivalents=?             "); call docu_use_equivalents_(die)
      case ("use_iam_itc_ffs=?             "); call docu_use_IAM_ITC_FFs_(die)
      case ("use_nn=?                      "); call docu_use_NN_(die)
      case ("use_nn_capping=?              "); call docu_use_NN_capping_(die)
      case ("use_ryde_capping=?            "); call docu_use_Ryde_capping_(die)
      case ("use_text_sfs=?                "); call docu_use_text_SFs_(die)
      case ("wavelength=?                  "); call docu_wavelength_(die)
      case ("help?                         "); call docu_help_

      case default; allocate(tonto%known_keywords(198))
      tonto%known_keywords(1) = "}                             "
      tonto%known_keywords(2) = "add_dispersion_to_f_calc=     "
      tonto%known_keywords(3) = "correct_dispersion=           "
      tonto%known_keywords(4) = "d_low_cutoff=                 "
      tonto%known_keywords(5) = "destroy_reflection_data       "
      tonto%known_keywords(6) = "display_correlation_matrix=   "
      tonto%known_keywords(7) = "display_near_0_eigenvectors=  "
      tonto%known_keywords(8) = "do_residual_cube=             "
      tonto%known_keywords(9) = "exp_scale_factor=             "
      tonto%known_keywords(10) = "f_calc_cutoff=                "
      tonto%known_keywords(11) = "f_pred_cutoff=                "
      tonto%known_keywords(12) = "f_sigma_cutoff=               "
      tonto%known_keywords(13) = "f_sigma_noise=                "
      tonto%known_keywords(14) = "f_z_cutoff=                   "
      tonto%known_keywords(15) = "hkl_range=                    "
      tonto%known_keywords(16) = "i_pred_cutoff=                "
      tonto%known_keywords(17) = "i_sigma_cutoff=               "
      tonto%known_keywords(18) = "i_z_cutoff=                   "
      tonto%known_keywords(19) = "kind=                         "
      tonto%known_keywords(20) = "make_f_predicted              "
      tonto%known_keywords(21) = "max_allowed_param_shift=      "
      tonto%known_keywords(22) = "max_iterations=               "
      tonto%known_keywords(23) = "max_prune_iterations=         "
      tonto%known_keywords(24) = "min_correlation=              "
      tonto%known_keywords(25) = "min_iterations=               "
      tonto%known_keywords(26) = "nn_level=                     "
      tonto%known_keywords(27) = "n_data_sets=                  "
      tonto%known_keywords(28) = "n_param=                      "
      tonto%known_keywords(29) = "n_worst_reflections=          "
      tonto%known_keywords(30) = "near_0_tol=                   "
      tonto%known_keywords(31) = "optimise_extinction=          "
      tonto%known_keywords(32) = "optimize_extinction=          "
      tonto%known_keywords(33) = "optimise_scale_factor=        "
      tonto%known_keywords(34) = "optimize_scale_factor=        "
      tonto%known_keywords(35) = "optimise_scale_factors=       "
      tonto%known_keywords(36) = "optimize_scale_factors=       "
      tonto%known_keywords(37) = "partition_model=              "
      tonto%known_keywords(38) = "exphar_power=                 "
      tonto%known_keywords(39) = "ff_fit_s_max=                 "
      tonto%known_keywords(40) = "ff_fit_n_gaussians=           "
      tonto%known_keywords(41) = "ff_fit_n_points=              "
      tonto%known_keywords(42) = "ff_fit_format=                "
      tonto%known_keywords(43) = "ri_exponent_ratio=            "
      tonto%known_keywords(44) = "ri_l_max=                     "
      tonto%known_keywords(45) = "put                           "
      tonto%known_keywords(46) = "put_worst_reflections         "
      tonto%known_keywords(47) = "put_sin_theta_on_lambda       "
      tonto%known_keywords(48) = "put_wavelength                "
      tonto%known_keywords(49) = "read_cif                      "
      tonto%known_keywords(50) = "read_fcf_file                 "
      tonto%known_keywords(51) = "redirect                      "
      tonto%known_keywords(52) = "refine_3rd_order_for_atoms=   "
      tonto%known_keywords(53) = "refine_4th_order_for_atoms=   "
      tonto%known_keywords(54) = "refine_4th_order_only=        "
      tonto%known_keywords(55) = "refine_adps_only=             "
      tonto%known_keywords(56) = "refine_anharmonicity=         "
      tonto%known_keywords(57) = "refine_extinction=            "
      tonto%known_keywords(58) = "refine_f=                     "
      tonto%known_keywords(59) = "refine_h_adps=                "
      tonto%known_keywords(60) = "refine_h_adps_only=           "
      tonto%known_keywords(61) = "refine_h_positions=           "
      tonto%known_keywords(62) = "refine_h_positions_only=      "
      tonto%known_keywords(63) = "refine_h_u_iso=               "
      tonto%known_keywords(64) = "adp_model=                    "
      tonto%known_keywords(65) = "n_soft_modes=                 "
      tonto%known_keywords(66) = "soft_mode_restraint=          "
      tonto%known_keywords(67) = "use_soft_mode_correlations=   "
      tonto%known_keywords(68) = "correlation_restraint=        "
      tonto%known_keywords(69) = "use_positive_tls_sigma=       "
      tonto%known_keywords(70) = "refine_nothing_for_atoms=     "
      tonto%known_keywords(71) = "refine_positions_only=        "
      tonto%known_keywords(72) = "refine_scale_factor=          "
      tonto%known_keywords(73) = "refine_scale_factors=         "
      tonto%known_keywords(74) = "refine_u_iso_for_atoms=       "
      tonto%known_keywords(75) = "reflection_data=              "
      tonto%known_keywords(76) = "remove_dispersion_from_f_exp= "
      tonto%known_keywords(77) = "restart_har_with_promolecule= "
      tonto%known_keywords(78) = "revert                        "
      tonto%known_keywords(79) = "sf_mixing_parameter=          "
      tonto%known_keywords(80) = "show_correlation_matrix=      "
      tonto%known_keywords(81) = "show_fit_output=              "
      tonto%known_keywords(82) = "show_fit_results=             "
      tonto%known_keywords(83) = "show_iam_output=              "
      tonto%known_keywords(84) = "show_iam_results=             "
      tonto%known_keywords(85) = "show_near_0_eigenvectors=     "
      tonto%known_keywords(86) = "show_refinement_output=       "
      tonto%known_keywords(87) = "show_refinement_results=      "
      tonto%known_keywords(88) = "show_rejects=                 "
      tonto%known_keywords(89) = "stl_high_cutoff=              "
      tonto%known_keywords(90) = "stl_limit=                    "
      tonto%known_keywords(91) = "stl_low_cutoff=               "
      tonto%known_keywords(92) = "stop_after_dispersion=        "
      tonto%known_keywords(93) = "synthesize_sigma_i=           "
      tonto%known_keywords(94) = "temperature=                  "
      tonto%known_keywords(95) = "tol_for_shift_on_esd=         "
      tonto%known_keywords(96) = "tol_near_0=                   "
      tonto%known_keywords(97) = "use_disk_sfs=                 "
      tonto%known_keywords(98) = "use_equivalents=              "
      tonto%known_keywords(99) = "use_iam_itc_ffs=              "
      tonto%known_keywords(100) = "use_nn=                       "
      tonto%known_keywords(101) = "use_nn_capping=               "
      tonto%known_keywords(102) = "use_ryde_capping=             "
      tonto%known_keywords(103) = "use_text_sfs=                 "
      tonto%known_keywords(104) = "wavelength=                   "
      tonto%known_keywords(105) = "add_dispersion_to_f_calc=?    "
      tonto%known_keywords(106) = "correct_dispersion=?          "
      tonto%known_keywords(107) = "d_low_cutoff=?                "
      tonto%known_keywords(108) = "destroy_reflection_data?      "
      tonto%known_keywords(109) = "display_correlation_mx=?      "
      tonto%known_keywords(110) = "display_near_0_eigenvectors=? "
      tonto%known_keywords(111) = "do_residual_cube=?            "
      tonto%known_keywords(112) = "exp_scale_factor=?            "
      tonto%known_keywords(113) = "f_calc_cutoff=?               "
      tonto%known_keywords(114) = "f_pred_cutoff=?               "
      tonto%known_keywords(115) = "f_sigma_cutoff=?              "
      tonto%known_keywords(116) = "f_sigma_noise=?               "
      tonto%known_keywords(117) = "f_z_cutoff=?                  "
      tonto%known_keywords(118) = "fcf_file=?                    "
      tonto%known_keywords(119) = "hkl_range=?                   "
      tonto%known_keywords(120) = "i_pred_cutoff=?               "
      tonto%known_keywords(121) = "i_sigma_cutoff=?              "
      tonto%known_keywords(122) = "i_z_cutoff=?                  "
      tonto%known_keywords(123) = "kind=?                        "
      tonto%known_keywords(124) = "make_f_predicted?             "
      tonto%known_keywords(125) = "max_allowed_param_shift=?     "
      tonto%known_keywords(126) = "max_iterations=?              "
      tonto%known_keywords(127) = "max_prune_iterations=?        "
      tonto%known_keywords(128) = "min_correlation=?             "
      tonto%known_keywords(129) = "min_iterations=?              "
      tonto%known_keywords(130) = "nn_level=?                    "
      tonto%known_keywords(131) = "n_data_sets=?                 "
      tonto%known_keywords(132) = "n_param=?                     "
      tonto%known_keywords(133) = "n_worst_reflections=?         "
      tonto%known_keywords(134) = "near_0_tol=?                  "
      tonto%known_keywords(135) = "optimize_scale_factors=?      "
      tonto%known_keywords(136) = "optimise_extinction=?         "
      tonto%known_keywords(137) = "optimize_extinction=?         "
      tonto%known_keywords(138) = "optimise_scale_factor=?       "
      tonto%known_keywords(139) = "optimize_scale_factor=?       "
      tonto%known_keywords(140) = "optimise_scale_factors=?      "
      tonto%known_keywords(141) = "refine_extinction=?           "
      tonto%known_keywords(142) = "refine_scale_factor=?         "
      tonto%known_keywords(143) = "refine_scale_factors=?        "
      tonto%known_keywords(144) = "partition_model=?             "
      tonto%known_keywords(145) = "exphar_power=?                "
      tonto%known_keywords(146) = "ff_fit_s_max=?                "
      tonto%known_keywords(147) = "ff_fit_n_gaussians=?          "
      tonto%known_keywords(148) = "ff_fit_n_points=?             "
      tonto%known_keywords(149) = "ff_fit_format=?               "
      tonto%known_keywords(150) = "ri_exponent_ratio=?           "
      tonto%known_keywords(151) = "ri_l_max=?                    "
      tonto%known_keywords(152) = "put?                          "
      tonto%known_keywords(153) = "put_worst_reflections?        "
      tonto%known_keywords(154) = "put_sin_theta_on_lambda?      "
      tonto%known_keywords(155) = "put_wavelength?               "
      tonto%known_keywords(156) = "read_cif?                     "
      tonto%known_keywords(157) = "redirect?                     "
      tonto%known_keywords(158) = "refine_3rd_order_for_atoms=?  "
      tonto%known_keywords(159) = "refine_4th_order_for_atoms=?  "
      tonto%known_keywords(160) = "refine_4th_order_only=?       "
      tonto%known_keywords(161) = "refine_adps_only=?            "
      tonto%known_keywords(162) = "refine_anharmonicity=?        "
      tonto%known_keywords(163) = "refine_f=?                    "
      tonto%known_keywords(164) = "refine_h_adps=?               "
      tonto%known_keywords(165) = "refine_h_adps_only=?          "
      tonto%known_keywords(166) = "refine_h_positions=?          "
      tonto%known_keywords(167) = "refine_h_positions_only=?     "
      tonto%known_keywords(168) = "adp_model=?                   "
      tonto%known_keywords(169) = "refine_h_u_iso=?              "
      tonto%known_keywords(170) = "refine_nothing_for_atoms=?    "
      tonto%known_keywords(171) = "refine_positions_only=?       "
      tonto%known_keywords(172) = "refine_u_iso_for_atoms=?      "
      tonto%known_keywords(173) = "reflection_data=?             "
      tonto%known_keywords(174) = "remove_dispersion_from_f_exp=?"
      tonto%known_keywords(175) = "restart_har_with_promolecule=?"
      tonto%known_keywords(176) = "revert?                       "
      tonto%known_keywords(177) = "sf_mixing_parameter=?         "
      tonto%known_keywords(178) = "show_fit_output=?             "
      tonto%known_keywords(179) = "show_fit_results=?            "
      tonto%known_keywords(180) = "show_iam_output=?             "
      tonto%known_keywords(181) = "show_iam_results=?            "
      tonto%known_keywords(182) = "show_refinement_output=?      "
      tonto%known_keywords(183) = "show_refinement_results=?     "
      tonto%known_keywords(184) = "show_rejects=?                "
      tonto%known_keywords(185) = "stl_high_cutoff=?             "
      tonto%known_keywords(186) = "stl_low_cutoff=?              "
      tonto%known_keywords(187) = "stl_limit=?                   "
      tonto%known_keywords(188) = "tol_for_shift_on_esd=?        "
      tonto%known_keywords(189) = "tol_near_0=?                  "
      tonto%known_keywords(190) = "use_disk_sfs=?                "
      tonto%known_keywords(191) = "use_equivalents=?             "
      tonto%known_keywords(192) = "use_iam_itc_ffs=?             "
      tonto%known_keywords(193) = "use_nn=?                      "
      tonto%known_keywords(194) = "use_nn_capping=?              "
      tonto%known_keywords(195) = "use_ryde_capping=?            "
      tonto%known_keywords(196) = "use_text_sfs=?                "
      tonto%known_keywords(197) = "wavelength=?                  "
      tonto%known_keywords(198) = "help?                         "
      call unknown_(tonto,word,"DIFFRACTION_DATA.READ:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine


! Reading

   subroutine docu_help()
   ! Documentation.
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"=========================")
      call text_(stdout,"Help for DIFFRACTION_DATA")
      call text_(stdout,"=========================")
      call docu_add_dispersion_to_F_calc_
      call docu_d_low_cutoff_
      call docu_destroy_reflection_data_
      call docu_show_corr_matrix_
      call docu_show_near_0_evecs_
      call docu_do_residual_cube_
      call docu_exp_scale_factor_
      call docu_F_calc_cutoff_
      call docu_F_pred_cutoff_
      call docu_F_sigma_cutoff_
      call docu_F_sigma_noise_
      call docu_F_z_cutoff_
      call docu_hkl_range_
      call docu_I_pred_cutoff_
      call docu_I_sigma_cutoff_
      call docu_I_z_cutoff_
   ! .docu_invariom_directory
   ! .docu_invariom_refinement
   ! .docu_junk
      call docu_data_kind_
      call docu_make_F_predicted_
      call docu_max_allowed_param_shift_
      call docu_max_iterations_
      call docu_max_prune_iterations_
      call docu_min_correlation_
      call docu_min_iterations_
   ! .docu_NKA_T_seq_no
      call docu_NN_level_
      call docu_n_data_sets_
      call docu_n_param_
      call docu_n_worst_reflections_
      call docu_near_0_tol_
      call docu_refine_extinction_
      call docu_refine_scale_
      call docu_partition_model_
      call docu_expHAR_power_
      call docu_FF_fit_
      call docu_RI_exponent_ratio_
      call docu_RI_l_max_
!   ! .prune_reflections ! Apply automatically?
      call docu_put_
      call docu_put_worst_reflections_
      call docu_put_sin_theta_on_lambda_
      call docu_put_wavelength_
      call docu_CIF_
      call docu_redirect_
      call docu_refine_3rd_order_
      call docu_refine_4th_order_
      call docu_refine_4th_order_only_
      call docu_refine_ADPs_only_
      call docu_refine_anharmonicity_
      call docu_refine_F_
      call docu_refine_H_ADPs_only_
      call docu_refine_H_positions_only_
      call docu_ADP_model_
      call docu_refine_H_U_iso_
      call docu_refine_nothing_for_atoms_
      call docu_refine_positions_only_
      call docu_refine_U_iso_for_atoms_
      call docu_reflection_data_
      call docu_remove_disp_from_F_exp_
      call docu_restart_with_promolecule_
      call docu_revert_
      call docu_SF_mixing_parameter_
      call docu_show_fit_output_
      call docu_show_fit_results_
      call docu_show_ref_output_
      call docu_show_ref_results_
      call docu_show_rejects_
      call docu_stl_high_cutoff_
      call docu_stl_low_cutoff_
      call docu_stl_limit_
   ! .docu_stop_after_dispersion
   ! .docu_synthesize_sigma_I
   ! .docu_temperature_factor_model
   ! .docu_temperature
      call docu_tol_for_shift_on_esd_
      call docu_use_disk_SFs_
      call docu_use_IAM_ITC_FFs_
      call docu_use_NN_
      call docu_use_NN_capping_
      call docu_use_Ryde_capping_
      call docu_use_text_SFs_
      call docu_wavelength_
      DIE("DIFFRACTION_DATA.READ:docu_help ... end of help documentation")
   end subroutine


! A

   subroutine docu_add_dispersion_to_F_calc(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"correct_dispersion=")
      call text_(stdout,"add_dispersion_to_F_calc= BIN [FALSE]")
      call flush_(stdout)
      call text_(stdout,". Set whether to add atom-based anomalous dispersion")
      call text_(stdout,"  corrections to the calculated structure factors, F_calc")
      if (present(die)) stop
   end subroutine

   subroutine read_add_dispersion_to_F_calc(self)
   ! Read whether to add dispersion to F_calc.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_add_dispersion_to_F_calc_(self,val)
      DIE_IF(self%remove_dispersion_from_F_exp,"DIFFRACTION_DATA.READ:read_add_dispersion_to_F_calc ... you already asked to remove dispersion from F_exp!")
   end subroutine


! D

   subroutine docu_d_low_cutoff(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"d_low_cutoff= REAL")
      call flush_(stdout)
      call text_(stdout,". Set the interplanar d spacing distance below which")
      call text_(stdout,"  reflections are removed from the data set")
      call flush_(stdout)
      call text_(stdout,". Must be a positive value")
      call text_(stdout,"  Initial negative value denotes feature is unused")
      if (present(die)) stop
   end subroutine

   subroutine read_d_low_cutoff(self)
   ! Read the d_low_cutoff (spacings below this are pruned) which is
   ! converted to a stl_high_cutoff for prune_high_angle_reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: d
      call read_quantity_(stdin,d)
      self%stl_high_cutoff = HALF/d
      ENSURE(self%stl_high_cutoff>=ZERO,"DIFFRACTION_DATA.READ:read_d_low_cutoff ... must be positive")
   end subroutine

   subroutine docu_destroy_reflection_data(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"destroy_reflection_data")
      call flush_(stdout)
      call text_(stdout,". Delete all reflection data, including the original")
      call text_(stdout,"  reflection before any pruning, and reset all scale")
      call text_(stdout,"  factors, extinction factors applied to correct |F_calc|")
      call text_(stdout,"  or modify F_exp| to default values.")
      call flush_(stdout)
      call text_(stdout,". Also, delete the measurement-group scale_factors.")
      call flush_(stdout)
      call text_(stdout,". Also, delete any hkl_range= data")
      if (present(die)) stop
   end subroutine

   subroutine docu_show_corr_matrix(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"show_correlation_mx=    BIN [FALSE]")
      call text_(stdout,"display_correlation_mx= BIN [FALSE]")
      call flush_(stdout)
      call text_(stdout,". Set whether to display the correlation matrix between")
      call text_(stdout,"  the least-squares refined crystal structure parameters")
      call flush_(stdout)
      call text_(stdout,". See also min_correlation=")
      call flush_(stdout)
      call text_(stdout,". Correlations with scale factors, extinction parameters")
      call text_(stdout,"  are not reported as they are refined outside of the LSQ")
      call text_(stdout,"  procedure in order to permit wavefunction fitting")
      call flush_(stdout)
      call text_(stdout,". See Davidson et al (2023) Acta Cryst B 72 p. 312-32")
      if (present(die)) stop
   end subroutine

   subroutine read_show_corrrelation_mx(self)
   ! Read whether to display the correlation matrix.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%show_correlation_mx)
   end subroutine

   subroutine docu_show_near_0_evecs(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"show_near_0_eigenvector=    BIN [FALSE]")
      call text_(stdout,"display_near_0_eigenvector= BIN [FALSE]")
      call flush_(stdout)
      call text_(stdout,". Read whether to display the eigenvectors of the")
      call text_(stdout,"  least-squares matrix corresponding to eigenvalues")
      call text_(stdout,"  which are nearly equal to zero")
      call flush_(stdout)
      call text_(stdout,". See also near_0_tol= ")
      if (present(die)) stop
   end subroutine

   subroutine read_show_near_0_evecs(self)
   ! Read whether to display the near 0 eigenvectros of the correlation matrix.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%show_near_0_eigenvectors)
   end subroutine

   subroutine docu_do_residual_cube(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"do_residual_cube= BIN [TRUE]")
      call flush_(stdout)
      call text_(stdout,". Set whether to calculate the residual density in the")
      call text_(stdout,"  unit cell, to obtain the maximum, minimu and root-mean-")
      call text_(stdout,"  -square values for the electron density fit")
      call flush_(stdout)
      call text_(stdout,". This calculation can take some time, so for exploratory")
      call text_(stdout,"  work it is best to set this option FALSE")
      if (present(die)) stop
   end subroutine

   subroutine read_do_residual_cube(self)
   ! Read whether to write the residual density gaussian cube file.
   ! Can be used to speed up writing.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%do_residual_cube)
   end subroutine


! E

   subroutine docu_exp_scale_factor(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"exp_scale_factor= REAL")
      call flush_(stdout)
      call text_(stdout,". A fixed scale factor which is applied to the")
      call text_(stdout,"  experimental structure factors, F_exp")
      call flush_(stdout)
      call text_(stdout,". Once applied, the F_exp values are changed forever")
      call flush_(stdout)
      call text_(stdout,". Negative initial value indicates this scaling is")
      call text_(stdout,"  not performed")
      if (present(die)) stop
   end subroutine

   subroutine read_exp_scale_factor(self)
   ! Read the structure factor multiplier
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%exp_scale_factor)
   end subroutine


! F

   subroutine docu_F_calc_cutoff(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"F_calc_cutoff= REAL [TOL(3)] ")
      call flush_(stdout)
      call text_(stdout,". Set autoff for |F_calc| values, used to prune out small")
      call text_(stdout,"  calculated structure factors.")
      call flush_(stdout)
      call text_(stdout,". Must be a positive value")
      call text_(stdout,". Initial negative value denotes feature is unused")
      call flush_(stdout)
      call text_(stdout,". Useful for removing systematic absences from the data;")
      call text_(stdout,"  essential for least-squares structure refinement to avoid")
      call text_(stdout,"  potential divide-by-zero errors.")
      if (present(die)) stop
   end subroutine

   subroutine read_F_calc_cutoff(self)
   ! Read the F_calc_cutoff for prune_F_calcs
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_F_calc_cutoff_(self,val)
   end subroutine

   subroutine docu_F_pred_cutoff(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"F_pred_cutoff= REAL")
      call flush_(stdout)
      call text_(stdout,". Set cutoff for |F_pred| values, used to prune out small")
      call text_(stdout,"  calculated structure factors.")
      call flush_(stdout)
      call text_(stdout,". Must be a positive value")
      call flush_(stdout)
      call text_(stdout,". Useful for removing systematic absences from the data;")
      call text_(stdout,"  essential for least-squares structure refinement to avoid")
      call text_(stdout,"  potential divide-by-zero errors.")
      if (present(die)) stop
   end subroutine

   subroutine read_F_pred_cutoff(self)
   ! Read the F_pred_cutoff for prune_F_preds
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_F_pred_cutoff_(self,val)
   end subroutine

   subroutine docu_F_sigma_cutoff(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"F_sigma_cutoff= REAL")
      call flush_(stdout)
      call text_(stdout,". Set cutoff for |F_exp/F_sigma| values, used to prune out")
      call text_(stdout,". reflections where this quantity is too *small*")
      call flush_(stdout)
      call text_(stdout,". Must be a positive value")
      if (present(die)) stop
   end subroutine

   subroutine read_F_sigma_cutoff(self)
   ! Read the maximum acceptable value for F_cutoff
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_F_sigma_cutoff_(self,val)
   end subroutine

   subroutine docu_F_sigma_noise(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"F_sigma_noise= REAL [ZERO] ")
      call flush_(stdout)
      call text_(stdout,". If set, gaussian random noise is added to every experimental")
      call text_(stdout,"  structure factor F_exp with standard deviation F_sigma.")
      call flush_(stdout)
      call text_(stdout,". Must be a positive value")
      call flush_(stdout)
      call text_(stdout,". Once set, the F_exp values are changed forever")
      if (present(die)) stop
   end subroutine

   subroutine read_F_sigma_noise(self)
   ! Read noise to add to F_exp and F_sigma, in units of F_sigma.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_F_sigma_noise_(self,val)
   end subroutine

   subroutine docu_F_z_cutoff(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"F_z_cutoff= REAL ")
      call flush_(stdout)
      call text_(stdout,". Set cutoff for |F_exp/F_sigma| values, used to prune out")
      call text_(stdout,"  reflections where this quantity, the z value, is too *large*")
      call flush_(stdout)
      call text_(stdout,". Must be a positive value")
      call flush_(stdout)
      call text_(stdout,". Used for pruning *bad* or *outlier* F_pred values")
      if (present(die)) stop
   end subroutine

   subroutine read_F_z_cutoff(self)
   ! Read the F_z cutoff for prune_bad_reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_F_z_cutoff_(self,val)
   end subroutine

   subroutine docu_fcf_file(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"fcf_file= STR ")
      call flush_(stdout)
      call text_(stdout,". If set, specifies the fcf file in the IUCr format from")
      call text_(stdout,"  which reflections will be read.")
      call flush_(stdout)
      call text_(stdout,". The file must end in .fcf")
      if (present(die)) stop
   end subroutine

   subroutine read_fcf_file(self)
   ! Read the name of the fcf file from which to read in a unique set
   ! of reflections *and immediately read it in*.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      call set_fcf_file_name_(self,val)
      call process_fcf_file_(self)
   end subroutine


! H

   subroutine docu_hkl_range(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"hkl_range= VEC{EVEC{INT}}@ ")
      call flush_(stdout)
      call text_(stdout,". If set, specifies a range of Miller indices (h k l)")
      call flush_(stdout)
      call text_(stdout,"     { { hmin hmax } { kmin kmax } { lmin lmax } } ")
      call flush_(stdout)
      call text_(stdout,"  for which reflections are generated.")
      call flush_(stdout)
      call text_(stdout,". Useful for obtaining calculated structure factors, F_calc")
      if (present(die)) stop
   end subroutine

   subroutine read_hkl_range(self)
   ! Read the F_pred_cutoff for prune_F_preds
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call destroy_(self%hkl_range)
      call read_all_(stdin,self%hkl_range)
   end subroutine


! I

   subroutine docu_I_pred_cutoff(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"I_pred_cutoff= REAL")
      call flush_(stdout)
      call text_(stdout,". Set cutoff for |I_pred| values, also known as F2 values,")
      call text_(stdout,". used to prune out small-valued reflection intensities.")
      call flush_(stdout)
      call text_(stdout,". Must be a positive value")
      call flush_(stdout)
      call text_(stdout,". Useful for removing systematic absences from the data;")
      call text_(stdout,"  essential for least-squares structure refinement to avoid")
      call text_(stdout,"  potential divide-by-zero errors.")
      if (present(die)) stop
   end subroutine

   subroutine read_I_pred_cutoff(self)
   ! Read the I_pred_cutoff for prune_I_preds
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_I_pred_cutoff_(self,val)
   end subroutine

   subroutine docu_I_sigma_cutoff(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"I_sigma_cutoff= REAL [-FOUR] ")
      call flush_(stdout)
      call text_(stdout,". Set cutoff for |I_exp/I_sigma| values, used to prune out")
      call text_(stdout,". reflections where this quantity is too *small*")
      call flush_(stdout)
      call text_(stdout,". Must be a positive value")
      call text_(stdout,"  Initial negative value denotes this feature is unused")
      if (present(die)) stop
   end subroutine

   subroutine read_I_sigma_cutoff(self)
   ! Read the maximum acceptable value for I_cutoff
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_I_sigma_cutoff_(self,val)
   end subroutine

   subroutine docu_I_z_cutoff(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"I_z_cutoff= REAL")
      call flush_(stdout)
      call text_(stdout,". Set cutoff for |I_exp/I_sigma| values, used to prune out")
      call text_(stdout,"  reflections where this quantity, the z value, is too *large*")
      call flush_(stdout)
      call text_(stdout,". Must be a positive value")
      call flush_(stdout)
      call text_(stdout,". Used for pruning *bad* or *outlier* F_pred values")
      if (present(die)) stop
   end subroutine

   subroutine read_I_z_cutoff(self)
   ! Read the I_z cutoff for prune_bad_reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_I_z_cutoff_(self,val)
   end subroutine


!   read_invariom_directory :: private
!   ! Read in the directory for the invariom database.
!      self :: INOUT
!
!      stdin.read(.invariom_database_directory)
!
!   end

!   read_invariom_refinement :: private
!   ! Read in the logical switch for the invariom refinement.
!      self :: INOUT
!
!      stdin.read(.invariom_refinement)
!
!   end

! K

   subroutine docu_data_kind(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"kind= STR")
      call flush_(stdout)
      call text_(stdout,". Read the kind of diffractioin reflections which are")
      call text_(stdout,"  to be input.")
      call flush_(stdout)
      call text_(stdout,". Allowed values: xray, pnd")
      if (present(die)) stop
   end subroutine

   subroutine read_data_kind(self)
   ! Read the data kind of crystal experiment
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      call set_data_kind_(self,val)
   end subroutine


! N

   subroutine docu_make_F_predicted(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"make_f_predicted")
      call flush_(stdout)
      call text_(stdout,". Make the predicted structure factors F_pred, including")
      call text_(stdout,"  any scale factros, extinction, and anomaloud dispersion")
      call text_(stdout,"  corrections, if they were requested.")
      call flush_(stdout)
      call text_(stdout,". A set of experimental structure factors must be present")
      if (present(die)) stop
   end subroutine

   subroutine docu_max_allowed_param_shift(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"max_allowed_param_shift= REAL [25.0]")
      call flush_(stdout)
      call text_(stdout,". Set maximum allowed change in a least-squares refined")
      call text_(stdout,"  crystal structure refinement.")
      call flush_(stdout)
      call text_(stdout,". If any one of the parameters shifts obtained from solving")
      call text_(stdout,"  the least-squares equations is greater than this value, it")
      call text_(stdout,"  is rescaled so that it becomes this value.")
      call flush_(stdout)
      call text_(stdout,". Must be a positive value.")
      call flush_(stdout)
      call text_(stdout,". Note: this cutoff is applied equally to all parameter")
      call text_(stdout,"  types, so it is governed by the average magnitude of the")
      call text_(stdout,"  smallest quantities, in atomic units.")
      if (present(die)) stop
   end subroutine

   subroutine read_max_allowed_param_shift(self)
   ! Read the maximum allowed shift in a structure refinement
   ! parameter
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%max_allowed_param_shift)
      ENSURE(self%max_allowed_param_shift>ZERO,"DIFFRACTION_DATA.READ:read_max_allowed_param_shift ... not positive")
   end subroutine

   subroutine docu_max_iterations(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"max_iteration= INT [100]")
      call flush_(stdout)
      call text_(stdout,". Set maximum number of iterations allowed to solve the")
      call text_(stdout,"  least-squares equations for the crystal structure parameters.")
      call flush_(stdout)
      call text_(stdout,". Must be a non-negative value.")
      call flush_(stdout)
      call text_(stdout,". Must be greater than value set for min_interations=")
      if (present(die)) stop
   end subroutine

   subroutine read_max_iterations(self)
   ! Read the maximum number of iterations
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%max_iterations)
      ENSURE(self%max_iterations>-1,"DIFFRACTION_DATA.READ:read_max_iterations ... max_iterations < 0 !")
      ENSURE(self%max_iterations>self%min_iterations,"DIFFRACTION_DATA.READ:read_max_iterations ... max_iterations <= min_iterations !")
   end subroutine

   subroutine docu_max_prune_iterations(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"max_prune_iterations= INT ")
      call flush_(stdout)
      call text_(stdout,". Set maximum number of iterations allowed for pruning")
      call text_(stdout,"  *bad* reflections via the f_z_cutoff= option. An iterative")
      call text_(stdout,"  process must be used since the scaling and extinction")
      call text_(stdout,"  corrections must be recalculated every time some")
      call text_(stdout,"  reflections are pruned from the data.")
      call flush_(stdout)
      call text_(stdout,". Must be a positive value.")
      if (present(die)) stop
   end subroutine

   subroutine read_max_prune_iterations(self)
   ! Read the maximum number of pruning iterations
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%max_prune_iterations)
      ENSURE(self%max_prune_iterations>0,"DIFFRACTION_DATA.READ:read_max_prune_iterations ... max_prune_iterations must be +ve")
   end subroutine

   subroutine docu_min_iterations(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"min_iterations= INT [2]")
      call flush_(stdout)
      call text_(stdout,". Set minimum number of iterations allowed to solve the")
      call text_(stdout,"  least-squares equations for the crystal structure parameters.")
      call flush_(stdout)
      call text_(stdout,". Must be a non-negative value.")
      call flush_(stdout)
      call text_(stdout,". Must be strictly less than value set for max_interations=")
      if (present(die)) stop
   end subroutine

   subroutine read_min_iterations(self)
   ! Read the minimum number of iterations
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%min_iterations)
      ENSURE(self%min_iterations>-1,"DIFFRACTION_DATA.READ:read_min_iterations ... min_iterations < 0 !")
      ENSURE(self%max_iterations>self%min_iterations,"DIFFRACTION_DATA.READ:read_min_iterations ... min_iterations >= max_iterations !")
   end subroutine

   subroutine docu_min_correlation(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"min_correlation= REAL [0.6] ")
      call flush_(stdout)
      call text_(stdout,". The minimum value for the absolute value of the")
      call text_(stdout,"  correlation matrix elements, below which they are")
      call text_(stdout,"  not printed out in the output.")
      call flush_(stdout)
      call text_(stdout,". Must be in the range (-ONE,ONE)")
      if (present(die)) stop
   end subroutine

   subroutine read_min_correlation(self)
   ! Read the minimum number of iterations
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%min_correlation)
      ENSURE(self%min_correlation>=-ONE,"DIFFRACTION_DATA.READ:read_min_correlation ... min_iterations < -1 !")
      ENSURE(self%min_correlation<=ONE,"DIFFRACTION_DATA.READ:read_min_correlation ... min_iterations > +1 !")
   end subroutine


! N

   subroutine docu_n_data_sets(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"n_data_sets= INT [1] ")
      call flush_(stdout)
      call text_(stdout,". Set the number of data sets used in a multiple-")
      call text_(stdout,"  -data-set least-squares crystal structure refinement")
      call flush_(stdout)
      call text_(stdout,". The different dara sets must be assigned different")
      call text_(stdout,"  scale group indices.")
      call flush_(stdout)
      call text_(stdout,". Value must by greater than 0.")
      call flush_(stdout)
      call text_(stdout,". Used to scale the residual density plots correctly")
      if (present(die)) stop
   end subroutine

   subroutine read_n_data_sets(self)
   ! Read the no of data sets used in a multiple-data-set refinement.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      call set_n_data_sets_(self,val)
   end subroutine

   subroutine docu_n_param(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"n_param= INT [1] ")
      call flush_(stdout)
      call text_(stdout,". Set the number of independent parameters in the least-")
      call text_(stdout,"  squares crystal structure refinement.")
      call flush_(stdout)
      call text_(stdout,". Must be a a non-negative value")
      call flush_(stdout)
      call text_(stdout,". Useful in XWR calculations which do not immediately")
      call text_(stdout,"  a Hirshfeld atom refinement, so that the GoF2 values")
      call text_(stdout,"  at lambda=ZERO are consistent.")
      if (present(die)) stop
   end subroutine

   subroutine read_n_param(self)
   ! Read the no of parameters for refinement explicitly.
   ! DO this if you are doing an XCW after an HAR
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      call set_n_param_(self,val)
   end subroutine

   subroutine docu_n_worst_reflections(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"n_worst reflections= INT [40] ")
      call flush_(stdout)
      call text_(stdout,". Set the number of *worst* agreeing predicted structure")
      call text_(stdout,"  factors, F_pred, when compared to F_exp, based on the")
      call text_(stdout,"  value of |F_pred - F_exp|/F_sigma, the Delta F_z value")
      call flush_(stdout)
      call text_(stdout,". Default is 40, but may be smaller if there are very")
      call text_(stdout,". few reflections")
      call flush_(stdout)
      call text_(stdout,". Must be a a non-negative value")
      if (present(die)) stop
   end subroutine

   subroutine read_n_worst_reflections(self)
   ! Read the no of worst reflections to show.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      call set_n_worst_reflections_(self,val)
   end subroutine


! O

   subroutine docu_refine_extinction(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"optimise_extinction=")
      call text_(stdout,"refine_extinction= BIN [FALSE] ")
      call flush_(stdout)
      call text_(stdout,". Set whether to use an extinction correctgion according")
      call text_(stdout,"  to the old Zachariasen model")
      call flush_(stdout)
      call text_(stdout,". See Zachariasen  (1967) Acta Cryst. 23 p. 558; also")
      call text_(stdout,"  Becker & Coppens (1974) Acta Cryst. A 30, p.  148")
      call flush_(stdout)
      call text_(stdout,". Consider whether you really need ths option for small")
      call text_(stdout,"  crystals, or those that do not have strongly absorbing")
      call text_(stdout,"  atoms. It is not clear to Dylan whether such a correctiom")
      call text_(stdout,"  interferes with path-length, or other corrections, which")
      call text_(stdout,"  may have already been applied.")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_extinction(self)
   ! Read the switch whether to correct extinction or not, according to the
   ! Larson formula
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_refine_extinction_(self,val)
   end subroutine

   subroutine docu_refine_scale(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"optimize_scale=")
      call text_(stdout,"optimize_scale_factor=")
      call text_(stdout,"optimize_scale_factors=")
      call text_(stdout,"refine_scale=")
      call text_(stdout,"refine_scale_factor=")
      call text_(stdout,"refine_scale_factors= BIN [TRUE]")
      call flush_(stdout)
      call text_(stdout,". Set whether to use a scale factor (or scale factors,")
      call text_(stdout,"  associated with each reflection group index) to fit")
      call text_(stdout,"  the calculated structure factor magnitudes |F_calc|")
      call text_(stdout,"  to the experimentally measured structure factors, F_exp")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_scale(self)
   ! Read the switch whether to use and overall scale factor to minimise
   ! the GoF2 statistic when calculating the structure factors.
   ! NOTE: this is not the same at the overall .scale_factor which is applied
   ! to the experimental structure factors.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%refine_scale)
   end subroutine


! P

   subroutine docu_partition_model(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"partition_model= STR ")
      call flush_(stdout)
      call text_(stdout,". Set the model used to partition the electron density")
      call text_(stdout,"  into atomic or diatomic fragments in order to perform")
      call text_(stdout,"  thermal smearing")
      call flush_(stdout)
      call text_(stdout,". Allowed values are (the oc- prefix may be left off):")
      call text_(stdout,"     - oc-hirshfeld,   Hirshfeld (stockholder) atoms")
      call text_(stdout,"     - oc-g-hirshfeld, Hirshfeld atoms from interpolated densities")
      call text_(stdout,"     - oc-salvador,    Salvador atoms (also: tfva)")
      call text_(stdout,"     - oc-g-salvador,  Salvador atoms from interpolated densities")
      call text_(stdout,"     - oc-crystal23,   Hirshfeld atoms from a CRYSTAL23 wavefunction")
      call text_(stdout,"     - oc-tfvp,        Salvador atoms, with the Mayer-Salvador radii")
      call text_(stdout,"                       taken from the promolecule density")
      call text_(stdout,"     - oc-tfvh,        Salvador atoms, with each boundary where the")
      call text_(stdout,"                       two atoms' spherical densities are equal")
      call text_(stdout,"     - oc-sph-tfva,    spherically averaged Salvador atoms.")
      call text_(stdout,"                       EXPERIMENTAL: for molecules with C, N, O it")
      call text_(stdout,"                       fits worse and may not converge")
      call text_(stdout,"     - oc-sph-tfvp,    as oc-sph-tfva, with the radii of oc-tfvp.")
      call text_(stdout,"                       EXPERIMENTAL, as oc-sph-tfva")
      call text_(stdout,"     - oc-sph-tfvh,    as oc-sph-tfva, with the radii of oc-tfvh.")
      call text_(stdout,"                       EXPERIMENTAL, as oc-sph-tfva")
      call text_(stdout,"     - oc-exphar,      exponential Hirshfeld atoms: the weight is")
      call text_(stdout,"                       rho_A^n / sum_B rho_B^n of the free-atom")
      call text_(stdout,"                       densities, n from expHAR_power= (2).")
      call text_(stdout,"                       Chodkiewicz & Wozniak, IUCrJ 12, 74 (2025)")
      call text_(stdout,"     - oc-sph-exphar,  spherically averaged oc-exphar atoms.")
      call text_(stdout,"                       EXPERIMENTAL, as oc-sph-tfva")
      call text_(stdout,"     - oc-ri,          Hirshfeld atoms fitted in functions on")
      call text_(stdout,"                       each atom, so that their transform is")
      call text_(stdout,"                       analytic. See RI_l_max= and")
      call text_(stdout,"                       RI_exponent_ratio=. EXPERIMENTAL")
      call text_(stdout,"     - tc-coppens, tc-stewart, tc-tanaka, two-center models,")
      call text_(stdout,"                       not for structure refinement")
      call text_(stdout,"     - none")
      call flush_(stdout)
      call text_(stdout,". See Davidson et al (2023) Acta Cryst B 72 p. 312-32")
      if (present(die)) stop
   end subroutine

   subroutine docu_RI_l_max(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"RI_l_max= INT [7]")
      call flush_(stdout)
      call text_(stdout,". The highest angular momentum of the functions in which")
      call text_(stdout,"  each Hirshfeld atom is fitted, for partition_model= oc-ri.")
      call flush_(stdout)
      call text_(stdout,". 7 reproduces Hirshfeld atom refinement to about 1 mA in")
      call text_(stdout,"  bonds to hydrogen: Chodkiewicz et al (2024) IUCrJ 11.")
      if (present(die)) stop
   end subroutine

   subroutine read_RI_l_max(self)
   ! Read the highest angular momentum of the oc-ri fitting functions.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      DIE_IF(val<0,"DIFFRACTION_DATA.READ:read_RI_l_max ... RI_l_max= must not be negative")
      call set_RI_l_max_(self,val)
   end subroutine

   subroutine docu_RI_exponent_ratio(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"RI_exponent_ratio= REAL [1.5]")
      call flush_(stdout)
      call text_(stdout,". The ratio of successive exponents of the functions in")
      call text_(stdout,"  which each Hirshfeld atom is fitted, for partition_model=")
      call text_(stdout,"  oc-ri. The exponents run from twice the largest basis")
      call text_(stdout,"  exponent on the atom down to its smallest one.")
      call flush_(stdout)
      call text_(stdout,". A smaller ratio gives more functions and a better fit,")
      call text_(stdout,"  until the fit becomes ill-conditioned.")
      if (present(die)) stop
   end subroutine

   subroutine docu_expHAR_power(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"expHAR_power= REAL [2]")
      call flush_(stdout)
      call text_(stdout,". The power n to which each free-atom density is raised in")
      call text_(stdout,"  the weight of partition_model= oc-exphar and oc-sph-exphar,")
      call text_(stdout,"  w_A = rho_A^n / sum_B rho_B^n. n = 1 is the Hirshfeld")
      call text_(stdout,"  partition; a larger n makes the atoms overlap less.")
      call text_(stdout,"  Chodkiewicz & Wozniak recommend 2.")
      if (present(die)) stop
   end subroutine

   subroutine read_expHAR_power(self)
   ! Read the power of the free-atom densities in the oc-exphar weight.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      DIE_IF(val<=ZERO,"DIFFRACTION_DATA.READ:read_expHAR_power ... expHAR_power= must be positive")
      call set_expHAR_power_(self,val)
   end subroutine

   subroutine docu_FF_fit(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"FF_fit_s_max= REAL [2]")
      call text_(stdout,"FF_fit_n_gaussians= INT [4]")
      call text_(stdout,"FF_fit_n_points= INT [201]")
      call text_(stdout,"FF_fit_format= STR [shelx]")
      call flush_(stdout)
      call text_(stdout,". Settings of fit_sph_atom_FFs, which fits the spherical form")
      call text_(stdout,"  factor of every unique atom of a sph- partition model to")
      call text_(stdout,"  n Gaussians and a constant, f(s) = sum a_i exp(-b_i s^2) + c,")
      call text_(stdout,"  on n_points of s = sin(theta)/lambda in [0,s_max] 1/Angstrom,")
      call text_(stdout,"  through gnuplot, starting from the International Tables")
      call text_(stdout,"  coefficients of the element. Only 4 Gaussians so far.")
      call text_(stdout,"  The coefficients are written to <name>.sph_ff.<format>:")
      call text_(stdout,"  shelx, one SFAC instruction per atom; or table, one line")
      call text_(stdout,"  per atom.")
      if (present(die)) stop
   end subroutine

   subroutine read_FF_fit_s_max(self)
   ! Read the range of the Gaussian fit of spherical form factors.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      DIE_IF(val<=ZERO,"DIFFRACTION_DATA.READ:read_FF_fit_s_max ... FF_fit_s_max= must be positive")
      call set_FF_fit_s_max_(self,val)
   end subroutine

   subroutine read_FF_fit_n_gaussians(self)
   ! Read the number of Gaussians in the fit of spherical form factors.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      DIE_IF(val<1,"DIFFRACTION_DATA.READ:read_FF_fit_n_gaussians ... FF_fit_n_gaussians= must be at least 1")
      call set_FF_fit_n_gaussians_(self,val)
   end subroutine

   subroutine read_FF_fit_n_points(self)
   ! Read the number of s points the spherical form factors are fitted on.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      DIE_IF(val<10,"DIFFRACTION_DATA.READ:read_FF_fit_n_points ... FF_fit_n_points= must be at least 10")
      call set_FF_fit_n_points_(self,val)
   end subroutine

   subroutine read_FF_fit_format(self)
   ! Read the output format of the fitted form-factor coefficients.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      call to_lower_case_(val)
      DIE_IF(NOT is_one_of_(val,["shelx","table"]),"DIFFRACTION_DATA.READ:read_FF_fit_format ... FF_fit_format= must be shelx or table")
      call set_FF_fit_format_(self,val)
   end subroutine

   subroutine read_RI_exponent_ratio(self)
   ! Read the ratio of successive exponents of the oc-ri fitting functions.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      DIE_IF(val<=ONE,"DIFFRACTION_DATA.READ:read_RI_exponent_ratio ... RI_exponent_ratio= must be greater than 1")
      call set_RI_exponent_ratio_(self,val)
   end subroutine

   subroutine read_partition_model(self)
   ! Read the partition model to used to correct for oversampled fragments
   ! of the unit cell when calculating the structure factors
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      call set_partition_model_(self,val)
   end subroutine

   subroutine docu_put(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"put")
      call flush_(stdout)
      call text_(stdout,". Output information concerning the data object to")
      call text_(stdout,"  the output file, usually stdout")
      if (present(die)) stop
   end subroutine

   subroutine docu_put_worst_reflections(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"put_worst_reflections")
      call flush_(stdout)
      call text_(stdout,". Output information concerning the *worst* or")
      call text_(stdout,"  *most disagreeable* reflections, based on the")
      call text_(stdout,"  magnitude of Delta F_z = |F_pred-F_exp|/F_sigma.")
      if (present(die)) stop
   end subroutine

   subroutine docu_put_sin_theta_on_lambda(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"put_sin_theta_on_lambda")
      call flush_(stdout)
      call text_(stdout,". Output a table of sin(theta)/lambda for every reflection")
      call flush_(stdout)
      call text_(stdout,". Ensure that wavelength= has been specified correctly")
      if (present(die)) stop
   end subroutine

   subroutine docu_put_wavelength(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"put_wavelength")
      call flush_(stdout)
      call text_(stdout,". Output the wavelength of the particles used for diffraction")
      call text_(stdout,"  in *Angstrom* units")
      if (present(die)) stop
   end subroutine


! R

   subroutine docu_CIF(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"read_CIF STR ")
      call flush_(stdout)
      call text_(stdout,". Read the name of a Crystallographic Inforemation File (CIF)")
      call text_(stdout,"  and then oprn it and read the reflections from it.")
      call flush_(stdout)
      call text_(stdout,". Note: this command has non-standard form since it expects")
      call text_(stdout,"  additional data on the input line")
      if (present(die)) stop
   end subroutine

   subroutine docu_redirect(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"redirect STR ")
      call flush_(stdout)
      call text_(stdout,". Read the name of a new input file, and start reading")
      call text_(stdout,"  commands from it.")
      call flush_(stdout)
      call text_(stdout,". Note: this command has non-standard form since it expects")
      call text_(stdout,"  additional data on the input line; however this command")
      call text_(stdout,"  also appears in almost every module")
      call flush_(stdout)
      call text_(stdout,". See also the revert command keyword.")
      if (present(die)) stop
   end subroutine

   subroutine redirect(self)
   ! Redirect input. (This code is for non-pointer self objects).
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: filename

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      filename = next_str_(stdin)
      call redirect_(stdin,trim(filename))
   end subroutine

   subroutine docu_refine_3rd_order(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"refine_3rd_order= VEC{STR}@ ")
      call flush_(stdout)
      call text_(stdout,". Specify the list of unique atom tags for which")
      call text_(stdout,"  anharmonic 3rd order Gram-Charlier coefficients")
      call text_(stdout,"  are refined.")
      call flush_(stdout)
      call text_(stdout,". Anharmonic 4th order Gram-Charlier coefficients")
      call text_(stdout,"  are *not* refined in this case.")
      call flush_(stdout)
      call text_(stdout,". See also refine_4th_order=")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_3rd_order(self)
   ! Read the list of atoms for which third-order anharmonicty
   ! parameters are to be refined.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_all_(stdin,self%refine_3rd_order_for_atom)
      call set_refine_anharmonicity_(self,TRUE)
   end subroutine

   subroutine docu_refine_4th_order(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"refine_4th_order= VEC{STR}@ ")
      call flush_(stdout)
      call text_(stdout,". Specify the list of unique atom tags for which")
      call text_(stdout,"  anharmonic 4th order Gram-Charlier coefficients")
      call text_(stdout,"  are refined.")
      call flush_(stdout)
      call text_(stdout,". Unless otherwise cencelled, this implies that")
      call text_(stdout,"  anharmonic 3rd order Gram-Charlier coefficients")
      call text_(stdout,"  are also refined.")
      call flush_(stdout)
      call text_(stdout,". See also refine_3rd_order=")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_4th_order(self)
   ! Read the list of atoms for which fourth-order anharmonicty
   ! parameters are to be refined.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_all_(stdin,self%refine_4th_order_for_atom)
      call set_refine_anharmonicity_(self,TRUE)
   end subroutine

   subroutine docu_refine_4th_order_only(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"refine_4th_order_only= BIN [FALSE] ")
      call flush_(stdout)
      call text_(stdout,". If set, and an anharmonic refinement is being performed,")
      call text_(stdout,"  only 3rd order Gram-Charlier coefficients are refined")
      call text_(stdout,"  for the specified atoms.")
      call flush_(stdout)
      call text_(stdout,". See also refine_3rd_order=")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_4th_order_only(self)
   ! Read whether to refine 4th order coefficients only,
   ! ignoring changes to 3rd order coefficients.
   ! NOTE: it is up to you to set the ADP3s to zero.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_refine_4th_order_only_(self,val)
   end subroutine

   subroutine docu_refine_ADPs_only(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"refine_ADPs_only= BIN [FALSE] ")
      call flush_(stdout)
      call text_(stdout,". Controls whether to refine the ADPs only in a least-")
      call text_(stdout,"  squares  crystal structure refinement. Positions are")
      call text_(stdout,"  not refined.")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_ADPs_only(self)
   ! Read whether to refine only the ADP's
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%refine_ADPs_only)
   end subroutine

   subroutine docu_refine_anharmonicity(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"refine_anharmonicity= BIN [FALSE] ")
      call flush_(stdout)
      call text_(stdout,". Controls whether an anharmonic refinement is performed.")
      call flush_(stdout)
      call text_(stdout,"  This option is best used to cancel an anharmonic refinement.")
      call flush_(stdout)
      call text_(stdout,". See also:")
      call text_(stdout,"     refine_3rd_order=")
      call text_(stdout,"     refine_4th_order=")
      call text_(stdout,"  which also activates this switch.")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_anharmonicity(self)
   ! Read whether to refine anharmonicity
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_refine_anharmonicity_(self,val)
   end subroutine

   subroutine docu_refine_F(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"refine_F= BIN [TRUE] ")
      call flush_(stdout)
      call text_(stdout,". Set whether to refine on structure factors magnitudes")
      call text_(stdout,"  F_exp rather than squared structure factors, I_exp.")
      call flush_(stdout)
      call text_(stdout,". WARNING: currently, setting this option FALSE does")
      call text_(stdout,". not work.")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_F(self)
   ! Read in whether to refine F's instead of I's
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%refine_F)
   end subroutine

   subroutine docu_refine_H_ADPs_only(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"refine_h_adps=")
      call text_(stdout,"refine_h_adps_only= BIN [FALSE] ")
      call flush_(stdout)
      call text_(stdout,". If set, only the H atom ADPs are refined in a")
      call text_(stdout,"  crystal structure refinement, nothing else.")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_H_ADPs(self)
   ! Read whether to refine only the H atom ADP's
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_refine_H_ADPs_(self,val)
   end subroutine

   subroutine docu_refine_H_positions_only(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"refine_h_positions=")
      call text_(stdout,"refine_h_positions_only= BIN [FALSE] ")
      call flush_(stdout)
      call text_(stdout,". If set, only the H atom positions are refined in a")
      call text_(stdout,"  crystal structure refinement, nothing else.")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_H_positions(self)
   ! Read whether to refine only the H atom positions
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_refine_H_positions_(self,val)
   end subroutine

   subroutine docu_refine_nothing_for_atoms(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"refine_nothing_for_atoms= VEC{STR}@")
      call flush_(stdout)
      call text_(stdout,". Specify the list of unique atom tags for which *no*")
      call text_(stdout,"  crystal structure parameter is refined.")
      call flush_(stdout)
      call text_(stdout,". This option is best used to *modify* previous options.")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_nothing_for_atoms(self)
   ! Read the list of atoms for which no parameter is refined.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_all_(stdin,self%refine_nothing_for_atom)
   end subroutine

   subroutine docu_refine_positions_only(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"refine_positions_only= BIN [FALSE] ")
      call flush_(stdout)
      call text_(stdout,". If set, only atom positions are refined in a")
      call text_(stdout,"  crystal structure refinement, nothing else.")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_positions_only(self)
   ! Read whether to refine only the atom positions
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%refine_positions_only)
   end subroutine

   subroutine docu_refine_H_U_iso(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"refine_h_u_iso= BIN [FALSE] ")
      call flush_(stdout)
      call text_(stdout,". If set, refine hydrogen atoms using isotropic ADPs.")
      if (present(die)) stop
   end subroutine

   subroutine docu_ADP_model(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"adp_model= STR [free]")
      call flush_(stdout)
      call text_(stdout,". free: refine each atom's ADP.")
      call text_(stdout,". tls:  U_i = U_i^high + T + A_i L A_i^T + A_i S + S^T A_i^T,")
      call text_(stdout,"        a rigid-body model with one T, L and S for the fragment,")
      call text_(stdout,"        about its centre of mass, plus each atom's U^high from")
      call text_(stdout,"        its stiff internal modes (make_internal_adps; zero if")
      call text_(stdout,"        not made). T, L and S keep the site symmetry. HAR only.")
      call text_(stdout,"        n_soft_modes= k adds the amplitudes of the k softest")
      call text_(stdout,"        internal modes (imaginary first), each restrained to its")
      call text_(stdout,"        harmonic value with sigma soft_mode_restraint= (0.5) of it;")
      call text_(stdout,"        imaginary modes are not restrained.")
      call text_(stdout,"        use_soft_mode_correlations= TRUE also refines each soft mode's")
      call text_(stdout,"        correlation with the rigid-body motion and with the other")
      call text_(stdout,"        soft modes, each correlation coefficient restrained toward")
      call text_(stdout,"        zero with sigma correlation_restraint= (0.5).")
      call text_(stdout,"        use_positive_tls_sigma= (TRUE) holds the model's covariance")
      call text_(stdout,"        positive semidefinite: no negative mean-square motion.")
      if (present(die)) stop
   end subroutine

   subroutine read_n_soft_modes(self)
   ! Read the number of soft modes a TLS model refines.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      call set_n_soft_modes_(self,val)
   end subroutine

   subroutine read_soft_mode_restraint(self)
   ! Read the relative sigma of the soft-mode amplitude restraints.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_soft_mode_restraint_(self,val)
   end subroutine

   subroutine read_use_soft_mode_correlations(self)
   ! Read whether a TLS model refines the soft modes' correlations.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_soft_mode_correlations_(self,val)
   end subroutine

   subroutine read_correlation_restraint(self)
   ! Read the sigma of the TLS correlation-coefficient restraints.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_correlation_restraint_(self,val)
   end subroutine

   subroutine read_use_positive_TLS_sigma(self)
   ! Read whether the TLS model's covariance is held positive semidefinite.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_positive_TLS_sigma_(self,val)
   end subroutine

   subroutine read_ADP_model(self)
   ! Read the ADP model, free or tls.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      call set_ADP_model_(self,val)
   end subroutine

   subroutine read_refine_H_U_iso(self)
   ! Read whether to refine only the H atom U_iso's
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_refine_H_U_iso_(self,val)
   end subroutine

   subroutine docu_refine_U_iso_for_atoms(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"refine_u_iso_for_atoms= VEC{STR}@ [FALSE] ")
      call flush_(stdout)
      call text_(stdout,". Specify the list of unique atom tags for which")
      call text_(stdout,"  to refine isotropic ADPs.")
      call flush_(stdout)
      call text_(stdout,". This option is best used to *modify* previous options.")
      if (present(die)) stop
   end subroutine

   subroutine read_refine_U_iso_for_atoms(self)
   ! Read the list of atoms refined isotropically
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_all_(stdin,self%refine_U_iso_for_atom)
   end subroutine

   subroutine docu_revert(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"revert")
      call flush_(stdout)
      call text_(stdout,". Stop reading the input file, return to the previous")
      call text_(stdout,"  input file, and continue reading from immediately")
      call text_(stdout,"  after the redirect keyword")
      call flush_(stdout)
      call text_(stdout,". It is an error if there was not a previous input file")
      call text_(stdout,"  in which a redirect command keyword appeared.")
      if (present(die)) stop
   end subroutine

   subroutine revert(self)
   ! Revert back to previous stdin file. (This code is for non-pointer self
   ! objects).
      type(DIFFRACTION_DATA_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call revert_(stdin)
   end subroutine

   subroutine docu_reflection_data(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"reflection_data= VEC{REFLECTION}@  ")
      call flush_(stdout)
      call text_(stdout,". The diffraction data")
      if (present(die)) stop
   end subroutine

   subroutine read_reflection_data(self)
   ! Read in reflection data in the standard order from "stdin".
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
   DIE_IF(self%wavelength<ZERO,"DIFFRACTION_DATA.READ:read_reflection_data ... specify wavelength for radiation before reflection_data!")
      if (allocated(self%reflections)) call destroy_(self%reflections)
      call read_list_keywords_(self%reflections)
   end subroutine

   subroutine docu_remove_disp_from_F_exp(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"remove_disp_from_f_exp= BIN")
      call flush_(stdout)
      call text_(stdout,". Set if you want to remove atom-based anomalous dispersion")
      call text_(stdout,"  from the experimental structure factors, F_exp")
      call flush_(stdout)
      call text_(stdout,". WARNING: this option is not working. Does anyone want")
      call text_(stdout,"  to fix it? Dylan does not, particularly")
      if (present(die)) stop
   end subroutine

   subroutine read_remove_disp_from_F_exp(self)
   ! Read whether to add dispersion to F_calc.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_remove_disp_from_F_exp_(self,val)
      DIE_IF(self%add_dispersion_to_F_calc,"DIFFRACTION_DATA.READ:read_remove_disp_from_F_exp ... you already asked to add dispersion to F_calc!")
   end subroutine

   subroutine docu_restart_with_promolecule(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"restart_with_promolecule= BIN")
      call flush_(stdout)
      call text_(stdout,". If set this will restart the SCF procedure in a")
      call text_(stdout,"  a HAR, or fragHAR refinement with a promolecule")
      call text_(stdout,"  guess, either with MOs or density_matrix, as was")
      call text_(stdout,"  set by you. If you do not use this option, the")
      call text_(stdout,"  the density matrix from a previous iteration is used.")
      call flush_(stdout)
      call text_(stdout,". This option may aid convergence when cluster")
      call text_(stdout,"  charges are used; but in this case, there may be")
      call text_(stdout,"  other issues causing the convergene problems.")
      if (present(die)) stop
   end subroutine

   subroutine read_restart_with_promolecule(self)
   ! Read whether to restart with promolecule initial guess.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%restart_HAR_with_promolecule)
   end subroutine


! S

   subroutine docu_SF_mixing_parameter(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"sf_mixing_parameter= REAL")
      call flush_(stdout)
      call text_(stdout,". Read the SF mixing parameter, which resets the")
      call text_(stdout,"  predicted structure factors F_pred to include")
      call text_(stdout,"  the specified fraction of the experimental")
      call text_(stdout,"  structure factors")
      call flush_(stdout)
      call text_(stdout,". It was thought at some point that this might help")
      call text_(stdout,". convergence of X-ray constrained wavefunction calculations.")
      if (present(die)) stop
   end subroutine

   subroutine read_SF_mixing_parameter(self)
   ! Read the SF mixining parameter which inlcudes some percentage of
   ! the experimental structure factors into the predicted structure
   ! factors. This may help convergence in constrained HF problems.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%SF_mixing_parameter)
   end subroutine

   subroutine docu_show_fit_output(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"show_fit_output= BIN")
      call flush_(stdout)
      call text_(stdout,". Set TRUE if the output - and parameters - from *every* cycle")
      call text_(stdout,"  of a rigid Hirshfeld atom least-squares structure refinement")
      call text_(stdout,"  are to be displayed.")
      call flush_(stdout)
      call text_(stdout,". See also show_fit_results=")
      call flush_(stdout)
      call text_(stdout,". Note: the output will look very corrupted if this option")
      call text_(stdout,"  is set during a HAR or similar refinement. It is intended")
      call text_(stdout,"  for testing or debugging HAR, for debugging purposes only.")
      if (present(die)) stop
   end subroutine

   subroutine read_show_fit_output(self)
   ! Read whether to show the fit output (including fit results)
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_show_fit_output_(self,val)
   end subroutine

   subroutine docu_show_fit_results(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"show_fit_results= BIN [TRUE]")
      call flush_(stdout)
      call text_(stdout,". Set TRUE if *only* the results from *every* cycle of a")
      call text_(stdout,"  rigid Hirshfeld atom least-squares structure refinement")
      call text_(stdout,"  should to be displayed.")
      call flush_(stdout)
      call text_(stdout,". See also show_fit_output=")
      call flush_(stdout)
      call text_(stdout,". Note: the output may look very corrupted if this option")
      call text_(stdout,"  is set during a HAR or similar refinement. It is intended")
      call text_(stdout,"  to hide IAM or other LSQ fit cycles, for testing, or in")
      call text_(stdout,"  HAR, for debugging purposes only.")
      if (present(die)) stop
   end subroutine

   subroutine read_show_fit_results(self)
   ! Read whether to show the fit results, without parameters
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_show_fit_results_(self,val)
   end subroutine

   subroutine docu_show_IAM_output(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"show_IAM_output= BIN [TRUE]")
      call flush_(stdout)
      call text_(stdout,". Set TRUE if the IAM fit statistics from *every* cycle of")
      call text_(stdout,"  an IAM structure refinement should be displayed.")
      call flush_(stdout)
      call text_(stdout,". See also show_IAM_results=")
      if (present(die)) stop
   end subroutine

   subroutine read_show_IAM_output(self)
   ! Read whether to show the IAM output (including IAM results)
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_show_IAM_output_(self,val)
   end subroutine

   subroutine docu_show_IAM_results(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"show_IAM_results= BIN [TRUE]")
      call flush_(stdout)
      call text_(stdout,". Set TRUE if *only* the fit statistics from an IAM structure")
      call text_(stdout,"  refinement should be displayed.")
      call flush_(stdout)
      call text_(stdout,". See also show_IAM_output=")
      if (present(die)) stop
   end subroutine

   subroutine read_show_IAM_results(self)
   ! Read whether to show the IAM results, without parameters
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_show_IAM_results_(self,val)
   end subroutine

   subroutine docu_show_ref_output(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"show_refinement_output= BIN")
      call flush_(stdout)
      call text_(stdout,". Set TRUE if the refinement statistics from *every* cycle of")
      call text_(stdout,"  of a Hirshfeld atom refinement (HAR) should be displayed.")
      call flush_(stdout)
      call text_(stdout,". See also show_refinement_results=")
      call text_(stdout,". See also show_refinement_output=")
      if (present(die)) stop
   end subroutine

   subroutine read_show_ref_output(self)
   ! Read whether to show any refinement output
   ! (including refinement results)
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_show_refinement_output_(self,val)
   end subroutine

   subroutine docu_show_ref_results(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"show_refinement_results= BIN [TRUE]")
      call flush_(stdout)
      call text_(stdout,". Set TRUE if *only* a the refinement statistics from a")
      call text_(stdout,"  Hirshfeld atom refinement (HAR) should be displayed.")
      call flush_(stdout)
      call text_(stdout,". See also show_refinement_output=")
      if (present(die)) stop
   end subroutine

   subroutine read_show_ref_results(self)
   ! Read whether to show the refinement results, without parameters
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_show_refinement_results_(self,val)
   end subroutine

   subroutine docu_show_rejects(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"show_rejects= BIN [FALSE]")
      call flush_(stdout)
      call text_(stdout,". Set TRUE if you want to see a list of the rejected")
      call text_(stdout,"  reflections when pruning reflections from a data set.")
      call text_(stdout,"  Normally only the *number* of rejects is reported.")
      call flush_(stdout)
      call text_(stdout,". Note: Once reflections are rejected, they are no longer")
      call text_(stdout,"  available within the program. Keep your input files.")
      if (present(die)) stop
   end subroutine

   subroutine read_show_rejects(self)
   ! Read whether to show the rejected reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%show_rejects)
   end subroutine

   subroutine docu_stl_high_cutoff(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"stl_high_cutoff= REAL")
      call flush_(stdout)
      call text_(stdout,". Reflections with sin(theta)/lambda *larger* than the")
      call text_(stdout,"  value set for this cutoff are pruned from the data.")
      call flush_(stdout)
      call text_(stdout,". Note: atomic units are used unless specified otherwise")
      call text_(stdout,"  on the input line.")
      call flush_(stdout)
      call text_(stdout,". Must be a non-negatve value.")
      call flush_(stdout)
      call text_(stdout,". Must have set the wavelength= for the reflection data.")
      if (present(die)) stop
   end subroutine

   subroutine read_stl_high_cutoff(self)
   ! Read the stl_cutoff for prune_high_angle_reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_quantity_(stdin,self%stl_high_cutoff)
      ENSURE(self%stl_high_cutoff>=ZERO,"DIFFRACTION_DATA.READ:read_stl_high_cutoff ... must be positive")
   end subroutine

   subroutine docu_stl_low_cutoff(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"stl_low_cutoff= REAL")
      call flush_(stdout)
      call text_(stdout,". Reflections with sin(theta)/lambda *smaller* than the")
      call text_(stdout,"  value set for this cutoff are pruned from the data.")
      call flush_(stdout)
      call text_(stdout,". Note: atomic units are used unless specified otherwise")
      call text_(stdout,"  on the input line.")
      call flush_(stdout)
      call text_(stdout,". Must be a non-negatve value.")
      call flush_(stdout)
      call text_(stdout,". Must have set the wavelength= for the reflection data.")
      if (present(die)) stop
   end subroutine

   subroutine read_stl_low_cutoff(self)
   ! Read the stl_cutoff for prune_low_angle_reflections
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_quantity_(stdin,self%stl_low_cutoff)
      ENSURE(self%stl_low_cutoff>=ZERO,"DIFFRACTION_DATA.READ:read_stl_low_cutoff ... must be positive")
   end subroutine

   subroutine docu_stl_limit(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"stl_limit= REAL")
      call flush_(stdout)
      call text_(stdout,". All reflections with a value of sin(theta)/lambda")
      call text_(stdout,"  less than this value are generated, for the purposes")
      call text_(stdout,"  of obtaining calculated structure fac tors, |F_calc|")
      call flush_(stdout)
      call text_(stdout,". Note: atomic units are used unless specified otherwise")
      call text_(stdout,"  on the input line.")
      call flush_(stdout)
      call text_(stdout,". Must be a pisitive value.")
      call flush_(stdout)
      call text_(stdout,". Must have set the wavelength= for the reflection data.")
      if (present(die)) stop
   end subroutine

   subroutine read_stl_limit(self)
   ! Read in the stl limit for generating (h k l)
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_quantity_(stdin,self%stl_limit)
      ENSURE(self%stl_limit>ZERO,"DIFFRACTION_DATA.READ:read_stl_limit ... must be +ve!")
   end subroutine


! T

!   docu_temperature_factor_model(die) :: selfless, private
!   ! Documentation.
!      die :: BIN, optional, IN
!
!      stdout.flush
!      stdout.flush
!      stdout.text("thermal_smearing_model= ")
!      stdout.text("temperature_factor_model= STR")
!      stdout.flush
!      stdout.text(". Specify the ADP model to be used to calculate the")
!      stdout.text("  anisotropic Debye-Waller factors for producing dynamic")
!      stdout.text("  or thermally smeared structure factors")
!      stdout.flush
!      stdout.text(". Allowed values are:")
!      stdout.text("     - none")
!      stdout.text("       ... for static structure factors;")
!      stdout.text("     - one-center")
!      stdout.text("       atom-based")
!      stdout.text("       ... for one-center models;")
!      stdout.text("     - tc-coppens")
!      stdout.text("       tc-stewart")
!      stdout.text("       tc-tanaka")
!      stdout.text("       ... for two-center based models")
!      stdout.flush
!      stdout.text(". See Davidson et al (2023) Acta Cryst B 72 p. 312-32")
!
!      if (present(die)) stop
!
!   end

!   read_temperature_factor_model :: private
!   ! Read the temperature factor model to use to correct for thermal
!   ! vibration in the calculated structure factors
!      self :: INOUT
!
!      val :: STR
!
!      stdin.read(val)
!      .set_temperature_factor_model(val)
!
!   end

   subroutine docu_tol_for_shift_on_esd(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"tol_for_shift_on_esd= REAL [0.01]")
      call flush_(stdout)
      call text_(stdout,". Tolerance for the largest change in any least-squares")
      call text_(stdout,"  -refined structural parameter, divided by its estimated")
      call text_(stdout,"  standard uncertainly (ESU), such that convergence of the")
      call text_(stdout,"  iterative LSQ procedure is regarded to have occured.")
      if (present(die)) stop
   end subroutine

   subroutine read_tol_for_shift_on_esd(self)
   ! Read the tolerance for the shift on esd to decide on convergence.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%tol_for_shift_on_esd)
      ENSURE(self%tol_for_shift_on_esd>ZERO,"DIFFRACTION_DATA.READ:read_tol_for_shift_on_esd ... must be positive")
   end subroutine

   subroutine docu_near_0_tol(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"near_0_tol= REAL [0.001]")
      call text_(stdout,"tol_near_0= REAL [0.001]")
      call flush_(stdout)
      call text_(stdout,". Specifies the absolute value for eigenvalues of the")
      call text_(stdout,"  least-squares matrix *below* which the corresponding")
      call text_(stdout,"  eigenvectors are regarded to describe linear dependencies.")
      call text_(stdout,"  These linear dependencies are removed when solving")
      call text_(stdout,"  the least-squares equations for crystal structure parameters.")
      call flush_(stdout)
      call text_(stdout,". Must be a positive quantity.")
      if (present(die)) stop
   end subroutine

   subroutine read_near_0_tol(self)
   ! Read the tolerance for deciding on *near* zero eigenvalues in
   ! solving the normal equations.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%near_0_tol)
      ENSURE(self%near_0_tol>ZERO,"DIFFRACTION_DATA.READ:read_near_0_tol ... near_0_tol must be positive")
   end subroutine


! U

   subroutine docu_use_disk_SFs(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"use_disk_SFs= BIN [FALSE]")
      call flush_(stdout)
      call text_(stdout,". Set if you want the atomic form factors to be written")
      call text_(stdout,"  out onto the disk, instead of being stored in memory.")
      call text_(stdout,"  This drastically reduced memory usage for a HAR")
      call text_(stdout,"  but at (possible) cost of slowing the calculation.")
      call flush_(stdout)
      call text_(stdout,". This option is set by default for fragHAR refinements")
      call flush_(stdout)
      call text_(stdout,". See Bergman et al (2020) IUCrJ 7 p. 1-8.")
      if (present(die)) stop
   end subroutine

   subroutine read_use_disk_SFs(self)
   ! Read whether to use disk structure factors.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_disk_SFs_(self,val)
   end subroutine

   subroutine docu_use_equivalents(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"use_disk_SFs= BIN [FALSE]")
      call flush_(stdout)
      call text_(stdout,". Set if you want to *keep* equivalent reflections.")
      call text_(stdout,"  If you do *not* set this TRUE, equivalent reflections")
      call text_(stdout,"  will be deleted.")
      call flush_(stdout)
      call text_(stdout,". IMPORTANT: Tonto requires a  symmetry independent set")
      call text_(stdout,"  of reflections.")
      call flush_(stdout)
      call text_(stdout,". Friedel pairs may or may not be present; if you are")
      call text_(stdout,"  using dispersion corrections, you had better have")
      call text_(stdout,"  Friedel pairs in you set of reflections!")
      if (present(die)) stop
   end subroutine

   subroutine read_use_equivalents(self)
   ! Read whether to keep equivalent reflections in the data set. 
   ! Normally symmetry-equivalent reflections will be pruned out.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_equivalents_(self,val)
   end subroutine

   subroutine docu_use_IAM_ITC_FFs(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"use_IAM_ITC_FFs= BIN [TRUE]")
      call flush_(stdout)
      call text_(stdout,". Set FALSE if an IAM refinement will use gaussian atomic")
      call text_(stdout,"  atomic densities.")
      call flush_(stdout)
      call text_(stdout,"  In that case sphericized density matrices are obtained")
      call text_(stdout,"  from a high-spin atomic ground state QM calculation using")
      call text_(stdout,"  the specified SCF method, and with the specified basis set.")
      call flush_(stdout)
      call text_(stdout,". The default is to use tabulated atomic form factors from")
      call text_(stdout,"  the International Tables volume C.")
      if (present(die)) stop
   end subroutine

   subroutine read_use_IAM_ITC_FFs(self)
   ! Read whether to use gaussian atomic densities in an IAM refinement.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_IAM_ITC_FFs_(self,val)
   end subroutine

   subroutine docu_use_NN(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"use_NN= BIN [FALSE]")
      call flush_(stdout)
      call text_(stdout,". Set TRUE if you want to break a periodic network")
      call text_(stdout,"  solid into H-atom-capped fragments, one centred on")
      call text_(stdout,"  each atom of the asymmetric unit, for the purpose of")
      call text_(stdout,"  efficiently obtaining aspherical atomic form factors,")
      call text_(stdout,"  the fragHAR procedure.")
      call flush_(stdout)
      call text_(stdout,". Unlike use_Ryde_capping, which splits a structure at")
      call text_(stdout,"  its compound boundaries, this makes a fragment for")
      call text_(stdout,"  every asymmetric unit atom by following bonds out")
      call text_(stdout,"  NN_level shells. It is intended for solids such as")
      call text_(stdout,"  quartz, where no molecule can be isolated at all.")
      call flush_(stdout)
      call text_(stdout,". Use NN_level= to set how far the fragments extend,")
      call text_(stdout,"  and use_NN_capping= to turn the capping off.")
      if (present(die)) stop
   end subroutine

   subroutine read_use_NN(self)
   ! Read whether to use nearest-neighbour capping to make atom-centred
   ! fragments of a periodic network solid.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_NN_(self,val)
   end subroutine

   subroutine docu_use_NN_capping(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"use_NN_capping= BIN [TRUE]")
      call flush_(stdout)
      call text_(stdout,". Set FALSE if you want the nearest-neighbour")
      call text_(stdout,"  fragments left with a bare outermost shell, instead")
      call text_(stdout,"  of having that shell replaced by capping hydrogen")
      call text_(stdout,"  atoms. Only meaningful when use_NN= is set.")
      call flush_(stdout)
      call text_(stdout,". A capping hydrogen is placed along the internuclear")
      call text_(stdout,"  vector of the bond it replaces, at the sum of the")
      call text_(stdout,"  two CCDC covalent radii from the atom it caps.")
      if (present(die)) stop
   end subroutine

   subroutine read_use_NN_capping(self)
   ! Read whether the nearest-neighbour fragments have their outermost
   ! shell replaced by capping hydrogen atoms.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_NN_capping_(self,val)
   end subroutine

   subroutine docu_NN_level(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"NN_level= INT [1]")
      call flush_(stdout)
      call text_(stdout,". The number of shells of nearest neighbours, by atom")
      call text_(stdout,"  bonding, to include in the fragment made around each")
      call text_(stdout,"  asymmetric unit atom of a periodic network solid.")
      call text_(stdout,"  Everything beyond this shell is replaced by a")
      call text_(stdout,"  capping hydrogen atom.")
      call flush_(stdout)
      call text_(stdout,". Setting this implies use_NN= TRUE.")
      call flush_(stdout)
      call text_(stdout,". For quartz, level 1 around the silicon atom gives")
      call text_(stdout,"  orthosilicic acid, and around the oxygen atom gives")
      call text_(stdout,"  silyloxysilane.")
      if (present(die)) stop
   end subroutine

   subroutine read_NN_level(self)
   ! Read how many shells of nearest neighbours are included before
   ! capping hydrogen atoms, for a periodic network solid.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)

      ! Should not be given unless the NN method is in use
      call set_use_NN_(self,TRUE)
      call set_NN_level_(self,val)
   end subroutine

   subroutine docu_use_Ryde_capping(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"use_Ryde_capping= BIN [FALSE]")
      call flush_(stdout)
      call text_(stdout,". Set TRUE if you want to use the Ulf Ryde method")
      call text_(stdout,"  for breaking up proteins into H-atom-capped")
      call text_(stdout,"  fragments, for the purpose of efficiently obtaining")
      call text_(stdout,"  aspherical atomic form factors, the fragHAR procedure.")
      call flush_(stdout)
      call text_(stdout,". This option is set by default for fragHAR_refinement")
      call flush_(stdout)
      call text_(stdout,". See Bergman et al (2020) IUCrJ 7 p. 1-8.")
      if (present(die)) stop
   end subroutine

   subroutine read_use_Ryde_capping(self)
   ! Read whether to use Ulf Ryde's capping method to split and cap
   ! polypeptides to make wavefunction and atomic structure factor
   ! calculations for proteins quicker.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_Ryde_capping_(self,val)
   end subroutine

   subroutine docu_use_text_SFs(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"use_text_SFs= BIN [FALSE]")
      call flush_(stdout)
      call text_(stdout,". Set if you want the atomic form factors to be written")
      call text_(stdout,"  out onto the disk in *text* or ASCII format, instead of")
      call text_(stdout,"  being stored in memory.")
      call flush_(stdout)
      call text_(stdout,". This option sets use_disk_SFs=")
      call flush_(stdout)
      call text_(stdout,". This option is set by default for fragHAR refinements")
      call flush_(stdout)
      call text_(stdout,". See Bergman et al (2020) IUCrJ 7 p. 1-8.")
      if (present(die)) stop
   end subroutine

   subroutine read_use_text_SFs(self)
   ! Read whether to use disk structure factors in ASCII format.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_text_SFs_(self,val)
   end subroutine

   subroutine docu_wavelength(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"wavelength= REAL ")
      call flush_(stdout)
      call text_(stdout,". Set the wavelength of the particles used for diffraction.")
      call text_(stdout,"  Do not forget to specify the units.")
      call flush_(stdout)
      call text_(stdout,". You must specify this quantity for sensible plane spacings")
      call text_(stdout,"  or for sensible pruning.")
      if (present(die)) stop
   end subroutine


! U

   subroutine read_wavelength(self)
   ! Read the experimental wavelength
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_quantity_(stdin,val)
      call set_wavelength_(self,val)
   end subroutine


! Undocumented so far ...

   subroutine read_synthesize_sigma_I(self)
   ! Read the switch whether to artificially create sigma(I) errors when
   ! evaluating the GoF2 statistics based on intensities. Refer to routine
   ! .I_sigma
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%synthesize_sigma_I)
   end subroutine

   subroutine read_stop_after_dispersion(self)
   ! Stop after applying dispersion correction in the zeroth iteration
   ! of a refinement.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%stop_after_dispersion)
   end subroutine

   subroutine read_temperature(self)
   ! Read the temperature
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%temperature)
      ENSURE(self%temperature>=0,"DIFFRACTION_DATA.READ:read_temperature ... temperature is not positive!")
   end subroutine


!   read_NKA_T_seq_no
!   ! Read the temperature sequence no for NKA output file
!      self :: INOUT
!
!      stdin.read(.NKA_T_sequence_no)
!
!   ENSURE(.NKA_T_sequence_no>0,"sequence no is not positive!")
!
!   end

   subroutine read_use_Voronoi_atoms(self)
   ! Read whether to use Voronoi atoms in refinement
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%use_Voronoi_atoms)
   end subroutine

   subroutine read_use_KM_atoms(self)
   ! Read whether to use KM atom density in refinement
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%use_KM_atoms)
   end subroutine

   subroutine read_use_zero_overlap(self)
   ! Read whether to use KM density in refinement
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      call read_(stdin,self%use_zero_overlap)
   end subroutine


! ========
! Read CIF
! ========

   subroutine read_CIF_0(self)
   ! Read information from a Crystallographic Information File
   ! with the "name" taken from stdin.
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name
      BIN :: found
      type(CIF_TYPE), allocatable :: cif

      ! Read name from stdin
      call read_(stdin,name)

      ! Create and open the CIF
      call create_(cif,name)
      call open_(cif)
      call find_data_block_(cif,found)
      ENSURE(found,"DIFFRACTION_DATA.READ:read_CIF_0 ... no crystal data block found")

      ! Read
      call read_CIF_(self,cif)

      ! Clean
      call destroy_(cif)
   end subroutine

   subroutine read_CIF_1(self,cif)
   ! Read information from a Crystallographic Information File "cif".
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      if (NOT cif%is_mmCIF) then
         call read_smCIF_(self,cif)
      else
         call read_mmCIF_(self,cif)
      end if
   end subroutine

   subroutine read_smCIF(self,cif)
   ! Read information from a small-molecule Crystallographic
   ! Information File "cif".
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN :: there
      STR(len=STR_SIZE) :: sval
      INT :: ival
      REAL :: rval,esd

      ! Read reflections
      call read_CIF_(self%reflections,cif)

      ! First prune I's and define F's
      if (have_I_exp_(self)) then
         DIE_IF(NOT have_nonzero_I_sigma_(self),"DIFFRACTION_DATA.READ:read_smCIF ... I_exp present but no I_sigma?")
         call prune_small_I_sigmas_(self)
         call reset_I_exp_I_sigma_(self%reflections)
      end if

      ! Next prune F's
      if (have_F_exp_(self)) then
         DIE_IF(NOT have_nonzero_F_sigma_(self),"DIFFRACTION_DATA.READ:read_smCIF ... F_exp present but no F_sigma?")
         call prune_small_F_sigmas_(self)
         call reset_F_exp_F_sigma_(self%reflections)
      end if

      ! Is there a wavelength in the CIF?
      call find_item_(cif,"_diffrn_radiation_wavelength",there)

      ! If there, read wavelength and override existing
      if (there) then
         call read_item_(cif,"_diffrn_radiation_wavelength",rval)
         rval = rval*BOHR_PER_ANGSTROM
         call set_wavelength_(self,rval)
      end if

      ! Read no of parameters
      call find_item_(cif,"_refine_ls_number_parameters",there)
      if (there) then
         call read_item_(cif,"_refine_ls_number_parameters",ival)
         call set_n_param_(self,ival)
      end if

      ! Read extinction. The method switches the correction on as well as
      ! off, and the coefficient is read back, so a job restarted from a CIF
      ! continues from the extinction factor already found.
      call find_item_(cif,"_refine_ls_extinction_method",there)
      if (there) then
         call read_item_(cif,"_refine_ls_extinction_method",sval)
         call to_lower_case_(sval)
         call set_refine_extinction_(self,sval/="none" AND sval/=".")
      end if
      if (self%refine_extinction) then
         call find_item_(cif,"_refine_ls_extinction_coef",there)
         if (there) then
            call read_item_(cif,"_refine_ls_extinction_coef",rval,esd)
            call set_extinction_factor_(self,rval,esd)
         end if
      end if

      ! Read XCW
      call find_item_(cif,"_refine_QCr_Psi_constraint",there)
      if (there) then
         call read_item_(cif,"_refine_QCr_Psi_constraint",sval)
         if (sval=="lambda*GoF2") call set_XCW_(self,TRUE)
      end if
   end subroutine

   subroutine read_mmCIF(self,cif)
   ! Read information from a macromolecular Crystallographic
   ! Information File "cif".
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN :: there
      REAL :: rval

      ! Read reflections
      call read_CIF_(self%reflections,cif)

      ! First prune I's and define F's
      if (have_I_exp_(self)) then
         DIE_IF(NOT have_nonzero_I_sigma_(self),"DIFFRACTION_DATA.READ:read_mmCIF ... I_exp present but no I_sigma?")
         call prune_small_I_sigmas_(self)
         call reset_I_exp_I_sigma_(self%reflections)
      end if

      ! Next prune F's
      if (have_F_exp_(self)) then
         DIE_IF(NOT have_nonzero_F_sigma_(self),"DIFFRACTION_DATA.READ:read_mmCIF ... F_exp present but no F_sigma?")
         call prune_small_F_sigmas_(self)
         call reset_F_exp_F_sigma_(self%reflections)
      end if

      ! Is there a wavelength in the CIF?
      call find_item_(cif,"_diffrn.radiation_wavelength",there)

      ! If there, read wavelength and override existing
      if (there) then
         call read_item_(cif,"_diffrn.radiation_wavelength",rval)
         rval = rval*BOHR_PER_ANGSTROM
         call set_wavelength_(self,rval)
      end if
   end subroutine

end module
