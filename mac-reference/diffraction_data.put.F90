!====================================================================
!
! DIFFRACTION_DATA.put: 
!
! Diffraction data structure for crystals: put methods
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
module DIFFRACTION_DATA_PUT_MODULE

#  include "macros"

   include "diffraction_data.put.use"

   implicit none

   include "diffraction_data.put.int"


contains

! =======
! Put CIF
! =======

   subroutine put_CIF_refinement(self)
   ! Put refinement information in CIF format
   ! No refinement info. possible without reflections!
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n
      REAL :: R
      STR(len=STR_SIZE) :: val
      if (NOT allocated(self%reflections)) return
      call flush_(stdout)
      call text_(stdout,"# ===============")
      call text_(stdout,"# Refinement data")
      call text_(stdout,"# ===============")
      call flush_(stdout)
      call text_(stdout,"_refine_ls_kind                     'Hirshfeld atom refinement (HAR)'")
      call text_(stdout,"_refine_ls_method_description")
      call text_(stdout,";")
      call text_(stdout,"   S.C. Capelli, H.-B. Burgi, B. Dittrich, S.Grabowsky")
      call text_(stdout,"   and Dylan Jayatilaka, IUCrJ (2014). 1, 361-379")
      call text_(stdout,";")
      if (self%XCW) then
         call text_(stdout,"_refine_QCr_Psi_constraint          'lambda*GoF2'")
         call text_(stdout,"_refine_QCr_method_description")
         call text_(stdout,";")
         call text_(stdout,"  'The Significance of Ionic Bonding in Sulfur Dioxide: ")
         call text_(stdout,"   Bond Orders from X-ray Diffraction Data' ")
         call text_(stdout,"   S. Grabowsky,* P. Luger, J. Buschmann, T. Schneider")
         call text_(stdout,"   T. Schirmeister, A.N. Sobolev, and D. Jayatilaka")
         call text_(stdout,";")
      else
         call text_(stdout,"_refine_QCr_Psi_constraint          'none'")
      end if
      call flush_(stdout)
      val = to_str_(self%near_0_tol,"e9.1")
      call text_(stdout,"_refine_ls_SVD_threshold            "//trim(val))
      call text_(stdout,"_refine_ls_coordinate_system        cartesian")
      call text_(stdout,"_refine_ls_structure_factor_coef    F")
      call text_(stdout,"_refine_ls_matrix_type              full")
      R = F_r_factor_(self%reflections)
      val = to_str_(R,"f10.4")
      call text_(stdout,"_refine_ls_R_factor_gt              "//trim(val))
      call text_(stdout,"_refine_ls_R_factor_ref             "//trim(val))
      call text_(stdout,"_refine_ls_R_factor_all             "//trim(val))
      R = F_weighted_r_factor_(self%reflections)
      val = to_str_(R,"f10.4")
      call text_(stdout,"_refine_ls_wR_factor_gt             "//trim(to_str_(R,"f10.4")))
      call text_(stdout,"_refine_ls_wR_factor_ref            "//trim(to_str_(R,"f10.4")))
      call text_(stdout,"_refine_ls_wR_factor_all            "//trim(to_str_(R,"f10.4")))
      R = F2_r_factor_(self%reflections)
      val = to_str_(R,"f10.4")
      call text_(stdout,"_refine_ls_R_Fsqd_factor            "//trim(to_str_(R,"f10.4")))
      R = (self%GoF2)
      val = to_str_(R,"f10.4")
      call text_(stdout,"_refine_ls_chi2                     "//trim(val))
      R = sqrt(self%GoF2)
      val = to_str_(R,"f10.4")
      call text_(stdout,"_refine_ls_goodness_of_fit_gt       "//trim(val))
      call text_(stdout,"_refine_ls_goodness_of_fit_ref      "//trim(val))
      call text_(stdout,"_refine_ls_goodness_of_fit_all      "//trim(val))
   ! stdout.text("_refine_ls_restrained_S_all             "//trim(val))
      n = size(self%reflections)
      val = to_str_(n)
      call text_(stdout,"_refine_ls_number_reflns            "//trim(val))
      call text_(stdout,"_refine_ls_number_parameters        "//trim(to_str_(self%n_param)))
      call text_(stdout,"_refine_ls_number_restraints        0")
      val = to_str_(n_constraints_(self))
      call text_(stdout,"_refine_ls_number_constraints       "//trim(val))
      if (self%refine_H_ADPs) then
         if (refine_positions_and_ADPs_(self)) then
            call text_(stdout,"_refine_ls_hydrogen_treatment       refall")
         else if (self%refine_ADPs_only) then
            call text_(stdout,"_refine_ls_hydrogen_treatment       refU")
         end if
      else
         call text_(stdout,"_refine_ls_hydrogen_treatment       refxyz")
      end if
      call text_(stdout,"_refine_ls_weighting_scheme         sigma")
      call text_(stdout,"_refine_ls_weighting_details        'w=1/\s(F)'")
      val = adjustl(to_str_(self%tol_for_shift_on_esd,"f10.4"))
      call text_(stdout,"_refine_ls_shift/su_conv_tol        "//trim(val))
      val = adjustl(to_str_(self%max_refshift_on_esd,"f10.4"))
      call text_(stdout,"_refine_ls_shift/su_max             "//trim(val))
      val = adjustl(to_str_(self%ave_refshift_on_esd,"f10.4"))
      call text_(stdout,"_refine_ls_shift/su_max_mean        "//trim(val))
      call text_(stdout,"_refine_ls_shift/su_max_par         '"//trim(self%max_refshift_on_esd_par)//"'")
      if (self%refine_extinction) then
         call text_(stdout,"_refine_ls_extinction_method        SHELXL")
         if (self%extinction_factor_esd>ZERO) then
            call text_(stdout,"_refine_ls_extinction_coef          ",flush=FALSE)
            call put_(stdout,self%extinction_factor,self%extinction_factor_esd,width=24,precision=8,d2_err=TRUE)
            call flush_(stdout)
         else
            val = adjustl(to_str_(self%extinction_factor,"e16.8"))
            call text_(stdout,"_refine_ls_extinction_coef          "//trim(val))
         end if
      else
         call text_(stdout,"_refine_ls_extinction_method        none")
         call text_(stdout,"_refine_ls_extinction_coef          .")
      end if
      if (self%do_residual_cube) then
         call flush_(stdout)
         val = adjustl(to_str_(self%max_residual,"f10.4"))
         call text_(stdout,"_refine_diff_density_max            "//trim(val))
         val = adjustl(to_str_(self%min_residual,"f10.4"))
         call text_(stdout,"_refine_diff_density_min            "//trim(val))
         val = adjustl(to_str_(self%rms_residual,"f10.4"))
         call text_(stdout,"_refine_diff_density_rms            "//trim(val))
         val = (to_str_(self%max_atom_residual,"f10.4"))
!     stdout.text("_refine_diff_density_max_atom       "//trim(val))
!     stdout.text("_refine_diff_density_max_atom_tag   "//trim(.max_atom_residual_tag))
!     val = (.min_atom_residual.to_str("f10.4"))
!     stdout.text("_refine_diff_density_min_atom       "//trim(val))
!     stdout.text("_refine_diff_density_min_atom_tag   "//trim(.min_atom_residual_tag))
!     val = (.rms_atom_residual.to_str("f10.4"))
!     stdout.text("_refine_diff_density_rms_atom       "//trim(val))
      end if
      call flush_(stdout)
      call text_(stdout,"_refine_special_details")
      call text_(stdout,";")
      call text_(stdout,". If constraints were applied they are defined by zero")
      call text_(stdout,"  eigenvalues of the least-squares hessian, see the")
      call text_(stdout,"  value of _refine_ls_SVD_threshold")
      call text_(stdout,";")
   end subroutine

   subroutine put_CIF_covariance_matrices(self,cif)
   ! Output the covariance matrix in CIF format
      type(DIFFRACTION_DATA_TYPE), IN :: self
      type(CIF_TYPE), IN :: cif

      ! Asymmetric unit covariance
      if (cif%use_CIF2) then
         if (allocated(self%covariance_mx)) then
            call put_CIF_covariance_mx("asym_unit",self%covariance_mx,cif%use_CIF2)
         end if
      end if

      ! Fragment covariance?
      if (cif%use_CIF2) then
         if (allocated(self%fragment_covariance_mx)) then
            call put_CIF_covariance_mx("fragment",self%fragment_covariance_mx,cif%use_CIF2)
         end if
      end if
   end subroutine

   subroutine put_CIF_covariance_mx(kind,covariance_mx,CIF2)
   ! Output the covariance matrix in CIF format
      STR(len=*), IN :: kind
      MAT(REAL,:,:), IN :: covariance_mx
      BIN, IN :: CIF2
   ENSURE(is_square_(covariance_mx),"DIFFRACTION_DATA.PUT:put_CIF_covariance_mx ... covariance is not square")

      ! Change defaults
      call save_style_(stdout)
      call set_real_fortran_fmt_(stdout,"e")
      call set_real_width_(stdout,16)
      call set_real_precision_(stdout,8)
      call flush_(stdout)
      call text_(stdout,"# NOTE: Cartesian 9Nx9N covariance matrix in BOHR units")
      call text_(stdout,"# The order of the variables (per atom) is as follows:")
      call text_(stdout,"# . Elements 1-3: positions: x y x")
      call text_(stdout,"# . Elements 4-9: 2nd order ADPs:")
      call text_(stdout,"#      U 11 22 33 12 13 23")
      call text_(stdout,"# . Elements 10-19: 3rd order ADPs (if refined):")
      call text_(stdout,"#      U 111 222 333 112 113 122 223 133 233 123")
      call text_(stdout,"# . Elements 20-34: 4th order ADPs (if refined):")
      call text_(stdout,"#      U 1112 1113 1222 2223 1333 2333")
      call text_(stdout,"#        1122 1133 2233 1123 1223 1233")
      call text_(stdout,"# . Matrix is output in (fortran) column order")
      call flush_(stdout)
      call text_(stdout,"_"//trim(kind)//"_covariance_mx")

      ! Opening brace
      if (CIF2) then
         call text_(stdout,"[")
      else
         call text_(stdout,";")
      end if

      ! Matrix contents
      call put_(stdout,covariance_mx,by_column=TRUE)
      call flush_(stdout)

      ! Closing brace
      if (CIF2) then
         call text_(stdout,"]")
      else
         call text_(stdout,";")
      end if

      ! Put back defaults
      call unsave_style_(stdout)
   end subroutine


! =========================
! Put fit/refinement output
! =========================

   subroutine put_fit_header(self)
   ! Put the fit header
      type(DIFFRACTION_DATA_TYPE), IN :: self
      if (self%is_IAM_refinement) then
         call flush_(stdout)
         call text_(stdout,"==============")
         call text_(stdout,"IAM refinement")
         call text_(stdout,"==============")
         call flush_(stdout)
         if (self%use_IAM_ITC_FFs) then
            call text_(stdout,"Using atomic form factors from International Tables vol. C.")
            call flush_(stdout)
            call text_(stdout,"Neutral-charged atoms are used, except for the H atom, where")
            call text_(stdout,"Stewart-Davidson-Simpson bonded-atom form factors are used.")
         else
            call text_(stdout,"Atomic form factors are from post-facto sphericized high-spin")
            call text_(stdout,"ground state neutral-charged atomic density matrices.")
            call flush_(stdout)
            call text_(stdout,"The QM method and basis set used were specified in the input.")
         end if
      else
         call flush_(stdout)
         call text_(stdout,"===================================")
         call text_(stdout,"Crystal structure fit (rigid atoms)")
         call text_(stdout,"===================================")
         call flush_(stdout)
         call text_(stdout,"A single SCF/rigid hirshfeld atom fit is performed.")
         call flush_(stdout)
         call text_(stdout,"Consider non-rigid hirshfeld_atom_refinement instead.")
      end if
   end subroutine

   subroutine put_refinement_header()
   ! Put the fit header
      call flush_(stdout)
      call text_(stdout,"==============================================")
      call text_(stdout,"Crystal structure refinement (non-rigid atoms)")
      call text_(stdout,"==============================================")
      call flush_(stdout)
      call text_(stdout,"Repeated SCF/rigid hirshfeld atom fits are performed")
      call text_(stdout,"which eventually removes the rigid atom constraint.")
      call flush_(stdout)
      call text_(stdout,"Agreement statistics are shown at the start and end")
      call text_(stdout,"of each 'micro' rigid fit cycle.")
      call flush_(stdout)
      call text_(stdout,"Convergence criteria for each fit-cycle are the same")
      call text_(stdout,"as for convergence of the overall refinement")
   end subroutine

   subroutine put_refinement_switches(self)
   ! Put the fit/refinement switches
      type(DIFFRACTION_DATA_TYPE), IN :: self
      STR(len=STR_SIZE) :: list
      call flush_(stdout)
      if (self%partition_model/=" ") then
         call show_(stdout,"Atom ED partition model     =",self%partition_model)
         call show_(stdout,"Is one-center TF model?     =",using_one_center_TF_model_(self))
      end if
      call show_(stdout,"Refine hydrogen ADP's       =",self%refine_H_ADPs)
      call show_(stdout,"Refine hydrogen U_iso       =",self%refine_H_U_iso)
      call show_(stdout,"Refine only ADP's           =",self%refine_ADPs_only)
      call show_(stdout,"Refine only positions       =",self%refine_positions_only)
      if (self%refine_anharmonicity) then
         call flush_(stdout)
         call show_(stdout,"Refine anharmonicity        =",TRUE)
         call show_(stdout,"Refine 4th order only?      =",self%refine_4th_order_only)
         list = to_concatenated_str_(self%refine_4th_order_for_atom)
         call show_(stdout,"Anharmonic atoms            =",trim(list))
      end if
      call flush_(stdout)
      call show_(stdout,"Refine scale factor         =",self%refine_scale)
      call show_(stdout,"Refine multiple scales      =",use_multiple_scale_factors_(self))
      call show_(stdout,"Refine extinction parameter =",self%refine_extinction)
      call show_(stdout,"Correct dispersion?         =",correct_dispersion_(self))
      if (correct_dispersion_(self)) then
         call show_(stdout,"Add dispersion into F_calc  =",self%add_dispersion_to_F_calc)
         call show_(stdout,"Remove dispersion frm F_exp =",self%remove_dispersion_from_F_exp)
      end if
      call flush_(stdout)
      call show_(stdout,"Max allowed parameter shift =",self%max_allowed_param_shift)
      call show_(stdout,"Max iterations              =",self%max_iterations)
      call show_(stdout,"Min iterations              =",self%min_iterations)
      call flush_(stdout)
      call show_(stdout,"Conv. tol. for shift on esd =",self%tol_for_shift_on_esd)
      call show_(stdout,"Nearly zero tolerance       =",self%near_0_tol)
   end subroutine

   subroutine put_fit_table_headings(self)
   ! Put the fit table header
      type(DIFFRACTION_DATA_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"====================")
      call text_(stdout,"Begin rigid-atom fit")
      call text_(stdout,"====================")
      call flush_(stdout)

      ! Put table headings
      call put_headings_(self%fit_table)
   end subroutine

   subroutine put_refinement_table_headings(self)
   ! Put the refinement table header
      type(DIFFRACTION_DATA_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"================")
      call text_(stdout,"Begin refinement")
      call text_(stdout,"================")
      call flush_(stdout)

      ! Put table headings
      call put_headings_(self%ref_table)
   end subroutine

   subroutine put_fit_table_body(self)
   ! Print the current iterations results
      type(DIFFRACTION_DATA_TYPE), INOUT :: self

      ! Set table body (leaky)
      call set_values_(self%fit_table(1),[self%fit_iteration])
      call set_values_(self%fit_table(2),[sqrt(self%GoF2_fit)])
      call set_values_(self%fit_table(3),[F_r_factor_(self%reflections)])
      call set_values_(self%fit_table(4),[F_weighted_r_factor_(self%reflections)])
      call set_values_(self%fit_table(5),[self%max_shift_on_esd])
      call set_values_(self%fit_table(6),[self%max_shift])
      call set_values_(self%fit_table(7),[self%max_shift_on_esd_par])
      call set_values_(self%fit_table(8),[self%n_param])
      call set_values_(self%fit_table(9),[n_near_0_(self)])

      ! Put out the body
      call put_body_(self%fit_table)

      ! Clear thje columns for next time (UNleaky!)
      call clear_columns_(self%fit_table)
   end subroutine

   subroutine put_refinement_table_body(self)
   ! Print the current iterations results
      type(DIFFRACTION_DATA_TYPE), INOUT :: self

      ! Set table body (leaky)
      call set_values_(self%ref_table(1),[self%ref_iteration])
      call set_values_(self%ref_table(2),[self%fit_iteration])
      call set_values_(self%ref_table(3),[sqrt(self%GoF2_ref0)])
      call set_values_(self%ref_table(4),[sqrt(self%GoF2_ref)])
      call set_values_(self%ref_table(5),[F_r_factor_(self%reflections)])
      call set_values_(self%ref_table(6),[F_weighted_r_factor_(self%reflections)])
      call set_values_(self%ref_table(7),[self%max_refshift_on_esd])
      call set_values_(self%ref_table(8),[self%max_refshift])
      call set_values_(self%ref_table(9),[self%max_refshift_on_esd_par])
      call set_values_(self%ref_table(10),[self%n_param])
      call set_values_(self%ref_table(11),[n_near_0_(self)])

      ! Put out the body
      call put_body_(self%ref_table)

      ! Clear thje columns for next time (UNleaky!)
      call clear_columns_(self%ref_table)
   end subroutine

   subroutine put_fit_results(self)
   ! Put the fit results
      type(DIFFRACTION_DATA_TYPE), IN :: self

      ! Results
      call flush_(stdout)
      if (self%is_IAM_refinement) then
         call text_(stdout,"==============")
         call text_(stdout,"IAM refinement")
         call text_(stdout,"==============")
      else
         call text_(stdout,"======================")
         call text_(stdout,"Rigid-atom fit results")
         call text_(stdout,"======================")
      end if
      call flush_(stdout)

      ! Print how it finished
      if (self%fit_converged) then; call text_(stdout,"Structure fit converged.")
      else if (self%GoF2_increased) then; call text_(stdout,"WARNING: fit stopped: GoF has increased.")
         call text_(stdout,"WARNING: fit results are unreliable.")
      else if (self%too_many_fit_iterations) then; call text_(stdout,"WARNING: fit stopped: too many iterations.")
      end if

      ! Which parameter combinations the data does not separate
      call put_near_0_eigenvector_report_(self)

      ! Put final fitting statistics
      call put_F_statistics_(self)
   end subroutine

   subroutine put_near_0_eigenvector_report(self)
   ! Report which refined parameters the data cannot separate, and how
   ! many are mixed in each undetermined direction.
   ! OFF by default, and deliberately NOT a warning. It cannot tell a
   ! harmless cause from a harmful one: a parameter constrained by
   ! symmetry gives an undetermined direction just as an over-parameterised
   ! model does, and when the site symmetry axis is not along a cartesian
   ! axis the symmetry case mixes parameters too. NH3 on a 3-fold axis
   ! along (1,1,1) reports IPR 2.0 on its nitrogen position, and there is
   ! nothing wrong with that refinement. Telling the two apart needs the
   ! site-symmetry-allowed subspace, i.e. explicit constraints -- see
   ! docs/TASK_CCTBX_INTO_TONTO.md and TASKS_AND_HISTORY.md.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: j,i,n
      STR(len=STR_SIZE) :: lab
      if (NOT self%show_near_0_eigenvectors) return
      if (NOT allocated(self%ls)) return
      if (NOT allocated(self%ls%near_0_IPR)) return
      if (self%ls%near_0<=0) return
      if (maxval(self%ls%near_0_IPR)<=DIFFRACTION_DATA_DELOCALISED_IPR) return
      call flush_(stdout)
      call text_(stdout,"The data does not separate these parameter combinations.")
      call text_(stdout,"A symmetry constraint looks the same as too many parameters.")
      call flush_(stdout)
      do j = 1,self%ls%near_0
         if (self%ls%near_0_IPR(j)<=DIFFRACTION_DATA_DELOCALISED_IPR) cycle
         call show_(stdout,"Parameters mixed in, approx.      =",self%ls%near_0_IPR(j))
         n = size(self%ls%near_0_evecs,1)
         do i = 1,n

            ! Show the parameters with real weight in this direction
            if (abs(self%ls%near_0_evecs(i,j))<TOL(1)) cycle
            ! NOTE: keep both branches to the (STR,REAL) form of "show".
            ! A (STR,INT,REAL) call has no specific and fails to compile --
            ! see CLAUDE.md section 8 on Foo's generic overloads.
            if (allocated(self%ls%param_label)) then
               lab = "   "//trim(self%ls%param_label(i))
            else
               lab = "   parameter "//trim(to_str_(i))
            end if
            call show_(stdout,trim(lab)//" weight",self%ls%near_0_evecs(i,j))
         end do
         call flush_(stdout)
      end do
   end subroutine

   subroutine put_refinement_results(self)
   ! Put the refinement results
      type(DIFFRACTION_DATA_TYPE), IN :: self

      ! Results
      call flush_(stdout)
      call text_(stdout,"============================")
      call text_(stdout,"Structure refinement results")
      call text_(stdout,"============================")
      call flush_(stdout)

      ! Print how it finished
      if (self%refinement_converged) then; call text_(stdout,"Structure refinement converged.")
      else if (self%GoF2_increased) then; call text_(stdout,"WARNING: refinement stopped: GoF has increased.")
      else if (self%too_many_ref_iterations) then; call text_(stdout,"WARNING: refinement stopped: too many iterations.")
      end if

      ! Which parameter combinations the data does not separate
      call put_near_0_eigenvector_report_(self)

      ! Put final fitting statistics
      call put_F_statistics_(self)
   end subroutine

   subroutine put_near_0_evecs(self,n_p)
   ! Put the near zero (linearly dependent) parameter eigenvectors
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT, IN :: n_p
      call flush_(stdout)
      call text_(stdout,"=======================================")
      call text_(stdout,"Near 0 eigenvalues of the normal matrix")
      call text_(stdout,"=======================================")
      call flush_(stdout)
      call show_(stdout,"No. of redundant parameters   =",n_p)
      call show_(stdout,"No. of unique parameters      =",self%n_param)
      call flush_(stdout)
      call show_(stdout,"Near-zero tolerance           =",self%near_0_tol)
      call show_(stdout,"No. of near-zero eigenvalues  =",n_near_0_(self))
      if (n_near_0_(self)>0) then
         if (allocated(self%ls%near_0_evecs)) then
            call flush_(stdout)
            call text_(stdout,"Near-zero eigenvalues:")
            call flush_(stdout)
            call save_style_(stdout)
            call set_real_fortran_fmt_(stdout,"e")
            call put_(stdout,self%ls%near_0_evals)
            call unsave_style_(stdout)
            call flush_(stdout)
            if (self%ls%near_0<=10 OR self%show_near_0_eigenvectors) then
               call text_(stdout,"Near-zero eigenvectors:")
               call flush_(stdout)
               call put_(stdout,self%ls%near_0_evecs)
            else
               call text_(stdout,"Too many near-zero eigenvectors to write out")
            end if
         end if
      end if
   end subroutine


!  ==============
!  Output methods
!  ==============

   subroutine put(self)
   ! Put out the diffraction data to file "out"
      type(DIFFRACTION_DATA_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"================")
      call text_(stdout,"Diffraction data")
      call text_(stdout,"================")
      call flush_(stdout)
      call show_(stdout,"Data kind                     =",self%data_kind)
      call show_(stdout,"Wavelength                    =",to_units_(self%wavelength,"angstrom"))
      call show_(stdout,"Temperature                   =",self%temperature)
      call show_(stdout,"NKA T sequence no.            =",self%NKA_T_sequence_no)
      call flush_(stdout)
      call show_(stdout,"F_sigma_cutoff                =",self%F_sigma_cutoff)
      call show_(stdout,"F_sigma_cutoff                =",self%I_sigma_cutoff)
      call show_(stdout,"F_z_cutoff                    =",self%F_z_cutoff)
      call show_(stdout,"I_z_cutoff                    =",self%I_z_cutoff)
      call show_(stdout,"F_calc_cutoff                 =",self%F_calc_cutoff)
      call show_(stdout,"F_pred_cutoff                 =",self%F_pred_cutoff)
      call show_(stdout,"I_pred_cutoff                 =",self%I_pred_cutoff)
      call show_(stdout,"stl_high_cutoff               =",self%stl_high_cutoff)
      call show_(stdout,"stl_low_cutoff                =",self%stl_low_cutoff)
      call show_(stdout,"stl_limit                     =",self%stl_limit)
      call show_(stdout,"show_rejects?                 =",self%show_rejects)
      call flush_(stdout)
      call show_(stdout,"partition_model               =",self%partition_model)
      call show_(stdout,"RI_l_max                      =",self%RI_l_max)
      call show_(stdout,"RI_exponent_ratio             =",self%RI_exponent_ratio)
      call show_(stdout,"expHAR_power                  =",self%expHAR_power)
      call show_(stdout,"FF_fit_s_max                  =",self%FF_fit_s_max)
      call show_(stdout,"FF_fit_n_gaussians            =",self%FF_fit_n_gaussians)
      call show_(stdout,"FF_fit_n_points               =",self%FF_fit_n_points)
      call show_(stdout,"FF_fit_format                 =",self%FF_fit_format)
      call show_(stdout,"using_one_center_TF_model?    =",using_one_center_TF_model_(self))
      call show_(stdout,"use_Voronoi_atoms?            =",self%use_Voronoi_atoms)
      call show_(stdout,"use_IAM_ITC_FFs?              =",self%use_IAM_ITC_FFs)
      call show_(stdout,"use_KM_atoms?                 =",self%use_KM_atoms)
      call show_(stdout,"use_zero_overlap?             =",self%use_zero_overlap)
      call flush_(stdout)
      call show_(stdout,"refine_scale?               =",self%refine_scale)
      call show_(stdout,"scale_factor                  =",self%scale_factor)
      call show_(stdout,"refine_extinction?          =",self%refine_extinction)
      call show_(stdout,"extinction_factor             =",self%extinction_factor)
      call show_(stdout,"extinction_factor_esd         =",self%extinction_factor_esd)
      call show_(stdout,"Correct dispersion?           =",correct_dispersion_(self))
      if (correct_dispersion_(self)) then
         call show_(stdout,"Add dispersion into F_calc    =",self%add_dispersion_to_F_calc)
         call show_(stdout,"Remove dispersion frm F_exp   =",self%remove_dispersion_from_F_exp)
      end if
      call flush_(stdout)
      call show_(stdout,"n_param                       =",self%n_param)
      call show_(stdout,"n_param_manually_set?         =",self%n_param_manually_set)
      call show_(stdout,"GoF2                          =",self%GoF2)
      call flush_(stdout)
      call show_(stdout,"fit_structure?                =",self%fit_structure)
      call show_(stdout,"refine_structure?             =",self%refine_structure)
      call show_(stdout,"refine_F?                     =",self%refine_F)
      call show_(stdout,"refine_positions_only?        =",self%refine_positions_only)
      call show_(stdout,"refine_H_positions            =",self%refine_H_positions)
      call show_(stdout,"refine_ADPs_only?             =",self%refine_ADPs_only)
      call show_(stdout,"refine_H_ADPs?                =",self%refine_H_ADPs)
      call show_(stdout,"refine_H_U_iso?               =",self%refine_H_U_iso)
      call show_(stdout,"refine_anharmonicity?         =",self%refine_anharmonicity)
      call show_(stdout,"restart_HAR_with_promolecule? =",self%restart_HAR_with_promolecule)
      call show_(stdout,"XCW?                          =",self%XCW)
      call flush_(stdout)
      call show_(stdout,"min_iterations                =",self%min_iterations)
      call show_(stdout,"max_iterations                =",self%max_iterations)
      call show_(stdout,"max_allowed_param_shift       =",self%max_allowed_param_shift)
      call show_(stdout,"tol_for_shift_on_esd          =",self%tol_for_shift_on_esd)
      call show_(stdout,"near_0_tol                    =",self%near_0_tol)
      call flush_(stdout)
      call show_(stdout,"show_fit_output?              =",self%show_fit_output)
      call show_(stdout,"show_near_0_eigenvectors?     =",self%show_near_0_eigenvectors)
      call show_(stdout,"show_correlation_mx?          =",self%show_correlation_mx)
      call show_(stdout,"min_correlation               =",self%min_correlation)
      call flush_(stdout)
      call show_(stdout,"fit_iteration                 =",self%fit_iteration)
      call show_(stdout,"GoF2_fit                      =",self%GoF2_fit)
      call show_(stdout,"GoF2_fit0                     =",self%GoF2_fit0)
      call show_(stdout,"max_shift                     =",self%max_shift)
      call show_(stdout,"max_shift_on_esd              =",self%max_shift_on_esd)
      call show_(stdout,"max_shift_on_esd_ind          =",self%max_shift_on_esd_ind)
      call show_(stdout,"max_shift_on_esd_par          =",self%max_shift_on_esd_par)
      call show_(stdout,"fit_converged?                =",self%fit_converged)
      call show_(stdout,"GoF2_increased?               =",self%GoF2_increased)
      call show_(stdout,"too_many_fit_iterations?      =",self%too_many_fit_iterations)
      call flush_(stdout)
      call show_(stdout,"ref_iteration                 =",self%ref_iteration)
      call show_(stdout,"GoF2_ref                      =",self%GoF2_ref)
      call show_(stdout,"GoF2_ref0                     =",self%GoF2_ref0)
      call show_(stdout,"max_refshift                  =",self%max_refshift)
      call show_(stdout,"max_refshift_on_esd           =",self%max_refshift_on_esd)
      call show_(stdout,"ave_refshift_on_esd           =",self%ave_refshift_on_esd)
      call show_(stdout,"max_refshift_on_esd_ind       =",self%max_refshift_on_esd_ind)
      call show_(stdout,"max_refshift_on_esd_par       =",self%max_refshift_on_esd_par)
      call show_(stdout,"refinement_converged?         =",self%refinement_converged)
      call show_(stdout,"too_many_ref_iterations?      =",self%too_many_ref_iterations)
      if (self%do_residual_cube) then
         call flush_(stdout)
         call show_(stdout,"max_residual                  =",self%max_residual)
         call show_(stdout,"min_residual                  =",self%min_residual)
         call show_(stdout,"rms_residual                  =",self%rms_residual)
         call show_(stdout,"max_atom_residual             =",self%max_atom_residual)
         call show_(stdout,"min_atom_residual             =",self%min_atom_residual)
         call show_(stdout,"min_atom_residual_tag         =",self%min_atom_residual_tag)
         call show_(stdout,"rms_atom_residual             =",self%rms_atom_residual)
      end if
      call flush_(stdout)
      call show_(stdout,"n_0                           =",n_0_eigenvalues_(self))
      call show_(stdout,"near_0                        =",n_near_0_(self))
      if (allocated(self%reflections)) then
         call flush_(stdout)
         call text_(stdout,"===========")
         call text_(stdout,"Reflections")
         call text_(stdout,"===========")
         call flush_(stdout)
         call put_reflection_data_(self)
      end if
   end subroutine

   subroutine put_wavelength(self)
   ! Put out the wavelength - for testing
      type(DIFFRACTION_DATA_TYPE), IN :: self
      call show_(stdout,"DIFFRACTION_DATA wavelength =",to_units_(self%wavelength,"angstrom"))
   end subroutine

   subroutine put_sin_theta_on_lambda(self)
   ! Output sin(theta)/lambda for all reflections.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.PUT:put_sin_theta_on_lambda ... No list of reflections")
   ENSURE(have_indices_(self%reflections),"DIFFRACTION_DATA.PUT:put_sin_theta_on_lambda ... No list of reflections")
      call text_(stdout,"sin(theta)/lambda for the reflections")
      call dash_(stdout,int_fields=3,real_fields=1)
      call put_(stdout,"h",int_width=TRUE)
      call put_(stdout,"k",int_width=TRUE)
      call put_(stdout,"l",int_width=TRUE)
      call put_(stdout,"stl")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=1)
      do n = 1,size(self%reflections)
         call put_(stdout,self%reflections(n)%h1)
         call put_(stdout,self%reflections(n)%h2)
         call put_(stdout,self%reflections(n)%h3)
         call put_(stdout,self%reflections(n)%stl)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=3,real_fields=1)
   end subroutine

   subroutine put_F_statistics(self)
   ! Output the structure factor goodness of fit statistics.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n_unmatched,n_pairs

      ! Show stats without GoF2(N=1)
      call put_F_statistics_(self%reflections,show_GoF2=FALSE)

      ! How many reflections have their Friedel mate in the data? Report the
      ! number of PAIRS, which is zero for the merged data most people supply
      ! and says at a glance that they are merged. The count of reflections
      ! *without* a mate, which this line used to show, equals N_r whenever the
      ! data are merged, and an equal pair of numbers reads like a fault.
      n_unmatched = n_unmatched_Friedel_pairs_(self%reflections)
      n_pairs = (size(self%reflections)-n_unmatched)/2

      ! No. of reflections/parameters
      call show_(stdout,"# of reflections,    N_r       =",size(self%reflections))
      call show_(stdout,"# of Friedel pairs found       =",n_pairs)
      call show_(stdout,"# of fit parameters, N_p       =",self%n_param)

      ! GOF statistics
      call show_(stdout,"GoF^2(N_p)                     =",self%GoF2)
      call show_(stdout,"GoF  (N_p)                     =",sqrt(self%GoF2))
      call show_(stdout,"Effective (mean) sigma^2       =",sigma2_eff_(self%reflections))

      ! Extinction?
      if (self%refine_extinction) then
         call show_(stdout,"Secondary extinction factor    =",self%extinction_factor)
         if (self%extinction_factor_esd>ZERO) then
            call show_(stdout,"Secondary extinction factor esd =",self%extinction_factor_esd)
         end if
      end if

      ! Optimize scale
      if (self%refine_scale AND use_multiple_scale_factors_(self)) then
         call show_(stdout,"Optimize extinction            =",self%refine_extinction)
         call show_(stdout,"Scale factors                  =",self%scale_factors)
      else if (self%refine_scale) then
         call show_(stdout,"Using single scale factor      =",TRUE)
         call show_(stdout,"Scale factor                   =",self%scale_factor)
      else
         call show_(stdout,"Using single scale factor      =",FALSE)
         call show_(stdout,"Scale factor                   =",self%scale_factor)
      end if
   end subroutine

   subroutine put_F_free_stats(self)
   ! Output the structure factor goodness of fit statistics.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      call put_F_free_stats_(self%reflections)
   end subroutine

   subroutine put_I_free_stats(self)
   ! Output the intensity goodness of fit statistics
      type(DIFFRACTION_DATA_TYPE), IN :: self
      call put_I_free_stats_(self%reflections)
      call put_correction_data_(self)
   end subroutine

   subroutine put_correction_data(self)
   ! Output the correction data
      type(DIFFRACTION_DATA_TYPE), IN :: self
      call flush_(stdout)
      call show_(stdout,"Using scale factor(s)          =",self%refine_scale)
      call show_(stdout,"Using extinction?              =",self%refine_extinction)
      call show_(stdout,"Density partition model        =",self%partition_model)
      call show_(stdout,"Is one-center TF model?        =",using_one_center_TF_model_(self))
      call show_(stdout,"Correct dispersion?            =",correct_dispersion_(self))
      if (correct_dispersion_(self)) then
         call show_(stdout,"Add dispersion into F_calc     =",self%add_dispersion_to_F_calc)
         call show_(stdout,"Remove dispersion frm F_exp    =",self%remove_dispersion_from_F_exp)
      end if
      if (self%refine_extinction) then
         call show_(stdout,"Optimize extinction            =",TRUE)
         call show_(stdout,"Secondary extinction factor    =",self%extinction_factor)
         if (self%extinction_factor_esd>ZERO) then
            call show_(stdout,"Secondary extinction factor esd =",self%extinction_factor_esd)
         end if
      else
         call show_(stdout,"Optimize extinction            =",FALSE)
      end if
      if (self%refine_scale AND use_multiple_scale_factors_(self)) then
         call show_(stdout,"Using multiple scale factors   =",TRUE)
         call show_(stdout,"Scale factors                  =",self%scale_factors)
      else if (self%refine_scale) then
         call show_(stdout,"Using single scale factor      =",TRUE)
      else
         call show_(stdout,"Using single scale factor      =",FALSE)
      end if
      call show_(stdout,"Scale factor                   =",self%scale_factor)
   end subroutine

   subroutine put_reflection_data(self)
   ! Output the reflection data and corresponding fit statistic,
   ! if available
      type(DIFFRACTION_DATA_TYPE), IN :: self
      call put_N_worst_reflections_(self)
      call put_reflections_(self)
   end subroutine

   subroutine put_worst_reflections(self)
   ! Put out a selection of the worst-fitting reflections
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n
      if (n_line_items_(stdin)==2) then

         ! Not nice, state not saved
         call read_(stdin,n)
         DIE_IF(is_odd_(n),"DIFFRACTION_DATA.PUT:put_worst_reflections ... must have an even number reflections")
         call put_N_worst_reflections_(self,n)
      else
         call put_N_worst_reflections_(self)
      end if
   end subroutine

   subroutine put_N_worst_reflections_0(self)
   ! Put out a salection of the worst-fitting reflections
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n_refl,n
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.PUT:put_N_worst_reflections_0 ... no reflections")
      if (have_F_pred_(self)) then
         if (have_F_exp_(self)) then
            n_refl = size(self%reflections)
            if (self%n_worst_reflections<0) then

            ! Put out a reasonable no. of bad reflections
               if (n_refl>1000) then; call put_N_worst_reflections_(self,n=40)
               else if (n_refl>100) then; call put_N_worst_reflections_(self,n=20)
               else if (n_refl>50) then; call put_N_worst_reflections_(self,n=10)
               end if
            else

            ! Make sure users no. of reflections is reasonable
               n = min(self%n_worst_reflections,n_refl)
               n = 2*(n/2)
               call put_N_worst_reflections_(self,n)
            end if
         end if
      end if
   end subroutine

   subroutine put_N_worst_reflections_1(self,n)
   ! Show the "n" worst reflections, "n" even: the n/2 with the largest F_z
   ! and the n/2 with the smallest, in two tables, each worst first.
   ! Reflections equal in F_z are ordered by (h,k,l).
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n
      VEC(REAL,:), allocatable :: F_z
      VEC(INT,:), allocatable :: ind,pos,neg,sel
      MAT(INT,:,:), allocatable :: hkl
      INT :: n_refl,n2
   ENSURE(have_F_exp_(self),"DIFFRACTION_DATA.PUT:put_N_worst_reflections_1 ... No experimental data")
   ENSURE(is_even_(n),"DIFFRACTION_DATA.PUT:put_N_worst_reflections_1 ... no. of reflections n is not even")

      ! No. of reflections
      n_refl = size(self%reflections)
      if (n_refl<n OR n<2) return
      n2 = n/2

      ! Sort keys
      call create_(F_z,n_refl)
      F_z = F_z_(self%reflections)
      call create_(hkl,n_refl,3)
      hkl(:,1) = self%reflections(:)%h1
      hkl(:,2) = self%reflections(:)%h2
      hkl(:,3) = self%reflections(:)%h3

      ! The n2 largest and the n2 smallest, each worst first
      call create_(ind,n_refl)
      call quick_sort_(F_z,ind,decreasing_order=TRUE,then_by=hkl)
      call create_(pos,n2); pos = ind(1:n2)
      call quick_sort_(F_z,ind,decreasing_order=FALSE,then_by=hkl)
      call create_(neg,n2); neg = ind(1:n2)
      call create_(sel,n); sel(1:n2) = pos; sel(n2+1:) = neg

      ! Header
      call flush_(stdout)
      call text_(stdout,"=================")
      call text_(stdout,"Worst reflections")
      call text_(stdout,"=================")
      call flush_(stdout)
      call text_(stdout,". Below are the "//trim(to_str_(n))//" worst-fitted reflections by")
      call text_(stdout,"  F_z = (F_exp-F_pred)/F_sigma, in two tables of "//trim(to_str_(n2))//",")
      call text_(stdout,"  each worst first")
      call text_(stdout,". The # column is the position of the reflection in the data set")
      call text_(stdout,". The full list of reflections is in the produced CIF file")
      call flush_(stdout)

      ! Statistics for the n together
      call put_F_statistics_(self%reflections(sel))

      ! Under-estimated, then over-estimated
      call flush_(stdout)
      call text_(stdout,"F_z > 0, F_exp > F_pred, the model under-estimates:")
      call put_structure_factor_table_(self%reflections(pos),index=pos)
      call flush_(stdout)
      call text_(stdout,"F_z < 0, F_exp < F_pred, the model over-estimates:")
      call put_structure_factor_table_(self%reflections(neg),index=neg)

      ! Clean
      call destroy_(sel)
      call destroy_(neg)
      call destroy_(pos)
      call destroy_(ind)
      call destroy_(hkl)
      call destroy_(F_z)
   end subroutine

   subroutine put_reflections(self)
   ! Show all the reflections
      type(DIFFRACTION_DATA_TYPE), IN :: self
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.PUT:put_reflections ... No reflection data")

      ! Header
      call flush_(stdout)
      call text_(stdout,"=================")
      call text_(stdout,"X-ray reflections")
      call text_(stdout,"=================")

      ! Put
      call put_(self%reflections)
   end subroutine

   subroutine put_reflection_intensities(self)
   ! Show all the reflections
      type(DIFFRACTION_DATA_TYPE), IN :: self
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.PUT:put_reflection_intensities ... No reflection data")

      ! Header
      call flush_(stdout)
      call text_(stdout,"=================")
      call text_(stdout,"X-ray intensities")
      call text_(stdout,"=================")

      ! Put
      call put_intensities_(self%reflections)
   end subroutine

   subroutine put_reflection_intensities_XD(self)
   ! Show all the reflections
      type(DIFFRACTION_DATA_TYPE), IN :: self
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.PUT:put_reflection_intensities_XD ... No reflection data")

      ! Header
      call flush_(stdout)
      call text_(stdout,"=================")
      call text_(stdout,"X-ray intensities")
      call text_(stdout,"=================")

      ! Put
      call put_intensities_XD_(self%reflections)
   end subroutine

   subroutine put_labelled_qq_plot(self,prefix)
   ! Output a qq plot to stdout.
   ! It is a plot of the experimental quantile vs expected quantile.
   !
   ! "prefix" heads the files written; CRYSTAL passes the job name.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      STR(len=*), optional, IN :: prefix
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.PUT:put_labelled_qq_plot ... no reflection data")
      call put_labelled_F_qq_plot_(self%reflections,prefix)
   end subroutine

   subroutine put_GOF_vs_STL_table(self)
   ! Output GOF and ratio vs. sin(theta)/lambda bins.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n_sections,i
      REAL :: X_min,X_max,X_bin
      VEC(INT,:), allocatable :: C
      VEC(REAL,:), allocatable :: stl,bin,GOF,ratio,RF,RwF,RF2,RwF2,dF,Fexp,Fz,Feos
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      n_sections = 20

      ! Sin theta on lambda
      call create_(stl,size(self%reflections))
      stl = stl_(self%reflections)

      ! Bin size ...
      X_min = minval(stl)
      X_max = maxval(stl)
      X_bin = (X_max-X_min)/n_sections

      ! Sin theta on lambda bins array for table
      call create_(bin,n_sections)
      bin = [(X_min+i*X_bin,i = 1,n_sections)]

      ! GOF
      call create_(GOF,n_sections)
      call create_(C,n_sections)
      call to_histogram_from_data_(GOF,F_z2_(self%reflections),stl,X_min,X_max,X_bin,C)
      GOF = sqrt(GOF/max(C-self%n_param,1))

      ! ratio
      call create_(ratio,n_sections)
      call to_histogram_from_data_(ratio,F_ratio_(self%reflections),stl,X_min,X_max,X_bin)
      ratio = ratio/max(C,1)

      ! R(F)
      call create_(RF,n_sections)
      call create_(dF,n_sections)
      call create_(Fexp,n_sections)
      call to_histogram_from_data_(dF,abs_delta_F_(self%reflections),stl,X_min,X_max,X_bin)
      call to_histogram_from_data_(Fexp,F_exp_(self%reflections),stl,X_min,X_max,X_bin)
      RF = dF/Fexp
      call destroy_(Fexp)
      call destroy_(dF)

      ! R(F2)
      call create_(RF2,n_sections)
      call create_(dF,n_sections)
      call create_(Fexp,n_sections)
      call to_histogram_from_data_(dF,abs_delta_F2_(self%reflections),stl,X_min,X_max,X_bin)
      call to_histogram_from_data_(Fexp,F2_exp_(self%reflections),stl,X_min,X_max,X_bin)
      RF2 = dF/Fexp
      call destroy_(Fexp)
      call destroy_(dF)

      ! Rw(F)
      call create_(RwF,n_sections)
      call create_(Fz,n_sections)
      call create_(Feos,n_sections)
      call to_histogram_from_data_(Fz,F_z2_(self%reflections),stl,X_min,X_max,X_bin)
      call to_histogram_from_data_(Feos,F_exp_on_sigma_2_(self%reflections),stl,X_min,X_max,X_bin)
      RwF = sqrt(Fz/Feos)
      call destroy_(Feos)
      call destroy_(Fz)

      ! Rw(F2)
      call create_(RwF2,n_sections)
      call create_(Fz,n_sections)
      call create_(Feos,n_sections)
      call to_histogram_from_data_(Fz,F2_z2_(self%reflections),stl,X_min,X_max,X_bin)
      call to_histogram_from_data_(Feos,F_exp_on_sigma_4_(self%reflections),stl,X_min,X_max,X_bin)
      RwF2 = sqrt(Fz/Feos)
      call destroy_(Feos)
      call destroy_(Fz)

      ! Header
      call flush_(stdout)
      call text_(stdout,"========================")
      call text_(stdout,"Fit statistics vs. angle")
      call text_(stdout,"========================")
      call flush_(stdout)
      call text_(stdout,"An analysis of the fit versus sin(theta)/lambda.")
      call flush_(stdout)

      ! Table
      bin = bin*BOHR_PER_ANGSTROM
      call create_(table,8)
      call set_heading_(table(1),"sin(th)/lambda"); call set_values_(table(1),bin)
      call set_subhead_(table(1),"max value/A^-1")
      call set_heading_(table(2),"# of"); call set_values_(table(2),C)
      call set_subhead_(table(2),"refl")
      call set_heading_(table(3),"GOF"); call set_values_(table(3),GOF)
      call set_heading_(table(4),"F_exp"); call set_values_(table(4),ratio)
      call set_subhead_(table(4),"/F_pred")
      call set_heading_(table(5),"R(F)"); call set_values_(table(5),RF)
      call set_heading_(table(6),"Rw(F)"); call set_values_(table(6),RwF)
      call set_heading_(table(7),"R(F2)"); call set_values_(table(7),RF2)
      call set_heading_(table(8),"Rw(F2)"); call set_values_(table(8),RwF2)
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(RwF2)
      call destroy_(RF2)
      call destroy_(RwF)
      call destroy_(RF)
      call destroy_(ratio)
      call destroy_(C)
      call destroy_(GOF)
      call destroy_(bin)
      call destroy_(stl)
   end subroutine

   subroutine put_GOF_vs_F_exp_table(self)
   ! Output GOF and ratio vs. F_exp bins.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n_sections,i
      REAL :: X_min,X_max,X_bin
      VEC(INT,:), allocatable :: C
      VEC(REAL,:), allocatable :: stl,bin,GOF,ratio,RF,RwF,RF2,RwF2,dF,Fexp,Fz,Feos
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      n_sections = 20

      ! F_exp
      call create_(stl,size(self%reflections))
      stl = F_exp_(self%reflections)

      ! Bin size ...
      X_min = minval(stl)
      X_max = maxval(stl)
      X_bin = (X_max-X_min)/n_sections

      ! Sin theta on lambda bins array for table
      call create_(bin,n_sections)
      bin = [(X_min+i*X_bin,i = 1,n_sections)]

      ! GOF
      call create_(GOF,n_sections)
      call create_(C,n_sections)
      call to_histogram_from_data_(GOF,F_z2_(self%reflections),stl,X_min,X_max,X_bin,C)
      GOF = sqrt(GOF/max(C-self%n_param,1))

      ! ratio
      call create_(ratio,n_sections)
      call to_histogram_from_data_(ratio,F_ratio_(self%reflections),stl,X_min,X_max,X_bin)
      ratio = ratio/max(C,1)

      ! R(F)
      call create_(RF,n_sections)
      call create_(dF,n_sections)
      call create_(Fexp,n_sections)
      call to_histogram_from_data_(dF,abs_delta_F_(self%reflections),stl,X_min,X_max,X_bin)
      call to_histogram_from_data_(Fexp,F_exp_(self%reflections),stl,X_min,X_max,X_bin)
      RF = ZERO
      where (abs(Fexp)>TOL(6))
         RF = dF/Fexp
      end where
      call destroy_(Fexp)
      call destroy_(dF)

      ! R(F2)
      call create_(RF2,n_sections)
      call create_(dF,n_sections)
      call create_(Fexp,n_sections)
      call to_histogram_from_data_(dF,abs_delta_F2_(self%reflections),stl,X_min,X_max,X_bin)
      call to_histogram_from_data_(Fexp,F2_exp_(self%reflections),stl,X_min,X_max,X_bin)
      RF2 = ZERO
      where (abs(Fexp)>TOL(6))
         RF2 = dF/Fexp
      end where
      call destroy_(Fexp)
      call destroy_(dF)

      ! Rw(F)
      call create_(RwF,n_sections)
      call create_(Fz,n_sections)
      call create_(Feos,n_sections)
      call to_histogram_from_data_(Fz,F_z2_(self%reflections),stl,X_min,X_max,X_bin)
      call to_histogram_from_data_(Feos,F_exp_on_sigma_2_(self%reflections),stl,X_min,X_max,X_bin)
      RwF = ZERO
      where (abs(Feos)>TOL(6))
         RwF = sqrt(Fz/Feos)
      end where
      call destroy_(Feos)
      call destroy_(Fz)

      ! Rw(F2)
      call create_(RwF2,n_sections)
      call create_(Fz,n_sections)
      call create_(Feos,n_sections)
      call to_histogram_from_data_(Fz,F2_z2_(self%reflections),stl,X_min,X_max,X_bin)
      call to_histogram_from_data_(Feos,F_exp_on_sigma_4_(self%reflections),stl,X_min,X_max,X_bin)
      RwF2 = ZERO
      where (abs(Feos)>TOL(6))
         RwF2 = sqrt(Fz/Feos)
      end where
      call destroy_(Feos)
      call destroy_(Fz)

      ! Header
      call flush_(stdout)
      call text_(stdout,"============================")
      call text_(stdout,"Fit statistics vs. intensity")
      call text_(stdout,"============================")
      call flush_(stdout)
      call text_(stdout,"An analysis of the fit versus F_exp is shown in the table below.")
      call flush_(stdout)

      ! Table
      call create_(table,8)
      call set_heading_(table(1),"F_exp"); call set_values_(table(1),bin)
      call set_subhead_(table(1),"max value")
      call set_heading_(table(2),"# of"); call set_values_(table(2),C)
      call set_subhead_(table(2),"refl")
      call set_heading_(table(3),"GOF"); call set_values_(table(3),GOF)
      call set_heading_(table(4),"F_exp"); call set_values_(table(4),ratio)
      call set_subhead_(table(4),"/F_pred")
      call set_heading_(table(5),"R(F)"); call set_values_(table(5),RF)
      call set_heading_(table(6),"Rw(F)"); call set_values_(table(6),RwF)
      call set_heading_(table(7),"R(F2)"); call set_values_(table(7),RF2)
      call set_heading_(table(8),"Rw(F2)"); call set_values_(table(8),RwF2)
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(RwF2)
      call destroy_(RF2)
      call destroy_(RwF)
      call destroy_(RF)
      call destroy_(ratio)
      call destroy_(C)
      call destroy_(GOF)
      call destroy_(bin)
      call destroy_(stl)
   end subroutine

   subroutine put_GOF_vs_STL_plot(self)
   ! Output GOF vs. sin(theta)/lambda bins.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n_sections,i
      VEC(REAL,:), allocatable :: H
      VEC(INT,:), allocatable :: C
      REAL :: X_min,X_max,X_bin,mid
      n_sections = 20
      X_min = minval(stl_(self%reflections))
      X_max = maxval(stl_(self%reflections))
      X_bin = (X_max-X_min)/n_sections
      call create_(H,n_sections)
      call create_(C,n_sections)
      call to_histogram_from_data_(H,F_z2_(self%reflections),stl_(self%reflections),X_min,X_max,X_bin,C)
      H = sqrt(H/max(C-self%n_param,1))
      call flush_(stdout)
      call text_(stdout,"GoF vs angle plot")
      call flush_(stdout)
      call put_(stdout,"sin(T)/L")
      call put_(stdout,"GOF")
      call flush_(stdout)
      call dash_(stdout,real_fields=2)
      do i = 1,n_sections
         mid = (i-1)*X_min+HALF*X_bin
         call put_(stdout,mid)
         call put_(stdout,H(i))
         call flush_(stdout)
      end do
      call flush_(stdout)
      call destroy_(C)
      call destroy_(H)
   end subroutine

   subroutine put_ratio_vs_STL_plot(self)
   ! Output ratio vs. sin(theta)/lambda bins.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n_sections,i
      VEC(REAL,:), allocatable :: H
      VEC(INT,:), allocatable :: C
      REAL :: X_min,X_max,X_bin,mid
      n_sections = 20
      X_min = minval(stl_(self%reflections))
      X_max = maxval(stl_(self%reflections))
      X_bin = (X_max-X_min)/n_sections
      call create_(H,n_sections)
      call create_(C,n_sections)
      call to_histogram_from_data_(H,F_ratio_(self%reflections),stl_(self%reflections),X_min,X_max,X_bin,C)
      H = H/max(C-self%n_param,1)
      call flush_(stdout)
      call text_(stdout,"Ratio (F_exp/F_pred) vs. angle")
      call flush_(stdout)
      call put_(stdout,"sin(T)/L")
      call put_(stdout,"exp/pred")
      call flush_(stdout)
      call dash_(stdout,real_fields=2)
      do i = 1,n_sections
         mid = (i-1)*X_min+HALF*X_bin
         call put_(stdout,mid)
         call put_(stdout,H(i))
         call flush_(stdout)
      end do
      call flush_(stdout)
      call destroy_(C)
      call destroy_(H)
   end subroutine

   subroutine put_GOF_vs_F_exp_plot(self)
   ! Output GOF vs. F_pred bins.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n_sections,i
      VEC(REAL,:), allocatable :: H
      VEC(INT,:), allocatable :: C
      REAL :: X_min,X_max,X_bin,mid
      n_sections = 20
      X_min = minval(F_exp_(self%reflections))
      X_max = maxval(F_exp_(self%reflections))
      X_bin = (X_max-X_min)/n_sections
      call create_(H,n_sections)
      call create_(C,n_sections)
      call to_histogram_from_data_(H,F_z2_(self%reflections),F_exp_(self%reflections),X_min,X_max,X_bin,C)
      H = sqrt(H/max(C-self%n_param,1))
      call flush_(stdout)
      call text_(stdout,"GoF vs F_exp")
      call flush_(stdout)
      call put_(stdout,"sin(T)/L")
      call put_(stdout,"GOF")
      call flush_(stdout)
      call dash_(stdout,real_fields=2)
      do i = 1,n_sections
         mid = (i-1)*X_min+HALF*X_bin
         call put_(stdout,mid)
         call put_(stdout,H(i))
         call flush_(stdout)
      end do
      call flush_(stdout)
      call destroy_(C)
      call destroy_(H)
   end subroutine

   subroutine put_ratio_vs_F_exp_plot(self)
   ! Output ratio vs. F_exp bins.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: n_sections,i
      VEC(REAL,:), allocatable :: H
      VEC(INT,:), allocatable :: C
      REAL :: X_min,X_max,X_bin,mid
      n_sections = 20
      X_min = minval(F_exp_(self%reflections))
      X_max = maxval(F_exp_(self%reflections))
      X_bin = (X_max-X_min)/n_sections
      call create_(H,n_sections)
      call create_(C,n_sections)
      call to_histogram_from_data_(H,F_ratio_(self%reflections),F_exp_(self%reflections),X_min,X_max,X_bin,C)
      H = H/max(C-self%n_param,1)
      call flush_(stdout)
      call text_(stdout,"Ratio (F_exp/F_pred) vs. F_exp")
      call flush_(stdout)
      call put_(stdout,"sin(T)/L")
      call put_(stdout,"exp/pred")
      call flush_(stdout)
      call dash_(stdout,real_fields=2)
      do i = 1,n_sections
         mid = (i-1)*X_min+HALF*X_bin
         call put_(stdout,mid)
         call put_(stdout,H(i))
         call flush_(stdout)
      end do
      call flush_(stdout)
      call destroy_(C)
      call destroy_(H)
   end subroutine

   subroutine put_F_calc_plots(self,prefix)
   ! Output some different plots about the calculated structure factors.
   !
   ! Each plot gets four files: the data, the label positions, a gnuplot script
   ! describing how to draw it, and the drawn PNG itself. Every data file has
   ! the same layout -- x, y, then h, k, l -- so that the worst outliers can be
   ! labelled with the reflection responsible, which is the reason for looking
   ! at these at all.
   !
   ! "prefix" heads every file written here. CRYSTAL passes the job name, so two
   ! jobs run in one directory no longer overwrite each other's plots.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      STR(len=*), optional, IN :: prefix
      VEC(REAL,:), allocatable :: ext
      VEC(type(REFLECTION_TYPE),:), allocatable :: ext_refs
      VEC(REAL,:), allocatable :: dev,xv
      MAT(INT,:,:), allocatable :: hkl
      INT :: n,n_refl
      REAL :: y,w
      STR(len=STR_SIZE) :: head,data
      head = "stdout"
      if (present(prefix)) head = prefix
      n_refl = size(self%reflections)

      ! x and y for the plot, and the Miller indices that go with them. The
      ! label placer needs all three as arrays, not just written to a file.
      call create_(dev,n_refl)
      call create_(xv,n_refl)
      call create_(hkl,n_refl,3)
      do n = 1,n_refl
         hkl(n,1) = self%reflections(n)%h1
         hkl(n,2) = self%reflections(n)%h2
         hkl(n,3) = self%reflections(n)%h3
      end do
      data = trim(head)//".F_z_vs_stl"
      call redirect_(stdout,data)
      call text_(stdout,"# Scatter plot of F_z = (Fexp-Fpred)/F_sigma vs sin(theta)/lambda :")
      call text_(stdout,"# 1st column: sin(theta)/lambda")
      call text_(stdout,"# 2nd column: (F_exp-F_pred)/F_sigma")
      call text_(stdout,"# Note: F_exp & F_pred may have dispersion removed")
      call text_(stdout,"# Note: Fpred may include extinction correction")
      call text_(stdout,"# 3rd, 4th, 5th columns: h, k, l")
      call flush_(stdout)
      do n = 1,n_refl
         xv(n) = self%reflections(n)%stl
         dev(n) = F_z_(self%reflections(n))
         call put_(stdout,xv(n))
         call put_(stdout,dev(n))
         call put_(stdout,hkl(n,1),width=5)
         call put_(stdout,hkl(n,2),width=5)
         call put_(stdout,hkl(n,3),width=5)
         call flush_(stdout)
      end do
      call revert_(stdout)
      call put_xy_plot_files_(self%reflections,data,xv,dev,hkl,"sin({/Symbol q})/{/Symbol l}","F_z = ( F^{o} - F^{c} ) / {/Symbol s}",x_non_negative=TRUE)
      data = trim(head)//".Delta_F_vs_stl"
      call redirect_(stdout,data)
      call text_(stdout,"# Scatter plot of Delta F = (F_exp-F_pred) vs sin(theta)/lambda :")
      call text_(stdout,"# 1st column: sin(theta)/lambda")
      call text_(stdout,"# 2nd column: Delta F = (F_exp-F_pred)")
      call text_(stdout,"# Note: F_exp & F_pred may have dispersion removed")
      call text_(stdout,"# Note: F_pred may include extinction correction")
      call text_(stdout,"# 3rd, 4th, 5th columns: h, k, l")
      call flush_(stdout)
      do n = 1,n_refl
         xv(n) = self%reflections(n)%stl
         dev(n) = delta_F_(self%reflections(n))
         call put_(stdout,xv(n))
         call put_(stdout,dev(n))
         call put_(stdout,hkl(n,1),width=5)
         call put_(stdout,hkl(n,2),width=5)
         call put_(stdout,hkl(n,3),width=5)
         call flush_(stdout)
      end do
      call revert_(stdout)
      call put_xy_plot_files_(self%reflections,data,xv,dev,hkl,"sin({/Symbol q})/{/Symbol l}","{/Symbol D}F = F^{o} - F^{c}",x_non_negative=TRUE)
      data = trim(head)//".F_z_vs_F_exp"
      call redirect_(stdout,data)
      call text_(stdout,"# Scatter plot of F_z = (Fexp-Fpred)/F_sigma vs F_exp :")
      call text_(stdout,"# 1st column: Fexp")
      call text_(stdout,"# 2nd column: (Fexp-Fpred)/F_sigma")
      call text_(stdout,"# Note: F_exp & F_pred may have dispersion removed")
      call text_(stdout,"# Note: F_pred may include extinction correction")
      call text_(stdout,"# 3rd, 4th, 5th columns: h, k, l")
      call flush_(stdout)
      do n = 1,n_refl
         xv(n) = self%reflections(n)%F_corr
         dev(n) = F_z_(self%reflections(n))
         call put_(stdout,xv(n))
         call put_(stdout,dev(n))
         call put_(stdout,hkl(n,1),width=5)
         call put_(stdout,hkl(n,2),width=5)
         call put_(stdout,hkl(n,3),width=5)
         call flush_(stdout)
      end do
      call revert_(stdout)
      call put_xy_plot_files_(self%reflections,data,xv,dev,hkl,"F^{o}","F_z = ( F^{o} - F^{c} ) / {/Symbol s}",x_non_negative=TRUE)
      if (NOT self%refine_extinction) then
         call destroy_(dev)
         call destroy_(xv)
         call destroy_(hkl)
         return
      end if

      ! Only do this if extinction was used
      call create_(ext_refs,n_refl)
      call create_(ext,n_refl)
      ext = F_scale_and_extn_correction_(self,ONE,self%extinction_factor)
      ! F_exp as well as F_corr: REFLECTION:F_z uses F_corr but
      ! REFLECTION:F_r divides by F_exp, and leaving it unset made the
      ! average correction print as Infinity.
      call set_F_corr_(ext_refs,self%reflections%F_pred/ext)
      call set_F_exp_(ext_refs,self%reflections%F_pred/ext)
      call set_F_pred_(ext_refs,self%reflections%F_pred)
      call set_F_sigma_(ext_refs,self%reflections%F_sigma)
      call destroy_(ext)
      w = ZERO
      y = ZERO
      do n = 1,n_refl
         y = y+abs(F_z_(ext_refs(n)))
         w = w+abs(F_rel_dev_(ext_refs(n)))
      end do
      y = y/n_refl
      w = w/n_refl
      data = trim(head)//".Delta_F_pred_z_vs_F_pred"
      call redirect_(stdout,data)
      call text_(stdout,"# The effects of intensity on extinction:")
      call text_(stdout,"# ")
      call text_(stdout,"# Plot of [F_pred(without ext)-F_pred(with ext)]/F_sigma vs F_pred(without ext):")
      call text_(stdout,"# 1st column: F_pred(without extinction")
      call text_(stdout,"# 2nd column: [F_pred(without ext)-F_pred(with ext)]/F_sigma")
      call text_(stdout,"# Note: Fexp & F_pred may have dispersion removed")
      call text_(stdout,"# ")
      call show_(stdout,"# Average value of abs[Fpred(without ext)-Fcalc(with ext)]/F_sigma  = ",y)
      call show_(stdout,"# Average value of abs(extinction correction)                       = ",w)
      call text_(stdout,"# 3rd, 4th, 5th columns: h, k, l")
      do n = 1,n_refl
         xv(n) = ext_refs(n)%F_pred
         dev(n) = F_z_(ext_refs(n))
         call put_(stdout,xv(n))
         call put_(stdout,dev(n))
         call put_(stdout,hkl(n,1),width=5)
         call put_(stdout,hkl(n,2),width=5)
         call put_(stdout,hkl(n,3),width=5)
         call flush_(stdout)
      end do
      call revert_(stdout)
      call put_xy_plot_files_(self%reflections,data,xv,dev,hkl,"F^{c} (no extinction)","[ F^{c}(no ext) - F^{c}(ext) ] / {/Symbol s}",x_non_negative=TRUE)
      data = trim(head)//".Delta_F_pred_z_vs_stl"
      call redirect_(stdout,data)
      call text_(stdout,"# The effects of scattering angle on extinction:")
      call text_(stdout,"# ")
      call text_(stdout,"# Plot of [Fpred(without ext)-Fpred(with ext)]/F_sigma vs sin(theta)/lambda:")
      call text_(stdout,"# 1st column: sin(theta)/lambda")
      call text_(stdout,"# 2nd column: [F_pred(without ext)-F_pred(with ext)]/F_sigma")
      call text_(stdout,"# Note: F_exp & F_pred may have dispersion removed")
      call text_(stdout,"# 3rd, 4th, 5th columns: h, k, l")
      do n = 1,n_refl
         xv(n) = self%reflections(n)%stl
         dev(n) = F_z_(ext_refs(n))
         call put_(stdout,xv(n))
         call put_(stdout,dev(n))
         call put_(stdout,hkl(n,1),width=5)
         call put_(stdout,hkl(n,2),width=5)
         call put_(stdout,hkl(n,3),width=5)
         call flush_(stdout)
      end do
      call revert_(stdout)
      call put_xy_plot_files_(self%reflections,data,xv,dev,hkl,"sin({/Symbol q})/{/Symbol l}","[ F^{c}(no ext) - F^{c}(ext) ] / {/Symbol s}",x_non_negative=TRUE)
      call destroy_(ext_refs)
      call destroy_(dev)
      call destroy_(xv)
      call destroy_(hkl)
   end subroutine

end module
