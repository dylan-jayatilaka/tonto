!====================================================================
!
! DIFFRACTION_DATA.INQ: 
!
! Diffraction data structure for crystals: inquiry methods
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
module DIFFRACTION_DATA_INQ_MODULE

#  include "macros"

   include "diffraction_data.inq.use"

   implicit none

   include "diffraction_data.inq.int"


contains

!  =======
!  Inquiry
!  =======

   pure function CIF_threshold_expression(self) result (res)
   ! Evaluate the CIF _reflns_threshhold_expression
   ! No refinement info. possible without reflections!
      type(DIFFRACTION_DATA_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      REAL :: cut,but
      res = "F>0"
      cut = self%F_sigma_cutoff
      if (cut>ZERO) res = trim(res)//" & F/u(F)>"//trim(adjustl(to_str_(cut,"f5.1")))
      cut = self%I_sigma_cutoff
      if (cut>ZERO) res = trim(res)//" & F2/u(F2)>"//trim(adjustl(to_str_(cut,"f5.1")))
      cut = self%F_z_cutoff
      if (cut>ZERO) res = trim(res)//" & |F_calc-F|/u(F)<"//trim(adjustl(to_str_(cut,"f5.1")))
      cut = self%F_calc_cutoff
      if (cut>ZERO) res = trim(res)//" & |F_calc|>10^-3"
      cut = self%F_pred_cutoff
      if (cut>ZERO) res = trim(res)//" & |F_pred|>10^-3"
      cut = self%stl_high_cutoff
      if (cut>ZERO) then
         but = cut*BOHR_PER_ANGSTROM
         res = trim(res)//" & sin(theta)/lambda<"//trim(adjustl(to_str_(but,"f5.1")))
      end if
      cut = self%stl_low_cutoff
      if (cut>ZERO) then
         but = cut*BOHR_PER_ANGSTROM
         res = trim(res)//" & sin(theta)/lambda>"//trim(adjustl(to_str_(but,"f5.1")))
      end if
      res = "'"//trim(res)//"'"
   end function

   PURE function lp_factor(self) result (res)
   ! Return the array of the Lorentz Polarization factors for all the
   ! reflections
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,size(self%reflections)) :: res
      INT :: n
      REAL :: c,s,two_theta
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.INQ:lp_factor ... no reflection data")
      do n = 1,size(self%reflections)
         two_theta = TWO*self%reflections(n)%theta
         c = cos(two_theta)
         s = sin(two_theta)
         if (is_zero_(s,TOL(8))) then
            res(n) = ZERO
         ! WARN("lp_factor for (000) reflection set to zero")
         else
            res(n) = (1+c*c)/(TWO*s)
         end if
      end do
   end function

   pure function n_near_0(self) result (res)
   ! The number of near-zero eigenvalues filtered out of the last solve.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: res
      res = 0
      if (allocated(self%ls)) res = self%ls%near_0
   end function

   pure function n_0_eigenvalues(self) result (res)
   ! The number of zero eigenvalues in the last solve.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: res
      res = 0
      if (allocated(self%ls)) res = self%ls%n_0
   end function

   pure function n_constraints(self) result (res)
   ! The number of constraints on the refined parameters: the pADP
   ! components removed by the Jacobian (isotropic ADPs, site symmetry)
   ! plus any directions the eigenvalue filter dropped.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: res
      res = 0
      if (allocated(self%ls)) res = self%ls%near_0+n_X_constraints_(self%ls)
   end function

   pure function have_set_wavelength(self) result (res)
   ! Return TRUE if the wavelength was set i.e. is +ve
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = self%wavelength>ZERO
   end function

   pure function reflection_data_exists(self) result (res)
   ! Return TRUE if reflection data exists
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = allocated(self%reflections)
   end function

   pure function have_F_exp(self) result (res)
   ! Return TRUE if have some experimental structure factors
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (allocated(self%reflections)) then
         if (all(self%reflections%F_exp_set)) then
            res = TRUE
         end if
      end if
      if (res) then
         res = have_nonzero_F_exp_(self%reflections) AND have_nonzero_F_sigma_(self%reflections)
      end if
   end function

   pure function have_nonzero_F_sigma(self) result (res)
   ! Return TRUE if have some F_sigma's
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = have_F_exp_(self)
   end function

   pure function have_I_exp(self) result (res)
   ! Return TRUE if have some I_exp's
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (allocated(self%reflections)) then
         if (all(self%reflections%I_exp_set)) then
            res = TRUE
         end if
      end if
      if (res) then
         res = have_nonzero_I_exp_(self%reflections) AND have_nonzero_I_sigma_(self%reflections)
      end if
   end function

   pure function have_nonzero_I_sigma(self) result (res)
   ! Return TRUE if have some I_sigma's
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = have_I_exp_(self)
   end function

   pure function have_F_calc(self) result (res)
   ! Return TRUE if have some calculated structure factors
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (allocated(self%reflections)) res = have_F_calc_(self%reflections)
   end function

   pure function have_F_pred(self) result (res)
   ! Return TRUE if have some predicted structure factors
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (allocated(self%reflections)) res = have_F_pred_(self%reflections)
   end function

   pure function use_multiple_scale_factors(self) result (res)
   ! Return TRUE if we are supposed to use multiple scale factors.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (allocated(self%reflections)) res = any(self%reflections%group>0)
   end function


!  ===================
!  Inquiry: refinement
!  ===================

   pure function using_one_center_TF_model(self) result (res)
   ! Return if this is a one-center TF model.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = self%partition_model(1:3)=="oc-"
   end function

   PURE function correct_dispersion(self) result (res)
   ! Are we correcting for dispersion by some method?
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = self%add_dispersion_to_F_calc OR self%remove_dispersion_from_F_exp
   end function

   PURE function no_of_3rd_order_atoms(self) result (res)
   ! The number of atoms on which there are third-order anharmonicity
   ! parameters
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: res
      res = 0
      if (allocated(self%refine_3rd_order_for_atom)) res = size(self%refine_3rd_order_for_atom)
   end function

   PURE function no_of_4th_order_atoms(self) result (res)
   ! The number of atoms on which there are third-order anharmonicity
   ! parameters
      type(DIFFRACTION_DATA_TYPE), IN :: self
      INT :: res
      res = 0
      if (allocated(self%refine_4th_order_for_atom)) res = size(self%refine_4th_order_for_atom)
   end function

   PURE function refine_positions_and_ADPs(self) result (res)
   ! A refinement of all positions and ADP's
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = NOT self%refine_positions_only AND NOT self%refine_ADPs_only
   end function

   PURE function refine_3rd_order(self) result (res)
   ! Refineme third-order anharmonicity for any atoms?
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = allocated(self%refine_3rd_order_for_atom)
   end function

   PURE function refine_4th_order(self) result (res)
   ! Refineme fourth-order anharmonicity for any atoms?
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = allocated(self%refine_4th_order_for_atom)
   end function


! ============================
! Inquiry: pruning reflections
! ============================

   PURE function is_prunable(self) result (res)
   ! Return TRUE if there's something to prune via a set cutoff.
   ! NOTE: Zero F_sigma's and stl cutoff's are assumed already 
   !       pruned at input time.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (self%F_sigma_cutoff>ZERO) then
         if (have_F_exp_(self)) then
            res = TRUE
         end if
      end if
      if (self%F_z_cutoff>ZERO) then
         if (have_F_pred_(self%reflections)) then
            res = TRUE
         end if
      end if
      if (self%F_calc_cutoff>ZERO) then
         if (have_F_calc_(self%reflections)) then
            res = TRUE
         end if
      end if
      if (self%F_pred_cutoff>ZERO) then
         if (have_F_pred_(self%reflections)) then
            res = TRUE
         end if
      end if
      if (self%I_sigma_cutoff>ZERO) then
         if (have_I_exp_(self)) then
            res = TRUE
         end if
      end if
   end function

   PURE function is_F_calc_prunable(self) result (res)
   ! Return TRUE if there's something to prune via a set cutoff
   ! where F_calc's are required, and the refinement iteration has
   ! not passed .max_prune_iterations (default -1, so never).
      type(DIFFRACTION_DATA_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (self%F_z_cutoff>ZERO) then
         if (have_F_pred_(self%reflections)) then
            res = TRUE
         end if
      end if
      if (self%F_calc_cutoff>ZERO) then
         if (have_F_calc_(self%reflections)) then
            res = TRUE
         end if
      end if
      if (self%F_pred_cutoff>ZERO) then
         if (have_F_pred_(self%reflections)) then
            res = TRUE
         end if
      end if
      res = res AND self%ref_iteration<=self%max_prune_iterations
   end function


! ======================================
! Scale & extinction factor optimisation
! ======================================

   PURE function F_scale_and_extn_correction_0(self) result (res)
   ! Return the extinction correction  factors "res" to the calculated
   ! individual structure factors. NOTE: also does scale factor
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,size(self%reflections)) :: res
      if (use_multiple_scale_factors_(self)) then

         ! Note plural "s" of scale_factors
         res = F_scale_and_extn_correction_(self,self%scale_factors,self%extinction_factor)
      else
         res = F_scale_and_extn_correction_(self,self%scale_factor,self%extinction_factor)
      end if
   end function

   PURE function I_scale_and_extn_correction_0(self) result (res)
   ! Return the I extinction correction  factors "res" to the calculated
   ! individual structure factors. NOTE: also does scale factor
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,size(self%reflections)) :: res
      if (use_multiple_scale_factors_(self)) then

         ! Note plural "s" of scale_factors
         res = I_scale_and_extn_correction_(self,self%scale_factors,self%extinction_factor)
      else
         res = I_scale_and_extn_correction_(self,self%scale_factor,self%extinction_factor)
      end if
   end function

   PURE function F_scale_and_extn_correction_1(self,scale_factor,extinction_factor) result (res)
   ! Return the F extinction correction  factors "res" to the calculated
   ! individual structure factors. NOTE: this routine also does scaling
   ! corrections without extinction.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      REAL, IN :: scale_factor
      REAL, IN :: extinction_factor
      VEC(REAL,size(self%reflections)) :: res
      CPX :: F_calc
      REAL :: F_abs2,val
      VEC(REAL,:), allocatable :: angle_part
      INT :: n_refl,n
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.INQ:F_scale_and_extn_correction_1 ... no reflection data")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.INQ:F_scale_and_extn_correction_1 ... no calculated structure factors")
      n_refl = size(self%reflections)
      if (is_zero_(extinction_factor,TOL(9))) then
         res = scale_factor
      else
         call create_(angle_part,n_refl)
         angle_part = extinction_angle_part_(self)
         do n = 1,n_refl
            F_calc = self%reflections(n)%F_calc
            F_abs2 = REALIFY(F_calc*conjg(F_calc))
            val = ONE+extinction_factor*F_abs2*angle_part(n)
            res(n) = scale_factor/sqrt(sqrt(val))
         end do
         call destroy_(angle_part)
      end if
   end function

   PURE function I_scale_and_extn_correction_1(self,scale_factor,extinction_factor) result (res)
   ! Return the extinction and scale correction factors "res" for the
   ! calculated intensities. The amplitude correction is
   ! [1+x|F_calc|^2 A]^(-1/4), so the intensity correction is its square.
   ! NOTE: this routine also does scaling corrections without extinction.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      REAL, IN :: scale_factor
      REAL, IN :: extinction_factor
      VEC(REAL,size(self%reflections)) :: res
      CPX :: F_calc
      REAL :: F_abs2,val
      VEC(REAL,:), allocatable :: angle_part
      INT :: n_refl,n
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.INQ:I_scale_and_extn_correction_1 ... no reflection data")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.INQ:I_scale_and_extn_correction_1 ... no calculated structure factors")
      n_refl = size(self%reflections)
      if (is_zero_(extinction_factor,TOL(9))) then
         res = scale_factor
      else
         call create_(angle_part,n_refl)
         angle_part = extinction_angle_part_(self)
         do n = 1,n_refl
            F_calc = self%reflections(n)%F_calc
            F_abs2 = REALIFY(F_calc*conjg(F_calc))
            val = ONE+extinction_factor*F_abs2*angle_part(n)
            res(n) = scale_factor/sqrt(val)
         end do
         call destroy_(angle_part)
      end if
   end function

   PURE function F_scale_and_extn_correction_2(self,scale_factors,extinction_factor) result (res)
   ! Return the F extinction correction factors "res" to the calculated
   ! individual structure factors. WARNING: this routine in fact does no
   ! extinction correction, only multiple scale factors.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,:), IN :: scale_factors
      REAL, IN :: extinction_factor
      VEC(REAL,size(self%reflections)) :: res
      CPX :: F_calc
      VEC(REAL,:), allocatable :: angle_part
      INT :: g,n,n_refl,n_group
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.INQ:F_scale_and_extn_correction_2 ... no reflection data")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.INQ:F_scale_and_extn_correction_2 ... no calculated structure factors")
   ENSURE(size(scale_factors)==maxval(self%reflections%group),"DIFFRACTION_DATA.INQ:F_scale_and_extn_correction_2 ... wrong # of groups")
      n_refl = size(self%reflections)
      n_group = size(scale_factors)
      if (is_zero_(extinction_factor,TOL(9))) then
         do g = 1,n_group
            do n = 1,n_refl
               if (self%reflections(n)%group==g) then
                  res(n) = scale_factors(g)
               end if
            end do
         end do
      else
         call create_(angle_part,n_refl)
         angle_part = extinction_angle_part_(self)
         do g = 1,n_group
            do n = 1,n_refl
               if (self%reflections(n)%group==g) then
                  F_calc = self%reflections(n)%F_calc
                  res(n) = scale_factors(g)/sqrt(sqrt(ONE+extinction_factor*REALIFY(F_calc*conjg(F_calc))*angle_part(n)))
               end if
            end do
         end do
         call destroy_(angle_part)
      end if
   end function

   PURE function I_scale_and_extn_correction_2(self,scale_factors,extinction_factor) result (res)
   ! Return the extinction and scale correction factors "res" for the
   ! calculated intensities, with multiple scale factors.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,:), IN :: scale_factors
      REAL, IN :: extinction_factor
      VEC(REAL,size(self%reflections)) :: res
      CPX :: F_calc
      REAL :: F_abs2,val
      VEC(REAL,:), allocatable :: angle_part
      INT :: g,n,n_refl,n_group
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.INQ:I_scale_and_extn_correction_2 ... no reflection data")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.INQ:I_scale_and_extn_correction_2 ... no calculated structure factors")
   ENSURE(size(scale_factors)==maxval(self%reflections%group),"DIFFRACTION_DATA.INQ:I_scale_and_extn_correction_2 ... wrong # of groups")
      n_refl = size(self%reflections)
      n_group = size(scale_factors)
      if (is_zero_(extinction_factor,TOL(9))) then
         do g = 1,n_group
            do n = 1,n_refl
               if (self%reflections(n)%group==g) then
                  res(n) = scale_factors(g)
               end if
            end do
         end do
      else
         call create_(angle_part,n_refl)
         angle_part = extinction_angle_part_(self)
         do g = 1,n_group
            do n = 1,n_refl
               if (self%reflections(n)%group==g) then
                  F_calc = self%reflections(n)%F_calc
                  F_abs2 = REALIFY(F_calc*conjg(F_calc))
                  val = ONE+extinction_factor*F_abs2*angle_part(n)
                  res(n) = scale_factors(g)/sqrt(val)
               end if
            end do
         end do
         call destroy_(angle_part)
      end if
   end function

   PURE function extinction_angle_part(self) result (res)
   ! Return the reflection-dependent part of the SHELXL empirical
   ! extinction correction, eq (62) of Bourhis et al. (2015):
   !
   !    F_calc' = k F_calc [1 + 0.001 x |F_calc|^2 lambda^3 / sin(2 theta)]^(-1/4)
   !
   ! The wavelength is held in bohr and converted to Angstrom here, which
   ! is the unit the 0.001 of the SHELXL convention is defined against.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,size(self%reflections)) :: res
      INT :: n
      REAL :: lambda3,s
   ENSURE(self%wavelength>ZERO,"DIFFRACTION_DATA.INQ:extinction_angle_part ... extinction needs a wavelength")
      lambda3 = self%wavelength*ANGSTROM_PER_BOHR
      lambda3 = lambda3*lambda3*lambda3
      do n = 1,size(self%reflections)
         s = sin(TWO*self%reflections(n)%theta)
         res(n) = TOL(3)*lambda3/max(s,TOL(6))
      end do
   end function

   PURE function d_F_pred_d_F_abs(self) result (res)
   ! Return the derivative of F_pred with respect to |F_calc|, one value per
   ! reflection. This is the scale and extinction factor with the dependence
   ! of the extinction factor itself on |F_calc| included:
   !
   !    F_pred = s |F_calc| y,  with  y = [1 + x |F_calc|^2 A]^(-1/4)
   !    d F_pred / d|F_calc| = s y [1 - (1/2) y^4 x A |F_calc|^2]
   !
   ! With no extinction y is one and this is just the scale factor, so
   ! callers need no test of their own. Same quantity as the "dX" factor in
   ! .d_F_pred_dX, which the least squares has always used.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,size(self%reflections)) :: res
      CPX :: F_calc
      VEC(REAL,:), allocatable :: angle_part,ext
      REAL :: x,F_abs2,val
      INT :: n,n_refl
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.INQ:d_F_pred_d_F_abs ... no reflection data")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.INQ:d_F_pred_d_F_abs ... no calculated structure factors")
      if (NOT self%refine_extinction) then
         res = F_scale_and_extn_correction_(self)
         return
      end if
      n_refl = size(self%reflections)
      x = self%extinction_factor
      call create_(ext,n_refl)
      call create_(angle_part,n_refl)
      ext = F_scale_and_extn_correction_(self)
      angle_part = extinction_angle_part_(self)
      do n = 1,n_refl
         F_calc = self%reflections(n)%F_calc
         F_abs2 = REALIFY(F_calc*conjg(F_calc))
         val = ONE+x*angle_part(n)*F_abs2
         res(n) = ext(n)*(ONE-HALF*x*angle_part(n)*F_abs2/val)
      end do
      call destroy_(angle_part)
      call destroy_(ext)
   end function

   PURE function GoF2F(self,p) result (res)
   ! Make the GoF2 between the calculated and experimental structure factors
   ! with extinction and scale parameters stored in vector p.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,:), IN :: p
      REAL :: res
      REAL :: tmp,tmp1
      VEC(REAL,:), allocatable :: ext
      type(REFLECTION_TYPE) :: ref
      INT :: n,n_refl
   ENSURE(have_F_exp_(self),"DIFFRACTION_DATA.INQ:GoF2F ... no experimental structure factors")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.INQ:GoF2F ... no calculated structure factors")
   ENSURE(size(p)==2,"DIFFRACTION_DATA.INQ:GoF2F ... wrong size, p")
      n_refl = size(self%reflections)
      call create_(ext,n_refl)
      ext = F_scale_and_extn_correction_(self,p(1),p(2))
      tmp = ZERO
      do n = 1,n_refl
         ref = self%reflections(n)
         tmp1 = (abs(ref%F_calc)*ext(n)-ref%F_corr)/ref%F_sigma
         tmp = tmp+tmp1*tmp1
      end do
      res = tmp/max(n_refl-self%n_param,1)
      call destroy_(ext)
   end function

   PURE function GoF2I(self,p) result (res)
   ! Make the GoF2 between the calculated and experimental squared
   ! structure factors
   ! with extinction and scale parameters stored in vector p.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,:), IN :: p
      REAL :: res
      REAL :: tmp,tmp1
      VEC(REAL,:), allocatable :: ext
      type(REFLECTION_TYPE) :: ref
      INT :: n,n_refl
   ENSURE(have_I_exp_(self),"DIFFRACTION_DATA.INQ:GoF2I ... no experimental structure factors")
   ENSURE(have_I_calc_(self%reflections),"DIFFRACTION_DATA.INQ:GoF2I ... no calculated structure factors")
   ENSURE(have_nonzero_I_sigma_(self%reflections),"DIFFRACTION_DATA.INQ:GoF2I ... no structure factor errors")
   ENSURE(size(p)==2,"DIFFRACTION_DATA.INQ:GoF2I ... wrong size, p")
      n_refl = size(self%reflections)
      call create_(ext,n_refl)
      ext = I_scale_and_extn_correction_(self,p(1),p(2))
      tmp = ZERO
      do n = 1,n_refl
         ref = self%reflections(n)
         tmp1 = (ref%I_calc*ext(n)-ref%I_exp)/ref%I_sigma
         tmp = tmp+tmp1*tmp1
      end do
      res = tmp/max(n_refl-self%n_param,1)
      call destroy_(ext)
   end function

   PURE function d_GoF2F(self,p) result (res)
   ! Return the derivative of the GoF2 with respect to the .scale_factor in
   ! p(1), and with respect to .extinction_factor in p(2). This routine is for
   ! use in the BFGS minimiser.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,:), IN :: p
      VEC(REAL,size(p)) :: res
   ENSURE(size(p)==2,"DIFFRACTION_DATA.INQ:d_GoF2F ... wrong size, p")
      res(1) = d_GoF2F_d_scale_(self,p)
      res(2) = d_GoF2F_d_ext_(self,p)
   end function

   PURE function d_GoF2I(self,p) result (res)
   ! Return the derivative of the GoF2 with respect to the .scale_factor in
   ! p(1), and with respect to .extinction_factor in p(2). This routine is for
   ! use in the BFGS minimiser.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,:), IN :: p
      VEC(REAL,size(p)) :: res
   ENSURE(size(p)==2,"DIFFRACTION_DATA.INQ:d_GoF2I ... wrong size, p")
      res(1) = d_GoF2I_d_scale_(self,p)
      res(2) = d_GoF2I_d_ext_(self,p)
   end function

   PURE function F_extinction_factor_esd(self,p) result (res)
   ! Return the error in the extinction factor from the joint
   ! scale/extinction weighted least-squares fit to the amplitudes.
   ! The covariance is the inverted Gauss-Newton normal matrix, scaled by
   ! the same GoF^2 as the rest of the refinement. Negative if unavailable.
   ! NOTE: conditional on the current structure factors. Extinction sits
   ! outside the pADP normal matrix, so its correlation with the
   ! structural parameters is not included.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,:), IN :: p
      REAL :: res
      VEC(REAL,:), allocatable :: angle_part,ext
      type(REFLECTION_TYPE) :: ref
      REAL :: F_abs,extn,sig2,d_scale,d_ext
      REAL :: normal11,normal12,normal22,det
      INT :: n,n_refl
   ENSURE(have_F_exp_(self),"DIFFRACTION_DATA.INQ:F_extinction_factor_esd ... no experimental structure factors")
   ENSURE(have_nonzero_F_sigma_(self),"DIFFRACTION_DATA.INQ:F_extinction_factor_esd ... no structure factor errors")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.INQ:F_extinction_factor_esd ... no calculated structure factors")
   ENSURE(size(p)==2,"DIFFRACTION_DATA.INQ:F_extinction_factor_esd ... wrong size, p")
      res = -ONE
      n_refl = size(self%reflections)
      if (n_refl<=self%n_param) return
      call create_(angle_part,n_refl)
      call create_(ext,n_refl)
      angle_part = extinction_angle_part_(self)
      ext = F_scale_and_extn_correction_(self,ONE,p(2))
      normal11 = ZERO
      normal12 = ZERO
      normal22 = ZERO
      do n = 1,n_refl
         ref = self%reflections(n)
         F_abs = abs(ref%F_calc)
         extn = ext(n)
         sig2 = ONE/(ref%F_sigma*ref%F_sigma)

         ! d F_pred / d scale, and d F_pred / d extinction
         d_scale = F_abs*extn
         d_ext = -QUARTER*p(1)*F_abs*F_abs*F_abs*angle_part(n)*extn*extn*extn*extn*extn
         normal11 = normal11+d_scale*d_scale*sig2
         normal12 = normal12+d_scale*d_ext*sig2
         normal22 = normal22+d_ext*d_ext*sig2
      end do

      ! The (2,2) element of the inverted 2x2 normal matrix
      det = normal11*normal22-normal12*normal12
      if (normal11>ZERO AND det>ZERO) then
         res = sqrt(max(ZERO,GoF2F_(self,p)*normal11/det))
      end if
      call destroy_(ext)
      call destroy_(angle_part)
   end function

   PURE function I_extinction_factor_esd(self,p) result (res)
   ! Return the error in the extinction factor from the joint
   ! scale/extinction weighted least-squares fit to the intensities.
   ! See the amplitude version above for the caveat.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,:), IN :: p
      REAL :: res
      VEC(REAL,:), allocatable :: angle_part,ext
      type(REFLECTION_TYPE) :: ref
      REAL :: I_calc,extn,sig2,d_scale,d_ext
      REAL :: normal11,normal12,normal22,det
      INT :: n,n_refl
   ENSURE(have_I_exp_(self),"DIFFRACTION_DATA.INQ:I_extinction_factor_esd ... no experimental intensities")
   ENSURE(have_I_calc_(self%reflections),"DIFFRACTION_DATA.INQ:I_extinction_factor_esd ... no calculated intensities")
   ENSURE(have_nonzero_I_sigma_(self%reflections),"DIFFRACTION_DATA.INQ:I_extinction_factor_esd ... no intensity errors")
   ENSURE(size(p)==2,"DIFFRACTION_DATA.INQ:I_extinction_factor_esd ... wrong size, p")
      res = -ONE
      n_refl = size(self%reflections)
      if (n_refl<=self%n_param) return
      call create_(angle_part,n_refl)
      call create_(ext,n_refl)
      angle_part = extinction_angle_part_(self)
      ext = I_scale_and_extn_correction_(self,ONE,p(2))
      normal11 = ZERO
      normal12 = ZERO
      normal22 = ZERO
      do n = 1,n_refl
         ref = self%reflections(n)
         I_calc = ref%I_calc
         extn = ext(n)
         sig2 = ONE/(ref%I_sigma*ref%I_sigma)
         d_scale = I_calc*extn
         d_ext = -HALF*p(1)*I_calc*I_calc*angle_part(n)*extn*extn*extn
         normal11 = normal11+d_scale*d_scale*sig2
         normal12 = normal12+d_scale*d_ext*sig2
         normal22 = normal22+d_ext*d_ext*sig2
      end do
      det = normal11*normal22-normal12*normal12
      if (normal11>ZERO AND det>ZERO) then
         res = sqrt(max(ZERO,GoF2I_(self,p)*normal11/det))
      end if
      call destroy_(ext)
      call destroy_(angle_part)
   end function

   PURE function d_GoF2F_d_scale(self,p) result (res)
   ! Derivative of the GoF^2 for F with respect to the scale factor
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,:), IN :: p
      REAL :: res
      REAL :: tmp,F_pred
      VEC(REAL,:), allocatable :: ext
      type(REFLECTION_TYPE) :: ref
      INT :: n,n_refl
   ENSURE(have_F_exp_(self),"DIFFRACTION_DATA.INQ:d_GoF2F_d_scale ... no experimental structure factors")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.INQ:d_GoF2F_d_scale ... no calculated structure factors")
   ENSURE(size(p)==2,"DIFFRACTION_DATA.INQ:d_GoF2F_d_scale ... wrong size, p")
      n_refl = size(self%reflections)
      call create_(ext,n_refl)
      ext = F_scale_and_extn_correction_(self,p(1),p(2))
      tmp = ZERO
      do n = 1,n_refl
         ref = self%reflections(n)
         F_pred = abs(ref%F_calc)*ext(n)
         tmp = tmp+F_pred*(F_pred-ref%F_corr)/(ref%F_sigma*ref%F_sigma)
      end do
      res = TWO*tmp/(p(1)*max(n_refl-self%n_param,1))
      call destroy_(ext)
   end function

   PURE function d_GoF2I_d_scale(self,p) result (res)
   ! Derivative of the GoF^2 for I with respect to the scale factor
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,:), IN :: p
      REAL :: res
      REAL :: tmp,I_pred
      VEC(REAL,:), allocatable :: ext
      type(REFLECTION_TYPE) :: ref
      INT :: n,n_refl
   ENSURE(have_I_exp_(self),"DIFFRACTION_DATA.INQ:d_GoF2I_d_scale ... no experimental structure factors")
   ENSURE(have_I_calc_(self%reflections),"DIFFRACTION_DATA.INQ:d_GoF2I_d_scale ... no calculated structure factors")
   ENSURE(have_nonzero_I_sigma_(self%reflections),"DIFFRACTION_DATA.INQ:d_GoF2I_d_scale ... no structure factor errors")
   ENSURE(size(p)==2,"DIFFRACTION_DATA.INQ:d_GoF2I_d_scale ... wrong size, p")
      n_refl = size(self%reflections)
      call create_(ext,n_refl)
      ext = I_scale_and_extn_correction_(self,p(1),p(2))
      tmp = ZERO
      do n = 1,n_refl
         ref = self%reflections(n)
         I_pred = ref%I_calc*ext(n)
      ! tmp = tmp + I_pred*(I_pred-ref.I_corr) / (ref.I_sigma*ref.I_sigma)
         tmp = tmp+I_pred*(I_pred-ref%I_exp)/(ref%I_sigma*ref%I_sigma)
      end do
      res = TWO*tmp/(p(1)*max(n_refl-self%n_param,1))
      call destroy_(ext)
   end function

   PURE function d_GoF2F_d_ext(self,p) result (res)
   ! Derivative of the GoF^2 for F with respect to the extinction parameter.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,:), IN :: p
      REAL :: res
      REAL :: tmp,extn,p1
      VEC(REAL,:), allocatable :: angle_bit,ext
      type(REFLECTION_TYPE) :: ref
      INT :: n,n_refl
   ENSURE(have_F_exp_(self),"DIFFRACTION_DATA.INQ:d_GoF2F_d_ext ... no experimental structure factors")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.INQ:d_GoF2F_d_ext ... no calculated structure factors")
   ENSURE(size(p)==2,"DIFFRACTION_DATA.INQ:d_GoF2F_d_ext ... wrong size, p")
      n_refl = size(self%reflections)
      call create_(angle_bit,n_refl)
      call create_(ext,n_refl)
      angle_bit = extinction_angle_part_(self)
      p1 = p(1)
      ext = F_scale_and_extn_correction_(self,ONE,p(2))
      tmp = ZERO
      do n = 1,n_refl
         ref = self%reflections(n)
         extn = ext(n)
         tmp = tmp+(p1*abs(ref%F_calc)*extn-ref%F_corr)*extn*extn*extn*extn*extn*abs(ref%F_calc)*abs(ref%F_calc)*abs(ref%F_calc)*angle_bit(n)/(ref%F_sigma*ref%F_sigma)
      end do
      res = -p1*tmp/(TWO*max(n_refl-self%n_param,1))
      call destroy_(ext)
      call destroy_(angle_bit)
   end function

   PURE function d_GoF2I_d_ext(self,p) result (res)
   ! Derivative of the GoF^2 for I with respect to the extinction parameter.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,:), IN :: p
      REAL :: res
      REAL :: tmp,extn,p1,I_calc
      VEC(REAL,:), allocatable :: angle_bit,ext
      type(REFLECTION_TYPE) :: ref
      INT :: n,n_refl
   ENSURE(have_I_exp_(self),"DIFFRACTION_DATA.INQ:d_GoF2I_d_ext ... no experimental intensities")
   ENSURE(have_I_calc_(self%reflections),"DIFFRACTION_DATA.INQ:d_GoF2I_d_ext ... no calculated intensities")
   ENSURE(have_nonzero_I_sigma_(self%reflections),"DIFFRACTION_DATA.INQ:d_GoF2I_d_ext ... no intensity errors")
   ENSURE(size(p)==2,"DIFFRACTION_DATA.INQ:d_GoF2I_d_ext ... wrong size, p")
      n_refl = size(self%reflections)
      call create_(angle_bit,n_refl)
      call create_(ext,n_refl)
      angle_bit = extinction_angle_part_(self)
      p1 = p(1)
      ext = I_scale_and_extn_correction_(self,ONE,p(2))
      tmp = ZERO
      do n = 1,n_refl
         ref = self%reflections(n)
         extn = ext(n)
         I_calc = ref%I_calc
         tmp = tmp+(p1*I_calc*extn-ref%I_exp)*extn*extn*extn*I_calc*I_calc*angle_bit(n)/(ref%I_sigma*ref%I_sigma)
      end do
      res = -p1*tmp/max(n_refl-self%n_param,1)
      call destroy_(ext)
      call destroy_(angle_bit)
   end function


! ============================
! Structure factor derivatives
! ============================

   PURE subroutine d_F_pred_dX(self,res,sig,del,dFdX)
   ! Evaluate "res", the derivative of the F_pred = s*|F_calc| with
   ! respect to some parameters X (e.g. thermal parameters) given
   ! "dFdX", the derivatives of |F_calc|  w.r.t. the X parameters.
   ! Includes derivatives w.r.t. the scale factor, but extinction is
   ! treated as a constant and should be OFF for exact results. Also
   ! makes "sig" and "del" required for the normal equations.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: res
      VEC(REAL,:), OUT :: sig
      VEC(REAL,:), OUT :: del
      MAT(REAL,:,:), IN :: dFdX
      type(REFLECTION_TYPE) :: ref
      VEC(REAL,:), allocatable :: Ft,Fx,dt,db,ext,angle_bit,d_scale
      REAL :: F_exp,F_sig,F_abs,F_til
      REAL :: sig2,F_es2,F_ts2,ext4,dX
      REAL :: val1,val2,dF,dsp
      REAL :: top,bot,extn,fac
      INT :: n_refl,n_p,p,n
   ENSURE(have_F_exp_(self),"DIFFRACTION_DATA.INQ:d_F_pred_dX ... no experimental structure factors")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.INQ:d_F_pred_dX ... no calculated structure factors")
   ENSURE(size(del,1)==size(self%reflections),"DIFFRACTION_DATA.INQ:d_F_pred_dX ... wrong size, del")
   ENSURE(size(res,1)==size(self%reflections),"DIFFRACTION_DATA.INQ:d_F_pred_dX ... wrong size, res")
   ENSURE(size(dFdX,1)==size(self%reflections),"DIFFRACTION_DATA.INQ:d_F_pred_dX ... wrong size, dF")
   ENSURE(size(dFdX,2)==size(res,2),"DIFFRACTION_DATA.INQ:d_F_pred_dX ... incompatible res, dFdX")

      ! Constants
      n_refl = size(self%reflections)
      n_p = size(dFdX,2)

      ! Temporaries
      call create_(Ft,n_refl)
      call create_(Fx,n_refl)
      call create_(dt,n_refl)
      call create_(db,n_refl)

      ! Extinction factor part
      call create_(ext,n_refl)
      ext = F_scale_and_extn_correction_(self,ONE,self%extinction_factor)

      ! Angle bit, extinction factor folded in. Not evaluated when
      ! extinction is off: the angle part needs a wavelength, which data
      ! refined without extinction need not have.
      call create_(angle_bit,n_refl)
      if (is_zero_(self%extinction_factor,TOL(9))) then
         angle_bit = ZERO
      else
         angle_bit = self%extinction_factor*extinction_angle_part_(self)
      end if

      ! Make temporaries for derivative of scale, extinction, F_abs
      top = ZERO
      bot = ZERO
      do n = 1,n_refl
         extn = ext(n)
         ref = self%reflections(n)
         F_exp = ref%F_corr  ! NOTE
         F_sig = ref%F_sigma
         F_abs = abs(ref%F_calc)
         F_til = extn*F_abs
         sig2 = ONE/(F_sig*F_sig)
         F_es2 = F_exp*sig2
         F_ts2 = F_til*sig2
         top = top+F_til*F_es2
         bot = bot+F_til*F_ts2
         ext4 = extn*extn
         ext4 = ext4*ext4
         dX = -HALF*ext4*angle_bit(n)*F_abs
         dX = ONE+F_abs*dX
         dt(n) = F_es2*extn*dX
         db(n) = extn*F_ts2*dX
         Ft(n) = F_til
         Fx(n) = self%scale_factor*extn*dX

         ! Return these
         sig(n) = sig2
         del(n) = (F_exp-ref%F_pred)*sig2
      end do
      call destroy_(angle_bit)
      call destroy_(ext)

      ! Derivative of the scale factor
      call create_(d_scale,n_p)
      bot = ONE/bot
      fac = -TWO*top*bot*bot
      do p = 1,n_p
         val1 = ZERO
         val2 = ZERO
         do n = 1,n_refl
            dF = dFdX(n,p)
            val1 = val1+dt(n)*dF
            val2 = val2+db(n)*dF
         end do
         d_scale(p) = bot*val1+fac*val2
      end do
      call destroy_(db)
      call destroy_(dt)

      ! Derivative of F_pred or F^m
      do p = 1,n_p
         dsp = d_scale(p)
         do n = 1,n_refl
            dF = dFdX(n,p)
            res(n,p) = dsp*Ft(n)+Fx(n)*dF
         end do
      end do

      ! Clean up
      call destroy_(d_scale)
      call destroy_(Fx)
      call destroy_(Ft)
   end subroutine

   PURE subroutine d_I_pred_dX(self,res,sig,del,dIdX)
   ! Evaluate "res", the derivative of the I_pred = s*I_calc with
   ! respect to some parameters X (e.g. thermal parameters) given
   ! "dIdX", the derivatives of I_calc = |F_calc|^2 w.r.t.  the X
   ! parameters. Includes derivatives w.r.t. the scale factor, but
   ! extinction is treated as a constant and should be OFF for exact
   ! results. Also makes "sig" and "del" required for the normal
   ! equations.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: res
      VEC(REAL,:), OUT :: sig
      VEC(REAL,:), OUT :: del
      MAT(REAL,:,:), IN :: dIdX
      type(REFLECTION_TYPE) :: ref
      VEC(REAL,:), allocatable :: It,Ix,dt,db,ext,angle_bit,d_scale
      REAL :: I_exp,I_sig,I_abs,I_til
      REAL :: sig2,I_es2,I_ts2,ext4,dX
      REAL :: val1,val2,dI,dsp
      REAL :: top,bot,extn,fac
      INT :: n_refl,n_p,p,n
   ENSURE(have_I_exp_(self),"DIFFRACTION_DATA.INQ:d_I_pred_dX ... no I_exp")
   ENSURE(have_I_calc_(self%reflections),"DIFFRACTION_DATA.INQ:d_I_pred_dX ... no I_calc")
   ENSURE(have_nonzero_I_sigma_(self%reflections),"DIFFRACTION_DATA.INQ:d_I_pred_dX ... no I_sigma")
   ENSURE(size(del,1)==size(self%reflections),"DIFFRACTION_DATA.INQ:d_I_pred_dX ... wrong size, del")
   ENSURE(size(res,1)==size(self%reflections),"DIFFRACTION_DATA.INQ:d_I_pred_dX ... wrong size, res")
   ENSURE(size(dIdX,1)==size(self%reflections),"DIFFRACTION_DATA.INQ:d_I_pred_dX ... wrong size, dI")
   ENSURE(size(dIdX,2)==size(res,2),"DIFFRACTION_DATA.INQ:d_I_pred_dX ... incompatible res, dIdX")

      ! Constants
      n_refl = size(self%reflections)
      n_p = size(dIdX,2)

      ! Temporaries
      call create_(It,n_refl)
      call create_(Ix,n_refl)
      call create_(dt,n_refl)
      call create_(db,n_refl)

      ! Extinction factor part
      call create_(ext,n_refl)
      ext = F_scale_and_extn_correction_(self,ONE,self%extinction_factor)

      ! Angle bit, extinction factor folded in. Not evaluated when
      ! extinction is off: the angle part needs a wavelength, which data
      ! refined without extinction need not have.
      call create_(angle_bit,n_refl)
      if (is_zero_(self%extinction_factor,TOL(9))) then
         angle_bit = ZERO
      else
         angle_bit = self%extinction_factor*extinction_angle_part_(self)
      end if

      ! Make temporaries for derivative of scale, extinction, I_abs
      top = ZERO
      bot = ZERO
      do n = 1,n_refl
         extn = ext(n)
         ref = self%reflections(n)

      ! I_exp  = ref.I_corr ! NOTE
         I_exp = ref%I_exp  ! NOTE
         I_sig = ref%I_sigma
         I_abs = abs(ref%I_calc)
         I_til = extn*I_abs
         sig2 = ONE/(I_sig*I_sig)
         I_es2 = I_exp*sig2
         I_ts2 = I_til*sig2
         top = top+I_til*I_es2
         bot = bot+I_til*I_ts2
         ext4 = extn*extn
         ext4 = ext4*ext4
         dX = -HALF*ext4*angle_bit(n)*I_abs
         dX = ONE+I_abs*dX
         dt(n) = I_es2*extn*dX
         db(n) = extn*I_ts2*dX
         It(n) = I_til
         Ix(n) = self%scale_factor*extn*dX

         ! Return these
         sig(n) = sig2
         del(n) = (I_exp-ref%I_pred)*sig2
      end do
      call destroy_(angle_bit)
      call destroy_(ext)

      ! Derivative of the scale factor
      call create_(d_scale,n_p)
      bot = ONE/bot
      fac = -TWO*top*bot*bot
      do p = 1,n_p
         val1 = ZERO
         val2 = ZERO
         do n = 1,n_refl
            dI = dIdX(n,p)
            val1 = val1+dt(n)*dI
            val2 = val2+db(n)*dI
         end do
         d_scale(p) = bot*val1+fac*val2
      end do
      call destroy_(db)
      call destroy_(dt)

      ! Derivative of I_pred
      do p = 1,n_p
         dsp = d_scale(p)
         do n = 1,n_refl
            dI = dIdX(n,p)
            res(n,p) = dsp*It(n)+Ix(n)*dI
         end do
      end do

      ! Clean up
      call destroy_(d_scale)
      call destroy_(Ix)
      call destroy_(It)
   end subroutine

   PURE subroutine d_F_abs_dX(self,res,dFdX)
   ! Evaluate "res", the derivative of the F_abs with respect to some
   ! parameters X (e.g. thermal parameters) given "dFdX", the
   ! derivatives of the *complex* structure factors  w.r.t. the X
   ! parameters.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: res
      MAT(CPX,:,:), IN :: dFdX
      VEC(CPX,:), allocatable :: F_con
      VEC(REAL,:), allocatable :: F_abi
      CPX :: F_cal
      INT :: n_refl,n_p,p,n
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.INQ:d_F_abs_dX ... no reflection data")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.INQ:d_F_abs_dX ... no calculated structure factors")
   ENSURE(size(res,1)==size(self%reflections),"DIFFRACTION_DATA.INQ:d_F_abs_dX ... wrong size, res")
   ENSURE(size(dFdX,1)==size(self%reflections),"DIFFRACTION_DATA.INQ:d_F_abs_dX ... wrong size, dFdX")
   ENSURE(size(dFdX,2)==size(res,2),"DIFFRACTION_DATA.INQ:d_F_abs_dX ... incompatible res, dFdX")
      n_refl = size(self%reflections)
      n_p = size(dFdX,2)
      call create_(F_con,n_refl)
      call create_(F_abi,n_refl)

      ! Setup
      do n = 1,n_refl
         F_cal = self%reflections(n)%F_calc
         F_con(n) = conjg(F_cal)
         F_abi(n) = ONE/abs(F_cal)
      end do

      ! Derivative of (d |F_calc|/d p)
      do p = 1,n_p
         do n = 1,n_refl
            res(n,p) = F_abi(n)*REALIFY(F_con(n)*dFdX(n,p))
         end do
      end do
      call destroy_(F_abi)
      call destroy_(F_con)
   end subroutine

   PURE subroutine d_F_abs_2_dX(self,res,dFdX)
   ! Evaluate "res", the derivative of the F_abs_2 with respect to some
   ! parameters X (e.g. thermal parameters) given "dFdX", the
   ! derivatives of the *complex* structure factors  w.r.t. the X
   ! parameters.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: res
      MAT(CPX,:,:), IN :: dFdX
      VEC(CPX,:), allocatable :: F_con  !conjugated structure factors
      CPX :: F_cal  !calculated structure factors
      INT :: n_refl,n_p,p,n
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.INQ:d_F_abs_2_dX ... no reflection data")
   ENSURE(have_F_calc_(self%reflections),"DIFFRACTION_DATA.INQ:d_F_abs_2_dX ... no calculated structure factors")
   ENSURE(size(res,1)==size(self%reflections),"DIFFRACTION_DATA.INQ:d_F_abs_2_dX ... wrong size, res")
   ENSURE(size(dFdX,1)==size(self%reflections),"DIFFRACTION_DATA.INQ:d_F_abs_2_dX ... wrong size, dFdX")
   ENSURE(size(dFdX,2)==size(res,2),"DIFFRACTION_DATA.INQ:d_F_abs_2_dX ... incompatible res, dFdX")
      n_refl = size(self%reflections)
      n_p = size(dFdX,2)
      call create_(F_con,n_refl)
      do n = 1,n_refl
         F_cal = self%reflections(n)%F_calc
         F_con(n) = conjg(F_cal)
      end do
      do p = 1,n_p
         do n = 1,n_refl
            res(n,p) = TWO*REALIFY(F_con(n)*dFdX(n,p))
         end do
      end do
      call destroy_(F_con)
   end subroutine


! =============================
! Inherited reflection routines
! =============================

   PURE function F_pred_cpx(self) result (res)
   ! The predicted structure factors including the complex phase.
   ! NOTE: this routine does *not* reset .GoF2
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(CPX,size(self%reflections)) :: res
      res = self%reflections%F_calc*F_scale_and_extn_correction_(self)
   end function

   function F_pred_complex(self) result (res)
   ! The predicted structure factors including the complex phase.
   ! NOTE: this routine does *not* reset .GoF2
      type(DIFFRACTION_DATA_TYPE), INOUT :: self
      VEC(CPX,size(self%reflections)) :: res
   ENSURE(allocated(self%reflections),"DIFFRACTION_DATA.INQ:F_pred_complex ... no reflection data")

      ! Multiply scale factor
      res = self%reflections%F_calc*F_scale_and_extn_correction_(self)
   end function

   PURE function F_exp_on_absolute_scale(self) result (res)
   ! The experimental structure factors F_exp corrected by the
   ! scale_factor(s) and extinction, to be on an absolute scale.
   ! NOTE: Dispersion *not* removed, see F_exp_scaled_corrected in CRYSTAL.
      type(DIFFRACTION_DATA_TYPE), IN :: self
      VEC(REAL,size(self%reflections)) :: res
      VEC(REAL,:), allocatable :: fac
   ENSURE(reflection_data_exists_(self),"DIFFRACTION_DATA.INQ:F_exp_on_absolute_scale ... no reflection data")

      ! Correction factors
      if (use_multiple_scale_factors_(self)) then
         fac = F_scale_and_extn_correction_(self,self%scale_factors,self%extinction_factor)
      else
         fac = F_scale_and_extn_correction_(self,self%scale_factor,self%extinction_factor)
      end if

      ! Apply
      res = self%reflections%F_exp/fac
   end function

end module
