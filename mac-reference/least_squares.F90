!---------------------------------------------------------------------------
!
! LEAST_SQUARES: the refined parameters of a least-squares fit.
!
! A fit refines parameters p. The model vector X -- for a structure
! refinement, the positions and ADPs of the atoms -- is made from them by
!
!    X = X_offset + J p,
!
! where J = dX/dp is the Jacobian and X_offset the part of X the
! parameters do not move. One parameter per free component is J = 1. An
! isotropic ADP, a site-symmetric atom, or a rigid-body (TLS) model is a
! different J; the solver does not care which. The fit solves the normal
! equations in p, keeps p, its esds and covariance, and gives the shift
! and covariance of X through J.
!
! The normal matrix A and right-hand side b are made by the caller from
! its own derivatives dF/dX as (dF J)^T w (dF J) and (dF J)^T w r. They may
! be restrained, chi^2 + (p - p0)^T W (p - p0), and near-zero directions
! of A are filtered out; see MAT{REAL}:solve_restrained_linear_equations.
!
! Copyright (C) Dylan Jayatilaka, 2026
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
!
!---------------------------------------------------------------------------
module LEAST_SQUARES_MODULE

#  include "macros"

   include "least_squares.use"

   implicit none

   include "least_squares.int"


contains

!  =================
!  Memory allocation
!  =================

   PURE subroutine create(self)
   ! Allocate an object
      type(LEAST_SQUARES_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"LEAST_SQUARES:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(LEAST_SQUARES_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"LEAST_SQUARES:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_param_labels(self,labels)
   ! Set the parameter labels.
      type(LEAST_SQUARES_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: labels
      self%param_label = labels
   end subroutine

   PURE subroutine set_jacobian(self,J,labels)
   ! Set the Jacobian J = dX/dp and the parameter labels. The parameter
   ! values, offset and covariance are cleared: set_parameters_from makes
   ! them for the X in hand.
      type(LEAST_SQUARES_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: J
      VEC(STR(len=*),:), IN :: labels
   ENSURE(size(labels)==size(J,2),"LEAST_SQUARES:set_jacobian ... one label per column of J")
      self%n_X = size(J,1)
      self%n_param = size(J,2)
      self%jacobian = J
      self%param_label = labels
      call destroy_(self%param)
      call destroy_(self%X_offset)
      call destroy_(self%param_esd)
      call destroy_(self%param_covariance)
      call destroy_(self%dp)
   end subroutine

   subroutine set_parameters_from(self,X,X_fixed,fixed)
   ! Make the parameters p and the offset X_offset for the model vector
   ! "X": p is the least-squares fit of X - X_fixed by J p, and then
   !    X_offset = X_fixed     where "fixed" is TRUE,
   !    X_offset = X - J p     elsewhere,
   ! so the rows not "fixed" reproduce X exactly (a free component, or one
   ! the parameters do not move), and the "fixed" rows are put on the
   ! model: X_fixed is their part that no parameter moves, such as the
   ! stiff-mode U of a TLS model. On return "X" is X_offset + J p.
      type(LEAST_SQUARES_TYPE), INOUT :: self
      VEC(REAL,:), INOUT :: X
      VEC(REAL,:), IN :: X_fixed
      VEC(BIN,:), IN :: fixed
      MAT(REAL,:,:), allocatable :: JTJ
      VEC(REAL,:), allocatable :: rhs
      INT :: i
   ENSURE(allocated(self%jacobian),"LEAST_SQUARES:set_parameters_from ... no Jacobian")
   ENSURE(size(X)==self%n_X,"LEAST_SQUARES:set_parameters_from ... wrong size, X")
   ENSURE(size(X_fixed)==self%n_X,"LEAST_SQUARES:set_parameters_from ... wrong size, X_fixed")
   ENSURE(size(fixed)==self%n_X,"LEAST_SQUARES:set_parameters_from ... wrong size, fixed")

      ! Normal equations of the fit, J^T J p = J^T (X - X_fixed)
      call create_(JTJ,self%n_param,self%n_param)
      call to_product_of_(JTJ,self%jacobian,self%jacobian,transpose_a=TRUE)
      call create_(rhs,self%n_param)
      call to_product_of_(rhs,self%jacobian,X-X_fixed,transpose_a=TRUE)
      call destroy_(self%param)
      call create_(self%param,self%n_param)
      call solve_linear_equation_(JTJ,rhs,self%param)

      ! The offset, then X on the model
      call destroy_(self%X_offset)
      call create_(self%X_offset,self%n_X)
      call to_product_of_(self%X_offset,self%jacobian,self%param)
      do i = 1,self%n_X
         if (fixed(i)) then; self%X_offset(i) = X_fixed(i)
         else; self%X_offset(i) = X(i)-self%X_offset(i)
         end if
      end do
      call to_product_of_(X,self%jacobian,self%param)
      X = X+self%X_offset
      call destroy_(rhs)
      call destroy_(JTJ)
   end subroutine

   subroutine make_param_from_X(self,X,p)
   ! Make "p", the parameters for which X_offset + J p is nearest "X" in
   ! the least-squares sense; exact when X is on the model.
      type(LEAST_SQUARES_TYPE), IN :: self
      VEC(REAL,:), IN :: X
      VEC(REAL,:), allocatable, OUT :: p
      MAT(REAL,:,:), allocatable :: JTJ
      VEC(REAL,:), allocatable :: rhs
   ENSURE(allocated(self%jacobian),"LEAST_SQUARES:make_param_from_X ... no Jacobian")
   ENSURE(allocated(self%X_offset),"LEAST_SQUARES:make_param_from_X ... no offset: set_parameters_from first")
   ENSURE(size(X)==self%n_X,"LEAST_SQUARES:make_param_from_X ... wrong size, X")
      call create_(JTJ,self%n_param,self%n_param)
      call to_product_of_(JTJ,self%jacobian,self%jacobian,transpose_a=TRUE)
      call create_(rhs,self%n_param)
      call to_product_of_(rhs,self%jacobian,X-self%X_offset,transpose_a=TRUE)
      call create_(p,self%n_param)
      call solve_linear_equation_(JTJ,rhs,p)
      call destroy_(rhs)
      call destroy_(JTJ)
   end subroutine


!  =======
!  Solving
!  =======

   subroutine solve_normal_equations(self,A,rhs,near_0_tol,dp)
   ! Solve the normal equations "A" dp = "rhs" for the parameter shift
   ! "dp", filtering out eigenvalues of A below "near_0_tol" and applying
   ! the restraints if .restraint_W is set. Keeps the shift in .dp, the
   ! covariance (A+W)^-1 in .param_covariance, the effective parameter
   ! count, and what the filter found.
      type(LEAST_SQUARES_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: A
      VEC(REAL,:), IN :: rhs
      REAL, IN :: near_0_tol
      VEC(REAL,:), OUT :: dp
      MAT(REAL,:,:), allocatable :: C
      INT :: n_p
   ENSURE(is_square_(A),"LEAST_SQUARES:solve_normal_equations ... A must be square")
   ENSURE(size(rhs)==size(A,1),"LEAST_SQUARES:solve_normal_equations ... wrong size, rhs")
   ENSURE(size(dp)==size(A,1),"LEAST_SQUARES:solve_normal_equations ... wrong size, dp")
      n_p = size(A,1)
      if (allocated(self%restraint_W)) then
         ENSURE(size(self%restraint_W,1)==n_p,"LEAST_SQUARES:solve_normal_equations ... wrong size, restraint_W")
         ENSURE(allocated(self%restraint_shift),"LEAST_SQUARES:solve_normal_equations ... no restraint_shift")
         ENSURE(size(self%restraint_shift)==n_p,"LEAST_SQUARES:solve_normal_equations ... wrong size, restraint_shift")
         call solve_restrained_linear_equations_(A,rhs,self%restraint_W,self%restraint_shift,dp,near_0_tol,self%near_0,self%near_0_evals,self%near_0_evecs,self%eigenvalues,C,self%n_param_eff)
      else
         call solve_ill_linear_equations_v1_(A,rhs,dp,near_0_tol,self%near_0,self%near_0_evals,self%near_0_evecs,self%eigenvalues,C)
         self%n_param_eff = n_p-self%n_0-self%near_0
      end if
      self%dp = dp
      call destroy_(self%param_covariance)
      call move_alloc(C,self%param_covariance)
      call update_near_0_IPRs_(self)
   end subroutine

   PURE subroutine update_near_0_IPRs(self)
   ! Make .near_0_IPR, the inverse participation ratio 1/sum_i v_i^4 of
   ! each near-zero eigenvector v: 1 when v lies on one parameter, n when
   ! it is spread evenly over n. It says how many parameters the data fail
   ! to separate in that direction, not whether that is harmless: a site
   ! symmetry mixes parameters as an over-parameterised model does. See
   ! Bell and Dean (1970) Discuss. Faraday Soc. 50 p.55-61.
      type(LEAST_SQUARES_TYPE), INOUT :: self
      INT :: j
      REAL :: v
      call destroy_(self%near_0_IPR)
      if (NOT allocated(self%near_0_evecs)) return
      if (self%near_0<=0) return
      call create_(self%near_0_IPR,self%near_0)
      do j = 1,self%near_0

         ! The eigenvectors are orthonormal, so no normalisation is needed
         v = sum(self%near_0_evecs(:,j)**4)
         if (v>ZERO) then; self%near_0_IPR(j) = ONE/v
         else; self%near_0_IPR(j) = ZERO
         end if
      end do
   end subroutine

   PURE subroutine update_parameters(self,scale)
   ! Add the last shift to the parameters, and make their esds from the
   ! covariance scaled by "scale" (GoF^2 for a weighted fit).
      type(LEAST_SQUARES_TYPE), INOUT :: self
      REAL, IN :: scale
      INT :: i
      if (allocated(self%param) AND allocated(self%dp)) then
         self%param = self%param+self%dp
      end if
      if (allocated(self%param_covariance)) then
         call destroy_(self%param_esd)
         call create_(self%param_esd,size(self%param_covariance,1))
         do i = 1,size(self%param_covariance,1)
            self%param_esd(i) = sqrt(max(ZERO,scale*self%param_covariance(i,i)))
         end do
      end if
   end subroutine


!  ==========================
!  From parameters to X space
!  ==========================

   PURE subroutine make_X_shift(self,dX)
   ! The shift in X from the last shift in p, dX = J dp.
      type(LEAST_SQUARES_TYPE), IN :: self
      VEC(REAL,:), OUT :: dX
   ENSURE(allocated(self%dp),"LEAST_SQUARES:make_X_shift ... no shift")
      if (allocated(self%jacobian)) then
         call to_product_of_(dX,self%jacobian,self%dp)
      else
         dX = self%dp
      end if
   end subroutine

   PURE subroutine make_X_covariance(self,CX)
   ! The covariance of X from that of p, CX = J C J^T.
      type(LEAST_SQUARES_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: CX
      MAT(REAL,:,:), allocatable :: JC
   ENSURE(allocated(self%param_covariance),"LEAST_SQUARES:make_X_covariance ... no covariance")
      if (allocated(self%jacobian)) then
         call create_(JC,self%n_X,self%n_param)
         call to_product_of_(JC,self%jacobian,self%param_covariance)
         call create_(CX,self%n_X,self%n_X)
         call to_product_of_(CX,JC,self%jacobian,transpose_b=TRUE)
         call destroy_(JC)
      else
         CX = self%param_covariance
      end if
   end subroutine

   pure function n_X_constraints(self) result (res)
   ! The number of X components the Jacobian removes.
      type(LEAST_SQUARES_TYPE), IN :: self
      INT :: res
      res = 0
      if (allocated(self%jacobian)) res = size(self%jacobian,1)-size(self%jacobian,2)
   end function


!  ======
!  Output
!  ======

   subroutine put_parameters(self,first,last)
   ! Put a table of the parameters "first" to "last", with their esds.
      type(LEAST_SQUARES_TYPE), IN :: self
      INT, IN :: first,last
      INT :: i
   ENSURE(allocated(self%param),"LEAST_SQUARES:put_parameters ... no parameters")
   ENSURE(first>=1 AND last<=size(self%param),"LEAST_SQUARES:put_parameters ... parameter range out of bounds")
      call dash_(stdout,int_fields=1,real_fields=2,width=20)
      call put_(stdout,"#",int_width=TRUE)
      call put_(stdout,"Parameter",width=20)
      call put_(stdout,"Value")
      call put_(stdout,"esd")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2,width=20)
      do i = first,last
         call put_(stdout,i)
         if (allocated(self%param_label)) then; call put_(stdout,trim(self%param_label(i)),width=20)
         else; call put_(stdout," ",width=20)
         end if
         call put_(stdout,self%param(i))
         if (allocated(self%param_esd)) then; call put_(stdout,self%param_esd(i))
         else; call put_(stdout," ")
         end if
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=2,width=20)
   end subroutine

end module
