!=================================================================
!
! XC_POTENTIAL:
!
! The outputs of a density functional on one batch of grid points: the
! energy per particle, the derivative "v_rho" of the functional with
! respect to the density of each spin channel, and for a gradient
! functional the vector "v_grad" which is contracted with the gradient of
! a basis function pair to give a matrix element of the potential. See
! DFT_FUNCTIONAL:new_r_potential and new_u_potential for the definitions.
! For a functional of the kinetic energy density, "v_tau" is its derivative
! with respect to that density, for each spin channel.
! The arrays are laid out as those of XC_DENSITY.
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
!==================================================================
module XC_POTENTIAL_MODULE

#  include "macros"

   include "xc_potential.use"

   implicit none

   include "xc_potential.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(XC_POTENTIAL_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"XC_POTENTIAL:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(XC_POTENTIAL_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"XC_POTENTIAL:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine create_grids(self,den)
   ! Allocate the arrays to match the densities "den" and set them to zero.
      type(XC_POTENTIAL_TYPE), INOUT :: self
      type(XC_DENSITY_TYPE), IN :: den
      call destroy_ptr_part_(self)
      call create_(self%energy,den%n_pt)
      self%energy = ZERO
      call create_(self%v_rho,den%n_pt,den%n_spin)
      self%v_rho = ZERO
      if (den%has_gradient) then
         call create_(self%v_grad,den%n_pt,3,den%n_spin)
         self%v_grad = ZERO
      end if
      if (den%has_tau) then
         call create_(self%v_tau,den%n_pt,den%n_spin)
         self%v_tau = ZERO
      end if
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Deallocate the arrays
      type(XC_POTENTIAL_TYPE), INOUT :: self
      if (allocated(self%v_tau)) call destroy_(self%v_tau)
      if (allocated(self%v_grad)) call destroy_(self%v_grad)
      if (allocated(self%v_rho)) call destroy_(self%v_rho)
      if (allocated(self%energy)) call destroy_(self%energy)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self)
   ! Set up defaults: no arrays
      type(XC_POTENTIAL_TYPE), INOUT :: self
      call destroy_ptr_part_(self)
   end subroutine


!  =======
!  Weights
!  =======

   PURE subroutine multiply_by(self,wt)
   ! Multiply the energy per particle and the potentials at each grid point
   ! by the quadrature weight "wt" of that point.
      type(XC_POTENTIAL_TYPE), INOUT :: self
      VEC(REAL,:), IN :: wt
      INT :: s,k
   ENSURE(size(wt)==size(self%energy),"XC_POTENTIAL:multiply_by ... wrong number of weights")
      self%energy = wt*self%energy
      do s = 1,size(self%v_rho,2)
         self%v_rho(:,s) = wt*self%v_rho(:,s)
      end do
      if (allocated(self%v_grad)) then
         do s = 1,size(self%v_grad,3)
            do k = 1,3
               self%v_grad(:,k,s) = wt*self%v_grad(:,k,s)
            end do
         end do
      end if
      if (allocated(self%v_tau)) then
         do s = 1,size(self%v_tau,2)
            self%v_tau(:,s) = wt*self%v_tau(:,s)
         end do
      end if
   end subroutine

end module
