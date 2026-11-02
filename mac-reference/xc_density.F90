!=================================================================
!
! XC_DENSITY:
!
! The inputs to a density functional on one batch of grid points: the
! density, its gradient if the functional needs one, and the kinetic
! energy density "tau" if it needs that. "tau" is half the sum over the
! occupied orbitals of the square of the orbital gradient. The last
! dimension of each array is the spin channel: one channel holds the
! total quantity, two hold the alpha and beta ones.
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
module XC_DENSITY_MODULE

#  include "macros"

   include "xc_density.use"

   implicit none

   include "xc_density.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(XC_DENSITY_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"XC_DENSITY:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(XC_DENSITY_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"XC_DENSITY:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Deallocate the arrays
      type(XC_DENSITY_TYPE), INOUT :: self
      if (allocated(self%tau)) call destroy_(self%tau)
      if (allocated(self%grad)) call destroy_(self%grad)
      if (allocated(self%rho)) call destroy_(self%rho)
   end subroutine

   PURE subroutine create_grids(self,n_pt,n_spin,has_gradient,has_tau)
   ! Allocate the arrays for "n_pt" grid points and "n_spin" spin channels
   ! and set them to zero. The gradient is allocated only if "has_gradient"
   ! is TRUE, and the kinetic energy density only if "has_tau" is present
   ! and TRUE.
      type(XC_DENSITY_TYPE), INOUT :: self
      INT, IN :: n_pt,n_spin
      BIN, IN :: has_gradient
      BIN, optional, IN :: has_tau
   ENSURE(n_spin==1 OR n_spin==2,"XC_DENSITY:create_grids ... n_spin must be 1 or 2")
      call destroy_ptr_part_(self)
      self%n_pt = n_pt
      self%n_spin = n_spin
      self%has_gradient = has_gradient
      self%has_tau = FALSE
      if (present(has_tau)) self%has_tau = has_tau
      call create_(self%rho,n_pt,n_spin)
      self%rho = ZERO
      if (self%has_gradient) then
         call create_(self%grad,n_pt,3,n_spin)
         self%grad = ZERO
      end if
      if (self%has_tau) then
         call create_(self%tau,n_pt,n_spin)
         self%tau = ZERO
      end if
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self)
   ! Set up defaults: no grid points, one spin channel, no gradient
   ! and no kinetic energy density
      type(XC_DENSITY_TYPE), INOUT :: self
      call destroy_ptr_part_(self)
      self%n_pt = 0
      self%n_spin = 1
      self%has_gradient = FALSE
      self%has_tau = FALSE
   end subroutine

end module
