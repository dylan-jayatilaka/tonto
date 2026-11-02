!===================================================================
!
! VEC{IRREP}: For representing an array of irreps
!
! Copyright (C) Daniel Grimwood, 1999
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
!===================================================================
module VEC_IRREP_MODULE

#  include "macros"

   include "vec{irrep}.use"

   implicit none

   include "vec{irrep}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self,dim)
   ! Allocate vector
      VEC(type(IRREP_TYPE),:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{IRREP}:create ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{IRREP}:create ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate the vector
      VEC(type(IRREP_TYPE),:), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{IRREP}:destroy ... error in deallocating memory")
      if (status/=0) stop
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of an irrepvec
      VEC(type(IRREP_TYPE),:), INOUT :: self
      INT :: i
      do i = 1,size(self)
         call destroy_ptr_part_(self(i))
      end do
   end subroutine


!  ===========
!  Set methods
!  ===========

   pure subroutine set_dimensions_0(self,val)
   ! Set the dimensions of the irreps
      VEC(type(IRREP_TYPE),:), INOUT :: self
      INT, IN :: val
      self(:)%dim = val
   end subroutine

   pure subroutine set_dimensions_1(self,values)
   ! Set the dimensions of the irreps
      VEC(type(IRREP_TYPE),:), INOUT :: self
      VEC(INT,:), IN :: values
      self(:)%dim = values
   end subroutine

   pure subroutine set_labels(self,values)
   ! Set the labels for the irreps
      VEC(type(IRREP_TYPE),:), INOUT :: self
      VEC(STR(len=*),:), IN :: values
      self(:)%label = values
   end subroutine

end module
