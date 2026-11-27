!=================================================================
!
! IRREP: 
! 
! For representing an irreducible representation of a pointgroup
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
!==================================================================
module IRREP_MODULE

#  include "macros"

   include "irrep.use"

   implicit none

   include "irrep.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(IRREP_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"IRREP:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(IRREP_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"IRREP:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of an irrep
      type(IRREP_TYPE), INOUT :: self
      call destroy_(self%chi)
      call destroy_(self%mx)
   end subroutine


!  ===========
!  Set methods
!  ===========

   pure subroutine set_dimension(self,val)
   ! Set the dimension of the irrep
      type(IRREP_TYPE), INOUT :: self
      INT, IN :: val
      self%dim = val
   end subroutine

   pure subroutine set_label(self,val)
   ! Set the label of the irrep
      type(IRREP_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%label = val
   end subroutine

end module
