!==================================================================
!
! VEC{DIIS}: DIID vectors
!
! Copyright (C) Dylan Jayatilaka, 2014
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
module VEC_DIIS_MODULE

#  include "macros"

   include "vec{diis}.use"

   implicit none

   include "vec{diis}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self,dim)
   ! Allocate vector
      VEC(type(DIIS_TYPE),:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{DIIS}:create ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{DIIS}:create ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate the vector
      VEC(type(DIIS_TYPE),:), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{DIIS}:destroy ... error in deallocating memory")
      if (status/=0) stop
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      VEC(type(DIIS_TYPE),:), INOUT :: self
      INT :: a

      ! The following code is inherited from VEC{OBJECT}
      do a = 1,size(self)
         call destroy_ptr_part_(self(a))
      end do
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults_0(self)
   ! Set default values
      VEC(type(DIIS_TYPE),:), OUT :: self
      INT :: n

      ! The following code is inherited from VEC{OBJECT}
      do n = 1,size(self)
         call set_defaults_(self(n))
      end do
   end subroutine

   pure subroutine set_defaults_1(self,diis)
   ! Set default values from a "diis" object
   ! NOTE: pointer parts are assigned, so nullify
   ! pointer parts of "diis" when destroying "self"
      VEC(type(DIIS_TYPE),:), INOUT :: self
      type(DIIS_TYPE), IN :: diis
      self(:) = diis
   end subroutine

   pure subroutine set_use_errors(self,val)
   ! Set whether to use error vectors
      VEC(type(DIIS_TYPE),:), INOUT :: self
      BIN, IN :: val
      INT :: i
      do i = 1,size(self)
         call set_use_errors_(self(i),val)
      end do
   end subroutine

end module
