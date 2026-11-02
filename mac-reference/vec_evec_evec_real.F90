!---------------------------------------------------------------------------
!
! VEC{EVEC{EVEC{REAL}}} : 
!
! A list of list of list of rweal numbers.
!
! Copyright (C) Dylan Jayatilaka, 2020.
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
module VEC_EVEC_EVEC_REAL_MODULE

#  include "macros"

   include "vec{evec{evec{real}}}.use"

   implicit none

   include "vec{evec{evec{real}}}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim)
   ! Allocate vector
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{EVEC{EVEC{REAL}}}:create_0 ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{EVEC{EVEC{REAL}}}:create_0 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine create_1(self,bounds)
   ! Allocate the vector with lower and upper bounds in "bounds"
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable, OUT :: self
      VEC(INT,2), IN :: bounds
      INT :: lb,ub
#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)
      lb = bounds(1)
      ub = bounds(2)

#ifdef USE_PRECONDITIONS
      allocate(self(lb:ub),stat=status)
      ENSURE(status==0,"VEC{EVEC{EVEC{REAL}}}:create_1 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine create_2(self,dim,dimv)
   ! Allocated a vector of vectors
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable, OUT :: self
      INT, IN :: dim,dimv

      ! The following code is inherited from VEC{EVEC{EVEC{INTRINSIC}}}
      call create_(self,dim)
      call create_vec_(self,dimv)
   end subroutine

   PURE subroutine create_vec(self,dimv)
   ! Allocate the encapsulated parts
      VEC(type(EVEC_EVEC_REAL_TYPE),:), INOUT :: self
      INT, IN :: dimv
      INT :: i

      ! The following code is inherited from VEC{EVEC{EVEC{INTRINSIC}}}
      do i = 1,size(self)
         call create_(self(i)%element,dimv)
      end do
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{EVEC{EVEC{REAL}}}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

end module
