!---------------------------------------------------------------------------
!
! MAT5{INT}: 5 dimensional matrices
!
! Copyright (C) dylan jayatilaka, 1998
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
module MAT5_INT_MODULE

#  include "macros"

   include "mat5{int}.use"

   implicit none

   include "mat5{int}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim1,dim2,dim3,dim4,dim5)
   ! Allocate a matrix with the given dimensions
      MAT5(INT,:,:,:,:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2,dim3,dim4,dim5

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT5{INTRINSIC}
      allocate(self(dim1,dim2,dim3,dim4,dim5),stat=status)
      ENSURE(status==0,"MAT5{INT}:create_0 ... error in allocating memory")
#else
      allocate(self(dim1,dim2,dim3,dim4,dim5))
#endif
   end subroutine

   PURE subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3,lb4,ub4,lb5,ub5)
   ! Allocate a matrix with the given bounds
      MAT5(INT,:,:,:,:,:), allocatable, OUT :: self
      INT, IN :: lb1,ub1
      INT, IN :: lb2,ub2
      INT, IN :: lb3,ub3
      INT, IN :: lb4,ub4
      INT, IN :: lb5,ub5

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT5{INTRINSIC}
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4,lb5:ub5),stat=status)
      ENSURE(status==0,"MAT5{INT}:create_1 ... error in allocating memory")
#else
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4,lb5:ub5))
#endif
   end subroutine

   PURE subroutine destroy_0(self)
   ! Deallocate "self"
      MAT5(INT,:,:,:,:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT5{INT}:destroy_0 ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


! Old

   PURE subroutine create_2(self,dim1,dim2,dim3,dim4,dim5)
   ! Create the object with the desired bounds
      MAT5(INT,:,:,:,:,:), PTR :: self
      INT, IN :: dim1,dim2,dim3,dim4,dim5

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT5{INTRINSIC}
      nullify(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim1,dim2,dim3,dim4,dim5),STAT=status)
      ENSURE(status==0,"MAT5{INT}:create_2 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim1,dim2,dim3,dim4,dim5))
#endif
   end subroutine

   PURE subroutine create_3(self,bounds)
   ! Create a 5-d array with all upper "bounds" specified
      MAT5(INT,:,:,:,:,:), PTR :: self
      VEC(INT,5), IN :: bounds

      ! The following code is inherited from MAT5{INTRINSIC}
      call create_(self,bounds(1),bounds(2),bounds(3),bounds(4),bounds(5))
   end subroutine

   PURE subroutine create_4(self,lb1,ub1,lb2,ub2,lb3,ub3,lb4,ub4,lb5,ub5)
   ! Create the object with the desired bounds
      MAT5(INT,:,:,:,:,:), PTR :: self
      INT, IN :: lb1,lb2,lb3,ub1,ub2,ub3,lb4,ub4,lb5,ub5

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT5{INTRINSIC}
      nullify(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4,lb5:ub5),STAT=status)
      ENSURE(status==0,"MAT5{INT}:create_4 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4,lb5:ub5))
#endif
   end subroutine

   PURE subroutine create_5(self,bounds1,bounds2,bounds3,bounds4,bounds5)
   ! Create the object with the desired bounds
      MAT5(INT,:,:,:,:,:), PTR :: self
      VEC(INT,2), IN :: bounds1,bounds2,bounds3,bounds4,bounds5

      ! The following code is inherited from MAT5{INTRINSIC}
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2),bounds3(1),bounds3(2),bounds4(1),bounds4(2),bounds5(1),bounds5(2))
   end subroutine

   PURE subroutine create_6(self,bounds)
   ! Create the object with the desired bounds
      MAT5(INT,:,:,:,:,:), PTR :: self
      MAT(INT,5,2), IN :: bounds

      ! The following code is inherited from MAT5{INTRINSIC}
      call create_(self,bounds(1,1),bounds(1,2),bounds(2,1),bounds(2,2),bounds(3,1),bounds(3,2),bounds(4,1),bounds(4,2),bounds(5,1),bounds(5,2))
   end subroutine

   PURE subroutine destroy_1(self)
   ! Destroy an object. This version does not destroy pointer parts.
      MAT5(INT,:,:,:,:,:), PTR :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT associated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT5{INT}:destroy_1 ... error in allocating memory")
      if (status/=0) stop
#else
      deallocate(self)
#endif
   end subroutine


!  ====
!  Copy
!  ====

   subroutine create_copy(self,c)
   ! Create a copy of matrix "c"
      MAT5(INT,:,:,:,:,:), PTR :: self
      MAT5(INT,:,:,:,:,:), IN :: c

      ! The following code is inherited from MAT5{INTRINSIC}
      call create_(self,lbound(c,1),ubound(c,1),lbound(c,2),ubound(c,2),lbound(c,3),ubound(c,3),lbound(c,4),ubound(c,4),lbound(c,5),ubound(c,5))
      self = c
   end subroutine


!  ==============
!  Shape and size
!  ==============

   function is_same_shape_as(self,b) result (res)
   ! Returns TRUE if the matrix "b" has the same shape as self
      MAT5(INT,:,:,:,:,:), IN :: self
      MAT5(INT,:,:,:,:,:), IN :: b
      BIN :: res

      ! The following code is inherited from MAT5{INTRINSIC}
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2) AND size(self,3)==size(b,3) AND size(self,4)==size(b,4) AND size(self,5)==size(b,5)
   end function


!  ===========
!  Set methods
!  ===========

   subroutine set_to_0(self,b)
   ! Set "self" to "b"
      MAT5(INT,:,:,:,:,:), INOUT :: self
      MAT5(INT,:,:,:,:,:) :: b
   ENSURE(is_same_shape_as_(self,b),"MAT5{INT}:set_to_0 ... different shapes")

      ! The following code is inherited from MAT5{INTRINSIC}
      self = b
   end subroutine

   subroutine set_to_1(self,b)
   ! Set "self" to vector "b" in standard order
      MAT5(INT,:,:,:,:,:), INOUT :: self
      VEC(INT,:) :: b
   ENSURE(size(self)==size(b),"MAT5{INT}:set_to_1 ... different sizes")

      ! The following code is inherited from MAT5{INTRINSIC}
      self = reshape(b,[size(self,1),size(self,2),size(self,3),size(self,4),size(self,5)])
   end subroutine

end module
