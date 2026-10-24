!---------------------------------------------------------------------------
!
!  MAT3{INT}: 3 dimensional integer matrices
!
! Copyright (C) Dylan Jayatilaka, 1998
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
module MAT3_INT_MODULE

#  include "macros"

   include "mat3{int}.use"

   implicit none

   include "mat3{int}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim1,dim2,dim3)
   ! Allocate a matrix with the given dimensions
      MAT3(INT,:,:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2,dim3

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT3{INTRINSIC}
      allocate(self(dim1,dim2,dim3),stat=status)
      ENSURE(status==0,"MAT3{INT}:create_0 ... error in allocating memory")
#else
      allocate(self(dim1,dim2,dim3))
#endif
   end subroutine

   PURE subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3)
   ! Allocate a matrix with the given bounds
      MAT3(INT,:,:,:), allocatable, OUT :: self
      INT, IN :: lb1,ub1
      INT, IN :: lb2,ub2
      INT, IN :: lb3,ub3

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT3{INTRINSIC}
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3),stat=status)
      ENSURE(status==0,"MAT3{INT}:create_1 ... error in allocating memory")
#else
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3))
#endif
   end subroutine

   PURE subroutine create_2(self,bounds1,bounds2,bounds3)
   ! Allocate matrix with the given pair-bounds
      MAT3(INT,:,:,:), allocatable, OUT :: self
      VEC(INT,2), IN :: bounds1,bounds2,bounds3

      ! The following code is inherited from MAT3{INTRINSIC}
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2),bounds3(1),bounds3(2))
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      MAT3(INT,:,:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT3{INT}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ============================
!  Size-of and shape operations 
!  ============================

   function is_same_shape_as(self,b) result (res)
   ! Returns TRUE if the matrix "b" has the same shape as self
      MAT3(INT,:,:,:), IN :: self
      MAT3(INT,:,:,:), IN :: b
      BIN :: res

      ! The following code is inherited from MAT3{INTRINSIC}
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2) AND size(self,3)==size(b,3)
   end function

   subroutine set_to_0(self,b)
   ! Set "self" to "b"
      MAT3(INT,:,:,:), INOUT :: self
      MAT3(INT,:,:,:), IN :: b

      ! The following code is inherited from MAT3{INTRINSIC}
      self = b
   end subroutine

   subroutine set_to_1(self,b)
   ! Set "self" to vector "b" in standard order
      MAT3(INT,:,:,:), INOUT :: self
      VEC(INT,:), IN :: b

      ! The following code is inherited from MAT3{INTRINSIC}
      self = reshape(b,[size(self,1),size(self,2),size(self,3)])
   end subroutine


!  =============
!  String widths
!  =============

   pure function str_lengths(self,spaces) result (res)
   ! Returns the minimal string lengths, with optional extra "spaces"
      MAT3(INT,:,:,:), IN :: self
      INT, IN, optional :: spaces
      MAT3(INT,size(self,1),size(self,2),size(self,3)) :: res
      INT :: i1,i2,i3

      ! The following code is inherited from MAT3{INTRINSIC}
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            do i3 = 1,size(self,3)
               res(i1,i2,i3) = str_length_(self(i1,i2,i3))
            end do
         end do
      end do
      if (present(spaces)) res = res+spaces
   end function

   pure subroutine get_max_str_length(self,msl,spaces)
   ! Returns the maximum of the string lengths.
      MAT3(INT,:,:,:), IN :: self
      INT, OUT :: msl
      INT, IN :: spaces
      INT :: i1,i2,i3,sl

      ! The following code is inherited from MAT3{INTRINSIC}
      msl = 0
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            do i3 = 1,size(self,3)
               call get_str_length_(self(i1,i2,i3),sl,spaces)
               msl = max(msl,sl)
            end do
         end do
      end do
   end subroutine

end module
