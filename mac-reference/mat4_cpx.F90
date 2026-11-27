!==================================================================
!
! MAT4{CPX}: Complex 4 dimensional matrices
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
!==================================================================
module MAT4_CPX_MODULE

#  include "macros"

   include "mat4{cpx}.use"

   implicit none

   include "mat4{cpx}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim1,dim2,dim3,dim4)
   ! Allocate a matrix with the given dimensions
      MAT4(CPX,:,:,:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2,dim3,dim4

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT4{INTRINSIC}
      allocate(self(dim1,dim2,dim3,dim4),stat=status)
      ENSURE(status==0,"MAT4{CPX}:create_0 ... error in allocating memory")
#else
      allocate(self(dim1,dim2,dim3,dim4))
#endif
   end subroutine

   PURE subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3,lb4,ub4)
   ! Allocate a matrix with the given bounds
      MAT4(CPX,:,:,:,:), allocatable, OUT :: self
      INT, IN :: lb1,ub1
      INT, IN :: lb2,ub2
      INT, IN :: lb3,ub3
      INT, IN :: lb4,ub4

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT4{INTRINSIC}
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4),stat=status)
      ENSURE(status==0,"MAT4{CPX}:create_1 ... error in allocating memory")
#else
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4))
#endif
   end subroutine

   PURE subroutine create_2(self,bounds1,bounds2,bounds3,bounds4)
   ! Allocate object with the desired bounds
      MAT4(CPX,:,:,:,:), allocatable, OUT :: self
      VEC(INT,2), IN :: bounds1,bounds2,bounds3,bounds4

      ! The following code is inherited from MAT4{INTRINSIC}
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2),bounds3(1),bounds3(2),bounds4(1),bounds4(2))
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      MAT4(CPX,:,:,:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT4{CPX}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  =================
!  Enquiry functions
!  =================

   function is_same_shape_as(self,b) result (res)
   ! Returns TRUE if the matrix "b" has the same shape as self
      MAT4(CPX,:,:,:,:), IN :: self
      MAT4(CPX,:,:,:,:), IN :: b
      BIN :: res

      ! The following code is inherited from MAT4{INTRINSIC}
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2) AND size(self,3)==size(b,3) AND size(self,4)==size(b,4)
   end function


!  ===========
!  Set methods
!  ===========

   subroutine set_to_0(self,b)
   ! Set "self" to "b"
      MAT4(CPX,:,:,:,:), OUT :: self
      MAT4(CPX,:,:,:,:), IN :: b
   ENSURE(is_same_shape_as_(self,b),"MAT4{CPX}:set_to_0 ... different shapes")

      ! The following code is inherited from MAT4{INTRINSIC}
      self = b
   end subroutine

   subroutine set_to_1(self,b)
   ! Set "self" to vector "b" in standard order
      MAT4(CPX,:,:,:,:), OUT :: self
      VEC(CPX,:), IN :: b
   ENSURE(size(self)==size(b),"MAT4{CPX}:set_to_1 ... different sizes")

      ! The following code is inherited from MAT4{INTRINSIC}
      self = reshape(b,[size(self,1),size(self,2),size(self,3),size(self,4)])
   end subroutine


!  =============
!  String widths
!  =============

   pure function str_lengths(self,dp,spaces) result (res)
   ! Returns the minimal string lengths, with optional extra "spaces"
      MAT4(CPX,:,:,:,:), IN :: self
      INT, IN :: dp
      INT, IN, optional :: spaces
      MAT4(INT,size(self,1),size(self,2),size(self,3),size(self,4)) :: res
      INT :: i1,i2,i3,i4

      ! The following code is inherited from MAT4{INTRINSIC}
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            do i3 = 1,size(self,3)
               do i4 = 1,size(self,4)
                  res(i1,i2,i3,i4) = str_length_(self(i1,i2,i3,i4),dp)
               end do
            end do
         end do
      end do
      if (present(spaces)) res = res+spaces
   end function

   pure subroutine get_max_str_length_0(self,msl,spaces)
   ! Returns the maximum of the string lengths.
      MAT4(CPX,:,:,:,:), IN :: self
      INT, OUT :: msl
      INT, IN :: spaces
      INT :: i1,i2,i3,i4,sl

      ! The following code is inherited from MAT4{INTRINSIC}
      msl = 0
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            do i3 = 1,size(self,3)
               do i4 = 1,size(self,4)
                  call get_str_length_(self(i1,i2,i3,i4),sl,spaces)
                  msl = max(msl,sl)
               end do
            end do
         end do
      end do
   end subroutine

   pure subroutine get_max_str_length_1(self,msl,max_dp,spaces)
   ! Returns the maximum of the string lengths.
      MAT4(CPX,:,:,:,:), IN :: self
      INT, OUT :: msl
      INT, IN :: max_dp
      INT, IN :: spaces
      INT :: i1,i2,i3,i4,sl

      ! The following code is inherited from MAT4{INTRINSIC}
      msl = 0
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            do i3 = 1,size(self,3)
               do i4 = 1,size(self,4)
                  call get_str_length_maxdp_(self(i1,i2,i3,i4),sl,max_dp,spaces)
                  msl = max(msl,sl)
               end do
            end do
         end do
      end do
   end subroutine

end module
