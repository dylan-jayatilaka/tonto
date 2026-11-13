!==================================================================
!
! MAT3{EVEC{INT}}
!
! A 3D matrix where each element is a vector of (possibly) a 
! different length
!
! Copyright (C) Dylan Jayatilaka, 2002
! Copyright (C) Dylan Jayatilaka, 2021
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
module MAT3_EVEC_INT_MODULE

#  include "macros"

   include "mat3{evec{int}}.use"

   implicit none

   include "mat3{evec{int}}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim1,dim2,dim3)
   ! Allocate a matrix with the given dimensions
      MAT3(type(EVEC_INT_TYPE),:,:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2,dim3

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT3{INTRINSIC}
      allocate(self(dim1,dim2,dim3),stat=status)
      ENSURE(status==0,"MAT3{EVEC{INT}}:create_0 ... error in allocating memory")
#else
      allocate(self(dim1,dim2,dim3))
#endif
   end subroutine

   PURE subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3)
   ! Allocate a matrix with the given bounds
      MAT3(type(EVEC_INT_TYPE),:,:,:), allocatable, OUT :: self
      INT, IN :: lb1,ub1
      INT, IN :: lb2,ub2
      INT, IN :: lb3,ub3

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT3{INTRINSIC}
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3),stat=status)
      ENSURE(status==0,"MAT3{EVEC{INT}}:create_1 ... error in allocating memory")
#else
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3))
#endif
   end subroutine

   PURE subroutine create_2(self,dim1,dim2,dim3,dimv)
   ! Allocate object with desired dimensions, ".element" part with bounds "dimv".
      MAT3(type(EVEC_INT_TYPE),:,:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2,dim3
      VEC(INT,2), IN :: dimv

      ! The following code is inherited from MAT3{EVEC{INTRINSIC}}
      call create_(self,dim1,dim2,dim3)
      call create_vec_(self,dimv)
   end subroutine

   PURE subroutine create_3(self,dim,dimv)
   ! Allocate object with desired dimensions, ".element" part size "dimv".
      MAT3(type(EVEC_INT_TYPE),:,:,:), allocatable, OUT :: self
      VEC(INT,3), IN :: dim
      INT, IN :: dimv

      ! The following code is inherited from MAT3{EVEC{INTRINSIC}}
      call create_(self,dim(1),dim(2),dim(3))
      call create_vec_(self,dimv)
   end subroutine

   PURE subroutine create_4(self,dim,dimv)
   ! Allocate object with desired dimensions, ".element" part with bounds "dimv".
      MAT3(type(EVEC_INT_TYPE),:,:,:), allocatable, OUT :: self
      VEC(INT,3), IN :: dim
      VEC(INT,2), IN :: dimv

      ! The following code is inherited from MAT3{EVEC{INTRINSIC}}
      call create_(self,dim(1),dim(2),dim(3))
      call create_vec_(self,dimv)
   end subroutine

   PURE subroutine create_vec_0(self,dimv)
   ! Allocate only the the ".element" pointer parts with size "dimv".
      MAT3(type(EVEC_INT_TYPE),:,:,:), INOUT :: self
      INT, IN :: dimv
      INT :: i1,i2,i3

      ! The following code is inherited from MAT3{EVEC{INTRINSIC}}
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            do i3 = 1,size(self,3)
               call create_(self(i1,i2,i3)%element,dimv)
            end do
         end do
      end do
   end subroutine

   PURE subroutine create_vec_1(self,dimv)
   ! Allocate only the the ".element" pointer parts with bounds "dimv".
      MAT3(type(EVEC_INT_TYPE),:,:,:), INOUT :: self
      VEC(INT,2), IN :: dimv
      INT :: i1,i2,i3

      ! The following code is inherited from MAT3{EVEC{INTRINSIC}}
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            do i3 = 1,size(self,3)
               call create_(self(i1,i2,i3)%element,dimv)
            end do
         end do
      end do
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      MAT3(type(EVEC_INT_TYPE),:,:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT3{EVEC{INT}}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Deallocate the ".element" allocatable parts
      MAT3(type(EVEC_INT_TYPE),:,:,:), INOUT :: self
      INT :: i1,i2,i3

      ! The following code is inherited from MAT3{EVEC{INTRINSIC}}
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            do i3 = 1,size(self,3)
               call destroy_(self(i1,i2,i3)%element)
            end do
         end do
      end do
   end subroutine


!  =======
!  Methods
!  =======

   pure function is_same_shape_as(self,b) result (res)
   ! Returns TRUE if the matrix "b" has the same shape as self
      MAT3(type(EVEC_INT_TYPE),:,:,:), IN :: self
      MAT3(type(EVEC_INT_TYPE),:,:,:), IN :: b
      BIN :: res

      ! The following code is inherited from MAT3{INTRINSIC}
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2) AND size(self,3)==size(b,3)
   end function

   pure subroutine zero(self)
   ! Zero all elements
      MAT3(type(EVEC_INT_TYPE),:,:,:), INOUT :: self
      INT :: i1,i2,i3

      ! The following code is inherited from MAT3{EVEC{INTRINSIC}}
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            do i3 = 1,size(self,3)
               self(i1,i2,i3)%element = 0
            end do
         end do
      end do
   end subroutine

   PURE subroutine set_to(self,v)
   ! Set self to "v". Up to you to make sure they are compatible!
      MAT3(type(EVEC_INT_TYPE),:,:,:), INOUT :: self
      MAT3(type(EVEC_INT_TYPE),:,:,:), IN :: v
      INT :: i1,i2,i3

      ! The following code is inherited from MAT3{EVEC{INTRINSIC}}
      do i1 = 1,size(v,1)
         do i2 = 1,size(v,2)
            do i3 = 1,size(v,3)
               self(i1,i2,i3)%element = v(i1,i2,i3)%element
            end do
         end do
      end do
   end subroutine

end module
