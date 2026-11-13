!==================================================================
!
!  MAT3{CPX}: Complex 3 dimensional matrices
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
!==================================================================
module MAT3_CPX_MODULE

#  include "macros"

   include "mat3{cpx}.use"

   implicit none

   include "mat3{cpx}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim1,dim2,dim3)
   ! Allocate a matrix with the given dimensions
      MAT3(CPX,:,:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2,dim3

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT3{INTRINSIC}
      allocate(self(dim1,dim2,dim3),stat=status)
      ENSURE(status==0,"MAT3{CPX}:create_0 ... error in allocating memory")
#else
      allocate(self(dim1,dim2,dim3))
#endif
   end subroutine

   PURE subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3)
   ! Allocate a matrix with the given bounds
      MAT3(CPX,:,:,:), allocatable, OUT :: self
      INT, IN :: lb1,ub1
      INT, IN :: lb2,ub2
      INT, IN :: lb3,ub3

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT3{INTRINSIC}
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3),stat=status)
      ENSURE(status==0,"MAT3{CPX}:create_1 ... error in allocating memory")
#else
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3))
#endif
   end subroutine

   PURE subroutine create_2(self,bounds1,bounds2,bounds3)
   ! Allocate matrix with the given pair-bounds
      MAT3(CPX,:,:,:), allocatable, OUT :: self
      VEC(INT,2), IN :: bounds1,bounds2,bounds3

      ! The following code is inherited from MAT3{INTRINSIC}
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2),bounds3(1),bounds3(2))
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      MAT3(CPX,:,:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT3{CPX}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ============================
!  Size-of and shape operations 
!  ============================

   function is_same_shape_as(self,b) result (res)
   ! Returns TRUE if the matrix "b" has the same shape as self
      MAT3(CPX,:,:,:), IN :: self
      MAT3(CPX,:,:,:), IN :: b
      BIN :: res

      ! The following code is inherited from MAT3{INTRINSIC}
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2) AND size(self,3)==size(b,3)
   end function

   subroutine set_to_0(self,b)
   ! Set "self" to "b"
      MAT3(CPX,:,:,:), INOUT :: self
      MAT3(CPX,:,:,:), IN :: b

      ! The following code is inherited from MAT3{INTRINSIC}
      self = b
   end subroutine

   subroutine set_to_1(self,b)
   ! Set "self" to vector "b" in standard order
      MAT3(CPX,:,:,:), INOUT :: self
      VEC(CPX,:), IN :: b

      ! The following code is inherited from MAT3{INTRINSIC}
      self = reshape(b,[size(self,1),size(self,2),size(self,3)])
   end subroutine


!  ========================
!  Change of basis routines
!  ========================

   PURE subroutine change_basis_to(self,new,V1,V2,V3)
   ! Change the basis of "self" in each dimension respectively using
   ! matrices "V1", "V2", and "V3" and place the result in "new".  
   !   new = self : V1 V2 V3
      MAT3(CPX,:,:,:), IN :: self
      MAT3(REAL,:,:,:), OUT :: new
      MAT(CPX,:,:), IN :: V1,V2,V3
      INT :: i,j
      MAT3(CPX,:,:,:), allocatable :: new1,new2
   ENSURE(size(self,1)==size(V1,1),"MAT3{CPX}:change_basis_to ... dim1 mismatch")
   ENSURE(size(self,2)==size(V2,1),"MAT3{CPX}:change_basis_to ... dim2 mismatch")
   ENSURE(size(self,3)==size(V3,1),"MAT3{CPX}:change_basis_to ... dim3 mismatch")
   ENSURE(size(new,1)==size(V1,2),"MAT3{CPX}:change_basis_to ... new dim1 mismatch")
   ENSURE(size(new,2)==size(V2,2),"MAT3{CPX}:change_basis_to ... new dim2 mismatch")
   ENSURE(size(new,3)==size(V3,2),"MAT3{CPX}:change_basis_to ... new dim3 mismatch")

      ! The following code is inherited from MAT3{INTRINSIC}
      call create_(new2,size(V1,2),size(V2,2),size(self,3))
      call create_(new1,size(V1,2),size(self,2),size(self,3))
      do i = 1,size(self,2)
         do j = 1,size(self,3)
            new1(:,i,j) = matmul(self(:,i,j),V1)
         end do
      end do
      do i = 1,size(new2,1)
         do j = 1,size(new2,3)
            new2(i,:,j) = matmul(new1(i,:,j),V2)
         end do
      end do
      do i = 1,size(new,1)
         do j = 1,size(new,2)
            new(i,j,:) = REALIFY(matmul(new2(i,j,:),V3))
         end do
      end do
      call destroy_(new1)
      call destroy_(new2)
   end subroutine


!  =============
!  String widths
!  =============

   pure function str_lengths(self,dp,spaces) result (res)
   ! Returns the minimal string lengths, with optional extra "spaces"
      MAT3(CPX,:,:,:), IN :: self
      INT, IN :: dp
      INT, IN, optional :: spaces
      MAT3(INT,size(self,1),size(self,2),size(self,3)) :: res
      INT :: i1,i2,i3

      ! The following code is inherited from MAT3{INTRINSIC}
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            do i3 = 1,size(self,3)
               res(i1,i2,i3) = str_length_(self(i1,i2,i3),dp)
            end do
         end do
      end do
      if (present(spaces)) res = res+spaces
   end function

   pure subroutine get_max_str_length_0(self,msl,spaces)
   ! Returns the maximum of the string lengths.
      MAT3(CPX,:,:,:), IN :: self
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

   pure subroutine get_max_str_length_1(self,msl,max_dp,spaces)
   ! Returns the maximum of the string lengths.
      MAT3(CPX,:,:,:), IN :: self
      INT, OUT :: msl
      INT, IN :: max_dp
      INT, IN :: spaces
      INT :: i1,i2,i3,sl

      ! The following code is inherited from MAT3{INTRINSIC}
      msl = 0
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            do i3 = 1,size(self,3)
               call get_str_length_maxdp_(self(i1,i2,i3),sl,max_dp,spaces)
               msl = max(msl,sl)
            end do
         end do
      end do
   end subroutine

end module
