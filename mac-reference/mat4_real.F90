!==================================================================
!
!  MAT4{REAL}: 4 dimensional matrices
!
! Copyright (C) Dylan Jayatilaka and Daniel Grimwood, 1998
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
module MAT4_REAL_MODULE

#  include "macros"

   include "mat4{real}.use"

   implicit none

   include "mat4{real}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim1,dim2,dim3,dim4)
   ! Allocate a matrix with the given dimensions
      MAT4(REAL,:,:,:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2,dim3,dim4

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT4{INTRINSIC}
      allocate(self(dim1,dim2,dim3,dim4),stat=status)
      ENSURE(status==0,"MAT4{REAL}:create_0 ... error in allocating memory")
#else
      allocate(self(dim1,dim2,dim3,dim4))
#endif
   end subroutine

   PURE subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3,lb4,ub4)
   ! Allocate a matrix with the given bounds
      MAT4(REAL,:,:,:,:), allocatable, OUT :: self
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
      ENSURE(status==0,"MAT4{REAL}:create_1 ... error in allocating memory")
#else
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4))
#endif
   end subroutine

   PURE subroutine create_2(self,bounds1,bounds2,bounds3,bounds4)
   ! Allocate object with the desired bounds
      MAT4(REAL,:,:,:,:), allocatable, OUT :: self
      VEC(INT,2), IN :: bounds1,bounds2,bounds3,bounds4

      ! The following code is inherited from MAT4{INTRINSIC}
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2),bounds3(1),bounds3(2),bounds4(1),bounds4(2))
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      MAT4(REAL,:,:,:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT4{REAL}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  =================
!  Enquiry functions
!  =================

   pure function is_same_shape_as(self,b) result (res)
   ! Returns TRUE if the matrix "b" has the same shape as self
      MAT4(REAL,:,:,:,:), IN :: self
      MAT4(REAL,:,:,:,:), IN :: b
      BIN :: res

      ! The following code is inherited from MAT4{INTRINSIC}
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2) AND size(self,3)==size(b,3) AND size(self,4)==size(b,4)
   end function

   pure function is_hypercube(self) result (res)
   ! Returns TRUE if self is a hypercube
      MAT4(REAL,:,:,:,:), IN :: self
      BIN :: res

      ! The following code is inherited from MAT4{INTRINSIC}
      res = size(self,1)==size(self,2) AND size(self,1)==size(self,3) AND size(self,1)==size(self,4)
   end function


!  ===========
!  Set methods
!  ===========

   pure subroutine set_to_0(self,b)
   ! Set "self" to "b"
      MAT4(REAL,:,:,:,:), OUT :: self
      MAT4(REAL,:,:,:,:), IN :: b
   ! ENSURE(is_same_shape_as_(self,b),"MAT4{REAL}:set_to_0 ... different shapes")

      ! The following code is inherited from MAT4{INTRINSIC}
      self = b
   end subroutine

   pure subroutine set_to_1(self,b)
   ! Set "self" to vector "b" in standard order
      MAT4(REAL,:,:,:,:), OUT :: self
      VEC(REAL,:), IN :: b
   ! ENSURE(size(self)==size(b),"MAT4{REAL}:set_to_1 ... different sizes")

      ! The following code is inherited from MAT4{INTRINSIC}
      self = reshape(b,[size(self,1),size(self,2),size(self,3),size(self,4)])
   end subroutine


!  ===========
!  Contraction
!  ===========

   PURE subroutine partial_trace(self,d1,d2,T)
   ! Contract self by tracing over its dimensions d1 and d2, d1<d2.
   ! return the matrix T.
      MAT4(REAL,:,:,:,:), IN :: self
      MAT(REAL,:,:), OUT :: T
      INT, IN :: d1,d2
      INT :: m,n
   ENSURE(1<=d1 AND d2<=4,"MAT4{REAL}:partial_trace ... dimensions out of bound")
   ENSURE(d1<d2,"MAT4{REAL}:partial_trace ... dimensions not in increasing order")
      if (d1==1 AND d2==2) then
         do m = 1,size(self,3)
            do n = 1,size(self,4)
               T(m,n) = trace_(self(:,:,m,n))
            end do
         end do
      else if (d1==1 AND d2==3) then
         do m = 1,size(self,2)
            do n = 1,size(self,4)
               T(m,n) = trace_(self(:,m,:,n))
            end do
         end do
      else if (d1==1 AND d2==4) then
         do m = 1,size(self,2)
            do n = 1,size(self,3)
               T(m,n) = trace_(self(:,m,n,:))
            end do
         end do
      else if (d1==2 AND d2==3) then
         do m = 1,size(self,1)
            do n = 1,size(self,4)
               T(m,n) = trace_(self(m,:,:,n))
            end do
         end do
      else if (d1==2 AND d2==4) then
         do m = 1,size(self,1)
            do n = 1,size(self,3)
               T(m,n) = trace_(self(m,:,n,:))
            end do
         end do
      else if (d1==3 AND d2==4) then
         do m = 1,size(self,1)
            do n = 1,size(self,2)
               T(m,n) = trace_(self(m,n,:,:))
            end do
         end do
      end if
   end subroutine

   PURE function contract_with(self,v) result (res)
   ! Contract with a 1-vector.
      MAT4(REAL,:,:,:,:), IN :: self
      VEC(REAL,:), IN :: v
      REAL :: res
      INT :: dim,i1,i2,i3,i4
   ENSURE(is_hypercube_(self),"MAT4{REAL}:contract_with ... self in not a hypercube")
   ENSURE(size(self,1)==size(v),"MAT4{REAL}:contract_with ... wrong size, v")
      dim = size(self,1)
      res = ZERO
      do i4 = 1,dim
         do i3 = 1,dim
            do i2 = 1,dim
               do i1 = 1,dim
                  res = res+self(i1,i2,i3,i4)*v(i1)*v(i2)*v(i3)*v(i4)
               end do
            end do
         end do
      end do
   end function

   PURE subroutine change_index_1_to(self,new,V)
   ! Change the 1st index basis of "self" using vectors "V", and place
   ! the result in "new".  new(1,:,:,:) = self(1,:,:,:) V(1,:)
      MAT4(REAL,:,:,:,:), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: new
      MAT(REAL,:,:), IN :: V
      INT :: i,j,k
   ENSURE(size(V,1)==size(self,1),"MAT4{REAL}:change_index_1_to ... wrong shapes")
   ENSURE(size(V,2)==size(new,1),"MAT4{REAL}:change_index_1_to ... wrong shapes")
      do i = 1,size(self,2)
         do j = 1,size(self,3)
            do k = 1,size(self,4)
               new(:,i,j,k) = matmul(self(:,i,j,k),V)
            end do
         end do
      end do
   end subroutine

   PURE subroutine change_index_2_to(self,new,V)
   ! Change the 2nd index basis of "self" using vectors "V", and place
   ! the result in "new".  new(:,2,:,:) = self(:,2,:,:) V(2,:)
      MAT4(REAL,:,:,:,:), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: new
      MAT(REAL,:,:), IN :: V
      INT :: i,j,k
   ENSURE(size(V,1)==size(self,2),"MAT4{REAL}:change_index_2_to ... wrong shapes")
   ENSURE(size(V,2)==size(new,2),"MAT4{REAL}:change_index_2_to ... wrong shapes")
      do i = 1,size(self,1)
         do j = 1,size(self,3)
            do k = 1,size(self,4)
               new(i,:,j,k) = matmul(self(i,:,j,k),V)
            end do
         end do
      end do
   end subroutine

   PURE subroutine change_index_3_to(self,new,V)
   ! Change the 3rd index basis of "self" using vectors "V", and place
   ! the result in "new".  new(:,:,3,:) = self(:,:,3,:) V(3,:)
      MAT4(REAL,:,:,:,:), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: new
      MAT(REAL,:,:), IN :: V
      INT :: i,j,k
   ENSURE(size(V,1)==size(self,3),"MAT4{REAL}:change_index_3_to ... wrong shapes")
   ENSURE(size(V,2)==size(new,3),"MAT4{REAL}:change_index_3_to ... wrong shapes")
      do i = 1,size(self,1)
         do j = 1,size(self,2)
            do k = 1,size(self,4)
               new(i,j,:,k) = matmul(self(i,j,:,k),V)
            end do
         end do
      end do
   end subroutine

   PURE subroutine change_index_4_to(self,new,V)
   ! Change the 3rd index basis of "self" using vectors "V", and place
   ! the result in "new".  new(:,:,:,4) = self(:,:,:,4) V(4,:)
      MAT4(REAL,:,:,:,:), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: new
      MAT(REAL,:,:), IN :: V
      INT :: i,j,k
   ENSURE(size(V,1)==size(self,4),"MAT4{REAL}:change_index_4_to ... wrong shapes")
   ENSURE(size(V,2)==size(new,4),"MAT4{REAL}:change_index_4_to ... wrong shapes")
      do i = 1,size(self,1)
         do j = 1,size(self,2)
            do k = 1,size(self,3)
               new(i,j,k,:) = matmul(self(i,j,k,:),V)
            end do
         end do
      end do
   end subroutine


!  ========================
!  Change of basis routines
!  ========================

   PURE subroutine change_basis_using(self,V)
   ! Change the basis of "self" using vectors "V"; self = self ... V
      MAT4(REAL,:,:,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: V
      MAT4(REAL,:,:,:,:), allocatable :: new
   ENSURE(is_hypercube_(self),"MAT4{REAL}:change_basis_using ... Non-hypercube matrix")
   ENSURE(is_square_(V),"MAT4{REAL}:change_basis_using ... Non-square matrix")
   ENSURE(size(self,1)==size(V,1),"MAT4{REAL}:change_basis_using ... wrong shapes")

      ! The following code is inherited from MAT4{INTRINSIC}
      call create_(new,size(self,1),size(self,1),size(self,1),size(self,1))
      call change_basis_to_(self,new,V)
      self = new
      call destroy_(new)
   end subroutine

   PURE subroutine change_basis_to(self,new,V)
   ! Change the basis of "self" using vectors "V", and place the result in
   ! "new".  new = self ... V
      MAT4(REAL,:,:,:,:), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: new
      MAT(REAL,:,:), IN :: V
      INT :: i,j,k
   ENSURE(is_hypercube_(self),"MAT4{REAL}:change_basis_to ... Non-hypercube matrix")
   ENSURE(is_hypercube_(new),"MAT4{REAL}:change_basis_to ... Non-hypercube new matrix")
   ENSURE(size(V,1)==size(self,1),"MAT4{REAL}:change_basis_to ... wrong shapes")
   ENSURE(size(V,2)==size(new,1),"MAT4{REAL}:change_basis_to ... wrong shapes")

      ! The following code is inherited from MAT4{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,size(self,1)
            do k = 1,size(self,1)
               new(:,i,j,k) = matmul(self(:,i,j,k),V)
            end do
         end do
      end do
      do i = 1,size(self,1)
         do j = 1,size(self,1)
            do k = 1,size(self,1)
               new(i,:,j,k) = matmul(new(i,:,j,k),V)
            end do
         end do
      end do
      do i = 1,size(self,1)
         do j = 1,size(self,1)
            do k = 1,size(self,1)
               new(i,j,:,k) = matmul(new(i,j,:,k),V)
            end do
         end do
      end do
      do i = 1,size(self,1)
         do j = 1,size(self,1)
            do k = 1,size(self,1)
               new(i,j,k,:) = matmul(new(i,j,k,:),V)
            end do
         end do
      end do
   end subroutine

   PURE subroutine back_transform_using(self,V)
   ! Change the basis of "self" using vectors "V"; self = self ... V^T
      MAT4(REAL,:,:,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: V
      MAT4(REAL,:,:,:,:), allocatable :: new
   ENSURE(is_hypercube_(self),"MAT4{REAL}:back_transform_using ... Non-hypercube matrix")
   ENSURE(is_square_(V),"MAT4{REAL}:back_transform_using ... Non-square matrix")
   ENSURE(size(self,1)==size(V,1),"MAT4{REAL}:back_transform_using ... wrong shapes")

      ! The following code is inherited from MAT4{INTRINSIC}
      call create_(new,size(self,1),size(self,1),size(self,1),size(self,1))
      call back_transform_to_(self,new,V)
      self = new
      call destroy_(new)
   end subroutine

   PURE subroutine back_transform_to(self,new,V)
   ! Change the basis of "self" using vectors "V", and place the result in
   ! "new".  new = self ... V^T
      MAT4(REAL,:,:,:,:), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: new
      MAT(REAL,:,:), IN :: V
      INT :: i,j,k
   ENSURE(is_hypercube_(self),"MAT4{REAL}:back_transform_to ... Non-hypercube matrix")
   ENSURE(is_hypercube_(new),"MAT4{REAL}:back_transform_to ... Non-hypercube new matrix")
   ENSURE(size(V,1)==size(self,1),"MAT4{REAL}:back_transform_to ... wrong shapes")
   ENSURE(size(V,2)==size(new,1),"MAT4{REAL}:back_transform_to ... wrong shapes")

      ! The following code is inherited from MAT4{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,size(self,1)
            do k = 1,size(self,1)
               new(:,i,j,k) = matmul(V,self(:,i,j,k))
            end do
         end do
      end do
      do i = 1,size(self,1)
         do j = 1,size(self,1)
            do k = 1,size(self,1)
               new(i,:,j,k) = matmul(V,new(i,:,j,k))
            end do
         end do
      end do
      do i = 1,size(self,1)
         do j = 1,size(self,1)
            do k = 1,size(self,1)
               new(i,j,:,k) = matmul(V,new(i,j,:,k))
            end do
         end do
      end do
      do i = 1,size(self,1)
         do j = 1,size(self,1)
            do k = 1,size(self,1)
               new(i,j,k,:) = matmul(V,new(i,j,k,:))
            end do
         end do
      end do
   end subroutine


!  ==============
!  Symmetrization
!  ==============

   pure subroutine make_symmetric(self)
   ! Make the upper pyramid of "self" the same as the lower pyramid
      MAT4(REAL,:,:,:,:), INOUT :: self

! ENSURE(.dim1==.dim2,"non-cube tensor")
! ENSURE(.dim1==.dim3,"non-cube tensor")
! ENSURE(.dim1==.dim4,"non-cube tensor")
      INT :: dim,i1,i2,i3,i4
      REAL :: val

      ! The following code is inherited from MAT4{INTRINSIC}
      dim = size(self,1)
      do i1 = 1,dim
         do i2 = 1,i1
            do i3 = 1,i2
               do i4 = 1,i3
                  val = self(i1,i2,i3,i4)
                  self(i1,i2,i4,i3) = val
                  self(i1,i3,i2,i4) = val
                  self(i1,i3,i4,i2) = val
                  self(i1,i4,i2,i3) = val
                  self(i1,i4,i3,i2) = val
                  self(i2,i1,i3,i4) = val
                  self(i2,i1,i4,i3) = val
                  self(i2,i3,i1,i4) = val
                  self(i2,i3,i4,i1) = val
                  self(i2,i4,i1,i3) = val
                  self(i2,i4,i3,i1) = val
                  self(i3,i1,i2,i4) = val
                  self(i3,i1,i4,i2) = val
                  self(i3,i2,i1,i4) = val
                  self(i3,i2,i4,i1) = val
                  self(i3,i4,i1,i2) = val
                  self(i3,i4,i2,i1) = val
                  self(i4,i1,i2,i3) = val
                  self(i4,i1,i3,i2) = val
                  self(i4,i2,i1,i3) = val
                  self(i4,i2,i3,i1) = val
                  self(i4,i3,i1,i2) = val
                  self(i4,i3,i2,i1) = val
               end do
            end do
         end do
      end do
   end subroutine


!  ======================================
!  Compression and uncompression routines
!  ======================================

   PURE subroutine compress_to_pyramid(self,py)
   ! Converts the lower pyramid of self to the pyramid "py".
      MAT4(REAL,:,:,:,:), IN :: self
      VEC(REAL,:), OUT :: py
      INT :: i,i1,i2,i3,i4
   ENSURE(is_hypercube_(self),"MAT4{REAL}:compress_to_pyramid ... matrix is not hypercube")
   ENSURE(size(py)>=pyramid_size_(self),"MAT4{REAL}:compress_to_pyramid ... pyramid array too small")

      ! The following code is inherited from MAT4{INTRINSIC}
      i = 0
      do i1 = 1,size(self,1)
         do i2 = 1,i1
            do i3 = 1,i2
               do i4 = 1,i3
                  i = i+1
                  py(i) = self(i1,i2,i3,i4)
               end do
            end do
         end do
      end do
   end subroutine

   PURE subroutine uncompress_from_pyramid(self,py)
   ! Converts the pyramid "py" into the symmetric cube "self".
      MAT4(REAL,:,:,:,:), OUT :: self
      VEC(REAL,:), IN :: py
      INT :: i,i1,i2,i3,i4
      REAL :: val
   ENSURE(is_hypercube_(self),"MAT4{REAL}:uncompress_from_pyramid ... matrix is not hypercube")
   ENSURE(size(py)>=pyramid_size_(self),"MAT4{REAL}:uncompress_from_pyramid ... pyramid array too small")

      ! The following code is inherited from MAT4{INTRINSIC}
      i = 0
      do i1 = 1,size(self,1)
         do i2 = 1,i1
            do i3 = 1,i2
               do i4 = 1,i3
                  i = i+1
                  val = py(i)
                  self(i1,i2,i3,i4) = val
                  self(i1,i2,i4,i3) = val
                  self(i1,i3,i2,i4) = val
                  self(i1,i3,i4,i2) = val
                  self(i1,i4,i2,i3) = val
                  self(i1,i4,i3,i2) = val
                  self(i2,i1,i3,i4) = val
                  self(i2,i1,i4,i3) = val
                  self(i2,i3,i1,i4) = val
                  self(i2,i3,i4,i1) = val
                  self(i2,i4,i1,i3) = val
                  self(i2,i4,i3,i1) = val
                  self(i3,i1,i2,i4) = val
                  self(i3,i1,i4,i2) = val
                  self(i3,i2,i1,i4) = val
                  self(i3,i2,i4,i1) = val
                  self(i3,i4,i1,i2) = val
                  self(i3,i4,i2,i1) = val
                  self(i4,i1,i2,i3) = val
                  self(i4,i1,i3,i2) = val
                  self(i4,i2,i1,i3) = val
                  self(i4,i2,i3,i1) = val
                  self(i4,i3,i1,i2) = val
                  self(i4,i3,i2,i1) = val
               end do
            end do
         end do
      end do
   end subroutine

   PURE function pyramid_size(self) result (res)
   ! Returns the size of the lower pyramid needed to store a
   ! totally symmetric self.
      MAT4(REAL,:,:,:,:), IN :: self
      INT :: res
      INT :: dim
   ENSURE(is_hypercube_(self),"MAT4{REAL}:pyramid_size ... matrix is not hypercube")

      ! The following code is inherited from MAT4{INTRINSIC}
      dim = size(self,1)
      res = dim*(dim+1)*(dim+3)/6
   end function


!  =============
!  String widths
!  =============

   pure function str_lengths(self,dp,spaces) result (res)
   ! Returns the minimal string lengths, with optional extra "spaces"
      MAT4(REAL,:,:,:,:), IN :: self
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
      MAT4(REAL,:,:,:,:), IN :: self
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
      MAT4(REAL,:,:,:,:), IN :: self
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
