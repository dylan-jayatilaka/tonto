!==================================================================
!
! MAT3{REAL}: 3 dimensional matrices
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
module MAT3_REAL_MODULE

#  include "macros"

   include "mat3{real}.use"

   implicit none

   include "mat3{real}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim)
   ! Allocate a matrix with dimensions in "dim"
      MAT3(REAL,:,:,:), allocatable, OUT :: self
      VEC(INT,3), IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT3{INTRINSIC}
      allocate(self(dim(1),dim(2),dim(3)),stat=status)
      ENSURE(status==0,"MAT3{REAL}:create_0 ... error in allocating memory")
#else
      allocate(self(dim(1),dim(2),dim(3)))
#endif
   end subroutine

   PURE subroutine create_1(self,dim1,dim2,dim3)
   ! Allocate a matrix with the given dimensions
      MAT3(REAL,:,:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2,dim3

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT3{INTRINSIC}
      allocate(self(dim1,dim2,dim3),stat=status)
      ENSURE(status==0,"MAT3{REAL}:create_1 ... error in allocating memory")
#else
      allocate(self(dim1,dim2,dim3))
#endif
   end subroutine

   PURE subroutine create_2(self,lb1,ub1,lb2,ub2,lb3,ub3)
   ! Allocate a matrix with the given bounds
      MAT3(REAL,:,:,:), allocatable, OUT :: self
      INT, IN :: lb1,ub1
      INT, IN :: lb2,ub2
      INT, IN :: lb3,ub3

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT3{INTRINSIC}
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3),stat=status)
      ENSURE(status==0,"MAT3{REAL}:create_2 ... error in allocating memory")
#else
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3))
#endif
   end subroutine

   PURE subroutine create_3(self,bounds1,bounds2,bounds3)
   ! Allocate matrix with the given pair-bounds
      MAT3(REAL,:,:,:), allocatable, OUT :: self
      VEC(INT,2), IN :: bounds1,bounds2,bounds3

      ! The following code is inherited from MAT3{INTRINSIC}
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2),bounds3(1),bounds3(2))
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      MAT3(REAL,:,:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT3{REAL}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ============================
!  Size-of and shape operations
!  ============================

   function is_same_shape_as(self,b) result (res)
   ! Returns TRUE if the matrix "b" has the same shape as self
      MAT3(REAL,:,:,:), IN :: self
      MAT3(REAL,:,:,:), IN :: b
      BIN :: res

      ! The following code is inherited from MAT3{INTRINSIC}
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2) AND size(self,3)==size(b,3)
   end function

   function is_cube(self) result (res)
   ! Returns TRUE if self is a cube
      MAT3(REAL,:,:,:), IN :: self
      BIN :: res

      ! The following code is inherited from MAT3{INTRINSIC}
      res = size(self,1)==size(self,2) AND size(self,1)==size(self,3)
   end function

   function is_square_23(self) result (res)
   ! Checks to see that the matrix formed by the 2nd and 3rd columns is square.
      MAT3(REAL,:,:,:), IN :: self
      BIN :: res
      res = size(self,2)==size(self,3)
   end function

   function tri_size_23(self) result (ltr)
   ! Returns the size of the lower triangle needed to store the matrix self.
   ! Assumes the matrix formed by columns 2 and 3 is the symmetric one.
      MAT3(REAL,:,:,:), IN :: self
      INT :: ltr
   ENSURE(is_square_23_(self),"MAT3{REAL}:tri_size_23 ... 2nd and 3rd dimensions not equal")
      ltr = size(self,1)*size(self,2)*(size(self,2)+1)/2
   end function


!  ======================
!  Set and change methods
!  ======================

   subroutine set_to_0(self,b)
   ! Set "self" to "b"
      MAT3(REAL,:,:,:), INOUT :: self
      MAT3(REAL,:,:,:), IN :: b

      ! The following code is inherited from MAT3{INTRINSIC}
      self = b
   end subroutine

   subroutine set_to_1(self,b)
   ! Set "self" to vector "b" in standard order
      MAT3(REAL,:,:,:), INOUT :: self
      VEC(REAL,:), IN :: b

      ! The following code is inherited from MAT3{INTRINSIC}
      self = reshape(b,[size(self,1),size(self,2),size(self,3)])
   end subroutine

   subroutine to_tri_23(self,tr)
   ! Converts the matrix self to the lower triangle tr.
   ! Assumes the matrix formed by columns 2 and 3 is the symmetric one.
      MAT3(REAL,:,:,:), IN :: self
      VEC(REAL,:) :: tr
      INT :: h,i,j,ij
   ENSURE(size(tr)>=tri_size_23_(self),"MAT3{REAL}:to_tri_23 ... tr array too small")
   ENSURE(is_square_23_(self),"MAT3{REAL}:to_tri_23 ... 2nd and 3rd dimensions not equal")
      ij = 0
      do h = 1,size(self,1)
         do i = 1,size(self,2)
            do j = 1,i
               ij = ij+1
               tr(ij) = self(h,j,i)
            end do
         end do
      end do
   end subroutine

   subroutine from_tri_23(self,tr)
   ! Converts the matrix self to the lower triangle tr.
   ! Assumes the matrix formed by columns 2 and 3 is the symmetric one.
      MAT3(REAL,:,:,:), INOUT :: self
      VEC(REAL,:), IN :: tr
      INT :: h,i,j,ij
   ENSURE(size(tr)>=tri_size_23_(self),"MAT3{REAL}:from_tri_23 ... tr array too small")
   ENSURE(is_square_23_(self),"MAT3{REAL}:from_tri_23 ... 2nd and 3rd dimensions not equal")
      ij = 0
      do h = 1,size(self,1)
         do i = 1,size(self,2)
            do j = 1,i
               ij = ij+1
               self(h,j,i) = tr(ij)
               self(h,i,j) = tr(ij)
            end do
         end do
      end do
   end subroutine


!  ==============
!  Symmetrization
!  ==============

   pure subroutine make_symmetric(self)
   ! Make the upper pyramid of "self" the same as the lower pyramid
      MAT3(REAL,:,:,:), INOUT :: self

! ENSURE(.dim1==.dim2,"non-cube tensor")
! ENSURE(.dim1==.dim3,"non-cube tensor")
      INT :: dim,i1,i2,i3
      REAL :: val
      dim = size(self,1)
      do i1 = 1,dim
         do i2 = 1,i1
            do i3 = 1,i2
               val = self(i1,i2,i3)
               self(i1,i3,i2) = val
               self(i2,i1,i3) = val
               self(i2,i3,i1) = val
               self(i3,i1,i2) = val
               self(i3,i2,i1) = val
            end do
         end do
      end do
   end subroutine

   subroutine transpose_12(self)
   ! Makes the matrix formed by the 1st and 2nd columns symmetrical.
      MAT3(REAL,:,:,:), INOUT :: self
      INT :: k
   ENSURE(size(self,1)==size(self,2),"MAT3{REAL}:transpose_12 ... non-square matrix")
      do k = 1,size(self,3)
         self(:,:,k) = transpose(self(:,:,k))
      end do
   end subroutine

   subroutine symmetric_reflect_12(self)
   ! Makes the matrix formed by the 1st and 2nd columns symmetrical.
      MAT3(REAL,:,:,:), INOUT :: self
      INT :: i,j
   ENSURE(size(self,1)==size(self,2),"MAT3{REAL}:symmetric_reflect_12 ... non-square matrix")
      do i = 1,size(self,1)
         do j = 1,i-1
            self(j,i,:) = self(i,j,:)
         end do
      end do
   end subroutine

   PURE subroutine symmetric_reflect_23(self,set_upper,set_lower)
   ! Make the upper triangle of "self" the same as the 
   ! lower triangle, or the reverse if "set_upper" is 
   ! FALSE or "set_lower" is TRUE, in indices 2 and 3.
      MAT3(REAL,:,:,:), INOUT :: self
      BIN, optional, IN :: set_upper
      BIN, optional, IN :: set_lower
      INT :: i,j
      BIN :: upper
   ENSURE(is_square_(self(1,:,:)),"MAT3{REAL}:symmetric_reflect_23 ... self(1,:,:) is not square!")
   ENSURE(NOT (present(set_upper) AND present(set_lower)),"MAT3{REAL}:symmetric_reflect_23 ... both options present!")
      upper = TRUE
      if (present(set_upper)) upper = set_upper
      if (present(set_lower)) upper = NOT set_lower
      if (upper) then

         ! Set the upper triangle
         do i = 1,size(self,2)
            do j = 1,i-1
               self(:,j,i) = self(:,i,j)
            end do
         end do
      else

         ! Set the lower triangle
         do i = 1,size(self,2)
            do j = 1,i-1
               self(:,i,j) = self(:,j,i)
            end do
         end do
      end if
   end subroutine

   PURE subroutine symmetrize_23(self)
   ! Set self to half of itself plus half its transpose, i.e.
   ! self = 1/2 (self + self^T)
      MAT3(REAL,:,:,:), INOUT :: self
      INT :: n,i,j
      REAL :: val
   ENSURE(size(self,2)==size(self,3),"MAT3{REAL}:symmetrize_23 ... dimensions 2 & 3 are not the same")
      do i = 1,size(self,2)
         do j = 1,i-1
            do n = 1,size(self,1)
               val = HALF*(self(n,i,j)+self(n,j,i))
               self(n,i,j) = val
               self(n,j,i) = val
            end do
         end do
      end do
   end subroutine


!  =====================
!  Trace and dot product
!  =====================

   function norm(self,a) result (res)
   ! Rubbish norm for testing.
      MAT3(REAL,:,:,:), IN :: self
      MAT3(REAL,:,:,:), IN :: a
      REAL :: res
   ENSURE(size(self,3)==size(a,3),"MAT3{REAL}:norm ... incompatible 3rd dimension")
      res = sqrt(abs(dot_(self,a)))
   end function

   function dot(self,a) result (res)
   ! Generalised dot product. Assumes a sum of dot products for the
   ! matrices self(:,:,i).
      MAT3(REAL,:,:,:), IN :: self
      MAT3(REAL,:,:,:), IN :: a
      REAL :: res
      INT :: i
   ENSURE(size(self,3)==size(a,3),"MAT3{REAL}:dot ... incompatible 3rd dimension")
      res = ZERO
      do i = 1,size(self,3)
         res = res+trace_product_with_(self(:,:,i),a(:,:,i),transpose_a=TRUE)
      end do
   end function

   subroutine trace_over_indices(self,d1,d2,T)
   ! Contract self by tracing over its dimensions d1 and d2, d1<d2.
   ! return the vector T.
      MAT3(REAL,:,:,:), IN :: self
      VEC(REAL,:), OUT :: T
      INT, IN :: d1,d2
      INT :: n
   ENSURE(1<=d1 AND d2<=3,"MAT3{REAL}:trace_over_indices ... dimensions out of bound")
   ENSURE(d1<d2,"MAT3{REAL}:trace_over_indices ... dimensions not in increasing order")
      if (d1==1 AND d2==2) then
         do n = 1,size(self,3)
            T(n) = trace_(self(:,:,n))
         end do
      else if (d1==1 AND d2==3) then
         do n = 1,size(self,2)
            T(n) = trace_(self(:,n,:))
         end do
      else if (d1==2 AND d2==3) then
         do n = 1,size(self,1)
            T(n) = trace_(self(n,:,:))
         end do
      end if
   end subroutine

   pure function contract_with(self,v) result (res)
   ! Contract with a 1-vector.
      MAT3(REAL,:,:,:), IN :: self
      VEC(REAL,:), IN :: v
      REAL :: res

! ENSURE(.is_cube,"self in not a cube")
! ENSURE(.dim1==v.dim,"wrong size, v")
      INT :: dim,i1,i2,i3
      dim = size(self,1)
      res = ZERO
      do i3 = 1,dim
         do i2 = 1,dim
            do i1 = 1,dim
               res = res+self(i1,i2,i3)*v(i1)*v(i2)*v(i3)
            end do
         end do
      end do
   end function


!  =======================
!  Direct product matrices
!  =======================

!   gaussian_xyz_matrices result (res) :: leaky
!   ! Return representation matrices for the s, p, d, f, and g (l = 0 ... 4)
!   ! xyz cartesian gaussian shell components from a list of the p xyz
!   ! representation matrices.
!   ! NOTE: nothing in self in pointer assigned, so it can be destroyed safely.
!      res :: VEC{MAT3_{REAL}}*
!   ENSURE(.dim1==3,"wrong 1st dimension, self")
!   ENSURE(.dim2==3,"wrong 1st dimension, self")
!   ENSURE(.dim3>0,"no p-type representation matrices")
!      allocate(res(0:4))
!      res(0).element.create(1,1,.dim3)
!      res(1).element.create(3,3,.dim3)
!      ! Now assign the transformation matrices
!      res(0).element = ONE
!      res(1).element = self
!      res(2).element => .gaussian_d_xyz_matrices
!      res(3).element => .gaussian_f_xyz_matrices
!      res(4).element => .gaussian_g_xyz_matrices
!   end

   PURE function gaussian_d_xyz_matrices(self) result (dtr)
   ! Return the representation matrices for d xyz products found in
   ! gaussian shells from the p xyz matrices.
      MAT3(REAL,:,:,:), IN :: self
      MAT3(REAL,:,:,:), allocatable :: dtr
      INT :: n,order
   ENSURE(size(self,1)==3,"MAT3{REAL}:gaussian_d_xyz_matrices ... wrong 1st dimension, self")
   ENSURE(size(self,2)==3,"MAT3{REAL}:gaussian_d_xyz_matrices ... wrong 2nd dimension, self")
   ENSURE(size(self,3)>0,"MAT3{REAL}:gaussian_d_xyz_matrices ... no p type matrices")
      order = size(self,3)
      call create_(dtr,6,6,order)
      do n = 1,order
         dtr(:,:,n) = gaussian_d_xyz_mx_(self(:,:,n))
      end do
   end function

   PURE function gaussian_f_xyz_matrices(self) result (ftr)
   ! Return the representation matrices for f xyz products found in
   ! gaussian shells from the p xyz matrices
      MAT3(REAL,:,:,:), IN :: self
      MAT3(REAL,:,:,:), allocatable :: ftr
      INT :: n,order
   ENSURE(size(self,1)==3,"MAT3{REAL}:gaussian_f_xyz_matrices ... wrong 1st dimension, self")
   ENSURE(size(self,2)==3,"MAT3{REAL}:gaussian_f_xyz_matrices ... wrong 2nd dimension, self")
   ENSURE(size(self,3)>0,"MAT3{REAL}:gaussian_f_xyz_matrices ... no p type matrices")
      order = size(self,3)
      call create_(ftr,10,10,order)
      do n = 1,order
         ftr(:,:,n) = gaussian_f_xyz_mx_(self(:,:,n))
      end do
   end function

   PURE function gaussian_g_xyz_matrices(self) result (gtr)
   ! Return the representation matrices for g xyz products found in
   ! gaussian shells from the p xyz matrices
      MAT3(REAL,:,:,:), IN :: self
      MAT3(REAL,:,:,:), allocatable :: gtr
      INT :: n,order
   ENSURE(size(self,1)==3,"MAT3{REAL}:gaussian_g_xyz_matrices ... wrong 1st dimension, self")
   ENSURE(size(self,2)==3,"MAT3{REAL}:gaussian_g_xyz_matrices ... wrong 2nd dimension, self")
   ENSURE(size(self,3)>0,"MAT3{REAL}:gaussian_g_xyz_matrices ... no p type matrices")
      order = size(self,3)
      call create_(gtr,15,15,order)
      do n = 1,order
         gtr(:,:,n) = gaussian_g_xyz_mx_(self(:,:,n))
      end do
   end function


!  ========================
!  Change of basis routines
!  ========================

   subroutine similarity_transform_12(self,V)
   ! Do a similarity tranform on the first two indices of self, i.e.
   ! self(:,:,i) -> V self(:,:,i) V^-1
      MAT3(REAL,:,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: V
      INT :: n
   ENSURE(size(self,1)==size(self,2),"MAT3{REAL}:similarity_transform_12 ... 1st two dimensions of self unequal")
   ENSURE(size(self,1)==size(V,1),"MAT3{REAL}:similarity_transform_12 ... incompatible transform matrix, V")
   ENSURE(is_square_(V),"MAT3{REAL}:similarity_transform_12 ... transform matrix not square")
   ENSURE(size(self,3)>0,"MAT3{REAL}:similarity_transform_12 ... no p type matrices")
      do n = 1,size(self,3)
         call similarity_transform_(self(:,:,n),V)
      end do
   end subroutine

   PURE subroutine change_basis_using(self,V)
   ! Change the basis of "self" using vectors "V"; self = self ... V
      MAT3(REAL,:,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: V
      MAT3(REAL,:,:,:), allocatable :: new
   ENSURE(is_cube_(self),"MAT3{REAL}:change_basis_using ... Non-cube matrix")
   ENSURE(is_square_(V),"MAT3{REAL}:change_basis_using ... Non-square matrix")
   ENSURE(size(self,1)==size(V,1),"MAT3{REAL}:change_basis_using ... wrong shapes")

      ! The following code is inherited from MAT3{INTRINSIC}
      call create_(new,size(self,1),size(self,1),size(self,1))
      call change_basis_to_(self,new,V)
      self = new
      call destroy_(new)
   end subroutine

   PURE subroutine change_basis_to(self,new,V)
   ! Change the basis of "self" using vectors "V", and place the result in
   ! "new".  new = self ... V
      MAT3(REAL,:,:,:), IN :: self
      MAT3(REAL,:,:,:), OUT :: new
      MAT(REAL,:,:), IN :: V
      INT :: i,j
   ENSURE(is_cube_(self),"MAT3{REAL}:change_basis_to ... Non-cube matrix")
   ENSURE(is_cube_(new),"MAT3{REAL}:change_basis_to ... Non-cube new matrix")
   ENSURE(size(V,1)==size(self,1),"MAT3{REAL}:change_basis_to ... wrong shapes")
   ENSURE(size(V,2)==size(new,1),"MAT3{REAL}:change_basis_to ... wrong shapes")

      ! The following code is inherited from MAT3{INTRINSIC}
      do i = 1,size(new,1)
         do j = 1,size(new,1)
            new(:,i,j) = matmul(self(:,i,j),V)
         end do
      end do
      do i = 1,size(new,1)
         do j = 1,size(new,1)
            new(i,:,j) = matmul(new(i,:,j),V)
         end do
      end do
      do i = 1,size(new,1)
         do j = 1,size(new,1)
            new(i,j,:) = matmul(new(i,j,:),V)
         end do
      end do
   end subroutine

   PURE subroutine back_transform_using(self,V)
   ! Change the basis of "self" using vectors "V"; self = self ... V^T
      MAT3(REAL,:,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: V
      MAT3(REAL,:,:,:), allocatable :: new
   ENSURE(is_cube_(self),"MAT3{REAL}:back_transform_using ... Non-cube matrix")
   ENSURE(is_square_(V),"MAT3{REAL}:back_transform_using ... Non-square matrix")
   ENSURE(size(self,1)==size(V,1),"MAT3{REAL}:back_transform_using ... wrong shapes")

      ! The following code is inherited from MAT3{INTRINSIC}
      call create_(new,size(self,1),size(self,1),size(self,1))
      call back_transform_to_(self,new,V)
      self = new
      call destroy_(new)
   end subroutine

   PURE subroutine back_transform_to(self,new,V)
   ! Change the basis of "self" using vectors "V", and place the result in
   ! "new".  new = self ... V^T
      MAT3(REAL,:,:,:), IN :: self
      MAT3(REAL,:,:,:), OUT :: new
      MAT(REAL,:,:), IN :: V
      INT :: i,j
   ENSURE(is_cube_(self),"MAT3{REAL}:back_transform_to ... Non-cube matrix")
   ENSURE(is_cube_(new),"MAT3{REAL}:back_transform_to ... Non-cube new matrix")
   ENSURE(size(V,1)==size(self,1),"MAT3{REAL}:back_transform_to ... wrong shapes")
   ENSURE(size(V,2)==size(new,1),"MAT3{REAL}:back_transform_to ... wrong shapes")

      ! The following code is inherited from MAT3{INTRINSIC}
      do i = 1,size(new,1)
         do j = 1,size(new,1)
            new(:,i,j) = matmul(V,self(:,i,j))
         end do
      end do
      do i = 1,size(new,1)
         do j = 1,size(new,1)
            new(i,:,j) = matmul(V,new(i,:,j))
         end do
      end do
      do i = 1,size(new,1)
         do j = 1,size(new,1)
            new(i,j,:) = matmul(V,new(i,j,:))
         end do
      end do
   end subroutine

   PURE subroutine set_vector_hyperpolarisability(self,beta)
   ! Set the vector hyprpolarisability
      MAT3(REAL,:,:,:), IN :: self
      VEC(REAL,:), OUT :: beta
      INT :: i,j
      REAL :: val
   ENSURE(is_cube_(self),"MAT3{REAL}:set_vector_hyperpolarisability ... self in not a cube")
   ENSURE(size(self,1)==3,"MAT3{REAL}:set_vector_hyperpolarisability ... self in not 3 dimensional")
      val = ZERO
      do i = 1,size(self,1)
         val = THREE*self(i,i,i)
         do j = 1,3
            if (i==j) cycle
            val = val+self(i,j,j)+self(j,i,j)+self(j,j,i)
         end do
         beta(i) = THIRD*val
      end do
   end subroutine


!  ======================================
!  Compression and uncompression routines
!  ======================================

   PURE subroutine compress_to_pyramid(self,py)
   ! Converts the lower pyramid of self to the pyramid "py".
      MAT3(REAL,:,:,:), IN :: self
      VEC(REAL,:), OUT :: py
      INT :: i,i1,i2,i3
   ENSURE(is_cube_(self),"MAT3{REAL}:compress_to_pyramid ... non-cube matrix")
   ENSURE(size(py)>=pyramid_size_(self),"MAT3{REAL}:compress_to_pyramid ... pyramid array too small")

      ! The following code is inherited from MAT3{INTRINSIC}
      i = 0
      do i1 = 1,size(self,1)
         do i2 = 1,i1
            do i3 = 1,i2
               i = i+1
               py(i) = self(i1,i2,i3)
            end do
         end do
      end do
   end subroutine

   PURE subroutine uncompress_from_pyramid(self,py)
   ! Converts the pyramid "py" into the symmetric cube "self".
      MAT3(REAL,:,:,:), OUT :: self
      VEC(REAL,:), IN :: py
      INT :: i,i1,i2,i3
      REAL :: val
   ENSURE(is_cube_(self),"MAT3{REAL}:uncompress_from_pyramid ... non-cube matrix")
   ENSURE(size(py)>=pyramid_size_(self),"MAT3{REAL}:uncompress_from_pyramid ... pyramid array too small")

      ! The following code is inherited from MAT3{INTRINSIC}
      i = 0
      do i1 = 1,size(self,1)
         do i2 = 1,i1
            do i3 = 1,i2
               i = i+1
               val = py(i)
               self(i1,i2,i3) = val
               self(i1,i3,i2) = val
               self(i2,i1,i3) = val
               self(i2,i3,i1) = val
               self(i3,i1,i2) = val
               self(i3,i2,i1) = val
            end do
         end do
      end do
   end subroutine

   PURE function pyramid_size(self) result (res)
   ! Returns the size of the lower pyramid needed to store a
   ! totally symmetric self.
      MAT3(REAL,:,:,:), IN :: self
      INT :: res
      INT :: dim
   ENSURE(is_cube_(self),"MAT3{REAL}:pyramid_size ... non-cube matrix")

      ! The following code is inherited from MAT3{INTRINSIC}
      dim = size(self,1)
      res = dim*(dim+1)*(dim+2)/6
   end function


!  =============
!  String widths
!  =============

   pure function str_lengths(self,dp,spaces) result (res)
   ! Returns the minimal string lengths, with optional extra "spaces"
      MAT3(REAL,:,:,:), IN :: self
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
      MAT3(REAL,:,:,:), IN :: self
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
      MAT3(REAL,:,:,:), IN :: self
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

   PURE subroutine expand_octupole_from(self,vec)
   ! Expand "self" from a compressed vector "vec" of ocutupole moments
      MAT3(REAL,:,:,:), OUT :: self
      VEC(REAL,10), IN :: vec
   ENSURE(is_cube_(self),"MAT3{REAL}:expand_octupole_from ... self is not a cube")
   ENSURE(size(self,1)==3,"MAT3{REAL}:expand_octupole_from ... self is not a 3-cube")
      self(1,1,1) = vec(1)  ! xxx
      self(2,2,2) = vec(2)  ! yyy
      self(3,3,3) = vec(3)  ! zzz
      self(1,1,2) = vec(4)  ! xxy
      self(2,1,1) = vec(4)
      self(1,2,1) = vec(4)
      self(1,1,3) = vec(5)  ! xxz
      self(3,1,1) = vec(5)
      self(1,3,1) = vec(5)
      self(2,2,1) = vec(6)  ! yyx
      self(2,1,2) = vec(6)
      self(1,2,2) = vec(6)
      self(2,2,3) = vec(7)  ! yyz
      self(2,3,2) = vec(7)
      self(3,2,2) = vec(7)
      self(3,3,1) = vec(8)  ! zzx 
      self(3,1,3) = vec(8)
      self(1,3,3) = vec(8)
      self(3,3,2) = vec(9)  ! zzy
      self(3,2,3) = vec(9)
      self(2,3,3) = vec(9)
      self(1,2,3) = vec(10)  ! xyz
      self(3,2,1) = vec(10)
      self(2,3,1) = vec(10)
      self(2,1,3) = vec(10)
      self(3,1,2) = vec(10)
      self(1,3,2) = vec(10)
   end subroutine

end module
