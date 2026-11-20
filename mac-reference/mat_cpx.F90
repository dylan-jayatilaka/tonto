!===================================================================
! complex matrix operations :: MAT{CPX} ...
!
! Copyright (C) Dylan Jayatilaka, 1996
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
!===================================================================
module MAT_CPX_MODULE

#  include "macros"

   include "mat{cpx}.use"

   implicit none

   include "mat{cpx}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim1,dim2)
   ! Allocate a matrix with the given dimensions
      MAT(CPX,:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{INTRINSIC}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim1,dim2),stat=status)
      ENSURE(status==0,"MAT{CPX}:create_0 ... error in allocating memory")
#else
      allocate(self(dim1,dim2))
#endif
   end subroutine

   PURE subroutine create_1(self,lb1,ub1,lb2,ub2)
   ! Allocate a matrix with the given bounds
      MAT(CPX,:,:), allocatable, OUT :: self
      INT, IN :: lb1,ub1
      INT, IN :: lb2,ub2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{INTRINSIC}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb1:ub1,lb2:ub2),stat=status)
      ENSURE(status==0,"MAT{CPX}:create_1 ... error in allocating memory")
#else
      allocate(self(lb1:ub1,lb2:ub2))
#endif
   end subroutine

   PURE subroutine create_2(self,bounds1,bounds2)
   ! Allocate object with the given array bounds
      MAT(CPX,:,:), allocatable, OUT :: self
      VEC(INT,2), IN :: bounds1
      VEC(INT,2), IN :: bounds2

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2))
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      MAT(CPX,:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT{CPX}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ============================
!  Size-of and shape operations
!  ============================

   pure function is_same_shape_as_0(self,a) result (res)
   ! Returns TRUE if the matrix "a" has the same shape as "self"
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), IN :: a
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(a,1) AND size(self,2)==size(a,2)
   end function

   pure function is_same_shape_as_1(self,a) result (res)
   ! Returns TRUE if the matrix "a" has the same shape as "self"
      MAT(CPX,:,:), IN :: self
      MAT(REAL,:,:), IN :: a
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(a,1) AND size(self,2)==size(a,2)
   end function

   pure function is_transposed_shape_of_0(self,a) result (res)
   ! Returns TRUE if the matrix "a" is the transposed shape of self
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), IN :: a
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(a,2) AND size(self,2)==size(a,1)
   end function

   pure function is_transposed_shape_of_1(self,a) result (res)
   ! Returns TRUE if the matrix "a" is the transposed shape of self
      MAT(CPX,:,:), IN :: self
      MAT(REAL,:,:), IN :: a
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(a,2) AND size(self,2)==size(a,1)
   end function

   pure function is_square(self) result (res)
   ! Returns TRUE if the matrix is square
      MAT(CPX,:,:), IN :: self
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(self,2)
   end function


!  =======================
!  Shrinking and expansion
!  =======================

   PURE subroutine shrink(self,dim1,dim2)
   ! Shrink allocatable self to have upper bounds dim1 x dim2.
      MAT(CPX,:,:), allocatable, INOUT :: self
      INT, IN :: dim1,dim2
      MAT(CPX,:,:), allocatable :: old
      INT :: lb1,lb2,ub1,ub2
   ENSURE(allocated(self),"MAT{CPX}:shrink ... matrix not allocated")
   ENSURE(dim1<=ubound(self,1),"MAT{CPX}:shrink ... 1st dimension given is too large.")
   ENSURE(dim2<=ubound(self,2),"MAT{CPX}:shrink ... 2nd dimension given is too large.")

      ! The following code is inherited from MAT{INTRINSIC}
      lb1 = lbound(self,1)
      lb2 = lbound(self,2)
      ub1 = ubound(self,1)
      ub2 = ubound(self,2)
      if (dim1==ub1 AND dim2==ub2) return

      ! Save
      old = self

      ! Shrink
      call destroy_(self)
      call create_(self,lb1,dim1,lb2,dim2)

      ! Copy contents
      self = old(lb1:dim1,lb2:dim2)
   end subroutine

   PURE subroutine shrink_columns(self,dim2)
   ! Shrink columns of allocatable self to upper bound dim2.
      MAT(CPX,:,:), allocatable, INOUT :: self
      INT, IN :: dim2
      INT :: ub1,ub2
   ENSURE(allocated(self),"MAT{CPX}:shrink_columns ... matrix not allocated")
   ENSURE(dim2<=ubound(self,2),"MAT{CPX}:shrink_columns ... 2nd dimension given is too large.")

      ! The following code is inherited from MAT{INTRINSIC}
      ub1 = ubound(self,1)
      ub2 = ubound(self,2)
      if (dim2==ub2) return
      call shrink_(self,ub1,dim2)
   end subroutine

   PURE subroutine expand(self,dim1,dim2)
   ! Expand allocatable self to dim1 x dim2.
   ! New elements are initialised.
      MAT(CPX,:,:), allocatable, INOUT :: self
      INT, IN :: dim1,dim2
      MAT(CPX,:,:), allocatable :: new
      INT :: lb1,lb2
      INT :: ub1,ub2

      ! The following code is inherited from MAT{INTRINSIC}
      if (NOT allocated(self)) then
         call create_(self,dim1,dim2)
      else
         lb1 = lbound(self,1)
         lb2 = lbound(self,2)
         ub1 = ubound(self,1)
         ub2 = ubound(self,2)
         ENSURE(dim1>=ubound(self,1),"MAT{CPX}:expand ... 1st dimension given is too small")
         ENSURE(dim2>=ubound(self,2),"MAT{CPX}:expand ... 2nd dimension given is too small")

         ! Copy contents
         call create_(new,lb1,dim1,lb2,dim2)
         new = ZERO
         new(lb1:ub1,lb2:ub2) = self

         ! Transfer
         call move_alloc(new,self)
      end if
   end subroutine

   PURE subroutine expand_rows(self,dim1)
   ! Expands the rows of allocatable self to upper bound dim1.
   ! New elements are zeroed.
      MAT(CPX,:,:), allocatable, INOUT :: self
      INT, IN :: dim1
      INT :: ub1,ub2

      ! The following code is inherited from MAT{INTRINSIC}
      ub1 = ubound(self,1)
      ub2 = ubound(self,2)
      if (dim1==ub1) return
      call expand_(self,dim1,ub2)
   end subroutine

   PURE subroutine expand_columns(self,dim2)
   ! Expand columns of allocatable self to upper bound dim2.
   ! New elements are zeroed.
      MAT(CPX,:,:), allocatable, INOUT :: self
      INT, IN :: dim2
      INT :: ub1,ub2

      ! The following code is inherited from MAT{INTRINSIC}
      ub1 = ubound(self,1)
      ub2 = ubound(self,2)
      if (dim2==ub2) return
      call expand_(self,ub1,dim2)
   end subroutine

   PURE subroutine append_columns(self,cols)
   ! Append the columns "cols" onto end of allocatable self.
      MAT(CPX,:,:), allocatable, INOUT :: self
      MAT(CPX,:,:), IN :: cols
      INT :: ub2

      ! The following code is inherited from MAT{INTRINSIC}
      if (NOT allocated(self)) then
         self = cols
      else
         ENSURE(size(self,1)==size(cols,1),"MAT{CPX}:append_columns ... 1st dimension wrong, cols")

         ! Expand
         ub2 = ubound(self,2)
         call expand_columns_(self,ub2+size(cols,2))

         ! Append
         self(:,ub2+1:) = cols(:,:)
      end if
   end subroutine

   PURE subroutine append_column(self,col)
   ! Append the column "col" onto end of allocatable self.
      MAT(CPX,:,:), allocatable, INOUT :: self
      VEC(CPX,:), IN :: col
      INT :: ub2

      ! The following code is inherited from MAT{INTRINSIC}
      if (NOT allocated(self)) then
         call create_(self,size(col),1)
         self(:,1) = col(:)
      else
         ENSURE(size(self,1)==size(col),"MAT{CPX}:append_column ... 2nd dimension given is too small")

         ! Expand
         ub2 = ubound(self,2)
         call expand_columns_(self,ub2+1)

         ! Append
         self(:,ub2+1) = col
      end if
   end subroutine

   PURE subroutine prune_column(self,col)
   ! Prune the column with index "col".
      MAT(CPX,:,:), allocatable, INOUT :: self
      INT, IN :: col
      INT :: lb2,ub2
   ENSURE(allocated(self),"MAT{CPX}:prune_column ... self is not.associated")
   ENSURE(is_in_range_(col,[lbound(self,2),ubound(self,2)]),"MAT{CPX}:prune_column ... col is out of range")

      ! The following code is inherited from MAT{INTRINSIC}
      lb2 = lbound(self,2)
      ub2 = ubound(self,2)

      ! Shift down
      self(:,col:ub2-1) = self(:,col+1:)

      ! Shrink
      call shrink_columns_(self,ub2-1)
   end subroutine


!  ====================
!  Comparison functions
!  ====================

   PURE function equals(self,b) result (res)
   ! Check if the matrix is the same as "b".
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), IN :: b
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = same_as_(self,b)
   end function

   PURE function same_as(self,b,eps) result (res)
   ! Check if the matrix is the same as "b", within "eps".
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), IN :: b
      REAL, optional, IN :: eps
      BIN :: res
      REAL :: del,tolerance

      ! Same shape?

      ! The following code is inherited from MAT{INTRINSIC}
      res = is_same_shape_as_(self,b)
      if (NOT res) return
      tolerance = REAL_EPSILON
      if (present(eps)) tolerance = eps

      ! Difference
      del = maxval(abs(self-b))

      ! Within tolerance?
      res = FALSE
      if (del<tolerance) res = TRUE
   end function


!  =================
!  Inquiry functions
!  =================

   PURE function is_diagonal(self,eps) result (res)
   ! Returns TRUE if the matrix "self" is a diagonal matrix to within
   ! tolerance "eps" (if present).
      MAT(CPX,:,:), IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      INT :: dim,i,j
   ENSURE(is_square_(self),"MAT{CPX}:is_diagonal ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      res = TRUE
      if (present(eps)) then
         do i = 1,dim
            do j = 1,dim
               if (i==j) cycle
               if (is_zero_(self(i,j),eps)) cycle
               res = FALSE
               return
            end do
         end do
      else
         do i = 1,dim
            do j = 1,dim
               if (i==j) cycle
               if (is_zero_(self(i,j))) cycle
               res = FALSE
               return
            end do
         end do
      end if
   end function

   PURE function has_unit_diagonal(self,eps) result (res)
   ! Returns TRUE if the matrix "self" has 1's as diagonal elements to within
   ! tolerance "eps" (if present).
      MAT(CPX,:,:), IN :: self
      REAL, IN, optional :: eps
      BIN :: res
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:has_unit_diagonal ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      res = TRUE
      do i = 1,size(self,1)
         if (is_one_(self(i,i),eps)) cycle
         res = FALSE
         return
      end do
   end function

   PURE function has_minus_unit_diagonal(self) result (res)
   ! Returns TRUE if the matrix "self" has -1's as diagonal elements
      MAT(CPX,:,:), IN :: self
      BIN :: res
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:has_minus_unit_diagonal ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      res = TRUE
      do i = 1,size(self,1)
         if (is_minus_one_(self(i,i))) cycle
         res = FALSE
         return
      end do
   end function

   PURE function is_unit_mx(self,eps) result (res)
   ! Returns TRUE if the matrix "self" is the unit matrix to within
   ! tolerance "eps" (if present).
      MAT(CPX,:,:), IN :: self
      REAL, optional, IN :: eps
      BIN :: res
   ENSURE(is_square_(self),"MAT{CPX}:is_unit_mx ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      if (present(eps)) then
         res = has_unit_diagonal_(self,eps) AND is_diagonal_(self,eps)
      else
         res = has_unit_diagonal_(self) AND is_diagonal_(self)
      end if
   end function

   PURE function is_inversion_mx(self) result (res)
   ! Returns TRUE if the matrix "self" is an inversion matrix
   ! i.e. minus the unit matrix
      MAT(CPX,:,:), IN :: self
      BIN :: res
   ENSURE(is_square_(self),"MAT{CPX}:is_inversion_mx ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      res = has_minus_unit_diagonal_(self) AND is_diagonal_(self)
   end function

   PURE function is_symmetric(self) result (res)
   ! Returns TRUE if the matrix "self" is a symmetric matrix
      MAT(CPX,:,:), IN :: self
      BIN :: res
      INT :: i,j
   ENSURE(is_square_(self),"MAT{CPX}:is_symmetric ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      res = TRUE
      do i = 1,size(self,1)
         do j = 1,i-1
            if (equals_(self(i,j),self(j,i))) cycle
            res = FALSE
            return
         end do
      end do
   end function

   PURE function is_antisymmetric(self) result (res)
   ! Returns TRUE if the matrix "self" is an antisymmetric matrix
      MAT(CPX,:,:), IN :: self
      BIN :: res
      INT :: dim,i,j
   ENSURE(is_square_(self),"MAT{CPX}:is_antisymmetric ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      res = TRUE
      do i = 1,dim
         do j = 1,i-1
            if (equals_(self(i,j),-self(j,i))) cycle
            res = FALSE
            return
         end do
      end do

      ! res = .is_square AND .same_as(-transpose(self),tol)
   end function

   PURE function is_zero(self,eps) result (res)
   ! Return TRUE is "self" is the zero matrix, i.e. every element is zero.
   ! If present, "eps" is used to decide when a small number is zero.
      MAT(CPX,:,:), IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      INT :: i,j

      ! The following code is inherited from MAT{INTRINSIC}
      res = TRUE
      do i = 1,size(self,1)
         do j = 1,size(self,2)
            if (is_zero_(self(i,j),eps)) cycle
            res = FALSE
            exit
         end do
      end do
   end function

   PURE function is_hermitian(self,tol) result (res)
   ! Returns TRUE if the matrix is hermitian. You can set the tolerance "tol"
   ! for comparison.
      MAT(CPX,:,:), IN :: self
      BIN :: res
      REAL, optional, IN :: tol
      res = is_square_(self) AND same_as_(self,transpose(conjg(self)),tol)
   end function

   PURE function is_antihermitian(self,tol) result (res)
   ! Returns TRUE if the matrix is antihermitian. You can set the tolerance
   ! "tol" for comparison.
      MAT(CPX,:,:), IN :: self
      BIN :: res
      REAL, optional, IN :: tol
      res = is_square_(self) AND same_as_(self,-transpose(conjg(self)),tol)
   end function


!  =================
!  Column operations
!  =================

   PURE function has_column(self,c,eps) result (res)
   ! Returns TRUE if the matrix "self" has a column "c", with "eps" tolerance.
      MAT(CPX,:,:), IN :: self
      VEC(CPX,:), IN :: c
      REAL, optional, IN :: eps
      BIN :: res
      INT :: n
   ENSURE(size(c)==size(self,1),"MAT{CPX}:has_column ... incompatible column size")

      ! The following code is inherited from MAT{INTRINSIC}
      res = FALSE
      do n = 1,size(self,2)
         res = same_as_(self(:,n),c,eps)
         if (res) exit
      end do
   end function

   PURE function index_for_column(self,c,eps) result (res)
   ! Return the index of the first column in "self" which matches "c",
   ! within tolerance "eps", or else return 0 for no match.
      MAT(CPX,:,:), IN :: self
      VEC(CPX,:), IN :: c
      REAL, optional, IN :: eps
      INT :: res
      INT :: n
   ENSURE(size(c)==size(self,1),"MAT{CPX}:index_for_column ... incompatible column size")

      ! The following code is inherited from MAT{INTRINSIC}
      res = 0
      do n = 1,size(self,2)
         if (same_as_(self(:,n),c,eps)) then
            res = n
            exit
         end if
      end do
   end function

   PURE function indices_for_column(self,c,eps) result (res)
   ! Return the indices of columns in "self" which match "c",
   ! within tolerance "eps", or else return 0 for no match.
      MAT(CPX,:,:), IN :: self
      VEC(CPX,:), IN :: c
      REAL, optional, IN :: eps
      VEC(INT,:), allocatable :: res
      INT :: n
   ENSURE(size(c)==size(self,1),"MAT{CPX}:indices_for_column ... incompatible column size")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(res,0)
      do n = 1,size(self,2)
         if (same_as_(self(:,n),c,eps)) then
            call append_(res,n)
         end if
      end do
   end function

   PURE subroutine swap_columns_0(self,col1,col2)
   ! Swap columns "col1" and "col2" of self
      MAT(CPX,:,:), INOUT :: self
      INT, IN :: col1,col2
      INT :: i
      CPX :: val
   ENSURE(col1<=size(self,2) AND col2<=size(self,2),"MAT{CPX}:swap_columns_0 ... columns exceed dimesions")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         val = self(i,col1)
         self(i,col1) = self(i,col2)
         self(i,col2) = val
      end do
   end subroutine

   PURE subroutine swap_columns_1(self,list)
   ! Sequentially swap all columns in a column "list",
   ! self(:,i)      = self(:,list(i))
   ! self(:,col(i)) = self(:,i)
      MAT(CPX,:,:), INOUT :: self
      VEC(INT,:), IN :: list
      INT :: l
   ENSURE(maxval(list)<=size(self,2),"MAT{CPX}:swap_columns_1 ... list value exceed column dimension")

      ! The following code is inherited from MAT{INTRINSIC}
      do l = 1,size(list)
         call swap_columns_(self,l,list(l))
      end do
   end subroutine

   PURE subroutine reverse_column_order(self)
   ! Reverse the order of the columns of self.
      MAT(CPX,:,:), INOUT :: self
      VEC(CPX,:), allocatable :: tmp
      INT :: n,n_col

      ! The following code is inherited from MAT{INTRINSIC}
      n_col = size(self,2)
      call create_(tmp,size(self,1))
      do n = 1,n_col/2
         tmp = self(:,n_col-n+1)
         self(:,n_col-n+1) = self(:,n)
         self(:,n) = tmp
      end do
      call destroy_(tmp)
   end subroutine

   PURE function column_norms(self) result (res)
   ! Return the norms of every column
      MAT(CPX,:,:), IN :: self
      VEC(REAL,size(self,2)) :: res
      INT :: i

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,2)
         res(i) = norm_(self(:,i))
      end do
   end function

   PURE subroutine get_column_norms(self,res)
   ! Return the norms of every column
      MAT(CPX,:,:), IN :: self
      VEC(REAL,:), OUT :: res
      INT :: i
   ENSURE(size(res)==size(self,2),"MAT{CPX}:get_column_norms ... wrong size, res array")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,2)
         res(i) = norm_(self(:,i))
      end do
   end subroutine

   PURE subroutine get_column_dot_products(self,res)
   ! Return the dot products of every column with itself.
   ! Good for testing distances without using a sqrt.
      MAT(CPX,:,:), IN :: self
      VEC(CPX,:), OUT :: res
      INT :: i
   ENSURE(size(res)==size(self,2),"MAT{CPX}:get_column_dot_products ... wrong size, res array")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,2)
         res(i) = dot_product(self(:,i),self(:,i))
      end do
   end subroutine

   pure function index_of_minimum_column_norm(self) result (res)
   ! Return the column index of the column with the *minimum* norm.
      MAT(CPX,:,:), IN :: self
      INT :: res
      INT :: i
      REAL :: val,tmp

      ! Length of 1st column

      ! The following code is inherited from MAT{INTRINSIC}
      res = 1
      val = norm_(self(:,1))

      ! Loop over remaining columns
      do i = 2,size(self,2)

         ! Is it smallest?
         tmp = norm_(self(:,i))
         if (tmp>=val) cycle

         ! Store smaller value
         val = tmp
         res = i
      end do
   end function

   PURE function max_abs_column_difference(self) result (res)
   ! Return the maximum of the absolute difference between all the column vector
   ! pairs of the matrix.
      MAT(CPX,:,:), IN :: self
      VEC(REAL,size(self,1)) :: res
      VEC(REAL,size(self,1)) :: diff
      VEC(CPX,size(self,1)) :: col_i,col_j
      INT :: i,j

      ! The following code is inherited from MAT{INTRINSIC}
      diff = ZERO
      do i = 1,size(self,2)
         col_i = self(:,i)
         do j = 1,i-1
            col_j = self(:,j)
            diff = max(real(abs(col_i-col_j),kind=REAL_KIND),diff)
         end do
      end do
      res = diff
   end function

   PURE function mean_column_vector(self) result (res)
   ! Return the mean of the column vectors.
      MAT(CPX,:,:), IN :: self
      VEC(CPX,size(self,1)) :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = sum_column_vectors_(self)/size(self,2)
   end function

   PURE function sum_column_vectors(self) result (res)
   ! Sum the column vectors (i.e. rows) in "self".
      MAT(CPX,:,:), IN :: self
      VEC(CPX,size(self,1)) :: res
      INT :: i,j
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         val = 0
         do j = 1,size(self,2)
            val = val+self(i,j)
         end do
         res(i) = val
      end do
   end function


!  ==============
!  Row operations
!  ==============

   pure function sum_row_vectors(self) result (res)
   ! Sum the row vectors (i.e. columns) in "self".
      MAT(CPX,:,:), IN :: self
      VEC(CPX,size(self,2)) :: res
      INT :: i,j
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      do j = 1,size(self,2)
         val = 0
         do i = 1,size(self,1)
            val = val+self(i,j)
         end do
         res(j) = val
      end do
   end function


!  ==========================================
!  Matrix algebra and vector space operations
!  ==========================================

   PURE recursive function determinant(self) result (res)
   ! Return the determinant
      MAT(CPX,:,:), IN :: self
      CPX :: res
      MAT(CPX,:,:), allocatable :: adj
      INT :: c
   ENSURE(is_square_(self),"MAT{CPX}:determinant ... non-square matrix")
   ENSURE(size(self,1)<4,"MAT{CPX}:determinant ... only works for up to size 3 matrices")

      ! The following code is inherited from MAT{INTRINSIC}
      select case (size(self,1))

      case (1)
         res = self(1,1)

      case (2)
         res = self(1,1)*self(2,2)-self(2,1)*self(1,2)

      case (3)
         res = self(1,1)*(self(2,2)*self(3,3)-self(2,3)*self(3,2))-self(1,2)*(self(2,1)*self(3,3)-self(2,3)*self(3,1))+self(1,3)*(self(2,1)*self(3,2)-self(2,2)*self(3,1))

      case default
         res = ZERO
         do c = 1,size(self)
            adj = adjugate_(self,c,1)
            res = res+self(c,1)*determinant_(adj)
         end do
      end select
   end function

   PURE function adjugate(self,i,j) result (res)
   ! Return the adjugate of a matrix
      MAT(CPX,:,:), IN :: self
      INT, IN :: i,j
      MAT(CPX,:,:), allocatable :: res
   ENSURE(is_square_(self),"MAT{CPX}:adjugate ... non-square matrix")
   ENSURE(size(self,1)>1,"MAT{CPX}:adjugate ... dimension must be greater than 1")

      ! Create (leaky)

      ! The following code is inherited from MAT{INTRINSIC}
      allocate(res(size(self,1)-1,size(self,1)-1))

      ! Define
      res(1:i-1,1:j-1) = self(1:i-1,1:j-1)
      res(i:,1:j-1) = self(i+1:,1:j-1)
      res(1:i-1,j:) = self(1:i-1,j+1:)
      res(i:,j:) = self(i+1:,j+1:)
   end function

   pure function sum_elements(self) result (res)
   ! Return the sum of the elements in "self"
      MAT(CPX,:,:), IN :: self
      CPX :: res
      INT :: i,j

      ! The following code is inherited from MAT{INTRINSIC}
      res = 0
      do j = 1,size(self,2)
         do i = 1,size(self,1)
            res = res+self(i,j)
         end do
      end do
   end function

   PURE function dot_0(self,l,r) result (res)
   ! Multiply the matrix self by vector "l" on the left and vector "r" on the
   ! right ie:  res = l^dagger self r. Useful for non-unit metric dot_products.
      MAT(CPX,:,:), IN :: self
      VEC(CPX,:), IN :: l,r
      CPX :: res
      VEC(CPX,:), allocatable :: w
   ENSURE(size(self,1)==size(l),"MAT{CPX}:dot_0 ... wrong size, r")
   ENSURE(size(self,2)==size(r),"MAT{CPX}:dot_0 ... wrong size, r")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(w,size(l))
      call to_product_of_(w,self,r)
      res = dot_product(l,w)
      call destroy_(w)
   end function

   PURE function dot_1(self,l,r) result (res)
   ! Multiply the matrix self by vector "l" on the left and vector "r" on the
   ! right ie:  res = l^dagger self r. Useful for non-unit metric dot_products.
      MAT(CPX,:,:), IN :: self
      VEC(REAL,:), IN :: l,r
      CPX :: res
      VEC(CPX,:), allocatable :: w
   ENSURE(size(self,1)==size(l),"MAT{CPX}:dot_1 ... wrong size, r")
   ENSURE(size(self,2)==size(r),"MAT{CPX}:dot_1 ... wrong size, r")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(w,size(l))
      call to_product_of_(w,self,r)
      res = dot_product(l,w)
      call destroy_(w)
   end function

   PURE subroutine rotate(self,v)
   ! Rotate vector "v" by self
      MAT(CPX,:,:), IN :: self
      VEC(CPX,:), INOUT :: v
      INT :: i,j
      VEC(CPX,:), allocatable :: w
      CPX :: val
   ENSURE(is_square_(self),"MAT{CPX}:rotate ... incompatible arrays sizes")
   ENSURE(size(self,2)==size(v),"MAT{CPX}:rotate ... incompatible arrays sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(w,size(self,2))
      do i = 1,size(self,1)
         val = ZERO
         do j = 1,size(self,2)
            val = val+self(i,j)*v(j)
         end do
         w(i) = val
      end do
      v = w
      call destroy_(w)
   end subroutine

   pure subroutine to_unit_mx(self)
   ! Set "self" to the unit matrix
      MAT(CPX,:,:), OUT :: self
      INT :: i

      ! The following code is inherited from MAT{INTRINSIC}
      self = ZERO
      do i = 1,min(size(self,1),size(self,2))
         self(i,i) = ONE
      end do
   end subroutine

   pure subroutine zero_small_values(self,eps)
   ! Zero elements of the matrix which are less than "eps" in magnitude
      MAT(CPX,:,:), INOUT :: self
      REAL, IN :: eps

      ! The following code is inherited from MAT{INTRINSIC}
      where (abs(self)<eps)
         self = ZERO
      end where
   end subroutine

   PURE subroutine set_to_0(self,a)
   ! Set self to "a"
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: a
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:set_to_0 ... incompatible shape")

      ! The following code is inherited from MAT{INTRINSIC}
      self = a
   end subroutine

   PURE subroutine set_to_1(self,a)
   ! Set self to "a"
      MAT(CPX,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:set_to_1 ... incompatible shape")

      ! The following code is inherited from MAT{INTRINSIC}
      self = a
   end subroutine

   PURE subroutine set_to_transpose_of_0(self,a)
   ! Self becomes the transpose of "a"
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      INT :: i,j,dim
   ENSURE(is_square_(self),"MAT{CPX}:set_to_transpose_of_0 ... non-square matrix")
   ENSURE(is_transposed_shape_of_(self,a),"MAT{CPX}:set_to_transpose_of_0 ... incompatible shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      do i = 1,dim
         do j = 1,dim
            self(i,j) = a(j,i)
         end do
      end do
   end subroutine

   PURE subroutine set_to_transpose_of_1(self,a)
   ! Self becomes the transpose of "a"
      MAT(CPX,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      INT :: i,j,dim
   ENSURE(is_square_(self),"MAT{CPX}:set_to_transpose_of_1 ... non-square matrix")
   ENSURE(is_transposed_shape_of_(self,a),"MAT{CPX}:set_to_transpose_of_1 ... incompatible shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      do i = 1,dim
         do j = 1,dim
            self(i,j) = a(j,i)
         end do
      end do
   end subroutine

   PURE subroutine to_transpose(self)
   ! Self becomes its own transpose.
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j,dim
      CPX :: tmp
   ENSURE(is_square_(self),"MAT{CPX}:to_transpose ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)

      ! Do it element by element, otherwise the intrinsic routine can
      ! run out of stack space
      do i = 1,dim
         do j = 1,i
            tmp = self(i,j)
            self(i,j) = self(j,i)
            self(j,i) = tmp
         end do
      end do
   end subroutine

   PURE subroutine plus_0(self,a)
   ! Add to self the matrix "a"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:plus_0 ... incompatible shape")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self+a
   end subroutine

   PURE subroutine plus_1(self,a)
   ! Add to self the matrix "a"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:plus_1 ... incompatible shape")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self+a
   end subroutine

   PURE subroutine minus_0(self,a)
   ! Subtract from self the matrix "a"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:minus_0 ... incompatible shape")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self-a
   end subroutine

   PURE subroutine minus_1(self,a)
   ! Subtract from self the matrix "a"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:minus_1 ... incompatible shape")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self-a
   end subroutine

   PURE subroutine to_scaled_0(self,a,fac)
   ! Set "self" to matrix "at" scaled by "fac"
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      CPX, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:to_scaled_0 ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = fac*a
   end subroutine

   PURE subroutine to_scaled_1(self,a,fac)
   ! Set "self" to matrix "at" scaled by "fac"
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      REAL, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:to_scaled_1 ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = fac*a
   end subroutine

   PURE subroutine to_scaled_2(self,a,fac)
   ! Set "self" to matrix "at" scaled by "fac"
      MAT(CPX,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      CPX, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:to_scaled_2 ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = fac*a
   end subroutine

   PURE subroutine to_scaled_3(self,a,fac)
   ! Set "self" to matrix "at" scaled by "fac"
      MAT(CPX,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      REAL, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:to_scaled_3 ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = fac*a
   end subroutine

   PURE subroutine plus_scaled_0(self,a,fac)
   ! Add to "self" matrix "a" scaled by "fac"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      CPX, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:plus_scaled_0 ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self+fac*a
   end subroutine

   PURE subroutine plus_scaled_1(self,a,fac)
   ! Add to "self" matrix "a" scaled by "fac"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      REAL, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:plus_scaled_1 ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self+fac*a
   end subroutine

   PURE subroutine plus_scaled_2(self,a,fac)
   ! Add to "self" matrix "a" scaled by "fac"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      CPX, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:plus_scaled_2 ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self+fac*a
   end subroutine

   PURE subroutine plus_scaled_3(self,a,fac)
   ! Add to "self" matrix "a" scaled by "fac"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      REAL, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:plus_scaled_3 ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self+fac*a
   end subroutine

   PURE subroutine minus_scaled_0(self,a,fac)
   ! Subtract from "self" matrix "a" scaled by "fac"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      CPX, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:minus_scaled_0 ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self-fac*a
   end subroutine

   PURE subroutine minus_scaled_1(self,a,fac)
   ! Subtract from "self" matrix "a" scaled by "fac"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      REAL, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:minus_scaled_1 ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self-fac*a
   end subroutine

   PURE subroutine minus_scaled_2(self,a,fac)
   ! Subtract from "self" matrix "a" scaled by "fac"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      CPX, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:minus_scaled_2 ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self-fac*a
   end subroutine

   PURE subroutine minus_scaled_3(self,a,fac)
   ! Subtract from "self" matrix "a" scaled by "fac"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      REAL, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:minus_scaled_3 ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self-fac*a
   end subroutine

   PURE subroutine to_product_of_0(self,a,b,transpose_a,transpose_b)
   ! Set "self" to the matrix product of "a" and "b". If present, "transpose_a"
   ! and "transpose_b" can be set to TRUE if "a" and "b" need to be transposed.
      MAT(CPX,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a,b
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: transpose_b
      BIN :: trans_a,trans_b
      INT :: i,j,k,opt
      REAL :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (trans_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:to_product_of_0 ... incompatible arrays")
         do j = 1,size(b,2)
            do i = 1,size(a,1)
               val = 0
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = val
            end do
         end do

      case (1)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_product_of_0 ... incompatible arrays")
         do j = 1,size(b,2)
            do i = 1,size(a,2)
               val = 0
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = val
            end do
         end do

      case (2)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_product_of_0 ... incompatible arrays")
         do j = 1,size(b,1)
            do i = 1,size(a,1)
               val = 0
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = val
            end do
         end do

      case (3)  ! .to_product_at_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_product_of_0 ... incompatible arrays")
         do j = 1,size(b,1)
            do i = 1,size(a,2)
               val = 0
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = val
            end do
         end do
      end select
   end subroutine

   PURE subroutine to_product_of_1(self,a,b,dagger_a,dagger_b,transpose_a,transpose_b)
   ! Set "self" to the matrix product of "a" and "b". If present, "transpose_a"
   ! and "transpose_b" can be set to TRUE if "a" and "b" need to be transposed.
   ! Likewise for the dagger operations.
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: a,b
      BIN, optional, IN :: dagger_a,dagger_b
      BIN, optional, IN :: transpose_a,transpose_b
      BIN :: dagg_a,dagg_b,trans_a,trans_b
      INT :: i,j,k,opt
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"MAT{CPX}:to_product_of_1 ... can't transpose and dagger a")
   ENSURE(NOT (present(dagger_b) AND present(transpose_b)),"MAT{CPX}:to_product_of_1 ... can't transpose and dagger b")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      dagg_a = FALSE; dagg_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a) opt = opt+1
      if (trans_a) opt = opt+2
      if (dagg_b) opt = opt+3
      if (trans_b) opt = opt+6
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = val
            end do
         end do

      case (1)  ! .to_product_ad_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(k,j)
               end do
               self(i,j) = val
            end do
         end do

      case (2)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = val
            end do
         end do

      case (3)  ! .to_product_an_bd
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*conjg(b(j,k))
               end do
               self(i,j) = val
            end do
         end do

      case (4)  ! .to_product_ad_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = conjg(val)
            end do
         end do

      case (5)  ! .to_product_at_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_product_of_1 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_product_of_1 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_product_of_1 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*conjg(b(j,k))
               end do
               self(i,j) = val
            end do
         end do

      case (6)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = val
            end do
         end do

      case (7)  ! .to_product_ad_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_product_of_1 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_product_of_1 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_product_of_1 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(j,k)
               end do
               self(i,j) = val
            end do
         end do

      case (8)  ! .to_product_at_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = val
            end do
         end do
      end select
   end subroutine

   PURE subroutine to_product_of_2(self,a,b,dagger_a,transpose_b)
   ! Set "self" to the product of complex matrix "a" and real matrix "b".  If
   ! present, "dagger_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! needs to be daggerred or transposed.
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      MAT(REAL,:,:), IN :: b
      BIN, optional, IN :: dagger_a
      BIN, optional, IN :: transpose_b
      BIN :: dagg_a,trans_b
      INT :: opt,i,j,k
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      dagg_a = FALSE; trans_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_product_of_2 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_product_of_2 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:to_product_of_2 ... incompatible shapes")
         do i = 1,size(a,1)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = val
            end do
         end do

      case (1)  ! .to_product_ad_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_product_of_2 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_product_of_2 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_product_of_2 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(k,j)
               end do
               self(i,j) = val
            end do
         end do

      case (2)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_product_of_2 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_product_of_2 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_product_of_2 ... incompatible shapes")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = val
            end do
         end do

      case (3)  ! .to_product_ad_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_product_of_2 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_product_of_2 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_product_of_2 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(j,k)
               end do
               self(i,j) = val
            end do
         end do
      end select
   end subroutine

   PURE subroutine to_product_of_3(self,a,b,transpose_a,dagger_b)
   ! Set "self" to the product of real matrix "a" and complex matrix "b".  If
   ! present, "transpose_a" and "dagger_b" can be set to TRUE if "a" and "b"
   ! needs to be daggerred or transposed.
      MAT(CPX,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      MAT(CPX,:,:), IN :: b
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: dagger_b
      BIN :: trans_a,dagg_b
      INT :: opt,i,j,k
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; dagg_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (trans_a) opt = opt+1
      if (dagg_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_product_of_3 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_product_of_3 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:to_product_of_3 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = val
            end do
         end do

      case (1)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_product_of_3 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_product_of_3 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_product_of_3 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = val
            end do
         end do

      case (2)  ! .to_product_an_bd
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_product_of_3 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_product_of_3 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_product_of_3 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*conjg(b(j,k))
               end do
               self(i,j) = val
            end do
         end do

      case (3)  ! .to_product_at_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_product_of_3 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_product_of_3 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_product_of_3 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*conjg(b(j,k))
               end do
               self(i,j) = val
            end do
         end do
      end select
   end subroutine

   PURE subroutine plus_product_of_0(self,a,b,transpose_a,transpose_b)
   ! Add to "self" the matrix product of "a" and "b". If present, "transpose_a"
   ! and "transpose_b" can be set to TRUE if "a" and "b" need to be transposed.
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a,b
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: transpose_b
      BIN :: trans_a,trans_b
      INT :: i,j,k,opt
      REAL :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (trans_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:plus_product_of_0 ... incompatible arrays")
         do j = 1,size(b,2)
            do i = 1,size(a,1)
               val = 0
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (1)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_product_of_0 ... incompatible arrays")
         do j = 1,size(b,2)
            do i = 1,size(a,2)
               val = 0
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (2)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_product_of_0 ... incompatible arrays")
         do j = 1,size(b,1)
            do i = 1,size(a,1)
               val = 0
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (3)  ! .to_product_at_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_product_of_0 ... incompatible arrays")
         do j = 1,size(b,1)
            do i = 1,size(a,2)
               val = 0
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do
      end select
   end subroutine

   PURE subroutine plus_product_of_1(self,a,b,dagger_a,dagger_b,transpose_a,transpose_b)
   ! Add to "self" the matrix product of "a" and "b". If present, "transpose_a"
   ! and "transpose_b" can be set to TRUE if "a" and "b" need to be transposed.
   ! Likewise for the dagger operations.
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: a,b
      BIN, optional, IN :: dagger_a,dagger_b
      BIN, optional, IN :: transpose_a,transpose_b
      BIN :: dagg_a,dagg_b,trans_a,trans_b
      INT :: i,j,k,opt
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"MAT{CPX}:plus_product_of_1 ... can't transpose and dagger a")
   ENSURE(NOT (present(dagger_b) AND present(transpose_b)),"MAT{CPX}:plus_product_of_1 ... can't transpose and dagger b")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      dagg_a = FALSE; dagg_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a) opt = opt+1
      if (trans_a) opt = opt+2
      if (dagg_b) opt = opt+3
      if (trans_b) opt = opt+6
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (1)  ! .to_product_ad_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(k,j)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (2)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (3)  ! .to_product_an_bd
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*conjg(b(j,k))
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (4)  ! .to_product_ad_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = self(i,j)+conjg(val)
            end do
         end do

      case (5)  ! .to_product_at_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_product_of_1 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_product_of_1 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_product_of_1 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*conjg(b(j,k))
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (6)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (7)  ! .to_product_ad_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_product_of_1 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_product_of_1 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_product_of_1 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(j,k)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (8)  ! .to_product_at_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do
      end select
   end subroutine

   PURE subroutine plus_product_of_2(self,a,b,dagger_a,transpose_b)
   ! Add to "self" the product of complex matrix "a" and real matrix "b".  If
   ! present, "dagger_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! needs to be daggerred or transposed.
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      MAT(REAL,:,:), IN :: b
      BIN, optional, IN :: dagger_a
      BIN, optional, IN :: transpose_b
      BIN :: dagg_a,trans_b
      INT :: opt,i,j,k
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      dagg_a = FALSE; trans_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_product_of_2 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_product_of_2 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:plus_product_of_2 ... incompatible shapes")
         do i = 1,size(a,1)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (1)  ! .to_product_ad_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_product_of_2 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_product_of_2 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_product_of_2 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(k,j)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (2)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_product_of_2 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_product_of_2 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_product_of_2 ... incompatible shapes")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (3)  ! .to_product_ad_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_product_of_2 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_product_of_2 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_product_of_2 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(j,k)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do
      end select
   end subroutine

   PURE subroutine plus_product_of_3(self,a,b,transpose_a,dagger_b)
   ! Add to "self" the product of real matrix "a" and complex matrix "b".  If
   ! present, "transpose_a" and "dagger_b" can be set to TRUE if "a" and "b"
   ! needs to be daggerred or transposed.
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      MAT(CPX,:,:), IN :: b
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: dagger_b
      BIN :: trans_a,dagg_b
      INT :: opt,i,j,k
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; dagg_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (trans_a) opt = opt+1
      if (dagg_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_product_of_3 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_product_of_3 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:plus_product_of_3 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (1)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_product_of_3 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_product_of_3 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_product_of_3 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (2)  ! .to_product_an_bd
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_product_of_3 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_product_of_3 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_product_of_3 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*conjg(b(j,k))
               end do
               self(i,j) = self(i,j)+val
            end do
         end do

      case (3)  ! .to_product_at_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_product_of_3 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_product_of_3 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_product_of_3 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*conjg(b(j,k))
               end do
               self(i,j) = self(i,j)+val
            end do
         end do
      end select
   end subroutine

   PURE subroutine to_scaled_product_of_0(self,a,b,fac,transpose_a,transpose_b)
   ! Set "self" to the matrix product of "a" and "b" scaled by "fac". If
   ! present, "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! need to be transposed.
      MAT(CPX,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a,b
      CPX, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: transpose_b
      BIN :: trans_a,trans_b
      INT :: i,j,k,opt
      REAL :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (trans_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         do j = 1,size(b,2)
            do i = 1,size(a,1)
               val = 0
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (1)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         do j = 1,size(b,2)
            do i = 1,size(a,2)
               val = 0
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (2)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         do j = 1,size(b,1)
            do i = 1,size(a,1)
               val = 0
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (3)  ! .to_product_at_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         do j = 1,size(b,1)
            do i = 1,size(a,2)
               val = 0
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine to_scaled_product_of_1(self,a,b,fac,transpose_a,transpose_b)
   ! Set "self" to the matrix product of "a" and "b" scaled by "fac". If
   ! present, "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! need to be transposed.
      MAT(CPX,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a,b
      REAL, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: transpose_b
      BIN :: trans_a,trans_b
      INT :: i,j,k,opt
      REAL :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (trans_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         do j = 1,size(b,2)
            do i = 1,size(a,1)
               val = 0
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (1)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         do j = 1,size(b,2)
            do i = 1,size(a,2)
               val = 0
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (2)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         do j = 1,size(b,1)
            do i = 1,size(a,1)
               val = 0
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (3)  ! .to_product_at_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         do j = 1,size(b,1)
            do i = 1,size(a,2)
               val = 0
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine to_scaled_product_of_2(self,a,b,fac,dagger_a,dagger_b,transpose_a,transpose_b)
   ! Set "self" to the matrix product of "a" and "b" scaled by "fac". If
   ! present, "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! need to be transposed.  Likewise for the dagger operations.
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: a,b
      CPX, IN :: fac
      BIN, optional, IN :: dagger_a,dagger_b
      BIN, optional, IN :: transpose_a,transpose_b
      BIN :: dagg_a,dagg_b,trans_a,trans_b
      INT :: i,j,k,opt
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"MAT{CPX}:to_scaled_product_of_2 ... can't transpose and dagger a")
   ENSURE(NOT (present(dagger_b) AND present(transpose_b)),"MAT{CPX}:to_scaled_product_of_2 ... can't transpose and dagger b")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      dagg_a = FALSE; dagg_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a) opt = opt+1
      if (trans_a) opt = opt+2
      if (dagg_b) opt = opt+3
      if (trans_b) opt = opt+6
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (1)  ! .to_product_ad_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (2)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (3)  ! .to_product_an_bd
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*conjg(b(j,k))
               end do
               self(i,j) = fac*val
            end do
         end do

      case (4)  ! .to_product_ad_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = fac*conjg(val)
            end do
         end do

      case (5)  ! .to_product_at_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_2 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*conjg(b(j,k))
               end do
               self(i,j) = fac*val
            end do
         end do

      case (6)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (7)  ! .to_product_ad_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_2 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (8)  ! .to_product_at_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine to_scaled_product_of_3(self,a,b,fac,dagger_a,dagger_b,transpose_a,transpose_b)
   ! Set "self" to the matrix product of "a" and "b" scaled by "fac". If
   ! present, "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! need to be transposed.  Likewise for the dagger operations.
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: a,b
      REAL, IN :: fac
      BIN, optional, IN :: dagger_a,dagger_b
      BIN, optional, IN :: transpose_a,transpose_b
      BIN :: dagg_a,dagg_b,trans_a,trans_b
      INT :: i,j,k,opt
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"MAT{CPX}:to_scaled_product_of_3 ... can't transpose and dagger a")
   ENSURE(NOT (present(dagger_b) AND present(transpose_b)),"MAT{CPX}:to_scaled_product_of_3 ... can't transpose and dagger b")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      dagg_a = FALSE; dagg_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a) opt = opt+1
      if (trans_a) opt = opt+2
      if (dagg_b) opt = opt+3
      if (trans_b) opt = opt+6
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (1)  ! .to_product_ad_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (2)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (3)  ! .to_product_an_bd
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*conjg(b(j,k))
               end do
               self(i,j) = fac*val
            end do
         end do

      case (4)  ! .to_product_ad_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = fac*conjg(val)
            end do
         end do

      case (5)  ! .to_product_at_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_3 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*conjg(b(j,k))
               end do
               self(i,j) = fac*val
            end do
         end do

      case (6)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (7)  ! .to_product_ad_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_3 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (8)  ! .to_product_at_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine to_scaled_product_of_4(self,a,b,fac,dagger_a,transpose_b)
   ! Set "self" to the product of complex matrix "a" and real matrix "b" scaled
   ! by "fac".  If present, "dagger_a" and "transpose_b" can be set to TRUE if
   ! "a" and "b" needs to be daggerred or transposed.
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      MAT(REAL,:,:), IN :: b
      CPX, IN :: fac
      BIN, optional, IN :: dagger_a
      BIN, optional, IN :: transpose_b
      BIN :: dagg_a,trans_b
      INT :: opt,i,j,k
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      dagg_a = FALSE; trans_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:to_scaled_product_of_4 ... incompatible shapes")
         do i = 1,size(a,1)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (1)  ! .to_product_ad_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_scaled_product_of_4 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (2)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_scaled_product_of_4 ... incompatible shapes")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (3)  ! .to_product_ad_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_4 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine to_scaled_product_of_5(self,a,b,fac,dagger_a,transpose_b)
   ! Set "self" to the product of complex matrix "a" and real matrix "b" scaled
   ! by "fac".  If present, "dagger_a" and "transpose_b" can be set to TRUE if
   ! "a" and "b" needs to be daggerred or transposed.
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      MAT(REAL,:,:), IN :: b
      REAL, IN :: fac
      BIN, optional, IN :: dagger_a
      BIN, optional, IN :: transpose_b
      BIN :: dagg_a,trans_b
      INT :: opt,i,j,k
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      dagg_a = FALSE; trans_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:to_scaled_product_of_5 ... incompatible shapes")
         do i = 1,size(a,1)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (1)  ! .to_product_ad_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_scaled_product_of_5 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (2)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_scaled_product_of_5 ... incompatible shapes")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (3)  ! .to_product_ad_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_5 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(j,k)
               end do
               self(i,j) = fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine to_scaled_product_of_6(self,a,b,fac,transpose_a,dagger_b)
   ! Set "self" to the product of real matrix "a" and complex matrix "b" scaled
   ! by "fac".  If present, "transpose_a" and "dagger_b" can be set to TRUE if
   ! "a" and "b" needs to be daggerred or transposed.
      MAT(CPX,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      MAT(CPX,:,:), IN :: b
      CPX, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: dagger_b
      BIN :: trans_a,dagg_b
      INT :: opt,i,j,k
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; dagg_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (trans_a) opt = opt+1
      if (dagg_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:to_scaled_product_of_6 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (1)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_scaled_product_of_6 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (2)  ! .to_product_an_bd
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_scaled_product_of_6 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*conjg(b(j,k))
               end do
               self(i,j) = fac*val
            end do
         end do

      case (3)  ! .to_product_at_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_6 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*conjg(b(j,k))
               end do
               self(i,j) = fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine to_scaled_product_of_7(self,a,b,fac,transpose_a,dagger_b)
   ! Set "self" to the product of real matrix "a" and complex matrix "b" scaled
   ! by "fac".  If present, "transpose_a" and "dagger_b" can be set to TRUE if
   ! "a" and "b" needs to be daggerred or transposed.
      MAT(CPX,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      MAT(CPX,:,:), IN :: b
      REAL, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: dagger_b
      BIN :: trans_a,dagg_b
      INT :: opt,i,j,k
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; dagg_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (trans_a) opt = opt+1
      if (dagg_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:to_scaled_product_of_7 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (1)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:to_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:to_scaled_product_of_7 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = fac*val
            end do
         end do

      case (2)  ! .to_product_an_bd
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:to_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:to_scaled_product_of_7 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*conjg(b(j,k))
               end do
               self(i,j) = fac*val
            end do
         end do

      case (3)  ! .to_product_at_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:to_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:to_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:to_scaled_product_of_7 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*conjg(b(j,k))
               end do
               self(i,j) = fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine plus_scaled_product_of_0(self,a,b,fac,transpose_a,transpose_b)
   ! Add to "self" the matrix product of "a" and "b" scaled by "fac". If
   ! present, "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! neeb to be transposed.
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a,b
      CPX, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: transpose_b
      BIN :: trans_a,trans_b
      INT :: i,j,k,opt
      REAL :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (trans_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         do j = 1,size(b,2)
            do i = 1,size(a,1)
               val = 0
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (1)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         do j = 1,size(b,2)
            do i = 1,size(a,2)
               val = 0
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (2)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         do j = 1,size(b,1)
            do i = 1,size(a,1)
               val = 0
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (3)  ! .to_product_at_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         do j = 1,size(b,1)
            do i = 1,size(a,2)
               val = 0
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine plus_scaled_product_of_1(self,a,b,fac,transpose_a,transpose_b)
   ! Add to "self" the matrix product of "a" and "b" scaled by "fac". If
   ! present, "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! neeb to be transposed.
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a,b
      REAL, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: transpose_b
      BIN :: trans_a,trans_b
      INT :: i,j,k,opt
      REAL :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (trans_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         do j = 1,size(b,2)
            do i = 1,size(a,1)
               val = 0
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (1)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         do j = 1,size(b,2)
            do i = 1,size(a,2)
               val = 0
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (2)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         do j = 1,size(b,1)
            do i = 1,size(a,1)
               val = 0
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (3)  ! .to_product_at_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         do j = 1,size(b,1)
            do i = 1,size(a,2)
               val = 0
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine plus_scaled_product_of_2(self,a,b,fac,dagger_a,dagger_b,transpose_a,transpose_b)
   ! Add to "self" the matrix product of "a" and "b" scaled by "fac". If
   ! present, "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! need to be transposed.  Likewise for the dagger operations.
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: a,b
      CPX, IN :: fac
      BIN, optional, IN :: dagger_a,dagger_b
      BIN, optional, IN :: transpose_a,transpose_b
      BIN :: dagg_a,dagg_b,trans_a,trans_b
      INT :: i,j,k,opt
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"MAT{CPX}:plus_scaled_product_of_2 ... can't transpose and dagger a")
   ENSURE(NOT (present(dagger_b) AND present(transpose_b)),"MAT{CPX}:plus_scaled_product_of_2 ... can't transpose and dagger b")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      dagg_a = FALSE; dagg_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a) opt = opt+1
      if (trans_a) opt = opt+2
      if (dagg_b) opt = opt+3
      if (trans_b) opt = opt+6
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (1)  ! .to_product_ad_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (2)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (3)  ! .to_product_an_bd
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*conjg(b(j,k))
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (4)  ! .to_product_ad_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*conjg(val)
            end do
         end do

      case (5)  ! .to_product_at_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*conjg(b(j,k))
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (6)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (7)  ! .to_product_ad_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (8)  ! .to_product_at_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine plus_scaled_product_of_3(self,a,b,fac,dagger_a,dagger_b,transpose_a,transpose_b)
   ! Add to "self" the matrix product of "a" and "b" scaled by "fac". If
   ! present, "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! need to be transposed.  Likewise for the dagger operations.
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: a,b
      REAL, IN :: fac
      BIN, optional, IN :: dagger_a,dagger_b
      BIN, optional, IN :: transpose_a,transpose_b
      BIN :: dagg_a,dagg_b,trans_a,trans_b
      INT :: i,j,k,opt
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"MAT{CPX}:plus_scaled_product_of_3 ... can't transpose and dagger a")
   ENSURE(NOT (present(dagger_b) AND present(transpose_b)),"MAT{CPX}:plus_scaled_product_of_3 ... can't transpose and dagger b")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      dagg_a = FALSE; dagg_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a) opt = opt+1
      if (trans_a) opt = opt+2
      if (dagg_b) opt = opt+3
      if (trans_b) opt = opt+6
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (1)  ! .to_product_ad_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (2)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (3)  ! .to_product_an_bd
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*conjg(b(j,k))
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (4)  ! .to_product_ad_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*conjg(val)
            end do
         end do

      case (5)  ! .to_product_at_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*conjg(b(j,k))
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (6)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (7)  ! .to_product_ad_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (8)  ! .to_product_at_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine plus_scaled_product_of_4(self,a,b,fac,dagger_a,transpose_b)
   ! Add to "self" the product of complex matrix "a" and real matrix "b" scaled
   ! by "fac".  If present, "dagger_a" and "transpose_b" can be set to TRUE if
   ! "a" and "b" needs to be daggerred or transposed.
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      MAT(REAL,:,:), IN :: b
      CPX, IN :: fac
      BIN, optional, IN :: dagger_a
      BIN, optional, IN :: transpose_b
      BIN :: dagg_a,trans_b
      INT :: opt,i,j,k
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      dagg_a = FALSE; trans_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:plus_scaled_product_of_4 ... incompatible shapes")
         do i = 1,size(a,1)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (1)  ! .to_product_ad_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_scaled_product_of_4 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (2)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_scaled_product_of_4 ... incompatible shapes")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (3)  ! .to_product_ad_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_4 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_4 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine plus_scaled_product_of_5(self,a,b,fac,dagger_a,transpose_b)
   ! Add to "self" the product of complex matrix "a" and real matrix "b" scaled
   ! by "fac".  If present, "dagger_a" and "transpose_b" can be set to TRUE if
   ! "a" and "b" needs to be daggerred or transposed.
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      MAT(REAL,:,:), IN :: b
      REAL, IN :: fac
      BIN, optional, IN :: dagger_a
      BIN, optional, IN :: transpose_b
      BIN :: dagg_a,trans_b
      INT :: opt,i,j,k
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      dagg_a = FALSE; trans_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:plus_scaled_product_of_5 ... incompatible shapes")
         do i = 1,size(a,1)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (1)  ! .to_product_ad_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_scaled_product_of_5 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (2)  ! .to_product_an_bt
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_scaled_product_of_5 ... incompatible shapes")
         do i = 1,size(a,1)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (3)  ! .to_product_ad_bt
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_5 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_5 ... incompatible shapes")
         do i = 1,size(a,2)
            do j = 1,size(b,1)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+conjg(a(k,i))*b(j,k)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine plus_scaled_product_of_6(self,a,b,fac,transpose_a,dagger_b)
   ! Add to "self" the product of real matrix "a" and complex matrix "b" scaled
   ! by "fac".  If present, "transpose_a" and "dagger_b" can be set to TRUE if
   ! "a" and "b" needs to be daggerred or transposed.
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      MAT(CPX,:,:), IN :: b
      CPX, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: dagger_b
      BIN :: trans_a,dagg_b
      INT :: opt,i,j,k
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; dagg_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (trans_a) opt = opt+1
      if (dagg_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:plus_scaled_product_of_6 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (1)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_scaled_product_of_6 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (2)  ! .to_product_an_bd
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_scaled_product_of_6 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*conjg(b(j,k))
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (3)  ! .to_product_at_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_6 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_6 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*conjg(b(j,k))
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine plus_scaled_product_of_7(self,a,b,fac,transpose_a,dagger_b)
   ! Add to "self" the product of real matrix "a" and complex matrix "b" scaled
   ! by "fac".  If present, "transpose_a" and "dagger_b" can be set to TRUE if
   ! "a" and "b" needs to be daggerred or transposed.
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      MAT(CPX,:,:), IN :: b
      REAL, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: dagger_b
      BIN :: trans_a,dagg_b
      INT :: opt,i,j,k
      CPX :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; dagg_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (trans_a) opt = opt+1
      if (dagg_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,1),"MAT{CPX}:plus_scaled_product_of_7 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (1)  ! .to_product_at_bn
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,2),"MAT{CPX}:plus_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,1),"MAT{CPX}:plus_scaled_product_of_7 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*b(k,j)
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (2)  ! .to_product_an_bd
         ENSURE(size(self,1)>=size(a,1),"MAT{CPX}:plus_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(a,2)==size(b,2),"MAT{CPX}:plus_scaled_product_of_7 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,2)
                  val = val+a(i,k)*conjg(b(j,k))
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do

      case (3)  ! .to_product_at_bd
         ENSURE(size(self,1)>=size(a,2),"MAT{CPX}:plus_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(self,2)>=size(b,1),"MAT{CPX}:plus_scaled_product_of_7 ... incompatible shapes")
         ENSURE(size(a,1)==size(b,2),"MAT{CPX}:plus_scaled_product_of_7 ... incompatible shapes")
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               val = (ZERO,ZERO)
               do k = 1,size(a,1)
                  val = val+a(k,i)*conjg(b(j,k))
               end do
               self(i,j) = self(i,j)+fac*val
            end do
         end do
      end select
   end subroutine

   PURE subroutine to_product_with_diagonal_0(self,a,diag,transpose_a)
   ! Set "self" to the matrix product of "a" with diagonal matrix "diag" (stored
   ! as a vector).  If present, ""transpose_a" can be set to TRUE if "a" needs
   ! to be transposed.
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(CPX,:), IN :: diag
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      INT :: opt,j
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:to_product_with_diagonal_0 ... incompatible dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE;
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (trans_a) opt = 1
      select case (opt)

      case (0)
         ENSURE(size(a,2)==size(diag),"MAT{CPX}:to_product_with_diagonal_0 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = a(:,j)*diag(j)
         end do

      case (1)
         ENSURE(size(a,1)==size(diag),"MAT{CPX}:to_product_with_diagonal_0 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = a(j,:)*diag(j)
         end do
      end select
   end subroutine

   PURE subroutine to_product_with_diagonal_1(self,a,diag,transpose_a)
   ! Set "self" to the matrix product of "a" with diagonal matrix "diag" (stored
   ! as a vector).  If present, ""transpose_a" can be set to TRUE if "a" needs
   ! to be transposed.
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(REAL,:), IN :: diag
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      INT :: opt,j
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:to_product_with_diagonal_1 ... incompatible dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE;
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (trans_a) opt = 1
      select case (opt)

      case (0)
         ENSURE(size(a,2)==size(diag),"MAT{CPX}:to_product_with_diagonal_1 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = a(:,j)*diag(j)
         end do

      case (1)
         ENSURE(size(a,1)==size(diag),"MAT{CPX}:to_product_with_diagonal_1 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = a(j,:)*diag(j)
         end do
      end select
   end subroutine

   PURE subroutine to_product_with_diagonal_2(self,a,diag,dagger_a,transpose_a)
   ! Set "self" to the matrix product of "a" with diagonal matrix "diag" (stored
   ! as a vector).  If present, "dagger_a" or "transpose_a" can be set to TRUE
   ! if "a" needs to be daggered or transposed.
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(CPX,:), IN :: diag
      BIN, optional, IN :: dagger_a
      BIN, optional, IN :: transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,j
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:to_product_with_diagonal_2 ... incompatible dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)  ! an
         ENSURE(size(a,2)==size(diag),"MAT{CPX}:to_product_with_diagonal_2 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = a(:,j)*diag(j)
         end do

      case (1)  ! ad
         ENSURE(size(a,1)==size(diag),"MAT{CPX}:to_product_with_diagonal_2 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = conjg(a(j,:))*diag(j)
         end do

      case (2)  ! at
         ENSURE(size(a,1)==size(diag),"MAT{CPX}:to_product_with_diagonal_2 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = a(j,:)*diag(j)
         end do
      end select
   end subroutine

   PURE subroutine to_product_with_diagonal_3(self,a,diag,dagger_a,transpose_a)
   ! Set "self" to the matrix product of "a" with diagonal matrix "diag" (stored
   ! as a vector).  If present, "dagger_a" or "transpose_a" can be set to TRUE
   ! if "a" needs to be daggered or transposed.
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(REAL,:), IN :: diag
      BIN, optional, IN :: dagger_a
      BIN, optional, IN :: transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,j
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:to_product_with_diagonal_3 ... incompatible dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)  ! an
         ENSURE(size(a,2)==size(diag),"MAT{CPX}:to_product_with_diagonal_3 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = a(:,j)*diag(j)
         end do

      case (1)  ! ad
         ENSURE(size(a,1)==size(diag),"MAT{CPX}:to_product_with_diagonal_3 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = conjg(a(j,:))*diag(j)
         end do

      case (2)  ! at
         ENSURE(size(a,1)==size(diag),"MAT{CPX}:to_product_with_diagonal_3 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = a(j,:)*diag(j)
         end do
      end select
   end subroutine

   PURE subroutine to_product_with_diagonal_4(self,dg,a,transpose_a)
   ! Set "self" to the matrix product of diagonal matrix "dg" (stored as a
   ! vector) and "a".  If present, "transpose_a" can be set to TRUE if "a" needs
   ! to be transposed.
      MAT(CPX,:,:), OUT :: self
      VEC(CPX,:), IN :: dg
      MAT(REAL,:,:), IN :: a
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      INT :: opt,i
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:to_product_with_diagonal_4 ... incompatible dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (trans_a) opt = 1
      select case (opt)

      case (0)  ! an
         ENSURE(size(a,1)==size(dg),"MAT{CPX}:to_product_with_diagonal_4 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*a(i,:)
         end do

      case (1)  ! at
         ENSURE(size(a,2)==size(dg),"MAT{CPX}:to_product_with_diagonal_4 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*a(:,i)
         end do
      end select
   end subroutine

   PURE subroutine to_product_with_diagonal_5(self,dg,a,transpose_a)
   ! Set "self" to the matrix product of diagonal matrix "dg" (stored as a
   ! vector) and "a".  If present, "transpose_a" can be set to TRUE if "a" needs
   ! to be transposed.
      MAT(CPX,:,:), OUT :: self
      VEC(REAL,:), IN :: dg
      MAT(REAL,:,:), IN :: a
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      INT :: opt,i
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:to_product_with_diagonal_5 ... incompatible dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (trans_a) opt = 1
      select case (opt)

      case (0)  ! an
         ENSURE(size(a,1)==size(dg),"MAT{CPX}:to_product_with_diagonal_5 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*a(i,:)
         end do

      case (1)  ! at
         ENSURE(size(a,2)==size(dg),"MAT{CPX}:to_product_with_diagonal_5 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*a(:,i)
         end do
      end select
   end subroutine

   PURE subroutine to_product_with_diagonal_6(self,dg,a,dagger_a,transpose_a)
   ! Set "self" to the matrix product of diagonal matrix "dg" (stored as a
   ! vector) and "a".  If present, "dagger_a" or "transpose_a" can be set to
   ! TRUE if "a" needs to be daggered or transposed.
      MAT(CPX,:,:), OUT :: self
      VEC(CPX,:), IN :: dg
      MAT(CPX,:,:), IN :: a
      BIN, optional, IN :: dagger_a
      BIN, optional, IN :: transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:to_product_with_diagonal_6 ... incompatible dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)  ! an
         ENSURE(size(a,1)==size(dg),"MAT{CPX}:to_product_with_diagonal_6 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*a(i,:)
         end do

      case (1)  ! ad
         ENSURE(size(a,2)==size(dg),"MAT{CPX}:to_product_with_diagonal_6 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*conjg(a(:,i))
         end do

      case (2)  ! at
         ENSURE(size(a,2)==size(dg),"MAT{CPX}:to_product_with_diagonal_6 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*a(:,i)
         end do
      end select
   end subroutine

   PURE subroutine to_product_with_diagonal_7(self,dg,a,dagger_a,transpose_a)
   ! Set "self" to the matrix product of diagonal matrix "dg" (stored as a
   ! vector) and "a".  If present, "dagger_a" or "transpose_a" can be set to
   ! TRUE if "a" needs to be daggered or transposed.
      MAT(CPX,:,:), OUT :: self
      VEC(REAL,:), IN :: dg
      MAT(CPX,:,:), IN :: a
      BIN, optional, IN :: dagger_a
      BIN, optional, IN :: transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i
   ENSURE(is_same_shape_as_(self,a),"MAT{CPX}:to_product_with_diagonal_7 ... incompatible dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)  ! an
         ENSURE(size(a,1)==size(dg),"MAT{CPX}:to_product_with_diagonal_7 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*a(i,:)
         end do

      case (1)  ! ad
         ENSURE(size(a,2)==size(dg),"MAT{CPX}:to_product_with_diagonal_7 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*conjg(a(:,i))
         end do

      case (2)  ! at
         ENSURE(size(a,2)==size(dg),"MAT{CPX}:to_product_with_diagonal_7 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*a(:,i)
         end do
      end select
   end subroutine


!  ================
!  Trace operations
!  ================

   pure function trace(self) result (res)
   ! Return the trace of self
      MAT(CPX,:,:), IN :: self
      CPX :: res
      INT :: i

      ! The following code is inherited from MAT{INTRINSIC}
      res = ZERO
      do i = 1,size(self,1)
         res = res+self(i,i)
      end do
   end function

   pure function trace_product_with_0(self,a,transpose_a,dagger_a) result (res)
   ! Return the trace of the product of "self" with matrix "a",
   ! and if "dagger_a" is present and TRUE, then dagger "a".
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), IN :: a
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: dagger_a
      CPX :: res
      INT :: i,j
      BIN :: dagg,trans

      ! The following code is inherited from MAT{INTRINSIC}
      dagg = FALSE
      trans = FALSE
      if (present(dagger_a)) dagg = dagger_a
      if (present(transpose_a)) trans = transpose_a
      if (dagg) then
         res = ZERO
         do j = 1,size(self,2)
            do i = 1,size(self,1)
               res = res+self(i,j)*conjg(a(i,j))
            end do
         end do
      else if (trans) then
         res = ZERO
         do j = 1,size(self,2)
            do i = 1,size(self,1)
               res = res+self(i,j)*a(i,j)
            end do
         end do
      else
         res = ZERO
         do j = 1,size(self,2)
            do i = 1,size(self,1)
               res = res+self(i,j)*a(j,i)
            end do
         end do
      end if
   end function

   pure function trace_product_with_1(self,a,transpose_a) result (res)
   ! Return the trace of the product of "self" with matrix "a",
   ! and if "transpose_a" is present and TRUE, then transpose "a".
      MAT(CPX,:,:), IN :: self
      MAT(REAL,:,:), IN :: a
      BIN, optional, IN :: transpose_a
      REAL :: res
      INT :: i,j
      BIN :: trans

      ! The following code is inherited from MAT{INTRINSIC}
      trans = FALSE
      if (present(transpose_a)) trans = transpose_a
      if (trans) then
         res = ZERO
         do j = 1,size(self,2)
            do i = 1,size(self,1)
               res = res+REALIFY(self(i,j))*a(i,j)
            end do
         end do
      else
         res = ZERO
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               res = res+REALIFY(self(i,j))*a(j,i)
            end do
         end do
      end if
   end function

   PURE function trace_product_with_2(self,a,b,c) result (res)
   ! Return the trace of the product of "self" with matrices "a", "b" and "c".
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), IN :: a,b,c
      CPX :: res
      MAT(CPX,:,:), allocatable :: W1,W2

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W2,size(a,1),size(c,2))
      call create_(W1,size(b,1),size(c,2))
      call to_product_of_(W1,b,c)
      call to_product_of_(W2,a,W1)
      call destroy_(W1)
      call create_(W1,size(self,1),size(c,2))
      call to_product_of_(W1,self,W2)
      res = trace_(W1)
      call destroy_(W1)
      call destroy_(W2)
   end function

   PURE function trace_product_with_3(self,a,b,c,d,e) result (res)
   ! Return the trace of the product of "self" with matrices "a", "b" ... "e".
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), IN :: a,b,c,d,e
      CPX :: res
      MAT(CPX,:,:), allocatable :: W1,W2

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W1,size(d,1),size(e,2))
      call to_product_of_(W1,d,e)  ! d*e
      call create_(W2,size(c,1),size(e,2))
      call to_product_of_(W2,c,W1)  ! c*d*e
      call destroy_(W1); call move_alloc(W2,W1)
      call create_(W2,size(b,1),size(e,2))
      call to_product_of_(W2,b,W1)  ! b*c*d*e
      call destroy_(W1); call move_alloc(W2,W1)
      call create_(W2,size(a,1),size(e,2))
      call to_product_of_(W2,a,W1)  ! a*b*c*d*e
      call destroy_(W1); call move_alloc(W2,W1)
      call create_(W2,size(self,1),size(e,2))
      call to_product_of_(W2,self,W1)  ! self*a*b*c*d*e
      res = trace_(W2)
      call destroy_(W2)
      call destroy_(W1)
   end function

   pure function dot_2(self,a) result (res)
   ! Synonym for trace_product_with(a) ... Return the trace of the product of
   ! "self" with matrix "a". This is really intended for use with symmetric
   ! matrices only.
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), IN :: a
      CPX :: res
      res = trace_product_with_(self,a,dagger_a=TRUE)
   end function


!  ========================
!  Change of basis routines
!  ========================

   PURE subroutine change_basis_using_0(self,V)
   ! Change the basis of "self" using vectors "V"; self = V^dagger self V
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: V
      MAT(CPX,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{CPX}:change_basis_using_0 ... Non-square self matrix")
   ENSURE(size(self,2)==size(V,1),"MAT{CPX}:change_basis_using_0 ... incompatible shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(V,2))
      call to_product_of_(W,self,V)
      call to_product_of_(self,V,W,DAGGER_A=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_using_1(self,V)
   ! Change the basis of "self" using vectors "V"; self = V^dagger self V
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: V
      MAT(CPX,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{CPX}:change_basis_using_1 ... Non-square self matrix")
   ENSURE(size(self,2)==size(V,1),"MAT{CPX}:change_basis_using_1 ... incompatible shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(V,2))
      call to_product_of_(W,self,V)
      call to_product_of_(self,V,W,TRANSPOSE_A=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_using_2(self,V)
   ! Change the basis of "self" using diagonal matrix "V" (stored as a vector).
   ! self = V self V
      MAT(CPX,:,:), INOUT :: self
      VEC(CPX,:), IN :: V
      MAT(CPX,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{CPX}:change_basis_using_2 ... non-square self matrix")
   ENSURE(size(self,1)==size(V),"MAT{CPX}:change_basis_using_2 ... incompatible shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(V),size(V))
      call to_product_with_diagonal_(W,self,V)
      call to_product_with_diagonal_(self,V,W)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_using_3(self,V)
   ! Change the basis of "self" using diagonal matrix "V" (stored as a vector).
   ! self = V self V
      MAT(CPX,:,:), INOUT :: self
      VEC(REAL,:), IN :: V
      MAT(CPX,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{CPX}:change_basis_using_3 ... non-square self matrix")
   ENSURE(size(self,1)==size(V),"MAT{CPX}:change_basis_using_3 ... incompatible shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(V),size(V))
      call to_product_with_diagonal_(W,self,V)
      call to_product_with_diagonal_(self,V,W)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_using_4(self,L,R)
   ! Change the basis of "self" using diagonal matrices "L" and "R" (stored as
   ! vectors).  self = L self R
      MAT(CPX,:,:), INOUT :: self
      VEC(CPX,:), IN :: L,R
      MAT(CPX,:,:), allocatable :: W
   ENSURE(size(self,1)==size(L) AND size(self,2)==size(R),"MAT{CPX}:change_basis_using_4 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(L),size(R))
      call to_product_with_diagonal_(W,self,R)
      call to_product_with_diagonal_(self,L,W)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_using_5(self,L,R)
   ! Change the basis of "self" using diagonal matrices "L" and "R" (stored as
   ! vectors).  self = L self R
      MAT(CPX,:,:), INOUT :: self
      VEC(REAL,:), IN :: L,R
      MAT(CPX,:,:), allocatable :: W
   ENSURE(size(self,1)==size(L) AND size(self,2)==size(R),"MAT{CPX}:change_basis_using_5 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(L),size(R))
      call to_product_with_diagonal_(W,self,R)
      call to_product_with_diagonal_(self,L,W)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_to_0(self,new,V)
   ! Change the basis of "self" using vectors "V", and place the result in
   ! "new".  new = V^dagger self V
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: new
      MAT(CPX,:,:), IN :: V
      MAT(CPX,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{CPX}:change_basis_to_0 ... Non-square self matrix")
   ENSURE(is_square_(new),"MAT{CPX}:change_basis_to_0 ... Non-square new matrix")
   ENSURE(size(V,1)==size(self,2),"MAT{CPX}:change_basis_to_0 ... wrong shapes")
   ENSURE(size(V,2)==size(new,2),"MAT{CPX}:change_basis_to_0 ... wrong shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(V,2))
      call to_product_of_(W,self,V)
      call to_product_of_(new,V,W,DAGGER_A=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_to_1(self,new,V)
   ! Change the basis of "self" using vectors "V", and place the result in
   ! "new".  new = V^dagger self V
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: new
      MAT(REAL,:,:), IN :: V
      MAT(CPX,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{CPX}:change_basis_to_1 ... Non-square self matrix")
   ENSURE(is_square_(new),"MAT{CPX}:change_basis_to_1 ... Non-square new matrix")
   ENSURE(size(V,1)==size(self,2),"MAT{CPX}:change_basis_to_1 ... wrong shapes")
   ENSURE(size(V,2)==size(new,2),"MAT{CPX}:change_basis_to_1 ... wrong shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(V,2))
      call to_product_of_(W,self,V)
      call to_product_of_(new,V,W,TRANSPOSE_A=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_to_2(self,new,L,R)
   ! Change the basis of "self" using left and right matrices "L" and "R"
   ! and place the result in "new", new = L^dagger self R
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: new
      MAT(CPX,:,:), IN :: L,R
      MAT(CPX,:,:), allocatable :: W
   ENSURE(size(self,2)==size(R,1),"MAT{CPX}:change_basis_to_2 ... incompatible sizes")
   ENSURE(size(self,1)==size(L,1),"MAT{CPX}:change_basis_to_2 ... incompatible sizes")
   ENSURE(size(new,2)==size(R,2),"MAT{CPX}:change_basis_to_2 ... incompatible sizes")
   ENSURE(size(new,1)==size(L,2),"MAT{CPX}:change_basis_to_2 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(R,2))
      call to_product_of_(W,self,R)
      call to_product_of_(new,L,W,DAGGER_A=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_to_3(self,new,L,R)
   ! Change the basis of "self" using left and right matrices "L" and "R"
   ! and place the result in "new", new = L^dagger self R
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: new
      MAT(REAL,:,:), IN :: L,R
      MAT(CPX,:,:), allocatable :: W
   ENSURE(size(self,2)==size(R,1),"MAT{CPX}:change_basis_to_3 ... incompatible sizes")
   ENSURE(size(self,1)==size(L,1),"MAT{CPX}:change_basis_to_3 ... incompatible sizes")
   ENSURE(size(new,2)==size(R,2),"MAT{CPX}:change_basis_to_3 ... incompatible sizes")
   ENSURE(size(new,1)==size(L,2),"MAT{CPX}:change_basis_to_3 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(R,2))
      call to_product_of_(W,self,R)
      call to_product_of_(new,L,W,TRANSPOSE_A=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine back_transform_using_0(self,V)
   ! Back transform "self" using vectors "V", and place the result in "self".
   ! self = V self V^dagger
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: V
      MAT(CPX,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{CPX}:back_transform_using_0 ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(V,1),size(self,2))
      call to_product_of_(W,V,self)
      call to_product_of_(self,W,V,DAGGER_B=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine back_transform_using_1(self,V)
   ! Back transform "self" using vectors "V", and place the result in "self".
   ! self = V self V^dagger
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: V
      MAT(CPX,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{CPX}:back_transform_using_1 ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(V,1),size(self,2))
      call to_product_of_(W,V,self)
      call to_product_of_(self,W,V,TRANSPOSE_B=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine back_transform_to_0(self,new,V)
   ! Back transform "self" using vectors "V", and place the result in "new".
   ! new = V self V^dagger
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: new
      MAT(CPX,:,:), IN :: V
      MAT(CPX,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{CPX}:back_transform_to_0 ... non-square matrix")
   ENSURE(is_square_(new),"MAT{CPX}:back_transform_to_0 ... non-square matrix")
   ENSURE(size(V,2)==size(self,1),"MAT{CPX}:back_transform_to_0 ... incompatible sizes")
   ENSURE(size(V,1)==size(new,1),"MAT{CPX}:back_transform_to_0 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(V,1),size(self,2))
      call to_product_of_(W,V,self)
      call to_product_of_(new,W,V,DAGGER_B=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine back_transform_to_1(self,new,V)
   ! Back transform "self" using vectors "V", and place the result in "new".
   ! new = V self V^dagger
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: new
      MAT(REAL,:,:), IN :: V
      MAT(CPX,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{CPX}:back_transform_to_1 ... non-square matrix")
   ENSURE(is_square_(new),"MAT{CPX}:back_transform_to_1 ... non-square matrix")
   ENSURE(size(V,2)==size(self,1),"MAT{CPX}:back_transform_to_1 ... incompatible sizes")
   ENSURE(size(V,1)==size(new,1),"MAT{CPX}:back_transform_to_1 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(V,1),size(self,2))
      call to_product_of_(W,V,self)
      call to_product_of_(new,W,V,TRANSPOSE_B=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine back_transform_to_2(self,new,L,R)
   ! Back transform "self" using left and right matrices "L" and "R"
   ! and place the result in "new", new = L self R^dagger
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: new
      MAT(CPX,:,:), IN :: L,R
      MAT(CPX,:,:), allocatable :: W
   ENSURE(size(self,2)==size(R,2),"MAT{CPX}:back_transform_to_2 ... incompatible sizes")
   ENSURE(size(self,1)==size(L,2),"MAT{CPX}:back_transform_to_2 ... incompatible sizes")
   ENSURE(size(new,2)==size(R,1),"MAT{CPX}:back_transform_to_2 ... incompatible sizes")
   ENSURE(size(new,1)==size(L,1),"MAT{CPX}:back_transform_to_2 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(R,1))
      call to_product_of_(W,self,R,DAGGER_B=TRUE)
      call to_product_of_(new,L,W)
      call destroy_(W)
   end subroutine

   PURE subroutine back_transform_to_3(self,new,L,R)
   ! Back transform "self" using left and right matrices "L" and "R"
   ! and place the result in "new", new = L self R^dagger
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: new
      MAT(REAL,:,:), IN :: L,R
      MAT(CPX,:,:), allocatable :: W
   ENSURE(size(self,2)==size(R,2),"MAT{CPX}:back_transform_to_3 ... incompatible sizes")
   ENSURE(size(self,1)==size(L,2),"MAT{CPX}:back_transform_to_3 ... incompatible sizes")
   ENSURE(size(new,2)==size(R,1),"MAT{CPX}:back_transform_to_3 ... incompatible sizes")
   ENSURE(size(new,1)==size(L,1),"MAT{CPX}:back_transform_to_3 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(R,1))
      call to_product_of_(W,self,R,TRANSPOSE_B=TRUE)
      call to_product_of_(new,L,W)
      call destroy_(W)
   end subroutine


!   similarity_transform(V) :: get_from(MAT{INTRINSIC}, V_TYPE?=>MAT{CPX})
!   ! Do a similarity transform of "self" using vectors "V": self = V self V^-1
!   end

!   similarity_transform(V) :: get_from(MAT{INTRINSIC}, V_TYPE?=>MAT{REAL})
!   ! Do a similarity transform of "self" using vectors "V": self = V self V^-1
!   end

!  ==========================
!  Operations on the diagonal
!  ==========================

   pure function diagonal(self) result (res)
   ! Return the diagonal of "self". 
      MAT(CPX,:,:), IN :: self
      VEC(CPX,:), allocatable :: res
      INT :: i,dim

      ! The following code is inherited from MAT{INTRINSIC}
      dim = min(size(self,1),size(self,2))
      allocate(res(dim))
      do i = 1,dim
         res(i) = self(i,i)
      end do
   end function

   PURE subroutine set_from_diagonal_0(self,d)
   ! Converts the diagonal vector "d" to matrix "self".
      MAT(CPX,:,:), OUT :: self
      VEC(CPX,:), IN :: d
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:set_from_diagonal_0 ... non-square matrix")
   ENSURE(size(d)==size(self,1),"MAT{CPX}:set_from_diagonal_0 ... incompatibale diagonal length")

      ! The following code is inherited from MAT{INTRINSIC}
      self = ZERO
      do i = 1,size(d)
         self(i,i) = d(i)
      end do
   end subroutine

   PURE subroutine set_from_diagonal_1(self,d)
   ! Converts the diagonal vector "d" to matrix "self".
      MAT(CPX,:,:), OUT :: self
      VEC(REAL,:), IN :: d
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:set_from_diagonal_1 ... non-square matrix")
   ENSURE(size(d)==size(self,1),"MAT{CPX}:set_from_diagonal_1 ... incompatibale diagonal length")

      ! The following code is inherited from MAT{INTRINSIC}
      self = ZERO
      do i = 1,size(d)
         self(i,i) = d(i)
      end do
   end subroutine

   PURE subroutine set_diagonal_to_0(self,d)
   ! Set the diagonal of "self" to vector "d"
      MAT(CPX,:,:), INOUT :: self
      VEC(CPX,:), IN :: d
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:set_diagonal_to_0 ... non-square matrix")
   ENSURE(size(d)==size(self,1),"MAT{CPX}:set_diagonal_to_0 ... incompatibale diagonal length")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(d)
         self(i,i) = d(i)
      end do
   end subroutine

   PURE subroutine set_diagonal_to_1(self,d)
   ! Set the diagonal of "self" to vector "d"
      MAT(CPX,:,:), INOUT :: self
      VEC(REAL,:), IN :: d
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:set_diagonal_to_1 ... non-square matrix")
   ENSURE(size(d)==size(self,1),"MAT{CPX}:set_diagonal_to_1 ... incompatibale diagonal length")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(d)
         self(i,i) = d(i)
      end do
   end subroutine

   PURE subroutine set_diagonal_to_2(self,val)
   ! Set the diagonal of "self" to "val"
      MAT(CPX,:,:), INOUT :: self
      CPX, IN :: val
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:set_diagonal_to_2 ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = val
      end do
   end subroutine

   PURE subroutine set_diagonal_to_3(self,val)
   ! Set the diagonal of "self" to "val"
      MAT(CPX,:,:), INOUT :: self
      REAL, IN :: val
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:set_diagonal_to_3 ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = val
      end do
   end subroutine

   PURE subroutine put_diagonal_to_0(self,d)
   ! Get the diagonal elements of "self" in vector "d"
      MAT(CPX,:,:), IN :: self
      VEC(CPX,:), OUT :: d
      INT :: i
   ENSURE(size(d)==min(size(self,1),size(self,2)),"MAT{CPX}:put_diagonal_to_0 ... d is incompatible")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(d)
         d(i) = (self(i,i))
      end do
   end subroutine

   PURE subroutine put_diagonal_to_1(self,d)
   ! Get the diagonal elements of "self" in vector "d"
      MAT(CPX,:,:), IN :: self
      VEC(REAL,:), OUT :: d
      INT :: i
   ENSURE(size(d)==min(size(self,1),size(self,2)),"MAT{CPX}:put_diagonal_to_1 ... d is incompatible")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(d)
         d(i) = REALIFY(self(i,i))
      end do
   end subroutine

   PURE subroutine increment_diagonal_by_0(self,val)
   ! Add "val" to the diagonal of "self"
      MAT(CPX,:,:), INOUT :: self
      CPX, IN :: val
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:increment_diagonal_by_0 ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = self(i,i)+val
      end do
   end subroutine

   PURE subroutine increment_diagonal_by_1(self,val)
   ! Add "val" to the diagonal of "self"
      MAT(CPX,:,:), INOUT :: self
      REAL, IN :: val
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:increment_diagonal_by_1 ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = self(i,i)+val
      end do
   end subroutine

   PURE subroutine scale_diagonal_by_0(self,fac)
   ! Weight the diagonal elements of "self" by "fac"
      MAT(CPX,:,:), INOUT :: self
      CPX, IN :: fac
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:scale_diagonal_by_0 ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = fac*self(i,i)
      end do
   end subroutine

   PURE subroutine scale_diagonal_by_1(self,fac)
   ! Weight the diagonal elements of "self" by "fac"
      MAT(CPX,:,:), INOUT :: self
      REAL, IN :: fac
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:scale_diagonal_by_1 ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = fac*self(i,i)
      end do
   end subroutine

   PURE subroutine zero_diagonal(self)
   ! Zero the diagonal elements of "self"
      MAT(CPX,:,:), INOUT :: self
      INT :: i
   ENSURE(is_square_(self),"MAT{CPX}:zero_diagonal ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = ZERO
      end do
   end subroutine

   PURE subroutine zero_off_diagonal(self)
   ! Zero the off diagonal elements of "self"
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{CPX}:zero_off_diagonal ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,size(self,1)
            if (i==j) cycle
            self(i,j) = ZERO
         end do
      end do
   end subroutine

   PURE function max_abs_diagonal_element(self) result (res)
   ! Get the maximum absolute value of the diagonal elements of the self matrix
      MAT(CPX,:,:), IN :: self
      REAL :: res
      INT :: dim,i
   ENSURE(min(size(self,1),size(self,2))>0,"MAT{CPX}:max_abs_diagonal_element ... cannot have zero sized dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = min(size(self,1),size(self,2))
      res = abs(self(1,1))
      do i = 2,dim
         res = max(real(abs(self(i,i)),kind=REAL_KIND),res)
      end do
   end function


!  ========================
!  Symmetrising and folding
!  ========================

   PURE subroutine symmetrize(self)
   ! Set self to half of itself plus half its transpose, i.e.
   ! self = 1/2 (self + self^T)
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
      CPX :: val
   ENSURE(is_square_(self),"MAT{CPX}:symmetrize ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,i-1
            val = HALF*(self(i,j)+self(j,i))
            self(i,j) = val
            self(j,i) = val
         end do
      end do
   end subroutine

   PURE subroutine antisymmetrize(self)
   ! Set self to half of itself minus half its transpose, i.e.
   ! self = 1/2 (self - self^T)
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
      CPX :: val
   ENSURE(is_square_(self),"MAT{CPX}:antisymmetrize ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,i
            val = HALF*(self(i,j)-self(j,i))
            self(i,j) = val
            self(j,i) = -val
         end do
      end do
   end subroutine

   PURE subroutine symmetric_fold(self)
   ! Add the upper triangle of "self" into the lower triangle
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{CPX}:symmetric_fold ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,i-1
            self(i,j) = self(i,j)+self(j,i)
         end do
      end do
   end subroutine

   PURE subroutine antisymmetric_fold(self)
   ! Subtract the upper triangle of "self" into the lower triangle
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{CPX}:antisymmetric_fold ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,i-1
            self(i,j) = self(i,j)-self(j,i)
         end do
      end do
   end subroutine

   PURE subroutine symmetric_reflect(self,set_upper,set_lower)
   ! Make the upper triangle of "self" the same as the lower triangle,
   ! or the reverse if "set_upper" is FALSE or "set_lower" is TRUE 
      MAT(CPX,:,:), INOUT :: self
      BIN, optional, IN :: set_upper
      BIN, optional, IN :: set_lower
      INT :: i,j
      BIN :: upper
   ENSURE(is_square_(self),"MAT{CPX}:symmetric_reflect ... non-square matrix")
   ENSURE(NOT (present(set_upper) AND present(set_lower)),"MAT{CPX}:symmetric_reflect ... both options present!")

      ! The following code is inherited from MAT{INTRINSIC}
      upper = TRUE
      if (present(set_upper)) upper = set_upper
      if (present(set_lower)) upper = NOT set_lower
      if (upper) then

         ! Set the upper triangle
         do i = 1,size(self,1)
            do j = 1,i-1
               self(j,i) = self(i,j)
            end do
         end do
      else

         ! Set the lower triangle
         do i = 1,size(self,1)
            do j = 1,i-1
               self(i,j) = self(j,i)
            end do
         end do
      end if
   end subroutine

   PURE subroutine antisymmetric_reflect(self)
   ! Make the upper triangle of "self" the negative of the lower triangle and
   ! make the diagonal zero.
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{CPX}:antisymmetric_reflect ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,i-1
            self(j,i) = -self(i,j)
         end do
      end do
      do i = 1,size(self,1)
         self(i,i) = ZERO
      end do
   end subroutine

   PURE subroutine symmetric_fold_to_triangle(self,tr)
   ! Add the upper triangle of "self" into the lower triangle and return
   ! the lower triangle "tr" as a vector across rows.
      MAT(CPX,:,:), IN :: self
      VEC(CPX,:), OUT :: tr
      INT :: i,j,ij
   ENSURE(is_square_(self),"MAT{CPX}:symmetric_fold_to_triangle ... non-square matrix")
   ENSURE(size(tr)>=tri_size_(self),"MAT{CPX}:symmetric_fold_to_triangle ... triangle array too small")

      ! The following code is inherited from MAT{INTRINSIC}
      ij = 0
      do i = 1,size(self,1)
         do j = 1,i
            ij = ij+1
            if (i==j) then
               tr(ij) = self(i,j)
            else
               tr(ij) = self(i,j)+self(j,i)
            end if
         end do
      end do
   end subroutine

   PURE subroutine hermitianize(self)
   ! Set self to half of itself plus half its dagger, i.e.
   ! self = 1/2 (self + self^dagger)
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
      CPX :: val
   ENSURE(is_square_(self),"MAT{CPX}:hermitianize ... non-square matrix")
      do i = 1,size(self,1)
         do j = 1,i
            val = HALF*(self(i,j)+conjg(self(j,i)))
            self(i,j) = val
            self(j,i) = conjg(val)
         end do
      end do
   end subroutine

   PURE subroutine antihermitianize(self)
   ! Set self to half of itself minus half its dagger, i.e.
   ! self = 1/2 (self - self^dagger)
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
      CPX :: val
   ENSURE(is_square_(self),"MAT{CPX}:antihermitianize ... non-square matrix")
      do i = 1,size(self,1)
         do j = 1,i
            val = HALF*(self(i,j)-conjg(self(j,i)))
            self(i,j) = val
            self(j,i) = -conjg(val)
         end do
      end do
   end subroutine

   PURE subroutine hermitian_fold(self)
   ! Add the hermitian conjugate of the upper half of the matrix into its lower
   ! half, and make the diagonal real.
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{CPX}:hermitian_fold ... non-square matrix")
      do i = 1,size(self,1)
         do j = 1,i-1
            self(i,j) = self(i,j)+conjg(self(j,i))
         end do
         self(i,i) = REALIFY(self(i,i))
      end do
   end subroutine

   PURE subroutine antihermitian_fold(self)
   ! Subtract the hermitian conjugate of the upper half of the matrix into its
   ! lower half, and make the diagonal complex
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{CPX}:antihermitian_fold ... non-square matrix")
      do i = 1,size(self,1)
         do j = 1,i
            self(i,j) = self(i,j)-conjg(self(j,i))
         end do
         self(i,i) = HALF*self(i,i)
      end do
   end subroutine

   PURE subroutine make_hermitian(self)
   ! Make the upper half of self hermitian with respect to the lower half
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{CPX}:make_hermitian ... non-square matrix")
      do i = 1,size(self,1)
         do j = 1,i-1
            self(j,i) = conjg(self(i,j))
         end do
      end do
      do i = 1,size(self,1)
         self(i,i) = REALIFY(self(i,i))
      end do
   end subroutine

   PURE subroutine hermitian_reflect(self)
   ! Make the upper half of self hermitian with respect
   ! to the lower half
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{CPX}:hermitian_reflect ... non-square matrix")
      do i = 1,size(self,1)
         do j = 1,i-1
            self(j,i) = conjg(self(i,j))
         end do
      end do
      do i = 1,size(self,1)
         self(i,i) = REALIFY(self(i,i))
      end do
   end subroutine

   PURE subroutine make_antihermitian(self)
   ! Make the upper half of self anti-hermitian with respect
   ! to the lower half
      MAT(CPX,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{CPX}:make_antihermitian ... non-square matrix")
      do i = 1,size(self,1)
         do j = 1,i-1
            self(j,i) = -conjg(self(i,j))
         end do
      end do
      do i = 1,size(self,1)
         self(i,i) = HALF*(self(i,i)-conjg(self(i,i)))
      end do
   end subroutine

   PURE subroutine antihermitian_reflect(self)
   ! Make the upper half of self anti-hermitian with respect
   ! to the lower half
      MAT(CPX,:,:), INOUT :: self
      INT :: dim1,i,j
   ENSURE(is_square_(self),"MAT{CPX}:antihermitian_reflect ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j = 1,i-1
            self(j,i) = -conjg(self(i,j))
         end do
      end do
      do i = 1,size(self,1)
         self(i,i) = HALF*(self(i,i)-conjg(self(i,i)))
      end do
   end subroutine


!  ======================================
!  Compression and uncompression routines
!  ======================================

   pure subroutine zip_mx_to(self,v)
   ! Converts matrix "self" to the vector "v", where vectors are
   ! stored in column order
      MAT(CPX,:,:), IN :: self
      VEC(CPX,:), OUT :: v
   ! ENSURE(v.dim==.dim1*.dim2,"vec is not large enough")
      INT :: i,j,ij

      ! The following code is inherited from MAT{INTRINSIC}
      ij = 0
      do i = 1,size(self,1)
         do j = 1,size(self,2)
            ij = ij+1
            v(ij) = self(j,i)
         end do
      end do
   end subroutine

   pure subroutine unzip_vector(self,v)
   ! Converts the vector "v" into a matrix "self".
      MAT(CPX,:,:), OUT :: self
      VEC(CPX,:), IN :: v
   ! ENSURE(v.dim==.dim1*.dim2,"matrix is wrong size")
      INT :: i,j,ij

      ! The following code is inherited from MAT{INTRINSIC}
      self = ZERO
      ij = 0
      do i = 1,size(self,1)
         do j = 1,size(self,2)
            ij = ij+1
            self(j,i) = v(ij)
         end do
      end do
   end subroutine

   pure subroutine zip_lower_triangles_to(self,sq)
   ! Compresses the hermitian matrix self to vector "sq". First comes the
   ! lower half of the real part, then lower half of the imaginary part.
      MAT(CPX,:,:), IN :: self
      VEC(REAL,:), OUT :: sq
      INT :: i,j,ij
      ij = 0

      ! Real part
      do i = 1,size(self,1)
         do j = 1,i
            sq(ij+j) = REALIFY(self(i,j))
         end do
         ij = ij+i
      end do

      ! Imaginary part
      do i = 1,size(self,1)
         do j = 1,i-1
            sq(ij+j) = aimag(self(i,j))
         end do
         ij = ij+i-1
      end do
   end subroutine

   PURE subroutine hermitian_unzip_triangles(self,sq)
   ! Uncompress the vector "sq" to a hermitian matrix assuming the lower half
   ! of the real part comes first, then the lower half of the imaginary part.
      MAT(CPX,:,:), OUT :: self
      VEC(REAL,:), IN :: sq
      INT :: i,j,ij
      REAL :: val
      ij = 0
      do i = 1,size(self,1)
         do j = 1,i
            val = sq(ij+j)
            self(i,j) = val
            self(j,i) = val
         end do
         ij = ij+i
      end do
      do i = 1,size(self,1)
         do j = 1,i-1
            val = sq(ij+j)
            self(i,j) = self(i,j)+IMAGIFY(val)
            self(j,i) = self(j,i)-IMAGIFY(val)
         end do
         ij = ij+i-1
      end do
   end subroutine

   PURE subroutine antihermitian_unzip_triangles(self,sq)
   ! Uncompress the vector "sq" to a hermitian matrix assuming the lower half
   ! of the real part comes first, then the lower half of the imaginary part.
      MAT(CPX,:,:), OUT :: self
      VEC(REAL,:), IN :: sq
      INT :: i,j,ij
      REAL :: val
      ij = 0
      do i = 1,size(self,1)
         do j = 1,i
            val = sq(ij+j)
            self(i,j) = val
            self(j,i) = -val
         end do
         ij = ij+i
      end do
      do i = 1,size(self,1)
         do j = 1,i-1
            val = sq(ij+j)
            self(i,j) = self(i,j)+IMAGIFY(val)
            self(j,i) = self(j,i)+IMAGIFY(val)
         end do
         ij = ij+i-1
      end do
   end subroutine

   pure subroutine zip_lower_triangle_to(self,tr)
   ! Converts the lower triangle of "self" to the vector "tr"
   ! using row order.
      MAT(CPX,:,:), IN :: self
      VEC(CPX,:), OUT :: tr
      INT :: i,j,ij

      ! The following code is inherited from MAT{INTRINSIC}
      ij = 0
      do i = 1,size(self,1)
         do j = 1,i
            ij = ij+1
            tr(ij) = self(i,j)
         end do
      end do
   end subroutine

   pure subroutine unzip_lower_triangle(self,tr)
   ! Converts the triangle "tr" into a lower triangular matrix "self".
      MAT(CPX,:,:), OUT :: self
      VEC(CPX,:), IN :: tr
      CPX :: tmp
      INT :: i,j,ij

      ! The following code is inherited from MAT{INTRINSIC}
      self = ZERO
      ij = 0
      do i = 1,size(self,1)
         do j = 1,i
            ij = ij+1
            tmp = tr(ij)
            self(i,j) = tmp
         end do
      end do
   end subroutine

   pure subroutine hermitian_unzip_triangle(self,tr)
   ! Converts the triangle "tr" into the hermitian matrix "self".
   ! WARNING: won't work for symmetric matrices
   ! WARNING: Note the missing "s" on triangle, to avoid name clash
      MAT(CPX,:,:), OUT :: self
      VEC(CPX,:), IN :: tr
      INT :: i,j,ij
      CPX :: val
      ij = 0
      do i = 1,size(self,1)
         do j = 1,i
            val = tr(ij+j)
            self(i,j) = val
            self(j,i) = conjg(val)
         end do
         ij = ij+i
      end do
   end subroutine

   pure function tri_size(self) result (ltr)
   ! Returns the size of the lower triangle needed to store self.
      MAT(CPX,:,:), IN :: self
      INT :: ltr
      INT :: dim

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      ltr = dim*(dim+1)/2
   end function


!  =================
!  Orthogonalisation
!  =================

   PURE subroutine Schmidt_orthonormalize_0(self,scale)
   ! Schmidt orthonormalize the column vectors in "self" metric. If
   ! "scale" is present, it is set to the product of the normalisation
   ! factors used to normalize each column after the Schmidt
   ! procedure.
      MAT(CPX,:,:), INOUT :: self
      REAL, optional, OUT :: scale
      CPX :: fac
      INT :: n,o
      REAL :: norm
   ENSURE(NOT is_zero_(self),"MAT{CPX}:Schmidt_orthonormalize_0 ... self is zero matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      if (present(scale)) scale = ONE
      do n = 1,size(self,2)
         do o = 1,n-1
            fac = dot_product(self(:,o),self(:,n))
            self(:,n) = self(:,n)-fac*self(:,o)
         end do
         norm = RE(dot_product(self(:,n),self(:,n)))
      ! ENSURE(norm>MAT_LINEAR_DEPENDENCE_TOL,"linear dependence, vector "//n.to_str)
         norm = ONE/sqrt(norm)
         self(:,n) = self(:,n)*norm
         if (present(scale)) scale = scale*norm
      end do
   end subroutine

   PURE subroutine Schmidt_orthonormalize_1(self,S,scale)
   ! Schmidt orthonormalize the column vectors in "self" using "S" as the
   ! metric. If "scale" is present, it is set to the product of the
   ! normalisation factors used to normalize each column after the Schmidt
   ! procedure.
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: S
      REAL, optional, OUT :: scale
      CPX :: fac
      INT :: n,o
      REAL :: norm
   ENSURE(is_square_(S),"MAT{CPX}:Schmidt_orthonormalize_1 ... S must be square matrix")
   ENSURE(size(S,1)==size(self,1),"MAT{CPX}:Schmidt_orthonormalize_1 ... incompatible metric S")
   ENSURE(NOT is_zero_(S),"MAT{CPX}:Schmidt_orthonormalize_1 ... S is zero matrix")
   ENSURE(NOT is_zero_(self),"MAT{CPX}:Schmidt_orthonormalize_1 ... self is zero matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      if (present(scale)) scale = ONE
      do n = 1,size(self,2)
         do o = 1,n-1
            fac = dot_(S,self(:,o),self(:,n))
            self(:,n) = self(:,n)-fac*self(:,o)
         end do
         norm = REALIFY(dot_(S,self(:,n),self(:,n)))
         ENSURE(norm>MAT_LINEAR_DEPENDENCE_TOL,"MAT{CPX}:Schmidt_orthonormalize_1 ... linear dependence, vector "//to_str_(n))
         norm = ONE/sqrt(norm)
         self(:,n) = self(:,n)*norm
         if (present(scale)) scale = scale*norm
      end do

      ! This is old code which is not so clean, kept cos it may be more efficient
   ! dim = .dim1
   ! T.create(dim)
   ! do n=1,dim
   !   do j=1,dim
   !     T(j) = dot_product(self(:j,n),S(:j,j))
   !     T(1:j-1) = T(1:j-1) + self(j,n) * S(:j-1,j)
   !   end
   !   do k=1,n-1
   !     proj = - dot_product(self(:,k),T)
   !     self(:,n) = self(:,n) + proj * self(:,k)
   !   end
   !   do j=1,dim
   !     T(j) = dot_product(self(:j,n), S(:j,j))
   !     T(:j-1) = T(:j-1) + self(j,n) * S(:j-1,j)
   !   end
   !   norm = dot_product(T,self(:,n))
   !   ENSURE(norm>TOL(10),"linear dependence in vector " // n.to_str)
   !   self(:,n) = self(:,n) / sqrt(norm)
   ! end
   ! T.destroy
   end subroutine

   PURE subroutine reverse_Schmidt_orthonormalize(self,S)
   ! Schmidt orthonormalize the column vectors in "self" using "S" as
   ! the metric.
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: S
      CPX :: fac
      INT :: n,o
      REAL :: norm
   ENSURE(is_square_(S),"MAT{CPX}:reverse_Schmidt_orthonormalize ... S must be square matrix")
   ENSURE(size(S,1)==size(self,1),"MAT{CPX}:reverse_Schmidt_orthonormalize ... incompatible metric S")
   ENSURE(NOT is_zero_(S),"MAT{CPX}:reverse_Schmidt_orthonormalize ... S is zero matrix")
   ENSURE(NOT is_zero_(self),"MAT{CPX}:reverse_Schmidt_orthonormalize ... self is zero matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do n = size(self,2),1,-1
         do o = size(self,2),n+1,-1
            fac = dot_(S,self(:,o),self(:,n))
            self(:,n) = self(:,n)-fac*self(:,o)
         end do
         norm = REALIFY(dot_(S,self(:,n),self(:,n)))
         ENSURE(norm>MAT_LINEAR_DEPENDENCE_TOL,"MAT{CPX}:reverse_Schmidt_orthonormalize ... linear dependence, vector "//to_str_(n))
         self(:,n) = self(:,n)/sqrt(norm)
      end do
   end subroutine

   PURE subroutine reverse_Schmidt_orthogonalise(self)
   ! Schmidt orthonormalize the column vectors in "self" using unit metric.
      MAT(CPX,:,:), INOUT :: self
      CPX :: fac
      REAL :: norm
      INT :: n,k
   ENSURE(size(self,1)>=size(self,2),"MAT{CPX}:reverse_Schmidt_orthogonalise ... more vectors than dimension of vector space")

      ! The following code is inherited from MAT{INTRINSIC}
      do n = size(self,2),1,-1
         do k = n-1,1,-1
            fac = dot_product(self(:,k),self(:,n))
            self(:,n) = self(:,n)-fac*self(:,k)
         end do
         norm = REALIFY(dot_product(self(:,n),self(:,n)))
         ENSURE(norm>TOL(10),"MAT{CPX}:reverse_Schmidt_orthogonalise ... linear dependence, vector "//to_str_(n))
         self(:,n) = self(:,n)/sqrt(norm)
      end do
   end subroutine

   PURE subroutine symmetrically_orthonormalize(self,S)
   ! Symmetrically orthonormalize the column vectors in "self" using
   ! "S" as the metric, which must be real and positive definite.
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: S
      MAT(CPX,:,:), allocatable :: SS,SI
   ENSURE(is_square_(S),"MAT{CPX}:symmetrically_orthonormalize ... S must be square matrix")
   ENSURE(size(S,1)==size(self,1),"MAT{CPX}:symmetrically_orthonormalize ... incompatible metric S")
   ENSURE(NOT is_zero_(self),"MAT{CPX}:symmetrically_orthonormalize ... self is zero matrix")
   ENSURE(NOT is_zero_(S),"MAT{CPX}:symmetrically_orthonormalize ... S is zero matrix")

      ! Get inverse sqrt "S" in self basis

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(SI,size(self,2),size(self,2))
      call create_(SS,size(self,2),size(self,2))
      call change_basis_to_(S,SS,self)
      call to_inverse_sqrt_of_(SI,SS)
      call destroy_(SS)

      ! Do symmetric orthonormalisation
      call create_(SS,size(self,1),size(self,2))
      call to_product_of_(SS,self,SI)
      self = SS

      ! Clean
      call destroy_(SS)
      call destroy_(SI)
   end subroutine

   PURE subroutine make_diagonally_dominant(self,permutation)
   ! Rearrange the order of the columns of self so that the largest magnitude
   ! elements in each column occur along the diagonal. If "permutation" is
   ! present, it is the array which achieves this ordering, i.e. at the end of
   ! the routine, what is done is: self = self(:,permutation).
      MAT(CPX,:,:), INOUT :: self
      VEC(INT,:), optional, OUT :: permutation
      VEC(INT,:), allocatable :: perm
      MAT(BIN,:,:), allocatable :: mask
      INT :: i,n
      VEC(INT,2) :: loc
   ENSURE(is_square_(self),"MAT{CPX}:make_diagonally_dominant ... not square")

      ! The following code is inherited from MAT{INTRINSIC}
      if (present(permutation)) then
         ENSURE(size(permutation)==size(self,2),"MAT{CPX}:make_diagonally_dominant ... wrong size, perm")
      end if
      n = size(self,2)
      call create_(perm,n)
      call create_(mask,n,n)
      mask = TRUE
      do i = 1,n
         loc = maxloc(abs(self),mask=mask)
         perm(loc(1)) = loc(2)
         mask(:,loc(2)) = FALSE  ! eliminate this column next time
         mask(loc(1),:) = FALSE  ! eliminate this row also
      end do
      self = self(:,perm)
      if (present(permutation)) permutation = perm
      call destroy_(mask)
      call destroy_(perm)
   end subroutine


!  =====================
!  Eigenproblem routines
!  =====================

   subroutine make_real_eigenproblem(self,eval,evec,eps)
   ! Diagonalises "self" using only real algebra:
   !
   !    (A+iB)*(u+iv)=l*(u+iv)
   !
   ! 1:  |A -B| * |u| = l * |u|
   ! i:  |B  A|   |v|       |v|
   !
   ! UNTESTED
      MAT(CPX,:,:), IN :: self
      VEC(REAL,:), OUT :: eval
      MAT(CPX,:,:), OUT :: evec
      REAL, optional, IN :: eps
      INT :: dim,i,j,l
      REAL :: accuracy,diff
      MAT(CPX,:,:), allocatable :: W
      MAT(REAL,:,:), allocatable :: W2,Wt,evec2
      VEC(REAL,:), allocatable :: eval2,temp2
      VEC(INT,:), allocatable :: ind
      CPX :: II
   ENSURE(is_hermitian_(self),"MAT{CPX}:make_real_eigenproblem ... self is not symmetrix")
   ENSURE(size(eval)==size(self,1),"MAT{CPX}:make_real_eigenproblem ... wrong size, eval")
   ENSURE(is_square_(evec),"MAT{CPX}:make_real_eigenproblem ... not square, evec")
      accuracy = TOL(12)
      if (present(eps)) accuracy = eps
      W = self
      dim = size(W,1)
      call create_(Wt,dim,dim)
      call create_(W2,2*dim,2*dim)
      Wt = real(W)
      W2(1:dim,1:dim) = Wt
      W2(dim+1:2*dim,dim+1:2*dim) = Wt
      Wt = aimag(W)
      W2(dim+1:2*dim,1:dim) = Wt
      W2(1:dim,dim+1:2*dim) = -Wt
      call destroy_(Wt)
      call destroy_(W)
      call create_(evec2,2*dim,2*dim)
      evec2 = ZERO
      call create_(eval2,2*dim)
      eval2 = ZERO
      call solve_symmetric_eigenproblem_(W2,eval2,evec2)
      call destroy_(W2)
      call create_(ind,2*dim)
      ind = 0
      call create_(temp2,2*dim)
      l = 1
      II = (ZERO,ONE)
      do i = 1,2*dim-1
         if (ind(i)==1) cycle

         ! Assign the eigenvalue & vector which has ind(i)/=0
         eval(l) = eval2(i)
         evec(:,l) = evec2(1:dim,i)+II*evec2(dim+1:2*dim,i)
         l = l+1
         ENSURE(l-1<dim+1,"MAT{CPX}:make_real_eigenproblem ... l greater than dim")
         do j = i+1,2*dim
            if (ind(j)==1) cycle

            ! Degnerate eigenvalue?
            if (abs(eval2(i)-eval2(j))<abs(eval2(i)-ONE)*TOL(4)) then
               diff = ZERO
               temp2(1:dim) = -evec2(dim+1:2*dim,j)
               temp2(dim+1:2*dim) = evec2(1:dim,j)
               diff = dot_product(evec2(:,i),temp2)-ONE
               if (abs(diff)<accuracy) then

                  ! Found the appropriate vector
                  ind(j) = 1
                  exit
               end if
            end if
         end do
      end do

      ! The real solver fixed the sign of evec2, but the complex combination
      ! u + iv formed above has a phase of its own. (This routine is marked
      ! UNTESTED above; the call is here for consistency with the others.)
      call canonicalise_eigenvector_phases_(evec)
      call destroy_(evec2)
      call destroy_(eval2)
      call destroy_(temp2)
      call destroy_(ind)
   end subroutine

   PURE subroutine diagonalize_Jacobi(self,eval,evec,eps,maxit)
   ! Diagonalises "self" using the Jacobi rotations method. Returns the
   ! eigenvalues "eval" and eigenvectors "evec". Input the maximum
   ! absolute off-diagonal element "eps" or the maximum number of
   ! iterations "maxit", if desired.
      MAT(CPX,:,:), IN :: self
      VEC(REAL,:), OUT :: eval
      MAT(CPX,:,:), OUT :: evec
      REAL, optional, IN :: eps
      INT, optional, IN :: maxit
      INT :: i,j,iter,maxiter
      MAT(CPX,:,:), allocatable :: W
      VEC(INT,:), allocatable :: ind
      REAL :: tol
      BIN :: done
   ENSURE(is_hermitian_(self),"MAT{CPX}:diagonalize_Jacobi ... self is not hermitian")
   ENSURE(size(eval)==size(self,1),"MAT{CPX}:diagonalize_Jacobi ... wrong size, eval")
   ENSURE(is_square_(evec),"MAT{CPX}:diagonalize_Jacobi ... evec is not square")
      tol = TOL(12)
      if (present(eps)) tol = eps
      maxiter = 750
      if (present(maxit)) maxiter = maxit
      W = self
      call to_unit_mx_(evec)
      iter = 0
      do
         iter = iter+1
         ENSURE(iter<=maxiter,"MAT{CPX}:diagonalize_Jacobi ... too many iterations")
         if (iter>maxiter) exit
         done = TRUE
         do i = 1,size(self,1)
            do j = 1,i-1
               if (abs(W(i,j))<tol AND iter>4) cycle
               done = FALSE
               call jacobi_rotation_(W,i,j,evec)
            end do
         end do
         done = done AND iter>3
         if (done) exit
      end do
      call put_diagonal_to_(W,eval)
      call destroy_(W)
      allocate(ind(size(self,1)))
      call quick_sort_(eval,ind)
      eval = eval(ind)
      evec = evec(:,ind)
      deallocate(ind)
   end subroutine

   pure subroutine jacobi_rotation(self,p,q,v)
   ! Do a two dimensional Jacobi rotation in the plane of "p" and "q" so as to
   ! eliminate the off diagonal element self(p,q) and also update the rotation
   ! matrix "v". NOTE: self must be symmetric
      MAT(CPX,:,:), INOUT :: self
      INT, IN :: p,q
      MAT(CPX,:,:), INOUT :: v
      INT :: i
      CPX :: s_pp,s_qq,s_pq,tau,s_ip,s_iq,r_ip,r_iq,ph,pc
      REAL :: a_pq,theta,t,c,s
      s_pp = self(p,p)
      s_qq = self(q,q)
      s_pq = self(p,q)
      a_pq = abs(s_pq)
      theta = REALIFY(s_qq-s_pp)/(TWO*a_pq)
      t = ONE/(abs(theta)+sqrt(theta*theta+ONE))
      if (theta<ZERO) t = -t
      c = ONE/sqrt(t*t+ONE)
      s = t*c
      tau = s/(ONE+c)
      ph = s_pq/a_pq
      pc = conjg(ph)
      do i = 1,size(self,1)
         s_ip = self(i,p)
         s_iq = self(i,q)
         r_ip = s_ip-s*(pc*s_iq+tau*s_ip)
         r_iq = s_iq+s*(ph*s_ip-tau*s_iq)
         self(i,p) = r_ip
         self(p,i) = r_ip
         self(i,q) = r_iq
         self(q,i) = r_iq
      end do
      self(p,p) = s_pp-t*pc*s_pq
      self(q,q) = s_qq+t*pc*s_pq
      self(p,q) = ZERO
      self(q,p) = ZERO
      do i = 1,size(self,1)
         s_ip = v(i,p)
         s_iq = v(i,q)
         v(i,p) = s_ip-s*(pc*s_iq+tau*s_ip)
         v(i,q) = s_iq+s*(ph*s_ip-tau*s_iq)
      end do
   end subroutine

   PURE subroutine canonicalise_eigenvector_phases(self,tol)
   ! Fix the arbitrary overall phase of every column of "self", which is taken
   ! to hold eigenvectors as columns. See VEC{CPX}:canonicalise_phase for the
   ! rule, and MAT{REAL}:canonicalise_eigenvector_signs for the restriction on
   ! where it may be used.
      MAT(CPX,:,:), INOUT :: self
      REAL, IN, optional :: tol
      VEC(CPX,:), allocatable :: v
      INT :: j
      call create_(v,size(self,1))
      do j = 1,size(self,2)
         v = self(:,j)
         call canonicalise_phase_(v,tol)
         self(:,j) = v
      end do
      call destroy_(v)
   end subroutine

   subroutine solve_hermitian_eigenproblem(self,eval,evec,routine)
   ! Solve the hermitian eigenvalue problem for self
      MAT(CPX,:,:), IN :: self
      VEC(REAL,:), OUT :: eval
      MAT(CPX,:,:), OUT :: evec
      STR(len=*), optional, IN :: routine
      call solve_H_eigenproblem_LAPACK_(self,eval,evec,routine)

      ! LAPACK does not fix the phase of an eigenvector; do it here so the
      ! same matrix gives the same output on every platform and library.
      call canonicalise_eigenvector_phases_(evec)
   end subroutine

   subroutine solve_H_eigenproblem_LAPACK(self,eval,evec,routine)
   ! Solve the hermitian eigenvalue problem for self. LAPACK version.
      MAT(CPX,:,:), IN :: self
      VEC(REAL,:), OUT :: eval
      MAT(CPX,:,:), OUT :: evec
      STR(len=*), optional, IN :: routine
      STR(len=STR_SIZE) :: rout
      VEC(CPX,:), allocatable :: ap,W
      VEC(REAL,:), allocatable :: RW
      INT :: dim,info
   ENSURE(is_square_(self),"MAT{CPX}:solve_H_eigenproblem_LAPACK ... non-square matrix")
   ENSURE(size(eval)>=size(self,1),"MAT{CPX}:solve_H_eigenproblem_LAPACK ... eval array too small")
   ENSURE(size(evec)>=size(self),"MAT{CPX}:solve_H_eigenproblem_LAPACK ... evec array too small")
      rout = "zhpev"
      if (present(routine)) rout = routine
      dim = size(self,1)
      select case (rout)

      case ("zheev")
         call create_(W,dim*dim)
         call create_(RW,3*dim)
         call set_to_(evec,self)
         info = 0
         call zheev("V","L",dim,evec,dim,eval,W,dim*dim,RW,info)
         call destroy_(RW)
         call destroy_(W)
         ENSURE(info==0,"MAT{CPX}:solve_H_eigenproblem_LAPACK ... no solution, error found, info="//trim(to_str_(info)))

      case ("zhpev")
         call create_(ap,dim*(dim+1)/2)
         call zip_lower_triangle_to_(self,ap)
         call create_(W,2*dim)
         call create_(RW,3*dim)
         call zhpev("V","U",dim,ap,eval,evec,dim,W,RW,info)
         call destroy_(RW)
         call destroy_(W)
         call destroy_(ap)
         ENSURE(info==0,"MAT{CPX}:solve_H_eigenproblem_LAPACK ... no solution, error found, info="//trim(to_str_(info)))
      end select
   end subroutine


!  ========
!  Cholesky
!  ========

   subroutine Cholesky_decomposition_0(self,tr)
   ! Decompose "self" into lower triangular matrices "L"
   !   self = L L^T or self = L L^\dagger
   ! where L is lower triangular and stored row-wise:
   ! i.e. (1,1), (2,1), (2,2), (3,1) ...
      MAT(CPX,:,:), INOUT :: self
      VEC(CPX,:), OUT :: tr
      MAT(CPX,:,:), allocatable :: L
   ENSURE(size(tr)==triangle_number_(size(self,1)),"MAT{CPX}:Cholesky_decomposition_0 ... wrong size, tr")
   ENSURE(is_square_(self),"MAT{CPX}:Cholesky_decomposition_0 ... self not square")

      ! Creates square

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(L,size(self,1),size(self,1))
      call Cholesky_decomposition_(self,L)

      ! Take the non-zero values
      call zip_lower_triangle_to_(L,tr)
      call destroy_(L)
   end subroutine

   subroutine Cholesky_decomposition_1(self,L)
   ! Decompose self into lower triangular matrices "L"
   !   self = L L^T or self = L L^\dagger
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: L
      INT :: i,j,dim
   ENSURE(is_square_(L),"MAT{CPX}:Cholesky_decomposition_1 ... L not square")
   ENSURE(is_same_shape_as_(self,L),"MAT{CPX}:Cholesky_decomposition_1 ... wrong size, L")
   ENSURE(all(diagonal_(L)/=ZERO),"MAT{CPX}:Cholesky_decomposition_1 ... L has zero diagonal elements!")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      L = ZERO

      ! Begin decomposition, with values being such that the matrix
      ! multiplication of it with its transpose returns the initial
      ! matrix. In the real case "conjg" is replaced with "1*".
      do i = 1,dim
         L(i,i) = sqrt(self(i,i)-sum(conjg(L(i,1:i-1))*L(i,1:i-1)))
         do j = i+1,dim
            L(j,i) = (self(i,j)-sum(conjg(L(i,1:i-1))*L(j,1:i-1)))/L(i,i)
         end do
      end do
      L = conjg(L)
   end subroutine


! ==========================================================
! Matrix functions: square roots, inverses, and exponentials
! ==========================================================

   subroutine to_sqrt_of(self,R)
   ! self = sqrt(R), cannot have R=self
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: R
      MAT(CPX,:,:), allocatable :: evec
      VEC(REAL,:), allocatable :: eval
      INT :: d,i,j,k
      REAL :: temp
      CPX :: val
   ENSURE(is_hermitian_(self),"MAT{CPX}:to_sqrt_of ... not hermitian")
      d = size(R,1)
      call create_(eval,d)
      call create_(evec,d,d)
      call solve_hermitian_eigenproblem_(R,eval,evec)
      do i = 1,d
         temp = eval(i)
         if (temp<=ZERO) then
            WARN("MAT{CPX}:to_sqrt_of ... non-positive eigenvalue, "//trim(to_str_(temp,"e15.8")))
         end if
         eval(i) = sqrt(abs(temp))
      end do
      do i = 1,d
         do j = 1,d
            val = IMAGIFY(ZERO)
            do k = 1,d
               val = val+evec(i,k)*eval(k)*conjg(evec(j,k))
            end do
            self(i,j) = val
         end do
      end do
      call destroy_(evec)
      call destroy_(eval)
   end subroutine

   PURE subroutine to_inverse_sqrt_of(self,R,tol)
   ! self = sqrt(R)^(-1), cannot have R=self
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: R
      REAL, optional, IN :: tol
      VEC(REAL,:), allocatable :: eval
      MAT(CPX,:,:), allocatable :: evec
      VEC(CPX,:), allocatable :: ev
      INT :: d,i,j,k
      REAL :: eps,temp
      CPX :: val
      eps = TOL(15)
      if (present(tol)) eps = tol
      d = size(R,1)
      call create_(ev,d)  ! WTF? Why is ev complex?
      call create_(eval,d)
      call create_(evec,d,d)
      call diagonalize_Jacobi_(R,eval,evec,eps)
      do i = 1,d
         temp = eval(i)
         if (abs(temp)<eps) then
            ev(i) = (ZERO,ZERO)
         else
            if (temp<ZERO) then
               ev(i) = (ZERO,ONE)/sqrt(-temp)
            else
               ev(i) = (ONE,ZERO)/sqrt(temp)
            end if
         end if
      end do
      do i = 1,d
         do j = 1,d
            val = IMAGIFY(ZERO)
            do k = 1,d
               val = val+evec(i,k)*ev(k)*conjg(evec(j,k))
            end do
            self(i,j) = val
         end do
      end do
      call destroy_(evec)
      call destroy_(eval)
      call destroy_(ev)
   end subroutine

   subroutine to_inverse_sqrt_of_2(self,R,tol)
   ! self = sqrt(R)^(-1), cannot have R=self
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:) :: R
      REAL, optional :: tol
      MAT(CPX,:,:), allocatable :: evec
      VEC(REAL,:), allocatable :: eval
      VEC(CPX,:), allocatable :: ev
      INT :: d,i,j,k
      REAL :: eps,temp
      CPX :: val
      eps = TOL(15)
      if (present(tol)) eps = tol
      d = size(R,1)
      call create_(ev,d)
      call create_(eval,d)
      call create_(evec,d,d)
      eval = ZERO
      evec = (ZERO,ZERO)

!     R.diagonalize_Jacobi(eval,evec,eps)
      call make_real_eigenproblem_(R,eval,evec)
      do i = 1,d
         temp = eval(i)
         if (abs(temp)<eps) then
            ev(i) = (ZERO,ZERO)
            WARN("MAT{CPX}:to_inverse_sqrt_of_2 ... linear dependence for the eigenvalue, "//trim(to_str_(temp,"e15.8")))
            WARN("MAT{CPX}:to_inverse_sqrt_of_2 ... the inverse made to ZERO")
         else
            if (temp<ZERO) then
               ev(i) = (ZERO,ONE)/sqrt(-temp)
               WARN("MAT{CPX}:to_inverse_sqrt_of_2 ... non-positive eigenvalue, "//trim(to_str_(temp,"e15.8")))
               WARN("MAT{CPX}:to_inverse_sqrt_of_2 ... non-positive definite mx")
            else; ev(i) = (ONE,ZERO)/sqrt(temp)
            end if
         end if
      end do
      do i = 1,d
         do j = 1,d
            val = IMAGIFY(ZERO)
            do k = 1,d
               val = val+evec(i,k)*ev(k)*conjg(evec(j,k))
            end do
            self(i,j) = val
         end do
      end do
      call destroy_(evec)
      call destroy_(eval)
      call destroy_(ev)
   end subroutine

   PURE subroutine to_power_series_inverse_of(self,S,tol,maxit)
   ! Set self to the power series inverse square root of "S".
   ! If "tol" is present, make sure that the maximum deviation from the exact
   ! answer is less than "tol" times the smallest element of "S". If "maxit"
   ! is present, use this as the maximum number of terms in the power series,
   ! before termination with an error.
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: S
      REAL, optional, IN :: tol
      INT, optional, IN :: maxit
      VEC(CPX,:), allocatable :: d
      VEC(INT,:), allocatable :: perm
      MAT(CPX,:,:), allocatable :: W,X
      INT :: maxiter,n,k
      REAL :: eps
   ENSURE(is_square_(S),"MAT{CPX}:to_power_series_inverse_of ... S not square")
   ENSURE(is_same_shape_as_(self,S),"MAT{CPX}:to_power_series_inverse_of ... wrong shape")

      ! The following code is inherited from MAT{INTRINSIC}
      eps = TOL(6)
      if (present(tol)) eps = tol
      maxiter = 100
      if (present(maxit)) maxiter = maxit
      n = size(S,1)
      call create_(perm,n)
      call create_(d,n)
      call create_(W,n,n)
      call create_(X,n,n)
      W = S
      call make_diagonally_dominant_(W,perm)
      call put_diagonal_to_(W,d)
      d = ONE/d
      call to_product_with_diagonal_(X,d,W)
      call to_unit_mx_(self)
      X = X-self
      W = X
      self = self-W
      k = 1
      do
         k = k+1
         ENSURE(k<=maxiter,"MAT{CPX}:to_power_series_inverse_of ... power series too long")
         W = matmul(X,W)
         if (is_odd_(k)) then; self = self-W
         else; self = self+W
         end if
         if (maxval(abs(W))<eps) exit
      end do
      call to_product_with_diagonal_(X,self,d)
      self = X
      self(perm,:) = self
      call destroy_(X)
      call destroy_(d)
      call destroy_(perm)
   end subroutine

   subroutine to_power_series_inv_sqrt_of(self,S,tol,prefactor,maxit)
   ! Set self to the inverse square root of "S", a matrix which is required to
   ! have a unit diagonal. The method uses a binomial power series expansion.
   ! If "tol" is present, make sure that the maximum deviation from the exact
   ! answer is less than "tol" times the smallest element of "S". If "maxit"
   ! is present, use this as the maximum number of terms in the power series,
   ! before termination with an error.
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), INOUT :: S
      REAL, optional, IN :: tol,prefactor
      INT, optional, IN :: maxit
      MAT(CPX,:,:), allocatable :: W,X
      INT :: maxiter,n,k
      REAL :: eps,fac,prefac
   ENSURE(has_unit_diagonal_(S),"MAT{CPX}:to_power_series_inv_sqrt_of ... must have unit diagonal")
   ENSURE(is_square_(S),"MAT{CPX}:to_power_series_inv_sqrt_of ... S not square")
   ENSURE(is_same_shape_as_(self,S),"MAT{CPX}:to_power_series_inv_sqrt_of ... wrong shape")

      ! The following code is inherited from MAT{INTRINSIC}
      eps = TOL(6)
      if (present(tol)) eps = tol
      maxiter = 100
      if (present(maxit)) maxiter = maxit
      prefac = ONE
      if (present(prefactor)) prefac = prefactor
      n = size(S,1)
      call create_(X,n,n)
      call create_(W,n,n)
      call to_unit_mx_(self)
      S = prefac*S
      S = S-self
      fac = -HALF
      X = fac*S
      W = X
      self = self+X
      k = 1
      do
         k = k+1
         fac = fac-ONE
         X = (fac/k)*matmul(S,W)
         W = X
         self = self+X
         if (maxval(abs(X))<eps) exit
         ENSURE(k<=maxiter,"MAT{CPX}:to_power_series_inv_sqrt_of ... power series too long")
      end do
      prefac = sqrt(prefac)
      self = prefac*self
      call to_unit_mx_(W)
      prefac = ONE/(prefac*prefac)
      S = prefac*(S+W)
      call destroy_(W)
      call destroy_(X)
   end subroutine

   subroutine to_exponential_of(self,X,tol)
   ! Exponentiate the matrix "X" using a power series expansion, self = exp(X),
      MAT(CPX,:,:), OUT :: self
      MAT(CPX,:,:), IN :: X
      REAL, optional, IN :: tol

      ! The following code is inherited from MAT{INTRINSIC}
      call exponentiate_to_(X,self,tol)
   end subroutine

   PURE subroutine exponentiate_to(self,U,tol)
   ! Exponentiate the matrix self using a power series expansion, U = exp(self),
   ! so that the maximum deviation from the exact answer is less than "tol"
   ! if present.
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: U
      REAL, optional, IN :: tol
      MAT(CPX,:,:), allocatable :: W,V
      INT :: n,k
      REAL :: eps,fac,error
   ENSURE(is_square_(U),"MAT{CPX}:exponentiate_to ... U not square")
   ENSURE(is_same_shape_as_(self,U),"MAT{CPX}:exponentiate_to ... wrong shape")

      ! The following code is inherited from MAT{INTRINSIC}
      eps = TOL(9)
      if (present(tol)) eps = tol
      n = size(U,1)
      call to_unit_mx_(U)
      call create_(W,n,n)
      call create_(V,n,n)
      W = self
      U = U+W
      k = 1
      do
         k = k+1
         fac = ONE/k
         call to_scaled_product_of_(V,self,W,fac)
         W = V
         U = U+W
         error = maxval(abs(W))
         if (error<eps) exit
      end do
      call destroy_(V)
      call destroy_(W)
   end subroutine

   subroutine antisymmetric_exponential_to(self,U,eval,evec)
   ! Make unitary matrix U = exp(self) where "self" must be antisymmetric.
   ! Uses the formula:  exp A = V (cos P) V^t + V (sin P)/P V^t A
   !                        P = sqrt diag(eig(A^t A))
   ! (c) dylan jayatilaka, university of western australia, 1993
   ! WARNING: Untested in TONTO and looks wrong.
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: U
      MAT(CPX,:,:), allocatable, optional, OUT :: evec
      VEC(REAL,:), allocatable, optional, OUT :: eval
      MAT(CPX,:,:), allocatable :: W,v_k
      REAL :: e,e2,cs,sn
      INT :: dim,k
   ENSURE(is_square_(self),"MAT{CPX}:antisymmetric_exponential_to ... self is a non-square matrix")
   ENSURE(is_same_shape_as_(self,U),"MAT{CPX}:antisymmetric_exponential_to ... incompatible shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      if (NOT present(eval)) then
         call create_(eval,dim)
      else
         ENSURE(size(eval)>=dim,"MAT{CPX}:antisymmetric_exponential_to ... eval too small")
      end if
      if (NOT present(evec)) then
         call create_(evec,dim,dim)
      else
         ENSURE(size(evec,1)==dim AND size(evec,2)==dim,"MAT{CPX}:antisymmetric_exponential_to ... evec incompatible")
      end if
      call create_(W,dim,dim)
      W = self

      ! U = -self^t*self = self^2, makes U hermitian
      U = matmul(self,W)

      ! diagonalise U ...
      call solve_hermitian_eigenproblem_(U,eval,evec)

      ! Exponentiate eigenvalues.
      U = ZERO
      do k = 1,dim
         e2 = eval(k)
         if (e2<0) then; e = sqrt(-e2); cs = cos(e); sn = sin(e)/e
         else if (e2>0) then; e = sqrt(e2); cs = cosh(e); sn = sinh(e)/e
         else; cs = ONE; sn = ONE
         end if
         v_k = evec(1:dim,k:k)
         call to_product_of_(W,v_k,v_k,transpose_b=TRUE)  ! V V^\dag part
         call plus_scaled_product_of_(U,W,self,sn)  ! sin part
         call plus_scaled_(U,W,cs)  ! cos part
      end do
      if (NOT present(eval)) call destroy_(eval)
      if (NOT present(evec)) call destroy_(evec)
   end subroutine


!  =============
!  String widths
!  =============

   pure function str_lengths(self,dp,spaces) result (res)
   ! Returns the minimal string lengths, with optional extra "spaces"
      MAT(CPX,:,:), IN :: self
      INT, IN :: dp
      INT, optional, IN :: spaces
      MAT(INT,size(self,1),size(self,2)) :: res
      INT :: i,j

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,size(self,2)
            res(i,j) = str_length_(self(i,j),dp)
         end do
      end do
      if (present(spaces)) res = res+spaces
   end function

   pure subroutine get_max_str_length_0(self,msl,spaces)
   ! Return "msl" the maximum of the string lengths, and including
   ! extra "spaces"
      MAT(CPX,:,:), IN :: self
      INT, OUT :: msl
      INT, IN :: spaces
      INT :: i,j,sl

      ! The following code is inherited from MAT{INTRINSIC}
      msl = 0
      do i = 1,size(self,1)
         do j = 1,size(self,2)
            call get_str_length_(self(i,j),sl,spaces)
            msl = max(msl,sl)
         end do
      end do
   end subroutine

   pure subroutine get_max_str_length_1(self,msl,max_dp,spaces)
   ! Return "msl" the maximum of the string lengths, keeping "max_dp"
   ! decimal places and including extra "spaces"
      MAT(CPX,:,:), IN :: self
      INT, OUT :: msl
      INT, IN :: max_dp
      INT, IN :: spaces
      INT :: i,j,sl

      ! The following code is inherited from MAT{INTRINSIC}
      msl = 0
      do i = 1,size(self,1)
         do j = 1,size(self,2)
            call get_str_length_maxdp_(self(i,j),sl,max_dp,spaces)
            msl = max(msl,sl)
         end do
      end do
   end subroutine


!  ===================
!  Spin-orbital blocks
!  ===================

!  Spin block returning routines

   PURE function aa_block(self) result (res)
   ! Return allocatable the alpha-alpha sector of the matrix
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), allocatable :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:aa_block ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:aa_block ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res = self(1:n,1:n)
   end function

   PURE function ba_block(self) result (res)
   ! Return allocatable the beta-alpha sector of the matrix
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), allocatable :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ba_block ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ba_block ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res = self(n+1:2*n,1:n)
   end function

   PURE function ab_block(self) result (res)
   ! Return the allocatable alpha-beta sector of the matrix
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), allocatable :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ab_block ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ab_block ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res = self(1:n,n+1:2*n)
   end function

   PURE function bb_block(self) result (res)
   ! Return the allocatable beta-beta sector of the matrix
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), allocatable :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:bb_block ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:bb_block ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res = self(n+1:2*n,n+1:2*n)
   end function


!  Spin block PTR returning routines

   function aa_block_ptr(self) result (res)
   ! Return the pointer alpha-alpha sector of the matrix
      MAT(CPX,:,:), target, IN :: self
      MAT(CPX,:,:), PTR :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:aa_block_ptr ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:aa_block_ptr ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res => self(1:n,1:n)
   end function

   function ba_block_ptr(self) result (res)
   ! Return the pointer beta-alpha sector of the matrix
      MAT(CPX,:,:), target, IN :: self
      MAT(CPX,:,:), PTR :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ba_block_ptr ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ba_block_ptr ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res => self(n+1:2*n,1:n)
   end function

   function ab_block_ptr(self) result (res)
   ! Return the pointer alpha-beta sector of the matrix
      MAT(CPX,:,:), target, IN :: self
      MAT(CPX,:,:), PTR :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ab_block_ptr ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ab_block_ptr ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res => self(1:n,n+1:2*n)
   end function

   function bb_block_ptr(self) result (res)
   ! Return the pointer beta-beta sector of the matrix
      MAT(CPX,:,:), target, IN :: self
      MAT(CPX,:,:), PTR :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:bb_block_ptr ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:bb_block_ptr ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res => self(n+1:2*n,n+1:2*n)
   end function


!  Spin block set routines

   PURE subroutine set_aa_block_to_0(self,X)
   ! Set the alpha-alpha sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_aa_block_to_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_aa_block_to_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = X
   end subroutine

   PURE subroutine set_ba_block_to_0(self,X)
   ! Set the beta-alpha sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_ba_block_to_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_ba_block_to_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = X
   end subroutine

   PURE subroutine set_ab_block_to_0(self,X)
   ! Set the alpha-beta sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_ab_block_to_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_ab_block_to_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = X
   end subroutine

   PURE subroutine set_bb_block_to_0(self,X)
   ! Set the beta-beta sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_bb_block_to_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_bb_block_to_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = X
   end subroutine

   PURE subroutine set_aa_block_to_1(self,X)
   ! Set the alpha-alpha sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_aa_block_to_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_aa_block_to_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = X
   end subroutine

   PURE subroutine set_ba_block_to_1(self,X)
   ! Set the beta-alpha sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_ba_block_to_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_ba_block_to_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = X
   end subroutine

   PURE subroutine set_ab_block_to_1(self,X)
   ! Set the alpha-beta sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_ab_block_to_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_ab_block_to_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = X
   end subroutine

   PURE subroutine set_bb_block_to_1(self,X)
   ! Set the beta-beta sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_bb_block_to_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_bb_block_to_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = X
   end subroutine

   PURE subroutine set_aa_block_to_2(self,X,fac)
   ! Set the alpha-alpha sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_aa_block_to_2 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_aa_block_to_2 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = fac*X
   end subroutine

   PURE subroutine set_ba_block_to_2(self,X,fac)
   ! Set the beta-alpha sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_ba_block_to_2 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_ba_block_to_2 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = fac*X
   end subroutine

   PURE subroutine set_ab_block_to_2(self,X,fac)
   ! Set the alpha-beta sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_ab_block_to_2 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_ab_block_to_2 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = fac*X
   end subroutine

   PURE subroutine set_bb_block_to_2(self,X,fac)
   ! Set the beta-beta sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_bb_block_to_2 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_bb_block_to_2 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = fac*X
   end subroutine

   PURE subroutine set_aa_block_to_3(self,X,fac)
   ! Set the alpha-alpha sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_aa_block_to_3 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_aa_block_to_3 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = fac*X
   end subroutine

   PURE subroutine set_ba_block_to_3(self,X,fac)
   ! Set the beta-alpha sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_ba_block_to_3 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_ba_block_to_3 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = fac*X
   end subroutine

   PURE subroutine set_ab_block_to_3(self,X,fac)
   ! Set the alpha-beta sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_ab_block_to_3 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_ab_block_to_3 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = fac*X
   end subroutine

   PURE subroutine set_bb_block_to_3(self,X,fac)
   ! Set the beta-beta sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_bb_block_to_3 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_bb_block_to_3 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = fac*X
   end subroutine

   PURE subroutine set_aa_block_to_4(self,X,fac)
   ! Set the alpha-alpha sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_aa_block_to_4 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_aa_block_to_4 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = fac*X
   end subroutine

   PURE subroutine set_ba_block_to_4(self,X,fac)
   ! Set the beta-alpha sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_ba_block_to_4 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_ba_block_to_4 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = fac*X
   end subroutine

   PURE subroutine set_ab_block_to_4(self,X,fac)
   ! Set the alpha-beta sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_ab_block_to_4 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_ab_block_to_4 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = fac*X
   end subroutine

   PURE subroutine set_bb_block_to_4(self,X,fac)
   ! Set the beta-beta sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_bb_block_to_4 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_bb_block_to_4 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = fac*X
   end subroutine

   PURE subroutine set_aa_block_to_5(self,X,fac)
   ! Set the alpha-alpha sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_aa_block_to_5 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_aa_block_to_5 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = fac*X
   end subroutine

   PURE subroutine set_ba_block_to_5(self,X,fac)
   ! Set the beta-alpha sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_ba_block_to_5 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_ba_block_to_5 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = fac*X
   end subroutine

   PURE subroutine set_ab_block_to_5(self,X,fac)
   ! Set the alpha-beta sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_ab_block_to_5 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_ab_block_to_5 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = fac*X
   end subroutine

   PURE subroutine set_bb_block_to_5(self,X,fac)
   ! Set the beta-beta sector of the matrix to "X"
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:set_bb_block_to_5 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:set_bb_block_to_5 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = fac*X
   end subroutine


!  Spin block put routines

   PURE subroutine put_aa_block_to_0(self,X)
   ! Put the alpha-alpha sector of the matrix to "X"
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:put_aa_block_to_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:put_aa_block_to_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      X = (self(1:n,1:n))
   end subroutine

   PURE subroutine put_ba_block_to_0(self,X)
   ! Put the beta-alpha sector of the matrix to "X"
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:put_ba_block_to_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:put_ba_block_to_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      X = (self(n+1:2*n,1:n))
   end subroutine

   PURE subroutine put_ab_block_to_0(self,X)
   ! Put the alpha-beta sector of the matrix to "X"
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:put_ab_block_to_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:put_ab_block_to_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      X = (self(1:n,n+1:2*n))
   end subroutine

   PURE subroutine put_bb_block_to_0(self,X)
   ! Put the beta-beta sector of the matrix to "X"
      MAT(CPX,:,:), IN :: self
      MAT(CPX,:,:), OUT :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:put_bb_block_to_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:put_bb_block_to_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      X = (self(n+1:2*n,n+1:2*n))
   end subroutine

   PURE subroutine put_aa_block_to_1(self,X)
   ! Put the alpha-alpha sector of the matrix to "X"
      MAT(CPX,:,:), IN :: self
      MAT(REAL,:,:), OUT :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:put_aa_block_to_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:put_aa_block_to_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      X = REALIFY(self(1:n,1:n))
   end subroutine

   PURE subroutine put_ba_block_to_1(self,X)
   ! Put the beta-alpha sector of the matrix to "X"
      MAT(CPX,:,:), IN :: self
      MAT(REAL,:,:), OUT :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:put_ba_block_to_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:put_ba_block_to_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      X = REALIFY(self(n+1:2*n,1:n))
   end subroutine

   PURE subroutine put_ab_block_to_1(self,X)
   ! Put the alpha-beta sector of the matrix to "X"
      MAT(CPX,:,:), IN :: self
      MAT(REAL,:,:), OUT :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:put_ab_block_to_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:put_ab_block_to_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      X = REALIFY(self(1:n,n+1:2*n))
   end subroutine

   PURE subroutine put_bb_block_to_1(self,X)
   ! Put the beta-beta sector of the matrix to "X"
      MAT(CPX,:,:), IN :: self
      MAT(REAL,:,:), OUT :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:put_bb_block_to_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:put_bb_block_to_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      X = REALIFY(self(n+1:2*n,n+1:2*n))
   end subroutine


!  Spin block plus routines

   PURE subroutine aa_block_plus_0(self,X)
   ! Add "X" to the alpha-alpha sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:aa_block_plus_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:aa_block_plus_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = self(1:n,1:n)+X
   end subroutine

   PURE subroutine ba_block_plus_0(self,X)
   ! Add "X" to the beta-alpha sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ba_block_plus_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ba_block_plus_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = self(n+1:2*n,1:n)+X
   end subroutine

   PURE subroutine ab_block_plus_0(self,X)
   ! Add "X" to the alpha-beta sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ab_block_plus_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ab_block_plus_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = self(1:n,n+1:2*n)+X
   end subroutine

   PURE subroutine bb_block_plus_0(self,X)
   ! Add "X" to the beta-beta sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:bb_block_plus_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:bb_block_plus_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n)+X
   end subroutine

   PURE subroutine aa_block_plus_1(self,X)
   ! Add "X" to the alpha-alpha sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:aa_block_plus_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:aa_block_plus_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = self(1:n,1:n)+X
   end subroutine

   PURE subroutine ba_block_plus_1(self,X)
   ! Add "X" to the beta-alpha sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ba_block_plus_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ba_block_plus_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = self(n+1:2*n,1:n)+X
   end subroutine

   PURE subroutine ab_block_plus_1(self,X)
   ! Add "X" to the alpha-beta sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ab_block_plus_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ab_block_plus_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = self(1:n,n+1:2*n)+X
   end subroutine

   PURE subroutine bb_block_plus_1(self,X)
   ! Add "X" to the beta-beta sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:bb_block_plus_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:bb_block_plus_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n)+X
   end subroutine

   PURE subroutine aa_block_plus_2(self,X,fac)
   ! Add "X" to the alpha-alpha sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:aa_block_plus_2 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:aa_block_plus_2 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = self(1:n,1:n)+fac*X
   end subroutine

   PURE subroutine ba_block_plus_2(self,X,fac)
   ! Add "X" to the beta-alpha sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ba_block_plus_2 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ba_block_plus_2 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = self(n+1:2*n,1:n)+fac*X
   end subroutine

   PURE subroutine ab_block_plus_2(self,X,fac)
   ! Add "X" to the alpha-beta sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ab_block_plus_2 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ab_block_plus_2 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = self(1:n,n+1:2*n)+fac*X
   end subroutine

   PURE subroutine bb_block_plus_2(self,X,fac)
   ! Add "X" to the beta-beta sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:bb_block_plus_2 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:bb_block_plus_2 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n)+fac*X
   end subroutine

   PURE subroutine aa_block_plus_3(self,X,fac)
   ! Add "X" to the alpha-alpha sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:aa_block_plus_3 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:aa_block_plus_3 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = self(1:n,1:n)+fac*X
   end subroutine

   PURE subroutine ba_block_plus_3(self,X,fac)
   ! Add "X" to the beta-alpha sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ba_block_plus_3 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ba_block_plus_3 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = self(n+1:2*n,1:n)+fac*X
   end subroutine

   PURE subroutine ab_block_plus_3(self,X,fac)
   ! Add "X" to the alpha-beta sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ab_block_plus_3 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ab_block_plus_3 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = self(1:n,n+1:2*n)+fac*X
   end subroutine

   PURE subroutine bb_block_plus_3(self,X,fac)
   ! Add "X" to the beta-beta sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(CPX,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:bb_block_plus_3 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:bb_block_plus_3 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n)+fac*X
   end subroutine

   PURE subroutine aa_block_plus_4(self,X,fac)
   ! Add "X" to the alpha-alpha sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:aa_block_plus_4 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:aa_block_plus_4 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = self(1:n,1:n)+fac*X
   end subroutine

   PURE subroutine ba_block_plus_4(self,X,fac)
   ! Add "X" to the beta-alpha sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ba_block_plus_4 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ba_block_plus_4 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = self(n+1:2*n,1:n)+fac*X
   end subroutine

   PURE subroutine ab_block_plus_4(self,X,fac)
   ! Add "X" to the alpha-beta sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ab_block_plus_4 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ab_block_plus_4 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = self(1:n,n+1:2*n)+fac*X
   end subroutine

   PURE subroutine bb_block_plus_4(self,X,fac)
   ! Add "X" to the beta-beta sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      CPX, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:bb_block_plus_4 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:bb_block_plus_4 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n)+fac*X
   end subroutine

   PURE subroutine aa_block_plus_5(self,X,fac)
   ! Add "X" to the alpha-alpha sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:aa_block_plus_5 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:aa_block_plus_5 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = self(1:n,1:n)+fac*X
   end subroutine

   PURE subroutine ba_block_plus_5(self,X,fac)
   ! Add "X" to the beta-alpha sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ba_block_plus_5 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ba_block_plus_5 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = self(n+1:2*n,1:n)+fac*X
   end subroutine

   PURE subroutine ab_block_plus_5(self,X,fac)
   ! Add "X" to the alpha-beta sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:ab_block_plus_5 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:ab_block_plus_5 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = self(1:n,n+1:2*n)+fac*X
   end subroutine

   PURE subroutine bb_block_plus_5(self,X,fac)
   ! Add "X" to the beta-beta sector of the matrix
      MAT(CPX,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{CPX}:bb_block_plus_5 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{CPX}:bb_block_plus_5 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n)+fac*X
   end subroutine

end module
