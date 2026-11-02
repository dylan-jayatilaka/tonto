!==================================================================
!
!  MAT{INT}: Integer matrix operations ...
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
!==================================================================
module MAT_INT_MODULE

#  include "macros"

   include "mat{int}.use"

   implicit none

   include "mat{int}.int"


#undef  ZERO
#define ZERO 0
#undef  ONE
#define ONE 1

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim1,dim2)
   ! Allocate a matrix with the given dimensions
      MAT(INT,:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{INTRINSIC}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim1,dim2),stat=status)
      ENSURE(status==0,"MAT{INT}:create_0 ... error in allocating memory")
#else
      allocate(self(dim1,dim2))
#endif
   end subroutine

   PURE subroutine create_1(self,lb1,ub1,lb2,ub2)
   ! Allocate a matrix with the given bounds
      MAT(INT,:,:), allocatable, OUT :: self
      INT, IN :: lb1,ub1
      INT, IN :: lb2,ub2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{INTRINSIC}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb1:ub1,lb2:ub2),stat=status)
      ENSURE(status==0,"MAT{INT}:create_1 ... error in allocating memory")
#else
      allocate(self(lb1:ub1,lb2:ub2))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      MAT(INT,:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT{INT}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ============================
!  Size-of and shape operations 
!  ============================

   pure function is_same_shape_as(self,a) result (res)
   ! Returns TRUE if the matrix "a" has the same shape as "self"
      MAT(INT,:,:), IN :: self
      MAT(INT,:,:), IN :: a
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(a,1) AND size(self,2)==size(a,2)
   end function

   pure function is_transposed_shape_of(self,a) result (res)
   ! Returns TRUE if the matrix "a" is the transposed shape of self
      MAT(INT,:,:), IN :: self
      MAT(INT,:,:), IN :: a
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(a,2) AND size(self,2)==size(a,1)
   end function

   pure function is_square(self) result (res)
   ! Returns TRUE if the matrix is square
      MAT(INT,:,:), IN :: self
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(self,2)
   end function


! =======================
! Shrinking and expansion
! =======================

   PURE subroutine shrink(self,dim1,dim2)
   ! Shrink allocatable self to have upper bounds dim1 x dim2.
      MAT(INT,:,:), allocatable, INOUT :: self
      INT, IN :: dim1,dim2
      MAT(INT,:,:), allocatable :: old
      INT :: lb1,lb2,ub1,ub2
   ENSURE(allocated(self),"MAT{INT}:shrink ... matrix not allocated")
   ENSURE(dim1<=ubound(self,1),"MAT{INT}:shrink ... 1st dimension given is too large.")
   ENSURE(dim2<=ubound(self,2),"MAT{INT}:shrink ... 2nd dimension given is too large.")

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
      MAT(INT,:,:), allocatable, INOUT :: self
      INT, IN :: dim2
      INT :: ub1,ub2
   ENSURE(allocated(self),"MAT{INT}:shrink_columns ... matrix not allocated")
   ENSURE(dim2<=ubound(self,2),"MAT{INT}:shrink_columns ... 2nd dimension given is too large.")

      ! The following code is inherited from MAT{INTRINSIC}
      ub1 = ubound(self,1)
      ub2 = ubound(self,2)
      if (dim2==ub2) return
      call shrink_(self,ub1,dim2)
   end subroutine

   PURE subroutine expand(self,dim1,dim2)
   ! Expand allocatable self to dim1 x dim2.
   ! New elements are initialised.
      MAT(INT,:,:), allocatable, INOUT :: self
      INT, IN :: dim1,dim2
      MAT(INT,:,:), allocatable :: new
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
         ENSURE(dim1>=ubound(self,1),"MAT{INT}:expand ... 1st dimension given is too small")
         ENSURE(dim2>=ubound(self,2),"MAT{INT}:expand ... 2nd dimension given is too small")

         ! Copy contents
         call create_(new,lb1,dim1,lb2,dim2)
         new = 0
         new(lb1:ub1,lb2:ub2) = self

         ! Transfer
         call move_alloc(new,self)
      end if
   end subroutine

   PURE subroutine expand_rows(self,dim1)
   ! Expands the rows of allocatable self to upper bound dim1.
   ! New elements are zeroed.
      MAT(INT,:,:), allocatable, INOUT :: self
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
      MAT(INT,:,:), allocatable, INOUT :: self
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
      MAT(INT,:,:), allocatable, INOUT :: self
      MAT(INT,:,:), IN :: cols
      INT :: ub2

      ! The following code is inherited from MAT{INTRINSIC}
      if (NOT allocated(self)) then
         self = cols
      else
         ENSURE(size(self,1)==size(cols,1),"MAT{INT}:append_columns ... 1st dimension wrong, cols")

         ! Expand
         ub2 = ubound(self,2)
         call expand_columns_(self,ub2+size(cols,2))

         ! Append
         self(:,ub2+1:) = cols(:,:)
      end if
   end subroutine

   PURE subroutine append_column(self,col)
   ! Append the column "col" onto end of allocatable self.
      MAT(INT,:,:), allocatable, INOUT :: self
      VEC(INT,:), IN :: col
      INT :: ub2

      ! The following code is inherited from MAT{INTRINSIC}
      if (NOT allocated(self)) then
         call create_(self,size(col),1)
         self(:,1) = col(:)
      else
         ENSURE(size(self,1)==size(col),"MAT{INT}:append_column ... 2nd dimension given is too small")

         ! Expand
         ub2 = ubound(self,2)
         call expand_columns_(self,ub2+1)

         ! Append
         self(:,ub2+1) = col
      end if
   end subroutine

   PURE subroutine prune_column(self,col)
   ! Prune the column with index "col".
      MAT(INT,:,:), allocatable, INOUT :: self
      INT, IN :: col
      INT :: lb2,ub2
   ENSURE(allocated(self),"MAT{INT}:prune_column ... self is not.associated")
   ENSURE(is_in_range_(col,[lbound(self,2),ubound(self,2)]),"MAT{INT}:prune_column ... col is out of range")

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
      MAT(INT,:,:), IN :: self
      MAT(INT,:,:), IN :: b
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = same_as_(self,b)
   end function

   PURE function same_as(self,b) result (res)
   ! Check if the matrix is the same as "b".
      MAT(INT,:,:), IN :: self
      MAT(INT,:,:), IN :: b
      BIN :: res
   ENSURE(is_same_shape_as_(self,b),"MAT{INT}:same_as ... incompatible dimensions")
      res = all(self==b)
   end function


!  ================
!  Range operations
!  ================

   pure function all_in_range(self,range) result (res)
   ! Return TRUE if all values of self are within the specified "range".
      MAT(INT,:,:), IN :: self
      VEC(INT,2), IN :: range
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = all(range(1)<=self AND self<=range(2))
   end function

   pure function in_range(self,range) result (res)
   ! Return element ij as TRUE if self(i,j) is within the specified "range".
      MAT(INT,:,:), IN :: self
      VEC(INT,2), IN :: range
      MAT(BIN,size(self,1),size(self,2)) :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = (range(1)<=self AND self<=range(2))
   end function

   pure function element_range(self) result (res)
   ! Return the range (smallest and largest value) of self.
      MAT(INT,:,:), IN :: self
      VEC(INT,2) :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res(1) = minval(self)
      res(2) = maxval(self)
   end function


!  =================
!  Inquiry functions
!  =================

   PURE function is_diagonal(self) result (res)
   ! Returns TRUE if the matrix "self" is a diagonal matrix
      MAT(INT,:,:), IN :: self
      BIN :: res
      INT :: dim,i,j
   ENSURE(is_square_(self),"MAT{INT}:is_diagonal ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      res = TRUE
      do i = 1,dim
         do j = 1,dim
            if (i==j) cycle
            if (is_zero_(self(i,j))) cycle
            res = FALSE
            return
         end do
      end do
   end function

   PURE function has_unit_diagonal(self) result (res)
   ! Returns TRUE if the matrix "self" has 1's as diagonal elements
      MAT(INT,:,:), IN :: self
      BIN :: res
      INT :: i
   ENSURE(is_square_(self),"MAT{INT}:has_unit_diagonal ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      res = TRUE
      do i = 1,size(self,1)
         if (is_one_(self(i,i))) cycle
         res = FALSE
         return
      end do
   end function

   PURE function has_minus_unit_diagonal(self) result (res)
   ! Returns TRUE if the matrix "self" has -1's as diagonal elements
      MAT(INT,:,:), IN :: self
      BIN :: res
      INT :: i
   ENSURE(is_square_(self),"MAT{INT}:has_minus_unit_diagonal ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      res = TRUE
      do i = 1,size(self,1)
         if (is_minus_one_(self(i,i))) cycle
         res = FALSE
         return
      end do
   end function

   PURE function is_unit_mx(self) result (res)
   ! Returns TRUE if the matrix "self" is the unit matrix
      MAT(INT,:,:), IN :: self
      BIN :: res
   ENSURE(is_square_(self),"MAT{INT}:is_unit_mx ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      res = has_unit_diagonal_(self) AND is_diagonal_(self)
   end function

   PURE function is_inversion_mx(self) result (res)
   ! Returns TRUE if the matrix "self" is an inversion matrix
   ! i.e. minus the unit matrix
      MAT(INT,:,:), IN :: self
      BIN :: res
   ENSURE(is_square_(self),"MAT{INT}:is_inversion_mx ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      res = has_minus_unit_diagonal_(self) AND is_diagonal_(self)
   end function

   PURE function is_symmetric(self) result (res)
   ! Returns TRUE if the matrix "self" is a symmetric matrix
      MAT(INT,:,:), IN :: self
      BIN :: res
      INT :: i,j
   ENSURE(is_square_(self),"MAT{INT}:is_symmetric ... Non-square matrix")

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
      MAT(INT,:,:), IN :: self
      BIN :: res
      INT :: dim,i,j
   ENSURE(is_square_(self),"MAT{INT}:is_antisymmetric ... Non-square matrix")

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

   pure function is_zero(self) result (res)
   ! Return TRUE if "self" is the zero matrix, i.e. every element is zero.
      MAT(INT,:,:), IN :: self
      BIN :: res
      res = all(self==0)
   end function


!  =================
!  Column operations 
!  =================

   PURE function has_column(self,c) result (res)
   ! Returns TRUE if the matrix "self" has a column "c". 
      MAT(INT,:,:), IN :: self
      VEC(INT,:), IN :: c
      BIN :: res
      INT :: n
   ENSURE(size(c)==size(self,1),"MAT{INT}:has_column ... incompatible column size")

      ! The following code is inherited from MAT{INTRINSIC}
      res = FALSE
      do n = 1,size(self,2)
         res = same_as_(self(:,n),c)
         if (res) exit
      end do
   end function

   PURE function index_for_column(self,c) result (res)
   ! Return the index of the first column in "self" which matches "c",
   ! or else 0 is returned if there was no match.
      MAT(INT,:,:), IN :: self
      VEC(INT,:), IN :: c
      INT :: res
      INT :: n
   ENSURE(size(c)==size(self,1),"MAT{INT}:index_for_column ... incompatible column size")

      ! The following code is inherited from MAT{INTRINSIC}
      res = 0
      do n = 1,size(self,2)
         if (same_as_(self(:,n),c)) then
            res = n
            exit
         end if
      end do
   end function

   PURE function indices_for_column(self,c) result (res)
   ! Return the indices of columns in "self" matching "c", 
   ! or else 0 is returned if there was no match.
      MAT(INT,:,:), IN :: self
      VEC(INT,:), IN :: c
      VEC(INT,:), allocatable :: res
      INT :: n
   ENSURE(size(c)==size(self,1),"MAT{INT}:indices_for_column ... incompatible column size")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(res,0)
      do n = 1,size(self,2)
         if (same_as_(self(:,n),c)) then
            call append_(res,n)
         end if
      end do
   end function

   PURE subroutine swap_columns_0(self,col1,col2)
   ! Swap columns "col1" and "col2" of self
      MAT(INT,:,:), INOUT :: self
      INT, IN :: col1,col2
      INT :: i
      INT :: val
   ENSURE(col1<=size(self,2) AND col2<=size(self,2),"MAT{INT}:swap_columns_0 ... columns exceed dimesions")

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
      MAT(INT,:,:), INOUT :: self
      VEC(INT,:), IN :: list
      INT :: l
   ENSURE(maxval(list)<=size(self,2),"MAT{INT}:swap_columns_1 ... list value exceed column dimension")

      ! The following code is inherited from MAT{INTRINSIC}
      do l = 1,size(list)
         call swap_columns_(self,l,list(l))
      end do
   end subroutine

   PURE subroutine reverse_column_order(self)
   ! Reverse the order of the columns of self.
      MAT(INT,:,:), INOUT :: self
      VEC(INT,:), allocatable :: tmp
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
      MAT(INT,:,:), IN :: self
      VEC(REAL,size(self,2)) :: res
      INT :: i

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,2)
         res(i) = norm_(self(:,i))
      end do
   end function

   PURE subroutine get_column_norms(self,res)
   ! Return the norms of every column
      MAT(INT,:,:), IN :: self
      VEC(REAL,:), OUT :: res
      INT :: i
   ENSURE(size(res)==size(self,2),"MAT{INT}:get_column_norms ... wrong size, res array")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,2)
         res(i) = norm_(self(:,i))
      end do
   end subroutine

   PURE subroutine get_column_dot_products(self,res)
   ! Return the dot products of every column with itself.
   ! Good for testing distances without using a sqrt.
      MAT(INT,:,:), IN :: self
      VEC(INT,:), OUT :: res
      INT :: i
   ENSURE(size(res)==size(self,2),"MAT{INT}:get_column_dot_products ... wrong size, res array")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,2)
         res(i) = dot_product(self(:,i),self(:,i))
      end do
   end subroutine

   pure function index_of_minimum_column_norm(self) result (res)
   ! Return the column index of the column with the *minimum* norm.
      MAT(INT,:,:), IN :: self
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
      MAT(INT,:,:), IN :: self
      VEC(REAL,size(self,1)) :: res
      VEC(REAL,size(self,1)) :: diff
      VEC(INT,size(self,1)) :: col_i,col_j
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

   pure function mean_column_vector(self) result (res)
   ! Return the mean of the column vectors.
      MAT(INT,:,:), IN :: self
      VEC(REAL,size(self,1)) :: res
      res = REALIFY(sum_column_vectors_(self))/size(self,2)
   end function

   pure function sum_column_vectors(self) result (res)
   ! Sum the column vectors (i.e. rows) in "self".
      MAT(INT,:,:), IN :: self
      VEC(INT,size(self,1)) :: res
      INT :: i,j
      INT :: val

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

   PURE function sum_row_vectors(self) result (res)
   ! Sum the row vectors (i.e. columns) in "self".
      MAT(INT,:,:), IN :: self
      VEC(INT,size(self,2)) :: res
      INT :: i,j
      INT :: val

      ! The following code is inherited from MAT{INTRINSIC}
      do j = 1,size(self,2)
         val = 0
         do i = 1,size(self,1)
            val = val+self(i,j)
         end do
         res(j) = val
      end do
   end function

   PURE function row_norms(self) result (res)
   ! Return the norms of every row
      MAT(INT,:,:), IN :: self
      VEC(REAL,size(self,1)) :: res
      INT :: i

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         res(i) = norm_(self(i,:))
      end do
   end function

   PURE subroutine swap_rows_0(self,row1,row2)
   ! Swap columns "row1" and "row2" of self
      MAT(INT,:,:), INOUT :: self
      INT, IN :: row1,row2
      INT :: i
      INT :: val
   ENSURE(row1<=size(self,1) AND row2<=size(self,1),"MAT{INT}:swap_rows_0 ... columns exceed dimesions")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,2)
         val = self(row1,i)
         self(row1,i) = self(row2,i)
         self(row2,i) = val
      end do
   end subroutine

   PURE subroutine swap_rows_1(self,list)
   ! Sequentially swap all rows in a row "list",
   ! self(i,:)       = self(list(i),:)
      MAT(INT,:,:), INOUT :: self
      VEC(INT,:), IN :: list
      INT :: l
   ENSURE(maxval(list)<=size(self,1),"MAT{INT}:swap_rows_1 ... list value exceed row dimension")

      ! The following code is inherited from MAT{INTRINSIC}
      do l = 1,size(list)
         call swap_rows_(self,l,list(l))
      end do
   end subroutine


!  ==========================================
!  Matrix algebra and vector space operations
!  ==========================================

   PURE recursive function determinant(self) result (res)
   ! Return the determinant
      MAT(INT,:,:), IN :: self
      INT :: res
      MAT(INT,:,:), allocatable :: adj
      INT :: c
   ENSURE(is_square_(self),"MAT{INT}:determinant ... non-square matrix")
   ENSURE(size(self,1)<4,"MAT{INT}:determinant ... only works for up to size 3 matrices")

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
      MAT(INT,:,:), IN :: self
      INT, IN :: i,j
      MAT(INT,:,:), allocatable :: res
   ENSURE(is_square_(self),"MAT{INT}:adjugate ... non-square matrix")
   ENSURE(size(self,1)>1,"MAT{INT}:adjugate ... dimension must be greater than 1")

      ! Create (leaky)

      ! The following code is inherited from MAT{INTRINSIC}
      allocate(res(size(self,1)-1,size(self,1)-1))

      ! Define
      res(1:i-1,1:j-1) = self(1:i-1,1:j-1)
      res(i:,1:j-1) = self(i+1:,1:j-1)
      res(1:i-1,j:) = self(1:i-1,j+1:)
      res(i:,j:) = self(i+1:,j+1:)
   end function

   PURE function dot(self,l,r) result (res)
   ! Multiply the matrix self by vector "l" on the left and vector "r" on the
   ! right ie:  res = l^dagger self r. Useful for non-unit metric dot_products.
      MAT(INT,:,:), IN :: self
      VEC(INT,:), IN :: l,r
      INT :: res
      VEC(INT,:), allocatable :: w
   ENSURE(size(self,1)==size(l),"MAT{INT}:dot ... wrong size, r")
   ENSURE(size(self,2)==size(r),"MAT{INT}:dot ... wrong size, r")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(w,size(l))
      call to_product_of_(w,self,r)
      res = dot_product(l,w)
      call destroy_(w)
   end function

   PURE subroutine rotate(self,v)
   ! Rotate vector "v" by self
      MAT(INT,:,:), IN :: self
      VEC(INT,:), INOUT :: v
      INT :: i,j
      VEC(INT,:), allocatable :: w
      INT :: val
   ENSURE(is_square_(self),"MAT{INT}:rotate ... incompatible arrays sizes")
   ENSURE(size(self,2)==size(v),"MAT{INT}:rotate ... incompatible arrays sizes")

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
      MAT(INT,:,:), OUT :: self
      INT :: i

      ! The following code is inherited from MAT{INTRINSIC}
      self = ZERO
      do i = 1,min(size(self,1),size(self,2))
         self(i,i) = ONE
      end do
   end subroutine

   PURE subroutine set_to(self,a)
   ! Set self to "a"
      MAT(INT,:,:), OUT :: self
      MAT(INT,:,:), IN :: a
   ENSURE(is_same_shape_as_(self,a),"MAT{INT}:set_to ... incompatible shape")

      ! The following code is inherited from MAT{INTRINSIC}
      self = a
   end subroutine

   PURE subroutine set_to_transpose_of(self,a)
   ! Self becomes the transpose of "a"
      MAT(INT,:,:), OUT :: self
      MAT(INT,:,:), IN :: a
      INT :: i,j,dim
   ENSURE(is_square_(self),"MAT{INT}:set_to_transpose_of ... non-square matrix")
   ENSURE(is_transposed_shape_of_(self,a),"MAT{INT}:set_to_transpose_of ... incompatible shapes")

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
      MAT(INT,:,:), INOUT :: self
      INT :: i,j,dim
      INT :: tmp
   ENSURE(is_square_(self),"MAT{INT}:to_transpose ... non-square matrix")

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

   PURE subroutine plus(self,a)
   ! Add to self the matrix "a"
      MAT(INT,:,:), INOUT :: self
      MAT(INT,:,:), IN :: a
   ENSURE(is_same_shape_as_(self,a),"MAT{INT}:plus ... incompatible shape")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self+a
   end subroutine

   PURE subroutine minus(self,a)
   ! Subtract from self the matrix "a"
      MAT(INT,:,:), INOUT :: self
      MAT(INT,:,:), IN :: a
   ENSURE(is_same_shape_as_(self,a),"MAT{INT}:minus ... incompatible shape")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self-a
   end subroutine

   PURE subroutine to_scaled(self,a,fac)
   ! Set "self" to matrix "at" scaled by "fac"
      MAT(INT,:,:), OUT :: self
      MAT(INT,:,:), IN :: a
      INT, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{INT}:to_scaled ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = fac*a
   end subroutine

   PURE subroutine plus_scaled(self,a,fac)
   ! Add to "self" matrix "a" scaled by "fac"
      MAT(INT,:,:), INOUT :: self
      MAT(INT,:,:), IN :: a
      INT, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{INT}:plus_scaled ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self+fac*a
   end subroutine

   PURE subroutine minus_scaled(self,a,fac)
   ! Subtract from "self" matrix "a" scaled by "fac"
      MAT(INT,:,:), INOUT :: self
      MAT(INT,:,:), IN :: a
      INT, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{INT}:minus_scaled ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self-fac*a
   end subroutine

   PURE subroutine to_product_of(self,a,b,transpose_a,transpose_b)
   ! Set "self" to the matrix product of "a" and "b". If present, "transpose_a"
   ! and "transpose_b" can be set to TRUE if "a" and "b" need to be transposed.
      MAT(INT,:,:), OUT :: self
      MAT(INT,:,:), IN :: a,b
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: transpose_b
      BIN :: trans_a,trans_b
      INT :: i,j,k,opt
      INT :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (trans_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{INT}:to_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{INT}:to_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{INT}:to_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{INT}:to_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{INT}:to_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{INT}:to_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,1),"MAT{INT}:to_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{INT}:to_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{INT}:to_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{INT}:to_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{INT}:to_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{INT}:to_product_of ... incompatible arrays")
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

   PURE subroutine plus_product_of(self,a,b,transpose_a,transpose_b)
   ! Add to "self" the matrix product of "a" and "b". If present, "transpose_a"
   ! and "transpose_b" can be set to TRUE if "a" and "b" need to be transposed.
      MAT(INT,:,:), INOUT :: self
      MAT(INT,:,:), IN :: a,b
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: transpose_b
      BIN :: trans_a,trans_b
      INT :: i,j,k,opt
      INT :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (trans_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{INT}:plus_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{INT}:plus_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{INT}:plus_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{INT}:plus_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{INT}:plus_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{INT}:plus_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,1),"MAT{INT}:plus_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{INT}:plus_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{INT}:plus_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{INT}:plus_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{INT}:plus_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{INT}:plus_product_of ... incompatible arrays")
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

   PURE subroutine to_scaled_product_of(self,a,b,fac,transpose_a,transpose_b)
   ! Set "self" to the matrix product of "a" and "b" scaled by "fac". If
   ! present, "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! need to be transposed.
      MAT(INT,:,:), OUT :: self
      MAT(INT,:,:), IN :: a,b
      INT, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: transpose_b
      BIN :: trans_a,trans_b
      INT :: i,j,k,opt
      INT :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (trans_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{INT}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{INT}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{INT}:to_scaled_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{INT}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{INT}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{INT}:to_scaled_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,1),"MAT{INT}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{INT}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{INT}:to_scaled_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{INT}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{INT}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{INT}:to_scaled_product_of ... incompatible arrays")
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

   PURE subroutine plus_scaled_product_of(self,a,b,fac,transpose_a,transpose_b)
   ! Add to "self" the matrix product of "a" and "b" scaled by "fac". If
   ! present, "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! neeb to be transposed.
      MAT(INT,:,:), INOUT :: self
      MAT(INT,:,:), IN :: a,b
      INT, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: transpose_b
      BIN :: trans_a,trans_b
      INT :: i,j,k,opt
      INT :: val

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE; trans_b = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (trans_a) opt = opt+1
      if (trans_b) opt = opt+2
      select case (opt)

      case (0)  ! .to_product_an_bn
         ENSURE(size(self,1)>=size(a,1),"MAT{INT}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{INT}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{INT}:plus_scaled_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{INT}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{INT}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{INT}:plus_scaled_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,1),"MAT{INT}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{INT}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{INT}:plus_scaled_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{INT}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{INT}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{INT}:plus_scaled_product_of ... incompatible arrays")
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

   PURE subroutine to_product_with_diagonal_0(self,a,diag,transpose_a)
   ! Set "self" to the matrix product of "a" with diagonal matrix "diag" (stored
   ! as a vector).  If present, ""transpose_a" can be set to TRUE if "a" needs
   ! to be transposed.
      MAT(INT,:,:), INOUT :: self
      MAT(INT,:,:), IN :: a
      VEC(INT,:), IN :: diag
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      INT :: opt,j
   ENSURE(is_same_shape_as_(self,a),"MAT{INT}:to_product_with_diagonal_0 ... incompatible dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE;
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (trans_a) opt = 1
      select case (opt)

      case (0)
         ENSURE(size(a,2)==size(diag),"MAT{INT}:to_product_with_diagonal_0 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = a(:,j)*diag(j)
         end do

      case (1)
         ENSURE(size(a,1)==size(diag),"MAT{INT}:to_product_with_diagonal_0 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = a(j,:)*diag(j)
         end do
      end select
   end subroutine

   PURE subroutine to_product_with_diagonal_1(self,dg,a,transpose_a)
   ! Set "self" to the matrix product of diagonal matrix "dg" (stored as a
   ! vector) and "a".  If present, "transpose_a" can be set to TRUE if "a" needs
   ! to be transposed.
      MAT(INT,:,:), OUT :: self
      VEC(INT,:), IN :: dg
      MAT(INT,:,:), IN :: a
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      INT :: opt,i
   ENSURE(is_same_shape_as_(self,a),"MAT{INT}:to_product_with_diagonal_1 ... incompatible dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (trans_a) opt = 1
      select case (opt)

      case (0)  ! an
         ENSURE(size(a,1)==size(dg),"MAT{INT}:to_product_with_diagonal_1 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*a(i,:)
         end do

      case (1)  ! at
         ENSURE(size(a,2)==size(dg),"MAT{INT}:to_product_with_diagonal_1 ... incompatible dimensions")
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
      MAT(INT,:,:), IN :: self
      INT :: res
      INT :: i

      ! The following code is inherited from MAT{INTRINSIC}
      res = ZERO
      do i = 1,size(self,1)
         res = res+self(i,i)
      end do
   end function

   pure function trace_product_with_0(self,a,transpose_a) result (res)
   ! Return the trace of the product of "self" with matrix "a",
   ! and if "transpose_a" is present and TRUE, then transpose "a".
      MAT(INT,:,:), IN :: self
      MAT(INT,:,:), IN :: a
      BIN, optional, IN :: transpose_a
      INT :: res
      INT :: i,j
      BIN :: trans

      ! The following code is inherited from MAT{INTRINSIC}
      trans = FALSE
      if (present(transpose_a)) trans = transpose_a
      if (trans) then
         res = ZERO
         do j = 1,size(self,2)
            do i = 1,size(self,1)
               res = res+(self(i,j))*a(i,j)
            end do
         end do
      else
         res = ZERO
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               res = res+(self(i,j))*a(j,i)
            end do
         end do
      end if
   end function

   PURE function trace_product_with_1(self,a,b,c) result (res)
   ! Return the trace of the product of "self" with matrices "a", "b" and "c".
      MAT(INT,:,:), IN :: self
      MAT(INT,:,:), IN :: a,b,c
      INT :: res
      MAT(INT,:,:), allocatable :: W1,W2

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

   PURE function trace_product_with_2(self,a,b,c,d,e) result (res)
   ! Return the trace of the product of "self" with matrices "a", "b" ... "e".
      MAT(INT,:,:), IN :: self
      MAT(INT,:,:), IN :: a,b,c,d,e
      INT :: res
      MAT(INT,:,:), allocatable :: W1,W2

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


!  ==========================
!  Operations on the diagonal
!  ==========================

   PURE subroutine set_from_diagonal(self,d)
   ! Converts the diagonal vector "d" to matrix "self".
      MAT(INT,:,:), OUT :: self
      VEC(INT,:), IN :: d
      INT :: i
   ENSURE(is_square_(self),"MAT{INT}:set_from_diagonal ... non-square matrix")
   ENSURE(size(d)==size(self,1),"MAT{INT}:set_from_diagonal ... incompatibale diagonal length")

      ! The following code is inherited from MAT{INTRINSIC}
      self = ZERO
      do i = 1,size(d)
         self(i,i) = d(i)
      end do
   end subroutine

   PURE subroutine set_diagonal_to_0(self,d)
   ! Set the diagonal of "self" to vector "d"
      MAT(INT,:,:), INOUT :: self
      VEC(INT,:), IN :: d
      INT :: i
   ENSURE(is_square_(self),"MAT{INT}:set_diagonal_to_0 ... non-square matrix")
   ENSURE(size(d)==size(self,1),"MAT{INT}:set_diagonal_to_0 ... incompatibale diagonal length")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(d)
         self(i,i) = d(i)
      end do
   end subroutine

   PURE subroutine set_diagonal_to_1(self,val)
   ! Set the diagonal of "self" to "val"
      MAT(INT,:,:), INOUT :: self
      INT, IN :: val
      INT :: i
   ENSURE(is_square_(self),"MAT{INT}:set_diagonal_to_1 ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = val
      end do
   end subroutine

   PURE subroutine put_diagonal_to(self,d)
   ! Get the diagonal elements of "self" in vector "d"
      MAT(INT,:,:), IN :: self
      VEC(INT,:), OUT :: d
      INT :: i
   ENSURE(size(d)==min(size(self,1),size(self,2)),"MAT{INT}:put_diagonal_to ... d is incompatible")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(d)
         d(i) = (self(i,i))
      end do
   end subroutine

   PURE subroutine increment_diagonal_by(self,val)
   ! Add "val" to the diagonal of "self"
      MAT(INT,:,:), INOUT :: self
      INT, IN :: val
      INT :: i
   ENSURE(is_square_(self),"MAT{INT}:increment_diagonal_by ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = self(i,i)+val
      end do
   end subroutine

   PURE subroutine scale_diagonal_by(self,fac)
   ! Weight the diagonal elements of "self" by "fac"
      MAT(INT,:,:), INOUT :: self
      INT, IN :: fac
      INT :: i
   ENSURE(is_square_(self),"MAT{INT}:scale_diagonal_by ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = fac*self(i,i)
      end do
   end subroutine

   PURE subroutine zero_diagonal(self)
   ! Zero the diagonal elements of "self"
      MAT(INT,:,:), INOUT :: self
      INT :: i
   ENSURE(is_square_(self),"MAT{INT}:zero_diagonal ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = ZERO
      end do
   end subroutine

   PURE subroutine zero_off_diagonal(self)
   ! Zero the off diagonal elements of "self"
      MAT(INT,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{INT}:zero_off_diagonal ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,size(self,1)
            if (i==j) cycle
            self(i,j) = ZERO
         end do
      end do
   end subroutine

   PURE function max_diagonal_element(self) result (res)
   ! Get the maximum element on the diagonal of the matrix
      MAT(INT,:,:), IN :: self
      INT :: res
      INT :: dim,i
   ENSURE(min(size(self,1),size(self,2))>0,"MAT{INT}:max_diagonal_element ... cannot have zero sized dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = min(size(self,1),size(self,2))
      res = self(1,1)
      do i = 2,dim
         res = max(self(i,i),res)
      end do
   end function

   PURE function max_abs_diagonal_element(self) result (res)
   ! Get the maximum absolute value of the diagonal elements of the self matrix
      MAT(INT,:,:), IN :: self
      REAL :: res
      INT :: dim,i
   ENSURE(min(size(self,1),size(self,2))>0,"MAT{INT}:max_abs_diagonal_element ... cannot have zero sized dimensions")

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

!   symmetrize :: get_from(MAT{INTRINSIC})
!   ! Set self to half of itself plus half its transpose, i.e.
!   ! self = 1/2 (self + self^T)
!   end
!
!   antisymmetrize :: get_from(MAT{INTRINSIC})
!   ! Set self to half of itself minus half its transpose, i.e.
!   ! self = 1/2 (self - self^T)
!   end
!
!   symmetric_fold :: get_from(MAT{INTRINSIC})
!   ! Add the upper triangle of "self" into the lower triangle
!   end
!
!   antisymmetric_fold :: get_from(MAT{INTRINSIC})
!   ! Subtract the upper triangle of "self" into the lower triangle
!   end
!
!   symmetric_reflect :: get_from(MAT{INTRINSIC})
!   ! Make the upper triangle of "self" the same as the lower triangle
!   end
!
!   antisymmetric_reflect :: get_from(MAT{INTRINSIC})
!   ! Make the upper triangle of "self" the negative of the lower triangle and
!   ! make the diagonal zero.
!   end
!
!   symmetric_fold_to_triangle(tr) :: get_from(MAT{INTRINSIC})
!   ! Add the upper triangle of "self" into the lower triangle and return
!   ! the lower triangle "tr" as a vector across rows.
!   end

!  ======================================
!  Compression and uncompression routines
!  ======================================

   pure subroutine zip_lower_triangle_to(self,tr)
   ! Converts the lower triangle of "self" to the vector "tr"
   ! using row order.
      MAT(INT,:,:), IN :: self
      VEC(INT,:), OUT :: tr
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

   pure subroutine symmetric_unzip_triangle(self,tr)
   ! Converts the triangle "tr" into the symmetric matrix "self".
      MAT(INT,:,:), OUT :: self
      VEC(INT,:), IN :: tr
      INT :: tmp
      INT :: i,j,ij

      ! The following code is inherited from MAT{INTRINSIC}
      ij = 0
      do i = 1,size(self,1)
         do j = 1,i
            ij = ij+1
            tmp = tr(ij)
            self(j,i) = tmp
            self(i,j) = tmp
         end do
      end do
   end subroutine

   pure function tri_size(self) result (ltr)
   ! Returns the size of the lower triangle needed to store self.
      MAT(INT,:,:), IN :: self
      INT :: ltr
      INT :: dim

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      ltr = dim*(dim+1)/2
   end function


!  =============
!  String widths
!  =============

   pure function str_lengths(self,spaces) result (res)
   ! Returns the minimal string lengths, with optional extra "spaces"
      MAT(INT,:,:), IN :: self
      INT, IN :: spaces
      MAT(INT,size(self,1),size(self,2)) :: res
      INT :: i,j

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,size(self,2)
            call get_str_length_(self(i,j),res(i,j),spaces)
         end do
      end do
   end function

   pure subroutine get_max_str_length(self,msl,spaces)
   ! Return "msl" the maximum of the string lengths, and including
   ! extra "spaces"
      MAT(INT,:,:), IN :: self
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


!  ======================
!  Binning and histograms
!  ======================

   PURE subroutine to_histogram_from_data(self,X,Y,X_min,X_max,X_bin,Y_min,Y_max,Y_bin)
   ! Set "self" to a histogram matrix whose ij-th element contains a 
   ! *count* of the values in array "data" whose elements correspond to
   ! the ij-th bin of "X" and "Y", where a 2D bin is a range of values
   ! in "X" (and "Y) of side length "X_bin" (and "Y_bin") beginning at
   ! "X_min" (and "Y_min") and ending at "X_max" (or "Y_max"). The
   ! dimension of self is calculated here.
   ! NOTE: Don't use binned data to calculate properties unless you
   ! really need to coarse grain.
      MAT(INT,:,:), allocatable, OUT :: self
      VEC(REAL,:), IN :: X,Y
      REAL, IN :: X_min,X_max,X_bin
      REAL, IN :: Y_min,Y_max,Y_bin
      INT :: n_X,n_Y,i,j,k
      REAL :: X_ran,Y_ran
   ENSURE(X_max>X_min,"MAT{INT}:to_histogram_from_data ... X_max is smaller than X_min!")
   ENSURE(Y_max>Y_min,"MAT{INT}:to_histogram_from_data ... Y_max is smaller than Y_min!")
   ENSURE(X_bin<(X_max-X_min),"MAT{INT}:to_histogram_from_data ... X_bin size is larger than [X_min,X_max]")
   ENSURE(Y_bin<(Y_max-Y_min),"MAT{INT}:to_histogram_from_data ... X_bin size is larger than [X_min,X_max]")

      ! The range of value ...
      X_ran = X_max-X_min
      Y_ran = Y_max-Y_min

      ! The number of bins ...
      n_X = ceiling(X_ran/X_bin)
      n_Y = ceiling(Y_ran/Y_bin)

      ! Create the histogram (leaky)
      call create_(self,n_X,n_Y)
      self = 0

      ! Now do the binning ...
      do k = 1,size(X)

         ! Get the bins
         i = ceiling(min(X(k)-X_min,X_ran)/X_bin)
         j = ceiling(min(Y(k)-Y_min,Y_ran)/Y_bin)

         ! Accumulate
         self(i,j) = self(i,j)+1
      end do
   end subroutine

   PURE subroutine bin_XY_data_0(self,X,Y,bin_side_length,data_count)
   ! Set self to a matrix whose ij-th element contains the number of
   ! data points [X(k),Y(k)] which lie in the ij-th bin. A bin is
   ! simply a range of values of side length "bin_side_length"
   ! covering the set of points from [X_min,Y_min] to [X_max,Y_max].
   ! The dimension of self is calculated within this routine. If
   ! "data_count" is present, then the bin count associated with each
   ! data item [X(k),Y(k)] is returned in an array.
      MAT(INT,:,:), allocatable, OUT :: self
      VEC(REAL,:), IN :: X,Y
      REAL, IN :: bin_side_length
      VEC(INT,:), allocatable, optional, OUT :: data_count
      REAL :: X_min,X_max,X_mid,X_ran
      REAL :: Y_min,Y_max,Y_mid,Y_ran
      VEC(REAL,2) :: X_range,Y_range
      INT :: dim,n_X,n_Y,i,j,k
   ENSURE(size(X)==size(Y),"MAT{INT}:bin_XY_data_0 ... incompatible data points")
      dim = size(X)
      X_min = minval(X); Y_min = minval(Y)
      X_max = maxval(X); Y_max = maxval(Y)
      X_mid = HALF*(X_min+X_max)
      Y_mid = HALF*(Y_min+Y_max)
      X_ran = X_max-X_min
      Y_ran = Y_max-Y_min
      n_X = ceiling(X_ran/bin_side_length)
      n_Y = ceiling(Y_ran/bin_side_length)
      X_min = X_mid-(n_X/TWO)*bin_side_length
      Y_min = Y_mid-(n_Y/TWO)*bin_side_length
      X_ran = X_ran/n_X
      Y_ran = Y_ran/n_Y

      ! Now do the binning ...
      call create_(self,n_X,n_Y)
      if (present(data_count)) call create_(data_count,dim)
      do i = 1,n_X
         do j = 1,n_Y
            X_range(1) = X_min+(i-1)*bin_side_length
            X_range(2) = X_range(1)+bin_side_length
            Y_range(1) = Y_min+(j-1)*bin_side_length
            Y_range(2) = Y_range(1)+bin_side_length
            self(i,j) = count(in_range_(X,X_range) AND in_range_(Y,Y_range))
            if (present(data_count)) then
               do k = 1,dim
                  if (is_in_range_(X(k),X_range) AND is_in_range_(Y(k),Y_range)) then
                     data_count(k) = self(i,j)
                  end if
               end do
            end if
         end do
      end do
   end subroutine

   PURE subroutine bin_XY_data_1(self,X,Y,XY_min,XY_max,bin_side_length,data_count)
   ! Set self to a matrix whose ij-th element contains the number of data points
   ! [X(k),Y(k)] which lie in the ij-th bin. A bin is simply a range of values
   ! of side length "bin_side_length" covering the set of points from
   ! [X_min,Y_min] to [X_max,Y_max]. The dimension of self is calculated within
   ! this routine. If "data_count" is present, then the bin count associated
   ! with each data item [X(k),Y(k)] is returned in an array.
      MAT(INT,:,:), allocatable, OUT :: self
      VEC(REAL,:), IN :: X,Y
      REAL, IN :: XY_min,XY_max,bin_side_length
      VEC(INT,:), allocatable, optional, OUT :: data_count
      REAL :: X_min,X_max,X_mid,X_ran
      REAL :: Y_min,Y_max,Y_mid,Y_ran
      VEC(REAL,2) :: X_range,Y_range
      INT :: dim,n_X,n_Y,i,j,k
   ENSURE(size(X)==size(Y),"MAT{INT}:bin_XY_data_1 ... incompatible data points")
      dim = size(X)
      X_min = XY_min; Y_min = XY_min
      X_max = XY_max; Y_max = XY_max
      X_mid = HALF*(X_min+X_max)
      Y_mid = HALF*(Y_min+Y_max)
      X_ran = X_max-X_min
      Y_ran = Y_max-Y_min
      n_X = ceiling(X_ran/bin_side_length)
      n_Y = ceiling(Y_ran/bin_side_length)
      X_min = X_mid-(n_X/TWO)*bin_side_length
      Y_min = Y_mid-(n_Y/TWO)*bin_side_length
      X_ran = X_ran/n_X
      Y_ran = Y_ran/n_Y

      ! Now do the binning ...
      call create_(self,n_X,n_Y)
      if (present(data_count)) call create_(data_count,dim)
      do i = 1,n_X
         do j = 1,n_Y
            X_range(1) = X_min+(i-1)*bin_side_length
            X_range(2) = X_range(1)+bin_side_length
            Y_range(1) = Y_min+(j-1)*bin_side_length
            Y_range(2) = Y_range(1)+bin_side_length
            self(i,j) = count(in_range_(X,X_range) AND in_range_(Y,Y_range))
            if (NOT present(data_count)) cycle
            do k = 1,dim
               if (is_in_range_(X(k),X_range) AND is_in_range_(Y(k),Y_range)) then
                  data_count(k) = self(i,j)
               end if
            end do
         end do
      end do
   end subroutine


!  ==========
!  Misc stuff
!  ==========

   PURE subroutine make_diagonally_dominant(self,permutation)
   ! Rearrange the order of the columns of self so that the largest magnitude
   ! elements in each column occur along the diagonal. If "permutation" is
   ! present, it is the array which achieves this ordering, i.e. at the end of
   ! the routine, what is done is: self = self(:,permutation).
      MAT(INT,:,:), INOUT :: self
      VEC(INT,:), optional, OUT :: permutation
      VEC(INT,:), allocatable :: perm
      MAT(BIN,:,:), allocatable :: mask
      INT :: i,n
      VEC(INT,2) :: loc
   ENSURE(is_square_(self),"MAT{INT}:make_diagonally_dominant ... not square")

      ! The following code is inherited from MAT{INTRINSIC}
      if (present(permutation)) then
         ENSURE(size(permutation)==size(self,2),"MAT{INT}:make_diagonally_dominant ... wrong size, perm")
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


! Remove?

   subroutine to_gaussian_xyz_powers_0(self,l_max)
   ! Make "self(1:3,i)", the three xyz powers of all cartesian gaussian
   ! functions "i" of angular momentum up to "l_max", where "i" is the standard
   ! lexical index of the cartesian gaussian. (This routine essentially defines
   ! the standard order).  The shape of "self" is: [3, l_max.n_comp_sum ].
      MAT(INT,:,:), OUT :: self
      INT, IN :: l_max
      INT :: i,L,a,b,c
   ENSURE(size(self,1)==3,"MAT{INT}:to_gaussian_xyz_powers_0 ... wrong 1st dimension, self")
   ENSURE(size(self,2)==(l_max+1)*(l_max+2)*(l_max+3)/6,"MAT{INT}:to_gaussian_xyz_powers_0 ... wrong 2nd dimension, self")
      i = 1  ! This is the total lexical index
      do L = 0,l_max  ! Loop over all shells with momentum L

                     ! Loop over powers a, b, c
         do a = L,floor((L+2)*THIRD),-1
            do b = min(L-a,a),floor((L-a+1)*HALF),-1
               c = L-a-b
               if (a==b AND b==c) then
                  self(:,i) = [a,a,a]
                  i = i+1
               else if (a>b AND b==c) then
                  self(:,i) = [a,b,b]
                  self(:,i+1) = [b,a,b]
                  self(:,i+2) = [b,b,a]
                  i = i+3
               else if (a==b AND b>c) then
                  self(:,i) = [a,a,c]
                  self(:,i+1) = [a,c,a]
                  self(:,i+2) = [c,a,a]
                  i = i+3
               else
                  self(:,i) = [a,b,c]
                  self(:,i+1) = [a,c,b]
                  self(:,i+2) = [b,a,c]
                  self(:,i+3) = [c,a,b]
                  self(:,i+4) = [b,c,a]
                  self(:,i+5) = [c,b,a]
                  i = i+6
               end if
            end do
         end do
      end do
   end subroutine

   subroutine to_gaussian_xyz_powers_1(self,l_max,index)
   ! Make "self(1:3,i)", the three xyz powers of all cartesian gaussian
   ! functions "i" of angular momentum up to "l_max", where "i" is the standard
   ! lexical index of the cartesian gaussian. (This routine essentially defines
   ! the standard order).  The shape of "self" is: [3, l_max.n_comp_sum ].
   ! Array "index" maps the three xyz powers of each cartesian gaussian back to
   ! its lexical index *within a shell of the same angular momentum* i.e. not
   ! the total lexical index. NOTE: "index" has lower bounds of 0, and so is
   ! passed as a pointer.
      MAT(INT,:,:), OUT :: self
      INT, IN :: l_max
      MAT3(INT,:,:,:), PTR :: index
      INT :: i,L,k,a,b,c
   ENSURE(size(self,1)==3,"MAT{INT}:to_gaussian_xyz_powers_1 ... wrong 1st dimension, self")
   ENSURE(size(self,2)==(l_max+1)*(l_max+2)*(l_max+3)/6,"MAT{INT}:to_gaussian_xyz_powers_1 ... wrong 2nd dimension, self")
   ENSURE(lbound(index,1)==0,"MAT{INT}:to_gaussian_xyz_powers_1 ... wrong lower bound, index")
   ENSURE(lbound(index,2)==0,"MAT{INT}:to_gaussian_xyz_powers_1 ... wrong lower bound, index")
   ENSURE(lbound(index,3)==0,"MAT{INT}:to_gaussian_xyz_powers_1 ... wrong lower bound, index")
   ENSURE(ubound(index,1)==l_max,"MAT{INT}:to_gaussian_xyz_powers_1 ... wrong upper bound, index")
   ENSURE(ubound(index,2)==l_max,"MAT{INT}:to_gaussian_xyz_powers_1 ... wrong upper bound, index")
   ENSURE(ubound(index,3)==l_max,"MAT{INT}:to_gaussian_xyz_powers_1 ... wrong upper bound, index")
      i = 1  ! This is the total lexical index
      do L = 0,l_max  ! Loop over all shells with momentum L
         k = 1  ! This is the local shell lexical index
      !                  Loop over powers a, b, c
         do a = L,floor((L+2)*THIRD),-1
            do b = min(L-a,a),floor((L-a+1)*HALF),-1
               c = L-a-b
               if (a==b AND b==c) then
                  self(:,i) = [a,a,a]
                  index(a,a,a) = k
                  i = i+1
                  k = k+1
               else if (a>b AND b==c) then
                  self(:,i) = [a,b,b]
                  self(:,i+1) = [b,a,b]
                  self(:,i+2) = [b,b,a]
                  index(a,b,b) = k
                  index(b,a,b) = k+1
                  index(b,b,a) = k+2
                  i = i+3
                  k = k+3
               else if (a==b AND b>c) then
                  self(:,i) = [a,a,c]
                  self(:,i+1) = [a,c,a]
                  self(:,i+2) = [c,a,a]
                  index(a,a,c) = k
                  index(a,c,a) = k+1
                  index(c,a,a) = k+2
                  i = i+3
                  k = k+3
               else
                  self(:,i) = [a,b,c]
                  self(:,i+1) = [a,c,b]
                  self(:,i+2) = [b,a,c]
                  self(:,i+3) = [c,a,b]
                  self(:,i+4) = [b,c,a]
                  self(:,i+5) = [c,b,a]
                  index(a,b,c) = k
                  index(a,c,b) = k+1
                  index(b,a,c) = k+2
                  index(c,a,b) = k+3
                  index(b,c,a) = k+4
                  index(c,b,a) = k+5
                  i = i+6
                  k = k+6
               end if
            end do
         end do
      end do
   end subroutine

   pure function floyd_warshall(self) result (res)
   ! find the all pairs shortest path
      MAT(INT,:,:), IN :: self
      MAT(INT,:,:), allocatable :: res
      INT :: i,j,k
      allocate(res(size(self,1),size(self,2)))
      res(:,:) = self(:,:)
      do k = 1,size(self,1)
         do i = 1,size(self,1)
            do j = 1,size(self,1)
               res(i,j) = min(res(i,j),res(i,k)+res(k,j))
            end do
         end do
      end do
   end function

end module
