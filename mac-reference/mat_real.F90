!==================================================================
!
! MAT{REAL}: Matrix operations ...
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
module MAT_REAL_MODULE

#  include "macros"

   include "mat{real}.use"

! #ifdef INTEL_ifort
!    USE lapack95, only: dsyev
!    USE lapack95, only: dgesv
!    USE lapack95, only: dgetrf
!    USE lapack95, only: dgetri
! #endif

   implicit none

   include "mat{real}.int"


   ! For getting rotation matrix to match vectors
! saved_reference :: MAT{REAL}*, private   DEFAULT_NULL
! saved_actual    :: MAT{REAL}*, private   DEFAULT_NULL

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim1,dim2)
   ! Allocate a matrix with the given dimensions
      MAT(REAL,:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{INTRINSIC}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim1,dim2),stat=status)
      ENSURE(status==0,"MAT{REAL}:create_0 ... error in allocating memory")
#else
      allocate(self(dim1,dim2))
#endif
   end subroutine

   PURE subroutine create_1(self,lb1,ub1,lb2,ub2)
   ! Allocate a matrix with the given bounds
      MAT(REAL,:,:), allocatable, OUT :: self
      INT, IN :: lb1,ub1
      INT, IN :: lb2,ub2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{INTRINSIC}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb1:ub1,lb2:ub2),stat=status)
      ENSURE(status==0,"MAT{REAL}:create_1 ... error in allocating memory")
#else
      allocate(self(lb1:ub1,lb2:ub2))
#endif
   end subroutine

   PURE subroutine create_2(self,bounds1,bounds2)
   ! Allocate object with the given array bounds
      MAT(REAL,:,:), allocatable, OUT :: self
      VEC(INT,2), IN :: bounds1
      VEC(INT,2), IN :: bounds2

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2))
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      MAT(REAL,:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT{REAL}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ============================
!  Size-of and shape operations
!  ============================

   pure function is_same_shape_as(self,a) result (res)
   ! Returns TRUE if the matrix "a" has the same shape as "self"
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), IN :: a
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(a,1) AND size(self,2)==size(a,2)
   end function

   pure function is_transposed_shape_of(self,a) result (res)
   ! Returns TRUE if the matrix "a" is the transposed shape of self
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), IN :: a
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(a,2) AND size(self,2)==size(a,1)
   end function

   pure function is_square(self) result (res)
   ! Returns TRUE if the matrix is square
      MAT(REAL,:,:), IN :: self
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(self,2)
   end function


! New

!  =======================
!  Shrinking and expansion
!  =======================

   PURE subroutine shrink(self,dim1,dim2)
   ! Shrink allocatable self to have upper bounds dim1 x dim2.
      MAT(REAL,:,:), allocatable, INOUT :: self
      INT, IN :: dim1,dim2
      MAT(REAL,:,:), allocatable :: old
      INT :: lb1,lb2,ub1,ub2
   ENSURE(allocated(self),"MAT{REAL}:shrink ... matrix not allocated")
   ENSURE(dim1<=ubound(self,1),"MAT{REAL}:shrink ... 1st dimension given is too large.")
   ENSURE(dim2<=ubound(self,2),"MAT{REAL}:shrink ... 2nd dimension given is too large.")

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
      MAT(REAL,:,:), allocatable, INOUT :: self
      INT, IN :: dim2
      INT :: ub1,ub2
   ENSURE(allocated(self),"MAT{REAL}:shrink_columns ... matrix not allocated")
   ENSURE(dim2<=ubound(self,2),"MAT{REAL}:shrink_columns ... 2nd dimension given is too large.")

      ! The following code is inherited from MAT{INTRINSIC}
      ub1 = ubound(self,1)
      ub2 = ubound(self,2)
      if (dim2==ub2) return
      call shrink_(self,ub1,dim2)
   end subroutine

   PURE subroutine expand(self,dim1,dim2)
   ! Expand allocatable self to dim1 x dim2.
   ! New elements are initialised.
      MAT(REAL,:,:), allocatable, INOUT :: self
      INT, IN :: dim1,dim2
      MAT(REAL,:,:), allocatable :: new
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
         ENSURE(dim1>=ubound(self,1),"MAT{REAL}:expand ... 1st dimension given is too small")
         ENSURE(dim2>=ubound(self,2),"MAT{REAL}:expand ... 2nd dimension given is too small")

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
      MAT(REAL,:,:), allocatable, INOUT :: self
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
      MAT(REAL,:,:), allocatable, INOUT :: self
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
      MAT(REAL,:,:), allocatable, INOUT :: self
      MAT(REAL,:,:), IN :: cols
      INT :: ub2

      ! The following code is inherited from MAT{INTRINSIC}
      if (NOT allocated(self)) then
         self = cols
      else
         ENSURE(size(self,1)==size(cols,1),"MAT{REAL}:append_columns ... 1st dimension wrong, cols")

         ! Expand
         ub2 = ubound(self,2)
         call expand_columns_(self,ub2+size(cols,2))

         ! Append
         self(:,ub2+1:) = cols(:,:)
      end if
   end subroutine

   PURE subroutine append_column(self,col)
   ! Append the column "col" onto end of allocatable self.
      MAT(REAL,:,:), allocatable, INOUT :: self
      VEC(REAL,:), IN :: col
      INT :: ub2

      ! The following code is inherited from MAT{INTRINSIC}
      if (NOT allocated(self)) then
         call create_(self,size(col),1)
         self(:,1) = col(:)
      else
         ENSURE(size(self,1)==size(col),"MAT{REAL}:append_column ... 2nd dimension given is too small")

         ! Expand
         ub2 = ubound(self,2)
         call expand_columns_(self,ub2+1)

         ! Append
         self(:,ub2+1) = col
      end if
   end subroutine

   PURE subroutine prune_column(self,col)
   ! Prune the column with index "col".
      MAT(REAL,:,:), allocatable, INOUT :: self
      INT, IN :: col
      INT :: lb2,ub2
   ENSURE(allocated(self),"MAT{REAL}:prune_column ... self is not.associated")
   ENSURE(is_in_range_(col,[lbound(self,2),ubound(self,2)]),"MAT{REAL}:prune_column ... col is out of range")

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
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), IN :: b
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = same_as_(self,b)
   end function

   pure function same_as(self,b,eps) result (res)
   ! Check if the matrix is the same as "b", within "eps".
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), IN :: b
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


!  ================
!  Range operations
!  ================

   pure function all_in_range(self,range) result (res)
   ! Return TRUE if all values of self are within the specified "range".
      MAT(REAL,:,:), IN :: self
      VEC(REAL,2), IN :: range
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = all(range(1)<=self AND self<=range(2))
   end function

   pure function in_range(self,range) result (res)
   ! Return element ij as TRUE if self(i,j) is within the specified "range".
      MAT(REAL,:,:), IN :: self
      VEC(REAL,2), IN :: range
      MAT(BIN,size(self,1),size(self,2)) :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = (range(1)<=self AND self<=range(2))
   end function

   function element_range(self) result (res)
   ! Return the range (smallest and largest value) of self.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,2) :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res(1) = minval(self)
      res(2) = maxval(self)
   end function

   pure function number_in_range(self,range,symmetric,absolute) result (res)
   ! Return the number of element self(i,j) within the specified
   ! "range".  If "symmetric" is present and TRUE, only the elements
   ! in "self" where the row is greater than the col index are
   ! reported. If "absolute" is present and TRUE, absolute values of
   ! "self" in the range are used.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,2), IN :: range
      BIN, IN :: symmetric
      BIN, optional, IN :: absolute
      INT :: res
      INT :: r,c
      REAL :: v,va
      BIN :: absol

      ! The following code is inherited from MAT{INTRINSIC}
      absol = FALSE
      if (present(absolute)) absol = absolute
      res = 0
      if (NOT symmetric) then

         ! Do whole matrix
         if (absol) then
            res = 0
            do r = 1,size(self,1)
               do c = 1,size(self,2)
                  v = self(r,c)
                  va = abs(v)
                  if (va<range(1)) cycle
                  if (va>range(2)) cycle
                  res = res+1
               end do
            end do
         else
            res = 0
            do r = 1,size(self,1)
               do c = 1,size(self,2)
                  v = self(r,c)
                  if (v<range(1)) cycle
                  if (v>range(2)) cycle
                  res = res+1
               end do
            end do
         end if
      else

         ! Do lower triangle
         if (absol) then
            res = 0
            do r = 1,size(self,1)
               do c = 1,r
                  v = self(r,c)
                  va = abs(v)
                  if (va<range(1)) cycle
                  if (va>range(2)) cycle
                  res = res+1
               end do
            end do
         else
            res = 0
            do r = 1,size(self,1)
               do c = 1,r
                  v = self(r,c)
                  if (v<range(1)) cycle
                  if (v>range(2)) cycle
                  res = res+1
               end do
            end do
         end if
      end if
   end function

   pure subroutine get_indices_in_range(self,range,row,col,val,symmetric,absolute)
   ! Get the "row" and "col" indices, and values "val", of the
   ! elements of "self" which are within the specified "range".
   ! If "absolute" is present absolute values of "self" are used.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,2), IN :: range
      BIN, IN :: symmetric
      VEC(INT,:), OUT :: row,col
      VEC(REAL,:), OUT :: val
      BIN, optional, IN :: absolute

   ! ENSURE(row.dim==col.dim,"inconsistent row and col dimension")
   ! ENSURE(row.dim==val.dim,"inconsistent row and val dimension")
   ! ENSURE(row.dim==.number_in_range(range,symmetric,absolute),"inconsistent row dimension")
      INT :: i,r,c
      REAL :: v,va
      BIN :: absol

      ! The following code is inherited from MAT{INTRINSIC}
      absol = FALSE
      if (present(absolute)) absol = absolute
      if (NOT symmetric) then

         ! Do whole matrix
         if (absol) then
            i = 0
            do r = 1,size(self,1)
               do c = 1,size(self,2)
                  v = self(r,c)
                  va = abs(v)
                  if (va<range(1)) cycle
                  if (va>range(2)) cycle
                  i = i+1
                  row(i) = r
                  col(i) = c
                  val(i) = v
               end do
            end do
         else
            i = 0
            do r = 1,size(self,1)
               do c = 1,size(self,2)
                  v = self(r,c)
                  if (v<range(1)) cycle
                  if (v>range(2)) cycle
                  i = i+1
                  row(i) = r
                  col(i) = c
                  val(i) = v
               end do
            end do
         end if
      else

         ! Do lower triangle
         if (absol) then
            i = 0
            do r = 1,size(self,1)
               do c = 1,r
                  v = self(r,c)
                  va = abs(v)
                  if (va<range(1)) cycle
                  if (va>range(2)) cycle
                  i = i+1
                  row(i) = r
                  col(i) = c
                  val(i) = v
               end do
            end do
         else
            i = 0
            do r = 1,size(self,1)
               do c = 1,r
                  v = self(r,c)
                  if (v<range(1)) cycle
                  if (v>range(2)) cycle
                  i = i+1
                  row(i) = r
                  col(i) = c
                  val(i) = v
               end do
            end do
         end if
      end if
   end subroutine


!  =================
!  Inquiry functions
!  =================

   PURE function is_diagonal(self,eps) result (res)
   ! Returns TRUE if the matrix "self" is a diagonal matrix to within
   ! tolerance "eps" (if present).
      MAT(REAL,:,:), IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      INT :: dim,i,j
   ENSURE(is_square_(self),"MAT{REAL}:is_diagonal ... Non-square matrix")

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
      MAT(REAL,:,:), IN :: self
      REAL, IN, optional :: eps
      BIN :: res
      INT :: i
   ENSURE(is_square_(self),"MAT{REAL}:has_unit_diagonal ... Non-square matrix")

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
      MAT(REAL,:,:), IN :: self
      BIN :: res
      INT :: i
   ENSURE(is_square_(self),"MAT{REAL}:has_minus_unit_diagonal ... Non-square matrix")

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
      MAT(REAL,:,:), IN :: self
      REAL, optional, IN :: eps
      BIN :: res
   ENSURE(is_square_(self),"MAT{REAL}:is_unit_mx ... Non-square matrix")

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
      MAT(REAL,:,:), IN :: self
      BIN :: res
   ENSURE(is_square_(self),"MAT{REAL}:is_inversion_mx ... Non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      res = has_minus_unit_diagonal_(self) AND is_diagonal_(self)
   end function

   PURE function is_symmetric(self) result (res)
   ! Returns TRUE if the matrix "self" is a symmetric matrix
      MAT(REAL,:,:), IN :: self
      BIN :: res
      INT :: i,j
   ENSURE(is_square_(self),"MAT{REAL}:is_symmetric ... Non-square matrix")

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
      MAT(REAL,:,:), IN :: self
      BIN :: res
      INT :: dim,i,j
   ENSURE(is_square_(self),"MAT{REAL}:is_antisymmetric ... Non-square matrix")

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

   pure function is_zero(self,eps) result (res)
   ! Return TRUE is "self" is the zero matrix, i.e. every element is zero.
   ! If present, "eps" is used to decide when a small number is zero.
      MAT(REAL,:,:), IN :: self
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

   PURE function is_orthogonal_mx(self,eps) result (res)
   ! Returns TRUE if the matrix "self" is an orthogonal matrix to
   ! within tolerance "eps" (if present).
      MAT(REAL,:,:), IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      MAT(REAL,:,:), allocatable :: W

      ! W = S^T S
      call create_(W,size(self,1),size(self,2))
      call to_product_of_(W,self,self,transpose_a=TRUE)

      ! Is W a unit matrix?
      res = is_unit_mx_(W,eps)
      call destroy_(W)
   end function


!  =================
!  Column operations
!  =================

   PURE function has_column(self,c,eps) result (res)
   ! Returns TRUE if the matrix "self" has a column "c", with "eps" tolerance.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), IN :: c
      REAL, optional, IN :: eps
      BIN :: res
      INT :: n
   ENSURE(size(c)==size(self,1),"MAT{REAL}:has_column ... incompatible column size")

      ! The following code is inherited from MAT{INTRINSIC}
      res = FALSE
      do n = 1,size(self,2)
         res = same_as_(self(:,n),c,eps)
         if (res) exit
      end do
   end function

   PURE function has_any_column_from(self,list) result (res)
   ! Returns TRUE if "self" has any column in common with "list".
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), IN :: list
      BIN :: res
      INT :: n
   ENSURE(size(list,1)==size(self,1),"MAT{REAL}:has_any_column_from ... incompatible row size")

      ! The following code is inherited from MAT{INTRINSIC}
      res = FALSE
      do n = 1,size(list,2)
         res = has_column_(self,list(:,n))
         if (res) exit
      end do
   end function

   PURE function index_for_column(self,c,eps) result (res)
   ! Return the index of the first column in "self" which matches "c",
   ! within tolerance "eps", or else return 0 for no match.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), IN :: c
      REAL, optional, IN :: eps
      INT :: res
      INT :: n
   ENSURE(size(c)==size(self,1),"MAT{REAL}:index_for_column ... incompatible column size")

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
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), IN :: c
      REAL, optional, IN :: eps
      VEC(INT,:), allocatable :: res
      INT :: n
   ENSURE(size(c)==size(self,1),"MAT{REAL}:indices_for_column ... incompatible column size")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(res,0)
      do n = 1,size(self,2)
         if (same_as_(self(:,n),c,eps)) then
            call append_(res,n)
         end if
      end do
   end function

   PURE subroutine compare_columns_with(self,m,col)
   ! Compare the columns of "self" with "m". The elements of array "col" are set
   ! TRUE if the corresponding column appears in "m"
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), IN :: m
      VEC(INT,:), OUT :: col
      INT :: n,i
   ENSURE(size(m,1)==size(self,1),"MAT{REAL}:compare_columns_with ... incompatible column size")
   ENSURE(size(col)==size(self,2),"MAT{REAL}:compare_columns_with ... wrong size for col")

      ! The following code is inherited from MAT{INTRINSIC}
      col = 0
      do n = 1,size(self,2)
         do i = 1,size(m,2)
            if (NOT same_as_(self(:,n),m(:,i))) cycle
            col(n) = i
            exit
         end do
      end do
   end subroutine

   PURE subroutine unique_columns_0(self,col)
   ! Compare the later columns of "self" with earlier columns to see if they are
   ! unique. The elements of array "col" are set TRUE if the corresponding
   ! column is unique.
      MAT(REAL,:,:), IN :: self
      VEC(BIN,:), OUT :: col
      INT :: n,i
   ENSURE(size(self,2)>0,"MAT{REAL}:unique_columns_0 ... must have at least one column")
   ENSURE(size(col)==size(self,2),"MAT{REAL}:unique_columns_0 ... wrong size for col")

      ! The following code is inherited from MAT{INTRINSIC}
      col = TRUE
      do n = 2,size(self,2)
         do i = 1,n-1
            if (NOT same_as_(self(:,n),self(:,i))) cycle
            col(n) = FALSE
            exit
         end do
      end do
   end subroutine

   PURE subroutine unique_columns_1(self,col)
   ! Compare the later columns of "self" with earlier columns to see if they are
   ! unique. The elements of array "col" are set to the indices of the unique
   ! columns.
      MAT(REAL,:,:), IN :: self
      VEC(INT,:), allocatable, OUT :: col
      INT :: n,i
      VEC(BIN,:), allocatable :: is_unique
   ENSURE(size(self,2)>0,"MAT{REAL}:unique_columns_1 ... must have at least one column")
   ENSURE(NOT allocated(col),"MAT{REAL}:unique_columns_1 ... col is allocated")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(is_unique,size(self,2))
      call unique_columns_(self,is_unique)
      call create_(col,count(is_unique))  ! this is leaky
      i = 1
      col(1) = 1
      do n = 2,size(self,2)
         if (NOT is_unique(n)) cycle
         i = i+1
         col(i) = n
      end do
      call destroy_(is_unique)
   end subroutine

   PURE function no_of_unique_columns(self) result (res)
   ! Compare the later columns of "self" with earlier columns to see if they are
   ! unique. Return the number of unique columns.
      MAT(REAL,:,:), IN :: self
      INT :: res
      INT :: n,i
   ENSURE(size(self,2)>0,"MAT{REAL}:no_of_unique_columns ... must have at least one column")

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,2)
      do n = 2,size(self,2)
         do i = 1,n-1
            if (NOT same_as_(self(:,n),self(:,i))) cycle
            res = res-1
            exit
         end do
      end do
   end function

   PURE subroutine swap_columns_0(self,col1,col2)
   ! Swap columns "col1" and "col2" of self
      MAT(REAL,:,:), INOUT :: self
      INT, IN :: col1,col2
      INT :: i
      REAL :: val
   ENSURE(col1<=size(self,2) AND col2<=size(self,2),"MAT{REAL}:swap_columns_0 ... columns exceed dimesions")

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
      MAT(REAL,:,:), INOUT :: self
      VEC(INT,:), IN :: list
      INT :: l
   ENSURE(maxval(list)<=size(self,2),"MAT{REAL}:swap_columns_1 ... list value exceed column dimension")

      ! The following code is inherited from MAT{INTRINSIC}
      do l = 1,size(list)
         call swap_columns_(self,l,list(l))
      end do
   end subroutine

   PURE subroutine reverse_column_order(self)
   ! Reverse the order of the columns of self.
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), allocatable :: tmp
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

   function column_norms(self) result (res)
   ! Return the norms of every column
      MAT(REAL,:,:), IN :: self
      VEC(REAL,size(self,2)) :: res
      INT :: i

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,2)
         res(i) = norm_(self(:,i))
      end do
   end function

   subroutine get_column_norms(self,res)
   ! Return the norms of every column
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), OUT :: res
      INT :: i
   ENSURE(size(res)==size(self,2),"MAT{REAL}:get_column_norms ... wrong size, res array")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,2)
         res(i) = norm_(self(:,i))
      end do
   end subroutine

   subroutine get_column_dot_products(self,res)
   ! Return the dot products of every column with itself.
   ! Good for testing distances without using a sqrt.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), OUT :: res
      INT :: i
   ENSURE(size(res)==size(self,2),"MAT{REAL}:get_column_dot_products ... wrong size, res array")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,2)
         res(i) = dot_product(self(:,i),self(:,i))
      end do
   end subroutine

   pure function index_of_minimum_column_norm(self) result (res)
   ! Return the column index of the column with the *minimum* norm.
      MAT(REAL,:,:), IN :: self
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

   pure function max_abs_column_difference(self) result (res)
   ! Return the maximum of the absolute difference between all the column vector
   ! pairs of the matrix.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,size(self,1)) :: res
      VEC(REAL,size(self,1)) :: diff
      VEC(REAL,size(self,1)) :: col_i,col_j
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
      MAT(REAL,:,:), IN :: self
      VEC(REAL,size(self,1)) :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = sum_column_vectors_(self)/size(self,2)
   end function

   pure function sum_column_vectors(self) result (res)
   ! Sum the column vectors (i.e. rows) in "self".
      MAT(REAL,:,:), IN :: self
      VEC(REAL,size(self,1)) :: res
      INT :: i,j
      REAL :: val

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         val = 0
         do j = 1,size(self,2)
            val = val+self(i,j)
         end do
         res(i) = val
      end do
   end function

   pure subroutine plus_column(self,col)
   ! Add column "col" to every column of "self".
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), optional, IN :: col

      ! The following code is inherited from MAT{INTRINSIC}
      self = self+spread(col,dim=2,ncopies=size(self,2))
   end subroutine

   pure subroutine minus_column(self,col)
   ! Subtract column "col" to every column of "self".
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), optional, IN :: col

      ! The following code is inherited from MAT{INTRINSIC}
      self = self-spread(col,dim=2,ncopies=size(self,2))
   end subroutine

   pure subroutine plus_scaled_column(self,col,fac)
   ! Add the "fac" scaled column "col" to every column of "self".
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), optional, IN :: col
      REAL, IN :: fac

      ! The following code is inherited from MAT{INTRINSIC}
      self = self+fac*spread(col,dim=2,ncopies=size(self,2))
   end subroutine


!  ==============
!  Row operations
!  ==============

   PURE subroutine unique_rows_0(self,row)
   ! Compare the later rows of "self" with earlier rows to see if they are
   ! unique. The elements of array "row" are set TRUE if the corresponding
   ! row is unique.
      MAT(REAL,:,:), IN :: self
      VEC(BIN,:), OUT :: row
      INT :: n,i
   ENSURE(size(self,1)>0,"MAT{REAL}:unique_rows_0 ... must have at least one row")
   ENSURE(size(row)==size(self,1),"MAT{REAL}:unique_rows_0 ... wrong size for row")

      ! The following code is inherited from MAT{INTRINSIC}
      row = TRUE
      do n = 2,size(self,1)
         do i = 1,n-1
            if (NOT same_as_(self(n,:),self(i,:))) cycle
            row(n) = FALSE
            exit
         end do
      end do
   end subroutine

   PURE subroutine unique_rows_1(self,row)
   ! Compare the later rows of "self" with earlier rows to see if they are
   ! unique. The elements of array "row" are set to the indices of the unique
   ! rows.
      MAT(REAL,:,:), IN :: self
      VEC(INT,:), allocatable, OUT :: row
      INT :: n,i
      VEC(BIN,:), allocatable :: is_unique
   ENSURE(size(self,1)>0,"MAT{REAL}:unique_rows_1 ... must have at least one row")
   ENSURE(size(row)==size(self,1),"MAT{REAL}:unique_rows_1 ... wrong size for row")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(is_unique,size(self,1))
      call unique_rows_(self,is_unique)
      call create_(row,count(is_unique))  ! this is leaky
      i = 1
      row(1) = 1
      do n = 2,size(self,1)
         if (NOT is_unique(n)) cycle
         i = i+1
         row(i) = n
      end do
      call destroy_(is_unique)
   end subroutine

   PURE function no_of_unique_rows(self) result (res)
   ! Compare the later row of "self" with earlier rows to see if they are
   ! unique. Return the number of unique rows.
      MAT(REAL,:,:), IN :: self
      INT :: res
      INT :: n,i
   ENSURE(size(self,1)>0,"MAT{REAL}:no_of_unique_rows ... must have at least one row")

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)
      do n = 2,size(self,1)
         do i = 1,n-1
            if (NOT same_as_(self(n,:),self(i,:))) cycle
            res = res-1
            exit
         end do
      end do
   end function

   PURE subroutine swap_rows_0(self,row1,row2)
   ! Swap columns "row1" and "row2" of self
      MAT(REAL,:,:), INOUT :: self
      INT, IN :: row1,row2
      INT :: i
      REAL :: val
   ENSURE(row1<=size(self,1) AND row2<=size(self,1),"MAT{REAL}:swap_rows_0 ... columns exceed dimesions")

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
      MAT(REAL,:,:), INOUT :: self
      VEC(INT,:), IN :: list
      INT :: l
   ENSURE(maxval(list)<=size(self,1),"MAT{REAL}:swap_rows_1 ... list value exceed row dimension")

      ! The following code is inherited from MAT{INTRINSIC}
      do l = 1,size(list)
         call swap_rows_(self,l,list(l))
      end do
   end subroutine

   PURE function row_norms(self) result (res)
   ! Return the norms of every row
      MAT(REAL,:,:), IN :: self
      VEC(REAL,size(self,1)) :: res
      INT :: i

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         res(i) = norm_(self(i,:))
      end do
   end function

   PURE subroutine get_row_norms(self,res)
   ! Return the norms of every row
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), OUT :: res
      INT :: i
   ENSURE(size(res)==size(self,1),"MAT{REAL}:get_row_norms ... wrong size, res array")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         res(i) = norm_(self(i,:))
      end do
   end subroutine

   pure function sum_row_vectors(self) result (res)
   ! Sum the row vectors (i.e. columns) in "self".
      MAT(REAL,:,:), IN :: self
      VEC(REAL,size(self,2)) :: res
      INT :: i,j
      REAL :: val

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
      MAT(REAL,:,:), IN :: self
      REAL :: res
      MAT(REAL,:,:), allocatable :: adj
      INT :: c
   ENSURE(is_square_(self),"MAT{REAL}:determinant ... non-square matrix")
   ENSURE(size(self,1)<4,"MAT{REAL}:determinant ... only works for up to size 3 matrices")

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
      MAT(REAL,:,:), IN :: self
      INT, IN :: i,j
      MAT(REAL,:,:), allocatable :: res
   ENSURE(is_square_(self),"MAT{REAL}:adjugate ... non-square matrix")
   ENSURE(size(self,1)>1,"MAT{REAL}:adjugate ... dimension must be greater than 1")

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
      MAT(REAL,:,:), IN :: self
      REAL :: res
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
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), IN :: l,r
      REAL :: res
      VEC(REAL,:), allocatable :: w
   ENSURE(size(self,1)==size(l),"MAT{REAL}:dot_0 ... wrong size, r")
   ENSURE(size(self,2)==size(r),"MAT{REAL}:dot_0 ... wrong size, r")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(w,size(l))
      call to_product_of_(w,self,r)
      res = dot_product(l,w)
      call destroy_(w)
   end function

   PURE function dot_1(self,l,r) result (res)
   ! Multiply the matrix self by vector "l" on the left and vector "r" on the
   ! right ie:  res = l^dagger self r. Useful for non-unit metric dot_products.
      MAT(REAL,:,:), IN :: self
      VEC(CPX,:), IN :: l,r
      CPX :: res
      VEC(CPX,:), allocatable :: w
   ENSURE(size(self,1)==size(l),"MAT{REAL}:dot_1 ... wrong size, r")
   ENSURE(size(self,2)==size(r),"MAT{REAL}:dot_1 ... wrong size, r")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(w,size(l))
      call to_product_of_(w,self,r)
      res = dot_product(l,w)
      call destroy_(w)
   end function

   PURE subroutine rotate(self,v)
   ! Rotate vector "v" by self
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), INOUT :: v
      INT :: i,j
      VEC(REAL,:), allocatable :: w
      REAL :: val
   ENSURE(is_square_(self),"MAT{REAL}:rotate ... incompatible arrays sizes")
   ENSURE(size(self,2)==size(v),"MAT{REAL}:rotate ... incompatible arrays sizes")

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

   pure subroutine jacobi_rotation_0(self,p,q)
   ! Do a two dimensional Jacobi rotation in the plane of "p" and "q" so as to
   ! eliminate the off diagonal element self(p,q). NOTE: self must be symmetric
      MAT(REAL,:,:), INOUT :: self
      INT, IN :: p,q
      INT :: i
      REAL :: s_pp,s_qq,s_pq,theta
      REAL :: t,c,s,tau
      REAL :: s_ip,s_iq
      REAL :: r_ip,r_iq
      s_pp = self(p,p)
      s_qq = self(q,q)
      s_pq = self(p,q)
      theta = (s_qq-s_pp)/(TWO*s_pq)
      t = ONE/(abs(theta)+sqrt(theta*theta+ONE))
      if (theta<ZERO) t = -t
      c = ONE/sqrt(t*t+ONE)
      s = t*c
      tau = s/(ONE+c)

      ! Jacobi rotations on "self"
      do i = 1,size(self,1)
         s_ip = self(i,p)
         s_iq = self(i,q)
         r_ip = s_ip-s*(s_iq+tau*s_ip)
         r_iq = s_iq+s*(s_ip-tau*s_iq)
         self(i,p) = r_ip; self(p,i) = r_ip
         self(i,q) = r_iq; self(q,i) = r_iq
      end do
      self(p,p) = s_pp-t*s_pq
      self(q,q) = s_qq+t*s_pq
      self(p,q) = ZERO
      self(q,p) = ZERO
   end subroutine

   pure subroutine jacobi_rotation_1(self,p,q,v)
   ! Do a two dimensional Jacobi rotation in the plane of "p" and "q"
   ! so as to eliminate the off diagonal element self(p,q) and also
   ! update the rotation matrix "v". NOTE: self must be symmetric
      MAT(REAL,:,:), INOUT :: self
      INT, IN :: p,q
      MAT(REAL,:,:), INOUT :: v
      INT :: i
      REAL :: s_pp,s_qq,s_pq,theta
      REAL :: t,c,s,tau
      REAL :: s_ip,s_iq
      REAL :: r_ip,r_iq
      s_pp = self(p,p)
      s_qq = self(q,q)
      s_pq = self(p,q)
      theta = (s_qq-s_pp)/(TWO*s_pq)
      t = ONE/(abs(theta)+sqrt(theta*theta+ONE))
      if (theta<ZERO) t = -t
      c = ONE/sqrt(t*t+ONE)
      s = t*c
      tau = s/(ONE+c)

      ! Jacobi rotations on "self"
      do i = 1,size(self,1)
         s_ip = self(i,p)
         s_iq = self(i,q)
         r_ip = s_ip-s*(s_iq+tau*s_ip)
         r_iq = s_iq+s*(s_ip-tau*s_iq)
         self(i,p) = r_ip; self(p,i) = r_ip
         self(i,q) = r_iq; self(q,i) = r_iq
      end do
      self(p,p) = s_pp-t*s_pq
      self(q,q) = s_qq+t*s_pq
      self(p,q) = ZERO
      self(q,p) = ZERO

      ! Inverse Jacobi rotn. on "v"
      do i = 1,size(self,1)
         s_ip = v(i,p)
         s_iq = v(i,q)
         v(i,p) = s_ip-s*(s_iq+tau*s_ip)
         v(i,q) = s_iq+s*(s_ip-tau*s_iq)
      end do
   end subroutine

   PURE subroutine to_3x3_rotation_mx(self,axis,angle)
   ! Set "self" to the rotation matrix which rotates around vector "axis" in
   ! an anticlockwise direction (when looking against the "axis" vector) by
   ! amount "angle" (in radians).
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: axis
      REAL, IN :: angle
      VEC(REAL,3) :: x,y,z,xp,yp
   ENSURE(is_square_(self),"MAT{REAL}:to_3x3_rotation_mx ... non-square matrix")
   ENSURE(size(self,1)==3,"MAT{REAL}:to_3x3_rotation_mx ... must be 3x3 matrix")
   ENSURE(size(axis)==3,"MAT{REAL}:to_3x3_rotation_mx ... axis is not 3 dimensional")
      z = axis
      call normalize_(z)
      x = cross_(z,[ONE,ZERO,ZERO])
      if (is_zero_(x,TOL(9))) x = cross_(z,[ZERO,ONE,ZERO])
      call normalize_(x)
      y = cross_(z,x)
      xp = cos(angle)*x+sin(angle)*y
      yp = cos(angle)*y-sin(angle)*x
      self = outer_product_with_(z,z)+outer_product_with_(xp,x)+outer_product_with_(yp,y)
   end subroutine

   PURE subroutine to_euler_rotation_mx(self,alpha,beta,gamma)
   ! Set "self" to the rotation matrix for the z-y-z Euler angles "alpha",
   ! "beta" and "gamma" (in radians): rotate by "gamma" about z, then by
   ! "beta" about y, then by "alpha" about z, i.e. Rz(alpha) Ry(beta) Rz(gamma).
   ! Each rotation is anticlockwise looking down the axis towards the origin.
      MAT(REAL,:,:), OUT :: self
      REAL, IN :: alpha,beta,gamma
      REAL :: ca,sa,cb,sb,cg,sg
   ENSURE(is_square_(self),"MAT{REAL}:to_euler_rotation_mx ... non-square matrix")
   ENSURE(size(self,1)==3,"MAT{REAL}:to_euler_rotation_mx ... must be 3x3 matrix")
      ca = cos(alpha); sa = sin(alpha)
      cb = cos(beta); sb = sin(beta)
      cg = cos(gamma); sg = sin(gamma)
      self(1,1) = ca*cb*cg-sa*sg
      self(1,2) = -ca*cb*sg-sa*cg
      self(1,3) = ca*sb
      self(2,1) = sa*cb*cg+ca*sg
      self(2,2) = -sa*cb*sg+ca*cg
      self(2,3) = sa*sb
      self(3,1) = -sb*cg
      self(3,2) = sb*sg
      self(3,3) = cb
   end subroutine

   pure subroutine to_unit_mx(self)
   ! Set "self" to the unit matrix
      MAT(REAL,:,:), OUT :: self
      INT :: i

      ! The following code is inherited from MAT{INTRINSIC}
      self = ZERO
      do i = 1,min(size(self,1),size(self,2))
         self(i,i) = ONE
      end do
   end subroutine

   pure subroutine zero_small_values(self,eps)
   ! Zero elements of the matrix which are less than "eps" in magnitude
      MAT(REAL,:,:), INOUT :: self
      REAL, IN :: eps

      ! The following code is inherited from MAT{INTRINSIC}
      where (abs(self)<eps)
         self = ZERO
      end where
   end subroutine

   PURE subroutine set_to(self,a)
   ! Set self to "a"
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a
   ENSURE(is_same_shape_as_(self,a),"MAT{REAL}:set_to ... incompatible shape")

      ! The following code is inherited from MAT{INTRINSIC}
      self = a
   end subroutine

   PURE subroutine set_to_transpose_of(self,a)
   ! Self becomes the transpose of "a"
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      INT :: i,j,dim
   ENSURE(is_square_(self),"MAT{REAL}:set_to_transpose_of ... non-square matrix")
   ENSURE(is_transposed_shape_of_(self,a),"MAT{REAL}:set_to_transpose_of ... incompatible shapes")

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
      MAT(REAL,:,:), INOUT :: self
      INT :: i,j,dim
      REAL :: tmp
   ENSURE(is_square_(self),"MAT{REAL}:to_transpose ... non-square matrix")

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
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
   ENSURE(is_same_shape_as_(self,a),"MAT{REAL}:plus ... incompatible shape")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self+a
   end subroutine

   PURE subroutine minus(self,a)
   ! Subtract from self the matrix "a"
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
   ENSURE(is_same_shape_as_(self,a),"MAT{REAL}:minus ... incompatible shape")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self-a
   end subroutine

   PURE subroutine to_scaled(self,a,fac)
   ! Set "self" to matrix "at" scaled by "fac"
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      REAL, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{REAL}:to_scaled ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = fac*a
   end subroutine

   PURE subroutine plus_scaled(self,a,fac)
   ! Add to "self" matrix "a" scaled by "fac"
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      REAL, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{REAL}:plus_scaled ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self+fac*a
   end subroutine

   PURE subroutine minus_scaled(self,a,fac)
   ! Subtract from "self" matrix "a" scaled by "fac"
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      REAL, IN :: fac
   ENSURE(is_same_shape_as_(self,a),"MAT{REAL}:minus_scaled ... different shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      self = self-fac*a
   end subroutine

   PURE subroutine to_product_of(self,a,b,transpose_a,transpose_b)
   ! Set "self" to the matrix product of "a" and "b". If present, "transpose_a"
   ! and "transpose_b" can be set to TRUE if "a" and "b" need to be transposed.
      MAT(REAL,:,:), OUT :: self
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
         ENSURE(size(self,1)>=size(a,1),"MAT{REAL}:to_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{REAL}:to_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{REAL}:to_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{REAL}:to_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{REAL}:to_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{REAL}:to_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,1),"MAT{REAL}:to_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{REAL}:to_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{REAL}:to_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{REAL}:to_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{REAL}:to_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{REAL}:to_product_of ... incompatible arrays")
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
      MAT(REAL,:,:), INOUT :: self
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
         ENSURE(size(self,1)>=size(a,1),"MAT{REAL}:plus_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{REAL}:plus_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{REAL}:plus_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{REAL}:plus_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{REAL}:plus_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{REAL}:plus_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,1),"MAT{REAL}:plus_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{REAL}:plus_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{REAL}:plus_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{REAL}:plus_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{REAL}:plus_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{REAL}:plus_product_of ... incompatible arrays")
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
      MAT(REAL,:,:), OUT :: self
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
         ENSURE(size(self,1)>=size(a,1),"MAT{REAL}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{REAL}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{REAL}:to_scaled_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{REAL}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{REAL}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{REAL}:to_scaled_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,1),"MAT{REAL}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{REAL}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{REAL}:to_scaled_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{REAL}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{REAL}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{REAL}:to_scaled_product_of ... incompatible arrays")
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
      MAT(REAL,:,:), INOUT :: self
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
         ENSURE(size(self,1)>=size(a,1),"MAT{REAL}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{REAL}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"MAT{REAL}:plus_scaled_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{REAL}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,2),"MAT{REAL}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"MAT{REAL}:plus_scaled_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,1),"MAT{REAL}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{REAL}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"MAT{REAL}:plus_scaled_product_of ... incompatible arrays")
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
         ENSURE(size(self,1)>=size(a,2),"MAT{REAL}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(self,2)>=size(b,1),"MAT{REAL}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"MAT{REAL}:plus_scaled_product_of ... incompatible arrays")
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

   PURE subroutine to_outer_product_of(self,a,fac)
   ! Set "self" to the outer product of "a" with itself, with an
   ! optional scale factor "fac".
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), IN :: a
      REAL, optional, IN :: fac
      INT :: i,j
   ENSURE(is_square_(self),"MAT{REAL}:to_outer_product_of ... self is not square")
   ENSURE(size(a)==size(self,1),"MAT{REAL}:to_outer_product_of ... incomptabile a")

      ! The following code is inherited from MAT{INTRINSIC}
      if (NOT present(fac)) then
         do i = 1,size(self,1)
            do j = 1,size(self,1)
               self(i,j) = a(i)*a(j)
            end do
         end do
      else
         do i = 1,size(self,1)
            do j = 1,size(self,1)
               self(i,j) = fac*a(i)*a(j)
            end do
         end do
      end if
   end subroutine

   PURE subroutine plus_outer_product_of(self,a,fac)
   ! Add to "self" the outer product of "a" with itself, with an
   ! optional scale factor "fac".
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), IN :: a
      REAL, optional, IN :: fac
      INT :: i,j
   ENSURE(is_square_(self),"MAT{REAL}:plus_outer_product_of ... self is not square")
   ENSURE(size(a)==size(self,1),"MAT{REAL}:plus_outer_product_of ... incomptabile a")

      ! The following code is inherited from MAT{INTRINSIC}
      if (NOT present(fac)) then
         do i = 1,size(self,1)
            do j = 1,size(self,1)
               self(i,j) = self(i,j)+a(i)*a(j)
            end do
         end do
      else
         do i = 1,size(self,1)
            do j = 1,size(self,1)
               self(i,j) = self(i,j)+fac*a(i)*a(j)
            end do
         end do
      end if
   end subroutine

   PURE subroutine to_product_with_diagonal_0(self,a,diag,transpose_a)
   ! Set "self" to the matrix product of "a" with diagonal matrix "diag" (stored
   ! as a vector).  If present, ""transpose_a" can be set to TRUE if "a" needs
   ! to be transposed.
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(REAL,:), IN :: diag
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      INT :: opt,j
   ENSURE(is_same_shape_as_(self,a),"MAT{REAL}:to_product_with_diagonal_0 ... incompatible dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE;
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (trans_a) opt = 1
      select case (opt)

      case (0)
         ENSURE(size(a,2)==size(diag),"MAT{REAL}:to_product_with_diagonal_0 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = a(:,j)*diag(j)
         end do

      case (1)
         ENSURE(size(a,1)==size(diag),"MAT{REAL}:to_product_with_diagonal_0 ... incompatible dimensions")
         do j = 1,size(self,2)
            self(:,j) = a(j,:)*diag(j)
         end do
      end select
   end subroutine

   PURE subroutine to_product_with_diagonal_1(self,dg,a,transpose_a)
   ! Set "self" to the matrix product of diagonal matrix "dg" (stored as a
   ! vector) and "a".  If present, "transpose_a" can be set to TRUE if "a" needs
   ! to be transposed.
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: dg
      MAT(REAL,:,:), IN :: a
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      INT :: opt,i
   ENSURE(is_same_shape_as_(self,a),"MAT{REAL}:to_product_with_diagonal_1 ... incompatible dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (trans_a) opt = 1
      select case (opt)

      case (0)  ! an
         ENSURE(size(a,1)==size(dg),"MAT{REAL}:to_product_with_diagonal_1 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*a(i,:)
         end do

      case (1)  ! at
         ENSURE(size(a,2)==size(dg),"MAT{REAL}:to_product_with_diagonal_1 ... incompatible dimensions")
         do i = 1,size(self,1)
            self(i,:) = dg(i)*a(:,i)
         end do
      end select
   end subroutine

   PURE subroutine plus_difference_product_with(self,U,ev,eo,tol)
   ! Do self += (ev(a)-eo(i)) * U(a,i).
   ! If "tol" is present, small ev/eo and (ev-eo) get set to zero
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: U
      VEC(REAL,:), IN :: ev
      VEC(REAL,:), IN :: eo
      REAL, optional, IN :: tol
      INT :: a,i
      REAL :: ea,ei,ai
   ENSURE(size(ev)==size(self,1),"MAT{REAL}:plus_difference_product_with ... wrong size, ev")
   ENSURE(size(eo)==size(self,2),"MAT{REAL}:plus_difference_product_with ... wrong size, eo")
      if (NOT present(tol)) then
         do i = 1,size(self,2)
            do a = 1,size(self,1)
               ea = ev(a)
               ei = eo(i)
               ai = ea-ei
               self(a,i) = self(a,i)+ai*U(a,i)
            end do
         end do
      else
         do i = 1,size(self,2)
            do a = 1,size(self,1)
               ea = ev(a)
               ei = eo(i)
               ai = ea-ei
               if (is_zero_(ai,tol)) ai = ZERO
               if (is_zero_(ea,tol)) ai = ZERO
               if (is_zero_(ei,tol)) ai = ZERO
               self(a,i) = self(a,i)+ai*U(a,i)
            end do
         end do
      end if
   end subroutine

   PURE subroutine plus_difference_divisor_with(self,U,ev,eo,tol)
   ! Do self += (ev(a)-eo(i))^-1 * U(a,i).
   ! If "tol" is present, small ev/eo and (ev-eo) get set to zero
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: U
      VEC(REAL,:), IN :: ev
      VEC(REAL,:), IN :: eo
      REAL, optional, IN :: tol
      INT :: a,i
      REAL :: ea,ei,ai
   ENSURE(size(ev)==size(self,1),"MAT{REAL}:plus_difference_divisor_with ... wrong size, ev")
   ENSURE(size(eo)==size(self,2),"MAT{REAL}:plus_difference_divisor_with ... wrong size, eo")
      if (NOT present(tol)) then
         do i = 1,size(self,2)
            do a = 1,size(self,1)
               ea = ev(a)
               ei = eo(i)
               ai = ea-ei
               if (is_zero_(ai,tol)) then; ai = ZERO
               else; ai = ONE/ai
               end if
               self(a,i) = self(a,i)+ai*U(a,i)
            end do
         end do
      else
         do i = 1,size(self,2)
            do a = 1,size(self,1)
               ea = ev(a)
               ei = eo(i)
               ai = ea-ei
               if (is_zero_(ai,tol)) then; ai = ZERO
               else; ai = ONE/ai
               end if
               if (is_zero_(ea,tol)) ai = ZERO
               if (is_zero_(ei,tol)) ai = ZERO
               self(a,i) = self(a,i)+ai*U(a,i)
            end do
         end do
      end if
   end subroutine


!  ================
!  Trace operations
!  ================

   pure function trace(self) result (res)
   ! Return the trace of self
      MAT(REAL,:,:), IN :: self
      REAL :: res
      INT :: i

      ! The following code is inherited from MAT{INTRINSIC}
      res = ZERO
      do i = 1,size(self,1)
         res = res+self(i,i)
      end do
   end function

   PURE function trace_product_with_0(self,a,transpose_a) result (res)
   ! Return the trace of the product of "self" with matrix "a",
   ! and if "transpose_a" is present and TRUE, then transpose "a".
      MAT(REAL,:,:), IN :: self
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

   PURE function dot_2(self,a) result (res)
   ! Synonym for trace_product_with(a) ... Return the trace of the product of
   ! "self" with matrix "a". This is really intended for use with symmetric
   ! matrices only.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), IN :: a
      REAL :: res
! ENSURE(.is_symmetric,"self is not symmetric")
! ENSURE(a.is_symmetric,"self is not symmetric")
      res = trace_product_with_(self,a,transpose_a=TRUE)
   end function

   PURE function trace_product_with_1(self,a,b,c) result (res)
   ! Return the trace of the product of "self" with matrices "a", "b" and "c".
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), IN :: a,b,c
      REAL :: res
      MAT(REAL,:,:), allocatable :: W1,W2

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
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), IN :: a,b,c,d,e
      REAL :: res
      MAT(REAL,:,:), allocatable :: W1,W2

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

   pure function contract_with(self,v) result (res)
   ! Contract with a 1-vector.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), IN :: v
      REAL :: res

! ENSURE(.is_square,"self is not square")
! ENSURE(.dim1==v.dim,"wrong size, v")
      INT :: dim,i1,i2
      dim = size(self,1)
      res = ZERO
      do i2 = 1,dim
         do i1 = 1,dim
            res = res+self(i1,i2)*v(i1)*v(i2)
         end do
      end do
   end function


!  ========================
!  Change of basis routines
!  ========================

   PURE subroutine change_basis_using_0(self,V)
   ! Change the basis of "self" using vectors "V"; self = V^dagger self V
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: V
      MAT(REAL,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{REAL}:change_basis_using_0 ... Non-square self matrix")
   ENSURE(size(self,2)==size(V,1),"MAT{REAL}:change_basis_using_0 ... incompatible shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(V,2))
      call to_product_of_(W,self,V)
      call to_product_of_(self,V,W,TRANSPOSE_A=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_using_1(self,V)
   ! Change the basis of "self" using diagonal matrix "V" (stored as a vector).
   ! self = V self V
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), IN :: V
      MAT(REAL,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{REAL}:change_basis_using_1 ... non-square self matrix")
   ENSURE(size(self,1)==size(V),"MAT{REAL}:change_basis_using_1 ... incompatible shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(V),size(V))
      call to_product_with_diagonal_(W,self,V)
      call to_product_with_diagonal_(self,V,W)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_using_2(self,L,R)
   ! Change the basis of "self" using left and right matrices "L" and "R"
   ! and place the result in "new", new = L^dagger self R
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: L,R
      MAT(REAL,:,:), allocatable :: W
   ENSURE(size(self,2)==size(R,1),"MAT{REAL}:change_basis_using_2 ... incompatible sizes")
   ENSURE(size(self,1)==size(L,1),"MAT{REAL}:change_basis_using_2 ... incompatible sizes")
   ENSURE(size(self,2)==size(R,2),"MAT{REAL}:change_basis_using_2 ... incompatible sizes")
   ENSURE(size(self,1)==size(L,2),"MAT{REAL}:change_basis_using_2 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(R,2))
      call to_product_of_(W,self,R)
      call to_product_of_(self,L,W,TRANSPOSE_A=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_using_3(self,L,R)
   ! Change the basis of "self" using diagonal matrices "L" and "R" (stored as
   ! vectors).  self = L self R
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), IN :: L,R
      MAT(REAL,:,:), allocatable :: W
   ENSURE(size(self,1)==size(L) AND size(self,2)==size(R),"MAT{REAL}:change_basis_using_3 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(L),size(R))
      call to_product_with_diagonal_(W,self,R)
      call to_product_with_diagonal_(self,L,W)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_to_0(self,new,V)
   ! Change the basis of "self" using vectors "V", and place the result in
   ! "new".  new = V^dagger self V
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: new
      MAT(REAL,:,:), IN :: V
      MAT(REAL,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{REAL}:change_basis_to_0 ... Non-square self matrix")
   ENSURE(is_square_(new),"MAT{REAL}:change_basis_to_0 ... Non-square new matrix")
   ENSURE(size(V,1)==size(self,2),"MAT{REAL}:change_basis_to_0 ... wrong shapes")
   ENSURE(size(V,2)==size(new,2),"MAT{REAL}:change_basis_to_0 ... wrong shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(V,2))
      call to_product_of_(W,self,V)
      call to_product_of_(new,V,W,TRANSPOSE_A=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_to_1(self,new,V)
   ! Change the basis of "self" using vectors "V", and place the result in
   ! "new" i.e. new = V^dagger self V. This version uses only intrinsic
   ! procedures to avoid circular dependencies.
      MAT(REAL,:,:), IN :: self
      MAT(CPX,:,:), OUT :: new
      MAT(CPX,:,:), IN :: V
      MAT(CPX,:,:), allocatable :: W
      INT :: status
      INT :: i,j,k
      CPX :: val
   ENSURE(is_square_(self),"MAT{REAL}:change_basis_to_1 ... Non-square matrix")
   ENSURE(size(new,1)==size(new,2),"MAT{REAL}:change_basis_to_1 ... Non-square matrix")
   ENSURE(size(V,1)==size(self,2),"MAT{REAL}:change_basis_to_1 ... wrong shapes")
   ENSURE(size(V,2)==size(new,2),"MAT{REAL}:change_basis_to_1 ... wrong shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      allocate(W(size(self,1),size(V,2)),STAT=status)

   ! W.to_product_of(self,V)
      do i = 1,size(self,1)
         do j = 1,size(new,1)
            val = IMAGIFY(ZERO)
            do k = 1,size(self,2)
               val = val+self(i,k)*V(k,j)
            end do
            W(i,j) = val
         end do
      end do

   ! new.to_product_of(V,W, TRANSPOSE_A=TRUE)
      do i = 1,size(new,1)
         do j = 1,size(new,1)
            val = IMAGIFY(ZERO)
            do k = 1,size(self,1)
               val = val+V(k,i)*W(k,j)
            end do
            new(i,j) = val
         end do
      end do
      deallocate(W)
   end subroutine

   PURE subroutine change_basis_to_2(self,new,L,R)
   ! Change the basis of "self" using left and right matrices "L" and "R"
   ! and place the result in "new", new = L^dagger self R
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: new
      MAT(REAL,:,:), IN :: L,R
      MAT(REAL,:,:), allocatable :: W
   ENSURE(size(self,2)==size(R,1),"MAT{REAL}:change_basis_to_2 ... incompatible sizes")
   ENSURE(size(self,1)==size(L,1),"MAT{REAL}:change_basis_to_2 ... incompatible sizes")
   ENSURE(size(new,2)==size(R,2),"MAT{REAL}:change_basis_to_2 ... incompatible sizes")
   ENSURE(size(new,1)==size(L,2),"MAT{REAL}:change_basis_to_2 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(R,2))
      call to_product_of_(W,self,R)
      call to_product_of_(new,L,W,TRANSPOSE_A=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine change_basis_to_3(self,new,L,R)
   ! Change the basis of "self" using diagonal matrices "L" and "R" (stored as
   ! vectors) and place the result in "new".  new = L self R
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), OUT :: new
      VEC(REAL,:), IN :: L,R
      MAT(REAL,:,:), allocatable :: W
   ENSURE(size(self,1)==size(L),"MAT{REAL}:change_basis_to_3 ... incompatible self and L")
   ENSURE(size(self,2)==size(R),"MAT{REAL}:change_basis_to_3 ... incompatible self and R")
   ENSURE(size(new,1)==size(L),"MAT{REAL}:change_basis_to_3 ... incompatible new and L")
   ENSURE(size(new,2)==size(R),"MAT{REAL}:change_basis_to_3 ... incompatible new and R")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(L),size(R))
      call to_product_with_diagonal_(W,self,R)
      call to_product_with_diagonal_(new,L,W)
      call destroy_(W)
   end subroutine

   PURE subroutine back_transform_using(self,V)
   ! Back transform "self" using vectors "V", and place the result in "self".
   ! self = V self V^dagger
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: V
      MAT(REAL,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{REAL}:back_transform_using ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(V,1),size(self,2))
      call to_product_of_(W,V,self)
      call to_product_of_(self,W,V,TRANSPOSE_B=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine back_transform_to_0(self,new,V)
   ! Back transform "self" using vectors "V", and place the result in "new".
   ! new = V self V^dagger
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: new
      MAT(REAL,:,:), IN :: V
      MAT(REAL,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{REAL}:back_transform_to_0 ... non-square matrix")
   ENSURE(is_square_(new),"MAT{REAL}:back_transform_to_0 ... non-square matrix")
   ENSURE(size(V,2)==size(self,1),"MAT{REAL}:back_transform_to_0 ... incompatible sizes")
   ENSURE(size(V,1)==size(new,1),"MAT{REAL}:back_transform_to_0 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(V,1),size(self,2))
      call to_product_of_(W,V,self)
      call to_product_of_(new,W,V,TRANSPOSE_B=TRUE)
      call destroy_(W)
   end subroutine

   PURE subroutine back_transform_to_1(self,new,V)
   ! Back transform "self" using vectors "V", and place the result in "new".
   ! i.e. new = V self V^dagger. This version uses only intrinsic calls to avoid
   ! circular dependencies.
      MAT(REAL,:,:), IN :: self
      MAT(CPX,:,:), OUT :: new
      MAT(CPX,:,:), IN :: V
      MAT(CPX,:,:), allocatable :: W
      INT :: i,j,k
      CPX :: val
   ENSURE(is_square_(self),"MAT{REAL}:back_transform_to_1 ... non-square matrix")
   ENSURE(size(new,1)==size(new,2),"MAT{REAL}:back_transform_to_1 ... Non-square matrix")
   ENSURE(size(V,2)==size(self,1),"MAT{REAL}:back_transform_to_1 ... incompatible sizes")
   ENSURE(size(V,1)==size(new,1),"MAT{REAL}:back_transform_to_1 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      allocate(W(size(V,1),size(self,2)))

   ! W.to_product_of(V,self)
      do i = 1,size(new,1)
         do j = 1,size(self,1)
            val = IMAGIFY(ZERO)
            do k = 1,size(self,1)
               val = val+V(i,k)*self(k,j)
            end do
            W(i,j) = val
         end do
      end do

   ! new.to_product_of(W,V, TRANSPOSE_B=TRUE)
      do j = 1,size(new,1)
         do i = 1,size(new,1)
            val = IMAGIFY(ZERO)
            do k = 1,size(self,1)
               val = val+W(i,k)*V(j,k)
            end do
            new(i,j) = val
         end do
      end do

      ! Clean
      deallocate(W)
   end subroutine

   PURE subroutine back_transform_to_2(self,new,L,R)
   ! Back transform "self" using left and right matrices "L" and "R"
   ! and place the result in "new", new = L self R^dagger
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: new
      MAT(REAL,:,:), IN :: L,R
      MAT(REAL,:,:), allocatable :: W
   ENSURE(size(self,2)==size(R,2),"MAT{REAL}:back_transform_to_2 ... incompatible sizes")
   ENSURE(size(self,1)==size(L,2),"MAT{REAL}:back_transform_to_2 ... incompatible sizes")
   ENSURE(size(new,2)==size(R,1),"MAT{REAL}:back_transform_to_2 ... incompatible sizes")
   ENSURE(size(new,1)==size(L,1),"MAT{REAL}:back_transform_to_2 ... incompatible sizes")

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(W,size(self,1),size(R,1))
      call to_product_of_(W,self,R,TRANSPOSE_B=TRUE)
      call to_product_of_(new,L,W)
      call destroy_(W)
   end subroutine

   subroutine similarity_transform(self,V)
   ! Do a similarity transform of "self" using vectors "V": self = V self V^-1
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: V
      MAT(REAL,:,:), allocatable :: V1,W
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:similarity_transform ... Non-square matrix")
   ENSURE(is_square_(V),"MAT{REAL}:similarity_transform ... Non-square matrix")
   ENSURE(size(self,1)==size(V,1),"MAT{REAL}:similarity_transform ... wrong shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)
      call create_(V1,n,n)
      call to_inverse_of_(V1,V)
      call create_(W,n,n)
      call to_product_of_(W,self,V1)
      call to_product_of_(self,V,W)
      call destroy_(W)
      call destroy_(V1)
   end subroutine


!  ==============
!  ADP comparison
!  ==============

   function S12_with_ADP(self,U2,U1err,U2err,S12err) result (res)
   ! Return the determinant a 3x3 matrix
      MAT(REAL,:,:), IN :: self
      MAT(REAL,3,3), IN :: U2
      VEC(REAL,6), optional, IN :: U1err,U2err
      REAL, optional, OUT :: S12err
      REAL :: res
      MAT(REAL,3,3) :: U1,U12
      MAT(REAL,:,:), allocatable :: a1,a2,a12
      REAL :: d1,d2,d12,R12,fac
      INT :: i,j,k
   ENSURE(present(U1err) EQV present(U2err),"MAT{REAL}:S12_with_ADP ... both errors must be present")

      ! Get inverses
      U1 = self
      U12 = U1+U2

      ! Determinants
      d1 = sqrt(determinant_(U1))
      d2 = sqrt(determinant_(U2))
      d12 = ONE/determinant_(U12)

      ! Result
      fac = d1*d2*d12
      R12 = sqrt(EIGHT*fac)
      res = 100d0*(ONE-R12)

      ! Get the error in S12
      if (NOT (present(U1err) AND present(U2err) AND present(S12err))) return
      k = 0
      S12err = ZERO
      do i = 1,3
         do j = 1,j
            k = k+1
            a1 = adjugate_(U1,i,j)
            a2 = adjugate_(U2,i,j)
            a12 = adjugate_(U12,i,j)
            S12err = S12err+determinant_(a1)*U1err(k)
            S12err = S12err+determinant_(a2)*U2err(k)
            S12err = S12err-determinant_(a12)*(U1err(k)+U2err(k))*d12*TWO
         end do
      end do
      S12err = (TWO*100d0*fac/R12)*S12err
   end function


!  ===============================
!  Tensor change of basis routines
!  ===============================

   PURE subroutine to_tensor_change_basis_of(self,V,reorder)
   ! If "V" is a matrix used to change basis for a symmetric
   ! matrix:
   !   e.g. new = V^dagger old V
   ! Then set "self" to the matrix representing the above
   ! transformation in terms of the lower triangle of
   ! "old" treated as a vector. If "reorder" is present
   ! and TRUE then the usual lower triangular order is
   ! changed to 11,22,33,12,13,23 used in crystallography
   ! NOTE: see also GAUSSIAN:d_xyz_rep_mx_for(R)
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), IN :: V
      BIN, optional, IN :: reorder
      VEC(INT,6) :: new
      INT :: n,ab,a,b,cd,c,d,nab,ncd
      BIN :: swap
   ENSURE(is_square_(self),"MAT{REAL}:to_tensor_change_basis_of ... self is non-square matrix")
   ENSURE(is_square_(V),"MAT{REAL}:to_tensor_change_basis_of ... V is non-square matrix")
   ENSURE(size(self,1)==triangle_number_(size(V,1)),"MAT{REAL}:to_tensor_change_basis_of ... Incompatible matrices")
      swap = FALSE
      if (present(reorder)) swap = reorder
      new = [1,4,2,5,6,3]
      n = size(V,1)
      self = ZERO
      if (NOT swap) then

         ! This does: M'_ab = sum_cd V_ca M_cd V_db
         ab = 0
         do a = 1,n
            do b = 1,a
               ab = ab+1
               cd = 0
               do c = 1,n
                  do d = 1,c
                     cd = cd+1
                     self(ab,cd) = V(c,a)*V(d,b)
                     if (c==d) cycle
                     self(ab,cd) = self(ab,cd)+V(d,a)*V(c,b)
                  end do
               end do
            end do
         end do
      else

         ! This does: M'_ab = sum_cd V_ca M_cd V_db
      ! DIE_IF(n/=3,"V must be 3x3")
         ab = 0
         do a = 1,n
            do b = 1,a
               ab = ab+1
               nab = new(ab)
               cd = 0
               do c = 1,n
                  do d = 1,c
                     cd = cd+1
                     ncd = new(cd)
                     self(nab,ncd) = V(c,a)*V(d,b)
                     if (c==d) cycle
                     self(nab,ncd) = self(nab,ncd)+V(d,a)*V(c,b)
                  end do
               end do
            end do
         end do
      end if
   end subroutine

   PURE subroutine to_tensor_back_transform_of(self,V,reorder)
   ! If "V" is a matrix used to change basis for a symmetric
   ! matrix:
   !   e.g. new = V old V^dagger
   ! Then set "self" to the matrix representing the above
   ! transformation in terms of the lower triangle of
   ! "old" treated as a vector. If "reorder" is present
   ! and TRUE then the usual lower triangular order is
   ! changed to 11,22,33,12,13,23 used in crystallography
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), IN :: V
      BIN, optional, IN :: reorder
      VEC(INT,6) :: new
      INT :: n,ab,a,b,cd,c,d,nab,ncd
      BIN :: swap
   ENSURE(is_square_(self),"MAT{REAL}:to_tensor_back_transform_of ... self is non-square matrix")
   ENSURE(is_square_(V),"MAT{REAL}:to_tensor_back_transform_of ... V is non-square matrix")
   ENSURE(size(self,1)==triangle_number_(size(V,1)),"MAT{REAL}:to_tensor_back_transform_of ... Incompatible matrices")
      swap = FALSE
      if (present(reorder)) swap = reorder
      new = [1,4,2,5,6,3]
      n = size(V,1)
      self = ZERO
      if (NOT swap) then

         ! This does: M'_ab = sum_cd V_ac M_cd V_bd
         ab = 0
         do a = 1,n
            do b = 1,a
               ab = ab+1
               cd = 0
               do c = 1,n
                  do d = 1,c
                     cd = cd+1
                     self(ab,cd) = V(a,c)*V(b,d)
                     if (c==d) cycle
                     self(ab,cd) = self(ab,cd)+V(a,d)*V(b,c)
                  end do
               end do
            end do
         end do
      else

         ! This does: M'_ab = sum_cd V_ac M_cd V_bd
         ab = 0
         do a = 1,n
            do b = 1,a
               ab = ab+1
               nab = new(ab)
               cd = 0
               do c = 1,n
                  do d = 1,c
                     cd = cd+1
                     ncd = new(cd)
                     self(nab,ncd) = V(a,c)*V(b,d)
                     if (c==d) cycle
                     self(nab,ncd) = self(nab,ncd)+V(a,d)*V(b,c)
                  end do
               end do
            end do
         end do
      end if
   end subroutine


!  ==========================
!  Operations on the diagonal
!  ==========================

   pure function diagonal(self) result (res)
   ! Return the diagonal of "self". 
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), allocatable :: res
      INT :: i,dim

      ! The following code is inherited from MAT{INTRINSIC}
      dim = min(size(self,1),size(self,2))
      allocate(res(dim))
      do i = 1,dim
         res(i) = self(i,i)
      end do
   end function

   PURE subroutine set_from_diagonal(self,d)
   ! Converts the diagonal vector "d" to matrix "self".
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: d
      INT :: i
   ENSURE(is_square_(self),"MAT{REAL}:set_from_diagonal ... non-square matrix")
   ENSURE(size(d)==size(self,1),"MAT{REAL}:set_from_diagonal ... incompatibale diagonal length")

      ! The following code is inherited from MAT{INTRINSIC}
      self = ZERO
      do i = 1,size(d)
         self(i,i) = d(i)
      end do
   end subroutine

   PURE subroutine set_diagonal_to_0(self,d)
   ! Set the diagonal of "self" to vector "d"
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), IN :: d
      INT :: i
   ENSURE(is_square_(self),"MAT{REAL}:set_diagonal_to_0 ... non-square matrix")
   ENSURE(size(d)==size(self,1),"MAT{REAL}:set_diagonal_to_0 ... incompatibale diagonal length")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(d)
         self(i,i) = d(i)
      end do
   end subroutine

   PURE subroutine set_diagonal_to_1(self,val)
   ! Set the diagonal of "self" to "val"
      MAT(REAL,:,:), INOUT :: self
      REAL, IN :: val
      INT :: i
   ENSURE(is_square_(self),"MAT{REAL}:set_diagonal_to_1 ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = val
      end do
   end subroutine

   PURE subroutine put_diagonal_to(self,d)
   ! Get the diagonal elements of "self" in vector "d"
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), OUT :: d
      INT :: i
   ENSURE(size(d)==min(size(self,1),size(self,2)),"MAT{REAL}:put_diagonal_to ... d is incompatible")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(d)
         d(i) = (self(i,i))
      end do
   end subroutine

   PURE subroutine increment_diagonal_by(self,val)
   ! Add "val" to the diagonal of "self"
      MAT(REAL,:,:), INOUT :: self
      REAL, IN :: val
      INT :: i
   ENSURE(is_square_(self),"MAT{REAL}:increment_diagonal_by ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = self(i,i)+val
      end do
   end subroutine

   PURE subroutine scale_diagonal_by(self,fac)
   ! Weight the diagonal elements of "self" by "fac"
      MAT(REAL,:,:), INOUT :: self
      REAL, IN :: fac
      INT :: i
   ENSURE(is_square_(self),"MAT{REAL}:scale_diagonal_by ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = fac*self(i,i)
      end do
   end subroutine

   PURE subroutine zero_diagonal(self)
   ! Zero the diagonal elements of "self"
      MAT(REAL,:,:), INOUT :: self
      INT :: i
   ENSURE(is_square_(self),"MAT{REAL}:zero_diagonal ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         self(i,i) = ZERO
      end do
   end subroutine

   PURE subroutine zero_off_diagonal(self)
   ! Zero the off diagonal elements of "self"
      MAT(REAL,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{REAL}:zero_off_diagonal ... non-square matrix")

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
      MAT(REAL,:,:), IN :: self
      REAL :: res
      INT :: dim,i
   ENSURE(min(size(self,1),size(self,2))>0,"MAT{REAL}:max_diagonal_element ... cannot have zero sized dimensions")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = min(size(self,1),size(self,2))
      res = self(1,1)
      do i = 2,dim
         res = max(self(i,i),res)
      end do
   end function

   PURE function max_abs_diagonal_element(self) result (res)
   ! Get the maximum absolute value of the diagonal elements of the self matrix
      MAT(REAL,:,:), IN :: self
      REAL :: res
      INT :: dim,i
   ENSURE(min(size(self,1),size(self,2))>0,"MAT{REAL}:max_abs_diagonal_element ... cannot have zero sized dimensions")

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
      MAT(REAL,:,:), INOUT :: self
      INT :: i,j
      REAL :: val
   ENSURE(is_square_(self),"MAT{REAL}:symmetrize ... non-square matrix")

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
      MAT(REAL,:,:), INOUT :: self
      INT :: i,j
      REAL :: val
   ENSURE(is_square_(self),"MAT{REAL}:antisymmetrize ... non-square matrix")

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
      MAT(REAL,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{REAL}:symmetric_fold ... non-square matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,i-1
            self(i,j) = self(i,j)+self(j,i)
         end do
      end do
   end subroutine

   PURE subroutine antisymmetric_fold(self)
   ! Subtract the upper triangle of "self" into the lower triangle
      MAT(REAL,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{REAL}:antisymmetric_fold ... non-square matrix")

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
      MAT(REAL,:,:), INOUT :: self
      BIN, optional, IN :: set_upper
      BIN, optional, IN :: set_lower
      INT :: i,j
      BIN :: upper
   ENSURE(is_square_(self),"MAT{REAL}:symmetric_reflect ... non-square matrix")
   ENSURE(NOT (present(set_upper) AND present(set_lower)),"MAT{REAL}:symmetric_reflect ... both options present!")

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
      MAT(REAL,:,:), INOUT :: self
      INT :: i,j
   ENSURE(is_square_(self),"MAT{REAL}:antisymmetric_reflect ... non-square matrix")

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
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), OUT :: tr
      INT :: i,j,ij
   ENSURE(is_square_(self),"MAT{REAL}:symmetric_fold_to_triangle ... non-square matrix")
   ENSURE(size(tr)>=tri_size_(self),"MAT{REAL}:symmetric_fold_to_triangle ... triangle array too small")

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


!  ======================================
!  Compression and uncompression routines
!  ======================================

   pure subroutine zip_mx_to(self,v)
   ! Converts matrix "self" to the vector "v", where vectors are
   ! stored in column order
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), OUT :: v
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
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: v
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

   pure subroutine zip_lower_triangle_to(self,tr)
   ! Converts the lower triangle of "self" to the vector "tr"
   ! using row order.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), OUT :: tr
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
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: tr
      REAL :: tmp
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

   pure subroutine antisymmetric_unzip_triangle(self,tr)
   ! Converts the triangle "tr" into the antisymmetric matrix "self".
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: tr
      REAL :: tmp
      INT :: dim,i,j,ij

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      ij = 0
      do i = 1,dim
         do j = 1,i
            ij = ij+1
            tmp = tr(ij)
            self(j,i) = -tmp
            self(i,j) = tmp
         end do
         self(i,i) = ZERO
      end do
   end subroutine

   pure subroutine unzip_lower_triangle(self,tr)
   ! Converts the triangle "tr" into a lower triangular matrix "self".
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: tr
      REAL :: tmp
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

   pure subroutine unzip_upper_triangle(self,tr)
   ! Converts the triangle "tr" into a upper triangular matrix "self".
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: tr
      REAL :: tmp
      INT :: i,j,ij

      ! The following code is inherited from MAT{INTRINSIC}
      self = ZERO
      ij = 0
      do i = 1,size(self,1)
         do j = 1,i
            ij = ij+1
            tmp = tr(ij)
            self(j,i) = tmp
         end do
      end do
   end subroutine

   pure subroutine unzip_triangles(self,lower,upper)
   ! Extract "self" from the "lower" and "upper" triangles.
   ! NOTE: Not checked that the "lower" and "upper" parts
   ! have the samne diagonals!
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: lower,upper
      INT :: i,j,ij
      ij = 0
      do i = 1,size(self,1)
         do j = 1,i
            ij = ij+1
            self(i,j) = lower(ij)
            self(j,i) = upper(ij)
         end do
      end do
   end subroutine

   pure function tri_size(self) result (ltr)
   ! Returns the size of the lower triangle needed to store self.
      MAT(REAL,:,:), IN :: self
      INT :: ltr
      INT :: dim

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      ltr = dim*(dim+1)/2
   end function

   pure subroutine flatten(self,vec)
   ! flatten  a matrix into a vector
   ! meant for singlet bra at present
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), OUT :: vec
      INT :: o,p
      INT :: counter
      counter = 0
      do o = 1,size(self,1)
         counter = counter+1
         vec(counter) = self(o,o)
      end do
      do o = 1,size(self,1)
         do p = 1,o-1
            counter = counter+1
            vec(counter) = self(o,p)
         end do
      end do
   end subroutine

   pure subroutine unflatten(self,vec_mat)
   ! unflatten a matrix into a vector of matrices
   ! meant for singlet contraction_wfs at present
      MAT(REAL,:,:), IN :: self
      VEC(type(EMAT_REAL_TYPE),:), OUT :: vec_mat
      INT :: n,o,p,dim1,dim
      INT :: counter
      REAL :: val
      dim = size(self,1)
      dim1 = int(sqrt(2*size(self,2)+QUARTER)-HALF)
      ! DIE_IF(vec_mat.dim/=dim,"vec_mat.dim/=self.dim1")
      ! DIE_IF(vec_mat(1).element.dim2/=dim1,"vec_mat(1).element.dim2/=dim1")
      do n = 1,dim
         counter = 0
         do o = 1,dim1
            counter = counter+1
            vec_mat(n)%element(o,o) = self(n,counter)
         end do
         do o = 1,dim1
            do p = 1,o-1
               counter = counter+1
               val = self(n,counter)/sqrt(TWO)
               vec_mat(n)%element(o,p) = val
               vec_mat(n)%element(p,o) = val
            end do
         end do
      end do
   end subroutine

   pure subroutine unflatten_triplets(self,vec_mat)
   ! unflatten a matrix into a vector of matrices
   ! meant for triplet contraction_wfs at present
      MAT(REAL,:,:), INOUT :: self
      VEC(type(EMAT_REAL_TYPE),:), INOUT :: vec_mat
      INT :: n,o,p,dim1,dim
      INT :: counter
      REAL :: val
      dim = size(self,1)
      dim1 = int(sqrt(2*size(self,2)+QUARTER)+HALF)
      ! DIE_IF(vec_mat.dim/=dim,"vec_mat.dim/=self.dim1")
      ! DIE_IF(vec_mat(1).element.dim2/=dim1,"vec_mat(1).element.dim2/=dim1")
      self = self/sqrt(TWO)
      do n = 1,dim
         counter = 0
         do o = 1,dim1-1
            do p = o+1,dim1
               counter = counter+1
               val = self(n,counter)
               vec_mat(n)%element(o,p) = val
               vec_mat(n)%element(p,o) = -val
            end do
         end do
      end do
   end subroutine


!  =================
!  Orthogonalisation
!  =================

   PURE subroutine normalize(self,S)
   ! Normalise the column vectors in "self" using "S" as the metric.
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: S
      INT :: n
      REAL :: norm,norm2
   ENSURE(is_square_(self),"MAT{REAL}:normalize ... non-square matrix")
   ENSURE(is_same_shape_as_(self,S),"MAT{REAL}:normalize ... not same shape as S")
   ENSURE(NOT is_zero_(S),"MAT{REAL}:normalize ... S is zero matrix")
   ENSURE(NOT is_zero_(self),"MAT{REAL}:normalize ... self is zero matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do n = 1,size(self,2)
         norm2 = dot_(S,self(:,n),self(:,n))
         norm = sqrt(norm2)
         if (norm2>ZERO) self(:,n) = self(:,n)/norm
      end do
   end subroutine

   function is_linearly_dependent(self,S,tol,col) result (res)
   ! Return TRUE if the columns are linearly dependent with respect to "S".
   ! If present, "tol" is the tolerance used to establish linear dependency.
   ! If present, "col" is the column number where the dependence was first
   ! noticed when Schmidt orthogonalising is used, starting from column 1.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), IN :: S
      REAL, optional, IN :: tol
      INT, optional, OUT :: col
      BIN :: res
      MAT(REAL,:,:), allocatable, target :: cols
      VEC(REAL,:), PTR :: old,new
      REAL :: fac
      INT :: n,o
      REAL :: test,norm,norm0
   ENSURE(is_square_(S),"MAT{REAL}:is_linearly_dependent ... S must be square matrix")
   ENSURE(size(S,1)==size(self,1),"MAT{REAL}:is_linearly_dependent ... incompatible metric S")
   ENSURE(NOT is_zero_(S),"MAT{REAL}:is_linearly_dependent ... S is zero matrix")
   ENSURE(NOT is_zero_(self),"MAT{REAL}:is_linearly_dependent ... self is zero matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      test = MAT_LINEAR_DEPENDENCE_TOL
      if (present(tol)) test = tol
      res = FALSE
      cols = self
      do n = 1,size(self,2)
         new => cols(:,n)
         norm0 = dot_(S,new,new)
         ENSURE(norm0>=ZERO,"MAT{REAL}:is_linearly_dependent ... length of column "//trim(to_str_(n))//"is negative")
         norm0 = sqrt(dot_(S,new,new))
         do o = 1,n-1
            old => cols(:,o)
            fac = dot_(S,old,new)
            new = new-fac*old
         end do
         norm = dot_(S,new,new)
         ENSURE(norm>=ZERO,"MAT{REAL}:is_linearly_dependent ... length of column "//trim(to_str_(n))//"is negative")
         norm = sqrt(norm)
         if ((norm/norm0)<test) then
            res = TRUE
            if (present(col)) col = n
            return
         end if
         norm = ONE/norm
         new = new*norm
      end do
      call destroy_(cols)
   end function

   PURE subroutine Schmidt_orthonormalize_0(self,lambda)
   ! Schmidt orthonormalize the column vectors in "self".
   ! If the eigenvalue (lambda) of a vector is less than a cutoff,
   ! then that vector is chosen to be an orthonormal component.
   ! Eigenvalues must be sorted largest to smallest.
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), IN :: lambda
      REAL :: fac,norm
      INT :: dim1,dim2,n,k,x,y,j
   ENSURE(size(lambda)>=size(self,2),"MAT{REAL}:Schmidt_orthonormalize_0 ... not enough eigenvalues")
   ENSURE(size(self,1)>=size(self,2),"MAT{REAL}:Schmidt_orthonormalize_0 ... more vectors than dimension of vector space")
      dim1 = size(self,1)
      dim2 = size(self,2)

      ! y is set to the first vanishing eigenvalue.
      y = dim2+1
      do x = 1,dim2
         if (lambda(x)<TOL(10)) then
            y = x
            exit
         end if
      end do

      ! Schmidt orthogonalisation.
      do n = 1,y-1
         do k = 1,n-1
            fac = dot_product(self(:,k),self(:,n))
            self(:,n) = self(:,n)-fac*self(:,k)
         end do
         norm = dot_product(self(:,n),self(:,n))
         ENSURE(norm>TOL(10),"MAT{REAL}:Schmidt_orthonormalize_0 ... linear dependence, vector "//to_str_(n))
         self(:,n) = self(:,n)*(ONE/sqrt(norm))
      end do

      ! Make up some orthogonal vectors for
      ! the vanishing eigenvalues.
      do n = y,dim2
         do j = 1,dim1
            self(:,n) = ZERO
            self(j,n) = ONE
            do k = 1,n-1
               fac = dot_product(self(:,k),self(:,n))
               self(:,n) = self(:,n)-fac*self(:,k)
            end do
            norm = dot_product(self(:,n),self(:,n))
            if (norm>TOL(10)) then

            ! Found an orthogonal vector
               self(:,n) = self(:,n)*(ONE/sqrt(norm))
            end if
         end do
      end do
   end subroutine

   PURE subroutine classical_Gram_Schmidt_QR(self,Q,R)
   ! Decompose m x n matrix "self"  as Q.R where
   ! Q is an orthogonal m x n, and an R is an
   ! upper triangular matrix, using the classical
   ! Gram-Schmidt algorithm.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: Q
      MAT(REAL,:,:), OUT :: R

      ! q_ represent the unnormalized columns of Q
      ! u is used to aggreagate the multiplications
      VEC(REAL,:), allocatable :: q_,u
      INT :: dim1,dim2,i,j
   ENSURE(size(self,1)>=size(self,2),"MAT{REAL}:classical_Gram_Schmidt_QR ... more vectors than dimension of vector space")
      dim1 = size(self,1)
      dim2 = size(self,2)

      ! q_ has the same size as the columns of the input 
      call create_(q_,dim1)
      call create_(u,dim1)
      u = ZERO
      do j = 1,dim2

         ! Because self(:,j) = Q(:,i)*R(i,j),
         ! the information can be stored in Q and R
         do i = 1,j-1
            R(i,j) = dot_product(Q(:,i),self(:,j))
         end do

         ! Aggregates information of all other columns
         do i = 1,j-1
            u = u+R(i,j)*Q(:,i)
         end do

         ! Makes this column orthonormal to all other columns
         q_ = self(:,j)-u
         u = ZERO
         R(j,j) = norm2(q_)
         Q(:,j) = q_/R(j,j)
      end do

      ! Clean
      call destroy_(u)
      call destroy_(q_)
   end subroutine

   PURE subroutine modified_Gram_Schmidt_QR_0(self,Q,R)
   ! Decompose m x n matrix "self" into factors Q.R where:
   ! - "Q" is an m x n  orthonormal matrix, and 
   ! - "R" is an n x n upper triangular matrix
   ! using the modified Gram-Schmidt algorithm.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: Q
      MAT(REAL,:,:), OUT :: R
      MAT(REAL,:,:), allocatable :: A
      INT :: j,k
   ENSURE(size(self,1)>=size(self,2),"MAT{REAL}:modified_Gram_Schmidt_QR_0 ... more vectors than dimension of vector space")
   ENSURE(is_same_shape_as_(self,Q),"MAT{REAL}:modified_Gram_Schmidt_QR_0 ... Wrong shape, Q")
   ENSURE(is_square_(R),"MAT{REAL}:modified_Gram_Schmidt_QR_0 ... R is not square!")
   ENSURE(size(R,1)==size(self,2),"MAT{REAL}:modified_Gram_Schmidt_QR_0 ... Wrong size, R")
      R = ZERO
      A = self
      do k = 1,size(self,2)

         ! Normalises Q
         R(k,k) = norm2(A(:,k))
         Q(:,k) = A(:,k)/R(k,k)

         ! Removes this vector from every proceeding column
         do j = k+1,size(self,2)
            R(k,j) = dot_product(Q(:,k),A(:,j))
            A(:,j) = A(:,j)-R(k,j)*Q(:,k)
         end do
      end do

      ! Clean
      call destroy_(A)
   end subroutine

   PURE subroutine modified_Gram_Schmidt_QR_1(self)
   ! Decompose m x n matrix "self" into factors Q.R where:
   ! - "Q" is an m x n  orthonormal matrix, and 
   ! - "R" is an n x n upper triangular matrix
   ! using the modified Gram-Schmidt algorithm.
   ! This version replaces "self" by "Q".
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), allocatable :: A,Q,R
      INT :: j,k
   ENSURE(size(self,1)>=size(self,2),"MAT{REAL}:modified_Gram_Schmidt_QR_1 ... more vectors than dimension of vector space")
      call create_(Q,size(self,1),size(self,2))
      call create_(R,size(self,2),size(self,2))
      R = ZERO
      A = self
      do k = 1,size(self,2)

         ! Normalises Q
         R(k,k) = norm2(A(:,k))
         Q(:,k) = A(:,k)/R(k,k)

         ! Removes this vector from every proceeding column
         do j = k+1,size(self,2)
            R(k,j) = dot_product(Q(:,k),A(:,j))
            A(:,j) = A(:,j)-R(k,j)*Q(:,k)
         end do
      end do
      self = Q

      ! Clean
      call destroy_(A)
      call destroy_(R)
      call destroy_(Q)
   end subroutine

   PURE subroutine Householder_QR_0(self,Q,R)
   ! Given an m x n matrix "self" decomposes it into Q.R where:
   ! - "Q" is an m x n orthonormal matrix, and
   ! - "R" is an n x n upper triangular matrix
   ! using the Householder algorithm.
   ! See Golub & van Loan p. 224 & 211, especially comments
   ! regarding how to deal with the outer product efficiently.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: Q
      MAT(REAL,:,:), OUT :: R

      ! nu is the Householder vector 
      ! b stores all the beta values
      VEC(REAL,:), allocatable :: nu,b

      ! A stores R and the Householder vectors. 
      MAT(REAL,:,:), allocatable :: A
      REAL :: beta
      INT :: dim1,dim2,j
   ENSURE(size(self,1)>=size(self,2),"MAT{REAL}:Householder_QR_0 ... more vectors than dimension of vector space")
   ENSURE(is_same_shape_as_(self,Q),"MAT{REAL}:Householder_QR_0 ... Wrong shape, Q")
   ENSURE(is_square_(R),"MAT{REAL}:Householder_QR_0 ... R is not square!")
   ENSURE(size(R,1)==size(self,2),"MAT{REAL}:Householder_QR_0 ... Wrong size, R")
      dim1 = size(self,1)
      dim2 = size(self,2)
      call create_(nu,dim1)
      call create_(A,dim1,dim2)
      call create_(b,dim2)
      R = ZERO
      A = self
      do j = 1,dim2

         ! Householder
         call Householder_vector_(A(j:,j),nu(j:),beta)
         b(j) = beta  ! store

         ! A = R from top to the diagonal, 
         ! beneath diagonal are non-nu(1) elements
         A(j:,j:) = A(j:,j:)-beta*outer_product_with_(nu(j:),matmul(nu(j:),A(j:,j:)))
         if (j<dim1) A(j+1:,j) = nu(j+1:)
      end do

      ! Upper triangle extracted.
      ! Not needed if you don't want R, see next routine ...
      R = ZERO
      do j = 1,dim2
         R(1:j,j) = A(1:j,j)
      end do

      ! Calculates Q if variable given, by aggregating the
      ! multiplication of the matrix to produce Q
      ! See G & v L p. 213 (5.1.5) but n is m, there.
      call to_unit_mx_(Q)
      nu = ONE
      do j = dim2,1,-1

         ! Extract nu from A
         if (j<dim1) nu(j+1:) = A(j+1:,j)

         ! Apply Householder
         beta = b(j)
         Q(j:,j:) = Q(j:,j:)-beta*outer_product_with_(nu(j:),matmul(nu(j:),Q(j:,j:)))
      end do

      ! Clean
      call destroy_(b)
      call destroy_(A)
      call destroy_(nu)
   end subroutine

   PURE subroutine Householder_QR_1(self,Q)
   ! Given an m x n matrix "self" decomposes it into Q.R where:
   ! - "Q" is an m x n orthonormal matrix, and
   ! - "R" is an n x n upper triangular matrix
   ! by the Householder method. This version doesn't give "R".
   ! See Golub & van Loan p. 224 & 211, especially comments
   ! regarding how to deal with the outer product efficiently.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: Q

      ! nu is the Householder vector 
      ! b stores all the beta values
      VEC(REAL,:), allocatable :: nu,b

      ! A stores R and the Householder vectors. 
      MAT(REAL,:,:), allocatable :: A
      REAL :: beta
      INT :: dim1,dim2,j
   ENSURE(size(self,1)>=size(self,2),"MAT{REAL}:Householder_QR_1 ... more vectors than dimension of vector space")
   ENSURE(is_same_shape_as_(self,Q),"MAT{REAL}:Householder_QR_1 ... Wrong shape, Q")
      dim1 = size(self,1)
      dim2 = size(self,2)
      call create_(nu,dim1)
      call create_(A,dim1,dim2)
      call create_(b,dim2)
      A = self
      do j = 1,dim2

         ! Householder
         call Householder_vector_(A(j:,j),nu(j:),beta)
         b(j) = beta  ! store

         ! A = R from top to the diagonal, 
         ! beneath diagonal are non-nu(1) elements
         A(j:,j:) = A(j:,j:)-beta*outer_product_with_(nu(j:),matmul(nu(j:),A(j:,j:)))
         if (j<dim1) A(j+1:,j) = nu(j+1:)
      end do

      ! Calculates Q if variable given, by aggregating the
      ! multiplication of the matrix to produce Q
      ! See G & v L p. 213 (5.1.5) but n is m, there.
      call to_unit_mx_(Q)
      nu = ONE
      do j = dim2,1,-1

         ! Extract nu from A
         if (j<dim1) nu(j+1:) = A(j+1:,j)

         ! Apply Householder
         beta = b(j)
         Q(j:,j:) = Q(j:,j:)-beta*outer_product_with_(nu(j:),matmul(nu(j:),Q(j:,j:)))
      end do

      ! Clean
      call destroy_(b)
      call destroy_(A)
      call destroy_(nu)
   end subroutine

   PURE subroutine Householder_QR_2(self)
   ! Given an m x n matrix "self" decomposes it into Q.R where:
   ! - "Q" is an m x n orthonormal matrix, and
   ! - "R" is an n x n upper triangular matrix
   ! by the Householder method and replace "self" with "Q".
   ! See Golub & van Loan p. 224 & 211, especially comments
   ! regarding how to deal with the outer product efficiently.
      MAT(REAL,:,:), INOUT :: self

      ! nu is the Householder vector 
      ! b stores all the beta values
      VEC(REAL,:), allocatable :: nu,b

      ! A stores R and the Householder vectors. 
      MAT(REAL,:,:), allocatable :: A
      REAL :: beta
      INT :: dim1,dim2,j
   ENSURE(size(self,1)>=size(self,2),"MAT{REAL}:Householder_QR_2 ... more vectors than dimension of vector space")
      dim1 = size(self,1)
      dim2 = size(self,2)
      call create_(nu,dim1)
      call create_(A,dim1,dim2)
      call create_(b,dim2)
      A = self
      do j = 1,dim2

         ! Householder
         call Householder_vector_(A(j:,j),nu(j:),beta)
         b(j) = beta  ! store

         ! A = R from top to the diagonal, 
         ! beneath diagonal are non-nu(1) elements
         A(j:,j:) = A(j:,j:)-beta*outer_product_with_(nu(j:),matmul(nu(j:),A(j:,j:)))
         if (j<dim1) A(j+1:,j) = nu(j+1:)
      end do

      ! Calculates Q if variable given, by aggregating the
      ! multiplication of the matrix to produce Q
      ! See G & v L p. 213 (5.1.5) but n is m, there.
      call to_unit_mx_(self)
      nu = ONE
      do j = dim2,1,-1

         ! Extract nu from A
         if (j<dim1) nu(j+1:) = A(j+1:,j)

         ! Apply Householder
         beta = b(j)
         self(j:,j:) = self(j:,j:)-beta*outer_product_with_(nu(j:),matmul(nu(j:),self(j:,j:)))
      end do

      ! Clean
      call destroy_(b)
      call destroy_(A)
      call destroy_(nu)
   end subroutine

   PURE subroutine block_Householder_QR(self,R,Q)
   ! Given an mxn matrix decomposes the matrix into an
   ! orthogonal mxm matrix, Q, and an upper triangular mxn
   ! matrix, R, using the Householder algorithm. Q is calculated
   ! through accumulation of W and Y.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: R
      MAT(REAL,:,:), OUT :: Q
      VEC(REAL,:), allocatable :: nu,b,v
      MAT(REAL,:,:), allocatable :: A,I,W,Y,M
      REAL :: beta
      INT :: dim1,dim2,j
   ENSURE(size(self,1)>=size(self,2),"MAT{REAL}:block_Householder_QR ... more vectorst than dimension of vector space")
   ENSURE(size(Q,1)==size(self,1),"MAT{REAL}:block_Householder_QR ... Wrong size Q")
   ENSURE(size(Q,2)==size(self,1),"MAT{REAL}:block_Householder_QR ... Wrong size Q")
      dim1 = size(self,1)
      dim2 = size(self,2)
      call create_(A,dim1,dim2)
      call create_(W,dim1,dim2)
      call create_(Y,dim1,dim2)
      call create_(M,dim1,dim1)
      call create_(I,dim1,dim1)
      call create_(b,dim2)

      ! Determination of A is identical to Householder qr routine.
      A = self
      call to_unit_mx_(I)
      do j = 1,dim2
         call create_(v,dim1-j+1)
         call create_(nu,dim1-j+1)
         v = A(j:dim1,j)
         call Householder_vector_(v,nu,beta)
         b(j) = beta
         A(j:dim1,j) = v
         A(j:dim1,j:dim2) = matmul(I(1:dim1-j+1,1:dim1-j+1)-beta*outer_product_(nu),A(j:dim1,j:dim2))
         if (j<dim1) then
            A(j+1:dim1,j) = nu(2:)
         end if
         call destroy_(v)
         call destroy_(nu)
      end do

      ! Upper triangle extracted.
      do j = 1,dim2
         R(1:j,j) = A(1:j,j)
      end do
      call create_(nu,size(self,1))
      nu = ZERO

      ! Y is simply the matrix of Householder vectors with 0's before
      ! the vector if the dimension of the vector is less than the
      ! dimension of the Y matrix
      Y = ZERO
      Y(1,1) = ONE
      Y(2:dim1,1) = A(2:dim1,1)

      ! W is the Z produced for each column. The first column is
      ! simply -beta*Y(1). Y keep updating.
      W(:,1) = -b(1)*Y(:,1)
      do j = 2,dim2
         nu(j-1) = ZERO
         nu(j) = ONE
         nu(j+1:dim1) = A(j+1:dim1,j)

         ! Update W
         M = I+matmul(W(:,1:j-1),transpose(Y(:,1:j-1)))
         W(:,j) = -b(j)*matmul(M,nu)

         ! Update Y
         Y(j,j) = ONE
         Y(j+1:dim1,j) = A(j+1:dim1,j)
      end do

      ! As per the proof from Golub and van Loan
      Q = I+matmul(W,transpose(Y))

      ! Clean
      call destroy_(b)
      call destroy_(I)
      call destroy_(M)
      call destroy_(Y)
      call destroy_(W)
      call destroy_(A)
   end subroutine

   PURE subroutine fast_Givens_QR(self,R,Q)
   ! Given an mxn matrix decomposes the matrix into an
   ! orthogonal mxm matrix, Q (if provided), and an upper 
   ! triangular mxn matrix, R, using the fast Givens algorithm.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: R
      MAT(REAL,:,:), optional, OUT :: Q
      VEC(REAL,:), allocatable :: di
      MAT(REAL,:,:), allocatable :: F,M,A,D,G
      INT :: dim1,dim2,i,j,k,t
      REAL :: alpha,beta
   ENSURE(size(self,1)>=size(self,2),"MAT{REAL}:fast_Givens_QR ... more vectors than dimension of vector space")
      dim1 = size(self,1)
      dim2 = size(self,2)
      call create_(di,dim1)
      call create_(A,dim1,dim2)
      call create_(D,dim1,dim1)
      call create_(M,dim1,dim1)
      call create_(F,dim1,dim1)
      call create_(G,2,2)
      A = self
      di = ONE
      D = ZERO
      call to_unit_mx_(M)
      call to_unit_mx_(F)

      ! If Q present, calculates the matrix M,
      ! so Q can be calculated
      if (present(Q)) then
         do j = 1,dim2
            do i = dim1,j+1,-1
               call fast_Givens_(A(i-1:i,j),di(i-1:i),alpha,beta,t)

            ! F is the scaled to dimension mxm matrix
            ! version of the 2x2 Givens transform 
            ! matrix
               F = ZERO
               do k = 1,dim1
                  F(k,k) = ONE
               end do
               if (t==1) then

               ! F and G are determined by the type variable given
               ! from the fast givens transform         
                  F(i-1,i-1) = beta
                  F(i,i) = alpha
                  F(i,i-1) = ONE
                  F(i-1,i) = ONE
                  G = ONE
                  G(1,1) = beta
                  G(2,2) = alpha

               ! Removes elements from the initial matrix.
                  A(i-1:i,j:dim2) = matmul(transpose(G),A(i-1:i,j:dim2))

               ! M is aggregated so Q can be calculated later         
                  M = matmul(M,F)
               else
                  G = ONE
                  F(i,i-1) = beta
                  F(i-1,i) = alpha
                  G(1,2) = alpha
                  G(2,1) = beta
                  A(i-1:i,j:dim2) = matmul(transpose(G),A(i-1:i,j:dim2))
                  M = matmul(M,F)
               end if
            end do
         end do

         ! D is calculated so Q and R can be calculated 
         do i = 1,dim1
            D(i,i) = 1/sqrt(di(i))
         end do
         call to_product_of_(R,D,A)
         call to_product_of_(Q,M,D)
      else

         ! As above, without the need to calculate Q
         do j = 1,dim2
            do i = dim1,j+1,-1
               call fast_Givens_(A(i-1:i,j),di(i-1:i),alpha,beta,t)
               if (t==1) then
                  G = ONE
                  G(1,1) = beta
                  G(2,2) = alpha
                  A(i-1:i,j:dim2) = matmul(transpose(G),A(i-1:i,j:dim2))
               else
                  G = ONE
                  G(1,2) = alpha
                  G(2,1) = beta
                  A(i-1:i,j:dim2) = matmul(transpose(G),A(i-1:i,j:dim2))
               end if
            end do
         end do
         do i = 1,dim1
            D(i,i) = 1/sqrt(di(i))
         end do
         call to_product_of_(R,D,A)
      end if
      call destroy_(di)
      call destroy_(A)
      call destroy_(D)
      call destroy_(M)
      call destroy_(F)
      call destroy_(G)
   end subroutine

   PURE subroutine Schmidt_orthonormalize_1(self,S,scales,n_dependent,ld_tol)
   ! Schmidt orthonormalize the vectors in "self" using "S" as the
   ! metric "scales" is set to the normalisation factors used to
   ! normalize each column after the Schmidt procedure. Only those
   ! vectors with norm more than the linear dependence tolerance are
   ! orthonormalized. the others are counted (n_dependent) and
   ! rejected at the end of the list.
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: S
      VEC(REAL,:), OUT :: scales
      INT, OUT :: n_dependent
      REAL, optional, IN :: ld_tol
      MAT(REAL,:,:), allocatable :: decreasing_vec
      VEC(REAL,:), allocatable :: keep
      REAL :: fac
      REAL :: tol
      REAL :: norm
      INT :: n,o,p,dim1,dim2
      VEC(BIN,:), allocatable :: skip
   ENSURE(NOT is_zero_(S),"MAT{REAL}:Schmidt_orthonormalize_1 ... S is zero matrix")
      n_dependent = 0
      tol = LINEAR_DEPENDENCE_TOL
      if (present(ld_tol)) tol = ld_tol
      dim1 = size(self,1)
      dim2 = size(self,2)
      call create_(keep,dim2)
      call create_(skip,dim1)
      skip = FALSE

      ! Eliminate function with small norms
      do n = 1,dim1
         norm = dot_(S,self(n,:),self(n,:))
         if (norm<tol) then
            n_dependent = n_dependent+1
            skip(n) = TRUE
            scales(n) = norm
         end if
      end do
      ENSURE(NOT skip(1),"MAT{REAL}:Schmidt_orthonormalize_1 ... ground state is quasilinearly dependent")
      do n = 1,dim1
         if (skip(n)) cycle
         keep = self(n,:)
         norm = dot_(S,keep,keep)
         ! Orthogonalisation
         do o = 1,n-1
            if (skip(o)) cycle
            fac = dot_(S,self(o,:),self(n,:))
            self(n,:) = self(n,:)-fac*self(o,:)
            norm = norm-fac*fac
         end do
         scales(n) = norm
         if (norm>tol) then

            ! normalisation of the orthogonalised vector
            norm = sqrt(norm)
            self(n,:) = self(n,:)/norm
         else
            n_dependent = n_dependent+1
            self(n,:) = keep
            skip(n) = TRUE
         end if
      end do
      call destroy_(keep)

      ! Reject the quasilinearly dependent vectors at the end 
      ! of the array
      o = 0
      p = 0
      call create_(decreasing_vec,dim1,dim2)
      do n = 1,dim1
         if (NOT skip(n)) then
            p = p+1
            decreasing_vec(p,:) = self(n,:)
         else
            decreasing_vec(dim1-o,:) = self(n,:)
            o = o+1
         end if
      end do
      self = decreasing_vec
      call destroy_(decreasing_vec)
      call destroy_(skip)
   end subroutine

   PURE subroutine Schmidt_orthonormalize_2(self,S,indices,scales,n_dependent,ld_tol)
   ! Schmidt orthonormalize the vectors in "self" using "S" as the
   ! metric.  "scales" is set to the normalisation factors used to
   ! normalize each column after the Schmidt procedure. Only those
   ! vectors with norm more than the linear dependence tolerance are
   ! orthonormalized. the others are counted (n_dependent) rejected at
   ! the end of the list, their new order is in indices (which makes
   ! the routine leaky).
      MAT(REAL,:,:), INOUT :: self
      VEC(INT,:), allocatable, OUT :: indices
      MAT(REAL,:,:), IN :: S
      VEC(REAL,:), OUT :: scales
      INT, OUT :: n_dependent
      REAL, optional, IN :: ld_tol
      MAT(REAL,:,:), allocatable :: decreasing_vec
      VEC(REAL,:), allocatable :: keep
      REAL :: fac,tol,norm
      INT :: n,o,p,dim1,dim2
      VEC(BIN,:), allocatable :: skip
   ENSURE(NOT is_zero_(S),"MAT{REAL}:Schmidt_orthonormalize_2 ... S is zero matrix")
      n_dependent = 0
      tol = LINEAR_DEPENDENCE_TOL
      if (present(ld_tol)) tol = ld_tol
      dim1 = size(self,1)
      dim2 = size(self,2)
      call create_(keep,dim2)
      call create_(skip,dim1)
      skip = FALSE

      ! Eliminate function with small norms
      do n = 1,dim1
         norm = dot_(S,self(n,:),self(n,:))
         if (norm<tol) then
            n_dependent = n_dependent+1
            skip(n) = TRUE
            scales(n) = norm
         end if
      end do
      ENSURE(NOT skip(1),"MAT{REAL}:Schmidt_orthonormalize_2 ... ground state is quasilinearly dependent")
      do n = 1,dim1
         if (skip(n)) cycle
         keep = self(n,:)
         norm = dot_(S,keep,keep)
         ! orthogonalisation
         do o = 1,n-1
            if (skip(o)) cycle
            fac = dot_(S,self(o,:),self(n,:))
            self(n,:) = self(n,:)-fac*self(o,:)
            norm = norm-fac*fac
         end do
         scales(n) = norm
         if (norm>tol) then

            ! Normalisation of the orthogonalised vector
            norm = sqrt(norm)
            self(n,:) = self(n,:)/norm
         else
            n_dependent = n_dependent+1
            self(n,:) = keep
            skip(n) = TRUE
         end if
      end do
      call destroy_(keep)

      ! Reject the quasilinearly dependent vectors 
      ! at the end of the array
      o = 0
      p = 0
      call create_(decreasing_vec,dim1,dim2)
      call create_(indices,dim1)
      do n = 1,dim1
         if (NOT skip(n)) then
            p = p+1
            decreasing_vec(p,:) = self(n,:)
            indices(n) = p
         else
            decreasing_vec(dim1-o,:) = self(n,:)
            indices(n) = dim1-o
            o = o+1
         end if
      end do
      self = decreasing_vec
      call destroy_(decreasing_vec)
      call destroy_(skip)
   end subroutine

   PURE subroutine Schmidt_orthonormalize_3(self,S,scales)
   ! Schmidt orthonormalize the column vectors in "self" using "S" as the
   ! metric. If "scales" is present, it is set to the
   ! normalisation factors used to normalize each column after the Schmidt
   ! procedure. Only those vectors with norm more than the linear dependence
   ! tolerance are normalized.
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: S
      VEC(REAL,:), OUT :: scales
      REAL :: fac
      INT :: n,o
      REAL :: norm,norm_sq
   ENSURE(is_square_(self),"MAT{REAL}:Schmidt_orthonormalize_3 ... non-square matrix")
   ENSURE(is_same_shape_as_(self,S),"MAT{REAL}:Schmidt_orthonormalize_3 ... not same shape as S")
   ENSURE(NOT is_zero_(S),"MAT{REAL}:Schmidt_orthonormalize_3 ... S is zero matrix")
   ENSURE(NOT is_zero_(self),"MAT{REAL}:Schmidt_orthonormalize_3 ... self is zero matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do n = 1,size(self,2)
         do o = 1,n-1
            fac = dot_(S,self(:,o),self(:,n))
            self(:,n) = self(:,n)-fac*self(:,o)
         end do
         norm_sq = dot_(S,self(:,n),self(:,n))
         norm = sqrt(norm_sq)
         scales(n) = norm
         if (norm_sq>MAT_LINEAR_DEPENDENCE_TOL) then
            self(:,n) = self(:,n)/norm
         end if
      end do
   end subroutine

   PURE subroutine Schmidt_orthonormalize_4(self,scale)
   ! Schmidt orthonormalize the column vectors in "self" metric. If
   ! "scale" is present, it is set to the product of the normalisation
   ! factors used to normalize each column after the Schmidt
   ! procedure.
      MAT(REAL,:,:), INOUT :: self
      REAL, optional, OUT :: scale
      REAL :: fac
      INT :: n,o
      REAL :: norm
   ENSURE(NOT is_zero_(self),"MAT{REAL}:Schmidt_orthonormalize_4 ... self is zero matrix")

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

   PURE subroutine Schmidt_orthonormalize_5(self,S,scale)
   ! Schmidt orthonormalize the column vectors in "self" using "S" as the
   ! metric. If "scale" is present, it is set to the product of the
   ! normalisation factors used to normalize each column after the Schmidt
   ! procedure.
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: S
      REAL, optional, OUT :: scale
      REAL :: fac
      INT :: n,o
      REAL :: norm
   ENSURE(is_square_(S),"MAT{REAL}:Schmidt_orthonormalize_5 ... S must be square matrix")
   ENSURE(size(S,1)==size(self,1),"MAT{REAL}:Schmidt_orthonormalize_5 ... incompatible metric S")
   ENSURE(NOT is_zero_(S),"MAT{REAL}:Schmidt_orthonormalize_5 ... S is zero matrix")
   ENSURE(NOT is_zero_(self),"MAT{REAL}:Schmidt_orthonormalize_5 ... self is zero matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      if (present(scale)) scale = ONE
      do n = 1,size(self,2)
         do o = 1,n-1
            fac = dot_(S,self(:,o),self(:,n))
            self(:,n) = self(:,n)-fac*self(:,o)
         end do
         norm = REALIFY(dot_(S,self(:,n),self(:,n)))
         ENSURE(norm>MAT_LINEAR_DEPENDENCE_TOL,"MAT{REAL}:Schmidt_orthonormalize_5 ... linear dependence, vector "//to_str_(n))
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

   PURE subroutine Schmidt_orthonormalize_6(self,S,from,to)
   ! Schmidt orthonormalize the *column* vectors in "self" using "S" as the
   ! metric. Only the columns starting from index "from" are orthonormalized,
   ! and further, those columns are only orthonormalized to first columns, up to
   ! the column with index "to".
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: S
      INT, IN :: from,to
      REAL :: fac
      INT :: n,o
      REAL :: norm
   ENSURE(is_square_(S),"MAT{REAL}:Schmidt_orthonormalize_6 ... S must be square matrix")
   ENSURE(size(S,1)==size(self,1),"MAT{REAL}:Schmidt_orthonormalize_6 ... incompatible metric S")
   ENSURE(NOT is_zero_(S),"MAT{REAL}:Schmidt_orthonormalize_6 ... S is zero matrix")
   ENSURE(NOT is_zero_(self),"MAT{REAL}:Schmidt_orthonormalize_6 ... self is zero matrix")
   ENSURE(from>to,"MAT{REAL}:Schmidt_orthonormalize_6 ... from must be greater than to")
   ENSURE(from<=size(self,2),"MAT{REAL}:Schmidt_orthonormalize_6 ... from to large")

      ! The following code is inherited from MAT{INTRINSIC}
      do n = from,size(self,2)
         do o = 1,to
            fac = dot_(S,self(:,o),self(:,n))
            self(:,n) = self(:,n)-fac*self(:,o)
         end do
         norm = REALIFY(dot_(S,self(:,n),self(:,n)))
         ENSURE(norm>MAT_LINEAR_DEPENDENCE_TOL,"MAT{REAL}:Schmidt_orthonormalize_6 ... linear dependence, vector "//to_str_(n))
         norm = ONE/sqrt(norm)
         self(:,n) = self(:,n)*norm
      end do
   end subroutine

   PURE subroutine reverse_Schmidt_orthonormalize(self,S)
   ! Schmidt orthonormalize the column vectors in "self" using "S" as
   ! the metric.
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: S
      REAL :: fac
      INT :: n,o
      REAL :: norm
   ENSURE(is_square_(S),"MAT{REAL}:reverse_Schmidt_orthonormalize ... S must be square matrix")
   ENSURE(size(S,1)==size(self,1),"MAT{REAL}:reverse_Schmidt_orthonormalize ... incompatible metric S")
   ENSURE(NOT is_zero_(S),"MAT{REAL}:reverse_Schmidt_orthonormalize ... S is zero matrix")
   ENSURE(NOT is_zero_(self),"MAT{REAL}:reverse_Schmidt_orthonormalize ... self is zero matrix")

      ! The following code is inherited from MAT{INTRINSIC}
      do n = size(self,2),1,-1
         do o = size(self,2),n+1,-1
            fac = dot_(S,self(:,o),self(:,n))
            self(:,n) = self(:,n)-fac*self(:,o)
         end do
         norm = REALIFY(dot_(S,self(:,n),self(:,n)))
         ENSURE(norm>MAT_LINEAR_DEPENDENCE_TOL,"MAT{REAL}:reverse_Schmidt_orthonormalize ... linear dependence, vector "//to_str_(n))
         self(:,n) = self(:,n)/sqrt(norm)
      end do
   end subroutine

   PURE subroutine reverse_Schmidt_orthogonalise(self)
   ! Schmidt orthonormalize the column vectors in "self" using unit metric.
      MAT(REAL,:,:), INOUT :: self
      REAL :: fac
      REAL :: norm
      INT :: n,k
   ENSURE(size(self,1)>=size(self,2),"MAT{REAL}:reverse_Schmidt_orthogonalise ... more vectors than dimension of vector space")

      ! The following code is inherited from MAT{INTRINSIC}
      do n = size(self,2),1,-1
         do k = n-1,1,-1
            fac = dot_product(self(:,k),self(:,n))
            self(:,n) = self(:,n)-fac*self(:,k)
         end do
         norm = (dot_product(self(:,n),self(:,n)))
         ENSURE(norm>TOL(10),"MAT{REAL}:reverse_Schmidt_orthogonalise ... linear dependence, vector "//to_str_(n))
         self(:,n) = self(:,n)/sqrt(norm)
      end do
   end subroutine

   PURE subroutine symmetrically_orthonormalize(self,S)
   ! Symmetrically orthonormalize the column vectors in "self" using
   ! "S" as the metric, which must be real and positive definite.
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: S
      MAT(REAL,:,:), allocatable :: SS,SI
   ENSURE(is_square_(S),"MAT{REAL}:symmetrically_orthonormalize ... S must be square matrix")
   ENSURE(size(S,1)==size(self,1),"MAT{REAL}:symmetrically_orthonormalize ... incompatible metric S")
   ENSURE(NOT is_zero_(self),"MAT{REAL}:symmetrically_orthonormalize ... self is zero matrix")
   ENSURE(NOT is_zero_(S),"MAT{REAL}:symmetrically_orthonormalize ... S is zero matrix")

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
      MAT(REAL,:,:), INOUT :: self
      VEC(INT,:), optional, OUT :: permutation
      VEC(INT,:), allocatable :: perm
      MAT(BIN,:,:), allocatable :: mask
      INT :: i,n
      VEC(INT,2) :: loc
   ENSURE(is_square_(self),"MAT{REAL}:make_diagonally_dominant ... not square")

      ! The following code is inherited from MAT{INTRINSIC}
      if (present(permutation)) then
         ENSURE(size(permutation)==size(self,2),"MAT{REAL}:make_diagonally_dominant ... wrong size, perm")
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


!  ========================
!  Idempotency purification
!  ========================

   PURE subroutine McWeeny_purify(self,S)
   ! Apply the McWeeny purification to obtain an idempotent density
   ! matrix, using "S" as the metric.
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: S
      MAT(REAL,:,:), allocatable :: W,PS,PSPS
      REAL :: diff
      INT :: n
      n = size(self,1)
      call create_(W,n,n)
      call create_(PS,n,n)
      call create_(PSPS,n,n)
      W = self
      PS = self
      PSPS = self
      do

         ! McWeeny term
         call to_product_of_(PS,self,S)
         call to_scaled_product_of_(W,PS,self,fac=THREE)
         call to_product_of_(PSPS,PS,PS)
         call plus_scaled_product_of_(W,PSPS,self,fac=-TWO)

         ! Same?
         diff = maxval(abs(self-W))
         self = W
         if (diff<REAL_EPSILON) exit
      end do
      call destroy_(PSPS)
      call destroy_(PS)
      call destroy_(W)
   end subroutine

   PURE subroutine Holas_5th_order_purify(self,S)
   ! Apply the Holas's 5th order purification to obtain an idempotent
   ! density matrix, using "S" as the metric. This was shown by
   ! J. Kim and Y. Jung (2011) CPL 511 p. 159-60.
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: S
      MAT(REAL,:,:), allocatable :: W,PS,PS2,PS3
      REAL :: diff
      INT :: n
      n = size(self,1)
      call create_(W,n,n)
      call create_(PS,n,n)
      call create_(PS2,n,n)
      call create_(PS3,n,n)
      W = self
      PS = self
      PS2 = self
      PS3 = self
      do

         ! Holas term
         call to_product_of_(PS,self,S)
         call to_product_of_(PS2,PS,PS)
         call to_scaled_product_of_(W,PS2,self,fac=TEN)
         call to_product_of_(PS3,PS2,PS)
         call plus_scaled_product_of_(W,PS3,self,fac=-FIFTEEN)
         call to_product_of_(PS2,PS3,PS)
         call plus_scaled_product_of_(W,PS2,self,fac=SIX)

         ! Same?
         diff = maxval(abs(self-W))
         self = W
         if (diff<REAL_EPSILON) exit
      end do
      call destroy_(PS3)
      call destroy_(PS2)
      call destroy_(PS)
      call destroy_(W)
   end subroutine


!  =====================
!  Eigenproblem routines
!  =====================

   PURE subroutine diagonalize_Jacobi_0(self,evals,eps,maxit)
   ! Diagonalises "self" using the Jacobi rotation method. Returns
   ! the eigenvalues in "evals". Set absolute off-diagonal value
   ! "eps", or the maximum number of iterations "maxit", if desired.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), OUT :: evals
      REAL, optional, OUT :: eps
      INT, optional, IN :: maxit
      INT :: i,j,iter,maxiter
      REAL :: accuracy,tol
      BIN :: done
      MAT(REAL,:,:), allocatable :: W
   ENSURE(is_square_(self),"MAT{REAL}:diagonalize_Jacobi_0 ... self is not square")
   ENSURE(is_symmetric_(self),"MAT{REAL}:diagonalize_Jacobi_0 ... self is not symmetrix")
   ENSURE(size(evals)==size(self,1),"MAT{REAL}:diagonalize_Jacobi_0 ... wrong size, evals")
      accuracy = TOL(12)
      if (present(eps)) accuracy = eps
      maxiter = 30
      if (present(maxit)) maxiter = maxit
      W = self
      iter = 0
      do
         iter = iter+1
         ENSURE(NOT iter>maxiter,"MAT{REAL}:diagonalize_Jacobi_0 ... too many iterations")
         if (iter>maxiter) exit
         tol = accuracy
         if (iter<=3) tol = TOL(1)*(sum_elements_(abs(W))-trace_(W))
         done = TRUE
         do i = 1,size(self,1)
            do j = 1,i-1
               if (abs(W(i,j))<tol) cycle
               done = FALSE
               call jacobi_rotation_(W,i,j)
            end do
         end do
         done = done AND iter>3
         if (done) exit
      end do
      call put_diagonal_to_(W,evals)

      ! Sort ascending -- see the note in the eigenvector variant below.
      ! Jacobi's output order is not canonical, so match dsyev's contract.
      call sort_(evals)
      call destroy_(W)
   end subroutine

   PURE subroutine diagonalize_Jacobi_1(self,evals,evecs,eps,maxit)
   ! Diagonalises "self" using the Jacobi rotations method. Returns
   ! the eigenvalues "evals" and eigenvectors "evecs". Set absolute
   ! off diagonal value "eps", or the maximum number of iterations
   ! "maxit", if desired.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), OUT :: evals
      MAT(REAL,:,:), OUT :: evecs
      REAL, optional, IN :: eps
      INT, optional, IN :: maxit
      INT :: i,j,iter,maxiter
      REAL :: tol
      BIN :: done
      MAT(REAL,:,:), allocatable :: W
      VEC(INT,:), allocatable :: order
   ENSURE(is_square_(self),"MAT{REAL}:diagonalize_Jacobi_1 ... self is not square")
   ENSURE(is_symmetric_(self),"MAT{REAL}:diagonalize_Jacobi_1 ... self is not symmetrix")
   ENSURE(size(evals)==size(self,1),"MAT{REAL}:diagonalize_Jacobi_1 ... wrong size, eigenvalues")
   ENSURE(is_square_(evecs),"MAT{REAL}:diagonalize_Jacobi_1 ... eigenvectors are not square")
   ENSURE(size(evals,1)>=size(self,1),"MAT{REAL}:diagonalize_Jacobi_1 ... wrong size, eigenvectors")
      tol = TOL(8)
      if (present(eps)) tol = eps
      maxiter = 500
      if (present(maxit)) maxiter = maxit
      W = self
      call to_unit_mx_(evecs)
      iter = 0
      do
         iter = iter+1
         ENSURE(NOT iter>maxiter,"MAT{REAL}:diagonalize_Jacobi_1 ... too many iterations")
         if (iter>maxiter) exit
         done = TRUE
         do i = 1,size(self,1)
            do j = 1,i-1
               if (abs(W(i,j))<tol) cycle
               done = FALSE
               call jacobi_rotation_(W,i,j,evecs)
            end do
         end do
         done = done AND iter>3
         if (done) exit
      end do
      call put_diagonal_to_(W,evals)

      ! Sort the eigenvalues ascending, permuting the eigenvectors with
      ! them. Jacobi leaves both in whatever order its sweeps happen to
      ! produce, which is NOT canonical: one ulp of difference in the input
      ! can change the sweep path and come out permuted. That is not
      ! hypothetical -- it made an atom's ADP principal axes print in a
      ! different order on macOS and on Linux from the same structure
      ! (tests/long/urea_rhf_DZP_consistent-cluster-charge_HAF, H3), with
      ! eigenvalues that were not even close to degenerate. LAPACK's dsyev
      ! guarantees ascending order and this routine is used in its place
      ! (to stay PURE), so match its contract.
      call create_(order,size(self,1))
      call quick_sort_(evals,order)  ! self untouched; evals(order) is sorted
      evals = evals(order)
      evecs = evecs(:,order)
      call destroy_(order)

      ! The SIGN is deliberately NOT canonicalised here, though it is as
      ! arbitrary as the order was. This routine diagonalises the ADP tensor
      ! and the refinement normal equations, where the eigenvectors are used
      ! in sign-invariant combinations (v v^T), so fixing the sign buys
      ! nothing -- and where an eigenspace is degenerate it cannot help
      ! anyway, a degenerate subspace being arbitrary up to a rotation and
      ! not merely a sign. Measured: canonicalising here moved the ADP esds
      ! of nh3_rhf_DZP_HAR and nh3_rhf_DZP_HAF_and_structure_factors without
      ! moving a single value. Canonicalise at the call site if a particular
      ! consumer needs it -- see MAT{REAL}:canonicalise_eigenvector_signs.
      call destroy_(W)
   end subroutine

   PURE subroutine canonicalise_eigenvector_signs(self,tol)
   ! Fix the arbitrary overall sign of every column of "self", which is taken
   ! to hold eigenvectors as columns. See VEC{REAL}:canonicalise_sign for the
   ! rule and for why an untreated sign makes output platform-dependent.
   !
   ! Applies to a matrix of eigenvectors ALONE. Do not use it on the left
   ! factor of a decomposition such as an SVD, where flipping a column of L
   ! must be matched by flipping the corresponding row of R^T or the product
   ! no longer reproduces the original matrix.
      MAT(REAL,:,:), INOUT :: self
      REAL, IN, optional :: tol
      VEC(REAL,:), allocatable :: v
      INT :: j
      call create_(v,size(self,1))
      do j = 1,size(self,2)
         v = self(:,j)
         call canonicalise_sign_(v,tol)
         self(:,j) = v
      end do
      call destroy_(v)
   end subroutine

   PURE subroutine project_out_vectors(self,V)
   ! Replace the symmetric matrix "self" by P self P, P = 1 - V V^T, where
   ! the columns of "V" are orthonormal: self then has no component
   ! along them, and each is an eigenvector with eigenvalue zero.
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: V
      MAT(REAL,:,:), allocatable :: W,X,G
      INT :: n,k
   ENSURE(is_square_(self),"MAT{REAL}:project_out_vectors ... self must be square")
   ENSURE(size(V,1)==size(self,1),"MAT{REAL}:project_out_vectors ... V and self differ in length")
      n = size(self,1)
      k = size(V,2)
      if (k==0) return

      ! W = self V and G = V^T self V; then with X = W - V G/2,
      ! P self P = self - V X^T - X V^T
      call create_(W,n,k)
      call to_product_of_(W,self,V)
      call create_(G,k,k)
      call to_product_of_(G,V,W,transpose_a=TRUE)
      X = W
      call plus_scaled_product_of_(X,V,G,fac=-HALF)
      call plus_scaled_product_of_(self,V,X,fac=-ONE,transpose_b=TRUE)
      call plus_scaled_product_of_(self,X,V,fac=-ONE,transpose_b=TRUE)
      call destroy_(G)
      call destroy_(X)
      call destroy_(W)
   end subroutine

   subroutine solve_symmetric_eigenproblem(self,eigenvalues,eigenvectors)
   ! Solve the symmetric eigenproblem for "self", yeilding a vector of
   ! "eigenvalues" and a matrix of "eigenvectors"
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), OUT :: eigenvalues
      MAT(REAL,:,:), OUT :: eigenvectors
      call solve_symm_eigenproblem_LAPACK_(self,eigenvalues,eigenvectors)

      ! LAPACK does not fix the sign of an eigenvector; do it here so the
      ! same matrix gives the same output on every platform and library.
      call canonicalise_eigenvector_signs_(eigenvectors)
   end subroutine

   subroutine solve_symm_eigenproblem_LAPACK(self,eigenvalues,eigenvectors)
   ! Solve the symmetric eigenproblem for "self", yeilding a vector of
   ! "eigenvalues" and a matrix of "eigenvectors". LAPACK version.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), OUT :: eigenvalues
      MAT(REAL,:,:), OUT :: eigenvectors
      VEC(REAL,:), allocatable :: W
      INT :: dim,fail,lwork
   ENSURE(is_square_(self),"MAT{REAL}:solve_symm_eigenproblem_LAPACK ... non-square matrix")
   ENSURE(size(eigenvalues)>=size(self,1),"MAT{REAL}:solve_symm_eigenproblem_LAPACK ... eigenvalue array too small")
   ENSURE(size(eigenvectors)>=size(self),"MAT{REAL}:solve_symm_eigenproblem_LAPACK ... eigenvector matrix too small")
      dim = size(self,1)
      lwork = max(dim*dim,3*dim-1)
      call create_(W,lwork)
      eigenvectors = self
      fail = 0
      call dsyev("V","L",dim,eigenvectors,dim,eigenvalues,W,lwork,fail)
      call destroy_(W)
      ENSURE(fail==0,"MAT{REAL}:solve_symm_eigenproblem_LAPACK ... no solution, error found")
   end subroutine

   PURE subroutine to_product_of_BLAS(self,a,b,transpose_a,transpose_b)
   ! Set "self" to the matrix product of "a" and "b", by BLAS dgemm. If
   ! present, "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! need to be transposed. "self" may be larger than the product; only its
   ! leading block is set.
   ! NOTE: dgemm is declared pure below, which the compiler cannot check. It
   ! is true: dgemm changes only its last matrix argument, and has no saved
   ! state and no I/O, except when called with bad arguments (xerbla stops).
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: a,b
      BIN, optional, IN :: transpose_a
      BIN, optional, IN :: transpose_b
      interface
         pure subroutine dgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
            STR(len=1), IN :: transa,transb
            INT, IN :: m,n,k,lda,ldb,ldc
            REAL, IN :: alpha,beta
            MAT(REAL,lda,*), IN :: a
            MAT(REAL,ldb,*), IN :: b
            MAT(REAL,ldc,*), INOUT :: c
         end subroutine
      end interface
      STR(len=1) :: ta,tb
      INT :: m,n,k
      ta = "N"; m = size(a,1); k = size(a,2)
      if (present(transpose_a)) then
         if (transpose_a) then
            ta = "T"; m = size(a,2); k = size(a,1)
         end if
      end if
      tb = "N"; n = size(b,2)
      if (present(transpose_b)) then
         if (transpose_b) then
            tb = "T"; n = size(b,1)
         end if
      end if
      ENSURE(size(self,1)>=m,"MAT{REAL}:to_product_of_BLAS ... incompatible arrays")
      ENSURE(size(self,2)>=n,"MAT{REAL}:to_product_of_BLAS ... incompatible arrays")
      ENSURE((tb=="N" AND size(b,1)==k) OR (tb=="T" AND size(b,2)==k),"MAT{REAL}:to_product_of_BLAS ... incompatible arrays")
      if (m==0 OR n==0) return
      if (k==0) then
         self(1:m,1:n) = ZERO
         return
      end if
      call dgemm(ta,tb,m,n,k,ONE,a,size(a,1),b,size(b,1),ZERO,self,size(self,1))
   end subroutine

   subroutine diagonalize_Davidson(self,eval,evec,n_eig,n_saved,maxiter,tolconv)
   ! Davidson diagonaliser for matrix "A".
   ! See Crouzeix et al (2006) Inria RR 1353
   ! Step numbers & symbols below mostly from p. 5 Crouzeix.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), allocatable, OUT :: eval  ! The eigenvalues
      MAT(REAL,:,:), allocatable, OUT :: evec  ! The eigenvector columns
      INT, IN, optional :: n_eig  ! No. of eigenvalues wanted
      INT, OUT, optional :: n_saved  ! No. of trial vector sets saved
      INT, IN, optional :: maxiter  ! Max. no. of iterations allowed
      REAL, IN, optional :: tolconv  ! Max. residual vec norm, to end
      MAT(REAL,:,:), allocatable :: R  ! Ritz vectors <- becomes "evec"
      MAT(REAL,:,:), allocatable :: V  ! V(:,n)       <- Davidson trial vectors
      MAT(REAL,:,:), allocatable :: W  ! W(:,n)       <- A.V
      MAT(REAL,:,:), allocatable :: H  ! "self"       <- in the basis "V"
      VEC(REAL,:), allocatable :: T  ! New direction, T
      VEC(REAL,:), allocatable :: rvl  ! Length of the residual vectors
      INT :: n_keep
      INT :: maxit
      INT :: iter,n_tr
      INT :: n,d,f,l,i
      REAL :: tolcon,bot
      BIN :: converged
      BIN :: too_many
      BIN :: collapse
   ENSURE(n_saved>=3,"MAT{REAL}:diagonalize_Davidson ... Must store at least 2 trial vector sets")

      ! Default values
      n = 2
      if (present(n_eig)) n = n_eig
      n_keep = 2
      if (present(n_saved)) n_keep = n_saved
      maxit = 5
      if (present(maxiter)) maxit = maxiter
      tolcon = TOL(7)
      if (present(tolconv)) tolcon = tolconv

      ! Sizes
      d = size(self,1)  ! "d" could be big!

      ! Results
      call create_(eval,n*n_keep)  ! <-- Will shrink to "n"
      call create_(R,d,n)  ! <-- Will be "evec"

      ! Storage
      call create_(rvl,n)  ! Residue vector length
      call create_(V,d,n*n_keep)  ! Need all in memory???
      call create_(W,d,n*n_keep)  ! Space for product self*V
      call create_(T,d)  ! Space for new directions

      ! Initialize
      call to_unit_mx_(V)  ! Initial guess
      iter = 0  ! Overall iteration count
      n_tr = 0  ! No. of trial vector sets - 1
      converged = FALSE
      too_many = FALSE
      do

         ! Iteration & no. of trial vector sets (-1)
         iter = iter+1
         n_tr = n_tr+1

         ! First & last trial vector columns
         l = n*(n_tr-1)
         f = l+1
         l = l+n

         ! 1. Expensive step, extends W
         ! Replace with low-mem matrix product
         W(:,f:l) = matmul(self,V(:,f:l))

         ! 2. Expand Rayleigh matrix
         call expand_(H,l,l)
         call create_(evec,l,l)
         H(f:l,:) = matmul(transpose(V(:,f:l)),W(:,1:l))
         call symmetric_reflect_(H,set_upper=TRUE)

         ! 3. Diagonalise (increasing eigenvalues)
         call diagonalize_Jacobi_(H,eval(1:l),evec)

         ! 4. Lowest Ritz vectors
         R = matmul(V(:,1:l),evec(:,1:n))

         ! Too many iters? Collapse space?
         too_many = iter>maxit; if (too_many) exit
         collapse = l==n*n_keep
         if (collapse) then

            ! 7b. Restart with Ritz vectors "R"
            n_tr = 0
            V(:,1:n) = R

            ! Clean
            call destroy_(evec)
            call destroy_(H)  ! <-- Note
         else

            ! 5-7. Get residual & new direction, per-column
            do i = 1,n

               ! 5. Residual in T & its length
               T = eval(i)*R(:,i)-matmul(W(:,1:l),evec(:,i))
               rvl(i) = norm2(T)

               ! 6. Preconditioner
               bot = eval(i)-H(i,i)
               if (abs(bot)<0.001d0) bot = bot+TOL(2)
               bot = ONE/bot

               ! 7a. Set new direction T
               V(:,l+i) = bot*T
            end do

            ! Finished?
            converged = all(rvl<tolcon); if (converged) exit

            ! 7. Orthonormalize vectors V
         ! V(:,1:l+n).modified_Gram_Schmidt_QR
            call Householder_QR_(V(:,1:l+n))

            ! Clean up
            call destroy_(evec)
         end if
      end do
      ENSURE(converged,"MAT{REAL}:diagonalize_Davidson ... No convergence")
      ENSURE(NOT too_many,"MAT{REAL}:diagonalize_Davidson ... Too many iterations")

      ! Clean up
      call destroy_(evec)
      call destroy_(H)
      call destroy_(T)
      call destroy_(W)
      call destroy_(V)
      call destroy_(rvl)

      ! Recover solution
      call move_alloc(R,evec)

      ! Fix the arbitrary eigenvector signs, as the direct solvers do
      call canonicalise_eigenvector_signs_(evec)
      call shrink_(eval,n)
   end subroutine

   subroutine internal_vectors(self,ivec,iv_tol)
   ! Extract the internal vectors of a matrix i.e. vectors in the
   ! orthocomplement of the kernel of the matrix.
   ! leaky because the number of internal vectors is unknown
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: ivec
      REAL, optional, IN :: iv_tol
      VEC(REAL,:), allocatable :: eval
      MAT(REAL,:,:), allocatable :: evec
      REAL :: tol
      INT :: i,h,dim
   ENSURE(size(self,1)==size(self,2),"MAT{REAL}:internal_vectors ... matrix not square")
      tol = INTERNAL_GEMINAL_TOL
      if (present(iv_tol)) tol = iv_tol
      dim = size(self,1)
      call create_(eval,dim)
      call create_(evec,dim,dim)
      call diagonalize_Jacobi_(self,eval,evec)

      ! Tests
      h = count(mask=eval>tol)
      DIE_IF(count(mask=eval<-TOL(10))/=0,"MAT{REAL}:internal_vectors ... matrix not positive")
      DIE_IF(h==0,"MAT{REAL}:internal_vectors ... no internal vector")
      call create_(ivec,h,dim)
      h = 0
      do i = 1,dim
         if (NOT is_zero_(eval(i),tol)) then
            h = h+1
            ivec(h,:) = evec(:,i)
         end if
      end do

      ! Clean
      call destroy_(eval)
      call destroy_(evec)
   end subroutine


!  ========================
!  Linear equation routines
!  ========================

   subroutine solve_linear_equation(self,rhs,solution,fail)
   ! Solve the linear equations posed by "self", with "rhs" as the RHS vector,
   ! yeilding vector "solution" as the answer
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), IN :: rhs
      VEC(REAL,:), OUT :: solution
      BIN, OUT, optional :: fail
      call solve_linear_equation_LAPACK_(self,rhs,solution,fail)
   end subroutine

   subroutine solve_linear_equation_LAPACK(self,rhs,solution,fail)
   ! Solve the linear equations posed by "self", with "rhs" as the RHS vector,
   ! yielding vector "solution" as the answer. LAPACK version.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), IN :: rhs
      VEC(REAL,:), OUT :: solution
      BIN, OUT, optional :: fail
      MAT(REAL,:,:), allocatable :: LU
      VEC(INT,:), allocatable :: pivot
      INT :: dim,nrhs,err
   ENSURE(is_square_(self),"MAT{REAL}:solve_linear_equation_LAPACK ... non-square matrix")
   ENSURE(size(rhs)==size(self,1),"MAT{REAL}:solve_linear_equation_LAPACK ... incompatible rhs")
   ENSURE(size(solution)==size(self,1),"MAT{REAL}:solve_linear_equation_LAPACK ... incompatible solution vector")
      dim = size(rhs)
      nrhs = 1
      call create_(LU,dim,dim)
      call create_(pivot,dim)
      LU = self
      solution = rhs
      call dgesv(dim,nrhs,LU,dim,pivot,solution,dim,err)
      if (present(fail)) then
         fail = FALSE
         if (err/=0) fail = TRUE
      else
         ENSURE(err==0,"MAT{REAL}:solve_linear_equation_LAPACK ... no solution, error found")
      end if
      call destroy_(pivot)
      call destroy_(LU)
   end subroutine

   subroutine to_cholesky_factor(self,fail)
   ! Replace the symmetric positive definite "self" by its lower
   ! Cholesky factor L, self = L L^T, with zeros above the diagonal.
   ! LAPACK version.
      MAT(REAL,:,:), INOUT :: self
      BIN, OUT, optional :: fail
      INT :: dim,err,i
   ENSURE(is_square_(self),"MAT{REAL}:to_cholesky_factor ... non-square matrix")
      dim = size(self,1)
      call dpotrf("L",dim,self,dim,err)
      if (present(fail)) then
         fail = err/=0
      else
         DIE_IF(err/=0,"MAT{REAL}:to_cholesky_factor ... matrix is not positive definite, at row "//trim(to_str_(err)))
      end if
      do i = 2,dim
         self(1:i-1,i) = ZERO
      end do
   end subroutine

   PURE subroutine solve_cholesky_equation(self,rhs,solution)
   ! With "self" a lower Cholesky factor L, as from .to_cholesky_factor,
   ! solve L L^T "solution" = "rhs".
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), IN :: rhs
      VEC(REAL,:), OUT :: solution
      INT :: dim,i
   ENSURE(is_square_(self),"MAT{REAL}:solve_cholesky_equation ... non-square matrix")
   ENSURE(size(rhs)==size(self,1),"MAT{REAL}:solve_cholesky_equation ... incompatible rhs")
   ENSURE(size(solution)==size(self,1),"MAT{REAL}:solve_cholesky_equation ... incompatible solution vector")
      dim = size(self,1)
      solution = rhs

      ! L y = rhs
      do i = 1,dim
         solution(i) = solution(i)/self(i,i)
         solution(i+1:dim) = solution(i+1:dim)-self(i+1:dim,i)*solution(i)
      end do

      ! L^T x = y
      do i = dim,1,-1
         solution(i) = (solution(i)-dot_product(self(i+1:dim,i),solution(i+1:dim)))/self(i,i)
      end do
   end subroutine

   subroutine solve_lower_triangular_equations(self,X)
   ! With "self" a lower triangular matrix L, as from .to_cholesky_factor,
   ! replace each column of "X" by the solution of L x = column, so that
   ! "X" becomes L^-1 X. LAPACK version.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), INOUT :: X
   ENSURE(is_square_(self),"MAT{REAL}:solve_lower_triangular_equations ... non-square matrix")
   ENSURE(size(X,1)==size(self,1),"MAT{REAL}:solve_lower_triangular_equations ... incompatible X")
      call dtrsm("L","L","N","N",size(X,1),size(X,2),ONE,self,size(self,1),X,size(X,1))
   end subroutine

   subroutine solve_linear_equations(self,rhs,solution)
   ! Solve the linear equations posed by "self", with "rhs" as a matrix of RHS
   ! vectors, yeilding matrix "solution" as a matrix of solution vectors.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), IN :: rhs
      MAT(REAL,:,:), OUT :: solution
      call solve_linear_equations_LAPACK_(self,rhs,solution)
   end subroutine

   subroutine solve_linear_equations_LAPACK(self,rhs,solution)
   ! Solve the linear equations posed by "self", with "rhs" as a matrix of RHS
   ! vectors, yeilding matrix "solution" as a matrix of solution vectors.
   ! LAPACK version
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), IN :: rhs
      MAT(REAL,:,:), OUT :: solution
      MAT(REAL,:,:), allocatable :: LU
      VEC(INT,:), allocatable :: pivot
      INT :: dim1,nrhs,err
   ENSURE(is_square_(self),"MAT{REAL}:solve_linear_equations_LAPACK ... non-square matrix")
   ENSURE(size(rhs,1)==size(self,2),"MAT{REAL}:solve_linear_equations_LAPACK ... rhs incompatible with coefficient matrix")
   ENSURE(nrhs>0,"MAT{REAL}:solve_linear_equations_LAPACK ... no rhs vectors")
      dim1 = size(rhs,1)
      nrhs = size(rhs,2)
      call create_(LU,dim1,dim1)
      call create_(pivot,dim1)
      LU = self
      solution = rhs
      call dgesv(dim1,nrhs,LU,dim1,pivot,solution,dim1,err)
      call destroy_(pivot)
      call destroy_(LU)
      ENSURE(err==0,"MAT{REAL}:solve_linear_equations_LAPACK ... no solution, error found")
   end subroutine

   subroutine solve_convex_linear_equation(self,rhs,solution,plist,keep0,keep1,fail)
   ! This routine solves the quadratic minimisation problem:
   !    f(X) = rhs . X  -  HALF * X^T . self . X  |  X>=0
   ! over all the elements in "plist", subject to the constraint that the
   ! "solution" X is positive for all elements in "plist". Any elements of the
   ! "solution" not in "plist" are not constrained to be positive nor do they
   ! participate in the evaluation of the quadratic function f -- they are
   ! usually lagrange multipliers. This routine involves solving reduced linear
   ! equations with "self" as the LHS and "rhs" as the RHS, over all partitions
   ! of the elements in "plist".  If "keep0" is present, then the indices in
   ! this sublist of "plist" must be kept when considering all partitions. If
   ! "keep1" is present, then the indices in this sublist of "plist" must be
   ! kept plus at most one extra index for each index in "keep1", and further,
   ! the positivity constraint is relaxed on these indices. It is an error if
   ! "keep0" and "keep1" are both present. If present, "fail" is set TRUE if no
   ! solutions are found, otherwise the routine terminates with an error.
   ! WARNING: if the dimension of the matrix is too large, this routine will
   ! take a long time.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), IN :: rhs
      VEC(REAL,:), OUT :: solution
      VEC(INT,:), IN :: plist
      VEC(INT,:), optional, IN :: keep0,keep1
      BIN, OUT, optional :: fail
      INT :: pdim,rdim,kdim,k,n,n_combinations
      VEC(INT,:), allocatable :: ulist,list
      MAT(INT,:,:), allocatable :: combination
      VEC(REAL,:), allocatable :: sol,sol0,rhs0
      REAL :: e,e0
      BIN :: found_one,even
   ENSURE(is_square_(self),"MAT{REAL}:solve_convex_linear_equation ... non-square matrix")
   ENSURE(size(rhs)==size(self,1),"MAT{REAL}:solve_convex_linear_equation ... incompatible rhs")
   ENSURE(size(solution)==size(self,1),"MAT{REAL}:solve_convex_linear_equation ... incompatible solution vector")
   ENSURE(size(plist)<=size(self,1),"MAT{REAL}:solve_convex_linear_equation ... plist to large")
   ENSURE(all_in_range_(plist,[1,size(self)]),"MAT{REAL}:solve_convex_linear_equation ... plist indices out of range")
   ENSURE(NOT (present(keep0) AND present(keep1)),"MAT{REAL}:solve_convex_linear_equation ... specify only keep0 or keep1")

!      if (.dim1==6) then
!      list.create(4)
!      sol.create(.dim1)
!      sol = ZERO
!      sol(1) = 0.3
!      sol(2) = 0.7
!      sol(4) = 0.4
!      sol(5) = 0.6
!      list = [1,2,4,5]

!               - HALF*dot_product(sol(list),matmul(self(list,list),sol(list)))
!      print *, "===in MAT, average energy =",e0
!      sol.destroy
!      list.destroy
!      end
!!      if (.dim1==3) then
!!      list.create(2)
!!      sol.create(.dim1)
!!      sol = ZERO
!!      sol(1) = 0.3
!!      sol(2) = 0.7
!!      list = [1,2]

!!               - HALF*dot_product(sol(list),matmul(self(list,list),sol(list)))
!!      print *, "===in MAT, average energy =",e0
!!      sol.destroy
!!      list.destroy
!!      end
      even = FALSE
      if (present(keep0)) then
         ENSURE(has_all_elements_common_with_(keep0,plist),"MAT{REAL}:solve_convex_linear_equation ... keep0 not in plist")
         kdim = size(keep0)
         even = is_even_(kdim)
      end if
      if (present(keep1)) then
         ENSURE(has_all_elements_common_with_(keep1,plist),"MAT{REAL}:solve_convex_linear_equation ... keep1 not in plist")
         kdim = size(keep1)
         even = is_even_(kdim)
      end if
      pdim = size(plist)
      WARN_IF(pdim>=20,"MAT{REAL}:solve_convex_linear_equation ... LHS dimension may be too large")
      found_one = FALSE
      e = huge(ONE)
      call create_(sol,size(self,1))
      call create_(ulist,size(self,1)); ulist = [(k,k = 1,size(self,1))]
      call prune_(ulist,plist)  ! ulist = all - plist = unconstrained
   ! print *, "-----------start------------"
   ! print *, "even =",even
   ! print *, "plist:"
   ! print *, plist
   ! print *, "ulist:"
   ! print *, ulist
      do k = pdim,1,-1  ! Loop over indice groups of length "k" which are non-zero

   ! print *, "---> combinations of length k =", k
         if (present(keep1)) then
            if (k>2*kdim) cycle
         end if
         n_combinations = int(choose_(pdim,k))
         call create_(combination,k,n_combinations)
         call make_combinations_of_length_(plist,k,combination)
         rdim = k+size(self,1)-pdim
         call create_(rhs0,rdim)
         call create_(sol0,rdim)
         call create_(list,rdim)  ! list = comb(plist) + ulist
         do n = 1,n_combinations  ! Loop over a particular "k" length combo

            ! print *, "combination:"
            ! print *, combination(:,n)
            if (present(keep1)) then  ! if present, keep only these elements
               if (NOT has_all_elements_common_with_(keep1,combination(:,n))) cycle
            end if
            if (present(keep0)) then  ! if present, keep only these elements

            ! print *, "keep0:"
            ! print *, keep0
            ! if (NOT keep0.has_all_elements_common_with(combination(:,n))) cycle
               if (NOT has_elements_common_with_(keep0,combination(:,n))) cycle
               if (even AND all(combination(:,n)>size(self,1)/2)) cycle
               if (even AND all(combination(:,n)<=size(self,1)/2)) cycle
            end if
            list(1:k) = combination(:,n)  ! The "list" of non-zero solution elements
            list(k+1:) = ulist  ! Always add the list of unconstrained elements
         ! print *, "list:"
         ! print *, list
         ! print *, "rhs:"
         ! print *, rhs
            rhs0 = rhs(list)
         ! print *, "rhs0:"
         ! print *, rhs0
         ! print *, "lhs0:"
         ! print *, self(list,list)
            call solve_linear_equation_(self(list,list),rhs0,sol0)  ! Find the subset solution, "sol"
            sol(list) = sol0
         ! print *, "sol0:"
         ! print *, sol0
         ! print *, "sol:"
         ! print *, sol
            if (NOT present(keep1) AND any(sol(list(1:k))<ZERO)) cycle  ! Unacceptable ..............
            e0 = dot_product(rhs(list),sol(list))-HALF*dot_product(sol(list),matmul(self(list,list),sol(list)))
         ! print *, "e0 = ",e0
            if (e0>=e) cycle  ! Found a better solution before
            solution = ZERO  ! Zero non-"list" solution elements
            solution(list) = sol(list)  ! Keep this solutions
         ! print *, "new solution found:"
         ! print *, solution
            e = e0  ! Keep this E value
            found_one = TRUE
         end do
         call destroy_(list)
         call destroy_(sol0); call destroy_(rhs0)
         call destroy_(combination)
      end do
      call destroy_(ulist)
      call destroy_(sol)
      if (present(fail)) then
         if (NOT found_one) then; fail = TRUE
         else; fail = FALSE
         end if
      else
         DIE_IF(NOT found_one,"MAT{REAL}:solve_convex_linear_equation ... acceptable solution was not found")
      end if
   end subroutine


!  ==================================
!  Singular value decomposition (SVD)
!  ==================================

   subroutine singular_value_decomposition(self,D,L,RT,fail)
   ! Find the singular value decomposition for "self".
   !   self = L * D * RT
   ! NOTE: R^T is returned as RT
   ! NOTE: "self" is destroyed by this process.
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), OUT :: D
      MAT(REAL,:,:), OUT :: L
      MAT(REAL,:,:), OUT :: RT
      BIN, optional, OUT :: fail
      call SVD_LAPACK_(self,D,L,RT,fail)
   end subroutine

   subroutine SVD_LAPACK(self,D,L,RT,fail)
   ! Find the singular value decomposition for "self".
   !   self = L * D * RT
   ! NOTE: R^T is returned as RT
   ! NOTE: "self" is destroyed by this process.
   ! This routine uses the divide & conquer method.
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), OUT :: D
      MAT(REAL,:,:), OUT :: L
      MAT(REAL,:,:), OUT :: RT
      BIN, OUT, optional :: fail
      VEC(REAL,:), allocatable :: W
      VEC(INT,:), allocatable :: I
      INT :: dimw,dimi,info
   ENSURE(size(self,1)>=size(self,2),"MAT{REAL}:SVD_LAPACK ... self must have dim1 >= dim2")
   ENSURE(size(D)==size(self,2),"MAT{REAL}:SVD_LAPACK ... wrong size, D")
   ENSURE(is_square_(L),"MAT{REAL}:SVD_LAPACK ... L is not square")
   ENSURE(size(L,1)==size(self,1),"MAT{REAL}:SVD_LAPACK ... wrong size, L")
   ENSURE(is_square_(RT),"MAT{REAL}:SVD_LAPACK ... RT is not square")
   ENSURE(size(RT,1)==size(self,2),"MAT{REAL}:SVD_LAPACK ... wrong size, RT")
      dimw = 2*(4*size(self,2)*size(self,2)+6*size(self,2)+size(self,1))
      dimi = 8*size(self,2)
      call create_(W,dimw)
      call create_(I,dimi)
      call dgesdd("A",size(self,1),size(self,2),self,size(self,1),D,L,size(self,1),RT,size(self,2),W,dimw,I,info)
      if (present(fail)) then
         fail = FALSE
         if (info/=0) fail = TRUE
      else
         ENSURE(info==0,"MAT{REAL}:SVD_LAPACK ... no solution, error found")
      end if
   end subroutine


!  =============
!  Least squares
!  =============

   subroutine solve_normal_equations(grad,shift,delta)
   ! Get the "shift" in some parameter vector by solving 
   ! the normal equations given a gradient vector "grad".
   ! "self" is used just workspace for the Hessian.
      VEC(REAL,:), IN :: grad
      VEC(REAL,:), OUT :: shift
      REAL, optional, IN :: delta
      MAT(REAL,:,:), allocatable :: H
      INT :: dim,i,j

      ! Hessian
      dim = size(grad)
      call create_(H,dim,dim)

      ! Make the normal matrix (outer product)
      do i = 1,dim
         do j = 1,dim
            H(i,j) = TWO*grad(i)*grad(j)
         end do
      end do

      ! Solve for the shifts .dX (leaky)
      call solve_ill_linear_equations_(H,grad,shift,1.0d-17,delta)
   end subroutine

   subroutine solve_ill_linear_equations_0(self,grad,solution,tol_near_0,delta)
   ! Solve a set of ill-conditioned linear equations. Eigenvalues of
   ! "self" less than "tol_near_0" are ignored.
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), IN :: grad
      VEC(REAL,:), OUT :: solution
      REAL, IN :: tol_near_0
      REAL, optional, IN :: delta
      VEC(REAL,:), allocatable :: rhs
      VEC(REAL,:), allocatable :: eval
      VEC(REAL,:), allocatable :: eval_near_0
      MAT(REAL,:,:), allocatable :: evec_near_0
      VEC(REAL,:), allocatable :: ival
      MAT(REAL,:,:), allocatable :: inv,evec,RT
      INT :: n_near_0,d,i,j
      REAL :: del
   ENSURE(is_square_(self),"MAT{REAL}:solve_ill_linear_equations_0 ... not square")

      ! Delta value
      del = ONE
      if (present(delta)) del = delta

      ! Temporaries
      d = size(self,1)
      call create_(eval,d)
      call create_(ival,d)
      call create_(evec,d,d)
      call create_(RT,d,d)
      call create_(inv,d,d)

      ! Solve SVD problem
      call singular_value_decomposition_(self,eval,evec,RT)

      ! Count eigenvalues which are nearly zero (leaky)
      n_near_0 = count(eval<=ZERO OR abs(eval)<=tol_near_0)
      call create_(eval_near_0,n_near_0)
      call create_(evec_near_0,d,n_near_0)
      j = 0
      do i = 1,d
         if (eval(i)<=ZERO OR abs(eval(i))<=tol_near_0) then
            j = j+1
            eval_near_0(j) = eval(i)
            evec_near_0(:,j) = evec(:,i)
            ival(i) = ZERO
         else
            ival(i) = ONE/eval(i)
         end if
      end do

      ! Make the pseudo inverse
      evec = transpose(evec)
      do i = 1,d
         do j = 1,d
            inv(i,j) = sum(RT(:,i)*ival(:)*evec(:,j))
         end do
      end do

      ! Get scaled RHS
      rhs = grad
      rhs = TWO*delta*grad

      ! Get the solution
      call to_product_of_(solution,inv,rhs)
      solution = -solution
   end subroutine

   subroutine solve_ill_linear_equations_1(self,rhs,ans,n0_tol,n0,n0_eval,n0_evec,eval,ps_inv,CM)
   ! Solve a set of ill-conditioned linear equations. Eigenvalues of
   ! "self" less than "n0_tol"  or which are negative are ignored.
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), IN :: rhs
      VEC(REAL,:), OUT :: ans
      REAL, IN :: n0_tol
      INT, OUT :: n0
      VEC(REAL,:), allocatable, OUT :: n0_eval
      MAT(REAL,:,:), allocatable, OUT :: n0_evec
      VEC(REAL,:), allocatable, OUT :: eval
      MAT(REAL,:,:), allocatable, OUT, optional :: ps_inv
      MAT(REAL,:,:), allocatable, OUT, optional :: CM
   ENSURE(is_square_(self),"MAT{REAL}:solve_ill_linear_equations_1 ... not square")

      ! Go back to eigenvalue method
      call solve_ill_linear_equations_v1_(self,rhs,ans,n0_tol,n0,n0_eval,n0_evec,eval,ps_inv,CM)
   end subroutine

   PURE subroutine solve_ill_linear_equations_v1(self,rhs,ans,n0_tol,n0,n0_eval,n0_evec,eval,ps_inv,CM)
   ! Solve a set of ill-conditioned linear equations. Eigenvalues of
   ! "self" less than "n0_tol" are ignored.
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), IN :: rhs
      VEC(REAL,:), OUT :: ans
      REAL, IN :: n0_tol
      INT, OUT :: n0
      VEC(REAL,:), allocatable, OUT :: n0_eval
      MAT(REAL,:,:), allocatable, OUT :: n0_evec
      VEC(REAL,:), allocatable, OUT :: eval
      MAT(REAL,:,:), allocatable, OUT, optional :: ps_inv
      MAT(REAL,:,:), allocatable, OUT, optional :: CM
      VEC(REAL,:), allocatable :: ival
      MAT(REAL,:,:), allocatable :: inv,evec
      REAL :: small
      INT :: d,i,j
   ENSURE(is_square_(self),"MAT{REAL}:solve_ill_linear_equations_v1 ... not square")

      ! Temporaries
      d = size(self,1)
      call create_(eval,d)
      call create_(ival,d)
      call create_(evec,d,d)
      call create_(inv,d,d)

      ! Solve the eigenvalue problem
      call diagonalize_Jacobi_(self,eval,evec)

      ! Count eigenvalues which are nearly zero (leaky)
      n0 = count(eval<=ZERO OR abs(eval)<=n0_tol)
      call create_(n0_eval,n0)
      call create_(n0_evec,d,n0)
      j = 0
      do i = 1,d
         if (eval(i)<=ZERO OR abs(eval(i))<=n0_tol) then

            ! Near-zero or negative eigenvalue
            j = j+1
            n0_eval(j) = eval(i)
            n0_evec(:,j) = evec(:,i)
            ival(i) = ZERO
         else

            ! OK to divide
            ival(i) = ONE/eval(i)
         end if
      end do

      ! Make the pseudo-inverse
      evec = transpose(evec)
      do i = 1,d
         do j = 1,d
            inv(i,j) = sum(evec(:,i)*ival(:)*evec(:,j))
         end do
      end do
      call destroy_(evec)

      ! Get the solution
      ans = matmul(inv,rhs)

      ! Get correlation matrix
      if (present(CM)) then
         call create_(CM,d,d)
         small = tiny(ONE)
         do i = 1,d
            do j = 1,d
               if (equals_(inv(i,i),ZERO,small)) then
                  CM(i,j) = ZERO
               else if (equals_(inv(j,j),ZERO,small)) then
                  CM(i,j) = ZERO
               else
                  CM(i,j) = inv(i,j)/sqrt(inv(i,i)*inv(j,j))
               end if
            end do
         end do
      end if

      ! Get uncertainties
      if (present(ps_inv)) then
         call move_alloc(inv,ps_inv)
      end if
   end subroutine

   PURE subroutine solve_restrained_linear_equations(self,rhs,W,prior_shift,ans,n0_tol,n0,n0_eval,n0_evec,eval,ps_inv,n_eff)
   ! Solve the restrained normal equations
   !    (self + W) ans = rhs + W prior_shift
   ! where "self" is the normal matrix A, "W" the restraint weight matrix
   ! and "prior_shift" the prior values minus the current ones. Then "ans"
   ! minimises  chi^2 + (p - p0)^T W (p - p0)  to first order. The
   ! eigenvalue filter of solve_ill_linear_equations_v1 still removes exact
   ! null directions. Also returns "ps_inv", the covariance (A+W)^-1, and
   ! "n_eff" = tr[(A+W)^-1 A], the effective number of parameters, which
   ! equals the number of kept eigenvalues when W is zero.
      MAT(REAL,:,:), IN :: self
      VEC(REAL,:), IN :: rhs
      MAT(REAL,:,:), IN :: W
      VEC(REAL,:), IN :: prior_shift
      VEC(REAL,:), OUT :: ans
      REAL, IN :: n0_tol
      INT, OUT :: n0
      VEC(REAL,:), allocatable, OUT :: n0_eval
      MAT(REAL,:,:), allocatable, OUT :: n0_evec
      VEC(REAL,:), allocatable, OUT :: eval
      MAT(REAL,:,:), allocatable, OUT :: ps_inv
      REAL, OUT :: n_eff
      MAT(REAL,:,:), allocatable :: AW
      VEC(REAL,:), allocatable :: b
   ENSURE(is_square_(self),"MAT{REAL}:solve_restrained_linear_equations ... not square")
   ENSURE(is_same_shape_as_(W,self),"MAT{REAL}:solve_restrained_linear_equations ... wrong shape, W")
   ENSURE(size(rhs)==size(self,1),"MAT{REAL}:solve_restrained_linear_equations ... wrong size, rhs")
   ENSURE(size(prior_shift)==size(self,1),"MAT{REAL}:solve_restrained_linear_equations ... wrong size, prior_shift")
      call create_(AW,size(self,1),size(self,1))
      AW = self+W
      call create_(b,size(self,1))
      call to_product_of_(b,W,prior_shift)
      b = rhs+b
      call solve_ill_linear_equations_v1_(AW,b,ans,n0_tol,n0,n0_eval,n0_evec,eval,ps_inv)
      n_eff = trace_product_with_(ps_inv,self)
      call destroy_(b)
      call destroy_(AW)
   end subroutine

   function check_restrained_solve() result (n_failed)
   ! Check solve_restrained_linear_equations on a small problem and return
   ! the number of failed checks: zero weight gives the plain solution;
   ! a large weight on one parameter holds it at its prior and lowers the
   ! effective number of parameters by one; a ridge on every parameter
   ! gives n_eff between those two.
      INT :: n_failed
      MAT(REAL,:,:), allocatable :: A,W,inv,n0_evec
      VEC(REAL,:), allocatable :: rhs,p0,x,y,n0_eval,eval
      REAL :: n_eff,n_eff_0,err,tol
      INT :: n0,i,j
      n_failed = 0
      tol = TOL(8)

      ! A well-conditioned 3 x 3 normal matrix and right-hand side
      call create_(A,3,3)
      A(1,:) = [4.0d0,1.0d0,0.5d0]
      A(2,:) = [1.0d0,3.0d0,0.2d0]
      A(3,:) = [0.5d0,0.2d0,2.0d0]
      call create_(rhs,3); rhs = [1.0d0,-2.0d0,0.5d0]
      call create_(p0,3); p0 = ZERO
      call create_(W,3,3)
      call create_(x,3)
      call create_(y,3)

      ! 1. No restraint: the plain solution, n_eff = 3
      W = ZERO
      call solve_restrained_linear_equations_(A,rhs,W,p0,x,tol,n0,n0_eval,n0_evec,eval,inv,n_eff)
      call to_product_of_(y,A,x)
      err = maxval(abs(y-rhs))
      if (err>tol OR abs(n_eff-THREE)>tol) n_failed = n_failed+1
      call destroy_(n0_eval); call destroy_(n0_evec); call destroy_(eval); call destroy_(inv)

      ! 2. A huge weight on parameter 2 with prior 0.7: it stays there,
      !    the others solve the reduced 2 x 2 problem, n_eff = 2
      W = ZERO; W(2,2) = TEN**12
      p0 = ZERO; p0(2) = 0.7d0
      call solve_restrained_linear_equations_(A,rhs,W,p0,x,tol,n0,n0_eval,n0_evec,eval,inv,n_eff)
      if (abs(x(2)-0.7d0)>TOL(6)) n_failed = n_failed+1
      call to_product_of_(y,A,x)
      err = max(abs(y(1)-rhs(1)),abs(y(3)-rhs(3)))
      if (err>TOL(6) OR abs(n_eff-TWO)>TOL(6)) n_failed = n_failed+1
      call destroy_(n0_eval); call destroy_(n0_evec); call destroy_(eval); call destroy_(inv)

      ! 3. A ridge on everything: 0 < n_eff < 3
      W = ZERO
      do i = 1,3
         W(i,i) = ONE
      end do
      p0 = ZERO
      call solve_restrained_linear_equations_(A,rhs,W,p0,x,tol,n0,n0_eval,n0_evec,eval,inv,n_eff)
      n_eff_0 = ZERO
      do i = 1,3
         do j = 1,3
            n_eff_0 = n_eff_0+inv(i,j)*A(j,i)
         end do
      end do
      if (n_eff<=ZERO OR n_eff>=THREE OR abs(n_eff-n_eff_0)>tol) n_failed = n_failed+1
      call destroy_(n0_eval); call destroy_(n0_evec); call destroy_(eval); call destroy_(inv)
      call destroy_(y); call destroy_(x); call destroy_(W); call destroy_(p0); call destroy_(rhs); call destroy_(A)
   end function


!   solve_ill_linear_equations_v2(rhs,solution,tol_near_0,n_near_0,eval_near_0,evec_near_0,eval,inverse,CM) :: leaky
!   ! Solve a set of ill-conditioned linear equations. Eigenvalues of
!   ! "self" less than "tol_near_0" are ignored.
!      self :: INOUT
!      rhs :: VEC{REAL}, IN
!      solution :: VEC{REAL}, OUT
!      tol_near_0 :: REAL, IN
!      n_near_0 :: INT, OUT
!      eval_near_0 :: VEC{REAL}@
!      evec_near_0 :: MAT{REAL}@
!      eval :: VEC{REAL}@
!      inverse,CM :: MAT{REAL}, OUT, optional
!
!   ENSURE(.is_square,"not square")
!
!      ival :: VEC{REAL}@
!      inv,evec,RT :: MAT{REAL}@
!      small :: REAL
!      d,i,j :: INT
!
!      ! Dimension
!      d = .dim1
!
!      ! Temporaries
!      eval.create(d)
!      ival.create(d)
!      evec.create(d,d)
!      RT.create(d,d)
!      inv.create(d,d)
!
!      ! Solve SVD problem
!      .singular_value_decomposition(eval,evec,RT)
!
!      ! Count eigenvalues which are nearly zero (leaky)
!      n_near_0 = count(eval<=ZERO OR abs(eval)<=tol_near_0)
!
!      eval_near_0.destroy
!      eval_near_0.create(n_near_0)
!      evec_near_0.destroy
!      evec_near_0.create(d,n_near_0)
!
!      j = 0
!
!      do i = 1,d
!
!         if (eval(i)<=ZERO OR abs(eval(i))<=tol_near_0) then
!
!            j = j + 1
!            eval_near_0(j)   = eval(i)
!            evec_near_0(:,j) = evec(:,i)
!            ival(i)          = ZERO
!
!         else
!
!            ival(i) = ONE/eval(i)
!
!         end
!
!      end
!
!      ! Make the pseudo inverse
!      evec = transpose(evec)
!      do i = 1,d
!      do j = 1,d
!         inv(i,j) = sum(RT(:,i)*ival(:)*evec(:,j))
!      end
!      end
!
!      ! Get the solution
!      solution.to_product_of(inv,rhs)
!
!      ! Get uncertainties
!      if (present(inverse)) then
!         inverse = inv
!      end
!
!      ! Get correlation matrix
!      if (present(CM)) then
!
!         small = tiny(ONE)
!
!         do i = 1,d
!         do j = 1,d
!            if      (inv(i,i).equals(ZERO,small)) then
!               CM(i,j) = ZERO
!            else if (inv(j,j).equals(ZERO,small)) then
!               CM(i,j) = ZERO
!            else
!               CM(i,j) = inv(i,j)/sqrt(inv(i,i)*inv(j,j))
!            end
!         end
!         end
!
!      end
!
!   end

!  ========
!  Cholesky
!  ========

   PURE subroutine Cholesky_decomposition_0(self,tr)
   ! Decompose "self" into lower triangular matrices "L"
   !   self = L L^T or self = L L^\dagger
   ! where L is lower triangular and stored row-wise:
   ! i.e. (1,1), (2,1), (2,2), (3,1) ...
      MAT(REAL,:,:), INOUT :: self
      VEC(REAL,:), OUT :: tr
      MAT(REAL,:,:), allocatable :: L
   ENSURE(size(tr)==triangle_number_(size(self,1)),"MAT{REAL}:Cholesky_decomposition_0 ... wrong size, tr")
   ENSURE(is_square_(self),"MAT{REAL}:Cholesky_decomposition_0 ... self not square")

      ! Creates square

      ! The following code is inherited from MAT{INTRINSIC}
      call create_(L,size(self,1),size(self,1))
      call Cholesky_decomposition_(self,L)

      ! Take the non-zero values
      call zip_lower_triangle_to_(L,tr)
      call destroy_(L)
   end subroutine

   PURE subroutine Cholesky_decomposition_1(self,L)
   ! Decompose self into lower triangular matrices "L"
   !   self = L L^T or self = L L^\dagger
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: L
      INT :: i,j,dim
   ENSURE(is_square_(L),"MAT{REAL}:Cholesky_decomposition_1 ... L not square")
   ENSURE(is_same_shape_as_(self,L),"MAT{REAL}:Cholesky_decomposition_1 ... wrong size, L")
   ENSURE(all(diagonal_(L)/=ZERO),"MAT{REAL}:Cholesky_decomposition_1 ... L has zero diagonal elements!")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      L = ZERO

      ! Begin decomposition, with values being such that the matrix
      ! multiplication of it with its transpose returns the initial
      ! matrix. In the real case "conjg" is replaced with "1*".
      do i = 1,dim
         L(i,i) = sqrt(self(i,i)-sum((L(i,1:i-1))*L(i,1:i-1)))
         do j = i+1,dim
            L(j,i) = (self(i,j)-sum((L(i,1:i-1))*L(j,1:i-1)))/L(i,i)
         end do
      end do
      L = (L)
   end subroutine


!  ==========================================================
!  Matrix functions: square roots, inverses, and exponentials
!  ==========================================================

   subroutine to_inverse_of_0(self,R)
   ! self = (R)^(-1); can have R=self
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: R
      call to_inverse_of_LAPACK_(self,R)
   end subroutine

   subroutine to_inverse_of_LAPACK(self,R)
   ! self = (R)^(-1); can have R=self. LAPACK version.
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: R
      MAT(REAL,:,:), allocatable :: W
      VEC(INT,:), allocatable :: ipiv
      INT :: d,d2,fail
   ENSURE(is_square_(self),"MAT{REAL}:to_inverse_of_LAPACK ... not square")
   ENSURE(is_same_shape_as_(self,R),"MAT{REAL}:to_inverse_of_LAPACK ... not same shape as R")
      d = size(R,1)
      d2 = d*d
      self = R
      call create_(ipiv,d)
      call create_(W,d,d)
      fail = 0
      call dgetrf(d,d,self,d,ipiv,fail)
      ENSURE(fail==0,"MAT{REAL}:to_inverse_of_LAPACK ... failed LU factorisation, fail = "//trim(to_str_(fail)))
      call dgetri(d,self,d,ipiv,W,d2,fail)
      ENSURE(fail==0,"MAT{REAL}:to_inverse_of_LAPACK ... failed back substitution, fail = "//trim(to_str_(fail)))
      call destroy_(ipiv)
      call destroy_(W)
   end subroutine

   PURE subroutine to_pseudo_inverse_of(self,R,tol_0)
   ! self = (R)^(-1); can have R=self. "tol_0" is used to decide when
   ! in eigenvalue of "self" is to be regarded as zero.
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: R
      REAL, optional, IN :: tol_0
      MAT(REAL,:,:), allocatable :: evec
      VEC(REAL,:), allocatable :: eval
      INT :: d,i,j,k
      REAL :: tol,temp
   ENSURE(is_square_(self),"MAT{REAL}:to_pseudo_inverse_of ... self must be a square matrix")
   ENSURE(is_same_shape_as_(self,R),"MAT{REAL}:to_pseudo_inverse_of ... self and R incomptabile")

      ! Get tolerance for zero eigenvalue
      tol = REAL_EPSILON
      if (present(tol_0)) tol = tol_0

      ! Temporaries
      d = size(self,1)
      call create_(eval,d)
      call create_(evec,d,d)

      ! Solve the eigenvalue problem
      call diagonalize_Jacobi_(R,eval,evec)

      ! Invert large eigenvalues
      j = 0
      do i = 1,d
         if (abs(eval(i))>tol) then; eval(i) = ONE/eval(i)
         else; eval(i) = ZERO
         end if
      end do

      ! Make the pseudo-inverse
      do i = 1,d
         do j = 1,d
            temp = ZERO
            do k = 1,d
               temp = temp+evec(i,k)*eval(k)*evec(j,k)
            end do
            self(i,j) = temp
         end do
      end do

      ! Clean
      call destroy_(evec)
      call destroy_(eval)
   end subroutine

   PURE subroutine to_inverse_of_1(self,R_eval,R_evec,tol,n_small)
   ! self = R^(-1), but supplied are the R eigenvalues "R_eval"
   ! and the R eigenvectors "R_evec". If "tol" is present use that to
   ! decide on zero eigenvalues.  If "n_small" return the number of
   ! small or negative eigenvalues.
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: R_eval
      MAT(REAL,:,:), IN :: R_evec
      REAL, optional, IN :: tol
      INT, optional, OUT :: n_small
      VEC(REAL,:), allocatable :: eval
      INT :: d,i,j,k,n
      REAL :: eps,temp

      ! Get tolerance for smallness
      eps = TOL(20)
      if (present(tol)) eps = tol

      ! Get inverse inverse eigenvalues
      d = size(R_eval)
      call create_(eval,d)
      n = 0
      do i = 1,d
         temp = R_eval(i)
         if (temp<-eps) then

         ! val = temp.to_str("e15.8")
         ! DIE("non-positive eigenvalue, "// trim(val))
            temp = ZERO
            n = n+1
         else if (temp<=eps) then

         ! val = temp.to_str("e15.8")
         ! WARN("small eigenvalue, "// trim(val))
            n = n+1
         end if
         if (temp<=eps) then; eval(i) = ZERO
         else; eval(i) = ONE/temp
         end if
      end do
      if (present(n_small)) n_small = n

      ! Back transform
      do i = 1,d
         do j = 1,d
            temp = ZERO
            do k = 1,d
               temp = temp+R_evec(i,k)*eval(k)*R_evec(j,k)
            end do
            self(i,j) = temp
         end do
      end do
      call destroy_(eval)
   end subroutine

   PURE subroutine to_sqrt(self,tol)
   ! Set self to sqrt(self). Negative eigenvalues are zeroed
   ! according to the value of optional small value "tol".
      MAT(REAL,:,:), OUT :: self
      REAL, optional, IN :: tol
      MAT(REAL,:,:), allocatable :: R
      R = self
      call to_sqrt_of_(self,R,tol)
      call destroy_(R)
   end subroutine

   PURE subroutine to_sqrt_of_0(self,R,tol)
   ! Set self to the sqrt(R). Cannot have R=self. Negative eigenvalues
   ! are zeroed.
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), IN :: R
      REAL, optional, IN :: tol

! Looks like compiler problem for gfortran on 3/3/09
! ENSURE(.is_symmetric,"not symmetric")
      MAT(REAL,:,:), allocatable :: evec
      VEC(REAL,:), allocatable :: eval
      INT :: d,i,j,k
      REAL :: eps,temp
      eps = TOL(20)
      if (present(tol)) eps = tol
      d = size(R,1)
      call create_(eval,d)
      call create_(evec,d,d)
      call diagonalize_Jacobi_(R,eval,evec)
      do i = 1,d
         temp = abs(eval(i))
         if (temp<=eps) then

         !  WARN("non-positive eigenvalue, " // trim(temp.to_str("e15.8")))
            eval(i) = ZERO
         else
            eval(i) = sqrt(temp)
         end if
      end do
      do i = 1,d
         do j = 1,d
            temp = ZERO
            do k = 1,d
               temp = temp+evec(i,k)*eval(k)*evec(j,k)
            end do
            self(i,j) = temp
         end do
      end do
      call destroy_(evec)
      call destroy_(eval)
   end subroutine

   PURE subroutine to_sqrt_of_1(self,R_eval,R_evec,tol)
   ! self = sqrt(R), but supplied are the R eigenvalues "R_eval"
   ! and the R eigenvectors "R_evec".
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: R_eval
      MAT(REAL,:,:), IN :: R_evec
      REAL, optional, IN :: tol
      VEC(REAL,:), allocatable :: eval
      INT :: d,i,j,k
   ! val :: STR
      REAL :: eps,temp
      eps = TOL(20)
      if (present(tol)) eps = tol
      d = size(R_eval)
      call create_(eval,d)
      do i = 1,d
         temp = R_eval(i)

      ! if (temp<-eps) then
      !    val = temp.to_str("e15.8")
      !    DIE("non-positive eigenvalue, "// trim(val))
      ! else if (temp<=eps) then
      !    val = temp.to_str("e15.8")
      !    WARN("small eigenvalue, "// trim(val))
         temp = abs(temp)
      ! end
         if (temp<=eps) then; eval(i) = ZERO
         else; eval(i) = sqrt(temp)
         end if
      end do
      do i = 1,d
         do j = 1,d
            temp = ZERO
            do k = 1,d
               temp = temp+R_evec(i,k)*eval(k)*R_evec(j,k)
            end do
            self(i,j) = temp
         end do
      end do
      call destroy_(eval)
   end subroutine

   PURE subroutine to_inverse_sqrt_of_0(self,R,tol,n_small,n_negative,shift)
   ! Set self = sqrt(R)^(-1). Cannot have R = self.
   ! If "tol" is present use that to decide on zero eigenvalues.
   ! If "n_small" return the number of small eigenvalues.
   ! If "shift" then use this value instead of small eigenvalues.
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), IN :: R
      REAL, optional, IN :: tol
      INT, optional, OUT :: n_small
      INT, optional, OUT :: n_negative
      REAL, optional, IN :: shift

! Looks like compiler problem for gfortran on 3/3/09
! ENSURE(.is_symmetric,"not symmetric")
      MAT(REAL,:,:), allocatable :: evec
      VEC(REAL,:), allocatable :: eval
      INT :: d,i,j,k,n_n,n_s
      REAL :: eps,sh,temp,val_n,val_s

      ! Get tolerance for smallness
      eps = TOL(20)
      if (present(tol)) eps = tol

      ! Shift for zeo eigenvalues
      sh = ZERO
      if (present(shift)) sh = shift

      ! Solve eigenproblem
      d = size(R,1)
      call create_(eval,d)
      call create_(evec,d,d)
      call diagonalize_Jacobi_(R,eval,evec)

      ! Inverse sqrt eigenvalues
      n_n = 0
      n_s = 0
      val_n = ZERO
      val_s = ZERO
      do i = 1,d
         temp = eval(i)
         if (temp<-eps) then

            ! Negative eigenvalues
            eval(i) = ZERO
            n_n = n_n+1
            val_n = min(temp,val_n)
         else if (temp<=eps) then

            ! Small eigenvalues
            eval(i) = sh
            n_s = n_s+1
            val_s = min(temp,val_s)
         else

            ! Normal eigenvalues
            eval(i) = ONE/sqrt(temp)
         end if
      end do
      if (present(n_small)) n_small = n_s
      if (present(n_negative)) n_negative = n_n

   !  if (n_s>0) then
   !     WARN("There were "//trim(n_s.to_str)//"small eigenvalues")
   !  end

   !  if (n_n>0) then
   !     WARN("There were "//trim(n_n.to_str)//"negative eigenvalues")
   !  end

      ! Back transform
      do i = 1,d
         do j = 1,d
            temp = ZERO
            do k = 1,d
               temp = temp+evec(i,k)*eval(k)*evec(j,k)
            end do
            self(i,j) = temp
         end do
      end do
      call destroy_(evec)
      call destroy_(eval)
   end subroutine

   PURE subroutine to_inverse_sqrt_of_1(self,R_eval,R_evec,tol,n_small,n_negative,shift)
   ! self = sqrt(R)^(-1), but supplied are the R eigenvalues "R_eval"
   ! and the R eigenvectors "R_evec". If "tol" is present use that to
   ! decide on zero eigenvalues.  If "n_small" return the number of
   ! small eigenvalues.
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: R_eval
      MAT(REAL,:,:), IN :: R_evec
      REAL, optional, IN :: tol
      INT, optional, OUT :: n_small
      INT, optional, OUT :: n_negative
      REAL, optional, IN :: shift
      VEC(REAL,:), allocatable :: eval
      INT :: d,i,j,k,n_n,n_s
      REAL :: eps,sh,temp,val_n,val_s

      ! Get tolerance for smallness
      eps = TOL(20)
      if (present(tol)) eps = tol

      ! Shift for zeo eigenvalues
      sh = ZERO
      if (present(shift)) sh = shift

      ! Get inverse sqrt eigenvalues
      d = size(R_eval)
      call create_(eval,d)
      n_n = 0
      n_s = 0
      val_n = ZERO
      val_s = ZERO
      do i = 1,d
         temp = R_eval(i)
         if (temp<-eps) then

            ! Negative eigenvalues
            eval(i) = ZERO
            n_n = n_n+1
            val_n = min(temp,val_n)
         else if (temp<=eps) then

            ! Small eigenvalues
            eval(i) = sh
            n_s = n_s+1
            val_s = min(temp,val_s)
         else

            ! Normal eigenvalues
            eval(i) = ONE/sqrt(temp)
         end if
      end do
      if (present(n_small)) n_small = n_s
      if (present(n_negative)) n_negative = n_n

   !  if (n_s>0) then
   !     WARN("There were "//trim(n_s.to_str)//"small eigenvalues")
   !  end

   !  if (n_n>0) then
   !     WARN("There were "//trim(n_n.to_str)//"negative eigenvalues")
   !  end

      ! Back transform
      do i = 1,d
         do j = 1,d
            temp = ZERO
            do k = 1,d
               temp = temp+R_evec(i,k)*eval(k)*R_evec(j,k)
            end do
            self(i,j) = temp
         end do
      end do
      call destroy_(eval)
   end subroutine

   PURE subroutine eigenfilter_using(self,R_eval,R_evec,tol,n_small,n_negative)
   ! self = filter(self), where small eigenvalues less than "tol" are
   ! removed from its spectrum. Supplied are the R eigenvalues "R_eval"
   ! and the R eigenvectors "R_evec". If "tol" is present use that to
   ! decide on small or negative eigenvalues.  If "n_small" return the
   ! number of small eigenvalues.
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,:), IN :: R_eval
      MAT(REAL,:,:), IN :: R_evec
      REAL, optional, IN :: tol
      INT, optional, OUT :: n_small
      INT, optional, OUT :: n_negative
      VEC(REAL,:), allocatable :: eval
      INT :: d,i,j,k,n_n,n_s
      REAL :: eps,temp,val_n,val_s

      ! Get tolerance for smallness
      eps = TOL(20)
      if (present(tol)) eps = tol

      ! Get inverse sqrt eigenvalues
      d = size(R_eval)
      call create_(eval,d)
      n_n = 0
      n_s = 0
      val_n = ZERO
      val_s = ZERO
      do i = 1,d
         temp = R_eval(i)
         if (temp<-eps) then
            eval(i) = ZERO
            n_n = n_n+1
            val_n = min(temp,val_n)
         else if (temp<=eps) then
            eval(i) = ZERO
            n_s = n_s+1
            val_s = min(temp,val_s)
         else
            eval(i) = temp
         end if
      end do
      if (present(n_small)) n_small = n_s
      if (present(n_negative)) n_negative = n_n

   !  if (n_s>0) then
   !     WARN("There were "//trim(n_s.to_str)//"small eigenvalues")
   !  end

   !  if (n_n>0) then
   !     WARN("There were "//trim(n_n.to_str)//"negative eigenvalues")
   !  end

      ! Back transform
      do i = 1,d
         do j = 1,d
            temp = ZERO
            do k = 1,d
               temp = temp+R_evec(i,k)*eval(k)*R_evec(j,k)
            end do
            self(i,j) = temp
         end do
      end do
      call destroy_(eval)
   end subroutine

   PURE subroutine to_power_series_inverse_of(self,S,tol,maxit)
   ! Set self to the power series inverse square root of "S".
   ! If "tol" is present, make sure that the maximum deviation from the exact
   ! answer is less than "tol" times the smallest element of "S". If "maxit"
   ! is present, use this as the maximum number of terms in the power series,
   ! before termination with an error.
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), IN :: S
      REAL, optional, IN :: tol
      INT, optional, IN :: maxit
      VEC(REAL,:), allocatable :: d
      VEC(INT,:), allocatable :: perm
      MAT(REAL,:,:), allocatable :: W,X
      INT :: maxiter,n,k
      REAL :: eps
   ENSURE(is_square_(S),"MAT{REAL}:to_power_series_inverse_of ... S not square")
   ENSURE(is_same_shape_as_(self,S),"MAT{REAL}:to_power_series_inverse_of ... wrong shape")

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
         ENSURE(k<=maxiter,"MAT{REAL}:to_power_series_inverse_of ... power series too long")
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

   PURE subroutine to_power_series_inv_sqrt_of(self,S,tol,prefactor,maxit)
   ! Set self to the inverse square root of "S", a matrix which is required to
   ! have a unit diagonal. The method uses a binomial power series expansion.
   ! If "tol" is present, make sure that the maximum deviation from the exact
   ! answer is less than "tol" times the smallest element of "S". If "maxit"
   ! is present, use this as the maximum number of terms in the power series,
   ! before termination with an error.
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), INOUT :: S
      REAL, optional, IN :: tol,prefactor
      INT, optional, IN :: maxit
      MAT(REAL,:,:), allocatable :: W,X
      INT :: maxiter,n,k
      REAL :: eps,fac,prefac
   ENSURE(has_unit_diagonal_(S),"MAT{REAL}:to_power_series_inv_sqrt_of ... must have unit diagonal")
   ENSURE(is_square_(S),"MAT{REAL}:to_power_series_inv_sqrt_of ... S not square")
   ENSURE(is_same_shape_as_(self,S),"MAT{REAL}:to_power_series_inv_sqrt_of ... wrong shape")

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
         ENSURE(k<=maxiter,"MAT{REAL}:to_power_series_inv_sqrt_of ... power series too long")
      end do
      prefac = sqrt(prefac)
      self = prefac*self
      call to_unit_mx_(W)
      prefac = ONE/(prefac*prefac)
      S = prefac*(S+W)
      call destroy_(W)
      call destroy_(X)
   end subroutine

   PURE subroutine to_power_product_inverse_of(self,S,tol,prefactor,maxit)
   ! Set self to the inverse of "S".  If "tol" is present, it is
   ! used to establish the maximum deviation from the unit matrix, in the matrix
   ! product self*S*self. If "prefactor" is present, matrix "S" is multiplied by
   ! it to ensure its eigenvalues are less than one. If "maxit" is present, it
   ! is used as the maximum number of terms in the power product series.  The
   ! method uses a binomial power series expansion of "S": if S = (1 + D) then
   ! approximately S^-1/2 = (1 - 1/2 D + ...). The matrix "S" is iteratively
   ! transformed by the approximate square root until the result in the unit
   ! matrix, within tolerance "tol" (if present). Finally, to form the inverse,
   ! the inverse square root is squared.
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), IN :: S
      REAL, optional, IN :: tol
      REAL, optional, IN :: prefactor
      INT, optional, IN :: maxit
      MAT(REAL,:,:), allocatable :: W
   ENSURE(is_square_(S),"MAT{REAL}:to_power_product_inverse_of ... S not square")
   ENSURE(is_same_shape_as_(self,S),"MAT{REAL}:to_power_product_inverse_of ... wrong shape")
      call to_power_product_inv_sqrt_of_(self,S,tol,prefactor,maxit)
      W = self
      self = matmul(W,W)
      call destroy_(W)
   end subroutine

   PURE subroutine to_power_product_inv_sqrt_of(self,S,tol,prefactor,maxit)
   ! Set self to the inverse square root of "S".  If "tol" is present,
   ! it is used to establish the maximum deviation from the unit
   ! matrix, in the matrix product self*S*self. If "prefactor" is
   ! present, matrix "S" is multiplied by it to ensure its eigenvalues
   ! are less than one. If "maxit" is present, it is used as the
   ! maximum number of terms in the power product series.  The method
   ! uses a binomial power series expansion of "S": if S = (1 + D)
   ! then approximately S^-1/2 = (1 - 1/2 D + ...). The matrix "S" is
   ! iteratively transformed into a new basis using the approximate
   ! square root, until the result is the unit matrix within tolerance
   ! "tol" (if present).
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), IN :: S
      REAL, optional, IN :: tol
      REAL, optional, IN :: prefactor
      INT, optional, IN :: maxit
      MAT(REAL,:,:), allocatable :: D,X
      INT :: maxiter,n,k
      REAL :: eps,prefac
   ENSURE(is_square_(S),"MAT{REAL}:to_power_product_inv_sqrt_of ... S not square")
   ENSURE(is_same_shape_as_(self,S),"MAT{REAL}:to_power_product_inv_sqrt_of ... wrong shape")
      eps = TOL(9)
      if (present(tol)) eps = tol
      maxiter = 100
      if (present(maxit)) maxiter = maxit
      prefac = ONE/FOUR
      if (present(prefactor)) prefac = prefactor
      n = size(S,1)
      call create_(X,n,n)
      call create_(D,n,n)
      X = prefac*S
      call to_unit_mx_(self)
      k = 0
      do
         k = k+1
         D = self
         self = (THREE/TWO)*D
         self = self-HALF*matmul(X,D)
         call change_basis_to_(S,X,self)
         X = prefac*X
         if (has_unit_diagonal_(X,eps)) exit
         ENSURE(k<=maxiter,"MAT{REAL}:to_power_product_inv_sqrt_of ... power series too long")
      end do
      self = sqrt(prefac)*self
      call destroy_(D)
      call destroy_(X)
   end subroutine

   PURE subroutine to_exponential_of(self,X,tol)
   ! Exponentiate the matrix "X" using a power series expansion, self = exp(X),
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, optional, IN :: tol

      ! The following code is inherited from MAT{INTRINSIC}
      call exponentiate_to_(X,self,tol)
   end subroutine

   PURE subroutine exponentiate_to(self,U,tol)
   ! Exponentiate the matrix self using a power series expansion, U = exp(self),
   ! so that the maximum deviation from the exact answer is less than "tol"
   ! if present.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: U
      REAL, optional, IN :: tol
      MAT(REAL,:,:), allocatable :: W,V
      INT :: n,k
      REAL :: eps,fac,error
   ENSURE(is_square_(U),"MAT{REAL}:exponentiate_to ... U not square")
   ENSURE(is_same_shape_as_(self,U),"MAT{REAL}:exponentiate_to ... wrong shape")

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

   PURE subroutine antisymmetric_exponential_to(self,U,eval,evec)
   ! Make unitary matrix U = exp(self) where "self" must be antisymmetric.
   ! Uses the formula:  exp A = V (cos P) V^t + V (sin P)/P V^t A
   !                        P = sqrt diag(eig(A^t A))
   ! (c) dylan jayatilaka, university of western australia, 1993
   ! WARNING: Untested in TONTO and looks wrong.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: U
      MAT(REAL,:,:), allocatable, optional, OUT :: evec
      VEC(REAL,:), allocatable, optional, OUT :: eval
      MAT(REAL,:,:), allocatable :: W,v_k
      REAL :: e,e2,cs,sn
      INT :: dim,k
   ENSURE(is_square_(self),"MAT{REAL}:antisymmetric_exponential_to ... self is a non-square matrix")
   ENSURE(is_same_shape_as_(self,U),"MAT{REAL}:antisymmetric_exponential_to ... incompatible shapes")

      ! The following code is inherited from MAT{INTRINSIC}
      dim = size(self,1)
      if (NOT present(eval)) then
         call create_(eval,dim)
      else
         ENSURE(size(eval)>=dim,"MAT{REAL}:antisymmetric_exponential_to ... eval too small")
      end if
      if (NOT present(evec)) then
         call create_(evec,dim,dim)
      else
         ENSURE(size(evec,1)==dim AND size(evec,2)==dim,"MAT{REAL}:antisymmetric_exponential_to ... evec incompatible")
      end if
      call create_(W,dim,dim)
      W = self

      ! U = -self^t*self = self^2, makes U hermitian
      U = matmul(self,W)

      ! diagonalise U ...
      call diagonalize_Jacobi_(U,eval,evec)

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


!   to_matching_rotation(reference,actual,L,fail)
!   ! Returns the rotation matrix which matches coordinates "reference"
!   ! to "actual" in a least squares sense.  actual = U x reference.
!   ! Return the mean fitting error "L" or else "fail"  for failure.
!      self :: OUT
!      reference :: MAT{REAL}, target, IN
!      actual :: MAT{REAL}, IN
!      L    :: REAL, OUT
!      fail :: BIN, OUT
!
!   ENSURE(.is_square,"self must be square")
!   ENSURE(.dim1==actual.dim1,"self incompatible with actual")
!   ENSURE(reference.dim2==actual.dim2,"reference incompatible with actual")
!
!      Rd :: MAT{REAL}@, target
!      U,X :: MAT{REAL}@
!      X0,Xv :: VEC{REAL}@
!      d,iteration :: INT
!      big :: REAL
!
!      ! Set dimension, number of points to fit
!      d = .dim1
!
!      ! Set minimum L value
!      L = ZERO
!
!      ! Start with U being unit matrix
!      .to_unit_mx
!
!      ! Create antisymmetric matrix X, temporary rotation U
!      X.create(d,d)
!      X0.create(d*(d+1)/2)
!      Xv.create(d*(d+1)/2)
!      U.create(d,d)
!      Rd.create(reference.dim1,reference.dim2)
!
!      ! Repeatedly update U until converged
!      iteration = 0
!      fail = FALSE
!      do
!
!         iteration = iteration + 1
!
!         ! Rotate actual positions
!         Rd.to_product_of(self,actual,transpose_A=TRUE)
!
!         ! Determine the gradient Xv at X=0
!         saved_reference => reference
!         saved_actual    => Rd
!         X0 = ZERO
!         MAT{REAL}::match_vectors(X0,L,Xv)
!
!         ! Exit if gradient converged or too many iterations
!         big = maxval(abs(Xv))
!         if (big<TOL(6)) exit
!         fail = iteration==1000
!         if (fail) exit
!
!
!         ! Do a line minimisation along gradient Xv from X=0
!         saved_reference => reference
!         saved_actual    => Rd
!         VEC{REAL}:line_minimize_from_v2(MAT{REAL}::match_vectors,X0,Xv,L,TOL(6))
!
!         ! Exponentiate X and update U
!         X.antisymmetric_unzip_triangle(X0)
!         U.to_exponential_of(X)
!         X.to_product_of(self,U)
!         self = X
!
!      end
!
!      ! Clean up
!      Rd.destroy
!      U.destroy
!      Xv.destroy
!      X0.destroy
!      X.destroy
!
!   end

!   match_vectors(Xv,L,dL) :: selfless, public
!   ! Take Xv, exponentiate it, and determine "L", the overlap between
!   ! "saved_actual" and "saved_reference". Also determine "dL" the
!   ! derivative at Xv=0. NOTE: "dL" is wrong unless Xv=0.
!      Xv :: VEC{REAL}, IN
!      L :: REAL, OUT
!      dL :: VEC{REAL}, optional, OUT
!
!      X,U,Rd :: MAT{REAL}@
!      d,n,p,q,pq,i :: INT
!      val :: REAL
!
!      ! Dimensions
!      d = saved_actual.dim1
!      n = saved_actual.dim2
!
!      ! Temporaries
!      X.create(d,d)
!      U.create(d,d)
!      Rd.create(d,n)
!
!      ! Exponentiate Xv and get L
!      X.antisymmetric_unzip_triangle(Xv)
!      U.to_exponential_of(X)
!      Rd.to_product_of(U,saved_actual,transpose_a=TRUE)
!      Rd = Rd - saved_reference
!      L = Rd.trace_product_with(Rd,transpose_a=TRUE)
!
!      ! Gradient of L
!      if (present(dL)) then
!         Rd = Rd + saved_reference
!         pq = 0
!         do p = 1,d
!         do q = 1,p
!            pq = pq + 1
!            val = ZERO
!            do i = 1,n
!               val = val + Rd(p,i)*saved_reference(q,i) - Rd(q,i)*saved_reference(p,i)
!            end
!            dL(pq) = -TWO*val
!         end
!         end
!      end
!
!      ! Clean up
!      Rd.destroy
!      U.destroy
!      X.destroy
!
!   end

!  =============
!  String widths
!  =============

   pure function str_lengths(self,dp,spaces) result (res)
   ! Returns the minimal string lengths, with optional extra "spaces"
      MAT(REAL,:,:), IN :: self
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
      MAT(REAL,:,:), IN :: self
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
      MAT(REAL,:,:), IN :: self
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
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), allocatable :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:aa_block ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:aa_block ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res = self(1:n,1:n)
   end function

   PURE function ba_block(self) result (res)
   ! Return allocatable the beta-alpha sector of the matrix
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), allocatable :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:ba_block ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:ba_block ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res = self(n+1:2*n,1:n)
   end function

   PURE function ab_block(self) result (res)
   ! Return the allocatable alpha-beta sector of the matrix
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), allocatable :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:ab_block ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:ab_block ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res = self(1:n,n+1:2*n)
   end function

   PURE function bb_block(self) result (res)
   ! Return the allocatable beta-beta sector of the matrix
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), allocatable :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:bb_block ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:bb_block ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res = self(n+1:2*n,n+1:2*n)
   end function


!  Spin block PTR returning routines

   function aa_block_ptr(self) result (res)
   ! Return the pointer alpha-alpha sector of the matrix
      MAT(REAL,:,:), target, IN :: self
      MAT(REAL,:,:), PTR :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:aa_block_ptr ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:aa_block_ptr ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res => self(1:n,1:n)
   end function

   function ba_block_ptr(self) result (res)
   ! Return the pointer beta-alpha sector of the matrix
      MAT(REAL,:,:), target, IN :: self
      MAT(REAL,:,:), PTR :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:ba_block_ptr ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:ba_block_ptr ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res => self(n+1:2*n,1:n)
   end function

   function ab_block_ptr(self) result (res)
   ! Return the pointer alpha-beta sector of the matrix
      MAT(REAL,:,:), target, IN :: self
      MAT(REAL,:,:), PTR :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:ab_block_ptr ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:ab_block_ptr ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res => self(1:n,n+1:2*n)
   end function

   function bb_block_ptr(self) result (res)
   ! Return the pointer beta-beta sector of the matrix
      MAT(REAL,:,:), target, IN :: self
      MAT(REAL,:,:), PTR :: res
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:bb_block_ptr ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:bb_block_ptr ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      res => self(n+1:2*n,n+1:2*n)
   end function


!  Spin block set routines

   PURE subroutine set_aa_block_to_0(self,X)
   ! Set the alpha-alpha sector of the matrix to "X"
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:set_aa_block_to_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:set_aa_block_to_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = X
   end subroutine

   PURE subroutine set_ba_block_to_0(self,X)
   ! Set the beta-alpha sector of the matrix to "X"
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:set_ba_block_to_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:set_ba_block_to_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = X
   end subroutine

   PURE subroutine set_ab_block_to_0(self,X)
   ! Set the alpha-beta sector of the matrix to "X"
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:set_ab_block_to_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:set_ab_block_to_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = X
   end subroutine

   PURE subroutine set_bb_block_to_0(self,X)
   ! Set the beta-beta sector of the matrix to "X"
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:set_bb_block_to_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:set_bb_block_to_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = X
   end subroutine

   PURE subroutine set_aa_block_to_1(self,X,fac)
   ! Set the alpha-alpha sector of the matrix to "X"
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:set_aa_block_to_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:set_aa_block_to_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = fac*X
   end subroutine

   PURE subroutine set_ba_block_to_1(self,X,fac)
   ! Set the beta-alpha sector of the matrix to "X"
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:set_ba_block_to_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:set_ba_block_to_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = fac*X
   end subroutine

   PURE subroutine set_ab_block_to_1(self,X,fac)
   ! Set the alpha-beta sector of the matrix to "X"
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:set_ab_block_to_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:set_ab_block_to_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = fac*X
   end subroutine

   PURE subroutine set_bb_block_to_1(self,X,fac)
   ! Set the beta-beta sector of the matrix to "X"
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:set_bb_block_to_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:set_bb_block_to_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = fac*X
   end subroutine


!  Spin block put routines

   PURE subroutine put_aa_block_to(self,X)
   ! Put the alpha-alpha sector of the matrix to "X"
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:put_aa_block_to ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:put_aa_block_to ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      X = (self(1:n,1:n))
   end subroutine

   PURE subroutine put_ba_block_to(self,X)
   ! Put the beta-alpha sector of the matrix to "X"
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:put_ba_block_to ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:put_ba_block_to ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      X = (self(n+1:2*n,1:n))
   end subroutine

   PURE subroutine put_ab_block_to(self,X)
   ! Put the alpha-beta sector of the matrix to "X"
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:put_ab_block_to ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:put_ab_block_to ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      X = (self(1:n,n+1:2*n))
   end subroutine

   PURE subroutine put_bb_block_to(self,X)
   ! Put the beta-beta sector of the matrix to "X"
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:put_bb_block_to ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:put_bb_block_to ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      X = (self(n+1:2*n,n+1:2*n))
   end subroutine


!  Spin block plus routines

   PURE subroutine aa_block_plus_0(self,X)
   ! Add "X" to the alpha-alpha sector of the matrix
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:aa_block_plus_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:aa_block_plus_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = self(1:n,1:n)+X
   end subroutine

   PURE subroutine ba_block_plus_0(self,X)
   ! Add "X" to the beta-alpha sector of the matrix
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:ba_block_plus_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:ba_block_plus_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = self(n+1:2*n,1:n)+X
   end subroutine

   PURE subroutine ab_block_plus_0(self,X)
   ! Add "X" to the alpha-beta sector of the matrix
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:ab_block_plus_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:ab_block_plus_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = self(1:n,n+1:2*n)+X
   end subroutine

   PURE subroutine bb_block_plus_0(self,X)
   ! Add "X" to the beta-beta sector of the matrix
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:bb_block_plus_0 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:bb_block_plus_0 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n)+X
   end subroutine

   PURE subroutine aa_block_plus_1(self,X,fac)
   ! Add "X" to the alpha-alpha sector of the matrix
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:aa_block_plus_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:aa_block_plus_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,1:n) = self(1:n,1:n)+fac*X
   end subroutine

   PURE subroutine ba_block_plus_1(self,X,fac)
   ! Add "X" to the beta-alpha sector of the matrix
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:ba_block_plus_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:ba_block_plus_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,1:n) = self(n+1:2*n,1:n)+fac*X
   end subroutine

   PURE subroutine ab_block_plus_1(self,X,fac)
   ! Add "X" to the alpha-beta sector of the matrix
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:ab_block_plus_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:ab_block_plus_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(1:n,n+1:2*n) = self(1:n,n+1:2*n)+fac*X
   end subroutine

   PURE subroutine bb_block_plus_1(self,X,fac)
   ! Add "X" to the beta-beta sector of the matrix
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      REAL, IN :: fac
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:bb_block_plus_1 ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"MAT{REAL}:bb_block_plus_1 ... not even-dimensioned")

      ! The following code is inherited from MAT{INTRINSIC}
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n)+fac*X
   end subroutine


!  General block putting

   PURE subroutine put_blocks_to(self,X,block_dim,block_list)
   ! Put from "self" the blocks listed in "block_list" into "X".
   ! Each block is square and has dimension "block_dim".
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: X
      INT, IN :: block_dim
      VEC(INT,:), IN :: block_list
      INT :: n,nb,i,j,ix,ib,jx,jb
   ENSURE(mod(size(self,1),block_dim)==0,"MAT{REAL}:put_blocks_to ... block_dim is not a divisor of dim1")
   ENSURE(mod(size(self,2),block_dim)==0,"MAT{REAL}:put_blocks_to ... block_dim is not a divisor of dim2")
   ENSURE(is_square_(X),"MAT{REAL}:put_blocks_to ... X is non-square matrix")
   ENSURE(size(X,1)==size(block_list)*block_dim,"MAT{REAL}:put_blocks_to ... wrong shape, X")

      ! Convenient dimensions

      ! The following code is inherited from MAT{INTRINSIC}
      n = block_dim
      nb = size(block_list)

      ! Assign the blocks
      do i = 1,nb
         ix = (i-1)*n
         ib = (block_list(i)-1)*n
         do j = 1,nb
            jx = (j-1)*n
            jb = (block_list(j)-1)*n
            X(ix+1:ix+n,jx+1:jx+n) = self(ib+1:ib+n,jb+1:jb+n)
         end do
      end do
   end subroutine

   PURE subroutine put_sub_blocks_to_0(self,X,dim,block_dims,block_list)
   ! Put from "self" into "X" the blocks of size "dim" listed in
   ! "block_list", but where the dimensions of the blocks in self are
   ! given in "block_dims".
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: X
      INT, IN :: dim
      VEC(INT,:), IN :: block_dims
      VEC(INT,:), optional, IN :: block_list
! ENSURE(all(block_list>=1),"block_list has non +ve elements")
! ENSURE(all(block_list<=block_dims.dim),"block_list has elements which are too large")
      INT :: nb,i,ib,ix,is,j,jb,jx,js
      VEC(INT,:), allocatable :: list
   ENSURE(is_square_(self),"MAT{REAL}:put_sub_blocks_to_0 ... self is non-square mjatrix")
   ENSURE(is_square_(X),"MAT{REAL}:put_sub_blocks_to_0 ... X is non-square matrix")
   ENSURE(size(self,1)==sum(block_dims),"MAT{REAL}:put_sub_blocks_to_0 ... self is incorrectly dimensioned")
   ENSURE(size(X,1)==size(block_dims)*dim,"MAT{REAL}:put_sub_blocks_to_0 ... X is incorrectly dimensioned")

      ! Get list of blocks

      ! The following code is inherited from MAT{INTRINSIC}
      if (present(block_list)) then

         ! Limited blocks
         nb = size(block_list)
         list = block_list
      else

         ! All blocks
         nb = size(block_dims)
         list = [(i,i = 1,nb)]
      end if

      ! Row block offsets
      ix = 0
      is = 0
      do i = 1,nb

         ! Row block
         ib = list(i)

         ! Col block offsets
         jx = 0
         js = 0
         do j = 1,nb

            ! Col block
            jb = list(j)

            ! Copy
            X(ix+1:ix+dim,jx+1:jx+dim) = self(is+1:is+dim,js+1:js+dim)

            ! Next col block
            jx = jx+dim
            js = js+block_dims(jb)
         end do

         ! Next row block
         ix = ix+dim
         is = is+block_dims(ib)
      end do
   end subroutine

   PURE subroutine put_sub_blocks_to_1(self,X,block_dim,X_block_dim,block_list,X_block_offset)
   ! Put from "self" into "X" the blocks listed in "block_list" BUT
   ! only the square sub blocks of size "X_block_dim" which start from
   ! "X_block_offset"+1 past a block boundary are copied into blocks
   ! of "X" i.e. a zero offset (default) assumes the first
   ! "X_block_dim" elements are copied from blocks of "self" into "X".
      MAT(REAL,:,:), IN :: self
      MAT(REAL,:,:), OUT :: X
      INT, IN :: block_dim,X_block_dim
      VEC(INT,:), optional, IN :: block_list
      INT, optional, IN :: X_block_offset
      INT :: offset,ns,nx,nb,i,j,ix,ib,jx,jb
      VEC(INT,:), allocatable :: list
   ENSURE(mod(size(self,1),block_dim)==0,"MAT{REAL}:put_sub_blocks_to_1 ... block_dim is not a divisor of dim1")
   ENSURE(mod(size(self,2),block_dim)==0,"MAT{REAL}:put_sub_blocks_to_1 ... block_dim is not a divisor of dim2")
   ENSURE(is_square_(X),"MAT{REAL}:put_sub_blocks_to_1 ... X is non-square matrix")

      ! Convenient dimensions

      ! The following code is inherited from MAT{INTRINSIC}
      ns = block_dim
      nx = X_block_dim
      if (present(block_list)) then

         ! Only blocks in "block_list"
         nb = size(block_list)
         list = block_list
      else

         ! Assume all blocks
         nb = size(self,1)/block_dim
         call create_(list,nb)
         list = [(i,i = 1,nb)]
      end if

      ! Assume zero offset
      offset = 0
      if (present(X_block_offset)) offset = X_block_offset

      ! Further tests ...
      ENSURE(X_block_dim+offset<=block_dim,"MAT{REAL}:put_sub_blocks_to_1 ... incompatible block offsets")
      ENSURE(size(X,1)==size(list)*X_block_dim,"MAT{REAL}:put_sub_blocks_to_1 ... wrong shape, X")

      ! Assign the blocks
      do i = 1,nb
         ix = (i-1)*nx
         ib = (list(i)-1)*ns+offset
         do j = 1,nb
            jx = (j-1)*nx
            jb = (list(j)-1)*ns+offset
            X(ix+1:ix+nx,jx+1:jx+nx) = self(ib+1:ib+nx,jb+1:jb+nx)
         end do
      end do

      ! Clean
      call destroy_(list)
   end subroutine

   PURE subroutine set_to_u_from_r(self,X)
   ! Set the alpha-alpha and beta-beta sectors
      MAT(REAL,:,:), INOUT :: self
      MAT(REAL,:,:), IN :: X
      INT :: n
   ENSURE(is_square_(self),"MAT{REAL}:set_to_u_from_r ... self is not square")
   ENSURE(is_square_(X),"MAT{REAL}:set_to_u_from_r ... X is not square")
   ENSURE(is_even_(size(X,1)),"MAT{REAL}:set_to_u_from_r ... X is not even-dimensioned")
   ENSURE(size(self,1)==2*size(X,1),"MAT{REAL}:set_to_u_from_r ... incompatible dimension, X")
      n = size(X)
      self = ZERO
      self(1:n,1:n) = X
      self(n+1:,n+1:) = X
   end subroutine


!  =====================
!  Conversion to strings
!  =====================

   pure function to_str(self,style,width,precision,left_justify) result (res)
   ! Change self to a string with specified "style", "width" and
   ! "precision" as defined in the fortran standard
      MAT(REAL,:,:), IN :: self
      STR(len=*), IN :: style
      INT, IN :: width,precision
      BIN, optional, IN :: left_justify
      MAT(STR(len=STR_SIZE),size(self,1),size(self,2)) :: res
      INT :: i,j

      ! The following code is inherited from MAT{INTRINSIC}
      do i = 1,size(self,1)
         do j = 1,size(self,2)
            res(i,j) = to_str_(self(i,j),style,width,precision,left_justify)
         end do
      end do
   end function


!  ===============
!  Unit conversion
!  ===============

   subroutine convert_to(self,units)
   ! Convert the number "self" in atomic units or generic units to a
   ! new number in "units".
      MAT(REAL,:,:), INOUT :: self
      STR(len=*), IN :: units
      REAL :: factor
   ENSURE(is_known_unit_(units),"MAT{REAL}:convert_to ... unknown units, "//units)
      factor = conversion_factor_(units)
      self = self*factor
   end subroutine

   subroutine convert_from(self,units)
   ! Convert the number "self" from "units" system to a new number
   ! in atomic units or generic units.  Returns "err" whether it was successful.
      MAT(REAL,:,:), INOUT :: self
      STR(len=*), IN :: units
      REAL :: factor
   ENSURE(is_known_unit_(units),"MAT{REAL}:convert_from ... unknown units, "//units)
      factor = ONE/(conversion_factor_(units))
      self = self*factor
   end subroutine


!  ==================================================
!  Gaussian function rotation representation matrices
!  =================================================

   PURE function gaussian_d_xyz_mx(self) result (dtr)
   ! Return the representation matrix for a d-type xyz product found in gaussian
   ! shells, induced by an xyz rotation matrix "self". The matrix representation
   ! induced is: d = d' * dtr, when the coordinates r of the gaussian shell
   ! functions are written in terms of new coordinates r' as: r = self^T * r'.
      MAT(REAL,:,:), IN :: self
      MAT(REAL,6,6) :: dtr
      INT :: i,i1,i2
      VEC(INT,6) :: d1,d2
   ENSURE(is_square_(self),"MAT{REAL}:gaussian_d_xyz_mx ... self not square")
   ENSURE(size(self,1)==3,"MAT{REAL}:gaussian_d_xyz_mx ... wrong size, self")
      d1 = [1,2,3,1,1,2]
      d2 = [1,2,3,2,3,3]
      do i = 1,6  ! loop on old coordinates
         i1 = d1(i)
         i2 = d2(i)
         dtr(1,i) = self(1,i1)*self(1,i2)
         dtr(2,i) = self(2,i1)*self(2,i2)
         dtr(3,i) = self(3,i1)*self(3,i2)
         dtr(4,i) = self(1,i1)*self(2,i2)+self(2,i1)*self(1,i2)
         dtr(5,i) = self(1,i1)*self(3,i2)+self(3,i1)*self(1,i2)
         dtr(6,i) = self(2,i1)*self(3,i2)+self(3,i1)*self(2,i2)
      end do
   end function

   PURE function gaussian_f_xyz_mx(self) result (ftr)
   ! Return the representation matrix for an f xyz product found in gaussian
   ! shells from a p-type xyz matrix
      MAT(REAL,:,:), IN :: self
      MAT(REAL,10,10) :: ftr
      INT :: i,i1,i2,i3
      VEC(INT,10) :: f1,f2,f3
   ENSURE(is_square_(self),"MAT{REAL}:gaussian_f_xyz_mx ... self not square")
   ENSURE(size(self,1)==3,"MAT{REAL}:gaussian_f_xyz_mx ... wrong size, self")
      f1 = [1,2,3,1,1,2,2,3,3,1]
      f2 = [1,2,3,1,1,2,2,3,3,2]
      f3 = [1,2,3,2,3,1,3,1,2,3]
      do i = 1,10
         i1 = f1(i)
         i2 = f2(i)
         i3 = f3(i)
         ftr(1,i) = self(1,i1)*self(1,i2)*self(1,i3)
         ftr(2,i) = self(2,i1)*self(2,i2)*self(2,i3)
         ftr(3,i) = self(3,i1)*self(3,i2)*self(3,i3)
         ftr(4,i) = self(1,i1)*self(1,i2)*self(2,i3)+self(1,i1)*self(2,i2)*self(1,i3)+self(2,i1)*self(1,i2)*self(1,i3)
         ftr(5,i) = self(1,i1)*self(1,i2)*self(3,i3)+self(1,i1)*self(3,i2)*self(1,i3)+self(3,i1)*self(1,i2)*self(1,i3)
         ftr(6,i) = self(1,i1)*self(2,i2)*self(2,i3)+self(2,i1)*self(1,i2)*self(2,i3)+self(2,i1)*self(2,i2)*self(1,i3)
         ftr(7,i) = self(3,i1)*self(2,i2)*self(2,i3)+self(2,i1)*self(3,i2)*self(2,i3)+self(2,i1)*self(2,i2)*self(3,i3)
         ftr(8,i) = self(1,i1)*self(3,i2)*self(3,i3)+self(3,i1)*self(1,i2)*self(3,i3)+self(3,i1)*self(3,i2)*self(1,i3)
         ftr(9,i) = self(2,i1)*self(3,i2)*self(3,i3)+self(3,i1)*self(2,i2)*self(3,i3)+self(3,i1)*self(3,i2)*self(2,i3)
         ftr(10,i) = self(1,i1)*self(2,i2)*self(3,i3)+self(1,i1)*self(3,i2)*self(2,i3)+self(2,i1)*self(1,i2)*self(3,i3)+self(2,i1)*self(3,i2)*self(1,i3)+self(3,i1)*self(1,i2)*self(2,i3)+self(3,i1)*self(2,i2)*self(1,i3)
      end do
   end function

   PURE function gaussian_g_xyz_mx(self) result (gtr)
   ! Return the representation matrix for a g xyz product found in gaussian
   ! shells from a p-type xyz matrix
      MAT(REAL,:,:), IN :: self
      MAT(REAL,15,15) :: gtr
      INT :: i,i1,i2,i3,i4
      VEC(INT,15) :: g1,g2,g3,g4
   ENSURE(is_square_(self),"MAT{REAL}:gaussian_g_xyz_mx ... self not square")
   ENSURE(size(self,1)==3,"MAT{REAL}:gaussian_g_xyz_mx ... wrong size, self")
      g1 = [1,2,3,1,1,2,2,3,3,1,1,2,1,2,3]
      g2 = [1,2,3,1,1,2,2,3,3,1,1,2,1,2,3]
      g3 = [1,2,3,1,1,2,2,3,3,2,3,3,2,1,1]
      g4 = [1,2,3,2,3,1,3,1,2,2,3,3,3,3,2]
      do i = 1,15
         i1 = g1(i)
         i2 = g2(i)
         i3 = g3(i)
         i4 = g4(i)
         gtr(1,i) = self(1,i1)*self(1,i2)*self(1,i3)*self(1,i4)
         gtr(2,i) = self(2,i1)*self(2,i2)*self(2,i3)*self(2,i4)
         gtr(3,i) = self(3,i1)*self(3,i2)*self(3,i3)*self(3,i4)
         gtr(4,i) = self(1,i1)*self(1,i2)*self(1,i3)*self(2,i4)+self(1,i1)*self(1,i2)*self(2,i3)*self(1,i4)+self(1,i1)*self(2,i2)*self(1,i3)*self(1,i4)+self(2,i1)*self(1,i2)*self(1,i3)*self(1,i4)
         gtr(5,i) = self(1,i1)*self(1,i2)*self(1,i3)*self(3,i4)+self(1,i1)*self(1,i2)*self(3,i3)*self(1,i4)+self(1,i1)*self(3,i2)*self(1,i3)*self(1,i4)+self(3,i1)*self(1,i2)*self(1,i3)*self(1,i4)
         gtr(6,i) = self(1,i1)*self(2,i2)*self(2,i3)*self(2,i4)+self(2,i1)*self(1,i2)*self(2,i3)*self(2,i4)+self(2,i1)*self(2,i2)*self(1,i3)*self(2,i4)+self(2,i1)*self(2,i2)*self(2,i3)*self(1,i4)
         gtr(7,i) = self(3,i1)*self(2,i2)*self(2,i3)*self(2,i4)+self(2,i1)*self(3,i2)*self(2,i3)*self(2,i4)+self(2,i1)*self(2,i2)*self(3,i3)*self(2,i4)+self(2,i1)*self(2,i2)*self(2,i3)*self(3,i4)
         gtr(8,i) = self(1,i1)*self(3,i2)*self(3,i3)*self(3,i4)+self(3,i1)*self(1,i2)*self(3,i3)*self(3,i4)+self(3,i1)*self(3,i2)*self(1,i3)*self(3,i4)+self(3,i1)*self(3,i2)*self(3,i3)*self(1,i4)
         gtr(9,i) = self(2,i1)*self(3,i2)*self(3,i3)*self(3,i4)+self(3,i1)*self(2,i2)*self(3,i3)*self(3,i4)+self(3,i1)*self(3,i2)*self(2,i3)*self(3,i4)+self(3,i1)*self(3,i2)*self(3,i3)*self(2,i4)
         gtr(10,i) = self(1,i1)*self(1,i2)*self(2,i3)*self(2,i4)+self(1,i1)*self(2,i2)*self(1,i3)*self(2,i4)+self(1,i1)*self(2,i2)*self(2,i3)*self(1,i4)+self(2,i1)*self(1,i2)*self(1,i3)*self(2,i4)+self(2,i1)*self(1,i2)*self(2,i3)*self(1,i4)+self(2,i1)*self(2,i2)*self(1,i3)*self(1,i4)
         gtr(11,i) = self(1,i1)*self(1,i2)*self(3,i3)*self(3,i4)+self(1,i1)*self(3,i2)*self(1,i3)*self(3,i4)+self(1,i1)*self(3,i2)*self(3,i3)*self(1,i4)+self(3,i1)*self(1,i2)*self(1,i3)*self(3,i4)+self(3,i1)*self(1,i2)*self(3,i3)*self(1,i4)+self(3,i1)*self(3,i2)*self(1,i3)*self(1,i4)
         gtr(12,i) = self(2,i1)*self(2,i2)*self(3,i3)*self(3,i4)+self(2,i1)*self(3,i2)*self(2,i3)*self(3,i4)+self(2,i1)*self(3,i2)*self(3,i3)*self(2,i4)+self(3,i1)*self(2,i2)*self(2,i3)*self(3,i4)+self(3,i1)*self(2,i2)*self(3,i3)*self(2,i4)+self(3,i1)*self(3,i2)*self(2,i3)*self(2,i4)
         gtr(13,i) = self(1,i1)*self(1,i2)*self(2,i3)*self(3,i4)+self(1,i1)*self(1,i2)*self(3,i3)*self(2,i4)+self(1,i1)*self(2,i2)*self(1,i3)*self(3,i4)+self(1,i1)*self(2,i2)*self(3,i3)*self(1,i4)+self(1,i1)*self(3,i2)*self(1,i3)*self(2,i4)+self(1,i1)*self(3,i2)*self(2,i3)*self(1,i4)+self(2,i1)*self(1,i2)*self(1,i3)*self(3,i4)+self(2,i1)*self(1,i2)*self(3,i3)*self(1,i4)+self(2,i1)*self(3,i2)*self(1,i3)*self(1,i4)+self(3,i1)*self(1,i2)*self(1,i3)*self(2,i4)+self(3,i1)*self(1,i2)*self(2,i3)*self(1,i4)+self(3,i1)*self(2,i2)*self(1,i3)*self(1,i4)
         gtr(14,i) = self(2,i1)*self(2,i2)*self(1,i3)*self(3,i4)+self(2,i1)*self(2,i2)*self(3,i3)*self(1,i4)+self(2,i1)*self(1,i2)*self(2,i3)*self(3,i4)+self(2,i1)*self(1,i2)*self(3,i3)*self(2,i4)+self(2,i1)*self(3,i2)*self(2,i3)*self(1,i4)+self(2,i1)*self(3,i2)*self(1,i3)*self(2,i4)+self(1,i1)*self(2,i2)*self(2,i3)*self(3,i4)+self(1,i1)*self(2,i2)*self(3,i3)*self(2,i4)+self(1,i1)*self(3,i2)*self(2,i3)*self(2,i4)+self(3,i1)*self(2,i2)*self(2,i3)*self(1,i4)+self(3,i1)*self(2,i2)*self(1,i3)*self(2,i4)+self(3,i1)*self(1,i2)*self(2,i3)*self(2,i4)
         gtr(15,i) = self(3,i1)*self(3,i2)*self(1,i3)*self(2,i4)+self(3,i1)*self(3,i2)*self(2,i3)*self(1,i4)+self(3,i1)*self(1,i2)*self(3,i3)*self(2,i4)+self(3,i1)*self(1,i2)*self(2,i3)*self(3,i4)+self(3,i1)*self(2,i2)*self(3,i3)*self(1,i4)+self(3,i1)*self(2,i2)*self(1,i3)*self(3,i4)+self(1,i1)*self(3,i2)*self(3,i3)*self(2,i4)+self(1,i1)*self(3,i2)*self(2,i3)*self(3,i4)+self(1,i1)*self(2,i2)*self(3,i3)*self(3,i4)+self(2,i1)*self(3,i2)*self(3,i3)*self(1,i4)+self(2,i1)*self(3,i2)*self(1,i3)*self(3,i4)+self(2,i1)*self(1,i2)*self(3,i3)*self(3,i4)
      end do
   end function


! ======================
! Binning and histograms
! ======================

   PURE subroutine to_histogram_from_data(self,data,X,Y,X_min,X_max,X_bin,Y_min,Y_max,Y_bin)
   ! Set "self" to a histogram matrix whose ij-th element contains the
   ! *sum* of the values in array "data" whose elements correspond to
   ! the ij-th bin in "X" and "Y", where a 2D bin is a range of
   ! values in "X" (and "Y) of side length "X_bin" (and "Y_bin")
   ! beginning at "X_min" (and "Y_min") and ending at "X_max" (and
   ! "Y_max"). The dimension of self is calculated here.
   ! NOTE: Don't use binned data to calculate properties unless you
   ! really need to coarse grain.
      MAT(REAL,:,:), allocatable, OUT :: self
      VEC(REAL,:), IN :: data,X,Y
      REAL, IN :: X_min,X_max,X_bin
      REAL, IN :: Y_min,Y_max,Y_bin
      INT :: n_X,n_Y,i,j,k
      REAL :: X_ran,Y_ran
   ENSURE(size(data)==size(X),"MAT{REAL}:to_histogram_from_data ... incompatible data and X descriptor")
   ENSURE(size(data)==size(Y),"MAT{REAL}:to_histogram_from_data ... incompatible data and Y descriptor")
   ENSURE(X_max>X_min,"MAT{REAL}:to_histogram_from_data ... X_max is smaller than X_min!")
   ENSURE(Y_max>Y_min,"MAT{REAL}:to_histogram_from_data ... Y_max is smaller than Y_min!")
   ENSURE(X_bin<(X_max-X_min),"MAT{REAL}:to_histogram_from_data ... X_bin size is larger than [X_min,X_max]")
   ENSURE(Y_bin<(Y_max-Y_min),"MAT{REAL}:to_histogram_from_data ... X_bin size is larger than [X_min,X_max]")

      ! The range of value ...
      X_ran = X_max-X_min
      Y_ran = Y_max-Y_min

      ! The number of bins ...
      n_X = ceiling(X_ran/X_bin)
      n_Y = ceiling(Y_ran/Y_bin)

      ! Create the histogram (leaky)
      call create_(self,n_X,n_Y)
      self = ZERO

      ! Now do the binning ...
      do k = 1,size(data)

         ! Get the bins
         i = ceiling(min(X(k)-X_min,X_ran)/X_bin)
         j = ceiling(min(Y(k)-Y_min,Y_ran)/Y_bin)

         ! Accumulate
         self(i,j) = self(i,j)+data(k)
      end do
   end subroutine


!  ==========
!  Misc stuff
!  ==========

   subroutine make_enclosing_sphere(self,pos,radius)
   ! Determine the position and radius of a sphere that encloses all points in
   ! the grid.
      MAT(REAL,:,:), IN :: self
      REAL, OUT :: radius
      VEC(REAL,3), OUT :: pos
      VEC(REAL,3) :: diff
      REAL :: dist
      INT :: n,n_pts
   ENSURE(size(self,2)==3,"MAT{REAL}:make_enclosing_sphere ... Second dimension of matrix is not 3.")
      n_pts = size(self,2)
      ! Get the center of the sphere.  Should use a better algorithm than just the
      ! average.
      pos = ZERO
      do n = 1,n_pts
         pos = pos+self(n,:)
      end do
      pos = pos/n_pts
      ! The radius is the distance to the furthest point.
      radius = 0
      do n = 1,n_pts
         diff = self(n,:)-pos
         dist = dot_product(diff,diff)
         if (dist>radius) radius = dist
      end do
      radius = sqrt(radius)
   end subroutine


!   make_corresponding_orbitals(left,right,theta,p)
!   ! This algorithm from Camp and King, J. Chem Phys. Vol 75(1), pp 268-274.
!   ! p is the dimenstion of the first block of the partitioned matrices.
!   ! Works best if "left" and "right" matrices are nonzero.
!      self  :: target, INOUT
!      left  :: MAT{REAL}, target
!      right :: MAT{REAL}, target
!      theta :: VEC{REAL}, OUT
!      p     :: INT, IN
!
!   ENSURE(.is_square,"non-square matrix")
!   ENSURE(.is_same_shape_as(right),"right is incompatible")
!   ENSURE(.is_same_shape_as(left),"left is incompatible")
!   ENSURE(size(theta)==min(.dim1,.dim1-p),"theta has wrong size")
!
!      Vp,Vq,Wp,Wq,M,Up,Uq :: MAT{REAL}*
!      M,MWq,Hq :: MAT{REAL}@
!      lambda :: VEC{REAL}@
!      minpq,q,n :: INT
!
!      n = .dim1
!      q = n - p
!      minpq = min(p,q)
!
!      ! I've only tested this for q>p.  Suspect p>q does not work.
!      Vp =>  left(  1:p,  1:p)
!      Vq =>  left(p+1:n,p+1:n)
!      Wp => right(  1:p,  1:p)
!      Wq => right(p+1:n,p+1:n)
!      M  =>  self(  1:p,p+1:n)
!      Up =>  self(  1:p,  1:p)
!      Uq =>  self(p+1:n,p+1:n)
!
!      right(  1:p,p+1: ) = ZERO
!      right(p+1:n,  1:p) = ZERO
!       left(  1:p,p+1:n) = ZERO
!       left(p+1:n,   1:p) = ZERO
!
!      .zero_small_values(TOL(10))
!
!      lambda.create(q)                       ! get eigenvalues and Wq.
!      Hq.create(q,q)
!      Hq.to_product_of(M,M,transpose_a=TRUE)
!      Hq.solve_symmetric_eigenproblem(lambda,Wq)
!      Hq.destroy
!
!      lambda.reverse_order                   ! get rotation angles, largest first.
!      theta = lambda(1:minpq)
!      lambda.destroy
!      theta.zero_small_values(TOL(10))
!
!   ENSURE(minval(theta)>=ZERO,"eigenvalues less than zero!")
!   ENSURE(maxval(theta)<=ONE,"eigenvalues more than one!")
!
!      theta = min(theta,ONE)
!      theta = max(theta,ZERO)
!
!      Wq.zero_small_values(TOL(10))          ! get Vp
!      Wq.reverse_column_order
!      MWq.create(p,q)
!      MWq.to_product_of(M,Wq)
!      Vp = MWq(1:p,1:p)
!      MWq.destroy
!      Vp.schmidt_orthonormalize(theta)
!
!      Vq.to_product_of(Uq,Wq)                   ! get Vq
!      Vq.reverse_schmidt_orthogonalise
!
!      Wp.to_product_of(Up,Vp,transpose_a=TRUE)  ! get Wp
!      Wp.reverse_schmidt_orthogonalise
!
!      theta = sqrt(theta)
!      theta = asin(theta)
!
!   end

   PURE subroutine to_multipole_W_translation_mx(self,R,l_max)
   ! Make the multipole translation matrix W(lm,jk)(R) evaluated at position "R"
   ! where the maximum values of l is "l_max". See the book by Helgaker, Olsen,
   ! and Simons, equations (9.13.58)--(9.13.61) and (9.13.65), pp. 413-414.
   ! NOTE: self is created.
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,3), IN :: R
      INT, IN :: l_max
      VEC(type(EVEC_REAL_TYPE),:), allocatable, target :: Rc,Rs
      VEC(REAL,:), PTR :: Rc_lj,Rs_lj
      INT :: j,k,jk,l,m,lm,sk,lj,mmk,mpk
      REAL :: fac
   ENSURE(l_max>=0,"MAT{REAL}:to_multipole_W_translation_mx ... l_max must be non-negative")
   ENSURE(is_square_(self),"MAT{REAL}:to_multipole_W_translation_mx ... self must be square")
   ENSURE(size(self,1)==(l_max+1)*(l_max+1),"MAT{REAL}:to_multipole_W_translation_mx ... self has wrong dimension")

      ! Note R, NOT -R, due to mistake (?) in Helgaker's book
      call make_R_harmonics_(Rc,Rs,R,l_max)

      ! Set self to zero, mainly for upper triangle
      self = ZERO

      ! Now make the translation matrix
      jk = 0
      do j = 0,l_max
         sk = 1
         if (is_even_(j)) sk = -1
         ! First do k<0
         do k = -j,-1
            jk = jk+1
            sk = -sk  ! initially,  (-1)==j
            fac = ONE
            if (k==0) fac = HALF
            lm = j*j
            do l = j,l_max  ! j<=l
               lj = l-j
               Rc_lj => Rc(lj)%element
               Rs_lj => Rs(lj)%element
               ! First do m<0
               do m = -l,-1
                  lm = lm+1
                  mmk = m-k
                  mpk = m+k
                  if (lj>=abs(mmk)) self(lm,jk) = self(lm,jk)+Rc_lj(mmk)  ! W^ss
                  if (lj>=abs(mpk)) self(lm,jk) = self(lm,jk)-sk*Rc_lj(mpk)  ! W^ss
               end do
               ! Now do m>=0
               do m = 0,l
                  lm = lm+1
                  mmk = m-k
                  mpk = m+k
                  if (lj>=abs(mmk)) self(lm,jk) = self(lm,jk)-Rs_lj(mmk)  ! W^cs
                  if (lj>=abs(mpk)) self(lm,jk) = self(lm,jk)+sk*Rs_lj(mpk)  ! W^cs
               end do
            end do
         end do

         ! Now do k>=0
         do k = 0,j
            jk = jk+1
            sk = -sk  ! initially,  (-1)==j, now continuing on
            fac = ONE
            if (k==0) fac = HALF
            lm = j*j
            do l = j,l_max  ! j<=l
               lj = l-j
               Rc_lj => Rc(lj)%element
               Rs_lj => Rs(lj)%element
               ! First do m<0
               do m = -l,-1
                  lm = lm+1
                  mmk = m-k
                  mpk = m+k
                  if (lj>=abs(mmk)) self(lm,jk) = self(lm,jk)+fac*Rs_lj(mmk)  ! W^sc
                  if (lj>=abs(mpk)) self(lm,jk) = self(lm,jk)+sk*fac*Rs_lj(mpk)  ! W^sc
               end do
               ! Now do m>=0
               do m = 0,l
                  lm = lm+1
                  mmk = m-k
                  mpk = m+k
                  if (lj>=abs(mmk)) self(lm,jk) = self(lm,jk)+fac*Rc_lj(mmk)  ! W^cc
                  if (lj>=abs(mpk)) self(lm,jk) = self(lm,jk)+sk*fac*Rc_lj(mpk)  ! W^cc
               end do
            end do
         end do
      end do
      !           if (m>=0 AND k>=0) self(lm,jk) = fac*(Rc_lj(m-k) + sk*Rc_lj(m+k)) ! W^cc
      !           if (m>=0 AND k<0 ) self(lm,jk) =     -Rs_lj(m-k) + sk*Rs_lj(m+k)  ! W^cs
      !           if (m<0  AND k>=0) self(lm,jk) = fac*(Rs_lj(m-k) + sk*Rs_lj(m+k)) ! W^sc
      !           if (m<0  AND k<0 ) self(lm,jk) =      Rc_lj(m-k) - sk*Rc_lj(m+k)  ! W^ss

      ! Must deallocate Rc, Rs
      do L = l_max,0,-1
         deallocate(Rs(L)%element)
         deallocate(Rc(L)%element)
      end do
      deallocate(Rs)
      deallocate(Rc)
   end subroutine

   PURE subroutine add_multipole_W_translation_mx(self,R,l_max)
   ! Add the multipole translation matrix W(lm,jk)(R) evaluated at position "R"
   ! where the maximum values of l is "l_max". See the book by Helgaker, Olsen,
   ! and Simons, equations (9.13.58)--(9.13.61) and (9.13.65), pp. 413-414.
   ! NOTE: self is created.
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,3), IN :: R
      INT, IN :: l_max
      VEC(type(EVEC_REAL_TYPE),:), allocatable, target :: Rc,Rs
      VEC(REAL,:), PTR :: Rc_lj,Rs_lj
      INT :: j,k,jk,l,m,lm,sk,lj,mmk,mpk
      REAL :: fac
   ENSURE(l_max>=0,"MAT{REAL}:add_multipole_W_translation_mx ... l_max must be non-negative")
   ENSURE(is_square_(self),"MAT{REAL}:add_multipole_W_translation_mx ... self must be square")
   ENSURE(size(self,1)==(l_max+1)*(l_max+1),"MAT{REAL}:add_multipole_W_translation_mx ... self has wrong dimension")

      ! Note R, NOT -R, when comparing to Helgaker's book
      call make_R_harmonics_(Rc,Rs,R,l_max)

      ! Set self to zero, mainly for upper triangle
      self = ZERO

      ! Now make the translation matrix
      jk = 0
      do j = 0,l_max
         sk = 1
         if (is_even_(j)) sk = -1
         ! First do k<0
         do k = -j,-1
            jk = jk+1
            sk = -sk  ! initially,  (-1)==j
            fac = ONE
            if (k==0) fac = HALF
            lm = j*j
            do l = j,l_max  ! j<=l
               lj = l-j
               Rc_lj => Rc(lj)%element
               Rs_lj => Rs(lj)%element
               ! First do m<0
               do m = -l,-1
                  lm = lm+1
                  mmk = m-k
                  mpk = m+k
                  if (lj>=abs(mmk)) self(lm,jk) = self(lm,jk)+Rc_lj(mmk)  ! W^ss
                  if (lj>=abs(mpk)) self(lm,jk) = self(lm,jk)-sk*Rc_lj(mpk)  ! W^ss
               end do
               ! Now do m>=0
               do m = 0,l
                  lm = lm+1
                  mmk = m-k
                  mpk = m+k
                  if (lj>=abs(mmk)) self(lm,jk) = self(lm,jk)-Rs_lj(mmk)  ! W^cs
                  if (lj>=abs(mpk)) self(lm,jk) = self(lm,jk)+sk*Rs_lj(mpk)  ! W^cs
               end do
            end do
         end do
         ! Now do k>=0
         do k = 0,j
            jk = jk+1
            sk = -sk  ! initially,  (-1)==j, now continuing on
            fac = ONE
            if (k==0) fac = HALF
            lm = j*j
            do l = j,l_max  ! j<=l
               lj = l-j
               Rc_lj => Rc(lj)%element
               Rs_lj => Rs(lj)%element
               ! First do m<0
               do m = -l,-1
                  lm = lm+1
                  mmk = m-k
                  mpk = m+k
                  if (lj>=abs(mmk)) self(lm,jk) = self(lm,jk)+fac*Rs_lj(mmk)  ! W^sc
                  if (lj>=abs(mpk)) self(lm,jk) = self(lm,jk)+sk*fac*Rs_lj(mpk)  ! W^sc
               end do
               ! Now do m>=0
               do m = 0,l
                  lm = lm+1
                  mmk = m-k
                  mpk = m+k
                  if (lj>=abs(mmk)) self(lm,jk) = self(lm,jk)+fac*Rc_lj(mmk)  ! W^cc
                  if (lj>=abs(mpk)) self(lm,jk) = self(lm,jk)+sk*fac*Rc_lj(mpk)  ! W^cc
               end do
            end do
         end do
      end do

      !           if (m>=0 AND k>=0) self(lm,jk) = fac*(Rc_lj(m-k) + sk*Rc_lj(m+k)) ! W^cc
      !           if (m>=0 AND k<0 ) self(lm,jk) =     -Rs_lj(m-k) + sk*Rs_lj(m+k)  ! W^cs
      !           if (m<0  AND k>=0) self(lm,jk) = fac*(Rs_lj(m-k) + sk*Rs_lj(m+k)) ! W^sc
      !           if (m<0  AND k<0 ) self(lm,jk) =      Rc_lj(m-k) - sk*Rc_lj(m+k)  ! W^ss

      ! Must deallocate Rc, Rs
      do L = l_max,0,-1
         deallocate(Rs(L)%element)
         deallocate(Rc(L)%element)
      end do
      deallocate(Rs)
      deallocate(Rc)
   end subroutine

   PURE subroutine to_multipole_T_interaction_mx(self,R,l_max,j_max)
   ! Make the multipole translation matrix T(lm,jk)(R) evaluated at position "R"
   ! where the maximum value of l and j are, respectoively, "l_max" and "j_max".
   ! See the book by Helgaker, Olsen, and Simons, equation (9.13.75), p. 415.
   ! NOTE: self is created.
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,3), IN :: R
      INT, IN :: l_max,j_max
      VEC(type(EVEC_REAL_TYPE),:), allocatable, target :: Ic,Is
      VEC(REAL,:), PTR :: Ic_lj,Is_lj
      INT :: j,k,jk,sj,l,m,lm,sk,lj,mmk,mpk
      REAL :: fac,fk,fm
   ENSURE(l_max>=0,"MAT{REAL}:to_multipole_T_interaction_mx ... l_max must be non-negative")
   ENSURE(size(self,1)==(l_max+1)*(l_max+1),"MAT{REAL}:to_multipole_T_interaction_mx ... self has wrong 1st (l_max) dimension")
   ENSURE(size(self,2)==(j_max+1)*(j_max+1),"MAT{REAL}:to_multipole_T_interaction_mx ... self has wrong 1st (j_max) dimension")
      call make_I_harmonics_(Ic,Is,R,l_max+j_max)

      ! Set self to zero, mainly for upper triangle
      self = ZERO
      jk = 0
      do j = 0,j_max
         sj = -1
         if (is_even_(j)) sj = 1
         sk = -sj
         ! First do k<0
         do k = -j,-1
            jk = jk+1
            sk = -sk  ! initially,  (-1)==j
            fk = ONE
            if (k==0) fk = HALF
            lm = 0
            do l = 0,l_max
               lj = l+j
               Ic_lj => Ic(lj)%element
               Is_lj => Is(lj)%element
               ! First do m<0
               do m = -l,-1
                  lm = lm+1
                  mpk = m+k
                  mmk = m-k
                  fm = ONE
                  if (m==0) fm = HALF
                  fac = 2*sj*fm*fk
                  self(lm,jk) = fac*(-Ic_lj(mpk)+sk*Ic_lj(mmk))  ! T^ss
               end do
               ! Now do m>=0
               do m = 0,l
                  lm = lm+1
                  mpk = m+k
                  mmk = m-k
                  fm = ONE
                  if (m==0) fm = HALF
                  fac = 2*sj*fm*fk
                  self(lm,jk) = fac*(Is_lj(mpk)-sk*Is_lj(mmk))  ! T^cs
               end do
            end do
         end do
         ! No do k>=0
         do k = 0,j
            jk = jk+1
            sk = -sk  ! initially,  (-1)==j
            fk = ONE
            if (k==0) fk = HALF
            lm = 0
            do l = 0,l_max
               lj = l+j
               Ic_lj => Ic(lj)%element
               Is_lj => Is(lj)%element
               ! First do m<0
               do m = -l,-1
                  lm = lm+1
                  mpk = m+k
                  mmk = m-k
                  fm = ONE
                  if (m==0) fm = HALF
                  fac = 2*sj*fm*fk
                  self(lm,jk) = fac*(Is_lj(mpk)+sk*Is_lj(mmk))  ! T^sc
               end do
               ! Now do m>=0
               do m = 0,l
                  lm = lm+1
                  mpk = m+k
                  mmk = m-k
                  fm = ONE
                  if (m==0) fm = HALF
                  fac = 2*sj*fm*fk
                  self(lm,jk) = fac*(Ic_lj(mpk)+sk*Ic_lj(mmk))  ! T^cc
               end do
            end do
         end do
      end do

      !           if (m>=0 AND k>=0) self(lm,jk) = fac*( Ic_lj(m+k) + sk*Ic_lj(m-k)) ! T^cc
      !           if (m>=0 AND k<0 ) self(lm,jk) = fac*( Is_lj(m+k) - sk*Is_lj(m-k)) ! T^cs
      !           if (m<0  AND k>=0) self(lm,jk) = fac*( Is_lj(m+k) + sk*Is_lj(m-k)) ! T^sc
      !           if (m<0  AND k<0 ) self(lm,jk) = fac*(-Ic_lj(m+k) + sk*Ic_lj(m-k)) ! T^ss

      ! Must deallocate Ic, Is
      do L = l_max+j_max,0,-1
         deallocate(Is(L)%element)
         deallocate(Ic(L)%element)
      end do
      deallocate(Is)
      deallocate(Ic)
   end subroutine

   PURE subroutine add_multipole_T_interaction_mx(self,R,l_max,j_max)
   ! Add the multipole translation matrix T(lm,jk)(R) evaluated at position "R"
   ! where the maximum value of l and j are, respectoively, "l_max" and "j_max".
   ! See the book by Helgaker, Olsen, and Simons, equation (9.13.75), p. 415.
   ! NOTE: self is created.
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,3), IN :: R
      INT, IN :: l_max,j_max
      VEC(type(EVEC_REAL_TYPE),:), allocatable, target :: Ic,Is
      VEC(REAL,:), PTR :: Ic_lj,Is_lj
      INT :: j,k,jk,sj,l,m,lm,sk,lj,mmk,mpk
      REAL :: fac,fk,fm
   ENSURE(l_max>=0,"MAT{REAL}:add_multipole_T_interaction_mx ... l_max must be non-negative")
   ENSURE(size(self,1)==(l_max+1)*(l_max+1),"MAT{REAL}:add_multipole_T_interaction_mx ... self has wrong 1st (l_max) dimension")
   ENSURE(size(self,2)==(j_max+1)*(j_max+1),"MAT{REAL}:add_multipole_T_interaction_mx ... self has wrong 1st (j_max) dimension")
      call make_I_harmonics_(Ic,Is,R,l_max+j_max)

      ! Set self to zero, mainly for upper triangle
      self = ZERO
      jk = 0
      do j = 0,j_max
         sj = -1
         if (is_even_(j)) sj = 1
         sk = -sj
         ! First do k<0
         do k = -j,-1
            jk = jk+1
            sk = -sk  ! initially,  (-1)==j
            fk = ONE
            if (k==0) fk = HALF
            lm = 0
            do l = 0,l_max
               lj = l+j
               Ic_lj => Ic(lj)%element
               Is_lj => Is(lj)%element
               ! First do m<0
               do m = -l,-1
                  lm = lm+1
                  mpk = m+k
                  mmk = m-k
                  fm = ONE
                  if (m==0) fm = HALF
                  fac = 2*sj*fm*fk
                  self(lm,jk) = self(lm,jk)+fac*(-Ic_lj(mpk)+sk*Ic_lj(mmk))  ! T^ss
               end do
               ! Now do m>=0
               do m = 0,l
                  lm = lm+1
                  mpk = m+k
                  mmk = m-k
                  fm = ONE
                  if (m==0) fm = HALF
                  fac = 2*sj*fm*fk
                  self(lm,jk) = self(lm,jk)+fac*(Is_lj(mpk)-sk*Is_lj(mmk))  ! T^cs
               end do
            end do
         end do
         ! No do k>=0
         do k = 0,j
            jk = jk+1
            sk = -sk  ! initially,  (-1)==j
            fk = ONE
            if (k==0) fk = HALF
            lm = 0
            do l = 0,l_max
               lj = l+j
               Ic_lj => Ic(lj)%element
               Is_lj => Is(lj)%element
               ! First do m<0
               do m = -l,-1
                  lm = lm+1
                  mpk = m+k
                  mmk = m-k
                  fm = ONE
                  if (m==0) fm = HALF
                  fac = 2*sj*fm*fk
                  self(lm,jk) = self(lm,jk)+fac*(Is_lj(mpk)+sk*Is_lj(mmk))  ! T^sc
               end do
               ! Now do m>=0
               do m = 0,l
                  lm = lm+1
                  mpk = m+k
                  mmk = m-k
                  fm = ONE
                  if (m==0) fm = HALF
                  fac = 2*sj*fm*fk
                  self(lm,jk) = self(lm,jk)+fac*(Ic_lj(mpk)+sk*Ic_lj(mmk))  ! T^cc
               end do
            end do
         end do
      end do
      !           if (m>=0 AND k>=0) self(lm,jk) = fac*( Ic_lj(m+k) + sk*Ic_lj(m-k)) ! T^cc
      !           if (m>=0 AND k<0 ) self(lm,jk) = fac*( Is_lj(m+k) - sk*Is_lj(m-k)) ! T^cs
      !           if (m<0  AND k>=0) self(lm,jk) = fac*( Is_lj(m+k) + sk*Is_lj(m-k)) ! T^sc
      !           if (m<0  AND k<0 ) self(lm,jk) = fac*(-Ic_lj(m+k) + sk*Ic_lj(m-k)) ! T^ss
      ! Must deallocate Ic, Is
      do L = l_max+j_max,0,-1
         deallocate(Is(L)%element)
         deallocate(Ic(L)%element)
      end do
      deallocate(Is)
      deallocate(Ic)
   end subroutine

   PURE subroutine to_R_mu_multipole_mx(self,points,l_max)
   ! Make the R_mu multipole matrix, self(i,j), which is the scaled regular
   ! mu-type solid harmonic evaluated at point P_i = "points(:,i)" for moment
   ! (L,M), where index j is L*L + M + L + 1, and where L is less than "l_max".
   ! This matrix is used to multiply the interaction matrix to evaluate the
   ! potential at the given points P_i from multipoles at Q,
   !     potential(i) = self * T(Q-P_i) * multipoles(Q).
   ! Reference: the book by Helgaker, Simons and Olsen, p. 407.
      MAT(REAL,:,:), OUT :: self
      MAT(REAL,:,:), IN :: points
      INT, IN :: l_max
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable :: Rm
      INT :: L,M,lm
   ENSURE(size(points,2)==3,"MAT{REAL}:to_R_mu_multipole_mx ... wrong 2nd dimension, points arrays")
   ENSURE(l_max>=0,"MAT{REAL}:to_R_mu_multipole_mx ... l_max must be non-negative")
   ENSURE(size(self,1)==size(points,1),"MAT{REAL}:to_R_mu_multipole_mx ... wrong 1st dimension, self")
   ENSURE(size(self,2)==(l_max+1)*(l_max+1),"MAT{REAL}:to_R_mu_multipole_mx ... wrong 2nd dimension, self")
      call make_R_mu_harmonics_(Rm,points,l_max)
      lm = 0
      do L = 0,l_max
         do M = -L,L  ! note the canonoical order here
            lm = lm+1
            self(:,lm) = Rm(L)%element(M)%element(:)
         end do
      end do
   end subroutine

   PURE subroutine expand_quadrupole_from(self,vec)
   ! Expand "self" from a compressed vector "vec" of quadrupole moments
      MAT(REAL,:,:), OUT :: self
      VEC(REAL,6), IN :: vec
   ENSURE(is_square_(self),"MAT{REAL}:expand_quadrupole_from ... self is not square")
   ENSURE(size(self,1)==3,"MAT{REAL}:expand_quadrupole_from ... self is not 3-dimensional")
      self(1,1) = vec(1)  ! xx
      self(2,2) = vec(2)  ! yy
      self(3,3) = vec(3)  ! zz
      self(1,2) = vec(4)  ! xy
      self(2,1) = vec(4)
      self(1,3) = vec(5)  ! xz
      self(3,1) = vec(5)
      self(2,3) = vec(6)  ! yz
      self(3,2) = vec(6)
   end subroutine

end module
