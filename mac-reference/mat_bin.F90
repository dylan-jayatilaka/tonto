!==================================================================
!
! MAT{BIN}: Logical matrices
!
! Copyright (C) Dylan Jayatilaka, 2002
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
module MAT_BIN_MODULE

#  include "macros"

   include "mat{bin}.use"

   implicit none

   include "mat{bin}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim1,dim2)
   ! Allocate a matrix with the given dimensions
      MAT(BIN,:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{INTRINSIC}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim1,dim2),stat=status)
      ENSURE(status==0,"MAT{BIN}:create_0 ... error in allocating memory")
#else
      allocate(self(dim1,dim2))
#endif
   end subroutine

   PURE subroutine create_1(self,lb1,ub1,lb2,ub2)
   ! Allocate a matrix with the given bounds
      MAT(BIN,:,:), allocatable, OUT :: self
      INT, IN :: lb1,ub1
      INT, IN :: lb2,ub2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{INTRINSIC}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb1:ub1,lb2:ub2),stat=status)
      ENSURE(status==0,"MAT{BIN}:create_1 ... error in allocating memory")
#else
      allocate(self(lb1:ub1,lb2:ub2))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      MAT(BIN,:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT{BIN}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ============================
!  Size-of and shape operations 
!  ============================

   pure function is_same_shape_as(self,a) result (res)
   ! Returns TRUE if the matrix "a" has the same shape as "self"
      MAT(BIN,:,:), IN :: self
      MAT(BIN,:,:), IN :: a
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(a,1) AND size(self,2)==size(a,2)
   end function

   pure function is_transposed_shape_of(self,a) result (res)
   ! Returns TRUE if the matrix "a" is the transposed shape of self
      MAT(BIN,:,:), IN :: self
      MAT(BIN,:,:), IN :: a
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(a,2) AND size(self,2)==size(a,1)
   end function

   pure function is_square(self) result (res)
   ! Returns TRUE if the matrix is square
      MAT(BIN,:,:), IN :: self
      BIN :: res

      ! The following code is inherited from MAT{INTRINSIC}
      res = size(self,1)==size(self,2)
   end function


!  =======================
!  Shrinking and expansion
!  =======================

   PURE subroutine shrink(self,dim1,dim2)
   ! Shrink allocatable self to have upper bounds dim1 x dim2.
      MAT(BIN,:,:), allocatable, INOUT :: self
      INT, IN :: dim1,dim2
      MAT(BIN,:,:), allocatable :: old
      INT :: lb1,lb2,ub1,ub2
   ENSURE(allocated(self),"MAT{BIN}:shrink ... matrix not allocated")
   ENSURE(dim1<=ubound(self,1),"MAT{BIN}:shrink ... 1st dimension given is too large.")
   ENSURE(dim2<=ubound(self,2),"MAT{BIN}:shrink ... 2nd dimension given is too large.")

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
      MAT(BIN,:,:), allocatable, INOUT :: self
      INT, IN :: dim2
      INT :: ub1,ub2
   ENSURE(allocated(self),"MAT{BIN}:shrink_columns ... matrix not allocated")
   ENSURE(dim2<=ubound(self,2),"MAT{BIN}:shrink_columns ... 2nd dimension given is too large.")

      ! The following code is inherited from MAT{INTRINSIC}
      ub1 = ubound(self,1)
      ub2 = ubound(self,2)
      if (dim2==ub2) return
      call shrink_(self,ub1,dim2)
   end subroutine

   PURE subroutine expand(self,dim1,dim2)
   ! Expand allocatable self to dim1 x dim2.
   ! New elements are initialised.
      MAT(BIN,:,:), allocatable, INOUT :: self
      INT, IN :: dim1,dim2
      MAT(BIN,:,:), allocatable :: new
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
         ENSURE(dim1>=ubound(self,1),"MAT{BIN}:expand ... 1st dimension given is too small")
         ENSURE(dim2>=ubound(self,2),"MAT{BIN}:expand ... 2nd dimension given is too small")

         ! Copy contents
         call create_(new,lb1,dim1,lb2,dim2)
         new = FALSE
         new(lb1:ub1,lb2:ub2) = self

         ! Transfer
         call move_alloc(new,self)
      end if
   end subroutine

   PURE subroutine expand_rows(self,dim1)
   ! Expands the rows of allocatable self to upper bound dim1.
   ! New elements are zeroed.
      MAT(BIN,:,:), allocatable, INOUT :: self
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
      MAT(BIN,:,:), allocatable, INOUT :: self
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
      MAT(BIN,:,:), allocatable, INOUT :: self
      MAT(BIN,:,:), IN :: cols
      INT :: ub2

      ! The following code is inherited from MAT{INTRINSIC}
      if (NOT allocated(self)) then
         self = cols
      else
         ENSURE(size(self,1)==size(cols,1),"MAT{BIN}:append_columns ... 1st dimension wrong, cols")

         ! Expand
         ub2 = ubound(self,2)
         call expand_columns_(self,ub2+size(cols,2))

         ! Append
         self(:,ub2+1:) = cols(:,:)
      end if
   end subroutine

   PURE subroutine append_column(self,col)
   ! Append the column "col" onto end of allocatable self.
      MAT(BIN,:,:), allocatable, INOUT :: self
      VEC(BIN,:), IN :: col
      INT :: ub2

      ! The following code is inherited from MAT{INTRINSIC}
      if (NOT allocated(self)) then
         call create_(self,size(col),1)
         self(:,1) = col(:)
      else
         ENSURE(size(self,1)==size(col),"MAT{BIN}:append_column ... 2nd dimension given is too small")

         ! Expand
         ub2 = ubound(self,2)
         call expand_columns_(self,ub2+1)

         ! Append
         self(:,ub2+1) = col
      end if
   end subroutine

   PURE subroutine prune_column(self,col)
   ! Prune the column with index "col".
      MAT(BIN,:,:), allocatable, INOUT :: self
      INT, IN :: col
      INT :: lb2,ub2
   ENSURE(allocated(self),"MAT{BIN}:prune_column ... self is not.associated")
   ENSURE(is_in_range_(col,[lbound(self,2),ubound(self,2)]),"MAT{BIN}:prune_column ... col is out of range")

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
      MAT(BIN,:,:), IN :: self
      MAT(BIN,:,:), IN :: b
      BIN :: res
      res = same_as_(self,b)
   end function

   PURE function same_as(self,b) result (res)
   ! Check if the matrix is the same as "b".
      MAT(BIN,:,:), IN :: self
      MAT(BIN,:,:), IN :: b
      BIN :: res
   ENSURE(is_same_shape_as_(self,b),"MAT{BIN}:same_as ... incompatible dimensions")
      res = all(self EQV b)
   end function


!  =============
!  String widths
!  =============

   pure function str_lengths(self,spaces) result (res)
   ! Returns the minimal string lengths, with optional extra "spaces"
      MAT(BIN,:,:), IN :: self
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
      MAT(BIN,:,:), IN :: self
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

end module
