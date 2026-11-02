!==================================================================
!
! VEC{BIN} : Boolean vector operations ...
!
! Copyright (C) Daniel Grimwood, 1999
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
module VEC_BIN_MODULE

#  include "macros"

   include "vec{bin}.use"

   implicit none

   include "vec{bin}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim)
   ! Allocate vector
      VEC(BIN,:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{BIN}:create_0 ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{BIN}:create_0 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine create_1(self,lb,ub)
   ! Allocate the vector with lower bound "lb", upper bound "ub"
      VEC(BIN,:), allocatable, OUT :: self
      INT, IN :: lb,ub

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb:ub),stat=status)
      ENSURE(status==0,"VEC{BIN}:create_1 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine create_2(self,bounds)
   ! Allocate the vector with lower and upper bounds in "bounds"
      VEC(BIN,:), allocatable, OUT :: self
      VEC(INT,2), IN :: bounds
      INT :: lb,ub
#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)
      lb = bounds(1)
      ub = bounds(2)

#ifdef USE_PRECONDITIONS
      allocate(self(lb:ub),stat=status)
      ENSURE(status==0,"VEC{BIN}:create_2 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      VEC(BIN,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{BIN}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

!  ==================
!  Size-of operations
!  ==================


!  =======================
!  Shrinking and expansion
!  =======================

   PURE subroutine shrink(self,dim,chop_start)
   ! Shrink allocatable "self" to dimension "dim" from the end.
   ! Contents are retained. If "chop_start" is present and TRUE,
   ! "self" is chopped from the start, and the contents from the end
   ! are retained.
      VEC(BIN,:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: chop_start
      VEC(BIN,:), allocatable :: new
      INT :: lb,ub
      BIN :: chop_end
   ENSURE(allocated(self),"VEC{BIN}:shrink ... no self array")
   ENSURE(dim<=size(self),"VEC{BIN}:shrink ... dim too large")

      ! Chop from start or end?

      ! The following code is inherited from VEC{OBJECT}
      chop_end = TRUE
      if (present(chop_start)) chop_end = NOT chop_start

      ! Bounds
      lb = lbound(self,dim=1)
      ub = ubound(self,dim=1)
      call create_(new,lb,lb+dim-1)
      if (chop_end) then
         new(lb:) = self(lb:lb+dim-1)
      else
         new(lb:) = [self(ub-dim+1:ub)]
      end if
      call destroy_(self)
      self = new
   end subroutine

   PURE subroutine expand(self,dim,init,grow_start)
   ! Expand allocatable "self" to dimension "dim" from the end. New
   ! slots are zeroed unless scalar "init" is set. If "grow_start" is
   ! present and TRUE, then "self" is expanded from the start, keeping
   ! the same lower bound.
      VEC(BIN,:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: init
      BIN, optional, IN :: grow_start
      INT :: lb,ub,nc
      BIN :: start
      VEC(BIN,:), allocatable :: new
      BIN :: val

      ! Value to initialize

      ! The following code is inherited from VEC{OBJECT}
      val = FALSE
      if (present(init)) val = init

      ! Chop from start or end?
      start = FALSE
      if (present(grow_start)) start = grow_start
      if (NOT allocated(self)) then
         call create_(self,dim)
         if (dim>0) self = val
      else if (size(self)==0) then
         call destroy_(self)
         call create_(self,dim)
         if (dim>0) self = val
      else
         ENSURE(dim>size(self),"VEC{BIN}:expand ... dim not large enough")
         ENSURE(dim>0,"VEC{BIN}:expand ... dim is not +ve")

         ! Bounds
         nc = dim-size(self)
         lb = lbound(self,dim=1)
         ub = ubound(self,dim=1)

         ! Grow it
         call create_(new,lb,lb+dim-1)
         if (start) then

            ! Expand the start
            new(lb:lb+nc-1) = val
            new(lb+nc:) = self
         else

            ! Default - append
            new(lb:ub) = self
            new(ub+1:) = val
         end if

         ! Transfer
         call move_alloc(new,self)
      end if
   end subroutine

   pure subroutine append_0(self,v)
   ! Append the contents of vector "v" to allocatable "self".
      VEC(BIN,:), allocatable, INOUT :: self
      VEC(BIN,:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = v
      else
         self = [self,v]
      end if
   end subroutine

   pure subroutine append_1(self,value)
   ! Appends the scalar "value" onto the end of allocatable "self".
      VEC(BIN,:), allocatable, INOUT :: self
      BIN, IN :: value

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [value]
      else
         self = [self,value]
      end if
   end subroutine

   PURE subroutine append_2(self,v1,v2)
   ! Appends vectors "v1" and "v2" to allocatable "self".
      VEC(BIN,:), allocatable, INOUT :: self
      VEC(BIN,:), IN :: v1,v2

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [v1,v2]
      else
         self = [self,v1,v2]
      end if
   end subroutine

   PURE subroutine prepend_0(self,v)
   ! Prepend vector "v" to allocatable "self".
      VEC(BIN,:), allocatable, INOUT :: self
      VEC(BIN,:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [v]
      else
         self = [v,self]
      end if
   end subroutine

   PURE subroutine prepend_1(self,value)
   ! Prepend a single "value" to allocatable "self".
      VEC(BIN,:), allocatable, INOUT :: self
      BIN, IN :: value

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [value]
      else
         self = [value,self]
      end if
   end subroutine

   PURE subroutine prune_element(self,k)
   ! Removes the element at position "k" from allocatable "self".
   ! Elements after that position are moved downward.
      VEC(BIN,:), allocatable, INOUT :: self
      INT, IN :: k
      VEC(BIN,:), allocatable :: old
      INT :: lb,ub

      ! Bounds

      ! The following code is inherited from VEC{OBJECT}
      lb = lbound(self,dim=1)
      ub = ubound(self,dim=1)

      ! Save
      call create_(old,lb,ub)
      old = self

      ! Transfer
      call create_(self,lb,ub-1)
      self(lb:k-1) = old(lb:k-1)
      self(k:ub-1) = old(k+1:ub)
   end subroutine


! ====================
! Comparison functions
! ====================

   pure function equals(self,v) result (res)
   ! Return true if "self" is the same as "v".
      VEC(BIN,:), IN :: self
      VEC(BIN,:), IN :: v
      BIN :: res
      res = same_as_(self,v)
   end function

   pure function same_as(self,v) result (res)
   ! Return true if "self" is the same as "v".
      VEC(BIN,:), IN :: self
      VEC(BIN,:), IN :: v
      BIN :: res
      if (size(self)/=size(v)) then; res = FALSE
      else; res = all(self EQV v)
      end if
   end function


! ================
! Sorting routines
! ================

   pure subroutine reverse_order(self)
   ! Reverse the order of the elements of self
      VEC(BIN,:), INOUT :: self
      INT :: n,dim

      ! The following code is inherited from VEC{INTRINSIC}
      dim = size(self)
      do n = 1,dim/2
         call swap_elements_(self,n,dim-n+1)
      end do
   end subroutine

   pure subroutine swap_elements(self,e1,e2)
   ! Swap elements "e1" and "e2" in "self".
      VEC(BIN,:), INOUT :: self
      INT, IN :: e1,e2

   ! ENSURE(e1<=.dim AND e2<=.dim,"element exceeds dimesion")
   ! ENSURE(e1>0 AND e2>0,"indices must be positive")
      BIN :: val

      ! The following code is inherited from VEC{INTRINSIC}
      val = self(e1)
      self(e1) = self(e2)
      self(e2) = val
   end subroutine


! ==========
! Misc stuff
! ==========

   pure subroutine set_to(self,v)
   ! Set the vector to "v". See also the "copy" routine.
      VEC(BIN,:), INOUT :: self
      VEC(BIN,:), IN :: v

      ! The following code is inherited from VEC{INTRINSIC}
      self = v
   end subroutine

   pure function n_true(self) result (res)
   ! Returns the number of true items in self.
      VEC(BIN,:), IN :: self
      INT :: res
      res = count(self)
   end function

   pure function n_false(self) result (res)
   ! Returns the number of false items in self.
      VEC(BIN,:), IN :: self
      INT :: res
      res = size(self)-count(self)
   end function

   pure function index_of_first_true_element(self) result (res)
   ! Returns the index of the first true element in self, or zero if
   ! there is no true element.
      VEC(BIN,:), IN :: self
      INT :: res
      INT :: i
      res = 0
      do i = 1,size(self)
         if (NOT self(i)) cycle
         res = i
         exit
      end do
   end function


! =============
! String widths
! =============

   pure function str_lengths(self) result (res)
   ! Returns the minimal string lengths.
      VEC(BIN,:), IN :: self
      VEC(INT,size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = str_length_(self(i))
      end do
   end function

   pure subroutine get_max_str_length(self,msl,spaces)
   ! Returns "msl", the maximum of the string lengths for "self",
   ! and with optional extra "spaces". 
      VEC(BIN,:), IN :: self
      INT, OUT :: msl
      INT, IN :: spaces
      INT :: i,sl

      ! The following code is inherited from VEC{INTRINSIC}
      msl = 0
      do i = 1,size(self)
         call get_str_length_(self(i),sl,spaces)
         msl = max(msl,sl)
      end do
   end subroutine


! =====================
! Conversion to strings
! =====================

   pure function to_str_0(self) result (res)
   ! Change self to a string of minimal length
      VEC(BIN,:), IN :: self
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = to_str_(self(i))
      end do
   end function

   pure function to_str_1(self,fmt,left_justify) result (res)
   ! Change self to a "string" using the specified fortran "fmt".
      VEC(BIN,:), IN :: self
      STR(len=*), IN :: fmt
      BIN, optional, IN :: left_justify
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = to_str_(self(i),fmt,left_justify)
      end do
   end function

   pure function to_str_2(self,width,left_justify) result (res)
   ! Change self to a string with specified "width".
      VEC(BIN,:), IN :: self
      INT, IN :: width
      BIN, optional, IN :: left_justify
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = to_str_(self(i),width,left_justify)
      end do
   end function

   pure function to_concatenated_str(self,fmt,separator) result (res)
   ! Change self to a string using "fmt" if specified, and with
   ! "separator" between each item, if specified.
      VEC(BIN,:), IN :: self
      STR(len=*), optional, IN :: fmt
      STR(len=*), optional, IN :: separator
      STR(len=STR_SIZE) :: res
      STR(len=STR_SIZE) :: str
      INT :: n

      ! The following code is inherited from VEC{INTRINSIC}
      res = " "
      do n = 1,size(self)
         if (present(fmt)) then; str = to_str_(self(n),fmt)
         else; str = to_str_(self(n))
         end if

         ! ENSURE(len_trim(res) + len_trim(str) < len(res), "result is too long")
         if (present(separator)) then; res = trim(res)//separator//trim(str)
         else; res = trim(res)//" "//trim(str)
         end if
      end do
   end function

end module
