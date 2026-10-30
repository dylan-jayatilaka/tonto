!====================================================================
!
!  VEC{STR}: String vectors
!
!  Normally, a STR variable means a character string of length
!  STR_SIZE.  However, in this module we use assumed length character
!  strings.  Note also that ELEMENT_TYPE_SIZE is defind as
!  "len(self(1))*CHR_SIZE".
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
!====================================================================
module VEC_STR_MODULE

#  include "macros"

   include "vec{str}.use"

   implicit none

   include "vec{str}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim)
   ! Allocate vector
      VEC(STR(len=*),:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{STR}:create_0 ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{STR}:create_0 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine create_1(self,lb,ub)
   ! Allocate the vector with lower bound "lb", upper bound "ub"
      VEC(STR(len=*),:), allocatable, OUT :: self
      INT, IN :: lb,ub

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb:ub),stat=status)
      ENSURE(status==0,"VEC{STR}:create_1 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine create_2(self,bounds)
   ! Allocate the vector with lower and upper bounds in "bounds"
      VEC(STR(len=*),:), allocatable, OUT :: self
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
      ENSURE(status==0,"VEC{STR}:create_2 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      VEC(STR(len=*),:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{STR}:destroy ... error in deallocating memory")
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
      VEC(STR(len=*),:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: chop_start
      VEC(STR(len=STR_SIZE),:), allocatable :: new
      INT :: lb,ub
      BIN :: chop_end
   ENSURE(allocated(self),"VEC{STR}:shrink ... no self array")
   ENSURE(dim<=size(self),"VEC{STR}:shrink ... dim too large")

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
      VEC(STR(len=*),:), allocatable, INOUT :: self
      INT, IN :: dim
      STR(len=*), optional, IN :: init
      BIN, optional, IN :: grow_start
      INT :: lb,ub,nc
      BIN :: start
      VEC(STR(len=STR_SIZE),:), allocatable :: new
      STR(len=STR_SIZE) :: val

      ! Value to initialize

      ! The following code is inherited from VEC{OBJECT}
      val = BLANK
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
         ENSURE(dim>size(self),"VEC{STR}:expand ... dim not large enough")
         ENSURE(dim>0,"VEC{STR}:expand ... dim is not +ve")

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
      VEC(STR(len=*),:), allocatable, INOUT :: self
      VEC(STR(len=*),:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = v
      else
         self = [self,v]
      end if
   end subroutine

   pure subroutine append_1(self,value)
   ! Appends the scalar "value" onto the end of allocatable "self".
      VEC(STR(len=*),:), allocatable, INOUT :: self
      STR(len=*), IN :: value

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [value]
      else
         self = [self,value]
      end if
   end subroutine

   PURE subroutine append_2(self,v1,v2)
   ! Appends vectors "v1" and "v2" to allocatable "self".
      VEC(STR(len=*),:), allocatable, INOUT :: self
      VEC(STR(len=*),:), IN :: v1,v2

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [v1,v2]
      else
         self = [self,v1,v2]
      end if
   end subroutine

   PURE subroutine prepend_0(self,v)
   ! Prepend vector "v" to allocatable "self".
      VEC(STR(len=*),:), allocatable, INOUT :: self
      VEC(STR(len=*),:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [v]
      else
         self = [v,self]
      end if
   end subroutine

   PURE subroutine prepend_1(self,value)
   ! Prepend a single "value" to allocatable "self".
      VEC(STR(len=*),:), allocatable, INOUT :: self
      STR(len=*), IN :: value

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
      VEC(STR(len=*),:), allocatable, INOUT :: self
      INT, IN :: k
      VEC(STR(len=STR_SIZE),:), allocatable :: old
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

   PURE subroutine prune_0(self,values)
   ! Removes the scalar "values" from allocatable "self" (if there).
   ! The order of the elements is otherwise unchanged.
      VEC(STR(len=*),:), allocatable, INOUT :: self
      VEC(STR(len=*),:), IN :: values
      INT :: lb

      ! The following code is inherited from VEC{OBJECT}
      lb = lbound(self,dim=1)
      self(lb:) = pack(self,mask=any(spread(self,2,size(values))==spread(values,1,size(self)),dim=1))
   end subroutine

   PURE subroutine prune_1(self,value)
   ! Remove the scalar "value" from allocatable "self" (if there).
   ! The order of the elements is otherwise unchanged.
      VEC(STR(len=*),:), allocatable, INOUT :: self
      STR(len=*), IN :: value
      INT :: lb

      ! The following code is inherited from VEC{OBJECT}
      lb = lbound(self,dim=1)
      self(lb:) = pack(self,mask=self/=value)
   end subroutine


! ====================
! Comparison functions
! ====================

   pure function equals(self,v) result (res)
   ! Return true if "self" is the same as "v".
      VEC(STR(len=*),:), IN :: self
      VEC(STR(len=*),:), IN :: v
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = same_as_(self,v)
   end function

   pure function same_as(self,v) result (res)
   ! Return true if "self" is the same as "v".
      VEC(STR(len=*),:), IN :: self
      VEC(STR(len=*),:), IN :: v
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      if (size(self)/=size(v)) then; res = FALSE
      else; res = all(self==v)
      end if
   end function

   pure function has_elements_common_with(self,v) result (res)
   ! Return true if "self" has elements common with "v"
      VEC(STR(len=*),:), IN :: self
      VEC(STR(len=*),:), IN :: v
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = any(spread(self,2,size(v))==spread(v,1,size(self)))
   end function

   pure function has_no_elements_common_with(self,v) result (res)
   ! Return true if "self" has no elements common with "v"
      VEC(STR(len=*),:), IN :: self
      VEC(STR(len=*),:), IN :: v
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = all(spread(self,2,size(v))/=spread(v,1,size(self)))
   end function

   pure function has_all_elements_common_with(self,v) result (res)
   ! Return true if ALL elements of "self" are common with any in "v"
      VEC(STR(len=*),:), IN :: self
      VEC(STR(len=*),:), IN :: v
      BIN :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = TRUE
      do i = 1,size(self)
         if (any(v==self(i))) cycle
         res = FALSE
         exit
      end do
   end function

   pure function n_elements_common_with(self,v) result (res)
   ! Return the number of elements of "self" which are common with "v"
      VEC(STR(len=*),:), IN :: self
      VEC(STR(len=*),:), IN :: v
      INT :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         if (any(v==self(i))) then
            res = res+1
         end if
      end do
   end function

   pure function n_elements_uncommon_with(self,v) result (res)
   ! Return the number of elements of "self" which are uncommon with "v"
      VEC(STR(len=*),:), IN :: self
      VEC(STR(len=*),:), IN :: v
      INT :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = size(self)-n_elements_common_with_(self,v)
   end function

   PURE function elements_common_with(self,v) result (res)
   ! Return the elements of "self" which are common with "v",
   ! e.g. if self = [1,1,2,1] and v = [1], res = [1,1,1].
   ! See also routine below.
      VEC(STR(len=*),:), IN :: self
      VEC(STR(len=*),:), IN :: v
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      INT :: i,n
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from VEC{INTRINSIC}
      n = n_elements_common_with_(self,v)
      call create_(res,n)
      n = 0

      ! Loop over self elements ...
      do i = 1,size(self)
         val = self(i)

         ! Is any v equal to *this* self(i)?
         ! If so, record it and move on ...
         if (any(v==val)) then
            n = n+1
            res(n) = val
         end if
      end do
   end function

   PURE function elements_uncommon_with(self,v) result (res)
   ! Return the elements of "self" which are uncommon with "v"
   ! e.g. if self = [1,1,2,1] and v = [1], res = [2].
   ! See also routine above.
      VEC(STR(len=*),:), IN :: self
      VEC(STR(len=*),:), IN :: v
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      INT :: i,n
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from VEC{INTRINSIC}
      n = n_elements_uncommon_with_(self,v)
      call create_(res,n)
      n = 0

      ! Loop over self elements ...
      do i = 1,size(self)
         val = self(i)

         ! Are *all* v unequal to *this* self(i)?
         ! If so, record it and move on ...
         if (all(v/=val)) then
            n = n+1
            res(n) = val
         end if
      end do
   end function

   PURE function indices_of_elements_matching(self,item) result (res)
   ! Return the indices of the elements in "self" which match "item"
   ! NOTE: this routine returns a pointer!
      VEC(STR(len=*),:), IN :: self
      STR(len=*), IN :: item
      VEC(INT,:), allocatable :: res
      INT :: dim,i,n

      ! The following code is inherited from VEC{INTRINSIC}
      dim = count(self==item)
      call create_(res,dim)
      n = 0
      do i = 1,size(self)
         if (self(i)/=item) cycle
         n = n+1
         res(n) = i
      end do
   end function

   PURE subroutine is_permutation_of(self,v,res)
   ! Return true if "self" is a permutation of "v" (permutation array
   ! not returned in this routine).
      VEC(STR(len=*),:), IN :: self
      VEC(STR(len=*),size(self)), IN :: v
      BIN, OUT :: res
      VEC(INT,:), allocatable :: permutation
      VEC(BIN,:), allocatable :: skip
      INT :: i,j

      ! The following code is inherited from VEC{INTRINSIC}
      call create_(skip,size(self))
      call create_(permutation,size(self))
      res = TRUE
      permutation = 0
      skip = FALSE
      do i = 1,size(self)
         do j = 1,size(self)
            if (skip(j)) cycle
            if (self(i)/=v(j)) cycle
            permutation(i) = j
            skip(j) = TRUE
            exit
         end do
         if (permutation(i)/=0) cycle
         res = FALSE
         exit
      end do
      call destroy_(permutation)
      call destroy_(skip)
   end subroutine


! =============================
! Repetition related operations
! =============================

   PURE subroutine remove_repetitions(self)
   ! Remove repeated elements which come later in the list. 
   ! NOTE: allocatable
   ! NOTE: the vector may shrink. 
   ! NOTE: this version for bin, str, int
      VEC(STR(len=*),:), allocatable, INOUT :: self
      VEC(STR(len=len(self(1))),:), allocatable :: unique
      INT :: i,j,n
      BIN :: found
   ENSURE(allocated(self),"VEC{STR}:remove_repetitions ... no vector")

      ! The following code is inherited from VEC{INTRINSIC}
      if (size(self)==1) return
      call create_(unique,size(self))
      n = 1
      unique(1) = self(1)
      do i = 2,size(self)
         found = FALSE
         do j = 1,n
            if (self(i)/=unique(j)) cycle
            found = TRUE
         end do
         if (found) cycle
         n = n+1
         unique(n) = self(i)
      end do
      self = unique(1:n)
      call destroy_(unique)
   end subroutine

   pure function has_repetitions(self) result (res)
   ! Return TRUE if self has at least one repeated element.
      VEC(STR(len=*),:), IN :: self
      BIN :: res
      INT :: i,j

      ! The following code is inherited from VEC{INTRINSIC}
      res = FALSE
      if (size(self)==1) return
      do i = 2,size(self)
         do j = 1,i-1
            if (self(i)/=self(j)) cycle
            res = TRUE
            return
         end do
      end do
   end function

   pure subroutine get_1st_repetition(self,fi,fj)
   ! Return "fi" and "fj", the first two indices with the same element
      VEC(STR(len=*),:), IN :: self
      INT, OUT :: fi,fj
      INT :: i,j

      ! The following code is inherited from VEC{OBJECT}
      if (size(self)<=1) return
      fi = 0
      fj = 0
      do i = 2,size(self)
         do j = 1,i-1
            if (NOT same_as_(self(i),self(j))) cycle
            fi = i
            fj = j
            return
         end do
      end do
   end subroutine

   pure function no_of_unique_elements(self) result (res)
   ! Return the number of unique elements in the vector.
      VEC(STR(len=*),:), IN :: self
      INT :: res
      INT :: n,i
      BIN :: same

      ! The following code is inherited from VEC{INTRINSIC}
      res = 1
      do n = 2,size(self)
         same = FALSE
         do i = 1,n-1
            if (self(n)/=self(i)) cycle
            same = TRUE
            exit
         end do
         if (NOT same) res = res+1
      end do
   end function


! ================
! Sorting routines
! ================

   PURE subroutine sort(self,decreasing_order)
   ! Sort array "self" from lowest to highest, using simple insertion sort.  If
   ! "decreasing_order" is present and TRUE sort from highest to lowest instead.
      VEC(STR(len=*),:), INOUT :: self
      BIN, IN, optional :: decreasing_order
      INT :: i,j,n
      BIN :: lowest_first

      ! The following code is inherited from VEC{OBJECT}
      lowest_first = TRUE
      if (present(decreasing_order)) lowest_first = NOT decreasing_order

      ! Size
      n = size(self)
      if (lowest_first) then
         do i = 1,n-1
            do j = i+1,n
               if (self(j)<self(i)) call swap_elements_(self,i,j)
            end do
         end do
      else
         do i = 1,n-1
            do j = i+1,n
               if (self(j)>self(i)) call swap_elements_(self,i,j)
            end do
         end do
      end if
   end subroutine

   PURE subroutine quick_sort_0(self,decreasing_order)
   ! Sort the vector into increasing order.If "decreasing_order" is present and
   ! TRUE, the vector is sorted from largest to smallest
      VEC(STR(len=*),:), INOUT :: self
      BIN, optional, IN :: decreasing_order
      BIN :: decreasing

      ! The following code is inherited from VEC{OBJECT}
      decreasing = FALSE
      if (present(decreasing_order)) decreasing = decreasing_order
      if (NOT decreasing) then; call quick_sort_increasing_(self)
      else; call quick_sort_decreasing_(self)
      end if
   end subroutine

   PURE recursive subroutine quick_sort_increasing_0(self)
   ! Sort the vector into order, smallest to largest
      VEC(STR(len=*),:), INOUT :: self
      VEC(STR(len=len(self(1))),:), allocatable :: smaller,larger
      INT :: n,ns,ne,nl
      STR(len=len(self(1))) :: chosen

      ! The following code is inherited from VEC{OBJECT}
      if (size(self)<=1) return
      n = size(self)
      chosen = self(1)
      ns = count(self<chosen)
      nl = count(self>chosen)
      ne = n-ns-nl
      call create_(smaller,ns)
      smaller = pack(self,self<chosen)
      call quick_sort_increasing_(smaller)
      call create_(larger,nl)
      larger = pack(self,self>chosen)
      call quick_sort_increasing_(larger)
      self(1:ns) = smaller
      self(ns+1:ns+ne) = chosen
      self(ns+ne+1:) = larger
      call destroy_(larger)
      call destroy_(smaller)
   end subroutine

   PURE recursive subroutine quick_sort_decreasing_0(self)
   ! Sort the vector into order, largest to smallest
      VEC(STR(len=*),:), INOUT :: self
      VEC(STR(len=len(self(1))),:), allocatable :: smaller,larger
      INT :: n,ns,ne,nl
      STR(len=len(self(1))) :: chosen

      ! The following code is inherited from VEC{OBJECT}
      if (size(self)<=1) return
      n = size(self)
      chosen = self(1)
      ns = count(self<chosen)
      nl = count(self>chosen)
      ne = n-ns-nl
      call create_(smaller,ns)
      smaller = pack(self,self<chosen)
      call quick_sort_decreasing_(smaller)
      call create_(larger,nl)
      larger = pack(self,self>chosen)
      call quick_sort_decreasing_(larger)
      self(1:nl) = larger
      self(nl+1:nl+ne) = chosen
      self(nl+ne+1:) = smaller
      call destroy_(larger)
      call destroy_(smaller)
   end subroutine

   PURE subroutine quick_sort_1(self,indices,decreasing_order,then_by)
   ! Return the "indices" which sort self from smallest to largest, i.e. on
   ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
   ! If "decreasing_order" is present and TRUE, the indices are sorted from
   ! largest to smallest. If "then_by" is present, equal elements are ordered
   ! by their rows of it, in increasing lexical order.
      VEC(STR(len=*),:), IN :: self
      VEC(INT,:), INOUT :: indices
      BIN, optional, IN :: decreasing_order
      MAT(INT,:,:), optional, IN :: then_by
      BIN :: decreasing
      INT :: i

      ! Initialise indices

      ! The following code is inherited from VEC{OBJECT}
      i = 0
      indices = [(i,i = 1,size(self))]

      ! Decreasing order?
      decreasing = FALSE
      if (present(decreasing_order)) decreasing = decreasing_order

      ! Sort
      if (NOT decreasing) then; call quick_sort_increasing_(self,indices)
      else; call quick_sort_decreasing_(self,indices)
      end if

      ! Order the ties
      if (present(then_by)) call order_ties_by_(self,indices,then_by)
   end subroutine

   PURE subroutine order_ties_by(self,indices,then_by)
   ! Put each run of equal elements in "self(indices)" into increasing lexical
   ! order of the rows "then_by(indices,:)".
      VEC(STR(len=*),:), IN :: self
      VEC(INT,:), INOUT :: indices
      MAT(INT,:,:), IN :: then_by
      INT :: a,b
   ENSURE(size(then_by,1)==size(self),"VEC{STR}:order_ties_by ... then_by needs one row per element")

      ! The following code is inherited from VEC{OBJECT}
      a = 1
      do
         if (a>=size(indices)) exit
         b = a
         do
            if (b==size(indices)) exit
            if (self(indices(b+1))/=self(indices(a))) exit
            b = b+1
         end do
         if (b>a) call sort_by_rows_(indices(a:b),then_by)
         a = b+1
      end do
   end subroutine

   PURE recursive subroutine quick_sort_increasing_1(self,indices)
   ! Return the indices which sort vector from smallest to largest, i.e. on
   ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
      VEC(STR(len=*),:), IN :: self
      VEC(INT,:), INOUT :: indices
      VEC(INT,:), allocatable :: list,small,equal,large,small_indices,equal_indices,large_indices
      INT :: n,i,ns,ne,nl
      STR(len=len(self(1))) :: chosen

      ! The following code is inherited from VEC{OBJECT}
      if (size(indices)<=1) return
      n = size(indices)

      ! List of indices
      i = 0
      call create_(list,n)
      list = [(i,i = 1,n)]

      ! Choose an element
      chosen = self(1)
      ns = count(self<chosen)
      nl = count(self>chosen)
      ne = n-ns-nl

      ! Indices of small/equal/large elements
      call create_(small,ns); call create_(small_indices,ns); small = pack(list,self<chosen); small_indices = indices(small)
      call create_(equal,ne); call create_(equal_indices,ne); equal = pack(list,self==chosen); equal_indices = indices(equal)
      call create_(large,nl); call create_(large_indices,nl); large = pack(list,self>chosen); large_indices = indices(large)

      ! Sort
      if (ns>1) call quick_sort_increasing_(self(small),small_indices)
      if (nl>1) call quick_sort_increasing_(self(large),large_indices)

      ! Assign ordered indices
      indices(1:ns) = small_indices
      indices(ns+1:ns+ne) = equal_indices
      indices(ns+ne+1:) = large_indices

      ! Clean
      call destroy_(large_indices); call destroy_(large)
      call destroy_(equal_indices); call destroy_(equal)
      call destroy_(small_indices); call destroy_(small)
      call destroy_(list)
   end subroutine

   PURE recursive subroutine quick_sort_decreasing_1(self,indices)
   ! Return the indices which sort vector from largest to smallest, i.e. on
   ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
      VEC(STR(len=*),:), IN :: self
      VEC(INT,:), INOUT :: indices
      VEC(INT,:), allocatable :: list,small,equal,large,small_indices,equal_indices,large_indices
      INT :: n,i,ns,ne,nl
      STR(len=len(self(1))) :: chosen

      ! The following code is inherited from VEC{OBJECT}
      if (size(indices)<=1) return
      n = size(indices)

      ! List of indices
      i = 0
      call create_(list,n); list = [(i,i = 1,n)]

      ! Choose an element
      chosen = self(1)
      ns = count(self>chosen)
      nl = count(self<chosen)
      ne = n-ns-nl

      ! Indices of small/equal/large elements
      call create_(small,ns); call create_(small_indices,ns); small = pack(list,self>chosen); small_indices = indices(small)
      call create_(equal,ne); call create_(equal_indices,ne); equal = pack(list,self==chosen); equal_indices = indices(equal)
      call create_(large,nl); call create_(large_indices,nl); large = pack(list,self<chosen); large_indices = indices(large)

      ! Sort
      if (ns>1) call quick_sort_decreasing_(self(small),small_indices)
      if (nl>1) call quick_sort_decreasing_(self(large),large_indices)

      ! Assign ordered indices
      indices(1:ns) = small_indices
      indices(ns+1:ns+ne) = equal_indices
      indices(ns+ne+1:) = large_indices

      ! Clean
      call destroy_(large_indices); call destroy_(large)
      call destroy_(equal_indices); call destroy_(equal)
      call destroy_(small_indices); call destroy_(small)
      call destroy_(list)
   end subroutine

   PURE subroutine reverse_order(self)
   ! Reverse the order of the elements of self
      VEC(STR(len=*),:), INOUT :: self
      INT :: n,dim

      ! The following code is inherited from VEC{INTRINSIC}
      dim = size(self)
      do n = 1,dim/2
         call swap_elements_(self,n,dim-n+1)
      end do
   end subroutine

   pure subroutine swap_elements(self,e1,e2)
   ! Swap elements "e1" and "e2" in "self".
      VEC(STR(len=*),:), INOUT :: self
      INT, IN :: e1,e2

   ! ENSURE(e1<=.dim AND e2<=.dim,"element exceeds dimesion")
   ! ENSURE(e1>0 AND e2>0,"indices must be positive")
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from VEC{INTRINSIC}
      val = self(e1)
      self(e1) = self(e2)
      self(e2) = val
   end subroutine


! =========================================
! Inclusion and matching related operations
! =========================================

   pure function has_any_included_in(self,string,at_start) result (res)
   ! Return TRUE if self has any element included in "string" which starts at
   ! the start of the "string", provided "at_start" is TRUE; otherwise
   ! returns TRUE even if the match was not at the start.
      VEC(STR(len=*),:), IN :: self
      STR(len=*), IN :: string
      BIN, optional, IN :: at_start
      BIN :: res
      BIN :: first
      first = FALSE
      if (present(at_start)) first = at_start
      if (first) then
         res = any(index(spread(string,1,size(self)),self)==1)
      else
         res = any(index(spread(string,1,size(self)),self)/=0)
      end if
   end function

   pure function index_of_first_included_in(self,string) result (res)
   ! Return the index of the first element in self which is included in
   ! "string", or zero otherwise.
      VEC(STR(len=*),:), IN :: self
      STR(len=*), IN :: string
      INT :: res
      INT :: i
      res = 0
      do i = 1,size(self)
         res = index(string,self(i))
         if (res>0) exit
      end do
   end function

   pure function has_any_including(self,string) result (res)
   ! Return TRUE if self has any element which includes "string".
      VEC(STR(len=*),:), IN :: self
      STR(len=*), IN :: string
      BIN :: res
      res = any(index(self,spread(string,1,size(self)))/=0)
   end function

   pure function includes(self,string,at_start) result (res)
   ! Return TRUE for a particular element, if that element of self includes "string".
   ! Returns FALSE if no element matches. If "at_start" is present and TRUE, then the
   ! result is TRUE only if the item matches at the start of the string.
      VEC(STR(len=*),:), IN :: self
      STR(len=*), IN :: string
      BIN, optional, IN :: at_start
      VEC(BIN,size(self)) :: res
      BIN :: first
      first = FALSE
      if (present(at_start)) first = at_start
      if (first) then
         res = index(self,spread(string,1,size(self)))==1
      else
         res = index(self,spread(string,1,size(self)))/=0
      end if
   end function

   pure function index_of_first_that_includes(self,string) result (res)
   ! Return the index of the first element of self that includes "string".
   ! Returns 0 if no element matches.
      VEC(STR(len=*),:), IN :: self
      STR(len=*), IN :: string
      INT :: res
      INT :: i
      res = 0
      do i = 1,size(self)
         res = index(self(i),string)
         if (res==0) cycle
         res = i
         exit
      end do
   end function

   pure function index_of(self,string) result (res)
   ! Return the first index of the "string" in self.
   ! Returns 0 if no element matches.
      VEC(STR(len=*),:), IN :: self
      STR(len=*), IN :: string
      INT :: res
      INT :: i
      res = 0
      do i = 1,size(self)
         if (self(i)/=string) cycle
         res = i
         exit
      end do
   end function

   PURE function index_of_matching_bracket(self,symbol) result (res)
   ! Return the first index of the matching bracket "symbol" in self.
   ! The first element of self need not be an opening bracket symbol.
      VEC(STR(len=*),:), IN :: self
      STR(len=*), IN :: symbol
      INT :: res
      VEC(STR(len=1),4) :: opening,closing
      STR(len=1) :: op,cl
      INT :: i,s,n
   ENSURE(index_of_(self,symbol)>0,"VEC{STR}:index_of_matching_bracket ... no open bracket symbol in self")
   ENSURE(index_of_(self,symbol)<size(self),"VEC{STR}:index_of_matching_bracket ... open bracket is at end of self")
      opening = ["{","(","[","<"]
      closing = ["}",")","]",">"]
      ENSURE(any(symbol==opening),"VEC{STR}:index_of_matching_bracket ... unrecognised open bracket symbol")
      op = symbol
      cl = closing(index_of_(opening,symbol))
      s = index_of_(self,op)
      n = 0
      do i = s+1,size(self)
         if (self(i)==op) n = n+1
         if (self(i)==cl AND n==0) exit
         if (self(i)==cl AND n>0) n = n-1
      end do
      res = i
      ENSURE(n==0,"VEC{STR}:index_of_matching_bracket ... unmatching number of closing bracket symbols")
   end function


! =============
! String widths
! =============

   pure function str_lengths(self) result (res)
   ! Returns the minimal string lengths.
      VEC(STR(len=*),:), IN :: self
      VEC(INT,size(self)) :: res
      res = len_trim(self)
   end function

   pure subroutine get_max_str_length(self,msl,spaces)
   ! Returns "msl", the maximum of the string lengths for "self",
   ! and with optional extra "spaces". 
      VEC(STR(len=*),:), IN :: self
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

! Below are strangely named?

   pure function to_str_vec_str_0(self) result (res)
   ! Change self to a string of minimal length
      VEC(STR(len=*),:), IN :: self
      VEC(STR(len=STR_SIZE),size(self)) :: res
      res = self
   end function

   pure function to_str_vec_str_1(self,fmt,left_justify) result (res)
   ! Change self to a "string" using the specified fortran "fmt".
      VEC(STR(len=*),:), IN :: self
      STR(len=*), IN :: fmt
      BIN, optional, IN :: left_justify
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = to_str_(self(i),fmt,left_justify)
      end do
   end function

   pure function to_str_vec_str_2(self,width,left_justify) result (res)
   ! Change self to a string with specified "width".
      VEC(STR(len=*),:), IN :: self
      INT, IN :: width
      BIN, optional, IN :: left_justify
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = to_str_(self(i),width,left_justify)
      end do
   end function

   PURE function char_array_to_str(self) result (res)
   ! Change len=1 char array "self" to a string
      STR(len=1), dimension(:), IN :: self
      STR(len=STR_SIZE) :: res
      INT :: i
   ENSURE(size(self)<=len(res),"VEC{STR}:char_array_to_str ... self is too large")
      do i = 1,len(res)
         if (i<=size(self)) then
            res(i:i) = self(i)
         else
            res(i:i) = " "  ! pad with spaces
         end if
      end do
   end function

   pure function to_concatenated_str(self,fmt,separator) result (res)
   ! Change self to a string using "fmt" if specified, and with
   ! "separator" between each item, if specified.
      VEC(STR(len=*),:), IN :: self
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

   pure subroutine concatenate_elementwise_with_0(self,v)
   ! Yield a vector which is the elementwise concatenation of "self"
   ! and "v" 
      VEC(STR(len=*),:), INOUT :: self
      VEC(STR(len=*),:), IN :: v
      INT :: i
      do i = 1,size(self)
         self(i) = trim(self(i))//trim(v(i))
      end do
   end subroutine

   pure subroutine concatenate_elementwise_with_1(self,value)
   ! Yield a vector which is the elementwise concatenation of "self"
   ! and "value".
      VEC(STR(len=*),:), INOUT :: self
      STR(len=*), IN :: value
      INT :: i
      do i = 1,size(self)
         self(i) = trim(self(i))//trim(value)
      end do
   end subroutine


! ==========
! Misc stuff
! ==========

   pure subroutine to_lower_case(self)
   ! Change upper case charaters to lower case in all elements
      VEC(STR(len=*),:), INOUT :: self
      INT :: i
      do i = 1,size(self)
         call to_lower_case_(self(i))
      end do
   end subroutine

   pure subroutine to_upper_case(self)
   ! Change lower case charaters to upper case in all elements
      VEC(STR(len=*),:), INOUT :: self
      INT :: i
      do i = 1,size(self)
         call to_upper_case_(self(i))
      end do
   end subroutine

   pure subroutine replace_all(self,a,b)
   ! Replace all occurences of string "a" by "b". String "b" can be zero
   ! length, however, replacements only occur up to the last nonblank
   ! character in "self" i.e. up to len_trim(self).
      VEC(STR(len=*),:), INOUT :: self
      STR(len=*), IN :: a,b
      INT :: i
      do i = 1,size(self)
         call replace_all_(self(i),a,b)
      end do
   end subroutine

   pure subroutine remove_blanks(self)
   ! Replace all blanks by moving all non-blank characters leftwards
      VEC(STR(len=*),:), INOUT :: self
      INT :: i
      do i = 1,size(self)
         call remove_blanks_(self(i))
      end do
   end subroutine

   pure subroutine pad_with(self,symbol,width,prepad)
   ! Pad the end of the string with "symbol" up to length "width".
   ! If "prepad" is present and TRUE, put the symbol before.
      VEC(STR(len=*),:), INOUT :: self
      STR(len=1), IN :: symbol
      INT, IN :: width
      BIN, IN, optional :: prepad
      BIN :: pre
      INT :: i,l
      pre = FALSE
      if (present(prepad)) pre = prepad
      if (pre) then
         do i = 1,size(self)
            l = len_trim(self(i))
            if (l>=width) cycle
            self(i) = trim(repeat(symbol,width-l))//trim(self(i))
         end do
      else
         do i = 1,size(self)
            l = len_trim(self(i))
            if (l>=width) cycle
            self(i) = trim(self(i))//repeat(symbol,width-l)
         end do
      end if
   end subroutine

end module
