!====================================================================
!
! VEC{REAL}: Vector operations ...
!
! Copyright (C) Dylan Jayatilaka, 1997
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
module VEC_REAL_MODULE

#  include "macros"

   include "vec{real}.use"

   implicit none

   include "vec{real}.int"


contains

!  =================
!  Memory allocation
!  =================

   PURE subroutine create_0(self,dim)
   ! Allocate vector
      VEC(REAL,:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{REAL}:create_0 ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{REAL}:create_0 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine create_1(self,lb,ub)
   ! Allocate the vector with lower bound "lb", upper bound "ub"
      VEC(REAL,:), allocatable, OUT :: self
      INT, IN :: lb,ub

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb:ub),stat=status)
      ENSURE(status==0,"VEC{REAL}:create_1 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine create_2(self,bounds)
   ! Allocate the vector with lower and upper bounds in "bounds"
      VEC(REAL,:), allocatable, OUT :: self
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
      ENSURE(status==0,"VEC{REAL}:create_2 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      VEC(REAL,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{REAL}:destroy ... error in deallocating memory")
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
      VEC(REAL,:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: chop_start
      VEC(REAL,:), allocatable :: new
      INT :: lb,ub
      BIN :: chop_end
   ENSURE(allocated(self),"VEC{REAL}:shrink ... no self array")
   ENSURE(dim<=size(self),"VEC{REAL}:shrink ... dim too large")

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
      VEC(REAL,:), allocatable, INOUT :: self
      INT, IN :: dim
      REAL, optional, IN :: init
      BIN, optional, IN :: grow_start
      INT :: lb,ub,nc
      BIN :: start
      VEC(REAL,:), allocatable :: new
      REAL :: val

      ! Value to initialize

      ! The following code is inherited from VEC{OBJECT}
      val = ZERO
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
         ENSURE(dim>size(self),"VEC{REAL}:expand ... dim not large enough")
         ENSURE(dim>0,"VEC{REAL}:expand ... dim is not +ve")

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
      VEC(REAL,:), allocatable, INOUT :: self
      VEC(REAL,:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = v
      else
         self = [self,v]
      end if
   end subroutine

   pure subroutine append_1(self,value)
   ! Appends the scalar "value" onto the end of allocatable "self".
      VEC(REAL,:), allocatable, INOUT :: self
      REAL, IN :: value

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [value]
      else
         self = [self,value]
      end if
   end subroutine

   PURE subroutine append_2(self,v1,v2)
   ! Appends vectors "v1" and "v2" to allocatable "self".
      VEC(REAL,:), allocatable, INOUT :: self
      VEC(REAL,:), IN :: v1,v2

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [v1,v2]
      else
         self = [self,v1,v2]
      end if
   end subroutine

   PURE subroutine prepend_0(self,v)
   ! Prepend vector "v" to allocatable "self".
      VEC(REAL,:), allocatable, INOUT :: self
      VEC(REAL,:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [v]
      else
         self = [v,self]
      end if
   end subroutine

   PURE subroutine prepend_1(self,value)
   ! Prepend a single "value" to allocatable "self".
      VEC(REAL,:), allocatable, INOUT :: self
      REAL, IN :: value

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
      VEC(REAL,:), allocatable, INOUT :: self
      INT, IN :: k
      VEC(REAL,:), allocatable :: old
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


!  ====================
!  Comparison functions
!  ====================

   pure function equals(self,v,eps) result (res)
   ! Return true if "self" is the same as "v", within range "eps" if supplied
      VEC(REAL,:), IN :: self
      VEC(REAL,:), IN :: v
      REAL, optional, IN :: eps
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = same_as_(self,v,eps)
   end function

   pure function same_as(self,v,eps) result (res)
   ! Return true if "self" is the same as "v", within range "eps" if supplied
      VEC(REAL,:), IN :: self
      VEC(REAL,:), IN :: v
      REAL, optional, IN :: eps
      BIN :: res
      REAL :: tol

      ! The following code is inherited from VEC{INTRINSIC}
      if (size(self)/=size(v)) then
         res = FALSE
      else
         tol = REAL_EPSILON
         if (present(eps)) tol = eps
         res = distance_to_(self,v)<tol
      end if
   end function

   pure function has_all_equal(self,eps) result (res)
   ! Return TRUE if "self" contains all the same elements, to within precision
   ! "eps", if supplied.
      VEC(REAL,:), IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = TRUE
      do i = 2,size(self)
         if (equals_(self(1),self(i),eps)) cycle
         res = FALSE
         exit
      end do
   end function

   pure function has_all_equal_to(self,val,eps) result (res)
   ! Return TRUE if "self" contains all the same values "val", to within
   ! precision "eps", if supplied.
      VEC(REAL,:), IN :: self
      REAL, IN :: val
      REAL, optional, IN :: eps
      BIN :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = TRUE
      do i = 1,size(self)
         if (equals_(val,self(i),eps)) cycle
         res = FALSE
         exit
      end do
   end function

   pure function has_any_equal_to(self,val,eps) result (res)
   ! Return TRUE if "self" has any values the same as "val", to within
   ! precision "eps", if supplied.
      VEC(REAL,:), IN :: self
      REAL, IN :: val
      REAL, optional, IN :: eps
      BIN :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = FALSE
      do i = 1,size(self)
         if (equals_(val,self(i),eps)) then
            res = TRUE
            exit
         end if
      end do
   end function

   pure function is_zero(self,eps) result (res)
   ! Return true if the vector is zero (within "eps", if supplied)
      VEC(REAL,:), IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      REAL :: tmp

      ! The following code is inherited from VEC{INTRINSIC}
      tmp = norm_(self)
      res = is_zero_(tmp,eps)
   end function

   pure function is_monotone(self) result (res)
   ! Return true if the vector is monotone increasing or decreasing
      VEC(REAL,:), IN :: self
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = is_monotonically_increasing_(self) OR is_monotonically_decreasing_(self)
   end function

   pure function is_monotonically_increasing(self) result (res)
   ! Return true if the vector is monotonically increasing
      VEC(REAL,:), IN :: self
      BIN :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = TRUE
      do i = 1,size(self)-1
         if (self(i+1)>self(i)) cycle
         res = FALSE
         exit
      end do
   end function

   pure function is_monotonically_decreasing(self) result (res)
   ! Return true if the vector is monotonically decreasing
      VEC(REAL,:), IN :: self
      BIN :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = TRUE
      do i = 1,size(self)-1
         if (self(i+1)<self(i)) cycle
         res = FALSE
         exit
      end do
   end function

   function is_z_axis(self,eps) result (res)
   ! Return true if the vector is set to the z-axis
      VEC(REAL,:), IN :: self
      REAL, IN, optional :: eps
      BIN :: res
   ENSURE(size(self)==3,"VEC{REAL}:is_z_axis ... must supply a 3 dimensional vector!")
      res = equals_(self(1),ZERO,eps)
      res = equals_(self(2),ZERO,eps) AND res
      res = equals_(self(3),ONE,eps) AND res
   end function


!  =============================
!  Repetition related operations
!  =============================

   PURE subroutine remove_repetitions(self,eps)
   ! Remove repeated elements which come later in the list. 
   ! NOTE: allocatable
   ! NOTE: the vector may shrink. 
   ! NOTE: this version for reals & complex
      VEC(REAL,:), allocatable, INOUT :: self
      REAL, optional, IN :: eps
      VEC(REAL,:), allocatable :: unique
      INT :: i,j,n
      BIN :: found
   ENSURE(allocated(self),"VEC{REAL}:remove_repetitions ... no vector")

      ! The following code is inherited from VEC{INTRINSIC}
      if (size(self)==1) return
      call create_(unique,size(self))
      n = 1
      unique(1) = self(1)
      do i = 2,size(self)
         found = FALSE
         do j = 1,n
            if (NOT equals_(self(i),unique(j),eps)) cycle
            found = TRUE
         end do
         if (found) cycle
         n = n+1
         unique(n) = self(i)
      end do
      call destroy_(self)
      call create_(self,n)
      self = unique(1:n)
      call destroy_(unique)
   end subroutine

   pure function has_repetitions(self,eps) result (res)
   ! Return TRUE if self has at least one repeated element.
   ! NOTE: this version for real, cpx
      VEC(REAL,:), IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      INT :: i,j

      ! The following code is inherited from VEC{INTRINSIC}
      res = FALSE
      if (size(self)==1) return
      do i = 2,size(self)
         do j = 1,i-1
            if (equals_(self(i),self(j),eps)) cycle
            res = TRUE
            return
         end do
      end do
   end function

   pure function no_of_unique_elements(self,eps) result (res)
   ! Return the number of unique elements in the vector.
   ! NOTE: this version for reals, cpx
      VEC(REAL,:), IN :: self
      REAL, optional, IN :: eps
      INT :: res
      INT :: n,i
      BIN :: same

      ! The following code is inherited from VEC{INTRINSIC}
      res = 1
      do n = 2,size(self)
         same = FALSE
         do i = 1,n-1
            if (equals_(self(n),self(i),eps)) cycle
            same = TRUE
            exit
         end do
         if (NOT same) res = res+1
      end do
   end function

   PURE subroutine make_repetition_count(self,cnt,n_kind,eps)
   ! Create "cnt" where each element cnt(i) is a count of the number
   ! times that self(i) is repeated in self(:). Later elements of
   ! "cnt" which are repetitions of earlier ones are assigned a count
   ! of zero. The number of different kinds is returned in "n_kind".
   ! NOTE: allocatable
   ! NOTE: this version for REAL, CPX
      VEC(REAL,:), IN :: self
      VEC(INT,:), allocatable, OUT :: cnt
      INT, OUT :: n_kind
      REAL, optional, IN :: eps
      INT :: n,l

      ! The following code is inherited from VEC{INTRINSIC}
      call create_(cnt,size(self))
      cnt = 1
      do n = 1,size(self)
         if (cnt(n)==0) cycle
         do l = n+1,size(self)
            if (cnt(l)==0) cycle
            if (NOT equals_(self(l),self(n),eps)) cycle
            cnt(n) = cnt(n)+1
            cnt(l) = 0
         end do
      end do
      n_kind = size(self)-count(cnt==0)
   end subroutine

   PURE subroutine make_unique_element_maps(self,unique_element,unique_element_for)
   ! Make a map of unique_element's, and the inverse map
   ! unique_element_for a given non-unique element in the list.
   ! NOTE: allocatable
      VEC(REAL,:), IN :: self
      VEC(INT,:), allocatable, OUT :: unique_element
      VEC(INT,:), allocatable, OUT :: unique_element_for
      INT :: n,k,i

      ! The following code is inherited from VEC{INTRINSIC}
      call create_(unique_element_for,size(self))
      call create_(unique_element,size(self))
      if (size(self)==0) return
      unique_element_for(1) = 1
      unique_element(1) = 1
      if (size(self)==1) return
      k = 1
      do n = 2,size(self)
         i = index_of_value_(self(1:n),self(n))
         if (i==n) then

            ! It's unique
            k = k+1
            unique_element(k) = n
            unique_element_for(n) = k
         else

            ! Bah. It's not unique
            unique_element_for(n) = unique_element_for(i)
         end if
      end do
      call shrink_(unique_element,k)
   end subroutine


!  ================
!  Sorting routines
!  ================

   PURE subroutine sort(self,decreasing_order)
   ! Sort array "self" from lowest to highest, using simple insertion sort.  If
   ! "decreasing_order" is present and TRUE sort from highest to lowest instead.
      VEC(REAL,:), INOUT :: self
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
      VEC(REAL,:), INOUT :: self
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
      VEC(REAL,:), INOUT :: self
      VEC(REAL,:), allocatable :: smaller,larger
      INT :: n,ns,ne,nl
      REAL :: chosen

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
      VEC(REAL,:), INOUT :: self
      VEC(REAL,:), allocatable :: smaller,larger
      INT :: n,ns,ne,nl
      REAL :: chosen

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
      VEC(REAL,:), IN :: self
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
      VEC(REAL,:), IN :: self
      VEC(INT,:), INOUT :: indices
      MAT(INT,:,:), IN :: then_by
      INT :: a,b
   ENSURE(size(then_by,1)==size(self),"VEC{REAL}:order_ties_by ... then_by needs one row per element")

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
      VEC(REAL,:), IN :: self
      VEC(INT,:), INOUT :: indices
      VEC(INT,:), allocatable :: list,small,equal,large,small_indices,equal_indices,large_indices
      INT :: n,i,ns,ne,nl
      REAL :: chosen

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
      VEC(REAL,:), IN :: self
      VEC(INT,:), INOUT :: indices
      VEC(INT,:), allocatable :: list,small,equal,large,small_indices,equal_indices,large_indices
      INT :: n,i,ns,ne,nl
      REAL :: chosen

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

   pure subroutine reverse_order(self)
   ! Reverse the order of the elements of self
      VEC(REAL,:), INOUT :: self
      INT :: n,dim

      ! The following code is inherited from VEC{INTRINSIC}
      dim = size(self)
      do n = 1,dim/2
         call swap_elements_(self,n,dim-n+1)
      end do
   end subroutine

   pure subroutine swap_elements(self,e1,e2)
   ! Swap elements "e1" and "e2" in "self".
      VEC(REAL,:), INOUT :: self
      INT, IN :: e1,e2

   ! ENSURE(e1<=.dim AND e2<=.dim,"element exceeds dimesion")
   ! ENSURE(e1>0 AND e2>0,"indices must be positive")
      REAL :: val

      ! The following code is inherited from VEC{INTRINSIC}
      val = self(e1)
      self(e1) = self(e2)
      self(e2) = val
   end subroutine

   PURE subroutine set_to_flattened_reverse_of(self,b)
   ! Set "self" to the flattened reverse of vector "b" i.e. the standard order
   ! is of "b" reversed and placed as a vector.
      VEC(REAL,:), OUT :: self
      MAT3(REAL,:,:,:), IN :: b
      INT :: n,i,j,k
   ENSURE(size(self)==size(b),"VEC{REAL}:set_to_flattened_reverse_of ... incompatible sizes")

      ! The following code is inherited from VEC{INTRINSIC}
      n = 0
      do k = 1,size(b,1)
         do j = 1,size(b,2)
            do i = 1,size(b,3)
               n = n+1
               self(n) = b(k,j,i)
            end do
         end do
      end do
   end subroutine


!  ====================
!  Numerical operations
!  ====================

   pure subroutine set_to(self,v)
   ! Set the vector to "v". See also the "copy" routine.
      VEC(REAL,:), INOUT :: self
      VEC(REAL,:), IN :: v

      ! The following code is inherited from VEC{INTRINSIC}
      self = v
   end subroutine

   pure subroutine plus(self,v,mask)
   ! Add vector "v" to "self"
      VEC(REAL,:), INOUT :: self
      VEC(BIN,:), optional, IN :: mask
      VEC(REAL,:), IN :: v
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      if (NOT present(mask)) then
         self = self+v
      else
         do i = 1,size(self)
            if (mask(i)) self(i) = self(i)+v(i)
         end do
      end if
   end subroutine

   pure subroutine minus(self,v,mask)
   ! Subtract vector "v" from "self"
      VEC(REAL,:), INOUT :: self
      VEC(REAL,:), IN :: v
      VEC(BIN,:), optional, IN :: mask
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      if (NOT present(mask)) then
         self = self-v
      else
         do i = 1,size(self)
            if (mask(i)) self(i) = self(i)-v(i)
         end do
      end if
   end subroutine

   pure subroutine to_scaled(self,v,fac)
   ! Set the vector to "v" scaled by "fac"
      VEC(REAL,:), INOUT :: self
      VEC(REAL,:), IN :: v
      REAL, IN :: fac

      ! The following code is inherited from VEC{INTRINSIC}
      self = fac*v
   end subroutine

   pure subroutine plus_scaled(self,v,fac)
   ! Add a vector "v" scaled by "fac" to "self"
      VEC(REAL,:), INOUT :: self
      VEC(REAL,:), IN :: v
      REAL, IN :: fac

      ! The following code is inherited from VEC{INTRINSIC}
      self = self+fac*v
   end subroutine

   pure function sum_elements(self) result (res)
   ! Return the sum of the elements in "self"
      VEC(REAL,:), IN :: self
      REAL :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         res = res+self(i)
      end do
   end function

   pure function dot_0(self,v) result (res)
   ! Return the dot product with "v"
      VEC(REAL,:), IN :: self
      VEC(REAL,:), IN :: v
      REAL :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         res = res+(self(i))*v(i)
      end do
   end function

   pure function dot_1(self,v) result (res)
   ! Return the dot product with "v"
      VEC(REAL,:), IN :: self
      VEC(CPX,:), IN :: v
      CPX :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         res = res+(self(i))*v(i)
      end do
   end function

   pure function cross(self,v) result (res)
   ! Return the cross product of "self" and "v".
      VEC(REAL,:), IN :: self
      VEC(REAL,:), IN :: v
      VEC(REAL,3) :: res

   ! ENSURE(.dim==3,"self must have dimension 3")

      ! The following code is inherited from VEC{INTRINSIC}
      res(1) = self(2)*v(3)-v(2)*self(3)
      res(2) = self(3)*v(1)-v(3)*self(1)
      res(3) = self(1)*v(2)-v(1)*self(2)
   end function

   pure subroutine to_cross_product_of(self,u,v)
   ! Set the vector to the cross product of "u" and "v".
      VEC(REAL,:), INOUT :: self
      VEC(REAL,:), IN :: u,v

   ! ENSURE(.dim==3,"self must have dimension 3")

      ! The following code is inherited from VEC{INTRINSIC}
      self(1) = u(2)*v(3)-v(2)*u(3)
      self(2) = u(3)*v(1)-v(3)*u(1)
      self(3) = u(1)*v(2)-v(1)*u(2)
   end subroutine

   PURE subroutine to_product_of(self,a,v,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v". If present,
   ! "transpose_a" can be set to TRUE if the matrix needs to be transposed.
      VEC(REAL,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(REAL,:), IN :: v
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      REAL :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{REAL}:to_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{REAL}:to_product_of ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{REAL}:to_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{REAL}:to_product_of ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = val
         end do
      end if
   end subroutine

   PURE subroutine plus_product_of(self,a,v,transpose_a)
   ! Add to "self" the product of the matrix "a" and vector "v". If present,
   ! "transpose_a" can be set to TRUE if the marix needs to be transposed.
      VEC(REAL,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(REAL,:), IN :: v
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      REAL :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{REAL}:plus_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{REAL}:plus_product_of ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{REAL}:plus_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{REAL}:plus_product_of ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+val
         end do
      end if
   end subroutine

   PURE subroutine minus_product_of(self,a,v,transpose_a)
   ! Subtract from "self" the product of the matrix "a" and vector "v". If
   ! present, "transpose_a" can be set to TRUE if the marix needs to be
   ! transposed.
      VEC(REAL,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(REAL,:), IN :: v
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      REAL :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{REAL}:minus_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{REAL}:minus_product_of ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)-val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{REAL}:minus_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{REAL}:minus_product_of ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)-val
         end do
      end if
   end subroutine

   PURE subroutine to_scaled_product_of(self,a,v,fac,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(REAL,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(REAL,:), IN :: v
      REAL, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      REAL :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{REAL}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{REAL}:to_scaled_product_of ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = fac*val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{REAL}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{REAL}:to_scaled_product_of ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = fac*val
         end do
      end if
   end subroutine

   PURE subroutine plus_scaled_product_of(self,a,v,fac,transpose_a)
   ! Add to self "self" the product of the matrix "a" and vector "v" scaled by
   ! "fac".  If present, "transpose_a" can be set to TRUE if the matrix needs to
   ! be transposed.
      VEC(REAL,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(REAL,:), IN :: v
      REAL, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      REAL :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{REAL}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{REAL}:plus_scaled_product_of ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{REAL}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{REAL}:plus_scaled_product_of ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      end if
   end subroutine

   PURE subroutine rotate_by(self,a)
   ! Rotate self by matrix "a", treating self as a column vector
      VEC(REAL,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(REAL,:), allocatable :: v
   ENSURE(size(a,1)==size(a,2),"VEC{REAL}:rotate_by ... matrix must be square")
   ENSURE(size(a,2)==size(self),"VEC{REAL}:rotate_by ... matrix and vector dimensions inconsistent")

      ! The following code is inherited from VEC{INTRINSIC}
      v = self
      call to_product_of_(self,a,v)
      call destroy_(v)
   end subroutine

   PURE subroutine translate_by(self,v)
   ! Translate self by vector "v".
      VEC(REAL,:), INOUT :: self
      VEC(REAL,:), IN :: v
   ENSURE(size(self)==size(v),"VEC{REAL}:translate_by ... vectors not of same dimension")

      ! The following code is inherited from VEC{INTRINSIC}
      self = self+v
   end subroutine

   PURE subroutine seitz_multiply(self,seitz)
   ! Self is operated on by the seitz matrix.  Self must be in fractional
   !  coordinates.
      VEC(REAL,:), INOUT :: self
      MAT(REAL,:,:), IN :: seitz
   ENSURE(size(seitz,1)==4,"VEC{REAL}:seitz_multiply ... seitz matrix must be 4x4")
   ENSURE(size(seitz,2)==4,"VEC{REAL}:seitz_multiply ... seitz matrix must be 4x4")
   ENSURE(size(self)==3,"VEC{REAL}:seitz_multiply ... vector not of dimension 3")
      call rotate_by_(self,seitz(1:3,1:3))
      call translate_by_(self,seitz(4,1:3))
   end subroutine

   pure function outer_product_with(self,v) result (res)
   ! Returns the outer product of "self" with vector "v".
      VEC(REAL,:), IN :: self
      VEC(REAL,:), IN :: v
      MAT(REAL,size(self),size(v)) :: res
      INT :: i,j

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         do j = 1,size(v)
            res(i,j) = self(i)*v(j)
         end do
      end do
   end function

   pure function outer_product(self) result (res)
   ! Returns the outer product of "self" with itself.
      VEC(REAL,:), IN :: self
      MAT(REAL,size(self),size(self)) :: res
      INT :: i,j

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         do j = 1,size(self)
            res(i,j) = self(i)*self(j)
         end do
      end do
   end function

   pure function norm(self) result (res)
   ! Return the norm of the vector
      VEC(REAL,:), IN :: self
      REAL :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = sqrt(real(dot_product(self,self),kind=REAL_KIND))
   end function

   pure subroutine normalize(self)
   ! Normalise the vector
      VEC(REAL,:), INOUT :: self

      ! The following code is inherited from VEC{INTRINSIC}
      self = self/norm_(self)
   end subroutine

   PURE subroutine canonicalise_sign(self,tol)
   ! Fix the arbitrary overall sign of "self" by making its largest-magnitude
   ! element positive.
   !
   ! An eigenvector is defined only up to a sign, and LAPACK does not say which
   ! one it returns, so the same matrix can come back with whole columns negated
   ! by a different library or platform.
   !
   ! Elements within RELATIVE tolerance "tol" of the largest count as tied, and
   ! the LOWEST-indexed of them decides. The tolerance is the point of the
   ! routine, not a detail: with an exact test, two components equal to within
   ! round-off select different elements on different platforms and the whole
   ! vector flips -- which is the failure this exists to stop. A generous "tol"
   ! is safe, since choosing a near-maximal element rather than the maximal one
   ! is still deterministic.
   !
   ! An all-zero vector has no sign to fix and is left alone.
      VEC(REAL,:), INOUT :: self
      REAL, IN, optional :: tol
      REAL :: maxv,eps
      INT :: i,ind
   ENSURE(size(self)>0,"VEC{REAL}:canonicalise_sign ... zero-length vector")
      eps = TOL(9)
      if (present(tol)) eps = tol
      maxv = largest_value_(self)
      if (maxv>ZERO) then
         ind = 1
         do i = 1,size(self)
            if (abs(self(i))<maxv*(ONE-eps)) cycle
            ind = i
            exit
         end do
         if (self(ind)<ZERO) self = -self
      end if
   end subroutine

   pure function distance_to(self,a) result (res)
   ! Return the distance to "a" if supplied
      VEC(REAL,:), IN :: self
      VEC(REAL,:), IN :: a
      REAL :: res
      VEC(REAL,size(self)) :: temp

      ! The following code is inherited from VEC{INTRINSIC}
      temp = self-a
      res = norm_(temp)
   end function

   PURE subroutine to_distances_between(self,mx,pos)
   ! Set self to the distance between columns of "mx" and  "pos"
      VEC(REAL,:), OUT :: self
      MAT(REAL,:,:), IN :: mx
      VEC(REAL,:), IN :: pos
      INT :: i
      VEC(REAL,:), allocatable :: v
      call create_(v,size(self))
      do i = 1,size(mx,2)
         v = mx(:,i)-pos
         self(i) = norm_(v)
      end do
      call destroy_(v)
   end subroutine

   pure function mean(self) result (res)
   ! Return the mean of the vector
      VEC(REAL,:), IN :: self
      REAL :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = sum(self)/REALIFY(size(self))
   end function

   pure function variance(self) result (res)
   ! Return the variance of the vector from its mean
      VEC(REAL,:), IN :: self
      REAL :: res
      VEC(REAL,:), allocatable :: v
      REAL :: mean
      mean = mean_(self)
      allocate(v(size(self)))
      v = self-mean
      res = dot_(v,v)/(REALIFY(size(self))-ONE)  !Kang corrected 01.Jul.2024
      deallocate(v)  ! Since "self - [sample mean]" is used instead of "self - [population mean] 
                                             ! it should be division by (N-1) but hot by N.
   end function

   pure function standard_deviation(self) result (res)
   ! Return the standard deviation of the vector from its mean
      VEC(REAL,:), IN :: self
      REAL :: res
      res = variance_(self)
      if (res/=ZERO) res = sqrt(res)
   end function

   pure function arcsinh(self) result (res)
   ! Return the arcsinh of self, where self is a vector of any real numbers.
      VEC(REAL,:), IN :: self
      VEC(REAL,size(self)) :: res
      res = log(self+sqrt(ONE+self*self))
   end function


!   integrate(a,b,accuracy) result (res) :: recursive, functional
!   ! Integrate the vector valued scalar function "self" between the limits
!   ! "a" and "b" using adaptive trapezoidal rule with Simpsons approximation.
!   ! If present, "accuracy" is the required accuracy of the integral.
!      interface
!         self(x) result (res)
!             x :: REAL
!            res :: VEC{REAL}@
!         end
!      end
!      a,b :: REAL
!      accuracy :: REAL, optional
!      res :: VEC{REAL}@
!       n :: INT
!      same :: BIN
!      tol,h,m :: REAL
!      fa,fb,fm,one_trap,two_trap,left,right :: VEC{REAL},PTR
!      tol = TOL(6)
!      if (present(accuracy)) tol = accuracy
!      h  = b-a
!      m  = (a+b)/TWO
!      fa => self(a)
!      fb => self(b)
!      fm => self(m)
!      n = size(fa)
!      one_trap.create(n)
!      two_trap.create(n)
!      one_trap = h*(fa+fb)/TWO
!      two_trap = h*(fa+TWO*fm+fb)/FOUR
!      fm.destroy
!      fb.destroy
!      fa.destroy
!      res.create(n)
!      res = abs(one_trap-two_trap)
!      same = maxval(res) < THREE*tol
!      if (same) then
!         res = (FOUR*two_trap - one_trap)/THREE
!         two_trap.destroy
!         one_trap.destroy
!      else
!         two_trap.destroy
!         one_trap.destroy
!         left  => .integrate(a,m,tol/TWO)
!         right => .integrate(m,b,tol/TWO)
!         res = left + right
!         right.destroy
!         left.destroy
!      end
!   end

!  ===========
!  Contraction
!  ===========

   subroutine to_contraction_i2_of_0(self,v,mx)
   ! Set "self" to the contraction of the second index of "v" with all
   ! indices of "mx", and collect the result over the first index of "v".
      VEC(REAL,:), INOUT :: self
      MAT(REAL,:,:), IN :: mx
      MAT(REAL,:,:), IN :: v
      INT :: dim,i1,i2,n
   ENSURE(size(mx,1)==size(v,2),"VEC{REAL}:to_contraction_i2_of_0 ... mx and v dim2 are inconsistent")
   ENSURE(size(self)==size(v,1),"VEC{REAL}:to_contraction_i2_of_0 ... self and v dim1 are inconsistent")
      dim = size(mx,1)
      self = ZERO
      do i2 = 1,dim
         do i1 = 1,dim
            do n = 1,size(self)
               self(n) = self(n)+mx(i1,i2)*v(n,i1)*v(n,i2)
            end do
         end do
      end do
   end subroutine

   subroutine to_contraction_i2_of_1(self,v,mx)
   ! Set "self" to the contraction of the second index of "v" with all
   ! indices of "mx", and collect the result over the first index of "v".
      VEC(REAL,:), INOUT :: self
      MAT(REAL,:,:), IN :: v
      MAT3(REAL,:,:,:), IN :: mx
      INT :: dim,i1,i2,i3,n
   ENSURE(size(mx,1)==size(v,2),"VEC{REAL}:to_contraction_i2_of_1 ... mx and v dim2 are inconsistent")
   ENSURE(size(self)==size(v,1),"VEC{REAL}:to_contraction_i2_of_1 ... self and v dim1 are inconsistent")
      dim = size(mx,1)
      self = ZERO
      do i3 = 1,dim
         do i2 = 1,dim
            do i1 = 1,dim
               do n = 1,size(self)
                  self(n) = self(n)+mx(i1,i2,i3)*v(n,i1)*v(n,i2)*v(n,i3)
               end do
            end do
         end do
      end do
   end subroutine

   subroutine to_contraction_i2_of_2(self,v,mx)
   ! Set "self" to the contraction of the second index of "v" with all
   ! indices of "mx", and collect the result over the first index of "v".
      VEC(REAL,:), INOUT :: self
      MAT4(REAL,:,:,:,:), IN :: mx
      MAT(REAL,:,:), IN :: v
      INT :: dim,i1,i2,i3,i4,n
   ENSURE(size(mx,1)==size(v,2),"VEC{REAL}:to_contraction_i2_of_2 ... mx and v dim2 are inconsistent")
   ENSURE(size(self)==size(v,1),"VEC{REAL}:to_contraction_i2_of_2 ... self and v dim1 are inconsistent")
      dim = size(mx,1)
      self = ZERO
      do i4 = 1,dim
         do i3 = 1,dim
            do i2 = 1,dim
               do i1 = 1,dim
                  do n = 1,size(self)
                     self(n) = self(n)+mx(i1,i2,i3,i4)*v(n,i1)*v(n,i2)*v(n,i3)*v(n,i4)
                  end do
               end do
            end do
         end do
      end do
   end subroutine


!  ==================
!  Min/max operations
!  ==================

   pure subroutine chop_small_values(self,val)
   ! Set all values in the self whose value is smaller than "val" to "val".
      VEC(REAL,:), INOUT :: self
      REAL, IN :: val
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         if (self(i)<val) self(i) = val
      end do
   end subroutine

   pure subroutine chop_large_values(self,val)
   ! Set all values in the self whose value is larger than "val" to "val".
      VEC(REAL,:), INOUT :: self
      REAL, IN :: val
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         if (self(i)>val) self(i) = val
      end do
   end subroutine

   pure subroutine chop_small_absolute_values(self,val)
   ! Set all values in the self whose absolute value is smaller than "val" to
   ! "val" times the sign of the original value.
      VEC(REAL,:), INOUT :: self
      REAL, IN :: val
      INT :: i
      REAL :: bb,ba

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         bb = self(i)
         ba = abs(bb)
         if (ba>=val) cycle
         self(i) = sign(ONE,bb)*val
      end do
   end subroutine

   pure subroutine chop_large_absolute_values(self,val)
   ! Set all values in the self whose absolute value is larger than "val" to
   ! "val" times the sign of the original value.
      VEC(REAL,:), INOUT :: self
      REAL, IN :: val
      INT :: dim,i
      REAL :: bb,ba,sign

      ! The following code is inherited from VEC{INTRINSIC}
      dim = size(self)
      do i = 1,dim
         bb = self(i)
         if (bb==ZERO) cycle
         ba = abs(bb)
         sign = bb/ba
         self(i) = sign*min(val,ba)
      end do
   end subroutine

   pure subroutine zero_small_values(self,eps)
   ! Zero elements of the vector which are less than "tol" in magnitude
      VEC(REAL,:), INOUT :: self
      REAL, optional, IN :: eps
      REAL :: tol

      ! The following code is inherited from VEC{INTRINSIC}
      tol = REAL_EPSILON
      if (present(eps)) tol = eps
      where (abs(self)<tol)
         self = ZERO
      end where
   end subroutine

   pure function largest_value(self) result (res)
   ! Return the maximum absolute value in the vector
      VEC(REAL,:), IN :: self
      REAL :: res
      REAL :: val
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = abs(self(1))
      do i = 2,size(self)
         val = abs(self(i))
         if (val>res) res = val
      end do
   end function

   pure function smallest_value(self) result (res)
   ! Return minimum absolute value in the vector
      VEC(REAL,:), IN :: self
      REAL :: res
      REAL :: val
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = abs(self(1))
      do i = 2,size(self)
         val = abs(self(i))
         if (val<res) res = val
      end do
   end function

   pure function no_of_vals_larger_than(self,tol) result (res)
   ! Return the number of elements larger than "tol".
      VEC(REAL,:), IN :: self
      REAL, IN :: tol
      INT :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = count(self>tol)
   end function

   pure function no_of_abs_vals_larger_than(self,tol) result (res)
   ! Return the number of absolute-value-elements larger than "tol".
      VEC(REAL,:), IN :: self
      REAL, IN :: tol
      INT :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = count(abs(self)>tol)
   end function


!  ================
!  Range operations
!  ================

   pure function number_in_range(self,range) result (res)
   ! Return the number of elements in self within the specified "range".
      VEC(REAL,:), IN :: self
      VEC(REAL,2), IN :: range
      INT :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = count(range(1)<=self AND self<=range(2))
   end function

   pure function indices_of_vals_larger_than(self,tol) result (ind)
   ! Indices "ind" of elements larger than "tol".
      VEC(REAL,:), IN :: self
      REAL, IN :: tol
      VEC(INT,no_of_vals_larger_than_(self,tol)) :: ind
      INT :: i,j

      ! The following code is inherited from VEC{INTRINSIC}
      j = 0
      do i = 1,size(self)
         if (self(i)>tol) then
            j = j+1
            ind(j) = i
         end if
      end do
   end function

   pure function indices_of_abs_vals_larger_than(self,tol) result (ind)
   ! Indices "ind" of absolute-value-elements larger than "tol".
      VEC(REAL,:), IN :: self
      REAL, IN :: tol
      VEC(INT,no_of_abs_vals_larger_than_(self,tol)) :: ind
      INT :: i,j

      ! The following code is inherited from VEC{INTRINSIC}
      j = 0
      do i = 1,size(self)
         if (abs(self(i))>tol) then
            j = j+1
            ind(j) = i
         end if
      end do
   end function

   pure subroutine get_indices_in_range(self,range,row,val)
   ! Get the "row" and "col" indices, and values "val", of the
   ! elements of "self" which are within the specified "range".
      VEC(REAL,:), IN :: self
      VEC(REAL,2), IN :: range
      VEC(INT,:), OUT :: row
      VEC(REAL,:), OUT :: val

   ! ENSURE(row.dim==.number_in_range(range),"wrong size, row")
   ! ENSURE(val.dim==.number_in_range(range),"wrong size, val")
      INT :: i,r

      ! The following code is inherited from VEC{INTRINSIC}
      i = 0
      do r = 1,size(self)
         if (self(r)<range(1) OR self(r)>range(2)) cycle
         i = i+1
         row(i) = r
         val(i) = self(r)
      end do
   end subroutine

   PURE function indices_in_range(self,range) result (row)
   ! Get the "row" and "col" indices, and values "val", of the
   ! elements of "self" which are within the specified "range".
      VEC(REAL,:), IN :: self
      VEC(REAL,2), IN :: range
      VEC(INT,:), allocatable :: row
      INT :: i,r,n

      ! The following code is inherited from VEC{INTRINSIC}
      n = number_in_range_(self,range)
      call create_(row,n)
      i = 0
      do r = 1,size(self)
         if (self(r)<range(1) OR self(r)>range(2)) cycle
         i = i+1
         row(i) = r
      end do
   end function

   pure function all_in_range(self,range) result (res)
   ! Return TRUE if all values of self are within the specified "range".
      VEC(REAL,:), IN :: self
      VEC(REAL,2), IN :: range
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = all(range(1)<=self AND self<=range(2))
   end function

   pure function in_range(self,range) result (res)
   ! Return element i as TRUE if self(i) is within the specified "range".
      VEC(REAL,:), IN :: self
      VEC(REAL,2), IN :: range
      VEC(BIN,size(self)) :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = (range(1)<=self AND self<=range(2))
   end function

   pure function is_in_range(self,low,high) result (res)
   ! Return TRUE if the "self" vector is in the box bounded by lower
   ! corner "low", and upper corner "high".
      VEC(REAL,:), IN :: self
      VEC(REAL,size(self)), IN :: low
      VEC(REAL,size(self)), IN :: high
      BIN :: res

   ! ENSURE(all(low<=high),"box limits are not increasing")

      ! The following code is inherited from VEC{INTRINSIC}
      res = (all(low<=self) AND all(self<=high))
   end function

   pure function element_range(self) result (res)
   ! Return the range (smallest and largest value) of self.
      VEC(REAL,:), IN :: self
      VEC(REAL,2) :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res(1) = minval(self)
      res(2) = maxval(self)
   end function


!  ==========================
!  Index returning operations
!  ==========================

   pure function index_of_maximum(self) result (ind)
   ! Return the index of the maximum in the vector
      VEC(REAL,:), IN :: self
      INT :: ind

      ! The following code is inherited from VEC{INTRINSIC}
      ind = maxval(maxloc(self))
   end function

   pure function index_of_minimum(self) result (ind)
   ! Return the index of the minimum in the vector
      VEC(REAL,:), IN :: self
      INT :: ind

      ! The following code is inherited from VEC{INTRINSIC}
      ind = minval(minloc(self))
   end function

   pure function index_of_largest_value(self) result (ind)
   ! Return the index "ind" of the largest absolute value in the vector
      VEC(REAL,:), IN :: self
      INT :: ind
      INT :: i
      REAL :: maxv,val

      ! The following code is inherited from VEC{INTRINSIC}
      maxv = abs(self(1))
      ind = 1
      do i = 2,size(self)
         val = abs(self(i))
         if (val>maxv) then
            maxv = val
            ind = i
         end if
      end do
   end function

   pure function index_of_smallest_value(self) result (ind)
   ! Return the index "ind" of the smallest value in the vector
      VEC(REAL,:), IN :: self
      INT :: ind
      INT :: i
      REAL :: minv,val

      ! The following code is inherited from VEC{INTRINSIC}
      minv = abs(self(1))
      ind = 1
      do i = 2,size(self)
         val = abs(self(i))
         if (val<minv) then
            minv = val
            ind = i
         end if
      end do
   end function

   pure function index_of_first_nonzero_value(self,eps) result (res)
   ! Returns the index of the first nonzero component of self within
   ! a tolerance "eps".
      VEC(REAL,:), IN :: self
      REAL, optional, IN :: eps
      INT :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         if (same_as_(self(i),ZERO,eps)) cycle
         res = i
         exit
      end do
   end function

   pure function index_of_first_zero_value(self,eps) result (res)
   ! Returns the index of the first zero component of self.
      VEC(REAL,:), IN :: self
      REAL, optional, IN :: eps
      INT :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         if (NOT same_as_(self(i),ZERO,eps)) cycle
         res = i
         exit
      end do
   end function

   pure function index_of_first_greater_than(self,val) result (res)
   ! Returns the index of the first element of self greater than "val".
   ! Returns 0 if no index is greater.
      VEC(REAL,:), IN :: self
      REAL, IN :: val
      INT :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         if (self(i)<=val) cycle
         res = i
         exit
      end do
   end function

   pure function index_of_value(self,val,eps,last) result (pos)
   ! Returns the first index in "self" which has the value "val", or 0
   ! if "val" is not present in the array. If present, "eps" is used
   ! to test equality with "val". If "last" is present, then the
   ! search is performed in reverse and the last match is returned.
      VEC(REAL,:), IN :: self
      REAL, IN :: val
      REAL, optional, IN :: eps
      BIN, optional, IN :: last
      INT :: pos
      INT :: i
      BIN :: reverse
      reverse = FALSE
      if (present(last)) reverse = last
      pos = 0
      if (NOT reverse) then
         do i = 1,size(self)
            if (same_as_(self(i),val,eps)) then
               pos = i
               exit
            end if
         end do
      else
         do i = size(self),1,-1
            if (same_as_(self(i),val,eps)) then
               pos = i
               exit
            end if
         end do
      end if
   end function


!  ======================
!  Binning and histograms
!  ======================

   PURE subroutine to_histogram_from_data(self,data,X,X_min,X_max,X_bin,C)
   ! Set "self" to a histogram matrix whose i-th element contains the
   ! *sum* of the values in array "data" whose elements correspond to
   ! the i-th bin of "X", where a bin is a range of descriptor values
   ! in "X" of side length "X_bin" beginning at "X_min" and ending at
   ! "X_max". The dimension of self is calculated within this routine. 
   ! NOTE: Don't use binned data to calculate properties unless you
   ! really need to coarse grain.
      VEC(REAL,:), INOUT :: self
      VEC(REAL,:), IN :: data,X
      REAL, IN :: X_min,X_max,X_bin
      VEC(INT,:), optional, OUT :: C
      INT :: n_X,i,k
      REAL :: X_ran
   ENSURE(size(self)==ceiling((1-TOL(4))*(X_max-X_min)/X_bin),"VEC{REAL}:to_histogram_from_data ... wrong size, self")
   ENSURE(size(data)==size(X),"VEC{REAL}:to_histogram_from_data ... incompatible data and X descriptor")
   ENSURE(X_max>X_min,"VEC{REAL}:to_histogram_from_data ... X_max is smaller than X_min!")
   ENSURE(X_bin<(X_max-X_min),"VEC{REAL}:to_histogram_from_data ... X_bin size is larger than [X_min,X_max]")

      ! The range of value ...
      X_ran = X_max-X_min

      ! The number of bins ...
      n_X = ceiling((1-TOL(4))*X_ran/X_bin)

      ! Create the histogram (leaky)
      self = ZERO
      if (present(C)) C = 0

      ! Now do the binning ...
      do k = 1,size(data)

         ! Get the bin
         i = ceiling(min(X(k)-X_min,X_ran)/X_bin)
         i = max(i,1)
         i = min(i,n_X)

         ! Accumulate
         self(i) = self(i)+data(k)
         if (present(C)) C(i) = C(i)+1
      end do
   end subroutine


!  =========================
!  Find and match operations
!  =========================

   subroutine find_opposite_pairs(self,pair,min,max)
   ! "pair(i)" returns the location of the element which has the opposite
   ! value to self(i) *and* is also negative, i.e. self(pair(i)) = -self(i) < 0.
   ! If no such match can be found, pair(i) is set to 0.
   ! unless self(i) is greater than max, in which case pair(i) is set to -1.
      VEC(REAL,:), IN :: self
      VEC(INT,:), OUT :: pair
      REAL, IN :: min,max
      INT :: i,j
      pair = 0
      do i = 1,size(self)
         if (abs(self(i))<min) then
            pair(i) = 0
         else if (abs(self(i))>max) then
            if (self(i)<0) pair(i) = -1
            if (self(i)>0) pair(i) = -2
         else
            do j = 1,i-1
               if (any(pair==j)) cycle
               if (abs(self(j)+self(i))<min) then
                  if (self(i)>0) then; pair(i) = j
                  else; pair(i) = 0
                  end if
               end if
            end do
         end if
      end do
   end subroutine

   subroutine find_pairs(self,pair,match_function,tol)
   ! Find pairs of values self(i) and self(pair(i)) where:
   ! match_function(self(i),self(pair(i))) = 0. If no such pair can be
   ! found, then pair(i) is set to 0. If more than one match is found
   ! then the first match from the start of the list is the one that
   ! is paired.
      VEC(REAL,:), IN :: self
      VEC(INT,:), OUT :: pair
      interface
         function match_function(arg1,arg2) result (res)
            REAL, IN :: arg1,arg2
            REAL :: res
         end function
      end interface
      REAL, optional, IN :: tol
      INT :: i,j
      REAL :: val
      BIN :: paired
      pair = 0

      ! Loop over elements
      do i = 1,size(self)

         ! Skip if used
         if (pair(i)>0) cycle
         paired = FALSE

         ! Loop over potential matches
         do j = 1,size(self)

            ! Skip if used
            if (pair(j)>0) cycle
            if (any(pair==j)) cycle
            val = match_function(self(i),self(j))

            ! Does it match?
            if (is_zero_(val,tol)) then
               pair(i) = j
               pair(j) = i
               paired = TRUE  ! Found
               exit
            end if
         end do

         ! If NOT found, no pair, but used
         if (NOT paired) then
            pair(i) = i
         end if
      end do
   end subroutine

   subroutine find_non_unit_pairs(self,pair,match_function,tol)
   ! Find pairs of values self(i) and self(pair(i)) where:
   ! match_function(self(i),self(pair(i))) = 0. If no such pair can be
   ! found, then pair(i) is set to 0. If more than one match is found
   ! then the first match from the start of the list is the one that
   ! is paired.
      VEC(REAL,:), IN :: self
      VEC(INT,:), OUT :: pair
      interface
         function match_function(arg1,arg2) result (res)
            REAL, IN :: arg1,arg2
            REAL :: res
         end function
      end interface
      REAL, optional, IN :: tol
      INT :: i,j
      REAL :: val
      BIN :: paired
      pair = 0

      ! Loop over elements
      do i = 1,size(self)

         ! Skip if used
         if (pair(i)>0) cycle
         paired = FALSE

         ! Loop over potential matches
         do j = 1,size(self)

            ! Skip if used
            if (pair(j)>0) cycle
            if (any(pair==j)) cycle
            if (equals_(self(j),ONE,tol)) cycle
            if (equals_(self(j),-ONE,tol)) cycle
            val = match_function(self(i),self(j))

            ! Does it match?
            if (is_zero_(val,tol)) then
               pair(i) = j
               pair(j) = i
               paired = TRUE  ! Found
               exit
            end if
         end do

         ! If NOT found, no pair, but used
         if (NOT paired) then
            pair(i) = i
         end if
      end do
   end subroutine


!  =============
!  String widths
!  =============

   pure function str_lengths_0(self) result (res)
   ! Returns the minimal string lengths.
      VEC(REAL,:), IN :: self
      VEC(INT,size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = str_length_(self(i))
      end do
   end function

   pure function str_lengths_1(self,max_dp) result (res)
   ! Return the string lengths "res" from the int part of the REAL value
   ! "self", keeping "max_dp" decimal places.
      VEC(REAL,:), IN :: self
      INT, IN :: max_dp
      VEC(INT,size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = str_length_(self(i),max_dp)
      end do
   end function

   function str_lengths_2(self,error,max_dp) result (res)
   ! Returns the minimal string lengths for "self" assuming it has
   ! an associated "error", with certain precision "dp" and optional
   ! extra "spaces".  NOTE: This version does not add alignment spaces.
      VEC(REAL,:), IN :: self
      VEC(REAL,:), IN :: error
      INT, IN :: max_dp
      VEC(INT,size(self)) :: res
      INT :: i,dp

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         call get_str_length_noalign_(self(i),error(i),res(i),dp,max_dp)
      end do
   end function

   pure subroutine get_max_str_length_0(self,msl,spaces)
   ! Returns "msl", the maximum of the string lengths for "self",
   ! and with optional extra "spaces". 
      VEC(REAL,:), IN :: self
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

   pure subroutine get_max_str_length_1(self,msl,max_dp,spaces)
   ! Returns "msl", the maximum of the string lengths for "self", with
   ! certain precision "max_dp", and with optional extra "spaces". 
      VEC(REAL,:), IN :: self
      INT, OUT :: msl
      INT, IN :: max_dp
      INT, IN :: spaces
      INT :: i,sl

      ! The following code is inherited from VEC{INTRINSIC}
      msl = 0
      do i = 1,size(self)
         call get_str_length_maxdp_(self(i),sl,max_dp,spaces)
         msl = max(msl,sl)
      end do
   end subroutine

   PURE subroutine get_max_str_length_2(self,msl,error,max_dp)
   ! Returns "msl", the maximum of the string lengths for "self",
   ! assuming it has an associated "error", with precision "max_dp".
      VEC(REAL,:), IN :: self
      INT, OUT :: msl
      VEC(REAL,:), IN :: error
      INT, IN :: max_dp
      INT :: i,sl,dp,de,le,cb

      ! The following code is inherited from VEC{INTRINSIC}
      msl = 0
      do i = 1,size(self)
         call get_str_length_align_(self(i),error(i),sl,dp,de,le,cb,max_dp)
         msl = max(msl,sl)
      end do
   end subroutine

   PURE subroutine get_max_str_length_3(self,msl,mdp,mcb,error,max_dp,max_cb,two_digit_error)
   ! Returns "msl", the maximum of the string lengths for "self", and
   ! the maximum decimal point "mdp", and the maximum closing bracket
   ! position "mcb" assuming "self" has an associated "error"s.
   ! If "max_cb" is present, use it to pad "msl".
      VEC(REAL,:), IN :: self
      INT, OUT :: msl
      INT, OUT :: mdp
      INT, OUT :: mcb
      VEC(REAL,:), IN :: error
      INT, IN :: max_dp
      INT, optional, IN :: max_cb
      BIN, optional, IN :: two_digit_error
      INT :: i,sl,dp,de,le,cb

      ! The following code is inherited from VEC{INTRINSIC}
      msl = 0
      mdp = 0
      mcb = 0
      do i = 1,size(self)
         call get_str_length_align_(self(i),error(i),sl,dp,de,le,cb,max_dp,max_cb,two_digit_error)
         msl = max(msl,sl)
         mdp = max(mdp,dp)
         mcb = max(mcb,cb)
      end do
   end subroutine


!  =====================
!  Conversion to strings
!  =====================

   pure function to_str_vec_real_0(self) result (res)
   ! Change self to a string of minimal length
      VEC(REAL,:), IN :: self
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = to_str_(self(i))
      end do
   end function

   pure function to_str_vec_real_1(self,fmt,left_justify) result (res)
   ! Change self to a "string" using the specified fortran "fmt".
      VEC(REAL,:), IN :: self
      STR(len=*), IN :: fmt
      BIN, optional, IN :: left_justify
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = to_str_(self(i),fmt,left_justify)
      end do
   end function

   pure function to_str_vec_real_2(self,style,width,precision,left_justify) result (res)
   ! Change self to a string with specified "style", "width" and
   ! "precision" as defined in the fortran standard 
      VEC(REAL,:), IN :: self
      STR(len=*), IN :: style
      INT, IN :: width
      INT, IN :: precision
      BIN, optional, IN :: left_justify
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = to_str_(self(i),style,width,precision,left_justify)
      end do
   end function

   pure function to_concatenated_str(self,fmt,separator) result (res)
   ! Change self to a string using "fmt" if specified, and with
   ! "separator" between each item, if specified.
      VEC(REAL,:), IN :: self
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

   pure function to_str_no_zeros(self) result (res)
   ! Change self to a "string" but remove trailing zeros
      VEC(REAL,:), IN :: self
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i
      do i = 1,size(self)
         res(i) = to_str_no_zeros_(self(i))
      end do
   end function


!  =====================
!  Conversion operations
!  =====================

   PURE subroutine convert_to(self,units)
   ! Convert the "self" in atomic units or generic units to a new
   ! number in "units".
      VEC(REAL,:), INOUT :: self
      STR(len=*), IN :: units
   ENSURE(is_known_unit_(units),"VEC{REAL}:convert_to ... unknown units, "//units)

      ! The following code is inherited from VEC{INTRINSIC}
      self = (self*conversion_factor_(units))
   end subroutine

   PURE subroutine convert_from(self,units)
   ! Convert the number "self" from "units" system to a new number
   ! in atomic units or generic units.
      VEC(REAL,:), INOUT :: self
      STR(len=*), IN :: units
   ENSURE(is_known_unit_(units),"VEC{REAL}:convert_from ... unknown units, "//units)

      ! The following code is inherited from VEC{INTRINSIC}
      self = (self/conversion_factor_(units))
   end subroutine


!  =======================
!  Spin-orbital operations
!  =======================

   PURE function a_block(self) result (res)
   ! return the alpha sector of the vector
      VEC(REAL,:), IN :: self
      VEC(REAL,size(self)/2) :: res
      INT :: n
   ENSURE(is_even_(size(self)),"VEC{REAL}:a_block ... self is not even-dimensioned")

      ! The following code is inherited from VEC{INTRINSIC}
      n = size(self)/2
      res = self(1:n)
   end function

   PURE function b_block(self) result (res)
   ! return the beta sector of the vector
      VEC(REAL,:), IN :: self
      VEC(REAL,size(self)/2) :: res
      INT :: n
   ENSURE(is_even_(size(self)),"VEC{REAL}:b_block ... self is not even-dimensioned")

      ! The following code is inherited from VEC{INTRINSIC}
      n = size(self)/2
      res = self(n+1:2*n)
   end function

   PURE subroutine set_a_block_to(self,X)
   ! Set the alpha sector of the vector
      VEC(REAL,:), INOUT :: self
      VEC(REAL,:), IN :: X
      INT :: n
   ENSURE(is_even_(size(self)),"VEC{REAL}:set_a_block_to ... self is not even-dimensioned")
   ENSURE(size(X)==size(self)/2,"VEC{REAL}:set_a_block_to ... wrong size, X")

      ! The following code is inherited from VEC{INTRINSIC}
      n = size(self)/2
      self(1:n) = X
   end subroutine

   PURE subroutine set_b_block_to(self,X)
   ! Set the beta sector of the vector
      VEC(REAL,:), INOUT :: self
      VEC(REAL,:), IN :: X
      INT :: n
   ENSURE(is_even_(size(self)),"VEC{REAL}:set_b_block_to ... self is not even-dimensioned")
   ENSURE(size(self)==2*size(X),"VEC{REAL}:set_b_block_to ... wrong size, X")

      ! The following code is inherited from VEC{INTRINSIC}
      n = size(self)/2
      self(n+1:2*n) = X
   end subroutine

   PURE subroutine put_a_block_to(self,X)
   ! Put the alpha sector to the vector "X"
      VEC(REAL,:), IN :: self
      VEC(REAL,:), OUT :: X
      INT :: n
   ENSURE(is_even_(size(self)),"VEC{REAL}:put_a_block_to ... self is not even-dimensioned")
   ENSURE(size(self)==2*size(X),"VEC{REAL}:put_a_block_to ... wrong size, X")

      ! The following code is inherited from VEC{INTRINSIC}
      n = size(self)/2
      X = self(1:n)
   end subroutine

   PURE subroutine put_b_block_to(self,X)
   ! Put the beta sector to the vector "X"
      VEC(REAL,:), IN :: self
      VEC(REAL,:), OUT :: X
      INT :: n
   ENSURE(is_even_(size(self)),"VEC{REAL}:put_b_block_to ... self is not even-dimensioned")
   ENSURE(size(self)==2*size(X),"VEC{REAL}:put_b_block_to ... wrong size, X")

      ! The following code is inherited from VEC{INTRINSIC}
      n = size(self)/2
      X = self(n+1:2*n)
   end subroutine

   PURE subroutine set_to_u_from_r(self,X)
   ! Set the alpha sector of the vector
      VEC(REAL,:), INOUT :: self
      VEC(REAL,:), IN :: X
      INT :: n
   ENSURE(is_even_(size(self)),"VEC{REAL}:set_to_u_from_r ... self is not even-dimensioned")
   ENSURE(size(self)==2*size(X),"VEC{REAL}:set_to_u_from_r ... wrong size, X")
      n = size(X)
      self(1:n) = X
      self(n+1:) = X
   end subroutine


!  =====================
!  Root finding routines
!  =====================

   subroutine bracket_root(self,z,direction,x1,x2,factor,val,max_it)
   ! Given a vector function self(z), an initial point "z" and a "direction",
   ! and initial distances "x1" and "x2" along "direction" from "z", bracket
   ! a root of self along "direction" by expansion. If "factor" is present it is
   ! used as the (linear) interval expansion factor. If "val" is present the
   ! values "x1" and "x2" will bracket the value x where self(x) = val.
   ! If "max_it" is present then it is the number of times the interval is
   ! expanded.
      interface
         function self(z) result (res)
            VEC(REAL,:), IN :: z
            REAL :: res
         end function
      end interface
      VEC(REAL,:) :: z,direction
      REAL :: x1,x2
      REAL, optional :: factor,val
      INT, optional :: max_it
      INT :: j
      REAL :: f1,f2
      REAL :: fac = 1.6d0
      REAL :: iso = 0.0d0
      INT :: maxit = 500
   ENSURE(x1/=x2,"VEC{REAL}:bracket_root ... non-zero range (x1,x2) required")
      if (present(factor)) fac = factor
      if (present(val)) iso = val
      if (present(max_it)) maxit = max_it
      if (x1>x2) call swap_with_(x1,x2)
      f1 = self(z+x1*direction)-iso
      f2 = self(z+x2*direction)-iso
      do j = 1,maxit
         if (f1*f2<ZERO) return
         if (abs(f1)<abs(f2)) then
            x1 = x1+fac*(x1-x2)
            f1 = self(z+x1*direction)-iso
         else
            x2 = x2+fac*(x2-x1)
            f2 = self(z+x2*direction)-iso
         end if
      end do
   end subroutine

   subroutine find_root_brent(self,z,direction,x1,x2,tol,root,val,max_it)
   ! Given a vector function self(x), an initial point "z", a "direction", and
   ! initial values "x1" and "x2" along this "direction" which bracket a root
   ! of self, return the "root" of self along "direction" to a precision "tol"
   ! using Brent's method. The point "z" is reset to be the vector point
   ! corresponding to this root. If "val" present, then root is set so that
   ! self(root) = val, i.e.  root is set top be an iso-value of self. If
   ! "max_it" is present it is set to the maximum number of iterations.
      interface
         function self(z) result (res)
            VEC(REAL,:), IN :: z
            REAL :: res
         end function
      end interface
      VEC(REAL,:) :: z,direction
      REAL :: x1,x2,tol
      REAL, optional :: root,val
      INT, optional :: max_it
      INT :: iter
      REAL :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
      INT :: maxit = 100
      REAL :: iso = ZERO
      REAL :: eps = TOL(8)
      if (present(max_it)) maxit = max_it
      if (present(val)) iso = val
      a = x1
      b = x2
      fa = self(z+a*direction)-iso
      fb = self(z+b*direction)-iso
      ENSURE((fa>0 AND fb<0) OR (fa<0 AND fb>0),"VEC{REAL}:find_root_brent ... root is not bracketed")
      c = b
      fc = fb
      d = ZERO
      e = ZERO
      do iter = 1,maxit
         if ((fb>0 AND fc>0) OR (fb<0 AND fc<0)) then
            c = a  ! Rename a,b,c and adjust bounding interval d
            fc = fa
            d = b-a
            e = d
         end if
         if (abs(fc)<abs(fb)) then
            a = b; b = c; c = a
            fa = fb; fb = fc; fc = fa
         end if
         ! Test convergence
         tol1 = TWO*eps*abs(b)+HALF*tol
         xm = HALF*(c-b)
         if (abs(xm)<=tol1 OR fb==ZERO) then
            if (present(root)) root = b
            z = z+b*direction
            return
         end if
         if (abs(e)>=tol1 AND abs(fa)>abs(fb)) then
            s = fb/fa  ! Attempt inverse quadratic interpolation
            if (a==c) then
               p = TWO*xm*s
               q = ONE-s
            else
               q = fa/fc
               r = fb/fc
               p = s*(TWO*xm*q*(q-r)-(b-a)*(r-ONE))
               q = (q-ONE)*(r-ONE)*(s-ONE)
            end if
            if (p>ZERO) q = -q
            p = abs(p)
            if (TWO*p<min(THREE*xm*q-abs(tol1*q),abs(e*q))) then
               e = d  ! Accept interpolation
               d = p/q
            else
               d = xm  ! Interpolation failed, use bisection
               e = d
            end if
         else
            d = xm  ! Bounds decreasing too slowly, use bisection
            e = d
         end if
         a = b; fa = fb
         if (abs(d)>tol1) then; b = b+d
         else; b = b+sign(tol1,xm)
         end if
         fb = self(z+b*direction)-iso
      end do
      DIE("VEC{REAL}:find_root_brent ... maximum iterations exceeded")
   end subroutine

   subroutine find_isopoint(self,z,direction,isovalue,tol,x1,x2)
   ! Given an initial point "z" and a "direction" to search along, return the
   ! isopoint "z" where the function "self" has value "isovalue", i.e. where
   ! self(p) = isovalue. "tol" is the accuracy to which "z" is determined.
   ! If present, "x1" and "x2" are an initial interval along "direction" from
   ! "p" where the isopoint is suspected to lie.
      VEC(REAL,:) :: z,direction
      interface
         function self(z) result (res)
            VEC(REAL,:), IN :: z
            REAL :: res
         end function
      end interface
      REAL :: isovalue,tol
      REAL, optional :: x1,x2
      REAL :: y1,y2
      y1 = ZERO; if (present(x1)) y1 = x1
      y2 = ONE; if (present(x2)) y2 = x2
      call bracket_root_(self,z,direction,y1,y2,val=isovalue)
      call find_root_brent_(self,z,direction,y1,y2,tol,val=isovalue)
   end subroutine


!  =====================
!  Minimisation routines
!  =====================

   subroutine minimise(self,new_direction,gradient,old_val,old_gradient,hessian)
   ! Move the vector "self" to a position closer to the minimum, based on the
   ! gradient.  Uses the BFGS scheme.  Doesn't check for convergence, just does
   ! one iteration.
      VEC(REAL,:), IN :: self
      VEC(REAL,:), OUT :: new_direction
      VEC(REAL,:), IN :: gradient
      VEC(REAL,:), INOUT :: old_val,old_gradient
      MAT(REAL,:,:), INOUT :: hessian
      VEC(REAL,:), allocatable :: dx,dg,hdg,u
      REAL :: dxdg,dghdg,dghdg_inv
      INT :: dim
   ENSURE(size(self)==size(new_direction),"VEC{REAL}:minimise ... vector size mismatch")
   ENSURE(size(self)==size(gradient),"VEC{REAL}:minimise ... vector size mismatch")
   ENSURE(size(self)==size(old_val),"VEC{REAL}:minimise ... vector size mismatch")
   ENSURE(size(self)==size(old_gradient),"VEC{REAL}:minimise ... vector size mismatch")
   ENSURE(size(self)==size(hessian,1),"VEC{REAL}:minimise ... incorrect dimension for hessian matrix")
   ENSURE(size(self)==size(hessian,2),"VEC{REAL}:minimise ... incorrect dimension for hessian matrix")
      dim = size(self)
      call create_(dx,dim); call create_(dg,dim); call create_(hdg,dim); call create_(u,dim)
      dg = gradient-old_gradient
      dx = self-old_val
      call to_product_of_(hdg,hessian,dg)
      dxdg = ONE/dot_product(dx,dg)
      dghdg = dot_product(dg,hdg)
      dghdg_inv = ONE/dghdg
      u = dxdg*dx-dghdg_inv*hdg
      hessian = hessian+dxdg*outer_product_with_(dx,dx)-dghdg_inv*outer_product_with_(hdg,hdg)+dghdg*outer_product_with_(u,u)
      call to_product_of_(dx,hessian,gradient)
      old_val = self
      old_gradient = gradient
      new_direction = -dx
      call destroy_(dx); call destroy_(dg); call destroy_(hdg); call destroy_(u)
   end subroutine

   subroutine minimize_BFGS(self,dself,p,fret,tol,gtol,step,max_it)
   ! Use the Broyden-Fletcher-Goldfarb-Shanno method to minimise a
   ! vector function self(p) with derivative function dself(p)
   ! starting from an initial point "p", returning the minimum point
   ! in "p", to a tolerance "gtol" in the gradient. If "step" is
   ! present it determines the size of the initial step in the line
   ! minimisation along the gradient direction. If "max_it" is
   ! present, set that to be the maximum iterations
      interface
         function self(p) result (res)
            VEC(REAL,:), IN :: p
            REAL :: res
         end function
      end interface
      interface
         function dself(p) result (res)
            VEC(REAL,:), IN :: p
            VEC(REAL,size(p)) :: res
         end function
      end interface
      VEC(REAL,:) :: p
      REAL :: fret
      REAL, IN :: tol,gtol
      REAL, optional :: step
      INT, optional, IN :: max_it
      INT :: n,iter,i,itmax
      REAL :: fac,fad,fae,fp,sumdg,sumxi,eps,stp,this_stp
      VEC(REAL,:), allocatable :: g,dg,hdg,pnew,xi
      MAT(REAL,:,:), PTR :: hessian
      BIN :: fail
      eps = tol/FOUR
      stp = ONE
      if (present(step)) stp = step
      itmax = 200
      if (present(max_it)) itmax = max_it
      n = size(p)
      call create_(g,n)
      call create_(dg,n)
      call create_(hdg,n)
      call create_(pnew,n)
      call create_(xi,n)
      allocate(hessian(n,n))
      fp = self(p)
      g = dself(p)
      hessian = ZERO
      do i = 1,n
         hessian(i,i) = ONE
      end do
      xi = -g
      iter = 0
      fail = FALSE
      do
         iter = iter+1
         pnew = p
         xi = (stp/norm_(xi))*xi  ! This is new
         call line_minimize_from_(self,pnew,xi,fret,tol,fp,this_stp)
         stp = stp*sqrt(abs(this_stp))
         fp = fret
         xi = pnew-p
         p = pnew
         if (largest_value_(xi)<tol) exit
         dg = g
         g = dself(p)
         if (largest_value_(g)<gtol) exit
         dg = g-dg
         call to_product_of_(hdg,hessian,dg)
         fac = dot_(dg,xi)
         fae = dot_(dg,hdg)
         sumdg = dot_(dg,dg)
         sumxi = dot_(xi,xi)
         if (fac**2>eps*sumdg*sumxi) then
            fac = ONE/fac
            fad = ONE/fae
            dg = fac*xi-fad*hdg
            hessian = hessian+fac*outer_product_with_(xi,xi)-fad*outer_product_with_(hdg,hdg)+fae*outer_product_with_(dg,dg)
            call minus_product_of_(xi,hessian,g)
         end if
         if (iter>itmax) then
            fail = TRUE
            exit
         end if
      end do
      DIE_IF(fail,"VEC{REAL}:minimize_BFGS ... exceeded allowed iterations")
      deallocate(hessian)
      call destroy_(xi)
      call destroy_(pnew)
      call destroy_(hdg)
      call destroy_(dg)
      call destroy_(g)
   end subroutine

   subroutine minimize_L_BFGS(self,dself,x,m,x_min,tol,max_it,it)
   ! Use the limited memory Broyden-Fletcher-Goldfarb-Shanno
   ! method to minimise a vector function self(x) with derivative
   ! function dself(x) starting from an initial point "x",
   ! returning the minimum point in "x_min", to a tolerance "tol"
   ! if present, otherwise it is set to 10^-3. A total of "m" vectors
   ! can be stored at any one time. If "max_it" is present, set that
   ! to be the maximum iterations, otherwise it
   ! is 1000 and "it" represents the number of iterations required
   ! to reach the minima. This code is all per the algorithm from
   ! Nocedal & Wright
      INT, IN :: m
      VEC(REAL,:), IN :: x
   ! Interface for vector functions   
      interface
         subroutine self(x,res)
            VEC(REAL,:), IN :: x
            REAL, OUT :: res
         end subroutine
      end interface
      interface
         subroutine dself(x,res)
            VEC(REAL,:), IN :: x
            VEC(REAL,:), OUT :: res
         end subroutine
      end interface
      VEC(REAL,:), OUT :: x_min
      INT, OUT :: it
      REAL, optional, IN :: tol
      INT, optional, IN :: max_it
      INT :: k,j,n,iter

      ! mag represents the change in each iteration, alpha, c1 and c2 are
      ! used in the line search algorithm and beta, gamma and rho are
      ! used in the Hessian approximation
      REAL :: mag,c1,c2,alpha,beta,gamma,eps,rho

      ! a is used to store the various alpha values in the two loop
      ! recursion, p,q and r are used as generic vectors, and as part
      ! of the two loop recursion process
      VEC(REAL,:), allocatable :: a,p,q,r

      ! used to calculate the vectors stored in S and Y and the
      ! diagonal Hessian, H
      VEC(REAL,size(x)) :: x_0,x_1

      ! Represents the change in x vector, S, change in y
      ! vector, Y
      MAT(REAL,:,:), PTR :: S,Y
      n = size(x)
      call create_(p,n)
      call create_(q,n)
      call create_(r,n)
      call create_(a,m)
      allocate(S(n,m))
      allocate(Y(n,m))

      ! Sets the iteration and step limits, if there is none input by
      ! the user
      iter = 1000
      eps = 10d-4
      if (present(max_it)) iter = max_it
      if (present(tol)) eps = tol

      ! The values used for line search algorithms. These values are
      ! the ones which usually work.
      c1 = 10d-4
      c2 = 9d-1

      ! Zeroes the S and Y matrices which store the s and y vectors.
      S = ZERO
      Y = ZERO

      ! Ensures the do while loop is entered
      k = 0
      mag = ONE

      ! Initialises the value of q, for use in step descent
      ! calculation 
      call dself(x,q)

      ! Initialises the values used in the two-loop recursion.
      rho = 1
      a = ZERO
      gamma = 1

      ! Will cause the program to exit if a suitable step can not be
      ! determined 
      alpha = ZERO
      x_1 = x

      ! Performs the loop until convergence, or the maximum number of
      ! iterations are reached.
      do while ((mag>eps) AND (k<=iter))

         ! The two loop recursion process approximates the Hessian by
         ! using stored values of delta x and delta grad, as well as a
         ! pre-scaling, gamma, to allow for a stepping at alpha = 1, to
         ! be suitable 

         ! H^0 is approximated by I   
         do j = k,max(1,k-m),-1
            n = mod(j,m)
            if (n==0) then
               n = m
            end if
            rho = 1/(dot_product(Y(:,n),S(:,n)))
            a(n) = rho*dot_product(S(:,n),q)
            q = q-a(n)*Y(:,n)
         end do
         r = gamma*q
         do j = max(1,k-m),k
            n = mod(j,m)
            if (n==0) then
               n = m
            end if
            rho = 1/dot_product(Y(:,n),S(:,n))
            beta = rho*dot_product(Y(:,n),r)
            r = r+(a(n)-beta)*S(:,n)
         end do

         ! This is the step toward the minimum
         p = -r

         ! Fines the ideal multiple of the step to take (usually 1)
         ! and stores that in alpha
         call line_search(self,dself,0.0005d0,x_1,p,c1,c2,alpha)

         ! Performs the step (and stores the previous x value)
         x_0 = x_1
         x_1 = x_1+alpha*p

         ! Stores the vectors, s and y, in matrices S and Y. If there
         ! are already m vectors, then it overwrites the oldest
         ! vector.
         n = mod(k,m)+1
         S(:,n) = x_1-x_0
         call dself(x_1,q)
         call dself(x_0,p)
         Y(:,n) = q-p

         ! Calculates the pre-scaling for H
         gamma = dot_product(S(:,n),Y(:,n))/dot_product(Y(:,n),Y(:,n))

         ! Updates the iteration number and calculates the shift (to
         ! check for convergence)
         k = k+1
         mag = norm2(S(:,n))
      end do

      ! Puts results to outputs
      x_min = x_1
      it = k

      ! Cleanup
      deallocate(S,Y)
      call destroy_(p)
      call destroy_(q)
      call destroy_(r)
   end subroutine

   subroutine minimize_FRPR(self,dself,p,fret,tol,ftol,algorithm,step)
   ! Use the Fletcher-Reeves-Polak-Ribiere method to minimise a vector
   ! function self(p) with derivative function dself(p) starting from
   ! an initial point "p", returning the minimum point in "p", to a
   ! tolerance "tol" and the minimum function value in "fret" to a
   ! tolerance "ftol". "algorithm" (if present) can be set to
   ! "Polak-Ribiere" (default) or "Fletcher-Reeves".
      interface
         function self(p) result (res)
            VEC(REAL,:), IN :: p
            REAL :: res
         end function
      end interface
      interface
         function dself(p) result (res)
            VEC(REAL,:), IN :: p
            VEC(REAL,size(p)) :: res
         end function
      end interface
      VEC(REAL,:) :: p
      REAL :: fret,tol,ftol
      STR(len=*), optional :: algorithm
      REAL, optional :: step
      INT :: itmax = 200
      INT :: n,iter
      REAL :: stp,this_stp,dgg,fp,gam,gg,dtol
      VEC(REAL,:), allocatable :: g,h,xi
      STR(len=STR_SIZE) :: alg
      BIN :: fail
      alg = "Polak-Ribiere"
      if (present(algorithm)) alg = algorithm
      stp = ONE
      if (present(step)) stp = step
      n = size(p)
      call create_(g,n)
      call create_(h,n)
      call create_(xi,n)
      fp = self(p)
      fret = fp
      xi = dself(p)
      g = -xi
      h = g
      xi = h
      iter = 0
      fail = FALSE
      do
         iter = iter+1
         dtol = largest_value_(xi)
         if (dtol<tol) exit
         xi = (stp/norm_(xi))*xi  ! This is new
         call line_minimize_from_(self,p,xi,fret,tol,fp,this_stp)
         if (abs(fret-fp)<=ftol AND dtol<tol) exit
         fail = iter>=itmax
         if (fail) exit
         stp = stp*sqrt(abs(this_stp))
         fp = fret
         xi = dself(p)
         gg = dot_(g,g)
         if (gg==ZERO) exit
         dgg = ZERO
         select case (alg)
         case ("Polak-Ribiere  "); dgg = dot_(xi,xi)+dot_(g,xi)
         case ("Fletcher-Reeves"); dgg = dot_(xi,xi)
         case default; allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "Polak-Ribiere  "
         tonto%known_keywords(2) = "Fletcher-Reeves"
         call unknown_(tonto,alg,"VEC{REAL}:minimize_FRPR")
         deallocate(tonto%known_keywords)
         end select
         gam = dgg/gg
         g = -xi
         h = g+gam*h
         xi = h
      end do
      DIE_IF(fail,"VEC{REAL}:minimize_FRPR ... exceeded allowed iterations")
      call destroy_(xi)
      call destroy_(h)
      call destroy_(g)
   end subroutine

   subroutine minimize_powell(self,p,directions,fret,tol,ftol)
   ! Use Powell's method to minimise a vector function self(p) starting from
   ! an initial point "p" along the initial (columns of) "directions", returning
   ! the minimum point in "p", the minimum function value in "fret" to a
   ! tolerance "ftol", and "tol" in the vector coordinates "p".
      interface
         function self(p) result (res)
            VEC(REAL,:), IN :: p
            REAL :: res
         end function
      end interface
      VEC(REAL,:) :: p
      MAT(REAL,:,:) :: directions
      REAL :: fret,tol,ftol
      INT :: itmax = 200
      INT :: n,iter,i,ibig
      REAL :: del,fp,fptt,t
      VEC(REAL,:), allocatable :: pt,ptt,xit
      BIN :: fail
   ENSURE(size(p)==size(directions,1),"VEC{REAL}:minimize_powell ... incompatible initial data")
   ENSURE(size(p)==size(directions,2),"VEC{REAL}:minimize_powell ... incompatible initial data")
      n = size(p)
      call create_(pt,n)
      call create_(ptt,n)
      call create_(xit,n)
      fret = self(p)
      pt = p  ! Save initial point
      iter = 0
      fail = FALSE
      do  ! Iteration loop
         iter = iter+1
         fp = fret
         ibig = 0
         del = 0
         do i = 1,n  ! Loop over all directions in set
            xit = directions(:,i)  ! Get direction
            fptt = fret
            call line_minimize_from_(self,p,xit,fret,tol)  ! Minimize along direction
            if (abs(fptt-fret)>del) then  ! Save if largest decrease so far
               del = abs(fptt-fret)
               ibig = i
            end if
         end do
         if (TWO*abs(fp-fret)<=ftol*(abs(fp)+abs(fret))) exit
         fail = iter>=itmax
         if (fail) exit
         ptt = TWO*p-pt  ! Construct extrapolated point and the
         xit = p-pt  ! average direction moved; save old
         pt = p  ! starting point
         fptt = self(ptt)
         if (fptt>=fp) cycle
         t = TWO*(fp-TWO*fret+fptt)*(fp-fret-del)**2-del*(fp-fptt)**2
         if (t>=0) cycle
         call line_minimize_from_(self,p,xit,fret,tol)  ! Move to minimum of the new direction
         directions(:,ibig) = directions(:,n)
         directions(:,n) = xit
      end do
      DIE_IF(fail,"VEC{REAL}:minimize_powell ... exceeded allowed iterations")
      call destroy_(xit)
      call destroy_(ptt)
      call destroy_(pt)
   end subroutine

   subroutine line_minimize_from(self,p,direction,fret,tol,f0,del)
   ! Given a vector function self(x), minimise from point "p" along "direction".
   ! Return the minimum point in "p" and the minimum value "fret", with an
   ! accuracy "tol". If "f0" is present it is used as the value of the function
   ! "self" at p, f0 = self(p). If "del" is present it is set to the length
   ! along "direction" where p achieved its minimum, useful for monitoring the
   ! step size.
      interface
         function self(p) result (res)
            VEC(REAL,:), IN :: p
            REAL :: res
         end function
      end interface
      VEC(REAL,:) :: p,direction
      REAL :: fret,tol
      REAL, optional :: f0,del
      REAL :: a,x,b,fa,fx,fb,xmin
      a = ZERO
      x = ONE
      call bracket_minimum_(self,p,direction,a,x,b,fa,fx,fb,f0)
      call minimize_brent_(self,p,direction,a,x,b,xmin,fret,tol,fx)
      if (present(del)) del = xmin
   end subroutine

   subroutine bracket_minimum(self,p,direction,a,b,c,fa,fb,fc,fa0,fb0)
   ! Given a vector function self(p), an initial point "p" and a "direction",
   ! and initial distances "a" and "b" along "direction" from "p", search in
   ! the downhill direction and return new distances "a", "b" and "c" along
   ! "direction" from "p" that bracket a minimum of the function self, and
   ! return the values of the function "fa", "fb", and "fc" at these points.
   ! NOTE: "c" is not used initially.
   ! If present, "fa0" is the initial value of self at a, fa0 = self(a).
   ! If present, "fb0" is the initial value of self at b, fb0 = self(b).
      interface
         function self(p) result (res)
            VEC(REAL,:), IN :: p
            REAL :: res
         end function
      end interface
      VEC(REAL,:) :: p,direction
      REAL :: a,b,c,fa,fb,fc
      REAL, optional :: fa0,fb0
      REAL :: gold = 1.618034
      REAL :: glimit = 100
      REAL :: tiny = 1.0d-20
      REAL :: fu,q,r,u,ulim
      INT :: iter
      BIN :: fail
   ENSURE(size(p)==size(direction),"VEC{REAL}:bracket_minimum ... incompatible vectors")
      if (present(fa0)) then; fa = fa0
      else; fa = self(p+a*direction)
      end if
      if (present(fb0)) then; fb = fb0
      else; fb = self(p+b*direction)
      end if
      if (fb>fa) then
         call swap_with_(a,b)
         call swap_with_(fa,fb)
      end if
      c = b+gold*(b-a)
      fc = self(p+c*direction)
      fail = TRUE
      fu = ZERO
      do iter = 1,100
         if (fb<fc) then
            fail = FALSE
            exit  ! bracket found
         end if
         r = (b-a)*(fb-fc)  ! get u by parabolic extrapolation
         q = (b-c)*(fb-fa)
         u = b-((b-c)*q-(b-a)*r)/(TWO*sign(max(abs(q-r),tiny),q-r))
         ulim = b+glimit*(c-b)
         if ((b-u)*(u-c)>ZERO) then  ! Parabolic u lies between b and c
            fu = self(p+u*direction)
            if (fu<fc) then  ! got a minimum between b and c
               a = b; fa = fb
               b = u; fb = fu
               fail = FALSE
               exit
            else if (fu>fb) then  ! got a minimum between a and u
               c = u; fc = fu
               fail = FALSE
               exit
            end if
            u = c+gold*(c-b)  ! parabolic fit no use, so magnify
            fu = self(p+u*direction)
         else if ((c-u)*(u-ulim)>0) then  ! Fit is between c and its allowed limit
            fu = self(p+u*direction)
            if (fu<fc) then
               b = c; fb = fc
               c = u; fc = fu
               u = c+gold*(c-b)
               fu = self(p+u*direction)
            end if
         else if ((u-ulim)*(ulim-c)>0) then
            u = ulim
            fu = self(p+u*direction)
         else
            u = c+gold*(c-b)  ! magnify
            fu = self(p+u*direction)
         end if
         a = b; fa = fb
         b = c; fb = fc
         c = u; fc = fu
      end do
      WARN_IF(fail,"VEC{REAL}:bracket_minimum ... exceeded maximum iterations")
      if (a>c) then
         call swap_with_(a,c)
         call swap_with_(fa,fc)
      end if
   end subroutine

   subroutine minimize_brent(self,p,direction,a,b,c,xmin,f,tol,fb0)
   ! Given a vector function self(x), an initial point "p", a "direction", and
   ! initial distances "a", "b" and "c" along "direction" from "p" which
   ! bracket a minimum in function self, return the minimum point "p" and its
   ! distance "xmin" along the "direction" from "p", and also the value "f" at
   ! the minimum to a precision "tol", using Brent's search method.
   ! If present, "fb0" is the initial value of self at b, fb0 = self(b)
      interface
         function self(p) result (res)
            VEC(REAL,:), IN :: p
            REAL :: res
         end function
      end interface
      VEC(REAL,:) :: p,direction
      REAL :: a,b,c,xmin,f,tol
      REAL, IN, optional :: fb0
      INT :: itmax = 100
      REAL :: cgold = 0.3819660d0
      REAL :: zeps = TOL(10)
      REAL :: d,e,etemp,fu,fv,fw,fx,pp,qq,r,tol1,tol2,u,v,w,x,xm
      INT :: iter,iters
      BIN :: fail
   ENSURE(size(p)==size(direction),"VEC{REAL}:minimize_brent ... incompatible vectors")
      if (a>c) call swap_with_(a,c)
      v = b
      w = b
      x = b
      if (present(fb0)) then; fx = fb0
      else; fx = self(p+x*direction)
      end if
      fv = fx
      fw = fx
      d = ZERO
      e = ZERO
      fail = TRUE
      b = c
      iters = 0
      fu = ZERO
      do iter = 1,itmax
         xm = HALF*(a+b)
         tol1 = tol*abs(x)+zeps
         tol2 = TWO*tol1
         if (abs(x-xm)<(tol2-HALF*(b-a))) then
            fail = FALSE
            exit
         end if
         if (abs(e)>tol1) then
            r = (x-w)*(fx-fv)
            qq = (x-v)*(fx-fw)
            pp = (x-v)*qq-(x-w)*r
            qq = TWO*(qq-r)
            if (qq>ZERO) pp = -pp
            qq = abs(qq)
            etemp = e
            e = d
            if (abs(pp)>=abs(HALF*qq*etemp) OR pp<=qq*(a-x) OR pp>=qq*(b-x)) then
               if (x>=xm) then; e = a-x
               else; e = b-x
               end if
               d = cgold*e
            else
               d = pp/qq
               u = x+d
               if ((u-a)<tol2 OR (b-u)<tol2) d = sign(tol1,xm-x)
            end if
         else
            if (x>=xm) then; e = a-x
            else; e = b-x
            end if
            d = cgold*e
         end if
         if (abs(d)>=tol1) then; u = x+d
         else; u = x+sign(tol1,d)
         end if
         fu = self(p+u*direction)
         if (fu<=fx) then
            if (u>=x) then; a = x
            else; b = x
            end if
            v = w; fv = fw
            w = x; fw = fx
            x = u; fx = fu
         else
            if (u<x) then; a = u
            else; b = u
            end if
            if (fu<=fw OR w==x) then
               v = w; fv = fw
               w = u; fw = fu
            else if (fu<=fv OR v==x OR v==w) then
               v = u; fv = fu
            end if
         end if
         iters = iters+1
      end do
      DIE_IF(fail,"VEC{REAL}:minimize_brent ... maximum iterations exceeded")
      f = fx
      xmin = x
      p = p+x*direction
   end subroutine

   subroutine minimize_golden(self,p,direction,a,b,c,xmin,f,tol)
   ! Given a vector function self(p), an initial point "p" and a "direction",
   ! and initial distances "a", "b" and "c" along "direction" from "p" which
   ! bracket a minimum in function self, return the minimum point "p" and its
   ! distance "xmin" along the "direction" from "p", and also the value "f" at
   ! the minimum to a precision "tol", using the golden section search method.
      interface
         function self(p) result (res)
            VEC(REAL,:), IN :: p
            REAL :: res
         end function
      end interface
      VEC(REAL,:) :: p,direction
      REAL :: a,b,c,xmin,f,tol
      REAL :: r = 0.618033399
      REAL :: s,f1,f2,x0,x1,x2,x3
   ENSURE(size(p)==size(direction),"VEC{REAL}:minimize_golden ... incompatible vectors")
      s = ONE-r
      x0 = a
      x3 = c
      if (abs(c-b)>abs(b-a)) then
         x1 = b; x2 = b+s*(c-b)
      else
         x2 = b; x1 = b-s*(b-a)
      end if
      f1 = self(p+x1*direction)
      f2 = self(p+x2*direction)
      do
         if (abs(x3-x0)<=tol*(abs(x1)+abs(x2))) exit
         if (f2<f1) then
            x0 = x1
            x1 = x2
            x2 = r*x1+s*x3
            f1 = f2
            f2 = self(p+x2*direction)
         else
            x3 = x2
            x2 = x1
            x1 = r*x2+s*x0
            f2 = f1
            f1 = self(p+x1*direction)
         end if
      end do
      if (f1<f2) then; f = f1; xmin = x1; p = p+x1*direction
      else; f = f2; xmin = x2; p = p+x2*direction
      end if
   end subroutine


! These methods use a type-specific call-back

! DIFFRACTION_DATA

   PURE subroutine min_BFGS(arg,F,dF,p,Fret,tol,gtol,step,maxit)
   ! Use the Broyden-Fletcher-Goldfarb-Shanno method to minimise a
   ! vector function F(p) with derivative function dF(p) starting from
   ! an initial point "p", returning the minimum point in "p", to a
   ! tolerance "gtol" in the gradient. If "step" is present it
   ! determines the size of the initial step in the line minimisation
   ! along the gradient direction. If "maxit" is present, set that to
   ! be the maximum iterations
      type(DIFFRACTION_DATA_TYPE), IN :: arg
      interface
         PURE function F(arg,p) result (res)
         use TYPES_MODULE
            type(DIFFRACTION_DATA_TYPE), IN :: arg
            VEC(REAL,:), IN :: p
            REAL :: res
         end function
      end interface
      interface
         PURE function dF(arg,p) result (res)
         use TYPES_MODULE
            type(DIFFRACTION_DATA_TYPE), IN :: arg
            VEC(REAL,:), IN :: p
            VEC(REAL,size(p)) :: res
         end function
      end interface
      VEC(REAL,:), INOUT :: p
      REAL, OUT :: Fret
      REAL, IN :: tol
      REAL, IN :: gtol
      REAL, optional, IN :: step
      INT, optional, IN :: maxit
      INT :: n,iter,i,itmax
      REAL :: fac,fad,fae,fp
      REAL :: sumdg,sumxi,eps
      REAL :: stp,this_stp
      VEC(REAL,:), allocatable :: g,dg,hdg,pnew,xi
      MAT(REAL,:,:), allocatable :: hessian
      BIN :: fail
      eps = tol/FOUR
      stp = ONE
      if (present(step)) stp = step
      itmax = 200
      if (present(maxit)) itmax = maxit
      n = size(p)
      call create_(g,n)
      call create_(dg,n)
      call create_(hdg,n)
      call create_(pnew,n)
      call create_(xi,n)
      allocate(hessian(n,n))

      ! Eval;uate "F" & "dF"
      fp = F(arg,p)
      g = dF(arg,p)
      hessian = ZERO
      do i = 1,n
         hessian(i,i) = ONE
      end do
      xi = -g
      iter = 0
      fail = FALSE
      do
         iter = iter+1
         pnew = p

         ! Minimize F along direction "xi"
         xi = (stp/norm_(xi))*xi
         call min_line_(arg,F,pnew,xi,Fret,tol,fp,this_stp)

         ! Stepsize & stuff
         stp = stp*sqrt(abs(this_stp))
         fp = Fret
         xi = pnew-p
         p = pnew
         if (maxval(abs(xi))<tol) exit

         ! EVakuate derivative dF
         dg = g
         g = dF(arg,p)
         if (maxval(abs(g))<gtol) exit
         dg = g-dg
         hdg = matmul(hessian,dg)
         fac = dot_product(dg,xi)
         fae = dot_product(dg,hdg)
         sumdg = dot_product(dg,dg)
         sumxi = dot_product(xi,xi)
         if (fac**2>eps*sumdg*sumxi) then
            fac = ONE/fac
            fad = ONE/fae
            dg = fac*xi-fad*hdg
            hessian = hessian+fac*outer_product_with_(xi,xi)-fad*outer_product_with_(hdg,hdg)+fae*outer_product_with_(dg,dg)
            xi = xi-matmul(hessian,g)
         end if
         if (iter>itmax) then
            fail = TRUE
            exit
         end if
      end do

   ! ENSURE, not DIE: this routine is PURE, and in a release build PURE is a
   ! real "pure" while DIE is still live, so a DIE here would not compile.
      ENSURE(NOT fail,"VEC{REAL}:min_BFGS ... BFGS exceeded the allowed iterations")
      deallocate(hessian)
      call destroy_(xi)
      call destroy_(pnew)
      call destroy_(hdg)
      call destroy_(dg)
      call destroy_(g)
   end subroutine

   PURE subroutine min_line_0(arg,F,p,direction,Fret,tol,F0,del)
   ! Given vector function "F(arg,*)", minimise from point "p" along
   ! "direction", and return minimum point in "p" and minimum value in
   ! "Fret", to an accuracy "tol". 
   ! If present "F0" is the initial value "F" at p, F0 = F(arg,p). 
   ! If present, "del" is the length along "direction" where p 
   ! achieved its minimum - useful for monitoring the step size.
      type(DIFFRACTION_DATA_TYPE), IN :: arg
      interface
         PURE function F(arg,p) result (res)
         use TYPES_MODULE
            type(DIFFRACTION_DATA_TYPE), IN :: arg
            VEC(REAL,:), IN :: p
            REAL :: res
         end function
      end interface
      VEC(REAL,:), INOUT :: p
      VEC(REAL,:), IN :: direction
      REAL, INOUT :: Fret
      REAL, IN :: tol
      REAL, optional, IN :: F0
      REAL, optional, OUT :: del
      REAL :: a,x,b,Fa,Fx,Fb,xmin
      a = ZERO
      x = ONE
      call min_brack_(arg,F,p,direction,a,x,b,Fa,Fx,Fb,F0)
      call min_brent_(arg,F,p,direction,a,x,b,xmin,Fret,tol,Fx)
      if (present(del)) del = xmin
   end subroutine

   PURE subroutine min_brack_0(arg,F,p,direction,a,b,c,Fa,Fb,Fc,Fa0,Fb0)
   ! Given vector function "F(arg,*)", a point "p", a "direction", and
   ! initial distances "a" and "b" along "direction" from "p", search
   ! in the downhill direction and return new distances "a", "b" and
   ! "c" along "direction" from "p" that bracket a minimum of the
   ! function "F(arg,*)", and return the values of the function 
   ! "Fa", "Fb", and "Fc" at these points.
   ! NOTE: "c" is not used initially.
   ! If present, "Fa0" is the initial value Fa0 = F(arg,a).
   ! If present, "Fb0" is the initial value Fb0 = F(arg,b).
      type(DIFFRACTION_DATA_TYPE), IN :: arg
      interface
         PURE function F(arg,p) result (res)
         use TYPES_MODULE
            type(DIFFRACTION_DATA_TYPE), IN :: arg
            VEC(REAL,:), IN :: p
            REAL :: res
         end function
      end interface
      VEC(REAL,:), IN :: p
      VEC(REAL,:), IN :: direction
      REAL, INOUT :: a,b,c
      REAL, OUT :: Fa,Fb,Fc
      REAL, optional, IN :: Fa0,Fb0
      REAL :: gold,glimit,tiny
      REAL :: fu,q,r,u,ulim
      INT :: iter
      BIN :: fail
   ENSURE(size(p)==size(direction),"VEC{REAL}:min_brack_0 ... incompatible vectors")
      gold = 1.618034
      glimit = 100
      tiny = 1.0d-20
      if (present(Fa0)) then; Fa = Fa0
      else; Fa = F(arg,p+a*direction)
      end if
      if (present(Fb0)) then; Fb = Fb0
      else; Fb = F(arg,p+b*direction)
      end if
      if (Fb>Fa) then
         call swap_with_(a,b)
         call swap_with_(Fa,Fb)
      end if
      c = b+gold*(b-a)
      fc = F(arg,p+c*direction)
      fail = TRUE
      fu = ZERO
      do iter = 1,100

         ! Bracket found?
         if (Fb<Fc) then
            fail = FALSE
            exit
         end if

         ! Get u by parabolic extrapolation
         r = (b-a)*(Fb-Fc)
         q = (b-c)*(Fb-Fa)
         u = b-((b-c)*q-(b-a)*r)/(TWO*sign(max(abs(q-r),tiny),q-r))
         ulim = b+glimit*(c-b)
         if ((b-u)*(u-c)>ZERO) then

            ! Parabolic u lies between b and c
            Fu = F(arg,p+u*direction)
            if (Fu<fc) then

               ! Minimum between b and c
               a = b; Fa = Fb
               b = u; Fb = Fu
               fail = FALSE
               exit
            else if (Fu>Fb) then

               ! Minimum between a and u
               c = u; Fc = Fu
               fail = FALSE
               exit
            end if
            ! Parabolic fit no use, so magnify
            u = c+gold*(c-b)
            Fu = F(arg,p+u*direction)
         else if ((c-u)*(u-ulim)>0) then

            ! Parabolic u between c and its allowed limit
            Fu = F(arg,p+u*direction)
            if (Fu<Fc) then
               b = c; Fb = Fc
               c = u; Fc = Fu
               u = c+gold*(c-b)
               Fu = F(arg,p+u*direction)
            end if
         else if ((u-ulim)*(ulim-c)>0) then
            u = ulim
            Fu = F(arg,p+u*direction)
         else

            ! Magnify
            u = c+gold*(c-b)
            Fu = F(arg,p+u*direction)
         end if
         a = b; Fa = Fb
         b = c; Fb = Fc
         c = u; Fc = Fu
      end do

   ! DIE_IF(fail,"exceeded maximum iterations")
      if (a>c) then
         call swap_with_(a,c)
         call swap_with_(Fa,Fc)
      end if
   end subroutine

   PURE subroutine min_brent_0(arg,F,p,direction,a,b,c,xmin,Fmin,tol,Fb0)
   ! Given vector function "F(arg,*)", point "p", a "direction", and
   ! initial distances "a", "b" and "c" along "direction" from "p"
   ! which bracket a minimum in "F(arg,*)", return in "p" the point of
   ! the minimum value & distance "xmin" along "direction" from "p". 
   ! Also return "Fmin" at the minimum to a precision "tol", using
   ! Brent's search method.  
   ! If present, "Fb0" is initial value of "F" at b, Fb0 = F(arg,b)
      type(DIFFRACTION_DATA_TYPE), IN :: arg
      interface
         PURE function F(arg,p) result (res)
         use TYPES_MODULE
            type(DIFFRACTION_DATA_TYPE), IN :: arg
            VEC(REAL,:), IN :: p
            REAL :: res
         end function
      end interface
      VEC(REAL,:), INOUT :: p
      VEC(REAL,:), IN :: direction
      REAL, INOUT :: a,b,c
      REAL, OUT :: xmin
      REAL, OUT :: Fmin
      REAL, IN :: tol
      REAL, IN, optional :: Fb0
      INT :: itmax,iter
      REAL :: cgold,zeps
      REAL :: d,e,etemp
      REAL :: Fu,Fv,Fw,Fx
      REAL :: u,v,w,x
      REAL :: xm,pp,qq,r
      REAL :: tol1,tol2
      BIN :: fail
   ENSURE(size(p)==size(direction),"VEC{REAL}:min_brent_0 ... incompatible vectors")
      itmax = 100
      cgold = 0.3819660d0
      zeps = TOL(10)
      if (a>c) call swap_with_(a,c)
      v = b
      w = b
      x = b
      if (present(Fb0)) then; Fx = Fb0
      else; Fx = F(arg,p+x*direction)
      end if
      Fv = Fx
      Fw = Fx
      b = c
      d = ZERO
      e = ZERO
      fu = ZERO
      fail = TRUE
      do iter = 1,itmax
         xm = HALF*(a+b)
         tol1 = tol*abs(x)+zeps
         tol2 = TWO*tol1
         if (abs(x-xm)<(tol2-HALF*(b-a))) then
            fail = FALSE
            exit
         end if
         if (abs(e)>tol1) then
            r = (x-w)*(Fx-Fv)
            qq = (x-v)*(Fx-Fw)
            pp = (x-v)*qq-(x-w)*r
            qq = TWO*(qq-r)
            if (qq>ZERO) pp = -pp
            qq = abs(qq)
            etemp = e
            e = d
            if (abs(pp)>=abs(HALF*qq*etemp) OR pp<=qq*(a-x) OR pp>=qq*(b-x)) then
               if (x>=xm) then; e = a-x
               else; e = b-x
               end if
               d = cgold*e
            else
               d = pp/qq
               u = x+d
               if ((u-a)<tol2 OR (b-u)<tol2) d = sign(tol1,xm-x)
            end if
         else
            if (x>=xm) then; e = a-x
            else; e = b-x
            end if
            d = cgold*e
         end if
         if (abs(d)>=tol1) then; u = x+d
         else; u = x+sign(tol1,d)
         end if
         Fu = F(arg,p+u*direction)
         if (Fu<=Fx) then

            ! Smaller value found
            if (u>=x) then; a = x
            else; b = x
            end if
            v = w; Fv = Fw
            w = x; Fw = Fx
            x = u; Fx = Fu
         else

            ! Look at other values
            if (u<x) then; a = u
            else; b = u
            end if
            if (Fu<=Fw OR w==x) then
               v = w; Fv = Fw
               w = u; Fw = Fu
            else if (Fu<=Fv OR v==x OR v==w) then
               v = u; Fv = Fu
            end if
         end if
      end do

   ! DIE_IF(fail,"maximum iterations exceeded")
      Fmin = Fx
      xmin = x
      p = p+x*direction
   end subroutine


! MOLECULE

   PURE subroutine min_line_1(arg,F,p,direction,Fret,tol,F0,del)
   ! Given vector function "F(arg,*)", minimise from point "p" along
   ! "direction", and return minimum point in "p" and minimum value in
   ! "Fret", to an accuracy "tol". 
   ! If present "F0" is the initial value "F" at p, F0 = F(arg,p). 
   ! If present, "del" is the length along "direction" where p 
   ! achieved its minimum - useful for monitoring the step size.
      type(MOLECULE_TYPE), IN :: arg
      interface
         PURE function F(arg,p) result (res)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: arg
            VEC(REAL,3), IN :: p
            REAL :: res
         end function
      end interface
      VEC(REAL,3), INOUT :: p
      VEC(REAL,3), IN :: direction
      REAL, INOUT :: Fret
      REAL, IN :: tol
      REAL, optional, IN :: F0
      REAL, optional, OUT :: del
      REAL :: a,x,b,Fa,Fx,Fb,xmin
      a = ZERO
      x = ONE
      call min_brack_(arg,F,p,direction,a,x,b,Fa,Fx,Fb,F0)
      call min_brent_(arg,F,p,direction,a,x,b,xmin,Fret,tol,Fx)
      if (present(del)) del = xmin
   end subroutine

   PURE subroutine min_brack_1(arg,F,p,direction,a,b,c,Fa,Fb,Fc,Fa0,Fb0)
   ! Given vector function "F(arg,*)", a point "p", a "direction", and
   ! initial distances "a" and "b" along "direction" from "p", search
   ! in the downhill direction and return new distances "a", "b" and
   ! "c" along "direction" from "p" that bracket a minimum of the
   ! function "F(arg,*)", and return the values of the function 
   ! "Fa", "Fb", and "Fc" at these points.
   ! NOTE: "c" is not used initially.
   ! If present, "Fa0" is the initial value Fa0 = F(arg,a).
   ! If present, "Fb0" is the initial value Fb0 = F(arg,b).
      type(MOLECULE_TYPE), IN :: arg
      interface
         PURE function F(arg,p) result (res)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: arg
            VEC(REAL,3), IN :: p
            REAL :: res
         end function
      end interface
      VEC(REAL,3), IN :: p
      VEC(REAL,3), IN :: direction
      REAL, INOUT :: a,b,c
      REAL, OUT :: Fa,Fb,Fc
      REAL, optional, IN :: Fa0,Fb0
      REAL :: gold,glimit,tiny
      REAL :: fu,q,r,u,ulim
      INT :: iter
      BIN :: fail
   ENSURE(size(p)==size(direction),"VEC{REAL}:min_brack_1 ... incompatible vectors")
      gold = 1.618034
      glimit = 100
      tiny = 1.0d-20
      if (present(Fa0)) then; Fa = Fa0
      else; Fa = F(arg,p+a*direction)
      end if
      if (present(Fb0)) then; Fb = Fb0
      else; Fb = F(arg,p+b*direction)
      end if
      if (Fb>Fa) then
         call swap_with_(a,b)
         call swap_with_(Fa,Fb)
      end if
      c = b+gold*(b-a)
      fc = F(arg,p+c*direction)
      fail = TRUE
      fu = ZERO
      do iter = 1,100

         ! Bracket found?
         if (Fb<Fc) then
            fail = FALSE
            exit
         end if

         ! Get u by parabolic extrapolation
         r = (b-a)*(Fb-Fc)
         q = (b-c)*(Fb-Fa)
         u = b-((b-c)*q-(b-a)*r)/(TWO*sign(max(abs(q-r),tiny),q-r))
         ulim = b+glimit*(c-b)
         if ((b-u)*(u-c)>ZERO) then

            ! Parabolic u lies between b and c
            Fu = F(arg,p+u*direction)
            if (Fu<fc) then

               ! Minimum between b and c
               a = b; Fa = Fb
               b = u; Fb = Fu
               fail = FALSE
               exit
            else if (Fu>Fb) then

               ! Minimum between a and u
               c = u; Fc = Fu
               fail = FALSE
               exit
            end if
            ! Parabolic fit no use, so magnify
            u = c+gold*(c-b)
            Fu = F(arg,p+u*direction)
         else if ((c-u)*(u-ulim)>0) then

            ! Parabolic u between c and its allowed limit
            Fu = F(arg,p+u*direction)
            if (Fu<Fc) then
               b = c; Fb = Fc
               c = u; Fc = Fu
               u = c+gold*(c-b)
               Fu = F(arg,p+u*direction)
            end if
         else if ((u-ulim)*(ulim-c)>0) then
            u = ulim
            Fu = F(arg,p+u*direction)
         else

            ! Magnify
            u = c+gold*(c-b)
            Fu = F(arg,p+u*direction)
         end if
         a = b; Fa = Fb
         b = c; Fb = Fc
         c = u; Fc = Fu
      end do

   ! DIE_IF(fail,"exceeded maximum iterations")
      if (a>c) then
         call swap_with_(a,c)
         call swap_with_(Fa,Fc)
      end if
   end subroutine

   PURE subroutine min_brent_1(arg,F,p,direction,a,b,c,xmin,Fmin,tol,Fb0)
   ! Given vector function "F(arg,*)", point "p", a "direction", and
   ! initial distances "a", "b" and "c" along "direction" from "p"
   ! which bracket a minimum in "F(arg,*)", return in "p" the point of
   ! the minimum value & distance "xmin" along "direction" from "p". 
   ! Also return "Fmin" at the minimum to a precision "tol", using
   ! Brent's search method.  
   ! If present, "Fb0" is initial value of "F" at b, Fb0 = F(arg,b)
      type(MOLECULE_TYPE), IN :: arg
      interface
         PURE function F(arg,p) result (res)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: arg
            VEC(REAL,3), IN :: p
            REAL :: res
         end function
      end interface
      VEC(REAL,3), INOUT :: p
      VEC(REAL,3), IN :: direction
      REAL, INOUT :: a,b,c
      REAL, OUT :: xmin
      REAL, OUT :: Fmin
      REAL, IN :: tol
      REAL, IN, optional :: Fb0
      INT :: itmax,iter
      REAL :: cgold,zeps
      REAL :: d,e,etemp
      REAL :: Fu,Fv,Fw,Fx
      REAL :: u,v,w,x
      REAL :: xm,pp,qq,r
      REAL :: tol1,tol2
      BIN :: fail
   ENSURE(size(p)==size(direction),"VEC{REAL}:min_brent_1 ... incompatible vectors")
      itmax = 100
      cgold = 0.3819660d0
      zeps = TOL(10)
      if (a>c) call swap_with_(a,c)
      v = b
      w = b
      x = b
      if (present(Fb0)) then; Fx = Fb0
      else; Fx = F(arg,p+x*direction)
      end if
      Fv = Fx
      Fw = Fx
      b = c
      d = ZERO
      e = ZERO
      fu = ZERO
      fail = TRUE
      do iter = 1,itmax
         xm = HALF*(a+b)
         tol1 = tol*abs(x)+zeps
         tol2 = TWO*tol1
         if (abs(x-xm)<(tol2-HALF*(b-a))) then
            fail = FALSE
            exit
         end if
         if (abs(e)>tol1) then
            r = (x-w)*(Fx-Fv)
            qq = (x-v)*(Fx-Fw)
            pp = (x-v)*qq-(x-w)*r
            qq = TWO*(qq-r)
            if (qq>ZERO) pp = -pp
            qq = abs(qq)
            etemp = e
            e = d
            if (abs(pp)>=abs(HALF*qq*etemp) OR pp<=qq*(a-x) OR pp>=qq*(b-x)) then
               if (x>=xm) then; e = a-x
               else; e = b-x
               end if
               d = cgold*e
            else
               d = pp/qq
               u = x+d
               if ((u-a)<tol2 OR (b-u)<tol2) d = sign(tol1,xm-x)
            end if
         else
            if (x>=xm) then; e = a-x
            else; e = b-x
            end if
            d = cgold*e
         end if
         if (abs(d)>=tol1) then; u = x+d
         else; u = x+sign(tol1,d)
         end if
         Fu = F(arg,p+u*direction)
         if (Fu<=Fx) then

            ! Smaller value found
            if (u>=x) then; a = x
            else; b = x
            end if
            v = w; Fv = Fw
            w = x; Fw = Fx
            x = u; Fx = Fu
         else

            ! Look at other values
            if (u<x) then; a = u
            else; b = u
            end if
            if (Fu<=Fw OR w==x) then
               v = w; Fv = Fw
               w = u; Fw = Fu
            else if (Fu<=Fv OR v==x OR v==w) then
               v = u; Fv = Fu
            end if
         end if
      end do

   ! DIE_IF(fail,"maximum iterations exceeded")
      Fmin = Fx
      xmin = x
      p = p+x*direction
   end subroutine


!  ==================
!  Householder vector 
!  ==================

   PURE subroutine Householder_vector(self,nu,beta)
   ! Returns Householder vector from "self" such that :
   ! - nu(1) = 1 and 
   ! - beta, where P = I - beta*nu*nu^T is orthogonal, and
   ! - P*self = norm2(x)*e_1
   ! Golub & van Loan p. 210.
      VEC(REAL,:), IN :: self
      VEC(REAL,:), OUT :: nu
      REAL, OUT :: beta
      REAL :: sigma,mu
      REAL :: s1,s2,nu1,nu2
      INT :: dim
   ENSURE(size(nu)==size(self),"VEC{REAL}:Householder_vector ... incompatible vectors")
      dim = size(self)
      sigma = dot_product(self(2:),self(2:))
      nu(1) = ONE
      nu(2:) = self(2:)
      if (sigma==ZERO) then

         ! self already orthogonal,
         ! no calcs required.
         beta = ZERO
      else
         s1 = self(1)
         s2 = s1*s1
         mu = sqrt(s2+sigma)
         if (s1<=ZERO) then
            nu1 = s1-mu
         else
            nu1 = -sigma/(s1+mu)
         end if
         nu2 = nu1*nu1

         ! beta is such that it normalizes P
         beta = TWO*nu2/(sigma+nu2)
         nu(2:) = nu(2:)/nu1
      end if
   end subroutine


!  ====================
!  Fast Givens rotation 
!  ====================

   PURE subroutine fast_Givens(self,diag,alpha,beta,typ)
   ! Given "self" and +ve diagonal "diag" compute the fast Givens 
   ! 2 x 2 transformation such that M^T.self = 0 and where
   ! M^T DM is diagonal. "typ" is the type of the transform.
   ! See Golub & van Loan p. 221
      VEC(REAL,:), IN :: self
      VEC(REAL,2), INOUT :: diag
      REAL, OUT :: alpha
      REAL, OUT :: beta
      INT, OUT :: typ
      REAL :: tau,gamma
   ENSURE(size(self)==2,"VEC{REAL}:fast_Givens ... wrong size, self")
   ENSURE(size(diag)==2,"VEC{REAL}:fast_Givens ... wrong size, diag")
   ENSURE(all(diag>ZERO),"VEC{REAL}:fast_Givens ... diag not positive")
      if (self(2)/=0) then

         ! By defining alpha, beta and gamma like this, M^Tx produces
         ! zero.
         alpha = -self(1)/self(2)
         beta = -alpha*diag(2)/diag(1)
         gamma = -alpha*beta
         if (gamma<=ONE) then

            ! Type 1 is defined by alpha and beta on the diagonal.
            ! Swap?
            typ = 1
            tau = diag(1)
            gamma = ONE+gamma
            diag(1) = gamma*diag(2)
            diag(2) = gamma*tau
         else

            ! Type 2 is defined by 1's on the diagonal and 
            ! alpha and beta on the off diagonal.
            typ = 2
            alpha = ONE/alpha
            beta = ONE/beta
            gamma = ONE/gamma
            gamma = ONE+gamma
            diag(1) = gamma*diag(1)
            diag(2) = gamma*diag(2)
         end if
      else
         typ = 2
         alpha = 0
         beta = 0
      end if
   end subroutine


!  =====================
!  3D Hermite polynomial
!  =====================

   PURE function hermite_polynomial_prob(self,n) result (res)
   ! Return the probabilists hermite polynomial with powers n(1),
   ! n(2), and n(3) for each of the cartesian directions.
      VEC(REAL,:), IN :: self
      VEC(INT,3), IN :: n
      REAL :: res
      VEC(REAL,3) :: H
      H(1) = hermite_polynomial_prob_(self(1),n(1))
      H(2) = hermite_polynomial_prob_(self(2),n(2))
      H(3) = hermite_polynomial_prob_(self(3),n(3))
      res = H(1)*H(2)*H(3)
   end function


!  ======================
!  Line search operations
!  ======================

   subroutine goldstein_line_search(self,dself,a,alphamax,x,p,c1,alpha)
   ! Given a real vector, x, function f, gradient function
   ! df, calculaes the ideal stepping scale alpha, given
   ! stepping p and constants c1, c2.
   ! NB: needs more testing
      interface
         subroutine self(x,res)
            VEC(REAL,:), IN :: x
            REAL, OUT :: res
         end subroutine
      end interface
      interface
         subroutine dself(x,res)
            VEC(REAL,:), IN :: x
            VEC(REAL,:), OUT :: res
         end subroutine
      end interface
      VEC(REAL,:), IN :: x
      VEC(REAL,:), INOUT :: p
      REAL, IN :: c1,a,alphamax
      REAL, OUT :: alpha
      REAL :: b,alpha0,phi0,phi1,phi,dphi0
      VEC(REAL,:), allocatable :: temp
      INT :: iter,j
      BIN :: converged
      iter = 10000
      call create_(temp,size(x))
      alpha0 = 0
      call self(x,phi0)
      call dself(x,temp)
      dphi0 = dot_product(temp,p)
      phi1 = 100
      alpha = a
      j = 0
      converged = FALSE
      b = alphamax
      do while ((converged EQV FALSE) AND (j<iter))
         call self(x+alpha*p,phi)
         if (phi<=phi0+c1*alpha*dphi0) then
            if (phi>=phi0+(1-c1)*alpha*dphi0) then
               converged = TRUE
            else
               alpha0 = alpha
            end if
         else
            b = alpha
         end if
         if (converged EQV TRUE) cycle
         alpha = (alpha0+b)/2
         j = j+1
      end do
      call destroy_(temp)
   end subroutine

   subroutine wp_line_search(self,dself,a,alphamax,x,p,c1,c2,alpha)
   ! Given a real vector, x, function f, gradient function
   ! df, calculaes the ideal stepping scale alpha, given
   ! stepping p and constants c1, c2.
   ! NB: needs more testing
      interface
         subroutine self(x,res)
            VEC(REAL,:), IN :: x
            REAL, OUT :: res
         end subroutine
      end interface
      interface
         subroutine dself(x,res)
            VEC(REAL,:), IN :: x
            VEC(REAL,:), OUT :: res
         end subroutine
      end interface
      VEC(REAL,:), IN :: x
      VEC(REAL,:), INOUT :: p
      REAL, IN :: c1,c2,a,alphamax
      REAL, OUT :: alpha
      REAL :: alpha1,alpha2,phi1,phi,dphi,dphi1
      VEC(REAL,:), allocatable :: temp
      INT :: iter,j
      BIN :: converged
      iter = 10000
      call create_(temp,size(x))
      alpha1 = 0
      call self(x,phi1)
      call dself(x,temp)
      dphi1 = dot_product(temp,p)
      alpha = a
      j = 0
      converged = FALSE
      alpha2 = alphamax
      do while ((converged EQV FALSE) AND (j<iter))
         call self(x+alpha*p,phi)
         if (phi<=phi1+c1*alpha*dphi1) then
            call dself(x+alpha*p,temp)
            dphi = dot_product(temp,p)
            if (dphi>=c2*dphi1) then
               converged = TRUE
            else
               phi1 = phi
               dphi1 = dphi
               alpha1 = alpha
               alpha = alpha+((alpha-alpha1)*dphi)/(dphi1-dphi)
            end if
         else
            alpha2 = alpha
            alpha = alpha1+HALF*(alpha-alpha1)/(1+(phi1-phi)/((alpha-alpha1)*dphi1))
         end if
         if (converged EQV TRUE) cycle
         j = j+1
      end do
      call destroy_(temp)
   end subroutine

   subroutine line_search(self,dself,alphamax,x,p,c1,c2,b)
   ! Given a real vector, x, function f, gradient function
   ! df, calculaes the ideal stepping scale alpha, given
   ! stepping p and constants c1, c2.

   ! Interface for vector functions   
      interface
         subroutine self(x,res)
            VEC(REAL,:), IN :: x
            REAL, OUT :: res
         end subroutine
      end interface
      interface
         subroutine dself(x,res)
            VEC(REAL,:), IN :: x
            VEC(REAL,:), OUT :: res
         end subroutine
      end interface
      VEC(REAL,:), IN :: x
      VEC(REAL,:), IN :: p

      ! Wolfe constants c1 and c2 and a highest value of alpha
      REAL, IN :: c1,c2,alphamax

      ! The output alpha value
      REAL, OUT :: b

      ! alpha is the current value of alpha. alpha0 is the previous
      ! value. phi0 is the initial phi value, phi1 is the previous phi
      ! value and phi is thge current phi value. The same is true for
      ! dphi0, dphi1 and dphi.
      REAL :: alpha,alpha0,phi0,phi1,phi,dphi,dphi0,dphi1

      ! temp is used in vector dot product calculations. Such as in
      ! phi' (dphi)
      VEC(REAL,:), allocatable :: temp

      ! The maximum (iter) and current (j) number of iterations
      INT :: iter,j

      ! Logical for if the loop can conclude
      BIN :: converged

      ! The maximum number of iterations that may be performed
      iter = 10000

      ! Allows temp to be used for calculations
      call create_(temp,size(x))

      ! Gives the first value of alpha
      call RANDOM_NUMBER(phi)
      alpha = alpha0*alphamax

      ! Initialise alpha0, phi0, dphi0, phi1, j and converged
      alpha0 = 0
      call self(x,phi0)
      call dself(x,temp)
      dphi0 = dot_product(temp,p)
      call self(x+alpha*p,phi1)
      phi1 = phi1-1
      j = 0
      converged = FALSE

      ! Performs the loop until it has converged or the loop has
      ! failed
      do while ((converged EQV FALSE) AND (j<iter))

      ! calculates phi for this iteration
         call self(x+alpha*p,phi)

      ! The interval alpha0, alpha will contain ideal step lengths if
      ! any of the if conditionals below are violated
         if (phi>phi0+c1*alpha*dphi0) then

      ! Sends the interval off to find an ideal step and stores it in
      ! b
            call zoom(self,dself,alpha0,alpha,x,p,c1,c2,b,converged)
         else if (phi>=phi1 AND j>1) then

      ! Sends the interval off to find an ideal step and stores it in
      ! b
            call zoom(self,dself,alpha0,alpha,x,p,c1,c2,b,converged)
         end if

      ! Ensures that if alpha has converged, the value does not
      ! change   
         if (converged EQV FALSE) then

      ! calculates dphi for this iteration      
            call dself(x+alpha*p,temp)
            dphi = dot_product(temp,p)

      ! If the second wolfe condition holds, this has converged      
            if (abs(dphi)<=-c2*dphi0) then
               converged = TRUE
      ! Stores the ideal stepping in b         
               b = alpha
            else if (dphi>=0) then

      ! Sends the interval off to find an ideal step and stores it in
      ! b
               call zoom(self,dself,alpha,alpha0,x,p,c1,c2,b,converged)
            end if

      ! Ensures that if alpha has converged, the value does not
      ! change   
            if (converged EQV FALSE) then

      ! Stores the values from this iteration, and proceeds to the
      ! next iteration         
               phi1 = phi
               dphi1 = dphi
               j = j+1
               alpha0 = alpha
               call RANDOM_NUMBER(phi)
               alpha = alpha+phi*alphamax
            end if
         end if
      end do

      ! Cleanup
      call destroy_(temp)
   end subroutine

   subroutine zoom(self,dself,low,high,x,p,c1,c2,alpha,converged)
   ! Given a real vector, x, function f, gradient function
   ! df, calculaes the ideal stepping scale alpha, given
   ! stepping p and constants c1, c2. An ideal stepping must also
   ! reside between low and high.

      ! Interface for vector functions   
      interface
         subroutine self(x,res)
            VEC(REAL,:), IN :: x
            REAL, OUT :: res
         end subroutine
      end interface
      interface
         subroutine dself(x,res)
            VEC(REAL,:), IN :: x
            VEC(REAL,:), OUT :: res
         end subroutine
      end interface

      ! Interval in which ideal step resides, low and high and Wolfe
      ! constants c1 and c2
      REAL, IN :: low,high,c1,c2
      VEC(REAL,:), IN :: x,p

      ! Ideal stepping scale
      REAL, OUT :: alpha

      ! Logical which describes whether or not it has converged to an
      ! ideal step
      BIN, OUT :: converged

      ! Vector used in various multiplications
      VEC(REAL,:), allocatable :: temp

      ! Flexible interval alphalo and alphahi. Initial value of phi
      ! and dphi, phi0 and dphi0. Current values at phi and dphi. phi
      ! value at alpha lo is at philo.
      REAL :: alphalo,alphahi,phi0,dphi,dphi0,phi,philo

      ! The current (j) and maximum (iter) number of iterations
      INT :: j,iter
      iter = 10000

      ! creates temp
      call create_(temp,size(x))

      ! Initialise alphalo, alphahi, phi0, dphi0, converged and j
      alphalo = low
      alphahi = high
      call self(x,phi0)
      call dself(x,temp)
      dphi0 = dot_product(temp,p)
      converged = FALSE
      j = 0

      ! Does the loops until interations maximum is reached or the
      ! ideal stepping is found.
      do while ((converged EQV FALSE) AND (j<iter))

      ! New value of alpha is determined by bisection
         alpha = (alphalo+alphahi)/2

      ! Calculates current value of philo and phi   
         call self(x+alphalo*p,philo)
         call self(x+alpha*p,phi)

      ! If either of these condition are satisfied alphahi becomes
      ! alpha amd the cycle repeats   
         if (phi>phi0+c1*alpha*dphi0) then
            alphahi = alpha
         else if (phi>=philo) then
            alphahi = alpha
         else

      ! Calculates the current value of dphi
            call dself(x+alpha*p,temp)
            dphi = dot_product(temp,p)

      ! Cycle is complete if strong Wolfe condition holds      
            if (abs(dphi)<=-c2*dphi0) then
               converged = TRUE
            end if

      ! Stops pointless calculations if ideal stepping has been found
            if (converged EQV FALSE) then

      ! Ensures alphahi and alphalo are the right way around         
               if (dphi*(alphahi-alphalo)>=ZERO) then
                  alphahi = alphalo
               end if
      ! alphalo becomes alpha         
               alphalo = alpha
            end if
         end if
         j = j+1
      end do
      call destroy_(temp)
   end subroutine


! ========================
! Multipoles and harmonics
! ========================

   PURE subroutine make_R_harmonics_0(Rc,Rs,point,l_max)
   ! Make the scaled regular solid harmonics "Rc" and "Rs" evaluated at a single
   ! "point" in space, up to angular momentum "l_max". The particular solid
   ! harmonic for quantum numbers "L" and "M" is, e.g. Rc(L).element(M) for the
   ! given "point". See the book by Helgaker, Olsen, and Simons, p. 416. 
   ! NOTE: "Rc" and "Rs" are.associated.
   ! NOTE: this routine is here because VEC{REAL} cannot use VEC{EVEC{REAL}}
      VEC(type(EVEC_REAL_TYPE),:), allocatable, OUT :: Rc,Rs
      VEC(REAL,3), IN :: point
      INT, IN :: l_max
      INT :: L,M,sn
      REAL :: top,bot,fac,r2,x,y,z
   ENSURE(l_max>=0,"VEC{REAL}:make_R_harmonics_0 ... l_max must be non-negative")
      allocate(Rc(0:l_max))
      allocate(Rs(0:l_max))
      do L = 0,l_max
         allocate(Rc(L)%element(-L:L))
         allocate(Rs(L)%element(-L:L))
      end do
      Rc(0)%element(0) = ONE
      Rs(0)%element(0) = ZERO
      x = point(1)
      y = point(2)
      z = point(3)
      r2 = x*x+y*y+z*z
      do L = 0,l_max-1
         bot = 2*L+2
         top = bot-1
         fac = ONE/bot
         Rc(L+1)%element(L+1) = -fac*(x*Rc(L)%element(L)-y*Rs(L)%element(L))
         Rs(L+1)%element(L+1) = -fac*(y*Rc(L)%element(L)+x*Rs(L)%element(L))
         Rc(L+1)%element(L) = z*Rc(L)%element(L)
         Rs(L+1)%element(L) = z*Rs(L)%element(L)
         do M = 0,L-1
            bot = ONE/((L+M+1)*(L-M+1))
            fac = top*bot
            Rc(L+1)%element(M) = fac*z*Rc(L)%element(M)-bot*r2*Rc(L-1)%element(M)
            Rs(L+1)%element(M) = fac*z*Rs(L)%element(M)-bot*r2*Rs(L-1)%element(M)
         end do
         sn = 1
         do M = 1,L+1
            sn = -sn  ! this is (-1)==M
            Rc(L+1)%element(-M) = sn*Rc(L+1)%element(M)
            Rs(L+1)%element(-M) = -sn*Rs(L+1)%element(M)
         end do
      end do
   end subroutine

   PURE subroutine make_R_harmonics_1(Rc,Rs,points,l_max)
   ! Make the scaled regular solid harmonics "Rc" and "Rs" evaluated at a set of
   ! "points" in space, up to angular momentum "l_max". The particular solid
   ! harmonic for quantum numbers "L" and "M" evaluated at point "points(i,:)
   ! is, e.g.  Rc(L).element(M).element(i). See the book by Helgaker, Olsen, and
   ! Simons, p. 416. 
   ! NOTE: "Rc" and Rs are.associated.
   ! NOTE: this routine is here since VEC{REAL} cannot use VEC{EVEC{EVEC{REAL}}}
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable, OUT :: Rc,Rs
      MAT(REAL,:,:), IN :: points
      INT, IN :: l_max
      VEC(REAL,:), allocatable :: r2,x,y,z
      INT :: L,M,sn
      REAL :: top,bot,fac
   ENSURE(size(points,2)==3,"VEC{REAL}:make_R_harmonics_1 ... wrong 2nd dimension, points arrays")
   ENSURE(l_max>=0,"VEC{REAL}:make_R_harmonics_1 ... l_max must be non-negative")
      allocate(Rc(0:l_max))
      allocate(Rs(0:l_max))
      do L = 0,l_max
         allocate(Rc(L)%element(-L:L))
         allocate(Rs(L)%element(-L:L))
         do M = -L,L
            allocate(Rc(L)%element(M)%element(size(points,1)))
            allocate(Rs(L)%element(M)%element(size(points,1)))
         end do
      end do
      Rc(0)%element(0)%element(:) = ONE
      Rs(0)%element(0)%element(:) = ZERO
      x = points(:,1)
      y = points(:,2)
      z = points(:,3)
      r2 = x*x+y*y+z*z
      do L = 0,l_max-1
         bot = 2*L+2
         top = bot-1
         fac = ONE/bot
         Rc(L+1)%element(L+1)%element(:) = -fac*(x*Rc(L)%element(L)%element(:)-y*Rs(L)%element(L)%element(:))
         Rs(L+1)%element(L+1)%element(:) = -fac*(y*Rc(L)%element(L)%element(:)+x*Rs(L)%element(L)%element(:))
         Rc(L+1)%element(L)%element(:) = z*Rc(L)%element(L)%element(:)
         Rs(L+1)%element(L)%element(:) = z*Rs(L)%element(L)%element(:)
         do M = 0,L-1
            bot = ONE/((L+M+1)*(L-M+1))
            fac = top*bot
            Rc(L+1)%element(M)%element(:) = fac*z*Rc(L)%element(M)%element(:)-bot*r2*Rc(L-1)%element(M)%element(:)
            Rs(L+1)%element(M)%element(:) = fac*z*Rs(L)%element(M)%element(:)-bot*r2*Rs(L-1)%element(M)%element(:)
         end do
         sn = 1
         do M = 1,L+1
            sn = -sn  ! this is (-1)==M
            Rc(L+1)%element(-M)%element(:) = sn*Rc(L+1)%element(M)%element(:)
            Rs(L+1)%element(-M)%element(:) = -sn*Rs(L+1)%element(M)%element(:)
         end do
      end do
      call destroy_(r2)
      call destroy_(z)
      call destroy_(y)
      call destroy_(x)
   end subroutine

   PURE subroutine make_R_multipoles(qc,qs,charges,points,l_max)
   ! Make the scaled regular solid harmonic multipole moments "qc" and "qs" for
   ! a given set of "charges" located at a set of "points", up to a maximum
   ! angular momentum "l_max".  Reference: the book by Helgaker, Olsen, and
   ! Simons, p. 414. NOTE: "qc" and "qs" are.associated.
      VEC(REAL,:), allocatable, OUT :: qc,qs
      VEC(REAL,:), IN :: charges
      MAT(REAL,:,:), IN :: points
      INT, IN :: l_max
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable :: Rc,Rs
      INT :: L,M,n,lm
   ENSURE(size(charges)==size(points,1),"VEC{REAL}:make_R_multipoles ... inconsistent charges and points arrays")
   ENSURE(size(points,2)==3,"VEC{REAL}:make_R_multipoles ... wrong 2nd dimension, points arrays")
   ENSURE(l_max>=0,"VEC{REAL}:make_R_multipoles ... l_max must be non-negative")
      n = (l_max+1)*(l_max+2)/2
      call create_(qc,n)
      call create_(qs,n)

      ! Leaky
      call make_R_harmonics_(Rc,Rs,points,l_max)
      lm = 0
      do L = 0,l_max
         do M = 0,L
            lm = lm+1
            qc(lm) = sum(charges*Rc(L)%element(M)%element(:))
            qs(lm) = sum(charges*Rs(L)%element(M)%element(:))
         end do
      end do

      ! Clean
      do L = l_max,0,-1
         do M = L,-L,-1
            deallocate(Rs(L)%element(M)%element)
            deallocate(Rc(L)%element(M)%element)
         end do
         deallocate(Rs(L)%element)
         deallocate(Rc(L)%element)
      end do
      deallocate(Rs)
      deallocate(Rc)
   end subroutine

   PURE subroutine make_R_mu_harmonics(Rm,points,l_max)
   ! Make scaled regular mu-type solid harmonics "Rm" evaluated at set of
   ! "points" in space, up to angular momentum "l_max". The particular solid
   ! harmonic is, e.g. Rm(L).element(M).element(i) where "L" and "M" are the
   ! angular quantum numbers and "i" is the point number. See the book by
   ! Helgaker, Olsen, and Simons, p. 412 and p. 416. NOTE: "Rm" are.associated.
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable, OUT :: Rm
      MAT(REAL,:,:), IN :: points
      INT, IN :: l_max
      INT :: L,M
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable :: Rc,Rs
   ENSURE(size(points,2)==3,"VEC{REAL}:make_R_mu_harmonics ... wrong 2nd dimension, points arrays")
   ENSURE(l_max>=0,"VEC{REAL}:make_R_mu_harmonics ... l_max must be non-negative")
      allocate(Rm(0:l_max))
      do L = 0,l_max
         allocate(Rm(L)%element(-L:L))
         do M = -L,L
            allocate(Rm(L)%element(M)%element(size(points,1)))
         end do
      end do

      ! Leaky
      call make_R_harmonics_(Rc,Rs,points,l_max)
      do L = 0,l_max
         do M = 0,L
            Rm(L)%element(M)%element(:) = Rc(L)%element(M)%element(:)
         end do
         do M = -L,-1
            Rm(L)%element(M)%element(:) = Rs(L)%element(M)%element(:)
         end do
      end do

      ! Clean
      do L = l_max,0,-1
         do M = L,-L,-1
            deallocate(Rs(L)%element(M)%element)
            deallocate(Rc(L)%element(M)%element)
         end do
         deallocate(Rs(L)%element)
         deallocate(Rc(L)%element)
      end do
      deallocate(Rs)
      deallocate(Rc)
   end subroutine

   PURE subroutine make_R_mu_multipoles(q,charges,points,l_max)
   ! Make the scaled regular mu-type solid harmonic multipole moments "q" for a
   ! given set of "charges" located at a set of "points", up to a maximum
   ! angular momentum "l_max".  Reference: the book by Helgaker, Olsen, and
   ! Simons, p. 414. 
      VEC(REAL,:), OUT :: q
      VEC(REAL,:), IN :: charges
      MAT(REAL,:,:), IN :: points
      INT, IN :: l_max
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable :: Rm
      INT :: L,M,lm
   ENSURE(size(q)==(l_max+1)*(l_max+1),"VEC{REAL}:make_R_mu_multipoles ... wrong size for q")
   ENSURE(size(charges)==size(points,1),"VEC{REAL}:make_R_mu_multipoles ... inconsistent charges and points arrays")
   ENSURE(size(points,2)==3,"VEC{REAL}:make_R_mu_multipoles ... wrong 2nd dimension, points arrays")
   ENSURE(l_max>=0,"VEC{REAL}:make_R_mu_multipoles ... l_max must be non-negative")

      ! Leaky
      call make_R_mu_harmonics_(Rm,points,l_max)
      lm = 0
      do L = 0,l_max
         do M = -L,L  ! note the canonoical order here
            lm = lm+1
            q(lm) = sum(charges*Rm(L)%element(M)%element(:))
         end do
      end do

      ! Clean
      do L = l_max,0,-1
         do M = L,-L,-1
            deallocate(Rm(L)%element(M)%element)
         end do
         deallocate(Rm(L)%element)
      end do
      deallocate(Rm)
   end subroutine

   PURE subroutine add_R_mu_multipoles(q,charges,points,l_max)
   ! Add the scaled regular mu-type solid harmonic multipole moments "q" for a
   ! given set of "charges" located at a set of "points", up to a maximum
   ! angular momentum "l_max".  Reference: the book by Helgaker, Olsen, and
   ! Simons, p. 414. 
      VEC(REAL,:), OUT :: q
      VEC(REAL,:), IN :: charges
      MAT(REAL,:,:), IN :: points
      INT, IN :: l_max
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable :: Rm
      INT :: L,M,lm
   ENSURE(size(q)==(l_max+1)*(l_max+1),"VEC{REAL}:add_R_mu_multipoles ... wrong size for q")
   ENSURE(size(charges)==size(points,1),"VEC{REAL}:add_R_mu_multipoles ... inconsistent charges and points arrays")
   ENSURE(size(points,2)==3,"VEC{REAL}:add_R_mu_multipoles ... wrong 2nd dimension, points arrays")
   ENSURE(l_max>=0,"VEC{REAL}:add_R_mu_multipoles ... l_max must be non-negative")

      ! Leaky
      call make_R_mu_harmonics_(Rm,points,l_max)
      lm = 0

      ! Note canonoical order here
      do L = 0,l_max
         do M = -L,L
            lm = lm+1
            q(lm) = q(lm)+sum(charges*Rm(L)%element(M)%element(:))
         end do
      end do

      ! Clean
      do L = l_max,0,-1
         do M = L,-L,-1
            deallocate(Rm(L)%element(M)%element)
         end do
         deallocate(Rm(L)%element)
      end do
      deallocate(Rm)
   end subroutine

   PURE subroutine make_I_harmonics_0(Ic,Is,point,l_max)
   ! Make the scaled irregular solid harmonics "Ic" and "Is" evaluated at a
   ! single "point" in space, up to angular momentum "l_max". The particular
   ! solid harmonic for quantum numbers "L" and "M" is, e.g. Ic(L).element(M)
   ! for the given "point". See the book by Helgaker, Olsen, and Simons, p. 416.
   ! NOTE: "Ic" and "Is" are.associated.
   ! NOTE: this routine is here since VEC{REAL} cannot use VEC{EVEC{REAL}}
      VEC(type(EVEC_REAL_TYPE),:), allocatable, OUT :: Ic,Is
      VEC(REAL,3), IN :: point
      INT, IN :: l_max
      INT :: L,M,sn
      REAL :: top,fac,r2,x,y,z
   ENSURE(l_max>=0,"VEC{REAL}:make_I_harmonics_0 ... l_max must be non-negative")
      allocate(Ic(0:l_max))
      allocate(Is(0:l_max))
      do L = 0,l_max
         allocate(Ic(L)%element(-L:L))
         allocate(Is(L)%element(-L:L))
      end do
      x = point(1)
      y = point(2)
      z = point(3)
      r2 = ONE/(x*x+y*y+z*z)
      Ic(0)%element(0) = sqrt(r2)
      Is(0)%element(0) = ZERO
      do L = 0,l_max-1
         fac = 2*L+1
         Ic(L+1)%element(L+1) = -fac*r2*(x*Ic(L)%element(L)-y*Is(L)%element(L))
         Is(L+1)%element(L+1) = -fac*r2*(y*Ic(L)%element(L)+x*Is(L)%element(L))
         Ic(L+1)%element(L) = fac*z*r2*Ic(L)%element(L)
         Is(L+1)%element(L) = fac*z*r2*Is(L)%element(L)
         do M = 0,L-1
            top = L*L-M*M
            Ic(L+1)%element(M) = r2*(fac*z*Ic(L)%element(M)-top*Ic(L-1)%element(M))
            Is(L+1)%element(M) = r2*(fac*z*Is(L)%element(M)-top*Is(L-1)%element(M))
         end do
         sn = 1
         do M = 1,L+1
            sn = -sn  ! this is (-1)==M
            Ic(L+1)%element(-M) = sn*Ic(L+1)%element(M)
            Is(L+1)%element(-M) = -sn*Is(L+1)%element(M)
         end do
      end do
   end subroutine

   PURE subroutine make_I_harmonics_1(Ic,Is,points,l_max)
   ! Make the scaled irregular solid harmonics "Ic" and "Is" evaluated at set of
   ! "points" in space, up to maximum angular momentum "l_max".  The particular
   ! solid harmonic for quantum numbers "L" and "M" evaluated at point
   ! "points(i,:)" is, e.g. "Ic(L).element(M).element(i)".  See the book by
   ! Helgaker, Olsen, and Simons, p. 416. 
   ! NOTE: "Ic" and "Is" are.associated.
   ! NOTE: this routine is here since VEC{REAL} cannot use VEC{EVEC{EVEC{REAL}}}
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable, OUT :: Ic,Is
      MAT(REAL,:,:), IN :: points
      INT, IN :: l_max
      VEC(REAL,:), allocatable :: r2,x,y,z
      INT :: L,M,sn
      REAL :: top,fac
   ENSURE(size(points,2)==3,"VEC{REAL}:make_I_harmonics_1 ... wrong 2nd dimension, points arrays")
   ENSURE(l_max>=0,"VEC{REAL}:make_I_harmonics_1 ... l_max must be non-negative")
      allocate(Ic(0:l_max))
      allocate(Is(0:l_max))
      do L = 0,l_max
         allocate(Ic(L)%element(-L:L))
         allocate(Is(L)%element(-L:L))
         do M = -L,L
            allocate(Ic(L)%element(M)%element(size(points,1)))
            allocate(Is(L)%element(M)%element(size(points,1)))
         end do
      end do
      x = points(:,1)
      y = points(:,2)
      z = points(:,3)
      r2 = ONE/(x*x+y*y+z*z)
      Ic(0)%element(0)%element(:) = sqrt(r2)
      Is(0)%element(0)%element(:) = ZERO
      do L = 0,l_max-1
         fac = 2*L+1
         Ic(L+1)%element(L+1)%element(:) = -fac*r2*(x*Ic(L)%element(L)%element(:)-y*Is(L)%element(L)%element(:))
         Is(L+1)%element(L+1)%element(:) = -fac*r2*(y*Ic(L)%element(L)%element(:)+x*Is(L)%element(L)%element(:))
         Ic(L+1)%element(L)%element(:) = fac*z*r2*Ic(L)%element(L)%element(:)
         Is(L+1)%element(L)%element(:) = fac*z*r2*Is(L)%element(L)%element(:)
         do M = 0,L-1
            top = L*L-M*M
            Ic(L+1)%element(M)%element(:) = r2*(fac*z*Ic(L)%element(M)%element(:)-top*Ic(L-1)%element(M)%element(:))
            Is(L+1)%element(M)%element(:) = r2*(fac*z*Is(L)%element(M)%element(:)-top*Is(L-1)%element(M)%element(:))
         end do
         sn = 1
         do M = 1,L+1
            sn = -sn  ! this is (-1)==M
            Ic(L+1)%element(-M)%element(:) = sn*Ic(L+1)%element(M)%element(:)
            Is(L+1)%element(-M)%element(:) = -sn*Is(L+1)%element(M)%element(:)
         end do
      end do
      call destroy_(r2)
      call destroy_(z)
      call destroy_(y)
      call destroy_(x)
   end subroutine

   subroutine make_I_mu_harmonics(Im,points,l_max)
   ! Make scaled irregular mu-type solid harmonics "Im" evaluated at set of
   ! "points" in space, up to angular momentum "l_max". The particular solid
   ! harmonic is, e.g. Im(L).element(M).element(i) where "L" and "M" are the
   ! angular quantum numbers and "i" is the point number. See the book by
   ! Helgaker, Olsen, and Simons, p. 412 and p. 416. NOTE: "Rm" are.associated.
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable, OUT :: Im
      MAT(REAL,:,:), IN :: points
      INT, IN :: l_max
      INT :: L,M
      VEC(type(EVEC_EVEC_REAL_TYPE),:), allocatable :: Ic,Is
   ENSURE(size(points,2)==3,"VEC{REAL}:make_I_mu_harmonics ... wrong 2nd dimension, points arrays")
   ENSURE(l_max>=0,"VEC{REAL}:make_I_mu_harmonics ... l_max must be non-negative")
      allocate(Im(0:l_max))
      do L = 0,l_max
         allocate(Im(L)%element(-L:L))
         do M = -L,L
            allocate(Im(L)%element(M)%element(size(points,1)))
         end do
      end do

      ! Leaky
      call make_I_harmonics_(Ic,Is,points,l_max)
      do L = 0,l_max
         do M = 0,L
            Im(L)%element(M)%element(:) = Ic(L)%element(M)%element(:)
         end do
         do M = -L,-1
            Im(L)%element(M)%element(:) = Is(L)%element(M)%element(:)
         end do
      end do

      ! Clean
      do L = l_max,0,-1
         do M = L,-L,-1
            deallocate(Is(L)%element(M)%element)
            deallocate(Ic(L)%element(M)%element)
         end do
         deallocate(Is(L)%element)
         deallocate(Ic(L)%element)
      end do
      deallocate(Is)
      deallocate(Ic)
   end subroutine

   PURE subroutine to_multipole_T_interaction_vec(self,R,j_max)
   ! Make the multipole translation vector T(1,jk)(R) evaluated at position "R"
   ! where the maximum value of j is "j_max" and only the monopole term is
   ! calculated for the first indsex of the T matrix. This is used for
   ! evaluating the potential at the position "R" where a charge is located.
   ! See the book by Helgaker, Olsen, and Simons, equation (9.13.75), p. 415.
   ! NOTE: self is associated.
      VEC(REAL,:), OUT :: self
      VEC(REAL,3), IN :: R
      INT, IN :: j_max
      VEC(type(EVEC_REAL_TYPE),:), allocatable, target :: Ic,Is
      VEC(REAL,:), PTR :: Ic_lj,Is_lj
      INT :: j,k,jk,sj,sk,L
      REAL :: fac,fk
   ENSURE(j_max>=0,"VEC{REAL}:to_multipole_T_interaction_vec ... j_max must be non-negative")
   ENSURE(size(self)==(j_max+1)*(j_max+1),"VEC{REAL}:to_multipole_T_interaction_vec ... self has wrong length")

      ! Leaky
      call make_I_harmonics_(Ic,Is,R,j_max)
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
            Ic_lj => Ic(j)%element
            Is_lj => Is(j)%element
            fac = sj*fk
            self(jk) = fac*(Is_lj(k)-sk*Is_lj(-k))  ! T^cs
         end do

         ! Now do k>=0
         do k = 0,j
            jk = jk+1
            sk = -sk  ! initially,  (-1)==j
            fk = ONE
            if (k==0) fk = HALF
            Ic_lj => Ic(j)%element
            Is_lj => Is(j)%element
            fac = sj*fk
            self(jk) = fac*(Ic_lj(k)+sk*Ic_lj(-k))  ! T^cc
         end do
      end do

      ! Must deallocate Ic, Is
      do L = j_max,0,-1
         deallocate(Is(L)%element)
         deallocate(Ic(L)%element)
      end do
      deallocate(Is)
      deallocate(Ic)
   end subroutine


! ==============
! Test functions
! ==============

   subroutine rosenbrock_function(x,res)
   ! Calculates the rosenbrock function   
      VEC(REAL,:), IN :: x
      REAL, OUT :: res
      res = (1-x(1))**2+100*(x(2)-x(1)**2)**2
   end subroutine

   subroutine d_rosenbrock_function(x,res)
   ! Calculates the gradient functon of the rosenbrock function   
      VEC(REAL,:), IN :: x
      VEC(REAL,:), OUT :: res
      res(1) = -2+2*x(1)-400*x(1)*(x(2)-x(1)**2)
      res(2) = -200*x(1)**2+200*x(2)
   end subroutine

   subroutine himmelblau_function(x,res)
   ! alculates the himmelblau function   
      VEC(REAL,:), IN :: x
      REAL, OUT :: res
      res = (x(1)**2+x(2)-11)**2+(x(1)+x(2)**2-7)**2
   end subroutine

   subroutine d_himmelblau_function(x,res)
   ! Calculates the gradient functon of the himmelblau function   
      VEC(REAL,:), IN :: x
      VEC(REAL,:), OUT :: res
      res(1) = 4*x(1)*(x(1)**2+x(2)-11)+2*(x(1)+x(2)**2-7)
      res(2) = 2*(x(1)**2+x(2)-11)+4*x(2)*(x(1)+x(2)**2-7)
   end subroutine

   subroutine beale_function(x,res)
   ! calculates the beale function   
      VEC(REAL,:), IN :: x
      REAL, OUT :: res
      res = (1.5-x(1)*(1-x(2)))**2+(2.25-x(1)*(1-x(2)**2))**2+(2.625-x(1)*(1-x(2)**3))**2
   end subroutine

   subroutine d_beale_function(x,res)
   ! Calculates the gradient functon of the beale function   
      VEC(REAL,:), IN :: x
      VEC(REAL,:), OUT :: res
      res(1) = (x(2)-1)*(2*x(1)*(x(2)-1)+3)+(x(2)**2-1)/2*(4*x(1)*(x(2)**2-1)+9)+(x(2)**3-1)/4*(8*x(1)*(x(2)**3-1)+21)
      res(2) = x(1)/4*(8*x(1)*(x(2)-1)+x(2)**2*(24*x(1)*(x(2)**3-1)+63)+4*x(2)*(4*x(1)*(x(2)**2-1)+9)+12)
   end subroutine

   subroutine sphere_function(x,res)
   ! calculates the sphere function   
      VEC(REAL,:), IN :: x
      REAL, OUT :: res
      INT :: i
      res = ZERO
      do i = 1,size(x)
         res = res+x(i)**2
      end do
   end subroutine

   subroutine d_sphere_function(x,res)
   ! Calculates the sphere functon of the beale function   
      VEC(REAL,:), IN :: x
      VEC(REAL,:), OUT :: res
      INT :: i
      do i = 1,size(x)
         res(i) = 2*x(i)
      end do
   end subroutine

end module
