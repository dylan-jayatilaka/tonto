!=================================================================
!
!  VEC{INT} : Integer vector operations ...
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
!=================================================================
module VEC_INT_MODULE

#  include "macros"

   include "vec{int}.use"

   implicit none

   include "vec{int}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim)
   ! Allocate vector
      VEC(INT,:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{INT}:create_0 ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{INT}:create_0 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine create_1(self,lb,ub)
   ! Allocate the vector with lower bound "lb", upper bound "ub"
      VEC(INT,:), allocatable, OUT :: self
      INT, IN :: lb,ub

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb:ub),stat=status)
      ENSURE(status==0,"VEC{INT}:create_1 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine create_2(self,bounds)
   ! Allocate the vector with lower and upper bounds in "bounds"
      VEC(INT,:), allocatable, OUT :: self
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
      ENSURE(status==0,"VEC{INT}:create_2 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      VEC(INT,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{INT}:destroy ... error in deallocating memory")
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
      VEC(INT,:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: chop_start
      VEC(INT,:), allocatable :: new
      INT :: lb,ub
      BIN :: chop_end
   ENSURE(allocated(self),"VEC{INT}:shrink ... no self array")
   ENSURE(dim<=size(self),"VEC{INT}:shrink ... dim too large")

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
      VEC(INT,:), allocatable, INOUT :: self
      INT, IN :: dim
      INT, optional, IN :: init
      BIN, optional, IN :: grow_start
      INT :: lb,ub,nc
      BIN :: start
      VEC(INT,:), allocatable :: new
      INT :: val

      ! Value to initialize

      ! The following code is inherited from VEC{OBJECT}
      val = 0
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
         ENSURE(dim>size(self),"VEC{INT}:expand ... dim not large enough")
         ENSURE(dim>0,"VEC{INT}:expand ... dim is not +ve")

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
      VEC(INT,:), allocatable, INOUT :: self
      VEC(INT,:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = v
      else
         self = [self,v]
      end if
   end subroutine

   pure subroutine append_1(self,value)
   ! Appends the scalar "value" onto the end of allocatable "self".
      VEC(INT,:), allocatable, INOUT :: self
      INT, IN :: value

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [value]
      else
         self = [self,value]
      end if
   end subroutine

   PURE subroutine append_2(self,v1,v2)
   ! Appends vectors "v1" and "v2" to allocatable "self".
      VEC(INT,:), allocatable, INOUT :: self
      VEC(INT,:), IN :: v1,v2

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [v1,v2]
      else
         self = [self,v1,v2]
      end if
   end subroutine

   PURE subroutine append_only_if_unique(self,value)
   ! Appends the single scalar "value" to the end of allocatable
   ! "self", but only if the "value" does not already occur in "self"
      VEC(INT,:), allocatable, OUT :: self
      INT, IN :: value

      ! The following code is inherited from VEC{INTRINSIC}
      if (NOT allocated(self)) then
         call append_(self,value)
      else if (all(self/=value)) then
         call append_(self,value)
      end if
   end subroutine

   PURE subroutine prepend_0(self,v)
   ! Prepend vector "v" to allocatable "self".
      VEC(INT,:), allocatable, INOUT :: self
      VEC(INT,:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [v]
      else
         self = [v,self]
      end if
   end subroutine

   PURE subroutine prepend_1(self,value)
   ! Prepend a single "value" to allocatable "self".
      VEC(INT,:), allocatable, INOUT :: self
      INT, IN :: value

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
      VEC(INT,:), allocatable, INOUT :: self
      INT, IN :: k
      VEC(INT,:), allocatable :: old
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
      VEC(INT,:), allocatable, INOUT :: self
      VEC(INT,:), IN :: values
      INT :: lb

      ! The following code is inherited from VEC{OBJECT}
      lb = lbound(self,dim=1)
      self(lb:) = pack(self,mask=any(spread(self,2,size(values))==spread(values,1,size(self)),dim=1))
   end subroutine

   PURE subroutine prune_1(self,value)
   ! Remove the scalar "value" from allocatable "self" (if there).
   ! The order of the elements is otherwise unchanged.
      VEC(INT,:), allocatable, INOUT :: self
      INT, IN :: value
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
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: v
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = same_as_(self,v)
   end function

   pure function same_as(self,v) result (res)
   ! Return true if "self" is the same as "v".
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: v
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      if (size(self)/=size(v)) then; res = FALSE
      else; res = all(self==v)
      end if
   end function

   pure function has_elements_common_with(self,v) result (res)
   ! Return true if "self" has elements common with "v"
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: v
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = any(spread(self,2,size(v))==spread(v,1,size(self)))
   end function

   pure function has_no_elements_common_with(self,v) result (res)
   ! Return true if "self" has no elements common with "v"
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: v
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = all(spread(self,2,size(v))/=spread(v,1,size(self)))
   end function

   pure function has_all_elements_common_with(self,v) result (res)
   ! Return true if ALL elements of "self" are common with any in "v"
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: v
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
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: v
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
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: v
      INT :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = size(self)-n_elements_common_with_(self,v)
   end function

   PURE function elements_common_with(self,v) result (res)
   ! Return the elements of "self" which are common with "v",
   ! e.g. if self = [1,1,2,1] and v = [1], res = [1,1,1].
   ! See also routine below.
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: v
      VEC(INT,:), allocatable :: res
      INT :: i,n
      INT :: val

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
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: v
      VEC(INT,:), allocatable :: res
      INT :: i,n
      INT :: val

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
      VEC(INT,:), IN :: self
      INT, IN :: item
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

   PURE subroutine common_indices_in_0(ind1,ind2,pos1,pos2,comm)
   ! Look through *sorted* index arrays "ind1", and "ind2" and return
   ! in "pos2" and "pos2" the indices of the *positions* in those
   ! arrays where their indices are equal, and also in "comm" the
   ! actual values which are common to both.
      VEC(INT,:), IN :: ind1,ind2
      VEC(INT,:), allocatable, OUT :: pos1,pos2
      VEC(INT,:), allocatable, OUT :: comm
      INT :: val1,val2
      INT :: dim1,dim2
      INT :: n,i1,i2
      dim1 = size(ind1)
      dim2 = size(ind2)
      call create_(pos1,dim1)
      call create_(pos2,dim2)
      call create_(comm,min(dim1,dim2))
      i1 = 1
      i2 = 1
      n = 0

      ! Find intersect of two sorted lists
      do while (i1<=dim1 AND i2<=dim2)
         val1 = ind1(i1)
         val2 = ind2(i2)
         if (val1==val2) then

            ! Found common value
            n = n+1
            comm(n) = val1
            pos1(n) = i1
            pos2(n) = i2
            ! Next in each list
            i1 = i1+1
            i2 = i2+1
         else if (val1<val2) then

            ! Next value ind1 list
            i1 = i1+1
         else

            ! Next value ind2 list
            i2 = i2+1
         end if
      end do

      ! Shrink to no. common
      call shrink_(pos1,n)
      call shrink_(pos2,n)
      call shrink_(comm,n)
   end subroutine

   PURE subroutine common_indices_in_1(ind1,ind2,ind3,pos1,pos2,pos3,comm)
   ! Look through *sorted* index arrays "ind1", and "ind2" and return
   ! in "pos2" and "pos2" the indices of the *positions* in those
   ! arrays where their indices are equal, and also in "comm" the
   ! actual values which are common to both.
   ! NOTE: Untested
      VEC(INT,:), IN :: ind1,ind2,ind3
      VEC(INT,:), allocatable, OUT :: pos1,pos2,pos3
      VEC(INT,:), allocatable, OUT :: comm
      INT :: val1,val2,val3
      INT :: dim1,dim2,dim3
      INT :: n,i1,i2,i3
      dim1 = size(ind1)
      dim2 = size(ind2)
      dim3 = size(ind3)
      call create_(pos1,dim1)
      call create_(pos2,dim2)
      call create_(pos3,dim2)
      call create_(comm,min(dim1,dim2,dim3))
      i1 = 1
      i2 = 1
      i3 = 1
      n = 0

      ! Find intersect of two sorted lists
      do while (i1<=dim1 AND i2<=dim2 AND i3<dim3)
         val1 = ind1(i1)
         val2 = ind2(i2)
         val3 = ind3(i3)
         if (val1==val2 AND val1==val3) then

            ! Found common value
            n = n+1
            comm(n) = val1
            pos1(n) = i1
            pos2(n) = i2
            pos3(n) = i3
            ! Next in each list
            i1 = i1+1
            i2 = i2+1
            i3 = i3+1
         else if (val1<val3 AND val2<val3) then
            i1 = i1+1
            i2 = i2+1
         else if (val2<val1 AND val3<val1) then
            i2 = i2+1
            i3 = i3+1
         else if (val3<val2 AND val1<val2) then
            i1 = i1+1
            i3 = i3+1
         else if (val2==val3) then
            i1 = i1+1
         else if (val1==val3) then
            i2 = i2+1
         else if (val1==val2) then
            i3 = i3+1
         end if
      end do

      ! Shrink to no. common
      call shrink_(pos1,n)
      call shrink_(pos2,n)
      call shrink_(pos3,n)
      call shrink_(comm,n)
   end subroutine

   PURE subroutine is_permutation_of(self,v,res,permutation)
   ! Return true if "self" is a permutation of "v" 
   ! i.e. self = v(permutation).
      VEC(INT,:), IN :: self
      VEC(INT,size(self)), IN :: v
      VEC(INT,size(self)), OUT :: permutation
      BIN, OUT :: res
      VEC(BIN,:), allocatable :: skip
      INT :: i,j

      ! The following code is inherited from VEC{INTRINSIC}
      res = TRUE
      permutation = 0
      call create_(skip,size(self))
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
      call destroy_(skip)
   end subroutine

   pure function is_zero(self) result (res)
   ! Return true if the vector is zero
      VEC(INT,:), IN :: self
      BIN :: res
      res = all(self==0)
   end function

   function is_z_axis(self) result (res)
   ! Return true if the vector is set to the z-axis
      VEC(INT,:), IN :: self
      BIN :: res
   ENSURE(size(self)==3,"VEC{INT}:is_z_axis ... must supply a 3 dimensional vector!")
      res = self(1)==0
      res = self(2)==0 AND res
      res = (1-self(3))==0 AND res
   end function


! =============================
! Repetition related operations
! =============================

   PURE subroutine remove_repetitions(self)
   ! Remove repeated elements which come later in the list. 
   ! NOTE: allocatable
   ! NOTE: the vector may shrink. 
   ! NOTE: this version for bin, str, int
      VEC(INT,:), allocatable, INOUT :: self
      VEC(INT,:), allocatable :: unique
      INT :: i,j,n
      BIN :: found
   ENSURE(allocated(self),"VEC{INT}:remove_repetitions ... no vector")

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
      VEC(INT,:), IN :: self
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

   pure function no_of_unique_elements(self) result (res)
   ! Return the number of unique elements in the vector.
      VEC(INT,:), IN :: self
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

   PURE subroutine to_unique_elements_of(self,X)
   ! Set self to the unique elements of "X".
   ! NOTE: allocatable
      VEC(INT,:), allocatable, OUT :: self
      VEC(INT,:), IN :: X
      INT :: i,j,n
      BIN :: found
   ENSURE(size(X)>0,"VEC{INT}:to_unique_elements_of ... no zero-size vector")

      ! The following code is inherited from VEC{INTRINSIC}
      call create_(self,size(X))
      self(1) = X(1)
      if (size(X)==1) return

      ! No. of unique elements
      n = 1
      do i = 2,size(self)
         found = FALSE
         do j = 1,n
            if (X(i)/=self(j)) cycle
            found = TRUE
         end do
         if (found) cycle
         n = n+1
         self(n) = X(i)
      end do
      call shrink_(self,n)
   end subroutine

   PURE subroutine make_repetition_count(self,cnt,n_kind)
   ! Create "cnt" where each element cnt(i) is a count of the number
   ! times that self(i) is repeated in self(:). Later elements of
   ! "cnt" which are repetitions of earlier ones are assigned a count
   ! of zero. The number of different kinds is returned in "n_kind".
   ! NOTE: allocatable
   ! NOTE: this version for bin, str, int
      VEC(INT,:), IN :: self
      VEC(INT,:), allocatable, OUT :: cnt
      INT, OUT :: n_kind
      INT :: n,l

      ! The following code is inherited from VEC{INTRINSIC}
      call create_(cnt,size(self))
      cnt = 1
      do n = 1,size(self)
         if (cnt(n)==0) cycle
         do l = n+1,size(self)
            if (cnt(l)==0) cycle
            if (self(l)/=self(n)) cycle
            cnt(n) = cnt(n)+1
            cnt(l) = 0
         end do
      end do
      n_kind = size(self)-count(cnt==0)
   end subroutine

   PURE subroutine to_indices_of_unique_elements_in(self,X,ignore_zeros)
   ! Sort through the vector and return the indices of the unique elements
   ! NOTE: allocatable
      VEC(INT,:), allocatable, OUT :: self
      VEC(INT,:), IN :: X
      BIN, optional, IN :: ignore_zeros
      INT :: i,j,n
      BIN :: found,no_zeros
   ENSURE(size(X)>0,"VEC{INT}:to_indices_of_unique_elements_in ... no zero-size vector")
      no_zeros = FALSE
      if (present(ignore_zeros)) no_zeros = ignore_zeros
      call create_(self,size(X))
      self(1) = 1
      if (size(self)==1) return
      n = 1
      do i = 2,size(self)
         found = FALSE
         do j = 1,n
            if (X(i)/=self(j)) cycle
            if (self(j)==0 AND no_zeros) cycle
            found = TRUE
         end do
         if (found) cycle
         n = n+1
         self(n) = i
      end do
      call shrink_(self,n)
   end subroutine

   PURE subroutine make_unique_element_maps(self,unique_element,unique_element_for)
   ! Make a map of unique_element's, and the inverse map
   ! unique_element_for a given non-unique element in the list.
   ! NOTE: allocatable
      VEC(INT,:), IN :: self
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


! ==========
! Set theory
! ==========

   PURE subroutine to_union_of_elements_in(self,u,v)
   ! Set "self" to the union of elements in "u" and "v".
      VEC(INT,:), allocatable, INOUT :: self
      VEC(INT,:), IN :: u,v
   ENSURE(NOT allocated(self),"VEC{INT}:to_union_of_elements_in ... self should not be allocated!")
      self = [u,v]
   end subroutine

   PURE subroutine to_intersection_of_elements_in(self,u,v)
   ! Set self to the intersection of elements in "u" and "v".
      VEC(INT,:), allocatable, INOUT :: self
      VEC(INT,:), IN :: u,v
      VEC(BIN,:), allocatable :: fu,fv
      INT :: i,j
   ENSURE(NOT allocated(self),"VEC{INT}:to_intersection_of_elements_in ... self should not be allocated!")
      call create_(fu,size(u))
      call create_(fv,size(v))

      ! No elements match
      fu = FALSE
      fv = FALSE
      do i = 1,size(u)
         if (NOT fu(i)) then
            do j = 1,size(v)
               if (NOT fu(j)) then
                  if (u(i)==v(j)) then
                     fu(i) = TRUE
                     fv(j) = TRUE
                  end if
               end if
            end do
         end if
      end do

      ! Redefine self
      self = pack(u,mask=fu)
   end subroutine

   PURE subroutine complement_of(self,v,first,last,step)
   ! Return the complement of "v" in the sequence made from the
   ! "first" element up to the "last" (in units of "step", if
   ! present).
      VEC(INT,:), allocatable, INOUT :: self
      VEC(INT,:), IN :: v
      INT, IN :: first
      INT, IN :: last
      INT, optional, IN :: step
   ENSURE(NOT allocated(self),"VEC{INT}:complement_of ... self should not be allocated!")

      ! Generate the sequence to be complemented
      call to_sequence_(self,first,last,step)

      ! Remove elements in v
      call remove_elements_in_(self,v)
   end subroutine

   PURE subroutine remove_elements_in(self,v)
   ! Remove the elements in "v" from "self" and shrink 
   ! (if necessary).
      VEC(INT,:), allocatable, INOUT :: self
      VEC(INT,:), IN :: v
      VEC(BIN,:), allocatable :: found
      INT :: i,j
   ENSURE(size(v)<=size(self),"VEC{INT}:remove_elements_in ... v is too large")

      ! None of the elements match
      call create_(found,size(self))
      found = FALSE
      do i = 1,size(v)
         do j = 1,size(self)
            if (NOT found(j)) then
               if (self(j)==v(i)) then
                  found(j) = TRUE
               end if
            end if
         end do
      end do

      ! Redefine self
      self = pack(self,mask=NOT found)
   end subroutine


! ================
! Sorting routines
! ================

   PURE subroutine sort(self,decreasing_order)
   ! Sort array "self" from lowest to highest, using simple insertion sort.  If
   ! "decreasing_order" is present and TRUE sort from highest to lowest instead.
      VEC(INT,:), INOUT :: self
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
      VEC(INT,:), INOUT :: self
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
      VEC(INT,:), INOUT :: self
      VEC(INT,:), allocatable :: smaller,larger
      INT :: n,ns,ne,nl
      INT :: chosen

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
      VEC(INT,:), INOUT :: self
      VEC(INT,:), allocatable :: smaller,larger
      INT :: n,ns,ne,nl
      INT :: chosen

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
      VEC(INT,:), IN :: self
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
      VEC(INT,:), IN :: self
      VEC(INT,:), INOUT :: indices
      MAT(INT,:,:), IN :: then_by
      INT :: a,b
   ENSURE(size(then_by,1)==size(self),"VEC{INT}:order_ties_by ... then_by needs one row per element")

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
      VEC(INT,:), IN :: self
      VEC(INT,:), INOUT :: indices
      VEC(INT,:), allocatable :: list,small,equal,large,small_indices,equal_indices,large_indices
      INT :: n,i,ns,ne,nl
      INT :: chosen

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
      VEC(INT,:), IN :: self
      VEC(INT,:), INOUT :: indices
      VEC(INT,:), allocatable :: list,small,equal,large,small_indices,equal_indices,large_indices
      INT :: n,i,ns,ne,nl
      INT :: chosen

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
      VEC(INT,:), INOUT :: self
      INT :: n,dim

      ! The following code is inherited from VEC{INTRINSIC}
      dim = size(self)
      do n = 1,dim/2
         call swap_elements_(self,n,dim-n+1)
      end do
   end subroutine

   pure subroutine swap_elements(self,e1,e2)
   ! Swap elements "e1" and "e2" in "self".
      VEC(INT,:), INOUT :: self
      INT, IN :: e1,e2

   ! ENSURE(e1<=.dim AND e2<=.dim,"element exceeds dimesion")
   ! ENSURE(e1>0 AND e2>0,"indices must be positive")
      INT :: val

      ! The following code is inherited from VEC{INTRINSIC}
      val = self(e1)
      self(e1) = self(e2)
      self(e2) = val
   end subroutine

   PURE subroutine sort_by_rows(self,keys)
   ! Reorder the index list "self" so that the rows "keys(self(i),:)" come out
   ! in increasing lexical order, column 1 compared first. Insertion sort, for
   ! the short runs that break ties in a sort.
      VEC(INT,:), INOUT :: self
      MAT(INT,:,:), IN :: keys
      INT :: i,j,v
   ENSURE(minval(self)>=1 AND maxval(self)<=size(keys,1),"VEC{INT}:sort_by_rows ... index outside keys")
      do i = 2,size(self)
         v = self(i)
         j = i-1
         do
            if (j<1) exit
            if (NOT row_precedes_(keys,v,self(j))) exit
            self(j+1) = self(j)
            j = j-1
         end do
         self(j+1) = v
      end do
   end subroutine

   pure function row_precedes(keys,i,j) result (res)
   ! TRUE if row "i" of "keys" comes before row "j" in lexical order.
      MAT(INT,:,:), IN :: keys
      INT, IN :: i,j
      BIN :: res
      INT :: c
      res = FALSE
      do c = 1,size(keys,2)
         if (keys(i,c)==keys(j,c)) cycle
         res = keys(i,c)<keys(j,c)
         exit
      end do
   end function


! ====================
! Numerical operations
! ====================

   pure subroutine set_to(self,v)
   ! Set the vector to "v". See also the "copy" routine.
      VEC(INT,:), INOUT :: self
      VEC(INT,:), IN :: v

      ! The following code is inherited from VEC{INTRINSIC}
      self = v
   end subroutine

   pure subroutine plus(self,v,mask)
   ! Add vector "v" to "self"
      VEC(INT,:), INOUT :: self
      VEC(BIN,:), optional, IN :: mask
      VEC(INT,:), IN :: v
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
      VEC(INT,:), INOUT :: self
      VEC(INT,:), IN :: v
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
      VEC(INT,:), INOUT :: self
      VEC(INT,:), IN :: v
      INT, IN :: fac

      ! The following code is inherited from VEC{INTRINSIC}
      self = fac*v
   end subroutine

   pure subroutine plus_scaled(self,v,fac)
   ! Add a vector "v" scaled by "fac" to "self"
      VEC(INT,:), INOUT :: self
      VEC(INT,:), IN :: v
      INT, IN :: fac

      ! The following code is inherited from VEC{INTRINSIC}
      self = self+fac*v
   end subroutine

   pure function sum_elements(self) result (res)
   ! Return the sum of the elements in "self"
      VEC(INT,:), IN :: self
      INT :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         res = res+self(i)
      end do
   end function

   pure function dot(self,v) result (res)
   ! Return the dot product with "v"
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: v
      INT :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         res = res+(self(i))*v(i)
      end do
   end function

   pure function cross(self,v) result (res)
   ! Return the cross product of "self" and "v".
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: v
      VEC(INT,3) :: res

   ! ENSURE(.dim==3,"self must have dimension 3")

      ! The following code is inherited from VEC{INTRINSIC}
      res(1) = self(2)*v(3)-v(2)*self(3)
      res(2) = self(3)*v(1)-v(3)*self(1)
      res(3) = self(1)*v(2)-v(1)*self(2)
   end function

   pure subroutine to_cross_product_of(self,u,v)
   ! Set the vector to the cross product of "u" and "v".
      VEC(INT,:), INOUT :: self
      VEC(INT,:), IN :: u,v

   ! ENSURE(.dim==3,"self must have dimension 3")

      ! The following code is inherited from VEC{INTRINSIC}
      self(1) = u(2)*v(3)-v(2)*u(3)
      self(2) = u(3)*v(1)-v(3)*u(1)
      self(3) = u(1)*v(2)-v(1)*u(2)
   end subroutine

   PURE subroutine to_product_of(self,a,v,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v". If present,
   ! "transpose_a" can be set to TRUE if the matrix needs to be transposed.
      VEC(INT,:), OUT :: self
      MAT(INT,:,:), IN :: a
      VEC(INT,:), IN :: v
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      INT :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{INT}:to_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{INT}:to_product_of ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{INT}:to_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{INT}:to_product_of ... incompatible arrays")
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
      VEC(INT,:), INOUT :: self
      MAT(INT,:,:), IN :: a
      VEC(INT,:), IN :: v
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      INT :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{INT}:plus_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{INT}:plus_product_of ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{INT}:plus_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{INT}:plus_product_of ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+val
         end do
      end if
   end subroutine

   PURE subroutine to_scaled_product_of(self,a,v,fac,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(INT,:), OUT :: self
      MAT(INT,:,:), IN :: a
      VEC(INT,:), IN :: v
      INT, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      INT :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{INT}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{INT}:to_scaled_product_of ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = fac*val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{INT}:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{INT}:to_scaled_product_of ... incompatible arrays")
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
      VEC(INT,:), INOUT :: self
      MAT(INT,:,:), IN :: a
      VEC(INT,:), IN :: v
      INT, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      INT :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{INT}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{INT}:plus_scaled_product_of ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{INT}:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{INT}:plus_scaled_product_of ... incompatible arrays")
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
      VEC(INT,:), INOUT :: self
      MAT(INT,:,:), IN :: a
      VEC(INT,:), allocatable :: v
   ENSURE(size(a,1)==size(a,2),"VEC{INT}:rotate_by ... matrix must be square")
   ENSURE(size(a,2)==size(self),"VEC{INT}:rotate_by ... matrix and vector dimensions inconsistent")

      ! The following code is inherited from VEC{INTRINSIC}
      v = self
      call to_product_of_(self,a,v)
      call destroy_(v)
   end subroutine

   PURE subroutine translate_by(self,v)
   ! Translate self by vector "v".
      VEC(INT,:), INOUT :: self
      VEC(INT,:), IN :: v
   ENSURE(size(self)==size(v),"VEC{INT}:translate_by ... vectors not of same dimension")

      ! The following code is inherited from VEC{INTRINSIC}
      self = self+v
   end subroutine

   pure function outer_product_with(self,v) result (res)
   ! Returns the outer product of "self" with vector "v".
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: v
      MAT(INT,size(self),size(v)) :: res
      INT :: i,j

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         do j = 1,size(v)
            res(i,j) = self(i)*v(j)
         end do
      end do
   end function

   pure function norm(self) result (res)
   ! Return the norm of the vector
      VEC(INT,:), IN :: self
      REAL :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = sqrt(real(dot_product(self,self),kind=REAL_KIND))
   end function

   pure function distance_to(self,a) result (res)
   ! Return the distance to "a" if supplied
      VEC(INT,:), IN :: self
      VEC(INT,:), IN :: a
      REAL :: res
      VEC(INT,size(self)) :: temp

      ! The following code is inherited from VEC{INTRINSIC}
      temp = self-a
      res = norm_(temp)
   end function


! ==================
! Min/max operations
! ==================

   pure subroutine chop_large_absolute_values(self,val)
   ! Set all values in the self whose absolute value is larger than "val" to
   ! "val" times the sign of the original value.
      VEC(INT,:), INOUT :: self
      INT, IN :: val
      INT :: dim,i
      INT :: bb,ba,sign

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

   pure function largest_value(self) result (res)
   ! Return the maximum absolute value in the vector
      VEC(INT,:), IN :: self
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
      VEC(INT,:), IN :: self
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
      VEC(INT,:), IN :: self
      INT, IN :: tol
      INT :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = count(self>tol)
   end function


! ================
! Range operations
! ================

   pure function all_in_range(self,range) result (res)
   ! Return TRUE if all values of self are within the specified "range".
      VEC(INT,:), IN :: self
      VEC(INT,2), IN :: range
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = all(range(1)<=self AND self<=range(2))
   end function

   pure function in_range(self,range) result (res)
   ! Return element i as TRUE if self(i) is within the specified "range".
      VEC(INT,:), IN :: self
      VEC(INT,2), IN :: range
      VEC(BIN,size(self)) :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = (range(1)<=self AND self<=range(2))
   end function

   pure function element_range(self) result (res)
   ! Return the range (smallest and largest value) of self.
      VEC(INT,:), IN :: self
      VEC(INT,2) :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res(1) = minval(self)
      res(2) = maxval(self)
   end function

   pure subroutine to_sequence_0(self,first,last,step)
   ! Set "self" to a seqience from "first" up to "last" with "step"
   ! spacing if present, or spacing 1 if not.
   ! NOTE: self must *not* be allocated.
      VEC(INT,:), PTR :: self
      INT, IN :: first
      INT, IN :: last
      INT, optional, IN :: step
      INT :: i,n,s,val

      ! Step
      s = 1
      if (present(step)) s = step

      ! Sequence length
      n = (last-first)/s
      if (n<0) then; n = 0
      else; n = n+1
      end if

      ! The sequence
      allocate(self(n))
      i = 0
      val = first

      ! Make it
      do
         if (val>last) exit
         i = i+1
         self(i) = val
         val = val+s
      end do
   end subroutine

   pure subroutine to_sequence_1(self,first,last,step)
   ! Set allocatable "self" to a sequence from "first" up to "last"
   ! with "step" spacing (if present) or else spacing 1 (if not).
   ! NOTE: self must *not* be allocated.
      VEC(INT,:), allocatable, OUT :: self
      INT, IN :: first
      INT, IN :: last
      INT, optional, IN :: step
      INT :: i,n,s,val

      ! Step
      s = 1
      if (present(step)) s = step

      ! Sequence length
      n = (last-first)/s
      if (n<0) then; n = 0
      else; n = n+1
      end if

      ! The sequence
      allocate(self(n))
      if (n==0) return

      ! Make it
      val = first
      do i = 1,n
         self(i) = val
         val = val+s
      end do
   end subroutine


! ==========================
! Index returning operations
! ==========================

   pure function index_of_maximum(self) result (ind)
   ! Return the index of the maximum in the vector
      VEC(INT,:), IN :: self
      INT :: ind

      ! The following code is inherited from VEC{INTRINSIC}
      ind = maxval(maxloc(self))
   end function

   pure function index_of_minimum(self) result (ind)
   ! Return the index of the minimum in the vector
      VEC(INT,:), IN :: self
      INT :: ind

      ! The following code is inherited from VEC{INTRINSIC}
      ind = minval(minloc(self))
   end function

   pure function index_of_largest_value(self) result (ind)
   ! Return the index "ind" of the largest absolute value in the vector
      VEC(INT,:), IN :: self
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
      VEC(INT,:), IN :: self
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

   pure function index_of_first_nonzero_value(self) result (res)
   ! Returns the index of the first nonzero component of self.
   ! This version uses the == test for strict equality to zero.
      VEC(INT,:), IN :: self
      INT :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         if (self(i)==0) cycle
         res = i
         exit
      end do
   end function

   pure function index_of_first_zero_value(self) result (res)
   ! Returns the index of the first zero component of self.
   ! This version uses the == test for strict equality to zero.
      VEC(INT,:), IN :: self
      INT :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         if (self(i)/=0) cycle
         res = i
         exit
      end do
   end function

   pure function index_of_value(self,val,last) result (res)
   ! Returns the first index in "self" which has the value "val", or 0
   ! if "val" is not present in the array. If "last" is present and
   ! TRUE then the last value matching is returned
      VEC(INT,:), IN :: self
      INT, IN :: val
      BIN, optional, IN :: last
      INT :: res
      INT :: i
      BIN :: reverse

      ! The following code is inherited from VEC{INTRINSIC}
      reverse = FALSE
      if (present(last)) reverse = last
      res = 0
      if (NOT reverse) then
         do i = 1,size(self)
            if (same_as_(self(i),val)) then
               res = i
               exit
            end if
         end do
      else
         do i = size(self),1,-1
            if (same_as_(self(i),val)) then
               res = i
               exit
            end if
         end do
      end if
   end function

   pure function index_of_value_ne_to(self,val) result (res)
   ! Returns the first index in "self" which does not have the value
   ! "val", or 0 if all the values are equal to "val". This version
   ! uses the == test for strict equality.
      VEC(INT,:), IN :: self
      INT, IN :: val
      INT :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         if (self(i)==val) cycle
         res = i
         exit
      end do
   end function


! =============
! String widths
! =============

   pure function str_lengths(self) result (res)
   ! Returns the minimal string lengths.
      VEC(INT,:), IN :: self
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
      VEC(INT,:), IN :: self
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

   pure function to_str_vec_int_0(self) result (res)
   ! Change self to a string of minimal length
      VEC(INT,:), IN :: self
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = to_str_(self(i))
      end do
   end function

   pure function to_str_vec_int_1(self,fmt,left_justify) result (res)
   ! Change self to a "string" using the specified fortran "fmt".
      VEC(INT,:), IN :: self
      STR(len=*), IN :: fmt
      BIN, optional, IN :: left_justify
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = to_str_(self(i),fmt,left_justify)
      end do
   end function

   pure function to_str_vec_int_2(self,width,left_justify) result (res)
   ! Change self to a string with specified "width".
      VEC(INT,:), IN :: self
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
      VEC(INT,:), IN :: self
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


! ========================
! Unit conversion routines
! ========================

   PURE subroutine convert_to(self,units)
   ! Convert the "self" in atomic units or generic units to a new
   ! number in "units".
      VEC(INT,:), INOUT :: self
      STR(len=*), IN :: units
   ENSURE(is_known_unit_(units),"VEC{INT}:convert_to ... unknown units, "//units)

      ! The following code is inherited from VEC{INTRINSIC}
      self = nint(self*conversion_factor_(units))
   end subroutine

   PURE subroutine convert_from(self,units)
   ! Convert the number "self" from "units" system to a new number
   ! in atomic units or generic units.
      VEC(INT,:), INOUT :: self
      STR(len=*), IN :: units
   ENSURE(is_known_unit_(units),"VEC{INT}:convert_from ... unknown units, "//units)

      ! The following code is inherited from VEC{INTRINSIC}
      self = nint(self/conversion_factor_(units))
   end subroutine


! ======================
! Binning and histograms
! ======================

   PURE subroutine to_histogram_from_data(self,X,X_min,X_max,X_bin)
   ! Set "self" to a histogram matrix whose i-th element contains
   ! a count of the elements in the i-th bin of data "X", where a bin
   ! is a range of descriptor values in "X" of side length "X_bin"
   ! beginning at "X_min" and ending at "X_max". The dimension of self
   ! is calculated within this routine. 
   ! NOTE: Don't use binned data to calculate properties unless you
   ! really need to coarse grain.
      VEC(INT,:), allocatable, OUT :: self
      VEC(REAL,:), IN :: X
      REAL, IN :: X_min,X_max,X_bin
      INT :: n_X,i,k
      REAL :: X_ran
   ENSURE(X_max>X_min,"VEC{INT}:to_histogram_from_data ... X_max is smaller than X_min!")
   ENSURE(X_bin<(X_max-X_min),"VEC{INT}:to_histogram_from_data ... X_bin size is larger than [X_min,X_max]")

      ! The range of value ...
      X_ran = X_max-X_min

      ! The number of bins ...
      n_X = ceiling(X_ran/X_bin)

      ! Create the histogram (leaky)
      call create_(self,n_X)
      self = 0

      ! Now do the binning ...
      do k = 1,size(X)

         ! Get the bin
         i = ceiling(min(X(k)-X_min,X_ran)/X_bin)

         ! Accumulate
         self(i) = self(i)+1
      end do
   end subroutine


! ==========
! Misc stuff
! ==========

   PURE recursive subroutine make_combinations_of_length(self,k,C)
   ! Make the combination matrix "C" of all p distinct combinations
   ! C(:,p) of the elements in self(:) of length "k"
      VEC(INT,:), IN :: self
      INT, IN :: k
      MAT(INT,:,:), OUT :: C
      INT :: s,s_k,s1_k1
   ENSURE(k<=size(self),"VEC{INT}:make_combinations_of_length ... k is too large")
   ENSURE(k>0,"VEC{INT}:make_combinations_of_length ... k must be positive")
   ENSURE(size(C,1)==k,"VEC{INT}:make_combinations_of_length ... wrong dim1, C")
   ENSURE(size(C,2)==int(choose_(size(self),k),kind=INT_KIND),"VEC{INT}:make_combinations_of_length ... wrong dim2, C")
      s = size(self)
      s_k = int(choose_(s,k),kind=INT_KIND)
      if (k==1) then
         C(1,:) = self(:)
      else if (k==s) then
         C(:,1) = self(:)
      else if (s>k) then
         s1_k1 = int(choose_((s-1),k-1),kind=INT_KIND)
         C(1,1:s1_k1) = self(1)
         call make_combinations_of_length_(self(2:),k-1,C(2:k,1:s1_k1))
         call make_combinations_of_length_(self(2:),k,C(:,s1_k1+1:s_k))
      end if
   end subroutine

   recursive function combinations_of_length(self,k) result (C)
   ! Returns the combination matrix "C" of all p distinct combinations
   ! C(:,p) of the elements in self(:) of length "k"
   ! WARNING: this routine allocates the result "C"; it should not be
   ! disassociated in the caller, it should be *deallocated*. This should be fixed
   ! later so that there is no allocation within this routine.
      VEC(INT,:), IN :: self
      INT :: k
      MAT(INT,:,:), allocatable :: C
      INT :: s,s_k,s1_k1
      MAT(INT,:,:), allocatable :: L,R
   ENSURE(k<=size(self),"VEC{INT}:combinations_of_length ... k is too large")
   ENSURE(k>0,"VEC{INT}:combinations_of_length ... k must be positive")
      s = size(self)
      s_k = int(choose_(s,k),kind=INT_KIND)
      allocate(C(k,s_k))
      if (k==1) then
         C(1,:) = self(:)
      else if (k==s) then
         C(:,1) = self(:)
      else if (s>k) then
         s1_k1 = int(choose_((s-1),k-1),kind=INT_KIND)
         C(1,1:s1_k1) = self(1)
         L = combinations_of_length_(self(2:),k-1)
         C(2:k,1:s1_k1) = L
         deallocate(L)
         R = combinations_of_length_(self(2:),k)
         C(:,s1_k1+1:s_k) = R
         deallocate(R)
      end if
   end function

   subroutine to_pair_vec_from(self,v1,v2)
   ! If "v1" and "v2" are vectors which contain some common elements, set "self"
   ! so that self(i)=j implies that v1(i)=v2(j). Further, self(i)=0 that implies
   ! that the vector element i in "v1" is unpaired with any in "v2".
      VEC(INT,:), INOUT :: self
      VEC(INT,:) :: v1,v2
      INT :: i,j
   ENSURE(size(v1)==size(v2),"VEC{INT}:to_pair_vec_from ... vectors are not compatible sizes")
      self = 0
      do i = 1,size(v1)
         do j = 1,size(v2)
            if ((v1(i)==v2(j)) AND (NOT (any(self==j)))) self(i) = j
         end do
      end do
   end subroutine

end module
