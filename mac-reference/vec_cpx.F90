!====================================================================
!
! VEC{CPX}: Complex vector operations ...
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
module VEC_CPX_MODULE

#  include "macros"

   include "vec{cpx}.use"

   implicit none

   include "vec{cpx}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim)
   ! Allocate vector
      VEC(CPX,:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{CPX}:create_0 ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{CPX}:create_0 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine create_1(self,lb,ub)
   ! Allocate the vector with lower bound "lb", upper bound "ub"
      VEC(CPX,:), allocatable, OUT :: self
      INT, IN :: lb,ub

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb:ub),stat=status)
      ENSURE(status==0,"VEC{CPX}:create_1 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine create_2(self,bounds)
   ! Allocate the vector with lower and upper bounds in "bounds"
      VEC(CPX,:), allocatable, OUT :: self
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
      ENSURE(status==0,"VEC{CPX}:create_2 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      VEC(CPX,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{CPX}:destroy ... error in deallocating memory")
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
      VEC(CPX,:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: chop_start
      VEC(CPX,:), allocatable :: new
      INT :: lb,ub
      BIN :: chop_end
   ENSURE(allocated(self),"VEC{CPX}:shrink ... no self array")
   ENSURE(dim<=size(self),"VEC{CPX}:shrink ... dim too large")

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
      VEC(CPX,:), allocatable, INOUT :: self
      INT, IN :: dim
      CPX, optional, IN :: init
      BIN, optional, IN :: grow_start
      INT :: lb,ub,nc
      BIN :: start
      VEC(CPX,:), allocatable :: new
      CPX :: val

      ! Value to initialize

      ! The following code is inherited from VEC{OBJECT}
      val = IMAGIFY(ZERO)
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
         ENSURE(dim>size(self),"VEC{CPX}:expand ... dim not large enough")
         ENSURE(dim>0,"VEC{CPX}:expand ... dim is not +ve")

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
      VEC(CPX,:), allocatable, INOUT :: self
      VEC(CPX,:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = v
      else
         self = [self,v]
      end if
   end subroutine

   pure subroutine append_1(self,value)
   ! Appends the scalar "value" onto the end of allocatable "self".
      VEC(CPX,:), allocatable, INOUT :: self
      CPX, IN :: value

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [value]
      else
         self = [self,value]
      end if
   end subroutine

   PURE subroutine append_2(self,v1,v2)
   ! Appends vectors "v1" and "v2" to allocatable "self".
      VEC(CPX,:), allocatable, INOUT :: self
      VEC(CPX,:), IN :: v1,v2

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [v1,v2]
      else
         self = [self,v1,v2]
      end if
   end subroutine

   PURE subroutine prepend_0(self,v)
   ! Prepend vector "v" to allocatable "self".
      VEC(CPX,:), allocatable, INOUT :: self
      VEC(CPX,:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [v]
      else
         self = [v,self]
      end if
   end subroutine

   PURE subroutine prepend_1(self,value)
   ! Prepend a single "value" to allocatable "self".
      VEC(CPX,:), allocatable, INOUT :: self
      CPX, IN :: value

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
      VEC(CPX,:), allocatable, INOUT :: self
      INT, IN :: k
      VEC(CPX,:), allocatable :: old
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
      VEC(CPX,:), IN :: self
      VEC(CPX,:), IN :: v
      REAL, optional, IN :: eps
      BIN :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = same_as_(self,v,eps)
   end function

   pure function same_as(self,v,eps) result (res)
   ! Return true if "self" is the same as "v", within range "eps" if supplied
      VEC(CPX,:), IN :: self
      VEC(CPX,:), IN :: v
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
      VEC(CPX,:), IN :: self
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

   pure function is_zero(self,eps) result (res)
   ! Return true if the vector is zero (within "eps", if supplied)
      VEC(CPX,:), IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      REAL :: tmp

      ! The following code is inherited from VEC{INTRINSIC}
      tmp = norm_(self)
      res = is_zero_(tmp,eps)
   end function


!  =============================
!  Repetition related operations
!  =============================

   PURE subroutine remove_repetitions(self,eps)
   ! Remove repeated elements which come later in the list. 
   ! NOTE: allocatable
   ! NOTE: the vector may shrink. 
   ! NOTE: this version for reals & complex
      VEC(CPX,:), allocatable, INOUT :: self
      REAL, optional, IN :: eps
      VEC(CPX,:), allocatable :: unique
      INT :: i,j,n
      BIN :: found
   ENSURE(allocated(self),"VEC{CPX}:remove_repetitions ... no vector")

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
      VEC(CPX,:), IN :: self
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
      VEC(CPX,:), IN :: self
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


!  ================
!  Sorting routines
!  ================

   pure subroutine reverse_order(self)
   ! Reverse the order of the elements of self
      VEC(CPX,:), INOUT :: self
      INT :: n,dim

      ! The following code is inherited from VEC{INTRINSIC}
      dim = size(self)
      do n = 1,dim/2
         call swap_elements_(self,n,dim-n+1)
      end do
   end subroutine

   pure subroutine swap_elements(self,e1,e2)
   ! Swap elements "e1" and "e2" in "self".
      VEC(CPX,:), INOUT :: self
      INT, IN :: e1,e2

   ! ENSURE(e1<=.dim AND e2<=.dim,"element exceeds dimesion")
   ! ENSURE(e1>0 AND e2>0,"indices must be positive")
      CPX :: val

      ! The following code is inherited from VEC{INTRINSIC}
      val = self(e1)
      self(e1) = self(e2)
      self(e2) = val
   end subroutine


!  ====================
!  Numerical operations
!  ====================

   pure subroutine set_to_0(self,v)
   ! Set the vector to "v". See also the "copy" routine.
      VEC(CPX,:), INOUT :: self
      VEC(CPX,:), IN :: v

      ! The following code is inherited from VEC{INTRINSIC}
      self = v
   end subroutine

   pure subroutine set_to_1(self,v)
   ! Set the vector to "v". See also the "copy" routine.
      VEC(CPX,:), INOUT :: self
      VEC(REAL,:), IN :: v

      ! The following code is inherited from VEC{INTRINSIC}
      self = v
   end subroutine

   pure subroutine plus_0(self,v,mask)
   ! Add vector "v" to "self"
      VEC(CPX,:), INOUT :: self
      VEC(BIN,:), optional, IN :: mask
      VEC(CPX,:), IN :: v
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

   pure subroutine plus_1(self,v,mask)
   ! Add vector "v" to "self"
      VEC(CPX,:), INOUT :: self
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

   pure subroutine minus_0(self,v,mask)
   ! Subtract vector "v" from "self"
      VEC(CPX,:), INOUT :: self
      VEC(CPX,:), IN :: v
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

   pure subroutine minus_1(self,v,mask)
   ! Subtract vector "v" from "self"
      VEC(CPX,:), INOUT :: self
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

   pure subroutine to_scaled_0(self,v,fac)
   ! Set the vector to "v" scaled by "fac"
      VEC(CPX,:), INOUT :: self
      VEC(CPX,:), IN :: v
      CPX, IN :: fac

      ! The following code is inherited from VEC{INTRINSIC}
      self = fac*v
   end subroutine

   pure subroutine to_scaled_1(self,v,fac)
   ! Set the vector to "v" scaled by "fac"
      VEC(CPX,:), INOUT :: self
      VEC(CPX,:), IN :: v
      REAL, IN :: fac

      ! The following code is inherited from VEC{INTRINSIC}
      self = fac*v
   end subroutine

   pure subroutine to_scaled_2(self,v,fac)
   ! Set the vector to "v" scaled by "fac"
      VEC(CPX,:), INOUT :: self
      VEC(REAL,:), IN :: v
      CPX, IN :: fac

      ! The following code is inherited from VEC{INTRINSIC}
      self = fac*v
   end subroutine

   pure subroutine to_scaled_3(self,v,fac)
   ! Set the vector to "v" scaled by "fac"
      VEC(CPX,:), INOUT :: self
      VEC(REAL,:), IN :: v
      REAL, IN :: fac

      ! The following code is inherited from VEC{INTRINSIC}
      self = fac*v
   end subroutine

   pure subroutine plus_scaled_0(self,v,fac)
   ! Add a vector "v" scaled by "fac" to "self"
      VEC(CPX,:), INOUT :: self
      VEC(CPX,:), IN :: v
      CPX, IN :: fac

      ! The following code is inherited from VEC{INTRINSIC}
      self = self+fac*v
   end subroutine

   pure subroutine plus_scaled_1(self,v,fac)
   ! Add a vector "v" scaled by "fac" to "self"
      VEC(CPX,:), INOUT :: self
      VEC(CPX,:), IN :: v
      REAL, IN :: fac

      ! The following code is inherited from VEC{INTRINSIC}
      self = self+fac*v
   end subroutine

   pure subroutine plus_scaled_2(self,v,fac)
   ! Add a vector "v" scaled by "fac" to "self"
      VEC(CPX,:), INOUT :: self
      VEC(REAL,:), IN :: v
      CPX, IN :: fac

      ! The following code is inherited from VEC{INTRINSIC}
      self = self+fac*v
   end subroutine

   pure subroutine plus_scaled_3(self,v,fac)
   ! Add a vector "v" scaled by "fac" to "self"
      VEC(CPX,:), INOUT :: self
      VEC(REAL,:), IN :: v
      REAL, IN :: fac

      ! The following code is inherited from VEC{INTRINSIC}
      self = self+fac*v
   end subroutine

   pure function sum_elements(self) result (res)
   ! Return the sum of the elements in "self"
      VEC(CPX,:), IN :: self
      CPX :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         res = res+self(i)
      end do
   end function

   pure function dot_0(self,v) result (res)
   ! Return the dot product with "v"
      VEC(CPX,:), IN :: self
      VEC(CPX,:), IN :: v
      CPX :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         res = res+CONJG(self(i))*v(i)
      end do
   end function

   pure function dot_1(self,v) result (res)
   ! Return the dot product with "v"
      VEC(CPX,:), IN :: self
      VEC(REAL,:), IN :: v
      CPX :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      res = 0
      do i = 1,size(self)
         res = res+CONJG(self(i))*v(i)
      end do
   end function

   pure function cross(self,v) result (res)
   ! Return the cross product of "self" and "v".
      VEC(CPX,:), IN :: self
      VEC(CPX,:), IN :: v
      VEC(CPX,3) :: res

   ! ENSURE(.dim==3,"self must have dimension 3")

      ! The following code is inherited from VEC{INTRINSIC}
      res(1) = self(2)*v(3)-v(2)*self(3)
      res(2) = self(3)*v(1)-v(3)*self(1)
      res(3) = self(1)*v(2)-v(1)*self(2)
   end function

   pure subroutine to_cross_product_of(self,u,v)
   ! Set the vector to the cross product of "u" and "v".
      VEC(CPX,:), INOUT :: self
      VEC(CPX,:), IN :: u,v

   ! ENSURE(.dim==3,"self must have dimension 3")

      ! The following code is inherited from VEC{INTRINSIC}
      self(1) = u(2)*v(3)-v(2)*u(3)
      self(2) = u(3)*v(1)-v(3)*u(1)
      self(3) = u(1)*v(2)-v(1)*u(2)
   end subroutine

   PURE subroutine to_product_of_0(self,a,v,dagger_a,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v". If present,
   ! "transpose_a" can be set to TRUE if the matrix needs to be transposed.
      VEC(CPX,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(CPX,:), IN :: v
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:to_product_of_0 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:to_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:to_product_of_0 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_product_of_0 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_product_of_0 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = val
         end do
      end select
   end subroutine

   PURE subroutine to_product_of_1(self,a,v,dagger_a,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v". If present,
   ! "transpose_a" can be set to TRUE if the matrix needs to be transposed.
      VEC(CPX,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(REAL,:), IN :: v
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:to_product_of_1 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:to_product_of_1 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = val
         end do
      end select
   end subroutine

   PURE subroutine to_product_of_2(self,a,v,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v". If present,
   ! "transpose_a" can be set to TRUE if the matrix needs to be transposed.
      VEC(CPX,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(CPX,:), IN :: v
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      CPX :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:to_product_of_2 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:to_product_of_2 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = val
         end do
      end if
   end subroutine

   PURE subroutine to_product_of_3(self,a,v,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v". If present,
   ! "transpose_a" can be set to TRUE if the matrix needs to be transposed.
      VEC(CPX,:), OUT :: self
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
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:to_product_of_3 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:to_product_of_3 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = val
         end do
      end if
   end subroutine

   PURE subroutine plus_product_of_0(self,a,v,dagger_a,transpose_a)
   ! Add to "self" the product of the matrix "a" and vector "v". If present,
   ! "transpose_a" can be set to TRUE if the matrix needs to be transposed.
      VEC(CPX,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(CPX,:), IN :: v
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:plus_product_of_0 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:plus_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:plus_product_of_0 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_product_of_0 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = self(i)+val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_product_of_0 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+val
         end do
      end select
   end subroutine

   PURE subroutine plus_product_of_1(self,a,v,dagger_a,transpose_a)
   ! Add to "self" the product of the matrix "a" and vector "v". If present,
   ! "transpose_a" can be set to TRUE if the matrix needs to be transposed.
      VEC(CPX,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(REAL,:), IN :: v
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:plus_product_of_1 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:plus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = self(i)+val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+val
         end do
      end select
   end subroutine

   PURE subroutine plus_product_of_2(self,a,v,transpose_a)
   ! Add to "self" the product of the matrix "a" and vector "v". If present,
   ! "transpose_a" can be set to TRUE if the marix needs to be transposed.
      VEC(CPX,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(CPX,:), IN :: v
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      CPX :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:plus_product_of_2 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:plus_product_of_2 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+val
         end do
      end if
   end subroutine

   subroutine plus_product_of_3(self,a,v,transpose_a)
   ! Add to "self" the product of the matrix "a" and vector "v". If present,
   ! "transpose_a" can be set to TRUE if the marix needs to be transposed.
      VEC(CPX,:), INOUT :: self
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
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:plus_product_of_3 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:plus_product_of_3 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+val
         end do
      end if
   end subroutine

   PURE subroutine minus_product_of_0(self,a,v,dagger_a,transpose_a)
   ! Subtract from "self" the product of the matrix "a" and vector "v". If
   ! present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(CPX,:), IN :: v
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:minus_product_of_0 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:minus_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:minus_product_of_0 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)-val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:minus_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:minus_product_of_0 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = self(i)-val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:minus_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:minus_product_of_0 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)-val
         end do
      end select
   end subroutine

   PURE subroutine minus_product_of_1(self,a,v,dagger_a,transpose_a)
   ! Subtract from "self" the product of the matrix "a" and vector "v". If
   ! present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(REAL,:), IN :: v
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:minus_product_of_1 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:minus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:minus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)-val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:minus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:minus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = self(i)-val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:minus_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:minus_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)-val
         end do
      end select
   end subroutine

   PURE subroutine minus_product_of_2(self,a,v,transpose_a)
   ! Subtract from "self" the product of the matrix "a" and vector "v". If
   ! present, "transpose_a" can be set to TRUE if the marix needs to be
   ! transposed.
      VEC(CPX,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(CPX,:), IN :: v
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      CPX :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:minus_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:minus_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)-val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:minus_product_of_2 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:minus_product_of_2 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)-val
         end do
      end if
   end subroutine

   PURE subroutine minus_product_of_3(self,a,v,transpose_a)
   ! Subtract from "self" the product of the matrix "a" and vector "v". If
   ! present, "transpose_a" can be set to TRUE if the marix needs to be
   ! transposed.
      VEC(CPX,:), INOUT :: self
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
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:minus_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:minus_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)-val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:minus_product_of_3 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:minus_product_of_3 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)-val
         end do
      end if
   end subroutine

   PURE subroutine to_scaled_product_of_0(self,a,v,fac,dagger_a,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(CPX,:), IN :: v
      CPX, IN :: fac
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:to_scaled_product_of_0 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = fac*val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = fac*val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_scaled_product_of_0 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = fac*val
         end do
      end select
   end subroutine

   PURE subroutine to_scaled_product_of_1(self,a,v,fac,dagger_a,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(CPX,:), IN :: v
      REAL, IN :: fac
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:to_scaled_product_of_1 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = fac*val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = fac*val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_scaled_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = fac*val
         end do
      end select
   end subroutine

   PURE subroutine to_scaled_product_of_2(self,a,v,fac,dagger_a,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(REAL,:), IN :: v
      CPX, IN :: fac
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:to_scaled_product_of_2 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = fac*val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = fac*val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = fac*val
         end do
      end select
   end subroutine

   PURE subroutine to_scaled_product_of_3(self,a,v,fac,dagger_a,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), OUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(REAL,:), IN :: v
      REAL, IN :: fac
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:to_scaled_product_of_3 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = fac*val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = fac*val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = fac*val
         end do
      end select
   end subroutine

   PURE subroutine to_scaled_product_of_4(self,a,v,fac,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(CPX,:), IN :: v
      CPX, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      CPX :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_scaled_product_of_4 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_scaled_product_of_4 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = fac*val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:to_scaled_product_of_4 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:to_scaled_product_of_4 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = fac*val
         end do
      end if
   end subroutine

   PURE subroutine to_scaled_product_of_5(self,a,v,fac,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(CPX,:), IN :: v
      REAL, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      CPX :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_scaled_product_of_5 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_scaled_product_of_5 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = fac*val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:to_scaled_product_of_5 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:to_scaled_product_of_5 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = fac*val
         end do
      end if
   end subroutine

   PURE subroutine to_scaled_product_of_6(self,a,v,fac,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), OUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(REAL,:), IN :: v
      CPX, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      REAL :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_scaled_product_of_6 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_scaled_product_of_6 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = fac*val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:to_scaled_product_of_6 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:to_scaled_product_of_6 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = fac*val
         end do
      end if
   end subroutine

   PURE subroutine to_scaled_product_of_7(self,a,v,fac,transpose_a)
   ! Set "self" to the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), OUT :: self
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
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:to_scaled_product_of_7 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:to_scaled_product_of_7 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = fac*val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:to_scaled_product_of_7 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:to_scaled_product_of_7 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = fac*val
         end do
      end if
   end subroutine

   PURE subroutine plus_scaled_product_of_0(self,a,v,fac,dagger_a,transpose_a)
   ! Add to "self" the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(CPX,:), IN :: v
      CPX, IN :: fac
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:plus_scaled_product_of_0 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = self(i)+fac*val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_scaled_product_of_0 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      end select
   end subroutine

   PURE subroutine plus_scaled_product_of_1(self,a,v,fac,dagger_a,transpose_a)
   ! Add to "self" the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(CPX,:), IN :: v
      REAL, IN :: fac
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:plus_scaled_product_of_1 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = self(i)+fac*val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_scaled_product_of_1 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      end select
   end subroutine

   PURE subroutine plus_scaled_product_of_2(self,a,v,fac,dagger_a,transpose_a)
   ! Add to "self" the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(REAL,:), IN :: v
      CPX, IN :: fac
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:plus_scaled_product_of_2 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = self(i)+fac*val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_scaled_product_of_2 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      end select
   end subroutine

   PURE subroutine plus_scaled_product_of_3(self,a,v,fac,dagger_a,transpose_a)
   ! Add to "self" the product of the matrix "a" and vector "v" scaled by "fac".
   ! If present, "transpose_a" can be set to TRUE if the matrix needs to be
   ! transposed.
      VEC(CPX,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(REAL,:), IN :: v
      REAL, IN :: fac
      BIN, optional, IN :: dagger_a,transpose_a
      BIN :: dagg_a,trans_a
      INT :: opt,i,k
      CPX :: val
   ENSURE(NOT (present(dagger_a) AND present(transpose_a)),"VEC{CPX}:plus_scaled_product_of_3 ... cant dagger and transpose")

      ! The following code is inherited from VEC{INTRINSIC}
      dagg_a = FALSE
      trans_a = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(transpose_a)) trans_a = transpose_a
      opt = 0
      if (dagg_a) opt = 1
      if (trans_a) opt = 2
      select case (opt)

      case (0)
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,1)
            val = (ZERO,ZERO)
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do

      case (1)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+conjg(a(k,i))*v(k)
            end do
            self(i) = self(i)+fac*val
         end do

      case (2)
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_scaled_product_of_3 ... incompatible arrays")
         do i = 1,size(a,2)
            val = (ZERO,ZERO)
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      end select
   end subroutine

   PURE subroutine plus_scaled_product_of_4(self,a,v,fac,transpose_a)
   ! Add to self "self" the product of the matrix "a" and vector "v" scaled by
   ! "fac".  If present, "transpose_a" can be set to TRUE if the matrix needs to
   ! be transposed.
      VEC(CPX,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(CPX,:), IN :: v
      CPX, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      CPX :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_scaled_product_of_4 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_scaled_product_of_4 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:plus_scaled_product_of_4 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:plus_scaled_product_of_4 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      end if
   end subroutine

   PURE subroutine plus_scaled_product_of_5(self,a,v,fac,transpose_a)
   ! Add to self "self" the product of the matrix "a" and vector "v" scaled by
   ! "fac".  If present, "transpose_a" can be set to TRUE if the matrix needs to
   ! be transposed.
      VEC(CPX,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(CPX,:), IN :: v
      REAL, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      CPX :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_scaled_product_of_5 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_scaled_product_of_5 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:plus_scaled_product_of_5 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:plus_scaled_product_of_5 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      end if
   end subroutine

   PURE subroutine plus_scaled_product_of_6(self,a,v,fac,transpose_a)
   ! Add to self "self" the product of the matrix "a" and vector "v" scaled by
   ! "fac".  If present, "transpose_a" can be set to TRUE if the matrix needs to
   ! be transposed.
      VEC(CPX,:), INOUT :: self
      MAT(REAL,:,:), IN :: a
      VEC(REAL,:), IN :: v
      CPX, IN :: fac
      BIN, optional, IN :: transpose_a
      BIN :: trans_a
      REAL :: val
      INT :: i,k

      ! The following code is inherited from VEC{INTRINSIC}
      trans_a = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (trans_a) then
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_scaled_product_of_6 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_scaled_product_of_6 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:plus_scaled_product_of_6 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:plus_scaled_product_of_6 ... incompatible arrays")
         do i = 1,size(a,1)
            val = 0
            do k = 1,size(a,2)
               val = val+a(i,k)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      end if
   end subroutine

   PURE subroutine plus_scaled_product_of_7(self,a,v,fac,transpose_a)
   ! Add to self "self" the product of the matrix "a" and vector "v" scaled by
   ! "fac".  If present, "transpose_a" can be set to TRUE if the matrix needs to
   ! be transposed.
      VEC(CPX,:), INOUT :: self
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
         ENSURE(size(a,2)<=size(self),"VEC{CPX}:plus_scaled_product_of_7 ... incompatible arrays")
         ENSURE(size(a,1)==size(v),"VEC{CPX}:plus_scaled_product_of_7 ... incompatible arrays")
         do i = 1,size(a,2)
            val = 0
            do k = 1,size(a,1)
               val = val+a(k,i)*v(k)
            end do
            self(i) = self(i)+fac*val
         end do
      else
         ENSURE(size(a,1)<=size(self),"VEC{CPX}:plus_scaled_product_of_7 ... incompatible arrays")
         ENSURE(size(a,2)==size(v),"VEC{CPX}:plus_scaled_product_of_7 ... incompatible arrays")
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
      VEC(CPX,:), INOUT :: self
      MAT(CPX,:,:), IN :: a
      VEC(CPX,:), allocatable :: v
   ENSURE(size(a,1)==size(a,2),"VEC{CPX}:rotate_by ... matrix must be square")
   ENSURE(size(a,2)==size(self),"VEC{CPX}:rotate_by ... matrix and vector dimensions inconsistent")

      ! The following code is inherited from VEC{INTRINSIC}
      v = self
      call to_product_of_(self,a,v)
      call destroy_(v)
   end subroutine

   PURE subroutine translate_by(self,v)
   ! Translate self by vector "v".
      VEC(CPX,:), INOUT :: self
      VEC(CPX,:), IN :: v
   ENSURE(size(self)==size(v),"VEC{CPX}:translate_by ... vectors not of same dimension")

      ! The following code is inherited from VEC{INTRINSIC}
      self = self+v
   end subroutine

   pure function outer_product_with(self,v) result (res)
   ! Returns the outer product of "self" with vector "v".
      VEC(CPX,:), IN :: self
      VEC(CPX,:), IN :: v
      MAT(CPX,size(self),size(v)) :: res
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
      VEC(CPX,:), IN :: self
      REAL :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = sqrt(real(dot_product(self,self),kind=REAL_KIND))
   end function

   pure subroutine normalize(self)
   ! Normalise the vector
      VEC(CPX,:), INOUT :: self

      ! The following code is inherited from VEC{INTRINSIC}
      self = self/norm_(self)
   end subroutine

   PURE subroutine canonicalise_phase(self,tol)
   ! Fix the arbitrary overall PHASE of "self" by rotating it so that its
   ! largest-magnitude element is real and positive.
   !
   ! The complex analogue of VEC{REAL}:canonicalise_sign, and a stronger
   ! condition: a real eigenvector is arbitrary up to a sign, a complex one up
   ! to any phase on the unit circle, so the fix is a rotation rather than a
   ! negation.
   !
   ! Elements within RELATIVE tolerance "tol" of the largest count as tied, and
   ! the LOWEST-indexed of them decides -- see VEC{REAL}:canonicalise_sign for
   ! why the tolerance carries the weight here.
   !
   ! An all-zero vector has no phase to fix and is left alone.
      VEC(CPX,:), INOUT :: self
      REAL, IN, optional :: tol
      REAL :: maxv,eps
      CPX :: phase
      INT :: i,ind
   ENSURE(size(self)>0,"VEC{CPX}:canonicalise_phase ... zero-length vector")
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

         ! Rotate so element "ind" becomes real and positive
         phase = conjg(self(ind))/abs(self(ind))
         self = phase*self
      end if
   end subroutine

   pure function distance_to(self,a) result (res)
   ! Return the distance to "a" if supplied
      VEC(CPX,:), IN :: self
      VEC(CPX,:), IN :: a
      REAL :: res
      VEC(CPX,size(self)) :: temp

      ! The following code is inherited from VEC{INTRINSIC}
      temp = self-a
      res = norm_(temp)
   end function

   pure function mean(self) result (res)
   ! Return the mean of the vector
      VEC(CPX,:), IN :: self
      CPX :: res

      ! The following code is inherited from VEC{INTRINSIC}
      res = sum(self)/REALIFY(size(self))
   end function


!  ==================
!  Min/max operations
!  ==================

   pure subroutine chop_large_values(self,val)
   ! Set all values in the self whose absolute value is larger than "val" to
   ! "val" times the sign of the original value.
      VEC(CPX,:), INOUT :: self
      REAL, IN :: val
      INT :: dim,i
      REAL :: ba
      CPX :: bb,phase
      dim = size(self)
      do i = 1,dim
         bb = self(i)
         ba = abs(bb)
         if (ba==ZERO) then
            self(i) = ZERO
         else
            phase = bb/ba
            self(i) = phase*min(val,ba)
         end if
      end do
   end subroutine

   pure subroutine zero_small_values(self,eps)
   ! Zero elements of the vector which are less than "tol" in magnitude
      VEC(CPX,:), INOUT :: self
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
      VEC(CPX,:), IN :: self
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
      VEC(CPX,:), IN :: self
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


!  ==========================
!  Index returning operations
!  ==========================

   pure function index_of_largest_value(self) result (ind)
   ! Return the index "ind" of the largest absolute value in the vector
      VEC(CPX,:), IN :: self
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
      VEC(CPX,:), IN :: self
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
      VEC(CPX,:), IN :: self
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
      VEC(CPX,:), IN :: self
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


!  =============
!  String widths
!  =============

   pure function str_lengths_0(self) result (res)
   ! Returns the minimal string lengths.
      VEC(CPX,:), IN :: self
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
      VEC(CPX,:), IN :: self
      INT, IN :: max_dp
      VEC(INT,size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = str_length_(self(i),max_dp)
      end do
   end function

   pure subroutine get_max_str_length_0(self,msl,spaces)
   ! Returns "msl", the maximum of the string lengths for "self",
   ! and with optional extra "spaces". 
      VEC(CPX,:), IN :: self
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
      VEC(CPX,:), IN :: self
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


!  =====================
!  Conversion to strings
!  =====================

   pure function to_str_vec_cpx_0(self) result (res)
   ! Change self to a string of minimal length
      VEC(CPX,:), IN :: self
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = to_str_(self(i))
      end do
   end function

   pure function to_str_vec_cpx_1(self,fmt,left_justify) result (res)
   ! Change self to a "string" using the specified fortran "fmt".
      VEC(CPX,:), IN :: self
      STR(len=*), IN :: fmt
      BIN, optional, IN :: left_justify
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: i

      ! The following code is inherited from VEC{INTRINSIC}
      do i = 1,size(self)
         res(i) = to_str_(self(i),fmt,left_justify)
      end do
   end function

   pure function to_str_vec_cpx_2(self,style,width,precision,left_justify) result (res)
   ! Change self to a string with specified "style", "width" and
   ! "precision" as defined in the fortran standard 
      VEC(CPX,:), IN :: self
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
      VEC(CPX,:), IN :: self
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


!  =======================
!  Spin-orbital operations
!  =======================

!   alpha result (res) :: get_from(VEC{INTRINSIC})
!   ! return the alpha sector of the vector
!   end

!   beta result (res) :: get_from(VEC{INTRINSIC})
!   ! return the beta sector of the vector
!   end

   PURE subroutine set_a_block_to_0(self,X)
   ! Set the alpha sector of the vector
      VEC(CPX,:), INOUT :: self
      VEC(CPX,:), IN :: X
      INT :: n
   ENSURE(is_even_(size(self)),"VEC{CPX}:set_a_block_to_0 ... self is not even-dimensioned")
   ENSURE(size(X)==size(self)/2,"VEC{CPX}:set_a_block_to_0 ... wrong size, X")

      ! The following code is inherited from VEC{INTRINSIC}
      n = size(self)/2
      self(1:n) = X
   end subroutine

   PURE subroutine set_b_block_to_0(self,X)
   ! Set the beta sector of the vector
      VEC(CPX,:), INOUT :: self
      VEC(CPX,:), IN :: X
      INT :: n
   ENSURE(is_even_(size(self)),"VEC{CPX}:set_b_block_to_0 ... self is not even-dimensioned")
   ENSURE(size(self)==2*size(X),"VEC{CPX}:set_b_block_to_0 ... wrong size, X")

      ! The following code is inherited from VEC{INTRINSIC}
      n = size(self)/2
      self(n+1:2*n) = X
   end subroutine

   PURE subroutine set_a_block_to_1(self,X)
   ! Set the alpha sector of the vector
      VEC(CPX,:), OUT :: self
      VEC(REAL,:), IN :: X
      INT :: n
   ENSURE(is_even_(size(self)),"VEC{CPX}:set_a_block_to_1 ... self is not even-dimensioned")
   ENSURE(size(X)==size(self)/2,"VEC{CPX}:set_a_block_to_1 ... wrong size, X")
      n = size(self)/2
      self(1:n) = X
   end subroutine

   PURE subroutine set_b_block_to_1(self,X)
   ! Set the beta sector of the vector
      VEC(CPX,:), OUT :: self
      VEC(REAL,:), IN :: X
      INT :: n
   ENSURE(is_even_(size(self)),"VEC{CPX}:set_b_block_to_1 ... self is not even-dimensioned")
   ENSURE(size(X)==size(self)/2,"VEC{CPX}:set_b_block_to_1 ... wrong size, X")
      n = size(self)/2
      self(n+1:2*n) = X
   end subroutine

end module
