!---------------------------------------------------------------------------
!
! VEC{EVEC{STR}} : a matrix where each column is (possibly) a different length
!
! Copyright (C) Dylan Jayatilaka, 2020
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
!
!---------------------------------------------------------------------------
module VEC_EVEC_STR_MODULE

#  include "macros"

   include "vec{evec{str}}.use"

   implicit none

   include "vec{evec{str}}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim)
   ! Allocate vector
      VEC(type(EVEC_STR_TYPE),:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{EVEC{STR}}:create_0 ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{EVEC{STR}}:create_0 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine create_1(self,bounds)
   ! Allocate the vector with lower and upper bounds in "bounds"
      VEC(type(EVEC_STR_TYPE),:), allocatable, OUT :: self
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
      ENSURE(status==0,"VEC{EVEC{STR}}:create_1 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      VEC(type(EVEC_STR_TYPE),:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{EVEC{STR}}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine create_2(self,dim,dimv)
   ! Allocate vector of vectors
      VEC(type(EVEC_STR_TYPE),:), allocatable, OUT :: self
      INT, IN :: dim,dimv

      ! The following code is inherited from VEC{EVEC{INTRINSIC}}
      call create_(self,[1,dim],[1,dimv])
   end subroutine

   PURE subroutine create_3(self,bounds1,bounds2)
   ! Create space for a vector of vectors
      VEC(type(EVEC_STR_TYPE),:), allocatable, OUT :: self
      VEC(INT,2), IN :: bounds1
      VEC(INT,2), IN :: bounds2

      ! The following code is inherited from VEC{EVEC{INTRINSIC}}
      call create_(self,bounds1)
      call create_vec_(self,bounds2)
   end subroutine

   PURE subroutine create_vec(self,bounds)
   ! Allocate the vector parts of "self"
      VEC(type(EVEC_STR_TYPE),:), INOUT :: self
      VEC(INT,2), IN :: bounds
      INT :: i

      ! The following code is inherited from VEC{EVEC{INTRINSIC}}
      do i = 1,size(self)
         call create_(self(i)%element,bounds)
      end do
   end subroutine

!  ==================
!  Size-of operations
!  ==================

   pure function max_element_dim(self) result (res)
   ! Return the maximum element dimension
      VEC(type(EVEC_STR_TYPE),:), IN :: self
      INT :: res
      INT :: s

      ! The following code is inherited from VEC{EVEC{INTRINSIC}}
      res = 0
      do s = 1,size(self)
         res = max(res,size(self(s)%element))
      end do
   end function


!  =======================
!  Shrinking and expansion
!  =======================

   PURE subroutine shrink(self,dim,chop_start)
   ! Shrink allocatable "self" to dimension "dim" from the end.
   ! Contents are retained. If "chop_start" is present and TRUE,
   ! "self" is chopped from the start, and the contents from the end
   ! are retained.
      VEC(type(EVEC_STR_TYPE),:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: chop_start
      VEC(type(EVEC_STR_TYPE),:), allocatable :: new
      INT :: lb,ub
      BIN :: chop_end
   ENSURE(allocated(self),"VEC{EVEC{STR}}:shrink ... no self array")
   ENSURE(dim<=size(self),"VEC{EVEC{STR}}:shrink ... dim too large")

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

   PURE subroutine expand(self,dim,grow_start)
   ! Expand allocatable "self" to dimension "dim" adding to the end.
   ! If "grow_start" is present and TRUE, then "self" is expanded from
   ! the start, keeping the same lower bound.
      VEC(type(EVEC_STR_TYPE),:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: grow_start
      INT :: nc
      VEC(type(EVEC_STR_TYPE),:), allocatable :: new
      BIN :: start

      ! Expand from start?

      ! The following code is inherited from VEC{OBJECT}
      start = FALSE
      if (present(grow_start)) start = grow_start
      if (NOT allocated(self)) then
         call create_(self,dim)
      else if (size(self)==0) then
         call destroy_(self)
         call create_(self,dim)
      else
         ENSURE(dim>size(self),"VEC{EVEC{STR}}:expand ... dim not large enough")
         ENSURE(dim>0,"VEC{EVEC{STR}}:expand ... dim is not +ve")
         call create_(new,dim)
         if (start) then
            nc = dim-size(self)+1
            new(nc:dim) = self
         else
            new(1:size(self)) = self
         end if

         ! Move it
         call move_alloc(new,self)
      end if
   end subroutine

   pure subroutine append_0(self,v)
   ! Append the contents of vector "v" to allocatable "self".
      VEC(type(EVEC_STR_TYPE),:), allocatable, INOUT :: self
      VEC(type(EVEC_STR_TYPE),:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = v
      else
         self = [self,v]
      end if
   end subroutine

   pure subroutine append_1(self,value)
   ! Appends the scalar "value" onto the end of allocatable "self".
      VEC(type(EVEC_STR_TYPE),:), allocatable, INOUT :: self
      type(EVEC_STR_TYPE), IN :: value

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [value]
      else
         self = [self,value]
      end if
   end subroutine

   PURE subroutine append_fifo(self,value,length)
   ! Expands self and insert "value"; if equal to "length", push the
   ! first/oldest vectors down/up the list
      VEC(type(EVEC_STR_TYPE),:), allocatable, INOUT :: self
      INT, IN :: length
      VEC(STR(len=*),:), IN :: value
      INT :: dim
   ENSURE(size(self)<=length,"VEC{EVEC{STR}}:append_fifo ... self is larger than length")

      ! The following code is inherited from VEC{EVEC{INTRINSIC}}
      dim = size(self)
      if (dim==length) then

         ! Move down & add to end
         self(1:length-1) = self(2:length)
         self(dim)%element = value
      else

         ! Expand by one and add
         call expand_(self,dim+1)
         self(dim+1)%element = value
      end if
   end subroutine

   PURE subroutine append_listed_to(self,res,list)
   ! Append together (i.e. concatenate) the vectors in self whose
   ! indices are listed in "list".
      VEC(type(EVEC_STR_TYPE),:), IN :: self
      VEC(INT,:), IN :: list
      VEC(STR(len=*),:), allocatable, OUT :: res
      INT :: i
   ENSURE(size(list)>=1,"VEC{EVEC{STR}}:append_listed_to ... list must not have zero dimension")
   ENSURE(maxval(list)<=size(self),"VEC{EVEC{STR}}:append_listed_to ... some elements of list are too large")
   ENSURE(minval(list)>=1,"VEC{EVEC{STR}}:append_listed_to ... list elements must be +ve")

      ! The following code is inherited from VEC{EVEC{INTRINSIC}}
      res = [(self(list(i))%element(:),i = 1,size(list))]
   end subroutine


!  =======
!  Strings
!  =======

   pure function item_str_length(self) result (res)
   ! Returns the max string length (not including spaces) over all
   ! elements.  
      VEC(type(EVEC_STR_TYPE),:), IN :: self
      INT :: res
      INT :: i,j

      ! Get the maximum element width
      res = 0
      do i = 1,size(self)
         do j = 1,size(self(i)%element)
            res = max(res,str_length_(self(i)%element(j)))
         end do
      end do
   end function

   pure function list_str_length(self,spaces) result (res)
   ! Returns the max of the concatenated list lengths
      VEC(type(EVEC_STR_TYPE),:), IN :: self
      INT, IN, optional :: spaces
      INT :: res
      INT :: wid
      wid = item_str_length_(self)
      if (present(spaces)) wid = wid+spaces
      res = wid*max_element_dim_(self)
   end function

end module
