!==================================================================
!
! MAT3{EVEC{INT}}
!
! A 3D matrix where each element is a vector of (possibly) a 
! different length
!
! Copyright (C) Dylan Jayatilaka, 2002
! Copyright (C) Dylan Jayatilaka, 2021
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
module MAT_EVEC_INT_MODULE

#  include "macros"

   include "mat{evec{int}}.use"

   implicit none

   include "mat{evec{int}}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim1,dim2)
   ! Allocate a matrix with the given dimensions
      MAT(type(EVEC_INT_TYPE),:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{INTRINSIC}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim1,dim2),stat=status)
      ENSURE(status==0,"MAT{EVEC{INT}}:create_0 ... error in allocating memory")
#else
      allocate(self(dim1,dim2))
#endif
   end subroutine

   PURE subroutine create_1(self,lb1,ub1,lb2,ub2)
   ! Allocate a matrix with the given bounds
      MAT(type(EVEC_INT_TYPE),:,:), allocatable, OUT :: self
      INT, IN :: lb1,ub1
      INT, IN :: lb2,ub2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{INTRINSIC}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb1:ub1,lb2:ub2),stat=status)
      ENSURE(status==0,"MAT{EVEC{INT}}:create_1 ... error in allocating memory")
#else
      allocate(self(lb1:ub1,lb2:ub2))
#endif
   end subroutine

   PURE subroutine create_2(self,dim1,dim2,dimv)
   ! Allocate object with desired dimensions, ".element" part with bounds "dimv".
      MAT(type(EVEC_INT_TYPE),:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2
      INT, IN :: dimv
      call create_(self,dim1,dim2)
      call create_vec_(self,dimv)
   end subroutine

   PURE subroutine create_3(self,dim,dimv)
   ! Allocate object with desired dimensions, ".element" part with bounds "dimv".
      MAT(type(EVEC_INT_TYPE),:,:), allocatable, OUT :: self
      VEC(INT,3), IN :: dim
      VEC(INT,2), IN :: dimv
      call create_(self,dim(1),dim(2))
      call create_vec_(self,dimv)
   end subroutine

   PURE subroutine create_vec_0(self,dimv)
   ! Allocate only the the ".element" pointer parts with size "dimv".
      MAT(type(EVEC_INT_TYPE),:,:), INOUT :: self
      INT, IN :: dimv
      INT :: i1,i2
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            call create_(self(i1,i2)%element,dimv)
         end do
      end do
   end subroutine

   PURE subroutine create_vec_1(self,dimv)
   ! Allocate only the the ".element" pointer parts with bounds "dimv".
      MAT(type(EVEC_INT_TYPE),:,:), INOUT :: self
      VEC(INT,2), IN :: dimv
      INT :: i1,i2
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            call create_(self(i1,i2)%element,dimv)
         end do
      end do
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      MAT(type(EVEC_INT_TYPE),:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT{EVEC{INT}}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Deallocate the ".element" allocatable parts
      MAT(type(EVEC_INT_TYPE),:,:), INOUT :: self
      INT :: i1,i2
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            call destroy_(self(i1,i2)%element)
         end do
      end do
   end subroutine

end module
