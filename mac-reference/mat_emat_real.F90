!==================================================================
!
! MAT{EMAT{REAL}}
!
! A matrix elements each of which is a matrix.
!
! Copyright (C) Dylan Jayatilaka, 2024
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
module MAT_EMAT_REAL_MODULE

#  include "macros"

   include "mat{emat{real}}.use"

   implicit none

   include "mat{emat{real}}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim1,dim2)
   ! Allocate a matrix with the given dimensions
      MAT(type(EMAT_REAL_TYPE),:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{INTRINSIC}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim1,dim2),stat=status)
      ENSURE(status==0,"MAT{EMAT{REAL}}:create_0 ... error in allocating memory")
#else
      allocate(self(dim1,dim2))
#endif
   end subroutine

   PURE subroutine create_1(self,dim1,dim2,dim3,dim4)
   ! Allocate object with desired dimensions, ".element" part with bounds "dimv".
      MAT(type(EMAT_REAL_TYPE),:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2,dim3,dim4
      call create_(self,dim1,dim2)
      call create_mat_(self,dim3,dim4)
   end subroutine

   PURE subroutine create_mat(self,dim3,dim4)
   ! Allocate all ".element" parts with the same shape.
      MAT(type(EMAT_REAL_TYPE),:,:), INOUT :: self
      INT, IN :: dim3,dim4
      INT :: i1,i2
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            call create_(self(i1,i2)%element,dim3,dim4)
         end do
      end do
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      MAT(type(EMAT_REAL_TYPE),:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT{EMAT{REAL}}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Deallocate the ".element" parts
      MAT(type(EMAT_REAL_TYPE),:,:), INOUT :: self
      INT :: i1,i2
      do i1 = 1,size(self,1)
         do i2 = 1,size(self,2)
            call destroy_(self(i1,i2)%element)
         end do
      end do
   end subroutine

end module
