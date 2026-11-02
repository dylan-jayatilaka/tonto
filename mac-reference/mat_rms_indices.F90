!-------------------------------------------------------------------------------
!
! MAT{RMS_INDICES}
!
! This object is used to store precalculated information pertaining to the
! reduced multiplication scheme (RMS) used in electron repulsion integral 
! evaluation. 
!
! Copyright (C) Dylan Jayatilaka, 2006
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
!
!-------------------------------------------------------------------------------
module MAT_RMS_INDICES_MODULE

#  include "macros"

   include "mat{rms_indices}.use"

   implicit none

   include "mat{rms_indices}.int"


contains

! ==========
! Allocation
! ==========

   PURE subroutine create_0(self,dim1,dim2)
   ! Allocate object
      MAT(type(RMS_INDICES_TYPE),:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT{OBJECT}
      allocate(self(dim1,dim2),stat=status)
      ENSURE(status==0,"MAT{RMS_INDICES}:create_0 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim1,dim2))
#endif
   end subroutine

   PURE subroutine create_1(self,lb1,ub1,lb2,ub2)
   ! Allocate object with the given bounds
      MAT(type(RMS_INDICES_TYPE),:,:), allocatable, OUT :: self
      INT, IN :: lb1,ub1,lb2,ub2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT{OBJECT}
      allocate(self(lb1:ub1,lb2:ub2),stat=status)
      ENSURE(status==0,"MAT{RMS_INDICES}:create_1 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb1:ub1,lb2:ub2))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate the object
      MAT(type(RMS_INDICES_TYPE),:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{OBJECT}
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT{RMS_INDICES}:destroy ... error in allocating memory")
      if (status/=0) stop
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      MAT(type(RMS_INDICES_TYPE),:,:), INOUT :: self
      INT :: i,j

      ! The following code is inherited from MAT{OBJECT}
      do i = 1,size(self,1)
         do j = 1,size(self,2)
            call destroy_ptr_part_(self(i,j))
         end do
      end do
   end subroutine


! ===========
! Set indices
! ===========

   subroutine set_indices(self,l_max,nx,ny,nz,px,py,pz)
   ! Set the rms indices. 
      MAT(type(RMS_INDICES_TYPE),:,:), allocatable, INOUT :: self
      INT, IN :: l_max
      VEC(INT,:), IN :: nx,ny,nz
      VEC(INT,:), IN :: px,py,pz
      INT :: lmax,lmin,f,l
   ENSURE(allocated(self),"MAT{RMS_INDICES}:set_indices ... no self object")
   ENSURE(lbound(self,1)==0,"MAT{RMS_INDICES}:set_indices ... lower bound 1 wrong")
   ENSURE(lbound(self,2)==0,"MAT{RMS_INDICES}:set_indices ... lower bound 2 wrong")
   ENSURE(ubound(self,1)==l_max,"MAT{RMS_INDICES}:set_indices ... upper bound 1 wrong")
   ENSURE(ubound(self,2)==l_max,"MAT{RMS_INDICES}:set_indices ... upper bound 2 wrong")
   ENSURE(size(nx)==n_comp_up_to(2*l_max),"MAT{RMS_INDICES}:set_indices ... wrong size, nx")
   ENSURE(size(ny)==size(nx),"MAT{RMS_INDICES}:set_indices ... wrong size, ny")
   ENSURE(size(nz)==size(nx),"MAT{RMS_INDICES}:set_indices ... wrong size, nz")
      do lmax = 0,l_max  ! Maximum l for one shell
         do lmin = 0,lmax  ! l for the other shell
            f = n_comp_up_to(lmax-1)+1
            l = n_comp_up_to(lmin+lmax)
            call set_indices_(self(lmax,lmin),nx(f:l),ny(f:l),nz(f:l),px(f:l),py(f:l),pz(f:l))
         end do
      end do
   end subroutine

   pure function n_comp_up_to(l) result (res)
   ! No. of gaussian cartesian component functions up to and including the shell
   ! with momentum "l".
      INT, IN :: l
      INT :: res
      res = (l+1)*(l+2)*(l+3)/6
   end function

end module
