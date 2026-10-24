!=====================================================================
!
! ERI_SCRATCH :
!
! Work arrays for the two-electron integral quartet loop. They are
! sized once, before the loop, for the largest shell quartet the basis
! can form, so that the loop itself does not allocate.
!
! Copyright (C) Dylan Jayatilaka, 2026
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
!=====================================================================
module ERI_SCRATCH_MODULE

#  include "macros"

   include "eri_scratch.use"

   implicit none

   include "eri_scratch.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(ERI_SCRATCH_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"ERI_SCRATCH:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(ERI_SCRATCH_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"ERI_SCRATCH:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  =====
!  Sizes
!  =====

   PURE subroutine set_sizes_for(self,pair)
   ! Size the work arrays for the largest shell quartet that the
   ! precomputed shell pairs "pair" can form: a dry run before the quartet
   ! loop. Primitive-pair screening only shrinks a quartet, so these are
   ! upper bounds.
      type(ERI_SCRATCH_TYPE), INOUT :: self
      MAT(type(SHELL1PAIR_TYPE),:,:), IN :: pair
      VEC(INT,17) :: n_prim_l
      INT :: i,j,l,l1,l2,n,n_2d,n_gauss,n_comp

      ! Largest (primitive pairs)*(l_sum+1) for each pair l_sum
      n_prim_l = 0
      n_gauss = 0
      n_comp = 0
      do j = 1,size(pair,2)
         do i = 1,size(pair,1)
            l = pair(i,j)%l_sum
            ENSURE(l<=16,"ERI_SCRATCH:set_sizes_for ... pair l_sum too large")
            n = pair(i,j)%n_gaussian_pairs*(l+1)
            n_prim_l(l+1) = max(n_prim_l(l+1),n)
            n_gauss = max(n_gauss,pair(i,j)%n_gaussian)
            n_comp = max(n_comp,pair(i,j)%n_comp_pairs)
         end do
      end do

      ! 2D integrals: n_roots * (ab prims)*(ab dim) * (cd prims)*(cd dim)
      n_2d = 0
      do l2 = 0,16
         do l1 = 0,16
            n = ((l1+l2+2)/2)*n_prim_l(l1+1)*n_prim_l(l2+1)
            n_2d = max(n_2d,n)
         end do
      end do
      call grow_to_(self,n_2d,n_gauss,n_gauss*n_gauss,n_gauss*n_comp,n_comp*n_comp)
      call grow_abcd_to_(self,n_comp*n_comp)
   end subroutine

   PURE subroutine grow_to(self,n_2d,n_ab,n_esfs,n_escd,n_ints)
   ! Grow the work arrays, where needed, to at least these lengths. They
   ! never shrink.
      type(ERI_SCRATCH_TYPE), INOUT :: self
      INT, IN :: n_2d,n_ab,n_esfs,n_escd,n_ints
      call grow(self%Ix,n_2d)
      call grow(self%Iy,n_2d)
      call grow(self%Iz,n_2d)
      call grow(self%esfs,n_esfs)
      call grow(self%escd,n_escd)
      call grow(self%ints,n_ints)
      call grow(self%ev,n_ab)
      call grow(self%fv,n_ab)
   end subroutine

   PURE subroutine grow_abcd_to(self,n)
   ! Grow the make_ERI output block, where needed, to at least length "n".
      type(ERI_SCRATCH_TYPE), INOUT :: self
      INT, IN :: n
      call grow(self%abcd,n)
   end subroutine

   PURE subroutine grow(v,n)
   ! Reallocate "v" to length "n" only if it is shorter.
      VEC(REAL,:), allocatable, INOUT :: v
      INT, IN :: n
      if (allocated(v)) then
         if (size(v)>=n) return
      end if
      call destroy_(v)
      call create_(v,n)
   end subroutine

end module
