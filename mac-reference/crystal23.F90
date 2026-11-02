!===================================================================
!
! CRYSTAL23: Data structure for storing essential CRYSTAL23
!            calculation results.
!
! Copyright (C) Dylan Jayatilaka, Kanghyun Chu, 2023
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
!===================================================================
module CRYSTAL23_MODULE

#  include "macros"

   include "crystal23.use"

   implicit none

   include "crystal23.int"

   MAT3(REAL,3,3,7), private :: WC23_P_to_X
   data WC23_P_to_X/ &
         ONE,      ZERO,      ZERO, &  ! P -> P
         ZERO,       ONE,      ZERO, &  !
         ZERO,      ZERO,       ONE, &  !
         ONE,      ZERO,      ZERO, &  ! P -> A
         ZERO,      HALF,      HALF, &  !
         ZERO,     -HALF,      HALF, &  !
         HALF,      ZERO,     -HALF, &  ! P -> B
         ZERO,       ONE,      ZERO, &  !
         HALF,      ZERO,      HALF, &  !
         HALF,      HALF,      ZERO, &  ! P -> C
      -HALF,      HALF,      ZERO, &  !
         ZERO,      ZERO,       ONE, &  !
      -HALF,      HALF,      HALF, &  ! P -> I
         HALF,     -HALF,      HALF, &  !
         HALF,      HALF,     -HALF, &  !
   TWOTHIRD,     THIRD,     THIRD, &  ! R -> H
      -THIRD,     THIRD,     THIRD, &  !
      -THIRD, -TWOTHIRD,     THIRD, &  !
         ZERO,      HALF,      HALF, &  ! P -> F
         HALF,      ZERO,      HALF, &  !
         HALF,      HALF,      ZERO/

contains

!  ===========
!  Allocatiion
!  ===========

   PURE subroutine create(self)
   ! Allocate an object
      type(CRYSTAL23_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"CRYSTAL23:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(CRYSTAL23_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"CRYSTAL23:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine WC23_to(lattice,W)
   ! Set the international table symbol to be "IT_symbol"
      STR(len=*), IN :: lattice
      MAT(REAL,3,3), OUT :: W
      STR(len=STR_SIZE) :: latt
      latt = lattice
      call to_upper_case_(latt)
      W = ZERO
      select case (latt)
      case ("P"); W = WC23_P_to_X(:,:,1)
      case ("A"); W = WC23_P_to_X(:,:,2)
      case ("B"); W = WC23_P_to_X(:,:,3)
      case ("C"); W = WC23_P_to_X(:,:,4)
      case ("I"); W = WC23_P_to_X(:,:,5)
      case ("R"); W = WC23_P_to_X(:,:,6)  ! R -> H
      case ("F"); W = WC23_P_to_X(:,:,7)
      end select
      ENSURE(NOT is_zero_(W),"CRYSTAL23:WC23_to ... Lattice system not found")
   end subroutine

end module
