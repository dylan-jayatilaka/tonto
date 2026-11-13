!===================================================================
! MAT{SHELL1PAIR} : a vector of pairs of gaussian SHELL1PAIR's.
!
! This object is used to store precalculated information used in 
! ERI integral evaluation.
!
! Copyright (C) Dylan Jayatilaka, 2000
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
module MAT_SHELL1PAIR_MODULE

#  include "macros"

   include "mat{shell1pair}.use"

   implicit none

   include "mat{shell1pair}.int"


contains

!  =================
!  Memory allocation
!  =================

   PURE subroutine create(self,dim1,dim2)
   ! Allocate object
      MAT(type(SHELL1PAIR_TYPE),:,:), allocatable, OUT :: self
      INT, IN :: dim1,dim2

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from MAT{OBJECT}
      allocate(self(dim1,dim2),stat=status)
      ENSURE(status==0,"MAT{SHELL1PAIR}:create ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim1,dim2))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate the object
      MAT(type(SHELL1PAIR_TYPE),:,:), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from MAT{OBJECT}
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAT{SHELL1PAIR}:destroy ... error in allocating memory")
      if (status/=0) stop
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      MAT(type(SHELL1PAIR_TYPE),:,:), INOUT :: self
      INT :: i,j

      ! The following code is inherited from MAT{OBJECT}
      do i = 1,size(self,1)
         do j = 1,size(self,2)
            call destroy_ptr_part_(self(i,j))
         end do
      end do
   end subroutine


!  ============================
!  Size-of and shape operations 
!  ============================

   pure function is_same_shape_as(self,a) result (res)
   ! Returns TRUE if the matrix "a" has the same shape as "self"
      MAT(type(SHELL1PAIR_TYPE),:,:), IN :: self
      MAT(type(SHELL1PAIR_TYPE),:,:), IN :: a
      BIN :: res

      ! The following code is inherited from MAT{OBJECT}
      res = size(self,1)==size(a,1) AND size(self,2)==size(a,2)
   end function

   pure function is_transposed_shape_of(self,a) result (res)
   ! Returns TRUE if the matrix "a" is the transposed shape of self
      MAT(type(SHELL1PAIR_TYPE),:,:), IN :: self
      MAT(type(SHELL1PAIR_TYPE),:,:), IN :: a
      BIN :: res

      ! The following code is inherited from MAT{OBJECT}
      res = size(self,1)==size(a,2) AND size(self,2)==size(a,1)
   end function

   pure function is_square(self) result (res)
   ! Returns TRUE if the matrix is square
      MAT(type(SHELL1PAIR_TYPE),:,:), IN :: self
      BIN :: res

      ! The following code is inherited from MAT{OBJECT}
      res = size(self,1)==size(self,2)
   end function


!  ==============================
!  Make precomputed integral data
!  ==============================

   PURE subroutine make_precomputed_shellpairs(self,basis)
   ! Make the SHELL1PAIR matrix "self" which includes non-positional
   ! precomputed data for the "basis" set needed for gaussian ERI
   ! integrals.
      MAT(type(SHELL1PAIR_TYPE),:,:), OUT :: self
      VEC(type(BASIS_TYPE),:), IN :: basis
      INT :: i,j,a,b,p,q

      ! Loop over bases "p"
      p = 0
      do i = 1,size(basis)
         do a = 1,basis(i)%n_shell
            p = p+1

         ! Loop over bases "q"
            q = 0
            do j = 1,size(basis)
               do b = 1,basis(j)%n_shell
                  q = q+1

            ! Set the shells; positions not set
                  call set_a_(self(p,q),basis(i)%shell(a))
                  call set_b_(self(p,q),basis(j)%shell(b))

            ! Precalculate non-positional info
                  call make_precalculated_data_(self(p,q))
               end do
            end do
         end do
      end do
   end subroutine


!  ======
!  Output
!  ======

   subroutine dump(self,object_name,lower_triangle)
   ! Dump a matrix object in text format
      MAT(type(SHELL1PAIR_TYPE),:,:), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: lower_triangle
      STR(len=STR_SIZE) :: element
      BIN :: lower
      INT :: i,j

      ! The following code is inherited from MAT{OBJECT}
      lower = FALSE
      if (present(lower_triangle)) lower = lower_triangle
      call text_(stdout,trim(object_name)//":MAT{TYPE} = {")
      call increment_margin_width_(stdout,1)
      call put_(stdout,"dimensions =",width=11)
      call put_(stdout,size(self,1),width=0)
      call put_(stdout,size(self,2),width=0)
      call flush_(stdout)
      if (NOT lower) then
         do i = 1,size(self,1)
            do j = 1,size(self,2)
               element = trim(object_name)//"("//trim(to_str_(i))//","//trim(to_str_(j))//")"
               call dump_(self(i,j),trim(element))
            end do
         end do
      else
         do i = 1,size(self,1)
            do j = 1,i
               element = trim(object_name)//"("//trim(to_str_(i))//","//trim(to_str_(j))//")"
               call dump_(self(i,j),trim(element))
            end do
         end do
      end if
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name,lower_triangle)
   ! Dump an allocatable matrix object in text format
      MAT(type(SHELL1PAIR_TYPE),:,:), allocatable, IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: lower_triangle
      STR(len=STR_SIZE) :: element
      BIN :: lower
      INT :: i,j

      ! The following code is inherited from MAT{OBJECT}
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":MAT{TYPE}* = null")
      else
         lower = FALSE
         if (present(lower_triangle)) lower = lower_triangle
         call text_(stdout,trim(object_name)//":MAT{TYPE} = {")
         call increment_margin_width_(stdout,1)
         call put_(stdout,"dimensions =",width=11)
         call put_(stdout,size(self,1),width=0)
         call put_(stdout,size(self,2),width=0)
         call flush_(stdout)
         if (NOT lower) then
            do i = 1,size(self,1)
               do j = 1,size(self,2)
                  element = trim(object_name)//"("//trim(to_str_(i))//","//trim(to_str_(j))//")"
                  call dump_(self(i,j),trim(element))
               end do
            end do
         else
            do i = 1,size(self,1)
               do j = 1,i
                  call flush_(stdout)
                  element = trim(object_name)//"("//trim(to_str_(i))//","//trim(to_str_(j))//")"
                  call dump_(self(i,j),trim(element))
               end do
            end do
         end if
         call increment_margin_width_(stdout,-1)
         call text_(stdout,"}")
      end if
   end subroutine

end module
