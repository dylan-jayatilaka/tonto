!====================================================================
!
! OPVECTOR: 
!
! Diagonals of operator matrices, e.g. eigenvalues an occupation
! numbers
!
! Provide a basis set (matrix) representation of the diagonal of a
! one-electron quantum mechanical operator. Can cope with polymorphic
! types of basis representations, including restricted, unrestricted,
! and general basis orbitals. Complex types aren't needed sice
! operators are hermitian.
!
! Copyright (C) Dylan Jayatilaka, 1998
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
module OPVECTOR_MODULE

#  include "macros"

   include "opvector.use"

   implicit none

   include "opvector.int"

   VEC(STR(len=1),3) :: allowed_genres
   data allowed_genres/"r","u","g"/

   ! Is this below needed?
! allowed_extended_kinds :: VEC{STR}(len=2,3)
! data allowed_extended_kinds/"r ","u ","gc"/

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self)
   ! Allocate an object
      type(OPVECTOR_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"OPVECTOR:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine create_1(self,n_bf)
   ! Create an empty opvector with size "n_bf" spatial orbitals
      type(OPVECTOR_TYPE), allocatable, OUT :: self
      INT, IN :: n_bf
      call create_(self)
      self%n_bf = n_bf
   end subroutine

   PURE subroutine create_2(self,genre)
   ! Create the spinorbital "genre" of right size
      type(OPVECTOR_TYPE), INOUT :: self
      STR(len=*), IN :: genre
   ENSURE(self%n_bf>0,"OPVECTOR:create_2 ... bad number of basis functions.")
   ENSURE(is_one_of_(genre,allowed_genres),"OPVECTOR:create_2 ... unknown genre, "//trim(genre))
      select case (genre)
      case ("r"); call create_(self%r,self%n_bf)
      case ("u"); call create_(self%a,self%n_bf)
         call create_(self%b,self%n_bf)
      case ("g"); call create_(self%g,2*self%n_bf)
      end select
   end subroutine

   PURE subroutine create_3(self,n_bf,genre)
   ! Create an opvector of size "n_bf", and "genre".
      type(OPVECTOR_TYPE), allocatable, OUT :: self
      INT, IN :: n_bf
      STR(len=*), IN :: genre
      call create_(self,n_bf)
      call create_(self,genre)
   end subroutine

   PURE subroutine destroy_0(self)
   ! Deallocate "self"
      type(OPVECTOR_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"OPVECTOR:destroy_0 ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_1(self,genre)
   ! Destroy the appropriate spinorbital kind of the opmatrix
      type(OPVECTOR_TYPE), INOUT :: self
      STR(len=*), IN :: genre
   ENSURE(is_one_of_(genre,allowed_genres),"OPVECTOR:destroy_1 ... unknown genre, "//trim(genre))
      select case (genre)
      case ("r"); call destroy_(self%r)
      case ("u"); call destroy_(self%a); call destroy_(self%b)
      case ("g"); call destroy_(self%g)
      end select
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of the opmatrix object
      type(OPVECTOR_TYPE), INOUT :: self
      call destroy_(self%r)
      call destroy_(self%a)
      call destroy_(self%b)
      call destroy_(self%g)
   end subroutine


!  ===========
!  Set methods
!  ===========

   pure subroutine set_defaults(self)
   ! Set defaults
      type(OPVECTOR_TYPE), INOUT :: self
      self%n_bf = 0
   end subroutine


!   set(n_bf) :: private, pure
!   ! Set with "n_bf" spatial orbitals
!      self :: INOUT
!      n_bf :: INT, IN
!      .n_bf = n_bf
!   end

   pure subroutine set_to_zero(self)
   ! Set self to "v".
      type(OPVECTOR_TYPE), INOUT :: self
      if (allocated(self%r)) self%r = ZERO
      if (allocated(self%a)) self%a = ZERO
      if (allocated(self%b)) self%b = ZERO
      if (allocated(self%g)) self%g = ZERO
   end subroutine


!  =======
!  Inquiry
!  =======

   pure function genre(self) result (res)
   ! Return the kind of spinorbitals used in the representation.
   ! The simplest spinorbital kind in use is the one returned.
      type(OPVECTOR_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      res = " "
      if (allocated(self%r)) then; res = "r"
      else if (allocated(self%a) AND allocated(self%b)) then; res = "u"
      else if (allocated(self%g)) then; res = "g"
      end if
   end function

   pure function has_any_genre(self) result (res)
   ! Returns true if any component of self has been created
      type(OPVECTOR_TYPE), IN :: self
      BIN :: res
      VEC(BIN,3) :: c
      c(1) = allocated(self%r)
      c(2) = allocated(self%a) AND allocated(self%b)
      c(3) = allocated(self%g)
      res = any(c)
   end function

   PURE function is_allocated_with_genre(self,genre) result (res)
   ! Returns FALSE if self is not allocated
   ! Returns FALSE, unless "genre" of self has been created.
      type(OPVECTOR_TYPE), allocatable, IN :: self
      STR(len=*), IN :: genre
      BIN :: res
      if (NOT allocated(self)) then; res = FALSE
      else; res = has_genre_(self,genre)
      end if
   end function

   pure function is_deallocated_or_no_genre(self) result (res)
   ! Returns true if self is deallocated OR has all components of
   ! self have been deallocated.
      type(OPVECTOR_TYPE), allocatable, IN :: self
      BIN :: res
      VEC(BIN,3) :: d
      if (NOT allocated(self)) then
         res = TRUE
      else
         d(1) = NOT allocated(self%r)
         d(2) = NOT allocated(self%a) AND NOT allocated(self%b)
         d(3) = NOT allocated(self%g)
         res = all(d)
      end if
   end function

   PURE function has_genre(self,genre) result (res)
   ! Returns true if subkind "genre" of self has been created
      type(OPVECTOR_TYPE), IN :: self
      STR(len=*), IN :: genre
      BIN :: res
   ENSURE(is_one_of_(genre,allowed_genres),"OPVECTOR:has_genre ... unknown genre, "//trim(genre))
      res = FALSE
      select case (genre)
      case ("r"); res = allocated(self%r)
      case ("u"); res = allocated(self%a) AND allocated(self%b)
      case ("g"); res = allocated(self%g)
      end select
   end function

   PURE function no_of_occupied(self,genre,tol) result (res)
   ! Returns the number of non-zero "occupied" elements, i.e. all those elements
   ! greater than TOL(7).  If "genre" is present, the number of occupied
   ! elements is returned for that kind (the default is determined by the
   ! .genre). if "tol" is present, it is used instead of TOL(7) to
   ! determine what is occupied.
      type(OPVECTOR_TYPE), IN :: self
      STR(len=*), optional, IN :: genre
      REAL, optional, IN :: tol
      INT :: res
      STR(len=STR_SIZE) :: itemkind
      REAL :: eps
      itemkind = genre_(self)
      if (present(genre)) itemkind = genre
      ENSURE(is_one_of_(itemkind,allowed_genres),"OPVECTOR:no_of_occupied ... unknown kind, "//trim(itemkind))
      eps = TOL(7)
      if (present(tol)) eps = tol
      res = 0
      select case (itemkind)

      case ("r")
         ENSURE(allocated(self%r),"OPVECTOR:no_of_occupied ... no restricted part")
         res = count(self%r>=eps)

      case ("u")
         ENSURE(allocated(self%a),"OPVECTOR:no_of_occupied ... no alpha part")
         ENSURE(allocated(self%b),"OPVECTOR:no_of_occupied ... no beta part")
         res = count(self%a>=eps)+count(self%b>=eps)
      end select
   end function


!  ==========
!  Conversion
!  ==========

   subroutine convert_to(self,new)
   ! Convert "self" to a "new" genre, and destroy the old genre.
      type(OPVECTOR_TYPE), INOUT :: self
      STR(len=*), IN :: new
      BIN :: fail
      STR(len=STR_SIZE) :: old
   ENSURE(is_one_of_(new,allowed_genres),"OPVECTOR:convert_to ... unknown genre, "//trim(new))

      ! Done?
      old = genre_(self)
      if (new==old) return
      call create_(self,genre=new)

      ! Convert old -> new
      fail = FALSE
      select case (old)

      case ("r")
         select case (new)
         case ("u")
            self%a = self%r
            self%b = self%r
         case default
            fail = TRUE
         end select

      case ("u")
         select case (new)
         case ("r")
            self%r = self%a  ! take alpha part
         case default
            fail = TRUE
         end select

      case ("g")
         select case (new)
         case ("u")
            call put_a_block_to_(self%g,self%a)
            call put_b_block_to_(self%g,self%b)
         case default
            fail = TRUE
         end select
      end select

      ! Clean
      call destroy_(self,old)
      if (fail) then
         DIE("OPVECTOR:convert_to ... can't convert genre "//trim(old)//" to genre "//trim(new))
      end if
   end subroutine

end module
