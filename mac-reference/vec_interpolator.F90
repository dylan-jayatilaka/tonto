!---------------------------------------------------------------------------
!
! VEC{INTERPOLATOR}: INTERPOLATOR vectors
!
! Copyright (C) Dylan Jayatilaka, 2002
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
module VEC_INTERPOLATOR_MODULE

#  include "macros"

   include "vec{interpolator}.use"

   implicit none

   include "vec{interpolator}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self,dim)
   ! Allocate vector
      VEC(type(INTERPOLATOR_TYPE),:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{INTERPOLATOR}:create ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{INTERPOLATOR}:create ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate the vector
      VEC(type(INTERPOLATOR_TYPE),:), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{INTERPOLATOR}:destroy ... error in deallocating memory")
      if (status/=0) stop
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      VEC(type(INTERPOLATOR_TYPE),:), INOUT :: self
      INT :: a

      ! The following code is inherited from VEC{OBJECT}
      do a = 1,size(self)
         call destroy_ptr_part_(self(a))
      end do
   end subroutine


!  ====
!  Copy
!  ====

!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self)
   ! Set default values
      VEC(type(INTERPOLATOR_TYPE),:), OUT :: self
      INT :: n

      ! The following code is inherited from VEC{OBJECT}
      do n = 1,size(self)
         call set_defaults_(self(n))
      end do
   end subroutine


!  ===========================
!  Non-list based I/O routines
!  ===========================

   subroutine read_keywords(self)
   ! Read in and process normal (non list-type) keywords from "stdin".
      VEC(type(INTERPOLATOR_TYPE),:), PTR :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","VEC{INTERPOLATOR}:read_keywords ... expecting open bracket symbol, {")

      ! Read the opening brace

      ! The following code is inherited from VEC{OBJECT}
      call read_(stdin,word)

      ! Echo the brace if requested
      if (tonto%keyword_echo) then
         call flush_(stdout)
         call text_(stdout,"keyword found --> "//trim(word))
      end if
      do  ! Loop over keywords within brace

         ! Read a keyword
         call read_(stdin,word)

         ! Unexpected end of file?
         DIE_IF(reverted_(stdin),"VEC{INTERPOLATOR}:read_keywords ... unexpected end of file while reading list keywords")

         ! Lower case word
         call to_lower_case_(word)

         ! Is there an equals next? Then join it on
         if (NOT exhausted_(stdin%buffer)) then
            if (next_item_(stdin)=="=") then
               call skip_next_item_(stdin)
               word = trim(word)//"="
            end if
         end if

         ! Echo the keyword if requested
         if (tonto%keyword_echo) then
            call flush_(stdout)
            call text_(stdout,"keyword found --> "//trim(word))
         end if

         ! Exit if it is a close brace
         if (word=="}") exit

         ! Process the keyword
         call process_keyword_(self,word)
      end do
   end subroutine

   subroutine process_keyword(self,keyword)
   ! Process a normal (non list-type) "keyword".
      VEC(type(INTERPOLATOR_TYPE),:), PTR :: self
      STR(len=*) :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}")  ! do nothing.
      case ("redirect          "); call redirect_(self)
      case ("revert            "); call revert_(self)
      case default; allocate(tonto%known_keywords(3))
      tonto%known_keywords(1) = "}"
      tonto%known_keywords(2) = "redirect          "
      tonto%known_keywords(3) = "revert            "
      call unknown_(tonto,word,"VEC{INTERPOLATOR}:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine redirect(self)
   ! Redirect input
      VEC(type(INTERPOLATOR_TYPE),:), PTR :: self
      STR(len=STR_SIZE) :: filename

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      filename = next_str_(stdin)
      call redirect_(stdin,trim(filename))
   end subroutine

   subroutine revert(self)
   ! Revert back to previous stdin file
      VEC(type(INTERPOLATOR_TYPE),:), PTR :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call revert_(stdin)
   end subroutine

end module
