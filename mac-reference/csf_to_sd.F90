!==================================================================
!
! CSF_TO_SD
!
! For converting CSFs to Slater determinants and vice versa.
! Based on J. Olsen (2014) JCP 141 p 034112.
!
! Copyright (C) Dylan Jayatilaka (2025)
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
module CSF_TO_SD_MODULE

#  include "macros"

   include "csf_to_sd.use"

   implicit none

   include "csf_to_sd.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(CSF_TO_SD_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"CSF_TO_SD:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(CSF_TO_SD_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"CSF_TO_SD:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      type(CSF_TO_SD_TYPE), INOUT :: self
      call destroy_(self%CSF_graph)
      call destroy_(self%SD_graph)
   end subroutine


!  ===========================
!  Set defaults, reset, update
!  ===========================

   PURE subroutine update(self)
   ! Set default SCF data values
      type(CSF_TO_SD_TYPE), INOUT :: self
      INT :: n
   ENSURE(self%n_e>1,"CSF_TO_SD:update ... n_e must be > 1")
      n = self%n_e
      call create_(self%CSF_graph,0,n,0,n)
      call create_(self%SD_graph,0,n,0,n)
      call make_graph_(self,self%CSF_graph)
   end subroutine

   PURE subroutine set_defaults(self)
   ! Set default SCF data values
      type(CSF_TO_SD_TYPE), INOUT :: self
   end subroutine

   subroutine set_n_e(self,val)
   ! Set number of electrons.
      type(CSF_TO_SD_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>0,"CSF_TO_SD:set_n_e ... n_evals must be +ve")
      self%n_e = val
   end subroutine

   PURE subroutine set_n_up(self,val)
   ! Set the number of "up couplings"
      type(CSF_TO_SD_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>0,"CSF_TO_SD:set_n_up ... n_keep must be +ve")
      self%n_up = val
   end subroutine


!  =============
!  Input methods
!  =============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(CSF_TO_SD_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","CSF_TO_SD:read_keywords ... expecting open bracket symbol, {")

      ! Read the open braces {

      ! The following code is inherited from OBJECT
      call read_(stdin,word)

      ! Echo keyword if requested
      if (tonto%keyword_echo) then
         call flush_(stdout)
         call text_(stdout,"keyword found --> "//trim(word))
      end if
      do  ! Loop over input keywords

         ! Read the keyword
         call read_(stdin,word)
         DIE_IF(reverted_(stdin),"CSF_TO_SD:read_keywords ... unexpected end of file while reading list keywords")
         call to_lower_case_(word)

         ! Append any lonely = sign
         if (NOT exhausted_(stdin%buffer)) then
            if (next_item_(stdin)=="=") then
               call skip_next_item_(stdin)
               word = trim(word)//"="
            end if
         end if

         ! Echo keyword if requested
         if (tonto%keyword_echo) then
            call flush_(stdout)
            call text_(stdout,"keyword found --> "//trim(word))
         end if

         ! Exit if keyword is closing brace }
         if (word=="}") exit

         ! Process the keyword 
         call process_keyword_(self,word)
      end do
   end subroutine

   subroutine process_keyword(self,keyword)
   ! Process a command "keyword". Data is inputted from "stdin", unless
   ! "word" is a sequence of blank separated strings. In this case,
   ! the sequence is processed as if it were a separate file.
      type(CSF_TO_SD_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      BIN :: die
      word = keyword
      call to_lower_case_(word)
      select case (word)

      case ("n_e=    "); call read_n_e_(self)
      case ("n_up=   "); call read_n_up_(self)
      case default; allocate(tonto%known_keywords(2))
      tonto%known_keywords(1) = "n_e=    "
      tonto%known_keywords(2) = "n_up=   "
      call unknown_(tonto,word,"CSF_TO_SD:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine docu_help()
   ! Documentation.
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"==================")
      call text_(stdout,"Help for CSF_TO_SD")
      call text_(stdout,"==================")
      DIE("CSF_TO_SD:docu_help ... end of help documentation")
   end subroutine


! N

   subroutine docu_n_e(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"n_e= INT    [0]")
      call flush_(stdout)
      call text_(stdout,". The number of electrons in the CSF or Slater determinant")
      if (present(die)) stop
   end subroutine

   subroutine read_n_e(self)
   ! Generic read and set
      type(CSF_TO_SD_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_n_e_(self,val)
   end subroutine

   subroutine docu_n_up(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"n_up= INT    [0]")
      call flush_(stdout)
      call text_(stdout,". The number of 'up coupled' electrons in the CSF")
      if (present(die)) stop
   end subroutine

   subroutine read_n_up(self)
   ! Generic read and set
      type(CSF_TO_SD_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_n_up_(self,val)
   end subroutine


!  ===============
!  Make the graphs
!  ===============

   PURE subroutine make_graph(self,G)
   ! Make Olsen's CSF graph "G".
   ! Note: is zero- dimensioned
      type(CSF_TO_SD_TYPE), INOUT :: self
      MAT(INT,:,:), allocatable, INOUT :: G
      INT :: n
      n = self%n_e
      G = 0
   end subroutine

end module
