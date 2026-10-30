!==================================================================
!
! VEC{COPPENSBASIS}: COPPENSBASIS vectors
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
!==================================================================
module VEC_COPPENSBASIS_MODULE

#  include "macros"

   include "vec{coppensbasis}.use"

   implicit none

   include "vec{coppensbasis}.int"

   STR(len=PATH_SIZE), private :: library_directory = VEC_BASIS_LIBRARY_DIRECTORY  ! a path

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim)
   ! Allocate vector
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{COPPENSBASIS}:create_0 ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{COPPENSBASIS}:create_0 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine create_1(self,lb,ub)
   ! Allocate the vector with lower bound "lb", upper bound "ub"
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, OUT :: self
      INT, IN :: lb,ub

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb:ub),stat=status)
      ENSURE(status==0,"VEC{COPPENSBASIS}:create_1 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate the vector
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{COPPENSBASIS}:destroy ... error in deallocating memory")
      if (status/=0) stop
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      VEC(type(COPPENSBASIS_TYPE),:), INOUT :: self
      INT :: a

      ! The following code is inherited from VEC{OBJECT}
      do a = 1,size(self)
         call destroy_ptr_part_(self(a))
      end do
   end subroutine


! ===============================
! Shrinking, expanding, appending
! ===============================

   PURE subroutine shrink(self,dim,chop_start)
   ! Shrink allocatable "self" to dimension "dim" from the end.
   ! Contents are retained. If "chop_start" is present and TRUE,
   ! "self" is chopped from the start, and the contents from the end
   ! are retained.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: chop_start
      VEC(type(COPPENSBASIS_TYPE),:), allocatable :: new
      INT :: lb,ub
      BIN :: chop_end
   ENSURE(allocated(self),"VEC{COPPENSBASIS}:shrink ... no self array")
   ENSURE(dim<=size(self),"VEC{COPPENSBASIS}:shrink ... dim too large")

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
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: grow_start
      INT :: nc
      VEC(type(COPPENSBASIS_TYPE),:), allocatable :: new
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
         ENSURE(dim>size(self),"VEC{COPPENSBASIS}:expand ... dim not large enough")
         ENSURE(dim>0,"VEC{COPPENSBASIS}:expand ... dim is not +ve")
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
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      VEC(type(COPPENSBASIS_TYPE),:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = v
      else
         self = [self,v]
      end if
   end subroutine

   pure subroutine append_1(self,value)
   ! Appends the scalar "value" onto the end of allocatable "self".
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      type(COPPENSBASIS_TYPE), IN :: value

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [value]
      else
         self = [self,value]
      end if
   end subroutine


!  ===
!  Set
!  ===

   subroutine set_library_directory(self,directory)
   ! Set the directory name for where the basis set libraries are stored.
      VEC(type(COPPENSBASIS_TYPE),:), IN :: self
      STR(len=*), IN :: directory

      ! The following code is inherited from VEC{BASIS}
      library_directory = directory
   end subroutine

   function library_file(self,basis_kind) result (res)
   ! Return the basis set library file for a particular "basis_kind".
      VEC(type(COPPENSBASIS_TYPE),:), IN :: self
      STR(len=*), IN :: basis_kind
      STR(len=PATH_SIZE) :: res

      ! The following code is inherited from VEC{BASIS}
      res = trim(library_directory)//"/"//trim(basis_kind)
   end function


!  =======================
!  List-based I/O Routines
!  =======================

   recursive subroutine read_list_keywords(self)
   ! This version is for allocatable arrays.
   ! Read in and process list-based keywords from "stdin". List-based keywords
   ! are those that are intended to apply to each individual element of the list
   ! through a list of "keys" stored in the associated list-element type module.
   ! NOTE: this routine will create the list, if required.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","VEC{COPPENSBASIS}:read_list_keywords ... expecting open bracket symbol, {")

      ! Read the open brace

      ! The following code is inherited from VEC{OBJECT}
      call read_(stdin,word)

      ! Echo the brace if requested
      if (tonto%keyword_echo) then
         call flush_(stdout)
         call text_(stdout,"keyword found --> "//trim(word))
      end if
      do  ! Loop over input list-type keywords

         ! Read a keyword
         call read_(stdin,word)

         ! Unexpected end of file?
         DIE_IF(reverted_(stdin),"VEC{COPPENSBASIS}:read_list_keywords ... unexpected end of file while reading list keywords")

         ! Lower case the word
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

         ! Process the list keyword
         call process_list_keyword_(self,word)
      end do
   end subroutine

   subroutine process_list_keyword(self,keyword)
   ! This version is for allocatable arrays.
   ! Process a list-type "keyword", common to all list-type objects.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word

      ! The following code is inherited from VEC{OBJECT}
      word = keyword
      call to_lower_case_(word)
      select case (word)

      ! Standard list-input keywords
      case ("altered_data=    "); call read_altered_data_(self)
      case ("append_data=     "); call read_append_data_(self)
      case ("data=            "); call read_data_(self)
      case ("do               "); call read_keywords_(self)
      case ("keys=            "); call read_keys_(self)
      case ("new_data=        "); call destroy_(self); call read_data_(self)
      case ("process_keys     "); call process_keys_(self)
      case ("process_keys_once"); call process_keys_once_(self)
      case ("redirect         "); call redirect_(self)
      case ("revert           "); call revert_(self)

      ! Backtrack & assume data= sans braces
      case default
         call move_to_previous_item_(stdin)
         call read_data_sans_braces_(self)
      end select
   end subroutine

   subroutine read_data(self)
   ! This version is for allocatable arrays.
   ! Repeatedly process the "keys" to read in the data list. 
   ! Opening and closing braces are needed.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word

      ! Read open braces

      ! The following code is inherited from VEC{OBJECT}
      call read_(stdin,word)  ! move past open brace
      ENSURE(word=="{","VEC{COPPENSBASIS}:read_data ... expecting open bracket symbol, {")

      ! Read data (process keys)
      call read_data_sans_braces_(self)

      ! Read close brace
      call read_(stdin,word)
      ENSURE(word=="}","VEC{COPPENSBASIS}:read_data ... expecting close bracket symbol, }")
   end subroutine

   subroutine read_data_sans_braces(self)
   ! This version is for allocatable arrays.
   ! Repeatedly process the "keys" to read in the data list. 
   ! Opening and closing braces are not needed.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      INT :: length

      ! Get the vector length

      ! The following code is inherited from VEC{OBJECT}
      length = data_length_(self)
      if (allocated(self)) then

         ! If created, is dimension the same
         ENSURE(length==size(self),"VEC{COPPENSBASIS}:read_data_sans_braces ... Mismatching new and old data lists")
      else

         ! If not created, create it
         call create_(self,length)
      end if

      ! Process keys
      call process_keys_(self)
   end subroutine

   subroutine read_data_BL_terminated(self)
   ! This version is for allocatable arrays.
   ! Repeatedly process the "keys" to read in the data list. Opening
   ! braces are not needed, but a blank line (BL) is used as an
   ! end-of-list terminator.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      INT :: length

      ! Get the vector length

      ! The following code is inherited from VEC{OBJECT}
      length = data_length_BL_terminated_(self)
      if (allocated(self)) then

         ! If created, is dimension the same
         ENSURE(length==size(self),"VEC{COPPENSBASIS}:read_data_BL_terminated ... Mismatching new and old data lists")
      else

         ! If not created, create it
         call create_(self,length)
      end if

      ! Process keys
      call process_keys_(self)

      ! Empty line there? Read it.
      call read_line_(stdin)
      ENSURE(stdin%buffer%string==" ","VEC{COPPENSBASIS}:read_data_BL_terminated ... expecting blank line")
   end subroutine

   function data_length_BL_terminated(self) result (length)
   ! This version is for allocatable arrays.
   ! Read ahead in stdin to get the "length" of the data list. The
   ! data list begins with the first data item, *not* a "{" symbol.
   ! The data list must be terminated by a blank line (BL).
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      INT :: length
      type(COPPENSBASIS_TYPE), allocatable :: tmp
      STR(len=STR_SIZE) :: word
      INT :: line,item
   ENSURE(next_item_(stdin)/="}","VEC{COPPENSBASIS}:data_length_BL_terminated ... empty data list!")

      ! Remove warning

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if

      ! Initialize result
      length = 0

      ! Read first item, initialize line no & item no.
      call read_(stdin,word)
      line = line_number_(stdin)
      item = previous_line_item_(stdin)

      ! Get the length
      do

         ! Move just before
         call move_to_previous_item_(stdin)

         ! Read a scalar & increment
         call create_(tmp)
         call process_keys_(tmp)
         call destroy_(tmp)
         length = length+1

         ! Blank-line terminated?
         call read_line_(stdin)
         if (stdin%buffer%string==" ") exit
         if (at_EOF_(stdin)) exit

         ! No blank line ... read first item
         call read_(stdin,word)
         if (at_EOF_(stdin)) exit  ! Oops
      end do

      ! Go back to start
      call move_to_line_(stdin,line)
      call move_to_line_item_(stdin,item)
   end function

   function data_length(self) result (length)
   ! This version is for allocatable arrays.
   ! Read ahead in stdin to get the "length" of the data list, i.e. the number
   ! of data items in the list. The data must begin with the first data item,
   ! *not* a "{" symbol.  The order of data items comprising the list is given
   ! by keys defined in the associated list-element type module. The data list
   ! must be terminated by a "}" symbol.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      INT :: length
      type(COPPENSBASIS_TYPE), allocatable :: tmp
      STR(len=STR_SIZE) :: word
      INT :: line,item
   ENSURE(next_item_(stdin)/="}","VEC{COPPENSBASIS}:data_length ... empty data list!")

      ! Remove warning

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if

      ! Initialize result
      length = 0

      ! Read first item, initialize line no & item no.
      call read_(stdin,word)
      line = line_number_(stdin)
      item = previous_line_item_(stdin)

      ! Get the length
      do

         ! Move just before
         call move_to_previous_item_(stdin)

         ! Read a scalar & increment
         call create_(tmp)
         call process_keys_(tmp)
         call destroy_(tmp)
         length = length+1

         ! Closing-brace there?
         call read_(stdin,word)
         call to_lower_case_(word)
         if (word=="}") exit
         if (at_EOF_(stdin)) exit
      end do

      ! Go back to start
      call move_to_line_(stdin,line)
      call move_to_line_item_(stdin,item)
   end function

   subroutine read_altered_data(self)
   ! This version is for allocatable arrays.
   ! Read in a sublist of the complete list, and alter the data for that
   ! sublist.  The order of the data items in the sublist is given by the "keys"
   ! defined in the associated list-element type module.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word
      INT :: s
   ENSURE(allocated(self),"VEC{COPPENSBASIS}:read_altered_data ... list does not exist yet")
   ENSURE(next_item_(stdin)=="{","VEC{COPPENSBASIS}:read_altered_data ... expecting open bracket symbol: {")

      ! Read brace {

      ! The following code is inherited from VEC{OBJECT}
      call read_(stdin,word)
      do

         ! Read index
         call read_(stdin,word)

         ! End of indices
         if (word=="}") exit

         ! Get index
         ENSURE(is_int_(word),"VEC{COPPENSBASIS}:read_altered_data ... expecting integer list-element index")
         s = to_int_(word)
         ENSURE(s<=size(self),"VEC{COPPENSBASIS}:read_altered_data ... list-element too large")
         ENSURE(s>0,"VEC{COPPENSBASIS}:read_altered_data ... list-element must be positive")

         ! Altered data for this index
         call process_keys_(self(s))
      end do
   end subroutine

   subroutine read_append_data(self)
   ! This version is for allocatable arrays.
   ! Read in a set of data to append to an existing set.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      VEC(type(COPPENSBASIS_TYPE),:), allocatable :: tmp
   ENSURE(allocated(self),"VEC{COPPENSBASIS}:read_append_data ... list does not exist yet")
   ENSURE(next_item_(stdin)=="{","VEC{COPPENSBASIS}:read_append_data ... expecting open bracket symbol: {")

      ! Read data

      ! The following code is inherited from VEC{OBJECT}
      call read_data_(tmp)

      ! Append it
      call append_(self,tmp)

      ! Clean
      call destroy_(tmp)
   end subroutine

   subroutine process_keys(self)
   ! This version is for allocatable arrays.
   ! Process the "keys" on each element of the list.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      INT :: s

      ! The following code is inherited from VEC{OBJECT}
      if (allocated(self)) then

         ! Process keys on each element
         do s = 1,size(self)
            call process_keys_(self(s))
         end do
      else

         ! Process keys once on anonymous element
         call process_keys_once_(self)
      end if
   end subroutine

   subroutine process_keys_once(self)
   ! This version is for allocatable arrays.
   ! Process the "keys" just once on an anonymous object. This is useful if the
   ! "keys" set global variables in the list module (e.g. the default order of
   ! lists *within* the list data) as opposed to being keys pertaining to each
   ! element of the list.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      type(COPPENSBASIS_TYPE) :: tmp

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if
      call process_keys_(tmp)
   end subroutine

   function keys_created(self) result (res)
   ! This version is for allocatable arrays.
   ! Return TRUE if the list-element keys are created.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      BIN :: res
      type(COPPENSBASIS_TYPE), allocatable :: tmp

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if
      call create_(tmp)
      res = keys_created_(tmp)
      call destroy_(tmp)
   end function

   subroutine set_keys(self,the_keys)
   ! This version is for allocatable arrays.
   ! This is for setting the "keys" externally.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      VEC(STR(len=*),:) :: the_keys
      type(COPPENSBASIS_TYPE), allocatable :: tmp

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if
      call create_(tmp)
      call set_keys_(tmp,the_keys)
      call destroy_(tmp)
   end subroutine

   subroutine clear_keys(self)
   ! This version is for allocatable arrays.
   ! This is for destroying the "keys" externally.
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      type(COPPENSBASIS_TYPE), allocatable :: tmp

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if
      call create_(tmp)
      call clear_keys_(tmp)
      call destroy_(tmp)
   end subroutine

   subroutine read_keys(self)
   ! This version is for allocatable arrays.
   ! Read a new set of keys
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      type(COPPENSBASIS_TYPE), allocatable :: tmp

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if
      call create_(tmp)
      call read_keys_(tmp)
      call destroy_(tmp)
   end subroutine

   subroutine redirect(self)
   ! Redirect input. (This code is for non-pointer self objects).
      VEC(type(COPPENSBASIS_TYPE),:), INOUT :: self
      STR(len=STR_SIZE) :: filename

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      filename = next_str_(stdin)
      call redirect_(stdin,trim(filename))
   end subroutine

   subroutine revert(self)
   ! Revert back to previous stdin file. (This code is for non-pointer self
   ! objects).
      VEC(type(COPPENSBASIS_TYPE),:), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call revert_(stdin)
   end subroutine


!  ===========================
!  Non-list based I/O routines
!  ===========================

   subroutine read_keywords(self)
   ! This version is for allocatable arrays.
   ! Read in and process normal (non list-type) keywords from "stdin".
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","VEC{COPPENSBASIS}:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"VEC{COPPENSBASIS}:read_keywords ... unexpected end of file while reading list keywords")

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
      VEC(type(COPPENSBASIS_TYPE),:), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                 ")  ! do nothing.
      case ("library_directory="); call read_library_directory_(self)
      case ("put               "); call put_(self)
      case ("redirect          "); call redirect_(self)
      case ("revert            "); call revert_(self)
      case default; allocate(tonto%known_keywords(5))
      tonto%known_keywords(1) = "}                 "
      tonto%known_keywords(2) = "library_directory="
      tonto%known_keywords(3) = "put               "
      tonto%known_keywords(4) = "redirect          "
      tonto%known_keywords(5) = "revert            "
      call unknown_(tonto,word,"VEC{COPPENSBASIS}:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_library_directory(self)
   ! Read the directory name for where the basis set libraries are stored.
      VEC(type(COPPENSBASIS_TYPE),:), IN :: self

      ! The following code is inherited from VEC{BASIS}
      call read_(stdin,library_directory)
   end subroutine

   subroutine read_library_data(self,library,labels,n_unique_labels)
   ! Open the file named "library", read the basis sets which have the right
   ! "labels", and assign them to the basis vector.  The file must begin with a
   ! "{" symbol and must have at least one "data=" list-type keyword following
   ! the open bracket. NOTE: Self is created or expanded by the number of unique
   ! labels in "labels" -- those which cannot be found already in "self".
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, INOUT :: self
      STR(len=*), IN :: library
      VEC(STR(len=*),:), IN :: labels
      INT, optional, OUT :: n_unique_labels
      VEC(STR(len=STR_SIZE),:), allocatable :: unique_labels
      BIN :: echo,stdin_created
      STR(len=STR_SIZE) :: word
      STR(len=PATH_SIZE) :: lib  ! a path
      type(COPPENSBASIS_TYPE), allocatable :: basis
      INT :: found,i,u,n_labels,N
      type(TEXTFILE_TYPE) :: file

      ! The following code is inherited from VEC{BASIS}
      if (NOT allocated(self)) then

         ! All labels are unique
         N = 0
         n_labels = size(labels)
         unique_labels = labels
         if (n_labels>0) call create_(self,n_labels)
      else

         ! Find and add only unique labels
         N = size(self)
         n_labels = size(labels)
         call create_(unique_labels,n_labels)
         u = 0
         do i = 1,n_labels
            if (any(self(:)%label==labels(i))) cycle
            u = u+1
            unique_labels(u) = labels(i)
         end do
         n_labels = u
         call shrink_(unique_labels,n_labels)
         if (n_labels>0) call expand_(self,N+n_labels)
      end if
      if (present(n_unique_labels)) n_unique_labels = n_labels
      if (n_labels==0) then
         call destroy_(unique_labels)
         return
      end if
      echo = tonto%keyword_echo
      if (echo) call set_keyword_echo_(tonto,FALSE)

      ! Get basis library directory; default: "./basis_sets"
      lib = library
      if (NOT exists_(file,lib)) lib = "./basis_sets/"//trim(filename_head_(library))
      DIE_IF(NOT exists_(file,lib),"VEC{COPPENSBASIS}:read_library_data ... no library basis set")

      ! Redirect/open stdin
      stdin_created = allocated(stdin)
      if (stdin_created) then
         call redirect_(stdin,lib)
      else
         call create_(stdin,name=lib)
         call open_for_(stdin,"read")
      end if

      ! Sanity check
      ENSURE(next_item_(stdin)=="{","VEC{COPPENSBASIS}:read_library_data ... expecting open bracket symbol, {")
      call read_(stdin,word)  ! Read the open bracket
      do  ! Loop over input list-type keywords
         call read_(stdin,word)  ! NOTE: The keys must be set here ...
         if (word=="}") exit
         if (word=="data=") exit
         call process_list_keyword_(self,word)
      end do
      ENSURE(keys_created_(self),"VEC{COPPENSBASIS}:read_library_data ... no keys= supplied")
      ENSURE(word=="data=","VEC{COPPENSBASIS}:read_library_data ... no data= list keyword")
      ENSURE(next_item_(stdin)=="{","VEC{COPPENSBASIS}:read_library_data ... expecting open bracket symbol, {")
      call read_(stdin,word)  ! move past open brace
      found = 0  ! Explicitly process data= keyword
      do  ! Try to match all basis labels
         call create_(basis)
         call process_keys_(basis)  ! Keys were set above.
         do i = 1,n_labels
            if (same_as_(basis%label,unique_labels(i),ignore_case=TRUE)) then
               found = found+1
               self(N+i) = basis
            end if
         end do
         call destroy_(basis)
         if (found==n_labels) exit  ! All bases found
         ENSURE(next_item_(stdin)/="}","VEC{COPPENSBASIS}:read_library_data ... missing basis sets in library")
      end do
      if (echo) call set_keyword_echo_(tonto,echo)
      if (stdin_created) then
         call revert_(stdin)
         call reset_io_status_(stdin)
      else
         call destroy_(stdin)
      end if
      call destroy_(unique_labels)
   end subroutine


!  ===============
!  Inquiry methods
!  ===============

   pure function no_of_orbitals(self) result (res)
   ! Work out and return the number of orbitals in the basis set
      VEC(type(COPPENSBASIS_TYPE),:), IN :: self
      INT :: res
      INT :: i
      res = 0
      do i = 1,size(self)
         res = res+no_of_orbitals_(self(i))
      end do
   end function

   pure function no_of_primitives(self) result (res)
   ! Work out and return the number of primitive fitting functions in the basis
      VEC(type(COPPENSBASIS_TYPE),:), IN :: self
      INT :: res
      INT :: i
      res = 0
      do i = 1,size(self)
         res = res+no_of_primitives_(self(i))
      end do
   end function

   function maximum_basis_n_value(self) result (res)
   ! Returns the maximum n value over all orbitals in every basis set of the
   ! vector
      VEC(type(COPPENSBASIS_TYPE),:), IN :: self
      INT :: res
      INT :: i
      res = 0
      do i = 1,size(self)
         res = max(maximum_basis_n_value_(self(i)),res)
      end do
   end function

   function same_as(self,vec) result (same)
   ! Return TRUE if the orbital vector "self" is the same as "vec".
      VEC(type(COPPENSBASIS_TYPE),:), IN :: self
      VEC(type(COPPENSBASIS_TYPE),:) :: vec
      BIN :: same
      INT :: i
      if (size(self)/=size(vec)) then
         same = FALSE
         return
      else
         same = TRUE
         do i = 1,size(self)
            same = same AND same_as_(self(i),vec(i))
            if (NOT same) exit
         end do
      end if
   end function


! =============
! Normalisation
! =============

   PURE subroutine unnormalize(self)
   ! Un-normalize each basis set.
      VEC(type(COPPENSBASIS_TYPE),:), INOUT :: self
      INT :: i
      do i = 1,size(self)
         call unnormalize_(self(i))
      end do
   end subroutine

   PURE subroutine renormalize(self)
   ! Renormalize each basis set.
      VEC(type(COPPENSBASIS_TYPE),:), INOUT :: self
      INT :: i
      do i = 1,size(self)
         call renormalize_(self(i))
      end do
   end subroutine


!  ====
!  Dump
!  ====

   subroutine dump(self,object_name)
   ! Dump a vector object
      VEC(type(COPPENSBASIS_TYPE),:), IN :: self
      STR(len=*), IN :: object_name
      STR(len=STR_SIZE) :: val
      INT :: i

      ! The following code is inherited from VEC{OBJECT}
      val = trim(object_name)//":VEC{COPPENSBASIS} = {"
      call text_(stdout,trim(val))
      call increment_margin_width_(stdout,1)
      call put_(stdout,"dimension =",width=11)
      call put_(stdout,size(self),width=0)
      call flush_(stdout)
      do i = 1,size(self)
         val = trim(object_name)//"("//trim(to_str_(i))//")"
         call dump_(self(i),trim(val))
      end do
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump an allocatable vector object
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, IN :: self
      STR(len=*), IN :: object_name
      STR(len=STR_SIZE) :: val
      INT :: i

      ! The following code is inherited from VEC{OBJECT}
      call flush_(stdout)
      if (NOT allocated(self)) then
         val = trim(object_name)//":VEC{COPPENSBASIS}* = null"
         call text_(stdout,trim(val))
      else
         val = trim(object_name)//":VEC{COPPENSBASIS}* = {"
         call text_(stdout,trim(val))
         call increment_margin_width_(stdout,1)
         call put_(stdout,"dimension =",width=11)
         call put_(stdout,size(self),width=0)
         call flush_(stdout)
         do i = 1,size(self)
            val = trim(object_name)//"("//trim(to_str_(i))//")"
            call dump_(self(i),trim(val))
         end do
         call increment_margin_width_(stdout,-1)
         call text_(stdout,"}")
      end if
   end subroutine


!  ======
!  Output
!  ======

   subroutine put(self)
   ! Output the list information
      VEC(type(COPPENSBASIS_TYPE),:), IN :: self
      INT :: i

      ! The following code is inherited from VEC{OBJECT}
      do i = 1,size(self)
         call put_(self(i))
         call flush_(stdout)
      end do
   end subroutine

end module
