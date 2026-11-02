!-----------------------------------------------------------------------------
!
!  BUFFER : operations on a string buffer ...
!
!  Synopsis
!
!  A BUFFER consists of a string (string), a pair of cursor positions
!  (item_start, item_end), an item counter (item_index), the number of items
!  in the buffer (n_items), a logical switch which indicates whether the
!  buffer has been analyzed (analyzed), and a variable indicating which
!  characters are to be regarded as initiating a comment (comment_chars).
!
!  Methods are divided into two types -- "put" operations and "get" operations.
!  The former involving placing value type variables, strings, integers, real's,
!  into the buffer, with or without formatting. The "get" operations involving
!  extracting value type variables from the string buffer.
!
!  There are also methods for moving around the buffer, including skipping
!  forwards or backwards, either by item or by character.
!
!  Notes
!
!  The buffer string is of length BSTR_SIZE. Problems will occur if you want
!  to analyze strings larger than this. The maximum size of any item is
!  STR_SIZE.
!
! Copyright (C) Dylan Jayatilaka, 1998
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
! $Id: buffer.foo 4305 2014-03-19 13:32:22Z dylan_ $
!-----------------------------------------------------------------------------
module BUFFER_MODULE

#  include "macros"

   include "buffer.use"

   implicit none

   include "buffer.int"


contains

! ==========
! Allocation
! ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(BUFFER_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"BUFFER:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(BUFFER_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"BUFFER:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
      type(BUFFER_TYPE), INOUT :: self
      call destroy_(self%replacement_list)
   end subroutine


!  ==============
!  Set operations
!  ==============

   pure subroutine set_defaults(self)
   ! Set default values
      type(BUFFER_TYPE), INOUT :: self
      self%string = " "
      self%item_start = 0
      self%item_end = 0
      self%item_index = 0
      self%n_items = 0
      self%analyzed = FALSE
      self%comment_chars = BUFFER_COMMENT_CHARS
      self%quote_chars = BUFFER_QUOTE_CHARS
      self%ignore_unmatched_quotes = FALSE
      self%eliminate_specials = TRUE
   end subroutine

   PURE subroutine set_and_analyze(self,string)
   ! Set the buffer .string to "string", analyze it.
      type(BUFFER_TYPE), INOUT :: self
      STR(len=*), IN :: string
      self%item_start = 0
      self%item_end = 0
      self%item_index = 0
      self%n_items = 0
      self%analyzed = FALSE
      call set_string_(self,string)
      if (self%eliminate_specials) call eliminate_special_chars_(self)
      if (allocated(self%replacement_list)) call replace_list_(self,self%replacement_list)
      call analyze_(self)
   end subroutine

   pure subroutine set_string(self,string)
   ! Set the buffer .string to "string"
      type(BUFFER_TYPE), INOUT :: self
      STR(len=*), IN :: string
      self%string = string
   end subroutine

   pure subroutine set_comment_chars(self,comment_chars)
   ! Set the "comment_chars" used to define the end of a parsable line
      type(BUFFER_TYPE), INOUT :: self
      STR(len=*), IN :: comment_chars
      self%comment_chars = comment_chars
   end subroutine

   pure subroutine set_quote_chars(self,quote_chars)
   ! Set the allowed "quote_chars" used to defined a quoted item.
      type(BUFFER_TYPE), INOUT :: self
      STR(len=*), IN :: quote_chars
      self%quote_chars = quote_chars
   end subroutine

   pure subroutine set_ignore_unmatched_quotes(self,ignore_unmatched_quotes)
   ! Set "ignore_unmatched_quotes" to prevent errors when there are
   ! unmatching quotes.
      type(BUFFER_TYPE), INOUT :: self
      BIN, IN :: ignore_unmatched_quotes
      self%ignore_unmatched_quotes = ignore_unmatched_quotes
   end subroutine

   pure subroutine set_eliminate_specials(self,eliminate_specials)
   ! Define whether to "eliminate_special" unreadable characters.
      type(BUFFER_TYPE), INOUT :: self
      BIN, IN :: eliminate_specials
      self%eliminate_specials = eliminate_specials
   end subroutine

   pure subroutine set_replacement_list(self,replacement_list)
   ! Define a "replacement_list" which replaces certain strings before
   ! the buffer is analyzed.
      type(BUFFER_TYPE), INOUT :: self
      MAT(STR(len=*),:,:), IN :: replacement_list
      self%replacement_list = replacement_list
   end subroutine


!  ===============
!  Clear & analyze
!  ===============

   PURE subroutine clear(self)
   ! Clear the buffer string and reset the counters. Do not reset any
   ! optional control switches e.g. like the .comment_chars
      type(BUFFER_TYPE), INOUT :: self
      self%string = " "
      self%item_start = 0
      self%item_end = 0
      self%item_index = 0
      self%n_items = 0
      self%analyzed = FALSE
      call analyze_(self)
   end subroutine

   pure subroutine replace_list(self,list)
   ! In the buffer .string, replace the strings in the first column of "list" by
   ! those in the second column of "list", respectively.Useful for preprocessing
   ! nasty lines.
      type(BUFFER_TYPE), INOUT :: self
      MAT(STR(len=*),:,:), IN :: list
      INT :: i
      STR(len=STR_SIZE) :: old,new
      do i = 1,size(list,1)
         old = list(i,1)
         new = list(i,2)
         if (new=='" "' OR new=="' '") then
            call replace_all_(self%string,trim(old),' ')
         else
            call replace_all_(self%string,trim(old),trim(new))
         end if
      end do
   end subroutine

   pure subroutine eliminate_special_chars(self)
   ! Remove any special characters from the buffer .string, by setting them to
   ! the blank character. Useful for getting rid of control characters in funny
   ! files from other crazy operating systems (read DOS).
      type(BUFFER_TYPE), INOUT :: self
      STR(len=52) :: letters
      STR(len=41) :: symbols
      STR(len=97) :: allowed
      INT :: i,k

      ! Initialize
      letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      symbols = "1234567890-=;',./`~!@#$%^&*()_+{}|:""<>?"
      allowed = letters//symbols//achar(91)//achar(92)//achar(93)

      ! Eliminate unknowns
      do i = 1,len_trim(self%string)
         k = verify(self%string(i:i),set=allowed)
         if (k==0) cycle
         self%string(i:i) = " "
      end do
   end subroutine

   PURE subroutine analyze(self)
   ! Analyze the buffer string and process it into items
      type(BUFFER_TYPE), INOUT :: self
      INT :: last,f,l
      STR(len=BSTR_SIZE) :: item
      last = 0
      self%n_items = 0
      do

         ! Get next item
         item = " "
         call get_next_item_(self%string(last+1:),item,f,l,self%comment_chars,self%quote_chars,self%ignore_unmatched_quotes)
         if (item==" ") exit

         ! Increment cursor/counter
         last = last+l+1
         self%n_items = self%n_items+1

         ! Too big?
         if (last>BSTR_SIZE) exit
      end do
      self%analyzed = TRUE
   end subroutine


!  ========================
!  Get operations on buffer
!  ========================

   PURE subroutine get_item(self,item)
   ! Get the next item in the buffer string as a str and increment
   ! the cursor
      type(BUFFER_TYPE), INOUT :: self
      STR(len=*), OUT :: item
      STR(len=BSTR_SIZE) :: bitem
      INT :: f,l
   ENSURE(self%analyzed,"BUFFER:get_item ... buffer not analyzed")
      item = " "
      if (NOT exhausted_(self)) then
         bitem = " "
         call get_next_item_(self%string(self%item_end+1:),bitem,f,l,self%comment_chars,self%quote_chars,self%ignore_unmatched_quotes)
         item = adjustl(bitem)
         self%item_start = self%item_end+f
         self%item_end = self%item_end+l
         self%item_index = self%item_index+1
      end if
   end subroutine

   PURE subroutine skip_item(self)
   ! Skip the next item in the buffer string, if there is one to skip.
      type(BUFFER_TYPE), INOUT :: self
      STR(len=BSTR_SIZE) :: bitem
      INT :: f,l
   ENSURE(self%analyzed,"BUFFER:skip_item ... buffer not analyzed")
      bitem = " "
      if (NOT exhausted_(self)) then
         call get_next_item_(self%string(self%item_end+1:),bitem,f,l,self%comment_chars,self%quote_chars,self%ignore_unmatched_quotes)
         self%item_start = self%item_end+f
         self%item_end = self%item_end+l
         self%item_index = self%item_index+1
      end if
   end subroutine

   PURE subroutine move_to_item(self,number)
   ! Move the cursor over to the *start* of item "number" (actually, it is moved
   ! to after the end of the previous item).
      type(BUFFER_TYPE), INOUT :: self
      INT, IN :: number
      INT :: item
   ENSURE(self%analyzed,"BUFFER:move_to_item ... buffer not analyzed")
   ENSURE(number<=self%n_items+1,"BUFFER:move_to_item ... not enough items")
   ENSURE(number>=1,"BUFFER:move_to_item ... cannot move to items less than 1")
      self%item_start = 0
      self%item_end = 0
      self%item_index = 0
      do item = 1,(number-1)
         call skip_item_(self)
      end do
   end subroutine

   PURE subroutine get_str(self,value)
   ! Get the next item in the buffer string as a str
      type(BUFFER_TYPE), INOUT :: self
      STR(len=*), OUT :: value
      call get_item_(self,value)
   end subroutine

   PURE subroutine get_bin(self,value)
   ! Get the next item in the buffer string as a logical
      type(BUFFER_TYPE), INOUT :: self
      BIN, OUT :: value
      STR(len=STR_SIZE) :: item
      call get_item_(self,item)
      ENSURE(is_bin_(item),"BUFFER:get_bin ... expected logical in input")
      value = to_bin_(item)
   end subroutine

   PURE subroutine get_int(self,value)
   ! Get the next item in the buffer string as an integer number
      type(BUFFER_TYPE), INOUT :: self
      INT, OUT :: value
      STR(len=STR_SIZE) :: item
      call get_item_(self,item)
      ENSURE(is_int_(item),"BUFFER:get_int ... expected integer in input")
      value = to_int_(item)
   end subroutine

   PURE subroutine get_real(self,value)
   ! Get the next item in the buffer string as a real number
      type(BUFFER_TYPE), INOUT :: self
      REAL, OUT :: value
      STR(len=STR_SIZE) :: item
      call get_item_(self,item)
      ENSURE(is_real_(item),"BUFFER:get_real ... expected real number in input")
      value = to_real_(item)
   end subroutine

   PURE subroutine get_cpx(self,value)
   ! Get the next item in the buffer string as a cpx number
      type(BUFFER_TYPE), INOUT :: self
      CPX, OUT :: value
      STR(len=STR_SIZE) :: item,item2
      call get_item_(self,item)
      item2 = " "
      if (NOT is_cpx_(item)) call get_item_(self,item2)  ! Try two real numbers
      item = trim(item)//" "//trim(item2)
      ENSURE(is_cpx_(item),"BUFFER:get_cpx ... expected complex number in input")
      value = to_cpx_(item)
   end subroutine

   pure subroutine get_formatted_int(self,value,fmt)
   ! Get a integer "value" into the buffer string using fortran fmt
   ! string "fmt", and increment the cursor.
      type(BUFFER_TYPE), INOUT :: self
      INT, OUT :: value
      STR(len=*), IN :: fmt
      INT :: first,last,width
      first = scan(fmt,"Ii")+1
      last = scan(fmt,")")-1
      read(fmt(first:last),*) width
      read(self%string(self%item_end+1:self%item_end+1+width),fmt) value
      self%item_end = self%item_end+width
      self%item_index = self%item_index+1
   end subroutine

   pure subroutine get_formatted_real(self,value,fmt)
   ! Get a real "value" into the buffer string using fortran fmt
   ! string "fmt", and increment the cursor.
      type(BUFFER_TYPE), INOUT :: self
      REAL, OUT :: value
      STR(len=*), IN :: fmt
      INT :: first,last,width
      first = scan(fmt,"FfEeDd")+1
      last = scan(fmt,".")-1
      read(fmt(first:last),*) width
      read(self%string(self%item_end+1:self%item_end+1+width),fmt) value
      self%item_end = self%item_end+width
      self%item_index = self%item_index+1
   end subroutine

   PURE subroutine get_imprecise_real(self,value,error)
   ! Get the next item in the buffer string as a real number "value" with a
   ! quoted "error" in parentheses immediately afterwards. If the error is not
   ! present in the string it is assumed to be zero. This only works for "f"
   ! numbers.
      type(BUFFER_TYPE), INOUT :: self
      REAL, OUT :: value,error
      STR(len=STR_SIZE) :: item
      call get_item_(self,item)
      ENSURE(is_real_(item) OR is_imprecise_real_(item),"BUFFER:get_imprecise_real ... expected REAL or imprecise REAL in input")
      call to_imprecise_real_(item,value,error)
   end subroutine


!  ========================
!  Put operations on buffer
!  ========================

   PURE subroutine move_cursor(self,skip)
   ! Move the cursor "skip" characters in the buffer string
      type(BUFFER_TYPE), INOUT :: self
      INT, IN :: skip
      self%item_end = self%item_end+skip
      ENSURE(self%item_end<=BSTR_SIZE,"BUFFER:move_cursor ... cursor beyond buffer end")
   end subroutine

   subroutine put_str(self,string)
   ! Put "string" into the buffer string *after* the current position and
   ! increment the cursor.
   !
   ! Deliberately not PURE: the length check below is a DIE, which is a call
   ! into SYSTEM, and it has to be live in the release build users run.
   !
   ! In a parallel job every process fills its own buffer and nothing is
   ! broadcast: only the process that writes the file needs the text, and it
   ! made it itself. See docs/TONTO_DEVELOPER_INFO.md section 1a.
      type(BUFFER_TYPE), INOUT :: self
      STR(len=*), IN :: string
      INT :: L
      STR(len=STR_SIZE) :: msg
      L = len(string)

      ! Overrunning .string is a wild write past the end of the buffer, so this
      ! must be a DIE and not an ENSURE -- see TASKS_AND_HISTORY.md.
      if (self%item_end+L>BSTR_SIZE) then
         write(msg,'(a,i0,a,i0,a)') "would be ",self%item_end+L," characters, over the ",BSTR_SIZE," character limit; it is: "
         DIE("BUFFER:put_str ... output line "//trim(msg)//self%string(1:self%item_end)//trim(string))
      end if
      self%string(self%item_end+1:self%item_end+L) = string
      call move_cursor_(self,L)
      self%analyzed = FALSE
   end subroutine

   subroutine put_formatted_str(self,value,fmt)
   ! Put a string "value" into the buffer string using fortran fmt
   ! string "fmt", and increment the cursor.
      type(BUFFER_TYPE), INOUT :: self
      STR(len=*), IN :: value
      STR(len=*), IN :: fmt
      STR(len=STR_SIZE) :: string
      string = " "
      write(string,"("//trim(fmt)//")") value
      call put_str_(self,trim(string))
      self%analyzed = FALSE
   end subroutine

   subroutine put_formatted_bin(self,value,fmt)
   ! Put a logical "value" into the buffer string using fortran fmt string
   ! "fmt", and increment the cursor.
      type(BUFFER_TYPE), INOUT :: self
      BIN, IN :: value
      STR(len=*), IN :: fmt
      STR(len=STR_SIZE) :: string
      write(string,"("//trim(fmt)//")") value
      call put_str_(self,trim(string))
      self%analyzed = FALSE
   end subroutine

   subroutine put_formatted_int(self,value,fmt)
   ! Put an integer "value" into the buffer string using fortran fmt string
   ! "fmt", and increment the cursor.
      type(BUFFER_TYPE), INOUT :: self
      INT, IN :: value
      STR(len=*), IN :: fmt
      STR(len=STR_SIZE) :: string
      write(string,"("//trim(fmt)//")") value
      call put_str_(self,trim(string))
      self%analyzed = FALSE
   end subroutine

   subroutine put_formatted_real(self,value,fmt)
   ! Put a real "value" into the buffer string using fortran format string
   ! "format", and increment the cursor.
      type(BUFFER_TYPE), INOUT :: self
      REAL, IN :: value
      STR(len=*), IN :: fmt
      STR(len=STR_SIZE) :: string
      write(string,"("//trim(fmt)//")") value
      call put_str_(self,trim(string))
      self%analyzed = FALSE
   end subroutine

   subroutine put_formatted_cpx(self,value,fmt)
   ! Put a cpx "value" into the buffer string using fortran format string
   ! "format", and increment the cursor.
      type(BUFFER_TYPE), INOUT :: self
      CPX, IN :: value
      STR(len=*), IN :: fmt
      STR(len=STR_SIZE) :: string
      write(string,"("//"2"//trim(fmt)//")") value
      call put_str_(self,trim(string))
      self%analyzed = FALSE
   end subroutine


!  ===============
!  Inquiry methods
!  ===============

   PURE subroutine get_next_item(self,res)
   ! Return the actual item which the cursor lies *before*, i.e. the
   ! next item to be processed. The cursor is positioned after the end
   ! of this item when the routine concludes.
      type(BUFFER_TYPE), INOUT :: self
      STR(len=*), OUT :: res
      call get_item_(self,res)
   end subroutine

   pure subroutine get_previous_item(self,res)
   ! Return the item which the cursor lies *after*, i.e. the previous item
   ! to be processed. The cursor remains unchanged after this routine.
      type(BUFFER_TYPE), IN :: self
      STR(len=*), OUT :: res
      if (self%item_start>0) then
         res = self%string(self%item_start:self%item_end)
      else
         res = " "
      end if
   end subroutine

   PURE subroutine get_all_items(self,res)
   ! Return all items as a vector
      type(BUFFER_TYPE), INOUT :: self
      VEC(STR(len=*),self%n_items), OUT :: res
      INT :: n
   ENSURE(self%analyzed,"BUFFER:get_all_items ... must be analyzed first")
   ENSURE(self%n_items>=1,"BUFFER:get_all_items ... must have at least one item")
      call move_to_item_(self,1)
      do n = 1,self%n_items
         call get_item_(self,res(n))
      end do
   end subroutine

   PURE subroutine get_all_remaining_items(self,res)
   ! Return all remaining items in the buffer as a vector
      type(BUFFER_TYPE), INOUT :: self
      VEC(STR(len=*),self%n_items-self%item_index), OUT :: res
      INT :: n,i
   ENSURE(self%analyzed,"BUFFER:get_all_remaining_items ... must be analyzed first")
   ENSURE(self%n_items-self%item_index>=0,"BUFFER:get_all_remaining_items ... must have some remaining items")
      call skip_item_(self)
      i = 0
      do n = 1,self%n_items-self%item_index
         i = i+1
         call get_item_(self,res(i))
      end do
   end subroutine

   PURE subroutine get_n_items(self,res)
   ! Return the number of items in the buffer string
      type(BUFFER_TYPE), INOUT :: self
      INT, OUT :: res
      if (NOT self%analyzed) call analyze_(self)
      res = self%n_items
   end subroutine

   PURE subroutine includes(self,item,res)
   ! Return TRUE if the buffer contains "item" as a separate entity
      type(BUFFER_TYPE), INOUT :: self
      STR(len=*), IN :: item
      BIN, OUT :: res
      STR(len=STR_SIZE) :: word
      call move_to_item_(self,1)
      res = FALSE
      search: do
         call get_str_(self,word)

         ! Is it there?
         if (word==item) then
            res = TRUE
            exit search
         end if

         ! Anything left?
         if (exhausted_(self)) exit
      end do search
   end subroutine

   pure function next_item_index(self) result (res)
   ! Return the number of the item which the cursor lies 
   ! *before*, i.e. the next item to be processed.
      type(BUFFER_TYPE), IN :: self
      INT :: res
      res = self%item_index+1
   end function

   pure function next_item_number(self) result (res)
   ! Return the number of the item which the cursor lies *before*,
   ! i.e. the next item to be processed.
      type(BUFFER_TYPE), IN :: self
      INT :: res
      res = self%item_index+1
   end function

   pure function buffer_string(self) result (res)
   ! Return the buffer string, less any blank spaces at the end
      type(BUFFER_TYPE), IN :: self
      STR(len=BSTR_SIZE) :: res
      res = " "
      res = self%string(1:max(len_trim(self%string),1))
   end function

   pure function cursor_pointer(self) result (res)
   ! Return a string of the form "----^" which is a pictorial
   ! representation of where the cursor position lies
      type(BUFFER_TYPE), IN :: self
      STR(len=BSTR_SIZE) :: res
      res = " "
      res = repeat("-",self%item_end-1)//"^"
   end function

   pure function analyzed(self) result (res)
   ! Return true if the buffer string has been analyzed
      type(BUFFER_TYPE), IN :: self
      BIN :: res
      res = self%analyzed
   end function

   pure function exhausted(self) result (res)
   ! Return true if there are no more items in the 
   ! buffer string that could be extracted
      type(BUFFER_TYPE), IN :: self
      BIN :: res
      res = self%n_items==0 OR self%item_index>=self%n_items
   end function

   pure function empty(self) result (res)
   ! Return true if the buffer string contains no items
      type(BUFFER_TYPE), IN :: self
      BIN :: res
      res = self%n_items==0
   end function

end module
