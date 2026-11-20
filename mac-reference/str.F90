!=======================================================================
!
!  STR: Methods of dealing with arbitrary length character strings
!
!  Notes
!
!  Normally, a STR variable means a character string of length STR_SIZE.
!  However, in this module we use arbitrary length character strings.
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
!=======================================================================
module STR_MODULE

#  include "macros"

   include "str.use"

   implicit none

   include "str.int"

   VEC(STR(len=1),6) :: opening = ["'",'"',"{","(","[","<"]
   VEC(STR(len=1),6) :: closing = ["'",'"',"}",")","]",">"]
   STR(len=52) :: letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
   STR(len=26) :: lowercase = "abcdefghijklmnopqrstuvwxyz"
   STR(len=26) :: uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   STR(len=10) :: numeric = "0123456789"
   STR(len=62) :: alphanumeric = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

!  Now make strings arguments arbitrary length by default

#  undef  SELF_TYPE_SIZE
#  define SELF_TYPE_SIZE  *

contains

!  =============
!  Process items
!  =============

   PURE function n_items(self) result (res)
   ! Return the number of items in the string
      STR(len=*), IN :: self
      INT :: res
      INT :: end,f,l,last
      STR(len=BSTR_SIZE) :: item
      last = len_trim(self)
      end = 0
      res = 0
      do
         item = " "
         call get_next_item_(self(end+1:),item,f,l)
         if (item==" ") exit
         end = end+l+1
         res = res+1
         if (end>last) exit
      end do
   end function

   PURE function item(self,n) result (res)
   ! Return the item no. "n" in the string.
      STR(len=*), IN :: self
      INT, IN :: n
      STR(len=len(self)) :: res
      call get_item_(self,n,res)
   end function

   PURE subroutine get_item(self,n,item,position)
   ! Get the item no. "n" in the self string and put the result in
   ! "item". If "position" is present it is set to the index of the
   ! character after the end of item "n".
      STR(len=*), IN :: self
      INT, IN :: n
      STR(len=*), OUT :: item
      INT, OUT, optional :: position
      STR(len=len(self)) :: word
      INT :: i,f,l
      f = 1
      do i = 1,n
         call get_next_item_(self(f:),word,last=l)
         if (word==" ") exit
         f = f+l
      end do

      ! Set the item and its position
      item = word
      if (present(position)) position = f
   end subroutine

   PURE subroutine get_next_item(self,item,first,last,comment_chars,quote_chars,ignore_unmatched_quotes)
   ! Get the first sequence of non-blank characters in the string
   ! (i.e. an "item") and (if present) the "first" and "last"
   ! character positions of the item in the self string.  If the first
   ! character of the word is a double quote then all text between it
   ! and the next double quote is treated as one item. If
   ! "quote_chars" is present and not blank, then any of these
   ! characters is regarded as a double quote; but if "quote_chars" is
   ! blank then no quotation characters are recognised. If
   ! "comment_chars" is present, the rest of the string following
   ! these comment characters is ignored.
      STR(len=*), IN :: self
      STR(len=*), OUT :: item
      INT, OUT, optional :: first,last
      STR(len=*), IN, optional :: comment_chars,quote_chars
      BIN, IN, optional :: ignore_unmatched_quotes
      BIN :: ignore
      STR(len=16) :: quotes
      STR(len=1) :: quote
      STR(len=len(self)) :: word
      INT :: f,l,m
      quotes = '"'
      if (present(quote_chars)) quotes = quote_chars
      ignore = FALSE
      if (present(ignore_unmatched_quotes)) ignore = ignore_unmatched_quotes
      call get_next_item_position_(self,f,l)
      if (f==0) then

         ! All characters are blanks
         word = " "
      else if (quotes/=" " AND scan(self(f:f),quotes)/=0) then

         ! Find quoted item
         quote = self(f:f)
         m = l
         l = quote_position_(self(f+1:),quote)
         if (l==0) then
            ENSURE(ignore,"STR:get_next_item ... unclosed quotes")
            l = m
            word = self(f:l)
         else
            l = f+l
            word = self(f+1:l-1)
         end if
      else if (present(comment_chars)) then

         ! Comment found
         if (scan(self(f:f),comment_chars)/=0) then
            l = f-1
            word = " "
         else
            word = self(f:l)
         end if
      else

         ! A plain old item found
         word = self(f:l)
      end if

      ! Set the item and its limits
      item = word
      if (present(first)) first = f
      if (present(last)) last = l
   end subroutine

   pure subroutine get_next_item_position(self,first,last)
   ! Get the first and last character positions of the first sequence of
   ! non-blank characters in the string (i.e. a "word")
      STR(len=*), IN :: self
      INT, OUT :: first,last
      first = verify(self," ")
      last = scan(self(max(first,1):)//" "," ")-1
      last = last+max(first,1)-1
      last = max(last,0)
   end subroutine

   PURE subroutine get_next_items(self,word)
   ! Get items 2 and after from self, i.e. all except the first item.
      STR(len=*), IN :: self
      STR(len=len(self)), OUT :: word
      INT :: l
      call get_next_item_(self,word,last=l)
      word = adjustl(self(2+l:))
   end subroutine


!  =============
!  String search
!  =============

   pure function quote_position(self,quote_chars) result (pos)
   ! Find the position of the first double quote character.
      STR(len=*), IN :: self
      INT :: pos
      STR(len=*), optional, IN :: quote_chars
      if (present(quote_chars)) then; pos = scan(self,quote_chars)
      else; pos = index(self,'"')
      end if
   end function

   PURE function index_of_matching_bracket(self,symbol) result (res)
   ! Return the first index of the matching "symbol" in self.  These
   ! are usually quotes, braces, parentheses, etc ... { ... }. The
   ! first element of self need not be an opening bracket symbol.
   ! Returns zero if no match could be found, and also a warning.
      STR(len=*), IN :: self
      STR(len=*), IN :: symbol
      INT :: res
      STR(len=1) :: op,cl,c
      INT :: i,s,n
   ENSURE(any(symbol==opening),"STR:index_of_matching_bracket ... unrecognised opening symbol")
   ENSURE(includes_(self,symbol),"STR:index_of_matching_bracket ... opening symbol cannot be found in self")
      op = symbol
      do i = 1,size(opening)
         if (op/=opening(i)) cycle
         exit
      end do
      cl = closing(i)
      s = scan(self,op)
      n = 0
      res = 0
      do i = s+1,len_trim(self)
         c = self(i:i)
         if (c==op) then; n = n+1
         else if (c==cl AND n==0) then; res = i; exit
         else if (c==cl AND n>0) then; n = n-1
         end if
      end do
      ENSURE(res/=0,"STR:index_of_matching_bracket ... unmatching number of closing bracket symbols")
   end function

   elemental function index_of_substring(self,substring,backwards) result (ind)
   ! Return the starting index of a substring in the original string
      STR(len=*), IN :: self
      STR(len=*), IN :: substring
      BIN, optional, IN :: backwards
      INT :: ind
      if (present(backwards)) then; ind = index(self,substring,backwards)
      else; ind = index(self,substring)
      end if
   end function

   pure function is_included_in(self,string,at_start) result (res)
   ! Return TRUE if self is included in "string". 
   ! Trailing blanks in self are ignored. If "at_start" 
   ! is present and TRUE then the result is TRUE only
   ! if self is included at the start of the string.
      STR(len=*), IN :: self
      STR(len=*), IN :: string
      BIN, IN, optional :: at_start
      BIN :: res
      INT :: ind
      ind = index(string,trim(self))
      res = ind/=0
      if (present(at_start)) then
         if (at_start) then
            res = ind==1
         end if
      end if
   end function

   pure function is_included_in_any(self,strvec) result (res)
   ! Return TRUE if self is included in any element of the string vector
   ! "strvec". Trailing blanks in self are *not* ignored.
      STR(len=*), IN :: self
      VEC(STR(len=*),:), IN :: strvec
      BIN :: res
      res = any(index(strvec,spread(self,1,size(strvec)))/=0)
   end function

   pure function includes_blanks(self,at_start) result (res)
   ! Return true if self includes any blank character.
      STR(len=*), IN :: self
      BIN, IN, optional :: at_start
      BIN :: res
      INT :: ind
      ind = index(trim(self)," ")
      res = ind/=0
      if (present(at_start)) then
         if (at_start) then
            res = ind==1
         end if
      end if
   end function

   pure function includes(self,string,at_start) result (res)
   ! Return true if self includes "string". Trailing blanks in self
   ! are ignored If "at_start" is present, the result is true only if
   ! "string" is the first part of self.
      STR(len=*), IN :: self
      STR(len=*), IN :: string
      BIN, IN, optional :: at_start
      BIN :: res
      INT :: ind
      ind = index(trim(self),string)
      res = ind/=0
      if (present(at_start)) then
         if (at_start) then
            res = ind==1
         end if
      end if
   end function

   pure function includes_any_in(self,strvec) result (res)
   ! Return TRUE if self includes any element of the string vector
   ! "strvec".  Trailing blanks in self are ignored.
      STR(len=*), IN :: self
      VEC(STR(len=*),:), IN :: strvec
      BIN :: res
      INT :: i
      res = FALSE
      do i = 1,size(strvec)
         res = index(trim(self),trim(strvec(i)))/=0
         if (res) exit
      end do
   end function

   pure function does_not_include(self,string) result (res)
   ! Return true if self does not include string. Traling blanks in
   ! self are ignored
      STR(len=*), IN :: self
      STR(len=*), IN :: string
      BIN :: res
      INT :: ind
      ind = index(self(1:len_trim(self)),string)
      res = (ind==0)
   end function

   pure function has_any_characters_in(self,set) result (res)
   ! Return TRUE if self has any of the characters in "set".
      STR(len=*), IN :: self
      STR(len=*), IN :: set
      BIN :: res
      res = scan(self,set)/=0
   end function

   pure function has_all_characters_in(self,set) result (res)
   ! Return TRUE if self has all of its characters in "set".
      STR(len=*), IN :: self
      STR(len=*), IN :: set
      BIN :: res
      res = index_of_character_not_in_(self,set)==0
   end function

   pure function index_of(self,set,backwards) result (ind)
   ! In self, scan from left to right and return the index of the single
   ! character "set". If backwards is present and true, scan from right to left.
      STR(len=*), IN :: self
      STR(len=1), IN :: set
      BIN, optional, IN :: backwards
      INT :: ind
      if (present(backwards)) then; ind = scan(self,set,backwards)
      else; ind = scan(self,set)
      end if
   end function

   pure function index_of_character_in(self,set,backwards) result (ind)
   ! In self, scan from left to right and return the index of the first
   ! character in "set". If backwards is present and true, scan from
   ! right to left
      STR(len=*), IN :: self
      STR(len=*), IN :: set
      BIN, IN, optional :: backwards
      INT :: ind
      if (present(backwards)) then; ind = scan(self,set,backwards)
      else; ind = scan(self,set)
      end if
   end function

   pure function index_of_character_not_in(self,set,backwards) result (ind)
   ! In self, scan from left to right and return the index of the first
   ! character *not* in "set". If backwards is present and true, scan from
   ! right to left
      STR(len=*), IN :: self
      STR(len=*), IN :: set
      BIN, optional, IN :: backwards
      INT :: ind
      ind = verify(self,set,backwards)
   end function

   pure function index_of_alphabetical(self) result (res)
   ! Returns the index of the first alphabetical character.
      STR(len=*), IN :: self
      INT :: res
      res = index_of_character_in_(self,letters)
   end function

   pure function index_of_nonalphabetical(self) result (res)
   ! Returns the index of the first non-alphabetical character.
      STR(len=*), IN :: self
      INT :: res
      res = index_of_character_not_in_(self,letters)
   end function

   pure function index_of_lcase_alphabetical(self) result (res)
   ! Returns the index of the first lower case alphabetical character.
      STR(len=*), IN :: self
      INT :: res
      res = index_of_character_in_(self,lowercase)
   end function

   pure function index_of_ucase_alphabetical(self) result (res)
   ! Returns the index of the first upper case alphabetical character.
      STR(len=*), IN :: self
      INT :: res
      res = index_of_character_in_(self,uppercase)
   end function

   pure function index_of_digit(self) result (res)
   ! Returns the index of the first digit character.
      STR(len=*), IN :: self
      INT :: res
      res = index_of_character_in_(self,numeric)
   end function

   pure function index_of_nondigit(self) result (res)
   ! Returns the index of the first non-digit character.
      STR(len=*), IN :: self
      INT :: res
      res = index_of_character_not_in_(self,numeric)
   end function

   pure function last_characters(self,n) result (res)
   ! Return the last "n" *nonblank* characters.
   ! If "n" is too largwe return the whole string.
      STR(len=*), IN :: self
      INT, IN :: n
      STR(len=n) :: res
      INT :: f,l
      l = index_of_character_not_in_(self," ",backwards=TRUE)
      f = max(l-n+1,1)
      res = self(f:l)
   end function

   pure function ends_in(self,fin,case_sensitive) result (res)
   ! Return TRUE if self ends in the string "fin".
      STR(len=*), IN :: self
      STR(len=*), IN :: fin
      BIN, optional, IN :: case_sensitive
      BIN :: res
      BIN :: use_case
      STR(len=STR_SIZE) :: tail
      use_case = FALSE
      if (present(case_sensitive)) use_case = case_sensitive
      tail = last_characters_(self,len(fin))
      if (NOT use_case) then
         res = trim(lower_cased_(fin))==trim(lower_cased_(tail))
      else
         res = trim(fin)==trim(tail)
      end if
   end function


!  ===========================
!  Self-string transformations
!  ===========================

   PURE function split(self) result (res)
   ! Split the string into a vector of separate items.
      STR(len=*), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      INT :: i,n
      n = n_items_(self)
      allocate(res(n))
      do i = 1,n
         res(i) = item_(self,i)
      end do
   end function

   PURE function braced_contents(self) result (res)
   ! Return the contents of the split string "{  a b c .... }" as a
   ! vector of separate items.
      STR(len=*), IN :: self
      VEC(STR(len=STR_SIZE),:), PTR :: res
      INT :: i,n
      n = n_items_(self)
      ENSURE(n>=2,"STR:braced_contents ... not enough elements")
      ENSURE(item_(self,1)=="{","STR:braced_contents ... list does not begin in an open brace, {")
      ENSURE(item_(self,n)=="}","STR:braced_contents ... list does not end in a close brace, }")
      allocate(res(n-2))
      do i = 2,n-1
         res(i-1) = item_(self,i)
      end do
   end function

   pure function trim_blanks_from_end(self) result (res)
   ! Return the trimmed version of "self"
      STR(len=*), IN :: self
      STR(len=len_trim(self)) :: res
      res = trim(self)
   end function

   pure function trim_blanks_from_start(self) result (res)
   ! Return "self" starting from first non-blank character
   ! WARNING: do not apply to a blank string!
      STR(len=*), IN :: self
      STR(len=len(self)) :: res
      res = self(verify(self," "):)
   end function

   pure function trim_from_end(self,fin) result (res)
   ! Remove "fin" from the end of "self" if it is there.
   ! Otherwise do nothing.
      STR(len=*), IN :: self
      STR(len=*), IN :: fin
      STR(len=STR_SIZE) :: res
      INT :: n,f,l
      if (ends_in_(self,fin)) then
         n = len_trim(fin)
         l = index_of_character_not_in_(self," ",backwards=TRUE)
         f = l-n+1
         res = self(1:f)
      else
         res = self
      end if
   end function

   elemental subroutine to_lower_case(self)
   ! Change upper case charaters to lower case in the original string
      STR(len=*), INOUT :: self
      INT :: i
      do i = 1,len_trim(self)
         if ("A"<=self(i:i) AND self(i:i)<="Z") then
            self(i:i) = achar(iachar(self(i:i))+32)
         end if
      end do
   end subroutine

   elemental function lower_cased(self) result (res)
   ! Return the lower case of self.
      STR(len=*), IN :: self
      STR(len=len(self)) :: res
      res = self
      call to_lower_case_(res)
   end function

   elemental subroutine to_upper_case(self)
   ! Change lower case charaters to upper case in the original string
      STR(len=*), INOUT :: self
      INT :: i
      do i = 1,len(self)
         if ("a"<=self(i:i) AND self(i:i)<="z") then
            self(i:i) = achar(iachar(self(i:i))-32)
         end if
      end do
   end subroutine

   pure function upper_cased(self) result (res)
   ! Return the upper case of self.
      STR(len=*), IN :: self
      STR(len=len(self)) :: res
      res = self
      call to_upper_case_(res)
   end function

   elemental subroutine left_justify(self)
   ! Left-justify "self" string
      STR(len=*), INOUT :: self
      self = adjustl(self)
   end subroutine

   elemental function left_justified(self) result (res)
   ! Return a left-justified "self" string
      STR(len=*), IN :: self
      STR(len=len(self)) :: res
      res = adjustl(self)
   end function

   elemental subroutine right_justify(self)
   ! Right-justify "self" string
      STR(len=*), INOUT :: self
      self = adjustr(self)
   end subroutine

   elemental function right_justified(self) result (res)
   ! Return a right-justified "self" string
      STR(len=*), IN :: self
      STR(len=len(self)) :: res
      res = adjustr(self)
   end function

   elemental subroutine center_justify(self)
   ! Centrify the self string
      STR(len=*), INOUT :: self
      self = center_justified_(self)
   end subroutine

   elemental function center_justified(self) result (res)
   ! Return a centrified self string
      STR(len=*), IN :: self
      STR(len=len(self)) :: res
      INT :: w,l,nr,nl
      res = adjustl(self)
      w = len(self)
      l = len_trim(res)
      nl = int((w-l)/2)
      nr = w-l-nl
      res = repeat(" ",nl)//res(1:l)//repeat(" ",nr)
   end function

   pure subroutine insert(self,string,position)
   ! Insert "string" at "position" into the self string.
      STR(len=*), INOUT :: self
      STR(len=*), IN :: string
      INT, IN :: position
      STR(len=len(self)) :: rest
      rest = self(position:)
      self(position:) = string
      self(position+len(string):) = rest
   end subroutine

   elemental subroutine replace_all(self,a,b,repeatedly)
   ! Replace all occurences of string "a" by "b" from left to right in
   ! "self". String "b" can be zero length. If "repeatedly" is present
   ! and TRUE, "b" is replaced repeatedly at a particular position in
   ! "self" before moving to the next position right.
      STR(len=*), INOUT :: self
      STR(len=*), IN :: a,b
      BIN, IN, optional :: repeatedly
      STR(len=len(self)) :: post
      INT :: len_a,len_b,maxlen,i
      BIN :: advance

      ! Target must be non-empty
      if (len(a)==0) return
      advance = TRUE
      if (present(repeatedly)) advance = FALSE

      ! Length of the strings
      len_a = len(a)
      len_b = len(b)

      ! Set maxlen, limit of replacements
      maxlen = len(self)
      if (len_b==0 OR len_trim(a)>0) then
         maxlen = len_trim(self)
      end if

      ! Index into string "self"
      i = 0

      ! Do replacement ...
      do

         ! Next character
         i = i+1

         ! Past end of self
         if (i+len_a-1>maxlen) exit

         ! No match? ... then go to next character in self
         if (self(i:i+len_a-1)/=a) cycle

         ! Get part after the "a" match in "post"
         post = self(i+len_a:)

         ! Assign the replacement
         if (len_b>0) self(i:i+len_b-1) = b
         ! ENSURE(i+len_b-1<=len(self),"replacement exceeds string length")

         ! Assign the post match
         self(i+len_b:) = post

         ! Move to the end of the replacement
         if (advance) then; i = i+len_b-1
         else; i = i-1
         end if
      end do
   end subroutine

   PURE subroutine replace(self,a,b)
   ! Replace first occurences of string "a" by "b". String "b" can be zero
   ! length, however, replacements only occur up to the last nonblank
   ! character in "self" i.e. up to len_trim(self).
      STR(len=*), INOUT :: self
      STR(len=*), IN :: a,b
      call replace_first_(self,a,b)
   end subroutine

   PURE subroutine replace_first(self,a,b)
   ! Replace first occurences of string "a" by "b". String "b" can be zero
   ! length, however, replacements only occur up to the last nonblank
   ! character in "self" i.e. up to len_trim(self).
      STR(len=*), INOUT :: self
      STR(len=*), IN :: a,b
      STR(len=len(self)) :: post
      INT :: len_a,len_b,i
   ENSURE(len(a)>0,"STR:replace_first ... len(a) must be non-zero")
      len_a = len(a)
      len_b = len(b)
      i = 0
      do
         i = i+1
         if (i+len_a-1>len_trim(self)) exit
         if (self(i:i+len_a-1)/=a) cycle
         post = self(i+len_a:)
         if (len_b>0) self(i:i+len_b-1) = b
         ENSURE(i+len_b<len(self),"STR:replace_first ... replacement exceeds string length")
         self(i+len_b:) = post
      ! i = i + len_b - 1
         exit
      end do
   end subroutine

   pure subroutine replace_end_from(self,set,new,plus)
   ! Replace a *contiguous* block of characters in the "set" at
   ! the *end* of the string with the specified "new" character.
   ! If specified, the replacement happens "plus" characters after the
   ! first character not in "set".
      STR(len=*), INOUT :: self
      STR(len=*), IN :: set
      STR(len=1), IN :: new
      INT, optional, IN :: plus
      INT :: p,from,i
      p = 0
      if (present(plus)) p = plus
      from = index_of_character_not_in_(self,set,backwards=TRUE)
      do i = from+p+1,len(self)
         self(i:i) = new
      end do
   end subroutine

   elemental subroutine remove_all(self,a)
   ! Remove all occurences of "a" from "self".
      STR(len=*), INOUT :: self
      STR(len=*), IN :: a
      call replace_all_(self,a,'')
   end subroutine

   elemental subroutine remove_blanks(self)
   ! Replace all blanks by moving all non-blank characters leftwards
      STR(len=*), INOUT :: self
      call remove_all_(self," ")
   end subroutine

   pure subroutine separate_all_characters(self)
   ! Separate all nonblank characters by one space
      STR(len=*), INOUT :: self
      INT :: i
      i = 1
      do
         if (i>=len_trim(self)) exit
         if (self(i:i)==" ") then
            self(i:) = self(i+1:)
         else
            self(i+2:) = self(i+1:)
            self(i+1:i+1) = " "
            i = i+2
         end if
      end do
   end subroutine

   elemental subroutine separate_before(self,characters)
   ! Separate the string by placing a space before each character that
   ! occurs in "characters".
      STR(len=*), INOUT :: self
      STR(len=*), IN :: characters
      STR(len=1) :: thischar
      INT :: i,last
      i = 1
      last = len(self)

      ! Loop over positions "i"
      do

         ! Past the end?
         if (i>len_trim(self)) exit

         ! Insert space if thischar matches
         thischar = self(i:i)
         if (includes_(characters,thischar)) then
            self(i+1:last) = self(i:last-1)  ! move all along by one.
            self(i:i) = " "
            i = i+1
         end if

         ! Next 
         i = i+1
      end do
   end subroutine

   elemental subroutine separate_after(self,characters)
   ! Separate the string by placing a space after each character that
   ! occurs in "characters".
      STR(len=*), INOUT :: self
      STR(len=*), IN :: characters
      STR(len=1) :: thischar
      INT :: i,last
      i = 1
      last = len(self)

      ! Loop over positions "i"
      do

         ! Past the end?
         if (i>=len_trim(self)-1) exit

         ! Insert space if thischar matches
         thischar = self(i:i)
         if (includes_(characters,thischar)) then
            self(i+2:last) = self(i+1:last-1)  ! move all along by one.
            self(i+1:i+1) = " "
            i = i+1
         end if

         ! Next 
         i = i+1
      end do
   end subroutine

   pure subroutine pad_with(self,symbol,width,prepad)
   ! Pad the end of the string with "symbol" up to length "width".
   ! If "prepad" is present and TRUE, put the symbol before.
      STR(len=*), INOUT :: self
      STR(len=1), IN :: symbol
      INT, IN :: width
      BIN, optional, IN :: prepad
      BIN :: pre
      INT :: l
      l = len_trim(self)
      if (l>=width) return
      pre = FALSE
      if (present(prepad)) pre = prepad
      if (pre) then; self = trim(repeat(symbol,width-l))//trim(self)
      else; self = trim(self)//repeat(symbol,width-l)
      end if
   end subroutine


!  ======================
!  File name manipulation 
!  ======================

   PURE function filename_head(self) result (res)
   ! Return the head part of a file name, e.g. if self is
   ! "/home/file.c" it returns the string "file". 
      STR(len=*), IN :: self
      STR(len=len(self)) :: res
      INT :: f,l,dim
   ENSURE(self/=" ","STR:filename_head ... string is blank!")
      dim = len_trim(self)

      ! Last slash
      f = index_of_character_in_(self(1:dim),"/",backwards=TRUE)
      if (f==0) then
         f = 1
      else
         f = f+1
      end if

      ! Last dot
      l = index_of_character_in_(self(f:dim),".")
      if (l==0) then
         res = self(f:dim)
      else
         res = self(f:f+l-2)
      end if
   end function

   PURE function filename_tail(self) result (res)
   ! Return the tail part of a file name, e.g. if self is
   ! "/home/file.c" it returnd the string "c". 
      STR(len=*), IN :: self
      STR(len=len(self)) :: res
      INT :: f,l,dim
   ENSURE(self/=" ","STR:filename_tail ... string is blank!")
      dim = len_trim(self)

      ! Last slash
      f = index_of_character_in_(self(1:dim),"/",backwards=TRUE)
      if (f==0) then
         f = 1
      else
         f = f+1
      end if

      ! Last dot
      l = index_of_character_in_(self(f:dim),".")
      if (l==0) then
         res = " "
      else
         res = self(f+l:dim)
      end if
   end function

   PURE function filename_directory(self) result (res)
   ! Return the directory part of a file name, e.g. if self is
   ! "/home/file.c" it returnd the string "/home". 
      STR(len=*), IN :: self
      STR(len=len(self)) :: res
      INT :: l
   ENSURE(self/=" ","STR:filename_directory ... string is blank!")
      l = index_of_character_in_(self,"/",backwards=TRUE)
      if (l==0) then
         res = "."
      else if (l==1) then
         res = "/"
      else
         l = l-1
         res = self(1:l)
      end if
   end function


!  =================
!  Inquiry functions
!  =================

   pure function equals(self,string,ignore_case) result (same)
   ! Test to see if the string is the same as another string
   ! If "ignore_case" is present and TRUE then case is ignored.
      STR(len=*), IN :: self
      STR(len=*), IN :: string
      BIN, IN, optional :: ignore_case
      BIN :: same
      same = same_as_(self,string,ignore_case)
   end function

   pure function same_as(self,string,ignore_case) result (same)
   ! Test to see if the string is the same as another string
   ! If "ignore_case" is present and TRUE then case is ignored.
      STR(len=*), IN :: self
      STR(len=*), IN :: string
      BIN, IN, optional :: ignore_case
      BIN :: same
      BIN :: ignore
      STR(len=len(self)) :: s1
      STR(len=len(string)) :: s2
      ignore = FALSE
      if (present(ignore_case)) ignore = ignore_case
      if (ignore) then
         s1 = self; call to_lower_case_(s1)
         s2 = string; call to_lower_case_(s2)
         same = s1==s2
      else
         same = self==string
      end if
   end function

   pure function is_real(self) result (res)
   ! Returns true if the string can be interpreted as a real number
      STR(len=*), IN :: self
      BIN :: res
      REAL :: value
      INT :: i,ios
      i = index_of_character_in_(self,"0123456789")
      if (i==0) then
         res = FALSE
      else
         read(unit=self,fmt=*,iostat=ios) value
         res = ios==0
         if (res) value = value
      end if
   end function

   elemental function is_cpx(self) result (res)
   ! Returns true if the string can be interpreted as a (fortran) complex number
      STR(len=*), IN :: self
      BIN :: res
      res = is_a_true_cpx_(self) OR is_a_real_pair_(self)
   end function

   elemental function is_a_true_cpx(self) result (res)
   ! Returns true if the string can be interpreted as a true (fortran) complex
   ! number
      STR(len=*), IN :: self
      BIN :: res
      CPX :: value
      INT :: i,ios
      i = index_of_character_in_(self,"0123456789")
      if (i==0) then
         res = FALSE
      else if ((self(1:1)=="(" OR self(1:2)=="-(") AND scan(self,",")>1) then
         read(unit=self,fmt=*,iostat=ios) value
         value = value
         res = ios==0
      else
         res = FALSE
      end if
   end function

   elemental function is_a_real_pair(self) result (res)
   ! Returns true if the string can be interpreted as a pair of
   ! double precision numbers comprising a complex number
      STR(len=*), IN :: self
      BIN :: res
      INT :: i,ios
      REAL :: r,c
      i = index_of_character_in_(self,"0123456789")
      if (i==0) then
         res = FALSE
      else
         read(unit=self,fmt=*,iostat=ios) r,c
         r = r
         c = c
         res = ios==0
      end if
   end function

   elemental function is_int(self) result (res)
   ! Returns true if the string can be interpreted as an integer number
      STR(len=*), IN :: self
      BIN :: res
      INT :: value,ios
      BIN :: char
      char = NOT has_any_characters_in_(self(1:1),"0123456789+-")
      if (char) then
         res = FALSE
      else
         read(unit=self,fmt=*,iostat=ios) value
         value = value
         res = ios==0
      end if
   end function

   elemental function is_a_true_int(self) result (res)
   ! Returns true if the string can be interpreted as an integer number
      STR(len=*), IN :: self
      BIN :: res
      INT :: value,ios
      if (has_all_characters_in_(self,"0123456789-+ ")) then
         read(unit=self,fmt=*,iostat=ios) value
         value = value
         res = ios==0
      else
         res = FALSE
      end if
   end function

   elemental function is_bin(self) result (res)
   ! Returns true if the string can be interpreted as a logical
      STR(len=*), IN :: self
      BIN :: res
      STR(len=len(self)) :: word
      read(unit=self,fmt=*) word
      call to_lower_case_(word)
      res = FALSE
      select case (word)
      case ("true","t","on","yes","y"); res = TRUE
      case ("false","f","off","no","n"); res = TRUE
      case default; res = FALSE
      end select
   end function

   elemental function is_imprecise_real(self) result (res)
   ! Returns TRUE if the string can be interpreted as an imprecise double, i.e.
   ! a real fortran F "number" followed immediately (without intervening spaces)
   ! by a quoted "error" in parentheses. 
      STR(len=*), IN :: self
      BIN :: res
      INT :: f,l
      f = index_of_substring_(self,"(")
      l = index_of_substring_(self,")")
      if (f==0 OR (l-f)<=1) then  ! there is no error
         res = FALSE
      else
         res = is_real_(self(1:f-1)) AND is_real_(self(f+1:l-1))
      end if
   end function

   elemental function is_alphabetical(self) result (res)
   ! Returns true if the string contains only alphabetical characters.
      STR(len=*), IN :: self
      BIN :: res
      res = index_of_character_not_in_(self,letters)==0
   end function

   elemental function is_lower_case(self) result (res)
   ! Returns true if the string has only lower case alphabetical characters.
      STR(len=*), IN :: self
      BIN :: res
      res = index_of_character_not_in_(self,lowercase)==0
   end function

   elemental function is_upper_case(self) result (res)
   ! Returns true if the string has only upper case alphabetical characters.
      STR(len=*), IN :: self
      BIN :: res
      res = index_of_character_not_in_(self,uppercase)==0
   end function

   elemental function is_numeric(self) result (res)
   ! Returns true if the string contains only digit characters.
      STR(len=*), IN :: self
      BIN :: res
      res = index_of_character_not_in_(self,numeric)==0
   end function

   elemental function is_alphanumeric(self) result (res)
   ! Returns true if the string contains only alphanumeric characters.
      STR(len=*), IN :: self
      BIN :: res
      res = index_of_character_not_in_(self,alphanumeric)==0
   end function

   elemental function has_some_alpha_characters(self) result (res)
   ! Return TRUE if self has some alphabetical characters
      STR(len=*), IN :: self
      BIN :: res
      res = index_of_character_in_(self,letters)/=0
   end function

   elemental function has_some_alphanumeric_chars(self) result (res)
   ! Return TRUE if self has some alphabetical characters
      STR(len=*), IN :: self
      BIN :: res
      res = index_of_character_in_(self,alphanumeric)/=0
   end function


!  =============
!  String widths
!  =============

   pure function no_of_fields_per_value(self) result (res)
   ! Returns the number of fields used to print a "self".
      STR(len=*), IN :: self
      INT :: res
      if (FALSE) res = transfer(self,res)
      res = 1
   end function

   pure function str_length(self) result (res)
   ! Returns the minimal string length.
      STR(len=*), IN :: self
      INT :: res
      res = len_trim(self)
   end function

   pure subroutine get_str_length(self,sl,spaces)
   ! Returns the minimal string length.
      STR(len=*), IN :: self
      INT, OUT :: sl
      INT, IN :: spaces
      sl = len_trim(self)+spaces
   end subroutine


!  =================
!  String conversion
!  =================

   pure function to_quoted_str(self) result (string)
   ! Change self to a quoated string if it has blanks.
      STR(len=*), IN :: self
      STR(len=(len_trim(self)+2)) :: string
      string = trim_blanks_from_start_(self)
      if (includes_blanks_(self)) string = '"'//trim(self)//'"'
   end function

   pure function to_str_0(self) result (string)
   ! Change self to a string of minimal length
      STR(len=*), IN :: self
      STR(len=len_trim(self)) :: string
      string = self
   end function

   pure function to_str_1(self,fmt,left_justify) result (string)
   ! Change self to a "string" using the specified fortran "fmt".
      STR(len=*), IN :: self
      STR(len=*), IN :: fmt
      BIN, IN, optional :: left_justify
      STR(len=STR_SIZE) :: string
      BIN :: leftify

      ! Use the given format

      ! The following code is inherited from INTRINSIC
      string = " "
      write(string,fmt="("//trim(fmt)//")") self

      ! Left justify
      leftify = TRUE
      if (present(left_justify)) leftify = left_justify
      if (leftify) string = adjustl(string)
   end function

   pure function to_str_2(self,width,left_justify) result (string)
   ! Change self to a string of the specified width
   ! Just takes the first width characters
      STR(len=*), IN :: self
      INT, IN :: width
      BIN, IN, optional :: left_justify
      STR(len=width) :: string
      string = self
      if (present(left_justify)) then
         if (NOT left_justify) then
            string = adjustr(string)
         end if
      end if
   end function

   pure subroutine to_fortran_format(self,nval,style,width,precision)
   ! Change self to a string with specified "style", "width" and
   ! "precision" as defined in the fortran standard
      STR(len=*), OUT :: self
      INT, IN :: nval,width,precision
      STR(len=*), IN :: style
      STR(len=STR_SIZE) :: num,wid,pre

      ! Write width/style/precision as a strings
      write(num,fmt=*) nval; num = adjustl(num)
      write(wid,fmt=*) width; wid = adjustl(wid)
      write(pre,fmt=*) precision; pre = adjustl(pre)

      ! Write the format "fmt"
      write(self,fmt="(a)") "("//trim(num)//trim(style)//trim(wid)//"."//trim(pre)//")"
   end subroutine


!  ==================================
!  Conversion to value type variables
!  ==================================

   elemental function to_real(self) result (value)
   ! Returns the real number corresponding to the first token string
      STR(len=*), IN :: self
      REAL :: value
      INT :: ios
      read(unit=self,fmt=*,iostat=ios) value
      ios = ios
      value = value
   end function

   elemental function frac_to_real(self) result (value)
   ! Returns the real number corresponding to the first token string,
   ! represented as a fraction.
      STR(len=*), IN :: self
      REAL :: value
      STR(len=len(self)) :: numerator,denominator,word
      REAL :: numer,denom
      INT :: ind
      word = self
      call remove_blanks_(word)
      call separate_before_(word,"/")
      call separate_after_(word,"/")
      ind = index_of_character_in_(word,"/")
      numerator = word(1:ind-1)
      denominator = word(ind+1:)

      ! Get the fraction
      numer = to_real_(numerator)
      denom = to_real_(denominator)
      value = numer/denom
   end function

   elemental function to_cpx(self) result (value)
   ! Returns the cpx number corresponding to the token string
   ! If a single token won't do, two tokens are inputted to see
   ! if it could be two real numbers in a row representing a cpx.
      STR(len=*), IN :: self
      CPX :: value
      REAL :: r,c
      value = ZERO
      if (is_a_true_cpx_(self)) then
         read(unit=self,fmt=*) value
      else if (is_a_real_pair_(self)) then
         read(unit=self,fmt=*) r,c
         value = cmplx(r,c,kind=CPX_KIND)

   ! else
   !    DIE("Could not read complex number")
      end if
   end function

   elemental function to_int(self) result (value)
   ! Returns the integer number corresponding to the first token string
      STR(len=*), IN :: self
      INT :: value
      INT :: ios
      read(unit=self,fmt=*,iostat=ios) value
      ios = ios
      value = value
   end function

   elemental function to_bin(self) result (value)
   ! Returns the logical corresponding to the first token string
      STR(len=*), IN :: self
      BIN :: value
      STR(len=len(self)) :: word
      value = FALSE
      word = self
      call to_lower_case_(word)
      value = FALSE
      select case (word)
      case ("true","t","on","yes","y"); value = TRUE
      case ("false","f","off","no","n"); value = FALSE
      end select
   end function

   PURE subroutine to_imprecise_real(self,value,error)
   ! Returns the imprecise number corresponding to the first token, i.e. the
   ! real number "value" with a quoted "error" in parentheses immediately
   ! afterwards.  If the error is not present in the string it is assumed to be
   ! zero. This only works for "f" numbers.
      STR(len=*), IN :: self
      REAL, OUT :: value,error
      STR(len=len(self)) :: item,real_str,err_str
      INT :: f,l,p

      ! Analyze the string
      call get_next_item_(self,item)
      f = index_of_substring_(item,"(")
      l = index_of_substring_(item,")")
      p = index_of_substring_(item,".")

      ! Extract value & error
      if (f==0 OR (l-f)<=1) then

         ! There is no error
         real_str = item
         err_str = "0"
      else

         ! Brackets present
         real_str = item(1:f-1)
         err_str = item(f+1:l-1)
      end if
      ENSURE(is_real_(real_str),"STR:to_imprecise_real ... expected real number in input")
      ENSURE(is_real_(err_str),"STR:to_imprecise_real ... expected real number error in input")

      ! Convert
      value = to_real_(real_str)
      error = to_real_(err_str)
      if (p>0) error = error*TEN**(-f+p+1)
   end subroutine


!  ============================
!  Conversion to self variables
!  ============================

   pure subroutine from_int(self,value)
   ! Set the original string to the result from changing integer
   ! "value" to a string
      STR(len=*), OUT :: self
      INT, IN :: value
      self = " "
      write(self,fmt=*) value
      call left_justify_(self)
   end subroutine

   pure subroutine from_bin(self,value)
   ! Set the original string to the result from changing logical
   ! "value" to a string
      STR(len=*), OUT :: self
      BIN, IN :: value
      self = " "
      write(self,*) value
      call left_justify_(self)
   end subroutine

   pure subroutine from_real(self,value)
   ! Set the original string to the result from changing real "value"
   ! to a string
      STR(len=*), OUT :: self
      REAL, IN :: value
      self = " "
      write(self,*) value
      call left_justify_(self)
   end subroutine

   pure subroutine from_cpx(self,value)
   ! Set the original string to the result from changing cpx "value"
   ! to a string
      STR(len=*), OUT :: self
      CPX, IN :: value
      self = " "
      write(self,*) value
      call left_justify_(self)
   end subroutine


!  ================
!  Units conversion
!  ================

   pure function is_known_unit(self) result (res)
   ! Return TRUE if the string represents a known unit string
      STR(len=*), IN :: self
      BIN :: res
      STR(len=len(self)) :: word
      INT :: l
      word = self
      call to_lower_case_(word)
      l = len_trim(word)
      if (word(l:l)=="s" AND l>1) word = word(1:l-1)
      res = is_one_of_(word,["amu             ","angstrom        ","angstroms       ","angstrom^2      ","angstrom^3      ","angstrom^4      ","angstrom^-1     ","angstrom^-3     ","angstrom^-5     ","bohr            ","bohrs           ","cm-1            ","debye           ","debye-angstrom  ","debye-angstrom^2","degree          ","degrees         ","ev              ","gb              ","joule           ","kelvin          ","kilojoule       ","kjoule          ","kj              ","kj/mol          ","kj.mol^-1       ","kcal/mol        ","kcal.mol^-1     ","mb              ","mw              ","meter           ","meters          ","radian          ","radians         ","wavenumber      "])
   end function

   PURE function conversion_factor(self) result (res)
   ! Return the conversion factor which converts a value *into* the specified
   ! unit "self", assuming that the value has default units. In most cases
   ! the default units are atomic units, or radians for angles, and words for
   ! memory.
      STR(len=*), IN :: self
      REAL :: res
      STR(len=len(self)) :: word
      INT :: l
   ENSURE(is_known_unit_(self),"STR:conversion_factor ... unknown units, "//self)
      res = ONE
      word = self
      call to_lower_case_(word)
      l = len_trim(word)
      if (word(l:l)=="s" AND l>1) word = word(1:l-1)
      select case (word)
      case ("amu             "); res = AMU_PER_MASS_OF_ELECTRON
      case ("angstrom        "); res = ANGSTROM_PER_BOHR
      case ("angstroms       "); res = ANGSTROM_PER_BOHR
      case ("angstrom^2      "); res = ANGSTROM_PER_BOHR**2
      case ("angstrom^3      "); res = ANGSTROM_PER_BOHR**3
      case ("angstrom^4      "); res = ANGSTROM_PER_BOHR**4
      case ("angstrom^-1     "); res = BOHR_PER_ANGSTROM
      case ("angstrom^-2     "); res = BOHR_PER_ANGSTROM**2
      case ("angstrom^-3     "); res = BOHR_PER_ANGSTROM**3
      case ("angstrom^-4     "); res = BOHR_PER_ANGSTROM**3
      case ("angstrom^-5     "); res = BOHR_PER_ANGSTROM**5
      case ("bohr            "); res = BOHR_PER_BOHR
      case ("bohrs           "); res = BOHR_PER_BOHR
      case ("cm^-1           "); res = WAVENUMBER_PER_HARTREE
      case ("debye           "); res = DEBYE_PER_AU
      case ("debye-angstrom  "); res = DEBYE_PER_AU*(ANGSTROM_PER_BOHR)
      case ("debye-angstrom^2"); res = DEBYE_PER_AU*(ANGSTROM_PER_BOHR)**2
      case ("degree          "); res = DEGREE_PER_RADIAN
      case ("degrees         "); res = DEGREE_PER_RADIAN
      case ("ev              "); res = EV_PER_HARTREE
      case ("gb              "); res = EIGHT/(1024*1024*1024)
      case ("joule           "); res = JOULE_PER_HARTREE
      case ("kelvin          "); res = KELVIN_PER_HARTREE
      case ("kilojoule       "); res = KJOULE_PER_HARTREE
      case ("kjoule          "); res = KJOULE_PER_HARTREE
      case ("kj              "); res = KJOULE_PER_HARTREE
      case ("kj/mol          "); res = KJOULEMOL_PER_HARTREE
      case ("kj.mol^-1       "); res = KJOULEMOL_PER_HARTREE
      case ("kcal/mol        "); res = KCALMOL_PER_HARTREE
      case ("kcal.mol^-1     "); res = KCALMOL_PER_HARTREE
      case ("mb              "); res = EIGHT/(1024*1024)
      case ("mw              "); res = ONE/(1024*1024)
      case ("meter           "); res = ANGSTROM_PER_BOHR*METER_PER_ANGSTROM
      case ("meters          "); res = ANGSTROM_PER_BOHR*METER_PER_ANGSTROM
      case ("radian          "); res = RADIAN_PER_DEGREE
      case ("radians         "); res = RADIAN_PER_DEGREE
      case ("wavenumber      "); res = WAVENUMBER_PER_HARTREE
      end select
   end function

   pure function is_one_of(self,allowed) result (res)
   ! Return TRUE if "self" is one of the strings in "allowed".
      STR(len=*), IN :: self
      VEC(STR(len=*),:), IN :: allowed
      BIN :: res

      ! Ignore case
      res = any(lower_cased(self)==lower_cased(allowed))
   end function

   pure function character_array(self,length) result (res)
   ! Convert the string to an array of dimension "length" of len=1 characters
      STR(len=*), IN :: self
      INT, IN :: length
      STR(len=1), dimension(length) :: res
      INT :: i
      do i = 1,length
         if (i<len(self)) then
            res(i) = self(i:i)
         else
            res(i) = " "
         end if
      end do
   end function

end module
