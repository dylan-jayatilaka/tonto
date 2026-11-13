!---------------------------------------------------------------------------
!
! BIN: methods which apply to logical (BIN) variables
!
! Copyright (C) Dylan Jayatilaka, 2011
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
! $Id: bin.foo dylan_ $
!---------------------------------------------------------------------------
module BIN_MODULE

#  include "macros"

   include "bin.use"

   implicit none

   include "bin.int"


contains

! =============
! String widths
! =============

   pure function no_of_fields_per_value(self) result (res)
   ! Returns the number of fields used to print a "self".
      BIN, IN :: self
      INT :: res
      if (FALSE) res = transfer(self,res)
      res = 1
   end function

   pure function str_length(self) result (res)
   ! Returns the minimal string length.
   ! Note: for non-real and non-complex only
      BIN, IN :: self
      INT :: res
      STR(len=STR_SIZE) :: string

      ! The following code is inherited from INTRINSIC
      string = " "
      write(string,fmt=*) self
      res = len_trim(adjustl(string))
   end function

   pure subroutine get_str_length(self,sl,spaces)
   ! Returns the minimal string length.
   ! Note: for non-real and non-complex only
      BIN, IN :: self
      INT, OUT :: sl
      INT, IN :: spaces
      STR(len=STR_SIZE) :: string

      ! The following code is inherited from INTRINSIC
      string = " "
      write(string,fmt=*) self
      sl = len_trim(adjustl(string))+spaces
   end subroutine


! =================
! String conversion
! =================

   pure function to_str_0(self) result (string)
   ! Change self to a string of minimal length
      BIN, IN :: self
      STR(len=str_length_(self)) :: string
      STR(len=STR_SIZE) :: tmpcpy

      ! Write self to STR into "tmpcpy"

      ! The following code is inherited from INTRINSIC
      tmpcpy = " "
      write(tmpcpy,fmt=*) self

      ! Transfer "tmpcpy" to the "string" with correct length
      write(string,fmt="(a)") trim(adjustl(tmpcpy))
   end function

   pure function to_str_1(self,fmt,left_justify) result (string)
   ! Change self to a "string" using the specified fortran "fmt".
      BIN, IN :: self
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
   ! Change self to a string with specified "width".
      BIN, IN :: self
      INT, IN :: width
      BIN, IN, optional :: left_justify
      STR(len=width) :: string
      BIN :: leftify
      STR(len=STR_SIZE) :: wid,fmt

      ! Write width as a string, in "wid"

      ! The following code is inherited from INTRINSIC
      write(wid,fmt="(a)") width

      ! Write the format "fmt" from "wid"
      write(fmt,fmt="(a)") "("//"L"//trim(wid)//")"

      ! Use the format "fmt" to get the result
      string = " "
      write(string,fmt=fmt) self

      ! Left justify
      leftify = TRUE
      if (present(left_justify)) leftify = left_justify
      if (leftify) string = adjustl(string)
   end function

end module
