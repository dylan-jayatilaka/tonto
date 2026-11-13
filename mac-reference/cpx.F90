!===================================================================
!
! CPX: Complex numbers
!
! Copyright (C) Dylan Jayatilaka, 1998
! Copyright (C) Dylan Jayatilaka, 2023
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
module CPX_MODULE

#  include "macros"

   include "cpx.use"

   implicit none

   include "cpx.int"


contains

! =============
! String widths
! =============

   pure function no_of_fields_per_value(self) result (res)
   ! Returns the number of fields used to print a "self".
      CPX, IN :: self
      INT :: res
      if (FALSE) res = transfer(self,res)
      res = 2
   end function

   pure function str_length_0(self) result (res)
   ! Returns the minimal string length.
   ! Note: for real and complex only
      CPX, IN :: self
      INT :: res
      STR(len=STR_SIZE) :: string

      ! The following code is inherited from INTRINSIC
      string = " "
      write(string,fmt=*) self
      res = len_trim(adjustl(string))
      if (real(self)<ZERO) res = res+1
   end function

   pure function str_length_1(self,dp,spaces) result (res)
   ! Return *twice* the string length from the int part of the REAL
   ! value "self", keeping "dp" decimal places with extra "spaces" 
   ! (if present).  The resuls is twice that for a real number.
      CPX, IN :: self
      INT, IN :: dp
      INT, optional, IN :: spaces
      INT :: res
      REAL :: val
      INT :: wid
      val = REALIFY(self)
      wid = str_length_(val,dp)
      if (present(spaces)) wid = wid+spaces
      res = no_of_fields_per_value_(self)*wid
   end function

   pure subroutine get_str_length(self,sl,spaces)
   ! Returns the minimal string length.
   ! Note: for real and complex only
      CPX, IN :: self
      INT, OUT :: sl
      INT, IN :: spaces
      STR(len=STR_SIZE) :: string

      ! The following code is inherited from INTRINSIC
      string = " "
      write(string,fmt=*) self
      sl = len_trim(adjustl(string))+spaces
      if (real(self)<ZERO) sl = sl+1
   end subroutine

   pure subroutine get_str_length_maxdp(self,sl,max_dp,spaces)
   ! Return "sl" as *twice* the string length from the int part of the
   ! REAL value "self", keeping "max_dp" decimal places with extra
   ! "spaces" (if present). The resuls is twice that for a real.
      CPX, IN :: self
      INT, OUT :: sl
      INT, IN :: max_dp
      INT, IN :: spaces
      REAL :: val
      INT :: wid
      val = REALIFY(self)
      call get_str_length_maxdp_(val,wid,max_dp,spaces)
      sl = no_of_fields_per_value_(self)*wid
   end subroutine


! =================
! String conversion
! =================

! This is for INTEL_ifort

   pure function to_str_cpx_0(self) result (string)
   ! Change self to a string of minimal length
      CPX, IN :: self
      STR(len=str_length_(self)) :: string
      STR(len=STR_SIZE) :: tmpcpy

      ! Write self to STR into "tmpcpy"

      ! The following code is inherited from INTRINSIC
      tmpcpy = " "
      write(tmpcpy,fmt=*) self

      ! Transfer "tmpcpy" to the "string" with correct length
      write(string,fmt="(a)") trim(adjustl(tmpcpy))
   end function

   pure function to_str_cpx_1(self,fmt,left_justify) result (string)
   ! Change self to a "string" using the specified fortran "fmt".
      CPX, IN :: self
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

   pure function to_str_cpx_2(self,style,width,precision,left_justify) result (string)
   ! Change self to a string with specified fortran "style", in a
   ! field of "width" characters and "precision" decimal points.
      CPX, IN :: self
      STR(len=*), IN :: style
      INT, IN :: width
      INT, IN :: precision
      BIN, IN, optional :: left_justify
      STR(len=2*width) :: string
      BIN :: leftify
      STR(len=STR_SIZE) :: wid,pre,fmt
      INT :: l

      ! Write width/style/precision as a strings
      ! NOTE: max on precision is a hack

      ! The following code is inherited from INTRINSIC
      write(wid,fmt=*) width; wid = adjustl(wid)
      write(pre,fmt=*) max(precision,0); pre = adjustl(pre)

      ! Write the format "fmt" (factor 2 for cpx)
      write(fmt,fmt="(a)") "(2"//trim(style)//trim(wid)//"."//trim(pre)//")"

      ! Use the "fmt" to get the result
      string = " "
      write(string,fmt=trim(fmt)) self

      ! Remove decimal point at end?
      l = len_trim(string)
      if (string(l:l)==".") string = string(1:l-1)

      ! Left justify
      leftify = TRUE
      if (present(left_justify)) leftify = left_justify
      if (leftify) string = adjustl(string)
   end function


! ================
! Equality testing
! ================

   pure function equals(self,c,eps) result (res)
   ! Test to see if "self" is the same as "c"
      CPX, IN :: self
      CPX, IN :: c
      REAL, optional, IN :: eps
      BIN :: res
      res = same_as_(self,c,eps)
   end function

   pure function same_as_0(self,c,eps) result (res)
   ! Test to see if "self" is the same as "c"
      CPX, IN :: self
      CPX, IN :: c
      REAL, optional, IN :: eps
      BIN :: res
      REAL :: tol
      tol = REAL_EPSILON
      if (present(eps)) tol = abs(eps)
      res = abs(self-c)<tol
   end function

   pure function same_as_1(self,r,eps) result (res)
   ! Test to see if "self" is the same as "c"
      CPX, IN :: self
      REAL, IN :: r
      REAL, optional, IN :: eps
      BIN :: res
      REAL :: tol
      tol = REAL_EPSILON
      if (present(eps)) tol = abs(eps)
      res = abs(self-r)<tol
   end function

   pure function is_zero(self,eps) result (res)
   ! Return TRUE if self is near enough zero. If present, "eps" defines how
   ! close to zero before the number is considered zero.
      CPX, IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      REAL :: tol
      tol = REAL_EPSILON
      if (present(eps)) tol = abs(eps)
      res = abs(self)<tol
   end function

   pure function is_one(self,eps) result (res)
   ! Return TRUE if self is near enough one. If present, "eps" defines how
   ! close to one before the number is considered zero.
      CPX, IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      REAL :: tol
      tol = REAL_EPSILON
      if (present(eps)) tol = abs(eps)
      res = abs(self-ONE)<tol
   end function

   pure function is_minus_one(self,eps) result (res)
   ! Return TRUE if self is near enough minus one. If present, "eps" defines how
   ! close to minus one before the number is considered zero.
      CPX, IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      REAL :: tol
      tol = REAL_EPSILON
      if (present(eps)) tol = abs(eps)
      res = abs(self+ONE)<tol
   end function

end module
