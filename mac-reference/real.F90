!===================================================================
!
! REAL: methods which apply to double precision numbers
!
! Copyright (C) Daniel Grimwood, 1999
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
module REAL_MODULE

#  include "macros"

   include "real.use"

   implicit none

   include "real.int"


   ! For Fermi-Dirac distribution
   ! NOTE: Energies and temperatures are in Hartree
   REAL, private :: E_Fermi
   REAL, private :: T_Fermi
   VEC(REAL,:), allocatable, private :: E_level
   INT, private :: n_electrons

   ! For calculating the series expansion of the ln Gamma function
   VEC(REAL,0:6), private :: ln_gamma_coefficients
   data ln_gamma_coefficients(0:6)/ &
       1.000000000190015d0, &
       76.18009172947146d0, -86.50532032941677d0, 24.01409824083091d0, &
      -1.231739572450155d0,0.1208650973866179d-2, -0.5395239384953d-5/

! gill_T1  :: VEC{REAL}*, private  DEFAULT_NULL
! gill_m   :: INT, private   DEFAULT(0)
! gill_eps :: REAL, private  DEFAULT(ZERO)

contains

!  ====================
!  Numerical operations
!  ====================

   pure subroutine plus(self,val)
   ! Add "val" to self
      REAL, INOUT :: self
      REAL, IN :: val
      self = self+val
   end subroutine

   pure subroutine minus(self,val)
   ! Subtract "val" to self
      REAL, INOUT :: self
      REAL, IN :: val
      self = self-val
   end subroutine

   pure subroutine times(self,val)
   ! Multiply "val" by self
      REAL, INOUT :: self
      REAL, IN :: val
      self = self*val
   end subroutine

   pure function raised_to(self,n) result (res)
   ! Raise "self" to the power "n"
      REAL, IN :: self
      INT, IN :: n
      REAL :: res
      INT :: i
      res = ONE
      do i = 1,abs(n)
         res = res*self
      end do
      if (n<0) res = ONE/res
   end function

   pure subroutine swap_with(self,x)
   ! Swap the value of "self" and "x"
      REAL, INOUT :: self
      REAL, INOUT :: x
      REAL :: keep
      keep = self
      self = x
      x = keep
   end subroutine

   pure subroutine to_sum_of_0(self,v)
   ! Set "self" to the sum of elements in vector "v"
      REAL, OUT :: self
      VEC(REAL,:), IN :: v
      INT :: i

      ! The following code is inherited from INTRINSIC
      self = 0
      do i = 1,size(v)
         self = self+v(i)
      end do
   end subroutine

   pure subroutine to_sum_of_1(self,mx)
   ! Set "self" to the sum of elements in matrix "mx"
      REAL, OUT :: self
      MAT(REAL,:,:), IN :: mx
      INT :: i,j

      ! The following code is inherited from INTRINSIC
      self = 0
      do j = 1,size(mx,2)
         do i = 1,size(mx,1)
            self = self+mx(i,j)
         end do
      end do
   end subroutine


!  =============
!  String widths
!  =============

   pure function no_of_fields_per_value(self) result (res)
   ! Returns the number of fields used to print a "self".
      REAL, IN :: self
      INT :: res
      if (FALSE) res = transfer(self,res)
      res = 1
   end function

   pure function str_length_0(self) result (res)
   ! Returns the minimal string length.
   ! Note: for real and complex only
      REAL, IN :: self
      INT :: res
      STR(len=STR_SIZE) :: string

      ! The following code is inherited from INTRINSIC
      string = " "
      write(string,fmt=*) self
      res = len_trim(adjustl(string))
      if (real(self)<ZERO) res = res+1
   end function

   pure function str_length_1(self,max_dp) result (sl)
   ! Return the string length "sl" from the int part of the REAL value
   ! "self", keeping "max_dp" decimal places.
      REAL, IN :: self
      INT, IN :: max_dp
      INT :: sl
      call get_str_length_maxdp_(self,sl,max_dp,0)
   end function

   pure subroutine get_str_length(self,sl,spaces)
   ! Returns the minimal string length.
   ! Note: for real and complex only
      REAL, IN :: self
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
   ! Return the string length "sl" from the int part of the REAL value
   ! "self", keeping "max_dp" decimal places, with extra "spaces".
      REAL, IN :: self
      INT, OUT :: sl
      INT, IN :: max_dp
      INT, IN :: spaces
      INT :: si

      ! Get the length of the integer part
      call get_si_(self,si)

      ! Add on decimal places & .
      sl = si+max_dp+1  ! plus .
      sl = sl+spaces
   end subroutine

   pure subroutine get_str_length_noalign(self,error,sl,dp,max_dp,d2_err)
   ! Return the string length "sl" of "self", assuming "self" has an
   ! absolute "error"s. If "error" is zero, "max_dp" decimal places
   ! are kept. NOTE: This version does not add alignment spaces.
      REAL, IN :: self
      REAL, IN :: error
      INT, OUT :: sl,dp
      INT, IN :: max_dp
      BIN, optional, IN :: d2_err  ! 2-digit error always?
      INT :: si,de,le

      ! Get error decimal place "dp", error digit "de" & length "le"
      call get_dp_de_le(error,dp,de,le,max_dp,two_digit_error=d2_err)

      ! Get the length of the integer part
      call get_si_(self,si)

      ! Get the string length "sl"
      sl = si+dp+1  ! plus .
      sl = sl+le+2  ! plus ( & )
   end subroutine

   pure subroutine get_str_length_align(self,error,sl,dp,de,le,cb,max_dp,max_cb,d2_err)
   ! If "self" is changed into a "string" with style "f" with the no. of 
   ! decimal places derived from the "error" in "self", but with the result 
   ! not exceeding "max_dp" decimal places, THEN return 
   ! . the string length "sl" of "self" 
   ! . the decimal place "dp" of the error 
   ! . the digit(s) "de" comprising the error
   ! . the length "le" of these errror digits (which is either 1 or 2)
   ! . the position "cb" of the closing brace after the decimal point.  
   ! If "error" is zero, "max_dp" decimal places are kept. 
   ! If "max_cb" is present, use it to pad "sl".
   ! If "d2_err" is present and TRUE 2 digits are used for error "de"
      REAL, IN :: self
      REAL, IN :: error
      INT, OUT :: sl
      INT, OUT :: dp
      INT, OUT :: de
      INT, OUT :: le
      INT, OUT :: cb
      INT, IN :: max_dp
      INT, optional, IN :: max_cb
      BIN, optional, IN :: d2_err
      INT :: si

      ! Get error decimal place "dp", error digit "de" & length "le"
      call get_dp_de_le(error,dp,de,le,max_dp,two_digit_error=d2_err)

      ! Get the length of the integer part
      call get_si_(self,si)

      ! Get string length "sl"
      sl = si+dp+1  ! plus .
      sl = sl+le+2  ! plus ( & )

      ! Get closing brace pos
      cb = dp+le+2

      ! Add pad characters to align dp
      if (present(max_cb)) then
         sl = sl+max_cb-cb
      end if
   end subroutine

   pure subroutine get_si(self,si)
   ! For a number "self" return the length of the integer part in
   ! front of the real number, including the sign.
      REAL, IN :: self
      INT, OUT :: si
      INT :: sf

      ! Integer part
      sf = floor(abs(self))

      ! Length of integer part
      call get_str_length_(sf,si,0)

      ! Add extra for -ve sign
      if (self<ZERO) si = si+1
   end subroutine

   pure subroutine get_dp_de_le(error,dp,de,le,max_dp,two_digit_error)
   ! For an absolute "error" return 
   ! . the decimal place "dp" of the error
   ! . the digit(s) "de" comprising the error, and 
   ! . the length "le" of the error digits (which is either 1 or 2)
   ! As input:
   ! . At most "max_dp" decimal places are kept
   ! . If "two_digit_error" is TRUE, 2 error digits are kept in "de"
      REAL, IN :: error
      INT, OUT :: dp
      INT, OUT :: de
      INT, OUT :: le
      INT, IN :: max_dp
      BIN, optional, IN :: two_digit_error
      BIN :: one_digit_error
      REAL :: abs_error

      ! Callers pass small negative esds (least-squares noise); log10 of a
      ! negative number is undefined and formatted differently per platform.
      ! The magnitude makes the result well defined. History in TASKS_AND_HISTORY.md.
      abs_error = abs(error)

      ! "NOT (x>0)" rather than "x==0": it catches zero and NaN in one test,
      ! since every comparison with a NaN is false, and callers do pass NaN esds.
      if (NOT (abs_error>ZERO)) then

         ! Zero error like (0)
         dp = max_dp
         de = 0
         le = 1
      else

         ! 2-digit error like (19)
         dp = min(abs(floor(log10(abs_error))),max_dp)+1
         de = nint(abs_error*10**dp)
         le = 2
         one_digit_error = TRUE
         if (present(two_digit_error)) one_digit_error = NOT two_digit_error

         ! Reduce to 1-digit error like (7) for Acta Cryst?
         if (one_digit_error) then
            if (20<=de AND de<=100) then
               dp = dp-1
               de = nint(abs_error*10**dp)
               le = 1
               if (de>=10) le = 2
            end if
         end if
      end if
   end subroutine


!  =================
!  String conversion
!  =================

   pure function to_str_0(self) result (string)
   ! Change self to a string of minimal length
      REAL, IN :: self
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
      REAL, IN :: self
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

   pure function to_str_2(self,style,width,precision,left_justify) result (string)
   ! Change self to a string with specified fortran "style", in a
   ! field of "width" characters and "precision" decimal points.
      REAL, IN :: self
      STR(len=*), IN :: style
      INT, IN :: width
      INT, IN :: precision
      BIN, IN, optional :: left_justify
      STR(len=1*width) :: string
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

   PURE function to_str_3(self,error,width,max_dp,max_cb,d2_err) result (string)
   ! Return "self" as a "string" with style "f" with the no. of decimal
   ! places derived from the "error" in "self", but with the result not
   ! exceeding "max_dp" decimal places. Used for output like:
   !    1.027(5)   ... which gives dp=3 and de=5
   !    1.0270(19) ... which gives dp=4 and de=19
   ! (provided "max_dp">=4). The errors are padded up to "max_cb" chars so 
   ! that for a given fixed "width", all decimal points would be aligned.
   ! If "d2_err" is present and TRUE, 2 error digits are used.
      REAL, IN :: self
      REAL, IN :: error
      INT, IN :: width
      INT, IN :: max_dp
      INT, optional, IN :: max_cb
      BIN, optional, IN :: d2_err
      STR(len=width) :: string  ! note width
      INT :: sl,dp,de,le,cb,pad

   ! si :: INT
   ! str :: STR

      ! Get the decimal place "dp" and the error digit "de"
      call get_str_length_align_(self,error,sl,dp,de,le,cb,max_dp,d2_err=d2_err)

      ! str = " "
      ! write(str,fmt="(i32)") floor(self)
      ! str = adjustl(str)
      ! si = len_trim(str)

      ! write(99,*) '========='
      ! write(99,*) 'self    =',self
      ! write(99,*) 'error   =',error
      ! write(99,*) 'str     =',trim(str)
      ! write(99,*) 'si      =',si          
      ! .get_si(si)
      ! write(99,*) 'si      =',si          
      ! write(99,*) 'sl      =',sl
      ! write(99,*) 'dp      =',dp
      ! write(99,*) 'de      =',de
      ! write(99,*) 'le      =',le
      ! write(99,*) 'cb      =',cb 
      ! write(99,*) 'width   =',width 
      ! write(99,*) 'max_dp  =',max_dp

      ! write(99,*) 'max_cb  =',max_cb

      ! Convert "self" with the right size
      string = to_str_(self,"f",width,precision=dp)

      ! Append error digits (and spaces, see comments above)
      string = trim(string)//"("//trim(to_str_(de))//")"

      ! Adjust to the right
      string = adjustr(string)

      ! Add padding and shift left
      if (present(max_cb)) then
         pad = max_cb-cb
         if (pad>0) then
            string = trim(string(pad+1:))//repeat(" ",pad)
         end if
      end if
   end function

   PURE subroutine get_str(self,string,error,width,max_dp,max_cb,d2_err)
   ! Return "self" as a "string" with style "f" with the no. of decimal
   ! places derived from the "error" in "self", but with the result not
   ! exceeding "max_dp" decimal places. Used for output like:
   !    1.027(5)   ... which gives dp=3 and de=5
   !    1.0270(19) ... which gives dp=4 and de=19
   ! (provided "max_dp">=4). The errors are padded up to "max_cb" chars so 
   ! that for a given fixed "width", all decimal points would be aligned.
   ! If "d2_err" is TRUE, two digits are used for the error digits.
      REAL, IN :: self
      REAL, IN :: error
      INT, IN :: width
      INT, IN :: max_dp
      INT, optional, IN :: max_cb
      BIN, optional, IN :: d2_err
      STR(len=width), OUT :: string
      string = to_str_(self,error,width,max_dp,max_cb,d2_err)
   end subroutine

   pure function to_str_no_zeros(self) result (string)
   ! Change self to a "string" but remove trailing zeros
      REAL, IN :: self
      STR(len=STR_SIZE) :: string
      INT :: i

      ! Write the real as a left-justified string
      string = " "
      write(string,fmt=*) self
      string = adjustl(string)

      ! Get rid of trailing zero's
      do i = len_trim(string),1,-1
         select case (string(i:i))
         case ("0"); string(i:i) = " "
         case ("."); string(i:i) = " "; exit
         case default; exit
         end select
      end do
   end function


!  ================
!  Equality testing
!  ================

   pure function equals(self,x,eps) result (res)
   ! Return TRUE is self is near enough to "x". If present, "eps"
   ! defines how close before the number is considered the same.
      REAL, IN :: self
      REAL, IN :: x
      REAL, optional, IN :: eps
      BIN :: res
      REAL :: tol
      tol = REAL_EPSILON
      if (present(eps)) tol = abs(eps)
      res = abs(self-x)<tol
   end function

   pure function same_as(self,x,eps) result (res)
   ! Return TRUE is self is near enough to "x". If present, "eps"
   ! defines how close before the number is considered the same.
      REAL, IN :: self
      REAL, IN :: x
      REAL, optional, IN :: eps
      BIN :: res
      res = equals_(self,x,eps)
   end function

   pure function is_zero(self,eps) result (res)
   ! Return TRUE if self is near enough zero. If present, "eps" defines how
   ! close to zero before the number is considered zero.
      REAL, IN :: self
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
      REAL, IN :: self
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
      REAL, IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      REAL :: tol
      tol = REAL_EPSILON
      if (present(eps)) tol = abs(eps)
      res = abs(self+ONE)<tol
   end function

   pure function is_int(self,eps) result (res)
   ! Return TRUE if self is near enough to an integer. If present, "eps" defines
   ! how close to zero the non-integer part is before the number is considered
   ! an integer.
      REAL, IN :: self
      REAL, optional, IN :: eps
      BIN :: res
      REAL :: val
      val = self-int(self)
      res = is_zero_(val,eps)
   end function

   pure function is_in_range(self,range) result (res)
   ! Return TRUE if self is within the specified "range".
      REAL, IN :: self
      VEC(REAL,2), IN :: range
      BIN :: res
      res = range(1)<=self AND self<=range(2)
   end function

   pure function same_sign_as(self,val) result (res)
   ! Return TRUE if self is the same sign as "val"
      REAL, IN :: self
      REAL, IN :: val
      BIN :: res
      res = self*val>ZERO
   end function


!  =====================
!  Numerical integration
!  =====================

! These are for illustration; consider using modules QUADRATURE or
! ADAPTIVE_QUADRATURE

   recursive function integral(self,a,b,accuracy) result (res)
   ! Integrate the function "self" between the limits "a" and "b"
   ! using adaptive trapezoidal rule with Simpsons approximation acceleration.
   ! If present, "accuracy" is the required accuracy of the integral.
      interface
         function self(x) result (res)
            REAL, IN :: x
            REAL :: res
         end function
      end interface
      REAL, IN :: a,b
      REAL, optional, IN :: accuracy
      REAL :: res
      res = integrate_adaptive_trapezoid_(self,a,b,accuracy)
   end function

   recursive function integrate_adaptive_trapezoid(self,a,b,accuracy) result (res)
   ! Integrate the function "self" between the limits "a" and "b"
   ! using adaptive trapezoidal rule with Simpsons approximation acceleration.
   ! If present, "accuracy" is the required accuracy of the integral.
      interface
         function self(x) result (res)
            REAL, IN :: x
            REAL :: res
         end function
      end interface
      REAL, IN :: a,b
      REAL, optional, IN :: accuracy
      REAL :: res
      REAL :: tol,h,m,fa,fb,fm,one_trap,two_trap,left,right
      tol = TOL(6)
      if (present(accuracy)) tol = accuracy
      h = b-a
      m = (a+b)/TWO
      fa = self(a)
      fb = self(b)
      fm = self(m)
      one_trap = h*(fa+fb)/TWO
      two_trap = h*(fa+TWO*fm+fb)/FOUR
      if (abs(one_trap-two_trap)<THREE*tol) then
         res = (FOUR*two_trap-one_trap)/THREE
      else
         left = integrate_adaptive_trapezoid_(self,a,m,tol/TWO)
         right = integrate_adaptive_trapezoid_(self,m,b,tol/TWO)
         res = left+right
      end if
   end function

   recursive function integrate_adaptive_simpson(self,a,b,accuracy) result (res)
   ! Integrate the function "self" between the limits "a" and "b"
   ! using adaptive Simpson rule with acceleration.
   ! If present, "accuracy" is the required accuracy of the integral.
      interface
         function self(x) result (res)
            REAL, IN :: x
            REAL :: res
         end function
      end interface
      REAL, IN :: a,b
      REAL, optional, IN :: accuracy
      REAL :: res
      REAL :: tol,h,m1,m2,m3,fa,fb,f1,f2,f3,s1,s2,left,right
      tol = TOL(6)
      if (present(accuracy)) tol = accuracy
      h = (b-a)/TWO
      m1 = (THREE*a+b)/FOUR
      m2 = (a+b)/TWO
      m3 = (a+THREE*b)/FOUR
      fa = self(a)
      fb = self(b)
      f1 = self(m1)
      f2 = self(m2)
      f3 = self(m3)
      s1 = h*(fa+FOUR*f2+fb)/THREE
      s2 = h*(fa+FOUR*f1+TWO*f2+FOUR*f3+fb)/SIX
      if (abs(s1-s2)<15.0d0*tol) then
         res = (16.0d0*s2-s1)/15.0d0
      else
         left = integrate_adaptive_simpson_(self,a,m2,tol/TWO)
         right = integrate_adaptive_simpson_(self,m2,b,tol/TWO)
         res = left+right
      end if
   end function


!  =================
!  Special functions
!  =================

   pure function arccos(self) result (res)
   ! Return the arccosine for self. Corrects bug for numbers close to 1.
      REAL, IN :: self
      REAL :: res
      if (abs(self-ONE)<TOL(5)) then; res = ZERO
      else if (abs(self+ONE)<TOL(5)) then; res = PI
      else; res = acos(self)
      end if
   end function

   pure function arcsin(self) result (res)
   ! Return the arcsine for self. Corrects bug for numbers close to 1.
      REAL, IN :: self
      REAL :: res
      if (abs(abs(self)-ONE)<TOL(5)) then
         if (self<0) then; res = -PI/TWO
         else; res = +PI/TWO
         end if
      else
         res = asin(self)
      end if
   end function

   pure function arcsinh(self) result (res)
   ! Return the arcsinh of self. Corrects bug for numbers close to 1.
   ! Note that self can be any real number.
      REAL, IN :: self
      REAL :: res
      res = log(self+sqrt(ONE+self*self))
   end function

   function my_sin(self) result (res)
   ! Return the sin for self. Used to test integration routines.
      REAL, IN :: self
      REAL :: res
      res = sin(self)
   end function

   ELEMENTAL function normal_probability(self,sigma2) result (res)
   ! Return Pr(-self<X<self) where X normally distributed with zero
   ! mean and variance "sigma2". "self" is made positive.
      REAL, IN :: self
      REAL, IN :: sigma2
      REAL :: res
      REAL :: z

      ! Integrate
      z = abs(self)/(sqrt(TWO*sigma2))
      res = error_function_(z)
   end function

   ELEMENTAL function z_from_cumulative_p(self) result (res)
   ! Produces the normal deviate Z corresponding to "self", a given
   ! lower tail area of P; Z is accurate to about 1 part in 10^16.
   ! Adapted from the Royal Statistical Society f77 routine "PPND16".
   ! Algorithm AS241  Appl. Stat. (1988) vol 37, no 3.
      REAL, IN :: self
      REAL :: res
      REAL :: q,r
      q = self-HALF
      if (abs(q)<0.425D0) then
         r = 0.180625d+0-q*q
         res = q*(((((((2.5090809287301226727d+3*r+3.3430575583588128105d+4)*r+6.7265770927008700853d+4)*r+4.5921953931549871457d+4)*r+1.3731693765509461125d+4)*r+1.9715909503065514427d+3)*r+1.3314166789178437745d+2)*r+3.3871328727963666080d+0)/(((((((5.2264952788528545610d+3*r+2.8729085735721942674d+4)*r+3.9307895800092710610d+4)*r+2.1213794301586595867d+4)*r+5.3941960214247511077d+3)*r+6.8718700749205790830d+2)*r+4.2313330701600911252d+1)*r+ONE)
      else
         if (q<ZERO) then; r = self
         else; r = ONE-self
         end if
         res = ZERO
         ENSURE(r>=ZERO,"REAL:z_from_cumulative_p ... probability is not between 0 and 1")
         r = sqrt(-log(r))
         if (r<FIVE) then
            r = r-1.6d+0
            res = (((((((7.74545014278341407640d-4*r+2.27238449892691845833d-2)*r+2.41780725177450611770d-1)*r+1.27045825245236838258d+0)*r+3.64784832476320460504d+0)*r+5.76949722146069140550d+0)*r+4.63033784615654529590d+0)*r+1.42343711074968357734d+0)/(((((((1.05075007164441684324d-9*r+5.47593808499534494600d-4)*r+1.51986665636164571966d-2)*r+1.48103976427480074590d-1)*r+6.89767334985100004550d-1)*r+1.67638483018380384940d+0)*r+2.05319162663775882187d+0)*r+ONE)
         else
            r = r-FIVE
            res = (((((((2.01033439929228813265d-7*r+2.71155556874348757815d-5)*r+1.24266094738807843860d-3)*r+2.65321895265761230930d-2)*r+2.96560571828504891230d-1)*r+1.78482653991729133580d+0)*r+5.46378491116411436990d+0)*r+6.65790464350110377720d+0)/(((((((2.04426310338993978564d-15*r+1.42151175831644588870d-7)*r+1.84631831751005468180d-5)*r+7.86869131145613259100d-4)*r+1.48753612908506148525d-2)*r+1.36929880922735805310d-1)*r+5.99832206555887937690d-1)*r+ONE)
         end if
         if (q<ZERO) res = -res
      end if
   end function

   PURE function cumulative_poisson(x,k) result (res)
   ! The cumulative poisson probability distribution, P_x(<k).
   ! The probability that the number of poisson events occuring
   ! will be between 0 and k-1 inclusive.  From numerical recipes.
      REAL, IN :: x
      INT, IN :: k
      REAL :: res
      res = ONE-incomplete_gamma(ONE*k,x)
   end function

   PURE function chi2(x2,nu) result (res)
   ! The chi2 probability distribution, P(chi^2,nu).
   ! The probability that the number of poisson events occuring
   ! will be between 0 and k-1 inclusive.  From numerical recipes.
      REAL, IN :: x2
      INT, IN :: nu
      REAL :: res
      res = incomplete_gamma(HALF*nu,HALF*x2)
   end function

   ELEMENTAL function error_function(self) result (res)
   ! The error function.
   ! From numerical recipes, if NO_ERF is set
      REAL, IN :: self
      REAL :: res

#ifdef NO_ERF
      res = incomplete_gamma_series(HALF,self*self)
      if (self<ONE) res = -res
#else
      res = erf(self)
#endif
   end function

   PURE function complementary_error_function(self) result (res)
   ! The complementary error function.
      REAL, IN :: self
      REAL :: res

#ifdef NO_ERF
      res = ONE-erf(self)
#else
      res = erfc(self)
#endif
   end function

   ELEMENTAL function incomplete_gamma(a,x) result (res)
   ! The incomplete gamma function P(a,x).
   ! From numerical recipes.
      REAL, IN :: a,x
      REAL :: res
   ENSURE(a>0,"REAL:incomplete_gamma ... a must be positive")
   ENSURE(x>=0,"REAL:incomplete_gamma ... x must be non-negative")
      if (x<a+ONE) then
         res = incomplete_gamma_series(a,x)
      else
         res = ONE-incomplete_gamma_cfrac(a,x)
      end if
   end function

   ELEMENTAL function incomplete_gamma_series(a,x) result (res)
   ! The incomplete gamma function P(a,x), series form.
   ! From numerical recipes.
      REAL, IN :: a,x
      REAL :: res
      INT :: n
      REAL :: eps,ap,del,ser,gln
   ENSURE(a>0,"REAL:incomplete_gamma_series ... a must be positive")
   ENSURE(x>=0,"REAL:incomplete_gamma_series ... x must be non-negative")
      eps = REAL_EPSILON
      if (x==ZERO) then
         res = ZERO
         return
      end if
      ap = a
      ser = ONE/a
      del = ser
      n = 0
      do
         n = n+1
         ap = ap+ONE
         del = del*x/ap
         ser = ser+del
         if (abs(del)<abs(ser)*eps) exit
      end do
      gln = ln_gamma_(a)
      res = ser*exp(-x+a*log(x)-gln)
      ENSURE(n<=REAL_MAX_ITERATIONS,"REAL:incomplete_gamma_series ... too many iterations")
   end function

   PURE function incomplete_gamma_cfrac(a,x) result (res)
   ! The *complementary* incomplete gamma function
   ! Q(a,x) = 1 - P(a,x), continued fraction form.
   ! From numerical recipes.
      REAL, IN :: a,x
      REAL :: res
      INT :: i
      REAL :: eps,fpmin
      REAL :: an,b,c,d,del,h,gln
   ENSURE(a>0,"REAL:incomplete_gamma_cfrac ... a must be positive")
   ENSURE(x>=0,"REAL:incomplete_gamma_cfrac ... x must be non-negative")
      eps = REAL_EPSILON
      fpmin = tiny(ONE)
      b = x+ONE-a
      c = ONE/fpmin
      d = ONE/b
      h = d
      i = 0
      do
         i = i+1
         an = -i*(i-a)
         b = b+TWO
         d = an*d+b
         c = b+an/c
         if (abs(d)<fpmin) d = fpmin
         if (abs(c)<fpmin) c = fpmin
         d = ONE/d
         del = d*c
         h = h*del
         if (abs(del-ONE)<eps) exit
         ENSURE(i<=REAL_MAX_ITERATIONS,"REAL:incomplete_gamma_cfrac ... too many iterations")
      end do
      gln = ln_gamma_(a)
      res = exp(-x+a*log(x)-gln)*h
   end function

   PURE function ln_gamma(self) result (res)
   ! The log of the gamma function. From Numerical Recipes, p. 207.
   ! This formula is accurate to 2 x 10^-10. Furthermore, the same
   ! formula can be used to calculate the complex gamma function
   ! for Re(z)>0, and the same error bound applies, everywhere in the
   ! positive complex half plane.
      REAL, IN :: self
      REAL :: res
      REAL :: ser,tmp,x,y
      INT :: j
   ENSURE(self>0,"REAL:ln_gamma ... argument must be positive")
      x = self
      y = x
      tmp = x+5.5d0
      tmp = (x+HALF)*log(tmp)-tmp
      ser = ln_gamma_coefficients(0)
      do j = 1,6
         y = y+ONE
         ser = ser+ln_gamma_coefficients(j)/y
      end do
      res = tmp+log(SQRT_2PI*ser/x)
   end function

   PURE function ln_gamma_v2(self) result (res)
   ! Another version of the natural logarithm of GAMMA(X).  It was taken from
   ! some code from the net called "quadrule.f90" It doesn't look as efficient
   ! as the Numerical Recipes code, but it seems to have slightly more accuracy.
   ! The original code had these comments:
   !
   ! The method uses Stirling's approximation, and is accurate to about
   ! 12 decimal places.
   !
   ! Reference:
   !    Arthur Stroud and Don Secrest,
   !    Gaussian Quadrature Formulas,
   !    Prentice Hall, 1966.
      REAL, IN :: self
      REAL :: res
      INT :: i,k,m
      REAL :: p,x,x2,y,z,z2
   ENSURE(self>ZERO,"REAL:ln_gamma_v2 ... argument must be non-negative")
      x = self
      if (x<HALF) then; m = 1; x2 = ONE-x
      else; m = 0; x2 = x
      end if
      k = -1
      do
         k = k+1
         if (SIX<x2+k) exit
      end do
      z = x2+k
      z2 = z*z
      y = (z-HALF)*log(z)-z+0.9189385332047D+00+(((((-4146.0D+00/z2+1820.0D+00)/z2-1287.0D+00)/z2+1716.0D+00)/z2-6006.0D+00)/z2+180180.0D+00)/z/2162160.0D+00
      if (k>0) then
         do i = 1,k
            y = y-log(x2+k-i)
         end do
      end if
      ENSURE(m/=0,"REAL:ln_gamma_v2 ... m is zero - fatal error")
      p = PI/sin(PI*(ONE-x2))
      ENSURE(p>ZERO,"REAL:ln_gamma_v2 ... p is not +ve - fatal error")
      y = log(p)-y
      res = y
   end function


!   gamma result (res) :: recursive
!   ! Computes the gamma function using Hastings's approximation. This code was
!   ! taken from the net from a routine called "quadrule.f90".
!   ! Reference:
!   !    Arthur Stroud and Don Secrest,
!   !    Gaussian Quadrature Formulas,
!   !    Prentice Hall, 1966.
!      res :: REAL
!   ENSURE(self>ZERO,"argument must be non-negative")
!   ENSURE(self<70.0d0,"argument must be less than 70")
!      gam :: REAL
!      x,y,z,za :: REAL

!            - 0.577191652D+00 ) * y + ONE
!      self = x
!      if (x==ONE) then
!        res = ONE
!        return
!      end if
!      if (x<=ONE) then
!        res = gam(x)/x
!        return
!      end if
!      z = x
!      za = ONE
!      do
!        z = z - ONE
!        if (z<ONE) then
!          res = za*gam(z)
!          exit
!        else if (z==ONE) then
!          res = za
!          exit
!        end if
!        za = za * z
!      end
!   end

!   s_from_p(p,mean)
!   ! Calculate the Poisson upper limit of cumulation "s" from the cumulation "p".
!   ! self is p, function returns s.
!       p,mean :: REAL, IN
!
!C      ..
!C      .. Scalar Arguments ..
!       REAL bound,p,s,xlam
!       INTEGER status,which
!C      ..
!C      .. Local Scalars ..
!       REAL fx,pp,ss,xxlam
!       LOGICAL qhi,qleft
!C      ..
!C      .. External Functions ..
!       REAL cumpoi
!       EXTERNAL cumpoi
!C      ..
!C      .. External Subroutines ..
!       EXTERNAL invr,stinvr
!
!       ENSURE(p>ZERO, "p not greater than zero")
!       ENSURE(p<ONE, "p not less than one")
!       ENSURE(xlam>ZERO,"xlam not > 0")
!
!           res = FIVE
!           CALL stinvr(ZERO,TEN==30,HALF,HALF,FIVE,TOL(4),TOL(4))
!           status = 0
!           CALL invr(status,res,fx,qleft,qhi)
!           do
!             if (status/=1) exit
!             fx = cumpoi(res,xlam) - p
!             CALL invr(status,res,fx,qleft,qhi)
!           end
!
!           if (status==-1) then
!             if (NOT (qleft)) then
!               status = 2
!               bound = inf
!             else
!               status = 1
!               bound = 0.0
!             end
!           end
!   end

   PURE function hermite_polynomial(self,n,normalize) result (res)
   ! Return "res", the value of the "n"th hermite polynomial H_n(self).
   ! If present and TRUE, "normalize" gives values normalized for use
   ! in generating harmonic vibrational wavefunctions. See below.
      REAL, IN :: self
      INT, IN :: n
      BIN, optional, IN :: normalize
      REAL :: res
      VEC(REAL,:), PTR :: coeff
      REAL :: x
      INT :: i
      allocate(coeff(n+1))
      coeff = hermite_polynomial_coeffs_(n,normalize)
      res = ZERO
      x = ONE
      do i = 0,n
         res = res+coeff(i+1)*x
         x = x*self
      end do
      deallocate(coeff)
   end function

   PURE function hermite_polynomial_prob(self,n) result (res)
   ! Return "res", the value of the "n"th probabilists' hermite
   ! polynomial He_n(self).
      REAL, IN :: self
      INT, IN :: n
      REAL :: res
      VEC(REAL,:), PTR :: coeff
      REAL :: x
      INT :: i
      allocate(coeff(n+1))
      coeff = hermite_polynomial_prob_coeffs_(n)
      res = ZERO
      x = ONE
      do i = 0,n
         res = res+coeff(i+1)*x
         x = x*self
      end do
      deallocate(coeff)
   end function

   PURE function harmonic_vibrational_fn(self,n) result (res)
   ! Return "res", the value of the "n"th harmonic vibrational wavefunction
   ! as a function of the dimensionless normal coordinate "self"
      REAL, IN :: self
      INT, IN :: n
      REAL :: res
      REAL :: q
      q = self
   !  fac = sqrt( ONE/ (sqrt(PI) * 2==n * n.factorial) )
      res = hermite_polynomial_(q,n,normalize=TRUE)*exp(-HALF*q*q)
   end function


!   boys_function_F(m) result (res)
!   ! The Boys function of order "m",
!   ! res = F_m(T) = \int_0^1 t^2m exp(-Tt^2) dt
!     self :: IN
!     res :: REAL
!   ENSURE(self>0,"argument must be positive")
!     ser,tmp,x,y :: REAL
!     j :: INT
!     x = self
!     y = x
!     tmp = x + 5.5d0
!     tmp = (x+HALF)*log(tmp) - tmp
!     ser = ln_gamma_coefficients(0)
!     do j = 1,6
!        y = y + ONE
!        ser = ser + ln_gamma_coefficients(j)/y
!     end
!     res = tmp + log(SQRT_2PI*ser/x)
!   end

!   gill_T1(m,eps) result (res)
!   ! The function whose root is the minimum value of T for which the use of a
!   ! multipole expansion is accurate to "eps".
!     self :: IN
!     m :: INT
!     eps,res :: REAL
!   ENSURE(self>0,"argument must be positive")
!   ENSURE(eps>0,"eps must be positive")
!   ENSURE(m>0,"m must be positive")
!      mmh,mph :: REAL
!      mmh = m - HALF
!      mph = m + HALF
!      res = self - mmh*log(self) - mph.ln_gamma - log(eps)
!   end

!   gill_T1_stub result (res)
!   ! The function whose root is the minimum value of T for which the use of a
!   ! multipole expansion for the Boys function of order "m" is accurate to
!   ! "eps".  This stub is used for root finding and takes its "m" and "eps"
!   ! values from the corresponding module variables.
!      res :: REAL
!   ENSURE(self>0,"argument must be positive")
!      m :: INT
!      eps,mmh,mph :: REAL
!      m = gill_m
!      eps = gill_eps
!      mmh = m - HALF
!      mph = m + HALF
!      res = self - mmh*log(self) - mph.ln_gamma - log(eps)
!   end

!   set_gill_T1(m,eps) :: selfless
!   ! Set Gill's T1(m) numbers which define the minimum value of T for which the
!   ! use of a multipole expansion is accurate to "eps".
!     self :: IN
!     m :: INT
!     eps :: REAL
!   ENSURE(eps>0,"eps must be positive")
!   ENSURE(m>0,"m must be positive")
!      x1,x2 :: REAL
!      i :: INT
!      if (allocated(gill_T1)) gill_T1.destroy
!      gill_T1.create(m)
!      do i = 1,m
!         gill_m = m
!         gill_eps = eps
!         x1 = ZERO
!         x2 = -TWO*log(eps)
!         REAL::bracket_root(REAL::gill_T1_stub,x1,x2)
!         REAL::find_root(REAL::gill_T1_stub,x1,x2,root,tol=)
!      end
!   end

!  ========================
!  Fermi-Dirac distribution
!  ========================

   subroutine set_T_Fermi(T)
   ! Set the Fermi temperature T_Fermi in *Hartree*
   ! for the Fermi-Dirac distribution
      REAL, IN :: T
   ENSURE(T>ZERO,"REAL:set_T_Fermi ... T must be greater than zero")
      T_Fermi = T
   end subroutine

   subroutine set_E_level_0(e)
   ! Set the energy levels for the Fermi-Dirac distribution
      VEC(REAL,:), IN :: e
      E_level = e
   end subroutine

   subroutine set_E_level_1(ea,eb)
   ! Set the alpha "ea" and beta "eb" energy levels for the
   ! Fermi-Dirac distribution
      VEC(REAL,:), IN :: ea,eb
      E_level = [ea,eb]
   end subroutine

   subroutine set_E_Fermi(e)
   ! Set the Fermi energy E_Fermi.
      REAL, IN :: e
      E_Fermi = e
   end subroutine

   subroutine set_n_electrons(N)
   ! Set the fermi energvy E_Fermi.
      INT, IN :: N
      n_electrons = N
   end subroutine

   PURE function Fermi_Dirac_fn(i) result (res)
   ! Return "res", the value of the Fermi-Dirac distribution
   ! function. The temperature and fermi_energy are set in this
   ! module.
      INT, IN :: i
      REAL :: res
      REAL :: kT,beta
   ENSURE(T_Fermi>ZERO,"REAL:Fermi_Dirac_fn ... T must be greater than zero")
   ENSURE(allocated(E_level),"REAL:Fermi_Dirac_fn ... no E_level array")
   ENSURE(i>0,"REAL:Fermi_Dirac_fn ... i must be +ve")
   ENSURE(i<=size(E_level),"REAL:Fermi_Dirac_fn ... i is too large")

      ! kT in J, then Hartree, then beta = 1/kT in Hartree^-1
      kT = BOLTZMANN_SI*(T_Fermi*KELVIN_PER_HARTREE)
      kT = kT*HARTREE_PER_JOULE
      beta = ONE/kT
      res = ONE/(ONE+exp(beta*(E_level(i)-E_Fermi)))
   end function

   function Fermi_Dirac_excess_fn(e) result (res)
   ! Evalaue the excess no. of electrons for the Fermi-Dirac
   ! distribution with Fermiu energy "e", where
   !    excess = n_electrons - sum_i Fermi_Dirac_fn(E_level(i)) 
   ! This is minimized to find the fermi energy
      REAL, IN :: e
      REAL :: res
      INT :: i
      REAL :: val
   ENSURE(T_Fermi>ZERO,"REAL:Fermi_Dirac_excess_fn ... T must be greater than zero")
   ENSURE(allocated(E_level),"REAL:Fermi_Dirac_excess_fn ... no E_level array")

      ! Set E_Fermi
      call set_E_Fermi(e)

      ! Work out the excess
      val = ZERO
      do i = 1,size(E_level)
         val = val+Fermi_Dirac_fn(i)
      end do
      res = n_electrons-val
   end function


!  ========================
!  Unit conversion routines
!  ========================

   PURE subroutine convert_to(self,units)
   ! Convert the number "self" in atomic units or generic units to a
   ! new number in "units".
      REAL, INOUT :: self
      STR(len=*), IN :: units
   ENSURE(is_known_unit_(units),"REAL:convert_to ... unknown units, "//units)
      self = self*conversion_factor_(units)
   end subroutine

   PURE function to_units(self,units) result (res)
   ! Convert the number "self" in atomic units or generic units to a
   ! new number in "units".
      REAL, IN :: self
      STR(len=*), IN :: units
      REAL :: res
   ENSURE(is_known_unit_(units),"REAL:to_units ... unknown units, "//units)
      res = self*conversion_factor_(units)
   end function

   PURE subroutine convert_from(self,units)
   ! Convert the number "self" from "units" system to a new number
   ! in atomic units or generic units.  Returns "err" whether it was successful.
      REAL, INOUT :: self
      STR(len=*), IN :: units
   ENSURE(is_known_unit_(units),"REAL:convert_from ... unknown units, "//units)
      self = self/conversion_factor_(units)
   end subroutine

   PURE function from_units(self,units) result (res)
   ! Convert the number "self" from "units" system to a new number
   ! in atomic units or generic units.  Returns "err" whether it was successful.
      REAL, IN :: self
      STR(len=*), IN :: units
      REAL :: res
   ENSURE(is_known_unit_(units),"REAL:from_units ... unknown units, "//units)
      res = self/conversion_factor_(units)
   end function


!  =====================
!  Root finding routines
!  =====================

   subroutine bracket_root(self,x1,x2,factor,max_it)
   ! Given a function self(x) and initial points "x1" and "x2", bracket a root
   ! of self(x) by expansion. If "factor" is present it is used as the (linear)
   ! interval expansion factor. If "max_it" is present then it is the number of
   ! times the interval is expanded.
      interface
         function self(x) result (res)
            REAL, IN :: x
            REAL :: res
         end function
      end interface
      REAL, INOUT :: x1,x2
      REAL, optional, IN :: factor
      INT, optional, IN :: max_it
      INT :: j,maxit
      REAL :: f1,f2,fac
   ENSURE(x1/=x2,"REAL:bracket_root ... non-zero range (x1,x2) required")
      fac = 1.6d0
      if (present(factor)) fac = factor
      maxit = 50
      if (present(max_it)) maxit = max_it
      f1 = self(x1)
      f2 = self(x2)
      do j = 1,maxit
         if (f1*f2<ZERO) return
         if (abs(f1)<abs(f2)) then
            x1 = x1+fac*(x1-x2)
            f1 = self(x1)
         else
            x2 = x2+fac*(x2-x1)
            f2 = self(x2)
         end if
      end do
      DIE("REAL:bracket_root ... Exceeded maximum number of iterations")
   end subroutine

   subroutine find_root_brent(self,x1,x2,root,tol,val,max_it)
   ! Given a function self(x) and initial points "x1" and "x2" which
   ! bracket a root of self, find the "root" to a precision "tol". If
   ! "val" present, then the root which makes self have this value is
   ! returned (an isovalue).  If "max_it" is present it is set to the
   ! maximum number of iterations.
      interface
         function self(x) result (res)
            REAL, IN :: x
            REAL :: res
         end function
      end interface
      REAL, IN :: x1,x2
      REAL, OUT :: root
      REAL, IN :: tol
      REAL, optional, IN :: val
      INT, optional, IN :: max_it
      INT :: iter
      REAL :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
      INT :: maxit = 100
      REAL :: iso = ZERO
      REAL :: eps = TOL(8)
      if (present(max_it)) maxit = max_it
      if (present(val)) iso = val
      a = x1
      b = x2
      fa = self(a)-iso
      fb = self(b)-iso
      ENSURE((fa>0 AND fb>0) OR (fa<0 AND fb<0),"REAL:find_root_brent ... root is not bracketed")
      c = b
      fc = fb
      d = ZERO
      e = ZERO
      do iter = 1,maxit

         ! Rename a,b,c and adjust bounding interval d
         if ((fb>0 AND fc>0) OR (fb<0 AND fc<0)) then
            c = a
            fc = fa
            d = b-a
            e = d
         end if
         if (abs(fc)<abs(fb)) then
            a = b; b = c; c = a
            fa = fb; fb = fc; fc = fa
         end if

         ! Test convergence
         tol1 = TWO*eps*abs(b)+HALF*tol
         xm = HALF*(c-b)
         if (abs(xm)<=tol1 OR fb==ZERO) then
            root = b
            return  ! <<<<
         end if

         ! Attempt inverse quadratic interpolation
         if (abs(e)>=tol1 AND abs(fa)>abs(fb)) then
            s = fb/fa
            if (a==c) then
               p = TWO*xm*s
               q = ONE-s
            else
               q = fa/fc
               r = fb/fc
               p = s*(TWO*xm*q*(q-r)-(b-a)*(r-ONE))
               q = (q-ONE)*(r-ONE)*(s-ONE)
            end if
            if (p>ZERO) q = -q
            p = abs(p)
            if (TWO*p<min(THREE*xm*q-abs(tol1*q),abs(e*q))) then
               e = d  ! Accept interpolation
               d = p/q
            else
               d = xm  ! Interpolation failed, use bisection
               e = d
            end if
         else
            d = xm  ! Bounds decreasing too slowly, use bisection
            e = d
         end if
         a = b; fa = fb
         if (abs(d)>tol1) then; b = b+d
         else; b = b+sign(tol1,xm)
         end if
         fb = self(b)-iso
      end do
      DIE("REAL:find_root_brent ... maximum iterations exceeded")
   end subroutine


!  =====================
!  Minimization routines
!  =====================

   subroutine bracket_minimum(self,a,b,c,fa,fb,fc)
   ! Given a function self(x) and initial points "a" and "b" search in
   ! the downhill direction and return points "a", "b" and "c" that bracket
   ! a minimum of the function, and return the value of the function
   ! "fa", "fb", and "fc" at these points. NOTE: "c" is not used initially.
      interface
         function self(x) result (res)
            REAL, IN :: x
            REAL :: res
         end function
      end interface
      REAL :: a,b,c,fa,fb,fc
      REAL :: gold = 1.618034
      REAL :: glimit = 100
      REAL :: tiny = 1.0d-20
      REAL :: fu,q,r,u,ulim
      fa = self(a)
      fb = self(b)
      if (fb>fa) then
         call swap_with_(a,b)
         call swap_with_(fa,fb)
      end if
      c = b+gold*(b-a)
      fc = self(c)
      do
         if (fb<fc) exit  ! bracket found
         r = (b-a)*(fb-fa)  ! get u by parabolic extrapolation
         q = (b-c)*(fb-fa)
         u = b-((b-c)*q-(b-a)*r)/(TWO*sign(max(abs(q-r),tiny),q-r))
         ulim = b+glimit*(c-b)
         if ((b-u)*(u-c)>ZERO) then  ! Parabolic u lies between b and c
            fu = self(u)
            if (fu<fc) then  ! got a minimum between b and c
               a = b; fa = fb
               b = u; fb = fu
               exit
            else if (fu>fb) then  ! got a minimum between a and u
               c = u; fc = fu
               exit
            end if
            u = c+gold*(c-b)  ! parabolic fit no use, so magnify
            fu = self(u)
         else if ((c-u)*(u-ulim)>0) then  ! Fit is between c and its allowed limit
            fu = self(u)
            if (fu<fc) then
               b = c; fb = fc
               c = u; fc = fu
               u = c+gold*(c-b)
               fu = self(u)
            end if
         else if ((u-ulim)*(ulim-c)>0) then
            u = ulim
            fu = self(u)
         else
            u = c+gold*(c-b)  ! magnify
            fu = self(u)
         end if
         a = b; fa = fb
         b = c; fb = fc
         c = u; fc = fu
      end do
      if (a>c) then
         call swap_with_(a,c)
         call swap_with_(fa,fc)
      end if
   end subroutine

   subroutine minimize_golden(self,a,b,c,xmin,f,tol)
   ! Given a function self(x) and initial points "a", "b" and "c"
   ! which bracket a minimum, return the minimum point "xmin" and the
   ! value "f" at the minimum to a precision "tol" using the golden
   ! section search method.
      interface
         function self(x) result (res)
            REAL, IN :: x
            REAL :: res
         end function
      end interface
      REAL :: a,b,c,xmin,f,tol
      REAL :: r = 0.618033399
      REAL :: s,f1,f2,x0,x1,x2,x3
      s = ONE-r
      x0 = a
      x3 = c
      if (abs(c-b)>abs(b-a)) then
         x1 = b; x2 = b+s*(c-b)
      else
         x2 = b; x1 = b-s*(b-a)
      end if
      f1 = self(x1)
      f2 = self(x2)
      do
         if (abs(x3-x0)<=tol*(abs(x1)+abs(x2))) exit
         if (f2<f1) then
            x0 = x1
            x1 = x2
            x2 = r*x1+s*x3
            f1 = f2
            f2 = self(x2)
         else
            x3 = x2
            x2 = x1
            x1 = r*x2+s*x0
            f2 = f1
            f1 = self(x1)
         end if
      end do
      if (f1<f2) then; f = f1; xmin = x1
      else; f = f2; xmin = x2
      end if
   end subroutine

   subroutine minimize_brent(self,a,b,c,xmin,f,tol)
   ! Given a function self(x) and initial points "a", "b" and "c"
   ! which bracket a minimum, return the minimum point "xmin" and the
   ! value "f" at the minimum to a precision "tol" using Brent's method.
      interface
         function self(x) result (res)
            REAL, IN :: x
            REAL :: res
         end function
      end interface
      REAL :: a,b,c,xmin,f,tol
      REAL :: cgold,zeps
      REAL :: d,e,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm
      INT :: itmax,iter
      BIN :: failed
      itmax = REAL_MAX_ITERATIONS
      cgold = 0.3819660
      zeps = TOL(10)
      if (a>c) call swap_with_(a,c)
      v = b
      b = c
      d = ZERO
      e = ZERO
      w = v
      x = v
      fx = self(x)
      fv = fx
      fw = fx
      failed = TRUE
      do iter = 1,itmax
         xm = HALF*(a+b)
         tol1 = tol*abs(x)+zeps
         tol2 = TWO*tol1
         if (abs(x-xm)<(tol2-HALF*(b-a))) then
            failed = FALSE
            exit
         end if
         if (abs(e)>tol1) then
            r = (x-w)*(fx-fv)
            q = (x-v)*(fx-fw)
            p = (x-v)*q-(x-w)*r
            q = TWO*(q-r)
            if (q>ZERO) p = -p
            q = abs(q)
            etemp = e
            e = d
            if (abs(p)>=abs(HALF*q*etemp) OR p<=q*(a-x) OR p>=q*(b-x)) then
               if (x>=xm) then; e = a-x
               else; e = b-x
               end if
               d = cgold*e
            else
               d = p/q
               u = x+d
               if ((u-a)<tol2 OR (b-u)<tol2) d = sign(tol1,xm-x)
            end if
         else
            if (x>=xm) then; e = a-x
            else; e = b-x
            end if
            d = cgold*e
         end if
         if (abs(d)>=tol1) then; u = x+d
         else; u = x+sign(tol1,d)
         end if
         fu = self(u)
         if (fu<=fx) then
            if (u>=x) then; a = x
            else; b = x
            end if
            v = w; fv = fw
            w = x; fw = fx
            x = u; fx = fu
         else
            if (u<x) then; a = u
            else; b = u
            end if
            if (fu<=fw OR w==x) then
               v = w; fv = fw
               w = u; fw = fu
            else if (fu<=fv OR v==x OR v==w) then
               v = u; fv = fu
            end if
         end if
      end do
      f = fx
      xmin = x
      DIE_IF(failed,"REAL:minimize_brent ... maximum iterations exceeded")
   end subroutine


!  ==========
!  Statistics
!  ==========

   subroutine to_random_normal(self)
   ! Set self to be a normal random number. From algorithm 488 in toms
   ! from Netlib. This procedure cannot be made pure.
   ! ALGORITHM APPEARED IN COMM. ACM, VOL. 17, NO. 12, P. 704.
      REAL, OUT :: self
      VEC(REAL,60) :: D = [0.674489750d0,0.475859630d0,0.383771164d0,0.328611323d0,0.291142827d0,0.263684322d0,0.242508452d0,0.225567444d0,0.211634166d0,0.199924267d0,0.189910758d0,0.181225181d0,0.173601400d0,0.166841909d0,0.160796729d0,0.155349717d0,0.150409384d0,0.145902577d0,0.141770033d0,0.137963174d0,0.134441762d0,0.131172150d0,0.128125965d0,0.125279090d0,0.122610883d0,0.120103560d0,0.117741707d0,0.115511892d0,0.113402349d0,0.111402720d0,0.109503852d0,0.107697617d0,0.105976772d0,0.104334841d0,0.102766012d0,0.101265052d0,0.099827234d0,0.098448282d0,0.097124309d0,0.095851778d0,0.094627461d0,0.093448407d0,0.092311909d0,0.091215482d0,0.090156838d0,0.089133867d0,0.088144619d0,0.087187293d0,0.086260215d0,0.085361834d0,0.084490706d0,0.083645487d0,0.082824924d0,0.082027847d0,0.081253162d0,0.080499844d0,0.079766932d0,0.079053527d0,0.078358781d0,0.077681899d0]
      REAL, save :: U = ZERO
      BIN, save :: first = TRUE
      REAL :: A,W,V
      INT :: I,J,N

      ! Note that on the first call, self is returned as zero, so to
      ! get around this, we iterate twice.  On all subsequent calls,
      ! iterate only once.
      N = 1
      if (first) N = 2
      do J = 1,N
         A = ZERO
         I = 0
         do
            U = U+U
            if (U<ONE) exit
            U = U-ONE
            I = I+1
            A = A-D(I)
         end do
         outer: do
            W = D(I+1)*U
            V = W*(HALF*W-A)
            do
               call random_number(U)
               if (V<=U) exit outer
               call random_number(V)
               if (U<=V) exit
            end do
            U = (V-U)/(ONE-U)
         end do outer
         U = (U-V)/(ONE-V)
         U = U+U
         if (U>=ONE) then
            U = U-ONE
            self = W-A
         else
            self = A-W
         end if
      end do
      first = FALSE
   end subroutine

end module
