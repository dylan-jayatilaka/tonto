!===================================================================
! INT: methods which apply to integers
!
! Copyright (C) Dylan Jayatilaka, 1998-2025
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
module INT_MODULE

#  include "macros"

   include "int.use"

   implicit none

   include "int.int"


contains

! ====================
! Numerical operations
! ====================

   pure subroutine to_sum_of_0(self,v)
   ! Set "self" to the sum of elements in vector "v"
      INT, OUT :: self
      VEC(INT,:), IN :: v
      INT :: i

      ! The following code is inherited from INTRINSIC
      self = 0
      do i = 1,size(v)
         self = self+v(i)
      end do
   end subroutine

   pure subroutine to_sum_of_1(self,mx)
   ! Set "self" to the sum of elements in matrix "mx"
      INT, OUT :: self
      MAT(INT,:,:), IN :: mx
      INT :: i,j

      ! The following code is inherited from INTRINSIC
      self = 0
      do j = 1,size(mx,2)
         do i = 1,size(mx,1)
            self = self+mx(i,j)
         end do
      end do
   end subroutine


! ================
! Equality testing
! ================

   pure function equals(self,i) result (same)
   ! Test to see if "self" is the same as "i"
      INT, IN :: self
      INT, IN :: i
      BIN :: same
      same = same_as_(self,i)
   end function

   pure function same_as(self,i) result (same)
   ! Test to see if "self" is the same as "i"
      INT, IN :: self
      INT, IN :: i
      BIN :: same
      same = self==i
   end function

   pure function is_zero(self) result (res)
   ! Test to see if "self" is zero
      INT, IN :: self
      BIN :: res
      res = self==0
   end function

   pure function is_one(self) result (res)
   ! Test to see if "self" is one
      INT, IN :: self
      BIN :: res
      res = self==1
   end function

   pure function is_minus_one(self) result (res)
   ! Test to see if "self" is minus one
      INT, IN :: self
      BIN :: res
      res = self==-1
   end function

   pure function is_even(self) result (res)
   ! Return TRUE if self is an even number
      INT, IN :: self
      BIN :: res
      res = mod(self,2)==0
   end function

   pure function is_odd(self) result (res)
   ! Return TRUE if self is an odd number
      INT, IN :: self
      BIN :: res
      res = mod(self,2)==1
   end function

   pure function is_in_range(self,range) result (res)
   ! Return TRUE if self is within the specified "range".
      INT, IN :: self
      VEC(INT,2), IN :: range
      BIN :: res
      res = range(1)<=self AND self<=range(2)
   end function


! =========
! Functions
! =========

   pure function delta(self,j) result (res)
   ! Kronecker delta function. Returns 1 if self==j, otherwise 0.
      INT, IN :: self
      INT, IN :: j
      INT :: res
      if (self==j) then; res = 1
      else; res = 0
      end if
   end function

   pure function raised_to(self,n) result (res)
   ! Raise "self" to the power "n"
      INT, IN :: self
      INT, IN :: n
      REAL :: res
      INT :: i
      res = ONE
      do i = 1,abs(n)
         res = res*self
      end do
      if (n<0) res = ONE/res
   end function

   pure subroutine swap_with(self,that)
   ! Swap the value of "self" and "that"
      INT, INOUT :: self
      INT, INOUT :: that
      INT :: keep
      keep = self
      self = that
      that = keep
   end subroutine

   elemental function factorial(self) result (res)
   ! Return the factorial of the integer
   ! res cannot be int, as int(4) dies as 13!.
      INT, IN :: self
      REAL :: res
      INT :: i
      select case (self)

      case (0); res = 1.0d0
      case (1); res = 1.0d0
      case (2); res = 2.0d0
      case (3); res = 6.0d0
      case (4); res = 24.0d0
      case (5); res = 120.0d0
      case (6); res = 720.0d0
      case (7); res = 5040.0d0
      case (8); res = 40320.0d0
      case (9); res = 362880.0d0

      case default
         res = 3628800.0d0
         do i = 11,self
            res = res*i
         end do
      end select
   end function

   elemental function double_factorial(self) result (res)
   ! Return the double factorial of the integer
   ! res cannot be integer, as int(4) dies at 11!!.
      INT, IN :: self
      REAL :: res
      INT :: i
      select case (self)
      case (0); res = 1.0d0
      case (1); res = 1.0d0
      case (2); res = 3.0d0
      case (3); res = 15.0d0
      case (4); res = 105.0d0
      case (5); res = 945.0d0
      case (6); res = 10395.0d0
      case (7); res = 135135.0d0
      case (8); res = 2027025.0d0
      case (9); res = 34459425.0d0
      case default
         res = 654729075.0d0
         do i = 11,self
            res = res*(2*i-1)
         end do
      end select
   end function

   elemental function gamma_plus_half(self) result (res)
   ! Return the Gamma(self+1/2)
      INT, IN :: self
      REAL :: res
      INT :: i
      select case (self)
      case (0); res = 1.0d0
      case (1); res = 1.0d0/2.0d0
      case (2); res = 3.0d0/4.0d0
      case (3); res = 15.0d0/8.0d0
      case (4); res = 105.0d0/16.0d0
      case (5); res = 945.0d0/32.0d0
      case (6); res = 10395.0d0/64.0d0
      case (7); res = 135135.0d0/128.0d0
      case (8); res = 2027025.0d0/256.0d0
      case (9); res = 34459425.0d0/512.0d0
      case default
         res = 654729075.0d0/1024.0d0
         do i = 11,self
            res = res*(2*i-1)/TWO
         end do
      end select
      res = res*sqrt(PI)
   end function

   pure function permutation_0(self,n) result (res)
   ! Return self!/n!
      INT, IN :: self
      INT, IN :: n
      REAL :: res
! ENSURE(self>=0,"must have non-negative argument")
! ENSURE(n   >=0,"must have non-negative n")
      INT :: i
      res = ONE
      if (self>=n) then
         do i = n+1,self
            res = res*i
         end do
      else
         do i = self+1,n
            res = res*i
         end do
         res = ONE/res
      end if
   end function

   pure function permutation_1(self,n,m) result (res)
   ! Return self!/(n!m!)
      INT, IN :: self
      INT, IN :: n,m
      REAL :: res
! ENSURE(self>=0,"must have non-negative argument")
! ENSURE( n  >=0,"must have non-negative n")
! ENSURE( m  >=0,"must have non-negative m")
      INT :: i
      res = permutation_(self,n)
      do i = 2,m
         res = res/i
      end do
   end function

   pure function choose(self,n) result (res)
   ! Return self!/n!(self-n)! , self>=n
      INT, IN :: self
      INT, IN :: n
      REAL :: res
! ENSURE(self>=0,"must have non-negative argument")
! ENSURE(self>=n,"must have self>=n")
      res = permutation_(self,n,self-n)
   end function

   PURE function sqrt_permutation_0(self,n) result (res)
   ! Return sqrt(self!)/n! , self>n
      INT, IN :: self
      INT, IN :: n
      REAL :: res
      REAL :: top,bot
      INT :: i
   ENSURE(self>=0,"INT:sqrt_permutation_0 ... must have non-negative argument")
   ENSURE(self>=n,"INT:sqrt_permutation_0 ... must have self>=n")
      bot = ONE
      top = ONE
      do i = 1,n
         bot = bot*i
      end do
      do i = n+1,self
         top = top*i
      end do
      res = sqrt(top/bot)
   end function

   PURE function sqrt_permutation_1(self,n,m) result (res)
   ! Return sqrt(self!)/(n!m!) , self>n, self>m
      INT, IN :: self
      INT, IN :: n,m
      REAL :: res
      INT :: i
   ENSURE(self>=0,"INT:sqrt_permutation_1 ... must have non-negative argument")
   ENSURE(self>=n,"INT:sqrt_permutation_1 ... must have self>=n")
   ENSURE(self>=m,"INT:sqrt_permutation_1 ... must have self>=m")
      res = sqrt_permutation_(self,n)
      do i = 2,m
         res = res/i
      end do
   end function

   pure function triangle(self) result (res)
   ! The triangle number corresponding to self
      INT, IN :: self
      INT :: res
      res = self*(self+1)/2
   end function

   pure function triangle_number(self) result (res)
   ! The triangle number corresponding to seld
      INT, IN :: self
      INT :: res
      res = self*(self+1)/2
   end function

   PURE function inverse_triangle_number(self) result (res)
   ! Return the inverse triangle number of self.
      INT, IN :: self
      INT :: res
   ENSURE(self>=0,"INT:inverse_triangle_number ... self must be non-positive")
      res = (1+int(sqrt(EIGHT*self-SEVEN)))/2
   end function

   PURE function radial_laguerre_function(self,l,zeta) result (res)
   ! Returns an array with the coefficients of the radial Laguerre function as
   ! defined in "Molecular Electronic Structure Theory" by Helgaker et al,
   ! equation (6.5.17) *except* that the factor r^l exp(-zeta*R) is set to one.
   ! Self=n is the principal quantum number.
      INT, IN :: self
      INT, IN :: l
      REAL, IN :: zeta
      VEC(REAL,:), PTR :: res
      INT :: n,n1,l1
      REAL :: fac
   ENSURE(self>=0,"INT:radial_laguerre_function ... non positive self")
      n = self
      n1 = n-l-1
      l1 = 2*l+2
      allocate(res(n1+1))
      res = assoc_laguerre_coeffs_(n1,l1)
      fac = (TWO*zeta)**((THREE/TWO)+l)*sqrt(choose_(n1,n+l+1))
      res = fac*res
   end function

   pure function sequence_up_to(self,last,step) result (res)
   ! Return a sequence from "self" up to "last" with "step" spacing
   ! if present, or spacing 1 if not.
      INT, IN :: self
      INT, IN :: last
      INT, optional, IN :: step
      VEC(INT,:), PTR :: res
      INT :: i,n,s,val

      ! Step
      s = 1
      if (present(step)) s = step

      ! Sequence length
      n = (last-self)/s
      if (n<0) then; n = 0
      else; n = n+1
      end if

      ! The sequence
      allocate(res(n))
      i = 0
      val = self

      ! Make it
      do
         if (val>last) exit
         i = i+1
         res(i) = val
         val = val+s
      end do
   end function


! =======
! Inquiry
! =======

   pure function check_if_one_of(self,allowed) result (ok)
   ! Returns TRUE if "self" matches one of the integers in "allowed".
      INT, IN :: self
      VEC(INT,:), IN :: allowed
      BIN :: ok
      ok = any(self==allowed)
   end function

   PURE function is_a_prime(self) result (res)
   ! returns TRUE is self is a primes
      INT, IN :: self
      BIN :: res
      INT :: i,j
   ENSURE(self>0,"INT:is_a_prime ... primes should not be negative")
      res = TRUE
      if (self==2) return
      do i = 2,int(sqrt(real(self)))+1
         j = modulo(self,i)
         if (j/=0) cycle
         res = FALSE; exit
      end do
   end function

   PURE function largest_int(self) result (res)
   ! Returns largest possible integer
      INT, IN :: self
      INT :: res
      INT :: i,j,k
   ENSURE(self>0,"INT:largest_int ... should not be negative")
      j = 0
      i = self
      do
         i = i*2
         if (i<j) exit
         j = i
      end do
      k = j
      do
         k = (k/2)
         i = j+k
         if (i<j) i = j
         j = i
         if (k==1) then
            res = i
            return
         end if
      end do
   end function


! =======================
! Polynomial coefficients
! =======================

   PURE function hermite_polynomial_coeffs(self,normalize) result (res)
   ! Returns an array with the coefficients of the Hermite Polynomials.  First
   ! elements is for x^0, second for x^1, and so on.  The length of the result
   ! is (self+1).  Method used is equation 13.9 from Arfken and Weber 4th ed.
   ! If present and TRUE, "normalize" gives coefficients normalized for use
   ! in generating harmonic vibrational wavefunctions.
      INT, IN :: self
      BIN, IN, optional :: normalize
      VEC(REAL,self+1) :: res
      BIN :: norm
      INT :: n,s,i,t
      REAL :: sn
   ENSURE(self>=0,"INT:hermite_polynomial_coeffs ... order must be 0 or greater")
      norm = FALSE
      if (present(normalize)) norm = normalize
      n = self
      t = 2
      res = ZERO
      if (norm) then
         sn = -ONE/sqrt(sqrt(PI))
         do s = 0,(n/2)
            sn = -sn
            i = n-2*s
            res(i+1) = sn*sqrt(raised_to_(t,2*i-n))*sqrt_permutation_(n,s,i)
         end do
      else
         sn = -ONE
         do s = 0,(n/2)
            sn = -sn
            i = n-2*s
            res(i+1) = sn*raised_to_(t,i)*permutation_(n,s,i)
         end do
      end if

!   select case (self)
!     case (0); res = [   1.0d0
!     case (1); res = [   0.0d0,   2.0d0
!     case (2); res = [  -2.0d0,   0.0d0,   4.0d0
!     case (3); res = [   0.0d0, -12.0d0,   0.0d0,   8.0d0
!     case (4); res = [  12.0d0,   0.0d0, -48.0d0,   0.0d0,  16.0d0
!     case (5); res = [   0.0d0, 120.0d0,   0.0d0,-160.0d0,   0.0d0, 32.0d0
!     case (6); res = [-120.0d0,   0.0d0, 720.0d0,   0.0d0,-480.0d0,  0.0d0,  64.0d0
!     case default;
!        res(2:n+1) = TWO* (n-1).hermite_polynomial
!        res(1:n-1) = res(1:n-1) - TWO*(n-1)* (n-2).hermite_polynomial
!   end
   end function

   PURE function hermite_polynomial_prob_coeffs(self) result (res)
   ! Returns an array with the coefficients of the probabilist's
   ! Hermite Polynomials.  First elements is for x^0, second for x^1,
   ! and so on.  The length of the result is (self+1).
      INT, IN :: self
      VEC(REAL,self+1) :: res
      INT :: n,s,i,t
      REAL :: sn
   ENSURE(self>=0,"INT:hermite_polynomial_prob_coeffs ... order must be 0 or greater")
      select case (self)

      case (0); res = [1.0d0]
      case (1); res = [0.0d0,1.0d0]
      case (2); res = [-1.0d0,0.0d0,1.0d0]
      case (3); res = [0.0d0,-3.0d0,0.0d0,1.0d0]
      case (4); res = [3.0d0,0.0d0,-6.0d0,0.0d0,1.0d0]
      case (5); res = [0.0d0,15.0d0,0.0d0,-10.0d0,0.0d0,1.0d0]
      case (6); res = [-15.0d0,0.0d0,45.0d0,0.0d0,-15.0d0,0.0d0,1.0d0]

      case default
         n = self
         t = 2
         sn = -ONE
         res = ZERO
         do s = 0,(n/2)
            sn = -sn
            i = n-2*s
            res(i+1) = sn/raised_to_(t,s)*permutation_(n,s,i)
         end do
      end select
   end function

   pure function legendre_polynomial_coeffs(self) result (res)
   ! Returns an array with the coefficients of the Legendre Polynomials.  First
   ! elements is for x^0, second for x^1, and so on.  The length of the result
   ! is (self+1).  Method used is equation 12.64 from Arfken and Weber 4th ed.
      INT, IN :: self
      VEC(REAL,self+1) :: res
      INT :: k,sk,sk2,s2k
      REAL :: sn,s2
      select case (self)
      case (0); s2 = ONE
      case (1); s2 = TWO
      case (2); s2 = FOUR
      case (3); s2 = EIGHT
      case default; s2 = TWO**self
      end select
      res = ZERO
      do k = 0,self/2
         if (mod(k,2)==0) then; sn = ONE
         else; sn = -ONE
         end if
         sk = self-k
         sk2 = 2*sk
         s2k = self-2*k
         res(s2k+1) = sn*factorial_(sk2)/(s2*factorial_(k)*factorial_(sk)*factorial_(s2k))
      end do
   end function

   pure function assoc_legendre_coeffs(self,m) result (res)
   ! Returns d^m/dx^m(Pn(x)) for the associated Legendre polynomial, but does
   ! not multiply by (1-x^2)^(m/2).
      INT, IN :: self
      INT, IN :: m
      VEC(REAL,self+1) :: res
      INT :: i,j
      res = legendre_polynomial_coeffs_(self)
      do i = 1,abs(m)
         do j = 1,self+1
            res(j) = res(j)*(j-1)
         end do
         res = eoshift(res,1,ZERO)
      end do
      if (m<0) then
         res = factorial_((self+m))/factorial_((self-m))*res
         if (mod(m,2)==1) res = -res
      end if
   end function

   PURE recursive function assoc_laguerre_coeffs(self,k) result (res)
   ! Returns an array with the coefficients of the associated Laguerre
   ! polynomials, L_n^k (x) = (1/n!) exp(x) x^{-k) d^n/dx^n [exp(-x)
   ! x^{n+k}].  First elements is for x^0, second for x^1, and so on.
   ! The length of the result is (self+1).  The method is from the recursion
   ! expansion (13.44) in Arfken & Weber, Mathematical Methods for Physicists,
   ! 4th Edition.
      INT, IN :: self
      INT, IN :: k
      VEC(REAL,self+1) :: res
      VEC(REAL,self) :: res1,res2
      INT :: n,n1,n2
   ENSURE(self>=0,"INT:assoc_laguerre_coeffs ... non positive self")
      n = self
      n1 = n-1
      n2 = n-2
      select case (self)

      case (0); res = [1]
      case (1); res = [-1,k+1]

      case default
         res2 = assoc_laguerre_coeffs_(n2,k)
         res1 = assoc_laguerre_coeffs_(n1,k)
         res(1:n) = -(n+k-1)*res2
         res(1:n) = res(1:n)+(2*n+k-1)*res1
         res(2:n+1) = res(2:n+1)-res1
         res = res/n
      end select
   end function


! ==============
! Bit operations
! ==============

   pure function bit_count(self) result (res)
   ! Returns the number of bits set in "self".
      INT, IN :: self
      INT :: res
      INT :: tmp
      tmp = self
      res = 0
      do
         if (tmp==0) exit
         if (BIT_SET_AT_POS_IN(tmp,0)) res = res+1
         tmp = MOVE_BITS_RIGHT_IN(tmp,1)
      end do
   end function

   elemental function set_bit_at(self,pos) result (res)
   ! Set bit in "self" at position "pos" and return in "res"
      INT, IN :: self
      INT, IN :: pos
      INT :: res
      res = SET_BIT_AT_POS_IN(self,pos)
   end function

   pure function set_bits_at(self,pos) result (res)
   ! Set bits in "self" at *positions* "pos" and return in "res"
      INT, IN :: self
      VEC(INT,:), IN :: pos
      INT :: res
      INT :: i
      res = self  ! NOTE
      do i = 1,size(pos)
         res = SET_BIT_AT_POS_IN(res,pos(i))
      end do
   end function

   pure function set_bits_from_0(self,v) result (res)
   ! Sets bits in "self" (without zeroing) corresponding to non-zero
   ! elements (first->last) in INT vector "v" and return result "res".
   ! e.g. 1101 = set_bits_from([1,1,0,1])
      INT, IN :: self
      VEC(INT,:), IN :: v
      INT :: res

! ENSURE(v.dim<bit_size(self),"v too long")
! ENSURE(minval(v)>=0,"some v values too small")
! ENSURE(maxval(v)<=1,"some v values too large")
      INT :: i
      res = self  ! NOTE
      do i = size(v),1,-1
         if (v(i)==0) cycle
         res = SET_BIT_AT_POS_IN(res,i)
      end do
   end function

   pure function set_bits_from_1(self,v) result (res)
   ! Sets bits in "self" (without zeroing) corresponding to TRUE
   ! elements (first->last) in BIN vector "v" and return in "res".
   ! e.g. 1101 = set_bits_at([T,T,F,T])
      INT, IN :: self
      VEC(BIN,:), IN :: v
      INT :: res

! ENSURE(v.dim<bit_size(self),"v too long")
      INT :: i
      res = self  ! NOTE
      do i = size(v),1,-1
         if (NOT v(i)) cycle
         res = SET_BIT_AT_POS_IN(res,i)
      end do
   end function

   elemental function clear_bit_at(self,pos) result (res)
   ! Clear bit in "self" at position "pos" and return in "res"
      INT, IN :: self
      INT, IN :: pos
      INT :: res
      res = CLR_BIT_AT_POS_IN(self,pos)
   end function

   pure function clear_bits_at(self,pos) result (res)
   ! clear bits in "self" at *positions* "pos" and return in "res"
      INT, IN :: self
      VEC(INT,:), IN :: pos
      INT :: res
      INT :: i
      res = self  ! NOTE
      do i = 1,size(pos)
         res = CLR_BIT_AT_POS_IN(res,pos(i))
      end do
   end function

   pure function clear_bits_from_0(self,v) result (res)
   ! clear bits in "self" (without zeroing) corresponding to non-zero
   ! elements (first->last) in INT vector "v" and return result "res".
   ! e.g. 1101 = set_bits_from([1,1,0,1])
      INT, IN :: self
      VEC(INT,:), IN :: v
      INT :: res

! ENSURE(v.dim<bit_size(self),"v too long")
! ENSURE(minval(v)>=0,"some v values too small")
! ENSURE(maxval(v)<=1,"some v values too large")
      INT :: i
      res = self  ! NOTE
      do i = size(v),1,-1
         if (v(i)==0) cycle
         res = CLR_BIT_AT_POS_IN(res,i)
      end do
   end function

   pure function clear_bits_from_1(self,v) result (res)
   ! Clear bits in "self" (without zeroing) corresponding to TRUE
   ! elements (first->last) in BIN vector "v" and return in "res".
   ! e.g. 1101 = set_bits_at([T,T,F,T])
      INT, IN :: self
      VEC(BIN,:), IN :: v
      INT :: res

! ENSURE(v.dim<bit_size(self),"v too long")
      INT :: i
      res = self  ! NOTE
      do i = size(v),1,-1
         if (NOT v(i)) cycle
         res = CLR_BIT_AT_POS_IN(res,i)
      end do
   end function

   elemental function move_bits_left(self,n) result (res)
   ! Move bits left "n" places in "self" and return in "res"
      INT, IN :: self
      INT, IN :: n
      INT :: res
      res = MOVE_BITS_LEFT_IN(self,n)
   end function

   elemental function move_bits_right(self,n) result (res)
   ! Move bits left "n" places in "self" and return in "res"
      INT, IN :: self
      INT, IN :: n
      INT :: res
      res = MOVE_BITS_RIGHT_IN(self,n)
   end function

   elemental function has_bit_set_at(self,pos) result (res)
   ! Returns true if bit at "pos" is set.
      INT, IN :: self
      INT, IN :: pos
      BIN :: res
      res = BIT_SET_AT_POS_IN(self,pos)
   end function

   pure function has_all_bits_set_at(self,pos) result (res)
   ! Returns true if *all* bits in "pos" are set.
      INT, IN :: self
      VEC(INT,:), IN :: pos
      BIN :: res
      INT :: i
      res = TRUE
      do i = 1,size(pos)
         res = BIT_SET_AT_POS_IN(self,pos(i))
         if (NOT res) exit
      end do
   end function

   pure function has_any_bits_set_at(self,pos) result (res)
   ! Returns true if *any* bit at "pos" is set.
      INT, IN :: self
      VEC(INT,:), IN :: pos
      BIN :: res
      INT :: i
      res = FALSE
      do i = 1,size(pos)
         res = BIT_SET_AT_POS_IN(self,pos(i))
         if (res) exit
      end do
   end function

   pure function has_bits_set_like_0(self,v) result (res)
   ! Return TRUE if bits in "self" correspond to bits in vector "v" 
      INT, IN :: self
      VEC(INT,:), IN :: v
      BIN :: res

! ENSURE(v.dim<bit_size(self),"v too long")
! ENSURE(minval(v)>=0,"some v values too small")
! ENSURE(maxval(v)<=1,"some v values too large")
      INT :: i
      BIN :: set
      res = FALSE
      do i = size(v),1,-1
         set = BIT_SET_AT_POS_IN(self,i)
         res = (v(i)==1 AND set) OR (v(i)==0 AND NOT set)
         if (NOT res) exit
      end do
   end function

   pure function has_bits_set_like_1(self,v) result (res)
   ! Return TRUE if bits in "self" correspond to bits in vector "v"
      INT, IN :: self
      VEC(BIN,:), IN :: v
      BIN :: res

! ENSURE(v.dim<bit_size(self),"v too long")
      INT :: i
      BIN :: set
      res = FALSE
      do i = size(v),1,-1
         set = BIT_SET_AT_POS_IN(self,i)
         res = (v(i) AND set) OR (NOT v(i) AND NOT set)
         if (NOT res) exit
      end do
   end function

   elemental function bits_from(self,pos,len) result (res)
   ! Return the bits in "self" from "pos", length "len"
      INT, IN :: self
      INT, IN :: pos,len
      INT :: res
      res = BITS_FROM_POS_IN(self,pos,len)
   end function


! =============
! String widths
! =============

   pure function no_of_fields_per_value(self) result (res)
   ! Returns the number of fields used to print a "self".
      INT, IN :: self
      INT :: res
      if (FALSE) res = transfer(self,res)
      res = 1
   end function

   pure function str_length(self) result (res)
   ! Returns the minimal string length.
   ! Note: for non-real and non-complex only
      INT, IN :: self
      INT :: res
      STR(len=STR_SIZE) :: string

      ! The following code is inherited from INTRINSIC
      string = " "
      write(string,fmt="(i32)") self
      res = len_trim(adjustl(string))
   end function

   pure function bit_string_length(self) result (res)
   ! Returns the minimal string length.
   ! Note: for non-real and non-complex only
      INT, IN :: self
      INT :: res
      STR(len=STR_SIZE) :: string

      ! The following code is inherited from INTRINSIC
      string = " "
      write(string,fmt="(b32)") self
      res = len_trim(adjustl(string))
   end function

   pure subroutine get_str_length(self,sl,spaces)
   ! Returns the minimal string length.
   ! Note: for non-real and non-complex only
      INT, IN :: self
      INT, OUT :: sl
      INT, IN :: spaces
      STR(len=STR_SIZE) :: string

      ! The following code is inherited from INTRINSIC
      string = " "
      write(string,fmt="(i32)") self
      sl = len_trim(adjustl(string))+spaces
   end subroutine

   pure subroutine get_bit_str_length(self,sl,spaces)
   ! Returns the minimal string length.
   ! Note: for non-real and non-complex only
      INT, IN :: self
      INT, OUT :: sl
      INT, IN :: spaces
      STR(len=STR_SIZE) :: string

      ! The following code is inherited from INTRINSIC
      string = " "
      write(string,fmt="(b32)") self
      sl = len_trim(adjustl(string))+spaces
   end subroutine


! =================
! String conversion
! =================

   pure function to_str_int_0(self) result (string)
   ! Change self to a string of minimal length
      INT, IN :: self
      STR(len=str_length_(self)) :: string
      STR(len=STR_SIZE) :: tmpcpy

      ! Write self to STR into "tmpcpy"

      ! The following code is inherited from INTRINSIC
      tmpcpy = " "
      write(tmpcpy,fmt=*) self

      ! Transfer "tmpcpy" to the "string" with correct length
      write(string,fmt="(a)") trim(adjustl(tmpcpy))
   end function

   pure function to_str_int_1(self,fmt,left_justify) result (string)
   ! Change self to a "string" using the specified fortran "fmt".
      INT, IN :: self
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

   pure function to_str_int_2(self,width,left_justify) result (string)
   ! Change self to a string with specified "width".
      INT, IN :: self
      INT, IN :: width
      BIN, IN, optional :: left_justify
      STR(len=width) :: string
      BIN :: leftify
      STR(len=STR_SIZE) :: wid,fmt

      ! Write width as a string, in "wid"

      ! The following code is inherited from INTRINSIC
      write(wid,fmt="(a)") width

      ! Write the format "fmt" from "wid"
      write(fmt,fmt="(a)") "("//"I"//trim(wid)//")"

      ! Use the format "fmt" to get the result
      string = " "
      write(string,fmt=fmt) self

      ! Left justify
      leftify = TRUE
      if (present(left_justify)) leftify = left_justify
      if (leftify) string = adjustl(string)
   end function

   pure function to_bit_string_0(self) result (string)
   ! Change self to a string of minimal length
      INT, IN :: self
      STR(len=bit_string_length_(self)) :: string
      STR(len=STR_SIZE) :: tmpcpy

      ! Write self to STR into "tmpcpy"

      ! The following code is inherited from INTRINSIC
      tmpcpy = " "
      write(tmpcpy,fmt="(b32)") self

      ! Transfer "tmpcpy" to the "string" with correct length
      write(string,fmt="(a)") trim(adjustl(tmpcpy))
   end function

   pure function to_bit_string_1(self,fmt,left_justify) result (string)
   ! Change self to a "string" using the specified fortran "fmt".
      INT, IN :: self
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

   pure function to_bit_string_2(self,width,left_justify) result (string)
   ! Change self to a string with specified "width".
      INT, IN :: self
      INT, IN :: width
      BIN, IN, optional :: left_justify
      STR(len=width) :: string
      BIN :: leftify
      STR(len=STR_SIZE) :: wid,fmt

      ! Write width as a string, in "wid"

      ! The following code is inherited from INTRINSIC
      write(wid,fmt="(a)") width

      ! Write the format "fmt" from "wid"
      write(fmt,fmt="(a)") "("//"I"//trim(wid)//")"

      ! Use the format "fmt" to get the result
      string = " "
      write(string,fmt=fmt) self

      ! Left justify
      leftify = TRUE
      if (present(left_justify)) leftify = left_justify
      if (leftify) string = adjustl(string)
   end function


! ========================
! Unit conversion routines
! ========================

   PURE subroutine convert_to(self,units)
   ! Convert the number "self" in atomic units or generic units to a
   ! new number in "units".
      INT, INOUT :: self
      STR(len=*), IN :: units
   ENSURE(is_known_unit_(units),"INT:convert_to ... unknown units, "//units)
      self = nint(self*conversion_factor_(units))
   end subroutine

   PURE function to_units(self,units) result (res)
   ! Convert the number "self" in atomic units or generic units to a
   ! new number in "units".
      INT, IN :: self
      STR(len=*), IN :: units
      REAL :: res
   ENSURE(is_known_unit_(units),"INT:to_units ... unknown units, "//units)
      res = nint(self*conversion_factor_(units))
   end function

   PURE subroutine convert_from(self,units)
   ! Convert the number "self" from "units" system to a new number
   ! in atomic units or generic units.  Returns "err" whether it was successful.
      INT, INOUT :: self
      STR(len=*), IN :: units
   ENSURE(is_known_unit_(units),"INT:convert_from ... unknown units, "//units)
      self = nint(self/conversion_factor_(units))
   end subroutine

   PURE function from_units(self,units) result (res)
   ! Convert "self" from "units" system to a new number in atomic
   ! units or generic units.  Returns "err" whether it was successful.
      INT, IN :: self
      STR(len=*), IN :: units
      REAL :: res
   ENSURE(is_known_unit_(units),"INT:from_units ... unknown units, "//units)
      res = nint(self/conversion_factor_(units))
   end function

end module
