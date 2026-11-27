!------------------------------------------------------------------------------
!
!==============================================================================
! QUADRATURE
!
! For integrating one dimensional functions by using quadrature. Most of the
! method implemented so far are for Gauss type quadratures --- i.e.  getting
! abscissas and weights based on orthogonal polynomial theory. The Gauss
! quadratures are based on the Numerical Recipes, pp. 144-149.
!
! The following Gauss quadratures are available, with weight function W(x):
! . Gauss-Chebyshev (1st kind): W(x) = (1-x^2)^{-1/2},        -1 < x < 1
! . Gauss-Chebyshev (2nd kind): W(x) = (1-x^2)^{+1/2},        -1 < x < 1
! . Gauss-Chebyshev (3rd kind): W(x) = (x/(1-x))^{+1/2},       0 < x < 1
! . Gauss-Laguerre:             W(x) = x^alpha e^-x,           0 < x < inf
! . Gauss-Legendre:             W(x) = 1,                     -1 < x < 1
! . Gauss-Hermite:              W(x) = e^{-x^2},            -inf < x < inf
! . Gauss-Jacobi:               W(x) = (1-x)^alpha(1+x)^beta, -1 < x < 1
!
! The following standard quadratures are available:
! . Open rectangle    :       0 < x < 1
! . Extended trapezoid:       0 < x < 1
! . Extended Simpson  :       0 < x < 1
! Note that the extended trapezoid method becomes the Euler-Maclaurin formula
! if the derivatives at the endpoints can be made zero.
!
! NOTE: is is up to you to transforms the points and weights to the appropriate
! integration interval in your problem.
!
! Copyright (C) Dylan Jayatilaka, 2004
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
!==============================================================================
module QUADRATURE_MODULE

#  include "macros"

   include "quadrature.use"

   implicit none

   include "quadrature.int"

   VEC(STR(len=25),12), private :: allowed_kinds
   data allowed_kinds/ &
      "extended_simpson         ", &                
      "extended_trapezoid       ", &                
      "gauss_chebyshev_1st      ", &     
      "gauss_chebyshev_2nd      ", &     
      "gauss_chebyshev_3rd      ", &     
      "gauss_laguerre           ", & 
      "gauss_legendre           ", & 
      "gauss_hermite            ", &
      "gauss_jacobi             ", &
      "open_extended_simpson    ", &
      "open_extended_trapezoid  ", &
      "open_rectangle           "  /

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(QUADRATURE_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"QUADRATURE:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(QUADRATURE_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"QUADRATURE:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_eps(self,eps)
   ! Set the relative accuracy needed for the roots. Used only for Gauss
   ! quadrature methods.
      type(QUADRATURE_TYPE), INOUT :: self
      REAL, IN :: eps
   ENSURE(eps>ZERO,"QUADRATURE:set_eps ... must have +ve eps")
      self%eps = eps
   end subroutine

   PURE subroutine set_maxit(self,maxit)
   ! Set the maximum number of iterations to use when solving for the roots.
   ! Used only for Gauss quadrature methods.
      type(QUADRATURE_TYPE), INOUT :: self
      INT, IN :: maxit
   ENSURE(maxit>0,"QUADRATURE:set_maxit ... maxit must be positive")
      self%maxit = maxit
   end subroutine

   PURE subroutine set_kind(self,kind)
   ! Set the kind of interpolation to use. 
      type(QUADRATURE_TYPE), INOUT :: self
      STR(len=*), IN :: kind
   ENSURE(is_one_of_(kind,allowed_kinds),"QUADRATURE:set_kind ... unrecognized kind, "//trim(kind))
      self%kind = lower_cased_(kind)
   end subroutine

   PURE subroutine set_n_pts(self,n_pts)
   ! Set the number of quadrature points wanted
      type(QUADRATURE_TYPE), INOUT :: self
      INT, IN :: n_pts
   ENSURE(n_pts>0,"QUADRATURE:set_n_pts ... n_points must be positive")
      self%n_pts = n_pts
   end subroutine

   PURE subroutine set_Gauss_Laguerre_alpha(self,alpha)
   ! Set the Gauss-Laguerre alpha parameter
      type(QUADRATURE_TYPE), INOUT :: self
      REAL, IN :: alpha
      self%Gauss_Laguerre_alpha = alpha
   end subroutine

   PURE subroutine set_Gauss_Jacobi_alpha(self,alpha)
   ! Set the Gauss-Jacobi alpha parameter
      type(QUADRATURE_TYPE), INOUT :: self
      REAL, IN :: alpha
      self%Gauss_Jacobi_alpha = alpha
   end subroutine

   PURE subroutine set_Gauss_Jacobi_beta(self,beta)
   ! Set the Gauss-Jacobi beta parameter
      type(QUADRATURE_TYPE), INOUT :: self
      REAL, IN :: beta
      self%Gauss_Jacobi_beta = beta
   end subroutine


!  ==========================
!  Finalize quadrature points 
!  ==========================

   PURE subroutine set_quadrature_points(self)
   ! Set the quadrature points. NOTE: if you want non-standard limits you must
   ! call .set_limits after this routine to rescale the points and weights
      type(QUADRATURE_TYPE), INOUT :: self
   ENSURE(self%kind/=" ","QUADRATURE:set_quadrature_points ... the kind of quadrature has not been set")
   ENSURE(self%n_pts>0,"QUADRATURE:set_quadrature_points ... no points have been set")
      if (self%kind(1:5)=="gauss") then; call set_roots_and_weights_(self)
      else; call set_points_and_weights_(self)
      end if
      self%finalized = TRUE
   end subroutine

   PURE subroutine set_limits(self,limits)
   ! Read in the limits of integration
      type(QUADRATURE_TYPE), INOUT :: self
      VEC(REAL,2), IN :: limits
   ENSURE(limits(1)<=limits(2),"QUADRATURE:set_limits ... lower limit must be smaller than upper limit")
   ENSURE(self%finalized,"QUADRATURE:set_limits ... set limits after defining quadrature points and weights")
      self%lower_limit = limits(1)
      self%upper_limit = limits(2)
      call rescale_points_and_weights_(self)
   end subroutine

   PURE subroutine rescale_points_and_weights(self)
   ! Rescale the points and weights using a linear transformation, if different
   ! lower and upper limits have been used.
      type(QUADRATURE_TYPE), INOUT :: self
   ENSURE(is_one_of_(self%kind,allowed_kinds([1,2,3,4,5,7,9,12])),"QUADRATURE:rescale_points_and_weights ... unrecognized kind, "//trim(self%kind))
   ENSURE(self%kind/=" ","QUADRATURE:rescale_points_and_weights ... the kind of quadrature has not been set")
   ENSURE(self%n_pts>0,"QUADRATURE:rescale_points_and_weights ... no points have been set")
   ENSURE(self%finalized,"QUADRATURE:rescale_points_and_weights ... set limits after defining quadrature points and weights")
      select case (self%kind)
      case ("extended_simpson   "); call rescale_z0p1_(self)
      case ("extended_trapezoid "); call rescale_z0p1_(self)
      case ("gauss_chebyshev_1st"); call rescale_m1p1_(self)
      case ("gauss_chebyshev_2nd"); call rescale_m1p1_(self)
      case ("gauss_chebyshev_3rd"); call rescale_z0p1_(self)
      case ("gauss_laguerre     ")  ! DIE("cant rescale linearly")
      case ("gauss_legendre     "); call rescale_m1p1_(self)
      case ("gauss_hermite      ")  ! DIE("cant rescale linearly")
      case ("gauss_jacobi       "); call rescale_m1p1_(self)
      case ("open_rectangle     "); call rescale_z0p1_(self)
      end select
   end subroutine

   PURE subroutine rescale_m1p1(self)
   ! Rescale the points and weights using a linear transformation, from the
   ! range [-1,1] to the new range.
      type(QUADRATURE_TYPE), INOUT :: self
      REAL :: s,m
      INT :: i
   ENSURE(self%kind/=" ","QUADRATURE:rescale_m1p1 ... the kind of quadrature has not been set")
   ENSURE(self%n_pts>0,"QUADRATURE:rescale_m1p1 ... no points have been set")
   ENSURE(self%finalized,"QUADRATURE:rescale_m1p1 ... set limits after defining quadrature points and weights")
      if (self%lower_limit==-ONE AND self%upper_limit==ONE) return
      s = HALF*(self%upper_limit-self%lower_limit)
      m = HALF*(self%upper_limit+self%lower_limit)
      do i = 1,self%n_pts
         self%pts(i) = s*self%pts(i)+m
         self%wts(i) = s*self%wts(i)
      end do
   end subroutine

   PURE subroutine rescale_z0p1(self)
   ! Rescale the points and weights using a linear transformation, from the
   ! range [0,1] to the new range.
      type(QUADRATURE_TYPE), INOUT :: self
      REAL :: s,m
      INT :: i
   ENSURE(self%kind/=" ","QUADRATURE:rescale_z0p1 ... the kind of quadrature has not been set")
   ENSURE(self%n_pts>0,"QUADRATURE:rescale_z0p1 ... no points have been set")
   ENSURE(self%finalized,"QUADRATURE:rescale_z0p1 ... set limits after defining quadrature points and weights")
      if (self%lower_limit==ZERO AND self%upper_limit==ONE) return
      s = (self%upper_limit-self%lower_limit)
      m = HALF*(self%upper_limit+self%lower_limit)
      do i = 1,self%n_pts
         self%pts(i) = s*(self%pts(i)-HALF)+m
         self%wts(i) = s*self%wts(i)
      end do
   end subroutine


!  ==================================
!  Gauss quadrature roots and weights
!  ==================================

   PURE subroutine set_roots_and_weights(self)
   ! Set the Gauss-Legendre roots and weights
      type(QUADRATURE_TYPE), INOUT :: self
   ENSURE(is_one_of_(self%kind,allowed_kinds(3:9)),"QUADRATURE:set_roots_and_weights ... unrecognized kind, "//trim(self%kind))
   ENSURE(self%n_pts>0,"QUADRATURE:set_roots_and_weights ... must have positive number of points")
   ENSURE(self%kind(1:5)=="gauss","QUADRATURE:set_roots_and_weights ... must be a gauss quadrature")
      call create_(self%pts,self%n_pts)  ! These are the roots
      call create_(self%wts,self%n_pts)
      select case (self%kind)
      case ("gauss_chebyshev_1st"); call set_Gauss_Chebyshev_1st_(self)
      case ("gauss_chebyshev_2nd"); call set_Gauss_Chebyshev_2nd_(self)
      case ("gauss_chebyshev_3rd"); call set_Gauss_Chebyshev_3rd_(self)
      case ("gauss_laguerre     "); call set_Gauss_Laguerre_(self)
      case ("gauss_legendre     "); call set_Gauss_Legendre_(self)
      case ("gauss_hermite      "); call set_Gauss_Hermite_(self)
      case ("gauss_jacobi       "); call set_Gauss_Jacobi_(self)
      end select
   end subroutine

   PURE subroutine set_Gauss_Chebyshev_1st(self)
   ! Set the Gauss-Legendre roots and weights
      type(QUADRATURE_TYPE), INOUT :: self
      INT :: i
      REAL :: pi_n,c
      self%lower_limit = -ONE
      self%upper_limit = ONE
      pi_n = PI/self%n_pts
      self%wts = pi_n
      do i = 1,self%n_pts
         c = cos(pi_n*(i-HALF))
         self%pts(i) = c
         if (NOT self%fold_wt_fn_in) cycle

         ! Include weight function in quadrature weight
         self%wts(i) = self%wts(i)*sqrt(ONE-c*c)
      end do
   end subroutine

   PURE subroutine set_Gauss_Chebyshev_2nd(self)
   ! Set the Gauss-Legendre roots and weights
      type(QUADRATURE_TYPE), INOUT :: self
      INT :: i
      REAL :: pi_n,c
      self%lower_limit = -ONE
      self%upper_limit = ONE
      pi_n = PI/(self%n_pts+1)
      do i = 1,self%n_pts
         c = cos(i*pi_n)
         self%pts(i) = c
         self%wts(i) = pi_n*(ONE-c*c)
         if (NOT self%fold_wt_fn_in) cycle

         ! Include weight function in quadrature weight
         self%wts(i) = self%wts(i)/sqrt(ONE-c*c)
      end do
   end subroutine

   PURE subroutine set_Gauss_Chebyshev_3rd(self)
   ! Set the Gauss-Legendre roots and weights
      type(QUADRATURE_TYPE), INOUT :: self
      INT :: i
      REAL :: pi_n,c
      self%lower_limit = ZERO
      self%upper_limit = ONE
      pi_n = PI/(2*self%n_pts+1)
      do i = 1,self%n_pts
         c = cos(pi_n*(i-HALF))
         c = c*c
         self%pts(i) = c
         self%wts(i) = pi_n*TWO*c
         if (NOT self%fold_wt_fn_in) cycle

         ! Include weight function in quadrature weight
         self%wts(i) = self%wts(i)*sqrt((ONE-c)/c)
      end do
   end subroutine

   PURE subroutine set_Gauss_Laguerre(self)
   ! Set the Gauss-Laguerre roots and weights
      type(QUADRATURE_TYPE), INOUT :: self
      INT :: i,j,n,i2,it
      REAL :: a,p1,p2,p3,pp,z,z1,g1,g2
      self%lower_limit = ZERO
      self%upper_limit = huge(ONE)
      n = self%n_pts
      a = self%Gauss_Laguerre_alpha

      ! Starting guesses (Numerical Recipes, gaulag): each root starts
      ! from the previous one, so "z" is carried across the loop.
      z = ZERO
      do i = 1,n
         i2 = i-2
         select case (i)
         case (1); z = (ONE+a)*(THREE+0.92d0*a)/(ONE+2.4d0*n+1.8d0*a)
         case (2); z = z+(15.0d0+6.25d0*a)/(ONE+0.9d0*a+2.5d0*n)
         case default; z = z+((ONE+2.55d0*i2)/(1.9d0*i2)+1.26d0*i2*a/(ONE+3.5d0*i2))*(z-self%pts(i2))/(ONE+0.3d0*a)
         end select
         it = 0
         do
            it = it+1
            p1 = ONE
            p2 = ZERO
            do j = 1,n
               p3 = p2
               p2 = p1
               p1 = ((2*j-1+a-z)*p2-(j-1+a)*p3)/j
            end do
            pp = (n*p1-(n+a)*p2)/z
            z1 = z
            z = z1-p1/pp
            if (abs(z-z1)<self%eps) exit
         end do
         ENSURE(it<=self%maxit,"QUADRATURE:set_Gauss_Laguerre ... too many interations")
         self%pts(i) = z
         g1 = a+n
         g2 = n
         self%wts(i) = -exp(ln_gamma_(g1)-ln_gamma_(g2))/(pp*n*p2)
      end do
      if (NOT self%fold_wt_fn_in) return
      do i = 1,self%n_pts
         z = self%pts(i)
         z = z**(-a)*exp(z)
         self%wts(i) = self%wts(i)*z
      end do
   end subroutine

   PURE subroutine set_Gauss_Legendre(self)
   ! Set the Gauss-Legendre roots and weights
      type(QUADRATURE_TYPE), INOUT :: self
      INT :: i,j,n,m,it
      REAL :: p1,p2,p3,pp,x1,x2,xl,xm,z,z1
      self%lower_limit = -ONE
      self%upper_limit = ONE

      ! Can change these limits and it will still work
      x1 = -ONE  ! lower_limit
      x2 = +ONE  ! upper_limit
      n = self%n_pts
      m = (n+1)/2
      xm = HALF*(x2+x1)
      xl = HALF*(x2-x1)
      do i = 1,m
         z = cos(PI*(i-QUARTER)/(n+HALF))
         it = 0
         do
            it = it+1
            p1 = ONE
            p2 = ZERO
            do j = 1,n
               p3 = p2
               p2 = p1
               p1 = ((2*j-1)*z*p2-(j-1)*p3)/j
            end do
            pp = n*(z*p1-p2)/(z*z-ONE)
            z1 = z
            z = z1-p1/pp
            if (abs(z-z1)<self%eps) exit
         end do
         ENSURE(it<=self%maxit,"QUADRATURE:set_Gauss_Legendre ... too many iterations")
         self%pts(i) = xm-xl*z
         self%pts(n+1-i) = xm+xl*z
         self%wts(i) = TWO*xl/((ONE-z*z)*pp*pp)
         self%wts(n+1-i) = self%wts(i)
      end do
   end subroutine

   PURE subroutine set_Gauss_Hermite(self)
   ! Set the Gauss-Hermite roots and weights
      type(QUADRATURE_TYPE), INOUT :: self
      INT :: i,j,n,m,it
      REAL :: pim4,p1,p2,p3,pp,z,z1
      self%lower_limit = -huge(ONE)
      self%upper_limit = huge(ONE)
      pim4 = 0.75112554444649425d0
      n = self%n_pts
      m = (n+1)/2

      ! Starting guesses (Numerical Recipes, gauher); "z" is carried
      ! across the loop, each root starting from the previous one.
      z = ZERO
      do i = 1,m
         select case (i)
         case (1); z = sqrt(REALIFY(2*n+1))-1.85575d0*(2*n+1)**(-SIXTH)
         case (2); z = z-1.14d0*n**0.426d0/z
         case (3); z = 1.86d0*z-0.86d0*self%pts(1)
         case (4); z = 1.91d0*z-0.91d0*self%pts(2)
         case default; z = TWO*z-self%pts(i-2)
         end select
         it = 0
         do
            it = it+1
            p1 = pim4
            p2 = ZERO
            do j = 1,n
               p3 = p2
               p2 = p1
               p1 = z*sqrt(TWO/j)*p2-sqrt(REALIFY(j-1)/REALIFY(j))*p3
            end do
            pp = sqrt(TWO*n)*p2
            z1 = z
            z = z1-p1/pp
            if (abs(z-z1)<self%eps) exit
         end do
         ENSURE(it<=self%maxit,"QUADRATURE:set_Gauss_Hermite ... too many interations")
         self%pts(i) = z
         self%pts(n+1-i) = -z
         self%wts(i) = TWO/(pp*pp)
         self%wts(n+1-i) = self%wts(i)
      end do
      if (NOT self%fold_wt_fn_in) return
      do i = 1,self%n_pts
         z = self%pts(i)
         z = exp(z*z)
         self%wts(i) = self%wts(i)*z
      end do
   end subroutine

   PURE subroutine set_Gauss_Jacobi(self)
   ! Set the Gauss-Jacobi roots and weights
      type(QUADRATURE_TYPE), INOUT :: self
      INT :: i,j,n,it
      REAL :: al,be,albe,an,bn,a,b,c,r1,r2,r3,p1,p2,p3,pp,z,z1,tmp,g1,g2,g3,g4
      self%lower_limit = -ONE
      self%upper_limit = ONE
      al = self%Gauss_Jacobi_alpha
      be = self%Gauss_Jacobi_beta
      albe = al+be
      n = self%n_pts

      ! Starting guesses (Numerical Recipes, gaujac); "z" is carried
      ! across the loop, each root starting from the previous one.
      z = ZERO
      do i = 1,n
         if (i==1) then
            an = al/n
            bn = be/n
            r1 = (ONE+al)*(2.78d0/(FOUR+n*n)+0.768d0*an/n)
            r2 = ONE+1.48d0*an+0.96d0*bn+0.452d0*an*an+0.83d0*an*bn
            z = ONE-r1/r2
         else if (i==2) then
            r1 = (4.1d0+al)/((ONE+al)*(ONE+0.156d0*al))
            r2 = ONE+0.06d0*(n-EIGHT)*(ONE+0.12d0*al)/n
            r3 = ONE+0.012d0*be*(ONE+0.25d0*abs(al))/n
            z = z-(ONE-z)*r1*r2*r3
         else if (i==3) then
            r1 = (1.67d0+0.28d0*al)/(ONE+0.37d0*al)
            r2 = ONE+0.22d0*(n-EIGHT)/n
            r3 = ONE+EIGHT*be/((6.28d0+be)*n*n)
            z = z-(self%pts(1)-z)*r1*r2*r3
         else if (i==(n-1)) then
            r1 = (ONE+0.235d0*be)/(0.766d0+0.119d0*be)
            r2 = ONE/(ONE+0.639d0*(n-FOUR)/(ONE+0.71d0*(n-FOUR)))
            r3 = ONE/(ONE+20.0d0*al/((7.5d0+al)*n*n))
            z = z+(z-self%pts(n-3))*r1*r2*r3
         else if (i==n) then
            r1 = (ONE+0.37d0*be)/(1.67d0+0.28d0*be)
            r2 = ONE/(ONE+0.22d0*(n-EIGHT)/n)
            r3 = ONE/(ONE+EIGHT*al/((6.28d0+al)*n*n))
            z = z+(z-self%pts(n-2))*r1*r2*r3
         else
            z = THREE*self%pts(i-1)-THREE*self%pts(i-2)+self%pts(i-3)
         end if
         it = 0
         do
            it = it+1
            tmp = TWO+albe
            p1 = (al-be+tmp*z)/TWO
            p2 = ONE
            do j = 2,n
               p3 = p2
               p2 = p1
               tmp = 2*j+albe
               a = 2*j*(j+albe)*(tmp-TWO)
               b = (tmp-ONE)*(al*al-be*be+tmp*(tmp-TWO)*z)
               c = TWO*(j-1+al)*(j-1+be)*tmp
               p1 = (b*p2-c*p3)/a
            end do
            pp = (n*(al-be-tmp*z)*p1+TWO*(n+al)*(n+be)*p2)/(tmp*(ONE-z*z))
            z1 = z
            z = z1-p1/pp
            if (abs(z-z1)<self%eps) exit
         end do
         ENSURE(it<=self%maxit,"QUADRATURE:set_Gauss_Jacobi ... too many interations")
         self%pts(i) = z
         g1 = al+n
         g2 = be+n
         g3 = n+ONE
         g4 = n+albe+ONE
         self%wts(i) = exp(ln_gamma_(g1)+ln_gamma_(g2)-ln_gamma_(g3)-ln_gamma_(g4))*tmp*TWO**albe/(pp*p2)
      end do
      if (NOT self%fold_wt_fn_in) return
      do i = 1,self%n_pts
         z = self%pts(i)
         z = (1-z)**(-al)*(1+z)**(-be)
         self%wts(i) = self%wts(i)*z
      end do
   end subroutine


!  ============================
!  Non-Gauss points and weights
!  ============================

   PURE subroutine set_points_and_weights(self)
   ! Set the points and weights
      type(QUADRATURE_TYPE), INOUT :: self
   ENSURE(is_one_of_(self%kind,allowed_kinds([1,2,3,4,10,11,12])),"QUADRATURE:set_points_and_weights ... unrecognized kind, "//trim(self%kind))
   ENSURE(self%n_pts>0,"QUADRATURE:set_points_and_weights ... must have positive number of points")
      call create_(self%pts,self%n_pts)  ! These are the roots
      call create_(self%wts,self%n_pts)
      select case (self%kind)
      case ("extended_simpson       "); call set_extended_simpson_(self)
      case ("extended_trapezoid     "); call set_extended_trapezoid_(self)
      case ("open_extended_simpson  "); call set_open_extended_simpson_(self)
      case ("open_extended_trapezoid"); call set_open_extended_trapezoid_(self)
      case ("open_rectangle         "); call set_open_rectangle_(self)
      end select
   end subroutine

   PURE subroutine set_open_rectangle(self)
   ! Set open rectangle points and weights in the interval [0,1].
      type(QUADRATURE_TYPE), INOUT :: self
      INT :: i
      REAL :: h,h2,r
   ENSURE(self%n_pts>=3,"QUADRATURE:set_open_rectangle ... must at least 1 points")
      self%lower_limit = ZERO
      self%upper_limit = ONE
      h = ONE/(self%n_pts+1)
      h2 = 1.5d0*h
      self%wts = h
      self%wts(1) = h2
      self%wts(self%n_pts) = h2
      r = h
      do i = 1,self%n_pts
         self%pts(i) = r
         r = r+h
      end do
   end subroutine

   PURE subroutine set_extended_trapezoid(self)
   ! Set extended trapezoid points and weights. The error is (-n h^3
   ! f''(z)/12) where n in the number of points minus one, h is the
   ! point spacing, and z is a number in the integration interval,
   ! which is here [0,1].
      type(QUADRATURE_TYPE), INOUT :: self
      INT :: i
      REAL :: h,h2,r
   ENSURE(self%n_pts>=2,"QUADRATURE:set_extended_trapezoid ... must at least 2 points")
      self%lower_limit = ZERO
      self%upper_limit = ONE
      h = ONE/(self%n_pts-1)
      h2 = HALF*h
      self%wts = h
      self%wts(1) = h2
      self%wts(self%n_pts) = h2
      r = ZERO
      do i = 1,self%n_pts
         self%pts(i) = r
         r = r+h
      end do
   end subroutine

   PURE subroutine set_open_extended_trapezoid(self)
   ! Set open extended trapezoid points and weights. Use this if you
   ! know that the function to be quadrated is zero on the endpoints.
   ! The error is (-n h^3 f''(z)/12) where n in the number of points
   ! minus one, h is the point spacing, and z is a number in the
   ! integration interval, which is here [0,1].
      type(QUADRATURE_TYPE), INOUT :: self
      INT :: i
      REAL :: h,r
   ENSURE(self%n_pts>2,"QUADRATURE:set_open_extended_trapezoid ... must have greater than 2 points")
      self%lower_limit = ZERO
      self%upper_limit = ONE
      h = ONE/(self%n_pts+1)
      self%wts = h
      r = h
      do i = 1,self%n_pts
         self%pts(i) = r
         r = r+h
      end do
   end subroutine

   PURE subroutine set_extended_simpson(self)
   ! Set extended Simpson points and weights
   ! The error is (-n h^5 f''''(z)/90) where n in the number of points minus one,
   ! h is the point spacing, and z is a number in the integration interval,
   ! which is here [0,1].
      type(QUADRATURE_TYPE), INOUT :: self
      INT :: i
      REAL :: h,h2,h3,h4,r
   ENSURE(self%n_pts>=5,"QUADRATURE:set_extended_simpson ... must at least 5 points")
   ENSURE(mod(self%n_pts,2)==1,"QUADRATURE:set_extended_simpson ... must have odd number of points")
      self%lower_limit = ZERO
      self%upper_limit = ONE
      h3 = THIRD/(self%n_pts-1)
      h4 = FOUR*h3
      h2 = TWO*h3
      h = THREE*h3
      self%wts(1) = h3
      self%wts(self%n_pts) = h3
      do i = 2,self%n_pts-1,2
         self%wts(i) = h4
      end do
      do i = 3,self%n_pts-2,2
         self%wts(i) = h2
      end do
      r = ZERO
      do i = 1,self%n_pts
         self%pts(i) = r
         r = r+h
      end do
   end subroutine

   PURE subroutine set_open_extended_simpson(self)
   ! Set open extended Simpson points and weights: Numerical Recipes
   ! eq. (4.1.20), the fourth-order open formula on n interior points
   ! x_i = i h, h = 1/(n+1), in [0,1]. Exact for cubics; the endpoint
   ! values are never used, so the integrand may be singular there.
      type(QUADRATURE_TYPE), INOUT :: self
      INT :: i,n
      REAL :: h,h2,h4,r
   ENSURE(self%n_pts>=7,"QUADRATURE:set_open_extended_simpson ... must have at least 7 points")
   ENSURE(mod(self%n_pts,2)==1,"QUADRATURE:set_open_extended_simpson ... must have odd number of points")
      self%lower_limit = ZERO
      self%upper_limit = ONE
      n = self%n_pts
      h = ONE/(n+1)
      h4 = FOUR*THIRD*h
      h2 = TWO*THIRD*h

      ! End weights, 27/12, 0, 13/12, mirrored at the other end
      self%wts(1) = 2.25d0*h; self%wts(n) = self%wts(1)
      self%wts(2) = ZERO; self%wts(n-1) = ZERO
      self%wts(3) = h*13.0d0/12.0d0
      self%wts(n-2) = self%wts(3)

      ! Interior Simpson alternation 4/3, 2/3, ...
      do i = 4,n-3,2
         self%wts(i) = h4
      end do
      do i = 5,n-3,2
         self%wts(i) = h2
      end do
      r = h
      do i = 1,self%n_pts
         self%pts(i) = r
         r = r+h
      end do
   end subroutine


!  ==================
!  Quadrature methods
!  ==================

   function quadrate(self,func) result (res)
   ! Quadrate the function "func".
      type(QUADRATURE_TYPE), IN :: self
      interface
         function func(x) result (res)
            REAL, IN :: x
            REAL :: res
         end function
      end interface
      REAL :: res
      INT :: i
      VEC(REAL,:), allocatable :: f
   ENSURE(self%n_pts>0,"QUADRATURE:quadrate ... no points")
   ENSURE(allocated(self%pts),"QUADRATURE:quadrate ... no points")
   ENSURE(allocated(self%wts),"QUADRATURE:quadrate ... no weights")
      call create_(f,self%n_pts)
      do i = 1,self%n_pts
         f(i) = func(self%pts(i))
      end do
      res = sum(self%wts*f)
      call destroy_(f)
   end function


!  ==========
!  Self-check
!  ==========

   function check_rules() result (n_failed)
   ! Check every rule against integrals known in closed form, print one
   ! line per rule, and return the number of rules that failed. Each
   ! Gauss rule is tested on the moments of its own weight function,
   ! which an n-point rule reproduces exactly up to degree 2n-1; each
   ! Newton-Cotes rule on the polynomials it must integrate exactly.
   ! Driven by run_quadrature (the `quadrature_rules_integrate_exactly` ctest).
      INT :: n_failed
      type(QUADRATURE_TYPE) :: q
      n_failed = 0
      call flush_(stdout)
      call text_(stdout,"Quadrature rule exactness (max |error|)")
      call flush_(stdout)
      call check_legendre(q,8,n_failed)
      call check_chebyshev_1st(q,8,n_failed)
      call check_chebyshev_2nd(q,8,n_failed)
      call check_chebyshev_3rd(q,8,n_failed)
      call check_laguerre(q,8,n_failed)
      call check_hermite(q,8,n_failed)
      call check_jacobi(q,8,n_failed)
      call check_newton_cotes(q,"open_rectangle          ",1,n_failed)
      call check_newton_cotes(q,"extended_trapezoid      ",1,n_failed)
      call check_open_trapezoid(q,n_failed)
      call check_newton_cotes(q,"extended_simpson        ",3,n_failed)
      call check_newton_cotes(q,"open_extended_simpson   ",3,n_failed)
      call flush_(stdout)
      call show_(stdout,"Rules failed =",n_failed)
   end function

   subroutine report_check(name,err,tol,n_failed)
   ! Print one line and count a failure
      STR(len=*), IN :: name
      REAL, IN :: err,tol
      INT, INOUT :: n_failed
      if (err<=tol) then
         call text_(stdout,"  "//trim(name)//" ok    "//to_str_(err))
      else
         call text_(stdout,"  "//trim(name)//" FAIL  "//to_str_(err))
         n_failed = n_failed+1
      end if
   end subroutine

   pure function double_factorial(k) result (res)
   ! k!! as a REAL, with (-1)!! = 0!! = 1
      INT, IN :: k
      REAL :: res
      INT :: i
      res = ONE
      i = k
      do
         if (i<2) exit
         res = res*i
         i = i-2
      end do
   end function

   subroutine check_legendre(q,n,n_failed)
   ! int_{-1}^{1} x^k dx = 2/(k+1) for even k, 0 for odd k
      type(QUADRATURE_TYPE), INOUT :: q
      INT, IN :: n
      INT, INOUT :: n_failed
      INT :: k
      REAL :: exact,err
      call set_kind_(q,"gauss_legendre")
      call set_n_pts_(q,n)
      call set_quadrature_points_(q)
      err = ZERO
      do k = 0,2*n-1
         exact = ZERO
         if (mod(k,2)==0) exact = TWO/(k+1)
         err = max(err,abs(sum(q%wts*q%pts**k)-exact))
      end do
      call report_check("gauss_legendre         ",err,TOL(12),n_failed)
   end subroutine

   subroutine check_chebyshev_1st(q,n,n_failed)
   ! The weights are folded, w_i = (pi/n) sqrt(1-x_i^2); undo the fold and
   ! test int x^k/sqrt(1-x^2) dx = pi (k-1)!!/k!! for even k
      type(QUADRATURE_TYPE), INOUT :: q
      INT, IN :: n
      INT, INOUT :: n_failed
      INT :: k
      REAL :: exact,err
      call set_kind_(q,"gauss_chebyshev_1st")
      call set_n_pts_(q,n)
      call set_quadrature_points_(q)
      err = ZERO
      do k = 0,2*n-1
         exact = ZERO
         if (mod(k,2)==0) exact = PI*double_factorial(k-1)/double_factorial(k)
         err = max(err,abs(sum(q%wts*q%pts**k/sqrt(ONE-q%pts**2))-exact))
      end do
      call report_check("gauss_chebyshev_1st    ",err,TOL(12),n_failed)
   end subroutine

   subroutine check_chebyshev_2nd(q,n,n_failed)
   ! int x^k sqrt(1-x^2) dx = pi (k-1)!!/(k+2)!! for even k
      type(QUADRATURE_TYPE), INOUT :: q
      INT, IN :: n
      INT, INOUT :: n_failed
      INT :: k
      REAL :: exact,err
      call set_kind_(q,"gauss_chebyshev_2nd")
      call set_n_pts_(q,n)
      call set_quadrature_points_(q)
      err = ZERO
      do k = 0,2*n-1
         exact = ZERO
         if (mod(k,2)==0) exact = PI*double_factorial(k-1)/double_factorial(k+2)
         err = max(err,abs(sum(q%wts*q%pts**k*sqrt(ONE-q%pts**2))-exact))
      end do
      call report_check("gauss_chebyshev_2nd    ",err,TOL(12),n_failed)
   end subroutine

   subroutine check_chebyshev_3rd(q,n,n_failed)
   ! int_0^1 x^k sqrt(x/(1-x)) dx = Gamma(k+3/2) Gamma(1/2) / Gamma(k+2)
      type(QUADRATURE_TYPE), INOUT :: q
      INT, IN :: n
      INT, INOUT :: n_failed
      INT :: k
      REAL :: exact,err,g1,g2,g3
      call set_kind_(q,"gauss_chebyshev_3rd")
      call set_n_pts_(q,n)
      call set_quadrature_points_(q)
      err = ZERO
      do k = 0,2*n-1
         g1 = k+1.5d0
         g2 = HALF
         g3 = k+TWO
         exact = exp(ln_gamma_(g1)+ln_gamma_(g2)-ln_gamma_(g3))
         err = max(err,abs(sum(q%wts*q%pts**k*sqrt(q%pts/(ONE-q%pts)))-exact))
      end do
      call report_check("gauss_chebyshev_3rd    ",err,TOL(12),n_failed)
   end subroutine

   subroutine check_laguerre(q,n,n_failed)
   ! int_0^inf x^(k+a) e^-x dx = Gamma(k+a+1), a = 1. The folded weights
   ! carry x^-a e^x, so the integrand applied is x^(k+a) e^-x. Relative error.
      type(QUADRATURE_TYPE), INOUT :: q
      INT, IN :: n
      INT, INOUT :: n_failed
      INT :: k
      REAL :: a,exact,err,g
      a = ONE
      call set_kind_(q,"gauss_laguerre")
      call set_n_pts_(q,n)
      call set_Gauss_Laguerre_alpha_(q,a)
      call set_quadrature_points_(q)
      err = ZERO
      do k = 0,2*n-1
         g = k+a+ONE
         exact = exp(ln_gamma_(g))
         err = max(err,abs(sum(q%wts*q%pts**(k+a)*exp(-q%pts))-exact)/exact)
      end do
      call report_check("gauss_laguerre (rel)   ",err,TOL(10),n_failed)
   end subroutine

   subroutine check_hermite(q,n,n_failed)
   ! int x^k e^-x^2 dx = Gamma((k+1)/2) for even k. Relative error, the
   ! moments reaching ~2000.
      type(QUADRATURE_TYPE), INOUT :: q
      INT, IN :: n
      INT, INOUT :: n_failed
      INT :: k
      REAL :: exact,err,g
      call set_kind_(q,"gauss_hermite")
      call set_n_pts_(q,n)
      call set_quadrature_points_(q)
      err = ZERO
      do k = 0,2*n-1
         exact = ZERO
         if (mod(k,2)==0) then
            g = HALF*(k+1)
            exact = exp(ln_gamma_(g))
         end if
         if (mod(k,2)==0) err = max(err,abs(sum(q%wts*q%pts**k*exp(-q%pts**2))-exact)/exact)
      end do
      call report_check("gauss_hermite (rel)    ",err,TOL(10),n_failed)
   end subroutine

   subroutine check_jacobi(q,n,n_failed)
   ! alpha = 1, beta = 2 (unequal on purpose): int x^k (1-x)(1+x)^2 dx for
   ! k = 0..3 is 4/3, 4/15, 4/15, 4/35
      type(QUADRATURE_TYPE), INOUT :: q
      INT, IN :: n
      INT, INOUT :: n_failed
      INT :: k
      REAL :: exact,err
      call set_kind_(q,"gauss_jacobi")
      call set_n_pts_(q,n)
      call set_Gauss_Jacobi_alpha_(q,ONE)
      call set_Gauss_Jacobi_beta_(q,TWO)
      call set_quadrature_points_(q)
      err = ZERO
      do k = 0,3
         select case (k)
         case (0); exact = FOUR/THREE
         case (1); exact = FOUR/15.0d0
         case (2); exact = FOUR/15.0d0
         case (3); exact = FOUR/35.0d0
         end select
         err = max(err,abs(sum(q%wts*q%pts**k*(ONE-q%pts)*(ONE+q%pts)**2)-exact))
      end do
      call report_check("gauss_jacobi           ",err,TOL(10),n_failed)
   end subroutine

   subroutine check_open_trapezoid(q,n_failed)
   ! The open trapezoid omits the end points, so it is only right for an
   ! integrand that vanishes there. For f = x(1-x) on 11 points the
   ! Euler-Maclaurin error is exactly h^2/12 (f'(1)-f'(0)) = -h^2/6.
      type(QUADRATURE_TYPE), INOUT :: q
      INT, INOUT :: n_failed
      REAL :: exact,err,h
      call set_kind_(q,"open_extended_trapezoid")
      call set_n_pts_(q,11)
      call set_quadrature_points_(q)
      h = ONE/12.0d0
      exact = ONE/SIX-h*h/SIX
      err = abs(sum(q%wts*q%pts*(ONE-q%pts))-exact)
      call report_check("open_extended_trapezoid",err,TOL(12),n_failed)
   end subroutine

   subroutine check_newton_cotes(q,kind,degree,n_failed)
   ! int_0^1 x^k dx = 1/(k+1), exact up to "degree", on 11 points
      type(QUADRATURE_TYPE), INOUT :: q
      STR(len=*), IN :: kind
      INT, IN :: degree
      INT, INOUT :: n_failed
      INT :: k
      REAL :: exact,err
      call set_kind_(q,kind)
      call set_n_pts_(q,11)
      call set_quadrature_points_(q)
      err = ZERO
      do k = 0,degree
         exact = ONE/(k+1)
         err = max(err,abs(sum(q%wts*q%pts**k)-exact))
      end do
      call report_check(kind,err,TOL(12),n_failed)
   end subroutine


! Test functions

   pure function test_sin(x) result (res)
   ! Sine function for integrating
      REAL, IN :: x
      REAL :: res
      res = sin(x)
   end function

   pure function test_x2(x) result (res)
   ! x^2 function for integrating
      REAL, IN :: x
      REAL :: res
      res = x*x
   end function

   pure function test_x3(x) result (res)
   ! x^3 function for integrating
      REAL, IN :: x
      REAL :: res
      res = x*x*x
   end function

   pure function test_x4(x) result (res)
   ! x^4 function for integrating
      REAL, IN :: x
      REAL :: res
      res = x*x*x*x
   end function

end module
