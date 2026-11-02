!=====================================================================
!
! SPHERICAL_HARMONIC: 
!
! This module contians method for the calculation of spherical
! harmonics, and associated utility methods utilising them for shape
! description and reconstruction. 
! 
! Not all methods here are *specific* to spherical harmonics, but
! because of their (sometimes) mutial dependence they have been placed
! here.
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
!=====================================================================
module SPHERICAL_HARMONIC_MODULE

#  include "macros"

   include "spherical_harmonic.use"

   implicit none

   include "spherical_harmonic.int"

   INT :: n_points = 5810

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Create an object
      type(SPHERICAL_HARMONIC_TYPE), allocatable, OUT :: self
      allocate(self)
      call create_(self%factorials,0,70)
      call set_defaults_(self)
   end subroutine

   PURE subroutine destroy(self)
   ! Destroy an object
      type(SPHERICAL_HARMONIC_TYPE), allocatable, INOUT :: self
      if (NOT allocated(self)) return
      deallocate(self)
   end subroutine


!  ===========
!  Set methods
!  ===========

   pure subroutine set_defaults(self)
   ! Set default object
      type(SPHERICAL_HARMONIC_TYPE), INOUT :: self
      INT :: N
      self%n_points = 0
      self%nfact = 70  ! the highest number we have a factorial stored for
      self%recur = FALSE
      self%factorials(0) = 1
      self%factorials(1) = 1
      do N = 2,self%nfact
         self%factorials(N) = self%factorials(N-1)*N
      end do
   end subroutine

   PURE subroutine set(self,L,M)
   ! Set default object
      type(SPHERICAL_HARMONIC_TYPE), INOUT :: self
      INT, IN :: L,M
   ENSURE(L>=0,"SPHERICAL_HARMONIC:set ... L is negative!")
   ENSURE(is_in_range_(M,[-L,L]),"SPHERICAL_HARMONIC:set ... M quantum number not in [-L,+L] range")
      self%L = L
      self%M = M
   end subroutine

   PURE subroutine set_l(self,L)
   ! Set default object
      type(SPHERICAL_HARMONIC_TYPE), INOUT :: self
      INT, IN :: L
   ENSURE(L>=0,"SPHERICAL_HARMONIC:set_l ... L is negative!")
      self%L = L
   end subroutine

   PURE subroutine set_M(self,M)
   ! Set default object
      type(SPHERICAL_HARMONIC_TYPE), INOUT :: self
      INT, IN :: M
   ENSURE(is_in_range_(M,[-self%L,self%L]),"SPHERICAL_HARMONIC:set_M ... M quantum number not in [-L,+L] range")
      self%M = M
   end subroutine


!  =============================
!  Cartesian spherical harmonics
!  =============================

   PURE subroutine values_at(self,x,y,z,r2,res)
   ! Evaluate the 2*.L values of the *real* solid harmonics at 
   ! the set of points "(x,y,z)". Also input "r2" values.
   ! From: Helgaker et al Molecular ELectronic Structure p. 218.
      type(SPHERICAL_HARMONIC_TYPE), IN :: self
      VEC(REAL,:), IN :: x,y,z
      VEC(REAL,:), IN :: r2
      MAT(REAL,size(x),2*self%L+1), OUT :: res
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: S
      VEC(REAL,:), allocatable :: Slp
      VEC(REAL,:), allocatable :: Slm
      VEC(INT,:), allocatable :: std
      VEC(INT,:), allocatable :: stp
      VEC(INT,:), allocatable :: stm
      REAL :: fac
      INT :: n_pt,l,m,n
      INT :: del,top,bot
   ENSURE(size(x)==size(y),"SPHERICAL_HARMONIC:values_at ... wrong dimernsion, y")
   ENSURE(size(x)==size(z),"SPHERICAL_HARMONIC:values_at ... wrong dimernsion, z")

      ! Initialise
      n_pt = size(x)
      allocate(S(0:self%L))
      do l = 0,self%L
         allocate(S(l)%element(n_pt,2*l+1))
      end do

      ! Equation (6.4.70)
      S(0)%element(:,1) = ONE

      ! Recurrence 
      do l = 0,self%L-1

         ! Equations (6.4.71) & (6.4.72)
         del = delta_(l,0)
         top = 2*l+1
         fac = sqrt((2**del)*top/REALIFY(top+1))
         del = 1-del  ! <===

         ! Standard order of harmonics
         call create_(std,-L,L)  ! Std order for M = [ L  , L  ]
         std = into_std_S_order_(l)
         call create_(stp,-L-1,L+1)  ! Std order for M = [-L-1, L+1]
         stp = into_std_S_order_(l+1)
         n = std(l)
         Slp = fac*S(l)%element(:,n)
         n = std(-l)
         Slm = fac*S(l)%element(:,n)*del
         n = stp(l+1)
         S(l+1)%element(:,n) = x*Slp-y*Slm
         n = stp(-l-1)
         S(l+1)%element(:,n) = y*Slp+x*Slm

         ! First term in (6.4.73)
         do m = -l,l
            bot = (l+m+1)*(l-m+1)
            fac = top/sqrt(REALIFY(bot))
            n = stp(m)
            S(l+1)%element(:,n) = fac*z*S(l)%element(:,std(m))
         end do
         if (l>=1) then

         ! Needed for S_{l-1,m}
            call create_(stm,-L+1,L-1)
            stm = into_std_S_order_(l-1)

         ! Second term in (6.4.73)
         ! Note limits on m are correct
            do m = -l+1,l-1
               bot = (l+m+1)*(l-m+1)
               fac = sqrt((l+m)*(l-m)/REALIFY(bot))
               n = stp(m)
               S(l+1)%element(:,n) = S(l+1)%element(:,n)-fac*r2*S(l-1)%element(:,stm(m))
            end do
            call destroy_(stm)
         end if
         call destroy_(stp)
         call destroy_(std)
      end do

      ! Set result
      res = S(self%L)%element(:,:)
   end subroutine


!  =======
!  Methods
!  =======

   function wigner3j(self,j1,m1,j2,m2,j3,m3) result (res)
   ! wigner3j calculated from clebsch_gordan through the symmetry relation:
      type(SPHERICAL_HARMONIC_TYPE) :: self
   ! |j1 j2 j3|
   ! |        | == ((-1)^(j1 -j2 - m3))/sqrt(2*j3 + 1) * cg(j1,m1,j2,m2,j3,-m3)
   ! |m1 m2 m3|
   ! the triangle inequalities should hold for both
      INT, IN :: j1,j2,j3,m1,m2,m3
      REAL :: res
      REAL :: cg,tmp
      tmp = (-1)**(j1-j2-m3)/sqrt(TWO*j3+1)
      cg = clebsch_gordan_(self,j1,m1,j2,m2,j3,-m3)
      res = tmp*cg
   end function

   function clebsch_gordan(self,j1,m1,j2,m2,j,m) result (res)
   ! Calculate the Clebsch-Gordan coefficient using explicit racah
      type(SPHERICAL_HARMONIC_TYPE) :: self
   ! formula (if the factorial is not too big) but fail using the
   ! recurrence relation etc. if fac is too big recur is a flag to
   ! test whether or not the factorials used in Clebsch were too
   ! large. Note that this is NOT thread safe or parellisable using
   ! this object for future reference
      INT, IN :: j,m,j1,j2,m1,m2
      REAL :: res
      self%recur = FALSE
      if (j1==j2 AND m1==-m2 AND j==0 AND m==0) then
         res = (-1)**(j-m)/sqrt(TWO*j+1)
      else if (j1==m1 AND m1==0 AND j2==j AND m2==m) then
         res = ONE
      else
         res = clebsch_(self,j1*2,m1*2,j2*2,m2*2,j*2,m*2)
         if (self%recur) then
            res = clebsch_rec_(self,j1,m1,j2,m2,j,m)
         end if
      end if
   end function

   function clebsch(self,j1,m1,j2,m2,j,m) result (res)
   ! Calculation using Racah formula taken from "Angular Momentum",
      type(SPHERICAL_HARMONIC_TYPE) :: self
   ! D.M.Brink & G.R.Satchler, Oxford, 1968
      INT :: j1,m1,j2,m2,j,m
      REAL :: res
      INT :: j1nm1,jnj2pm1,j2pm2,jnj1nm2,j1pj2nj
      INT :: k,mink,maxk
      INT :: iphase
      REAL :: tmp
      BIN :: a,b,c,d,e,f
      res = ZERO
      a = abs(m1)>j1
      b = abs(m2)>j2
      c = abs(m)>j
      d = j1<0 OR j2<0 OR j<0
      e = abs(j1-j2)>j
      f = j>j1+j2
      if (a OR b OR c OR d OR e OR f OR (NOT ((m1+m2)==M))) return
      j1nm1 = (j1-m1)/2
      jnj2pm1 = (j-j2+m1)/2
      j2pm2 = (j2+m2)/2
      jnj1nm2 = (j-j1-m2)/2
      j1pj2nj = (j1+j2-j)/2

      ! check if evenness is valid i.e. j1 and m1 both even/odd
      a = (j1nm1*2==j1-m1)
      b = (j2pm2*2==j2+m2)
      c = (j1pj2nj*2==j1+j2-j)
      if (NOT (a AND b AND c)) then
         return
      end if
      mink = max(max(-jnj2pm1,-jnj1nm2),0)
      maxk = min(min(j1nm1,j2pm2),j1pj2nj)
      if (NOT ((mink/2)*2==mink)) then
         iphase = -1
      else
         iphase = 1
      end if
      do k = mink,maxk
         tmp = (fac_(self,j1nm1-k)*fac_(self,jnj2pm1+k)*fac_(self,j2pm2-k)*fac_(self,jnj1nm2+k)*fac_(self,k)*fac_(self,j1pj2nj-k))
         res = res+iphase/tmp
         iphase = -iphase
      end do
      if (mink>maxk) then
         res = ONE
      end if
      tmp = sqrt(ONE*fac_(self,j1pj2nj))
      tmp = tmp*sqrt(fac_(self,(j1+j-j2)/2))
      tmp = tmp*sqrt(fac_(self,(j2+j-j1)/2))
      tmp = tmp/sqrt(fac_(self,(j1+j2+j)/2+1))
      tmp = tmp*sqrt(ONE*(j+1))
      tmp = tmp*sqrt(fac_(self,(j1+m1)/2))
      tmp = tmp*sqrt(fac_(self,j1nm1))
      tmp = tmp*sqrt(fac_(self,j2pm2))
      tmp = tmp*sqrt(fac_(self,(j2-m2)/2))
      tmp = tmp*sqrt(fac_(self,(j+m)/2))
      tmp = tmp*sqrt(fac_(self,(j-m)/2))
      res = res*tmp
      return
   end function

   recursive function clebsch_rec(self,l1,m1,l2,m2,l,m) result (retval)
   ! Recursion relations
      type(SPHERICAL_HARMONIC_TYPE), IN :: self
      INT, IN :: l1,l2,m1,m2,l,m
      REAL :: retval
      BIN :: base_case,one_case,a,b,c,d,e
      REAL :: inv,left,right
      a = (m==(m1+m2))
      b = ((abs(l1-l2)<=l) AND (l<=(l1+l2)))
      c = ((-l1<=m1) AND (m1<=l1))
      d = ((-l2<=m2) AND (m2<=l2))
      e = ((-l<=m) AND (m<=l))
      one_case = (((l1==m1) AND (m1==0)) AND ((l2==l) AND (m2==m)))
      base_case = ((l1==l2) AND (m1==-m2) AND (l==m) AND m==0)
      if (a AND b AND c AND d AND e) then

      ! DO STUFF
      ! the recurrence relation is as follows:
      ! sqrt(l(l+1) - m(m - 1)) * <l1, m1, l2, m2|l, m> =
      ! sqrt(l1(l1 + 1) - m1(m1 - 1)) * <l1, m1 - 1, l2, m2|l, m - 1>
      ! + sqrt(l2(l2+1) - m2(m2 - 1)) * <l1, m1, l2, m2 - 2|l, m - 1>
         if (one_case) then
            retval = 1.0
         else if (base_case) then
            retval = ((-1)**(l1-m1))/(sqrt(2.0*l1+1.0))
         else
            left = sqrt(1.0*(l1*(l1+1)-m1*(m1-1)))
            right = sqrt(1.0*(l2*(l2+1)-m2*(m2-1)))
            inv = ONE/(sqrt(1.0*(l*(l+1)-m*(m-1))))
            retval = inv*(left*clebsch_rec_(self,l1,m1-1,l2,m2,l,m-1)+right*clebsch_rec_(self,l1,m1,l2,m2-1,l,m-1))
         end if
      else
         retval = 0.0
      end if
   end function

   function fac(self,l) result (res)
   ! Factorials
      type(SPHERICAL_HARMONIC_TYPE) :: self
      INT, IN :: l
      REAL :: res
   ENSURE(allocated(self%factorials),"SPHERICAL_HARMONIC:fac ... Factorials array not created?")
      res = ONE
      if (l<=1) return
      if (l>70) then
         WARN("SPHERICAL_HARMONIC:fac ... Factorials above 70! are not supported")
         self%recur = TRUE
         return
      end if
      res = self%factorials(l)
   end function


!  =======
!  Methods
!  =======

   recursive function z_harmonic(self,l,m,theta) result (res)
   ! Taken from appendix 1 in Burel & Henocq 1995
      type(SPHERICAL_HARMONIC_TYPE), IN :: self
      INT, IN :: l,m
      REAL, IN :: theta
      REAL :: res
      REAL :: fac,tmp1,tmp2
      if (m>l AND NOT m==0) then
         res = 0.0
      else if (l<0) then
         res = z_harmonic_(self,abs(l)-1,m,theta)
      else if (l==m) then
         fac = factorial_(l)
         res = ((-0.5)**l)/(ONE*fac)
         res = (sin(theta)**l)*res*sqrt(ONE*factorial_(2*l+1)/(4*PI))
      else if (m<0) then
         res = ((-ONE)**m)*z_harmonic_(self,l,-m,theta)
      else

            ! TMP1 and TMP2 have been checked and are working correctly
         tmp1 = sqrt(((2.0*l-1)*(2.0*l+1))/((l+m)*(l-m)))
         tmp2 = sqrt((ONE*(l+m-1)*(l-m-1))/((2*l-1)*(2*l-3)))
         res = tmp1*(cos(theta)*z_harmonic_(self,l-1,m,theta)-tmp2*z_harmonic_(self,l-2,m,theta))
      end if
   end function

   function sphericalHarmonicRec(self,l,m,theta,phi) result (res)
   ! Evaluate spherical harmonic
      type(SPHERICAL_HARMONIC_TYPE), IN :: self
      INT, IN :: l,m
      REAL, IN :: theta,phi
      CPX :: res
      CPX :: j
      j = cmplx(0,1)
      res = z_harmonic_(self,l,m,theta)*exp(j*m*phi)
   end function

   function is_star_domain(vertices,normals) result (res)
   ! Tests whether the region is non-reentrant and can be expanded in
   ! spherical harmonics
      MAT(REAL,:,:), allocatable, IN :: vertices
      MAT(REAL,:,:), allocatable, IN :: normals
      BIN :: res
      VEC(REAL,3) :: centre,u,v
      INT :: n_pt,i
      n_pt = max(size(vertices,1),1)
      do i = 1,3
         centre(i) = sum(vertices(i,:))/n_pt
      end do
      do i = 1,n_pt
         u = normals(:,i)
         call normalize_(u)
         v = vertices(:,i)-centre
         if (dot_product(v,u)<0.0) then
            res = FALSE
            return
         end if
      end do
      res = TRUE
   end function

   function sht(f,l_max) result (coefficients)
   ! untested, better interface for spherical harmonic transform
      INT, IN :: l_max
      CPX :: coefficients((l_max+1)*(l_max+1))
      interface
         function f(r,theta,phi) result (res)
            REAL, IN :: r,theta,phi
            REAL :: res
         end function
      end interface
      INT :: i,l,m
      type(LEBEDEV_TYPE), allocatable :: lebedev
      VEC(REAL,:), allocatable :: f_vals
      MAT(REAL,:,:), allocatable :: grid
      call create_(lebedev)
      call set_n_pts_(lebedev,5810)
      call cart2sph_(lebedev%pts,grid)
      call create_(f_vals,size(grid,1))
      do i = 1,size(grid,1)
         f_vals(i) = f(grid(i,1),grid(i,2),grid(i,3))
      end do
      do l = 0,l_max
         do m = -l,l
            coefficients(2*l+m+1) = sum(lebedev%wts(:)*f_vals(:)*conjg(sphericalHarmonicY(l,m,grid(:,1),grid(:,2))))
         end do
      end do
      call destroy_(lebedev)
      call destroy_(f_vals)
   end function

   function get_centre(points) result (centre)
   ! Get the mean of the list of "points"
      MAT(REAL,:,:), allocatable, IN :: points  ! Assumed to be a 3 x N matrix
      VEC(REAL,3) :: centre
      centre(1) = sum(points(1,:))/size(points,2)
      centre(2) = sum(points(2,:))/size(points,2)
      centre(3) = sum(points(3,:))/size(points,2)
   end function

   subroutine shift_origin(points)
   ! Shift the origin of the points
      MAT(REAL,:,:), allocatable, INOUT :: points
      VEC(REAL,3) :: centre
      INT :: N
      ! shift surface to be about origin
      centre = get_centre(points)
      do N = 1,size(points,2)
         points(:,N) = (points(:,N)-centre)
      end do
   end subroutine

   subroutine interpolate_surface_functions(grid,surface_sph,property,f1,f2)
   ! Assumes spherical_points is 3 x N and column 1 contains the function values
   ! to be interpolated
      MAT(REAL,:,:), allocatable, IN :: grid,surface_sph
      VEC(REAL,:), allocatable, IN :: property
      VEC(REAL,:), allocatable, OUT :: f1,f2
      INT :: N,X
      REAL :: minimum_dist,d
      VEC(REAL,3) :: p,tmp
      call create_(f1,size(grid,2))
      call create_(f2,size(grid,2))
      f1 = 0
      f2 = 0
      do N = 1,size(grid,2)
         minimum_dist = 100000000
         p = grid(:,N)
         do X = 1,size(surface_sph,2)
            tmp = surface_sph(:,X)
               ! calculate distance and update
            d = 1-(sin(tmp(2))*sin(p(2))*cos(tmp(3)-p(3))+cos(tmp(2))*cos(p(2)))
            if (d<minimum_dist) then
               f1(N) = surface_sph(1,X)
               f2(N) = property(X)
               minimum_dist = d
            end if
         end do
            ! Hacky test to see if we've got something sensible
         ENSURE(NOT minimum_dist>=100000000,"SPHERICAL_HARMONIC:interpolate_surface_functions ... Couldn't find any point even close???")
      end do
   end subroutine

   subroutine get_surface_decomposition(l_max,surface,property,coeff,pcoeff,radius)
   ! Get the spherical harmonic surface decomposition
      INT, IN :: l_max
      MAT(REAL,:,:), allocatable, INOUT :: surface
      VEC(REAL,:), allocatable, IN :: property
      VEC(CPX,:), allocatable, INOUT :: coeff,pcoeff
      REAL, OUT :: radius
      MAT(REAL,:,:), allocatable :: grid_points,grid_sph,surface_sph
      VEC(CPX,n_points) :: ylm
      INT :: n_pt,L,M,X,lm
      type(LEBEDEV_TYPE), allocatable :: lebedev
      VEC(REAL,:), allocatable :: cvals,fvals
      CPX :: val,cval
      X = (l_max+1)**2
      n_pt = size(surface,2)
      ! create structures
      call create_(coeff,X)
      call create_(pcoeff,X)
      call create_(lebedev)
      call shift_origin_(surface)
      call cart2sph_(surface,surface_sph)

      ! normalize the surface by its mean radius
      radius = sum(surface_sph(1,:))/n_pt
      surface_sph(1,:) = surface_sph(1,:)/radius
      call set_n_pts_(lebedev,n_points)
      call create_(grid_points,3,n_points)
      grid_points = transpose(lebedev%pts)
      coeff = 0
      pcoeff = 0
      call cart2sph_(grid_points,grid_sph)
      call interpolate_surface_functions_(grid_sph,surface_sph,property,fvals,cvals)

      ! we should now have an association of closest lebedev points
      ! coeff are indexed as follows: {c00,c1-1,c10,c11,c2-2,c2-1,c20,c21,c22... etc.}
      lm = 1
      do L = 0,l_max
         do M = -L,L
            val = cmplx(0,0)
            cval = cmplx(0,0)
               ! theta and phi have been checked and make sense
            ylm(:) = sphericalHarmonicY_(l,m,grid_sph(2,:),grid_sph(3,:))
            ylm(:) = conjg(ylm(:))*lebedev%wts(:)
            val = sum(fvals(:)*ylm(:))
            cval = sum(cvals(:)*ylm(:))
            coeff(lm) = 4*PI*val
            pcoeff(lm) = 4*PI*cval
            lm = lm+1
         end do
      end do
      call destroy_(grid_points)
      call destroy_(grid_sph)
      call destroy_(fvals)
      call destroy_(cvals)
      call destroy_(lebedev)
      call destroy_(surface_sph)
   end subroutine

   subroutine reconstruct_shape(coefficients,points,l_max)
   ! Reconstruct the shape from the spherical harmonic decomposition
      VEC(CPX,:), allocatable, IN :: coefficients
      MAT(REAL,:,:), allocatable, INOUT :: points
      INT, IN :: l_max
      INT :: X,N,a,L,lm,J
      REAL :: phi,theta
      CPX :: c,y
      call create_(points,91*181,3)
      X = 0
      do N = 0,90
         theta = N*PI/90
         do a = 0,180
            x = x+1
            phi = a*PI/90
            lm = 1
            c = 0
            do L = 0,l_max
               do J = -L,L
                  y = sphericalHarmonicY_(L,J,theta,phi)
                  c = c+(y*coefficients(lm))
                  lm = lm+1
               end do
            end do
            points(X,1) = abs(c)*sin(theta)*sin(phi)
            points(X,2) = abs(c)*sin(theta)*cos(phi)
            points(X,3) = abs(c)*cos(theta)
         end do
      end do
   end subroutine

   function pi_tensor(self,l1,l2,l,m,coefficients) result (sigma)
   ! Implementation of the so called 'PI tensor', providing
      type(SPHERICAL_HARMONIC_TYPE), IN :: self
      INT, IN :: l1,l2,l,m
      VEC(CPX,:), allocatable :: coefficients
      CPX :: sigma
      INT :: m1,m2,ref
      REAL :: cg
      CPX :: c1,c2
      sigma = cmplx(0.0,0.0)
      do m1 = -l1,l1
         do m2 = -l2,l2
            cg = clebsch_gordan_(self,l1,m1,l2,m2,l,m)
            ref = ((l1)**2+l1+m1+1)
            c1 = coefficients(ref)
            ref = (l2)**2+l2+m2+1
            c2 = coefficients(ref)
            sigma = sigma+cg*c1*c2
         end do
      end do
   end function

   subroutine make_invariants(coefficients,l_max,invariants)
   ! Make the spherical harmonic invariants
      VEC(CPX,:), allocatable, IN :: coefficients
      INT, IN :: l_max
      VEC(REAL,:), allocatable, OUT :: invariants
      INT :: lm,L,M
      INT :: num,invnum
      REAL :: sigma
      CPX :: c
      call create_(invariants,l_max+1)
      invariants = 0
      lm = 1
      ! keeping track of the number of N, P or Q invariants
      num = 0
      ! keeping track of the total number of invariants
      invnum = 1
      ! N-Invariants
      do L = 0,l_max
         if (num>=10) then
            exit
         end if
         sigma = 0
         do M = -L,L
            c = coefficients(lm)
            c = c*conjg(c)
            sigma = sigma+real(c)
            lm = lm+1
         end do
         invariants(invnum) = sqrt(sigma)
         num = num+1
         invnum = invnum+1
      end do
   end subroutine

   subroutine cart2sph(cartesian,spherical)
   ! expects a 3xN array and will create spherical_points as 3xN
      MAT(REAL,:,:), IN :: cartesian
      MAT(REAL,:,:), allocatable, OUT :: spherical
      INT :: N
      REAL :: x,y,z
      REAL :: r,theta,phi
      call create_(spherical,3,size(cartesian,2))
      do N = 1,size(cartesian,2)
         x = cartesian(1,N)
         y = cartesian(2,N)
         z = cartesian(3,N)
         r = sqrt(x**2+y**2+z**2)
         theta = acos(z/r)
         phi = atan2(y,x)
         spherical(1,N) = r
         spherical(2,N) = theta
         spherical(3,N) = phi
      end do
   end subroutine

   elemental function sphericalHarmonicY(l,m,theta,phi) result (res)
   ! calculates the spherical harmonics at l, m for angles theta, phi
   ! currently only up to l == 14 is supported by this method, 
   ! can utilise the recursive sphericalHarmonicRec if  higher order
   ! l values are needed
      INT, IN :: l,m
      REAL, IN :: theta,phi
      CPX :: res
      CPX :: j
      j = cmplx(0,1)
      if (l==0) then

            ! l -> 0
         if (m==0) then
            res = 1/(2.*Sqrt(PI))
         end if  ! l-> 0, end of M values
      else if (l==1) then

            ! l -> 1
         if (m==-1) then
            res = (Sqrt(3/(2.*PI))*sin(theta))/(2.*exp(j*phi))
         else if (m==0) then
            res = (Sqrt(3/PI)*cos(theta))/2.
         else if (m==1) then
            res = -(exp(j*phi)*Sqrt(3/(2.*PI))*sin(theta))/2.
         end if  ! l-> 1, end of M values
      else if (l==2) then

            ! l -> 2
         if (m==-2) then
            res = (Sqrt(15/(2.*PI))*sin(theta)**2)/(4.*exp(2*j*phi))
         else if (m==-1) then
            res = (Sqrt(15/(2.*PI))*cos(theta)*sin(theta))/(2.*exp(j*phi))
         else if (m==0) then
            res = (Sqrt(5/PI)*(-1+3*cos(theta)**2))/4.
         else if (m==1) then
            res = -(exp(j*phi)*Sqrt(15/(2.*PI))*cos(theta)*sin(theta))/2.
         else if (m==2) then
            res = (exp(2*j*phi)*Sqrt(15/(2.*PI))*sin(theta)**2)/4.
         end if  ! l-> 2, end of M values
      else if (l==3) then

            ! l -> 3
         if (m==-3) then
            res = (Sqrt(35/PI)*sin(theta)**3)/(8.*exp(3*j*phi))
         else if (m==-2) then
            res = (Sqrt(105/(2.*PI))*cos(theta)*sin(theta)**2)/(4.*exp(2*j*phi))
         else if (m==-1) then
            res = (Sqrt(21/PI)*(-1+5*cos(theta)**2)*sin(theta))/(8.*exp(j*phi))
         else if (m==0) then
            res = (Sqrt(7/PI)*(-3*cos(theta)+5*cos(theta)**3))/4.
         else if (m==1) then
            res = -(exp(j*phi)*Sqrt(21/PI)*(-1+5*cos(theta)**2)*sin(theta))/8.
         else if (m==2) then
            res = (exp(2*j*phi)*Sqrt(105/(2.*PI))*cos(theta)*sin(theta)**2)/4.
         else if (m==3) then
            res = -(exp(3*j*phi)*Sqrt(35/PI)*sin(theta)**3)/8.
         end if  ! l-> 3, end of M values
      else if (l==4) then

            ! l -> 4
         if (m==-4) then
            res = (3*Sqrt(35/(2.*PI))*sin(theta)**4)/(16.*exp(4*j*phi))
         else if (m==-3) then
            res = (3*Sqrt(35/PI)*cos(theta)*sin(theta)**3)/(8.*exp(3*j*phi))
         else if (m==-2) then
            res = (3*Sqrt(5/(2.*PI))*(-1+7*cos(theta)**2)*sin(theta)**2)/(8.*exp(2*j*phi))
         else if (m==-1) then
            res = (3*Sqrt(5/PI)*cos(theta)*(-3+7*cos(theta)**2)*sin(theta))/(8.*exp(j*phi))
         else if (m==0) then
            res = (3*(3-30*cos(theta)**2+35*cos(theta)**4))/(16.*Sqrt(PI))
         else if (m==1) then
            res = (-3*exp(j*phi)*Sqrt(5/PI)*cos(theta)*(-3+7*cos(theta)**2)*sin(theta))/8.
         else if (m==2) then
            res = (3*exp(2*j*phi)*Sqrt(5/(2.*PI))*(-1+7*cos(theta)**2)*sin(theta)**2)/8.
         else if (m==3) then
            res = (-3*exp(3*j*phi)*Sqrt(35/PI)*cos(theta)*sin(theta)**3)/8.
         else if (m==4) then
            res = (3*exp(4*j*phi)*Sqrt(35/(2.*PI))*sin(theta)**4)/16.
         end if  ! l-> 4, end of M values
      else if (l==5) then

            ! l -> 5
         if (m==-5) then
            res = (3*Sqrt(77/PI)*sin(theta)**5)/(32.*exp(5*j*phi))
         else if (m==-4) then
            res = (3*Sqrt(385/(2.*PI))*cos(theta)*sin(theta)**4)/(16.*exp(4*j*phi))
         else if (m==-3) then
            res = (Sqrt(385/PI)*(-1+9*cos(theta)**2)*sin(theta)**3)/(32.*exp(3*j*phi))
         else if (m==-2) then
            res = (Sqrt(1155/(2.*PI))*cos(theta)*(-1+3*cos(theta)**2)*sin(theta)**2)/(8.*exp(2*j*phi))
         else if (m==-1) then
            res = (Sqrt(165/(2.*PI))*(1-14*cos(theta)**2+21*cos(theta)**4)*sin(theta))/(16.*exp(j*phi))
         else if (m==0) then
            res = (Sqrt(11/PI)*(15*cos(theta)-70*cos(theta)**3+63*cos(theta)**5))/16.
         else if (m==1) then
            res = -(exp(j*phi)*Sqrt(165/(2.*PI))*(1-14*cos(theta)**2+21*cos(theta)**4)*sin(theta))/16.
         else if (m==2) then
            res = (exp(2*j*phi)*Sqrt(1155/(2.*PI))*cos(theta)*(-1+3*cos(theta)**2)*sin(theta)**2)/8.
         else if (m==3) then
            res = -(exp(3*j*phi)*Sqrt(385/PI)*(-1+9*cos(theta)**2)*sin(theta)**3)/32.
         else if (m==4) then
            res = (3*exp(4*j*phi)*Sqrt(385/(2.*PI))*cos(theta)*sin(theta)**4)/16.
         else if (m==5) then
            res = (-3*exp(5*j*phi)*Sqrt(77/PI)*sin(theta)**5)/32.
         end if  ! l-> 5, end of M values
      else if (l==6) then

            ! l -> 6
         if (m==-6) then
            res = (Sqrt(3003/PI)*sin(theta)**6)/(64.*exp(6*j*phi))
         else if (m==-5) then
            res = (3*Sqrt(1001/PI)*cos(theta)*sin(theta)**5)/(32.*exp(5*j*phi))
         else if (m==-4) then
            res = (3*Sqrt(91/(2.*PI))*(-1+11*cos(theta)**2)*sin(theta)**4)/(32.*exp(4*j*phi))
         else if (m==-3) then
            res = (Sqrt(1365/PI)*cos(theta)*(-3+11*cos(theta)**2)*sin(theta)**3)/(32.*exp(3*j*phi))
         else if (m==-2) then
            res = (Sqrt(1365/PI)*(1-18*cos(theta)**2+33*cos(theta)**4)*sin(theta)**2)/(64.*exp(2*j*phi))
         else if (m==-1) then
            res = (Sqrt(273/(2.*PI))*cos(theta)*(5-30*cos(theta)**2+33*cos(theta)**4)*sin(theta))/(16.*exp(j*phi))
         else if (m==0) then
            res = (Sqrt(13/PI)*(-5+105*cos(theta)**2-315*cos(theta)**4+231*cos(theta)**6))/32.
         else if (m==1) then
            res = -(exp(j*phi)*Sqrt(273/(2.*PI))*cos(theta)*(5-30*cos(theta)**2+33*cos(theta)**4)*sin(theta))/16.
         else if (m==2) then
            res = (exp(2*j*phi)*Sqrt(1365/PI)*(1-18*cos(theta)**2+33*cos(theta)**4)*sin(theta)**2)/64.
         else if (m==3) then
            res = -(exp(3*j*phi)*Sqrt(1365/PI)*cos(theta)*(-3+11*cos(theta)**2)*sin(theta)**3)/32.
         else if (m==4) then
            res = (3*exp(4*j*phi)*Sqrt(91/(2.*PI))*(-1+11*cos(theta)**2)*sin(theta)**4)/32.
         else if (m==5) then
            res = (-3*exp(5*j*phi)*Sqrt(1001/PI)*cos(theta)*sin(theta)**5)/32.
         else if (m==6) then
            res = (exp(6*j*phi)*Sqrt(3003/PI)*sin(theta)**6)/64.
         end if  ! l-> 6, end of M values
      else if (l==7) then

            ! l -> 7
         if (m==-7) then
            res = (3*Sqrt(715/(2.*PI))*sin(theta)**7)/(64.*exp(7*j*phi))
         else if (m==-6) then
            res = (3*Sqrt(5005/PI)*cos(theta)*sin(theta)**6)/(64.*exp(6*j*phi))
         else if (m==-5) then
            res = (3*Sqrt(385/(2.*PI))*(-1+13*cos(theta)**2)*sin(theta)**5)/(64.*exp(5*j*phi))
         else if (m==-4) then
            res = (3*Sqrt(385/(2.*PI))*cos(theta)*(-3+13*cos(theta)**2)*sin(theta)**4)/(32.*exp(4*j*phi))
         else if (m==-3) then
            res = (3*Sqrt(35/(2.*PI))*(3-66*cos(theta)**2+143*cos(theta)**4)*sin(theta)**3)/(64.*exp(3*j*phi))
         else if (m==-2) then
            res = (3*Sqrt(35/PI)*cos(theta)*(15-110*cos(theta)**2+143*cos(theta)**4)*sin(theta)**2)/(64.*exp(2*j*phi))
         else if (m==-1) then
            res = (Sqrt(105/(2.*PI))*(-5+135*cos(theta)**2-495*cos(theta)**4+429*cos(theta)**6)*sin(theta))/(64.*exp(j*phi))
         else if (m==0) then
            res = (Sqrt(15/PI)*(-35*cos(theta)+315*cos(theta)**3-693*cos(theta)**5+429*cos(theta)**7))/32.
         else if (m==1) then
            res = -(exp(j*phi)*Sqrt(105/(2.*PI))*(-5+135*cos(theta)**2-495*cos(theta)**4+429*cos(theta)**6)*sin(theta))/64.
         else if (m==2) then
            res = (3*exp(2*j*phi)*Sqrt(35/PI)*cos(theta)*(15-110*cos(theta)**2+143*cos(theta)**4)*sin(theta)**2)/64.
         else if (m==3) then
            res = (-3*exp(3*j*phi)*Sqrt(35/(2.*PI))*(3-66*cos(theta)**2+143*cos(theta)**4)*sin(theta)**3)/64.
         else if (m==4) then
            res = (3*exp(4*j*phi)*Sqrt(385/(2.*PI))*cos(theta)*(-3+13*cos(theta)**2)*sin(theta)**4)/32.
         else if (m==5) then
            res = (-3*exp(5*j*phi)*Sqrt(385/(2.*PI))*(-1+13*cos(theta)**2)*sin(theta)**5)/64.
         else if (m==6) then
            res = (3*exp(6*j*phi)*Sqrt(5005/PI)*cos(theta)*sin(theta)**6)/64.
         else if (m==7) then
            res = (-3*exp(7*j*phi)*Sqrt(715/(2.*PI))*sin(theta)**7)/64.
         end if  ! l-> 7, end of M values
      else if (l==8) then

            ! l -> 8
         if (m==-8) then
            res = (3*Sqrt(12155/(2.*PI))*sin(theta)**8)/(256.*exp(8*j*phi))
         else if (m==-7) then
            res = (3*Sqrt(12155/(2.*PI))*cos(theta)*sin(theta)**7)/(64.*exp(7*j*phi))
         else if (m==-6) then
            res = (Sqrt(7293/PI)*(-1+15*cos(theta)**2)*sin(theta)**6)/(128.*exp(6*j*phi))
         else if (m==-5) then
            res = (3*Sqrt(17017/(2.*PI))*cos(theta)*(-1+5*cos(theta)**2)*sin(theta)**5)/(64.*exp(5*j*phi))
         else if (m==-4) then
            res = (3*Sqrt(1309/(2.*PI))*(1-26*cos(theta)**2+65*cos(theta)**4)*sin(theta)**4)/(128.*exp(4*j*phi))
         else if (m==-3) then
            res = (Sqrt(19635/(2.*PI))*cos(theta)*(3-26*cos(theta)**2+39*cos(theta)**4)*sin(theta)**3)/(64.*exp(3*j*phi))
         else if (m==-2) then
            res = (3*Sqrt(595/PI)*(-1+33*cos(theta)**2-143*cos(theta)**4+143*cos(theta)**6)*sin(theta)**2)/(128.*exp(2*j*phi))
         else if (m==-1) then
            res = (3*Sqrt(17/(2.*PI))*cos(theta)*(-35+385*cos(theta)**2-1001*cos(theta)**4+715*cos(theta)**6)*sin(theta))/(64.*exp(j*phi))
         else if (m==0) then
            res = (Sqrt(17/PI)*(35-1260*cos(theta)**2+6930*cos(theta)**4-12012*cos(theta)**6+6435*cos(theta)**8))/256.
         else if (m==1) then
            res = (-3*exp(j*phi)*Sqrt(17/(2.*PI))*cos(theta)*(-35+385*cos(theta)**2-1001*cos(theta)**4+715*cos(theta)**6)*sin(theta))/64.
         else if (m==2) then
            res = (3*exp(2*j*phi)*Sqrt(595/PI)*(-1+33*cos(theta)**2-143*cos(theta)**4+143*cos(theta)**6)*sin(theta)**2)/128.
         else if (m==3) then
            res = -(exp(3*j*phi)*Sqrt(19635/(2.*PI))*cos(theta)*(3-26*cos(theta)**2+39*cos(theta)**4)*sin(theta)**3)/64.
         else if (m==4) then
            res = (3*exp(4*j*phi)*Sqrt(1309/(2.*PI))*(1-26*cos(theta)**2+65*cos(theta)**4)*sin(theta)**4)/128.
         else if (m==5) then
            res = (-3*exp(5*j*phi)*Sqrt(17017/(2.*PI))*cos(theta)*(-1+5*cos(theta)**2)*sin(theta)**5)/64.
         else if (m==6) then
            res = (exp(6*j*phi)*Sqrt(7293/PI)*(-1+15*cos(theta)**2)*sin(theta)**6)/128.
         else if (m==7) then
            res = (-3*exp(7*j*phi)*Sqrt(12155/(2.*PI))*cos(theta)*sin(theta)**7)/64.
         else if (m==8) then
            res = (3*exp(8*j*phi)*Sqrt(12155/(2.*PI))*sin(theta)**8)/256.
         end if  ! l-> 8, end of M values
      else if (l==9) then

            ! l -> 9
         if (m==-9) then
            res = (Sqrt(230945/PI)*sin(theta)**9)/(512.*exp(9*j*phi))
         else if (m==-8) then
            res = (3*Sqrt(230945/(2.*PI))*cos(theta)*sin(theta)**8)/(256.*exp(8*j*phi))
         else if (m==-7) then
            res = (3*Sqrt(13585/PI)*(-1+17*cos(theta)**2)*sin(theta)**7)/(512.*exp(7*j*phi))
         else if (m==-6) then
            res = (Sqrt(40755/PI)*cos(theta)*(-3+17*cos(theta)**2)*sin(theta)**6)/(128.*exp(6*j*phi))
         else if (m==-5) then
            res = (3*Sqrt(2717/PI)*(1-30*cos(theta)**2+85*cos(theta)**4)*sin(theta)**5)/(256.*exp(5*j*phi))
         else if (m==-4) then
            res = (3*Sqrt(95095/(2.*PI))*cos(theta)*(1-10*cos(theta)**2+17*cos(theta)**4)*sin(theta)**4)/(128.*exp(4*j*phi))
         else if (m==-3) then
            res = (Sqrt(21945/PI)*(-1+39*cos(theta)**2-195*cos(theta)**4+221*cos(theta)**6)*sin(theta)**3)/(256.*exp(3*j*phi))
         else if (m==-2) then
            res = (3*Sqrt(1045/PI)*cos(theta)*(-7+91*cos(theta)**2-273*cos(theta)**4+221*cos(theta)**6)*sin(theta)**2)/(128.*exp(2*j*phi))
         else if (m==-1) then
            res = (3*Sqrt(95/(2.*PI))*(7-308*cos(theta)**2+2002*cos(theta)**4-4004*cos(theta)**6+2431*cos(theta)**8)*sin(theta))/(256.*exp(j*phi))
         else if (m==0) then
            res = (Sqrt(19/PI)*(315*cos(theta)-4620*cos(theta)**3+18018*cos(theta)**5-25740*cos(theta)**7+12155*cos(theta)**9))/256.
         else if (m==1) then
            res = (-3*exp(j*phi)*Sqrt(95/(2.*PI))*(7-308*cos(theta)**2+2002*cos(theta)**4-4004*cos(theta)**6+2431*cos(theta)**8)*sin(theta))/256.
         else if (m==2) then
            res = (3*exp(2*j*phi)*Sqrt(1045/PI)*cos(theta)*(-7+91*cos(theta)**2-273*cos(theta)**4+221*cos(theta)**6)*sin(theta)**2)/128.
         else if (m==3) then
            res = -(exp(3*j*phi)*Sqrt(21945/PI)*(-1+39*cos(theta)**2-195*cos(theta)**4+221*cos(theta)**6)*sin(theta)**3)/256.
         else if (m==4) then
            res = (3*exp(4*j*phi)*Sqrt(95095/(2.*PI))*cos(theta)*(1-10*cos(theta)**2+17*cos(theta)**4)*sin(theta)**4)/128.
         else if (m==5) then
            res = (-3*exp(5*j*phi)*Sqrt(2717/PI)*(1-30*cos(theta)**2+85*cos(theta)**4)*sin(theta)**5)/256.
         else if (m==6) then
            res = (exp(6*j*phi)*Sqrt(40755/PI)*cos(theta)*(-3+17*cos(theta)**2)*sin(theta)**6)/128.
         else if (m==7) then
            res = (-3*exp(7*j*phi)*Sqrt(13585/PI)*(-1+17*cos(theta)**2)*sin(theta)**7)/512.
         else if (m==8) then
            res = (3*exp(8*j*phi)*Sqrt(230945/(2.*PI))*cos(theta)*sin(theta)**8)/256.
         else if (m==9) then
            res = -(exp(9*j*phi)*Sqrt(230945/PI)*sin(theta)**9)/512.
         end if  ! l-> 9, end of M values
      else if (l==10) then

            ! l -> 10
         if (m==-10) then
            res = (Sqrt(969969/PI)*sin(theta)**10)/(1024.*exp(10*j*phi))
         else if (m==-9) then
            res = (Sqrt(4849845/PI)*cos(theta)*sin(theta)**9)/(512.*exp(9*j*phi))
         else if (m==-8) then
            res = (Sqrt(255255/(2.*PI))*(-1+19*cos(theta)**2)*sin(theta)**8)/(512.*exp(8*j*phi))
         else if (m==-7) then
            res = (3*Sqrt(85085/PI)*cos(theta)*(-3+19*cos(theta)**2)*sin(theta)**7)/(512.*exp(7*j*phi))
         else if (m==-6) then
            res = (3*Sqrt(5005/PI)*(3-102*cos(theta)**2+323*cos(theta)**4)*sin(theta)**6)/(1024.*exp(6*j*phi))
         else if (m==-5) then
            res = (3*Sqrt(1001/PI)*cos(theta)*(15-170*cos(theta)**2+323*cos(theta)**4)*sin(theta)**5)/(256.*exp(5*j*phi))
         else if (m==-4) then
            res = (3*Sqrt(5005/(2.*PI))*(-1+45*cos(theta)**2-255*cos(theta)**4+323*cos(theta)**6)*sin(theta)**4)/(256.*exp(4*j*phi))
         else if (m==-3) then
            res = (3*Sqrt(5005/PI)*cos(theta)*(-7+105*cos(theta)**2-357*cos(theta)**4+323*cos(theta)**6)*sin(theta)**3)/(256.*exp(3*j*phi))
         else if (m==-2) then
            res = (3*Sqrt(385/(2.*PI))*(7-364*cos(theta)**2+2730*cos(theta)**4-6188*cos(theta)**6+4199*cos(theta)**8)*sin(theta)**2)/(512.*exp(2*j*phi))
         else if (m==-1) then
            res = (Sqrt(1155/(2.*PI))*cos(theta)*(63-1092*cos(theta)**2+4914*cos(theta)**4-7956*cos(theta)**6+4199*cos(theta)**8)*sin(theta))/(256.*exp(j*phi))
         else if (m==0) then
            res = (Sqrt(21/PI)*(-63+3465*cos(theta)**2-30030*cos(theta)**4+90090*cos(theta)**6-109395*cos(theta)**8+46189*cos(theta)**10))/512.
         else if (m==1) then
            res = -(exp(j*phi)*Sqrt(1155/(2.*PI))*cos(theta)*(63-1092*cos(theta)**2+4914*cos(theta)**4-7956*cos(theta)**6+4199*cos(theta)**8)*sin(theta))/256.
         else if (m==2) then
            res = (3*exp(2*j*phi)*Sqrt(385/(2.*PI))*(7-364*cos(theta)**2+2730*cos(theta)**4-6188*cos(theta)**6+4199*cos(theta)**8)*sin(theta)**2)/512.
         else if (m==3) then
            res = (-3*exp(3*j*phi)*Sqrt(5005/PI)*cos(theta)*(-7+105*cos(theta)**2-357*cos(theta)**4+323*cos(theta)**6)*sin(theta)**3)/256.
         else if (m==4) then
            res = (3*exp(4*j*phi)*Sqrt(5005/(2.*PI))*(-1+45*cos(theta)**2-255*cos(theta)**4+323*cos(theta)**6)*sin(theta)**4)/256.
         else if (m==5) then
            res = (-3*exp(5*j*phi)*Sqrt(1001/PI)*cos(theta)*(15-170*cos(theta)**2+323*cos(theta)**4)*sin(theta)**5)/256.
         else if (m==6) then
            res = (3*exp(6*j*phi)*Sqrt(5005/PI)*(3-102*cos(theta)**2+323*cos(theta)**4)*sin(theta)**6)/1024.
         else if (m==7) then
            res = (-3*exp(7*j*phi)*Sqrt(85085/PI)*cos(theta)*(-3+19*cos(theta)**2)*sin(theta)**7)/512.
         else if (m==8) then
            res = (exp(8*j*phi)*Sqrt(255255/(2.*PI))*(-1+19*cos(theta)**2)*sin(theta)**8)/512.
         else if (m==9) then
            res = -(exp(9*j*phi)*Sqrt(4849845/PI)*cos(theta)*sin(theta)**9)/512.
         else if (m==10) then
            res = (exp(10*j*phi)*Sqrt(969969/PI)*sin(theta)**10)/1024.
         end if  ! l-> 10, end of M values
      else if (l==11) then

            ! l -> 11
         if (m==-11) then
            res = (Sqrt(2028117/(2.*PI))*sin(theta)**11)/(1024.*exp(11*j*phi))
         else if (m==-10) then
            res = (Sqrt(22309287/PI)*cos(theta)*sin(theta)**10)/(1024.*exp(10*j*phi))
         else if (m==-9) then
            res = (Sqrt(1062347/(2.*PI))*(-1+21*cos(theta)**2)*sin(theta)**9)/(1024.*exp(9*j*phi))
         else if (m==-8) then
            res = (Sqrt(15935205/(2.*PI))*cos(theta)*(-1+7*cos(theta)**2)*sin(theta)**8)/(512.*exp(8*j*phi))
         else if (m==-7) then
            res = (Sqrt(838695/(2.*PI))*(1-38*cos(theta)**2+133*cos(theta)**4)*sin(theta)**7)/(1024.*exp(7*j*phi))
         else if (m==-6) then
            res = (Sqrt(167739/PI)*cos(theta)*(15-190*cos(theta)**2+399*cos(theta)**4)*sin(theta)**6)/(1024.*exp(6*j*phi))
         else if (m==-5) then
            res = (3*Sqrt(3289/(2.*PI))*(-5+255*cos(theta)**2-1615*cos(theta)**4+2261*cos(theta)**6)*sin(theta)**5)/(1024.*exp(5*j*phi))
         else if (m==-4) then
            res = (3*Sqrt(23023/(2.*PI))*cos(theta)*(-5+85*cos(theta)**2-323*cos(theta)**4+323*cos(theta)**6)*sin(theta)**4)/(256.*exp(4*j*phi))
         else if (m==-3) then
            res = (Sqrt(345345/PI)*(1-60*cos(theta)**2+510*cos(theta)**4-1292*cos(theta)**6+969*cos(theta)**8)*sin(theta)**3)/(1024.*exp(3*j*phi))
         else if (m==-2) then
            res = (Sqrt(49335/(2.*PI))*cos(theta)*(21-420*cos(theta)**2+2142*cos(theta)**4-3876*cos(theta)**6+2261*cos(theta)**8)*sin(theta)**2)/(512.*exp(2*j*phi))
         else if (m==-1) then
            res = (Sqrt(759/PI)*(-21+1365*cos(theta)**2-13650*cos(theta)**4+46410*cos(theta)**6-62985*cos(theta)**8+29393*cos(theta)**10)*sin(theta))/(1024.*exp(j*phi))
         else if (m==0) then
            res = (Sqrt(23/PI)*(-693*cos(theta)+15015*cos(theta)**3-90090*cos(theta)**5+218790*cos(theta)**7-230945*cos(theta)**9+88179*cos(theta)**11))/512.
         else if (m==1) then
            res = -(exp(j*phi)*Sqrt(759/PI)*(-21+1365*cos(theta)**2-13650*cos(theta)**4+46410*cos(theta)**6-62985*cos(theta)**8+29393*cos(theta)**10)*sin(theta))/1024.
         else if (m==2) then
            res = (exp(2*j*phi)*Sqrt(49335/(2.*PI))*cos(theta)*(21-420*cos(theta)**2+2142*cos(theta)**4-3876*cos(theta)**6+2261*cos(theta)**8)*sin(theta)**2)/512.
         else if (m==3) then
            res = -(exp(3*j*phi)*Sqrt(345345/PI)*(1-60*cos(theta)**2+510*cos(theta)**4-1292*cos(theta)**6+969*cos(theta)**8)*sin(theta)**3)/1024.
         else if (m==4) then
            res = (3*exp(4*j*phi)*Sqrt(23023/(2.*PI))*cos(theta)*(-5+85*cos(theta)**2-323*cos(theta)**4+323*cos(theta)**6)*sin(theta)**4)/256.
         else if (m==5) then
            res = (-3*exp(5*j*phi)*Sqrt(3289/(2.*PI))*(-5+255*cos(theta)**2-1615*cos(theta)**4+2261*cos(theta)**6)*sin(theta)**5)/1024.
         else if (m==6) then
            res = (exp(6*j*phi)*Sqrt(167739/PI)*cos(theta)*(15-190*cos(theta)**2+399*cos(theta)**4)*sin(theta)**6)/1024.
         else if (m==7) then
            res = -(exp(7*j*phi)*Sqrt(838695/(2.*PI))*(1-38*cos(theta)**2+133*cos(theta)**4)*sin(theta)**7)/1024.
         else if (m==8) then
            res = (exp(8*j*phi)*Sqrt(15935205/(2.*PI))*cos(theta)*(-1+7*cos(theta)**2)*sin(theta)**8)/512.
         else if (m==9) then
            res = -(exp(9*j*phi)*Sqrt(1062347/(2.*PI))*(-1+21*cos(theta)**2)*sin(theta)**9)/1024.
         else if (m==10) then
            res = (exp(10*j*phi)*Sqrt(22309287/PI)*cos(theta)*sin(theta)**10)/1024.
         else if (m==11) then
            res = -(exp(11*j*phi)*Sqrt(2028117/(2.*PI))*sin(theta)**11)/1024.
         end if  ! l-> 11, end of M values
      else if (l==12) then

            ! l -> 12
         if (m==-12) then
            res = (5*Sqrt(676039/PI)*sin(theta)**12)/(4096.*exp(12*j*phi))
         else if (m==-11) then
            res = (5*Sqrt(2028117/(2.*PI))*cos(theta)*sin(theta)**11)/(1024.*exp(11*j*phi))
         else if (m==-10) then
            res = (5*Sqrt(88179/PI)*(-1+23*cos(theta)**2)*sin(theta)**10)/(2048.*exp(10*j*phi))
         else if (m==-9) then
            res = (5*Sqrt(323323/(2.*PI))*cos(theta)*(-3+23*cos(theta)**2)*sin(theta)**9)/(1024.*exp(9*j*phi))
         else if (m==-8) then
            res = (5*Sqrt(138567/(2.*PI))*(1-42*cos(theta)**2+161*cos(theta)**4)*sin(theta)**8)/(2048.*exp(8*j*phi))
         else if (m==-7) then
            res = (5*Sqrt(138567/(2.*PI))*cos(theta)*(5-70*cos(theta)**2+161*cos(theta)**4)*sin(theta)**7)/(1024.*exp(7*j*phi))
         else if (m==-6) then
            res = (5*Sqrt(2431/PI)*(-5+285*cos(theta)**2-1995*cos(theta)**4+3059*cos(theta)**6)*sin(theta)**6)/(2048.*exp(6*j*phi))
         else if (m==-5) then
            res = (15*Sqrt(17017/(2.*PI))*cos(theta)*(-5+95*cos(theta)**2-399*cos(theta)**4+437*cos(theta)**6)*sin(theta)**5)/(1024.*exp(5*j*phi))
         else if (m==-4) then
            res = (15*Sqrt(1001/PI)*(5-340*cos(theta)**2+3230*cos(theta)**4-9044*cos(theta)**6+7429*cos(theta)**8)*sin(theta)**4)/(4096.*exp(4*j*phi))
         else if (m==-3) then
            res = (5*Sqrt(1001/PI)*cos(theta)*(45-1020*cos(theta)**2+5814*cos(theta)**4-11628*cos(theta)**6+7429*cos(theta)**8)*sin(theta)**3)/(1024.*exp(3*j*phi))
         else if (m==-2) then
            res = (5*Sqrt(3003/(2.*PI))*(-3+225*cos(theta)**2-2550*cos(theta)**4+9690*cos(theta)**6-14535*cos(theta)**8+7429*cos(theta)**10)*sin(theta)**2)/(1024.*exp(2*j*phi))
         else if (m==-1) then
            res = (5*Sqrt(39/PI)*cos(theta)*(-231+5775*cos(theta)**2-39270*cos(theta)**4+106590*cos(theta)**6-124355*cos(theta)**8+52003*cos(theta)**10)*sin(theta))/(1024.*exp(j*phi))
         else if (m==0) then
            res = (5*(231-18018*cos(theta)**2+225225*cos(theta)**4-1021020*cos(theta)**6+2078505*cos(theta)**8-1939938*cos(theta)**10+676039*cos(theta)**12))/(2048.*Sqrt(PI))
         else if (m==1) then
            res = (-5*exp(j*phi)*Sqrt(39/PI)*cos(theta)*(-231+5775*cos(theta)**2-39270*cos(theta)**4+106590*cos(theta)**6-124355*cos(theta)**8+52003*cos(theta)**10)*sin(theta))/1024.
         else if (m==2) then
            res = (5*exp(2*j*phi)*Sqrt(3003/(2.*PI))*(-3+225*cos(theta)**2-2550*cos(theta)**4+9690*cos(theta)**6-14535*cos(theta)**8+7429*cos(theta)**10)*sin(theta)**2)/1024.
         else if (m==3) then
            res = (-5*exp(3*j*phi)*Sqrt(1001/PI)*cos(theta)*(45-1020*cos(theta)**2+5814*cos(theta)**4-11628*cos(theta)**6+7429*cos(theta)**8)*sin(theta)**3)/1024.
         else if (m==4) then
            res = (15*exp(4*j*phi)*Sqrt(1001/PI)*(5-340*cos(theta)**2+3230*cos(theta)**4-9044*cos(theta)**6+7429*cos(theta)**8)*sin(theta)**4)/4096.
         else if (m==5) then
            res = (-15*exp(5*j*phi)*Sqrt(17017/(2.*PI))*cos(theta)*(-5+95*cos(theta)**2-399*cos(theta)**4+437*cos(theta)**6)*sin(theta)**5)/1024.
         else if (m==6) then
            res = (5*exp(6*j*phi)*Sqrt(2431/PI)*(-5+285*cos(theta)**2-1995*cos(theta)**4+3059*cos(theta)**6)*sin(theta)**6)/2048.
         else if (m==7) then
            res = (-5*exp(7*j*phi)*Sqrt(138567/(2.*PI))*cos(theta)*(5-70*cos(theta)**2+161*cos(theta)**4)*sin(theta)**7)/1024.
         else if (m==8) then
            res = (5*exp(8*j*phi)*Sqrt(138567/(2.*PI))*(1-42*cos(theta)**2+161*cos(theta)**4)*sin(theta)**8)/2048.
         else if (m==9) then
            res = (-5*exp(9*j*phi)*Sqrt(323323/(2.*PI))*cos(theta)*(-3+23*cos(theta)**2)*sin(theta)**9)/1024.
         else if (m==10) then
            res = (5*exp(10*j*phi)*Sqrt(88179/PI)*(-1+23*cos(theta)**2)*sin(theta)**10)/2048.
         else if (m==11) then
            res = (-5*exp(11*j*phi)*Sqrt(2028117/(2.*PI))*cos(theta)*sin(theta)**11)/1024.
         else if (m==12) then
            res = (5*exp(12*j*phi)*Sqrt(676039/PI)*sin(theta)**12)/4096.
         end if  ! l-> 12, end of M values
      else if (l==13) then

            ! l -> 13
         if (m==-13) then
            res = (15*Sqrt(156009/(2.*PI))*sin(theta)**13)/(4096.*exp(13*j*phi))
         else if (m==-12) then
            res = (15*Sqrt(2028117/PI)*cos(theta)*sin(theta)**12)/(4096.*exp(12*j*phi))
         else if (m==-11) then
            res = (3*Sqrt(2028117/(2.*PI))*(-1+25*cos(theta)**2)*sin(theta)**11)/(4096.*exp(11*j*phi))
         else if (m==-10) then
            res = (3*Sqrt(2028117/PI)*cos(theta)*(-3+25*cos(theta)**2)*sin(theta)**10)/(2048.*exp(10*j*phi))
         else if (m==-9) then
            res = (3*Sqrt(88179/PI)*(3-138*cos(theta)**2+575*cos(theta)**4)*sin(theta)**9)/(4096.*exp(9*j*phi))
         else if (m==-8) then
            res = (3*Sqrt(4849845/(2.*PI))*cos(theta)*(3-46*cos(theta)**2+115*cos(theta)**4)*sin(theta)**8)/(2048.*exp(8*j*phi))
         else if (m==-7) then
            res = (3*Sqrt(692835/PI)*(-1+63*cos(theta)**2-483*cos(theta)**4+805*cos(theta)**6)*sin(theta)**7)/(4096.*exp(7*j*phi))
         else if (m==-6) then
            res = (3*Sqrt(969969/PI)*cos(theta)*(-5+105*cos(theta)**2-483*cos(theta)**4+575*cos(theta)**6)*sin(theta)**6)/(2048.*exp(6*j*phi))
         else if (m==-5) then
            res = (3*Sqrt(51051/(2.*PI))*(5-380*cos(theta)**2+3990*cos(theta)**4-12236*cos(theta)**6+10925*cos(theta)**8)*sin(theta)**5)/(4096.*exp(5*j*phi))
         else if (m==-4) then
            res = (3*Sqrt(51051/PI)*cos(theta)*(45-1140*cos(theta)**2+7182*cos(theta)**4-15732*cos(theta)**6+10925*cos(theta)**8)*sin(theta)**4)/(4096.*exp(4*j*phi))
         else if (m==-3) then
            res = (3*Sqrt(15015/(2.*PI))*(-9+765*cos(theta)**2-9690*cos(theta)**4+40698*cos(theta)**6-66861*cos(theta)**8+37145*cos(theta)**10)*sin(theta)**3)/(4096.*exp(3*j*phi))
         else if (m==-2) then
            res = (3*Sqrt(1365/(2.*PI))*cos(theta)*(-99+2805*cos(theta)**2-21318*cos(theta)**4+63954*cos(theta)**6-81719*cos(theta)**8+37145*cos(theta)**10)*sin(theta)**2)/(1024.*exp(2*j*phi))
         else if (m==-1) then
            res = (3*Sqrt(273/(2.*PI))*(33-2970*cos(theta)**2+42075*cos(theta)**4-213180*cos(theta)**6+479655*cos(theta)**8-490314*cos(theta)**10+185725*cos(theta)**12)*sin(theta))/(2048.*exp(j*phi))
         else if (m==0) then
            res = (3*Sqrt(3/PI)*(3003*cos(theta)-90090*cos(theta)**3+765765*cos(theta)**5-2771340*cos(theta)**7+4849845*cos(theta)**9-4056234*cos(theta)**11+1300075*cos(theta)**13))/2048.
         else if (m==1) then
            res = (-3*exp(j*phi)*Sqrt(273/(2.*PI))*(33-2970*cos(theta)**2+42075*cos(theta)**4-213180*cos(theta)**6+479655*cos(theta)**8-490314*cos(theta)**10+185725*cos(theta)**12)*sin(theta))/2048.
         else if (m==2) then
            res = (3*exp(2*j*phi)*Sqrt(1365/(2.*PI))*cos(theta)*(-99+2805*cos(theta)**2-21318*cos(theta)**4+63954*cos(theta)**6-81719*cos(theta)**8+37145*cos(theta)**10)*sin(theta)**2)/1024.
         else if (m==3) then
            res = (-3*exp(3*j*phi)*Sqrt(15015/(2.*PI))*(-9+765*cos(theta)**2-9690*cos(theta)**4+40698*cos(theta)**6-66861*cos(theta)**8+37145*cos(theta)**10)*sin(theta)**3)/4096.
         else if (m==4) then
            res = (3*exp(4*j*phi)*Sqrt(51051/PI)*cos(theta)*(45-1140*cos(theta)**2+7182*cos(theta)**4-15732*cos(theta)**6+10925*cos(theta)**8)*sin(theta)**4)/4096.
         else if (m==5) then
            res = (-3*exp(5*j*phi)*Sqrt(51051/(2.*PI))*(5-380*cos(theta)**2+3990*cos(theta)**4-12236*cos(theta)**6+10925*cos(theta)**8)*sin(theta)**5)/4096.
         else if (m==6) then
            res = (3*exp(6*j*phi)*Sqrt(969969/PI)*cos(theta)*(-5+105*cos(theta)**2-483*cos(theta)**4+575*cos(theta)**6)*sin(theta)**6)/2048.
         else if (m==7) then
            res = (-3*exp(7*j*phi)*Sqrt(692835/PI)*(-1+63*cos(theta)**2-483*cos(theta)**4+805*cos(theta)**6)*sin(theta)**7)/4096.
         else if (m==8) then
            res = (3*exp(8*j*phi)*Sqrt(4849845/(2.*PI))*cos(theta)*(3-46*cos(theta)**2+115*cos(theta)**4)*sin(theta)**8)/2048.
         else if (m==9) then
            res = (-3*exp(9*j*phi)*Sqrt(88179/PI)*(3-138*cos(theta)**2+575*cos(theta)**4)*sin(theta)**9)/4096.
         else if (m==10) then
            res = (3*exp(10*j*phi)*Sqrt(2028117/PI)*cos(theta)*(-3+25*cos(theta)**2)*sin(theta)**10)/2048.
         else if (m==11) then
            res = (-3*exp(11*j*phi)*Sqrt(2028117/(2.*PI))*(-1+25*cos(theta)**2)*sin(theta)**11)/4096.
         else if (m==12) then
            res = (15*exp(12*j*phi)*Sqrt(2028117/PI)*cos(theta)*sin(theta)**12)/4096.
         else if (m==13) then
            res = (-15*exp(13*j*phi)*Sqrt(156009/(2.*PI))*sin(theta)**13)/4096.
         end if  ! l-> 13, end of M values
      else if (l==14) then

            ! l -> 14
         if (m==-14) then
            res = (15*Sqrt(646323/(2.*PI))*sin(theta)**14)/(8192.*exp(14*j*phi))
         else if (m==-13) then
            res = (15*Sqrt(4524261/(2.*PI))*cos(theta)*sin(theta)**13)/(4096.*exp(13*j*phi))
         else if (m==-12) then
            res = (5*Sqrt(1508087/PI)*(-1+27*cos(theta)**2)*sin(theta)**12)/(8192.*exp(12*j*phi))
         else if (m==-11) then
            res = (5*Sqrt(58815393/(2.*PI))*cos(theta)*(-1+9*cos(theta)**2)*sin(theta)**11)/(4096.*exp(11*j*phi))
         else if (m==-10) then
            res = (Sqrt(58815393/(2.*PI))*(1-50*cos(theta)**2+225*cos(theta)**4)*sin(theta)**10)/(8192.*exp(10*j*phi))
         else if (m==-9) then
            res = (Sqrt(98025655/PI)*cos(theta)*(3-50*cos(theta)**2+135*cos(theta)**4)*sin(theta)**9)/(4096.*exp(9*j*phi))
         else if (m==-8) then
            res = (Sqrt(12785955/(2.*PI))*(-1+69*cos(theta)**2-575*cos(theta)**4+1035*cos(theta)**6)*sin(theta)**8)/(4096.*exp(8*j*phi))
         else if (m==-7) then
            res = (Sqrt(20092215/PI)*cos(theta)*(-7+161*cos(theta)**2-805*cos(theta)**4+1035*cos(theta)**6)*sin(theta)**7)/(4096.*exp(7*j*phi))
         else if (m==-6) then
            res = (Sqrt(46881835/(2.*PI))*(1-84*cos(theta)**2+966*cos(theta)**4-3220*cos(theta)**6+3105*cos(theta)**8)*sin(theta)**6)/(8192.*exp(6*j*phi))
         else if (m==-5) then
            res = (3*Sqrt(9376367/(2.*PI))*cos(theta)*(5-140*cos(theta)**2+966*cos(theta)**4-2300*cos(theta)**6+1725*cos(theta)**8)*sin(theta)**5)/(4096.*exp(5*j*phi))
         else if (m==-4) then
            res = (3*Sqrt(2467465/PI)*(-1+95*cos(theta)**2-1330*cos(theta)**4+6118*cos(theta)**6-10925*cos(theta)**8+6555*cos(theta)**10)*sin(theta)**4)/(8192.*exp(4*j*phi))
         else if (m==-3) then
            res = (Sqrt(224315/(2.*PI))*cos(theta)*(-99+3135*cos(theta)**2-26334*cos(theta)**4+86526*cos(theta)**6-120175*cos(theta)**8+58995*cos(theta)**10)*sin(theta)**3)/(4096.*exp(3*j*phi))
         else if (m==-2) then
            res = (Sqrt(39585/(2.*PI))*(33-3366*cos(theta)**2+53295*cos(theta)**4-298452*cos(theta)**6+735471*cos(theta)**8-817190*cos(theta)**10+334305*cos(theta)**12)*sin(theta)**2)/(8192.*exp(2*j*phi))
         else if (m==-1) then
            res = (Sqrt(3045/(2.*PI))*cos(theta)*(429-14586*cos(theta)**2+138567*cos(theta)**4-554268*cos(theta)**6+1062347*cos(theta)**8-965770*cos(theta)**10+334305*cos(theta)**12)*sin(theta))/(2048.*exp(j*phi))
         else if (m==0) then
            res = (Sqrt(29/PI)*(-429+45045*cos(theta)**2-765765*cos(theta)**4+4849845*cos(theta)**6-14549535*cos(theta)**8+22309287*cos(theta)**10-16900975*cos(theta)**12+5014575*cos(theta)**14))/4096.
         else if (m==1) then
            res = -(exp(j*phi)*Sqrt(3045/(2.*PI))*cos(theta)*(429-14586*cos(theta)**2+138567*cos(theta)**4-554268*cos(theta)**6+1062347*cos(theta)**8-965770*cos(theta)**10+334305*cos(theta)**12)*sin(theta))/2048.
         else if (m==2) then
            res = (exp(2*j*phi)*Sqrt(39585/(2.*PI))*(33-3366*cos(theta)**2+53295*cos(theta)**4-298452*cos(theta)**6+735471*cos(theta)**8-817190*cos(theta)**10+334305*cos(theta)**12)*sin(theta)**2)/8192.
         else if (m==3) then
            res = -(exp(3*j*phi)*Sqrt(224315/(2.*PI))*cos(theta)*(-99+3135*cos(theta)**2-26334*cos(theta)**4+86526*cos(theta)**6-120175*cos(theta)**8+58995*cos(theta)**10)*sin(theta)**3)/4096.
         else if (m==4) then
            res = (3*exp(4*j*phi)*Sqrt(2467465/PI)*(-1+95*cos(theta)**2-1330*cos(theta)**4+6118*cos(theta)**6-10925*cos(theta)**8+6555*cos(theta)**10)*sin(theta)**4)/8192.
         else if (m==5) then
            res = (-3*exp(5*j*phi)*Sqrt(9376367/(2.*PI))*cos(theta)*(5-140*cos(theta)**2+966*cos(theta)**4-2300*cos(theta)**6+1725*cos(theta)**8)*sin(theta)**5)/4096.
         else if (m==6) then
            res = (exp(6*j*phi)*Sqrt(46881835/(2.*PI))*(1-84*cos(theta)**2+966*cos(theta)**4-3220*cos(theta)**6+3105*cos(theta)**8)*sin(theta)**6)/8192.
         else if (m==7) then
            res = -(exp(7*j*phi)*Sqrt(20092215/PI)*cos(theta)*(-7+161*cos(theta)**2-805*cos(theta)**4+1035*cos(theta)**6)*sin(theta)**7)/4096.
         else if (m==8) then
            res = (exp(8*j*phi)*Sqrt(12785955/(2.*PI))*(-1+69*cos(theta)**2-575*cos(theta)**4+1035*cos(theta)**6)*sin(theta)**8)/4096.
         else if (m==9) then
            res = -(exp(9*j*phi)*Sqrt(98025655/PI)*cos(theta)*(3-50*cos(theta)**2+135*cos(theta)**4)*sin(theta)**9)/4096.
         else if (m==10) then
            res = (exp(10*j*phi)*Sqrt(58815393/(2.*PI))*(1-50*cos(theta)**2+225*cos(theta)**4)*sin(theta)**10)/8192.
         else if (m==11) then
            res = (-5*exp(11*j*phi)*Sqrt(58815393/(2.*PI))*cos(theta)*(-1+9*cos(theta)**2)*sin(theta)**11)/4096.
         else if (m==12) then
            res = (5*exp(12*j*phi)*Sqrt(1508087/PI)*(-1+27*cos(theta)**2)*sin(theta)**12)/8192.
         else if (m==13) then
            res = (-15*exp(13*j*phi)*Sqrt(4524261/(2.*PI))*cos(theta)*sin(theta)**13)/4096.
         else if (m==14) then
            res = (15*exp(14*j*phi)*Sqrt(646323/(2.*PI))*sin(theta)**14)/8192.
         end if  ! l-> 14, end of M values
      end if  ! of all L values
   end function

end module
