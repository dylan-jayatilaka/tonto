!===================================================================
!
!  GAUSSIAN2 : Pairs of gaussian functions
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
!===================================================================
module GAUSSIAN2_MODULE

#  include "macros"

   include "gaussian2.use"

   implicit none

   include "gaussian2.int"

   REAL, private :: pi212 = 1.1283791670955d+0  !2*Pi^-1/2

!  The i-th point and weight for n-point Gauss-Hermite quadrature
   VEC(INT,9), private :: first_pt = [1,2,4,7,11,16,22,29,37]
   VEC(INT,9), private :: last_pt = [1,3,6,10,15,21,28,36,45]
   VEC(REAL,45), private :: gauss_hermite_pt
   data gauss_hermite_pt/                                                                      &
       0.000000000000000d+0,                                                                   & ! H_11
      -0.707106781186548d+0, 0.707106781186548d+0,                                             & ! H_12 H_22
      -1.224744871391590d+0, 0.000000000000000d+0, 1.224744871391590d+0,                       & ! H_13 H_23 H_33
      -1.650680123885780d+0,-0.524647623275290d+0, 0.524647623275290d+0, 1.650680123885780d+0, & ! H_*4
      -2.020182870456090d+0,-0.958572464613819d+0, 0.000000000000000d+0, 0.958572464613819d+0, & ! H_*5
      2.020182870456090d+0,                                                                   &
      -2.350604973674000d+0,-1.335849074014000d+0,-0.436077411928000d+0, 0.436077411928000d+0, & ! H_*6
      1.335849074014000d+0, 2.350604973674000d+0,                                             &
      -2.651961356835233d+0,-1.673551628767471d+0,-0.816287882858965d+0, 0.000000000000000d+0, & ! H_*7
      0.816287882858965d+0, 1.673551628767471d+0, 2.651961356835233d+0,                       &
      -2.930637420257244d+0,-1.981656756695843d+0,-1.157193712446780d+0,-0.381186990207322d+0, & ! H_*8
      0.381186990207322d+0, 1.157193712446780d+0, 1.981656756695843d+0, 2.930637420257244d+0, &
      -3.190993201781528d+0,-2.266580584531843d+0,-1.468553289216668d+0,-0.723551018752838d+0, & ! H_*9
      0.000000000000000d+0, 0.723551018752838d+0, 1.468553289216668d+0, 2.266580584531843d+0, &
       3.190993201781528d+0/
   VEC(REAL,45), private :: gauss_hermite_wt
   data gauss_hermite_wt/                                                                      &
       1.772453850905520d+0,                                                                   & ! W_11
      0.886226925452800d+0, 0.886226925452800d+0,                                             & ! W_12 W_22
      0.295408975150900d+0, 1.181635900604000d+0, 0.295408975150900d+0,                       & ! W_13 W_23 W_33
      8.131283544725000d-2, 8.049140900055000d-1, 8.049140900055000d-1, 8.131283544725000d-2, & ! W_*4
      1.995324205905000d-2, 3.936193231522000d-1, 9.453087204829000d-1, 3.936193231522000d-1, & ! W_*5
      1.995324205905000d-2,                                                                   &
       4.530009905509000d-3, 1.570673203229000d-1, 7.246295952244000d-1, 7.246295952244000d-1, & ! W_*6
      1.570673203229000d-1, 4.530009905509000d-3,                                             &
       9.717812450995000d-4, 5.451558281913000d-2, 4.256072526101000d-1, 8.102646175568000d-1, & ! W_*7
      4.256072526101000d-1, 5.451558281913000d-2, 9.717812450995000d-4,                       &
       1.996040722114000d-4, 1.707798300741000d-2, 2.078023258149000d-1, 6.611470125582000d-1, & ! W_*8
      6.611470125582000d-1, 2.078023258149000d-1, 1.707798300741000d-2, 1.996040722114000d-4, &
       3.960697726326000d-5, 4.943624275537000d-3, 8.847452739438000d-2, 4.326515590026000d-1, & ! W_*9
      7.202352156061000d-1, 4.326515590026000d-1, 8.847452739438000d-2, 4.943624275537000d-3, &
       3.960697726326000d-5/
   VEC(REAL,109), private :: gaussian_nucleus_eta
   data gaussian_nucleus_eta/                                                 &
      2.1248239171d+09, 1.1671538870d+09, 8.9266848806d+08, 7.8788802914d+08, &
      7.1178709563d+08, 6.8077502929d+08, 6.2865615725d+08, 5.8631436655d+08, &
      5.3546911034d+08, 5.2105715255d+08, 4.8349721509d+08, 4.7254270882d+08, &
      4.4335984491d+08, 4.3467748823d+08, 4.1117553148d+08, 4.0407992047d+08, &
      3.8463852873d+08, 3.5722217300d+08, 3.6228128110d+08, 3.5722217300d+08, &
      3.3451324570d+08, 3.2263108827d+08, 3.1181925878d+08, 3.0842641793d+08, &
      2.9881373610d+08, 2.9578406371d+08, 2.8716667270d+08, 2.8996391416d+08, &
      2.7665979354d+08, 2.7419021043d+08, 2.6267002737d+08, 2.5235613399d+08, &
      2.5042024280d+08, 2.4130163719d+08, 2.4305454351d+08, 2.3461213272d+08, &
      2.3301551109d+08, 2.2839354730d+08, 2.2690621893d+08, 2.2544431039d+08, &
      2.2120420724d+08, 2.1458511597d+08, 2.1458511597d+08, 2.0965270287d+08, &
      2.0846586999d+08, 2.0500935221d+08, 2.0389047621d+08, 1.9648639618d+08, &
      1.9548577691d+08, 1.9067718154d+08, 1.8975246242d+08, 1.8193056289d+08, &
      1.8444240538d+08, 1.8030529331d+08, 1.7950688281d+08, 1.7565009043d+08, &
      1.7490463170d+08, 1.7416744147d+08, 1.7343837120d+08, 1.7129844956d+08, &
      1.7060044589d+08, 1.6591550422d+08, 1.6527352089d+08, 1.6215880671d+08, &
      1.6155419421d+08, 1.5977529080d+08, 1.5977529080d+08, 1.5636673634d+08, &
      1.5581702004d+08, 1.5314257850d+08, 1.5262201512d+08, 1.5008710340d+08, &
      1.4959325643d+08, 1.4813689532d+08, 1.4671710337d+08, 1.4442808782d+08, &
      1.4398142103d+08, 1.4309883584d+08, 1.4223027307d+08, 1.4011788914d+08, &
      1.3888925203d+08, 1.3768840081d+08, 1.3729411599d+08, 1.3729411599d+08, &
      1.3690277000d+08, 1.3242350205d+08, 1.3206733609d+08, 1.3101367628d+08, &
      1.3066730974d+08, 1.2897067480d+08, 1.2930539512d+08, 1.2700881714d+08, &
      1.2733038109d+08, 1.2512299012d+08, 1.2543221826d+08, 1.2420711085d+08, &
      1.2420711085d+08, 1.2301273547d+08, 1.2271879740d+08, 1.2127611477d+08, &
      1.2099285491d+08, 1.2071131346d+08, 1.1987683191d+08, 1.2015331850d+08, &
      1.1987683191d+08, 1.1960199758d+08, 1.1987683191d+08, 1.1905722195d+08, &
      1.1878724932d+08/

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(GAUSSIAN2_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"GAUSSIAN2:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(GAUSSIAN2_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"GAUSSIAN2:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ===
!  Set
!  ===

   pure subroutine set_0(self,Ga,Gb)
   ! Set the gaussian pair object to "Ga" and "Gb"
      type(GAUSSIAN2_TYPE), INOUT :: self
      type(GAUSSIAN_TYPE), IN :: Ga,Gb
      self%a = Ga
      self%b = Gb
   end subroutine

   pure subroutine set_1(self,l_a,pos_a,ex_a,l_b,pos_b,ex_b)
   ! Set a gaussian object
      type(GAUSSIAN2_TYPE), INOUT :: self
      INT, IN :: l_a,l_b
      VEC(REAL,3), IN :: pos_a,pos_b
      REAL, IN :: ex_a,ex_b
      call set_l_(self%a,l_a)
      call set_position_(self%a,pos_a)
      call set_exponent_(self%a,ex_a)
      call set_l_(self%b,l_b)
      call set_position_(self%b,pos_b)
      call set_exponent_(self%b,ex_b)
   end subroutine

   pure subroutine set_2(self,ex_a,ex_b)
   ! Set the exponents of the pair
      type(GAUSSIAN2_TYPE), INOUT :: self
      REAL, optional, IN :: ex_a,ex_b
      if (present(ex_a)) call set_exponent_(self%a,ex_a)
      if (present(ex_b)) call set_exponent_(self%b,ex_b)
   end subroutine


!  ==================
!  Fourier transforms
!  ==================

   pure subroutine make_e_coeff(self,e,comp)
   ! Return the McMurchie-Davidson "e" coefficients for component "comp"
   ! (c) dylan jayatilaka, april 1995
   ! Checked using mathematica, may 1995
      type(GAUSSIAN2_TYPE), IN :: self
      MAT3(REAL,0:,0:,0:), OUT :: e
      INT, IN :: comp
      INT :: t,a,b,ap,am,bp,bm,abp,ab,abm,l_max,l_a,l_b
      REAL :: R,gamma,g1,g2,a1,b1,e000
      l_a = self%a%l
      l_b = self%b%l
      l_max = l_a+l_b
      R = self%a%position(comp)-self%b%position(comp)
      gamma = self%a%exponent+self%b%exponent
      g1 = ONE/gamma
      b1 = self%b%exponent*g1*R
      e000 = exp(-self%a%exponent*b1*R)
      e(0,0,0) = e000

      ! Return for e000
      if (l_max==0) return
      g2 = HALF*g1
      a1 = self%a%exponent*g1*R
      if (l_a>=1) then
         e(1,1,0) = g2*e000
         e(0,1,0) = -b1*e000
      end if
      if (l_b>=1) then
         e(1,0,1) = g2*e000
         e(0,0,1) = a1*e000
      end if

      ! Return for eXX1
      if (l_max==1) return

      ! Consider case b=0
      do a = 1,l_a-1
         ap = a+1
         am = a-1
         e(ap,ap,0) = g2*e(a,a,0)
         e(a,ap,0) = g2*e(am,a,0)-b1*e(a,a,0)
         e(0,ap,0) = -b1*e(0,a,0)+e(1,a,0)
         do t = 1,am
            e(t,ap,0) = g2*e(t-1,a,0)-b1*e(t,a,0)+(t+1)*e(t+1,a,0)
         end do
      end do

      ! Consider case a=0
      do b = 1,l_b-1
         bp = b+1
         bm = b-1
         e(bp,0,bp) = g2*e(b,0,b)
         e(b,0,bp) = g2*e(bm,0,b)+a1*e(b,0,b)
         e(0,0,bp) = a1*e(0,0,b)+e(1,0,b)
         do t = 1,bm
            e(t,0,bp) = g2*e(t-1,0,b)+a1*e(t,0,b)+(t+1)*e(t+1,0,b)
         end do
      end do

      ! Now do the rest, a/=0 & b/=0
      do b = 1,l_b
         do a = 0,l_a-1
            ab = a+b
            ap = a+1
            abp = ab+1
            abm = ab-1
            e(abp,ap,b) = g2*e(ab,a,b)
            e(ab,ap,b) = g2*e(abm,a,b)-b1*e(ab,a,b)
            e(0,ap,b) = -b1*e(0,a,b)+e(1,a,b)
            do t = 1,abm
               e(t,ap,b) = g2*e(t-1,a,b)-b1*e(t,a,b)+(t+1)*e(t+1,a,b)
            end do
         end do
      end do
   end subroutine

   pure subroutine make_e_coeff_derivative(self,e1,comp,e0)
   ! Return the derivative McMurchie-Davidson "e1" coefficients for
   ! component "comp" with repect to R = .a.position - .b.position. The zero order
   ! derivative E coefficients "e0" for the same component must be supplied.
   ! This routine is based on Helgaker & Taylor, TCA 83, p. 177 (1992)
   ! (c) dylan jayatilaka, may 1998
      type(GAUSSIAN2_TYPE), IN :: self
      INT, IN :: comp
      MAT3(REAL,0:,0:,0:), IN :: e0
      MAT3(REAL,0:,0:,0:), OUT :: e1  ! Generic e coefficient table
      INT :: t,a,b,ap,am,bp,bm,abp,ab,abm,max
      REAL :: R,gamma,g1,g2,a0,a1,b0,b1,e0000,e1000
      max = self%a%l+self%b%l
      R = self%a%position(comp)-self%b%position(comp)
      gamma = self%a%exponent+self%b%exponent
      g1 = 1/gamma
      b0 = self%b%exponent*g1
      b1 = b0*R
      e0000 = e0(0,0,0)
      e1000 = -2*self%a%exponent*b1*e0000
      e1(0,0,0) = e1000
      if (max==0) return  ! Return for e000
      a0 = self%a%exponent*g1
      a1 = a0*R
      g2 = HALF*g1
      if (self%a%l>=1) then
         e1(1,1,0) = g2*e1000
         e1(0,1,0) = -b1*e1000-b0*e0000
      end if
      if (self%b%l>=1) then
         e1(1,0,1) = g2*e1000
         e1(0,0,1) = a1*e1000+a0*e0000
      end if
      if (max==1) return  ! Return for eXX1
      do a = 1,self%a%l-1  ! Consider case b=0
         ap = a+1
         am = a-1
         e1(ap,ap,0) = g2*e1(a,a,0)
         e1(a,ap,0) = g2*e1(am,a,0)-b1*e1(a,a,0)-b0*e0(a,a,0)
         e1(0,ap,0) = -b1*e1(0,a,0)-b0*e0(0,a,0)+e1(1,a,0)
         do t = 1,am
            e1(t,ap,0) = g2*e1(t-1,a,0)-b1*e1(t,a,0)-b0*e0(t,a,0)+(t+1)*e1(t+1,a,0)
         end do
      end do
      do b = 1,self%b%l-1  ! Consider case a=0
         bp = b+1
         bm = b-1
         e1(bp,0,bp) = g2*e1(b,0,b)
         e1(b,0,bp) = g2*e1(bm,0,b)+a1*e1(b,0,b)+a0*e0(b,0,b)
         e1(0,0,bp) = a1*e1(0,0,b)+a0*e0(0,0,b)+e1(1,0,b)
         do t = 1,bm
            e1(t,0,bp) = g2*e1(t-1,0,b)+a1*e1(t,0,b)+a0*e0(t,0,b)+(t+1)*e1(t+1,0,b)
         end do
      end do
      do b = 1,self%b%l  ! Now do the rest, a/=0 & b/=0
         do a = 0,self%a%l-1
            ab = a+b
            ap = a+1
            abp = ab+1
            abm = ab-1
            e1(abp,ap,b) = g2*e1(ab,a,b)
            e1(ab,ap,b) = g2*e1(abm,a,b)-b1*e1(ab,a,b)-b0*e0(ab,a,b)
            e1(0,ap,b) = -b1*e1(0,a,b)-b0*e0(0,a,b)+e1(1,a,b)
            do t = 1,abm
               e1(t,ap,b) = g2*e1(t-1,a,b)-b1*e1(t,a,b)-b0*e0(t,a,b)+(t+1)*e1(t+1,a,b)
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_ft_component(self,res,comp,k_pts)
   ! Do the Fourier transform of component "comp" of the gaussian pair
   ! evaluated at the points "k_pts", without the prefactors.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT3(CPX,1:,0:,0:), OUT :: res
      INT, IN :: comp
      VEC(REAL,:), IN :: k_pts
      MAT3(REAL,:,:,:), allocatable :: e
      MAT(REAL,:,:), allocatable :: kp
      VEC(REAL,:), allocatable :: vr,vi
      INT :: a,b,t,k_max,t_max
      k_max = size(k_pts)
      t_max = self%a%l+self%b%l

      ! Make the E coefficients
      call create_(e,[0,t_max],[0,self%a%l],[0,self%b%l])
      call make_e_coeff_(self,e,comp)

      ! Define kp array containing powers k_pts(n)^t
      if (t_max>0) then
         call create_(kp,[1,k_max],[1,t_max])
         kp(:,1) = k_pts
         do t = 2,t_max
            kp(:,t) = kp(:,t-1)*k_pts
         end do
         do t = 2,t_max
            if (mod(t,4)<=1) cycle
            kp(:,t) = -kp(:,t)
         end do
      end if

      ! Real, imaginary parts of answer: (vr,vi)
      call create_(vr,k_max)
      call create_(vi,k_max)
      do a = 0,self%a%l
         do b = 0,self%b%l
            t_max = a+b

         ! Real part
            vr = e(0,a,b)
            do t = 2,t_max,2
               vr = vr+e(t,a,b)*kp(:,t)
            end do

         ! Imaginary part
            vi = ZERO
            do t = 1,t_max,2
               vi = vi+e(t,a,b)*kp(:,t)
            end do
            res(:,a,b) = COMPLEXIFY(vr,vi)
         end do
      end do
      call destroy_(vi); call destroy_(vr)
      call destroy_(kp); call destroy_(e)
   end subroutine

   PURE subroutine make_ft_dab_component(self,res,comp,k_pts,k_max)
   ! Do the Fourier transform of the derivative of component a minus b
   ! of a a pair of gaussians, for component "comp" of the guassian
   ! product pair, evaliated for the points "k_pts".
   ! Dimensions of res are [[1,size(k_pts)],[0,.a.l],[0,.b.l]].
      type(GAUSSIAN2_TYPE), IN :: self
      MAT3(CPX,1:,0:,0:), OUT :: res
      INT, IN :: comp
      VEC(REAL,:), IN :: k_pts
      INT, IN :: k_max
      REAL :: kn,fac
      INT :: a,b,t,t_max,n
      MAT3(REAL,:,:,:), allocatable :: e0,e1
      VEC(REAL,:), allocatable :: val  ! Local variables
      MAT(REAL,:,:), allocatable :: k1

      ! Define k1 array containing
      t_max = self%a%l+self%b%l
      call create_(k1,[1,k_max],[0,t_max+1])
      do n = 1,k_max  ! powers k^n of k_pts
         k1(n,0) = ONE
         kn = k_pts(n)
         do t = 1,t_max+1
            k1(n,t) = k1(n,t-1)*kn
         end do
      end do

      ! Now evaluate; use real arithmetic where possible
      call create_(val,k_max)
      call create_(e0,[0,self%a%l+self%b%l],[0,self%a%l],[0,self%b%l])
      call make_e_coeff_(self,e0,comp)
      call create_(e1,[0,self%a%l+self%b%l],[0,self%a%l],[0,self%b%l])
      call make_e_coeff_derivative_(self,e1,comp,e0)
      do a = 0,self%a%l
         do b = 0,self%b%l
            t_max = a+b
            val = ZERO  ! Do the real part first ...
            do t = 0,t_max,4
               val(:) = val(:)+e1(t,a,b)*k1(:,t)
            end do
            do t = 2,t_max,4
               val(:) = val(:)-e1(t,a,b)*k1(:,t)
            end do
            val = TWO*val
            res(:,a,b) = cmplx(val(:),ZERO,kind=CPX_KIND)
            val = ZERO  ! Now do imaginary part ...
            do t = 1,t_max,4
               val(:) = val(:)+e1(t,a,b)*k1(:,t)
            end do
            do t = 3,t_max,4
               val(:) = val(:)-e1(t,a,b)*k1(:,t)
            end do
            val = TWO*val
            res(:,a,b) = res(:,a,b)+cmplx(ZERO,val(:),kind=CPX_KIND)
         end do
      end do
      fac = (self%a%exponent-self%b%exponent)/(self%a%exponent+self%b%exponent)
      do a = 0,self%a%l
         do b = 0,self%b%l
            t_max = a+b
            val = ZERO  ! Do the imaginary part first ...
            do t = 0,t_max,4
               val(:) = val(:)+e0(t,a,b)*k1(:,t+1)
            end do
            do t = 2,t_max,4
               val(:) = val(:)-e0(t,a,b)*k1(:,t+1)
            end do
            val = fac*val
            res(:,a,b) = res(:,a,b)+cmplx(ZERO,val(:),kind=CPX_KIND)
            val = ZERO  ! Now do real part ...
            do t = 1,t_max,4
               val(:) = val(:)-e0(t,a,b)*k1(:,t+1)
            end do
            do t = 3,t_max,4
               val(:) = val(:)+e0(t,a,b)*k1(:,t+1)
            end do
            val = fac*val
            res(:,a,b) = res(:,a,b)+cmplx(val(:),ZERO,kind=CPX_KIND)
         end do
      end do

      ! Clean
      call destroy_(e1)
      call destroy_(e0)
      call destroy_(val)
      call destroy_(k1)
   end subroutine

   PURE subroutine make_ft_da_db_component(self,da,db,comp,k_pts)
   ! Do the Fourier transform of the derivative w.r.t. the position of component
   ! a, "da", and the derivative w.r.t. the position of component b, "db", of
   ! the gaussian pair, for component "comp" (either x, y, z), evaluated for the
   ! points "k_pts".  Dimensions of "da", "db" are ([1,size(k_pts)],[0,.a.l],[0,.b.l]).
      type(GAUSSIAN2_TYPE), IN :: self
      MAT3(CPX,1:,0:,0:), OUT :: da,db
      INT, IN :: comp
      VEC(REAL,:), IN :: k_pts
      REAL :: kn,fac_a,fac_b
      INT :: a,b,t,t_max,k_max,n
      MAT3(REAL,:,:,:), allocatable :: e0,e1
      VEC(REAL,:), allocatable :: val  ! Local variables
      MAT(REAL,:,:), allocatable :: k1
      t_max = self%a%l+self%b%l
      k_max = size(da,1)
      call create_(k1,[1,k_max],[0,t_max+1])  ! Define k1 array containing
      do n = 1,k_max  ! powers k^n of k_pts
         k1(n,0) = ONE
         kn = k_pts(n)
         do t = 1,t_max+1
            k1(n,t) = k1(n,t-1)*kn
         end do
      end do

      ! Now evaluate; use real arithmetic where possible
      call create_(val,k_max)
      call create_(e0,[0,self%a%l+self%b%l],[0,self%a%l],[0,self%b%l])
      call make_e_coeff_(self,e0,comp)
      call create_(e1,[0,self%a%l+self%b%l],[0,self%a%l],[0,self%b%l])
      call make_e_coeff_derivative_(self,e1,comp,e0)
      do a = 0,self%a%l
         do b = 0,self%b%l
            t_max = a+b
            val = ZERO  ! Do the real part first ...
            do t = 0,t_max,4
               val(:) = val(:)+e1(t,a,b)*k1(:,t)
            end do
            do t = 2,t_max,4
               val(:) = val(:)-e1(t,a,b)*k1(:,t)
            end do
            da(:,a,b) = COMPLEXIFY(val(:),ZERO)
            val = ZERO  ! Now do imaginary part ...
            do t = 1,t_max,4
               val(:) = val(:)+e1(t,a,b)*k1(:,t)
            end do
            do t = 3,t_max,4
               val(:) = val(:)-e1(t,a,b)*k1(:,t)
            end do
            da(:,a,b) = da(:,a,b)+COMPLEXIFY(ZERO,val(:))
            db(:,a,b) = -da(:,a,b)
         end do
      end do
      fac_a = self%a%exponent/(self%a%exponent+self%b%exponent)
      fac_b = self%b%exponent/(self%a%exponent+self%b%exponent)
      do a = 0,self%a%l
         do b = 0,self%b%l
            t_max = a+b
            val = ZERO  ! Do the imaginary part first ...
            do t = 0,t_max,4
               val(:) = val(:)+e0(t,a,b)*k1(:,t+1)
            end do
            do t = 2,t_max,4
               val(:) = val(:)-e0(t,a,b)*k1(:,t+1)
            end do
            da(:,a,b) = da(:,a,b)+fac_a*COMPLEXIFY(ZERO,val(:))
            db(:,a,b) = db(:,a,b)+fac_b*COMPLEXIFY(ZERO,val(:))
            val = ZERO  ! Now do real part ...
            do t = 1,t_max,4
               val(:) = val(:)-e0(t,a,b)*k1(:,t+1)
            end do
            do t = 3,t_max,4
               val(:) = val(:)+e0(t,a,b)*k1(:,t+1)
            end do
            da(:,a,b) = da(:,a,b)+fac_a*COMPLEXIFY(val(:),ZERO)
            db(:,a,b) = db(:,a,b)+fac_b*COMPLEXIFY(val(:),ZERO)
         end do
      end do
      call destroy_(e1)
      call destroy_(e0)
      call destroy_(val)
      call destroy_(k1)
   end subroutine

   PURE subroutine make_ft_dab(self,res,ft)
   ! Form the Fourier transform of the derivative of the product
   ! of two gaussians w.r.t. coordinates in shell b, from the zeroth
   ! derivative Fourier transform.  Dimensions of res are
   ! [[1,size(k_pts)],[0,l_a],[0,l_b]].
      type(GAUSSIAN2_TYPE), IN :: self
      MAT3(CPX,1:,0:,0:), OUT :: res
      MAT3(CPX,1:,0:,0:), IN :: ft
      INT :: a,b,l_a,l_b
      l_a = ubound(ft,2)-1
      l_b = ubound(ft,3)-1
      res = ZERO
      do a = 0,l_a
         do b = 0,l_b
            res(:,a,b) = res(:,a,b)-TWO*self%b%exponent*ft(:,a,b+1)
            res(:,a,b) = res(:,a,b)+TWO*self%a%exponent*ft(:,a+1,b)
            if (b>0) res(:,a,b) = res(:,a,b)+b*ft(:,a,b-1)
            if (a>0) res(:,a,b) = res(:,a,b)-a*ft(:,a-1,b)
         end do
      end do
   end subroutine

   PURE subroutine make_FT(self,ft,k_pts)
   ! Calculates the Fourier transform for a product of two primitive
   ! gaussian functions, evaluated at a series of k points "k_pts".
   ! This routine is based on formula (9) in Jayatilaka, CPL 230, p. 228 (1995)
      type(GAUSSIAN2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: ft
      MAT(REAL,:,:), IN :: k_pts
   ENSURE(size(ft,1)==size(k_pts,1),"GAUSSIAN2:make_FT ... inconsistent first dimension")
   ENSURE(size(k_pts,2)==3,"GAUSSIAN2:make_FT ... second dimension of k_pts must be 3")
      select case (self%a%l+self%b%l)
      case (0); call make_ft_00_(self,ft(:,1,1),k_pts)
      case default; call make_ft_XX_(self,ft,k_pts)
      end select
   end subroutine

   PURE subroutine make_ft_00(self,ft,k_pts)
   ! Calculates the Fourier transform for a product of two primitive
   ! gaussian functions, evaluated at a series of k points "k_pts".
   ! This routine is based on formula (9) in Jayatilaka, CPL 230, p. 228 (1995)
   ! Specialised routine for .a.l=0 and .b.l=0.
      type(GAUSSIAN2_TYPE), IN :: self
      VEC(CPX,:), OUT :: ft
      MAT(REAL,:,:), IN :: k_pts
      VEC(REAL,3) :: pos
      VEC(REAL,:), allocatable :: k_x,k_y,k_z
      REAL :: g1_exa,g1_exb,e00
      REAL :: pifac,gamma,g1,g4,th,kk
      REAL :: k1,k2,k3,P1,P2,P3,PI_on_gamma
      INT :: k
      CPX :: I

      ! Constants
      I = COMPLEXIFY(ZERO,ONE)
      gamma = self%a%exponent+self%b%exponent
      g1 = ONE/gamma
      g4 = QUARTER*g1
      PI_on_gamma = PI*g1
      pifac = sqrt(PI_on_gamma)*PI_on_gamma

      ! Gaussian pair center
      g1_exa = g1*self%a%exponent
      g1_exb = g1*self%b%exponent
      P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
      P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
      P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)

      ! Reciprocal lattice K points
      k_x = k_pts(:,1)
      k_y = k_pts(:,2)
      k_z = k_pts(:,3)

      ! E coefficient
      pos = self%a%position-self%b%position
      e00 = exp(-self%a%exponent*self%b%exponent*dot_product(pos,pos)/gamma)
      pifac = pifac*e00

      ! Do the Fourier transform
      do k = 1,size(k_x)
         k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
         th = k1*P1+k2*P2+k3*P3
         kk = k1*k1+k2*k2+k3*k3
         ft(k) = pifac*(cos(th)+I*sin(th))*exp(-kk*g4)
      end do

      ! Clean
      call destroy_(k_z)
      call destroy_(k_y)
      call destroy_(k_x)
   end subroutine

   PURE subroutine make_ft_XX(self,ft,k_pts)
   ! Calculates the Fourier transform for a product of two primitive
   ! gaussian functions, evaluated at a series of k points "k_pts".
   ! This routine is based on formula (9) in Jayatilaka, CPL 230, p. 228 (1995)
      type(GAUSSIAN2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: ft
      MAT(REAL,:,:), target, IN :: k_pts
      MAT3(CPX,:,:,:), allocatable, target :: ft_x,ft_y,ft_z
      VEC(CPX,:), PTR :: ft_xb,ft_yb,ft_zb  ! pointers
      MAT(CPX,:,:), PTR :: ft_xbx,ft_yby,ft_zbz  ! pointers
      VEC(CPX,:), allocatable :: prefac
      REAL :: g1_exa,g1_exb
      REAL :: pifac,gamma,g1,g4,dot1,dot2
      REAL :: k1,k2,k3,P1,P2,P3,PI_on_gamma
      INT :: k_max,k
      VEC(REAL,:), allocatable :: k_x,k_y,k_z
      VEC(INT,:), allocatable :: ax,ay,az
      VEC(INT,:), allocatable :: bx,by,bz
      INT :: a,xa,ya,za,fa,la
      INT :: b,xb,yb,zb,fb,lb
      INT :: bxlast,bylast,bzlast
      k_max = size(k_pts,1)
      call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
      call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
      call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
      call create_(prefac,k_max)
      gamma = self%a%exponent+self%b%exponent
      g1 = ONE/gamma
      g4 = QUARTER*g1
      PI_on_gamma = PI*g1
      pifac = sqrt(PI_on_gamma)*PI_on_gamma
      g1_exa = g1*self%a%exponent
      g1_exb = g1*self%b%exponent
      P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
      P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
      P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
      k_x = k_pts(:,1)
      k_y = k_pts(:,2)
      k_z = k_pts(:,3)

      ! Get the Fourier transforms of each component.
      call make_ft_component_(self,ft_x,1,k_x)
      call make_ft_component_(self,ft_y,2,k_y)
      call make_ft_component_(self,ft_z,3,k_z)
      do k = 1,k_max
         k1 = k_x(k)
         k2 = k_y(k)
         k3 = k_z(k)
         dot1 = k1*P1+k2*P2+k3*P3
         dot2 = -g4*(k1*k1+k2*k2+k3*k3)
         prefac(k) = pifac*exp(COMPLEXIFY(dot2,dot1))
      end do

      ! Loop over shell pairs
      select case (self%a%l)

      case (0)
         select case (self%b%l)

         ! <S|S>
         case (0)
            ft(:,1,1) = ft_x(:,0,0)*ft_y(:,0,0)*ft_z(:,0,0)*prefac

         ! <S|P>
         case (1)
            ft(:,1,1) = ft_x(:,0,1)*ft_y(:,0,0)*ft_z(:,0,0)*prefac
            ft(:,1,2) = ft_x(:,0,0)*ft_y(:,0,1)*ft_z(:,0,0)*prefac
            ft(:,1,3) = ft_x(:,0,0)*ft_y(:,0,0)*ft_z(:,0,1)*prefac

         ! <S|X>
         case default
            do b = 0,self%b%l  ! incorporate prefac into ft_z
               ft_z(:,0,b) = ft_z(:,0,b)*prefac
            end do
            fb = self%b%first_gaussian
            lb = self%b%last_gaussian
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            bxlast = bx(1)
            bylast = by(1)
            bzlast = bz(1)
            ft_xb => ft_x(:,0,bxlast)
            ft_yb => ft_y(:,0,bylast)
            ft_zb => ft_z(:,0,bzlast)
            do b = 1,self%b%n_comp  ! Loop over all elements and multiply prefactor
               xb = bx(b)
               yb = by(b)
               zb = bz(b)
               if (bxlast/=xb) then
                  ft_xb => ft_x(:,0,xb)
                  bxlast = xb
               end if
               if (bylast/=yb) then
                  ft_yb => ft_y(:,0,yb)
                  bylast = yb
               end if
               if (bzlast/=zb) then
                  ft_zb => ft_z(:,0,zb)
                  bzlast = zb
               end if
               ft(:,1,b) = ft_xb(:)*ft_yb(:)*ft_zb(:)
            end do
         end select

      case (1)
         select case (self%b%l)

         ! <P|S>
         case (0)
            ft(:,1,1) = ft_x(:,1,0)*ft_y(:,0,0)*ft_z(:,0,0)*prefac
            ft(:,2,1) = ft_x(:,0,0)*ft_y(:,1,0)*ft_z(:,0,0)*prefac
            ft(:,3,1) = ft_x(:,0,0)*ft_y(:,0,0)*ft_z(:,1,0)*prefac

         ! <P|P>
         case (1)
            ft_z(:,0,0) = ft_z(:,0,0)*prefac
            ft(:,1,1) = ft_x(:,1,1)*ft_y(:,0,0)*ft_z(:,0,0)
            ft(:,2,1) = ft_x(:,0,1)*ft_y(:,1,0)*ft_z(:,0,0)
            ft(:,3,1) = ft_x(:,0,1)*ft_y(:,0,0)*ft_z(:,1,0)*prefac
            ft(:,1,2) = ft_x(:,1,0)*ft_y(:,0,1)*ft_z(:,0,0)
            ft(:,2,2) = ft_x(:,0,0)*ft_y(:,1,1)*ft_z(:,0,0)
            ! don't change the order.
            ft_x(:,0,0) = ft_x(:,0,0)*prefac
            ft(:,3,2) = ft_x(:,0,0)*ft_y(:,0,1)*ft_z(:,1,0)
            ft(:,1,3) = ft_x(:,1,0)*ft_y(:,0,0)*ft_z(:,0,1)*prefac
            ft(:,2,3) = ft_x(:,0,0)*ft_y(:,1,0)*ft_z(:,0,1)
            ft(:,3,3) = ft_x(:,0,0)*ft_y(:,0,0)*ft_z(:,1,1)

         ! <P|X>
         case default
            fb = self%b%first_gaussian
            lb = self%b%last_gaussian
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            do b = 0,self%b%l
               ft_z(:,0,b) = ft_z(:,0,b)*prefac
            end do
            do b = 1,self%b%n_comp  ! Loop over all elements and multiply prefactor
               xb = bx(b)
               yb = by(b)
               zb = bz(b)
               ft_xb => ft_x(:,0,xb)
               ft_yb => ft_y(:,0,yb)
               ft_zb => ft_z(:,0,zb)
               ft(:,1,b) = ft_x(:,1,xb)*ft_yb*ft_zb
               ft(:,2,b) = ft_xb*ft_y(:,1,yb)*ft_zb
               ft(:,3,b) = ft_xb*ft_yb*ft_z(:,1,zb)*prefac(:)
            end do
         end select

      case default
         select case (self%b%l)

         ! <X|S>
         case (0)
            do a = 0,self%a%l
               ft_z(:,a,0) = ft_z(:,a,0)*prefac(:)
            end do
            fa = self%a%first_gaussian
            la = self%a%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            do a = 1,self%a%n_comp
               xa = ax(a)
               ya = ay(a)
               za = az(a)
               ft(:,a,1) = ft_x(:,xa,0)*ft_y(:,ya,0)*ft_z(:,za,0)
            end do

         ! <X|P>
         case (1)
            do a = 0,self%a%l  ! incorporate prefac into ft_z
               ft_z(:,a,0) = ft_z(:,a,0)*prefac(:)
               ft_z(:,a,1) = ft_z(:,a,1)*prefac(:)
            end do
            fa = self%a%first_gaussian
            la = self%a%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            do a = 1,self%a%n_comp
               xa = ax(a)
               ya = ay(a)
               za = az(a)
               ft(:,a,1) = ft_x(:,xa,1)*ft_y(:,ya,0)*ft_z(:,za,0)
               ft(:,a,2) = ft_x(:,xa,0)*ft_y(:,ya,1)*ft_z(:,za,0)
               ft(:,a,3) = ft_x(:,xa,0)*ft_y(:,ya,0)*ft_z(:,za,1)
            end do

         ! <X|X>
         ! This is the general routine.  It uses reduced
         ! multiplication, and pointers to minimise array finding.
         case default
            do b = 0,self%b%l  ! incorporate prefac into ft_z
               do a = 0,self%a%l
                  ft_z(:,a,b) = ft_z(:,a,b)*prefac(:)
               end do
            end do
            fa = self%a%first_gaussian; la = self%a%last_gaussian
            fb = self%b%first_gaussian; lb = self%b%last_gaussian
            ax = px(fa:la); ay = py(fa:la); az = pz(fa:la)
            bx = px(fb:lb); by = py(fb:lb); bz = pz(fb:lb)
            bxlast = bx(1)
            bylast = by(1)
            bzlast = bz(1)
            ft_xbx => ft_x(:,:,bxlast)
            ft_yby => ft_y(:,:,bylast)
            ft_zbz => ft_z(:,:,bzlast)
            do b = 1,self%b%n_comp  ! Loop over all elements and multiply prefactor
               xb = bx(b)
               yb = by(b)
               zb = bz(b)
               if (bxlast/=xb) then
                  ft_xbx => ft_x(:,:,xb)
                  bxlast = xb
               end if
               if (bylast/=yb) then
                  ft_yby => ft_y(:,:,yb)
                  bylast = yb
               end if
               if (bzlast/=zb) then
                  ft_zbz => ft_z(:,:,zb)
                  bzlast = zb
               end if
               do a = 1,self%a%n_comp
                  xa = ax(a)
                  ya = ay(a)
                  za = az(a)
                  ft(:,a,b) = ft_xbx(:,xa+1)*ft_yby(:,ya+1)*ft_zbz(:,za+1)
               end do
            end do
         end select
      end select

      ! Clean
      call destroy_(k_z)
      call destroy_(k_y)
      call destroy_(k_x)
      call destroy_(prefac)
      call destroy_(ft_z)
      call destroy_(ft_y)
      call destroy_(ft_x)
   end subroutine

   PURE subroutine make_ft_v2(self,ft,k_pts)
   ! Calculates the Fourier transform for a product of two primitive
   ! gaussian functions, evaluated at a series of k points "k_pts".
   ! This routine is based on formula (9) in Jayatilaka, CPL 230, p. 228 (1995)
      type(GAUSSIAN2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: ft
      MAT(REAL,:,:), IN :: k_pts
      MAT3(CPX,:,:,:), allocatable :: ft_x,ft_y,ft_z
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(CPX,:), allocatable :: prefac
      VEC(REAL,:), allocatable :: k_x,k_y,k_z
      REAL :: g1_exa,g1_exb
      REAL :: pifac,gamma,g1,g4,th,kk,k1,k2,k3,P1,P2,P3,PI_on_gamma
      INT :: k_max,a,b,k,fa,la,fb,lb
      CPX :: I

      ! Constants
      I = COMPLEXIFY(ZERO,ONE)
      gamma = self%a%exponent+self%b%exponent
      g1 = ONE/gamma
      g4 = QUARTER*g1
      PI_on_gamma = PI*g1
      pifac = sqrt(PI_on_gamma)*PI_on_gamma

      ! Space for fourier transform of each component
      k_max = size(k_pts,1)
      call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
      call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
      call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])

      ! Reciprocal lattice K points
      k_x = k_pts(:,1)
      k_y = k_pts(:,2)
      k_z = k_pts(:,3)

      ! Gaussian pair center
      g1_exa = g1*self%a%exponent
      g1_exb = g1*self%b%exponent
      P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
      P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
      P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)

      ! Make prefactor
      call create_(prefac,k_max)
      do k = 1,k_max
         k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
         th = k1*P1+k2*P2+k3*P3
         kk = k1*k1+k2*k2+k3*k3
         prefac(k) = pifac*(cos(th)+I*sin(th))*exp(-kk*g4)
      end do

      ! Fourier transforms of each component.
      call make_ft_component_(self,ft_x,1,k_x)
      call make_ft_component_(self,ft_y,2,k_y)
      call make_ft_component_(self,ft_z,3,k_z)

      ! Incorporate prefactor into ft_z
      do b = 0,self%b%l
         do a = 0,self%a%l
            ft_z(:,a,b) = ft_z(:,a,b)*prefac
         end do
      end do
      call destroy_(prefac)

      ! Gaussian function power arrays
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = px(fa:la); ay = py(fa:la); az = pz(fa:la)
      bx = px(fb:lb); by = py(fb:lb); bz = pz(fb:lb)

      ! Do the Fourier transform
      ft = ft_x(:,ax,bx)*ft_y(:,ay,by)*ft_z(:,az,bz)

      ! Clean up
      call destroy_(ft_z); call destroy_(ft_y); call destroy_(ft_x)
   end subroutine

   PURE subroutine make_ft_nabla(self,ft,k_pts)
   ! Calculates the Fourier transform of the current integrals for the product
   ! of two primitive gaussian functions, evaluated at a series of points
   ! "k_pts".
   ! This routine is based on formula (9) in Jayatilaka, CPL 230, p. 228 (1995)
   ! size of "ft" is [k_max],[.a.l.n_comp],[.b.l.n_comp],[3
      type(GAUSSIAN2_TYPE), IN :: self
      MAT4(CPX,:,:,:,:), OUT :: ft
      MAT(REAL,:,:), IN :: k_pts
      CPX :: ci,prefac
      REAL :: pifac,gamma,g1,g4,dot1,dot2,k1,k2,k3,P1,P2,P3
      INT :: k_max,a,b,k,xa,ya,za,xb,yb,zb,fa,la,fb,lb
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      MAT3(CPX,:,:,:), allocatable :: ft_x,ft_y,ft_z,fd_x,fd_y,fd_z

      ! A few useful constants ...
      ci = IMAGIFY(ONE)
      k_max = size(k_pts,1)
      gamma = self%a%exponent+self%b%exponent
      g1 = 1/gamma
      g4 = QUARTER*g1
      pifac = (PI/gamma)**(THREE/TWO)
      P1 = g1*(self%a%exponent*self%a%position(1)+self%b%exponent*self%b%position(1))
      P2 = g1*(self%a%exponent*self%a%position(2)+self%b%exponent*self%b%position(2))
      P3 = g1*(self%a%exponent*self%a%position(3)+self%b%exponent*self%b%position(3))
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = px(fa:la); ay = py(fa:la); az = pz(fa:la)
      bx = px(fb:lb); by = py(fb:lb); bz = pz(fb:lb)
      call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
      call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
      call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
      call make_ft_component_(self,ft_x,1,k_pts(:,1))  ! Get the Fourier transforms
      call make_ft_component_(self,ft_y,2,k_pts(:,2))  ! of each component.
      call make_ft_component_(self,ft_z,3,k_pts(:,3))
      call create_(fd_x,[1,k_max],[0,self%a%l],[0,self%b%l])
      call create_(fd_y,[1,k_max],[0,self%a%l],[0,self%b%l])
      call create_(fd_z,[1,k_max],[0,self%a%l],[0,self%b%l])
      call make_ft_dab_component_(self,fd_x,1,k_pts(:,1),k_max)  ! Get the Fourier transforms
      call make_ft_dab_component_(self,fd_y,2,k_pts(:,2),k_max)  ! of the derivative of each
      call make_ft_dab_component_(self,fd_z,3,k_pts(:,3),k_max)  ! component.
      do b = 1,self%b%n_comp  ! Loop over all elements and multiply prefactor
         xb = bx(b)
         yb = by(b)
         zb = bz(b)
         do a = 1,self%a%n_comp
            xa = ax(a)
            ya = ay(a)
            za = az(a)
            do k = 1,k_max
               k1 = k_pts(k,1)
               k2 = k_pts(k,2)
               k3 = k_pts(k,3)
               dot1 = k1*P1+k2*P2+k3*P3
               dot2 = k1*k1+k2*k2+k3*k3
               dot2 = -dot2*g4
               prefac = pifac*exp(ci*dot1)*exp(dot2)
               ft(k,a,b,1) = prefac*fd_x(k,xa,xb)*ft_y(k,ya,yb)*ft_z(k,za,zb)
               ft(k,a,b,2) = prefac*ft_x(k,xa,xb)*fd_y(k,ya,yb)*ft_z(k,za,zb)
               ft(k,a,b,3) = prefac*ft_x(k,xa,xb)*ft_y(k,ya,yb)*fd_z(k,za,zb)
            end do
         end do
      end do
      call destroy_(fd_z); call destroy_(fd_y); call destroy_(fd_x)
      call destroy_(ft_z); call destroy_(ft_y); call destroy_(ft_x)
   end subroutine

   PURE subroutine make_ft_da_db(self,ft_da,ft_db,k_pts)
   ! Calculates the Fourier transform of the derivatives of the product
   ! of two primitive gaussian functions, w.r.t. positions of center a and b,
   ! "ft_ad", and "ft_db" respectively, evaluated at a series of points "k_pts".
   ! This routine is based on formula (9) in Jayatilaka, CPL 230, p. 228 (1995)
   ! size of "ft_da" and "ft_db" is ([k_max],[.a.l.n_comp],[.b.l.n_comp],3)
      type(GAUSSIAN2_TYPE), IN :: self
      MAT4(CPX,:,:,:,:), OUT :: ft_da
      MAT4(CPX,:,:,:,:), OUT :: ft_db
      MAT(REAL,:,:), IN :: k_pts
      CPX :: ci,prefac
      REAL :: pifac,gamma,g1,g4,dot1,dot2,k1,k2,k3,P1,P2,P3
      INT :: k_max,a,b,k,xa,ya,za,xb,yb,zb,fa,la,fb,lb
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      MAT3(CPX,:,:,:), allocatable :: ft_x,ft_y,ft_z,fa_x,fa_y,fa_z,fb_x,fb_y,fb_z

      ! The imaginary constant i ...
      ci = IMAGIFY(ONE)
      k_max = size(k_pts,1)
      gamma = self%a%exponent+self%b%exponent
      g1 = 1/gamma
      g4 = QUARTER*g1
      pifac = (PI/gamma)**(THREE/TWO)
      P1 = g1*(self%a%exponent*self%a%position(1)+self%b%exponent*self%b%position(1))
      P2 = g1*(self%a%exponent*self%a%position(2)+self%b%exponent*self%b%position(2))
      P3 = g1*(self%a%exponent*self%a%position(3)+self%b%exponent*self%b%position(3))
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = px(fa:la); ay = py(fa:la); az = pz(fa:la)
      bx = px(fb:lb); by = py(fb:lb); bz = pz(fb:lb)
      call create_(ft_x,1,k_max,0,self%a%l,0,self%b%l)
      call create_(ft_y,1,k_max,0,self%a%l,0,self%b%l)
      call create_(ft_z,1,k_max,0,self%a%l,0,self%b%l)
      call make_ft_component_(self,ft_x,1,k_pts(:,1))  ! Get the Fourier transforms
      call make_ft_component_(self,ft_y,2,k_pts(:,2))  ! of each component.
      call make_ft_component_(self,ft_z,3,k_pts(:,3))
      call create_(fa_x,1,k_max,0,self%a%l,0,self%b%l)
      call create_(fa_y,1,k_max,0,self%a%l,0,self%b%l)
      call create_(fa_z,1,k_max,0,self%a%l,0,self%b%l)
      call create_(fb_x,1,k_max,0,self%a%l,0,self%b%l)
      call create_(fb_y,1,k_max,0,self%a%l,0,self%b%l)
      call create_(fb_z,1,k_max,0,self%a%l,0,self%b%l)
      call make_ft_da_db_component_(self,fa_x,fb_x,1,k_pts(:,1))  ! Get the Fourier transforms
      call make_ft_da_db_component_(self,fa_y,fb_y,2,k_pts(:,2))  ! of the derivative of each
      call make_ft_da_db_component_(self,fa_z,fb_z,3,k_pts(:,3))  ! component.
      do a = 1,self%a%n_comp  ! Loop over all elements and multiply prefactor
         xa = ax(a)
         ya = ay(a)
         za = az(a)
         do b = 1,self%b%n_comp
            xb = bx(b)
            yb = by(b)
            zb = bz(b)
            do k = 1,k_max
               k1 = k_pts(k,1)
               k2 = k_pts(k,2)
               k3 = k_pts(k,3)
               dot1 = k1*P1+k2*P2+k3*P3
               dot2 = k1*k1+k2*k2+k3*k3
               dot2 = -dot2*g4
               prefac = pifac*exp(ci*dot1)*exp(dot2)
               ft_da(k,a,b,1) = prefac*fa_x(k,xa,xb)*ft_y(k,ya,yb)*ft_z(k,za,zb)
               ft_da(k,a,b,2) = prefac*ft_x(k,xa,xb)*fa_y(k,ya,yb)*ft_z(k,za,zb)
               ft_da(k,a,b,3) = prefac*ft_x(k,xa,xb)*ft_y(k,ya,yb)*fa_z(k,za,zb)
               ft_db(k,a,b,1) = prefac*fb_x(k,xa,xb)*ft_y(k,ya,yb)*ft_z(k,za,zb)
               ft_db(k,a,b,2) = prefac*ft_x(k,xa,xb)*fb_y(k,ya,yb)*ft_z(k,za,zb)
               ft_db(k,a,b,3) = prefac*ft_x(k,xa,xb)*ft_y(k,ya,yb)*fb_z(k,za,zb)
            end do
         end do
      end do
      call destroy_(fb_z); call destroy_(fb_y); call destroy_(fb_x)
      call destroy_(fa_z); call destroy_(fa_y); call destroy_(fa_x)
      call destroy_(ft_z); call destroy_(ft_y); call destroy_(ft_x)
   end subroutine

   PURE subroutine make_ft_r(self,ft,k_pts)
   ! Calculates the Fourier transform of the derivative of a product of two
   ! primitive gaussian functions, times vector r, evaluated at a series of
   ! points "k_pts".
   ! This routine is based on formula (9) in Jayatilaka, CPL 230, p. 228 (1995)
   ! size of "ft" is [k_max,.a.n_comp,.b.n_comp,3]
      type(GAUSSIAN2_TYPE), IN :: self
      MAT4(CPX,:,:,:,:), OUT :: ft
      MAT(REAL,:,:), IN :: k_pts
      CPX :: ci,prefac
      REAL :: pifac,gamma,g1,g4,dot1,dot2,k1,k2,k3,P1,P2,P3
      INT :: k_max,a,b,k,xa,ya,za,xb,yb,zb,fa,la,fb,lb
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      MAT3(CPX,:,:,:), allocatable :: ft_x,ft_y,ft_z

      ! A few useful constants ...
      ci = IMAGIFY(ONE)
      k_max = size(k_pts,1)
      gamma = self%a%exponent+self%b%exponent
      g1 = 1/gamma
      g4 = QUARTER*g1
      pifac = (PI/gamma)**(THREE/TWO)
      P1 = g1*(self%a%exponent*self%a%position(1)+self%b%exponent*self%b%position(1))
      P2 = g1*(self%a%exponent*self%a%position(2)+self%b%exponent*self%b%position(2))
      P3 = g1*(self%a%exponent*self%a%position(3)+self%b%exponent*self%b%position(3))
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = px(fa:la); ay = py(fa:la); az = pz(fa:la)
      bx = px(fb:lb); by = py(fb:lb); bz = pz(fb:lb)
      call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l+1])
      call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l+1])
      call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l+1])

      ! Get the Fourier transforms of each component.
      call make_ft_component_(self,ft_x,1,k_pts(:,1))
      call make_ft_component_(self,ft_y,2,k_pts(:,2))
      call make_ft_component_(self,ft_z,3,k_pts(:,3))
      do a = 1,self%a%n_comp  ! Loop over all elements and multiply prefactor
         xa = ax(a)
         ya = ay(a)
         za = az(a)
         do b = 1,self%b%n_comp
            xb = bx(b)
            yb = by(b)
            zb = bz(b)
            do k = 1,k_max
               k1 = k_pts(k,1)
               k2 = k_pts(k,2)
               k3 = k_pts(k,3)
               dot1 = k1*P1+k2*P2+k3*P3
               dot2 = k1*k1+k2*k2+k3*k3
               dot2 = -dot2*g4
               prefac = pifac*exp(ci*dot1)*exp(dot2)
               ft(k,a,b,1) = prefac*(ft_x(k,xa,xb+1)+self%b%position(1)*ft_x(k,xa,xb))*ft_y(k,ya,yb)*ft_z(k,za,zb)
               ft(k,a,b,2) = prefac*ft_x(k,xa,xb)*(ft_y(k,ya,yb+1)+self%b%position(2)*ft_y(k,ya,yb))*ft_z(k,za,zb)
               ft(k,a,b,3) = prefac*ft_x(k,xa,xb)*ft_y(k,ya,yb)*(ft_z(k,za,zb+1)+self%b%position(3)*ft_z(k,za,zb))
            end do
         end do
      end do
      call destroy_(ft_z)
      call destroy_(ft_y)
      call destroy_(ft_x)
   end subroutine

   subroutine put_e_coeff(self,out)
   ! Put the McMurchie Davidson E coefficients to file "out"
      type(GAUSSIAN2_TYPE), IN :: self
      type(TEXTFILE_TYPE) :: out
      INT :: a,b,t
      MAT3(REAL,:,:,:), allocatable :: Ex
      MAT3(REAL,:,:,:), allocatable :: Ey
      MAT3(REAL,:,:,:), allocatable :: Ez
      call flush_(out)
      call text_(out,"GAUSSIAN2 output:")
      call flush_(out)
      call text_(out,"Dylan Jayatilaka, University of Western Australia, March 1998")
      call flush_(out)
      call show_(out,"l_a   =",self%a%l)
      call show_(out,"l_b   =",self%a%l)
      call show_(out,"Ra    =",self%a%position)
      call show_(out,"Rb    =",self%b%position)
      call show_(out,"R     =",self%a%position-self%b%position)
      call show_(out,"alpha =",self%a%exponent)
      call show_(out,"beta  =",self%b%exponent)
      call flush_(out)
      call dash_(out,int_fields=3,real_fields=3)
      call put_(out,"a",int_width=TRUE)
      call put_(out,"b",int_width=TRUE)
      call put_(out,"t",int_width=TRUE)
      call put_(out,"Ex(t,a,b)")
      call put_(out,"Ey(t,a,b)")
      call put_(out,"Ez(t,a,b)")
      call flush_(out)
      call create_(Ex,[0,self%a%l+self%b%l],[0,self%a%l],[0,self%b%l])
      call make_e_coeff_(self,Ex,1)
      call create_(Ey,[0,self%a%l+self%b%l],[0,self%a%l],[0,self%b%l])
      call make_e_coeff_(self,Ey,2)
      call create_(Ez,[0,self%a%l+self%b%l],[0,self%a%l],[0,self%b%l])
      call make_e_coeff_(self,Ez,3)
      call dash_(out,int_fields=3,real_fields=3)
      do a = 0,self%a%l
         do b = 0,self%b%l
            do t = 0,a+b
               call put_(out,a)
               call put_(out,b)
               call put_(out,t)
               call put_(out,Ex(t,a,b))
               call put_(out,Ey(t,a,b))
               call put_(out,Ez(t,a,b))
               call flush_(out)
            end do
         end do
      end do
      call destroy_(Ez)
      call destroy_(Ey)
      call destroy_(Ex)
   end subroutine


!  ======================
!  CADPAC-style integrals
!  ======================

   PURE function normalisation_factor(self) result (res)
   ! Return the normaliston factor assuming it is x^l
      type(GAUSSIAN2_TYPE), IN :: self
      REAL :: res
      res = normalisation_factor_(self%a)*normalisation_factor_(self%b)
   end function

   PURE subroutine change_to_spherical_0(self,C,S)
   ! Transform a cartesian integral block "C" to a spherical integral
   ! block "S"
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), IN :: C
      MAT(REAL,:,:), OUT :: S
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: UU
      MAT(REAL,:,:), allocatable :: W,U
      UU = spherical_harmonics_for
      if (self%a%l<2 AND self%b%l<2) then
         S = C  ! don't use this case
      else if (self%a%l>=2 AND self%b%l>=2) then
         call create_(W,self%a%n_comp,self%b%n_sph)
         U = UU(self%b%l)%element
         call to_product_of_(W,C,U)
         U = UU(self%a%l)%element
         call to_product_of_(S,U,W,transpose_a=TRUE)
         call destroy_(W)
      else if (self%a%l<2 AND self%b%l>=2) then
         U = UU(self%b%l)%element
         call to_product_of_(S,C,U)
      else if (self%b%l<2 AND self%a%l>=2) then
         U = UU(self%a%l)%element
         call to_product_of_(S,U,C,transpose_a=TRUE)
      end if
   end subroutine

   PURE subroutine change_to_spherical_1(self,C,S)
   ! Transform a cartesian integral block "C" to a spherical integral
   ! block "S"
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(CPX,:,:), IN :: C
      MAT(CPX,:,:), OUT :: S
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: UU
      MAT(REAL,:,:), allocatable :: U
      MAT(CPX,:,:), allocatable :: W
      UU = spherical_harmonics_for
      if (self%a%l<2 AND self%b%l<2) then
         S = C  ! don't use this case
      else if (self%a%l>=2 AND self%b%l>=2) then
         call create_(W,self%a%n_comp,self%b%n_sph)
         U = UU(self%b%l)%element
         call to_product_of_(W,C,U)
         U = UU(self%a%l)%element
         call to_product_of_(S,U,W,transpose_a=TRUE)
         call destroy_(W)
      else if (self%a%l<2 AND self%b%l>=2) then
         U = UU(self%b%l)%element
         call to_product_of_(S,C,U)
      else if (self%b%l<2 AND self%a%l>=2) then
         U = UU(self%a%l)%element
         call to_product_of_(S,U,C,transpose_a=TRUE)
      end if
   end subroutine

   PURE subroutine make_normalized_overlap_ints(self,S,spherical)
   ! Calculate S, a cartesian or spherical block of integrals.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: S
      BIN, optional, IN :: spherical
      MAT(REAL,:,:), allocatable :: W
      BIN :: is_spherical
      is_spherical = FALSE
      if (present(spherical)) is_spherical = spherical
      if (NOT is_spherical) then
         call make_overlap_ints_(self,S)
      else if (self%a%l<2 AND self%b%l<2) then
         call make_overlap_ints_(self,S)
      else
         call create_(W,self%a%n_comp,self%b%n_comp)
         call make_overlap_ints_(self,W)
         call change_to_spherical_(self,W,S)
         call destroy_(W)
      end if
      S = S*normalisation_factor_(self)
   end subroutine

   PURE subroutine make_overlap_ints_0(self,S)
   ! Make the overlap integral matrix "S". Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: S
      MAT(REAL,:,:), allocatable :: Sx,Sy,Sz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rab
      REAL :: p,prefac
      INT :: fa,la,fb,lb
   ! a,b,aix,aiy,aiz,bix,biy,biz :: INT
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx,self%a%l+1,self%b%l+1); call create_(Sy,self%a%l+1,self%b%l+1); call create_(Sz,self%a%l+1,self%b%l+1)
      call make_overlap_ints_(self,Sx,1)
      call make_overlap_ints_(self,Sy,2)
      call make_overlap_ints_(self,Sz,3)

      ! The following loops can be simplified by the one line statement:
      S = Sx(ax,bx)*Sy(ay,by)*Sz(az,bz)

   ! do b = 1,.b.n_comp
   !   bix = bx(b)
   !   biy = by(b)
   !   biz = bz(b)
   !   do a = 1,.a.n_comp
   !     aix = ax(a)
   !     aiy = ay(a)
   !     aiz = az(a)
   !     S(a,b) = Sx(aix,bix)*Sy(aiy,biy)*Sz(aiz,biz)
   !   end
   ! end
      call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
      p = self%a%exponent+self%b%exponent
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)/(p*sqrt(p))
      S = prefac*S
   end subroutine

   PURE subroutine make_overlap_ints_1(self,Sx,x,max_a,max_b)
   ! Make the 1-D overlap xyz integral matrix "Sx" for component "x".
   ! If present, "max_a" and "max_b" are used for the maximum angular momenta
   ! of centers (a) and (b) respectively -- for derivative integrals.
   ! Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Sx
      INT, IN :: x
      INT, optional, IN :: max_a,max_b
      REAL :: p,t,rp,ax,bx,wt,pt,ra,rb
      INT :: l_a,l_b,a,b,i,k
      l_a = self%a%l; if (present(max_a)) l_a = max_a
      l_b = self%b%l; if (present(max_b)) l_b = max_b
      p = self%a%exponent+self%b%exponent
      t = sqrt(p)
      ax = self%a%position(x)
      bx = self%b%position(x)
      rp = (self%a%exponent*ax+self%b%exponent*bx)/p
      Sx = ZERO
      do a = 1,l_a+1
         do b = 1,l_b+1
            i = (a+b)/2
            do k = first_pt(i),last_pt(i)
               wt = gauss_hermite_wt(k)
               pt = gauss_hermite_pt(k)/t+rp
               ra = pt-ax
               rb = pt-bx
               if (a>1) wt = wt*ra**(a-1)
               if (b>1) wt = wt*rb**(b-1)
               Sx(a,b) = Sx(a,b)+wt
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_3_center_overlap_ints_0(self,S,l_c,pos_c,ex_c)
   ! Make the three-center overlap integrals "S(a,b,c)" of the two
   ! gaussians with a third, of angular momentum "l_c" and exponent
   ! "ex_c" at "pos_c". Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: S
      INT, IN :: l_c
      VEC(REAL,3), IN :: pos_c
      REAL, IN :: ex_c
      MAT3(REAL,:,:,:), allocatable :: Sx,Sy,Sz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz,cx,cy,cz
      VEC(REAL,3) :: rab,rac,rbc
      REAL :: p,prefac
      INT :: fa,la,fb,lb,fc,lc
   ENSURE((self%a%l+self%b%l+l_c)/2+1<=size(first_pt),"GAUSSIAN2:make_3_center_overlap_ints_0 ... not enough Gauss-Hermite points")
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      fc = n_comp_up_to_(l_c-1)+1; lc = n_comp_up_to_(l_c)
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      cx = nx(fc:lc); cy = ny(fc:lc); cz = nz(fc:lc)
      call create_(Sx,self%a%l+1,self%b%l+1,l_c+1)
      call create_(Sy,self%a%l+1,self%b%l+1,l_c+1)
      call create_(Sz,self%a%l+1,self%b%l+1,l_c+1)
      call make_3_center_overlap_ints_(self,Sx,1,l_c,pos_c(1),ex_c)
      call make_3_center_overlap_ints_(self,Sy,2,l_c,pos_c(2),ex_c)
      call make_3_center_overlap_ints_(self,Sz,3,l_c,pos_c(3),ex_c)
      S = Sx(ax,bx,cx)*Sy(ay,by,cy)*Sz(az,bz,cz)
      call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
      p = self%a%exponent+self%b%exponent+ex_c
      rab = self%a%position-self%b%position
      rac = self%a%position-pos_c
      rbc = self%b%position-pos_c
      prefac = self%a%exponent*self%b%exponent*dot_(rab,rab)+self%a%exponent*ex_c*dot_(rac,rac)+self%b%exponent*ex_c*dot_(rbc,rbc)
      prefac = exp(-prefac/p)/(p*sqrt(p))
      S = prefac*S
   end subroutine

   PURE subroutine make_3_center_overlap_ints_1(self,Sx,x,l_c,cx,ex_c)
   ! Make the 1-D three-center overlap integrals "Sx" for component
   ! "x", the third gaussian at "cx". Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: Sx
      INT, IN :: x,l_c
      REAL, IN :: cx,ex_c
      REAL :: p,t,rp,ax,bx,wt,pt,ra,rb,rc
      INT :: a,b,c,i,k
      p = self%a%exponent+self%b%exponent+ex_c
      t = sqrt(p)
      ax = self%a%position(x)
      bx = self%b%position(x)
      rp = (self%a%exponent*ax+self%b%exponent*bx+ex_c*cx)/p
      Sx = ZERO
      do a = 1,self%a%l+1
         do b = 1,self%b%l+1
            do c = 1,l_c+1
               i = (a+b+c-1)/2
               do k = first_pt(i),last_pt(i)
                  wt = gauss_hermite_wt(k)
                  pt = gauss_hermite_pt(k)/t+rp
                  ra = pt-ax
                  rb = pt-bx
                  rc = pt-cx
                  if (a>1) wt = wt*ra**(a-1)
                  if (b>1) wt = wt*rb**(b-1)
                  if (c>1) wt = wt*rc**(c-1)
                  Sx(a,b,c) = Sx(a,b,c)+wt
               end do
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_S_1st_deriv_ints(self,AAx,AAy,AAz)
   ! Make the first derivative overlap integrals "AAx", "AAy" and "AAz" which are
   ! the derivatives of the overlap matrix elements with respect to position A.
   ! The derivatives with respect to position B are not evaluated. By translational
   ! invariance, they are just the negative of the derivatives w.r.t. A.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: AAx,AAy,AAz
      MAT(REAL,:,:), allocatable :: Sx,Sy,Sz,Dx,Dy,Dz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rab
      REAL :: p,prefac
      INT :: fa,la,fb,lb
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx,self%a%l+2,self%b%l+1); call create_(Sy,self%a%l+2,self%b%l+1); call create_(Sz,self%a%l+2,self%b%l+1)
      call create_(Dx,self%a%l+1,self%b%l+1); call create_(Dy,self%a%l+1,self%b%l+1); call create_(Dz,self%a%l+1,self%b%l+1)
      call make_overlap_ints_(self,Sx,1,self%a%l+1)
      call make_overlap_ints_(self,Sy,2,self%a%l+1)
      call make_overlap_ints_(self,Sz,3,self%a%l+1)

      ! Diffn. with respect to electron coordinates
      call differentiate_(self,Sx,"left",Dx)
      call differentiate_(self,Sy,"left",Dy)
      call differentiate_(self,Sz,"left",Dz)

!     The following loops can be simplified by the statements:
      AAx = -Dx(ax,bx)*Sy(ay,by)*Sz(az,bz)
      AAy = -Sx(ax,bx)*Dy(ay,by)*Sz(az,bz)
      AAz = -Sx(ax,bx)*Sy(ay,by)*Dz(az,bz)
      call destroy_(Dz); call destroy_(Dy); call destroy_(Dx)
      call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
      p = self%a%exponent+self%b%exponent
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)/(p*sqrt(p))
      AAx = prefac*AAx
      AAy = prefac*AAy
      AAz = prefac*AAz
   end subroutine

   PURE subroutine make_oscillator_overlap_ints_0(self,S,c,d,lc_min,lc_max,ld_min,ld_max)
   ! Make the oscillator overlap integral matrix "S". "c" and "d" are the centroid
   ! positions of the bra and ket. "lc_min", "lc_max", "ld_min" and "ld_max" are
   ! the corresponding minimum and maximum angular momenta for the centroid
   ! polynomials on "c" and "d".
      type(GAUSSIAN2_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: S
      VEC(REAL,3), IN :: c,d
      INT, IN :: lc_min,lc_max,ld_min,ld_max
      MAT4(REAL,:,:,:,:), allocatable :: Sx,Sy,Sz
      VEC(INT,:), allocatable :: ax,ay,az
      VEC(INT,:), allocatable :: bx,by,bz
      VEC(INT,:), allocatable :: cx,cy,cz
      VEC(INT,:), allocatable :: dx,dy,dz
      VEC(REAL,3) :: rab
      REAL :: p,prefac
      INT :: fa,la,fb,lb,fc,lc,fd,ld
   ENSURE(size(S,3)==(n_comp_up_to_(lc_max)-n_comp_up_to_(lc_min-1)),"GAUSSIAN2:make_oscillator_overlap_ints_0 ... S: bad dim3")
   ENSURE(size(S,4)==(n_comp_up_to_(ld_max)-n_comp_up_to_(ld_min-1)),"GAUSSIAN2:make_oscillator_overlap_ints_0 ... S: bad dim4")
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      fc = n_comp_up_to_(lc_min-1)+1; lc = n_comp_up_to_(lc_max)
      fd = n_comp_up_to_(ld_min-1)+1; ld = n_comp_up_to_(ld_max)
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      cx = nx(fc:lc); cy = ny(fc:lc); cz = nz(fc:lc)
      dx = nx(fd:ld); dy = ny(fd:ld); dz = nz(fd:ld)
      call create_(Sx,self%a%l+1,self%b%l+1,lc_max+1,ld_max+1)
      call create_(Sy,self%a%l+1,self%b%l+1,lc_max+1,ld_max+1)
      call create_(Sz,self%a%l+1,self%b%l+1,lc_max+1,ld_max+1)
      call make_oscillator_overlap_ints_(self,Sx,1,c(1),lc_max,d(1),ld_max)
      call make_oscillator_overlap_ints_(self,Sy,2,c(2),lc_max,d(2),ld_max)
      call make_oscillator_overlap_ints_(self,Sz,3,c(3),lc_max,d(3),ld_max)
      S = Sx(ax,bx,cx,dx)*Sy(ay,by,cy,dy)*Sz(az,bz,cz,dz)
      call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
      p = self%a%exponent+self%b%exponent
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)/(p*sqrt(p))
      S = prefac*S
   end subroutine

   PURE subroutine make_oscillator_overlap_ints_1(self,Sx,x,cx,l_c,dx,l_d,max_a,max_b)
   ! Make the 1-D overlap xyz integral matrix "Sx" for component "x".
   ! The oscillator polynomials are centered at "cx" and "dx". "l_c"
   ! and "l_d" are the corresponding maximum angular momenta of each
   ! oscillator. If present, "max_a" and "max_b" specify the maximum
   ! angular momenta on centers a and b respectively. Uses
   ! Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: Sx
      INT, IN :: x,l_c,l_d
      REAL, IN :: cx,dx
      REAL, optional, IN :: max_a,max_b
      REAL :: p,t,rp,ax,bx,wt,pt,ra,rb,rc,rd
      INT :: l_a,l_b,a,b,c,d,i,k
      l_a = self%a%l; if (present(max_a)) l_a = int(max_a)
      l_b = self%b%l; if (present(max_b)) l_b = int(max_b)
      p = self%a%exponent+self%b%exponent
      t = sqrt(p)
      ax = self%a%position(x)
      bx = self%b%position(x)
      rp = (self%a%exponent*ax+self%b%exponent*bx)/p
      Sx = ZERO
      do a = 1,l_a+1
         do b = 1,l_b+1
            do c = 1,l_c+1
               do d = 1,l_d+1
                  i = (a+b+c+d)/2
                  do k = first_pt(i),last_pt(i)
                     wt = gauss_hermite_wt(k)
                     pt = gauss_hermite_pt(k)/t+rp
                     ra = pt-ax
                     rb = pt-bx
                     rc = pt-cx
                     rd = pt-dx
                     if (a>1) wt = wt*ra**(a-1)
                     if (b>1) wt = wt*rb**(b-1)
                     if (c>1) wt = wt*rc**(c-1)
                     if (d>1) wt = wt*rd**(d-1)
                     Sx(a,b,c,d) = Sx(a,b,c,d)+wt
                  end do
               end do
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_normalized_kinetic_ints(self,T,spherical)
   ! Calculate T, a cartesian or spherical block of integrals.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: T
      BIN, optional, IN :: spherical
      MAT(REAL,:,:), allocatable :: W
      BIN :: is_spherical
      is_spherical = FALSE
      if (present(spherical)) is_spherical = spherical
      if (NOT is_spherical) then
         call make_kinetic_ints_(self,T)
      else if (self%a%l<2 AND self%b%l<2) then
         call make_kinetic_ints_(self,T)
      else
         call create_(W,self%a%n_comp,self%b%n_comp)
         call make_kinetic_ints_(self,W)
         call change_to_spherical_(self,W,T)
         call destroy_(W)
      end if
      T = T*normalisation_factor_(self)
   end subroutine

   PURE subroutine make_kinetic_ints(self,T)
   ! Make the kinetic integrals "T". Uses Gauss-Hermite quadrature. This routine is
   ! very inefficient. This includes the factor of -1/2 in T = -1/2 \nabla^2.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: T
      MAT(REAL,:,:), allocatable :: Sx,Sy,Sz,Dx,Dy,Dz,Tx,Ty,Tz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rab
      REAL :: p,prefac
      INT :: fa,la,fb,lb
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx,self%a%l+1,self%b%l+3); call create_(Sy,self%a%l+1,self%b%l+3); call create_(Sz,self%a%l+1,self%b%l+3)
      call create_(Tx,self%a%l+1,self%b%l+1); call create_(Ty,self%a%l+1,self%b%l+1); call create_(Tz,self%a%l+1,self%b%l+1)
      call create_(Dx,self%a%l+1,self%b%l+2); call create_(Dy,self%a%l+1,self%b%l+2); call create_(Dz,self%a%l+1,self%b%l+2)
      call make_overlap_ints_(self,Sx,1,max_b=self%b%l+2)
      call make_overlap_ints_(self,Sy,2,max_b=self%b%l+2)
      call make_overlap_ints_(self,Sz,3,max_b=self%b%l+2)
      call differentiate_(self,Sx,"right",Dx)  ! with respect to electron coordinates !
      call differentiate_(self,Sy,"right",Dy)
      call differentiate_(self,Sz,"right",Dz)
      call differentiate_(self,Dx,"right",Tx)  ! with respect to electron coordinates !
      call differentiate_(self,Dy,"right",Ty)
      call differentiate_(self,Dz,"right",Tz)
      call destroy_(Dz)
      call destroy_(Dy)
      call destroy_(Dx)
      T = Tx(ax,bx)*Sy(ay,by)*Sz(az,bz)+Sx(ax,bx)*Ty(ay,by)*Sz(az,bz)+Sx(ax,bx)*Sy(ay,by)*Tz(az,bz)
      call destroy_(Tz); call destroy_(Ty); call destroy_(Tx)
      call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
      p = self%a%exponent+self%b%exponent
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)/(p*sqrt(p))
      T = -HALF*prefac*T
   end subroutine

   PURE subroutine make_T_1st_deriv_ints(self,AAx,AAy,AAz)
   ! Make the first derivative kinetic integrals "AAx", "AAy" and "AAz" which are
   ! the derivatives of the kinetic matrix elements with respect to position A.
   ! The derivatives with respect to position B are not evaluated. By translational
   ! invariance, they are just the negative of the derivatives w.r.t. A.
   ! This includes the factor of -1/2 in T = -1/2 \nabla^2.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: AAx,AAy,AAz
      MAT(REAL,:,:), allocatable :: SSx,SSy,SSz
      MAT(REAL,:,:), allocatable :: SAx,SAy,SAz
      MAT(REAL,:,:), allocatable :: TTx,TTy,TTz
      MAT(REAL,:,:), allocatable :: TAx,TAy,TAz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rab
      REAL :: p,prefac
      INT :: fa,la,fb,lb
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(SSx,self%a%l+4,self%b%l+1); call create_(SSy,self%a%l+4,self%b%l+1); call create_(SSz,self%a%l+4,self%b%l+1)
      call create_(SAx,self%a%l+3,self%b%l+1); call create_(SAy,self%a%l+3,self%b%l+1); call create_(SAz,self%a%l+3,self%b%l+1)
      call create_(TTx,self%a%l+2,self%b%l+1); call create_(TTy,self%a%l+2,self%b%l+1); call create_(TTz,self%a%l+2,self%b%l+1)
      call create_(TAx,self%a%l+1,self%b%l+1); call create_(TAy,self%a%l+1,self%b%l+1); call create_(TAz,self%a%l+1,self%b%l+1)
      call make_overlap_ints_(self,SSx,1,self%a%l+3)
      call make_overlap_ints_(self,SSy,2,self%a%l+3)
      call make_overlap_ints_(self,SSz,3,self%a%l+3)
      call differentiate_(self,SSx,"left",SAx)  ! with respect to electron coordinates !
      call differentiate_(self,SSy,"left",SAy)
      call differentiate_(self,SSz,"left",SAz)
      call differentiate_(self,SAx,"left",TTx)  ! with respect to electron coordinates !
      call differentiate_(self,SAy,"left",TTy)
      call differentiate_(self,SAz,"left",TTz)
      call differentiate_(self,TTx,"left",TAx)  ! with respect to electron coordinates !
      call differentiate_(self,TTy,"left",TAy)
      call differentiate_(self,TTz,"left",TAz)
      AAx = TAx(ax,bx)*SSy(ay,by)*SSz(az,bz)+SAx(ax,bx)*TTy(ay,by)*SSz(az,bz)+SAx(ax,bx)*SSy(ay,by)*TTz(az,bz)
      AAy = TTx(ax,bx)*SAy(ay,by)*SSz(az,bz)+SSx(ax,bx)*TAy(ay,by)*SSz(az,bz)+SSx(ax,bx)*SAy(ay,by)*TTz(az,bz)
      AAz = TTx(ax,bx)*SSy(ay,by)*SAz(az,bz)+SSx(ax,bx)*TTy(ay,by)*SAz(az,bz)+SSx(ax,bx)*SSy(ay,by)*TAz(az,bz)
      call destroy_(TAz); call destroy_(TAy); call destroy_(TAx)
      call destroy_(TTz); call destroy_(TTy); call destroy_(TTx)
      call destroy_(SAz); call destroy_(SAy); call destroy_(SAx)
      call destroy_(SSz); call destroy_(SSy); call destroy_(SSx)
      p = self%a%exponent+self%b%exponent
      rab = self%a%position-self%b%position
      prefac = HALF*exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)/(p*sqrt(p))
      AAx = prefac*AAx  ! should be -1/2 factor on line above, but derivatives are
      AAy = prefac*AAy  ! wrt electrons not nuclei so that reverses the sign
      AAz = prefac*AAz
   end subroutine

   PURE subroutine make_dipole_ints_0(self,Dx,Dy,Dz,origin)
   ! Make the dipole integral matrices "Dx", "Dy", and "Dz".
   ! "origin" is the gauge origin for the integrals.
   ! Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Dx,Dy,Dz
      VEC(REAL,3), IN :: origin
      MAT(REAL,:,:), allocatable :: Sx,Sy,Sz,Rx,Ry,Rz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rab
      REAL :: p,prefac
      INT :: fa,la,fb,lb
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx,self%a%l+1,self%b%l+1); call create_(Sy,self%a%l+1,self%b%l+1); call create_(Sz,self%a%l+1,self%b%l+1)
      call create_(Rx,self%a%l+1,self%b%l+1); call create_(Ry,self%a%l+1,self%b%l+1); call create_(Rz,self%a%l+1,self%b%l+1)
      call make_dipole_ints_(self,Sx,Rx,1,origin(1))
      call make_dipole_ints_(self,Sy,Ry,2,origin(2))
      call make_dipole_ints_(self,Sz,Rz,3,origin(3))
      Dx = Rx(ax,bx)*Sy(ay,by)*Sz(az,bz)
      Dy = Sx(ax,bx)*Ry(ay,by)*Sz(az,bz)
      Dz = Sx(ax,bx)*Sy(ay,by)*Rz(az,bz)
      call destroy_(Rz); call destroy_(Ry); call destroy_(Rx)
      call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
      p = self%a%exponent+self%b%exponent
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)/(p*sqrt(p))
      Dx = prefac*Dx
      Dy = prefac*Dy
      Dz = prefac*Dz
   end subroutine

   PURE subroutine make_dipole_ints_1(self,Sx,Dx,x,ox,max_a,max_b)
   ! Make 1-D overlap integrals "Sx" and dipole integrals "Dx" for component "x".
   ! "ox" is the origin for the dipole terms. If present, "max_a" and "max_b"
   ! override the l value for gaussian a or b. Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Sx,Dx
      INT, IN :: x
      REAL, IN :: ox
      INT, optional, IN :: max_a,max_b
      REAL :: p,t,ax,bx,rp,wt,pt,ra,rb,rd
      INT :: l_a,l_b,a,b,i,k
      l_a = self%a%l; if (present(max_a)) l_a = max_a
      l_b = self%b%l; if (present(max_b)) l_b = max_b
      p = self%a%exponent+self%b%exponent
      t = sqrt(p)
      ax = self%a%position(x)
      bx = self%b%position(x)
      rp = (self%a%exponent*ax+self%b%exponent*bx)/p
      Sx = ZERO
      Dx = ZERO
      do a = 1,l_a+1
         do b = 1,l_b+1
            i = (a+b+1)/2
            do k = first_pt(i),last_pt(i)
               wt = gauss_hermite_wt(k)
               pt = gauss_hermite_pt(k)/t+rp
               ra = pt-ax
               rb = pt-bx
               rd = pt-ox
               if (a>1) wt = wt*ra**(a-1)
               if (b>1) wt = wt*rb**(b-1)
               Sx(a,b) = Sx(a,b)+wt
               Dx(a,b) = Dx(a,b)+rd*wt
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_quadrupole_ints_0(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz,origin)
   ! Make the qudrupole integral matrices "Qxx", ..., "Qyz".
   ! "origin" is the gauge origin for the integrals.
   ! Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Qxx,Qyy,Qzz,Qxy,Qxz,Qyz
      VEC(REAL,3), IN :: origin
      MAT(REAL,:,:), allocatable :: Sx,Sy,Sz,Rx,Ry,Rz,Qx,Qy,Qz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rab
      REAL :: p,prefac
      INT :: fa,la,fb,lb
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx,self%a%l+1,self%b%l+1); call create_(Sy,self%a%l+1,self%b%l+1); call create_(Sz,self%a%l+1,self%b%l+1)
      call create_(Rx,self%a%l+1,self%b%l+1); call create_(Ry,self%a%l+1,self%b%l+1); call create_(Rz,self%a%l+1,self%b%l+1)
      call create_(Qx,self%a%l+1,self%b%l+1); call create_(Qy,self%a%l+1,self%b%l+1); call create_(Qz,self%a%l+1,self%b%l+1)
      call make_quadrupole_ints_(self,Sx,Rx,Qx,1,origin(1))
      call make_quadrupole_ints_(self,Sy,Ry,Qy,2,origin(2))
      call make_quadrupole_ints_(self,Sz,Rz,Qz,3,origin(3))
      Qxx = Qx(ax,bx)*Sy(ay,by)*Sz(az,bz)
      Qyy = Sx(ax,bx)*Qy(ay,by)*Sz(az,bz)
      Qzz = Sx(ax,bx)*Sy(ay,by)*Qz(az,bz)
      Qxy = Rx(ax,bx)*Ry(ay,by)*Sz(az,bz)
      Qxz = Rx(ax,bx)*Sy(ay,by)*Rz(az,bz)
      Qyz = Sx(ax,bx)*Ry(ay,by)*Rz(az,bz)
      call destroy_(Qz); call destroy_(Qy); call destroy_(Qx)
      call destroy_(Rz); call destroy_(Ry); call destroy_(Rx)
      call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
      p = self%a%exponent+self%b%exponent
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)/(p*sqrt(p))
      Qxx = prefac*Qxx
      Qyy = prefac*Qyy
      Qzz = prefac*Qzz
      Qxy = prefac*Qxy
      Qxz = prefac*Qxz
      Qyz = prefac*Qyz
   end subroutine

   PURE subroutine make_quadrupole_ints_1(self,Sx,Dx,Qx,x,ox,max_a,max_b)
   ! Make the 1-D overlap integrals "Sx", dipole integrals "Dx" and quadruple
   ! integrals "Qx" for component "x". "ox" is the origin for the dipole terms.
   ! If present, "max_a" and "max_b" override the l value for gaussian a or b.
   ! Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Sx,Dx,Qx
      INT, IN :: x
      REAL, IN :: ox
      INT, optional, IN :: max_a,max_b
      REAL :: p,t,ax,bx,rp,wt,pt,ra,rb,rd
      INT :: l_a,l_b,a,b,i,k
      l_a = self%a%l; if (present(max_a)) l_a = max_a
      l_b = self%b%l; if (present(max_b)) l_b = max_b
      p = self%a%exponent+self%b%exponent
      t = sqrt(p)
      ax = self%a%position(x)
      bx = self%b%position(x)
      rp = (self%a%exponent*ax+self%b%exponent*bx)/p
      Sx = ZERO
      Dx = ZERO
      Qx = ZERO
      do a = 1,l_a+1
         do b = 1,l_b+1
            i = (a+b+2)/2
            do k = first_pt(i),last_pt(i)
               wt = gauss_hermite_wt(k)
               pt = gauss_hermite_pt(k)/t+rp
               ra = pt-ax
               rb = pt-bx
               rd = pt-ox
               if (a>1) wt = wt*ra**(a-1)
               if (b>1) wt = wt*rb**(b-1)
               Sx(a,b) = Sx(a,b)+wt
               Dx(a,b) = Dx(a,b)+rd*wt
               Qx(a,b) = Qx(a,b)+rd*rd*wt
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_octupole_ints_0(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz,origin)
   ! Make the octupole integral matrices "Oxxx", ..., "Oxyz".
   ! "origin" is the gauge origin for the integrals.
   ! Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz
      VEC(REAL,3), IN :: origin
      MAT(REAL,:,:), allocatable :: Sx,Sy,Sz,Rx,Ry,Rz,Qx,Qy,Qz,Ox,Oy,Oz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rab
      REAL :: p,prefac
      INT :: fa,la,fb,lb
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx,self%a%l+1,self%b%l+1); call create_(Sy,self%a%l+1,self%b%l+1); call create_(Sz,self%a%l+1,self%b%l+1)
      call create_(Rx,self%a%l+1,self%b%l+1); call create_(Ry,self%a%l+1,self%b%l+1); call create_(Rz,self%a%l+1,self%b%l+1)
      call create_(Qx,self%a%l+1,self%b%l+1); call create_(Qy,self%a%l+1,self%b%l+1); call create_(Qz,self%a%l+1,self%b%l+1)
      call create_(Ox,self%a%l+1,self%b%l+1); call create_(Oy,self%a%l+1,self%b%l+1); call create_(Oz,self%a%l+1,self%b%l+1)
      call make_octupole_ints_(self,Sx,Rx,Qx,Ox,1,origin(1))
      call make_octupole_ints_(self,Sy,Ry,Qy,Oy,2,origin(2))
      call make_octupole_ints_(self,Sz,Rz,Qz,Oz,3,origin(3))
      Oxxx = Ox(ax,bx)*Sy(ay,by)*Sz(az,bz)
      Oyyy = Sx(ax,bx)*Oy(ay,by)*Sz(az,bz)
      Ozzz = Sx(ax,bx)*Sy(ay,by)*Oz(az,bz)
      Oxxy = Qx(ax,bx)*Ry(ay,by)*Sz(az,bz)
      Oxxz = Qx(ax,bx)*Sy(ay,by)*Rz(az,bz)
      Oyyx = Rx(ax,bx)*Qy(ay,by)*Sz(az,bz)
      Oyyz = Sx(ax,bx)*Qy(ay,by)*Rz(az,bz)
      Ozzx = Rx(ax,bx)*Sy(ay,by)*Qz(az,bz)
      Ozzy = Sx(ax,bx)*Ry(ay,by)*Qz(az,bz)
      Oxyz = Rx(ax,bx)*Ry(ay,by)*Rz(az,bz)
      call destroy_(Oz); call destroy_(Oy); call destroy_(Ox)
      call destroy_(Qz); call destroy_(Qy); call destroy_(Qx)
      call destroy_(Rz); call destroy_(Ry); call destroy_(Rx)
      call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
      p = self%a%exponent+self%b%exponent
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)/(p*sqrt(p))
      Oxxx = prefac*Oxxx
      Oyyy = prefac*Oyyy
      Ozzz = prefac*Ozzz
      Oxxy = prefac*Oxxy
      Oxxz = prefac*Oxxz
      Oyyx = prefac*Oyyx
      Oyyz = prefac*Oyyz
      Ozzx = prefac*Ozzx
      Ozzy = prefac*Ozzy
      Oxyz = prefac*Oxyz
   end subroutine

   PURE subroutine make_octupole_ints_1(self,Sx,Dx,Qx,Ox,x,og,max_a,max_b)
   ! Make the 1-D overlap integrals "Sx", dipole integrals "Dx", quadruple
   ! integrals "Qx", and the ocupole integrals "Ox", for component "x". "og" is
   ! the origin for the dipole terms.
   ! If present, "max_a" and "max_b" override the l value for gaussian a or b.
   ! Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Sx,Dx,Qx,Ox
      INT, IN :: x
      REAL, IN :: og
      INT, optional, IN :: max_a,max_b
      REAL :: p,t,ax,bx,rp,wt,pt,ra,rb,rd,rd2
      INT :: l_a,l_b,a,b,i,k
      l_a = self%a%l; if (present(max_a)) l_a = max_a
      l_b = self%b%l; if (present(max_b)) l_b = max_b
      p = self%a%exponent+self%b%exponent
      t = sqrt(p)
      ax = self%a%position(x)
      bx = self%b%position(x)
      rp = (self%a%exponent*ax+self%b%exponent*bx)/p
      Sx = ZERO
      Dx = ZERO
      Qx = ZERO
      Ox = ZERO
      do a = 1,l_a+1
         do b = 1,l_b+1
            i = (a+b+3)/2
            do k = first_pt(i),last_pt(i)
               wt = gauss_hermite_wt(k)
               pt = gauss_hermite_pt(k)/t+rp
               ra = pt-ax
               rb = pt-bx
               rd = pt-og
               rd2 = rd*rd
               if (a>1) wt = wt*ra**(a-1)
               if (b>1) wt = wt*rb**(b-1)
               Sx(a,b) = Sx(a,b)+wt
               Dx(a,b) = Dx(a,b)+rd*wt
               Qx(a,b) = Qx(a,b)+rd2*wt
               Ox(a,b) = Ox(a,b)+rd2*rd*wt
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_normalized_NA_ints(self,NA,c,spherical,eta)
   ! Calculate "NA", a cartesian or spherical block of nuclear attraction
   ! integrals for a nucleus at position "c". If "eta" is present the
   ! nucleus is a gaussian charge of that exponent.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: NA
      VEC(REAL,3), IN :: c
      BIN, optional, IN :: spherical
      REAL, optional, IN :: eta
      MAT(REAL,:,:), allocatable :: W
      BIN :: is_spherical
      is_spherical = FALSE
      if (present(spherical)) is_spherical = spherical
      if (NOT is_spherical) then
         call make_nuclear_attraction_ints_(self,NA,c,eta)
      else if (self%a%l<2 AND self%b%l<2) then
         call make_nuclear_attraction_ints_(self,NA,c,eta)
      else
         call create_(W,self%a%n_comp,self%b%n_comp)
         call make_nuclear_attraction_ints_(self,W,c,eta)
         call change_to_spherical_(self,W,NA)
         call destroy_(W)
      end if
      NA = NA*normalisation_factor_(self)
   end subroutine

   PURE subroutine make_nuclear_attraction_ints_0(self,NA,c,eta)
   ! Make the nuclear attraction integral matrix "NA" for a nucleus at
   ! position "c". Uses Rys decomposition of 1/r_c and Gauss-Hermite
   ! quadrature. If "eta" is present the nucleus is the gaussian charge
   ! (eta/pi)^3/2 exp(-eta r_c^2); see "make_pVp_ints".
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: NA
      VEC(REAL,3), IN :: c
      REAL, optional, IN :: eta
      MAT(REAL,:,:), allocatable :: NAx,NAy,NAz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac,fac,t2
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      n_roots = (self%a%l+self%b%l+2)/2
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c
      call create_(rys,n_roots)
      xx = p*dot_(rpc,rpc)
      if (present(eta)) then
         fac = eta/(p+eta)
         xx = fac*xx
      end if
      call get_weights_and_u_roots_(rys,xx)
      call create_(NAx,self%a%l+1,self%b%l+1)
      call create_(NAy,self%a%l+1,self%b%l+1)
      call create_(NAz,self%a%l+1,self%b%l+1)
      NA = ZERO
      do k = 1,n_roots
         uu = p*rys%root(k)
         wt = rys%weight(k)
         if (present(eta)) then
            t2 = rys%root(k)/(ONE+rys%root(k))
            uu = p*fac*t2/(ONE-fac*t2)
         end if
         call make_nuclear_attraction_ints_(self,NAx,1,uu,c(1))
         call make_nuclear_attraction_ints_(self,NAy,2,uu,c(2))
         call make_nuclear_attraction_ints_(self,NAz,3,uu,c(3))
         NAz = NAz*wt
         NA = NA+NAx(ax,bx)*NAy(ay,by)*NAz(az,bz)
      end do
      call destroy_(rys)
      call destroy_(NAz); call destroy_(NAy); call destroy_(NAx)
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/p
      if (present(eta)) prefac = prefac*sqrt(fac)
      NA = prefac*NA
   end subroutine

   PURE subroutine make_nuclear_attraction_ints_1(self,Nx,x,uu,cx,max_a,max_b)
   ! Make the 1-D nuclear attraction xyz integrals "Nx", for component "x", for
   ! a particular Rys-gaussian with exponent "uu", used in the decomposition of
   ! 1/r_c centerd at position "cx". If present, "max_a" and "max_b" override
   ! the l value for gaussian a or b.
   ! Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Nx
      INT, IN :: x
      REAL, IN :: uu,cx
      INT, optional, IN :: max_a,max_b
      REAL :: p,t,ax,bx,r0,wt,pt,ra,rb
      INT :: l_a,l_b,a,b,i,k
      l_a = self%a%l; if (present(max_a)) l_a = max_a
      l_b = self%b%l; if (present(max_b)) l_b = max_b
      p = self%a%exponent+self%b%exponent+uu
      t = sqrt(p)
      ax = self%a%position(x)
      bx = self%b%position(x)
      r0 = (self%a%exponent*ax+self%b%exponent*bx+uu*cx)/p
      Nx = ZERO
      do a = 1,l_a+1
         do b = 1,l_b+1
            i = (a+b)/2  ! should be max((a+b-1)/2,1) ?
            do k = first_pt(i),last_pt(i)
               wt = gauss_hermite_wt(k)
               pt = gauss_hermite_pt(k)/t+r0
               ra = pt-ax
               rb = pt-bx
               if (a>1) wt = wt*ra**(a-1)
               if (b>1) wt = wt*rb**(b-1)
               Nx(a,b) = Nx(a,b)+wt
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_NA_1st_deriv_ints(self,DAx,DAy,DAz,DBx,DBy,DBz,c)
   ! Make the derivatives nuclear attraction integrals w.r.t. centers A and B in
   ! "DAx", "DAy" and "DAz", and "DBx", "DBy" and "DBz" for a nucleus at position
   ! "c". Uses Rys decomposition of 1/r_c and Gauss-Hermite quadrature
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: DAx,DAy,DAz
      MAT(REAL,:,:), OUT :: DBx,DBy,DBz
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: NAx,NAy,NAz
      MAT(REAL,:,:), allocatable :: AAx,AAy,AAz
      MAT(REAL,:,:), allocatable :: BBx,BBy,BBz
      VEC(INT,:), allocatable :: ax,ay,az
      VEC(INT,:), allocatable :: bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(NAx,self%a%l+2,self%b%l+2); call create_(NAy,self%a%l+2,self%b%l+2); call create_(NAz,self%a%l+2,self%b%l+2)
      call create_(AAx,self%a%l+1,self%b%l+1); call create_(AAy,self%a%l+1,self%b%l+1); call create_(AAz,self%a%l+1,self%b%l+1)
      call create_(BBx,self%a%l+1,self%b%l+1); call create_(BBy,self%a%l+1,self%b%l+1); call create_(BBz,self%a%l+1,self%b%l+1)

   ! n_roots = (.a.l+.b.l+3)/2
      n_roots = (self%a%l+self%b%l+1)/2+1
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c
      call create_(rys,n_roots)
      xx = p*dot_(rpc,rpc)
      call get_weights_and_u_roots_(rys,xx)
      DAx = ZERO; DAy = ZERO; DAz = ZERO
      DBx = ZERO; DBy = ZERO; DBz = ZERO
      do k = 1,n_roots
         uu = p*rys%root(k)
         wt = rys%weight(k)
         call make_nuclear_attraction_ints_(self,NAx,1,uu,c(1),self%a%l+1,self%b%l+1)
         call make_nuclear_attraction_ints_(self,NAy,2,uu,c(2),self%a%l+1,self%b%l+1)
         call make_nuclear_attraction_ints_(self,NAz,3,uu,c(3),self%a%l+1,self%b%l+1)
         call differentiate_(self,NAx,"right",BBx)  ! with respect to electron coordinates !
         call differentiate_(self,NAy,"right",BBy)
         call differentiate_(self,NAz,"right",BBz)
         call differentiate_(self,NAx,"left ",AAx)  ! with respect to electron coordinates !
         call differentiate_(self,NAy,"left ",AAy)
         call differentiate_(self,NAz,"left ",AAz)
         NAz = NAz*wt
         AAz = AAz*wt
         BBz = BBz*wt
         DAx = DAx-AAx(ax,bx)*NAy(ay,by)*NAz(az,bz)
         DAy = DAy-NAx(ax,bx)*AAy(ay,by)*NAz(az,bz)
         DAz = DAz-NAx(ax,bx)*NAy(ay,by)*AAz(az,bz)
         DBx = DBx-BBx(ax,bx)*NAy(ay,by)*NAz(az,bz)
         DBy = DBy-NAx(ax,bx)*BBy(ay,by)*NAz(az,bz)
         DBz = DBz-NAx(ax,bx)*NAy(ay,by)*BBz(az,bz)
      end do
      call destroy_(rys)
      call destroy_(BBz); call destroy_(BBy); call destroy_(BBx)
      call destroy_(AAz); call destroy_(AAy); call destroy_(AAx)
      call destroy_(NAz); call destroy_(NAy); call destroy_(NAx)
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/p
      DAx = prefac*DAx; DAy = prefac*DAy; DAz = prefac*DAz
      DBx = prefac*DBx; DBy = prefac*DBy; DBz = prefac*DBz
   end subroutine

   PURE subroutine make_normalized_pVp_ints(self,W,c,spherical,eta)
   ! Calculate "W", a cartesian or spherical block of the integrals of
   ! p.(1/r_c)p for a nucleus at position "c", where p is the momentum.
   ! If "eta" is present the nucleus is a gaussian charge of that exponent.
   ! See "make_pVp_ints".
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: W
      VEC(REAL,3), IN :: c
      BIN, optional, IN :: spherical
      REAL, optional, IN :: eta
      MAT(REAL,:,:), allocatable :: Wc
      BIN :: is_spherical
      is_spherical = FALSE
      if (present(spherical)) is_spherical = spherical
      if (NOT is_spherical) then
         call make_pVp_ints_(self,W,c,eta)
      else if (self%a%l<2 AND self%b%l<2) then
         call make_pVp_ints_(self,W,c,eta)
      else
         call create_(Wc,self%a%n_comp,self%b%n_comp)
         call make_pVp_ints_(self,Wc,c,eta)
         call change_to_spherical_(self,Wc,W)
         call destroy_(Wc)
      end if
      W = W*normalisation_factor_(self)
   end subroutine

   PURE subroutine make_pVp_ints(self,W,c,eta)
   ! Make the integral matrix "W" of p.(1/r_c)p for a nucleus at position
   ! "c", where p = -i grad is the momentum:
   !    W_ab = <grad a| 1/r_c |grad b> ,
   ! the sum over x, y and z of the nuclear attraction integral between
   ! the derivatives of the two gaussians. This is the scalar part of
   ! (sigma.p)(1/r_c)(sigma.p), the only integral a scalar-relativistic
   ! hamiltonian needs beyond the non-relativistic ones. Uses Rys
   ! decomposition of 1/r_c and Gauss-Hermite quadrature.
   !
   ! If "eta" is present the nucleus is not a point but the unit charge
   ! (eta/pi)^3/2 exp(-eta r_c^2), whose potential is erf(sqrt(eta) r_c)/r_c.
   ! That is the same integral over gaussians exp(-u^2 r_c^2), stopped at
   ! u^2 = eta: with p the sum of the exponents and f = eta/(p+eta), the
   ! Rys argument is scaled by f, a root t^2 gives u^2 = p f t^2/(1-f t^2),
   ! and the result is scaled by sqrt(f).
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: W
      VEC(REAL,3), IN :: c
      REAL, optional, IN :: eta
      MAT(REAL,:,:), allocatable :: NAx,NAy,NAz,DAx,DAy,DAz,DDx,DDy,DDz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac,fac,t2
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys

      ! Cartesian powers of the components
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)

      ! Rys roots and weights; one power more on each side than for 1/r_c
      n_roots = (self%a%l+self%b%l+4)/2
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c
      fac = ONE
      if (present(eta)) fac = eta/(p+eta)
      call create_(rys,n_roots)
      xx = fac*p*dot_(rpc,rpc)
      call get_weights_and_u_roots_(rys,xx)

      ! 1-D integrals, their left derivatives, and left-and-right derivatives
      call create_(NAx,self%a%l+2,self%b%l+2); call create_(NAy,self%a%l+2,self%b%l+2); call create_(NAz,self%a%l+2,self%b%l+2)
      call create_(DAx,self%a%l+1,self%b%l+2); call create_(DAy,self%a%l+1,self%b%l+2); call create_(DAz,self%a%l+1,self%b%l+2)
      call create_(DDx,self%a%l+1,self%b%l+1); call create_(DDy,self%a%l+1,self%b%l+1); call create_(DDz,self%a%l+1,self%b%l+1)
      W = ZERO
      do k = 1,n_roots
         t2 = rys%root(k)/(ONE+rys%root(k))
         uu = p*fac*t2/(ONE-fac*t2)
         wt = rys%weight(k)
         call make_nuclear_attraction_ints_(self,NAx,1,uu,c(1),self%a%l+1,self%b%l+1)
         call make_nuclear_attraction_ints_(self,NAy,2,uu,c(2),self%a%l+1,self%b%l+1)
         call make_nuclear_attraction_ints_(self,NAz,3,uu,c(3),self%a%l+1,self%b%l+1)
         call differentiate_(self,NAx,"left ",DAx)
         call differentiate_(self,NAy,"left ",DAy)
         call differentiate_(self,NAz,"left ",DAz)
         call differentiate_(self,DAx,"right",DDx)
         call differentiate_(self,DAy,"right",DDy)
         call differentiate_(self,DAz,"right",DDz)
         NAz = NAz*wt
         DDz = DDz*wt
         W = W+DDx(ax,bx)*NAy(ay,by)*NAz(az,bz)+NAx(ax,bx)*DDy(ay,by)*NAz(az,bz)+NAx(ax,bx)*NAy(ay,by)*DDz(az,bz)
      end do
      call destroy_(rys)
      call destroy_(DDz); call destroy_(DDy); call destroy_(DDx)
      call destroy_(DAz); call destroy_(DAy); call destroy_(DAx)
      call destroy_(NAz); call destroy_(NAy); call destroy_(NAx)
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/p
      W = prefac*sqrt(fac)*W
   end subroutine


!   make_normalized_DKH_ints(T,SOx,SOy,SOz,c,spherical) :: PURE
!   ! Calculate T, a cartesian or spherical block of integrals.
!      self :: IN
!      T,SOx,SOy,SOz :: MAT{REAL}, OUT
!      c :: VEC{REAL}(3), IN
!      spherical :: BIN, optional, IN
!
!      W,Wx,Wy,Wz :: MAT{REAL}@
!      is_spherical :: BIN
!
!      is_spherical = FALSE
!      if (present(spherical)) is_spherical = spherical
!
!      if (NOT is_spherical)then
!         .make_DKH_ints(T,SOx,SOy,SOz,c)
!      elseif ( .a.l<2 AND .b.l<2) then
!         .make_DKH_ints(T,SOx,SOy,SOz,c)
!      else
!         W.create(.a.n_comp,.b.n_comp)
!         Wx.create(.a.n_comp,.b.n_comp)
!         Wy.create(.a.n_comp,.b.n_comp)
!         Wz.create(.a.n_comp,.b.n_comp)
!         .make_DKH_ints(W,Wx,Wy,Wz,c)
!         .change_to_spherical(W,T)
!         .change_to_spherical(Wx,SOx)
!         .change_to_spherical(Wy,SOy)
!         .change_to_spherical(Wz,SOz)
!         Wz.destroy
!         Wy.destroy
!         Wx.destroy
!         W.destroy
!      end
!
!      T   = T   * .normalisation_factor
!      SOx = SOx * .normalisation_factor
!      SOy = SOy * .normalisation_factor
!      SOz = SOz * .normalisation_factor
!
!   end

!   make_DKH_ints(T,SOx,SOy,SOz,c)
!   ! Make the Douglas-Kroll-Hess spin orbit integrals.
!   ! "c" is a given nuclear center.
!      self :: INOUT
!      T,SOx,SOy,SOz :: MAT{REAL}, OUT
!      c :: VEC{REAL}(3), IN
!
!      NAx,NAy,NAz, AAx,AAy,AAz,BBx,BBy,BBz,ABx,ABy,ABz :: MAT{REAL}@
!      ax,ay,az,bx,by,bz :: VEC{INT}@
!      rpc,rab :: VEC{REAL}(3)
!      p,xx,uu,wt,prefac :: REAL
!      n_roots,k,fa,la,fb,lb :: INT
!      rys :: RYS@
!
!      ! Indexing array limits
!      fa = .a.first_gaussian; la = .a.last_gaussian
!      fb = .b.first_gaussian; lb = .b.last_gaussian
!
!      ax = GAUSSIAN_DATA::nx(fa:la); ay = GAUSSIAN_DATA::ny(fa:la); az = GAUSSIAN_DATA::nz(fa:la)
!      bx = GAUSSIAN_DATA::nx(fb:lb); by = GAUSSIAN_DATA::ny(fb:lb); bz = GAUSSIAN_DATA::nz(fb:lb)
!
!      ! 2D integrals
!!      NAx.create(.a.l+2,.b.l+3); NAy.create(.a.l+2,.b.l+3); NAz.create(.a.l+2,.b.l+3)
!      NAx.create(.a.l+2,.b.l+2); NAy.create(.a.l+2,.b.l+2); NAz.create(.a.l+2,.b.l+2)
!      AAx.create(.a.l+1,.b.l+2); AAy.create(.a.l+1,.b.l+2); AAz.create(.a.l+1,.b.l+2)
!      BBx.create(.a.l+2,.b.l+1); BBy.create(.a.l+2,.b.l+1); BBz.create(.a.l+2,.b.l+1)
!!      BBx.create(.a.l+1,.b.l+1); BBy.create(.a.l+1,.b.l+1); BBz.create(.a.l+1,.b.l+1)
!      ABx.create(.a.l+1,.b.l+1); ABy.create(.a.l+1,.b.l+1); ABz.create(.a.l+1,.b.l+1)
!
!      ! Roots and weights
!      n_roots = (.a.l+.b.l+4)/2
!      p = .a.exponent + .b.exponent
!      rpc = (.a.exponent*.a.position + .b.exponent*.b.position)/p - c
!      rys.create(n_roots)
!      xx = p*rpc.dot(rpc)
!      rys.get_weights_and_u_roots(xx)
!
!      ! Assemble the integrals
!      T = ZERO
!      SOx = ZERO; SOy = ZERO; SOz = ZERO
!
!      do k = 1,n_roots
!
!         uu = p*rys.root(k)
!         wt = rys.weight(k)
!
!         .make_nuclear_attraction_ints(NAx,1,uu,c(1),.a.l+1,.b.l+1)
!         .make_nuclear_attraction_ints(NAy,2,uu,c(2),.a.l+1,.b.l+1)
!         .make_nuclear_attraction_ints(NAz,3,uu,c(3),.a.l+1,.b.l+1)
!
!         .differentiate(NAx,"right",BBx)
!         .differentiate(NAy,"right",BBy)
!         .differentiate(NAz,"right",BBz)
!
!         .differentiate(NAx,"left ",AAx)
!         .differentiate(NAy,"left ",AAy)
!         .differentiate(NAz,"left ",AAz)
!
!         .differentiate(AAx,"right",ABx)
!         .differentiate(AAy,"right",ABy)
!         .differentiate(AAz,"right",ABz)
!
!         NAz = NAz*wt
!         AAz = AAz*wt
!         BBz = BBz*wt
!         ABz = ABz*wt
!

!               + NAx(ax,bx)*NAy(ay,by)*ABz(az,bz)
!

!                   - NAx(ax,bx)*BBy(ay,by)*AAz(az,bz)
!

!                   - AAx(ax,bx)*NAy(ay,by)*BBz(az,bz)
!

!                   - BBx(ax,bx)*AAy(ay,by)*NAz(az,bz)
!
!      end
!
!      ! Clean up
!      rys.destroy
!      ABz.destroy; ABy.destroy; ABx.destroy
!      BBz.destroy; BBy.destroy; BBx.destroy
!      AAz.destroy; AAy.destroy; AAx.destroy
!      NAz.destroy; NAy.destroy; NAx.destroy
!
!      ! Include prefactor
!      rab = .a.position - .b.position
!      prefac = exp(-.a.exponent*.b.exponent*rab.dot(rab)/p)*pi212/p
!      T = prefac*T
!      SOx = prefac*SOx; SOy = prefac*SOy; SOz = prefac*SOz
!
!   end

!   make_normalized_DK3_ppZpp_ints(ppZpp,c,spherical)
!   ! Calculate T, a cartesian or spherical block of integrals.
!      ppZpp :: MAT{REAL}
!      c :: VEC{REAL}(3)
!      spherical :: BIN, optional
!
!      W :: MAT{REAL}@
!      is_spherical :: BIN
!
!      is_spherical = FALSE
!      if (present(spherical)) is_spherical = spherical
!
!      if (NOT is_spherical) then
!         .make_DK3_ppZpp_ints(ppZpp,c)
!      elseif (.a.l<2 AND .b.l<2) then
!         .make_DK3_ppZpp_ints(ppZpp,c)
!      else
!         W.create(.a.n_comp,.b.n_comp)
!         .make_DK3_ppZpp_ints(W,c)
!         .change_to_spherical(W,ppZpp)
!         W.destroy
!      end
!
!      ppZpp = ppZpp * .normalisation_factor
!
!   end

!   make_DK3_ppZpp_ints(ppZpp,c)
!   ! Make the Douglas-Kroll-Hess p2Zp2 integrals for teh DK3 hamiltonian
!   ! "c" is a given nuclear center.
!      self :: INOUT
!      ppZpp :: MAT{REAL}, OUT
!      c :: VEC{REAL}(3), IN
!
!      NAx,NAy,NAz, AAx,AAy,AAz,BBx,BBy,BBz,ABx,ABy,ABz :: MAT{REAL}@
!      AB2x,AB2y,AB2z,A2x,A2y,A2z,B2x,B2y,B2z :: MAT{REAL}@
!      ax,ay,az,bx,by,bz :: VEC{INT}@
!      rpc,rab :: VEC{REAL}(3)
!      p,xx,uu,wt,prefac :: REAL
!      n_roots,k,fa,la,fb,lb :: INT
!      rys :: RYS@
!
!      ! Indexing array limits
!      fa = .a.first_gaussian; la = .a.last_gaussian
!      fb = .b.first_gaussian; lb = .b.last_gaussian
!
!      ax = GAUSSIAN_DATA::nx(fa:la); ay = GAUSSIAN_DATA::ny(fa:la); az = GAUSSIAN_DATA::nz(fa:la)
!      bx = GAUSSIAN_DATA::nx(fb:lb); by = GAUSSIAN_DATA::ny(fb:lb); bz = GAUSSIAN_DATA::nz(fb:lb)
!
!      ! 2D integrals
!      ! the dimension ???
!      NAx.create(.a.l+3,.b.l+3); NAy.create(.a.l+3,.b.l+3); NAz.create(.a.l+3,.b.l+3)
!      AAx.create(.a.l+2,.b.l+3); AAy.create(.a.l+2,.b.l+3); AAz.create(.a.l+2,.b.l+3)
!      A2x.create(.a.l+1,.b.l+3); A2y.create(.a.l+1,.b.l+3); A2z.create(.a.l+1,.b.l+3)
!      BBx.create(.a.l+3,.b.l+2); BBy.create(.a.l+3,.b.l+2); BBz.create(.a.l+3,.b.l+2)
!      B2x.create(.a.l+3,.b.l+1); B2y.create(.a.l+3,.b.l+1); B2z.create(.a.l+3,.b.l+1)
!      ABx.create(.a.l+2,.b.l+1); ABy.create(.a.l+2,.b.l+1); ABz.create(.a.l+2,.b.l+1)
!      AB2x.create(.a.l+1,.b.l+1); AB2y.create(.a.l+1,.b.l+1); AB2z.create(.a.l+1,.b.l+1)
!
!      ! Roots and weights
!!      n_roots = (.a.l+.b.l+8)/2
!      n_roots = (.a.l+.b.l+6)/2
!      p = .a.exponent + .b.exponent
!      rpc = (.a.exponent*.a.position + .b.exponent*.b.position)/p - c
!      rys.create(n_roots)
!      xx = p*rpc.dot(rpc)
!      rys.get_weights_and_u_roots(xx)
!
!      ! Assemble the integrals
!      ppZpp = ZERO
!!      SOx = ZERO; SOy = ZERO; SOz = ZERO
!
!      do k = 1,n_roots
!
!         uu = p*rys.root(k)
!         wt = rys.weight(k)
!
!         .make_nuclear_attraction_ints(NAx,1,uu,c(1),.a.l+2,.b.l+2)
!         .make_nuclear_attraction_ints(NAy,2,uu,c(2),.a.l+2,.b.l+2)
!         .make_nuclear_attraction_ints(NAz,3,uu,c(3),.a.l+2,.b.l+2)
!
!
!         .differentiate(NAx,"left ",AAx)
!         .differentiate(NAy,"left ",AAy)
!         .differentiate(NAz,"left ",AAz)
!         .differentiate(AAx,"left ",A2x)
!         .differentiate(AAy,"left ",A2y)
!         .differentiate(AAz,"left ",A2z)
!
!         .differentiate(NAx,"right",BBx)
!         .differentiate(NAy,"right",BBy)
!         .differentiate(NAz,"right",BBz)
!         .differentiate(BBx,"right",B2x)
!         .differentiate(BBy,"right",B2y)
!         .differentiate(BBz,"right",B2z)
!
!         .differentiate(B2x,"left",ABx)
!         .differentiate(B2y,"left",ABy)
!         .differentiate(B2z,"left",ABz)
!
!         .differentiate(ABx,"left",AB2x)
!         .differentiate(ABy,"left",AB2y)
!         .differentiate(ABz,"left",AB2z)
!
!         NAz = NAz*wt
!         A2z = A2z*wt
!         B2z = B2z*wt
!!         AAz = AAz*wt
!!         BBz = BBz*wt
!         AB2z = AB2z*wt
!

!               + NAx(ax,bx)*NAy(ay,by)*AB2z(az,bz)
!
!! I leave out the SO ints at the moment

!!                   - NAx(ax,bx)*BBy(ay,by)*AAz(az,bz)
!

!!                   - AAx(ax,bx)*NAy(ay,by)*BBz(az,bz)
!

!!                   - BBx(ax,bx)*AAy(ay,by)*NAz(az,bz)
!
!      end
!
!      ! Clean up
!      rys.destroy
!      ABz.destroy; ABy.destroy; ABx.destroy
!      AB2z.destroy; AB2y.destroy; AB2x.destroy
!      BBz.destroy; BBy.destroy; BBx.destroy
!      AAz.destroy; AAy.destroy; AAx.destroy
!      B2z.destroy; B2y.destroy; B2x.destroy
!      A2z.destroy; A2y.destroy; A2x.destroy
!      NAz.destroy; NAy.destroy; NAx.destroy
!
!      ! Include prefactor
!      rab = .a.position - .b.position
!      prefac = exp(-.a.exponent*.b.exponent*rab.dot(rab)/p)*pi212/p
!      ppZpp = prefac*ppZpp
!!      SOx = prefac*SOx; SOy = prefac*SOy; SOz = prefac*SOz
!
!   end

   PURE subroutine make_spin_orbit_ints(self,Lx,Ly,Lz,c)
   ! Make the spin orbit integral matrices "Lx", "Ly", and "Lz" around 
   ! nucleus "c".  This integrals is for: 
   !    (r - r_c) x nabla / |r - r_c|^3, 
   ! with no minus sign on nabla. Uses Rys decomposition of 1/r_c and
   ! Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Lx,Ly,Lz
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: NAx,NAy,NAz
      MAT(REAL,:,:), allocatable :: AAx,AAy,AAz
      MAT(REAL,:,:), allocatable :: BBx,BBy,BBz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(NAx,self%a%l+2,self%b%l+2); call create_(NAy,self%a%l+2,self%b%l+2); call create_(NAz,self%a%l+2,self%b%l+2)
      call create_(AAx,self%a%l+1,self%b%l+1); call create_(AAy,self%a%l+1,self%b%l+1); call create_(AAz,self%a%l+1,self%b%l+1)
      call create_(BBx,self%a%l+1,self%b%l+1); call create_(BBy,self%a%l+1,self%b%l+1); call create_(BBz,self%a%l+1,self%b%l+1)
      n_roots = (self%a%l+self%b%l+3)/2
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c
      call create_(rys,n_roots)
      xx = p*dot_(rpc,rpc)
      call get_weights_and_u_roots_(rys,xx)
      Lx = ZERO
      Ly = ZERO
      Lz = ZERO
      do k = 1,n_roots
         uu = p*rys%root(k)
         wt = rys%weight(k)
         call make_nuclear_attraction_ints_(self,NAx,1,uu,c(1),self%a%l+1,self%b%l+1)
         call make_nuclear_attraction_ints_(self,NAy,2,uu,c(2),self%a%l+1,self%b%l+1)
         call make_nuclear_attraction_ints_(self,NAz,3,uu,c(3),self%a%l+1,self%b%l+1)
         NAz = NAz*wt
         call differentiate_(self,NAx,"right",BBx)  ! with respect to electron coordinates !
         call differentiate_(self,NAy,"right",BBy)
         call differentiate_(self,NAz,"right",BBz)
         call differentiate_(self,NAx,"left ",AAx)  ! with respect to electron coordinates !
         call differentiate_(self,NAy,"left ",AAy)
         call differentiate_(self,NAz,"left ",AAz)
         Lx = Lx+NAx(ax,bx)*(AAy(ay,by)*BBz(az,bz)-BBy(ay,by)*AAz(az,bz))
         Ly = Ly+NAy(ay,by)*(AAz(az,bz)*BBx(ax,bx)-BBz(az,bz)*AAx(ax,bx))
         Lz = Lz+NAz(az,bz)*(AAx(ax,bx)*BBy(ay,by)-BBx(ax,bx)*AAy(ay,by))
      end do
      call destroy_(rys)
      call destroy_(BBz); call destroy_(BBy); call destroy_(BBx)
      call destroy_(AAz); call destroy_(AAy); call destroy_(AAx)
      call destroy_(NAz); call destroy_(NAy); call destroy_(NAx)
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/p
      Lx = prefac*Lx
      Ly = prefac*Ly
      Lz = prefac*Lz
   end subroutine

   PURE subroutine make_E_field_ints_0(self,Ex,Ey,Ez,c)
   ! Make the electric field integral matrices "Ei" at position "c".
   ! If present, Uses Rys decomposition of 1/r_c and Gauss-Hermite
   ! quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ex,Ey,Ez
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: Sx,Sy,Sz
      MAT(REAL,:,:), allocatable :: Fx,Fy,Fz
      VEC(INT,:), allocatable :: ax,ay,az
      VEC(INT,:), allocatable :: bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys
      fa = self%a%first_gaussian
      la = self%a%last_gaussian
      fb = self%b%first_gaussian
      lb = self%b%last_gaussian
      ax = nx(fa:la)
      ay = ny(fa:la)
      az = nz(fa:la)
      bx = nx(fb:lb)
      by = ny(fb:lb)
      bz = nz(fb:lb)
      call create_(Sx,self%a%l+1,self%b%l+1); call create_(Sy,self%a%l+1,self%b%l+1); call create_(Sz,self%a%l+1,self%b%l+1)
      call create_(Fx,self%a%l+1,self%b%l+1); call create_(Fy,self%a%l+1,self%b%l+1); call create_(Fz,self%a%l+1,self%b%l+1)
      n_roots = (self%a%l+self%b%l+3)/2
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c
      xx = p*dot_(rpc,rpc)
      call create_(rys,n_roots)
      call get_weights_and_u_roots_(rys,xx)
      Ex = ZERO
      Ey = ZERO
      Ez = ZERO
      do k = 1,n_roots
         uu = p*rys%root(k)
         wt = rys%weight(k)
         call make_E_field_ints_(self,Sx,Fx,1,uu,c(1))
         call make_E_field_ints_(self,Sy,Fy,2,uu,c(2))
         call make_E_field_ints_(self,Sz,Fz,3,uu,c(3))
         Sz = Sz*wt
         Fz = Fz*wt
         Ex = Ex+Fx(ax,bx)*Sy(ay,by)*Sz(az,bz)
         Ey = Ey+Sx(ax,bx)*Fy(ay,by)*Sz(az,bz)
         Ez = Ez+Sx(ax,bx)*Sy(ay,by)*Fz(az,bz)
      end do
      call destroy_(rys)
      call destroy_(Fz); call destroy_(Fy); call destroy_(Fx)
      call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/p
      Ex = prefac*Ex
      Ey = prefac*Ey
      Ez = prefac*Ez
   end subroutine

   PURE subroutine make_E_field_ints_1(self,Sx,Fx,x,uu,cx,max_a,max_b)
   ! Make the 1-D nuclear attraction integrals "Sx", and the electric field
   ! integrals "Fx" for component "x", for a particular Rys-gaussian
   ! with exponent "uu", used in the decomposition of (c-r)/|c-r|^3
   ! centerd at position "cx". Uses Gauss-Hermite quadrature.
   ! If present, "max_a" and "max_b" indicate the l value to be used
   ! for gaussian a or b.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Sx,Fx
      INT, IN :: x
      REAL, IN :: uu,cx
      INT, optional, IN :: max_a,max_b
      REAL :: p,t,ax,bx,r0,ff
      REAL :: pt,wt,ra,rb,ru
      INT :: l_a,l_b,a,b,i,k
      l_a = self%a%l; if (present(max_a)) l_a = max_a
      l_b = self%b%l; if (present(max_b)) l_b = max_b
      p = self%a%exponent+self%b%exponent+uu
      t = sqrt(p)
      ax = self%a%position(x)
      bx = self%b%position(x)
      r0 = (self%a%exponent*ax+self%b%exponent*bx+uu*cx)/p
      Sx = ZERO
      Fx = ZERO
      do a = 1,l_a+1
         do b = 1,l_b+1
            i = (a+b+1)/2  ! should be max((a+b)/2,1) ?
            do k = first_pt(i),last_pt(i)
               pt = gauss_hermite_pt(k)/t+r0
               ra = pt-ax
               rb = pt-bx
               ru = TWO*uu*(cx-pt)
               ff = ONE
               if (a>1) ff = ff*ra**(a-1)
               if (b>1) ff = ff*rb**(b-1)
               wt = gauss_hermite_wt(k)
               Sx(a,b) = Sx(a,b)+ff*wt
               Fx(a,b) = Fx(a,b)+(ru*ff)*wt
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_E_gradient_ints_0(self,Exx,Eyy,Ezz,Exy,Exz,Eyz,c)
   ! Make the electric field gradient integral matrices "Ei" at position "c".
   ! This does not contain the density contribution from the delta function.
   ! Uses Rys decomposition of 1/r_c and Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Exx,Eyy,Ezz
      MAT(REAL,:,:), OUT :: Exy,Exz,Eyz
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: Sx,Sy,Sz,Fx,Fy,Fz,Gx,Gy,Gz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx,self%a%l+1,self%b%l+1); call create_(Sy,self%a%l+1,self%b%l+1); call create_(Sz,self%a%l+1,self%b%l+1)
      call create_(Fx,self%a%l+1,self%b%l+1); call create_(Fy,self%a%l+1,self%b%l+1); call create_(Fz,self%a%l+1,self%b%l+1)
      call create_(Gx,self%a%l+1,self%b%l+1); call create_(Gy,self%a%l+1,self%b%l+1); call create_(Gz,self%a%l+1,self%b%l+1)
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c
      n_roots = (self%a%l+self%b%l+4)/2
      call create_(rys,n_roots)
      xx = p*dot_(rpc,rpc)
      call get_weights_and_u_roots_(rys,xx)
      Exx = ZERO; Eyy = ZERO; Ezz = ZERO
      Exy = ZERO; Exz = ZERO; Eyz = ZERO
      do k = 1,n_roots
         uu = p*rys%root(k)
         wt = rys%weight(k)
         call make_E_gradient_ints_(self,Sx,Fx,Gx,1,uu,c(1))
         call make_E_gradient_ints_(self,Sy,Fy,Gy,2,uu,c(2))
         call make_E_gradient_ints_(self,Sz,Fz,Gz,3,uu,c(3))
         Sz = Sz*wt
         Fz = Fz*wt
         Gz = Gz*wt
         Exx = Exx+Gx(ax,bx)*Sy(ay,by)*Sz(az,bz)
         Eyy = Eyy+Sx(ax,bx)*Gy(ay,by)*Sz(az,bz)
         Ezz = Ezz+Sx(ax,bx)*Sy(ay,by)*Gz(az,bz)
         Exy = Exy-Fx(ax,bx)*Fy(ay,by)*Sz(az,bz)  ! cancel the extra minus sign on Fy
         Exz = Exz-Fx(ax,bx)*Sy(ay,by)*Fz(az,bz)
         Eyz = Eyz-Sx(ax,bx)*Fy(ay,by)*Fz(az,bz)
      end do
      call destroy_(rys)
      call destroy_(Gz); call destroy_(Gy); call destroy_(Gx)
      call destroy_(Fz); call destroy_(Fy); call destroy_(Fx)
      call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/p
      Exx = prefac*Exx; Eyy = prefac*Eyy; Ezz = prefac*Ezz
      Exy = prefac*Exy; Exz = prefac*Exz; Eyz = prefac*Eyz
   end subroutine

   PURE subroutine make_E_gradient_ints_1(self,Sx,Fx,Gx,x,uu,cx,max_a,max_b)
   ! Make the 1-D overlap integrals "Sx", and the electric field
   ! integrals "Fx", and the electric field gradient integral "Gx" for
   ! component "x", for a particular Rys-gaussian with exponent "uu",
   ! used in the decomposition of (c-r)/|c-r|^3 centerd at position
   ! "cx". Uses Gauss-Hermite quadrature.  If present, "max_a" and
   ! "max_b" indicate the l value to be used for gaussian a or b.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Sx,Fx,Gx
      INT, IN :: x
      REAL, IN :: uu,cx
      INT, optional, IN :: max_a,max_b
      REAL :: p,t,ax,bx,r0,ff
      REAL :: pt,ra,rb,ru,rg
      INT :: l_a,l_b,a,b,i,k
      l_a = self%a%l; if (present(max_a)) l_a = max_a
      l_b = self%b%l; if (present(max_b)) l_b = max_b
      p = self%a%exponent+self%b%exponent+uu
      t = sqrt(p)
      ax = self%a%position(x)
      bx = self%b%position(x)
      r0 = (self%a%exponent*ax+self%b%exponent*bx+uu*cx)/p
      Sx = ZERO; Fx = ZERO; Gx = ZERO
      do a = 1,l_a+1
         do b = 1,l_b+1
            i = (a+b+2)/2  ! should be max((a+b)/2,1) ?
            do k = first_pt(i),last_pt(i)
               pt = gauss_hermite_pt(k)/t+r0
               ra = pt-ax
               rb = pt-bx
               ru = TWO*uu*(cx-pt)  ! this is Ex = - d/dcx phi(c)
               rg = TWO*uu*(ONE-ru*(cx-pt))  ! this is dEx/dcx
               ff = gauss_hermite_wt(k)
               if (a>1) ff = ff*ra**(a-1)
               if (b>1) ff = ff*rb**(b-1)  ! this is \int dx g(a;ax) g(b;bx) e^(-uu*(cx-r)^2)
               Sx(a,b) = Sx(a,b)+ff  ! integrand = 1
               Fx(a,b) = Fx(a,b)+ru*ff  ! integrand = (cx-r)/|c-r|^3
               Gx(a,b) = Gx(a,b)+rg*ff  ! integrand = 3*(cx-r)^2/|c-r|^5 - 1/|c-r|^3
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_spin_orbit_B_ints(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz,c,origin)
   ! Make gauge modified (B field) spin orbit integral matrices "Qij" around
   ! nucleus "c".  using gauge origin "origin".  The integrals are:
   !    Qij(a,b) = <a| (r-o)_i (r-c)_j / |r-c|^3 |b>
   ! Uses Rys decomposition of 1/|r-c| and Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Qxx,Qxy,Qxz
      MAT(REAL,:,:), OUT :: Qyx,Qyy,Qyz
      MAT(REAL,:,:), OUT :: Qzx,Qzy,Qzz
      VEC(REAL,3), IN :: c,origin
      MAT(REAL,:,:), allocatable :: Sx,Sy,Sz
      MAT(REAL,:,:), allocatable :: Dx,Dy,Dz
      MAT(REAL,:,:), allocatable :: Fx,Fy,Fz
      MAT(REAL,:,:), allocatable :: Qx,Qy,Qz
      VEC(INT,:), allocatable :: ax,ay,az
      VEC(INT,:), allocatable :: bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx,self%a%l+1,self%b%l+1); call create_(Sy,self%a%l+1,self%b%l+1); call create_(Sz,self%a%l+1,self%b%l+1)
      call create_(Dx,self%a%l+1,self%b%l+1); call create_(Dy,self%a%l+1,self%b%l+1); call create_(Dz,self%a%l+1,self%b%l+1)
      call create_(Fx,self%a%l+1,self%b%l+1); call create_(Fy,self%a%l+1,self%b%l+1); call create_(Fz,self%a%l+1,self%b%l+1)
      call create_(Qx,self%a%l+1,self%b%l+1); call create_(Qy,self%a%l+1,self%b%l+1); call create_(Qz,self%a%l+1,self%b%l+1)
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c
      n_roots = (self%a%l+self%b%l+3)/2
      call create_(rys,n_roots)
      xx = p*dot_(rpc,rpc)
      call get_weights_and_u_roots_(rys,xx)
      Qxx = ZERO; Qyx = ZERO; Qzx = ZERO
      Qxy = ZERO; Qyy = ZERO; Qzy = ZERO
      Qxz = ZERO; Qyz = ZERO; Qzz = ZERO
      do k = 1,n_roots
         uu = p*rys%root(k)
         wt = rys%weight(k)
         call make_Q_field_ints_(self,Sx,Dx,Fx,Qx,1,uu,c(1),origin(1))
         call make_Q_field_ints_(self,Sy,Dy,Fy,Qy,2,uu,c(2),origin(2))
         call make_Q_field_ints_(self,Sz,Dz,Fz,Qz,3,uu,c(3),origin(3))
         Sz = Sz*wt
         Dz = Dz*wt
         Fz = Fz*wt
         Qz = Qz*wt
         Qxx = Qxx+Qx(ax,bx)*Sy(ay,by)*Sz(az,bz)
         Qyx = Qyx+Fx(ax,bx)*Dy(ay,by)*Sz(az,bz)
         Qzx = Qzx+Fx(ax,bx)*Sy(ay,by)*Dz(az,bz)
         Qxy = Qxy+Dx(ax,bx)*Fy(ay,by)*Sz(az,bz)
         Qyy = Qyy+Sx(ax,bx)*Qy(ay,by)*Sz(az,bz)
         Qzy = Qzy+Sx(ax,bx)*Fy(ay,by)*Dz(az,bz)
         Qxz = Qxz+Dx(ax,bx)*Sy(ay,by)*Fz(az,bz)
         Qyz = Qyz+Sx(ax,bx)*Dy(ay,by)*Fz(az,bz)
         Qzz = Qzz+Sx(ax,bx)*Sy(ay,by)*Qz(az,bz)
      end do
      call destroy_(rys)
      call destroy_(Qz); call destroy_(Qy); call destroy_(Qx)
      call destroy_(Fz); call destroy_(Fy); call destroy_(Fx)
      call destroy_(Dz); call destroy_(Dy); call destroy_(Dx)
      call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/p
      Qxx = prefac*Qxx; Qyx = prefac*Qyx; Qzx = prefac*Qzx
      Qxy = prefac*Qxy; Qyy = prefac*Qyy; Qzy = prefac*Qzy
      Qxz = prefac*Qxz; Qyz = prefac*Qyz; Qzz = prefac*Qzz
   end subroutine

   PURE subroutine make_Q_field_ints(self,Sx,Dx,Fx,Qx,x,uu,cx,ox,max_a,max_b)
   ! Make the 1-D overlap integrals "Sx", dipole integrals "Dx",
   ! electric field integrals "Fx", and the dipole-electric field
   ! integrals "Qx" for component "x". "uu" is the Rys-gaussian
   ! exponent used in the decomposition of (c-r)/|c-r|^3 centerd at
   ! position "cx".  "ox" is the origin for the dipole terms. If
   ! present, "max_a" and "max_b" override the l value for gaussian a
   ! or b. Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Sx,Dx,Fx,Qx
      INT, IN :: x
      REAL, IN :: uu,cx,ox
      INT, optional, IN :: max_a,max_b
      REAL :: p,t,ax,bx,r0,wt
      REAL :: pt,ra,rb,ru,rd
      INT :: l_a,l_b,a,b,i,k
      l_a = self%a%l; if (present(max_a)) l_a = max_a
      l_b = self%b%l; if (present(max_b)) l_b = max_b
      p = self%a%exponent+self%b%exponent+uu
      t = sqrt(p)
      ax = self%a%position(x)
      bx = self%b%position(x)
      r0 = (self%a%exponent*ax+self%b%exponent*bx+uu*cx)/p
      Sx = ZERO; Dx = ZERO; Fx = ZERO; Qx = ZERO
      do a = 1,l_a+1
         do b = 1,l_b+1
            i = (a+b+2)/2  ! should be max((a+b+1)/2,1) ?
            do k = first_pt(i),last_pt(i)
               wt = gauss_hermite_wt(k)
               pt = gauss_hermite_pt(k)/t+r0
               ra = pt-ax
               rb = pt-bx
               ru = TWO*uu*(cx-pt)
               rd = pt-ox
               if (a>1) wt = wt*ra**(a-1)
               if (b>1) wt = wt*rb**(b-1)
               Sx(a,b) = Sx(a,b)+wt
               Dx(a,b) = Dx(a,b)+rd*wt
               Fx(a,b) = Fx(a,b)+ru*wt
               Qx(a,b) = Qx(a,b)+ru*rd*wt
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_L_ints(self,Lx,Ly,Lz,origin)
   ! Make the angular momentum integral matrices "Lx", "Ly", and "Lz".
   ! The integrals are for (r x nabla) with *no* minus sign on nabla.
   ! "origin" is the gauge origin for the integrals.
   ! Uses Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Lx,Ly,Lz
      VEC(REAL,3), IN :: origin
      MAT(REAL,:,:), allocatable :: Sx,Sy,Sz
      MAT(REAL,:,:), allocatable :: Rx,Ry,Rz
      MAT(REAL,:,:), allocatable :: Dx,Dy,Dz
      VEC(INT,:), allocatable :: ax,ay,az
      VEC(INT,:), allocatable :: bx,by,bz
      VEC(REAL,3) :: rab
      REAL :: p,prefac
      INT :: fa,la,fb,lb
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx,self%a%l+1,self%b%l+2); call create_(Sy,self%a%l+1,self%b%l+2); call create_(Sz,self%a%l+1,self%b%l+2)
      call create_(Rx,self%a%l+1,self%b%l+2); call create_(Ry,self%a%l+1,self%b%l+2); call create_(Rz,self%a%l+1,self%b%l+2)
      call create_(Dx,self%a%l+1,self%b%l+1); call create_(Dy,self%a%l+1,self%b%l+1); call create_(Dz,self%a%l+1,self%b%l+1)
      call make_dipole_ints_(self,Sx,Rx,1,origin(1),max_b=self%b%l+1)
      call make_dipole_ints_(self,Sy,Ry,2,origin(2),max_b=self%b%l+1)
      call make_dipole_ints_(self,Sz,Rz,3,origin(3),max_b=self%b%l+1)
      call differentiate_(self,Sx,"right",Dx)
      call differentiate_(self,Sy,"right",Dy)
      call differentiate_(self,Sz,"right",Dz)
      Lx = Sx(ax,bx)*(Ry(ay,by)*Dz(az,bz)-Dy(ay,by)*Rz(az,bz))
      Ly = Sy(ay,by)*(Rz(az,bz)*Dx(ax,bx)-Dz(az,bz)*Rx(ax,bx))
      Lz = Sz(az,bz)*(Rx(ax,bx)*Dy(ay,by)-Dx(ax,bx)*Ry(ay,by))
      call destroy_(Dz); call destroy_(Dy); call destroy_(Dx)
      call destroy_(Rz); call destroy_(Ry); call destroy_(Rx)
      call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
      p = self%a%exponent+self%b%exponent
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)/(p*sqrt(p))
      Lx = prefac*Lx
      Ly = prefac*Ly
      Lz = prefac*Lz
   end subroutine

   PURE subroutine differentiate(self,I,index,D)
   ! Differentiate a gaussian-integral matrix "I" with respect to the
   ! *electron* coordinate on basis function "index", which can be "left"
   ! or "right". Place the result in "D".
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), IN :: I
      STR(len=*), IN :: index
      MAT(REAL,:,:), OUT :: D
      INT :: d_a,d_b,a,b
      REAL :: a2,b2
      d_a = ubound(D,1); d_b = ubound(D,2)
      D = ZERO
      select case (index)

      case ("l","left")
         ENSURE(ubound(I,1)>1,"GAUSSIAN2:differentiate ... I array too small to differentiate")
         ENSURE(ubound(I,1)>d_a,"GAUSSIAN2:differentiate ... I and D arrays are incompatible")
         ENSURE(ubound(I,2)>=d_b,"GAUSSIAN2:differentiate ... I and D arrays are incompatible")
         a2 = TWO*self%a%exponent
         D(1,:) = -a2*I(2,1:d_b)
         do a = 2,d_a
            D(a,:) = -a2*I(a+1,1:d_b)+(a-1)*I(a-1,1:d_b)
         end do

      case ("r","right")
         ENSURE(ubound(I,2)>1,"GAUSSIAN2:differentiate ... I array too small to differentiate")
         ENSURE(ubound(I,2)>d_b,"GAUSSIAN2:differentiate ... I and D arrays are incompatible")
         ENSURE(ubound(I,1)>=d_a,"GAUSSIAN2:differentiate ... I and D arrays are incompatible")
         b2 = TWO*self%b%exponent
         D(:,1) = -b2*I(1:d_a,2)
         do b = 2,d_b
            D(:,b) = -b2*I(1:d_a,b+1)+(b-1)*I(1:d_a,b-1)
         end do
      end select
   end subroutine

   PURE subroutine make_magnetic_jp_ints(self,Jx,Jy,Jz,c)
   ! Make the magnetic Jp integrals "Ji", evaluated at point "c",
   !    <a|J(c)|b> = \int dr [g_a grad g_b - g_b grad g_a] x (c - r)/ |c - r|^3
   ! These are used to get the magnetic field from the paramegnetic current
   ! density,
   !      B(c) = ( mu_o / 4 pi ) \int dr J(r) x (c - r)/|c - r|^3
   ! Note: in atomic units mu_o = 4 pi / c^2.
   ! Note: prefactor *not* inlcuded in these integrals
   ! Uses a double quadrature based on Rys method and Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Jx,Jy,Jz
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: Sx00,Sy00,Sz00
      MAT(REAL,:,:), allocatable :: Sx10,Sy10,Sz10
      MAT(REAL,:,:), allocatable :: Sx01,Sy01,Sz01
      MAT(REAL,:,:), allocatable :: Sxmm,Symm,Szmm
      MAT(REAL,:,:), allocatable :: Fx00,Fy00,Fz00
      VEC(INT,:), allocatable :: ax,ay,az
      VEC(INT,:), allocatable :: bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys

      ! Gaussian function limits
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian

      ! Guassian powers
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)

      ! Allocate arrays
      call create_(Sx00,self%a%l+2,self%b%l+2); call create_(Sy00,self%a%l+2,self%b%l+2); call create_(Sz00,self%a%l+2,self%b%l+2)
      call create_(Sx10,self%a%l+1,self%b%l+1); call create_(Sy10,self%a%l+1,self%b%l+1); call create_(Sz10,self%a%l+1,self%b%l+1)
      call create_(Sx01,self%a%l+1,self%b%l+1); call create_(Sy01,self%a%l+1,self%b%l+1); call create_(Sz01,self%a%l+1,self%b%l+1)
      call create_(Sxmm,self%a%l+1,self%b%l+1); call create_(Symm,self%a%l+1,self%b%l+1); call create_(Szmm,self%a%l+1,self%b%l+1)
      call create_(Fx00,self%a%l+2,self%b%l+2); call create_(Fy00,self%a%l+2,self%b%l+2); call create_(Fz00,self%a%l+2,self%b%l+2)

      ! Gaussian product center
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c

      ! Get no of roots
      n_roots = (self%a%l+self%b%l+4)/2
      call create_(rys,n_roots)
      xx = p*dot_(rpc,rpc)
      call get_weights_and_u_roots_(rys,xx)

      ! Initialize
      Jx = ZERO
      Jy = ZERO
      Jz = ZERO

      ! Loop over roots
      do k = 1,n_roots

         ! Roots and weights
         uu = p*rys%root(k)
         wt = rys%weight(k)

         ! Get overlap and E-Field integrals
         call make_E_field_ints_(self,Sx00,Fx00,1,uu,c(1),self%a%l+1,self%b%l+1)
         call make_E_field_ints_(self,Sy00,Fy00,2,uu,c(2),self%a%l+1,self%b%l+1)
         call make_E_field_ints_(self,Sz00,Fz00,3,uu,c(3),self%a%l+1,self%b%l+1)

         ! Differentiate left/right
         call differentiate_(self,Sx00,"left",Sx10); call differentiate_(self,Sx00,"right",Sx01)
         call differentiate_(self,Sy00,"left",Sy10); call differentiate_(self,Sy00,"right",Sy01)
         call differentiate_(self,Sz00,"left",Sz10); call differentiate_(self,Sz00,"right",Sz01)

         ! Subtract
         Sxmm = Sx01-Sx10
         Symm = Sy01-Sy10
         Szmm = Sz01-Sz10

         ! Fold in weights
         Sz00 = Sz00*wt
         Fz00 = Fz00*wt
         Szmm = Szmm*wt

         ! Evaluate
         Jx = Jx+Sx00(ax,bx)*(Symm(ay,by)*Fz00(az,bz)-Fy00(ay,by)*Szmm(az,bz))
         Jy = Jy+Sy00(ay,by)*(Szmm(az,bz)*Fx00(ax,bx)-Fz00(az,bz)*Sxmm(ax,bx))
         Jz = Jz+Sz00(az,bz)*(Sxmm(ax,bx)*Fy00(ay,by)-Fx00(ax,bx)*Symm(ay,by))
      end do

      ! Clean
      call destroy_(rys)
      call destroy_(Fz00); call destroy_(Fy00); call destroy_(Fx00)
      call destroy_(Szmm); call destroy_(Symm); call destroy_(Sxmm)
      call destroy_(Sz01); call destroy_(Sy01); call destroy_(Sx01)
      call destroy_(Sz10); call destroy_(Sy10); call destroy_(Sx10)
      call destroy_(Sz00); call destroy_(Sy00); call destroy_(Sx00)

      ! Add prefactor
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/p
      Jx = prefac*Jx
      Jy = prefac*Jy
      Jz = prefac*Jz
   end subroutine

   PURE subroutine make_magnetic_S_ints(self,M,c)
   ! Make the magnetic spin density integrals "Mij", evaluated at point "c",
   !    <a|Mij(c)|b> = \int dr grad_i ( g_a(r) g_b(r) ) (c - r)_j/ |c - r|^3
   !                 = M(a,b,i,j)
   ! These are used to get the magnetic field from the spin density,
   !      B(c) = -(g mu_b mu_o / 4 pi)
   !             \int dr curl rho_spin(r) x (c - r)/|c - r|3
   ! Note: in atomic units mu_o = 4 pi / c^2.
   ! Uses a double quadrature based on Rys method and Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: M
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: Sx00,Sy00,Sz00
      MAT(REAL,:,:), allocatable :: Sx10,Sy10,Sz10
      MAT(REAL,:,:), allocatable :: Sx01,Sy01,Sz01
      MAT(REAL,:,:), allocatable :: Sxpp,Sypp,Szpp
      MAT(REAL,:,:), allocatable :: Fx00,Fy00,Fz00
      MAT(REAL,:,:), allocatable :: Fx10,Fy10,Fz10
      MAT(REAL,:,:), allocatable :: Fx01,Fy01,Fz01
      MAT(REAL,:,:), allocatable :: Fxpp,Fypp,Fzpp
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys

      ! Gaussian function limits
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)

      ! 2D integrals
      call create_(Sx00,self%a%l+2,self%b%l+2); call create_(Sy00,self%a%l+2,self%b%l+2); call create_(Sz00,self%a%l+2,self%b%l+2)
      call create_(Sx10,self%a%l+1,self%b%l+1); call create_(Sy10,self%a%l+1,self%b%l+1); call create_(Sz10,self%a%l+1,self%b%l+1)
      call create_(Sx01,self%a%l+1,self%b%l+1); call create_(Sy01,self%a%l+1,self%b%l+1); call create_(Sz01,self%a%l+1,self%b%l+1)
      call create_(Sxpp,self%a%l+1,self%b%l+1); call create_(Sypp,self%a%l+1,self%b%l+1); call create_(Szpp,self%a%l+1,self%b%l+1)
      call create_(Fx00,self%a%l+2,self%b%l+2); call create_(Fy00,self%a%l+2,self%b%l+2); call create_(Fz00,self%a%l+2,self%b%l+2)
      call create_(Fx10,self%a%l+1,self%b%l+1); call create_(Fy10,self%a%l+1,self%b%l+1); call create_(Fz10,self%a%l+1,self%b%l+1)
      call create_(Fx01,self%a%l+1,self%b%l+1); call create_(Fy01,self%a%l+1,self%b%l+1); call create_(Fz01,self%a%l+1,self%b%l+1)
      call create_(Fxpp,self%a%l+1,self%b%l+1); call create_(Fypp,self%a%l+1,self%b%l+1); call create_(Fzpp,self%a%l+1,self%b%l+1)

      ! Gaussisn center
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c

      ! No of roots
      n_roots = (self%a%l+self%b%l+4)/2

      ! Get roots & weights
      call create_(rys,n_roots)
      xx = p*dot_(rpc,rpc)
      call get_weights_and_u_roots_(rys,xx)

      ! Initialize
      M = ZERO

      ! Loop over roots
      do k = 1,n_roots

         ! Roots and weights
         uu = p*rys%root(k)
         wt = rys%weight(k)

         ! Get overlap and E-Field integrals
         call make_E_field_ints_(self,Sx00,Fx00,1,uu,c(1),self%a%l+1,self%b%l+1)
         call make_E_field_ints_(self,Sy00,Fy00,2,uu,c(2),self%a%l+1,self%b%l+1)
         call make_E_field_ints_(self,Sz00,Fz00,3,uu,c(3),self%a%l+1,self%b%l+1)

         ! Fold in weights
         Sz00 = Sz00*wt
         Fz00 = Fz00*wt

         ! Differentiate left/right
         call differentiate_(self,Sx00,"left",Sx10); call differentiate_(self,Sx00,"right",Sx01)
         call differentiate_(self,Sy00,"left",Sy10); call differentiate_(self,Sy00,"right",Sy01)
         call differentiate_(self,Sz00,"left",Sz10); call differentiate_(self,Sz00,"right",Sz01)
         call differentiate_(self,Fx00,"left",Fx10); call differentiate_(self,Fx00,"right",Fx01)
         call differentiate_(self,Fy00,"left",Fy10); call differentiate_(self,Fy00,"right",Fy01)
         call differentiate_(self,Fz00,"left",Fz10); call differentiate_(self,Fz00,"right",Fz01)

         ! Symmetric combination
         Sxpp = Sx01+Sx10
         Sypp = Sy01+Sy10
         Szpp = Sz01+Sz10
         Fxpp = Fx01+Fx10
         Fypp = Fy01+Fy10
         Fzpp = Fz01+Fz10

         ! Evaluate
         M(:,:,1,1) = M(:,:,1,1)+Fxpp(ax,bx)*Sy00(ay,by)*Sz00(az,bz)  ! x has pp
         M(:,:,1,2) = M(:,:,1,2)+Sxpp(ax,bx)*Fy00(ay,by)*Sz00(az,bz)
         M(:,:,1,3) = M(:,:,1,3)+Sxpp(ax,bx)*Sy00(ay,by)*Fz00(az,bz)
         M(:,:,2,1) = M(:,:,2,1)+Fx00(ax,bx)*Sypp(ay,by)*Sz00(az,bz)  ! y has pp
         M(:,:,2,2) = M(:,:,2,2)+Sx00(ax,bx)*Fypp(ay,by)*Sz00(az,bz)
         M(:,:,2,3) = M(:,:,2,3)+Sx00(ax,bx)*Sypp(ay,by)*Fz00(az,bz)
         M(:,:,3,1) = M(:,:,3,1)+Fx00(ax,bx)*Sy00(ay,by)*Szpp(az,bz)  ! z has pp
         M(:,:,3,2) = M(:,:,3,2)+Sx00(ax,bx)*Fy00(ay,by)*Szpp(az,bz)
         M(:,:,3,3) = M(:,:,3,3)+Sx00(ax,bx)*Sy00(ay,by)*Fzpp(az,bz)
      end do

      ! Clean
      call destroy_(rys)
      call destroy_(Fzpp); call destroy_(Fypp); call destroy_(Fxpp)
      call destroy_(Fz01); call destroy_(Fy01); call destroy_(Fx01)
      call destroy_(Fz10); call destroy_(Fy10); call destroy_(Fx10)
      call destroy_(Fz00); call destroy_(Fy00); call destroy_(Fx00)
      call destroy_(Szpp); call destroy_(Sypp); call destroy_(Sxpp)
      call destroy_(Sz01); call destroy_(Sy01); call destroy_(Sx01)
      call destroy_(Sz10); call destroy_(Sy10); call destroy_(Sx10)
      call destroy_(Sz00); call destroy_(Sy00); call destroy_(Sx00)

      ! Add prefactor
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/p
      M = prefac*M
   end subroutine

   PURE subroutine make_nabla_field_ints(self,M,c)
   ! Make the magnetic spin density integrals "Mij", evaluated at point "c",
   !    <a|Mij(c)|b> = \int dr g_a(r) ( grad_i g_b(r) ) (c - r)_j/ |c - r|^3
   !                 = M(a,b,i,j)
   ! These are used to get the magnetic field from the spin density,
   !      B(c) = -(g mu_b mu_o / 4 pi)
   !             \int dr curl rho_spin(r) x (c - r)/|c - r|3
   ! Note: in atomic units mu_o = 4 pi / c^2.
   ! Uses a double quadrature based on Rys method and Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: M
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: Sx00,Sy00,Sz00
      MAT(REAL,:,:), allocatable :: Sx01,Sy01,Sz01
      MAT(REAL,:,:), allocatable :: Fx00,Fy00,Fz00
      MAT(REAL,:,:), allocatable :: Fx01,Fy01,Fz01
      VEC(INT,:), allocatable :: ax,ay,az
      VEC(INT,:), allocatable :: bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys

      ! Gaussian function limits
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)

      ! 2D integrals (dimension one higher than actual l value needed)
      call create_(Sx00,self%a%l+1,self%b%l+2); call create_(Sy00,self%a%l+1,self%b%l+2); call create_(Sz00,self%a%l+1,self%b%l+2)
      call create_(Sx01,self%a%l+1,self%b%l+1); call create_(Sy01,self%a%l+1,self%b%l+1); call create_(Sz01,self%a%l+1,self%b%l+1)
      call create_(Fx00,self%a%l+1,self%b%l+2); call create_(Fy00,self%a%l+1,self%b%l+2); call create_(Fz00,self%a%l+1,self%b%l+2)
      call create_(Fx01,self%a%l+1,self%b%l+1); call create_(Fy01,self%a%l+1,self%b%l+1); call create_(Fz01,self%a%l+1,self%b%l+1)

      ! Gaussian center
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c

      ! No of roots
      n_roots = (self%a%l+self%b%l+4)/2

      ! Get roots & weights
      call create_(rys,n_roots)
      xx = p*dot_(rpc,rpc)
      call get_weights_and_u_roots_(rys,xx)

      ! Initialize
      M = ZERO

      ! Loop over roots
      do k = 1,n_roots

         ! Roots and weights
         uu = p*rys%root(k)
         wt = rys%weight(k)

         ! Get overlap and E-Field integrals
         call make_E_field_ints_(self,Sx00,Fx00,1,uu,c(1),self%a%l,self%b%l+1)
         call make_E_field_ints_(self,Sy00,Fy00,2,uu,c(2),self%a%l,self%b%l+1)
         call make_E_field_ints_(self,Sz00,Fz00,3,uu,c(3),self%a%l,self%b%l+1)

         ! Fold in weights
         Sz00 = Sz00*wt
         Fz00 = Fz00*wt

         ! Differentiate left/right
         call differentiate_(self,Sx00,"right",Sx01)
         call differentiate_(self,Sy00,"right",Sy01)
         call differentiate_(self,Sz00,"right",Sz01)
         call differentiate_(self,Fx00,"right",Fx01)
         call differentiate_(self,Fy00,"right",Fy01)
         call differentiate_(self,Fz00,"right",Fz01)

         ! Evaluate
         M(:,:,1,1) = M(:,:,1,1)+Fx01(ax,bx)*Sy00(ay,by)*Sz00(az,bz)  ! x has grad
         M(:,:,1,2) = M(:,:,1,2)+Sx01(ax,bx)*Fy00(ay,by)*Sz00(az,bz)
         M(:,:,1,3) = M(:,:,1,3)+Sx01(ax,bx)*Sy00(ay,by)*Fz00(az,bz)
         M(:,:,2,1) = M(:,:,2,1)+Fx00(ax,bx)*Sy01(ay,by)*Sz00(az,bz)  ! y has grad
         M(:,:,2,2) = M(:,:,2,2)+Sx00(ax,bx)*Fy01(ay,by)*Sz00(az,bz)
         M(:,:,2,3) = M(:,:,2,3)+Sx00(ax,bx)*Sy01(ay,by)*Fz00(az,bz)
         M(:,:,3,1) = M(:,:,3,1)+Fx00(ax,bx)*Sy00(ay,by)*Sz01(az,bz)  ! z has grad
         M(:,:,3,2) = M(:,:,3,2)+Sx00(ax,bx)*Fy00(ay,by)*Sz01(az,bz)
         M(:,:,3,3) = M(:,:,3,3)+Sx00(ax,bx)*Sy00(ay,by)*Fz01(az,bz)
      end do

      ! Clean
      call destroy_(rys)
      call destroy_(Fz01); call destroy_(Fy01); call destroy_(Fx01)
      call destroy_(Fz00); call destroy_(Fy00); call destroy_(Fx00)
      call destroy_(Sz01); call destroy_(Sy01); call destroy_(Sx01)
      call destroy_(Sz00); call destroy_(Sy00); call destroy_(Sx00)

      ! Add prefactor
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/p
      M = prefac*M
   end subroutine

   PURE subroutine make_solenoidal_jp_ints(self,Jx,Jy,Jz,c)
   ! Make the solenoidal Jp integrals "Ji", evaluated at point "c",
   !    <a|J(c)|b> = (1/8 PI) \int dr curl [g_a grad g_b - g_b grad g_a] x (c - r)/ |c - r|^3
   ! Uses a double quadrature based on Rys method and Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Jx,Jy,Jz
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: Sx00,Sy00,Sz00
      MAT(REAL,:,:), allocatable :: Sx10,Sy10,Sz10
      MAT(REAL,:,:), allocatable :: Sx01,Sy01,Sz01
      MAT(REAL,:,:), allocatable :: Sxpp,Sypp,Szpp
      MAT(REAL,:,:), allocatable :: Sxmm,Symm,Szmm
      MAT(REAL,:,:), allocatable :: Fx00,Fy00,Fz00
      MAT(REAL,:,:), allocatable :: Fx10,Fy10,Fz10
      MAT(REAL,:,:), allocatable :: Fx01,Fy01,Fz01
      MAT(REAL,:,:), allocatable :: Fxpp,Fypp,Fzpp
      MAT(REAL,:,:), allocatable :: Fxmm,Fymm,Fzmm
      VEC(INT,:), allocatable :: ax,ay,az
      VEC(INT,:), allocatable :: bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx00,self%a%l+2,self%b%l+2); call create_(Sy00,self%a%l+2,self%b%l+2); call create_(Sz00,self%a%l+2,self%b%l+2)
      call create_(Sx10,self%a%l+1,self%b%l+1); call create_(Sy10,self%a%l+1,self%b%l+1); call create_(Sz10,self%a%l+1,self%b%l+1)
      call create_(Sx01,self%a%l+1,self%b%l+1); call create_(Sy01,self%a%l+1,self%b%l+1); call create_(Sz01,self%a%l+1,self%b%l+1)
      call create_(Sxpp,self%a%l+1,self%b%l+1); call create_(Sypp,self%a%l+1,self%b%l+1); call create_(Szpp,self%a%l+1,self%b%l+1)
      call create_(Sxmm,self%a%l+1,self%b%l+1); call create_(Symm,self%a%l+1,self%b%l+1); call create_(Szmm,self%a%l+1,self%b%l+1)
      call create_(Fx00,self%a%l+2,self%b%l+2); call create_(Fy00,self%a%l+2,self%b%l+2); call create_(Fz00,self%a%l+2,self%b%l+2)
      call create_(Fx10,self%a%l+1,self%b%l+1); call create_(Fy10,self%a%l+1,self%b%l+1); call create_(Fz10,self%a%l+1,self%b%l+1)
      call create_(Fx01,self%a%l+1,self%b%l+1); call create_(Fy01,self%a%l+1,self%b%l+1); call create_(Fz01,self%a%l+1,self%b%l+1)
      call create_(Fxpp,self%a%l+1,self%b%l+1); call create_(Fypp,self%a%l+1,self%b%l+1); call create_(Fzpp,self%a%l+1,self%b%l+1)
      call create_(Fxmm,self%a%l+1,self%b%l+1); call create_(Fymm,self%a%l+1,self%b%l+1); call create_(Fzmm,self%a%l+1,self%b%l+1)
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c
      n_roots = (self%a%l+self%b%l+2)/2+1
      call create_(rys,n_roots)
      xx = p*dot_(rpc,rpc)
      call get_weights_and_u_roots_(rys,xx)
      Jx = ZERO
      Jy = ZERO
      Jz = ZERO
      do k = 1,n_roots
         uu = p*rys%root(k)
         wt = rys%weight(k)
         call make_E_field_ints_(self,Sx00,Fx00,1,uu,c(1),self%a%l+1,self%b%l+1)
         call make_E_field_ints_(self,Sy00,Fy00,2,uu,c(2),self%a%l+1,self%b%l+1)
         call make_E_field_ints_(self,Sz00,Fz00,3,uu,c(3),self%a%l+1,self%b%l+1)
         Sz00 = Sz00*wt
         Fz00 = Fz00*wt
         call differentiate_(self,Sx00,"left",Sx10); call differentiate_(self,Sx00,"right",Sx01)
         call differentiate_(self,Sy00,"left",Sy10); call differentiate_(self,Sy00,"right",Sy01)
         call differentiate_(self,Sz00,"left",Sz10); call differentiate_(self,Sz00,"right",Sz01)
         call differentiate_(self,Fx00,"left",Fx10); call differentiate_(self,Fx00,"right",Fx01)
         call differentiate_(self,Fy00,"left",Fy10); call differentiate_(self,Fy00,"right",Fy01)
         call differentiate_(self,Fz00,"left",Fz10); call differentiate_(self,Fz00,"right",Fz01)
         Sxpp = Sx01+Sx10; Sxmm = Sx01-Sx10
         Sypp = Sy01+Sy10; Symm = Sy01-Sy10
         Szpp = Sz01+Sz10; Szmm = Sz01-Sz10
         Fxpp = Fx01+Fx10; Fxmm = Fx01-Fx10
         Fypp = Fy01+Fy10; Fymm = Fy01-Fy10
         Fzpp = Fz01+Fz10; Fzmm = Fz01-Fz10
         Jx = Jx+Sxmm(ax,bx)*Sy00(ay,by)*Fzpp(az,bz)-Sxpp(ax,bx)*Sy00(ay,by)*Fzmm(az,bz)-Sxpp(ax,bx)*Fymm(ay,by)*Sz00(az,bz)+Sxmm(ax,bx)*Fypp(ay,by)*Sz00(az,bz)
         Jy = Jy+Fxpp(ax,bx)*Symm(ay,by)*Sz00(az,bz)-Fxmm(ax,bx)*Sypp(ay,by)*Sz00(az,bz)-Sx00(ax,bx)*Sypp(ay,by)*Fzmm(az,bz)+Sx00(ax,bx)*Symm(ay,by)*Fzpp(az,bz)
         Jz = Jz+Sx00(ax,bx)*Fypp(ay,by)*Szmm(az,bz)-Sx00(ax,bx)*Fymm(ay,by)*Szpp(az,bz)-Fxmm(ax,bx)*Sy00(ay,by)*Szpp(az,bz)+Fxpp(ax,bx)*Sy00(ay,by)*Szmm(az,bz)
      end do
      call destroy_(rys)
      call destroy_(Fzmm); call destroy_(Fymm); call destroy_(Fxmm)
      call destroy_(Fzpp); call destroy_(Fypp); call destroy_(Fxpp)
      call destroy_(Fz01); call destroy_(Fy01); call destroy_(Fx01)
      call destroy_(Fz10); call destroy_(Fy10); call destroy_(Fx10)
      call destroy_(Fz00); call destroy_(Fy00); call destroy_(Fx00)
      call destroy_(Szmm); call destroy_(Symm); call destroy_(Sxmm)
      call destroy_(Szpp); call destroy_(Sypp); call destroy_(Sxpp)
      call destroy_(Sz01); call destroy_(Sy01); call destroy_(Sx01)
      call destroy_(Sz10); call destroy_(Sy10); call destroy_(Sx10)
      call destroy_(Sz00); call destroy_(Sy00); call destroy_(Sx00)
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/(8*PI*p)
      Jx = prefac*Jx
      Jy = prefac*Jy
      Jz = prefac*Jz
   end subroutine

   PURE subroutine make_solenoidal_jd_ints(self,Jx,Jy,Jz,B_field,origin,c)
   ! Make the solenoidal Jd integrals "Ji", with external magnetic field
   ! "B_field", gauge origin "origin",evaluated at point "c",
   !    <a|J(c)|b> = (1/8 PI) \int dr (curl J^d_ab(r)) x (c - r)/ |c - r|^3
   !     curl J^d_ab(r) = B (2 g_a(r)g_b(r) + (r-r_o).grad g_a(r)g_b(r))
   !                      - (r-r_o)(B.grad g_a(r)g_b(r))
   ! Uses a double quadrature based on Rys method and Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Jx,Jy,Jz
      VEC(REAL,3), IN :: B_field,origin,c
      MAT(REAL,:,:), allocatable :: Sx00,Sy00,Sz00,Sx10,Sy10,Sz10,Sx01,Sy01,Sz01,Sxpp,Sypp,Szpp
      MAT(REAL,:,:), allocatable :: Dx00,Dy00,Dz00,Dx10,Dy10,Dz10,Dx01,Dy01,Dz01,Dxpp,Dypp,Dzpp
      MAT(REAL,:,:), allocatable :: Fx00,Fy00,Fz00,Fx10,Fy10,Fz10,Fx01,Fy01,Fz01,Fxpp,Fypp,Fzpp
      MAT(REAL,:,:), allocatable :: Qx00,Qy00,Qz00,Qx10,Qy10,Qz10,Qx01,Qy01,Qz01,Qxpp,Qypp,Qzpp
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac,B_x,B_y,B_z
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys
      B_x = B_field(1)
      B_y = B_field(2)
      B_z = B_field(3)
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx00,self%a%l+2,self%b%l+2); call create_(Sy00,self%a%l+2,self%b%l+2); call create_(Sz00,self%a%l+2,self%b%l+2)
      call create_(Sx10,self%a%l+1,self%b%l+1); call create_(Sy10,self%a%l+1,self%b%l+1); call create_(Sz10,self%a%l+1,self%b%l+1)
      call create_(Sx01,self%a%l+1,self%b%l+1); call create_(Sy01,self%a%l+1,self%b%l+1); call create_(Sz01,self%a%l+1,self%b%l+1)
      call create_(Sxpp,self%a%l+1,self%b%l+1); call create_(Sypp,self%a%l+1,self%b%l+1); call create_(Szpp,self%a%l+1,self%b%l+1)
      call create_(Dx00,self%a%l+2,self%b%l+2); call create_(Dy00,self%a%l+2,self%b%l+2); call create_(Dz00,self%a%l+2,self%b%l+2)
      call create_(Dx10,self%a%l+1,self%b%l+1); call create_(Dy10,self%a%l+1,self%b%l+1); call create_(Dz10,self%a%l+1,self%b%l+1)
      call create_(Dx01,self%a%l+1,self%b%l+1); call create_(Dy01,self%a%l+1,self%b%l+1); call create_(Dz01,self%a%l+1,self%b%l+1)
      call create_(Dxpp,self%a%l+1,self%b%l+1); call create_(Dypp,self%a%l+1,self%b%l+1); call create_(Dzpp,self%a%l+1,self%b%l+1)
      call create_(Fx00,self%a%l+2,self%b%l+2); call create_(Fy00,self%a%l+2,self%b%l+2); call create_(Fz00,self%a%l+2,self%b%l+2)
      call create_(Fx10,self%a%l+1,self%b%l+1); call create_(Fy10,self%a%l+1,self%b%l+1); call create_(Fz10,self%a%l+1,self%b%l+1)
      call create_(Fx01,self%a%l+1,self%b%l+1); call create_(Fy01,self%a%l+1,self%b%l+1); call create_(Fz01,self%a%l+1,self%b%l+1)
      call create_(Fxpp,self%a%l+1,self%b%l+1); call create_(Fypp,self%a%l+1,self%b%l+1); call create_(Fzpp,self%a%l+1,self%b%l+1)
      call create_(Qx00,self%a%l+2,self%b%l+2); call create_(Qy00,self%a%l+2,self%b%l+2); call create_(Qz00,self%a%l+2,self%b%l+2)
      call create_(Qx10,self%a%l+1,self%b%l+1); call create_(Qy10,self%a%l+1,self%b%l+1); call create_(Qz10,self%a%l+1,self%b%l+1)
      call create_(Qx01,self%a%l+1,self%b%l+1); call create_(Qy01,self%a%l+1,self%b%l+1); call create_(Qz01,self%a%l+1,self%b%l+1)
      call create_(Qxpp,self%a%l+1,self%b%l+1); call create_(Qypp,self%a%l+1,self%b%l+1); call create_(Qzpp,self%a%l+1,self%b%l+1)
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c
      n_roots = (self%a%l+self%b%l+3)/2+1
      call create_(rys,n_roots)
      xx = p*dot_(rpc,rpc)
      call get_weights_and_u_roots_(rys,xx)
      Jx = ZERO
      Jy = ZERO
      Jz = ZERO
      do k = 1,n_roots
         uu = p*rys%root(k)
         wt = rys%weight(k)
         call make_Q_field_ints_(self,Sx00,Dx00,Fx00,Qx00,1,uu,c(1),origin(1),self%a%l+1,self%b%l+1)
         call make_Q_field_ints_(self,Sy00,Dy00,Fy00,Qy00,2,uu,c(2),origin(2),self%a%l+1,self%b%l+1)
         call make_Q_field_ints_(self,Sz00,Dz00,Fz00,Qz00,3,uu,c(3),origin(3),self%a%l+1,self%b%l+1)
         Sz00 = Sz00*wt
         Dz00 = Dz00*wt
         Fx00 = -Fx00  ! minus sign to give (c-r) instead of (r-c)
         Fy00 = -Fy00
         Fz00 = -Fz00*wt
         Qx00 = -Qx00
         Qy00 = -Qy00
         Qz00 = -Qz00*wt
         call differentiate_(self,Sx00,"left",Sx10); call differentiate_(self,Sx00,"right",Sx01)
         call differentiate_(self,Sy00,"left",Sy10); call differentiate_(self,Sy00,"right",Sy01)
         call differentiate_(self,Sz00,"left",Sz10); call differentiate_(self,Sz00,"right",Sz01)
         call differentiate_(self,Dx00,"left",Dx10); call differentiate_(self,Dx00,"right",Dx01)
         call differentiate_(self,Dy00,"left",Dy10); call differentiate_(self,Dy00,"right",Dy01)
         call differentiate_(self,Dz00,"left",Dz10); call differentiate_(self,Dz00,"right",Dz01)
         call differentiate_(self,Fx00,"left",Fx10); call differentiate_(self,Fx00,"right",Fx01)
         call differentiate_(self,Fy00,"left",Fy10); call differentiate_(self,Fy00,"right",Fy01)
         call differentiate_(self,Fz00,"left",Fz10); call differentiate_(self,Fz00,"right",Fz01)
         call differentiate_(self,Qx00,"left",Qx10); call differentiate_(self,Qx00,"right",Qx01)
         call differentiate_(self,Qy00,"left",Qy10); call differentiate_(self,Qy00,"right",Qy01)
         call differentiate_(self,Qz00,"left",Qz10); call differentiate_(self,Qz00,"right",Qz01)
         Sxpp = Sx01+Sx10
         Sypp = Sy01+Sy10
         Szpp = Sz01+Sz10
         Dxpp = Dx01+Dx10
         Dypp = Dy01+Dy10
         Dzpp = Dz01+Dz10
         Fxpp = Fx01+Fx10
         Fypp = Fy01+Fy10
         Fzpp = Fz01+Fz10
         Qxpp = Qx01+Qx10
         Qypp = Qy01+Qy10
         Qzpp = Qz01+Qz10
         Jx = Jx+B_field(2)*(TWO*Sx00(ax,bx)*Sy00(ay,by)*Fz00(az,bz)+Dxpp(ax,bx)*Sy00(ay,by)*Fz00(az,bz)+Sx00(ax,bx)*Dypp(ay,by)*Fz00(az,bz)+Sx00(ax,bx)*Sy00(ay,by)*Qzpp(az,bz))-B_field(3)*(TWO*Sx00(ax,bx)*Fy00(ay,by)*Sz00(az,bz)+Dxpp(ax,bx)*Fy00(ay,by)*Sz00(az,bz)+Sx00(ax,bx)*Qypp(ay,by)*Sz00(az,bz)+Sx00(ax,bx)*Fy00(ay,by)*Dzpp(az,bz))-B_field(1)*(Sxpp(ax,bx)*Dy00(ay,by)*Fz00(az,bz)-Sxpp(ax,bx)*Fy00(ay,by)*Dz00(az,bz))-B_field(2)*(Sx00(ax,bx)*Dypp(ay,by)*Fz00(az,bz)-Sx00(ax,bx)*Fypp(ay,by)*Dz00(az,bz))-B_field(3)*(Sx00(ax,bx)*Dy00(ay,by)*Fzpp(az,bz)-Sx00(ax,bx)*Fy00(ay,by)*Dzpp(az,bz))
         Jy = Jy+B_field(3)*(TWO*Fx00(ax,bx)*Sy00(ay,by)*Sz00(az,bz)+Qxpp(ax,bx)*Sy00(ay,by)*Sz00(az,bz)+Fx00(ax,bx)*Dypp(ay,by)*Sz00(az,bz)+Fx00(ax,bx)*Sy00(ay,by)*Dzpp(az,bz))-B_field(1)*(TWO*Sx00(ax,bx)*Sy00(ay,by)*Fz00(az,bz)+Dxpp(ax,bx)*Sy00(ay,by)*Fz00(az,bz)+Sx00(ax,bx)*Dypp(ay,by)*Fz00(az,bz)+Sx00(ax,bx)*Sy00(ay,by)*Qzpp(az,bz))-B_field(1)*(Fxpp(ax,bx)*Sy00(ay,by)*Dz00(az,bz)-Dxpp(ax,bx)*Sy00(ay,by)*Fz00(az,bz))-B_field(2)*(Fx00(ax,bx)*Sypp(ay,by)*Dz00(az,bz)-Dx00(ax,bx)*Sypp(ay,by)*Fz00(az,bz))-B_field(3)*(Fx00(ax,bx)*Sy00(ay,by)*Dzpp(az,bz)-Dx00(ax,bx)*Sy00(ay,by)*Fzpp(az,bz))
         Jz = Jz+B_field(1)*(TWO*Sx00(ax,bx)*Fy00(ay,by)*Sz00(az,bz)+Dxpp(ax,bx)*Fy00(ay,by)*Sz00(az,bz)+Sx00(ax,bx)*Qypp(ay,by)*Sz00(az,bz)+Sx00(ax,bx)*Fy00(ay,by)*Dzpp(az,bz))-B_field(2)*(TWO*Fx00(ax,bx)*Sy00(ay,by)*Sz00(az,bz)+Qxpp(ax,bx)*Sy00(ay,by)*Sz00(az,bz)+Fx00(ax,bx)*Dypp(ay,by)*Sz00(az,bz)+Fx00(ax,bx)*Sy00(ay,by)*Dzpp(az,bz))-B_field(1)*(Dxpp(ax,bx)*Fy00(ay,by)*Sz00(az,bz)-Fxpp(ax,bx)*Dy00(ay,by)*Sz00(az,bz))-B_field(2)*(Dx00(ax,bx)*Fypp(ay,by)*Sz00(az,bz)-Fx00(ax,bx)*Dypp(ay,by)*Sz00(az,bz))-B_field(3)*(Dx00(ax,bx)*Fy00(ay,by)*Szpp(az,bz)-Fx00(ax,bx)*Dy00(ay,by)*Szpp(az,bz))
      end do
      call destroy_(rys)
      call destroy_(Qzpp); call destroy_(Qypp); call destroy_(Qxpp)
      call destroy_(Qz01); call destroy_(Qy01); call destroy_(Qx01)
      call destroy_(Qz10); call destroy_(Qy10); call destroy_(Qx10)
      call destroy_(Qz00); call destroy_(Qy00); call destroy_(Qx00)
      call destroy_(Fzpp); call destroy_(Fypp); call destroy_(Fxpp)
      call destroy_(Fz01); call destroy_(Fy01); call destroy_(Fx01)
      call destroy_(Fz10); call destroy_(Fy10); call destroy_(Fx10)
      call destroy_(Fz00); call destroy_(Fy00); call destroy_(Fx00)
      call destroy_(Qzpp); call destroy_(Qypp); call destroy_(Qxpp)
      call destroy_(Qz01); call destroy_(Qy01); call destroy_(Qx01)
      call destroy_(Qz10); call destroy_(Qy10); call destroy_(Qx10)
      call destroy_(Qz00); call destroy_(Qy00); call destroy_(Qx00)
      call destroy_(Szpp); call destroy_(Sypp); call destroy_(Sxpp)
      call destroy_(Sz01); call destroy_(Sy01); call destroy_(Sx01)
      call destroy_(Sz10); call destroy_(Sy10); call destroy_(Sx10)
      call destroy_(Sz00); call destroy_(Sy00); call destroy_(Sx00)
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/(8*PI*p)
      Jx = prefac*Jx
      Jy = prefac*Jy
      Jz = prefac*Jz
   end subroutine

   PURE subroutine make_irrotational_jp_ints(self,Jx,Jy,Jz,c)
   ! Make the solenoidal Jp integrals "Ji", evaluated at point "c",
   !    <a|J(c)|b> = (1/8 PI) \int dr curl [g_a grad g_b - g_b grad g_a] x (c - r)/ |c - r|^3
   ! Uses a double quadrature based on Rys method and Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Jx,Jy,Jz
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: Sx00,Sy00,Sz00,Sx10,Sy10,Sz10,Sx01,Sy01,Sz01,Sxmm,Symm,Szmm
      MAT(REAL,:,:), allocatable :: Fx00,Fy00,Fz00,Fx10,Fy10,Fz10,Fx01,Fy01,Fz01,Fxmm,Fymm,Fzmm
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac
      INT :: n_roots,k,fa,la,fb,lb
      type(RYS_TYPE), allocatable :: rys
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx00,self%a%l+3,self%b%l+3); call create_(Sy00,self%a%l+3,self%b%l+3); call create_(Sz00,self%a%l+3,self%b%l+3)
      call create_(Sx10,self%a%l+2,self%b%l+2); call create_(Sy10,self%a%l+2,self%b%l+2); call create_(Sz10,self%a%l+2,self%b%l+2)
      call create_(Sx01,self%a%l+2,self%b%l+2); call create_(Sy01,self%a%l+2,self%b%l+2); call create_(Sz01,self%a%l+2,self%b%l+2)
      call create_(Sxmm,self%a%l+2,self%b%l+2); call create_(Symm,self%a%l+2,self%b%l+2); call create_(Szmm,self%a%l+2,self%b%l+2)
      call create_(Fx00,self%a%l+3,self%b%l+3); call create_(Fy00,self%a%l+3,self%b%l+3); call create_(Fz00,self%a%l+3,self%b%l+3)
      call create_(Fx10,self%a%l+2,self%b%l+2); call create_(Fy10,self%a%l+2,self%b%l+2); call create_(Fz10,self%a%l+2,self%b%l+2)
      call create_(Fx01,self%a%l+2,self%b%l+2); call create_(Fy01,self%a%l+2,self%b%l+2); call create_(Fz01,self%a%l+2,self%b%l+2)
      call create_(Fxmm,self%a%l+2,self%b%l+2); call create_(Fymm,self%a%l+2,self%b%l+2); call create_(Fzmm,self%a%l+2,self%b%l+2)
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c
      n_roots = (self%a%l+self%b%l+3)/2+1
      call create_(rys,n_roots)
      xx = p*dot_(rpc,rpc)
      call get_weights_and_u_roots_(rys,xx)
      Jx = ZERO; Jy = ZERO; Jz = ZERO
      do k = 1,n_roots
         uu = p*rys%root(k)
         wt = rys%weight(k)
         call make_E_field_ints_(self,Sx00,Fx00,1,uu,c(1),self%a%l+2,self%b%l+2)
         call make_E_field_ints_(self,Sy00,Fy00,2,uu,c(2),self%a%l+2,self%b%l+2)
         call make_E_field_ints_(self,Sz00,Fz00,3,uu,c(3),self%a%l+2,self%b%l+2)
         Sz00 = Sz00*wt
         Fz00 = Fz00*wt
         call differentiate_(self,Sx00,"right",Sx01); call differentiate_(self,Sx01,"right",Sxmm)
         call differentiate_(self,Sx00,"left",Sx10); call differentiate_(self,Sx10,"left",Sx01)
         call differentiate_(self,Sy00,"right",Sy01); call differentiate_(self,Sy01,"right",Symm)
         call differentiate_(self,Sy00,"left",Sy10); call differentiate_(self,Sy10,"left",Sy01)
         call differentiate_(self,Sz00,"right",Sz01); call differentiate_(self,Sz01,"right",Szmm)
         call differentiate_(self,Sz00,"left",Sz10); call differentiate_(self,Sz10,"left",Sz01)
         call differentiate_(self,Fx00,"right",Fx01); call differentiate_(self,Fx01,"right",Fxmm)
         call differentiate_(self,Fx00,"left",Fx10); call differentiate_(self,Fx10,"left",Fx01)
         call differentiate_(self,Fy00,"right",Fy01); call differentiate_(self,Fy01,"right",Fymm)
         call differentiate_(self,Fy00,"left",Fy10); call differentiate_(self,Fy10,"left",Fy01)
         call differentiate_(self,Fz00,"right",Fz01); call differentiate_(self,Fz01,"right",Fzmm)
         call differentiate_(self,Fz00,"left",Fz10); call differentiate_(self,Fz10,"left",Fz01)
         Sxmm = Sxmm-Sx01
         Symm = Symm-Sy01
         Szmm = Szmm-Sz01
         Fxmm = Fxmm-Fx01
         Fymm = Fymm-Fy01
         Fzmm = Fzmm-Fz01
         Jx = Jx+Fxmm(ax,bx)*Sy00(ay,by)*Sz00(az,bz)+Fx00(ax,bx)*Symm(ay,by)*Sz00(az,bz)+Fx00(ax,bx)*Sy00(ay,by)*Szmm(az,bz)
         Jy = Jy+Sxmm(ax,bx)*Fy00(ay,by)*Sz00(az,bz)+Sx00(ax,bx)*Fymm(ay,by)*Sz00(az,bz)+Sx00(ax,bx)*Fy00(ay,by)*Szmm(az,bz)
         Jz = Jz+Sxmm(ax,bx)*Sy00(ay,by)*Fz00(az,bz)+Sx00(ax,bx)*Symm(ay,by)*Fz00(az,bz)+Sx00(ax,bx)*Sy00(ay,by)*Fzmm(az,bz)
      end do
      call destroy_(rys)
      call destroy_(Fzmm); call destroy_(Fymm); call destroy_(Fxmm)
      call destroy_(Fz01); call destroy_(Fy01); call destroy_(Fx01)
      call destroy_(Fz10); call destroy_(Fy10); call destroy_(Fx10)
      call destroy_(Fz00); call destroy_(Fy00); call destroy_(Fx00)
      call destroy_(Szmm); call destroy_(Symm); call destroy_(Sxmm)
      call destroy_(Sz01); call destroy_(Sy01); call destroy_(Sx01)
      call destroy_(Sz10); call destroy_(Sy10); call destroy_(Sx10)
      call destroy_(Sz00); call destroy_(Sy00); call destroy_(Sx00)
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/(8*PI*p)
      Jx = prefac*Jx
      Jy = prefac*Jy
      Jz = prefac*Jz
   end subroutine

   PURE subroutine make_irrotational_jd_ints(self,Jx,Jy,Jz,B_field,origin,c)
   ! Make the solenoidal Jd integrals "Ji", with external magnetic field
   ! "B_field", gauge origin at "origin", and evaluated at point "c",
   !    <a|J(c)|b> = (1/8 PI) \int dr (div J^d_ab(r)) (c - r)/|c - r|3
   !     J^d_ab(r) = B (2 g_a(r)g_b(r) + (r-r_o).grad g_a(r)g_b(r))
   !                 - (r-r_o)(B.grad g_a(r)g_b(r))
   ! Uses a double quadrature based on Rys method and Gauss-Hermite quadrature.
      type(GAUSSIAN2_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: Jx,Jy,Jz
      VEC(REAL,3), IN :: B_field,origin,c
      MAT(REAL,:,:), allocatable :: Sx00,Sy00,Sz00,Sx10,Sy10,Sz10,Sx01,Sy01,Sz01
      MAT(REAL,:,:), allocatable :: Sxpp,Sypp,Szpp,Dx00,Dy00,Dz00
      MAT(REAL,:,:), allocatable :: Fx00,Fy00,Fz00,Fx10,Fy10,Fz10,Fx01,Fy01,Fz01
      MAT(REAL,:,:), allocatable :: Fxpp,Fypp,Fzpp,Qx00,Qy00,Qz00
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,3) :: rpc,rab
      REAL :: p,xx,uu,wt,prefac,B_x,B_y,B_z
      INT :: n_roots,k,a,b,aix,aiy,aiz,bix,biy,biz,fa,la,fb,lb
      REAL :: Sx00_ab,Sy00_ab,Sz00_ab,Fx00_ab,Fy00_ab,Fz00_ab
      REAL :: Dx00_ab,Dy00_ab,Dz00_ab,Qx00_ab,Qy00_ab,Qz00_ab
      REAL :: Sxpp_ab,Sypp_ab,Szpp_ab,Fxpp_ab,Fypp_ab,Fzpp_ab
      type(RYS_TYPE), allocatable :: rys
      B_x = B_field(1)
      B_y = B_field(2)
      B_z = B_field(3)
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      call create_(Sx00,self%a%l+2,self%b%l+2); call create_(Sy00,self%a%l+2,self%b%l+2); call create_(Sz00,self%a%l+2,self%b%l+2)
      call create_(Sx10,self%a%l+1,self%b%l+1); call create_(Sy10,self%a%l+1,self%b%l+1); call create_(Sz10,self%a%l+1,self%b%l+1)
      call create_(Sx01,self%a%l+1,self%b%l+1); call create_(Sy01,self%a%l+1,self%b%l+1); call create_(Sz01,self%a%l+1,self%b%l+1)
      call create_(Sxpp,self%a%l+1,self%b%l+1); call create_(Sypp,self%a%l+1,self%b%l+1); call create_(Szpp,self%a%l+1,self%b%l+1)
      call create_(Dx00,self%a%l+2,self%b%l+2); call create_(Dy00,self%a%l+2,self%b%l+2); call create_(Dz00,self%a%l+2,self%b%l+2)
      call create_(Fx00,self%a%l+2,self%b%l+2); call create_(Fy00,self%a%l+2,self%b%l+2); call create_(Fz00,self%a%l+2,self%b%l+2)
      call create_(Fx10,self%a%l+1,self%b%l+1); call create_(Fy10,self%a%l+1,self%b%l+1); call create_(Fz10,self%a%l+1,self%b%l+1)
      call create_(Fx01,self%a%l+1,self%b%l+1); call create_(Fy01,self%a%l+1,self%b%l+1); call create_(Fz01,self%a%l+1,self%b%l+1)
      call create_(Fxpp,self%a%l+1,self%b%l+1); call create_(Fypp,self%a%l+1,self%b%l+1); call create_(Fzpp,self%a%l+1,self%b%l+1)
      call create_(Qx00,self%a%l+2,self%b%l+2); call create_(Qy00,self%a%l+2,self%b%l+2); call create_(Qz00,self%a%l+2,self%b%l+2)
      p = self%a%exponent+self%b%exponent
      rpc = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)/p-c
      n_roots = (self%a%l+self%b%l+2)/2+1
      call create_(rys,n_roots)
      xx = p*dot_(rpc,rpc)
      call get_weights_and_u_roots_(rys,xx)
      Jx = ZERO; Jy = ZERO; Jz = ZERO
      do k = 1,n_roots
         uu = p*rys%root(k)
         wt = rys%weight(k)
         call make_Q_field_ints_(self,Sx00,Dx00,Fx00,Qx00,1,uu,c(1),origin(1),self%a%l+1,self%b%l+1)
         call make_Q_field_ints_(self,Sy00,Dy00,Fy00,Qy00,2,uu,c(2),origin(2),self%a%l+1,self%b%l+1)
         call make_Q_field_ints_(self,Sz00,Dz00,Fz00,Qz00,3,uu,c(3),origin(3),self%a%l+1,self%b%l+1)
         Sz00 = Sz00*wt
         Dz00 = Dz00*wt
         Fz00 = Fz00*wt
         Qz00 = Qz00*wt
         call differentiate_(self,Sx00,"left",Sx10); call differentiate_(self,Sx00,"right",Sx01)
         call differentiate_(self,Sy00,"left",Sy10); call differentiate_(self,Sy00,"right",Sy01)
         call differentiate_(self,Sz00,"left",Sz10); call differentiate_(self,Sz00,"right",Sz01)
         call differentiate_(self,Fx00,"left",Fx10); call differentiate_(self,Fx00,"right",Fx01)
         call differentiate_(self,Fy00,"left",Fy10); call differentiate_(self,Fy00,"right",Fy01)
         call differentiate_(self,Fz00,"left",Fz10); call differentiate_(self,Fz00,"right",Fz01)
         Sxpp = Sx01+Sx10
         Sypp = Sy01+Sy10
         Szpp = Sz01+Sz10
         Fxpp = Fx01+Fx10
         Fypp = Fy01+Fy10
         Fzpp = Fz01+Fz10
         do b = 1,self%b%n_comp
            bix = bx(b)
            biy = by(b)
            biz = bz(b)
            do a = 1,self%a%n_comp
               aix = ax(a)
               aiy = ay(a)
               aiz = az(a)
               Sx00_ab = Sx00(aix,bix)
               Sy00_ab = Sy00(aiy,biy)
               Sz00_ab = Sz00(aiz,biz)
               Fx00_ab = Fx00(aix,bix)
               Fy00_ab = Fy00(aiy,biy)
               Fz00_ab = Fz00(aiz,biz)
               Dx00_ab = Dx00(aix,bix)
               Dy00_ab = Dy00(aiy,biy)
               Dz00_ab = Dz00(aiz,biz)
               Qx00_ab = Qx00(aix,bix)
               Qy00_ab = Qy00(aiy,biy)
               Qz00_ab = Qz00(aiz,biz)
               Sxpp_ab = Sxpp(aix,bix)
               Sypp_ab = Sypp(aiy,biy)
               Szpp_ab = Szpp(aiz,biz)
               Fxpp_ab = Fxpp(aix,bix)
               Fypp_ab = Fypp(aiy,biy)
               Fzpp_ab = Fzpp(aiz,biz)
               Jx(a,b) = Jx(a,b)+B_y*Fxpp_ab*Sy00_ab*Dz00_ab-B_z*Fxpp_ab*Dy00_ab*Sz00_ab+B_z*Qx00_ab*Sypp_ab*Sz00_ab-B_x*Fx00_ab*Sypp_ab*Dz00_ab+B_x*Fx00_ab*Dy00_ab*Szpp_ab-B_y*Qx00_ab*Sy00_ab*Szpp_ab
               Jy(a,b) = Jy(a,b)+B_y*Sxpp_ab*Fy00_ab*Dz00_ab-B_z*Sxpp_ab*Qy00_ab*Sz00_ab+B_z*Dx00_ab*Fypp_ab*Sz00_ab-B_x*Sx00_ab*Fypp_ab*Dz00_ab+B_x*Sx00_ab*Qy00_ab*Szpp_ab-B_y*Dx00_ab*Fy00_ab*Szpp_ab
               Jz(a,b) = Jz(a,b)+B_y*Sxpp_ab*Sy00_ab*Qz00_ab-B_z*Sxpp_ab*Dy00_ab*Fz00_ab+B_z*Dx00_ab*Sypp_ab*Fz00_ab-B_x*Sx00_ab*Sypp_ab*Qz00_ab+B_x*Sx00_ab*Dy00_ab*Fzpp_ab-B_y*Dx00_ab*Sy00_ab*Fzpp_ab
            end do
         end do
      end do
      call destroy_(rys)
      call destroy_(Qz00); call destroy_(Qy00); call destroy_(Qx00)
      call destroy_(Fzpp); call destroy_(Fypp); call destroy_(Fxpp)
      call destroy_(Fz01); call destroy_(Fy01); call destroy_(Fx01)
      call destroy_(Fz10); call destroy_(Fy10); call destroy_(Fx10)
      call destroy_(Fz00); call destroy_(Fy00); call destroy_(Fx00)
      call destroy_(Dz00); call destroy_(Dy00); call destroy_(Dx00)
      call destroy_(Szpp); call destroy_(Sypp); call destroy_(Sxpp)
      call destroy_(Sz01); call destroy_(Sy01); call destroy_(Sx01)
      call destroy_(Sz10); call destroy_(Sy10); call destroy_(Sx10)
      call destroy_(Sz00); call destroy_(Sy00); call destroy_(Sx00)
      rab = self%a%position-self%b%position
      prefac = exp(-self%a%exponent*self%b%exponent*dot_(rab,rab)/p)*pi212/(8*PI*p)
      Jx = prefac*Jx
      Jy = prefac*Jy
      Jz = prefac*Jz
   end subroutine

   subroutine put(self)
   ! Put the object to file "stdout"
      type(GAUSSIAN2_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"GAUSSIAN2:")
      call flush_(stdout)
      call show_(stdout,"l_a   =",self%a%l)
      call show_(stdout,"l_b   =",self%b%l)
      call show_(stdout,"pos_a =",self%a%position)
      call show_(stdout,"pos_b =",self%b%position)
      call show_(stdout,"ex_a  =",self%a%exponent)
      call show_(stdout,"ex_b  =",self%b%exponent)
   end subroutine


!  ================================
!  Finite gaussian nucleus routines
!  ================================

   function nucleus_eta(c) result (eta)
   ! get the nucleus exponent
      INT, IN :: c
      REAL :: eta
      eta = gaussian_nucleus_eta(c)
   end function


!  ========
!  Transfer
!  ========

   pure subroutine transfer_2d_ints(self,Ix,Iy,Iz,AB,max_b)
   ! A hack from the 4 center integrals to let the finite gaussian NA
   ! ints work Use the transfer relation to put momenta on centr B to
   ! get all the 2d integrals "Ix", "Iy" and "Iz". If present, "max_b"
   ! and "max_d" are the maximum l-values desired for centers b and d,
   ! respectively.  You must ensure that the "Ii" arrays are big
   ! enough, in this case.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT3(REAL,:,:,:), INOUT :: Ix,Iy,Iz
      VEC(REAL,3), IN :: AB
      INT, optional, IN :: max_b
      INT :: le1,la,lb
      INT :: a,b,a1,b1
      la = self%a%l
      lb = self%b%l
      if (present(max_b)) lb = max_b
      if (lb==0) return
      if (lb/=0) then
         le1 = la+lb+1
         do b = 1,lb
            do a = 1,le1-b
               b1 = b+1
               a1 = a+1
               Ix(:,a,b1) = Ix(:,a1,b)+AB(1)*Ix(:,a,b)
               Iy(:,a,b1) = Iy(:,a1,b)+AB(2)*Iy(:,a,b)
               Iz(:,a,b1) = Iz(:,a1,b)+AB(3)*Iz(:,a,b)
            end do
         end do
      end if
   end subroutine


!   transfer_2d_ints_2(Ix,Iy,Iz,AB,max_a,max_b) :: private, pure
!   ! A hack from the 4 center integrals to let the finite gaussian DKH
!   ! ints work Use the transfer relation to put momenta on centr B to
!   ! get all the 2d integrals "Ix", "Iy" and "Iz". If present, "max_b"
!   ! and "max_d" are the maximum l-values desired for centers b and d,
!   ! respectively.  You must ensure that the "Ii" arrays are big
!   ! enough, in this case.
!      self :: IN
!      Ix,Iy,Iz :: MAT3{REAL}, INOUT
!      AB :: VEC{REAL}(3), IN
!      max_a, max_b :: INT, optional, IN
!
!      le1,l_a,l_b,a,b,a1,b1 :: INT
!
!      l_a = .a.l; l_b = .b.l
!      if (present(max_a)) l_a=max_a
!      if (present(max_b)) l_b=max_b
!
!      if (l_b==0) return
!
!      if (l_b/=0) then
!         le1 = l_a + l_b + 1
!         do b = 1,l_b
!         do a = 1,le1 - b
!            b1 = b + 1
!            a1 = a + 1
!            Ix(:,a,b1) = Ix(:,a1,b) + AB(1)*Ix(:,a,b)
!            Iy(:,a,b1) = Iy(:,a1,b) + AB(2)*Iy(:,a,b)
!            Iz(:,a,b1) = Iz(:,a1,b) + AB(3)*Iz(:,a,b)
!         end
!         end
!      end
!
!   end

   pure subroutine form_2d_ints(self,Ix,Iy,Iz,t2,wt,rho,zinv,PA,QP,max_e)
   ! Forms the two dimensional integrals "Ix", "Iy" and "Iz" with momenta only
   ! on center A, using Rys roots "t2" and weights "wt".
   ! Other variables are intermediates, to avoid calculations: see make_ERI_ints.
   ! If present, "max_e" and "max_f" are used for the maximum angular momenta
   ! of centers (a+b) and (c+d) respectively -- for derivative integrals.
      type(GAUSSIAN2_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: Ix,Iy,Iz
      VEC(REAL,:), IN :: t2,wt
      REAL, IN :: rho,zinv
      VEC(REAL,3), IN :: PA,QP
      INT, optional, IN :: max_e
      VEC(REAL,size(t2)) :: rzt,ce,ce1
      INT :: l_e,l_f,e,em1,ep1,k
      l_e = self%a%l+self%b%l
      l_f = 0
      if (present(max_e)) l_e = max_e
      Ix(:,1) = ONE
      Iy(:,1) = ONE
      Iz(:,1) = ONE
      if (l_e>0) then
         rzt = rho*t2*zinv
         Ix(:,2) = PA(1)+rzt(:)*QP(1)
         Iy(:,2) = PA(2)+rzt(:)*QP(2)
         Iz(:,2) = PA(3)+rzt(:)*QP(3)
         if (l_e>1) then
            ce = (ONE-rzt)*HALF*zinv
            do e = 2,l_e
               em1 = e-1
               ep1 = e+1
               ce1 = em1*ce
               Ix(:,ep1) = Ix(:,2)*Ix(:,e)+ce1*Ix(:,em1)
               Iy(:,ep1) = Iy(:,2)*Iy(:,e)+ce1*Iy(:,em1)
               Iz(:,ep1) = Iz(:,2)*Iz(:,e)+ce1*Iz(:,em1)
            end do
         end if
      end if

      ! Multiply Iz by the weight
      do k = 1,size(Iz,1)
         Iz(k,:) = Iz(k,:)*wt(k)
      end do
   end subroutine

end module
