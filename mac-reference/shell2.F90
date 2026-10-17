!====================================================================
! SHELL2 : pair of SHELLs
!
! Given two shells, can calculate
!  - Fourier transform of their product, evaluated on a grid
!  - overlap integrals
!  - kinetic energy integrals
!  - nuclear attraction integrals
!
! Based on the Rys method, as described by Lindh et al,
! J. Chem. Phys 84(7) 3963-3974
!
! - overlap (non rys version) and kinetic energy integrals work 
!   at least up to (q|q).
! - methods using the rys module are limited up to about (m|m) 
!   due to the general rys method algorithm failing.  (As tested with
!   atoms on the same center.  Different centers can go higher, but
!   not recommended).
!
! Copyright (C) Daniel Grimwood, 1998
! Copyright (C) Dylan Jayatilaka 1998-2006
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
module SHELL2_MODULE

#  include "macros"

   include "shell2.use"

   implicit none

   include "shell2.int"

   REAL, private :: pair_cutoff = SHELL2_PAIR_CUTOFF

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self)
   ! Allocate an object
      type(SHELL2_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"SHELL2:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(SHELL2_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"SHELL2:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      type(SHELL2_TYPE), INOUT :: self
      call destroy_ptr_part_(self%a)
      call destroy_ptr_part_(self%b)
      call destroy_(self%exponent_sum)
      call destroy_(self%exponent_inv)
      call destroy_(self%contraction_prefactor)
      call destroy_(self%pair_center)
      call destroy_(self%center_diff)
   end subroutine


! Try to get rid of these ...

   PURE subroutine create_1(self,shell_a,shell_b)
   ! Create a copy of a shell1 objects
      type(SHELL2_TYPE), allocatable, INOUT :: self
      type(SHELL1_TYPE), IN :: shell_a
      type(SHELL1_TYPE), IN :: shell_b
      call create_(self)
      call set_(self,shell_a,shell_b)
   end subroutine

   PURE subroutine create_2(self,shell_a,shell_b,pos_a,pos_b)
   ! Create a copy of a shell1 objects with positions
      type(SHELL2_TYPE), allocatable, INOUT :: self
      type(SHELL1_TYPE), IN :: shell_a
      type(SHELL1_TYPE), IN :: shell_b
      VEC(REAL,3), IN :: pos_a
      VEC(REAL,3), IN :: pos_b
      call create_(self)
      call set_(self,shell_a,shell_b,pos_a,pos_b)
   end subroutine

   PURE subroutine create_3(self,shell_a,shell_b,pos_a,pos_b)
   ! Create a copy of a shell1 objects with positions
      type(SHELL2_TYPE), allocatable, INOUT :: self
      type(SHELL_TYPE), IN :: shell_a
      type(SHELL_TYPE), IN :: shell_b
      VEC(REAL,3), IN :: pos_a,pos_b
      call create_(self)
      call set_(self,shell_a,shell_b,pos_a,pos_b)
   end subroutine


!  ==================
!  Set & precalculate
!  ==================

   PURE subroutine set_0(self,shell_a,shell_b)
   ! Copy the shell2 using shell1 objects
      type(SHELL2_TYPE), INOUT :: self
      type(SHELL1_TYPE), IN :: shell_a
      type(SHELL1_TYPE), IN :: shell_b
      self%a = shell_a
      self%b = shell_b
      call precalculate_(self)
   end subroutine

   PURE subroutine set_1(self,shell_a,shell_b,pos_a,pos_b)
   ! Copy the shell2 using shell objects and positions
      type(SHELL2_TYPE), INOUT :: self
      type(SHELL1_TYPE), IN :: shell_a
      type(SHELL1_TYPE), IN :: shell_b
      VEC(REAL,:), IN :: pos_a
      VEC(REAL,:), IN :: pos_b
      call set_(self%a,shell_a,pos_a)
      call set_(self%b,shell_b,pos_b)
      call precalculate_(self)
   end subroutine

   PURE subroutine set_2(self,shell_a,shell_b,pos_a,pos_b)
   ! Copy the shell2 using shell objects and positions
      type(SHELL2_TYPE), INOUT :: self
      type(SHELL_TYPE), IN :: shell_a
      type(SHELL_TYPE), IN :: shell_b
      VEC(REAL,:), IN :: pos_a
      VEC(REAL,:), IN :: pos_b
      call set_(self%a,shell_a,pos_a)
      call set_(self%b,shell_b,pos_b)
      call precalculate_(self)
   end subroutine


!  ===========
!  Set methods
!  ===========

   pure subroutine set_spherical(self,val)
   ! Set sphericals to "val" defaults
      type(SHELL2_TYPE), INOUT :: self
      BIN, IN :: val
      self%is_spherical = val
   end subroutine

   subroutine set_pair_cutoff(self,cutoff)
   ! Set the "cutoff" for the exponential prefactor for the product of
   ! two gaussian primitives, below which that primitive pair is
   ! isgnored in the calculation of the ERI's.
      type(SHELL2_TYPE), INOUT :: self
      REAL, IN :: cutoff
   ENSURE(cutoff>ZERO,"SHELL2:set_pair_cutoff ... cutoff must be pisitive")
      if (FALSE) self = self
      pair_cutoff = cutoff
   end subroutine

   PURE subroutine set_and_precompute_from(self,shellpr,pos_a,pos_b,same_center,skip)
   ! Set the a and b parts from "shellpr" but override the positions.
   ! "same_center" should be set TRUE if pos_a==pos_b.  "skip" can be
   ! set TRUE if NOT same_center; it skips gaussian product pairs
   ! which are too small to worry about.
      type(SHELL2_TYPE), OUT :: self
      type(SHELL1PAIR_TYPE), IN :: shellpr
      VEC(REAL,:), IN :: pos_a
      VEC(REAL,:), IN :: pos_b
      BIN, IN :: same_center
      BIN, IN :: skip
      VEC(REAL,3) :: AB,At,P,b_pos_b,av
      REAL :: a,b,r2ab,b_r2ab,ab_inv,prefac,s,cp,cc
      INT :: i,j,bg,ag

      ! Pointer assign
      self%a = shellpr%a
      self%b = shellpr%b

      ! Shells must be the same, necessary?
      self%is_spherical = shellpr%a%is_spherical

      ! Overwrite positions
      self%a%position = pos_a
      self%b%position = pos_b

      ! Set coming in to save a pos test
      self%same_center = same_center
      self%n_gaussian_pairs = shellpr%n_gaussian_pairs
      self%l_max = shellpr%l_max
      self%l_min = shellpr%l_min
      self%l_sum = shellpr%l_sum
      self%first_gaussian = shellpr%first_gaussian
      self%last_gaussian = shellpr%last_gaussian
      self%n_gaussian = shellpr%n_gaussian

      ! Destroy only *some* pointer parts
      self%destroy_all = FALSE  ! <--------

      ! Now make the gaussian-primitive-pair info ...

      ! Pointer assign
      self%exponent_sum = shellpr%exponent_sum
      self%exponent_inv = shellpr%exponent_inv
      call create_(self%pair_center,3,self%n_gaussian_pairs)
      call create_(self%center_diff,3,self%n_gaussian_pairs)
      if (NOT skip) then  ! do all pairs
         if (same_center) then

            ! Pointer assign
            self%contraction_prefactor = shellpr%contraction_prefactor
            i = 0
            do bg = 1,self%b%n_cc
               do ag = 1,self%a%n_cc
                  i = i+1
            ! .contraction_prefactor(i)  = shellpr.contraction_prefactor(i)
                  self%pair_center(:,i) = pos_a
                  self%center_diff(:,i) = ZERO
               end do
            end do
            self%av_pair_center = pos_a
         else

            ! Create
            call create_(self%contraction_prefactor,self%n_gaussian_pairs)
            AB = pos_a-pos_b

            ! Want position of shell1 with higher angular momentum.
            if (self%a%l>self%b%l) then; At = pos_a
            else; At = pos_b
            end if
            s = ZERO
            av = ZERO
            r2ab = dot_product(AB,AB)
            i = 0
            do bg = 1,self%b%n_cc
               b = self%b%exponent(bg)
               b_pos_b = b*pos_b
               b_r2ab = b*r2ab
               do ag = 1,self%a%n_cc
                  i = i+1
                  a = self%a%exponent(ag)
                  ab_inv = self%exponent_inv(i)
                  cp = shellpr%contraction_prefactor(i)
                  prefac = exp(-a*b_r2ab*ab_inv)
                  self%contraction_prefactor(i) = prefac*cp
                  P = (b_pos_b+a*pos_a)*ab_inv
                  self%pair_center(:,i) = P  ! This can't this be precalculated
                  self%center_diff(:,i) = P-At  ! it could be compressed, below
                  cc = cp/(ab_inv*sqrt(ab_inv))
                  s = s+cc
                  av = P*cc
               end do
            end do
            self%av_pair_center = av/s
         end if
      else  ! skip small pairs

         ! Create
         call create_(self%contraction_prefactor,self%n_gaussian_pairs)
         AB = pos_a-pos_b

         ! Want position of shell1 with higher angular momentum.
         if (self%a%l>self%b%l) then; At = pos_a
         else; At = pos_b
         end if
         s = ZERO
         av = ZERO
         r2ab = dot_product(AB,AB)
         i = 0
         j = 0
         do bg = 1,self%b%n_cc
            b = self%b%exponent(bg)
            b_pos_b = b*pos_b
            b_r2ab = b*r2ab
            do ag = 1,self%a%n_cc
               i = i+1
               a = self%a%exponent(ag)
               ab_inv = self%exponent_inv(i)
               prefac = exp(-a*b_r2ab*ab_inv)
               if (prefac<pair_cutoff) cycle  ! skip
               j = j+1
               cp = shellpr%contraction_prefactor(i)
               self%contraction_prefactor(i) = prefac*cp
               P = (b_pos_b+a*pos_a)*ab_inv
               self%pair_center(:,j) = P
               self%center_diff(:,j) = P-At
               cc = cp/(ab_inv*sqrt(ab_inv))
               s = s+cc
               av = P*cc
            end do
         end do
         self%av_pair_center = av/s
      end if
   end subroutine

   PURE subroutine precalculate(self)
   ! Set up some commonly used intermediates
      type(SHELL2_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: anorm,bnorm
      VEC(REAL,3) :: AB,At,P
      REAL :: b_cc,a,b,exp_sum,exp_inv,r2ab,b_r2ab,a_exp_inv
      INT :: ag,bg,i
      self%l_max = max(self%a%l,self%b%l)
      self%l_min = min(self%a%l,self%b%l)
      self%l_sum = self%a%l+self%b%l
      self%first_gaussian = n_comp_up_to_(self%l_max-1)+1
      self%last_gaussian = n_comp_up_to_(self%l_sum)
      self%n_gaussian = self%last_gaussian-self%first_gaussian+1
      self%n_gaussian_pairs = self%a%n_cc*self%b%n_cc
      self%destroy_all = TRUE
      call create_(self%exponent_sum,self%n_gaussian_pairs)
      call create_(self%exponent_inv,self%n_gaussian_pairs)
      call create_(self%contraction_prefactor,self%n_gaussian_pairs)
      call create_(self%pair_center,3,self%n_gaussian_pairs)
      call create_(self%center_diff,3,self%n_gaussian_pairs)
      call create_(anorm,self%a%n_comp)
      call create_(bnorm,self%b%n_comp)
      AB = self%a%position-self%b%position

      ! Want position of shell1 with higher angular momentum.
      if (self%a%l>self%b%l) then; At = self%a%position
      else; At = self%b%position
      end if
      r2ab = dot_product(AB,AB)
      i = 0
      do bg = 1,self%b%n_cc
         b = self%b%exponent(bg)
         b_r2ab = b*r2ab
         b_cc = self%b%contraction(bg)
         do ag = 1,self%a%n_cc
            i = i+1
            a = self%a%exponent(ag)
            exp_sum = a+b
            exp_inv = ONE/exp_sum
            P = (b*self%b%position+a*self%a%position)*exp_inv
            self%exponent_sum(i) = exp_sum
            self%exponent_inv(i) = exp_inv
            a_exp_inv = a*exp_inv
            self%contraction_prefactor(i) = b_cc*self%a%contraction(ag)*exp_inv*sqrt(exp_inv)*exp(-b_r2ab*a_exp_inv)
            self%pair_center(:,i) = P
            self%center_diff(:,i) = P-At
         end do
      end do

   ! .kappa_max = maxval(.contraction_prefactor/.exponent_inv**(THREE/TWO))
   end subroutine


!  ===========================
!  Fourier transform integrals
!  ===========================

   PURE function skip_ft_0(self,cutoff) result (res)
   ! Whether the ft for this shell pair is too small.
      type(SHELL2_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      REAL :: gamma,g1
      REAL :: e2,ex,ey,ez
      REAL :: e000x,e000y,e000z
      INT :: a,b,t_max
      MAT3(REAL,:,:,:), allocatable :: e
      type(GAUSSIAN2_TYPE) :: G
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      t_max = self%a%l+self%b%l
      call create_(e,[0,t_max],[0,self%a%l],[0,self%b%l])
      e = ZERO
      res = TRUE
      do a = 1,self%a%n_cc
         call set_(G,ex_a=self%a%exponent(a))
         do b = 1,self%b%n_cc
            call set_(G,ex_b=self%b%exponent(b))
            call make_e_coeff_(G,e,1); ex = maxval(e); e000x = e(0,0,0)
            call make_e_coeff_(G,e,2); ey = maxval(e); e000y = e(0,0,0)
            call make_e_coeff_(G,e,3); ez = maxval(e); e000z = e(0,0,0)
            gamma = self%a%exponent(a)+self%b%exponent(b)
            g1 = ONE/gamma
            e2 = ex*ey*ez*g1*sqrt(g1)*e000x*e000y*e000z
            if (e2>cutoff) then
               res = FALSE
               call destroy_(e)
               return
            end if
         end do
      end do
      call destroy_(e)
   end function

   PURE function skip_ft_1(self,Pmax,cutoff) result (res)
   ! Whether the ft for this shell pair is too small.
      type(SHELL2_TYPE), IN :: self
      REAL, IN :: Pmax,cutoff
      BIN :: res
      REAL :: fac,gamma,g1,e2,ex,ey,ez,e000x,e000y,e000z
      INT :: a,b,t_max
      MAT3(REAL,:,:,:), allocatable :: e
      type(GAUSSIAN2_TYPE) :: G
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      t_max = self%a%l+self%b%l
      call create_(e,[0,t_max],[0,self%a%l],[0,self%b%l])
      e = ZERO
      res = TRUE
      do a = 1,self%a%n_cc
         call set_(G,ex_a=self%a%exponent(a))
         fac = self%a%contraction(a)*Pmax
         do b = 1,self%b%n_cc
            call set_(G,ex_b=self%b%exponent(b))
            call make_e_coeff_(G,e,1); ex = maxval(e); e000x = e(0,0,0)
            call make_e_coeff_(G,e,2); ey = maxval(e); e000y = e(0,0,0)
            call make_e_coeff_(G,e,3); ez = maxval(e); e000z = e(0,0,0)
            gamma = self%a%exponent(a)+self%b%exponent(b)
            g1 = ONE/gamma
            e2 = ex*ey*ez*g1*sqrt(g1)*e000x*e000y*e000z
            if (e2*self%b%contraction(b)*fac>cutoff) then
               res = FALSE
               call destroy_(e)
               return
            end if
         end do
      end do
      call destroy_(e)
   end function


! These versions do no thermal smearing or partitioning

   PURE subroutine make_ft_static(self,res,k_pts)
   ! Calculates the Fourier transform for a product of two contracted
   ! gaussian shells, evaluated at a series of k points k_pts
   ! Dimensions of res are [k_max,.a.n_comp,.b.n_comp].
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: res
      MAT(REAL,:,:), IN :: k_pts
      MAT3(CPX,:,:,:), allocatable :: W
      INT :: i,k_max
      if (NOT self%is_spherical) then
         call make_ft_c_(self,res,k_pts)
      else if (self%l_max<2) then
         call make_ft_c_(self,res,k_pts)
      else
         k_max = size(k_pts,1)
         call create_(W,k_max,self%a%n_comp,self%b%n_comp)
         call make_ft_c_(self,W,k_pts)
         do i = 1,k_max
            call change_to_spherical_(self,W(i,:,:),res(i,:,:))
         end do
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_ft_c_0(self,res,k_pts)
   ! Calculates the Fourier transform for a product of two contracted
   ! gaussian shells, evaluated at a series of k points k_pts
   ! Dimensions of res are [k_max,.a.n_comp,.b.n_comp].
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: res
      MAT(REAL,:,:), IN :: k_pts
      call make_ft_c_(self,res,k_pts(:,1),k_pts(:,2),k_pts(:,3))
   end subroutine

   PURE subroutine make_ft_c_1(self,res,k_x,k_y,k_z)
   ! Calculates the Fourier transform for a product of two contracted
   ! gaussian shells, evaluated at a series of k points k_pts
   ! Dimensions of res are [k_max,.a.n_comp,.b.n_comp].
   ! This version has the gaussian2 stuff inlined and special cases optimised,
   ! it's pretty messy but much faster.
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: res
      VEC(REAL,:), IN :: k_x,k_y,k_z
      type(GAUSSIAN2_TYPE) :: G
      MAT3(REAL,:,:,:), allocatable :: e
      VEC(REAL,3) :: r
      VEC(CPX,:), allocatable :: prefac
      VEC(REAL,:), allocatable :: k_x2,k_y2,k_z2
      MAT3(CPX,:,:,:), allocatable :: ft_x,ft_y,ft_z
      VEC(CPX,:), allocatable :: ft_x01,ft_y01,ft_z01
      VEC(CPX,:), allocatable :: ft_x10,ft_y10,ft_z10
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      INT :: k,k_max
      INT :: a,xa,ya,za,aa,fa,la
      INT :: b,xb,yb,zb,bb,fb,lb
      REAL :: pifac,PI_on_gamma,gamma,g1,g4,dot1,dot2
      REAL :: e00,ca,r2,fac
      REAL :: k1,k2,k3,P1,P2,P3
      REAL :: Ex000,Ey000,Ez000
      REAL :: Ex001,Ey001,Ez001
      REAL :: Ex101,Ey101,Ez101
      REAL :: Ex010,Ey010,Ez010
      REAL :: Ex110,Ey110,Ez110
      REAL :: Ex111,Ey111,Ez111
      REAL :: Ex211,Ey211,Ez211
      REAL :: Ex011,Ey011,Ez011
      REAL :: g1_exa,ex_a
      REAL :: g1_exb,ex_b
      k_max = size(k_x)
      res = ZERO
      call set_(G%a,self%a%l,self%a%position,ZERO)
      call set_(G%b,self%b%l,self%b%position,ZERO)
      select case (self%a%l)

      case (0)
         select case (self%b%l)

         case (0)
            ! <S|S>
            r = self%a%position-self%b%position
            r2 = dot_product(r,r)
            do a = 1,self%a%n_cc
               ca = self%a%contraction(a)
               ex_a = self%a%exponent(a)
               do b = 1,self%b%n_cc
                  ex_b = self%b%exponent(b)
                  g1 = ONE/(ex_a+ex_b)
                  g4 = -QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*ex_a
                  g1_exb = g1*ex_b
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  e00 = exp(-ex_a*ex_b*r2*g1)
                  fac = pifac*e00*ca*self%b%contraction(b)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = g4*(k1*k1+k2*k2+k3*k3)
                     res(k,1,1) = res(k,1,1)+fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
               end do
            end do

         case (1)
            ! <S|P>
            call create_(prefac,k_max)
            call create_(e,[0,1],[0,0],[0,1])
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_e_coeff_(G,e,1)
                  Ex000 = e(0,0,0)
                  Ex001 = e(0,0,1)
                  Ex101 = e(1,0,1)
                  call make_e_coeff_(G,e,2)
                  Ey000 = e(0,0,0)
                  Ey001 = e(0,0,1)
                  Ey101 = e(1,0,1)
                  call make_e_coeff_(G,e,3)
                  Ez000 = e(0,0,0)
                  Ez001 = e(0,0,1)
                  Ez101 = e(1,0,1)
                  fac = pifac*ca*self%b%contraction(bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  res(:,1,1) = res(:,1,1)+COMPLEXIFY(Ex001,Ex101*k_x)*Ey000*Ez000*prefac
                  res(:,1,2) = res(:,1,2)+COMPLEXIFY(Ey001,Ey101*k_y)*Ex000*Ez000*prefac
                  res(:,1,3) = res(:,1,3)+COMPLEXIFY(Ez001,Ez101*k_z)*Ex000*Ey000*prefac
               end do
            end do
            call destroy_(e)
            call destroy_(prefac)

         case default
            ! <S|X>
            fb = self%b%first_gaussian; lb = self%b%last_gaussian
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  do b = 0,self%b%l  ! incorporate prefac into ft_z
                     ft_z(:,0,b) = ft_z(:,0,b)*prefac
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     res(:,1,b) = res(:,1,b)+ft_x(:,0,xb)*ft_y(:,0,yb)*ft_z(:,0,zb)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_y)
            call destroy_(ft_z)
         end select

      case (1)
         select case (self%b%l)

         case (0)
            ! <P|S>
            call create_(prefac,k_max)
            call create_(e,[0,1],[0,1],[0,0])
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_e_coeff_(G,e,1)
                  Ex000 = e(0,0,0)
                  Ex010 = e(0,1,0)
                  Ex110 = e(1,1,0)
                  call make_e_coeff_(G,e,2)
                  Ey000 = e(0,0,0)
                  Ey010 = e(0,1,0)
                  Ey110 = e(1,1,0)
                  call make_e_coeff_(G,e,3)
                  Ez000 = e(0,0,0)
                  Ez010 = e(0,1,0)
                  Ez110 = e(1,1,0)
                  fac = pifac*ca*self%b%contraction(bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  res(:,1,1) = res(:,1,1)+COMPLEXIFY(Ex010,Ex110*k_x)*Ey000*Ez000*prefac
                  res(:,2,1) = res(:,2,1)+COMPLEXIFY(Ey010,Ey110*k_y)*Ex000*Ez000*prefac
                  res(:,3,1) = res(:,3,1)+COMPLEXIFY(Ez010,Ez110*k_z)*Ex000*Ey000*prefac
               end do
            end do
            call destroy_(e)
            call destroy_(prefac)

         case (1)
            ! <P|P>
            call create_(ft_x,[1,k_max],[0,1],[0,1])
            call create_(ft_y,[1,k_max],[0,1],[0,1])
            call create_(ft_z,[1,k_max],[0,1],[0,1])
            call create_(prefac,k_max)
            call create_(e,[0,2],[0,1],[0,1])
            call create_(ft_x01,k_max)
            call create_(ft_x10,k_max)
            call create_(ft_y01,k_max)
            call create_(ft_y10,k_max)
            call create_(ft_z01,k_max)
            call create_(ft_z10,k_max)
            call create_(k_x2,k_max)
            call create_(k_y2,k_max)
            call create_(k_z2,k_max)
            k_x2 = k_x*k_x
            k_y2 = k_y*k_y
            k_z2 = k_z*k_z
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  fac = pifac*ca*self%b%contraction(bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  call make_e_coeff_(G,e,1)
                  Ex000 = e(0,0,0)
                  Ex001 = e(0,0,1)
                  Ex010 = e(0,1,0)
                  Ex011 = e(0,1,1)
                  Ex101 = e(1,0,1)
                  Ex110 = e(1,1,0)
                  Ex111 = e(1,1,1)
                  Ex211 = e(2,1,1)
                  call make_e_coeff_(G,e,2)
                  Ey000 = e(0,0,0)
                  Ey001 = e(0,0,1)
                  Ey010 = e(0,1,0)
                  Ey011 = e(0,1,1)
                  Ey101 = e(1,0,1)
                  Ey110 = e(1,1,0)
                  Ey111 = e(1,1,1)
                  Ey211 = e(2,1,1)
                  call make_e_coeff_(G,e,3)
                  Ez000 = e(0,0,0)
                  Ez001 = e(0,0,1)
                  Ez010 = e(0,1,0)
                  Ez011 = e(0,1,1)
                  Ez101 = e(1,0,1)
                  Ez110 = e(1,1,0)
                  Ez111 = e(1,1,1)
                  Ez211 = e(2,1,1)
                  ft_x01 = COMPLEXIFY(Ex001,Ex101*k_x)
                  ft_x10 = COMPLEXIFY(Ex010,Ex110*k_x)
                  ft_y01 = COMPLEXIFY(Ey001,Ey101*k_y)
                  ft_y10 = COMPLEXIFY(Ey010,Ey110*k_y)
                  ft_z01 = COMPLEXIFY(Ez001,Ez101*k_z)
                  ft_z10 = COMPLEXIFY(Ez010,Ez110*k_z)
                  res(:,1,1) = res(:,1,1)+COMPLEXIFY(Ex011-Ex211*k_x2,Ex111*k_x)*Ey000*Ez000*prefac
                  res(:,2,1) = res(:,2,1)+ft_x01*ft_y10*Ez000*prefac
                  res(:,3,1) = res(:,3,1)+ft_x01*Ey000*ft_z10*prefac
                  res(:,1,2) = res(:,1,2)+ft_x10*ft_y01*Ez000*prefac
                  res(:,2,2) = res(:,2,2)+COMPLEXIFY(Ey011-Ey211*k_y2,Ey111*k_y)*Ex000*Ez000*prefac
                  res(:,3,2) = res(:,3,2)+Ex000*ft_y01*ft_z10*prefac
                  res(:,1,3) = res(:,1,3)+ft_x10*Ey000*ft_z01*prefac
                  res(:,2,3) = res(:,2,3)+Ex000*ft_y10*ft_z01*prefac
                  res(:,3,3) = res(:,3,3)+COMPLEXIFY(Ez011-Ez211*k_z2,Ez111*k_z)*Ex000*Ey000*prefac
               end do
            end do
            call destroy_(k_z2)
            call destroy_(k_y2)
            call destroy_(k_x2)
            call destroy_(ft_z10)
            call destroy_(ft_z01)
            call destroy_(ft_y10)
            call destroy_(ft_y01)
            call destroy_(ft_x10)
            call destroy_(ft_x01)
            call destroy_(e)
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)

         case default
            ! <P|X>
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fb = self%b%first_gaussian
            lb = self%b%last_gaussian
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  do b = 0,self%b%l
                     ft_z(:,0,b) = ft_z(:,0,b)*prefac
                     ft_z(:,1,b) = ft_z(:,1,b)*prefac
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     res(:,1,b) = res(:,1,b)+ft_x(:,1,xb)*ft_y(:,0,yb)*ft_z(:,0,zb)
                     res(:,2,b) = res(:,2,b)+ft_x(:,0,xb)*ft_y(:,1,yb)*ft_z(:,0,zb)
                     res(:,3,b) = res(:,3,b)+ft_x(:,0,xb)*ft_y(:,0,yb)*ft_z(:,1,zb)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)
         end select

      case default
         select case (self%b%l)

         case (0)
            ! <X|S>
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fa = self%a%first_gaussian
            la = self%a%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  do a = 0,self%a%l
                     ft_z(:,a,0) = ft_z(:,a,0)*prefac
                  end do
                  do a = 1,self%a%n_comp
                     xa = ax(a)
                     ya = ay(a)
                     za = az(a)
                     res(:,a,1) = res(:,a,1)+ft_x(:,xa,0)*ft_y(:,ya,0)*ft_z(:,za,0)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)

         case (1)
            ! <X|P>
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fa = self%a%first_gaussian; la = self%a%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  do a = 0,self%a%l  ! incorporate prefac into ft_z
                     ft_z(:,a,0) = ft_z(:,a,0)*prefac
                     ft_z(:,a,1) = ft_z(:,a,1)*prefac
                  end do
                  do a = 1,self%a%n_comp
                     xa = ax(a)
                     ya = ay(a)
                     za = az(a)
                     res(:,a,1) = res(:,a,1)+ft_x(:,xa,1)*ft_y(:,ya,0)*ft_z(:,za,0)
                     res(:,a,2) = res(:,a,2)+ft_x(:,xa,0)*ft_y(:,ya,1)*ft_z(:,za,0)
                     res(:,a,3) = res(:,a,3)+ft_x(:,xa,0)*ft_y(:,ya,0)*ft_z(:,za,1)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)

         case default
            ! <X|Y>
            ! This is the general routine.  It includes reduced multiplication,
            ! and use of pointers to minimise array finding.
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fa = self%a%first_gaussian
            la = self%a%last_gaussian
            fb = self%b%first_gaussian
            lb = self%b%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)
                  do k = 1,k_max
                     k1 = k_x(k)
                     k2 = k_y(k)
                     k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  ! Incorporate prefac into ft_z
                  do b = 0,self%b%l
                     do a = 0,self%a%l
                        ft_z(:,a,b) = ft_z(:,a,b)*prefac
                     end do
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     do a = 1,self%a%n_comp
                        xa = ax(a)
                        ya = ay(a)
                        za = az(a)
                        res(:,a,b) = res(:,a,b)+ft_x(:,xa,xb)*ft_y(:,ya,yb)*ft_z(:,za,zb)
                     end do
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)
         end select
      end select
   end subroutine


! This versions does only partitioning *no thermal*
! It is intended for single-atom partitions

   PURE subroutine make_FT_parted_0(self,FT,k_pts,partition)
   ! Calculate the "FT[k_max,.a.n_comp,.b_n_comp]" for the two shells
   ! in "self" for "k_pts" partitioned with factors "partition" of
   ! dimension [k_max,.a.n_comp,.b.n_comp].
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: FT
      MAT(REAL,:,:), IN :: k_pts
      MAT(REAL,:,:), IN :: partition
      call make_FT_parted_(self,FT,k_pts(:,1),k_pts(:,2),k_pts(:,3),partition)
   end subroutine

   PURE subroutine make_FT_parted_1(self,FT,k1,k2,k3,partition)
   ! Calculate the "FT[k_max,.a.n_comp,.b_n_comp]" for the two shells
   ! in "self" for series of k points (k1,k2,k3) partitioned with
   ! factors "partition" of dimension [k_max,.a.n_comp,.b.n_comp].
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: FT
      VEC(REAL,:), IN :: k1,k2,k3
      MAT(REAL,:,:), IN :: partition
      MAT(CPX,:,:), allocatable :: C,S
      INT :: i
      if (NOT self%is_spherical) then
         call make_FT_parted_c_(self,FT,k1,k2,k3,partition)
      else if (self%l_max<2) then
         call make_FT_parted_c_(self,FT,k1,k2,k3,partition)
      else
         call make_FT_parted_c_(self,FT,k1,k2,k3,partition)
         call create_(C,self%a%n_comp,self%b%n_comp)
         call create_(S,self%a%n_comp,self%b%n_comp)
         do i = 1,size(FT,1)
            C = FT(i,:,:)
            call change_to_spherical_(self,C,S)
            FT(i,:,:) = S
         end do
         call destroy_(S)
         call destroy_(C)
      end if
   end subroutine

   PURE subroutine make_FT_parted_c(self,res,k_x,k_y,k_z,partition)
   ! Calculates the Fourier transform for a product of two contracted
   ! gaussian shells, evaluated at a series of k points k_pts
   ! Dimensions of res are [k_max,.a.n_comp,.b.n_comp].
   ! This version has the gaussian2 stuff inlined and special cases optimised,
   ! it's pretty messy but much faster.
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: res
      VEC(REAL,:), IN :: k_x,k_y,k_z
      MAT(REAL,:,:), IN :: partition
      type(GAUSSIAN2_TYPE) :: G
      MAT3(REAL,:,:,:), allocatable :: e
      VEC(REAL,3) :: r
      VEC(CPX,:), allocatable :: prefac
      VEC(REAL,:), allocatable :: k_x2,k_y2,k_z2
      MAT3(CPX,:,:,:), allocatable :: ft_x,ft_y,ft_z
      VEC(CPX,:), allocatable :: ft_x01,ft_y01,ft_z01
      VEC(CPX,:), allocatable :: ft_x10,ft_y10,ft_z10
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      INT :: k,k_max
      INT :: a,xa,ya,za,aa,fa,la
      INT :: b,xb,yb,zb,bb,fb,lb
      REAL :: pifac,PI_on_gamma,gamma,g1,g4,dot1,dot2
      REAL :: e00,ca,r2,fac
      REAL :: k1,k2,k3,P1,P2,P3
      REAL :: Ex000,Ey000,Ez000
      REAL :: Ex001,Ey001,Ez001
      REAL :: Ex101,Ey101,Ez101
      REAL :: Ex010,Ey010,Ez010
      REAL :: Ex110,Ey110,Ez110
      REAL :: Ex111,Ey111,Ez111
      REAL :: Ex211,Ey211,Ez211
      REAL :: Ex011,Ey011,Ez011
      REAL :: g1_exa,ex_a
      REAL :: g1_exb,ex_b
      k_max = size(k_x)
      res = IMAGIFY(ZERO)
      call set_(G%a,self%a%l,self%a%position,ZERO)
      call set_(G%b,self%b%l,self%b%position,ZERO)
      select case (self%a%l)

      case (0)
         select case (self%b%l)

         case (0)

            ! <S|S>
            r = self%a%position-self%b%position
            r2 = dot_product(r,r)
            do a = 1,self%a%n_cc
               ca = self%a%contraction(a)
               ex_a = self%a%exponent(a)
               do b = 1,self%b%n_cc
                  ex_b = self%b%exponent(b)
                  g1 = ONE/(ex_a+ex_b)
                  g4 = -QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*ex_a
                  g1_exb = g1*ex_b
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  e00 = exp(-ex_a*ex_b*r2*g1)
                  fac = pifac*e00*ca*self%b%contraction(b)*partition(a,b)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = g4*(k1*k1+k2*k2+k3*k3)
                     res(k,1,1) = res(k,1,1)+fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
               end do
            end do

         case (1)

            ! <S|P>
            call create_(prefac,k_max)
            call create_(e,[0,1],[0,0],[0,1])
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_e_coeff_(G,e,1)
                  Ex000 = e(0,0,0)
                  Ex001 = e(0,0,1)
                  Ex101 = e(1,0,1)
                  call make_e_coeff_(G,e,2)
                  Ey000 = e(0,0,0)
                  Ey001 = e(0,0,1)
                  Ey101 = e(1,0,1)
                  call make_e_coeff_(G,e,3)
                  Ez000 = e(0,0,0)
                  Ez001 = e(0,0,1)
                  Ez101 = e(1,0,1)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  res(:,1,1) = res(:,1,1)+COMPLEXIFY(Ex001,Ex101*k_x)*Ey000*Ez000*prefac
                  res(:,1,2) = res(:,1,2)+COMPLEXIFY(Ey001,Ey101*k_y)*Ex000*Ez000*prefac
                  res(:,1,3) = res(:,1,3)+COMPLEXIFY(Ez001,Ez101*k_z)*Ex000*Ey000*prefac
               end do
            end do
            call destroy_(e)
            call destroy_(prefac)

         case default
            ! <S|X>
            fb = self%b%first_gaussian; lb = self%b%last_gaussian
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  do b = 0,self%b%l  ! incorporate prefac into ft_z
                     ft_z(:,0,b) = ft_z(:,0,b)*prefac
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     res(:,1,b) = res(:,1,b)+ft_x(:,0,xb)*ft_y(:,0,yb)*ft_z(:,0,zb)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_y)
            call destroy_(ft_z)
         end select  ! --- case (.b.l)

      case (1)
         select case (self%b%l)

         case (0)
            ! <P|S>
            call create_(prefac,k_max)
            call create_(e,[0,1],[0,1],[0,0])
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_e_coeff_(G,e,1)
                  Ex000 = e(0,0,0)
                  Ex010 = e(0,1,0)
                  Ex110 = e(1,1,0)
                  call make_e_coeff_(G,e,2)
                  Ey000 = e(0,0,0)
                  Ey010 = e(0,1,0)
                  Ey110 = e(1,1,0)
                  call make_e_coeff_(G,e,3)
                  Ez000 = e(0,0,0)
                  Ez010 = e(0,1,0)
                  Ez110 = e(1,1,0)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  res(:,1,1) = res(:,1,1)+COMPLEXIFY(Ex010,Ex110*k_x)*Ey000*Ez000*prefac
                  res(:,2,1) = res(:,2,1)+COMPLEXIFY(Ey010,Ey110*k_y)*Ex000*Ez000*prefac
                  res(:,3,1) = res(:,3,1)+COMPLEXIFY(Ez010,Ez110*k_z)*Ex000*Ey000*prefac
               end do
            end do
            call destroy_(e)
            call destroy_(prefac)

         case (1)
            ! <P|P>
            call create_(ft_x,[1,k_max],[0,1],[0,1])
            call create_(ft_y,[1,k_max],[0,1],[0,1])
            call create_(ft_z,[1,k_max],[0,1],[0,1])
            call create_(prefac,k_max)
            call create_(e,[0,2],[0,1],[0,1])
            call create_(ft_x01,k_max)
            call create_(ft_x10,k_max)
            call create_(ft_y01,k_max)
            call create_(ft_y10,k_max)
            call create_(ft_z01,k_max)
            call create_(ft_z10,k_max)
            call create_(k_x2,k_max)
            call create_(k_y2,k_max)
            call create_(k_z2,k_max)
            k_x2 = k_x*k_x
            k_y2 = k_y*k_y
            k_z2 = k_z*k_z
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  call make_e_coeff_(G,e,1)
                  Ex000 = e(0,0,0)
                  Ex001 = e(0,0,1)
                  Ex010 = e(0,1,0)
                  Ex011 = e(0,1,1)
                  Ex101 = e(1,0,1)
                  Ex110 = e(1,1,0)
                  Ex111 = e(1,1,1)
                  Ex211 = e(2,1,1)
                  call make_e_coeff_(G,e,2)
                  Ey000 = e(0,0,0)
                  Ey001 = e(0,0,1)
                  Ey010 = e(0,1,0)
                  Ey011 = e(0,1,1)
                  Ey101 = e(1,0,1)
                  Ey110 = e(1,1,0)
                  Ey111 = e(1,1,1)
                  Ey211 = e(2,1,1)
                  call make_e_coeff_(G,e,3)
                  Ez000 = e(0,0,0)
                  Ez001 = e(0,0,1)
                  Ez010 = e(0,1,0)
                  Ez011 = e(0,1,1)
                  Ez101 = e(1,0,1)
                  Ez110 = e(1,1,0)
                  Ez111 = e(1,1,1)
                  Ez211 = e(2,1,1)
                  ft_x01 = COMPLEXIFY(Ex001,Ex101*k_x)
                  ft_x10 = COMPLEXIFY(Ex010,Ex110*k_x)
                  ft_y01 = COMPLEXIFY(Ey001,Ey101*k_y)
                  ft_y10 = COMPLEXIFY(Ey010,Ey110*k_y)
                  ft_z01 = COMPLEXIFY(Ez001,Ez101*k_z)
                  ft_z10 = COMPLEXIFY(Ez010,Ez110*k_z)
                  res(:,1,1) = res(:,1,1)+COMPLEXIFY(Ex011-Ex211*k_x2,Ex111*k_x)*Ey000*Ez000*prefac
                  res(:,2,1) = res(:,2,1)+ft_x01*ft_y10*Ez000*prefac
                  res(:,3,1) = res(:,3,1)+ft_x01*Ey000*ft_z10*prefac
                  res(:,1,2) = res(:,1,2)+ft_x10*ft_y01*Ez000*prefac
                  res(:,2,2) = res(:,2,2)+COMPLEXIFY(Ey011-Ey211*k_y2,Ey111*k_y)*Ex000*Ez000*prefac
                  res(:,3,2) = res(:,3,2)+Ex000*ft_y01*ft_z10*prefac
                  res(:,1,3) = res(:,1,3)+ft_x10*Ey000*ft_z01*prefac
                  res(:,2,3) = res(:,2,3)+Ex000*ft_y10*ft_z01*prefac
                  res(:,3,3) = res(:,3,3)+COMPLEXIFY(Ez011-Ez211*k_z2,Ez111*k_z)*Ex000*Ey000*prefac
               end do
            end do
            call destroy_(k_z2)
            call destroy_(k_y2)
            call destroy_(k_x2)
            call destroy_(ft_z10)
            call destroy_(ft_z01)
            call destroy_(ft_y10)
            call destroy_(ft_y01)
            call destroy_(ft_x10)
            call destroy_(ft_x01)
            call destroy_(e)
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)

         case default
            ! <P|X>
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fb = self%b%first_gaussian
            lb = self%b%last_gaussian
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  do b = 0,self%b%l
                     ft_z(:,0,b) = ft_z(:,0,b)*prefac
                     ft_z(:,1,b) = ft_z(:,1,b)*prefac
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     res(:,1,b) = res(:,1,b)+ft_x(:,1,xb)*ft_y(:,0,yb)*ft_z(:,0,zb)
                     res(:,2,b) = res(:,2,b)+ft_x(:,0,xb)*ft_y(:,1,yb)*ft_z(:,0,zb)
                     res(:,3,b) = res(:,3,b)+ft_x(:,0,xb)*ft_y(:,0,yb)*ft_z(:,1,zb)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)
         end select  ! -- case (.b.l)

      case default
         select case (self%b%l)

         case (0)

            ! <X|S>
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fa = self%a%first_gaussian
            la = self%a%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  do a = 0,self%a%l
                     ft_z(:,a,0) = ft_z(:,a,0)*prefac
                  end do
                  do a = 1,self%a%n_comp
                     xa = ax(a)
                     ya = ay(a)
                     za = az(a)
                     res(:,a,1) = res(:,a,1)+ft_x(:,xa,0)*ft_y(:,ya,0)*ft_z(:,za,0)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)

         case (1)

            ! <X|P>
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fa = self%a%first_gaussian; la = self%a%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  do a = 0,self%a%l  ! incorporate prefac into ft_z
                     ft_z(:,a,0) = ft_z(:,a,0)*prefac
                     ft_z(:,a,1) = ft_z(:,a,1)*prefac
                  end do
                  do a = 1,self%a%n_comp
                     xa = ax(a)
                     ya = ay(a)
                     za = az(a)
                     res(:,a,1) = res(:,a,1)+ft_x(:,xa,1)*ft_y(:,ya,0)*ft_z(:,za,0)
                     res(:,a,2) = res(:,a,2)+ft_x(:,xa,0)*ft_y(:,ya,1)*ft_z(:,za,0)
                     res(:,a,3) = res(:,a,3)+ft_x(:,xa,0)*ft_y(:,ya,0)*ft_z(:,za,1)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)

         case default

            ! <X|Y>
            ! This is the general routine.  It includes reduced multiplication,
            ! and use of pointers to minimise array finding.
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fa = self%a%first_gaussian
            la = self%a%last_gaussian
            fb = self%b%first_gaussian
            lb = self%b%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(COMPLEXIFY(dot2,dot1))
                  end do
                  ! Incorporate prefac into ft_z
                  do b = 0,self%b%l
                     do a = 0,self%a%l
                        ft_z(:,a,b) = ft_z(:,a,b)*prefac
                     end do
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     do a = 1,self%a%n_comp
                        xa = ax(a)
                        ya = ay(a)
                        za = az(a)
                        res(:,a,b) = res(:,a,b)+ft_x(:,xa,xb)*ft_y(:,ya,yb)*ft_z(:,za,zb)
                     end do
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)
         end select  ! -- case (.b.l)
      end select
   end subroutine


! ====================================================
! This versions does thermal smearing and partitioning
! ******** It seems to be the main routine *********
! ====================================================

   PURE subroutine make_FT(self,res,k_pts,thermal,partition)
   ! Calculates "res", the Fourier transform for this contracted
   ! shell pair a product of two contracted gaussian shells, evaluated
   ! at a series of k points "k_pts". "thermal" smearing and "partition"
   ! factors are used.
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: res
      MAT(REAL,:,:), IN :: k_pts
      MAT3(REAL,:,:,:), IN :: thermal
      MAT(REAL,:,:), IN :: partition
      MAT3(CPX,:,:,:), allocatable :: W
      INT :: i
   ENSURE(size(res,1)==size(k_pts,1),"SHELL2:make_FT ... incomptaible dim1, res and k_pts")
   ENSURE(size(res,2)==self%a%n_comp,"SHELL2:make_FT ... incomptaible dim2, res and shell a")
   ENSURE(size(res,3)==self%b%n_comp,"SHELL2:make_FT ... incomptaible dim2, res and shell a")

   ! read_option :: BIN = TRUE
   ! option :: STR = "?"
   ! if (read_option) then
   !    stdin.read(option)
   !    stdout.show("Using FT routine ",option)
   !    read_option = FALSE
   ! end
   ! select case (option)
   ! case ("old "); .make_ft_old(res,k_pts,thermal,partition)
   ! case ("old2"); .make_ft_old2(res,k_pts,thermal,partition)
   ! case ("v0  "); .make_ft_v0(res,k_pts,thermal,partition)
   ! case ("v1  "); .make_ft_v1(res,k_pts,thermal,partition)
   ! case ("v2  "); .make_ft_v2(res,k_pts,thermal,partition)
   ! case ("v3  "); .make_ft_v3(res,k_pts,thermal,partition)
   ! case default;  UNKNOWN(option)
   ! end
   ! .make_ft_v3(res,k_pts,thermal,partition)
      if (NOT self%is_spherical) then

         ! Cartesian
         call make_ft_c_(self,res,k_pts,thermal,partition)
      else if (self%l_max<2) then

         ! Spherical s & p
         call make_ft_c_(self,res,k_pts,thermal,partition)
      else

         ! Spherical general
         call create_(W,size(res,1),self%a%n_comp,self%b%n_comp)
         call make_ft_c_(self,W,k_pts,thermal,partition)
         do i = 1,size(res,1)
            call change_to_spherical_(self,W(i,:,:),res(i,:,:))
         end do
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_ft_c_2(self,res,k_pts,thermal,partition)
   ! Calculates the Fourier transform for a product of two contracted
   ! gaussian shells, evaluated at a series of k points k_pts
   ! Dimensions of res are [k_max,.a.n_comp,.b.n_comp].
   ! Also adds in thermal correction and partition factors.
   ! This version has the gaussian2 stuff inlined and special cases
   ! optimised, it's pretty messy but much faster.
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: res
      MAT(REAL,:,:), IN :: k_pts
      MAT3(REAL,:,:,:), IN :: thermal
      MAT(REAL,:,:), IN :: partition
      MAT3(REAL,:,:,:), allocatable :: e
      MAT3(CPX,:,:,:), allocatable :: ft_x,ft_y,ft_z
      VEC(REAL,:), allocatable :: k_x,k_y,k_z,k_x2,k_y2,k_z2
      VEC(CPX,:), allocatable :: prefac,ft_x01,ft_x10,ft_y01,ft_y10,ft_z01,ft_z10
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      REAL :: fac,pi32,g1,g2,g4,dot1,dot2,k1,k2,k3,P1,P2,P3,ex_a,ex_b
      INT :: a,b,i,k,xa,ya,za,xb,yb,zb,aa,bb,k_max,fa,la,fb,lb
      REAL :: Ex001,Ey001,Ez001,Ex101,Ey101,Ez101
      REAL :: Ex010,Ey010,Ez010,Ex110,Ey110,Ez110,Ex111,Ey111,Ez111
      REAL :: Ex211,Ey211,Ez211,Ex011,Ey011,Ez011
      REAL :: AB1,AB2,AB3,PA1,PA2,PA3,PB1,PB2,PB3
      pi32 = sqrt(PI)*PI
      k_max = size(k_pts,1)
      k_x = k_pts(:,1)
      k_y = k_pts(:,2)
      k_z = k_pts(:,3)
      PA1 = self%a%position(1)
      PA2 = self%a%position(2)
      PA3 = self%a%position(3)
      PB1 = self%b%position(1)
      PB2 = self%b%position(2)
      PB3 = self%b%position(3)
      AB1 = PA1-PB1
      AB2 = PA2-PB2
      AB3 = PA3-PB3
      res = ZERO
      select case (self%a%l)

      case (0)
         select case (self%b%l)

         ! <S|S>
         case (0)  ! (l_a,l_b) = (0,0)
            i = 0
            do b = 1,self%b%n_cc
               do a = 1,self%a%n_cc
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g4 = -QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  fac = pi32*self%contraction_prefactor(i)*partition(a,b)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = g4*(k1*k1+k2*k2+k3*k3)
                     res(k,1,1) = res(k,1,1)+thermal(k,a,b)*(fac*exp(cmplx(dot2,dot1,kind=CPX_KIND)))
                  end do
               end do
            end do

         ! <S|P>
         case (1)  ! (l_a,l_b) = (0,1)
            call create_(prefac,k_max)
            call create_(e,[0,1],[0,0],[0,1])
            i = 0
            do b = 1,self%b%n_cc
               ex_b = self%b%exponent(b)
               do a = 1,self%a%n_cc
                  i = i+1
                  ex_a = self%a%exponent(a)
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_e_coeff_(self,e,AB1,ex_a,ex_b,g1,g2)
                  Ex001 = e(0,0,1)
                  Ex101 = e(1,0,1)
                  call make_e_coeff_(self,e,AB2,ex_a,ex_b,g1,g2)
                  Ey001 = e(0,0,1)
                  Ey101 = e(1,0,1)
                  call make_e_coeff_(self,e,AB3,ex_a,ex_b,g1,g2)
                  Ez001 = e(0,0,1)
                  Ez101 = e(1,0,1)
                  fac = pi32*self%contraction_prefactor(i)*partition(a,b)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*thermal(k,a,b)
                  end do
                  res(:,1,1) = res(:,1,1)+cmplx(Ex001,Ex101*k_x,kind=CPX_KIND)*prefac
                  res(:,1,2) = res(:,1,2)+cmplx(Ey001,Ey101*k_y,kind=CPX_KIND)*prefac
                  res(:,1,3) = res(:,1,3)+cmplx(Ez001,Ez101*k_z,kind=CPX_KIND)*prefac
               end do
            end do
            call destroy_(e)
            call destroy_(prefac)

         ! <S|X>
         case default  ! (l_a,l_b) = (0,X)
            fb = self%b%first_gaussian
            lb = self%b%last_gaussian
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_ft_component_(self,ft_x,AB1,ex_a,ex_b,g1,g2,k_x,k_max)
                  call make_ft_component_(self,ft_y,AB2,ex_a,ex_b,g1,g2,k_y,k_max)
                  call make_ft_component_(self,ft_z,AB3,ex_a,ex_b,g1,g2,k_z,k_max)
                  fac = pi32*self%contraction_prefactor(i)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*thermal(k,aa,bb)
                  end do
                  do b = 0,self%b%l  ! incorporate prefac into ft_z
                     ft_z(:,0,b) = ft_z(:,0,b)*prefac(:)
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     res(:,1,b) = res(:,1,b)+ft_x(:,0,xb)*ft_y(:,0,yb)*ft_z(:,0,zb)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_y)
            call destroy_(ft_z)
         end select

      case (1)
         select case (self%b%l)

         ! <P|S>
         case (0)  ! (l_a,l_b) = (1,0)
            call create_(prefac,k_max)
            call create_(e,[0,1],[0,1],[0,0])
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_e_coeff_(self,e,AB1,ex_a,ex_b,g1,g2)
                  Ex010 = e(0,1,0)
                  Ex110 = e(1,1,0)
                  call make_e_coeff_(self,e,AB2,ex_a,ex_b,g1,g2)
                  Ey010 = e(0,1,0)
                  Ey110 = e(1,1,0)
                  call make_e_coeff_(self,e,AB3,ex_a,ex_b,g1,g2)
                  Ez010 = e(0,1,0)
                  Ez110 = e(1,1,0)
                  fac = pi32*self%contraction_prefactor(i)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*thermal(k,aa,bb)
                  end do
                  res(:,1,1) = res(:,1,1)+cmplx(Ex010,Ex110*k_x,kind=CPX_KIND)*prefac
                  res(:,2,1) = res(:,2,1)+cmplx(Ey010,Ey110*k_y,kind=CPX_KIND)*prefac
                  res(:,3,1) = res(:,3,1)+cmplx(Ez010,Ez110*k_z,kind=CPX_KIND)*prefac
               end do
            end do
            call destroy_(e)
            call destroy_(prefac)

         ! <P|P>
         case (1)  ! (l_a,l_b) = (1,1)
            call create_(ft_x,[1,k_max],[0,1],[0,1])
            call create_(ft_y,[1,k_max],[0,1],[0,1])
            call create_(ft_z,[1,k_max],[0,1],[0,1])
            call create_(prefac,k_max)
            call create_(e,[0,2],[0,1],[0,1])
            call create_(ft_x01,k_max); call create_(ft_y01,k_max); call create_(ft_z01,k_max)
            call create_(ft_x10,k_max); call create_(ft_y10,k_max); call create_(ft_z10,k_max)
            k_x2 = k_x*k_x
            k_y2 = k_y*k_y
            k_z2 = k_z*k_z
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  fac = pi32*self%contraction_prefactor(i)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*thermal(k,aa,bb)
                  end do
                  call make_e_coeff_(self,e,AB1,ex_a,ex_b,g1,g2)
                  Ex001 = e(0,0,1)
                  Ex010 = e(0,1,0)
                  Ex011 = e(0,1,1)
                  Ex101 = e(1,0,1)
                  Ex110 = e(1,1,0)
                  Ex111 = e(1,1,1)
                  Ex211 = e(2,1,1)
                  call make_e_coeff_(self,e,AB2,ex_a,ex_b,g1,g2)
                  Ey001 = e(0,0,1)
                  Ey010 = e(0,1,0)
                  Ey011 = e(0,1,1)
                  Ey101 = e(1,0,1)
                  Ey110 = e(1,1,0)
                  Ey111 = e(1,1,1)
                  Ey211 = e(2,1,1)
                  call make_e_coeff_(self,e,AB3,ex_a,ex_b,g1,g2)
                  Ez001 = e(0,0,1)
                  Ez010 = e(0,1,0)
                  Ez011 = e(0,1,1)
                  Ez101 = e(1,0,1)
                  Ez110 = e(1,1,0)
                  Ez111 = e(1,1,1)
                  Ez211 = e(2,1,1)
                  ft_x01 = cmplx(Ex001,Ex101*k_x,kind=CPX_KIND)
                  ft_x10 = cmplx(Ex010,Ex110*k_x,kind=CPX_KIND)
                  ft_y01 = cmplx(Ey001,Ey101*k_y,kind=CPX_KIND)
                  ft_y10 = cmplx(Ey010,Ey110*k_y,kind=CPX_KIND)
                  ft_z01 = cmplx(Ez001,Ez101*k_z,kind=CPX_KIND)
                  ft_z10 = cmplx(Ez010,Ez110*k_z,kind=CPX_KIND)
                  res(:,1,1) = res(:,1,1)+cmplx(Ex011-Ex211*k_x2,Ex111*k_x,kind=CPX_KIND)*prefac
                  res(:,2,2) = res(:,2,2)+cmplx(Ey011-Ey211*k_y2,Ey111*k_y,kind=CPX_KIND)*prefac
                  res(:,3,3) = res(:,3,3)+cmplx(Ez011-Ez211*k_z2,Ez111*k_z,kind=CPX_KIND)*prefac
                  res(:,2,1) = res(:,2,1)+ft_x01*ft_y10*prefac
                  res(:,1,2) = res(:,1,2)+ft_x10*ft_y01*prefac
                  res(:,3,1) = res(:,3,1)+ft_x01*ft_z10*prefac
                  res(:,1,3) = res(:,1,3)+ft_x10*ft_z01*prefac
                  res(:,3,2) = res(:,3,2)+ft_y01*ft_z10*prefac
                  res(:,2,3) = res(:,2,3)+ft_y10*ft_z01*prefac
               end do
            end do
            call destroy_(ft_z10); call destroy_(ft_z01); call destroy_(ft_y10)
            call destroy_(ft_y01); call destroy_(ft_x10); call destroy_(ft_x01)
            call destroy_(e); call destroy_(prefac)
            call destroy_(ft_x); call destroy_(ft_z); call destroy_(ft_y)

         ! <P|X>
         case default  ! (l_a,l_b) = (1,X)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fb = self%b%first_gaussian
            lb = self%b%last_gaussian
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_ft_component_(self,ft_x,AB1,ex_a,ex_b,g1,g2,k_x,k_max)
                  call make_ft_component_(self,ft_y,AB2,ex_a,ex_b,g1,g2,k_y,k_max)
                  call make_ft_component_(self,ft_z,AB3,ex_a,ex_b,g1,g2,k_z,k_max)
                  fac = pi32*self%contraction_prefactor(i)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*thermal(k,aa,bb)
                  end do
                  do b = 0,self%b%l
                     ft_z(:,0,b) = ft_z(:,0,b)*prefac(:)
                     ft_z(:,1,b) = ft_z(:,1,b)*prefac(:)
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     res(:,1,b) = res(:,1,b)+ft_x(:,1,xb)*ft_y(:,0,yb)*ft_z(:,0,zb)
                     res(:,2,b) = res(:,2,b)+ft_x(:,0,xb)*ft_y(:,1,yb)*ft_z(:,0,zb)
                     res(:,3,b) = res(:,3,b)+ft_x(:,0,xb)*ft_y(:,0,yb)*ft_z(:,1,zb)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x); call destroy_(ft_z); call destroy_(ft_y)
         end select

      case default

         ! <X|S>
         select case (self%b%l)

         case (0)  ! (l_a,l_b) = (X,0)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fa = self%a%first_gaussian
            la = self%a%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_ft_component_(self,ft_x,AB1,ex_a,ex_b,g1,g2,k_x,k_max)
                  call make_ft_component_(self,ft_y,AB2,ex_a,ex_b,g1,g2,k_y,k_max)
                  call make_ft_component_(self,ft_z,AB3,ex_a,ex_b,g1,g2,k_z,k_max)
                  fac = pi32*self%contraction_prefactor(i)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*thermal(k,aa,bb)
                  end do
                  do a = 0,self%a%l
                     ft_z(:,a,0) = ft_z(:,a,0)*prefac(:)
                  end do
                  do a = 1,self%a%n_comp
                     xa = ax(a)
                     ya = ay(a)
                     za = az(a)
                     res(:,a,1) = res(:,a,1)+ft_x(:,xa,0)*ft_y(:,ya,0)*ft_z(:,za,0)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x); call destroy_(ft_z); call destroy_(ft_y)

         ! <X|P>
         case (1)  ! (l_a,l_b) = (X,1)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fa = self%a%first_gaussian
            la = self%a%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_ft_component_(self,ft_x,AB1,ex_a,ex_b,g1,g2,k_x,k_max)
                  call make_ft_component_(self,ft_y,AB2,ex_a,ex_b,g1,g2,k_y,k_max)
                  call make_ft_component_(self,ft_z,AB3,ex_a,ex_b,g1,g2,k_z,k_max)
                  fac = pi32*self%contraction_prefactor(i)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*thermal(k,aa,bb)
                  end do
                  do a = 0,self%a%l  ! incorporate prefac into ft_z
                     ft_z(:,a,0) = ft_z(:,a,0)*prefac(:)
                     ft_z(:,a,1) = ft_z(:,a,1)*prefac(:)
                  end do
                  do a = 1,self%a%n_comp
                     xa = ax(a)
                     ya = ay(a)
                     za = az(a)
                     res(:,a,1) = res(:,a,1)+ft_x(:,xa,1)*ft_y(:,ya,0)*ft_z(:,za,0)
                     res(:,a,2) = res(:,a,2)+ft_x(:,xa,0)*ft_y(:,ya,1)*ft_z(:,za,0)
                     res(:,a,3) = res(:,a,3)+ft_x(:,xa,0)*ft_y(:,ya,0)*ft_z(:,za,1)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x); call destroy_(ft_z); call destroy_(ft_y)

         ! <X|X>
         case default  ! (l_a,l_b) = (X,X)

            ! This is the general routine.
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fa = self%a%first_gaussian; la = self%a%last_gaussian
            fb = self%b%first_gaussian; lb = self%b%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_ft_component_(self,ft_x,AB1,ex_a,ex_b,g1,g2,k_x,k_max)
                  call make_ft_component_(self,ft_y,AB2,ex_a,ex_b,g1,g2,k_y,k_max)
                  call make_ft_component_(self,ft_z,AB3,ex_a,ex_b,g1,g2,k_z,k_max)
                  fac = pi32*self%contraction_prefactor(i)*partition(aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*thermal(k,aa,bb)
                  end do
                  do b = 0,self%b%l  ! incorporate prefac into ft_z
                     do a = 0,self%a%l
                        ft_z(:,a,b) = ft_z(:,a,b)*prefac(:)
                     end do
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     do a = 1,self%a%n_comp
                        xa = ax(a)
                        ya = ay(a)
                        za = az(a)
                        res(:,a,b) = res(:,a,b)+ft_x(:,xa,xb)*ft_y(:,ya,yb)*ft_z(:,za,zb)
                     end do
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)
         end select
      end select
   end subroutine

   pure subroutine make_e_coeff(self,e,R,ex_a,ex_b,g1,g2)
   ! Return the McMurchie-Davidson "e" coefficients for atoms
   ! separated by distance "R". Assumes starting value e000 = ONE.
   ! For optimized code.
      type(SHELL2_TYPE), IN :: self
      MAT3(REAL,0:,0:,0:), OUT :: e
      REAL, IN :: R,ex_a,ex_b,g1,g2
      INT :: t,a,b,ap,am,bp,bm,abp,ab,abm,l_a,l_b
      REAL :: a1,b1
      e(0,0,0) = ONE
      if (self%l_sum==0) return
      l_a = self%a%l
      l_b = self%b%l
      a1 = ex_a*g1*R
      b1 = ex_b*g1*R
      if (l_a>=1) then
         e(1,1,0) = g2
         e(0,1,0) = -b1
      end if
      if (l_b>=1) then
         e(1,0,1) = g2
         e(0,0,1) = a1
      end if
      if (self%l_sum==1) return

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

   PURE subroutine make_ft_component(self,res,R,ex_a,ex_b,g1,g2,k_pts,k_max)
   ! Do the Fourier transform of component "comp" of the gaussian pair
   ! evaluated at the points "k_pts", without the prefactors.
   ! Used only in the default case
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,1:,0:,0:), OUT :: res
      REAL, IN :: R,ex_a,ex_b,g1,g2
      VEC(REAL,:), IN :: k_pts
      INT, IN :: k_max
      MAT3(REAL,:,:,:), allocatable :: e
      MAT(REAL,:,:), allocatable :: kp
      VEC(REAL,:), allocatable :: vr,vi
      INT :: a,b,t,t_max

      ! Make the E coefficients
      call create_(e,[0,self%l_sum],[0,self%a%l],[0,self%b%l])
      call make_e_coeff_(self,e,R,ex_a,ex_b,g1,g2)

      ! Define kp array containing powers k_pts(n)^t
      if (self%l_sum>0) then
         call create_(kp,[1,k_max],[1,self%l_sum])
         kp(:,1) = k_pts
         do t = 2,self%l_sum
            kp(:,t) = kp(:,t-1)*k_pts
         end do
         do t = 2,self%l_sum
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
            res(:,a,b) = cmplx(vr,vi,kind=CPX_KIND)
         end do
      end do
      call destroy_(vi); call destroy_(vr)
      call destroy_(kp); call destroy_(e)
   end subroutine

   PURE subroutine make_temperature_factors(self,TF,model,k_pts,ADP_a,ADP_b)
   ! Makes the temperature factors "TF" for a given "model" over all
   ! pairs of gaussians, using ADPs "ADP_a" and ADP_b" for the atoms
   ! on which the pair of gaussians is centred.
   ! See Jayatilaka & Grimwood (2001) Acta Cryst A 57 p76-86 sec 4.2.
      type(SHELL2_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: TF
      STR(len=*), IN :: model
      MAT(REAL,:,:), IN :: k_pts
      MAT(REAL,3,3), IN :: ADP_a
      MAT(REAL,3,3), IN :: ADP_b
      MAT(REAL,3,3) :: Uab
      REAL :: separation
      REAL :: bondlength
      REAL :: Ta,Tb,g
      VEC(REAL,3) :: S
      INT :: k
   ENSURE(size(TF,1)==size(k_pts,1),"SHELL2:make_temperature_factors ... wrong dim2, TF")
   ENSURE(size(TF,2)==self%a%n_cc,"SHELL2:make_temperature_factors ... wrong dim2, TF")
   ENSURE(size(TF,3)==self%b%n_cc,"SHELL2:make_temperature_factors ... wrong dim3, TF")
      select case (trim(model))

      case ("none      ")
         TF = ONE

      case ("tc-coppens")
         ! tf=HALF[exp(-(1/2)S(Ua)S)+exp(-(1/2)S(Ub)S)]
         do k = 1,size(k_pts,1)
            S = k_pts(k,:)
            Ta = -HALF*dot_product(S,matmul(ADP_a,S))
            Tb = -HALF*dot_product(S,matmul(ADP_b,S))
            TF(k,:,:) = HALF*(exp(Ta)+exp(Tb))
         end do

      case ("tc-stewart")
         separation = norm2(self%a%position-self%b%position)
         bondlength = 2.5
         g = HALF
         if (separation>bondlength) g = QUARTER
         ! tf=exp[-HALF Sg(Ua+Ub)S], g=1/2,1/4]
         Uab = -HALF*g*(ADP_a+ADP_b)
         do k = 1,size(k_pts,1)
            S = k_pts(k,:)
            Ta = dot_product(S,matmul(Uab,S))
            TF(k,:,:) = exp(Ta)
         end do

      case ("tc-tanaka ")
         ! tf=exp[-HALF S (Ua*alpha + Ub*beta)/gamma S]
         call make_Tanaka_TFs_(self,TF,k_pts,ADP_a,ADP_b)
      end select
   end subroutine


!   make_partition_factors(part,model,ra,rb,is_zero) :: PURE
!   ! Make the partitition factors "part" over the pairs of contracted
!   ! gaussians in shell "self" using repetition factors "ra" and "rb"
!   ! and for a particular thermal "model".  
!   ! See Jayatilaka & Grimwood (2001) Acta Cryst A 57 p76-86 sec 4.3.
!      self :: IN
!      model :: STR, IN
!      part :: MAT{REAL}, OUT
!      ra,rb :: INT, IN
!      is_zero :: BIN, optional, OUT
!
!   ENSURE(part.dim1==.a.n_cc,"wrong dim1, part")
!   ENSURE(part.dim2==.b.n_cc,"wrong dim2, part")
!
!      fa,fb :: REAL
!
!      if (present(is_zero)) then
!         if (ra<1 AND rb<1) then
!            is_zero = TRUE
!            part = ZERO
!            return
!         else
!            is_zero = FALSE
!         end
!      end
!
!      if (ra<1) then; fa = ZERO
!      else;           fa = ONE/ra
!      end
!
!      if (rb<1) then; fb = ZERO
!      else;           fb = ONE/rb
!      end
!
!      select case (trim(model))
!      case ("none       "); part = ONE
!      case ("tc-tanaka  "); .make_Tanaka_partition(part,fa,fb)
!      end
!
!   end

! NOTE: Same as make_ft_static but primitive a, b loops inverted
!       AND pair_positions transposed too ...

   PURE subroutine make_FT_no_TF(self,res,k_pts,Wc)
   ! Make "res", of shape [k_max,.a.n_comp,.b.n_comp], which is the
   ! Fourier transform at reciprocal "k_pts", of shape [:,3], for an
   ! atom whose weights "Wcx" are defined on the points "i" of the
   ! primitive gaussian product centers in ".pair_center(:,i)".
   ! The order of "i" is over primitives of shell a in the inner loop,
   ! and the ptimitives in shell b in the outer loop.
   ! NOTE:  make_ft_static has these loops inverted.
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: res
      MAT(REAL,:,:), IN :: k_pts
      VEC(REAL,:), IN :: Wc
      MAT3(CPX,:,:,:), allocatable :: W
      INT :: i
   ENSURE(size(res,1)==size(k_pts,1),"SHELL2:make_FT_no_TF ... incomptaible dim1, res and k_pts")
   ENSURE(size(res,2)==self%a%n_comp,"SHELL2:make_FT_no_TF ... incomptaible dim2, res and shell a")
   ENSURE(size(res,3)==self%b%n_comp,"SHELL2:make_FT_no_TF ... incomptaible dim2, res and shell a")
   ENSURE(size(k_pts,2)==3,"SHELL2:make_FT_no_TF ... wrong dim3, k_pts")
   ENSURE(size(Wc)==self%n_gaussian_pairs,"SHELL2:make_FT_no_TF ... wrong dim, Wc")
      if (NOT self%is_spherical) then

         ! Cartesian
         call make_FT_no_TF_c_(self,res,k_pts,Wc)
      else if (self%l_max<2) then

         ! Spherical s & p
         call make_FT_no_TF_c_(self,res,k_pts,Wc)
      else

         ! Spherical general
         call create_(W,size(res,1),self%a%n_comp,self%b%n_comp)
         call make_FT_no_TF_c_(self,W,k_pts,Wc)
         do i = 1,size(res,1)
            call change_to_spherical_(self,W(i,:,:),res(i,:,:))
         end do
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_FT_no_TF_c(self,res,k_pts,Wc)
   ! Make "res", of shape [k_max,.a.n_comp,.b.n_comp], which is the
   ! Fourier transform at reciprocal "k_pts", of shape [:,3], for an
   ! atom whose weights "Wc" are defined on the points "i" of the
   ! primitive gaussian product centers in ".pair_center(:,i)".
   ! The order of "i" is over primitives of shell a in the inner loop,
   ! and the ptimitives in shell b in the outer loop.
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: res
      MAT(REAL,:,:), IN :: k_pts
      VEC(REAL,:), IN :: Wc
      MAT3(REAL,:,:,:), allocatable :: e
      MAT3(CPX,:,:,:), allocatable :: ft_x,ft_y,ft_z
      VEC(REAL,:), allocatable :: k_x,k_y,k_z
      VEC(REAL,:), allocatable :: k_x2,k_y2,k_z2
      VEC(CPX,:), allocatable :: prefac
      VEC(CPX,:), allocatable :: ft_x01,ft_x10
      VEC(CPX,:), allocatable :: ft_y01,ft_y10
      VEC(CPX,:), allocatable :: ft_z01,ft_z10
      VEC(INT,:), allocatable :: ax,ay,az
      VEC(INT,:), allocatable :: bx,by,bz
      REAL :: Ex001,Ey001,Ez001
      REAL :: Ex101,Ey101,Ez101
      REAL :: Ex010,Ey010,Ez010
      REAL :: Ex110,Ey110,Ez110
      REAL :: Ex111,Ey111,Ez111
      REAL :: Ex211,Ey211,Ez211
      REAL :: Ex011,Ey011,Ez011
      REAL :: fac,pi32
      REAL :: g1,g2,g4
      REAL :: ex_a,ex_b
      REAL :: dot1,dot2
      REAL :: k1,k2,k3
      REAL :: P1,P2,P3
      REAL :: AB1,AB2,AB3
      REAL :: PA1,PA2,PA3
      REAL :: PB1,PB2,PB3
      INT :: xa,ya,za
      INT :: xb,yb,zb
      INT :: aa,a,fa,la
      INT :: bb,b,fb,lb
      INT :: k_max,k,i
      pi32 = sqrt(PI)*PI
      k_max = size(k_pts,1)
      k_x = k_pts(:,1)
      k_y = k_pts(:,2)
      k_z = k_pts(:,3)
      PA1 = self%a%position(1)
      PA2 = self%a%position(2)
      PA3 = self%a%position(3)
      PB1 = self%b%position(1)
      PB2 = self%b%position(2)
      PB3 = self%b%position(3)
      AB1 = PA1-PB1
      AB2 = PA2-PB2
      AB3 = PA3-PB3
      res = ZERO
      select case (self%a%l)

      case (0)
         select case (self%b%l)

         ! <S|S>
         case (0)  ! (l_a,l_b) = (0,0)
            i = 0
            do b = 1,self%b%n_cc
               do a = 1,self%a%n_cc
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g4 = -QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  fac = pi32*self%contraction_prefactor(i)*Wc(i)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = g4*(k1*k1+k2*k2+k3*k3)
                     res(k,1,1) = res(k,1,1)+fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))
                  end do
               end do
            end do

         ! <S|P>
         case (1)  ! (l_a,l_b) = (0,1)
            call create_(prefac,k_max)
            call create_(e,[0,1],[0,0],[0,1])
            i = 0
            do b = 1,self%b%n_cc
               ex_b = self%b%exponent(b)
               do a = 1,self%a%n_cc
                  i = i+1
                  ex_a = self%a%exponent(a)
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_e_coeff_(self,e,AB1,ex_a,ex_b,g1,g2)
                  Ex001 = e(0,0,1)
                  Ex101 = e(1,0,1)
                  call make_e_coeff_(self,e,AB2,ex_a,ex_b,g1,g2)
                  Ey001 = e(0,0,1)
                  Ey101 = e(1,0,1)
                  call make_e_coeff_(self,e,AB3,ex_a,ex_b,g1,g2)
                  Ez001 = e(0,0,1)
                  Ez101 = e(1,0,1)
                  fac = pi32*self%contraction_prefactor(i)*Wc(i)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))
                  end do
                  res(:,1,1) = res(:,1,1)+cmplx(Ex001,Ex101*k_x,kind=CPX_KIND)*prefac
                  res(:,1,2) = res(:,1,2)+cmplx(Ey001,Ey101*k_y,kind=CPX_KIND)*prefac
                  res(:,1,3) = res(:,1,3)+cmplx(Ez001,Ez101*k_z,kind=CPX_KIND)*prefac
               end do
            end do
            call destroy_(e)
            call destroy_(prefac)

         ! <S|X>
         case default  ! (l_a,l_b) = (0,X)
            fb = self%b%first_gaussian
            lb = self%b%last_gaussian
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_ft_component_(self,ft_x,AB1,ex_a,ex_b,g1,g2,k_x,k_max)
                  call make_ft_component_(self,ft_y,AB2,ex_a,ex_b,g1,g2,k_y,k_max)
                  call make_ft_component_(self,ft_z,AB3,ex_a,ex_b,g1,g2,k_z,k_max)
                  fac = pi32*self%contraction_prefactor(i)*Wc(i)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))
                  end do
                  do b = 0,self%b%l  ! incorporate prefac into ft_z
                     ft_z(:,0,b) = ft_z(:,0,b)*prefac(:)
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     res(:,1,b) = res(:,1,b)+ft_x(:,0,xb)*ft_y(:,0,yb)*ft_z(:,0,zb)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_y)
            call destroy_(ft_z)
         end select

      case (1)
         select case (self%b%l)

         ! <P|S>
         case (0)  ! (l_a,l_b) = (1,0)
            call create_(prefac,k_max)
            call create_(e,[0,1],[0,1],[0,0])
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_e_coeff_(self,e,AB1,ex_a,ex_b,g1,g2)
                  Ex010 = e(0,1,0)
                  Ex110 = e(1,1,0)
                  call make_e_coeff_(self,e,AB2,ex_a,ex_b,g1,g2)
                  Ey010 = e(0,1,0)
                  Ey110 = e(1,1,0)
                  call make_e_coeff_(self,e,AB3,ex_a,ex_b,g1,g2)
                  Ez010 = e(0,1,0)
                  Ez110 = e(1,1,0)
                  fac = pi32*self%contraction_prefactor(i)*Wc(i)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))
                  end do
                  res(:,1,1) = res(:,1,1)+cmplx(Ex010,Ex110*k_x,kind=CPX_KIND)*prefac
                  res(:,2,1) = res(:,2,1)+cmplx(Ey010,Ey110*k_y,kind=CPX_KIND)*prefac
                  res(:,3,1) = res(:,3,1)+cmplx(Ez010,Ez110*k_z,kind=CPX_KIND)*prefac
               end do
            end do
            call destroy_(e)
            call destroy_(prefac)

         ! <P|P>
         case (1)  ! (l_a,l_b) = (1,1)
            call create_(ft_x,[1,k_max],[0,1],[0,1])
            call create_(ft_y,[1,k_max],[0,1],[0,1])
            call create_(ft_z,[1,k_max],[0,1],[0,1])
            call create_(prefac,k_max)
            call create_(e,[0,2],[0,1],[0,1])
            call create_(ft_x01,k_max); call create_(ft_y01,k_max); call create_(ft_z01,k_max)
            call create_(ft_x10,k_max); call create_(ft_y10,k_max); call create_(ft_z10,k_max)
            k_x2 = k_x*k_x
            k_y2 = k_y*k_y
            k_z2 = k_z*k_z
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  fac = pi32*self%contraction_prefactor(i)*Wc(i)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))
                  end do
                  call make_e_coeff_(self,e,AB1,ex_a,ex_b,g1,g2)
                  Ex001 = e(0,0,1)
                  Ex010 = e(0,1,0)
                  Ex011 = e(0,1,1)
                  Ex101 = e(1,0,1)
                  Ex110 = e(1,1,0)
                  Ex111 = e(1,1,1)
                  Ex211 = e(2,1,1)
                  call make_e_coeff_(self,e,AB2,ex_a,ex_b,g1,g2)
                  Ey001 = e(0,0,1)
                  Ey010 = e(0,1,0)
                  Ey011 = e(0,1,1)
                  Ey101 = e(1,0,1)
                  Ey110 = e(1,1,0)
                  Ey111 = e(1,1,1)
                  Ey211 = e(2,1,1)
                  call make_e_coeff_(self,e,AB3,ex_a,ex_b,g1,g2)
                  Ez001 = e(0,0,1)
                  Ez010 = e(0,1,0)
                  Ez011 = e(0,1,1)
                  Ez101 = e(1,0,1)
                  Ez110 = e(1,1,0)
                  Ez111 = e(1,1,1)
                  Ez211 = e(2,1,1)
                  ft_x01 = cmplx(Ex001,Ex101*k_x,kind=CPX_KIND)
                  ft_x10 = cmplx(Ex010,Ex110*k_x,kind=CPX_KIND)
                  ft_y01 = cmplx(Ey001,Ey101*k_y,kind=CPX_KIND)
                  ft_y10 = cmplx(Ey010,Ey110*k_y,kind=CPX_KIND)
                  ft_z01 = cmplx(Ez001,Ez101*k_z,kind=CPX_KIND)
                  ft_z10 = cmplx(Ez010,Ez110*k_z,kind=CPX_KIND)
                  res(:,1,1) = res(:,1,1)+cmplx(Ex011-Ex211*k_x2,Ex111*k_x,kind=CPX_KIND)*prefac
                  res(:,2,2) = res(:,2,2)+cmplx(Ey011-Ey211*k_y2,Ey111*k_y,kind=CPX_KIND)*prefac
                  res(:,3,3) = res(:,3,3)+cmplx(Ez011-Ez211*k_z2,Ez111*k_z,kind=CPX_KIND)*prefac
                  res(:,2,1) = res(:,2,1)+ft_x01*ft_y10*prefac
                  res(:,1,2) = res(:,1,2)+ft_x10*ft_y01*prefac
                  res(:,3,1) = res(:,3,1)+ft_x01*ft_z10*prefac
                  res(:,1,3) = res(:,1,3)+ft_x10*ft_z01*prefac
                  res(:,3,2) = res(:,3,2)+ft_y01*ft_z10*prefac
                  res(:,2,3) = res(:,2,3)+ft_y10*ft_z01*prefac
               end do
            end do
            call destroy_(ft_z10); call destroy_(ft_z01); call destroy_(ft_y10)
            call destroy_(ft_y01); call destroy_(ft_x10); call destroy_(ft_x01)
            call destroy_(e); call destroy_(prefac)
            call destroy_(ft_x); call destroy_(ft_z); call destroy_(ft_y)

         ! <P|X>
         case default  ! (l_a,l_b) = (1,X)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fb = self%b%first_gaussian
            lb = self%b%last_gaussian
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_ft_component_(self,ft_x,AB1,ex_a,ex_b,g1,g2,k_x,k_max)
                  call make_ft_component_(self,ft_y,AB2,ex_a,ex_b,g1,g2,k_y,k_max)
                  call make_ft_component_(self,ft_z,AB3,ex_a,ex_b,g1,g2,k_z,k_max)
                  fac = pi32*self%contraction_prefactor(i)*Wc(i)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))
                  end do
                  do b = 0,self%b%l
                     ft_z(:,0,b) = ft_z(:,0,b)*prefac(:)
                     ft_z(:,1,b) = ft_z(:,1,b)*prefac(:)
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     res(:,1,b) = res(:,1,b)+ft_x(:,1,xb)*ft_y(:,0,yb)*ft_z(:,0,zb)
                     res(:,2,b) = res(:,2,b)+ft_x(:,0,xb)*ft_y(:,1,yb)*ft_z(:,0,zb)
                     res(:,3,b) = res(:,3,b)+ft_x(:,0,xb)*ft_y(:,0,yb)*ft_z(:,1,zb)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x); call destroy_(ft_z); call destroy_(ft_y)
         end select

      case default

         ! <X|S>
         select case (self%b%l)

         case (0)  ! (l_a,l_b) = (X,0)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fa = self%a%first_gaussian
            la = self%a%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_ft_component_(self,ft_x,AB1,ex_a,ex_b,g1,g2,k_x,k_max)
                  call make_ft_component_(self,ft_y,AB2,ex_a,ex_b,g1,g2,k_y,k_max)
                  call make_ft_component_(self,ft_z,AB3,ex_a,ex_b,g1,g2,k_z,k_max)
                  fac = pi32*self%contraction_prefactor(i)*Wc(i)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))
                  end do
                  do a = 0,self%a%l
                     ft_z(:,a,0) = ft_z(:,a,0)*prefac(:)
                  end do
                  do a = 1,self%a%n_comp
                     xa = ax(a)
                     ya = ay(a)
                     za = az(a)
                     res(:,a,1) = res(:,a,1)+ft_x(:,xa,0)*ft_y(:,ya,0)*ft_z(:,za,0)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x); call destroy_(ft_z); call destroy_(ft_y)

         ! <X|P>
         case (1)  ! (l_a,l_b) = (X,1)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fa = self%a%first_gaussian
            la = self%a%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_ft_component_(self,ft_x,AB1,ex_a,ex_b,g1,g2,k_x,k_max)
                  call make_ft_component_(self,ft_y,AB2,ex_a,ex_b,g1,g2,k_y,k_max)
                  call make_ft_component_(self,ft_z,AB3,ex_a,ex_b,g1,g2,k_z,k_max)
                  fac = pi32*self%contraction_prefactor(i)*Wc(i)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))
                  end do
                  do a = 0,self%a%l  ! incorporate prefac into ft_z
                     ft_z(:,a,0) = ft_z(:,a,0)*prefac(:)
                     ft_z(:,a,1) = ft_z(:,a,1)*prefac(:)
                  end do
                  do a = 1,self%a%n_comp
                     xa = ax(a)
                     ya = ay(a)
                     za = az(a)
                     res(:,a,1) = res(:,a,1)+ft_x(:,xa,1)*ft_y(:,ya,0)*ft_z(:,za,0)
                     res(:,a,2) = res(:,a,2)+ft_x(:,xa,0)*ft_y(:,ya,1)*ft_z(:,za,0)
                     res(:,a,3) = res(:,a,3)+ft_x(:,xa,0)*ft_y(:,ya,0)*ft_z(:,za,1)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x); call destroy_(ft_z); call destroy_(ft_y)

         ! <X|X>
         case default  ! (l_a,l_b) = (X,X)

            ! This is the general routine.
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            fa = self%a%first_gaussian; la = self%a%last_gaussian
            fb = self%b%first_gaussian; lb = self%b%last_gaussian
            ax = px(fa:la)
            ay = py(fa:la)
            az = pz(fa:la)
            bx = px(fb:lb)
            by = py(fb:lb)
            bz = pz(fb:lb)
            i = 0
            do bb = 1,self%b%n_cc
               ex_b = self%b%exponent(bb)
               do aa = 1,self%a%n_cc
                  ex_a = self%a%exponent(aa)
                  i = i+1
                  g1 = self%exponent_inv(i)
                  g2 = HALF*g1
                  g4 = QUARTER*g1
                  P1 = self%pair_center(1,i)
                  P2 = self%pair_center(2,i)
                  P3 = self%pair_center(3,i)
                  call make_ft_component_(self,ft_x,AB1,ex_a,ex_b,g1,g2,k_x,k_max)
                  call make_ft_component_(self,ft_y,AB2,ex_a,ex_b,g1,g2,k_y,k_max)
                  call make_ft_component_(self,ft_z,AB3,ex_a,ex_b,g1,g2,k_z,k_max)
                  fac = pi32*self%contraction_prefactor(i)*Wc(i)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))
                  end do
                  do b = 0,self%b%l  ! incorporate prefac into ft_z
                     do a = 0,self%a%l
                        ft_z(:,a,b) = ft_z(:,a,b)*prefac(:)
                     end do
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     do a = 1,self%a%n_comp
                        xa = ax(a)
                        ya = ay(a)
                        za = az(a)
                        res(:,a,b) = res(:,a,b)+ft_x(:,xa,xb)*ft_y(:,ya,yb)*ft_z(:,za,zb)
                     end do
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)
         end select
      end select
   end subroutine


! There is a lot of dead code here.
! Leave it for testing/optimization
! because these integrals are so expensive
! it is worth looking at the attempts.

   subroutine make_ft_old(self,res,k_pts,thermal,partition)
   ! Calculates the Fourier transform for a product of two contracted
   ! gaussian shells, evaluated at a series of k points k_pts
   ! Dimensions of res are [k_max,.a.n_comp,.b.n_comp].
   ! Also adds in thermal correction and partition factors.
   ! This version has the gaussian2 stuff inlined and special cases optimised,
   ! it's pretty messy but much faster.
      type(SHELL2_TYPE), INOUT :: self
      MAT3(CPX,:,:,:), target :: res
      MAT(REAL,:,:), target :: k_pts
      MAT3(REAL,:,:,:), target :: thermal
      MAT(REAL,:,:), IN :: partition
      type(GAUSSIAN2_TYPE) :: G
      MAT3(REAL,:,:,:), allocatable :: e
      VEC(REAL,3) :: pos
      MAT3(CPX,:,:,:), allocatable, target :: ft_x,ft_y,ft_z
      VEC(REAL,:), allocatable :: k_x2,k_y2,k_z2
      VEC(REAL,:), PTR :: therm,k_x,k_y,k_z
      VEC(CPX,:), PTR :: res_ij,ft_xb,ft_yb,ft_zb
      VEC(CPX,:), allocatable :: prefac
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      MAT(CPX,:,:), PTR :: ft_xbx,ft_yby,ft_zbz
      VEC(CPX,:), allocatable :: ft_x01,ft_x10,ft_y01,ft_y10,ft_z01,ft_z10
      REAL :: pifac,gamma,g1,g4,dot1,dot2,k1,k2,k3,P1,P2,P3,PI_on_gamma
      INT :: a,b,k,xa,ya,za,xb,yb,zb,bxlast,bylast,bzlast,aa,bb,k_max,fa,la,fb,lb
      CPX :: fac1
      REAL :: g1_exa,g1_exb,e00,ex_a,ex_b,ca,fac
      REAL :: Ex000,Ey000,Ez000,Ex001,Ey001,Ez001,Ex101,Ey101,Ez101
      REAL :: Ex010,Ey010,Ez010,Ex110,Ey110,Ez110,Ex111,Ey111,Ez111
      REAL :: Ex211,Ey211,Ez211,Ex011,Ey011,Ez011
      REAL :: PA1,PA2,PA3,PB1,PB2,PB3,r2
      k_max = size(k_pts,1)
      res = ZERO
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      select case (self%a%l)

      case (0)
         select case (self%b%l)

         case (0)
            k_x => k_pts(:,1)
            k_y => k_pts(:,2)
            k_z => k_pts(:,3)
            pos = self%a%position-self%b%position
            r2 = dot_product(pos,pos)
            res_ij => res(:,1,1)
            PA1 = self%a%position(1); PA2 = self%a%position(2); PA3 = self%a%position(3)
            PB1 = self%b%position(1); PB2 = self%b%position(2); PB3 = self%b%position(3)
            do a = 1,self%a%n_cc
               ca = self%a%contraction(a)
               ex_a = self%a%exponent(a)
               do b = 1,self%b%n_cc
                  ex_b = self%b%exponent(b)
                  g1 = ONE/(ex_a+ex_b)
                  g4 = -QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*ex_a
                  g1_exb = g1*ex_b
                  P1 = g1_exa*PA1+g1_exb*PB1
                  P2 = g1_exa*PA2+g1_exb*PB2
                  P3 = g1_exa*PA3+g1_exb*PB3
                  e00 = exp(-ex_a*ex_b*r2*g1)
                  fac1 = cmplx(pifac*e00,ZERO,kind=CPX_KIND)*ca*self%b%contraction(b)*partition(a,b)
                  therm => thermal(:,a,b)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = g4*(k1*k1+k2*k2+k3*k3)
                     res_ij(k) = res_ij(k)+therm(k)*(fac1*exp(cmplx(dot2,dot1,kind=CPX_KIND)))
                  end do
               end do
            end do

         case (1)
            call create_(prefac,k_max)
            k_x => k_pts(:,1)
            k_y => k_pts(:,2)
            k_z => k_pts(:,3)
            PA1 = self%a%position(1); PA2 = self%a%position(2); PA3 = self%a%position(3)
            PB1 = self%b%position(1); PB2 = self%b%position(2); PB3 = self%b%position(3)
            call create_(e,[0,1],[0,0],[0,1])
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*PA1+g1_exb*PB1
                  P2 = g1_exa*PA2+g1_exb*PB2
                  P3 = g1_exa*PA3+g1_exb*PB3
                  call make_e_coeff_(G,e,1)
                  Ex000 = e(0,0,0)
                  Ex001 = e(0,0,1)
                  Ex101 = e(1,0,1)
                  call make_e_coeff_(G,e,2)
                  Ey000 = e(0,0,0)
                  Ey001 = e(0,0,1)
                  Ey101 = e(1,0,1)
                  call make_e_coeff_(G,e,3)
                  Ez000 = e(0,0,0)
                  Ez001 = e(0,0,1)
                  Ez101 = e(1,0,1)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*therm(k)
                  end do
                  res_ij => res(:,1,1)
                  res_ij(:) = res_ij(:)+cmplx(Ex001,Ex101*k_x,kind=CPX_KIND)*Ey000*Ez000*prefac(:)
                  res_ij => res(:,1,2)
                  res_ij(:) = res_ij(:)+Ex000*cmplx(Ey001,Ey101*k_y,kind=CPX_KIND)*Ez000*prefac(:)
                  res_ij => res(:,1,3)
                  res_ij(:) = res_ij(:)+Ex000*Ey000*cmplx(Ez001,Ez101*k_z,kind=CPX_KIND)*prefac(:)
               end do
            end do
            call destroy_(e)
            call destroy_(prefac)

         case default
            fb = self%b%first_gaussian; lb = self%b%last_gaussian
            bx = px(fb:lb); by = py(fb:lb); bz = pz(fb:lb)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            PA1 = self%a%position(1); PA2 = self%a%position(2); PA3 = self%a%position(3)
            PB1 = self%b%position(1); PB2 = self%b%position(2); PB3 = self%b%position(3)
            k_x => k_pts(:,1)
            k_y => k_pts(:,2)
            k_z => k_pts(:,3)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*PA1+g1_exb*PB1
                  P2 = g1_exa*PA2+g1_exb*PB2
                  P3 = g1_exa*PA3+g1_exb*PB3
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*therm(k)
                  end do
                  do b = 0,self%b%l  ! incorporate prefac into ft_z
                     ft_z(:,0,b) = ft_z(:,0,b)*prefac(:)
                  end do
                  bxlast = bx(1)
                  bylast = by(1)
                  bzlast = bz(1)
                  ft_xb => ft_x(:,0,bxlast)
                  ft_yb => ft_y(:,0,bylast)
                  ft_zb => ft_z(:,0,bzlast)
                  do b = 1,self%b%n_comp
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
                     res_ij => res(:,1,b)
                     res_ij(:) = res_ij(:)+ft_xb(:)*ft_yb(:)*ft_zb(:)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_y)
            call destroy_(ft_z)
         end select

      case (1)
         select case (self%b%l)

         case (0)
            call create_(prefac,k_max)
            call create_(e,[0,1],[0,1],[0,0])
            PA1 = self%a%position(1); PA2 = self%a%position(2); PA3 = self%a%position(3)
            PB1 = self%b%position(1); PB2 = self%b%position(2); PB3 = self%b%position(3)
            k_x => k_pts(:,1)
            k_y => k_pts(:,2)
            k_z => k_pts(:,3)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*PA1+g1_exb*PB1
                  P2 = g1_exa*PA2+g1_exb*PB2
                  P3 = g1_exa*PA3+g1_exb*PB3
                  call make_e_coeff_(G,e,1)
                  Ex000 = e(0,0,0)
                  Ex010 = e(0,1,0)
                  Ex110 = e(1,1,0)
                  call make_e_coeff_(G,e,2)
                  Ey000 = e(0,0,0)
                  Ey010 = e(0,1,0)
                  Ey110 = e(1,1,0)
                  call make_e_coeff_(G,e,3)
                  Ez000 = e(0,0,0)
                  Ez010 = e(0,1,0)
                  Ez110 = e(1,1,0)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*therm(k)
                  end do
                  res_ij => res(:,1,1)
                  res_ij(:) = res_ij(:)+cmplx(Ex010,Ex110*k_x(:),kind=CPX_KIND)*Ey000*Ez000*prefac(:)
                  res_ij => res(:,2,1)
                  res_ij(:) = res_ij(:)+Ex000*cmplx(Ey010,Ey110*k_y(:),kind=CPX_KIND)*Ez000*prefac(:)
                  res_ij => res(:,3,1)
                  res_ij(:) = res_ij(:)+Ex000*Ey000*cmplx(Ez010,Ez110*k_z(:),kind=CPX_KIND)*prefac(:)
               end do
            end do
            call destroy_(e)
            call destroy_(prefac)

         case (1)
            call create_(ft_x,[1,k_max],[0,1],[0,1])
            call create_(ft_y,[1,k_max],[0,1],[0,1])
            call create_(ft_z,[1,k_max],[0,1],[0,1])
            call create_(prefac,k_max)
            k_x => k_pts(:,1)
            k_y => k_pts(:,2)
            k_z => k_pts(:,3)
            PA1 = self%a%position(1); PA2 = self%a%position(2); PA3 = self%a%position(3)
            PB1 = self%b%position(1); PB2 = self%b%position(2); PB3 = self%b%position(3)
            call create_(e,[0,2],[0,1],[0,1])
            call create_(ft_x01,k_max)
            call create_(ft_x10,k_max)
            call create_(ft_y01,k_max)
            call create_(ft_y10,k_max)
            call create_(ft_z01,k_max)
            call create_(ft_z10,k_max)
            call create_(k_x2,k_max)
            call create_(k_y2,k_max)
            call create_(k_z2,k_max)
            k_x2 = k_x*k_x
            k_y2 = k_y*k_y
            k_z2 = k_z*k_z
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*PA1+g1_exb*PB1
                  P2 = g1_exa*PA2+g1_exb*PB2
                  P3 = g1_exa*PA3+g1_exb*PB3
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*therm(k)
                  end do
                  call make_e_coeff_(G,e,1)
                  Ex000 = e(0,0,0)
                  Ex001 = e(0,0,1)
                  Ex010 = e(0,1,0)
                  Ex011 = e(0,1,1)
                  Ex101 = e(1,0,1)
                  Ex110 = e(1,1,0)
                  Ex111 = e(1,1,1)
                  Ex211 = e(2,1,1)
                  call make_e_coeff_(G,e,2)
                  Ey000 = e(0,0,0)
                  Ey001 = e(0,0,1)
                  Ey010 = e(0,1,0)
                  Ey011 = e(0,1,1)
                  Ey101 = e(1,0,1)
                  Ey110 = e(1,1,0)
                  Ey111 = e(1,1,1)
                  Ey211 = e(2,1,1)
                  call make_e_coeff_(G,e,3)
                  Ez000 = e(0,0,0)
                  Ez001 = e(0,0,1)
                  Ez010 = e(0,1,0)
                  Ez011 = e(0,1,1)
                  Ez101 = e(1,0,1)
                  Ez110 = e(1,1,0)
                  Ez111 = e(1,1,1)
                  Ez211 = e(2,1,1)
                  ft_x01(:) = cmplx(Ex001,Ex101*k_x(:),kind=CPX_KIND)
                  ft_x10(:) = cmplx(Ex010,Ex110*k_x(:),kind=CPX_KIND)
                  ft_y01(:) = cmplx(Ey001,Ey101*k_y(:),kind=CPX_KIND)
                  ft_y10(:) = cmplx(Ey010,Ey110*k_y(:),kind=CPX_KIND)
                  ft_z01(:) = cmplx(Ez001,Ez101*k_z(:),kind=CPX_KIND)
                  ft_z10(:) = cmplx(Ez010,Ez110*k_z(:),kind=CPX_KIND)
                  res_ij => res(:,1,1)
                  res_ij(:) = res_ij(:)+cmplx(Ex011-Ex211*k_x2(:),Ex111*k_x(:),kind=CPX_KIND)*Ey000*Ez000*prefac(:)
                  res_ij => res(:,2,1)
                  res_ij(:) = res_ij(:)+ft_x01(:)*ft_y10(:)*Ez000*prefac(:)
                  res_ij => res(:,3,1)
                  res_ij(:) = res_ij(:)+ft_x01(:)*Ey000*ft_z10(:)*prefac(:)
                  res_ij => res(:,1,2)
                  res_ij(:) = res_ij(:)+ft_x10(:)*ft_y01(:)*Ez000*prefac(:)
                  res_ij => res(:,2,2)
                  res_ij(:) = res_ij(:)+Ex000*cmplx(Ey011-Ey211*k_y2(:),Ey111*k_y(:),kind=CPX_KIND)*Ez000*prefac(:)
                  res_ij => res(:,3,2)
                  res_ij(:) = res_ij(:)+Ex000*ft_y01(:)*ft_z10(:)*prefac(:)
                  res_ij => res(:,1,3)
                  res_ij(:) = res_ij(:)+ft_x10(:)*Ey000*ft_z01(:)*prefac(:)
                  res_ij => res(:,2,3)
                  res_ij(:) = res_ij(:)+Ex000*ft_y10(:)*ft_z01(:)*prefac(:)
                  res_ij => res(:,3,3)
                  res_ij(:) = res_ij(:)+Ex000*Ey000*cmplx(Ez011-Ez211*k_z2(:),Ez111*k_z(:),kind=CPX_KIND)*prefac(:)
               end do
            end do
            call destroy_(k_z2)
            call destroy_(k_y2)
            call destroy_(k_x2)
            call destroy_(ft_z10)
            call destroy_(ft_z01)
            call destroy_(ft_y10)
            call destroy_(ft_y01)
            call destroy_(ft_x10)
            call destroy_(ft_x01)
            call destroy_(e)
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)

         case default
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            k_x => k_pts(:,1)
            k_y => k_pts(:,2)
            k_z => k_pts(:,3)
            fb = self%b%first_gaussian; lb = self%b%last_gaussian
            bx = px(fb:lb); by = py(fb:lb); bz = pz(fb:lb)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*therm(k)
                  end do
                  do b = 0,self%b%l
                     ft_z(:,0,b) = ft_z(:,0,b)*prefac(:)
                     ft_z(:,1,b) = ft_z(:,1,b)*prefac(:)
                  end do
                  do b = 1,self%b%n_comp
                     xb = bx(b)
                     yb = by(b)
                     zb = bz(b)
                     ft_xb => ft_x(:,0,xb)
                     ft_yb => ft_y(:,0,yb)
                     ft_zb => ft_z(:,0,zb)
                     res_ij => res(:,1,b)
                     res_ij(:) = res_ij(:)+ft_x(:,1,xb)*ft_yb*ft_zb
                     res_ij => res(:,2,b)
                     res_ij(:) = res_ij(:)+ft_xb*ft_y(:,1,yb)*ft_zb
                     res_ij => res(:,3,b)
                     res_ij(:) = res_ij(:)+ft_xb*ft_yb*ft_z(:,1,zb)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)
         end select

      case default
         select case (self%b%l)

         case (0)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            k_x => k_pts(:,1)
            k_y => k_pts(:,2)
            k_z => k_pts(:,3)
            fa = self%a%first_gaussian; la = self%a%last_gaussian
            ax = px(fa:la); ay = py(fa:la); az = pz(fa:la)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*therm(k)
                  end do
                  do a = 0,self%a%l
                     ft_z(:,a,0) = ft_z(:,a,0)*prefac(:)
                  end do
                  do a = 1,self%a%n_comp
                     xa = ax(a)
                     ya = ay(a)
                     za = az(a)
                     res_ij => res(:,a,1)
                     res_ij(:) = res_ij(:)+ft_x(:,xa,0)*ft_y(:,ya,0)*ft_z(:,za,0)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)

         case (1)
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            k_x => k_pts(:,1)
            k_y => k_pts(:,2)
            k_z => k_pts(:,3)
            fa = self%a%first_gaussian; la = self%a%last_gaussian
            ax = px(fa:la); ay = py(fa:la); az = pz(fa:la)
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*therm(k)
                  end do
                  do a = 0,self%a%l  ! incorporate prefac into ft_z
                     ft_z(:,a,0) = ft_z(:,a,0)*prefac(:)
                     ft_z(:,a,1) = ft_z(:,a,1)*prefac(:)
                  end do
                  do a = 1,self%a%n_comp
                     xa = ax(a)
                     ya = ay(a)
                     za = az(a)
                     res_ij => res(:,a,1)
                     res_ij(:) = res_ij(:)+ft_x(:,xa,1)*ft_y(:,ya,0)*ft_z(:,za,0)
                     res_ij => res(:,a,2)
                     res_ij(:) = res_ij(:)+ft_x(:,xa,0)*ft_y(:,ya,1)*ft_z(:,za,0)
                     res_ij => res(:,a,3)
                     res_ij(:) = res_ij(:)+ft_x(:,xa,0)*ft_y(:,ya,0)*ft_z(:,za,1)
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)

         case default
            call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])
            call create_(prefac,k_max)
            k_x => k_pts(:,1)
            k_y => k_pts(:,2)
            k_z => k_pts(:,3)
            fa = self%a%first_gaussian; la = self%a%last_gaussian
            fb = self%b%first_gaussian; lb = self%b%last_gaussian
            ax = px(fa:la); ay = py(fa:la); az = pz(fa:la)
            bx = px(fb:lb); by = py(fb:lb); bz = pz(fb:lb)

         ! This is the general routine.  It includes reduced multiplication,
         ! and use of pointers to minimise array finding.
            do aa = 1,self%a%n_cc
               ca = self%a%contraction(aa)
               ex_a = self%a%exponent(aa)
               call set_(G,ex_a=ex_a)
               do bb = 1,self%b%n_cc
                  ex_b = self%b%exponent(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%exponent+G%b%exponent
                  g1 = ONE/gamma
                  g4 = QUARTER*g1
                  PI_on_gamma = PI*g1
                  pifac = sqrt(PI_on_gamma)*PI_on_gamma
                  g1_exa = g1*G%a%exponent
                  g1_exb = g1*G%b%exponent
                  P1 = g1_exa*self%a%position(1)+g1_exb*self%b%position(1)
                  P2 = g1_exa*self%a%position(2)+g1_exb*self%b%position(2)
                  P3 = g1_exa*self%a%position(3)+g1_exb*self%b%position(3)
                  call make_ft_component_(G,ft_x,1,k_x)
                  call make_ft_component_(G,ft_y,2,k_y)
                  call make_ft_component_(G,ft_z,3,k_z)
                  fac = pifac*ca*self%b%contraction(bb)*partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                     k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                     dot1 = k1*P1+k2*P2+k3*P3
                     dot2 = -g4*(k1*k1+k2*k2+k3*k3)
                     prefac(k) = fac*exp(cmplx(dot2,dot1,kind=CPX_KIND))*therm(k)
                  end do
                  do b = 0,self%b%l  ! incorporate prefac into ft_z
                     do a = 0,self%a%l
                        ft_z(:,a,b) = ft_z(:,a,b)*prefac(:)
                     end do
                  end do
                  bxlast = bx(1)
                  bylast = by(1)
                  bzlast = bz(1)
                  ft_xbx => ft_x(:,:,bxlast)
                  ft_yby => ft_y(:,:,bylast)
                  ft_zbz => ft_z(:,:,bzlast)
                  do b = 1,self%b%n_comp
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
                        res_ij => res(:,a,b)
                        res_ij(:) = res_ij(:)+ft_xbx(:,xa+1)*ft_yby(:,ya+1)*ft_zbz(:,za+1)
                     end do
                  end do
               end do
            end do
            call destroy_(prefac)
            call destroy_(ft_x)
            call destroy_(ft_z)
            call destroy_(ft_y)
         end select
      end select
   end subroutine

   subroutine make_ft_v0(self,res,k_pts,thermal,partition)
   ! Calculates the Fourier transform for a product of two contracted
   ! gaussian shells, evaluated at a series of k points k_pts
   ! Dimensions of res are [k_max,.a.n_comp,.b.n_comp].
   ! Also adds in thermal correction and partition factors.
   ! This is a basic version using gaussian pairs.
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:) :: res
      MAT(REAL,:,:), IN :: k_pts
      MAT3(REAL,:,:,:), target, IN :: thermal
      MAT(REAL,:,:), IN :: partition
      type(GAUSSIAN2_TYPE) :: G
      MAT3(CPX,:,:,:), allocatable :: ft_ab
      VEC(REAL,:), PTR :: therm
      INT :: a,b,i,j,k_max
      REAL :: ca,fac
      k_max = size(k_pts,1)
      call create_(ft_ab,k_max,self%a%n_comp,self%b%n_comp)
      res = ZERO
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do a = 1,self%a%n_cc
         call set_(G,ex_a=self%a%exponent(a))
         ca = self%a%contraction(a)
         do b = 1,self%b%n_cc
            call set_(G,ex_b=self%b%exponent(b))
            call make_ft_(G,ft_ab,k_pts)
            if (self%l_sum==0) then; call make_ft_00_(G,ft_ab(:,1,1),k_pts)
            else; call make_ft_XX_(G,ft_ab,k_pts)  ! slower older version
            end if
            fac = ca*self%b%contraction(b)*partition(a,b)
            therm => thermal(:,a,b)
            do j = 1,self%b%n_comp
               do i = 1,self%a%n_comp
                  res(:,i,j) = res(:,i,j)+fac*therm*ft_ab(:,i,j)
               end do
            end do
         end do
      end do
      call destroy_(ft_ab)
   end subroutine

   subroutine make_ft_v1(self,res,k_pts,thermal,partition)
   ! Calculates the Fourier transform for a product of two contracted
   ! gaussian shells, evaluated at a series of k points k_pts
   ! Dimensions of res are [k_max,.a.n_comp,.b.n_comp].
   ! Also adds in thermal correction and partition factors.
   ! This is a basic version using gaussian pairs.
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:) :: res
      MAT(REAL,:,:), IN :: k_pts
      MAT3(REAL,:,:,:), target, IN :: thermal
      MAT(REAL,:,:), IN :: partition
      type(GAUSSIAN2_TYPE) :: G
      MAT3(CPX,:,:,:), allocatable :: ft_ab
      VEC(REAL,:), PTR :: therm
      INT :: a,b,i,j,k_max
      REAL :: ca,fac
      k_max = size(k_pts,1)
      call create_(ft_ab,k_max,self%a%n_comp,self%b%n_comp)
      res = ZERO
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do a = 1,self%a%n_cc
         call set_(G,ex_a=self%a%exponent(a))
         ca = self%a%contraction(a)
         do b = 1,self%b%n_cc
            call set_(G,ex_b=self%b%exponent(b))
            if (self%l_sum==0) then; call make_ft_00_(G,ft_ab(:,1,1),k_pts)
            else; call make_ft_v2_(G,ft_ab,k_pts)  ! cleaner newer version
            end if
            fac = ca*self%b%contraction(b)*partition(a,b)
            therm => thermal(:,a,b)
            do j = 1,self%b%n_comp
               do i = 1,self%a%n_comp
                  res(:,i,j) = res(:,i,j)+fac*therm*ft_ab(:,i,j)
               end do
            end do
         end do
      end do
      call destroy_(ft_ab)
   end subroutine

   subroutine make_ft_v2(self,res,k_pts,thermal,partition)
   ! Calculates the Fourier transform for a product of two contracted
   ! gaussian shells, evaluated at a series of k points k_pts
   ! Dimensions of res are [k_max,.a.n_comp,.b.n_comp].
   ! Also adds in thermal correction and partition factors.
   ! This is an inlined version of v1 above.
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:) :: res
      MAT(REAL,:,:), target :: k_pts
      MAT3(REAL,:,:,:), target :: thermal
      MAT(REAL,:,:), IN :: partition
      type(GAUSSIAN2_TYPE) :: G
      VEC(CPX,:), allocatable :: prefac
      MAT3(CPX,:,:,:), allocatable :: ft_x,ft_y,ft_z
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz
      VEC(REAL,:), PTR :: therm,k_x,k_y,k_z
      REAL :: pi32,g1,g4,th,kk,k1,k2,k3,P1,P2,P3,fac,ca,cb
      INT :: ag,bg,i,a,b,k,k_max,fa,la,fb,lb
      CPX :: II

      ! Constants
      II = cmplx(ZERO,ONE,kind=CPX_KIND)
      pi32 = sqrt(PI)*PI

      ! Reciprocal lattice K points
      k_max = size(k_pts,1)
      k_x => k_pts(:,1)
      k_y => k_pts(:,2)
      k_z => k_pts(:,3)
      select case (self%l_sum)

      case (0)

         ! Loop on primitive pairs and make the FT integrals
         i = 0
         res = ZERO
         do bg = 1,self%b%n_cc
            cb = self%b%contraction(bg)
            do ag = 1,self%a%n_cc
               ca = self%a%contraction(ag)
               i = i+1

               ! Constants
               g1 = self%exponent_inv(i)
               g4 = QUARTER*g1
               fac = pi32*self%contraction_prefactor(i)*partition(ag,bg)
               therm => thermal(:,ag,bg)
               P1 = self%pair_center(1,i)
               P2 = self%pair_center(2,i)
               P3 = self%pair_center(3,i)

               ! Prefactor is essentially the integral
               do k = 1,k_max
                  k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                  th = k1*P1+k2*P2+k3*P3
                  kk = k1*k1+k2*k2+k3*k3
                  res(k,1,1) = res(k,1,1)+fac*(cos(th)+II*sin(th))*exp(-kk*g4)*therm(k)
               end do
            end do
         end do

      case default

         ! Space for fourier transform of each component
         call create_(ft_x,[1,k_max],[0,self%a%l],[0,self%b%l])
         call create_(ft_y,[1,k_max],[0,self%a%l],[0,self%b%l])
         call create_(ft_z,[1,k_max],[0,self%a%l],[0,self%b%l])

         ! Prefactor part
         call create_(prefac,k_max)

         ! Gaussian function power arrays
         fa = self%a%first_gaussian; la = self%a%last_gaussian
         fb = self%b%first_gaussian; lb = self%b%last_gaussian
         ax = px(fa:la); ay = py(fa:la); az = pz(fa:la)
         bx = px(fb:lb); by = py(fb:lb); bz = pz(fb:lb)

         ! Loop on primitive pairs and make the FT integrals
         i = 0
         res = ZERO
         call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
         do bg = 1,self%b%n_cc
            call set_(G,ex_b=self%b%exponent(bg))
            cb = self%b%contraction(bg)
            do ag = 1,self%a%n_cc
               call set_(G,ex_a=self%a%exponent(ag))
               ca = self%a%contraction(ag)
               i = i+1

               ! Constants
               g1 = self%exponent_inv(i)
               g4 = QUARTER*g1
               fac = pi32*sqrt(g1)*g1*ca*cb*partition(ag,bg)
               therm => thermal(:,ag,bg)
               P1 = self%pair_center(1,i)
               P2 = self%pair_center(2,i)
               P3 = self%pair_center(3,i)

               ! Make prefactor
               do k = 1,k_max
                  k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                  th = k1*P1+k2*P2+k3*P3
                  kk = k1*k1+k2*k2+k3*k3
                  prefac(k) = fac*(cos(th)+II*sin(th))*exp(-kk*g4)*therm(k)
               end do

               ! Fourier transforms of each component.
               call make_ft_component_(G,ft_x,1,k_x)
               call make_ft_component_(G,ft_y,2,k_y)
               call make_ft_component_(G,ft_z,3,k_z)

               ! Incorporate prefactor into ft_z
               do b = 0,self%b%l
                  do a = 0,self%a%l
                     ft_z(:,a,b) = prefac*ft_z(:,a,b)
                  end do
               end do

               ! Make the FT integrals
               res = res+ft_x(:,ax,bx)*ft_y(:,ay,by)*ft_z(:,az,bz)
            end do
         end do
         call destroy_(prefac)
         call destroy_(ft_z); call destroy_(ft_y); call destroy_(ft_x)
      end select
   end subroutine

   subroutine make_ft_v3(self,res,k_pts,thermal,partition)
   ! Calculates the Fourier transform for a product of two contracted
   ! gaussian shells, evaluated at a series of k points k_pts
   ! Dimensions of res are [k_max,.a.n_comp,.b.n_comp].
   ! Also adds in thermal correction and partition factors.
   ! This version uses the transfer relation.
      type(SHELL2_TYPE), INOUT :: self
      MAT3(CPX,:,:,:), target :: res
      MAT(REAL,:,:), target :: k_pts
      MAT3(REAL,:,:,:), target :: thermal
      MAT(REAL,:,:), IN :: partition
      VEC(CPX,:), allocatable :: prefac
      VEC(CPX,:), PTR :: res11
      VEC(REAL,:), PTR :: therm,k_x,k_y,k_z
      MAT(CPX,:,:), allocatable :: ft,ft_x,ft_y,ft_z
      REAL :: pi32,g1,g2,g4,th,kk,k1,k2,k3,P1,P2,P3,AB1,AB2,AB3,fac,ea,eb
      INT :: ag,bg,i,k,k_max
      CPX :: II

      ! Constants
      II = cmplx(ZERO,ONE,kind=CPX_KIND)
      pi32 = sqrt(PI)*PI
      AB1 = self%a%position(1)-self%b%position(1)
      AB2 = self%a%position(2)-self%b%position(2)
      AB3 = self%a%position(3)-self%b%position(3)

      ! Reciprocal lattice K points
      k_max = size(k_pts,1)
      k_x => k_pts(:,1)
      k_y => k_pts(:,2)
      k_z => k_pts(:,3)
      select case (self%l_sum)

      case (0)

         ! Loop on primitive pairs and make the FT integrals
         i = 0
         res11 => res(:,1,1)
         res11 = ZERO
         do bg = 1,self%b%n_cc
            eb = self%b%exponent(bg)
            do ag = 1,self%a%n_cc
               ea = self%a%exponent(ag)
               i = i+1

               ! Constants
               g1 = self%exponent_inv(i)
               g4 = QUARTER*g1
               fac = pi32*self%contraction_prefactor(i)*partition(ag,bg)
               therm => thermal(:,ag,bg)
               P1 = self%pair_center(1,i)
               P2 = self%pair_center(2,i)
               P3 = self%pair_center(3,i)

               ! Prefactor is essentially the integral
               do k = 1,k_max
                  k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                  th = k1*P1+k2*P2+k3*P3
                  kk = k1*k1+k2*k2+k3*k3
               ! res11(k) = res11(k) + (cos(th)+II*sin(th))*(fac*exp(-kk*g4)*therm(k))
                  res11(k) = res11(k)+fac*exp(-kk*g4+II*th)*therm(k)
               end do
            end do
         end do

      case default

         ! Space for fourier transform of each component
         call create_(ft,k_max,self%n_gaussian)
         call create_(ft_x,[1,k_max],[0,self%l_sum])
         call create_(ft_y,[1,k_max],[0,self%l_sum])
         call create_(ft_z,[1,k_max],[0,self%l_sum])

         ! Prefactor part
         call create_(prefac,k_max)

         ! Loop on primitive pairs and make the FT integrals
         i = 0
         ft = ZERO
         do bg = 1,self%b%n_cc
            eb = self%b%exponent(bg)
            do ag = 1,self%a%n_cc
               ea = self%a%exponent(ag)
               i = i+1

               ! Constants
               g1 = self%exponent_inv(i)
               g2 = HALF*g1
               g4 = QUARTER*g1
               fac = pi32*self%contraction_prefactor(i)*partition(ag,bg)
               therm => thermal(:,ag,bg)
               P1 = self%pair_center(1,i)
               P2 = self%pair_center(2,i)
               P3 = self%pair_center(3,i)

               ! Make prefactor
               do k = 1,k_max
                  k1 = k_x(k); k2 = k_y(k); k3 = k_z(k)
                  th = k1*P1+k2*P2+k3*P3
                  kk = k1*k1+k2*k2+k3*k3
               ! prefac(k) = (cos(th)+II*sin(th))*(fac*exp(-kk*g4)*therm(k))
                  prefac(k) = fac*exp(-kk*g4+II*th)*therm(k)
               end do

               ! Fourier transforms of each component.
               call make_ft_es_(self,ft_x,AB1,ea,eb,g1,g2,k_x,k_max)
               call make_ft_es_(self,ft_y,AB2,ea,eb,g1,g2,k_y,k_max)
               call make_ft_es_(self,ft_z,AB3,ea,eb,g1,g2,k_z,k_max,prefac)

               ! Make the FT integrals with reduced multiplication
            ! .make_ft_rms_v1(ft,ft_x,ft_y,ft_z,prefac)
               call make_ft_rms_v2_(self,ft,ft_x,ft_y,ft_z,prefac)
            end do
         end do
         call destroy_(prefac)
         call destroy_(ft_z); call destroy_(ft_y); call destroy_(ft_x)
         call transfer_ft_(self,ft,res)
         call destroy_(ft)
      end select
   end subroutine

   PURE subroutine make_ft_es_0(self,es,R,ex_a,ex_b,g1,g2,k1,k_max)
   ! Do the (e|s) type fourier transform integrals "es" for axis
   ! component with atom separation "R", with exponents "ex_a" and
   ! "ex_b", evaluated at the points "k1", *without* prefactors.
   ! WARNING: es(:,0) is not set, it assumed to be eliminated by RMS.
   !          See next routine where it is set using prefac.
      type(SHELL2_TYPE), IN :: self
      MAT(CPX,1:,0:), OUT :: es
      REAL, IN :: R,ex_a,ex_b,g1,g2
      VEC(REAL,:), target, IN :: k1
      INT, IN :: k_max
      MAT(REAL,:,:), allocatable :: e,kp
      VEC(REAL,:), allocatable :: vr,vi,k2,k3,k4
      INT :: a,t
      select case (self%l_sum)

      case (0)

      ! es = ONE

      case (1)

         ! Make the es type E coefficients
         call create_(e,[0,self%l_sum],[0,self%l_sum])
         call make_e_coeff_es_(self,e,R,ex_a,ex_b,g1,g2)

      ! es(:,0) = ONE
         es(:,1) = cmplx(e(0,1),e(1,1)*k1,kind=CPX_KIND)
         call destroy_(e)

      case (2)

         ! Make the es type E coefficients
         call create_(e,[0,self%l_sum],[0,self%l_sum])
         call make_e_coeff_es_(self,e,R,ex_a,ex_b,g1,g2)
         call create_(k2,k_max); k2 = k1*k1

      ! es(:,0) = ONE
         es(:,1) = cmplx(e(0,1),e(1,1)*k1,kind=CPX_KIND)
         es(:,2) = cmplx(e(0,2)-e(2,2)*k2,e(1,2)*k1,kind=CPX_KIND)
         call destroy_(k2)
         call destroy_(e)

      case (3)

         ! Make the es type E coefficients
         call create_(e,[0,self%l_sum],[0,self%l_sum])
         call make_e_coeff_es_(self,e,R,ex_a,ex_b,g1,g2)
         call create_(k2,k_max); k2 = k1*k1
         call create_(k3,k_max); k3 = k2*k1

      ! es(:,0) = ONE
         es(:,1) = cmplx(e(0,1),e(1,1)*k1,kind=CPX_KIND)
         es(:,2) = cmplx(e(0,2)-e(2,2)*k2,e(1,2)*k1,kind=CPX_KIND)
         es(:,3) = cmplx(e(0,3)-e(2,3)*k2,e(1,3)*k1-e(3,3)*k3,kind=CPX_KIND)
         call destroy_(k3); call destroy_(k2)
         call destroy_(e)

      case (4)

         ! Make the es type E coefficients
         call create_(e,[0,self%l_sum],[0,self%l_sum])
         call make_e_coeff_es_(self,e,R,ex_a,ex_b,g1,g2)
         call create_(k2,k_max); k2 = k1*k1
         call create_(k3,k_max); k3 = k2*k1
         call create_(k4,k_max); k4 = k3*k1

      ! es(:,0) = ONE
         es(:,1) = cmplx(e(0,1),e(1,1)*k1,kind=CPX_KIND)
         es(:,2) = cmplx(e(0,2)-e(2,2)*k2,e(1,2)*k1,kind=CPX_KIND)
         es(:,3) = cmplx(e(0,3)-e(2,3)*k2,e(1,3)*k1-e(3,3)*k3,kind=CPX_KIND)
         es(:,4) = cmplx(e(0,4)-e(2,4)*k2+e(4,4)*k4,e(1,4)*k1-e(3,4)*k3,kind=CPX_KIND)
         call destroy_(k4); call destroy_(k3); call destroy_(k2)
         call destroy_(e)

      case default

         ! Make the es type E coefficients
         call create_(e,[0,self%l_sum],[0,self%l_sum])
         call make_e_coeff_es_(self,e,R,ex_a,ex_b,g1,g2)

         ! Define kp array containing powers k(n)^t
         call create_(kp,[1,k_max],[2,self%l_sum])
         kp(:,2) = k1*k1
         do t = 3,self%l_sum
            kp(:,t) = kp(:,t-1)*k1
         end do
         do t = 2,self%l_sum
            if (mod(t,4)<=1) cycle
            kp(:,t) = -kp(:,t)
         end do

         ! Real, imaginary parts of answer: (vr,vi)
         call create_(vr,k_max)
         call create_(vi,k_max)

      ! es(:,0) = ONE
         es(:,1) = cmplx(e(0,1),e(1,1)*k1,kind=CPX_KIND)
         do a = 2,self%l_sum
            vr = e(0,a)
            do t = 2,a,2
               vr = vr+e(t,a)*kp(:,t)
            end do
            vi = e(1,a)*k1
            do t = 3,a,2
               vi = vi+e(t,a)*kp(:,t)
            end do
            es(:,a) = cmplx(vr,vi,kind=CPX_KIND)
         end do
         call destroy_(vi); call destroy_(vr)
         call destroy_(kp); call destroy_(e)
      end select
   end subroutine

   PURE subroutine make_ft_es_1(self,es,R,ex_a,ex_b,g1,g2,k1,k_max,prefac)
   ! Do the (e|s) type fourier transform integrals "es" for axis
   ! component with atom separation "R", with exponents "ex_a" and
   ! "ex_b", evaluated at the points "k1", *without* prefactors.
   ! "prefac" factors to apply to columns of "es".
      type(SHELL2_TYPE), IN :: self
      MAT(CPX,1:,0:), OUT :: es
      REAL, IN :: R,ex_a,ex_b,g1,g2
      VEC(REAL,:), target, IN :: k1
      INT, IN :: k_max
      VEC(CPX,:), IN :: prefac
      MAT(REAL,:,:), allocatable :: e,kp
      VEC(REAL,:), allocatable :: vr,vi,k2,k3,k4
      INT :: a,t
      select case (self%l_sum)

      case (0)
         es(:,0) = prefac

      case (1)

         ! Make the es type E coefficients
         call create_(e,[0,self%l_sum],[0,self%l_sum])
         call make_e_coeff_es_(self,e,R,ex_a,ex_b,g1,g2)
         es(:,0) = prefac
         es(:,1) = cmplx(e(0,1),e(1,1)*k1,kind=CPX_KIND)*prefac
         call destroy_(e)

      case (2)

         ! Make the es type E coefficients
         call create_(e,[0,self%l_sum],[0,self%l_sum])
         call make_e_coeff_es_(self,e,R,ex_a,ex_b,g1,g2)
         call create_(k2,k_max); k2 = k1*k1
         es(:,0) = prefac
         es(:,1) = cmplx(e(0,1),e(1,1)*k1,kind=CPX_KIND)*prefac
         es(:,2) = cmplx(e(0,2)-e(2,2)*k2,e(1,2)*k1,kind=CPX_KIND)*prefac
         call destroy_(k2)
         call destroy_(e)

      case (3)

         ! Make the es type E coefficients
         call create_(e,[0,self%l_sum],[0,self%l_sum])
         call make_e_coeff_es_(self,e,R,ex_a,ex_b,g1,g2)
         call create_(k2,k_max); k2 = k1*k1
         call create_(k3,k_max); k3 = k2*k1
         es(:,0) = prefac
         es(:,1) = cmplx(e(0,1),e(1,1)*k1,kind=CPX_KIND)*prefac
         es(:,2) = cmplx(e(0,2)-e(2,2)*k2,e(1,2)*k1,kind=CPX_KIND)*prefac
         es(:,3) = cmplx(e(0,3)-e(2,3)*k2,e(1,3)*k1-e(3,3)*k3,kind=CPX_KIND)*prefac
         call destroy_(k3); call destroy_(k2)
         call destroy_(e)

      case (4)

         ! Make the es type E coefficients
         call create_(e,[0,self%l_sum],[0,self%l_sum])
         call make_e_coeff_es_(self,e,R,ex_a,ex_b,g1,g2)
         call create_(k2,k_max); k2 = k1*k1
         call create_(k3,k_max); k3 = k2*k1
         call create_(k4,k_max); k4 = k3*k1
         es(:,0) = prefac
         es(:,1) = cmplx(e(0,1),e(1,1)*k1,kind=CPX_KIND)*prefac
         es(:,2) = cmplx(e(0,2)-e(2,2)*k2,e(1,2)*k1,kind=CPX_KIND)*prefac
         es(:,3) = cmplx(e(0,3)-e(2,3)*k2,e(1,3)*k1-e(3,3)*k3,kind=CPX_KIND)*prefac
         es(:,4) = cmplx(e(0,4)-e(2,4)*k2+e(4,4)*k4,e(1,4)*k1-e(3,4)*k3,kind=CPX_KIND)*prefac
         call destroy_(k4); call destroy_(k3); call destroy_(k2)
         call destroy_(e)

      case default

         ! Make the es type E coefficients
         call create_(e,[0,self%l_sum],[0,self%l_sum])
         call make_e_coeff_es_(self,e,R,ex_a,ex_b,g1,g2)

         ! Define kp array containing powers k(n)^t
         call create_(kp,[1,k_max],[2,self%l_sum])
         kp(:,2) = k1*k1
         do t = 3,self%l_sum
            kp(:,t) = kp(:,t-1)*k1
         end do
         do t = 2,self%l_sum
            if (mod(t,4)<=1) cycle
            kp(:,t) = -kp(:,t)
         end do

         ! Real, imaginary parts of answer: (vr,vi)
         call create_(vr,k_max)
         call create_(vi,k_max)
         es(:,0) = prefac
         es(:,1) = cmplx(e(0,1),e(1,1)*k1,kind=CPX_KIND)*prefac
         do a = 2,self%l_sum
            vr = e(0,a)
            do t = 2,a,2
               vr = vr+e(t,a)*kp(:,t)
            end do
            vi = e(1,a)*k1
            do t = 3,a,2
               vi = vi+e(t,a)*kp(:,t)
            end do
            es(:,a) = cmplx(vr,vi,kind=CPX_KIND)*prefac
         end do
         call destroy_(vi); call destroy_(vr)
         call destroy_(kp); call destroy_(e)
      end select
   end subroutine

   pure subroutine make_e_coeff_es(self,e,R,ex_a,ex_b,g1,g2)
   ! Return the McMurchie-Davidson (e|s) type "e" coefficients for
   ! component "comp" of the gaussian pair with exponents "ex_a" and
   ! "ex_b" and with flattened pair index "i".
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,0:,0:), OUT :: e
      REAL, IN :: R,ex_a,ex_b,g1,g2
      INT :: a,ap,am,t
      REAL :: b1

      ! The normal starting e00 factor is taken out
      ! and included in cc_prefactor
      e(0,0) = ONE
      if (self%l_sum==0) return

      ! The highest l center is used to generate the integrals
      ! to minimise the transfer cost.
      if (self%a%l>self%b%l) then; b1 = ex_b*g1*R
      else; b1 = -ex_a*g1*R
      end if

      ! e(X,1)
      e(1,1) = g2
      e(0,1) = -b1
      if (self%l_sum==1) return

      ! Make the e(t,a), up to a = .l_sum
      do a = 1,self%l_sum-1
         ap = a+1
         am = a-1
         e(ap,ap) = g2*e(a,a)
         e(a,ap) = g2*e(am,a)-b1*e(a,a)
         e(0,ap) = -b1*e(0,a)+e(1,a)
         do t = 1,am
            e(t,ap) = g2*e(t-1,a)-b1*e(t,a)+(t+1)*e(t+1,a)
         end do
      end do
   end subroutine


!   make_ft_rms_v1(es,Ix,Iy,Iz,wt) :: private, PURE
!   ! Multiply the Ix, Iy, Iz integrals to get the (e|s) integrals
!   ! using th reduced multiplication scheme. This version for FT
!   ! integrals. WARNING: the 0,0,0 case is eliminated, assumed
!   ! to be handled by special case code.
!      self :: INOUT
!      es :: MAT{CPX}, INOUT
!      Ix,Iy,Iz :: MAT{CPX}(1:,0:), IN
!      wt :: VEC{CPX}, IN
!
!      Ixy :: VEC{CPX}@
!   !  exyz0 :: VEC{INT}*
!      ex,ey,ez :: VEC{INT}@
!      eyz0,exz0,exy0,ez0,ey0,ex0,en0,erms :: VEC{INT}*
!      exy_rms :: VEC{EVEC{INT}}@
!      er :: RMS_INDICES@, target
!      ee,em,e,x,y,z,f,l :: INT
!
!      f = .first_gaussian
!      l = .last_gaussian
!
!      ex = GAUSSIAN_DATA::px(f:l); ey = GAUSSIAN_DATA::py(f:l); ez = GAUSSIAN_DATA::pz(f:l)
!      er = GAUSSIAN_DATA::rms_indices(.l_max,.l_min)
!
!   !  exyz0 => er.xyz0
!      eyz0  => er.yz0; exz0 => er.xz0; exy0 => er.xy0
!      ez0   => er.z0;  ey0  => er.y0;  ex0  => er.x0
!      en0   => er.n0
!
!    ! do ee = 1,exyz0.dim
!    !    e = exyz0(ee)
!    !    es(:,e) = es(:,e) + wt
!    ! end
!
!      do ee = 1,eyz0.dim
!         e = eyz0(ee)
!         x = ex(e)
!         es(:,e) = es(:,e) + Ix(:,x)*wt
!      end
!
!      do ee = 1,exz0.dim
!         e = exz0(ee)
!         y = ey(e)
!         es(:,e) = es(:,e) + Iy(:,y)*wt
!      end
!
!      do ee = 1,exy0.dim
!         e = exy0(ee)
!         z = ez(e)
!         es(:,e) = es(:,e) + Iz(:,z)
!      end
!
!      do ee = 1,ez0.dim
!         e = ez0(ee)
!         x = ex(e)
!         y = ey(e)
!         es(:,e) = es(:,e) + Ix(:,x)*Iy(:,y)*wt
!      end
!
!      do ee = 1,ey0.dim
!         e = ey0(ee)
!         x = ex(e)
!         z = ez(e)
!         es(:,e) = es(:,e) + Ix(:,x)*Iz(:,z)
!      end
!
!      do ee = 1,ex0.dim
!         e = ex0(ee)
!         y = ey(e)
!         z = ez(e)
!         es(:,e) = es(:,e) + Iy(:,y)*Iz(:,z)
!      end
!
!      exy_rms => er.xy_rms_n
!
!      if (exy_rms.dim==en0.dim) then
!
!         do ee = 1,en0.dim
!            e = en0(ee)
!            x = ex(e); y = ey(e); z = ez(e)
!            es(:,e) = es(:,e) + Ix(:,x)*Iy(:,y)*Iz(:,z)
!         end
!
!      else if (exy_rms.dim>0) then
!
!         ! Apply (e) RMS here
!         Ixy.create(Ix.dim1)
!         do ee = 1,exy_rms.dim                 
!         
!            erms => exy_rms(ee).element
!            e = en0(erms(1))
!            x = ex(e)
!            y = ey(e)
!            z = ez(e)
!         
!            Ixy = Ix(:,x)*Iy(:,y)
!            es(:,e) = es(:,e) + Ixy*Iz(:,z)
!         
!            do em = 2,erms.dim
!               e = en0(erms(em))
!               z = ez(e)
!               es(:,e) = es(:,e) + Ixy*Iz(:,z)
!            end
!         
!         end
!         
!         Ixy.destroy
!
!      end
!
!   end

   PURE subroutine make_ft_rms_v2(self,es,Ix,Iy,Iz,wt)
   ! Multiply the Ix, Iy, Iz integrals to get the (e|s) integrals
   ! using th reduced multiplication scheme. This version for FT
   ! integrals. This version removes one level of indirection on the
   ! RMS arrays. WARNING: the 0,0,0 case is eliminated, assumed
   ! to be handled by special case code.
      type(SHELL2_TYPE), INOUT :: self
      MAT(CPX,:,:), INOUT :: es
      MAT(CPX,1:,0:), IN :: Ix,Iy,Iz
      VEC(CPX,:), IN :: wt
      VEC(CPX,:), allocatable :: Ixy
   ! xyz0 :: VEC{INT}*
      VEC(INT,:), allocatable :: yz0,yz0px,xz0,xz0py,xy0,xy0pz
      VEC(INT,:), allocatable :: z0,z0px,z0py,y0,y0px,y0pz,x0,x0py,x0pz
      VEC(INT,:), allocatable :: n1,n1px,n1py,n1pz
      VEC(type(EVEC_INT_TYPE),:), allocatable :: n2,n2px,n2py,n2pz
      type(RMS_INDICES_TYPE), allocatable :: er
      INT :: ee,em,e,x,y,z
      er = rms_indices(self%l_max,self%l_min)

   ! xyz0 => er.xyz0
      yz0 = er%yz0; yz0px = er%yz0px
      xz0 = er%xz0; xz0py = er%xz0py
      xy0 = er%xy0; xy0pz = er%xy0pz
      z0 = er%z0; z0px = er%z0px; z0py = er%z0py
      y0 = er%y0; y0px = er%y0px; y0pz = er%y0pz
      x0 = er%x0; x0py = er%x0py; x0pz = er%x0pz
      n1 = er%n1; n1px = er%n1px; n1py = er%n1py; n1pz = er%n1pz
      n2 = er%n2; n2px = er%n2px; n2py = er%n2py; n2pz = er%n2pz

   ! do ee = 1,xyz0.dim
   !    e = xyz0(ee)
   !    es(:,e) = es(:,e) + wt
   ! end
      do ee = 1,size(yz0)
         e = yz0(ee)
         x = yz0px(ee)
         es(:,e) = es(:,e)+Ix(:,x)*wt
      end do
      do ee = 1,size(xz0)
         e = xz0(ee)
         y = xz0py(ee)
         es(:,e) = es(:,e)+Iy(:,y)*wt
      end do
      do ee = 1,size(xy0)
         e = xy0(ee)
         z = xy0pz(ee)
         es(:,e) = es(:,e)+Iz(:,z)
      end do
      do ee = 1,size(z0)
         e = z0(ee)
         x = z0px(ee)
         y = z0py(ee)
         es(:,e) = es(:,e)+Ix(:,x)*Iy(:,y)*wt
      end do
      do ee = 1,size(y0)
         e = y0(ee)
         x = y0px(ee)
         z = y0pz(ee)
         es(:,e) = es(:,e)+Ix(:,x)*Iz(:,z)
      end do
      do ee = 1,size(x0)
         e = x0(ee)
         y = x0py(ee)
         z = x0pz(ee)
         es(:,e) = es(:,e)+Iy(:,y)*Iz(:,z)
      end do
      do ee = 1,size(n1)
         e = n1(ee)
         x = n1px(ee)
         y = n1py(ee)
         z = n1pz(ee)
         es(:,e) = es(:,e)+Ix(:,x)*Iy(:,y)*Iz(:,z)
      end do
      if (size(n2)==0) return
      call create_(Ixy,size(Ix,1))

      ! Apply RMS here
      do ee = 1,size(n2)
         e = n2(ee)%element(1)
         x = n2px(ee)%element(1)
         y = n2py(ee)%element(1)
         z = n2pz(ee)%element(1)
         Ixy = Ix(:,x)*Iy(:,y)
         es(:,e) = es(:,e)+Ixy*Iz(:,z)
         do em = 2,size(n2(ee)%element)
            e = n2(ee)%element(em)
            z = n2pz(ee)%element(em)
            es(:,e) = es(:,e)+Ixy*Iz(:,z)
         end do
      end do
      call destroy_(Ixy)
   end subroutine


!  ==============================
!  Partition and thermal smearing
!  ==============================

   PURE subroutine make_Tanaka_partition_0(self,part,fa,fb)
   ! Make "part", the symmetry factors for partitioning the density
   ! contribution over *primitive* gaussian pair. 
   ! NOTE: this is the Tanaka scheme, eqn (21) in JG (2001) paper.
   ! NOTE: fa & fb are 1/n_A and 1/n_B in that paper.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: part
      REAL, IN :: fa,fb
      INT :: a,b
      REAL :: ex_a,ex_b
   ENSURE(size(part,1)==self%a%n_cc,"SHELL2:make_Tanaka_partition_0 ... wrong dim1, part")
   ENSURE(size(part,2)==self%b%n_cc,"SHELL2:make_Tanaka_partition_0 ... wrong dim1, part")
      do b = 1,self%b%n_cc
         ex_b = self%b%exponent(b)
         do a = 1,self%a%n_cc
            ex_a = self%a%exponent(a)
            part(a,b) = (ex_b*fb+ex_a*fa)/(ex_a+ex_b)
         end do
      end do
   end subroutine

   PURE subroutine make_Tanaka_partition_1(self,part,sa,sb,center)
   ! Make "part", the Tanaka factors for partitioning the density for
   ! atom *a* over all pairs of gaussians in the shell.
   ! K. Tanaka (1988) Acta Cryst. A44, 1002–8 
   ! NOTE: this is the Tanaka scheme, eqn (21) in JG (2001) paper.
   ! NOTE: sa & sb are 1/n_A and 1/n_B in that paper which are the
   !       same as site-symmetry factors when applied single-atom
   !       regions ... I think ... DJ
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: part
      REAL, IN :: sa,sb
      STR(len=*), IN :: center
      INT :: a,b
      REAL :: ea,eb
      REAL :: fa,fb
   ENSURE(is_one_of_(center,["A","B"]),"SHELL2:make_Tanaka_partition_1 ... wrong dim1, res")
   ENSURE(size(part,1)==self%a%n_cc,"SHELL2:make_Tanaka_partition_1 ... wrong dim1, part")
   ENSURE(size(part,2)==self%b%n_cc,"SHELL2:make_Tanaka_partition_1 ... wrong dim2, part")
      select case (center)

      case ("A")
         do b = 1,self%b%n_cc
            eb = self%b%exponent(b)
            do a = 1,self%a%n_cc
               ea = self%a%exponent(a)
               fa = ea*sa
               part(a,b) = fa/(ea+eb)
            end do
         end do

      case ("B")
         do b = 1,self%b%n_cc
            eb = self%b%exponent(b)
            fb = eb*sb
            do a = 1,self%a%n_cc
               ea = self%a%exponent(a)
               part(a,b) = fb/(ea+eb)
            end do
         end do
      end select  ! -- select case
   end subroutine

   pure subroutine make_Tanaka_TFs(self,TF,k_pts,Ua,Ub)
   ! Make the Tanaka two-center temperature factors "TF" for each pair
   ! of gaussian primitives, given ADPs "Ua" and "Ub" on each center.
      type(SHELL2_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: TF
      MAT(REAL,:,:), IN :: k_pts
      MAT(REAL,:,:), IN :: Ua,Ub

   ! ENSURE(TF.dim1==k_pts.dim1,"wrong dim1, TF")
   ! ENSURE(TF.dim2==.a.n_cc,"wrong dim2, TF")
   ! ENSURE(TF.dim3==.b.n_cc,"wrong dim3, TF")
      MAT(REAL,3,3) :: U
      MAT(REAL,3,3) :: Ua_ea
      MAT(REAL,3,3) :: Ub_eb
      REAL :: ea,eb
      REAL :: U11,U22,U33
      REAL :: U21_12
      REAL :: U31_13
      REAL :: U32_23
      REAL :: S1,S2,S3
      INT :: a,b,k,k_max
      k_max = size(k_pts,1)
      do a = 1,self%a%n_cc
         ea = self%a%exponent(a)
         Ua_ea = ea*Ua
         do b = 1,self%b%n_cc
            eb = self%b%exponent(b)
            Ub_eb = eb*Ub
            U = -HALF*(Ua_ea+Ub_eb)/(ea+eb)
            U11 = U(1,1)
            U22 = U(2,2)
            U33 = U(3,3)
            U21_12 = U(2,1)+U(1,2)
            U31_13 = U(3,1)+U(1,3)
            U32_23 = U(3,2)+U(2,3)
            do k = 1,k_max
               S1 = k_pts(k,1)
               S2 = k_pts(k,2)
               S3 = k_pts(k,3)

               ! S.U.S has been expanded to save multiplies and array accesses.
               TF(k,a,b) = exp(S1*(S1*U11+S2*U21_12+S3*U31_13)+S2*(S2*U22+S3*U32_23)+S3*S3*U33)
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_KM_partition(self,part)
   ! Make the Koritsanzky-Michael factors for partitioning the density
   ! contribution for atom *a* in the shell. res is [.a.n_cc,.b.n_cc].
   ! J.R. Michael & T.Koritsanzky (2015) J Math Chem 53, p 250-9
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: part
      INT :: a,b
      REAL :: ea,eb,a2,b2,ab
   ENSURE(size(part,1)==self%a%n_cc,"SHELL2:make_KM_partition ... wrong dim1, part")
   ENSURE(size(part,2)==self%b%n_cc,"SHELL2:make_KM_partition ... wrong dim2, part")
      do b = 1,self%b%n_cc
         eb = self%b%exponent(b)
         b2 = eb*eb
         do a = 1,self%a%n_cc
            ea = self%a%exponent(a)
            a2 = ea*ea
            ab = ea*eb
            part(a,b) = (a2+ab)/(a2+TWO*ab+b2)
         end do
      end do
   end subroutine


!  ===================================
!  Fourier transform current integrals
!  ===================================

   PURE subroutine make_ft_nabla_0(self,res,k_pts)
   ! Calculates the Fourier transform for a product of the gradient of
   ! shell a minus shell b for two contracted gaussian shells, evaluated
   ! at a series of k points k_pts
   ! Dimensions of res are [k_max,.n_comp_a,.n_comp_b,3].
      type(SHELL2_TYPE), IN :: self
      MAT4(CPX,:,:,:,:), OUT :: res
      MAT(REAL,:,:), IN :: k_pts
      type(GAUSSIAN2_TYPE) :: G
      MAT4(CPX,:,:,:,:), allocatable :: ft_ab
      INT :: a,b,k_max
      REAL :: ca,cb,cacb
      k_max = size(k_pts,1)
      res = ZERO
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do a = 1,self%a%n_cc
         call set_(G,ex_a=self%a%exponent(a))
         ca = self%a%contraction(a)
         do b = 1,self%b%n_cc
            call set_(G,ex_b=self%b%exponent(b))
            cb = self%b%contraction(b)
            cacb = ca*cb
            call create_(ft_ab,k_max,self%a%n_comp,self%b%n_comp,3)
            call make_ft_nabla_(G,ft_ab,k_pts)
            res = res+ft_ab*cacb
            call destroy_(ft_ab)
         end do
      end do
   end subroutine

   PURE subroutine make_ft_nabla_1(self,res,k_pts,thermal,partition)
   ! Calculates "res" the Fourier transform for a product of the gradient of
   ! shell a minus shell b for two contracted gaussian shells, evaluated
   ! at a series of k points "k_pts", including "thermal" smearing corrections
   ! and "partition" factors.
   ! Dimensions of res are [k_max,.n_comp_a,.n_comp_b,3].
      type(SHELL2_TYPE), IN :: self
      MAT4(CPX,:,:,:,:), OUT :: res
      MAT(REAL,:,:), IN :: k_pts
      MAT3(REAL,:,:,:), IN :: thermal
      MAT(REAL,:,:), IN :: partition
      VEC(REAL,:), allocatable :: therm
      type(GAUSSIAN2_TYPE) :: G
      MAT4(CPX,:,:,:,:), allocatable :: ft_ab
      INT :: a,b,k_max,i,j
      REAL :: ca,cb,fac
      k_max = size(k_pts,1)
      res = ZERO
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do a = 1,self%a%n_cc
         call set_(G,ex_a=self%a%exponent(a))
         ca = self%a%contraction(a)
         do b = 1,self%b%n_cc
            call set_(G,ex_b=self%b%exponent(b))
            cb = self%b%contraction(b)
            call create_(ft_ab,k_max,self%a%n_comp,self%b%n_comp,3)
            call make_ft_nabla_(G,ft_ab,k_pts)
            fac = ca*cb*partition(a,b)
            therm = thermal(:,a,b)
            do i = 1,self%a%n_comp
               do j = 1,self%b%n_comp
                  res(:,i,j,1) = res(:,i,j,1)+ft_ab(:,i,j,1)*fac*therm(:)
                  res(:,i,j,2) = res(:,i,j,2)+ft_ab(:,i,j,2)*fac*therm(:)
                  res(:,i,j,3) = res(:,i,j,3)+ft_ab(:,i,j,3)*fac*therm(:)
               end do
            end do
            call destroy_(ft_ab)
         end do
      end do
   end subroutine

   PURE subroutine make_ft_r(self,res,k_pts)
   ! Calculates the Fourier transform for a product of two contracted
   ! gaussian shells, evaluated at a series of k points k_pts
   ! Dimensions of res are [k_max,.n_comp_a,.n_comp_b,3].
      type(SHELL2_TYPE), IN :: self
      MAT4(CPX,:,:,:,:), OUT :: res
      MAT(REAL,:,:), IN :: k_pts
      type(GAUSSIAN2_TYPE) :: G
      MAT4(CPX,:,:,:,:), allocatable :: ft_ab
      INT :: a,b,k_max
      REAL :: ca,cb,cacb
      k_max = size(k_pts,1)
      res = ZERO
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do a = 1,self%a%n_cc
         call set_(G,ex_a=self%a%exponent(a))
         ca = self%a%contraction(a)
         do b = 1,self%b%n_cc
            call set_(G,ex_b=self%b%exponent(b))
            cb = self%b%contraction(b)
            cacb = ca*cb
            call create_(ft_ab,k_max,self%a%n_comp,self%b%n_comp,3)
            call make_ft_r_(G,ft_ab,k_pts)
            res = res+ft_ab*cacb
            call destroy_(ft_ab)
         end do
      end do
   end subroutine


!  ===========================
!  Averaged fourier transforms
!  ===========================

   PURE subroutine make_spherically_averaged_ft(self,res,k_values)
   ! Calculates the spherically averaged Fourier transform for a product of two
   ! contracted gaussian shells, "res", evaluated at a series of "k_values".
   ! Dimensions of res are [.a.n_comp,.b.n_comp,k_values.dim].
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: res
      VEC(REAL,:), IN :: k_values
      type(GAUSSIAN2_TYPE) :: G
      MAT(REAL,:,:), allocatable :: k_pts
      MAT3(CPX,:,:,:), allocatable :: ft_ab
      VEC(REAL,:), allocatable :: wt,fac_wt
      INT :: k,n,n_pts,t,p,a,b,i,j
      REAL :: k_val,k_val_st,th,ph,ct,st,cp,sp,ca,fac
      type(QUADRATURE_TYPE), allocatable :: theta,phi
   ENSURE(size(res,1)==self%a%n_comp,"SHELL2:make_spherically_averaged_ft ... wrong 1st dimension for res")
   ENSURE(size(res,2)==self%b%n_comp,"SHELL2:make_spherically_averaged_ft ... wrong 2nd dimension for res")
   ENSURE(size(res,3)==size(k_values),"SHELL2:make_spherically_averaged_ft ... wrong 3rd dimension for res")

      ! First set the quadrature objects
      n = 10
      n_pts = 2*n*n
      call create_(theta)
      call set_kind_(theta,"gauss_legendre")
      call set_n_pts_(theta,n)
      call set_quadrature_points_(theta)
      call set_limits_(theta,[ZERO,PI])
      call create_(phi)
      call set_kind_(phi,"gauss_legendre")
      call set_n_pts_(phi,2*n)
      call set_quadrature_points_(phi)
      call set_limits_(phi,[ZERO,2*PI])
      res = ZERO
      call create_(wt,n_pts)
      call create_(fac_wt,n_pts)
      call create_(k_pts,n_pts,3)
      call create_(ft_ab,n_pts,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)

      ! Loop over each k_value one by one
      do k = 1,size(k_values)
         k_val = k_values(k)
         n = 0
         ! For each k_value, define a shell of spherical k_pts
         do t = 1,theta%n_pts
            do p = 1,phi%n_pts
               n = n+1
               th = theta%pts(t)
               ph = phi%pts(p)
               ct = cos(th); st = sin(th)
               cp = cos(ph); sp = sin(ph)
               k_val_st = k_val*st
               k_pts(n,1) = k_val_st*cp  ! the spherical shell
               k_pts(n,2) = k_val_st*sp
               k_pts(n,3) = k_val*ct
               wt(n) = st*theta%wts(t)*phi%wts(p)  ! the area x weights
            end do
         end do

         ! Now do the Fourier transform for this shell, and average it
         do a = 1,self%a%n_cc
            call set_(G,ex_a=self%a%exponent(a))
            ca = self%a%contraction(a)
            do b = 1,self%b%n_cc
               call set_(G,ex_b=self%b%exponent(b))
               call make_ft_(G,ft_ab,k_pts)
               fac = ca*self%b%contraction(b)
               fac_wt = fac*wt
               do j = 1,self%b%n_comp
                  do i = 1,self%a%n_comp
                     res(i,j,k) = res(i,j,k)+sum(ft_ab(:,i,j)*fac_wt)
                  end do
               end do
            end do
         end do
      end do
      call destroy_(ft_ab)
      call destroy_(k_pts)
      call destroy_(fac_wt)
      call destroy_(wt)
      call destroy_(phi); call destroy_(theta)
      fac = ONE/(FOUR*PI)
      res = fac*res
   end subroutine

   PURE subroutine make_z_circularly_averaged_ft(self,res,k_values,n_theta)
   ! Calculates the z circularly averaged Fourier transform for a product of two
   ! contracted gaussian shells, "res", evaluated at a series of "k_values" in
   ! reciprocal space, and with "n_theta" equally spaced angles from the z axis,
   ! from 0 -> 2*Pi. Size of "res" is [.a.n_comp,.b.n_comp,k_values.dim,n_theta].
      type(SHELL2_TYPE), IN :: self
      MAT4(CPX,:,:,:,:), OUT :: res
      VEC(REAL,:), IN :: k_values
      INT, IN :: n_theta
      type(GAUSSIAN2_TYPE) :: G
      MAT(REAL,:,:), allocatable :: k_pts
      MAT3(CPX,:,:,:), allocatable :: ft_ab
      VEC(REAL,:), allocatable :: wt,fac_wt
      INT :: k,n,n_pts,t,p,a,b,f,l
      REAL :: k_val,k_val_st,th,ph,ct,st,cp,sp,fac,angle
      type(QUADRATURE_TYPE), allocatable :: phi
   ENSURE(size(res,1)==self%a%n_comp,"SHELL2:make_z_circularly_averaged_ft ... wrong 1st dimension for res")
   ENSURE(size(res,2)==self%b%n_comp,"SHELL2:make_z_circularly_averaged_ft ... wrong 2nd dimension for res")
   ENSURE(size(res,3)==size(k_values),"SHELL2:make_z_circularly_averaged_ft ... wrong 3rd dimension for res")
   ENSURE(size(res,4)==n_theta+1,"SHELL2:make_z_circularly_averaged_ft ... wrong 4th dimension for res")

      ! First set the quadrature objects
      angle = (PI/(2*n_theta))
      n = 10  ! points per Pi for phi
      n_pts = 2*n*(n_theta+1)  !
      call create_(phi)
      call set_kind_(phi,"gauss_legendre")
      call set_n_pts_(phi,2*n)
      call set_quadrature_points_(phi)
      call set_limits_(phi,[ZERO,2*PI])
      res = ZERO
      call create_(wt,n_pts)
      call create_(fac_wt,n_pts)
      call create_(k_pts,n_pts,3)
      call create_(ft_ab,n_pts,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)

      ! Loop over each k_value one by one
      do k = 1,size(k_values)
         k_val = k_values(k)
         n = 0
         ! For each k_value, define a shell of spherical k_pts
         do t = 0,n_theta
            do p = 1,phi%n_pts
               n = n+1
               th = t*angle
               ph = phi%pts(p)
               ct = cos(th); st = sin(th)
               cp = cos(ph); sp = sin(ph)
               k_val_st = k_val*st
               k_pts(n,1) = k_val_st*cp  ! the spherical shell
               k_pts(n,2) = k_val_st*sp
               k_pts(n,3) = k_val*ct
            end do
         end do

         ! Now do the Fourier transform for this shell, and average it
         call make_ft_static_(self,ft_ab,k_pts)
         f = 1
         l = phi%n_pts
         do t = 1,n_theta+1
            do b = 1,self%b%n_comp
               do a = 1,self%a%n_comp
                  res(a,b,k,t) = res(a,b,k,t)+sum(ft_ab(f:l,a,b)*phi%wts)
               end do
            end do
            f = f+phi%n_pts
            l = l+phi%n_pts
         end do
      end do
      call destroy_(ft_ab)
      call destroy_(k_pts)
      call destroy_(fac_wt)
      call destroy_(wt)
      call destroy_(phi)
      fac = ONE/(TWO*PI)
      res = fac*res
   end subroutine

!  ======================
!  CADPAC-style integrals
!  ======================

   PURE subroutine make_overlap_ints(self,I)
   ! Calculate "I, a generic cartesian or spherical
   ! block of integrals.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: I
      MAT(REAL,:,:), allocatable :: W

      ! The following code is inherited from SHELL2
      if (NOT self%is_spherical) then
         call make_overlap_ints_c_(self,I)
      else if (self%l_max<2) then
         call make_overlap_ints_c_(self,I)
      else
         call create_(W,self%a%n_comp,self%b%n_comp)
         call make_overlap_ints_c_(self,W)
         call change_to_spherical_(self,W,I)
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_overlap_ints_c(self,S)
   ! Calculates overlap integral matrix, using Gauss-Hermite
   ! quadrature, like in CADPAC. This is for cartesians only.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: S
      MAT(REAL,:,:), allocatable :: SS
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb
      S = ZERO
      call create_(SS,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_overlap_ints_(G,SS)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            S = S+SS*cacb
         end do
      end do
      call destroy_(SS)
   end subroutine

   PURE subroutine make_3_center_overlap_ints(self,S,c)
   ! Make "S(a,b,c)", the overlap integrals of the two shells with a
   ! third shell "c". The a and b indices are spherical if the shell
   ! pair is, the c index if "c" is; "S" is dimensioned to suit.
      type(SHELL2_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: S
      type(SHELL1_TYPE), IN :: c
      MAT3(REAL,:,:,:), allocatable :: SS,T
      MAT(REAL,:,:), allocatable :: W,U
      type(GAUSSIAN2_TYPE) :: G
      INT :: ia,ib,ic,k,m
      BIN :: c_sph
      c_sph = c%is_spherical AND c%l>=2

      ! Contracted cartesian integrals
      call create_(T,self%a%n_comp,self%b%n_comp,c%n_comp)
      call create_(SS,self%a%n_comp,self%b%n_comp,c%n_comp)
      T = ZERO
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do ic = 1,c%n_cc
         do ib = 1,self%b%n_cc
            do ia = 1,self%a%n_cc
               call set_(G,self%a%exponent(ia),self%b%exponent(ib))
               call make_3_center_overlap_ints_(G,SS,c%l,c%position,c%exponent(ic))
               T = T+SS*(self%a%contraction(ia)*self%b%contraction(ib)*c%contraction(ic))
            end do
         end do
      end do

      ! The c index to spherical
      if (c_sph) then
         U = spherical_harmonics_for(c%l)%element
         call destroy_(SS)
         call create_(SS,self%a%n_comp,self%b%n_comp,c%n_sph)
         SS = ZERO
         do m = 1,c%n_sph
            do k = 1,c%n_comp
               SS(:,:,m) = SS(:,:,m)+T(:,:,k)*U(k,m)
            end do
         end do
         call destroy_(T)
         T = SS
      end if
      call destroy_(SS)

      ! The a and b indices to spherical
      if (self%is_spherical AND self%l_max>=2) then
         call create_(W,self%a%n_comp,self%b%n_comp)
         do m = 1,size(T,3)
            W = T(:,:,m)
            call change_to_spherical_(self,W,S(:,:,m))
         end do
         call destroy_(W)
      else
         S = T
      end if
      call destroy_(T)
   end subroutine

   PURE subroutine make_weighted_overlap_ints(self,Sa,Sb,fab)
   ! Calculates weighted overlap integrals "Sa" and "Sb" from a given
   ! set of primitive weights "fab" weight assign to shell a centered.
   ! NOTE: should only be called when the centers are different.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Sa,Sb
      MAT(REAL,:,:), IN :: fab
      MAT(REAL,:,:), allocatable :: SS
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb,fa,fb
      Sa = ZERO
      Sb = ZERO
      call create_(SS,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            SS = ZERO
            call make_overlap_ints_(G,SS)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            fa = fab(a,b)
            fb = ONE-fa
            fa = fa*cacb
            fb = fb*cacb
            Sa = Sa+fa*SS
            Sb = Sb+fb*SS
         end do
      end do
      call destroy_(SS)
   end subroutine

   PURE subroutine make_weighted_dipole_ints_0(self,Ax,Ay,Az,Bx,By,Bz,fab)
   ! Make the weighted dipole moment integral matrices.
   ! Dipole origin are at the respective shell positions.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ax,Ay,Az
      MAT(REAL,:,:), OUT :: Bx,By,Bz
      MAT(REAL,:,:), IN :: fab
      MAT(REAL,:,:), allocatable :: DDx,DDy,DDz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b,n_a,n_b
      REAL :: fa,fb,cacb
      Ax = ZERO; Ay = ZERO; Az = ZERO
      Bx = ZERO; By = ZERO; Bz = ZERO
      n_a = self%a%n_comp; n_b = self%b%n_comp
      call create_(DDx,n_a,n_b); call create_(DDy,n_a,n_b); call create_(DDz,n_a,n_b)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            fa = fab(a,b)
            fb = ONE-fa
            cacb = self%a%contraction(a)*self%b%contraction(b)
            fa = fa*cacb
            fb = fb*cacb
            call make_dipole_ints_(G,DDx,DDy,DDz,self%a%position)
            Ax = Ax+fa*DDx; Ay = Ay+fa*DDy; Az = Az+fa*DDz
            call make_dipole_ints_(G,DDx,DDy,DDz,self%b%position)
            Bx = Bx+fb*DDx; By = By+fb*DDy; Bz = Bz+fb*DDz
         end do
      end do
      call destroy_(DDz); call destroy_(DDy); call destroy_(DDx)
   end subroutine

   PURE subroutine make_weighted_quadrupole_ints(self,Axx,Ayy,Azz,Axy,Axz,Ayz,Bxx,Byy,Bzz,Bxy,Bxz,Byz,fab)
   ! Make the quadrupole moment integral matrices "Qij" weighted by given factors
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Axx,Ayy,Azz,Axy,Axz,Ayz,Bxx,Byy,Bzz,Bxy,Bxz,Byz
      MAT(REAL,:,:), IN :: fab
      MAT(REAL,:,:), allocatable :: QQxx,QQyy,QQzz,QQxy,QQxz,QQyz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b,n_a,n_b
      REAL :: fa,fb,cacb
      Axx = ZERO; Ayy = ZERO; Azz = ZERO
      Axy = ZERO; Axz = ZERO; Ayz = ZERO
      Bxx = ZERO; Byy = ZERO; Bzz = ZERO
      Bxy = ZERO; Bxz = ZERO; Byz = ZERO
      n_a = self%a%n_comp; n_b = self%b%n_comp
      call create_(QQxx,n_a,n_b); call create_(QQyy,n_a,n_b); call create_(QQzz,n_a,n_b)
      call create_(QQxy,n_a,n_b); call create_(QQxz,n_a,n_b); call create_(QQyz,n_a,n_b)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            fa = fab(a,b)
            fb = ONE-fa
            cacb = self%a%contraction(a)*self%b%contraction(b)
            fa = fa*cacb
            fb = fb*cacb
            call make_quadrupole_ints_(G,QQxx,QQyy,QQzz,QQxy,QQxz,QQyz,self%a%position)
            Axx = Axx+QQxx*fa; Ayy = Ayy+QQyy*fa; Azz = Azz+QQzz*fa
            Axy = Axy+QQxy*fa; Axz = Axz+QQxz*fa; Ayz = Ayz+QQyz*fa
            call make_quadrupole_ints_(G,QQxx,QQyy,QQzz,QQxy,QQxz,QQyz,self%b%position)
            Bxx = Bxx+QQxx*fb; Byy = Byy+QQyy*fb; Bzz = Bzz+QQzz*fb
            Bxy = Bxy+QQxy*fb; Bxz = Bxz+QQxz*fb; Byz = Byz+QQyz*fb
         end do
      end do
      call destroy_(QQyz); call destroy_(QQxz); call destroy_(QQxy)
      call destroy_(QQzz); call destroy_(QQyy); call destroy_(QQxx)
   end subroutine

   PURE subroutine make_weighted_octupole_ints(self,Axxx,Ayyy,Azzz,Axxy,Axxz,Ayyx,Ayyz,Azzx,Azzy,Axyz,Bxxx,Byyy,Bzzz,Bxxy,Bxxz,Byyx,Byyz,Bzzx,Bzzy,Bxyz,fab)
   ! Make the octupole moment integral matrices "Oijk"
   ! with gauge origin "origin"
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), IN :: fab
      MAT(REAL,:,:), OUT :: Axxx,Ayyy,Azzz,Axxy,Axxz,Ayyx,Ayyz,Azzx,Azzy,Axyz
      MAT(REAL,:,:), OUT :: Bxxx,Byyy,Bzzz,Bxxy,Bxxz,Byyx,Byyz,Bzzx,Bzzy,Bxyz
      MAT(REAL,:,:), allocatable :: OOxxx,OOyyy,OOzzz,OOxxy,OOxxz,OOyyx,OOyyz,OOzzx,OOzzy,OOxyz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b,n_a,n_b
      REAL :: fa,fb,cacb
      Axxx = ZERO; Ayyy = ZERO; Azzz = ZERO
      Axxy = ZERO; Axxz = ZERO
      Ayyx = ZERO; Ayyz = ZERO
      Azzx = ZERO; Azzy = ZERO
      Axyz = ZERO
      Bxxx = ZERO; Byyy = ZERO; Bzzz = ZERO
      Bxxy = ZERO; Bxxz = ZERO
      Byyx = ZERO; Byyz = ZERO
      Bzzx = ZERO; Bzzy = ZERO
      Bxyz = ZERO
      n_a = self%a%n_comp; n_b = self%b%n_comp
      call create_(OOxxx,n_a,n_b); call create_(OOyyy,n_a,n_b); call create_(OOzzz,n_a,n_b)
      call create_(OOxxy,n_a,n_b); call create_(OOxxz,n_a,n_b)
      call create_(OOyyx,n_a,n_b); call create_(OOyyz,n_a,n_b)
      call create_(OOzzx,n_a,n_b); call create_(OOzzy,n_a,n_b)
      call create_(OOxyz,n_a,n_b)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            fa = fab(a,b)
            fb = ONE-fa
            cacb = self%a%contraction(a)*self%b%contraction(b)
            fa = fa*cacb
            fb = fb*cacb
            call make_octupole_ints_(G,OOxxx,OOyyy,OOzzz,OOxxy,OOxxz,OOyyx,OOyyz,OOzzx,OOzzy,OOxyz,self%a%position)
            Axxx = Axxx+OOxxx*fa; Ayyy = Ayyy+OOyyy*fa; Azzz = Azzz+OOzzz*fa
            Axxy = Axxy+OOxxy*fa; Axxz = Axxz+OOxxz*fa
            Ayyx = Ayyx+OOyyx*fa; Ayyz = Ayyz+OOyyz*fa
            Azzx = Azzx+OOzzx*fa; Azzy = Azzy+OOzzy*fa
            Axyz = Axyz+OOxyz*fa
            call make_octupole_ints_(G,OOxxx,OOyyy,OOzzz,OOxxy,OOxxz,OOyyx,OOyyz,OOzzx,OOzzy,OOxyz,self%b%position)
            Bxxx = Bxxx+OOxxx*fb; Byyy = Byyy+OOyyy*fb; Bzzz = Bzzz+OOzzz*fb
            Bxxy = Bxxy+OOxxy*fb; Bxxz = Bxxz+OOxxz*fb
            Byyx = Byyx+OOyyx*fb; Byyz = Byyz+OOyyz*fb
            Bzzx = Bzzx+OOzzx*fb; Bzzy = Bzzy+OOzzy*fb
            Bxyz = Bxyz+OOxyz*fb
         end do
      end do
      call destroy_(OOxyz)
      call destroy_(OOzzy); call destroy_(OOzzx)
      call destroy_(OOyyz); call destroy_(OOyyx)
      call destroy_(OOxxz); call destroy_(OOxxy)
      call destroy_(OOzzz); call destroy_(OOyyy); call destroy_(OOxxx)
   end subroutine

   PURE subroutine make_oscillator_overlap_ints(self,S,c,d,lc_min,lc_max,ld_min,ld_max)
   ! Calculates oscillator overlap integral matrix, using Gauss-Hermite
   ! quadrature, like in CADPAC
      type(SHELL2_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: S
      VEC(REAL,3), IN :: c,d
      INT, IN :: lc_min,lc_max,ld_min,ld_max
      MAT4(REAL,:,:,:,:), allocatable :: SS
      type(GAUSSIAN2_TYPE) :: G
      REAL :: cacb
      INT :: a,b,nc,nd
   ENSURE(size(S,1)==self%a%n_comp,"SHELL2:make_oscillator_overlap_ints ... wrong size for S")
   ENSURE(size(S,2)==self%b%n_comp,"SHELL2:make_oscillator_overlap_ints ... wrong size for S")
   ENSURE(size(S,3)==(n_comp_up_to_(lc_max)-n_comp_up_to_(lc_min-1)),"SHELL2:make_oscillator_overlap_ints ... wrong size for S")
   ENSURE(size(S,4)==(n_comp_up_to_(ld_max)-n_comp_up_to_(ld_min-1)),"SHELL2:make_oscillator_overlap_ints ... wrong size for S")
      S = ZERO
      nc = n_comp_up_to_(lc_max)-n_comp_up_to_(lc_min-1)
      nd = n_comp_up_to_(ld_max)-n_comp_up_to_(ld_min-1)
      call create_(SS,self%a%n_comp,self%b%n_comp,nc,nd)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            cacb = self%a%contraction(a)*self%b%contraction(b)
            call make_oscillator_overlap_ints_(G,SS,c,d,lc_min,lc_max,ld_min,ld_max)
            S = S+SS*cacb
         end do
      end do
      call destroy_(SS)
   end subroutine

   subroutine make_S_1st_deriv_ints(self,Ax,Ay,Az)
   ! Calculates the derivatives of the overlap integrals with respect to
   ! position A, in "Ax", "Ay", and "Az" using Gauss-Hermite quadrature.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ax,Ay,Az
      MAT(REAL,:,:), allocatable :: AAx,AAy,AAz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb
      Ax = ZERO; Ay = ZERO; Az = ZERO
      call create_(AAx,self%a%n_comp,self%b%n_comp)
      call create_(AAy,self%a%n_comp,self%b%n_comp)
      call create_(AAz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_S_1st_deriv_ints_(G,AAx,AAy,AAz)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Ax = Ax+AAx*cacb
            Ay = Ay+AAy*cacb
            Az = Az+AAz*cacb
         end do
      end do
      call destroy_(AAz)
      call destroy_(AAy)
      call destroy_(AAx)
   end subroutine

   subroutine make_T_1st_deriv_ints(self,Ax,Ay,Az)
   ! Calculates the derivatives of the kinetic integrals with respect to
   ! position A, in "Ax", "Ay", and "Az" using Gauss-Hermite quadrature.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ax,Ay,Az
      MAT(REAL,:,:), allocatable :: AAx,AAy,AAz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb
      Ax = ZERO; Ay = ZERO; Az = ZERO
      call create_(AAx,self%a%n_comp,self%b%n_comp)
      call create_(AAy,self%a%n_comp,self%b%n_comp)
      call create_(AAz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_T_1st_deriv_ints_(G,AAx,AAy,AAz)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Ax = Ax+AAx*cacb
            Ay = Ay+AAy*cacb
            Az = Az+AAz*cacb
         end do
      end do
      call destroy_(AAz)
      call destroy_(AAy)
      call destroy_(AAx)
   end subroutine

   subroutine make_NA_1st_deriv_ints(self,Ax,Ay,Az,Bx,By,Bz,c)
   ! Calculates the derivatives of the nuclear attraction integrals with respect to
   ! positions A *and* B, in "Ax", "Ay", "Az", and "Bx", "By", "Bz" for a given
   ! nuclear position "c".
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ax,Ay,Az,Bx,By,Bz
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: AAx,AAy,AAz,BBx,BBy,BBz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb
      Ax = ZERO; Ay = ZERO; Az = ZERO
      Bx = ZERO; By = ZERO; Bz = ZERO
      call create_(AAx,self%a%n_comp,self%b%n_comp)
      call create_(AAy,self%a%n_comp,self%b%n_comp)
      call create_(AAz,self%a%n_comp,self%b%n_comp)
      call create_(BBx,self%a%n_comp,self%b%n_comp)
      call create_(BBy,self%a%n_comp,self%b%n_comp)
      call create_(BBz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_NA_1st_deriv_ints_(G,AAx,AAy,AAz,BBx,BBy,BBz,c)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Ax = Ax+AAx*cacb
            Ay = Ay+AAy*cacb
            Az = Az+AAz*cacb
            Bx = Bx+BBx*cacb
            By = By+BBy*cacb
            Bz = Bz+BBz*cacb
         end do
      end do
      call destroy_(BBz); call destroy_(BBy); call destroy_(BBx)
      call destroy_(AAz); call destroy_(AAy); call destroy_(AAx)
   end subroutine

   PURE subroutine make_dipole_ints(self,Dx,Dy,Dz,origin)
   ! As .make_dipole_ints_c, but spherical blocks for a spherical shell pair.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Dx,Dy,Dz
      VEC(REAL,3), IN :: origin
      MAT3(REAL,:,:,:), allocatable :: W
      if (NOT self%is_spherical OR self%l_max<2) then
         call make_dipole_ints_c_(self,Dx,Dy,Dz,origin)
      else
         call create_(W,self%a%n_comp,self%b%n_comp,3)
         call make_dipole_ints_c_(self,W(:,:,1),W(:,:,2),W(:,:,3),origin)
         call change_to_spherical_(self,W(:,:,1),Dx)
         call change_to_spherical_(self,W(:,:,2),Dy)
         call change_to_spherical_(self,W(:,:,3),Dz)
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_dipole_ints_c(self,Dx,Dy,Dz,origin)
   ! Make the dipole moment integral matrices "Di"
   ! with gauge origin "origin"
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Dx,Dy,Dz
      VEC(REAL,3), IN :: origin
      MAT(REAL,:,:), allocatable :: DDx,DDy,DDz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b,n_a,n_b
      REAL :: cacb
      Dx = ZERO
      Dy = ZERO
      Dz = ZERO
      n_a = self%a%n_comp
      n_b = self%b%n_comp
      call create_(DDx,n_a,n_b)
      call create_(DDy,n_a,n_b)
      call create_(DDz,n_a,n_b)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_dipole_ints_(G,DDx,DDy,DDz,origin)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Dx = Dx+DDx*cacb
            Dy = Dy+DDy*cacb
            Dz = Dz+DDz*cacb
         end do
      end do
      call destroy_(DDz)
      call destroy_(DDy)
      call destroy_(DDx)
   end subroutine

   PURE subroutine make_weighted_dipole_ints_1(self,Ax,Ay,Az,Bx,By,Bz)
   ! Make the weighted dipole moment integral matrices.
   ! Dipole origin are at the respective shell positions.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ax,Ay,Az,Bx,By,Bz
      MAT(REAL,:,:), allocatable :: DDx,DDy,DDz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b,n_a,n_b
      REAL :: fa,fb,cacb
      Ax = ZERO; Ay = ZERO; Az = ZERO
      Bx = ZERO; By = ZERO; Bz = ZERO
      n_a = self%a%n_comp
      n_b = self%b%n_comp
      call create_(DDx,n_a,n_b); call create_(DDy,n_a,n_b); call create_(DDz,n_a,n_b)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            fa = self%a%exponent(a)/(self%a%exponent(a)+self%b%exponent(b))
            fb = ONE-fa
            cacb = self%a%contraction(a)*self%b%contraction(b)
            fa = fa*cacb
            fb = fb*cacb
            call make_dipole_ints_(G,DDx,DDy,DDz,self%a%position)
            Ax = Ax+fa*DDx; Ay = Ay+fa*DDy; Az = Az+fa*DDz
            call make_dipole_ints_(G,DDx,DDy,DDz,self%b%position)
            Bx = Bx+fb*DDx; By = By+fb*DDy; Bz = Bz+fb*DDz
         end do
      end do
      call destroy_(DDz); call destroy_(DDy); call destroy_(DDx)
   end subroutine

   PURE subroutine make_quadrupole_ints(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz,origin)
   ! As .make_quadrupole_ints_c, but spherical blocks for a spherical shell pair.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Qxx,Qyy,Qzz,Qxy,Qxz,Qyz
      VEC(REAL,3), IN :: origin
      MAT3(REAL,:,:,:), allocatable :: W
      if (NOT self%is_spherical OR self%l_max<2) then
         call make_quadrupole_ints_c_(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz,origin)
      else
         call create_(W,self%a%n_comp,self%b%n_comp,6)
         call make_quadrupole_ints_c_(self,W(:,:,1),W(:,:,2),W(:,:,3),W(:,:,4),W(:,:,5),W(:,:,6),origin)
         call change_to_spherical_(self,W(:,:,1),Qxx)
         call change_to_spherical_(self,W(:,:,2),Qyy)
         call change_to_spherical_(self,W(:,:,3),Qzz)
         call change_to_spherical_(self,W(:,:,4),Qxy)
         call change_to_spherical_(self,W(:,:,5),Qxz)
         call change_to_spherical_(self,W(:,:,6),Qyz)
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_quadrupole_ints_c(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz,origin)
   ! Make the quadrupole moment integral matrices "Qij"
   ! with gauge origin "origin"
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Qxx,Qyy,Qzz,Qxy,Qxz,Qyz
      VEC(REAL,3), IN :: origin
      MAT(REAL,:,:), allocatable :: QQxx,QQyy,QQzz,QQxy,QQxz,QQyz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b,n_a,n_b
      REAL :: cacb
      Qxx = ZERO; Qyy = ZERO; Qzz = ZERO
      Qxy = ZERO; Qxz = ZERO; Qyz = ZERO
      n_a = self%a%n_comp
      n_b = self%b%n_comp
      call create_(QQxx,n_a,n_b); call create_(QQyy,n_a,n_b); call create_(QQzz,n_a,n_b)
      call create_(QQxy,n_a,n_b); call create_(QQxz,n_a,n_b); call create_(QQyz,n_a,n_b)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_quadrupole_ints_(G,QQxx,QQyy,QQzz,QQxy,QQxz,QQyz,origin)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Qxx = Qxx+QQxx*cacb; Qyy = Qyy+QQyy*cacb; Qzz = Qzz+QQzz*cacb
            Qxy = Qxy+QQxy*cacb; Qxz = Qxz+QQxz*cacb; Qyz = Qyz+QQyz*cacb
         end do
      end do
      call destroy_(QQyz); call destroy_(QQxz); call destroy_(QQxy)
      call destroy_(QQzz); call destroy_(QQyy); call destroy_(QQxx)
   end subroutine

   PURE subroutine make_octupole_ints(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz,origin)
   ! As .make_octupole_ints_c, but spherical blocks for a spherical shell pair.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz
      VEC(REAL,3), IN :: origin
      MAT3(REAL,:,:,:), allocatable :: W
      if (NOT self%is_spherical OR self%l_max<2) then
         call make_octupole_ints_c_(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz,origin)
      else
         call create_(W,self%a%n_comp,self%b%n_comp,10)
         call make_octupole_ints_c_(self,W(:,:,1),W(:,:,2),W(:,:,3),W(:,:,4),W(:,:,5),W(:,:,6),W(:,:,7),W(:,:,8),W(:,:,9),W(:,:,10),origin)
         call change_to_spherical_(self,W(:,:,1),Oxxx)
         call change_to_spherical_(self,W(:,:,2),Oyyy)
         call change_to_spherical_(self,W(:,:,3),Ozzz)
         call change_to_spherical_(self,W(:,:,4),Oxxy)
         call change_to_spherical_(self,W(:,:,5),Oxxz)
         call change_to_spherical_(self,W(:,:,6),Oyyx)
         call change_to_spherical_(self,W(:,:,7),Oyyz)
         call change_to_spherical_(self,W(:,:,8),Ozzx)
         call change_to_spherical_(self,W(:,:,9),Ozzy)
         call change_to_spherical_(self,W(:,:,10),Oxyz)
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_octupole_ints_c(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz,origin)
   ! Make the octupole moment integral matrices "Oijk"
   ! with gauge origin "origin"
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz
      VEC(REAL,3), IN :: origin
      MAT(REAL,:,:), allocatable :: OOxxx,OOyyy,OOzzz,OOxxy,OOxxz,OOyyx,OOyyz,OOzzx,OOzzy,OOxyz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b,n_a,n_b
      REAL :: cacb
      Oxxx = ZERO; Oyyy = ZERO; Ozzz = ZERO
      Oxxy = ZERO; Oxxz = ZERO
      Oyyx = ZERO; Oyyz = ZERO
      Ozzx = ZERO; Ozzy = ZERO
      Oxyz = ZERO
      n_a = self%a%n_comp
      n_b = self%b%n_comp
      call create_(OOxxx,n_a,n_b); call create_(OOyyy,n_a,n_b); call create_(OOzzz,n_a,n_b)
      call create_(OOxxy,n_a,n_b); call create_(OOxxz,n_a,n_b)
      call create_(OOyyx,n_a,n_b); call create_(OOyyz,n_a,n_b)
      call create_(OOzzx,n_a,n_b); call create_(OOzzy,n_a,n_b)
      call create_(OOxyz,n_a,n_b)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_octupole_ints_(G,OOxxx,OOyyy,OOzzz,OOxxy,OOxxz,OOyyx,OOyyz,OOzzx,OOzzy,OOxyz,origin)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Oxxx = Oxxx+OOxxx*cacb; Oyyy = Oyyy+OOyyy*cacb; Ozzz = Ozzz+OOzzz*cacb
            Oxxy = Oxxy+OOxxy*cacb; Oxxz = Oxxz+OOxxz*cacb
            Oyyx = Oyyx+OOyyx*cacb; Oyyz = Oyyz+OOyyz*cacb
            Ozzx = Ozzx+OOzzx*cacb; Ozzy = Ozzy+OOzzy*cacb
            Oxyz = Oxyz+OOxyz*cacb
         end do
      end do
      call destroy_(OOxyz)
      call destroy_(OOzzy); call destroy_(OOzzx)
      call destroy_(OOyyz); call destroy_(OOyyx)
      call destroy_(OOxxz); call destroy_(OOxxy)
      call destroy_(OOzzz); call destroy_(OOyyy); call destroy_(OOxxx)
   end subroutine

   PURE subroutine make_nuclear_attraction_ints(self,N,c)
   ! Make the nuclear attraction integral matrix "N" for nucleus at position "c"
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: N
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: NN
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb
      N = ZERO
      call create_(NN,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_nuclear_attraction_ints_(G,NN,c)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            N = N+NN*cacb
         end do
      end do
      call destroy_(NN)
   end subroutine

   PURE subroutine make_E_field_ints(self,Ex,Ey,Ez,p)
   ! As .make_E_field_ints_c, but spherical blocks for a spherical shell pair.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ex,Ey,Ez
      VEC(REAL,3), IN :: p
      MAT3(REAL,:,:,:), allocatable :: W
      if (NOT self%is_spherical OR self%l_max<2) then
         call make_E_field_ints_c_(self,Ex,Ey,Ez,p)
      else
         call create_(W,self%a%n_comp,self%b%n_comp,3)
         call make_E_field_ints_c_(self,W(:,:,1),W(:,:,2),W(:,:,3),p)
         call change_to_spherical_(self,W(:,:,1),Ex)
         call change_to_spherical_(self,W(:,:,2),Ey)
         call change_to_spherical_(self,W(:,:,3),Ez)
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_E_field_ints_c(self,Ex,Ey,Ez,p)
   ! Make the electric field integral matrices "Ei" evaluated at
   ! the position "p"
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ex,Ey,Ez
      VEC(REAL,3), IN :: p
      MAT(REAL,:,:), allocatable :: EEx,EEy,EEz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b,n_a,n_b
      REAL :: cacb
      Ex = ZERO; Ey = ZERO; Ez = ZERO
      n_a = self%a%n_comp
      n_b = self%b%n_comp
      call create_(EEx,n_a,n_b)
      call create_(EEy,n_a,n_b)
      call create_(EEz,n_a,n_b)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_E_field_ints_(G,EEx,EEy,EEz,p)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Ex = Ex+EEx*cacb; Ey = Ey+EEy*cacb; Ez = Ez+EEz*cacb
         end do
      end do
      call destroy_(EEz)
      call destroy_(EEy)
      call destroy_(EEx)
   end subroutine

   PURE subroutine make_E_gradient_ints(self,Exx,Eyy,Ezz,Exy,Exz,Eyz,c)
   ! As .make_E_gradient_ints_c, but spherical blocks for a spherical shell pair.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Exx,Eyy,Ezz
      MAT(REAL,:,:), OUT :: Exy,Exz,Eyz
      VEC(REAL,:), IN :: c
      MAT3(REAL,:,:,:), allocatable :: W
      if (NOT self%is_spherical OR self%l_max<2) then
         call make_E_gradient_ints_c_(self,Exx,Eyy,Ezz,Exy,Exz,Eyz,c)
      else
         call create_(W,self%a%n_comp,self%b%n_comp,6)
         call make_E_gradient_ints_c_(self,W(:,:,1),W(:,:,2),W(:,:,3),W(:,:,4),W(:,:,5),W(:,:,6),c)
         call change_to_spherical_(self,W(:,:,1),Exx)
         call change_to_spherical_(self,W(:,:,2),Eyy)
         call change_to_spherical_(self,W(:,:,3),Ezz)
         call change_to_spherical_(self,W(:,:,4),Exy)
         call change_to_spherical_(self,W(:,:,5),Exz)
         call change_to_spherical_(self,W(:,:,6),Eyz)
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_E_gradient_ints_c(self,Exx,Eyy,Ezz,Exy,Exz,Eyz,c)
   ! Make the electric field gradient integral matrices "Eij" evaluated
   ! at the position "c".
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Exx,Eyy,Ezz
      MAT(REAL,:,:), OUT :: Exy,Exz,Eyz
      VEC(REAL,:), IN :: c
      MAT(REAL,:,:), allocatable :: EExx,EEyy,EEzz,EExy,EExz,EEyz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b,n_a,n_b
      REAL :: cacb
      Exx = ZERO; Eyy = ZERO; Ezz = ZERO
      Exy = ZERO; Exz = ZERO; Eyz = ZERO
      n_a = self%a%n_comp
      n_b = self%b%n_comp
      call create_(EExx,n_a,n_b); call create_(EEyy,n_a,n_b); call create_(EEzz,n_a,n_b)
      call create_(EExy,n_a,n_b); call create_(EExz,n_a,n_b); call create_(EEyz,n_a,n_b)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_E_gradient_ints_(G,EExx,EEyy,EEzz,EExy,EExz,EEyz,c)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Exx = Exx+EExx*cacb; Eyy = Eyy+EEyy*cacb; Ezz = Ezz+EEzz*cacb
            Exy = Exy+EExy*cacb; Exz = Exz+EExz*cacb; Eyz = Eyz+EEyz*cacb
         end do
      end do
      call destroy_(EEyz); call destroy_(EExz); call destroy_(EExy)
      call destroy_(EEzz); call destroy_(EEyy); call destroy_(EExx)
   end subroutine

   PURE subroutine make_spin_orbit_ints(self,Lx,Ly,Lz,c)
   ! As .make_spin_orbit_ints_c, but spherical blocks for a spherical shell pair.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Lx,Ly,Lz
      VEC(REAL,3), IN :: c
      MAT3(REAL,:,:,:), allocatable :: W
      if (NOT self%is_spherical OR self%l_max<2) then
         call make_spin_orbit_ints_c_(self,Lx,Ly,Lz,c)
      else
         call create_(W,self%a%n_comp,self%b%n_comp,3)
         call make_spin_orbit_ints_c_(self,W(:,:,1),W(:,:,2),W(:,:,3),c)
         call change_to_spherical_(self,W(:,:,1),Lx)
         call change_to_spherical_(self,W(:,:,2),Ly)
         call change_to_spherical_(self,W(:,:,3),Lz)
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_spin_orbit_ints_c(self,Lx,Ly,Lz,c)
   ! Make the spin orbit integral matrices "Lx" "Ly" and "Lz" for nucleus at
   ! position "c"
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Lx,Ly,Lz
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: LLx,LLy,LLz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb
      Lx = ZERO; Ly = ZERO; Lz = ZERO
      call create_(LLx,self%a%n_comp,self%b%n_comp)
      call create_(LLy,self%a%n_comp,self%b%n_comp)
      call create_(LLz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_spin_orbit_ints_(G,LLx,LLy,LLz,c)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Lx = Lx+LLx*cacb
            Ly = Ly+LLy*cacb
            Lz = Lz+LLz*cacb
         end do
      end do
      call destroy_(LLz); call destroy_(LLy); call destroy_(LLx)
   end subroutine

   PURE subroutine make_spin_orbit_B_ints(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz,c,origin)
   ! As .make_spin_orbit_B_ints_c, but spherical blocks for a spherical shell pair.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz
      VEC(REAL,3), IN :: c,origin
      MAT3(REAL,:,:,:), allocatable :: W
      if (NOT self%is_spherical OR self%l_max<2) then
         call make_spin_orbit_B_ints_c_(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz,c,origin)
      else
         call create_(W,self%a%n_comp,self%b%n_comp,9)
         call make_spin_orbit_B_ints_c_(self,W(:,:,1),W(:,:,2),W(:,:,3),W(:,:,4),W(:,:,5),W(:,:,6),W(:,:,7),W(:,:,8),W(:,:,9),c,origin)
         call change_to_spherical_(self,W(:,:,1),Qxx)
         call change_to_spherical_(self,W(:,:,2),Qxy)
         call change_to_spherical_(self,W(:,:,3),Qxz)
         call change_to_spherical_(self,W(:,:,4),Qyx)
         call change_to_spherical_(self,W(:,:,5),Qyy)
         call change_to_spherical_(self,W(:,:,6),Qyz)
         call change_to_spherical_(self,W(:,:,7),Qzx)
         call change_to_spherical_(self,W(:,:,8),Qzy)
         call change_to_spherical_(self,W(:,:,9),Qzz)
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_spin_orbit_B_ints_c(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz,c,origin)
   ! Make the gauge modified (B field) spin orbit integral matrices "Qij"
   ! for nucleus at position "c" and gauge origin "origin"
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz
      VEC(REAL,3), IN :: c,origin
      MAT(REAL,:,:), allocatable :: QQxx,QQxy,QQxz,QQyx,QQyy,QQyz,QQzx,QQzy,QQzz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b,n_a,n_b
      REAL :: cacb
      Qxx = ZERO; Qyx = ZERO; Qzx = ZERO
      Qxy = ZERO; Qyy = ZERO; Qzy = ZERO
      Qxz = ZERO; Qyz = ZERO; Qzz = ZERO
      n_a = self%a%n_comp
      n_b = self%b%n_comp
      call create_(QQxx,n_a,n_b); call create_(QQyx,n_a,n_b); call create_(QQzx,n_a,n_b)
      call create_(QQxy,n_a,n_b); call create_(QQyy,n_a,n_b); call create_(QQzy,n_a,n_b)
      call create_(QQxz,n_a,n_b); call create_(QQyz,n_a,n_b); call create_(QQzz,n_a,n_b)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_spin_orbit_B_ints_(G,QQxx,QQxy,QQxz,QQyx,QQyy,QQyz,QQzx,QQzy,QQzz,c,origin)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Qxx = Qxx+QQxx*cacb; Qxy = Qxy+QQxy*cacb; Qxz = Qxz+QQxz*cacb
            Qyx = Qyx+QQyx*cacb; Qyy = Qyy+QQyy*cacb; Qyz = Qyz+QQyz*cacb
            Qzx = Qzx+QQzx*cacb; Qzy = Qzy+QQzy*cacb; Qzz = Qzz+QQzz*cacb
         end do
      end do
      call destroy_(QQzz); call destroy_(QQyz); call destroy_(QQxz)
      call destroy_(QQzy); call destroy_(QQyy); call destroy_(QQxy)
      call destroy_(QQzx); call destroy_(QQyx); call destroy_(QQxx)
   end subroutine

   PURE subroutine make_L_ints(self,Lx,Ly,Lz,origin)
   ! As .make_L_ints_c, but spherical blocks for a spherical shell pair.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Lx,Ly,Lz
      VEC(REAL,3), IN :: origin
      MAT3(REAL,:,:,:), allocatable :: W
      if (NOT self%is_spherical OR self%l_max<2) then
         call make_L_ints_c_(self,Lx,Ly,Lz,origin)
      else
         call create_(W,self%a%n_comp,self%b%n_comp,3)
         call make_L_ints_c_(self,W(:,:,1),W(:,:,2),W(:,:,3),origin)
         call change_to_spherical_(self,W(:,:,1),Lx)
         call change_to_spherical_(self,W(:,:,2),Ly)
         call change_to_spherical_(self,W(:,:,3),Lz)
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_L_ints_c(self,Lx,Ly,Lz,origin)
   ! Calculate the angular momentum integral matrices "Lx", "Ly", "Lz",
   ! with "origin" as gauge origin, using Gauss-Hermite quadrature
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Lx,Ly,Lz
      VEC(REAL,3), IN :: origin
      MAT(REAL,:,:), allocatable :: LLx,LLy,LLz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb
      Lx = ZERO; Ly = ZERO; Lz = ZERO
      call create_(LLx,self%a%n_comp,self%b%n_comp)
      call create_(LLy,self%a%n_comp,self%b%n_comp)
      call create_(LLz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_L_ints_(G,LLx,LLy,LLz,origin)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Lx = Lx+LLx*cacb
            Ly = Ly+LLy*cacb
            Lz = Lz+LLz*cacb
         end do
      end do
      call destroy_(LLz); call destroy_(LLy); call destroy_(LLx)
   end subroutine

   PURE subroutine make_solenoidal_jp_ints(self,Jx,Jy,Jz,c)
   ! Make the solenoidal "Ji" integral matrices evaluated at position "c"
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Jx,Jy,Jz
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: Ix,Iy,Iz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb
      Jx = ZERO; Jy = ZERO; Jz = ZERO
      call create_(Ix,self%a%n_comp,self%b%n_comp)
      call create_(Iy,self%a%n_comp,self%b%n_comp)
      call create_(Iz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_solenoidal_jp_ints_(G,Ix,Iy,Iz,c)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Jx = Jx+Ix*cacb
            Jy = Jy+Iy*cacb
            Jz = Jz+Iz*cacb
         end do
      end do
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
   end subroutine

   PURE subroutine make_irrotational_jp_ints(self,Jx,Jy,Jz,c)
   ! Make the irrotational "Ji" integral matrices evaluated at position "c"
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Jx,Jy,Jz
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: Ix,Iy,Iz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb
      Jx = ZERO; Jy = ZERO; Jz = ZERO
      call create_(Ix,self%a%n_comp,self%b%n_comp)
      call create_(Iy,self%a%n_comp,self%b%n_comp)
      call create_(Iz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_irrotational_jp_ints_(G,Ix,Iy,Iz,c)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Jx = Jx+Ix*cacb
            Jy = Jy+Iy*cacb
            Jz = Jz+Iz*cacb
         end do
      end do
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
   end subroutine

   PURE subroutine make_magnetic_jp_ints(self,Jx,Jy,Jz,c)
   ! Make the magnetic "Ji" integral matrices evaluated at position "c"
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Jx,Jy,Jz
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: Ix,Iy,Iz
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb

      ! Initialize
      Jx = ZERO
      Jy = ZERO
      Jz = ZERO

      ! Allocate primitive integralsd
      call create_(Ix,self%a%n_comp,self%b%n_comp)
      call create_(Iy,self%a%n_comp,self%b%n_comp)
      call create_(Iz,self%a%n_comp,self%b%n_comp)

      ! The the pair (no exponents)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)

      ! Get the primitive integrals
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_magnetic_jp_ints_(G,Ix,Iy,Iz,c)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            Jx = Jx+Ix*cacb
            Jy = Jy+Iy*cacb
            Jz = Jz+Iz*cacb
         end do
      end do

      ! Clean
      call destroy_(Iz)
      call destroy_(Iy)
      call destroy_(Ix)
   end subroutine

   PURE subroutine make_magnetic_S_ints(self,M,c)
   ! Make the magnetic "M(:,:,i,j)" integral matrices evaluated at position "c"
      type(SHELL2_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: M
      VEC(REAL,3), IN :: c
      MAT4(REAL,:,:,:,:), allocatable :: I
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb
   ENSURE(size(M,3)==3,"SHELL2:make_magnetic_S_ints ... wrong shape for M")
   ENSURE(size(M,4)==3,"SHELL2:make_magnetic_S_ints ... wrong shape for M")

      ! Initialize
      M = ZERO

      ! Primitive integrals
      call create_(I,self%a%n_comp,self%b%n_comp,3,3)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_magnetic_S_ints_(G,I,c)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            M = M+I*cacb
         end do
      end do

      ! Clean
      call destroy_(I)
   end subroutine

   PURE subroutine make_nabla_field_ints_0(self,M,c)
   ! Make the nabla field "M(:,:,i,j)" integral matrices evaluated at
   ! position "c"
      type(SHELL2_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: M
      VEC(REAL,3), IN :: c
      MAT4(REAL,:,:,:,:), allocatable :: I
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb
   ENSURE(size(M,3)==3,"SHELL2:make_nabla_field_ints_0 ... wrong shape for M")
   ENSURE(size(M,4)==3,"SHELL2:make_nabla_field_ints_0 ... wrong shape for M")

      ! Initialize
      M = ZERO

      ! Primitive integrals
      call create_(I,self%a%n_comp,self%b%n_comp,3,3)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_nabla_field_ints_(G,I,c)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            M = M+I*cacb
         end do
      end do

      ! Clean
      call destroy_(I)
   end subroutine

   PURE subroutine make_nabla_field_ints_1(self,M,N,c)
   ! Make the nabla field "M(:,:,i,j)" integral matrices evaluated at position "c"
      type(SHELL2_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: M,N
      VEC(REAL,3), IN :: c
      MAT4(REAL,:,:,:,:), allocatable :: I,J
      type(GAUSSIAN2_TYPE) :: G
      INT :: a,b
      REAL :: cacb
   ENSURE(size(M,3)==3,"SHELL2:make_nabla_field_ints_1 ... wrong shape for M")
   ENSURE(size(M,4)==3,"SHELL2:make_nabla_field_ints_1 ... wrong shape for M")
   ENSURE(size(N,3)==3,"SHELL2:make_nabla_field_ints_1 ... wrong shape for N")
   ENSURE(size(N,4)==3,"SHELL2:make_nabla_field_ints_1 ... wrong shape for N")

      ! Initialize
      M = ZERO
      N = ZERO

      ! Primitive integrals
      call create_(I,self%a%n_comp,self%b%n_comp,3,3)
      call set_(G,self%a%l,self%a%position,ZERO,self%b%l,self%b%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%a%exponent(a),self%b%exponent(b))
            call make_nabla_field_ints_(G,I,c)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            M = M+I*cacb
         end do
      end do

      ! Clean
      call destroy_(I)
      call create_(J,self%b%n_comp,self%a%n_comp,3,3)
      call set_(G,self%b%l,self%b%position,ZERO,self%a%l,self%a%position,ZERO)
      do b = 1,self%b%n_cc
         do a = 1,self%a%n_cc
            call set_(G,self%b%exponent(b),self%a%exponent(a))
            call make_nabla_field_ints_(G,J,c)
            cacb = self%a%contraction(a)*self%b%contraction(b)
            N = N+J*cacb
         end do
      end do

      ! Clean
      call destroy_(J)
   end subroutine


!  ==============================
!  Roland Lindh's style integrals
!  ==============================

   PURE subroutine make_overlap(self,I)
   ! Calculate "I, a generic cartesian or spherical
   ! block of integrals.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: I
      MAT(REAL,:,:), allocatable :: W

      ! The following code is inherited from SHELL2
      if (NOT self%is_spherical) then
         call make_overlap_c_(self,I)
      else if (self%l_max<2) then
         call make_overlap_c_(self,I)
      else
         call create_(W,self%a%n_comp,self%b%n_comp)
         call make_overlap_c_(self,W)
         call change_to_spherical_(self,W,I)
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine make_overlap_c(self,ab)
   ! Calculate the overlap matrix for the two shells
   ! For cartesians only.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: ab
      VEC(REAL,:), allocatable :: es
      INT :: n
      n = n_comp_up_to_(self%a%l+self%b%l)-n_comp_up_to_(self%l_max-1)
      call create_(es,n)
      call make_overlap_es_(self,es)
      call transfer_(self,es,ab)
      call destroy_(es)
   end subroutine

   PURE subroutine make_overlap_es(self,es)
   ! Make the (es) overlap integrals, summed over primitives
      type(SHELL2_TYPE), IN :: self
      VEC(REAL,:), OUT :: es
      VEC(REAL,:), allocatable :: temp
      VEC(REAL,3) :: AB
      INT :: a,b,templb,tempub,i
      REAL :: r2ab,ea,eb
      tempub = n_comp_up_to_(self%l_sum)
      templb = n_comp_up_to_(self%l_max-1)+1
      call create_(temp,tempub)
      AB = self%a%position-self%b%position
      r2ab = dot_product(AB,AB)
      es = ZERO
      i = 0
      do b = 1,self%b%n_cc
         eb = self%b%exponent(b)
         do a = 1,self%a%n_cc
            i = i+1
            ea = self%a%exponent(a)
            call form_overlap_es_(self,temp,ea,eb,r2ab,i)
            es = es+self%a%contraction(a)*self%b%contraction(b)*temp(templb:tempub)
         end do
      end do
      call destroy_(temp)
   end subroutine

   PURE subroutine form_overlap_es(self,es,ea,eb,r2ab,i)
   ! Form the (es) overlap integrals for a pair of primitives
      type(SHELL2_TYPE), IN :: self
      VEC(REAL,:), INOUT :: es
      REAL, IN :: r2ab,ea,eb
      INT, IN :: i
      VEC(REAL,3) :: PE
      REAL :: half_zinv,zinv,zeta,zz,PI_zinv,PEi
      INT :: j,ne,z,lz,a1,a2
      zeta = self%exponent_sum(i)
      zinv = self%exponent_inv(i)
      zz = ea*eb/zeta
      PI_zinv = PI/zeta
      es = ZERO

      ! s||s
      es(1) = PI_zinv*sqrt(PI_zinv)*exp(-zz*r2ab)
      if (self%l_sum>0) then
         PE = self%center_diff(:,i)

         ! p||s
         es(2) = PE(1)*es(1)
         es(3) = PE(2)*es(1)
         es(4) = PE(3)*es(1)
         if (self%l_sum>1) then
            half_zinv = HALF*zinv

            ! d||s to .l_sum||s
            ne = n_comp_up_to_(self%l_sum)
            do j = 5,ne
               a1 = index_m1(j)
               z = first_nonzero(j)
               lz = pp(z,j)
               PEi = PE(z)
               if (lz<2) then
                  es(j) = PEi*es(a1)
               else
                  a2 = index_m1(a1)
                  es(j) = PEi*es(a1)+half_zinv*(lz-1)*es(a2)
               end if
            end do
         end if
      end if
   end subroutine

   PURE subroutine get_kei_0(self,kei)
   ! Calculate the kinetic energy and overlap (optional) integrals.
   ! (More efficient than making them separate).
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: kei
      MAT(REAL,:,:), allocatable :: Wt
      if (NOT self%is_spherical) then
         call make_kei_c_(self,kei)
      else if (self%l_max<2) then
         call make_kei_c_(self,kei)
      else
         call create_(Wt,self%a%n_comp,self%b%n_comp)
         call make_kei_c_(self,Wt)
         call change_to_spherical_(self,Wt,kei)
         call destroy_(Wt)
      end if
   end subroutine

   PURE subroutine get_kei_1(self,kei,overlap)
   ! Calculate the kinetic energy and overlap (optional) integrals.
   ! (More efficient than making them separate).
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: kei
      MAT(REAL,:,:), INOUT :: overlap
      MAT(REAL,:,:), allocatable :: Wt,Ws
      if (NOT self%is_spherical) then
         call make_kei_c_(self,kei,overlap)
      else if (self%l_max<2) then
         call make_kei_c_(self,kei,overlap)
      else
         call create_(Wt,self%a%n_comp,self%b%n_comp)
         call create_(Ws,self%a%n_comp,self%b%n_comp)
         call make_kei_c_(self,Wt,Ws)
         call change_to_spherical_(self,Wt,kei)
         call change_to_spherical_(self,Ws,overlap)
         call destroy_(Ws)
         call destroy_(Wt)
      end if
   end subroutine

   PURE subroutine make_kei_c(self,kei,overlap)
   ! Makes the kei and overlap matrics, summed over primitives
   ! Is called by kei, but does not do the orbital normalisation corrections
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: kei
      MAT(REAL,:,:), optional, INOUT :: overlap
      MAT(REAL,:,:), allocatable :: temp_kei,temp_overlap
      VEC(REAL,3) :: P,PA,PB,ara,brb,AB
      INT :: a,b,alb,aub,blb,bub
      REAL :: ca,ea,eb,zeta,zinv2,r2ab
      alb = n_comp_up_to_(self%a%l-1)+1; aub = alb+self%a%n_comp-1
      blb = n_comp_up_to_(self%b%l-1)+1; bub = blb+self%b%n_comp-1
      call create_(temp_kei,aub,bub)
      call create_(temp_overlap,aub,bub)
      kei = ZERO
      if (present(overlap)) overlap = ZERO
      AB = self%a%position-self%b%position
      r2ab = dot_product(AB,AB)
      do a = 1,self%a%n_cc
         ca = self%a%contraction(a)
         ea = self%a%exponent(a)
         ara = ea*self%a%position
         do b = 1,self%b%n_cc
            eb = self%b%exponent(b)
            brb = eb*self%b%position
            zeta = ea+eb
            zinv2 = HALF/zeta
            P = (ara+brb)/zeta
            PA = P-self%a%position
            PB = P-self%b%position
            if (self%a%l>self%b%l) then
               call form_overlap_mat_a_(self,temp_kei,temp_overlap,PA,PB,zinv2,zeta,ea,eb,r2ab)
            else
               call form_overlap_mat_b_(self,temp_kei,temp_overlap,PA,PB,zinv2,zeta,ea,eb,r2ab)
            end if
            if (present(overlap)) overlap = overlap+ca*self%b%contraction(b)*temp_overlap(alb:aub,blb:bub)
            kei = kei+ca*self%b%contraction(b)*temp_kei(alb:aub,blb:bub)
         end do
      end do
      call destroy_(temp_kei)
      call destroy_(temp_overlap)
   end subroutine

   PURE subroutine form_overlap_mat_a(self,kei,overlap,PA,PB,zinv2,zeta,ea,eb,r2ab)
   ! Makes the kinetic energy and overlap matrices for a primitive pair
   ! if .a.l > .b.l
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: overlap,kei
      VEC(REAL,3), IN :: PA,PB
      REAL, IN :: zinv2,zeta,ea,eb,r2ab
      VEC(INT,3) :: b_momenta
      INT :: j,z,lza,lzb,a,a1,a2,b,b1,laub,na,nb,b_n_comp_sum
      INT :: la,lb,la1_n_comp_sum,lb1_n_comp_sum,la_n_comp_sum,lb_n_comp_sum
      REAL :: PAi,PBi,PI_zinv,zz,zz2,zinv2_na,zinv2_nb,zz_ea,zz_eb,zz_ea_na
      zz = ea*eb/zeta
      zz2 = 2*zz
      zz_ea = zz/ea
      zz_eb = zz/eb
      b_n_comp_sum = n_comp_up_to_(self%b%l)
      PI_zinv = PI/zeta
      overlap(1,1) = PI_zinv*sqrt(PI_zinv)*exp(-zz*r2ab)  ! s||s
      kei(1,1) = zz*(3.0-zz2*r2ab)*overlap(1,1)
      do j = 2,b_n_comp_sum  ! s||p to s||lb
         z = first_nonzero(j)
         lza = nn(z,j)
         a1 = index_m1(j)
         PBi = PB(z)
         PAi = PA(z)
         if (lza<3) then
            overlap(1,j) = PBi*overlap(1,a1)
            kei(1,j) = PBi*kei(1,a1)+zz2*overlap(1,j)
            overlap(j,1) = PAi*overlap(a1,1)
            kei(j,1) = PAi*kei(a1,1)+zz2*overlap(j,1)
         else
            na = lza-2
            zinv2_na = zinv2*na
            a2 = index_m1(a1)
            overlap(1,j) = PBi*overlap(1,a1)+zinv2_na*overlap(1,a2)
            kei(1,j) = PBi*kei(1,a1)+zinv2_na*kei(1,a2)+zz2*overlap(1,j)-zz_eb*na*overlap(1,a2)
            overlap(j,1) = PAi*overlap(a1,1)+zinv2_na*overlap(a2,1)
            kei(j,1) = PAi*kei(a1,1)+zinv2_na*kei(a2,1)+zz2*overlap(j,1)-zz_ea*na*overlap(a2,1)
         end if
      end do
      do j = b_n_comp_sum+1,n_comp_up_to_(self%a%l-self%b%l)  ! lb||s to la-lb||s
         z = first_nonzero(j)
         lza = nn(z,j)
         a1 = index_m1(j)
         PAi = PA(z)
         if (lza<3) then
            overlap(j,1) = PAi*overlap(a1,1)
            kei(j,1) = PAi*kei(a1,1)+zz2*overlap(j,1)
         else
            na = lza-2
            zinv2_na = zinv2*na
            a2 = index_m1(a1)
            overlap(j,1) = PAi*overlap(a1,1)+zinv2_na*overlap(a2,1)
            kei(j,1) = PAi*kei(a1,1)+zinv2_na*kei(a2,1)+zz2*overlap(j,1)-zz_ea*na*overlap(a2,1)
         end if
      end do
      laub = self%a%l-self%b%l
      do lb = 1,self%b%l
         lb1_n_comp_sum = n_comp_up_to_(lb-1)+1
         lb_n_comp_sum = n_comp_up_to_(lb)
         laub = laub+1
         do la = 1,laub
            la1_n_comp_sum = n_comp_up_to_(la-1)+1
            la_n_comp_sum = n_comp_up_to_(la)
            do a = la1_n_comp_sum,la_n_comp_sum
               z = first_nonzero(a)
               lza = nn(z,a)
               a1 = index_m1(a)
               PAi = PA(z)
               if (lza==2) then
                  do b = lb1_n_comp_sum,lb_n_comp_sum
                     lzb = nn(z,b)
                     if (lzb==1) then
                        overlap(a,b) = PAi*overlap(a1,b)
                        kei(a,b) = PAi*kei(a1,b)+zz2*overlap(a,b)
                     else
                        b_momenta = nn(:,b)
                        b_momenta(z) = b_momenta(z)-1
                        b1 = index_of(b_momenta(1),b_momenta(2),b_momenta(3))
                        nb = lzb-1
                        zinv2_nb = zinv2*nb
                        overlap(a,b) = PAi*overlap(a1,b)+zinv2_nb*overlap(a1,b1)
                        kei(a,b) = PAi*kei(a1,b)+zinv2_nb*kei(a1,b1)+zz2*overlap(a,b)
                     end if
                  end do
               else
                  na = lza-2
                  zinv2_na = zinv2*na
                  zz_ea_na = zz_ea*na
                  a2 = index_m1(a1)
                  do b = lb1_n_comp_sum,lb_n_comp_sum
                     lzb = nn(z,b)
                     if (lzb==1) then
                        overlap(a,b) = PAi*overlap(a1,b)+zinv2_na*overlap(a2,b)
                        kei(a,b) = PAi*kei(a1,b)+zinv2_na*kei(a2,b)+zz2*overlap(a,b)-zz_ea_na*overlap(a2,b)
                     else
                        b_momenta = nn(:,b)
                        b_momenta(z) = b_momenta(z)-1
                        b1 = index_of(b_momenta(1),b_momenta(2),b_momenta(3))
                        nb = lzb-1
                        zinv2_nb = zinv2*nb
                        overlap(a,b) = PAi*overlap(a1,b)+zinv2_na*overlap(a2,b)+zinv2_nb*overlap(a1,b1)
                        kei(a,b) = PAi*kei(a1,b)+zinv2_na*kei(a2,b)+zinv2_nb*kei(a1,b1)+zz2*overlap(a,b)-zz_ea_na*overlap(a2,b)
                     end if
                  end do
               end if
            end do
         end do
      end do
   end subroutine

   PURE subroutine form_overlap_mat_b(self,kei,overlap,PA,PB,zinv2,zeta,ea,eb,r2ab)
   ! Makes the kinetic energy and overlap matrices for a primitive pair
   ! if .b.l > .a.l
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: overlap,kei
      VEC(REAL,3), IN :: PA,PB
      REAL, IN :: zinv2,zeta,ea,eb,r2ab
      VEC(INT,3) :: a_momenta
      INT :: j,z,lza,lzb,a,a1,b,b1,b2,lbub,la,lb,nb,na,a_n_comp_sum
      INT :: la1_n_comp_sum,lb1_n_comp_sum,la_n_comp_sum,lb_n_comp_sum
      REAL :: PAi,PBi,PI_zinv,zz,zz2,zinv2_na,zinv2_nb,zz_ea,zz_eb,zz_eb_nb
      zz = ea*eb/zeta
      zz2 = 2*zz
      zz_ea = zz/ea
      zz_eb = zz/eb
      a_n_comp_sum = n_comp_up_to_(self%a%l)
      PI_zinv = PI/zeta
      overlap(1,1) = PI_zinv*sqrt(PI_zinv)*exp(-zz*r2ab)  ! s||s
      kei(1,1) = zz*(3.0-zz2*r2ab)*overlap(1,1)
      do j = 2,a_n_comp_sum  ! p||s to la||s
         z = first_nonzero(j)
         lzb = nn(z,j)
         b1 = index_m1(j)
         PBi = PB(z)
         PAi = PA(z)
         if (lzb<3) then
            overlap(j,1) = PAi*overlap(b1,1)
            kei(j,1) = PAi*kei(b1,1)+zz2*overlap(j,1)
            overlap(1,j) = PBi*overlap(1,b1)
            kei(1,j) = PBi*kei(1,b1)+zz2*overlap(1,j)
         else
            nb = lzb-2
            zinv2_nb = zinv2*nb
            b2 = index_m1(b1)
            overlap(j,1) = PAi*overlap(b1,1)+zinv2_nb*overlap(b2,1)
            kei(j,1) = PAi*kei(b1,1)+zinv2_nb*kei(b2,1)+zz2*overlap(j,1)-zz_ea*nb*overlap(b2,1)
            overlap(1,j) = PBi*overlap(1,b1)+zinv2_nb*overlap(1,b2)
            kei(1,j) = PBi*kei(1,b1)+zinv2_nb*kei(1,b2)+zz2*overlap(1,j)-zz_eb*nb*overlap(1,b2)
         end if
      end do
      do j = a_n_comp_sum+1,n_comp_up_to_(self%b%l-self%a%l)  ! s||lb to s||lb-la
         z = first_nonzero(j)
         lzb = nn(z,j)
         b1 = index_m1(j)
         PBi = PB(z)
         if (lzb<3) then
            overlap(1,j) = PBi*overlap(1,b1)
            kei(1,j) = PBi*kei(1,b1)+zz2*overlap(1,j)
         else
            nb = lzb-2
            zinv2_nb = zinv2*nb
            b2 = index_m1(b1)
            overlap(1,j) = PBi*overlap(1,b1)+zinv2_nb*overlap(1,b2)
            kei(1,j) = PBi*kei(1,b1)+zinv2_nb*kei(1,b2)+zz2*overlap(1,j)-zz_eb*nb*overlap(1,b2)
         end if
      end do
      lbub = self%b%l-self%a%l
      do la = 1,self%a%l
         la1_n_comp_sum = n_comp_up_to_(la-1)+1
         la_n_comp_sum = n_comp_up_to_(la)
         lbub = lbub+1
         do lb = 1,lbub
            lb1_n_comp_sum = n_comp_up_to_(lb-1)+1
            lb_n_comp_sum = n_comp_up_to_(lb)
            do b = lb1_n_comp_sum,lb_n_comp_sum
               z = first_nonzero(b)
               lzb = nn(z,b)
               b1 = index_m1(b)
               PBi = PB(z)
               if (lzb==2) then
                  do a = la1_n_comp_sum,la_n_comp_sum
                     lza = nn(z,a)
                     if (lza==1) then
                        overlap(a,b) = PBi*overlap(a,b1)
                        kei(a,b) = PBi*kei(a,b1)+zz2*overlap(a,b)
                     else
                        a_momenta = nn(:,a)
                        a_momenta(z) = a_momenta(z)-1
                        a1 = index_of(a_momenta(1),a_momenta(2),a_momenta(3))
                        na = lza-1
                        zinv2_na = zinv2*na
                        overlap(a,b) = PBi*overlap(a,b1)+zinv2_na*overlap(a1,b1)
                        kei(a,b) = PBi*kei(a,b1)+zinv2_na*kei(a1,b1)+zz2*overlap(a,b)
                     end if
                  end do
               else
                  nb = lzb-2
                  zinv2_nb = zinv2*nb
                  zz_eb_nb = zz_eb*nb
                  b2 = index_m1(b1)
                  do a = la1_n_comp_sum,la_n_comp_sum
                     lza = nn(z,a)
                     if (lza==1) then
                        overlap(a,b) = PBi*overlap(a,b1)+zinv2_nb*overlap(a,b2)
                        kei(a,b) = PBi*kei(a,b1)+zinv2_nb*kei(a,b2)+zz2*overlap(a,b)-zz_eb_nb*overlap(a,b2)
                     else
                        a_momenta = nn(:,a)
                        a_momenta(z) = a_momenta(z)-1
                        a1 = index_of(a_momenta(1),a_momenta(2),a_momenta(3))
                        na = lza-1
                        zinv2_na = zinv2*na
                        overlap(a,b) = PBi*overlap(a,b1)+zinv2_nb*overlap(a,b2)+zinv2_na*overlap(a1,b1)
                        kei(a,b) = PBi*kei(a,b1)+zinv2_nb*kei(a,b2)+zinv2_na*kei(a1,b1)+zz2*overlap(a,b)-zz_eb_nb*overlap(a,b2)
                     end if
                  end do
               end if
            end do
         end do
      end do
   end subroutine

   PURE subroutine get_nuc(self,ab,mass_c,pos_c)
   ! Calculate ab, which is either  a cartesian or a spherical
   ! block of integrals.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: ab
      REAL, IN :: mass_c
      VEC(REAL,3), IN :: pos_c
      MAT(REAL,:,:), allocatable :: W
      if (NOT self%is_spherical) then
         call get_nuc_c_(self,ab,mass_c,pos_c)
      else if (self%l_max<2) then
         call get_nuc_c_(self,ab,mass_c,pos_c)
      else
         call create_(W,self%a%n_comp,self%b%n_comp)
         call get_nuc_c_(self,W,mass_c,pos_c)
         call change_to_spherical_(self,W,ab)
         call destroy_(W)
      end if
   end subroutine

   PURE subroutine get_nuc_c(self,ab,mass_c,pos_c)
   ! Calculate the nuclear attraction matrix "ab" for the two shells
   ! with the nucleus at "pos_c".
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: ab
      REAL, IN :: mass_c
      VEC(REAL,3), IN :: pos_c
      VEC(REAL,:), allocatable :: es
      REAL :: ss
      INT :: n
      if (self%l_sum==0) then

         ! ss
         call make_nuc_ss_(self,ss,mass_c,pos_c)
         ab(1,1) = ss
      else if (self%l_sum==1) then

         ! ps or sp
         call make_nuc_ps_(self,ab,mass_c,pos_c)
      else if (self%l_max==1) then

         ! pp
         call make_nuc_pp_(self,ab,mass_c,pos_c)
      else if (self%l_sum==2 AND self%l_max==2) then

         ! ds or sd
         call make_nuc_ds_(self,ab,mass_c,pos_c)
      else if (self%l_sum==3 AND self%l_max==1) then

         ! dp or pd
         call make_nuc_dp_(self,ab,mass_c,pos_c)
      else

         ! General case
         n = n_comp_up_to_(self%l_sum)-n_comp_up_to_(self%l_max-1)
         call create_(es,n)
         call make_nuc_es_(self,es,mass_c,pos_c)
         call transfer_(self,es,ab)
         call destroy_(es)
      end if
   end subroutine

   PURE subroutine make_nuc_es(self,es,mass_c,pos_c)
   ! Make the (es) nuclear attraction integrals, summed over primitives
   ! Numbers may be slightly different to cadpac due to a relativistic
   ! correction term.
      type(SHELL2_TYPE), IN :: self
      VEC(REAL,:), OUT :: es
      REAL, IN :: mass_c
      VEC(REAL,3), IN :: pos_c
      MAT(REAL,:,:), allocatable, target :: Ixa,Iya,Iza
      VEC(REAL,:), PTR :: Ix,Iy,Iz
      type(RYS_TYPE), allocatable :: rys
      REAL :: rzt,ce,e1_ce,rho_zinv,half_zinv,Ix2,Iy2,Iz2,wt
      REAL :: QPx,QPy,QPz,PAx,PAy,PAz,zeta,zinv,rho,xx,eta_c,two_pi
      REAL :: Ixe,Iye,Ize,Ixe1,Iye1,Ize1,Ixep1,Iyep1,Izep1
      INT :: ag,bg,e,ep1,n_roots,i,j,n,n_sum
      if (mass_c<TOL(15)) then; eta_c = 1.0d30  ! Very big.
      else; eta_c = 3880000000d0*(mass_c**(-TWOTHIRD))
      end if
      eta_c = 1.0d60  ! uncomment this to get rid of relativistic correction.
      n_roots = (self%l_sum+2)/2
      n_sum = n_roots*self%a%n_cc*self%b%n_cc
      call create_(rys,n_roots)
      call create_(Ixa,n_sum,self%l_sum+1)
      call create_(Iya,n_sum,self%l_sum+1)
      call create_(Iza,n_sum,self%l_sum+1)
      two_pi = TWO*PI
      i = 0
      j = 0
      do bg = 1,self%b%n_cc
         do ag = 1,self%a%n_cc
            i = i+1
            zeta = self%exponent_sum(i)
            zinv = self%exponent_inv(i)
            rho = zeta*eta_c/(zeta+eta_c)
            PAx = self%center_diff(1,i)
            PAy = self%center_diff(2,i)
            PAz = self%center_diff(3,i)
            QPx = pos_c(1)-self%pair_center(1,i)
            QPy = pos_c(2)-self%pair_center(2,i)
            QPz = pos_c(3)-self%pair_center(3,i)
            xx = rho*(QPx*QPx+QPy*QPy+QPz*QPz)
            call get_weights_(rys,xx)
            rys%weight(1:rys%n_roots) = rys%weight(1:rys%n_roots)*(two_pi*self%contraction_prefactor(i)*sqrt(rho))
            half_zinv = HALF*zinv
            rho_zinv = rho*zinv
            do n = 1,n_roots
               j = j+1
               Ix => Ixa(j,:)
               Iy => Iya(j,:)
               Iz => Iza(j,:)
               wt = rys%weight(n)
               Ix(1) = ONE
               Iy(1) = ONE
               Iz(1) = wt
               if (self%l_sum>0) then
                  rzt = rys%root(n)*rho_zinv
                  Ix2 = PAx+rzt*QPx
                  Iy2 = PAy+rzt*QPy
                  Iz2 = PAz+rzt*QPz
                  Ix(2) = Ix2; Iy(2) = Iy2; Iz(2) = Iz2*wt
                  if (self%l_sum>1) then
                     ce = (ONE-rzt)*half_zinv
                     Ixep1 = Ix2*Ix2+ce
                     Iyep1 = Iy2*Iy2+ce
                     Izep1 = Iz2*Iz2+ce
                     Ix(3) = Ixep1
                     Iy(3) = Iyep1
                     Iz(3) = Izep1*wt
                     if (self%l_sum>2) then
                        Ixe1 = Ix2; Iye1 = Iy2; Ize1 = Iz2
                        Ixe = Ixep1; Iye = Iyep1; Ize = Izep1
                        e1_ce = ce
                        do e = 3,self%l_sum
                           ep1 = e+1
                  ! e1_ce = (e-1)*ce
                           e1_ce = e1_ce+ce
                           Ixep1 = Ix2*Ixe+e1_ce*Ixe1
                           Iyep1 = Iy2*Iye+e1_ce*Iye1
                           Izep1 = Iz2*Ize+e1_ce*Ize1
                           Ix(ep1) = Ixep1; Iy(ep1) = Iyep1; Iz(ep1) = Izep1*wt
                           Ixe1 = Ixe; Iye1 = Iye; Ize1 = Ize
                           Ixe = Ixep1; Iye = Iyep1; Ize = Izep1
                        end do
                     end if
                  end if
               end if
            end do
         end do
      end do
      call combine_nuc_2d_ints_(self,es,Ixa,Iya,Iza,Iza(:,1))
      call destroy_(Iza)
      call destroy_(Iya)
      call destroy_(Ixa)
      call destroy_(rys)
   end subroutine

   PURE subroutine combine_nuc_2d_ints(self,es,Ix,Iy,Iz,wt)
   ! Sum the Ix, Iy, and Iz 2d-integrals to get the (e|s) integrals.
      type(SHELL2_TYPE), IN :: self
      VEC(REAL,:), INOUT :: es
      MAT(REAL,:,:), IN :: Ix,Iy,Iz
      VEC(REAL,:), IN :: wt
      VEC(REAL,:), allocatable :: Ixy
      VEC(INT,:), allocatable :: ex,ey,ez
      VEC(INT,:), allocatable :: exyz0,eyz0,exz0,exy0,ez0,ey0,ex0,en0,erms
      VEC(type(EVEC_INT_TYPE),:), allocatable :: exy_rms
      type(RMS_INDICES_TYPE), allocatable, target :: er
      INT :: ee,em,e,xe,ye,ze,f,l
      f = self%first_gaussian
      l = self%last_gaussian
      ex = nx(f:l)
      ey = ny(f:l)
      ez = nz(f:l)
      er = rms_indices(self%l_max,self%l_min)
      exyz0 = er%xyz0
      eyz0 = er%yz0; exz0 = er%xz0; exy0 = er%xy0
      ez0 = er%z0; ey0 = er%y0; ex0 = er%x0
      en0 = er%n0
      do ee = 1,size(exyz0)
         e = exyz0(ee)
         es(e) = sum(wt)
      end do
      do ee = 1,size(eyz0)
         e = eyz0(ee)
         xe = ex(e)
         es(e) = sum(Ix(:,xe)*wt)
      end do
      do ee = 1,size(exz0)
         e = exz0(ee)
         ye = ey(e)
         es(e) = sum(Iy(:,ye)*wt)
      end do
      do ee = 1,size(exy0)
         e = exy0(ee)
         ze = ez(e)
         es(e) = sum(Iz(:,ze))
      end do
      do ee = 1,size(ez0)
         e = ez0(ee)
         xe = ex(e)
         ye = ey(e)
         es(e) = sum(Ix(:,xe)*Iy(:,ye)*wt)
      end do
      do ee = 1,size(ey0)
         e = ey0(ee)
         xe = ex(e)
         ze = ez(e)
         es(e) = sum(Ix(:,xe)*Iz(:,ze))
      end do
      do ee = 1,size(ex0)
         e = ex0(ee)
         ye = ey(e)
         ze = ez(e)
         es(e) = sum(Iy(:,ye)*Iz(:,ze))
      end do
      exy_rms = er%xy_rms_n
      if (size(exy_rms)==size(en0)) then
         do ee = 1,size(en0)
            e = en0(ee)
            xe = ex(e); ye = ey(e); ze = ez(e)
            es(e) = sum(Ix(:,xe)*Iy(:,ye)*Iz(:,ze))
         end do
      else if (size(exy_rms)>0) then

         ! Apply (e) RMS here
         call create_(Ixy,size(Ix,1))
         do ee = 1,size(exy_rms)
            erms = exy_rms(ee)%element
            e = en0(erms(1))
            xe = ex(e)
            ye = ey(e)
            ze = ez(e)
            Ixy = Ix(:,xe)*Iy(:,ye)
            es(e) = sum(Ixy*Iz(:,ze))
            do em = 2,size(erms)
               e = en0(erms(em))
               ze = ez(e)
               es(e) = sum(Ixy*Iz(:,ze))
            end do
         end do
         call destroy_(Ixy)
      end if
   end subroutine

   PURE subroutine make_nuc_dp(self,dp,mass_c,pos_c)
   ! Make the (dp) nuclear attraction integrals, summed over primitives
   ! Numbers may be slightly different to cadpac due to a relativistic
   ! correction term.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: dp
      REAL, IN :: mass_c
      VEC(REAL,3), IN :: pos_c
      type(RYS_TYPE), allocatable :: rys
      REAL :: rzt,ce,rho_zinv,half_zinv,wt
      REAL :: QPx,QPy,QPz,PAx,PAy,PAz,zeta,zinv,rho,xx,eta_c,two_pi
      REAL :: Ix2,Iy2,Iz2,Ix3,Iy3,Iz3,two_ce,ABx,ABy,ABz
      REAL :: dxx_s,dyy_s,dzz_s,dxy_s,dxz_s,dyz_s,fxxx_s,fyyy_s,fzzz_s
      REAL :: fxxy_s,fxxz_s,fxyy_s,fyyz_s,fxzz_s,fyzz_s,fxyz_s
      INT :: ag,bg,i,j,n
      if (mass_c<TOL(15)) then; eta_c = 1.0d30  ! Very big.
      else; eta_c = 3880000000d0*(mass_c**(-TWOTHIRD))
      end if
      eta_c = 1.0d60  ! uncomment this to get rid of relativistic correction.
      call create_(rys,2)
      two_pi = TWO*PI
      i = 0
      j = 0
      dxx_s = ZERO; dyy_s = ZERO; dzz_s = ZERO
      dxy_s = ZERO; dxz_s = ZERO; dyz_s = ZERO
      fxxx_s = ZERO; fyyy_s = ZERO; fzzz_s = ZERO
      fxxy_s = ZERO; fxxz_s = ZERO; fxyy_s = ZERO
      fyyz_s = ZERO; fxzz_s = ZERO; fyzz_s = ZERO
      fxyz_s = ZERO
      do bg = 1,self%b%n_cc
         do ag = 1,self%a%n_cc
            i = i+1
            zeta = self%exponent_sum(i)
            zinv = self%exponent_inv(i)
            rho = zeta*eta_c/(zeta+eta_c)
            PAx = self%center_diff(1,i)
            PAy = self%center_diff(2,i)
            PAz = self%center_diff(3,i)
            QPx = pos_c(1)-self%pair_center(1,i)
            QPy = pos_c(2)-self%pair_center(2,i)
            QPz = pos_c(3)-self%pair_center(3,i)
            xx = rho*(QPx*QPx+QPy*QPy+QPz*QPz)
            call get_weights_(rys,xx)
            rys%weight(1:rys%n_roots) = rys%weight(1:rys%n_roots)*(two_pi*self%contraction_prefactor(i)*sqrt(rho))
            half_zinv = HALF*zinv
            rho_zinv = rho*zinv
            do n = 1,2
               j = j+1
               wt = rys%weight(n)
               rzt = rys%root(n)*rho_zinv
               ce = (ONE-rzt)*half_zinv
               Ix2 = PAx+rzt*QPx
               Iy2 = PAy+rzt*QPy
               Iz2 = PAz+rzt*QPz
               Ix3 = Ix2*Ix2+ce
               Iy3 = Iy2*Iy2+ce
               Iz3 = (Iz2*Iz2+ce)*wt
               Iz2 = Iz2*wt
               two_ce = ce+ce
               dxx_s = dxx_s+Ix3*wt
               dyy_s = dyy_s+Iy3*wt
               dzz_s = dzz_s+Iz3
               dxy_s = dxy_s+Ix2*Iy2*wt
               dxz_s = dxz_s+Ix2*Iz2
               dyz_s = dyz_s+Iy2*Iz2
               fxxx_s = fxxx_s+Ix2*(Ix3+two_ce)*wt
               fyyy_s = fyyy_s+Iy2*(Iy3+two_ce)*wt
               fzzz_s = fzzz_s+Iz2*(Iz3+two_ce)*wt
               fxxy_s = fxxy_s+Ix3*Iy2*wt
               fxxz_s = fxxz_s+Ix3*Iz2
               fxyy_s = fxyy_s+Ix2*Iy3*wt
               fyyz_s = fyyz_s+Iy3*Iz2
               fxzz_s = fxzz_s+Ix3*Iz3
               fyzz_s = fyzz_s+Iy2*Iz3
               fxyz_s = fxyz_s+Ix2*Iy2*Iz2
            end do
         end do
      end do
      if (self%same_center) then
         if (self%a%l==2) then
            dp(1,1) = fxxx_s
            dp(2,1) = fxyy_s
            dp(3,1) = fxzz_s
            dp(4,1) = fxxy_s
            dp(5,1) = fxxz_s
            dp(6,1) = fxyz_s
            dp(1,2) = fxxy_s
            dp(2,2) = fyyy_s
            dp(3,2) = fyzz_s
            dp(4,2) = fxyy_s
            dp(5,2) = fxyz_s
            dp(6,2) = fyyz_s
            dp(1,3) = fxxz_s
            dp(2,3) = fyyz_s
            dp(3,3) = fzzz_s
            dp(4,3) = fxyz_s
            dp(5,3) = fxzz_s
            dp(6,3) = fyzz_s
         else
            dp(1,1) = fxxx_s
            dp(1,2) = fxyy_s
            dp(1,3) = fxzz_s
            dp(1,4) = fxxy_s
            dp(1,5) = fxxz_s
            dp(1,6) = fxyz_s
            dp(2,1) = fxxy_s
            dp(2,2) = fyyy_s
            dp(2,3) = fyzz_s
            dp(2,4) = fxyy_s
            dp(2,5) = fxyz_s
            dp(2,6) = fyyz_s
            dp(3,1) = fxxz_s
            dp(3,2) = fyyz_s
            dp(3,3) = fzzz_s
            dp(3,4) = fxyz_s
            dp(3,5) = fxzz_s
            dp(3,6) = fyzz_s
         end if
      else
         if (self%a%l==2) then
            ABx = self%a%position(1)-self%b%position(1)
            ABy = self%a%position(2)-self%b%position(2)
            ABz = self%a%position(3)-self%b%position(3)
            dp(1,1) = fxxx_s+ABx*dxx_s
            dp(2,1) = fxyy_s+ABx*dyy_s
            dp(3,1) = fxzz_s+ABx*dzz_s
            dp(4,1) = fxxy_s+ABx*dxy_s
            dp(5,1) = fxxz_s+ABx*dxz_s
            dp(6,1) = fxyz_s+ABx*dyz_s
            dp(1,2) = fxxy_s+ABy*dxx_s
            dp(2,2) = fyyy_s+ABy*dyy_s
            dp(3,2) = fyzz_s+ABy*dzz_s
            dp(4,2) = fxyy_s+ABy*dxy_s
            dp(5,2) = fxyz_s+ABy*dxz_s
            dp(6,2) = fyyz_s+ABy*dyz_s
            dp(1,3) = fxxz_s+ABz*dxx_s
            dp(2,3) = fyyz_s+ABz*dyy_s
            dp(3,3) = fzzz_s+ABz*dzz_s
            dp(4,3) = fxyz_s+ABz*dxy_s
            dp(5,3) = fxzz_s+ABz*dxz_s
            dp(6,3) = fyzz_s+ABz*dyz_s
         else
            ABx = self%b%position(1)-self%a%position(1)
            ABy = self%b%position(2)-self%a%position(2)
            ABz = self%b%position(3)-self%a%position(3)
            dp(1,1) = fxxx_s+ABx*dxx_s
            dp(1,2) = fxyy_s+ABx*dyy_s
            dp(1,3) = fxzz_s+ABx*dzz_s
            dp(1,4) = fxxy_s+ABx*dxy_s
            dp(1,5) = fxxz_s+ABx*dxz_s
            dp(1,6) = fxyz_s+ABx*dyz_s
            dp(2,1) = fxxy_s+ABy*dxx_s
            dp(2,2) = fyyy_s+ABy*dyy_s
            dp(2,3) = fyzz_s+ABy*dzz_s
            dp(2,4) = fxyy_s+ABy*dxy_s
            dp(2,5) = fxyz_s+ABy*dxz_s
            dp(2,6) = fyyz_s+ABy*dyz_s
            dp(3,1) = fxxz_s+ABz*dxx_s
            dp(3,2) = fyyz_s+ABz*dyy_s
            dp(3,3) = fzzz_s+ABz*dzz_s
            dp(3,4) = fxyz_s+ABz*dxy_s
            dp(3,5) = fxzz_s+ABz*dxz_s
            dp(3,6) = fyzz_s+ABz*dyz_s
         end if
      end if
      call destroy_(rys)
   end subroutine

   PURE subroutine make_nuc_ds(self,ds,mass_c,pos_c)
   ! Make the (ds) nuclear attraction integrals, summed over primitives
   ! Numbers may be slightly different to cadpac due to a relativistic
   ! correction term.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: ds
      REAL, IN :: mass_c
      VEC(REAL,3), IN :: pos_c
      type(RYS_TYPE), allocatable :: rys
      REAL :: rzt,ce,rho_zinv,half_zinv,Ix2,Iy2,Iz2,wt,Iz2_wt
      REAL :: QPx,QPy,QPz,PAx,PAy,PAz,zeta,zinv,rho,xx,eta_c,two_pi
      REAL :: dxx_s,dyy_s,dzz_s,dxy_s,dxz_s,dyz_s
      INT :: ag,bg,i,n
      if (mass_c<TOL(15)) then; eta_c = 1.0d30  ! Very big.
      else; eta_c = 3880000000d0*(mass_c**(-TWOTHIRD))
      end if
      eta_c = 1.0d60  ! uncomment this to get rid of relativistic correction.
      call create_(rys,2)
      two_pi = TWO*PI
      dxx_s = ZERO; dyy_s = ZERO; dzz_s = ZERO
      dxy_s = ZERO; dxz_s = ZERO; dyz_s = ZERO
      i = 0
      do bg = 1,self%b%n_cc
         do ag = 1,self%a%n_cc
            i = i+1
            zeta = self%exponent_sum(i)
            zinv = self%exponent_inv(i)
            rho = zeta*eta_c/(zeta+eta_c)
            PAx = self%center_diff(1,i)
            PAy = self%center_diff(2,i)
            PAz = self%center_diff(3,i)
            QPx = pos_c(1)-self%pair_center(1,i)
            QPy = pos_c(2)-self%pair_center(2,i)
            QPz = pos_c(3)-self%pair_center(3,i)
            xx = rho*(QPx*QPx+QPy*QPy+QPz*QPz)
            call get_weights_(rys,xx)
            rys%weight(1:rys%n_roots) = rys%weight(1:rys%n_roots)*(two_pi*self%contraction_prefactor(i)*sqrt(rho))
            half_zinv = HALF*zinv
            rho_zinv = rho*zinv
            do n = 1,2
               wt = rys%weight(n)
               rzt = rys%root(n)*rho_zinv
               Ix2 = PAx+rzt*QPx
               Iy2 = PAy+rzt*QPy
               Iz2 = PAz+rzt*QPz
               ce = (ONE-rzt)*half_zinv
               Iz2_wt = Iz2*wt
               dxx_s = dxx_s+(Ix2*Ix2+ce)*wt
               dyy_s = dyy_s+(Iy2*Iy2+ce)*wt
               dzz_s = dzz_s+(Iz2*Iz2+ce)*wt
               dxy_s = dxy_s+Ix2*Iy2*wt
               dxz_s = dxz_s+Ix2*Iz2_wt
               dyz_s = dyz_s+Iy2*Iz2_wt
            end do
         end do
      end do
      call destroy_(rys)
      if (self%a%l==2) then
         ds(1,1) = dxx_s
         ds(2,1) = dyy_s
         ds(3,1) = dzz_s
         ds(4,1) = dxy_s
         ds(5,1) = dxz_s
         ds(6,1) = dyz_s
      else
         ds(1,1) = dxx_s
         ds(1,2) = dyy_s
         ds(1,3) = dzz_s
         ds(1,4) = dxy_s
         ds(1,5) = dxz_s
         ds(1,6) = dyz_s
      end if
   end subroutine

   PURE subroutine make_nuc_pp(self,pp,mass_c,pos_c)
   ! Make the (es) nuclear attraction integrals, summed over primitives
   ! Numbers may be slightly different to cadpac due to a relativistic
   ! correction term.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: pp
      REAL, IN :: mass_c
      VEC(REAL,3), IN :: pos_c
      type(RYS_TYPE), allocatable :: rys
      REAL :: rzt,ce,rho_zinv,half_zinv,Ix2,Iy2,Iz2,wt,Iy2_wt,Iz2_wt
      REAL :: QPx,QPy,QPz,PAx,PAy,PAz,zeta,zinv,rho,xx,eta_c,two_pi
      REAL :: BAx,BAy,BAz,px_s,py_s,pz_s,dxx_s,dyy_s,dzz_s,dxy_s,dxz_s,dyz_s
      INT :: ag,bg,i,n
      if (mass_c<TOL(15)) then; eta_c = 1.0d30  ! Very big.
      else; eta_c = 3880000000d0*(mass_c**(-TWOTHIRD))
      end if
      eta_c = 1.0d60  ! uncomment this to get rid of relativistic correction.
      call create_(rys,2)
      two_pi = TWO*PI
      i = 0
      px_s = ZERO; py_s = ZERO; pz_s = ZERO
      dxx_s = ZERO; dyy_s = ZERO; dzz_s = ZERO
      dxy_s = ZERO; dxz_s = ZERO; dyz_s = ZERO
      do bg = 1,self%b%n_cc
         do ag = 1,self%a%n_cc
            i = i+1
            zeta = self%exponent_sum(i)
            zinv = self%exponent_inv(i)
            rho = zeta*eta_c/(zeta+eta_c)
            PAx = self%center_diff(1,i)
            PAy = self%center_diff(2,i)
            PAz = self%center_diff(3,i)
            QPx = pos_c(1)-self%pair_center(1,i)
            QPy = pos_c(2)-self%pair_center(2,i)
            QPz = pos_c(3)-self%pair_center(3,i)
            xx = rho*(QPx*QPx+QPy*QPy+QPz*QPz)
            call get_weights_(rys,xx)
            rys%weight(1:rys%n_roots) = rys%weight(1:rys%n_roots)*(two_pi*self%contraction_prefactor(i)*sqrt(rho))
            half_zinv = HALF*zinv
            rho_zinv = rho*zinv
            do n = 1,2
               wt = rys%weight(n)
               rzt = rys%root(n)*rho_zinv
               Ix2 = PAx+rzt*QPx
               Iy2 = PAy+rzt*QPy
               Iz2 = PAz+rzt*QPz
               ce = (ONE-rzt)*half_zinv
               Iz2_wt = Iz2*wt
               Iy2_wt = Iy2*wt
               px_s = px_s+Ix2*wt
               py_s = py_s+Iy2_wt
               pz_s = pz_s+Iz2_wt
               dxx_s = dxx_s+(Ix2*Ix2+ce)*wt
               dyy_s = dyy_s+(Iy2*Iy2+ce)*wt
               dzz_s = dzz_s+(Iz2*Iz2+ce)*wt
               dxy_s = dxy_s+Ix2*Iy2_wt
               dxz_s = dxz_s+Ix2*Iz2_wt
               dyz_s = dyz_s+Iy2*Iz2_wt
            end do
         end do
      end do
      call destroy_(rys)
      if (self%same_center) then
         pp(1,1) = dxx_s
         pp(1,2) = dxy_s
         pp(1,3) = dxz_s
         pp(2,1) = dxy_s
         pp(2,2) = dyy_s
         pp(2,3) = dyz_s
         pp(3,1) = dxz_s
         pp(3,2) = dyz_s
         pp(3,3) = dzz_s
      else
         BAx = self%b%position(1)-self%a%position(1)
         BAy = self%b%position(2)-self%a%position(2)
         BAz = self%b%position(3)-self%a%position(3)
         pp(1,1) = dxx_s+BAx*px_s
         pp(1,2) = dxy_s+BAx*py_s
         pp(1,3) = dxz_s+BAx*pz_s
         pp(2,1) = dxy_s+BAy*px_s
         pp(2,2) = dyy_s+BAy*py_s
         pp(2,3) = dyz_s+BAy*pz_s
         pp(3,1) = dxz_s+BAz*px_s
         pp(3,2) = dyz_s+BAz*py_s
         pp(3,3) = dzz_s+BAz*pz_s
      end if
   end subroutine

   PURE subroutine make_nuc_ps(self,ps,mass_c,pos_c)
   ! Make the (ps) nuclear attraction integrals, summed over primitives
   ! Numbers may be slightly different to cadpac due to a relativistic
   ! correction term.
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: ps
      REAL, IN :: mass_c
      VEC(REAL,3), IN :: pos_c
      type(RYS_TYPE), allocatable :: rys
      REAL :: rzt,wt
      REAL :: QPx,QPy,QPz,PAx,PAy,PAz,zeta,rho,xx,eta_c,two_pi,ps_x,ps_y,ps_z
      INT :: ag,bg,i
      if (mass_c<TOL(15)) then; eta_c = 1.0d30  ! Very big.
      else; eta_c = 3880000000d0*(mass_c**(-TWOTHIRD))
      end if
      eta_c = 1.0d60  ! uncomment this to get rid of relativistic correction.
      call create_(rys,1)
      two_pi = TWO*PI
      i = 0
      ps_x = ZERO; ps_y = ZERO; ps_z = ZERO
      do bg = 1,self%b%n_cc
         do ag = 1,self%a%n_cc
            i = i+1
            zeta = self%exponent_sum(i)
            rho = zeta*eta_c/(zeta+eta_c)
            PAx = self%center_diff(1,i)
            PAy = self%center_diff(2,i)
            PAz = self%center_diff(3,i)
            QPx = pos_c(1)-self%pair_center(1,i)
            QPy = pos_c(2)-self%pair_center(2,i)
            QPz = pos_c(3)-self%pair_center(3,i)
            xx = rho*(QPx*QPx+QPy*QPy+QPz*QPz)
            call get_weights_(rys,xx)
            wt = rys%weight(1)*(two_pi*self%contraction_prefactor(i)*sqrt(rho))
            rzt = rys%root(1)*rho*self%exponent_inv(i)
            ps_x = ps_x+(PAx+rzt*QPx)*wt
            ps_y = ps_y+(PAy+rzt*QPy)*wt
            ps_z = ps_z+(PAz+rzt*QPz)*wt
         end do
      end do
      if (self%a%l==1) then
         ps(1,1) = ps_x
         ps(2,1) = ps_y
         ps(3,1) = ps_z
      else
         ps(1,1) = ps_x
         ps(1,2) = ps_y
         ps(1,3) = ps_z
      end if
      call destroy_(rys)
   end subroutine

   PURE subroutine make_nuc_ss(self,es,mass_c,pos_c)
   ! Make the (ss) nuclear attraction integrals, summed over primitives
   ! Numbers may be slightly different to cadpac due to a relativistic
   ! correction term.
      type(SHELL2_TYPE), IN :: self
      REAL, OUT :: es
      REAL, IN :: mass_c
      VEC(REAL,3), IN :: pos_c
      type(RYS_TYPE), allocatable :: rys
      REAL :: QPx,QPy,QPz,zeta,rho,xx,eta_c,two_pi
      INT :: ag,bg,i
      if (mass_c<TOL(15)) then; eta_c = 1.0d30  ! Very big.
      else; eta_c = 3880000000d0*(mass_c**(-TWOTHIRD))
      end if
      eta_c = 1.0d60  ! uncomment this to get rid of relativistic correction.
      call create_(rys,1)
      two_pi = TWO*PI
      es = ZERO
      i = 0
      do bg = 1,self%b%n_cc
         do ag = 1,self%a%n_cc
            i = i+1
            zeta = self%exponent_sum(i)
            rho = zeta*eta_c/(zeta+eta_c)
            QPx = pos_c(1)-self%pair_center(1,i)
            QPy = pos_c(2)-self%pair_center(2,i)
            QPz = pos_c(3)-self%pair_center(3,i)
            xx = rho*(QPx*QPx+QPy*QPy+QPz*QPz)
            call get_weights_(rys,xx)
            es = es+(two_pi*self%contraction_prefactor(i)*sqrt(rho))*rys%weight(1)
         end do
      end do
      call destroy_(rys)
   end subroutine

   PURE subroutine transfer(self,es,ab)
   ! Applies the transfer equation to (e|s) to give (a|b)
      type(SHELL2_TYPE), IN :: self
      VEC(REAL,:), IN :: es
      MAT(REAL,:,:), OUT :: ab
      if (self%same_center) then
         if (self%a%l>self%b%l) then; call transfer_l_a_same_(self,es,ab)
         else; call transfer_l_b_same_(self,es,ab)
         end if
      else
         if (self%a%l>self%b%l) then; call transfer_l_a_high_(self,es,ab)
         else; call transfer_l_b_high_(self,es,ab)
         end if
      end if
   end subroutine

   PURE subroutine transfer_l_a_same(self,es,ab)
   ! Applies the transfer equation to (e|s) to give (a|b) where shell
   ! a has the higher angular momentum. This version when a and b have
   ! the same center.
      type(SHELL2_TYPE), IN :: self
      VEC(REAL,:), IN :: es
      MAT(REAL,:,:), OUT :: ab
      MAT(REAL,:,:), allocatable :: int_new,int_old
      INT :: a,b,a1,a2,a3,aa,b1,l,na,fa,nb,fb,pb,t
   ENSURE(self%a%l>=self%b%l,"SHELL2:transfer_l_a_same ... shell a must have higher momentum than b")
   ENSURE(size(es)==(n_comp_up_to_(self%l_sum)-n_comp_up_to_(self%l_max-1)),"SHELL2:transfer_l_a_same ... wrong size, es")
   ENSURE(size(ab,1)==self%a%n_comp,"SHELL2:transfer_l_a_same ... wrong 1st dimension, ab")
   ENSURE(size(ab,2)==self%b%n_comp,"SHELL2:transfer_l_a_same ... wrong 2nd dimension, ab")
      select case (self%b%l)

      case (0)  ! (a|s) case
         ab(:,1) = es

      case (1)
         if (self%a%l==1) then  ! (p|p) case
            ab(1,1) = es(4)
            ab(1,2) = es(7)
            ab(1,3) = es(8)
            ab(2,1) = es(7)
            ab(2,2) = es(5)
            ab(2,3) = es(9)
            ab(3,1) = es(8)
            ab(3,2) = es(9)
            ab(3,3) = es(6)
         else  ! (a|p) case
            na = self%a%n_comp
            fa = n_comp_up_to_(self%a%l-1)  ! Offset into the index array
            do a = 1,na  ! Put es straight to ab
               aa = a+fa
               a1 = index_p1(aa,1)-fa
               a2 = index_p1(aa,2)-fa
               a3 = index_p1(aa,3)-fa
               ab(a,1) = es(a1)
               ab(a,2) = es(a2)
               ab(a,3) = es(a3)
            end do
         end if

      case default  ! general (a|b) case
         fa = n_comp_up_to_(self%a%l-1)  ! Offset into the index array
         na = size(es)-n_comp_(self%l_sum)  ! No. of a's in int_new goes down
         allocate(int_new(na,3))
         do a = 1,na  ! Start with p shell on a
            aa = a+fa
            a1 = index_p1(aa,1)-fa
            a2 = index_p1(aa,2)-fa
            a3 = index_p1(aa,3)-fa
            int_new(a,1) = es(a1)
            int_new(a,2) = es(a2)
            int_new(a,3) = es(a3)
         end do
         fb = 1
         nb = 3
         do l = 2,self%b%l-1
            na = na-n_comp_(self%l_sum-l+1)  ! No. of a's in int_new goes down
            pb = fb  ! Previous offset for b, in int_old
            fb = fb+nb  ! Offset into index arrays
            nb = n_comp_(l)  ! Make only the needed b's
            call move_alloc(int_new,int_old)
            allocate(int_new(na,nb))
            do b = 1,nb
               b1 = index_m1(b+fb)-pb  ! b1 relative to int_old
               t = first_nonzero(b+fb)  ! Transfer component
               do a = 1,na
                  a1 = index_p1(a+fa,t)-fa
                  int_new(a,b) = int_old(a1,b1)
               end do
            end do
            deallocate(int_old)
         end do
         na = self%a%n_comp  ! Final transfer into (a|b)
         pb = fb
         fb = fb+nb
         nb = self%b%n_comp
         call move_alloc(int_new,int_old)
         do b = 1,nb
            b1 = index_m1(b+fb)-pb
            t = first_nonzero(b+fb)
            do a = 1,na
               a1 = index_p1(a+fa,t)-fa
               ab(a,b) = int_old(a1,b1)
            end do
         end do
         deallocate(int_old)
      end select  ! case statement
   end subroutine

   PURE subroutine transfer_l_b_same(self,es,ab)
   ! Applies the transfer equation to (e|s) to give (a|b) where shell b has the higher
   ! angular momentum. This version when a and b have the same center.
      type(SHELL2_TYPE), IN :: self
      VEC(REAL,:), IN :: es
      MAT(REAL,:,:), OUT :: ab
      MAT(REAL,:,:), allocatable :: int_new,int_old
      INT :: a,b,a1,b1,b2,b3,bb,l,na,fa,pa,nb,fb,t
   ENSURE(self%b%l>=self%a%l,"SHELL2:transfer_l_b_same ... shell b must have higher momentum than a")
   ENSURE(size(es)==(n_comp_up_to_(self%l_sum)-n_comp_up_to_(self%l_max-1)),"SHELL2:transfer_l_b_same ... wrong size, es")
   ENSURE(size(ab,1)==self%a%n_comp,"SHELL2:transfer_l_b_same ... wrong 1st dimension, ab")
   ENSURE(size(ab,2)==self%b%n_comp,"SHELL2:transfer_l_b_same ... wrong 2nd dimension, ab")
      select case (self%a%l)

      case (0)  ! (s|b) case
         ab(1,:) = es

      case (1)
         if (self%b%l==1) then  ! (p|p) case
            ab(1,1) = es(4)
            ab(2,1) = es(7)
            ab(3,1) = es(8)
            ab(1,2) = es(7)
            ab(2,2) = es(5)
            ab(3,2) = es(9)
            ab(1,3) = es(8)
            ab(2,3) = es(9)
            ab(3,3) = es(6)
         else  ! (p|b) case
            nb = self%b%n_comp
            fb = n_comp_up_to_(self%b%l-1)  ! Offset into the index array
            do b = 1,nb  ! Put es straight to ab
               bb = b+fb
               b1 = index_p1(bb,1)-fb
               b2 = index_p1(bb,2)-fb
               b3 = index_p1(bb,3)-fb
               ab(1,b) = es(b1)
               ab(2,b) = es(b2)
               ab(3,b) = es(b3)
            end do
         end if

      case default
         fb = n_comp_up_to_(self%b%l-1)  ! Offset into the index array
         nb = size(es)-n_comp_(self%l_sum)  ! No. of b's in int_new goes down
         allocate(int_new(nb,3))  ! NOTE: int_new ia transposed
         do b = 1,nb  ! Start with p shell on a
            bb = b+fb
            b1 = index_p1(bb,1)-fb
            b2 = index_p1(bb,2)-fb
            b3 = index_p1(bb,3)-fb
            int_new(b,1) = es(b1)
            int_new(b,2) = es(b2)
            int_new(b,3) = es(b3)
         end do
         fa = 1
         na = 3
         do l = 2,self%a%l-1
            nb = nb-n_comp_(self%l_sum-l+1)  ! No. of b's in int_new goes down
            pa = fa  ! Previous offset for a, in int_old
            fa = fa+na  ! Offset into index arrays
            na = n_comp_(l)  ! Make only the needed a's
            call move_alloc(int_new,int_old)
            allocate(int_new(nb,na))  ! TRANSPOSED!
            do a = 1,na
               a1 = index_m1(a+fa)-pa  ! b1 relative to int_old
               t = first_nonzero(a+fa)  ! Transfer component
               do b = 1,nb
                  b1 = index_p1(b+fb,t)-fb
                  int_new(b,a) = int_old(b1,a1)
               end do
            end do
            deallocate(int_old)
         end do
         nb = self%b%n_comp  ! Final transfer into (a|b)
         pa = fa
         fa = fa+na
         na = self%a%n_comp
         call move_alloc(int_new,int_old)
         do a = 1,na
            a1 = index_m1(a+fa)-pa
            t = first_nonzero(a+fa)
            do b = 1,nb
               b1 = index_p1(b+fb,t)-fb
               ab(a,b) = int_old(b1,a1)
            end do
         end do
         deallocate(int_old)
      end select  ! case statement
   end subroutine

   PURE subroutine transfer_l_a_high(self,es,ab)
   ! Applies the transfer equation to (e|s) to give (a|b) where shell
   ! a has the higher angular momentum. NOTE: .set_hrr_indexing_arrays
   ! must be called beforehand.
      type(SHELL2_TYPE), IN :: self
      VEC(REAL,:), IN :: es
      MAT(REAL,:,:), OUT :: ab
      MAT(REAL,:,:), allocatable :: int_new,int_old
      VEC(REAL,3) :: R
      REAL :: Rt,R1,R2,R3,es_a
      INT :: a,b,a1,a2,a3,aa,b1,l,na,fa,nb,fb,pb,t
   ENSURE(self%a%l>=self%b%l,"SHELL2:transfer_l_a_high ... shell a must have higher momentum than b")
   ENSURE(size(es)==(n_comp_up_to_(self%l_sum)-n_comp_up_to_(self%l_max-1)),"SHELL2:transfer_l_a_high ... wrong size, es")
   ENSURE(size(ab,1)==self%a%n_comp,"SHELL2:transfer_l_a_high ... wrong 1st dimension, ab")
   ENSURE(size(ab,2)==self%b%n_comp,"SHELL2:transfer_l_a_high ... wrong 2nd dimension, ab")
      select case (self%b%l)

      case (0)  ! (a|s) case
         ab(:,1) = es

      case (1)
         R = self%a%position-self%b%position
         R1 = R(1); R2 = R(2); R3 = R(3)
         if (self%a%l==1) then  ! (p|p) case
            es_a = es(1)
            ab(1,1) = es(4)+R1*es_a
            ab(1,2) = es(7)+R2*es_a
            ab(1,3) = es(8)+R3*es_a
            es_a = es(2)
            ab(2,1) = es(7)+R1*es_a
            ab(2,2) = es(5)+R2*es_a
            ab(2,3) = es(9)+R3*es_a
            es_a = es(3)
            ab(3,1) = es(8)+R1*es_a
            ab(3,2) = es(9)+R2*es_a
            ab(3,3) = es(6)+R3*es_a
         else  ! (a|p) case
            na = self%a%n_comp
            fa = n_comp_up_to_(self%a%l-1)  ! Offset into the index array
            do a = 1,na  ! Put es straight to ab
               aa = a+fa
               a1 = index_p1(aa,1)-fa
               a2 = index_p1(aa,2)-fa
               a3 = index_p1(aa,3)-fa
               es_a = es(a)
               ab(a,1) = es(a1)+R1*es_a
               ab(a,2) = es(a2)+R2*es_a
               ab(a,3) = es(a3)+R3*es_a
            end do
         end if

      case default
         ! general (a|b) case
         R = self%a%position-self%b%position
         R1 = R(1); R2 = R(2); R3 = R(3)
         fa = n_comp_up_to_(self%a%l-1)  ! Offset into the index array
         na = size(es)-n_comp_(self%l_sum)  ! No. of a's in int_new goes down
         allocate(int_new(na,3))
         do a = 1,na  ! Start with p shell on a
            aa = a+fa
            a1 = index_p1(aa,1)-fa
            a2 = index_p1(aa,2)-fa
            a3 = index_p1(aa,3)-fa
            es_a = es(a)
            int_new(a,1) = es(a1)+R1*es_a
            int_new(a,2) = es(a2)+R2*es_a
            int_new(a,3) = es(a3)+R3*es_a
         end do
         fb = 1
         nb = 3
         do l = 2,self%b%l-1
            na = na-n_comp_(self%l_sum-l+1)  ! No. of a's in int_new goes down
            pb = fb  ! Previous offset for b, in int_old
            fb = fb+nb  ! Offset into index arrays
            nb = n_comp_(l)  ! Make only the needed b's
            call move_alloc(int_new,int_old)
            allocate(int_new(na,nb))
            do b = 1,nb
               b1 = index_m1(b+fb)-pb  ! b1 relative to int_old
               t = first_nonzero(b+fb)  ! Transfer component
               Rt = R(t)
               do a = 1,na
                  a1 = index_p1(a+fa,t)-fa
                  int_new(a,b) = int_old(a1,b1)+Rt*int_old(a,b1)
               end do
            end do
            deallocate(int_old)
         end do
         na = self%a%n_comp  ! Final transfer into (a|b)
         pb = fb
         fb = fb+nb
         nb = self%b%n_comp
         call move_alloc(int_new,int_old)
         do b = 1,nb
            b1 = index_m1(b+fb)-pb
            t = first_nonzero(b+fb)
            Rt = R(t)
            do a = 1,na
               a1 = index_p1(a+fa,t)-fa
               ab(a,b) = int_old(a1,b1)+Rt*int_old(a,b1)
            end do
         end do
         deallocate(int_old)
      end select  ! case statement
   end subroutine

   PURE subroutine transfer_l_b_high(self,es,ab)
   ! Applies the transfer equation to (e|s) to give (a|b) where shell b has the higher
   ! angular momentum. NOTE: .set_hrr_indexing_arrays must be called beforehand.
      type(SHELL2_TYPE), IN :: self
      VEC(REAL,:), IN :: es
      MAT(REAL,:,:), OUT :: ab
      MAT(REAL,:,:), allocatable :: int_new,int_old
      VEC(REAL,3) :: R
      REAL :: Rt,R1,R2,R3,es_b
      INT :: a,b,a1,b1,b2,b3,bb,l,na,fa,pa,nb,fb,t
   ENSURE(self%b%l>=self%a%l,"SHELL2:transfer_l_b_high ... shell b must have higher momentum than a")
   ENSURE(size(es)==(n_comp_up_to_(self%l_sum)-n_comp_up_to_(self%l_max-1)),"SHELL2:transfer_l_b_high ... wrong size, es")
   ENSURE(size(ab,1)==self%a%n_comp,"SHELL2:transfer_l_b_high ... wrong 1st dimension, ab")
   ENSURE(size(ab,2)==self%b%n_comp,"SHELL2:transfer_l_b_high ... wrong 2nd dimension, ab")
      select case (self%a%l)
      case (0)  ! (s|b) case
         ab(1,:) = es
      case (1)
         R = self%b%position-self%a%position
         R1 = R(1); R2 = R(2); R3 = R(3)
         if (self%b%l==1) then  ! (p|p) case
            es_b = es(1)
            ab(1,1) = es(4)+R1*es_b
            ab(2,1) = es(7)+R2*es_b
            ab(3,1) = es(8)+R3*es_b
            es_b = es(2)
            ab(1,2) = es(7)+R1*es_b
            ab(2,2) = es(5)+R2*es_b
            ab(3,2) = es(9)+R3*es_b
            es_b = es(3)
            ab(1,3) = es(8)+R1*es_b
            ab(2,3) = es(9)+R2*es_b
            ab(3,3) = es(6)+R3*es_b
         else  ! (p|b) case
            nb = self%b%n_comp
            fb = n_comp_up_to_(self%b%l-1)  ! Offset into the index array
            do b = 1,nb  ! Put es straight to ab
               bb = b+fb
               b1 = index_p1(bb,1)-fb
               b2 = index_p1(bb,2)-fb
               b3 = index_p1(bb,3)-fb
               es_b = es(b)
               ab(1,b) = es(b1)+R1*es_b
               ab(2,b) = es(b2)+R2*es_b
               ab(3,b) = es(b3)+R3*es_b
            end do
         end if
      case default
         R = self%b%position-self%a%position
         R1 = R(1); R2 = R(2); R3 = R(3)
         fb = n_comp_up_to_(self%b%l-1)  ! Offset into the index array
         nb = size(es)-n_comp_(self%l_sum)  ! No. of b's in int_new goes down
         allocate(int_new(nb,3))  ! NOTE: int_new ia transposed
         do b = 1,nb  ! Start with p shell on a
            bb = b+fb
            b1 = index_p1(bb,1)-fb
            b2 = index_p1(bb,2)-fb
            b3 = index_p1(bb,3)-fb
            es_b = es(b)
            int_new(b,1) = es(b1)+R1*es_b
            int_new(b,2) = es(b2)+R2*es_b
            int_new(b,3) = es(b3)+R3*es_b
         end do
         fa = 1
         na = 3
         do l = 2,self%a%l-1
            nb = nb-n_comp_(self%l_sum-l+1)  ! No. of b's in int_new goes down
            pa = fa  ! Previous offset for a, in int_old
            fa = fa+na  ! Offset into index arrays
            na = n_comp_(l)  ! Make only the needed a's
            call move_alloc(int_new,int_old)
            allocate(int_new(nb,na))  ! TRANSPOSED!
            do a = 1,na
               a1 = index_m1(a+fa)-pa  ! b1 relative to int_old
               t = first_nonzero(a+fa)  ! Transfer component
               Rt = R(t)
               do b = 1,nb
                  b1 = index_p1(b+fb,t)-fb
                  int_new(b,a) = int_old(b1,a1)+Rt*int_old(b,a1)
               end do
            end do
            deallocate(int_old)
         end do
         nb = self%b%n_comp  ! Final transfer into (a|b)
         pa = fa
         fa = fa+na
         na = self%a%n_comp
         call move_alloc(int_new,int_old)
         do a = 1,na
            a1 = index_m1(a+fa)-pa
            t = first_nonzero(a+fa)
            Rt = R(t)
            do b = 1,nb
               b1 = index_p1(b+fb,t)-fb
               ab(a,b) = int_old(b1,a1)+Rt*int_old(b,a1)
            end do
         end do
         deallocate(int_old)
      end select  ! case statement
   end subroutine

   PURE subroutine transfer_ft(self,es,ab)
   ! Applies the transfer equation to (e|s) to give (a|b)
      type(SHELL2_TYPE), IN :: self
      MAT(CPX,:,:), IN :: es
      MAT3(CPX,:,:,:), OUT :: ab
      if (self%same_center) then
         if (self%a%l>self%b%l) then; call transfer_l_a_same_ft_(self,es,ab)
         else; call transfer_l_b_same_ft_(self,es,ab)
         end if
      else
         if (self%a%l>self%b%l) then; call transfer_l_a_high_ft_(self,es,ab)
         else; call transfer_l_b_high_ft_(self,es,ab)
         end if
      end if
   end subroutine

   PURE subroutine transfer_l_a_same_ft(self,es,ab)
   ! Applies the transfer equation to (e|s) to give (a|b) where shell a has the higher
   ! angular momentum. This version when a and b have the same center.
      type(SHELL2_TYPE), IN :: self
      MAT(CPX,:,:), IN :: es
      MAT3(CPX,:,:,:), OUT :: ab
      MAT3(CPX,:,:,:), allocatable :: int_new,int_old
      INT :: nk,a,b,a1,a2,a3,aa,b1,l,na,fa,nb,fb,pb,t
   ENSURE(self%a%l>=self%b%l,"SHELL2:transfer_l_a_same_ft ... shell a must have higher momentum than b")
   ENSURE(size(es,2)==(n_comp_up_to_(self%l_sum)-n_comp_up_to_(self%l_max-1)),"SHELL2:transfer_l_a_same_ft ... wrong size, es")
   ENSURE(size(ab,2)==self%a%n_comp,"SHELL2:transfer_l_a_same_ft ... wrong 2nd dimension, ab")
   ENSURE(size(ab,3)==self%b%n_comp,"SHELL2:transfer_l_a_same_ft ... wrong 3rd dimension, ab")
   ENSURE(size(ab,1)==size(es,1),"SHELL2:transfer_l_a_same_ft ... incompatible ab and es")
      nk = size(es,1)
      select case (self%b%l)
      case (0)  ! (a|s) case
         ab(:,:,1) = es
      case (1)
         if (self%a%l==1) then  ! (p|p) case
            ab(:,1,1) = es(:,4)
            ab(:,1,2) = es(:,7)
            ab(:,1,3) = es(:,8)
            ab(:,2,1) = es(:,7)
            ab(:,2,2) = es(:,5)
            ab(:,2,3) = es(:,9)
            ab(:,3,1) = es(:,8)
            ab(:,3,2) = es(:,9)
            ab(:,3,3) = es(:,6)
         else  ! (a|p) case
            na = self%a%n_comp
            fa = n_comp_up_to_(self%a%l-1)  ! Offset into the index array
            do a = 1,na  ! Put es straight to ab
               aa = a+fa
               a1 = index_p1(aa,1)-fa
               a2 = index_p1(aa,2)-fa
               a3 = index_p1(aa,3)-fa
               ab(:,a,1) = es(:,a1)
               ab(:,a,2) = es(:,a2)
               ab(:,a,3) = es(:,a3)
            end do
         end if
      case default
            ! general (a|b) case
         fa = n_comp_up_to_(self%a%l-1)  ! Offset into the index array
         na = size(es,2)-n_comp_(self%l_sum)  ! No. of a's in int_new goes down
         allocate(int_new(nk,na,3))
         do a = 1,na  ! Start with p shell on a
            aa = a+fa
            a1 = index_p1(aa,1)-fa
            a2 = index_p1(aa,2)-fa
            a3 = index_p1(aa,3)-fa
            int_new(:,a,1) = es(:,a1)
            int_new(:,a,2) = es(:,a2)
            int_new(:,a,3) = es(:,a3)
         end do
         fb = 1
         nb = 3
         do l = 2,self%b%l-1
            na = na-n_comp_(self%l_sum-l+1)  ! No. of a's in int_new goes down
            pb = fb  ! Previous offset for b, in int_old
            fb = fb+nb  ! Offset into index arrays
            nb = n_comp_(l)  ! Make only the needed b's
            call move_alloc(int_new,int_old)
            allocate(int_new(nk,na,nb))
            do b = 1,nb
               b1 = index_m1(b+fb)-pb  ! b1 relative to int_old
               t = first_nonzero(b+fb)  ! Transfer component
               do a = 1,na
                  a1 = index_p1(a+fa,t)-fa
                  int_new(:,a,b) = int_old(:,a1,b1)
               end do
            end do
            deallocate(int_old)
         end do
         na = self%a%n_comp  ! Final transfer into (a|b)
         pb = fb
         fb = fb+nb
         nb = self%b%n_comp
         call move_alloc(int_new,int_old)
         do b = 1,nb
            b1 = index_m1(b+fb)-pb
            t = first_nonzero(b+fb)
            do a = 1,na
               a1 = index_p1(a+fa,t)-fa
               ab(:,a,b) = int_old(:,a1,b1)
            end do
         end do
         deallocate(int_old)
      end select  ! case statement
   end subroutine

   PURE subroutine transfer_l_b_same_ft(self,es,ab)
   ! Applies the transfer equation to (e|s) to give (a|b) where shell b has the higher
   ! angular momentum. This version when a and b have the same center.
      type(SHELL2_TYPE), IN :: self
      MAT(CPX,:,:), IN :: es
      MAT3(CPX,:,:,:), OUT :: ab
      MAT3(CPX,:,:,:), allocatable :: int_new,int_old
      INT :: nk,a,b,a1,b1,b2,b3,bb,l,na,fa,pa,nb,fb,t
   ENSURE(self%b%l>=self%a%l,"SHELL2:transfer_l_b_same_ft ... shell b must have higher momentum than a")
   ENSURE(size(es,2)==(n_comp_up_to_(self%l_sum)-n_comp_up_to_(self%l_max-1)),"SHELL2:transfer_l_b_same_ft ... wrong size, es")
   ENSURE(size(ab,2)==self%a%n_comp,"SHELL2:transfer_l_b_same_ft ... wrong 2nd dimension, ab")
   ENSURE(size(ab,3)==self%b%n_comp,"SHELL2:transfer_l_b_same_ft ... wrong 3rd dimension, ab")
   ENSURE(size(ab,1)==size(es,1),"SHELL2:transfer_l_b_same_ft ... incompatible ab and es")
      nk = size(es,1)
      select case (self%a%l)
      case (0)  ! (s|b) case
         ab(:,1,:) = es
      case (1)
         if (self%b%l==1) then  ! (p|p) case
            ab(:,1,1) = es(:,4)
            ab(:,2,1) = es(:,7)
            ab(:,3,1) = es(:,8)
            ab(:,1,2) = es(:,7)
            ab(:,2,2) = es(:,5)
            ab(:,3,2) = es(:,9)
            ab(:,1,3) = es(:,8)
            ab(:,2,3) = es(:,9)
            ab(:,3,3) = es(:,6)
         else  ! (p|b) case
            nb = self%b%n_comp
            fb = n_comp_up_to_(self%b%l-1)  ! Offset into the index array
            do b = 1,nb  ! Put es straight to ab
               bb = b+fb
               b1 = index_p1(bb,1)-fb
               b2 = index_p1(bb,2)-fb
               b3 = index_p1(bb,3)-fb
               ab(:,1,b) = es(:,b1)
               ab(:,2,b) = es(:,b2)
               ab(:,3,b) = es(:,b3)
            end do
         end if
      case default
         fb = n_comp_up_to_(self%b%l-1)  ! Offset into the index array
         nb = size(es,2)-n_comp_(self%l_sum)  ! No. of b's in int_new goes down
         allocate(int_new(nk,nb,3))  ! NOTE: int_new ia transposed
         do b = 1,nb  ! Start with p shell on a
            bb = b+fb
            b1 = index_p1(bb,1)-fb
            b2 = index_p1(bb,2)-fb
            b3 = index_p1(bb,3)-fb
            int_new(:,b,1) = es(:,b1)
            int_new(:,b,2) = es(:,b2)
            int_new(:,b,3) = es(:,b3)
         end do
         fa = 1
         na = 3
         do l = 2,self%a%l-1
            nb = nb-n_comp_(self%l_sum-l+1)  ! No. of b's in int_new goes down
            pa = fa  ! Previous offset for a, in int_old
            fa = fa+na  ! Offset into index arrays
            na = n_comp_(l)  ! Make only the needed a's
            call move_alloc(int_new,int_old)
            allocate(int_new(nk,nb,na))  ! TRANSPOSED!
            do a = 1,na
               a1 = index_m1(a+fa)-pa  ! b1 relative to int_old
               t = first_nonzero(a+fa)  ! Transfer component
               do b = 1,nb
                  b1 = index_p1(b+fb,t)-fb
                  int_new(:,b,a) = int_old(:,b1,a1)
               end do
            end do
            deallocate(int_old)
         end do
         nb = self%b%n_comp  ! Final transfer into (a|b)
         pa = fa
         fa = fa+na
         na = self%a%n_comp
         call move_alloc(int_new,int_old)
         do a = 1,na
            a1 = index_m1(a+fa)-pa
            t = first_nonzero(a+fa)
            do b = 1,nb
               b1 = index_p1(b+fb,t)-fb
               ab(:,a,b) = int_old(:,b1,a1)
            end do
         end do
         deallocate(int_old)
      end select  ! case statement
   end subroutine

   PURE subroutine transfer_l_a_high_ft(self,es,ab)
   ! Applies the transfer equation to (e|s) to give (a|b) where shell a has the higher
   ! angular momentum. NOTE: .set_hrr_indexing_arrays must be called beforehand.
      type(SHELL2_TYPE), IN :: self
      MAT(CPX,:,:), IN :: es
      MAT3(CPX,:,:,:), OUT :: ab
      MAT3(CPX,:,:,:), allocatable :: int_new,int_old
      VEC(REAL,3) :: R
      REAL :: Rt,R1,R2,R3
      INT :: nk,a,b,a1,a2,a3,aa,b1,l,na,fa,nb,fb,pb,t
   ENSURE(self%a%l>=self%b%l,"SHELL2:transfer_l_a_high_ft ... shell a must have higher momentum than b")
   ENSURE(size(es,2)==(n_comp_up_to_(self%l_sum)-n_comp_up_to_(self%l_max-1)),"SHELL2:transfer_l_a_high_ft ... wrong size, es")
   ENSURE(size(ab,2)==self%a%n_comp,"SHELL2:transfer_l_a_high_ft ... wrong 2nd dimension, ab")
   ENSURE(size(ab,3)==self%b%n_comp,"SHELL2:transfer_l_a_high_ft ... wrong 3rd dimension, ab")
   ENSURE(size(ab,1)==size(es,1),"SHELL2:transfer_l_a_high_ft ... incompatible ab and es")
      nk = size(es,1)
      select case (self%b%l)

      case (0)  ! (a|s) case
         ab(:,:,1) = es

      case (1)
         R = self%a%position-self%b%position
         R1 = R(1); R2 = R(2); R3 = R(3)
         if (self%a%l==1) then  ! (p|p) case
            ab(:,1,1) = es(:,4)+R1*es(:,1)
            ab(:,1,2) = es(:,7)+R2*es(:,1)
            ab(:,1,3) = es(:,8)+R3*es(:,1)
            ab(:,2,1) = es(:,7)+R1*es(:,2)
            ab(:,2,2) = es(:,5)+R2*es(:,2)
            ab(:,2,3) = es(:,9)+R3*es(:,2)
            ab(:,3,1) = es(:,8)+R1*es(:,3)
            ab(:,3,2) = es(:,9)+R2*es(:,3)
            ab(:,3,3) = es(:,6)+R3*es(:,3)
         else  ! (a|p) case
            na = self%a%n_comp
            fa = n_comp_up_to_(self%a%l-1)  ! Offset into the index array
            do a = 1,na  ! Put es straight to ab
               aa = a+fa
               a1 = index_p1(aa,1)-fa
               a2 = index_p1(aa,2)-fa
               a3 = index_p1(aa,3)-fa
               ab(:,a,1) = es(:,a1)+R1*es(:,a)
               ab(:,a,2) = es(:,a2)+R2*es(:,a)
               ab(:,a,3) = es(:,a3)+R3*es(:,a)
            end do
         end if

      case default
         ! general (a|b) case
         R = self%a%position-self%b%position
         R1 = R(1); R2 = R(2); R3 = R(3)
         fa = n_comp_up_to_(self%a%l-1)  ! Offset into the index array
         na = size(es,2)-n_comp_(self%l_sum)  ! No. of a's in int_new goes down
         allocate(int_new(nk,na,3))
         do a = 1,na  ! Start with p shell on a
            aa = a+fa
            a1 = index_p1(aa,1)-fa
            a2 = index_p1(aa,2)-fa
            a3 = index_p1(aa,3)-fa
            int_new(:,a,1) = es(:,a1)+R1*es(:,a)
            int_new(:,a,2) = es(:,a2)+R2*es(:,a)
            int_new(:,a,3) = es(:,a3)+R3*es(:,a)
         end do
         fb = 1
         nb = 3
         do l = 2,self%b%l-1
            na = na-n_comp_(self%l_sum-l+1)  ! No. of a's in int_new goes down
            pb = fb  ! Previous offset for b, in int_old
            fb = fb+nb  ! Offset into index arrays
            nb = n_comp_(l)  ! Make only the needed b's
            call move_alloc(int_new,int_old)
            allocate(int_new(nk,na,nb))
            do b = 1,nb
               b1 = index_m1(b+fb)-pb  ! b1 relative to int_old
               t = first_nonzero(b+fb)  ! Transfer component
               Rt = R(t)
               do a = 1,na
                  a1 = index_p1(a+fa,t)-fa
                  int_new(:,a,b) = int_old(:,a1,b1)+Rt*int_old(:,a,b1)
               end do
            end do
            deallocate(int_old)
         end do

            ! Final transfer into (a|b)
         na = self%a%n_comp
         pb = fb
         fb = fb+nb
         nb = self%b%n_comp
         call move_alloc(int_new,int_old)
         do b = 1,nb
            b1 = index_m1(b+fb)-pb
            t = first_nonzero(b+fb)
            Rt = R(t)
            do a = 1,na
               a1 = index_p1(a+fa,t)-fa
               ab(:,a,b) = int_old(:,a1,b1)+Rt*int_old(:,a,b1)
            end do
         end do
         deallocate(int_old)
      end select  ! case statement
   end subroutine

   PURE subroutine transfer_l_b_high_ft(self,es,ab)
   ! Applies the transfer equation to (e|s) to give (a|b) where shell b has the higher
   ! angular momentum. NOTE: .set_hrr_indexing_arrays must be called beforehand.
      type(SHELL2_TYPE), IN :: self
      MAT(CPX,:,:), IN :: es
      MAT3(CPX,:,:,:), OUT :: ab
      MAT3(CPX,:,:,:), allocatable :: int_new,int_old
      VEC(REAL,3) :: R
      REAL :: Rt,R1,R2,R3
      INT :: nk,a,b,a1,b1,b2,b3,bb,l,na,fa,pa,nb,fb,t
   ENSURE(self%b%l>=self%a%l,"SHELL2:transfer_l_b_high_ft ... shell b must have higher momentum than a")
   ENSURE(size(es,2)==(n_comp_up_to_(self%l_sum)-n_comp_up_to_(self%l_max-1)),"SHELL2:transfer_l_b_high_ft ... wrong size, es")
   ENSURE(size(ab,2)==self%a%n_comp,"SHELL2:transfer_l_b_high_ft ... wrong 2nd dimension, ab")
   ENSURE(size(ab,3)==self%b%n_comp,"SHELL2:transfer_l_b_high_ft ... wrong 3rd dimension, ab")
   ENSURE(size(ab,1)==size(es,1),"SHELL2:transfer_l_b_high_ft ... incompatible ab and es")
      nk = size(es,1)
      select case (self%a%l)

      case (0)  ! (s|b) case
         ab(:,1,:) = es

      case (1)
         R = self%b%position-self%a%position
         R1 = R(1); R2 = R(2); R3 = R(3)
         if (self%b%l==1) then  ! (p|p) case
            ab(:,1,1) = es(:,4)+R1*es(:,1)
            ab(:,2,1) = es(:,7)+R2*es(:,1)
            ab(:,3,1) = es(:,8)+R3*es(:,1)
            ab(:,1,2) = es(:,7)+R1*es(:,2)
            ab(:,2,2) = es(:,5)+R2*es(:,2)
            ab(:,3,2) = es(:,9)+R3*es(:,2)
            ab(:,1,3) = es(:,8)+R1*es(:,3)
            ab(:,2,3) = es(:,9)+R2*es(:,3)
            ab(:,3,3) = es(:,6)+R3*es(:,3)
         else  ! (p|b) case
            nb = self%b%n_comp
            fb = n_comp_up_to_(self%b%l-1)  ! Offset into the index array
            do b = 1,nb  ! Put es straight to ab
               bb = b+fb
               b1 = index_p1(bb,1)-fb
               b2 = index_p1(bb,2)-fb
               b3 = index_p1(bb,3)-fb
               ab(:,1,b) = es(:,b1)+R1*es(:,b)
               ab(:,2,b) = es(:,b2)+R2*es(:,b)
               ab(:,3,b) = es(:,b3)+R3*es(:,b)
            end do
         end if

      case default
         R = self%b%position-self%a%position
         R1 = R(1); R2 = R(2); R3 = R(3)
         fb = n_comp_up_to_(self%b%l-1)  ! Offset into the index array
         nb = size(es,2)-n_comp_(self%l_sum)  ! No. of b's in int_new goes down
         allocate(int_new(nk,nb,3))  ! NOTE: int_new ia transposed
         do b = 1,nb  ! Start with p shell on a
            bb = b+fb
            b1 = index_p1(bb,1)-fb
            b2 = index_p1(bb,2)-fb
            b3 = index_p1(bb,3)-fb
            int_new(:,b,1) = es(:,b1)+R1*es(:,b)
            int_new(:,b,2) = es(:,b2)+R2*es(:,b)
            int_new(:,b,3) = es(:,b3)+R3*es(:,b)
         end do
         fa = 1
         na = 3
         do l = 2,self%a%l-1
            nb = nb-n_comp_(self%l_sum-l+1)  ! No. of b's in int_new goes down
            pa = fa  ! Previous offset for a, in int_old
            fa = fa+na  ! Offset into index arrays
            na = n_comp_(l)  ! Make only the needed a's
            call move_alloc(int_new,int_old)
            allocate(int_new(nk,nb,na))  ! TRANSPOSED!
            do a = 1,na
               a1 = index_m1(a+fa)-pa  ! b1 relative to int_old
               t = first_nonzero(a+fa)  ! Transfer component
               Rt = R(t)
               do b = 1,nb
                  b1 = index_p1(b+fb,t)-fb
                  int_new(:,b,a) = int_old(:,b1,a1)+Rt*int_old(:,b,a1)
               end do
            end do
            deallocate(int_old)
         end do

            ! Final transfer into (a|b)
         nb = self%b%n_comp
         pa = fa
         fa = fa+na
         na = self%a%n_comp
         call move_alloc(int_new,int_old)
         do a = 1,na
            a1 = index_m1(a+fa)-pa
            t = first_nonzero(a+fa)
            Rt = R(t)
            do b = 1,nb
               b1 = index_p1(b+fb,t)-fb
               ab(:,a,b) = int_old(:,b1,a1)+Rt*int_old(:,b,a1)
            end do
         end do
         deallocate(int_old)
      end select  ! case statement
   end subroutine


! ====================================
! Normalization routines and functions
! ====================================

   PURE subroutine change_to_spherical_0(self,C,S)
   ! Change a cartesian integral block "C" to a spherical integral
   ! block "S"
      type(SHELL2_TYPE), IN :: self
      MAT(REAL,:,:), IN :: C
      MAT(REAL,:,:), OUT :: S
      MAT(REAL,:,:), allocatable :: W
      MAT(REAL,:,:), allocatable :: U
      if (self%l_max<2) then
         S = C  ! don't use this case
      else if (self%l_min>=2) then
         call create_(W,self%a%n_comp,self%b%n_sph)
         U = spherical_harmonics_for(self%b%l)%element
         call to_product_of_(W,C,U)
         U = spherical_harmonics_for(self%a%l)%element
         call to_product_of_(S,U,W,transpose_a=TRUE)
         call destroy_(W)
      else if (self%a%l<2 AND self%b%l>=2) then
         U = spherical_harmonics_for(self%b%l)%element
         call to_product_of_(S,C,U)
      else if (self%b%l<2 AND self%a%l>=2) then
         U = spherical_harmonics_for(self%a%l)%element
         call to_product_of_(S,U,C,transpose_a=TRUE)
      end if
   end subroutine

   PURE subroutine change_to_spherical_1(self,C,S)
   ! Change a cartesian integral block "C" to a spherical integral
   ! block "S"
      type(SHELL2_TYPE), IN :: self
      MAT(CPX,:,:), IN :: C
      MAT(CPX,:,:), OUT :: S
      MAT(REAL,:,:), allocatable :: U
      MAT(CPX,:,:), allocatable :: W
      if (self%l_max<2) then
         S = C  ! don't use this case
      else if (self%l_min>=2) then
         call create_(W,self%a%n_comp,self%b%n_sph)
         U = spherical_harmonics_for(self%b%l)%element
         call to_product_of_(W,C,U)
         U = spherical_harmonics_for(self%a%l)%element
         call to_product_of_(S,U,W,transpose_a=TRUE)
         call destroy_(W)
      else if (self%a%l<2 AND self%b%l>=2) then
         U = spherical_harmonics_for(self%b%l)%element
         call to_product_of_(S,C,U)
      else if (self%b%l<2 AND self%a%l>=2) then
         U = spherical_harmonics_for(self%a%l)%element
         call to_product_of_(S,U,C,transpose_a=TRUE)
      end if
   end subroutine

   PURE subroutine change_to_spherical_2(self,C,S)
   ! Change a cartesian integral block "C" to a spherical integral
   ! block "S"
      type(SHELL2_TYPE), IN :: self
      MAT3(CPX,:,:,:), IN :: C
      MAT3(CPX,:,:,:), OUT :: S
      MAT(REAL,:,:), allocatable :: U
      MAT3(CPX,:,:,:), allocatable :: W
      INT :: i
      do i = 1,size(C,3)
         if (self%l_max<2) then
            S = C  ! don't use this case
         else if (self%l_min>=2) then
            call create_(W,self%a%n_comp,self%b%n_sph,size(C,3))
            U = spherical_harmonics_for(self%b%l)%element
            call to_product_of_(W(:,:,i),C(:,:,i),U)
            U = spherical_harmonics_for(self%a%l)%element
            call to_product_of_(S(:,:,i),U,W(:,:,i),transpose_a=TRUE)
            call destroy_(W)
         else if (self%a%l<2 AND self%b%l>=2) then
            U = spherical_harmonics_for(self%b%l)%element
            call to_product_of_(S(:,:,i),C(:,:,i),U)
         else if (self%b%l<2 AND self%a%l>=2) then
            U = spherical_harmonics_for(self%a%l)%element
            call to_product_of_(S(:,:,i),U,C(:,:,i),transpose_a=TRUE)
         end if
      end do
   end subroutine

   subroutine put(self)
   ! Put the shell2 information on file "out"
      type(SHELL2_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"Shell a:")
      call put_(self%a)
      call flush_(stdout)
      call text_(stdout,"Shell b:")
      call put_(self%b)
      call flush_(stdout)
      call show_(stdout,"n_gaussian_pairs =",self%n_gaussian_pairs)
      call show_(stdout,"l_max            =",self%l_max)
      call show_(stdout,"l_min            =",self%l_min)
      call show_(stdout,"l_sum            =",self%l_sum)
      call show_(stdout,"first_gaussian   =",self%first_gaussian)
      call show_(stdout,"last_gaussian    =",self%last_gaussian)
      call show_(stdout,"n_gaussian       =",self%n_gaussian)
      call show_(stdout,"destroy_all      =",self%destroy_all)
      call show_(stdout,"same_center      =",self%same_center)
      if (allocated(self%exponent_sum)) then
         call text_(stdout,"exponent_sum:")
         call put_(stdout,self%exponent_sum)
      end if
      if (allocated(self%exponent_inv)) then
         call text_(stdout,"exponent_inv:")
         call put_(stdout,self%exponent_inv)
      end if
      if (allocated(self%contraction_prefactor)) then
         call text_(stdout,"cc_prefactor:")
         call put_(stdout,self%contraction_prefactor)
      end if
      if (allocated(self%pair_center)) then
         call text_(stdout,"pair_center:")
         call put_(stdout,self%pair_center)
      end if
      if (allocated(self%center_diff)) then
         call text_(stdout,"center_diff:")
         call put_(stdout,self%center_diff)
      end if
   end subroutine

end module
