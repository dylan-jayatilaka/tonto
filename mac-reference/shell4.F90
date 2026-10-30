!=================================================================
!
! Copyright (C) Daniel Grimwood, March 1998
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
!=================================================================
module SHELL4_MODULE

#  include "macros"

   include "shell4.use"

   implicit none

   include "shell4.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(SHELL4_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"SHELL4:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(SHELL4_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"SHELL4:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      type(SHELL4_TYPE), INOUT :: self
      call destroy_ab_(self)
      call destroy_cd_(self)
   end subroutine

   PURE subroutine destroy_ab(self)
   ! Destroy the shella and shellb pointer parts of self
      type(SHELL4_TYPE), INOUT :: self
      call destroy_ptr_part_(self%a)
      call destroy_ptr_part_(self%b)
   end subroutine

   PURE subroutine destroy_cd(self)
   ! Destroy the shellc and shelld pointer parts of self
      type(SHELL4_TYPE), INOUT :: self
      call destroy_ptr_part_(self%c)
      call destroy_ptr_part_(self%d)
   end subroutine


! Try to get rid of these

!   create(shell_a,shell_b,shell_c,shell_d) :: leaky
!   ! Create a shell4 object from copies of shell1s.
!      self :: allocatable, INOUT
!      shell_a,shell_b,shell_c,shell_d :: SHELL1, IN
!      .create
!      .copy(shell_a,shell_b,shell_c,shell_d)
!   end

!   create(shell_a,shell_b,shell_c,shell_d,pos_a,pos_b,pos_c,pos_d) :: leaky
!   ! Create a shell4 object from copies of shells and their positions.
!      self :: allocatable, INOUT
!      shell_a,shell_b,shell_c,shell_d :: SHELL, IN
!      pos_a,pos_b,pos_c,pos_d :: VEC{REAL}(3), IN
!      .create
!      .copy(shell_a,shell_b,shell_c,shell_d,pos_a,pos_b,pos_c,pos_d)
!   end

!  ====================================================
!  Setting parts of self from other shells and shell1's
!  ====================================================

!   copy(shell_a,shell_b,shell_c,shell_d) :: leaky
!   ! Copy the shell4 using from shell1 objects
!      self 
!      shell_a,shell_b,shell_c,shell_d :: SHELL1, IN
!      .a.copy(shell_a)
!      .b.copy(shell_b)
!      .c.copy(shell_c)
!      .d.copy(shell_d)
!   end

!   copy(shell_a,shell_b,shell_c,shell_d,pos_a,pos_b,pos_c,pos_d) :: leaky
!   ! Set the shell4 using shell objects and positions
!      shell_a,shell_b,shell_c,shell_d :: SHELL, IN
!      pos_a,pos_b,pos_c,pos_d :: VEC{REAL}, IN
!      .a.copy(shell_a,pos_a)
!      .b.copy(shell_b,pos_b)
!      .c.copy(shell_c,pos_c)
!      .d.copy(shell_d,pos_d)
!   end

!   copy_ab(shell_a,shell_b,pos_a,pos_b) :: leaky
!   ! Copy the a and b parts of the shell4 using from shell1 objects
!      shell_a,shell_b :: SHELL, IN
!      pos_a,pos_b :: VEC{REAL}, IN
!      .a.copy(shell_a,pos_a)
!      .b.copy(shell_b,pos_b)
!   end

!   copy_cd(shell_c,shell_d,pos_c,pos_d) :: leaky
!   ! Copy the c and d parts of the shell4 using from shell1 objects
!      shell_c,shell_d :: SHELL, IN
!      pos_c,pos_d :: VEC{REAL}, IN
!      .c.copy(shell_c,pos_c)
!      .d.copy(shell_d,pos_d)
!   end

!  ===========
!  Set methods
!  ===========

   pure subroutine set_spherical(self,val)
   ! Set sphericals to "val" defaults
      type(SHELL4_TYPE), INOUT :: self
      BIN, IN :: val
      self%is_spherical = val
   end subroutine


!  ======================
!  Miscellaneous Routines
!  ======================

   pure subroutine unnormalize(self)
   ! Unnormalize each shell in this shell quartet
      type(SHELL4_TYPE), INOUT :: self
      call unnormalize_(self%a)
      call unnormalize_(self%b)
      call unnormalize_(self%c)
      call unnormalize_(self%d)
   end subroutine


!  ===========
!  ERI cutoffs
!  ===========

   pure function ab_kappa_max(self) result (res)
   ! Return the largest kappa_ab used in the Lindh integrals.
      type(SHELL4_TYPE), IN :: self
      REAL :: res
      VEC(REAL,3) :: AB
      REAL :: b,b_cc,a,ab_inv,prefac,r2_ab
      INT :: bg,ag
      AB = self%b%position-self%a%position
      r2_ab = dot_product(AB,AB)
      res = ZERO
      do bg = 1,self%b%n_cc
         b = self%b%exponent(bg)
         b_cc = self%b%contraction(bg)
         do ag = 1,self%a%n_cc
            a = self%a%exponent(ag)
            ab_inv = ONE/(a+b)
            prefac = b_cc*self%a%contraction(ag)*ab_inv*sqrt(ab_inv)*exp(-a*b*r2_ab*ab_inv)
            res = max(res,prefac/(sqrt(ab_inv)*ab_inv))
         end do
      end do
   end function

   pure function cd_kappa_max(self) result (res)
   ! Return the largest kappa_cd used in the Lindh integrals.
      type(SHELL4_TYPE), IN :: self
      REAL :: res
      VEC(REAL,3) :: CD
      REAL :: d,d_cc,c,cd_inv,prefac,r2_cd
      INT :: dg,cg
      CD = self%d%position-self%c%position
      r2_cd = dot_product(CD,CD)
      res = ZERO
      do dg = 1,self%d%n_cc
         d = self%d%exponent(dg)
         d_cc = self%d%contraction(dg)
         do cg = 1,self%c%n_cc
            c = self%c%exponent(cg)
            cd_inv = ONE/(c+d)
            prefac = d_cc*self%c%contraction(cg)*cd_inv*sqrt(cd_inv)*exp(-c*d*r2_cd*cd_inv)
            res = max(res,prefac/(sqrt(cd_inv)*cd_inv))
         end do
      end do
   end function

   pure function skip_ERI_0(self) result (res)
   ! Whether the ERI block will be less than a cutoff value.
      type(SHELL4_TYPE), IN :: self
      BIN :: res
      res = (ab_kappa_max_(self)*cd_kappa_max_(self)<SHELL4_ERI_CUTOFF)
   end function

   pure function skip_ERI_1(self,cutoff) result (res)
   ! Whether the ERI block will be less than a cutoff value.
      type(SHELL4_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = (ab_kappa_max_(self)*cd_kappa_max_(self)<cutoff)
   end function


!  ======================
!  CADPAC-style integrals
!  ======================

   PURE subroutine make_ERI_ints(self,I)
   ! Make the ERI integral matrix, using Gauss-Hermite quadrature, like in
   ! CADPAC. This is not expected to be as efficient as Daniel's code, below!
   ! But probably much easier to understand.
      type(SHELL4_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: I
      MAT4(REAL,:,:,:,:), allocatable :: II
      type(GAUSSIAN4_TYPE) :: G
      VEC(REAL,4) :: c4
      INT :: a,b,c,d
      I = ZERO
      call create_(II,self%a%n_comp,self%b%n_comp,self%c%n_comp,self%d%n_comp)
      call set_l_(G%a,self%a%l)
      call set_l_(G%b,self%b%l)
      call set_l_(G%c,self%c%l)
      call set_l_(G%d,self%d%l)
      call set_position_(G%a,self%a%position)
      call set_position_(G%b,self%b%position)
      call set_position_(G%c,self%c%position)
      call set_position_(G%d,self%d%position)
      do a = 1,self%a%n_cc
         call set_exponent_(G%a,self%a%exponent(a))
         c4(1) = self%a%contraction(a)
         do b = 1,self%b%n_cc
            call set_exponent_(G%b,self%b%exponent(b))
            c4(2) = self%b%contraction(b)
            do c = 1,self%c%n_cc
               call set_exponent_(G%c,self%c%exponent(c))
               c4(3) = self%c%contraction(c)
               do d = 1,self%d%n_cc
                  call set_exponent_(G%d,self%d%exponent(d))
                  c4(4) = self%d%contraction(d)
                  call make_ERI_ints_(G,II)
                  I = I+product(c4)*II
               end do
            end do
         end do
      end do
      call destroy_(II)
   end subroutine

   PURE subroutine make_spin_orbit_ints_0(self,S,O)
   ! Make the spin same orbit integrals, "Sx", ... , and the
   ! spin other orbit integrals "Ox", ... , using
   ! Gauss-Hermite quadrature. Probably not the best implementation.
      type(SHELL4_TYPE), IN :: self
      MAT5(REAL,:,:,:,:,:), OUT :: S,O
      call make_spin_orbit_ints_(self,S(:,:,:,:,1),S(:,:,:,:,2),S(:,:,:,:,3),O(:,:,:,:,1),O(:,:,:,:,2),O(:,:,:,:,3))
   end subroutine

   PURE subroutine make_spin_orbit_ints_1(self,Sx,Sy,Sz,Ox,Oy,Oz)
   ! Make the spin same orbit integrals, "Sx", ... , and the
   ! spin other orbit integrals "Ox", ... , using
   ! Gauss-Hermite quadrature. Probably not the best implementation.
      type(SHELL4_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: Sx,Sy,Sz
      MAT4(REAL,:,:,:,:), OUT :: Ox,Oy,Oz
      MAT4(REAL,:,:,:,:), allocatable :: SSx,SSy,SSz
      MAT4(REAL,:,:,:,:), allocatable :: OOx,OOy,OOz
      type(GAUSSIAN4_TYPE) :: G
      INT :: a,b,c,d,na,nb,nc,nd
      REAL :: cc
      Sx = ZERO; Sy = ZERO; Sz = ZERO
      Ox = ZERO; Oy = ZERO; Oz = ZERO
      na = self%a%n_comp; nb = self%b%n_comp; nc = self%c%n_comp; nd = self%d%n_comp
      call create_(SSx,na,nb,nc,nd); call create_(SSy,na,nb,nc,nd); call create_(SSz,na,nb,nc,nd)
      call create_(OOx,na,nb,nc,nd); call create_(OOy,na,nb,nc,nd); call create_(OOz,na,nb,nc,nd)
      call set_l_(G%a,self%a%l); call set_position_(G%a,self%a%position)
      call set_l_(G%b,self%b%l); call set_position_(G%b,self%b%position)
      call set_l_(G%c,self%c%l); call set_position_(G%c,self%c%position)
      call set_l_(G%d,self%d%l); call set_position_(G%d,self%d%position)
      do a = 1,self%a%n_cc
         do b = 1,self%b%n_cc
            do c = 1,self%c%n_cc
               do d = 1,self%d%n_cc
                  call set_exponent_(G%a,self%a%exponent(a))
                  call set_exponent_(G%b,self%b%exponent(b))
                  call set_exponent_(G%c,self%c%exponent(c))
                  call set_exponent_(G%d,self%d%exponent(d))
                  call make_spin_orbit_ints_(G,SSx,SSy,SSz,OOx,OOy,OOz)
                  cc = self%a%contraction(a)*self%b%contraction(b)*self%c%contraction(c)*self%d%contraction(d)
                  Sx = Sx+SSx*cc
                  Sy = Sy+SSy*cc
                  Sz = Sz+SSz*cc
                  Ox = Ox+OOx*cc
                  Oy = Oy+OOy*cc
                  Oz = Oz+OOz*cc
               end do
            end do
         end do
      end do
      call destroy_(OOz); call destroy_(OOy); call destroy_(OOx)
      call destroy_(SSz); call destroy_(SSy); call destroy_(SSx)
   end subroutine

   PURE subroutine make_spin_spin_dipole_ints(self,Dxx,Dyy,Dzz,Dxy,Dxz,Dyz)
   ! Make the spin spin magnetic dipole integrals, "Dij"
   ! using Gauss-Hermite quadrature. For sure, not the
   ! best implementation, but where else will you get em', eh?
      type(SHELL4_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: Dxx,Dyy,Dzz
      MAT4(REAL,:,:,:,:), OUT :: Dxy,Dxz,Dyz
      MAT4(REAL,:,:,:,:), allocatable :: Mxx,Myy,Mzz
      MAT4(REAL,:,:,:,:), allocatable :: Mxy,Mxz,Myz
      type(GAUSSIAN4_TYPE) :: G
      INT :: a,b,c,d,na,nb,nc,nd
      REAL :: cc
      Dxx = ZERO; Dyy = ZERO; Dzz = ZERO
      Dxy = ZERO; Dxz = ZERO; Dyz = ZERO
      na = self%a%n_comp; nb = self%b%n_comp; nc = self%c%n_comp; nd = self%d%n_comp
      call create_(Mxx,na,nb,nc,nd); call create_(Myy,na,nb,nc,nd); call create_(Mzz,na,nb,nc,nd)
      call create_(Mxy,na,nb,nc,nd); call create_(Mxz,na,nb,nc,nd); call create_(Myz,na,nb,nc,nd)
      call set_l_(G%a,self%a%l); call set_position_(G%a,self%a%position)
      call set_l_(G%b,self%b%l); call set_position_(G%b,self%b%position)
      call set_l_(G%c,self%c%l); call set_position_(G%c,self%c%position)
      call set_l_(G%d,self%d%l); call set_position_(G%d,self%d%position)
      do a = 1,self%a%n_cc
         do b = 1,self%b%n_cc
            do c = 1,self%c%n_cc
               do d = 1,self%d%n_cc
                  call set_exponent_(G%a,self%a%exponent(a))
                  call set_exponent_(G%b,self%b%exponent(b))
                  call set_exponent_(G%c,self%c%exponent(c))
                  call set_exponent_(G%d,self%d%exponent(d))
                  call make_spin_spin_dipole_ints_(G,Mxx,Myy,Mzz,Mxy,Mxz,Myz)
                  cc = self%a%contraction(a)*self%b%contraction(b)*self%c%contraction(c)*self%d%contraction(d)
                  Dxx = Dxx+Mxx*cc
                  Dyy = Dyy+Myy*cc
                  Dzz = Dzz+Mzz*cc
                  Dxy = Dxy+Mxy*cc
                  Dxz = Dxz+Mxz*cc
                  Dyz = Dyz+Myz*cc
               end do
            end do
         end do
      end do
      call destroy_(Myz); call destroy_(Mxz); call destroy_(Mxy)
      call destroy_(Mzz); call destroy_(Myy); call destroy_(Mxx)
   end subroutine

   PURE subroutine make_ERI_derivatives(self,AA,BB,CC,DD)
   !
      type(SHELL4_TYPE), IN :: self
      MAT5(REAL,:,:,:,:,:), optional, OUT :: AA,BB,CC,DD
      MAT5(REAL,:,:,:,:,:), allocatable :: AX,BX,CX,DX
      type(GAUSSIAN4_TYPE) :: G
      INT :: a,b,c,d,na,nb,nc,nd
      REAL :: fac
      call set_l_(G%a,self%a%l); call set_position_(G%a,self%a%position)
      call set_l_(G%b,self%b%l); call set_position_(G%b,self%b%position)
      call set_l_(G%c,self%c%l); call set_position_(G%c,self%c%position)
      call set_l_(G%d,self%d%l); call set_position_(G%d,self%d%position)
      na = self%a%n_comp
      nb = self%b%n_comp
      nc = self%c%n_comp
      nd = self%d%n_comp
      if (present(AA)) then
         AA = ZERO
         call create_(AX,na,nb,nc,nd,3)
      end if
      if (present(BB)) then
         BB = ZERO
         call create_(BX,na,nb,nc,nd,3)
      end if
      if (present(CC)) then
         CC = ZERO
         call create_(CX,na,nb,nc,nd,3)
      end if
      if (present(DD)) then
         DD = ZERO
         call create_(DX,na,nb,nc,nd,3)
      end if
      do a = 1,self%a%n_cc
         do b = 1,self%b%n_cc
            do c = 1,self%c%n_cc
               do d = 1,self%d%n_cc
                  call set_exponent_(G%a,self%a%exponent(a))
                  call set_exponent_(G%b,self%b%exponent(b))
                  call set_exponent_(G%c,self%c%exponent(c))
                  call set_exponent_(G%d,self%d%exponent(d))
                  call make_ERI_derivatives_(G,AA=AX,BB=BX,CC=CX,DD=DX)
                  fac = self%a%contraction(a)*self%b%contraction(b)*self%c%contraction(c)*self%d%contraction(d)
                  if (present(AA)) AA = AA+AX*fac
                  if (present(BB)) BB = BB+BX*fac
                  if (present(CC)) CC = CC+CX*fac
                  if (present(DD)) DD = DD+DX*fac
               end do
            end do
         end do
      end do
   end subroutine


! =============================================================
!                   Roland Lindh-style integrals
!
! Electron repulsion integrals from Lindh, Ryu and Liu,
! J. Chem. Phys 95(8) 1991, 5889-5897.
!
! See also:
! Obara and Saika, J. Chem. Phys. 84(7), 1986, 3963-3974.
! Head-Gordon and Pople, J. Chem. Phys. 89(9), 1988, 5777-5786.
! =============================================================

   PURE subroutine get_ERI(self,abcd)
   ! Makes the (ab|cd) integrals, summed over the primitives
   ! (uses the transfer equation to make (ab|cd) from (es|fs))
      type(SHELL4_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: abcd
      MAT3(REAL,:,:,:), allocatable :: escd
      MAT(REAL,:,:), allocatable :: esfs
      INT :: eub,fub,ab_l_max,cd_l_max
      ab_l_max = max(self%a%l,self%b%l)-1
      cd_l_max = max(self%c%l,self%d%l)-1
      eub = n_comp_up_to_(self%a%l+self%b%l)-n_comp_up_to_(ab_l_max)
      fub = n_comp_up_to_(self%c%l+self%d%l)-n_comp_up_to_(cd_l_max)
      call create_(esfs,eub,fub)
      call make_esfs_(self,esfs)
      call create_(escd,eub,self%c%n_comp,self%d%n_comp)
      call transfer_cd_(self,esfs,escd)
      call transfer_ab_(self,escd,abcd)
      call destroy_(escd)
      call destroy_(esfs)
   end subroutine

   PURE subroutine make_esfs(self,esfs)
   ! Makes the initial (es|fs) integrals, summed over the primitives
      type(SHELL4_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: esfs
      MAT3(REAL,:,:,:), allocatable, target :: Ixa,Iya,Iza
      type(RYS_TYPE), allocatable :: rys
      MAT(REAL,:,:), PTR :: Ix,Iy,Iz
      VEC(REAL,3) :: At,Ct,P,Q,PA,QC,QP,AB,CD
      REAL :: zeta,eta,xx,zinv,rho,einv,rho_zinv,rho_einv
      REAL :: ce,cf,bb,ce1,kappa_ab,kappa_cd,normab,norm,r2ab,r2cd
      REAL :: a_cc,b_cc,c_cc,d_cc,a_ex,b_ex,c_ex,d_ex
      REAL :: t2,t2_rz,t2_re,half_zinv,half_einv,f1_bb,f1_cf
      INT :: ag,bg,cg,dg,nroots,eub,fub,dim1,dim2
      INT :: e,f,e1,f1,fp1,ep1,n,i,n_sum,ab_l_sum,cd_l_sum,ab_l_max,cd_l_max
      ab_l_sum = self%a%l+self%b%l
      cd_l_sum = self%c%l+self%d%l
      ab_l_max = max(self%a%l,self%b%l)-1
      cd_l_max = max(self%c%l,self%d%l)-1
      eub = n_comp_up_to_(ab_l_sum)-n_comp_up_to_(ab_l_max)
      fub = n_comp_up_to_(cd_l_sum)-n_comp_up_to_(cd_l_max)
      dim1 = ab_l_sum+1
      dim2 = cd_l_sum+1
      nroots = (dim1+dim2)/2

      ! number of elements to sum over
      n_sum = nroots*self%a%n_cc*self%b%n_cc*self%c%n_cc*self%d%n_cc
      call create_(Ixa,n_sum,dim1,dim2)
      call create_(Iya,n_sum,dim1,dim2)
      call create_(Iza,n_sum,dim1,dim2)
      AB = self%a%position-self%b%position
      CD = self%c%position-self%d%position
      r2ab = dot_product(AB,AB)
      r2cd = dot_product(CD,CD)

      ! Want position of shell1 with higher angular momentum.
      if (self%a%l>self%b%l) then; At = self%a%position
      else; At = self%b%position
      end if
      if (self%c%l>self%d%l) then; Ct = self%c%position
      else; Ct = self%d%position
      end if
      call create_(rys,nroots)
      i = 0
      do ag = 1,self%a%n_cc
         a_cc = self%a%contraction(ag)
         a_ex = self%a%exponent(ag)
         do bg = 1,self%b%n_cc
            b_cc = self%b%contraction(bg)
            b_ex = self%b%exponent(bg)
            zeta = a_ex+b_ex
            zinv = ONE/zeta
            half_zinv = HALF*zinv
            kappa_ab = exp(-a_ex*b_ex*r2ab*zinv)
            normab = TWOPI5ON2*b_cc*a_cc*zinv*sqrt(zinv)*kappa_ab
            P = (b_ex*self%b%position+a_ex*self%a%position)*zinv
            PA = P-At
            do cg = 1,self%c%n_cc
               c_cc = self%c%contraction(cg)
               c_ex = self%c%exponent(cg)
               do dg = 1,self%d%n_cc
                  d_cc = self%d%contraction(dg)
                  d_ex = self%d%exponent(dg)
                  eta = c_ex+d_ex
                  einv = ONE/eta
                  half_einv = HALF*einv
                  kappa_cd = exp(-c_ex*d_ex*r2cd*einv)
                  norm = normab*d_cc*c_cc*einv*sqrt(einv)*kappa_cd
                  Q = (d_ex*self%d%position+c_ex*self%c%position)*einv
                  QC = Q-Ct
                  rho = zeta*eta/(zeta+eta)
                  rho_zinv = rho*zinv
                  rho_einv = rho*einv
                  half_einv = HALF*einv
                  QP = Q-P
                  xx = rho*dot_product(QP,QP)
                  call get_weights_(rys,xx)
                  rys%weight(1:rys%n_roots) = rys%weight(1:rys%n_roots)*(norm*sqrt(rho))

            ! Now make the 2 dimensional integrals.
                  do n = 1,nroots
                     i = i+1
                     Ix => Ixa(i,:,:)
                     Iy => Iya(i,:,:)
                     Iz => Iza(i,:,:)
                     t2 = rys%root(n)
                     t2_rz = t2*rho_zinv
                     t2_re = t2*rho_einv
                     cf = (ONE-t2_re)*half_einv
                     ce = (ONE-t2_rz)*half_zinv
                     bb = t2_rz*half_einv
                     Ix(1,1) = ONE
                     Iy(1,1) = ONE
                     Iz(1,1) = ONE
                     if (ab_l_sum>0) then
                        Ix(2,1) = PA(1)+t2_rz*QP(1)
                        Iy(2,1) = PA(2)+t2_rz*QP(2)
                        Iz(2,1) = PA(3)+t2_rz*QP(3)
                        do e = 2,ab_l_sum
                           e1 = e-1
                           ep1 = e+1
                           ce1 = (e-1)*ce
                           Ix(ep1,1) = Ix(2,1)*Ix(e,1)+ce1*Ix(e1,1)
                           Iy(ep1,1) = Iy(2,1)*Iy(e,1)+ce1*Iy(e1,1)
                           Iz(ep1,1) = Iz(2,1)*Iz(e,1)+ce1*Iz(e1,1)
                        end do
                     end if
                     if (cd_l_sum>0) then
                        Ix(1,2) = QC(1)-t2_re*QP(1)
                        Iy(1,2) = QC(2)-t2_re*QP(2)
                        Iz(1,2) = QC(3)-t2_re*QP(3)
                        do f = 2,cd_l_sum
                           f1 = f-1
                           fp1 = f+1
                           f1_cf = (f-1)*cf
                           Ix(1,fp1) = Ix(1,2)*Ix(1,f)+f1_cf*Ix(1,f1)
                           Iy(1,fp1) = Iy(1,2)*Iy(1,f)+f1_cf*Iy(1,f1)
                           Iz(1,fp1) = Iz(1,2)*Iz(1,f)+f1_cf*Iz(1,f1)
                        end do
                        if (ab_l_sum>0) then
                           Ix(2,2) = Ix(1,2)*Ix(2,1)+bb
                           Iy(2,2) = Iy(1,2)*Iy(2,1)+bb
                           Iz(2,2) = Iz(1,2)*Iz(2,1)+bb
                           do f = 2,cd_l_sum
                              f1 = f-1
                              fp1 = f+1
                              f1_cf = (f-1)*cf
                              Ix(2,fp1) = Ix(1,2)*Ix(2,f)+f1_cf*Ix(2,f1)+bb*Ix(1,f)
                              Iy(2,fp1) = Iy(1,2)*Iy(2,f)+f1_cf*Iy(2,f1)+bb*Iy(1,f)
                              Iz(2,fp1) = Iz(1,2)*Iz(2,f)+f1_cf*Iz(2,f1)+bb*Iz(1,f)
                           end do
                        end if
                     end if
                     do e = 2,ab_l_sum
                        e1 = e-1
                        ep1 = e+1
                        ce1 = (e-1)*ce
                        do f = 2,cd_l_sum+1
                           f1 = f-1
                           f1_bb = (f-1)*bb
                           Ix(ep1,f) = Ix(2,1)*Ix(e,f)+ce1*Ix(e1,f)+f1_bb*Ix(e,f1)
                           Iy(ep1,f) = Iy(2,1)*Iy(e,f)+ce1*Iy(e1,f)+f1_bb*Iy(e,f1)
                           Iz(ep1,f) = Iz(2,1)*Iz(e,f)+ce1*Iz(e1,f)+f1_bb*Iz(e,f1)
                        end do
                     end do
                     Iz = Iz*rys%weight(n)
                  end do
               end do
            end do
         end do
      end do
      call destroy_(rys)
      call form_3d_ints_(self,Ixa,Iya,Iza,esfs,eub,fub,n_sum)
      call destroy_(Iza)
      call destroy_(Iya)
      call destroy_(Ixa)
   end subroutine

   PURE subroutine form_3d_ints(self,Ix,Iy,Iz,esfs,eub,fub,n_sum)
   ! Forms (es|fs) from the two dimensional integrals, summed over primitives.
   ! This is the main routine, all the others are specialised and may break if
   ! given the wrong shell4.
      type(SHELL4_TYPE), IN :: self
      MAT3(REAL,:,:,:), IN :: Ix,Iy,Iz
      MAT(REAL,:,:), OUT :: esfs
      INT, IN :: eub,fub,n_sum
      if (min(self%a%l,self%b%l)<3 AND min(self%c%l,self%d%l)<3) then

         ! s or p or d
         call form_3d_ints_no_rm_(self,Ix,Iy,Iz,esfs)
      else
         call form_3d_ints_rm_(self,Ix,Iy,Iz,esfs,eub,fub,n_sum)
      end if
   end subroutine

   PURE subroutine form_3d_ints_rm(self,Ix,Iy,Iz,esfs,eub,fub,n_sum)
   ! Forms the three dimensional integrals from the two dimensional integrals,
   ! summed over primitives.
   ! This version uses the reduced multiplication scheme.
   ! WARNING: this routine has a bug for l>3
      type(SHELL4_TYPE), IN :: self
      MAT3(REAL,:,:,:), IN :: Ix,Iy,Iz
      MAT(REAL,:,:), OUT :: esfs
      INT, IN :: eub,fub,n_sum
      VEC(INT,:), allocatable :: e_x,e_y,e_z
      VEC(INT,:), allocatable :: f_x,f_y,f_z
      VEC(INT,:), allocatable :: ii_e_ivec,ii_f_ivec
      MAT3(REAL,:,:,:), allocatable :: Ief
      INT :: e,f,zf,yf,ze,ye,iie,iif,fe,le,ff,lf
      INT :: dime,dimf,dime1,dimf1,dime2,dimf2
      dime = self%a%l+self%b%l+1
      dime1 = dime+1
      dime2 = 2*dime1+1
      dimf = self%c%l+self%d%l+1
      dimf1 = dimf+1
      dimf2 = 2*dimf1+1
      fe = n_comp_up_to_(max(self%a%l,self%b%l)-1)+1
      ff = n_comp_up_to_(max(self%c%l,self%d%l)-1)+1
      le = n_comp_up_to_(self%a%l+self%b%l)
      lf = n_comp_up_to_(self%c%l+self%d%l)
      e_x = nx(fe:le)
      e_y = ny(fe:le)
      e_z = nz(fe:le)
      f_x = nx(ff:lf)
      f_y = ny(ff:lf)
      f_z = nz(ff:lf)
      call create_(ii_e_ivec,eub)
      do e = 1,eub
         ze = e_z(e)
         ii_e_ivec(e) = -dime1+ze*(dime2-ze)/2+e_y(e)
      end do
      call create_(ii_f_ivec,fub)
      do f = 1,fub
         zf = f_z(f)
         ii_f_ivec(f) = -dimf1+zf*(dimf2-zf)/2+f_y(f)
      end do

      ! Apply reduced multiplication scheme to Iy and Iz, store in triangle.
      call create_(Ief,n_sum,dime*dime1/2,dimf*dimf1/2)
      iif = 0
      do zf = 1,dimf
         do yf = 1,dimf1-zf
            iif = iif+1
            iie = 0
            do ze = 1,dime
               do ye = 1,dime1-ze
                  iie = iie+1
                  Ief(:,iie,iif) = Iy(:,ye,yf)*Iz(:,ze,zf)
               end do
            end do
         end do
      end do

      ! Now add in the Ix 2d integrals and sum over contractions and roots
      esfs = sum(Ix(:,e_x,f_x)*Ief(:,ii_e_ivec,ii_f_ivec),dim=1)
      call destroy_(Ief)
      call destroy_(ii_f_ivec)
      call destroy_(ii_e_ivec)
   end subroutine

   PURE subroutine form_3d_ints_no_rm(self,Ix,Iy,Iz,esfs)
   ! Forms the three dimensional integrals from the two dimensional integrals,
   ! summed over primitives.
   ! This version does not use the reduced multiplication scheme.
      type(SHELL4_TYPE), IN :: self
      MAT3(REAL,:,:,:), IN :: Ix,Iy,Iz
      MAT(REAL,:,:), OUT :: esfs
      INT :: fe,le,ff,lf
      VEC(INT,:), allocatable :: e_x,e_y,e_z
      VEC(INT,:), allocatable :: f_x,f_y,f_z
      fe = n_comp_up_to_(max(self%a%l,self%b%l)-1)+1
      ff = n_comp_up_to_(max(self%c%l,self%d%l)-1)+1
      le = n_comp_up_to_(self%a%l+self%b%l)
      lf = n_comp_up_to_(self%c%l+self%d%l)
      e_x = nx(fe:le)
      e_y = ny(fe:le)
      e_z = nz(fe:le)
      f_x = nx(ff:lf)
      f_y = ny(ff:lf)
      f_z = nz(ff:lf)
      esfs = sum(Ix(:,e_x,f_x)*Iy(:,e_y,f_y)*Iz(:,e_z,f_z),dim=1)
   end subroutine

   PURE subroutine transfer_cd(self,esfs,escd)
   ! Applies the transfer equation to (es|fs) to give (es|cd)
      type(SHELL4_TYPE), IN :: self
      MAT(REAL,:,:), IN :: esfs
      MAT3(REAL,:,:,:), OUT :: escd
      if (self%c%l>self%d%l) then
         call transfer_l_c_highest_(self,esfs,escd)
      else
         call transfer_l_d_highest_(self,esfs,escd)
      end if
   end subroutine

   PURE subroutine transfer_ab_0(self,escd,abcd)
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
      type(SHELL4_TYPE), IN :: self
      MAT3(REAL,:,:,:), IN :: escd
      MAT4(REAL,:,:,:,:), OUT :: abcd
      if (self%a%l>self%b%l) then
         call transfer_l_a_highest_(self,escd,abcd)
      else
         call transfer_l_b_highest_(self,escd,abcd)
      end if
   end subroutine

   PURE subroutine transfer_ab_1(self,escd,abcd)
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
      type(SHELL4_TYPE), IN :: self
      MAT(REAL,:,:), IN :: escd
      MAT3(REAL,:,:,:), OUT :: abcd
      if (self%a%l>self%b%l) then
         call transfer_l_a_highest_(self,escd,abcd)
      else
         call transfer_l_b_highest_(self,escd,abcd)
      end if
   end subroutine

   PURE subroutine transfer_l_c_highest(self,esfs,escd)
   ! Applies the transfer equation to (es|fs) to give (es|cd)
      type(SHELL4_TYPE), IN :: self
      MAT(REAL,:,:), IN :: esfs
      MAT3(REAL,:,:,:), OUT :: escd
      MAT3(REAL,:,:,:), allocatable :: int_new,int_old
      MAT3(INT,:,:,:), allocatable :: index_c,index_d
      MAT(INT,:,:), allocatable :: components,components_c,components_d
      VEC(INT,:), allocatable :: comp_to_use,component_to_use
      VEC(REAL,3) :: CD
      INT :: c,d,c1,c2,c3,d1,ld,cub,dub,ab_l_max
      INT :: cx,cy,cz,dx,dy,dz,j,clb,dlb,dob,e,eub
      REAL :: CDi,CDx,CDy,CDz,esfs_ec
      select case (self%d%l)

      case (0)
         escd(:,:,1) = esfs

      case (1)
         ab_l_max = max(self%a%l,self%b%l)-1
         clb = n_comp_up_to_(self%c%l-1)
         cub = self%c%n_comp
         eub = n_comp_up_to_(self%a%l+self%b%l)-n_comp_up_to_(ab_l_max)
         CD = self%c%position-self%d%position
         components = nn(:,clb+1:clb+cub)
         index_c = index_of
         CDx = CD(1)
         CDy = CD(2)
         CDz = CD(3)
         do c = 1,cub
            cx = components(1,c)
            cy = components(2,c)
            cz = components(3,c)
            c1 = index_c(cx+1,cy,cz)-clb
            c2 = index_c(cx,cy+1,cz)-clb
            c3 = index_c(cx,cy,cz+1)-clb
            do e = 1,eub
               esfs_ec = esfs(e,c)
               escd(e,c,1) = esfs(e,c1)+CDx*esfs_ec
               escd(e,c,2) = esfs(e,c2)+CDy*esfs_ec
               escd(e,c,3) = esfs(e,c3)+CDz*esfs_ec
            end do
         end do

      case default
         ab_l_max = max(self%a%l,self%b%l)-1
         clb = n_comp_up_to_(self%c%l-1)
         eub = n_comp_up_to_(self%a%l+self%b%l)-n_comp_up_to_(ab_l_max)
         CD = self%c%position-self%d%position
         cub = n_comp_up_to_((self%c%l+self%d%l)-1)-clb
         comp_to_use = first_nonzero
         components = nn
         components_c = nn(:,clb+1:clb+cub)
         index_c = index_of
         index_d = index_of
         allocate(int_new(eub,cub,3))
         CDx = CD(1)
         CDy = CD(2)
         CDz = CD(3)
         do c = 1,cub
            cx = components_c(1,c)
            cy = components_c(2,c)
            cz = components_c(3,c)
            c1 = index_c(cx+1,cy,cz)-clb
            c2 = index_c(cx,cy+1,cz)-clb
            c3 = index_c(cx,cy,cz+1)-clb
            do e = 1,eub
               esfs_ec = esfs(e,c)
               int_new(e,c,1) = esfs(e,c1)+CDx*esfs_ec
               int_new(e,c,2) = esfs(e,c2)+CDy*esfs_ec
               int_new(e,c,3) = esfs(e,c3)+CDz*esfs_ec
            end do
         end do
         dlb = 1
         do ld = 2,self%d%l-1
            dob = dlb
            dlb = n_comp_up_to_(ld-1)
            dub = n_comp_(ld)
            cub = n_comp_up_to_((self%c%l+self%d%l)-ld)-clb
            component_to_use = comp_to_use(dlb+1:dlb+dub)
            components_d = components(:,dlb+1:dlb+dub)
            call move_alloc(int_new,int_old)
            allocate(int_new(eub,cub,dub))
            do d = 1,dub
               dx = components_d(1,d)
               dy = components_d(2,d)
               dz = components_d(3,d)
               j = component_to_use(d)
               call subtract_from_component(dx,dy,dz,j)
               d1 = index_d(dx,dy,dz)-dob
               CDi = CD(j)
               do c = 1,cub
                  cx = components_c(1,c)
                  cy = components_c(2,c)
                  cz = components_c(3,c)
                  call add_to_component(cx,cy,cz,j)
                  c1 = index_c(cx,cy,cz)-clb
                  int_new(:,c,d) = int_old(:,c1,d1)+CDi*int_old(:,c,d1)
               end do
            end do
            deallocate(int_old)
         end do
         dob = dlb
         dlb = n_comp_up_to_(self%d%l-1)
         dub = self%d%n_comp
         cub = self%c%n_comp
         component_to_use = comp_to_use(dlb+1:dlb+dub)
         components_d = components(:,dlb+1:dlb+dub)
         call move_alloc(int_new,int_old)
         do d = 1,dub
            dx = components_d(1,d)
            dy = components_d(2,d)
            dz = components_d(3,d)
            j = component_to_use(d)
            call subtract_from_component(dx,dy,dz,j)
            d1 = index_d(dx,dy,dz)-dob
            CDi = CD(j)
            do c = 1,cub
               cx = components_c(1,c)
               cy = components_c(2,c)
               cz = components_c(3,c)
               call add_to_component(cx,cy,cz,j)
               c1 = index_c(cx,cy,cz)-clb
               escd(:,c,d) = int_old(:,c1,d1)+CDi*int_old(:,c,d1)
            end do
         end do
         deallocate(int_old)
      end select
   end subroutine

   PURE subroutine transfer_l_d_highest(self,esfs,escd)
   ! Applies the transfer equation to (es|fs) to give (es|cd)
      type(SHELL4_TYPE), IN :: self
      MAT(REAL,:,:), IN :: esfs
      MAT3(REAL,:,:,:), OUT :: escd
      MAT3(REAL,:,:,:), allocatable :: int_new,int_old
      MAT3(INT,:,:,:), allocatable :: index_c,index_d
      MAT(INT,:,:), allocatable :: components,components_c,components_d
      VEC(INT,:), allocatable :: comp_to_use,component_to_use
      VEC(REAL,3) :: DC
      INT :: c,d,c1,d1,d2,d3,lc,cub,dub,ab_l_max
      INT :: cx,cy,cz,dx,dy,dz,j,clb,dlb,cob,e,eub
      REAL :: DCi,DCx,DCy,DCz,esfs_ed
      select case (self%c%l)

      case (0)
         escd(:,1,:) = esfs

      case (1)
         ab_l_max = max(self%a%l,self%b%l)-1
         dlb = n_comp_up_to_(self%d%l-1)
         dub = self%d%n_comp
         eub = n_comp_up_to_(self%a%l+self%b%l)-n_comp_up_to_(ab_l_max)
         DC = self%d%position-self%c%position
         components = nn(:,dlb+1:dlb+dub)
         index_d = index_of
         DCx = DC(1)
         DCy = DC(2)
         DCz = DC(3)
         do d = 1,dub
            dx = components(1,d)
            dy = components(2,d)
            dz = components(3,d)
            d1 = index_d(dx+1,dy,dz)-dlb
            d2 = index_d(dx,dy+1,dz)-dlb
            d3 = index_d(dx,dy,dz+1)-dlb
            do e = 1,eub
               esfs_ed = esfs(e,d)
               escd(e,1,d) = esfs(e,d1)+DCx*esfs_ed
               escd(e,2,d) = esfs(e,d2)+DCy*esfs_ed
               escd(e,3,d) = esfs(e,d3)+DCz*esfs_ed
            end do
         end do

      case default
         ab_l_max = max(self%a%l,self%b%l)-1
         dlb = n_comp_up_to_(self%d%l-1)
         eub = n_comp_up_to_(self%a%l+self%b%l)-n_comp_up_to_(ab_l_max)
         DC = self%d%position-self%c%position
         dub = n_comp_up_to_(self%c%l+self%d%l-1)-dlb
         comp_to_use = first_nonzero
         components = nn
         components_d = nn(:,dlb+1:dlb+dub)
         index_c = index_of
         index_d = index_of
         allocate(int_new(eub,dub,3))
         DCx = DC(1)
         DCy = DC(2)
         DCz = DC(3)
         do d = 1,dub
            dx = components_d(1,d)
            dy = components_d(2,d)
            dz = components_d(3,d)
            d1 = index_d(dx+1,dy,dz)-dlb
            d2 = index_d(dx,dy+1,dz)-dlb
            d3 = index_d(dx,dy,dz+1)-dlb
            do e = 1,eub
               esfs_ed = esfs(e,d)
               int_new(e,d,1) = esfs(e,d1)+DCx*esfs_ed
               int_new(e,d,2) = esfs(e,d2)+DCy*esfs_ed
               int_new(e,d,3) = esfs(e,d3)+DCz*esfs_ed
            end do
         end do
         clb = 1
         do lc = 2,self%c%l-1
            cob = clb
            clb = n_comp_up_to_(lc-1)
            cub = n_comp_(lc)
            dub = n_comp_up_to_((self%c%l+self%d%l)-lc)-dlb
            component_to_use = comp_to_use(clb+1:clb+cub)
            components_c = components(:,clb+1:clb+cub)
            call move_alloc(int_new,int_old)
            allocate(int_new(eub,dub,cub))
            do c = 1,cub
               cx = components_c(1,c)
               cy = components_c(2,c)
               cz = components_c(3,c)
               j = component_to_use(c)
               call subtract_from_component(cx,cy,cz,j)
               c1 = index_c(cx,cy,cz)-cob
               DCi = DC(j)
               do d = 1,dub
                  dx = components_d(1,d)
                  dy = components_d(2,d)
                  dz = components_d(3,d)
                  call add_to_component(dx,dy,dz,j)
                  d1 = index_d(dx,dy,dz)-dlb
                  int_new(:,d,c) = int_old(:,d1,c1)+DCi*int_old(:,d,c1)
               end do
            end do
            deallocate(int_old)
         end do
         cob = clb
         clb = n_comp_up_to_(self%c%l-1)
         cub = self%c%n_comp
         dub = self%d%n_comp
         component_to_use = comp_to_use(clb+1:clb+cub)
         components_c = components(:,clb+1:clb+cub)
         call move_alloc(int_new,int_old)
         do c = 1,cub
            cx = components_c(1,c)
            cy = components_c(2,c)
            cz = components_c(3,c)
            j = component_to_use(c)
            call subtract_from_component(cx,cy,cz,j)
            c1 = index_c(cx,cy,cz)-cob
            DCi = DC(j)
            do d = 1,dub
               dx = components_d(1,d)
               dy = components_d(2,d)
               dz = components_d(3,d)
               call add_to_component(dx,dy,dz,j)
               d1 = index_d(dx,dy,dz)-dlb
               escd(:,c,d) = int_old(:,d1,c1)+DCi*int_old(:,d,c1)
            end do
         end do
         deallocate(int_old)
      end select
   end subroutine

   PURE subroutine transfer_l_a_highest_0(self,escd,abcd)
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
      type(SHELL4_TYPE), IN :: self
      MAT3(REAL,:,:,:), IN :: escd
      MAT4(REAL,:,:,:,:), OUT :: abcd
      MAT4(REAL,:,:,:,:), allocatable :: int_new,int_old
      MAT3(INT,:,:,:), allocatable :: index_a,index_b
      MAT(INT,:,:), allocatable :: components,components_a,components_b
      VEC(INT,:), allocatable :: comp_to_use,component_to_use
      VEC(REAL,3) :: AB
      INT :: a,b,c,d,a1,a2,a3,b1,lb,aub,bub,cub,dub
      INT :: ax,ay,az,bx,by,bz,j,alb,blb,bob
      REAL :: ABi,ABx,ABy,ABz,escd_acd
      select case (self%b%l)
      case (0)
         abcd(:,1,:,:) = escd

      case (1)
         alb = n_comp_up_to_(self%a%l-1)
         aub = self%a%n_comp
         cub = self%c%n_comp
         dub = self%d%n_comp
         AB = self%a%position-self%b%position
         components = nn(:,alb+1:alb+aub)
         index_a = index_of
         ABx = AB(1)
         ABy = AB(2)
         ABz = AB(3)
         do a = 1,aub
            ax = components(1,a)
            ay = components(2,a)
            az = components(3,a)
            a1 = index_a(ax+1,ay,az)-alb
            a2 = index_a(ax,ay+1,az)-alb
            a3 = index_a(ax,ay,az+1)-alb
            do d = 1,dub
               do c = 1,cub
                  escd_acd = escd(a,c,d)
                  abcd(a,1,c,d) = escd(a1,c,d)+ABx*escd_acd
                  abcd(a,2,c,d) = escd(a2,c,d)+ABy*escd_acd
                  abcd(a,3,c,d) = escd(a3,c,d)+ABz*escd_acd
               end do
            end do
         end do

      case default
         alb = n_comp_up_to_(self%a%l-1)
         AB = self%a%position-self%b%position
         aub = n_comp_up_to_((self%a%l+self%b%l)-1)-alb
         bub = self%b%n_comp
         cub = self%c%n_comp
         dub = self%d%n_comp
         comp_to_use = first_nonzero
         components = nn
         components_a = nn(:,alb+1:alb+aub)
         index_a = index_of
         index_b = index_of
         allocate(int_new(cub,dub,bub,aub))
         ABx = AB(1)
         ABy = AB(2)
         ABz = AB(3)
         do a = 1,aub
            ax = components_a(1,a)
            ay = components_a(2,a)
            az = components_a(3,a)
            a1 = index_a(ax+1,ay,az)-alb
            a2 = index_a(ax,ay+1,az)-alb
            a3 = index_a(ax,ay,az+1)-alb
            do d = 1,dub
               do c = 1,cub
                  escd_acd = escd(a,c,d)
                  int_new(c,d,1,a) = escd(a1,c,d)+ABx*escd_acd
                  int_new(c,d,2,a) = escd(a2,c,d)+ABy*escd_acd
                  int_new(c,d,3,a) = escd(a3,c,d)+ABz*escd_acd
               end do
            end do
         end do
         blb = 1
         do lb = 2,self%b%l-1
            bob = blb
            blb = n_comp_up_to_(lb-1)
            bub = n_comp_(lb)
            aub = n_comp_up_to_((self%a%l+self%b%l)-lb)-alb
            component_to_use = comp_to_use(blb+1:blb+bub)
            components_b = components(:,blb+1:blb+bub)
            call move_alloc(int_new,int_old)
            allocate(int_new(cub,dub,bub,aub))
            do b = 1,bub
               bx = components_b(1,b)
               by = components_b(2,b)
               bz = components_b(3,b)
               j = component_to_use(b)
               call subtract_from_component(bx,by,bz,j)
               b1 = index_b(bx,by,bz)-bob
               ABi = AB(j)
               do a = 1,aub
                  ax = components_a(1,a)
                  ay = components_a(2,a)
                  az = components_a(3,a)
                  call add_to_component(ax,ay,az,j)
                  a1 = index_a(ax,ay,az)-alb
                  int_new(:,:,b,a) = int_old(:,:,b1,a1)+ABi*int_old(:,:,b1,a)
               end do
            end do
            deallocate(int_old)
         end do
         bob = blb
         blb = n_comp_up_to_(self%b%l-1)
         bub = self%b%n_comp
         aub = self%a%n_comp
         component_to_use = comp_to_use(blb+1:blb+bub)
         components_b = components(:,blb+1:blb+bub)
         call move_alloc(int_new,int_old)
         do b = 1,bub
            bx = components_b(1,b)
            by = components_b(2,b)
            bz = components_b(3,b)
            j = component_to_use(b)
            call subtract_from_component(bx,by,bz,j)
            b1 = index_b(bx,by,bz)-bob
            ABi = AB(j)
            do a = 1,aub
               ax = components_a(1,a)
               ay = components_a(2,a)
               az = components_a(3,a)
               call add_to_component(ax,ay,az,j)
               a1 = index_a(ax,ay,az)-alb
               abcd(a,b,:,:) = int_old(:,:,b1,a1)+ABi*int_old(:,:,b1,a)
            end do
         end do
         deallocate(int_old)
      end select
   end subroutine

   PURE subroutine transfer_l_b_highest_0(self,escd,abcd)
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
      type(SHELL4_TYPE), IN :: self
      MAT3(REAL,:,:,:), IN :: escd
      MAT4(REAL,:,:,:,:), OUT :: abcd
      MAT4(REAL,:,:,:,:), allocatable :: int_new,int_old
      MAT(INT,:,:), allocatable :: components,components_a,components_b
      MAT3(INT,:,:,:), allocatable :: index_a,index_b
      VEC(INT,:), allocatable :: comp_to_use,component_to_use
      VEC(REAL,3) :: BA
      INT :: a,b,c,d,a1,b1,b2,b3,la,aub,bub,cub,dub
      INT :: ax,ay,az,bx,by,bz,j,alb,blb,aob
      REAL :: BAi,BAx,BAy,BAz,escd_bcd
      select case (self%a%l)

      case (0)
         abcd(1,:,:,:) = escd

      case (1)
         blb = n_comp_up_to_(self%b%l-1)
         bub = self%b%n_comp
         cub = self%c%n_comp
         dub = self%d%n_comp
         BA = self%b%position-self%a%position
         components = nn(:,blb+1:blb+bub)
         index_b = index_of
         BAx = BA(1)
         BAy = BA(2)
         BAz = BA(3)
         do b = 1,bub
            bx = components(1,b)
            by = components(2,b)
            bz = components(3,b)
            b1 = index_b(bx+1,by,bz)-blb
            b2 = index_b(bx,by+1,bz)-blb
            b3 = index_b(bx,by,bz+1)-blb
            do d = 1,dub
               do c = 1,cub
                  escd_bcd = escd(b,c,d)
                  abcd(1,b,c,d) = escd(b1,c,d)+BAx*escd_bcd
                  abcd(2,b,c,d) = escd(b2,c,d)+BAy*escd_bcd
                  abcd(3,b,c,d) = escd(b3,c,d)+BAz*escd_bcd
               end do
            end do
         end do

      case default
         blb = n_comp_up_to_(self%b%l-1)
         BA = self%b%position-self%a%position
         bub = n_comp_up_to_((self%a%l+self%b%l)-1)-blb
         aub = self%a%n_comp
         cub = self%c%n_comp
         dub = self%d%n_comp
         comp_to_use = first_nonzero
         components = nn
         components_b = nn(:,blb+1:blb+bub)
         index_b = index_of
         index_a = index_of
         allocate(int_new(cub,dub,bub,aub))
         BAx = BA(1)
         BAy = BA(2)
         BAz = BA(3)
         do b = 1,bub
            bx = components_b(1,b)
            by = components_b(2,b)
            bz = components_b(3,b)
            b1 = index_b(bx+1,by,bz)-blb
            b2 = index_b(bx,by+1,bz)-blb
            b3 = index_b(bx,by,bz+1)-blb
            do c = 1,cub
               do d = 1,dub
                  escd_bcd = escd(b,c,d)
                  int_new(c,d,b,1) = escd(b1,c,d)+BAx*escd_bcd
                  int_new(c,d,b,2) = escd(b2,c,d)+BAy*escd_bcd
                  int_new(c,d,b,3) = escd(b3,c,d)+BAz*escd_bcd
               end do
            end do
         end do
         alb = 1
         do la = 2,self%a%l-1
            aob = alb
            alb = n_comp_up_to_(la-1)
            aub = n_comp_(la)
            bub = n_comp_up_to_((self%a%l+self%b%l)-la)-blb
            component_to_use = comp_to_use(alb+1:alb+aub)
            components_a = components(:,alb+1:alb+aub)
            call move_alloc(int_new,int_old)
            allocate(int_new(cub,dub,bub,aub))
            do a = 1,aub
               ax = components_a(1,a)
               ay = components_a(2,a)
               az = components_a(3,a)
               j = component_to_use(a)
               call subtract_from_component(ax,ay,az,j)
               a1 = index_a(ax,ay,az)-aob
               BAi = BA(j)
               do b = 1,bub
                  bx = components_b(1,b)
                  by = components_b(2,b)
                  bz = components_b(3,b)
                  call add_to_component(bx,by,bz,j)
                  b1 = index_b(bx,by,bz)-blb
                  int_new(:,:,b,a) = int_old(:,:,b1,a1)+BAi*int_old(:,:,b,a1)
               end do
            end do
            deallocate(int_old)
         end do
         aob = alb
         alb = n_comp_up_to_(self%a%l-1)
         aub = self%a%n_comp
         bub = self%b%n_comp
         component_to_use = comp_to_use(alb+1:alb+aub)
         components_a = components(:,alb+1:alb+aub)
         call move_alloc(int_new,int_old)
         do a = 1,aub
            ax = components_a(1,a)
            ay = components_a(2,a)
            az = components_a(3,a)
            j = component_to_use(a)
            call subtract_from_component(ax,ay,az,j)
            a1 = index_a(ax,ay,az)-aob
            BAi = BA(j)
            do b = 1,bub
               bx = components_b(1,b)
               by = components_b(2,b)
               bz = components_b(3,b)
               call add_to_component(bx,by,bz,j)
               b1 = index_b(bx,by,bz)-blb
               abcd(a,b,:,:) = int_old(:,:,b1,a1)+BAi*int_old(:,:,b,a1)
            end do
         end do
         deallocate(int_old)
      end select
   end subroutine

   PURE subroutine transfer_l_a_highest_1(self,escd,abcd)
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
      type(SHELL4_TYPE), IN :: self
      MAT(REAL,:,:), IN :: escd
      MAT3(REAL,:,:,:), OUT :: abcd
      MAT3(REAL,:,:,:), allocatable :: int_new,int_old
      MAT(INT,:,:), allocatable :: components,components_a,components_b
      MAT3(INT,:,:,:), allocatable :: index_a,index_b
      VEC(INT,:), allocatable :: comp_to_use,component_to_use
      VEC(REAL,3) :: AB
      INT :: a,b,f,a1,a2,a3,b1,lb,aub,bub,fub
      INT :: ax,ay,az,bx,by,bz,j,alb,blb,bob
      REAL :: ABi,ABx,ABy,ABz,escd_acd
      select case (self%b%l)

      case (0)
         abcd(:,1,:) = escd

      case (1)
         alb = n_comp_up_to_(self%a%l-1)
         aub = self%a%n_comp
         fub = size(escd,2)
         AB = self%a%position-self%b%position
         components = nn(:,alb+1:alb+aub)
         index_a = index_of
         ABx = AB(1)
         ABy = AB(2)
         ABz = AB(3)
         do a = 1,aub
            ax = components(1,a)
            ay = components(2,a)
            az = components(3,a)
            a1 = index_a(ax+1,ay,az)-alb
            a2 = index_a(ax,ay+1,az)-alb
            a3 = index_a(ax,ay,az+1)-alb
            do f = 1,fub
               escd_acd = escd(a,f)
               abcd(a,1,f) = escd(a1,f)+ABx*escd_acd
               abcd(a,2,f) = escd(a2,f)+ABy*escd_acd
               abcd(a,3,f) = escd(a3,f)+ABz*escd_acd
            end do
         end do

      case default
         alb = n_comp_up_to_(self%a%l-1)
         AB = self%a%position-self%b%position
         aub = n_comp_up_to_((self%a%l+self%b%l)-1)-alb
         bub = self%b%n_comp
         fub = size(escd,2)
         comp_to_use = first_nonzero
         components = nn
         components_a = nn(:,alb+1:alb+aub)
         index_a = index_of
         index_b = index_of
         allocate(int_new(fub,bub,aub))
         ABx = AB(1)
         ABy = AB(2)
         ABz = AB(3)
         do a = 1,aub
            ax = components_a(1,a)
            ay = components_a(2,a)
            az = components_a(3,a)
            a1 = index_a(ax+1,ay,az)-alb
            a2 = index_a(ax,ay+1,az)-alb
            a3 = index_a(ax,ay,az+1)-alb
            do f = 1,fub
               escd_acd = escd(a,f)
               int_new(f,1,a) = escd(a1,f)+ABx*escd_acd
               int_new(f,2,a) = escd(a2,f)+ABy*escd_acd
               int_new(f,3,a) = escd(a3,f)+ABz*escd_acd
            end do
         end do
         blb = 1
         do lb = 2,self%b%l-1
            bob = blb
            blb = n_comp_up_to_(lb-1)
            bub = n_comp_(lb)
            aub = n_comp_up_to_((self%a%l+self%b%l)-lb)-alb
            component_to_use = comp_to_use(blb+1:blb+bub)
            components_b = components(:,blb+1:blb+bub)
            call move_alloc(int_new,int_old)
            allocate(int_new(fub,bub,aub))
            do b = 1,bub
               bx = components_b(1,b)
               by = components_b(2,b)
               bz = components_b(3,b)
               j = component_to_use(b)
               call subtract_from_component(bx,by,bz,j)
               b1 = index_b(bx,by,bz)-bob
               ABi = AB(j)
               do a = 1,aub
                  ax = components_a(1,a)
                  ay = components_a(2,a)
                  az = components_a(3,a)
                  call add_to_component(ax,ay,az,j)
                  a1 = index_a(ax,ay,az)-alb
                  int_new(:,b,a) = int_old(:,b1,a1)+ABi*int_old(:,b1,a)
               end do
            end do
            deallocate(int_old)
         end do
         bob = blb
         blb = n_comp_up_to_(self%b%l-1)
         bub = self%b%n_comp
         aub = self%a%n_comp
         component_to_use = comp_to_use(blb+1:blb+bub)
         components_b = components(:,blb+1:blb+bub)
         call move_alloc(int_new,int_old)
         do b = 1,bub
            bx = components_b(1,b)
            by = components_b(2,b)
            bz = components_b(3,b)
            j = component_to_use(b)
            call subtract_from_component(bx,by,bz,j)
            b1 = index_b(bx,by,bz)-bob
            ABi = AB(j)
            do a = 1,aub
               ax = components_a(1,a)
               ay = components_a(2,a)
               az = components_a(3,a)
               call add_to_component(ax,ay,az,j)
               a1 = index_a(ax,ay,az)-alb
               do f = 1,fub
                  abcd(a,b,f) = int_old(f,b1,a1)+ABi*int_old(f,b1,a)
               end do
            end do
         end do
         deallocate(int_old)
      end select
   end subroutine

   PURE subroutine transfer_l_b_highest_1(self,escd,abcd)
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
      type(SHELL4_TYPE), IN :: self
      MAT(REAL,:,:), IN :: escd
      MAT3(REAL,:,:,:), OUT :: abcd
      MAT3(REAL,:,:,:), allocatable :: int_new,int_old
      MAT(INT,:,:), allocatable :: components,components_a,components_b
      VEC(INT,:), allocatable :: comp_to_use,component_to_use
      MAT3(INT,:,:,:), allocatable :: index_a,index_b
      VEC(REAL,3) :: BA
      INT :: a,b,f,a1,b1,b2,b3,la,aub,bub,fub
      INT :: ax,ay,az,bx,by,bz,j,alb,blb,aob
      REAL :: BAi,BAx,BAy,BAz,escd_bcd
      select case (self%a%l)

      case (0)
         abcd(1,:,:) = escd

      case (1)
         blb = n_comp_up_to_(self%b%l-1)
         bub = self%b%n_comp
         fub = size(escd,2)
         BA = self%b%position-self%a%position
         components = nn(:,blb+1:blb+bub)
         index_b = index_of
         BAx = BA(1)
         BAy = BA(2)
         BAz = BA(3)
         do b = 1,bub
            bx = components(1,b)
            by = components(2,b)
            bz = components(3,b)
            b1 = index_b(bx+1,by,bz)-blb
            b2 = index_b(bx,by+1,bz)-blb
            b3 = index_b(bx,by,bz+1)-blb
            do f = 1,fub
               escd_bcd = escd(b,f)
               abcd(1,b,f) = escd(b1,f)+BAx*escd_bcd
               abcd(2,b,f) = escd(b2,f)+BAy*escd_bcd
               abcd(3,b,f) = escd(b3,f)+BAz*escd_bcd
            end do
         end do

      case default
         blb = n_comp_up_to_(self%b%l-1)
         BA = self%b%position-self%a%position
         bub = n_comp_up_to_((self%a%l+self%b%l)-1)-blb
         aub = self%a%n_comp
         fub = size(escd,2)
         comp_to_use = first_nonzero
         components = nn
         components_b = nn(:,blb+1:blb+bub)
         index_b = index_of
         index_a = index_of
         allocate(int_new(fub,bub,aub))
         BAx = BA(1)
         BAy = BA(2)
         BAz = BA(3)
         do b = 1,bub
            bx = components_b(1,b)
            by = components_b(2,b)
            bz = components_b(3,b)
            b1 = index_b(bx+1,by,bz)-blb
            b2 = index_b(bx,by+1,bz)-blb
            b3 = index_b(bx,by,bz+1)-blb
            do f = 1,fub
               escd_bcd = escd(b,f)
               int_new(f,b,1) = escd(b1,f)+BAx*escd_bcd
               int_new(f,b,2) = escd(b2,f)+BAy*escd_bcd
               int_new(f,b,3) = escd(b3,f)+BAz*escd_bcd
            end do
         end do
         alb = 1
         do la = 2,self%a%l-1
            aob = alb
            alb = n_comp_up_to_(la-1)
            aub = n_comp_(la)
            bub = n_comp_up_to_((self%a%l+self%b%l)-la)-blb
            component_to_use = comp_to_use(alb+1:alb+aub)
            components_a = components(:,alb+1:alb+aub)
            call move_alloc(int_new,int_old)
            allocate(int_new(fub,bub,aub))
            do a = 1,aub
               ax = components_a(1,a)
               ay = components_a(2,a)
               az = components_a(3,a)
               j = component_to_use(a)
               call subtract_from_component(ax,ay,az,j)
               a1 = index_a(ax,ay,az)-aob
               BAi = BA(j)
               do b = 1,bub
                  bx = components_b(1,b)
                  by = components_b(2,b)
                  bz = components_b(3,b)
                  call add_to_component(bx,by,bz,j)
                  b1 = index_b(bx,by,bz)-blb
                  int_new(:,b,a) = int_old(:,b1,a1)+BAi*int_old(:,b,a1)
               end do
            end do
            deallocate(int_old)
         end do
         aob = alb
         alb = n_comp_up_to_(self%a%l-1)
         aub = self%a%n_comp
         bub = self%b%n_comp
         component_to_use = comp_to_use(alb+1:alb+aub)
         components_a = components(:,alb+1:alb+aub)
         call move_alloc(int_new,int_old)
         do a = 1,aub
            ax = components_a(1,a)
            ay = components_a(2,a)
            az = components_a(3,a)
            j = component_to_use(a)
            call subtract_from_component(ax,ay,az,j)
            a1 = index_a(ax,ay,az)-aob
            BAi = BA(j)
            do b = 1,bub
               bx = components_b(1,b)
               by = components_b(2,b)
               bz = components_b(3,b)
               call add_to_component(bx,by,bz,j)
               b1 = index_b(bx,by,bz)-blb
               do f = 1,fub
                  abcd(a,b,f) = int_old(f,b1,a1)+BAi*int_old(f,b,a1)
               end do
            end do
         end do
         deallocate(int_old)
      end select
   end subroutine

   PURE subroutine add_to_component(x,y,z,j)
   ! Adds one to the component specified by j.  Used by transfer equation.
   ! j=1 => x=x+1, j=2 => y=y+1, j=3 => z=z+1.
      INT, INOUT :: x,y,z
      INT, IN :: j
      select case (j)
      case (1); x = x+1
      case (2); y = y+1
      case (3); z = z+1
      end select
   end subroutine

   pure subroutine subtract_from_component(x,y,z,j)
   ! Subtracts one from the component specified by j.  Used by transfer
   ! equation.  j=1 => x=x-1, j=2 => y=y-1, j=3 => z=z-1.
      INT, INOUT :: x,y,z
      INT, IN :: j
      select case (j)
      case (1); x = x-1
      case (2); y = y-1
      case (3); z = z-1
      end select
   end subroutine


!  ==============================
!  Make the J and K contributions
!  ==============================

   PURE subroutine make_r_JK(self,J,K,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
   ! Make the J and K contribution due to self and P and add it in.
   ! For any shell4!
      type(SHELL4_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: J
      MAT(REAL,:,:), INOUT :: K
      MAT(REAL,:,:), IN :: P
      REAL, IN :: factor
      INT, IN :: fa,la,fb,lb
      INT, IN :: fc,lc,fd,ld
      MAT4(REAL,:,:,:,:), allocatable :: I4
      REAL :: P_dc,Jcd,P_db,P_cb,Kbc,Kbd,I_abcd
      INT :: a,b,c,d
      call create_(I4,fa,la,fb,lb,fc,lc,fd,ld)
      call get_ERI_(self,I4)
      if (factor>0.9) then
         do d = fd,ld
            do c = fc,lc
               P_dc = P(d,c)
               Jcd = ZERO
               do b = fb,lb
                  P_db = P(d,b)
                  P_cb = P(c,b)
                  Kbc = ZERO
                  Kbd = ZERO
                  do a = fa,la
                     I_abcd = I4(a,b,c,d)
                     J(a,b) = J(a,b)+I_abcd*P_dc
                     K(a,c) = K(a,c)+I_abcd*P_db
                     K(a,d) = K(a,d)+I_abcd*P_cb
                     Jcd = Jcd+I_abcd*P(b,a)
                     Kbc = Kbc+I_abcd*P(d,a)
                     Kbd = Kbd+I_abcd*P(c,a)
                  end do
                  K(b,c) = K(b,c)+Kbc
                  K(b,d) = K(b,d)+Kbd
               end do
               J(c,d) = J(c,d)+Jcd
            end do
         end do
      else
         do d = fd,ld
            do c = fc,lc
               P_dc = factor*P(d,c)
               Jcd = ZERO
               do b = fb,lb
                  P_db = factor*P(d,b)
                  P_cb = factor*P(c,b)
                  Kbc = ZERO
                  Kbd = ZERO
                  do a = fa,la
                     I_abcd = I4(a,b,c,d)
                     J(a,b) = J(a,b)+I_abcd*P_dc
                     K(a,c) = K(a,c)+I_abcd*P_db
                     K(a,d) = K(a,d)+I_abcd*P_cb
                     Jcd = Jcd+I_abcd*P(b,a)
                     Kbc = Kbc+I_abcd*P(d,a)
                     Kbd = Kbd+I_abcd*P(c,a)
                  end do
                  K(b,c) = K(b,c)+factor*Kbc
                  K(b,d) = K(b,d)+factor*Kbd
               end do
               J(c,d) = J(c,d)+factor*Jcd
            end do
         end do
      end if
      call destroy_(I4)
   end subroutine


!  =============================
!  Make only the J contributions
!  =============================

   subroutine make_r_J(self,J,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
   ! Make the J contribution due to self and P and add it in.
   ! For any shell4!
      type(SHELL4_TYPE), IN :: self
      MAT(REAL,:,:) :: J
      MAT(REAL,:,:), IN :: P
      REAL, IN :: factor
      INT, IN :: fa,la,fb,lb,fc,lc,fd,ld
      MAT4(REAL,:,:,:,:), allocatable :: I4
      REAL :: P_dc,Jcd,I_abcd
      INT :: a,b,c,d
      call create_(I4,fa,la,fb,lb,fc,lc,fd,ld)
      call get_ERI_(self,I4)
      if (factor>0.9) then
         do d = fd,ld
            do c = fc,lc
               P_dc = P(d,c)
               Jcd = ZERO
               do b = fb,lb
                  do a = fa,la
                     I_abcd = I4(a,b,c,d)
                     J(a,b) = J(a,b)+I_abcd*P_dc
                     Jcd = Jcd+I_abcd*P(b,a)
                  end do
               end do
               J(c,d) = J(c,d)+Jcd
            end do
         end do
      else
         do d = fd,ld
            do c = fc,lc
               P_dc = factor*P(d,c)
               Jcd = ZERO
               do b = fb,lb
                  do a = fa,la
                     I_abcd = I4(a,b,c,d)
                     J(a,b) = J(a,b)+I_abcd*P_dc
                     Jcd = Jcd+I_abcd*P(b,a)
                  end do
               end do
               J(c,d) = J(c,d)+factor*Jcd
            end do
         end do
      end if
      call destroy_(I4)
   end subroutine


!  ======
!  Output
!  ======

   subroutine put(self)
   ! Put the shell4 information to file "out"
      type(SHELL4_TYPE), IN :: self
      INT :: n_cc,i
      call flush_(stdout)
      call show_(stdout,"A shell l quantum number =",self%a%l)
      call show_(stdout,"B shell l quantum number =",self%b%l)
      call show_(stdout,"C shell l quantum number =",self%c%l)
      call show_(stdout,"D shell l quantum number =",self%d%l)
      call show_(stdout,"A position               =",self%a%position)
      call show_(stdout,"B position               =",self%b%position)
      call show_(stdout,"C position               =",self%c%position)
      call show_(stdout,"D position               =",self%d%position)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=8)
      call put_(stdout,"N",int_width=TRUE)
      call put_(stdout,"ex_a")
      call put_(stdout,"cc_a")
      call put_(stdout,"ex_b")
      call put_(stdout,"cc_b")
      call put_(stdout,"ex_c")
      call put_(stdout,"cc_c")
      call put_(stdout,"ex_d")
      call put_(stdout,"cc_d")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=8)
      n_cc = max(self%a%n_cc,self%b%n_cc,self%c%n_cc,self%d%n_cc)
      do i = 1,n_cc
         call put_(stdout,i)
         if (i<=self%a%n_cc) then
            call put_(stdout,self%a%exponent(i))
            call put_(stdout,self%a%contraction(i))
         else
            call tab_(stdout,real_fields=2)
         end if
         if (i<=self%b%n_cc) then
            call put_(stdout,self%b%exponent(i))
            call put_(stdout,self%b%contraction(i))
         else
            call tab_(stdout,real_fields=2)
         end if
         if (i<=self%c%n_cc) then
            call put_(stdout,self%c%exponent(i))
            call put_(stdout,self%c%contraction(i))
         else
            call tab_(stdout,real_fields=2)
         end if
         if (i<=self%d%n_cc) then
            call put_(stdout,self%d%exponent(i))
            call put_(stdout,self%d%contraction(i))
         else
            call tab_(stdout,real_fields=2)
         end if
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=8)
   end subroutine

end module
