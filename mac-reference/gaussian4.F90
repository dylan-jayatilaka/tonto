!==================================================================
!
!  GAUSSIAN4 : Quartets of gaussian functions
!
! Copyright (C) Dylan Jayatilaka, 1999
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
!==================================================================
module GAUSSIAN4_MODULE

#  include "macros"

   include "gaussian4.use"

   implicit none

   include "gaussian4.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(GAUSSIAN4_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"GAUSSIAN4:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(GAUSSIAN4_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"GAUSSIAN4:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  =========
!  Integrals
!  =========

   PURE subroutine make_ERI_ints(self,I)
   ! Make ERI matrix "I" using Rys decomposition of 1/r_{12}.
      type(GAUSSIAN4_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), OUT :: I
      MAT5(REAL,:,:,:,:,:), allocatable :: Ix,Iy,Iz
      VEC(INT,:), allocatable :: ax,ay,az
      VEC(INT,:), allocatable :: bx,by,bz
      VEC(INT,:), allocatable :: cx,cy,cz
      VEC(INT,:), allocatable :: dx,dy,dz
      type(RYS_TYPE), allocatable :: rys
      VEC(REAL,3) :: AB,CD,P,Q
      VEC(REAL,3) :: PA,QC,QP
      REAL :: zeta,eta
      REAL :: zinv,einv
      REAL :: zeinv,rho
      REAL :: xx,AB2,CD2,fac
      INT :: le,lf,n_roots
      INT :: fa,la,fb,lb
      INT :: fc,lc,fd,ld

      ! Angular momenta and # of roots
      le = self%a%l+self%b%l
      lf = self%c%l+self%d%l
      n_roots = (le+lf)/2+2

      ! Allocate 2D integrals
      call create_(Ix,n_roots,le+1,self%b%l+1,lf+1,self%d%l+1)
      call create_(Iy,n_roots,le+1,self%b%l+1,lf+1,self%d%l+1)
      call create_(Iz,n_roots,le+1,self%b%l+1,lf+1,self%d%l+1)

      ! Gaussian index arrays
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      fc = self%c%first_gaussian; lc = self%c%last_gaussian
      fd = self%d%first_gaussian; ld = self%d%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      cx = nx(fc:lc); cy = ny(fc:lc); cz = nz(fc:lc)
      dx = nx(fd:ld); dy = ny(fd:ld); dz = nz(fd:ld)

      ! Exponents
      zeta = self%a%exponent+self%b%exponent
      eta = self%c%exponent+self%d%exponent
      zinv = ONE/zeta
      einv = ONE/eta
      zeinv = ONE/(zeta+eta)
      rho = zeta*eta*zeinv

      ! Positions
      AB = self%a%position-self%b%position
      CD = self%c%position-self%d%position
      P = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)*zinv
      Q = (self%c%exponent*self%c%position+self%d%exponent*self%d%position)*einv
      PA = P-self%a%position
      QC = Q-self%c%position
      QP = Q-P

      ! Rys roots
      xx = rho*(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
      call create_(rys,n_roots)
      call get_weights_(rys,xx)

      ! Form the 2D integrals
      call form_2d_ints_(self,Ix(:,:,1,:,1),Iy(:,:,1,:,1),Iz(:,:,1,:,1),rys%root(1:rys%n_roots),rys%weight(1:rys%n_roots),rho,zinv,einv,PA,QC,QP)
      call transfer_2d_ints_(self,Ix,Iy,Iz,AB,CD)

      ! Form the integrals
      I = sum(Ix(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz),dim=1)

      ! Cleanup
      call destroy_(rys)
      call destroy_(Iz)
      call destroy_(Iy)
      call destroy_(Ix)

      ! Prefactor
      AB2 = AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      CD2 = CD(1)*CD(1)+CD(2)*CD(2)+CD(3)*CD(3)
      fac = self%a%exponent*self%b%exponent*AB2*zinv+self%c%exponent*self%d%exponent*CD2*einv
      fac = TWOPI5ON2*sqrt(zeinv)*zinv*einv*exp(-fac)
      I = fac*I
   end subroutine

   pure subroutine transfer_2d_ints(self,Ix,Iy,Iz,AB,CD,max_b,max_d)
   ! Use the transfer relation to put momenta on centers B and D to get all
   ! the 2d integrals "Ix", "Iy" and "Iz". If present, "max_b" and "max_d"
   ! are the maximum l-values desired for centers b and d, respectively.
   ! You must ensure that the "Ii" arrays are big enough, in this case.
      type(GAUSSIAN4_TYPE), IN :: self
      MAT5(REAL,:,:,:,:,:), INOUT :: Ix,Iy,Iz
      VEC(REAL,3), IN :: AB,CD
      INT, IN, optional :: max_b,max_d
      INT :: le1,lf1,l_a,l_b,l_c,l_d
      INT :: a,b,c,d,a1,b1,c1,d1,la1
      l_a = self%a%l
      l_b = self%b%l
      l_c = self%c%l
      l_d = self%d%l
      if (present(max_b)) l_b = max_b
      if (present(max_d)) l_d = max_d
      if (l_b==0 AND l_d==0) return
      if (l_b/=0) then
         le1 = l_a+l_b+1
         do b = 1,l_b
            do a = 1,le1-b
               b1 = b+1
               a1 = a+1
               Ix(:,a,b1,:,1) = Ix(:,a1,b,:,1)+AB(1)*Ix(:,a,b,:,1)
               Iy(:,a,b1,:,1) = Iy(:,a1,b,:,1)+AB(2)*Iy(:,a,b,:,1)
               Iz(:,a,b1,:,1) = Iz(:,a1,b,:,1)+AB(3)*Iz(:,a,b,:,1)
            end do
         end do
      end if
      if (l_d/=0) then

      !  la1 = l_a + 1
         la1 = l_a+l_b-self%b%l+1  ! increase more along a as for b
         lf1 = l_c+l_d+1
         do d = 1,l_d
            do c = 1,lf1-d
               d1 = d+1
               c1 = c+1
               Ix(:,1:la1,:,c,d1) = Ix(:,1:la1,:,c1,d)+CD(1)*Ix(:,1:la1,:,c,d)
               Iy(:,1:la1,:,c,d1) = Iy(:,1:la1,:,c1,d)+CD(2)*Iy(:,1:la1,:,c,d)
               Iz(:,1:la1,:,c,d1) = Iz(:,1:la1,:,c1,d)+CD(3)*Iz(:,1:la1,:,c,d)
            end do
         end do
      end if
   end subroutine

   pure subroutine form_2d_ints(self,Ix,Iy,Iz,t2,wt,rho,zinv,einv,PA,QC,QP,max_e,max_f)
   ! Forms the two dimensional integrals "Ix", "Iy" and "Iz" with momenta only
   ! on centers A and C, using Rys roots "t2" and weights "wt".
   ! Other variables are intermediates, to avoid calculations: see make_ERI_ints.
   ! If present, "max_e" and "max_f" are used for the maximum angular momenta
   ! of centers (a+b) and (c+d) respectively -- for derivative integrals.
      type(GAUSSIAN4_TYPE), IN :: self
      MAT3(REAL,:,:,:), INOUT :: Ix,Iy,Iz
      VEC(REAL,:), IN :: t2,wt
      REAL, IN :: rho,zinv,einv
      VEC(REAL,3), IN :: PA,QC,QP
      INT, optional, IN :: max_e,max_f
      VEC(REAL,size(t2)) :: ret,rzt,ce,cf,bb,ce1,cf1
      INT :: le,lf,e,f,em1,fm1,fp1,ep1,k
      le = self%a%l+self%b%l
      lf = self%c%l+self%d%l
      if (present(max_e)) le = max_e
      if (present(max_f)) lf = max_f
      Ix(:,1,1) = ONE
      Iy(:,1,1) = ONE
      Iz(:,1,1) = ONE
      if (le/=0 OR lf/=0) then
         if (lf>0) then
            ret = rho*t2*einv
            Ix(:,1,2) = QC(1)-ret(:)*QP(1)
            Iy(:,1,2) = QC(2)-ret(:)*QP(2)
            Iz(:,1,2) = QC(3)-ret(:)*QP(3)
            if (lf>1) then
               cf = (ONE-ret)*HALF*einv
               do f = 2,lf
                  fm1 = f-1
                  fp1 = f+1
                  cf1 = fm1*cf
                  Ix(:,1,fp1) = Ix(:,1,2)*Ix(:,1,f)+cf1*Ix(:,1,fm1)
                  Iy(:,1,fp1) = Iy(:,1,2)*Iy(:,1,f)+cf1*Iy(:,1,fm1)
                  Iz(:,1,fp1) = Iz(:,1,2)*Iz(:,1,f)+cf1*Iz(:,1,fm1)
               end do
            end if
         end if
         if (le>0) then
            rzt = rho*t2*zinv
            Ix(:,2,1) = PA(1)+rzt(:)*QP(1)
            Iy(:,2,1) = PA(2)+rzt(:)*QP(2)
            Iz(:,2,1) = PA(3)+rzt(:)*QP(3)
            if (le>1) then
               ce = (ONE-rzt)*HALF*zinv
               do e = 2,le
                  em1 = e-1
                  ep1 = e+1
                  ce1 = em1*ce
                  Ix(:,ep1,1) = Ix(:,2,1)*Ix(:,e,1)+ce1*Ix(:,em1,1)
                  Iy(:,ep1,1) = Iy(:,2,1)*Iy(:,e,1)+ce1*Iy(:,em1,1)
                  Iz(:,ep1,1) = Iz(:,2,1)*Iz(:,e,1)+ce1*Iz(:,em1,1)
               end do
            end if
         end if
         if (lf>0 AND le>0) then
            bb = HALF*einv*rzt
            Ix(:,2,2) = Ix(:,1,2)*Ix(:,2,1)+bb
            Iy(:,2,2) = Iy(:,1,2)*Iy(:,2,1)+bb
            Iz(:,2,2) = Iz(:,1,2)*Iz(:,2,1)+bb
            do f = 2,lf
               fm1 = f-1
               fp1 = f+1
               cf1 = fm1*cf
               Ix(:,2,fp1) = Ix(:,1,2)*Ix(:,2,f)+cf1*Ix(:,2,fm1)+bb*Ix(:,1,f)
               Iy(:,2,fp1) = Iy(:,1,2)*Iy(:,2,f)+cf1*Iy(:,2,fm1)+bb*Iy(:,1,f)
               Iz(:,2,fp1) = Iz(:,1,2)*Iz(:,2,f)+cf1*Iz(:,2,fm1)+bb*Iz(:,1,f)
            end do
            do e = 2,le
               em1 = e-1
               ep1 = e+1
               ce1 = em1*ce
               do f = 2,lf+1
                  fm1 = f-1
                  Ix(:,ep1,f) = Ix(:,2,1)*Ix(:,e,f)+ce1*Ix(:,em1,f)+fm1*bb(:)*Ix(:,e,fm1)
                  Iy(:,ep1,f) = Iy(:,2,1)*Iy(:,e,f)+ce1*Iy(:,em1,f)+fm1*bb(:)*Iy(:,e,fm1)
                  Iz(:,ep1,f) = Iz(:,2,1)*Iz(:,e,f)+ce1*Iz(:,em1,f)+fm1*bb(:)*Iz(:,e,fm1)
               end do
            end do
         end if
      end if

      ! Multiply Iz by the weight
      do k = 1,size(Iz,1)
         Iz(k,:,:) = Iz(k,:,:)*wt(k)
      end do
   end subroutine

   PURE subroutine make_spin_orbit_ints(self,Sx,Sy,Sz,Ox,Oy,Oz)
   ! Make the same-spin orbit integrals "Sx" "Sy" "Sz"  and the
   ! other spin orbit integrals  "Ox" "Oy" "Oz" using Rys method.
   ! Reference: Bearpark et al., Mol. Phys. 80, p. 479 (1993)
      type(GAUSSIAN4_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), OUT :: Sx,Sy,Sz
      MAT4(REAL,:,:,:,:), OUT :: Ox,Oy,Oz
      MAT5(REAL,:,:,:,:,:), allocatable :: Ix,Iy,Iz,LLx,LLy,LLz,RRx,RRy,RRz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz
      type(RYS_TYPE), allocatable :: rys
      VEC(REAL,3) :: AB,CD,P,Q,PA,QC,QP
      REAL :: zeta,zinv,eta,einv,zeinv,rho,xx,AB2,CD2,fac
      INT :: l_e,l_f,l_a,l_b,l_c,l_d,n_roots,fa,la,fb,lb,fc,lc,fd,ld
      l_a = self%a%l+1; l_b = self%b%l+1
      l_c = self%c%l+1; l_d = self%d%l+1
      l_e = self%a%l+self%b%l+1; l_f = self%c%l+self%d%l+1  ! One higherfor differentiating
      n_roots = (l_e+l_f+2)/2
      call create_(Ix,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)  ! Basic intermediate integrals
      call create_(Iy,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)
      call create_(Iz,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      fc = self%c%first_gaussian; lc = self%c%last_gaussian
      fd = self%d%first_gaussian; ld = self%d%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      cx = nx(fc:lc); cy = ny(fc:lc); cz = nz(fc:lc)
      dx = nx(fd:ld); dy = ny(fd:ld); dz = nz(fd:ld)
      zeta = self%a%exponent+self%b%exponent
      eta = self%c%exponent+self%d%exponent
      zinv = ONE/zeta
      einv = ONE/eta
      zeinv = ONE/(zeta+eta)
      rho = zeta*eta*zeinv
      AB = self%a%position-self%b%position
      CD = self%c%position-self%d%position
      P = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)*zinv
      Q = (self%c%exponent*self%c%position+self%d%exponent*self%d%position)*einv
      PA = P-self%a%position
      QC = Q-self%c%position
      QP = Q-P
      xx = rho*(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
      call create_(rys,n_roots)
      call get_weights_(rys,xx)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      Ix = ZERO; Iy = ZERO; Iz = ZERO
      call form_2d_ints_(self,Ix(:,:,1,:,1),Iy(:,:,1,:,1),Iz(:,:,1,:,1),rys%root(1:rys%n_roots),rys%weight(1:rys%n_roots),rho,zinv,einv,PA,QC,QP,l_e,l_f)
                                                            ! one unit higher for differentiating
      call transfer_2d_ints_(self,Ix,Iy,Iz,AB,CD,max_b=l_b,max_d=l_d)  ! a & c will automatically be one unit higher
      call create_(LLx,n_roots,l_a,l_b,l_c,l_d)  ! Derivative integrals
      call create_(LLy,n_roots,l_a,l_b,l_c,l_d)
      call create_(LLz,n_roots,l_a,l_b,l_c,l_d)
      call create_(RRx,n_roots,l_a,l_b,l_c,l_d)
      call create_(RRy,n_roots,l_a,l_b,l_c,l_d)
      call create_(RRz,n_roots,l_a,l_b,l_c,l_d)
      call differentiate_(self,Ix,"a",LLx); call differentiate_(self,Iy,"a",LLy); call differentiate_(self,Iz,"a",LLz)
      call differentiate_(self,Ix,"b",RRx); call differentiate_(self,Iy,"b",RRy); call differentiate_(self,Iz,"b",RRz)
      ! Form the same-spin orbit integrals
      Sx = sum(Ix(:,ax,bx,cx,dx)*(LLy(:,ay,by,cy,dy)*RRz(:,az,bz,cz,dz)-RRy(:,ay,by,cy,dy)*LLz(:,az,bz,cz,dz)),dim=1)
      Sy = sum(Iy(:,ay,by,cy,dy)*(LLz(:,az,bz,cz,dz)*RRx(:,ax,bx,cx,dx)-RRz(:,az,bz,cz,dz)*LLx(:,ax,bx,cx,dx)),dim=1)
      Sz = sum(Iz(:,az,bz,cz,dz)*(LLx(:,ax,bx,cx,dx)*RRy(:,ay,by,cy,dy)-RRx(:,ax,bx,cx,dx)*LLy(:,ay,by,cy,dy)),dim=1)
      call differentiate_(self,Ix,"c",LLx); call differentiate_(self,Iy,"c",LLy); call differentiate_(self,Iz,"c",LLz)
      call differentiate_(self,Ix,"d",RRx); call differentiate_(self,Iy,"d",RRy); call differentiate_(self,Iz,"d",RRz)
      ! Form the other spin orbit integrals
      Ox = sum(Ix(:,ax,bx,cx,dx)*(LLy(:,ay,by,cy,dy)*RRz(:,az,bz,cz,dz)-RRy(:,ay,by,cy,dy)*LLz(:,az,bz,cz,dz)),dim=1)
      Oy = sum(Iy(:,ay,by,cy,dy)*(LLz(:,az,bz,cz,dz)*RRx(:,ax,bx,cx,dx)-RRz(:,az,bz,cz,dz)*LLx(:,ax,bx,cx,dx)),dim=1)
      Oz = sum(Iz(:,az,bz,cz,dz)*(LLx(:,ax,bx,cx,dx)*RRy(:,ay,by,cy,dy)-RRx(:,ax,bx,cx,dx)*LLy(:,ay,by,cy,dy)),dim=1)
      call destroy_(RRz); call destroy_(RRy); call destroy_(RRx)
      call destroy_(LLz); call destroy_(LLy); call destroy_(LLx)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call destroy_(rys)
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
      AB2 = AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      CD2 = CD(1)*CD(1)+CD(2)*CD(2)+CD(3)*CD(3)
      fac = self%a%exponent*self%b%exponent*AB2*zinv+self%c%exponent*self%d%exponent*CD2*einv
      fac = TWOPI5ON2*sqrt(zeinv)*zinv*einv*exp(-fac)
      Sx = fac*Sx; Sy = fac*Sy; Sz = fac*Sz
      Ox = fac*Ox; Oy = fac*Oy; Oz = fac*Oz
   end subroutine

   PURE subroutine differentiate(self,I,index,ID)
   ! Differentiate a gaussian-integral matrix "I" with respect to the
   ! *nuclear* coordinate on basis function "index", which can be
   ! "a" "b" "c" or "d" depending on which center is differntiated.
   ! Place the result in "ID".
      type(GAUSSIAN4_TYPE), IN :: self
      MAT5(REAL,:,:,:,:,:), OUT :: I,ID
      STR(len=*), IN :: index
      INT :: d_a,d_b,d_c,d_d
      INT :: a,b,c,d
      REAL :: a2,b2,c2,d2
      d_a = ubound(ID,2)
      d_b = ubound(ID,3)
      d_c = ubound(ID,4)
      d_d = ubound(ID,5)
      ID = ZERO
      select case (index)

      case ("a")
         ENSURE(ubound(I,2)>1,"GAUSSIAN4:differentiate ... I array too small to differentiate")
         ENSURE(ubound(I,2)>d_a,"GAUSSIAN4:differentiate ... I and ID arrays are incompatible")
         a2 = TWO*self%a%exponent
         ID(:,1,:,:,:) = a2*I(:,2,1:d_b,1:d_c,1:d_d)
         do a = 2,d_a
            ID(:,a,:,:,:) = a2*I(:,a+1,1:d_b,1:d_c,1:d_d)-(a-1)*I(:,a-1,1:d_b,1:d_c,1:d_d)
         end do

      case ("b")
         ENSURE(ubound(I,3)>1,"GAUSSIAN4:differentiate ... I array too small to differentiate")
         ENSURE(ubound(I,3)>d_b,"GAUSSIAN4:differentiate ... I and ID arrays are incompatible")
         b2 = TWO*self%b%exponent
         ID(:,:,1,:,:) = b2*I(:,1:d_a,2,1:d_c,1:d_d)
         do b = 2,d_b
            ID(:,:,b,:,:) = b2*I(:,1:d_a,b+1,1:d_c,1:d_d)-(b-1)*I(:,1:d_a,b-1,1:d_c,1:d_d)
         end do

      case ("c")
         ENSURE(ubound(I,4)>1,"GAUSSIAN4:differentiate ... I array too small to differentiate")
         ENSURE(ubound(I,4)>d_c,"GAUSSIAN4:differentiate ... I and ID arrays are incompatible")
         c2 = TWO*self%c%exponent
         ID(:,:,:,1,:) = c2*I(:,1:d_a,1:d_b,2,1:d_d)
         do c = 2,d_c
            ID(:,:,:,c,:) = c2*I(:,1:d_a,1:d_b,c+1,1:d_d)-(c-1)*I(:,1:d_a,1:d_b,c-1,1:d_d)
         end do

      case ("d")
         ENSURE(ubound(I,5)>1,"GAUSSIAN4:differentiate ... I array too small to differentiate")
         ENSURE(ubound(I,5)>d_d,"GAUSSIAN4:differentiate ... I and ID arrays are incompatible")
         d2 = TWO*self%d%exponent
         ID(:,:,:,:,1) = d2*I(:,1:d_a,1:d_b,1:d_c,2)
         do d = 2,d_d
            ID(:,:,:,:,d) = d2*I(:,1:d_a,1:d_b,1:d_c,d+1)-(d-1)*I(:,1:d_a,1:d_b,1:d_c,d-1)
         end do
      end select
   end subroutine

   PURE subroutine make_spin_spin_dipole_ints(self,Dxx,Dyy,Dzz,Dxy,Dxz,Dyz)
   ! Make the spin spin magnetic dipole-dipole integrals "Dij" using
   ! Rys method.  Reference: None. But see Bearpark et al., Mol. Phys.
   ! 80, p. 479 (1993) for inspiration.
      type(GAUSSIAN4_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), OUT :: Dxx,Dyy,Dzz
      MAT4(REAL,:,:,:,:), OUT :: Dxy,Dxz,Dyz
      MAT5(REAL,:,:,:,:,:), allocatable :: Ix,Iy,Iz
      MAT5(REAL,:,:,:,:,:), allocatable :: Lx,Ly,Lz
      MAT5(REAL,:,:,:,:,:), allocatable :: Rx,Ry,Rz
      MAT5(REAL,:,:,:,:,:), allocatable :: LL,LR,RR
      VEC(INT,:), allocatable :: ax,ay,az
      VEC(INT,:), allocatable :: bx,by,bz
      VEC(INT,:), allocatable :: cx,cy,cz
      VEC(INT,:), allocatable :: dx,dy,dz
      type(RYS_TYPE), allocatable :: rys
      VEC(REAL,3) :: AB,CD,P,Q,PA,QC,QP
      REAL :: zeta,zinv,eta,einv,zeinv,rho,xx,AB2,CD2,fac
      REAL :: Dxx_abcd,Dyy_abcd,Dzz_abcd,Dxy_abcd,Dxz_abcd,Dyz_abcd,Ixyn,Ixzn,Iyzn
      REAL :: Lxn,Lyn,Lzn,Ixn,Iyn,Izn,Rxn,Ryn,Rzn
      INT :: l_e,l_f,l_a,l_b,l_c,l_d,n_roots,fa,la,fb,lb,fc,lc,fd,ld
      INT :: a,b,c,d,n,aix,aiy,aiz,bix,biy,biz,cix,ciy,ciz,dix,diy,diz
      l_a = self%a%l+1; l_b = self%b%l+1
      l_c = self%c%l+1; l_d = self%d%l+1
      l_e = self%a%l+self%b%l+2; l_f = self%c%l+self%d%l  ! Two higher for differentiating A, B.
      n_roots = (l_e+l_f+2)/2
      call create_(Ix,n_roots,l_e+1,l_b+2,l_f+1,l_d+1)  ! Basic intermediate integrals
      call create_(Iy,n_roots,l_e+1,l_b+2,l_f+1,l_d+1)
      call create_(Iz,n_roots,l_e+1,l_b+2,l_f+1,l_d+1)
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      fc = self%c%first_gaussian; lc = self%c%last_gaussian
      fd = self%d%first_gaussian; ld = self%d%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      cx = nx(fc:lc); cy = ny(fc:lc); cz = nz(fc:lc)
      dx = nx(fd:ld); dy = ny(fd:ld); dz = nz(fd:ld)
      zeta = self%a%exponent+self%b%exponent
      eta = self%c%exponent+self%d%exponent
      zinv = ONE/zeta
      einv = ONE/eta
      zeinv = ONE/(zeta+eta)
      rho = zeta*eta*zeinv
      AB = self%a%position-self%b%position
      CD = self%c%position-self%d%position
      P = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)*zinv
      Q = (self%c%exponent*self%c%position+self%d%exponent*self%d%position)*einv
      PA = P-self%a%position
      QC = Q-self%c%position
      QP = Q-P
      xx = rho*(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
      call create_(rys,n_roots)
      call get_weights_(rys,xx)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      Ix = ZERO; Iy = ZERO; Iz = ZERO
      call form_2d_ints_(self,Ix(:,:,1,:,1),Iy(:,:,1,:,1),Iz(:,:,1,:,1),rys%root(1:rys%n_roots),rys%weight(1:rys%n_roots),rho,zinv,einv,PA,QC,QP,l_e,l_f)
                                                   ! two units higher on B for differentiating
      call transfer_2d_ints_(self,Ix,Iy,Iz,AB,CD,max_b=l_b+1)  ! A will automatically be 2 units higher
      call create_(Lx,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Ly,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Lz,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Rx,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Ry,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Rz,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(LL,n_roots,l_a,l_b,l_c,l_d)
      call create_(LR,n_roots,l_a,l_b,l_c,l_d)
      call create_(RR,n_roots,l_a,l_b,l_c,l_d)
      call differentiate_(self,Ix,"a",Lx); call differentiate_(self,Ix,"b",Rx)
      call differentiate_(self,Iy,"a",Ly); call differentiate_(self,Iy,"b",Ry)
      call differentiate_(self,Iz,"a",Lz); call differentiate_(self,Iz,"b",Rz)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call differentiate_(self,Lx,"a",LL); call differentiate_(self,Rx,"b",RR)
      call differentiate_(self,Lx,"b",LR); LR = TWO*LR

!         +     RR(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz),dim=1)
      do d = 1,self%d%n_comp
         dix = dx(d)
         diy = dy(d)
         diz = dz(d)
         do c = 1,self%c%n_comp
            cix = cx(c)
            ciy = cy(c)
            ciz = cz(c)
            do b = 1,self%b%n_comp
               bix = bx(b)
               biy = by(b)
               biz = bz(b)
               do a = 1,self%a%n_comp
                  aix = ax(a)
                  aiy = ay(a)
                  aiz = az(a)
                  Dxx_abcd = ZERO
                  do n = 1,n_roots
                     Iyzn = Iy(n,aiy,biy,ciy,diy)*Iz(n,aiz,biz,ciz,diz)
                     Dxx_abcd = Dxx_abcd+Iyzn*(LL(n,aix,bix,cix,dix)+LR(n,aix,bix,cix,dix)+RR(n,aix,bix,cix,dix))
                  end do
                  Dxx(a,b,c,d) = Dxx_abcd
               end do
            end do
         end do
      end do
      call differentiate_(self,Ly,"a",LL); call differentiate_(self,Ry,"b",RR)
      call differentiate_(self,Ly,"b",LR); LR = TWO*LR

!         +     Ix(:,ax,bx,cx,dx)*RR(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz),dim=1)
      do d = 1,self%d%n_comp
         dix = dx(d)
         diy = dy(d)
         diz = dz(d)
         do c = 1,self%c%n_comp
            cix = cx(c)
            ciy = cy(c)
            ciz = cz(c)
            do b = 1,self%b%n_comp
               bix = bx(b)
               biy = by(b)
               biz = bz(b)
               do a = 1,self%a%n_comp
                  aix = ax(a)
                  aiy = ay(a)
                  aiz = az(a)
                  Dyy_abcd = ZERO
                  do n = 1,n_roots
                     Ixzn = Ix(n,aix,bix,cix,dix)*Iz(n,aiz,biz,ciz,diz)
                     Dyy_abcd = Dyy_abcd+Ixzn*(LL(n,aiy,biy,ciy,diy)+LR(n,aiy,biy,ciy,diy)+RR(n,aiy,biy,ciy,diy))
                  end do
                  Dyy(a,b,c,d) = Dyy_abcd
               end do
            end do
         end do
      end do
      call differentiate_(self,Lz,"a",LL); call differentiate_(self,Rz,"b",RR)
      call differentiate_(self,Lz,"b",LR); LR = TWO*LR

!         +     Ix(:,ax,bx,cx,dx)*Iz(:,ay,by,cy,dy)*RR(:,az,bz,cz,dz),dim=1)
      do d = 1,self%d%n_comp
         dix = dx(d)
         diy = dy(d)
         diz = dz(d)
         do c = 1,self%c%n_comp
            cix = cx(c)
            ciy = cy(c)
            ciz = cz(c)
            do b = 1,self%b%n_comp
               bix = bx(b)
               biy = by(b)
               biz = bz(b)
               do a = 1,self%a%n_comp
                  aix = ax(a)
                  aiy = ay(a)
                  aiz = az(a)
                  Dzz_abcd = ZERO
                  do n = 1,n_roots
                     Ixyn = Ix(n,aix,bix,cix,dix)*Iy(n,aiy,biy,ciy,diy)
                     Dzz_abcd = Dzz_abcd+Ixyn*(LL(n,aiz,biz,ciz,diz)+LR(n,aiz,biz,ciz,diz)+RR(n,aiz,biz,ciz,diz))
                  end do
                  Dzz(a,b,c,d) = Dzz_abcd
               end do
            end do
         end do
      end do
      call destroy_(RR); call destroy_(LR); call destroy_(LL)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!         +     Rx(:,ax,bx,cx,dx)*Ry(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz),dim=1)

!         +     Rx(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Rz(:,az,bz,cz,dz),dim=1)

!         +     Ix(:,ax,bx,cx,dx)*Ry(:,ay,by,cy,dy)*Rz(:,az,bz,cz,dz),dim=1)
      do d = 1,self%d%n_comp
         dix = dx(d)
         diy = dy(d)
         diz = dz(d)
         do c = 1,self%c%n_comp
            cix = cx(c)
            ciy = cy(c)
            ciz = cz(c)
            do b = 1,self%b%n_comp
               bix = bx(b)
               biy = by(b)
               biz = bz(b)
               do a = 1,self%a%n_comp
                  aix = ax(a)
                  aiy = ay(a)
                  aiz = az(a)
                  Dxy_abcd = ZERO
                  Dxz_abcd = ZERO
                  Dyz_abcd = ZERO
                  do n = 1,n_roots
                     Lxn = Lx(n,aix,bix,cix,dix)
                     Lyn = Ly(n,aiy,biy,ciy,diy)
                     Lzn = Lz(n,aiz,biz,ciz,diz)
                     Ixn = Ix(n,aix,bix,cix,dix)
                     Iyn = Iy(n,aiy,biy,ciy,diy)
                     Izn = Iz(n,aiz,biz,ciz,diz)
                     Rxn = Rx(n,aix,bix,cix,dix)
                     Ryn = Ry(n,aiy,biy,ciy,diy)
                     Rzn = Rz(n,aiz,biz,ciz,diz)
                     Dxy_abcd = Dxy_abcd+Izn*(Lxn*Lyn+Lxn*Ryn+Rxn*Lyn+Rxn*Ryn)
                     Dxz_abcd = Dxz_abcd+Iyn*(Lxn*Lzn+Lxn*Rzn+Rxn*Lzn+Rxn*Rzn)
                     Dyz_abcd = Dyz_abcd+Ixn*(Lyn*Lzn+Lyn*Rzn+Ryn*Lzn+Ryn*Rzn)
                  end do
                  Dxy(a,b,c,d) = Dxy_abcd
                  Dxz(a,b,c,d) = Dxz_abcd
                  Dyz(a,b,c,d) = Dyz_abcd
               end do
            end do
         end do
      end do

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call destroy_(Rz); call destroy_(Ry); call destroy_(Rx)
      call destroy_(Lz); call destroy_(Ly); call destroy_(Lx)
      call destroy_(rys)
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
      AB2 = AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      CD2 = CD(1)*CD(1)+CD(2)*CD(2)+CD(3)*CD(3)
      fac = self%a%exponent*self%b%exponent*AB2*zinv+self%c%exponent*self%d%exponent*CD2*einv
      fac = TWOPI5ON2*sqrt(zeinv)*zinv*einv*exp(-fac)
      Dxx = fac*Dxx; Dyy = fac*Dyy; Dzz = fac*Dzz
      Dxy = fac*Dxy; Dxz = fac*Dxz; Dyz = fac*Dyz
   end subroutine

   PURE subroutine make_ERI_derivatives(self,AA,BB,CC,DD)
   ! Make the ERI gradient integrals "AA", "BB", "CC" and "DD". The
   ! last dimension determines whether the derivative is x, y, or z.
      type(GAUSSIAN4_TYPE), INOUT :: self
      MAT5(REAL,:,:,:,:,:), optional, OUT :: AA,BB,CC,DD
      MAT5(REAL,:,:,:,:,:), allocatable :: IIx,IIy,IIz,AAx,AAy,AAz,BBx,BBy,BBz,CCx,CCy,CCz,DDx,DDy,DDz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz
      type(RYS_TYPE), allocatable :: rys
      VEC(REAL,3) :: AB,CD,P,Q,PA,QC,QP
      REAL :: zeta,zinv,eta,einv,zeinv,rho,xx,AB2,CD2,fac
      INT :: l_e,l_f,l_a,l_b,l_c,l_d,n_roots,fa,la,fb,lb,fc,lc,fd,ld
      l_a = self%a%l+1; l_b = self%b%l+1
      l_c = self%c%l+1; l_d = self%d%l+1
      l_e = self%a%l+self%b%l+1; l_f = self%c%l+self%d%l+1  ! One higher for differentiating
      n_roots = (l_e+l_f+2)/2
      call create_(IIx,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)  ! Basic intermediate integrals
      call create_(IIy,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)
      call create_(IIz,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)
      fa = self%a%first_gaussian; la = self%a%last_gaussian
      fb = self%b%first_gaussian; lb = self%b%last_gaussian
      fc = self%c%first_gaussian; lc = self%c%last_gaussian
      fd = self%d%first_gaussian; ld = self%d%last_gaussian
      ax = nx(fa:la); ay = ny(fa:la); az = nz(fa:la)
      bx = nx(fb:lb); by = ny(fb:lb); bz = nz(fb:lb)
      cx = nx(fc:lc); cy = ny(fc:lc); cz = nz(fc:lc)
      dx = nx(fd:ld); dy = ny(fd:ld); dz = nz(fd:ld)
      zeta = self%a%exponent+self%b%exponent
      eta = self%c%exponent+self%d%exponent
      zinv = ONE/zeta
      einv = ONE/eta
      zeinv = ONE/(zeta+eta)
      rho = zeta*eta*zeinv
      AB = self%a%position-self%b%position
      CD = self%c%position-self%d%position
      P = (self%a%exponent*self%a%position+self%b%exponent*self%b%position)*zinv
      Q = (self%c%exponent*self%c%position+self%d%exponent*self%d%position)*einv
      PA = P-self%a%position
      QC = Q-self%c%position
      QP = Q-P
      xx = rho*(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
      call create_(rys,n_roots)
      call get_weights_(rys,xx)
      AB2 = AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      CD2 = CD(1)*CD(1)+CD(2)*CD(2)+CD(3)*CD(3)
      fac = self%a%exponent*self%b%exponent*AB2*zinv+self%c%exponent*self%d%exponent*CD2*einv
      fac = TWOPI5ON2*sqrt(zeinv)*zinv*einv*exp(-fac)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IIx = ZERO; IIy = ZERO; IIz = ZERO
      call form_2d_ints_(self,IIx(:,:,1,:,1),IIy(:,:,1,:,1),IIz(:,:,1,:,1),rys%root(1:rys%n_roots),rys%weight(1:rys%n_roots),rho,zinv,einv,PA,QC,QP,l_e,l_f)
                                                               ! one unit higher for differentiating
      call transfer_2d_ints_(self,IIx,IIy,IIz,AB,CD,max_b=l_b,max_d=l_d)  ! a & c will automatically be one unit higher

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (present(AA)) then
         call create_(AAx,n_roots,l_a,l_b,l_c,l_d)  ! Derivative integrals
         call create_(AAy,n_roots,l_a,l_b,l_c,l_d)
         call create_(AAz,n_roots,l_a,l_b,l_c,l_d)
         call differentiate_(self,IIx,"a",AAx); call differentiate_(self,IIy,"a",AAy); call differentiate_(self,IIz,"a",AAz)
         AA(:,:,:,:,1) = sum(AAx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
         AA(:,:,:,:,2) = sum(IIx(:,ax,bx,cx,dx)*AAy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
         AA(:,:,:,:,3) = sum(IIx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*AAz(:,az,bz,cz,dz),dim=1)
         call destroy_(AAz); call destroy_(AAy); call destroy_(AAx)
         AA = fac*AA
      end if

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (present(BB)) then
         call create_(BBx,n_roots,l_a,l_b,l_c,l_d)  ! Derivative integrals
         call create_(BBy,n_roots,l_a,l_b,l_c,l_d)
         call create_(BBz,n_roots,l_a,l_b,l_c,l_d)
         call differentiate_(self,IIx,"b",BBx); call differentiate_(self,IIy,"b",BBy); call differentiate_(self,IIz,"b",BBz)
         BB(:,:,:,:,1) = sum(BBx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
         BB(:,:,:,:,2) = sum(IIx(:,ax,bx,cx,dx)*BBy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
         BB(:,:,:,:,3) = sum(IIx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*BBz(:,az,bz,cz,dz),dim=1)
         call destroy_(BBz); call destroy_(BBy); call destroy_(BBx)
         BB = fac*BB
      end if

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (present(CC)) then
         call create_(CCx,n_roots,l_a,l_b,l_c,l_d)  ! Derivative integrals
         call create_(CCy,n_roots,l_a,l_b,l_c,l_d)
         call create_(CCz,n_roots,l_a,l_b,l_c,l_d)
         call differentiate_(self,IIx,"c",CCx); call differentiate_(self,IIy,"c",CCy); call differentiate_(self,IIz,"c",CCz)
         CC(:,:,:,:,1) = sum(CCx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
         CC(:,:,:,:,2) = sum(IIx(:,ax,bx,cx,dx)*CCy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
         CC(:,:,:,:,3) = sum(IIx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*CCz(:,az,bz,cz,dz),dim=1)
         call destroy_(CCz); call destroy_(CCy); call destroy_(CCx)
         CC = fac*CC
      end if

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (present(DD)) then
         call create_(DDx,n_roots,l_a,l_b,l_c,l_d)  ! Derivative integrals
         call create_(DDy,n_roots,l_a,l_b,l_c,l_d)
         call create_(DDz,n_roots,l_a,l_b,l_c,l_d)
         call differentiate_(self,IIx,"d",DDx); call differentiate_(self,IIy,"d",DDy); call differentiate_(self,IIz,"d",DDz)
         DD(:,:,:,:,1) = sum(DDx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
         DD(:,:,:,:,2) = sum(IIx(:,ax,bx,cx,dx)*DDy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
         DD(:,:,:,:,3) = sum(IIx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*DDz(:,az,bz,cz,dz),dim=1)
         call destroy_(DDz); call destroy_(DDy); call destroy_(DDx)
         DD = fac*DD
      end if

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call destroy_(rys)
      call destroy_(IIz)
      call destroy_(IIy)
      call destroy_(IIx)
   end subroutine

   subroutine put(self)
   ! Put the object to file "out"
      type(GAUSSIAN4_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"GAUSSIAN4 output:")
      call flush_(stdout)
      call show_(stdout,"l_a   =",self%a%l)
      call show_(stdout,"l_b   =",self%b%l)
      call show_(stdout,"l_c   =",self%c%l)
      call show_(stdout,"l_d   =",self%d%l)
      call show_(stdout,"Ra    =",self%a%position(1),self%a%position(2),self%a%position(3))
      call show_(stdout,"Rb    =",self%b%position(1),self%b%position(2),self%b%position(3))
      call show_(stdout,"Rc    =",self%c%position(1),self%c%position(2),self%c%position(3))
      call show_(stdout,"Rd    =",self%d%position(1),self%d%position(2),self%d%position(3))
      call show_(stdout,"alpha =",self%a%exponent)
      call show_(stdout,"beta  =",self%b%exponent)
      call show_(stdout,"gamma =",self%c%exponent)
      call show_(stdout,"delta =",self%d%exponent)
   end subroutine

end module
