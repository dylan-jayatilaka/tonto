!-------------------------------------------------------------------------------
!
! T_TENSOR: produces the elements of cartesian interaction tensors aka 
! 'T tensors' used in electrodynamics
!
! Copyright (C) Peter Spackman, 2017
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
!-------------------------------------------------------------------------------
module T_TENSOR_MODULE

#  include "macros"

   include "t_tensor.use"

   implicit none

   include "t_tensor.int"


contains

!  ==========
!  Allocation
!  ==========

   subroutine create(self,a,b)
   ! create the T tensors from two locations
      type(T_TENSOR_TYPE), INOUT :: self
      VEC(REAL,3), IN :: a,b
      VEC(REAL,3) :: r
      REAL :: d
      INT :: i,j,k,l
      r = b-a
      d = norm2(r)
      self%scalar = t0(d)
      self%alpha = ZERO
      self%x = t1(d,r(1)); self%alpha(1) = self%x
      self%y = t1(d,r(2)); self%alpha(2) = self%y
      self%z = t1(d,r(3)); self%alpha(3) = self%z
      self%xx = t2(d,[r(1),r(1)],[1,1])
      self%alpha_beta(1,1) = self%xx
      self%yy = t2(d,[r(2),r(2)],[2,2])
      self%alpha_beta(2,2) = self%yy
      self%zz = t2(d,[r(3),r(3)],[3,3])
      self%alpha_beta(3,3) = self%zz
      self%xy = t2(d,[r(1),r(2)],[1,2])
      self%alpha_beta(1,2) = self%xy
      self%alpha_beta(2,1) = self%xy
      self%xz = t2(d,[r(1),r(3)],[1,3])
      self%alpha_beta(1,3) = self%xz
      self%alpha_beta(3,1) = self%xz
      self%yz = t2(d,[r(2),r(3)],[2,3])
      self%alpha_beta(2,3) = self%yz
      self%alpha_beta(3,2) = self%yz
      self%alpha_beta_gamma = ZERO
      self%xxx = t3(d,[r(1),r(1),r(1)],[1,1,1])
      self%alpha_beta_gamma(1,1,1) = self%xxx
      self%yyy = t3(d,[r(2),r(2),r(2)],[2,2,2])
      self%alpha_beta_gamma(2,2,2) = self%yyy
      self%zzz = t3(d,[r(3),r(3),r(3)],[3,3,3])
      self%alpha_beta_gamma(3,3,3) = self%zzz
      self%xxy = t3(d,[r(1),r(1),r(2)],[1,1,2])
      self%alpha_beta_gamma(1,1,2) = self%xxy
      self%alpha_beta_gamma(1,2,1) = self%xxy
      self%alpha_beta_gamma(2,1,1) = self%xxy
      self%xxz = t3(d,[r(1),r(1),r(3)],[1,1,3])
      self%alpha_beta_gamma(1,1,3) = self%xxz
      self%alpha_beta_gamma(1,3,1) = self%xxz
      self%alpha_beta_gamma(3,1,1) = self%xxz
      self%yyx = t3(d,[r(2),r(2),r(1)],[2,2,1])
      self%alpha_beta_gamma(2,2,1) = self%yyx
      self%alpha_beta_gamma(2,1,2) = self%yyx
      self%alpha_beta_gamma(1,2,2) = self%yyx
      self%yyz = t3(d,[r(2),r(2),r(3)],[2,2,3])
      self%alpha_beta_gamma(2,2,3) = self%yyz
      self%alpha_beta_gamma(2,3,2) = self%yyz
      self%alpha_beta_gamma(3,2,2) = self%yyz
      self%zzx = t3(d,[r(3),r(3),r(1)],[3,3,1])
      self%alpha_beta_gamma(3,3,1) = self%zzx
      self%alpha_beta_gamma(3,1,3) = self%zzx
      self%alpha_beta_gamma(1,3,3) = self%zzx
      self%zzy = t3(d,[r(3),r(3),r(2)],[3,3,2])
      self%alpha_beta_gamma(3,3,2) = self%zzy
      self%alpha_beta_gamma(3,2,3) = self%zzy
      self%alpha_beta_gamma(2,3,3) = self%zzy
      self%xyz = t3(d,[r(1),r(2),r(3)],[1,2,3])
      self%alpha_beta_gamma(1,2,3) = self%xyz
      self%alpha_beta_gamma(1,3,2) = self%xyz
      self%alpha_beta_gamma(2,1,3) = self%xyz
      self%alpha_beta_gamma(2,3,1) = self%xyz
      self%alpha_beta_gamma(3,1,2) = self%xyz
      self%alpha_beta_gamma(3,2,1) = self%xyz
      do i = 1,3
         do j = 1,3
            do k = 1,3
               self%alpha_beta_gamma(i,j,k) = t3(d,[r(i),r(j),r(k)],[i,j,k])
            end do
         end do
      end do
      self%alpha_beta_gamma_delta = ZERO
      do i = 1,3
         do j = 1,3
            do k = 1,3
               do l = 1,3
                  self%alpha_beta_gamma_delta(i,j,k,l) = t4(d,[r(i),r(j),r(k),r(l)],[i,j,k,l])
               end do
            end do
         end do
      end do
   end subroutine

   pure subroutine create_fast(self,a,b)
   ! create the T tensors from two locations
      type(T_TENSOR_TYPE), INOUT :: self
      VEC(REAL,3), IN :: a,b
      VEC(REAL,3) :: r
      REAL :: d
      r = b-a
      d = norm2(r)
      self%scalar = t0(d)
      self%alpha = ZERO
      self%x = t1(d,r(1));
      self%y = t1(d,r(2));
      self%z = t1(d,r(3));
      self%xx = t2_fast(d,r(1),r(1),1,1)
      self%yy = t2_fast(d,r(2),r(2),2,2)
      self%zz = t2_fast(d,r(3),r(3),3,3)
      self%xy = t2_fast(d,r(1),r(2),1,2)
      self%xz = t2_fast(d,r(1),r(3),1,3)
      self%yz = t2_fast(d,r(2),r(3),2,3)
      self%xxx = t3_fast(d,r(1),r(1),r(1),1,1,1)
      self%yyy = t3_fast(d,r(2),r(2),r(2),2,2,2)
      self%zzz = t3_fast(d,r(3),r(3),r(3),3,3,3)
      self%xxy = t3_fast(d,r(1),r(1),r(2),1,1,2)
      self%xxz = t3_fast(d,r(1),r(1),r(3),1,1,3)
      self%yyx = t3_fast(d,r(2),r(2),r(1),2,2,1)
      self%yyz = t3_fast(d,r(2),r(2),r(3),2,2,3)
      self%zzx = t3_fast(d,r(3),r(3),r(1),3,3,1)
      self%zzy = t3_fast(d,r(3),r(3),r(2),3,3,2)
      self%xyz = t3_fast(d,r(1),r(2),r(3),1,2,3)
   end subroutine

   pure function kd(i,j) result (res)
   ! calculate the kronecker delta
      INT, IN :: i,j
      INT :: res
      res = 0
      if (i==j) res = 1
   end function

   pure function t0(d) result (res)
   ! calculate T elements of order 0 (point)
      REAL, IN :: d
      REAL :: res
      res = ONE/d
   end function

   pure function t1(d,r) result (res)
   ! calculate T elements of order 1 (charge-dipole)
      REAL, IN :: d,r
      REAL :: res
      res = -r/(d**3)
   end function

   pure function t2(d,r,idx) result (res)
   ! calculate T elements of order 2 (dipole-dipole/charge-quadrupole)
      REAL, IN :: d
      VEC(REAL,2), IN :: r
      VEC(INT,2), IN :: idx
      REAL :: res
      res = (THREE*r(1)*r(2))-d*d*kd(idx(1),idx(2))
      res = res/(d**5)
   end function

   pure function t2_fast(d,r1,r2,i,j) result (res)
   ! calculate T elements of order 2 (dipole-dipole/charge-quadrupole)
      REAL, IN :: d
      REAL, IN :: r1,r2
      INT, IN :: i,j
      REAL :: res
      res = (THREE*r1*r2)-d*d*kd(i,j)
      res = res/(d**5)
   end function

   pure function t3(d,r,idx) result (res)
   ! calculate T elements of order 3 (quadrupole-quadrupole/charge-octapole)
      REAL, IN :: d
      VEC(REAL,3), IN :: r
      VEC(INT,3), IN :: idx
      REAL :: res
      INT :: d_12,d_13,d_23
      d_12 = kd(idx(1),idx(2)); d_13 = kd(idx(1),idx(3))
      d_23 = kd(idx(2),idx(3))
      res = FIVE*r(1)*r(2)*r(3)-d*d*(r(1)*d_23+r(2)*d_13+r(3)*d_12)
      res = res*THREE/(d**SEVEN)
   end function

   pure function t3_fast(d,r1,r2,r3,i,j,k) result (res)
   ! calculate T elements of order 3 (quadrupole-quadrupole/charge-octapole)
      REAL, IN :: d
      REAL, IN :: r1,r2,r3
      INT, IN :: i,j,k
      REAL :: res
      INT :: d_12,d_13,d_23
      d_12 = kd(i,j); d_13 = kd(i,k)
      d_23 = kd(j,k)
      res = FIVE*r1*r2*r3-d*d*(r1*d_23+r2*d_13+r3*d_12)
      res = res*THREE/(d**SEVEN)
   end function

   pure function t4(d,r,idx) result (res)
   ! calculate T elements of order (charge-hexadecapole)
      REAL, IN :: d
      VEC(REAL,4), IN :: r
      VEC(INT,4), IN :: idx
      REAL :: res
      INT :: d_12,d_13,d_14,d_23,d_24,d_34
      d_12 = kd(idx(1),idx(2)); d_13 = kd(idx(1),idx(3)); d_14 = kd(idx(1),idx(4))
      d_23 = kd(idx(1),idx(3)); d_24 = kd(idx(2),idx(4)); d_34 = kd(idx(3),idx(4))
      res = r(1)*r(2)*d_34+r(1)*r(3)*d_24+r(1)*r(4)*d_23+r(2)*r(3)*d_14+r(2)*r(4)*d_13+r(3)*r(4)*d_12
      res = 105d0*r(1)*r(2)*r(3)*r(4)-res*(15d0*d**TWO)+(THREE*d**FOUR)*(d_12*d_34+d_13*d_24+d_14*d_23)
      res = res/(d**NINE)
   end function

   function interaction_energy(self,C_a,C_b,D_a,D_b,Q_a,Q_b,O_a,O_b) result (res)
   ! calculate the expectation value of the interaction operator up to l == 4
   ! excluding hexadecapole-charge term
      type(T_TENSOR_TYPE), IN :: self
      REAL, IN :: C_a,C_b  ! charges
      VEC(REAL,3), IN :: D_a,D_b  ! dipoles
      MAT(REAL,3,3), IN :: Q_a,Q_b  !quadrupoles
      MAT3(REAL,3,3,3), IN :: O_a,O_b  ! octupoles
      VEC(REAL,5) :: res
      INT :: i,j,k,l
      REAL :: total_a,total_b
      res = ZERO
      ! charge-charge term
      res(1) = self%scalar*C_a*C_b

      ! dipole-charge term
      do i = 1,3
         res(2) = res(2)+self%alpha(i)*(C_a*D_b(i)-D_a(i)*C_b)
      end do

      ! charge-quadrupole/dipole-dipole term
      do i = 1,3
         do j = 1,3
            res(3) = res(3)+self%alpha_beta(i,j)*(HALF*(C_a*Q_b(i,j)+Q_a(i,j)*C_b)-D_a(i)*D_b(j))
         end do
      end do
      total_a = ZERO
      total_b = ZERO
      ! charge-octapole/quadrupole-dipole term
      do i = 1,3
         do j = 1,3
            do k = 1,3
               total_a = total_a+self%alpha_beta_gamma(i,j,k)*(O_b(i,j,k)*C_a-O_a(i,j,k)*C_b)
               total_b = total_b+self%alpha_beta_gamma(i,j,k)*(Q_b(i,j)*D_a(k)+Q_a(i,j)*D_b(k))
            end do
         end do
      end do
      res(4) = SIXTH*total_a+HALF*total_b
      total_a = ZERO
      total_b = ZERO
      do i = 1,3
         do j = 1,3
            do k = 1,3
               do l = 1,3
                  total_a = total_a+self%alpha_beta_gamma_delta(i,j,k,l)*Q_a(i,j)*Q_b(k,l)
                  total_b = total_b+self%alpha_beta_gamma_delta(i,j,k,l)*(O_b(i,j,k)*D_a(l)+D_b(l)*O_a(i,j,k))
               end do
            end do
         end do
      end do
      res(5) = QUARTER*total_a-SIXTH*total_b
   end function


!  interaction_energy_fast(C_a, C_b, D_a, D_b, Q_a, Q_b, O_a, O_b) result (res) :: public
!  ! calculate the expectation value of the interaction operator up to l == 4
!  ! excluding hexadecapole-charge term
!     self :: IN
!     C_a, C_b :: REAL, IN ! charges
!     D_a, D_b :: VEC{REAL}(3), IN ! dipoles
!     Q_a, Q_b :: VEC{REAL}(6), IN !quadrupoles
!     O_a, O_b :: VEC{REAL}(10), IN ! octupoles
!     res :: VEC{REAL}(5)
!     i,j,k,l :: INT
!     total_a, total_b :: REAL
!     res = ZERO
!     ! charge-charge term
!     res(1) = .scalar * C_a * C_b

!     ! dipole-charge term
!     do i = 1,3
!        res(2) = res(2) + .alpha(i)*(C_a*D_b(i) - D_a(i)*C_b)
!     end do

!     ! charge-quadrupole/dipole-dipole term
!     res(3) = res(3) + .xx*(HALF *(C_a*Q_b(1)  + Q_a(1)*C_b) - D_a(1)*D_b(1))
!     res(3) = res(3) + .yy*(HALF *(C_a*Q_b(2)  + Q_a(2)*C_b) - D_a(2)*D_b(2))
!     res(3) = res(3) + .zz*(HALF *(C_a*Q_b(3)  + Q_a(3)*C_b) - D_a(3)*D_b(3))
!     res(3) = res(3) + .xy*(C_a*Q_b(4)  + Q_a(4)*C_b - D_a(1)*D_b(2))
!     res(3) = res(3) + .xz*(C_a*Q_b(5)  + Q_a(5)*C_b - D_a(1)*D_b(3))
!     res(3) = res(3) + .yz*(C_a*Q_b(6)  + Q_a(6)*C_b - D_a(2)*D_b(3))

!     total_a = ZERO
!     total_b = ZERO
!     ! charge-octapole/quadrupole-dipole term

!     total_a = total_a + .xxx*(O_b(1)*C_a- O_a(1)*C_b)
!     total_b = total_b + .xxx*(Q_b(1) * D_a(1) + Q_a(1) * D_b(1))
!     total_a = total_a + .yyy*(O_b(2)*C_a- O_a(2)*C_b)
!     total_b = total_b + .yyy*(Q_b(2) * D_a(2) + Q_a(2) * D_b(2))
!     total_a = total_a + .zzz*(O_b(3)*C_a- O_a(3)*C_b)
!     total_b = total_b + .zzz*(Q_b(3) * D_a(3) + Q_a(3) * D_b(3))
!     ! xxy
!     total_a = total_a + THREE * .xxy*(O_b(4)*C_a- O_a(4)*C_b)
!     total_b = total_b + THREE * .xxy*(Q_b(1) * D_a(2) + Q_a(1) * D_b(2))
!     ! xxz
!     total_a = total_a + THREE * .xxy*(O_b(5)*C_a- O_a(5)*C_b)
!     total_b = total_b + THREE * .xxy*(Q_b(1) * D_a(3) + Q_a(1) * D_b(3))
!     ! yyx
!     total_a = total_a + THREE * .yyx*(O_b(6)*C_a- O_a(5)*C_b)
!     total_b = total_b + THREE * .yyx*(Q_b(5) * D_a(1) + Q_a(5) * D_b(1))
!     ! yyz
!     total_a = total_a + THREE * .yyz*(O_b(7)*C_a- O_a(7)*C_b)
!     total_b = total_b + THREE * .yyz*(Q_b(5) * D_a(3) + Q_a(5) * D_b(3))
!     ! zzx 
!     total_a = total_a + THREE * .zzx*(O_b(8)*C_a- O_a(8)*C_b)
!     total_b = total_b + THREE * .zzx*(Q_b(3) * D_a(1) + Q_a(3) * D_b(1))
!     ! zzx 
!     total_a = total_a + THREE * .zzx*(O_b(9)*C_a- O_a(9)*C_b)
!     total_b = total_b + THREE * .zzx*(Q_b(3) * D_a(2) + Q_a(3) * D_b(2))
!     ! xyz
!     total_a = total_a + NINE * .xyz*(O_b(10)*C_a- O_a(10)*C_b)
!     total_b = total_b + NINE * .xyz*(Q_b(5) * D_a(3) + Q_a(5) * D_b(3))

!     res(4) = SIXTH * total_a + HALF *total_b

!     total_a = ZERO
!     total_b = ZERO
!     ! TODO FAST l==4 term
!     !do i = 1,3
!     !do j = 1,3
!     !do k = 1,3
!     !do l = 1,3
!     !   total_a = total_a + .alpha_beta_gamma_delta(i,j,k,l) * Q_a(i,j)*Q_b(k,l) 
!     !   total_b = total_b + .alpha_beta_gamma_delta(i,j,k,l) * (O_b(i,j,k)*D_a(l)  + D_b(l) * O_a(i,j,k))
!     !end do
!     !end do
!     !end do
!     !end do
!     !res(5) = QUARTER * total_a - SIXTH * total_b
!  end

   function electrostatic_potential_0(self,C_a,D_a,Q_a,O_a) result (res)
   ! calculate the expectation value of the interaction operator up to l == 4
   ! excluding hexadecapole-charge term
      type(T_TENSOR_TYPE), IN :: self
      REAL, IN :: C_a  ! charges
      VEC(REAL,3), IN :: D_a  ! dipoles
      MAT(REAL,3,3), IN :: Q_a  !quadrupoles
      MAT3(REAL,3,3,3), IN :: O_a  ! octupoles
      VEC(REAL,4) :: res
      INT :: i,j,k
      REAL :: total_a
      res = ZERO
      ! charge-charge term
      res(1) = self%scalar*C_a

      ! dipole-charge term
      do i = 1,3
         res(2) = res(2)+self%alpha(i)*(D_a(i))
      end do

      ! charge-quadrupole/dipole-dipole term
      do i = 1,3
         do j = 1,3
            res(3) = res(3)+self%alpha_beta(i,j)*(HALF*(Q_a(i,j)))
         end do
      end do
      total_a = ZERO
      ! charge-octapole/quadrupole-dipole term
      do i = 1,3
         do j = 1,3
            do k = 1,3
               total_a = total_a+self%alpha_beta_gamma(i,j,k)*(O_a(i,j,k))
            end do
         end do
      end do
      res(4) = SIXTH*total_a
   end function

   pure function electrostatic_potential_1(self,C_a,D_a,Q_a,O_a) result (res)
   ! calculate the expectation value of the interaction operator up to l == 4
   ! excluding hexadecapole-charge term
      type(T_TENSOR_TYPE), IN :: self
      REAL, IN :: C_a  ! charges
      VEC(REAL,3), IN :: D_a  ! dipoles
      VEC(REAL,6), IN :: Q_a  !quadrupoles
      VEC(REAL,10), IN :: O_a  ! octupoles
      VEC(REAL,4) :: res
      INT :: i
      res = ZERO
      ! charge term
      res(1) = self%scalar*C_a

      ! dipole term
      do i = 1,3
         res(2) = res(2)+self%alpha(i)*(D_a(i))
      end do

      ! quadrupole term
      res(3) = res(3)+self%xx*HALF*Q_a(1)
      res(3) = res(3)+self%yy*HALF*Q_a(2)
      res(3) = res(3)+self%zz*HALF*Q_a(3)
      res(3) = res(3)+self%xy*Q_a(4)
      res(3) = res(3)+self%xz*Q_a(5)
      res(3) = res(3)+self%yz*Q_a(6)

      ! octapole term
      res(4) = res(4)+self%xxx*O_a(1)
      res(4) = res(4)+self%yyy*O_a(2)
      res(4) = res(4)+self%zzz*O_a(3)
      res(4) = res(4)*SIXTH
      res(4) = res(4)+HALF*self%xxy*O_a(4)
      res(4) = res(4)+HALF*self%xxz*O_a(5)
      res(4) = res(4)+HALF*self%yyx*O_a(6)
      res(4) = res(4)+HALF*self%yyz*O_a(7)
      res(4) = res(4)+HALF*self%zzx*O_a(8)
      res(4) = res(4)+HALF*self%zzy*O_a(9)
      res(4) = res(4)+self%xyz*O_a(10)
   end function

   PURE subroutine electrostatic_potential_grid(self,C,D,Q,O,pos,pt,grid)
   ! Evaluate the electrostatoc potential on "pt" and add into "grid"
      type(T_TENSOR_TYPE), INOUT :: self
      VEC(REAL,:), INOUT :: grid
      MAT(REAL,:,:), IN :: pt
      REAL, IN :: C  ! charges
      VEC(REAL,3), IN :: pos,D  ! dipoles
      VEC(REAL,6), IN :: Q  !quadrupoles
      VEC(REAL,10), IN :: O  ! octupoles
      INT :: n_pt,k
      VEC(REAL,4) :: V
      n_pt = size(grid)
      do k = 1,n_pt
         call create_fast_(self,pos,pt(k,:))
         V = electrostatic_potential_(self,C,D,Q,O)
         grid(k) = grid(k)-sum(V)
      end do
   end subroutine

end module
