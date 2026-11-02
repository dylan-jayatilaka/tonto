!=====================================================================
!
! GAUSSIAN_PAIR_LIST :
!
! The primitive gaussian pairs of a molecule's basis that survive the
! pair cutoff, stored flat and grouped by (l_max,l_min) class. Built
! once per geometry; the two-electron kernels loop over two class
! blocks at a time, with no per-quartet set-up.
!
! Copyright (C) Dylan Jayatilaka, 2026
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
module GAUSSIAN_PAIR_LIST_MODULE

#  include "macros"

   include "gaussian_pair_list.use"

   implicit none

   include "gaussian_pair_list.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(GAUSSIAN_PAIR_LIST_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"GAUSSIAN_PAIR_LIST:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(GAUSSIAN_PAIR_LIST_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"GAUSSIAN_PAIR_LIST:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine set_sizes(self,n_pairs,n_classes)
   ! Allocate the pair and class arrays for "n_pairs" pairs and
   ! "n_classes" classes.
      type(GAUSSIAN_PAIR_LIST_TYPE), INOUT :: self
      INT, IN :: n_pairs,n_classes
      self%n_pairs = n_pairs
      self%n_classes = n_classes
      call create_(self%class_first,n_classes)
      call create_(self%class_last,n_classes)
      call create_(self%class_l_sum,n_classes)
      call create_(self%class_first_gaussian,n_classes)
      call create_(self%class_n_gaussian,n_classes)
      self%class_first = 1
      self%class_last = 0
      self%class_l_sum = 0
      self%class_first_gaussian = 1
      self%class_n_gaussian = 1
      call create_(self%shell_pair,n_pairs)
      call create_(self%factor,n_pairs)
      call create_(self%exponent_sum,n_pairs)
      call create_(self%exponent_inv,n_pairs)
      call create_(self%prefactor,n_pairs)
      call create_(self%center,3,n_pairs)
      call create_(self%center_diff,3,n_pairs)
   end subroutine


!  ========
!  J kernel
!  ========

   PURE subroutine add_to_J(self,JJ,PP,cutoff,I_max,P_max)
   ! Add to the transfer-space coulomb vectors "JJ" the contribution of
   ! every primitive quartet, contracting at once with the transfer-space
   ! density "PP". Each unordered pair of primitive pairs is visited once.
   ! A pair of pairs is skipped when its Schwarz bound times the largest
   ! density element "P_max" of either shell pair is below "cutoff" divided
   ! by the primitive pair counts of both shell pairs. The
   ! bound of a pair is the AO bound "I_max" of its shell pair, sqrt of
   ! the largest (ab|ab), scaled by the pair's share of the largest
   ! transfer-space bound in that shell pair; the transfer coefficients
   ! are common to a shell pair's primitives, but transfer-space values
   ! alone do not bound AO contributions. Within each
   ! class the pairs are taken by decreasing bound, so the inner loop stops
   ! at the first pair that fails against the class's largest density. The
   ! survivors for one "k" are done in batches of at most "batch_max"
   ! (partner,root) entries, see add_batch_to_J.
      type(GAUSSIAN_PAIR_LIST_TYPE), IN :: self
      VEC(type(EVEC_REAL_TYPE),:), INOUT :: JJ
      VEC(type(EVEC_REAL_TYPE),:), IN :: PP
      REAL, IN :: cutoff
      VEC(REAL,:), IN :: I_max,P_max
      INT, parameter :: batch_max = 256
      MAT3(REAL,:,:,:), allocatable :: Ix,Iy,Iz
      MAT(REAL,:,:), allocatable :: rt,wt,Pg,fvp
      VEC(REAL,:), allocatable :: ev,Q,class_P_max,xv,C00x,C00y,C00z,D00x,D00y,D00z,B10,B01,B00,W
      VEC(INT,:), allocatable :: nxe,nye,nze,nxf,nyf,nzf,order,jv,n_sp,class_n_min
      type(RYS_TYPE) :: rys
      INT :: A,B,k,j,k_pos,j_pos,j_last,sp_k,n_e,n_f,fe,ff,e,f,n,n_roots,l_e,l_f,l_max
      INT :: m,i,n_big,first,last,chunk,c0,mc
      REAL :: zeta,eta,QPx,QPy,QPz,Qk,Pk,bound,cut_k,cut_B
      l_max = maxval(self%class_l_sum)
      call create_(Ix,batch_max,l_max+1,l_max+1)
      call create_(Iy,batch_max,l_max+1,l_max+1)
      call create_(Iz,batch_max,l_max+1,l_max+1)
      n = maxval(self%class_n_gaussian)
      call create_(ev,n)
      call create_(Pg,batch_max,n)
      call create_(fvp,batch_max,n)
      call create_(nxe,n); call create_(nye,n); call create_(nze,n)
      call create_(nxf,n); call create_(nyf,n); call create_(nzf,n)
      call create_(C00x,batch_max); call create_(C00y,batch_max); call create_(C00z,batch_max)
      call create_(D00x,batch_max); call create_(D00y,batch_max); call create_(D00z,batch_max)
      call create_(B10,batch_max); call create_(B01,batch_max); call create_(B00,batch_max)
      call create_(W,batch_max)
      call create_(class_P_max,self%n_classes)
      class_P_max = ZERO

      ! Primitive pairs kept per shell pair: the cutoff for a pair of pairs
      ! is divided by both counts, so what one shell quartet loses to
      ! screening stays below "cutoff", as for the shell-quartet test
      call create_(n_sp,size(I_max))
      n_sp = 0
      do k = 1,self%n_pairs
         n_sp(self%shell_pair(k)) = n_sp(self%shell_pair(k))+1
      end do
      call create_(class_n_min,self%n_classes)
      class_n_min = 1

      ! Schwarz bound of each pair; each class's pairs by decreasing bound
      call create_(Q,self%n_pairs)
      call make_AO_Schwarz_bounds_(self,Q,I_max)
      call create_(order,self%n_pairs)
      call make_class_order_(self,order,Q)
      n_big = max(maxval(self%class_last-self%class_first+1),1)
      call create_(jv,n_big)
      call create_(xv,n_big)
      do A = 1,self%n_classes
         first = self%class_first(A)
         last = self%class_last(A)
         if (last<first) cycle
         class_P_max(A) = maxval(P_max(self%shell_pair(first:last)))
         class_n_min(A) = minval(n_sp(self%shell_pair(first:last)))
      end do
      do A = 1,self%n_classes
         if (self%class_last(A)<self%class_first(A)) cycle
         l_e = self%class_l_sum(A)
         n_e = self%class_n_gaussian(A)
         fe = self%class_first_gaussian(A)-1
         do e = 1,n_e
            nxe(e) = nx(fe+e)
            nye(e) = ny(fe+e)
            nze(e) = nz(fe+e)
         end do
         do B = 1,A
            if (self%class_last(B)<self%class_first(B)) cycle
            l_f = self%class_l_sum(B)
            n_f = self%class_n_gaussian(B)
            ff = self%class_first_gaussian(B)-1
            do f = 1,n_f
               nxf(f) = nx(ff+f)
               nyf(f) = ny(ff+f)
               nzf(f) = nz(ff+f)
            end do
            n_roots = (l_e+l_f+2)/2
            call set_n_roots_(rys,n_roots)
            chunk = max(batch_max/n_roots,1)
            call destroy_(rt); call destroy_(wt)
            call create_(rt,n_roots,chunk)
            call create_(wt,n_roots,chunk)
            do k_pos = self%class_first(A),self%class_last(A)
               k = order(k_pos)
               sp_k = self%shell_pair(k)
               Qk = Q(k)
               Pk = P_max(sp_k)

               ! The largest bound in class B fails the loosest cutoff: so
               ! does every other
               cut_k = cutoff/n_sp(sp_k)
               cut_B = cut_k/class_n_min(B)
               if (Qk*Q(order(self%class_first(B)))*max(Pk,class_P_max(B))<cut_B) cycle
               zeta = self%exponent_sum(k)
               j_last = self%class_last(B)
               if (A==B) j_last = k_pos

               ! Gather the surviving partners and their X
               m = 0
               do j_pos = self%class_first(B),j_last
                  j = order(j_pos)
                  bound = Qk*Q(j)
                  if (bound*max(Pk,class_P_max(B))<cut_B) exit
                  if (bound*max(Pk,P_max(self%shell_pair(j)))<cut_k/n_sp(self%shell_pair(j))) cycle
                  m = m+1
                  jv(m) = j
                  eta = self%exponent_sum(j)
                  QPx = self%center(1,j)-self%center(1,k)
                  QPy = self%center(2,j)-self%center(2,k)
                  QPz = self%center(3,j)-self%center(3,k)
                  xv(m) = zeta*eta/(zeta+eta)*(QPx*QPx+QPy*QPy+QPz*QPz)
               end do
               do c0 = 1,m,chunk
                  mc = min(chunk,m-c0+1)
                  call get_weights_t2_(rys,mc,xv(c0:c0+mc-1),rt,wt)
                  call add_batch_to_J_(self,JJ,PP,k,jv(c0:c0+mc-1),mc,n_roots,mc*n_roots,l_e,l_f,n_e,n_f,nxe,nye,nze,nxf,nyf,nzf,rt,wt,Ix,Iy,Iz,C00x,C00y,C00z,D00x,D00y,D00z,B10,B01,B00,W,Pg,fvp,ev)
               end do
            end do
         end do
      end do
   end subroutine

   PURE subroutine add_batch_to_J(self,JJ,PP,k,jv,m,n_roots,n_p,l_e,l_f,n_e,n_f,nxe,nye,nze,nxf,nyf,nzf,rt,wt,Ix,Iy,Iz,C00x,C00y,C00z,D00x,D00y,D00z,B10,B01,B00,W,Pg,fvp,ev)
   ! Add to "JJ" the quartets of pair "k" with the "m" pairs "jv", whose
   ! roots and weights are "rt" and "wt". Each (partner,root) is one entry
   ! p of n_p = m*n_roots, and every array is laid out with p first, so the
   ! 2D recursions and the contraction run down contiguous columns. The
   ! work arrays are explicit-shape views of the caller's buffers.
      type(GAUSSIAN_PAIR_LIST_TYPE), IN :: self
      VEC(type(EVEC_REAL_TYPE),:), INOUT :: JJ
      VEC(type(EVEC_REAL_TYPE),:), IN :: PP
      INT, IN :: k,m,n_roots,n_p,l_e,l_f,n_e,n_f
      VEC(INT,m), IN :: jv
      VEC(INT,*), IN :: nxe,nye,nze
      VEC(INT,*), IN :: nxf,nyf,nzf
      MAT(REAL,n_roots,m), IN :: rt,wt
      MAT3(REAL,n_p,l_e+1,l_f+1), INOUT :: Ix,Iy,Iz
      VEC(REAL,n_p), INOUT :: C00x,C00y,C00z,D00x,D00y,D00z,B10,B01,B00,W
      MAT(REAL,n_p,n_f), INOUT :: Pg,fvp
      VEC(REAL,n_e), INOUT :: ev
      INT :: i,j,n,p,e,f,sp_k,sp_j,xe,ye,ze,xf,yf,zf
      REAL :: zeta,eta,zinv,einv,rho,rho_zinv,rho_einv,half_zinv,half_einv
      REAL :: PAx,PAy,PAz,QCx,QCy,QCz,QPx,QPy,QPz,cc,t2,t2_rz,t2_re,t,s,pk,fac
      sp_k = self%shell_pair(k)
      zeta = self%exponent_sum(k)
      zinv = self%exponent_inv(k)
      half_zinv = HALF*zinv
      PAx = self%center_diff(1,k)
      PAy = self%center_diff(2,k)
      PAz = self%center_diff(3,k)

      ! Recursion coefficients, weight and cd density for each entry
      p = 0
      do i = 1,m
         j = jv(i)
         sp_j = self%shell_pair(j)
         eta = self%exponent_sum(j)
         einv = self%exponent_inv(j)
         half_einv = HALF*einv
         QCx = self%center_diff(1,j)
         QCy = self%center_diff(2,j)
         QCz = self%center_diff(3,j)
         QPx = self%center(1,j)-self%center(1,k)
         QPy = self%center(2,j)-self%center(2,k)
         QPz = self%center(3,j)-self%center(3,k)
         rho = zeta*eta/(zeta+eta)
         rho_zinv = rho*zinv
         rho_einv = rho*einv
         cc = TWOPI5ON2*self%prefactor(k)*self%prefactor(j)*sqrt(rho)*self%factor(j)
         do n = 1,n_roots
            p = p+1
            t2 = rt(n,i)
            t2_rz = t2*rho_zinv
            t2_re = t2*rho_einv
            C00x(p) = PAx+t2_rz*QPx
            C00y(p) = PAy+t2_rz*QPy
            C00z(p) = PAz+t2_rz*QPz
            D00x(p) = QCx-t2_re*QPx
            D00y(p) = QCy-t2_re*QPy
            D00z(p) = QCz-t2_re*QPz
            B10(p) = (ONE-t2_rz)*half_zinv
            B01(p) = (ONE-t2_re)*half_einv
            B00(p) = t2_rz*half_einv
            W(p) = cc*wt(n,i)
         end do
      end do
      do f = 1,n_f
         p = 0
         do i = 1,m
            t = PP(self%shell_pair(jv(i)))%element(f)
            do n = 1,n_roots
               p = p+1
               Pg(p,f) = t
            end do
         end do
      end do
      call make_2d_batch(Ix,n_p,l_e,l_f,C00x,D00x,B10,B01,B00)
      call make_2d_batch(Iy,n_p,l_e,l_f,C00y,D00y,B10,B01,B00)
      call make_2d_batch(Iz,n_p,l_e,l_f,C00z,D00z,B10,B01,B00)
      do f = 1,l_f+1
         do e = 1,l_e+1
            do p = 1,n_p
               Iz(p,e,f) = Iz(p,e,f)*W(p)
            end do
         end do
      end do

      ! Contract: ab side with the cd density, cd side with the ab density
      ev = ZERO
      fvp = ZERO
      do f = 1,n_f
         xf = nxf(f); yf = nyf(f); zf = nzf(f)
         do e = 1,n_e
            xe = nxe(e); ye = nye(e); ze = nze(e)
            pk = PP(sp_k)%element(e)
            s = ZERO
            do p = 1,n_p
               t = Ix(p,xe,xf)*Iy(p,ye,yf)*Iz(p,ze,zf)
               s = s+t*Pg(p,f)
               fvp(p,f) = fvp(p,f)+t*pk
            end do
            ev(e) = ev(e)+s
         end do
      end do
      fac = self%factor(k)
      JJ(sp_k)%element(1:n_e) = JJ(sp_k)%element(1:n_e)+fac*ev
      p = 0
      do i = 1,m
         j = jv(i)
         if (j==k) then
            p = p+n_roots
            cycle
         end if
         sp_j = self%shell_pair(j)
         do n = 1,n_roots
            p = p+1
            JJ(sp_j)%element(1:n_f) = JJ(sp_j)%element(1:n_f)+fac*fvp(p,1:n_f)
         end do
      end do
   end subroutine

   PURE subroutine make_2d_batch(I,n_p,l_e,l_f,C00,D00,B10,B01,B00)
   ! Make the 2D Rys integrals "I(p,e+1,f+1)", e<=l_e, f<=l_f, for one
   ! cartesian direction and all n_p entries p, by the vertical recursions.
      INT, IN :: n_p,l_e,l_f
      MAT3(REAL,n_p,l_e+1,l_f+1), INOUT :: I
      VEC(REAL,n_p), IN :: C00,D00,B10,B01,B00
      INT :: p,e,f
      do p = 1,n_p
         I(p,1,1) = ONE
      end do
      if (l_e>0) then
         do p = 1,n_p
            I(p,2,1) = C00(p)
         end do
      end if
      do e = 2,l_e
         do p = 1,n_p
            I(p,e+1,1) = C00(p)*I(p,e,1)+(e-1)*B10(p)*I(p,e-1,1)
         end do
      end do
      do f = 1,l_f
         if (f==1) then
            do p = 1,n_p
               I(p,1,2) = D00(p)
            end do
         else
            do p = 1,n_p
               I(p,1,f+1) = D00(p)*I(p,1,f)+(f-1)*B01(p)*I(p,1,f-1)
            end do
         end if
         if (l_e>0) then
            do p = 1,n_p
               I(p,2,f+1) = C00(p)*I(p,1,f+1)+f*B00(p)*I(p,1,f)
            end do
         end if
         do e = 2,l_e
            do p = 1,n_p
               I(p,e+1,f+1) = C00(p)*I(p,e,f+1)+(e-1)*B10(p)*I(p,e-1,f+1)+f*B00(p)*I(p,e,f)
            end do
         end do
      end do
   end subroutine


!  ==========================
!  Density fitting of J (RI-J)
!  ==========================

   PURE subroutine add_to_RI_J(self,aux,make_d,d,PP,JJ,c,cutoff,I_max,w_max)
   ! One pass of a density-fitted coulomb build over the three-center
   ! integrals (k|j), j a pair of "self" and k an auxiliary primitive of
   ! "aux": a list of one-center pairs of class (L,0) whose .shell_pair
   ! is the auxiliary shell. If "make_d", add to "d" the integrals of
   ! the auxiliary functions with the transfer-space density "PP":
   ! d(s).element(e) is for cartesian component e of auxiliary shell s.
   ! Otherwise add to the transfer-space coulomb vectors "JJ" the
   ! integrals contracted with the fitting coefficients "c", laid out as
   ! "d"; "JJ" is then finished as in add_to_J, but without the last
   ! factor of two. An integral is skipped when its bound times a weight
   ! is below "cutoff" divided by the primitive counts of its shell pair
   ! and its auxiliary shell. The weight is "w_max": for "d" the largest
   ! density element of each shell pair, for "JJ" the largest coefficient
   ! of each auxiliary shell. The bounds are as in add_to_J, from "I_max".
      type(GAUSSIAN_PAIR_LIST_TYPE), IN :: self
      type(GAUSSIAN_PAIR_LIST_TYPE), IN :: aux
      BIN, IN :: make_d
      VEC(type(EVEC_REAL_TYPE),:), INOUT :: d,JJ
      VEC(type(EVEC_REAL_TYPE),:), IN :: PP,c
      REAL, IN :: cutoff
      VEC(REAL,:), IN :: I_max,w_max
      INT, parameter :: batch_max = 256
      MAT3(REAL,:,:,:), allocatable :: Ix,Iy,Iz
      MAT(REAL,:,:), allocatable :: rt,wt,Pg,fvp
      VEC(REAL,:), allocatable :: ev,Q,Q_aux,class_w_max,xv,C00x,C00y,C00z,D00x,D00y,D00z,B10,B01,B00,W
      VEC(INT,:), allocatable :: nxe,nye,nze,nxf,nyf,nzf,order,jv,n_sp,n_as,class_n_max
      type(RYS_TYPE) :: rys
      INT :: A,B,k,j,j_pos,s_k,sp_j,n_e,n_f,fe,ff,e,f,n,n_roots,l_e,l_f
      INT :: m,n_big,first,last,chunk,c0,mc
      REAL :: zeta,eta,QPx,QPy,QPz,bound,cut_k,wk
      l_e = maxval(aux%class_l_sum)
      l_f = maxval(self%class_l_sum)
      call create_(Ix,batch_max,l_e+1,l_f+1)
      call create_(Iy,batch_max,l_e+1,l_f+1)
      call create_(Iz,batch_max,l_e+1,l_f+1)
      n = maxval(aux%class_n_gaussian)
      call create_(ev,n)
      call create_(nxe,n); call create_(nye,n); call create_(nze,n)
      n = maxval(self%class_n_gaussian)
      call create_(Pg,batch_max,n)
      call create_(fvp,batch_max,n)
      call create_(nxf,n); call create_(nyf,n); call create_(nzf,n)
      call create_(C00x,batch_max); call create_(C00y,batch_max); call create_(C00z,batch_max)
      call create_(D00x,batch_max); call create_(D00y,batch_max); call create_(D00z,batch_max)
      call create_(B10,batch_max); call create_(B01,batch_max); call create_(B00,batch_max)
      call create_(W,batch_max)

      ! Primitives kept per shell pair, and per auxiliary shell
      call create_(n_sp,size(I_max))
      n_sp = 0
      do j = 1,self%n_pairs
         n_sp(self%shell_pair(j)) = n_sp(self%shell_pair(j))+1
      end do
      call create_(n_as,size(d))
      n_as = 0
      do k = 1,aux%n_pairs
         n_as(aux%shell_pair(k)) = n_as(aux%shell_pair(k))+1
      end do

      ! Bounds; each class's pairs by decreasing bound
      call create_(Q,self%n_pairs)
      call make_AO_Schwarz_bounds_(self,Q,I_max)
      call create_(order,self%n_pairs)
      call make_class_order_(self,order,Q)
      call create_(Q_aux,aux%n_pairs)
      call make_Schwarz_bounds_(aux,Q_aux)
      n_big = max(maxval(self%class_last-self%class_first+1),1)
      call create_(jv,n_big)
      call create_(xv,n_big)
      call create_(class_w_max,self%n_classes)
      call create_(class_n_max,self%n_classes)
      class_w_max = ZERO
      class_n_max = 1
      do B = 1,self%n_classes
         first = self%class_first(B)
         last = self%class_last(B)
         if (last<first) cycle
         if (make_d) class_w_max(B) = maxval(w_max(self%shell_pair(first:last)))
         class_n_max(B) = maxval(n_sp(self%shell_pair(first:last)))
      end do
      do A = 1,aux%n_classes
         if (aux%class_last(A)<aux%class_first(A)) cycle
         l_e = aux%class_l_sum(A)
         n_e = aux%class_n_gaussian(A)
         fe = aux%class_first_gaussian(A)-1
         do e = 1,n_e
            nxe(e) = nx(fe+e)
            nye(e) = ny(fe+e)
            nze(e) = nz(fe+e)
         end do
         do B = 1,self%n_classes
            if (self%class_last(B)<self%class_first(B)) cycle
            l_f = self%class_l_sum(B)
            n_f = self%class_n_gaussian(B)
            ff = self%class_first_gaussian(B)-1
            do f = 1,n_f
               nxf(f) = nx(ff+f)
               nyf(f) = ny(ff+f)
               nzf(f) = nz(ff+f)
            end do
            n_roots = (l_e+l_f+2)/2
            call set_n_roots_(rys,n_roots)
            chunk = max(batch_max/n_roots,1)
            call destroy_(rt); call destroy_(wt)
            call create_(rt,n_roots,chunk)
            call create_(wt,n_roots,chunk)
            do k = aux%class_first(A),aux%class_last(A)
               s_k = aux%shell_pair(k)
               cut_k = cutoff/n_as(s_k)
               zeta = aux%exponent_sum(k)
               if (make_d) then; wk = class_w_max(B)
               else; wk = w_max(s_k)
               end if

               ! Gather the surviving partners and their X
               m = 0
               do j_pos = self%class_first(B),self%class_last(B)
                  j = order(j_pos)
                  sp_j = self%shell_pair(j)
                  bound = Q_aux(k)*Q(j)
                  if (bound*wk<cut_k/class_n_max(B)) exit
                  if (make_d) then
                     if (bound*w_max(sp_j)<cut_k/n_sp(sp_j)) cycle
                  else
                     if (bound*wk<cut_k/n_sp(sp_j)) cycle
                  end if
                  m = m+1
                  jv(m) = j
                  eta = self%exponent_sum(j)
                  QPx = self%center(1,j)-aux%center(1,k)
                  QPy = self%center(2,j)-aux%center(2,k)
                  QPz = self%center(3,j)-aux%center(3,k)
                  xv(m) = zeta*eta/(zeta+eta)*(QPx*QPx+QPy*QPy+QPz*QPz)
               end do
               do c0 = 1,m,chunk
                  mc = min(chunk,m-c0+1)
                  call get_weights_t2_(rys,mc,xv(c0:c0+mc-1),rt,wt)
                  call add_batch_to_RI_J_(self,aux,make_d,d,PP,JJ,c,k,jv(c0:c0+mc-1),mc,n_roots,mc*n_roots,l_e,l_f,n_e,n_f,nxe,nye,nze,nxf,nyf,nzf,rt,wt,Ix,Iy,Iz,C00x,C00y,C00z,D00x,D00y,D00z,B10,B01,B00,W,Pg,fvp,ev)
               end do
            end do
         end do
      end do
   end subroutine

   PURE subroutine add_batch_to_RI_J(self,aux,make_d,d,PP,JJ,c,k,jv,m,n_roots,n_p,l_e,l_f,n_e,n_f,nxe,nye,nze,nxf,nyf,nzf,rt,wt,Ix,Iy,Iz,C00x,C00y,C00z,D00x,D00y,D00z,B10,B01,B00,W,Pg,fvp,ev)
   ! The integrals of auxiliary primitive "k" of "aux" with the "m"
   ! pairs "jv", contracted as add_to_RI_J describes. The layout is that
   ! of add_batch_to_J, the auxiliary primitive on the e side.
      type(GAUSSIAN_PAIR_LIST_TYPE), IN :: self
      type(GAUSSIAN_PAIR_LIST_TYPE), IN :: aux
      BIN, IN :: make_d
      VEC(type(EVEC_REAL_TYPE),:), INOUT :: d,JJ
      VEC(type(EVEC_REAL_TYPE),:), IN :: PP,c
      INT, IN :: k,m,n_roots,n_p,l_e,l_f,n_e,n_f
      VEC(INT,m), IN :: jv
      VEC(INT,*), IN :: nxe,nye,nze
      VEC(INT,*), IN :: nxf,nyf,nzf
      MAT(REAL,n_roots,m), IN :: rt,wt
      MAT3(REAL,n_p,l_e+1,l_f+1), INOUT :: Ix,Iy,Iz
      VEC(REAL,n_p), INOUT :: C00x,C00y,C00z,D00x,D00y,D00z,B10,B01,B00,W
      MAT(REAL,n_p,n_f), INOUT :: Pg,fvp
      VEC(REAL,n_e), INOUT :: ev
      INT :: i,j,n,p,e,f,s_k,sp_j,xe,ye,ze,xf,yf,zf
      REAL :: zeta,eta,zinv,einv,rho,rho_zinv,rho_einv,half_zinv,half_einv
      REAL :: PAx,PAy,PAz,QCx,QCy,QCz,QPx,QPy,QPz,cc,t2,t2_rz,t2_re,t,s,ck
      s_k = aux%shell_pair(k)
      zeta = aux%exponent_sum(k)
      zinv = aux%exponent_inv(k)
      half_zinv = HALF*zinv
      PAx = aux%center_diff(1,k)
      PAy = aux%center_diff(2,k)
      PAz = aux%center_diff(3,k)

      ! Recursion coefficients and weight for each entry
      p = 0
      do i = 1,m
         j = jv(i)
         eta = self%exponent_sum(j)
         einv = self%exponent_inv(j)
         half_einv = HALF*einv
         QCx = self%center_diff(1,j)
         QCy = self%center_diff(2,j)
         QCz = self%center_diff(3,j)
         QPx = self%center(1,j)-aux%center(1,k)
         QPy = self%center(2,j)-aux%center(2,k)
         QPz = self%center(3,j)-aux%center(3,k)
         rho = zeta*eta/(zeta+eta)
         rho_zinv = rho*zinv
         rho_einv = rho*einv
         cc = TWOPI5ON2*aux%prefactor(k)*self%prefactor(j)*sqrt(rho)*self%factor(j)
         do n = 1,n_roots
            p = p+1
            t2 = rt(n,i)
            t2_rz = t2*rho_zinv
            t2_re = t2*rho_einv
            C00x(p) = PAx+t2_rz*QPx
            C00y(p) = PAy+t2_rz*QPy
            C00z(p) = PAz+t2_rz*QPz
            D00x(p) = QCx-t2_re*QPx
            D00y(p) = QCy-t2_re*QPy
            D00z(p) = QCz-t2_re*QPz
            B10(p) = (ONE-t2_rz)*half_zinv
            B01(p) = (ONE-t2_re)*half_einv
            B00(p) = t2_rz*half_einv
            W(p) = cc*wt(n,i)
         end do
      end do
      call make_2d_batch(Ix,n_p,l_e,l_f,C00x,D00x,B10,B01,B00)
      call make_2d_batch(Iy,n_p,l_e,l_f,C00y,D00y,B10,B01,B00)
      call make_2d_batch(Iz,n_p,l_e,l_f,C00z,D00z,B10,B01,B00)
      do f = 1,l_f+1
         do e = 1,l_e+1
            do p = 1,n_p
               Iz(p,e,f) = Iz(p,e,f)*W(p)
            end do
         end do
      end do
      if (make_d) then

         ! The density of each partner, then the sum over all of them.
         ! The factor of two: a shell pair stands for (ab| and (ba|
         do f = 1,n_f
            p = 0
            do i = 1,m
               t = PP(self%shell_pair(jv(i)))%element(f)
               do n = 1,n_roots
                  p = p+1
                  Pg(p,f) = t
               end do
            end do
         end do
         ev = ZERO
         do f = 1,n_f
            xf = nxf(f); yf = nyf(f); zf = nzf(f)
            do e = 1,n_e
               xe = nxe(e); ye = nye(e); ze = nze(e)
               s = ZERO
               do p = 1,n_p
                  s = s+Ix(p,xe,xf)*Iy(p,ye,yf)*Iz(p,ze,zf)*Pg(p,f)
               end do
               ev(e) = ev(e)+s
            end do
         end do
         d(s_k)%element(1:n_e) = d(s_k)%element(1:n_e)+TWO*ev
      else
         fvp = ZERO
         do f = 1,n_f
            xf = nxf(f); yf = nyf(f); zf = nzf(f)
            do e = 1,n_e
               xe = nxe(e); ye = nye(e); ze = nze(e)
               ck = c(s_k)%element(e)
               do p = 1,n_p
                  fvp(p,f) = fvp(p,f)+Ix(p,xe,xf)*Iy(p,ye,yf)*Iz(p,ze,zf)*ck
               end do
            end do
         end do
         p = 0
         do i = 1,m
            sp_j = self%shell_pair(jv(i))
            do n = 1,n_roots
               p = p+1
               JJ(sp_j)%element(1:n_f) = JJ(sp_j)%element(1:n_f)+fvp(p,1:n_f)
            end do
         end do
      end if
   end subroutine

   PURE subroutine make_coulomb_metric(self,V,first)
   ! For a list of auxiliary primitives, as in add_to_RI_J, make "V",
   ! the coulomb integrals (s e|s' f) between the contracted cartesian
   ! auxiliary functions. Function e of auxiliary shell s is row
   ! first(s)+e-1 of "V".
      type(GAUSSIAN_PAIR_LIST_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: V
      VEC(INT,:), IN :: first
      MAT(REAL,:,:), allocatable :: Ix,Iy,Iz
      VEC(INT,:), allocatable :: nxe,nye,nze,nxf,nyf,nzf
      type(RYS_TYPE) :: rys
      INT :: A,B,k,j,e,f,n,n_e,n_f,fe,ff,l_e,l_f,l_max,row,col
      REAL :: zeta,eta,rho,rho_zinv,rho_einv,half_zinv,half_einv,t2_rz,t2_re,B10,B01,B00,cc,w
      VEC(REAL,3) :: QP
      l_max = maxval(self%class_l_sum)
      call create_(Ix,l_max+1,l_max+1)
      call create_(Iy,l_max+1,l_max+1)
      call create_(Iz,l_max+1,l_max+1)
      n = maxval(self%class_n_gaussian)
      call create_(nxe,n); call create_(nye,n); call create_(nze,n)
      call create_(nxf,n); call create_(nyf,n); call create_(nzf,n)
      V = ZERO
      do A = 1,self%n_classes
         if (self%class_last(A)<self%class_first(A)) cycle
         l_e = self%class_l_sum(A)
         n_e = self%class_n_gaussian(A)
         fe = self%class_first_gaussian(A)-1
         do e = 1,n_e
            nxe(e) = nx(fe+e)
            nye(e) = ny(fe+e)
            nze(e) = nz(fe+e)
         end do
         do B = 1,self%n_classes
            if (self%class_last(B)<self%class_first(B)) cycle
            l_f = self%class_l_sum(B)
            n_f = self%class_n_gaussian(B)
            ff = self%class_first_gaussian(B)-1
            do f = 1,n_f
               nxf(f) = nx(ff+f)
               nyf(f) = ny(ff+f)
               nzf(f) = nz(ff+f)
            end do
            call set_n_roots_(rys,(l_e+l_f+2)/2)
            do k = self%class_first(A),self%class_last(A)
               zeta = self%exponent_sum(k)
               half_zinv = HALF*self%exponent_inv(k)
               row = first(self%shell_pair(k))-1
               do j = self%class_first(B),self%class_last(B)
                  eta = self%exponent_sum(j)
                  half_einv = HALF*self%exponent_inv(j)
                  col = first(self%shell_pair(j))-1
                  QP = self%center(:,j)-self%center(:,k)
                  rho = zeta*eta/(zeta+eta)
                  rho_zinv = rho*self%exponent_inv(k)
                  rho_einv = rho*self%exponent_inv(j)
                  cc = TWOPI5ON2*self%prefactor(k)*self%prefactor(j)*sqrt(rho)
                  call get_weights_(rys,rho*dot_product(QP,QP))
                  do n = 1,rys%n_roots
                     t2_rz = rys%root(n)*rho_zinv
                     t2_re = rys%root(n)*rho_einv
                     B10 = (ONE-t2_rz)*half_zinv
                     B01 = (ONE-t2_re)*half_einv
                     B00 = t2_rz*half_einv
                     call make_2d(Ix,l_e,l_f,t2_rz*QP(1),-t2_re*QP(1),B10,B01,B00)
                     call make_2d(Iy,l_e,l_f,t2_rz*QP(2),-t2_re*QP(2),B10,B01,B00)
                     call make_2d(Iz,l_e,l_f,t2_rz*QP(3),-t2_re*QP(3),B10,B01,B00)
                     w = cc*rys%weight(n)
                     do f = 1,n_f
                        do e = 1,n_e
                           V(row+e,col+f) = V(row+e,col+f)+w*Ix(nxe(e),nxf(f))*Iy(nye(e),nyf(f))*Iz(nze(e),nzf(f))
                        end do
                     end do
                  end do
               end do
            end do
         end do
      end do
   end subroutine


!  ==================================
!  Potential integrals at points (COSX)
!  ==================================

   PURE subroutine make_point_potentials(self,E,cls,first,last,Q,Q_min,n_pt,n_roots,n_p,l_f,n_f,x,y,z,xv,rt,wt,Ix,Iy,Iz,Dx,Dy,Dz,B01,W)
   ! Set "E(g,f)" to the integral of the (es| component f with
   ! 1/|r-r_g|, r_g = (x(g),y(g),z(g)), summed over the primitive pairs
   ! "first" to "last", which must belong to one shell pair, of class
   ! "cls"; a primitive pair whose bound "Q" is below "Q_min" is left
   ! out. A point is the limit of an auxiliary s primitive of infinite
   ! exponent in add_batch_to_RI_J, so there is no recursion on its
   ! side. l_f, n_f and n_roots = (l_f+2)/2 are those of the class. Entry
   ! p = (n-1)*n_pt+g of n_p = n_pt*n_roots is root n at point g, so the
   ! loops over points are contiguous. The work arrays are
   ! explicit-shape views of the caller's buffers.
      type(GAUSSIAN_PAIR_LIST_TYPE), IN :: self
      INT, IN :: cls,first,last,n_pt,n_roots,n_p,l_f,n_f
      VEC(REAL,:), IN :: Q
      REAL, IN :: Q_min
      MAT(REAL,n_pt,n_f), OUT :: E
      VEC(REAL,n_pt), IN :: x,y,z
      VEC(REAL,n_pt), INOUT :: xv
      MAT(REAL,n_roots,n_pt), INOUT :: rt,wt
      MAT(REAL,n_p,l_f+1), INOUT :: Ix,Iy,Iz
      VEC(REAL,n_p), INOUT :: Dx,Dy,Dz,B01,W
      type(RYS_TYPE) :: rys
      INT :: k,g,n,p,f,ff,xf,yf,zf,p0
      REAL :: zeta,half_zinv,cc,Px,Py,Pz,PCx,PCy,PCz,t2
      ff = self%class_first_gaussian(cls)-1
      call set_n_roots_(rys,n_roots)
      E = ZERO
      do k = first,last
         if (Q(k)<Q_min) cycle
         zeta = self%exponent_sum(k)
         half_zinv = HALF*self%exponent_inv(k)
         cc = TWO*PI*self%prefactor(k)*sqrt(zeta)
         Px = self%center(1,k)
         Py = self%center(2,k)
         Pz = self%center(3,k)
         PCx = self%center_diff(1,k)
         PCy = self%center_diff(2,k)
         PCz = self%center_diff(3,k)
         do g = 1,n_pt
            xv(g) = zeta*((Px-x(g))*(Px-x(g))+(Py-y(g))*(Py-y(g))+(Pz-z(g))*(Pz-z(g)))
         end do
         call get_weights_t2_(rys,n_pt,xv,rt,wt)
         p = 0
         do n = 1,n_roots
            do g = 1,n_pt
               p = p+1
               t2 = rt(n,g)
               Dx(p) = PCx-t2*(Px-x(g))
               Dy(p) = PCy-t2*(Py-y(g))
               Dz(p) = PCz-t2*(Pz-z(g))
               B01(p) = (ONE-t2)*half_zinv
               W(p) = cc*wt(n,g)
            end do
         end do
         call make_1d_batch(Ix,n_p,l_f,Dx,B01)
         call make_1d_batch(Iy,n_p,l_f,Dy,B01)
         call make_1d_batch(Iz,n_p,l_f,Dz,B01)
         do f = 1,l_f+1
            do p = 1,n_p
               Iz(p,f) = Iz(p,f)*W(p)
            end do
         end do
         do f = 1,n_f
            xf = nx(ff+f)
            yf = ny(ff+f)
            zf = nz(ff+f)
            do n = 1,n_roots
               p0 = (n-1)*n_pt
               do g = 1,n_pt
                  E(g,f) = E(g,f)+Ix(p0+g,xf)*Iy(p0+g,yf)*Iz(p0+g,zf)
               end do
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_1d_batch(I,n_p,l_f,D00,B01)
   ! Make the 1D Rys integrals "I(p,f+1)", f<=l_f, for one cartesian
   ! direction and all n_p entries p: make_2d_batch with l_e = 0.
      INT, IN :: n_p,l_f
      MAT(REAL,n_p,l_f+1), INOUT :: I
      VEC(REAL,n_p), IN :: D00,B01
      INT :: p,f
      do p = 1,n_p
         I(p,1) = ONE
      end do
      if (l_f>0) then
         do p = 1,n_p
            I(p,2) = D00(p)
         end do
      end if
      do f = 2,l_f
         do p = 1,n_p
            I(p,f+1) = D00(p)*I(p,f)+(f-1)*B01(p)*I(p,f-1)
         end do
      end do
   end subroutine


!  ======
!  Bounds
!  ======

   PURE subroutine make_AO_Schwarz_bounds(self,Q,I_max)
   ! Set "Q(k)" to the bound of pair "k" in the AO basis: the AO bound
   ! "I_max" of its shell pair, scaled by the pair's share of the largest
   ! transfer-space bound in that shell pair. Transfer-space values alone
   ! do not bound AO contributions.
      type(GAUSSIAN_PAIR_LIST_TYPE), IN :: self
      VEC(REAL,self%n_pairs), OUT :: Q
      VEC(REAL,:), IN :: I_max
      VEC(REAL,:), allocatable :: Q_sp
      INT :: k,sp_k
      call make_Schwarz_bounds_(self,Q)
      call create_(Q_sp,size(I_max))
      Q_sp = ZERO
      do k = 1,self%n_pairs
         sp_k = self%shell_pair(k)
         Q_sp(sp_k) = max(Q_sp(sp_k),Q(k))
      end do
      do k = 1,self%n_pairs
         sp_k = self%shell_pair(k)
         if (Q_sp(sp_k)>ZERO) Q(k) = I_max(sp_k)*Q(k)/Q_sp(sp_k)
      end do
      call destroy_(Q_sp)
   end subroutine

   PURE subroutine make_class_order(self,order,Q)
   ! Set "order" so that order(first:last) lists the pairs of each
   ! class by decreasing "Q".
      type(GAUSSIAN_PAIR_LIST_TYPE), IN :: self
      VEC(INT,self%n_pairs), OUT :: order
      VEC(REAL,self%n_pairs), IN :: Q
      VEC(REAL,:), allocatable :: Qc
      VEC(INT,:), allocatable :: idx
      INT :: A,first,last
      do A = 1,self%n_classes
         first = self%class_first(A)
         last = self%class_last(A)
         if (last<first) cycle
         call create_(Qc,last-first+1)
         call create_(idx,last-first+1)
         Qc = Q(first:last)
         call quick_sort_(Qc,idx,decreasing_order=TRUE)
         order(first:last) = first-1+idx
         call destroy_(idx)
         call destroy_(Qc)
      end do
   end subroutine

   PURE subroutine make_Schwarz_bounds(self,Q)
   ! Set "Q(k)" to the square root of the largest diagonal integral
   ! (e_k|e_k) of pair "k" over its (es| components, so that every
   ! (e_k|f_j) is bounded by Q(k)*Q(j).
      type(GAUSSIAN_PAIR_LIST_TYPE), IN :: self
      VEC(REAL,self%n_pairs), OUT :: Q
      MAT(REAL,:,:), allocatable :: Ix,Iy,Iz
      VEC(INT,:), allocatable :: nxe,nye,nze
      type(RYS_TYPE) :: rys
      INT :: A,k,e,n,n_e,fe,l_e,l_max
      VEC(REAL,:), allocatable :: diag
      REAL :: zinv,half_zinv,t2_rz,B10,B00,cc
      l_max = maxval(self%class_l_sum)
      call create_(Ix,l_max+1,l_max+1)
      call create_(Iy,l_max+1,l_max+1)
      call create_(Iz,l_max+1,l_max+1)
      n = maxval(self%class_n_gaussian)
      call create_(nxe,n); call create_(nye,n); call create_(nze,n)
      call create_(diag,n)
      do A = 1,self%n_classes
         if (self%class_last(A)<self%class_first(A)) cycle
         l_e = self%class_l_sum(A)
         n_e = self%class_n_gaussian(A)
         fe = self%class_first_gaussian(A)-1
         do e = 1,n_e
            nxe(e) = nx(fe+e)
            nye(e) = ny(fe+e)
            nze(e) = nz(fe+e)
         end do
         call set_n_roots_(rys,l_e+1)
         call get_weights_(rys,ZERO)
         do k = self%class_first(A),self%class_last(A)

            ! Both sides the same pair: X = 0, rho = zeta/2, QP = 0
            zinv = self%exponent_inv(k)
            half_zinv = HALF*zinv
            cc = TWOPI5ON2*self%prefactor(k)*self%prefactor(k)*sqrt(HALF*self%exponent_sum(k))
            diag(1:n_e) = ZERO
            do n = 1,l_e+1
               t2_rz = HALF*rys%root(n)
               B10 = (ONE-t2_rz)*half_zinv
               B00 = t2_rz*half_zinv
               call make_2d(Ix,l_e,l_e,self%center_diff(1,k),self%center_diff(1,k),B10,B10,B00)
               call make_2d(Iy,l_e,l_e,self%center_diff(2,k),self%center_diff(2,k),B10,B10,B00)
               call make_2d(Iz,l_e,l_e,self%center_diff(3,k),self%center_diff(3,k),B10,B10,B00)
               do e = 1,n_e
                  diag(e) = diag(e)+rys%weight(n)*Ix(nxe(e),nxe(e))*Iy(nye(e),nye(e))*Iz(nze(e),nze(e))
               end do
            end do
            Q(k) = sqrt(abs(cc*maxval(diag(1:n_e))))
         end do
      end do
   end subroutine

   PURE subroutine make_2d(I,l_e,l_f,C00,D00,B10,B01,B00)
   ! Make the 2D Rys integrals "I(e+1,f+1)", e<=l_e, f<=l_f, for one
   ! cartesian direction by the vertical recursions.
      MAT(REAL,:,:), INOUT :: I
      INT, IN :: l_e,l_f
      REAL, IN :: C00,D00,B10,B01,B00
      INT :: e,f
      I(1,1) = ONE
      if (l_e>0) I(2,1) = C00
      do e = 2,l_e
         I(e+1,1) = C00*I(e,1)+(e-1)*B10*I(e-1,1)
      end do
      do f = 1,l_f
         if (f==1) then; I(1,2) = D00
         else; I(1,f+1) = D00*I(1,f)+(f-1)*B01*I(1,f-1)
         end if
         do e = 1,l_e
            I(e+1,f+1) = C00*I(e,f+1)+f*B00*I(e,f)
            if (e>1) I(e+1,f+1) = I(e+1,f+1)+(e-1)*B10*I(e-1,f+1)
         end do
      end do
   end subroutine

end module
