!---------------------------------------------------------------------------
!
! FOURIER_SUMS: Fourier transforms of a density given on a set of points.
!
! Plain procedures belonging to no object: the form factor routines in
! MOLECULE.RHO and MOLECULE.HAR prepare the points and weights, and call
! these to do the sums.
!
! fitted_exp_ikr_sums gives the same sum from a fit of the density in
! functions on one centre, whose transform is analytic: the oc-ri
! partition model. It uses BLAS, and no sin or cos except for the phase.
!
! WHY THIS FILE IS WRITTEN AS IT IS
!
! The sum f(k) = sum_i w_i exp(i k.r_i) takes most of the time of a HAR
! job (81.6% of a fragHAR job), and nearly all of that is the n_k x n_pt
! cosines and sines. Six ways of doing it were timed, on 8000 k and 6000
! points, with scripts/ff_loop_bench.f90. Nanoseconds per term:
!
!                                 Mac    Linux   Linux     Linux, flags
!                                 M2     -Ofast  as below  below + native
!   A  k outside, complex exp     18.3   10-12.6  34.2     33.3
!   B  real cos and sin sums      (no better than A)
!   C  tiles of k: DGEMM for k.r,
!      cos/sin, DGEMV with w      17.5   14.1     14.0      7.4
!   D  points outside, k inside   18.5   12.4     12.4      3.8
!   E  as D, own sin/cos, using
!      integer rounding            2.0   18.1     18.9      2.4
!   F  as E, real arithmetic only  3.2   wrong     9.5      3.0
!
! (Mac: -Ofast -mcpu=apple-m2, with -fno-associative-math for E and F.
! "Linux, as below" is generic x86-64 with this file's flags.)
!
! What the table says:
! - The cost is sin and cos, not the dot products, so the question is
!   whether the compiler can vectorise them: compute several at once in
!   the processor's vector instructions.
! - A is fast on Linux only because glibc supplies vector sin and cos
!   that gfortran calls under -Ofast. macOS has none, hence 18 ns.
! - Loops swapped (D) put k innermost, the shape that vectorises.
! - E and F compute sin and cos in Fortran, so any compiler can vectorise
!   them. E rounds with integer instructions, which generic x86-64 vector
!   code lacks; F rounds by adding and subtracting 1.5*2^52, and so
!   vectorises everywhere. F is used: fastest or near it on every build.
! - BLAS tiling (C) never wins, and was dropped.
!
! COMPILER FLAGS. -Ofast breaks F: it regroups the three-part pi/2
! reduction (errors up to 207 units in the last place) and removes the
! 1.5*2^52 rounding altogether. -fno-associative-math alone is not
! enough. So this file is compiled -O3 -fno-fast-math -fno-math-errno
! -fno-trapping-math in every optimised build (CMakeLists.txt). With
! those flags sin_cos is within 1 unit in the last place for |x| <= 200,
! and the form factors agree with the library sin and cos to ~3e-16.
!
! These results belong to gfortran 14 on these processors. Another
! compiler, or another language, may vectorise the library sin and cos
! itself, and make sin_cos unnecessary: time it again before porting.
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
!
!---------------------------------------------------------------------------
module FOURIER_SUMS_MODULE

#  include "macros"

   include "fourier_sums.use"

   implicit none

   include "fourier_sums.int"


   ! Constants for sin_cos: 2/pi, 1.5*2^52 for rounding, pi/2 in three parts
   ! (Cody-Waite), and the Cephes polynomial coefficients for sin and cos.
   REAL, parameter :: sc_2_on_pi = 0.63661977236758134308d0
   REAL, parameter :: sc_big = 6755399441055744.0d0
   REAL, parameter :: sc_p1 = 1.57079625129699707031d0
   REAL, parameter :: sc_p2 = 7.54978941586159635335d-8
   REAL, parameter :: sc_p3 = 5.39030285815811905290d-15
   REAL, parameter :: sc_s1 = -1.66666666666666307295d-1
   REAL, parameter :: sc_s2 = 8.33333333332211858878d-3
   REAL, parameter :: sc_s3 = -1.98412698295895385996d-4
   REAL, parameter :: sc_s4 = 2.75573136213857245213d-6
   REAL, parameter :: sc_s5 = -2.50507477628578072866d-8
   REAL, parameter :: sc_s6 = 1.58962301576546568060d-10
   REAL, parameter :: sc_c1 = 4.16666666666665929218d-2
   REAL, parameter :: sc_c2 = -1.38888888888730564116d-3
   REAL, parameter :: sc_c3 = 2.48015872888517045348d-5
   REAL, parameter :: sc_c4 = -2.75573141792967388112d-7
   REAL, parameter :: sc_c5 = 2.08757008419747316778d-9
   REAL, parameter :: sc_c6 = -1.13585365213876817300d-11

contains

   PURE subroutine exp_ikr_sums(res,k_pts,pt,w)
   ! Set "res(k)" to the sum over points "i" of w(i)*exp(i k.r_i),
   ! where "k" is row k of "k_pts" and "r_i" is row i of "pt".
   ! The loop over k is innermost so that it vectorises, sin and cos
   ! included; each k still sums its points in order.
      VEC(CPX,:), OUT :: res
      MAT(REAL,:,:), IN :: k_pts
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), IN :: w
      VEC(REAL,:), allocatable :: re,im
      REAL :: x,y,z,wi,kr,s,c
      INT :: k,i
   ENSURE(size(res)==size(k_pts,1),"FOURIER_SUMS:exp_ikr_sums ... res and k_pts: different number of k")
   ENSURE(size(k_pts,2)==3,"FOURIER_SUMS:exp_ikr_sums ... k_pts: need 3 columns")
   ENSURE(size(pt,2)==3,"FOURIER_SUMS:exp_ikr_sums ... pt: need 3 columns")
   ENSURE(size(w)==size(pt,1),"FOURIER_SUMS:exp_ikr_sums ... w and pt: different number of points")
      call create_(re,size(k_pts,1)); re = ZERO
      call create_(im,size(k_pts,1)); im = ZERO
      do i = 1,size(pt,1)
         x = pt(i,1)
         y = pt(i,2)
         z = pt(i,3)
         wi = w(i)
         do k = 1,size(k_pts,1)
            kr = k_pts(k,1)*x+k_pts(k,2)*y+k_pts(k,3)*z
            call sin_cos(kr,s,c)
            re(k) = re(k)+wi*c
            im(k) = im(k)+wi*s
         end do
      end do
      res = COMPLEXIFY(re,im)
      call destroy_(im)
      call destroy_(re)
   end subroutine

   PURE subroutine fourier_series_at(res,q_pts,coeff,pt)
   ! Set "res(i)" to the real part of the sum over reflections "n" of
   ! coeff(n)*exp(-i q_n.r_i), where "q_n" is row n of "q_pts" and "r_i"
   ! is row i of "pt": a Fourier series, with the sign of the exponent
   ! that gives the density of a crystal from its structure factors.
   ! The loop over points is innermost so that it vectorises, sin and
   ! cos included.
      VEC(REAL,:), OUT :: res
      MAT(REAL,:,:), IN :: q_pts
      VEC(CPX,:), IN :: coeff
      MAT(REAL,:,:), IN :: pt
      REAL :: qx,qy,qz,re,im,qr,s,c
      INT :: n,i
   ENSURE(size(res)==size(pt,1),"FOURIER_SUMS:fourier_series_at ... res and pt: different number of points")
   ENSURE(size(q_pts,2)==3,"FOURIER_SUMS:fourier_series_at ... q_pts: need 3 columns")
   ENSURE(size(pt,2)==3,"FOURIER_SUMS:fourier_series_at ... pt: need 3 columns")
   ENSURE(size(coeff)==size(q_pts,1),"FOURIER_SUMS:fourier_series_at ... coeff and q_pts: different number of reflections")
      res = ZERO
      do n = 1,size(q_pts,1)
         qx = q_pts(n,1)
         qy = q_pts(n,2)
         qz = q_pts(n,3)
         re = REALIFY(coeff(n))
         im = aimag(coeff(n))
         do i = 1,size(pt,1)
            qr = qx*pt(i,1)+qy*pt(i,2)+qz*pt(i,3)
            call sin_cos(qr,s,c)
            res(i) = res(i)+re*c+im*s
         end do
      end do
   end subroutine

   PURE subroutine spherical_bessel(j,x,l_max)
   ! Set "j(l+1)" to the spherical Bessel function j_l(x) for l = 0 to
   ! "l_max". j_0 and j_1 are a sine and a cosine; above them the
   ! three-term recurrence runs upward when x > l_max, where it is stable,
   ! and otherwise downward from an order well above l_max, scaled at the
   ! end to the known j_0 (Miller's method).
      VEC(REAL,:), OUT :: j
      REAL, IN :: x
      INT, IN :: l_max
      REAL :: s,c,jp,jc,jm,scale
      INT :: l,n,n_top
   ENSURE(size(j)==l_max+1,"FOURIER_SUMS:spherical_bessel ... j: wrong dim")
   ENSURE(x>=ZERO,"FOURIER_SUMS:spherical_bessel ... x must not be negative")

      ! Near zero: j_0 = 1 and the rest vanish as x^l
      if (x<1.0d-8) then
         j = ZERO
         j(1) = ONE
         return
      end if
      s = sin(x)/x
      c = cos(x)
      j(1) = s
      if (l_max==0) return
      j(2) = (s-c)/x
      if (x>l_max) then

         ! Upward
         do l = 1,l_max-1
            j(l+2) = (2*l+1)/x*j(l+1)-j(l)
         end do
      else

         ! Downward from n_top, keeping the orders wanted, rescaling when
         ! the unnormalised values grow large
         n_top = l_max+30
         jp = ZERO
         jc = 1.0d-30
         do n = n_top,1,-1
            jm = (2*n+1)/x*jc-jp
            if (n-1<=l_max) j(n) = jm
            jp = jc
            jc = jm
            if (abs(jc)>1.0d100) then
               jp = jp*1.0d-100
               jc = jc*1.0d-100
               if (n-1<=l_max) j(n:l_max+1) = j(n:l_max+1)*1.0d-100
            end if
         end do
         scale = s/j(1)
         j = j*scale
      end if
   end subroutine

   PURE subroutine sinc_kr_sums(res,k,r,w)
   ! Set "res(j)" to the sum over points "i" of w(i)*sin(kr)/kr, with
   ! k = k(j) and r = r(i): the transform of a spherical density given
   ! at radii "r" with radial weights "w". The limit 1 is used at kr = 0.
      VEC(REAL,:), OUT :: res
      VEC(REAL,:), IN :: k
      VEC(REAL,:), IN :: r
      VEC(REAL,:), IN :: w
      REAL :: ri,wi,kr,s,c
      INT :: j,i
   ENSURE(size(res)==size(k),"FOURIER_SUMS:sinc_kr_sums ... res and k: different lengths")
   ENSURE(size(w)==size(r),"FOURIER_SUMS:sinc_kr_sums ... w and r: different lengths")
      res = ZERO
      do i = 1,size(r)
         ri = r(i)
         wi = w(i)
         do j = 1,size(k)
            kr = k(j)*ri
            call sin_cos(kr,s,c)
            res(j) = res(j)+wi*merge(ONE,s/kr,kr==ZERO)
         end do
      end do
   end subroutine

   PURE subroutine fitted_exp_ikr_sums(res,k_pts,pt,w,centre,alpha,l_max,fit)
   ! Set "res(k)" to the transform, sum over i of w(i)*exp(i k.r_i) as in
   ! exp_ikr_sums, but of a fit to the density rather than of the points.
   ! The fitting functions sit on "centre": N r^l exp(-alpha r^2) Y_lm, one
   ! for each exponent in "alpha" and each real spherical harmonic Y_lm up
   ! to "l_max". The fit minimises the overlap norm of the error, which by
   ! Parseval weights the error in res equally at every k. The metric is
   ! diagonal in (l,m), and the transform of each function is analytic:
   !   (pi/a)^(3/2) (i/2a)^l k^l Y_lm(k^) exp(-k^2/4a).
   ! If present, "fit" is set to the fitted density at the points, for checks.
   ! See docs/TASK_RI_FITTING.md.
      VEC(CPX,:), OUT :: res
      MAT(REAL,:,:), IN :: k_pts
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), IN :: w
      VEC(REAL,3), IN :: centre
      VEC(REAL,:), IN :: alpha
      INT, IN :: l_max
      VEC(REAL,:), OUT, optional :: fit
      MAT(REAL,:,:), allocatable :: r,Y,Yk,E,G,B,C,V,N,T,S,Lc,Fp,Pj
      VEC(REAL,:), allocatable :: bv,cv,re,im,sum_l,rad
      VEC(INT,:), allocatable :: ind,first
      REAL :: gam,kr,sn,cs
      INT :: n_pt,n_k,n_a,n_lm,n_s,l,l1,m,f,i,j,k,ip,ish,D
   ENSURE(size(res)==size(k_pts,1),"FOURIER_SUMS:fitted_exp_ikr_sums ... res and k_pts: different number of k")
   ENSURE(size(k_pts,2)==3,"FOURIER_SUMS:fitted_exp_ikr_sums ... k_pts: need 3 columns")
   ENSURE(size(pt,2)==3,"FOURIER_SUMS:fitted_exp_ikr_sums ... pt: need 3 columns")
   ENSURE(size(w)==size(pt,1),"FOURIER_SUMS:fitted_exp_ikr_sums ... w and pt: different number of points")
   ENSURE(l_max>=0,"FOURIER_SUMS:fitted_exp_ikr_sums ... l_max is negative")
   ENSURE(all(alpha>ZERO),"FOURIER_SUMS:fitted_exp_ikr_sums ... exponents must be positive")
      n_pt = size(pt,1)
      n_k = size(k_pts,1)
      n_a = size(alpha)
      n_lm = (l_max+1)*(l_max+1)

      ! Points relative to the centre
      call create_(r,n_pt,3)
      do i = 1,3
         r(:,i) = pt(:,i)-centre(i)
      end do

      ! Solid harmonics r^l Y_lm at the points, and k^l Y_lm at the k
      call create_(Y,n_pt,n_lm)
      call make_solid_harmonics(Y,r,l_max)
      call create_(Yk,n_k,n_lm)
      call make_solid_harmonics(Yk,k_pts,l_max)

      ! N normalises N r^l exp(-a r^2) over r^2 dr; T also holds the
      ! factor (pi/a)^(3/2) (2a)^(-l) of its transform. Column l+1.
      call create_(N,n_a,l_max+1)
      call create_(T,n_a,l_max+1)
      do l = 0,l_max
         l1 = l+1
         gam = double_factorial_(l1)*sqrt(PI)/TWO**l1  ! Gamma(l+3/2)
         N(:,l1) = sqrt(TWO*(TWO*alpha)**(l+1.5d0)/gam)
         T(:,l1) = N(:,l1)*(PI/alpha)**1.5d0/(TWO*alpha)**l
      end do

      ! Right-hand side B(n,lm) = sum_i w_i exp(-a_n r_i^2) r_i^l Y_lm,
      ! taken a shell of the grid at a time: the points of a shell share one
      ! radius R_s, so P(s,lm) = sum over the shell of w_i r_i^l Y_lm is the
      ! projection of the density onto Y_lm, and B = sum_s exp(-a R_s^2) P.
      ! A Lebedev shell of degree D integrates Y_lm exactly only for l <= D,
      ! so it is left out for higher l. Pruned grids use degree 5 near the
      ! nucleus, where the density is nearly spherical: kept in, its l = 0
      ! part leaks into l >= 6 and ruins the fit.
      call create_(rad,n_pt)
      rad = sqrt(r(:,1)**2+r(:,2)**2+r(:,3)**2)
      call create_(ind,n_pt)
      call quick_sort_(rad,ind)
      call create_(first,n_pt+1)
      n_s = 1
      first(1) = 1
      do ip = 2,n_pt
         if (rad(ind(ip))-rad(ind(first(n_s)))>1.0d-9) then
            n_s = n_s+1
            first(n_s) = ip
         end if
      end do
      first(n_s+1) = n_pt+1
      call create_(Pj,n_s,n_lm)
      call create_(E,n_s,n_a)
      do ish = 1,n_s
         Pj(ish,:) = ZERO
         do ip = first(ish),first(ish+1)-1
            i = ind(ip)
            Pj(ish,:) = Pj(ish,:)+w(i)*Y(i,:)
         end do
         D = lebedev_degree(first(ish+1)-first(ish))
         D = max(D-4,0)
         if (D<l_max) Pj(ish,(D+1)**2+1:) = ZERO
         E(ish,:) = exp(-alpha*rad(ind(first(ish)))**2)
      end do
      call create_(B,n_a,n_lm)
      call to_product_of_BLAS_(B,E,Pj,transpose_a=TRUE)
      call destroy_(E)
      call destroy_(Pj)
      call destroy_(first)
      call destroy_(ind)
      call destroy_(rad)

      ! Solve one overlap block per l, the same for every m. The overlap of
      ! two normalised functions is (2 sqrt(a b)/(a+b))^(l+3/2).
      ! C holds the coefficients times N.
      call create_(C,n_a,n_lm)
      call create_(S,n_a,n_a)
      call create_(Lc,n_a,n_a)
      call create_(bv,n_a)
      call create_(cv,n_a)
      do l = 0,l_max
         l1 = l+1
         do j = 1,n_a
            do i = 1,n_a
               S(i,j) = (TWO*sqrt(alpha(i)*alpha(j))/(alpha(i)+alpha(j)))**(l+1.5d0)
            end do
         end do
         Lc = ONE  ! Cholesky_decomposition checks its diagonal on entry
         call Cholesky_decomposition_(S,Lc)
         f = l*l
         do m = 1,2*l+1
            bv = N(:,l1)*B(:,f+m)
            call solve_cholesky_equation_(Lc,bv,cv)
            C(:,f+m) = cv*N(:,l1)
         end do
      end do

      ! The fitted density at the points: sum over n,lm of
      ! c N exp(-a r^2) r^l Y_lm
      if (present(fit)) then
         call create_(E,n_pt,n_a)
         do j = 1,n_a
            E(:,j) = exp(-alpha(j)*(r(:,1)**2+r(:,2)**2+r(:,3)**2))
         end do
         call create_(Fp,n_pt,n_lm)
         call to_product_of_BLAS_(Fp,E,C)
         fit = sum(Fp*Y,dim=2)
         call destroy_(Fp)
         call destroy_(E)
      end if
      call destroy_(Y)
      call destroy_(r)

      ! Fold the transform factor into the coefficients
      do l = 0,l_max
         f = l*l
         do m = 1,2*l+1
            C(:,f+m) = C(:,f+m)*T(:,l+1)/N(:,l+1)
         end do
      end do
      call destroy_(cv)
      call destroy_(bv)
      call destroy_(Lc)
      call destroy_(S)
      call destroy_(B)
      call destroy_(T)
      call destroy_(N)

      ! Radial factors exp(-k^2/4a), then V(k,lm) = sum_n G(k,n) C(n,lm)
      call create_(G,n_k,n_a)
      do j = 1,n_a
         G(:,j) = exp(-(k_pts(:,1)**2+k_pts(:,2)**2+k_pts(:,3)**2)/(FOUR*alpha(j)))
      end do
      call create_(V,n_k,n_lm)
      call to_product_of_BLAS_(V,G,C)
      V = V*Yk
      call destroy_(G)
      call destroy_(C)
      call destroy_(Yk)

      ! Sum over (l,m) with the factor i^l
      call create_(re,n_k); re = ZERO
      call create_(im,n_k); im = ZERO
      call create_(sum_l,n_k)
      do l = 0,l_max
         f = l*l
         sum_l = sum(V(:,f+1:f+2*l+1),dim=2)
         select case (mod(l,4))
         case (0); re = re+sum_l
         case (1); im = im+sum_l
         case (2); re = re-sum_l
         case (3); im = im-sum_l
         end select
      end do
      call destroy_(sum_l)
      call destroy_(V)

      ! Move the fit to the centre: times exp(i k.centre)
      do k = 1,n_k
         kr = k_pts(k,1)*centre(1)+k_pts(k,2)*centre(2)+k_pts(k,3)*centre(3)
         call sin_cos(kr,sn,cs)
         res(k) = COMPLEXIFY(re(k)*cs-im(k)*sn,re(k)*sn+im(k)*cs)
      end do
      call destroy_(im)
      call destroy_(re)
   end subroutine

   PURE subroutine fitted_exp_ikr_transpose(X,k_pts,coef,pt,centre,alpha,l_max)
   ! Set "X(i)" to Re sum_k coef(k) K(k,r_i), where K is the kernel of
   ! fitted_exp_ikr_sums: that routine returns res(k) = sum_i w(i) K(k,r_i)
   ! for weights "w" at the points "pt", so this is its transpose, and
   ! sum_k Re[coef(k) res(k)] = sum_i w(i) X(i) exactly. It replaces the
   ! point-by-point sum of exp(i k.r_i) when the form factors come from the
   ! fit. Cost: the k points times the fitting functions, then the points
   ! times the fitting functions; the k and the points never meet. The
   ! shells, the Lebedev degree masks and the metric are made exactly as
   ! there. See docs/REPORT_ON_XCW_CONVERGENCE.md.
      VEC(REAL,:), OUT :: X
      MAT(REAL,:,:), IN :: k_pts
      VEC(CPX,:), IN :: coef
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: centre
      VEC(REAL,:), IN :: alpha
      INT, IN :: l_max
      MAT(REAL,:,:), allocatable :: r,Y,Yk,G,Dr,Di,Zr,Zi,Z,U,N,T,S,Lc,E,Q
      VEC(REAL,:), allocatable :: dr_,di_,bv,cv,rad
      VEC(INT,:), allocatable :: ind,first,shell_of,mask_from
      REAL :: gam,kr,sn,cs
      INT :: n_pt,n_k,n_a,n_lm,n_s,l,l1,m,f,i,j,k,ip,ish,D
   ENSURE(size(coef)==size(k_pts,1),"FOURIER_SUMS:fitted_exp_ikr_transpose ... coef and k_pts: different number of k")
   ENSURE(size(k_pts,2)==3,"FOURIER_SUMS:fitted_exp_ikr_transpose ... k_pts: need 3 columns")
   ENSURE(size(pt,2)==3,"FOURIER_SUMS:fitted_exp_ikr_transpose ... pt: need 3 columns")
   ENSURE(size(X)==size(pt,1),"FOURIER_SUMS:fitted_exp_ikr_transpose ... X and pt: different number of points")
   ENSURE(l_max>=0,"FOURIER_SUMS:fitted_exp_ikr_transpose ... l_max is negative")
   ENSURE(all(alpha>ZERO),"FOURIER_SUMS:fitted_exp_ikr_transpose ... exponents must be positive")
      n_pt = size(pt,1)
      n_k = size(k_pts,1)
      n_a = size(alpha)
      n_lm = (l_max+1)*(l_max+1)

      ! Normalisation N and transform factor T, as in fitted_exp_ikr_sums
      call create_(N,n_a,l_max+1)
      call create_(T,n_a,l_max+1)
      do l = 0,l_max
         l1 = l+1
         gam = double_factorial_(l1)*sqrt(PI)/TWO**l1
         N(:,l1) = sqrt(TWO*(TWO*alpha)**(l+1.5d0)/gam)
         T(:,l1) = N(:,l1)*(PI/alpha)**1.5d0/(TWO*alpha)**l
      end do

      ! d(k) = coef(k) exp(i k.centre), split into real and imaginary parts
      call create_(dr_,n_k)
      call create_(di_,n_k)
      do k = 1,n_k
         kr = k_pts(k,1)*centre(1)+k_pts(k,2)*centre(2)+k_pts(k,3)*centre(3)
         call sin_cos(kr,sn,cs)
         dr_(k) = real(coef(k))*cs-aimag(coef(k))*sn
         di_(k) = real(coef(k))*sn+aimag(coef(k))*cs
      end do

      ! Z(n,lm) = Re[ i^l sum_k d(k) k^l Y_lm(k) exp(-k^2/4a_n) ]
      call create_(Yk,n_k,n_lm)
      call make_solid_harmonics(Yk,k_pts,l_max)
      call create_(G,n_k,n_a)
      do j = 1,n_a
         G(:,j) = exp(-(k_pts(:,1)**2+k_pts(:,2)**2+k_pts(:,3)**2)/(FOUR*alpha(j)))
      end do
      call create_(Dr,n_k,n_lm)
      call create_(Di,n_k,n_lm)
      do j = 1,n_lm
         Dr(:,j) = dr_*Yk(:,j)
         Di(:,j) = di_*Yk(:,j)
      end do
      call create_(Zr,n_a,n_lm)
      call create_(Zi,n_a,n_lm)
      call to_product_of_BLAS_(Zr,G,Dr,transpose_a=TRUE)
      call to_product_of_BLAS_(Zi,G,Di,transpose_a=TRUE)
      call create_(Z,n_a,n_lm)
      do l = 0,l_max
         f = l*l
         do m = 1,2*l+1
            select case (mod(l,4))
            case (0); Z(:,f+m) = Zr(:,f+m)
            case (1); Z(:,f+m) = -Zi(:,f+m)
            case (2); Z(:,f+m) = -Zr(:,f+m)
            case (3); Z(:,f+m) = Zi(:,f+m)
            end select
         end do
      end do
      call destroy_(Zi)
      call destroy_(Zr)
      call destroy_(Di)
      call destroy_(Dr)
      call destroy_(G)
      call destroy_(Yk)
      call destroy_(di_)
      call destroy_(dr_)

      ! U(:,lm) = N o S_l^-1 (T o Z(:,lm)), the same metric blocks
      call create_(U,n_a,n_lm)
      call create_(S,n_a,n_a)
      call create_(Lc,n_a,n_a)
      call create_(bv,n_a)
      call create_(cv,n_a)
      do l = 0,l_max
         l1 = l+1
         do j = 1,n_a
            do i = 1,n_a
               S(i,j) = (TWO*sqrt(alpha(i)*alpha(j))/(alpha(i)+alpha(j)))**(l+1.5d0)
            end do
         end do
         Lc = ONE
         call Cholesky_decomposition_(S,Lc)
         f = l*l
         do m = 1,2*l+1
            bv = T(:,l1)*Z(:,f+m)
            call solve_cholesky_equation_(Lc,bv,cv)
            U(:,f+m) = N(:,l1)*cv
         end do
      end do
      call destroy_(cv)
      call destroy_(bv)
      call destroy_(Lc)
      call destroy_(S)
      call destroy_(Z)
      call destroy_(T)
      call destroy_(N)

      ! The points: relative to the centre, solid harmonics, and the
      ! shells by radius with their Lebedev degree masks
      call create_(r,n_pt,3)
      do i = 1,3
         r(:,i) = pt(:,i)-centre(i)
      end do
      call create_(Y,n_pt,n_lm)
      call make_solid_harmonics(Y,r,l_max)
      call create_(rad,n_pt)
      rad = sqrt(r(:,1)**2+r(:,2)**2+r(:,3)**2)
      call destroy_(r)
      call create_(ind,n_pt)
      call quick_sort_(rad,ind)
      call create_(first,n_pt+1)
      n_s = 1
      first(1) = 1
      do ip = 2,n_pt
         if (rad(ind(ip))-rad(ind(first(n_s)))>1.0d-9) then
            n_s = n_s+1
            first(n_s) = ip
         end if
      end do
      first(n_s+1) = n_pt+1

      ! Q(s,lm) = sum_n exp(-a_n R_s^2) U(n,lm), and the mask per shell
      call create_(E,n_s,n_a)
      call create_(mask_from,n_s)
      do ish = 1,n_s
         E(ish,:) = exp(-alpha*rad(ind(first(ish)))**2)
         D = lebedev_degree(first(ish+1)-first(ish))
         D = max(D-4,0)
         mask_from(ish) = n_lm+1
         if (D<l_max) mask_from(ish) = (D+1)**2+1
      end do
      call create_(Q,n_s,n_lm)
      call to_product_of_BLAS_(Q,E,U)
      call destroy_(E)
      call destroy_(U)

      ! X(i) = sum over unmasked lm of Y(i,lm) Q(s_i,lm)
      X = ZERO
      do ish = 1,n_s
         f = mask_from(ish)-1
         do ip = first(ish),first(ish+1)-1
            i = ind(ip)
            X(i) = sum(Y(i,1:f)*Q(ish,1:f))
         end do
      end do
      call destroy_(Q)
      call destroy_(mask_from)
      call destroy_(first)
      call destroy_(ind)
      call destroy_(rad)
      call destroy_(Y)
   end subroutine

   pure function lebedev_degree(n) result (res)
   ! Return the degree of the largest Lebedev grid with at most "n" points:
   ! the highest l of the spherical harmonics it integrates exactly. A shell
   ! with some points removed is taken to be the grid below, to be safe.
      INT, IN :: n
      INT :: res
      VEC(INT,32) :: sizes,degree
      INT :: i
      sizes = [6,14,26,38,50,74,86,110,146,170,194,230,266,302,350,434,590,770,974,1202,1454,1730,2030,2354,2702,3074,3470,3890,4334,4802,5294,5810]
      degree = [3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,35,41,47,53,59,65,71,77,83,89,95,101,107,113,119,125,131]
      res = 0
      do i = 1,32
         if (sizes(i)>n) exit
         res = degree(i)
      end do
   end function

   PURE subroutine make_solid_harmonics(Y,r,l_max)
   ! Set columns l*l+1 to (l+1)^2 of "Y" to the solid harmonics
   ! |r|^l Y_lm(r^) at the points "r", for l = 0 to "l_max". Y_lm are real
   ! spherical harmonics normalised to 1 over the unit sphere, with m in
   ! the order of GAUSSIAN_DATA:spherical_harmonics_for.
      MAT(REAL,:,:), OUT :: Y
      MAT(REAL,:,:), IN :: r
      INT, IN :: l_max
      MAT(REAL,:,:), allocatable :: U,M
      VEC(INT,3) :: p
      INT :: l,c,f,nc
   ENSURE(size(r,2)==3,"FOURIER_SUMS:make_solid_harmonics ... r: need 3 columns")
   ENSURE(size(Y,1)==size(r,1),"FOURIER_SUMS:make_solid_harmonics ... Y and r: different number of points")
   ENSURE(size(Y,2)==(l_max+1)*(l_max+1),"FOURIER_SUMS:make_solid_harmonics ... Y: wrong number of columns")
   ENSURE(l_max<=l_max_saved,"FOURIER_SUMS:make_solid_harmonics ... l_max too large: raise it with GAUSSIAN_DATA:set_indices")
      do l = 0,l_max
         nc = n_comp(l)
         f = n_comp_up_to(l-1)
         call create_(M,size(r,1),nc)
         do c = 1,nc
            p = pp(:,f+c)
            M(:,c) = r(:,1)**p(1)*r(:,2)**p(2)*r(:,3)**p(3)
         end do
         call create_(U,nc,2*l+1)
         call make_normalised_harmonics(U,l)
         Y(:,l*l+1:(l+1)*(l+1)) = matmul(M,U)
         call destroy_(U)
         call destroy_(M)
      end do
   end subroutine

   pure function harmonic_column(l,m) result (res)
   ! The column of make_solid_harmonics that holds Y_lm, for the usual
   ! real-harmonic index m = -l to l: the columns are in Molden order
   ! 0, +1, -1, +2, -2, ... within each l, with m > 0 cosine-like and
   ! m < 0 sine-like, so that for l = 1, m = -1, 0, 1 are y, z, x.
      INT, IN :: l,m
      INT :: res
      if (m>0) then; res = l*l+2*m
      else if (m<0) then; res = l*l+2*abs(m)+1
      else; res = l*l+1
      end if
   end function

   PURE subroutine make_normalised_harmonics(U,l)
   ! Set "U(c,m)" to the coefficients of the monomials x^i y^j z^k of
   ! degree "l", in GAUSSIAN_DATA's order, in the real spherical harmonic
   ! Y_lm: GAUSSIAN_DATA:spherical_harmonics_for(l), rescaled so that each
   ! Y_lm has integral 1 over the unit sphere. The normalisation is done
   ! here, from exact integrals, so that it does not depend on the
   ! convention of those coefficients.
      MAT(REAL,:,:), OUT :: U
      INT, IN :: l
      MAT(REAL,:,:), allocatable :: A,G
      REAL :: off,gl
      INT :: f,nc,c,d,m,n
   ENSURE(size(U,1)==n_comp(l),"FOURIER_SUMS:make_normalised_harmonics ... U: wrong number of rows")
   ENSURE(size(U,2)==2*l+1,"FOURIER_SUMS:make_normalised_harmonics ... U: wrong number of columns")
      nc = n_comp(l)
      f = n_comp_up_to(l-1)
      U = spherical_harmonics_for(l)%element

      ! A(c,d) = integral of monomials c and d over the unit sphere, and
      ! G(m,n) = the same for Y_lm and Y_ln
      call create_(A,nc,nc)
      do d = 1,nc
         do c = 1,nc
            A(c,d) = sphere_integral(pp(:,f+c)+pp(:,f+d))
         end do
      end do
      call create_(G,2*l+1,2*l+1)
      G = matmul(transpose(U),matmul(A,U))
      call destroy_(A)
      do m = 1,2*l+1
         U(:,m) = U(:,m)/sqrt(G(m,m))
      end do

      ! Different m must be orthogonal
      off = ZERO
      do n = 1,2*l+1
         do m = 1,2*l+1
            if (m/=n) off = max(off,abs(G(m,n))/sqrt(G(m,m)*G(n,n)))
         end do
      end do
      ENSURE(off<1.0d-10,"FOURIER_SUMS:make_normalised_harmonics ... harmonics are not orthogonal")

      ! A harmonic is orthogonal on the sphere to every polynomial of
      ! degree l-2, which is what makes the transform formula hold
      off = ZERO
      if (l>=2) then
         n = n_comp_up_to(l-3)
         do d = 1,n_comp(l-2)
            do m = 1,2*l+1
               gl = ZERO
               do c = 1,nc
                  gl = gl+U(c,m)*sphere_integral(pp(:,f+c)+pp(:,n+d))
               end do
               off = max(off,abs(gl))
            end do
         end do
      end if
      ENSURE(off<1.0d-10,"FOURIER_SUMS:make_normalised_harmonics ... harmonics are not harmonic")
      call destroy_(G)
   end subroutine

   pure function sphere_integral(p) result (res)
   ! Return the integral of x^p(1) y^p(2) z^p(3) over the unit sphere:
   ! 4 pi (p1-1)!! (p2-1)!! (p3-1)!! / (p1+p2+p3+1)!! if every p is even,
   ! otherwise zero. INT:double_factorial of n is (2n-1)!!.
      VEC(INT,3), IN :: p
      REAL :: res
      VEC(INT,3) :: h
      INT :: t
      if (any(mod(p,2)/=0)) then
         res = ZERO
      else
         h = p/2
         t = sum(h)+1
         res = FOUR*PI*double_factorial_(h(1))*double_factorial_(h(2))*double_factorial_(h(3))/double_factorial_(t)
      end if
   end function

   ELEMENTAL subroutine sin_cos(x,s,c)
   ! Set "s" and "c" to sin(x) and cos(x), within 1 unit in the last place
   ! for |x| <= 200, using real arithmetic only so that a loop calling it
   ! vectorises on any processor. x is reduced to r in [-pi/4,pi/4] by a
   ! three-part pi/2, and the quadrant chosen by real comparisons.
   ! Needs this file's compiler flags: see the top of the file.
      REAL, IN :: x
      REAL, OUT :: s,c
      REAL :: n,q,r,z,sr,cr
      BIN :: even
      n = (x*sc_2_on_pi+sc_big)-sc_big  ! nearest integer
      q = n-FOUR*((0.25d0*n-0.375d0+sc_big)-sc_big)  ! n mod 4
      r = ((x-n*sc_p1)-n*sc_p2)-n*sc_p3
      z = r*r
      sr = r+r*z*(sc_s1+z*(sc_s2+z*(sc_s3+z*(sc_s4+z*(sc_s5+z*sc_s6)))))
      cr = ONE-HALF*z+z*z*(sc_c1+z*(sc_c2+z*(sc_c3+z*(sc_c4+z*(sc_c5+z*sc_c6)))))
      even = q==ZERO OR q==TWO
      s = merge(sr,cr,even)
      c = merge(cr,sr,even)
      s = merge(-s,s,q>=TWO)
      c = merge(-c,c,q==ONE OR q==TWO)
   end subroutine

end module
