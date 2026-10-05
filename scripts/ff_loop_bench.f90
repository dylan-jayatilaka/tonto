! Timing test for the aspherical form factor loop of Tonto's HAR,
! MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk, outside Tonto.
!
! For one atom:  f(k) = sum_i rho(i) exp(i k.r_i),  k = 1..n_k,  i = 1..n_pt.
! Four ways of computing the same thing:
!   A  as Tonto does it now: k outside, points inside, complex exp
!   B  the same loops with real cos and sin sums
!   C  tiled over k, as the plan proposes: a matrix multiply (DGEMM) gives a
!      tile of k.r, then cos and sin of the tile, then two matrix-vector
!      products (DGEMV) with rho
!   D  the loops swapped: points outside, k inside, with a running cos sum
!      and sin sum for every k -- the inner loop over k is the shape a
!      compiler can vectorise, cos and sin included
!   E  as D, with sin and cos computed together in Fortran (sincos_poly):
!      reduce x to r in [-pi/4,pi/4] by a three-part pi/2, evaluate the
!      Cephes polynomials for sin r and cos r, and choose by quadrant with
!      merge, so the loop body is plain arithmetic the compiler can vectorise
!   F  as E, with no integer arithmetic (sincos_real): rounding by adding and
!      subtracting 1.5*2^52, the quadrant as a real number -- for processors
!      whose vector instructions cannot round or convert doubles (generic
!      x86-64). Needs -fno-associative-math, or the rounding is optimised away.
! Each is timed, and its largest relative difference from A is printed.
!
! Usage:  ff_loop_bench [n_k n_pt tile]      defaults 8000 6000 256

program ff_loop_bench
   use, intrinsic :: iso_fortran_env, only: dp => real64, i8 => int64
   implicit none

   integer :: n_k, n_pt, tile, i, j, k0, m
   real(dp), allocatable :: kv(:,:), pt(:,:), rho(:)
   real(dp), allocatable :: re(:), im(:), kr(:,:), c(:,:), s(:,:)
   complex(dp), allocatable :: fA(:), fB(:), fC(:), fD(:), fE(:), fF(:)
   complex(dp) :: f
   real(dp) :: x, cs, sn, px, py, pz, r, t(6)
   integer(i8) :: t0, t1, rate
   character(len=32) :: arg

   n_k = 8000; n_pt = 6000; tile = 256
   if (command_argument_count() >= 1) then; call get_command_argument(1, arg); read(arg,*) n_k;  end if
   if (command_argument_count() >= 2) then; call get_command_argument(2, arg); read(arg,*) n_pt; end if
   if (command_argument_count() >= 3) then; call get_command_argument(3, arg); read(arg,*) tile; end if

   ! k up to about 10 bohr^-1; points within 6 bohr; a positive density
   allocate(kv(n_k,3), pt(n_pt,3), rho(n_pt))
   call random_seed_fixed()
   call random_number(kv);  kv  = 20.0_dp*(kv - 0.5_dp)
   call random_number(pt);  pt  = 12.0_dp*(pt - 0.5_dp)
   call random_number(rho); rho = 1.0e-3_dp*rho

   allocate(fA(n_k), fB(n_k), fC(n_k), fD(n_k), fE(n_k), fF(n_k), re(n_k), im(n_k))
   call system_clock(count_rate=rate)

   ! A: as now
   call system_clock(t0)
   do j = 1, n_k
      f = (0.0_dp, 0.0_dp)
      do i = 1, n_pt
         x = kv(j,1)*pt(i,1) + kv(j,2)*pt(i,2) + kv(j,3)*pt(i,3)
         f = f + rho(i)*exp(cmplx(0.0_dp, x, dp))
      end do
      fA(j) = f
   end do
   call system_clock(t1); t(1) = real(t1-t0,dp)/rate

   ! B: real cos and sin sums
   call system_clock(t0)
   do j = 1, n_k
      cs = 0.0_dp; sn = 0.0_dp
      do i = 1, n_pt
         x = kv(j,1)*pt(i,1) + kv(j,2)*pt(i,2) + kv(j,3)*pt(i,3)
         cs = cs + rho(i)*cos(x)
         sn = sn + rho(i)*sin(x)
      end do
      fB(j) = cmplx(cs, sn, dp)
   end do
   call system_clock(t1); t(2) = real(t1-t0,dp)/rate

   ! C: tiles of k, DGEMM for k.r, DGEMV for the sums
   allocate(kr(tile,n_pt), c(tile,n_pt), s(tile,n_pt))
   call system_clock(t0)
   do k0 = 1, n_k, tile
      m = min(tile, n_k - k0 + 1)
      call dgemm('N', 'T', m, n_pt, 3, 1.0_dp, kv(k0,1), n_k, pt, n_pt, 0.0_dp, kr, tile)
      c(1:m,:) = cos(kr(1:m,:))
      s(1:m,:) = sin(kr(1:m,:))
      call dgemv('N', m, n_pt, 1.0_dp, c, tile, rho, 1, 0.0_dp, re(k0), 1)
      call dgemv('N', m, n_pt, 1.0_dp, s, tile, rho, 1, 0.0_dp, im(k0), 1)
   end do
   fC = cmplx(re, im, dp)
   call system_clock(t1); t(3) = real(t1-t0,dp)/rate
   deallocate(kr, c, s)

   ! D: loops swapped, a running sum per k
   call system_clock(t0)
   re = 0.0_dp; im = 0.0_dp
   do i = 1, n_pt
      px = pt(i,1); py = pt(i,2); pz = pt(i,3); r = rho(i)
      do j = 1, n_k
         x = kv(j,1)*px + kv(j,2)*py + kv(j,3)*pz
         re(j) = re(j) + r*cos(x)
         im(j) = im(j) + r*sin(x)
      end do
   end do
   fD = cmplx(re, im, dp)
   call system_clock(t1); t(4) = real(t1-t0,dp)/rate

   ! E: loops swapped, sin and cos in Fortran
   call system_clock(t0)
   re = 0.0_dp; im = 0.0_dp
   do i = 1, n_pt
      px = pt(i,1); py = pt(i,2); pz = pt(i,3); r = rho(i)
      do j = 1, n_k
         x = kv(j,1)*px + kv(j,2)*py + kv(j,3)*pz
         call sincos_poly(x, sn, cs)
         re(j) = re(j) + r*cs
         im(j) = im(j) + r*sn
      end do
   end do
   fE = cmplx(re, im, dp)
   call system_clock(t1); t(5) = real(t1-t0,dp)/rate

   ! F: as E, no integer arithmetic
   call system_clock(t0)
   re = 0.0_dp; im = 0.0_dp
   do i = 1, n_pt
      px = pt(i,1); py = pt(i,2); pz = pt(i,3); r = rho(i)
      do j = 1, n_k
         x = kv(j,1)*px + kv(j,2)*py + kv(j,3)*pz
         call sincos_real(x, sn, cs)
         re(j) = re(j) + r*cs
         im(j) = im(j) + r*sn
      end do
   end do
   fF = cmplx(re, im, dp)
   call system_clock(t1); t(6) = real(t1-t0,dp)/rate

   write(*,'(a,i0,a,i0,a,i0,a,es9.2,a)') 'n_k = ', n_k, ', n_pt = ', n_pt, ', tile = ', tile, &
      ', ', real(n_k,dp)*n_pt, ' terms'
   write(*,'(a)') '                                         seconds   ns/term   largest rel. diff from A'
   call report('A  as now (k outside, complex exp)     ', t(1), fA)
   call report('B  cos and sin sums                     ', t(2), fB)
   call report('C  tiles: DGEMM, cos/sin, DGEMV         ', t(3), fC)
   call report('D  loops swapped (points outside)       ', t(4), fD)
   call report('E  as D, sin and cos in Fortran          ', t(5), fE)
   call report('F  as E, no integer arithmetic          ', t(6), fF)
   call check_sincos()

contains

   subroutine report(label, secs, f)
      character(len=*), intent(in) :: label
      real(dp), intent(in) :: secs
      complex(dp), intent(in) :: f(:)
      write(*,'(a,f10.3,f10.3,es18.2)') label, secs, 1.0e9_dp*secs/(real(n_k,dp)*n_pt), &
         maxval(abs(f - fA))/maxval(abs(fA))
   end subroutine

   pure elemental subroutine sincos_poly(x, s, c)
   ! sin(x) and cos(x) together, by Cody-Waite reduction to r = x - n pi/2
   ! and the Cephes minimax polynomials on [-pi/4,pi/4]. Accurate to about
   ! one unit in the last place for |x| up to about 1e5.
      real(dp), intent(in)  :: x
      real(dp), intent(out) :: s, c
      real(dp), parameter :: two_over_pi = 0.63661977236758134308_dp
      real(dp), parameter :: p1 = 1.57079625129699707031_dp        ! pi/2 in three parts
      real(dp), parameter :: p2 = 7.54978941586159635335e-8_dp
      real(dp), parameter :: p3 = 5.39030285815811905290e-15_dp
      real(dp), parameter :: s1 = -1.66666666666666307295e-1_dp, s2 = 8.33333333332211858878e-3_dp, &
                             s3 = -1.98412698295895385996e-4_dp,  s4 = 2.75573136213857245213e-6_dp, &
                             s5 = -2.50507477628578072866e-8_dp,  s6 = 1.58962301576546568060e-10_dp
      real(dp), parameter :: c1 = 4.16666666666665929218e-2_dp,  c2 = -1.38888888888730564116e-3_dp, &
                             c3 = 2.48015872888517045348e-5_dp,  c4 = -2.75573141792967388112e-7_dp, &
                             c5 = 2.08757008419747316778e-9_dp,  c6 = -1.13585365213876817300e-11_dp
      real(dp) :: n, r, z, sr, cr
      integer :: q
      n  = anint(x*two_over_pi)
      r  = ((x - n*p1) - n*p2) - n*p3
      z  = r*r
      sr = r + r*z*(s1 + z*(s2 + z*(s3 + z*(s4 + z*(s5 + z*s6)))))
      cr = 1.0_dp - 0.5_dp*z + z*z*(c1 + z*(c2 + z*(c3 + z*(c4 + z*(c5 + z*c6)))))
      q  = iand(int(n), 3)
      s  = merge(sr, cr, iand(q,1) == 0)
      c  = merge(cr, sr, iand(q,1) == 0)
      s  = merge(-s, s, q >= 2)
      c  = merge(-c, c, q == 1 .or. q == 2)
   end subroutine

   pure elemental subroutine sincos_real(x, s, c)
   ! As sincos_poly, with every step in real arithmetic: n and the quadrant
   ! are rounded by adding and subtracting 1.5*2^52, and the quadrant is
   ! chosen by real comparisons, so a processor that cannot round or convert
   ! doubles in its vector instructions can still vectorise it.
      real(dp), intent(in)  :: x
      real(dp), intent(out) :: s, c
      real(dp), parameter :: two_over_pi = 0.63661977236758134308_dp
      real(dp), parameter :: big = 6755399441055744.0_dp          ! 1.5*2^52
      real(dp), parameter :: p1 = 1.57079625129699707031_dp
      real(dp), parameter :: p2 = 7.54978941586159635335e-8_dp
      real(dp), parameter :: p3 = 5.39030285815811905290e-15_dp
      real(dp), parameter :: s1 = -1.66666666666666307295e-1_dp, s2 = 8.33333333332211858878e-3_dp, &
                             s3 = -1.98412698295895385996e-4_dp,  s4 = 2.75573136213857245213e-6_dp, &
                             s5 = -2.50507477628578072866e-8_dp,  s6 = 1.58962301576546568060e-10_dp
      real(dp), parameter :: c1 = 4.16666666666665929218e-2_dp,  c2 = -1.38888888888730564116e-3_dp, &
                             c3 = 2.48015872888517045348e-5_dp,  c4 = -2.75573141792967388112e-7_dp, &
                             c5 = 2.08757008419747316778e-9_dp,  c6 = -1.13585365213876817300e-11_dp
      real(dp) :: n, q, r, z, sr, cr
      n  = (x*two_over_pi + big) - big                       ! nearest integer
      q  = n - 4.0_dp*((0.25_dp*n - 0.375_dp + big) - big)   ! n mod 4, in 0..3
      r  = ((x - n*p1) - n*p2) - n*p3
      z  = r*r
      sr = r + r*z*(s1 + z*(s2 + z*(s3 + z*(s4 + z*(s5 + z*s6)))))
      cr = 1.0_dp - 0.5_dp*z + z*z*(c1 + z*(c2 + z*(c3 + z*(c4 + z*(c5 + z*c6)))))
      s  = merge(sr, cr, q == 0.0_dp .or. q == 2.0_dp)
      c  = merge(cr, sr, q == 0.0_dp .or. q == 2.0_dp)
      s  = merge(-s, s, q >= 2.0_dp)
      c  = merge(-c, c, q == 1.0_dp .or. q == 2.0_dp)
   end subroutine

   subroutine check_sincos()
   ! The largest error of sincos_poly against the intrinsics, in units of
   ! epsilon, over a million arguments in [-200,200].
      integer :: ii
      real(dp) :: xx, ss, cc, es, ec
      es = 0.0_dp; ec = 0.0_dp
      do ii = 1, 1000000
         xx = -200.0_dp + 400.0_dp*(ii - 0.5_dp)/1000000
         call sincos_poly(xx, ss, cc)
         es = max(es, abs(ss - sin(xx)))
         ec = max(ec, abs(cc - cos(xx)))
         call sincos_real(xx, ss, cc)
         es = max(es, abs(ss - sin(xx)))
         ec = max(ec, abs(cc - cos(xx)))
      end do
      write(*,'(a,f6.2,a,f6.2,a)') 'E and F sin/cos against the intrinsics, |x| <= 200: sin within', &
         es/epsilon(1.0_dp), ', cos within', ec/epsilon(1.0_dp), ' epsilon'
   end subroutine

   subroutine random_seed_fixed()
      integer :: n
      integer, allocatable :: seed(:)
      call random_seed(size=n)
      allocate(seed(n)); seed = 12345
      call random_seed(put=seed)
   end subroutine

end program
