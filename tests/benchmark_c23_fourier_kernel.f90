program benchmark_c23_fourier_kernel
   implicit none
   integer, parameter :: rk = selected_real_kind(15, 300)
   integer, parameter :: n_pt = 4000, n_k = 512, repeats = 3
   real(rk), allocatable :: pt(:,:), rho(:), k_pts(:,:)
   complex(rk), allocatable :: old_ff(:), new_ff(:)
   real(rk) :: kr, sf_real, sf_imag
   real(rk) :: old_seconds, new_seconds, started, max_difference
   complex(rk) :: sf
   integer :: i, k, repeat

   allocate(pt(n_pt,3), rho(n_pt), k_pts(n_k,3))
   allocate(old_ff(n_k), new_ff(n_k))
   call random_seed
   call random_number(pt)
   call random_number(rho)
   call random_number(k_pts)
   pt = 20.0_rk * (pt - 0.5_rk)
   k_pts = 8.0_rk * (k_pts - 0.5_rk)

   call cpu_time(started)
   do repeat = 1, repeats
      do k = 1, n_k
         sf = cmplx(0.0_rk, 0.0_rk, rk)
         do i = 1, n_pt
            kr = dot_product(k_pts(k,:), pt(i,:))
            sf = sf + rho(i) * exp(cmplx(0.0_rk, kr, rk))
         end do
         old_ff(k) = sf
      end do
   end do
   call cpu_time(old_seconds)
   old_seconds = old_seconds - started

   call cpu_time(started)
   do repeat = 1, repeats
      do k = 1, n_k
         sf_real = 0.0_rk
         sf_imag = 0.0_rk
         do i = 1, n_pt
            kr = dot_product(k_pts(k,:), pt(i,:))
            sf_real = sf_real + rho(i) * cos(kr)
            sf_imag = sf_imag + rho(i) * sin(kr)
         end do
         new_ff(k) = cmplx(sf_real, sf_imag, rk)
      end do
   end do
   call cpu_time(new_seconds)
   new_seconds = new_seconds - started

   max_difference = maxval(abs(old_ff - new_ff))
   write(*,'(a,es12.4)') "maximum absolute difference: ", max_difference
   write(*,'(a,f10.4,a)') "complex-exp kernel: ", old_seconds, " s"
   write(*,'(a,f10.4,a)') "real sin/cos kernel: ", new_seconds, " s"
   write(*,'(a,f8.3,a)') "speed ratio (old/new): ", &
      old_seconds / max(new_seconds, tiny(new_seconds)), "x"

   if (max_difference > 1.0e-10_rk) error stop "kernel mismatch"
end program benchmark_c23_fourier_kernel
