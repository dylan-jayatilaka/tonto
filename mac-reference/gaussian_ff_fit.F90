!---------------------------------------------------------------------------
!
! GAUSSIAN_FF_FIT: fit a spherical form factor to a few Gaussians.
!
! Plain procedures belonging to no object. They fit a form factor f(s),
! given on points s = sin(theta)/lambda in 1/Angstrom, to the form every
! crystallographic program reads, the International Tables one,
!
!    f(s) = sum_{i=1..n} a_i exp(-b_i s^2) + c,
!
! and write the result in the format of a refinement program.
!
! The fit is done in square roots of the coefficients, so a_i, b_i and c
! come back positive, as the International Tables' are.
!
! The minimiser is gnuplot's "fit" (Levenberg-Marquardt), run through the
! SYSTEM_COMMAND part of "tonto": the data and a script go to disk, gnuplot
! runs, and the coefficients come back from a file the script prints. The
! same script draws f(s) - fit(s). gnuplot absent is not fatal: "ok" comes
! back FALSE, the start values are returned, and the files are left so the
! fit can be run by hand.
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
!
!---------------------------------------------------------------------------
module GAUSSIAN_FF_FIT_MODULE

#  include "macros"

   include "gaussian_ff_fit.use"

   implicit none

   include "gaussian_ff_fit.int"


contains

!  ==========
!  Evaluation
!  ==========

   pure function gaussian_sum(s,a,b,c) result (res)
   ! The fitted form, sum_i a(i) exp(-b(i) s^2) + c, at every point of "s".
      VEC(REAL,:), IN :: s
      VEC(REAL,:), IN :: a,b
      REAL, IN :: c
      VEC(REAL,size(s)) :: res
      INT :: i
      res = c
      do i = 1,size(a)
         res = res+a(i)*exp(-b(i)*s*s)
      end do
   end function

   pure subroutine deviations(s,f,a,b,c,s_max,rms,max_dev)
   ! The rms and the largest absolute deviation of the fit from "f" over
   ! the points of "s" within [0,s_max].
      VEC(REAL,:), IN :: s,f
      VEC(REAL,:), IN :: a,b
      REAL, IN :: c
      REAL, IN :: s_max
      REAL, OUT :: rms,max_dev
      VEC(REAL,size(s)) :: g
      REAL :: d
      INT :: i,n
      g = gaussian_sum_(s,a,b,c)
      rms = ZERO; max_dev = ZERO; n = 0
      do i = 1,size(s)
         if (s(i)>s_max) cycle
         d = abs(f(i)-g(i))
         rms = rms+d*d
         max_dev = max(max_dev,d)
         n = n+1
      end do
      if (n>0) rms = sqrt(rms/n)
   end subroutine


!  =======
!  The fit
!  =======

   subroutine fit(s,f,a,b,c,s_max,stem,ok,a_err,b_err,c_err)
   ! Fit f(s), given at the points "s" (1/Angstrom), on [0,s_max] to
   ! sum_i a(i) exp(-b(i) s^2) + c. On entry a, b and c are the starting
   ! values; on exit they are the fit, with their esds in a_err, b_err and
   ! c_err if wanted. Files written, all headed by "stem":
   !    <stem>.dat      s and f(s)
   !    <stem>.gnuplot  the script: the fit, then the difference plot
   !    <stem>.fit.log  gnuplot's own record of the fit
   !    <stem>.coeffs   the coefficients and esds, read back here
   !    <stem>.png      f(s) - fit(s)
   ! "ok" is FALSE if gnuplot could not be run or every fit failed; a, b
   ! and c are then the starting values still. A fit that fails (gnuplot
   ! stops on a singular matrix when two Gaussians fall together) is
   ! retried from the same a and c with b scaled by 0.7, 1.4, 0.5 and 2.
   ! MPI: the master rank writes and runs; every rank reads the result.
      VEC(REAL,:), IN :: s,f
      VEC(REAL,:), INOUT :: a,b
      REAL, INOUT :: c
      REAL, IN :: s_max
      STR(len=*), IN :: stem
      BIN, OUT :: ok
      VEC(REAL,:), optional, OUT :: a_err,b_err
      REAL, optional, OUT :: c_err
      type(TEXTFILE_TYPE), allocatable :: cf
      VEC(REAL,:), allocatable :: ae,be,b0
      STR(len=STR_SIZE) :: data,script,coeffs
      VEC(REAL,5) :: scale
      REAL :: ce,wssr
      INT :: ndf,i,try
   ENSURE(size(s)==size(f),"GAUSSIAN_FF_FIT:fit ... s and f differ in length")
   ENSURE(size(a)==size(b),"GAUSSIAN_FF_FIT:fit ... a and b differ in length")
   ENSURE(size(a)>0,"GAUSSIAN_FF_FIT:fit ... no Gaussians to fit")
   ENSURE(s_max>ZERO,"GAUSSIAN_FF_FIT:fit ... s_max must be positive")
      data = trim(stem)//".dat"
      script = trim(stem)//".gnuplot"
      coeffs = trim(stem)//".coeffs"
      ok = FALSE

      ! Every process makes the same output calls, which write on the master
      ! alone. Only the master runs gnuplot, and it tells the others the result.
      call put_data_file_(data,s,f,s_max)
      b0 = b
      scale = [ONE,0.7d0,1.4d0,HALF,TWO]
      do try = 1,size(scale)
         b = scale(try)*b0
         call put_script_(script,stem,a,b,c,s_max)
         if (is_master_processor_(tonto)) then
            call execute_(tonto,"rm -f '"//trim(coeffs)//"'")
            call execute_(tonto,"gnuplot '"//trim(script)//"' > /dev/null 2>&1")
            ok = NOT command_failed_(tonto)
         end if
         PARALLEL_BROADCAST(ok,master_processor_(tonto))
         if (ok) ok = exists_(stdout,coeffs)
         if (ok) exit
      end do
      if (NOT ok) then
         b = b0
         call flush_(stdout)
         call text_(stdout,"WARNING: the form factor fit "//trim(stem)//" failed from every start.")
         call text_(stdout,"The data and the last script were written; see")
         call text_(stdout,"   gnuplot '"//trim(script)//"'")
         call flush_(stdout)
      end if
      call destroy_(b0)
      if (NOT ok) return

      ! Read back: a_i a_i_err b_i b_i_err ... c c_err WSSR NDF
      call create_(ae,size(a))
      call create_(be,size(b))
      call create_(cf,coeffs)
      call open_for_(cf,"read")
      do i = 1,size(a)
         call read_(cf,a(i)); call read_(cf,ae(i))
         call read_(cf,b(i)); call read_(cf,be(i))
      end do
      call read_(cf,c); call read_(cf,ce)
      call read_(cf,wssr); call read_(cf,ndf)
      call close_(cf)
      call destroy_(cf)
      if (present(a_err)) a_err = ae
      if (present(b_err)) b_err = be
      if (present(c_err)) c_err = ce
      call destroy_(be)
      call destroy_(ae)
   end subroutine

   subroutine put_data_file(data,s,f,s_max)
   ! Write "data": two columns, s and f(s), for s within [0,s_max].
      STR(len=*), IN :: data
      VEC(REAL,:), IN :: s,f
      REAL, IN :: s_max
      INT :: i
      call redirect_(stdout,data)
      call text_(stdout,"# Spherical atom form factor to be fitted")
      call text_(stdout,"# 1st column: sin(theta)/lambda, 1/Angstrom")
      call text_(stdout,"# 2nd column: f(s), electrons")
      do i = 1,size(s)
         if (s(i)>s_max) cycle
         call text_(stdout,trim(to_str_(s(i),"es22.14"))//" "//trim(to_str_(f(i),"es22.14")))
      end do
      call revert_(stdout)
   end subroutine

   subroutine put_script(script,stem,a,b,c,s_max)
   ! Write the gnuplot "script": the model with its start values, the
   ! fit on [0,s_max], the print of the coefficients, and the plot of
   ! the difference.
      STR(len=*), IN :: script,stem
      VEC(REAL,:), IN :: a,b
      REAL, IN :: c
      REAL, IN :: s_max
      STR(len=STR_SIZE) :: fn,via,si,sm
      INT :: i

      ! The model is written in square roots, f(x) = sum A_i^2 exp(-B_i^2 x^2) + C^2,
      ! so that every a_i, b_i and c stays positive, as in the International
      ! Tables; the printed values and esds are converted back to a, b, c.
      fn = "f(x) = C**2"
      via = "C"
      do i = size(a),1,-1
         si = trim(adjustl(to_str_(i)))
         fn = "f(x) = A"//trim(si)//"**2*exp(-B"//trim(si)//"**2*x**2) + "//fn(8:)
         via = "A"//trim(si)//",B"//trim(si)//","//via
      end do
      sm = trim(adjustl(to_str_(s_max,"f10.4")))
      call redirect_(stdout,script)
      call text_(stdout,"# Fit of a spherical atom form factor to Gaussians, written by Tonto.")
      call text_(stdout,"# Run with:  gnuplot '"//trim(script)//"'")
      call text_(stdout,trim(fn))
      do i = 1,size(a)
         si = trim(adjustl(to_str_(i)))
         call text_(stdout,"A"//trim(si)//" = "//trim(adjustl(to_str_(sqrt(abs(a(i))),"es22.14"))))
         call text_(stdout,"B"//trim(si)//" = "//trim(adjustl(to_str_(sqrt(abs(b(i))),"es22.14"))))
      end do
      call text_(stdout,"C = "//trim(adjustl(to_str_(sqrt(abs(c)),"es22.14"))))
      call text_(stdout,"set fit quiet")
      call text_(stdout,"set fit errorvariables")
      call text_(stdout,"set fit limit 1e-10")
      call text_(stdout,"set fit maxiter 2000")
      call text_(stdout,"set fit logfile '"//trim(stem)//".fit.log'")
      call text_(stdout,"fit [0:"//trim(sm)//"] f(x) '"//trim(stem)//".dat' using 1:2 via "//trim(via))
      ! One value per line, so Tonto can read them back
      call text_(stdout,"set print '"//trim(stem)//".coeffs'")
      do i = 1,size(a)
         si = trim(adjustl(to_str_(i)))
         call text_(stdout,"print A"//trim(si)//"**2; print 2*abs(A"//trim(si)//")*A"//trim(si)//"_err")
         call text_(stdout,"print B"//trim(si)//"**2; print 2*abs(B"//trim(si)//")*B"//trim(si)//"_err")
      end do
      call text_(stdout,"print C**2; print 2*abs(C)*C_err")
      call text_(stdout,"print FIT_WSSR; print FIT_NDF")
      call text_(stdout,"set terminal pngcairo size 900,600")
      call text_(stdout,"set output '"//trim(stem)//".png'")
      call text_(stdout,"set xlabel 'sin(theta)/lambda / 1/Angstrom'")
      call text_(stdout,"set ylabel 'f(s) - fit(s) / electrons'")
      call text_(stdout,"set grid")
      call text_(stdout,"plot '"//trim(stem)//".dat' using 1:($2-f($1)) with lines title 'f(s) - fit'")
      call revert_(stdout)
   end subroutine


!  ======
!  Output
!  ======

   subroutine put_SHELX_SFAC(label,a,b,c,comment)
   ! Write one SHELX SFAC instruction for a fitted atom type, as
   !    SFAC label a1 b1 a2 b2 a3 b3 a4 b4 c f' f" mu r wt
   ! to the current stdout. The fit must have four Gaussians, which is
   ! what SHELX reads; f', f", mu, r and wt are written as 0, to be
   ! filled in by the user. "comment" goes before it as a REM line.
      STR(len=*), IN :: label
      VEC(REAL,:), IN :: a,b
      REAL, IN :: c
      STR(len=*), optional, IN :: comment
      STR(len=STR_SIZE) :: line
      INT :: i
   ENSURE(size(a)==4,"GAUSSIAN_FF_FIT:put_SHELX_SFAC ... SHELX SFAC takes exactly four Gaussians")
      if (present(comment)) call text_(stdout,"REM "//trim(comment))
      line = "SFAC "//trim(label)
      do i = 1,4
         line = trim(line)//" "//trim(to_str_(a(i),"f10.5"))//" "//trim(to_str_(b(i),"f10.5"))
      end do
      line = trim(line)//" "//trim(to_str_(c,"f10.5"))//" ="
      call text_(stdout,trim(line))
      call text_(stdout,"     0.00000   0.00000   0.00000   0.00000   0.00000")
   end subroutine

   subroutine put_table_line(label,a,b,c)
   ! Write one line of the plain table: label, n, a_1..a_n, b_1..b_n, c.
      STR(len=*), IN :: label
      VEC(REAL,:), IN :: a,b
      REAL, IN :: c
      STR(len=STR_SIZE) :: line
      INT :: i
      line = label
      line = line(1:8)//" "//trim(to_str_(size(a),"i4"))
      do i = 1,size(a)
         line = trim(line)//" "//trim(to_str_(a(i),"f14.6"))
      end do
      do i = 1,size(a)
         line = trim(line)//" "//trim(to_str_(b(i),"f14.6"))
      end do
      line = trim(line)//" "//trim(to_str_(c,"f14.6"))
      call text_(stdout,trim(line))
   end subroutine


!  =====
!  Check
!  =====

   function check_round_trip() result (n_failed)
   ! Fit the International Tables curve of carbon, made from its own
   ! coefficients on an even s grid to 2/Angstrom, starting from
   ! those coefficients perturbed by 10%, and count how many of the
   ! nine come back further than 1e-4 from where they started.
   ! Returns -1 if gnuplot could not be run.
      INT :: n_failed
      VEC(REAL,:), allocatable :: s,f,a,b,a0,b0
      REAL :: c,c0,rms,max_dev
      INT :: i,n
      BIN :: ok
      n = 201
      call create_(s,n)
      do i = 1,n
         s(i) = TWO*(i-1)/(n-1)
      end do

      ! Carbon, International Tables Vol. C, Table 6.1.1.4
      a0 = [2.31000d0,1.02000d0,1.58860d0,0.865000d0]
      b0 = [20.8439d0,10.2075d0,0.568700d0,51.6512d0]
      c0 = 0.215600d0
      f = gaussian_sum_(s,a0,b0,c0)
      a = 1.1d0*a0; b = 0.9d0*b0; c = 1.1d0*c0
      call fit_(s,f,a,b,c,TWO,"carbon_IT_check",ok)
      if (NOT ok) then
         n_failed = -1
      else
         n_failed = count(abs(a-a0)>1.0d-4)+count(abs(b-b0)>1.0d-4)
         if (abs(c-c0)>1.0d-4) n_failed = n_failed+1
         call deviations_(s,f,a,b,c,TWO,rms,max_dev)
         call flush_(stdout)
         call text_(stdout,"Round-trip fit of the carbon International Tables curve:")
         call show_(stdout,"rms deviation         =",rms)
         call show_(stdout,"largest deviation     =",max_dev)
         call show_(stdout,"coefficients off      =",n_failed)
         call flush_(stdout)
      end if
      call destroy_(b); call destroy_(a)
      call destroy_(f); call destroy_(s)
   end function

end module
