!-------------------------------------------------------------------------------
!
! run_gaussian_ff_fit: checks GAUSSIAN_FF_FIT, the fit of a spherical form
! factor to Gaussians through gnuplot, by a round trip on the carbon
! International Tables curve. Exits non-zero if a coefficient does not come
! back, and with 77 (ctest "skipped") if gnuplot cannot be run.
! The `gaussian_ff_fit` ctest.
!
! Copyright (C) Dylan Jayatilaka, 2026
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation; either
! version 2 of the License, or (at your option) any later version.
!
!-------------------------------------------------------------------------------
program run_GAUSSIAN_FF_FIT

#  include "macros"

   include "run_gaussian_ff_fit.use"

   implicit none

   INT :: n_failed
   TONTO_CREATE
   call start_timing_(std_time)
   call create_std_err
   call open_for_(std_err,"write")
   call create_stdout
   n_failed = check_round_trip_()
   if (n_failed<0) then
      call text_(stdout,"gnuplot could not be run: the check is skipped")
      call flush_(stdout)
      stop 77
   end if
   DIE_IF(n_failed>0,"run_GAUSSIAN_FF_FIT:run_GAUSSIAN_FF_FIT ... Gaussian form factor fit check failed")
   call destroy_(std_err)
   call destroy_(stdout)
   TONTO_DESTROY

end program
