!-------------------------------------------------------------------------------
!
! run_lsq_restraint: checks MAT{REAL}:solve_restrained_linear_equations, the
! restrained normal-equation solver used by the structure refinement, and
! exits non-zero if a check fails. The `restrained_least_squares_known_answer` ctest.
!
! Copyright (C) Dylan Jayatilaka, 2026
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation; either
! version 2 of the License, or (at your option) any later version.
!
!-------------------------------------------------------------------------------
program run_LSQ_RESTRAINT

#  include "macros"

   include "run_lsq_restraint.use"

   implicit none

   INT :: n_failed
   TONTO_CREATE
   call start_timing_(std_time)
   call create_std_err
   call open_for_(std_err,"write")
   call create_stdout
   n_failed = check_restrained_solve_()
   DIE_IF(n_failed>0,"run_LSQ_RESTRAINT:run_LSQ_RESTRAINT ... restrained least-squares check failed")
   call destroy_(std_err)
   call destroy_(stdout)
   TONTO_DESTROY

end program
