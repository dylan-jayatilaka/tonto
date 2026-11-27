program run_QUADRATURE

#  include "macros"

   include "run_quadrature.use"

   implicit none

! Exercise every rule in the QUADRATURE module against integrals known in
! closed form, and fail loudly if any is wrong. Registered as the
! `quadrature_rules_integrate_exactly` ctest; no reference file is involved. The checks
! themselves are QUADRATURE:check_rules.
   INT :: n_failed
   TONTO_CREATE
   call start_timing_(std_time)
   call create_std_err
   call open_for_(std_err,"write")
   call create_stdout
   n_failed = check_rules_()
   DIE_IF(n_failed>0,"run_QUADRATURE:run_QUADRATURE ... QUADRATURE rule check failed")
   call destroy_(std_err)
   call destroy_(stdout)
   TONTO_DESTROY

end program
