program run_LIBXC

#  include "macros"

   include "run_libxc.use"

   implicit none

! Check that Tonto can call libxc: Slater exchange from libxc, for one and for
! two spin channels, against its closed form; that libxc reports PBE0 as a
! gradient functional with a quarter of exact exchange and r2SCAN as one that
! needs the kinetic energy density; and that each hand-written functional of
! DFT_FUNCTIONAL equals its libxc counterpart (DFT_FUNCTIONAL:check_libxc).
! Fails loudly if any is wrong. Registered as the `libxc_gives_slater_exchange`
! ctest; no reference file is involved. The routines called are in
! csrc/tonto_libxc.c.
   interface
      subroutine tonto_xc_handle(handle,name,nspin)
         INT, OUT :: handle
         STR(*), IN :: name
         INT, IN :: nspin
      end subroutine
      subroutine tonto_xc_rung(handle,rung)
         INT, IN :: handle
         INT, OUT :: rung
      end subroutine
      subroutine tonto_xc_exact_exchange(handle,a0)
         INT, IN :: handle
         REAL, OUT :: a0
      end subroutine
      pure subroutine tonto_xc_lda_exc_vxc(handle,np,rho,exc,vrho)
         INT, IN :: handle,np
         VEC(REAL,*), IN :: rho
         VEC(REAL,*), INOUT :: exc,vrho
      end subroutine
   end interface
   INT, parameter :: n_pt = 5
   VEC(REAL,n_pt) :: rho,exc,vrho,ref
   VEC(REAL,2*n_pt) :: rho2,vrho2
   REAL :: ra,rb,cx,a0,err
   INT :: handle,rung,i,n_failed
   TONTO_CREATE
   call start_timing_(std_time)
   call create_std_err
   call open_for_(std_err,"write")
   call create_stdout
   n_failed = 0
   rho = [1.0d-6,1.0d-3,0.1d0,1.0d0,100.0d0]
   call flush_(stdout)
   call text_(stdout,"libxc against closed forms (max relative error)")
   call flush_(stdout)

   ! Slater exchange, one spin channel: -(3/4)(3/pi)^(1/3) rho^(1/3) per particle
   call tonto_xc_handle(handle,"lda_x",1)
   DIE_IF(handle==0,"run_LIBXC:run_LIBXC ... libxc could not set up lda_x")
   call tonto_xc_lda_exc_vxc(handle,n_pt,rho,exc,vrho)
   cx = -THREEQUARTERS*(THREE/PI)**THIRD
   ref = cx*rho**THIRD
   err = max(maxval(abs(exc/ref-ONE)),maxval(abs(vrho/(FOURTHIRD*ref)-ONE)))
   call show_(stdout,"Slater exchange, restricted    =",err,precision=14)
   if (err>TOL(12)) n_failed = n_failed+1

   ! The same for two spin channels, with three parts alpha to one part beta
   do i = 1,n_pt
      rho2(2*i-1) = THREEQUARTERS*rho(i)
      rho2(2*i) = QUARTER*rho(i)
   end do
   call tonto_xc_handle(handle,"lda_x",2)
   DIE_IF(handle==0,"run_LIBXC:run_LIBXC ... libxc could not set up lda_x for two spin channels")
   call tonto_xc_lda_exc_vxc(handle,n_pt,rho2,exc,vrho2)
   cx = -THREEQUARTERS*(SIX/PI)**THIRD
   err = ZERO
   do i = 1,n_pt
      ra = rho2(2*i-1)
      rb = rho2(2*i)
      err = max(err,abs(exc(i)/(cx*(ra**FOURTHIRD+rb**FOURTHIRD)/rho(i))-ONE))
      err = max(err,abs(vrho2(2*i-1)/(FOURTHIRD*cx*ra**THIRD)-ONE))
      err = max(err,abs(vrho2(2*i)/(FOURTHIRD*cx*rb**THIRD)-ONE))
   end do
   call show_(stdout,"Slater exchange, unrestricted  =",err,precision=14)
   if (err>TOL(12)) n_failed = n_failed+1

   ! PBE0: a gradient functional with a quarter of exact exchange
   call tonto_xc_handle(handle,"hyb_gga_xc_pbeh",1)
   DIE_IF(handle==0,"run_LIBXC:run_LIBXC ... libxc could not set up hyb_gga_xc_pbeh")
   call tonto_xc_rung(handle,rung)
   call tonto_xc_exact_exchange(handle,a0)
   call show_(stdout,"PBE0 exact exchange fraction   =",a0)
   if (rung/=2 OR abs(a0-QUARTER)>TOL(12)) n_failed = n_failed+1

   ! r2SCAN exchange needs the kinetic energy density
   call tonto_xc_handle(handle,"mgga_x_r2scan",1)
   DIE_IF(handle==0,"run_LIBXC:run_LIBXC ... libxc could not set up mgga_x_r2scan")
   call tonto_xc_rung(handle,rung)
   call show_(stdout,"r2SCAN exchange, kind of input =",rung)
   if (rung/=3) n_failed = n_failed+1

   ! A name libxc does not have
   call tonto_xc_handle(handle,"no_such_functional",1)
   if (handle/=0) n_failed = n_failed+1

   ! Each hand-written functional against its libxc counterpart
   n_failed = n_failed+check_libxc_()
   call flush_(stdout)
   call show_(stdout,"Checks failed =",n_failed)
   DIE_IF(n_failed>0,"run_LIBXC:run_LIBXC ... libxc check failed")
   call destroy_(std_err)
   call destroy_(stdout)
   TONTO_DESTROY

end program
