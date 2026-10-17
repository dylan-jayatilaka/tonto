!==================================================================
!
! SCF_DATA: Store SCF data and deal with iteration control ......
!
! Copyright (C) Daniel Grimwood, 1998
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
!==================================================================
module SCF_DATA_MODULE

#  include "macros"

   include "scf_data.use"

   implicit none

   include "scf_data.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(SCF_DATA_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"SCF_DATA:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(SCF_DATA_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"SCF_DATA:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      type(SCF_DATA_TYPE), INOUT :: self
      call destroy_ptr_part_(self%SCF_DIIS)
      call destroy_(self%CP_DIIS)

   ! .crystal.destroy
      call destroy_(self%cluster)
      call destroy_(self%cluster_charges)
      call destroy_(self%cluster_charge_positions)
      call destroy_(self%table_info)
      call destroy_(self%table)
   end subroutine


!  ===============================================
!  Set defaults, reset, update and realted methods
!  ===============================================

   PURE subroutine set_defaults(self)
   ! Set default SCF data values
      type(SCF_DATA_TYPE), INOUT :: self
      type(SCF_DATA_TYPE) :: data
      self = data

      ! DIIS convergence related data (leaky)
      call set_defaults_(self%SCF_DIIS)
   end subroutine

   PURE subroutine set_pauli_terms_on(self)
   ! Turn on all the Pauli terms
      type(SCF_DATA_TYPE), INOUT :: self
      self%using_1e_sl_term = TRUE
      self%using_1e_srxa_term = TRUE
      self%using_2e_sl_term = TRUE
      self%using_aa_term = TRUE
      self%using_bl_term = TRUE
      self%using_bs_term = TRUE
      self%using_bs_t_term = TRUE
   end subroutine

   PURE subroutine set_pauli_terms_off(self)
   ! Turn on all the Pauli terms
      type(SCF_DATA_TYPE), INOUT :: self
      self%using_1e_sl_term = FALSE
      self%using_1e_srxa_term = FALSE
      self%using_2e_sl_term = FALSE
      self%using_aa_term = FALSE
      self%using_bl_term = FALSE
      self%using_bs_term = FALSE
      self%using_bs_t_term = FALSE
   end subroutine


!   set_crystal(crystal) :: PURE
!   ! Set the crystal
!      self :: INOUT
!      crystal :: CRYSTAL@, IN
!
!      ! Set crystal and penalty
!      if (.iteration==0) then
!      if (crystal.allocated) then
!         .crystal = crystal
!      end
!      end
!
!   end

   PURE subroutine set_penalty(self,crystal)
   ! Set the penalties for the diffraction data present
      type(SCF_DATA_TYPE), INOUT :: self
      type(CRYSTAL_TYPE), IN :: crystal
      self%old_penalty = self%penalty
      select case (self%SCF_kind)

      case ("xray_rhf","xray_uhf","xray_gchf")
         ENSURE(allocated(crystal%xray_data),"SCF_DATA:set_penalty ... No diffraction data")
         self%penalty = crystal%xray_data%GoF2

      case ("xray_rks","xray_uks")
         ENSURE(allocated(crystal%xray_data),"SCF_DATA:set_penalty ... No diffraction data")
         self%penalty = crystal%xray_data%GoF2
      end select
   end subroutine

   PURE subroutine set_table(self)
   ! Set up the SCF table
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: n

      ! Initialize save margin note data
      call destroy_(self%table_info)
      call create_(self%table_info,1,20)
      call set_margin_info_(self)

      ! Set table headings
      if (NOT is_constrained_SCF_(self)) then

         ! The margin-note column is last; the optional Time column
         ! goes before it.
         n = 5
         if (self%show_timings) n = 6
         call create_(self%table,n)
         call set_heading_(self%table(1),"Iter")
         call set_subhead_(self%table(1),"   #")
         call force_width_(self%table(1),5)
         call set_heading_(self%table(2),"Energy")
         call set_subhead_(self%table(2)," - E -")
         call set_heading_(self%table(3),"Delta")
         call set_subhead_(self%table(3),"- E -")
         call set_heading_(self%table(4),"- DIIS -")
         call set_subhead_(self%table(4),"Gradient")
         if (self%show_timings) then
            call set_heading_(self%table(5),"  Time")
            call set_subhead_(self%table(5),"  CPU s")
            call force_width_(self%table(5),9)
            call set_real_precision_(self%table(5),2)
         end if
         call set_width_(self%table(n),0)

         ! Table widths
         call set_width_from_(self%table(2),self%energy)
         call set_width_from_(self%table(3),self%energy)
         call set_width_from_(self%table(4),self%energy)
         call force_width_(self%table(n),0)
         call set_left_(self%table(n),TRUE)
         call set_list_as_column_(self%table(n),TRUE)
      else
         n = 8
         if (self%show_timings) n = 9
         call create_(self%table,n)
         call set_heading_(self%table(1),"Iter")
         call set_subhead_(self%table(1),"   #")
         call force_width_(self%table(1),5)
         call set_heading_(self%table(2),"Lambda")
         call set_subhead_(self%table(2)," - L -")
         call set_heading_(self%table(3),"   GoF")
         call set_heading_(self%table(4),"Energy")
         call set_subhead_(self%table(4),"- E -")
         call set_heading_(self%table(5),"  Delta ")
         call set_subhead_(self%table(5),"E+L*GoF2")
         call set_heading_(self%table(6),"- DIIS -")
         call set_subhead_(self%table(6),"Gradient")
         call set_heading_(self%table(7)," <MO|M0>")
         call set_subhead_(self%table(7)," overlap")
         if (self%show_timings) then
            call set_heading_(self%table(8),"  Time")
            call set_subhead_(self%table(8),"  CPU s")
            call force_width_(self%table(8),9)
            call set_real_precision_(self%table(8),2)
         end if

         ! Table widths
         call set_width_from_(self%table(2),self%lambda)
         call set_width_from_(self%table(3),sqrt(self%penalty)*TEN**(-4))
         call set_real_precision_(self%table(3),2)
         call set_width_from_(self%table(4),self%energy)
         call set_width_from_(self%table(5),self%fit_value)
         call set_width_from_(self%table(6),self%energy)
         call set_width_from_(self%table(7),self%energy)
         call force_width_(self%table(n),0)
         call set_left_(self%table(n),TRUE)
         call set_list_as_column_(self%table(n),TRUE)
      end if
   end subroutine

   PURE subroutine set_CP_table(self)
   ! Set up the CP table
      type(SCF_DATA_TYPE), INOUT :: self

      ! Initialize save margin note data
      call destroy_(self%table_info)
      call create_(self%table_info,1,20)
      call set_margin_info_(self)

      ! Set table headings
      call create_(self%table,4)
      call set_heading_(self%table(1),"Iter")
      call set_heading_(self%table(2),"      max")
      call set_subhead_(self%table(2),"|Delta U|")
      call set_heading_(self%table(3),"max DIIS")
      call set_subhead_(self%table(3),"   error")
      call set_width_(self%table(4),0)

      ! Table widths
      call set_width_from_(self%table(1),self%max_iterations)
      call set_width_from_(self%table(2),1.0d4)
      call set_width_from_(self%table(3),1.0d4)
      call force_width_(self%table(4),0)
      call set_left_(self%table(4),TRUE)
      call set_list_as_column_(self%table(4),TRUE)
   end subroutine

   PURE subroutine set_margin_info(self,n_notes)
   ! Set the margin information "info" for the SCF iterations
   ! This usesd SAVE vafiables, so is not PURE.
      type(SCF_DATA_TYPE), INOUT :: self
      INT, optional, OUT :: n_notes
      INT :: n,i,s

      ! Initialize saved switches
      if (self%iteration<=0) then
         self%delta_E_converged = FALSE
         self%damping_on = FALSE
         self%damping_off = FALSE
         self%pFON_on = FALSE
         self%pFON_off = FALSE
         self%level_shift_on = FALSE
         self%level_shift_off = FALSE
         self%DIIS_converged = FALSE
         self%DIIS_saving = FALSE
         self%DIIS_started = FALSE
         self%DIIS_used_up = FALSE
      end if

      ! Initialize info
      n = 0
      self%table_info(1)%element(:) = " "

      ! No notes ...
      if (SCF_done_(self)) return

      ! Set margin info ...
      if (NOT self%delta_E_converged AND delta_converged_(self) AND self%iteration>0) then
         n = n+1
         self%table_info(1)%element(n) = " *Difference has converged"
         self%delta_E_converged = TRUE
      end if
      if (NOT self%DIIS_converged AND is_converged_(self%SCF_DIIS)) then
         n = n+1
         self%table_info(1)%element(n) = " *Gradient has converged"
         self%DIIS_converged = TRUE
      end if
      if (apply_damping_(self) AND NOT self%damping_on) then
         n = n+1
         self%table_info(1)%element(n) = " *Damping on"
         self%damping_on = TRUE
      else if (NOT apply_damping_(self) AND NOT self%damping_off) then
         n = n+1
         self%table_info(1)%element(n) = " *Damping was off"
         self%damping_off = TRUE
      end if
      if (self%using_pFON) then
         if (apply_pFON_(self) AND NOT self%pFON_on) then
            n = n+1
            self%table_info(1)%element(n) = " *pFON occupations on"
            self%pFON_on = TRUE
         else if (NOT apply_pFON_(self) AND self%pFON_on AND NOT self%pFON_off) then
            n = n+1
            self%table_info(1)%element(n) = " *pFON occupations was off"
            self%pFON_off = TRUE
         end if
      end if
      if (apply_level_shifting_(self) AND NOT self%level_shift_on) then
         n = n+1
         self%table_info(1)%element(n) = " *Levelshift on"
         self%level_shift_on = TRUE
      else if (NOT apply_level_shifting_(self) AND NOT self%level_shift_off) then
         n = n+1
         self%table_info(1)%element(n) = " *Levelshift was off"
         self%level_shift_off = TRUE
      end if
      if (self%using_DIIS) then
         if (saved_iteration_(self%SCF_DIIS)==0 AND NOT self%DIIS_saving) then
            n = n+1
            self%table_info(1)%element(n) = " *DIIS starts saving now"
            self%DIIS_saving = TRUE
         end if
         if (actual_iteration_(self%SCF_DIIS)==0 AND NOT self%DIIS_started) then
            n = n+1
            self%table_info(1)%element(n) = " *DIIS extrapolates now"
            self%DIIS_started = TRUE
         end if
         if (subspace_saturated_(self%SCF_DIIS) AND self%DIIS_used_up) then
            n = n+1
            i = replaced_iteration_(self%SCF_DIIS)
            s = self%SCF_DIIS%new
            self%table_info(1)%element(n) = " *DIIS replaced saved #"//trim(to_str_(s))//" iter #"//trim(to_str_(i))
         end if
         if (self%SCF_DIIS%kept<=2 AND self%DIIS_used_up) then
            n = n+1
            i = self%SCF_DIIS%iter_4(1)-1
            s = self%SCF_DIIS%new
            if (self%SCF_DIIS%kept==1) then
               self%table_info(1)%element(n) = " *DIIS stuck, total reset"
            else
               self%table_info(1)%element(n) = " *DIIS stuck, kept saved #"//trim(to_str_(s))//" iter #"//trim(to_str_(i))
            end if
            self%DIIS_used_up = FALSE
         end if

         ! This must come last
         if (subspace_saturated_(self%SCF_DIIS) AND NOT self%DIIS_used_up) then
            n = n+1
            self%table_info(1)%element(n) = " *DIIS subspace is saturated"
            self%DIIS_used_up = TRUE
         end if
      end if
      if (present(n_notes)) n_notes = n
   end subroutine

   PURE subroutine reset(self)
   ! Reset SCF energy and iteration data , but don't change
   ! nuclear_repulsion_energy, convergence, direct, or max_it options
      type(SCF_DATA_TYPE), INOUT :: self

      ! Stored total energy/Lagrangian
      self%energy = ZERO
      self%fit_value = ZERO
      self%old_fit_value = ZERO

      ! Stored part-energies
      self%nuclear_attraction_energy = ZERO  ! Electron-nuclear attraction energy
      self%charge_attraction_energy = ZERO  ! Point-charge attraction energy
      self%electron_repulsion_energy = ZERO  ! Electron-electron repulsion energy
      self%kinetic_energy = ZERO  ! Electron kinetic energy
      self%old_energy = ZERO

      ! Iteration count
      self%iteration = 0
      self%total_iterations = 0
      self%lambda_iteration = 0
      self%penalty = ZERO

      ! DIIS (leaky)
      call reset_iteration_defaults_(self%SCF_DIIS)

      ! BFGS
      call set_defaults_(self%bfgs)
   end subroutine

   PURE subroutine reset_for_next_lambda(self)
   ! Reset a few things for the next constrained-scf lambda iteration:
      type(SCF_DATA_TYPE), INOUT :: self

      ! Reset iteration & margin info
      self%iteration = -1
      call set_margin_info_(self)

      ! DIIS (leaky)
      call reset_iteration_defaults_(self%SCF_DIIS)
   end subroutine

   PURE subroutine reset_CP(self,n_pert)
   ! Reset the "n_pert" CPHF DIIS extrapolators
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: n_pert

      ! Reset iterations
      call reset_(self)

      ! Set defaults
      call set_n_pert_(self,n_pert)
      call set_using_damping_(self,FALSE)
      call set_using_level_shift_(self,FALSE)
      call set_min_iterations_(self,max(2,self%min_iterations))

      ! Set DIIS defaults
      call set_convergence_tolerance_(self%SCF_DIIS,TOL(4))
      call set_keep_(self%SCF_DIIS,4)

      ! CPHF DIIS (leaky)
      call destroy_(self%CP_DIIS)
      call create_(self%CP_DIIS,n_pert)
      call set_defaults_(self%CP_DIIS,self%SCF_DIIS)
      call set_use_errors_(self%CP_DIIS,TRUE)
   end subroutine

   pure subroutine update_lambda(self)
   ! Increments lambda by lambda_step
      type(SCF_DATA_TYPE), INOUT :: self
      self%lambda_iteration = self%lambda_iteration+1
      self%lambda = self%lambda+self%lambda_step
      self%fit_value = self%energy+self%lambda*self%penalty
      self%old_fit_value = ZERO
   end subroutine

   pure subroutine update_iteration(self)
   ! Update the SCF iteration
      type(SCF_DATA_TYPE), INOUT :: self
      self%iteration = self%iteration+1
      self%total_iterations = self%total_iterations+1
   end subroutine

   PURE subroutine update_energy(self,energy,crystal)
   ! Update the energy and penalty needed in constrained fits.
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: energy
      type(CRYSTAL_TYPE), optional, IN :: crystal

      ! Keep old values
      self%old_energy = self%energy
      self%old_fit_value = self%fit_value

      ! Set new values
      self%energy = energy
      if (is_constrained_SCF_(self)) then
         call set_penalty_(self,crystal)
         self%fit_value = energy+self%lambda*self%penalty
         if (self%iteration==0) self%old_fit_value = ZERO
      end if

      ! Set initial energy
      if (self%iteration==0) then
         self%E_initial = self%energy
      end if

      ! Set energy differences
      self%delta = self%energy-self%old_energy
   end subroutine

   PURE subroutine update_DIIS_from_CP_DIIS(self)
   ! Update the CP vector difference
      type(SCF_DATA_TYPE), INOUT :: self
      self%SCF_DIIS = self%CP_DIIS(1)
   end subroutine

   pure subroutine update_CP_difference(self,delta)
   ! Update the CP vector difference
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: delta
      self%delta = delta
   end subroutine


!  ============
!  Set routines
!  ============

   pure subroutine set_ELMO_B(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val
      self%elmo_b = val
   end subroutine

   pure subroutine set_ELMO_P(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val
      self%elmo_p = val
   end subroutine

   pure subroutine set_use_ELMO_01(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_ELMO_01 = val
   end subroutine

   pure subroutine set_use_ELMO_11(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_ELMO_11 = val
   end subroutine

   pure subroutine set_use_BFGS(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_BFGS = val
   end subroutine

   pure subroutine set_use_LSQ(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_LSQ = val
   end subroutine

   pure subroutine set_1e_sl_factor(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%sl_1e_factor = val
   end subroutine

   pure subroutine set_2e_sl_factor(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%sl_2e_factor = val
   end subroutine

   pure subroutine set_atom_overlap_cutoff(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%atom_overlap_cutoff = val
   end subroutine

   pure subroutine set_delete_SCF_archives(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%delete_SCF_archives = val
   end subroutine

   pure subroutine set_delete_all_archives(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%delete_all_archives = val
   end subroutine

   PURE subroutine set_cluster_radius(self,val)
   ! Set the cluster radius
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>0,"SCF_DATA:set_cluster_radius ... cluster_radius must be positive")
      self%cluster_radius = val
   end subroutine

   PURE subroutine set_convergence(self,val)
   ! Set SCF energy convergence criteria
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

! ENSURE(.convergence>TOL(12),"convergence may be too small")
      self%convergence = val
   end subroutine

   PURE subroutine set_guess_convergence(self,val)
   ! Set the energy convergence criterion for a *guess* SCF -- the atomic
   ! calculations behind the ANOs, promolecule and Hirshfeld weights.
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val
      self%guess_convergence = val
   end subroutine

   PURE subroutine set_damp_factor(self,val)
   ! Set the SCF density matrix damping factor
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>ZERO,"SCF_DATA:set_damp_factor ... damp_factor must be positive")
   ENSURE(val<=ONE,"SCF_DATA:set_damp_factor ... damp_factor must not exceed 1 ")
      self%damp_factor = val
   end subroutine

   PURE subroutine set_damp_finish(self,val)
   ! Set when density matrix damping finishes.
   ! NOTE: This also sets level_shift_finish to the same.
   !       If you don't want this, then set it separately.
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>0,"SCF_DATA:set_damp_finish ... damp_finish must be positive")
      self%damp_finish = val
      self%level_shift_finish = val
   end subroutine

   PURE subroutine set_defragment(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%defragment = val
   end subroutine

   subroutine set_functional(self,val)
   ! Set the whole DFT functional by one name "val". The name is either one
   ! of the common names below, or any name as libxc spells it, such as
   ! "hyb_gga_xc_pbeh". It is stored as an exchange and a correlation
   ! functional, exactly as if the two had been set; a libxc functional
   ! that is exchange and correlation together is stored as the exchange,
   ! with no correlation.
   ! NOTE: programs disagree on the local correlation in B3LYP, and the
   ! energies differ: by 37 millihartree for a water molecule.
   ! . "b3lyp-gaussian" has the fit of Vosko, Wilk and Nusair to the RPA
   !   correlation energy (their functional III; VWN1RPA or VWN-RPA
   !   elsewhere). It is B3LYP in Gaussian, NWChem and Psi4, B3LYP/G in ORCA,
   !   and hyb_gga_xc_b3lyp in libxc. "b3lyp" means this one.
   ! . "b3lyp-orca" has their fit to the Ceperley-Alder energy (functional
   !   V, VWN5). It is B3LYP in ORCA, Turbomole and GAMESS, and
   !   hyb_gga_xc_b3lyp5 in libxc.
   ! . A third form, hyb_gga_xc_b3lyp3 in libxc, can be asked for by that name.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      STR(len=STR_SIZE) :: name,exch,corr
      name = val
      call to_lower_case_(name)
      select case (name)
      case ("lda           "); exch = "slater"; corr = "vwn5"
      case ("xalpha        "); exch = "xalpha"; corr = "none"
      case ("blyp          "); exch = "becke88"; corr = "lyp"
      case ("b3lyp         "); exch = "b3lypgx"; corr = "b3lypgc"
      case ("b3lyp-gaussian"); exch = "b3lypgx"; corr = "b3lypgc"
      case ("b3lyp-orca    "); exch = "b3lypx"; corr = "b3lypc"
      case ("pbe           "); exch = "gga_x_pbe"; corr = "gga_c_pbe"
      case ("pbe0          "); exch = "hyb_gga_xc_pbeh"; corr = "none"
      case ("r2scan        "); exch = "mgga_x_r2scan"; corr = "mgga_c_r2scan"
      case default
         DIE_IF(NOT is_libxc_functional_(name),"SCF_DATA:set_functional ... unknown DFT functional: "//trim(name))
         exch = name
         corr = "none"
      end select
      call set_exchange_functional_(self,exch)
      call set_correlation_functional_(self,corr)
      self%DFT_functional = name
   end subroutine

   subroutine get_libxc_functional_info(self,name,uses_gradient,uses_tau,a0)
   ! Check that libxc can evaluate the functional "name" for Tonto, and stop
   ! with a message if not. Return "uses_gradient", whether it uses the
   ! density gradient; "uses_tau", whether it uses the kinetic energy
   ! density; and "a0", its fraction of exact exchange.
      type(SCF_DATA_TYPE), IN :: self
      STR(len=*), IN :: name
      BIN, OUT :: uses_gradient,uses_tau
      REAL, OUT :: a0
      BIN :: found
      INT :: rung
      call get_libxc_info_(name,found,rung,a0)
      DIE_IF(NOT found,"SCF_DATA:get_libxc_functional_info ... libxc has no functional called "//trim(name)//", or this Tonto was built without libxc")
      DIE_IF(rung>3,"SCF_DATA:get_libxc_functional_info ... the functional "//trim(name)//" needs the Laplacian of the density, which Tonto does not make")
      DIE_IF(a0<ZERO,"SCF_DATA:get_libxc_functional_info ... the functional "//trim(name)//" needs range separation or non-local correlation, which Tonto does not have")
      uses_gradient = rung>=2
      uses_tau = rung==3
   end subroutine

   subroutine set_correlation_functional(self,val)
   ! Set the DFT correlation functional. A name as libxc spells it, such as
   ! "gga_c_pbe", is evaluated by libxc.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      REAL :: a0
      self%DFT_correlation_functional = val
      call to_lower_case_(self%DFT_correlation_functional)
      self%DFT_functional = " "
      self%using_meta_GGA_correlation = FALSE
      if (is_libxc_functional_(self%DFT_correlation_functional)) then
         call get_libxc_functional_info_(self,self%DFT_correlation_functional,self%using_GGA_correlation,self%using_meta_GGA_correlation,a0)
         DIE_IF(a0>ZERO,"SCF_DATA:set_correlation_functional ... give a hybrid functional as the exchange functional, or by dft_functional=")
         return
      end if
      select case (self%DFT_correlation_functional)
      case ("       "); self%using_GGA_correlation = FALSE
      case ("none   "); self%using_GGA_correlation = FALSE
      case ("vwn5   "); self%using_GGA_correlation = FALSE
      case ("vwn3   "); self%using_GGA_correlation = FALSE
      case ("lyp    "); self%using_GGA_correlation = TRUE
      case ("b3lypc "); self%using_GGA_correlation = TRUE
      case ("b3lypgc"); self%using_GGA_correlation = TRUE
      case default; allocate(tonto%known_keywords(7))
      tonto%known_keywords(1) = "       "
      tonto%known_keywords(2) = "none   "
      tonto%known_keywords(3) = "vwn5   "
      tonto%known_keywords(4) = "vwn3   "
      tonto%known_keywords(5) = "lyp    "
      tonto%known_keywords(6) = "b3lypc "
      tonto%known_keywords(7) = "b3lypgc"
      call unknown_(tonto,self%DFT_correlation_functional,"SCF_DATA:set_correlation_functional")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine set_exchange_functional(self,val)
   ! Set the DFT exchange functional. A name as libxc spells it, such as
   ! "gga_x_pbe", is evaluated by libxc, and may be a hybrid, or exchange
   ! and correlation together; its fraction of exact exchange is libxc's.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      REAL :: a0
      self%DFT_exchange_functional = val
      call to_lower_case_(self%DFT_exchange_functional)
      self%DFT_functional = " "
      self%using_hybrid_exchange = FALSE
      self%using_meta_GGA_exchange = FALSE
      if (is_libxc_functional_(self%DFT_exchange_functional)) then
         call get_libxc_functional_info_(self,self%DFT_exchange_functional,self%using_GGA_exchange,self%using_meta_GGA_exchange,a0)
         if (a0>ZERO) then
            self%using_hybrid_exchange = TRUE
            self%hybrid_exchange_factor = a0
         end if
         return
      end if
      select case (self%DFT_exchange_functional)
      case ("       "); self%using_GGA_exchange = FALSE
      case ("none   "); self%using_GGA_exchange = FALSE
      case ("slater "); self%using_GGA_exchange = FALSE
      case ("xalpha "); self%using_GGA_exchange = FALSE
      case ("becke88"); self%using_GGA_exchange = TRUE
      ! b3lypx and b3lypgx share the SAME exchange routine, which computes only
      ! 0.08*E_LDA + 0.72*E_GGA. B3LYP's remaining 0.2*E_HF comes from the Fock
      ! matrix, gated on .using_hybrid_exchange -- so both names must set it, or
      ! the exact exchange is silently dropped. See TASKS_AND_HISTORY.md.
      case ("b3lypx "); self%using_GGA_exchange = TRUE
         self%using_hybrid_exchange = TRUE
         self%hybrid_exchange_factor = 0.2d0
      case ("b3lypgx"); self%using_GGA_exchange = TRUE
         self%using_hybrid_exchange = TRUE
         self%hybrid_exchange_factor = 0.2d0
      case default; allocate(tonto%known_keywords(7))
      tonto%known_keywords(1) = "       "
      tonto%known_keywords(2) = "none   "
      tonto%known_keywords(3) = "slater "
      tonto%known_keywords(4) = "xalpha "
      tonto%known_keywords(5) = "becke88"
      tonto%known_keywords(6) = "b3lypx "
      tonto%known_keywords(7) = "b3lypgx"
      call unknown_(tonto,self%DFT_exchange_functional,"SCF_DATA:set_exchange_functional")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   pure subroutine set_DIIS_error(self,val)
   ! Set the DIIS error and the starting iteration for automatic DIIS start
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val
      self%old_DIIS_error = self%DIIS_error
      self%DIIS_error = val
   end subroutine

   pure subroutine set_DIIS_error_temp_cutoff(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%DIIS_error_temp_cutoff = val
   end subroutine

   pure subroutine set_ERI_bf_overlap_cutoff(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%ERI_bf_overlap_cutoff = val
   end subroutine

   pure subroutine set_ee_adiabatic_constant(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%ee_adiabatic_constant = val
   end subroutine

   pure subroutine set_Fermi_Amaldi(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_Fermi_Amaldi = val
   end subroutine

   pure subroutine set_ERI_disk_cutoff(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%ERI_disk_cutoff = val
   end subroutine

   pure subroutine set_ERI_J_density_cutoff(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%ERI_J_density_cutoff = val
   end subroutine

   pure subroutine set_ERI_K_density_cutoff(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%ERI_K_density_cutoff = val
   end subroutine

   subroutine set_ERI_primitive_pair_cutoff(self,val)
   ! Set the ERI primitive pair cutoff. The value that the integrals
   ! actually use lives in SHELL1QUARTET; the component is kept in step
   ! so that .using_nonstandrad_ERI_cutoffs can see it.
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val
      type(SHELL1QUARTET_TYPE) :: sh4
      call set_pair_cutoff_(sh4,val)
      self%ERI_primitive_pair_cutoff = val
   end subroutine

   subroutine set_ERI_accuracy(self,acc)
   ! Set the four ERI cutoffs together from a named accuracy level, and
   ! remember the level as the one the user asked for. The default is
   ! "low"; "very_low" is the primitive-pair cutoff of 1e-6 that was the
   ! default before 2026-09-16 and reproduces the older references.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: acc
   DIE_IF(NOT is_one_of_(acc,["very_low","low     ","medium  ","high    "]),"SCF_DATA:set_ERI_accuracy ... unknown eri_accuracy=, use very_low, low, medium or high")
      self%ERI_accuracy = acc
      call to_lower_case_(self%ERI_accuracy)
      self%ERI_accuracy_given = TRUE
      call apply_ERI_accuracy_(self,self%ERI_accuracy)
   end subroutine

   subroutine apply_ERI_accuracy(self,acc)
   ! Set the four ERI cutoffs from the named accuracy level "acc", without
   ! disturbing the level that was asked for. The primitive pair cutoff
   ! carries essentially all of the error and is the cheapest to tighten;
   ! the Schwarz and density cutoffs set the floor beneath it.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: acc
   ENSURE(is_one_of_(acc,["very_low","low     ","medium  ","high    "]),"SCF_DATA:apply_ERI_accuracy ... unknown ERI accuracy")
      select case (acc)

      case ("very_low")
         call set_ERI_primitive_pair_cutoff_(self,TOL(6))
         call set_ERI_Schwarz_cutoff_(self,TOL(9))
         call set_ERI_J_density_cutoff_(self,TOL(9))
         call set_ERI_K_density_cutoff_(self,TOL(9))

      case ("low")
         call set_ERI_primitive_pair_cutoff_(self,TOL(9))
         call set_ERI_Schwarz_cutoff_(self,TOL(9))
         call set_ERI_J_density_cutoff_(self,TOL(9))
         call set_ERI_K_density_cutoff_(self,TOL(9))

      case ("medium")
         call set_ERI_primitive_pair_cutoff_(self,TOL(12))
         call set_ERI_Schwarz_cutoff_(self,TOL(12))
         call set_ERI_J_density_cutoff_(self,TOL(12))
         call set_ERI_K_density_cutoff_(self,TOL(12))

      case ("high")
         call set_ERI_primitive_pair_cutoff_(self,TOL(15))
         call set_ERI_Schwarz_cutoff_(self,TOL(15))
         call set_ERI_J_density_cutoff_(self,TOL(15))
         call set_ERI_K_density_cutoff_(self,TOL(15))
      end select
   end subroutine

   PURE subroutine set_escalate_ERI_accuracy(self,val)
   ! Set whether the ERI cutoffs are loosened while the density is damped.
   ! Asking for this puts the named accuracy levels in charge of the cutoffs.
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%escalate_ERI_accuracy = val
      self%ERI_accuracy_given = TRUE
   end subroutine

   PURE function ERI_accuracy_rank(self,acc) result (res)
   ! Return the position of the named ERI accuracy level "acc" in the
   ! ladder, so that two levels can be compared.
      type(SCF_DATA_TYPE), IN :: self
      STR(len=*), IN :: acc
      INT :: res
   ENSURE(is_one_of_(acc,["very_low","low     ","medium  ","high    "]),"SCF_DATA:ERI_accuracy_rank ... unknown ERI accuracy")
      select case (acc)
      case ("very_low"); res = 1
      case ("low     "); res = 2
      case ("medium  "); res = 3
      case ("high    "); res = 4
      end select
   end function

   PURE function effective_ERI_accuracy(self) result (res)
   ! The accuracy level this iteration actually runs at: the level asked
   ! for, held down to "very_low" while the density is damped if
   ! escalation is on, and never below "medium" for an x-ray constrained
   ! SCF -- the fit is to structure factors, and loose integrals move them.
      type(SCF_DATA_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      res = self%ERI_accuracy
      if (self%escalate_ERI_accuracy AND apply_damping_(self)) res = "very_low"
      if (is_constrained_SCF_(self) AND ERI_accuracy_rank_(self,res)<ERI_accuracy_rank_(self,"medium  ")) res = "medium"
   end function

   subroutine set_ERI_accuracy_for_iteration(self)
   ! Set the ERI cutoffs for the iteration about to be done. This does
   ! nothing unless the named accuracy levels are in use: a job that sets
   ! neither eri_accuracy= nor escalate_eri_accuracy= keeps the cutoffs it
   ! has.
      type(SCF_DATA_TYPE), INOUT :: self
      if (NOT self%ERI_accuracy_given) return
      call apply_ERI_accuracy_(self,effective_ERI_accuracy_(self))
   end subroutine

   pure subroutine set_ERI_Schwarz_cutoff(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%ERI_Schwarz_cutoff = val
   end subroutine

   pure subroutine set_guess_output(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%guess_output = val
   end subroutine

   PURE subroutine set_initial_atomic_states(self,qmult)
   ! Set the elemental dispersion coefficients from a "table"
   ! of values comprising sequential element symbols, f' and f''.
      type(SCF_DATA_TYPE), INOUT :: self
      VEC(INT,:), IN :: qmult
      INT :: n,i,q,mult,offset
   ENSURE((size(qmult)/2)*2==size(qmult),"SCF_DATA:set_initial_atomic_states ... the must be an even number of charges/multiplicities")

      ! Size of array
      n = size(qmult)/2

      ! Create
      call destroy_(self%initial_atomic_states)
      call create_(self%initial_atomic_states,n,2)
      offset = 0
      do i = 1,n

         ! Get charge & multiplicity
         q = qmult(offset+1)
         mult = qmult(offset+2)

         ! Set the table
         self%initial_atomic_states(i,1) = q
         self%initial_atomic_states(i,2) = mult
         offset = offset+2
      end do
   end subroutine

   PURE subroutine set_initial_density(self,val)
   ! Set the initial density type to use for SCF guess
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%initial_density = val
      call to_lower_case_(self%initial_density)
      select case (self%initial_density)
      case ("core                ")  ! No reading
      case ("fock                ")  ! No reading
      case ("promolecule         ")  ! No reading
      case ("progroup            ")  ! No reading
      case ("r                   ")
      case ("restricted          "); self%initial_density = "r "
      case ("u                   ")
      case ("unrestricted        "); self%initial_density = "u "
      case ("gc                  ")
      case ("general_complex     "); self%initial_density = "gc"
      end select

      ! Need to reset set these
      self%restart_SCF = TRUE
      self%initial_MOs = "--using density--"
   end subroutine

   PURE subroutine set_initial_MOs(self,val)
   ! Read the initial MOs to use as SCF guess
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%initial_MOs = val
      call to_lower_case_(self%initial_MOs)
      select case (self%initial_MOs)
      case ("r                   ")
      case ("restricted          "); self%initial_MOs = "r "
      case ("u                   ")
      case ("unrestricted        "); self%initial_MOs = "u "
      case ("gc                  ")
      case ("general_complex     "); self%initial_MOs = "gc"
      end select

      ! Need to reset set these
      self%restart_SCF = TRUE
      self%initial_density = "--using MO's--"
   end subroutine

   pure subroutine set_initial_lambda(self,val)
   ! Set the initial lambda value for constrained SCF
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val
      self%initial_lambda = val
      self%lambda = val
   end subroutine

   PURE subroutine set_initial_Us(self,val)
   ! Set initial U's to use as CPHF guess
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%initial_Us = val
      call to_lower_case_(self%initial_Us)
      select case (self%initial_Us)
      case ("  ")
      case ("r ")
      end select
   end subroutine

   pure subroutine set_is_guess(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%is_guess = val
   end subroutine

   pure subroutine set_lambda_max(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%lambda_max = val
   end subroutine

   pure subroutine set_lambda_step(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%lambda_step = val
   end subroutine

   pure subroutine set_level_shift(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%level_shift = val
   end subroutine

   pure subroutine set_level_shift_finish(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%level_shift_finish = val
   end subroutine

   pure subroutine set_linear_dependence_shift(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%linear_dependence_shift = val
   end subroutine

   pure subroutine set_linear_dependence_tol(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%linear_dependence_tol = val
   end subroutine

   pure subroutine set_min_overlap_mx_eigenvalue(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%min_overlap_mx_eigenvalue = val
   end subroutine

   pure subroutine set_n_small_S_eigenvalues(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%n_small_S_eigenvalues = val
   end subroutine

   pure subroutine set_n_neg_S_eigenvalues(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%n_neg_S_eigenvalues = val
   end subroutine

   pure subroutine set_n_pert(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%n_pert = val
   end subroutine

   PURE subroutine set_nhf(self,val)
   ! Set what ... ?
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val
      self%nhf = val
   end subroutine

   PURE subroutine set_min_iterations(self,val)
   ! Set the minimum no. of SCF interations
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>=0,"SCF_DATA:set_min_iterations ... min_iteration must be non-negative")
   ENSURE(self%max_iterations>=val,"SCF_DATA:set_min_iterations ... must not exceed max_iterations!")
      self%min_iterations = val
   end subroutine

   PURE subroutine set_max_iterations(self,val)
   ! Set the maximum no. of SCF interations
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>=0,"SCF_DATA:set_max_iterations ... must be non-negative")
   ENSURE(val>=self%min_iterations,"SCF_DATA:set_max_iterations ... must be at least as large as min_iterations!")
      self%max_iterations = val
   end subroutine

   pure subroutine set_max_update_stepsize(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%max_update_stepsize = val
   end subroutine

   pure subroutine set_MO_gradient_stepsize(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%MO_gradient_stepsize = val
   end subroutine

   pure subroutine set_output(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%output = val
      self%output_results = val
   end subroutine

   pure subroutine set_output_results(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%output_results = val
   end subroutine

   pure subroutine set_show_timings(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%show_timings = val
   end subroutine

   pure subroutine set_quantization_axis(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      VEC(REAL,3), IN :: val

      ! The following code is inherited from OBJECT
      self%quantization_axis = val
   end subroutine

   PURE subroutine set_relativity_kind(self,val)
   ! Set the kind of relativistic hamiltonian: "none", "x2c" (scalar
   ! exact two-component), "pauli" or "zora".
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%relativity_kind = val
      call to_lower_case_(self%relativity_kind)
      select case (self%relativity_kind)
      case ("none")
      case ("x2c")
      case ("pauli")
      case ("zora")
      case default
         STOP_IF(TRUE,"SCF_DATA:set_relativity_kind ... relativity_kind= must be none, x2c, pauli or zora; for iotc or dkh use x2c")
      end select
      call set_pauli_terms_off_(self)
      if (self%relativity_kind=="pauli") call set_pauli_terms_on_(self)
   end subroutine

   pure subroutine set_restart_skip_asfs(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%restart_skip_asfs = val
   end subroutine

   pure subroutine set_save_cluster_charges(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%save_cluster_charges = val
   end subroutine

   pure subroutine set_save_20_cluster_charges(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%save_20_cluster_charges = val
   end subroutine

   PURE subroutine set_SCF_kind(self,val)
   ! Read the kind of SCF calculation
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%SCF_kind = val
      call to_lower_case_(self%SCF_kind)
      select case (self%SCF_kind)
      case ("gchf        ")  ! General complex Hartree-Fock
      case ("ghf         ")  ! General Hartree-Fock
      case ("group       ")  ! General Hartree-Fock
      case ("no_ghf      ")  ! Nono-orthogonal GHF
      case ("rchf        ")  ! Restricted complex Hartree-Fock
      case ("rhf         ")  ! Restricted Hartree-Fock
      case ("rks         ")  ! Restricted Kohn-Sham
      case ("rohf        ")  ! Restricted open shell Hartree-Fock
      case ("uchf        ")  ! Unrestricted complex Hartree-Fock
      case ("uhf         ")  ! Unrestricted Hartree-Fock
      case ("uks         ")  ! Unrestricted Kohn-Sham
      case ("unrestricted"); self%SCF_kind = "uhf"
      case ("restricted  "); self%SCF_kind = "rhf"
      case ("general     "); self%SCF_kind = "ghf"
      case ("xray_rhf    ")  ! X-ray-constrained restricted Hartree-Fock
      case ("xray_uhf    ")  ! X-ray-constrained unrestricted Hartree-Fock
      case ("xray_gchf   ")  ! X-ray-constrained unrestricted Hartree-Fock
      case ("xray_rks    ")  ! X-ray-constrained restricted Kohn-Sham
      case ("xray_uks    ")  ! X-ray-constrained unrestricted Kohn-Sham
   ! case("pnd_uhf     ") ! polarised neutron diffraction uhf
   ! case("pnd_uks     ") ! polarised neutron diffraction uks
   ! case("xray_pnd_uhf") ! joint xray, pnd uhf
   ! case("xray_pnd_uks") ! joint xray, pnd uks
      case ("fragment-rhf ")  ! 
      case ("fragment-rks ")  ! 
      end select
   end subroutine

   pure subroutine set_stabilize_density(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%stabilize_density = val
   end subroutine

   pure subroutine set_test(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%test = val
   end subroutine

   subroutine set_temperature(self,val)
   ! Set the electronic temperature
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>=ZERO,"SCF_DATA:set_temperature ... must set non-negative temperature in Hartree")
      if (val==ZERO) then
         self%temperature = ZERO
         self%using_FON = FALSE
      else
         self%temperature = val
         self%using_FON = TRUE
      end if
   end subroutine

   pure subroutine set_use_ELMO(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_ELMO = val
   end subroutine

   pure subroutine set_use_NOMO(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_NOMO = val
   end subroutine

   pure subroutine set_using_1e_sl_term(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_1e_sl_term = val
   end subroutine

   pure subroutine set_using_1e_srxa_term(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_1e_srxa_term = val
   end subroutine

   pure subroutine set_using_1e_zora_term(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_1e_zora_term = val
   end subroutine

   pure subroutine set_using_2e_sl_term(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_2e_sl_term = val
   end subroutine

   pure subroutine set_using_aa_term(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_aa_term = val
   end subroutine

   pure subroutine set_using_bl_term(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_bl_term = val
   end subroutine

   pure subroutine set_using_bs_term(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_bs_term = val
   end subroutine

   pure subroutine set_using_bs_t_term(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_bs_t_term = val
   end subroutine

   pure subroutine set_using_CP_DIIS(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_CP_DIIS = val
   end subroutine

   pure subroutine set_using_cluster_charges(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_cluster_charges = val
   end subroutine

   pure subroutine set_using_damping(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_damping = val
   end subroutine

   pure subroutine set_use_default_guess_options(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_default_guess_options = val
   end subroutine

   pure subroutine set_using_delta_build(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_delta_build = val
   end subroutine

   pure subroutine set_using_batched_XC(self,val)
   ! Set .using_batched_XC
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_batched_XC = val
   end subroutine

   pure subroutine set_using_gaussian_pair_J(self,val)
   ! Set .using_gaussian_pair_J
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_gaussian_pair_J = val
   end subroutine

   pure subroutine set_using_RI_J(self,val)
   ! Set .using_RI_J
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_RI_J = val
   end subroutine

   pure subroutine set_using_COSX(self,val)
   ! Set .using_COSX
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_COSX = val
   end subroutine

   pure subroutine set_using_COSX_final_grid(self,val)
   ! Set .using_COSX_final_grid
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_COSX_final_grid = val
   end subroutine

   subroutine make_COSX_grids(self)
   ! Make whichever of .COSX_grid and .COSX_final_grid was not read,
   ! with its default: accuracy very_low for the iterations, high for
   ! the energy of the converged SCF.
      type(SCF_DATA_TYPE), INOUT :: self
      if (NOT allocated(self%COSX_grid)) then
         call create_(self%COSX_grid)
         call set_accuracy_(self%COSX_grid,"very_low")
      end if
      if (NOT allocated(self%COSX_final_grid)) then
         call create_(self%COSX_final_grid)
         call set_accuracy_(self%COSX_final_grid,"high")
      end if
   end subroutine

   pure subroutine set_using_COSX_overlap_fitting(self,val)
   ! Set .using_COSX_overlap_fitting
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_COSX_overlap_fitting = val
   end subroutine

   pure subroutine set_delta_build_period(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%delta_build_period = val
   end subroutine

   pure subroutine set_using_direct_SCF(self,val)
   ! Set whether to use direct SCF or not
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%using_direct_SCF = val
   !  if (.using_direct_SCF) .using_delta_build = TRUE ! removed 2026-09-12: the
   !  delta build is opt-in; see types.foo
   end subroutine

   pure subroutine set_using_DK1(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%DK1 = val
   end subroutine

   pure subroutine set_using_DK2(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%DK2 = val
   end subroutine

   pure subroutine set_using_DK3(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%DK3 = val
   end subroutine

   pure subroutine set_using_DK1_PCE(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%DK1_PCE = val
   end subroutine

   pure subroutine set_using_DK2_PCE(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%DK2_PCE = val
   end subroutine

   pure subroutine set_using_exponential_update(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_exponential_update = val
   end subroutine

   pure subroutine set_using_DIIS(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_DIIS = val
   end subroutine

   pure subroutine set_using_level_shift(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_level_shift = val
   end subroutine

   pure subroutine set_using_MO_gradient_update(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_MO_gradient_update = val
   end subroutine

   pure subroutine set_using_NDDO(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_NDDO = val
   end subroutine

   pure subroutine set_using_NUDO(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_NUDO = val
   end subroutine

   pure subroutine set_using_orbital_locking(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_orbital_locking = val
   end subroutine

   pure subroutine set_using_PCE_correction(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%PCE_CORR = val
   end subroutine

   pure subroutine set_using_PIE(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_PIE = val
   end subroutine

   pure subroutine set_using_ppzpp_method(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_ppzpp = val
   end subroutine

   pure subroutine set_die_if_not_converged(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%die_if_not_converged = val
   end subroutine

   pure subroutine set_converged(self,val)
   ! Record whether the SCF loop ended converged
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%converged = val
   end subroutine

   pure subroutine set_using_pFON_damping(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_pFON = val
   end subroutine

   pure subroutine set_pFON_start_temperature(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%pFON_start_temperature = val
   end subroutine

   pure subroutine set_pFON_cooling_rate(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%pFON_cooling_rate = val
   end subroutine

   pure subroutine set_pFON_finish(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%pFON_finish = val
   end subroutine

   PURE subroutine set_ANO_kind(self,val)
   ! Set how an open-shell ANO atom is made spherical: average, aoc or fon.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%ANO_kind = val
      call to_lower_case_(self%ANO_kind)
      ENSURE(is_one_of_(self%ANO_kind,["average","aoc    ","fon    "]),"SCF_DATA:set_ANO_kind ... ano_kind= must be average, aoc or fon, not "//trim(val))
   end subroutine

   pure subroutine set_open_shell_ensemble(self,n_orbitals,n_electrons,coupling)
   ! Make this SCF an open-shell ensemble: "n_electrons" held at equal
   ! fractional occupation in "n_orbitals", with the average-of-
   ! configuration "coupling" a.
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: n_orbitals,n_electrons
      REAL, IN :: coupling
      self%using_open_shell_ensemble = TRUE
      self%n_open_orbitals = n_orbitals
      self%n_open_electrons = n_electrons
      self%AOC_coupling = coupling
      self%ensemble_energy_correction = ZERO
   end subroutine

   pure subroutine set_using_qq_cluster_charges(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_qq_cluster_charges = val
   end subroutine

   pure subroutine set_using_SC_cluster_charges(self,val)
   ! Set whether to use SC cluster charges
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%using_SC_cluster_charges = val
      if (val) self%using_cluster_charges = val
   end subroutine

   pure subroutine set_using_SC_Lorentz_fields(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_SC_Lorentz_fields = val
   end subroutine

   pure subroutine set_using_SO_FDFF_method(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%using_SO_FDFF = val
   end subroutine

   pure subroutine set_VMO_canonisation(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val

      ! The following code is inherited from OBJECT
      self%VMO_canonisation = val
   end subroutine

   pure subroutine set_VMO_canonisation_MO(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%VMO_canonisation_MO = val
   end subroutine

   PURE subroutine set_XC_atom_separation_cutoff(self,val)
   ! Set the XC atom separation cutoff
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>ZERO,"SCF_DATA:set_XC_atom_separation_cutoff ... cutoff must be positive")
      self%XC_atom_separation_cutoff = val
   end subroutine

   pure subroutine set_put_constraint_stiffness(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%put_constraint_stiffness = val
   end subroutine

   pure subroutine set_use_stiffness_correction(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_stiffness_correction = val
   end subroutine

   pure subroutine set_use_matrix_free_stiffness(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_matrix_free_stiffness = val
   end subroutine

   PURE subroutine set_stiffness_cg_tolerance(self,val)
   ! Set the relative residual at which the conjugate gradient solve of
   ! the matrix-free correction stops.
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>ZERO,"SCF_DATA:set_stiffness_cg_tolerance ... stiffness_cg_tolerance must be positive")
      self%stiffness_cg_tolerance = val
   end subroutine

   PURE subroutine set_stiffness_preconditioner(self,val)
   ! Set the preconditioner of the conjugate gradient solve in the
   ! matrix-free correction: "none" (the default) or "diagonal", the
   ! exact diagonal of the gain matrix made once per lambda. On ammonia
   ! the diagonal doubles the iteration count: the gain matrix is far
   ! from diagonal. Kept for other cases.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
   ENSURE(is_one_of_(val,["none    ","diagonal"]),"SCF_DATA:set_stiffness_preconditioner ... unknown stiffness preconditioner")
      self%stiffness_preconditioner = val
   end subroutine

   PURE subroutine set_stiffness_deflation(self,val)
   ! Set how many leading eigenvectors of the gain matrix deflate the
   ! conjugate gradient solve of the matrix-free correction; 0 is none.
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>=0,"SCF_DATA:set_stiffness_deflation ... stiffness_deflation must not be negative")
      self%stiffness_deflation = val
   end subroutine

   PURE subroutine set_stiffness_samples(self,val)
   ! Set the number of sign vectors in the trace estimator for p_eff.
      type(SCF_DATA_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>0,"SCF_DATA:set_stiffness_samples ... stiffness_samples must be positive")
      self%stiffness_samples = val
   end subroutine

   PURE subroutine set_lambda_criterion(self,val)
   ! Set the criterion that chooses lambda, one of "loo" (the
   ! leave-one-out sum, complete cross-validation, the default), "gcv"
   ! (generalised cross-validation), "aic_sigma" (the AIC with the variance
   ! scale fitted), "aic" or "bic". The minimum of the first three does
   ! not move with a common scale error in the sigmas.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
   ENSURE(is_one_of_(val,["gcv      ","aic_sigma","aic      ","bic      ","loo      "]),"SCF_DATA:set_lambda_criterion ... unknown lambda criterion")
      self%lambda_criterion = val
   end subroutine

   pure function criterion_value(self) result (res)
   ! The value of the chosen lambda criterion from the stiffness
   ! statistics, which must have been made.
      type(SCF_DATA_TYPE), IN :: self
      REAL :: res
      select case (self%lambda_criterion)
      case ("gcv      "); res = self%GCV
      case ("aic_sigma"); res = self%AIC_sigma
      case ("aic      "); res = self%chi2_fit+TWO*self%p_eff
      case ("bic      "); res = self%chi2_fit+self%p_eff*log(real(size(self%stiffness_eval),kind=REAL_KIND))
      case ("loo      "); res = self%LOO_sum
      case default; res = self%LOO_sum
      end select
   end function

   subroutine record_lambda_point(self,GoF2)
   ! Record the converged "GoF2" at the current lambda, and fit the
   ! last three points with lambda > 0 to the asymptotic form of
   ! Davidson, Grabowsky and Jayatilaka (2022), Acta Cryst. B78, 397,
   !    GoF^2(lambda) = D + E/lambda^2 + F/lambda^4
   ! whose halting value is lambda_TIH = |F/D|^(1/4). Three points
   ! determine D, E and F exactly. Also record the chosen lambda
   ! criterion and keep the lambda where it is smallest.
      type(SCF_DATA_TYPE), INOUT :: self
      REAL, IN :: GoF2
      MAT(REAL,3,3) :: A,Ai
      VEC(REAL,3) :: x,y,c
      REAL :: val
      INT :: i,n,m
      call append_(self%lambda_history,self%lambda)
      call append_(self%GoF2_history,GoF2)

      ! The chosen criterion, and its minimum so far
      if (self%stiffness_made) then
         val = criterion_value_(self)
         call append_(self%criterion_history,val)
         if (size(self%criterion_history)==1 OR val<self%criterion_min) then
            self%criterion_min = val
            self%lambda_opt = self%lambda
         end if
      end if

      ! The last three points with lambda > 0
      n = size(self%lambda_history)
      m = 0
      do i = n,1,-1
         if (self%lambda_history(i)<=ZERO) cycle
         m = m+1
         x(m) = ONE/self%lambda_history(i)**2
         y(m) = self%GoF2_history(i)
         if (m==3) exit
      end do
      if (m<3) return

      ! Solve for D, E, F
      do i = 1,3
         A(i,1) = ONE
         A(i,2) = x(i)
         A(i,3) = x(i)*x(i)
      end do
      call to_inverse_of_(Ai,A)
      call to_product_of_(c,Ai,y)
      self%TIH3_D = c(1)
      self%TIH3_E = c(2)
      self%TIH3_F = c(3)
      self%TIH3_made = TRUE
      self%lambda_TIH3 = ZERO
      if (abs(c(1))>ZERO) self%lambda_TIH3 = sqrt(sqrt(abs(c(3)/c(1))))
   end subroutine

   pure subroutine set_zero_oo_constraint_block(self,val)
   ! Generic set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%zero_oo_constraint_block = val
   end subroutine


!  =============
!  Input methods
!  =============

   ! NOTE : this must be finalized after the call.

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","SCF_DATA:read_keywords ... expecting open bracket symbol, {")

      ! Read the open braces {

      ! The following code is inherited from OBJECT
      call read_(stdin,word)

      ! Echo keyword if requested
      if (tonto%keyword_echo) then
         call flush_(stdout)
         call text_(stdout,"keyword found --> "//trim(word))
      end if
      do  ! Loop over input keywords

         ! Read the keyword
         call read_(stdin,word)
         DIE_IF(reverted_(stdin),"SCF_DATA:read_keywords ... unexpected end of file while reading list keywords")
         call to_lower_case_(word)

         ! Append any lonely = sign
         if (NOT exhausted_(stdin%buffer)) then
            if (next_item_(stdin)=="=") then
               call skip_next_item_(stdin)
               word = trim(word)//"="
            end if
         end if

         ! Echo keyword if requested
         if (tonto%keyword_echo) then
            call flush_(stdout)
            call text_(stdout,"keyword found --> "//trim(word))
         end if

         ! Exit if keyword is closing brace }
         if (word=="}") exit

         ! Process the keyword 
         call process_keyword_(self,word)
      end do
   end subroutine

   subroutine process_keyword(self,keyword)
   ! Process a command "keyword". Data is inputted from "stdin", unless
   ! "word" is a sequence of blank separated strings. In this case,
   ! the sequence is processed as if it were a separate file.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("1e_sl_factor=                "); call read_1e_sl_factor_(self)
      case ("2e_sl_factor=                "); call read_2e_sl_factor_(self)
      case ("}                            ")  ! exit case

      case ("atom_overlap_cutoff=         "); call read_atom_overlap_cutoff_(self)

      case ("cluster_radius=              "); call read_cluster_radius_(self)
      case ("convergence=                 "); call read_convergence_(self)
      case ("converge=                    "); call read_convergence_(self)
      case ("guess_convergence=           "); call read_guess_convergence_(self)
      case ("cosx_final_grid=             "); call read_COSX_final_grid_(self)
      case ("cosx_grid=                   "); call read_COSX_grid_(self)

      case ("damp_factor=                 "); call read_damp_factor_(self)
      case ("damp_finish=                 "); call read_damp_finish_(self)
      case ("defragment=                  "); call read_defragment_(self)  ! testing, turn off defragment in case of network compounds, Lorraine. 
      case ("delete_all_archives=         "); call read_delete_all_archives_(self)
      case ("delete_scf_archives=         "); call read_delete_SCF_archives_(self)
      case ("dft_correlation_functional=  "); call read_correlation_functional_(self)
      case ("dft_exchange_functional=     "); call read_exchange_functional_(self)
      case ("dft_functional=              "); call read_functional_(self)
      case ("diis=                        "); call read_DIIS_(self)
      case ("diis_error_temp_cutoff=      "); call read_DIIS_error_temp_cutoff_(self)
      case ("direct=                      "); call read_using_direct_SCF_(self)  ! deprecated
      case ("do_outputs=                  "); call read_do_outputs_(self)

      case ("ee_adiabatic_constant=       "); call read_ee_adiabatic_constant_(self)
      case ("elmo_b=                      "); call read_ELMO_B_(self)
      case ("elmo_p=                      "); call read_ELMO_P_(self)
      case ("eri_accuracy=                "); call read_ERI_accuracy_(self)
      case ("eri_bf_overlap_cutoff=       "); call read_ERI_bf_overlap_cutoff_(self)
      case ("eri_disk_cutoff=             "); call read_ERI_disk_cutoff_(self)
      case ("eri_j_density_cutoff=        "); call read_ERI_J_density_cutoff_(self)
      case ("eri_k_density_cutoff=        "); call read_ERI_K_density_cutoff_(self)
      case ("eri_primitive_pair_cutoff=   "); call read_ERI_primitive_pair_cutoff_(self)
      case ("eri_schwarz_cutoff=          "); call read_ERI_Schwarz_cutoff_(self)
      case ("escalate_eri_accuracy=       "); call read_escalate_ERI_accuracy_(self)

      case ("guess_output=                "); call read_guess_output_(self)

      case ("initial_atomic_states=       "); call read_initial_atomic_states_(self)
      case ("initial_density=             "); call read_initial_density_(self)
      case ("initial_lambda=              "); call read_initial_lambda_(self)
      case ("initial_mos=                 "); call read_initial_MOs_(self)
      case ("initial_us=                  "); call read_initial_Us_(self)

      case ("kind=                        "); call read_SCF_kind_(self)  !  deprecated

      case ("lambda_max=                  "); call read_lambda_max_(self)
      case ("lambda_step=                 "); call read_lambda_step_(self)
      case ("level_shift=                 "); call read_level_shift_(self)
      case ("level_shift_finish=          "); call read_level_shift_finish_(self)
      case ("linear_dependence_shift=     "); call read_linear_dependence_shift_(self)
      case ("linear_dependence_tol=       "); call read_linear_dependence_tol_(self)

      case ("min_iterations=              "); call read_min_iterations_(self)
      case ("max_iterations=              "); call read_max_iterations_(self)
      case ("max_update_stepsize=         "); call read_max_update_stepsize_(self)
      case ("mo_gradient_stepsize=        "); call read_MO_gradient_stepsize_(self)

      case ("nhf=                         "); call read_nhf_(self)

      case ("output=                      "); call read_output_(self)
      case ("output_results=              "); call read_output_results_(self)
      case ("show_timings=                "); call read_show_timings_(self)

      case ("put_cluster                  "); call put_cluster_(self)
      case ("put_cluster_charges          "); call put_cluster_charges_(self,"cluster_charges")

      case ("quantization_axis=           "); call read_quantization_axis_(self)

      case ("relativity_kind=             "); call read_relativity_kind_(self)
      case ("restart_skip_asfs=           "); call read_restart_skip_ASFs_(self)

      case ("save_cluster_charges=        "); call read_save_cluster_charges_(self)
      case ("save_20_cluster_charges=     "); call read_save_20_cluster_charges_(self)
      case ("scf_kind=                    "); call read_SCF_kind_(self)
      case ("scf_type=                    "); call read_SCF_kind_(self)  !  deprecated
      case ("stabilize_density=           "); call read_stabilize_density_(self)

      case ("temperature=                 "); call read_temperature_(self)
      case ("test=                        "); call read_test_(self)

      case ("use_1e_sl_term=              "); call read_using_1e_sl_term_(self)
      case ("use_1e_s(rxa)_term=          "); call read_using_1e_srxa_term_(self)
      case ("use_1e_zora_term=            "); call read_using_1e_zora_term_(self)
      case ("use_2e_sl_term=              "); call read_using_2e_sl_term_(self)
      case ("use_aa_term=                 "); call read_using_aa_term_(self)
      case ("use_bfgs=                    "); call read_use_BFGS_(self)
      case ("use_bl_term=                 "); call read_using_bl_term_(self)
      case ("use_bs_term=                 "); call read_using_bs_term_(self)
      case ("use_bs_t_term=               "); call read_using_bs_t_term_(self)
      case ("use_cp_DIIS=                 "); call read_using_CP_DIIS_(self)
      case ("use_cluster_charges=         "); call read_using_cluster_charges_(self)
      case ("use_damping=                 "); call read_using_damping_(self)
      case ("use_default_guess_options=   "); call read_use_default_guess_options_(self)
      case ("use_delta_build=             "); call read_using_delta_build_(self)
      case ("use_batched_xc=              "); call read_using_batched_XC_(self)
      case ("use_gaussian_pair_j=         "); call read_using_gaussian_pair_J_(self)
      case ("use_cosx=                    "); call read_using_COSX_(self)
      case ("use_cosx_final_grid=         "); call read_using_COSX_final_grid_(self)
      case ("use_cosx_overlap_fitting=    "); call read_using_COSX_overlap_fitting_(self)
      case ("use_ri_j=                    "); call read_using_RI_J_(self)
      case ("delta_build_period=          "); call read_delta_build_period_(self)
      case ("use_direct_scf=              "); call read_using_direct_SCF_(self)
      case ("use_diis=                    "); call read_using_DIIS_(self)
      case ("use_dk1=                     "); call read_using_DK1_(self)
      case ("use_dk2=                     "); call read_using_DK2_(self)
      case ("use_dk3=                     "); call read_using_DK3_(self)
      case ("use_dk1_pce=                 "); call read_using_DK1_PCE_(self)
      case ("use_dk2_pce=                 "); call read_using_DK2_PCE_(self)
      case ("use_elmo=                    "); call read_use_ELMO_(self)
      case ("use_elmos=                   "); call read_use_ELMO_(self)
      case ("use_elmo_01=                 "); call read_use_ELMO_01_(self)
      case ("use_elmo_11=                 "); call read_use_ELMO_11_(self)
      case ("use_exponential_update=      "); call read_using_exponential_update_(self)
      case ("use_fermi_amaldi=            "); call read_using_Fermi_Amaldi_(self)
      case ("use_level_shifting=          "); call read_using_level_shift_(self)
      case ("use_level_shift=             "); call read_using_level_shift_(self)
      case ("use_lsq=                     "); call read_use_LSQ_(self)
      case ("use_mo_gradient_update=      "); call read_using_MO_gradient_update_(self)
      case ("use_nddo=                    "); call read_using_NDDO_(self)
      case ("use_nomo=                    "); call read_use_NOMO_(self)
      case ("use_nudo=                    "); call read_using_NUDO_(self)
      case ("use_orbital_locking=         "); call read_using_orbital_locking_(self)
      case ("use_pce_corr=                "); call read_using_PCE_correction_(self)
      case ("use_pie=                     "); call read_using_PIE_(self)
      case ("use_ppzpp=                   "); call read_using_ppzpp_method_(self)
      case ("die_if_not_converged=        "); call read_die_if_not_converged_(self)
      case ("use_pfon_damping=            "); call read_using_pFON_damping_(self)
      case ("use_pfon=                    "); call read_using_pFON_damping_(self)
      case ("ano_kind=                    "); call read_ANO_kind_(self)
      case ("pfon_start_temperature=      "); call read_pFON_start_temperature_(self)
      case ("pfon_cooling_rate=           "); call read_pFON_cooling_rate_(self)
      case ("pfon_finish=                 "); call read_pFON_finish_(self)
      case ("use_qq_cluster_charges=      "); call read_using_qq_cluster_charges_(self)
      case ("use_sc_cluster_charges=      "); call read_using_SC_cluster_charges_(self)
      case ("use_sc_lorentz_fields=       "); call read_using_SC_Lorentz_fields_(self)
      case ("use_so_fdff=                 "); call read_using_SO_FDFF_method_(self)

      case ("vmo_canonisation=            "); call read_VMO_canonisation_(self)
      case ("vmo_canonisation_mo=         "); call read_VMO_canonisation_MO_(self)

      case ("xc_atom_separation_cutoff=   "); call read_XC_atom_separation_cutoff_(self)

      case ("lambda_criterion=            "); call read_lambda_criterion_(self)
      case ("put_constraint_stiffness=    "); call read_put_constraint_stiffness_(self)
      case ("stiffness_cg_tolerance=      "); call read_stiffness_cg_tolerance_(self)
      case ("stiffness_deflation=         "); call read_stiffness_deflation_(self)
      case ("stiffness_preconditioner=    "); call read_stiffness_preconditioner_(self)
      case ("stiffness_samples=           "); call read_stiffness_samples_(self)
      case ("use_matrix_free_stiffness=   "); call read_use_matrix_free_stiffness_(self)
      case ("use_stiffness_correction=    "); call read_use_stiffness_correction_(self)

      case ("zero_oo_constraint_block=    "); call read_zero_oo_constraint_block_(self)

      case default; allocate(tonto%known_keywords(134))
      tonto%known_keywords(1) = "1e_sl_factor=                "
      tonto%known_keywords(2) = "2e_sl_factor=                "
      tonto%known_keywords(3) = "}                            "
      tonto%known_keywords(4) = "atom_overlap_cutoff=         "
      tonto%known_keywords(5) = "cluster_radius=              "
      tonto%known_keywords(6) = "convergence=                 "
      tonto%known_keywords(7) = "converge=                    "
      tonto%known_keywords(8) = "guess_convergence=           "
      tonto%known_keywords(9) = "cosx_final_grid=             "
      tonto%known_keywords(10) = "cosx_grid=                   "
      tonto%known_keywords(11) = "damp_factor=                 "
      tonto%known_keywords(12) = "damp_finish=                 "
      tonto%known_keywords(13) = "defragment=                  "
      tonto%known_keywords(14) = "delete_all_archives=         "
      tonto%known_keywords(15) = "delete_scf_archives=         "
      tonto%known_keywords(16) = "dft_correlation_functional=  "
      tonto%known_keywords(17) = "dft_exchange_functional=     "
      tonto%known_keywords(18) = "dft_functional=              "
      tonto%known_keywords(19) = "diis=                        "
      tonto%known_keywords(20) = "diis_error_temp_cutoff=      "
      tonto%known_keywords(21) = "direct=                      "
      tonto%known_keywords(22) = "do_outputs=                  "
      tonto%known_keywords(23) = "ee_adiabatic_constant=       "
      tonto%known_keywords(24) = "elmo_b=                      "
      tonto%known_keywords(25) = "elmo_p=                      "
      tonto%known_keywords(26) = "eri_accuracy=                "
      tonto%known_keywords(27) = "eri_bf_overlap_cutoff=       "
      tonto%known_keywords(28) = "eri_disk_cutoff=             "
      tonto%known_keywords(29) = "eri_j_density_cutoff=        "
      tonto%known_keywords(30) = "eri_k_density_cutoff=        "
      tonto%known_keywords(31) = "eri_primitive_pair_cutoff=   "
      tonto%known_keywords(32) = "eri_schwarz_cutoff=          "
      tonto%known_keywords(33) = "escalate_eri_accuracy=       "
      tonto%known_keywords(34) = "guess_output=                "
      tonto%known_keywords(35) = "initial_atomic_states=       "
      tonto%known_keywords(36) = "initial_density=             "
      tonto%known_keywords(37) = "initial_lambda=              "
      tonto%known_keywords(38) = "initial_mos=                 "
      tonto%known_keywords(39) = "initial_us=                  "
      tonto%known_keywords(40) = "kind=                        "
      tonto%known_keywords(41) = "lambda_max=                  "
      tonto%known_keywords(42) = "lambda_step=                 "
      tonto%known_keywords(43) = "level_shift=                 "
      tonto%known_keywords(44) = "level_shift_finish=          "
      tonto%known_keywords(45) = "linear_dependence_shift=     "
      tonto%known_keywords(46) = "linear_dependence_tol=       "
      tonto%known_keywords(47) = "min_iterations=              "
      tonto%known_keywords(48) = "max_iterations=              "
      tonto%known_keywords(49) = "max_update_stepsize=         "
      tonto%known_keywords(50) = "mo_gradient_stepsize=        "
      tonto%known_keywords(51) = "nhf=                         "
      tonto%known_keywords(52) = "output=                      "
      tonto%known_keywords(53) = "output_results=              "
      tonto%known_keywords(54) = "show_timings=                "
      tonto%known_keywords(55) = "put_cluster                  "
      tonto%known_keywords(56) = "put_cluster_charges          "
      tonto%known_keywords(57) = "quantization_axis=           "
      tonto%known_keywords(58) = "relativity_kind=             "
      tonto%known_keywords(59) = "restart_skip_asfs=           "
      tonto%known_keywords(60) = "save_cluster_charges=        "
      tonto%known_keywords(61) = "save_20_cluster_charges=     "
      tonto%known_keywords(62) = "scf_kind=                    "
      tonto%known_keywords(63) = "scf_type=                    "
      tonto%known_keywords(64) = "stabilize_density=           "
      tonto%known_keywords(65) = "temperature=                 "
      tonto%known_keywords(66) = "test=                        "
      tonto%known_keywords(67) = "use_1e_sl_term=              "
      tonto%known_keywords(68) = "use_1e_s(rxa)_term=          "
      tonto%known_keywords(69) = "use_1e_zora_term=            "
      tonto%known_keywords(70) = "use_2e_sl_term=              "
      tonto%known_keywords(71) = "use_aa_term=                 "
      tonto%known_keywords(72) = "use_bfgs=                    "
      tonto%known_keywords(73) = "use_bl_term=                 "
      tonto%known_keywords(74) = "use_bs_term=                 "
      tonto%known_keywords(75) = "use_bs_t_term=               "
      tonto%known_keywords(76) = "use_cp_DIIS=                 "
      tonto%known_keywords(77) = "use_cluster_charges=         "
      tonto%known_keywords(78) = "use_damping=                 "
      tonto%known_keywords(79) = "use_default_guess_options=   "
      tonto%known_keywords(80) = "use_delta_build=             "
      tonto%known_keywords(81) = "use_batched_xc=              "
      tonto%known_keywords(82) = "use_gaussian_pair_j=         "
      tonto%known_keywords(83) = "use_cosx=                    "
      tonto%known_keywords(84) = "use_cosx_final_grid=         "
      tonto%known_keywords(85) = "use_cosx_overlap_fitting=    "
      tonto%known_keywords(86) = "use_ri_j=                    "
      tonto%known_keywords(87) = "delta_build_period=          "
      tonto%known_keywords(88) = "use_direct_scf=              "
      tonto%known_keywords(89) = "use_diis=                    "
      tonto%known_keywords(90) = "use_dk1=                     "
      tonto%known_keywords(91) = "use_dk2=                     "
      tonto%known_keywords(92) = "use_dk3=                     "
      tonto%known_keywords(93) = "use_dk1_pce=                 "
      tonto%known_keywords(94) = "use_dk2_pce=                 "
      tonto%known_keywords(95) = "use_elmo=                    "
      tonto%known_keywords(96) = "use_elmos=                   "
      tonto%known_keywords(97) = "use_elmo_01=                 "
      tonto%known_keywords(98) = "use_elmo_11=                 "
      tonto%known_keywords(99) = "use_exponential_update=      "
      tonto%known_keywords(100) = "use_fermi_amaldi=            "
      tonto%known_keywords(101) = "use_level_shifting=          "
      tonto%known_keywords(102) = "use_level_shift=             "
      tonto%known_keywords(103) = "use_lsq=                     "
      tonto%known_keywords(104) = "use_mo_gradient_update=      "
      tonto%known_keywords(105) = "use_nddo=                    "
      tonto%known_keywords(106) = "use_nomo=                    "
      tonto%known_keywords(107) = "use_nudo=                    "
      tonto%known_keywords(108) = "use_orbital_locking=         "
      tonto%known_keywords(109) = "use_pce_corr=                "
      tonto%known_keywords(110) = "use_pie=                     "
      tonto%known_keywords(111) = "use_ppzpp=                   "
      tonto%known_keywords(112) = "die_if_not_converged=        "
      tonto%known_keywords(113) = "use_pfon_damping=            "
      tonto%known_keywords(114) = "use_pfon=                    "
      tonto%known_keywords(115) = "ano_kind=                    "
      tonto%known_keywords(116) = "pfon_start_temperature=      "
      tonto%known_keywords(117) = "pfon_cooling_rate=           "
      tonto%known_keywords(118) = "pfon_finish=                 "
      tonto%known_keywords(119) = "use_qq_cluster_charges=      "
      tonto%known_keywords(120) = "use_sc_cluster_charges=      "
      tonto%known_keywords(121) = "use_sc_lorentz_fields=       "
      tonto%known_keywords(122) = "use_so_fdff=                 "
      tonto%known_keywords(123) = "vmo_canonisation=            "
      tonto%known_keywords(124) = "vmo_canonisation_mo=         "
      tonto%known_keywords(125) = "xc_atom_separation_cutoff=   "
      tonto%known_keywords(126) = "lambda_criterion=            "
      tonto%known_keywords(127) = "put_constraint_stiffness=    "
      tonto%known_keywords(128) = "stiffness_cg_tolerance=      "
      tonto%known_keywords(129) = "stiffness_deflation=         "
      tonto%known_keywords(130) = "stiffness_preconditioner=    "
      tonto%known_keywords(131) = "stiffness_samples=           "
      tonto%known_keywords(132) = "use_matrix_free_stiffness=   "
      tonto%known_keywords(133) = "use_stiffness_correction=    "
      tonto%known_keywords(134) = "zero_oo_constraint_block=    "
      call unknown_(tonto,word,"SCF_DATA:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   PURE subroutine update(self)
   ! Make sure the input satisfies sanity checks and generate
   ! any other missing data
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val
   ENSURE(self%SCF_kind/=" ","SCF_DATA:update ... no scf kind specified")
   ENSURE(self%max_iterations>=self%min_iterations,"SCF_DATA:update ... max_it must be greater than min_it!")
   ENSURE(self%SCF_DIIS%keep>0,"SCF_DATA:update ... DIIS.keep must be > 0")
   ENSURE(self%SCF_DIIS%start_iteration>0,"SCF_DATA:update ... DIIS.start_iteration must be > 0")
   ENSURE(self%SCF_DIIS%save_iteration>0,"SCF_DATA:update ... DIIS.save_iteration must be > 0")
      if (self%using_1e_zora_term) self%using_1e_sl_term = FALSE
      if (self%using_1e_sl_term) self%using_1e_zora_term = FALSE
      if (self%using_SC_cluster_charges OR self%using_cluster_charges) then
         val = is_constrained_wavefunction_(self) AND NOT self%using_SC_cluster_charges
         ENSURE(NOT val,"SCF_DATA:update ... must use SC_cluster_charges!")
         val = self%initial_density=="core" OR self%initial_density=="promolecule"
         ENSURE(NOT val,"SCF_DATA:update ... must use previous densityor MOs for initial guess")
         if (self%using_SC_cluster_charges) then
            self%using_cluster_charges = TRUE
         end if
      end if
   end subroutine


!  ====
!  Read
!  ====

   subroutine read_1e_sl_factor(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_1e_sl_factor_(self,val)
   end subroutine

   subroutine read_2e_sl_factor(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_2e_sl_factor_(self,val)
   end subroutine

   subroutine read_atom_overlap_cutoff(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_atom_overlap_cutoff_(self,val)
   end subroutine

   subroutine read_cluster_radius(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_cluster_radius_(self,val)
   end subroutine

   subroutine read_convergence(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_convergence_(self,val)
   end subroutine

   subroutine read_guess_convergence(self)
   ! Read the guess-SCF energy convergence criterion.
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_guess_convergence_(self,val)
   end subroutine

   subroutine read_correlation_functional(self)
   ! Read the DFT correlation functional. It may not be given together
   ! with dft_functional=, which names the whole functional.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      DIE_IF(self%DFT_functional/=" ","SCF_DATA:read_correlation_functional ... dft_functional= was given: do not also give dft_correlation_functional=")
      call set_correlation_functional_(self,val)
      self%DFT_parts_given = TRUE
   end subroutine

   subroutine read_functional(self)
   ! Read the whole DFT functional by one name; see set_functional. It may
   ! not be given together with dft_exchange_functional= or
   ! dft_correlation_functional=.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      DIE_IF(self%DFT_parts_given,"SCF_DATA:read_functional ... dft_exchange_functional= or dft_correlation_functional= was given: do not also give dft_functional=")
      call set_functional_(self,val)
   end subroutine

   subroutine read_damp_factor(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_damp_factor_(self,val)
   end subroutine

   subroutine read_damp_finish(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_damp_finish_(self,val)
   end subroutine

   subroutine read_defragment(self)
   ! Read whether to defragment the cluster at the boundaries. If set TRUE, the
   ! cluster ends are defragmented, i.e. any atoms which are bonded at the ends
   ! of the cluster are included into the cluster. LORRAINE!
      type(SCF_DATA_TYPE), INOUT :: self
      call read_(stdin,self%defragment)
   end subroutine

   subroutine read_delete_all_archives(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_delete_all_archives_(self,val)
   end subroutine

   subroutine read_delete_SCF_archives(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_delete_SCF_archives_(self,val)
   end subroutine

   subroutine read_DIIS(self)
   ! Read the DIIS options
      type(SCF_DATA_TYPE), INOUT :: self
      call read_keywords_(self%SCF_DIIS)
   end subroutine

   subroutine read_DIIS_error_temp_cutoff(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_DIIS_error_temp_cutoff_(self,val)
   end subroutine

   subroutine read_do_outputs(self)
   ! Read whether to output result files for every lambda step in the
   ! XCW. This will mess up the stdout text!
      type(SCF_DATA_TYPE), INOUT :: self
      call read_(stdin,self%do_outputs)
   end subroutine

   subroutine read_ELMO_B(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_ELMO_B_(self,val)
   end subroutine

   subroutine read_ELMO_P(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_ELMO_P_(self,val)
   end subroutine

   subroutine read_ERI_accuracy(self)
   ! Read a named ERI accuracy level, one of "very_low", "low", "medium"
   ! or "high", and set the four ERI cutoffs from it.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
      call read_(stdin,word)
      call set_ERI_accuracy_(self,word)
   end subroutine

   subroutine read_escalate_ERI_accuracy(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_escalate_ERI_accuracy_(self,val)
   end subroutine

   subroutine read_ERI_bf_overlap_cutoff(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_ERI_bf_overlap_cutoff_(self,val)
   end subroutine

   subroutine read_ERI_disk_cutoff(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_ERI_disk_cutoff_(self,val)
   end subroutine

   subroutine read_ERI_J_density_cutoff(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_ERI_J_density_cutoff_(self,val)
   end subroutine

   subroutine read_ERI_K_density_cutoff(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_ERI_K_density_cutoff_(self,val)
   end subroutine

   subroutine read_ERI_primitive_pair_cutoff(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_ERI_primitive_pair_cutoff_(self,val)
   end subroutine

   subroutine read_ERI_Schwarz_cutoff(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_ERI_Schwarz_cutoff_(self,val)
   end subroutine

   subroutine read_ee_adiabatic_constant(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_ee_adiabatic_constant_(self,val)
   end subroutine

   subroutine read_exchange_functional(self)
   ! Read the DFT exchange functional. It may not be given together with
   ! dft_functional=, which names the whole functional.
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      DIE_IF(self%DFT_functional/=" ","SCF_DATA:read_exchange_functional ... dft_functional= was given: do not also give dft_exchange_functional=")
      call set_exchange_functional_(self,val)
      self%DFT_parts_given = TRUE
   end subroutine

   subroutine read_guess_output(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_guess_output_(self,val)
   end subroutine

   subroutine read_initial_density(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_initial_density_(self,val)
   end subroutine

   subroutine read_initial_atomic_states(self)
   ! Read the initial charges and multiplicities as a flat list for
   ! the unique atoms.
      type(SCF_DATA_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: qmult
      call read_all_(stdin,qmult)
      call set_initial_atomic_states_(self,qmult)
      call destroy_(qmult)
   end subroutine

   subroutine read_initial_lambda(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_initial_lambda_(self,val)
   end subroutine

   subroutine read_initial_MOs(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_initial_MOs_(self,val)
   end subroutine

   subroutine read_initial_Us(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_initial_Us_(self,val)
   end subroutine

   subroutine read_lambda_max(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_lambda_max_(self,val)
   end subroutine

   subroutine read_lambda_step(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_lambda_step_(self,val)
   end subroutine

   subroutine read_level_shift(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_level_shift_(self,val)
   end subroutine

   subroutine read_level_shift_finish(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_level_shift_finish_(self,val)
   end subroutine

   subroutine read_linear_dependence_shift(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_linear_dependence_shift_(self,val)
   end subroutine

   subroutine read_linear_dependence_tol(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_linear_dependence_tol_(self,val)
   end subroutine

   subroutine read_min_iterations(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_min_iterations_(self,val)
   end subroutine

   subroutine read_max_iterations(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_max_iterations_(self,val)
   end subroutine

   subroutine read_max_update_stepsize(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_max_update_stepsize_(self,val)
   end subroutine

   subroutine read_MO_gradient_stepsize(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_MO_gradient_stepsize_(self,val)
   end subroutine

   subroutine read_nhf(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_nhf_(self,val)
   end subroutine

   subroutine read_output(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_output_(self,val)
   end subroutine

   subroutine read_output_results(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_output_results_(self,val)
   end subroutine

   subroutine read_show_timings(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_show_timings_(self,val)
   end subroutine

   subroutine read_quantization_axis(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      VEC(REAL,3) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_quantization_axis_(self,val)
   end subroutine

   subroutine read_relativity_kind(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_relativity_kind_(self,val)
   end subroutine

   subroutine read_restart_skip_ASFs(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_restart_skip_asfs_(self,val)
   end subroutine

   subroutine read_SCF_kind(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_SCF_kind_(self,val)
   end subroutine

   subroutine read_save_cluster_charges(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_save_cluster_charges_(self,val)
   end subroutine

   subroutine read_save_20_cluster_charges(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_save_20_cluster_charges_(self,val)
   end subroutine

   subroutine read_stabilize_density(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_stabilize_density_(self,val)
   end subroutine

   subroutine read_temperature(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_temperature_(self,val)
   end subroutine

   subroutine read_test(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_test_(self,val)
   end subroutine

   subroutine read_use_BFGS(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_BFGS_(self,val)
   end subroutine

   subroutine read_use_default_guess_options(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_default_guess_options_(self,val)
   end subroutine

   subroutine read_use_ELMO(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_ELMO_(self,val)
   end subroutine

   subroutine read_use_ELMO_01(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_ELMO_01_(self,val)
   end subroutine

   subroutine read_use_ELMO_11(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_ELMO_11_(self,val)
   end subroutine

   subroutine read_use_LSQ(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_LSQ_(self,val)
   end subroutine

   subroutine read_use_NOMO(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_NOMO_(self,val)
   end subroutine

   subroutine read_using_1e_sl_term(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_1e_sl_term_(self,val)
   end subroutine

   subroutine read_using_1e_srxa_term(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_1e_srxa_term_(self,val)
   end subroutine

   subroutine read_using_1e_zora_term(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_1e_zora_term_(self,val)
   end subroutine

   subroutine read_using_2e_sl_term(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_2e_sl_term_(self,val)
   end subroutine

   subroutine read_using_aa_term(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_aa_term_(self,val)
   end subroutine

   subroutine read_using_bl_term(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_bl_term_(self,val)
   end subroutine

   subroutine read_using_bs_term(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_bs_term_(self,val)
   end subroutine

   subroutine read_using_bs_t_term(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_bs_t_term_(self,val)
   end subroutine

   subroutine read_using_CP_DIIS(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_CP_DIIS_(self,val)
   end subroutine

   subroutine read_using_cluster_charges(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_cluster_charges_(self,val)
   end subroutine

   subroutine read_using_damping(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_damping_(self,val)
   end subroutine

   subroutine read_using_delta_build(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_delta_build_(self,val)
   end subroutine

   subroutine read_using_batched_XC(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_batched_XC_(self,val)
   end subroutine

   subroutine read_using_gaussian_pair_J(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_gaussian_pair_J_(self,val)
   end subroutine

   subroutine read_using_RI_J(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_RI_J_(self,val)
   end subroutine

   subroutine read_using_COSX(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_COSX_(self,val)
   end subroutine

   subroutine read_COSX_grid(self)
   ! Read .COSX_grid as a becke_grid= block; what it does not set keeps
   ! the default of .make_COSX_grids.
      type(SCF_DATA_TYPE), INOUT :: self
      call destroy_(self%COSX_grid)
      call make_COSX_grids_(self)
      call read_keywords_(self%COSX_grid)
   end subroutine

   subroutine read_COSX_final_grid(self)
   ! Read .COSX_final_grid as a becke_grid= block; what it does not set
   ! keeps the default of .make_COSX_grids.
      type(SCF_DATA_TYPE), INOUT :: self
      call destroy_(self%COSX_final_grid)
      call make_COSX_grids_(self)
      call read_keywords_(self%COSX_final_grid)
   end subroutine

   subroutine read_using_COSX_final_grid(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_COSX_final_grid_(self,val)
   end subroutine

   subroutine read_using_COSX_overlap_fitting(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_COSX_overlap_fitting_(self,val)
   end subroutine

   subroutine read_delta_build_period(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_delta_build_period_(self,val)
   end subroutine

   subroutine read_using_direct_SCF(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_direct_SCF_(self,val)
   end subroutine

   subroutine read_using_DK1(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_DK1_(self,val)
   end subroutine

   subroutine read_using_DK2(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_DK2_(self,val)
   end subroutine

   subroutine read_using_DK3(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_DK3_(self,val)
   end subroutine

   subroutine read_using_DK1_PCE(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_DK1_PCE_(self,val)
   end subroutine

   subroutine read_using_DK2_PCE(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_DK2_PCE_(self,val)
   end subroutine

   subroutine read_using_DIIS(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_DIIS_(self,val)
   end subroutine

   subroutine read_using_exponential_update(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_exponential_update_(self,val)
   end subroutine

   subroutine read_using_Fermi_Amaldi(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_Fermi_Amaldi_(self,val)
   end subroutine

   subroutine read_using_level_shift(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_level_shift_(self,val)
   end subroutine

   subroutine read_using_MO_gradient_update(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_MO_gradient_update_(self,val)
   end subroutine

   subroutine read_using_NDDO(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_NDDO_(self,val)
   end subroutine

   subroutine read_using_NUDO(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_NUDO_(self,val)
   end subroutine

   subroutine read_using_orbital_locking(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_orbital_locking_(self,val)
   end subroutine

   subroutine read_using_PCE_correction(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_PCE_correction_(self,val)
   end subroutine

   subroutine read_using_PIE(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_PIE_(self,val)
   end subroutine

   subroutine read_using_ppzpp_method(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_ppzpp_method_(self,val)
   end subroutine

   subroutine read_die_if_not_converged(self)
   ! Read whether an unconverged SCF stops the job
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_die_if_not_converged_(self,val)
   end subroutine

   subroutine read_ANO_kind(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_ANO_kind_(self,val)
   end subroutine

   subroutine read_pFON_start_temperature(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_pFON_start_temperature_(self,val)
   end subroutine

   subroutine read_pFON_cooling_rate(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_pFON_cooling_rate_(self,val)
   end subroutine

   subroutine read_pFON_finish(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_pFON_finish_(self,val)
   end subroutine

   subroutine read_using_pFON_damping(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_pFON_damping_(self,val)
   end subroutine

   subroutine read_using_qq_cluster_charges(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_qq_cluster_charges_(self,val)
   end subroutine

   subroutine read_using_SC_cluster_charges(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_SC_cluster_charges_(self,val)
   end subroutine

   subroutine read_using_SC_Lorentz_fields(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_SC_Lorentz_fields_(self,val)
   end subroutine

   subroutine read_using_SO_FDFF_method(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_using_SO_FDFF_method_(self,val)
   end subroutine

   subroutine read_VMO_canonisation(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_VMO_canonisation_(self,val)
   end subroutine

   subroutine read_VMO_canonisation_MO(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_VMO_canonisation_MO_(self,val)
   end subroutine

   subroutine read_XC_atom_separation_cutoff(self)
   ! Generic read and set a quantity with units
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_XC_atom_separation_cutoff_(self,val)
   end subroutine

   subroutine read_lambda_criterion(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_lambda_criterion_(self,val)
   end subroutine

   subroutine read_put_constraint_stiffness(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_put_constraint_stiffness_(self,val)
   end subroutine

   subroutine read_use_matrix_free_stiffness(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_matrix_free_stiffness_(self,val)
   end subroutine

   subroutine read_stiffness_cg_tolerance(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_stiffness_cg_tolerance_(self,val)
   end subroutine

   subroutine read_stiffness_deflation(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_stiffness_deflation_(self,val)
   end subroutine

   subroutine read_stiffness_preconditioner(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_stiffness_preconditioner_(self,val)
   end subroutine

   subroutine read_stiffness_samples(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_stiffness_samples_(self,val)
   end subroutine

   subroutine read_use_stiffness_correction(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_stiffness_correction_(self,val)
   end subroutine

   subroutine read_zero_oo_constraint_block(self)
   ! Generic read and set
      type(SCF_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_zero_oo_constraint_block_(self,val)
   end subroutine


!  ================================================
!  Query the type of orbitals and their number kind
!  ===============================================

!   spinorbital_kind(SCF_kind) result (res) :: pure
!   ! Return the kind of spinorbitals used for the fock_mx and
!   ! density_mx (but *not* neccesarily the MO's -- see below)
!   ! in doing a particular "SCF_kind" of SCF calculation.
!      self :: IN
!      SCF_kind :: STR, optional, IN
!      res :: STR
!
!      s_kind :: STR
!
!      s_kind = .SCF_kind
!      if (present(SCF_kind)) s_kind = SCF_kind
!
!      select case (s_kind)
!      case("group        "); res = "restricted" ! for now
!      case("rhf          "); res = "restricted"
!      case("rks          "); res = "restricted"
!      case("rohf         "); res = "unrestricted"
!      case("uhf          "); res = "unrestricted"
!      case("uks          "); res = "unrestricted"
!      case("gchf         "); res = "general_complex"
!    ! case("pnd_uhf      "); res = "unrestricted"
!    ! case("pnd_uks      "); res = "unrestricted"
!      case("xray_rhf     "); res = "restricted"
!      case("xray_rks     "); res = "restricted"
!      case("xray_uhf     "); res = "unrestricted"
!      case("xray_uks     "); res = "unrestricted"
!      case("xray_gchf    "); res = "general_complex"
!    ! case("xray_pnd_uhf "); res = "unrestricted"
!    ! case("xray_pnd_uks "); res = "unrestricted"
!      case("rdvpt        "); res = "restricted"
!      case("fragment-rhf "); res = "restricted" 
!      case("fragment-rks "); res = "restricted" 
!    ! case default; DIE("unknown scf kind: "//trim(s_kind))
!      end
!
!   end

   pure function spinorbital_genre(self,SCF_kind) result (res)
   ! Return the kind of spinorbitals used for the fock_mx and
   ! density_mx (but *not* neccesarily the MO's -- see below)
   ! in doing a particular "SCF_kind" of SCF calculation.
      type(SCF_DATA_TYPE), IN :: self
      STR(len=*), optional, IN :: SCF_kind
      STR(len=STR_SIZE) :: res
      STR(len=STR_SIZE) :: s_kind
      s_kind = self%SCF_kind
      if (present(SCF_kind)) s_kind = SCF_kind
      select case (s_kind)
      case ("group        "); res = "r"  ! for now
      case ("rhf          "); res = "r"
      case ("rks          "); res = "r"
      case ("rohf         "); res = "u"
      case ("uhf          "); res = "u"
      case ("uks          "); res = "u"
      case ("gchf         "); res = "gc"
   ! case("pnd_uhf      "); res = "u"
   ! case("pnd_uks      "); res = "u"
      case ("xray_rhf     "); res = "r"
      case ("xray_rks     "); res = "r"
      case ("xray_uhf     "); res = "u"
      case ("xray_uks     "); res = "u"
      case ("xray_gchf    "); res = "gc"
   ! case("xray_pnd_uhf "); res = "u"
   ! case("xray_pnd_uks "); res = "u"
      case ("rdvpt        "); res = "r"
      case ("fragment-rhf "); res = "r"
      case ("fragment-rks "); res = "r"
      end select
   end function

   pure function initial_density_genre(self) result (res)
   ! Return the kind of spinorbitals used for the initial density
   ! when it is to be *read in* not calculated.
      type(SCF_DATA_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      select case (self%initial_density)
      case ("core           "); res = "r"
      case ("fock           "); res = "r"
      case ("g              "); res = "gc"
      case ("general_complex"); res = "gc"
      case ("promolecule    "); res = "r"
      case ("r              "); res = "r"
      case ("restricted     "); res = "r"
      case ("u              "); res = "u"
      case ("unrestricted   "); res = "u"
      end select
   end function

   pure function is_restricted(self) result (res)
   ! Return TRUE if a restricted calculation
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = spinorbital_genre_(self)=="r"
   end function

   pure function is_unrestricted(self) result (res)
   ! Return TRUE if a restricted calculation
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = spinorbital_genre_(self)=="u"
   end function


!   is_general result (res) :: pure
!   ! Return TRUE if a general calculation
!      self :: IN
!      res :: BIN
!      res = .spinorbital_genre=="g"
!   end

   pure function is_general_complex(self) result (res)
   ! Return TRUE if a general_complex calculation
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = spinorbital_genre_(self)=="gc"
   end function

   PURE function MOs_kind(self,SCF_kind) result (res)
   ! Return the kind of spinorbitals used for the molecular orbitals
   ! in a particular "SCF_kind". In only differs from the
   ! .spinorbital_kind for the ROHF kind of SCF calculation.
   ! This is also the kind of the core_mx needed for the SCF.
      type(SCF_DATA_TYPE), IN :: self
      STR(len=*), optional, IN :: SCF_kind
      STR(len=STR_SIZE) :: res
      STR(len=STR_SIZE) :: s_kind
      s_kind = self%SCF_kind
      if (present(SCF_kind)) s_kind = SCF_kind
      select case (s_kind)
      case ("rhf","rks"); res = "restricted"
      case ("rohf"); res = "restricted"
      case ("uhf","uks"); res = "unrestricted"
      case ("ghf"); res = "general"
      case ("no_ghf"); res = "general"
      case ("rchf"); res = "restricted_complex"
      case ("uchf"); res = "unrestricted_complex"
      case ("gchf"); res = "general_complex"
   ! case("pnd_uhf");      res = "unrestricted"
   ! case("pnd_uks");      res = "unrestricted"
      case ("xray_rhf"); res = "restricted"
      case ("xray_rks"); res = "restricted"
      case ("xray_uhf"); res = "unrestricted"
      case ("xray_uks"); res = "unrestricted"
      case ("xray_gchf"); res = "general_complex"
   ! case("xray_pnd_uhf"); res = "unrestricted"
   ! case("xray_pnd_uks"); res = "unrestricted"
      case ("rdvpt"); res = "restricted"
      case ("farghar-rhf "); res = "restricted"  ! <<< This is a hack!
      case ("farghar-rks "); res = "restricted"  ! <<< This is a hack!
      end select
   end function

   PURE function MOs_genre(self,SCF_kind) result (res)
   ! Return the genre of spinorbitals used for the MOs in a particular
   ! "SCF_kind". In only differs from the .spinorbital_kind for the
   ! ROHF kind of SCF calculation.  This is also the kind of the
   ! core_mx needed for the SCF.
      type(SCF_DATA_TYPE), IN :: self
      STR(len=*), optional, IN :: SCF_kind
      STR(len=STR_SIZE) :: res
      STR(len=STR_SIZE) :: s_kind
      s_kind = self%SCF_kind
      if (present(SCF_kind)) s_kind = SCF_kind
      select case (s_kind)
      case ("rhf         "); res = "r"
      case ("rks         "); res = "r"
      case ("rohf        "); res = "r"
      case ("uhf         "); res = "u"
      case ("uks         "); res = "u"
      case ("gchf        "); res = "gc"
   ! case("pnd_uhf     "); res = "u"
   ! case("pnd_uks     "); res = "u"
      case ("xray_rhf    "); res = "r"
      case ("xray_rks    "); res = "r"
      case ("xray_uhf    "); res = "u"
      case ("xray_uks    "); res = "u"
      case ("xray_gchf   "); res = "gc"
   ! case("xray_pnd_uhf"); res = "u"
   ! case("xray_pnd_uks"); res = "u"
      case ("rdvpt       "); res = "r"
      case ("farghar-rhf "); res = "r"  ! <<< This is a hack!
      case ("farghar-rks "); res = "r"  ! <<< This is a hack!
      end select
   end function

   PURE function MO_energies_kind(self,SCF_kind) result (res)
   ! Return the kind of vectors used for the orbital energies in a
   ! particular "SCF_kind"
      type(SCF_DATA_TYPE), IN :: self
      STR(len=*), optional, IN :: SCF_kind
      STR(len=STR_SIZE) :: res
      STR(len=STR_SIZE) :: s_kind
      s_kind = self%SCF_kind
      if (present(SCF_kind)) s_kind = SCF_kind
      select case (s_kind)
      case ("rhf","rks"); res = "restricted"
      case ("rohf"); res = "restricted"
      case ("uhf","uks"); res = "unrestricted"
      case ("ghf"); res = "general"
      case ("no_ghf"); res = "general"
      case ("rchf"); res = "restricted"
      case ("uchf"); res = "unrestricted"
      case ("gchf"); res = "general"
   ! case("pnd_uhf");      res = "unrestricted"
   ! case("pnd_uks");      res = "unrestricted"
      case ("xray_rhf"); res = "restricted"
      case ("xray_uhf"); res = "unrestricted"
      case ("xray_gchf"); res = "general"
   ! case("xray_pnd_uhf"); res = "unrestricted"
   ! case("xray_pnd_uks"); res = "unrestricted"
      case ("xray_rks"); res = "restricted"
      case ("xray_uks"); res = "unrestricted"
      case ("rdvpt"); res = "restricted"
      case ("fragment-rhf "); res = "restricted"
      case ("fragment-rks "); res = "restricted"
      end select
   end function

   PURE function MO_energies_genre(self,SCF_kind) result (res)
   ! Return the kind of vectors used for the orbital energies in a
   ! particular "SCF_kind"
      type(SCF_DATA_TYPE), IN :: self
      STR(len=*), optional, IN :: SCF_kind
      STR(len=STR_SIZE) :: res
      STR(len=STR_SIZE) :: s_kind
      s_kind = self%SCF_kind
      if (present(SCF_kind)) s_kind = SCF_kind
      select case (s_kind)
      case ("rhf         "); res = "r"
      case ("rks         "); res = "r"
      case ("rohf        "); res = "r"
      case ("uhf         "); res = "u"
      case ("uks         "); res = "u"
      case ("gchf        "); res = "g"
   ! case("pnd_uhf     "); res = "u"
   ! case("pnd_uks     "); res = "u"
      case ("xray_rhf    "); res = "r"
      case ("xray_rks    "); res = "r"
      case ("xray_uhf    "); res = "u"
      case ("xray_uks    "); res = "u"
      case ("xray_gchf   "); res = "g"
   ! case("xray_pnd_uhf"); res = "u"
   ! case("xray_pnd_uks"); res = "u"
      case ("rdvpt       "); res = "r"
      case ("farghar-rhf "); res = "r"  ! <<< This is a hack!
      case ("farghar-rks "); res = "r"  ! <<< This is a hack!
      end select
   end function

   PURE function MOs_number_kind(self) result (res)
   ! Return the kind of numbers used for a particular "kind" of 
   ! SCF calculation
      type(SCF_DATA_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      select case (self%SCF_kind)
      case ("rhf","rks","rohf","uhf","uks","ghf","no_ghf"); res = "real"
      case ("rchf","uchf","gchf","xray_gchf"); res = "complex"
      case ("xray_rhf","xray_rks","xray_uks"); res = "real"
      case ("xray_uhf","pnd_uhf","xray_pnd_uhf"); res = "real"
      case ("pnd_uks","xray_pnd_uks"); res = "real"
      case ("rdvpt","restricted_dvpt"); res = "real"
      end select
   end function

   PURE function is_DFT_calculation(self,SCF_kind) result (res)
   ! Return TRUE if the scf kind is a DFT calculation
      type(SCF_DATA_TYPE), IN :: self
      STR(len=*), optional, IN :: SCF_kind
      BIN :: res
      STR(len=STR_SIZE) :: s_kind
      s_kind = self%SCF_kind
      if (present(SCF_kind)) s_kind = SCF_kind
      res = FALSE
      select case (s_kind)
      case ("rhf"); res = FALSE
      case ("rks"); res = TRUE
      case ("rohf"); res = FALSE
      case ("uhf"); res = FALSE
      case ("uks"); res = TRUE
      case ("ghf"); res = FALSE
      case ("no_ghf"); res = FALSE
      case ("rchf"); res = FALSE
      case ("uchf"); res = FALSE
      case ("gchf"); res = FALSE
      case ("xray_rhf"); res = FALSE
      case ("xray_rks"); res = TRUE
      case ("xray_uhf"); res = FALSE
      case ("xray_gchf"); res = FALSE
      case ("xray_uks"); res = TRUE
   ! case("pnd_uhf");      res = FALSE
   ! case("pnd_uks");      res = TRUE
   ! case("xray_pnd_uhf"); res = FALSE
   ! case("xray_pnd_uks"); res = TRUE
      case ("rdvpt"); res = FALSE
      case ("fragment-rhf "); res = FALSE
      case ("fragment-rks "); res = TRUE
      end select
   end function

   PURE function is_constrained_wavefunction(self,SCF_kind) result (res)
   ! Return TRUE if the scf kind is a constrained wavefunction
      type(SCF_DATA_TYPE), IN :: self
      STR(len=*), optional, IN :: SCF_kind
      BIN :: res
      STR(len=STR_SIZE) :: s_kind
      s_kind = self%SCF_kind
      if (present(SCF_kind)) s_kind = SCF_kind
      res = FALSE
      select case (s_kind)
      case ("rhf"); res = FALSE
      case ("rks"); res = FALSE
      case ("rohf"); res = FALSE
      case ("uhf"); res = FALSE
      case ("uks"); res = FALSE
      case ("ghf"); res = FALSE
      case ("no_ghf"); res = FALSE
      case ("rchf"); res = FALSE
      case ("uchf"); res = FALSE
      case ("gchf"); res = FALSE
      case ("xray_rhf"); res = TRUE
      case ("xray_rks"); res = TRUE
      case ("xray_uhf"); res = TRUE
      case ("xray_gchf"); res = TRUE
      case ("xray_uks"); res = TRUE
   ! case("pnd_uhf");       res = TRUE
   ! case("pnd_uks");       res = TRUE
   ! case("xray_pnd_uhf");  res = TRUE
   ! case("xray_pnd_uks");  res = TRUE
      case ("fragment-rhf"); res = FALSE
      case ("fragment-rks"); res = FALSE
      end select
   end function

   pure function is_constrained_SCF(self) result (res)
   ! Return TRUE if we are doing a constrained SCF calculation.
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = self%SCF_kind(1:5)=="xray_"
   ! res = .SCF_kind(1:5)=="xray_" OR .SCF_kind(1:4)=="pnd_"
   end function

   PURE function DFT_functional_name(self) result (res)
   ! Return the name of the DFT functional
      type(SCF_DATA_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
   ENSURE(is_DFT_calculation_(self),"SCF_DATA:DFT_functional_name ... must be a DFT calculation")
      res = self%DFT_functional
      if (res/=" ") return
      if (self%DFT_exchange_functional/=" " AND self%DFT_exchange_functional/="none") then
         res = self%DFT_exchange_functional
      end if
      if (self%DFT_correlation_functional/=" " AND self%DFT_correlation_functional/="none") then
         if (res/=" ") then
            res = trim(res)//"-"//trim(self%DFT_correlation_functional)
         else
            res = self%DFT_correlation_functional
         end if
      end if
   end function

   PURE function temperature_for_pFON(self) result (res)
   ! Return the temperature (in Kelvin) to use in the pFON method: the
   ! start temperature cooled by the cooling rate per iteration, from
   ! iteration 0 (the initial guess), and zero once pFON is off.
   ! NOTE: To see whether to apply pFON use .apply_pFON
      type(SCF_DATA_TYPE), IN :: self
      REAL :: res
      res = ZERO
      if (apply_pFON_(self)) then
         res = max(ZERO,self%pFON_start_temperature-self%pFON_cooling_rate*self%iteration)
      end if
   end function

   pure function apply_pFON(self) result (res)
   ! Return TRUE if pFON is to be applied this iteration: it is wanted,
   ! the DIIS error is still above DIIS_error_temp_cutoff, the iteration
   ! is before pFON_finish, and the schedule has not cooled to zero.
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      if (NOT self%using_pFON) then; res = FALSE
      else if (self%DIIS_error<self%DIIS_error_temp_cutoff) then; res = FALSE
      else if (self%iteration>=self%pFON_finish) then; res = FALSE
      else; res = self%pFON_start_temperature-self%pFON_cooling_rate*self%iteration>ZERO
      end if
   end function


!  =====
!  Tests
!  =====

   pure function SCF_done(self) result (res)
   ! Return TRUE if the scf procedure is done i.e. either converged or
   ! gone on for too long ... and, it must also have executed the
   ! requested minimum number of times ...
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = (is_converged_(self) OR exceeded_max_it_(self)) AND exceeded_min_it_(self)
   end function

   pure function is_converged(self) result (res)
   ! Return TRUE if the SCF appears converged *now*: the energy change and
   ! the DIIS error are both below their tolerances. Recomputed on every
   ! call, so it changes if the energy is remade after the loop; the state
   ! at the end of the loop is kept in .converged.
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = delta_converged_(self) AND is_converged_(self%SCF_DIIS)
   end function

   pure function delta_converged(self) result (res)
   ! For normal SCF calculations, return TRUE if the energy difference has
   ! converged. For constrained HF calculations, return TRUE if the fit_values
   ! have converged.
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (is_constrained_SCF_(self)) then
         res = abs(self%fit_value-self%old_fit_value)<self%convergence
      else
         res = abs(self%delta)<self%convergence
      end if
   end function

   pure function exceeded_max_it(self) result (res)
   ! Return TRUE if the SCF_DATA has exceeded the maximum iterations
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = self%iteration>=self%max_iterations
   end function

   pure function exceeded_min_it(self) result (res)
   ! Return TRUE if the SCF_DATA has exceeded the minimum iterations
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = self%iteration>=self%min_iterations
      if (is_constrained_SCF_(self)) res = res AND (self%iteration>1)
   end function

   pure function exceeded_lambda_max(self) result (res)
   ! Return TRUE if the SCF_DATA has exceeded the maximum lambda
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = self%lambda>(ONE+TOL(10))*self%lambda_max
                        ! TOL(10) allows for roundoff errors
   end function

   pure function will_exceed_lambda_max(self) result (res)
   ! Return TRUE if the SCF_DATA will exceeded the maximum lambda
   ! in the next iteration.
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = self%lambda+self%lambda_step>(ONE+TOL(10))*self%lambda_max
                        ! TOL(10) allows for roundoff errors
   end function

   pure function do_delta_build(self) result (res)
   ! Return TRUE if a delta fock matrix build is allowed (assuming that the old
   ! fock matrix and old density matrix are available)
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = self%using_delta_build AND self%iteration>0
      ! Every .delta_build_period iterations, rebuild from the full density
      if (res AND self%delta_build_period>0) res = mod(self%iteration,self%delta_build_period)/=0
   end function

   pure function using_meta_GGA_functional(self) result (res)
   ! Return TRUE if a functional of the kinetic energy density (a meta-GGA)
   ! is being used in the DFT calculation.
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = self%using_meta_GGA_exchange OR self%using_meta_GGA_correlation
   end function

   pure function using_GGA_functional(self) result (res)
   ! Return TRUE if a generalised gradient approximation (GGA) functional is
   ! being used in the DFT calculation.
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = self%using_GGA_exchange OR self%using_GGA_correlation
   end function

   pure function using_nonstandrad_ERI_cutoffs(self) result (res)
   ! Return TRUE if nonstandard ERI cutoffs are being used
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      type(SCF_DATA_TYPE) :: std
      res = self%ERI_disk_cutoff/=std%ERI_disk_cutoff OR self%ERI_Schwarz_cutoff/=std%ERI_Schwarz_cutoff OR self%ERI_J_density_cutoff/=std%ERI_J_density_cutoff OR self%ERI_K_density_cutoff/=std%ERI_K_density_cutoff OR self%ERI_primitive_pair_cutoff/=std%ERI_primitive_pair_cutoff
   end function


!  ======================
!  DIIS tests. Be careful
!  ======================

   pure function apply_DIIS(self) result (res)
   ! Return TRUE if DIIS extrapolation must be applied this iteration,
   ! or has been applied this iteration.
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      if (NOT self%using_DIIS) then; res = FALSE
      else; res = apply_DIIS_(self%SCF_DIIS)
      end if
   end function

   pure function apply_damping(self) result (res)
   ! Return TRUE if density matrix damping is to be applied this iteration
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = self%using_damping AND self%iteration<self%damp_finish
   end function

   pure function apply_level_shifting(self) result (res)
   ! Return TRUE if level shifting must be applied this iteration
      type(SCF_DATA_TYPE), IN :: self
      BIN :: res
      res = NOT is_zero_(self%level_shift) AND self%using_level_shift AND self%iteration<self%level_shift_finish
   end function


!   fitting result (res) :: pure
!   ! Return true if we are fitting the wavefunction.
!      self :: IN
!      res :: BIN
!
!      res = FALSE
!      select case (.SCF_kind)
!      case("xray_rhf");     res = TRUE
!      case("xray_rks");     res = TRUE
!      case("xray_uhf");     res = TRUE
!      case("xray_uks");     res = TRUE
!      case("xray_gchf");    res = TRUE
!      case("pnd_uhf ");     res = TRUE
!      case("pnd_uks ");     res = TRUE
!      case("xray_pnd_uhf"); res = TRUE
!      case("xray_pnd_uks"); res = TRUE
!      case default;         res = FALSE
!      end
!
!   end

!  ===============
!  Output routines
!  ===============

   subroutine put_banner(self)
   ! Prints out the nuclear energy and initial guess energy.
      type(SCF_DATA_TYPE), IN :: self
      if (NOT self%output) return
      call flush_(stdout)
      call text_(stdout,"===============")
      call text_(stdout,"SCF calculation")
      call text_(stdout,"===============")
      call flush_(stdout)
   end subroutine

   subroutine put_COSX_grid(self,label,grid)
   ! Put the settings of the COSX grid "grid" under "label".
      type(SCF_DATA_TYPE), IN :: self
      STR(len=*), IN :: label
      type(BECKE_GRID_TYPE), allocatable, IN :: grid
      STR(len=STR_SIZE) :: tag
      tag = label
      tag(32:32) = "="
      if (NOT allocated(grid)) then
         call text_(stdout,tag(1:32)//" default")
         return
      end if
      call show_(stdout,tag(1:32),grid%accuracy)
      call show_(stdout,"  radial points                =",grid%n_radial_pts)
      call show_(stdout,"  angular order, heavy atoms   =",grid%l_angular_grid)
      call show_(stdout,"  angular order, H             =",grid%l_H_angular_grid)
      if (grid%pruning_scheme=="adaptive") call show_(stdout,"  angular order, bonding       =",grid%l_bonding_angular_grid)
      call show_(stdout,"  pruning scheme               =",grid%pruning_scheme)
   end subroutine

   subroutine put_options(self,crystal,nucleus_model)
   ! Prints out a summary of what options being used. "nucleus_model",
   ! which belongs to the molecule, is printed with the relativity options.
      type(SCF_DATA_TYPE), IN :: self
      type(CRYSTAL_TYPE), optional, IN :: crystal
      STR(len=*), optional, IN :: nucleus_model
      VEC(REAL,3) :: q
      STR(len=STR_SIZE) :: acc
      if (NOT self%output) return
      call show_(stdout,"SCF kind                       =",self%SCF_kind)
      if (self%use_Fermi_Amaldi) then
         call show_(stdout,"Using Fermi Amaldi?            =",self%use_Fermi_Amaldi)
      end if
      if (self%using_FON) then
         call flush_(stdout)
         call text_(stdout,"Finite temperature SCF:")
         call flush_(stdout)
         call show_(stdout,"Temperature (in Kelvin)        =",to_units_(self%temperature,"kelvin"))
      end if
      call flush_(stdout)
      call text_(stdout,"Initial guess options:")
      call flush_(stdout)
      call show_(stdout,"Kind of initial density        =",self%initial_density)
      call show_(stdout,"Kind of initial MO's           =",self%initial_MOs)
      if (is_DFT_calculation_(self)) then
         if (self%DFT_functional/=" ") then
            call show_(stdout,"DFT functional                 =",self%DFT_functional)
            if (self%using_hybrid_exchange) then
               call show_(stdout,"Fraction of exact exchange     =",self%hybrid_exchange_factor)
            end if
         end if
         call show_(stdout,"DFT Exchange functional        =",self%DFT_exchange_functional)
         call show_(stdout,"DFT Correlation functional     =",self%DFT_correlation_functional)
      end if
      if (NOT self%using_direct_SCF) then
         call show_(stdout,"Using direct SCF?              =",self%using_direct_SCF)
      end if
      if (self%using_delta_build) then
         call show_(stdout,"Using delta build              =",self%using_delta_build)
      end if
      if (self%using_cluster_charges) then
         call show_(stdout,"Using cluster charges          =",self%using_cluster_charges)
         call show_(stdout,"Self consistent charges?       =",self%using_SC_cluster_charges)
         call show_(stdout,"Cluster radius                 =",self%cluster_radius)
         call show_(stdout,"Defragment                     =",self%defragment)  !testing, turn off defragment in case of network compounds, Lorraine. 
      end if
      if (self%using_SC_Lorentz_fields) then
         call show_(stdout,"Using SC Lorentz fields        =",self%using_SC_Lorentz_fields)
      end if

      ! How J and K are built. Every one of these changes the numbers, so it is
      ! reported whenever it is not the default -- the Becke grid inheritance bug
      ! (TASKS_AND_HISTORY.md) stayed hidden precisely because nothing echoed what was
      ! actually in force.
      if (self%using_gaussian_pair_J OR self%using_RI_J OR self%using_COSX OR NOT self%using_batched_XC) then
         call flush_(stdout)
         call text_(stdout,"Coulomb and exchange:")
         call flush_(stdout)
         if (self%using_gaussian_pair_J) call show_(stdout,"J from primitive pair list?    =",self%using_gaussian_pair_J)
         if (self%using_RI_J) call show_(stdout,"J by RI density fitting?       =",self%using_RI_J)
         if (self%using_COSX) then
            call show_(stdout,"K by COSX quadrature?          =",self%using_COSX)
            call put_COSX_grid_(self,"COSX grid",self%COSX_grid)
            if (self%using_COSX_final_grid) call put_COSX_grid_(self,"COSX final grid",self%COSX_final_grid)
            call show_(stdout,"COSX overlap fitting?          =",self%using_COSX_overlap_fitting)
         end if
         if (NOT self%using_batched_XC) call show_(stdout,"Batched XC integration?        =",self%using_batched_XC)
      end if
      if (using_nonstandrad_ERI_cutoffs_(self) OR self%ERI_accuracy_given) then
         call set_real_fortran_fmt_(stdout,"e")
         call flush_(stdout)
         call text_(stdout,"Integrals:")
         call flush_(stdout)
         if (self%ERI_accuracy_given) then
            acc = effective_ERI_accuracy_(self)
            call show_(stdout,"ERI accuracy                   =",self%ERI_accuracy)
            call show_(stdout,"Escalate ERI accuracy          =",self%escalate_ERI_accuracy)
            if (acc/=self%ERI_accuracy) call show_(stdout,"ERI accuracy in force          =",acc)
         end if
         call show_(stdout,"ERI disk cutoff                =",self%ERI_disk_cutoff)
         call show_(stdout,"ERI Schwarz test cutoff        =",self%ERI_Schwarz_cutoff)
         call show_(stdout,"ERI J density cutoff           =",self%ERI_J_density_cutoff)
         call show_(stdout,"ERI K density cutoff           =",self%ERI_K_density_cutoff)
         call show_(stdout,"ERI primitive pair cutoff      =",self%ERI_primitive_pair_cutoff)
         call set_real_fortran_fmt_(stdout,"f")
      end if
      if (self%using_NDDO) call show_(stdout,"Doing NDDO?                    =",self%using_NDDO)
      if (self%relativity_kind/="none") then
         call flush_(stdout)
         call text_(stdout,"Relativistic/Magnetic options:")
         call flush_(stdout)
         call show_(stdout,"Relativity kind                =",self%relativity_kind)
         if (present(nucleus_model)) then
            call show_(stdout,"Nucleus model                  =",nucleus_model)
         end if
         if (spinorbital_genre_(self)=="gc") then
            q = self%quantization_axis
            call show_(stdout,"Quantization axis              =",q(1),q(2),q(3))
            call show_(stdout,"ZORA (1 electron) terms        =",self%using_1e_zora_term)
            call show_(stdout,"Using B:L term                 =",self%using_bl_term)
            call show_(stdout,"Using B:S term                 =",self%using_bs_term)
            call show_(stdout,"Using B:S T term               =",self%using_bs_t_term)
            call show_(stdout,"Using A:A term                 =",self%using_aa_term)
            call show_(stdout,"Using 1e S:L term              =",self%using_1e_sl_term)
            call show_(stdout,"Using 1e S:(rxA) term          =",self%using_1e_srxa_term)
            call show_(stdout,"Using 2e S:L term              =",self%using_2e_sl_term)
            call show_(stdout,"Factor for 1e S:L term         =",self%sl_1e_factor)
            call show_(stdout,"Factor for 2e S:L term         =",self%sl_2e_factor)
         end if
      end if  ! relativity
      call flush_(stdout)
      call text_(stdout,"SCF termination options:")
      call flush_(stdout)
      call set_real_fortran_fmt_(stdout,"e")
      call show_(stdout,"Delta E convergence tolerance  =",self%convergence)
      call show_(stdout,"Guess SCF convergence          =",self%guess_convergence)
      call show_(stdout,"Gradient/DIIS convergence      =",self%SCF_DIIS%convergence_tolerance)
      call set_real_fortran_fmt_(stdout,"f")
      call show_(stdout,"Minimum # of iterations        =",self%min_iterations)
      call show_(stdout,"Maximum # of iterations        =",self%max_iterations)
      call flush_(stdout)
      call text_(stdout,"Linear dependence options:")
      call flush_(stdout)
      call set_real_fortran_fmt_(stdout,"e")
      call show_(stdout,"Linear dependence tolerance    =",self%linear_dependence_tol)
      call show_(stdout,"Linear dependence shift        =",self%linear_dependence_shift)
      call show_(stdout,"Min. overlap matrix eigenvalue =",self%min_overlap_mx_eigenvalue)
      call show_(stdout,"No. of small S eigenvalues     =",self%n_small_S_eigenvalues)
      call show_(stdout,"No. of neg.  S eigenvalues     =",self%n_neg_S_eigenvalues)
      call flush_(stdout)
      call text_(stdout,"Convergence acceleration options:")
      call flush_(stdout)
      call set_real_fortran_fmt_(stdout,"f")
      call show_(stdout,"Using level shift              =",self%using_level_shift)
      if (self%using_level_shift) then
         call show_(stdout,"Level shift                    =",self%level_shift)
         call show_(stdout,"Level shift  quits at          =",self%level_shift_finish)
      end if
      if (self%using_orbital_locking) then
         call show_(stdout,"Using orbital locking          =",self%using_orbital_locking)
      end if
      call show_(stdout,"Using density damping          =",self%using_damping)
      if (self%using_damping) then
         call show_(stdout,"Damping factor                 =",self%damp_factor)
         call show_(stdout,"Damping quits at               =",self%damp_finish)
      end if
      if (self%ANO_kind/="average") then
         call show_(stdout,"ANO atoms made spherical by    =",self%ANO_kind)
      end if
      if (self%using_open_shell_ensemble) then
         call show_(stdout,"Open-shell ensemble orbitals   =",self%n_open_orbitals)
         call show_(stdout,"Open-shell ensemble electrons  =",self%n_open_electrons)
         call show_(stdout,"Ensemble coupling constant a   =",self%AOC_coupling)
      end if
      if (self%using_pFON) then
         call show_(stdout,"Using pFON occupations         =",self%using_pFON)
         call show_(stdout,"pFON start temperature /K      =",self%pFON_start_temperature)
         call show_(stdout,"pFON cooling rate /K per iter  =",self%pFON_cooling_rate)
         call show_(stdout,"pFON quits at                  =",self%pFON_finish)
         call show_(stdout,"pFON quits at DIIS error       =",self%DIIS_error_temp_cutoff)
      end if
      if (self%using_MO_gradient_update OR self%using_exponential_update) then
         call show_(stdout,"Using MO gradient update       =",self%using_MO_gradient_update)
         call show_(stdout,"Using exponential update       =",self%using_exponential_update)
         call set_real_fortran_fmt_(stdout,"e")
         call show_(stdout,"MO gradient stepsize           =",self%MO_gradient_stepsize)
         call show_(stdout,"Maximum update stepsize        =",self%max_update_stepsize)
         call set_real_fortran_fmt_(stdout,"f")
      end if
      call show_(stdout,"Using DIIS                     =",self%using_DIIS)
      if (self%using_DIIS) call put_(self%SCF_DIIS)
      if (is_constrained_SCF_(self)) then
         call flush_(stdout)
         call text_(stdout,"====================================")
         call text_(stdout,"Experimental wavefunction parameters")
         call text_(stdout,"====================================")
         call flush_(stdout)
         call show_(stdout,"Lambda fitting parameter       =",self%lambda)
         call show_(stdout,"Lambda max                     =",self%lambda_max)
         call show_(stdout,"Lambda step                    =",self%lambda_step)
         call show_(stdout,"Zero occ-occ constraint?       =",self%zero_oo_constraint_block)
         call put_crystal_fit_info_(self,crystal)
      end if
   end subroutine

   subroutine put_table_head(self)
   ! Prints out the SCF iteration banner (and related options)
      type(SCF_DATA_TYPE), IN :: self
   ENSURE(allocated(self%table),"SCF_DATA:put_table_head ... no table created")
      if (NOT self%output) return
      call flush_(stdout)
      call text_(stdout,"==============")
      call text_(stdout,"SCF iterations")
      call text_(stdout,"==============")

      ! Put the table
      call put_headings_(self%table)
   end subroutine

   subroutine put_table_header(self)
   ! Prints out the SCF iteration banner (and related options)
      type(SCF_DATA_TYPE), IN :: self
   ENSURE(allocated(self%table),"SCF_DATA:put_table_header ... no table created")
      if (NOT self%output) return

      ! Put the table
      call put_headings_(self%table)
   end subroutine

   subroutine put_table_body_and_footer(self,crystal,flush)
   ! Print out the results for the current iteration.
   ! If converged, print out footer as well.
   ! NOTE: must have called put_table_head before!
      type(SCF_DATA_TYPE), INOUT :: self
      type(CRYSTAL_TYPE), optional, IN :: crystal
      BIN, optional, IN :: flush
      INT :: n
      if (NOT self%output) return

      ! Set margin notes ...
      call set_margin_info_(self)

      ! Set table body (leaky)
      if (NOT is_constrained_SCF_(self)) then
         call set_values_(self%table(1),[self%iteration],auto=FALSE)
         call set_values_(self%table(2),[self%energy])
         call set_values_(self%table(3),[self%delta])
         call set_values_(self%table(4),[self%DIIS_error])
         if (self%show_timings) call set_values_(self%table(5),[self%time_iteration])
         n = size(self%table)
         call set_values_(self%table(n),self%table_info)
         call force_width_(self%table(n),0)
      else
         call set_values_(self%table(1),[self%iteration],auto=FALSE)
         call set_values_(self%table(2),[self%lambda])
         call set_values_(self%table(3),[sqrt(self%penalty)])
         call set_values_(self%table(4),[self%energy])
         call set_values_(self%table(5),[self%fit_value-self%old_fit_value])
         call set_values_(self%table(6),[self%DIIS_error])
         call set_values_(self%table(7),[self%overlap_with_M0s])
         if (self%show_timings) call set_values_(self%table(8),[self%time_iteration])
         n = size(self%table)
         call set_values_(self%table(n),self%table_info)
         call force_width_(self%table(n),0)
      end if

      ! Put out the body
      call put_body_(self%table)

      ! Clear columns for next time (UNleaky!)
      call clear_columns_(self%table)
      if (SCF_done_(self)) then
         call put_footer_(self%table)
         call flush_(stdout)
         if (is_converged_(self)) then; call text_(stdout,"* * * SCF has converged * * *")
         else; call text_(stdout,"* * * SCF has not converged * * *")
         end if
         call put_footer_(self%table,flush=flush)
         if (is_constrained_SCF_(self) AND NOT will_exceed_lambda_max_(self)) then
            call put_crystal_footer_(self,crystal)
!          .table.put_headings
         end if
      end if
   end subroutine

   subroutine put_results(self,crystal)
   ! Prints out the results
      type(SCF_DATA_TYPE), IN :: self
      type(CRYSTAL_TYPE), optional, IN :: crystal
      if (NOT self%output_results) return
      call flush_(stdout)
      call text_(stdout,"===========")
      call text_(stdout,"SCF results")
      call text_(stdout,"===========")
      call put_energies_(self)
      if (is_constrained_SCF_(self)) call put_crystal_fit_info_(self,crystal)
      if (self%show_timings) call put_timings_(self)
   end subroutine

   subroutine put_timings(self)
   ! Print the CPU time spent in each phase of the SCF
      type(SCF_DATA_TYPE), IN :: self
      REAL :: total
      total = self%time_1e+self%time_grid+self%time_guess+self%time_fock_2e+self%time_xc+self%time_diis+self%time_diag+self%time_density
      if (total<=ZERO) return
      call flush_(stdout)
      call text_(stdout,"SCF timing, CPU seconds and percent of the SCF total:")
      call flush_(stdout)
      call put_timing_line_(self,"One-electron matrices",self%time_1e,total)
      call put_timing_line_(self,"DFT grid",self%time_grid,total)
      call put_timing_line_(self,"Initial guess",self%time_guess,total)
      call put_timing_line_(self,"Fock build, J and K",self%time_fock_2e,total)
      call put_timing_line_(self,"XC matrix and energy",self%time_xc,total)
      call put_timing_line_(self,"DIIS extrapolation",self%time_diis,total)
      call put_timing_line_(self,"MO update (diagonalise)",self%time_diag,total)
      call put_timing_line_(self,"Density matrix",self%time_density,total)
      call put_timing_line_(self,"Total",total,total)
      if (self%XC_grid_points>0) then
         call flush_(stdout)
         call show_(stdout,"DFT grid points used           =",self%XC_grid_points)
         call show_(stdout,"DFT grid points before pruning =",self%XC_grid_points_unpruned)
      end if
   end subroutine

   subroutine put_timing_line(self,label,t,total)
   ! One line of put_timings
      type(SCF_DATA_TYPE), IN :: self
      STR(len=*), IN :: label
      REAL, IN :: t,total
      REAL :: pct
      STR(len=28) :: name
      STR(len=STR_SIZE) :: ts,ps
      pct = 100.0d0*t/total
      name = label
      ts = to_str_(t,"f10.2")
      ps = to_str_(pct,"f8.1")
      call text_(stdout,"  "//name//repeat(" ",10-len_trim(ts))//trim(ts)//repeat(" ",8-len_trim(ps))//trim(ps)//" %")
   end subroutine

   subroutine put_initial_guess_energies(self)
   ! Prints out initial guess energies
      type(SCF_DATA_TYPE), IN :: self
      if (NOT self%output) return
      call flush_(stdout)
      call text_(stdout,"======================")
      call text_(stdout,"Initial guess energies")
      call text_(stdout,"======================")
      call put_energies_(self)
   end subroutine

   subroutine put_energies(self)
   ! Prints out the table foot for an SCF calculation, after convergence
   ! ornot as the case may be
      type(SCF_DATA_TYPE), IN :: self
      INT :: wid
      if (NOT self%output_results) return

      ! Nice width
      wid = len_trim(to_str_(self%nuclear_attraction_energy,"f",50,stdout%style%real_precision))+1
      if (self%relativity_kind=="x2c") wid = len_trim(to_str_(self%kinetic_energy,"f",50,stdout%style%real_precision))+1

      ! Print
      call flush_(stdout)
      call show_(stdout,"Total energy (E_e+V_NN)   E       =",self%energy,wid)
      call show_(stdout,"Total electronic energy   E_e     =",self%energy-self%nuclear_repulsion_energy,wid)
      call show_(stdout,"Nuclear repulsion energy  V_NN    =",self%nuclear_repulsion_energy,wid)
      call show_(stdout,"Electron repulsion energy V_ee    =",self%electron_repulsion_energy,wid)
      if (is_DFT_calculation_(self)) then

      ! V_ee = J + a_0 E_x + E_xc; the pieces follow it
         call show_(stdout,"Coulomb energy            J       =",self%electron_repulsion_energy-self%exact_exchange_energy-self%XC_energy,wid)
         if (self%using_hybrid_exchange) then
            call show_(stdout,"Scaled exact exchange     a_0*E_x =",self%exact_exchange_energy,wid)
         end if
         call show_(stdout,"XC energy                 E_xc    =",self%XC_energy,wid)
      end if
      if (self%relativity_kind=="x2c") then

      ! The kinetic and nuclear attraction energies are not separate
         call show_(stdout,"One-electron energy (X2C) h       =",self%kinetic_energy,wid)
      else
         call show_(stdout,"Nuclear attraction energy V_eN    =",self%nuclear_attraction_energy,wid)
      end if
      if (self%using_cluster_charges) then
         call show_(stdout,"Charge attraction energy  V_cN    =",self%charge_attraction_energy,wid)
      end if
      if (self%relativity_kind/="x2c") then
         call show_(stdout,"Total potential energy    V       =",self%energy-self%kinetic_energy,wid)
         call show_(stdout,"Kinetic energy            T       =",self%kinetic_energy,wid)
         call show_(stdout,"Virial ratio             -V/T     =",(self%kinetic_energy-self%energy)/self%kinetic_energy,wid)
      end if
      if (self%using_FON) then
         call flush_(stdout)
         call show_(stdout,"Fermi energy                      =",self%E_Fermi,wid)
      end if
   end subroutine

   subroutine put_crystal_fit_info(self,crystal)
   ! Prints out the crystal structure factor statistics.
      type(SCF_DATA_TYPE), IN :: self
      type(CRYSTAL_TYPE), IN :: crystal
      if (NOT self%output_results) return
      if (NOT is_constrained_SCF_(self)) return
      call show_(stdout,"Penalty in F                   =",self%penalty)
      if (allocated(crystal%xray_data) AND includes_(self%SCF_kind,"xray")) then
         call flush_(stdout)
         call text_(stdout,"==========")
         call text_(stdout,"X-ray data")
         call text_(stdout,"==========")
         call put_F_statistics_(crystal%xray_data)
         if (crystal%r_free_percentage>0 AND allocated(crystal%xray_r_free_data)) call put_F_free_stats_(crystal%xray_r_free_data)
         call put_stiffness_statistics_(self)
      end if

   ! if (.crystal.pnd_data.allocated AND .SCF_kind.includes("pnd")) then
   !    stdout.flush
   !    stdout.text("========")
   !    stdout.text("PND data")
   !    stdout.text("========")
   !    .crystal.pnd_data.PUT:put_F_statistics

    !       .crystal.pnd_r_free_data.put_F_free_stats
   ! end
   end subroutine

   subroutine put_stiffness_statistics(self)
   ! Print the statistics made by MOLECULE.SCF:make_constraint_stiffness,
   ! in the style of the structure factor statistics above them.
      type(SCF_DATA_TYPE), IN :: self
      if (NOT self%stiffness_made) return
      call flush_(stdout)
      call text_(stdout,"Fit statistics from the constraint stiffness:")
      call flush_(stdout)
      call show_(stdout,"Effective no. of parameters    =",self%p_eff)
      call show_(stdout,"Chi^2, sum of (F_z)^2          =",self%chi2_fit)
      call show_(stdout,"AIC, sigma-free                =",self%AIC_sigma)
      call show_(stdout,"GCV                            =",self%GCV)
      call show_(stdout,"Leave-one-out sum of squares   =",self%LOO_sum)
      if (self%TIH3_made) then
         call show_(stdout,"TIH 3-point lambda_opt         =",self%lambda_TIH3)
         call show_(stdout,"TIH 3-point D (error term)     =",self%TIH3_D)
         call show_(stdout,"TIH 3-point E                  =",self%TIH3_E)
         call show_(stdout,"TIH 3-point F                  =",self%TIH3_F)
      end if
      if (self%use_matrix_free_stiffness) then
         call show_(stdout,"Largest gain per unit lambda   =",self%gamma_max)
         call show_(stdout,"CG iterations, last solve      =",self%stiffness_cg_iterations)
      end if
      call show_(stdout,"Lambda criterion               =",trim(self%lambda_criterion))
      call show_(stdout,"Criterion at this lambda       =",criterion_value_(self))
      if (allocated(self%criterion_history)) then
         call show_(stdout,"Smallest criterion so far      =",self%criterion_min)
         call show_(stdout,"Optimum lambda so far          =",self%lambda_opt)
      end if
   end subroutine

   subroutine put_crystal_footer(self,crystal)
   ! Prints out the crystal structure factor statistics
   ! In the footer of a completed lambda calculation.
      type(SCF_DATA_TYPE), IN :: self
      type(CRYSTAL_TYPE), optional, IN :: crystal
      if (NOT self%output) return
      if (NOT is_constrained_SCF_(self)) return

!     stdout.show("Penalty in F                   =", .penalty)
      if (allocated(crystal%xray_data) AND includes_(self%SCF_kind,"xray")) then
         call put_results_(self,crystal)
         call put_F_statistics_(crystal%xray_data)
         if (crystal%r_free_percentage>0 AND allocated(crystal%xray_r_free_data)) then
            call put_F_free_stats_(crystal%xray_r_free_data)
         end if
      end if

   ! if (.crystal.pnd_data.allocated AND .SCF_kind.includes("pnd")) then
   !    .crystal.pnd_data.PUT:put_F_statistics

    !       .crystal.pnd_r_free_data.PUT:put_F_free_stats
   ! end
   end subroutine


!  =================================
!  Coupled-perturbed output routines
!  =================================

   subroutine put_CP_banner(self)
   ! Prints out the coupled-perturbed banner
      type(SCF_DATA_TYPE), IN :: self
      if (NOT self%output) return
      call flush_(stdout)
      call text_(stdout,"=====================")
      call text_(stdout,"Coupled-Perturbed SCF")
      call text_(stdout,"=====================")
      call flush_(stdout)
   end subroutine

   subroutine put_CP_options(self)
   ! Prints a summary of what CP options are being used.
      type(SCF_DATA_TYPE), IN :: self
      if (NOT self%output) return
      call show_(stdout,"No. of perturbations           =",self%n_pert)
      call flush_(stdout)
      call show_(stdout,"SCF kind                       =",self%SCF_kind)
      call flush_(stdout)
      call text_(stdout,"Termination options:")
      call flush_(stdout)
      call set_real_fortran_fmt_(stdout,"e")
      call show_(stdout,"|Delta U| convergence tol      =",self%convergence)
      call show_(stdout,"DIIS convergence tolerance     =",self%SCF_DIIS%convergence_tolerance)
      call set_real_fortran_fmt_(stdout,"f")
      call show_(stdout,"Minimum # of iterations        =",self%min_iterations)
      call show_(stdout,"Maximum # of iterations        =",self%max_iterations)
      call show_(stdout,"Using DIIS                     =",self%using_DIIS)
      if (self%using_DIIS) then
         call put_(self%SCF_DIIS)
         call flush_(stdout)
         call text_(stdout,"Note:")
         call text_(stdout,". DIIS applies separately to each RHS")
         call text_(stdout,". The DIIS error vector is (H.U - RHS)")
         call text_(stdout,". Below, notes refer only to perturbation #1")
      end if
   end subroutine

   subroutine put_CP_table_head(self)
   ! Prints out the CP iteration banner
      type(SCF_DATA_TYPE), IN :: self
   ENSURE(allocated(self%table),"SCF_DATA:put_CP_table_head ... no table created")
      if (NOT self%output) return
      call flush_(stdout)
      call text_(stdout,"====================")
      call text_(stdout,"Solve CPHF equations")
      call text_(stdout,"====================")

      ! Put the table
      call put_headings_(self%table)
   end subroutine

   subroutine put_CP_table_body_and_footer(self,flush)
   ! Print out the CP results for the current iteration. 
   ! If converged, print out footer as well.
   ! NOTE: must have called put_CP_table_head before!
      type(SCF_DATA_TYPE), INOUT :: self
      BIN, optional, IN :: flush
      if (NOT self%output) return
      if (NOT SCF_done_(self)) then

         ! Set margin notes ...
         call set_margin_info_(self)

         ! Set table body (leaky)
         if (NOT is_constrained_SCF_(self)) then
            call set_values_(self%table(1),[self%iteration])
            call set_values_(self%table(2),[self%delta])
            call set_values_(self%table(3),[self%DIIS_error])
            call set_values_(self%table(4),self%table_info)
            call force_width_(self%table(4),0)
         end if

         ! Put out the body
         if (self%iteration>0) call put_body_(self%table)

         ! Clear columns for next time (UNleaky!)
         call clear_columns_(self%table)
      else  ! the SCF is done ...
         call put_footer_(self%table)
         call flush_(stdout)
         if (is_converged_(self)) then; call text_(stdout,"Converged")
         else; call text_(stdout,"NOT Converged")
         end if
         call put_body_(self%table)
         call put_footer_(self%table,flush=flush)
      end if
   end subroutine


!  ======================
!  Cluster-charges output
!  ======================

   subroutine put_cluster(self)
   ! Put the cluster out
      type(SCF_DATA_TYPE), IN :: self
   ENSURE(allocated(self%cluster),"SCF_DATA:put_cluster ... no cluster!")
      call put_(self%cluster)
   end subroutine

   subroutine put_cluster_charges(self,name)
   ! Put the cluster charges out, for possible import
   ! into the gaussian program
      type(SCF_DATA_TYPE), IN :: self
      STR(len=*), IN :: name
      INT :: i
      REAL :: fac
   ENSURE(allocated(self%cluster_charges),"SCF_DATA:put_cluster_charges ... no cluster_charge array")
   ENSURE(allocated(self%cluster_charge_positions),"SCF_DATA:put_cluster_charges ... no cluster_charge_positions array")
      fac = conversion_factor_("angstrom")

      ! Change to a new file
      call redirect_(stdout,name)
      call flush_(stdout)
      call text_(stdout,"===============================================")
      call text_(stdout,"Cluster monopole & dipole charges and positions")
      call text_(stdout,"===============================================")
      call flush_(stdout)
      call text_(stdout,". Position (in A) is given first followed by the charge")
      call flush_(stdout)
      call text_(stdout,". The charges include at least a monopole and dipole,")
      call text_(stdout,"  the latter simulated by two close-space point charges")
      call flush_(stdout)
      call dash_(stdout,real_fields=4)
      call put_(stdout,"X")
      call put_(stdout,"Y")
      call put_(stdout,"Z")
      call put_(stdout,"Charge")
      call flush_(stdout)
      do i = 1,size(self%cluster_charges)
         call put_(stdout,self%cluster_charge_positions(1,i)*fac)
         call put_(stdout,self%cluster_charge_positions(2,i)*fac)
         call put_(stdout,self%cluster_charge_positions(3,i)*fac)
         call put_(stdout,self%cluster_charges(i))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=4)

      ! Change back
      call revert_(stdout)
   end subroutine

end module
