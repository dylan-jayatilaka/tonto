!==================================================================
!
! MOLECULE.FOCK: Make specific-types of fock matrices, J and K
! matrices, the two-electron integral archives.
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1996
! Copyright (C) Dylan Jayatilaka 2012
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
module MOLECULE_FOCK_MODULE

#  include "macros"

   include "molecule.fock.use"

   implicit none

   include "molecule.fock.int"


contains

!  =============
!  Fock matrices
!  =============

   subroutine make_Fock_mx(self,F,P,core,r12,xc)
   ! If core is present and FALSE, the core part is not computed
   ! If r12  is present and FALSE, the r12  part is not computed
   ! If xc   is present and FALSE, the XC   part is not computed
   ! NOTE: If you set core=TRUE then for ROHF, both types of 
   !       core matrix should exist. See initialize_SCF. 
   ! NOTE: If possible, use the SCF:make_scf_Fock_mx routine.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: F
      type(OPMATRIX_TYPE), IN :: P
      BIN, optional, IN :: core,r12,xc
      BIN :: direct
   ENSURE(allocated(self%SCF_data),"MOLECULE.FOCK:make_Fock_mx ... no SCF_data")
   ENSURE(has_genre_(P,spinorbital_genre_(self%SCF_data)),"MOLECULE.FOCK:make_Fock_mx ... incompatible density")
      direct = self%SCF_data%using_direct_SCF
      if (NOT direct) call get_ERI_integrals_(self)
      self%SCF_data%DFT_energy_correction = ZERO  ! Set these to zero since
      self%SCF_data%DFT_alpha_energy_correction = ZERO  ! they may be calculated
      self%SCF_data%DFT_beta_energy_correction = ZERO
      self%SCF_data%XC_energy = ZERO
      self%SCF_data%exact_exchange_energy = ZERO
      select case (self%SCF_data%SCF_kind)

      case ("rhf","xray_rhf","embedded_rhf")
         call make_r_Fock_mx_(self,F,P,direct,core,r12)

      case ("rks","xray_rks")
         call make_r_KS_Fock_mx_(self,F,P,direct,core,r12,xc)

      case ("uks","xray_uks","pnd_uks","xray_pnd_uks")
         call make_u_KS_Fock_mx_(self,F,P,core,r12,xc)

      case ("rohf")
         call make_ro_Fock_mx_(self,F,P,direct,core,r12)

      case ("uhf","xray_uhf","pnd_uhf","xray_pnd_uhf")
         call make_u_Fock_mx_(self,F,P,direct,core,r12)

      case ("gchf","xray_gchf")
         call make_gc_Fock_mx_(self,F,P,direct,core,r12)
         call add_gc_so_Fock_mx_(self,F,P)
      end select
   end subroutine

   subroutine make_r_Fock_mx(self,F,P,direct,core,r12)
   ! Make a new restricted Fock matrix "F" from the density matrix "P".
   ! If present and TRUE , "direct" means calculate integrals on the fly
   ! If present and FALSE, "core" removes the core matrix contribution
   ! If present and FALSE, "r12"  removes the two electron contribution
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: F
      type(OPMATRIX_TYPE), IN :: P
      BIN, optional, IN :: direct,core,r12

! ENSURE(F.n_bf==.n_bf, "Fock matrix dimensions wrong")
! ENSURE(P.n_bf==.n_bf, "Density matrix dimensions wrong")
      MAT(REAL,:,:), allocatable :: J,K
      BIN :: do_direct,add_core,add_r12
      REAL :: ad
      BIN :: fm
   ENSURE(allocated(P%r),"MOLECULE.FOCK:make_r_Fock_mx ... no density matrix")

      ! Recreate
      call destroy_(F,"r ")
      call create_(F,"r ")
      do_direct = TRUE
      add_core = TRUE
      add_r12 = TRUE
      if (present(direct)) do_direct = direct
      if (present(core)) add_core = core
      if (present(r12)) add_r12 = r12
      if (add_r12) then
         call create_(J,self%n_bf,self%n_bf)
         call create_(K,self%n_bf,self%n_bf)
         if (do_direct) then; call make_r_JK_(self,J,K,P%r)
         else; call make_r_JK_disk_(self,J,K,P%r)
         end if
         fm = self%SCF_data%use_Fermi_Amaldi
         ad = self%SCF_data%ee_adiabatic_constant

         ! Checked with AG 11/06/16
         ! What is AG? --dylan
         if (equals_(ad,ONE)) then
            F%r = J-HALF*K
         else
            if (fm) then; F%r = REALIFY(self%n_e-1+ad)/REALIFY(self%n_e)*J-ad*HALF*K
            else; F%r = ad*J-ad*HALF*K
            end if
         end if
         call destroy_(K)
         call destroy_(J)
      else
         F%r = ZERO
      end if

      ! Add core
      if (add_core) call plus_(F,self%core_mx)
   end subroutine

   subroutine make_r_KS_Fock_mx(self,F,P,direct,core,r12,xc)
   ! Make restricted Kohn-Sham Fock matrix "F" from density matrix "P".  
   ! If present and TRUE , "direct" means calculate integrals on the fly
   ! If present and FALSE, "core" removes the core matrix contribution
   ! If present and FALSE, "r12"  removes the two electron contribution
   ! If present and FALSE, "xc"  removes the exchange correlation part
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: F
      type(OPMATRIX_TYPE), IN :: P
      BIN, optional, IN :: direct,core,r12,xc

! ENSURE(F.n_bf==.n_bf, "Fock matrix dimensions wrong")
! ENSURE(P.n_bf==.n_bf, "Density matrix dimensions wrong")
      MAT(REAL,:,:), allocatable :: K
      BIN :: do_direct,add_core,add_r12,add_xc
   ENSURE(allocated(P%r),"MOLECULE.FOCK:make_r_KS_Fock_mx ... no density matrix")

      ! Recreate
      call destroy_(F,"r ")
      call create_(F,"r ")
      do_direct = TRUE
      add_core = TRUE
      add_r12 = TRUE
      add_xc = TRUE
      if (present(direct)) do_direct = direct
      if (present(core)) add_core = core
      if (present(r12)) add_r12 = r12
      if (present(xc)) add_xc = xc
      if (add_r12) then
         if (do_direct) then
            if (self%SCF_data%using_hybrid_exchange) then
               call create_(K,self%n_bf,self%n_bf)
               call make_r_JK_(self,F%r,K,P%r)
               K = K*(-HALF)*self%SCF_data%hybrid_exchange_factor
               ! a_0 E_x: K already carries -a_0/2, and the exchange energy is half the trace
               self%SCF_data%exact_exchange_energy = HALF*trace_product_with_(P%r,K)
               F%r = F%r+K
               call destroy_(K)
            else
               call make_r_J_engine_(self,F%r,P%r)
            end if
         else
            if (self%SCF_data%using_hybrid_exchange) then
               call create_(K,self%n_bf,self%n_bf)
               call make_r_JK_disk_(self,F%r,K,P%r)
               K = K*(-HALF)*self%SCF_data%hybrid_exchange_factor
                  ! a_0 E_x: K already carries -a_0/2, and the exchange energy is half the trace
               self%SCF_data%exact_exchange_energy = HALF*trace_product_with_(P%r,K)
               F%r = F%r+K
               call destroy_(K)
            else
               call make_r_J_disk_(self,F%r,P%r)
            end if
         end if

         ! Exchange correlation matrix
         if (add_xc) call add_XC_mx_(self,F)
      else
         F%r = ZERO
      end if

      ! Add core
      if (add_core) call plus_(self%fock_mx,self%core_mx)
   end subroutine

   subroutine make_u_Fock_mx(self,F,P,direct,core,r12)
   ! Make a new unrestricted Fock matrix.
   ! If present and TRUE , "direct" means calculate integrals on the fly
   ! If present and FALSE, "core" removes the core matrix contribution
   ! If present and FALSE, "r12"  removes the two electron contribution
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: F
      type(OPMATRIX_TYPE), IN :: P
      BIN, optional, IN :: direct,core,r12

! ENSURE(F.n_bf==.n_bf, "Fock matrix dimensions wrong")
! ENSURE(P.n_bf==.n_bf, "Density matrix dimensions wrong")
      type(OPMATRIX_TYPE), allocatable :: J,K
      BIN :: do_direct,add_core,add_r12
   ENSURE(allocated(P%a),"MOLECULE.FOCK:make_u_Fock_mx ... no density matrix")
   ENSURE(allocated(P%b),"MOLECULE.FOCK:make_u_Fock_mx ... no density matrix")
      call destroy_(F,"u ")
      call create_(F,"u ")
      do_direct = TRUE
      add_core = TRUE
      add_r12 = TRUE
      if (present(direct)) do_direct = direct
      if (present(core)) add_core = core
      if (present(r12)) add_r12 = r12
      call create_(J,self%n_bf,"u ")
      call create_(K,self%n_bf,"u ")
      if (add_r12) then
         if (do_direct) then
            call make_u_JK_(self,J,K,P)
      ! else
      !    DIE("disk UHF not implemented")
         end if
         F%a = J%a+J%b-K%a
         F%b = J%a+J%b-K%b
      else
         F%a = ZERO
         F%b = ZERO
      end if

      ! Clean
      call destroy_(K)
      call destroy_(J)

      ! Add core
      if (add_core) call plus_(self%fock_mx,self%core_mx)
   end subroutine

   subroutine make_u_KS_Fock_mx(self,F,P,core,r12,xc)
   ! Make restricted Kohn-Sham Fock matrix "F" from density matrix "P".
   ! If present and FALSE, "core" removes the core matrix contribution
   ! If present and FALSE, "r12"  removes the two electron contribution
   ! If present and FALSE, "xc"   removes the exchange correlation
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: F
      type(OPMATRIX_TYPE), IN :: P
      BIN, optional, IN :: core,r12,xc

! ENSURE(F.n_bf==.n_bf, "Fock matrix dimensions wrong")
! ENSURE(P.n_bf==.n_bf, "Density matrix dimensions wrong")
      type(OPMATRIX_TYPE), allocatable :: J,K
      BIN :: add_core,add_r12,add_xc
   ENSURE(allocated(P%a),"MOLECULE.FOCK:make_u_KS_Fock_mx ... no density matrix")
   ENSURE(allocated(P%b),"MOLECULE.FOCK:make_u_KS_Fock_mx ... no density matrix")
      call destroy_(F,"u ")
      call create_(F,"u ")
      add_core = TRUE
      add_r12 = TRUE
      add_xc = TRUE
      if (present(core)) add_core = core
      if (present(r12)) add_r12 = r12
      if (present(xc)) add_xc = xc
      call create_(J,self%n_bf,"u ")
      call create_(K,self%n_bf,"u ")
      if (add_r12) then
         if (self%SCF_data%using_RI_J AND allocated(self%RI_metric_factor) AND NOT self%SCF_data%using_hybrid_exchange) then

            ! No exact exchange: one fit of the total density serves both
            J%b = P%a+P%b
            call make_r_J_engine_(self,J%a,J%b)
            J%b = ZERO
         else
            call make_u_JK_(self,J,K,P)
         end if
         F%a = J%a+J%b
         F%b = J%a+J%b
         if (self%SCF_data%using_hybrid_exchange) then

            ! Each spin takes its own exchange, as in .make_u_Fock_mx
            F%a = F%a-self%SCF_data%hybrid_exchange_factor*K%a
            F%b = F%b-self%SCF_data%hybrid_exchange_factor*K%b
            ! a_0 E_x, with E_x = -(1/2) sum over spins of tr(P K)
            self%SCF_data%exact_exchange_energy = -HALF*self%SCF_data%hybrid_exchange_factor*(trace_product_with_(P%a,K%a)+trace_product_with_(P%b,K%b))
         end if
         if (add_xc) call add_XC_mx_(self,F)
      else
         F%a = ZERO
         F%b = ZERO
      end if

      ! Clean
      call destroy_(J)
      call destroy_(K)

      ! Add core
      if (add_core) call plus_(self%fock_mx,self%core_mx)
   end subroutine

   subroutine make_ro_Fock_mx(self,F,P,direct,core,r12)
   ! Make a new restricted open shell Fock matrix.
   ! If present and TRUE , "direct" means calculate integrals on the fly
   ! If present and FALSE, "core" removes the core matrix contribution
   ! If present and FALSE, "r12"  removes the two electron contribution
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: F
      type(OPMATRIX_TYPE), IN :: P
      BIN, optional, IN :: direct,core,r12
      MAT(REAL,:,:), allocatable :: W
   ENSURE(allocated(self%overlap_mx),"MOLECULE.FOCK:make_ro_Fock_mx ... no overlap matrix")

      ! Clean
      call destroy_(F,"r ")
      call create_(F,"r ")

      ! Make UHF Fock
      call make_u_Fock_mx_(self,F,P,direct,core,r12)

      ! Change Fa & Fb to MO basis
      call change_basis_using_(F%a,self%MOs%r)
      call change_basis_using_(F%b,self%MOs%r)

      ! Define the Fr in the MO basis
      F%r = HALF*(F%a+F%b)
      F%r(self%n_b+1:self%n_a,1:self%n_b) = F%b(self%n_b+1:self%n_a,1:self%n_b)
      F%r(1:self%n_b,self%n_b+1:self%n_a) = F%b(1:self%n_b,self%n_b+1:self%n_a)
      F%r(self%n_a+1:,self%n_b+1:self%n_a) = F%a(self%n_a+1:,self%n_b+1:self%n_a)
      F%r(self%n_b+1:self%n_a,self%n_a+1:) = F%a(self%n_b+1:self%n_a,self%n_a+1:)

      ! Change Fr back to AO basis
   ! .make_overlap_mx
      W = F%a
      call to_product_of_(W,self%MOs%r,self%overlap_mx,transpose_a=TRUE)
      call change_basis_using_(F%r,W)
   end subroutine

   subroutine make_AOC_effective_Fock_mx(self)
   ! For an atomic SCF run as the average of configuration (an open
   ! shell of n_open_orbitals holding n_open_electrons at equal
   ! occupation): .fock_mx holds F_c = h + G(D) for the ensemble density.
   ! Replace it by the effective Fock matrix of Roothaan's open-shell
   ! theory, whose off-diagonal MO blocks vanish when the energy is
   ! stationary, and set .SCF_data.ensemble_energy_correction so that
   ! 1/2 tr[D(h+F)] plus it is the average-of-configuration energy.
   !
   ! The theory. The closed shells have density D_c, occupation 2. The
   ! open shell has N_o orbitals and n electrons, each orbital with
   ! occupation 2f, f = n/(2 N_o), and density D_o. D = D_c + D_o. With
   ! G(D) = J(D) - K(D)/2 and E_2[D] = tr[D G(D)]/2, the two-electron
   ! energy averaged over every determinant of the configuration gives
   !       E = tr[D h] + E_2[D] - (1-a) E_2[D_o]
   !       a = 2 N_o (n-1) / (n (2 N_o - 1))
   ! which is the RHF energy of the fractional density less (1-a) times
   ! the self-interaction of the open shell; a = 1 for a closed shell
   ! and 0 for one electron. (Roothaan, Rev. Mod. Phys. 32, 179 (1960),
   ! with his coupling constants a = b taken for the average.) The Fock
   ! operators are F_c = h + G(D) for the closed shells and
   ! F_o = F_c - (1-a) G(D_o) for the open shell. The energy is
   ! stationary when these MO blocks vanish, with c, o, v the closed,
   ! open and virtual orbitals:
   !       (F_c)_cv = 0,   (F_o)_ov = 0,   (F_c - f F_o)_co = 0
   ! The effective Fock matrix made here has the cc, cv and vv blocks of
   ! F_c, the oo and ov blocks of F_o, and (F_c - f F_o)/(1-f) as its co
   ! block. It is Hermitian, has the three conditions as its
   ! off-diagonal blocks, and is taken back to the AO basis as
   ! S C F C^T S, as make_ro_Fock_mx does. The open shell is the Aufbau
   ! one: the orbitals after the closed ones in energy order. One open
   ! shell only, so Cr and Cu, and no atom beyond Kr.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), allocatable :: Po,Go
      MAT(REAL,:,:), allocatable :: Fc,Fo,Fe,W
      REAL :: a,f
      INT :: n_c,n_o,o1,o2,n
   ENSURE(allocated(self%SCF_data),"MOLECULE.FOCK:make_AOC_effective_Fock_mx ... no SCF_data")
   ENSURE(self%SCF_data%using_open_shell_ensemble,"MOLECULE.FOCK:make_AOC_effective_Fock_mx ... no open-shell ensemble")
   ENSURE(is_allocated_with_genre_(self%MOs,"r "),"MOLECULE.FOCK:make_AOC_effective_Fock_mx ... no MO's")
   ENSURE(has_genre_(self%fock_mx,"r "),"MOLECULE.FOCK:make_AOC_effective_Fock_mx ... no restricted Fock matrix")
   ENSURE(has_genre_(self%density_mx,"r "),"MOLECULE.FOCK:make_AOC_effective_Fock_mx ... no restricted density matrix")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.FOCK:make_AOC_effective_Fock_mx ... no overlap matrix")
      n_o = self%SCF_data%n_open_orbitals
      n_c = (self%n_e-self%SCF_data%n_open_electrons)/2
      f = self%SCF_data%n_open_electrons/(TWO*n_o)
      a = self%SCF_data%AOC_coupling
      o1 = n_c+1
      o2 = n_c+n_o
      n = self%n_bf

      ! The open-shell density D_o, and G(D_o) from a two-electron build
      call create_(Po,n)
      call create_(Po,"r ")
      call to_scaled_product_of_(Po%r,self%MOs%r(:,o1:o2),self%MOs%r(:,o1:o2),fac=TWO*f,transpose_b=TRUE)
      call create_(Go,n)
      call make_Fock_mx_(self,Go,Po,core=FALSE,xc=FALSE)

      ! F_c and F~_o = F_c - (1-a) G(D_o), to the MO basis
      Fc = self%fock_mx%r
      Fo = Fc-(ONE-a)*Go%r
      call change_basis_using_(Fc,self%MOs%r)
      call change_basis_using_(Fo,self%MOs%r)

      ! The effective Fock matrix: F_c except the open-shell rows and
      ! columns, which take F~_o, and the closed-open block (F_c - f F~_o)/(1-f)
      Fe = Fc
      Fe(o1:o2,o1:o2) = Fo(o1:o2,o1:o2)
      if (o2<n) then
         Fe(o1:o2,o2+1:n) = Fo(o1:o2,o2+1:n)
         Fe(o2+1:n,o1:o2) = Fo(o2+1:n,o1:o2)
      end if
      if (n_c>0) then
         Fe(1:n_c,o1:o2) = (Fc(1:n_c,o1:o2)-f*Fo(1:n_c,o1:o2))/(ONE-f)
         Fe(o1:o2,1:n_c) = (Fc(o1:o2,1:n_c)-f*Fo(o1:o2,1:n_c))/(ONE-f)
      end if

      ! Back to the AO basis, F = S C Fe C^T S
      call create_(W,n,n)
      call to_product_of_(W,self%MOs%r,self%overlap_mx,transpose_a=TRUE)
      call change_basis_using_(Fe,W)

      ! The energy correction: E_AOC - 1/2 tr[D(h+Fe)]
      self%SCF_data%ensemble_energy_correction = HALF*trace_product_with_(self%density_mx%r,self%fock_mx%r-Fe)-(ONE-a)*HALF*trace_product_with_(Po%r,Go%r)
      self%fock_mx%r = Fe
      call destroy_(W)
      call destroy_(Fe); call destroy_(Fo); call destroy_(Fc)
      call destroy_(Go); call destroy_(Po)
   end subroutine

   subroutine make_gc_Fock_mx(self,F,P,direct,core,r12)
   ! Make a new complex general Fock matrix.
   ! If present and TRUE , "direct" means calculate integrals on the fly
   ! If present and FALSE, "core" removes the core matrix contribution
   ! If present and FALSE, "r12"  removes the two electron contribution
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: F
      type(OPMATRIX_TYPE), IN :: P
      BIN, optional, IN :: direct,core,r12

! ENSURE(F.n_bf==.n_bf, "Fock matrix dimensions wrong")
! ENSURE(P.n_bf==.n_bf, "Density matrix dimensions wrong")
      MAT(CPX,:,:), allocatable :: Ka,Kb,Kba
      MAT(CPX,:,:), allocatable :: Pa,Pb,Pba
      MAT(REAL,:,:), allocatable :: J,Pab
      BIN :: do_direct,add_core,add_r12
   ENSURE(allocated(P%gc),"MOLECULE.FOCK:make_gc_Fock_mx ... no density matrix")
      call destroy_(F,"gc")
      call create_(F,"gc")
      do_direct = TRUE
      add_core = TRUE
      add_r12 = TRUE
      if (present(direct)) do_direct = direct
      if (present(core)) add_core = core
      if (present(r12)) add_r12 = r12
      if (add_r12) then
         call create_(J,self%n_bf,self%n_bf)
         call create_(Ka,self%n_bf,self%n_bf)
         call create_(Kb,self%n_bf,self%n_bf)
         call create_(Kba,self%n_bf,self%n_bf)
         call create_(Pa,self%n_bf,self%n_bf); call put_aa_block_to_(P%gc,Pa)
         call create_(Pb,self%n_bf,self%n_bf); call put_bb_block_to_(P%gc,Pb)
         call create_(Pba,self%n_bf,self%n_bf); call put_ba_block_to_(P%gc,Pba)
         call create_(Pab,self%n_bf,self%n_bf)
         Pab = RE(Pa)+RE(Pb)
         if (do_direct) then; call make_gc_JK_direct_(self,J,Ka,Kb,Kba,Pab,Pa,Pb,Pba)
         else; call make_gc_JK_disk_(self,J,Ka,Kb,Kba,Pab,Pa,Pb,Pba)
         end if
         call destroy_(Pab)
         call destroy_(Pba)
         call destroy_(Pb)
         call destroy_(Pa)
         Ka = J-Ka; call set_aa_block_to_(F%gc,Ka)
         Kb = J-Kb; call set_bb_block_to_(F%gc,Kb)
         Kba = -Kba; call set_ba_block_to_(F%gc,Kba)
         call destroy_(Kba)
         call destroy_(Kb)
         call destroy_(Ka)
         call destroy_(J)
      else
         F%gc = ZERO
      end if
      call make_hermitian_(F%gc)

      ! Add core
      if (add_core) call plus_(F,self%core_mx)
   end subroutine

   subroutine add_gc_so_Fock_mx(self,F,P,direct)
   ! Add in the general complex two electron spin-orbit contribution
   ! to the Fock matrix.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: F
      type(OPMATRIX_TYPE), IN :: P
      BIN, optional, IN :: direct

! ENSURE(F.n_bf==.n_bf, "Fock matrix dimensions wrong")
! ENSURE(P.n_bf==.n_bf, "Density matrix dimensions wrong")
      MAT4(CPX,:,:,:,:), allocatable :: Pgc
      MAT5(CPX,:,:,:,:,:), allocatable :: JS,KS,JO,KO
      MAT(CPX,:,:), allocatable :: Faa,Fbb,Fba
      CPX :: I
      REAL :: fac
      INT :: a,b,x,y,z
      BIN :: do_direct
   ENSURE(allocated(F%gc),"MOLECULE.FOCK:add_gc_so_Fock_mx ... no fock matrix")
   ENSURE(allocated(P%gc),"MOLECULE.FOCK:add_gc_so_Fock_mx ... no density matrix")
      do_direct = TRUE
      if (present(direct)) do_direct = direct
      if (self%SCF_data%using_2e_sl_term) then
         call get_spin_orbit_integrals_(self)
         call create_(Faa,self%n_bf,self%n_bf)
         call create_(Fbb,self%n_bf,self%n_bf)
         call create_(Fba,self%n_bf,self%n_bf)
         call create_(JS,self%n_bf,self%n_bf,2,2,3); call create_(KS,self%n_bf,self%n_bf,2,2,3)
         call create_(JO,self%n_bf,self%n_bf,2,2,3); call create_(KO,self%n_bf,self%n_bf,2,2,3)
         call create_(Pgc,self%n_bf,self%n_bf,2,2)
         call put_aa_block_to_(P%gc,Pgc(:,:,1,1))
         call put_ba_block_to_(P%gc,Pgc(:,:,2,1))
         call put_ab_block_to_(P%gc,Pgc(:,:,1,2))
         call put_bb_block_to_(P%gc,Pgc(:,:,2,2))
         if (do_direct) then
            call make_gc_so_JK_direct_(self,JS,KS,JO,KO,Pgc)
         else
            call make_gc_so_JK_disk_(self,JS(:,:,:,:,1),KS(:,:,:,:,1),JO(:,:,:,:,1),KO(:,:,:,:,1),Pgc,"x")
            call make_gc_so_JK_disk_(self,JS(:,:,:,:,2),KS(:,:,:,:,2),JO(:,:,:,:,2),KO(:,:,:,:,2),Pgc,"y")
            call make_gc_so_JK_disk_(self,JS(:,:,:,:,3),KS(:,:,:,:,3),JO(:,:,:,:,3),KO(:,:,:,:,3),Pgc,"z")
         end if
         call destroy_(Pgc)
         I = (ZERO,ONE)
         a = 1; b = 2
         x = 1; y = 2; z = 3
         Faa = THREE*JO(:,:,a,a,z)+JO(:,:,b,b,z)+THREE*JS(:,:,a,a,z)-JS(:,:,b,b,z)+TWO*JS(:,:,a,b,x)+TWO*JS(:,:,b,a,x)+JO(:,:,a,b,x)+JO(:,:,b,a,x)+I*TWO*JS(:,:,a,b,y)-I*TWO*JS(:,:,b,a,y)+I*JO(:,:,a,b,y)-I*JO(:,:,b,a,y)-TWO*KO(:,:,b,a,x)-KO(:,:,a,b,x)-TWO*KS(:,:,a,b,x)-KS(:,:,b,a,x)+I*TWO*KO(:,:,b,a,y)-I*KO(:,:,a,b,y)-I*TWO*KS(:,:,a,b,y)+I*KS(:,:,b,a,y)-THREE*KO(:,:,a,a,z)-THREE*KS(:,:,a,a,z)
         Fbb = -THREE*JO(:,:,b,b,z)-JO(:,:,a,a,z)-THREE*JS(:,:,b,b,z)+JS(:,:,a,a,z)+TWO*JS(:,:,a,b,x)+TWO*JS(:,:,b,a,x)+JO(:,:,a,b,x)+JO(:,:,b,a,x)+I*TWO*JS(:,:,a,b,y)-I*TWO*JS(:,:,b,a,y)+I*JO(:,:,a,b,y)-I*JO(:,:,b,a,y)-TWO*KO(:,:,a,b,x)-KO(:,:,b,a,x)-TWO*KS(:,:,b,a,x)-KS(:,:,a,b,x)-I*TWO*KO(:,:,a,b,y)+I*KO(:,:,b,a,y)+I*TWO*KS(:,:,b,a,y)-I*KS(:,:,a,b,y)+THREE*KO(:,:,b,b,z)+THREE*KS(:,:,b,b,z)
         Fba = TWO*JO(:,:,a,a,x)+TWO*JO(:,:,b,b,x)+JS(:,:,a,a,x)+JS(:,:,b,b,x)+I*TWO*JO(:,:,a,a,y)+I*TWO*JO(:,:,b,b,y)+I*JS(:,:,a,a,y)+I*JS(:,:,b,b,y)-TWO*KO(:,:,a,a,x)-KO(:,:,b,b,x)-TWO*KS(:,:,b,b,x)-KS(:,:,a,a,x)-I*TWO*KO(:,:,a,a,y)-I*KO(:,:,b,b,y)-I*TWO*KS(:,:,b,b,y)-I*KS(:,:,a,a,y)+KO(:,:,b,a,z)-KS(:,:,b,a,z)
         call destroy_(KO)
         call destroy_(JO)
         call destroy_(KS)
         call destroy_(JS)
         fac = G_FACTOR/(EIGHT*SPEED_OF_LIGHT_AU**2)
         fac = fac*self%SCF_data%sl_2e_factor

         ! Add in the contributions
         call aa_block_plus_(F%gc,Faa,I*fac)
         call bb_block_plus_(F%gc,Fbb,I*fac)
         call ba_block_plus_(F%gc,Fba,I*fac)
         Fbb = conjg(transpose(Fba))
         call ab_block_plus_(F%gc,Fbb,-I*fac)
         call destroy_(Fba)
         call destroy_(Fbb)
         call destroy_(Faa)
      end if
   end subroutine


!  ================================
!  Two-electron CIS/TD A.V matrices
!  ================================

   NON_MPI_PURE subroutine u_CIS_AV(self,J,K,P)
   ! Make CIS A.V Coulomb "J" and Exchange "K" Fock-like 
   ! contributions to the Davidson product. The 1st dim's 
   ! correspond to *four times* the no. of eigenvalues.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), OUT :: J
      MAT3(REAL,:,:,:), OUT :: K
      MAT3(REAL,:,:,:), IN :: P
      VEC(REAL,:), allocatable :: I
      type(SHELL1QUARTET_TYPE) :: sh4s
      type(ERI_SCRATCH_TYPE) :: work
      type(SHELL1QUARTET_TYPE) :: sh4n
      INT :: ab,cd,abcd,n
      INT :: a,b,c,d
      INT :: aa,bb,cc,dd
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      INT :: na,nb,nc,nd
      INT :: atom_a,atom_b
      INT :: atom_c,atom_d
      REAL :: factor
      REAL :: I_abcd
      BIN :: ab_same
      BIN :: any_same
   ENSURE(allocated(self%basis_shell1pair),"MOLECULE.FOCK:u_CIS_AV ... no precomputed shell pair information")
   ENSURE(is_same_shape_as_(P,J),"MOLECULE.FOCK:u_CIS_AV ... J & P, incompatible")
   ENSURE(is_same_shape_as_(P,K),"MOLECULE.FOCK:u_CIS_AV ... K & P, incompatible")
   ENSURE(size(P,2)==size(P,3),"MOLECULE.FOCK:u_CIS_AV ... P is not square in dim2 & dim3")
   ! cutoff   :: REAL
   ! max_P    :: VEC{REAL}@
   ! P_max    :: REAL
   ! IP_max   :: REAL
   ! skip     :: BIN
      J = ZERO
      K = ZERO

      ! Max (ab|ab) integrals for Schwarz test
   ! cutoff = .SCF_data.ERI_Schwarz_cutoff
   ! if (.max_I.deallocated) .initialize_max_I ! leaky

      ! Max density matrix elements
   ! max_P.create(.n_shell_pairs)
   ! .make_max_density_elements(max_P,P)
   ! P_max  = maxval(max_P)
   ! IP_max = maxval(.max_I) * P_max

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      do ab = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.FOCK:u_CIS_AV")

         ! Inexact rejection
      ! if (.max_I(ab)*IP_max < cutoff) cycle

         ! Shell quartet (ab| indices
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)

         ! Shell quartets, same-center & not
         ab_same = atom_a==atom_b
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         do cd = 1,ab

            ! Another inexact Schwarz-like test
         ! if (.max_I(ab)*.max_I(cd)*P_max < cutoff) cycle

            ! Shell quartet |cd) indices
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)

            ! Actual Schwarz test
         ! skip = ::Schwarz_inequality_test(cutoff,ab,cd,aa,bb,cc,dd,max_P,.max_I)
         ! if (skip) cycle

            ! Coincidence factor
            factor = ONE
            if (aa==bb) factor = HALF
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) factor = HALF*factor

            ! Atom coincidence
            any_same = ab_same OR atom_c==atom_d OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d

            ! Integrals
            if (any_same) then

               ! Without skips
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_ERI_into(sh4n,work)
            else

               ! Skip small contributions
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_ERI_into(sh4s,work)
            end if

            ! Fock mx contribution
            abcd = 0
            do d = fd,ld
               do c = fc,lc
                  do b = fb,lb
                     do a = fa,la
                        abcd = abcd+1
                        I_abcd = factor*work%abcd(abcd)
                        do n = 1,size(P,1)
                           J(n,a,b) = J(n,a,b)+I_abcd*(P(n,c,d)+P(n,d,c))
                           J(n,c,d) = J(n,c,d)+I_abcd*(P(n,a,b)+P(n,b,a))
                           K(n,a,c) = K(n,a,c)+I_abcd*P(n,b,d)
                           K(n,a,d) = K(n,a,d)+I_abcd*P(n,b,c)
                           K(n,b,c) = K(n,b,c)+I_abcd*P(n,a,d)
                           K(n,b,d) = K(n,b,d)+I_abcd*P(n,a,c)
                           K(n,c,a) = K(n,c,a)+I_abcd*P(n,d,b)
                           K(n,c,b) = K(n,c,b)+I_abcd*P(n,d,a)
                           K(n,d,b) = K(n,d,b)+I_abcd*P(n,c,a)
                           K(n,d,a) = K(n,d,a)+I_abcd*P(n,c,b)
                        end do
                     end do
                  end do
               end do
            end do
         end do
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.FOCK:u_CIS_AV")

   ! max_P.destroy

      ! Undo 1/2 from coincidence factor
      ! And make J symmetric
      call weight_diagonal_blocks_23_(self,J,TWO)
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM_23(J)
      else
         call symmetric_reflect_23_(J)
      end if

      ! K is the other OUT argument of this routine and was never reduced, so
      ! callers got a per-rank partial. Plain elementwise sum, not the symmetric
      ! variant: the loop fills both K(n,a,c) and K(n,c,a), so K is not
      ! triangle-only and must not be forced symmetric.
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(K)
      end if

   ! J = TWO*J
   end subroutine

   NON_MPI_PURE subroutine r_CIS_S0_AV(self,F,P)
   ! Make the *restricted* CIS S=0 singlet A.V Fock-like 
   ! matrices "F" from asymmetric density matrices "P". 
   ! The first dimension is the state index.  
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), OUT :: F
      MAT3(REAL,:,:,:), IN :: P
      MAT3(REAL,:,:,:), allocatable :: K
      VEC(REAL,:), allocatable :: I
      type(SHELL1QUARTET_TYPE) :: sh4s
      type(ERI_SCRATCH_TYPE) :: work
      type(SHELL1QUARTET_TYPE) :: sh4n
      INT :: ab,cd,abcd,n
      INT :: a,b,c,d
      INT :: aa,bb,cc,dd
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      INT :: na,nb,nc,nd
      INT :: atom_a,atom_b
      INT :: atom_c,atom_d
      REAL :: factor
      REAL :: I_abcd
      BIN :: ab_same
      BIN :: any_same
   ENSURE(allocated(self%basis_shell1pair),"MOLECULE.FOCK:r_CIS_S0_AV ... no precomputed shell pair information")
   ENSURE(is_same_shape_as_(P,F),"MOLECULE.FOCK:r_CIS_S0_AV ... F & P, incompatible")
   ENSURE(size(P,2)==size(P,3),"MOLECULE.FOCK:r_CIS_S0_AV ... P is not square in dim2 & dim3")
   ! cutoff   :: REAL
   ! max_P    :: VEC{REAL}@
   ! P_max    :: REAL
   ! IP_max   :: REAL
   ! skip     :: BIN
      F = ZERO

      ! Exchange part. Lazy.
      call create_(K,shape(F))
      K = ZERO

      ! Max (ab|ab) integrals for Schwarz test
   ! cutoff = .SCF_data.ERI_Schwarz_cutoff
   ! if (.max_I.deallocated) .initialize_max_I ! leaky

      ! Max density matrix elements
   ! max_P.create(.n_shell_pairs)
   ! .make_max_density_elements(max_P,P)
   ! P_max  = maxval(max_P)
   ! IP_max = maxval(.max_I) * P_max

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      do ab = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.FOCK:r_CIS_S0_AV")

         ! Inexact rejection
      ! if (.max_I(ab)*IP_max < cutoff) cycle

         ! Shell quartet (ab| indices
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)

         ! Shell quartets, same-center & not
         ab_same = atom_a==atom_b
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         do cd = 1,ab

            ! Another inexact Schwarz-like test
         ! if (.max_I(ab)*.max_I(cd)*P_max < cutoff) cycle

            ! Shell quartet |cd) indices
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)

            ! Actual Schwarz test
         ! skip = ::Schwarz_inequality_test(cutoff,ab,cd,aa,bb,cc,dd,max_P,.max_I)
         ! if (skip) cycle

            ! Coincidence factor
            factor = ONE
            if (aa==bb) factor = HALF
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) factor = HALF*factor

            ! Atom coincidence
            any_same = ab_same OR atom_c==atom_d OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d

            ! Integrals
            if (any_same) then

               ! Without skips
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_ERI_into(sh4n,work)
            else

               ! Skip small contributions
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_ERI_into(sh4s,work)
            end if

            ! Fock mx contribution
            abcd = 0
            do d = fd,ld
               do c = fc,lc
                  do b = fb,lb
                     do a = fa,la
                        abcd = abcd+1
                        I_abcd = factor*work%abcd(abcd)
                        do n = 1,size(P,1)
                           F(n,a,b) = F(n,a,b)+I_abcd*(P(n,c,d)+P(n,d,c))
                           F(n,c,d) = F(n,c,d)+I_abcd*(P(n,a,b)+P(n,b,a))
                           K(n,a,c) = K(n,a,c)+I_abcd*P(n,b,d)
                           K(n,a,d) = K(n,a,d)+I_abcd*P(n,b,c)
                           K(n,b,c) = K(n,b,c)+I_abcd*P(n,a,d)
                           K(n,b,d) = K(n,b,d)+I_abcd*P(n,a,c)
                           K(n,c,a) = K(n,c,a)+I_abcd*P(n,d,b)
                           K(n,c,b) = K(n,c,b)+I_abcd*P(n,d,a)
                           K(n,d,b) = K(n,d,b)+I_abcd*P(n,c,a)
                           K(n,d,a) = K(n,d,a)+I_abcd*P(n,c,b)
                        end do
                     end do
                  end do
               end do
            end do
         end do
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.FOCK:r_CIS_S0_AV")

   ! max_P.destroy

      ! Undo 1/2 from coincidence factor
      ! And make J symmetric
      call weight_diagonal_blocks_23_(self,F,TWO)
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM_23(F)
      else
         call symmetric_reflect_23_(F)
      end if

      ! K is accumulated in the same rank-partitioned loop as F but was never
      ! reduced, so F = TWO*F - K mixed a fully-reduced F with a per-rank K.
      ! A plain elementwise sum reproduces the serial K exactly (the serial path
      ! does not reflect K either).
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(K)
      end if

      ! Make F
      F = TWO*F-K

      ! Clean
      call destroy_(K)
   end subroutine

   NON_MPI_PURE subroutine r_CIS_S1_AV(self,F,P)
   ! Make the *restricted* CIS S=0 singlet A.V Fock-like 
   ! matrices "F" from asymmetric density matrices "P". 
   ! The first dimension is the state index.  
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), OUT :: F
      MAT3(REAL,:,:,:), IN :: P
      VEC(REAL,:), allocatable :: I
      type(SHELL1QUARTET_TYPE) :: sh4s
      type(ERI_SCRATCH_TYPE) :: work
      type(SHELL1QUARTET_TYPE) :: sh4n
      INT :: ab,cd,abcd,n
      INT :: a,b,c,d
      INT :: aa,bb,cc,dd
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      INT :: na,nb,nc,nd
      INT :: atom_a,atom_b
      INT :: atom_c,atom_d
      REAL :: factor
      REAL :: I_abcd
      BIN :: ab_same
      BIN :: any_same
   ENSURE(allocated(self%basis_shell1pair),"MOLECULE.FOCK:r_CIS_S1_AV ... no precomputed shell pair information")
   ENSURE(is_same_shape_as_(P,F),"MOLECULE.FOCK:r_CIS_S1_AV ... F & P, incompatible")
   ENSURE(size(P,2)==size(P,3),"MOLECULE.FOCK:r_CIS_S1_AV ... P is not square in dim2 & dim3")
   ! cutoff   :: REAL
   ! max_P    :: VEC{REAL}@
   ! P_max    :: REAL
   ! IP_max   :: REAL
   ! skip     :: BIN
      F = ZERO

      ! Max (ab|ab) integrals for Schwarz test
   ! cutoff = .SCF_data.ERI_Schwarz_cutoff
   ! if (.max_I.deallocated) .initialize_max_I ! leaky

      ! Max density matrix elements
   ! max_P.create(.n_shell_pairs)
   ! .make_max_density_elements(max_P,P)
   ! P_max  = maxval(max_P)
   ! IP_max = maxval(.max_I) * P_max

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      do ab = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.FOCK:r_CIS_S1_AV")

         ! Inexact rejection
      ! if (.max_I(ab)*IP_max < cutoff) cycle

         ! Shell quartet (ab| indices
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)

         ! Shell quartets, same-center & not
         ab_same = atom_a==atom_b
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         do cd = 1,ab

            ! Another inexact Schwarz-like test
         ! if (.max_I(ab)*.max_I(cd)*P_max < cutoff) cycle

            ! Shell quartet |cd) indices
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)

            ! Actual Schwarz test
         ! skip = ::Schwarz_inequality_test(cutoff,ab,cd,aa,bb,cc,dd,max_P,.max_I)
         ! if (skip) cycle

            ! Coincidence factor
            factor = ONE
            if (aa==bb) factor = HALF
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) factor = HALF*factor

            ! Atom coincidence
            any_same = ab_same OR atom_c==atom_d OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d

            ! Integrals
            if (any_same) then

               ! Without skips
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_ERI_into(sh4n,work)
            else

               ! Skip small contributions
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_ERI_into(sh4s,work)
            end if

            ! Fock mx contribution
            abcd = 0
            do d = fd,ld
               do c = fc,lc
                  do b = fb,lb
                     do a = fa,la
                        abcd = abcd+1
                        I_abcd = factor*work%abcd(abcd)
                        do n = 1,size(P,1)
                           F(n,a,c) = F(n,a,c)-I_abcd*P(n,b,d)
                           F(n,a,d) = F(n,a,d)-I_abcd*P(n,b,c)
                           F(n,b,c) = F(n,b,c)-I_abcd*P(n,a,d)
                           F(n,b,d) = F(n,b,d)-I_abcd*P(n,a,c)
                           F(n,c,a) = F(n,c,a)-I_abcd*P(n,d,b)
                           F(n,c,b) = F(n,c,b)-I_abcd*P(n,d,a)
                           F(n,d,b) = F(n,d,b)-I_abcd*P(n,c,a)
                           F(n,d,a) = F(n,d,a)-I_abcd*P(n,c,b)
                        end do
                     end do
                  end do
               end do
            end do
         end do
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.FOCK:r_CIS_S1_AV")

   ! max_P.destroy

      ! The shell-pair loop above is rank-partitioned, and this routine had NO
      ! reduction of any kind -- F was left as a per-rank partial. A plain
      ! elementwise sum is what is wanted here: unlike r_CIS_S0_AV, the serial
      ! path does not reflect F, so the loop already fills it completely and
      ! summing the rank partials reproduces the serial result exactly.
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(F)
      end if
   end subroutine


!   make_r_F_direct_v0(J,P) :: PURE
!   ! Make Fock matrices "J" from symmetric density matrices "P". 
!   ! The first index is a perturbation, or state index.  "J" is
!   ! the Coulomb matrix, then the exchange contribution gets 
!   ! added in, which is a bit wasteful.
!      self :: INOUT
!      J :: MAT3{REAL}, OUT
!      P :: MAT3{REAL}, IN
!
!   ENSURE(.basis_shell1pair.allocated,"no precomputed shell pair information")
!   ENSURE(P.is_same_shape_as(J),"J & P, incompatible")
!   ENSURE(P.dim2==P.dim3,"P not square in dim2 & dim3")
!
!      K :: MAT{REAL}@
!      n :: INT
!
!      K.create(.n_bf,.n_bf)
!
!      do n = 1,P.dim1
!
!         ! Make A.U (AO) -> A.U (MO) = P
!         .make_r_JK_engine(J(n,:,:),K,P(n,:,:))
!       ! J(n,:,:) = TWO*J(n,:,:) - TWO*K
!         J(n,:,:) =              - TWO*K
!
!      end
!
!      K.destroy
!
!   end

!   make_r_F_direct_v3(F,P) :: PURE
!   ! Make Fock matrices "F" from asymmetric density matrices "P". 
!   ! This version for CIS/TD and for testing: all integrals done!
!      self :: INOUT
!      F :: MAT3{REAL}, OUT
!      P :: MAT3{REAL}, IN
!
!   ENSURE(.basis_shell1pair.allocated,"no precomputed shell pair information")
!   ENSURE(P.is_same_shape_as(J),"J & P, incompatible")
!   ENSURE(P.dim2==P.dim3,"P not square in dim2 & dim3")
!
!      K :: MAT3{REAL}@
!      I, max_P   :: VEC{REAL}@
!      sh4s, sh4n :: SHELL1QUARTET
!      ab,cd, abcd, n :: INT
!      a,  b,  c,  d  :: INT
!      aa, bb, cc, dd :: INT
!      fa, fb, fc, fd :: INT
!      la, lb, lc, ld :: INT
!      na, nb, nc, nd :: INT
!      atom_a, atom_b :: INT
!      atom_c, atom_d :: INT
!      factor, cutoff :: REAL
!      P_max, IP_max  :: REAL
!      I1, I2   :: REAL
!      ab_same  :: BIN
!      any_same :: BIN
!      skip :: BIN
!
!      F = ZERO
!
!      ! Max (ab|ab) integrals for Schwarz test
!    ! cutoff = .SCF_data.ERI_Schwarz_cutoff
!    ! if (.max_I.deallocated) .initialize_max_I ! leaky
!
!      ! Max density matrix elements
!    ! max_P.create(.n_shell_pairs)
!    ! .make_max_density_elements(max_P,P)
!    ! P_max  = maxval(max_P)
!    ! IP_max = maxval(.max_I) * P_max
!
!      do aa = 1,.n_shell
!      do bb = 1,.n_shell
!
!         ! Inexact rejection
!    !    if (.max_I(ab)*IP_max < cutoff) cycle
!
!         ! Shell quartet (ab| indices
!         .set_shell2_indices_from(aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
!
!         ! Shell quartets, same-center & not
!         ab_same = atom_a==atom_b

!         .set_shell1q_ab_from(aa,bb,sh4s,skip=TRUE)
!         .set_shell1q_ab_from(aa,bb,sh4n,skip=FALSE)
!
!         do cc = 1,.n_shell
!         do dd = 1,.n_shell
!
!            ! Another inexact Schwarz-like test
!    !       if (.max_I(ab)*.max_I(cd)*P_max < cutoff) cycle
!
!            ! Shell quartet |cd) indices
!            .set_shell2_indices_from(cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
!
!            ! Actual Schwarz test
!    !       skip = ::Schwarz_inequality_test(cutoff,ab,cd,aa,bb,cc,dd,max_P,.max_I)
!    !       if (skip) cycle
!
!            ! Coincidence factor
!            factor = ONE
!          ! if (aa==bb) factor = HALF
!          ! if (cc==dd) factor = HALF * factor
!          ! if (aa==cc AND bb==dd) factor = HALF * factor
!
!            ! Atom coincidence

!               atom_b==atom_d
!
!            ! Integrals
!            if (any_same) then
!               ! Without skips
!               .set_shell1q_cd_from(cc,dd,sh4n,skip=FALSE)
!               I.create(sh4n.ab_n_bf_pairs * sh4n.cd_n_bf_pairs)
!               SHELL1QUARTET::make_ERI(sh4n,I)
!               sh4n.destroy_cd
!            else
!               ! Skip small contributions
!               .set_shell1q_cd_from(cc,dd,sh4s,skip=TRUE)
!               I.create(sh4s.ab_n_bf_pairs * sh4s.cd_n_bf_pairs)
!               SHELL1QUARTET::make_ERI(sh4s,I)
!               sh4s.destroy_cd
!            end
!
!            ! Fock mx contribution
!            abcd = 0
!            do d = fd,ld
!            do c = fc,lc
!            do b = fb,lb
!            do a = fa,la
!               abcd = abcd + 1
!               I1 = factor * I(abcd)
!               I2 = TWO*I1
!               do n = 1,P.dim1
!                  F(n,a,b) = F(n,a,b) + I2*P(n,c,d)
!                  F(n,a,c) = F(n,a,c) - I1*P(n,b,d)
!               end
!            end
!            end
!            end
!            end
!
!            I.destroy
!
!         end
!         end
!
!         ! Clean up bits
!         sh4n.destroy_ab
!         sh4s.destroy_ab
!
!      end
!      end
!
!   end

!  ===========================
!  Two-electron J & K matrices
!  ===========================

   PURE subroutine make_cartesian_density(self,P,Pc)
   ! Expand the spherical-basis density matrix "P" into the cartesian
   ! basis as "Pc". A spherical function is a fixed combination of the
   ! cartesians of its own shell, U, so this is blockwise Pc = U P U^T.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), IN :: P
      MAT(REAL,:,:), OUT :: Pc
      INT :: i,j,la,lb
      INT :: fi,li,fj,lj,gi,hi,gj,hj
      MAT(REAL,:,:), allocatable :: W,V
   ENSURE(size(Pc,1)==self%n_bf_cartesian,"MOLECULE.FOCK:make_cartesian_density ... wrong size, Pc")
      do i = 1,self%n_shell
         fi = self%first_basis_fn_for_shell(i); li = self%last_basis_fn_for_shell(i)
         gi = self%first_cartesian_bf_for_shell(i); hi = self%last_cartesian_bf_for_shell(i)
         la = self%angular_moment_for_shell(i)
         do j = 1,self%n_shell
            fj = self%first_basis_fn_for_shell(j); lj = self%last_basis_fn_for_shell(j)
            gj = self%first_cartesian_bf_for_shell(j); hj = self%last_cartesian_bf_for_shell(j)
            lb = self%angular_moment_for_shell(j)
            if (la<2) then  ! U is the identity below l=2
               call create_(W,li-fi+1,lj-fj+1)
               W = P(fi:li,fj:lj)
            else
               call create_(W,hi-gi+1,lj-fj+1)
               call to_product_of_(W,spherical_harmonics_for(la)%element,P(fi:li,fj:lj))
            end if
            if (lb<2) then
               Pc(gi:hi,gj:hj) = W
            else
               call create_(V,hi-gi+1,hj-gj+1)
               call to_product_of_(V,W,spherical_harmonics_for(lb)%element,transpose_b=TRUE)
               Pc(gi:hi,gj:hj) = V
               call destroy_(V)
            end if
            call destroy_(W)
         end do
      end do
   end subroutine

   PURE subroutine make_spherical_matrix(self,Mc,M)
   ! Contract the cartesian-basis matrix "Mc" back to the spherical
   ! basis as "M", blockwise M = U^T Mc U. The inverse operation to
   ! .make_cartesian_density, and exact for any matrix linear in the
   ! density -- which is what J and K are.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), IN :: Mc
      MAT(REAL,:,:), OUT :: M
      INT :: i,j,la,lb
      INT :: fi,li,fj,lj,gi,hi,gj,hj
      MAT(REAL,:,:), allocatable :: W,V
   ENSURE(size(Mc,1)==self%n_bf_cartesian,"MOLECULE.FOCK:make_spherical_matrix ... wrong size, Mc")
      do i = 1,self%n_shell
         fi = self%first_basis_fn_for_shell(i); li = self%last_basis_fn_for_shell(i)
         gi = self%first_cartesian_bf_for_shell(i); hi = self%last_cartesian_bf_for_shell(i)
         la = self%angular_moment_for_shell(i)
         do j = 1,self%n_shell
            fj = self%first_basis_fn_for_shell(j); lj = self%last_basis_fn_for_shell(j)
            gj = self%first_cartesian_bf_for_shell(j); hj = self%last_cartesian_bf_for_shell(j)
            lb = self%angular_moment_for_shell(j)
            if (la<2) then
               call create_(W,hi-gi+1,hj-gj+1)
               W = Mc(gi:hi,gj:hj)
            else
               call create_(W,li-fi+1,hj-gj+1)
               call to_product_of_(W,spherical_harmonics_for(la)%element,Mc(gi:hi,gj:hj),transpose_a=TRUE)
            end if
            if (lb<2) then
               M(fi:li,fj:lj) = W
            else
               call create_(V,li-fi+1,lj-fj+1)
               call to_product_of_(V,W,spherical_harmonics_for(lb)%element)
               M(fi:li,fj:lj) = V
               call destroy_(V)
            end if
            call destroy_(W)
         end do
      end do
   end subroutine

   PURE subroutine set_cartesian_shell_limits(self,first,last)
   ! Put the cartesian shell limits in place of the spherical ones, so a
   ! builder that works only in a cartesian basis can be run. The limits
   ! it replaces are kept in "first" and "last" for .reset_shell_limits.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(INT,:), allocatable, OUT :: first,last
      call create_(first,self%n_shell); first = self%first_basis_fn_for_shell
      call create_(last,self%n_shell); last = self%last_basis_fn_for_shell
      self%first_basis_fn_for_shell = self%first_cartesian_bf_for_shell
      self%last_basis_fn_for_shell = self%last_cartesian_bf_for_shell
   end subroutine

   PURE subroutine reset_shell_limits(self,first,last)
   ! Put back the shell limits kept by .set_cartesian_shell_limits.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(INT,:), allocatable, INOUT :: first,last
      self%first_basis_fn_for_shell = first
      self%last_basis_fn_for_shell = last
      call destroy_(first); call destroy_(last)
   end subroutine

   NON_MPI_PURE subroutine make_r_JK_engine(self,J,K,P)
   ! Make "J" and "K" from a symmetric density matrix "P" with the J/K
   ! engine, .make_r_JK_engine_c. A spherical basis is built in the
   ! cartesian one: J and K are linear in P, so expanding P, building,
   ! and contracting back is exact.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J,K
      MAT(REAL,:,:), IN :: P
      MAT(REAL,:,:), allocatable :: Pc,Jc,Kc
      VEC(INT,:), allocatable :: first,last
      INT :: n
      if (NOT self%use_spherical_basis) then
         call make_r_JK_engine_c_(self,J,K,P)
         return
      end if
      n = self%n_bf_cartesian
      call create_(Pc,n,n); call create_(Jc,n,n); call create_(Kc,n,n)
      call make_cartesian_density_(self,P,Pc)
      call set_cartesian_shell_limits_(self,first,last)
      call make_r_JK_engine_c_(self,Jc,Kc,Pc)
      call reset_shell_limits_(self,first,last)
      call make_spherical_matrix_(self,Jc,J)
      call make_spherical_matrix_(self,Kc,K)
      call destroy_(Kc); call destroy_(Jc); call destroy_(Pc)
   end subroutine

   NON_MPI_PURE subroutine make_r_J_engine(self,J,P)
   ! Make "J" from a symmetric density matrix "P" with the J engine,
   ! .make_r_J_engine_c, a spherical basis as in .make_r_JK_engine.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J
      MAT(REAL,:,:), IN :: P
      MAT(REAL,:,:), allocatable :: Pc,Jc
      VEC(INT,:), allocatable :: first,last
      INT :: n
      if (NOT self%use_spherical_basis) then
         call make_r_J_engine_c_(self,J,P)
         return
      end if
      n = self%n_bf_cartesian
      call create_(Pc,n,n); call create_(Jc,n,n)
      call make_cartesian_density_(self,P,Pc)
      call set_cartesian_shell_limits_(self,first,last)
      call make_r_J_engine_c_(self,Jc,Pc)
      call reset_shell_limits_(self,first,last)
      call make_spherical_matrix_(self,Jc,J)
      call destroy_(Jc); call destroy_(Pc)
   end subroutine

   NON_MPI_PURE subroutine make_u_JK_engine(self,J,K,P)
   ! Make unrestricted "J" and "K" from "P" with the J/K engine,
   ! .make_u_JK_engine_c, a spherical basis as in .make_r_JK_engine.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: J,K
      type(OPMATRIX_TYPE), IN :: P
      type(OPMATRIX_TYPE), allocatable :: Pc,Jc,Kc
      VEC(INT,:), allocatable :: first,last
      INT :: n
   ENSURE(has_genre_(J,"u "),"MOLECULE.FOCK:make_u_JK_engine ... unrestricted J not created")
   ENSURE(has_genre_(K,"u "),"MOLECULE.FOCK:make_u_JK_engine ... unrestricted K not created")
   ENSURE(has_genre_(P,"u "),"MOLECULE.FOCK:make_u_JK_engine ... unrestricted P not created")
      if (NOT self%use_spherical_basis) then
         call make_u_JK_engine_c_(self,J,K,P)
         return
      end if
      n = self%n_bf_cartesian
      call create_(Pc,n,"u "); call create_(Jc,n,"u "); call create_(Kc,n,"u ")
      call make_cartesian_density_(self,P%a,Pc%a)
      call make_cartesian_density_(self,P%b,Pc%b)
      call set_cartesian_shell_limits_(self,first,last)
      call make_u_JK_engine_c_(self,Jc,Kc,Pc)
      call reset_shell_limits_(self,first,last)
      call make_spherical_matrix_(self,Jc%a,J%a)
      call make_spherical_matrix_(self,Jc%b,J%b)
      call make_spherical_matrix_(self,Kc%a,K%a)
      call make_spherical_matrix_(self,Kc%b,K%b)
      call destroy_(Kc); call destroy_(Jc); call destroy_(Pc)
   end subroutine

   NON_MPI_PURE subroutine make_r_JK_direct(self,J,K,P)
   ! Make the real coulomb matrix "J" and exchange matrix "K" matrix
   ! from a symmetric density matrix "P" directly.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J,K
      MAT(REAL,:,:), IN :: P
      VEC(REAL,:), allocatable :: I,max_P
      type(SHELL1QUARTET_TYPE) :: sh4s,sh4n
      type(ERI_SCRATCH_TYPE) :: work
      INT :: ab,cd,abcd
      INT :: a,b,c,d
      INT :: aa,bb,cc,dd
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      INT :: na,nb,nc,nd
      INT :: atom_a,atom_b
      INT :: atom_c,atom_d
      REAL :: factor,cutoff
      REAL :: P_max,IP_max,I_abcd
      BIN :: skip,ab_same,cd_same
      BIN :: any_same,no_max_I
   ENSURE(allocated(self%basis_shell1pair),"MOLECULE.FOCK:make_r_JK_direct ... no precomputed shell pair information")
      J = ZERO
      K = ZERO
      cutoff = self%SCF_data%ERI_Schwarz_cutoff
      no_max_I = FALSE
      if (NOT allocated(self%max_I)) no_max_I = TRUE
      if (no_max_I) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call make_max_density_elements_(self,max_P,P)
      P_max = maxval(max_P)
      IP_max = maxval(self%max_I)*P_max

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      do ab = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.FOCK:make_r_JK_direct")
         if (self%max_I(ab)*IP_max<cutoff) cycle
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
         ab_same = atom_a==atom_b
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         do cd = 1,ab
            if (self%max_I(ab)*self%max_I(cd)*P_max<cutoff) cycle
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
            cd_same = atom_c==atom_d
            skip = Schwarz_inequality_test(cutoff,ab,cd,aa,bb,cc,dd,max_P,self%max_I)
            if (skip) cycle
            factor = ONE
            if (aa==bb) factor = HALF
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) factor = HALF*factor
            any_same = ab_same OR cd_same OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d
            if (any_same) then
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_ERI_into(sh4n,work)
            else
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_ERI_into(sh4s,work)
            end if
            abcd = 0
            do d = fd,ld
               do c = fc,lc
                  do b = fb,lb
                     do a = fa,la
                        abcd = abcd+1
                        I_abcd = factor*work%abcd(abcd)
                        J(a,b) = J(a,b)+I_abcd*P(d,c)
                        J(c,d) = J(c,d)+I_abcd*P(b,a)
                        K(a,c) = K(a,c)+I_abcd*P(d,b)
                        K(a,d) = K(a,d)+I_abcd*P(c,b)
                        K(b,c) = K(b,c)+I_abcd*P(d,a)
                        K(b,d) = K(b,d)+I_abcd*P(c,a)
                     end do
                  end do
               end do
            end do
         end do
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.FOCK:make_r_JK_direct")
      call destroy_(max_P)
      call weight_diagonal_blocks_(self,J,TWO)
      call blockwise_symmetric_fold_(self,K)
      J = TWO*J
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(J)
         PARALLEL_SYMMETRIC_SUM(K)
      else
         call symmetric_reflect_(J)
         call symmetric_reflect_(K)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_r_JK_engine_c(self,J,K,P)
   ! Make the restricted real coulomb matrix "J" from a symmetric
   ! density matrix "P" directly. This uses the J matrix engine, but
   ! it can be easily disabled.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J,K
      MAT(REAL,:,:), IN :: P

      ! Cartesian only: a spherical basis comes through .make_r_JK_engine.
      VEC(type(EVEC_REAL_TYPE),:), allocatable, target :: PP,JJ
      VEC(REAL,:), allocatable :: max_P
      VEC(REAL,:), PTR :: Jab,Jcd
      VEC(REAL,:), PTR :: Pab,Pcd
      type(SHELL1QUARTET_TYPE) :: sh4s,sh4n
      INT :: ab,cd,aa,bb,cc,dd,fa,fb,fc,fd,la,lb,lc,ld
      INT :: na,nb,nc,nd,atom_a,atom_b,atom_c,atom_d
      REAL :: factor,cutoff,JP_cut,KP_cut,P_max,IP_max
      BIN :: skip_J,skip_K,ab_same,cd_same,any_same,no_max_I,ab_eq_cd,list_J
      MAT(REAL,:,:), allocatable :: J_list
      type(ERI_SCRATCH_TYPE) :: work

      ! With the pair list or RI-J, J comes from .make_r_J_engine, which
      ! chooses between them, and the loop below makes K only
      list_J = self%SCF_data%using_gaussian_pair_J
      if (self%SCF_data%using_RI_J AND allocated(self%RI_metric_factor)) list_J = TRUE
      if (list_J) then
         call create_(J_list,size(J,1),size(J,2))
         call make_r_J_engine_c_(self,J_list,P)
      end if

      ! Prepare for J-matrix engine
      call reverse_transfer_(self,P,PP)
      call create_transfer_(self,JJ)

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)

      ! Schwarz test and density test cutoffs
      cutoff = self%SCF_data%ERI_Schwarz_cutoff
      JP_cut = self%SCF_data%ERI_J_density_cutoff
      KP_cut = self%SCF_data%ERI_K_density_cutoff

      ! Get Schwarz test integrals and densities. .max_I depends only on
      ! the basis and the geometry, so it is built once and kept; the
      ! geometry-change routines in MOLECULE.SET destroy it. It used to be
      ! destroyed at the end of every Fock build, one (ab|ab) pass per
      ! SCF iteration for nothing.
      no_max_I = FALSE
      if (NOT allocated(self%max_I)) no_max_I = TRUE
      if (no_max_I) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call make_max_density_elements_(self,max_P,P)
      P_max = maxval(max_P)
      IP_max = maxval(self%max_I)*P_max

      ! Initialise J, K
      J = ZERO
      K = ZERO

      ! Loop over (ab| basis function pairs
      do ab = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.FOCK:make_r_JK_engine_c")
         if (self%max_I(ab)*IP_max<cutoff) cycle
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
         ab_same = atom_a==atom_b

         ! Set primitive-pair skipped and non-skipped quartets
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)

         ! Set up (ab| J/P matrix vectors
         Jab => JJ(ab)%element
         Pab => PP(ab)%element

         ! Loop over |cd) basis function pairs
         do cd = 1,ab
            if (self%max_I(ab)*self%max_I(cd)*P_max<cutoff) cycle
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
            cd_same = atom_c==atom_d
            call Schwarz_test(skip_J,skip_K,cutoff,JP_cut,KP_cut,ab,cd,aa,bb,cc,dd,max_P,self%max_I)
            if (list_J) skip_J = TRUE
            if (skip_J AND skip_K) cycle

            ! Set up |cd) J/P matrix vectors
            Jcd => JJ(cd)%element
            Pcd => PP(cd)%element

            ! Integral (ab|cd) permutation factor
            ab_eq_cd = FALSE
            if (aa==bb) then; factor = HALF
            else; factor = ONE
            end if
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) then
               ab_eq_cd = TRUE
               factor = HALF*factor
            end if

            ! Make the J/K matrix contributions
            any_same = ab_same OR cd_same OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d
            if (any_same) then
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_r_JK_engine_(sh4n,Jab,Jcd,Pab,Pcd,K,P,skip_J,skip_K,ab_eq_cd,factor,fa,la,fb,lb,fc,lc,fd,ld,work)
            else
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_r_JK_engine_(sh4s,Jab,Jcd,Pab,Pcd,K,P,skip_J,skip_K,ab_eq_cd,factor,fa,la,fb,lb,fc,lc,fd,ld,work)
            end if
         end do
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.FOCK:make_r_JK_engine_c")
      call destroy_(max_P)

      ! Do the J matrix forward transfer
      call forward_transfer_(self,JJ,J)

      ! Add in diagonal block factors
      call weight_diagonal_blocks_(self,J,TWO)
      call blockwise_symmetric_fold_(self,K)
      J = TWO*J

      ! Parallel sums, if reqd.
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(J)
         PARALLEL_SYMMETRIC_SUM(K)
      else
         call symmetric_reflect_(J)
         call symmetric_reflect_(K)
      end if
      call destroy_(JJ)
      call destroy_(PP)
      if (list_J) then
         J = J_list
         call destroy_(J_list)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_r_J_direct(self,J,P)
   ! Make the real coulomb matrix "J" matrix from a
   ! symmetric density matrix "P" directly.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J
      MAT(REAL,:,:), IN :: P
      VEC(REAL,:), allocatable :: I,max_P
      type(SHELL1QUARTET_TYPE) :: sh4s,sh4n
      type(ERI_SCRATCH_TYPE) :: work
      INT :: ab,cd,abcd
      INT :: aa,bb,cc,dd
      INT :: a,b,c,d
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      INT :: na,nb,nc,nd
      INT :: atom_a,atom_b,atom_c,atom_d
      REAL :: factor,cutoff
      REAL :: P_max,IP_max,I_abcd
      BIN :: skip,no_max_I
      BIN :: ab_same,cd_same,any_same
      J = ZERO
      cutoff = self%SCF_data%ERI_Schwarz_cutoff
      no_max_I = FALSE
      if (NOT allocated(self%max_I)) no_max_I = TRUE
      if (no_max_I) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call make_max_density_elements_(self,max_P,P)
      P_max = maxval(max_P)
      IP_max = maxval(self%max_I)*P_max

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      do ab = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.FOCK:make_r_J_direct")
         if (self%max_I(ab)*IP_max<cutoff) cycle
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
         ab_same = atom_a==atom_b
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         do cd = 1,ab
            if (self%max_I(ab)*self%max_I(cd)*P_max<cutoff) cycle
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
            cd_same = atom_c==atom_d
            skip = Schwarz_inequality_test(cutoff,ab,cd,aa,bb,cc,dd,max_P,self%max_I)
            if (skip) cycle
            factor = ONE
            if (aa==bb) factor = HALF
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) factor = HALF*factor
            any_same = ab_same OR cd_same OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d
            if (any_same) then
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_ERI_into(sh4n,work)
            else
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_ERI_into(sh4s,work)
            end if
            abcd = 0
            do d = fd,ld
               do c = fc,lc
                  do b = fb,lb
                     do a = fa,la
                        abcd = abcd+1
                        I_abcd = factor*work%abcd(abcd)
                        J(a,b) = J(a,b)+I_abcd*P(d,c)
                        J(c,d) = J(c,d)+I_abcd*P(b,a)
                     end do
                  end do
               end do
            end do
         end do
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.FOCK:make_r_J_direct")
      call destroy_(max_P)
      call weight_diagonal_blocks_(self,J,TWO)
      J = TWO*J
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(J)
      else
         call symmetric_reflect_(J)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_u_J_engine(self,J,P)
   ! Make the alpha and beta coulomb matrix "J" from the corresponding
   ! components of the density matrix "P" directly. This uses the J
   ! matrix engine, but it can be easily disabled.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), OUT :: J
      type(OPMATRIX_TYPE), IN :: P
      call make_r_J_engine_(self,J%a,P%a)
      call make_r_J_engine_(self,J%b,P%b)
   end subroutine

   NON_MPI_PURE subroutine make_r_J_engine_c(self,J,P)
   ! Make the real coulomb matrix "J" from a symmetric density matrix
   ! "P" directly. This uses the J matrix engine, but it can be easily
   ! disabled.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J
      MAT(REAL,:,:), IN :: P

      ! Cartesian only: a spherical basis comes through .make_r_J_engine.
      VEC(type(EVEC_REAL_TYPE),:), allocatable, target :: PP,JJ
      VEC(REAL,:), allocatable :: max_P
      VEC(REAL,:), PTR :: Jab,Jcd
      VEC(REAL,:), PTR :: Pab,Pcd
      type(SHELL1QUARTET_TYPE) :: sh4s,sh4n
      INT :: ab,cd,aa,bb,cc,dd,fa,fb,fc,fd,la,lb,lc,ld
      INT :: na,nb,nc,nd,atom_a,atom_b,atom_c,atom_d
      REAL :: factor,cutoff,P_max,IP_max
      BIN :: skip,ab_same,cd_same,any_same,no_max_I,ab_eq_cd
      type(ERI_SCRATCH_TYPE) :: work

      ! Atoms in a promolecule guess have no metric, and take the exact J
      if (self%SCF_data%using_RI_J AND allocated(self%RI_metric_factor)) then
         call make_r_J_RI_(self,J,P)
         return
      end if
      if (self%SCF_data%using_gaussian_pair_J) then
         call make_r_J_gaussian_pairs_(self,J,P)
         return
      end if
      call reverse_transfer_(self,P,PP)
      call create_transfer_(self,JJ)

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      J = ZERO
      cutoff = self%SCF_data%ERI_Schwarz_cutoff
      no_max_I = FALSE
      if (NOT allocated(self%max_I)) no_max_I = TRUE
      if (no_max_I) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call make_max_density_elements_(self,max_P,P)
      P_max = maxval(max_P)
      IP_max = maxval(self%max_I)*P_max
      do ab = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.FOCK:make_r_J_engine_c")
         if (self%max_I(ab)*IP_max<cutoff) cycle
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
         ab_same = atom_a==atom_b
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         Jab => JJ(ab)%element
         Pab => PP(ab)%element
         do cd = 1,ab
            if (self%max_I(ab)*self%max_I(cd)*P_max<cutoff) cycle
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
            cd_same = atom_c==atom_d
            call Schwarz_J_test(skip,cutoff,ab,cd,max_P,self%max_I)
            if (skip) cycle
            Jcd => JJ(cd)%element
            Pcd => PP(cd)%element
            if (aa==bb) then; factor = HALF
            else; factor = ONE
            end if
            if (cc==dd) factor = HALF*factor
            ab_eq_cd = FALSE
            if (aa==cc AND bb==dd) then
               ab_eq_cd = TRUE
               factor = HALF*factor
            end if
            any_same = ab_same OR cd_same OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d
            if (any_same) then
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_r_J_engine_(sh4n,Jab,Jcd,Pab,Pcd,factor,ab_eq_cd,work)
            else
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_r_J_engine_(sh4s,Jab,Jcd,Pab,Pcd,factor,ab_eq_cd,work)
            end if
         end do
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.FOCK:make_r_J_engine_c")
      call destroy_(max_P)
      call forward_transfer_(self,JJ,J)
      call weight_diagonal_blocks_(self,J,TWO)
      J = TWO*J
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(J)
      else
         call symmetric_reflect_(J)
      end if
      call destroy_(JJ)
      call destroy_(PP)
   end subroutine

   PURE subroutine make_r_J_gaussian_pairs(self,J,P)
   ! Make the real coulomb matrix "J" from a symmetric density matrix "P"
   ! with the primitive pair list: as make_r_J_engine, with Schwarz
   ! screening per primitive pair and the pair cutoff applied to every
   ! pair of two atoms.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J
      MAT(REAL,:,:), IN :: P
      VEC(type(EVEC_REAL_TYPE),:), allocatable :: PP,JJ
      VEC(REAL,:), allocatable :: max_P
      type(GAUSSIAN_PAIR_LIST_TYPE) :: gpl
      call reverse_transfer_(self,P,PP)
      call create_transfer_(self,JJ)
      call make_gaussian_pair_list_(self,gpl)
      if (NOT allocated(self%max_I)) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call make_max_density_elements_(self,max_P,P)
      J = ZERO
      call add_to_J_(gpl,JJ,PP,self%SCF_data%ERI_Schwarz_cutoff,self%max_I,max_P)
      call forward_transfer_(self,JJ,J)
      call weight_diagonal_blocks_(self,J,TWO)
      J = TWO*J
      call symmetric_reflect_(J)
      call destroy_(JJ)
      call destroy_(PP)
   end subroutine

   PURE subroutine make_r_J_RI(self,J,P)
   ! Make the real coulomb matrix "J" from a symmetric density matrix "P"
   ! by density fitting in the auxiliary basis with the coulomb metric
   ! (RI-J): d = (Q|P), V c = d, J = (ab|Q) c. Nothing is stored; each
   ! build makes two passes over the three-center integrals.
   ! .initialize_RI_J must have been called at this geometry.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J
      MAT(REAL,:,:), IN :: P
      VEC(type(EVEC_REAL_TYPE),:), allocatable :: PP,JJ,d,c
      VEC(REAL,:), allocatable :: max_P,c_max,ds,cs
      MAT(REAL,:,:), allocatable :: U
      VEC(INT,:), allocatable :: shell_l,first_cart,first_sph
      type(GAUSSIAN_PAIR_LIST_TYPE) :: gpl,aux
      INT :: s,l,f,n,n_shell
      REAL :: cutoff
   ENSURE(allocated(self%RI_metric_factor),"MOLECULE.FOCK:make_r_J_RI ... call .initialize_RI_J first")
      call reverse_transfer_(self,P,PP)
      call create_transfer_(self,JJ)
      call make_gaussian_pair_list_(self,gpl)
      call make_auxiliary_pair_list_(self,aux,shell_l,first_cart,first_sph)
      if (NOT allocated(self%max_I)) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call make_max_density_elements_(self,max_P,P)
      n_shell = size(shell_l)
      call create_(d,n_shell)
      call create_(c,n_shell)
      do s = 1,n_shell
         n = n_comp_(shell_l(s))
         call create_(d(s)%element,n)
         call create_(c(s)%element,n)
         d(s)%element = ZERO
         c(s)%element = ZERO
      end do
      cutoff = self%SCF_data%ERI_Schwarz_cutoff

      ! The auxiliary functions with the density
      call add_to_RI_J_(gpl,aux,TRUE,d,PP,JJ,c,cutoff,self%max_I,max_P)

      ! The fitting coefficients: spherical, solve, back to cartesian
      n = size(self%RI_metric_factor,1)
      call create_(ds,n)
      call create_(cs,n)
      do s = 1,n_shell
         l = shell_l(s)
         f = first_sph(s)
         if (l<2) then
            ds(f:f+2*l) = d(s)%element
         else
            U = spherical_harmonics_for(l)%element
            ds(f:f+2*l) = matmul(d(s)%element,U)
         end if
      end do
      call solve_cholesky_equation_(self%RI_metric_factor,ds,cs)
      call create_(c_max,n_shell)
      do s = 1,n_shell
         l = shell_l(s)
         f = first_sph(s)
         if (l<2) then
            c(s)%element = cs(f:f+2*l)
         else
            U = spherical_harmonics_for(l)%element
            c(s)%element = matmul(U,cs(f:f+2*l))
         end if
         c_max(s) = maxval(abs(c(s)%element))
      end do

      ! The coulomb matrix of the fitted density
      call add_to_RI_J_(gpl,aux,FALSE,d,PP,JJ,c,cutoff,self%max_I,c_max)
      J = ZERO
      call forward_transfer_(self,JJ,J)
      call weight_diagonal_blocks_(self,J,TWO)
      call symmetric_reflect_(J)
      call destroy_(c_max)
      call destroy_(cs); call destroy_(ds)
      call destroy_(c); call destroy_(d)
      call destroy_(max_P)
      call destroy_(JJ)
      call destroy_(PP)
   end subroutine

   subroutine initialize_RI_J(self)
   ! Read the auxiliary basis sets if need be, and make
   ! .RI_metric_factor, the Cholesky factor of the coulomb metric
   ! between the spherical auxiliary functions, for this geometry.
      type(MOLECULE_TYPE), INOUT :: self
      call make_RI_metric_(self,self%RI_metric_factor)
      call to_cholesky_factor_(self%RI_metric_factor)
   end subroutine

   subroutine make_RI_metric(self,V,times,n_shell,n_cart)
   ! Make "V", the coulomb metric (P|1/r12|Q) between the spherical
   ! auxiliary functions, for this geometry, reading the auxiliary basis
   ! sets if need be. It is made in the cartesian functions by the Rys
   ! kernel on the auxiliary pair list, then transformed a shell pair at
   ! a time (.make_spherical_metric). If present, "times" returns the CPU
   ! seconds of those two steps, and "n_shell" and "n_cart" the numbers
   ! of auxiliary shells and cartesian auxiliary functions.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable, OUT :: V
      VEC(REAL,2), optional, OUT :: times
      INT, optional, OUT :: n_shell,n_cart
      MAT(REAL,:,:), allocatable :: Vc
      VEC(INT,:), allocatable :: shell_l,first_cart,first_sph
      type(GAUSSIAN_PAIR_LIST_TYPE) :: aux
      REAL :: t0,t1
      INT :: ns,nc,n_sph
      call resolve_auxiliary_bases_(self)
      call make_auxiliary_pair_list_(self,aux,shell_l,first_cart,first_sph)
      ns = size(shell_l)
      nc = first_cart(ns)+n_comp_(shell_l(ns))-1
      n_sph = first_sph(ns)+2*shell_l(ns)
      call create_(Vc,nc,nc)
      t0 = current_cpu_time()
      call make_coulomb_metric_(aux,Vc,first_cart)
      t1 = current_cpu_time()
      call create_(V,n_sph,n_sph)
      call make_spherical_metric_(self,V,Vc,shell_l,first_cart,first_sph)
      call destroy_(Vc)
      if (present(times)) times = [t1-t0,current_cpu_time()-t1]
      if (present(n_shell)) n_shell = ns
      if (present(n_cart)) n_cart = nc
   end subroutine

   PURE subroutine make_spherical_metric(self,V,Vc,shell_l,first_cart,first_sph)
   ! Make "V", the metric between the spherical auxiliary functions,
   ! from "Vc", that between the cartesian ones, a block per pair of
   ! shells: V(s,t) = U_s^T Vc(s,t) U_t, where U_l is the cartesian to
   ! spherical matrix of angular momentum l, the identity below l = 2.
   ! "shell_l", "first_cart" and "first_sph" are as from
   ! .make_auxiliary_pair_list.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: V
      MAT(REAL,:,:), IN :: Vc
      VEC(INT,:), IN :: shell_l,first_cart,first_sph
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: U
      INT :: s,t,ls,lt,cs,ct,ss,st,ncs,nct,nss,nst,l
      call create_(U,maxval(shell_l)+1)
      do l = 2,maxval(shell_l)
         U(l+1)%element = spherical_harmonics_for(l)%element
      end do
      do t = 1,size(shell_l)
         lt = shell_l(t)
         ct = first_cart(t); nct = n_comp_(lt)
         st = first_sph(t); nst = 2*lt+1
         do s = 1,size(shell_l)
            ls = shell_l(s)
            cs = first_cart(s); ncs = n_comp_(ls)
            ss = first_sph(s); nss = 2*ls+1
            if (ls<2 AND lt<2) then
               V(ss:ss+nss-1,st:st+nst-1) = Vc(cs:cs+ncs-1,ct:ct+nct-1)
            else if (ls<2) then
               V(ss:ss+nss-1,st:st+nst-1) = matmul(Vc(cs:cs+ncs-1,ct:ct+nct-1),U(lt+1)%element)
            else if (lt<2) then
               V(ss:ss+nss-1,st:st+nst-1) = matmul(transpose(U(ls+1)%element),Vc(cs:cs+ncs-1,ct:ct+nct-1))
            else
               V(ss:ss+nss-1,st:st+nst-1) = matmul(transpose(U(ls+1)%element),matmul(Vc(cs:cs+ncs-1,ct:ct+nct-1),U(lt+1)%element))
            end if
         end do
      end do
      call destroy_(U)
   end subroutine

   subroutine put_RI_metric_timings(self)
   ! Time the three steps of .initialize_RI_J for this geometry and
   ! auxiliary basis: the cartesian coulomb metric (the Rys kernel on
   ! the auxiliary pair list) and its transformation to the spherical
   ! functions, both by .make_RI_metric, and the Cholesky factor. Each
   ! step is timed three times and the least CPU time is put, on lines
   ! that begin "CPU time" so the
   ! test harness ignores them; log det V, from the Cholesky factor, is
   ! the check on the integrals. Needs only the atoms and
   ! auxiliary_basis_name=. For comparison with published timings of
   ! two-center integrals (TASKS_AND_HISTORY.md).
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: V
      VEC(REAL,3) :: t
      VEC(REAL,2) :: tt
      REAL :: t0,t1,log_det
      INT :: k,n_shell,n_cart,n_sph,rep
      t = huge(ONE)
      do rep = 1,3
         call make_RI_metric_(self,V,tt,n_shell,n_cart)
         t(1) = min(t(1),tt(1))
         t(2) = min(t(2),tt(2))
         t0 = current_cpu_time()
         call to_cholesky_factor_(V)
         t1 = current_cpu_time(); t(3) = min(t(3),t1-t0)
      end do
      n_sph = size(V,1)
      call flush_(stdout)
      call text_(stdout,"=====================================")
      call text_(stdout,"Coulomb metric of the auxiliary basis")
      call text_(stdout,"=====================================")
      call flush_(stdout)
      call show_(stdout,"Auxiliary basis                =",self%auxiliary_basis_name)
      call show_(stdout,"Auxiliary shells               =",n_shell)
      call show_(stdout,"Spherical functions            =",n_sph)
      call show_(stdout,"Cartesian functions            =",n_cart)
      log_det = ZERO
      do k = 1,n_sph
         log_det = log_det+TWO*log(V(k,k))
      end do
      call show_(stdout,"log det V                      =",log_det)
      call flush_(stdout)
      call show_(stdout,"CPU time, Rys integrals        =",t(1))
      call show_(stdout,"CPU time, to spherical         =",t(2))
      call show_(stdout,"CPU time, Cholesky factor      =",t(3))
      call show_(stdout,"CPU time, total                =",sum(t))
      call destroy_(V)
   end subroutine

   PURE subroutine make_auxiliary_pair_list(self,aux,shell_l,first_cart,first_sph)
   ! Make "aux", the auxiliary primitives as a list of one-center pairs
   ! of class (L,0), for GAUSSIAN_PAIR_LIST:add_to_RI_J. The auxiliary
   ! shells are numbered in atom order, and for each "shell_l" is its
   ! angular momentum and "first_cart" and "first_sph" its first
   ! cartesian and spherical function.
      type(MOLECULE_TYPE), IN :: self
      type(GAUSSIAN_PAIR_LIST_TYPE), INOUT :: aux
      VEC(INT,:), allocatable, OUT :: shell_l,first_cart,first_sph
      INT :: atom,b,k,g,s,m,l,l_max,n_shell,n_prim,fc,fs
      REAL :: a
   ENSURE(allocated(self%auxiliary_basis_for_atom),"MOLECULE.FOCK:make_auxiliary_pair_list ... auxiliary bases not resolved")
      n_shell = 0
      n_prim = 0
      l_max = 0
      do atom = 1,self%n_atom
         b = self%auxiliary_basis_for_atom(atom)
         n_shell = n_shell+self%auxiliary_basis(b)%n_shell
         do k = 1,self%auxiliary_basis(b)%n_shell
            n_prim = n_prim+self%auxiliary_basis(b)%shell(k)%n_cc
            l_max = max(l_max,self%auxiliary_basis(b)%shell(k)%l)
         end do
      end do
      call create_(shell_l,n_shell)
      call create_(first_cart,n_shell)
      call create_(first_sph,n_shell)
      s = 0
      fc = 1
      fs = 1
      do atom = 1,self%n_atom
         b = self%auxiliary_basis_for_atom(atom)
         do k = 1,self%auxiliary_basis(b)%n_shell
            s = s+1
            l = self%auxiliary_basis(b)%shell(k)%l
            shell_l(s) = l
            first_cart(s) = fc
            first_sph(s) = fs
            fc = fc+n_comp_(l)
            fs = fs+2*l+1
         end do
      end do
      call set_sizes_(aux,n_prim,l_max+1)
      m = 0
      do l = 0,l_max
         aux%class_first(l+1) = m+1
         aux%class_l_sum(l+1) = l
         aux%class_first_gaussian(l+1) = n_comp_up_to_(l-1)+1
         aux%class_n_gaussian(l+1) = n_comp_(l)
         s = 0
         do atom = 1,self%n_atom
            b = self%auxiliary_basis_for_atom(atom)
            do k = 1,self%auxiliary_basis(b)%n_shell
               s = s+1
               if (self%auxiliary_basis(b)%shell(k)%l/=l) cycle
               do g = 1,self%auxiliary_basis(b)%shell(k)%n_cc
                  m = m+1
                  a = self%auxiliary_basis(b)%shell(k)%exponent(g)
                  aux%shell_pair(m) = s
                  aux%factor(m) = ONE
                  aux%exponent_sum(m) = a
                  aux%exponent_inv(m) = ONE/a
                  aux%prefactor(m) = self%auxiliary_basis(b)%shell(k)%contraction(g)/(a*sqrt(a))
                  aux%center(:,m) = self%atom(atom)%position
                  aux%center_diff(:,m) = ZERO
               end do
            end do
         end do
         aux%class_last(l+1) = m
      end do
   end subroutine

   PURE subroutine make_gaussian_pair_list(self,gpl)
   ! Make the primitive pair list "gpl" for the current geometry. A pair
   ! on two atoms is kept if its gaussian product exponential is at least
   ! .SCF_data.ERI_primitive_pair_cutoff; a pair on one atom is always kept.
   ! Pairs are grouped by (l_max,l_min) class, and by shell pair within it.
      type(MOLECULE_TYPE), IN :: self
      type(GAUSSIAN_PAIR_LIST_TYPE), INOUT :: gpl
      VEC(REAL,3) :: AB,At,pos_a,pos_b,Pc
      REAL :: a,b,r2ab,ab_inv,ex,cutoff
      INT :: pass,n_classes,cls,l_max,l_min,q,aa,bb,sa,sb,atom_a,atom_b
      INT :: fa,la,na,fb,lb,nb,ag,bg,i,m
      BIN :: same
   ENSURE(allocated(self%basis_shell1pair),"MOLECULE.FOCK:make_gaussian_pair_list ... no precomputed shell pair information")
      cutoff = self%SCF_data%ERI_primitive_pair_cutoff
      n_classes = (self%basis_l_max+1)*(self%basis_l_max+2)/2

      ! Pass 1 counts the pairs kept, pass 2 stores them
      do pass = 1,2
         m = 0
         cls = 0
         do l_max = 0,self%basis_l_max
            do l_min = 0,l_max
               cls = cls+1
               if (pass==2) gpl%class_first(cls) = m+1
               do q = 1,self%n_shell_pairs
                  call set_shell2_indices_from_(self,q,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
                  sa = self%basis_shell_for_shell(aa)
                  sb = self%basis_shell_for_shell(bb)
                  if (self%basis_shell1pair(sa,sb)%l_max/=l_max) cycle
                  if (self%basis_shell1pair(sa,sb)%l_min/=l_min) cycle
                  if (pass==2) then
                     gpl%class_l_sum(cls) = self%basis_shell1pair(sa,sb)%l_sum
                     gpl%class_first_gaussian(cls) = self%basis_shell1pair(sa,sb)%first_gaussian
                     gpl%class_n_gaussian(cls) = self%basis_shell1pair(sa,sb)%n_gaussian
                  end if
                  pos_a = self%atom(atom_a)%position
                  pos_b = self%atom(atom_b)%position
                  same = atom_a==atom_b
                  if (self%basis_shell1pair(sa,sb)%a%l>self%basis_shell1pair(sa,sb)%b%l) then; At = pos_a
                  else; At = pos_b
                  end if
                  AB = pos_a-pos_b
                  r2ab = dot_product(AB,AB)
                  i = 0
                  do bg = 1,self%basis_shell1pair(sa,sb)%b%n_cc
                     b = self%basis_shell1pair(sa,sb)%b%exponent(bg)
                     do ag = 1,self%basis_shell1pair(sa,sb)%a%n_cc
                        i = i+1
                        a = self%basis_shell1pair(sa,sb)%a%exponent(ag)
                        ab_inv = self%basis_shell1pair(sa,sb)%exponent_inv(i)
                        ex = ONE
                        if (NOT same) then
                           ex = exp(-a*b*r2ab*ab_inv)
                           if (ex<cutoff) cycle
                        end if
                        m = m+1
                        if (pass==1) cycle
                        Pc = (b*pos_b+a*pos_a)*ab_inv
                        gpl%shell_pair(m) = q
                        gpl%factor(m) = ONE
                        if (aa==bb) gpl%factor(m) = HALF
                        gpl%exponent_sum(m) = self%basis_shell1pair(sa,sb)%exponent_sum(i)
                        gpl%exponent_inv(m) = ab_inv
                        gpl%prefactor(m) = ex*self%basis_shell1pair(sa,sb)%contraction_prefactor(i)
                        gpl%center(:,m) = Pc
                        gpl%center_diff(:,m) = Pc-At
                     end do
                  end do
               end do
               if (pass==2) gpl%class_last(cls) = m
            end do
         end do
         if (pass==1) call set_sizes_(gpl,m,n_classes)
      end do
   end subroutine

   subroutine make_r_JK(self,J,K,P)
   ! Make "J" and "K" for the restricted density "P", with integrals on
   ! the fly: by the J/K engine, or with .SCF_data.using_COSX, J apart
   ! and K by quadrature. Without .initialize_COSX (the atoms of a
   ! promolecule guess) the engine is used.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J,K
      MAT(REAL,:,:), IN :: P
      if (self%SCF_data%using_COSX AND allocated(self%COSX_pts)) then
         call make_r_J_engine_(self,J,P)
         call make_r_K_COSX_(self,K,P)
      else
         call make_r_JK_engine_(self,J,K,P)
      end if
   end subroutine

   subroutine make_u_JK(self,J,K,P)
   ! As .make_r_JK, for an unrestricted density: J and K of each spin.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: J,K
      type(OPMATRIX_TYPE), IN :: P
      if (self%SCF_data%using_COSX AND allocated(self%COSX_pts)) then
         call make_r_J_engine_(self,J%a,P%a)
         call make_r_J_engine_(self,J%b,P%b)
         call make_r_K_COSX_(self,K%a,P%a)
         call make_r_K_COSX_(self,K%b,P%b)
      else
         call make_u_JK_engine_(self,J,K,P)
      end if
   end subroutine

   subroutine make_r_K_COSX(self,K,P)
   ! Make the exchange matrix "K" from the symmetric density "P" by the
   ! chain-of-spheres method on the grid of .initialize_COSX. A spherical
   ! basis is taken through the cartesian functions, as
   ! .make_r_JK_engine. The quadrature index of K is then multiplied
   ! by .COSX_fit, if it exists (overlap fitting), and K is made symmetric.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: K
      MAT(REAL,:,:), IN :: P
      MAT(REAL,:,:), allocatable :: Pc,Kc,Kf
      VEC(INT,:), allocatable :: first,last
      INT :: n
   ENSURE(allocated(self%COSX_pts),"MOLECULE.FOCK:make_r_K_COSX ... call .initialize_COSX first")
      if (NOT self%use_spherical_basis) then
         call make_r_K_COSX_on_(self,K,P,self%COSX_pts,self%COSX_wts,self%COSX_batch_pt,self%COSX_batch_first,self%COSX_batch_last,self%COSX_batch_shell,self%COSX_batch_first_shell,self%COSX_batch_last_shell)
      else
         n = self%n_bf_cartesian
         call create_(Pc,n,n)
         call create_(Kc,n,n)
         call make_cartesian_density_(self,P,Pc)
         call set_cartesian_shell_limits_(self,first,last)
         call make_r_K_COSX_on_(self,Kc,Pc,self%COSX_pts,self%COSX_wts,self%COSX_batch_pt,self%COSX_batch_first,self%COSX_batch_last,self%COSX_batch_shell,self%COSX_batch_first_shell,self%COSX_batch_last_shell)
         call reset_shell_limits_(self,first,last)
         call make_spherical_matrix_(self,Kc,K)
         call destroy_(Kc); call destroy_(Pc)
      end if
      if (allocated(self%COSX_fit)) then
         call create_(Kf,size(K,1),size(K,2))
         call to_product_of_BLAS_(Kf,self%COSX_fit,K)
         K = Kf
         call destroy_(Kf)
      end if
      K = HALF*(K+transpose(K))
   end subroutine

   subroutine initialize_COSX(self,grid)
   ! Make .COSX_grid, the grid for the chain-of-spheres exchange, from
   ! "grid", and its batches, for this geometry. They are made by the XC
   ! grid routines with .COSX_grid standing in for .becke_grid; the XC
   ! grid is put back untouched.
      type(MOLECULE_TYPE), INOUT :: self
      type(BECKE_GRID_TYPE), IN :: grid
      type(BECKE_GRID_TYPE), allocatable :: xc_grid
      MAT(REAL,:,:), allocatable :: pts
      VEC(REAL,:), allocatable :: wts
      VEC(INT,:), allocatable :: index,first_pt,last_pt,b_pt,b_first,b_last,b_shell,b_first_shell,b_last_shell
      VEC(type(EVEC_INT_TYPE),:), allocatable :: overlapping
      INT :: n_unpruned
   ENSURE(allocated(self%SCF_data),"MOLECULE.FOCK:initialize_COSX ... no SCF_data")
      call destroy_COSX_grid_(self)
      call create_(self%COSX_grid)
      self%COSX_grid = grid

      ! Put the XC grid aside
      call move_alloc(self%becke_grid,xc_grid)
      call move_alloc(self%XC_pts,pts)
      call move_alloc(self%XC_wts,wts)
      call move_alloc(self%XC_index,index)
      call move_alloc(self%XC_first_pt,first_pt)
      call move_alloc(self%XC_last_pt,last_pt)
      call move_alloc(self%XC_batch_pt,b_pt)
      call move_alloc(self%XC_batch_first,b_first)
      call move_alloc(self%XC_batch_last,b_last)
      call move_alloc(self%XC_batch_shell,b_shell)
      call move_alloc(self%XC_batch_first_shell,b_first_shell)
      call move_alloc(self%XC_batch_last_shell,b_last_shell)
      call move_alloc(self%overlapping_atoms_for_atom,overlapping)
      n_unpruned = self%XC_n_unpruned

      ! The exchange grid, made as the XC grid is
      call move_alloc(self%COSX_grid,self%becke_grid)
      call set_atom_info_(self%becke_grid,self%atom)
      call set_grid_data_(self%becke_grid)
      call set_overlapping_atoms_for_atom_(self)
      call make_XC_grid_(self)
      call make_XC_batches_(self)
      call move_alloc(self%XC_pts,self%COSX_pts)
      call move_alloc(self%XC_wts,self%COSX_wts)
      call move_alloc(self%XC_batch_pt,self%COSX_batch_pt)
      call move_alloc(self%XC_batch_first,self%COSX_batch_first)
      call move_alloc(self%XC_batch_last,self%COSX_batch_last)
      call move_alloc(self%XC_batch_shell,self%COSX_batch_shell)
      call move_alloc(self%XC_batch_first_shell,self%COSX_batch_first_shell)
      call move_alloc(self%XC_batch_last_shell,self%COSX_batch_last_shell)
      call destroy_XC_grid_(self)
      call move_alloc(self%becke_grid,self%COSX_grid)

      ! Put the XC grid back
      call move_alloc(xc_grid,self%becke_grid)
      call move_alloc(pts,self%XC_pts)
      call move_alloc(wts,self%XC_wts)
      call move_alloc(index,self%XC_index)
      call move_alloc(first_pt,self%XC_first_pt)
      call move_alloc(last_pt,self%XC_last_pt)
      call move_alloc(b_pt,self%XC_batch_pt)
      call move_alloc(b_first,self%XC_batch_first)
      call move_alloc(b_last,self%XC_batch_last)
      call move_alloc(b_shell,self%XC_batch_shell)
      call move_alloc(b_first_shell,self%XC_batch_first_shell)
      call move_alloc(b_last_shell,self%XC_batch_last_shell)
      call move_alloc(overlapping,self%overlapping_atoms_for_atom)
      self%XC_n_unpruned = n_unpruned
      if (self%SCF_data%using_COSX_overlap_fitting) call make_COSX_fit_(self)
      if (self%SCF_data%show_timings) call show_(stdout,"COSX grid points               =",size(self%COSX_pts,1))
   end subroutine

   subroutine make_COSX_fit(self)
   ! Make .COSX_fit = S S_num^-1, where S_num is the overlap matrix by
   ! quadrature on .COSX_pts. Applied to the quadrature index of the
   ! COSX exchange matrix it makes the quadrature exact for every
   ! product of two basis functions (overlap fitting).
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: S_num,G0,Gw,Ss,Y
      VEC(REAL,:), allocatable :: xs,ys,zs,rhs,sol
      VEC(INT,:), allocatable :: bf
      type(SHELL1_TYPE) :: sh
      INT :: ib,f0,np,ns,q,ip,j,s,fs,ls,fa,na,off
      BIN :: fail
   ENSURE(allocated(self%COSX_pts),"MOLECULE.FOCK:make_COSX_fit ... no COSX grid")
      call make_overlap_mx_(self)
      call create_(S_num,self%n_bf,self%n_bf)
      S_num = ZERO
      do ib = 1,size(self%COSX_batch_first)
         f0 = self%COSX_batch_first(ib)
         np = self%COSX_batch_last(ib)-f0+1
         fs = self%COSX_batch_first_shell(ib)
         ls = self%COSX_batch_last_shell(ib)
         if (ls<fs) cycle
         ns = 0
         do j = fs,ls
            s = self%COSX_batch_shell(j)
            ns = ns+self%last_basis_fn_for_shell(s)-self%first_basis_fn_for_shell(s)+1
         end do
         call create_(xs,np)
         call create_(ys,np)
         call create_(zs,np)
         do q = 1,np
            ip = self%COSX_batch_pt(f0+q-1)
            xs(q) = self%COSX_pts(ip,1)
            ys(q) = self%COSX_pts(ip,2)
            zs(q) = self%COSX_pts(ip,3)
         end do
         call create_(bf,ns)
         call create_(G0,np,ns)
         off = 0
         do j = fs,ls
            s = self%COSX_batch_shell(j)
            fa = self%first_basis_fn_for_shell(s)
            na = self%last_basis_fn_for_shell(s)-fa+1
            sh = self%basis_shell1pair(self%basis_shell_for_shell(s),1)%a
            sh%position = self%atom(self%atom_for_shell(s))%position
            call make_grid_(sh,G0(:,off+1:off+na),xs,ys,zs)
            do q = 1,na
               bf(off+q) = fa+q-1
            end do
            off = off+na
         end do
         call create_(Gw,np,ns)
         do j = 1,ns
            do q = 1,np
               Gw(q,j) = self%COSX_wts(self%COSX_batch_pt(f0+q-1))*G0(q,j)
            end do
         end do
         call create_(Ss,ns,ns)
         call to_product_of_BLAS_(Ss,G0,Gw,transpose_a=TRUE)
         S_num(bf,bf) = S_num(bf,bf)+Ss
         call destroy_(Ss)
         call destroy_(Gw)
         call destroy_(G0)
         call destroy_(bf)
         call destroy_(zs)
         call destroy_(ys)
         call destroy_(xs)
      end do

      ! Y = S_num^-1 S, and S S_num^-1 is its transpose
      call to_cholesky_factor_(S_num,fail)
      DIE_IF(fail,"MOLECULE.FOCK:make_COSX_fit ... the overlap matrix on the COSX grid is not positive definite")
      call create_(Y,self%n_bf,self%n_bf)
      call create_(rhs,self%n_bf)
      call create_(sol,self%n_bf)
      do j = 1,self%n_bf
         rhs = self%overlap_mx(:,j)
         call solve_cholesky_equation_(S_num,rhs,sol)
         Y(:,j) = sol
      end do
      call destroy_(sol)
      call destroy_(rhs)
      call destroy_(self%COSX_fit)
      call create_(self%COSX_fit,self%n_bf,self%n_bf)
      self%COSX_fit = transpose(Y)
      call destroy_(Y)
      call destroy_(S_num)
   end subroutine

   subroutine make_r_K_COSX_on(self,K,P,pts,wts,batch_pt,batch_first,batch_last,batch_shell,batch_first_shell,batch_last_shell)
   ! The chain-of-spheres exchange matrix on the grid "pts", "wts",
   ! batched as MOLECULE.RHO:make_XC_batches does, in the cartesian
   ! functions: K(m,n) = sum_g w(g) X(g,m) sum_s A(g;n,s) F(g,s), not
   ! made symmetric, where X are the basis functions at the points,
   ! F = X P, and A(g;n,s) is the potential at point g of the product of
   ! functions n and s. The potentials come from the primitive pair
   ! list, one shell pair and one batch at a time, as (es| integrals that
   ! the transfer matrix of the shell pair turns into (ab|. On a batch,
   ! a shell pair is skipped when its Schwarz bound, times the largest F
   ! of its two shells there, times the largest sum over the batch of
   ! |w X| for one function, is below .SCF_data.ERI_Schwarz_cutoff: that
   ! product estimates what the pair adds to an element of K. A primitive
   ! pair is skipped likewise, by its own bound and the cutoff divided by
   ! the number of primitive pairs of its shell pair.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: K
      MAT(REAL,:,:), IN :: P
      MAT(REAL,:,:), IN :: pts
      VEC(REAL,:), IN :: wts
      VEC(INT,:), IN :: batch_pt,batch_first,batch_last,batch_shell,batch_first_shell,batch_last_shell
      type(GAUSSIAN_PAIR_LIST_TYPE) :: gpl
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: T
      VEC(INT,:), allocatable :: sp_class,sp_first,sp_last,bf
      MAT(REAL,:,:), allocatable :: G0,Gw,Ps,F,G,Ks,Ix,Iy,Iz,rt,wr,Wab,Eb
      VEC(REAL,:), allocatable :: xs,ys,zs,xv,Dx,Dy,Dz,B01,W,es,col,F_max,Qp
      type(SHELL1_TYPE) :: sh
      type(SHELL2_TYPE) :: sh2
      INT :: ib,f0,np,ns,q,ip,j,s,fs,ls,fa,la,na,fb,lb,nb,aa,bb,off,n,cls,kp,ie,i,ja,jb
      INT :: l_max,r_max,np_max,n_e,l_f,n_roots,e0
      REAL :: tv,cutoff,wF,Xw_sum
   ENSURE(size(K,1)==size(P,1),"MOLECULE.FOCK:make_r_K_COSX_on ... K and P differ in size")
      n = size(K,1)
      cutoff = self%SCF_data%ERI_Schwarz_cutoff
      if (NOT allocated(self%max_I)) call initialize_max_I_(self)
      call create_(F_max,self%n_shell)
      call make_gaussian_pair_list_(self,gpl)

      ! The bound of each primitive pair, as for the J kernel
      call create_(Qp,gpl%n_pairs)
      call make_AO_Schwarz_bounds_(gpl,Qp,self%max_I)

      ! The class and the primitives of each shell pair, which are contiguous
      call create_(sp_class,self%n_shell_pairs); sp_class = 0
      call create_(sp_first,self%n_shell_pairs); sp_first = 1
      call create_(sp_last,self%n_shell_pairs); sp_last = 0
      do cls = 1,gpl%n_classes
         do kp = gpl%class_first(cls),gpl%class_last(cls)
            q = gpl%shell_pair(kp)
            if (sp_class(q)==0) then
               sp_class(q) = cls
               sp_first(q) = kp
            end if
            sp_last(q) = kp
         end do
      end do

      ! The transfer of each shell pair as a matrix, (ab| from (es|
      call create_(T,self%n_shell_pairs)
      do q = 1,self%n_shell_pairs
         if (sp_class(q)==0) cycle
         call set_shell2_from_(self,q,sh2,fa,la,na,fb,lb,nb)
         n_e = sh2%n_gaussian
         call create_(T(q)%element,na*nb,n_e)
         call create_(es,n_e)
         call create_(Wab,na,nb)
         do ie = 1,n_e
            es = ZERO
            es(ie) = ONE
            call transfer_(sh2,es,Wab)
            T(q)%element(:,ie) = reshape(Wab,[na*nb])
         end do
         call destroy_(Wab)
         call destroy_(es)
         call destroy_ptr_part_(sh2)
      end do

      ! Work arrays for the largest batch and class
      l_max = maxval(gpl%class_l_sum)
      r_max = (l_max+2)/2
      np_max = maxval(batch_last-batch_first+1)
      call create_(xv,np_max)
      call create_(rt,r_max,np_max)
      call create_(wr,r_max,np_max)
      call create_(Ix,np_max*r_max,l_max+1)
      call create_(Iy,np_max*r_max,l_max+1)
      call create_(Iz,np_max*r_max,l_max+1)
      call create_(Dx,np_max*r_max)
      call create_(Dy,np_max*r_max)
      call create_(Dz,np_max*r_max)
      call create_(B01,np_max*r_max)
      call create_(W,np_max*r_max)
      call create_(Eb,np_max*maxval(gpl%class_n_gaussian),1)
      K = ZERO
      do ib = 1,size(batch_first)
         f0 = batch_first(ib)
         np = batch_last(ib)-f0+1
         fs = batch_first_shell(ib)
         ls = batch_last_shell(ib)
         if (ls<fs) cycle
         ns = 0
         do j = fs,ls
            s = batch_shell(j)
            ns = ns+self%last_basis_fn_for_shell(s)-self%first_basis_fn_for_shell(s)+1
         end do

         ! The cartesian basis functions on the batch
         call create_(xs,np)
         call create_(ys,np)
         call create_(zs,np)
         do q = 1,np
            ip = batch_pt(f0+q-1)
            xs(q) = pts(ip,1)
            ys(q) = pts(ip,2)
            zs(q) = pts(ip,3)
         end do
         call create_(bf,ns)
         call create_(G0,np,ns)
         off = 0
         do j = fs,ls
            s = batch_shell(j)
            fa = self%first_basis_fn_for_shell(s)
            na = self%last_basis_fn_for_shell(s)-fa+1
            sh = self%basis_shell1pair(self%basis_shell_for_shell(s),1)%a
            sh%position = self%atom(self%atom_for_shell(s))%position
            sh%is_spherical = FALSE
            call make_grid_(sh,G0(:,off+1:off+na),xs,ys,zs)
            do q = 1,na
               bf(off+q) = fa+q-1
            end do
            off = off+na
         end do

         ! F = X P
         call create_(Ps,ns,n)
         Ps = P(bf,:)
         call create_(F,np,n)
         call to_product_of_BLAS_(F,G0,Ps)
         call destroy_(Ps)

         ! The largest F of each shell on this batch
         do s = 1,self%n_shell
            F_max(s) = maxval(abs(F(:,self%first_basis_fn_for_shell(s):self%last_basis_fn_for_shell(s))))
         end do

         ! w X, and the largest sum of |w X| for one function
         call create_(Gw,np,ns)
         Xw_sum = ZERO
         do j = 1,ns
            do q = 1,np
               Gw(q,j) = wts(batch_pt(f0+q-1))*G0(q,j)
            end do
            Xw_sum = max(Xw_sum,sum(abs(Gw(:,j))))
         end do
         call destroy_(G0)

         ! G(g,n) = sum_s A(g;n,s) F(g,s)
         call create_(G,np,n)
         G = ZERO
         call create_(col,np)
         do q = 1,self%n_shell_pairs
            cls = sp_class(q)
            if (cls==0) cycle
            call set_shell2_indices_from_(self,q,aa,bb,fa,la,na,fb,lb,nb)
            wF = max(F_max(aa),F_max(bb))*Xw_sum
            if (self%max_I(q)*wF<cutoff) cycle
            n_e = gpl%class_n_gaussian(cls)
            l_f = gpl%class_l_sum(cls)
            n_roots = (l_f+2)/2

            ! Eb is E(np,n_e), column ie starting after e0 = (ie-1)*np
            call make_point_potentials_(gpl,Eb,cls,sp_first(q),sp_last(q),Qp,cutoff/(wF*(sp_last(q)-sp_first(q)+1)),np,n_roots,np*n_roots,l_f,n_e,xs,ys,zs,xv,rt,wr,Ix,Iy,Iz,Dx,Dy,Dz,B01,W)
            i = 0
            do jb = 1,nb
               do ja = 1,na
                  i = i+1
                  col = ZERO
                  do ie = 1,n_e
                     tv = T(q)%element(i,ie)
                     if (tv==ZERO) cycle
                     e0 = (ie-1)*np
                     col = col+tv*Eb(e0+1:e0+np,1)
                  end do
                  G(:,fa+ja-1) = G(:,fa+ja-1)+col*F(:,fb+jb-1)
                  if (aa/=bb) G(:,fb+jb-1) = G(:,fb+jb-1)+col*F(:,fa+ja-1)
               end do
            end do
         end do
         call destroy_(col)
         call destroy_(F)

         ! K = (w X)^T G
         call create_(Ks,ns,n)
         call to_product_of_BLAS_(Ks,Gw,G,transpose_a=TRUE)
         K(bf,:) = K(bf,:)+Ks
         call destroy_(Ks)
         call destroy_(G)
         call destroy_(Gw)
         call destroy_(bf)
         call destroy_(zs)
         call destroy_(ys)
         call destroy_(xs)
      end do
      call destroy_(Eb)
      call destroy_(Qp)
      call destroy_(F_max)
      call destroy_(W); call destroy_(B01)
      call destroy_(Dz); call destroy_(Dy); call destroy_(Dx)
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
      call destroy_(wr); call destroy_(rt); call destroy_(xv)
      call destroy_(T)
      call destroy_(sp_last); call destroy_(sp_first); call destroy_(sp_class)
   end subroutine

   subroutine put_COSX_shell_errors(self)
   ! For every atom and every radial shell of its DFT grid: the shell's
   ! contribution to the chain-of-spheres exchange energy -(1/4) tr(P K)
   ! at the angular order the pruning scheme assigns, and the error of that
   ! and of L = 5, 11, ... 59 against L = 89 on the same shell.
   ! Becke-partitioned like the real grid, with no overlap fitting. The
   ! molecular totals are compared with the exact exchange energy. Run on
   ! a converged restricted density. A diagnostic for the COSX grid
   ! (TASKS_AND_HISTORY.md); the analogue of MOLECULE.RHO:put_grid_shell_errors.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(INT,11) :: Ls
      MAT(REAL,:,:), allocatable :: P,J,Kx
      VEC(INT,:), allocatable :: first,last
      VEC(INT,:), allocatable :: overlapping_atom
      VEC(REAL,11) :: X,X_mol
      INT :: a,i,k,nr,nL,L_used,n,n_used,n_atom_pts
      REAL :: r,wr,X_used,X_used_mol,atom_err,atom_X,X_exact
      STR(len=STR_SIZE) :: head
   ENSURE(allocated(self%becke_grid),"MOLECULE.FOCK:put_COSX_shell_errors ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.FOCK:put_COSX_shell_errors ... no finalized becke_grid")
   ENSURE(is_allocated_with_genre_(self%density_mx,"r "),"MOLECULE.FOCK:put_COSX_shell_errors ... needs a restricted density")

      ! The last order is the reference
      Ls = [5,11,17,23,29,35,41,47,53,59,89]
      nL = size(Ls)
      if (NOT allocated(self%overlapping_atoms_for_atom)) call set_overlapping_atoms_for_atom_(self)

      ! The exact exchange energy, for the molecular totals
      call create_(J,self%n_bf,self%n_bf)
      call create_(Kx,self%n_bf,self%n_bf)
      call make_r_JK_engine_(self,J,Kx,self%density_mx%r)
      X_exact = -0.25d0*sum(self%density_mx%r*Kx)
      call destroy_(Kx); call destroy_(J)

      ! The quadrature works in the cartesian functions; tr(P K) is the same
      if (self%use_spherical_basis) then
         call create_(P,self%n_bf_cartesian,self%n_bf_cartesian)
         call make_cartesian_density_(self,self%density_mx%r,P)
         call set_cartesian_shell_limits_(self,first,last)
      else
         call create_(P,self%n_bf,self%n_bf)
         P = self%density_mx%r
      end if
      call flush_(stdout)
      call text_(stdout,"==================================================")
      call text_(stdout,"COSX grid: angular error per radial shell, by atom")
      call text_(stdout,"==================================================")
      call flush_(stdout)
      call text_(stdout,"X     = shell contribution to the COSX exchange energy, -(1/4) tr(P K)")
      call text_(stdout,"dX(L) = X at Lebedev order L minus X at L = "//trim(to_str_(Ls(nL)))//"; 'used' is the order the pruning scheme assigns")
      call flush_(stdout)
      head = "  shell   r/bohr   L used  n_pts         X used        dX used"
      do k = 1,nL-1
         head = trim(head)//"   dX("//trim(to_str_(Ls(k)))//")"
      end do
      X_mol = ZERO
      X_used_mol = ZERO
      do a = 1,self%n_atom
         overlapping_atom = self%overlapping_atoms_for_atom(a)%element
         nr = n_radial_pts_for_atom_(self%becke_grid,a)
         atom_err = ZERO
         atom_X = ZERO
         n_atom_pts = 0
         call flush_(stdout)
         call text_(stdout,"Atom "//trim(to_str_(a))//"  "//trim(self%atom(a)%label))
         call flush_(stdout)
         call text_(stdout,trim(head))
         do i = 1,nr
            call radial_shell_for_atom_(self%becke_grid,a,i,r,wr)
            L_used = angular_order_for_shell_(self%becke_grid,a,i)
            do k = 1,nL
               call COSX_shell_energy_(self,X(k),n,P,a,r,wr,Ls(k),overlapping_atom)
               if (Ls(k)==L_used) then
                  X_used = X(k); n_used = n
               end if
            end do
            if (all(Ls/=L_used)) call COSX_shell_energy_(self,X_used,n_used,P,a,r,wr,L_used,overlapping_atom)
            X_mol = X_mol+X
            X_used_mol = X_used_mol+X_used
            atom_err = atom_err+abs(X_used-X(nL))
            atom_X = atom_X+X_used
            n_atom_pts = n_atom_pts+n_used
            call put_(stdout,i,width=7)
            call put_(stdout,r,width=9,precision=4,fmt="f")
            call put_(stdout,L_used,width=7)
            call put_(stdout,n_used,width=7)
            call put_(stdout,X_used,width=15,precision=8,fmt="f")
            call put_(stdout,X_used-X(nL),width=11,precision=2,fmt="e")
            do k = 1,nL-1
               call put_(stdout,X(k)-X(nL),width=11,precision=2,fmt="e")
            end do
            call flush_(stdout)
         end do
         call flush_(stdout)
         call show_(stdout,"  points on this atom's grid          =",n_atom_pts)
         call show_(stdout,"  X of this atom's grid               =",atom_X)
         call show_(stdout,"  sum over shells of |dX used|        =",atom_err)
      end do
      if (self%use_spherical_basis) call reset_shell_limits_(self,first,last)
      call destroy_(P)
      call flush_(stdout)
      call show_(stdout,"Exact exchange energy                 =",X_exact)
      call show_(stdout,"COSX exchange energy, orders used     =",X_used_mol)
      call flush_(stdout)
      call text_(stdout,"      L        X_COSX - X_exact")
      do k = 1,nL
         call put_(stdout,Ls(k),width=7)
         call put_(stdout,X_mol(k)-X_exact,width=24,precision=4,fmt="e")
         call flush_(stdout)
      end do
   end subroutine

   subroutine COSX_shell_energy(self,X,n,P,a,r,wr,L,overlapping_atom)
   ! The chain-of-spheres exchange energy "X" = -(1/4) tr(P K) of the
   ! Lebedev shell of order "L", radius "r" and radial weight "wr" about
   ! atom "a", Becke-partitioned with "overlapping_atom", and its number
   ! of points "n". "P" is in the basis the shell limits describe.
      type(MOLECULE_TYPE), INOUT :: self
      REAL, OUT :: X
      INT, OUT :: n
      MAT(REAL,:,:), IN :: P
      INT, IN :: a,L
      REAL, IN :: r,wr
      VEC(INT,:), IN :: overlapping_atom
      type(LEBEDEV_TYPE), allocatable :: leb
      MAT(REAL,:,:), allocatable :: pt,K
      VEC(REAL,:), allocatable :: wt
      VEC(INT,:), allocatable :: b_pt,b_shell
      INT :: i
      call create_(leb)
      call set_l_(leb,L)
      n = leb%n_pts
      call create_(pt,n,3)
      call create_(wt,n)
      pt(:,1) = self%atom(a)%position(1)+r*leb%pts(:,1)
      pt(:,2) = self%atom(a)%position(2)+r*leb%pts(:,2)
      pt(:,3) = self%atom(a)%position(3)+r*leb%pts(:,3)
      wt = wr*leb%wts
      call destroy_(leb)
      call apply_partition_(self%becke_grid,pt,wt,a,overlapping_atom)

      ! One batch holding every point and every shell
      call create_(b_pt,n)
      do i = 1,n
         b_pt(i) = i
      end do
      call create_(b_shell,self%n_shell)
      do i = 1,self%n_shell
         b_shell(i) = i
      end do
      call create_(K,size(P,1),size(P,2))
      call make_r_K_COSX_on_(self,K,P,pt,wt,b_pt,[1],[n],b_shell,[1],[self%n_shell])
      X = -0.25d0*sum(P*K)
      call destroy_(K)
      call destroy_(b_shell)
      call destroy_(b_pt)
      call destroy_(wt)
      call destroy_(pt)
   end subroutine

   NON_MPI_PURE subroutine make_r_K_direct(self,K,P)
   ! Make the real exchange matrix "K" from a symmetric 
   ! density matrix "P" directly.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: K
      MAT(REAL,:,:), IN :: P
      VEC(REAL,:), allocatable :: I,max_P
      type(SHELL1QUARTET_TYPE) :: sh4s,sh4n
      type(ERI_SCRATCH_TYPE) :: work
      INT :: abcd
      INT :: ab,cd
      INT :: a,b,c,d
      INT :: aa,bb,cc,dd
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      INT :: na,nb,nc,nd
      INT :: atom_a,atom_b,atom_c,atom_d
      REAL :: factor,cutoff,P_max,IP_max,I_abcd
      BIN :: skip,ab_same,cd_same,any_same,no_max_I
      K = ZERO
      cutoff = self%SCF_data%ERI_Schwarz_cutoff
      no_max_I = FALSE
      if (NOT allocated(self%max_I)) no_max_I = TRUE
      if (no_max_I) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call make_max_density_elements_(self,max_P,P)
      P_max = maxval(max_P)
      IP_max = maxval(self%max_I)*P_max

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      do ab = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.FOCK:make_r_K_direct")
         if (self%max_I(ab)*IP_max<cutoff) cycle
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
         ab_same = atom_a==atom_b
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         do cd = 1,ab
            if (self%max_I(ab)*self%max_I(cd)*P_max<cutoff) cycle
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
            cd_same = atom_c==atom_d
            call Schwarz_K_test(skip,cutoff,ab,cd,aa,bb,cc,dd,max_P,self%max_I)
            if (skip) cycle

            ! Coincidence factors
            if (aa==bb) then; factor = HALF
            else; factor = ONE
            end if
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) factor = HALF*factor
            any_same = ab_same OR cd_same OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d
            if (any_same) then
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_ERI_into(sh4n,work)
            else
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_ERI_into(sh4s,work)
            end if
            abcd = 0
            do d = fd,ld
               do c = fc,lc
                  do b = fb,lb
                     do a = fa,la
                        abcd = abcd+1
                        I_abcd = factor*work%abcd(abcd)
                        K(a,c) = K(a,c)+I_abcd*P(d,b)
                        K(a,d) = K(a,d)+I_abcd*P(c,b)
                        K(b,c) = K(b,c)+I_abcd*P(d,a)
                        K(b,d) = K(b,d)+I_abcd*P(c,a)
                     end do
                  end do
               end do
            end do
         end do
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.FOCK:make_r_K_direct")
      call destroy_(max_P)
      call blockwise_symmetric_fold_(self,K)
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(K)
      else
         call symmetric_reflect_(K)
      end if
   end subroutine

   subroutine make_r_JK_disk(self,J,K,P)
   ! Make the real coulomb matrix "J" and exchange matrix "K" matrix
   ! from a symmetric density matrix "P" from disk integral archive
   ! "eri_integrals"
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J,K
      MAT(REAL,:,:), IN :: P
      type(ARCHIVE_TYPE) :: eri_archive
      type(ARCHIVE_TYPE) :: ind_archive
      MAT4(REAL,:,:,:,:), allocatable :: I
      INT :: q,a,b,c,d
      INT :: fa,la,fb,lb
      INT :: fc,lc,fd,ld
      INT :: n_quartets
      REAL :: I_abcd,P_dc,P_db,P_cb
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_r_JK_disk ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_r_JK_disk ... no atom list")
      call set_(eri_archive,self%name,"eri_integrals")
      call set_(ind_archive,self%name,"eri_index")
      ENSURE(exists_(eri_archive),"MOLECULE.FOCK:make_r_JK_disk ... no integral file")
      ENSURE(exists_(ind_archive),"MOLECULE.FOCK:make_r_JK_disk ... no integral index file")
      call open_for_(eri_archive,"read-only")
      call open_for_(ind_archive,"read-only")
      J = ZERO
      K = ZERO
      n_quartets = no_of_shell_quartets_(self%atom)
      do
         call read_(ind_archive%Bfile,q)
         if (q>n_quartets) exit
         call set_shell4_indices_from_(self,q,fa,la,fb,lb,fc,lc,fd,ld)
         call create_(I,fa,la,fb,lb,fc,lc,fd,ld)
         call read_(eri_archive%Bfile,I)
         do d = fd,ld
            do c = fc,lc
               P_dc = P(d,c)
               do b = fb,lb
                  P_db = P(d,b)
                  P_cb = P(c,b)
                  do a = fa,la
                     I_abcd = I(a,b,c,d)
                     J(a,b) = J(a,b)+I_abcd*P_dc
                     J(c,d) = J(c,d)+I_abcd*P(b,a)
                     K(a,c) = K(a,c)+I_abcd*P_db
                     K(a,d) = K(a,d)+I_abcd*P_cb
                     K(b,c) = K(b,c)+I_abcd*P(d,a)
                     K(b,d) = K(b,d)+I_abcd*P(c,a)
                  end do
               end do
            end do
         end do
         call destroy_(I)
      end do
      call close_(ind_archive)
      call close_(eri_archive)
      call weight_diagonal_blocks_(self,J,TWO)
      call blockwise_symmetric_fold_(self,K)
      call symmetric_reflect_(J)
      call symmetric_reflect_(K)
      J = TWO*J
   end subroutine

   subroutine make_r_J_disk(self,J,P)
   ! Make the real coulomb matrix "J" from a
   ! symmetric density matrix "P" from disk integral archive "eri_integrals"
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J
      MAT(REAL,:,:), IN :: P
      type(ARCHIVE_TYPE) :: eri_archive
      type(ARCHIVE_TYPE) :: ind_archive
      MAT4(REAL,:,:,:,:), allocatable :: I
      INT :: q,a,b,c,d
      INT :: fa,la,fb,lb
      INT :: fc,lc,fd,ld
      INT :: n_quartets
      REAL :: I_abcd,P_dc
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_r_J_disk ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_r_J_disk ... no atom list")
      call set_(eri_archive,self%name,"eri_integrals")
      call set_(ind_archive,self%name,"eri_index")
      ENSURE(exists_(eri_archive),"MOLECULE.FOCK:make_r_J_disk ... no integral file")
      ENSURE(exists_(ind_archive),"MOLECULE.FOCK:make_r_J_disk ... no integral index file")
      call open_for_(eri_archive,"read-only")
      call open_for_(ind_archive,"read-only")
      J = ZERO
      n_quartets = no_of_shell_quartets_(self%atom)
      do
         call read_(ind_archive%Bfile,q)
         if (q>n_quartets) exit
         call set_shell4_indices_from_(self,q,fa,la,fb,lb,fc,lc,fd,ld)
         call create_(I,fa,la,fb,lb,fc,lc,fd,ld)
         call read_(eri_archive%Bfile,I)
         do d = fd,ld
            do c = fc,lc
               P_dc = P(d,c)
               do b = fb,lb
                  do a = fa,la
                     I_abcd = I(a,b,c,d)
                     J(a,b) = J(a,b)+I_abcd*P_dc
                     J(c,d) = J(c,d)+I_abcd*P(b,a)
                  end do
               end do
            end do
         end do
         call destroy_(I)
      end do
      call close_(ind_archive)
      call close_(eri_archive)
      call weight_diagonal_blocks_(self,J,TWO)
      call symmetric_reflect_(J)
      J = TWO*J
   end subroutine

   subroutine make_r_K_disk(self,K,P)
   ! Make the real exchange matrix "K" from a symmetric density matrix "P" from
   ! disk integral archive "eri_integrals"
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: K
      MAT(REAL,:,:), IN :: P
      type(ARCHIVE_TYPE) :: eri_archive
      type(ARCHIVE_TYPE) :: ind_archive
      MAT4(REAL,:,:,:,:), allocatable :: I
      INT :: q,a,b,c,d
      INT :: fa,la,fb,lb
      INT :: fc,lc,fd,ld
      INT :: n_quartets
      REAL :: I_abcd,P_db,P_cb
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_r_K_disk ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_r_K_disk ... no atom list")
      call set_(eri_archive,self%name,"eri_integrals")
      call set_(ind_archive,self%name,"eri_index")
      ENSURE(exists_(eri_archive),"MOLECULE.FOCK:make_r_K_disk ... no integral file")
      ENSURE(exists_(ind_archive),"MOLECULE.FOCK:make_r_K_disk ... no integral index file")
      call open_for_(eri_archive,"read-only")
      call open_for_(ind_archive,"read-only")
      K = ZERO
      n_quartets = no_of_shell_quartets_(self%atom)
      do
         call read_(ind_archive%Bfile,q)
         if (q>n_quartets) exit
         call set_shell4_indices_from_(self,q,fa,la,fb,lb,fc,lc,fd,ld)
         call create_(I,fa,la,fb,lb,fc,lc,fd,ld)
         call read_(eri_archive%Bfile,I)
         do d = fd,ld
            do c = fc,lc
               do b = fb,lb
                  P_db = P(d,b)
                  P_cb = P(c,b)
                  do a = fa,la
                     I_abcd = I(a,b,c,d)
                     K(a,c) = K(a,c)+I_abcd*P_db
                     K(a,d) = K(a,d)+I_abcd*P_cb
                     K(b,c) = K(b,c)+I_abcd*P(d,a)
                     K(b,d) = K(b,d)+I_abcd*P(c,a)
                  end do
               end do
            end do
         end do
         call destroy_(I)
      end do
      call close_(ind_archive)
      call close_(eri_archive)
      call blockwise_symmetric_fold_(self,K)
      call symmetric_reflect_(K)
   end subroutine

   PURE subroutine make_r_J_nosym(self,J,P)
   ! Make the only the real coulomb matrix "J" from the
   ! density matrix "P" which has no permutational symmetry.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J
      MAT(REAL,:,:), IN :: P
      VEC(REAL,:), allocatable :: I,max_P
      type(SHELL1QUARTET_TYPE) :: sh4s,sh4n
      type(ERI_SCRATCH_TYPE) :: work
      INT :: ab,cd,abcd
      INT :: aa,bb,cc,dd
      INT :: a,b,c,d
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      INT :: na,nb,nc,nd
      INT :: atom_a,atom_b,atom_c,atom_d
      REAL :: factor,cutoff
      REAL :: P_max,IP_max,I_abcd
      BIN :: skip,no_max_I
      BIN :: ab_same,cd_same,any_same
      J = ZERO
      cutoff = self%SCF_data%ERI_disk_cutoff
      no_max_I = FALSE
      if (NOT allocated(self%max_I)) no_max_I = TRUE
      if (no_max_I) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call make_max_density_elements_(self,max_P,P)
      P_max = maxval(max_P)
      IP_max = maxval(self%max_I)*P_max

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      do ab = 1,self%n_shell_pairs
         if (self%max_I(ab)*IP_max<cutoff) cycle
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
         ab_same = atom_a==atom_b
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         do cd = 1,ab
            if (self%max_I(ab)*self%max_I(cd)*P_max<cutoff) cycle
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
            cd_same = atom_c==atom_d
            skip = Schwarz_inequality_test(cutoff,ab,cd,aa,bb,cc,dd,max_P,self%max_I)
            if (skip) cycle
            if (aa==bb) then; factor = HALF  ! coincidence factors
            else; factor = ONE
            end if
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) factor = HALF*factor
            any_same = ab_same OR cd_same OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d
            if (any_same) then
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_ERI_into(sh4n,work)
            else
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_ERI_into(sh4s,work)
            end if
            abcd = 0
            do d = fd,ld
               do c = fc,lc
                  do b = fb,lb
                     do a = fa,la
                        abcd = abcd+1
                        I_abcd = factor*work%abcd(abcd)
                        J(a,b) = J(a,b)+I_abcd*P(d,c)  ! These are symmetric
                        J(a,b) = J(a,b)+I_abcd*P(c,d)
                        J(b,a) = J(b,a)+I_abcd*P(d,c)
                        J(b,a) = J(b,a)+I_abcd*P(c,d)
                        J(c,d) = J(c,d)+I_abcd*P(b,a)
                        J(c,d) = J(c,d)+I_abcd*P(a,b)
                        J(d,c) = J(d,c)+I_abcd*P(b,a)
                        J(d,c) = J(d,c)+I_abcd*P(a,b)
                     end do
                  end do
               end do
            end do
         end do
      end do
      call destroy_(max_P)
   end subroutine

   PURE subroutine make_r_JK_nosym(self,J,K,P)
   ! Make the real coulomb matrix "J" and exchange matrix "K" matrix
   ! from the density matrix "P" which has no permutational symmetry.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J,K
      MAT(REAL,:,:), IN :: P
      VEC(REAL,:), allocatable :: I,max_P
      type(SHELL1QUARTET_TYPE) :: sh4s,sh4n
      type(ERI_SCRATCH_TYPE) :: work
      INT :: ab,cd,abcd
      INT :: aa,bb,cc,dd
      INT :: a,b,c,d
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      INT :: na,nb,nc,nd
      INT :: atom_a,atom_b,atom_c,atom_d
      REAL :: factor,cutoff
      REAL :: P_max,IP_max,I_abcd
      BIN :: skip,no_max_I
      BIN :: ab_same,cd_same
      BIN :: any_same
      J = ZERO
      K = ZERO
      cutoff = self%SCF_data%ERI_disk_cutoff
      no_max_I = FALSE
      if (NOT allocated(self%max_I)) no_max_I = TRUE
      if (no_max_I) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call make_max_density_elements_(self,max_P,P)
      P_max = maxval(max_P)
      IP_max = maxval(self%max_I)*P_max

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      do ab = 1,self%n_shell_pairs
         if (self%max_I(ab)*IP_max<cutoff) cycle
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
         ab_same = atom_a==atom_b
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         do cd = 1,ab
            if (self%max_I(ab)*self%max_I(cd)*P_max<cutoff) cycle
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
            cd_same = atom_c==atom_d
            skip = Schwarz_inequality_test(cutoff,ab,cd,aa,bb,cc,dd,max_P,self%max_I)
            if (skip) cycle
            if (aa==bb) then; factor = HALF  ! coincidence factors
            else; factor = ONE
            end if
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) factor = HALF*factor
            any_same = ab_same OR cd_same OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d
            if (any_same) then
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_ERI_into(sh4n,work)
            else
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_ERI_into(sh4s,work)
            end if
            abcd = 0
            do d = fd,ld
               do c = fc,lc
                  do b = fb,lb
                     do a = fa,la
                        abcd = abcd+1
                        I_abcd = factor*work%abcd(abcd)
                        J(a,b) = J(a,b)+I_abcd*P(d,c)  ! These are symmetric
                        J(a,b) = J(a,b)+I_abcd*P(c,d)
                        J(b,a) = J(b,a)+I_abcd*P(d,c)
                        J(b,a) = J(b,a)+I_abcd*P(c,d)
                        J(c,d) = J(c,d)+I_abcd*P(b,a)
                        J(c,d) = J(c,d)+I_abcd*P(a,b)
                        J(d,c) = J(d,c)+I_abcd*P(b,a)
                        J(d,c) = J(d,c)+I_abcd*P(a,b)
                        K(a,d) = K(a,d)+I_abcd*P(b,c)  ! These are asymmetric
                        K(a,c) = K(a,c)+I_abcd*P(b,d)
                        K(b,c) = K(b,c)+I_abcd*P(a,d)
                        K(b,d) = K(b,d)+I_abcd*P(a,c)
                        K(d,a) = K(d,a)+I_abcd*P(c,b)
                        K(c,a) = K(c,a)+I_abcd*P(d,b)
                        K(c,b) = K(c,b)+I_abcd*P(d,a)
                        K(d,b) = K(d,b)+I_abcd*P(c,a)
                     end do
                  end do
               end do
            end do
         end do
      end do
      call destroy_(max_P)
   end subroutine

   PURE subroutine make_u_JK_direct(self,J,K,P)
   ! Make the unrestricted coulomb matrices "J" and unrestricted
   ! exchange matrix "K" matrix from density matrix "P".
   ! This does not use th JK matrix engine
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: J,K
      type(OPMATRIX_TYPE), IN :: P
      MAT(REAL,:,:), allocatable :: PP
      VEC(REAL,:), allocatable :: I,max_P
      type(SHELL1QUARTET_TYPE) :: sh4s,sh4n
      type(ERI_SCRATCH_TYPE) :: work
      INT :: ab,cd,abcd
      INT :: a,b,c,d
      INT :: aa,bb,cc,dd
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      INT :: na,nb,nc,nd
      INT :: atom_a,atom_b,atom_c,atom_d
      REAL :: factor,cutoff,I_abcd
      BIN :: skip,no_max_I
      BIN :: ab_same,cd_same
      BIN :: any_same
   ENSURE(has_genre_(J,"u "),"MOLECULE.FOCK:make_u_JK_direct ... unrestricted J not created")
   ENSURE(has_genre_(K,"u "),"MOLECULE.FOCK:make_u_JK_direct ... unrestricted K not created")
   ENSURE(has_genre_(P,"u "),"MOLECULE.FOCK:make_u_JK_direct ... unrestricted P not created")
      call set_to_zero_(J)
      call set_to_zero_(K)
      cutoff = self%SCF_data%ERI_Schwarz_cutoff
      no_max_I = FALSE
      if (NOT allocated(self%max_I)) no_max_I = TRUE
      if (no_max_I) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call create_(PP,self%n_bf,self%n_bf)
      PP = P%a+P%b
      call make_max_density_elements_(self,max_P,PP)
      call destroy_(PP)

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      do ab = 1,self%n_shell_pairs
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
         ab_same = atom_a==atom_b
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         do cd = 1,ab
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
            cd_same = atom_c==atom_d

            ! (ab|cd)) < sqrt(ab|ab) * sqrt(cd|cd) test.
            skip = Schwarz_inequality_test(cutoff,ab,cd,aa,bb,cc,dd,max_P,self%max_I)
            if (skip) cycle

            ! Evaluate the integral coincidence factor
            factor = ONE
            if (aa==bb) factor = HALF
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) factor = HALF*factor
            any_same = ab_same OR cd_same OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d

            ! Calculate I = (ab|cd)
            if (any_same) then
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_ERI_into(sh4n,work)
            else
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_ERI_into(sh4s,work)
            end if

            ! Evlauate J, K from (ab|cd)
            abcd = 0
            do d = fd,ld
               do c = fc,lc
                  do b = fb,lb
                     do a = fa,la
                        abcd = abcd+1
                        I_abcd = factor*work%abcd(abcd)
                        J%a(a,b) = J%a(a,b)+I_abcd*P%a(d,c)
                        J%a(c,d) = J%a(c,d)+I_abcd*P%a(b,a)
                        K%a(a,c) = K%a(a,c)+I_abcd*P%a(d,b)
                        K%a(a,d) = K%a(a,d)+I_abcd*P%a(c,b)
                        K%a(b,c) = K%a(b,c)+I_abcd*P%a(d,a)
                        K%a(b,d) = K%a(b,d)+I_abcd*P%a(c,a)
                        J%b(a,b) = J%b(a,b)+I_abcd*P%b(d,c)
                        J%b(c,d) = J%b(c,d)+I_abcd*P%b(b,a)
                        K%b(a,c) = K%b(a,c)+I_abcd*P%b(d,b)
                        K%b(a,d) = K%b(a,d)+I_abcd*P%b(c,b)
                        K%b(b,c) = K%b(b,c)+I_abcd*P%b(d,a)
                        K%b(b,d) = K%b(b,d)+I_abcd*P%b(c,a)
                     end do
                  end do
               end do
            end do
         end do
      end do
      call destroy_(max_P)
      call weight_diagonal_blocks_(self,J%a,TWO)
      call weight_diagonal_blocks_(self,J%b,TWO)
      call blockwise_symmetric_fold_(self,K%a)
      call blockwise_symmetric_fold_(self,K%b)
      call symmetric_reflect_(J%a)
      call symmetric_reflect_(J%b)
      call symmetric_reflect_(K%a)
      call symmetric_reflect_(K%b)
      J%a = TWO*J%a
      J%b = TWO*J%b
   end subroutine

   NON_MPI_PURE subroutine make_JK_engine(self,J,K,P)
   ! Make "J" and "K" from a symmetric density matrix "P" with the J/K
   ! engine, in either basis.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: J,K
      type(OPMATRIX_TYPE), IN :: P
   ENSURE(genre_(J)==genre_(K),"MOLECULE.FOCK:make_JK_engine ... J and K have different spinorbital kinds")
   ENSURE(genre_(J)==genre_(P),"MOLECULE.FOCK:make_JK_engine ... J and P have different spinorbital kinds")
      if (has_genre_(J,"u ")) then
         call make_u_JK_engine_(self,J,K,P)
      else
         call make_r_JK_engine_(self,J%r,K%r,P%r)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_JK_direct(self,J,K,P)
   ! Make the real coulomb matrix "J" from a symmetric density matrix "P"
   ! directly. This uses the J matrix engine, but it can be easily disabled.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: J,K
      type(OPMATRIX_TYPE), IN :: P
   ENSURE(genre_(J)==genre_(K),"MOLECULE.FOCK:make_JK_direct ... J and K have different spinorbital kinds")
   ENSURE(genre_(J)==genre_(P),"MOLECULE.FOCK:make_JK_direct ... J and P have different spinorbital kinds")
      if (has_genre_(J,"u ")) then
         call make_u_JK_direct_(self,J,K,P)
      else
         call make_r_JK_direct_(self,J%r,K%r,P%r)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_J_engine(self,J,P)
   ! Make the real coulomb matrix "J" from a symmetric density matrix "P"
   ! directly. This uses the J matrix engine, but it can be easily disabled.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: J
      type(OPMATRIX_TYPE), IN :: P
   ENSURE(genre_(J)==genre_(P),"MOLECULE.FOCK:make_J_engine ... J and P have different spinorbital kinds")
      if (has_genre_(J,"u ")) then
         call make_u_J_engine_(self,J,P)
      else
         call make_r_J_engine_(self,J%r,P%r)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_u_JK_engine_c(self,J,K,P)
   ! Make the real coulomb matrix "J" from a symmetric density matrix "P"
   ! directly. This uses the J matrix engine, but it can be easily disabled.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: J,K
      type(OPMATRIX_TYPE), IN :: P
      VEC(type(EVEC_REAL_TYPE),:), allocatable, target :: PA,PB,JA,JB
      VEC(REAL,:), allocatable :: max_P
      VEC(REAL,:), PTR :: JAab,JBab,JAcd,JBcd
      VEC(REAL,:), PTR :: PAab,PBab,PAcd,PBcd
      type(SHELL1QUARTET_TYPE) :: sh4s,sh4n
      INT :: ab,cd,aa,bb,cc,dd,fa,fb,fc,fd,la,lb,lc,ld
      INT :: na,nb,nc,nd,atom_a,atom_b,atom_c,atom_d
      REAL :: factor,cutoff,JP_cut,KP_cut,P_max,IP_max
      BIN :: skip_J,skip_K,ab_same,cd_same,any_same,no_max_I,ab_eq_cd,list_J
      MAT(REAL,:,:), allocatable :: Ja_list,Jb_list
      type(ERI_SCRATCH_TYPE) :: work
   ENSURE(has_genre_(J,"u "),"MOLECULE.FOCK:make_u_JK_engine_c ... unrestricted J not created")
   ENSURE(has_genre_(K,"u "),"MOLECULE.FOCK:make_u_JK_engine_c ... unrestricted K not created")
   ENSURE(has_genre_(P,"u "),"MOLECULE.FOCK:make_u_JK_engine_c ... unrestricted P not created")

      ! As in .make_r_JK_engine: J apart, and the loop below makes K only
      list_J = self%SCF_data%using_gaussian_pair_J
      if (self%SCF_data%using_RI_J AND allocated(self%RI_metric_factor)) list_J = TRUE
      if (list_J) then
         call create_(Ja_list,size(J%a,1),size(J%a,2))
         call create_(Jb_list,size(J%b,1),size(J%b,2))
         call make_r_J_engine_c_(self,Ja_list,P%a)
         call make_r_J_engine_c_(self,Jb_list,P%b)
      end if

      ! Prepare for J-matrix engine
      call reverse_transfer_(self,P%a,PA)
      call reverse_transfer_(self,P%b,PB)
      call create_transfer_(self,JA)
      call create_transfer_(self,JB)

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)

      ! Schwarz test and density test cutoffs
      cutoff = self%SCF_data%ERI_Schwarz_cutoff
      JP_cut = self%SCF_data%ERI_J_density_cutoff
      KP_cut = self%SCF_data%ERI_K_density_cutoff

      ! Get Schwarz test integrals and densities
      no_max_I = FALSE
      if (NOT allocated(self%max_I)) no_max_I = TRUE
      if (no_max_I) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call make_max_density_elements_(self,max_P,P%a,P%b)
      P_max = maxval(max_P)
      IP_max = maxval(self%max_I)*P_max

      ! Initialise J, K
      J%a = ZERO; J%b = ZERO
      K%a = ZERO; K%b = ZERO

      ! Loop over (ab| basis function pairs
      do ab = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.FOCK:make_u_JK_engine_c")
         if (self%max_I(ab)*IP_max<cutoff) cycle
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
         ab_same = atom_a==atom_b

         ! Set primitive-pair skipped and non-skipped quartets
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)

         ! Set up (ab| J/P matrix vectors
         JAab => JA(ab)%element; JBab => JB(ab)%element
         PAab => PA(ab)%element; PBab => PB(ab)%element

         ! Loop over |cd) basis function pairs
         do cd = 1,ab
            if (self%max_I(ab)*self%max_I(cd)*P_max<cutoff) cycle
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
            cd_same = atom_c==atom_d
            call Schwarz_test(skip_J,skip_K,cutoff,JP_cut,KP_cut,ab,cd,aa,bb,cc,dd,max_P,self%max_I)
            if (list_J) skip_J = TRUE
            if (skip_J AND skip_K) cycle

            ! Set up |cd) J/P matrix vectors
            JAcd => JA(cd)%element; JBcd => JB(cd)%element
            PAcd => PA(cd)%element; PBcd => PB(cd)%element

            ! Integral (ab|cd) permutation factor
            ab_eq_cd = FALSE
            if (aa==bb) then; factor = HALF
            else; factor = ONE
            end if
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) then
               ab_eq_cd = TRUE
               factor = HALF*factor
            end if

            ! Make the J/K matrix contributions
            any_same = ab_same OR cd_same OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d
            if (any_same) then
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_r_JK_engine_(sh4n,JAab,JBab,JAcd,JBcd,PAab,PBab,PAcd,PBcd,K%a,K%b,P%a,P%b,skip_J,skip_K,ab_eq_cd,factor,fa,la,fb,lb,fc,lc,fd,ld,work)
            else
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_r_JK_engine_(sh4s,JAab,JBab,JAcd,JBcd,PAab,PBab,PAcd,PBcd,K%a,K%b,P%a,P%b,skip_J,skip_K,ab_eq_cd,factor,fa,la,fb,lb,fc,lc,fd,ld,work)
            end if
         end do
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.FOCK:make_u_JK_engine_c")
      call destroy_(max_P)

      ! Do the J matrix forward transfer
      call forward_transfer_(self,JA,J%a)
      call forward_transfer_(self,JB,J%b)

      ! Add in diagonal block factors
      call weight_diagonal_blocks_(self,J%a,TWO)
      call weight_diagonal_blocks_(self,J%b,TWO)
      call blockwise_symmetric_fold_(self,K%a)
      call blockwise_symmetric_fold_(self,K%b)
      J%a = TWO*J%a
      J%b = TWO*J%b

      ! Parallel sums, if reqd.
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(J%a)
         PARALLEL_SYMMETRIC_SUM(J%b)
         PARALLEL_SYMMETRIC_SUM(K%a)
         PARALLEL_SYMMETRIC_SUM(K%b)
      else
         call symmetric_reflect_(J%a)
         call symmetric_reflect_(J%b)
         call symmetric_reflect_(K%a)
         call symmetric_reflect_(K%b)
      end if

      ! Clean up transfer arrays
      call destroy_(JB); call destroy_(JA)
      call destroy_(PB); call destroy_(PA)
      if (list_J) then
         J%a = Ja_list
         J%b = Jb_list
         call destroy_(Jb_list)
         call destroy_(Ja_list)
      end if
   end subroutine

   subroutine make_u_JK_disk(self,J,Ka,Kb,P,Pa,Pb)
   ! Make the real coulomb matrices "J" and exchange matrices "Ka" and "Kb"
   ! matrix from a symmetric density matrices "P", "Pa", and "Pb" directly.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J,Ka,Kb
      MAT(REAL,:,:), IN :: P,Pa,Pb
      MAT4(REAL,:,:,:,:), allocatable :: I
      type(ARCHIVE_TYPE), allocatable :: eri_archive
      type(ARCHIVE_TYPE), allocatable :: ind_archive
      INT :: n_quartets
      INT :: q,a,b,c,d
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      REAL :: I_abcd,P_dc
      REAL :: Pa_db,Pa_cb
      REAL :: Pb_db,Pb_cb
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_u_JK_disk ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_u_JK_disk ... no atom list")
      call create_(eri_archive,self%name,"eri_integrals")
      call create_(ind_archive,self%name,"eri_index")
      ENSURE(exists_(eri_archive),"MOLECULE.FOCK:make_u_JK_disk ... no integral file")
      ENSURE(exists_(ind_archive),"MOLECULE.FOCK:make_u_JK_disk ... no integral index file")
      call open_for_(eri_archive,"read-only")
      call open_for_(ind_archive,"read-only")
      J = ZERO
      Ka = ZERO
      Kb = ZERO
      n_quartets = no_of_shell_quartets_(self%atom)
      do
         call read_(ind_archive%Bfile,q)
         if (q>n_quartets) exit
         call set_shell4_indices_from_(self,q,fa,la,fb,lb,fc,lc,fd,ld)
         call create_(I,fa,la,fb,lb,fc,lc,fd,ld)
         call read_(eri_archive%Bfile,I)
         do d = fd,ld
            do c = fc,lc
               P_dc = P(d,c)
               do b = fb,lb
                  Pa_db = Pa(d,b)
                  Pb_db = Pb(d,b)
                  Pa_cb = Pa(c,b)
                  Pb_cb = Pb(c,b)
                  do a = fa,la
                     I_abcd = I(a,b,c,d)
                     J(a,b) = J(a,b)+I_abcd*P_dc
                     J(c,d) = J(c,d)+I_abcd*P(b,a)
                     Ka(a,c) = Ka(a,c)+I_abcd*Pa_db
                     Ka(a,d) = Ka(a,d)+I_abcd*Pa_cb
                     Ka(b,c) = Ka(b,c)+I_abcd*Pa(d,a)
                     Ka(b,d) = Ka(b,d)+I_abcd*Pa(c,a)
                     Kb(a,c) = Kb(a,c)+I_abcd*Pb_db
                     Kb(a,d) = Kb(a,d)+I_abcd*Pb_cb
                     Kb(b,c) = Kb(b,c)+I_abcd*Pb(d,a)
                     Kb(b,d) = Kb(b,d)+I_abcd*Pb(c,a)
                  end do
               end do
            end do
         end do
         call destroy_(I)
      end do
      call destroy_(ind_archive)
      call destroy_(eri_archive)
      call weight_diagonal_blocks_(self,J,TWO)
      call blockwise_symmetric_fold_(self,Ka)
      call blockwise_symmetric_fold_(self,Kb)
      call symmetric_reflect_(J)
      call symmetric_reflect_(Ka)
      call symmetric_reflect_(Kb)
      J = TWO*J
   end subroutine

   PURE subroutine make_g_JK_direct(self,J,Ka,Kb,Kba,P,Pa,Pb,Pba)
   ! Make the general matrices "J" and exchange matrices "Ka", "Kb"
   ! and "Kba" from a density matrices "P", "Pa", "Pb" and "Pba"
   ! directly from the integrals.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J,Ka,Kb,Kba
      MAT(REAL,:,:), IN :: P,Pa,Pb,Pba
      MAT(REAL,:,:), allocatable :: abs_P
      VEC(REAL,:), allocatable :: max_P,I
      type(SHELL1QUARTET_TYPE) :: sh4s,sh4n
      type(ERI_SCRATCH_TYPE) :: work
      INT :: ab,cd
      INT :: abcd
      INT :: aa,bb,cc,dd
      INT :: a,b,c,d
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      INT :: na,nb,nc,nd
      INT :: atom_a,atom_b,atom_c,atom_d
      REAL :: I_abcd,factor,cutoff
      BIN :: skip,no_max_I
      BIN :: ab_same,cd_same,any_same
      J = ZERO
      Ka = ZERO
      Kb = ZERO
      Kba = ZERO
      cutoff = self%SCF_data%ERI_Schwarz_cutoff
      no_max_I = FALSE
      if (NOT allocated(self%max_I)) no_max_I = TRUE
      if (no_max_I) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call create_(abs_P,self%n_bf,self%n_bf)
      abs_P = max(abs(P),abs(Pa),abs(Pb),abs(Pba))
      call make_max_density_elements_(self,max_P,abs_P)
      call destroy_(abs_P)

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      do ab = 1,self%n_shell_pairs
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
         ab_same = atom_a==atom_b
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         do cd = 1,ab
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
            cd_same = atom_c==atom_d
            skip = Schwarz_inequality_test(cutoff,ab,cd,aa,bb,cc,dd,max_P,self%max_I)
            if (skip) cycle
            factor = ONE  ! Evaluate the integrals
            if (aa==bb) factor = HALF  ! Coincidence factors
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) factor = HALF*factor
            any_same = ab_same OR cd_same OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d
            if (any_same) then
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_ERI_into(sh4n,work)
            else
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_ERI_into(sh4s,work)
            end if
            I = factor*I
            abcd = 0
            do d = fd,ld
               do c = fc,lc
                  do b = fb,lb
                     do a = fa,la
                        abcd = abcd+1
                        I_abcd = work%abcd(abcd)
                        J(a,b) = J(a,b)+I_abcd*P(d,c)  ! These are symmetric
                        J(c,d) = J(c,d)+I_abcd*P(b,a)
                        Ka(a,d) = Ka(a,d)+I_abcd*Pa(b,c)  ! These are symmetric
                        Ka(a,c) = Ka(a,c)+I_abcd*Pa(b,d)  ! but must be folded
                        Ka(b,c) = Ka(b,c)+I_abcd*Pa(a,d)
                        Ka(b,d) = Ka(b,d)+I_abcd*Pa(a,c)
                        Kb(a,d) = Kb(a,d)+I_abcd*Pb(b,c)  ! These are symmetric
                        Kb(a,c) = Kb(a,c)+I_abcd*Pb(b,d)  ! but must be folded
                        Kb(b,c) = Kb(b,c)+I_abcd*Pb(a,d)
                        Kb(b,d) = Kb(b,d)+I_abcd*Pb(a,c)
                        Kba(a,d) = Kba(a,d)+I_abcd*Pba(b,c)  ! These are asymmetric
                        Kba(a,c) = Kba(a,c)+I_abcd*Pba(b,d)
                        Kba(b,c) = Kba(b,c)+I_abcd*Pba(a,d)
                        Kba(b,d) = Kba(b,d)+I_abcd*Pba(a,c)
                        Kba(d,a) = Kba(d,a)+I_abcd*Pba(c,b)
                        Kba(c,a) = Kba(c,a)+I_abcd*Pba(d,b)
                        Kba(c,b) = Kba(c,b)+I_abcd*Pba(d,a)
                        Kba(d,b) = Kba(d,b)+I_abcd*Pba(c,a)
                     end do
                  end do
               end do
            end do
         end do
      end do
      call destroy_(max_P)
      call weight_diagonal_blocks_(self,J,TWO)
      call blockwise_symmetric_fold_(self,Ka)
      call blockwise_symmetric_fold_(self,Kb)
      call symmetric_reflect_(J)
      call symmetric_reflect_(Ka)
      call symmetric_reflect_(Kb)
      J = TWO*J
   end subroutine

   PURE subroutine make_gc_JK_direct(self,J,Ka,Kb,Kba,P,Pa,Pb,Pba)
   ! Make the general complex matrices "J" and exchange matrices "Ka",
   ! "Kb" and "Kba" from a density matrices "P", "Pa", "Pb" and "Pba"
   ! directly from the integrals.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J
      MAT(CPX,:,:), OUT :: Ka,Kb,Kba
      MAT(REAL,:,:), IN :: P
      MAT(CPX,:,:), IN :: Pa,Pb,Pba
      VEC(REAL,:), allocatable :: max_P,I
      MAT(REAL,:,:), allocatable :: abs_P
      type(SHELL1QUARTET_TYPE) :: sh4s,sh4n
      type(ERI_SCRATCH_TYPE) :: work
      INT :: ab,cd,abcd
      INT :: aa,bb,cc,dd
      INT :: a,b,c,d
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      INT :: na,nb,nc,nd
      INT :: atom_a,atom_b,atom_c,atom_d
      REAL :: I_abcd,factor,cutoff
      BIN :: skip,no_max_I
      BIN :: ab_same,cd_same,any_same
      J = ZERO
      Ka = ZERO
      Kb = ZERO
      Kba = ZERO
      cutoff = self%SCF_data%ERI_Schwarz_cutoff
      no_max_I = FALSE
      if (NOT allocated(self%max_I)) no_max_I = TRUE
      if (no_max_I) call initialize_max_I_(self)
      call create_(max_P,self%n_shell_pairs)
      call create_(abs_P,self%n_bf,self%n_bf)
      abs_P = max(abs(P),abs(Pa),abs(Pb),abs(Pba))
      call make_max_density_elements_(self,max_P,abs_P)
      call destroy_(abs_P)

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      do ab = 1,self%n_shell_pairs
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
         ab_same = atom_a==atom_b
         if (NOT ab_same) call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         do cd = 1,ab
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
            cd_same = atom_c==atom_d
            skip = Schwarz_inequality_test(cutoff,ab,cd,aa,bb,cc,dd,max_P,self%max_I)
            if (skip) cycle
            factor = ONE  ! Evaluate the integrals
            if (aa==bb) factor = HALF  ! Coincidence factors
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) factor = HALF*factor
            any_same = ab_same OR cd_same OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d
            if (any_same) then
               call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
               call make_ERI_into(sh4n,work)
            else
               call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
               call make_ERI_into(sh4s,work)
            end if
            I = factor*I
            abcd = 0
            do d = fd,ld
               do c = fc,lc
                  do b = fb,lb
                     do a = fa,la
                        abcd = abcd+1
                        I_abcd = work%abcd(abcd)
                        J(a,b) = J(a,b)+I_abcd*P(d,c)  ! These are symmetric
                        J(c,d) = J(c,d)+I_abcd*P(b,a)
                        Ka(a,d) = Ka(a,d)+I_abcd*Pa(b,c)  ! These are symmetric
                        Ka(a,c) = Ka(a,c)+I_abcd*Pa(b,d)  ! but must be folded
                        Ka(b,c) = Ka(b,c)+I_abcd*Pa(a,d)
                        Ka(b,d) = Ka(b,d)+I_abcd*Pa(a,c)
                        Kb(a,d) = Kb(a,d)+I_abcd*Pb(b,c)  ! These are symmetric
                        Kb(a,c) = Kb(a,c)+I_abcd*Pb(b,d)  ! but must be folded
                        Kb(b,c) = Kb(b,c)+I_abcd*Pb(a,d)
                        Kb(b,d) = Kb(b,d)+I_abcd*Pb(a,c)
                        Kba(a,d) = Kba(a,d)+I_abcd*Pba(b,c)  ! These are asymmetric
                        Kba(a,c) = Kba(a,c)+I_abcd*Pba(b,d)
                        Kba(b,c) = Kba(b,c)+I_abcd*Pba(a,d)
                        Kba(b,d) = Kba(b,d)+I_abcd*Pba(a,c)
                        Kba(d,a) = Kba(d,a)+I_abcd*Pba(c,b)
                        Kba(c,a) = Kba(c,a)+I_abcd*Pba(d,b)
                        Kba(c,b) = Kba(c,b)+I_abcd*Pba(d,a)
                        Kba(d,b) = Kba(d,b)+I_abcd*Pba(c,a)
                     end do
                  end do
               end do
            end do
         end do
      end do
      call destroy_(max_P)
      call weight_diagonal_blocks_(self,J,TWO)
      call blockwise_hermitian_fold_(self,Ka)
      call blockwise_hermitian_fold_(self,Kb)
      call symmetric_reflect_(J)
      J = TWO*J
      call make_hermitian_(Ka)
      call make_hermitian_(Kb)
   end subroutine

   subroutine make_gc_JK_disk(self,J,Ka,Kb,Kba,P,Pa,Pb,Pba)
   ! Make the real coulomb matrices "J" and exchange matrices "Ka" and
   ! "Kb" matrix from a symmetric density matrices "P", "Pa", and "Pb"
   ! directly.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: J
      MAT(CPX,:,:), OUT :: Ka,Kb,Kba
      MAT(REAL,:,:), IN :: P
      MAT(CPX,:,:), IN :: Pa,Pb,Pba
      MAT4(REAL,:,:,:,:), allocatable :: I
      type(ARCHIVE_TYPE), allocatable :: eri_archive
      type(ARCHIVE_TYPE), allocatable :: ind_archive
      INT :: n_quartets
      INT :: q,a,b,c,d
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      REAL :: I_abcd
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_gc_JK_disk ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_gc_JK_disk ... no atom list")
      call create_(eri_archive,self%name,"eri_integrals")
      call create_(ind_archive,self%name,"eri_index")
      ENSURE(exists_(eri_archive),"MOLECULE.FOCK:make_gc_JK_disk ... no integral file")
      ENSURE(exists_(ind_archive),"MOLECULE.FOCK:make_gc_JK_disk ... no integral index file")
      call open_for_(eri_archive,"read-only")
      call open_for_(ind_archive,"read-only")
      J = ZERO
      Ka = ZERO
      Kb = ZERO
      Kba = ZERO
      n_quartets = no_of_shell_quartets_(self%atom)
      do
         call read_(ind_archive%Bfile,q)
         if (q>n_quartets) exit
         call set_shell4_indices_from_(self,q,fa,la,fb,lb,fc,lc,fd,ld)
         allocate(I(fa:la,fb:lb,fc:lc,fd:ld))
         call read_(eri_archive%Bfile,I)
         do d = fd,ld
            do c = fc,lc
               do b = fb,lb
                  do a = fa,la
                     I_abcd = I(a,b,c,d)
                     J(a,b) = J(a,b)+I_abcd*P(d,c)  ! These are symmetric
                     J(c,d) = J(c,d)+I_abcd*P(b,a)
                     Ka(a,d) = Ka(a,d)+I_abcd*Pa(b,c)  ! These are symmetric but must be folded
                     Ka(a,c) = Ka(a,c)+I_abcd*Pa(b,d)
                     Ka(b,c) = Ka(b,c)+I_abcd*Pa(a,d)
                     Ka(b,d) = Ka(b,d)+I_abcd*Pa(a,c)
                     Kb(a,d) = Kb(a,d)+I_abcd*Pb(b,c)  ! These are symmetric but must be folded
                     Kb(a,c) = Kb(a,c)+I_abcd*Pb(b,d)
                     Kb(b,c) = Kb(b,c)+I_abcd*Pb(a,d)
                     Kb(b,d) = Kb(b,d)+I_abcd*Pb(a,c)
                     Kba(a,d) = Kba(a,d)+I_abcd*Pba(b,c)  ! These are asymmetric
                     Kba(a,c) = Kba(a,c)+I_abcd*Pba(b,d)
                     Kba(b,c) = Kba(b,c)+I_abcd*Pba(a,d)
                     Kba(b,d) = Kba(b,d)+I_abcd*Pba(a,c)
                     Kba(d,a) = Kba(d,a)+I_abcd*Pba(c,b)
                     Kba(c,a) = Kba(c,a)+I_abcd*Pba(d,b)
                     Kba(c,b) = Kba(c,b)+I_abcd*Pba(d,a)
                     Kba(d,b) = Kba(d,b)+I_abcd*Pba(c,a)
                  end do
               end do
            end do
         end do
         deallocate(I)
      end do
      call destroy_(ind_archive)
      call destroy_(eri_archive)
      call weight_diagonal_blocks_(self,J,TWO)
      call blockwise_hermitian_fold_(self,Ka)
      call blockwise_hermitian_fold_(self,Kb)
      call symmetric_reflect_(J)
      call make_hermitian_(Ka)
      call make_hermitian_(Kb)
      J = TWO*J
   end subroutine

   PURE subroutine make_gc_so_JK_direct(self,JS,KS,JO,KO,P)
   ! Make the general complex spin orbit Classical and Exchange matrices.
   ! Make them directly from integrals on the fly.
      type(MOLECULE_TYPE), INOUT :: self
      MAT5(CPX,:,:,:,:,:), OUT :: JS,KS
      MAT5(CPX,:,:,:,:,:), OUT :: JO,KO
      MAT4(CPX,:,:,:,:), IN :: P
      MAT5(REAL,:,:,:,:,:), allocatable :: S,O
      INT :: ab,cd
      INT :: aa,bb,cc,dd
      INT :: a,b,c,d
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      INT :: na,nb,nc,nd
      INT :: k,m,n
      REAL :: S_abcd
      REAL :: O_abcd
      REAL :: factor
      type(SHELL4_TYPE) :: sh4
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_gc_so_JK_direct ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_gc_so_JK_direct ... no atom list")
      JS = ZERO; KS = ZERO
      JO = ZERO; KO = ZERO
      do ab = 1,self%n_shell_pairs
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb)
         do cd = 1,ab
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd)
            call set_shell4_from_(self,aa,bb,cc,dd,sh4)
            if (skip_ERI_(sh4)) then  ! ??????
               call destroy_ptr_part_(sh4)
               cycle
            end if
            allocate(S(fa:la,fb:lb,fc:lc,fd:ld,3))
            allocate(O(fa:la,fb:lb,fc:lc,fd:ld,3))

            ! Evaluate the integrals
            call make_spin_orbit_ints_(sh4,S,O)

            ! Coincidence factors
            factor = ONE
            if (aa==bb) factor = HALF
            if (cc==dd) factor = HALF*factor
            if (aa==cc AND bb==dd) factor = HALF*factor
            S = factor*S
            O = factor*O
            do k = 1,3
               do a = fa,la
                  do b = fb,lb
                     do c = fc,lc
                        do d = fd,ld
                           S_abcd = S(a,b,c,d,k)
                           O_abcd = O(a,b,c,d,k)
                           do m = 1,2
                              do n = 1,2
                                 JS(a,b,m,n,k) = JS(a,b,m,n,k)+S_abcd*P(d,c,m,n)
                                 JS(a,b,m,n,k) = JS(a,b,m,n,k)+S_abcd*P(c,d,m,n)
                                 JS(b,a,m,n,k) = JS(b,a,m,n,k)-S_abcd*P(d,c,m,n)
                                 JS(b,a,m,n,k) = JS(b,a,m,n,k)-S_abcd*P(c,d,m,n)
                                 JS(c,d,m,n,k) = JS(c,d,m,n,k)+O_abcd*P(b,a,m,n)
                                 JS(c,d,m,n,k) = JS(c,d,m,n,k)+O_abcd*P(a,b,m,n)
                                 JS(d,c,m,n,k) = JS(d,c,m,n,k)-O_abcd*P(b,a,m,n)
                                 JS(d,c,m,n,k) = JS(d,c,m,n,k)-O_abcd*P(a,b,m,n)
                                 JO(a,b,m,n,k) = JO(a,b,m,n,k)+O_abcd*P(d,c,m,n)
                                 JO(a,b,m,n,k) = JO(a,b,m,n,k)-O_abcd*P(c,d,m,n)
                                 JO(b,a,m,n,k) = JO(b,a,m,n,k)+O_abcd*P(d,c,m,n)
                                 JO(b,a,m,n,k) = JO(b,a,m,n,k)-O_abcd*P(c,d,m,n)
                                 JO(c,d,m,n,k) = JO(c,d,m,n,k)+S_abcd*P(b,a,m,n)
                                 JO(c,d,m,n,k) = JO(c,d,m,n,k)-S_abcd*P(a,b,m,n)
                                 JO(d,c,m,n,k) = JO(d,c,m,n,k)+S_abcd*P(b,a,m,n)
                                 JO(d,c,m,n,k) = JO(d,c,m,n,k)-S_abcd*P(a,b,m,n)
                                 KS(a,d,m,n,k) = KS(a,d,m,n,k)+S_abcd*P(b,c,m,n)
                                 KS(a,c,m,n,k) = KS(a,c,m,n,k)+S_abcd*P(b,d,m,n)
                                 KS(b,d,m,n,k) = KS(b,d,m,n,k)-S_abcd*P(a,c,m,n)
                                 KS(b,c,m,n,k) = KS(b,c,m,n,k)-S_abcd*P(a,d,m,n)
                                 KS(c,b,m,n,k) = KS(c,b,m,n,k)+O_abcd*P(d,a,m,n)
                                 KS(c,a,m,n,k) = KS(c,a,m,n,k)+O_abcd*P(d,b,m,n)
                                 KS(d,b,m,n,k) = KS(d,b,m,n,k)-O_abcd*P(c,a,m,n)
                                 KS(d,a,m,n,k) = KS(d,a,m,n,k)-O_abcd*P(c,b,m,n)
                                 KO(a,d,m,n,k) = KO(a,d,m,n,k)+O_abcd*P(b,c,m,n)
                                 KO(a,c,m,n,k) = KO(a,c,m,n,k)-O_abcd*P(b,d,m,n)
                                 KO(b,d,m,n,k) = KO(b,d,m,n,k)+O_abcd*P(a,c,m,n)
                                 KO(b,c,m,n,k) = KO(b,c,m,n,k)-O_abcd*P(a,d,m,n)
                                 KO(c,b,m,n,k) = KO(c,b,m,n,k)+S_abcd*P(d,a,m,n)
                                 KO(c,a,m,n,k) = KO(c,a,m,n,k)-S_abcd*P(d,b,m,n)
                                 KO(d,b,m,n,k) = KO(d,b,m,n,k)+S_abcd*P(c,a,m,n)
                                 KO(d,a,m,n,k) = KO(d,a,m,n,k)-S_abcd*P(c,b,m,n)
                              end do
                           end do
                        end do
                     end do
                  end do
               end do
            end do
            deallocate(O)
            deallocate(S)
            call destroy_ptr_part_(sh4)
         end do
      end do
   end subroutine

   subroutine make_gc_so_JK_disk(self,JS,KS,JO,KO,P,component)
   ! Make the general complex spin orbit Classical and Exchange matrices
   ! for a particular "component", either "x", "y" or "z".
      type(MOLECULE_TYPE), INOUT :: self
      MAT4(CPX,:,:,:,:), OUT :: JS,KS
      MAT4(CPX,:,:,:,:), OUT :: JO,KO
      MAT4(CPX,:,:,:,:), IN :: P
      STR(len=1), IN :: component
      MAT4(REAL,:,:,:,:), allocatable :: S,O
      type(ARCHIVE_TYPE) :: SO_archive
      type(ARCHIVE_TYPE) :: ind_archive
      INT :: n_quartets
      INT :: q,m,n
      INT :: a,b,c,d
      INT :: fa,fb,fc,fd
      INT :: la,lb,lc,ld
      REAL :: S_abcd
      REAL :: O_abcd
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_gc_so_JK_disk ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_gc_so_JK_disk ... no atom list")
      call set_(SO_archive,self%name,"SO"//component//"_integrals")
      call set_(ind_archive,self%name,"SO_indices")
      ENSURE(exists_(SO_archive),"MOLECULE.FOCK:make_gc_so_JK_disk ... no spin orbit integral file!")
      ENSURE(exists_(ind_archive),"MOLECULE.FOCK:make_gc_so_JK_disk ... no spin orbit integral index file")
      call open_for_(SO_archive,"read-only")
      call open_for_(ind_archive,"read-only")
      JS = ZERO; KS = ZERO
      JO = ZERO; KO = ZERO
      n_quartets = no_of_shell_quartets_(self%atom)
      do
         call read_(ind_archive%Bfile,q)
         if (q>n_quartets) exit
         call set_shell4_indices_from_(self,q,fa,la,fb,lb,fc,lc,fd,ld)
         call create_(S,fa,la,fb,lb,fc,lc,fd,ld)
         call create_(O,fa,la,fb,lb,fc,lc,fd,ld)
         call read_(SO_archive%Bfile,S)
         call read_(SO_archive%Bfile,O)
         do a = fa,la
            do b = fb,lb
               do c = fc,lc
                  do d = fd,ld
                     S_abcd = S(a,b,c,d)
                     O_abcd = O(a,b,c,d)
                     do m = 1,2
                        do n = 1,2
                           JS(a,b,m,n) = JS(a,b,m,n)+S_abcd*P(d,c,m,n)
                           JS(a,b,m,n) = JS(a,b,m,n)+S_abcd*P(c,d,m,n)
                           JS(b,a,m,n) = JS(b,a,m,n)-S_abcd*P(d,c,m,n)
                           JS(b,a,m,n) = JS(b,a,m,n)-S_abcd*P(c,d,m,n)
                           JS(c,d,m,n) = JS(c,d,m,n)+O_abcd*P(b,a,m,n)
                           JS(c,d,m,n) = JS(c,d,m,n)+O_abcd*P(a,b,m,n)
                           JS(d,c,m,n) = JS(d,c,m,n)-O_abcd*P(b,a,m,n)
                           JS(d,c,m,n) = JS(d,c,m,n)-O_abcd*P(a,b,m,n)
                           JO(a,b,m,n) = JO(a,b,m,n)+O_abcd*P(d,c,m,n)
                           JO(a,b,m,n) = JO(a,b,m,n)-O_abcd*P(c,d,m,n)
                           JO(b,a,m,n) = JO(b,a,m,n)+O_abcd*P(d,c,m,n)
                           JO(b,a,m,n) = JO(b,a,m,n)-O_abcd*P(c,d,m,n)
                           JO(c,d,m,n) = JO(c,d,m,n)+S_abcd*P(b,a,m,n)
                           JO(c,d,m,n) = JO(c,d,m,n)-S_abcd*P(a,b,m,n)
                           JO(d,c,m,n) = JO(d,c,m,n)+S_abcd*P(b,a,m,n)
                           JO(d,c,m,n) = JO(d,c,m,n)-S_abcd*P(a,b,m,n)
                           KS(a,d,m,n) = KS(a,d,m,n)+S_abcd*P(b,c,m,n)
                           KS(a,c,m,n) = KS(a,c,m,n)+S_abcd*P(b,d,m,n)
                           KS(b,d,m,n) = KS(b,d,m,n)-S_abcd*P(a,c,m,n)
                           KS(b,c,m,n) = KS(b,c,m,n)-S_abcd*P(a,d,m,n)
                           KS(c,b,m,n) = KS(c,b,m,n)+O_abcd*P(d,a,m,n)
                           KS(c,a,m,n) = KS(c,a,m,n)+O_abcd*P(d,b,m,n)
                           KS(d,b,m,n) = KS(d,b,m,n)-O_abcd*P(c,a,m,n)
                           KS(d,a,m,n) = KS(d,a,m,n)-O_abcd*P(c,b,m,n)
                           KO(a,d,m,n) = KO(a,d,m,n)+O_abcd*P(b,c,m,n)
                           KO(a,c,m,n) = KO(a,c,m,n)-O_abcd*P(b,d,m,n)
                           KO(b,d,m,n) = KO(b,d,m,n)+O_abcd*P(a,c,m,n)
                           KO(b,c,m,n) = KO(b,c,m,n)-O_abcd*P(a,d,m,n)
                           KO(c,b,m,n) = KO(c,b,m,n)+S_abcd*P(d,a,m,n)
                           KO(c,a,m,n) = KO(c,a,m,n)-S_abcd*P(d,b,m,n)
                           KO(d,b,m,n) = KO(d,b,m,n)+S_abcd*P(c,a,m,n)
                           KO(d,a,m,n) = KO(d,a,m,n)-S_abcd*P(c,b,m,n)
                        end do
                     end do
                  end do
               end do
            end do
         end do
         call destroy_(O)
         call destroy_(S)
      end do
      call close_(SO_archive)
      call close_(ind_archive)
   end subroutine


!  =============
!  Schwarz tests
!  =============

   pure function Schwarz_inequality_test(cutoff,ab,cd,a,b,c,d,P_max,I_max) result (skip)
   ! Return "skip", a logical variable which is TRUE if the maximum
   ! contribution to the restricted Fock matrix (based on the schwarz
   ! inequality test) from a shell of ERI integrals (ab|cd) with shell
   ! pair indices "ab" and "cd" is smaller than a preset "cutoff".
   ! "P_max" is the maximum density matrix elements for each shell
   ! pair "ab", and "I_max(ab)" is maximum value of the square root of
   ! the integral (ab|ab) for the shell pair with index "ab".
      REAL, IN :: cutoff
      INT, IN :: ab,cd
      INT, IN :: a,b,c,d
      VEC(REAL,:), IN :: P_max,I_max
      BIN :: skip
      REAL :: Pmax
      INT :: ac,ad
      INT :: bc,bd
      INT :: k,l,m,n

      ! Indices for (ab|cd) < sqrt(ab|ab) * sqrt(cd|cd) test.
      k = max(b,c); l = min(b,c)
      m = max(b,d); n = min(b,d)
      ac = ((a-1)*a)/2+c
      ad = ac-c+d
      bc = ((k-1)*k)/2+l
      bd = ((m-1)*m)/2+n
      Pmax = max(P_max(ab),P_max(cd),P_max(ac),P_max(ad),P_max(bc),P_max(bd))
      if (Pmax<TOL(20)) then
         skip = TRUE
      else
         if (Pmax*I_max(ab)*I_max(cd)>cutoff) then; skip = FALSE
         else; skip = TRUE
         end if
      end if
   end function

   pure subroutine Schwarz_test(skip_J,skip_K,cutoff,JP_cut,KP_cut,ab,cd,a,b,c,d,P_max,I_max)
   ! Return "skip_J" and "skip_K" which are set TRUE if the
   ! contribution to the J and K matrices (based on the Schwarz
   ! inequality test) from a shell of ERI integrals (ab|cd) with shell
   ! pair indices "ab" and "cd" is smaller than a preset "cutoff".
   ! "P_max(ab)" is the maximum density matrix elements for each shell
   ! pair "ab", and "I_max(ab)" is maximum value of the square root of
   ! the integral (ab|ab) for the shell pair with index "ab".
      BIN, OUT :: skip_J
      BIN, OUT :: skip_K
      REAL, IN :: cutoff
      REAL, IN :: JP_cut,KP_cut
      INT, IN :: ab,cd,a,b,c,d
      VEC(REAL,:), IN :: P_max,I_max
      REAL :: JP_max,KP_max
      INT :: ac,ad,bc,bd,k,l,m,n

      ! Indices for (ab|cd) < sqrt(ab|ab) * sqrt(cd|cd) test.
      k = max(b,c); l = min(b,c)
      m = max(b,d); n = min(b,d)
      ac = (a-1)*a/2+c
      ad = ac-c+d
      bc = (k-1)*k/2+l
      bd = (m-1)*m/2+n
      JP_max = max(P_max(ab),P_max(cd))
      KP_max = max(P_max(ac),P_max(ad),P_max(bc),P_max(bd))
      skip_J = FALSE
      if (JP_max<JP_cut) then; skip_J = TRUE
      else if (JP_max*I_max(ab)*I_max(cd)<cutoff) then; skip_J = TRUE
      end if
      skip_K = FALSE
      if (KP_max<KP_cut) then; skip_K = TRUE
      else if (KP_max*I_max(ab)*I_max(cd)<cutoff) then; skip_K = TRUE
      end if
   end subroutine

   pure subroutine Schwarz_J_test(skip_J,cutoff,ab,cd,P_max,I_max)
   ! Return "skip_J" which is set TRUE if the maximum contribution to
   ! the J matrix (based on the schwarz inequality test) from a shell
   ! of ERI integrals (ab|cd) with shell pair indices "ab" and "cd" is
   ! smaller than a preset "cutoff". "P_max(ab)" is the maximum
   ! density matrix elements for each shell pair "ab", and "I_max(ab)"
   ! is maximum value of the square root of the integral (ab|ab) for
   ! the shell pair with index "ab".
      BIN, OUT :: skip_J
      REAL, IN :: cutoff
      INT, IN :: ab,cd
      VEC(REAL,:), IN :: P_max
      VEC(REAL,:), IN :: I_max
      REAL :: Jmax
      Jmax = max(P_max(ab),P_max(cd))
      skip_J = FALSE
      if (Jmax<TOL(20)) then; skip_J = TRUE
      else if (Jmax*I_max(ab)*I_max(cd)<cutoff) then; skip_J = TRUE
      end if
   end subroutine

   pure subroutine Schwarz_K_test(skip_K,cutoff,ab,cd,a,b,c,d,P_max,I_max)
   ! Return "skip_K", logical variables are TRUE if the maximum
   ! contribution to the K matrices (based on the schwarz inequality
   ! test) from a shell of ERI integrals (ab|cd) with shell pair
   ! indices "ab" and "cd" is smaller than a preset "cutoff".
   ! "P_max(ab)" is the maximum density matrix elements for each shell
   ! pair "ab", and "I_max(ab)" is maximum value of the square root of
   ! the integral (ab|ab) for the shell pair with index "ab".
      BIN, OUT :: skip_K
      REAL, IN :: cutoff
      INT, IN :: ab,cd
      INT, IN :: a,b,c,d
      VEC(REAL,:), IN :: P_max
      VEC(REAL,:), IN :: I_max
      REAL :: Kmax
      INT :: ac,ad
      INT :: bc,bd
      INT :: k,l,m,n
      k = max(b,c); l = min(b,c)
      m = max(b,d); n = min(b,d)

      ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
      ac = (a-1)*a/2+c
      ad = ac-c+d
      bc = (k-1)*k/2+l
      bd = (m-1)*m/2+n
      Kmax = max(P_max(ac),P_max(ad),P_max(bc),P_max(bd))
      skip_K = FALSE
      if (Kmax<TOL(20)) then; skip_K = TRUE
      else if (Kmax*I_max(ab)*I_max(cd)<cutoff) then; skip_K = TRUE
      end if
   end subroutine

   PURE subroutine make_max_density_elements_0(self,P_max,P)
   ! Make "P_max", the maximum of the (a,b) elements of "P" 
   ! over each shell pair, for use in the Schwarz test.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: P_max
      MAT(REAL,:,:), IN :: P
      INT :: ab,aa,bb
      INT :: fa,fb,la,lb
      INT :: na,nb,a,b
      REAL :: Pmax
   ENSURE(size(P_max)==self%n_shell_pairs,"MOLECULE.FOCK:make_max_density_elements_0 ... wrong size, P_max")
   ENSURE(is_square_(P),"MOLECULE.FOCK:make_max_density_elements_0 ... wrong shape, P")
   ENSURE(size(P,1)==self%last_basis_fn_for_shell(self%n_shell),"MOLECULE.FOCK:make_max_density_elements_0 ... wrong size, P")

      ! Store the largest integral for each shell pair ab|ab
      do ab = 1,self%n_shell_pairs
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb)
         Pmax = ZERO
         do a = fa,la
            do b = fb,lb
               Pmax = max(abs(P(a,b)),Pmax)
            end do
         end do
         P_max(ab) = Pmax
      end do
   end subroutine

   PURE subroutine make_max_density_elements_1(self,P_max,P)
   ! Make "P_max", the maximum of the (a,b) elements of "P" 
   ! over each shell pair *and* each perturbation, for use 
   ! in the Schwarz test.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: P_max
      MAT3(REAL,:,:,:), IN :: P
      INT :: ab,aa,bb
      INT :: fa,fb,la,lb
      INT :: na,nb,a,b
      REAL :: Pmax
   ENSURE(size(P_max)==self%n_shell_pairs,"MOLECULE.FOCK:make_max_density_elements_1 ... wrong size, P_max")
   ENSURE(size(P,2)==self%last_basis_fn_for_shell(self%n_shell),"MOLECULE.FOCK:make_max_density_elements_1 ... wrong dim2, P")
   ENSURE(size(P,3)==self%last_basis_fn_for_shell(self%n_shell),"MOLECULE.FOCK:make_max_density_elements_1 ... wrong dim3, P")

      ! Store the largest integral for each shell pair ab|ab
      do ab = 1,self%n_shell_pairs
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb)
         Pmax = ZERO
         do a = fa,la
            do b = fb,lb
               Pmax = max(maxval(abs(P(:,a,b))),Pmax)
            end do
         end do
         P_max(ab) = Pmax
      end do
   end subroutine

   PURE subroutine make_max_density_elements_2(self,P_max,Pa,Pb)
   ! Make "P_max", the maximum of the (a,b) density matrix elements of
   ! "Pa" and "Pb" over each shell pair, for use in the Schwarz test.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: P_max
      MAT(REAL,:,:), IN :: Pa,Pb
      INT :: ab,aa,bb
      INT :: fa,fb,la,lb
      INT :: na,nb,a,b
      REAL :: Pmax
   ENSURE(size(P_max)==self%n_shell_pairs,"MOLECULE.FOCK:make_max_density_elements_2 ... wrong size, P_max")
   ENSURE(is_square_(Pa),"MOLECULE.FOCK:make_max_density_elements_2 ... wrong shape, Pa")
   ENSURE(is_square_(Pb),"MOLECULE.FOCK:make_max_density_elements_2 ... wrong shape, Pb")
   ENSURE(size(Pa,1)==self%last_basis_fn_for_shell(self%n_shell),"MOLECULE.FOCK:make_max_density_elements_2 ... wrong size, Pa")
   ENSURE(size(Pb,1)==self%last_basis_fn_for_shell(self%n_shell),"MOLECULE.FOCK:make_max_density_elements_2 ... wrong size, Pb")

      ! Store the largest integral for each shell pair ab|ab
      do ab = 1,self%n_shell_pairs
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb)
         Pmax = ZERO
         do a = fa,la
            do b = fb,lb
               Pmax = max(abs(Pa(a,b)),abs(Pb(a,b)),Pmax)
            end do
         end do
         P_max(ab) = Pmax
      end do
   end subroutine

   PURE subroutine initialize_max_I(self)
   ! Initialize Schwarz inequality test integrals
      type(MOLECULE_TYPE), INOUT :: self
      call create_(self%max_I,self%n_shell_pairs)
      call make_max_abab_integrals_(self,self%max_I)
   end subroutine

   PURE subroutine make_max_abab_integrals(self,I_max)
   ! Make "I_max", the square root of the maximum of the 
   ! (ab|ab) ERI integrals.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: I_max
      INT :: ab,sa,sb
      INT :: fa,fb,la,lb
      INT :: na,nb,a,b
      type(SHELL1QUARTET_TYPE) :: sh4q
      MAT4(REAL,:,:,:,:), allocatable :: Iabab
      REAL :: Imax
   ENSURE(size(I_max)==self%n_shell_pairs,"MOLECULE.FOCK:make_max_abab_integrals ... wrong size, I_max")
      do ab = 1,self%n_shell_pairs
         call set_shell2_indices_from_(self,ab,sa,sb,fa,la,na,fb,lb,nb)

         ! The J/K engine works in the cartesian basis, so its Schwarz
         ! bounds must too: a spherical (ab|ab) is not a bound on the
         ! cartesian integrals it screens.
         if (self%use_spherical_basis) then
            fa = self%first_cartesian_bf_for_shell(sa)
            la = self%last_cartesian_bf_for_shell(sa)
            fb = self%first_cartesian_bf_for_shell(sb)
            lb = self%last_cartesian_bf_for_shell(sb)
            na = la-fa+1
            nb = lb-fb+1
         end if
         call set_shell1q_ab_from_(self,sa,sb,sh4q,skip=FALSE)  ! (ab|ab} shell
         call set_shell1q_cd_from_(self,sa,sb,sh4q,skip=FALSE)
         call set_spherical_(sh4q,FALSE)
         call create_(Iabab,na,nb,na,nb)
         call make_ERI(sh4q,Iabab)
         call destroy_ptr_part_(sh4q)
         Imax = ZERO
         do a = 1,na
            do b = 1,nb
               Imax = max(abs(Iabab(a,b,a,b)),Imax)
            end do
         end do
         I_max(ab) = sqrt(Imax)
         call destroy_(Iabab)
      end do
   end subroutine

   PURE subroutine make_max_abab_so_integrals(self,I_max)
   ! Make "I_max", the square root of the maximum of the all (ab|ab)
   ! spin-orbit integrals.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: I_max
      INT :: ab,sa,sb,fa,fb,la,lb,na,nb,a,b
      type(SHELL4_TYPE) :: sh4
      MAT5(REAL,:,:,:,:,:), allocatable :: S,O
      REAL :: Imax
   ENSURE(size(I_max)==self%n_shell_pairs,"MOLECULE.FOCK:make_max_abab_so_integrals ... wrong size, I_max")
      call set_spherical_(sh4,self%use_spherical_basis)

      ! Store the largest integral for each shell pair ab|ab
      do ab = 1,self%n_shell_pairs  ! Store the largest integral for each
         call set_shell2_indices_from_(self,ab,sa,sb,fa,la,na,fb,lb,nb)
         call set_shell4_ab_from_(self,sa,sb,sh4)
         call set_shell4_cd_from_(self,sa,sb,sh4)
         call create_(S,na,nb,na,nb,3)
         call create_(O,na,nb,na,nb,3)
         call make_spin_orbit_ints_(sh4,S,O)
         call destroy_ptr_part_(sh4)
         Imax = ZERO
         do a = 1,na
            do b = 1,nb
               Imax = max(abs(S(a,b,a,b,1)),abs(S(a,b,a,b,2)),abs(S(a,b,a,b,3)),abs(O(a,b,a,b,1)),abs(O(a,b,a,b,2)),abs(O(a,b,a,b,3)),Imax)
            end do
         end do
         I_max(ab) = sqrt(Imax)
         call destroy_(O)
         call destroy_(S)
      end do
   end subroutine

   PURE subroutine make_max_S_integrals(self,S_max)
   ! Make "S_max", the maximum of the overlap integrals between basis
   ! functions in all pairs of shells.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: S_max
      INT :: ab
      INT :: sa,sb,na,nb
      INT :: fa,la,fb,lb
   ENSURE(size(S_max)==self%n_shell_pairs,"MOLECULE.FOCK:make_max_S_integrals ... wrong size, S_max")
      do ab = 1,self%n_shell_pairs
         call set_shell2_indices_from_(self,ab,sa,sb,fa,la,na,fb,lb,nb)
         S_max(ab) = maxval(abs(self%overlap_mx(fa:la,fb:lb)))
      end do
   end subroutine


!  ===============
!  J-matrix engine
!  ===============

   PURE subroutine reverse_transfer(self,P,PV)
   ! Apply the reverse horizontal recursion relation (i.e. the reverse
   ! transfer relation) to matrix "P" to produce the shell pair vector
   ! "PV" which has to be multipled by the (es|fs) source integrals to
   ! give the J-matrix.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), IN :: P
      VEC(type(EVEC_REAL_TYPE),:), allocatable, OUT :: PV
      VEC(REAL,3) :: R
      VEC(REAL,:), allocatable :: Rx,Ry,Rz
      VEC(INT,:), allocatable :: ax,ay,az,bx,by,bz,binx,biny,binz
      INT :: q,fa,la,na,fb,lb,nb,atom_a,atom_b,aa,bb,af,al,bf,bl,f
      INT :: i,k,a,b,x,y,z,xa,ya,za,xb,yb,zb,xx,yy,zz
      BIN :: new_atom_pair,same_atoms,a_gt_b,a_lt_b
      REAL :: R1,R2,R3,T1,T2,T3,fx,fy,fz,P_ab
      VEC(REAL,:), allocatable :: ab
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:reverse_transfer ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:reverse_transfer ... no atom info")
      call create_(Rx,self%basis_l_max+1)
      call create_(Ry,self%basis_l_max+1)
      call create_(Rz,self%basis_l_max+1)
      call create_(PV,self%n_shell_pairs)  ! leaky
      a_gt_b = TRUE
      a_lt_b = TRUE
      aa = 0
      bb = 0
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         call create_(ab,sh%n_gaussian)  ! leaky
      ! PV(q).element.create(sh.n_gaussian)                   ! leaky
      ! ab => PV(q).element
         if (sh%l_sum==0) then  ! SS case
            ab(1) = P(fa,fb)
         else if (sh%a%l==0) then  ! XS case
            fb = fb-1
            if (sh%l_max<2) then  ! No normalizers
               do b = 1,sh%b%n_comp
                  ab(b) = P(fa,fb+b)
               end do
            else  ! Include normalizers
               do b = 1,sh%b%n_comp
                  ab(b) = P(fa,fb+b)
               end do
            end if
         else if (sh%b%l==0) then  ! SX case
            fa = fa-1
            if (sh%l_max<2) then  ! No normalizers
               do a = 1,sh%a%n_comp
                  ab(a) = P(fa+a,fb)
               end do
            else  ! Include normalizers
               do a = 1,sh%a%n_comp
                  ab(a) = P(fa+a,fb)
               end do
            end if
         else  ! General case
            ab = ZERO
            af = n_comp_up_to_(sh%a%l-1)+1
            al = af+sh%a%n_comp-1
            bf = n_comp_up_to_(sh%b%l-1)+1
            bl = bf+sh%b%n_comp-1
            ax = nx(af:al)
            ay = ny(af:al)
            az = nz(af:al)
            bx = nx(bf:bl)
            by = ny(bf:bl)
            bz = nz(bf:bl)
            af = af-1
            bf = bf-1
            fa = fa-1
            fb = fb-1
            new_atom_pair = atom_a/=aa OR atom_b/=bb
            same_atoms = atom_a==atom_b
            aa = atom_a; bb = atom_b
            if (same_atoms) then  ! No transfer
               if (sh%a%l>sh%b%l) then; f = af
               else; f = bf
               end if
               if (sh%l_max<2) then  ! No normalising factors
                  do b = 1,sh%b%n_comp
                     xb = bx(b); yb = by(b); zb = bz(b)
                     do a = 1,sh%a%n_comp
                        P_ab = P(fa+a,fb+b)
                        xa = ax(a); ya = ay(a); za = az(a)
                        xx = xa+xb-1
                        yy = ya+yb-1
                        zz = za+zb-1
                        i = index_of(xx,yy,zz)-f
                        ab(i) = ab(i)+P_ab
                     end do
                  end do
               else  ! Include normalizers
                  k = 0
                  do b = 1,sh%b%n_comp
                     xb = bx(b); yb = by(b); zb = bz(b)
                     do a = 1,sh%a%n_comp
                        k = k+1
                        P_ab = P(fa+a,fb+b)
                        xa = ax(a); ya = ay(a); za = az(a)
                        xx = xa+xb-1
                        yy = ya+yb-1
                        zz = za+zb-1
                        i = index_of(xx,yy,zz)-f
                        ab(i) = ab(i)+P_ab
                     end do
                  end do
               end if
            else if (sh%a%l>sh%b%l) then  ! Transfer to a
               if (new_atom_pair OR a_lt_b) then
                  a_gt_b = TRUE; a_lt_b = FALSE
                  R = sh%a%position-sh%b%position
                  R1 = R(1); R2 = R(2); R3 = R(3)
                  T1 = R1; T2 = R2; T3 = R3
                  Rx(1) = ONE; Ry(1) = ONE; Rz(1) = ONE
                  do i = 2,sh%l_max+1
                     Rx(i) = T1; T1 = R1*T1
                     Ry(i) = T2; T2 = R2*T2
                     Rz(i) = T3; T3 = R3*T3
                  end do
               end if
               if (sh%l_max<2) then  ! No normalizers
                  do b = 1,sh%b%n_comp
                     xb = bx(b); yb = by(b); zb = bz(b)
                     binx = binomial_p1(xb)%element
                     biny = binomial_p1(yb)%element
                     binz = binomial_p1(zb)%element
                     do a = 1,sh%a%n_comp
                        P_ab = P(fa+a,fb+b)
                        xa = ax(a); ya = ay(a); za = az(a)
                        do x = 1,xb
                           xx = xa+x-1
                           fx = binx(x)*Rx(xb-x+1)*P_ab
                           do y = 1,yb
                              yy = ya+y-1
                              fy = biny(y)*Ry(yb-y+1)
                              do z = 1,zb
                                 zz = za+z-1
                                 fz = binz(z)*Rz(zb-z+1)
                                 i = index_of(xx,yy,zz)-af
                                 ab(i) = ab(i)+fx*fy*fz
                              end do
                           end do
                        end do
                     end do
                  end do
               else  ! Include normalizers
                  k = 0
                  do b = 1,sh%b%n_comp
                     xb = bx(b); yb = by(b); zb = bz(b)
                     binx = binomial_p1(xb)%element
                     biny = binomial_p1(yb)%element
                     binz = binomial_p1(zb)%element
                     do a = 1,sh%a%n_comp
                        k = k+1
                        P_ab = P(fa+a,fb+b)
                        xa = ax(a); ya = ay(a); za = az(a)
                        do x = 1,xb
                           xx = xa+x-1
                           fx = binx(x)*Rx(xb-x+1)*P_ab
                           do y = 1,yb
                              yy = ya+y-1
                              fy = biny(y)*Ry(yb-y+1)
                              do z = 1,zb
                                 zz = za+z-1
                                 fz = binz(z)*Rz(zb-z+1)
                                 i = index_of(xx,yy,zz)-af
                                 ab(i) = ab(i)+fx*fy*fz
                              end do
                           end do
                        end do
                     end do
                  end do
               end if
            else  ! Transfer to b
               if (new_atom_pair OR a_gt_b) then
                  a_gt_b = FALSE; a_lt_b = TRUE
                  R = sh%b%position-sh%a%position
                  R1 = R(1); R2 = R(2); R3 = R(3)
                  T1 = R1; T2 = R2; T3 = R3
                  Rx(1) = ONE; Ry(1) = ONE; Rz(1) = ONE
                  do i = 2,sh%l_max+1
                     Rx(i) = T1; T1 = R1*T1
                     Ry(i) = T2; T2 = R2*T2
                     Rz(i) = T3; T3 = R3*T3
                  end do
               end if
               if (sh%l_max<2) then  ! No normalizers
                  do a = 1,sh%a%n_comp
                     xa = ax(a); ya = ay(a); za = az(a)
                     binx = binomial_p1(xa)%element
                     biny = binomial_p1(ya)%element
                     binz = binomial_p1(za)%element
                     do b = 1,sh%b%n_comp
                        P_ab = P(fa+a,fb+b)
                        xb = bx(b); yb = by(b); zb = bz(b)
                        do x = 1,xa
                           xx = xb+x-1
                           fx = binx(x)*Rx(xa-x+1)*P_ab
                           do y = 1,ya
                              yy = yb+y-1
                              fy = biny(y)*Ry(ya-y+1)
                              do z = 1,za
                                 zz = zb+z-1
                                 fz = binz(z)*Rz(za-z+1)
                                 i = index_of(xx,yy,zz)-bf
                                 ab(i) = ab(i)+fx*fy*fz
                              end do
                           end do
                        end do
                     end do
                  end do
               else  ! Include normalizers
                  k = 0
                  do b = 1,sh%b%n_comp
                     xb = bx(b); yb = by(b); zb = bz(b)
                     do a = 1,sh%a%n_comp
                        xa = ax(a); ya = ay(a); za = az(a)
                        binx = binomial_p1(xa)%element
                        biny = binomial_p1(ya)%element
                        binz = binomial_p1(za)%element
                        k = k+1
                        P_ab = P(fa+a,fb+b)
                        do x = 1,xa
                           xx = xb+x-1
                           fx = binx(x)*Rx(xa-x+1)*P_ab
                           do y = 1,ya
                              yy = yb+y-1
                              fy = biny(y)*Ry(ya-y+1)
                              do z = 1,za
                                 zz = zb+z-1
                                 fz = binz(z)*Rz(za-z+1)
                                 i = index_of(xx,yy,zz)-bf
                                 ab(i) = ab(i)+fx*fy*fz
                              end do
                           end do
                        end do
                     end do
                  end do
               end if
            end if  ! Trasfer to b
         end if  ! general case
         call move_alloc(ab,PV(q)%element)
         call destroy_ptr_part_(sh)
      end do  ! loop over shell pairs
      call destroy_(Rz)
      call destroy_(Ry)
      call destroy_(Rx)
   end subroutine

   PURE subroutine forward_transfer(self,JJ,J)
   ! Apply the horizontal recursion relation (i.e. the transfer
   ! relation) to shellpair vector "JJ" to produce the matrix "J".
   ! This is a post-processing operation to shift momentum from
   ! center a -> b.
      type(MOLECULE_TYPE), IN :: self
      VEC(type(EVEC_REAL_TYPE),:), IN :: JJ
      MAT(REAL,:,:), OUT :: J
      INT :: q,fa,la,na,fb,lb,nb
      MAT(REAL,:,:), allocatable :: W
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:forward_transfer ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:forward_transfer ... no atom info")
   ENSURE(size(JJ)==self%n_shell_pairs,"MOLECULE.FOCK:forward_transfer ... wrong size, JJ")
   ENSURE(size(J,1)==self%n_bf OR size(J,1)==self%n_bf_cartesian,"MOLECULE.FOCK:forward_transfer ... wrong size, J")
   ENSURE(is_square_(J),"MOLECULE.FOCK:forward_transfer ... wrong shape, J")
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call create_(W,sh%a%n_comp,sh%b%n_comp)
         call transfer_(sh,JJ(q)%element,W)
         J(fa:la,fb:lb) = W
         call destroy_(W)
         call destroy_ptr_part_(sh)
      end do
   end subroutine

   PURE subroutine create_transfer(self,JJ)
   ! Create space for the transfer vector "JJ". It is a shellpair
   ! vector of flattened es-type componenets from which ab components
   ! can be formed by the transfer relation.
      type(MOLECULE_TYPE), IN :: self
      VEC(type(EVEC_REAL_TYPE),:), allocatable, INOUT :: JJ
      INT :: q,fa,la,na,fb,lb,nb
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:create_transfer ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:create_transfer ... no atom info")

      ! Leaky
      call create_(JJ,self%n_shell_pairs)
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call create_(JJ(q)%element,sh%n_gaussian)  ! leaky
         JJ(q)%element = ZERO
         call destroy_ptr_part_(sh)
      end do
   end subroutine


!  ===========================
!  J & K shell-block weighting
!  ===========================

   pure subroutine weight_diagonal_blocks_0(self,X,fac)
   ! Weight the diagonal blocks of matrix "X" by "fac"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: X
      REAL, IN :: fac
      INT :: n,f,l
      do n = 1,self%n_shell
         f = self%first_basis_fn_for_shell(n)
         l = self%last_basis_fn_for_shell(n)
         X(f:l,f:l) = fac*X(f:l,f:l)
      end do
   end subroutine

   pure subroutine weight_diagonal_blocks_23(self,X,fac)
   ! Weight last diagonal blocks of matrices "X" by "fac"
      type(MOLECULE_TYPE), IN :: self
      MAT3(REAL,:,:,:), INOUT :: X
      REAL, IN :: fac
      INT :: n,f,l
      do n = 1,self%n_shell
         f = self%first_basis_fn_for_shell(n)
         l = self%last_basis_fn_for_shell(n)
         X(:,f:l,f:l) = fac*X(:,f:l,f:l)
      end do
   end subroutine

   pure subroutine weight_diagonal_blocks_1(self,X,fac)
   ! Weight the diagonal blocks of matrix "X" by "fac"
      type(MOLECULE_TYPE), IN :: self
      MAT(CPX,:,:), INOUT :: X
      REAL, IN :: fac
      INT :: n,f,l
      do n = 1,self%n_shell
         f = self%first_basis_fn_for_shell(n)
         l = self%last_basis_fn_for_shell(n)
         X(f:l,f:l) = fac*X(f:l,f:l)
      end do
   end subroutine

   pure subroutine set_diagonal_blocks(self,X,fac)
   ! Set the diagonal blocks of matrix "X" to "fac"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: X
      REAL, IN :: fac
      INT :: n,f,l
      do n = 1,self%n_shell
         f = self%first_basis_fn_for_shell(n)
         l = self%last_basis_fn_for_shell(n)
         X(f:l,f:l) = fac
      end do
   end subroutine

   pure subroutine blockwise_symmetric_fold(self,X)
   ! Symmetrically fold the matrix "X" blockwise
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: X
      INT :: a,fa,la
      INT :: b,fb,lb
      do a = 1,self%n_shell
         do b = 1,a
            fa = self%first_basis_fn_for_shell(a)
            la = self%last_basis_fn_for_shell(a)
            fb = self%first_basis_fn_for_shell(b)
            lb = self%last_basis_fn_for_shell(b)
            X(fa:la,fb:lb) = X(fa:la,fb:lb)+transpose(X(fb:lb,fa:la))
         end do
      end do
   end subroutine

   pure subroutine blockwise_symmetric_fold_23(self,X)
   ! Symmetrically fold the matrix "X" on last blocks
      type(MOLECULE_TYPE), IN :: self
      MAT3(REAL,:,:,:), INOUT :: X
      INT :: a,fa,la
      INT :: b,fb,lb
      INT :: n
      do a = 1,self%n_shell
         do b = 1,a
            fa = self%first_basis_fn_for_shell(a)
            la = self%last_basis_fn_for_shell(a)
            fb = self%first_basis_fn_for_shell(b)
            lb = self%last_basis_fn_for_shell(b)
            do n = 1,size(X,1)
               X(n,fa:la,fb:lb) = X(n,fa:la,fb:lb)+transpose(X(n,fb:lb,fa:la))
            end do
         end do
      end do
   end subroutine

   pure subroutine blockwise_hermitian_fold(self,X)
   ! Hermitian fold the matrix "X" blockwise
      type(MOLECULE_TYPE), IN :: self
      MAT(CPX,:,:), INOUT :: X
      INT :: a,fa,la
      INT :: b,fb,lb
      do a = 1,self%n_shell
         do b = 1,a
            fa = self%first_basis_fn_for_shell(a)
            la = self%last_basis_fn_for_shell(a)
            fb = self%first_basis_fn_for_shell(b)
            lb = self%last_basis_fn_for_shell(b)
            X(fa:la,fb:lb) = X(fa:la,fb:lb)+transpose(conjg(X(fb:lb,fa:la)))
         end do
      end do
   end subroutine


!  ==================
!  Atom group testing
!  ==================

   function in_same_atom_group_0(self,atom_a,atom_b,group) result (res)
   ! Return TRUE if the atoms are all in the same atom_group
   ! If present, "group" is set to the common group index.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: atom_a,atom_b
      INT, optional, OUT :: group
      BIN :: res
      INT :: n,n_group
      BIN :: found
   ENSURE(allocated(self%atom_group),"MOLECULE.FOCK:in_same_atom_group_0 ... no atom group information")
   ENSURE(atom_a<=size(self%atom),"MOLECULE.FOCK:in_same_atom_group_0 ... atom_a out of range")
   ENSURE(atom_b<=size(self%atom),"MOLECULE.FOCK:in_same_atom_group_0 ... atom_b out of range")
      n_group = size(self%atom_group)
      res = TRUE
      found = FALSE
      do n = 1,n_group
         if (any(atom_a==self%atom_group(n)%atom_index)) then
            found = TRUE
            if (NOT any(atom_b==self%atom_group(n)%atom_index)) res = FALSE
            exit
         end if
      end do
      ENSURE(found,"MOLECULE.FOCK:in_same_atom_group_0 ... atom_a was not found in any atom_group")
      if (present(group)) then
         if (res) then; group = n
         else; group = 0
         end if
      end if
   end function

   function in_same_atom_group_1(self,atom_a,atom_b,atom_c,atom_d) result (res)
   ! Return TRUE if the atoms are all in the same atom_group
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: atom_a,atom_b
      INT, IN :: atom_c,atom_d
      BIN :: res
      INT :: n,n_group
      BIN :: found
   ENSURE(allocated(self%atom_group),"MOLECULE.FOCK:in_same_atom_group_1 ... no atom group information")
   ENSURE(atom_a<=size(self%atom),"MOLECULE.FOCK:in_same_atom_group_1 ... atom_a out of range")
   ENSURE(atom_b<=size(self%atom),"MOLECULE.FOCK:in_same_atom_group_1 ... atom_b out of range")
   ENSURE(atom_c<=size(self%atom),"MOLECULE.FOCK:in_same_atom_group_1 ... atom_c out of range")
   ENSURE(atom_d<=size(self%atom),"MOLECULE.FOCK:in_same_atom_group_1 ... atom_d out of range")
      n_group = size(self%atom_group)
      res = TRUE
      found = FALSE
      do n = 1,n_group
         if (any(atom_a==self%atom_group(n)%atom_index)) then
            found = TRUE
            if (NOT any(atom_b==self%atom_group(n)%atom_index)) res = FALSE
            if (NOT any(atom_c==self%atom_group(n)%atom_index)) res = FALSE
            if (NOT any(atom_d==self%atom_group(n)%atom_index)) res = FALSE
            exit
         end if
      end do
      ENSURE(found,"MOLECULE.FOCK:in_same_atom_group_1 ... atom_a was not found in any atom_group")
   end function


!  ==============
!  Restricted DFT
!  ==============

   subroutine add_XC_mx(self,K)
   ! Add the DFT exchange correlation matrix to "K".
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: K
   ENSURE(allocated(self%SCF_data),"MOLECULE.FOCK:add_XC_mx ... no SCF_data")
   ENSURE(is_DFT_calculation_(self%SCF_data),"MOLECULE.FOCK:add_XC_mx ... this is not a DFT calculation")
   ENSURE(allocated(self%density_mx),"MOLECULE.FOCK:add_XC_mx ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.FOCK:add_XC_mx ... no density matrix")
   ENSURE(has_genre_(K,spinorbital_genre_(self%SCF_data)),"MOLECULE.FOCK:add_XC_mx ... appropriate part of K not created")

      ! Now add the contribution
      select case (self%SCF_data%SCF_kind)

      case ("rks","xray_rks")
         call add_r_XC_mx_(self,K%r)

      case ("uks","xray_uks","pnd_uks","xray_pnd_uks")
         call add_u_XC_mx_(self,K%a,K%b)
      end select
   end subroutine

   subroutine add_r_XC_mx(self,K)
   ! Calculate the exchange correlation matrix numerically.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: K
      REAL :: E,E_xc
   ENSURE(allocated(self%SCF_data),"MOLECULE.FOCK:add_r_XC_mx ... no SCF_data")
   ENSURE(is_DFT_calculation_(self%SCF_data),"MOLECULE.FOCK:add_r_XC_mx ... this is not a DFT calculation")
   DIE_IF(using_meta_GGA_functional_(self%SCF_data) AND NOT self%SCF_data%using_batched_XC,"MOLECULE.FOCK:add_r_XC_mx ... a meta-GGA functional needs use_batched_xc= TRUE")
      if (self%SCF_data%using_batched_XC) then
         call add_XC_mx_batched_(self,K,E,E_xc)
      else if (using_GGA_functional_(self%SCF_data)) then
         call add_GGA_XC_mx_(self,K,E,E_xc)
      else
         call add_LDA_XC_mx_(self,K,E,E_xc)
      end if
      self%SCF_data%DFT_energy_correction = E
      self%SCF_data%XC_energy = E_xc
   end subroutine

   subroutine add_u_XC_mx(self,Ka,Kb)
   ! Calculate the exchange correlation matrix numerically.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: Ka,Kb
      REAL :: E,Ea,Eb,E_xc
   DIE_IF(using_meta_GGA_functional_(self%SCF_data) AND NOT self%SCF_data%using_batched_XC,"MOLECULE.FOCK:add_u_XC_mx ... a meta-GGA functional needs use_batched_xc= TRUE")
      if (self%SCF_data%using_batched_XC) then
         call add_XC_mx_batched_(self,Ka,Kb,E,Ea,Eb,E_xc)
      else if (NOT using_GGA_functional_(self%SCF_data)) then
         call add_LDA_XC_mx_(self,Ka,Kb,E,Ea,Eb,E_xc)
      else
         call add_GGA_XC_mx_(self,Ka,Kb,E,Ea,Eb,E_xc)
      end if
      self%SCF_data%DFT_energy_correction = E
      self%SCF_data%DFT_alpha_energy_correction = Ea
      self%SCF_data%DFT_beta_energy_correction = Eb
      self%SCF_data%XC_energy = E_xc
   end subroutine


! LDA

   subroutine add_LDA_XC_mx_0(self,K,E,E_xc)
   ! Add the exchange and correlation matrices to K. This routine supports
   ! local functional only, either exchange or correlation.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: K
      REAL, OUT :: E
      REAL, OUT :: E_xc
      MAT(REAL,:,:), allocatable :: V,pt
      VEC(REAL,:), allocatable :: wt
      type(XC_DENSITY_TYPE) :: den
      type(XC_POTENTIAL_TYPE) :: pot
      VEC(type(EVEC_BIN_TYPE),:), allocatable :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: bf_grid0
      INT :: c
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:add_LDA_XC_mx_0 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:add_LDA_XC_mx_0 ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.FOCK:add_LDA_XC_mx_0 ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.FOCK:add_LDA_XC_mx_0 ... no restricted density matrix")

      ! Main temporaries
      E = ZERO
      call create_(V,self%n_bf,self%n_bf); V = ZERO

      ! Loop over integration atoms "c"
      do c = 1,self%n_atom

         ! Make electron density on the Becke-grid for atom "c"
         call make_rho_becke_atom_grid_(self,den,pt,wt,bf_skip,bf_grid0,c,self%density_mx%r)

         ! Make the XC energy per particle and potential on the grid
         call destroy_(pt)
         call set_XC_energy_and_potential_(self,pot,den)

         ! Incorporate grid & partition weights
         call multiply_by_(pot,wt)

         ! Clean
         call destroy_ptr_part_(den)
         call destroy_(wt)

         ! Add the XC matrix elements into V, energy to E
         ! for atom "c" Becke-grid
         call add_LDA_XC_mx_(self,V,E,pot,bf_skip,bf_grid0,c,self%density_mx%r)

         ! Clean up
         call destroy_ptr_part_(pot)
         call destroy_(bf_grid0)
         call destroy_(bf_skip)
      end do  ! -- integration atom c

      ! Add the XC contribution to K
      K = K+V
      call symmetric_reflect_(K)

      ! Get DFT energy correction
      call symmetric_reflect_(V)
      E_xc = E  ! the XC energy itself, before it becomes the energy correction
      E = E-HALF*trace_product_with_(V,self%density_mx%r)

      ! Clean
      call destroy_(V)
   end subroutine

   subroutine add_LDA_XC_mx_1(self,V,E,pot,bf_skip,bf_grid0,c,D)
   ! Add XC potential matrix elements to "V", the XC energy to "E".
   ! "pot" holds the XC potential and the XC energy per particle on the
   ! becke grid for atom "c", both weighted by the partition function
   ! for that atom.
   ! "bf_skip" and "bf_grid0 are basis function grids.  "D" is the
   ! density matrix.  This routine supports local functional only.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), target, INOUT :: V
      REAL, INOUT :: E
      type(XC_POTENTIAL_TYPE), IN :: pot
      VEC(type(EVEC_BIN_TYPE),:), IN :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), IN :: bf_grid0
      INT, IN :: c
      MAT(REAL,:,:), target, IN :: D
      VEC(REAL,:), allocatable :: ga0,gb0
      VEC(INT,:), allocatable :: overlapping_atom,pi,pj,pn
      VEC(BIN,:), allocatable :: skipab,skipa,skipb
      MAT(REAL,:,:), PTR :: VV,DD
      REAL :: Dab,fac,val,wal,ga,gb,gab
      INT :: n_pt,n_keep,ca,cb,oa,fsa,lsa,ob,fsb,lsb
      INT :: sa,fa,la,na,sb,fb,lb,nb,a,b,p,i,j,n
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:add_LDA_XC_mx_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:add_LDA_XC_mx_1 ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.FOCK:add_LDA_XC_mx_1 ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.FOCK:add_LDA_XC_mx_1 ... no restricted density matrix")
   ENSURE(allocated(self%becke_grid),"MOLECULE.FOCK:add_LDA_XC_mx_1 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.FOCK:add_LDA_XC_mx_1 ... no finalized becke_grid")

      ! Skip array for ab shell
      n_pt = size(pot%energy)
      call create_(skipab,n_pt)

      ! Overlapping atoms
      overlapping_atom = self%overlapping_atoms_for_atom(c)%element

      ! Loop c-overlapping atoms "oa"
      do ca = 1,size(overlapping_atom)
         oa = overlapping_atom(ca)
         fsa = self%first_shell_for_atom(oa)
         lsa = self%last_shell_for_atom(oa)

         ! Loop shells on atom a
         do sa = fsa,lsa
            if (NOT allocated(bf_skip(sa)%element)) cycle
            fa = self%first_basis_fn_for_shell(sa)
            la = self%last_basis_fn_for_shell(sa)
            na = la-fa+1

            ! Set the "sb" basis function grid
            skipa = bf_skip(sa)%element

            ! Loop c-overlapping atoms b
            do cb = 1,ca
               ob = overlapping_atom(cb)
               fsb = self%first_shell_for_atom(ob)
               lsb = self%last_shell_for_atom(ob)

               ! Loop shells on atom b
               do sb = fsb,min(lsb,sa)
                  if (NOT allocated(bf_skip(sb)%element)) cycle
                  fb = self%first_basis_fn_for_shell(sb)
                  lb = self%last_basis_fn_for_shell(sb)
                  nb = lb-fb+1

                  ! Set the "sb" basis function grid
                  skipb = bf_skip(sb)%element

                  ! How many points in common?
                  skipab(1:n_pt) = skipa OR skipb
                  n_keep = count(NOT skipab(1:n_pt))
                  if (n_keep==0) cycle

                  ! Factor
                  fac = TWO
                  if (sa==sb) fac = ONE

                  ! Density block
                  VV => V(fa:la,fb:lb)
                  DD => D(fa:la,fb:lb)
                  if (na*nb==1) then
                     ga0 = bf_grid0(sa)%element(:,1)
                     gb0 = bf_grid0(sb)%element(:,1)
                     Dab = fac*DD(1,1)
                     i = 0
                     j = 0
                     val = ZERO
                     wal = ZERO
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        ga = ga0(i)
                        gb = gb0(j)
                        gab = ga*gb
                        val = val+pot%v_rho(n,1)*gab
                        wal = wal+pot%energy(n)*gab
                     end do
                     VV(1,1) = VV(1,1)+val
                     E = E+wal*Dab
                  else

                     ! Create space for gathered index info
                     call create_(pi,n_keep)
                     call create_(pj,n_keep)
                     call create_(pn,n_keep)

                     ! Make mapper arrays
                     p = 0
                     i = 0
                     j = 0
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        p = p+1
                        pi(p) = i
                        pj(p) = j
                        pn(p) = n
                     end do

                     ! Add density contribution to rho
                     do a = 1,na
                        ga0 = bf_grid0(sa)%element(:,a)
                        do b = 1,nb
                           gb0 = bf_grid0(sb)%element(:,b)
                           Dab = fac*DD(a,b)
                           val = ZERO
                           wal = ZERO
                           do p = 1,n_keep
                              i = pi(p)
                              j = pj(p)
                              n = pn(p)
                              ga = ga0(i)
                              gb = gb0(j)
                              gab = ga*gb
                              val = val+pot%v_rho(n,1)*gab
                              wal = wal+pot%energy(n)*gab
                           end do
                           VV(a,b) = VV(a,b)+val
                           E = E+wal*Dab
                        end do
                     end do

                     ! Clean up
                     call destroy_(pn)
                     call destroy_(pj)
                     call destroy_(pi)
                  end if
               end do  ! -- loops sa,sb
            end do
         end do  ! -- lops ca,cb
      end do

      ! Clean up
      call destroy_(skipab)
   end subroutine


! GGA

   subroutine add_GGA_XC_mx_0(self,K,E,E_xc)
   ! Add the exchange and correlation matrices to K. This routine
   ! supports non-local functionals, either exchange or correlation.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: K
      REAL, OUT :: E
      REAL, OUT :: E_xc
      VEC(REAL,:), allocatable :: wt
      type(XC_DENSITY_TYPE) :: den
      type(XC_POTENTIAL_TYPE) :: pot
      MAT(REAL,:,:), allocatable :: V,pt
      VEC(type(EVEC_BIN_TYPE),:), allocatable :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: bf_grid0
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: bf_gridx,bf_gridy,bf_gridz
      INT :: c
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:add_GGA_XC_mx_0 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:add_GGA_XC_mx_0 ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.FOCK:add_GGA_XC_mx_0 ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.FOCK:add_GGA_XC_mx_0 ... no restricted density matrix")

      ! Main temporaries
      E = ZERO
      call create_(V,self%n_bf,self%n_bf); V = ZERO

      ! Loop over integration atoms "c"
      do c = 1,self%n_atom

         ! Make electron density on the Becke-grid for atom "c"
         call make_rho_becke_atom_grid_(self,den,pt,wt,bf_skip,bf_grid0,bf_gridx,bf_gridy,bf_gridz,c,self%density_mx%r)

         ! Make the XC energy per particle and potentials on the grid
         call destroy_(pt)
         call set_XC_energy_and_potential_(self,pot,den)

         ! Incorporate grid & partition weights
         call multiply_by_(pot,wt)

         ! Clean
         call destroy_ptr_part_(den)
         call destroy_(wt)

         ! Add the XC matrix elements into V, energy to E
         ! for atom "c" Becke-grid
         call add_GGA_XC_mx_(self,V,E,pot,bf_skip,bf_grid0,bf_gridx,bf_gridy,bf_gridz,c,self%density_mx%r)

         ! Clean up
         call destroy_ptr_part_(pot)
         call destroy_(bf_gridz)
         call destroy_(bf_gridy)
         call destroy_(bf_gridx)
         call destroy_(bf_grid0)
         call destroy_(bf_skip)
      end do  ! -- integration atom c

      ! Add the XC contribution to K
      K = K+V
      call symmetric_reflect_(K)

      ! Get DFT energy correction
      call symmetric_reflect_(V)
      E_xc = E  ! the XC energy itself, before it becomes the energy correction
      E = E-HALF*trace_product_with_(V,self%density_mx%r)

      ! Clean
      call destroy_(V)
   end subroutine

   subroutine add_GGA_XC_mx_1(self,V,E,pot,bf_skip,bf_grd0,bf_grdx,bf_grdy,bf_grdz,c,D)
   ! Add XC potential matrix elements to "V", the XC energy to "E".
   ! "pot" holds the XC potential, its gradient part, and the XC energy
   ! per particle on the becke grid for atom "c", all weighted by the
   ! partition function for that atom.  "bf_skip",
   ! "bf_grd0", "bf_grdx", "bf_grdy", "bf_grdz" are basis function
   ! grids and their derivatives.  "D" is the density matrix.  This
   ! routine supports non-local functionals.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), target, INOUT :: V
      REAL, OUT :: E
      type(XC_POTENTIAL_TYPE), IN :: pot
      VEC(type(EVEC_BIN_TYPE),:), IN :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), IN :: bf_grd0
      VEC(type(EMAT_REAL_TYPE),:), IN :: bf_grdx,bf_grdy,bf_grdz
      INT, IN :: c
      MAT(REAL,:,:), target, IN :: D
      VEC(INT,:), allocatable :: overlapping_atom,pi,pj,pn
      VEC(BIN,:), allocatable :: skipab
      MAT(REAL,:,:), PTR :: VV,DD
      REAL :: Dab,fac,val,wal,ga,gb,gab
      INT :: n_pt,n_keep,ca,cb,oa,fsa,lsa,ob,fsb,lsb
      INT :: sa,fa,la,na,sb,fb,lb,nb,a,b,p,i,j,n
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:add_GGA_XC_mx_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:add_GGA_XC_mx_1 ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.FOCK:add_GGA_XC_mx_1 ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.FOCK:add_GGA_XC_mx_1 ... no restricted density matrix")
   ENSURE(allocated(self%becke_grid),"MOLECULE.FOCK:add_GGA_XC_mx_1 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.FOCK:add_GGA_XC_mx_1 ... no finalized becke_grid")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.FOCK:add_GGA_XC_mx_1 ... no overlapping_atoms_for_atom")

      ! Skip array and common-point maps for the ab shell pair
      n_pt = size(pot%energy)
      call create_(skipab,n_pt)
      call create_(pi,n_pt)
      call create_(pj,n_pt)
      call create_(pn,n_pt)

      ! Overlapping atoms
      overlapping_atom = self%overlapping_atoms_for_atom(c)%element

      ! Loop c-overlapping atoms "oa"
      do ca = 1,size(overlapping_atom)
         oa = overlapping_atom(ca)
         fsa = self%first_shell_for_atom(oa)
         lsa = self%last_shell_for_atom(oa)

         ! Loop shells on atom a
         do sa = fsa,lsa
            if (NOT allocated(bf_skip(sa)%element)) cycle
            fa = self%first_basis_fn_for_shell(sa)
            la = self%last_basis_fn_for_shell(sa)
            na = la-fa+1

            ! Loop c-overlapping atoms b
            do cb = 1,ca
               ob = overlapping_atom(cb)
               fsb = self%first_shell_for_atom(ob)
               lsb = self%last_shell_for_atom(ob)

               ! Loop shells on atom b
               do sb = fsb,min(lsb,sa)
                  if (NOT allocated(bf_skip(sb)%element)) cycle
                  fb = self%first_basis_fn_for_shell(sb)
                  lb = self%last_basis_fn_for_shell(sb)
                  nb = lb-fb+1

                  ! How many points in common?
                  skipab(1:n_pt) = bf_skip(sa)%element OR bf_skip(sb)%element
                  n_keep = count(NOT skipab(1:n_pt))
                  if (n_keep==0) cycle

                  ! Factor
                  fac = TWO
                  if (sa==sb) fac = ONE

                  ! Density block
                  VV => V(fa:la,fb:lb)
                  DD => D(fa:la,fb:lb)

                  ! Map common points to the "sa" and "sb" grids
                  p = 0
                  i = 0
                  j = 0
                  do n = 1,n_pt
                     if (NOT bf_skip(sa)%element(n)) i = i+1
                     if (NOT bf_skip(sb)%element(n)) j = j+1
                     if (skipab(n)) cycle
                     p = p+1
                     pi(p) = i
                     pj(p) = j
                     pn(p) = n
                  end do

                  ! Basis function grids are read in place, not copied
                  do a = 1,na
                     do b = 1,nb
                        Dab = fac*DD(a,b)
                        val = ZERO
                        wal = ZERO
                        do p = 1,n_keep
                           i = pi(p)
                           j = pj(p)
                           n = pn(p)
                           ga = bf_grd0(sa)%element(i,a)
                           gb = bf_grd0(sb)%element(j,b)
                           gab = ga*gb
                           val = val+pot%v_rho(n,1)*gab+pot%v_grad(n,1,1)*(bf_grdx(sa)%element(i,a)*gb+ga*bf_grdx(sb)%element(j,b))+pot%v_grad(n,2,1)*(bf_grdy(sa)%element(i,a)*gb+ga*bf_grdy(sb)%element(j,b))+pot%v_grad(n,3,1)*(bf_grdz(sa)%element(i,a)*gb+ga*bf_grdz(sb)%element(j,b))
                           wal = wal+pot%energy(n)*gab
                        end do
                        VV(a,b) = VV(a,b)+val
                        E = E+wal*Dab
                     end do
                  end do
               end do  ! -- loops sa,sb
            end do
         end do  ! -- lops ca,cb
      end do

      ! Clean up
      call destroy_(pn)
      call destroy_(pj)
      call destroy_(pi)
      call destroy_(skipab)
   end subroutine

   subroutine add_XC_mx_batched_0(self,K,E,E_xc)
   ! Add the restricted XC matrix to "K" and return the energy correction
   ! "E", as add_GGA_XC_mx(K,E) and add_LDA_XC_mx(K,E) do, batch by batch
   ! over the molecular grid (MOLECULE.RHO:make_XC_batches). Per batch, with
   ! G0 the significant basis functions on the batch points, Gx,Gy,Gz their
   ! gradients (GGA only), and Ds the matching block of D: X = G0 Ds,
   ! rho = sum G0*X, grad rho = 2 sum G*X; with V0 the XC potential and
   ! Vx,Vy,Vz its gradient part, Z = (w V0/2) G0 + w Vx Gx + w Vy Gy +
   ! w Vz Gz, and the XC matrix block is G0^T Z + Z^T G0. A meta-GGA also
   ! needs the kinetic energy density tau = (1/2) sum over x,y,z of
   ! sum G*(G Ds), and adds the block (1/2) sum over x,y,z of
   ! G^T (w Vtau) G, with Vtau the derivative of the functional with
   ! respect to tau.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: K
      REAL, OUT :: E
      REAL, OUT :: E_xc
      MAT(REAL,:,:), allocatable :: V,Ds,X,Z,Vs
      MAT(REAL,:,:), allocatable :: G0,Gx,Gy,Gz
      VEC(REAL,:), allocatable :: wt
      type(XC_DENSITY_TYPE) :: den
      type(XC_POTENTIAL_TYPE) :: pot
      VEC(REAL,:), allocatable :: xs,ys,zs,r2
      VEC(INT,:), allocatable :: bf
      VEC(REAL,3) :: pos
      type(SHELL1_TYPE) :: sh
      BIN :: gga,mgga
      INT :: b,f,np,ns,q,p,j,ix,s,fs,ls,fa,na,off
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:add_XC_mx_batched_0 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:add_XC_mx_batched_0 ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.FOCK:add_XC_mx_batched_0 ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.FOCK:add_XC_mx_batched_0 ... no restricted density matrix")
   ENSURE(allocated(self%becke_grid),"MOLECULE.FOCK:add_XC_mx_batched_0 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.FOCK:add_XC_mx_batched_0 ... no finalized becke_grid")
      gga = using_GGA_functional_(self%SCF_data)
      mgga = using_meta_GGA_functional_(self%SCF_data)
      if (NOT allocated(self%XC_pts)) call make_XC_grid_(self)
      if (NOT allocated(self%XC_batch_first)) call make_XC_batches_(self)
      E = ZERO
      call create_(V,self%n_bf,self%n_bf)
      V = ZERO
      do b = 1,size(self%XC_batch_first)
         f = self%XC_batch_first(b)
         np = self%XC_batch_last(b)-f+1
         fs = self%XC_batch_first_shell(b)
         ls = self%XC_batch_last_shell(b)
         if (ls<fs) cycle

         ! Number of significant basis functions
         ns = 0
         do j = fs,ls
            s = self%XC_batch_shell(j)
            ns = ns+self%last_basis_fn_for_shell(s)-self%first_basis_fn_for_shell(s)+1
         end do

         ! Basis functions, and gradients for a GGA, shell by shell
         call create_(bf,ns)
         call create_(G0,np,ns)
         call create_(xs,np)
         call create_(ys,np)
         call create_(zs,np)
         if (gga) then
            call create_(Gx,np,ns)
            call create_(Gy,np,ns)
            call create_(Gz,np,ns)
            call create_(r2,np)
         else

            ! make_grid takes the points themselves
            do q = 1,np
               p = self%XC_batch_pt(f+q-1)
               xs(q) = self%XC_pts(p,1)
               ys(q) = self%XC_pts(p,2)
               zs(q) = self%XC_pts(p,3)
            end do
         end if
         off = 0
         do j = fs,ls
            s = self%XC_batch_shell(j)
            fa = self%first_basis_fn_for_shell(s)
            na = self%last_basis_fn_for_shell(s)-fa+1
            sh = self%basis_shell1pair(self%basis_shell_for_shell(s),1)%a
            pos = self%atom(self%atom_for_shell(s))%position
            sh%position = pos
            if (gga) then

               ! make_nabla_grid takes displacements from the shell
               do q = 1,np
                  p = self%XC_batch_pt(f+q-1)
                  xs(q) = self%XC_pts(p,1)-pos(1)
                  ys(q) = self%XC_pts(p,2)-pos(2)
                  zs(q) = self%XC_pts(p,3)-pos(3)
                  r2(q) = xs(q)*xs(q)+ys(q)*ys(q)+zs(q)*zs(q)
               end do
               call make_nabla_grid_(sh,Gx(:,off+1:off+na),Gy(:,off+1:off+na),Gz(:,off+1:off+na),G0(:,off+1:off+na),xs,ys,zs,r2)
            else
               call make_grid_(sh,G0(:,off+1:off+na),xs,ys,zs)
            end if
            do q = 1,na
               bf(off+q) = fa+q-1
            end do
            off = off+na
         end do
         if (gga) call destroy_(r2)
         call destroy_(zs)
         call destroy_(ys)
         call destroy_(xs)

         ! Density, and its gradient for a GGA
         call create_(Ds,ns,ns)
         Ds = self%density_mx%r(bf,bf)
         call create_(X,np,ns)
         call to_product_of_BLAS_(X,G0,Ds)
         call create_grids_(den,np,1,gga,mgga)
         do j = 1,ns
            den%rho(:,1) = den%rho(:,1)+G0(:,j)*X(:,j)
         end do
         if (gga) then
            do j = 1,ns
               den%grad(:,1,1) = den%grad(:,1,1)+Gx(:,j)*X(:,j)
               den%grad(:,2,1) = den%grad(:,2,1)+Gy(:,j)*X(:,j)
               den%grad(:,3,1) = den%grad(:,3,1)+Gz(:,j)*X(:,j)
            end do
            den%grad = TWO*den%grad
         end if
         call destroy_(X)
         if (mgga) then
            call add_density_of_grid_fns_(self,den%tau(:,1),Gx,Ds)
            call add_density_of_grid_fns_(self,den%tau(:,1),Gy,Ds)
            call add_density_of_grid_fns_(self,den%tau(:,1),Gz,Ds)
            den%tau = HALF*den%tau
         end if
         call destroy_(Ds)

         ! Functional
         call create_(wt,np)
         do q = 1,np
            wt(q) = self%XC_wts(self%XC_batch_pt(f+q-1))
         end do
         call set_XC_energy_and_potential_(self,pot,den)
         E = E+sum(wt*pot%energy*den%rho(:,1))
         pot%v_rho(:,1) = HALF*wt*pot%v_rho(:,1)
         if (gga) then
            do ix = 1,3
               pot%v_grad(:,ix,1) = wt*pot%v_grad(:,ix,1)
            end do
         end if
         if (mgga) pot%v_tau(:,1) = QUARTER*wt*pot%v_tau(:,1)
         call destroy_ptr_part_(den)
         call destroy_(wt)

         ! XC matrix block
         call create_(Z,np,ns)
         if (gga) then
            do j = 1,ns
               Z(:,j) = pot%v_rho(:,1)*G0(:,j)+pot%v_grad(:,1,1)*Gx(:,j)+pot%v_grad(:,2,1)*Gy(:,j)+pot%v_grad(:,3,1)*Gz(:,j)
            end do
         else
            do j = 1,ns
               Z(:,j) = pot%v_rho(:,1)*G0(:,j)
            end do
         end if
         call create_(Vs,ns,ns)
         call to_product_of_BLAS_(Vs,G0,Z,transpose_a=TRUE)
         if (mgga) then
            call add_product_of_grid_fns_(self,Vs,Gx,pot%v_tau(:,1))
            call add_product_of_grid_fns_(self,Vs,Gy,pot%v_tau(:,1))
            call add_product_of_grid_fns_(self,Vs,Gz,pot%v_tau(:,1))
         end if
         V(bf,bf) = V(bf,bf)+Vs+transpose(Vs)
         call destroy_(Vs)
         call destroy_(Z)
         if (gga) then
            call destroy_(Gz)
            call destroy_(Gy)
            call destroy_(Gx)
         end if
         call destroy_ptr_part_(pot)
         call destroy_(G0)
         call destroy_(bf)
      end do

      ! Add the XC contribution to K
      K = K+V
      call symmetric_reflect_(K)

      ! Get DFT energy correction
      E_xc = E  ! the XC energy itself, before it becomes the energy correction
      E = E-HALF*trace_product_with_(V,self%density_mx%r)
      call destroy_(V)
   end subroutine

   subroutine add_XC_mx_batched_1(self,Ka,Kb,E,Ea,Eb,E_xc)
   ! As add_XC_mx_batched(K,E), for the open-shell case: one density and
   ! one XC block per spin, as add_GGA_XC_mx(Ka,Kb,E,Ea,Eb) and
   ! add_LDA_XC_mx(Ka,Kb,E,Ea,Eb) do.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: Ka,Kb
      REAL, OUT :: E,Ea,Eb
      REAL, OUT :: E_xc
      MAT(REAL,:,:), allocatable :: Va,Vb,Ds,X,Z,Vs
      MAT(REAL,:,:), allocatable :: G0,Gx,Gy,Gz
      VEC(REAL,:), allocatable :: wt
      type(XC_DENSITY_TYPE) :: den
      type(XC_POTENTIAL_TYPE) :: pot
      VEC(REAL,:), allocatable :: xs,ys,zs,r2
      VEC(INT,:), allocatable :: bf
      VEC(REAL,3) :: pos
      type(SHELL1_TYPE) :: sh
      BIN :: gga,mgga
      INT :: b,f,np,ns,q,p,j,ix,s,fs,ls,fa,na,off
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:add_XC_mx_batched_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:add_XC_mx_batched_1 ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.FOCK:add_XC_mx_batched_1 ... no density matrix")
   ENSURE(allocated(self%density_mx%a),"MOLECULE.FOCK:add_XC_mx_batched_1 ... no alpha density matrix")
   ENSURE(allocated(self%density_mx%b),"MOLECULE.FOCK:add_XC_mx_batched_1 ... no beta density matrix")
   ENSURE(allocated(self%becke_grid),"MOLECULE.FOCK:add_XC_mx_batched_1 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.FOCK:add_XC_mx_batched_1 ... no finalized becke_grid")
      gga = using_GGA_functional_(self%SCF_data)
      mgga = using_meta_GGA_functional_(self%SCF_data)
      if (NOT allocated(self%XC_pts)) call make_XC_grid_(self)
      if (NOT allocated(self%XC_batch_first)) call make_XC_batches_(self)
      Ea = ZERO
      Eb = ZERO
      call create_(Va,self%n_bf,self%n_bf); Va = ZERO
      call create_(Vb,self%n_bf,self%n_bf); Vb = ZERO
      do b = 1,size(self%XC_batch_first)
         f = self%XC_batch_first(b)
         np = self%XC_batch_last(b)-f+1
         fs = self%XC_batch_first_shell(b)
         ls = self%XC_batch_last_shell(b)
         if (ls<fs) cycle

         ! Number of significant basis functions
         ns = 0
         do j = fs,ls
            s = self%XC_batch_shell(j)
            ns = ns+self%last_basis_fn_for_shell(s)-self%first_basis_fn_for_shell(s)+1
         end do

         ! Basis functions, and gradients for a GGA, shell by shell
         call create_(bf,ns)
         call create_(G0,np,ns)
         call create_(xs,np)
         call create_(ys,np)
         call create_(zs,np)
         if (gga) then
            call create_(Gx,np,ns)
            call create_(Gy,np,ns)
            call create_(Gz,np,ns)
            call create_(r2,np)
         else

            ! make_grid takes the points themselves
            do q = 1,np
               p = self%XC_batch_pt(f+q-1)
               xs(q) = self%XC_pts(p,1)
               ys(q) = self%XC_pts(p,2)
               zs(q) = self%XC_pts(p,3)
            end do
         end if
         off = 0
         do j = fs,ls
            s = self%XC_batch_shell(j)
            fa = self%first_basis_fn_for_shell(s)
            na = self%last_basis_fn_for_shell(s)-fa+1
            sh = self%basis_shell1pair(self%basis_shell_for_shell(s),1)%a
            pos = self%atom(self%atom_for_shell(s))%position
            sh%position = pos
            if (gga) then

               ! make_nabla_grid takes displacements from the shell
               do q = 1,np
                  p = self%XC_batch_pt(f+q-1)
                  xs(q) = self%XC_pts(p,1)-pos(1)
                  ys(q) = self%XC_pts(p,2)-pos(2)
                  zs(q) = self%XC_pts(p,3)-pos(3)
                  r2(q) = xs(q)*xs(q)+ys(q)*ys(q)+zs(q)*zs(q)
               end do
               call make_nabla_grid_(sh,Gx(:,off+1:off+na),Gy(:,off+1:off+na),Gz(:,off+1:off+na),G0(:,off+1:off+na),xs,ys,zs,r2)
            else
               call make_grid_(sh,G0(:,off+1:off+na),xs,ys,zs)
            end if
            do q = 1,na
               bf(off+q) = fa+q-1
            end do
            off = off+na
         end do
         if (gga) call destroy_(r2)
         call destroy_(zs)
         call destroy_(ys)
         call destroy_(xs)

         ! Alpha and beta densities, and their gradients for a GGA
         call create_(Ds,ns,ns)
         call create_(X,np,ns)
         call create_grids_(den,np,2,gga,mgga)
         Ds = self%density_mx%a(bf,bf)
         call to_product_of_BLAS_(X,G0,Ds)
         do j = 1,ns
            den%rho(:,1) = den%rho(:,1)+G0(:,j)*X(:,j)
         end do
         if (gga) then
            do j = 1,ns
               den%grad(:,1,1) = den%grad(:,1,1)+Gx(:,j)*X(:,j)
               den%grad(:,2,1) = den%grad(:,2,1)+Gy(:,j)*X(:,j)
               den%grad(:,3,1) = den%grad(:,3,1)+Gz(:,j)*X(:,j)
            end do
            den%grad(:,:,1) = TWO*den%grad(:,:,1)
         end if
         if (mgga) then
            call add_density_of_grid_fns_(self,den%tau(:,1),Gx,Ds)
            call add_density_of_grid_fns_(self,den%tau(:,1),Gy,Ds)
            call add_density_of_grid_fns_(self,den%tau(:,1),Gz,Ds)
         end if
         Ds = self%density_mx%b(bf,bf)
         call to_product_of_BLAS_(X,G0,Ds)
         do j = 1,ns
            den%rho(:,2) = den%rho(:,2)+G0(:,j)*X(:,j)
         end do
         if (gga) then
            do j = 1,ns
               den%grad(:,1,2) = den%grad(:,1,2)+Gx(:,j)*X(:,j)
               den%grad(:,2,2) = den%grad(:,2,2)+Gy(:,j)*X(:,j)
               den%grad(:,3,2) = den%grad(:,3,2)+Gz(:,j)*X(:,j)
            end do
            den%grad(:,:,2) = TWO*den%grad(:,:,2)
         end if
         if (mgga) then
            call add_density_of_grid_fns_(self,den%tau(:,2),Gx,Ds)
            call add_density_of_grid_fns_(self,den%tau(:,2),Gy,Ds)
            call add_density_of_grid_fns_(self,den%tau(:,2),Gz,Ds)
         end if
         if (mgga) den%tau = HALF*den%tau
         call destroy_(X)
         call destroy_(Ds)

         ! Functional
         call create_(wt,np)
         do q = 1,np
            wt(q) = self%XC_wts(self%XC_batch_pt(f+q-1))
         end do
         call set_XC_energy_and_potential_(self,pot,den)
         Ea = Ea+sum(wt*pot%energy*den%rho(:,1))
         Eb = Eb+sum(wt*pot%energy*den%rho(:,2))
         do s = 1,2
            pot%v_rho(:,s) = HALF*wt*pot%v_rho(:,s)
            if (NOT gga) cycle
            do ix = 1,3
               pot%v_grad(:,ix,s) = wt*pot%v_grad(:,ix,s)
            end do
         end do
         if (mgga) then
            pot%v_tau(:,1) = QUARTER*wt*pot%v_tau(:,1)
            pot%v_tau(:,2) = QUARTER*wt*pot%v_tau(:,2)
         end if
         call destroy_ptr_part_(den)
         call destroy_(wt)

         ! Alpha and beta XC matrix blocks
         call create_(Z,np,ns)
         call create_(Vs,ns,ns)
         if (gga) then
            do j = 1,ns
               Z(:,j) = pot%v_rho(:,1)*G0(:,j)+pot%v_grad(:,1,1)*Gx(:,j)+pot%v_grad(:,2,1)*Gy(:,j)+pot%v_grad(:,3,1)*Gz(:,j)
            end do
         else
            do j = 1,ns
               Z(:,j) = pot%v_rho(:,1)*G0(:,j)
            end do
         end if
         call to_product_of_BLAS_(Vs,G0,Z,transpose_a=TRUE)
         if (mgga) then
            call add_product_of_grid_fns_(self,Vs,Gx,pot%v_tau(:,1))
            call add_product_of_grid_fns_(self,Vs,Gy,pot%v_tau(:,1))
            call add_product_of_grid_fns_(self,Vs,Gz,pot%v_tau(:,1))
         end if
         Va(bf,bf) = Va(bf,bf)+Vs+transpose(Vs)
         if (gga) then
            do j = 1,ns
               Z(:,j) = pot%v_rho(:,2)*G0(:,j)+pot%v_grad(:,1,2)*Gx(:,j)+pot%v_grad(:,2,2)*Gy(:,j)+pot%v_grad(:,3,2)*Gz(:,j)
            end do
         else
            do j = 1,ns
               Z(:,j) = pot%v_rho(:,2)*G0(:,j)
            end do
         end if
         call to_product_of_BLAS_(Vs,G0,Z,transpose_a=TRUE)
         if (mgga) then
            call add_product_of_grid_fns_(self,Vs,Gx,pot%v_tau(:,2))
            call add_product_of_grid_fns_(self,Vs,Gy,pot%v_tau(:,2))
            call add_product_of_grid_fns_(self,Vs,Gz,pot%v_tau(:,2))
         end if
         Vb(bf,bf) = Vb(bf,bf)+Vs+transpose(Vs)
         call destroy_(Vs)
         call destroy_(Z)
         if (gga) then
            call destroy_(Gz)
            call destroy_(Gy)
            call destroy_(Gx)
         end if
         call destroy_ptr_part_(pot)
         call destroy_(G0)
         call destroy_(bf)
      end do

      ! Add the XC contribution to K
      Ka = Ka+Va
      Kb = Kb+Vb
      call symmetric_reflect_(Ka)
      call symmetric_reflect_(Kb)

      ! Get DFT energy correction
      E_xc = Ea+Eb  ! the XC energy itself, before the energy corrections
      Ea = Ea-HALF*trace_product_with_(Va,self%density_mx%a)
      Eb = Eb-HALF*trace_product_with_(Vb,self%density_mx%b)
      E = Ea+Eb
      call destroy_(Vb)
      call destroy_(Va)
   end subroutine


! Set functional

   PURE subroutine add_density_of_grid_fns(self,res,G,D)
   ! Add to "res", at each grid point, the sum over functions a and b of
   ! G(point,a) D(a,b) G(point,b), where "G" holds the values of the
   ! functions on the grid points and "D" is a density matrix block.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), INOUT :: res
      MAT(REAL,:,:), IN :: G,D
      MAT(REAL,:,:), allocatable :: X
      INT :: j
      call create_(X,size(G,1),size(G,2))
      call to_product_of_BLAS_(X,G,D)
      do j = 1,size(G,2)
         res = res+G(:,j)*X(:,j)
      end do
      call destroy_(X)
   end subroutine

   PURE subroutine add_product_of_grid_fns(self,V,G,w)
   ! Add to "V" the matrix with elements, for functions a and b, the sum
   ! over grid points of G(point,a) w(point) G(point,b), where "G" holds
   ! the values of the functions on the grid points.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: V
      MAT(REAL,:,:), IN :: G
      VEC(REAL,:), IN :: w
      MAT(REAL,:,:), allocatable :: Z,P
      INT :: j
      call create_(Z,size(G,1),size(G,2))
      do j = 1,size(G,2)
         Z(:,j) = w*G(:,j)
      end do
      call create_(P,size(G,2),size(G,2))
      call to_product_of_BLAS_(P,G,Z,transpose_a=TRUE)
      V = V+P
      call destroy_(P)
      call destroy_(Z)
   end subroutine

   PURE subroutine set_XC_energy_and_potential(self,pot,den)
   ! Set "pot", the exchange correlation energy per particle and the
   ! potentials needed for the matrix elements of the exchange correlation
   ! potential, on the grid points whose densities are in "den". "pot" is
   ! set up here, for one or for two spin channels as "den" has.
      type(MOLECULE_TYPE), INOUT :: self
      type(XC_POTENTIAL_TYPE), INOUT :: pot
      type(XC_DENSITY_TYPE), IN :: den
      STR(len=STR_SIZE) :: exch,corr
      type(DFT_FUNCTIONAL_TYPE) :: add
   ENSURE(allocated(self%SCF_data),"MOLECULE.FOCK:set_XC_energy_and_potential ... no SCF_data")
   ENSURE(allocated(self%becke_grid),"MOLECULE.FOCK:set_XC_energy_and_potential ... no becke_grid")

      ! Set to zero
      call create_grids_(pot,den)

      ! Return?
      if (NOT is_DFT_calculation_(self%SCF_data)) then
         return
      end if
      call set_defaults_(add)
      call set_rho_cutoff_(add,self%becke_grid%rho_cutoff)
      exch = self%SCF_data%DFT_exchange_functional
      corr = self%SCF_data%DFT_correlation_functional
      if (exch/="none") call add_energy_and_potential_(add,exch,pot,den)
      if (corr/="none") call add_energy_and_potential_(add,corr,pot,den)
   end subroutine


!  ================
!  Unrestricted DFT
!  ================

! LDA

   subroutine add_LDA_XC_mx_2(self,Ka,Kb,E,Ea,Eb,E_xc)
   ! Add the exchange and correlation matrices to "Ka", "Kb". Return
   ! the DFT energy corrections "Ea", "Eb" and the total "E". This
   ! routine supports local functional only, either exchange or
   ! correlation.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: Ka,Kb
      REAL, OUT :: E,Ea,Eb
      REAL, OUT :: E_xc
      VEC(REAL,:), allocatable :: wt
      type(XC_DENSITY_TYPE) :: den
      type(XC_POTENTIAL_TYPE) :: pot
      MAT(REAL,:,:), allocatable :: Va,Vb,pt
      VEC(type(EVEC_BIN_TYPE),:), allocatable :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: bf_grid0
      INT :: c
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:add_LDA_XC_mx_2 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:add_LDA_XC_mx_2 ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.FOCK:add_LDA_XC_mx_2 ... no density matrix")
   ENSURE(allocated(self%density_mx%a),"MOLECULE.FOCK:add_LDA_XC_mx_2 ... no alpha density matrix")
   ENSURE(allocated(self%density_mx%b),"MOLECULE.FOCK:add_LDA_XC_mx_2 ... no alpha density matrix")

      ! Main temporaries
      Ea = ZERO; Eb = ZERO
      call create_(Va,self%n_bf,self%n_bf); Va = ZERO
      call create_(Vb,self%n_bf,self%n_bf); Vb = ZERO

      ! Loop over integration atoms "c"
      do c = 1,self%n_atom

         ! Make electron density on the Becke-grid for atom "c"
         call make_rho_becke_atom_grid_(self,den,pt,wt,bf_skip,bf_grid0,c,self%density_mx%a,self%density_mx%b)

         ! Make the XC energy per particle and potentials on the grid
         call destroy_(pt)
         call set_XC_energy_and_potential_(self,pot,den)

         ! Incorporate grid & partition weights
         call multiply_by_(pot,wt)

         ! Clean
         call destroy_ptr_part_(den)
         call destroy_(wt)

         ! Add the XC matrix elements into V, energy to E
         ! for atom "c" Becke-grid
         call add_LDA_XC_mx_(self,Va,Vb,Ea,Eb,pot,bf_skip,bf_grid0,c,self%density_mx%a,self%density_mx%b)

         ! Clean up
         call destroy_ptr_part_(pot)
         call destroy_(bf_grid0)
         call destroy_(bf_skip)
      end do  ! -- integration atom c

      ! Add the XC contribution to K
      Ka = Ka+Va
      Kb = Kb+Vb
      call symmetric_reflect_(Ka)
      call symmetric_reflect_(Kb)

      ! Get DFT energy correction
      call symmetric_reflect_(Va)
      call symmetric_reflect_(Vb)
      E_xc = Ea+Eb  ! the XC energy itself, before the energy corrections
      Ea = Ea-HALF*trace_product_with_(Va,self%density_mx%a)
      Eb = Eb-HALF*trace_product_with_(Vb,self%density_mx%b)
      E = Ea+Eb

      ! Clean
      call destroy_(Vb)
      call destroy_(Va)
   end subroutine

   subroutine add_LDA_XC_mx_3(self,Va,Vb,Ea,Eb,pot,bf_skip,bf_grid0,c,Da,Db)
   ! Add XC potential matrix elements to "Va", "Vb", the XC energy
   ! corrections to "Ea", "Eb". "pot" holds the alpha and beta XC
   ! potentials and the XC energy per particle on the becke grid for
   ! atom "c", all weighted by the partition function for that atom.
   ! "bf_skip"
   ! and "bf_grid0 are basis function grids.  "Da", "Db" are the
   ! density matrices.  This routine supports local functional only.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), target, INOUT :: Va,Vb
      REAL, INOUT :: Ea,Eb
      type(XC_POTENTIAL_TYPE), IN :: pot
      VEC(type(EVEC_BIN_TYPE),:), IN :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), IN :: bf_grid0
      INT, IN :: c
      MAT(REAL,:,:), target, IN :: Da,Db
      VEC(REAL,:), allocatable :: ga0,gb0
      VEC(INT,:), allocatable :: overlapping_atom,pi,pj,pn
      VEC(BIN,:), allocatable :: skipab,skipa,skipb
      MAT(REAL,:,:), PTR :: VVa,VVb
      MAT(REAL,:,:), PTR :: DDa,DDb
      MAT(REAL,:,:), allocatable :: grida0,gridb0
      REAL :: Da_ab,Db_ab,fac,vala,valb,wal,ga,gb,gab
      INT :: n_pt,n_keep,ca,cb,oa,fsa,lsa,ob,fsb,lsb
      INT :: sa,fa,la,na,sb,fb,lb,nb,a,b,p,i,j,n
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:add_LDA_XC_mx_3 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:add_LDA_XC_mx_3 ... no atom list")
   ENSURE(allocated(self%becke_grid),"MOLECULE.FOCK:add_LDA_XC_mx_3 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.FOCK:add_LDA_XC_mx_3 ... no finalized becke_grid")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.FOCK:add_LDA_XC_mx_3 ... no overlapping_atoms_for_atom")

      ! Skip array for ab shell
      n_pt = size(pot%energy)
      call create_(skipab,n_pt)

      ! Overlapping atoms
      overlapping_atom = self%overlapping_atoms_for_atom(c)%element

      ! Loop c-overlapping atoms "oa"
      do ca = 1,size(overlapping_atom)
         oa = overlapping_atom(ca)
         fsa = self%first_shell_for_atom(oa)
         lsa = self%last_shell_for_atom(oa)

         ! Loop shells on atom a
         do sa = fsa,lsa
            if (NOT allocated(bf_skip(sa)%element)) cycle
            fa = self%first_basis_fn_for_shell(sa)
            la = self%last_basis_fn_for_shell(sa)
            na = la-fa+1

            ! Set the "sb" basis function grid
            skipa = bf_skip(sa)%element
            grida0 = bf_grid0(sa)%element

            ! Loop c-overlapping atoms b
            do cb = 1,ca
               ob = overlapping_atom(cb)
               fsb = self%first_shell_for_atom(ob)
               lsb = self%last_shell_for_atom(ob)

               ! Loop shells on atom b
               do sb = fsb,min(lsb,sa)
                  if (NOT allocated(bf_skip(sb)%element)) cycle
                  fb = self%first_basis_fn_for_shell(sb)
                  lb = self%last_basis_fn_for_shell(sb)
                  nb = lb-fb+1

                  ! Set the "sb" basis function grid
                  skipb = bf_skip(sb)%element
                  gridb0 = bf_grid0(sb)%element

                  ! How many points in common?
                  skipab(1:n_pt) = skipa OR skipb
                  n_keep = count(NOT skipab(1:n_pt))
                  if (n_keep==0) cycle

                  ! Factor
                  fac = TWO
                  if (sa==sb) fac = ONE

                  ! Density block
                  VVa => Va(fa:la,fb:lb)
                  VVb => Vb(fa:la,fb:lb)
                  DDa => Da(fa:la,fb:lb)
                  DDb => Db(fa:la,fb:lb)
                  if (na*nb==1) then
                     ga0 = grida0(:,1)
                     gb0 = gridb0(:,1)
                     Da_ab = fac*DDa(1,1)
                     Db_ab = fac*DDb(1,1)
                     i = 0
                     j = 0
                     vala = ZERO
                     valb = ZERO
                     wal = ZERO
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        ga = ga0(i)
                        gb = gb0(j)
                        gab = ga*gb
                        vala = vala+pot%v_rho(n,1)*gab
                        valb = valb+pot%v_rho(n,2)*gab
                        wal = wal+pot%energy(n)*gab
                     end do
                     VVa(1,1) = VVa(1,1)+vala
                     VVb(1,1) = VVb(1,1)+valb
                     Ea = Ea+wal*Da_ab
                     Eb = Eb+wal*Db_ab
                  else

                     ! Create space for gathered index info
                     call create_(pi,n_keep)
                     call create_(pj,n_keep)
                     call create_(pn,n_keep)

                     ! Make mapper arrays
                     p = 0
                     i = 0
                     j = 0
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        p = p+1
                        pi(p) = i
                        pj(p) = j
                        pn(p) = n
                     end do

                     ! Add density contribution to rho
                     do a = 1,na
                        ga0 = grida0(:,a)
                        do b = 1,nb
                           gb0 = gridb0(:,b)
                           Da_ab = fac*DDa(a,b)
                           Db_ab = fac*DDb(a,b)
                           vala = ZERO
                           valb = ZERO
                           wal = ZERO
                           do p = 1,n_keep
                              i = pi(p)
                              j = pj(p)
                              n = pn(p)
                              ga = ga0(i)
                              gb = gb0(j)
                              gab = ga*gb
                              vala = vala+pot%v_rho(n,1)*gab
                              valb = valb+pot%v_rho(n,2)*gab
                              wal = wal+pot%energy(n)*gab
                           end do
                           VVa(a,b) = VVa(a,b)+vala
                           VVb(a,b) = VVb(a,b)+valb
                           Ea = Ea+wal*Da_ab
                           Eb = Eb+wal*Db_ab
                        end do
                     end do

                     ! Clean up
                     call destroy_(pn)
                     call destroy_(pj)
                     call destroy_(pi)
                  end if
               end do  ! -- loops sa,sb
            end do
         end do  ! -- lops ca,cb
      end do

      ! Clean up
      call destroy_(skipab)
   end subroutine


! GGA

   subroutine add_GGA_XC_mx_2(self,Ka,Kb,E,Ea,Eb,E_xc)
   ! Add the exchange and correlation matrices to "Ka", "Kb". Return
   ! the DFT energy coorections "Ea", "Eb" and total "E". This routine
   ! supports non-local functionals, either exchange or correlation.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: Ka,Kb
      REAL, OUT :: E,Ea,Eb
      REAL, OUT :: E_xc
      VEC(REAL,:), allocatable :: wt
      type(XC_DENSITY_TYPE) :: den
      type(XC_POTENTIAL_TYPE) :: pot
      MAT(REAL,:,:), allocatable :: Va,Vb,pt
      VEC(type(EVEC_BIN_TYPE),:), allocatable :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: bf_grid0,bf_gridx,bf_gridy,bf_gridz
      INT :: c
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:add_GGA_XC_mx_2 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:add_GGA_XC_mx_2 ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.FOCK:add_GGA_XC_mx_2 ... no density matrix")
   ENSURE(allocated(self%density_mx%a),"MOLECULE.FOCK:add_GGA_XC_mx_2 ... no alpha density matrix")
   ENSURE(allocated(self%density_mx%b),"MOLECULE.FOCK:add_GGA_XC_mx_2 ... no alpha density matrix")

      ! Main temporaries
      Ea = ZERO; Eb = ZERO
      call create_(Va,self%n_bf,self%n_bf); Va = ZERO
      call create_(Vb,self%n_bf,self%n_bf); Vb = ZERO

      ! Loop over integration atoms "c"
      do c = 1,self%n_atom

         ! Make electron density on the Becke-grid for atom "c"
         call make_rho_becke_atom_grid_(self,den,pt,wt,bf_skip,bf_grid0,bf_gridx,bf_gridy,bf_gridz,c,self%density_mx%a,self%density_mx%b)

         ! Make the XC energy per particle and potentials on the grid
         call destroy_(pt)
         call set_XC_energy_and_potential_(self,pot,den)

         ! Incorporate grid & partition weights
         call multiply_by_(pot,wt)

         ! Clean
         call destroy_ptr_part_(den)
         call destroy_(wt)

         ! Add the XC matrix elements into V, energy to E
         ! for atom "c" Becke-grid
         call add_GGA_XC_mx_(self,Va,Vb,Ea,Eb,pot,bf_skip,bf_grid0,bf_gridx,bf_gridy,bf_gridz,c,self%density_mx%a,self%density_mx%b)

         ! Clean up
         call destroy_ptr_part_(pot)
         call destroy_(bf_gridz)
         call destroy_(bf_gridy)
         call destroy_(bf_gridx)
         call destroy_(bf_grid0)
         call destroy_(bf_skip)
      end do  ! -- integration atom c

      ! Add the XC contribution to K
      Ka = Ka+Va
      Kb = Kb+Vb
      call symmetric_reflect_(Ka)
      call symmetric_reflect_(Kb)

      ! Get DFT energy correction
      call symmetric_reflect_(Va)
      call symmetric_reflect_(Vb)
      E_xc = Ea+Eb  ! the XC energy itself, before the energy corrections
      Ea = Ea-HALF*trace_product_with_(Va,self%density_mx%a)
      Eb = Eb-HALF*trace_product_with_(Vb,self%density_mx%b)
      E = Ea+Eb

      ! Clean
      call destroy_(Vb)
      call destroy_(Va)
   end subroutine

   subroutine add_GGA_XC_mx_3(self,Va,Vb,Ea,Eb,pot,bf_skip,bf_gr0,bf_grx,bf_gry,bf_grz,c,Da,Db)
   ! Add XC potential matrix elements to "Va", "Vb", the XC energy
   ! corrections to "Ea", "Eb". "pot" holds the alpha and beta XC
   ! potentials, their gradient parts, and the XC energy per particle
   ! on the becke grid for atom "c", all weighted by the partition
   ! function for that atom.  "bf_skip"
   ! and "bf_gr0" ... "bf_grz" are basis function (derivative)
   ! grids.  "Da", "Db" are the density matrices.  This routine
   ! supports non-local functionals.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), target, INOUT :: Va,Vb
      REAL, INOUT :: Ea,Eb
      type(XC_POTENTIAL_TYPE), IN :: pot
      VEC(type(EVEC_BIN_TYPE),:), IN :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), IN :: bf_gr0,bf_grx,bf_gry,bf_grz
      INT, IN :: c
      MAT(REAL,:,:), target, IN :: Da,Db
      VEC(REAL,:), allocatable :: ga0,gax,gay,gaz
      VEC(REAL,:), allocatable :: gb0,gbx,gby,gbz
      VEC(INT,:), allocatable :: overlapping_atom,pi,pj,pn
      VEC(BIN,:), allocatable :: skipab,skipa,skipb
      MAT(REAL,:,:), allocatable :: grida0,gridax,griday,gridaz
      MAT(REAL,:,:), allocatable :: gridb0,gridbx,gridby,gridbz
      MAT(REAL,:,:), PTR :: VVa,VVb
      MAT(REAL,:,:), PTR :: DDa,DDb
      REAL :: Da_ab,Db_ab,fac,vala,valb,wal,ga,gb,gab,gx,gy,gz
      INT :: n_pt,n_keep,ca,cb,oa,fsa,lsa,ob,fsb,lsb
      INT :: sa,fa,la,na,sb,fb,lb,nb,a,b,p,i,j,n
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:add_GGA_XC_mx_3 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:add_GGA_XC_mx_3 ... no atom list")
   ENSURE(allocated(self%becke_grid),"MOLECULE.FOCK:add_GGA_XC_mx_3 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.FOCK:add_GGA_XC_mx_3 ... no finalized becke_grid")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.FOCK:add_GGA_XC_mx_3 ... no overlapping_atoms_for_atom")

      ! Skip array for ab shell
      n_pt = size(pot%energy)
      call create_(skipab,n_pt)

      ! Overlapping atoms
      overlapping_atom = self%overlapping_atoms_for_atom(c)%element

      ! Loop c-overlapping atoms "oa"
      do ca = 1,size(overlapping_atom)
         oa = overlapping_atom(ca)
         fsa = self%first_shell_for_atom(oa)
         lsa = self%last_shell_for_atom(oa)

         ! Loop shells on atom a
         do sa = fsa,lsa
            if (NOT allocated(bf_skip(sa)%element)) cycle
            fa = self%first_basis_fn_for_shell(sa)
            la = self%last_basis_fn_for_shell(sa)
            na = la-fa+1

            ! Set the "sb" basis function grid
            skipa = bf_skip(sa)%element
            grida0 = bf_gr0(sa)%element
            gridax = bf_grx(sa)%element
            griday = bf_gry(sa)%element
            gridaz = bf_grz(sa)%element

            ! Loop c-overlapping atoms b
            do cb = 1,ca
               ob = overlapping_atom(cb)
               fsb = self%first_shell_for_atom(ob)
               lsb = self%last_shell_for_atom(ob)

               ! Loop shells on atom b
               do sb = fsb,min(lsb,sa)
                  if (NOT allocated(bf_skip(sb)%element)) cycle
                  fb = self%first_basis_fn_for_shell(sb)
                  lb = self%last_basis_fn_for_shell(sb)
                  nb = lb-fb+1

                  ! Set the "sb" basis function grid
                  skipb = bf_skip(sb)%element
                  gridb0 = bf_gr0(sb)%element
                  gridbx = bf_grx(sb)%element
                  gridby = bf_gry(sb)%element
                  gridbz = bf_grz(sb)%element

                  ! How many points in common?
                  skipab(1:n_pt) = skipa OR skipb
                  n_keep = count(NOT skipab(1:n_pt))
                  if (n_keep==0) cycle

                  ! Factor
                  fac = TWO
                  if (sa==sb) fac = ONE

                  ! Density block
                  VVa => Va(fa:la,fb:lb)
                  VVb => Vb(fa:la,fb:lb)
                  DDa => Da(fa:la,fb:lb)
                  DDb => Db(fa:la,fb:lb)
                  if (na*nb==1) then
                     ga0 = grida0(:,1)
                     gax = gridax(:,1)
                     gay = griday(:,1)
                     gaz = gridaz(:,1)
                     gb0 = gridb0(:,1)
                     gbx = gridbx(:,1)
                     gby = gridby(:,1)
                     gbz = gridbz(:,1)
                     Da_ab = fac*DDa(1,1)
                     Db_ab = fac*DDb(1,1)
                     i = 0
                     j = 0
                     vala = ZERO
                     valb = ZERO
                     wal = ZERO
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        ga = ga0(i)
                        gb = gb0(j)
                        gab = ga*gb
                        gx = gax(i)*gb+ga*gbx(j)
                        gy = gay(i)*gb+ga*gby(j)
                        gz = gaz(i)*gb+ga*gbz(j)
                        vala = vala+pot%v_rho(n,1)*gab+pot%v_grad(n,1,1)*gx+pot%v_grad(n,2,1)*gy+pot%v_grad(n,3,1)*gz
                        valb = valb+pot%v_rho(n,2)*gab+pot%v_grad(n,1,2)*gx+pot%v_grad(n,2,2)*gy+pot%v_grad(n,3,2)*gz
                        wal = wal+pot%energy(n)*gab
                     end do
                     VVa(1,1) = VVa(1,1)+vala
                     VVb(1,1) = VVb(1,1)+valb
                     Ea = Ea+wal*Da_ab
                     Eb = Eb+wal*Db_ab
                  else

                     ! Create space for gathered index info
                     call create_(pi,n_keep)
                     call create_(pj,n_keep)
                     call create_(pn,n_keep)

                     ! Make mapper arrays
                     p = 0
                     i = 0
                     j = 0
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        p = p+1
                        pi(p) = i
                        pj(p) = j
                        pn(p) = n
                     end do

                     ! Add density contribution to rho
                     do a = 1,na
                        ga0 = grida0(:,a)
                        gax = gridax(:,a)
                        gay = griday(:,a)
                        gaz = gridaz(:,a)
                        do b = 1,nb
                           gb0 = gridb0(:,b)
                           gbx = gridbx(:,b)
                           gby = gridby(:,b)
                           gbz = gridbz(:,b)
                           Da_ab = fac*DDa(a,b)
                           Db_ab = fac*DDb(a,b)
                           vala = ZERO
                           valb = ZERO
                           wal = ZERO
                           do p = 1,n_keep
                              i = pi(p)
                              j = pj(p)
                              n = pn(p)
                              ga = ga0(i)
                              gb = gb0(j)
                              gab = ga*gb
                              gx = gax(i)*gb+ga*gbx(j)
                              gy = gay(i)*gb+ga*gby(j)
                              gz = gaz(i)*gb+ga*gbz(j)
                              vala = vala+pot%v_rho(n,1)*gab+pot%v_grad(n,1,1)*gx+pot%v_grad(n,2,1)*gy+pot%v_grad(n,3,1)*gz
                              valb = valb+pot%v_rho(n,2)*gab+pot%v_grad(n,1,2)*gx+pot%v_grad(n,2,2)*gy+pot%v_grad(n,3,2)*gz
                              wal = wal+pot%energy(n)*gab
                           end do
                           VVa(a,b) = VVa(a,b)+vala
                           VVb(a,b) = VVb(a,b)+valb
                           Ea = Ea+wal*Da_ab
                           Eb = Eb+wal*Db_ab
                        end do
                     end do

                     ! Clean up
                     call destroy_(pn)
                     call destroy_(pj)
                     call destroy_(pi)
                  end if
               end do  ! -- loops sa,sb
            end do
         end do  ! -- lops ca,cb
      end do

      ! Clean up
      call destroy_(skipab)
   end subroutine


! Set functional

! Numerical J matrix -- calculated two ways

!   make_numerical_J_mx_v1(J)
!   ! Evaluate the coulomb matrix "J" using numerical evaluation of the potential
!   ! on a DFT integration grid.
!     J :: MAT{REAL}
!   ENSURE(.basis_info_made, "no basis info")
!   ENSURE(.atom.allocated,  "no atom list")
!   ENSURE(.dftgrid.allocated, "need to specify dftgrid")
!     pt :: MAT{REAL}*
!     wt,p :: VEC{REAL}*
!     n_pt :: INT
!     n_pt = .dftgrid.n_pts*.n_atom
!     p.create(n_pt)
!     pt.create(n_pt,3)
!     wt.create(n_pt)
!     .dftgrid.make_grid(pt,wt,.atom)
!     .make_density_grid(p,pt)
!     p = wt*p ! Now we have the effective charges
!     wt.destroy
!     .make_nuclear_attraction_mx(J,charges=p,points=pt)
!     pt.destroy
!     p.destroy
!   end

!   make_numerical_J_mx_v2(J)
!   ! Evaluate the coulomb matrix "J" using numerical evaluation of the potential
!   ! on a DFT integration grid.
!     J :: MAT{REAL}
!   ENSURE(.basis_info_made, "no basis info")
!   ENSURE(.atom.allocated,  "no atom list")
!   ENSURE(.dftgrid.allocated, "need to specify dftgrid")
!     JJ,pt :: MAT{REAL}*
!     wt,q :: VEC{REAL}*
!     n_pt, p,fa,la,fb,lb,atom_a,atom_b :: INT
!     sh :: SHELL2
!     atom :: VEC{ATOM}*
!     n_pt = .dftgrid.n_pts*.n_atom
!     q.create(n_pt)
!     pt.create(n_pt,3)
!     wt.create(n_pt)
!     .dftgrid.make_grid(pt,wt,.atom)
!     .make_density_grid(q,pt)
!     q = wt*q ! Now we have the effective charges
!     wt.destroy
!     J = ZERO
!     do p = 1,.n_shell_pairs
!        .set_shell2_from(p,sh,fa,la,fb,lb,atom_a,atom_b)
!        if (atom_a==atom_b) then
!           atom.create(1)
!           atom(1) = .atom(atom_a)
!        else
!           atom.create(2)
!           atom(1) = .atom(atom_a)
!           atom(2) = .atom(atom_b)
!        end
!        JJ.create(sh.a.n_comp,sh.b.n_comp)
!        .dftgrid.make_potential_MEs_of(MOLECULE::electric_potential,q,pt,sh,atom,JJ)
!        atom.nullify_ptr_part; atom.destroy
!        J(fa:la,fb:lb) = J(fa:la,fb:lb) - JJ
!        JJ.destroy
!        sh.destroy_ptr_part
!     end
!     J.symmetric_reflect
!     pt.destroy
!     q.destroy
!   end

!  ======================
!  Two electron integrals
!  ======================

   subroutine get_ERI_integrals(self)
   ! Get the electron repulsion integrals on disk. If the integral
   ! file is already there, do nothing.
      type(MOLECULE_TYPE), IN :: self
      type(ARCHIVE_TYPE) :: eri_archive
      type(ARCHIVE_TYPE) :: ind_archive
      call set_(eri_archive,self%name,"eri_integrals")
      call set_(ind_archive,self%name,"eri_index")
      if ((NOT exists_(eri_archive)) OR (NOT exists_(ind_archive))) then
         call open_for_(eri_archive,"write-only")
         call open_for_(ind_archive,"write-only")
         call make_ERI_integrals_(self,eri_archive,ind_archive)
         call close_(ind_archive)
         call close_(eri_archive)
      end if
   end subroutine

   subroutine make_ERI_integrals_0(self,eri_archive,eri_index)
   ! Calculate the electron repulsion integrals (ERI's) over all basis
   ! functions Outputs to archive "eri_archive".
   ! NOTE: that index coincidence factors are included.
   ! NOTE: Replace SHELL4 in here with SHELL1QUARTET
      type(MOLECULE_TYPE), IN :: self
      type(ARCHIVE_TYPE), INOUT :: eri_archive
      type(ARCHIVE_TYPE), INOUT :: eri_index
      type(SHELL4_TYPE) :: sh4
      MAT4(REAL,:,:,:,:), allocatable :: ERI
      INT :: q,a,b,c,d,skipped
      INT :: atom_a,atom_b,atom_c,atom_d
      INT :: n_shell_quartets
      REAL :: factor
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.FOCK:make_ERI_integrals_0 ... not implemented")
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_ERI_integrals_0 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_ERI_integrals_0 ... no atom list")
      skipped = 0
      if (self%SCF_data%output) then
         call set_real_fortran_fmt_(stdout,"e")
         call show_(stdout,"Skipping electron repulsion integrals less than =",self%SCF_data%ERI_disk_cutoff)
         call set_real_fortran_fmt_(stdout,"f")
      end if
      n_shell_quartets = no_of_shell_quartets_(self%atom)
      if (self%SCF_data%using_NDDO) then
         do q = 1,n_shell_quartets
            call set_shell4_indices_from_(self,q,a,b,c,d,atom_a,atom_b,atom_c,atom_d)
            if (atom_a==atom_b AND atom_c==atom_d) then
               call set_shell4_from_(self,q,sh4,a,b,c,d)
            else
               skipped = skipped+1
               cycle
            end if
            if (skip_ERI_(sh4)) then
               skipped = skipped+1
               call destroy_ptr_part_(sh4)
               cycle
            end if
            call create_(ERI,sh4%a%n_comp,sh4%b%n_comp,sh4%c%n_comp,sh4%d%n_comp)
            call get_ERI_(sh4,ERI)

            ! Coincidence factors
            factor = ONE
            if (a==b) factor = HALF
            if (c==d) factor = HALF*factor
            if (a==c AND b==d) factor = HALF*factor
            ERI = factor*ERI
            call write_(eri_archive%Bfile,ERI)
            call write_(eri_index%Bfile,q)
            call destroy_(ERI)
            call destroy_ptr_part_(sh4)
         end do
      else if (self%SCF_data%using_NUDO) then
         do q = 1,n_shell_quartets
            call set_shell4_indices_from_(self,q,a,b,c,d,atom_a,atom_b,atom_c,atom_d)
            if (bonded_(self%atom,atom_a,atom_b) AND bonded_(self%atom,atom_c,atom_d)) then
               call set_shell4_from_(self,q,sh4,a,b,c,d)
            else
               skipped = skipped+1
               cycle
            end if
            if (skip_ERI_(sh4)) then
               skipped = skipped+1
               call destroy_ptr_part_(sh4)
               cycle
            end if
            call create_(ERI,sh4%a%n_comp,sh4%b%n_comp,sh4%c%n_comp,sh4%d%n_comp)
            call get_ERI_(sh4,ERI)

            ! Coincidence factors
            factor = ONE
            if (a==b) factor = HALF
            if (c==d) factor = HALF*factor
            if (a==c AND b==d) factor = HALF*factor
            ERI = factor*ERI
            call write_(eri_archive%Bfile,ERI)
            call write_(eri_index%Bfile,q)
            call destroy_(ERI)
            call destroy_ptr_part_(sh4)
         end do
      else
         do q = 1,n_shell_quartets
            call set_shell4_from_(self,q,sh4,a,b,c,d)
            if (skip_ERI_(sh4,self%SCF_data%ERI_disk_cutoff)) then
               skipped = skipped+1
               call destroy_ptr_part_(sh4)
               cycle
            end if
            call create_(ERI,sh4%a%n_comp,sh4%b%n_comp,sh4%c%n_comp,sh4%d%n_comp)
            call get_ERI_(sh4,ERI)

            ! Coincidence factors
            factor = ONE
            if (a==b) factor = HALF
            if (c==d) factor = HALF*factor
            if (a==c AND b==d) factor = HALF*factor
            ERI = factor*ERI
            call write_(eri_archive%Bfile,ERI)
            call write_(eri_index%Bfile,q)
            call destroy_(ERI)
            call destroy_ptr_part_(sh4)
         end do
      end if
      if (self%SCF_data%output) then
         a = n_shell_quartets
         call text_(stdout,"Skipped "//trim(to_str_(skipped))//" out of "//trim(to_str_(a))//" blocks.")
      end if
      call write_(eri_index%Bfile,n_shell_quartets+1)
   end subroutine

   subroutine get_spin_orbit_integrals(self)
   ! Get the spin orbit integrals on disk. If the integral files are
   ! file is already there, do nothing.
      type(MOLECULE_TYPE), IN :: self
      type(ARCHIVE_TYPE) :: SOx_archive,SOy_archive,SOz_archive
      type(ARCHIVE_TYPE) :: ind_archive
      call set_(SOx_archive,self%name,"SOx_integrals")
      call set_(SOy_archive,self%name,"SOy_integrals")
      call set_(SOz_archive,self%name,"SOz_integrals")
      call set_(ind_archive,self%name,"SO_indices")
      if (NOT exists_(SOx_archive)) then
         call open_for_(SOx_archive,"write-only")
         call open_for_(SOy_archive,"write-only")
         call open_for_(SOz_archive,"write-only")
         call open_for_(ind_archive,"write-only")
         call make_spin_orbit_integrals_(self,SOx_archive,SOy_archive,SOz_archive,ind_archive)
         call close_(SOx_archive)
         call close_(SOy_archive)
         call close_(SOz_archive)
         call close_(ind_archive)
      end if
   end subroutine

   subroutine make_spin_orbit_integrals(self,SOx_archive,SOy_archive,SOz_archive,ind_archive)
   ! Calculate the spin orbit integrals. Outputs the spin same-orbit integrals
   ! to for each component i to "SOi_archive". The shell quartet index for
   ! these integrals are put in "ind_archive". Note that index coincidence
   ! factors are included.
      type(MOLECULE_TYPE), IN :: self
      type(ARCHIVE_TYPE), INOUT :: SOx_archive,SOy_archive,SOz_archive
      type(ARCHIVE_TYPE), INOUT :: ind_archive
      type(SHELL4_TYPE) :: sh4
      MAT4(REAL,:,:,:,:), allocatable :: Sx,Sy,Sz
      MAT4(REAL,:,:,:,:), allocatable :: Ox,Oy,Oz
      INT :: na,nb,nc,nd
      INT :: a,b,c,d,skipped
      INT :: atom_a,atom_b,atom_c,atom_d
      INT :: q,n_shell_quartets
      REAL :: factor
      STR(len=STR_SIZE) :: cutoff
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.FOCK:make_spin_orbit_integrals ... not implemented")
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_spin_orbit_integrals ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_spin_orbit_integrals ... no atom list")
      call from_real_(cutoff,SHELL4_ERI_CUTOFF)
      if (self%SCF_data%output) then
         call text_(stdout,"Skipping electron repulsion integrals less than "//trim(cutoff)//".")
      end if
      n_shell_quartets = no_of_shell_quartets_(self%atom)
      skipped = 0
      do q = 1,n_shell_quartets
         if (self%SCF_data%using_NDDO) then
            call set_shell4_indices_from_(self,q,a,b,c,d,atom_a,atom_b,atom_c,atom_d)
            if (atom_a==atom_b AND atom_c==atom_d) then
               call set_shell4_from_(self,q,sh4,a,b,c,d)
            else
               skipped = skipped+1
               cycle
            end if
         else if (self%SCF_data%using_NUDO) then
            call set_shell4_indices_from_(self,q,a,b,c,d,atom_a,atom_b,atom_c,atom_d)
            if (bonded_(self%atom,atom_a,atom_b) AND bonded_(self%atom,atom_c,atom_d)) then
               call set_shell4_from_(self,q,sh4,a,b,c,d)
            else
               skipped = skipped+1
               cycle
            end if
         else
            call set_shell4_from_(self,q,sh4,a,b,c,d)
         end if
         if (skip_ERI_(sh4)) then
            skipped = skipped+1
            call destroy_ptr_part_(sh4)
            cycle
         end if
         na = sh4%a%n_comp; nb = sh4%b%n_comp
         nc = sh4%c%n_comp; nd = sh4%d%n_comp
         call create_(Sx,na,nb,nc,nd)
         call create_(Sy,na,nb,nc,nd)
         call create_(Sz,na,nb,nc,nd)
         call create_(Ox,na,nb,nc,nd)
         call create_(Oy,na,nb,nc,nd)
         call create_(Oz,na,nb,nc,nd)
         call make_spin_orbit_ints_(sh4,Sx,Sy,Sz,Ox,Oy,Oz)

         ! Coincidence factors
         factor = ONE
         if (a==b) factor = HALF
         if (c==d) factor = HALF*factor
         if (a==c AND b==d) factor = HALF*factor
         Sx = factor*Sx; Sy = factor*Sy; Sz = factor*Sz
         Ox = factor*Ox; Oy = factor*Oy; Oz = factor*Oz
         call write_(SOx_archive%Bfile,Sx); call write_(SOx_archive%Bfile,Ox)
         call write_(SOy_archive%Bfile,Sy); call write_(SOy_archive%Bfile,Oy)
         call write_(SOz_archive%Bfile,Sz); call write_(SOz_archive%Bfile,Oz)
         call write_(ind_archive%Bfile,q)
         call destroy_(Oz)
         call destroy_(Oy)
         call destroy_(Ox)
         call destroy_(Sz)
         call destroy_(Sy)
         call destroy_(Sx)
         call destroy_ptr_part_(sh4)
      end do
      if (self%SCF_data%output) then
         a = n_shell_quartets
         call text_(stdout,"Skipped "//trim(to_str_(skipped))//" out of "//trim(to_str_(a))//" blocks.")
      end if
      call write_(ind_archive%Bfile,n_shell_quartets+1)
   end subroutine


!  ================
!  For geminal code
!  ================

   PURE subroutine make_ERI_integrals_1(self,v)
   ! Calculate the electron repulsion integrals (ERI's) over all basis
   ! functions and in array "v". This uses up a lot of space!
      type(MOLECULE_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: v
      VEC(REAL,:), allocatable :: I
      type(SHELL1QUARTET_TYPE) :: sh4n
      type(ERI_SCRATCH_TYPE) :: work
      INT :: ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b
      INT :: cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d
      INT :: a,b,c,d,abcd
      REAL :: val
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_ERI_integrals_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_ERI_integrals_1 ... no atom list")
   ENSURE(all(shape(v)==[self%n_bf,self%n_bf,self%n_bf,self%n_bf]),"MOLECULE.FOCK:make_ERI_integrals_1 ... wrong shape for array v")

      ! Size the integral work arrays once, for the largest quartet
      call set_sizes_for_(work,self%basis_shell1pair)
      do ab = 1,self%n_shell_pairs
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb,atom_a,atom_b)
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         do cd = 1,ab
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd,atom_c,atom_d)
            call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
            call make_ERI_into(sh4n,work)

            ! Now symmetrise the integrals
            abcd = 0
            do d = fd,ld
               do c = fc,lc
                  do b = fb,lb
                     do a = fa,la
                        abcd = abcd+1
                        val = work%abcd(abcd)
                        v(a,b,c,d) = val
                        v(a,b,d,c) = val
                        v(b,a,c,d) = val
                        v(b,a,d,c) = val
                        v(c,d,a,b) = val
                        v(c,d,b,a) = val
                        v(d,c,a,b) = val
                        v(d,c,b,a) = val
                     end do
                  end do
               end do
            end do
         end do
      end do
   end subroutine

   subroutine make_MO_ERI_integrals_0(self,v,eri_archive,add_core,new_n_bf)
   ! Calculate the electron repulsion integrals (ERI's) in the MO
   ! basis from the AO integrals stored in "v" and write them to disk
   ! in "eri_archive". If "add_core" is present and true, the one
   ! electron core integrals are added into the two electron MO
   ! integrals. Optional arguments if one uses a reduced mo basis set.
      type(MOLECULE_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: v
      type(ARCHIVE_TYPE), INOUT :: eri_archive
      BIN, optional, IN :: add_core
      INT, optional, IN :: new_n_bf
      MAT4(REAL,:,:,:,:), allocatable :: new_v
      MAT(REAL,:,:), allocatable :: P,H,S
      INT :: a,b,c,d
      BIN :: core
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_MO_ERI_integrals_0 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_MO_ERI_integrals_0 ... no atom list")
   ENSURE(allocated(self%MOs),"MOLECULE.FOCK:make_MO_ERI_integrals_0 ... no molecular orbitals")
   ENSURE(allocated(self%MOs%r),"MOLECULE.FOCK:make_MO_ERI_integrals_0 ... no molecular orbitals")
   ENSURE(allocated(self%core_mx),"MOLECULE.FOCK:make_MO_ERI_integrals_0 ... no core matrix")
   ENSURE(allocated(self%core_mx%r),"MOLECULE.FOCK:make_MO_ERI_integrals_0 ... no restricted core matrix")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.FOCK:make_MO_ERI_integrals_0 ... no overlap matrix")
      if (present(new_n_bf)) then
         call create_(P,self%n_bf,new_n_bf)
         P = self%MOs%r(:,1:new_n_bf)
         call create_(new_v,new_n_bf,new_n_bf,new_n_bf,new_n_bf)
      else
         call create_(P,self%n_bf,self%n_bf)
         P = self%MOs%r
      end if
      core = FALSE
      if (present(add_core)) core = add_core
      call create_(H,self%n_bf,self%n_bf)
      call create_(S,self%n_bf,self%n_bf)
      if (core) then
         H = self%core_mx%r/(TWO*self%n_a-1)
         S = self%overlap_mx
      end if
      do a = 1,self%n_bf
         do b = 1,a-1

         !pcc should swap a and b to make it faster
            if (core) v(a,b,:,:) = v(a,b,:,:)+H*S(a,b)+S*H(a,b)
            call change_basis_using_(v(a,b,:,:),P)
            v(b,a,:,:) = v(a,b,:,:)
         end do
         if (core) v(a,a,:,:) = v(a,a,:,:)+H*S(a,a)+S*H(a,a)
         call change_basis_using_(v(a,a,:,:),P)
      end do
      do c = 1,self%n_bf
         do d = 1,c-1
            call change_basis_using_(v(:,:,c,d),P)
            v(:,:,d,c) = v(:,:,c,d)
         end do
         call change_basis_using_(v(:,:,c,c),P)
      end do
      call destroy_(P)
      if (present(new_n_bf)) then
         new_v = v(1:new_n_bf,1:new_n_bf,1:new_n_bf,1:new_n_bf)
         call shrink_(H,new_n_bf,new_n_bf)
         do c = 1,new_n_bf
            do d = 1,c-1
               H = new_v(:,d,c,:)
               new_v(:,d,c,:) = new_v(:,c,d,:)
               new_v(:,c,d,:) = H
            end do
         end do
         call write_(eri_archive,new_v)
         call destroy_(new_v)
      else
         do c = 1,self%n_bf
            do d = 1,c-1
               H = v(:,d,c,:)
               v(:,d,c,:) = v(:,c,d,:)
               v(:,c,d,:) = H
            end do
         end do
         call write_(eri_archive,v)
      end if
      call destroy_(S)
      call destroy_(H)
   end subroutine

   subroutine make_read_MO_ERI_integrals(self,v,eri_archive,add_core,new_n_bf)
   ! Calculate the electron repulsion integrals (ERI's) in read-in MO
   ! basis from the AO integrals stored in "v" and write them to disk
   ! in "eri_archive". If "add_core" is present and true, the one
   ! electron core integrals are added into the two electron MO
   ! integrals.  optional arguments if one uses a reduced mo basis set
      type(MOLECULE_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: v
      type(ARCHIVE_TYPE), INOUT :: eri_archive
      BIN, optional, IN :: add_core
      INT, optional, IN :: new_n_bf
      MAT4(REAL,:,:,:,:), allocatable :: new_v
      MAT(REAL,:,:), allocatable :: P,H,S
      INT :: a,b,c,d
      BIN :: core
      type(TEXTFILE_TYPE), allocatable :: in
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_read_MO_ERI_integrals ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_read_MO_ERI_integrals ... no atom list")
   ENSURE(allocated(self%core_mx),"MOLECULE.FOCK:make_read_MO_ERI_integrals ... no core matrix")
   ENSURE(allocated(self%core_mx%r),"MOLECULE.FOCK:make_read_MO_ERI_integrals ... no restricted core matrix")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.FOCK:make_read_MO_ERI_integrals ... no overlap matrix")
      if (present(new_n_bf)) then
         call create_(P,self%n_bf,new_n_bf)
         call create_(new_v,new_n_bf,new_n_bf,new_n_bf,new_n_bf)
      else
         call create_(P,self%n_bf,self%n_bf)
      end if
      call create_(in,"readin_mos")
      call open_for_(in,"read")
      call read_(in,P,by_column=TRUE)
      call close_(in)
      call destroy_(in)
      call text_(stdout,"readin mos")
      call put_(stdout,P)
      call text_(stdout," ")
      core = FALSE
      if (present(add_core)) core = add_core
      call create_(H,self%n_bf,self%n_bf)
      call create_(S,self%n_bf,self%n_bf)
      if (core) then
         ENSURE(allocated(self%core_mx%r),"MOLECULE.FOCK:make_read_MO_ERI_integrals ... no restricted core matrix")
         H = self%core_mx%r/(TWO*self%n_a-1)
         S = self%overlap_mx
      end if
      do a = 1,self%n_bf
         do b = 1,a-1
            if (core) v(a,b,:,:) = v(a,b,:,:)+H*S(a,b)+S*H(a,b)
            call change_basis_using_(v(a,b,:,:),P)
            v(b,a,:,:) = v(a,b,:,:)
         end do
         if (core) v(a,a,:,:) = v(a,a,:,:)+H*S(a,a)+S*H(a,a)
         call change_basis_using_(v(a,a,:,:),P)
      end do
      do c = 1,self%n_bf
         do d = 1,c-1
            call change_basis_using_(v(:,:,c,d),P)
            v(:,:,d,c) = v(:,:,c,d)
         end do
         call change_basis_using_(v(:,:,c,c),P)
      end do
      call destroy_(P)
      if (present(new_n_bf)) then
         new_v = v(1:new_n_bf,1:new_n_bf,1:new_n_bf,1:new_n_bf)
         call shrink_(H,new_n_bf,new_n_bf)
         do c = 1,new_n_bf
            do d = 1,c-1
               H = new_v(:,d,c,:)
               new_v(:,d,c,:) = new_v(:,c,d,:)
               new_v(:,c,d,:) = H
            end do
         end do
         call write_(eri_archive,new_v)
         call destroy_(new_v)
      else
         do c = 1,self%n_bf
            do d = 1,c-1
               H = v(:,d,c,:)
               v(:,d,c,:) = v(:,c,d,:)
               v(:,c,d,:) = H
            end do
         end do
         call write_(eri_archive,v)
      end if
      call destroy_(S)
      call destroy_(H)
   end subroutine

   subroutine make_MO_EDM_integrals(self,new_n_bf)
   ! Calculate the electric dipole moment (EDM) integrals in the MO basis
   ! optional arguments if one uses a reduced mo basis set
      type(MOLECULE_TYPE), INOUT :: self
      INT, optional, IN :: new_n_bf
      type(ARCHIVE_TYPE) :: Dx_archive,Dy_archive,Dz_archive
      MAT4(REAL,:,:,:,:), allocatable :: v,new_v
      MAT(REAL,:,:), allocatable :: P,Dx,Dy,Dz,S
      INT :: n_bf,a,b,c,d
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_MO_EDM_integrals ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_MO_EDM_integrals ... no atom list")
   ENSURE(allocated(self%MOs),"MOLECULE.FOCK:make_MO_EDM_integrals ... no molecular orbitals")
   ENSURE(allocated(self%MOs%r),"MOLECULE.FOCK:make_MO_EDM_integrals ... no molecular orbitals")
      call set_(Dx_archive,self%name,"Dx_integrals")
      call set_(Dy_archive,self%name,"Dy_integrals")
      call set_(Dz_archive,self%name,"Dz_integrals")
      n_bf = self%n_bf
      call create_(v,n_bf,n_bf,n_bf,n_bf)
      if (present(new_n_bf)) then
         call create_(P,n_bf,new_n_bf)
         P = self%MOs%r(:,1:new_n_bf)
         call create_(new_v,new_n_bf,new_n_bf,new_n_bf,new_n_bf)
      else
         call create_(P,n_bf,n_bf)
         P = self%MOs%r
      end if
      call create_(Dx,n_bf,n_bf)
      call create_(Dy,n_bf,n_bf)
      call create_(Dz,n_bf,n_bf)
      call create_(S,n_bf,n_bf)
      call make_dipole_matrices_(self,Dx,Dy,Dz)
      call make_overlap_mx_(self)

      ! Minus for electronic charge
      Dx = -Dx/(2*self%n_a-1)
      Dy = -Dy/(2*self%n_a-1)
      Dz = -Dz/(2*self%n_a-1)
      S = self%overlap_mx

      ! Do x component
      v = ZERO
      do a = 1,n_bf
         do b = 1,a-1
            v(a,b,:,:) = Dx*S(a,b)+S*Dx(a,b)
            call change_basis_using_(v(a,b,:,:),P)
            v(b,a,:,:) = v(a,b,:,:)
         end do
         v(a,a,:,:) = Dx*S(a,a)+S*Dx(a,a)
         call change_basis_using_(v(a,a,:,:),P)
      end do
      do c = 1,n_bf
         do d = 1,c-1
            call change_basis_using_(v(:,:,c,d),P)
            v(:,:,d,c) = v(:,:,c,d)
         end do
         call change_basis_using_(v(:,:,c,c),P)
      end do
      if (present(new_n_bf)) then
         new_v = v(1:new_n_bf,1:new_n_bf,1:new_n_bf,1:new_n_bf)
         call shrink_(Dx,new_n_bf,new_n_bf)
         do c = 1,new_n_bf
            do d = 1,c-1
               Dx = new_v(:,d,c,:)
               new_v(:,d,c,:) = new_v(:,c,d,:)
               new_v(:,c,d,:) = Dx
            end do
         end do
         call write_(Dx_archive,new_v)
      else
         do c = 1,self%n_bf
            do d = 1,c-1
               Dx = v(:,d,c,:)
               v(:,d,c,:) = v(:,c,d,:)
               v(:,c,d,:) = Dx
            end do
         end do
         call write_(Dx_archive,v)
      end if
      call destroy_(Dx)

      ! Do y component
      v = ZERO
      do a = 1,n_bf
         do b = 1,a-1
            v(a,b,:,:) = Dy*S(a,b)+S*Dy(a,b)
            call change_basis_using_(v(a,b,:,:),P)
            v(b,a,:,:) = v(a,b,:,:)
         end do
         v(a,a,:,:) = Dy*S(a,a)+S*Dy(a,a)
         call change_basis_using_(v(a,a,:,:),P)
      end do
      do c = 1,n_bf
         do d = 1,c-1
            call change_basis_using_(v(:,:,c,d),P)
            v(:,:,d,c) = v(:,:,c,d)
         end do
         call change_basis_using_(v(:,:,c,c),P)
      end do
      if (present(new_n_bf)) then
         new_v = v(1:new_n_bf,1:new_n_bf,1:new_n_bf,1:new_n_bf)
         call shrink_(Dy,new_n_bf,new_n_bf)
         do c = 1,new_n_bf
            do d = 1,c-1
               Dy = new_v(:,d,c,:)
               new_v(:,d,c,:) = new_v(:,c,d,:)
               new_v(:,c,d,:) = Dy
            end do
         end do
         call write_(Dy_archive,new_v)
      else
         do c = 1,self%n_bf
            do d = 1,c-1
               Dy = v(:,d,c,:)
               v(:,d,c,:) = v(:,c,d,:)
               v(:,c,d,:) = Dy
            end do
         end do
         call write_(Dy_archive,v)
      end if
      call destroy_(Dy)

      ! Do z component
      v = ZERO
      do a = 1,n_bf
         do b = 1,a-1
            v(a,b,:,:) = Dz*S(a,b)+S*Dz(a,b)
            call change_basis_using_(v(a,b,:,:),P)
            v(b,a,:,:) = v(a,b,:,:)
         end do
         v(a,a,:,:) = Dz*S(a,a)+S*Dz(a,a)
         call change_basis_using_(v(a,a,:,:),P)
      end do
      do c = 1,n_bf
         do d = 1,c-1
            call change_basis_using_(v(:,:,c,d),P)
            v(:,:,d,c) = v(:,:,c,d)
         end do
         call change_basis_using_(v(:,:,c,c),P)
      end do
      if (present(new_n_bf)) then
         new_v = v(1:new_n_bf,1:new_n_bf,1:new_n_bf,1:new_n_bf)
         call shrink_(Dz,new_n_bf,new_n_bf)
         do c = 1,new_n_bf
            do d = 1,c-1
               Dz = new_v(:,d,c,:)
               new_v(:,d,c,:) = new_v(:,c,d,:)
               new_v(:,c,d,:) = Dz
            end do
         end do
         call write_(Dz_archive,new_v)
      else
         do c = 1,self%n_bf
            do d = 1,c-1
               Dz = v(:,d,c,:)
               v(:,d,c,:) = v(:,c,d,:)
               v(:,c,d,:) = Dz
            end do
         end do
         call write_(Dz_archive,v)
      end if
      call destroy_(Dz)
      call destroy_(P)
      call destroy_(S)
      call destroy_(new_v)
      call destroy_(v)
   end subroutine

   subroutine make_read_MO_EDM_integrals(self,new_n_bf)
   ! Calculate the electric dipole moment (EDM) integrals in the
   ! readin MO basis
   ! optional arguments if one uses a reduced mo basis set
      type(MOLECULE_TYPE), INOUT :: self
      INT, optional, IN :: new_n_bf
      type(ARCHIVE_TYPE) :: Dx_archive,Dy_archive,Dz_archive
      MAT4(REAL,:,:,:,:), allocatable :: v,new_v
      MAT(REAL,:,:), allocatable :: P,Dx,Dy,Dz,S
      INT :: n_bf,a,b,c,d
      type(TEXTFILE_TYPE), allocatable :: in
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_read_MO_EDM_integrals ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_read_MO_EDM_integrals ... no atom list")
      if (present(new_n_bf)) then
         call create_(P,self%n_bf,new_n_bf)
         call create_(new_v,new_n_bf,new_n_bf,new_n_bf,new_n_bf)
      else
         call create_(P,self%n_bf,self%n_bf)
      end if
      call create_(in,"readin_mos")
      call open_for_(in,"read")
      call read_(in,P,by_column=TRUE)
      call close_(in)
      call destroy_(in)
      call set_(Dx_archive,self%name,"Dx_integrals")
      call set_(Dy_archive,self%name,"Dy_integrals")
      call set_(Dz_archive,self%name,"Dz_integrals")
      n_bf = self%n_bf
      call create_(v,n_bf,n_bf,n_bf,n_bf)
      call create_(Dx,n_bf,n_bf)
      call create_(Dy,n_bf,n_bf)
      call create_(Dz,n_bf,n_bf)
      call create_(S,n_bf,n_bf)
      call make_dipole_matrices_(self,Dx,Dy,Dz)
      call make_overlap_mx_(self)
      Dx = -Dx/(TWO*self%n_a-1)  !minus for electronic charge
      Dy = -Dy/(TWO*self%n_a-1)
      Dz = -Dz/(TWO*self%n_a-1)
      S = self%overlap_mx

      ! Do x component
      v = ZERO
      do a = 1,n_bf
         do b = 1,a-1
            v(a,b,:,:) = Dx*S(a,b)+S*Dx(a,b)
            call change_basis_using_(v(a,b,:,:),P)
            v(b,a,:,:) = v(a,b,:,:)
         end do
         v(a,a,:,:) = Dx*S(a,a)+S*Dx(a,a)
         call change_basis_using_(v(a,a,:,:),P)
      end do
      do c = 1,n_bf
         do d = 1,c-1
            call change_basis_using_(v(:,:,c,d),P)
            v(:,:,d,c) = v(:,:,c,d)
         end do
         call change_basis_using_(v(:,:,c,c),P)
      end do
      if (present(new_n_bf)) then
         new_v = v(1:new_n_bf,1:new_n_bf,1:new_n_bf,1:new_n_bf)
         call shrink_(Dx,new_n_bf,new_n_bf)
         do c = 1,new_n_bf
            do d = 1,c-1
               Dx = new_v(:,d,c,:)
               new_v(:,d,c,:) = new_v(:,c,d,:)
               new_v(:,c,d,:) = Dx
            end do
         end do
         call write_(Dx_archive,new_v)
      else
         do c = 1,self%n_bf
            do d = 1,c-1
               Dx = v(:,d,c,:)
               v(:,d,c,:) = v(:,c,d,:)
               v(:,c,d,:) = Dx
            end do
         end do
         call write_(Dx_archive,v)
      end if
      call destroy_(Dx)

   ! Do y component
      v = ZERO
      do a = 1,n_bf
         do b = 1,a-1
            v(a,b,:,:) = Dy*S(a,b)+S*Dy(a,b)
            call change_basis_using_(v(a,b,:,:),P)
            v(b,a,:,:) = v(a,b,:,:)
         end do
         v(a,a,:,:) = Dy*S(a,a)+S*Dy(a,a)
         call change_basis_using_(v(a,a,:,:),P)
      end do
      do c = 1,n_bf
         do d = 1,c-1
            call change_basis_using_(v(:,:,c,d),P)
            v(:,:,d,c) = v(:,:,c,d)
         end do
         call change_basis_using_(v(:,:,c,c),P)
      end do
      if (present(new_n_bf)) then
         new_v = v(1:new_n_bf,1:new_n_bf,1:new_n_bf,1:new_n_bf)
         call shrink_(Dy,new_n_bf,new_n_bf)
         do c = 1,new_n_bf
            do d = 1,c-1
               Dy = new_v(:,d,c,:)
               new_v(:,d,c,:) = new_v(:,c,d,:)
               new_v(:,c,d,:) = Dy
            end do
         end do
         call write_(Dy_archive,new_v)
      else
         do c = 1,self%n_bf
            do d = 1,c-1
               Dy = v(:,d,c,:)
               v(:,d,c,:) = v(:,c,d,:)
               v(:,c,d,:) = Dy
            end do
         end do
         call write_(Dy_archive,v)
      end if
      call destroy_(Dy)

   ! Do z component
      v = ZERO
      do a = 1,n_bf
         do b = 1,a-1
            v(a,b,:,:) = Dz*S(a,b)+S*Dz(a,b)
            call change_basis_using_(v(a,b,:,:),P)
            v(b,a,:,:) = v(a,b,:,:)
         end do
         v(a,a,:,:) = Dz*S(a,a)+S*Dz(a,a)
         call change_basis_using_(v(a,a,:,:),P)
      end do
      do c = 1,n_bf
         do d = 1,c-1
            call change_basis_using_(v(:,:,c,d),P)
            v(:,:,d,c) = v(:,:,c,d)
         end do
         call change_basis_using_(v(:,:,c,c),P)
      end do
      if (present(new_n_bf)) then
         new_v = v(1:new_n_bf,1:new_n_bf,1:new_n_bf,1:new_n_bf)
         call shrink_(Dz,new_n_bf,new_n_bf)
         do c = 1,new_n_bf
            do d = 1,c-1
               Dz = new_v(:,d,c,:)
               new_v(:,d,c,:) = new_v(:,c,d,:)
               new_v(:,c,d,:) = Dz
            end do
         end do
         call write_(Dz_archive,new_v)
      else
         do c = 1,self%n_bf
            do d = 1,c-1
               Dz = v(:,d,c,:)
               v(:,d,c,:) = v(:,c,d,:)
               v(:,c,d,:) = Dz
            end do
         end do
         call write_(Dz_archive,v)
      end if
      call destroy_(Dz)
      call destroy_(P)
      call destroy_(S)
      if (present(new_n_bf)) then
         call destroy_(new_v)
      end if
      call destroy_(v)
   end subroutine


!  ==================
!  Lower memory ERIs?
!  ==================

   subroutine make_MO_ERIs_v0(self,v_MO,lo,hv)
   ! Calculate two-electron repulsion integrals (ERI's) 
   ! "v_MO" in the (11|22) chemical notation, in MO basis.
   ! Produces integrals in MO window:
   !    "lo" <= w <= "hv"
   ! where "w" is an index of the 2e integrals "vMO".
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: lo
      INT, IN :: hv
      MAT4(REAL,lo:hv,lo:hv,lo:hv,lo:hv), OUT :: v_MO  ! NOTE
      INT :: ab,cd
      INT :: a,aa,fa,la,na
      INT :: b,bb,fb,lb,nb
      INT :: c,cc,fc,lc,nc
      INT :: d,dd,fd,ld,nd
      MAT(REAL,:,:), allocatable :: MOs,MOt,W,X
      MAT4(REAL,:,:,:,:), allocatable :: vAO,vXX

      ! MO range
      MOs = self%MOs%r(:,lo:hv)  ! MOs
      MOt = transpose(self%MOs%r(:,lo:hv))

      ! Do by shell blocks
      v_MO = ZERO

      ! SCHWARZ LIMITS?
      do ab = 1,self%n_shell_pairs

         ! (ab| shell pair a>b
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb)
         do cd = 1,self%n_shell_pairs

            ! (cd| shell pair c>d
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd)

            ! Half-MO integrals
            call create_(vXX,[lo,hv],[lo,hv],[1,nc],[1,nd])

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call create_(vAO,na,nb,nc,nd)
            call make_ERIs_for_shellpairs_(self,ab,cd,vAO)
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            ! Transform ab
            if (aa==bb) then
               do c = 1,nc
                  do d = 1,nd
                     W = matmul(vAO(:,:,c,d),MOs(fb:lb,:))
                     X = matmul(MOt(:,fa:la),W)
                     vXX(:,:,c,d) = X
                  end do
               end do
            else
               do c = 1,nc
                  do d = 1,nd
                     W = matmul(vAO(:,:,c,d),MOs(fb:lb,:))
                     X = matmul(MOt(:,fa:la),W)
                     vXX(:,:,c,d) = X+transpose(X)
                  end do
               end do
            end if
            call destroy_(vAO)

            ! Transform cd
            if (cc==dd) then
               do a = lo,hv
                  do b = lo,hv
                     W = matmul(vXX(a,b,:,:),MOs(fd:ld,:))
                     X = matmul(MOt(:,fc:lc),W)
                     v_MO(a,b,:,:) = v_MO(a,b,:,:)+X
                  end do
               end do
            else
               do a = lo,hv
                  do b = lo,hv
                     W = matmul(vXX(a,b,:,:),MOs(fd:ld,:))
                     X = matmul(MOt(:,fc:lc),W)
                     v_MO(a,b,:,:) = v_MO(a,b,:,:)+X+transpose(X)
                  end do
               end do
            end if
            call destroy_(vXX)
         end do
      end do

      ! Clean
      call destroy_(MOt)
      call destroy_(MOs)
   end subroutine

   subroutine make_MO_ERIs_v1(self,v_MO,lo,hv)
   ! Calculate two-electron repulsion integrals (ERI's) 
   ! "v_MO" in the (11|22) chemical notation, in MO basis.
   ! Produces integrals in MO window:
   !    "lo" <= w <= "hv"
   ! where "w" is an index of the 2e integrals "vMO".
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: lo
      INT, IN :: hv
      MAT4(REAL,lo:hv,lo:hv,lo:hv,lo:hv), OUT :: v_MO  ! NOTE
      INT :: ab,cd
      INT :: a,aa,fa,la,na
      INT :: b,bb,fb,lb,nb
      INT :: c,cc,fc,lc,nc
      INT :: d,dd,fd,ld,nd
      MAT(REAL,:,:), allocatable :: MOs,MOt,W,X
      MAT4(REAL,:,:,:,:), allocatable :: vAO,vXX

      ! MO range
      MOs = self%MOs%r(:,lo:hv)  ! MOs
      MOt = transpose(self%MOs%r(:,lo:hv))

      ! Do by shell blocks
      v_MO = ZERO

      ! SCHWARZ LIMITS?
      do ab = 1,self%n_shell_pairs

         ! (ab| shell pair a>b
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb)
         do cd = 1,self%n_shell_pairs

            ! (cd| shell pair c>d
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd)

            ! Half-MO integrals
            call create_(vXX,[lo,hv],[lo,hv],[1,nc],[1,nd])

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call create_(vAO,na,nb,nc,nd)
            call make_ERIs_for_shellpairs_(self,ab,cd,vAO)
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            ! Transform ab
            if (aa==bb) then
               do c = 1,nc
                  do d = 1,nd
                     W = matmul(vAO(:,:,c,d),MOs(fb:lb,:))
                     X = matmul(MOt(:,fa:la),W)
                     vXX(:,:,c,d) = X
                  end do
               end do
            else
               do c = 1,nc
                  do d = 1,nd
                     W = matmul(vAO(:,:,c,d),MOs(fb:lb,:))
                     X = matmul(MOt(:,fa:la),W)
                     vXX(:,:,c,d) = X+transpose(X)
                  end do
               end do
            end if
            call destroy_(vAO)

            ! Transform cd
            if (cc==dd) then
               do a = lo,hv
                  do b = lo,hv
                     W = matmul(vXX(a,b,:,:),MOs(fd:ld,:))
                     X = matmul(MOt(:,fc:lc),W)
                     v_MO(:,:,a,b) = v_MO(:,:,a,b)+X
                  end do
               end do
            else
               do a = lo,hv
                  do b = lo,hv
                     W = matmul(vXX(a,b,:,:),MOs(fd:ld,:))
                     X = matmul(MOt(:,fc:lc),W)
                     v_MO(:,:,a,b) = v_MO(:,:,a,b)+X+transpose(X)
                  end do
               end do
            end if
            call destroy_(vXX)
         end do
      end do

      ! Clean
      call destroy_(MOt)
      call destroy_(MOs)
   end subroutine

   subroutine make_MO_ERIs_v2(self,v_MO,lo,hv)
   ! Calculate two-electron repulsion integrals (ERI's) 
   ! "v_MO" in the (11|22) chemical notation, in MO basis.
   ! Produces integrals in MO window:
   !    "lo" <= w <= "hv"
   ! where "w" is an index of the 2e integrals "vMO".
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: lo
      INT, IN :: hv
      MAT4(REAL,lo:hv,lo:hv,lo:hv,lo:hv), OUT :: v_MO  ! NOTE
      INT :: ab,cd
      INT :: a,aa,fa,la,na
      INT :: b,bb,fb,lb,nb
      INT :: c,cc,fc,lc,nc
      INT :: d,dd,fd,ld,nd
      MAT(REAL,:,:), allocatable :: MOs,MOt,X
      MAT4(REAL,:,:,:,:), allocatable :: vAO,vXX

      ! MO range
      MOs = self%MOs%r(:,lo:hv)  ! MOs
      MOt = transpose(self%MOs%r(:,lo:hv))

      ! Do by shell blocks
      v_MO = ZERO

      ! SCHWARZ LIMITS?
      do ab = 1,self%n_shell_pairs

         ! (ab| shell pair a>b
         call set_shell2_indices_from_(self,ab,aa,bb,fa,la,na,fb,lb,nb)
         do cd = 1,self%n_shell_pairs

            ! (cd| shell pair c>d
            call set_shell2_indices_from_(self,cd,cc,dd,fc,lc,nc,fd,ld,nd)

            ! Half-MO integrals
         ! vXX.create([lo,hv],[lo,hv],[1,nc],[1,nd])
            call create_(vXX,[1,nc],[1,nd],[lo,hv],[lo,hv])

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call create_(vAO,na,nb,nc,nd)
            call make_ERIs_for_shellpairs_(self,ab,cd,vAO)
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            ! Transform ab
            if (aa==bb) then
               do c = 1,nc
                  do d = 1,nd
                     X = matmul(MOt(:,fa:la),matmul(vAO(:,:,c,d),MOs(fb:lb,:)))
                     vXX(c,d,:,:) = X
                  end do
               end do
            else
               do c = 1,nc
                  do d = 1,nd
                     X = matmul(MOt(:,fa:la),matmul(vAO(:,:,c,d),MOs(fb:lb,:)))
                     vXX(c,d,:,:) = X+transpose(X)
                  end do
               end do
            end if
            call destroy_(vAO)

            ! Transform cd
            if (cc==dd) then
               do a = lo,hv
                  do b = lo,hv
                     X = matmul(MOt(:,fc:lc),matmul(vXX(:,:,a,b),MOs(fd:ld,:)))
                     v_MO(:,:,a,b) = v_MO(:,:,a,b)+X
                  end do
               end do
            else
               do a = lo,hv
                  do b = lo,hv
                     X = matmul(MOt(:,fc:lc),matmul(vXX(:,:,a,b),MOs(fd:ld,:)))
                     v_MO(:,:,a,b) = v_MO(:,:,a,b)+X+transpose(X)
                  end do
               end do
            end if
            call destroy_(vXX)
         end do
      end do

      ! Clean
      call destroy_(MOt)
      call destroy_(MOs)
   end subroutine

   PURE subroutine make_ERIs_for_shellpairs(self,ab,cd,v)
   ! Calculate the (11|22) type ERIs for a basis-set shell 
   ! pair with pair-index "ab" and place in array "v".
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: ab
      INT, IN :: cd
      MAT4(REAL,:,:,:,:), OUT :: v
      VEC(REAL,:), allocatable :: I
      type(SHELL1QUARTET_TYPE) :: sh4n,sh4s
      INT :: a,aa,na,atom_a
      INT :: b,bb,nb,atom_b
      INT :: c,cc,nc,atom_c
      INT :: d,dd,nd,atom_d
      INT :: abcd
      BIN :: same
   ENSURE(self%basis_info_made,"MOLECULE.FOCK:make_ERIs_for_shellpairs ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.FOCK:make_ERIs_for_shellpairs ... no atom list")

      ! Basis fn limits for (ab| & |cd) shell pairs
      call set_shell2_indices_from_(self,ab,aa,bb,na,nb,atom_a,atom_b)
      call set_shell2_indices_from_(self,cd,cc,dd,nc,nd,atom_c,atom_d)

      ! Get (ab|cd) integrals in I(.)
      same = atom_a==atom_b OR atom_c==atom_d OR atom_a==atom_c OR atom_a==atom_d OR atom_b==atom_c OR atom_b==atom_d
      if (same) then
         call set_shell1q_ab_from_(self,aa,bb,sh4n,skip=FALSE)
         call set_shell1q_cd_from_(self,cc,dd,sh4n,skip=FALSE)
         call create_(I,sh4n%ab_n_bf_pairs*sh4n%cd_n_bf_pairs)
         call make_ERI(sh4n,I)
      else
         call set_shell1q_ab_from_(self,aa,bb,sh4s,skip=TRUE)
         call set_shell1q_cd_from_(self,cc,dd,sh4s,skip=TRUE)
         call create_(I,sh4s%ab_n_bf_pairs*sh4s%cd_n_bf_pairs)
         call make_ERI(sh4s,I)
      end if

      ! Copy integrals
      abcd = 0
      do d = 1,nd
         do c = 1,nc
            do b = 1,nb
               do a = 1,na
                  abcd = abcd+1
                  v(a,b,c,d) = I(abcd)
               end do
            end do
         end do
      end do

      ! Clean
      call destroy_(I)
   end subroutine


!  ========================================
!  Electron repulsion integrals - in memory
!  ========================================

   subroutine make_MO_ERI_chem_integrals(self,v)
   ! Calculate the electron repulsion integrals (ERI's) 
   ! in the (11|22) chemical notation, in the MO basis, 
   ! from the AO integrals
      type(MOLECULE_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: v
      INT :: a,b,c,d
      v = ZERO
      call make_ERI_integrals_(self,v)
      do a = 1,self%n_bf
         call change_basis_using_(v(a,a,:,:),self%MOs%r)
         do b = 1,a-1
            call change_basis_using_(v(a,b,:,:),self%MOs%r)
            v(b,a,:,:) = v(a,b,:,:)
         end do
      end do
      do c = 1,self%n_bf
         call change_basis_using_(v(:,:,c,c),self%MOs%r)
         do d = 1,c-1
            call change_basis_using_(v(:,:,c,d),self%MOs%r)
            v(:,:,d,c) = v(:,:,c,d)
         end do
      end do
   end subroutine

   PURE subroutine make_MO_ERI_integrals_1(self,v)
   ! Calculate the electron repulsion integrals (ERI's) in the MO
   ! basis from the AO integrals
      type(MOLECULE_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: v
      MAT(REAL,:,:), allocatable :: P,H
      INT :: a,b,r,s
      call create_(P,self%n_bf,self%n_bf)
      P = self%MOs%r
      v = ZERO
      call make_ERI_integrals_(self,v)
      do a = 1,self%n_bf
         do b = 1,a-1

            !pcc should swap a and b to make it faster
            call change_basis_using_(v(a,b,:,:),P)
            v(b,a,:,:) = v(a,b,:,:)
         end do
         call change_basis_using_(v(a,a,:,:),P)
      end do

      ! stdout.flush
      ! stdout.text(" First half (ab) of ERI has been transformed! ")
      do r = 1,self%n_bf
         do s = 1,r-1
            call change_basis_using_(v(:,:,r,s),P)
            v(:,:,s,r) = v(:,:,r,s)
         end do
         call change_basis_using_(v(:,:,r,r),P)
      end do
      call destroy_(P)

      ! stdout.flush
      ! stdout.text(" Second half (rs) of ERI has been transformed! ")
      call create_(H,self%n_bf,self%n_bf)

      ! Swap b and c to have it like <a1b2|c1d2> and not (a1b1c2d2) order
      ! hence the Szabo Ostlund notation will work
      do r = 1,self%n_bf
         do s = 1,r-1
            H = v(:,s,r,:)
            v(:,s,r,:) = v(:,r,s,:)
            v(:,r,s,:) = H
         end do
      end do

      ! stdout.flush
      ! stdout.text(" Swap (br) of ERI has been performed! ")
      call destroy_(H)
   end subroutine

   PURE subroutine make_u_MO_ERI_integrals(self,vaa,vbb,vab)
   ! Calculate the electron repulsion integrals (ERI's) in the MO
   ! basis from the AO integrals for the unrestricted MP2 could be
   ! split into three separate routines to lower the the core memory
   ! demands by a factor of three
      type(MOLECULE_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), OUT :: vaa,vbb,vab
      MAT(REAL,:,:), allocatable :: Pa,Pb,H
      INT :: a,b,r,s
      call create_(Pa,self%n_bf,self%n_bf)
      call create_(Pb,self%n_bf,self%n_bf)
      Pa = self%MOs%a
      Pb = self%MOs%b
      vaa = ZERO; vab = ZERO; vbb = ZERO
      call make_ERI_integrals_(self,vaa)
      vab = vaa
      vbb = vaa
      do a = 1,self%n_bf
         do b = 1,a-1

            !pcc should swap a and b to make it faster
            call change_basis_using_(vaa(a,b,:,:),Pa)
            call change_basis_using_(vab(a,b,:,:),Pb)
            call change_basis_using_(vbb(a,b,:,:),Pb)
            vaa(b,a,:,:) = vaa(a,b,:,:)
            vab(b,a,:,:) = vab(a,b,:,:)
            vbb(b,a,:,:) = vbb(a,b,:,:)
         end do
         call change_basis_using_(vaa(a,a,:,:),Pa)
         call change_basis_using_(vab(a,a,:,:),Pb)
         call change_basis_using_(vbb(a,a,:,:),Pb)
      end do

      ! stdout.flush
      ! stdout.text(" First half (ab) of ERI has been transformed! ")
      do r = 1,self%n_bf
         do s = 1,r-1
            call change_basis_using_(vaa(:,:,r,s),Pa)
            call change_basis_using_(vab(:,:,r,s),Pa)
            call change_basis_using_(vbb(:,:,r,s),Pb)
            vaa(:,:,s,r) = vaa(:,:,r,s)
            vab(:,:,s,r) = vab(:,:,r,s)
            vbb(:,:,s,r) = vbb(:,:,r,s)
         end do
         call change_basis_using_(vaa(:,:,r,r),Pa)
         call change_basis_using_(vab(:,:,r,r),Pa)
         call change_basis_using_(vbb(:,:,r,r),Pb)
      end do
      call destroy_(Pa)
      call destroy_(Pb)

      ! stdout.flush
      ! stdout.text(" Second half (rs) of ERI has been transformed! ")
      call create_(H,self%n_bf,self%n_bf)

      ! swap b and c to have it like <a1b2|c1d2> and not (a1b1c2d2) order
      ! hence the Szabo Ostlund notation will work
      do r = 1,self%n_bf
         do s = 1,r-1
            H = vaa(:,s,r,:)
            vaa(:,s,r,:) = vaa(:,r,s,:)
            vaa(:,r,s,:) = H
            H = vab(:,s,r,:)
            vab(:,s,r,:) = vab(:,r,s,:)
            vab(:,r,s,:) = H
            H = vbb(:,s,r,:)
            vbb(:,s,r,:) = vbb(:,r,s,:)
            vbb(:,r,s,:) = H
         end do
      end do

      ! stdout.flush
      ! stdout.text(" Swap (br) of ERI has been performed! ")
      call destroy_(H)
   end subroutine

   PURE subroutine make_gc_MO_ERI_integrals(self,v)
   ! Calculate the electron repulsion integrals (ERI's) in the MO
   ! basis from the AO integrals
   ! current version: L. Rispanova, Diploma Thesis, STUBA, 2016.
   ! (supervisor: L.  Bucinsky)
      type(MOLECULE_TYPE), IN :: self
      MAT4(CPX,:,:,:,:), OUT :: v
      MAT4(CPX,:,:,:,:), allocatable :: va_new,vb_new
      MAT(CPX,:,:), allocatable :: P,H
      MAT(CPX,:,:), allocatable :: m1,m2
      MAT4(REAL,:,:,:,:), allocatable :: vr
      INT :: a,b,r,s
      CPX :: x1
      call create_(P,2*self%n_bf,2*self%n_bf)
      P = self%MOs%gc
      x1 = (ONE,ZERO)
      v = (ZERO,ZERO)
      call create_(vr,self%n_bf,self%n_bf,self%n_bf,self%n_bf)
      vr = ZERO
      call make_ERI_integrals_(self,vr)
      call create_(va_new,self%n_bf,self%n_bf,2*self%n_bf,2*self%n_bf)
      call create_(vb_new,self%n_bf,self%n_bf,2*self%n_bf,2*self%n_bf)
      va_new = (ZERO,ZERO)
      vb_new = (ZERO,ZERO)
      do a = 1,self%n_bf
         do b = 1,self%n_bf
            call change_basis_to_(vr(a,b,:,:),va_new(a,b,:,:),P(1:self%n_bf,:))
            call change_basis_to_(vr(a,b,:,:),vb_new(a,b,:,:),P(1+self%n_bf:,:))
         end do
      end do
      call destroy_(vr)

      ! stdout.flush
      ! stdout.text(" First half (ab) of ERI has been transformed! ")
      call create_(m1,self%n_bf,self%n_bf)
      call create_(m2,2*self%n_bf,2*self%n_bf)
      do r = 1,2*self%n_bf
         do s = 1,2*self%n_bf
            m1 = va_new(:,:,r,s)
            m2 = (ZERO,ZERO)
            call change_basis_to_(m1,m2,P(1:self%n_bf,:))  !aa
            v(:,:,r,s) = m2
            m2 = (ZERO,ZERO)
            call change_basis_to_(m1,m2,P(1+self%n_bf:,:))  !ba
            v(:,:,r,s) = m2+v(:,:,r,s)
            m1 = vb_new(:,:,r,s)
            m2 = (ZERO,ZERO)
            call change_basis_to_(m1,m2,P(1:self%n_bf,:))  !ab
            v(:,:,r,s) = m2+v(:,:,r,s)
            m2 = (ZERO,ZERO)
            call change_basis_to_(m1,m2,P(1+self%n_bf:,:))  !bb
            v(:,:,r,s) = m2+v(:,:,r,s)
         end do
      end do

      ! stdout.flush
      ! stdout.text(" Second half (rs) of ERI has been transformed! ")
      call destroy_(m1)
      call destroy_(m2)
      call destroy_(va_new)
      call destroy_(vb_new)
      call create_(H,2*self%n_bf,2*self%n_bf)

      ! swap b and c to have it like <a1b2|c1d2> and not (a1b1|c2d2) order
      ! hence the Szabo Ostlund notation will work
      do r = 1,2*self%n_bf
         do s = 1,r-1
            H = v(:,s,r,:)
            v(:,s,r,:) = v(:,r,s,:)
            v(:,r,s,:) = H
         end do
      end do

      ! stdout.flush
      ! stdout.text(" Swap (br) of ERI has been performed! ")
      ! stdout.flush
      ! WARN("The FOCK routine on ERI GCHF should be OK now.")
      ! stdout.flush
      call destroy_(H)
   end subroutine

end module
