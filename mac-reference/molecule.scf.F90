!==================================================================
!
! MOLECULE.SCF: SCF-related methods.
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
!
!==================================================================
module MOLECULE_SCF_MODULE

#  include "macros"

   include "molecule.scf.use"

   implicit none

   include "molecule.scf.int"


contains

!  ====================
!  Atom partition setup
!  ====================

   subroutine make_atom_partition_info(self,need_moments)
   ! Make the atom partition information, including the atomic charges
   ! and moments, by the X-ray partition model. A model that is not
   ! one-centre makes no moments; if "need_moments" is TRUE, or cluster
   ! charges or Lorentz fields are in use, Hirshfeld atom moments are
   ! made for it.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: need_moments
      STR(len=STR_SIZE) :: model
      BIN :: need

      ! Are moments needed whatever the model?
      need = FALSE
      if (present(need_moments)) need = need_moments
      if (allocated(self%SCF_data)) then
         if (self%SCF_data%using_cluster_charges OR self%SCF_data%using_SC_Lorentz_fields) need = TRUE
      end if

      ! Get partition model
      model = "oc-hirshfeld"
      if (allocated(self%crystal)) then
         if (allocated(self%crystal%xray_data)) then
            if (self%crystal%xray_data%partition_model/=" ") then
               model = self%crystal%xray_data%partition_model
            end if
         end if
      end if

      ! oc-ri needs the spherical harmonics up to RI_l_max
      if (model=="oc-ri") call set_indices_(self%crystal%xray_data%RI_l_max)

      ! Set up atom partition info
      select case (model)
      case ("oc-crystal23  "); call make_HA_info_(self)
      case ("oc-g-hirshfeld"); call make_HA_info_(self)
      case ("oc-g-salvador "); call make_SA_info_(self)
      case ("oc-hirshfeld  "); call make_HA_info_(self)
      case ("oc-exphar     "); call make_HA_info_(self)
      case ("oc-sph-exphar "); call make_HA_info_(self)
      case ("oc-ri         "); call make_HA_info_(self)
      case ("oc-salvador   "); call make_SA_info_(self)
      case ("oc-tfvp       "); call make_SA_info_(self)
      case ("oc-tfvh       "); call make_SA_info_(self)
      case ("oc-sph-tfvp   "); call make_SA_info_(self)
      case ("oc-sph-tfvh   "); call make_SA_info_(self)
      case ("oc-sph-tfva   "); call make_SA_info_(self)
      case default
         if (need) call make_HA_info_(self)
      end select

      ! Atom partition info (including atomic moments) is now made. Required
      ! by make_cluster_charge_mx / the cluster-charge core matrix. (Release
      ! builds compile out that ENSURE, so this was previously never set.)
      self%partition_info_made = TRUE
   end subroutine


!  ===============
!  Hirshfeld atoms
!  ===============

   subroutine make_HA_info(self,skip_ANOs,skip_moments,output)
   ! Set up the quanitites for a Hirshfeld calculation:
   ! (1) DFT grid information
   ! (2) ANO's and ANO interpolator grid information
   ! (3) Assign NO's to MO's
   ! (4) Make HA atomic moments
   ! NOTE: this procedure is in .SCF because a molecular SCF
   ! is done for each atom. We could change this by making
   ! atom-specific SCF routines ... that would be nicer.
   ! WARNING: WHY SKIP STUFF, just call what you need? --dylan
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: skip_ANOs
      BIN, optional, IN :: skip_moments
      BIN, optional, IN :: output
      BIN :: skip_ANO
      BIN :: skip_mom
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_HA_info ... no atoms")
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_HA_info ... no basis info")
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_HA_info ... no SCF_data")

      ! Skip ANOs & interpolatoprs?
      skip_ANO = FALSE
      if (present(skip_ANOs)) skip_ANO = skip_ANOs

      ! Skip Hirshfeld atom moments
      skip_mom = FALSE
      if (present(skip_moments)) then
         skip_mom = skip_moments
         ! If need moments, need interpolators
         if (NOT skip_mom) skip_ANO = FALSE
      end if

      ! Resolve bases & update
      call update_atom_basis_group_info_(self)

      ! Set up integration grid
      call set_up_becke_grid_(self)  ! NOT PURE

      ! Skip ANO's and interpolators?
      if (NOT skip_ANO) then
         call make_ANOs_and_interpolators_(self,output)
      end if

      ! Make Hirshfeld atom moments?
      ! WARNING: other atomic partitions!
      ! MAYBE: skip this?
      if (NOT skip_mom) then
         if (allocated(self%density_mx)) then
            if (NOT self%partition_info_made) then
               call make_r_HA_moments_(self,output=output)
            end if
         end if
      end if

      ! Assign NO's to MO's for density evaluation
   ! if (.MOs.allocated) .assign_NOs_to_MOs
   end subroutine


!   read_or_make_SCF_DM :: leaky, private
!   ! Read of make an SCF density matrix if it is not there
!      self :: INOUT
!
!   ENSURE(.basis_info_made, "no basis info")
!
!      archive :: ARCHIVE
!      genre :: STR
!
!      .density_mx.destroy
!      .density_mx.create(.n_bf)
!
!      ! Get genre
!      if (.SCF_data.allocated) then; genre = .SCF_data.spinorbital_genre
!      else;                         genre = "r "
!      end
!
!      ! Read density or make it from MO's
!      archive.set(.name,"density_mx",genre)
!      if (archive.exists) then
!         archive.read(.density_mx)
!      else
!         .MOs.destroy
!         .MOs.create(.n_bf)
!         archive.set(.name,"MOs",genre)
!         archive.read(.MOs)
!         .make_SCF_density_mx
!      end
!
!   end

!  =================
!  Structure factors
!  =================

!  X-ray

   subroutine make_X_SFs(self,output)
   ! Make the X-ray structure factors for this molecule.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: output
      BIN :: out
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_X_SFs ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.SCF:make_X_SFs ... no x-ray reflection data")

      ! Output
      out = FALSE
      if (present(output)) out = output
      if (out) then
         call flush_(stdout)
         call text_(stdout,"============================")
         call text_(stdout,"Make X-ray structure factors")
         call text_(stdout,"============================")
         call flush_(stdout)
         call show_(stdout,"Number of reflections     = ",size(self%crystal%xray_data%reflections))
         call show_(stdout,"Density partition model   = ",trim(self%crystal%xray_data%partition_model))
         call show_(stdout,"Is one-center TF model?   = ",using_one_center_TF_model_(self%crystal))
         call show_(stdout,"Use disk SF data?         = ",self%crystal%xray_data%use_disk_SFs)
         call flush_(stdout)
      end if

   ! if (.crystal.uses_oc_atom_partition) .make_atom_partition_info

      ! Make the SF's
      select case (self%crystal%xray_data%partition_model)

      ! Two-center models
      case ("none          ","tc-coppens    ","tc-stewart    ","tc-tanaka     "); call make_X_SFs_RF_(self)

      ! One-center models
      case ("oc-crystal23  "); call make_X_SFs_HAR_(self)  ! Done in HAR
      case ("oc-g-hirshfeld"); call make_X_SFs_HAR_(self)
      case ("oc-hirshfeld  "); call make_X_SFs_HAR_(self)
      case ("oc-exphar     "); call make_X_SFs_HAR_(self)
      case ("oc-sph-exphar "); call make_X_SFs_HAR_(self)
      case ("oc-ri         "); call make_X_SFs_HAR_(self)
      case ("oc-g-salvador "); call make_X_SFs_HAR_(self)
      case ("oc-salvador   "); call make_X_SFs_HAR_(self)
      case ("oc-tfvp       "); call make_X_SFs_HAR_(self)
      case ("oc-tfvh       "); call make_X_SFs_HAR_(self)
      case ("oc-sph-tfvp   "); call make_X_SFs_HAR_(self)
      case ("oc-sph-tfvh   "); call make_X_SFs_HAR_(self)
      case ("oc-sph-tfva   "); call make_X_SFs_HAR_(self)
      end select
   end subroutine


!   make_X_free_SFs
!   ! Make the *free* X-ray structure factors for this molecule.
!      self :: INOUT
!
!   ENSURE(.crystal.allocated, "no crystal")
!   ENSURE(.crystal.xray_r_free_data.allocated, "no x-ray R-free reflection data")
!
!      ! Assign data
!      .crystal.assign_xray_r_free
!
!
!      ! Make the SF's
!      select case (.crystal.temperature_factor_model)
!

!            "tc-tanaka   ") ; .make_X_SFs_RF
!
!      case ("oc-hirshfeld") ; .make_X_SFs_HAR
!
!      case ("oc-tanaka   ") ; .make_X_SFs_TAR
!
!      end
!
!   end

!  X-ray helpers

   subroutine make_X_SFs_RF(self)
   ! Make basis-fn pair structure factors for this molecule.
   ! NOTE: includes the temperature factor/ADPs
      type(MOLECULE_TYPE), INOUT :: self
      VEC(CPX,:), allocatable :: sf_eq
      VEC(CPX,:), allocatable :: Fc
      MAT(REAL,:,:), allocatable :: k_pts
      STR(len=STR_SIZE) :: genre
      INT :: n_k,n_refl
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_X_SFs_RF ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.SCF:make_X_SFs_RF ... no reflection data")
   ENSURE(allocated(self%density_mx),"MOLECULE.SCF:make_X_SFs_RF ... no density matrix")
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_X_SFs_RF ... no SCF_data")
      n_k = n_unique_SF_k_pts_(self%crystal)
      n_refl = size(self%crystal%xray_data%reflections)

      ! Allocate complex structure factors
      call create_(Fc,n_refl)

      ! Make k points
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)

      ! Make density matrix
      call make_r_density_mx_(self)

      ! Symmetry-generated SF'd
      call create_(sf_eq,n_k)

      ! Make symmetry-generated SF's
      genre = spinorbital_genre_(self%SCF_data)
      if (includes_(genre,"complex")) then

         ! Complex
         if (spinorbital_genre_(self%SCF_data)=="gc") then
            call make_FT_gc_(self,sf_eq,self%density_mx%gc,k_pts)
         end if
      else

         ! Real
         call make_FT_(self,sf_eq,self%density_mx%r,k_pts)
         if (allocated(self%SCF_data)) then
            if (spinorbital_genre_(self%SCF_data)=="u ") then

            ! Ensure pure UHF
               call destroy_(self%density_mx,"r ")
            end if
         end if
      end if

      ! Make the predicted SF's
      call make_F_predicted_from_(self%crystal,sf_eq)
   end subroutine

   subroutine make_X_SFs_HAR(self)
   ! Make the structure factors for this molecule.
   ! WARNING: this does not seem to include the temperature factor
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_X_SFs_HAR ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.SCF:make_X_SFs_HAR ... no reflection data")
      if (self%crystal%xray_data%use_disk_SFs) then; call make_X_SFs_HAR_disk_(self)
      else; call make_X_SFs_HAR_memory_(self)
      end if
   end subroutine

   subroutine make_X_SFs_HAR_disk(self)
   ! Make the structure factors for this molecule.
   ! WARNING: this does not seem to include the temperature factor
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_X_SFs_HAR_disk ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.SCF:make_X_SFs_HAR_disk ... no reflection data")

      ! Get atomic form factors
      call get_Hirshfeld_atom_FFs_disk_(self)

      ! Wait for every rank to finish writing before anyone reads. The loop
      ! inside get_Hirshfeld_atom_FFs_disk is a `parallel do`, so the <tag>-SFs
      ! files are scattered across ranks, while the readers (CRYSTAL:shift_-
      ! update_ff and the two others in CRYSTAL) want all of them. Nothing else
      ! orders that: PARALLEL:unlock_parallel_do only clears a rank-local
      ! string, so a `parallel do` has NO implicit barrier.
      !
      ! Here rather than inside that routine because THIS call is collective --
      ! every rank reaches it, the same number of times. Its per-fragment
      ! callers in fragment_SCF are not; see the note there.
      PARALLEL_BARRIER

      ! Make F_pred & GoF2
      call make_F_predicted_(self%crystal)
   end subroutine

   subroutine make_X_SFs_HAR_memory(self)
   ! Make the structure factors for this molecule.
   ! WARNING: this does not seem to include the temperature factor
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable :: ff
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_X_SFs_HAR_memory ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.SCF:make_X_SFs_HAR_memory ... no reflection data")

      ! ANOs & interpolators
      call make_atom_partition_info_(self)

      ! Get atomic form factors
      call make_aspherical_atomic_FFs_(self,ff)

      ! Make F_pred & GoF2
      call make_F_predicted_(self%crystal,ff)
      call destroy_(ff)
   end subroutine


!   make_X_SFs_TAR :: private
!   ! Make basis-fn pair structure factors for this molecule.
!   ! NOTE: includes the temperature factor/ADPs
!      self :: INOUT
!
!   ENSURE(.crystal.allocated, "no crystal")
!   ENSURE(.crystal.xray_data.allocated, "no reflection data")
!   ENSURE(.density_mx.allocated, "no density matrix")
!   ENSURE(.SCF_data.allocated, "no SCF_data")
!
!      ff :: MAT{CPX}@
!
!      ! Get atomic form factors
!      .get_Tanaka_atom_FFs(ff)
!
!      ! Make F_pred & GoF2
!      .crystal.make_F_predicted(ff)
!
!      ff.destroy
!
!   end

!   make_X_SFs_C23 :: private
!   ! Make the CRYSTAL23 structure factors from, the XML file.
!      self :: INOUT
!
!      ff :: MAT{CPX}@
!
!      ! ANOs & interpolators
!      .make_atom_partition_info
!
!      ! Get atomic form factors
!      .make_C23_Hirshfeld_atom_FFs(ff)
!
!      ! Make F_pred & GoF2
!      .crystal.make_F_predicted(ff)
!
!      ff.destroy
!
!   end

!  For fragHAR

   subroutine make_F_predicted(self)
   ! Make F predicted from SFs files.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_F_predicted ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.SCF:make_F_predicted ... no x-ray reflection data")

      ! Make the SF's
      call make_F_predicted_(self%crystal)  ! NOT PURE
   end subroutine


!  PND

!   make_N_SFs
!   ! Make the PND structure factors for this molecule.
!      self :: INOUT
!
!   ENSURE(.crystal.allocated, "no crystal")
!   ENSURE(.crystal.xray_data.allocated, "no reflection data")
!   ENSURE(.crystal.spacegroup.centrosymmetric, "must be a centrosymmetric crystal")
!
!      if (.crystal.temperature_factor_model=="hirshfeld") then
!         .make_N_SFs_h
!      else
!         .make_N_SFs_p
!      end
!
!   end

!   make_N_free_SFs
!   ! Make the free PND structure factors for this molecule.
!      self :: INOUT
!
!   ENSURE(.crystal.allocated, "no crystal")
!   ENSURE(.crystal.xray_data.allocated, "no reflection data")
!   ENSURE(.crystal.spacegroup.centrosymmetric, "must be a centrosymmetric crystal")
!
!      ! Assign free SF's
!      if (.crystal.temperature_factor_model=="hirshfeld") then
!         .make_N_SFs_h
!      else
!         .make_N_SFs_p
!      end
!
!   end

!  PND helpers

!   make_N_SFs_h :: private
!   ! Make the free PND structure factors for this molecule.
!   ! WARNING: this does not seem to include the temperature factor
!      self :: INOUT
!
!   ENSURE(.crystal.allocated, "no crystal")
!   ENSURE(.crystal.xray_data.allocated, "no reflection data")
!
!      if (.crystal.xray_data.use_disk_SFs) then; .make_N_SFs_h_disk
!      else;                                 .make_N_SFs_h_memory
!      end
!
!   end

!   make_N_SFs_h_memory :: private
!   ! Make the structure factors for this molecule.
!   ! WARNING: this does not seem to include the temperature factor
!      self :: INOUT
!
!   ENSURE(.crystal.allocated, "no crystal")
!   ENSURE(.crystal.xray_data.allocated, "no reflection data")
!   ENSURE(.density_mx.allocated, "no density matrix")
!   ENSURE(.SCF_data.allocated, "no SCF_data")
!
!      ff :: MAT{CPX}@
!
!      ! Get atomic spin form factors
!      .make_Hirshfeld_atom_FFs(ff,spin_density=TRUE)
!
!      ! Make F_pred & GoF2
!      .crystal.make_F_predicted(ff)
!
!      ff.destroy
!
!   end

!   make_N_SFs_h_disk :: private
!   ! Make the structure factors for this molecule.
!   ! WARNING: this does not seem to include the temperature factor
!      self :: INOUT
!
!   ENSURE(.crystal.allocated, "no crystal")
!   ENSURE(.crystal.xray_data.allocated, "no reflection data")
!   ENSURE(.density_mx.allocated, "no density matrix")
!   ENSURE(.SCF_data.allocated, "no SCF_data")
!
!      ! Get atomic spin form factors
!      .make_Hirshfeld_atom_FFs(spin_density=TRUE)
!
!      ! Make F_pred & GoF2
!      .crystal.make_F_predicted
!
!   end

!   make_N_SFs_p :: private
!   ! Make the S_z structure factors for this molecule.
!      self :: INOUT
!
!   ENSURE(.crystal.allocated, "no crystal")
!   ENSURE(.crystal.xray_data.allocated, "no reflection data")
!   ENSURE(.crystal.spacegroup.centrosymmetric, "must be a centrosymmetric crystal")
!
!      sf_eq,Fc :: VEC{CPX}@
!      k_pts :: MAT{REAL}@
!      genre :: STR
!      complx :: BIN
!      n_k,n_refl :: INT
!
!      n_k    = .crystal.n_unique_SF_k_pts
!      n_refl = .crystal.xray_data.reflections.dim
!
!      ! Allocate complex structure factors
!      Fc.create(n_refl)
!
!      ! Make unique k points
!      k_pts.create(n_k,3)
!      .crystal.make_unique_X_SF_k_pts(k_pts)
!
!      ! Make S_z density matrix
!      .make_r_Sz_density_mx
!
!      ! Make the symmetry-generated SF's
!      sf_eq.create(n_k)
!      genre = .density_mx.genre
!      complx = genre.includes("complex")
!      if (complx) then
!         .make_ft(sf_eq,.density_mx.r_complex,k_pts)
!      else
!         .make_ft(sf_eq,.density_mx.r,k_pts)
!      end
!
!      ! Destroy the S_z density matrix
!      .density_mx.destroy("r ")
!
!      ! Make the predicted SF's
!      .crystal.make_F_predicted_from(sf_eq)
!
!   end

!  ===============================
!  Energy density matrix of Krogel
!  ===============================

   subroutine make_E_SCF_density_mx(self,method)
   ! Make the density matrix from the molecular orbitals.
   ! . If "damp" is present use it to damp the updated density matrix.
   !   An .old_density_mx must exist for this to work.
   ! . If "n_a" is present it is used as the number of alpha electrons
   !   in embedded scf calculations.
   ! . If "method" is present it is used as the kind of density matrix
   !   to make, otherwise taken from .SCF_data.SCF_kind, or guessed
   !   from the spinorbital_genre of the MO's.
   ! NOTE:
   ! . If .SCF_data.do_delta_build is set and a a .delta_density_mx
   !   is created, then a delta density is made.
   ! . If .SCF_data.apply_pFON a finite temperature density matrix is
   !   made with temperature .SCF_data.temperature_for_pFON (untested)
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), optional, IN :: method
      MAT(REAL,:,:), allocatable :: MO,MOa,MOb
      STR(len=STR_SIZE) :: scf_method,genre
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_E_SCF_density_mx ... no basis info")
   ENSURE(allocated(self%MOs),"MOLECULE.SCF:make_E_SCF_density_mx ... no molecular orbitals")
   ENSURE(has_any_genre_(self%MOs),"MOLECULE.SCF:make_E_SCF_density_mx ... no molecular orbitals")
   ENSURE(allocated(self%fock_mx),"MOLECULE.SCF:make_E_SCF_density_mx ... no fock matrix")
   ENSURE(has_any_genre_(self%fock_mx),"MOLECULE.SCF:make_E_SCF_density_mx ... no fock matrix")
   ENSURE(allocated(self%core_mx),"MOLECULE.SCF:make_E_SCF_density_mx ... no core matrix")

      ! Ensure we have this
   ! if (.core_mx.deallocated) then
   !    .make_core_mx
   ! end

      ! Determine (or guess) the kind of SCF method to be used.
      if (present(method)) then; scf_method = method
      else if (allocated(self%SCF_data)) then; scf_method = self%SCF_data%SCF_kind
      else; scf_method = guess_SCF_kind_(self%MOs)
      end if

      ! Determine (or guess) the genre of density matrix to be made
      if (allocated(self%SCF_data)) then; genre = spinorbital_genre_(self%SCF_data)
      else; genre = genre_(self%MOs)
      end if

      ! Make the Krogel matrix
      self%E_density_mx = self%fock_mx
      call plus_(self%E_density_mx,self%core_mx)
      call scale_by_(self%E_density_mx,HALF)

      ! Make the E natural orbitals
      call make_E_NOs_(self)

      ! Make the E density matrix
      ! Not complete
      select case (scf_method)

      case ("rhf","rks","xray_rks","xray_rhf","noninteracting-group-rhf")
         ENSURE(is_allocated_with_genre_(self%E_NOs,"r "),"MOLECULE.SCF:make_E_SCF_density_mx ... no energy NO's")
         ENSURE(self%spin_multiplicity==1,"MOLECULE.SCF:make_E_SCF_density_mx ... Not a singlet state: "//trim(chemical_formula_(self%atom))//", mult="//trim(to_str_(self%spin_multiplicity)))
         MO = self%E_NOs%r(:,1:self%n_a)
         call to_scaled_product_of_(self%E_density_mx%r,MO,MO,fac=TWO,transpose_b=TRUE)

      case ("rohf")
         ENSURE(is_allocated_with_genre_(self%MOs,"r "),"MOLECULE.SCF:make_E_SCF_density_mx ... no MO's")
         MOa = self%MOs%r(:,1:self%n_a)
         MOb = self%MOs%r(:,1:self%n_b)
         call to_product_of_(self%density_mx%a,MOa,MOa,transpose_b=TRUE)
         call to_product_of_(self%density_mx%b,MOb,MOb,transpose_b=TRUE)

      case ("uhf","uks","xray_uhf","xray_uks","pnd_uhf","pnd_uks","xray_pnd_uhf","xray_pnd_uks")
         ENSURE(is_allocated_with_genre_(self%MOs,"u "),"MOLECULE.SCF:make_E_SCF_density_mx ... no MO's")
         MOa = self%MOs%a(:,1:self%n_a)
         MOb = self%MOs%b(:,1:self%n_b)
         call to_product_of_(self%density_mx%a,MOa,MOa,transpose_b=TRUE)
         call to_product_of_(self%density_mx%b,MOb,MOb,transpose_b=TRUE)
      end select
   end subroutine


!  ===========
!  Core matrix
!  ===========

!  These could go in the ATOM module if the atomic_SCF and 
!  ANOs could be factored out.

   subroutine make_core_mx(self,genre)
   ! Get the core hamiltonian matrix.
   ! If possible the kind is made from the kind of SCF_data.
   ! If its already in memory, do nothing; otherwise make it.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), optional, IN :: genre
      STR(len=STR_SIZE) :: genus
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_core_mx ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_core_mx ... no atom list")

      ! Create core_mx if not there
      if (NOT allocated(self%core_mx)) then
         call create_(self%core_mx,self%n_bf)
      end if

      ! Make sure restricted part is there at least
      if (NOT has_genre_(self%core_mx,"r ")) then
         call make_r_core_mx_(self)
      end if
      genus = spinorbital_genre_(self%SCF_data)
      if (present(genre)) genus = genre
      if (has_genre_(self%core_mx,genus)) then
         return
      end if

      ! Make or convert it
      select case (genus)

      case ("gc")
         call make_gc_core_mx_(self)

      case ("r ")
         ! Already there

      case default
         call convert_to_(self%core_mx,genus,destroy_original=FALSE)
      end select
   end subroutine

   subroutine make_r_core_mx(self)
   ! Make the *restricted* core hamiltonian matrix appropriate for the
   ! current scf calculation. If already in memory, do nothing.
   ! If it isn't, burn everything and start again from scratch.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: Z
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_r_core_mx ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_r_core_mx ... no atom list")
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_r_core_mx ... no SCF_data")

      ! Is it already there?
      if (allocated(self%core_mx)) then
         if (has_genre_(self%core_mx,"r ")) then
            return
         end if
      end if

      ! Allocate core matrix
      call destroy_(self%core_mx)
      call create_(self%core_mx,self%n_bf)
      call create_(self%core_mx,"r ")
      DIE_IF(self%nucleus_model/="point" AND self%SCF_data%relativity_kind/="x2c","MOLECULE.SCF:make_r_core_mx ... nucleus_model= gaussian needs relativity_kind= x2c")

      ! Make the core matrix here (relativistic vs non-rel)
      select case (self%SCF_data%relativity_kind)
      case ("x2c"); call make_r_X2C_core_mx_(self,self%core_mx%r)
      case default; call make_r_NREL_core_mx_(self,self%core_mx%r)
      end select

      ! Finite-electric-field contribution
      if (NOT is_zero_(self%E_field)) call add_E_field_term_to_(self,self%core_mx%r)

      ! No cluster charges for SCF guess
      if (self%SCF_data%is_guess) return
      if (self%SCF_data%using_cluster_charges) then

         ! Add cluster charges if needed
         ! NOTE: includes the self-consistent case
         if (NOT self%partition_info_made) then
            ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_r_core_mx ... no crystal")
            call make_atom_partition_info_(self)
         end if
         call create_(Z,self%n_bf,self%n_bf)
         call make_cluster_charge_mx_(self,Z)  ! leaky
         self%core_mx%r = self%core_mx%r+Z
         call destroy_(Z)
      else if (self%SCF_data%using_SC_Lorentz_fields) then

         ! Add Lorentz fields if needed
         if (NOT self%partition_info_made) then
            ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_r_core_mx ... no crystal")
            call make_atom_partition_info_(self)  ! NOT PURE
         end if
         call create_(Z,self%n_bf,self%n_bf)
         call make_Lorentz_core_(self,Z)
         self%core_mx%r = self%core_mx%r+Z
         call destroy_(Z)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_r_NREL_core_mx(self,H)
   ! Make the non-relativistic core hamiltonian (T+NA) matrix "H".
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: H
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_r_NREL_core_mx ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_r_NREL_core_mx ... no atom list")

      ! Get kinetic matrix (leaky)
      call make_kinetic_energy_mx_(self)
      call uncompress_(self%kinetic_energy_mx)

      ! Get nuclear matrix (leaky)
      call make_nuclear_attraction_mx_(self)
      call uncompress_(self%nuclear_attraction_mx)

      ! Make core matrix
      H = self%kinetic_energy_mx%r+self%nuclear_attraction_mx%r

      ! Compress (leaky)
      call compress_(self%kinetic_energy_mx)
      call compress_(self%nuclear_attraction_mx)
   end subroutine

   NON_MPI_PURE subroutine add_E_field_term_to(self,H)
   ! Add to the core matrix "H" the energy of the electrons in the
   ! applied electric field.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: H
      MAT(REAL,:,:), allocatable :: Dx,Dy,Dz
   ENSURE(self%basis_info_made,"MOLECULE.SCF:add_E_field_term_to ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:add_E_field_term_to ... no atom list")
      call create_(Dx,self%n_bf,self%n_bf)
      call create_(Dy,self%n_bf,self%n_bf)
      call create_(Dz,self%n_bf,self%n_bf)
      call make_dipole_matrices_(self,Dx,Dy,Dz)
      H = H+Dx*self%E_field(1)+Dy*self%E_field(2)+Dz*self%E_field(3)
      call destroy_(Dz)
      call destroy_(Dy)
      call destroy_(Dx)
   end subroutine

   NON_MPI_PURE subroutine make_gc_core_mx(self)
   ! Make the general complex core hamiltonian matrix.
   ! If already in memory, do nothing.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_gc_core_mx ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_gc_core_mx ... no atom list")
   ENSURE(allocated(self%core_mx),"MOLECULE.SCF:make_gc_core_mx ... no core matrix")
   ENSURE(has_genre_(self%core_mx,"r "),"MOLECULE.SCF:make_gc_core_mx ... no restricted core matrix")

      ! Return if core matrix already there
      if (has_genre_(self%core_mx,"gc")) return

      ! Allocate core matrix
      call create_(self%core_mx,"gc")
      self%core_mx%gc = (ZERO,ZERO)

      ! Add the lower half in ... depending on the method
      ! NOTE: there may be some failures here ...
      select case (self%SCF_data%relativity_kind)
   ! case ("douglas-kroll-hess"); .add_gc_DKH_core_mx
   ! case ("dkh");                .add_gc_DKH_core_mx
      case ("zora"); call add_gc_ZORA_core_mx_(self)
   ! case ("iotc");               .set_gc_IOTC_core_mx
      case ("pauli"); call add_gc_Pauli_core_mx_(self)
      case ("none"); call add_gc_core_mx_(self)
      end select

      ! Make it Hermitian
      call make_hermitian_(self%core_mx%gc)
   end subroutine


!   add_gc_DKH_core_mx
!   ! Add the DKH core hamiltonian
!   ! NOTE: only lower half is made
!   ENSURE(.SCF_data.allocated,"no SCF_data")
!   ENSURE(.core_mx.is_allocated_with_genre("gc"),"no core matrix")
!
!      T,Lx,Ly,Lz :: MAT{REAL}@
!      fac :: REAL
!      I :: CPX
!
!      I = (ZERO,ONE)
!
!      T.create(.n_bf,.n_bf)
!      Lx.create(.n_bf,.n_bf)
!      Ly.create(.n_bf,.n_bf)
!      Lz.create(.n_bf,.n_bf)
!
!      .REL:make_DKH_matrices(T,Lx,Ly,Lz)
!
!      .core_mx.gc.aa_block_plus(T)
!      .core_mx.gc.bb_block_plus(T)
!
!      fac= ONE
!      .core_mx.gc.ba_block_plus(Lx,fac*I)
!      .core_mx.gc.ba_block_plus(Ly,-fac)
!      .core_mx.gc.aa_block_plus(Lz,fac*I)
!      .core_mx.gc.bb_block_plus(Lz,-fac*I)
!
!      T.destroy
!      Lz.destroy
!      Ly.destroy
!      Lx.destroy
!
!   end

   PURE subroutine add_gc_ZORA_core_mx(self)
   ! Add the core hamiltonain to a general complex "F"
   ! NOTE: only lower half is made
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: T,Lx,Ly,Lz
      REAL :: fac
      CPX :: I
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:add_gc_ZORA_core_mx ... no SCF_data")
   ENSURE(self%SCF_data%using_1e_zora_term,"MOLECULE.SCF:add_gc_ZORA_core_mx ...  no Pauli?")
   ENSURE(is_allocated_with_genre_(self%core_mx,"gc"),"MOLECULE.SCF:add_gc_ZORA_core_mx ... no core matrix")
      I = (ZERO,ONE)
      call create_(T,self%n_bf,self%n_bf)
      call create_(Lx,self%n_bf,self%n_bf)
      call create_(Ly,self%n_bf,self%n_bf)
      call create_(Lz,self%n_bf,self%n_bf)
      call make_1e_ZORA_matrices_(self,T,Lx,Ly,Lz)
      call aa_block_plus_(self%core_mx%gc,T)
      call bb_block_plus_(self%core_mx%gc,T)
      fac = G_FACTOR/TWO
      fac = fac*self%SCF_data%sl_1e_factor
      call ba_block_plus_(self%core_mx%gc,Lx,-fac*I)
      call ba_block_plus_(self%core_mx%gc,Ly,fac)
      call aa_block_plus_(self%core_mx%gc,Lz,-fac*I)
      call bb_block_plus_(self%core_mx%gc,Lz,fac*I)
      call destroy_(T)
      call destroy_(Lz)
      call destroy_(Ly)
      call destroy_(Lx)
   end subroutine


!   set_gc_IOTC_core_mx
!   ! Add the core hamiltonain to a general complex "F"
!   ! NOTE: only lower half is made
!   ENSURE(.SCF_data.allocated,"no SCF_data")
!   ENSURE(.core_mx.is_allocated_with_genre("gc"),"no core matrix")
!
!      .REL:make_gc_IOTC_core_mx(.core_mx.gc)
!
!   end

   NON_MPI_PURE subroutine add_gc_Pauli_core_mx(self)
   ! Add the Pauli core hamiltonian ... including B-field terms
   ! NOTE: only lower half is made
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: T,Lx,Ly,Lz
      MAT(REAL,:,:), allocatable :: xx,yy,zz,xy,xz,yz
      MAT(REAL,3,3) :: BB
      INT :: k,l
      REAL :: fac
      CPX :: I
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:add_gc_Pauli_core_mx ... no SCF_data")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.SCF:add_gc_Pauli_core_mx ... no overlap matrix")
   ENSURE(allocated(self%core_mx),"MOLECULE.SCF:add_gc_Pauli_core_mx ... no core matrix")
   ENSURE(has_genre_(self%core_mx,"r "),"MOLECULE.SCF:add_gc_Pauli_core_mx ... no R core matrix")
   ENSURE(has_genre_(self%core_mx,"gc"),"MOLECULE.SCF:add_gc_Pauli_core_mx ... no GC core matrix")
      I = (ZERO,ONE)
      call aa_block_plus_(self%core_mx%gc,self%core_mx%r)
      call bb_block_plus_(self%core_mx%gc,self%core_mx%r)

      ! The Pauli terms below are not normally active
      ! if Douglas-Kroll has been used ...
      if (self%SCF_data%using_1e_sl_term) then
         call create_(Lx,self%n_bf,self%n_bf)
         call create_(Ly,self%n_bf,self%n_bf)
         call create_(Lz,self%n_bf,self%n_bf)
         call make_spin_orbit_matrices_(self,Lx,Ly,Lz)
         fac = G_FACTOR/(EIGHT*SPEED_OF_LIGHT_AU**2)
         fac = fac*self%SCF_data%sl_1e_factor
         call ba_block_plus_(self%core_mx%gc,Lx,-fac*I)
         call ba_block_plus_(self%core_mx%gc,Ly,fac)
         call aa_block_plus_(self%core_mx%gc,Lz,-fac*I)
         call bb_block_plus_(self%core_mx%gc,Lz,fac*I)
         call destroy_(Lz)
         call destroy_(Ly)
         call destroy_(Lx)
      end if
      if (NOT is_zero_(self%B_field)) then
         if (self%SCF_data%using_bs_term) then
            fac = G_FACTOR/FOUR
            call aa_block_plus_(self%core_mx%gc,self%overlap_mx,fac*self%B_field(3))
            call bb_block_plus_(self%core_mx%gc,self%overlap_mx,-fac*self%B_field(3))
            call ba_block_plus_(self%core_mx%gc,self%overlap_mx,fac*self%B_field(1))
            call ba_block_plus_(self%core_mx%gc,self%overlap_mx,fac*self%B_field(2)*I)
         end if
         if (self%SCF_data%using_bs_t_term) then
            call create_(T,self%n_bf,self%n_bf)
            call make_kinetic_energy_mx_(self,T)
            fac = -G_FACTOR/(FOUR*SPEED_OF_LIGHT_AU**2)
            call aa_block_plus_(self%core_mx%gc,T,fac*self%B_field(3))
            call bb_block_plus_(self%core_mx%gc,T,-fac*self%B_field(3))
            call ba_block_plus_(self%core_mx%gc,T,fac*self%B_field(1))
            call ba_block_plus_(self%core_mx%gc,T,fac*self%B_field(2)*I)
            call destroy_(T)
         end if
         if (self%SCF_data%using_bl_term) then
            call create_(Lx,self%n_bf,self%n_bf)
            call create_(Ly,self%n_bf,self%n_bf)
            call create_(Lz,self%n_bf,self%n_bf)
            call make_L_matrices_(self,Lx,Ly,Lz)
            Lx = Lx*self%B_field(1)+Ly*self%B_field(2)+Lz*self%B_field(3)
            fac = HALF
            call aa_block_plus_(self%core_mx%gc,Lx,fac*I)
            call bb_block_plus_(self%core_mx%gc,Lx,fac*I)
            call destroy_(Lz)
            call destroy_(Ly)
            call destroy_(Lx)
         end if
         if (self%SCF_data%using_aa_term) then
            call create_(xx,self%n_bf,self%n_bf)
            call create_(yy,self%n_bf,self%n_bf)
            call create_(zz,self%n_bf,self%n_bf)
            call create_(xy,self%n_bf,self%n_bf)
            call create_(xz,self%n_bf,self%n_bf)
            call create_(yz,self%n_bf,self%n_bf)
            call make_quadrupole_matrices_(self,xx,yy,zz,xy,xz,yz)
            fac = ONE/EIGHT
            do k = 1,3
               BB(k,k) = dot_(self%B_field,self%B_field)-self%B_field(k)*self%B_field(k)
            end do
            do k = 1,3
               do l = k+1,3
                  BB(k,l) = -TWO*self%B_field(k)*self%B_field(l)
               end do
            end do
            BB = fac*BB
            xx = BB(1,1)*xx
            xx = xx+BB(2,2)*yy
            xx = xx+BB(3,3)*zz
            xx = xx+BB(1,2)*xy
            xx = xx+BB(1,3)*xz
            xx = xx+BB(2,3)*yz
            call aa_block_plus_(self%core_mx%gc,xx)
            call bb_block_plus_(self%core_mx%gc,xx)
            call destroy_(yz)
            call destroy_(xz)
            call destroy_(xy)
            call destroy_(zz)
            call destroy_(yy)
            call destroy_(xx)
         end if
         if (self%SCF_data%using_1e_srxa_term) then
            call create_(Lx,self%n_bf,self%n_bf)
            call create_(Ly,self%n_bf,self%n_bf)
            call create_(Lz,self%n_bf,self%n_bf)
            call make_spin_orbit_B_matrices_(self,Lx,Ly,Lz)
            fac = G_FACTOR/(TWO*EIGHT*(SPEED_OF_LIGHT_AU)**2)
            call ba_block_plus_(self%core_mx%gc,Lx,fac)
            call ba_block_plus_(self%core_mx%gc,Ly,fac*I)
            call aa_block_plus_(self%core_mx%gc,Lz,fac)
            call bb_block_plus_(self%core_mx%gc,Lz,-fac)
            call destroy_(Lz)
            call destroy_(Ly)
            call destroy_(Lx)
         end if
      end if
   end subroutine

   PURE subroutine add_gc_core_mx(self)
   ! Add the Pauli core hamiltonian ... including B-field terms
   ! NOTE: only lower half is made
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:add_gc_core_mx ... no SCF_data")
   ENSURE(allocated(self%core_mx),"MOLECULE.SCF:add_gc_core_mx ... no core matrix")
   ENSURE(has_genre_(self%core_mx,"r "),"MOLECULE.SCF:add_gc_core_mx ... no R core matrix")
   ENSURE(has_genre_(self%core_mx,"gc"),"MOLECULE.SCF:add_gc_core_mx ... no GC core matrix")

      ! Define the gc parts
      call aa_block_plus_(self%core_mx%gc,self%core_mx%r)
      call bb_block_plus_(self%core_mx%gc,self%core_mx%r)
   end subroutine

   subroutine put_cluster_charges(self)
   ! Make the point charge matrix "Z" from the point charges in .atom
   ! and the positions derived from ".SCF_data.cluster".
   ! WARNING: the crystal asym unit charges must be defined.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name

      ! Make it
      call make_cluster_charges_(self)

      ! Write
      name = trim(self%name)//".gaussian-point-charges"
      call put_cluster_charges_(self%SCF_data,name)
   end subroutine

   subroutine make_cluster_charges(self)
   ! Make the cluster charges and their positions, and save them in
   ! ".SCF_data". The atomic charges and moments they come from are made
   ! afresh from the density matrix, by the X-ray partition model; a
   ! model that is not one-centre uses Hirshfeld atoms.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: pos
      VEC(REAL,:), allocatable :: charge
      INT :: n_charges
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_cluster_charges ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_cluster_charges ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_cluster_charges ... no crystal data")
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_cluster_charges ... no SCF_data")

      ! Make the atomic charges and moments afresh, by the partition model
      self%partition_info_made = FALSE
      call make_atom_partition_info_(self,need_moments=TRUE)

      ! Set up the cluster ... leaky
      call destroy_(self%SCF_data%cluster)
      call create_(self%SCF_data%cluster)
      call set_defaults_(self%SCF_data%cluster,self%crystal)

      ! Make the cluster
      call set_generation_method_(self%SCF_data%cluster,"within_radius")
      if (self%SCF_data%defragment) then
         call set_defragment_(self%SCF_data%cluster,TRUE)
      else
         call set_defragment_(self%SCF_data%cluster,FALSE)
      end if
      call set_radius_(self%SCF_data%cluster,self%SCF_data%cluster_radius)
      call update_info_(self%SCF_data%cluster)  ! NOT PURE

      ! Make the cluster charges
      n_charges = self%SCF_data%cluster%n_atoms-self%SCF_data%cluster%n_fragment_atoms
      if (NOT self%SCF_data%using_qq_cluster_charges) then
         n_charges = 3*n_charges
         call create_(charge,n_charges)
         call create_(pos,3,n_charges)
         call make_non_fragment_mu_charges_(self%SCF_data%cluster,pos,charge)
      else
         n_charges = 9*n_charges
         call create_(charge,n_charges)
         call create_(pos,3,n_charges)
         call make_non_fragment_qq_charges_(self%SCF_data%cluster,pos,charge)
      end if

      ! Charges that are all zero mean the atomic moments were never made
      if (n_charges>0) then
         DIE_IF(is_zero_(charge),"MOLECULE.SCF:make_cluster_charges ... every cluster charge is zero: the Hirshfeld atom moments were not made")
      end if

      ! Save the information
      self%SCF_data%cluster_charges = charge
      self%SCF_data%cluster_charge_positions = pos
   end subroutine

   subroutine make_cluster_charge_mx(self,Z)
   ! Make the point charge matrix "Z" from the point charges in .atom
   ! and the positions derived from ".SCF_data.cluster".
   ! WARNING: the crystal asym unit charges must be defined.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: Z
      VEC(REAL,:), allocatable :: charge
      MAT(REAL,:,:), allocatable :: Z_c,pos
      INT :: n_charges,q,c
      INT :: fa,la,na
      INT :: fb,lb,nb
      REAL :: q_c
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_cluster_charge_mx ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_cluster_charge_mx ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_cluster_charge_mx ... no crystal data")
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_cluster_charge_mx ... no SCF_data")
   ENSURE(self%partition_info_made,"MOLECULE.SCF:make_cluster_charge_mx ... no Hirshfeld atom moments")

      ! Set up the cluster ... leaky
      call destroy_(self%SCF_data%cluster)
      call create_(self%SCF_data%cluster)
      call set_defaults_(self%SCF_data%cluster,self%crystal)

      ! Make the cluster
      call set_generation_method_(self%SCF_data%cluster,"within_radius")
      if (self%SCF_data%defragment) then
         call set_defragment_(self%SCF_data%cluster,TRUE)
      else
         call set_defragment_(self%SCF_data%cluster,FALSE)
      end if
      call set_radius_(self%SCF_data%cluster,self%SCF_data%cluster_radius)
      call update_info_(self%SCF_data%cluster)  ! leaky

      ! Make the cluster charges
      n_charges = self%SCF_data%cluster%n_atoms-self%SCF_data%cluster%n_fragment_atoms
      if (NOT self%SCF_data%using_qq_cluster_charges) then
         n_charges = 3*n_charges
         call create_(charge,n_charges)
         call create_(pos,3,n_charges)
         call make_non_fragment_mu_charges_(self%SCF_data%cluster,pos,charge)
      else
         n_charges = 9*n_charges
         call create_(charge,n_charges)
         call create_(pos,3,n_charges)
         call make_non_fragment_qq_charges_(self%SCF_data%cluster,pos,charge)
      end if

      ! Charges that are all zero mean the atomic moments were never made
      if (n_charges>0) then
         DIE_IF(is_zero_(charge),"MOLECULE.SCF:make_cluster_charge_mx ... every cluster charge is zero: the Hirshfeld atom moments were not made")
      end if

      ! Make the charge contribution to one electron Hamiltonian
      Z = ZERO
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.SCF:make_cluster_charge_mx")
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call create_(Z_c,na,nb)
         do c = 1,size(charge)
            q_c = charge(c)
            if (equals_(q_c,ZERO)) cycle
            call get_nuc_(sh,Z_c,ZERO,pos(:,c))
            Z(fa:la,fb:lb) = Z(fa:la,fb:lb)-q_c*Z_c
         end do
         call destroy_(Z_c)
         call destroy_ptr_part_(sh)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.SCF:make_cluster_charge_mx")
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(Z)
      else
         call symmetric_reflect_(Z)
      end if
      if (self%SCF_data%save_cluster_charges) then
         if (self%SCF_data%save_20_cluster_charges) then

            ! For test jobs only, reduces output
            self%SCF_data%cluster_charges = charge(1:20)
            self%SCF_data%cluster_charge_positions = pos(:,1:20)
         else
            self%SCF_data%cluster_charges = charge
            self%SCF_data%cluster_charge_positions = pos
         end if
      else
         call destroy_(pos)
         call destroy_(charge)
      end if
   end subroutine

   subroutine make_Lorentz_core(self,Z)
   ! Add the local electric fields "F" to the core matrix, made from
   ! the Lorentz factor tensors
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: Z
      VEC(REAL,:), allocatable :: rF,pF,ww
      VEC(REAL,:), allocatable :: ra,ga,gb
      VEC(REAL,:), allocatable :: wt,xa,ya,za,a2
      VEC(INT,:), allocatable :: near_atom,pi,pj
      VEC(BIN,:), allocatable :: skipa0,skipa
      VEC(BIN,:), allocatable :: skipab,skipb
      MAT(REAL,:,:), allocatable :: F,pt,bf_save
      MAT(REAL,:,:), allocatable :: ZZ
      VEC(type(EVEC_BIN_TYPE),:), allocatable :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: bf_grid
      REAL :: cutoff,fac,val
      INT :: n_pt,n_keep
      INT :: c,ca,cb
      INT :: oa,fsa,lsa,fba
      INT :: ob,fsb,lsb,as
      INT :: sa,fa,la,na
      INT :: sb,fb,lb,nb
      INT :: a,b,i,j,n,p
      type(SHELL1_TYPE) :: sha
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_Lorentz_core ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_Lorentz_core ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_Lorentz_core ... no crystal")
   ENSURE(self%partition_info_made,"MOLECULE.SCF:make_Lorentz_core ... no Hirshfeld atom moments")

      ! Make the local Lorentz fields at every atom
      call create_(F,3,self%n_atom)
      call make_Lorentz_fields_(self,F)

      ! Basis function cutoff
      cutoff = self%becke_grid%basis_fn_cutoff

      ! Max no of becke grid points per atom
      n_pt = max_no_of_pts_per_atom_(self%becke_grid)

      ! Atom "a" displaced points and skip switches
      call create_(xa,n_pt); call create_(ya,n_pt); call create_(za,n_pt)
      call create_(a2,n_pt)
      call create_(skipa0,n_pt)
      call create_(skipab,n_pt)

      ! Initialise
      Z = ZERO

      ! Loop over integration atoms "c"
      do c = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.SCF:make_Lorentz_core")

         ! Get the Becke grid for atom "c"
         call make_atom_grid_(self%becke_grid,pt,wt,bf_save,c)
         n_pt = size(pt,1)

         ! Overlapping atoms
         near_atom = self%overlapping_atoms_for_atom(c)%element

         ! Make stockholder weight function "Wc" for atom "c"
         ! If using interpolatators, then on overlapping atoms
         ! are used ... otherwise nothing is skipped
         call apply_stockholder_atom_weight_(self,wt,c,pt)

         ! Prune the grid with new "wt", get saved bf grid bf_save,
         ! and new no of points, n_pt -- leaky
         call prune_grid_(self%becke_grid,pt,wt,bf_save)
         n_pt = size(pt,1)

         ! r.F for each grid point
         call create_(rF,n_pt)
         rF = pt(:,1)*F(1,c)+pt(:,2)*F(2,c)+pt(:,3)*F(3,c)

         ! Basis function grid and skip list
         call create_(bf_skip,self%n_shell)
         call create_(bf_grid,self%n_shell)

         ! Loop c-overlapping atoms "oa"
         do ca = 1,size(near_atom)
            oa = near_atom(ca)
            fsa = self%first_shell_for_atom(oa)
            lsa = self%last_shell_for_atom(oa)
            fba = self%first_basis_fn_for_atom(oa)
            ra = self%atom(oa)%position

            ! Make the displaced points for atom "oa"
            call make_displaced_skip_pts_(self%atom(oa),xa,ya,za,a2,skipa0,pt,n_pt,cutoff)

            ! Loop shells on atom a
            do sa = fsa,lsa
               fa = self%first_basis_fn_for_shell(sa)
               la = self%last_basis_fn_for_shell(sa)
               na = la-fa+1
               as = self%basis_shell_for_shell(sa)

               ! Set shell "sa"
               sha = self%basis_shell1pair(as,1)%a
               sha%position = ra

               ! Make shell "sa" skipa list
               call create_(skipa,n_pt)
               call make_skip_list_(sha,skipa,n_keep,a2,skipa0,n_pt,cutoff)

               ! Next shell "sa" if no sig. points
               if (n_keep==0) then
                  call destroy_(skipa)
                  cycle
               end if

               ! Assign the "sa" skipa list
               bf_skip(sa)%element = skipa

               ! Make/copy the "sa" basis function grid
               ! This must be copied since skipa could be TRUE
               call create_(bf_grid(sa)%element,n_keep,na)
               if (oa==c) then
                  do a = 1,na
                     i = 0
                     do n = 1,n_pt
                        if (skipa(n)) cycle
                        i = i+1
                        bf_grid(sa)%element(i,a) = bf_save(n,a+fa-fba)
                     end do
                  end do
               else
                  call make_skip_grid_(sha,bf_grid(sa)%element,n_keep,xa,ya,za,a2,skipa,n_pt)
               end if

               ! Loop c-overlapping atoms b
               do cb = 1,ca
                  ob = near_atom(cb)
                  fsb = self%first_shell_for_atom(ob)
                  lsb = self%last_shell_for_atom(ob)

                  ! Loop shells on atom b
                  do sb = fsb,min(lsb,sa)
                     if (NOT allocated(bf_skip(sb)%element)) cycle

                     ! Shell b limits
                     fb = self%first_basis_fn_for_shell(sb)
                     lb = self%last_basis_fn_for_shell(sb)
                     nb = lb-fb+1

                     ! Set the "sb" basis function grid
                     skipb = bf_skip(sb)%element

                     ! How many points in common?
                     skipab(1:n_pt) = skipa OR skipb
                     n_keep = count(NOT skipab(1:n_pt))
                     if (n_keep==0) cycle

                     ! Density block
                     ZZ = Z(fa:la,fb:lb)
                     if (na*nb==1) then

                        ! Add density contribution to rho
                        ga = bf_grid(sa)%element(:,1)
                        gb = bf_grid(sb)%element(:,1)
                        i = 0; j = 0
                        val = ZERO
                        do n = 1,n_pt
                           if (NOT skipa(n)) i = i+1
                           if (NOT skipb(n)) j = j+1
                           if (skipab(n)) cycle
                           fac = ga(i)*gb(j)*wt(n)
                           val = val+fac*rF(n)
                        end do
                        ZZ(1,1) = ZZ(1,1)+val
                     else

                        ! Create space for gathered index info
                        call create_(pi,n_keep); call create_(pj,n_keep); call create_(ww,n_keep)
                        call create_(pF,n_keep)

                        ! Make mapper arrays
                        p = 0; i = 0; j = 0
                        do n = 1,n_pt
                           if (NOT skipa(n)) i = i+1
                           if (NOT skipb(n)) j = j+1
                           if (skipab(n)) cycle
                           p = p+1
                           pi(p) = i
                           pj(p) = j
                           ww(p) = wt(n)
                           pF(p) = rF(n)
                        end do

                        ! Add density contribution to rho
                        do a = 1,na
                           ga = bf_grid(sa)%element(:,a)
                           do b = 1,nb
                              gb = bf_grid(sb)%element(:,b)
                              val = ZERO
                              do p = 1,n_keep
                                 i = pi(p)
                                 j = pj(p)
                                 fac = ga(i)*gb(j)*ww(p)
                                 val = val+fac*pF(p)
                              end do
                              ZZ(a,b) = ZZ(a,b)+val
                           end do
                        end do

                        ! Clean up
                        call destroy_(pF)
                        call destroy_(ww)
                        call destroy_(pj)
                        call destroy_(pi)
                     end if
                  end do  ! -- loops sa,sb
               end do
            end do  ! -- lops ca,cb
         end do

         ! Clean up
         call destroy_(bf_save)
         call destroy_(bf_grid)
         call destroy_(bf_skip)
         call destroy_(rF)
         call destroy_(wt)
         call destroy_(pt)
      end do  ! -- integration atom c
      UNLOCK_PARALLEL_DO("MOLECULE.SCF:make_Lorentz_core")

      ! Clean up
      call destroy_(skipab)
      call destroy_(skipa0)
      call destroy_(a2)
      call destroy_(za)
      call destroy_(ya)
      call destroy_(xa)
      call destroy_(F)

      ! Parallel
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(Z)
      else
         call symmetric_reflect_(Z)
      end if
   end subroutine

   subroutine make_Lorentz_fields(self,F)
   ! Make the local Lorentz electric fields "F" at each atom site due
   ! to the crystal environment calculated from the charges and
   ! dipoles on each atom (so the Hirshfeld or other charges and
   ! dipoles need to be made). Only monopole and dipole contributions
   ! are made.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: F
      type(CLUSTER_TYPE), allocatable :: cluster
      MAT(REAL,:,:), allocatable :: xyz
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_Lorentz_fields ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_Lorentz_fields ... no crystal")
   ENSURE(self%partition_info_made,"MOLECULE.SCF:make_Lorentz_fields ... no atomic moments")
   ENSURE(size(F,1)==3,"MOLECULE.SCF:make_Lorentz_fields ... wromg dim1, F")
   ENSURE(size(F,2)==self%n_atom,"MOLECULE.SCF:make_Lorentz_fields ... wromg dim1, F")

      ! Set up the cluster ... leaky
      call create_(cluster)
      call set_defaults_(cluster,self%crystal)
      xyz = coordinates_(self%atom)
      call set_fragment_geometry_(cluster,xyz,cartesian=TRUE)
      call destroy_(xyz)
      call set_generation_method_(cluster,"within_radius")
      call set_defragment_(cluster,FALSE)
      call set_radius_(cluster,ZERO)
      call update_info_(cluster)  ! NOT PURE - annoying !

      ! Make the local Lorentz fields at every atom
      call make_Lorentz_fields_(cluster,F)
      call destroy_(cluster)
   end subroutine

   subroutine make_Lorentz_interactions(self,I0,I1)
   ! Make the local Lorentz interactions terms with charges and
   ! dipoles due to dipoles ONLY on the atoms in the crystal
   ! environment (so the Hirshfeld or other charges and dipoles need
   ! to be made).  A minus sign is neede when combined.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: I0
      MAT(REAL,:,:), OUT :: I1
      type(CLUSTER_TYPE), allocatable :: cluster
      MAT(REAL,:,:), allocatable :: xyz
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_Lorentz_interactions ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_Lorentz_interactions ... no crystal")
   ENSURE(self%partition_info_made,"MOLECULE.SCF:make_Lorentz_interactions ... no atomic moments")
   ENSURE(size(I1,1)==3,"MOLECULE.SCF:make_Lorentz_interactions ... wrong dim1, I2")
   ENSURE(size(I1,2)==self%n_atom,"MOLECULE.SCF:make_Lorentz_interactions ... wrong dim2, I2")
   ENSURE(size(I0)==self%n_atom,"MOLECULE.SCF:make_Lorentz_interactions ... wrong dim, I0")

      ! Set up the cluster ... leaky
      call create_(cluster)
      call set_defaults_(cluster,self%crystal)
      xyz = coordinates_(self%atom)
      call set_fragment_geometry_(cluster,xyz,cartesian=TRUE)
      call destroy_(xyz)
      call set_generation_method_(cluster,"within_radius")
      call set_defragment_(cluster,FALSE)
      call set_radius_(cluster,ZERO)
      call update_info_(cluster)  ! NOT PURE - crazy!

      ! Make the local Lorentz interactions with charges
      ! and dipoles at every atom
      call make_Lorentz_interactions_(cluster,I0,I1)
      call destroy_(cluster)
   end subroutine


!  ====================
!  Fock matrix routines
!  ====================

   subroutine make_scf_Fock_mx(self,core,r12)
   ! If r12  is present and FALSE, the r12 part is not computed.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: core,r12
      type(OPMATRIX_TYPE), allocatable :: dF
      BIN :: delta_build,add_core
      REAL :: t0,t1
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_scf_Fock_mx ... no SCF_data")
   ENSURE(allocated(self%density_mx),"MOLECULE.SCF:make_scf_Fock_mx ... no density matrix")
   ENSURE(has_genre_(self%density_mx,spinorbital_genre_(self%SCF_data)),"MOLECULE.SCF:make_scf_Fock_mx ... incompatible density")
      t0 = current_cpu_time()

      ! Use a delta fock matrix build?
      delta_build = do_delta_build_(self%SCF_data) AND allocated(self%delta_density_mx) AND allocated(self%fock_2e_mx)
      if (delta_build) delta_build = has_genre_(self%delta_density_mx,spinorbital_genre_(self%SCF_data))
      if (NOT delta_build) then

         ! Make the 2e fock matrix from density
         call destroy_(self%fock_2e_mx)
         call create_(self%fock_2e_mx,self%n_bf)
         call make_Fock_mx_(self,self%fock_2e_mx,self%density_mx,core=FALSE,r12=r12,xc=FALSE)
      else

         ! Make the 2e fock matrix from delta density
         call uncompress_(self%fock_2e_mx)
         call create_(dF,self%n_bf)
         call make_Fock_mx_(self,dF,self%delta_density_mx,core=FALSE,r12=r12,xc=FALSE)
         call plus_(self%fock_2e_mx,dF)  ! Add Delta F
         call destroy_(dF)
      end if

      ! Set fock matrix & save space
      call destroy_(self%fock_mx)
      self%fock_mx = self%fock_2e_mx
      call compress_(self%fock_2e_mx)
      t1 = current_cpu_time(); self%SCF_data%time_fock_2e = self%SCF_data%time_fock_2e+t1-t0

      ! Now do the XC part if needed
      if (is_DFT_calculation_(self%SCF_data)) then
         call add_XC_mx_(self,self%fock_mx)
      end if
      t0 = current_cpu_time(); self%SCF_data%time_xc = self%SCF_data%time_xc+t0-t1

      ! Force update of 1e hamiltonian if required
      if (self%SCF_data%using_SC_cluster_charges OR self%SCF_data%using_SC_Lorentz_fields) then
         call destroy_(self%core_mx)
         self%partition_info_made = FALSE  ! <=== LOOK
      end if

      ! Add one electron part?
      add_core = TRUE
      if (present(core)) add_core = core

      ! Add core. Make sure it is there.
      if (add_core) then
         call make_core_mx_(self,spinorbital_genre_(self%SCF_data))  ! NOT PURE
         call make_core_mx_(self,MOs_genre_(self%SCF_data))
         call plus_(self%fock_mx,self%core_mx)
      end if

      ! An HF average of configuration: the effective open-shell Fock matrix
      if (add_core AND self%SCF_data%using_open_shell_ensemble) then
         if (NOT is_DFT_calculation_(self%SCF_data)) then
            call make_AOC_effective_Fock_mx_(self)
         end if
      end if
   end subroutine


!  ===
!  SCF
!  ===

   subroutine SCF(self)
   ! Do an SCF calculation. The following :
   !   .MOs, .MO_energies, .density_mx
   ! are produced as results.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:SCF ... no SCF_data provided")
      select case (self%SCF_data%SCF_kind)

      case ("group       ")
         call atom_group_SCF_(self)

      case ("fragment-rhf","fragment-rks")
         call fragment_SCF_(self)  ! How different to above?

      case ("xray_rhf    ","xray_rks    ","xray_uhf    ","xray_uks    ","xray_gchf   ","pnd_uhf     ","pnd_uks     ","xray_pnd_uhf","xray_pnd_uks")
         call constrained_SCF_(self)

      case default
         call usual_SCF_(self)
      end select
   end subroutine

   subroutine promolecule_SCF(self)
   ! Do an initial promolecule SCF.
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: use_cc,use_sc

      ! Save guess
      use_cc = self%SCF_data%using_cluster_charges
      use_sc = self%SCF_data%using_SC_cluster_charges

      ! Do promolecule restart; set .SCF_data.output=YES to see ...
      call set_initial_density_(self%SCF_data,"promolecule")
      call set_using_cluster_charges_(self%SCF_data,FALSE)

      !!!!!!!!
      call SCF_(self)
      !!!!!!!!

      ! Set guess back
      call set_initial_density_(self%SCF_data,spinorbital_genre_(self%SCF_data))
      call set_using_cluster_charges_(self%SCF_data,use_cc)
      call set_using_SC_cluster_charges_(self%SCF_data,use_sc)
   end subroutine

   subroutine stop_if_SCF_not_converged(self)
   ! An SCF that ended unconverged has not produced an answer. Say what
   ! state it stopped in and stop, unless .SCF_data.die_if_not_converged is
   ! FALSE, in which case say that the job goes on with the unconverged
   ! orbitals. Called after the results are printed and archived, so the
   ! table can be read and a restart made.
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:stop_if_SCF_not_converged ... no SCF_data")
      if (self%SCF_data%converged) return
      call flush_(stdout)
      call text_(stdout,"The SCF did not converge:")
      call flush_(stdout)
      call show_(stdout,"No. of iterations           =",self%SCF_data%iteration)
      call show_(stdout,"Maximum allowed             =",self%SCF_data%max_iterations)
      call show_(stdout,"Energy                      =",self%SCF_data%energy)
      call show_(stdout,"Last energy change          =",self%SCF_data%delta)
      call show_(stdout,"Convergence tolerance       =",self%SCF_data%convergence)
      call show_(stdout,"DIIS error                  =",self%SCF_data%DIIS_error)
      call flush_(stdout)
      if (self%SCF_data%die_if_not_converged) then
         call text_(stdout,"Raise max_iterations=, or set die_if_not_converged= FALSE in scfdata=")
         call text_(stdout,"to go on with the unconverged orbitals.")
         call flush_(stdout)
         DIE("MOLECULE.SCF:stop_if_SCF_not_converged ... SCF did not converge")
      end if
      call text_(stdout,"WARNING: going on with the unconverged orbitals, as die_if_not_converged= FALSE asks.")
      call flush_(stdout)
   end subroutine


!  =========
!  Usual SCF
!  =========

   subroutine usual_SCF(self)
   ! Do an SCF calculation. The .MOs,
   ! .MO_energies, and .density_mx are produced as results.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(self%basis_info_made,"MOLECULE.SCF:usual_SCF ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:usual_SCF ... no atom list")
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:usual_SCF ... no SCF_data")
      call initialize_SCF_(self)
      call put_SCF_banner_and_options_(self)
      call put_SCF_table_header_(self)
      do
         call do_timed_SCF_iteration_(self)
         if (SCF_done_(self%SCF_data)) exit
      end do
      call set_converged_(self%SCF_data,is_converged_(self%SCF_data))
      if (self%SCF_data%using_COSX) call make_COSX_final_energy_(self)
      call canonicalize_virtual_MOs_(self,output=TRUE)
      call put_SCF_results_(self)
      call archive_SCF_results_(self)
      call stop_if_SCF_not_converged_(self)
      call cleanup_scf_(self)

      ! Recalculate charges to be sure
      call set_partition_info_made_(self,FALSE)
      call set_u_HA_moments_made_(self,FALSE)
      call set_atomic_polarisabilities_made_(self,FALSE)
   end subroutine

   subroutine make_COSX_final_energy(self)
   ! Remake the Fock matrix and the energies of the converged density
   ! on .SCF_data.COSX_final_grid. The orbitals stay those of the
   ! coarser SCF grid: the energy is stationary, so what that costs is
   ! second order in their error.
      type(MOLECULE_TYPE), INOUT :: self
      if (NOT allocated(self%COSX_pts)) return
      if (NOT self%SCF_data%using_COSX_final_grid) return
      call initialize_COSX_(self,self%SCF_data%COSX_final_grid)
      call make_scf_Fock_mx_(self)
      call update_SCF_data_energies_(self)
   end subroutine

   subroutine atomic_SCF(self,converged)
   ! Do an atomic SCF calculation; this is for initial guesses.
   ! The molecule comprises a single atom. The .MOs,
   ! .MO_energies, and .density_mx are produced as results.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, OUT :: converged
   ENSURE(allocated(self%atom),"MOLECULE.SCF:atomic_SCF ... no atom list")
   ENSURE(size(self%atom)==1,"MOLECULE.SCF:atomic_SCF ... atom list length > 1")
   ENSURE(self%basis_info_made,"MOLECULE.SCF:atomic_SCF ... no basis info")
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:atomic_SCF ... no SCF_data")
      call initialize_atomic_SCF_(self)
      call put_SCF_banner_and_options_(self)
      call put_SCF_table_header_(self)
      do
         call do_timed_SCF_iteration_(self)
         if (SCF_done_(self%SCF_data)) exit
      end do
      call set_converged_(self%SCF_data,is_converged_(self%SCF_data))
      if (present(converged)) converged = self%SCF_data%converged
      call put_SCF_results_(self)
      call archive_SCF_results_(self)
      call cleanup_scf_(self)
   end subroutine

   subroutine do_timed_SCF_iteration(self)
   ! One SCF iteration, with the CPU time of each phase accumulated in
   ! .SCF_data and the whole iteration's time put in the table.
      type(MOLECULE_TYPE), INOUT :: self
      REAL :: t0,t1
      t0 = current_cpu_time()
      call extrapolate_Fock_mx_(self)
      t1 = current_cpu_time(); self%SCF_data%time_diis = self%SCF_data%time_diis+t1-t0
      call update_MOs_(self)
      t0 = current_cpu_time(); self%SCF_data%time_diag = self%SCF_data%time_diag+t0-t1
      call make_SCF_density_mx_(self,damp=apply_damping_(self%SCF_data))
      t1 = current_cpu_time(); self%SCF_data%time_density = self%SCF_data%time_density+t1-t0
      call set_ERI_accuracy_for_iteration_(self%SCF_data)
      call make_scf_Fock_mx_(self)  ! times its own two parts
      t0 = current_cpu_time()
      call update_SCF_data_(self)
      call update_SCF_data_error_(self)
      self%SCF_data%time_iteration = t0-self%SCF_data%time_iteration
      call put_SCF_table_body_(self)
      self%SCF_data%time_iteration = t0
   end subroutine

   subroutine initialize_SCF(self)
   ! Initialize any molecular SCF procedure
      type(MOLECULE_TYPE), INOUT :: self
      REAL :: t0,t1
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:initialize_SCF ... no SCF_data")
      self%SCF_data%time_1e = ZERO
      self%SCF_data%time_grid = ZERO
      self%SCF_data%time_guess = ZERO
      self%SCF_data%time_fock_2e = ZERO
      self%SCF_data%time_xc = ZERO
      self%SCF_data%time_diis = ZERO
      self%SCF_data%time_diag = ZERO
      self%SCF_data%time_density = ZERO

      ! Overlap matrix
      t0 = current_cpu_time()
      call make_overlap_mx_(self)
      t1 = current_cpu_time(); self%SCF_data%time_1e = self%SCF_data%time_1e+t1-t0

      ! Density fitting of J: before the guess, which may build a Fock matrix
      if (self%SCF_data%using_RI_J) then
         DIE_IF(self%auxiliary_basis_name==" ","MOLECULE.SCF:initialize_SCF ... use_RI_J= needs auxiliary_basis_name=")
         call initialize_RI_J_(self)
         t1 = current_cpu_time(); self%SCF_data%time_1e = self%SCF_data%time_1e+t1-t0
      end if

      ! Integration grids; COSX has its own
      if (is_DFT_calculation_(self%SCF_data)) call initialize_DFT_grids_(self)
      if (self%SCF_data%using_COSX) then
         call make_COSX_grids_(self%SCF_data)
         call initialize_COSX_(self,self%SCF_data%COSX_grid)
      end if
      t0 = current_cpu_time(); self%SCF_data%time_grid = self%SCF_data%time_grid+t0-t1

      ! Get initial guess
      call get_initial_guess_(self)
      t1 = current_cpu_time(); self%SCF_data%time_guess = self%SCF_data%time_guess+t1-t0

      ! Core matrix: both kinds
      call destroy_(self%core_mx)
      call make_core_mx_(self,spinorbital_genre_(self%SCF_data))
      call make_core_mx_(self,MOs_genre_(self%SCF_data))
      t0 = current_cpu_time(); self%SCF_data%time_1e = self%SCF_data%time_1e+t0-t1

      ! Atomic densities, to prune the DFT grid by the promolecule density.
      ! After the guess: a promolecule guess has made them already, with
      ! its output, and a second call would return early and print nothing.
      if (is_DFT_calculation_(self%SCF_data)) then
         if (self%becke_grid%prune_rho_cutoff>ZERO) then
            call make_ANOs_and_interpolators_(self,output=FALSE)
         end if
      end if
      t1 = current_cpu_time(); self%SCF_data%time_grid = self%SCF_data%time_grid+t1-t0

      ! Get Fock matrix
      call make_scf_Fock_mx_(self)

      ! Grid size, for the timing summary
      if (allocated(self%XC_wts)) then
         self%SCF_data%XC_grid_points = size(self%XC_wts)
         self%SCF_data%XC_grid_points_unpruned = self%XC_n_unpruned
      end if

      ! Arm the incremental Fock build: make_SCF_density_mx forms the delta
      ! density only when this exists, and make_scf_Fock_mx then updates
      ! .fock_2e_mx from it. Nothing allocated it before 2026-09-12, so the
      ! default use_delta_build= TRUE did nothing for years.
      call destroy_(self%delta_density_mx)
      if (self%SCF_data%using_delta_build) call create_(self%delta_density_mx,self%n_bf)

      ! The table column is the time between successive rows
      self%SCF_data%time_iteration = current_cpu_time()

      ! Make structure factors?
      ! Read lambda=0 structure factors
      if (is_constrained_SCF_(self%SCF_data)) then
         call make_constraint_data_(self)
         call set_XCW_(self%crystal%xray_data,TRUE)  ! add one to n_param
         call make_constraint_data_(self)
      end if

      ! Reset all SCF data (except DFT parts)
      call reset_(self%SCF_data)
      if (is_constrained_SCF_(self%SCF_data)) call set_penalty_(self%SCF_data,self%crystal)
      call update_SCF_data_energies_(self)

      ! Add constraint to F?
      if (is_constrained_SCF_(self%SCF_data)) call add_constraint_to_Fock_mx_(self)
      if (is_constrained_SCF_(self%SCF_data) AND self%SCF_data%use_stiffness_correction) then
         if (allocated(self%MOs) AND allocated(self%MO_energies)) then
            call correct_constraint_overshoot_(self)
         end if
      end if

      ! Initialise DIIS error
      if (self%SCF_data%using_DIIS) call update_SCF_data_error_(self)

      ! Initialise Schwarz inequality integrals (leaky)
      call initialize_max_I_(self)

      ! Initialise SCF table (leaky)
      call set_table_(self%SCF_data)

      ! ERI cutoffs for the first iteration, before the options are printed
      call set_ERI_accuracy_for_iteration_(self%SCF_data)
   end subroutine

   subroutine initialize_atomic_SCF(self)
   ! Initialize an atomic SCF procedure
   ! The guess must be core. There is not fitting.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:initialize_atomic_SCF ... no SCF_data")
      self%SCF_data%time_1e = ZERO
      self%SCF_data%time_grid = ZERO
      self%SCF_data%time_guess = ZERO
      self%SCF_data%time_fock_2e = ZERO
      self%SCF_data%time_xc = ZERO
      self%SCF_data%time_diis = ZERO
      self%SCF_data%time_diag = ZERO
      self%SCF_data%time_density = ZERO
      ENSURE(NOT is_constrained_SCF_(self%SCF_data),"MOLECULE.SCF:initialize_atomic_SCF ... fitting not allowed")

      ! Overlap matrix
      call make_overlap_mx_(self)

      ! Integration grids
      if (is_DFT_calculation_(self%SCF_data)) call initialize_DFT_grids_(self)

      ! Get core guess -- different
      call set_is_guess_(self%SCF_data,TRUE)
      call make_core_mx_guess_MOs_(self)

      ! Core matrix: both kinds
      call destroy_(self%core_mx)
      call make_core_mx_(self,spinorbital_genre_(self%SCF_data))
      call make_core_mx_(self,MOs_genre_(self%SCF_data))

      ! Get Fock matrix
      call make_scf_Fock_mx_(self)

      ! Timers: the table column is the time between successive rows
      self%SCF_data%time_iteration = current_cpu_time()

      ! Reset all SCF data (except DFT parts)
      call reset_(self%SCF_data)
      call update_SCF_data_energies_(self)

      ! Initialise DIIS error
      call update_SCF_data_error_(self)

      ! Initialise Schwarz inequality integrals (leaky)
      call initialize_max_I_(self)

      ! Initialise SCF table (leaky)
      call set_table_(self%SCF_data)
   end subroutine

   subroutine update_SCF_data_energies(self)
   ! Update SCF energies
      type(MOLECULE_TYPE), INOUT :: self
      REAL :: e
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:update_SCF_data_energies ... no SCF_data")

      ! Energies
      self%SCF_data%nuclear_repulsion_energy = nuclear_repulsion_energy_(self%atom)
      if (self%SCF_data%relativity_kind/="x2c") self%SCF_data%nuclear_attraction_energy = nuclear_attraction_energy_(self)
      if (self%SCF_data%using_cluster_charges) self%SCF_data%charge_attraction_energy = charge_attraction_energy_(self)
      self%SCF_data%kinetic_energy = kinetic_energy_(self)

      ! Under X2C only the sum of the kinetic and nuclear attraction
      ! energies is defined; it is kept in the kinetic energy
      if (self%SCF_data%relativity_kind=="x2c") then
         self%SCF_data%nuclear_attraction_energy = ZERO
         self%SCF_data%kinetic_energy = expectation_(self%density_mx,self%core_mx%r)-self%SCF_data%charge_attraction_energy
      end if

      ! Total SCF energy [including V(nuc) repulsion] and penalty
      e = SCF_energy_(self)

      ! Update energy & penalty
      call update_energy_(self%SCF_data,e,self%crystal)

      ! Set electron repulsion energy by subtraction
      self%SCF_data%electron_repulsion_energy = self%SCF_data%energy-self%SCF_data%nuclear_repulsion_energy-self%SCF_data%nuclear_attraction_energy-self%SCF_data%charge_attraction_energy-self%SCF_data%kinetic_energy
   end subroutine

   subroutine update_SCF_data(self)
   ! Update the scf iteration count, scf energies used to
   ! test for convergence in the SCF procedure.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:update_SCF_data ... no SCF_data")

      ! New iteration
      call update_iteration_(self%SCF_data)

      ! Update energies and penalty
      call update_SCF_data_energies_(self)
   end subroutine

   subroutine update_SCF_data_error(self)
   ! Update the SCF_data gradient of the SCF energy with respect to
   ! orbital rotations, otherwise known as the "DIIS error". This is
   ! used to check for SCF convergence.
      type(MOLECULE_TYPE), INOUT :: self
      REAL :: DIIS_error
      call make_DIIS_error_length_(self,DIIS_error)
   end subroutine

   subroutine put_SCF_banner_and_options(self)
   ! Put out the SCF banner and options
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:put_SCF_banner_and_options ... no SCF_data")
   ENSURE(self%SCF_data%iteration==0,"MOLECULE.SCF:put_SCF_banner_and_options ... iteration is not 0!")
   ENSURE(self%SCF_data%lambda_iteration==0,"MOLECULE.SCF:put_SCF_banner_and_options ... lambda_iteration is not 0!")

      ! SCF banner and options
      call put_banner_(self%SCF_data)
      call put_options_(self%SCF_data,self%crystal,self%nucleus_model)

      ! Diffraction data-treatment options
      if (self%SCF_data%output) then
         if (is_constrained_SCF_(self%SCF_data)) then
            if (allocated(self%crystal%xray_data) AND includes_(self%SCF_data%SCF_kind,"xray")) then
               call flush_(stdout)
               call text_(stdout,"X-ray data-treatment options:")
               call put_correction_data_(self%crystal)
            end if

      ! if (.crystal.pnd_data.allocated AND .SCF_data.SCF_kind.includes("pnd")) then
      !    .crystal.assign_pnd
      !    stdout.flush
      !    stdout.text("PND data-treatment options:")
      !    .crystal.put_correction_data
      ! end
         end if
      end if

      ! Initial guess energies
      call put_initial_guess_energies_(self%SCF_data)
   end subroutine

   subroutine put_SCF_table_header_only(self)
   ! Put out the SCF iteration table header
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:put_SCF_table_header_only ... no SCF_data")

      ! Table head & n-th iteration results
      call put_table_header_(self%SCF_data)
   end subroutine

   subroutine put_SCF_table_header(self)
   ! Put out the SCF iteration table header
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:put_SCF_table_header ... no SCF_data")

      ! Table head & 0-th iteration results
      call put_table_head_(self%SCF_data)
      call put_table_body_and_footer_(self%SCF_data,self%crystal)
   end subroutine

   subroutine put_SCF_table_body(self,flush)
   ! Put out the SCF iteration results, body and footer
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: flush
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:put_SCF_table_body ... no SCF_data")
      call put_table_body_and_footer_(self%SCF_data,self%crystal,flush)
   end subroutine

   subroutine put_SCF_results(self)
   ! Put out the SCF results
      type(MOLECULE_TYPE), IN :: self
      STR(len=STR_SIZE) :: name
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:put_SCF_results ... no SCF_data")
      call put_results_(self%SCF_data,self%crystal)
      if (self%SCF_data%save_cluster_charges) then
         name = trim(self%name)//".gaussian-point-charges"
         call put_cluster_charges_(self%SCF_data,name)
      end if
   end subroutine

   subroutine archive_SCF_results(self)
   ! Save the SCF results in various archives on disk.
   ! Including any foreign-format files like .wfx.
      type(MOLECULE_TYPE), IN :: self
      STR(len=STR_SIZE) :: name
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:archive_SCF_results ... no SCF_data")
      if (self%SCF_data%delete_all_archives) return
      call put_archive_(self,self%MOs,"MOs",with_lambda=TRUE)
      call put_archive_(self,self%MO_energies,"MO_energies",with_lambda=TRUE)
      call put_archive_(self,self%density_mx,"density_mx",with_lambda=TRUE)

      ! Put out cluster charges
      if (allocated(self%SCF_data%cluster_charges)) then
         name = trim(self%name)//".gaussian-point-charges"
         call put_cluster_charges_(self%SCF_data,name)
      end if

      ! Aimall wfx file, modified by Florian?
      if (is_constrained_SCF_(self%SCF_data)) then
         call put_florian_wfn_file_(self,with_lambda=TRUE)
      end if
   end subroutine

   subroutine cleanup_scf(self)
   ! Clean up the SCF files produced.
   ! Keep only MOs and density matrix
      type(MOLECULE_TYPE), INOUT :: self
      call destroy_tmp_SCF_matrices_(self)
      if (allocated(self%SCF_data)) then
         call reset_iteration_defaults_(self%SCF_data%SCF_DIIS)
         if (self%SCF_data%delete_SCF_archives) call delete_SCF_archives_(self,keep_MOs=TRUE,keep_DM=TRUE)
      end if

      ! Clean
      call destroy_(self%SCF_data%table_info)
      call destroy_(self%SCF_data%table)
      call destroy_(self%max_I)

      ! Disarm the incremental build: a Fock build after the SCF (properties,
      ! a second SCF) must start from the full density
      call destroy_(self%delta_density_mx)
   end subroutine


!  ===============
!  Constrained SCF
!  ===============

   subroutine constrained_SCF(self)
   ! Do a constrained SCF calculation.
   ! The following objects are produced as results:
   !   .MOs, .MO_energies, .density_mx
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(self%basis_info_made,"MOLECULE.SCF:constrained_SCF ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:constrained_SCF ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:constrained_SCF ... no crystal information")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.SCF:constrained_SCF ... no diffraction data")
      call initialize_SCF_(self)
      call put_SCF_banner_and_options_(self)
      call put_SCF_table_header_(self)
      do  ! lambda loop
         do  ! scf loop
            call extrapolate_Fock_mx_(self)
            call update_MOs_(self)
            call make_SCF_density_mx_(self,damp=apply_damping_(self%SCF_data))
            call make_scf_Fock_mx_(self)
            call make_constraint_data_(self)
            call update_SCF_data_(self)
            call add_constraint_to_Fock_mx_(self)
            if (self%SCF_data%use_stiffness_correction) call correct_constraint_overshoot_(self)
            call update_SCF_data_error_(self)
            if (self%SCF_data%put_constraint_stiffness AND SCF_done_(self%SCF_data)) then
               self%SCF_data%stiffness_physical_gaps = TRUE
               call make_constraint_stiffness_(self)
               self%SCF_data%stiffness_physical_gaps = FALSE
               call record_lambda_point_(self%SCF_data,F_GoF2_(self%crystal%xray_data%reflections))
            end if
         ! .put_SCF_table_body(flush=TRUE)
            call put_table_body_and_footer_(self%SCF_data,self%crystal,flush=TRUE)
            if (SCF_done_(self%SCF_data)) exit
         end do
         call set_converged_(self%SCF_data,is_converged_(self%SCF_data))
         call archive_SCF_results_(self)

         ! The stiffness report at this lambda
         if (self%SCF_data%put_constraint_stiffness) call put_constraint_stiffness_(self)

         ! Calculate the residuals ...
         ! Before exiting lambda loop
         if (self%crystal%xray_data%do_residual_cube) then
            call get_minmax_residual_density_(self,output=FALSE)
            call put_CIFs_(self)
            call put_tonto_fcf_XCW_(self)
            call put_olex_fcf_XCW_(self)
            call put_xd_fco_XCW_(self)
         end if
         call update_lambda_(self%SCF_data)
         if (exceeded_lambda_max_(self%SCF_data)) exit
         call reset_for_next_lambda_(self)
         call update_SCF_data_error_(self)
         call put_SCF_table_header_only_(self)
      end do
      call put_SCF_results_(self)
      if (self%SCF_data%put_constraint_stiffness AND self%SCF_data%stiffness_made) call put_reflection_influence_(self)
      call stop_if_SCF_not_converged_(self)
      call cleanup_scf_(self)

      ! Recalculate charges to be sure
      call set_partition_info_made_(self,FALSE)
      call set_u_HA_moments_made_(self,FALSE)
      call set_atomic_polarisabilities_made_(self,FALSE)

      ! Put data analysis & cube output
      if (self%crystal%xray_data%do_standard_plots) then
         call put_X_data_analysis_(self%crystal,output=TRUE,job=self%name)
      end if
   end subroutine

   subroutine make_constraint_data(self)
   ! Make the predicted constraint data, e.g. X-ray structure factors
   ! or PND structure factors, or whatever data are to be used in the
   ! constrained wavefunction procedure
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: SCF_kind
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_constraint_data ... no SCF_data")
      SCF_kind = self%SCF_data%SCF_kind
      select case (SCF_kind)

      case ("xray_rhf","xray_rks","xray_uhf","xray_uks","xray_gchf")
         call make_X_SFs_(self)
   !    if (.crystal.xray_r_free_data.allocated) .make_X_free_SFs

   ! case ("pnd_uhf","pnd_uks");
   !    .make_N_SFs
   !    if (.crystal.pnd_r_free_data.allocated)  .make_N_free_SFs

   ! case ("xray_pnd_uhf","xray_pnd_uks");
   !    if (.crystal.uses_oc_atom_partition) .make_atom_partition_info
   !    .make_X_SFs
   !    .make_N_SFs
   !    if (.crystal.xray_r_free_data.allocated) .make_X_free_SFs
   !    if (.crystal.pnd_r_free_data.allocated)  .make_N_free_SFs

      case default
         DIE("MOLECULE.SCF:make_constraint_data ... unknown wavefunction fitting kind, "//trim(SCF_kind))
      end select
   end subroutine

   subroutine add_constraint_to_Fock_mx(self)
   ! Make the constraint matrix for wavefunction fitting and add it to the fock
   ! matrix to obtain the effective fock matrix.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), allocatable :: C
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:add_constraint_to_Fock_mx ... no scf data")
   ENSURE(allocated(self%fock_mx),"MOLECULE.SCF:add_constraint_to_Fock_mx ... no fock matrix")
   ENSURE(has_any_genre_(self%fock_mx),"MOLECULE.SCF:add_constraint_to_Fock_mx ... no fock matrix")
      call create_(C,self%n_bf,spinorbital_genre_(self%SCF_data))
      if (is_zero_(self%SCF_data%lambda)) then

         ! Save some work if lamda is zero
         call set_to_zero_(C)
      else

         ! Make the constraint term
         call make_X_constraint_(self,C)
         call plus_scaled_(self%fock_mx,C,self%SCF_data%lambda)
      end if

      ! Clean up
      call destroy_(C)
   end subroutine

   subroutine make_X_constraint(self,C)
   ! Make constraint matrix for restricted or unrestricted SCF cases
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: C

   ! tmp :: OPMATRIX@
      MAT(REAL,:,:), allocatable :: CR
   ENSURE(any(spinorbital_genre_(self%SCF_data)==["r","u"]),"MOLECULE.SCF:make_X_constraint ... only rhf/uhf!")
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_X_constraint ... no crystal info")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.SCF:make_X_constraint ... no reflection data")
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_X_constraint ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_X_constraint ... no atom list")
      select case (self%SCF_data%SCF_kind)

      case ("xray_rhf","xray_rks")
         if (uses_Hirshfeld_atoms_(self%crystal)) then
            call make_H_r_constraint_(self,C%r)
         else
            call make_r_constraint_(self,C%r)
         end if

      ! .project_out_small_evals(C.r)

      case ("xray_uhf","xray_uks")
         if (uses_Hirshfeld_atoms_(self%crystal)) then
            call make_H_r_constraint_(self,C%a)
         else
            call make_r_constraint_(self,C%a)
         end if
         C%b = C%a

      case ("xray_gchf")
         if (uses_Hirshfeld_atoms_(self%crystal)) then
            DIE("MOLECULE.SCF:make_X_constraint ... hirshi not yet available for xray_gchf")
            ! .make_H_r_constraint(C.a)
         else
            C%gc = (ZERO,ZERO)
            call create_(CR,2*self%n_bf,2*self%n_bf)
            call make_gc_constraint_(self,CR)
         end if
         C%gc = CR
         call destroy_(CR)
      end select
   end subroutine

   subroutine project_out_small_evals(self,C)
   ! Project out small eigenvectors from the constraint matrix "C".
   ! smallest to largest.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: C
      MAT(REAL,:,:), allocatable :: P
      MAT(REAL,:,:), allocatable :: evec
      VEC(REAL,:), allocatable :: eval
      INT :: n,i
      REAL :: tol,shift
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:project_out_small_evals ... no SCF_data")

      ! The eigenvalues of the LSQ constraint matrix
      call create_(eval,self%n_bf)
      call create_(evec,self%n_bf,self%n_bf)
      call solve_symmetric_eigenproblem_(C,eval,evec)

      ! Return if all abs(evals) are big enough
      eval = abs(eval)
      tol = 0.1d0
      n = index_of_first_greater_than_(eval,tol)
      if (n>0) then

         ! Set the projector
         shift = self%SCF_data%linear_dependence_shift
         call create_(P,self%n_bf,self%n_bf)
         call to_unit_mx_(P)
         do i = 1,self%n_bf
            if (eval(i)<tol) then
               call plus_outer_product_of_(P,evec(:,i),shift)
            end if
         end do

         ! Change basis of C to remove
         call change_basis_using_(C,P)
         call destroy_(P)
      end if

      ! Clean up
      call destroy_(eval)
      call destroy_(evec)
   end subroutine

   subroutine make_r_constraint(self,C,resid)
   ! Make the constraint matrix "C" for restricted wavefunction fitting.
   ! If "resid" is present it is used as the standardised residuals
   ! (F_pred - F_exp)/sigma in place of those of the current density.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: C
      VEC(REAL,:), optional, IN :: resid
      MAT(REAL,:,:), allocatable :: k_pts
      MAT3(CPX,:,:,:), allocatable :: ft_ab
      MAT3(CPX,:,:,:), allocatable :: ft_ab_eq
      VEC(CPX,:), allocatable :: Fc
      VEC(REAL,:), allocatable :: fac_times_alpha
      VEC(REAL,:), allocatable :: Fm,Fexp,Fsig
      VEC(REAL,:), allocatable :: alpha,d_alpha,Fcr,Fci
      REAL :: fac,cutoff
      INT :: q,fa,la,fb,lb,na,nb
      INT :: i,j,atom_a,atom_b
      INT :: n_k,n_refl,n_par
      type(SHELL2_TYPE) :: sh
      n_k = n_unique_SF_k_pts_(self%crystal)
      n_refl = size(self%crystal%xray_data%reflections)
      n_par = self%crystal%xray_data%n_param
      fac = TWO/max(n_refl-n_par,1)
      call create_(fac_times_alpha,n_refl)
      call create_(Fcr,n_refl)
      call create_(Fci,n_refl)
      Fc = self%crystal%xray_data%reflections%F_calc
      alpha = F_scale_and_extn_correction_(self%crystal%xray_data)
      d_alpha = d_F_pred_d_F_abs_(self%crystal%xray_data)
      Fexp = self%crystal%xray_data%reflections%F_exp
      Fsig = self%crystal%xray_data%reflections%F_sigma
      Fm = max(abs(Fc),TOL(10))

      ! NOTE: "alpha" makes F_pred, "d_alpha" differentiates it. They differ
      ! only when extinction is on, since the extinction factor depends on
      ! |F_calc| and so on the density matrix.
      if (present(resid)) then
         fac_times_alpha = fac*d_alpha*resid/(Fsig*Fm)
      else
         fac_times_alpha = fac*d_alpha*(alpha*Fm-Fexp)/(Fsig*Fsig*Fm)
      end if
      call destroy_(Fm)
      call destroy_(Fsig)
      call destroy_(Fexp)
      call destroy_(d_alpha)
      call destroy_(alpha)
      Fcr = fac_times_alpha*RE(Fc)
      Fci = fac_times_alpha*IM(Fc)
      call destroy_(fac_times_alpha)
      call destroy_(Fc)
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)
      cutoff = TOL(10)/self%n_shell_pairs
      C = ZERO
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.SCF:make_r_constraint")
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (skip_ft_(sh,cutoff)) then
            call destroy_ptr_part_(sh)
            cycle
         end if
         call create_(ft_ab,[1,n_refl],[fa,la],[fb,lb])
         call create_(ft_ab_eq,n_k,na,nb)
         call make_FT_pair_(self,ft_ab_eq,k_pts,sh,atom_a,atom_b)
         call sum_unique_sf_ints_(self%crystal%spacegroup,ft_ab,ft_ab_eq,self%crystal%xray_data%reflections)
         call destroy_(ft_ab_eq)
         do i = fa,la
            do j = fb,min(lb,i)
               C(i,j) = sum_elements_(RE(ft_ab(:,i,j))*Fcr+IM(ft_ab(:,i,j))*Fci)
            end do
         end do
         call destroy_(ft_ab)
         call destroy_ptr_part_(sh)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.SCF:make_r_constraint")

      ! Symmetrise
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(C)
      else
         call symmetric_reflect_(C)
      end if

      ! Get rid of OO block if requested
      if (self%SCF_data%zero_oo_constraint_block) then
         call change_basis_using_(C,self%MOs%r)
         C(1:self%n_a,1:self%n_a) = ZERO
         call back_transform_using_(C,self%MOs%r)
      end if

      ! Clean up
      call destroy_(k_pts)
      call destroy_(Fci)
      call destroy_(Fcr)
   end subroutine

   subroutine make_constraint_stiffness(self)
   ! Make, for an X-ray constrained restricted wavefunction at the
   ! current lambda, the gain matrix G of the plain SCF step in the
   ! space of reflections, and the fit statistics that follow from it.
   ! The constraint adds lambda times d(GoF^2)/dD to the Fock matrix. A
   ! change in the density changes each |F_calc|, hence that term, hence
   ! the next density: G is that loop for lambda = 1, in units of each
   ! reflection's sigma, with the orbital response uncoupled (the
   ! two-electron response is left out) and the direction of the refitted
   ! scale factor removed. The hat matrix H = lambda G (1 + lambda G)^-1
   ! says how far each prediction follows its own observation; its trace,
   ! plus the scale and extinction parameters, is the effective number of
   ! parameters, and its diagonal gives the leave-one-out residuals
   ! r/(1-h) without refitting. Everything is stored in .SCF_data. Only
   ! for the two-centre partition models.
   ! Theory: docs/REPORT_ON_XCW_CONVERGENCE.md.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: k_pts
      MAT3(CPX,:,:,:), allocatable :: ft_ab,ft_ab_eq
      MAT3(REAL,:,:,:), allocatable :: dF
      MAT(REAL,:,:), allocatable :: dF_ao,half,dF_ov,B_mx,gain
      VEC(CPX,:), allocatable :: Fc
      VEC(REAL,:), allocatable :: Fm,Fexp,Fsig,alpha,d_alpha,wt,share,proj
      REAL :: fac,cutoff,lam,val,p_eff,chi2
      INT :: q,fa,la,fb,lb,na,nb,atom_a,atom_b
      INT :: i,j,k,ia,n_k,n_refl,n_par,n_occ,n_vir
      type(SHELL2_TYPE) :: sh
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_constraint_stiffness ... no SCF_data")
   ENSURE(spinorbital_genre_(self%SCF_data)=="r","MOLECULE.SCF:make_constraint_stiffness ... only for restricted wavefunctions")
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_constraint_stiffness ... no crystal info")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.SCF:make_constraint_stiffness ... no reflection data")
   ENSURE(allocated(self%MOs),"MOLECULE.SCF:make_constraint_stiffness ... no molecular orbitals")
   ENSURE(allocated(self%MO_energies),"MOLECULE.SCF:make_constraint_stiffness ... no orbital energies")
      if (stiffness_is_matrix_free_(self)) then
         call make_constraint_stiffness_estimates_(self)
         return
      end if
      n_k = n_unique_SF_k_pts_(self%crystal)
      n_refl = size(self%crystal%xray_data%reflections)
      n_par = self%crystal%xray_data%n_param
      n_occ = self%n_a
      n_vir = self%n_bf-self%n_a
      fac = TWO/max(n_refl-n_par,1)

      ! The weight of each reflection: scale over sigma
      Fc = self%crystal%xray_data%reflections%F_calc
      alpha = F_scale_and_extn_correction_(self%crystal%xray_data)
      d_alpha = d_F_pred_d_F_abs_(self%crystal%xray_data)
      Fexp = self%crystal%xray_data%reflections%F_exp
      Fsig = self%crystal%xray_data%reflections%F_sigma
      call create_(Fm,n_refl)
      call create_(wt,n_refl)
      Fm = max(abs(Fc),TOL(10))
      wt = sqrt(abs(alpha*d_alpha))/Fsig

      ! The derivative of each |F_calc| with the density matrix
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)
      cutoff = TOL(10)/self%n_shell_pairs
      call create_(dF,n_refl,self%n_bf,self%n_bf)
      dF = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (skip_ft_(sh,cutoff)) then
            call destroy_ptr_part_(sh)
            cycle
         end if
         call create_(ft_ab,[1,n_refl],[fa,la],[fb,lb])
         call create_(ft_ab_eq,n_k,na,nb)
         call make_FT_pair_(self,ft_ab_eq,k_pts,sh,atom_a,atom_b)
         call sum_unique_sf_ints_(self%crystal%spacegroup,ft_ab,ft_ab_eq,self%crystal%xray_data%reflections)
         call destroy_(ft_ab_eq)
         do i = fa,la
            do j = fb,min(lb,i)
               dF(:,i,j) = (RE(ft_ab(:,i,j))*RE(Fc)+IM(ft_ab(:,i,j))*IM(Fc))/Fm
               dF(:,j,i) = dF(:,i,j)
            end do
         end do
         call destroy_(ft_ab)
         call destroy_ptr_part_(sh)
      end do
      call destroy_(k_pts)

      ! Its occupied-virtual block, weighted, over the root of the gap
      call create_(dF_ao,self%n_bf,self%n_bf)
      call create_(half,self%n_bf,n_vir)
      call create_(dF_ov,n_occ,n_vir)
      call create_(B_mx,n_refl,n_occ*n_vir)
      do k = 1,n_refl
         dF_ao = dF(k,:,:)
         call to_product_of_(half,dF_ao,self%MOs%r(:,n_occ+1:self%n_bf))
         call to_product_of_(dF_ov,self%MOs%r(:,1:n_occ),half,transpose_a=TRUE)
         ia = 0
         do j = 1,n_vir
            do i = 1,n_occ
               ia = ia+1
               val = orbital_gap_(self,i,n_occ+j)
               B_mx(k,ia) = TWO*wt(k)*dF_ov(i,j)/sqrt(val)
            end do
         end do
      end do
      call destroy_(dF_ov)
      call destroy_(half)
      call destroy_(dF_ao)
      call destroy_(dF)

      ! The scale factor is refitted at every step, so a change in the
      ! structure factors along F_calc itself costs nothing: remove it
      call create_(share,n_refl)
      call create_(proj,n_occ*n_vir)
      share = wt*Fm
      share = share/sqrt(sum(share*share))
      call to_product_of_(proj,B_mx,share,transpose_a=TRUE)
      do k = 1,n_refl
         B_mx(k,:) = B_mx(k,:)-share(k)*proj
      end do
      call destroy_(proj)
      call destroy_(share)

      ! The gain matrix for lambda = 1, its eigenvalues, its diagonal
      call create_(gain,n_refl,n_refl)
      call to_product_of_(gain,B_mx,B_mx,transpose_b=TRUE)
      gain = fac*gain
      call destroy_(self%SCF_data%stiffness_B)
      self%SCF_data%stiffness_B = B_mx
      call destroy_(B_mx)
      call destroy_(self%SCF_data%stiffness_eval)
      call destroy_(self%SCF_data%stiffness_evec)
      call destroy_(self%SCF_data%stiffness_diag)
      call destroy_(self%SCF_data%hat_diag)
      call destroy_(self%SCF_data%fit_residual)
      call create_(self%SCF_data%stiffness_eval,n_refl)
      call create_(self%SCF_data%stiffness_evec,n_refl,n_refl)
      call create_(self%SCF_data%stiffness_diag,n_refl)
      call create_(self%SCF_data%hat_diag,n_refl)
      call create_(self%SCF_data%fit_residual,n_refl)
      do k = 1,n_refl
         self%SCF_data%stiffness_diag(k) = gain(k,k)
      end do
      call solve_symmetric_eigenproblem_(gain,self%SCF_data%stiffness_eval,self%SCF_data%stiffness_evec)
      call destroy_(gain)
      self%SCF_data%gamma_max = self%SCF_data%stiffness_eval(n_refl)

      ! The hat matrix diagonal, the effective number of parameters, the
      ! residuals, and the criteria for choosing lambda
      lam = self%SCF_data%lambda
      self%SCF_data%fit_residual = (alpha*Fm-Fexp)/Fsig
      self%SCF_data%hat_diag = ZERO
      p_eff = n_par
      do j = 1,n_refl
         val = lam*self%SCF_data%stiffness_eval(j)/(ONE+lam*self%SCF_data%stiffness_eval(j))
         p_eff = p_eff+val
         self%SCF_data%hat_diag = self%SCF_data%hat_diag+val*self%SCF_data%stiffness_evec(:,j)*self%SCF_data%stiffness_evec(:,j)
      end do
      chi2 = sum(self%SCF_data%fit_residual*self%SCF_data%fit_residual)
      self%SCF_data%p_eff = p_eff
      self%SCF_data%chi2_fit = chi2
      self%SCF_data%AIC_sigma = n_refl*log(chi2/n_refl)+TWO*p_eff
      self%SCF_data%GCV = n_refl*chi2/(n_refl-p_eff)**2
      self%SCF_data%LOO_sum = sum((self%SCF_data%fit_residual/(ONE-self%SCF_data%hat_diag))**2)
      self%SCF_data%stiffness_made = TRUE
      self%SCF_data%stiffness_lambda_iteration = self%SCF_data%lambda_iteration

      ! Clean up
      call destroy_(wt)
      call destroy_(Fm)
      call destroy_(Fsig)
      call destroy_(Fexp)
      call destroy_(d_alpha)
      call destroy_(alpha)
      call destroy_(Fc)
   end subroutine

   pure function orbital_gap(self,i,a) result (res)
   ! The orbital energy difference between virtual orbital "a" and
   ! occupied orbital "i", restricted case. While the level shift is
   ! applied the virtual energies carry it; the correction wants them so,
   ! since that is the operator it corrects, and the statistics at
   ! convergence want the physical gap, which .SCF_data.stiffness_physical_gaps asks for.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: i,a
      REAL :: res
      res = self%MO_energies%r(a)-self%MO_energies%r(i)
      if (self%SCF_data%stiffness_physical_gaps AND apply_level_shifting_(self%SCF_data)) then
         res = res-self%SCF_data%level_shift
      end if
   end function

   pure function stiffness_is_matrix_free(self) result (res)
   ! TRUE if the stiffness is to be made without the derivative array:
   ! when asked for, and always for the Hirshfeld-atom partition models,
   ! which have no explicit derivative route.
      type(MOLECULE_TYPE), IN :: self
      BIN :: res
      res = self%SCF_data%use_matrix_free_stiffness
      if (allocated(self%crystal)) res = res OR uses_Hirshfeld_atoms_(self%crystal)
   end function

   subroutine make_Hirshfeld_FFs_of(self,ff,dens)
   ! Return "ff", the static Hirshfeld atom form factors of the density
   ! described by the AO matrix "dens", which need not be the SCF
   ! density, for the symmetry related k points and every unique
   ! fragment atom; for partition_model= oc-ri from the fit. The same
   ! loop as make_Hirshfeld_atom_FFs, without rebuilding the density.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable, OUT :: ff
      MAT(REAL,:,:), IN :: dens
      VEC(REAL,:), allocatable :: rho
      MAT(REAL,:,:), allocatable :: pt,k_pts
      REAL :: s2
      INT :: n_atom,n_k,u,c
      n_atom = size(self%crystal%asymmetric_unit_atom)
      n_k = n_unique_SF_k_pts_(self%crystal)
      call create_(ff,n_k,n_atom)
      ff = ZERO
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)
      do u = PARALLEL_DO_START(1,1),self%crystal%n_unique_frag_atoms,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.SCF:make_Hirshfeld_FFs_of")
         c = self%crystal%unique_frag_atom(u)
         s2 = self%crystal%frag_atom_sym_factor(c)
         if (is_zero_(s2)) cycle
         s2 = ONE/s2
         call make_Hirshfeld_atom_ED_grid_(self,rho,pt,c,dens)
         call make_atom_FFs_on_grid_(self,ff(:,u),k_pts,pt,rho,c)
         ff(:,u) = ff(:,u)*s2*self%atom(c)%site_occupancy
         call destroy_(rho)
         call destroy_(pt)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.SCF:make_Hirshfeld_FFs_of")
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(ff)
      end if
      call destroy_(k_pts)
   end subroutine

   subroutine make_B_times(self,v,res)
   ! Return "res" = B v for an occupied-virtual vector "v" (virtual index
   ! outer, occupied inner), with the B of make_constraint_stiffness, but
   ! without forming B: "res" is the set of structure factors of the
   ! symmetrised transition density c_occ V c_vir^T, V = v/sqrt(gap),
   ! projected on the phase of F_calc and scaled by 2 sqrt(alpha d_alpha)
   ! over sigma, with the direction of the refitted scale factor removed.
   ! One structure factor evaluation.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: v
      VEC(REAL,:), OUT :: res
      MAT(REAL,:,:), allocatable :: k_pts,vmat,half,pmat
      MAT(CPX,:,:), allocatable :: ff
      VEC(CPX,:), allocatable :: sf_eq,FP,F0,Fc
      VEC(REAL,:), allocatable :: Fm,Fsig,alpha,d_alpha,wt,u
      INT :: i,j,ia,n_k,n_refl,n_occ,n_vir
      n_k = n_unique_SF_k_pts_(self%crystal)
      n_refl = size(self%crystal%xray_data%reflections)
      n_occ = self%n_a
      n_vir = self%n_bf-self%n_a

      ! The symmetrised transition density
      call create_(vmat,n_occ,n_vir)
      ia = 0
      do j = 1,n_vir
         do i = 1,n_occ
            ia = ia+1
            vmat(i,j) = v(ia)/sqrt(orbital_gap_(self,i,n_occ+j))
         end do
      end do
      call create_(half,self%n_bf,n_vir)
      call to_product_of_(half,self%MOs%r(:,1:n_occ),vmat)
      call create_(pmat,self%n_bf,self%n_bf)
      call to_product_of_(pmat,half,self%MOs%r(:,n_occ+1:self%n_bf),transpose_b=TRUE)
      call symmetrize_(pmat)
      call destroy_(half)
      call destroy_(vmat)

      ! Its structure factors: through the Hirshfeld atoms, less the
      ! part that does not depend on the density (dispersion), or by
      ! the two-centre transform
      call create_(FP,n_refl)
      if (uses_Hirshfeld_atoms_(self%crystal)) then

         ! CRYSTAL:make_F_calc also stores what it makes as F_calc, so the
         ! F_calc of the SCF density is kept and put back
         Fc = self%crystal%xray_data%reflections%F_calc
         call make_Hirshfeld_FFs_of_(self,ff,pmat)
         call make_F_calc_(self%crystal,FP,ff)
         if (correct_dispersion_(self%crystal%xray_data)) then
            call create_(F0,n_refl)
            ff = ZERO
            call make_F_calc_(self%crystal,F0,ff)
            FP = FP-F0
            call destroy_(F0)
         end if
         call set_F_calc_(self%crystal%xray_data%reflections,Fc)
         call destroy_(Fc)
         call destroy_(ff)
      else
         call create_(k_pts,n_k,3)
         call make_unique_X_SF_k_pts_(self%crystal,k_pts)
         call create_(sf_eq,n_k)
         call make_FT_(self,sf_eq,pmat,k_pts)
         call sum_unique_sf_(self%crystal%spacegroup,FP,sf_eq,self%crystal%xray_data%reflections)
         call destroy_(sf_eq)
         call destroy_(k_pts)
      end if
      call destroy_(pmat)

      ! Projected on the phase of F_calc, weighted, scale direction removed
      Fc = self%crystal%xray_data%reflections%F_calc
      alpha = F_scale_and_extn_correction_(self%crystal%xray_data)
      d_alpha = d_F_pred_d_F_abs_(self%crystal%xray_data)
      Fsig = self%crystal%xray_data%reflections%F_sigma
      call create_(Fm,n_refl)
      call create_(wt,n_refl)
      call create_(u,n_refl)
      Fm = max(abs(Fc),TOL(10))
      wt = sqrt(abs(alpha*d_alpha))/Fsig
      res = TWO*wt*(RE(Fc)*RE(FP)+IM(Fc)*IM(FP))/Fm
      u = wt*Fm
      u = u/sqrt(sum(u*u))
      res = res-u*sum(u*res)

      ! Clean up
      call destroy_(u)
      call destroy_(wt)
      call destroy_(Fm)
      call destroy_(Fsig)
      call destroy_(d_alpha)
      call destroy_(alpha)
      call destroy_(Fc)
      call destroy_(FP)
   end subroutine

   subroutine make_BT_times(self,w,res)
   ! Return "res" = B^T w for a reflection vector "w", with the B of
   ! make_constraint_stiffness, but without forming B: "res" is the
   ! occupied-virtual block, over sqrt(gap), of the constraint matrix
   ! built from the residual vector 2 sqrt(alpha/d_alpha) w (N-p)/2,
   ! with the direction of the refitted scale factor removed from "w"
   ! first. One constraint build.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: w
      VEC(REAL,:), OUT :: res
      MAT(REAL,:,:), allocatable :: half,g_ov
      MAT(REAL,:,:), allocatable :: C
      VEC(CPX,:), allocatable :: Fc
      VEC(REAL,:), allocatable :: Fm,Fsig,alpha,d_alpha,wt,u,resid
      REAL :: fac
      INT :: i,j,ia,n_refl,n_par,n_occ,n_vir
      n_refl = size(self%crystal%xray_data%reflections)
      n_par = self%crystal%xray_data%n_param
      n_occ = self%n_a
      n_vir = self%n_bf-self%n_a
      fac = TWO/max(n_refl-n_par,1)

      ! The residual vector that makes the constraint 2 sum_k w_k w'_k A_k
      Fc = self%crystal%xray_data%reflections%F_calc
      alpha = F_scale_and_extn_correction_(self%crystal%xray_data)
      d_alpha = d_F_pred_d_F_abs_(self%crystal%xray_data)
      Fsig = self%crystal%xray_data%reflections%F_sigma
      call create_(Fm,n_refl)
      call create_(wt,n_refl)
      call create_(u,n_refl)
      call create_(resid,n_refl)
      Fm = max(abs(Fc),TOL(10))
      wt = sqrt(abs(alpha*d_alpha))/Fsig
      u = wt*Fm
      u = u/sqrt(sum(u*u))
      resid = w-u*sum(u*w)
      resid = TWO*sqrt(abs(alpha/d_alpha))*resid/fac

      ! The constraint matrix and its scaled occupied-virtual block
      call create_(C,self%n_bf,self%n_bf)
      if (uses_Hirshfeld_atoms_(self%crystal)) then; call make_H_r_constraint_(self,C,resid=resid)
      else; call make_r_constraint_(self,C,resid=resid)
      end if
      call create_(half,self%n_bf,n_vir)
      call create_(g_ov,n_occ,n_vir)
      call to_product_of_(half,C,self%MOs%r(:,n_occ+1:self%n_bf))
      call to_product_of_(g_ov,self%MOs%r(:,1:n_occ),half,transpose_a=TRUE)
      ia = 0
      do j = 1,n_vir
         do i = 1,n_occ
            ia = ia+1
            res(ia) = g_ov(i,j)/sqrt(orbital_gap_(self,i,n_occ+j))
         end do
      end do

      ! Clean up
      call destroy_(g_ov)
      call destroy_(half)
      call destroy_(C)
      call destroy_(resid)
      call destroy_(u)
      call destroy_(wt)
      call destroy_(Fm)
      call destroy_(Fsig)
      call destroy_(d_alpha)
      call destroy_(alpha)
      call destroy_(Fc)
   end subroutine

   subroutine apply_stiffness_operator(self,x,res,lam)
   ! Return "res" = (1 + lam G) x for a reflection vector "x", with
   ! G = (2/(N-p)) B B^T applied through make_BT_times and make_B_times.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: x
      VEC(REAL,:), OUT :: res
      REAL, IN :: lam
      VEC(REAL,:), allocatable :: t
      REAL :: fac
      INT :: n_refl,n_par
      n_refl = size(self%crystal%xray_data%reflections)
      n_par = self%crystal%xray_data%n_param
      fac = TWO/max(n_refl-n_par,1)
      call create_(t,self%n_a*(self%n_bf-self%n_a))
      call make_BT_times_(self,x,t)
      call make_B_times_(self,t,res)
      res = x+lam*fac*res
      call destroy_(t)
   end subroutine

   subroutine make_stiffness_preconditioner(self)
   ! Make the exact diagonal of the gain matrix, G_kk = (2/(N-p)) times
   ! the squared norm of row k of B, without storing B: the derivatives
   ! are built for a batch of reflections at a time, transformed to the
   ! occupied-virtual block and squared, and the batch is dropped. Each
   ! batch costs one pass over the shell pairs. Kept in .SCF_data with
   ! the lambda iteration it was made at. Used only when
   ! .SCF_data.stiffness_preconditioner is "diagonal": on ammonia it
   ! doubles the conjugate gradient count, the gain matrix being far
   ! from diagonal (docs/REPORT_ON_XCW_CONVERGENCE.md, section 9).
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: k_pts
      MAT3(CPX,:,:,:), allocatable :: ft_ab,ft_ab_eq
      MAT3(REAL,:,:,:), allocatable :: dF
      MAT(REAL,:,:), allocatable :: dF_ao,half,dF_ov
      VEC(CPX,:), allocatable :: Fc
      VEC(REAL,:), allocatable :: Fm,alpha,d_alpha,Fsig,wt,u,row
      REAL :: fac,cutoff,val
      INT :: q,fa,la,fb,lb,na,nb,atom_a,atom_b
      INT :: i,j,k,kb,k1,k2,n_batch,n_k,n_refl,n_par,n_occ,n_vir
      type(SHELL2_TYPE) :: sh
      n_k = n_unique_SF_k_pts_(self%crystal)
      n_refl = size(self%crystal%xray_data%reflections)
      n_par = self%crystal%xray_data%n_param
      n_occ = self%n_a
      n_vir = self%n_bf-self%n_a
      fac = TWO/max(n_refl-n_par,1)

      ! Batches of reflections, about 160 MB of derivatives each
      n_batch = max(1,min(n_refl,20000000/(self%n_bf*self%n_bf)))
      Fc = self%crystal%xray_data%reflections%F_calc
      alpha = F_scale_and_extn_correction_(self%crystal%xray_data)
      d_alpha = d_F_pred_d_F_abs_(self%crystal%xray_data)
      Fsig = self%crystal%xray_data%reflections%F_sigma
      call create_(Fm,n_refl)
      call create_(wt,n_refl)
      Fm = max(abs(Fc),TOL(10))
      wt = sqrt(abs(alpha*d_alpha))/Fsig
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)
      cutoff = TOL(10)/self%n_shell_pairs
      call destroy_(self%SCF_data%stiffness_precond)
      call create_(self%SCF_data%stiffness_precond,n_refl)
      self%SCF_data%stiffness_precond = ZERO
      call create_(dF_ao,self%n_bf,self%n_bf)
      call create_(half,self%n_bf,n_vir)
      call create_(dF_ov,n_occ,n_vir)
      call create_(row,n_occ*n_vir)
      k1 = 1
      do
         if (k1>n_refl) exit
         k2 = min(k1+n_batch-1,n_refl)

         ! The derivatives of |F_calc| for this batch
         call create_(dF,[k1,k2],[1,self%n_bf],[1,self%n_bf])
         dF = ZERO
         do q = 1,self%n_shell_pairs
            call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
            if (skip_ft_(sh,cutoff)) then
               call destroy_ptr_part_(sh)
               cycle
            end if
            call create_(ft_ab,[1,n_refl],[fa,la],[fb,lb])
            call create_(ft_ab_eq,n_k,na,nb)
            call make_FT_pair_(self,ft_ab_eq,k_pts,sh,atom_a,atom_b)
            call sum_unique_sf_ints_(self%crystal%spacegroup,ft_ab,ft_ab_eq,self%crystal%xray_data%reflections)
            call destroy_(ft_ab_eq)
            do i = fa,la
               do j = fb,min(lb,i)
                  dF(k1:k2,i,j) = (RE(ft_ab(k1:k2,i,j))*RE(Fc(k1:k2))+IM(ft_ab(k1:k2,i,j))*IM(Fc(k1:k2)))/Fm(k1:k2)
                  dF(k1:k2,j,i) = dF(k1:k2,i,j)
               end do
            end do
            call destroy_(ft_ab)
            call destroy_ptr_part_(sh)
         end do

         ! Each row of B, squared and summed
         do k = k1,k2
            dF_ao = dF(k,:,:)
            call to_product_of_(half,dF_ao,self%MOs%r(:,n_occ+1:self%n_bf))
            call to_product_of_(dF_ov,self%MOs%r(:,1:n_occ),half,transpose_a=TRUE)
            val = ZERO
            do j = 1,n_vir
               do i = 1,n_occ
                  val = val+dF_ov(i,j)**2/(orbital_gap_(self,i,n_occ+j))
               end do
            end do
            self%SCF_data%stiffness_precond(k) = fac*FOUR*wt(k)*wt(k)*val
         end do
         call destroy_(dF)
         k1 = k2+1
      end do

      ! This is the diagonal before the scale direction is removed from
      ! B, an upper bound on the projected one, which would need the
      ! whole matrix.
      self%SCF_data%stiffness_precond_lambda_iteration = self%SCF_data%lambda_iteration

      ! Clean up
      call destroy_(row)
      call destroy_(dF_ov)
      call destroy_(half)
      call destroy_(dF_ao)
      call destroy_(k_pts)
      call destroy_(wt)
      call destroy_(Fm)
      call destroy_(Fsig)
      call destroy_(d_alpha)
      call destroy_(alpha)
      call destroy_(Fc)
   end subroutine

   subroutine make_stiffness_deflation(self,start)
   ! Make the .SCF_data.stiffness_deflation leading eigenvectors of the
   ! gain matrix G and their eigenvalues, by Lanczos with full
   ! reorthogonalisation, started from "start" and run for twice as many
   ! steps as vectors wanted plus two, with G applied through the
   ! products above. The largest eigenvalue is kept as the largest gain.
   ! Kept in .SCF_data with the lambda iteration they were made at.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: start
      MAT(REAL,:,:), allocatable :: vlan,tmat,smat
      VEC(REAL,:), allocatable :: vcur,vprev,w,alpha_l,beta_l,theta,coef
      REAL :: norm
      INT :: j,k,m,m_l,n_refl
      n_refl = size(self%crystal%xray_data%reflections)
      m = min(self%SCF_data%stiffness_deflation,n_refl)
      m_l = min(2*m+2,n_refl)
      call create_(vlan,n_refl,m_l)
      call create_(vcur,n_refl)
      call create_(vprev,n_refl)
      call create_(w,n_refl)
      call create_(alpha_l,m_l)
      call create_(beta_l,m_l)
      alpha_l = ZERO
      beta_l = ZERO

      ! Lanczos
      vprev = ZERO
      vcur = start
      norm = sqrt(sum(vcur*vcur))
      if (norm<=ZERO) then
         vcur = ONE
         norm = sqrt(sum(vcur*vcur))
      end if
      vcur = vcur/norm
      k = 0
      do j = 1,m_l
         vlan(:,j) = vcur
         k = j
         call apply_stiffness_operator_(self,vcur,w,ONE)
         w = w-vcur
         alpha_l(j) = sum(vcur*w)
         w = w-alpha_l(j)*vcur
         if (j>1) w = w-beta_l(j-1)*vprev
         ! Full reorthogonalisation, twice
         call create_(coef,j)
         call to_product_of_(coef,vlan(:,1:j),w,transpose_a=TRUE)
         w = w-matmul(vlan(:,1:j),coef)
         call to_product_of_(coef,vlan(:,1:j),w,transpose_a=TRUE)
         w = w-matmul(vlan(:,1:j),coef)
         call destroy_(coef)
         norm = sqrt(sum(w*w))
         if (j==m_l OR norm<=TOL(10)) exit
         beta_l(j) = norm
         vprev = vcur
         vcur = w/norm
      end do

      ! The tridiagonal matrix and its eigenpairs, ascending
      call create_(tmat,k,k)
      call create_(smat,k,k)
      call create_(theta,k)
      tmat = ZERO
      do j = 1,k
         tmat(j,j) = alpha_l(j)
         if (j<k) then
            tmat(j,j+1) = beta_l(j)
            tmat(j+1,j) = beta_l(j)
         end if
      end do
      call solve_symmetric_eigenproblem_(tmat,theta,smat)

      ! The leading Ritz vectors
      m = min(m,k)
      call destroy_(self%SCF_data%deflation_W)
      call destroy_(self%SCF_data%deflation_theta)
      call create_(self%SCF_data%deflation_W,n_refl,m)
      call create_(self%SCF_data%deflation_theta,m)
      do j = 1,m
         self%SCF_data%deflation_theta(j) = max(theta(k-m+j),ZERO)
         self%SCF_data%deflation_W(:,j) = matmul(vlan(:,1:k),smat(:,k-m+j))
      end do
      self%SCF_data%gamma_max = self%SCF_data%deflation_theta(m)
      self%SCF_data%deflation_lambda_iteration = self%SCF_data%lambda_iteration

      ! Clean up
      call destroy_(theta)
      call destroy_(smat)
      call destroy_(tmat)
      call destroy_(beta_l)
      call destroy_(alpha_l)
      call destroy_(w)
      call destroy_(vprev)
      call destroy_(vcur)
      call destroy_(vlan)
   end subroutine

   subroutine solve_stiffness_system(self,rhs,y,lam)
   ! Solve (1 + lam G) y = rhs by conjugate gradients, to the relative
   ! residual .SCF_data.stiffness_cg_tolerance, with G applied through
   ! the products above, preconditioned by 1 + lam times the estimated
   ! diagonal of G when that has been made for this lambda. The
   ! iteration count is kept in .SCF_data.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: rhs
      VEC(REAL,:), OUT :: y
      REAL, IN :: lam
      VEC(REAL,:), allocatable :: r,z,p,Ap,Minv,coef
      REAL :: rz,rz_new,pAp,alpha_cg,beta_cg,tol
      INT :: it,j,m,n_refl,max_it
      BIN :: deflate
      n_refl = size(rhs)
      max_it = min(n_refl,200)
      tol = self%SCF_data%stiffness_cg_tolerance*sqrt(sum(rhs*rhs))

      ! Deflation: the leading eigenvectors of G, if made for this lambda
      deflate = FALSE
      m = 0
      if (allocated(self%SCF_data%deflation_W)) then
         if (self%SCF_data%deflation_lambda_iteration==self%SCF_data%lambda_iteration) then
            deflate = TRUE
            m = size(self%SCF_data%deflation_theta)
            call create_(coef,m)
         end if
      end if

      ! The preconditioner, the identity unless the diagonal was asked for
      call create_(Minv,n_refl)
      Minv = ONE
      if (self%SCF_data%stiffness_preconditioner=="diagonal") then
         if (allocated(self%SCF_data%stiffness_precond)) then
            if (self%SCF_data%stiffness_precond_lambda_iteration==self%SCF_data%lambda_iteration) then
               Minv = ONE/(ONE+lam*self%SCF_data%stiffness_precond)
            end if
         end if
      end if
      call create_(r,n_refl)
      call create_(z,n_refl)
      call create_(p,n_refl)
      call create_(Ap,n_refl)
      y = ZERO
      r = rhs
      z = Minv*r
      if (deflate) call apply_stiffness_deflation_(self,r,z,lam,coef)
      p = z
      rz = sum(r*z)
      it = 0
      do
         if (sqrt(sum(r*r))<=tol OR it>=max_it) exit
         call apply_stiffness_operator_(self,p,Ap,lam)
         pAp = sum(p*Ap)
         if (pAp<=ZERO) exit
         alpha_cg = rz/pAp
         y = y+alpha_cg*p
         r = r-alpha_cg*Ap
         z = Minv*r
         if (deflate) call apply_stiffness_deflation_(self,r,z,lam,coef)
         rz_new = sum(r*z)
         beta_cg = rz_new/rz
         rz = rz_new
         p = z+beta_cg*p
         it = it+1
      end do
      self%SCF_data%stiffness_cg_iterations = it
      if (deflate) call destroy_(coef)
      call destroy_(Ap)
      call destroy_(p)
      call destroy_(z)
      call destroy_(r)
      call destroy_(Minv)
   end subroutine

   PURE subroutine apply_stiffness_deflation(self,r,z,lam,coef)
   ! Add to "z" the spectral correction W [(1 + lam theta)^-1 - 1] W^T r,
   ! which maps the deflated eigenvalues of 1 + lam G to one. "coef" is
   ! work space of the number of deflation vectors.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), IN :: r
      VEC(REAL,:), INOUT :: z
      REAL, IN :: lam
      VEC(REAL,:), INOUT :: coef
      INT :: j
      call to_product_of_(coef,self%SCF_data%deflation_W,r,transpose_a=TRUE)
      do j = 1,size(coef)
         coef(j) = coef(j)*(ONE/(ONE+lam*self%SCF_data%deflation_theta(j))-ONE)
      end do
      z = z+matmul(self%SCF_data%deflation_W,coef)
   end subroutine

   subroutine make_constraint_stiffness_estimates(self)
   ! The fit statistics of make_constraint_stiffness without forming the
   ! gain matrix. The m leading eigenpairs (theta_j, w_j) of G from Lanczos,
   ! .SCF_data.stiffness_deflation of them, give the stiff part exactly:
   !    tr H = sum_j lam theta_j/(1 + lam theta_j) + tr[H (1 - W W^T)],
   !    H_kk = sum_j lam theta_j/(1 + lam theta_j) W_kj^2 + remainder,
   ! and the remainder, H minus that part, comes from Hutchinson's
   ! estimator with .SCF_data.stiffness_samples sign vectors, each needing
   ! one conjugate gradient solve, itself deflated. The Ritz part is used
   ! as a control variate: unbiased however well the Ritz vectors have
   ! converged, and small in variance when they have. Without
   ! deflation vectors the whole trace is estimated that way and the
   ! largest gain comes from power iteration. The eigenvalues, the own
   ! gains and the share of the stiffest mode are not made.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(CPX,:), allocatable :: Fc
      VEC(REAL,:), allocatable :: Fm,Fexp,Fsig,alpha,z,Hz,x,res,wz
      VEC(INT,:), allocatable :: ind
      REAL :: lam,val,p_eff,chi2,norm,h
      INT :: seed
      INT :: j,k,m,n_refl,n_par,n_w
      BIN :: deflated
      n_refl = size(self%crystal%xray_data%reflections)
      n_par = self%crystal%xray_data%n_param
      lam = self%SCF_data%lambda

      ! The residuals
      Fc = self%crystal%xray_data%reflections%F_calc
      alpha = F_scale_and_extn_correction_(self%crystal%xray_data)
      Fexp = self%crystal%xray_data%reflections%F_exp
      Fsig = self%crystal%xray_data%reflections%F_sigma
      call create_(Fm,n_refl)
      Fm = max(abs(Fc),TOL(10))
      call destroy_(self%SCF_data%fit_residual)
      call destroy_(self%SCF_data%hat_diag)
      call destroy_(self%SCF_data%stiffness_eval)
      call destroy_(self%SCF_data%stiffness_evec)
      call destroy_(self%SCF_data%stiffness_diag)
      call destroy_(self%SCF_data%stiffness_B)
      call create_(self%SCF_data%fit_residual,n_refl)
      call create_(self%SCF_data%hat_diag,n_refl)
      self%SCF_data%fit_residual = (alpha*Fm-Fexp)/Fsig
      self%SCF_data%hat_diag = ZERO

      ! The deflation vectors for this lambda, started from the residuals
      if (lam>ZERO AND self%SCF_data%stiffness_deflation>0) then
         if (self%SCF_data%deflation_lambda_iteration/=self%SCF_data%lambda_iteration) then
            call make_stiffness_deflation_(self,self%SCF_data%fit_residual)
         end if
      end if
      deflated = FALSE
      n_w = 0
      if (allocated(self%SCF_data%deflation_W)) then
         if (self%SCF_data%deflation_lambda_iteration==self%SCF_data%lambda_iteration) then
            deflated = TRUE
            n_w = size(self%SCF_data%deflation_theta)
         end if
      end if

      ! The stiff part exactly
      p_eff = ZERO
      if (deflated AND lam>ZERO) then
         do j = 1,n_w
            h = lam*self%SCF_data%deflation_theta(j)/(ONE+lam*self%SCF_data%deflation_theta(j))
            p_eff = p_eff+h
            self%SCF_data%hat_diag = self%SCF_data%hat_diag+h*self%SCF_data%deflation_W(:,j)**2
         end do
      end if

      ! The rest from sign vectors, projected off the deflation vectors.
      ! The signs come from a fixed linear congruential sequence (Lehmer's,
      ! modulus 65537), the same on every rank.
      call create_(z,n_refl)
      call create_(Hz,n_refl)
      call create_(x,n_refl)
      if (deflated) call create_(wz,n_w)
      seed = 12345
      m = self%SCF_data%stiffness_samples
      if (lam>ZERO AND m>0) then
         val = ZERO
         call create_(res,n_refl)
         res = ZERO
         do j = 1,m
            do k = 1,n_refl
               seed = mod(75*seed+74,65537)
               if (mod(seed/256,2)==0) then; z(k) = ONE
               else; z(k) = -ONE
               end if
            end do
            call solve_stiffness_system_(self,z,x,lam)
            Hz = z-x
            if (deflated) then
               call to_product_of_(wz,self%SCF_data%deflation_W,z,transpose_a=TRUE)
               do k = 1,n_w
                  wz(k) = wz(k)*lam*self%SCF_data%deflation_theta(k)/(ONE+lam*self%SCF_data%deflation_theta(k))
               end do
               Hz = Hz-matmul(self%SCF_data%deflation_W,wz)
            end if
            val = val+sum(z*Hz)
            res = res+z*Hz
         end do
         p_eff = p_eff+val/m
         self%SCF_data%hat_diag = self%SCF_data%hat_diag+res/m
         call destroy_(res)
      end if

      ! The large leverages exactly, one solve each: they dominate the
      ! leave-one-out sum through 1/(1-h)^2, which magnifies their noise.
      ! h_k = 1 - [(1 + lam G)^-1]_kk, for up to 20 with an estimate
      ! above 1/2; the rest kept within [0,1).
      if (lam>ZERO AND m>0) then
         call create_(ind,n_refl)
         call quick_sort_(self%SCF_data%hat_diag,ind,decreasing_order=TRUE)
         do j = 1,min(20,n_refl)
            k = ind(j)
            if (self%SCF_data%hat_diag(k)<=HALF) exit
            z = ZERO
            z(k) = ONE
            call solve_stiffness_system_(self,z,x,lam)
            self%SCF_data%hat_diag(k) = ONE-x(k)
         end do
         call destroy_(ind)
      end if
      self%SCF_data%hat_diag = max(ZERO,min(self%SCF_data%hat_diag,0.999d0))
      p_eff = p_eff+n_par
      if (deflated) call destroy_(wz)
      call destroy_(x)
      call destroy_(Hz)
      call destroy_(z)

      ! The largest gain: from Lanczos if made, else by power iteration
      if (deflated) then
         self%SCF_data%gamma_max = self%SCF_data%deflation_theta(n_w)
      else
         call create_(x,n_refl)
         call create_(res,n_refl)
         x = ONE
         x = x/sqrt(sum(x*x))
         val = ZERO
         do j = 1,30
            call apply_stiffness_operator_(self,x,res,ONE)
            res = res-x
            norm = sqrt(sum(res*res))
            if (norm<=ZERO) exit
            if (abs(norm-val)<=TOL(6)*norm) then
               val = norm
               exit
            end if
            val = norm
            x = res/norm
         end do
         self%SCF_data%gamma_max = val
         call destroy_(res)
         call destroy_(x)
      end if

      ! The statistics
      chi2 = sum(self%SCF_data%fit_residual*self%SCF_data%fit_residual)
      self%SCF_data%p_eff = p_eff
      self%SCF_data%chi2_fit = chi2
      self%SCF_data%AIC_sigma = n_refl*log(chi2/n_refl)+TWO*p_eff
      self%SCF_data%GCV = n_refl*chi2/(n_refl-p_eff)**2
      self%SCF_data%LOO_sum = sum((self%SCF_data%fit_residual/(ONE-self%SCF_data%hat_diag))**2)
      self%SCF_data%stiffness_made = TRUE
      self%SCF_data%stiffness_lambda_iteration = self%SCF_data%lambda_iteration

      ! Clean up
      call destroy_(Fm)
      call destroy_(Fsig)
      call destroy_(Fexp)
      call destroy_(alpha)
      call destroy_(Fc)
   end subroutine

   subroutine put_constraint_stiffness(self)
   ! Print, at a converged lambda, the summary made by
   ! make_constraint_stiffness: the largest gains per unit lambda, the
   ! stable fraction of new density for a damped step, and the fit
   ! statistics. The table of reflections is printed once, at the end of
   ! the run, by put_reflection_influence.
      type(MOLECULE_TYPE), INOUT :: self
      REAL :: g_max,lam,val
      INT :: k,m,n_refl
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:put_constraint_stiffness ... no SCF_data")
   ENSURE(self%SCF_data%stiffness_made,"MOLECULE.SCF:put_constraint_stiffness ... no constraint stiffness")
      n_refl = size(self%crystal%xray_data%reflections)
      lam = self%SCF_data%lambda
      g_max = self%SCF_data%gamma_max
      call flush_(stdout)
      call text_(stdout,"====================")
      call text_(stdout,"Constraint stiffness")
      call text_(stdout,"====================")
      call flush_(stdout)
      call text_(stdout,". The gain is how much a plain SCF step amplifies an error in")
      call text_(stdout,"  the structure factors; a step mixing in a fraction x of the new")
      call text_(stdout,"  density converges only if x < 2/(1 + lambda times the gain).")
      call text_(stdout,". The effective number of parameters is the summed sensitivity")
      call text_(stdout,"  of each prediction to its own observation.")
      call text_(stdout,". The criteria choose lambda: the smallest is best. Leave-one-out")
      call text_(stdout,"  and GCV do not move with a common error in the sigmas.")
      call flush_(stdout)
      call show_(stdout,"No. of reflections               =",n_refl)
      call show_(stdout,"Lambda                           =",lam,precision=6)
      call show_(stdout,"Largest gain per unit lambda     =",g_max,precision=3)
      if (allocated(self%SCF_data%stiffness_eval)) then
         val = sum(self%SCF_data%stiffness_eval)
         call show_(stdout,"Sum of all gains per unit lambda =",val,precision=3)
      end if
      val = lam*g_max
      call show_(stdout,"Largest gain at this lambda      =",val,precision=3)
      val = TWO/(ONE+lam*g_max)
      call show_(stdout,"Largest stable new fraction      =",val,precision=3)
      val = ONE/g_max
      call show_(stdout,"Lambda where undamped steps fail =",val,precision=6)
      call flush_(stdout)
      call show_(stdout,"Effective no. of parameters      =",self%SCF_data%p_eff,precision=3)
      call show_(stdout,"Chi^2, sum of (F_z)^2            =",self%SCF_data%chi2_fit,precision=3)
      val = self%SCF_data%chi2_fit+TWO*self%SCF_data%p_eff
      call show_(stdout,"AIC, chi^2 + 2 p_eff             =",val,precision=3)
      val = self%SCF_data%chi2_fit+self%SCF_data%p_eff*log(real(n_refl,kind=REAL_KIND))
      call show_(stdout,"BIC, chi^2 + p_eff ln N          =",val,precision=3)
      call show_(stdout,"AIC, N ln(chi^2/N) + 2 p_eff     =",self%SCF_data%AIC_sigma,precision=3)
      call show_(stdout,"GCV, N chi^2/(N-p_eff)^2         =",self%SCF_data%GCV,precision=6)
      call show_(stdout,"Leave-one-out sum of squares     =",self%SCF_data%LOO_sum,precision=3)
      if (stiffness_is_matrix_free_(self)) then
         call show_(stdout,"Sign vectors for p_eff           =",self%SCF_data%stiffness_samples)
      end if
      call flush_(stdout)
      if (allocated(self%SCF_data%stiffness_eval)) then
         call text_(stdout,"The ten largest gains per unit lambda:")
         do m = n_refl,max(n_refl-9,1),-1
            call put_(stdout,self%SCF_data%stiffness_eval(m),fmt="f",width=12,precision=3)
         end do
         call flush_(stdout)
      else if (allocated(self%SCF_data%deflation_theta)) then
         call text_(stdout,"The largest gains per unit lambda, from Lanczos:")
         k = size(self%SCF_data%deflation_theta)
         do m = k,max(k-9,1),-1
            call put_(stdout,self%SCF_data%deflation_theta(m),fmt="f",width=12,precision=3)
         end do
         call flush_(stdout)
      end if
   end subroutine

   subroutine put_reflection_influence(self)
   ! Print, for the last converged lambda, how much each reflection
   ! influences the fit: its leverage, its residual, its residual when left
   ! out, and its Cook's distance
   !    D = r^2 h / (p_eff (1-h)^2),
   ! with r the standardised residual and h the leverage, in a table
   ! sorted by D, then a histogram of D in multiples of the usual
   ! threshold 4/N.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: Fexp,Fsig,cook,col,stl
      VEC(INT,:), allocatable :: ind,hh
      VEC(INT,8) :: nbin
      REAL :: thresh,r,h,edge
      INT :: b,k,m,n_refl,n_col
      BIN :: have_gain
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:put_reflection_influence ... no SCF_data")
   ENSURE(self%SCF_data%stiffness_made,"MOLECULE.SCF:put_reflection_influence ... no constraint stiffness")
      n_refl = size(self%crystal%xray_data%reflections)
      Fexp = self%crystal%xray_data%reflections%F_exp
      Fsig = self%crystal%xray_data%reflections%F_sigma
      thresh = FOUR/n_refl

      ! Cook's distance, and the order of the table
      call create_(cook,n_refl)
      do k = 1,n_refl
         h = self%SCF_data%hat_diag(k)
         r = self%SCF_data%fit_residual(k)
         cook(k) = r*r*h/(self%SCF_data%p_eff*(ONE-h)**2)
      end do
      call create_(ind,n_refl)
      call quick_sort_(cook,ind,decreasing_order=TRUE)
      call flush_(stdout)
      call text_(stdout,"============================")
      call text_(stdout,"Influence of each reflection")
      call text_(stdout,"============================")
      call flush_(stdout)
      call text_(stdout,". For the last converged lambda, sorted by Cook's distance.")
      call text_(stdout,". stl is sin(theta)/lambda in 1/Angstrom; F/sigma is the observed")
      call text_(stdout,"  magnitude over its sigma.")
      call text_(stdout,". The residual is (F_pred - F_exp)/sigma; left out, it is the same")
      call text_(stdout,"  residual had the reflection been left out of the fit, r/(1-h).")
      call text_(stdout,". The leverage h is how far the prediction follows its own")
      call text_(stdout,"  observation: 0 not at all, 1 all the way.")
      call text_(stdout,". The own gain is the reflection's share of the stiffness of the")
      call text_(stdout,"  plain SCF step, per unit lambda (explicit form only).")
      call text_(stdout,". Cook's distance is how far the whole fit would move without the")
      call text_(stdout,"  reflection; above 4/N it is worth a second look.")
      call flush_(stdout)
      call show_(stdout,"Lambda                           =",self%SCF_data%lambda_history(size(self%SCF_data%lambda_history)),precision=6)
      call show_(stdout,"Cook's distance threshold, 4/N   =",thresh,precision=4)
      m = 0
      do k = 1,n_refl
         if (cook(k)>thresh) m = m+1
      end do
      call show_(stdout,"Reflections above it             =",m)
      call flush_(stdout)

      ! The table
      have_gain = allocated(self%SCF_data%stiffness_diag)
      n_col = 10
      if (have_gain) n_col = 11
      call create_(table,n_col)
      call create_(hh,n_refl)
      call create_(col,n_refl)
      call create_(stl,n_refl)
      do m = 1,n_refl
         hh(m) = self%crystal%xray_data%reflections(ind(m))%h1
      end do
      call set_heading_(table(1),"h"); call set_values_(table(1),hh)
      do m = 1,n_refl
         hh(m) = self%crystal%xray_data%reflections(ind(m))%h2
      end do
      call set_heading_(table(2),"k"); call set_values_(table(2),hh)
      do m = 1,n_refl
         hh(m) = self%crystal%xray_data%reflections(ind(m))%h3
      end do
      call set_heading_(table(3),"l"); call set_values_(table(3),hh)
      do m = 1,n_refl
         stl(m) = self%crystal%xray_data%reflections(ind(m))%stl
      end do
      call set_heading_(table(4),"stl"); call set_values_(table(4),stl)
      call set_heading_(table(5),"F/sigma"); call set_values_(table(5),Fexp(ind)/Fsig(ind))
      col = self%SCF_data%fit_residual(ind)
      call set_heading_(table(6),"Residual"); call set_values_(table(6),col)
      col = self%SCF_data%hat_diag(ind)
      call set_heading_(table(7),"Leverage"); call set_subhead_(table(7),"h"); call set_values_(table(7),col)
      col = self%SCF_data%fit_residual(ind)/(ONE-self%SCF_data%hat_diag(ind))
      call set_heading_(table(8),"Residual"); call set_subhead_(table(8),"left out"); call set_values_(table(8),col)
      col = cook(ind)
      call set_heading_(table(9),"Cook's"); call set_subhead_(table(9),"distance"); call set_values_(table(9),col)
      col = cook(ind)/thresh
      call set_heading_(table(10),"Cook's"); call set_subhead_(table(10),"/ (4/N)"); call set_values_(table(10),col)
      if (have_gain) then
         col = self%SCF_data%stiffness_diag(ind)
         call set_heading_(table(11),"Own"); call set_subhead_(table(11),"gain"); call set_values_(table(11),col)
      end if
      call put_(table)
      call destroy_(table)
      call destroy_(stl)
      call destroy_(col)
      call destroy_(hh)

      ! A histogram of Cook's distance, in multiples of 4/N
      nbin = 0
      do k = 1,n_refl
         r = cook(k)/thresh
         b = 1
         edge = 0.125d0
         do while (r>=edge AND b<8)
            b = b+1
            edge = TWO*edge
         end do
         nbin(b) = nbin(b)+1
      end do
      call flush_(stdout)
      call text_(stdout,"Number of reflections by Cook's distance, in multiples of 4/N:")
      call flush_(stdout)
      call text_(stdout,"  below 1/8   1/8-1/4   1/4-1/2   1/2-1       1-2       2-4       4-8  8 and up")
      do b = 1,8
         call put_(stdout,nbin(b),width=10)
      end do
      call flush_(stdout)
      call destroy_(ind)
      call destroy_(cook)
      call destroy_(Fsig)
      call destroy_(Fexp)
   end subroutine

   subroutine correct_constraint_overshoot(self)
   ! Remove the overshoot of the plain constrained SCF step, in the
   ! space of reflections. On entry the Fock matrix is F(D) + lambda C(r),
   ! with "r" the standardised residuals of the current density, and its
   ! occupied-virtual block "g" in the current orbitals is the gradient.
   ! Diagonalising it rotates the orbitals by -g/(e_a - e_i) to first
   ! order, which moves the residuals by
   !    dr = -2 B (g/sqrt(e_a - e_i))
   ! and the stiff directions of that move overshoot. The step with the
   ! constraint curvature included, the uncoupled Newton step, moves them
   ! by (1 + lambda G)^-1 dr instead. That step is what diagonalising
   ! F(D) + lambda C(r + y) gives, with y = (1 + lambda G)^-1 dr, so the
   ! constraint is rebuilt with residuals r + y. The gain matrix G and B
   ! are remade from the current orbitals at every iteration: a stored B
   ! refers to the orbitals it was made in, and the gradient in those
   ! orbitals measures the rotation already made, not the error left, so
   ! reusing it diverges (TASKS_AND_HISTORY.md, CLOSED 2026-10-09). What the
   ! reflections cannot see is left to DIIS. Only for restricted
   ! wavefunctions. Theory: docs/REPORT_ON_XCW_CONVERGENCE.md.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), allocatable :: C
      MAT(REAL,:,:), allocatable :: half,g_ov
      VEC(REAL,:), allocatable :: gt,dr,p,y
      REAL :: lam
      INT :: i,j,ia,n_refl,n_occ,n_vir
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:correct_constraint_overshoot ... no SCF_data")
   ENSURE(spinorbital_genre_(self%SCF_data)=="r","MOLECULE.SCF:correct_constraint_overshoot ... only for restricted wavefunctions")
   ENSURE(allocated(self%fock_mx),"MOLECULE.SCF:correct_constraint_overshoot ... no fock matrix")
   ENSURE(allocated(self%MOs),"MOLECULE.SCF:correct_constraint_overshoot ... no molecular orbitals")
   ENSURE(allocated(self%MO_energies),"MOLECULE.SCF:correct_constraint_overshoot ... no orbital energies")
      lam = self%SCF_data%lambda
      if (is_zero_(lam)) return

      ! The gain matrix and B at the current orbitals, unless matrix-free.
      ! The statistics made with them belong to this iterate, not to a
      ! converged one, so they are not left for the footer to print.
      if (NOT stiffness_is_matrix_free_(self)) then
         call make_constraint_stiffness_(self)
         self%SCF_data%stiffness_made = FALSE
      end if
      n_refl = size(self%crystal%xray_data%reflections)
      n_occ = self%n_a
      n_vir = self%n_bf-self%n_a

      ! The scaled orbital gradient of the effective Fock matrix
      call create_(half,self%n_bf,n_vir)
      call create_(g_ov,n_occ,n_vir)
      call to_product_of_(half,self%fock_mx%r,self%MOs%r(:,n_occ+1:self%n_bf))
      call to_product_of_(g_ov,self%MOs%r(:,1:n_occ),half,transpose_a=TRUE)
      call create_(gt,n_occ*n_vir)
      ia = 0
      do j = 1,n_vir
         do i = 1,n_occ
            ia = ia+1
            gt(ia) = g_ov(i,j)/sqrt(orbital_gap_(self,i,n_occ+j))
         end do
      end do
      call destroy_(g_ov)
      call destroy_(half)

      ! The move of the residuals a plain step would make
      call create_(dr,n_refl)
      if (stiffness_is_matrix_free_(self)) then
         call make_B_times_(self,gt,dr)
      else
         call to_product_of_(dr,self%SCF_data%stiffness_B,gt)
      end if
      dr = -TWO*dr
      call destroy_(gt)

      ! The move with the constraint curvature included: by conjugate
      ! gradients, with a preconditioner made once per lambda, or in the
      ! eigenbasis of G
      call create_(y,n_refl)
      call create_(p,n_refl)
      if (stiffness_is_matrix_free_(self)) then
         if (self%SCF_data%stiffness_preconditioner=="diagonal" AND self%SCF_data%stiffness_precond_lambda_iteration/=self%SCF_data%lambda_iteration) then
            call make_stiffness_preconditioner_(self)
         end if
         if (self%SCF_data%stiffness_deflation>0 AND self%SCF_data%deflation_lambda_iteration/=self%SCF_data%lambda_iteration) then
            call make_stiffness_deflation_(self,dr)
         end if
         call solve_stiffness_system_(self,dr,y,lam)
      else
         call to_product_of_(p,self%SCF_data%stiffness_evec,dr,transpose_a=TRUE)
         do j = 1,n_refl
            p(j) = p(j)/(ONE+lam*self%SCF_data%stiffness_eval(j))
         end do
         call to_product_of_(y,self%SCF_data%stiffness_evec,p)
      end if

      ! Rebuild the constraint with the shifted residuals, by the same
      ! partition model as the constraint itself, and add it
      call create_(C,self%n_bf,spinorbital_genre_(self%SCF_data))
      if (uses_Hirshfeld_atoms_(self%crystal)) then; call make_H_r_constraint_(self,C%r,resid=y)
      else; call make_r_constraint_(self,C%r,resid=y)
      end if
      call plus_scaled_(self%fock_mx,C,lam)

      ! Clean up
      call destroy_(C)
      call destroy_(y)
      call destroy_(p)
      call destroy_(dr)
   end subroutine

   subroutine make_gc_constraint(self,C)
   ! Make the constraint matrix "C" for restricted wavefunction fitting.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: C  ! CPX ????
      MAT(REAL,:,:), allocatable :: k_pts
      MAT3(CPX,:,:,:), allocatable :: ft_ab
      MAT3(CPX,:,:,:), allocatable :: ft_ab_eq
      VEC(CPX,:), allocatable :: Fc
      VEC(REAL,:), allocatable :: fac_times_alpha
      VEC(REAL,:), allocatable :: Fm,Fexp,Fsig
      VEC(REAL,:), allocatable :: alpha,d_alpha,Fcr,Fci
      REAL :: fac,cutoff
      INT :: q,fa,la,fb,lb,na,nb
      INT :: i,j,atom_a,atom_b
      INT :: n_k,n_refl,n_par
      type(SHELL2_TYPE) :: sh
      n_k = n_unique_SF_k_pts_(self%crystal)
      n_refl = size(self%crystal%xray_data%reflections)
      n_par = self%crystal%xray_data%n_param
      fac = TWO/max(n_refl-n_par,1)
      call create_(fac_times_alpha,n_refl)
      call create_(Fcr,n_refl)
      call create_(Fci,n_refl)
      call create_(Fc,n_refl)
      Fc = self%crystal%xray_data%reflections%F_calc
      call create_(alpha,n_refl)
      alpha = F_scale_and_extn_correction_(self%crystal%xray_data)
      call create_(d_alpha,n_refl)
      d_alpha = d_F_pred_d_F_abs_(self%crystal%xray_data)
      call create_(Fexp,n_refl); Fexp = self%crystal%xray_data%reflections%F_exp
      call create_(Fsig,n_refl); Fsig = self%crystal%xray_data%reflections%F_sigma
      call create_(Fm,n_refl); Fm = max(abs(Fc),TOL(10))

      ! See the note in .make_r_constraint: "alpha" makes F_pred, "d_alpha"
      ! differentiates it, and they differ only when extinction is on.
      fac_times_alpha = fac*d_alpha*(alpha*Fm-Fexp)/(Fsig*Fsig*Fm)
      call destroy_(Fm)
      call destroy_(Fsig)
      call destroy_(Fexp)
      call destroy_(d_alpha)
      call destroy_(alpha)
      Fcr = fac_times_alpha*RE(Fc)
      Fci = fac_times_alpha*IM(Fc)
      call destroy_(fac_times_alpha)
      call destroy_(Fc)
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)
      cutoff = TOL(10)/self%n_shell_pairs
      C = ZERO
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.SCF:make_gc_constraint")
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (skip_ft_(sh,cutoff)) then
            call destroy_ptr_part_(sh)
            cycle
         end if
         call create_(ft_ab,[1,n_refl],[fa,la],[fb,lb])
         call create_(ft_ab_eq,n_k,na,nb)
         call make_FT_pair_(self,ft_ab_eq,k_pts,sh,atom_a,atom_b)
         call sum_unique_sf_ints_(self%crystal%spacegroup,ft_ab,ft_ab_eq,self%crystal%xray_data%reflections)
         call destroy_(ft_ab_eq)
         do i = fa,la
            do j = fb,min(lb,i)
               C(i,j) = sum_elements_(RE(ft_ab(:,i,j))*Fcr+IM(ft_ab(:,i,j))*Fci)
               C(i+self%n_bf,j+self%n_bf) = sum_elements_(RE(ft_ab(:,i,j))*Fcr+IM(ft_ab(:,i,j))*Fci)  ! ???
            end do
         end do
         call destroy_(ft_ab)
         call destroy_ptr_part_(sh)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.SCF:make_gc_constraint")

      ! Symmetrise
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(C)
      else
         call symmetric_reflect_(C)  ! make hermitean ????
      end if

      ! Get rid of OO block if requested -- not valid for gchf
      !if (.SCF_data.zero_oo_constraint_block) then
      !   C.change_basis_using(.MOs.r)
      !   C(1:.n_a,1:.n_a) = ZERO
      !   C.back_transform_using(.MOs.r)
      !end

      ! Clean up
      call destroy_(k_pts)
      call destroy_(Fci)
      call destroy_(Fcr)
   end subroutine

   subroutine make_H_r_constraint(self,constraint,resid)
   ! Make the "constraint" matrix for restricted X-ray wavefunction
   ! fitting ...using Hirshfeld atoms to do thermal smearing. If "resid"
   ! is present it is used as the standardised residuals
   ! (F_pred - F_exp)/sigma in place of those of the current density.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), target :: constraint
      VEC(REAL,:), optional, IN :: resid
      VEC(CPX,:), allocatable :: Fc,tfc,coef
      VEC(REAL,:), allocatable :: ri_alpha
      BIN :: fitted
      VEC(REAL,:), allocatable :: Fcr,Fci,X,wt,Fm
      VEC(REAL,:), allocatable :: Fexp,Fsig,alpha
      VEC(REAL,:), allocatable :: d_alpha
      VEC(REAL,:), allocatable :: fac_times_alpha
      VEC(REAL,:), allocatable :: xa,ya,za,a2
      VEC(REAL,:), allocatable :: tf2,tf3,tf4
      VEC(REAL,:), allocatable :: ra,ga,gb
      VEC(INT,:), allocatable :: near_atom
      VEC(INT,:), allocatable :: pi,pj,pn
      VEC(BIN,:), allocatable :: skipa0,skipa
      VEC(BIN,:), allocatable :: skipab,skipb
      MAT(REAL,3,3) :: adp2
      MAT3(REAL,3,3,3) :: adp3
      MAT4(REAL,3,3,3,3) :: adp4
      MAT(REAL,:,:), allocatable :: k_pt,pt
      MAT(REAL,:,:), allocatable :: bf_save
      VEC(type(EVEC_BIN_TYPE),:), allocatable :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: bf_grid
      REAL :: fac,cutoff,rf,val
      INT :: ca,oa,fsa,lsa,sa,fa,la,na
      INT :: cb,ob,fsb,lsb,sb,fb,lb,nb
      INT :: n_k,n_refl,n_par,n_pt,n_keep
      INT :: c,fba,as,a,b,p,i,j,n
      type(SHELL1_TYPE) :: sha
   ENSURE(has_Hirshfeld_inputs_(self),"MOLECULE.SCF:make_H_r_constraint ... no becke_grid/ANOs! make_HA_info?")
   ENSURE(allocated(self%density_mx),"MOLECULE.SCF:make_H_r_constraint ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.SCF:make_H_r_constraint ... no restricted density matrix")
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_H_r_constraint ... no scf data")
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_H_r_constraint ... no crystal info")
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_H_r_constraint ... no basis info")

      ! The fitted Hirshfeld atoms take the transpose of their fit in
      ! place of the point-by-point sum over reflections
      fitted = self%crystal%xray_data%partition_model=="oc-ri"

      ! Number of reflections
      n_k = n_unique_SF_k_pts_(self%crystal)
      n_refl = size(self%crystal%xray_data%reflections)
      n_par = self%crystal%xray_data%n_param

      ! Multipliers for the ft integrals
      call create_(Fcr,n_refl)
      call create_(Fci,n_refl)

      ! The calculated (complex) structure factors
      call create_(Fc,n_refl)
      Fc = self%crystal%xray_data%reflections%F_calc

      ! Make the multipliers ... and clean up
      fac = TWO/max(n_refl-n_par,1)
      call create_(fac_times_alpha,n_refl)
      call create_(alpha,n_refl)
      call create_(d_alpha,n_refl)
      call create_(Fexp,n_refl)
      call create_(Fsig,n_refl)
      call create_(Fm,n_refl)
      alpha = F_scale_and_extn_correction_(self%crystal%xray_data)
      d_alpha = d_F_pred_d_F_abs_(self%crystal%xray_data)
      Fexp = self%crystal%xray_data%reflections%F_exp
      Fsig = self%crystal%xray_data%reflections%F_sigma
      Fm = max(abs(Fc),TOL(10))

      ! See the note in .make_r_constraint: "alpha" makes F_pred, "d_alpha"
      ! differentiates it, and they differ only when extinction is on.
      if (present(resid)) then
         fac_times_alpha = fac*d_alpha*resid/(Fsig*Fm)
      else
         fac_times_alpha = fac*d_alpha*(alpha*Fm-Fexp)/(Fsig*Fsig*Fm)
      end if
      call destroy_(Fm)
      call destroy_(Fsig)
      call destroy_(Fexp)
      call destroy_(d_alpha)
      call destroy_(alpha)
      Fcr = fac_times_alpha*RE(Fc)
      Fci = fac_times_alpha*IM(Fc)
      call destroy_(fac_times_alpha)
      call destroy_(Fc)

      ! Symmetry generated K points and SF's
      call create_(k_pt,n_k,3)

      ! Make list of symmetry non-equivalent symops
      ! and the necessary k points
      call make_unique_X_SF_k_pts_(self%crystal,k_pt)

      ! Basis function cutoff
      cutoff = self%becke_grid%basis_fn_cutoff

      ! Max no of becke grid points per atom
      n_pt = max_no_of_pts_per_atom_(self%becke_grid)

      ! Atom "a" displaced points and skip switches
      call create_(xa,n_pt)
      call create_(ya,n_pt)
      call create_(za,n_pt)
      call create_(a2,n_pt)
      call create_(skipa0,n_pt)
      call create_(skipab,n_pt)

      ! Initialse the constraint
      constraint = ZERO

      ! Loop over integration atoms "c"
      ! -- Must we loop on all, or just asymmetric unit?
      do c = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.SCF:make_H_r_constraint")

         ! Repetition factor for this atom
         rf = self%crystal%repetition_factor(c)
         if (is_zero_(rf)) cycle
         rf = ONE/rf

         ! Get the Becke grid for atom "c"
         call make_atom_grid_(self%becke_grid,pt,wt,bf_save,c)
         n_pt = size(pt,1)

         ! Overlapping atoms
         near_atom = self%overlapping_atoms_for_atom(c)%element

         ! Make stockholder weight function "Wc" for atom "c"
         ! If using interpolatators, then on overlapping atoms
         ! are used ... otherwise nothing is skipped
         call apply_stockholder_atom_weight_(self,wt,c,pt)

         ! Prune the grid with new "wt", get saved bf grid bf_save,
         ! and new no of points, n_pt -- leaky
         call prune_grid_(self%becke_grid,pt,wt,bf_save)
         n_pt = size(pt,1)

         ! Basis function grid and skip list
         call create_(bf_skip,self%n_shell)
         call create_(bf_grid,self%n_shell)

         ! Harmonic temperature factor (for each reflection)
         call put_ADP2_vector_to_(self%atom(c),adp2)
         call create_(tf2,n_k)
         call to_contraction_i2_of_(tf2,k_pt,adp2)
         tf2 = exp(-HALF*tf2)

         ! Die if there are only ADP3s
         DIE_IF(has_only_ADP3s_and_errors_(self%atom(c)),"MOLECULE.SCF:make_H_r_constraint ... only ADP3s with HA XCW: unimplemented")

         ! Anharmonic temperature factor?
         if (has_only_ADP4s_and_errors_(self%atom(c))) then

            ! Get anharmonic constants
            call put_ADP3_vector_to_(self%atom(c),adp3)
            call put_ADP4_vector_to_(self%atom(c),adp4)

            ! Temperature factor contributions
            call create_(tfc,n_k)
            call create_(tf3,n_k)
            call create_(tf4,n_k)

            ! Harmonic and anharmonic temperature factors (for each reflection)
            call to_contraction_i2_of_(tf3,k_pt,adp3)
            call to_contraction_i2_of_(tf4,k_pt,adp4)
            tfc = tf2*SIXTH*cmplx(SIX+QUARTER*tf4,-tf3,kind=CPX_KIND)

            ! Get effective density "X"
            call create_(X,n_pt)
            if (fitted) then
               call create_(coef,n_k)
               call make_unique_sf_coefficients_(self%crystal%spacegroup,coef,tfc,Fcr,Fci,self%crystal%xray_data%reflections)
               call make_RI_exponents_(self,ri_alpha,c)
               call fitted_exp_ikr_transpose_(X,k_pt,coef,pt,self%atom(c)%position,ri_alpha,self%crystal%xray_data%RI_l_max)
               call destroy_(ri_alpha)
               call destroy_(coef)
            else
               call sum_unique_sf_pts_(self%crystal%spacegroup,X,k_pt,pt,tfc,Fcr,Fci,self%crystal%xray_data%reflections)
            end if

            ! Fold effective density "X" into "wt"
            wt = rf*wt*X

            ! Clean
            call destroy_(X)
            call destroy_(tf4)
            call destroy_(tf3)
            call destroy_(tfc)
            call destroy_(tf2)
         else

            ! Get effective density "X"
            call create_(X,n_pt)
            if (fitted) then
               call create_(coef,n_k)
               call create_(tfc,n_k)
               tfc = tf2
               call make_unique_sf_coefficients_(self%crystal%spacegroup,coef,tfc,Fcr,Fci,self%crystal%xray_data%reflections)
               call destroy_(tfc)
               call make_RI_exponents_(self,ri_alpha,c)
               call fitted_exp_ikr_transpose_(X,k_pt,coef,pt,self%atom(c)%position,ri_alpha,self%crystal%xray_data%RI_l_max)
               call destroy_(ri_alpha)
               call destroy_(coef)
            else
               call sum_unique_sf_pts_(self%crystal%spacegroup,X,k_pt,pt,tf2,Fcr,Fci,self%crystal%xray_data%reflections)
            end if

            ! Fold effective density "X" into "wt"
            wt = rf*wt*X

            ! Clean
            call destroy_(X)
            call destroy_(tf2)
         end if

         ! Loop c-overlapping atoms a
         do ca = 1,size(near_atom)
            oa = near_atom(ca)
            fsa = self%first_shell_for_atom(oa)
            lsa = self%last_shell_for_atom(oa)
            fba = self%first_basis_fn_for_atom(oa)
            ra = self%atom(oa)%position

            ! Make the displaced points for atom "a"
            call make_displaced_skip_pts_(self%atom(oa),xa,ya,za,a2,skipa0,pt,n_pt,cutoff)

            ! Loop shells on atom a
            do sa = fsa,lsa

               ! Shell a limits
               fa = self%first_basis_fn_for_shell(sa)
               la = self%last_basis_fn_for_shell(sa)
               na = la-fa+1
               as = self%basis_shell_for_shell(sa)

               ! Set shell "sa"
               sha = self%basis_shell1pair(as,1)%a
               sha%position = ra

               ! Make shell "sa" skip list
               call create_(skipa,n_pt)
               call make_skip_list_(sha,skipa,n_keep,a2,skipa0,n_pt,cutoff)

               ! Next shell "sa" if no sig. points
               if (n_keep==0) then
                  call destroy_(skipa)
                  cycle
               end if

               ! Assign the "sa" skipa list
               bf_skip(sa)%element = skipa

               ! Make/copy the "sa" basis function grid
               ! This must be copied since skipa could be TRUE
               call create_(bf_grid(sa)%element,n_keep,na)
               if (oa==c) then
                  do a = 1,na
                     i = 0
                     do n = 1,n_pt
                        if (skipa(n)) cycle
                        i = i+1
                        bf_grid(sa)%element(i,a) = bf_save(n,a+fa-fba)
                     end do
                  end do
               else
                  call make_skip_grid_(sha,bf_grid(sa)%element,n_keep,xa,ya,za,a2,skipa,n_pt)
               end if

               ! Loop c-overlapping atoms b
               do cb = 1,ca
                  ob = near_atom(cb)
                  fsb = self%first_shell_for_atom(ob)
                  lsb = self%last_shell_for_atom(ob)

                  ! Loop shells on atom b
                  do sb = fsb,min(lsb,sa)

                     ! No significant points
                     if (NOT allocated(bf_skip(sb)%element)) cycle

                     ! Shell b limits
                     fb = self%first_basis_fn_for_shell(sb)
                     lb = self%last_basis_fn_for_shell(sb)
                     nb = lb-fb+1

                     ! Set the "sb" basis function grid
                     skipb = bf_skip(sb)%element

                     ! How many points in common?
                     skipab(1:n_pt) = skipa OR skipb
                     n_keep = count(NOT skipab(1:n_pt))
                     if (n_keep==0) cycle

                     ! Product of ga and gb and effective density for "c"
                     ! Add the contribution to constraint "C"
                  ! CC = constraint(fa:la,fb:lb)
                     if (na*nb==1) then
                        ga = bf_grid(sa)%element(:,1)
                        gb = bf_grid(sb)%element(:,1)
                        val = ZERO
                        i = 0; j = 0
                        do n = 1,n_pt
                           if (NOT skipa(n)) i = i+1
                           if (NOT skipb(n)) j = j+1
                           if (skipab(n)) cycle
                           val = val+ga(i)*gb(j)*wt(n)
                        end do
                        constraint(fa,fb) = constraint(fa,fb)+val
                     else

                        ! Create space for gathered index info
                        call create_(pi,n_keep)
                        call create_(pj,n_keep)
                        call create_(pn,n_keep)

                        ! Make mapper arrays
                        p = 0; i = 0; j = 0
                        do n = 1,n_pt
                           if (NOT skipa(n)) i = i+1
                           if (NOT skipb(n)) j = j+1
                           if (skipab(n)) cycle
                           p = p+1
                           pi(p) = i
                           pj(p) = j
                           pn(p) = n
                        end do
                        do a = 1,na
                           ga = bf_grid(sa)%element(:,a)
                           do b = 1,nb
                              gb = bf_grid(sb)%element(:,b)
                              val = ZERO
                              do p = 1,n_keep
                                 i = pi(p)
                                 j = pj(p)
                                 n = pn(p)
                                 val = val+ga(i)*gb(j)*wt(n)
                              end do
                              constraint(fa-1+a,fb-1+b) = constraint(fa-1+a,fb-1+b)+val
                           end do
                        end do

                        ! Clean up
                        call destroy_(pn)
                        call destroy_(pj)
                        call destroy_(pi)
                     end if
                  end do
               end do
            end do
         end do

         ! Clean up atom "c" stuff
         call destroy_(bf_grid)
         call destroy_(bf_skip)
         call destroy_(bf_save)
         call destroy_(wt)
         call destroy_(pt)
      end do  ! -- loop over atom "c"
      UNLOCK_PARALLEL_DO("MOLECULE.SCF:make_H_r_constraint")

      ! Clean up
      call destroy_(skipab)
      call destroy_(skipa0)
      call destroy_(a2)
      call destroy_(za)
      call destroy_(ya)
      call destroy_(xa)
      call destroy_(k_pt)
      call destroy_(Fci)
      call destroy_(Fcr)

      ! Symmetrise
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(constraint)
      else
         call symmetric_reflect_(constraint)
      end if

      ! Get rid of OO block if requested
      if (self%SCF_data%zero_oo_constraint_block) then
         call change_basis_using_(constraint,self%MOs%r)
         constraint(1:self%n_a,1:self%n_a) = ZERO
         call back_transform_using_(constraint,self%MOs%r)
      end if
   end subroutine

   subroutine make_pnd_constraint(self,C)
   ! Make the constraint matrix "C" for restricted wavefunction fitting.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), INOUT :: C
      MAT(REAL,:,:), allocatable :: k_pts
      MAT3(CPX,:,:,:), allocatable :: ft_ab_eq,ft_ab
      VEC(CPX,:), allocatable :: Fc,fac_times_alpha
      VEC(REAL,:), allocatable :: Fexp,Fsig,alpha
      REAL :: fac,cutoff
      INT :: q,fa,la,fb,lb,na,nb
      INT :: i,j,atom_a,atom_b
      INT :: n_k,n_refl,n_par
      type(SHELL2_TYPE) :: sh
   ENSURE(allocated(self%crystal),"MOLECULE.SCF:make_pnd_constraint ... no crystal info")
   ENSURE(self%crystal%spacegroup%centrosymmetric,"MOLECULE.SCF:make_pnd_constraint ... crystal must be centrosymmetric")
   DIE_IF(NOT self%crystal%spacegroup%centrosymmetric,"MOLECULE.SCF:make_pnd_constraint ... crystal must be centrosymmetric")
      n_k = n_unique_SF_k_pts_(self%crystal)
      n_refl = size(self%crystal%xray_data%reflections)
      n_par = self%crystal%xray_data%n_param
      fac = -G_FACTOR/(TWO*max(n_refl-n_par,1))
      call create_(fac_times_alpha,n_refl)
      call create_(Fc,n_refl)
      Fc = self%crystal%xray_data%reflections%F_calc
      call create_(alpha,n_refl)
      alpha = F_scale_and_extn_correction_(self%crystal%xray_data)
      call create_(Fexp,n_refl); Fexp = self%crystal%xray_data%reflections%F_exp
      call create_(Fsig,n_refl); Fsig = self%crystal%xray_data%reflections%F_sigma
      fac_times_alpha = fac*alpha*(alpha*Fc-Fexp)/(Fsig*Fsig)
      call destroy_(Fsig)
      call destroy_(Fexp)
      call destroy_(alpha)
      call destroy_(Fc)
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)
      cutoff = TOL(10)/self%n_shell_pairs
      C = ZERO
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.SCF:make_pnd_constraint")
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (skip_ft_(sh,cutoff)) then
            call destroy_ptr_part_(sh)
            cycle
         end if
         call create_(ft_ab,[1,n_refl],[fa,la],[fb,lb])
         call create_(ft_ab_eq,n_k,na,nb)
         call make_FT_pair_(self,ft_ab_eq,k_pts,sh,atom_a,atom_b)
         call sum_unique_sf_ints_(self%crystal%spacegroup,ft_ab,ft_ab_eq,self%crystal%xray_data%reflections)
         call destroy_(ft_ab_eq)
         do i = fa,la
            do j = fb,min(lb,i)
               C(i,j) = sum_elements_(RE(fac_times_alpha(:)*ft_ab(:,i,j)))
            end do
         end do
         call destroy_(ft_ab)
         call destroy_ptr_part_(sh)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.SCF:make_pnd_constraint")
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(C)
      else
         call symmetric_reflect_(C)
      end if
      call destroy_(k_pts)
      call destroy_(fac_times_alpha)
   end subroutine

   subroutine reset_for_next_lambda(self)
   ! Reset the iteration counters for the next lambda iteration.
   ! Also reset the fock matrices on disk.
      type(MOLECULE_TYPE), INOUT :: self
      call make_overlap_mx_(self)  ! why this?
      call reset_for_next_lambda_(self%SCF_data)
   end subroutine


!  ============
!  Embedded SCF
!  ============

!   embedded_scf :: leaky
!   ! Do an SCF calculation for a molecular fragment embedded in a crystal
!   ! generated cluster. The .MOs and .MO_energies are
!   ! obtained for the embedded fragment. The .density_mx is obtained for the
!   ! whole cluster. There must be a pre-existing cluster, and self must be the
!   ! same as this cluster. Also, there must be a .saved molecule representing
!   ! the embedded fragment whose orbitals are obtained.
!   ENSURE(.basis_info_made,"no basis info")
!   ENSURE(.atom.allocated,"no atom list")
!   ENSURE(.cluster.allocated,"no cluster")
!   ENSURE(.saved.allocated,"no saved fragment molecule")
!   ENSURE(.saved.MOs.allocated,"no saved molecular orbitals")
!     .initialize_scf
!     .put_SCF_results
!     do
!       if (.SCF_data.SCF_done) exit
!       .add_cluster_projector(.fock_mx,.MOs)
!       .accelerate_Fock_mx
!       ! Get back the fragment MO's
!       .unarchive_MOs
!       .update_MOs
!       ! Symmetry transform the occupied MO's around the crystal cluster.
!       ! This stuffs up the fragment MO's.
!       .transform_embedded_orbitals(.MOs,do_virtual=FALSE)
!       ! This is the cluster density matrix
!       .make_SCF_density_mx(n_a=.saved.n_a*.cluster.n_fragment_symop)
!       ! This is the cluster fock matrix
!       .make_scf_Fock_mx
!       ! Now make the fragment density matrix, and update info
!       .make_SCF_density_mx(n_a=.saved.n_a,damp=FALSE)
!       .update_SCF_data
!       .update_SCF_data_error
!       .put_SCF_results
!     end
!     .archive_SCF_results
!     .cleanup_scf
!   end
!
!   initialize_embedded_scf :: leaky
!   ! Initialise the embedded SCF procedure
!   ENSURE(.SCF_data.allocated,"no SCF_data")
!   ENSURE(.saved.allocated,"no saved molecule")
!   ENSURE(.cluster.allocated,"no cluster")
!   ENSURE(.cluster.n_fragment_symop>0,"no cluster fragment symops")
!   ENSURE(.n_bf==.saved.n_bf*.cluster.n_fragment_symop,"cluster is not a whole multiple of the fragment")

!      if (.SCF_data.is_DFT_calculation) .initialize_DFT_grids
!      ! Initialize and archive the MO's
!      .initialize_embedded_orbitals
!      .transform_embedded_orbitals(.MOs,do_virtual=TRUE)
!      .put_archive(.MOs,"MOs",with_lambda=TRUE)
!      ! This is the cluster density matrix
!      .make_SCF_density_mx(n_a=.saved.n_a*.cluster.n_fragment_symop,damp=FALSE)
!      ! This is the cluster fock matrix
!      .make_scf_Fock_mx
!      ! Now we have back the fragment density matrix
!      .make_SCF_density_mx(n_a=.saved.n_a,damp=FALSE)
!      .initialize_SCF_data
!      .initialize_SCF_data_error
!      .SCF_data.SCF_DIIS.cleanup

!   end
!
!   initialize_embedded_orbitals :: leaky
!   ! Initialise the embedded cluster .molecular orbitals from those in the
!   ! .saved fragment. This does not do any symmetry transformation.
!   ENSURE(.SCF_data.allocated,"no SCF_data")
!   ENSURE(.saved.allocated,"no saved molecule")
!   ENSURE(.cluster.allocated,"no cluster")
!   ENSURE(.cluster.n_fragment_symop>0,"no cluster fragment symops")
!   ENSURE(.n_bf==.saved.n_bf*.cluster.n_fragment_symop,"cluster is not a whole multiple of the fragment")
!      nsb,nso,nsv,no,nv :: INT
!      nsb = .saved.n_bf
!      nso = .saved.n_a
!      nsv = nsb - nso
!      no  = nso*.cluster.n_fragment_symop
!      nv  = nsv*.cluster.n_fragment_symop
!      select case (.SCF_data.SCF_kind)
!         case ("embedded_rhf","crystal_rhf")
!            .MOs.destroy("r ")
!            .MOs.r.create(.n_bf,.n_bf)
!            .MOs.r = ZERO
!            .MOs.r(1:nsb,1:nso) = .saved.MOs.r(:,1:nso)
!            .MOs.r(1:nsb,no+1:no+nsv) = .saved.MOs.r(:,nso+1:nso+nsv)
!            .MO_energies.create(genre=.SCF_data.MO_energies_kind)
!         case default
!            DIE("unknown or unimplemented SCF kind, "//trim(.SCF_data.SCF_kind))
!      end
!   end
!
!   transform_embedded_orbitals(MO,do_virtual)
!   ! Transform the (.saved) embedded molecular orbitals "MO" into the crystal
!   ! orbitals over the cluster (i.e. this molecule) using crystal symmetry
!   ! operations.
!      MO :: OPMATRIX@
!      do_virtual :: BIN, optional
!   ENSURE(.SCF_data.allocated,"no SCF_data")
!   ENSURE(.saved.allocated,"no SCF_data")
!      select case (.SCF_data.SCF_kind)
!         case ("embedded_rhf","crystal_rhf")
!            .transform_embedded_orbitals_r(MO.r,do_virtual)
!         case default
!            DIE("unknown or unimplemented SCF kind, "//trim(.SCF_data.SCF_kind))
!      end
!   end
!
!   transform_embedded_orbitals_r(MO,do_virtual)
!   ! Transform the (.saved) embedded molecular orbitals "MO" into the crystal
!   ! orbitals over the cluster (i.e. this molecule) using crystal symmetry
!   ! operations.
!      MO :: MAT{REAL}@
!      do_virtual :: BIN, optional
!   ENSURE(MO.is_square,"MO is not square")
!   ENSURE(MO.dim1==.n_bf,"wrong size for MO")
!   ENSURE(.saved.allocated,"no saved molecule")
!   ENSURE(.cluster.allocated,"no cluster")
!   ENSURE(.cluster.n_fragment_symop>0,"no cluster fragment symops")
!   ENSURE(.n_bf==.saved.n_bf*.cluster.n_fragment_symop,"cluster is not a whole multiple of the fragment")
!      seitz :: MAT3{REAL}@
!      tr :: VEC{MAT3_{REAL}}@
!      nso,nsv,no,nv,q,op,fo,lo,fv,lv,s,a,fs,ls,sf,sl,p,fp,fa,L :: INT
!      do_virt :: BIN
!      do_virt = FALSE
!      if (present(do_virtual)) do_virt = do_virtual
!      seitz = .crystal.xyz_seitz_matrices
!      tr.make_gaussian_xyz_matrices(seitz)
!      nso = .saved.n_a
!      nsv = .saved.n_bf - nso
!      no  = nso*.cluster.n_fragment_symop
!      nv  = nsv*.cluster.n_fragment_symop
!      ! Assume the occupied neighbouring orbitals occur as the end columns (due
!      ! to the use of a projection operator of high energy)
!    ! MO(:,no+1:no+nv) = MO(:,nso+1:nso+nv)
!      MO(:,nso+1:no) = ZERO                    ! to be defined below
!      if (do_virt) MO(:,no+nsv+1:no+nv) = ZERO ! to be defined below
!      lo = nso
!      lv = no+nsv
!      do q = 2,.cluster.n_fragment_symop                  ! Loop over generating cluster symops
!         op  = .cluster.fragment_symop(1,q)               ! The seitz op for the cluster symop
!         fo = lo + 1
!         lo = lo + nso
!         fv = lv + 1
!         lv = lv + nsv
!         do s = 1,.n_shell                                ! Loops over shells s in the row i.e. ao's.
!            a = .atom_for_shell(s)                        ! The atom for shell s
!            p  = .cluster.fragment_parents_for_atom(a,q)  ! Which atom (not necesarily fragment) did "a" come from?
!            if (p==0) cycle                               ! No parent? then cycle
!            fs = .first_basis_fn_for_shell(s)
!            ls = .last_basis_fn_for_shell(s)
!            fa = .first_basis_fn_for_atom(a)
!            fp = .first_basis_fn_for_atom(p)
!            sf = fp + fs - fa                             ! first function for embedded shell
!            sl = fp + ls - fa                             ! last  function for embedded shell
!            L  = .angular_moment_for_shell(s)
!            MO(fs:ls,fo:lo).to_product_of(tr(L)[:,:,op],MO(sf:sl,1:nso))

!            MO(fs:ls,fv:lv).to_product_of(tr(L)[:,:,op],MO(sf:sl,no+1:no+nsv))
!         end
!      end
!      tr.destroy
!      seitz.destroy
!   end
!
!   collapse_crystal_orbitals_r(MO) :: leaky
!   ! Collapse the crystal orbitals back into the (.saved) embedded fragment
!   ! orbitals.
!      MO :: MAT{REAL}@
!   ENSURE(MO.dim1==.n_bf,"wrong dim1 for MO")
!   ENSURE(MO.dim2==.n_bf,"wrong dim1 for MO")
!      nso,nsv,no,nv :: INT
!      nso = .saved.n_a
!      nsv = .saved.n_bf - nso
!      no  = nso*.cluster.n_fragment_symop
!      nv  = nsv*.cluster.n_fragment_symop
!      MO(:,nso+1:nso+nsv) = MO(:,no +1:no+nsv)
!      MO.shrink_columns(nso+nsv)
!   end
!
!   add_cluster_projector(F,MO)
!   ! Add the cluster projector to the fock matrix "F" using the molecular
!   ! orbitals "MO. This ensures that the occupied orbitals are orthogonal to the
!   ! occupied orbitals of neighbouring molecules.
!     F,MO :: OPMATRIX@
!   ENSURE(.SCF_data.allocated,"no SCF_data")
!   ENSURE(F.allocated,"no Fock matrix")
!   ENSURE(F.has_any_genre,"no Fock matrix")
!   ENSURE(F.genre==.SCF_data.spinorbital_genre,"incompatible Fock matrix")
!     P,S :: MAT{REAL}@
!     k,l :: INT
!     SCF_kind :: STR
!     SCF_kind = .SCF_data.SCF_kind
!     select case (SCF_kind)
!        case ("rhf","xray_rhf","embedded_rhf","crystal_rhf")
!           k = .saved.n_a + 1
!           l = .saved.n_a*.cluster.n_fragment_symop
!           P.create(.n_bf,.n_bf)
!           S.create(.n_bf,.n_bf); .make_r_overlap_mx(S)
!           P.to_product_of(MO.r(:,k:l),MO.r(:,k:l),transpose_b=TRUE)
!           P.change_basis_using(S)
!           .fock_mx.r = .fock_mx.r + .SCF_data.kappa*P
!           S.destroy
!           P.destroy
!        case default
!           DIE("unknown or unimplemented SCF kind, "//trim(SCF_kind))
!     end
!   end

!   transform_embedded_orbitals(MO)
!   ! Transform the embedded orbitals "MO".
!      MO :: MAT{REAL}
!      seitz :: MAT3{REAL}@
!      tr :: VEC{MAT3_{REAL}}@
!      n_embedded_bf,n_embedded_shell :: INT
!      i,ai,fi,li,if,il,j,aj,fj,lj,jf,jl, q,u,f,p,fa,fp,L :: INT
!      n_embedded_bf = .saved.n_bf
!      n_embedded_shell = .saved.n_shell
!      seitz = .crystal.xyz_seitz_matrices
!      tr.make_gaussian_xyz_matrices(seitz)
!      do i = n_embedded_shell+1,.n_shell                  ! Loop over group i of shells (orbitals) in column
!         ai = .atom_for_shell(i)                          ! The atom that the orbitals in group i belong to
!         q  = .cluster.fragment_symop_for_atom(ai)        ! Which symop generated these shells?
!     !   u  = .cluster.unit_cell_atom_for_atom(ai)
!     !   f  = .crystal.frag_cell_for_unit_cell_atom(u)
!     !   p  = .crystal.frag_atom_for_frag_cell_atom(f)    ! Which fragment atom did "ai" come from?
!         p  = .cluster.fragment_parents_for_atom(ai,q)    ! Which fragment atom did "ai" come from?
!         fi = .first_basis_fn_for_shell(i)
!         li = .last_basis_fn_for_shell(i)
!         fa = .first_basis_fn_for_atom(ai)
!         fp = .saved.first_basis_fn_for_atom(p)
!         if = fp + fi - fa                                ! first function for embedded shell
!         il = fp + li - fa                                ! last  function for embedded shell
!     !      stdout.text("======================================")
!     !      stdout.show("shell i                  =",i)
!     !      stdout.show("atom ai for shell i      =",ai)
!     !      stdout.show("parent p for atom a      =",p)
!     !      stdout.show("clust symop q for atom a =",q)
!     !      stdout.show("first fn for shell i, fi =",fi)
!     !      stdout.show("last  fn for shell i, li =",li)
!     !      stdout.show("first fn for atom a,  fa =",fa)
!     !      stdout.show("first fn for atom p,  fp =",fp)
!     !      stdout.show("first embedded fn,    if =",if)
!     !      stdout.show("last  embedded fn,    il =",il)
!         do j = 1,.n_shell                                ! Loops over shells s in the row i.e. ao's.
!            aj = .atom_for_shell(j)                       ! The atom for shells s
!            p  = .cluster.fragment_parents_for_atom(aj,q) ! Which atom (not necesarily fragment) did "a" come from?
!            if (p==0) cycle
!            fj = .first_basis_fn_for_shell(j)
!            lj = .last_basis_fn_for_shell(j)
!            fa = .first_basis_fn_for_atom(aj)
!            fp = .first_basis_fn_for_atom(p)
!            jf = fp + fj - fa                             ! first function for embedded shell
!            jl = fp + lj - fa                             ! last  function for embedded shell
!            L  = .angular_moment_for_shell(j)
!     !      stdout.flush
!     !      stdout.show("shell j                  =",j)
!     !      stdout.show("atom aj for shell j      =",aj)
!     !      stdout.show("parent p for atom a      =",p)
!     !      stdout.show("clust symop q for atom a =",q)
!     !      stdout.show("first fn for shell j, fj =",fj)
!     !      stdout.show("last  fn for shell j, lj =",lj)
!     !      stdout.show("first fn for atom a,  fa =",fa)
!     !      stdout.show("first fn for atom p,  fp =",fp)
!     !      stdout.show("first embedded fn,    jf =",jf)
!     !      stdout.show("last  embedded fn,    jl =",jl)
!     !      stdout.flush
!            MO(fj:lj,fi:li).to_product_of(tr(L)[:,:,q],MO(jf:jl,if:il))
!         end
!      end
!      tr.destroy
!      seitz.destroy
!   end

!  ===========
!  Crystal SCF
!  ===========
!
!   crystal_scf :: leaky
!   ! Do an SCF calculation for a molecular fragment embedded in a crystal
!   ! generated cluster. The .MOs and .MO_energies are
!   ! obtained for the embedded fragment. The .density_mx is obtained for the
!   ! whole cluster. There must be a pre-existing cluster, and self must be the
!   ! same as this cluster. Also, there must be a .saved molecule representing the
!   ! embedded fragment whose orbitals are obtained.
!   ENSURE(.basis_info_made,"no basis info")
!   ENSURE(.atom.allocated,"no atom list")
!   ENSURE(.cluster.allocated,"no cluster")
!   ENSURE(.saved.allocated,"no saved fragment molecule")
!   ENSURE(.saved.MOs.allocated,"no saved molecular orbitals")
!     .initialize_scf
!     .put_SCF_results
!     do
!       if (.SCF_data.SCF_done) exit
!       .add_cluster_projector(.fock_mx,.MOs)
!       .accelerate_Fock_mx
!       ! Get back the fragment MO's
!       .unarchive_MOs
!       .update_MOs
!       ! Symmetry transform the occupied MO's around the crystal cluster.
!       ! This stuffs up the fragment MO's.
!       .transform_embedded_orbitals(.MOs,do_virtual=FALSE)
!       ! This is the cluster density matrix
!       .make_SCF_density_mx(n_a=.saved.n_a*.cluster.n_fragment_symop)
!       ! This is the cluster fock matrix
!       .make_scf_Fock_mx
!       ! Now make the fragment density matrix, and update info
!       .make_SCF_density_mx(n_a=.saved.n_a,damp=FALSE)
!       .update_SCF_data
!       .update_SCF_data_error
!       .put_SCF_results
!     end
!     .archive_SCF_results
!     .cleanup_scf
!   end
!
!   initialize_crystal_scf :: leaky
!   ! Initialise the crystal SCF procedure
!   ENSURE(.SCF_data.allocated,"no SCF_data")
!   ENSURE(.saved.allocated,"no saved molecule")
!   ENSURE(.cluster.allocated,"no cluster")
!   ENSURE(.cluster.n_fragment_symop>0,"no cluster fragment symops")
!   ENSURE(.n_bf==.saved.n_bf*.cluster.n_fragment_symop,"cluster is not a whole multiple of the fragment")
!      ! Initialise and archive the MO's
!      .initialize_embedded_orbitals
!      .transform_embedded_orbitals(.MOs,do_virtual=TRUE)
!      .put_archive(.MOs,"MOs",with_lambda=TRUE)
!      ! This is the cluster density matrix
!      .make_SCF_density_mx(n_a=.saved.n_a*.cluster.n_fragment_symop,damp=FALSE)
!      ! This is the cluster fock matrix
!      .make_scf_Fock_mx
!      ! Now we have back the fragment density matrix
!      .make_SCF_density_mx(n_a=.saved.n_a,damp=FALSE)
!      .initialize_SCF_data
!      .initialize_SCF_data_error
!      .SCF_data.SCF_DIIS.cleanup
!   end
!
!   set_crystal_SCF_occupations
!   ! Set the SCF occupation numbers for the natural orbitals
!   ENSURE(.saved.allocated,"no saved molecule fragment")
!      NO_kind :: STR
!      n_a,n_b,n_e :: INT
!      n_a = .saved.n_a
!      n_b = .saved.n_b
!      n_e = .saved.n_e
!      NO_kind = .NOs.genre
!      select case (NO_kind)
!         case ("r ")
!            .NO_occ_nos.r(1:n_a) = TWO
!         case ("u ")
!            .NO_occ_nos.a(1:n_a) = ONE
!            .NO_occ_nos.b(1:n_b) = ONE
!      end
!   end
!
!   make_effective_charges(charges,positions)
!   ! Make a list of effective point charges that accurately represents the total
!   ! charge of the unit cell.  Includes contributions from electrons and nuclei.
!   ! NOTE: this is normally called on a .saved fragment
!     charges :: VEC{REAL}, target
!     positions :: MAT{REAL}, target
!   ENSURE(charges.dim==(.dftgrid.n_pts+1)*.n_atom,"charges has wrong size")
!   ENSURE(charges.dim==positions.dim2,"position and charges arrays are incorrect")
!   ENSURE(positions.dim1==3,"dimensions of position array are incorrect")
!   ENSURE(.atom.allocated,"Need a list of atoms")
!   ENSURE(.dftgrid.allocated,"no DFT grid")
!     ch,wt,rho,deficit :: VEC{REAL}@
!     pt :: MAT{REAL}@
!     shift,pos3 :: VEC{REAL}(3)
!     n_pt,n,i,f,l,ff,ll :: INT
!     Z,val :: REAL
!     ! Make the effective electronic charges "ch" and positions "pt"
!     n_pt = .dftgrid.n_pts*.n_atom
!     ch.create(n_pt); pt.create(n_pt,3)
!     wt.create(n_pt); rho.create(n_pt)
!     .dftgrid.make_grid(pt,wt,.atom)
!     .make_density_grid(rho,pt)
!     ch = -rho*wt
!     rho.destroy; wt.destroy
!   ! stdout.set_real_style("e")
!   ! stdout.text("charges:")
!   ! stdout.put(ch)
!   ! stdout.text("pt:")
!   ! stdout.put(pt)
!     ! First get the atomic charges, which are assigned to balance the
!     ! electronic charge in case the DFT integration grid is poor.
!     deficit.create(.n_atom)
!     n_pt = .dftgrid.n_pts
!     l = 0
!     do n = 1,.n_atom
!        f = l + 1
!        l = l + n_pt
!        Z = ZERO
!        do i = f,l
!           Z = Z + ch(i)
!        end
!        deficit(n) = -Z
!     end
!     ! Now set the full "charges" and "positions" arrays, including the
!     ! (corrected) atomic charges, at the end of the list
!     l = 0
!     ll = 0
!     do n = 1,.n_atom
!        f  = l  + 1
!        l  = l  + n_pt
!        ff = ll + 1
!        ll = ll + n_pt
!        charges(f:l) = ch(ff:ll)
!        positions(:,f:l) = transpose(pt(ff:ll,:))
!        l = l + 1
!        charges(l) = deficit(n)
!        deficit(n) = .atom(n).atomic_number - deficit(n)
!        positions(:,l) = .atom(n).position
!     end
!     stdout.set_real_style("e")
!     stdout.flush
!     stdout.text("Deficit charges:")
!     stdout.put(deficit)
!     stdout.show("sum of deficit charges =", VEC{REAL}:sum_elements(deficit))
!     stdout.show("sum of charges         =", VEC{REAL}:sum_elements(charges))
!     deficit.destroy
!   ! stdout.flush
!   ! stdout.text("Charges:")
!   ! stdout.flush
!   ! stdout.put(charges)
!   ! stdout.flush
!   ! stdout.text("Positions:")
!   ! stdout.flush
!   ! stdout.put(positions,"transpose")
!     stdout.set_real_style("f")
!     pt.destroy; ch.destroy
!   end

!   make_uc_effective_charges(charges,positions)
!   ! Make a list of effective point charges that accurately represents the total
!   ! charge of the unit cell.  Includes contributions from electrons and nuclei.
!     charges :: VEC{REAL}, target
!     positions :: MAT{REAL}, target
!   ENSURE(charges.dim==(.dftgrid.n_pts+1)*.n_atom,"charges has wrong size")
!   ENSURE(charges.dim==positions.dim2,"position and charges arrays are incorrect")
!   ENSURE(positions.dim1==3,"dimensions of position array are incorrect")
!   ENSURE(.atom.allocated,"Need a list of atoms")
!   ENSURE(.saved.dftgrid.allocated,"no DFT grid")
!     ch,wt,rho,deficit :: VEC{REAL}@
!     pt :: MAT{REAL}@
!     shift,pos3 :: VEC{REAL}(3)
!     n_pt,n,i,f,l,ff,ll :: INT
!     ZZ,val :: REAL
!
!     pos,new_pos :: MAT{REAL}@
!     Z,n_ch,n_pt,n,f,l,s :: INT
!
!     stdout.text("making saved dft grid")
!     n_ch = .saved.dftgrid.n_pts
!     n_pt = .saved.dftgrid.n_pts
!     ch.create(n_pt)
!     pos.create(3,n_pt)
!     .saved.dftgrid.make_grid(pos,wt,.saved.atom)
!     ! Change fragment positions into fractionals
!     .crystal.unit_cell.change_into_fractional(pos)
!     new_pos.create(pos.dim1,pos.dim2)
!     ! Use symmetry to generate the effective charges and their positions in the
!     ! whole unit cell. This assumes that the fragment cell is generated from
!     ! the *whole* fragment by .crystal.Z symops.
!     l = 0
!     do n = 1,Z
!        f = l + 1
!        l = l + n_ch
!        .charges(f:l) = .fragment_charges
!        new_pos = pos
!        s = .crystal.symop_for_frag_cell_atom(1+(n-1)*.crystal.n_fragment_atoms)
!        .crystal.transform_geometry(new_pos,op=s)
!        .crystal.put_to_unit_cell(new_pos)
!        .positions(:,f:l) = new_pos ! in cartesians
!     end
!     new_pos.destroy; pos.destroy
!
!     do n = 1,.n_atoms          ! Loop over atoms in whole cluster
!     do q = 1,.n_fragment_symop ! Loop over fragment symops
!        child = .geometry(:,n)  ! Get the image of the parent p under the fragment symop s
!        op = int(.fragment_symop(1,q))
!        tr = .fragment_symop(2:4,q)
!        .crystal.transform_position(child,op,tr)
!        if (NOT .geometry.has_column(child,col=c)) cycle
!        .fragment_parents_for_atom(c,q) = n
!     end
!     end
!
!     ! Make the effective electronic charges "ch" and positions "pt"
!     Z = .n_atom/.saved.n_atom
!     n_pt = .saved.dftgrid.n_pts*Z
!     stdout.show("n_pt =",n_pt)
!     ch.create(n_pt); pt.create(n_pt,3)
!     wt.create(n_pt); rho.create(n_pt)
!
!     stdout.text("made dft grid")
!     stdout.text("making density grid")
!     .make_density_grid(rho,pt)
!     stdout.text("made density grid")
!     ch = -rho*wt
!     rho.destroy; wt.destroy
!     stdout.set_real_style("e")
!     stdout.text("charges:")
!     stdout.put(ch)
!     stdout.text("pt:")
!     stdout.put(pt)
!     ! First get the atomic charges, which are assigned to balance the
!     ! electronic charge in case the DFT integration grid is poor.
!     deficit.create(.n_atom)
!     n_pt = .dftgrid.n_pts
!     l = 0
!     do n = 1,.n_atom
!        f = l + 1
!        l = l + n_pt
!        ZZ = ZERO
!        do i = f,l
!           ZZ = ZZ + ch(i)
!        end
!        deficit(n) = -ZZ
!     end
!     ! Now set the full "charges" and "positions" arrays, including the
!     ! (corrected) atomic charges, at the end of the list
!     l = 0
!     ll = 0
!     do n = 1,.n_atom
!        f  = l  + 1
!        l  = l  + n_pt
!        ff = ll + 1
!        ll = ll + n_pt
!        charges(f:l) = ch(ff:ll)
!        positions(:,f:l) = transpose(pt(ff:ll,:))
!        l = l + 1
!        charges(l) = deficit(n)
!        deficit(n) = .atom(n).atomic_number - deficit(n)
!        positions(:,l) = .atom(n).position
!     end
!     stdout.set_real_style("e")
!     stdout.flush
!     stdout.text("Deficit charges:")
!     stdout.put(deficit)
!     stdout.show("sum of deficit charges =", VEC{REAL}:sum_elements(deficit))
!     stdout.show("sum of charges         =", VEC{REAL}:sum_elements(charges))
!     deficit.destroy
!   ! stdout.flush
!   ! stdout.text("Charges:")
!   ! stdout.flush
!   ! stdout.put(charges")
!   ! stdout.flush
!   ! stdout.text("Positions:")
!   ! stdout.flush
!   ! stdout.put(positions,"transpose")
!     stdout.set_real_style("f")
!     pt.destroy; ch.destroy
!   end

!   make_unit_cell_point_charges(charges,positions)
!   ! Make a list of point charges that accurately represents the total charge of
!   ! the unit cell.  Includes contributions from electrons and nuclei.
!   ! NOTE: this is normally called on a .saved fragment
!     charges :: VEC{REAL}, target
!     positions :: MAT{REAL}, target
!   ENSURE(charges.dim==positions.dim2,"position and charges arrays are incorrect")
!   ENSURE(positions.dim1==3,"dimensions of position array are incorrect")
!   ENSURE(.atom.allocated,"Need a list of atoms")
!   ENSURE(.dftgrid.allocated,"no DFT grid")
!   ENSURE(.crystal.allocated,"no crystal data")
!   ENSURE(.crystal.Z.is_int(TOL(3)), "must have an integer number of fragments in the unit cell")
!   ENSURE(.crystal.frag_atom_for_frag_cell_atom.allocated,"fragment cell map not created")
!   ENSURE(charges.dim==(.dftgrid.n_pts+1)*.crystal.n_unit_cell_atoms,"charges has wrong size")
!     ch,wt,rho,deficit :: VEC{REAL}@
!     pos,pt,uc_pos :: MAT{REAL}@
!     shift,pos3 :: VEC{REAL}(3)
!     n_pt,Z,n,f,l,s :: INT
!     e_sum,n_sum :: REAL
!     ! Make the effective electronic charges for the fragment.
!     n_pt = .dftgrid.n_pts*.n_atom
!     deficit.create(.n_atom)
!     ch.create(n_pt); pos.create(3,n_pt)
!     wt.create(n_pt); pt.create(n_pt,3); rho.create(n_pt)
!     .dftgrid.make_grid(pt,wt,.atom)
!     .make_density_grid(rho,pt)
!     ch = -rho*wt
!     pos = transpose(pt)
!     ! Work out the deficit charge for each atom, which is the charge that can
!     ! be ascribed to the dft integration grid point at the atomic nucleus. It
!     ! should normally be negative because near the nucleus the density should
!     ! be evaluated in the midpoint of the region but it is evaluated closer to
!     ! the nucleus and hence is larger than it should be (?). Thats my
!     ! explanation anyhow === dylan
!     Z = .crystal.Z
!     n_pt = .dftgrid.n_pts
!     l = 0
!     do n = 1,.n_atom
!        f = l + 1
!        l = l + n_pt
!        deficit(n) = .atom(n).atomic_number +  VEC{REAL}:sum_elements(ch(f:l))
!     end
!     stdout.flush
!     stdout.text("Deficit charges:")
!     stdout.put(deficit)
!     stdout.show("sum of deficit charges =",  VEC{REAL}:sum_elements(deficit))
!     stdout.show("Z times sum of deficit =",Z*VEC{REAL}:sum_elements(deficit))
!     rho.destroy; pt.destroy; wt.destroy
!     ! When added to any fractional position, "shift" will convert that
!     ! position to be such that the center of the fragment molecule is at the
!     ! center of the unit cell.
!     shift = .center_of_atoms ! The fragment molecule center
!     .crystal.unit_cell.change_into_fractional(shift)
!     shift = [HALF,HALF,HALF] - shift
!     ! Use symmetry to generate the effective electronic charges and their
!     ! positions in the whole unit cell. This assumes that the fragment cell is
!     ! generated from the *whole* fragment by .crystal.Z symops.
!     .crystal.unit_cell.change_into_fractional(pos)
!     n_pt = .dftgrid.n_pts*.n_atom
!     l = 0
!     do n = 1,Z
!        f = l + 1
!        l = l + n_pt
!        charges(f:l) = ch
!        positions(:,f:l) = pos
!        s = .crystal.symop_for_frag_cell_atom(1+(n-1)*Z)
!        .crystal.transform_geometry(positions(:,f:l),op=s)
!        ! Shift the positions so that they are relative to the fragment molecule
!        ! center before changing to the unit cell -- so the unit cell surrounds
!        ! the central fragment molecule.
!        positions(:,f:l) = positions(:,f:l) + spread(shift,dim=2,ncopies=n_pt)
!        .crystal.put_to_unit_cell(positions(:,f:l))
!        positions(:,f:l) = positions(:,f:l) - spread(shift,dim=2,ncopies=n_pt)
!        .crystal.unit_cell.change_from_fractional(positions(:,f:l))
!     end
!     pos.destroy; ch.destroy
!     ! Sum electron charges
!     n_pt = n_pt*Z
!     e_sum = VEC{REAL}:sum_elements(charges(1:n_pt))
!     stdout.show("sum of electrn charges =",e_sum)
!     ! Add the nuclear charges to the list. Don't forget to add the deficit
!     ! charges.
!     do n = 1,.crystal.n_unit_cell_atoms
!        f = .crystal.frag_atom_for_unit_cell_atom(n)
!        charges(n_pt+n) = .atom(f).atomic_number - deficit(f)
!        pos3 = .atom(f).position
!        .crystal.unit_cell.change_into_fractional(pos3)
!        pos3 = pos3 + shift
!        .crystal.put_to_unit_cell(pos3)
!        pos3 = pos3 - shift
!        .crystal.unit_cell.change_from_fractional(pos3)
!        positions(:,n_pt+n) = pos3
!     end
!     deficit.destroy
!     n_sum = VEC{REAL}:sum_elements(charges(n_pt+1:))
!     stdout.show("sum of nuclear charges =",n_sum)
!   end

!  ================================
!  Orbital update/cleaning routines
!  ================================

   subroutine Schmidt_orthonormalize_MOs(self)
   ! Schmidt orthonormalize the ".MOs" and archive
      type(MOLECULE_TYPE), INOUT :: self
      call make_overlap_mx_(self)
      call Schmidt_orthonormalize_(self,self%MOs)
   end subroutine

   subroutine Schmidt_orthonormalize(self,MO,scale)
   ! Schmidt orthonormalize the molecular orbitals, "MO"
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: MO
      REAL, optional, OUT :: scale
      call make_overlap_mx_(self)
      call Schmidt_orthonormalize_(MO,self%overlap_mx,scale)
   end subroutine

   subroutine symmetrically_orthonormalize(self,MO)
   ! Symmetrically orthonormalize the molecular orbitals, "MO"
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: MO
      call make_overlap_mx_(self)
      call symmetrically_orthonormalize_(MO,self%overlap_mx)
   end subroutine

   subroutine update_MOs(self)
   ! Solve for the MOs. Requires a set of existing MOs!
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:update_MOs ... no SCF_data")
   ENSURE(allocated(self%MOs),"MOLECULE.SCF:update_MOs ... no old MO's")
   ENSURE(has_any_genre_(self%MOs),"MOLECULE.SCF:update_MOs ... no old MO's")
      call MO_eigen_update_(self)
      if (is_constrained_SCF_(self%SCF_data)) then
      end if
   end subroutine

   subroutine MO_eigen_update(self)
   ! Solve for the molecular orbitals.  Requires a set of old
   ! molecular orbitals.  The new molecular orbitals "cU" are found
   ! from the old MOs "c" by solving :
   !    (c^T F c)U = c^T S c U E 
   ! where U is an orthogonal matrix, F fock matrix.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%MOs),"MOLECULE.SCF:MO_eigen_update ... no old MO's")
   ENSURE(has_any_genre_(self%MOs),"MOLECULE.SCF:MO_eigen_update ... no old MO's")

      ! Save the reference M0s.
      !
      ! Normally these are the lambda=0 MOs. But a RESTART job begins from a
      ! stored density matrix at lambda>0 and never makes a lambda=0 pass, so
      ! this used to leave .M0s unallocated -- and the overlap below then
      ! dereferenced a null array descriptor and segfaulted. That was invisible
      ! for as long as the reference existed, because write_archive was eating
      ! the "scf" keyword before the SCF ever ran (see MOLECULE.PUT:put_archive).
      !
      ! For a restart we therefore seed .M0s with the MOs at the RESTART lambda.
      ! The overlap is then measured against that starting point rather than
      ! against lambda=0 -- see the note on M0s in types.foo.
      if (is_constrained_SCF_(self%SCF_data)) then
         if (is_zero_(self%SCF_data%lambda) OR NOT allocated(self%M0s)) then
            self%M0s = self%MOs  ! leaky
         end if
      end if

      ! Update the MO's
      select case (self%SCF_data%SCF_kind)

      case ("rhf                     ","rks                     ","rohf                    ","xray_rhf                ","xray_rks                ","noninteracting-group-rhf")
         call MO_r_eigen_update_(self,self%MO_energies%r,self%MOs%r,self%fock_mx%r)

      case ("uhf                     ","uks                     ","xray_uhf                ","xray_uks                ","pnd_uhf                 ","pnd_uks                 ","xray_pnd_uhf            ","xray_pnd_uks            ")
         call MO_r_eigen_update_(self,self%MO_energies%a,self%MOs%a,self%fock_mx%a)
         call MO_r_eigen_update_(self,self%MO_energies%b,self%MOs%b,self%fock_mx%b)

      case default
         DIE("MOLECULE.SCF:MO_eigen_update ... unknown scf kind, "//trim(self%SCF_data%SCF_kind))
      end select

      ! Calculate overlap with M0s
      if (is_constrained_SCF_(self%SCF_data)) then
         if (self%SCF_data%lambda>ZERO) then
            if (is_restricted_(self%SCF_data)) then

         ! Guard regardless: passing an unallocated .M0s to MO_M0_r_overlap
         ! dereferences a null array descriptor and segfaults with no message.
         ! The seeding above should make this unreachable, so if it ever fires
         ! the seeding logic is wrong -- say so rather than crashing.
               DIE_IF(NOT allocated(self%M0s),"MOLECULE.SCF:MO_eigen_update ... no reference M0s: a constrained-SCF overlap needs either a lambda=0 pass or a restart seed")
               self%SCF_data%overlap_with_M0s = MO_M0_r_overlap_(self,self%MOs%r,self%M0s%r)
            end if
         end if
      end if
   end subroutine

   function MO_M0_r_overlap(self,MO,M0) result (res)
   ! Return the overlap between the current MOs and another 
   ! set of M0s, usually these will be in a constrained SCF 
   ! calculation, with lambda>0. Intended for L-curve plots.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), IN :: MO,M0
      REAL :: res
      VEC(REAL,:), allocatable :: D
      MAT(REAL,:,:), allocatable :: S,L,R
      INT :: no
   ENSURE(is_square_(MO),"MOLECULE.SCF:MO_M0_r_overlap ... Wrong shape, MO")
   ENSURE(size(MO,1)==self%n_bf,"MOLECULE.SCF:MO_M0_r_overlap ... Wrong size, MO")
   ENSURE(is_same_shape_as_(MO,M0),"MOLECULE.SCF:MO_M0_r_overlap ... incompatible MOs")
      no = self%n_a
      call create_(S,no,no)
      call create_(D,no)
      call create_(L,no,no)
      call create_(R,no,no)

      ! Get cross-overlap
      call change_basis_to_(self%overlap_mx,S,MO(:,1:no),M0(:,1:no))

      ! Get SVD
      call singular_value_decomposition_(S,D,L,R)

      ! Overlap
      res = product(D)
      call destroy_(R)
      call destroy_(L)
      call destroy_(D)
      call destroy_(S)
   end function

   subroutine MO_r_eigen_update(self,MO_energies,MO,F)
   ! Solve for the new molecular orbital energies "MO_energies" and
   ! the new molecular orbitals "MO", given an initial old set (in
   ! "MO") and a fock matrix "F".  The new molecular orbitals "MO*U"
   ! are found from the old MOs "MO" by solving : 
   !   (c^T F c)U = c^T S c U E  .. where U is an orthogonal matrix.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: MO_energies
      MAT(REAL,:,:), INOUT :: MO
      MAT(REAL,:,:), IN :: F
      MAT(REAL,:,:), allocatable :: G,U
      INT :: i
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:MO_r_eigen_update ... no SCF_data")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.SCF:MO_r_eigen_update ... no overlap_mx")
   ENSURE(allocated(self%overlap_mx_evecs),"MOLECULE.SCF:MO_r_eigen_update ... no overlap_mx_evecs")
   ENSURE(allocated(self%overlap_mx_evals),"MOLECULE.SCF:MO_r_eigen_update ... no overlap_mx_evals")

      ! Copy F -> G
      call create_(G,self%n_bf,self%n_bf)
      G = F

      ! Change F into old MO basis, G
      call change_basis_using_(G,MO)

      ! Level shift in old MO basis
      if (apply_level_shifting_(self%SCF_data)) then
         do i = self%n_a+1,self%n_bf
            G(i,i) = self%SCF_data%level_shift+G(i,i)
         end do
      end if

      ! Get rotation U from old to new MO's
      call create_(U,self%n_bf,self%n_bf)
      call solve_symmetric_eigenproblem_(G,MO_energies,U)
   ! G.diagonalize_Jacobi(MO_energies,U)

      ! Lock orbitals in place?
      if (self%SCF_data%using_orbital_locking) call make_diagonally_dominant_(U)

      ! Update orbitals
      call to_product_of_(G,MO,U)

      ! Project linear dependence
   ! .left_project_dependence_r(G)

      ! Finished
      MO = G

      ! Clean up
      call destroy_(U)
      call destroy_(G)
   end subroutine


!   left_project_dependence_r(MO)
!   ! Left project the linear dependence from an "MO" like matrix
!   ! WARNING: this routine assumed overlap_mx_evals are ordered
!   ! smallest to largest.
!      MO :: MAT{REAL}
!
!   ENSURE(.SCF_data.allocated,"no SCF_data")
!   ENSURE(.overlap_mx.allocated,"no overlap_mx")
!   ENSURE(.overlap_mx_evecs.allocated,"no overlap_mx_evecs")
!   ENSURE(.overlap_mx_evals.allocated,"no overlap_mx_evals")
!   ENSURE(MO.is_square,"MO not square")
!   ENSURE(MO.dim1==.n_bf,"wrong size, MO")
!
!      P,W :: MAT{REAL}@
!      n,i :: INT
!      tol :: REAL
!
!      ! Return if all overlap matrix eigenvalues are big enough
!      tol = .SCF_data.linear_dependence_tol
!      n   = .overlap_mx_evals.index_of_first_greater_than(tol)
!      if (n==1) return
!
!      ! Set the projector
!      P.create(.n_bf,.n_bf)
!      P.to_unit_mx
!      do i = 1,n-1
!         P.plus_outer_product_of(.overlap_mx_evecs(:,i),-ONE)
!      end
!
!      ! Project it now
!      W.create(.n_bf,.n_bf)
!      W = MO
!      MO.to_product_of(P,W)
!
!      ! Clean up
!      W.destroy
!      P.destroy
!
!   end

   subroutine MO_g_eigen_update(self,MO_energies,MO,F)
   ! Solve for the new molecular orbital energies "MO_energies" and the new
   ! molecular orbitals "MO", given an initial old set (in "MO") and a fock
   ! matrix "F".
   ! The new molecular orbitals "MO*U" are found from the old MOs "MO" by
   ! solving (c^T F c)U = c^T S c U E ...... where U is an orthogonal matrix.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:) :: MO_energies
      MAT(REAL,:,:) :: MO,F
      MAT(REAL,:,:), allocatable :: G,U
      INT :: i
   DIE_IF(NOT is_symmetric_(F),"MOLECULE.SCF:MO_g_eigen_update ... F is not symmetric! WTF?!")

      ! Change F into old MO basis --> G
      call create_(G,2*self%n_bf,2*self%n_bf)
      call change_basis_to_(F,G,MO)

      ! Level shift in old MO basis
      if (apply_level_shifting_(self%SCF_data)) then
         do i = self%n_e+1,2*self%n_bf
            G(i,i) = self%SCF_data%level_shift+G(i,i)
         end do
      end if

      ! Get rotation U from old to new MO's
      call create_(U,2*self%n_bf,2*self%n_bf)
      call symmetrize_(G)
      call diagonalize_Jacobi_(G,MO_energies,U)

      ! Lock orbitals in place, if requested
      if (self%SCF_data%using_orbital_locking) call make_diagonally_dominant_(U)

      ! Update orbitals
      call to_product_of_(G,MO,U)
      MO = G

      ! Clean up
      call destroy_(U)
      call destroy_(G)
   end subroutine

   subroutine MO_gc_eigen_update(self,MO_energies,MO,F)
   ! Solve for the new molecular orbital energies "MO_energies" and the new
   ! molecular orbitals "MO", given an initial old set (in "MO") and a fock
   ! matrix "F".
   ! The new molecular orbitals "MO*U" are found from the old MOs "MO" by
   ! solving (c^T F c)U = c^T S c U E ...... where U is an orthogonal matrix.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:) :: MO_energies
      MAT(CPX,:,:) :: MO,F
      MAT(CPX,:,:), allocatable :: G,U
      INT :: i
   DIE_IF(NOT is_hermitian_(F),"MOLECULE.SCF:MO_gc_eigen_update ... F is not hermitian! WTF?!")

      ! Change F into old MO basis --> G
      call create_(G,2*self%n_bf,2*self%n_bf)
      call change_basis_to_(F,G,MO)

      ! Level shift in old MO basis
      if (apply_level_shifting_(self%SCF_data)) then
         do i = self%n_e+1,2*self%n_bf
            G(i,i) = self%SCF_data%level_shift+G(i,i)
         end do
      end if

      ! Get rotation U from old to new MO's
      call create_(U,2*self%n_bf,2*self%n_bf)
      call make_hermitian_(G)
   ! G.solve_hermitian_eigenproblem(MO_energies,U)
      call diagonalize_Jacobi_(G,MO_energies,U)

      ! Lock orbitals in place, if requested
      if (self%SCF_data%using_orbital_locking) call make_diagonally_dominant_(U)

      ! Update orbitals
      call to_product_of_(G,MO,U)
      MO = G

      ! Clean up
      call destroy_(U)
      call destroy_(G)
   end subroutine

   subroutine MO_g_reorder(self,MO_energies,MO)
   ! Reorder the GC molecular orbitals so that the alpha orbitals come
   ! first, then the beta. This assumes that the alph and betra
   ! orbitals are nearly degenerate.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:) :: MO_energies
      MAT(REAL,:,:) :: MO
      VEC(REAL,:), allocatable :: e
      MAT(REAL,:,:), allocatable :: W
      INT :: i,k
      call create_(e,2*self%n_bf)
      call create_(W,2*self%n_bf,2*self%n_bf)
      k = 0
      do i = 1,2*self%n_b-1,2
         k = k+1
         e(k) = MO_energies(i)
         W(:,k) = MO(:,i)
      end do
      do i = 2,2*self%n_b,2
         k = k+1
         e(k) = MO_energies(i)
         W(:,k) = MO(:,i)
      end do
      do i = 2*self%n_b+1,self%n_e
         k = k+1
         e(k) = MO_energies(i)
         W(:,k) = MO(:,i)
      end do
      do i = self%n_e+1,2*self%n_bf
         k = k+1
         e(k) = MO_energies(i)
         W(:,k) = MO(:,i)
      end do
      MO_energies = e
      MO = W
      call destroy_(W)
      call destroy_(e)
   end subroutine

   subroutine MO_gc_reorder(self,MO_energies,MO)
   ! Reorder the GC molecular orbitals so that the alpha orbitals come
   ! first, then the beta. This assumes that the alph and betra
   ! orbitals are nearly degenerate.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:) :: MO_energies
      MAT(CPX,:,:) :: MO
      VEC(REAL,:), allocatable :: e
      MAT(CPX,:,:), allocatable :: W
      INT :: i,k
      call create_(e,2*self%n_bf)
      call create_(W,2*self%n_bf,2*self%n_bf)
      k = 0
      do i = 1,2*self%n_b-1,2
         k = k+1
         e(k) = MO_energies(i)
         W(:,k) = MO(:,i)
      end do
      do i = 2,2*self%n_b,2
         k = k+1
         e(k) = MO_energies(i)
         W(:,k) = MO(:,i)
      end do
      do i = 2*self%n_b+1,self%n_e
         k = k+1
         e(k) = MO_energies(i)
         W(:,k) = MO(:,i)
      end do
      do i = self%n_e+1,2*self%n_bf
         k = k+1
         e(k) = MO_energies(i)
         W(:,k) = MO(:,i)
      end do
      MO_energies = e
      MO = W
      call destroy_(W)
      call destroy_(e)
   end subroutine


!  ==========================
!  Energy evaluation routines
!  ==========================

   NON_MPI_PURE function kinetic_energy(self) result (res)
   ! Evaluates the SCF kinetic energy as a trace of ".density_mx"
   ! with the ".kinetic matrix".
      type(MOLECULE_TYPE), IN :: self
      REAL :: res
      MAT(CPX,:,:), allocatable :: HH
      MAT(REAL,:,:), allocatable :: H
      STR(len=STR_SIZE) :: SCF_kind
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:kinetic_energy ... no SCF_data")
   ENSURE(allocated(self%density_mx),"MOLECULE.SCF:kinetic_energy ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.SCF:kinetic_energy ... no density matrix")
      res = ZERO
      SCF_kind = self%SCF_data%SCF_kind
      select case (SCF_kind)

      case ("rhf     ","rks     ","xray_rhf","xray_rks","noninteracting-group-rhf")
         call create_(H,self%n_bf,self%n_bf)
         call make_kinetic_energy_mx_(self,H)
         res = trace_product_with_(self%density_mx%r,H)
         call destroy_(H)

      case ("uhf         ","uks         ","xray_uhf    ","xray_uks    ","rohf        ","pnd_uhf     ","pnd_uks     ","xray_pnd_uhf","xray_pnd_uks")
         call create_(H,self%n_bf,self%n_bf)
         call make_kinetic_energy_mx_(self,H)
         res = trace_product_with_(self%density_mx%a,H)
         res = trace_product_with_(self%density_mx%b,H)+res
         call destroy_(H)

      case ("gchf","xray_gchf")
         call create_(HH,2*self%n_bf,2*self%n_bf)
         call create_(H,self%n_bf,self%n_bf)
         call make_kinetic_energy_mx_(self,H)
         HH = ZERO
         call set_aa_block_to_(HH,H)
         call set_bb_block_to_(HH,H)
         res = RE(trace_product_with_(self%density_mx%gc,HH))
         call destroy_(H)
         call destroy_(HH)
      end select
   end function

   NON_MPI_PURE function nuclear_attraction_energy(self) result (res)
   ! Evaluates the SCF electron-nuclear attraction energy as a trace
   ! of ".density_mx"
      type(MOLECULE_TYPE), IN :: self
      REAL :: res
      MAT(CPX,:,:), allocatable :: HH
      MAT(REAL,:,:), allocatable :: H
      STR(len=STR_SIZE) :: SCF_kind
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:nuclear_attraction_energy ... no SCF_data")
   ENSURE(allocated(self%density_mx),"MOLECULE.SCF:nuclear_attraction_energy ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.SCF:nuclear_attraction_energy ... no density matrix")
   ENSURE(allocated(self%nuclear_attraction_mx),"MOLECULE.SCF:nuclear_attraction_energy ... no nuclear_attraction_mx")
      res = ZERO
      SCF_kind = self%SCF_data%SCF_kind
      select case (SCF_kind)

      case ("rhf     ","rks     ","xray_rhf","xray_rks","noninteracting-group-rhf")
         call create_(H,self%n_bf,self%n_bf)
         call make_nuclear_attraction_mx_(self,H)
         res = trace_product_with_(self%density_mx%r,H)
         call destroy_(H)

      case ("uhf         ","uks         ","xray_uhf    ","xray_uks    ","rohf        ","pnd_uhf     ","pnd_uks     ","xray_pnd_uhf","xray_pnd_uks")
         call create_(H,self%n_bf,self%n_bf)
         call make_nuclear_attraction_mx_(self,H)
         res = trace_product_with_(self%density_mx%a,H)
         res = trace_product_with_(self%density_mx%b,H)+res
         call destroy_(H)

      case ("gchf","xray_gchf")
         call create_(HH,2*self%n_bf,2*self%n_bf)
         call create_(H,self%n_bf,self%n_bf)
         call make_nuclear_attraction_mx_(self,H)
         HH = ZERO
         call set_aa_block_to_(HH,H)
         call set_bb_block_to_(HH,H)
         res = RE(trace_product_with_(self%density_mx%gc,HH))
         call destroy_(H)
         call destroy_(HH)
      end select
   end function

   NON_MPI_PURE function charge_attraction_energy(self) result (res)
   ! Evaluates the point-charge attraction energy as a trace of
   ! ".density_mx" with the ".kinetic matrix".
      type(MOLECULE_TYPE), IN :: self
      REAL :: res
      MAT(CPX,:,:), allocatable :: HH
      MAT(REAL,:,:), allocatable :: H,T
      STR(len=STR_SIZE) :: SCF_kind
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:charge_attraction_energy ... no SCF_data")
   ENSURE(allocated(self%density_mx),"MOLECULE.SCF:charge_attraction_energy ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.SCF:charge_attraction_energy ... no density matrix")
   ENSURE(allocated(self%core_mx),"MOLECULE.SCF:charge_attraction_energy ... no core matrix")
      res = ZERO
      SCF_kind = self%SCF_data%SCF_kind
      select case (SCF_kind)

      case ("rhf     ","rks     ","xray_rhf","xray_rks","noninteracting-group-rhf")
         call create_(H,self%n_bf,self%n_bf)
         call create_(T,self%n_bf,self%n_bf)
         call make_nuclear_attraction_mx_(self,H)
         call make_kinetic_energy_mx_(self,T)
         H = self%core_mx%r-H-T
         res = trace_product_with_(self%density_mx%r,H)
         call destroy_(T)
         call destroy_(H)

      case ("uhf         ","uks         ","xray_uhf    ","xray_uks    ","rohf        ","pnd_uhf     ","pnd_uks     ","xray_pnd_uhf","xray_pnd_uks")
         call create_(H,self%n_bf,self%n_bf)
         call create_(T,self%n_bf,self%n_bf)
         call make_kinetic_energy_mx_(self,T)
         call make_nuclear_attraction_mx_(self,H)
         H = self%core_mx%a-H-T
         res = trace_product_with_(self%density_mx%a,H)
         res = trace_product_with_(self%density_mx%b,H)+res
         call destroy_(T)
         call destroy_(H)

      case ("gchf","xray_gchf")
         call create_(HH,2*self%n_bf,2*self%n_bf)
         call create_(H,self%n_bf,self%n_bf)
         call create_(T,self%n_bf,self%n_bf)
         call make_kinetic_energy_mx_(self,T)
         call make_nuclear_attraction_mx_(self,H)
         HH = ZERO
         H = H+T
         call set_aa_block_to_(HH,H)
         call set_bb_block_to_(HH,H)
         HH = self%core_mx%gc-HH
         res = RE(trace_product_with_(self%density_mx%gc,HH))
         call destroy_(T)
         call destroy_(H)
         call destroy_(HH)
      end select
   end function

   function SCF_energy_0(self) result (res)
   ! Evaluates the total SCF energy (including nuclear) as
   ! a trace of ".density_mx" with the ".fock matrix".
      type(MOLECULE_TYPE), IN :: self
      REAL :: res
   ENSURE(allocated(self%density_mx),"MOLECULE.SCF:SCF_energy_0 ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.SCF:SCF_energy_0 ... no density matrix")
   ENSURE(allocated(self%fock_mx),"MOLECULE.SCF:SCF_energy_0 ... no fock matrix")
   ENSURE(has_any_genre_(self%fock_mx),"MOLECULE.SCF:SCF_energy_0 ... no fock matrix")
      res = SCF_energy_(self,self%density_mx,self%fock_mx)  ! NOT PURE
   end function

   function SCF_energy_1(self,P,F) result (res)
   ! Evaluates the total SCF energy (including nuclear) as 
   ! a trace of "P" with the fock matrix "F".
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), IN :: P,F
      REAL :: res
      INT :: g,a
      VEC(INT,:), allocatable :: gr
      VEC(REAL,:), allocatable :: I0
      MAT(REAL,:,:), allocatable :: I1
      VEC(REAL,3) :: mu
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:SCF_energy_1 ... no scf data")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:SCF_energy_1 ... no atoms")
      res = ZERO

      ! Electronic + nuclear energy
      select case (self%SCF_data%SCF_kind)

      case ("noninteracting-group-rhf")
         res = SCF_electronic_energy_(self,P,F)
         do g = 1,size(self%atom_group)
            gr = self%atom_group(g)%atom_index
            res = res+nuclear_repulsion_energy_(self%atom(gr))
         end do

      case default
         res = SCF_electronic_energy_(self,P,F)+nuclear_repulsion_energy_(self%atom)
      end select

      ! Add finite field
      if (NOT is_zero_(self%E_field)) then
         mu = nuclear_dipole_moment_(self%atom)
         res = res-dot_(self%E_field,mu)
      end if

      ! Add finite local Lorentz fields
      ! Only dipole contributions are done
      ! The charge-charge terms are missing
      if (self%SCF_data%using_SC_Lorentz_fields) then
         call create_(I0,self%n_atom)
         call create_(I1,3,self%n_atom)
         call make_Lorentz_interactions_(self,I0,I1)
         do a = 1,self%n_atom
            res = res-self%atom(a)%charge*I0(a)
            res = res-dot_(I1(:,a),self%atom(a)%dipole)
         end do
         call destroy_(I1)
         call destroy_(I0)
      end if
   end function

   PURE function SCF_electronic_energy_0(self,P,core) result (res)
   ! Evaluates the SCF electronic energy as a trace of density "P"
   ! with ".fock_mx". If "core" is present and FALSE, the core
   ! contribution is not added.
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), IN :: P
      BIN, optional, IN :: core
      REAL :: res
   ENSURE(allocated(self%fock_mx),"MOLECULE.SCF:SCF_electronic_energy_0 ... no fock matrix")
   ENSURE(has_any_genre_(self%fock_mx),"MOLECULE.SCF:SCF_electronic_energy_0 ... no fock matrix")
      res = SCF_electronic_energy_(self,P,self%fock_mx,core)
   end function

   PURE function SCF_electronic_energy_1(self,P,F,core) result (res)
   ! Evaluates the SCF electronic energy as a trace of density "P"
   ! with fock matrix "F". If "core" is present and FALSE, the core
   ! contribution is not added.  NOTE: this includes the DFT
   ! electronic energies, and it assumes that these energy
   ! contributions were calculated at the time of the formation of "F"
   ! from "P", and that they were stored in .SCF_data.
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), IN :: P,F
      BIN, optional, IN :: core
      REAL :: res
      BIN :: add_core
      type(OPMATRIX_TYPE), allocatable :: W
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:SCF_electronic_energy_1 ... no SCF_data")
   ENSURE(allocated(self%core_mx),"MOLECULE.SCF:SCF_electronic_energy_1 ... core_mx missing")
   ENSURE(has_genre_(self%core_mx,spinorbital_genre_(self%SCF_data)),"MOLECULE.SCF:SCF_electronic_energy_1 ... core_mx missing")
      add_core = TRUE
      if (present(core)) add_core = core

      ! W = F + h
      W = F
      if (add_core) call plus_(W,self%core_mx,spinorbital_genre_(self%SCF_data))
      res = ZERO
      select case (self%SCF_data%SCF_kind)

      case ("rhf                     ","xray_rhf                ","noninteracting-group-rhf")
         res = HALF*trace_product_with_(P%r,W%r)+self%SCF_data%ensemble_energy_correction

      case ("rks                     ","xray_rks                ")
         res = HALF*trace_product_with_(P%r,W%r)+self%SCF_data%DFT_energy_correction+self%SCF_data%ensemble_energy_correction

      case ("uhf                     ","xray_uhf                ","rohf                    ","pnd_uhf                 ","xray_pnd_uhf            ")
         res = HALF*trace_product_with_(P%a,W%a)+HALF*trace_product_with_(P%b,W%b)

      case ("uks                     ","xray_uks                ","pnd_uks                 ","xray_pnd_uks            ")
         res = HALF*trace_product_with_(P%a,W%a)+HALF*trace_product_with_(P%b,W%b)+self%SCF_data%DFT_alpha_energy_correction+self%SCF_data%DFT_beta_energy_correction

      case ("gchf                    ","xray_gchf               ")
         res = HALF*RE(trace_product_with_(P%gc,W%gc))
      end select

      ! Clean
      call destroy_(W)
   end function

   PURE function SCF_electronic_energies_0(self) result (res)
   ! Evaluates the SCF electronic energies as a trace of density "P" with
   ! fock matrix "F". In the case of UHF the energies are the E^alpha and E^beta
   ! energies. NOTE: this includes the DFT energy contributions.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), allocatable :: res
      res = SCF_electronic_energies_(self,self%density_mx,self%fock_mx)
   end function

   PURE function SCF_electronic_energies_1(self,P,F) result (res)
   ! Evaluates the SCF electronic energies as a trace of density "P" with
   ! fock matrix "F". In the case of UHF the energies are the E^alpha and E^beta
   ! energies. NOTE: this includes the DFT electronic energies, and it assumes
   ! that these energy contributions were calculated at the time of the
   ! formation of "F" from "P", and that they were stored in .SCF_data.
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), IN :: P,F
      VEC(REAL,:), allocatable :: res
      type(OPMATRIX_TYPE), allocatable :: W
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:SCF_electronic_energies_1 ... no SCF_data")
   ENSURE(allocated(self%core_mx),"MOLECULE.SCF:SCF_electronic_energies_1 ... no core_mx")
      W = F
      select case (self%SCF_data%SCF_kind)

      case ("rhf                     ","xray_rhf                ","noninteracting-group-rhf")
         call create_(res,1)
         call plus_(W,self%core_mx)
         res(1) = HALF*trace_product_with_(P%r,W%r)

      case ("rks                     ","xray_rks                ")
         call create_(res,1)
         call plus_(W,self%core_mx)
         res(1) = HALF*trace_product_with_(P%r,W%r)+self%SCF_data%DFT_energy_correction

      case ("uhf                     ","xray_uhf                ","rohf                    ","pnd_uhf                 ","xray_pnd_uhf            ")
         call create_(res,2)
         call plus_(W,self%core_mx)
         res(1) = HALF*trace_product_with_(P%a,W%a)
         res(2) = HALF*trace_product_with_(P%b,W%b)

      case ("uks                     ","xray_uks                ","pnd_uks                 ","xray_pnd_uks            ")
         call create_(res,2)
         call plus_(W,self%core_mx)
         res(1) = HALF*trace_product_with_(P%a,W%a)+self%SCF_data%DFT_alpha_energy_correction
         res(2) = HALF*trace_product_with_(P%b,W%b)+self%SCF_data%DFT_beta_energy_correction

      case ("gchf                    ","xray_gchf               ")
         call create_(res,1)
         call plus_(W,self%core_mx)
         res(1) = HALF*RE(trace_product_with_(P%gc,W%gc))
      end select

      ! Clean
      call destroy_(W)
   end function


!  ==============================
!  Initial orbital guess routines
!  ==============================

   subroutine get_initial_guess(self)
   ! Get the initial guess for the ".density_mx" and the ".MOs". 
   ! It is better to start from MOs, read them in.
   ! That is why  .SCF_data.initial_MOs is set to a known spin-
   ! orbital type it will overide .SCF_data.initial_density.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:get_initial_guess ... no SCF_data")
      select case (self%SCF_data%initial_MOs)
      case ("--using density--"); call get_initial_density_mx_(self)
      ! Different types of saved .MOs
      case ("r                "); call read_old_MOs_(self)
      case ("restricted       "); call read_old_MOs_(self)
      case ("u                "); call read_old_MOs_(self)
      case ("unrestricted     "); call read_old_MOs_(self)
      case ("gc               "); call read_old_MOs_(self)
      case ("general_complex  "); call read_old_MOs_(self)
      case default; DIE("MOLECULE.SCF:get_initial_guess ... unknown MOs, "//trim(self%SCF_data%initial_MOs))
      end select
   end subroutine

   subroutine read_old_MOs(self)
   ! Get the initial guess for the .MOs by reading an
   ! old archive of .MOs, converting the MO's if
   ! necessary, and then making the SCF density matrix as well.
      type(MOLECULE_TYPE), INOUT :: self

   ! arch :: ARCHIVE

   ! ! Recreate MO's
   ! .MOs.destroy
   ! .MOs.create(.n_bf,.SCF_data.initial_MOs)

   ! ! Recreate energies
   ! .MO_energies.destroy
   ! .MO_energies.create(.n_bf,genre=.SCF_data.MO_energies_genre)

   ! ! Read
   ! arch.set(.name,"MOs",genre=.SCF_data.initial_MOs)
   ! arch.read(.MOs)
      call read_archive_(self,"MOs",genre=self%SCF_data%initial_MOs)
      call read_archive_(self,"MO_energies",genre=self%SCF_data%initial_MOs)

      ! Convert (if necessary)
      call convert_to_(self%MOs,MOs_genre_(self%SCF_data),self%n_a,self%n_b,self%SCF_data%quantization_axis)

      ! Re-orthonormalize ?
   ! if (.SCF_data.allocated) then
   ! if (.SCF_data.initial_MOs=="r ") then
   ! .symorthonormalize_occupied_MOs
   ! end
   ! end

      ! Make the density matrix
      call make_SCF_density_mx_(self)
   end subroutine

   subroutine make_atom_MOs_guess(self)
   ! Get the initial guess for the .MOs by getting the highest
   ! occupied restricted natural orbitals from the sum of atomic densities guess
   ! for the density matrix. Convert these .MOs, if neccesary.
   ! Also make the SCF .density_mx from these .MOs.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: MOs_genre
      type(ARCHIVE_TYPE) :: arch

      ! Make promolecule density
      call make_promolecule_density_mx_(self)

      ! What is this?
      if (genre_(self%density_mx)/="r ") then
         call set_(arch,self%name,"promolecule_density_mx",genre="r ")
         call destroy_(self%density_mx,"r ")
         call read_(arch,self%density_mx)
      end if
      call make_NOs_(self)
      call put_NOs_(self)
      call assign_MOs_to_NOs_(self)

      ! Convert MO's if needed
      MOs_genre = MOs_genre_(self%SCF_data)
      call convert_to_(self%MOs,MOs_genre,self%n_a,self%n_b,self%SCF_data%quantization_axis)

      ! Recreate orbital energies
      call destroy_(self%MO_energies)
      call create_(self%MO_energies,genre=MO_energies_genre_(self%SCF_data))

      ! Make density matrix
      call make_SCF_density_mx_(self)
   end subroutine

   subroutine get_initial_density_mx(self)
   ! Get the initial guess for the ".density_mx"
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:get_initial_density_mx ... no SCF_data")
      call set_is_guess_(self%SCF_data,TRUE)

      ! A promolecule guess is a superposition of atomic ANO densities, and
      ! MOLECULE.SCF:make_ANOs_and_interpolators deliberately returns early for
      ! a single atom ("if (.n_atom<=1) return"). So there are no ANOs to
      ! superpose, and MOLECULE.RHO:add_ANO_densities hits
      ! ENSURE(.has_all_ANO_matrices,"no ANOs"). In a DEBUG build that reports
      ! cleanly; in RELEASE, where ENSURE is compiled out, it SEGFAULTS.
      ! Measured 2026-08-13: H, He and Li single atoms all died, closed shell
      ! and open shell alike, with promolecule but not with core.
      ! The core guess is the only coherent behaviour here, and the fragment
      ! path already does exactly this (see "if (.mol(g).atom.dim==1) then
      ! .mol(g).SCF_data.set_initial_density("core")" below).
      if (self%n_atom<=1 AND self%SCF_data%initial_density=="promolecule") then
         call set_initial_density_(self%SCF_data,"core")
      end if
      select case (self%SCF_data%initial_density)
      ! Make different .density_mx
      case ("core           "); call make_core_mx_guess_MOs_(self)
      case ("fock           "); call make_Fock_mx_guess_MOs_(self)
      case ("group          "); call make_atom_group_guess_MOs_(self)
      case ("promolecule    "); call make_promol_guess_MOs_(self)
      ! Read different .density_mx
      case ("r              "); call read_old_density_mx_(self)
      case ("restricted     "); call read_old_density_mx_(self)
      case ("u              "); call read_old_density_mx_(self)
      case ("unrestricted   "); call read_old_density_mx_(self)
      case ("gc             "); call read_old_density_mx_(self)
      case ("general_complex"); call read_old_density_mx_(self)
      case default; DIE("MOLECULE.SCF:get_initial_density_mx ... unknown density, "//trim(self%SCF_data%initial_density))
      end select
      call set_is_guess_(self%SCF_data,FALSE)
   end subroutine

   subroutine read_old_density_mx(self)
   ! Read an old .density_mx of type "initial_guess" and use it to
   ! make initial .MOs to start an scf calculation.
   ! Convert the .density_mx, if neccesary.
      type(MOLECULE_TYPE), INOUT :: self

   !  genre :: STR
   !  arch :: ARCHIVE

   !  genre = .SCF_data.initial_density_genre

   !  ! Create
   !  .density_mx.destroy
   !  .density_mx.create(.n_bf,genre)

   !  ! Read
   !  arch.set(.name,"density_mx")
   !  arch.read(.density_mx,genre=genre)
      call read_archive_(self,"density_mx",genre=initial_density_genre_(self%SCF_data))

      ! Convert
      call convert_to_(self%density_mx,spinorbital_genre_(self%SCF_data))

      ! Make a fock matrix & starting MO's
      call make_scf_Fock_mx_(self)
      call make_Fock_mx_guess_MOs_(self)
   end subroutine

   subroutine make_core_mx_guess_MOs(self)
   ! Do a core scf for the ".density_mx", ".MOs" and
   ! ".MO_energies". Also makes the restricted ".core_mx".
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: MOs_genre
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_core_mx_guess_MOs ... no SCF_data")

      ! Recreate and initialize (leaky)
      call destroy_(self%MO_energies)
      call create_(self%MO_energies,self%n_bf,"r ")
      call destroy_(self%MOs)
      call create_(self%MOs,self%n_bf,"r ")

      ! Get restricted core and overlap matrices (leaky)
      call make_r_core_mx_(self)
      call make_overlap_mx_(self)

      ! Get the guess orbitals
      call diagonalize_r_mx_(self,self%core_mx%r,self%MO_energies%r,self%MOs%r)

      ! Convert the orbitals to the right kind (leaky)
      MOs_genre = MO_energies_genre_(self%SCF_data)
      call convert_to_(self%MO_energies,MOs_genre)
      MOs_genre = MOs_genre_(self%SCF_data)
      call convert_to_(self%MOs,MOs_genre,self%n_a,self%n_b,self%SCF_data%quantization_axis)

      ! Make the densityy matrix
      call make_SCF_density_mx_(self)

      ! Get a reasonable set of starting MO's
      call make_scf_Fock_mx_(self)
      call make_Fock_mx_guess_MOs_(self)
   end subroutine

   subroutine make_promol_guess_MOs(self)
   ! Make a ".density matrix" and fock matrix from the sum of atomic
   ! densities.  Then get initial orbitals from this fock matrix, and
   ! make the density corresponding to these initial orbitals.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_promol_guess_MOs ... no SCF_data")

      ! Get initial MO's
      if (self%SCF_data%SCF_kind=="rohf") then; call make_promolecule_guess_rohf_(self)
      else; call make_promolecule_density_mx_(self)
      end if

      ! Get a starting fock matrix from this density
      call make_scf_Fock_mx_(self)

      ! Make the MO's 
      call make_Fock_mx_guess_MOs_(self)
   end subroutine

   subroutine make_promolecule_guess_rohf(self)
   ! Make a forced *restricted* ".density matrix" and fock matrix from
   ! the sum of atomic densities.  Then get initial restricted
   ! orbitals from this fock matrix. This is neccessary for ROHF
   ! initial guesses.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: charge,mult
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_promolecule_guess_rohf ... no SCF_data")
   ENSURE(self%SCF_data%SCF_kind=="rohf","MOLECULE.SCF:make_promolecule_guess_rohf ... not an ROHF calculation")

      ! Store original charge & multiplicity
      charge = self%charge
      mult = self%spin_multiplicity

      ! Reset to closed shell +1 charge
      ! (-1 charge may not converge).
      if (is_odd_(self%n_e)) self%charge = self%charge+1
      self%spin_multiplicity = 1

      ! Set to RHF
      call set_SCF_kind_(self%SCF_data,"rhf")
   ! .set_atom_info

      ! Make promolecule density
      call make_promolecule_density_mx_(self)

      ! Get a starting MO's from this density
      call make_scf_Fock_mx_(self)
      call make_Fock_mx_guess_MOs_(self)

      ! Change back to original state
      self%charge = charge
      self%spin_multiplicity = mult

      ! And back to ROHF
      call set_SCF_kind_(self%SCF_data,"rohf")
   ! .set_atom_info

      ! Make (uhf) density matrix from (rhf) MO's
      call make_SCF_density_mx_(self)
   end subroutine

   subroutine make_promolecule_density_mx(self)
   ! Make the ANO densities and add them together.
      type(MOLECULE_TYPE), INOUT :: self

      ! ANO's
      call make_ANOs_and_interpolators_(self)

      ! Add spherical atomic densities
      call destroy_(self%density_mx)
      call add_ANO_densities_(self,self%density_mx)

      ! Convert 
      call convert_to_(self%density_mx,spinorbital_genre_(self%SCF_data),factor=HALF)

      ! Save this
      call put_archive_(self,self%density_mx,"density_mx")
   end subroutine

   subroutine make_atom_group_guess_MOs(self)
   ! Make a .density matrix and .fock_mx from the sum of noninteracting
   ! atom_group densities. Then get initial orbitals from this fock matrix,
   ! and make the density corresponding to these initial orbitals.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(self%SCF_data%SCF_kind/="rohf","MOLECULE.SCF:make_atom_group_guess_MOs ... atom guess not available for ROHF")

      ! Sum of group densities
      call atom_group_SCF_(self)

      ! Get a reasonable set of starting MO's
      call make_scf_Fock_mx_(self)
      call make_Fock_mx_guess_MOs_(self)
   end subroutine

   subroutine make_group_energies(self,g,E_SCF,E_T,E_Z,E_C,E_K,fac)
   ! Assuming group densities exist (see .do_atom_group_SCF), then
   ! for group number "g" make the group SCF energy "E_SCF", the group
   ! kinetic energy "E_T", the group nuclear attraction energy "E_Z",
   ! the group coulomb energy "E_C", and the group exchange energy
   ! "E_K". If "fac" is present, multiply all the energy values by
   ! this factor.
      type(MOLECULE_TYPE), target, INOUT :: self
      INT, IN :: g
      REAL, OUT :: E_SCF,E_T,E_Z,E_C,E_K
      REAL, optional, IN :: fac
      MAT(REAL,:,:), allocatable :: C,K
      INT :: n_bf
   ENSURE(allocated(self%atom_group),"MOLECULE.SCF:make_group_energies ... no atom_group info")
   ENSURE(g<=size(self%atom_group),"MOLECULE.SCF:make_group_energies ... group out of range")
   ENSURE(allocated(self%mol),"MOLECULE.SCF:make_group_energies ... no .mol")
   ENSURE(size(self%mol)==size(self%atom_group),"MOLECULE.SCF:make_group_energies ... wrong size, .mol")
   ENSURE(allocated(self%mol(g)%SCF_data),"MOLECULE.SCF:make_group_energies ... no atom_group molecule SCF_data")
   ENSURE(self%mol(g)%spin_multiplicity==1,"MOLECULE.SCF:make_group_energies ... only singlets allowed for now")
      E_SCF = ZERO
      E_T = ZERO
      E_Z = ZERO
      E_C = ZERO
      E_K = ZERO
      if (no_of_electrons_(self%mol(g))>=1) then

         ! Get the SCF energy
         E_SCF = self%mol(g)%SCF_data%energy

         ! Get the kinetic energy
         E_T = self%mol(g)%SCF_data%kinetic_energy

         ! Get the nuclear attraction energy
         E_Z = self%mol(g)%SCF_data%nuclear_attraction_energy

         ! Coulomb/exchange matrices
         n_bf = self%mol(g)%n_bf
         call create_(C,n_bf,n_bf)
         call create_(K,n_bf,n_bf)
         call make_r_JK_nosym_(self%mol(g),C,K,self%mol(g)%density_mx%r)

         ! Coulomb & exchange energies
         E_C = HALF*trace_product_with_(C,self%mol(g)%density_mx%r)
         E_K = -QUARTER*trace_product_with_(K,self%mol(g)%density_mx%r)
         call destroy_(K)
         call destroy_(C)

         ! Clean up disk
         call delete_SCF_archives_(self%mol(g))
      end if
      if (present(fac)) then
         E_SCF = fac*E_SCF
         E_T = fac*E_T
         E_Z = fac*E_Z
         E_C = fac*E_C
         E_K = fac*E_K
      end if
   end subroutine

   subroutine make_Fock_mx_guess_MOs(self)
   ! From an initial .fock_mx make the .MOs, and
   ! .MO_energies.  Then make the .density_mx from these
   ! orbitals.  Destroy .fock_mx afterwards
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: MOs_genre
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_Fock_mx_guess_MOs ... no SCF_data")
   ENSURE(allocated(self%fock_mx),"MOLECULE.SCF:make_Fock_mx_guess_MOs ... need an initial fock matrix")
   ENSURE(has_genre_(self%fock_mx,spinorbital_genre_(self%SCF_data)),"MOLECULE.SCF:make_Fock_mx_guess_MOs ... need right kind of initial fock matrix")

      ! Clean
      call destroy_(self%MOs)
      call destroy_(self%MO_energies)

      ! Get core and overlap
      call make_overlap_mx_(self)

      ! MO/Fock matrix genre
      MOs_genre = MOs_genre_(self%SCF_data)

      ! Build the Fock matrix
      select case (MOs_genre)

      case ("r ")
         ! Restricted MO's
         call create_(self%MO_energies,self%n_bf,"r ")
         call create_(self%MOs,self%n_bf,"r ")
         call make_r_guess_from_(self,self%fock_mx%r,self%MO_energies%r,self%MOs%r)

      case ("u ")
         call create_(self%MO_energies,self%n_bf,"u ")
         call create_(self%MOs,self%n_bf,"u ")
         call make_r_guess_from_(self,self%fock_mx%a,self%MO_energies%a,self%MOs%a)
         call make_r_guess_from_(self,self%fock_mx%b,self%MO_energies%b,self%MOs%b)

      case ("gc")
         call create_(self%MO_energies,self%n_bf,"g ")
         call create_(self%MOs,self%n_bf,"gc")
         call make_gc_Fock_guess_(self,self%MO_energies%g,self%MOs%gc,self%fock_mx%gc)

      case default
         DIE("MOLECULE.SCF:make_Fock_mx_guess_MOs ... unknown MO kind, "//trim(MOs_genre))
      end select

      ! Delete Fock matrix used to make MO's
   ! .fock_mx.destroy(MOs_genre)

      ! Make the (spinorbital_genre) density matrix
      call make_SCF_density_mx_(self)
   end subroutine

   subroutine make_gc_Fock_guess(self,MO_energies,MO,fock_mx)
   ! Make/guess the orbital energies "MO_energies" and orbitals "MO" from
   ! a given complex general "fock_mx".
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: MO_energies
      MAT(CPX,:,:), OUT :: MO
      MAT(CPX,:,:), IN :: fock_mx
      MAT(REAL,:,:), allocatable :: S,R
      MAT(CPX,:,:), allocatable :: H
   DIE_IF(NOT is_hermitian_(fock_mx),"MOLECULE.SCF:make_gc_Fock_guess ... fock_mx not hermitian!  WTF?!")
   ENSURE(allocated(self%overlap_mx),"MOLECULE.SCF:make_gc_Fock_guess ... no overlap matrix")
      call create_(R,2*self%n_bf,2*self%n_bf)
      call create_(S,self%n_bf,self%n_bf)

      ! S = S^-1/2, R = big inverse S
      call make_r_overlap_inverse_sqrt_(self,S)

      ! R = big inverse S
      R = ZERO
      call set_aa_block_to_(R,S)
      call set_bb_block_to_(R,S)
      call destroy_(S)

      ! H = Fock matrix
      call create_(H,2*self%n_bf,2*self%n_bf)
      H = fock_mx

      ! H = S^-1/2 F S^-1/2
      call change_basis_using_(H,R)
      call diagonalize_Jacobi_(H,MO_energies,MO)

      !  c  = S^-1/2 c~
      call to_product_of_(H,R,MO)
      MO = H

      ! Clean
      call destroy_(H)
      call destroy_(R)
   end subroutine

   subroutine make_r_guess_from(self,fock_mx,MO_energies,MO)
   ! Make initial "MO's" and "MO_energies" from a given "fock_mx"
   ! ... for cartesian basis functions
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: fock_mx
      MAT(REAL,:,:), OUT :: MO
      VEC(REAL,:), OUT :: MO_energies
      MAT(REAL,:,:), allocatable :: F,R
   ENSURE(is_symmetric_(fock_mx),"MOLECULE.SCF:make_r_guess_from ... fock_mx not symmetric!  WTF?!")
      call create_(F,self%n_bf,self%n_bf)
      call create_(R,self%n_bf,self%n_bf)

      !  Get S^{-1/2 }
      call make_r_overlap_inverse_sqrt_(self,R)  ! INOUT

      !  H~ = S^-1/2 H S^-1/2
      call change_basis_to_(fock_mx,F,R)

      ! Remove linear dependence
      if (allocated(self%SCF_data)) call shift_dependence_from_r_(self,F)

      !  Solve for c~
      call solve_symmetric_eigenproblem_(F,MO_energies,MO)

      ! Back transform: c = S^{-1/2} c~
      call to_product_of_(F,R,MO)
      MO = F

      ! Clean
      call destroy_(R)
      call destroy_(F)
   end subroutine

   subroutine diagonalize_r_mx(self,H,eigenvalues,eigenvectors)
   ! Diagonalize a restricted one-electron AO operator matrix "H",
   ! return the "eigenvalues" and "eigenvector" coefficients.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: H
      VEC(REAL,:), OUT :: eigenvalues
      MAT(REAL,:,:), OUT :: eigenvectors
      MAT(REAL,:,:), allocatable :: F,R
   ENSURE(is_symmetric_(H),"MOLECULE.SCF:diagonalize_r_mx ... H not symmetric!  WTF?!")
      call create_(F,self%n_bf,self%n_bf)
      call create_(R,self%n_bf,self%n_bf)

      !  Get S^{-1/2 }
      call make_r_overlap_inverse_sqrt_(self,R)

      !  H~ = S^-1/2 H S^-1/2
      call change_basis_to_(H,F,R)

      ! Remove linear dependence
      if (allocated(self%SCF_data)) call shift_dependence_from_r_(self,F)

      !  Solve for cO~
      call solve_symmetric_eigenproblem_(F,eigenvalues,eigenvectors)

      ! Back transform: c = S^{-1/2} c~
      call to_product_of_(F,R,eigenvectors)
      eigenvectors = F

      ! Clean
      call destroy_(R)
      call destroy_(F)
   end subroutine

   subroutine diagonalize_g_mx(self,H,eigenvalues,eigenvectors)
   ! Diagonalize a general (complex) one-electron AO operator matrix
   ! "H", return the "eigenvalues" and "eigenvector" coefficients wrt
   ! the basis.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: H
      VEC(REAL,:), OUT :: eigenvalues
      MAT(REAL,:,:), OUT :: eigenvectors

! DIE_IF(NOT H.is_hermitian,"fock_mx not hermitian!  WTF?!")
      MAT(REAL,:,:), allocatable :: S,R
      MAT(REAL,:,:), allocatable :: F

      ! The following code is inherited from MOLECULE.SCF
      call create_(R,2*self%n_bf,2*self%n_bf)
      call create_(S,self%n_bf,self%n_bf)

      ! S = S^-1/2, R = big inverse S
      call make_r_overlap_inverse_sqrt_(self,S)

      ! R = big inverse S
      R = ZERO
      call set_aa_block_to_(R,S)
      call set_bb_block_to_(R,S)
      call destroy_(S)

      ! H = Fock matrix
      call create_(F,2*self%n_bf,2*self%n_bf)
      F = H

      ! H = S^-1/2 F S^-1/2
      call change_basis_using_(F,R)
      call diagonalize_Jacobi_(F,eigenvalues,eigenvectors)

      !  c  = S^-1/2 c~
      call to_product_of_(F,R,eigenvectors)
      eigenvectors = F

      ! Clean
      call destroy_(F)
      call destroy_(R)
   end subroutine

   subroutine diagonalize_gc_mx(self,H,eigenvalues,eigenvectors)
   ! Diagonalize a general (complex) one-electron AO operator matrix
   ! "H", return the "eigenvalues" and "eigenvector" coefficients wrt
   ! the basis.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), IN :: H
      VEC(REAL,:), OUT :: eigenvalues
      MAT(CPX,:,:), OUT :: eigenvectors

! DIE_IF(NOT H.is_hermitian,"fock_mx not hermitian!  WTF?!")
      MAT(REAL,:,:), allocatable :: S,R
      MAT(CPX,:,:), allocatable :: F

      ! The following code is inherited from MOLECULE.SCF
      call create_(R,2*self%n_bf,2*self%n_bf)
      call create_(S,self%n_bf,self%n_bf)

      ! S = S^-1/2, R = big inverse S
      call make_r_overlap_inverse_sqrt_(self,S)

      ! R = big inverse S
      R = ZERO
      call set_aa_block_to_(R,S)
      call set_bb_block_to_(R,S)
      call destroy_(S)

      ! H = Fock matrix
      call create_(F,2*self%n_bf,2*self%n_bf)
      F = H

      ! H = S^-1/2 F S^-1/2
      call change_basis_using_(F,R)
      call diagonalize_Jacobi_(F,eigenvalues,eigenvectors)

      !  c  = S^-1/2 c~
      call to_product_of_(F,R,eigenvectors)
      eigenvectors = F

      ! Clean
      call destroy_(F)
      call destroy_(R)
   end subroutine


!  =============
!  Group density
!  =============

   subroutine make_atom_group_density_mx(self)
   ! Make the promolecule density matrix from *symmetrically
   ! orthonormalized* group (monomer) molecular orbitals. Do not
   ! confuse this with the progroup density matrix, made with
   ! .make_progroup_density_mx. The symmetrically orthonormalized
   ! molecular orbitals are made and are ordered by group.  This will
   ! destroy any existing restricted density matrix and MOs.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%MOs),"MOLECULE.SCF:make_atom_group_density_mx ... no MO's")
      call do_atom_group_SCF_(self,MOs=TRUE)  ! leaky
      call symorthonormalize_occupied_MOs_(self)  ! leaky

      ! Assign the NO's (occupations must exist!)
      self%NOs = self%MOs

      ! Make density from NO's
      call make_density_mx_from_NOs_(self)

      ! Clean
      call destroy_(self%NOs)
   end subroutine

   subroutine symorthonormalize_occupied_MOs(self)
   ! Symmetrically orthonormalize the occupied MO's. The occupied
   ! orbitals are defined by the .NO_occ_nos vector. The
   ! virtual orbitals are schmidt orthonormalized to the occupied MOs.
   ! Only works for restricted at the moment.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: occ,vir,all
      MAT(REAL,:,:), allocatable :: MO
      INT :: i,no,nv
   ENSURE(allocated(self%MOs),"MOLECULE.SCF:symorthonormalize_occupied_MOs ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.SCF:symorthonormalize_occupied_MOs ... no rhf MO's")

      ! Hoe many occupied and virtual?
      no = count(self%NO_occ_nos%r>0.1)
      nv = count(self%NO_occ_nos%r<=0.1)
      ENSURE((no+nv)==self%n_bf,"MOLECULE.SCF:symorthonormalize_occupied_MOs ... error determining no and nv")

      ! Banner
      call flush_(stdout)
      call text_(stdout,"Symorthnormalising occupied MO's")
      call flush_(stdout)
      call text_(stdout,"The occupied orbitals are defined by the current ")
      call text_(stdout,"occupation numbers. Virtual orbitals are Schmidt-")
      call text_(stdout,"orthonormalized to the occupied MO's. Virtual orbitals")
      call text_(stdout,"are Schmidt-orthonormalized to the occupied MO's.")
      call flush_(stdout)
      call show_(stdout,"No. of occupied MO's =",no)
      call show_(stdout,"No. of virtual  MO's =",nv)

      ! Get indices of occ & virtuals
      call create_(occ,no)
      call create_(vir,nv)
      i = 0
      occ = pack([(i,i = 1,self%n_bf)],self%NO_occ_nos%r>0.1)
      vir = pack([(i,i = 1,self%n_bf)],self%NO_occ_nos%r<=0.1)

      ! Make sure overlap exists
      call make_overlap_mx_(self)

      ! Symmetric orthonormalize the occupied MO's
      call create_(MO,self%n_bf,no)
      MO = self%MOs%r(:,occ)
      call symmetrically_orthonormalize_(MO,self%overlap_mx)
      self%MOs%r(:,occ) = MO
      call destroy_(MO)

      ! Schmidt orthonormalize the virtuals to the occupieda
      all = [occ,vir]
      call create_(MO,self%n_bf,self%n_bf)
      MO = self%MOs%r(:,all)
      call Schmidt_orthonormalize_(MO,self%overlap_mx)
      self%MOs%r(:,all) = MO

      ! Clean up
      call destroy_(all)
      call destroy_(MO)
      call destroy_(vir)
      call destroy_(occ)
   end subroutine


!  Newer, restricted only

   subroutine merge_atom_group_DMs(self)
   ! Merge the group DMs into the supermolecule MOs.
   ! All occupied first, then all virtuals, as columns.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: mess,genre
      INT :: g,nb,fb,lb
   ENSURE(allocated(self%atom_group),"MOLECULE.SCF:merge_atom_group_DMs ... no .atom_group info")
   ENSURE(allocated(self%mol),"MOLECULE.SCF:merge_atom_group_DMs ... no .mol")
   ENSURE(size(self%mol)==size(self%atom_group),"MOLECULE.SCF:merge_atom_group_DMs ... wrong size, .mol")
      if (is_restricted_(self%SCF_data)) then; genre = "r "
      else; genre = "u "
      end if
      call destroy_(self%density_mx)
      call create_(self%density_mx,self%n_bf,genre)
      if (genre=="u ") then
         self%density_mx%a = ZERO
         lb = 0
         ! Loop over atom group "g" and set MOs
         do g = 1,size(self%atom_group)
            mess = "group "//trim(to_str_(g))
            DIE_IF(NOT allocated(self%mol(g)%density_mx),trim(mess)//"MOLECULE.SCF:merge_atom_group_DMs ...  density matrix undefined!")
            if (no_of_electrons_(self%mol(g))>=1) then

               ! Sizes
               nb = self%mol(g)%n_bf
               ! MO/GO counters
               fb = lb+1
               lb = lb+nb
               ! Copy the block
               self%density_mx%a(fb:lb,fb:lb) = self%mol(g)%density_mx%a
            end if
         end do
         self%density_mx%b = ZERO
         lb = 0
         ! Loop over atom group "g" and set MOs
         do g = 1,size(self%atom_group)
            mess = "group "//trim(to_str_(g))
            DIE_IF(NOT allocated(self%mol(g)%density_mx),trim(mess)//"MOLECULE.SCF:merge_atom_group_DMs ...  density matrix undefined!")
            if (no_of_electrons_(self%mol(g))>=1) then

               ! Sizes
               nb = self%mol(g)%n_bf
               ! MO/GO counters
               fb = lb+1
               lb = lb+nb
               ! Copy the block
               self%density_mx%b(fb:lb,fb:lb) = self%mol(g)%density_mx%b
            end if
         end do
      else
         self%density_mx%r = ZERO
         lb = 0
         ! Loop over atom group "g" and set MOs
         do g = 1,size(self%atom_group)
            mess = "group "//trim(to_str_(g))
            DIE_IF(NOT allocated(self%mol(g)%density_mx),trim(mess)//"MOLECULE.SCF:merge_atom_group_DMs ...  density matrix undefined!")
            if (no_of_electrons_(self%mol(g))>=1) then

               ! Sizes
               nb = self%mol(g)%n_bf
               ! MO/GO counters
               fb = lb+1
               lb = lb+nb
               ! Copy the block
               self%density_mx%r(fb:lb,fb:lb) = self%mol(g)%density_mx%r
            end if
         end do
      end if
   end subroutine

   subroutine merge_group_MOs(self)
   ! Merge the group MOs into the supermolecule MOs.
   ! All occupied first, then all virtuals, as columns.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: mess
      INT :: g,nb,no,nv
      INT :: fb,lb,fo,lo,fv,lv
      BIN :: all_restricted
      STR(len=STR_SIZE) :: knd
   ENSURE(allocated(self%atom_group),"MOLECULE.SCF:merge_group_MOs ... no atom_group info")
   ENSURE(allocated(self%mol),"MOLECULE.SCF:merge_group_MOs ... no .mol")
   ENSURE(size(self%mol)==size(self%atom_group),"MOLECULE.SCF:merge_group_MOs ... wrong size, .mol")

      ! Check if ANY of the atom groups has an unrestricted wavefunction
      all_restricted = TRUE
      do g = 1,size(self%atom_group)
         knd = genre_(self%mol(g)%MOs)
         if (knd=="u ") then
            all_restricted = FALSE
            exit
         end if
      end do
      if (allocated(self%MOs)) then
         call destroy_(self%MOs)
      end if
      if (all_restricted) then

         ! If not, use the restricted case
         call merge_group_MOs_r_(self)
      else
         call create_(self%MOs,self%n_bf,"u ")
         ! Alpha part
         self%MOs%a = ZERO

         ! MO/GO counters
         lb = 0
         lo = 0
         lv = self%n_a

         ! Loop over atom group "g" and set MOs
         do g = 1,size(self%atom_group)
            mess = "group "//trim(to_str_(g))
            DIE_IF(NOT allocated(self%mol(g)%MOs),trim(mess)//"MOLECULE.SCF:merge_group_MOs ...  MOs not defined!")
            if (no_of_electrons_(self%mol(g))<1) cycle

            ! Sizes
            nb = self%mol(g)%n_bf
            no = self%mol(g)%n_a
            nv = nb-no  ! number of unoccupied orbitals

            ! MO/GO counters
            fb = lb+1
            lb = lb+nb
            fo = lo+1
            lo = lo+no
            fv = lv+1
            lv = lv+nv

            ! Copy occupied and virtuals
            if (allocated(self%mol(g)%MOs%a)) then
               self%MOs%a(fb:lb,fo:lo) = self%mol(g)%MOs%a(:,1:no)
               self%MOs%a(fb:lb,fv:lv) = self%mol(g)%MOs%a(:,no+1:nb)
            else
               self%MOs%a(fb:lb,fo:lo) = self%mol(g)%MOs%r(:,1:no)
               self%MOs%a(fb:lb,fv:lv) = self%mol(g)%MOs%r(:,no+1:nb)
            end if
         end do

         ! Beta part
         self%MOs%b = ZERO
         lb = 0
         lo = 0
         ! change in unrestricted case
         lv = self%n_b

         ! Loop over atom group "g" and set MOs
         do g = 1,size(self%atom_group)
            mess = "group "//trim(to_str_(g))
            DIE_IF(NOT allocated(self%mol(g)%MOs),trim(mess)//"MOLECULE.SCF:merge_group_MOs ...  MOs not defined!")
            if (no_of_electrons_(self%mol(g))<1) cycle

            ! Sizes
            nb = self%mol(g)%n_bf
            no = self%mol(g)%n_b
            nv = nb-no

            ! MO/GO counters
            fb = lb+1
            lb = lb+nb
            fo = lo+1
            lo = lo+no
            fv = lv+1
            lv = lv+nv

            ! Copy occupied and virtuals
            if (allocated(self%mol(g)%MOs%b)) then
               self%MOs%b(fb:lb,fo:lo) = self%mol(g)%MOs%b(:,1:no)
               self%MOs%b(fb:lb,fv:lv) = self%mol(g)%MOs%b(:,no+1:nb)
            else
               self%MOs%b(fb:lb,fo:lo) = self%mol(g)%MOs%r(:,1:no)
               self%MOs%b(fb:lb,fv:lv) = self%mol(g)%MOs%r(:,no+1:nb)
            end if
         end do
      end if
   end subroutine

   subroutine merge_group_MOs_r(self)
   ! Merge the group MOs into the supermolecule MOs.
   ! All occupied first, then all virtuals, as columns.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: mess
      INT :: g,nb,no,nv
      INT :: fb,lb,fo,lo,fv,lv
   ENSURE(allocated(self%atom_group),"MOLECULE.SCF:merge_group_MOs_r ... no atom_group info")
   ENSURE(allocated(self%mol),"MOLECULE.SCF:merge_group_MOs_r ... no .mol")
   ENSURE(size(self%mol)==size(self%atom_group),"MOLECULE.SCF:merge_group_MOs_r ... wrong size, .mol")
      call destroy_(self%MOs)
      call create_(self%MOs,self%n_bf,"r ")
      self%MOs%r = ZERO
      lb = 0
      lo = 0
      ! change in unrestricted case
      lv = self%n_a

      ! Loop over atom group "g" and set MOs
      do g = 1,size(self%atom_group)
         mess = "group "//trim(to_str_(g))
         DIE_IF(NOT allocated(self%mol(g)%MOs),trim(mess)//"MOLECULE.SCF:merge_group_MOs_r ...  MOs not defined!")
         if (no_of_electrons_(self%mol(g))<1) cycle

         ! Sizes
         nb = self%mol(g)%n_bf
         no = self%mol(g)%n_a
         nv = nb-no

         ! MO/GO counters
         fb = lb+1
         lb = lb+nb
         fo = lo+1
         lo = lo+no
         fv = lv+1
         lv = lv+nv

         ! Copy occupied and virtuals
         self%MOs%r(fb:lb,fo:lo) = self%mol(g)%MOs%r(:,1:no)
         self%MOs%r(fb:lb,fv:lv) = self%mol(g)%MOs%r(:,no+1:nb)
      end do
   end subroutine

   subroutine symmorthonormalize_occupied_MOs(self)
   ! Symmetrically orthonormalize the occupied MO's. The occupied
   ! orbitals are the first .n_a orbitals. The remaining virtual
   ! orbitals are schmidt orthonormalized to the occupied MOs.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%MOs),"MOLECULE.SCF:symmorthonormalize_occupied_MOs ... no MO's")
      if (genre_(self%MOs)=="u ") then
         call symmorthonormalize_occupied_MOs_u_(self)
      else
         call symmorthonormalize_occupied_MOs_r_(self)
      end if
   end subroutine

   subroutine symmorthonormalize_occupied_MOs_r(self)
   ! Symmetrically orthonormalize the occupied MO's. The occupied
   ! orbitals are the first .n_a orbitals. The remaining virtual
   ! orbitals are schmidt orthonormalized to the occupied MOs.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: MO
      INT :: no,nv
   ENSURE(allocated(self%MOs),"MOLECULE.SCF:symmorthonormalize_occupied_MOs_r ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.SCF:symmorthonormalize_occupied_MOs_r ... no rhf MO's")
      no = self%n_a
      nv = self%n_bf-no

      ! Make sure overlap exists
      call make_overlap_mx_(self)

      ! Symmetric orthonormalize the occupied MO's
      call create_(MO,self%n_bf,no)
      MO = self%MOs%r(:,1:no)
      call symmetrically_orthonormalize_(MO,self%overlap_mx)
      self%MOs%r(:,1:no) = MO
      call destroy_(MO)

      ! Schmidt orthonormalize the virtuals to occupieds
      call create_(MO,self%n_bf,nv)
      MO = self%MOs%r(:,no+1:)
      call schmidt_orthonormalize_(MO,self%overlap_mx)
      self%MOs%r(:,no+1:) = MO
      call destroy_(MO)
   end subroutine

   subroutine symmorthonormalize_occupied_MOs_u(self)
   ! Symmetrically orthonormalize the occupied MO's. The occupied
   ! orbitals are the first .n_a orbitals. The remaining virtual
   ! orbitals are schmidt orthonormalized to the occupied MOs.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: MO
      INT :: no,nv
   ENSURE(allocated(self%MOs),"MOLECULE.SCF:symmorthonormalize_occupied_MOs_u ... no MOs")
   ENSURE(allocated(self%MOs%a),"MOLECULE.SCF:symmorthonormalize_occupied_MOs_u ... no alpha MOs")

      ! Alpha case
      no = self%n_a
      nv = self%n_bf-no

      ! Make sure overlap exists
      call make_overlap_mx_(self)

      ! Symmetric orthonormalize the occupied MO's
      call create_(MO,self%n_bf,no)
      MO = self%MOs%a(:,1:no)
      call symmetrically_orthonormalize_(MO,self%overlap_mx)
      self%MOs%a(:,1:no) = MO
      call destroy_(MO)

      ! Schmidt orthonormalize the virtuals to occupieds
      call create_(MO,self%n_bf,nv)
      MO = self%MOs%a(:,no+1:)
      call schmidt_orthonormalize_(MO,self%overlap_mx)
      self%MOs%a(:,no+1:) = MO
      call destroy_(MO)

      ! beta case
      no = self%n_b
      nv = self%n_bf-no

      ! Symmetric orthonormalize the occupied MO's
      call create_(MO,self%n_bf,no)
      MO = self%MOs%b(:,1:no)
      call symmetrically_orthonormalize_(MO,self%overlap_mx)
      self%MOs%b(:,1:no) = MO
      call destroy_(MO)

      ! Schmidt orthonormalize the virtuals to occupieds
      call create_(MO,self%n_bf,nv)
      MO = self%MOs%b(:,no+1:)
      call schmidt_orthonormalize_(MO,self%overlap_mx)
      self%MOs%b(:,no+1:) = MO
      call destroy_(MO)
   end subroutine


!  ==============
!  Atom group SCF
!  ==============

   subroutine atom_group_SCF(self)
   ! Reads a progroup .density_mx from the archive, otherwise
   ! makes it. Then convert it to the right kind for an SCF calc.
      type(MOLECULE_TYPE), INOUT :: self
      type(ARCHIVE_TYPE) :: arch
      call set_(arch,self%name,"group_density_mx",genre="r ")
      if (exists_(arch)) then

         ! Read the answer?
         call destroy_(self%density_mx)
         call create_(self%density_mx,self%n_bf,"r ")
         call read_(arch,self%density_mx)
      else

         ! Make the answer
         call do_atom_group_SCF_(self)
      end if

      ! Convert
      call convert_to_(self%density_mx,spinorbital_genre_(self%SCF_data),factor=HALF)
   end subroutine

   subroutine make_progroup_density_mx(self)
   ! Make the density matrix which is a sum of density matrices for
   ! each group specified in the molecule in the atom_group array.
   ! This will destroy any existing restricted density matrix archive.
      type(MOLECULE_TYPE), INOUT :: self
      type(ARCHIVE_TYPE) :: archive
      type(OPMATRIX_TYPE), allocatable :: P

      ! Leaky
      call do_atom_group_SCF_(self)

      ! Archive as the density_mx
      call create_(P,self%n_bf)
      call set_(archive,self%name,"group_density_mx",genre="r ")
      call read_(archive,P,genre="r ")
      call set_(archive,self%name,"density_mx",genre="r ")
      call write_(archive,P,genre="r ")
      call destroy_(P)
   end subroutine

   subroutine do_atom_group_SCF(self,MOs,output)
   ! Make a ".density matrix" from the sum of atom_group densities.
   ! If "MOs" is present and true, make ".MOs" which
   ! are the columns of orbitals from each group and also make the
   ! ".NO_occ_nos" vector set to 2 for the occupied group
   ! MO's.  If output is FALSE, the density matrix is not archived
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: MOs
      BIN, optional, IN :: output
      BIN :: do_output,do_MOs
      type(ARCHIVE_TYPE) :: arch
      INT :: g
   ENSURE(allocated(self%atom_group),"MOLECULE.SCF:do_atom_group_SCF ... no .atom_group info")
   ENSURE(allocated(self%mol),"MOLECULE.SCF:do_atom_group_SCF ... no .mol")
   ENSURE(size(self%mol)==size(self%atom_group),"MOLECULE.SCF:do_atom_group_SCF ... no .mol")
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:do_atom_group_SCF ... no SCF_data")

      ! Optional switches
      do_output = TRUE
      if (present(output)) do_output = output
      do_MOs = FALSE
      if (present(MOs)) do_MOs = MOs

      ! Destroy this density matrix
      DIE_IF(spinorbital_genre_(self%SCF_data)/="r ","MOLECULE.SCF:do_atom_group_SCF ... only for restricted DM so far")
      call destroy_(self%density_mx)
      call create_(self%density_mx,self%n_bf,spinorbital_genre_(self%SCF_data))
      self%density_mx%r = ZERO

      ! Destroy the MO's if requested
      if (do_MOs) then
         call destroy_(self%MOs)
         call create_(self%MOs,self%n_bf,"r ")
         self%MOs%r = ZERO
         call destroy_(self%NO_occ_nos)
         call create_(self%NO_occ_nos,self%n_bf,"r ")
         self%NO_occ_nos%r = ZERO
      end if

      ! Make the ANOs: atom groups need it (leaky)
      call make_ANOs_and_interpolators_(self)

      ! Update ANOs in atom_group(:).mol's
      call update_group_crystal_and_ANOs_(self)

      ! Loop over atom group "g"
      do g = 1,size(self%atom_group)

         ! Skip SCF for atoms without electrons
         if (no_of_electrons_(self%mol(g))<1) cycle

         ! Set SCF options
      ! if (.atom_group(g).n_atoms>1) then
      !    genre = mol.SCF_data.spinorbital_genre
      !    if (mol.put_archive_exists("MOs",genre)) then;
      !       mol.SCF_data.set_initial_MOs(genre)
      !    else
      !       mol.SCF_data.set_initial_density("promolecule")
      !    end
      ! end

         ! Output details?
         if (do_output) call put_basics_(self%mol(g))

         ! Do the SCF !!!!!!!!!!!!!!!!!!!!!
         call set_level_shift_(self%mol(g)%SCF_data,100d0)
         call SCF_(self%mol(g))
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         ! Make the AO density matrix for this group
         if (self%atom_group(g)%n_MOs>0) then
            call show_(stdout,"using fewer MOs =",self%atom_group(g)%n_MOs)
            call text_(stdout,"normal P:")
            call make_r_density_mx_(self%mol(g))
            call put_(stdout,self%mol(g)%density_mx%r)
            call text_(stdout,"reduced P:")
            call make_SCF_density_mx_(self%mol(g),method="group",n_a=self%atom_group(g)%n_MOs)
            call put_(stdout,self%mol(g)%density_mx%r)
         else
            call make_r_density_mx_(self%mol(g))
         end if

         ! Copy this group density matrix
         call set_group_density_(self,g,self%mol(g))

         ! Copy this group's MO's and occupations
         if (do_MOs) then
            call set_group_MOs_(self,g,self%mol(g))
         end if

         ! Clean up files
         call cleanup_scf_(self%mol(g))
      end do
      if (do_output) then
         call set_(arch,self%name,"group_density_mx",genre="r ")
         call write_(arch,self%density_mx,genre="r ")
      end if
      if (do_output AND do_MOs) then
         call set_(arch,self%name,"group_MOs",genre="r ")
         call write_(arch,self%MOs,genre="r ")
      end if
   end subroutine

   subroutine fragment_SCF(self,restart,do_SFs,spin_density)
   ! Do an fragment SCF calculation on each of the .atom_group.mol
   ! molecules and calculate the structure factors.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: restart
      BIN, optional, IN :: do_SFs
      BIN, optional, IN :: spin_density

      ! The fragments integrate on the parent's grid settings, and this must
      ! precede the .mol(g).initialize_DFT_grids calls in both routines below
      call set_group_becke_grids_(self)
      if (tonto%n_processors<=1) then
         call fragment_SCF_norm_(self,restart,do_SFs,spin_density)
      else
         call fragment_SCF_para_(self,restart,do_SFs,spin_density)
      end if

      ! Wait for every rank to finish before anyone reads. Both routines above
      ! write each fragment's <tag>-SFs files from whichever rank owns that
      ! fragment, and the readers want all of them.
      !
      ! This is the right level for it: fragment_SCF is called collectively --
      ! every rank reaches it -- whereas the per-fragment
      ! get_Hirshfeld_atom_FFs_disk calls inside those two routines are
      ! rank-local, and a barrier there would deadlock. See the note in
      ! MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk.
      PARALLEL_BARRIER
   end subroutine

   subroutine fragment_SCF_norm(self,restart,do_SFs,spin_density)
   ! Do an fragment SCF calculation on each of the .atom_group.mol
   ! molecules and calculate the structure factors. 
   ! If "restart" is present and TRUE then fragHAR fragment SCF's are
   ! restarted with MOs/density matrices from the previous iteration.
   ! Otherwise they are restart as requested in SCF_data block using
   ! most likely "promolecule".
   ! If "spin_density" is present magnetic structure factors are
   ! calculated instead of ED structure factors.
   ! If "do_SFs" is FALSE do not calculate the atomic form factors.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: restart
      BIN, optional, IN :: do_SFs
      BIN, optional, IN :: spin_density

! ENSURE(.atom_group.allocated,"no .atom_group")
! ENSURE(.mol.allocated,"no .mol")
! ENSURE(.mol.dim==.atom_group.dim,"no .mol")
! ENSURE(.SCF_data.allocated,"no SCF_data")
! ENSURE(.crystal.allocated,"no crystal")
! ENSURE(.cif.allocated,"no cif specified")
      INT :: n_group,g
      BIN :: SFs
      STR(len=STR_SIZE) :: name,guess

      ! Do structure factors?
      SFs = TRUE
      if (present(do_SFs)) SFs = do_SFs

      ! Need these for SFs
      if (SFs) then
         call make_ANOs_and_interpolators_(self)
         call delete_Hirshfeld_atom_FFs_(self)
      end if

      ! Output details?
      call put_atom_group_mols_(self)
      do g = 1,size(self%atom_group)

         ! Set up a molecule for this group of atoms
         self%mol(g)%name = self%atom_group(g)%name

         ! Skip SCF for atoms without electrons
         if (no_of_electrons_(self%mol(g))<1) cycle

         ! Make crystal
         if (NOT allocated(self%mol(g)%crystal)) then
            call update_fragment_info_(self%mol(g)%crystal,self%mol(g)%atom)
         end if

         ! Set up SCF options
      ! mol.set_SCF_guess_defaults_from(.SCF_data)
      ! if (.SCF_data.SCF_kind=="fragment-rks") then
      !    mol.becke_grid.destroy
      !    mol.make_atom_partition_info
      !  ! mol.set_up_becke_grid
      ! end
         call set_output_(self%mol(g)%SCF_data,TRUE)
         if (size(self%mol(g)%atom)==1) then
            call set_initial_density_(self%mol(g)%SCF_data,"core")
         else
            call set_initial_MOs_(self%mol(g)%SCF_data,self%SCF_data%initial_MOs)
            call set_initial_density_(self%mol(g)%SCF_data,self%SCF_data%initial_density)
         end if
         if (present(restart)) then
            if (restart) then
               guess = spinorbital_genre_(self%mol(g)%SCF_data)
               call set_initial_density_(self%mol(g)%SCF_data,guess)
            end if
         end if

         ! SCF !!!!!!!!!!!!!!!!!!!!!!!!!!!!
         call set_per_rank_IO_allowed_(tonto,TRUE)
         name = trim(self%mol(g)%name)
         call redirect_(stdout,name)
         call move_to_end_(stdout)
         call usual_SCF_(self%mol(g))  ! <<<
         call revert_(stdout)
         call set_per_rank_IO_allowed_(tonto,FALSE)
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if (SFs) then

            ! NOTE: change DFT -> becke
            call initialize_DFT_grids_(self%mol(g))

            ! Assign previously made ANOs
            call resolve_ANOs_from_(self%mol(g),self)

            ! Write SFs for each fragment to disk - in parallel
            n_group = size(self%atom_group(g)%atom_index)
            call set_per_rank_IO_allowed_(tonto,TRUE)
            call get_Hirshfeld_atom_FFs_disk_(self%mol(g),spin_density=spin_density)
         ! .mol(g).get_Hirshfeld_atom_FFs(n_group,spin_density)
            call set_per_rank_IO_allowed_(tonto,FALSE)
         end if
      end do
   end subroutine

   subroutine fragment_SCF_para(self,restart,do_SFs,spin_density)
   ! Do a fragment SCF calculation on each of the .atom_group.mol molecules and
   ! calculate the structure factors. Parallel version; FRAGMENT_SCF_NORM is the
   ! serial one and FRAGMENT_SCF picks between them on .n_processors.
   !
   ! HOW THE WORK IS SHARED. There are TWO strategies here, chosen on rank count,
   ! and they are genuinely different algorithms -- which is why results are not
   ! comparable between them, and why any parallel fragHAR test must pin a rank
   ! count.
   !
   !   n_processors == 2   a plain `parallel do g` -- static cyclic. With only
   !                       two ranks a dedicated scheduler would leave ONE
   !                       worker, which is worse than cyclic.
   !
   !   n_processors >  2   a dynamic master/worker pool, below.
   !
   ! THE POOL. Fragments differ enormously in cost -- a two-atom cap against a
   ! thirty-atom residue -- so static distribution leaves ranks idle behind the
   ! one big fragment. Instead:
   !
   !   1. `.mol` is sorted by basis-function count, LARGEST FIRST, into
   !      tonto.p_loop_list (`n_bf.quick_sort(...,decreasing_order=TRUE)`).
   !      That is longest-processing-time-first: start the expensive fragments
   !      early so the short ones fill the tail. Built once and cached -- the
   !      `if (tonto.p_loop_list.deallocated)` guard -- since it is reused every
   !      HAR cycle and the fragments do not change size.
   !
   !   2. Ranks then take the next fragment off a SHARED COUNTER held in the
   !      master's memory, via one-sided MPI RMA: PARALLEL:setup_p_loop makes an
   !      MPI window over .p_loop_index, and PARALLEL:next_p_loop_index does an
   !      MPI_Win_lock_exclusive + MPI_Get_accumulate (atomic fetch-and-add) on
   !      it. So a rank that finishes early immediately takes more work; nothing
   !      is pre-assigned.
   !
   !   3. The MASTER DOES NO SCIENCE -- `if (tonto.is_master_processor) cycle`.
   !      It hosts the window and hands out indices. That is the cost of this
   !      scheme: at 3 ranks a third of the machine is scheduling. It is also
   !      exactly why the 2-rank case takes the other branch.
   !
   ! setup_p_loop also does `.lock_parallel_do("no")`, which is what makes any
   ! `parallel do` INSIDE a fragment run serially on its owning rank -- the
   ! "MPI on the outside" rule. So .mol(g)'s SCF, integrals and grids are
   ! ordinary serial code from that rank's point of view.
   !
   ! WHY THE ASSIGNMENT ORDER DOES NOT MOVE THE NUMBERS. Each fragment's form
   ! factors are written to its own <tag>-SFs archive, keyed by atom tag, not
   ! accumulated into a shared array. So which rank did which fragment cannot
   ! change the result. The rank-count sensitivity is the two-strategy split
   ! above, not scheduling non-determinism.
   !
   ! THE CALLS IN HERE ARE RANK-LOCAL, which matters for collectives: rank r
   ! executes the loop body only for the fragments it draws, and the master
   ! never executes it at all. A barrier, broadcast or reduction placed in the
   ! body -- or in anything it calls, such as
   ! MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk -- would run a different number of
   ! times on different ranks and deadlock. Collectives for this phase belong at
   ! the end of FRAGMENT_SCF, which every rank reaches.
   !
   ! KNOWN ROUGH EDGES (see TASKS_AND_HISTORY.md, "fragment_SCF_para"):
   !   . PARALLEL:next_p_loop_index evaluates `.p_loop_list(.p_loop_index)`
   !     BEFORE the caller's exit test -- and its own comment says it must be
   !     called first -- so on the final draw it indexes one past the end. Read
   !     and discarded in an optimised build; a -fcheck=bounds MPI build should
   !     abort there.
   !   . The window is created per call and the list cached globally on `tonto`,
   !     so two different fragment sets in one run would reuse a stale list.
   !   . The long-term shape is sub-communicators (MPI_Comm_split): give each
   !     fragment a GROUP of ranks, parallelise within a fragment as well as
   !     across them, and stop donating a whole rank to scheduling. That would
   !     dissolve both branches.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: restart
      BIN, optional, IN :: do_SFs
      BIN, optional, IN :: spin_density

! ENSURE(.atom_group.allocated,"no atom_group info")
! ENSURE(.SCF_data.allocated,"no SCF_data")
! ENSURE(.crystal.allocated,"no crystal")
! ENSURE(.cif.allocated,"no cif specified")
      BIN :: SFs
      STR(len=STR_SIZE) :: name,guess
      INT :: g,n_group,x,w
      VEC(INT,:), allocatable :: n_bf

      ! Do structure factors?
      SFs = FALSE
      if (present(do_SFs)) SFs = do_SFs

      ! Need these for SFs
      if (SFs) call make_ANOs_and_interpolators_(self)

      ! Output details?
      call put_atom_group_mols_(self)

      ! Loop over atom group "g" -- dynamic pool above 2 ranks, cyclic at 2.
      ! See the header for why the split exists and what it costs.
      if (tonto%n_processors>2) then

         ! Longest-processing-time-first order, built once and cached on tonto.
         if (NOT allocated(tonto%p_loop_list)) then
            call destroy_(tonto%p_loop_list)
            call create_(tonto%p_loop_list,size(self%mol))
            call create_(n_bf,size(self%mol))
            do x = 1,size(self%mol)
               n_bf(x) = self%mol(x)%n_bf
            end do
            call quick_sort_(n_bf,tonto%p_loop_list,decreasing_order=TRUE)
            call destroy_(n_bf)
         end if
         call setup_p_loop_(tonto,w,lb=1,ub=size(self%atom_group))

         ! PER-RANK I/O for the whole loop -- see the 2-rank branch below for
         ! why. Set outside the loop and on EVERY rank, including the master,
         ! which schedules and cycles without ever entering the body.
         call set_per_rank_IO_allowed_(tonto,TRUE)
         do

            ! NOTE: call this before exit conditions
            g = next_p_loop_index_(tonto,w)

            ! Exit if no more to do, or master.
            ! NOTE: Must be in this order! next_p_loop_index above performs the
            ! atomic fetch-and-add, so every rank has to call it before anyone
            ! tests the result -- skipping it on the master would stall the
            ! counter. The master then cycles: it schedules, it does no science.
            if (exit_p_loop_(tonto)) exit
            if (is_master_processor_(tonto)) cycle

            ! Skip SCF for atoms without electrons
            if (no_of_electrons_(self%mol(g))<1) cycle

            ! Make crystal
            if (NOT allocated(self%mol(g)%crystal)) then
               call update_fragment_info_(self%mol(g)%crystal,self%mol(g)%atom)
            end if
            call set_output_(self%mol(g)%SCF_data,TRUE)
            if (size(self%mol(g)%atom)==1) then
               call set_initial_density_(self%mol(g)%SCF_data,"core")
            else
               call set_initial_MOs_(self%mol(g)%SCF_data,self%SCF_data%initial_MOs)
               call set_initial_density_(self%mol(g)%SCF_data,self%SCF_data%initial_density)
            end if
            if (present(restart)) then
               if (restart) then
                  guess = spinorbital_genre_(self%mol(g)%SCF_data)
                  call set_initial_density_(self%mol(g)%SCF_data,guess)
               end if
            end if

            ! SCF !!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! NOTE: no set_per_rank_IO_allowed here -- see the 2-rank branch.
            name = trim(self%mol(g)%name)
            call redirect_(stdout,name)
            call move_to_end_(stdout)
            call usual_SCF_(self%mol(g))  ! <<<
            call revert_(stdout)
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if (SFs) then

               ! NOTE: change DFT -> becke
               call initialize_DFT_grids_(self%mol(g))

               ! Assign previously made ANOs
               call resolve_ANOs_from_(self%mol(g),self)

               ! Write SFs for each fragment to disk - in parallel
               n_group = size(self%atom_group(g)%atom_index)
               call get_Hirshfeld_atom_FFs_disk_(self%mol(g),spin_density=spin_density)
            end if
         end do
         call unlock_parallel_do_(tonto,"no")

         ! Back to shared I/O: the ranks resynchronise here.
         call set_per_rank_IO_allowed_(tonto,FALSE)
      else

         ! Exactly 2 ranks: static cyclic. A dedicated master here would leave a
         ! single worker, i.e. slower than serial plus the RMA overhead.
         !
         ! PER-RANK I/O for the whole loop. Each rank runs a different fragment,
         ! so it opens, writes and closes its OWN files -- and the I/O layer's
         ! bookkeeping broadcasts (unit, io_status, record, the text buffer) must
         ! be skipped, or the ranks synchronise on state that is deliberately
         ! different and fall out of step. That was the MPI_ERR_TRUNCATE at
         ! "Making F_pred".
         !
         ! Set OUTSIDE the loop, on purpose: the mode has to be identical on
         ! every rank, including one that draws zero iterations.
         call set_per_rank_IO_allowed_(tonto,TRUE)
         do g = PARALLEL_DO_START(1,1),size(self%mol),PARALLEL_DO_STRIDE(1)
         LOCK_PARALLEL_DO("MOLECULE.SCF:fragment_SCF_para")

            ! Skip SCF for atoms without electrons
            if (no_of_electrons_(self%mol(g))<1) cycle

            ! Make crystal
            if (NOT allocated(self%mol(g)%crystal)) then
               call update_fragment_info_(self%mol(g)%crystal,self%mol(g)%atom)
            end if
            call set_output_(self%mol(g)%SCF_data,TRUE)
            if (size(self%mol(g)%atom)==1) then
               call set_initial_density_(self%mol(g)%SCF_data,"core")
            else
               call set_initial_MOs_(self%mol(g)%SCF_data,self%SCF_data%initial_MOs)
               call set_initial_density_(self%mol(g)%SCF_data,self%SCF_data%initial_density)
            end if
            if (present(restart)) then
               if (restart) then
                  guess = spinorbital_genre_(self%mol(g)%SCF_data)
                  call set_initial_density_(self%mol(g)%SCF_data,guess)
               end if
            end if

            ! SCF !!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! NOTE: no set_per_rank_IO_allowed here. The mode is set once,
            ! outside the loop. Toggling it back to FALSE mid-body -- which is
            ! what this used to do -- re-enables the bookkeeping broadcasts
            ! while the two ranks are on *different* fragments, and they then
            ! block on each other's records. That was the MPI_ERR_TRUNCATE.
            name = trim(self%mol(g)%name)
            call redirect_(stdout,name)
            call move_to_end_(stdout)
            call usual_SCF_(self%mol(g))  ! <<<
            call revert_(stdout)
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if (SFs) then

               ! NOTE: change DFT -> becke
               call initialize_DFT_grids_(self%mol(g))

               ! Assign previously made ANOs
               call resolve_ANOs_from_(self%mol(g),self)

               ! Write SFs for each fragment to disk - in parallel
               n_group = size(self%atom_group(g)%atom_index)
               call get_Hirshfeld_atom_FFs_disk_(self%mol(g),spin_density=spin_density)
            end if
         end do
         UNLOCK_PARALLEL_DO("MOLECULE.SCF:fragment_SCF_para")

         ! Back to shared I/O: the ranks resynchronise here.
         call set_per_rank_IO_allowed_(tonto,FALSE)
      end if
      call barrier_(tonto)
   end subroutine


!  ===========
!  Monomer MOs
!  ===========

   subroutine make_monomer_MOs(self)
   ! Make the monomer MOs from symmetrically orthonormalized group
   ! (monomer) molecular orbitals. The promolecule MOs are ordered by
   ! group.  The orbitals and the corresponding promolecule density
   ! are *not* archived.  This will destroy any existing restricted
   ! density matrix and MOs.
      type(MOLECULE_TYPE), INOUT :: self
      call do_atom_group_SCF_(self,MOs=TRUE,output=FALSE)
   end subroutine

   subroutine make_promol_MOs(self)
   ! Make the promolecule MOs from symmetrically orthonormalized group
   ! (monomer) molecular orbitals. The promolecule orbitals are
   ! ordered by group.  The orbitals and the corresponding density are
   ! *not* archived.  This will destroy any existing restricted
   ! density matrix and MOs.
      type(MOLECULE_TYPE), INOUT :: self
      call do_atom_group_SCF_(self,MOs=TRUE,output=FALSE)
      call symorthonormalize_occupied_MOs_(self)
   end subroutine


!  =============
!  Make ANO data
!  =============

   subroutine make_ANOs_and_interpolators(self,output)
   ! Make the atomic natural orbitals (ANOs) *and* the corresponding
   ! spherically-averaged interpolators for the molecule.
   ! NOTE: this applies only to molecules with more than one atom.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: output
      if (self%n_atom<=1) return

      ! Atomic natural orbitals, atomic energy, atomic density
      ! matrices, and atomic MO's
      call make_ANOs_(self,output)

      ! ANO interpolators (leaky)
      call make_ANO_interpolators_(self)  ! NOT PURE

      ! Atom shell-basis-function-limit info
      call update_atom_shell_info_(self)
   end subroutine

   subroutine make_ANOs(self,output)
   ! Get the restricted atomic natural orbitals (ANO) data and
   ! corresponding atomic-SCF data for all atoms in the molecule.
   ! NOTE: do not use this, prefer make_ANOs_and_interpolators.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: output
      INT :: a,b
      VEC(INT,:), allocatable :: indx
      VEC(BIN,:), allocatable :: conv,done
      BIN :: first,converged
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      BIN :: show
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:make_ANOs ... No SCF_data")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_ANOs ... no atoms")
   ENSURE(self%atom_info_made,"MOLECULE.SCF:make_ANOs ... no atom info")
   ENSURE(allocated(self%basis),"MOLECULE.SCF:make_ANOs ... no basis set")
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_ANOs ... no basis_set info")
   ENSURE(self%n_atom_kind>0,"MOLECULE.SCF:make_ANOs ... no atom kinds set")

      ! Verbose output
      show = FALSE
      if (present(output)) show = output
      call set_basis_kind_(self%atom,"gaussian")

      ! Already there?
      if (has_all_ANO_matrices_(self)) return
      call create_(done,self%n_atom)
      done = FALSE
      first = TRUE
      do a = 1,self%n_atom

         ! ANOs made?
         if (done(a)) cycle
         if (NOT has_ANO_matrices_(self%atom(a))) then
            call make_ANOs_for_atom_(self,a,converged)
            call append_(indx,a)
            call append_(conv,converged)
            if (first) then
               if (NOT show) then  ! Print only for non-verbose
                  call flush_(stdout)
                  call text_(stdout,"Making gaussian ANO data ...")
                  first = FALSE
               end if
            end if
         end if
         done(a) = TRUE

         ! Set atoms of same kind
         do b = a+1,self%n_atom

            ! Right kind?
            if (done(b)) cycle
            if (NOT is_same_kind_as_(self%atom(b),self%atom(a))) cycle
            call set_is_unique_kind_(self%atom(b),FALSE)
            self%atom(b)%density_mx = self%atom(a)%density_mx
            self%atom(b)%NOs = self%atom(a)%NOs
            self%atom(b)%NO_occ_nos = self%atom(a)%NO_occ_nos
            self%atom(b)%AOs = self%atom(a)%AOs
            call set_energy_(self%atom(b),self%atom(a)%energy)
            done(b) = TRUE
         end do
      end do
      call destroy_(done)

      ! Table headings
      if (show) then
         call flush_(stdout)
         call text_(stdout,"===============================")
         call text_(stdout,"Table of atomic SCF calculation")
         call text_(stdout,"===============================")
         call flush_(stdout)
         call text_(stdout,"Summary of unique atomi SCFs needed for spherical atomic densities")
         call flush_(stdout)
         call create_(table,3)
         call set_heading_(table(1),"ID")
         call set_values_(table(1),self%atom(indx(:))%label)
         call set_real_precision_(table(2),1)
         call set_heading_(table(2),"Z")
         call set_values_(table(2),self%atom(indx(:))%nuclear_charge)
         call set_heading_(table(3),"SCF")
         call set_subhead_(table(3),"OK?")
         call set_values_(table(3),conv)
         call put_(table)
         call destroy_(table)
      end if

      ! An atomic SCF that did not converge leaves ANOs built from whatever the
      ! last iterate happened to be. That is not a small error: the result then
      ! depends on the BLAS/LAPACK implementation and even on compiler code
      ! generation, and it silently corrupts every quantity derived from the
      ! atomic projectors (Roby indices, Salvador and Hirshfeld partitioning,
      ! atomic charges/dipoles) while leaving molecular totals looking correct.
      ! Refuse to continue rather than emit meaningless numbers.
      if (allocated(conv)) then
         if (NOT all(conv)) then
            call flush_(stdout)
            call text_(stdout,"* * * The atomic SCF did NOT converge for these atoms: * * *")
            do a = 1,size(conv)
               if (conv(a)) cycle
               call text_(stdout,"   "//trim(self%atom(indx(a))%label))
            end do
            call flush_(stdout)
            call text_(stdout,"Atoms with a partly-filled degenerate shell (e.g. p1, p2) may need the")
            call text_(stdout,"guess SCF to keep damping and level shifting well past the DIIS handover.")
            call text_(stdout,"Set them with:")
            call text_(stdout,"   scfdata= { use_default_guess_options= FALSE")
            call text_(stdout,"              max_iterations= ...  damp_factor= ...  damp_finish= ...")
            call text_(stdout,"              level_shift= ...     level_shift_finish= ... }")
            call flush_(stdout)
            DIE("MOLECULE.SCF:make_ANOs ... atomic SCF for the ANOs did not converge; the atomic projectors would be meaningless")
         end if
      end if
      call destroy_(conv)
      call destroy_(indx)
      DIE_IF(NOT has_all_ANO_matrices_(self),"MOLECULE.SCF:make_ANOs ... ANO's were not made!")
   end subroutine

   subroutine make_ANOs_for_atom(self,a,converged)
   ! Make the spherically averaged restricted atomic natural orbitals
   ! (ANO) and density matrix data and interpolator for atom "a".
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: a
      BIN, optional, OUT :: converged
      type(MOLECULE_TYPE), allocatable :: mol
   ENSURE(NOT allocated(self%atom(a)%NOs),"MOLECULE.SCF:make_ANOs_for_atom ... NO's exist! Atom "//trim(to_str_(a)))
   ENSURE(NOT allocated(self%atom(a)%NO_occ_nos),"MOLECULE.SCF:make_ANOs_for_atom ... OCCs exist! Atom "//trim(to_str_(a)))
   ENSURE(NOT allocated(self%atom(a)%density_mx),"MOLECULE.SCF:make_ANOs_for_atom ... DM exists! Atom "//trim(to_str_(a)))

      ! Create the molecule
      call create_(mol)
      call set_molecule_from_atom_(self,a,mol)

      ! Reset some SCF options
      call set_using_direct_scf_(mol%SCF_data,TRUE)  ! use direct scf
      call set_is_guess_(mol%SCF_data,TRUE)  ! don't use cluster charges
      call set_delete_all_archives_(mol%SCF_data,TRUE)

      ! A spherical ensemble instead of the average after the SCF?
      call set_atomic_SCF_ensemble_(self,mol)

      ! Output?
      if (self%SCF_data%guess_output) call put_basics_(mol)

      ! Do the atomic SCF
      call atomic_SCF_(mol,converged)

      ! Eliminate core electrons from DM?
      ! Bespoke DM code
      ! Add code

      ! Spherically average the AO density
      call make_r_density_mx_(mol)
      call spherically_average_(mol%atom(1),mol%density_mx%r,self%use_spherical_basis)

      ! Make the natural orbitals ... leaky
      call make_NOs_(mol)

      ! Clean up the SCF files
      call delete_SCF_archives_(mol)

      ! Destroy the atoms's NO  ... leaky
      call destroy_(self%atom(a)%AOs)
      call destroy_(self%atom(a)%density_mx)
      call destroy_(self%atom(a)%NO_occ_nos)
      call destroy_(self%atom(a)%NOs)

      ! Copy the results
      self%atom(a)%NOs = mol%NOs
      self%atom(a)%NO_occ_nos = mol%NO_occ_nos
      self%atom(a)%density_mx = mol%density_mx
      self%atom(a)%AOs = mol%MOs
      call set_energy_(self%atom(a),mol%SCF_data%energy)
      call set_atom_data_made_(self%atom(a),TRUE)

      ! Safe to destroy
      call destroy_(mol)
   end subroutine

   subroutine set_atomic_SCF_ensemble(self,mol)
   ! Set up "mol", a one-atom molecule, for the atomic SCF that ano_kind=
   ! asks for: "average" leaves it alone; "aoc" makes it an RHF average
   ! of configuration, "fon" a restricted KS with held fractional
   ! occupations, for an atom with one Aufbau open shell. A closed-shell
   ! atom, or one without a single open shell, is left alone.
      type(MOLECULE_TYPE), IN :: self
      type(MOLECULE_TYPE), INOUT :: mol
      STR(len=STR_SIZE) :: kind
      INT :: l,n,n_o
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:set_atomic_SCF_ensemble ... no SCF_data")
   ENSURE(allocated(mol%SCF_data),"MOLECULE.SCF:set_atomic_SCF_ensemble ... no SCF_data for the atom")
   ENSURE(size(mol%atom)==1,"MOLECULE.SCF:set_atomic_SCF_ensemble ... not a one-atom molecule")
      kind = self%SCF_data%ANO_kind
      if (kind=="average") return
      call ground_state_open_shell_(mol%atom(1)%atomic_number,l,n)
      if (l<0) then
         call flush_(stdout)
         call text_(stdout,"ano_kind= "//trim(kind)//": atom "//trim(mol%atom(1)%label)//" has no single open shell; its density is averaged after the SCF")
         return
      end if
      if (n==0) return
      n_o = 2*l+1
      select case (kind)
      case ("aoc")
         call set_SCF_kind_(mol%SCF_data,"rhf")
      case ("fon")
         DIE_IF(NOT is_DFT_calculation_(self%SCF_data),"MOLECULE.SCF:set_atomic_SCF_ensemble ... ano_kind= fon needs a DFT calculation")
         call set_SCF_kind_(mol%SCF_data,"rks")
         call set_exchange_functional_(mol%SCF_data,self%SCF_data%DFT_exchange_functional)
         call set_correlation_functional_(mol%SCF_data,self%SCF_data%DFT_correlation_functional)
      end select
      call set_open_shell_ensemble_(mol%SCF_data,n_o,n,AOC_coupling_(n_o,n))
      call set_using_delta_build_(mol%SCF_data,FALSE)
      call set_using_pFON_damping_(mol%SCF_data,FALSE)
   end subroutine

   subroutine make_NAOs(self)
   ! Get the restricted natural atomic orbitals (NAO) data
      type(MOLECULE_TYPE), INOUT :: self
      INT :: a
      BIN :: do_NAOs
   ENSURE(allocated(self%atom),"MOLECULE.SCF:make_NAOs ... no atoms")
   ENSURE(self%atom_info_made,"MOLECULE.SCF:make_NAOs ... no atom info")
   ENSURE(allocated(self%basis),"MOLECULE.SCF:make_NAOs ... no basis set")
   ENSURE(self%basis_info_made,"MOLECULE.SCF:make_NAOs ... no basis_set info")
   ENSURE(allocated(self%density_mx),"MOLECULE.SCF:make_NAOs ... no basis_set info")
      if (has_all_NAO_matrices_(self)) return

      ! Get restricted DM
      call make_r_density_mx_(self)

      ! NAOs or projected NAOs?
      do_NAOs = TRUE
      if (allocated(self%roby)) then
         if (self%roby%use_projected_NAOs) then
            do_NAOs = FALSE
         end if
      end if
      if (do_NAOs) then

         ! NBO stype NAOs
         do a = 1,self%n_atom
            call make_NAOs_for_atom_(self,a)
         end do
      else

         ! Projected NAOs from P_A \rho P_A, P_A = atomic projector
         do a = 1,self%n_atom
            call make_pNAOs_for_atom_(self,a)
         end do
      end if
   end subroutine

   subroutine make_NAOs_for_atom(self,a)
   ! Make the spherically averaged restricted atomic natural orbitals
   ! (ANO) and density matrix data and interpolator for atom "a".
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: a
      MAT(REAL,:,:), allocatable :: NAO,S,V,X,P
      VEC(REAL,:), allocatable :: occupation
      VEC(INT,1) :: group
      INT :: n_bf
   ENSURE(allocated(self%overlap_mx),"MOLECULE.SCF:make_NAOs_for_atom ... no overlap matrix")
   ENSURE(allocated(self%density_mx),"MOLECULE.SCF:make_NAOs_for_atom ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.SCF:make_NAOs_for_atom ... no restricted density matrix")

      ! Clean up
      call destroy_(self%atom(a)%NAOs)
      call destroy_(self%atom(a)%NAO_occupations)

      ! No. of basis functions in the group
      n_bf = self%atom(a)%basis%n_bf
      group = [a]

      ! Create the (full) NAO and occupation matrix
      call create_(NAO,n_bf,n_bf)
      call create_(occupation,n_bf)

      ! S = overlap matrix for group(:) basis
      call create_(S,n_bf,n_bf)
      call AO_subspace_set_(self,S,self%overlap_mx,group,group)

      ! Get V = S^(1/2)
      call create_(V,n_bf,n_bf)
      call to_sqrt_of_(V,S)

      ! Diagonal part of P matrix
      call create_(P,n_bf,n_bf)
      call AO_subspace_set_(self,P,self%density_mx%r,group,group)
      if (allocated(self%roby)) then
         if (self%roby%show_calcs) then
            call show_(stdout,"Atom = ",self%atom(a)%tag)
            call text_(stdout,"S:")
            call put_(stdout,S)
            call text_(stdout,"P:")
            call put_(stdout,P)
         end if
      end if
      if (allocated(self%roby)) then
         if (self%roby%use_spherical_averaging) then

         ! Symmetrize P
            call spherically_average_(self%atom(a),P,self%use_spherical_basis)
         end if
      end if
      if (allocated(self%roby)) then
         if (self%roby%show_calcs) then
            call text_(stdout,"P after symmetrization:")
            call put_(stdout,P)
         end if
      end if

      ! Transform P -> S^(1/2) P S^(1/2)
      call create_(X,n_bf,n_bf)
      call change_basis_to_(P,X,V)

      ! Diagonalize
      call solve_symmetric_eigenproblem_(X,occupation,P)

      ! Get V = S^(-1/2)
      call to_inverse_sqrt_of_(V,S)

      ! Back transform
      call to_product_of_(NAO,V,P)

      ! Clean
      call destroy_(X)
      call destroy_(P)
      call destroy_(V)
      call destroy_(S)

      ! Reverse order
      occupation = occupation(n_bf:1:-1)
      NAO = NAO(:,n_bf:1:-1)
      if (allocated(self%roby)) then
         if (self%roby%show_calcs) then
            call flush_(stdout)
            call text_(stdout,"NAO occupations:")
            call show_(stdout,"Atom #         = ",a)
            call show_(stdout,"Atom label     = ",trim(self%atom(a)%tag))
            call flush_(stdout)
            call put_(stdout,occupation)
            call text_(stdout,"NAO:")
            call put_(stdout,NAO)
         end if
      end if

      ! Assign
      self%atom(a)%NAOs = NAO
      self%atom(a)%NAO_occupations = occupation
   end subroutine

   subroutine make_pNAOs_for_atom(self,a)
   ! Make the projected restricted natural atomic orbitals (pNAOs).
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: a
      MAT(REAL,:,:), allocatable :: NAO,S,Smh,P,Q
      VEC(REAL,:), allocatable :: occupation
      VEC(INT,1) :: group
      INT :: n_bf
   ENSURE(NOT allocated(self%atom(a)%NAOs),"MOLECULE.SCF:make_pNAOs_for_atom ... NAO's exist! Atom "//trim(to_str_(a)))
   ENSURE(NOT allocated(self%atom(a)%NAO_occupations),"MOLECULE.SCF:make_pNAOs_for_atom ... NAO occs exist! Atom "//trim(to_str_(a)))
   ENSURE(allocated(self%overlap_mx),"MOLECULE.SCF:make_pNAOs_for_atom ... no overlap matrix")
   ENSURE(allocated(self%density_mx),"MOLECULE.SCF:make_pNAOs_for_atom ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.SCF:make_pNAOs_for_atom ... no restricted density matrix")

      ! No. of basis functions in the group
      n_bf = self%atom(a)%basis%n_bf
      group = [a]

      ! Create the (full) NAO and occupation matrix
      call create_(NAO,n_bf,n_bf)
      call create_(occupation,n_bf)

      ! Density matrices (see below)
      call create_(Q,n_bf,n_bf)
      call create_(P,n_bf,n_bf)

      ! S = S(:,a), overlap matrix for all,atom A
      call create_(S,self%n_bf,n_bf)
      call AO_subspace_set_(self,S,self%overlap_mx,col_atom=group)

      ! Transform density matrix D -> P = S^T D S
      call change_basis_to_(self%density_mx%r,P,S)
      if (allocated(self%roby)) then
         if (self%roby%show_calcs) then
            call show_(stdout,"Atom = ",self%atom(a)%tag)
            call text_(stdout,"S:")
            call put_(stdout,S)
            call text_(stdout,"P:")
            call put_(stdout,P)
         end if
      end if

      ! S_A = overlap matrix atom a
      call destroy_(S)
      call create_(Smh,n_bf,n_bf)
      call create_(S,n_bf,n_bf)
      call AO_subspace_set_(self,S,self%overlap_mx,group,group)

      ! Get (S_A)^-1
      call to_inverse_of_(Smh,S)

      ! Transform density P -> Q = (S_A)^-1 P (S_A)^-1
      call change_basis_to_(P,Q,Smh)
      if (allocated(self%roby)) then
         if (self%roby%show_calcs) then
            call put_(stdout,S)
            call text_(stdout,"Q:")
            call put_(stdout,Q)
         end if
      end if
      if (allocated(self%roby)) then
         if (self%roby%use_spherical_averaging) then

         ! Symmetrize P
            call spherically_average_(self%atom(a),Q,self%use_spherical_basis)
         end if
      end if
      if (allocated(self%roby)) then
         if (self%roby%show_calcs) then
            call text_(stdout,"Q after symmetrization:")
            call put_(stdout,Q)
         end if
      end if

      ! Get (S_A)^1/2
      call to_sqrt_of_(Smh,S)

      ! Transform density Q -> Q = (S_A)^1/2 Q (S_A)^1/2
      call change_basis_to_(Q,P,Smh)

      ! Diagonalize; now Q = eigenvectors
      call solve_symmetric_eigenproblem_(P,occupation,Q)

      ! Get (S_A)^-1/2
      call to_inverse_sqrt_of_(Smh,S)

      ! Back transform
      call to_product_of_(NAO,Smh,Q)

      ! Clean
      call destroy_(S)
      call destroy_(Smh)
      call destroy_(P)
      call destroy_(Q)

      ! Reverse order
      occupation = occupation(n_bf:1:-1)
      NAO = NAO(:,n_bf:1:-1)
      if (allocated(self%roby)) then
         if (self%roby%show_calcs) then
            call text_(stdout,"NAO occupation:")
            call put_(stdout,occupation)
            call text_(stdout,"NAO:")
            call put_(stdout,NAO)
         end if
      end if

      ! Assign
      self%atom(a)%NAOs = NAO
      self%atom(a)%NAO_occupations = occupation
   end subroutine


!  ==========================
!  DIIS/Orbital extrapolation
!  ==========================

   subroutine make_DIIS_error_length(self,length)
   ! Make the SCF error vector from the current fock_mx and
   ! density_mx, and return the error "length".  Useful for reporting the
   ! error length.
      type(MOLECULE_TYPE), INOUT :: self
      REAL, OUT :: length
      type(OPMATRIX_TYPE), allocatable :: C
   ENSURE(allocated(self%fock_mx),"MOLECULE.SCF:make_DIIS_error_length ... no fock matrix")
   ENSURE(has_any_genre_(self%fock_mx),"MOLECULE.SCF:make_DIIS_error_length ... no fock matrix")
   ENSURE(allocated(self%density_mx),"MOLECULE.SCF:make_DIIS_error_length ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.SCF:make_DIIS_error_length ... no density matrix")

      ! Make the commutator
      call create_(C,self%fock_mx%n_bf,genre_(self%fock_mx))
      call make_SCF_DIIS_commutator_(self,C)
      call compress_(C)

      ! Its length
      length = ZERO
      select case (genre_(self%fock_mx))
      case ("r "); length = norm_(C%tri)
      case ("u "); length = norm_(C%tri)
      case ("gc"); length = norm_(C%sqr)
      end select
      call destroy_(C)

      ! Set the length
      call set_error_length_(self%SCF_data%SCF_DIIS,length)
      call set_DIIS_error_(self%SCF_data,length)
   end subroutine

   subroutine extrapolate_Fock_mx(self)
   ! Extrapolate the fock matrix, currently only using DIIS.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: spinorbital_genre,SCF_kind
      REAL :: error
      type(OPMATRIX_TYPE), allocatable :: C
      INT :: dim
   ENSURE(allocated(self%SCF_data),"MOLECULE.SCF:extrapolate_Fock_mx ... no scf data")
   ENSURE(allocated(self%fock_mx),"MOLECULE.SCF:extrapolate_Fock_mx ... no fock matrix")
   ENSURE(allocated(self%density_mx),"MOLECULE.SCF:extrapolate_Fock_mx ... no density matrix")

      ! DIIS sometimes forbidden 
      if (NOT self%SCF_data%using_DIIS) return
      call set_DIIS_error_(self%SCF_data,ZERO)
      if (NOT has_any_genre_(self%fock_mx)) return

      ! Spinorbital kind before compression
      spinorbital_genre = genre_(self%fock_mx)

      ! Make commutator C
      call create_(C,self%fock_mx%n_bf,spinorbital_genre)
      call make_SCF_DIIS_commutator_(self,C)

      ! Compress
      call compress_(C)
      call compress_(self%fock_mx)

      ! Extrapolate compressed entities
      SCF_kind = self%SCF_data%SCF_kind
      select case (SCF_kind)

      case ("rhf                     ","rks                     ","noninteracting-group-rhf")
         dim = size(C%tri)
         call extrapolate_(self%SCF_data%SCF_DIIS,self%fock_mx%tri,C%tri,dim)  ! NOT PURE
         error = norm_(C%tri)

      case ("xray_rhf                ","xray_rks                ")
         dim = size(C%tri)
         call extrapolate_(self%SCF_data%SCF_DIIS,self%fock_mx%tri,C%tri,dim)
         error = norm_(C%tri)

      case ("rohf                    ")
         dim = size(C%tri)
         call extrapolate_(self%SCF_data%SCF_DIIS,self%fock_mx%tri,C%tri,dim)
         error = norm_(C%tri)

      case ("uhf                     ","uks                     ","xray_uhf                ","xray_uks                ","pnd_uhf                 ","pnd_uks                 ","xray_pnd_uhf            ","xray_pnd_uks            ")
         dim = size(C%tri)
         call extrapolate_(self%SCF_data%SCF_DIIS,self%fock_mx%tri,C%tri,dim)
         error = norm_(C%tri)

      case ("gchf                    ","xray_gchf               ")
         dim = size(C%sqr)
         call extrapolate_(self%SCF_data%SCF_DIIS,self%fock_mx%sqr,C%sqr,dim)
         error = norm_(C%sqr)
      end select

      ! Set the error
      call set_DIIS_error_(self%SCF_data,error)

      ! Clean
      call destroy_(C)

      ! Uncompress
      call uncompress_(self%fock_mx)
   end subroutine


!  ======================
!  Setting the DIIS error
!  ======================

   subroutine make_SCF_DIIS_commutator(self,C)
   ! Make "C", the DIIS commutator FDS - SDF of the current Fock and
   ! density matrices: the gradient of the SCF energy with respect to
   ! orbital rotations, zero at convergence. The part of it that rotates
   ! orbitals into combinations of basis functions removed for linear
   ! dependence is taken out: the SCF never makes those rotations, so
   ! that part does not go to zero.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: C
   ENSURE(has_any_genre_(self%fock_mx),"MOLECULE.SCF:make_SCF_DIIS_commutator ... no Fock matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.SCF:make_SCF_DIIS_commutator ... no density matrix")

      ! Get overlap matrix and its eigenvectors (leaky)
      call make_overlap_mx_(self)
      call make_DIIS_commutator_(C,self%fock_mx,self%density_mx,self%overlap_mx)

      ! Remove the linearly dependent part
      select case (genre_(self%fock_mx))
      case ("r "); call remove_dependence_from_r_(self,C%r)
      case ("u "); call remove_dependence_from_r_(self,C%a)
      end select
   end subroutine

   PURE subroutine make_DIIS_commutator(C,F,D,S)
   ! Make the DIIS commutator opmatrix, C = FDS - SDF.
      type(OPMATRIX_TYPE), INOUT :: C
      type(OPMATRIX_TYPE), IN :: F,D
      MAT(REAL,:,:), IN :: S
      select case (genre_(F))

      case ("r ")
         call make_DIIS_commutator_r_(C%r,F%r,D%r,S)

      case ("u ")
         call make_DIIS_commutator_r_(C%a,F%a,D%a,S)
         call make_DIIS_commutator_r_(C%b,F%b,D%b,S)
         C%a = C%a+C%b

      case ("gc")
         call make_DIIS_commutator_gc_(C%gc,F%gc,D%gc,S)
      end select
   end subroutine

   PURE subroutine make_DIIS_commutator_r(C,F,D,S)
   ! Make the real DIIS commutator matrix, C = FDS - SDF.
      MAT(REAL,:,:), OUT :: C
      MAT(REAL,:,:), IN :: F,D,S
      MAT(REAL,:,:), allocatable :: W
      call create_(W,size(F,1),size(F,1))
      call to_product_of_(W,D,S)
      call to_product_of_(C,F,W)
      call destroy_(W)
      call antisymmetric_fold_(C)
      call antisymmetric_reflect_(C)
   end subroutine

   PURE subroutine make_DIIS_commutator_gc(C,F,D,S)
   ! Make the complex DIIS commutator matrix, C = FDS - SDF.
      MAT(CPX,:,:), OUT :: C
      MAT(CPX,:,:), IN :: F,D
      MAT(REAL,:,:), IN :: S
      MAT(CPX,:,:), allocatable :: O,W
      call create_(W,size(F,1),size(F,1))

      ! Make the overlap matrix
      call create_(O,size(F,1),size(F,1))
      O = ZERO
      call set_aa_block_to_(O,S)
      call set_bb_block_to_(O,S)

      ! D . S
      call to_product_of_(W,D,O)
      call destroy_(O)

      ! F . D . S
      call to_product_of_(C,F,W)
      call destroy_(W)

      ! Make FDS - SDF
      call antihermitian_fold_(C)
      call antihermitian_reflect_(C)
   end subroutine


!  =========================
!  Canonicalisation routines
!  =========================

   subroutine canonicalize_occupied_MOs(self)
   ! Generate the Fock matrix from the molecular orbitals and
   ! diagonalise it on the occupied-occupied block to get the
   ! canonical molecular orbitals, and write them out.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: SCF_kind
   ENSURE(allocated(self%MOs),"MOLECULE.SCF:canonicalize_occupied_MOs ... need old MOs!")
   ENSURE(allocated(self%MO_energies),"MOLECULE.SCF:canonicalize_occupied_MOs ... need old energies")
      call make_SCF_density_mx_(self)
      call make_scf_Fock_mx_(self)
      SCF_kind = self%SCF_data%SCF_kind
      select case (SCF_kind)

      case ("rhf     ","rks     ","xray_rhf","xray_rks","noninteracting-group-rhf")
         call destroy_(self%MO_energies)
         call create_(self%MO_energies,self%n_bf,"r ")
         call canonicalize_r_occ_MOs_(self,self%MO_energies%r,self%MOs%r,self%fock_mx%r,self%n_e/2)
         call flush_(stdout)
         call text_(stdout,"Canonicalized MO energies:")
         call put_(stdout,self%MO_energies%r)

      case ("uhf         ","uks         ","xray_uhf    ","xray_uks    ","rohf        ","pnd_uhf     ","pnd_uks     ","xray_pnd_uhf","xray_pnd_uks")
         call destroy_(self%MO_energies)
         call create_(self%MO_energies,self%n_bf,"u ")
         call canonicalize_r_occ_MOs_(self,self%MO_energies%a,self%MOs%a,self%fock_mx%a,self%n_a)
         call canonicalize_r_occ_MOs_(self,self%MO_energies%b,self%MOs%b,self%fock_mx%b,self%n_b)
         call flush_(stdout)
         call text_(stdout,"Canonicalized alpha MO energies:")
         call put_(stdout,self%MO_energies%a(1:self%n_a))
         call flush_(stdout)
         call text_(stdout,"Canonicalized beta MO energies:")
         call put_(stdout,self%MO_energies%b(1:self%n_b))
      end select
   end subroutine

   subroutine canonicalize_r_occ_MOs(self,MO_energies,MO,F,n)
   ! Digaonalise the Fock matrix "F" in the occupied-occupied 
   ! and virtual-virtual block of the molecular orbitals "MO", 
   ! and reset the occupied "MO_energies".
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: MO_energies
      MAT(REAL,:,:), INOUT :: MO
      MAT(REAL,:,:), IN :: F
      INT, IN :: n
      MAT(REAL,:,:), allocatable :: G,U,MO_old
      INT :: v
      call create_(G,n,n)
      call create_(U,n,n)
      call create_(MO_old,self%n_bf,n)

      ! Canonicalise the occ-occ block
      MO_old = MO(:,1:n)
      call change_basis_to_(F,G,MO_old)
      call solve_symmetric_eigenproblem_(G,MO_energies(1:n),U)  ! NOT PURE
      call to_product_of_(MO(:,1:n),MO_old,U)
      call destroy_(MO_old)
      call destroy_(U)
      call destroy_(G)

      ! Canonicalise the vir-vir block
      v = self%n_bf-n
      call create_(G,v,v)
      call create_(U,v,v)
      call create_(MO_old,self%n_bf,v)
      MO_old = MO(:,n+1:)
      call change_basis_to_(F,G,MO_old)
      call solve_symmetric_eigenproblem_(G,MO_energies(n+1:),U)
      call to_product_of_(MO(:,n+1:),MO_old,U)
      call destroy_(MO_old)
      call destroy_(U)
      call destroy_(G)
   end subroutine

   subroutine canonicalize_virtual_MOs(self,output)
   ! Generate the Fock matrix from the molecular orbitals and
   ! diagonalise it on the virtual-virtual block to get new
   ! virtual MOs and virtual MO energies.
   ! NOTE: only for restricted, and Hizinaga-Arnau, so far.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: output
      MAT(REAL,:,:), allocatable :: F,Fv
      MAT(REAL,:,:), allocatable :: J,K,P
      MAT(REAL,:,:), allocatable :: Uv,VMO
      VEC(REAL,:), allocatable :: OMO_en
      STR(len=STR_SIZE) :: method
      INT :: n,no,nv,r
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      BIN :: out

      ! Any canonicalization requested?
      if (self%SCF_data%SCF_kind/="rhf") return
      if (self%SCF_data%VMO_canonisation==" ") return

      ! Other ensure conditions
      if (NOT allocated(self%MOs)) return
      if (NOT allocated(self%MOs%r)) return
      if (NOT allocated(self%fock_mx)) return
      if (NOT allocated(self%fock_mx%r)) return

      ! Output
      out = FALSE
      if (present(output)) out = output

      ! Sizes
      n = self%n_bf
      no = self%n_e/2
      nv = n-no

      ! Canonocalised VV MO energies (leaky)
      call create_(self%SCF_data%VMO_energies,nv)
      OMO_en = self%MO_energies%r(no+1:no+10)  ! Old MO energies

      ! Canonicalisation method
      method = self%SCF_data%VMO_canonisation

      ! MO to eliminate from density
      r = self%SCF_data%VMO_canonisation_MO
      if (output) then

         ! Mo energies before & after
         call flush_(stdout)
         call text_(stdout,"=======================")
         call text_(stdout,"Canonicalise Vitual MOs")
         call text_(stdout,"=======================")
         call flush_(stdout)
         call text_(stdout,". The method works by removing an occupied MO (OMO)")
         call text_(stdout,"  specified by its orbital index 'r'")
         call flush_(stdout)
         call text_(stdout,"  A -ve number 'r' for the canonisation MO means that")
         call text_(stdout,"  (HOMO+r) is removed from the the density matrix used")
         call text_(stdout,"  to calculate the Fock matrix, which is the OMO which")
         call text_(stdout,"  is assumed 'excited'.")
         call flush_(stdout)
         call text_(stdout,". A method may excite singlet or triplet states;")
         call text_(stdout,"  see Hunt & Goddard (1969) CPL 3 p 414.")
         call flush_(stdout)
         call text_(stdout,"  An average of singlet and triplet state energies was")
         call text_(stdout,"  suggested, see Huzinaga & Arnau (1971) JCP 54 p. 1948.")
         call flush_(stdout)
         call text_(stdout,". VV Fock energies are reported, which are the diagonal")
         call text_(stdout,"  elements of the original Fock mx in the new VMO basis.")
         call flush_(stdout)
         call show_(stdout,"VMO canonisation method   = ",method)
         if (method/="exchange") then
            call show_(stdout,"Canonisation MO removed   = ",r)
         end if
      end if

      ! Deal with -ve occupied MO to remove
      if (r<=0) r = no+r
      ENSURE(0<r AND r<=no,"MOLECULE.SCF:canonicalize_virtual_MOs ... removed MO is not occupied")

      ! Density matrix for removed orbital
      if (method/="exchange") then
         call create_(P,n,n)
         P = matmul(self%MOs%r(:,r:r),transpose(self%MOs%r(:,r:r)))
      end if

      ! Fock matrices  !!!!!!!!!!!!!!
      call create_(Uv,nv,nv)  ! VMO rotation
      call create_(Fv,nv,nv)  ! virtual Fock
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! Canonicalise
      select case (method)

      case ("exchange")
         call create_(F,n,n)  ! is exchange
         call create_(J,n,n)
         call make_r_JK_engine_(self,J,F,self%density_mx%r)
         call destroy_(J)

      case ("hunt-goddard-1")
         call create_(J,n,n)
         call create_(K,n,n)
         call make_r_JK_engine_(self,J,K,P)
         F = self%fock_mx%r-J+TWO*K
         call destroy_(K)
         call destroy_(J)
         call destroy_(P)

      case ("hunt-goddard-3")
         call create_(J,n,n)
         call make_r_J_engine_(self,J,P)
         F = self%fock_mx%r-J
         call destroy_(J)
         call destroy_(P)

      case ("huzinaga-arnau")
         call create_(J,n,n)
         call create_(K,n,n)
         call make_r_JK_engine_(self,J,K,P)
         F = self%fock_mx%r-J+K
         call destroy_(K)
         call destroy_(J)
         call destroy_(P)

      case default
         DIE("MOLECULE.SCF:canonicalize_virtual_MOs ... unknown VMO canonicalization, "//trim(method))
      end select

      ! Transform new F to virtual
      VMO = self%MOs%r(:,no+1:)
      call change_basis_to_(F,Fv,VMO)
      call destroy_(VMO)
      call destroy_(F)

      ! Canonicalize new VMOs. 
      ! Keep new VMO energies.
      call solve_symmetric_eigenproblem_(Fv,self%SCF_data%VMO_energies,Uv)
      VMO = self%MOs%r(:,no+1:)
      self%MOs%r(:,no+1:) = matmul(VMO,Uv)

      ! New diagonal Fock mx energies
      VMO = self%MOs%r(:,no+1:)
      call change_basis_to_(self%fock_mx%r,Fv,VMO)
      call put_diagonal_to_(Fv,self%MO_energies%r(no+1:))

      ! Virtual sector of Fock mx for TD
      call move_alloc(Fv,self%TD_data%Fv)

      ! Clean
      call destroy_(VMO)
      call destroy_(Uv)

      ! Output
      if (output) then
         call flush_(stdout)
         call text_(stdout,"Lowest virtial MO energies:")
         call flush_(stdout)

         ! Table headings
         call create_(table,3)
         call set_heading_(table(1),"  HF MO")
         call set_subhead_(table(1),"energies")
         call set_values_(table(1),OMO_en)
         call set_heading_(table(2)," VV Fock")
         call set_subhead_(table(2),"energies")
         call set_values_(table(2),self%MO_energies%r(no+1:no+10))
         call set_heading_(table(3),"Canonisd")
         call set_subhead_(table(3),"energies")
         call set_values_(table(3),self%SCF_data%VMO_energies(1:10))

         ! Make the table
         call put_(table,label_rows=TRUE)

         ! Clean
         call destroy_(table)
      end if
   end subroutine


!  =================
!  Localize orbitals
!  =================

   subroutine localize_MOs(self)
   ! Localize the MO's
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), allocatable :: D,Q
   ENSURE(allocated(self%MOs),"MOLECULE.SCF:localize_MOs ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.SCF:localize_MOs ... no MO's")
   ENSURE(self%basis_info_made,"MOLECULE.SCF:localize_MOs ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.SCF:localize_MOs ... no atom info")

      ! Leaky
      call create_(self%localiser)
      call create_(D,self%n_bf,self%n_bf,3)
      call create_(Q,self%n_bf,self%n_bf,6)
      call make_dipole_matrices_(self,D(:,:,1),D(:,:,2),D(:,:,3))
      call make_quadrupole_matrices_(self,Q(:,:,1),Q(:,:,2),Q(:,:,3),Q(:,:,4),Q(:,:,5),Q(:,:,6))
      call set_defaults_(self%localiser,self%MOs%r(:,1:self%n_a),D,Q)
      call set_MO_(self%localiser,self%MOs%r(:,1:self%n_a))
      call read_keywords_(self%localiser)
      call destroy_(Q)
      call destroy_(D)
   end subroutine

end module
