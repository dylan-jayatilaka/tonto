!==================================================================
!
! MOLECULE.XTAL: An object representation of a molecule.
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1996
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
module MOLECULE_XTAL_MODULE

#  include "macros"

   include "molecule.xtal.use"

   implicit none

   include "molecule.xtal.int"


contains

!  ========
!  Read CIF
!  ========

   subroutine read_CIF(self)
   ! Read in CIF information, reset name of job to cif file name
      type(MOLECULE_TYPE), INOUT :: self
      if (NOT allocated(self%cif)) call create_(self%cif)
      call read_keywords_(self%cif)
      call update_(self%cif)
   end subroutine

   subroutine read_CIF_atoms(self,cif)
   ! Read atom position covariances, and pisitions from a "cif", and
   ! reset and bond lengths, and *if* the crystal is there, make
   ! crystal fragment atom info.
      type(MOLECULE_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
   ENSURE(cif%start_of_data>0,"MOLECULE.XTAL:read_CIF_atoms ... no start of data in CIF")
   ENSURE(cif%end_of_data>cif%start_of_data,"MOLECULE.XTAL:read_CIF_atoms ... no end of data in CIF")

      ! Open the CIF
      if (NOT is_open_(cif%file)) call open_(cif)

      ! Read covariances from refinement?
      ! Make sure to read refinement settings
      call read_CIF_covariances_(self,cif)

      ! Read in the atoms. No bond length reset.
      call read_CIF_atoms_only_(self,cif)

      ! Only continue if there are some atoms ...
      if (NOT allocated(self%crystal)) return

      ! Set cartesian atom and crystal info. 
      ! Bond lengths may be reset in here.
      call reset_CIF_atoms_and_crystal_(self,cif)

      ! Make the esds (if covariances were read)
      call read_CIF_crystal_esds_(self,cif)
   end subroutine

   subroutine read_CIF_atoms_only(self,cif)
   ! Read atom info from a "cif". Do not reset bond lengths.
      type(MOLECULE_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
   ENSURE(cif%start_of_data>0,"MOLECULE.XTAL:read_CIF_atoms_only ... no start of data in CIF")
   ENSURE(cif%end_of_data>cif%start_of_data,"MOLECULE.XTAL:read_CIF_atoms_only ... no end of data in CIF")

      ! Open the CIF
      if (NOT is_open_(cif%file)) call open_(cif)

      ! Clean up any old atoms
      call destroy_(self%atom)

      ! Read the atoms from the CIF & set covariances
      call read_CIF_(self%atom,cif)

      ! Make each atom's cartesian covariance from its CIF standard
      ! uncertainties, assuming they are uncorrelated. Must be done here:
      ! it is the last point at which the errors are still in the crystal
      ! frame and the unit cell is in scope. The change to cartesians
      ! follows in update_atom_basis_group_info below.
      if (allocated(self%crystal)) then
         call make_pADP_covariances_from_esds_(self%atom,self%crystal%unit_cell)
      end if

      ! Set defaults and bases
      self%spin_multiplicity = default_spin_multiplicity_(self)

      ! Finalize
      call update_atom_basis_group_info_(self)

      ! Finalize this, maybe
      call update_CIF_atom_pADPs_(self)
      if (allocated(self%crystal)) then
         self%crystal%asymmetric_unit_source = "from-cif"
      end if
   end subroutine

   subroutine update_CIF_atom_pADPs(self)
   ! Update any pADPs for the atoms in the light of any *new* crystal
   ! information.
      type(MOLECULE_TYPE), INOUT :: self

      ! Set the atom covariances (for errors)
      ! This is for cartesian cif2 only
      if (allocated(self%crystal)) then
         if (allocated(self%crystal%xray_data)) then
            if (allocated(self%crystal%xray_data%fragment_covariance_mx)) then
               call set_pADP_errors_to_(self%atom,self%crystal%xray_data%fragment_covariance_mx,H_U_iso=FALSE)
            end if
         end if
      end if
   end subroutine

   subroutine read_CIF_covariances(self,cif)
   ! Read the CIF covariances and set pADP esds.
   ! This routine gets the no. of pADPs from the (square root) of the
   ! length of the covariance matrices.
   ! NOTE: covariances assumed to only be in CIF2 (cartesian) files
   ! NOTE: this is not enough, we need to read refinement settings to
   !       get the nPADPs (number of refined parameters) correct.
      type(MOLECULE_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      STR(len=STR_SIZE) :: label
      BIN :: fcov_there,fcov_inputted
      BIN :: acov_there,acov_inputted
   ENSURE(cif%start_of_data>0,"MOLECULE.XTAL:read_CIF_covariances ... no start of data in CIF")
   ENSURE(cif%end_of_data>cif%start_of_data,"MOLECULE.XTAL:read_CIF_covariances ... no end of data in CIF")

      ! Basic sanity
      if (NOT cif%use_CIF2) return
      if (NOT allocated(self%crystal)) return
      if (NOT allocated(self%crystal%xray_data)) return

      ! If covariances already there, assume they are OK
      acov_there = allocated(self%crystal%xray_data%covariance_mx)
      fcov_there = allocated(self%crystal%xray_data%fragment_covariance_mx)
      if (acov_there AND fcov_there) return

      ! Open the CIF if needed
      if (NOT is_open_(cif%file)) call open_(cif)

      ! Below needs to be modified to remove duplicate looped
      ! item search when blank-labelled covariance is not needed

      ! Asymmetric covariance there?
      label = "_asym_unit"
      call find_item_(cif,trim(label)//"_covariance_mx",acov_there)
      acov_inputted = FALSE
      if (acov_there) then

         ! Read it: Should we be creating x_ray_data?
         if (NOT allocated(self%crystal%xray_data)) call create_(self%crystal%xray_data)
         call read_CIF_covariance_(cif,trim(label),self%crystal%xray_data%covariance_mx)
         acov_inputted = TRUE
      end if

      ! Fragment covariance there?
      label = "_fragment"
      call find_looped_item_(cif,trim(label)//"_covariance_mx",fcov_there)
      fcov_inputted = FALSE
      if (fcov_there) then

         ! Read it
         if (NOT allocated(self%crystal%xray_data)) call create_(self%crystal%xray_data)
         call read_CIF_covariance_(cif,trim(label),self%crystal%xray_data%fragment_covariance_mx)
         fcov_inputted = TRUE

   ! else if (acov_there) then
   !    ! Read it
   !    if (.crystal.xray_data.deallocated) .crystal.xray_data.create
   !    .set_frag_from_asym_cov_mx
      end if
   end subroutine

   subroutine read_CIF_crystal(self,cif)
   ! Read crystal unit cell and spacegroup information (but NOT atom
   ! positions) from a CIF file called "cif", and then produce the
   ! atom and crystal informational arrays.
   ! NOTE: only to be called after reading .atom's from the CIF,
   ! see the routine below.
      type(MOLECULE_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
   ENSURE(cif%start_of_data>0,"MOLECULE.XTAL:read_CIF_crystal ... no start of data in CIF")
   ENSURE(cif%end_of_data>cif%start_of_data,"MOLECULE.XTAL:read_CIF_crystal ... no end of data in CIF")

      ! Read in the only crystal information
      call read_CIF_crystal_only_(self,cif)

      ! Only continue if there are some atoms ...
      if (NOT allocated(self%atom)) return

      ! Update
      call update_CIF_crystal_atoms_(self,cif)
   end subroutine

   subroutine read_CIF_crystal_only(self,cif)
   ! Read from a "cif" crystal unit cell and spacegroup information,
   ! and any covariance matrices from a refinement -- but *NOT* atom
   ! positions and *NOT* reflection data.
      type(MOLECULE_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif

      ! Create the crystal; it shouldn't really exist beforehand
   ENSURE(cif%start_of_data>0,"MOLECULE.XTAL:read_CIF_crystal_only ... no start of data in CIF")
   ENSURE(cif%end_of_data>cif%start_of_data,"MOLECULE.XTAL:read_CIF_crystal_only ... no end of data in CIF")
   WARN_IF(allocated(self%crystal),"MOLECULE.XTAL:read_CIF_crystal_only ... crystal= data was already defined")
      if (NOT allocated(self%crystal)) call create_(self%crystal)

      ! Read in the crystal from the CIF
      call read_CIF_spacegroup_and_cell_(self%crystal,cif)

      ! Read covariances from refinement?
      ! Make sure they are inputed there.
      call read_CIF_covariances_(self,cif)
   end subroutine

   subroutine update_CIF_crystal_atoms(self,cif)
   ! Update the any hydrogen-atom bond lengths and remake
   ! any crystal fragmemnt_atom info.
   ! NOTE: only to be called after reading .atom's from the CIF
      type(MOLECULE_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
   ENSURE(cif%start_of_data>0,"MOLECULE.XTAL:update_CIF_crystal_atoms ... no start of data in CIF")
   ENSURE(cif%end_of_data>cif%start_of_data,"MOLECULE.XTAL:update_CIF_crystal_atoms ... no end of data in CIF")

      ! Only continue if there are some atoms ...
      if (NOT allocated(self%atom)) return

      ! Set cartesian atom and crystal info. 
      ! Bond lengths may be reset in here.
      call reset_CIF_atoms_and_crystal_(self,cif)

      ! Make the esds (if covariances were read)
      call read_CIF_crystal_esds_(self,cif)
   end subroutine

   subroutine reset_CIF_atoms_and_crystal(self,cif)
   ! Set the atom and crystal fragment_atom info.
   ! If requested in cif= { ... }, reset H atom bond lengths.
      type(MOLECULE_TYPE), INOUT :: self
      type(CIF_TYPE), IN :: cif
      VEC(REAL,:), allocatable :: H_bond_length
      BIN :: has_Hs,reset
   ENSURE(allocated(self%atom),"MOLECULE.XTAL:reset_CIF_atoms_and_crystal ... no atoms")

      ! Is crystal there?
      if (NOT allocated(self%crystal)) return
      if (NOT self%crystal%spacegroup%analyzed) return

      ! Must be cartesian
      call resolve_axis_system_(self)
      if (has_all_Hs_single_bonded_(self%atom,has_Hs)) then

         ! Reset H bond lengths the simple way.
         ! If disordered, the disordered atoms must
         ! be proximate to their bonded neighbours.
         if (has_Hs) then
            if (apply_bond_length_(cif)) then
               if (apply_BH_bond_length_(cif)) call set_bond_lengths_(self%atom,5,1,cif%BH_bond_length)
               if (apply_CH_bond_length_(cif)) call set_bond_lengths_(self%atom,6,1,cif%CH_bond_length)
               if (apply_NH_bond_length_(cif)) call set_bond_lengths_(self%atom,7,1,cif%NH_bond_length)
               if (apply_OH_bond_length_(cif)) call set_bond_lengths_(self%atom,8,1,cif%OH_bond_length)
               call destroy_asymmetric_unit_(self%crystal)
            end if
         end if
      else

         ! There are floating hydrogens or multiple bonded H's ...
         ! Uses the crystal to reset the hydrogen bond lengths.
         ! Uses UC conn. table to skip multiple-bonded H atoms.
         call create_(H_bond_length,5,8)
         H_bond_length(5) = cif%BH_bond_length
         H_bond_length(6) = cif%CH_bond_length
         H_bond_length(7) = cif%NH_bond_length
         H_bond_length(8) = cif%OH_bond_length

         ! Remake fragment data?
         call reset_H_bond_lengths_in_(self%crystal,self%atom,H_bond_length,reset)
         call destroy_(H_bond_length)

         ! Return if nothing was done
         if (NOT reset AND allocated(self%crystal%asymmetric_unit_atom)) then
            return
         end if
         if (has_disorder_(self%atom)) then
            WARN("MOLECULE.XTAL:reset_CIF_atoms_and_crystal ... DISORDERED ASYMMETRIC CELL MAY BE PRUNED")
            WARN("MOLECULE.XTAL:reset_CIF_atoms_and_crystal ... SOME ATOM INFORMATION MAY BE LOST")
         end if
         call destroy_asymmetric_unit_(self%crystal)
      end if

      ! Re-analyze the fragment symmetry information
      ! Maybe not all things in here need doing again ...
      call update_fragment_info_(self%crystal,self%atom)

      ! Finalize atoms & groups as well
      call update_atom_basis_group_info_(self)
   end subroutine

   subroutine read_CIF_crystal_esds(self,cif)
   ! Read the CIF covariances and set pADP esds.
      type(MOLECULE_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN :: fcov_there
      BIN :: acov_there
   ENSURE(allocated(self%atom),"MOLECULE.XTAL:read_CIF_crystal_esds ... no atoms")
   ENSURE(cif%start_of_data>0,"MOLECULE.XTAL:read_CIF_crystal_esds ... no start of data in CIF")
   ENSURE(cif%end_of_data>cif%start_of_data,"MOLECULE.XTAL:read_CIF_crystal_esds ... no end of data in CIF")
      if (NOT allocated(self%crystal)) return
      if (NOT allocated(self%crystal%xray_data)) return

      ! Open the CIF if needed
      if (NOT is_open_(cif%file)) call open_(cif)
      acov_there = allocated(self%crystal%xray_data%covariance_mx)
      fcov_there = allocated(self%crystal%xray_data%fragment_covariance_mx)
      if (fcov_there) then
         if (acov_there) then
            if (allocated(self%crystal%fragment_atom)) then

         ! .crystal.set_frag_from_asym_cov_mx
               call set_fragment_esds_(self%crystal,self%crystal%fragment_atom)
            end if
            if (allocated(self%atom)) then
               call set_fragment_esds_(self%crystal,self%atom)
            end if
         end if
      end if
   end subroutine


!   create_cluster_mol :: leaky, public
!   ! Create a new "self" by generating a real molecular cluster from
!   ! information stored in .cluster information object.  The original
!   ! molecule replaced by the m-th molecular fragment in the .cluster
!   ! object.
!      self :: IN
!
!      mol :: MOLECULE
!      m :: INT
!
!      stdin.read(m)
!
!      .create_cluster_mol(m, mol)
!
!   end

!   create_cluster_mol(m, mol) :: leaky, public
!   ! Create a new "self" by generating a real molecular cluster from
!   ! information stored in .cluster information object.  The original
!   ! molecule replaced by the defragmented cluster, while the m-th
!   ! molecular fragment in the .cluster is in mol
!   ! -- This routine does not make sense, it does two unrelated things
!   ! -- dylan
!   ! OK - it seems to generate all molecules in a cluster, but it
!   ! keeps re-making the cluster, which is very wasteful, so that was
!   ! confusing ... can be easily fixed by factoring that part out of
!   ! this routine. -- remove for now ...
!      self :: allocatable, INOUT
!      m    :: INT, IN
!      mol  :: MOLECULE, OUT
!
!   ENSURE(.crystal.allocated,"no crystal data")
!   ENSURE(.cluster.allocated,"no cluster data")
!   ENSURE(.cluster.info_made,"no cluster data")
!
!      stdout.flush
!      stdout.text("Expanding cluster, creating molecule m = "//trim(m.to_str))
!
!      ! Destroy previously saved molecule
!      ! (In case this is in a loop)
!
!      ! Reset .atom list to molecule m
!      .atom.destroy
!      .cluster.create_atom_list(.atom)
!
!      ! Finalize basis on new .atom list
!      .update_atom_basis_group_info
!
!      ! Make new crystal mapping info for this fragment
!      .crystal.update_fragment_info(.atom)
!
!      ! Save this molecule & get a clean new molecule
!      .set_molecule_from_atom_group(m, mol) ! NOT PURE
!
!      ! If a CIF exists, copy that
!      if (.cif.allocated) mol.cif = .cif
!
!      ! Saved crystal is the same
!      mol.crystal = .crystal
!      mol.cluster = .cluster
!      mol.crystal.update_fragment_info(mol.atom)
!
!   end

!   create_HS_cluster :: leaky
!   ! Create a cluster from the molecules which touch the Hirshfeld
!   ! surface in isosurface. It is assumed there is a .saved.cluster
!   ! and that the current .atom's correspond to it (i.e. this is
!   ! called after a standard HS job).
!      self :: PTR
!
!   ENSURE(.saved.crystal.allocated,"no crystal data")
!   ENSURE(.saved.cluster.allocated,"no cluster data")
!   ENSURE(.saved.cluster.info_made,"no cluster data")
!   ENSURE(.saved.atom.allocated,"no atoms")
!   ENSURE(.atom.allocated,"no atoms")
!   ENSURE(.isosurface.allocated,"no isosurface")
!
!      surface_atoms :: VEC{INT}@
!      atom :: VEC{ATOM}*
!      n :: INT
!
!      ! Get surface atoms
!      n = .saved.atom.dim
!      surface_atoms = .isosurface.surface_group_atom_indices(.atom(n+1:))
!      surface_atoms = surface_atoms + n
!
!      ! Append them to the fragment
!      .atom.destroy_bases
!      atom = .atom(1:n)
!      atom.append(.atom(surface_atoms))
!
!      surface_atoms.destroy
!
!      ! Base crystal on this fragment
!      .atom.destroy
!      .atom => atom
!      .crystal.update_fragment_info(atom)
!      .crystal.set_destroy_fragment_atom(TRUE)
!
!      ! Make the new HS-contact-group-cluster
!      .cluster.destroy
!      .cluster.create(.crystal)
!      .cluster.set_generation_method("fragment")
!      .cluster.set_defragment(TRUE)
!      .cluster.make_info
!      .cluster.create_atom_list(atom)
!      .cluster.destroy
!
!      ! Set the completed-group atom list
!    ! .atom.destroy ! will destroy as part of crystal
!      .atom => atom
!
!      ! Base crystal on this cluster
!      .crystal.destroy_asymmetric_unit
!      .crystal.destroy_fragment_data
!      .crystal.update_fragment_info(.atom,assign_atom=TRUE)
!
!      ! Change name to reflect atom-list change
!      .name = trim(.saved.name) // "_HS_cluster"
!
!      ! Finalize atom info
!      .update_atom_basis_group_info
!
!   end

!  ===========================
!  Fourier transform integrals
!  ===========================

   NON_MPI_PURE subroutine make_FT_0(self,res,dens,k_pts)
   ! Return "res", the Fourier transform of a density described by an
   ! AO density matrix "dens" evaluated at some reflections "k_pts".
   ! NOTE: This version uses analytic gaussian function products.
   ! NOTE: Includes *temperature* and *partition* factors in here.
      type(MOLECULE_TYPE), IN :: self
      VEC(CPX,:), OUT :: res
      MAT(REAL,:,:), IN :: dens
      MAT(REAL,:,:), IN :: k_pts
      INT :: k_max,k,fa,fb,la,lb,na,nb,q,atom_a,atom_b
      MAT3(CPX,:,:,:), allocatable :: FT_ab
      REAL :: cutoff
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.XTAL:make_FT_0 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.XTAL:make_FT_0 ... no atom list")
   ENSURE(size(res)==size(k_pts,1),"MOLECULE.XTAL:make_FT_0 ... res and k_pts, incomptabile")
      k_max = size(k_pts,1)
      cutoff = TOL(10)/self%n_shell_pairs
      res = ZERO
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.XTAL:make_FT_0")

         ! Get shell pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (skip_ft_(sh,cutoff)) then

            ! Too small
            call destroy_ptr_part_(sh)
         else

            ! Make the FT - includes thermal smearing
            call create_(FT_ab,k_max,na,nb)
            call make_FT_pair_(self,FT_ab,k_pts,sh,atom_a,atom_b)

            ! Trace with density matrix
            ! Count off-diagonals twice
            if (fa/=fb) then
               do k = 1,k_max
                  res(k) = res(k)+TWO*sum(FT_ab(k,:,:)*dens(fa:la,fb:lb))
               end do
            else
               do k = 1,k_max
                  res(k) = res(k)+sum(FT_ab(k,:,:)*dens(fa:la,fb:lb))
               end do
            end if

            ! Clean
            call destroy_(FT_ab)
            call destroy_ptr_part_(sh)
         end if
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.XTAL:make_FT_0")
      PARALLEL_SUM(res)
   end subroutine

   PURE subroutine make_FT_1(self,res,dens,k_pts)
   ! Return "res", the Fourier transform of a density described by an
   ! AO density matrix "dens" evaluated at some reflections "k_pts".
   ! NOTE: Includes temperature factors hidden in here
      type(MOLECULE_TYPE), IN :: self
      VEC(CPX,:), OUT :: res
      MAT(CPX,:,:), IN :: dens
      MAT(REAL,:,:), IN :: k_pts
      INT :: k_max,k,fa,fb,la,lb,na,nb,q,atom_a,atom_b
      MAT3(CPX,:,:,:), allocatable :: FT_ab
      REAL :: cutoff
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.XTAL:make_FT_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.XTAL:make_FT_1 ... no atom list")
   ENSURE(size(res)==size(k_pts,1),"MOLECULE.XTAL:make_FT_1 ... res and k_pts, incomptabile")
      k_max = size(k_pts,1)
      cutoff = TOL(10)/self%n_shell_pairs
      res = ZERO
      do q = 1,self%n_shell_pairs

         ! Get shell pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (skip_ft_(sh,cutoff)) then

            ! Too small
            call destroy_ptr_part_(sh)
         else

            ! Make the FT - includes thermal smearing
            call create_(FT_ab,k_max,na,nb)
            call make_FT_pair_(self,FT_ab,k_pts,sh,atom_a,atom_b)

            ! Trace with density matrix
            ! Count off-diagonals twice
            if (fa/=fb) then
               do k = 1,k_max
                  res(k) = res(k)+TWO*trace_product_with_(FT_ab(k,:,:),dens(fb:lb,fa:la))
               end do
            else
               do k = 1,k_max
                  res(k) = res(k)+trace_product_with_(FT_ab(k,:,:),dens(fb:lb,fa:la))
               end do
            end if

            ! Clean
            call destroy_(FT_ab)
            call destroy_ptr_part_(sh)
         end if
      end do
   end subroutine

   PURE subroutine make_FT_gc(self,res,dens,k_pts)
   ! Fourier transform of a density described by AO density matrix dens
   ! evaluated at a series of reciprocal space points k_pts
   ! Size of res is [size(k_pts,1)].
      type(MOLECULE_TYPE), IN :: self
      VEC(CPX,:), OUT :: res
      MAT(CPX,:,:), IN :: dens
      MAT(REAL,:,:), IN :: k_pts
      INT :: k_max,q,k,n_bf
      MAT3(CPX,:,:,:), allocatable :: FT_ab
      INT :: fa,la,na,atom_a
      INT :: fb,lb,nb,atom_b
      MAT(CPX,:,:), allocatable :: dens_ba_aa,dens_ba_bb
      MAT(CPX,:,:), allocatable :: dens_ba_ab,dens_ba_ba
      REAL :: cutoff
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.XTAL:make_FT_gc ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.XTAL:make_FT_gc ... no atom list")
      n_bf = self%n_bf
      k_max = size(k_pts,1)
      cutoff = TOL(10)/self%n_shell_pairs
      res = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (skip_ft_(sh,cutoff)) then
            call destroy_ptr_part_(sh)
            cycle
         end if

         ! FT integrals
         call create_(FT_ab,k_max,na,nb)
         call make_FT_pair_(self,FT_ab,k_pts,sh,atom_a,atom_b)

         ! DM blocks
         dens_ba_aa = dens(fb:lb,fa:la)
         dens_ba_ab = dens(fb:lb,fa+n_bf:la+n_bf)
         dens_ba_ba = dens(fb+n_bf:lb+n_bf,fa:la)
         dens_ba_bb = dens(fb+n_bf:lb+n_bf,fa+n_bf:la+n_bf)
         if (fa/=fb) then  ! count off-diagonals twice.
            do k = 1,k_max
               res(k) = res(k)+TWO*trace_product_with_(FT_ab(k,:,:),dens_ba_aa)
               res(k) = res(k)+TWO*trace_product_with_(FT_ab(k,:,:),dens_ba_ab)
               res(k) = res(k)+TWO*trace_product_with_(FT_ab(k,:,:),dens_ba_ba)
               res(k) = res(k)+TWO*trace_product_with_(FT_ab(k,:,:),dens_ba_bb)
            end do
         else
            do k = 1,k_max
               res(k) = res(k)+trace_product_with_(FT_ab(k,:,:),dens_ba_aa)
               res(k) = res(k)+trace_product_with_(FT_ab(k,:,:),dens_ba_ab)
               res(k) = res(k)+trace_product_with_(FT_ab(k,:,:),dens_ba_ba)
               res(k) = res(k)+trace_product_with_(FT_ab(k,:,:),dens_ba_bb)
            end do
         end if
         call destroy_(FT_ab)
         call destroy_ptr_part_(sh)
      end do
   end subroutine

   PURE subroutine make_FT_deriv_U_0(self,res,dens,k_pts)
   ! Fourier transform of a density described by AO density matrix dens
   ! evaluated at a series of reciprocal space points k_pts
   ! Size of res is [k_max,n_atom*6]
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), IN :: dens
      MAT(REAL,:,:), IN :: k_pts
      MAT(CPX,:,:), OUT :: res
      CPX :: FT_ab2,tmp1,tmp2,tmpa1,tmpa2,tmpa3,tmpa4,tmpa5,tmpa6
      INT :: k_max,k,fa,fb,la,lb,na,nb,q,atom_a,atom_b,basea,baseb
      REAL :: delta,g,separation,x,y,z,z2
      VEC(REAL,3) :: diff
      MAT3(CPX,:,:,:), allocatable :: FT_ab
      MAT(REAL,:,:), allocatable :: dens_ba
      REAL :: cutoff
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.XTAL:make_FT_deriv_U_0 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.XTAL:make_FT_deriv_U_0 ... no atom list")
      k_max = size(k_pts,1)
      cutoff = TOL(10)/self%n_shell_pairs
      res = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (skip_ft_(sh,cutoff)) then
            call destroy_ptr_part_(sh)
            cycle
         end if
         call create_(FT_ab,k_max,na,nb)
         call make_FT_pair_(self,FT_ab,k_pts,sh,atom_a,atom_b)
         dens_ba = dens(fb:lb,fa:la)

         ! Count off-diagonals twice.
         if (fa/=fb) FT_ab = TWO*FT_ab

         ! k_pts is 2pi * Bh so formula was divided by 4pi^2.
         diff = self%atom(atom_a)%position-self%atom(atom_b)%position
         separation = norm_(diff)
         g = HALF
         if (separation>2.5d0) g = QUARTER
         g = g*(-HALF)
         basea = (atom_a-1)*6
         if (atom_a==atom_b) then
            delta = TWO*g
            do k = 1,k_max
               FT_ab2 = trace_product_with_(FT_ab(k,:,:),dens_ba)*delta
               x = k_pts(k,1)  ! SBh_x
               y = k_pts(k,2)  ! SBh_y
               z = k_pts(k,3)  ! SBh_z
               tmp1 = FT_ab2*x
               tmp2 = FT_ab2*y
               z2 = TWO*z
               res(basea+1,k) = res(basea+1,k)+tmp1*x
               res(basea+2,k) = res(basea+2,k)+tmp1*y*TWO
               res(basea+3,k) = res(basea+3,k)+tmp1*z2
               res(basea+4,k) = res(basea+4,k)+tmp2*y
               res(basea+5,k) = res(basea+5,k)+tmp2*z2
               res(basea+6,k) = res(basea+6,k)+FT_ab2*z*z
            end do
         else
            baseb = (atom_b-1)*6
            do k = 1,k_max
               FT_ab2 = trace_product_with_(FT_ab(k,:,:),dens_ba)*g
               x = k_pts(k,1)  ! SBh_x
               y = k_pts(k,2)  ! SBh_y
               z = k_pts(k,3)  ! SBh_z
               tmp1 = FT_ab2*x
               tmp2 = FT_ab2*y
               z2 = TWO*z
               tmpa1 = tmp1*x
               tmpa2 = tmp1*y*TWO
               tmpa3 = tmp1*z2
               tmpa4 = tmp2*y
               tmpa5 = tmp2*z2
               tmpa6 = FT_ab2*z*z
               res(basea+1,k) = res(basea+1,k)+tmpa1
               res(basea+2,k) = res(basea+2,k)+tmpa2
               res(basea+3,k) = res(basea+3,k)+tmpa3
               res(basea+4,k) = res(basea+4,k)+tmpa4
               res(basea+5,k) = res(basea+5,k)+tmpa5
               res(basea+6,k) = res(basea+6,k)+tmpa6
               res(baseb+1,k) = res(baseb+1,k)+tmpa1
               res(baseb+2,k) = res(baseb+2,k)+tmpa2
               res(baseb+3,k) = res(baseb+3,k)+tmpa3
               res(baseb+4,k) = res(baseb+4,k)+tmpa4
               res(baseb+5,k) = res(baseb+5,k)+tmpa5
               res(baseb+6,k) = res(baseb+6,k)+tmpa6
            end do
         end if
         call destroy_(FT_ab)
         call destroy_ptr_part_(sh)
      end do
   end subroutine

   PURE subroutine make_FT_deriv_U_1(self,res,dens,k_pts)
   ! Fourier transform of a density described by AO density matrix dens
   ! evaluated at a series of reciprocal space points k_pts
   ! Size of res is [k_max,n_atom*6]
      type(MOLECULE_TYPE), IN :: self
      MAT(CPX,:,:), IN :: dens
      MAT(REAL,:,:), IN :: k_pts
      MAT(CPX,:,:), OUT :: res
      CPX :: FT_ab2,tmp1,tmp2
      INT :: k_max,k,fa,fb,la,lb,na,nb,q
      INT :: atom_a,atom_b,basea,baseb
      REAL :: delta,g,separation,x,y,z
      VEC(REAL,3) :: diff
      MAT3(CPX,:,:,:), allocatable :: FT_ab
      MAT(CPX,:,:), allocatable :: dens_ba
      REAL :: cutoff
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.XTAL:make_FT_deriv_U_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.XTAL:make_FT_deriv_U_1 ... no atom list")
      k_max = size(k_pts,1)
      cutoff = TOL(10)/self%n_shell_pairs
      res = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (skip_ft_(sh,cutoff)) then
            call destroy_ptr_part_(sh)
            cycle
         end if
         call create_(FT_ab,k_max,na,nb)
         call make_FT_pair_(self,FT_ab,k_pts,sh,atom_a,atom_b)
         dens_ba = dens(fb:lb,fa:la)

         ! Count off-diagonals twice.
         if (fa/=fb) FT_ab = TWO*FT_ab

         ! k_pts is 2pi * Bh so formula was divided by 4pi^2.
         diff = self%atom(atom_a)%position-self%atom(atom_b)%position
         separation = norm_(diff)
         g = HALF
         if (separation>2.5d0) g = QUARTER
         g = g*(-HALF)
         basea = (atom_a-1)*6
         if (atom_a==atom_b) then
            delta = TWO*g
            do k = 1,k_max
               FT_ab2 = trace_product_with_(FT_ab(k,:,:),dens_ba)*delta
               x = k_pts(k,1)  ! SBh_x
               y = k_pts(k,2)  ! SBh_y
               z = k_pts(k,3)  ! SBh_z
               tmp1 = FT_ab2*x
               tmp2 = FT_ab2*y
               res(basea+1,k) = res(basea+1,k)+tmp1*x
               res(basea+2,k) = res(basea+2,k)+tmp1*y
               res(basea+3,k) = res(basea+3,k)+tmp1*z
               res(basea+4,k) = res(basea+4,k)+tmp2*y
               res(basea+5,k) = res(basea+5,k)+tmp2*z
               res(basea+6,k) = res(basea+6,k)+FT_ab2*z*z
            end do
         else
            baseb = (atom_b-1)*6
            do k = 1,k_max
               FT_ab2 = trace_product_with_(FT_ab(k,:,:),dens_ba)*g
               x = k_pts(k,1)  ! SBh_x
               y = k_pts(k,2)  ! SBh_y
               z = k_pts(k,3)  ! SBh_z
               tmp1 = FT_ab2*x
               tmp2 = FT_ab2*y
               res(basea+1,k) = res(basea+1,k)+tmp1*x
               res(basea+2,k) = res(basea+2,k)+tmp1*y
               res(basea+3,k) = res(basea+3,k)+tmp1*z
               res(basea+4,k) = res(basea+4,k)+tmp2*y
               res(basea+5,k) = res(basea+5,k)+tmp2*z
               res(basea+6,k) = res(basea+6,k)+FT_ab2*z*z
               res(baseb+1,k) = res(baseb+1,k)+tmp1*x
               res(baseb+2,k) = res(baseb+2,k)+tmp1*y
               res(baseb+3,k) = res(baseb+3,k)+tmp1*z
               res(baseb+4,k) = res(baseb+4,k)+tmp2*y
               res(baseb+5,k) = res(baseb+5,k)+tmp2*z
               res(baseb+6,k) = res(baseb+6,k)+FT_ab2*z*z
            end do
         end if
         call destroy_(FT_ab)
         call destroy_ptr_part_(sh)
      end do
   end subroutine

   PURE subroutine make_FT_pair(self,FT_ab,k_pts,sh,atom_a,atom_b)
   ! Make the FT for one gaussian shell pair "sh" of gaussians.
   ! Includes *partitioning* and *temperature factors*.
      type(MOLECULE_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: FT_ab
      MAT(REAL,:,:), IN :: k_pts
      type(SHELL2_TYPE), IN :: sh
      INT, IN :: atom_a
      INT, IN :: atom_b
      BIN :: is_0
      MAT3(REAL,:,:,:), allocatable :: TF
      MAT(REAL,:,:), allocatable :: part
      MAT(REAL,:,:), allocatable :: Ua,Ub
      STR(len=STR_SIZE) :: model
   ENSURE(allocated(self%crystal),"MOLECULE.XTAL:make_FT_pair ... no crystal data")

      ! Partition factors
      call create_(part,sh%a%n_cc,sh%b%n_cc)
      call make_partition_factors_(self,part,sh,atom_a,atom_b,is_0)
      if (NOT is_0) then

         ! Get thermal factors "TF"
         call create_(TF,size(k_pts,1),sh%a%n_cc,sh%b%n_cc)
         Ua = self%atom(atom_a)%ADP_tensor
         Ub = self%atom(atom_b)%ADP_tensor
         model = partition_model_(self%crystal)
         call make_temperature_factors_(sh,TF,model,k_pts,Ua,Ub)

         ! Do thermal *and* partition factor
         call make_ft_(sh,FT_ab,k_pts,TF,part)
         call destroy_(TF)
      else
         FT_ab = ZERO
      end if
      call destroy_(part)
   end subroutine

   PURE subroutine FT_for_a_in_pair_ab(self,FT,k1,k2,k3,sh,atom_a,atom_b)
   ! Make FT(h) the fourier transform with respect to a list of
   ! k points (k1,k2,k3) of an atom-A partitioned SHELL2 shellpair
   ! "sh" centered on "atom_a" and "atom_b".
   ! NOTE: FOR TWO-ÇENTER MODELS ONLY
      type(MOLECULE_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: FT
      VEC(REAL,:), IN :: k1,k2,k3
      type(SHELL2_TYPE), IN :: sh
      INT, IN :: atom_a,atom_b
      BIN :: is_zero
      MAT(REAL,:,:), allocatable :: p
      INT :: k_max
   ENSURE(allocated(self%crystal),"MOLECULE.XTAL:FT_for_a_in_pair_ab ... no crystal data")
      k_max = size(k1)
      call create_(p,sh%a%n_cc,sh%b%n_cc)

      ! Partition factors
      call make_partition_factors_(self,p,sh,atom_a,atom_b,is_zero)
      call make_FT_parted_(sh,FT,k1,k2,k3,p)
      call destroy_(p)
   end subroutine


!  ============
!  Partitioning
!  ============

   PURE subroutine make_partition_factors(self,part,sh,atom_a,atom_b,is_zero)
   ! Make the partitition factors "part" over the contractions in a
   ! SHELL2 "sh" on centers "atom_a" and "atom_b" and for a particular
   ! thermal ".crystal.xray_data.partition_model",.
   ! NOTE: FOR TWO-ÇENTER MODELS ONLY
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: part
      type(SHELL2_TYPE), IN :: sh
      INT, IN :: atom_a,atom_b
      BIN, optional, OUT :: is_zero
      REAL :: ra,rb,fa,fb
   ENSURE(allocated(self%crystal),"MOLECULE.XTAL:make_partition_factors ... no crystal data")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.XTAL:make_partition_factors ... no crystal data")

      ! Get the rep. factors
      ! These are n_A and n_B in JG (2001) paper
      ra = self%crystal%repetition_factor(atom_a)
      rb = self%crystal%repetition_factor(atom_b)
      if (present(is_zero)) then
         if (ra<1 AND rb<1) then
            is_zero = TRUE
            return
         else
            is_zero = FALSE
         end if
      end if

      ! The f's are the reciprocal of the n's
      ! If any n is zero that region is disregarded.
      if (ra<1) then; fa = ZERO
      else; fa = ONE/ra
      end if
      if (rb<1) then; fb = ZERO
      else; fb = ONE/rb
      end if
      select case (trim(self%crystal%xray_data%partition_model))
      case ("none       "); part = ONE
      case ("tc-coppens "); part = HALF*(fa+fb)
      case ("tc-stewart "); part = HALF*(fa+fb)  ! Use Mulliken
      case ("tc-tanaka  "); call make_Tanaka_partition_(sh,part,fa,fb)
      end select
   end subroutine


!   make_partition_factors(part,sh,ra,rb,is_zero) :: PURE
!   ! Make the partitition factors "part" over the contractions in a
!   ! SHELL2 "sh" with repetition or site symmetry factors "ra" 
!   ! and "rb".
!      self :: IN
!      part :: MAT{REAL}, OUT
!      sh :: SHELL2, IN
!      ra,rb :: REAL, IN
!      is_zero :: BIN, optional, OUT
!
!   ENSURE(.crystal.allocated,"no crystal data")
!
!      fa,fb :: REAL
!
!      if (present(is_zero)) then
!        if (ra<1 AND rb<1) then
!          is_zero = TRUE
!          return
!        else
!          is_zero = FALSE
!        end
!      end
!
!      ! The f's are the reciprocal of the n's
!      ! If any n is zero tat region is disregarded.
!      if (ra<1) then; fa = ZERO
!      else;           fa = ONE/ra
!      end
!
!      if (rb<1) then; fb = ZERO
!      else;           fb = ONE/rb
!      end
!
!      ! Tanaka method only for now ...
!      sh.make_Tanaka_partition(part,fa,fb)
!
!   end

!  ================
!  Thermal smearing
!  ================

!   make_temperature_factors(TF,k_pts,a,b,sh) :: PURE
!   ! Makes the temperature factors "TF" for every pair of the
!   ! contraction coefficients.  Dimensions of "res" are
!   ! [k_max,.a.n_cc,.b.n_cc].
!      self :: IN
!      TF :: MAT3{REAL}, OUT
!      k_pts :: MAT{REAL}, IN
!      a,b :: INT, IN
!      sh :: SHELL2, IN
!
!   ENSURE(NOT .crystal.xray_data.allocated,"no crystal.data")
!
!      Ua,Ub,Uab :: MAT{REAL}(3,3)
!      S,US,diff :: VEC{REAL}(3)
!      separation,bondlength,Ta,Tb,g :: REAL
!      k :: INT
!
!      select case (trim(.crystal.temperature_factor_model))
!
!      case ("none      ")
!
!         TF = ONE
!
!      case ("tc-coppens") 
!      
!         ! tf=HALF[exp(-(1/2)S(Ua)S)+exp(-(1/2)S(Ub)S)]
!         Ua = -HALF * .atom(a).ADP_tensor
!         Ub = -HALF * .atom(b).ADP_tensor
!
!         do k = 1,k_pts.dim1
!            S = k_pts(k,:)
!            US = matmul(Ua,S)
!            Ta = dot_product(S,US)
!            US = matmul(Ub,S)
!            Tb = dot_product(S,US)
!            TF(k,:,:) = HALF*(exp(Ta)+exp(Tb))
!         end
!
!      case ("tc-stewart") 
!      
!         ! tf=exp[-HALF Sg(Ua+Ub)S], g=1/2,1/4]
!         Ua   = .atom(a).ADP_tensor
!         Ub   = .atom(b).ADP_tensor
!         Uab  = Ua + Ub
!         diff = .atom(a).position-.atom(b).position
!
!         bondlength = 2.5
!         separation = diff.norm
!         g = HALF
!         if (separation > bondlength) g = QUARTER
!
!         Uab = -HALF * g * Uab    
!
!         do k = 1,k_pts.dim1
!            S = k_pts(k,:)
!            US = matmul(Uab,S)
!            Ta = dot_product(S,US)
!            TF(k,:,:) = exp(Ta)
!         end
!
!      case ("tc-tanaka ") 
!      
!         ! tf=exp[-HALF S (Ua*alpha + Ub*beta)/gamma S]
!         Ua = .atom(a).ADP_tensor
!         Ub = .atom(b).ADP_tensor
!         sh.make_Tanaka_temperature_factors(TF,k_pts,Ua,Ub)
!
!      end
!
!   end

!   FT_thermally_smear(FT_ab,k_pts,a,b)
!   ! Thermally smear the fourier transform integrals "FT_ab" evaluated
!   ! on "k_pts" for shell pair (ab) centered on atoms "a" and "b" .
!      self :: IN
!      FT_ab :: MAT3{CPX}, INOUT
!      k_pts :: MAT{REAL}, IN
!      a,b :: INT, IN
!
!      Ua,Ub,Uab :: MAT{REAL}(3,3)
!      S,US,diff :: VEC{REAL}(3)
!      separation,bondlength,Ta,Tb,g :: REAL
!      k,k_max :: INT
!
!      k_max = k_pts.dim1
!
!      select case (trim(.crystal.temperature_factor_model))
!
!      case ("none      ")
!
!      case ("tc-coppens") 
!      
!         ! tf=HALF[exp(-(1/2)S(Ua)S)+exp(-(1/2)S(Ub)S)]
!         Ua = -HALF * .atom(a).ADP_tensor
!         Ub = -HALF * .atom(b).ADP_tensor
!
!         do k = 1,k_max
!            S = k_pts(k,:)
!            US = matmul(Ua,S)
!            Ta = dot_product(S,US)
!            US = matmul(Ub,S)
!            Tb = dot_product(S,US)
!            FT_ab(k,:,:) = FT_ab(k,:,:) * HALF*(exp(Ta)+exp(Tb))
!         end
!
!      case ("tc-stewart") 
!      
!         ! tf=exp[-HALF Sg(Ua+Ub)S], g=1/2,1/4
!         Ua = .atom(a).ADP_tensor
!         Ub = .atom(b).ADP_tensor
!         Uab = Ua + Ub
!         diff = .atom(a).position-.atom(b).position
!
!         bondlength = 2.5e0
!         separation = diff.norm
!         g = HALF
!         if (separation > bondlength) g=QUARTER
!
!         Uab = -HALF * g * Uab
!
!         do k = 1,k_max
!            S = k_pts(k,:)
!            US = matmul(Uab,S)
!            Ta = dot_product(S,US)
!            FT_ab(k,:,:) = exp(Ta)*FT_ab(k,:,:)
!         end
!
!      case ("tc-tanaka") 
!      
!         ! tf=exp[-HALF S (Ua*alpha + Ub*beta)/gamma S]
!         DIE("cannot thermally smear at contracted level.")
!
!      case default
!         DIE("unknown model, "//trim(.crystal.temperature_factor_model))
!
!      end
!
!   end

!  ==================
!  Archived integrals
!  ==================

   subroutine get_FT_ints(self)
   ! Get the fourier transform of the overlap integrals.
   ! If the archive file exists, read it; otherwise make it.
      type(MOLECULE_TYPE), IN :: self
      type(ARCHIVE_TYPE) :: arch
      call set_(arch,self%name,"FT_ints")
      if (NOT exists_(arch)) call make_FT_ints_(self)
   end subroutine

   subroutine make_FT_ints(self)
   ! Make Fourier transform of the overlap integrals on an archive
      type(MOLECULE_TYPE), IN :: self
      type(ARCHIVE_TYPE) :: arch
      MAT(REAL,:,:), allocatable :: k_pts
      INT :: fa,la,na
      INT :: fb,lb,nb
      INT :: q,atom_a,atom_b
      INT :: n_refl,n_uniq
      MAT3(CPX,:,:,:), allocatable :: FT_ab_eq,FT_ab
      type(SHELL2_TYPE) :: sh
   ENSURE(allocated(self%crystal),"MOLECULE.XTAL:make_FT_ints ... no crystal")
      call set_(arch,self%name,"FT_ints")
      call open_for_(arch,"write-only")
      n_refl = size(self%crystal%xray_data%reflections)
      n_uniq = n_unique_SF_k_pts_(self%crystal)
      call create_(k_pts,n_uniq,3)
      call make_unique_SF_k_pts_(self%crystal%xray_data%reflections,k_pts,self%crystal%spacegroup,self%crystal%unit_cell)
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         call create_(FT_ab_eq,n_uniq,na,nb)
         call create_(FT_ab,n_refl,na,nb)
         call make_FT_pair_(self,FT_ab_eq,k_pts,sh,atom_a,atom_b)
         call sum_unique_sf_ints_(self%crystal%spacegroup,FT_ab,FT_ab_eq,self%crystal%xray_data%reflections)
         call write_(arch%Bfile,FT_ab)
         call destroy_(FT_ab)
         call destroy_(FT_ab_eq)
         call destroy_ptr_part_(sh)
      end do
      call destroy_(k_pts)
      call close_(arch)
   end subroutine

end module
