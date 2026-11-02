!===================================================================
!
! CRYSTAL: Data structure for crystals
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1999
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
!===================================================================
module CRYSTAL_MODULE

#  include "macros"

   include "crystal.use"

   implicit none

   include "crystal.int"


contains

!  ===========
!  Allocatiion
!  ===========

   PURE subroutine create(self)
   ! Allocate an object
      type(CRYSTAL_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"CRYSTAL:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(CRYSTAL_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"CRYSTAL:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Erase all pointer information
      type(CRYSTAL_TYPE), INOUT :: self
      call destroy_ptr_part_(self%spacegroup)
      call destroy_fragment_data_(self)
      call destroy_asymmetric_unit_(self)
      call destroy_data_(self)
   end subroutine

   PURE subroutine destroy_data(self)
   ! Erase all data pointer information
      type(CRYSTAL_TYPE), INOUT :: self
      call destroy_(self%xray_data)
      call destroy_(self%xray_r_free_data)

   ! .pnd_data.destroy
   ! .pnd_r_free_data.destroy
   end subroutine

   PURE subroutine destroy_fragment_data(self)
   ! Destroy the geometry and symmetry data for the fragment and unit cell.
   ! NOTE: Don't destroy asymmetric unit atom info
      type(CRYSTAL_TYPE), INOUT :: self
      call destroy_(self%fragment_atom)
      call destroy_(self%fragment_geometry)

      ! Never destroy .asymmetric_unit_geometry
      ! Never destroy .asymmetric_unit_atom
      call destroy_(self%unit_cell_geometry)
      call destroy_(self%unit_cell_atom)
      call destroy_(self%unique_unit_cell_atom)
      call destroy_(self%unique_uc_atom_offset)
      call destroy_(self%is_asym_unit_cell_atom)
      call destroy_(self%asym_atom_for_unit_cell_atom)
      call destroy_(self%asym_symop_for_unit_cell_atom)
      call destroy_(self%asym_shift_for_unit_cell_atom)
      call destroy_(self%frag_parent_for_cell_atom)
      call destroy_(self%frag_atom_for_unit_cell_atom)
      call destroy_(self%unit_cell_atom_for_frag_atom)
      call destroy_(self%unit_cell_shft_for_frag_atom)
      call destroy_(self%frag_atom_for_asym_atom)
      call destroy_(self%asym_atom_for_frag_atom)
      call destroy_(self%unique_frag_atom)
      call destroy_(self%unique_index_for_frag_atom)
      call destroy_(self%unique_atom_for_frag_atom)
      call destroy_(self%unique_symop_for_frag_atom)
      call destroy_(self%asym_symop_for_frag_atom)
      call destroy_(self%asym_symops_for_frag_atom)
   ! .frag_pair_for_asym_atom_pair.destroy
   ! .asym_pair_for_frag_atom_pair.destroy
      call destroy_(self%repetition_factor)
      call destroy_(self%asym_atom_sym_factor)
      call destroy_(self%asym_atom_stabilizer)
      call destroy_(self%unit_cell_connection)
      call destroy_(self%uc_vdw_connection)
      call destroy_(self%unit_cell_mol_for_atom)
      call destroy_(self%stabilizer_symop)
      call destroy_(self%str)
      call destroy_(self%ptr)
      call destroy_(self%dtr)
      call destroy_(self%ftr)
      call destroy_(self%gtr)
      self%fragment_info_made = FALSE
   end subroutine

   PURE subroutine destroy_asymmetric_unit(self)
   ! Destroy the asymmetric unit geometry. Normally this will be set
   ! by reading a CIF file. If not, it will be set from a given
   ! fragment atom list, in the "update_fragment_info" routine. Once
   ! set, it is never destroyed unless explicitly requested by a call
   ! or a keyword.
      type(CRYSTAL_TYPE), INOUT :: self
      call destroy_(self%asymmetric_unit_geometry)
      call destroy_(self%asymmetric_unit_atom)
      self%asymmetric_unit_source = "?"

      ! Destroy unit cell as well ...
      ! which depends on asymmetric unit
      call destroy_(self%unit_cell_geometry)
      call destroy_(self%unit_cell_atom)
   end subroutine


!  ===========
!  Set methods
!  ===========

   subroutine set_minimal_copy(self,crystal)
   ! Set self to be crystal.
   ! A finalize can generate the remaining, if needed.
      type(CRYSTAL_TYPE), INOUT :: self
      type(CRYSTAL_TYPE), IN :: crystal
      self = crystal
      call destroy_ptr_part_(self)
      self%spacegroup = crystal%spacegroup
      if (allocated(crystal%xray_data)) self%xray_data = crystal%xray_data

   ! if (crystal.pnd_data.allocated)  .pnd_data = crystal.pnd_data
   end subroutine

   subroutine update_diffraction_data(self)
   ! Set angles, equivalence factors, and the free reflection data.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: list
      INT :: n_refl,n_unique
      if (allocated(self%xray_data)) then

         ! Test for equivalents & remove
         ! This could also go in xray_data.update below ...
         if (allocated(self%xray_data%reflections)) then
            if (NOT self%xray_data%use_equivalents) then
               n_refl = size(self%xray_data%reflections)
               call make_list_of_equivalents_v2_(self,list,n_unique)
               if (n_unique<n_refl) then
                  call flush_(stdout)
                  call flush_(stdout)
                  call text_(stdout,"===============================")
                  call text_(stdout,"Equivalent reflections detected")
                  call text_(stdout,"===============================")
                  call flush_(stdout)
                  call text_(stdout,". You have equivalent reflections in your data!")
                  call text_(stdout,"  This isn't allowed so I will keep only the first ")
                  call text_(stdout,"  equivalent, and purge the rest.")
                  call flush_(stdout)
                  call text_(stdout,". If you don't like this then first merge your data.")
                  call flush_(stdout)
                  call text_(stdout,". Alternatively, set use_equivalents= to stop this.")
                  call flush_(stdout)
                  call show_(stdout,"No. of reflections before removing equivalents = ",n_refl)
                  call show_(stdout,"No. of reflections after purging               = ",n_unique)
                  call remove_equivalents_(self,list,n_unique)
               end if
               call destroy_(list)
            end if
         end if

         ! Prune reflections
         call update_xray_data_(self)
         if (self%r_free_percentage>0 AND allocated(self%xray_data%reflections)) then
            self%xray_r_free_data = self%xray_data
            call destroy_(self%xray_r_free_data%reflections)
            call set_r_free_reflections_(self,self%xray_data%reflections,self%xray_r_free_data%reflections)
         end if
      end if

   ! if (.pnd_data.allocated) then
   !   .pnd_data.update(.unit_cell,.spacegroup) ! NOT PURE
   !   if (.r_free_percentage>0 AND .pnd_data.reflections.allocated) then
   !      .pnd_r_free_data = .pnd_data
   !      .pnd_r_free_data.reflections.destroy
   !      .set_r_free_reflections(.pnd_data.reflections,.pnd_r_free_data.reflections)
   !   end
   ! end
   end subroutine

   subroutine set_r_free_reflections(self,data,free_data)
   ! Set the R free reflection data. "data" is destroyed and created
   ! again without those reflections in "free_data".
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: data
      VEC(type(REFLECTION_TYPE),:), allocatable, OUT :: free_data
      VEC(REAL,:), allocatable :: harvest
      VEC(INT,:), allocatable :: free,keep
      REAL :: percentage
      INT :: i,n
   ENSURE(allocated(data),"CRYSTAL:set_r_free_reflections ... no diffraction data")
   ENSURE(NOT allocated(free_data),"CRYSTAL:set_r_free_reflections ... r_free_data exists")
      if (self%r_free_percentage<=0) return
      if (NOT allocated(data)) return

      ! Set the percentage of reflections to reject
      percentage = self%r_free_percentage/100.0d0

      ! Harvest random numbers
      n = size(data)
      call create_(harvest,n)
      if (is_master_processor_(tonto)) then
         call random_number(harvest)  ! NOT PURE
      end if
      PARALLEL_BROADCAST(harvest,master_processor_(tonto))

      ! Reflections for r_free have p<percentage
      ! Otherwise, they are kept and used for fitting
      do i = 1,n
         if (harvest(i)<percentage) then; call append_(free,i)
         else; call append_(keep,i)
         end if
      end do
      call destroy_(harvest)

   ! stdout.show("No. of reflections kept for R free =",free.dim)
   ! stdout.flush

      ! Set free_data & kept data
      free_data = data(free)
      data = data(keep)

      ! Clean
      call destroy_(free)
      call destroy_(keep)
   end subroutine


!  ===================
!  Cluster set methods
!  ===================

   PURE subroutine set_atom_density_cutoff(self,tol)
   ! Set the atom_density_cutoff used to eliminate spherical atomic
   ! density contributions less than "tol". The interpolator table
   ! will not hold values smaller than "tol".
      type(CRYSTAL_TYPE), INOUT :: self
      REAL, IN :: tol
   ENSURE(tol>ZERO,"CRYSTAL:set_atom_density_cutoff ... tol must be +ve")
      self%atom_density_cutoff = tol
   end subroutine

   PURE subroutine set_defragment(self,val)
   ! Generic set
      type(CRYSTAL_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%defragment = val
   end subroutine

   PURE subroutine set_fragment_geometry(self,fragment_geometry,cartesian)
   ! Makes an atom list from "fragment_geometry" to which
   ! .fragment_atoms gets set. The .asymmetric_unit_atoms (which must
   ! be in cartesian system) and .unit_cell_geometry have to be
   ! defined. Set "cartesian" to TRUE if the "fragment_geometry" is in
   ! cartesians, default is fractional coordinates.
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: fragment_geometry
      BIN, optional, IN :: cartesian
      VEC(type(ATOM_TYPE),:), allocatable :: atom
      VEC(INT,:), allocatable :: list
      VEC(REAL,3) :: pf,pu
      MAT(REAL,:,:), allocatable :: fg
      INT :: n_frag_atoms,f,u,a
      REAL :: tol
      BIN :: cart,found
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:set_fragment_geometry ... no .unit_cell_geometry")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:set_fragment_geometry ... no .asymmetric_unit_atom")
   ENSURE(allocated(self%asym_atom_for_unit_cell_atom),"CRYSTAL:set_fragment_geometry ... no .asym_atom_for_unit_cell_atom")
      cart = FALSE
      if (present(cartesian)) cart = cartesian

      ! Transform fragment geometry to fractional
      fg = fragment_geometry
      if (cart) call change_into_fractional_(self%unit_cell,fg)

      ! Create the list of atom indices -- leaky!!
      n_frag_atoms = size(fg,2)
      call create_(list,n_frag_atoms)

      ! Tolerances to see if atom positions are the same
      ! --- getting this number wrong can stuff things
      tol = self%same_atom_tol

      ! Find and create fragment atoms
      do f = 1,n_frag_atoms

         ! Shift fragment atom to unit cell
         pf = fg(:,f)
         call put_to_unit_cell_(self,pf)
         found = FALSE

         ! Loop unit cell atoms
         do u = 1,self%n_unit_cell_atoms

            ! Is u the same as f?
            pu = self%unit_cell_geometry(:,u)
            found = same_as_(pu,pf,tol)
            if (NOT found) cycle

            ! Set asymmetric atom index for atom f
            a = self%asym_atom_for_unit_cell_atom(u)
            list(f) = a
            exit
         end do
         ENSURE(found,"CRYSTAL:set_fragment_geometry ... fragment atom "//trim(to_str_(f))//" was not found in the unit cell")
      end do

      ! Make the atom list, cartesian coordinates
      atom = self%asymmetric_unit_atom(list)
      call set_axis_system_to_(atom,"crystal")
      call set_positions_to_(atom,fg)
      call zero_pADP_errors_(atom)
      call change_axis_system_to_(atom,"cartesian",self%unit_cell)

      ! Make the fragment data; make it destroyable
      call update_fragment_info_(self,atom)

      ! Clean
      call destroy_(atom)
      call destroy_(list)
      call destroy_(fg)
   end subroutine

   PURE subroutine set_generation_method(self,method)
   ! Set .generation_method, which tells how to generate the cluster
   ! e.g. whether to add atoms to the cluster within a certain radius
   ! of the starting fragment, or whether to use the unit cell ad the
   ! cluster, or just the initial starting fragment (i.e. the null
   ! cluster) or the initial fragment and the unit cell.
      type(CRYSTAL_TYPE), INOUT :: self
      STR(len=*), IN :: method
      self%generation_method = method
      call to_lower_case_(self%generation_method)
      select case (self%generation_method)
      case ("for_asymm_unit_hs         "); call set_reorder_in_mols_(self,FALSE)
      case ("for_hirshfeld_surface     ")
      case ("fragment                  ")
      case ("unit_cell                 ")
      case ("unit_cell_plus            ")
      case ("for_unit_cell_density     ")
      case ("offset_unit_cell          ")
      case ("offset_unit_cell_density  ")
      case ("within_radius             ")
      end select
   end subroutine

   PURE subroutine set_radius(self,val)
   ! Generic set
      type(CRYSTAL_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%radius = val
   end subroutine

   PURE subroutine set_reorder_in_mols(self,val)
   ! Generic set
      type(CRYSTAL_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%reorder_in_mols = val
   end subroutine

   PURE subroutine set_unit_cell_offset(self,offset)
   ! Set the unit_cell_offset to use when making an offset unit cell.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(REAL,3), IN :: offset
   ENSURE(all(-HALF<=offset) AND all(offset<=HALF),"CRYSTAL:set_unit_cell_offset ... offset must be between -1/2 and +1/2")
      self%unit_cell_offset = offset
   end subroutine


!  ============
!  Read methods
!  ============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(CRYSTAL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","CRYSTAL:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"CRYSTAL:read_keywords ... unexpected end of file while reading list keywords")
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
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
      type(CRYSTAL_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                             ")  ! exit read_loop
   ! case ("asymmetric_unit_geometry=     "); .read_asymmetric_unit_geometry
   ! case ("destroy                       "); .destroy_ptr_part
   ! case ("bijvoet                       "); .make_list_of_Bijvoet_classes
      case ("destroy_asymmetric_unit       "); call destroy_asymmetric_unit_(self)
      case ("destroy_spacegroup            "); call destroy_ptr_part_(self%spacegroup)
   ! case ("expand_unique_reflections     "); .expand_unique_reflections
      case ("finalize=                     "); call read_finalize_(self)
      case ("junk=                         "); call read_junk_(self)
   ! case ("kind=                         "); .read_kind
   ! case ("merge_equivalents             "); .merge_equivalents
   ! case ("merge_ordered_equivalents     "); .merge_ordered_equivalents
   ! case ("pnd_data=                     "); .read_pnd_data
      case ("prune_asymmetric_unit=        "); call read_prune_asymmetric_unit_(self)
      case ("put                           "); call put_(self)
      case ("put_bond_info_with_errors     "); call put_bond_info_with_errors_(self)
      case ("put_bond_info_with_errors_to  "); call put_bond_info_with_errors_to_(self)
      case ("put_data_analysis             "); call put_X_data_analysis_(self)
      case ("put_all_reflection_data       "); call put_all_reflection_data_(self)
      case ("put_reflections               "); call put_reflection_data_(self)
      case ("put_reflection_data           "); call put_reflection_data_(self)
      case ("put_reflection_intensities    "); call put_reflection_intensities_(self)
      case ("put_reflection_intensities_xd "); call put_reflection_intensities_XD_(self)
      case ("r_free_percentage=            "); call read_r_free_percentage_(self)
      case ("read_cif                      "); call read_CIF_(self)
      case ("redirect                      "); call redirect_(self)
      case ("reorder_in_mols=              "); call read_reorder_in_mols_(self)
      case ("repetition_factors=           "); call read_repetition_factors_(self)
      case ("revert                        "); call revert_(self)
      case ("same_atom_tol=                "); call read_same_atom_tol_(self)
      case ("spacegroup=                   "); call read_spacegroup_(self)
      case ("unit_cell=                    "); call read_unit_cell_(self)
      case ("unit_cell_tol=                "); call read_unit_cell_tol_(self)
      case ("units=                        "); call read_units_(self)
      case ("use_manual_repetition_factors="); call read_use_manual_rep_factors_(self)
      case ("xray_data=                    "); call read_xray_data_(self)

      case ("! Cluster keywords            ")

      case ("atom_density_cutoff=          "); call read_atom_density_cutoff_(self)
      case ("defragment=                   "); call read_defragment_(self)
      case ("fractional_fragment=          "); call read_fractional_fragment_(self)
      case ("generation_method=            "); call read_generation_method_(self)
      case ("make_cluster_info             "); call make_cluster_info_(self)
      case ("unit_cell_offset=             "); call read_unit_cell_offset_(self)
      case ("put_averaged_lff_l_tensors    "); call put_averaged_LFF_tensors_(self)
      case ("put_centroid_lff_l_tensors    "); call put_centroid_LFF_tensors_(self)
      case ("put_centroid_lff_l_tensors_old"); call put_centroid_LFF_tensors_old_(self)
      case ("put_cluster_info              "); call put_cluster_info_(self)
      case ("put_unit_cell_geometry_cart   "); call put_unit_cell_geometry_cartesian_(self)
      case ("put_lff_info                  "); call put_LFF_info_(self)
      case ("radius=                       "); call read_radius_(self)

      case default; allocate(tonto%known_keywords(42))
      tonto%known_keywords(1) = "}                             "
      tonto%known_keywords(2) = "destroy_asymmetric_unit       "
      tonto%known_keywords(3) = "destroy_spacegroup            "
      tonto%known_keywords(4) = "finalize=                     "
      tonto%known_keywords(5) = "junk=                         "
      tonto%known_keywords(6) = "prune_asymmetric_unit=        "
      tonto%known_keywords(7) = "put                           "
      tonto%known_keywords(8) = "put_bond_info_with_errors     "
      tonto%known_keywords(9) = "put_bond_info_with_errors_to  "
      tonto%known_keywords(10) = "put_data_analysis             "
      tonto%known_keywords(11) = "put_all_reflection_data       "
      tonto%known_keywords(12) = "put_reflections               "
      tonto%known_keywords(13) = "put_reflection_data           "
      tonto%known_keywords(14) = "put_reflection_intensities    "
      tonto%known_keywords(15) = "put_reflection_intensities_xd "
      tonto%known_keywords(16) = "r_free_percentage=            "
      tonto%known_keywords(17) = "read_cif                      "
      tonto%known_keywords(18) = "redirect                      "
      tonto%known_keywords(19) = "reorder_in_mols=              "
      tonto%known_keywords(20) = "repetition_factors=           "
      tonto%known_keywords(21) = "revert                        "
      tonto%known_keywords(22) = "same_atom_tol=                "
      tonto%known_keywords(23) = "spacegroup=                   "
      tonto%known_keywords(24) = "unit_cell=                    "
      tonto%known_keywords(25) = "unit_cell_tol=                "
      tonto%known_keywords(26) = "units=                        "
      tonto%known_keywords(27) = "use_manual_repetition_factors="
      tonto%known_keywords(28) = "xray_data=                    "
      tonto%known_keywords(29) = "! Cluster keywords            "
      tonto%known_keywords(30) = "atom_density_cutoff=          "
      tonto%known_keywords(31) = "defragment=                   "
      tonto%known_keywords(32) = "fractional_fragment=          "
      tonto%known_keywords(33) = "generation_method=            "
      tonto%known_keywords(34) = "make_cluster_info             "
      tonto%known_keywords(35) = "unit_cell_offset=             "
      tonto%known_keywords(36) = "put_averaged_lff_l_tensors    "
      tonto%known_keywords(37) = "put_centroid_lff_l_tensors    "
      tonto%known_keywords(38) = "put_centroid_lff_l_tensors_old"
      tonto%known_keywords(39) = "put_cluster_info              "
      tonto%known_keywords(40) = "put_unit_cell_geometry_cart   "
      tonto%known_keywords(41) = "put_lff_info                  "
      tonto%known_keywords(42) = "radius=                       "
      call unknown_(tonto,word,"CRYSTAL:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(CRYSTAL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_junk(self)
   ! Read in a junk string, useful for ignoring a field
      type(CRYSTAL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call skip_next_item_(stdin)
   end subroutine

   subroutine redirect(self)
   ! Redirect input. (This code is for non-pointer self objects).
      type(CRYSTAL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: filename

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      filename = next_str_(stdin)
      call redirect_(stdin,trim(filename))
   end subroutine

   subroutine revert(self)
   ! Revert back to previous stdin file. (This code is for non-pointer self
   ! objects).
      type(CRYSTAL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call revert_(stdin)
   end subroutine


!   read_kind
!   ! Read the kind of crystal experiment
!      self :: INOUT
!
!      stdin.read(.data_kind)
!      .data_kind.to_lower_case
!
!      select case (.data_kind)
!      case("x-ray")
!      case("pnd  ")
!      case default; UNKNOWN(.data_kind)
!      end
!
!   end

   subroutine read_finalize(self)
   ! Read whether to finalize the object.
      type(CRYSTAL_TYPE), INOUT :: self
      call read_(stdin,self%finalize)
   end subroutine

   subroutine read_r_free_percentage(self)
   ! Read the R free percentage
      type(CRYSTAL_TYPE), INOUT :: self
      call read_(stdin,self%r_free_percentage)
      DIE_IF(self%r_free_percentage<0,"CRYSTAL:read_r_free_percentage ... percentage must be non-negative")
      DIE_IF(self%r_free_percentage>100,"CRYSTAL:read_r_free_percentage ... percentage must be less than 100")
   end subroutine

   subroutine read_same_atom_tol(self)
   ! Read the tolerance for deciding if two atoms are
   ! the same in fractional coordinates.
      type(CRYSTAL_TYPE), INOUT :: self
      call read_(stdin,self%same_atom_tol)
   end subroutine

   subroutine read_spacegroup(self)
   ! Read the spacegroup
      type(CRYSTAL_TYPE), INOUT :: self
      call read_keywords_(self%spacegroup)
      call analyze_(self%spacegroup)
   end subroutine

   subroutine read_unit_cell(self)
   ! Read the unit cell information
      type(CRYSTAL_TYPE), INOUT :: self
   ENSURE(NOT allocated(self%xray_data),"CRYSTAL:read_unit_cell ... enter unit cell information before xray_data= !")
! ENSURE(.pnd_data.deallocated, "enter unit cell information before pnd_data= !")
      call read_keywords_(self%unit_cell)
      call make_info_(self%unit_cell)
   end subroutine

   subroutine read_unit_cell_tol(self)
   ! Read the tolerance for deciding if two atoms are
   ! the same in fractional coordinates.
      type(CRYSTAL_TYPE), INOUT :: self
      call read_(stdin,self%unit_cell_tol)
   end subroutine

   subroutine read_xray_data(self)
   ! Read in the xray diffraction data
      type(CRYSTAL_TYPE), INOUT :: self
   WARN_IF(NOT self%unit_cell%info_made,"CRYSTAL:read_xray_data ... normally, you enter unit_cell= information before xray_data=")
   WARN_IF(NOT self%spacegroup%analyzed,"CRYSTAL:read_xray_data ... normally, you enter spacegroup= information before xray_data=")
   WARN_IF(allocated(self%xray_data),"CRYSTAL:read_xray_data ... xray_data is already defined")
      if (NOT allocated(self%xray_data)) call create_(self%xray_data)
      call read_keywords_(self%xray_data)
      call update_xray_data_(self)
   end subroutine


!   read_pnd_data :: leaky
!   ! Read in the pnd diffraction data
!      self :: INOUT
!
!   ENSURE(.unit_cell.info_made,"enter unit cell information before pnd_data= !")
!   ENSURE(.spacegroup.analyzed,"enter spacegroup information before xray_data= !")
! ! WARN_IF(.pnd_data.allocated,"pnd diffraction data already defined!")
!
!      .pnd_data.destroy
!      .pnd_data.create
!      .pnd_data.data_kind = "pnd"
!      .pnd_data.read_keywords
!      .pnd_data.update(.unit_cell,.spacegroup)
!
!   end

   subroutine read_use_manual_rep_factors(self)
   ! Read whether to use manually entered repetition factors.
   ! If set set FALSE, the repetition factors will be destroyed.
      type(CRYSTAL_TYPE), INOUT :: self
      call read_(stdin,self%use_manual_repetition_factors)
      if (NOT self%use_manual_repetition_factors) call destroy_(self%repetition_factor)
   end subroutine

   subroutine read_reorder_in_mols(self)
   ! Read whether to reorder any generated cluster of atoms into
   ! consecutive atom-connected fragments. Set to FALSE if you are
   ! generating a cluster for Hirshfeld weight calculations.
      type(CRYSTAL_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_reorder_in_mols_(self,val)
   end subroutine

   subroutine read_repetition_factors(self)
   ! Read in the crystal fragment repetition factors. Useful to get structure
   ! factor contributions from a small portion of the fragment.
      type(CRYSTAL_TYPE), INOUT :: self
      call read_all_(stdin,self%repetition_factor)
      self%use_manual_repetition_factors = TRUE
   end subroutine

   subroutine read_prune_asymmetric_unit(self)
   ! Read whether to prune the asymmetric unit or not. Sometimes the asymmetric
   ! unit may not be asymmetric i.e. it may have symmetry-equivalent atoms
   ! repeated. This could be because of an input or CIF file error, or it could
   ! be genuinely correct because the repeated atom corresponds to a disordered
   ! position. For Hirshfeld surface plots, which can cope with disorder, you
   ! probably do not want to prune the asymmetric atom list, but for crystal
   ! calculations you must do so.
      type(CRYSTAL_TYPE), INOUT :: self
      call read_(stdin,self%prune_asymmetric_unit)
   end subroutine

   subroutine read_CIF_0(self)
   ! Read information from a Crystallographic Information File object, with
   ! the "name" taken from stdin.
      type(CRYSTAL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name
      BIN :: found
      type(CIF_TYPE), allocatable :: cif

      ! Read name from stdin
      call read_(stdin,name)

      ! Create and open the CIF
      call destroy_(cif)
      call create_(cif,name)
      call open_(cif)
      call find_crystal_data_block_(cif,found)
      ENSURE(found,"CRYSTAL:read_CIF_0 ... no crystal data block found")

      ! Read
      call read_CIF_(self,cif)

      ! Clean
      call destroy_(cif)
   end subroutine

   subroutine read_CIF_1(self,cif,skip_atoms)
   ! Read information from "cif".  If "skip_atoms" is TRUE,
   ! the fragment_atom coordinates are not processed.
      type(CRYSTAL_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN, optional, IN :: skip_atoms
      BIN :: skip
      skip = FALSE
      if (present(skip_atoms)) skip = skip_atoms

      ! Read the spacegroup &  unit_cell
      call read_CIF_spacegroup_and_cell_(self,cif)

      ! Read the atom list (if requested)
      if (NOT skip) then
         call read_CIF_(self%fragment_atom,cif)
      end if

      ! Assume this is X-ray data! BAD !!!
      if (NOT allocated(self%xray_data)) call create_(self%xray_data)
      call set_data_kind_(self%xray_data,"x-ray")
      call read_CIF_(self%xray_data,cif)
      call update_xray_data_(self)

      ! Update
      call update_diffraction_data_(self)

      ! Eventually, this will be used
      self%asymmetric_unit_source = "from-cif"
   end subroutine

   subroutine update_xray_data(self)
   ! Update .xray_data for the unit cell and spacegroup, which prunes
   ! the reflections. A repeated xray_data= block re-prunes in place;
   ! the form factors on disk are cut down to match, not recomputed.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable :: old
   ENSURE(allocated(self%xray_data),"CRYSTAL:update_xray_data ... no diffraction data")
      if (allocated(self%xray_data%reflections)) old = self%xray_data%reflections
      call update_(self%xray_data,self%unit_cell,self%spacegroup)  ! NOT PURE
      if (allocated(old)) then
         if (size(self%xray_data%reflections)<size(old)) call prune_disk_SFs_(self,old)
         call destroy_(old)
      end if
   end subroutine

   subroutine prune_xray_data(self)
   ! Prune .xray_data.reflections by the set cutoffs, and cut the form
   ! factors on disk down to match. Every prune of a list that already
   ! has form factors on disk must come through here, not through
   ! DIFFRACTION_DATA.SET:prune_reflections directly.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable :: old
   ENSURE(allocated(self%xray_data),"CRYSTAL:prune_xray_data ... no diffraction data")
   ENSURE(allocated(self%xray_data%reflections),"CRYSTAL:prune_xray_data ... no reflection data")
      old = self%xray_data%reflections
      call prune_reflections_(self%xray_data)  ! NOT PURE
      if (size(self%xray_data%reflections)<size(old)) call prune_disk_SFs_(self,old)
      call destroy_(old)
   end subroutine

   subroutine prune_disk_SFs(self,old)
   ! Cut each unique fragment atom's "<tag>-SFs" file, written for the
   ! reflection list "old", down to the reflections now in .xray_data,
   ! which must be a subset of "old" in the same order. A file is one
   ! record of .spacegroup.n_unique_SF_symops blocks, each in reflection
   ! order, so only the pruned reflections drop out of every block.
   ! Atoms without a file (caps, zero site factor) are skipped.
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(REFLECTION_TYPE),:), IN :: old
      INT :: n_sym,n_old,n_new,u,f,s,n,i
      VEC(INT,:), allocatable :: keep
      VEC(CPX,:), allocatable :: ff_old,ff_new
      type(ARCHIVE_TYPE), allocatable :: arch
   ENSURE(allocated(self%xray_data),"CRYSTAL:prune_disk_SFs ... no diffraction data")
   ENSURE(allocated(self%xray_data%reflections),"CRYSTAL:prune_disk_SFs ... no reflection data")
      if (NOT self%xray_data%use_disk_SFs) return
      if (NOT self%fragment_info_made) return
      if (NOT have_F_calc_(self%xray_data%reflections)) return  ! no refinement yet, so no files of ours
      n_sym = self%spacegroup%n_unique_SF_symops
      n_old = size(old)
      n_new = size(self%xray_data%reflections)

      ! keep(n) is where the n-th current reflection sits in "old"
      call create_(keep,n_new)
      i = 0
      do n = 1,n_new
         do
            i = i+1
            DIE_IF(i>n_old,"CRYSTAL:prune_disk_SFs ... pruned reflection list is not a subset of the old one")
            if (old(i)%h1/=self%xray_data%reflections(n)%h1) cycle
            if (old(i)%h2/=self%xray_data%reflections(n)%h2) cycle
            if (old(i)%h3/=self%xray_data%reflections(n)%h3) cycle
            exit
         end do
         keep(n) = i
      end do
      call create_(ff_old,n_sym*n_old)
      call create_(ff_new,n_sym*n_new)
      call flush_(stdout)
      call show_(stdout,"Form factors on disk, reflections  =",n_old)
      call show_(stdout,"Cut to the kept reflections        =",n_new)

      ! Loop over unique *frag* atoms
      do u = 1,self%n_unique_frag_atoms
         f = self%unique_frag_atom(u)

         ! Read, cut down, and write back. FILE:read is master-only and
         ! broadcasts; FILE:write is master-only. See shift_update_ff.
         call create_(arch,trim(self%fragment_atom(f)%tag)//"-SFs")
         if (self%xray_data%use_text_SFs) call set_format_(arch,"ascii")
         if (exists_(arch)) then
            call read_(arch,ff_old)
            do s = 1,n_sym
               do n = 1,n_new
                  ff_new((s-1)*n_new+n) = ff_old((s-1)*n_old+keep(n))
               end do
            end do
            call write_(arch,ff_new)
         end if
         call destroy_(arch)
      end do

      ! Clean
      call destroy_(ff_new)
      call destroy_(ff_old)
      call destroy_(keep)
   end subroutine

   subroutine read_CIF_spacegroup_and_cell(self,cif)
   ! Read the spacegroup and unit_cell information from "cif". 
   ! NOTE: defaults are *not* set in case the user wants to set some x-ray 
   ! refinement options before the CIF and its reflections are processed.
      type(CRYSTAL_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN :: found
      VEC(STR(len=STR_SIZE),:), allocatable :: itemvec
   ! clean   :: CRYSTAL

      ! Set defaults
   ! self = clean

      ! Read the chemical formula if it is there
      call read_item_(cif,"_chemical_formula_sum",self%CIF_chemical_formula,itemvec,found)
      if (NOT found) self%CIF_chemical_formula = "?"
      ENSURE(NOT allocated(itemvec),"CRYSTAL:read_CIF_spacegroup_and_cell ... multi line chemical formula")

      ! Read the spacegroup
      call read_CIF_(self%spacegroup,cif)

      ! Read the unit cell
      call read_CIF_(self%unit_cell,cif)
   end subroutine

   subroutine read_CIF_xray_data(self,cif)
   ! Read only the xray_data information from "cif".
      type(CRYSTAL_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
   ENSURE(self%unit_cell%info_made,"CRYSTAL:read_CIF_xray_data ... enter unit cell information before xray_data= !")
   ENSURE(self%spacegroup%analyzed,"CRYSTAL:read_CIF_xray_data ... enter spacegroup information before xray_data= !")
      if (NOT allocated(self%xray_data)) call create_(self%xray_data)
      call set_data_kind_(self%xray_data,"x-ray")
      call read_CIF_(self%xray_data,cif)
      call update_xray_data_(self)

      ! Update
      call update_diffraction_data_(self)
   end subroutine


!   read_asymmetric_unit_geometry :: private
!   ! Read in the asymmetric unit geometry in crystal coordinates.
!   ! The coordinates are read in as a single vector ordered as x,y,z
!   ! incrementing fastest, for the first to the last atom.
!      self :: INOUT
!      geometry :: VEC{REAL}@
!
! ! WARN_IF(.asymmetric_unit_geometry.allocated,"asymmetric_unit_geometry exists!")
!
!      stdin.read_all(geometry)
!      ENSURE(modulo(geometry.dim,3)==0,"number of coordinates must be divisible by 3")
!
!      .n_asymmetric_unit_atoms = geometry.dim/3
!      .asymmetric_unit_geometry.destroy
!      .asymmetric_unit_geometry.create(3,.n_asymmetric_unit_atoms)
!      .asymmetric_unit_geometry = reshape(geometry,[3,.n_asymmetric_unit_atoms])
!      geometry.destroy
!
!      .asymmetric_unit_source = "manual-input"
!
!   end

!  Cluster keywords

   subroutine read_atom_density_cutoff(self)
   ! This number is used to define a cluster radius (see above). The radius is
   ! defined from it, as the *largest* distance from any atom in the
   ! asymmetric_cell_atom list where the atomic electron density becomes smaller
   ! than this number. It requires that slater or coppens bases are defined for
   ! the asymmetric_cell_atom's, and it requires that the generation method be
   ! set to "for_hirshfeld_surface". It is primarily used for clusters generated
   ! from a fragment, which will correctly produce the Hirshfeld surface.
      type(CRYSTAL_TYPE), INOUT :: self
      call read_quantity_(stdin,self%atom_density_cutoff)
   end subroutine

   subroutine read_defragment(self)
   ! Read whether to defragment the cluster at the boundaries. If set TRUE, the
   ! cluster ends are defragmented, i.e. any atoms which are bonded at the ends
   ! of the cluster are included into the cluster.
      type(CRYSTAL_TYPE), INOUT :: self
      call read_(stdin,self%defragment)
   end subroutine

   subroutine read_generation_method(self)
   ! Read the add criteria, whether to add atoms by whole clusters within a
   ! certain radius of the starting fragment, or by individual atoms within a
   ! certain distance of the starting fragment.
      type(CRYSTAL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: generation_method
      call read_(stdin,generation_method)
      call set_generation_method_(self,generation_method)
   end subroutine

   subroutine read_fractional_fragment(self)
   ! Read in the fragment geometry, in fractional coordinates.
   ! NOTE: All fragment atom positions must correspond to the
   ! positions of actual atoms in the crystal lattice.
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: fragment_geometry
      VEC(REAL,:), allocatable :: tmp
      INT :: n
      call read_all_(stdin,tmp)
      ENSURE(mod(size(tmp),3)==0,"CRYSTAL:read_fractional_fragment ... # of elements not divisible by 3")
      n = size(tmp)/3
      call create_(fragment_geometry,3,n)
      fragment_geometry = reshape(tmp,[3,n])
      call set_fragment_geometry_(self,fragment_geometry)

      ! Clean
      call destroy_(fragment_geometry)
      call destroy_(tmp)
   end subroutine

   subroutine read_radius(self)
   ! Read the radius of the cluster. Usually, all atoms with this distance of
   ! any atom in the fragment_geometry will appear in the cluster.
      type(CRYSTAL_TYPE), INOUT :: self
      call read_quantity_(stdin,self%radius)
   end subroutine

   subroutine read_unit_cell_offset(self)
   ! Read the add criteria, whether to add atoms by whole clusters within a
   ! certain radius of the starting fragment, or by individual atoms within a
   ! certain distance of the starting fragment.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(REAL,3) :: offset
      call read_(stdin,offset)
      call set_unit_cell_offset_(self,offset)
   end subroutine


!  ===============
!  General methods
!  ===============

   PURE function n_unique_SF_k_pts(self) result (res)
   ! The number of unique k-points for an SF calculation
      type(CRYSTAL_TYPE), IN :: self
      INT :: res
   ENSURE(self%spacegroup%analyzed,"CRYSTAL:n_unique_SF_k_pts ... spacegroup not analyzed")
   ENSURE(allocated(self%xray_data),"CRYSTAL:n_unique_SF_k_pts ... no data")
   ENSURE(allocated(self%xray_data%reflections),"CRYSTAL:n_unique_SF_k_pts ... no reflection data")
      res = n_unique_SF_k_pts_(self%xray_data%reflections,self%spacegroup)
   end function

   PURE subroutine make_unique_X_SF_k_pts(self,k)
   ! Make the non-translational non-inverted symmetry generated
   ! reciprocal lattice vectors "k" required for structure factor
   ! calculations.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: k
   ENSURE(size(k,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_X_SF_k_pts ... k: wrong dim1")
   ENSURE(size(k,2)==3,"CRYSTAL:make_unique_X_SF_k_pts ... k: wrong dim2")
      call make_unique_SF_k_pts_(self%xray_data%reflections,k(:,1),k(:,2),k(:,3),self%spacegroup,self%unit_cell)
   end subroutine

   PURE subroutine make_unique_X_SF_Miller_indices(self,k)
   ! Make the non-translational non-inverted symmetry generated
   ! reciprocal Miller indices "k" required for structure factor
   ! calculations.
      type(CRYSTAL_TYPE), IN :: self
      MAT(INT,:,:), OUT :: k
   ENSURE(size(k,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_X_SF_Miller_indices ... k: wrong dim1")
   ENSURE(size(k,2)==3,"CRYSTAL:make_unique_X_SF_Miller_indices ... k: wrong dim2")
      call make_unique_SF_Miller_indices_(self%xray_data%reflections,k(:,1),k(:,2),k(:,3),self%spacegroup)
   end subroutine

   PURE function n_symop_generated_SF_k_pts(self) result (res)
   ! The number of symmetry generated k-points for an SF calculation
      type(CRYSTAL_TYPE), IN :: self
      INT :: res
   ENSURE(self%spacegroup%analyzed,"CRYSTAL:n_symop_generated_SF_k_pts ... spacegroup not analyzed")
   ENSURE(allocated(self%xray_data),"CRYSTAL:n_symop_generated_SF_k_pts ... no data")
   ENSURE(allocated(self%xray_data%reflections),"CRYSTAL:n_symop_generated_SF_k_pts ... no reflection data")
      res = self%spacegroup%n_seitz*size(self%xray_data%reflections)
   end function


!  ====================================================
!  Set fragment info arrays, unique operators, Z number
!  ====================================================

!  This is the initialisation or update

   PURE subroutine update_fragment_info(self,atom)
   ! This routine sets the ".fragment_atom" from the "atom" list.
   !
   ! The fragment is a finite portion/model of the crystal, perhaps
   ! one molecule or a cluster of atoms.
   !
   ! In addition a slew of information arrays about the fragment and
   ! crystal are made e.g. like the asymmetric unit atoms, the unit
   ! cell atoms; mapping arrays between these atoms; the stablizer
   ! symops ! for the fragment atoms; and the unit-cell connection
   ! table.
   !
   ! This is a suped-up set_defaults routine.
   !
   ! DETAILS:
   !
   ! A. The .asymmetric_unit_geometry is defined from the symmetry
   !    unique "atoms", if it is not already defined. If it is already
   !    defined the fragment atoms must be symmetry related to these
   !    asymmetric unit atom positions. The asymmetric unit is crucial
   ! B. The .asymmetric_unit_geometry is never destroyed unless
   !    explicitly requested.
   !    Not true anymore!!!! See my comment below! Lorraine
   ! 1. The .unit_cell_geometry is made from the asymmetric unit.
   !    Mapping arrays which relate the unit cell atoms to the
   !    asymmetric unit atoms are made,
   ! 2. Other mapping arrays are made which relate the fragment atoms
   !    to the unique asymmetric unit atoms, including sometimes the
   !    symops needed to generate them.
   ! 3. The .repetition factors needed for structure factor calcs on
   !    the fragment are made. The Z number is made.
   !    unchanged (the stabilizer) is made
   ! 4. The unit cell connection table is made.
   ! 5. The set of symmetry operations which leave the fragment
   !    unchanged (the stabilizer) is made
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      if (NOT self%spacegroup%analyzed) return

      ! Store the fragment atom list
      self%fragment_atom = atom
      self%destroy_fragment_atom = TRUE

      ! Get fragment geometry from the atom list
      self%fragment_geometry = coordinates_(self%fragment_atom,skip_dummies=TRUE)
      self%n_fragment_atoms = size(self%fragment_geometry,2)

      ! Fragment unit-cell offset
      call change_into_fractional_(self%unit_cell,self%fragment_geometry)
      self%fragment_offset = int(mean_column_vector_(self%fragment_geometry)*(ONE-TOL(8)))  ! shrink toward zero: FP-robust at cell boundary

      ! Make asymmetric_unit_geometry only if it does not already
      ! exist e.g. from having been ready previously from a CIF
      if (NOT allocated(self%asymmetric_unit_geometry)) then

!     ==============
!     The below was causing a crash in the tests.
!     We need to sort this out
!     --dylan
!     ==============
!     I am forcing the unit_cell_geometry to be updated here due to 
!     reordering of atoms when a cluster is created. In the example
!     used for debug, a cluster was created with defrag from the
!     original cif which still correcponded to the asymmetric unit,
!     however, the atom were placed in a different order (according to
!     fragments) which is fine but the refinement was doen using this
!     new ordering and when the cif is output after the refinement,
!     the assymetric unit cef was outputing the xyz for the correct
!     atoms, but using the covariance matrix from the fragment
!     geometry (new order of atoms), which means he was placing the
!     esds of on atom into the correct positions of a different atom.
!     The easiest way to solve this that I found was to remake the
!     assymetric unit geomatry from the unique atoms of the fragment
!     atoms. This is Lorraine.

         ! From fragment geometry make:
         !  .asymmetric_unit_geometry
         !  .unit_cell_geometry
         !  .asymmetric_unit_atoms
         !  .unit_cell_atom
         !  .asym_atom_for_unit_cell_atom.destroy
         !  .asym_symop_for_unit_cell_atom.destroy
         !  .asym_shift_for_unit_cell_atom.destroy
         call make_asymmetric_unit_(self,self%prune_asymmetric_unit)
         if (self%asymmetric_unit_source=="?") self%asymmetric_unit_source = "from-fragment"
      end if

      ! Sanity check
      ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:update_fragment_info ... no asymmetric_unit_atom list!")

      ! Relate fragment atoms to unit cell
      call make_frag_atom_for_cell_atom_(self)

      ! Make unique fragment atoms
      call make_unique_frag_atoms_(self)

      ! Make unique fragment atom-pairs
      call make_asym_symops_for_frag_atom_(self)

      ! Make the unit cell connection table
      call make_unit_cell_connections_(self)
      call make_uc_vdw_connections_(self)

      ! Z factors and zep. factors for SF calculations
      call make_repetition_factors_(self)
      call make_asym_atom_sym_factors_(self)
      call make_frag_atom_sym_factors_(self)
      self%Z = z_factor_(self)

      ! Symops which leave fragment_geometry unchanged
      call make_stabilizer_symops_(self)
      call make_stabilizer_xyz_matrices_(self)
      self%fragment_info_made = TRUE
   end subroutine

   PURE subroutine make_asymmetric_unit(self,prune_asymmetric_unit)
   ! Get the all the atom positions in the unit cell. Also make the
   ! symops allocated with each unit cell atom when it is made from
   ! the asymmetric unit atoms. If "prune_asymmetric_unit" is present
   ! and true, any non-asymmetric unit atoms are eliminated from the
   ! asymmetric unit. This should always be done.
   ! NOTE: .fragment_atom is in the cartesian axis system, but
   !       .fragment_geometry is in the fractional crystal system.
      type(CRYSTAL_TYPE), INOUT :: self
      BIN, optional, IN :: prune_asymmetric_unit
      VEC(REAL,3) :: pa,pos
      VEC(INT,3) :: offset
      MAT3(REAL,:,:,:), allocatable :: seitz
      MAT(REAL,:,:), allocatable :: asymmetric_unit_geometry
      INT :: n,a,b,s,col,i,u
      BIN :: prune,new,unique,frac,redundant,mess
      type(MAP_INT_INT_TYPE), allocatable :: pruned
      VEC(INT,:), allocatable :: list
      VEC(INT,:), allocatable :: cols
      REAL :: tol
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:make_asymmetric_unit ... no .fragment_atom")
   ENSURE(allocated(self%fragment_geometry),"CRYSTAL:make_asymmetric_unit ... no .fragment_geometry")
   ENSURE(NOT allocated(self%asymmetric_unit_geometry),"CRYSTAL:make_asymmetric_unit ... asymmetric unit exists")

      ! Set default options
      prune = TRUE
      if (present(prune_asymmetric_unit)) prune = prune_asymmetric_unit

      ! Clean up from before
      call destroy_(self%unit_cell_geometry)
      call destroy_(self%unique_unit_cell_atom)
      call destroy_(self%unique_uc_atom_offset)
      call destroy_(self%is_asym_unit_cell_atom)
      call destroy_(self%asym_atom_for_unit_cell_atom)
      call destroy_(self%asym_symop_for_unit_cell_atom)
      call destroy_(self%asym_shift_for_unit_cell_atom)

      ! List of indices of the asymmetric unit atoms to be made
      list = [(a,a = 1,self%n_fragment_atoms)]

      ! Initialise .asymmetric_unit_geometry as the whole fragment
      self%asymmetric_unit_geometry = self%fragment_geometry
      self%n_asymmetric_unit_atoms = self%n_fragment_atoms

      ! Copy asymmetric unit & shift it to the unit cell
      asymmetric_unit_geometry = self%asymmetric_unit_geometry
      call put_to_unit_cell_(self,asymmetric_unit_geometry)

      ! Append new atoms "n" to unit cell
      call create_(self%unit_cell_geometry,3,0)

      ! Tolerances to see if atom positions are the same
      ! --- getting this number wrong can stuff things
      tol = self%same_atom_tol

      ! Fractional atom occupancies?
      frac = has_disorder_(self%fragment_atom)
   ! WARN_IF(frac,"This structure has fractional occupancies and is disordered.")

      ! Initial loop counters
      call create_(pruned,0)
      redundant = FALSE
      n = 0  ! unit cell atom counter
      a = 0  ! asymmetric unit atom counter

      ! Loop: atoms "a" in
      ! the asymmetric unit
      do

         ! Next asymmetric unit atom
         a = a+1

         ! Loop over symops
         do s = 1,self%spacegroup%n_seitz

            ! Get the (unshifted) unit atom cell-offset
            pa = self%asymmetric_unit_geometry(:,a)
            call transform_position_(self,pa,1,to_unit_cell=TRUE,unit_cell_shift=offset)

            ! Transform (shifted) asym unit atom with symop "s"
            pa = self%asymmetric_unit_geometry(:,a)
            call transform_position_(self,pa,s,to_unit_cell=TRUE)

            ! Prune symmetry redundant atoms?
            if (prune) then
               do  ! remove *all* redundant atoms

                  ! Is symmetry-generated atom "pa" already in the asymmetric unit?
                  col = index_for_column_(asymmetric_unit_geometry(:,a+1:),pa,tol)
                  unique = col==0
                  if (unique) exit

                  ! Actual atom index to prune
                  b = col+a

                  ! If already there, are the atoms at the two sites
                  ! crystallographically distinct?
                  unique = are_distinct_(self%fragment_atom,list(a),list(b))
                  if (unique) exit

                  ! Store pruned pair
                  call append_pair_(pruned,list(a),list(b))

                  ! This is a symmetry-redundant structure
                  redundant = TRUE
                  frac = NOT equals_(self%fragment_atom(list(b))%site_occupancy,ONE)

                  ! Prune the atom
                  self%n_asymmetric_unit_atoms = self%n_asymmetric_unit_atoms-1
                  call prune_column_(self%asymmetric_unit_geometry,b)
                  call prune_column_(asymmetric_unit_geometry,b)
                  call prune_element_(list,b)
               end do
            end if

            ! Is generated position "pa" new in the unit cell?
            cols = indices_for_column_(self%unit_cell_geometry,pa,tol)
            new = size(cols)==0

            ! If old, is it distinct? E.g. different occupancy?
            if (NOT new) then
               do i = 1,size(cols)
                  b = self%asym_atom_for_unit_cell_atom(cols(i))
                  new = are_distinct_(self%fragment_atom,list(a),list(b))
                  if (NOT new) exit
               end do
            end if
            call destroy_(cols)
            if (NOT new) cycle

            ! New unit cell atom here ...
            n = n+1

            ! Append it's info ...
            call append_column_(self%unit_cell_geometry,pa)
            call append_(self%asym_atom_for_unit_cell_atom,a)
            call append_(self%asym_symop_for_unit_cell_atom,s)
            call append_column_(self%asym_shift_for_unit_cell_atom,offset)

            ! If s=1 it must be an asymmetric unit atom
            if (s==1) then
               call append_(self%unique_unit_cell_atom,n)
               call append_(self%unique_uc_atom_offset,offset)
               call append_(self%is_asym_unit_cell_atom,TRUE)
            else
               call append_(self%is_asym_unit_cell_atom,FALSE)
            end if
         end do

         ! Finished?
         if (a==self%n_asymmetric_unit_atoms) exit
      end do
      self%n_unit_cell_atoms = n

      ! Sanity check and print out
      mess = redundant AND prune
   ! WARN_IF(mess,"Symmetry-redundant atoms were pruned.")

      ! Die if pruned a symmetry-redundant fractional atom.
      if (mess AND frac) then

      ! WARN("Symmetry-redundant pruned atom had a fractional occupancy!")

      ! stdout.flush
      ! stdout.text("Fragment atom list:")
      ! .fragment_atom.put_coordinates

      ! stdout.flush
      ! stdout.text("Parent and pruned fragment atoms:")
      ! stdout.flush
      ! do i = 1,pruned.n_keys
      !    stdout.put(pruned.keys(i))
      !    stdout.put(pruned.values(i))
      !    stdout.flush
      ! end
      ! WARN("Symmetry-redundant pruned atom had a fractional occupancy!")
      end if

      ! Die if no unit cell
   ! ENSURE(.unit_cell_geometry.allocated,"The unit_cell_geometry was not created!")

      ! >> Make the asymmetric unit atoms <<
      ! NOTE: everything in cartesian system.
      self%asymmetric_unit_atom = self%fragment_atom(list)

      ! >> Make the unit cell atoms <<
      call destroy_(self%unit_cell_atom)
      call create_(self%unit_cell_atom,self%n_unit_cell_atoms)
      seitz = xyz_seitz_matrices_(self)
      do u = 1,self%n_unit_cell_atoms
         a = self%asym_atom_for_unit_cell_atom(u)
         s = self%asym_symop_for_unit_cell_atom(u)

         ! Copy the parent asymmetric atom
         self%unit_cell_atom(u) = self%asymmetric_unit_atom(a)

         ! Transform the pADPs (which must be in cartesians)
         call transform_pADP_vector_with_(self%unit_cell_atom(u),seitz(:,:,s))

         ! Reset position manually
         pos = matmul(self%unit_cell%direct_mx,self%unit_cell_geometry(:,u))
         call set_position_(self%unit_cell_atom(u),pos)
         call set_group_(self%unit_cell_atom(u),0)  ! Initialize (crystal).unit_cell_atom.group Kang added 2025 Mar. 12	 
      end do
      call destroy_(seitz)
      call set_axis_system_to_(self%unit_cell_atom,"cartesian")
      call update_(self%unit_cell_atom)  ! .unit_cell_atom.group is updated

      ! Clean
      call destroy_(asymmetric_unit_geometry)
      call destroy_(list)
      call destroy_(pruned)
   end subroutine

   PURE subroutine make_frag_atom_for_cell_atom(self)
   ! Arrays which relate the fragment atoms to the unit cell atoms.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(INT,3) :: sh
      VEC(REAL,3) :: pu,pa,pf,pf0
      INT :: u,f,a,s
      BIN :: found,ident
      REAL :: tol
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:make_frag_atom_for_cell_atom ... no .fragment_atom")
   ENSURE(allocated(self%fragment_geometry),"CRYSTAL:make_frag_atom_for_cell_atom ... no .fragment_geometry")
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:make_frag_atom_for_cell_atom ... no .unit_cell_geometry")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:make_frag_atom_for_cell_atom ... no .asymmetric_unit_atom")
   ENSURE(allocated(self%asym_atom_for_unit_cell_atom),"CRYSTAL:make_frag_atom_for_cell_atom ... no .asym_atom_for_cell_atom")
   ENSURE(allocated(self%asym_symop_for_unit_cell_atom),"CRYSTAL:make_frag_atom_for_cell_atom ... no .asym_symop_for_cell_atom")
   ENSURE(self%spacegroup%analyzed,"CRYSTAL:make_frag_atom_for_cell_atom ... no spacegroup info")

      ! Tolerances to see if atom positions are the same
      ! --- getting this number wrong can stuff things
      tol = self%same_atom_tol

      ! Create the arrays we want
      call destroy_(self%frag_atom_for_unit_cell_atom)
      call destroy_(self%unit_cell_atom_for_frag_atom)
      call destroy_(self%unit_cell_shft_for_frag_atom)
      call destroy_(self%frag_atom_for_asym_atom)
      call destroy_(self%asym_atom_for_frag_atom)
      call destroy_(self%asym_symop_for_frag_atom)
      call destroy_(self%asym_shift_for_frag_atom)
      call create_(self%frag_atom_for_unit_cell_atom,self%n_unit_cell_atoms)
      call create_(self%unit_cell_atom_for_frag_atom,self%n_fragment_atoms)
      call create_(self%unit_cell_shft_for_frag_atom,3,self%n_fragment_atoms)
      call create_(self%frag_atom_for_asym_atom,self%n_asymmetric_unit_atoms)
      call create_(self%asym_atom_for_frag_atom,self%n_fragment_atoms)
      call create_(self%asym_symop_for_frag_atom,self%n_fragment_atoms)
      call create_(self%asym_shift_for_frag_atom,3,self%n_fragment_atoms)

      ! Initialise
      self%frag_atom_for_unit_cell_atom = 0
      self%unit_cell_atom_for_frag_atom = 0
      self%unit_cell_shft_for_frag_atom = 0
      self%frag_atom_for_asym_atom = 0
      self%asym_atom_for_frag_atom = 0

      ! Loop over fragment atoms
      do f = 1,self%n_fragment_atoms

         ! Shift fragment atom to unit cell
         pf0 = self%fragment_geometry(:,f)
         pf = pf0
         call put_to_unit_cell_(self,pf,shift=sh)
         found = FALSE

         ! Loop unit cell atoms
         do u = 1,self%n_unit_cell_atoms

            ! Asymmetric atom a for unit cell atom u
            a = self%asym_atom_for_unit_cell_atom(u)

            ! Unit cell position
            pu = self%unit_cell_geometry(:,u)

            ! Is unit cell atom u the fragment atom f?
            found = same_as_(pu,pf,tol)
            ident = is_xtal_identical_kind_to_(self%fragment_atom(f),self%asymmetric_unit_atom(a))
            found = found AND ident

            ! Cycle if not the same
            if (NOT found) cycle

            ! Relate unit cell atom and fragment atom
            ! These are identical by translation.
            self%frag_atom_for_unit_cell_atom(u) = f
            self%unit_cell_atom_for_frag_atom(f) = u
            self%unit_cell_shft_for_frag_atom(:,f) = sh

            ! Relate asym unit atom and fragent atom
            ! Maybe >1 frag atom  matching the asym atom
            self%asym_atom_for_frag_atom(f) = a
            if (self%frag_atom_for_asym_atom(a)==0) self%frag_atom_for_asym_atom(a) = f

            ! Asym symop which can generate this frag atom
            s = self%asym_symop_for_unit_cell_atom(u)
            self%asym_symop_for_frag_atom(f) = s

            ! Get shift for asym atom -> frag atom
            pa = self%asymmetric_unit_geometry(:,a)
            pf = self%fragment_geometry(:,f)
            call transform_position_(self,pa,s,ignore_glide=TRUE)
            self%asym_shift_for_frag_atom(:,f) = pf-pa
            exit
         end do
         ENSURE(found,"CRYSTAL:make_frag_atom_for_cell_atom ... fragment atom "//trim(to_str_(f))//" was not found in the unit cell")
      end do

      ! Relate unit cell atom to parent frag atom
      ! These are identical by a crystal symop.
      call destroy_(self%frag_parent_for_cell_atom)
      call create_(self%frag_parent_for_cell_atom,self%n_unit_cell_atoms)
      do u = 1,self%n_unit_cell_atoms
         a = self%asym_atom_for_unit_cell_atom(u)
         f = self%frag_atom_for_asym_atom(a)
         self%frag_parent_for_cell_atom(u) = f
      end do
   end subroutine

   PURE subroutine make_unique_frag_atoms(self)
   ! Make the unique fragment atoms (these are those corresponding to
   ! the asymmetric unit atoms) and also make symops which relate the
   ! non-unique to the unique.
      type(CRYSTAL_TYPE), INOUT :: self
      INT :: f,a,m,u,s,t
      BIN :: unique
   ENSURE(allocated(self%asym_atom_for_frag_atom),"CRYSTAL:make_unique_frag_atoms ... no .asym_atom_for_frag_atom")
   ENSURE(allocated(self%unit_cell_atom_for_frag_atom),"CRYSTAL:make_unique_frag_atoms ... no .unit_cell_atom_for_frag_atom")
   ENSURE(allocated(self%asym_symop_for_unit_cell_atom),"CRYSTAL:make_unique_frag_atoms ... no .asym_symop_for_cell_atom")

      ! Create the arrays we want
      call destroy_(self%unique_frag_atom)
      call destroy_(self%unique_index_for_frag_atom)
      call destroy_(self%unique_atom_for_frag_atom)
      call destroy_(self%unique_symop_for_frag_atom)
      call create_(self%unique_index_for_frag_atom,self%n_fragment_atoms)
      call create_(self%unique_atom_for_frag_atom,self%n_fragment_atoms)
      call create_(self%unique_symop_for_frag_atom,self%n_fragment_atoms)
      self%n_unique_frag_atoms = 0

      ! Loop over the fragment atoms
      do f = 1,self%n_fragment_atoms

         ! Asym atom for this fragment atom
         a = self%asym_atom_for_frag_atom(f)

         ! Asym symop which can generate this frag atom
         u = self%unit_cell_atom_for_frag_atom(f)
         s = self%asym_symop_for_unit_cell_atom(u)

         ! Does this asym atom "a" occur before?
         ! If not, its unique.
         m = index_of_value_(self%asym_atom_for_frag_atom(1:f-1),a)
         unique = m==0
         if (unique) then
            self%n_unique_frag_atoms = self%n_unique_frag_atoms+1
            self%unique_index_for_frag_atom(f) = self%n_unique_frag_atoms
            self%unique_atom_for_frag_atom(f) = f
            self%unique_symop_for_frag_atom(f) = 1  ! identity
            call append_(self%unique_frag_atom,f)  ! Indices of unique frag atoms
         else
            self%unique_index_for_frag_atom(f) = 0
            self%unique_atom_for_frag_atom(f) = m  ! 1 <= m <= f-1
            u = self%unit_cell_atom_for_frag_atom(m)
            t = self%asym_symop_for_unit_cell_atom(u)
            t = self%spacegroup%inverse_of_symops(t)  ! inverse of symop(t) maps m -> a
            t = self%spacegroup%product_of_symops(s,t)  ! s maps a -> f (except for trans)
            self%unique_symop_for_frag_atom(f) = t
         end if
      end do
   end subroutine

   PURE subroutine make_asym_symops_for_frag_atom(self)
   ! Make all symops which generate a fragment atom (up to
   ! translation equivalence) from a given asymmetric unit atom.
   ! These are only a given symop which does the mapping multiplied by
   ! the asym atoms stabilizer group, but it is convenient for
   ! symmetrisation.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(REAL,3) :: pa,pf
      INT :: s,f,a
      BIN :: same
      REAL :: tol
   ENSURE(allocated(self%asym_atom_for_frag_atom),"CRYSTAL:make_asym_symops_for_frag_atom ... no .asym_atom_for_frag_atom")
   ENSURE(allocated(self%asymmetric_unit_geometry),"CRYSTAL:make_asym_symops_for_frag_atom ... no .asym_atom_for_frag_atom")
      tol = self%same_atom_tol

      ! Create
      call destroy_(self%asym_symops_for_frag_atom)
      call create_(self%asym_symops_for_frag_atom,self%n_fragment_atoms)

      ! Loop over symops
      do s = 1,self%spacegroup%n_seitz

         ! Loop over the fragment atoms
         do f = 1,self%n_fragment_atoms

            ! Asym atom for this fragment atom
            a = self%asym_atom_for_frag_atom(f)

            ! Put asym atom into unit cell.
            pa = self%asymmetric_unit_geometry(:,a)
            call transform_position_(self,pa,1,to_unit_cell=TRUE)

            ! Transform asym unit atom with symop "s"
            pa = self%asymmetric_unit_geometry(:,a)
            call transform_position_(self,pa,s,to_unit_cell=TRUE)

            ! Put fragment atom to unit cell
            pf = self%fragment_geometry(:,f)
            call put_to_unit_cell_(self,pf)

            ! If same add the symop to the list
            same = same_as_(pa,pf,tol)
            if (same) then
               call append_(self%asym_symops_for_frag_atom(f)%element,s)
            end if
         end do
      end do
   end subroutine

   PURE subroutine make_unit_cell_connections(self)
   ! From the .asymmetric_unit_atom list make the .unit_cell_connection
   ! table and the .unit_cell_mol_for_atom list which has the list of
   ! distinct molecules in the unit cell i.e. those groups of unit
   ! cell atoms which are connected to each other (ignoring/accounting
   ! for translational symmetry). NOTE: only connections to
   ! next-neighboring unit cells are considered regardless of the size
   ! of covalent radii.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable :: new
      VEC(INT,:), allocatable :: unit_cell_mol_for_atom,unique
      MAT(REAL,:,:), allocatable :: geom0,geom1
      MAT(REAL,3,3) :: rcm
      VEC(REAL,3) :: pos_i,pos_j,offset
      INT :: i,j,par_i,par_j,h1,h2,h3,mi,mj
      type(ATOM_TYPE) :: atom_i,atom_j
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:make_unit_cell_connections ... no unit_cell in the crystal")
   ENSURE(allocated(self%frag_parent_for_cell_atom),"CRYSTAL:make_unit_cell_connections ... no frag_parent_for_cell_atom")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:make_unit_cell_connections ... no asymmetric_unit_atom list")

      ! Create the arrays we want
      call destroy_(self%unit_cell_connection)
      call destroy_(self%unit_cell_mol_for_atom)
      call create_(self%unit_cell_connection,self%n_unit_cell_atoms,0)
      call create_(self%unit_cell_mol_for_atom,self%n_unit_cell_atoms)

      ! Set worst case scenario: all atoms on different molecules
      call create_(unit_cell_mol_for_atom,self%n_unit_cell_atoms)
      do i = 1,self%n_unit_cell_atoms
         unit_cell_mol_for_atom(i) = i
      end do

      ! Define a new connected atom: its 3-element shift and index
      ! This is just an expanded version of CLUSTER_UA_CODE
      call create_(new,1)
      call create_(new(1)%element,4)

      ! Get unit cell cartesian geometry
      rcm = self%unit_cell%direct_mx
      call create_(geom0,3,self%n_unit_cell_atoms)
      geom0 = matmul(rcm,self%unit_cell_geometry)

      ! First make connections in the unit cell
      ! and the distinct mols
      h1 = 0
      h2 = 0
      h3 = 0

         ! Loop over the (0,0,0) unit cell atoms i
      do i = 1,self%n_unit_cell_atoms

            ! Set atom_i
         pos_i = geom0(:,i)
         par_i = self%asym_atom_for_unit_cell_atom(i)
         call set_atomic_number_(atom_i,self%asymmetric_unit_atom(par_i)%atomic_number)
         call set_position_(atom_i,pos_i)

            ! Atom i belongs to mol "mi"
         mi = unit_cell_mol_for_atom(i)

            ! Loop over the (0,0,0) unit cell atoms j
         do j = i+1,self%n_unit_cell_atoms

               ! Gross test for closeness
            pos_j = geom0(:,j)
            if (abs(pos_j(1)-pos_i(1))>SEVEN) cycle
            if (abs(pos_j(2)-pos_i(2))>SEVEN) cycle
            if (abs(pos_j(3)-pos_i(3))>SEVEN) cycle

               ! It could be close: Set atom_j
            par_j = self%asym_atom_for_unit_cell_atom(j)
            call set_atomic_number_(atom_j,self%asymmetric_unit_atom(par_j)%atomic_number)
            call set_position_(atom_j,pos_j)

               ! Cycle if not bonded ...
            if (NOT is_bonded_to_(atom_i,atom_j)) cycle

               ! Append the bonded atom and the cell it is in ...
            new(1)%element(:) = [h1,h2,h3,j]; call append_(self%unit_cell_connection(i)%element,new)
            new(1)%element(4) = i; call append_(self%unit_cell_connection(j)%element,new)

               ! Atom j belongs to mol "mj"
            mj = unit_cell_mol_for_atom(j)

               ! Replace all previous by the lowest mol
            if (mj<mi) then
               where (unit_cell_mol_for_atom==mi) unit_cell_mol_for_atom = mj
               mi = mj  ! replace mol for atom i too
            else if (mj>mi) then
               where (unit_cell_mol_for_atom==mj) unit_cell_mol_for_atom = mi
            end if
         end do
      end do

      ! <<< Now make connections to neighbouring cells
      call create_(geom1,3,self%n_unit_cell_atoms)
      do h1 = -1,1
         do h2 = -1,1
            do h3 = -1,1
               if (h1==0 AND h2==0 AND h3==0) cycle

         ! Get offset unit cell positions
               offset = matmul(rcm,[h1,h2,h3])
               geom1 = geom0+spread(offset,2,self%n_unit_cell_atoms)

         ! Loop over (0,0,0) unit cell atoms
               do i = 1,self%n_unit_cell_atoms

            ! Set atom_i
                  pos_i = geom0(:,i)
                  par_i = self%asym_atom_for_unit_cell_atom(i)
                  call set_atomic_number_(atom_i,self%asymmetric_unit_atom(par_i)%atomic_number)
                  call set_position_(atom_i,pos_i)

            ! Atom i belongs to mol "mi"
                  mi = unit_cell_mol_for_atom(i)  ! Atom i belongs to mol "mi"

            ! Loop over (h1,h2,h3) unit cell atoms
                  do j = 1,self%n_unit_cell_atoms

               ! Gross test for closeness
                     pos_j = geom1(:,j)
                     if (abs(pos_j(1)-pos_i(1))>TEN) cycle
                     if (abs(pos_j(2)-pos_i(2))>TEN) cycle
                     if (abs(pos_j(3)-pos_i(3))>TEN) cycle

               ! It could be close: Set atom_j
                     par_j = self%asym_atom_for_unit_cell_atom(j)
                     call set_atomic_number_(atom_j,self%asymmetric_unit_atom(par_j)%atomic_number)
                     call set_position_(atom_j,pos_j)

               ! Cycle if not bonded ...
                     if (NOT is_bonded_to_(atom_i,atom_j)) cycle
                     new(1)%element(:) = [h1,h2,h3,j]
                     call append_(self%unit_cell_connection(i)%element,new)

               ! Atom j belongs to mol "mj"
                     mj = unit_cell_mol_for_atom(j)
                     if (mj<mi) then  ! replace all previous by the lowest mol
                        where (unit_cell_mol_for_atom==mi) unit_cell_mol_for_atom = mj
                        mi = mj  ! replace mol for atom i too
                     else if (mj>mi) then
                        where (unit_cell_mol_for_atom==mj) unit_cell_mol_for_atom = mi
                     end if
                  end do
               end do  ! unit cell atoms
            end do
         end do
      end do

      ! Clean
      call destroy_(geom1)
      call destroy_(geom0)
      call destroy_(new)

      ! Now make the .unit_cell_mol_for_atom list consecutive
      self%unit_cell_mol_for_atom = unit_cell_mol_for_atom
      call to_unique_elements_of_(unique,unit_cell_mol_for_atom)
      call sort_(unique)
      self%n_unit_cell_mols = size(unique)
      do mi = 1,self%n_unit_cell_mols
         mj = unique(mi)
         if (mj==mi) cycle
         where (unit_cell_mol_for_atom==mj) self%unit_cell_mol_for_atom = mi
      end do
      call destroy_(unique)
      call destroy_(unit_cell_mol_for_atom)
   end subroutine

   PURE subroutine make_uc_vdw_connections(self)
   ! From the .asymmetric_unit_atom list make the .uc_vdw_connection
   ! table. Essentially aame as above with different bond criteria.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable :: vdw
      MAT(REAL,:,:), allocatable :: geom0,geom1
      MAT(REAL,3,3) :: rcm
      VEC(REAL,3) :: pos_i,pos_j,offset
      INT :: i,j,par_i,par_j,h1,h2,h3
      type(ATOM_TYPE) :: atom_i,atom_j
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:make_uc_vdw_connections ... no unit_cell in the crystal")
   ENSURE(allocated(self%frag_parent_for_cell_atom),"CRYSTAL:make_uc_vdw_connections ... no frag_parent_for_cell_atom")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:make_uc_vdw_connections ... no asymmetric_unit_atom list")
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:make_uc_vdw_connections ... no unit_cell_mol_for_atom list")

      ! Create the arrays we want
      call destroy_(self%uc_vdw_connection)
      call create_(self%uc_vdw_connection,self%n_unit_cell_atoms,0)

      ! Define a new connected atom: its index and 3-element shift
      call create_(vdw,1)
      call create_(vdw(1)%element,4)

      ! Get unit cell cartesian geometry
      rcm = self%unit_cell%direct_mx
      call create_(geom0,3,self%n_unit_cell_atoms)
      geom0 = matmul(rcm,self%unit_cell_geometry)

      ! First make connections in the unit cell
      ! and the distinct mols
      h1 = 0
      h2 = 0
      h3 = 0

         ! Loop over the (0,0,0) unit cell atoms i
      do i = 1,self%n_unit_cell_atoms

            ! Set atom_i
         pos_i = geom0(:,i)
         par_i = self%asym_atom_for_unit_cell_atom(i)
         call set_atomic_number_(atom_i,self%asymmetric_unit_atom(par_i)%atomic_number)
         call set_position_(atom_i,pos_i)

            ! Loop over the (0,0,0) unit cell atoms j
         do j = i+1,self%n_unit_cell_atoms

               ! Gross test for closeness
            pos_j = geom0(:,j)
            if (abs(pos_j(1)-pos_i(1))>SEVEN) cycle
            if (abs(pos_j(2)-pos_i(2))>SEVEN) cycle
            if (abs(pos_j(3)-pos_i(3))>SEVEN) cycle

               ! It could be close: Set atom_j
            par_j = self%asym_atom_for_unit_cell_atom(j)
            call set_atomic_number_(atom_j,self%asymmetric_unit_atom(par_j)%atomic_number)
            call set_position_(atom_j,pos_j)
            if (self%unit_cell_mol_for_atom(i)==self%unit_cell_mol_for_atom(j)) cycle
            if (NOT is_vdw_bonded_to_(atom_i,atom_j)) cycle

               ! Append the bonded atom and the cell it is in ...
            vdw(1)%element(:) = [h1,h2,h3,j]; call append_(self%uc_vdw_connection(i)%element,vdw)
            vdw(1)%element(4) = i; call append_(self%uc_vdw_connection(j)%element,vdw)
         end do
      end do

      ! <<< Now make connections to neighbouring cells
      call create_(geom1,3,self%n_unit_cell_atoms)
      do h1 = -1,1
         do h2 = -1,1
            do h3 = -1,1
               if (h1==0 AND h2==0 AND h3==0) cycle

         ! Get offset unit cell positions
               offset = matmul(rcm,[h1,h2,h3])
               geom1 = geom0+spread(offset,2,self%n_unit_cell_atoms)

         ! Loop over (0,0,0) unit cell atoms
               do i = 1,self%n_unit_cell_atoms

            ! Set atom_i
                  pos_i = geom0(:,i)
                  par_i = self%asym_atom_for_unit_cell_atom(i)
                  call set_atomic_number_(atom_i,self%asymmetric_unit_atom(par_i)%atomic_number)
                  call set_position_(atom_i,pos_i)

            ! Loop over (h1,h2,h3) unit cell atoms
                  do j = 1,self%n_unit_cell_atoms

               ! Gross test for closeness
                     pos_j = geom1(:,j)
                     if (abs(pos_j(1)-pos_i(1))>TEN) cycle
                     if (abs(pos_j(2)-pos_i(2))>TEN) cycle
                     if (abs(pos_j(3)-pos_i(3))>TEN) cycle

               ! It could be close: Set atom_j
                     par_j = self%asym_atom_for_unit_cell_atom(j)
                     call set_atomic_number_(atom_j,self%asymmetric_unit_atom(par_j)%atomic_number)
                     call set_position_(atom_j,pos_j)

               ! Cycle if not bonded ...
                     if (self%unit_cell_mol_for_atom(i)==self%unit_cell_mol_for_atom(j)) cycle
                     if (NOT is_vdw_bonded_to_(atom_i,atom_j)) cycle
                     vdw(1)%element(:) = [h1,h2,h3,j]
                     call append_(self%uc_vdw_connection(i)%element,vdw)
                  end do
               end do
            end do
         end do
      end do

      ! Clean
      call destroy_(geom1)
      call destroy_(geom0)
      call destroy_(vdw)
   end subroutine

   PURE function z_factor(self) result (res)
   ! The Z crystallographic factor for ".fragment_geometry", defined
   ! as the ratio of the number of electrons in the unit cell on the
   ! number of electrons in the fragment.
      type(CRYSTAL_TYPE), IN :: self
      REAL :: res
      REAL :: ue,fe
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:z_factor ... no asymmetric_unit_atom")
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:z_factor ... no fragment_atom")
   ENSURE(allocated(self%asym_atom_for_unit_cell_atom),"CRYSTAL:z_factor ... no asym_atom_for_unit_cell_atom")

      ! Get # of electrons in unit cell
      !ue = .asymmetric_unit_atom(.asym_atom_for_unit_cell_atom).no_of_electrons
      ue = no_of_electrons_(self%asymmetric_unit_atom,self%asym_atom_for_unit_cell_atom)

      ! Get # of electrons in fragment
      fe = no_of_electrons_(self%fragment_atom)

      ! Z factor is the ratio
      res = ue/fe
   end function

   PURE function no_of_unit_cell_electrons(self) result (res)
   ! The totl number of electrons in the unit cell aka F_000.
      type(CRYSTAL_TYPE), IN :: self
      REAL :: res
      INT :: a,n,r
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:no_of_unit_cell_electrons ... no asymmetric_unit_atom")
   ENSURE(allocated(self%asym_atom_sym_factor),"CRYSTAL:no_of_unit_cell_electrons ... no .asym_atom_sym_factor")
      n = self%spacegroup%n_seitz
      res = 0
      do a = 1,self%n_asymmetric_unit_atoms
         r = int(self%asym_atom_sym_factor(a))
         res = res+self%asymmetric_unit_atom(a)%atomic_number*n/r
      end do
   end function


!   set_frag_atom_from_asym_atom_ADPs :: PURE
!   ! Make a new atom list for the cluster. Note that the .asymmetric_unit_atom's
!   ! must have ADP (thermal) tensors in the cartesian axis system; this will normally
!   ! be the case for Tonto's internal representation ....
!   ! NOTE: basis sets are pointer copied!
!      self :: IN
!      atom :: VEC{ATOM}, OUT
!
!   ENSURE(.n_atoms>0,"no atoms in cluster")
!   ENSURE(atom.dim==.n_atoms,"wrong size, atomsr")
!   ENSURE(.geometry.allocated,"no geometry")
!   ENSURE(.occupation_list.allocated,"no occupation_list")
!   ENSURE(.fragment_info_made,"no crystal fragment info")
!   ENSURE(.asymmetric_unit_atom.allocated, "no asymmetric_unit_atom data")
!   ENSURE(.asymmetric_unit_atom.has_cartesian_axes, "asymmetric_unit_atom not in cartesian axes")
!   ENSURE(.molecule_for_atom.allocated, "no molecule_for_atom info")
!
!      ulist :: VEC{INT}@
!      seitz :: MAT3{REAL}@
!      a,u,p,s :: INT
!
!      ! Seitz matrices (cartesian!)
!      seitz = .xyz_seitz_matrices
!
!      ! Loop over cluster atoms "a"
!      do f = 1,.n_fragment_atoms
!
!         ! Extract unit cell atom, asymmetric parent and symop
!         u = ulist(f)
!         u = .unit_cell_atom_for_frag_atom(f)
!         p = .asym_atom_for_unit_cell_atom(u)
!         s = .asym_symop_for_unit_cell_atom(u)
!
!         ! Copy the parent atom
!         ! NOTE: .basis is a ptr copy
!         .fragment_atom(f) = .asymmetric_unit_atom(p)
!
!         ! Transform the pADPs (which must be in cartesians)
!         .fragment_atom(f).transform_pADP_vector_with(seitz(:,:,s))
!
!         ! ?
!         ! Change pos to cartesian
!         ! Errors (if any) are changed straightforwardly
!         atom.change_axis_system_to("cartesian",.unit_cell)
!
!      end
!
!      ! Free memory
!      seitz.destroy
!
!   end

!  ==================
!  Repetition factors
!  ==================

   PURE subroutine make_repetition_factors(self)
   ! The number of times *any* fragment atom is mapped onto a
   ! particular fragment atom with index "a" under *all* spacegroup
   ! symmetry group, is the ".repetition_factor(a)".
   ! It is used to correct structure factors for fragment geometries
   ! which are "oversampled" relative to the asymmetric cell geometry.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(REAL,3) :: pa,pb
      INT :: a,b,n,n_same
      BIN :: same
      REAL :: tol
   ENSURE(allocated(self%fragment_geometry),"CRYSTAL:make_repetition_factors ... no fragment_geometry!")
      if (self%use_manual_repetition_factors) then
         ENSURE(allocated(self%repetition_factor),"CRYSTAL:make_repetition_factors ... no repetiton_factors entered!")
      else
         tol = self%same_atom_tol
         call destroy_(self%repetition_factor)
         call create_(self%repetition_factor,self%n_fragment_atoms)
         do a = 1,self%n_fragment_atoms
            pa = self%fragment_geometry(:,a)
            call put_to_unit_cell_(self,pa)
            n_same = 0
            do b = 1,self%n_fragment_atoms
               do n = 1,self%spacegroup%n_seitz
                  pb = self%fragment_geometry(:,b)
                  call transform_position_(self,pb,n,to_unit_cell=TRUE)
                  same = same_as_(pa,pb,tol)
                  if (same) n_same = n_same+1
               end do
            end do
            self%repetition_factor(a) = n_same
         end do
      end if
   end subroutine

   PURE subroutine make_asym_atom_sym_factors(self)
   ! The number of times an asymmetric cell atom with index "a" is
   ! mapped into itself under the symmetry group is the
   ! ".asym_atom_sym_factor(a)". Also record the stabilizing
   ! operations in ".asym_atom_stabilizer(a)[:]". These are used in
   ! atomic-type structure factor calculations based on the asymmetric
   ! cell geometry.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(REAL,3) :: pa,pn
      INT :: a,n,n_same
      BIN :: same
      REAL :: tol
   ENSURE(allocated(self%fragment_geometry),"CRYSTAL:make_asym_atom_sym_factors ... no fragment_geometry!")
      tol = self%same_atom_tol
      call destroy_(self%asym_atom_sym_factor)
      call destroy_(self%asym_atom_stabilizer)
      call create_(self%asym_atom_sym_factor,self%n_asymmetric_unit_atoms)
      call create_(self%asym_atom_stabilizer,self%n_asymmetric_unit_atoms)
      self%n_stabilizer_max = 0
      do a = 1,self%n_asymmetric_unit_atoms
         pa = self%asymmetric_unit_geometry(:,a)
         call put_to_unit_cell_(self,pa)
         n_same = 0
         do n = 1,self%spacegroup%n_seitz
            pn = pa
            call transform_position_(self,pn,n,to_unit_cell=TRUE)
            same = same_as_(pa,pn,tol)
            if (NOT same) cycle
            n_same = n_same+1
            call append_(self%asym_atom_stabilizer(a)%element,n)
            self%n_stabilizer_max = max(n_same,self%n_stabilizer_max)
         end do
         self%asym_atom_sym_factor(a) = n_same
      end do
   end subroutine

   PURE subroutine make_frag_atom_sym_factors(self)
   ! The number of times a fragment atom with index "f" is mapped into
   ! itself under the symmetry group is ".frag_atom_sym_factor(a)". 
   ! The stabilizing operations in ".frag_atom_stabilizer(a)[:]". 
   ! These are used in atomic-type structure factor calculations based
   ! on the fragment geometry.
      type(CRYSTAL_TYPE), INOUT :: self
      INT :: f,a
   ENSURE(allocated(self%fragment_geometry),"CRYSTAL:make_frag_atom_sym_factors ... no fragment_geometry!")
   ENSURE(allocated(self%asym_atom_sym_factor),"CRYSTAL:make_frag_atom_sym_factors ... no asym_atom_sym_factor!")
   ENSURE(allocated(self%asym_atom_stabilizer),"CRYSTAL:make_frag_atom_sym_factors ... no asym_atom_sym_factor!")
      call destroy_(self%frag_atom_sym_factor)
      call destroy_(self%frag_atom_stabilizer)
      call create_(self%frag_atom_sym_factor,self%n_fragment_atoms)
      call create_(self%frag_atom_stabilizer,self%n_fragment_atoms)
      do f = 1,self%n_fragment_atoms
         a = self%asym_atom_for_frag_atom(f)
         self%frag_atom_sym_factor(f) = self%asym_atom_sym_factor(a)
         self%frag_atom_stabilizer(f)%element = self%asym_atom_stabilizer(a)%element
      end do
   end subroutine


!  =============================
!  Reset hydrogen atom positions
!  =============================

   PURE subroutine reset_H_bond_lengths_in(self,atom,H_bond_length,reset)
   ! Resets the coordinates of the single-bonded H "atoms" so that
   ! their bond lengths have those in the "H_bond_length" array.
   ! Negative values mean no changes. If present "reset" is set
   ! TRUE if at least on the H atom positions was reset.
   ! *Multiple bonded H atom (from unit cell conn. table) *ignored*
   ! WARNING: this routine destroys all the fragment data.
      type(CRYSTAL_TYPE), allocatable, INOUT :: self
      VEC(type(ATOM_TYPE),:), INOUT :: atom
      VEC(REAL,:), allocatable, IN :: H_bond_length
      BIN, optional, OUT :: reset
      INT :: lb,ub,ua,fh,fa,uh,Za,n
      VEC(REAL,3) :: hh,ph,pa,sh,ch,ca,dd,d
      MAT(REAL,3,3) :: rcm,inv
      BIN :: done
   ENSURE(allocated(H_bond_length),"CRYSTAL:reset_H_bond_lengths_in ... no H_bond_length array")
      done = FALSE
      if (present(reset)) reset = done
      if (all(H_bond_length<ZERO)) return

      ! Destroy all the fragment and asymmetric unit data
      call destroy_fragment_data_(self)
      call destroy_asymmetric_unit_(self)

      ! Make unit cell connection table
      call update_fragment_info_(self,atom)

      ! Elements whose bond lengths to change
      lb = lbound(H_bond_length,1)
      ub = ubound(H_bond_length,1)

      ! Cell matrices
      rcm = self%unit_cell%direct_mx
      inv = self%unit_cell%inverse_mx

      ! Loop fragment atom positions
      do fh = 1,self%n_fragment_atoms

         ! If fragment atom is not H atom, cycle ...
         if (atom(fh)%atomic_number/=1) cycle

         ! The H atom  must have only one bond
         uh = self%unit_cell_atom_for_frag_atom(fh)
         if (size(self%unit_cell_connection(uh)%element)>1) then
            n = size(self%unit_cell_connection(uh)%element)
         ! WARN("Hydrogen #"//fh.to_str.trim//" is bonded to "//n.to_str.trim//" atoms")
            cycle
         end if

         ! Which frag atom is the H connected to?
         ua = self%unit_cell_connection(uh)%element(1)%element(4)
         fa = self%frag_atom_for_unit_cell_atom(ua)
         if (fa==0) cycle

         ! If it isn't one of those to be reset, cycle
         Za = atom(fa)%atomic_number
         if (Za<lb OR Za>ub) cycle
         if (H_bond_length(Za)<ZERO) cycle
         done = TRUE

         ! Get the H atom pos relative to A
         hh = self%fragment_geometry(:,fh)
         pa = self%fragment_geometry(:,fa)
         dd = hh-pa
         d = mod(dd,[ONE,ONE,ONE])
         where (d>HALF) d = d-ONE
         where (d<-HALF) d = d+ONE
         ph = pa+d
         sh = ph-hh

         ! Get new cartesian H atom position
         ch = matmul(rcm,ph)
         ca = matmul(rcm,pa)
         dd = ch-ca
         call normalize_(dd)
         ch = ca+H_bond_length(Za)*dd

         ! Put H atom back
         ph = matmul(inv,ch)-sh
         ch = matmul(rcm,ph)

         ! Reset the H atom position
         call set_position_(atom(fh),ch)
      end do
      if (present(reset)) reset = done

      ! Clean up
      call destroy_fragment_data_(self)
   end subroutine


!  ================
!  Position changes
!  ================

   ! The next three routines should be in SPACEGROUP

   PURE subroutine transform_geometry(self,g,op,translate,ignore_glide,to_unit_cell)
   ! Transform the positions "g" in fractional coordinates with the
   ! Seitz operator with index "op". If present, "translate" will be
   ! added to the transformed position. If present and TRUE, "ignore_glide"
   ! will not add the glide vector part of the Seitz operator.
   ! If present and TRUE, "to_unit_cell" will translate the fractional
   ! coordinates into the (1,1,1) unit cell.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: g
      INT, IN :: op
      VEC(REAL,3), optional, IN :: translate
      BIN, optional, IN :: ignore_glide
      BIN, optional, IN :: to_unit_cell
      INT :: n
   ENSURE(size(g,1)==3,"CRYSTAL:transform_geometry ... incorrect size for array g")
      do n = 1,size(g,2)
         call transform_position_(self,g(:,n),op,translate,ignore_glide,to_unit_cell)
      end do
   end subroutine

   PURE subroutine transform_position(self,p,op,translate,ignore_glide,to_unit_cell,unit_cell_shift)
   ! Transform the position "p" in fractional coordinates with the
   ! Seitz operator with index "op". If present, "translate" will be
   ! added to the transformed position. If present and TRUE,
   ! "ignore_glide" will not add the glide vector part of the Seitz
   ! operator.  If present and TRUE, "to_unit_cell" will translate the
   ! fractional coordinates into the (0:1,0:1,0:1) unit cell, and
   ! "unit_cell_shift" is set to the translation vector required to do
   ! this.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,3), INOUT :: p
      INT, IN :: op
      VEC(REAL,3), optional, IN :: translate
      BIN, optional, IN :: ignore_glide
      BIN, optional, IN :: to_unit_cell
      VEC(INT,3), optional, OUT :: unit_cell_shift
      BIN :: ignore,to_cell
   ENSURE(allocated(self%spacegroup%seitz),"CRYSTAL:transform_position ... Seitz matrices not initialized")
   ENSURE(op>0,"CRYSTAL:transform_position ... operator index out of bounds")
   ENSURE(op<=self%spacegroup%n_seitz,"CRYSTAL:transform_position ... operator index out of bounds")
      ignore = FALSE
      if (present(ignore_glide)) ignore = ignore_glide
      if (ignore) then
         p = matmul(self%spacegroup%seitz(1:3,1:3,op),p)
      else
         p = matmul(self%spacegroup%seitz(1:3,1:3,op),p)+self%spacegroup%seitz(1:3,4,op)
      end if
      if (present(translate)) p = p+translate
      to_cell = FALSE
      if (present(to_unit_cell)) to_cell = to_unit_cell
      if (to_cell) call put_to_unit_cell_(self,p,unit_cell_shift)
   end subroutine

   PURE subroutine transform_xyz_position(self,p,op,translate,ignore_glide,to_unit_cell,unit_cell_shift)
   ! Transform the *cartesian* position "p" with the Seitz operator
   ! with index "op". If present, "translate" will be added to the
   ! transformed position. If present and TRUE, "ignore_glide" will
   ! not add the glide vector part of the Seitz operator.  If present
   ! and TRUE, "to_unit_cell" will translate the fractional
   ! coordinates into the (0:1,0:1,0:1) unit cell, and
   ! "unit_cell_shift" is set to the translation vector required to do
   ! this.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,3), INOUT :: p
      INT, IN :: op
      VEC(REAL,3), optional, IN :: translate
      BIN, optional, IN :: ignore_glide
      BIN, optional, IN :: to_unit_cell
      VEC(INT,3), optional, OUT :: unit_cell_shift
      VEC(REAL,3) :: pc
      pc = p
      call change_into_fractional_(self%unit_cell,pc)
      call transform_position_(self,pc,op,translate,ignore_glide,to_unit_cell,unit_cell_shift)
      call change_from_fractional_(self%unit_cell,pc)
      p = pc
   end subroutine


   ! The next four routines should be in UNITCELL

   PURE subroutine put_to_unit_cell_0(self,g)
   ! Transform the geometry "g" in fractional coordinates into the
   ! (1,1,1) unit cell.  All atoms will be shifted into the first unit cell
   ! independently, so the resulting geometry may not reflect the shape of the
   ! original molecule.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: g
      INT :: n
   ENSURE(size(g,1)==3 OR size(g,2)==3,"CRYSTAL:put_to_unit_cell_0 ... incorrect shape for array g")
      if (size(g,1)==3) then
         do n = 1,size(g,2)
            call put_to_unit_cell_(self,g(:,n))
         end do
      else
         do n = 1,size(g,1)
            call put_to_unit_cell_(self,g(n,:))
         end do
      end if
   end subroutine

   PURE subroutine put_to_unit_cell_1(self,p,shift)
   ! Transform the position "p" in fractional coordinates into the
   ! [0:1,0:1,0:1) unit cell. If present, "shift" is set to the
   ! translation vector needed to effect the change to the unit cell.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,3), INOUT :: p
      VEC(INT,3), optional, OUT :: shift
      VEC(REAL,3) :: new,pi
      REAL :: tol
   ENSURE(size(p)==3,"CRYSTAL:put_to_unit_cell_1 ... p must be length 3")
      tol = self%unit_cell_tol

      ! Change p to cell boundary (within tol)
      pi = REALIFY(nint(p))  ! Nearest int to p
      where (abs(p-pi)<tol) p = pi

      ! The new unit cell coordinate
      new = p-floor(p)

      ! The shift applied to the (corrected) p
      ! to get it into the unit cell ...
      if (present(shift)) shift = int(new-p)

      ! Set p
      p = new
   end subroutine


!   move_to_unit_cell(g) :: PURE
!   ! Transform the geometry "g" in fractional coordinates into the first
!   ! lattice cell.  The shape of the molecule remains intact, so some of it
!   ! may cross into other cells.  The center of the molecule will be in the
!   ! first cell.
!      self :: INOUT
!      g :: MAT{REAL}, INOUT
!
!   ENSURE(g.dim1==3, "incorrect size for array g")
!
!      n,n_atom :: INT
!      center :: VEC{INT}(3)
!
!      n_atom = g.dim2
!      center = CRYSTAL::unit_cell_offset(g)
!      do n = 1, n_atom
!         g(:,n) = g(:,n) - center
!      end
!
!   end

!   unit_cell_offset(g) result (res) :: selfless, private, PURE
!   ! Which hkl indices match the center of the fragment geometry "g" when
!   ! expressed in crystal coordinates.  Usually we expect that it is 0,0,0 but
!   ! not always.
!      g :: MAT{REAL}, IN
!      res :: VEC{INT}(3)
!
!   ENSURE(g.dim1==3, "incorrect size for array g")
!
!      res = int(g.sum_column_vectors/g.dim2)
!
!   end

   ! For integration grids

!   make_symmetry_equivalents_from(pt0,wt0,site_index,pt,wt) :: PURE, leaky
!   ! Transform *cartesian* 3D points "pt0" and correspoding 1D weights
!   ! "wt0" from an integration grid by the symmetry operators whose
!   ! indices are in "site_symops" and return them in "pt" and "wt"
!      self :: IN
!      pt0 :: MAT{REAL}, IN
!      wt0 :: VEC{REAL}, IN
!      site_index :: INT, IN
!      pt :: MAT{REAL}@, OUT
!      wt :: VEC{REAL}@, OUT
!
!   ENSURE(.spacegroup.seitz.allocated,"Seitz matrices not initialized")
!   ENSURE(.frag_atom_stabilizer.allocated,"No site symmetry operators!")
!   ENSURE(site_index>0,     "operator index out of bounds")
!
!     seitz :: MAT3{REAL}@
!     g,s,ss, n_ss,n_pt :: INT
!
!     n_pt = wt0.dim
!     n_ss = .frag_atom_stabilizer(site_index).element.dim
!
!     ! Rotated points & cloned weights (leaky)
!     pt.create(n_pt*n_ss,3)              
!     wt.create(n_pt*n_ss)
! 
!     ! Cartesian seitz matrices
!     seitz = .transposed_xyz_seitz_matrices 
!
!     s = 0
!     do ss = 1,n_ss
!
!        ! Site symop index
!        g = .frag_atom_stabilizer(site_index).element(ss)
!
!        ! Do the rotation
!        pt(s+1:s+n_pt,:) = matmul(pt0,seitz(:,:,g))
!        wt(s+1:s+n_pt  ) = wt0
!
!        ! Space for next block of pointsd
!        s = s + n_pt
!
!     end
!
!     ! Average by the no. of site symops
!     wt = wt/n_ss
!
!   end

!  ===============
!  Fragment widths
!  ===============

   pure function fragment_width(self) result (res)
   ! Return the width "res" of the fragment in each of the 3 axis directions.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,3) :: res

   ! ENSURE(.fragment_geometry.allocated,"no fragment geometry")
      res = max_abs_column_difference_(self%fragment_geometry)
   end function


!  ==============
!  Seitz matrices
!  ==============

   PURE subroutine make_xyz_seitz_matrices(self,res)
   ! Create and return a list of the 3x3 part of the seitz matrices
   ! in the cartesian axis system. NOTE: these are actually the
   ! *transpose* of the matrices in Hall's paper.
   !               S_cartesian  =  D  S^_crystal  D^-1
   ! NOTE: I think they are not transposed any more ...
      type(CRYSTAL_TYPE), IN :: self
      MAT3(REAL,:,:,:), allocatable, OUT :: res
      INT :: i
   ENSURE(allocated(self%spacegroup%seitz),"CRYSTAL:make_xyz_seitz_matrices ... no Seitz matrices")
      call create_(res,3,3,self%spacegroup%n_seitz)  ! leaky
      do i = 1,self%spacegroup%n_seitz
         res(:,:,i) = matmul(self%unit_cell%direct_mx,matmul(self%spacegroup%seitz(1:3,1:3,i),self%unit_cell%inverse_mx))
      end do
   end subroutine

   PURE function xyz_seitz_matrices(self) result (res)
   ! Create and return a list of the 3x3 part of the seitz matrices
   ! in the cartesian axis system.
      type(CRYSTAL_TYPE), IN :: self
      MAT3(REAL,:,:,:), allocatable :: res
      INT :: i
   ENSURE(allocated(self%spacegroup%seitz),"CRYSTAL:xyz_seitz_matrices ... no Seitz matrices")

      ! leaky
      call create_(res,3,3,self%spacegroup%n_seitz)
      do i = 1,self%spacegroup%n_seitz
         res(:,:,i) = matmul(self%unit_cell%direct_mx,matmul(self%spacegroup%seitz(1:3,1:3,i),self%unit_cell%inverse_mx))
      end do
   end function

   PURE function transposed_xyz_seitz_matrices(self) result (res)
   ! Create and return a list of the 3x3 part of the seitz matrices
   ! in the cartesian axis system.
      type(CRYSTAL_TYPE), IN :: self
      MAT3(REAL,:,:,:), allocatable :: res
      INT :: i
   ENSURE(allocated(self%spacegroup%seitz),"CRYSTAL:transposed_xyz_seitz_matrices ... no Seitz matrices")
      call create_(res,3,3,self%spacegroup%n_seitz)  ! leaky
      do i = 1,self%spacegroup%n_seitz
         res(:,:,i) = matmul(self%unit_cell%reciprocal_mx,matmul(transpose(self%spacegroup%seitz(1:3,1:3,i)),transpose(self%unit_cell%direct_mx)))
      end do
   end function


!  ===========
!  Stabilizers
!  ===========

   PURE subroutine make_stabilizer_symops(self)
   ! Make a list of the indices of the Seitz matrices,
   ! ".stabilizer_symop", which will generate the same geometries to
   ! that in ".fragment_geometry" when both the original and
   ! transformed geometries are converted to unit cell coordinates.
   ! ".n_stabilizer_symops" is set to the number of these symmetry
   ! operations.
   ! NOTE: geometries NOT whole-shifted to unit cell ... should they ! be?
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: g1,gi
      INT :: i,n
      BIN :: identical
      call destroy_(self%stabilizer_symop)
      call create_(self%stabilizer_symop,self%spacegroup%n_seitz)
      call create_(g1,3,self%n_fragment_atoms)
      call create_(gi,3,self%n_fragment_atoms)
      g1 = self%fragment_geometry
      n = 1
      self%stabilizer_symop(1) = 1
      do i = 2,self%spacegroup%n_seitz
         gi = self%fragment_geometry
         call transform_geometry_(self,gi,i)
         identical = is_same_geometry_(self,gi,g1)
         if (NOT identical) cycle
         n = n+1
         self%stabilizer_symop(n) = i
      end do
      call destroy_(gi)
      call destroy_(g1)
      self%n_stabilizer_symops = n
      call shrink_(self%stabilizer_symop,n)
   end subroutine

   PURE function is_same_geometry(self,geom_i,geom_j) result (res)
   ! Return TRUE if the geometries "geom_i" and "geom_j" in fractional
   ! coordinates are the same, exactly.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), IN :: geom_i
      MAT(REAL,:,:), IN :: geom_j
      BIN :: res
      INT :: i,j,n_atom
      VEC(BIN,:), allocatable :: skip
      VEC(REAL,3) :: delta,zero_vec
      BIN :: same
      REAL :: tol
   ENSURE(size(geom_i,1)==3,"CRYSTAL:is_same_geometry ... incorrect size for array geom_i")
   ENSURE(size(geom_j,1)==3,"CRYSTAL:is_same_geometry ... incorrect size for array geom_j")
   ENSURE(size(geom_i,2)==size(geom_j,2),"CRYSTAL:is_same_geometry ... incompatible sizes for geom_i, geom_j")
      tol = self%same_atom_tol
      zero_vec = ZERO
      n_atom = size(geom_i,2)
      call create_(skip,n_atom); skip(:) = FALSE
      do i = 1,n_atom
         do j = 1,n_atom

         ! mod 1 fractional coordinate comparison Kang 2024 Jul 2
            delta = geom_i(:,i)-geom_j(:,j)
            delta = MOD(delta+HALF+ONE,ONE)-HALF  ! Difference vector (=delta) in [-1/2, 1/2) domain
            same = same_as_(delta,zero_vec,tol)
            if (NOT same OR skip(j)) cycle
            skip(j) = TRUE
            exit
         end do
      end do
      res = all(skip)  ! True if all atoms in i were matched (skipped) in j
      call destroy_(skip)
   end function

   PURE function xyz_stabilizer_matrices(self) result (res)
   ! Create and return a list of the 3x3 part of the seitz matrices
   ! in the cartesian axis which stabilize the fragment geometry.
      type(CRYSTAL_TYPE), IN :: self
      MAT3(REAL,:,:,:), allocatable :: res
      INT :: i,s
   ENSURE(allocated(self%spacegroup%seitz),"CRYSTAL:xyz_stabilizer_matrices ... no Seitz matrices")
   ENSURE(allocated(self%stabilizer_symop),"CRYSTAL:xyz_stabilizer_matrices ... no stabilizer symop matrices")
      call create_(res,3,3,self%n_stabilizer_symops)
      do i = 1,self%n_stabilizer_symops
         s = self%stabilizer_symop(i)
         res(:,:,i) = matmul(self%unit_cell%direct_mx,matmul(self%spacegroup%seitz(1:3,1:3,s),self%unit_cell%inverse_mx))
      end do
   end function


!  Use GAUSSIAN_DATA for the routines below ...

   PURE subroutine make_stabilizer_xyz_matrices(self)
   ! Make the representation matrices for xyz products found in
   ! gaussian shells, i.e. work out matrix R, where the shell row vector
   ! p'(r) = p(S^{-1}r) = p(r)R, and S is a stabilizer symop.
   ! WARNING: this routine is tied to an explicit ordering of the cartesian
   ! gaussian basis functions in a shell.
      type(CRYSTAL_TYPE), INOUT :: self
      call create_(self%str,1,1,self%n_stabilizer_symops)
      self%str = ONE
      self%ptr = xyz_stabilizer_matrices_(self)  ! 3  x 1
      self%dtr = gaussian_d_xyz_matrices_(self%ptr)  ! 6  x 6
      self%ftr = gaussian_f_xyz_matrices_(self%ptr)  ! 10 x 10
      self%gtr = gaussian_g_xyz_matrices_(self%ptr)  ! 15 x 15
   end subroutine

   PURE function xyz_stabilizer_mx(self,n,l) result (res)
   ! Return the "n"-th stabilizer representation matrix for xyz
   ! product found in a gaussian shell of angular momentum "l"
      type(CRYSTAL_TYPE), IN :: self
      INT, IN :: n,l
      MAT(REAL,:,:), allocatable :: res
   ENSURE(l>=0,"CRYSTAL:xyz_stabilizer_mx ... l must be non-negative")
   ENSURE(l<=4,"CRYSTAL:xyz_stabilizer_mx ... l must be no greater than 4 (no h functions or higher)")
      select case (l)
      case (0); res = self%str(:,:,n)
      case (1); res = self%ptr(:,:,n)
      case (2); res = self%dtr(:,:,n)
      case (3); res = self%ftr(:,:,n)
      case (4); res = self%gtr(:,:,n)
      end select
   end function


!  =====================================
!  Service methods used by other modules
!  =====================================

! Don't delete code below just yet ...

!   make_phased_mx_for_symop(u,phase,mask) :: private
!   ! Return the sum of the "phase" shifts times the seitz matrices for all
!   ! symops which are equivalent to the "u"-th unique symmetry operation,
!   ! .spacegroup.unique_SF_symop(u), as determined by the "mask" array, when
!   ! mask(u)==.spacegroup.unique_SF_symop(u).
!     u :: INT, IN
!     phase :: MAT3{CPX}, OUT
!     mask :: VEC{INT}, IN
!
!   ENSURE(.spacegroup.analyzed,"no spacegroup")
!   ENSURE(u<=.spacegroup.n_unique_SF_symops,"symop index out of range")
!   ENSURE(phase.dim1==.xray_data.reflections.dim,"wrong size, dim=1, phase array")
!   ENSURE(phase.dim2==3,"wrong size, dim=2, phase array")
!   ENSURE(phase.dim3==3,"wrong size, dim=3, phase array")
!
!     pi2,tx,ty,tz,dot :: REAL
!     s,n,n_refl,h,k,l :: INT
!     seitz :: MAT{REAL}(4,4)
!     refl :: VEC{REFLECTION}*
!
!     refl => .xray_data.reflections
!     n_refl = refl.dim
!     pi2 = TWO*PI
!
!     phase = ZERO
!     do s = 1,.spacegroup.n_seitz
!
!       ! Sum only those which match unique symop u
!       if (mask(s)/=.spacegroup.unique_SF_symop(u)) cycle
!
!       ! Get the symop translation vector
!       seitz = .spacegroup.seitz(:,:,s)
!       tx = pi2*seitz(1,4)
!       ty = pi2*seitz(2,4)
!       tz = pi2*seitz(3,4)
!
!       ! Sum the phase here
!       ! This seems a kludge because we don't know the magnetic symmetry
!       if (seitz(3,3)>0) then                  ! M points along +z always
!         do n = 1,n_refl
!           h = refl(n).h1
!           k = refl(n).h2
!           l = refl(n).h3
!           dot = h*tx+k*ty+l*tz

!              seitz(:,:)*exp(cmplx(ZERO,dot,kind=CPX_KIND))
!         end
!       else                                    ! M points in -z direction
!         seitz(3,:3) = -seitz(3,:3)            ! Invert
!         do n = 1,n_refl
!           h = refl(n).h1
!           k = refl(n).h2
!           l = refl(n).h3
!           dot = h*tx+k*ty+l*tz

!              seitz(:,:)*exp(cmplx(ZERO,dot,kind=CPX_KIND))
!         end
!       end
!
!     end
!
!   end
!
!   sum_PND_spin_ints(ft_ints,unique_ft_ints)
!   ! Form the Fourier transform integrals "ft_ints", required for the spin
!   ! magnetic structure factors, from a sum of the list of unique structure
!   ! factor intergals "unique_ft_ints".  Dimensions of ft_ints are
!   ! [.reflections.dim,n_comp_a,n_comp_b,3].
!     ft_ints :: MAT4{CPX}
!     unique_ft_ints :: MAT3{CPX}, IN
!
!   ENSURE(.xray_data.allocated,"no data")
!   ENSURE(.xray_data.reflections.allocated,"no reflection data")
!   ENSURE(ft_ints.dim1==.xray_data.reflections.dim,"wrong size, array ft_ints")
!   ENSURE(unique_ft_ints.dim1==.spacegroup.n_unique_SF_symops*.xray_data.reflections.dim,"wrong size, array unique_ft_ints")
!
!     phase :: MAT3{CPX}*
!     q :: MAT{REAL}*
!     u,uf,n,n_refl :: INT
!
!     n_refl = .xray_data.reflections.dim
!     q.create(n_refl,3)
!     phase.create(n_refl,3,3)
!
!     .make_k_pts(q)
!
!     ft_ints = ZERO
!     do u = 1,.spacegroup.n_unique_SF_symops
!
!        uf = n_refl*(u-1)
!
!        ! Add up translated symop contribution
!        .make_phased_mx_for_symop(u,phase,.spacegroup.translated_symop)
!        do n = 1,n_refl

!              unique_ft_ints(uf+n,:,:)

!              unique_ft_ints(uf+n,:,:)

!              unique_ft_ints(uf+n,:,:)
!        end
!
!        ! Add up inverted symop contribution, if any
!        if ( all(.spacegroup.inverted_symop/=.spacegroup.unique_SF_symop(u)) ) cycle ! for inversions...
!        .make_phased_mx_for_symop(u,phase,.spacegroup.inverted_symop)
!        do n = 1,n_refl

!              conjg(unique_ft_ints(uf+n,:,:))

!              conjg(unique_ft_ints(uf+n,:,:))

!              conjg(unique_ft_ints(uf+n,:,:))
!        end
!
!     end
!     phase.destroy
!
!     ! The factor of two for conversion to Bohr magnetons cancels the
!     ! factor of half for the S=sigma/2 operator, and g_e x mu_B = 1.
!     do n = 1,n_refl
!       ft_ints(n,:,:,:) = ft_ints(n,:,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
!     end
!     q.destroy
!
!   end
!
!   sum_PND_nabla_ints(ft_ints,unique_ft_ints)
!   ! Form the fourier transform nabla_a_3 integrals "ft_ints", required for the
!   ! PND magnetic structure factors, from a sum of "unique_ft_ints".
!   ! Dimensions of ft_ints are [.reflections.dim,n_comp_a,n_comp_b,3].
!     ft_ints :: MAT3{CPX}
!     unique_ft_ints :: MAT4{CPX}, IN
!
!   ENSURE(.xray_data.allocated,"no data")
!   ENSURE(.xray_data.reflections.allocated,"no reflection data")
!   ENSURE(ft_ints.dim1==.xray_data.reflections.dim,"wrong size, array ft_ints")
!   ENSURE(unique_ft_ints.dim1==.spacegroup.n_unique_SF_symops*.xray_data.reflections.dim,"wrong size, array unique_ft_ints")
!
!     phase :: MAT3{CPX}*
!     q :: MAT{REAL}*
!     u,uf,n,n_refl :: INT
!
!     n_refl = .xray_data.reflections.dim
!     q.create(n_refl,3)
!     phase.create(n_refl,3,3)
!
!     .make_k_pts(q)
!
!     ft_ints = ZERO
!     do u = 1,.spacegroup.n_unique_SF_symops
!
!       uf = n_refl*(u-1)
!
!       ! Add up translated symop contribution
!       .make_phased_mx_for_symop(u,phase,mask=.spacegroup.translated_symop)
!       do n = 1,n_refl

!          (q(n,1)*phase(n,2,3)-q(n,2)*phase(n,1,3))*unique_ft_ints(uf+n,:,:,3)
!       end
!
!       ! Add up inverted symop contribution, if any
!       if ( all(.spacegroup.inverted_symop/=.spacegroup.unique_SF_symop(u)) ) cycle ! for inversions ...
!       .make_phased_mx_for_symop(u,phase,mask=.spacegroup.translated_symop) ! .inverted_symop here ??????????????
!       do n = 1,n_refl

!          (q(n,1)*phase(n,2,3)-q(n,2)*phase(n,1,3))*conjg(unique_ft_ints(uf+n,:,:,3))
!       end
!     end
!     phase.destroy
!
!     ! The factor of 2 to convert to Bohr magnetons cancels the factor
!     ! of 1/2 for the Bohr magneton.
!     do n = 1,n_refl
!       ft_ints(n,:,:) = ft_ints(n,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
!     end
!     q.destroy
!
!   end
!
!   sum_ft_spin_ints(ft_ints,unique_ft_ints)
!   ! Form the Fourier transform integrals "ft_ints", required for the spin
!   ! magnetic structure factors, from a sum of the list of unique structure
!   ! factor intergals "unique_ft_ints".  Dimensions of ft_ints are
!   ! [.reflections.dim,n_comp_a,n_comp_b,3,3].
!     ft_ints :: MAT5{CPX}
!     unique_ft_ints :: MAT3{CPX}, IN
!
!   ENSURE(.xray_data.allocated,"no data")
!   ENSURE(.xray_data.reflections.allocated,"no reflection data")
!   ENSURE(ft_ints.dim1==.xray_data.reflections.dim,"wrong size, array ft_ints")
!   ENSURE(unique_ft_ints.dim1==.spacegroup.n_unique_SF_symops*.xray_data.reflections.dim,"wrong size, array unique_ft_ints")
!
!     phase :: VEC{CPX}*
!     q :: MAT{REAL}*
!     ints :: MAT{CPX}*
!     u,uf,n,n_refl :: INT
!
!     n_refl = .xray_data.reflections.dim
!     phase.create(n_refl)
!     q.create(n_refl,3)
!     ints.create(ft_ints.dim2,ft_ints.dim3,3)
!
!     .make_k_pts(q)
!
!     ft_ints = ZERO
!     do u = 1,.spacegroup.n_unique_SF_symops
!
!       uf = n_refl*(u-1)
!
!       ! Add up translated symop contribution
!       .make_phases_for_symop(u,phase,mask=.spacegroup.translated_symop)
!       do n = 1,n_refl
!         ints = phase(n)*unique_ft_ints(uf+n,:,:)
!         ft_ints(n,:,:,1,1) = ft_ints(n,:,:,1,1) + q(n,1)*q(n,1)*ints(:,:)
!         ft_ints(n,:,:,2,1) = ft_ints(n,:,:,2,1) + q(n,2)*q(n,1)*ints(:,:)
!         ft_ints(n,:,:,2,2) = ft_ints(n,:,:,2,2) + q(n,2)*q(n,2)*ints(:,:)
!         ft_ints(n,:,:,3,1) = ft_ints(n,:,:,3,1) + q(n,3)*q(n,1)*ints(:,:)
!         ft_ints(n,:,:,3,2) = ft_ints(n,:,:,3,2) + q(n,3)*q(n,2)*ints(:,:)
!         ft_ints(n,:,:,3,3) = ft_ints(n,:,:,3,3) + q(n,3)*q(n,3)*ints(:,:)
!       end
!
!       ! Add up inverted symop contribution, if any
!       if ( all(.spacegroup.inverted_symop/=.spacegroup.unique_SF_symop(u)) ) cycle ! for inversions...
!       .make_phases_for_symop(u,phase,mask=.spacegroup.inverted_symop)
!       do n = 1,n_refl
!         ints = phase(n)*conjg(unique_ft_ints(uf+n,:,:))
!         ft_ints(n,:,:,1,1) = ft_ints(n,:,:,1,1) + q(n,1)*q(n,1)*ints(:,:)
!         ft_ints(n,:,:,2,1) = ft_ints(n,:,:,2,1) + q(n,2)*q(n,1)*ints(:,:)
!         ft_ints(n,:,:,2,2) = ft_ints(n,:,:,2,2) + q(n,2)*q(n,2)*ints(:,:)
!         ft_ints(n,:,:,3,1) = ft_ints(n,:,:,3,1) + q(n,3)*q(n,1)*ints(:,:)
!         ft_ints(n,:,:,3,2) = ft_ints(n,:,:,3,2) + q(n,3)*q(n,2)*ints(:,:)
!         ft_ints(n,:,:,3,3) = ft_ints(n,:,:,3,3) + q(n,3)*q(n,3)*ints(:,:)
!       end
!     end
!     ints.destroy
!
!     ! The factor of two for conversion to Bohr magnetons cancels the
!     ! factor of half for the S operator
!     do n = 1,n_refl
!        ft_ints(n,:,:,:,:) = -ft_ints(n,:,:,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
!     end
!     q.destroy
!     phase.destroy
!
!   end
!
!   sum_ft_r_ints(ft_ints,unique_ft_ints,B)
!   ! Form the Fourier transform dipole integrals "ft_ints", required for the PND
!   ! magnetic structure factors, from a sum of the list of unique structure
!   ! factor intergals "unique_ft_ints".  "B" is the external magnetic field.
!   ! Note: only inversions are allowed as symmetry elements for PND simulations.
!   ! Dimensions of ft are [.reflections.dim,n_comp_a,n_comp_b,3].
!     ft_ints :: MAT4{CPX}
!     unique_ft_ints :: MAT4{CPX}, IN
!     B :: VEC{REAL}(3)
!
!   ENSURE(.xray_data.allocated,"no data")
!   ENSURE(.xray_data.reflections.allocated,"no reflection data")
!   ENSURE(ft_ints.dim1==.xray_data.reflections.dim,"wrong size, array ft_ints")
!   ENSURE(unique_ft_ints.dim1==.spacegroup.n_unique_SF_symops*.xray_data.reflections.dim,"wrong size, array unique_ft_ints")
!
!     phase :: VEC{CPX}*
!     q :: MAT{REAL}*
!     ints :: MAT3{CPX}*
!     u,uf,n,n_refl :: INT
!     ci :: CPX
!
!     n_refl = .xray_data.reflections.dim
!     phase.create(n_refl)
!     q.create(n_refl,3)
!     ints.create(ft_ints.dim2,ft_ints.dim3,3)
!
!     .make_k_pts(q)
!
!     ft_ints = ZERO
!     do u = 1,.spacegroup.n_unique_SF_symops
!
!       uf = n_refl*(u-1)
!
!       ! Add up translated symop contribution
!       .make_phases_for_symop(u,phase,mask=.spacegroup.translated_symop)
!       do n = 1,n_refl
!         ints = phase(n)*unique_ft_ints(uf+n,:,:,:)

!                                             - q(n,3)*(B(3)*ints(:,:,1)-B(1)*ints(:,:,3))

!                                             - q(n,1)*(B(1)*ints(:,:,2)-B(2)*ints(:,:,1))

!                                             - q(n,2)*(B(2)*ints(:,:,3)-B(3)*ints(:,:,2))
!       end
!
!       ! Add up inverted symop contribution, if any
!       if ( all(.spacegroup.inverted_symop/=.spacegroup.unique_SF_symop(u)) ) cycle ! for inversions ...
!       .make_phases_for_symop(u,phase,mask=.spacegroup.inverted_symop)
!       do n = 1,n_refl
!         ints = -phase(n)*conjg(unique_ft_ints(uf+n,:,:,:)) ! note minus sign

!                                             - q(n,3)*(B(3)*ints(:,:,1)-B(1)*ints(:,:,3))

!                                             - q(n,1)*(B(1)*ints(:,:,2)-B(2)*ints(:,:,1))

!                                             - q(n,2)*(B(2)*ints(:,:,3)-B(3)*ints(:,:,2))
!       end
!     end
!     ints.destroy
!
!     ! The factor of two for conversion to Bohr magnetons cancels the
!     ! factor of half for the S operator
!     ci = (ZERO,ONE)
!     do n = 1,n_refl
!       ft_ints(n,:,:,:) = -ci*ft_ints(n,:,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
!     end
!     q.destroy
!     phase.destroy
!
!   end
!
!   sum_ft_nabla_ints(ft_ints,unique_ft_ints)
!   ! Form the fourier transform nabla_a integrals "ft_ints", required for the
!   ! PND magnetic striucture factors, from a sum of "unique_ft_ints".
!   ! Note: only inversions are allowed as symmetry elements for PND simulations.
!   ! Note: the complex conjugate nabla_b integrals are not included.
!   ! Dimensions of ft_ints are [.reflections.dim,n_comp_a,n_comp_b,3].
!      ft_ints :: MAT4{CPX}
!      unique_ft_ints :: MAT4{CPX}, IN
!
!   ENSURE(.xray_data.allocated,"no data")
!   ENSURE(.xray_data.reflections.allocated,"no reflection data")
!   ENSURE(ft_ints.dim1==.xray_data.reflections.dim,"wrong size, array ft_ints")
!   ENSURE(unique_ft_ints.dim1==.spacegroup.n_unique_SF_symops*.xray_data.reflections.dim,"wrong size, array unique_ft_ints")
!
!      phase :: VEC{CPX}*
!      q :: MAT{REAL}*
!      ints :: MAT3{CPX}*
!      u,uf,n,n_refl :: INT
!
!      n_refl = .xray_data.reflections.dim
!      phase.create(n_refl)
!      q.create(n_refl,3)
!      ints.create(ft_ints.dim2,ft_ints.dim3,3)
!
!      .make_k_pts(q)
!
!      ft_ints = ZERO
!      do u = 1,.spacegroup.n_unique_SF_symops
!
!         uf = n_refl*(u-1)
!
!         ! Add up translated symop contribution
!         .make_phases_for_symop(u,phase,mask=.spacegroup.translated_symop)
!         do n = 1,n_refl
!            ints = phase(n)*unique_ft_ints(uf+n,:,:,:)
!            ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + q(n,2)*ints(:,:,3)-q(n,3)*ints(:,:,2)
!            ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + q(n,3)*ints(:,:,1)-q(n,1)*ints(:,:,3)
!            ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + q(n,1)*ints(:,:,2)-q(n,2)*ints(:,:,1)
!         end
!
!         ! Add up inverted symop contribution, if any
!         if ( all(.spacegroup.inverted_symop/=.spacegroup.unique_SF_symop(u)) ) cycle ! for inversions ...
!         .make_phases_for_symop(u,phase,mask=.spacegroup.inverted_symop)
!         do n = 1,n_refl
!            ints = -phase(n)*conjg(unique_ft_ints(uf+n,:,:,:)) ! note minus sign
!            ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + q(n,2)*ints(:,:,3)-q(n,3)*ints(:,:,2)
!            ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + q(n,3)*ints(:,:,1)-q(n,1)*ints(:,:,3)
!            ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + q(n,1)*ints(:,:,2)-q(n,2)*ints(:,:,1)
!         end
!
!      end
!      ints.destroy
!
!      ! The factor of 2 to convert to Bohr magnetons cancels the factor
!      ! of 1/2 for the Bohr magneton
!      ! Extra minus sign introduced, but not sure why ...
!      ! Reversed minus sign introduced ...
!      do n = 1,n_refl
!          ft_ints(n,:,:,:) = ft_ints(n,:,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
!      end
!      q.destroy
!      phase.destroy
!
!   end
!
!   sum_ft_j_ints(ft_ints,unique_ft_ints)
!   ! Form the fourier transform j integrals "ft_ints", required for the PND
!   ! magnetic striucture factors, from a sum of "unique_ft_ints".
!   ! Note: only inversions are allowed as symmetry elements for PND simulations.
!   ! Note: the complex conjugate nabla_b integrals are not included.
!   ! Dimensions of ft_ints are [.reeflections.dim,n_comp_a,n_comp_b,3].
!     ft_ints :: MAT4{CPX}
!     unique_ft_ints :: MAT4{CPX}, IN
!
!   ENSURE(.xray_data.allocated,"no data")
!   ENSURE(.xray_data.reflections.allocated,"no reflection data")
!   ENSURE(ft_ints.dim1==.xray_data.reflections.dim,"wrong size, array ft_ints")
!   ENSURE(unique_ft_ints.dim1==.spacegroup.n_unique_SF_symops*.xray_data.reflections.dim,"wrong size, array unique_ft_ints")
!
!     phase :: VEC{CPX}*
!     u,uf, n,n_refl :: INT
!
!     n_refl = .xray_data.reflections.dim
!     phase.create(n_refl)
!
!     ft_ints = ZERO
!     do u = 1,.spacegroup.n_unique_SF_symops
!
!        uf = n_refl*(u-1)
!
!        ! Add up translated symop contribution
!        .make_phases_for_symop(u,phase,mask=.spacegroup.translated_symop)
!        do n = 1,n_refl
!          ft_ints(n,:,:,:) = ft_ints(n,:,:,:) + phase(n)*unique_ft_ints(uf+n,:,:,:)
!        end
!
!        ! Add up inverted symop contribution, if any
!        if ( all(.spacegroup.inverted_symop/=.spacegroup.unique_SF_symop(u)) ) cycle ! for inversions...
!        .make_phases_for_symop(u,phase,mask=.spacegroup.inverted_symop)
!        do n = 1,n_refl
!          ft_ints(n,:,:,:) = ft_ints(n,:,:,:) - phase(n)*conjg(unique_ft_ints(uf+n,:,:,:))
!        end
!
!     end
!     phase.destroy
!
!   end

!  =====
!  Tests
!  =====

   PURE function do_fragHAR(self) result (res)
   ! Returns TRUE if we are refining fragments.
      type(CRYSTAL_TYPE), allocatable, IN :: self
      BIN :: res
      res = FALSE
      if (allocated(self)) then
         if (allocated(self%xray_data)) then
            res = self%xray_data%refine_fragments
         end if
      end if
   end function

   pure function use_Ryde_capping(self) result (res)
   ! Return TRUE if Ulf Ryde's capping method for proteins is to be
   ! used in a fragHAR refinement.
      type(CRYSTAL_TYPE), allocatable, IN :: self
      BIN :: res
      res = FALSE
      if (allocated(self)) then
         if (allocated(self%xray_data)) then
            res = self%xray_data%use_Ryde_capping
         end if
      end if
   end function

   pure function use_NN(self) result (res)
   ! Return TRUE if the nearest-neighbour capping method for periodic
   ! network solids is to be used in a fragHAR refinement.
      type(CRYSTAL_TYPE), allocatable, IN :: self
      BIN :: res
      res = FALSE
      if (allocated(self)) then
         if (allocated(self%xray_data)) then
            res = self%xray_data%use_NN
         end if
      end if
   end function

   pure function use_NN_capping(self) result (res)
   ! Return TRUE if the nearest-neighbour fragments are to have their
   ! outermost shell replaced by capping hydrogen atoms.
      type(CRYSTAL_TYPE), allocatable, IN :: self
      BIN :: res
      res = TRUE
      if (allocated(self)) then
         if (allocated(self%xray_data)) then
            res = self%xray_data%use_NN_capping
         end if
      end if
   end function

   pure function NN_level(self) result (res)
   ! Return the number of nearest-neighbour shells to include when
   ! making the fragment around each asymmetric unit atom, and 1 if
   ! there is no diffraction data to say otherwise.
      type(CRYSTAL_TYPE), allocatable, IN :: self
      INT :: res
      res = 1
      if (allocated(self)) then
         if (allocated(self%xray_data)) then
            res = self%xray_data%NN_level
         end if
      end if
   end function


!  ===============
!  Inquiry methods
!  ===============

!   xray_data_exists result (res) :: pure
!   ! Return TRUE if the diffraction data information exists
!      self :: IN
!      res :: BIN
!      res = .xray_data.allocated
!   end

!   fragment_data_exists result (res) :: pure
!   ! Return TRUE if a fragment information exists
!      self :: IN
!      res :: BIN
!      res = .fragment_geometry.allocated
!   end

!   asymmetric_unit_exists result (res) :: pure
!   ! Return TRUE if the asymmetric unit geometry information exists
!      self :: IN
!      res :: BIN
!      res = .asymmetric_unit_geometry.allocated
!   end

!   unit_cell_geometry_exists result (res) :: pure
!   ! Return TRUE if the unit_cell_geometry information exists
!      self :: IN
!      res :: BIN
!      res = .unit_cell_geometry.allocated
!   end

!  ==========================================
!  Partitioning and temperature factor models
!  ==========================================

   pure function partition_model(self) result (res)
   ! Model for partitioning fragments of the molecule
      type(CRYSTAL_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      if (allocated(self%xray_data)) then; res = self%xray_data%partition_model
      else; res = " "
      end if
   end function

   pure function uses_numerical_FFs(self) result (res)
   ! Returns TRUE if numerical integration of the electron density is
   ! required for atomic form factors (or other) refinements.
   ! Currently only "hirshfeld" is supported for this.
      type(CRYSTAL_TYPE), IN :: self
      BIN :: res
      res = partition_model_(self)=="oc-hirshfeld" OR partition_model_(self)=="oc-ri" OR partition_model_(self)=="oc-exphar"
   end function

   pure function uses_oc_atom_partition(self) result (res)
   ! Returns TRUE if one-center atomic partition model is used.
      type(CRYSTAL_TYPE), IN :: self
      BIN :: res
      STR(len=STR_SIZE) :: model
      model = partition_model_(self)
      res = model(1:2)=="oc"
   end function

   pure function uses_Hirshfeld_atoms(self) result (res)
   ! Returns TRUE if numerical integration of the electron density is
      type(CRYSTAL_TYPE), IN :: self
      BIN :: res
      res = partition_model_(self)=="oc-hirshfeld" OR partition_model_(self)=="oc-ri" OR partition_model_(self)=="oc-exphar"
   end function

   pure function uses_Tanaka_atoms(self) result (res)
   ! Returns TRUE if one-center Tanaka atoms are being used.
      type(CRYSTAL_TYPE), IN :: self
      BIN :: res
      res = partition_model_(self)=="oc-tanaka"
   end function

   pure function using_one_center_TF_model(self) result (res)
   ! Temperature factor model to use for structure factors.
      type(CRYSTAL_TYPE), IN :: self
      BIN :: res
      if (allocated(self%xray_data)) then; res = using_one_center_TF_model_(self%xray_data)
      else; res = FALSE
      end if
   end function


!   uses_one_center_TF_model result (res) :: pure
!   ! Temperature factor model to use for structure factors.
!      self :: IN
!      res :: BIN
!
!      val :: STR
!
!      val = .temperature_factor_model
!      res = val=="one-center" OR val=="atom-based"
!
!   end

!   temperature_factor_model result (res) :: pure
!   ! Temperature factor model to use for structure factors.
!      self :: IN
!      res :: STR
!      if (.xray_data_exists) then; res = .xray_data.temperature_factor_model
!      else;                        res = " "
!      end
!   end

!  =======================
!  Equivalents and merging
!  =======================

   PURE function is_symmetry_unmerged(self) result (res)
   ! Returns TRUE if the list of reflections has symmetry equivalents
   ! i.e. is symmetry *unmerged* or is Friedel unmerged.
      type(CRYSTAL_TYPE), IN :: self
      BIN :: res
      INT :: i,j,g
      INT :: n_refl
      VEC(INT,3) :: p,q,r
      BIN :: same
   ENSURE(allocated(self%xray_data),"CRYSTAL:is_symmetry_unmerged ... no diffraction data")
   ENSURE(allocated(self%xray_data%reflections),"CRYSTAL:is_symmetry_unmerged ... no reflection data")
      n_refl = size(self%xray_data%reflections)

      ! Loop reflection
      same = FALSE
      refl: do i = 1,n_refl
         p = Miller_indices_(self%xray_data%reflections(i))

         ! The other reflection
         same = FALSE
         do j = i+1,n_refl
            do g = 1,self%spacegroup%n_seitz

            ! Symmetry transform it
               q = Miller_indices_(self%xray_data%reflections(j))
               r = int(matmul(q,self%spacegroup%seitz(1:3,1:3,g)),kind=INT_KIND)
               same = same_as_(p,r)
               if (same) exit refl
            end do
         end do
      end do refl
      res = same
   end function

   PURE subroutine make_list_of_equivalents_v2(self,list,n_unique)
   ! Make a "list" of the indices of the first equivalent reflection
   ! i.e. list(i) = j < i is the first reflection symmetry-equivalent 
   ! the i-th reflection.  "n_unique" is returned as the number of
   ! unique reflections, where list(i) == i.
   ! NOTE: Includes inversions, if the spacegroup is centrosymmetric.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), allocatable, OUT :: list
      INT, OUT :: n_unique
      INT :: i,j,g,n_refl
      VEC(INT,3) :: p,q,r
      MAT(REAL,3,3) :: seitz
      BIN :: same
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_list_of_equivalents_v2 ... no diffraction data")
   ENSURE(allocated(self%xray_data%reflections),"CRYSTAL:make_list_of_equivalents_v2 ... no reflection data")
      n_refl = size(self%xray_data%reflections)

      ! Default list(:) values
      call create_(list,n_refl)  ! leaky
      call to_sequence_(list,1,n_refl)
      n_unique = 0

      ! Loop reflection indices "p"
      do i = 1,n_refl
         p = Miller_indices_(self%xray_data%reflections(i))

         ! Find equivalents
         same = FALSE
         refl: do j = i+1,n_refl
            q = Miller_indices_(self%xray_data%reflections(j))
            do g = 1,self%spacegroup%n_seitz

               ! Get symmetry equivalent indices "r"
               seitz = transpose(self%spacegroup%seitz(1:3,1:3,g))
               r = nint(matmul(seitz,q))
               ! Equivalent?
               same = same_as_(p,r)
               if (same) then
                  list(j) = i  ! Set here
                  exit refl
               end if
            end do
         end do refl

         ! Increment
         if (NOT same) n_unique = n_unique+1
      end do
   end subroutine

   PURE subroutine remove_equivalents(self,list,n_unique)
   ! If there are any equivalent reflections keep only the one that
   ! occures first in the list.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(INT,:), IN :: list
      INT, IN :: n_unique
      VEC(type(REFLECTION_TYPE),:), allocatable :: reflections
      INT :: i,n
   ENSURE(allocated(self%xray_data),"CRYSTAL:remove_equivalents ... no diffraction data")
   ENSURE(allocated(self%xray_data%reflections),"CRYSTAL:remove_equivalents ... no reflection data")

      ! Define set of new unique reflections
      call create_(reflections,n_unique)  ! leaky
      n = 0
      do i = 1,size(self%xray_data%reflections)
         if (list(i)<i) cycle
         n = n+1
         reflections(n) = self%xray_data%reflections(i)
      end do

      ! Set new reflections
      self%xray_data%reflections = reflections
   end subroutine


!   merge_equivalents :: leaky, PURE
!   ! This routine will merge this list of equivalents into a single reflections
!   ! with a particular sigma value allocated with it. WARNING: it assumes that
!   ! there are a sufficient number of equivalents so that the sigma value can be
!   ! calculated by the standard formula for the standard deviation.
!      self :: INOUT
!
!   ENSURE(.xray_data.allocated,"no diffraction data")
!   ENSURE(.xray_data.reflections.allocated,"no reflection data")
!
!      list :: VEC{EVEC{INT}}@
!      reflections :: VEC{REFLECTION}@
!      n_unique,n_refl,n,i :: INT
!
!      n_refl = .xray_data.reflections.dim
!
!      .make_list_of_equivalents(list,n_unique)
!
!      ! Define set of new reflections
!      reflections.create(n_unique)
!      n = 0
!      do i = 1,n_refl
!
!         if (any(list(i)[:]<i)) cycle
!
!         n = n + 1
!
!         reflections(n) = .xray_data.reflections(i)
!         reflections(n).set_F_exp(.xray_data.reflections(list(i)[:]).F_exp.mean)
!         reflections(n).set_F_sigma(.xray_data.reflections(list(i)[:]).F_sigma.mean)
!
!      end
!
!      ! Set new reflections
!      .xray_data.reflections = reflections
!
!      ! Clean up
!      list.destroy
!
!   end

!   remove_equivalents :: leaky, PURE
!   ! If there are any equivalent reflections keep only the one that
!   ! occurs first in the list.
!      self :: INOUT
!
!      list :: VEC{INT}@
!      n_unique :: INT
!
!      .make_list_of_equivalents_v2(list,n_unique)
!      .remove_equivalents(list,n_unique)
!      list.destroy
!
!   end

!   merge_ordered_equivalents :: leaky, PURE
!   ! This routine will merge apartially ordered list of equivalents
!   ! into a single reflections with a particular sigma value
!   ! allocated with it.
!   ! WARNING: it assumes that there are a sufficient number of
!   ! equivalents so that the sigma value can be calculated by the
!   ! standard formula for the standard deviation.
!      self :: INOUT
!
!   ENSURE(.xray_data.allocated,"no diffraction data")
!   ENSURE(.xray_data.reflections.allocated,"no reflection data")
!
!      list :: VEC{EVEC{INT}}@
!      reflections :: VEC{REFLECTION}@
!      n_unique,n_refl,n,i :: INT
!
!      n_refl = .xray_data.reflections.dim
!
!      .make_list_of_equivalents_ord(list,n_unique)
!
!      ! Define set of new reflections
!      reflections.create(n_unique)
!      n = 0
!      do i = 1,n_refl
!
!         if (list(i)[1]<i) cycle
!
!         n = n + 1
!         reflections(n) = .xray_data.reflections(i)
!         reflections(n).set_F_exp(.xray_data.reflections(list(i)[:]).F_exp.mean)
!         reflections(n).set_F_sigma(.xray_data.reflections(list(i)[:]).F_exp.standard_deviation)
!
!      end
!
!      ! Reset reflections
!      .xray_data.reflections = reflections
!
!      list.destroy
!
!   end

!   make_list_of_equivalents_ord(list,n_unique) :: leaky, private, PURE
!   ! Make a list of the equivalent reflections. For each reflection i,
!   ! list(i).element gives the list of equivalent reflections. If
!   ! list(i).element has only one element and its index is less than i then we
!   ! know it is an equivalent, and this index is the index of its equivalent.
!      self :: IN
!      list :: VEC{EVEC{INT}}@, OUT
!      n_unique :: INT, OUT
!
!   ENSURE(.xray_data.allocated,"no diffraction data")
!   ENSURE(.xray_data.reflections.allocated,"no reflection data")
!
!      i,j,g,n_refl :: INT
!      p,q,r :: VEC{INT}(3)
!      same :: BIN
!
!      n_refl = .xray_data.reflections.dim
!
!      list.create(n_refl)
!      list(1).element.append(1)
!
!      n_unique = 1
!
!      do i = 2,n_refl
!
!         p(1) = .xray_data.reflections(i).h1
!         p(2) = .xray_data.reflections(i).h2
!         p(3) = .xray_data.reflections(i).h3
!         
!         same = FALSE
!         
!         j = i - 1
!         main: do g = 1,.spacegroup.n_seitz
!            q(1) = .xray_data.reflections(j).h1
!            q(2) = .xray_data.reflections(j).h2
!            q(3) = .xray_data.reflections(j).h3
!            r = int(matmul(.spacegroup.seitz(1:3,1:3,g),q),kind=INT_KIND)
!            same = p.same_as(r) OR p.same_as(-r)
!            if (NOT same) cycle
!            exit main
!         end do main
!         
!         if (same) then
!            list(j).element.append(i)
!            list(i).element.append(j)
!          ! stdout.text("reflections "//trim(i.to_str)//" and "//trim(j.to_str)//" are the same")
!         else
!            n_unique = n_unique + 1
!            list(i).element.append(i)
!         end
!
!      end
!
!   end

!   make_list_of_Bijvoet_classes :: leaky, private
!   ! Make a list of the bijvoet classes. For each reflection i,
!   ! list(1:2)[i][j] gives the list of bijvoet equivalent 
!   ! reflections. Class one has determinant 1 for the Miller 
!   ! indices, class 2 has determinant -1. 
!      self :: IN
!
!   ENSURE(.xray_data.allocated,"no diffraction data")
!   ENSURE(.xray_data.reflections.allocated,"no reflection data")
!
!    ! list :: VEC{EVEC{EVEC{INT}}}@
!      n_refl,np,nm :: INT
!      i, j, g, sgn :: INT
!      signp,signm,skip :: VEC{BIN}@
!      listp,listm :: VEC{EVEC{INT}}@
!      hi,hj,hg :: VEC{INT}(3)
!      same :: BIN
!
!      n_refl = .xray_data.reflections.dim
!
!      ! Bijvoet pairs
!      signp.create(n_refl)
!      signm.create(n_refl)
!       skip.create(n_refl)
!
!      ! Sort into the two classes
!      np = 0
!      nm = 0
!      signp = FALSE
!      signm = FALSE
!      skip  = FALSE
!      do i = 1,n_refl
!         hi = .xray_data.reflections(i).miller_indices
!         where (hi==0) hi = 1
!         sgn = sign(1,product(hi))
!         if (sgn==1) then; signp(i) = TRUE
!         else;             signm(i) = TRUE
!         end
!      end
!
!      np = count(signp)
!      nm = count(signm)
!      if (np+nm/=n_refl) then
!         stdout.text("Two classes do not exhaust reflections!")
!         stdout.show("np     = ",np)
!         stdout.show("nm     = ",nm)
!         stdout.show("n_refl = ",n_refl)
!         DIE("Two classes do not exhaust reflections!")
!      end
!
!      ! Do +ve sign class first
!      skip = FALSE
!      listp.create(np)
!      np = 0
!      do i = 1,n_refl
!
!         if (skip(i)) cycle
!         if (signm(i)) cycle
!
!         ! Include reflection itself
!         np = np + 1
!         listp(np).element.append(i)
!        
!         do j = 2,n_refl
!
!            if (skip(j)) cycle
!            if (signm(j)) cycle
!
!            do g = 1,.spacegroup.n_seitz
!           
!               hj = .xray_data.reflections(j).miller_indices
!               hg = int(matmul(hj,.spacegroup.seitz(1:3,1:3,g)),kind=INT_KIND)
!           
!               ! Equivalent class j-Bijvoet refln? 
!               ! By symop "g"
!               same = hi.same_as(hg)
!               if (same) then
!                  listp(np).element.append(j)
!                  skip(j) = TRUE
!                  exit
!               end
!            end
!         end
!      end
!    ! listp.shrink(np)
!
!      stdout.text("List of +ve Bijovoet pairs")
!      do i = 1,np
!         stdout.show("i = ",i)
!         stdout.put(listp(i).element,by_column=TRUE)
!      end
!
!      ! Do -ve sign class first
!      skip = FALSE
!      listm.create(nm)
!      nm = 0
!      do i = 1,n_refl
!
!         if (skip(i)) cycle
!         if (signp(i)) cycle
!
!         ! Include reflection itself
!         nm = nm + 1
!         listm(nm).element.append(i)
!        
!         do j = 2,n_refl
!
!            if (skip(j)) cycle
!            if (signp(j)) cycle
!
!            do g = 1,.spacegroup.n_seitz
!           
!               hj = .xray_data.reflections(j).miller_indices
!               hg = int(matmul(hj,.spacegroup.seitz(1:3,1:3,g)),kind=INT_KIND)
!           
!               ! Equivalent class j-Bijvoet refln? 
!               ! By symop "g"
!               same = hi.same_as(hg)
!               if (same) then
!                  listm(nm).element.append(j)
!                  skip(j) = TRUE
!                  exit
!               end
!            end
!         end
!      end
!    ! listm.shrink(nm)
!
!      stdout.text("List of -ve Bijovoet pairs")
!      do i = 1,nm
!         stdout.show("i = ",i)
!         stdout.put(listm(i).element,by_column=TRUE)
!      end
!
!      ! Move the answers in
!    ! list.create(2)
!    ! call move_alloc(listp,list(1).element)
!    ! call move_alloc(listm,list(2).element)
!
!   end

!   is_Friedel_unmerged result (res)  :: PURE
!   ! Returns TRUE if the list of reflections is Friedel unmerged.
!   ! *Every* reflection must have matching Friedel pair.
!      self :: IN
!      res :: BIN
!
!   ENSURE(.xray_data.allocated,"no diffraction data")
!   ENSURE(.xray_data.reflections.allocated,"no reflection data")
!
!      res = .xray_data.reflections.is_Friedel_unmerged
!
!   end

!   expand_unique_reflections :: leaky
!   ! This routine will expand a unique list of reflections into a
!   ! complete redundant list of reflections.
!      self :: INOUT
!
!   ENSURE(.xray_data.allocated,"no diffraction data")
!   ENSURE(.xray_data.reflections.allocated,"no reflection data")
!
!      list :: VEC{EVEC{INT}}@
!      reflections :: VEC{REFLECTION}*
!      n_unique,n_refl,n_max,n,i,j,g :: INT
!      p,q,r :: VEC{INT}(3)
!      same :: BIN
!
!      ! Store the number of reflections
!      n_refl = .xray_data.reflections.dim
!
!      ! Make the list of equivalents, to detect any repetitions
!      .make_list_of_equivalents(list,n_unique,show=TRUE)
!      DIE_IF(n_unique/=n_refl,"the list of reflections is not unique")
!      list.destroy
!
!      ! Create the maximum possible list of total reflections
!      n_max = n_refl*.spacegroup.n_seitz
!      reflections.create(n_refl*.spacegroup.n_seitz)
!
!      ! Loop over remaining reflections
!      n = 0
!      do i = 1,n_refl
!
!        p = .xray_data.reflections(i).Miller_indices
!
!        ! Apply symmetry to this relflection
!        do g = 1,.spacegroup.n_seitz
!           r = int(matmul(.spacegroup.seitz(1:3,1:3,g),p),kind=INT_KIND)
!
!           ! Is it the same as another in the list?
!           same = FALSE
!           do j = 1,n-1
!              q = reflections(j).Miller_indices
!              same = q.same_as(r)
!              if (same) exit
!           end
!           if (NOT same) then
!              n = n + 1
!              DIE_IF(n>n_max,"more than expected number of reflections")
!              reflections(n) = .xray_data.reflections(i)
!              reflections(n).set_hkl(r)
!           end
!        end
!      end
!      reflections.shrink(n)
!      .xray_data.reflections.destroy
!      .xray_data.reflections => reflections
!   end

!  =============================
!  Wrappered reflection routines
!  =============================

   function F2_exp_scaled_corrected(self) result (res)
   ! The experimental structure factors squares, F_exp, on an absolute
   ! scale, and corrected for dispersion (if any).
   ! Method follows XD (2015) manual p. 45.
   ! See also Ibers & Hamilton (1964) ACta Cryst. 17 p781.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,size(self%xray_data%reflections)) :: res
   ENSURE(allocated(self%xray_data),"CRYSTAL:F2_exp_scaled_corrected ... no xray_data")
      res = F_exp_scaled_corrected_(self)
      res = res*res
   end function


!   F_exp result (res) :: PURE
!   ! The experimental structure factors
!      self :: IN
!      res :: VEC{REAL}(.xray_data.reflections.dim)
!
!   ENSURE(.xray_data_exists, "no diffraction data")
!
!      res = .xray_data.reflections(:).F_exp
!
!   end

   NON_MPI_PURE function F_exp_scaled_corrected(self) result (res)
   ! The experimental structure factors F_exp on an absolute scale,
   ! and corrected for dispersion (if any).
   ! Method follows XD (2015) manual p. 45.
   ! See also Ibers & Hamilton (1964) Acta Cryst. 17 p781.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,size(self%xray_data%reflections)) :: res
      VEC(CPX,:), allocatable :: Fc
   ENSURE(allocated(self%xray_data),"CRYSTAL:F_exp_scaled_corrected ... no xray_data")
      if (self%xray_data%add_dispersion_to_F_calc) then

         ! Get dispersion correction
         call create_(Fc,size(self%xray_data%reflections))
         call add_dispersion_correction_(self,Fc)

         ! Get complex F_exp on absolute scale & remove dispersion
         Fc = F_exp_on_absolute_scale_(self%xray_data)-abs(Fc)

         ! Get corrected F_exp
         res = abs(Fc)

         ! Clean
         call destroy_(Fc)
      else

         ! Get F_exp on absolute scale
         res = F_exp_on_absolute_scale_(self%xray_data)
      end if
   end function

   PURE function xyz_asymmetric_unit_geometry(self) result (res)
   ! Return the asymmetric unit geometry in cartesian coordinates.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,3,self%n_asymmetric_unit_atoms) :: res
   ENSURE(allocated(self%asymmetric_unit_geometry),"CRYSTAL:xyz_asymmetric_unit_geometry ... no asymmetric unit")
      res = matmul(self%unit_cell%direct_mx,self%asymmetric_unit_geometry)
   end function


!  =================
!  Structure factors
!  =================

   subroutine remove_dispersion_if_asked(self)
   ! Remove the anomalous dispersion from F_exp, to make F_corr, and from
   ! F_calc -- the XD/Shelx convention -- if that is what was asked for.
   ! NOTE: call this immediately after the scale factor is optimised, and
   ! only there. It needs the scale, and it needs F_calc to still carry the
   ! dispersion that make_F_calc_from added, because remove_anom_from_F_exp
   ! takes its phase from F_calc. Exactly one call per rebuild of F_calc:
   ! remove_anom_from_F_calc subtracts F_disp from whatever F_calc holds, so
   ! calling it twice subtracts twice.
      type(CRYSTAL_TYPE), INOUT :: self
   ENSURE(allocated(self%xray_data),"CRYSTAL:remove_dispersion_if_asked ... no xray_data")
      if (self%xray_data%remove_dispersion_from_F_exp) then
         call remove_anom_from_F_exp_(self%xray_data%reflections,self%xray_data%scale_factor)
         call remove_anom_from_F_calc_(self%xray_data%reflections)

         ! New scale, for the corrected pair
         call make_F_predicted_(self%xray_data,update_n_param=FALSE)
      end if
   end subroutine

   subroutine make_F_predicted_0(self,ff)
   ! Make and set the predicted structure factors, F_pred, from
   ! "ff", the unique *fragment* atom form factors for the
   ! symmetry-generated related k points
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(CPX,:,:), IN :: ff
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_F_predicted_0 ... no data")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_F_predicted_0 ... no fragment info")

      ! Make F_predicted, F_calc & derivatives
      call set_F_calc_from_(self,ff)

      ! Optimise scale, extinction & set GoF2(F)
      call make_F_predicted_(self%xray_data)

      ! Remove dispersion from F_exp, if that convention was asked for
      call remove_dispersion_if_asked_(self)
   end subroutine

   subroutine make_F_predicted_1(self,prune)
   ! Make and set the predicted structure factors, F_pred, reading the
   ! temperature independent aspherical atomic form factors from disk.
      type(CRYSTAL_TYPE), INOUT :: self
      BIN, optional, IN :: prune
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_F_predicted_1 ... no data")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_F_predicted_1 ... no fragment info")

      ! Make F_predicted, F_calc & derivatives
      call set_F_calc_from_(self,prune)  ! NOT PURE

      ! Optimise scale, extinction & set GoF2(F)
      call make_F_predicted_(self%xray_data)

      ! Remove dispersion from F_exp, if that convention was asked for
      call remove_dispersion_if_asked_(self)
   end subroutine

   subroutine set_F_calc_from_0(self,ff,prune)
   ! Set the predicted structure factors, F_pred, from "ff", the
   ! unique *fragment* atom form factors for the
   ! symmetry-generated related k points
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(CPX,:,:), IN :: ff
      BIN, optional, IN :: prune
      VEC(CPX,:), allocatable :: Fc
      BIN :: do_prune,has_zeros,show
   ENSURE(allocated(self%xray_data),"CRYSTAL:set_F_calc_from_0 ... no data")
   ENSURE(self%fragment_info_made,"CRYSTAL:set_F_calc_from_0 ... no fragment info")
      do_prune = FALSE
      if (present(prune)) do_prune = prune
      call create_(Fc,size(self%xray_data%reflections))

      ! Make F_predicted, F_calc & derivatives
      call make_F_calc_(self,Fc,ff)

      ! Set the calculated SF's
      has_zeros = any(abs(Fc)<=self%xray_data%F_calc_cutoff)
      call set_F_calc_(self%xray_data%reflections,Fc)
      call destroy_(Fc)
      if (do_prune AND has_zeros) then
         WARN("CRYSTAL:set_F_calc_from_0 ... zero or small F_calc's are being pruned")
         show = self%xray_data%show_rejects
         call set_show_rejects_(self%xray_data,TRUE)
         call prune_zero_F_calcs_(self%xray_data)
         call set_show_rejects_(self%xray_data,show)
      end if
   end subroutine

   subroutine set_F_calc_from_1(self,prune)
   ! Set the predicted structure factors, F_pred, reading the
   ! temperature independent structure factors from disk.
      type(CRYSTAL_TYPE), INOUT :: self
      BIN, optional, IN :: prune
      VEC(CPX,:), allocatable :: Fc
      BIN :: do_prune,has_zeros,show
   ENSURE(allocated(self%xray_data),"CRYSTAL:set_F_calc_from_1 ... no data")
   ENSURE(self%fragment_info_made,"CRYSTAL:set_F_calc_from_1 ... no fragment info")
      do_prune = FALSE
      if (present(prune)) do_prune = prune
      call create_(Fc,size(self%xray_data%reflections))

      ! Make F_predicted, F_calc & derivatives
      call make_F_calc_(self,Fc)

      ! Set the calculated SF's
      has_zeros = any(abs(Fc)<=self%xray_data%F_calc_cutoff)
      call set_F_calc_(self%xray_data%reflections,Fc)
      call destroy_(Fc)
      if (do_prune AND has_zeros) then
         WARN("CRYSTAL:set_F_calc_from_1 ... zero or small F_calc's are being pruned")
         show = self%xray_data%show_rejects
         call set_show_rejects_(self%xray_data,TRUE)
         call prune_zero_F_calcs_(self%xray_data)
         call set_show_rejects_(self%xray_data,show)
      end if
   end subroutine

   subroutine make_F_calc_0(self,Fc,ff)
   ! Make the calculated structure factors "Fc", where "ff" are
   ! the unique *fragment* atom form factors for the symmetry
   ! generated related k points
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(CPX,:), OUT :: Fc
      MAT(CPX,:,:), IN :: ff
      VEC(CPX,:), allocatable :: sf_e
      INT :: n_k
   ENSURE(size(ff,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_F_calc_0 ... ff: wrong dim1")
   ENSURE(size(ff,2)==size(self%asymmetric_unit_atom),"CRYSTAL:make_F_calc_0 ... ff: wrong dim2")
   ENSURE(size(Fc)==size(self%xray_data%reflections),"CRYSTAL:make_F_calc_0 ... wrong size, Fc")
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_F_calc_0 ... no data")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_F_calc_0 ... no fragment info")
      n_k = n_unique_SF_k_pts_(self)

      ! Symmetry-generated T-indepedent structure factors
      call create_(sf_e,n_k)

      ! Make them
      call make_unique_sf_(self,sf_e,ff)

      ! Sum the symmetry-generated SF's to get
      ! the actual structure factors Fc
      call make_F_calc_from_(self,sf_e,Fc)
      call destroy_(sf_e)
   end subroutine

   subroutine make_F_calc_1(self,Fc)
   ! Make the calculated structure factors "Fc", where "ff" are
   ! the unique *fragment* atom form factors for the symmetry
   ! generated related k points
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(CPX,:), OUT :: Fc
      VEC(CPX,:), allocatable :: sf_e
      INT :: n_k
   ENSURE(size(Fc)==size(self%xray_data%reflections),"CRYSTAL:make_F_calc_1 ... wrong size, Fc")
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_F_calc_1 ... no data")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_F_calc_1 ... no fragment info")
      n_k = n_unique_SF_k_pts_(self)

      ! Symmetry-generated T-indepedent structure factors
      call create_(sf_e,n_k)

      ! Make them
      call make_unique_sf_(self,sf_e)

      ! Sum the symmetry-generated SF's to get
      ! the actual structure factors Fc
      call make_F_calc_from_(self,sf_e,Fc)
      call destroy_(sf_e)
   end subroutine

   subroutine make_F_predicted_from(self,sf_e)
   ! Make and set the predicted structure factors, F_pred, from
   ! "sf_e", the aspherical temperature-independent form factors for
   ! every symmetry-generated structure factor.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(CPX,:), IN :: sf_e
      VEC(CPX,:), allocatable :: Fc
   ENSURE(size(sf_e,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_F_predicted_from ... wrong size, sf_e")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_F_predicted_from ... no fragment info")
      call create_(Fc,size(self%xray_data%reflections))

      ! Make the calculated SF's
      call make_F_calc_from_(self,sf_e,Fc)

      ! Set the calculated SF's
      call set_F_calc_(self%xray_data%reflections,Fc)
      call destroy_(Fc)

      ! Optimise scale, extinction & set GoF2(F)
      call make_F_predicted_(self%xray_data)

      ! Remove dispersion from F_exp, if that convention was asked for
      call remove_dispersion_if_asked_(self)
   end subroutine

   subroutine set_F_calc_from_2(self,sf_e)
   ! Set the predicted structure factors, F_pred, from "sf_e", the
   ! aspherical temperature-independent form factors for every
   ! symmetry-generated structure factor.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(CPX,:), IN :: sf_e
      VEC(CPX,:), allocatable :: Fc
   ENSURE(size(sf_e,1)==n_unique_SF_k_pts_(self),"CRYSTAL:set_F_calc_from_2 ... wrong size, sf_e")
   ENSURE(self%fragment_info_made,"CRYSTAL:set_F_calc_from_2 ... no fragment info")
      call create_(Fc,size(self%xray_data%reflections))
      call make_F_calc_from_(self,sf_e,Fc)
      call destroy_(Fc)
   end subroutine

   subroutine make_F_calc_from(self,sf_e,Fc)
   ! Make and set the predicted structure factors, F_pred, from
   ! "sf_e", the aspherical temperature-independent form factors for
   ! every symmetry-generated structure factor.
   ! NOTE: This method was first described by Rollet (1970) in
   !       "Crystallographic Computing", ed. F.R. Ahmed, p. 167-84.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(CPX,:), IN :: sf_e
      VEC(CPX,:), OUT :: Fc
      VEC(CPX,:), allocatable :: Fa
   ENSURE(size(sf_e,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_F_calc_from ... wrong size, sf_e")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_F_calc_from ... no fragment info")

      ! Sum the symmetry-generated SF's to get
      ! the actual structure factors Fc
      call sum_unique_sf_(self%spacegroup,Fc,sf_e,self%xray_data%reflections)

      ! Include dispersion?
      if (correct_dispersion_(self%xray_data)) then

         ! Get dispersion correction
         call create_(Fa,size(self%xray_data%reflections))
         call get_dispersion_correction_(self,Fa)
         call set_F_disp_(self%xray_data%reflections,Fa)

         ! Add dispersion into Fc
         Fc = Fc+Fa
         call destroy_(Fa)
      end if

      ! Set the calculated SF's
   ! has_zeros = any(abs(Fc)<=.xray_data.F_calc_cutoff)
   ! WARN_IF(has_zeros,"there are zero or small F_calc's; do you need to prune them?")
      call set_F_calc_(self%xray_data%reflections,Fc)
   end subroutine

   subroutine make_F_calc_derivs_0(self,dFc,ff)
   ! Make "dFc", the derivatives of the calculated structure factors
   ! with respect to positional and ADP parameters, using "ff", the
   ! temperature-independent atomic form factors. Also set the
   ! calculated and prediected structure factors.
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(CPX,:,:), OUT :: dFc
      MAT(CPX,:,:), IN :: ff
      MAT(CPX,:,:), allocatable :: sf_d
      VEC(CPX,:), allocatable :: sf_e
      INT :: n_k,n_p
   ENSURE(size(ff,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_F_calc_derivs_0 ... ff: wrong dim1")
   ENSURE(size(ff,2)==size(self%asymmetric_unit_atom),"CRYSTAL:make_F_calc_derivs_0 ... ff: wrong dim2")
   ENSURE(size(dFc,1)==size(self%xray_data%reflections),"CRYSTAL:make_F_calc_derivs_0 ... dFc: wrong dim1")
   ENSURE(size(dFc,2)==no_of_pADPs_(self%asymmetric_unit_atom),"CRYSTAL:make_F_calc_derivs_0 ... dFc: wrong dim2")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_F_calc_derivs_0 ... no fragment info")

      ! Constants
      n_k = n_unique_SF_k_pts_(self)
      n_p = no_of_pADPs_(self%asymmetric_unit_atom)

      ! Symmetry generated structure factors
      call create_(sf_d,n_k,n_p)  ! Symmetry generated SF derivatives
      call create_(sf_e,n_k)  ! Symmetry generated SF's

      ! Make the symmetry-generated structure factors and their
      ! derivatives including ADPs but no dispersion.
      call make_unique_sf_derivs_(self,sf_e,sf_d,ff)

      ! Make and set F_pred with scale & extinction.
      call reset_F_corr_to_F_exp_(self%xray_data%reflections)  ! We fit to F_corr
      call make_F_predicted_from_(self,sf_e)  ! ... and applies the dispersion convention
      call destroy_(sf_e)

   ! .xray_data.PUT:put_F_statistics
   ! .xray_data.PUT:put

      ! Sum the symmetry-generated SF's to get
      ! the derivative structure factors dFc
      call sum_unique_sf_derivs_(self%spacegroup,dFc,sf_d,self%xray_data%reflections)
      call destroy_(sf_d)

      ! Add derivative of anomalous contribution to |SF|, dFc
      ! Only if F_calc has anomalous dispersion added into it.
      if (self%xray_data%add_dispersion_to_F_calc) then
         call add_D_dispersion_correction_(self,dFc)
      end if
   end subroutine

   subroutine make_F_calc_derivs_1(self,dFc)
   ! Make "dFc", the derivatives of the calculated structure factors
   ! with respect to positional and ADP parameters, reading
   ! temperature-independent structure factors from disk. Also set the
   ! calculated and prediected structure factors.
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(CPX,:,:), OUT :: dFc
      MAT(CPX,:,:), allocatable :: sf_d
      VEC(CPX,:), allocatable :: sf_e
      INT :: n_k,n_p
   ENSURE(size(dFc,1)==size(self%xray_data%reflections),"CRYSTAL:make_F_calc_derivs_1 ... dFc: wrong dim1")
   ENSURE(size(dFc,2)==no_of_pADPs_(self%asymmetric_unit_atom),"CRYSTAL:make_F_calc_derivs_1 ... dFc: wrong dim2")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_F_calc_derivs_1 ... no fragment info")

      ! Constants
      n_k = n_unique_SF_k_pts_(self)
      n_p = no_of_pADPs_(self%asymmetric_unit_atom)

      ! Symmetry generated structure factors
      call create_(sf_d,n_k,n_p)  ! Symmetry generated SF derivatives
      call create_(sf_e,n_k)  ! Symmetry generated SF's

      ! Make the symmetry-generated structure factors and their
      ! derivatives including ADPs but no dispersion.
      call make_unique_sf_derivs_(self,sf_e,sf_d)  ! processor 1 stops here

      ! Make and set F_pred with scale & extinction.
      call reset_F_corr_to_F_exp_(self%xray_data%reflections)  ! We fit to F_corr
      call make_F_predicted_from_(self,sf_e)  ! ... and applies the dispersion convention
      call destroy_(sf_e)

      ! Sum the symmetry-generated SF's to get
      ! the derivative structure factors dFc
      call sum_unique_sf_derivs_(self%spacegroup,dFc,sf_d,self%xray_data%reflections)
      call destroy_(sf_d)

      ! Add derivative of anomalous contribution to |SF|, dFc
      ! Only if F_calc has anomalous dispersion added into it.
      if (self%xray_data%add_dispersion_to_F_calc) then
         call add_D_dispersion_correction_(self,dFc)
      end if
   end subroutine

   subroutine make_F_calc_derivs_for_atom(self,a,dFc,ff)
   ! Make "dFc", the derivatives of the calculated structure factors
   ! with respect to positional and ADP parameters for asymmetric unit
   ! atom "a", using "ff", the temperature-independent atomic form
   ! factors. 
   ! NOTE: this routine sets no F_calc, scale factor or F_pred, so neither
   ! dispersion convention can be applied here -- hence the DIE. Nothing
   ! calls it today. See TASKS_AND_HISTORY.md.
      type(CRYSTAL_TYPE), INOUT :: self
      INT, IN :: a
      MAT(CPX,:,:), OUT :: dFc
      VEC(CPX,:), IN :: ff
      MAT(CPX,:,:), allocatable :: sf_d
      VEC(CPX,:), allocatable :: sf_e
      INT :: n_k,n_p
   DIE_IF(correct_dispersion_(self%xray_data),"CRYSTAL:make_F_calc_derivs_for_atom ... cannot apply a dispersion convention in the per-atom derivative path")
   ENSURE(size(ff,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_F_calc_derivs_for_atom ... ff: wrong dim1")
   ENSURE(size(dFc,1)==size(self%xray_data%reflections),"CRYSTAL:make_F_calc_derivs_for_atom ... dFc: wrong dim1")
   ENSURE(size(dFc,2)==no_of_pADPs_(self%asymmetric_unit_atom(a)),"CRYSTAL:make_F_calc_derivs_for_atom ... dFc: wrong dim2")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_F_calc_derivs_for_atom ... no fragment info")

      ! Constants
      n_k = n_unique_SF_k_pts_(self)
      n_p = no_of_pADPs_(self%asymmetric_unit_atom(a))

      ! Symmetry generated structure factors
      call create_(sf_d,n_k,n_p)  ! Symmetry generated SF derivatives
      call create_(sf_e,n_k)  ! Symmetry generated SF's

      ! Make the symmetry-generated structure factors and their
      ! derivatives including ADPs but no dispersion.
      call make_unique_sf_derivs_for_atom_(self,a,sf_e,sf_d,ff)
      call destroy_(sf_e)  ! not used here, only the derivatives are

      ! Sum the symmetry-generated SF's to get
      ! the derivative structure factors dFc
      call sum_unique_sf_derivs_(self%spacegroup,dFc,sf_d,self%xray_data%reflections)
      call destroy_(sf_d)

      ! Add derivative of anomalous contribution to |SF|, dFc
      ! Only if F_calc has anomalous dispersion added into it.
      if (self%xray_data%add_dispersion_to_F_calc) then
         call add_D_dispersion_correction_for_atom_(self,a,dFc)
      end if
   end subroutine

   subroutine symmetrize_FFs(self,ff)
   ! Symmetrize the form factor for unique symmetry operators using
   ! the stabilizers, using spacegroup.symmetrize_unique_SFs 
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable, INOUT :: ff
      VEC(CPX,:), allocatable :: sf_a
      INT :: a
      REAL :: diff,diffmax
      VEC(INT,3) :: diffmax_hkl
   ENSURE(self%spacegroup%analyzed,"CRYSTAL:symmetrize_FFs ... spacegroup not analyzed")
   ENSURE(size(ff,2)==self%n_unique_frag_atoms,"CRYSTAL:symmetrize_FFs ... Number of unique fragment atoms: wrong size")
      if (NOT allocated(self%asymmetric_FF_symmetrization_rss)) then
         call create_(self%asymmetric_FF_symmetrization_rss,size(ff,2))
      end if
      if (NOT allocated(self%asymmetric_FF_symmetrization_rmax)) then
         call create_(self%asymmetric_FF_symmetrization_rmax,size(ff,2))
      end if
      if (NOT allocated(self%asymmetric_FF_symmetrization_rmax_hkl)) then
         call create_(self%asymmetric_FF_symmetrization_rmax_hkl,size(ff,2),3)
      end if
      do a = 1,self%n_asymmetric_unit_atoms
         sf_a = ff(:,a)
         ! Symmetrize
         call symmetrize_unique_SFs_(self%spacegroup,sf_a,self%asym_atom_stabilizer(a)%element,self%xray_data%reflections,diff,diffmax,diffmax_hkl)
         self%asymmetric_FF_symmetrization_rss(a) = diff
         self%asymmetric_FF_symmetrization_rmax(a) = diffmax
         self%asymmetric_FF_symmetrization_rmax_hkl(a,:) = diffmax_hkl
         ! Overwrite the form factor
         ff(:,a) = sf_a
      end do
      call destroy_(sf_a)
   end subroutine


!  ============================
!  Structure refinement methods
!  ============================

   subroutine LS_structure_fit_0(self,output,results)
   ! Least-squares fit the the positions and ADPs of the unique
   ! fragment atoms so that the structure factors obtained from the
   ! static *aspherical* *atomic* form factor, read from disk, match
   ! the experimental structure factors. The structure factors are for
   ! all symmetry genetrated structure factors, and only for the
   ! *unique* .fragment atom's which correspond to but are not
   ! necessarily the same as the .asymmetric_unit_atom's.
   !
   ! NOTE: this routine does only one cycle of fitting with the
   !       current density matrix i.e. this is a "rigid atom fit".
   !
   ! NOTE: It's tricky exactly which atom's pADPs are used to do the
   !       refinement:
   !
   ! * Structure factors are only for *unique* fragment atoms since
   !   the QM wavefunction refers to the fragment_atoms.
   !
   ! * However, asymmetric unit atoms are used to "store" the updating
   !   geometry; thus unique fragment atom pADPs and shifts are
   !   calculated (and derivatives with respect to these unique
   !   fragment atom positions and ADPs) but these parameters and
   !   shifts are later transformed back into asymmetric unit.
      type(CRYSTAL_TYPE), INOUT :: self
      BIN, IN :: output
      BIN, IN :: results
      MAT(REAL,:,:), allocatable :: dF
   ENSURE(allocated(self%xray_data),"CRYSTAL:LS_structure_fit_0 ... no data")
   ENSURE(self%fragment_info_made,"CRYSTAL:LS_structure_fit_0 ... no fragment info")

      ! Create refinement arrays & initialize (leaky)
      ! - asymmetric atom and fragment atom pADPs are set.
      call initialize_fit_data_(self)

      ! Place asym atom position/ADPs in vector .xray_data.X_fit
      ! - asymmetric_unit_atom keeps the pADPs
      call put_pADP_vector_to_(self%asymmetric_unit_atom,self%xray_data%X_fit)

      ! If refining, store a copy of the pADPs in .xray_data.X_ref
      if (self%xray_data%refine_structure) self%xray_data%X_ref = self%xray_data%X_fit

      ! Banner, options/switches, initial parameters,
      ! start banner, and iteration table header.
      if (output) then
         call put_fit_header_info_(self)
      end if

      ! Begin rigid-atom fit loop
      do

         ! Set asym atom positions/ADPs from param vector
      ! .asymmetric_unit_atom.set_pADP_vector_to(.xray_data.X_fit)

         ! Get unique *fragment* atom shifts in .xray_data.dX ...
         call get_parameter_shifts_(self)  ! .data

         ! Change/store unique frag atom -> asym atom shifts
         ! Change/store the covariance matrix too.
         call set_asym_from_ufrag_shifts_(self)  ! .xray_data.dX now for asym atoms
         call set_asym_from_ufrag_cov_mx_(self)

         ! Stop symmetry breaking
         ! NOTE: we should apply this to the gradient *not*
         ! the shift to make sure the shift maintains symmetry
         call stabilize_asym_atom_shifts_(self)

         ! Store old asym-atom parameter vector.
         ! Add shift dA to get new .xray_data.X_fit
         call update_fit_parameters_(self%xray_data)
         call set_pADP_vector_to_(self%asymmetric_unit_atom,self%xray_data%X_fit)

         ! Set the fit errors e.g. max(dX/esd)
         ! Copy them to the asymmetric unit atoms.
         call update_fit_esds_(self)
         call set_pADP_errors_to_(self%asymmetric_unit_atom,self%xray_data%covariance_mx,self%xray_data%refine_H_U_iso)

         ! Set ALL fragment_atom positions/ADPs from asym atoms HERE
         ! >>>fragment_atom must be the same as molecule.atom
         call create_(dF,3,self%n_fragment_atoms)
         call set_frag_from_asym_pADPs_(self,dF)

         ! Update structure factors, the unique (rigid) frag atom SF's.
         ! Use the frag atom position shifts "dF".
         call shift_update_ff_(self,dF)
         call destroy_(dF)

         ! Put the iteration results
         if (output) then
            call put_fit_table_body_(self%xray_data)
         end if

         ! Finished?
         call update_fit_info_(self%xray_data)
         if (self%xray_data%fit_finished) exit

         ! Prune? The next cycle's shift_update_ff reads the files, so
         ! they must be cut down with the list.
         if (is_F_calc_prunable_(self%xray_data)) then
            call prune_xray_data_(self)
         end if
      end do

      ! Table footer
      if (output) then
         call put_footer_(self%xray_data%fit_table)
      end if

      ! Set the fragment covariances from cov matrix
      call set_fragment_esds_(self)

      ! Final results
      if (results) then
         call put_fit_results_(self)
      end if

      ! Print out bond length, angle, torsion tables with errors
      if (output) then
         call put_bond_info_with_errors_(self)
      end if

      ! Finally: remake .asymmetric_unit_atom/unit_cell geometry from
      ! .fragment_atom, which *must* be assigned since it must point
      ! to molecule.atom
      call destroy_(self%asymmetric_unit_atom)
      call destroy_(self%asymmetric_unit_geometry)
      self%asymmetric_unit_source = "tonto-ls-structure-fit"
      call update_fragment_info_(self,self%fragment_atom)
   end subroutine

   subroutine LS_structure_fit_1(self,ff,output,results)
   ! Least-squares fit the the positions and ADPs (pADPs) of the
   ! unique fragment atoms in .crystal.asymmetric_unit_atoms so that
   ! the Goodness of Fit (GoF) between predicted and observed
   ! structure factors is minimised. In "ff" are the *aspherical*
   ! atomic form factors for all symmetry genetrated structure
   ! factors (first index) for the *unique* .fragment_atoms which
   ! correspond to .asymmetric_unit_atoms (second index). The unique
   ! fragment atoms are associated to the QM fragment, so they are
   ! not necessarily the same as the .asymmetric_unit_atoms which
   ! usually come from an external CIF.
   !
   ! NOTE: this routine does only *one* cycle of fitting with the
   !       current density matrix i.e. this is a "rigid atom fit".
   !
   ! NOTE: It's tricky exactly which atom's pADPs are used to do the
   !       refinement:
   !
   ! * Structure factors are calculated only for *unique* fragment
   !   atoms since the QM wavefunction refers to the fragment_atoms.
   !   cf. the description of "ff" above.
   !
   ! * However, asymmetric_unit_atoms are used to "store" the updating
   !   geometry; thus unique fragment atom pADPs and shifts are
   !   calculated (and derivatives with respect to these unique
   !   fragment atom positions and ADPs) but these parameters and
   !   shifts are later symmetry transformed into the asymmetric unit.
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(CPX,:,:), INOUT :: ff
      BIN, IN :: output
      BIN, IN :: results
      MAT(REAL,:,:), allocatable :: dF
   ENSURE(size(ff,1)==n_unique_SF_k_pts_(self),"CRYSTAL:LS_structure_fit_1 ... ff: wrong dim1")
   ENSURE(size(ff,2)==self%n_asymmetric_unit_atoms,"CRYSTAL:LS_structure_fit_1 ... ff: wrong dim2")
   ENSURE(allocated(self%xray_data),"CRYSTAL:LS_structure_fit_1 ... no data")
   ENSURE(self%fragment_info_made,"CRYSTAL:LS_structure_fit_1 ... no fragment info")

      ! Create refinement arrays & initialize (leaky)
      ! - asymmetric atom and fragment atom pADPs are set.
      call initialize_fit_data_(self)

      ! Place asym atom position/ADPs in vector .xray_data.X_fit
      ! - asymmetric_unit_atom keeps the pADPs
      ! - unique frag atoms are *refined*
      call put_pADP_vector_to_(self%asymmetric_unit_atom,self%xray_data%X_fit)

      ! Store a copy of the pADPs in:  .xray_data.X_ref
      if (self%xray_data%refine_structure) self%xray_data%X_ref = self%xray_data%X_fit

      ! Banner, options/switches, initial parameters,
      ! start banner, and iteration table header.
      if (output) then
         call put_fit_header_info_(self)
      end if

      ! Rigid-atom fit loop
      do

         ! Get unique *fragment* atom shifts in .xray_data.dX ...
         call get_parameter_shifts_(self,ff)  ! .data

         ! Change/store unique frag atom -> asym atom shifts
         ! Change/store the covariance matrix too.
         call set_asym_from_ufrag_shifts_(self)  ! .xray_data.dX now for asym atoms
         call set_asym_from_ufrag_cov_mx_(self)

         ! Stop symmetry breaking
         ! NOTE: we should apply this to the gradient *not*
         ! the shift to make sure the shift maintains symmetry
         call stabilize_asym_atom_shifts_(self)

         ! Store old asym-atom parameter vector.
         ! Add shift dA to get new .xray_data.X_fit
         call update_fit_parameters_(self%xray_data)
         call set_pADP_vector_to_(self%asymmetric_unit_atom,self%xray_data%X_fit)

         ! Set the fit errors e.g. max(dX/esd)
         ! Copy them to the asymmetric unit atoms.
         call update_fit_esds_(self)
         call set_pADP_errors_to_(self%asymmetric_unit_atom,self%xray_data%covariance_mx,self%xray_data%refine_H_U_iso)

         ! Set ALL fragment_atom positions/ADPs from asym atoms HERE
         ! >>> fragment_atom must be the same as molecule.atom 
         ! >>> Except for the pHAR (Sorry Dylan. Kang). 
         !     In pHAR, molecule.atom are from Crystal23 xml, and fixed
         call create_(dF,3,self%n_fragment_atoms)
         call set_frag_from_asym_pADPs_(self,dF)

         ! Update "ff", the unique (rigid) frag atom FF's.
         ! Use the frag atom position shifts "dF".
         call shift_update_ff_(self,ff,dF)
         call destroy_(dF)

         ! Put the iteration results
         if (output) then
            call put_fit_table_body_(self%xray_data)
         end if

         ! Finished?
         call update_fit_info_(self%xray_data)
         if (self%xray_data%fit_finished) exit
      end do

      ! Table footer
      if (output) then
         call put_footer_(self%xray_data%fit_table)
      end if

      ! Set the fragment covariances from asymm cov matrix
      ! If you want errors to be right *during* fit cycles
      ! you must call this inside the above loop!
      call set_fragment_esds_(self)

      ! Final results
      if (results) then
         call put_fit_results_(self)
      end if

      ! Print out bond length, angle tables with errors
      ! And other error analysis information
      if (output) then
         call put_correlations_(self)
         if (allocated(self%asymmetric_FF_symmetrization_rss)) call put_asymmetric_FF_symmetrization_rss_(self)
         call put_bond_info_with_errors_(self)
         call put_X_data_analysis_(self)
      end if

      ! Finally: remake .asymmetric_unit_atom/unit_cell geometry 
      ! from the .fragment_atom. A bit crazy since these held
      ! the refined geometry. Think of it as a refresh for crystal.
      call destroy_(self%asymmetric_unit_atom)
      call destroy_(self%asymmetric_unit_geometry)
      self%asymmetric_unit_source = "tonto-ls-structure-fit"
      call update_fragment_info_(self,self%fragment_atom)
   end subroutine


!  Refinement initialization

   subroutine initialize_fit_data(self)
   ! Initialise a rigid-atom fit
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      INT :: f,a,n_p,n_f
   ENSURE(allocated(self%xray_data),"CRYSTAL:initialize_fit_data ... no diffraction data")
   ENSURE(self%fragment_info_made,"CRYSTAL:initialize_fit_data ... no fragment info")

      ! Create space for asymmetric unit atom pADPs (leaky)
      call reset_pADPs_and_errors_(self%asymmetric_unit_atom,self%xray_data%refine_3rd_order_for_atom,self%xray_data%refine_4th_order_for_atom)

      ! Create space for the fragment_atom pADPs (leaky)
      do f = 1,self%n_fragment_atoms
         a = self%asym_atom_for_frag_atom(f)
         n_p = no_of_pADPs_(self%asymmetric_unit_atom(a))
         call reset_pADPs_and_errors_(self%fragment_atom(f),n_p)
      end do

      ! Set fragment_atom pADPs from asymmetric unit atom pADPs
      call set_frag_from_asym_pADPs_(self)

      ! Set the no. of parameters
      n_p = no_of_pADPs_(self%asymmetric_unit_atom)
      n_f = no_of_pADPs_(self%fragment_atom)

      ! Create refinement arrays & initialize (leaky)
      labels = tag_pADP_labels_(self%asymmetric_unit_atom)
      call initialize_fit_data_(self%xray_data,n_p,n_f,labels)

      ! The map from refined parameters to pADPs (leaky)
      call make_refinement_jacobian_(self)

      ! The parameters themselves; a TLS model puts the atoms on it
      call set_refinement_parameters_(self)
   end subroutine

   subroutine initialize_refinement_data(self)
   ! Initialise a full refinement.
   ! . For each atom set the pADP parameter vectors
   ! . Set refinement stat data & output tables
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      INT :: n_p
   ENSURE(allocated(self%xray_data),"CRYSTAL:initialize_refinement_data ... no diffraction data")

      ! Create space for parameters (leaky)
      call reset_pADPs_and_errors_(self%asymmetric_unit_atom,self%xray_data%refine_3rd_order_for_atom,self%xray_data%refine_4th_order_for_atom)

      ! Set the no. of parameters
      n_p = no_of_pADPs_(self%asymmetric_unit_atom)

      ! Create refinement arrays & initialize (leaky)
      labels = tag_pADP_labels_(self%asymmetric_unit_atom)
      call initialize_refinement_data_(self%xray_data,n_p,labels)

      ! The map from refined parameters to pADPs (leaky)
      call make_refinement_jacobian_(self)
   end subroutine

   PURE subroutine set_fragment_U_high(self,U_high)
   ! Set .fragment_U_high, the stiff-mode U of each atom of the TLS body
   ! (3,3,n), bohr^2: the fixed part of a TLS model.
      type(CRYSTAL_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), IN :: U_high
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:set_fragment_U_high ... no fragment atoms")
   ENSURE(size(U_high,3)==n_TLS_atoms_(self),"CRYSTAL:set_fragment_U_high ... one U_high per TLS body atom")
      call destroy_(self%fragment_U_high)
      self%fragment_U_high = U_high
   end subroutine

   PURE subroutine set_soft_modes(self,V,a0,freq,temperature)
   ! Set the soft modes whose amplitudes a TLS model refines: "V" the
   ! displacement of each TLS body atom per unit normal coordinate,
   ! (3 x n_TLS_atoms, n_soft); "a0" their harmonic <Q^2>, negative for
   ! an imaginary mode; "freq" their frequencies, cm^-1, negative for
   ! imaginary; and the "temperature" (K) a0 is for.
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: V
      VEC(REAL,:), IN :: a0,freq
      REAL, IN :: temperature
   ENSURE(size(V,1)==3*n_TLS_atoms_(self),"CRYSTAL:set_soft_modes ... V: one row per TLS body atom coordinate")
   ENSURE(size(a0)==size(V,2) AND size(freq)==size(V,2),"CRYSTAL:set_soft_modes ... one a0 and frequency per mode")
      call destroy_(self%soft_mode_vectors)
      call destroy_(self%soft_mode_harmonic)
      call destroy_(self%soft_mode_frequency)
      self%soft_mode_vectors = V
      self%soft_mode_harmonic = a0
      self%soft_mode_frequency = freq
      self%soft_mode_temperature = temperature
   end subroutine

   pure function n_TLS_atoms(self) result (res)
   ! The number of atoms in the TLS body: the leading fragment atoms in the
   ! group of the first one. For a molecule that is every fragment atom; for
   ! a cluster made by create_cluster it is the central molecule.
      type(CRYSTAL_TYPE), IN :: self
      INT :: res
      INT :: g,n_f
      n_f = size(self%fragment_atom)
      res = n_f
      g = self%fragment_atom(1)%group
      do res = 1,n_f
         if (self%fragment_atom(res)%group/=g) exit
      end do
      res = res-1
   end function

   pure function n_soft_modes(self) result (res)
   ! The number of soft modes in the TLS model.
      type(CRYSTAL_TYPE), IN :: self
      INT :: res
      res = 0
      if (allocated(self%soft_mode_vectors)) res = size(self%soft_mode_vectors,2)
   end function

   pure function n_TLS_sigma(self) result (res)
   ! The number of elements of the model's covariance Sigma = <v v^T>,
   ! v = (t, lambda, Q_1 ... Q_K): the 21 of the rigid body, then one
   ! amplitude <Q_j^2> per soft mode, then, with use_soft_mode_correlations=,
   ! every cross term of a soft mode. See TLS_sigma_pairs for the order.
      type(CRYSTAL_TYPE), IN :: self
      INT :: res
      INT :: n_v
      n_v = 6+n_soft_modes_(self)
      res = 21+n_soft_modes_(self)
      if (allocated(self%xray_data)) then
         if (self%xray_data%use_soft_mode_correlations) res = n_v*(n_v+1)/2
      end if
   end function

   PURE subroutine TLS_sigma_pairs(self,pr)
   ! "pr(:,i)" is the pair of coordinates (p,q), p <= q, of element i of
   ! Sigma, coordinates 1-3 t, 4-6 lambda, 6+j soft mode j. The order: the
   ! 21 rigid-body elements row by row; the amplitudes (6+j,6+j); then the
   ! cross terms, each mode's six with the rigid body and then those
   ! between soft modes.
      type(CRYSTAL_TYPE), IN :: self
      MAT(INT,:,:), allocatable, OUT :: pr
      INT :: n_s,i,j,p,q
      n_s = n_soft_modes_(self)
      call create_(pr,2,n_TLS_sigma_(self))
      i = 0
      do p = 1,6
         do q = p,6
            i = i+1
            pr(:,i) = [p,q]
         end do
      end do
      do j = 1,n_s
         i = i+1
         pr(:,i) = [6+j,6+j]
      end do
      if (i==size(pr,2)) return
      do j = 1,n_s
         do p = 1,6
            i = i+1
            pr(:,i) = [p,6+j]
         end do
      end do
      do j = 1,n_s
         do q = j+1,n_s
            i = i+1
            pr(:,i) = [6+j,6+q]
         end do
      end do
   end subroutine

   PURE function TLS_response(self,g) result (res)
   ! d U_g / d sigma for TLS body atom "g": the change in
   ! (U_xx,U_yy,U_zz,U_xy,U_xz,U_yz) per unit of each element sigma_pq of
   ! Sigma = <v v^T>, in the order of TLS_sigma_pairs. U_g = B_g Sigma B_g^T
   ! with B_g = [1 | A_g | d_g1 ... d_gK]: A_g lambda = lambda x r_g, r_g
   ! from .TLS_origin, and d_gj atom g's displacement per unit normal
   ! coordinate of soft mode j.
      type(CRYSTAL_TYPE), IN :: self
      INT, IN :: g
      MAT(REAL,6,n_TLS_sigma_(self)) :: res
      VEC(REAL,3) :: d
      MAT(REAL,:,:), allocatable :: Bm
      MAT(REAL,3,3) :: M
      MAT(INT,:,:), allocatable :: pr
      INT :: p,q,k,i,j,n_v
      n_v = 6+n_soft_modes_(self)
      call create_(Bm,3,n_v)
      Bm = ZERO
      d = self%fragment_atom(g)%position-self%TLS_origin
      do i = 1,3
         Bm(i,i) = ONE
      end do
      Bm(1,5) = d(3); Bm(1,6) = -d(2)
      Bm(2,4) = -d(3); Bm(2,6) = d(1)
      Bm(3,4) = d(2); Bm(3,5) = -d(1)
      do k = 1,n_soft_modes_(self)
         Bm(:,6+k) = self%soft_mode_vectors(3*g-2:3*g,k)
      end do
      call TLS_sigma_pairs_(self,pr)
      do k = 1,n_TLS_sigma_(self)
         p = pr(1,k)
         q = pr(2,k)
         do i = 1,3
            do j = 1,3
               M(i,j) = Bm(i,p)*Bm(j,q)
            end do
         end do
         if (p/=q) M = M+transpose(M)
         res(:,k) = [M(1,1),M(2,2),M(3,3),M(1,2),M(1,3),M(2,3)]
      end do
      call destroy_(pr)
      call destroy_(Bm)
   end function

   subroutine make_TLS_jacobian_columns(self,Jt)
   ! Make "Jt", the columns of the refinement Jacobian for a TLS model, with
   ! its soft-mode amplitudes if any:
   ! d X / d p for the TLS parameters p, nonzero on the ADP rows of the
   ! unique fragment atoms. Sigma = [[T,S^T],[S,L]] is restricted to the
   ! values for which every TLS body atom's U obeys the crystal symmetry:
   ! images of one asymmetric unit atom agree, and an atom on a special
   ! position keeps its site symmetry. Directions no U sees (tr S) are
   ! dropped. Sets .TLS_origin, .TLS_basis (Sigma for each parameter) and
   ! .TLS_unique_response. Each column is scaled so its largest entry is one.
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable, OUT :: Jt
      MAT3(REAL,:,:,:), allocatable :: seitz
      MAT(REAL,:,:), allocatable :: Rall,C,Cg,CtC,Z,MZ,GZ,evec,Q
      VEC(REAL,:), allocatable :: eval,colnorm
      VEC(INT,:), allocatable :: stab
      MAT(REAL,6,6) :: W
      MAT(REAL,3,3) :: R
      REAL :: mass,big,fac
      INT :: n_f,n_u,n_c,k,g,u,v,fu,fg,a,ag,t,i,j,ub,n_stab,n_sig
   ENSURE(self%fragment_info_made,"CRYSTAL:make_TLS_jacobian_columns ... no fragment info")

      ! The TLS body: the first n_f fragment atoms, holding the unique ones
      n_f = n_TLS_atoms_(self)
      n_sig = n_TLS_sigma_(self)
      n_u = self%n_unique_frag_atoms
      DIE_IF(maxval(self%unique_frag_atom(1:n_u))>n_f,"CRYSTAL:make_TLS_jacobian_columns ... TLS: a unique fragment atom lies outside the TLS body")
      seitz = xyz_seitz_matrices_(self)

      ! Origin: the TLS body's centre of mass
      self%TLS_origin = ZERO
      mass = ZERO
      do g = 1,n_f
         self%TLS_origin = self%TLS_origin+self%fragment_atom(g)%mass*self%fragment_atom(g)%position
         mass = mass+self%fragment_atom(g)%mass
      end do
      self%TLS_origin = self%TLS_origin/mass

      ! d U / d sigma for every TLS body atom. Each column is scaled to unit
      ! length, so the tests for null directions below treat a soft-mode
      ! amplitude (whose response carries 1/m, m in electron masses) on the
      ! same footing as Sigma; the scaling is undone in Q at the end.
      call create_(Rall,6*n_f,n_sig)
      do g = 1,n_f
         Rall(6*g-5:6*g,:) = TLS_response_(self,g)
      end do
      call create_(colnorm,n_sig)
      do i = 1,n_sig
         colnorm(i) = norm2(Rall(:,i))
         if (colnorm(i)<=ZERO) colnorm(i) = ONE
         Rall(:,i) = Rall(:,i)/colnorm(i)
      end do

      ! Symmetry conditions on sigma, as rows of C sigma = 0
      n_stab = 0
      do u = 1,n_u
         a = self%asym_atom_for_frag_atom(self%unique_frag_atom(u))
         n_stab = n_stab+size(self%asym_atom_stabilizer(a)%element)
      end do
      call create_(C,6*(n_f+n_stab),n_sig)
      C = ZERO
      n_c = 0
      do g = 1,n_f

         ! The unique atom imaging the same asymmetric unit atom
         ag = self%asym_atom_for_frag_atom(g)
         fu = 0
         do v = 1,n_u
            if (self%asym_atom_for_frag_atom(self%unique_frag_atom(v))/=ag) cycle
            fu = self%unique_frag_atom(v)
         end do
         if (fu==0 OR fu==g) cycle
         R = matmul(seitz(:,:,self%asym_symop_for_frag_atom(g)),transpose(seitz(:,:,self%asym_symop_for_frag_atom(fu))))
         call symmetric_tensor_2_product_mx_(W,R)
         C(n_c+1:n_c+6,:) = Rall(6*g-5:6*g,:)-matmul(W,Rall(6*fu-5:6*fu,:))
         n_c = n_c+6
      end do
      do u = 1,n_u
         fu = self%unique_frag_atom(u)
         a = self%asym_atom_for_frag_atom(fu)
         stab = self%asym_atom_stabilizer(a)%element
         do t = 1,size(stab)
            R = matmul(seitz(:,:,self%asym_symop_for_frag_atom(fu)),matmul(seitz(:,:,stab(t)),transpose(seitz(:,:,self%asym_symop_for_frag_atom(fu)))))
            call symmetric_tensor_2_product_mx_(W,R)
            C(n_c+1:n_c+6,:) = matmul(W,Rall(6*fu-5:6*fu,:))-Rall(6*fu-5:6*fu,:)
            n_c = n_c+6
         end do
         call destroy_(stab)
      end do

      ! The site-symmetric sigma: the null space of C
      call create_(CtC,n_sig,n_sig)
      call to_product_of_(CtC,C(1:n_c,:),C(1:n_c,:),transpose_a=TRUE)
      call create_(eval,n_sig)
      call create_(evec,n_sig,n_sig)
      call solve_symmetric_eigenproblem_(CtC,eval,evec)
      big = max(maxval(abs(eval)),ONE)
      k = count(abs(eval)<=TOL(8)*big)
      call create_(Z,n_sig,k)
      j = 0
      do i = 1,n_sig
         if (abs(eval(i))>TOL(8)*big) cycle
         j = j+1
         Z(:,j) = evec(:,i)
      end do
      if (n_c==0) then
         call destroy_(Z)
         call create_(Z,n_sig,n_sig)
         Z = ZERO
         do i = 1,n_sig
            Z(i,i) = ONE
         end do
         k = n_sig
      end if

      ! Drop what no U sees (tr S): keep the range of Rall within Z
      call create_(MZ,6*n_f,k)
      call to_product_of_(MZ,Rall,Z)
      call create_(GZ,k,k)
      call to_product_of_(GZ,MZ,MZ,transpose_a=TRUE)
      call destroy_(eval); call destroy_(evec)
      call create_(eval,k)
      call create_(evec,k,k)
      call solve_symmetric_eigenproblem_(GZ,eval,evec)
      big = maxval(abs(eval))
      j = count(eval>TOL(10)*big)
      call create_(Q,n_sig,j)
      t = 0
      do i = 1,k
         if (eval(i)<=TOL(10)*big) cycle
         t = t+1
         Q(:,t) = matmul(Z,evec(:,i))
      end do
      k = j

      ! Back to unscaled sigma: Rall Q is unchanged
      do i = 1,n_sig
         Q(i,:) = Q(i,:)/colnorm(i)
         Rall(:,i) = Rall(:,i)*colnorm(i)
      end do
      call destroy_(colnorm)

      ! The Jacobian columns: d U_u / d p on each unique atom's ADP rows
      call destroy_(self%TLS_unique_response)
      call create_(self%TLS_unique_response,6*n_u,k)
      call create_(Jt,no_of_pADPs_(self%asymmetric_unit_atom),k)
      Jt = ZERO
      ub = 0
      do u = 1,n_u
         fu = self%unique_frag_atom(u)
         call create_(Cg,6,k)
         call to_product_of_(Cg,Rall(6*fu-5:6*fu,:),Q)
         Jt(ub+4:ub+9,:) = Cg
         self%TLS_unique_response(6*u-5:6*u,:) = Cg
         call destroy_(Cg)
         ub = ub+no_of_pADPs_(self%fragment_atom(fu))
      end do
      do j = 1,k
         fac = maxval(abs(Jt(:,j)))
         if (fac<=ZERO) cycle
         Jt(:,j) = Jt(:,j)/fac
         Q(:,j) = Q(:,j)/fac
         self%TLS_unique_response(:,j) = self%TLS_unique_response(:,j)/fac
      end do
      call destroy_(self%TLS_basis)
      self%TLS_basis = Q
      call destroy_(Q); call destroy_(GZ); call destroy_(MZ); call destroy_(Z)
      call destroy_(evec); call destroy_(eval); call destroy_(CtC); call destroy_(C); call destroy_(Rall)
      call destroy_(seitz)
   end subroutine

   subroutine set_refinement_parameters(self)
   ! Make the refined parameters p in .xray_data.ls for the unique fragment
   ! atoms' pADPs X. For a TLS model the ADP rows are put on the model,
   ! U = U_high + (TLS part fitted to U - U_high), and the atoms -- unique
   ! fragment and asymmetric unit -- are given those ADPs.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: X,X_fixed,v
      VEC(BIN,:), allocatable :: fixed
      MAT3(REAL,:,:,:), allocatable :: seitz_T
      type(ATOM_TYPE) :: X_atom
      INT :: n_X,u,f,a,s,n,ub
      BIN :: tls
   ENSURE(allocated(self%xray_data),"CRYSTAL:set_refinement_parameters ... no data")
   ENSURE(allocated(self%xray_data%ls),"CRYSTAL:set_refinement_parameters ... no least-squares data")
      if (NOT allocated(self%xray_data%ls%jacobian)) return
      tls = self%xray_data%ADP_model=="tls"

      ! X, and its fixed part
      n_X = self%xray_data%ls%n_X
      call create_(X,n_X)
      call create_(X_fixed,n_X)
      call create_(fixed,n_X)
      X_fixed = ZERO
      fixed = FALSE
      ub = 0
      do u = 1,self%n_unique_frag_atoms
         f = self%unique_frag_atom(u)
         n = no_of_pADPs_(self%fragment_atom(f))
         X(ub+1:ub+n) = self%fragment_atom(f)%pADP_vector
         if (tls) then
            fixed(ub+4:ub+9) = TRUE
            if (allocated(self%fragment_U_high)) then
               X_fixed(ub+4) = self%fragment_U_high(1,1,f)
               X_fixed(ub+5) = self%fragment_U_high(2,2,f)
               X_fixed(ub+6) = self%fragment_U_high(3,3,f)
               X_fixed(ub+7) = self%fragment_U_high(1,2,f)
               X_fixed(ub+8) = self%fragment_U_high(1,3,f)
               X_fixed(ub+9) = self%fragment_U_high(2,3,f)
            end if
         end if
         ub = ub+n
      end do
      call set_parameters_from_(self%xray_data%ls,X,X_fixed,fixed)

      ! A TLS model: give the atoms the model's ADPs
      if (tls) then
         seitz_T = transposed_xyz_seitz_matrices_(self)
         ub = 0
         do u = 1,self%n_unique_frag_atoms
            f = self%unique_frag_atom(u)
            a = self%asym_atom_for_frag_atom(f)
            s = self%asym_symop_for_frag_atom(f)
            n = no_of_pADPs_(self%fragment_atom(f))
            v = X(ub+1:ub+n)
            call set_pADP_vector_to_(self%fragment_atom(f),v)
            call create_(X_atom%pADP_vector,n)
            call set_pADP_vector_to_(X_atom,v)
            call transform_pADP_vector_with_(X_atom,seitz_T(:,:,s))
            call set_pADP_vector_to_(self%asymmetric_unit_atom(a),X_atom%pADP_vector)
            call destroy_ptr_part_(X_atom)
            call destroy_(v)
            ub = ub+n
         end do
         call destroy_(seitz_T)
         call set_frag_from_asym_pADPs_(self)
      end if
      call destroy_(fixed)
      call destroy_(X_fixed)
      call destroy_(X)
   end subroutine

   subroutine set_TLS_restraints(self)
   ! Set the restraint weights W and shift of .xray_data.ls for the current
   ! parameters of a TLS model. Each restraint holds a linear function
   ! r . sigma of the elements sigma of Sigma toward a target, with a
   ! sigma s:
   ! . a soft-mode amplitude toward its harmonic value a0, s =
   !   soft_mode_restraint x a0; an imaginary mode has none and is free;
   ! . with use_soft_mode_correlations=, a cross term toward zero, s =
   !   correlation_restraint x sqrt(scale_p scale_q): its correlation
   !   coefficient toward zero;
   ! . with use_positive_TLS_sigma=, v^T Sigma v toward zero for each eigenvector
   !   v of Sigma with a negative eigenvalue, s = 1/100 of its scale: the
   !   inequality Sigma >= 0, held while the data push against it.
   ! The scale of a coordinate is its Sigma diagonal (at least 1/1000 of
   ! the largest of T or L), or for a soft mode the harmonic amplitude at
   ! |frequency|. sigma = Q p, Q = .TLS_basis, so row r acts on p as r Q.
   ! With R the rows r Q / s and t = (target - r . sigma)/s, W = R^T R and
   ! the shift d = R^T y with (R R^T) y = t, so that W d = R^T t.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: X,p,sv,scl,tg,sgm,t,y,ev,e2
      MAT(INT,:,:), allocatable :: pr
      MAT(REAL,:,:), allocatable :: rw,RQ,RR,Smat,evec,V2
      REAL :: w,kT,big,fac
      INT :: n_s,n_sig,n_v,k,first,n_p,n_r,i,j,a,ip,iq
   ENSURE(allocated(self%xray_data%ls),"CRYSTAL:set_TLS_restraints ... no least-squares data")
      n_s = n_soft_modes_(self)
      n_sig = n_TLS_sigma_(self)
      n_v = 6+n_s
      k = size(self%TLS_basis,2)
      first = self%TLS_first_param
      n_p = self%xray_data%ls%n_param
      call TLS_sigma_pairs_(self,pr)

      ! The current Sigma
      call make_unique_frag_X_(self,X)
      call make_param_from_X_(self%xray_data%ls,X,p)
      call create_(sv,n_sig)
      call to_product_of_(sv,self%TLS_basis,p(first:first+k-1))
      call create_(Smat,n_v,n_v)
      Smat = ZERO
      do i = 1,n_sig
         Smat(pr(1,i),pr(2,i)) = sv(i)
         Smat(pr(2,i),pr(1,i)) = sv(i)
      end do

      ! The scale of each coordinate
      call create_(scl,n_v)
      do i = 1,6
         scl(i) = Smat(i,i)
      end do
      big = max(maxval(scl(1:3)),TOL(10))
      do i = 1,3
         scl(i) = max(scl(i),TOL(3)*big)
      end do
      big = max(maxval(scl(4:6)),TOL(10))
      do i = 4,6
         scl(i) = max(scl(i),TOL(3)*big)
      end do
      kT = self%soft_mode_temperature/KELVIN_PER_HARTREE
      do j = 1,n_s
         w = abs(self%soft_mode_frequency(j))/WAVENUMBER_PER_HARTREE
         scl(6+j) = HALF/w
         if (kT>ZERO) scl(6+j) = scl(6+j)/tanh(HALF*w/kT)
      end do

      ! The restraints, as rows on sigma
      call create_(rw,n_sig+n_v,n_sig)
      call create_(tg,n_sig+n_v)
      call create_(sgm,n_sig+n_v)
      rw = ZERO
      n_r = 0
      do j = 1,n_s
         if (self%soft_mode_harmonic(j)<=ZERO) cycle
         n_r = n_r+1
         rw(n_r,21+j) = ONE
         tg(n_r) = self%soft_mode_harmonic(j)
         sgm(n_r) = self%xray_data%soft_mode_restraint*self%soft_mode_harmonic(j)
      end do
      if (self%xray_data%use_soft_mode_correlations) then
         do i = 22+n_s,n_sig
            ip = pr(1,i)
            iq = pr(2,i)
            n_r = n_r+1
            rw(n_r,i) = ONE
            tg(n_r) = ZERO
            sgm(n_r) = self%xray_data%correlation_restraint*sqrt(scl(ip)*scl(iq))
         end do
      end if
      self%n_TLS_held_at_zero = 0
      if (self%xray_data%use_positive_TLS_sigma) then
         call create_(ev,n_v)
         call create_(evec,n_v,n_v)
         RR = Smat
         call solve_symmetric_eigenproblem_(RR,ev,evec)
         big = maxval(abs(ev))
         do a = 1,n_v
            if (ev(a)>=-TOL(8)*big) cycle
            n_r = n_r+1
            do i = 1,n_sig
               ip = pr(1,i)
               iq = pr(2,i)
               fac = evec(ip,a)*evec(iq,a)
               if (ip/=iq) fac = TWO*fac
               rw(n_r,i) = fac
            end do
            tg(n_r) = ZERO
            sgm(n_r) = sum(evec(:,a)**2*scl)/HUNDRED
            self%n_TLS_held_at_zero = self%n_TLS_held_at_zero+1
         end do
         call destroy_(RR); call destroy_(evec); call destroy_(ev)
      end if
      call destroy_(self%xray_data%ls%restraint_W)
      call destroy_(self%xray_data%ls%restraint_shift)

      ! None: leave the solve unrestrained, as without soft modes
      if (n_r==0 AND n_s==0) then
         call destroy_(Smat); call destroy_(scl); call destroy_(sgm); call destroy_(tg); call destroy_(rw)
         call destroy_(sv); call destroy_(p); call destroy_(X); call destroy_(pr)
         return
      end if
      call create_(self%xray_data%ls%restraint_W,n_p,n_p)
      call create_(self%xray_data%ls%restraint_shift,n_p)
      self%xray_data%ls%restraint_W = ZERO
      self%xray_data%ls%restraint_shift = ZERO
      if (n_r>0) then
         call create_(RQ,n_r,k)
         call to_product_of_(RQ,rw(1:n_r,:),self%TLS_basis)
         call create_(t,n_r)
         call to_product_of_(t,rw(1:n_r,:),sv)
         do i = 1,n_r
            t(i) = (tg(i)-t(i))/sgm(i)
            RQ(i,:) = RQ(i,:)/sgm(i)
         end do
         self%xray_data%ls%restraint_W(first:first+k-1,first:first+k-1) = matmul(transpose(RQ),RQ)
         ! y = (R R^T)^+ t, dropping directions no parameter reaches
         call create_(RR,n_r,n_r)
         call to_product_of_(RR,RQ,RQ,transpose_b=TRUE)
         call create_(e2,n_r)
         call create_(V2,n_r,n_r)
         call solve_symmetric_eigenproblem_(RR,e2,V2)
         big = max(maxval(abs(e2)),TOL(20))
         call create_(y,n_r)
         y = ZERO
         do i = 1,n_r
            if (e2(i)<=TOL(10)*big) cycle
            y = y+V2(:,i)*dot_product(V2(:,i),t)/e2(i)
         end do
         self%xray_data%ls%restraint_shift(first:first+k-1) = matmul(transpose(RQ),y)
         call destroy_(y); call destroy_(V2); call destroy_(e2); call destroy_(RR); call destroy_(t); call destroy_(RQ)
      end if
      call destroy_(Smat); call destroy_(scl); call destroy_(sgm); call destroy_(tg); call destroy_(rw)
      call destroy_(sv); call destroy_(p); call destroy_(X); call destroy_(pr)
   end subroutine

   PURE subroutine make_unique_frag_X(self,X)
   ! Make "X", the unique fragment atoms' pADP vectors end to end, in the
   ! order of the refinement Jacobian.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,:), allocatable, OUT :: X
      INT :: u,f,n,ub,n_X
      n_X = 0
      do u = 1,self%n_unique_frag_atoms
         n_X = n_X+no_of_pADPs_(self%fragment_atom(self%unique_frag_atom(u)))
      end do
      call create_(X,n_X)
      ub = 0
      do u = 1,self%n_unique_frag_atoms
         f = self%unique_frag_atom(u)
         n = no_of_pADPs_(self%fragment_atom(f))
         X(ub+1:ub+n) = self%fragment_atom(f)%pADP_vector
         ub = ub+n
      end do
   end subroutine

   subroutine put_TLS_results(self)
   ! Put T, L and S of a TLS refinement, about the fragment's centre of
   ! mass, with esds. They are fitted to the unique fragment atoms' ADPs
   ! minus U_high, which lie on the model, and their esds come from the
   ! covariance of the TLS parameters scaled by GoF^2.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: Mt,MtM,Cp,CS,Sm,Esd
      VEC(REAL,:), allocatable :: y,p,rhs,sv,var
      MAT(REAL,3,3) :: Um
      VEC(REAL,3) :: L_evals
      MAT(REAL,3,3) :: L_evecs
      REAL :: ang,deg2,adeg
      INT :: n_u,k,u,f,i,j,pp,qq,m,first
   ENSURE(allocated(self%TLS_basis),"CRYSTAL:put_TLS_results ... no TLS model")
   ENSURE(allocated(self%TLS_unique_response),"CRYSTAL:put_TLS_results ... no TLS model")
      n_u = self%n_unique_frag_atoms
      k = size(self%TLS_basis,2)
      first = self%TLS_first_param

      ! The TLS part of each unique atom's ADP, and the fit to it
      call create_(y,6*n_u)
      do u = 1,n_u
         f = self%unique_frag_atom(u)
         Um = self%fragment_atom(f)%ADP_tensor
         if (allocated(self%fragment_U_high)) Um = Um-self%fragment_U_high(:,:,f)
         y(6*u-5:6*u) = [Um(1,1),Um(2,2),Um(3,3),Um(1,2),Um(1,3),Um(2,3)]
      end do
      Mt = self%TLS_unique_response
      call create_(MtM,k,k)
      call to_product_of_(MtM,Mt,Mt,transpose_a=TRUE)
      call create_(rhs,k)
      call to_product_of_(rhs,Mt,y,transpose_a=TRUE)
      call create_(p,k)
      call solve_linear_equation_(MtM,rhs,p)

      ! Sigma and its esds; the 21 upper-triangle elements
      call create_(sv,n_TLS_sigma_(self))
      call to_product_of_(sv,self%TLS_basis,p)
      call create_(var,n_TLS_sigma_(self))
      var = ZERO
      if (allocated(self%xray_data%ls%param_covariance)) then
         Cp = self%xray_data%ls%param_covariance(first:first+k-1,first:first+k-1)
         call create_(CS,n_TLS_sigma_(self),k)
         call to_product_of_(CS,self%TLS_basis,Cp)
         do i = 1,n_TLS_sigma_(self)
            var(i) = self%xray_data%GoF2*dot_product(CS(i,:),self%TLS_basis(i,:))
         end do
         call destroy_(CS)
         call destroy_(Cp)
      end if
      call create_(Sm,6,6)
      call create_(Esd,6,6)
      m = 0
      do pp = 1,6
         do qq = pp,6
            m = m+1
            Sm(pp,qq) = sv(m); Sm(qq,pp) = sv(m)
            Esd(pp,qq) = sqrt(max(ZERO,var(m))); Esd(qq,pp) = Esd(pp,qq)
         end do
      end do
      ang = ANGSTROM_PER_BOHR
      deg2 = (180d0/PI)**2
      adeg = ANGSTROM_PER_BOHR*180d0/PI
      call flush_(stdout)
      call text_(stdout,"===========================")
      call text_(stdout,"Rigid-body (TLS) parameters")
      call text_(stdout,"===========================")
      call flush_(stdout)
      call text_(stdout,". U_i = U_i^high + T + A_i L A_i^T + A_i S + S^T A_i^T, about the")
      call text_(stdout,"  fragment's centre of mass; tr S = 0. Esds in brackets.")
      call flush_(stdout)
      call show_(stdout,"No. of TLS parameters           =",k)
      call show_(stdout,"U_high from stiff modes used?   =",allocated(self%fragment_U_high))
      call show_(stdout,"Origin x /A                     =",self%TLS_origin(1)*ang)
      call show_(stdout,"Origin y /A                     =",self%TLS_origin(2)*ang)
      call show_(stdout,"Origin z /A                     =",self%TLS_origin(3)*ang)
      call flush_(stdout)
      call text_(stdout,"T /A^2:")
      do i = 1,3
         call text_(stdout,"   "//trim(TLS_element_str_(self,Sm(i,1:3)*ang*ang,Esd(i,1:3)*ang*ang)))
      end do
      call flush_(stdout)
      call text_(stdout,"L /deg^2:")
      do i = 4,6
         call text_(stdout,"   "//trim(TLS_element_str_(self,Sm(i,4:6)*deg2,Esd(i,4:6)*deg2)))
      end do
      call flush_(stdout)
      call text_(stdout,"S /(A deg), rows lambda, columns t:")
      do i = 4,6
         call text_(stdout,"   "//trim(TLS_element_str_(self,Sm(i,1:3)*adeg,Esd(i,1:3)*adeg)))
      end do
      call flush_(stdout)
      Um = Sm(4:6,4:6)*deg2
      call solve_symmetric_eigenproblem_(Um,L_evals,L_evecs)
      call show_(stdout,"L eigenvalues /deg^2, smallest  =",L_evals(1))
      call show_(stdout,"L eigenvalues /deg^2, middle    =",L_evals(2))
      call show_(stdout,"L eigenvalues /deg^2, largest   =",L_evals(3))
      call show_(stdout,"rms libration /deg              =",sqrt(max(ZERO,sum(L_evals)/THREE)))
      call flush_(stdout)

      ! Soft modes, and their correlations
      if (n_soft_modes_(self)>0) call put_soft_mode_results_(self,sv(22:),var(22:))
      if (n_soft_modes_(self)>0 AND self%xray_data%use_soft_mode_correlations) call put_soft_mode_correlations_(self,sv,var)
      if (self%xray_data%use_positive_TLS_sigma) then
         call show_(stdout,"Directions of Sigma held at 0   =",self%n_TLS_held_at_zero)
         call flush_(stdout)
      end if

      ! Model selection
      call put_model_selection_(self)
      call destroy_(Esd); call destroy_(Sm); call destroy_(var); call destroy_(sv)
      call destroy_(p); call destroy_(rhs); call destroy_(MtM); call destroy_(Mt); call destroy_(y)
   end subroutine

   subroutine put_soft_mode_results(self,a,var)
   ! Put the refined soft-mode amplitudes "a" (<Q^2>, atomic units) with
   ! the variances "var", against their harmonic values, and the frequency
   ! each refined amplitude implies at the data's temperature.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,:), IN :: a,var
      REAL :: w,kT
      INT :: j
      kT = self%soft_mode_temperature/KELVIN_PER_HARTREE
      call flush_(stdout)
      call text_(stdout,"Soft internal modes, amplitude <Q^2> in atomic units (m_e^1/2 bohr)^2.")
      call text_(stdout,"Harmonic: from the Hessian. Implied: the frequency that gives the refined")
      call text_(stdout,"amplitude at the same temperature. A negative frequency is imaginary.")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=5)
      call put_(stdout,"#",int_width=TRUE)
      call put_(stdout,"Freq /cm^-1")
      call put_(stdout,"Harmonic")
      call put_(stdout,"Refined")
      call put_(stdout,"esd")
      call put_(stdout,"Implied /cm^-1")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=5)
      do j = 1,n_soft_modes_(self)
         call put_(stdout,j)
         call put_(stdout,self%soft_mode_frequency(j))
         if (self%soft_mode_harmonic(j)>ZERO) then; call put_(stdout,self%soft_mode_harmonic(j))
         else; call put_(stdout,"--")
         end if
         call put_(stdout,a(j))
         call put_(stdout,sqrt(max(ZERO,var(j))))
         if (a(j)>ZERO) then
            w = frequency_for_amplitude(a(j),kT)
            call put_(stdout,w*WAVENUMBER_PER_HARTREE)
         else
            call put_(stdout,"--")
         end if
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=5)
   end subroutine

   subroutine put_soft_mode_correlations(self,sv,var)
   ! Put the correlation coefficient of each soft mode with the rigid-body
   ! coordinates and the other soft modes, Sigma_pq/sqrt(Sigma_pp Sigma_qq),
   ! from the elements "sv" of Sigma and their variances "var", with esds.
   ! A coefficient whose diagonal elements are not positive is not shown.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,:), IN :: sv,var
      MAT(INT,:,:), allocatable :: pr
      MAT(REAL,:,:), allocatable :: Smat,Vmat
      VEC(STR(len=STR_SIZE),:), allocatable :: lab
      REAL :: den
      INT :: n_s,n_v,i,j,q
      n_s = n_soft_modes_(self)
      n_v = 6+n_s
      call TLS_sigma_pairs_(self,pr)
      call create_(Smat,n_v,n_v); Smat = ZERO
      call create_(Vmat,n_v,n_v); Vmat = ZERO
      do i = 1,n_TLS_sigma_(self)
         Smat(pr(1,i),pr(2,i)) = sv(i); Smat(pr(2,i),pr(1,i)) = sv(i)
         Vmat(pr(1,i),pr(2,i)) = var(i); Vmat(pr(2,i),pr(1,i)) = var(i)
      end do
      call create_(lab,n_v)
      lab(1:6) = ["t_x     ","t_y     ","t_z     ","lambda_x","lambda_y","lambda_z"]
      do j = 1,n_s
         lab(6+j) = "Q_"//trim(to_str_(j))
      end do
      call flush_(stdout)
      call text_(stdout,"Soft-mode correlation coefficients Sigma_pq/sqrt(Sigma_pp Sigma_qq),")
      call text_(stdout,"one row per soft mode; esds in brackets.")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=n_v)
      call put_(stdout,"#",int_width=TRUE)
      do q = 1,n_v
         call put_(stdout,trim(lab(q)))
      end do
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=n_v)
      do j = 1,n_s
         call put_(stdout,j)
         do q = 1,n_v
            den = Smat(6+j,6+j)*Smat(q,q)
            if (q==6+j OR den<=ZERO) then
               call put_(stdout,"--")
            else
               den = sqrt(den)
               call put_(stdout,trim(TLS_element_str_1_(self,Smat(6+j,q)/den,sqrt(max(ZERO,Vmat(6+j,q)))/den)))
            end if
         end do
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=n_v)
      call destroy_(lab); call destroy_(Vmat); call destroy_(Smat); call destroy_(pr)
   end subroutine

   PURE function TLS_element_str_1(self,val,esd) result (res)
   ! "val" with its "esd" in brackets, as a short string.
      type(CRYSTAL_TYPE), IN :: self
      REAL, IN :: val,esd
      STR(len=STR_SIZE) :: res
      if (esd>TOL(10)) then; res = trim(adjustl(to_str_(val,esd,12,4)))
      else; res = trim(adjustl(to_str_(val,"f12.4")))
      end if
   end function

   PURE function frequency_for_amplitude(a,kT) result (res)
   ! The frequency w (atomic units) for which a harmonic oscillator at
   ! temperature kT (hartree) has mean-square amplitude <Q^2> = "a":
   ! coth(w/2kT)/(2w) = a, which falls monotonically with w. At kT = 0,
   ! w = 1/(2a). Found by bisection in log w.
      REAL, IN :: a,kT
      REAL :: res
      REAL :: lo,hi,w,f
      INT :: i
      if (kT<=ZERO) then
         res = HALF/a
         return
      end if
      lo = log(TOL(10))
      hi = log(TEN)
      do i = 1,200
         w = exp(HALF*(lo+hi))
         f = HALF/(w*tanh(HALF*w/kT))
         if (f>a) then; lo = log(w)
         else; hi = log(w)
         end if
      end do
      res = exp(HALF*(lo+hi))
   end function

   subroutine put_model_selection(self)
   ! Put the numbers for comparing refinements of the same data with
   ! different models: chi^2, the effective number of parameters p_eff
   ! (tr[(A+W)^-1 A] for the structural ones, plus scale and extinction),
   ! GoF, and the criteria AIC = chi^2 + 2 p_eff and BIC = chi^2 + p_eff ln n.
   ! Smaller AIC or BIC is the better model.
      type(CRYSTAL_TYPE), IN :: self
      REAL :: chi2,p_eff,n
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_model_selection ... no data")
      n = size(self%xray_data%reflections)
      chi2 = self%xray_data%GoF2*(n-self%xray_data%n_param)
      p_eff = self%xray_data%n_param
      if (allocated(self%xray_data%ls)) then
         p_eff = self%xray_data%n_param-self%xray_data%n_param_structure+self%xray_data%ls%n_param_eff
      end if
      call flush_(stdout)
      call text_(stdout,"Model selection:")
      call flush_(stdout)
      call show_(stdout,"No. of reflections n            =",n)
      call show_(stdout,"Effective no. of parameters     =",p_eff)
      call show_(stdout,"chi^2                           =",chi2)
      call show_(stdout,"GoF                             =",sqrt(self%xray_data%GoF2))
      call show_(stdout,"AIC = chi^2 + 2 p_eff           =",chi2+TWO*p_eff)
      call show_(stdout,"BIC = chi^2 + p_eff ln n        =",chi2+p_eff*log(n))
      call flush_(stdout)
   end subroutine

   PURE function TLS_element_str(self,val,esd) result (res)
   ! Three matrix elements "val" with their "esd"s in brackets, as a string.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,3), IN :: val,esd
      STR(len=STR_SIZE) :: res
      INT :: i
      res = " "
      do i = 1,3
         if (esd(i)>TOL(10)) then; res = trim(res)//"  "//trim(adjustl(to_str_(val(i),esd(i),12,6)))
         else; res = trim(res)//"  "//trim(adjustl(to_str_(val(i),"f12.6")))
         end if
      end do
   end function

   PURE subroutine refinement_flags_for_frag_atom(self,f,refine_positions,refine_ADPs,refine_Uiso)
   ! Which of fragment atom "f"'s parameters are refined: its positions, its
   ! ADPs, and whether the ADP is the single U_iso. Hydrogens follow
   ! refine_H_positions, refine_H_ADPs and refine_H_U_iso; atoms named in
   ! refine_nothing_for_atom refine nothing; those in refine_U_iso_for_atom
   ! refine U_iso. The derivative routines and make_refinement_jacobian both
   ! use this, so they cannot disagree.
      type(CRYSTAL_TYPE), IN :: self
      INT, IN :: f
      BIN, OUT :: refine_positions,refine_ADPs,refine_Uiso
      STR(len=STR_SIZE) :: tag
   ENSURE(allocated(self%xray_data),"CRYSTAL:refinement_flags_for_frag_atom ... no data")
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:refinement_flags_for_frag_atom ... no fragment_atom")
      tag = self%fragment_atom(f)%tag
      refine_positions = NOT self%xray_data%refine_ADPs_only
      refine_ADPs = NOT self%xray_data%refine_positions_only
      refine_Uiso = FALSE
      if (self%fragment_atom(f)%atomic_number==1) then
         refine_positions = self%xray_data%refine_H_positions
         refine_ADPs = self%xray_data%refine_H_ADPs
         refine_Uiso = self%xray_data%refine_H_U_iso
      end if
      if (allocated(self%xray_data%refine_U_iso_for_atom)) then
         if (any(self%xray_data%refine_U_iso_for_atom==tag)) then
            refine_Uiso = TRUE
         end if
      end if
      if (allocated(self%xray_data%refine_nothing_for_atom)) then
         if (any(self%xray_data%refine_nothing_for_atom==tag)) then
            refine_positions = FALSE
            refine_ADPs = FALSE
         end if
      end if
      ENSURE(NOT (refine_Uiso AND has_only_ADP4s_and_errors_(self%fragment_atom(f))),"CRYSTAL:refinement_flags_for_frag_atom ... can't refine isotropic ADPs & ADP4s for "//trim(tag))
      ENSURE(NOT (refine_Uiso AND has_only_ADP3s_and_errors_(self%fragment_atom(f))),"CRYSTAL:refinement_flags_for_frag_atom ... can't refine isotropic ADPs & ADP3s for "//trim(tag))
   end subroutine

   subroutine make_refinement_jacobian(self)
   ! Make .xray_data.X_jacobian, which maps the refined parameters onto the
   ! pADP vector of the unique fragment atoms, and the labels of those
   ! parameters. For each atom: a unit column for every refined component;
   ! one column summing U_xx, U_yy and U_zz for an isotropic ADP; and on a
   ! special position, a basis of the site-symmetric part of those columns,
   ! so the symmetry is built into the least squares instead of being
   ! filtered out of it. Each column is scaled so its largest entry is one.
      type(CRYSTAL_TYPE), INOUT :: self
      INT :: u,f,a,s,t,n,m,i,j,k,ub,n_X,n_p,n_stab,nz,imax
      BIN :: refine_positions,refine_ADPs,refine_Uiso
      MAT(REAL,:,:), allocatable :: Cols,Proj,Jac
      VEC(REAL,:), allocatable :: v,w
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      VEC(INT,:), allocatable :: stabilizer
      MAT3(REAL,:,:,:), allocatable :: seitz,seitz_T
      type(ATOM_TYPE) :: X_atom
      REAL :: nrm
   ENSURE(self%fragment_info_made,"CRYSTAL:make_refinement_jacobian ... no fragment info")
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_refinement_jacobian ... no data")
   ENSURE(allocated(self%asym_atom_stabilizer),"CRYSTAL:make_refinement_jacobian ... no asym_atom_stabilizer")
      seitz = xyz_seitz_matrices_(self)
      seitz_T = transposed_xyz_seitz_matrices_(self)
      n_X = no_of_pADPs_(self%asymmetric_unit_atom)
      call create_(Jac,n_X,n_X+21)
      Jac = ZERO
      call create_(labels,n_X+21)
      n_p = 0
      ub = 0
      do u = 1,self%n_unique_frag_atoms
         f = self%unique_frag_atom(u)
         a = self%asym_atom_for_frag_atom(f)
         s = self%asym_symop_for_frag_atom(f)
         n = no_of_pADPs_(self%fragment_atom(f))
         call refinement_flags_for_frag_atom_(self,f,refine_positions,refine_ADPs,refine_Uiso)

         ! A TLS model makes every atom's ADP; its columns come below
         if (self%xray_data%ADP_model=="tls") refine_ADPs = FALSE

         ! Candidate columns: the refined components
         call create_(Cols,n,n)
         Cols = ZERO
         m = 0
         if (refine_positions) then
            do i = 1,3
               m = m+1
               Cols(i,m) = ONE
            end do
         end if
         if (refine_ADPs) then
            if (refine_Uiso) then
               m = m+1
               Cols(4:6,m) = ONE
            else
               do i = 4,n
                  m = m+1
                  Cols(i,m) = ONE
               end do
            end if
         end if

         ! On a special position, project the columns onto the site-symmetric
         ! subspace, then orthonormalise them and drop what vanishes. P is the
         ! stabiliser average, taken in the asymmetric unit frame and moved to
         ! this fragment atom's frame with its symop.
         stabilizer = self%asym_atom_stabilizer(a)%element
         n_stab = size(stabilizer)
         if (n_stab>1 AND m>0) then
            call create_(Proj,n,n)
            call create_(v,n)
            call create_(w,n)
            call create_(X_atom%pADP_vector,n)
            do j = 1,n
               v = ZERO
               v(j) = ONE
               call set_pADP_vector_to_(X_atom,v)
               call transform_pADP_vector_with_(X_atom,seitz_T(:,:,s))
               v = X_atom%pADP_vector
               w = ZERO
               do t = 1,n_stab
                  call set_pADP_vector_to_(X_atom,v)
                  call transform_pADP_vector_with_(X_atom,seitz(:,:,stabilizer(t)))
                  w = w+X_atom%pADP_vector
               end do
               w = w/REALIFY(n_stab)
               call set_pADP_vector_to_(X_atom,w)
               call transform_pADP_vector_with_(X_atom,seitz(:,:,s))
               Proj(:,j) = X_atom%pADP_vector
            end do
            call destroy_ptr_part_(X_atom)
            Cols(:,1:m) = matmul(Proj,Cols(:,1:m))

            ! Gram-Schmidt on the columns; drop dependent ones
            k = 0
            do j = 1,m
               v = Cols(:,j)
               do i = 1,k
                  v = v-dot_product(Cols(:,i),v)*Cols(:,i)
               end do
               nrm = sqrt(dot_product(v,v))
               if (nrm<TOL(8)) cycle
               k = k+1
               Cols(:,k) = v/nrm
            end do
            m = k

            ! Largest entry of each column to one; a unit column stays a unit column
            do j = 1,m
               imax = maxloc(abs(Cols(:,j)),dim=1)
               Cols(:,j) = Cols(:,j)/Cols(imax,j)
            end do
            call destroy_(w)
            call destroy_(v)
            call destroy_(Proj)
         end if

         ! Copy in, and label each parameter
         do j = 1,m
            n_p = n_p+1
            Jac(ub+1:ub+n,n_p) = Cols(:,j)
            nz = count(abs(Cols(:,j))>TOL(10))
            if (nz==1) then
               i = maxloc(abs(Cols(:,j)),dim=1)
               labels(n_p) = tag_pADP_label_(self%asymmetric_unit_atom(a),i)
            else if (nz==3 AND abs(Cols(4,j)-ONE)<TOL(10) AND abs(Cols(5,j)-ONE)<TOL(10) AND abs(Cols(6,j)-ONE)<TOL(10)) then
               labels(n_p) = trim(self%asymmetric_unit_atom(a)%tag)//" Uiso"
            else
               labels(n_p) = trim(self%asymmetric_unit_atom(a)%tag)//" sym"//trim(to_str_(j))
            end if
         end do
         call destroy_(Cols)
         call destroy_(stabilizer)
         ub = ub+n
      end do

      ! The TLS model's columns, after the positions
      if (self%xray_data%ADP_model=="tls") then
         call make_TLS_jacobian_columns_(self,Cols)
         self%TLS_first_param = n_p+1
         do j = 1,size(Cols,2)
            n_p = n_p+1
            Jac(:,n_p) = Cols(:,j)
            labels(n_p) = "TLS "//trim(to_str_(j))
         end do
         call destroy_(Cols)
      end if

      ! Keep the Jacobian, the labels and the parameter count
      if (NOT allocated(self%xray_data%ls)) call create_(self%xray_data%ls)
      call set_jacobian_(self%xray_data%ls,Jac(:,1:n_p),labels(1:n_p))
      call set_n_param_structure_(self%xray_data,n_p)
      call update_n_param_(self%xray_data)
      call destroy_(labels)
      call destroy_(Jac)
      call destroy_(seitz_T)
      call destroy_(seitz)
   end subroutine

   subroutine get_parameter_shifts_0(self,ff)
   ! Get the param shifts -- either F or F2
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(CPX,:,:), INOUT :: ff

      ! NOTE: the F2 branch is NOT implemented -- "get_parameter_shifts_I"
      ! does not exist. Until it does this must DIE and not fall through:
      ! falling through computed no shifts at all, and a zero shift reads
      ! as convergence, so the refinement reported success having refined
      ! nothing. The machinery below it does exist -- see
      ! DIFFRACTION_DATA.SET:solve_normal_equations, which dispatches on
      ! .refine_F correctly, and the commented solve_I_normal_equations.
   ENSURE(allocated(self%xray_data),"CRYSTAL:get_parameter_shifts_0 ... no data")
   DIE_IF(NOT self%xray_data%refine_F,"CRYSTAL:get_parameter_shifts_0 ... refine_F= FALSE is not implemented: no F2 parameter shifts")
      call get_parameter_shifts_F_(self,ff)
   end subroutine

   subroutine get_parameter_shifts_1(self)
   ! Get the param shifts -- either F or F2
      type(CRYSTAL_TYPE), INOUT :: self

      ! See the note in the "ff" overload above.
   ENSURE(allocated(self%xray_data),"CRYSTAL:get_parameter_shifts_1 ... no data")
   DIE_IF(NOT self%xray_data%refine_F,"CRYSTAL:get_parameter_shifts_1 ... refine_F= FALSE is not implemented: no F2 parameter shifts")
      call get_parameter_shifts_F_(self)
   end subroutine


!  Refinement: shifts and SF update

   subroutine get_parameter_shifts_F_0(self,ff)
   ! Get the *asymmetric* atom shifts in ".xray_data.dX" by solving the
   ! normal equations, where "ff", the unique *fragment* atom
   ! form factors for the symmetry generated k points
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(CPX,:,:), INOUT :: ff
      MAT(CPX,:,:), allocatable :: dFc
      MAT(REAL,:,:), allocatable :: dFa
      INT :: n_k,n_p
   ENSURE(size(ff,1)==n_unique_SF_k_pts_(self),"CRYSTAL:get_parameter_shifts_F_0 ... ff: wrong dim1")
   ENSURE(size(ff,2)==size(self%asymmetric_unit_atom),"CRYSTAL:get_parameter_shifts_F_0 ... ff: wrong dim2")
   ENSURE(allocated(self%xray_data),"CRYSTAL:get_parameter_shifts_F_0 ... no data")
   ENSURE(self%fragment_info_made,"CRYSTAL:get_parameter_shifts_F_0 ... no fragment info")

      ! Constants
      n_k = size(ff,1)
      n_p = no_of_pADPs_(self%asymmetric_unit_atom)
      call create_(dFa,size(self%xray_data%reflections),n_p)
      call create_(dFc,size(self%xray_data%reflections),n_p)

      ! Make F_predicted, F_calc & derivatives
      call make_F_calc_derivs_(self,dFc,ff)

      ! Get the derivative absolute SF's, F_abs = |F_calc|
      call d_F_abs_dX_(self%xray_data,dFa,dFc)
      call destroy_(dFc)

      ! Get the unique atom shifts, dX (leaky)
      if (self%xray_data%ADP_model=="tls") call set_TLS_restraints_(self)
      call solve_normal_equations_(self%xray_data,dFa)
      call destroy_(dFa)
   end subroutine

   subroutine get_parameter_shifts_F_1(self)
   ! Get the *asymmetric* atom shifts in ".xray_data.dX" by solving the
   ! normal equations. Structure factors are read from disk.
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable :: dFc
      MAT(REAL,:,:), allocatable :: dFa
      INT :: n_k,n_p,n_r
   ENSURE(allocated(self%xray_data),"CRYSTAL:get_parameter_shifts_F_1 ... no data")
   ENSURE(self%fragment_info_made,"CRYSTAL:get_parameter_shifts_F_1 ... no fragment info")

      ! Constants
      n_k = n_unique_SF_k_pts_(self)
      n_p = no_of_pADPs_(self%asymmetric_unit_atom)
      n_r = size(self%xray_data%reflections)
      call create_(dFa,n_r,n_p)
      call create_(dFc,n_r,n_p)

      ! Make F_predicted, F_calc & derivatives
      call make_F_calc_derivs_(self,dFc)

      ! Get the derivative absolute SF's, F_abs = |F_calc|
      call d_F_abs_dX_(self%xray_data,dFa,dFc)
      call destroy_(dFc)

      ! Get the unique atom shifts, dX (leaky)
      if (self%xray_data%ADP_model=="tls") call set_TLS_restraints_(self)
      call solve_normal_equations_(self%xray_data,dFa)
      call destroy_(dFa)
   end subroutine

   NON_MPI_PURE subroutine get_dispersion_correction(self,Fa)
   ! Get the anomalous dispersion correction in "Fa" for the set of
   ! symmetry-unique atoms - do not add the contribution into anything.
   ! 1/2/18 
      type(CRYSTAL_TYPE), IN :: self
      VEC(CPX,:), OUT :: Fa
      VEC(CPX,:), allocatable :: Fu
      MAT(REAL,:,:), allocatable :: k_pts
      INT :: n_k,u,f
      REAL :: rf
   ENSURE(allocated(self%xray_data),"CRYSTAL:get_dispersion_correction ... no diffraction data!")
      n_k = n_symop_generated_SF_k_pts_(self)
      call create_(k_pts,n_k,3)
      call create_(Fu,n_k)

      ! The full list of symmetry-generated k points
      call make_symop_generated_SF_k_pts_(self%xray_data%reflections,k_pts,self%spacegroup,self%unit_cell)

      ! Note: each atom contribution scales by a site-symmetry factor
      Fu = ZERO
      do u = 1,self%n_unique_frag_atoms
         f = self%unique_frag_atom(u)
         rf = self%frag_atom_sym_factor(f)
         if (is_zero_(rf)) cycle
         call add_dispersion_(self%fragment_atom(f),Fu,k_pts,rf)
      end do

      ! Add together symmetry contributions from Fu into Fa
      call get_full_sd_(self%spacegroup,Fa,Fu,self%xray_data%reflections)
   end subroutine

   NON_MPI_PURE subroutine add_dispersion_correction(self,Fc)
   ! Add the anomalous dispersion correction in "Fc" for the set of
   ! symmetry-unique atoms.
   ! 1/2/18 
   ! Anomalous dispersion checked for the YQ28 (harmonic) IAM refined
   ! job but modified to remove refinement and calculate a molecular
   ! density with STO-3G basis, and by comparing to structure factors
   ! from the version below which was previously checked against XD:
   !    Version: 18.01.31 v. daf061c
   !    Platform: Linux-4.13.0-32-generic
   !    Build-date: 2018-01-31 14:35
      type(CRYSTAL_TYPE), IN :: self
      VEC(CPX,:), INOUT :: Fc
      VEC(CPX,:), allocatable :: Fu
      MAT(REAL,:,:), allocatable :: k_pts
      INT :: n_k,u,f
      REAL :: rf
   ENSURE(allocated(self%xray_data),"CRYSTAL:add_dispersion_correction ... no diffraction data!")
      n_k = n_symop_generated_SF_k_pts_(self)
      call create_(k_pts,n_k,3)
      call create_(Fu,n_k)

      ! The full list of symmetry-generated k points
      call make_symop_generated_SF_k_pts_(self%xray_data%reflections,k_pts,self%spacegroup,self%unit_cell)

      ! Note: each atom contribution scales by a site-symmetry factor
      Fu = ZERO
      do u = 1,self%n_unique_frag_atoms
         f = self%unique_frag_atom(u)
         rf = self%frag_atom_sym_factor(f)
         if (is_zero_(rf)) cycle
         call add_dispersion_(self%fragment_atom(f),Fu,k_pts,rf)
      end do

      ! Add together symmetry contributions
      call add_full_sd_(self%spacegroup,Fc,Fu,self%xray_data%reflections)
   end subroutine

   NON_MPI_PURE subroutine add_D_dispersion_correction(self,dFc)
   ! Add the derivative anomalous dispersion correction in "dFc" for
   ! wrt all paameters. This routine sums over all atoms in the unit
   ! cell --- it does not take advantage of inversion symmetry.
      type(CRYSTAL_TYPE), IN :: self
      MAT(CPX,:,:), INOUT :: dFc
      MAT(CPX,:,:), allocatable :: dFa,dFb
      MAT(REAL,:,:), allocatable :: k_pts
      INT :: n_k,n_p,u,f,base
      REAL :: rf
   ENSURE(allocated(self%xray_data),"CRYSTAL:add_D_dispersion_correction ... no diffraction data!")

      ! Size of symmetry generated k points
      n_k = n_symop_generated_SF_k_pts_(self)
      call create_(k_pts,n_k,3)

      ! Size of anamalous Jacobian
      n_p = no_of_pADPs_(self%asymmetric_unit_atom)
      call create_(dFa,n_k,n_p)
      call create_(dFb,size(self%xray_data%reflections),n_p)

      ! The full list of symmetry-generated k points
      call make_symop_generated_SF_k_pts_(self%xray_data%reflections,k_pts,self%spacegroup,self%unit_cell)

      ! Note: each atom contribution scales by a site-symmetry factor
      dFa = ZERO
      do u = 1,self%n_unique_frag_atoms
         f = self%unique_frag_atom(u)
         rf = self%frag_atom_sym_factor(f)
         if (is_zero_(rf)) cycle
         base = no_of_pADPs_up_to_atom_(self%fragment_atom(self%unique_frag_atom),u)
         call add_derivative_dispn_(self%fragment_atom(f),dFa,base,k_pts,rf)
      end do

      ! Add together symmetry contributions
      dFb = ZERO
      call add_full_sf_derivs_d_(self%spacegroup,dFb,dFa,self%xray_data%reflections)
      dFc = dFc+dFb

      ! Clean
      call destroy_(dFb)
      call destroy_(dFa)
      call destroy_(k_pts)
   end subroutine

   NON_MPI_PURE subroutine add_D_dispersion_correction_for_atom(self,a,dFc)
   ! Add the derivative anomalous dispersion correction in "dFc" for
   ! wrt the parameters on asymmetric unit atom "a". This routine sums
   ! over all atoms in the unit cell --- it does not take advantage of
   ! inversion symmetry.
      type(CRYSTAL_TYPE), IN :: self
      INT, IN :: a
      MAT(CPX,:,:), INOUT :: dFc
      MAT(CPX,:,:), allocatable :: dFa,dFb
      MAT(REAL,:,:), allocatable :: k_pts
      INT :: n_k,n_p,f,base
      REAL :: rf
   ENSURE(allocated(self%xray_data),"CRYSTAL:add_D_dispersion_correction_for_atom ... no diffraction data!")

      ! Size of symmetry generated k points
      n_k = n_symop_generated_SF_k_pts_(self)
      call create_(k_pts,n_k,3)

      ! Size of anamalous Jacobian
      n_p = no_of_pADPs_(self%asymmetric_unit_atom(a))
      call create_(dFa,n_k,n_p)
      call create_(dFb,size(self%xray_data%reflections),n_p)

      ! The full list of symmetry-generated k points
      call make_symop_generated_SF_k_pts_(self%xray_data%reflections,k_pts,self%spacegroup,self%unit_cell)

      ! Note: each atom contribution scales by a site-symmetry factor
      dFa = ZERO

      ! Sum over unit cell atoms
      f = self%unique_frag_atom(a)
      rf = self%frag_atom_sym_factor(f)
      if (NOT is_zero_(rf)) then
         base = 0
         call add_derivative_dispn_(self%fragment_atom(f),dFa,base,k_pts,rf)
      end if

      ! Add together symmetry contributions
      dFb = ZERO
      call add_full_sf_derivs_d_(self%spacegroup,dFb,dFa,self%xray_data%reflections)
      dFc = dFc+dFb

      ! Clean
      call destroy_(dFb)
      call destroy_(dFa)
      call destroy_(k_pts)
   end subroutine

   PURE subroutine shift_update_ff_0(self,ff,dF)
   ! Update the unique *fragment* atom form factors "ff" with
   ! the frag atom positional shifts (after stabilization) in "dF"
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(CPX,:,:), INOUT :: ff
      MAT(REAL,:,:), IN :: dF
      REAL :: d1,d2,d3,k1,k2,k3,kr
      INT :: n_k,u,f,k
      MAT(REAL,:,:), allocatable :: k_pt
   ENSURE(size(ff,1)==n_unique_SF_k_pts_(self),"CRYSTAL:shift_update_ff_0 ... ff: wrong dim1")
   ENSURE(size(ff,2)==self%n_asymmetric_unit_atoms,"CRYSTAL:shift_update_ff_0 ... ff: wrong dim2")
   ENSURE(size(dF,1)==3,"CRYSTAL:shift_update_ff_0 ... dF: wrong dim1")
   ENSURE(size(dF,2)==self%n_fragment_atoms,"CRYSTAL:shift_update_ff_0 ... dF: wrong dim2")

      ! Get the # of k points
      n_k = n_unique_SF_k_pts_(self)

      ! Make k points
      call create_(k_pt,n_k,3)
      call make_unique_SF_k_pts_(self%xray_data%reflections,k_pt,self%spacegroup,self%unit_cell)

      ! Loop over unique *frag* atoms
      do u = 1,self%n_unique_frag_atoms

         ! Link ufrag and asym atoms
         f = self%unique_frag_atom(u)

         ! Get the shift
         d1 = dF(1,f)
         d2 = dF(2,f)
         d3 = dF(3,f)

         ! Update ff
         do k = 1,n_k
            k1 = k_pt(k,1)
            k2 = k_pt(k,2)
            k3 = k_pt(k,3)
            kr = k1*d1+k2*d2+k3*d3
            ff(k,u) = ff(k,u)*cmplx(cos(kr),sin(kr),kind=CPX_KIND)
         end do
      end do

      ! Clean
      call destroy_(k_pt)
   end subroutine

   subroutine shift_update_ff_1(self,dF)
   ! Update the unique *fragment* atom structure factors "ff" with
   ! the frag atom positional shifts (after stabilization) in "dF"
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: dF
      REAL :: d1,d2,d3,k1,k2,k3,kr
      INT :: n_k,u,f,k
      VEC(CPX,:), allocatable :: ff
      MAT(REAL,:,:), allocatable :: k_pt
      type(ARCHIVE_TYPE), allocatable :: arch
   ENSURE(allocated(self%xray_data),"CRYSTAL:shift_update_ff_1 ... no diffraction data!")
   ENSURE(size(dF,1)==3,"CRYSTAL:shift_update_ff_1 ... dF: wrong dim1")
   ENSURE(size(dF,2)==self%n_fragment_atoms,"CRYSTAL:shift_update_ff_1 ... dF: wrong dim2")

      ! Get the # of k points
      n_k = n_unique_SF_k_pts_(self)

      ! Space for temp-indepedent aspherical SFs
      call create_(ff,n_k)

      ! Make k points
      call create_(k_pt,n_k,3)
      call make_unique_SF_k_pts_(self%xray_data%reflections,k_pt,self%spacegroup,self%unit_cell)

      ! Loop over unique *frag* atoms
      do u = 1,self%n_unique_frag_atoms

         ! Link ufrag and asym atoms
         f = self%unique_frag_atom(u)

         ! Get the shift
         d1 = dF(1,f)
         d2 = dF(2,f)
         d3 = dF(3,f)

         ! Read stored SFS
         call create_(arch,trim(self%fragment_atom(f)%tag)//"-SFs")
         if (self%xray_data%use_text_SFs) call set_format_(arch,"ascii")
      ! arch.parallel_read(ff)
         call read_(arch,ff)  ! NOT PURE
         call destroy_(arch)

         ! Update ff
         do k = 1,n_k
            k1 = k_pt(k,1)
            k2 = k_pt(k,2)
            k3 = k_pt(k,3)
            kr = k1*d1+k2*d2+k3*d3
            ff(k) = ff(k)*cmplx(cos(kr),sin(kr),kind=CPX_KIND)
         end do

         ! Write out updated SFs
         !
         ! Plain `write`, NOT `per_rank_write`. The `do u` loop above is SERIAL,
         ! so every rank arrives here with the same "f" and therefore the same
         ! file name, and per_rank_write is the ordinary write with the
         ! master-only guard REMOVED -- every rank writing one file at once,
         ! with no locking and no offsets. Unique atom labels do not save this:
         ! they make different ATOMS use different files, which is what makes
         ! the `parallel do u` version in MOLECULE.RHO:get_Hirshfeld_atom_FFs_-
         ! disk correct, but here every rank is on the SAME label.
         !
         ! Worse than it looks, because this is a read-modify-write: the read
         ! above is FILE:read, which is master-only and BROADCASTS the values,
         ! so all ranks already hold master's copy -- and then all of them wrote
         ! it back over each other. The read side is master-authoritative
         ! already; the write side now agrees with it.
         !
         ! Master-only, not `parallel do`, deliberately: correctness first.
         ! Distributing this loop is available (the per-atom work is
         ! independent, and the commented-out `parallel_read` above shows it was
         ! the intent) but it is a rewrite, and not obviously a win -- N ranks
         ! each opening, writing and closing a small file at the same instant
         ! contends on the filesystem, badly over NFS/Lustre. See TASKS_AND_HISTORY.md.
         call create_(arch,trim(self%fragment_atom(f)%tag)//"-SFs")
         if (self%xray_data%use_text_SFs) call set_format_(arch,"ascii")
         call write_(arch,ff)
         call destroy_(arch)
      end do

      ! Clean
      call destroy_(k_pt)
      call destroy_(ff)
   end subroutine

   PURE subroutine set_fragment_esds_0(self)
   ! Set *all* the errors in the .fragment_atom positions/ADPs
      type(CRYSTAL_TYPE), INOUT :: self
      call set_fragment_esds_(self,self%fragment_atom)
   end subroutine

   PURE subroutine set_fragment_esds_1(self,fragment_atom)
   ! Set *all* the errors in the .fragment_atom positions/ADPs
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), INOUT :: fragment_atom
   ENSURE(self%fragment_info_made,"CRYSTAL:set_fragment_esds_1 ... no fragment info")
   ENSURE(allocated(self%xray_data),"CRYSTAL:set_fragment_esds_1 ... no data")
   ENSURE(allocated(self%xray_data%fragment_covariance_mx),"CRYSTAL:set_fragment_esds_1 ... no data.fragment_covariance_mx")
   ENSURE(size(self%fragment_atom)==size(fragment_atom),"CRYSTAL:set_fragment_esds_1 ... wrong dim, fragment_atom")

      ! Get the fragment covariances
      call set_frag_from_asym_cov_mx_(self)

      ! Set the errors
      call set_pADP_errors_to_(fragment_atom,self%xray_data%fragment_covariance_mx,self%xray_data%refine_H_U_iso)
   end subroutine


!  Refinement: unique frag <-> asym frag parameters

   PURE subroutine set_frag_from_asym_pADPs(self,dF)
   ! Set *all* .fragment_atom pADPs from the .asymmetric_unit_atom pADPs.
   ! If "dF" present, the shift in each frag atom position is returned.
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(REAL,:,:), optional, OUT :: dF
      INT :: f,a,s
      MAT3(REAL,:,:,:), allocatable :: seitz
      MAT(REAL,3,3) :: CM
      VEC(REAL,3) :: shift
   ENSURE(self%fragment_info_made,"CRYSTAL:set_frag_from_asym_pADPs ... no fragment info")

      ! Symmetry operators & cell matrix
      seitz = xyz_seitz_matrices_(self)

      ! Cell matrix
      CM = self%unit_cell%direct_mx

      ! Loop on *all* fragment atom's
      ! Set the .fragment_atom positions/ADP's
      ! from the .asymmetric_unit_atom's
      do f = 1,self%n_fragment_atoms

         ! Links between the atom indices & symop s
         a = self%asym_atom_for_frag_atom(f)
         s = self%asym_symop_for_frag_atom(f)

         ! Store original position
         if (present(dF)) dF(:,f) = -self%fragment_atom(f)%position

         ! Set frag atom pADPs
         call set_pADP_vector_to_(self%fragment_atom(f),self%asymmetric_unit_atom(a)%pADP_vector)

         ! Now transform frag atom pADPs with symop
         call transform_pADP_vector_with_(self%fragment_atom(f),seitz(:,:,s))

         ! Add any fragment shift to (cartesian)  position
         call to_product_of_(shift,CM,self%asym_shift_for_frag_atom(:,f))
         shift = self%fragment_atom(f)%position+shift
         call set_position_(self%fragment_atom(f),shift)

         ! Store shift
         if (present(dF)) dF(:,f) = dF(:,f)+self%fragment_atom(f)%position
      end do

      ! Clean
      call destroy_(seitz)
   end subroutine

   PURE subroutine set_asym_from_ufrag_shifts(self)
   ! From "dX", the unique .fragment_atom shifts in positions and
   ! ADP's, return the *asymmetric* atom shifts in positions and
   ! ADP's; place the shifts in the same "dX" parameter vector.
      type(CRYSTAL_TYPE), INOUT :: self
      INT :: u,f,a,s,n,ub,uf,ul,ab,af,al
      VEC(REAL,:), allocatable :: dA
      MAT3(REAL,:,:,:), allocatable :: seitz
      type(ATOM_TYPE) :: X_atom
   ENSURE(self%fragment_info_made,"CRYSTAL:set_asym_from_ufrag_shifts ... no fragment info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:set_asym_from_ufrag_shifts ... no asymmetric_unit_atom")
   ENSURE(allocated(self%xray_data),"CRYSTAL:set_asym_from_ufrag_shifts ... no data")
   ENSURE(allocated(self%xray_data%dX),"CRYSTAL:set_asym_from_ufrag_shifts ... no data.dX")

      ! Get symops
      seitz = transposed_xyz_seitz_matrices_(self)

      ! Asym atom shift vector
      call create_(dA,size(self%xray_data%dX))

      ! Base for ufrag atoms
      ub = 0

      ! Loop over unique frag atoms
      ! Order of asym atoms is the same.
      do u = 1,self%n_unique_frag_atoms

         ! Link ufrag and asym atoms
         f = self%unique_frag_atom(u)
         a = self%asym_atom_for_frag_atom(f)
         s = self%asym_symop_for_frag_atom(f)

         ! No of pADPs & a-base
         n = no_of_pADPs_(self%asymmetric_unit_atom(a))
         ab = no_of_pADPs_up_to_atom_(self%asymmetric_unit_atom,a)

         ! Block limits
         af = ab+1
         al = ab+n
         uf = ub+1
         ul = ub+n

         ! Put ufrag shifts at position f1 in "X_atom" (leaky)
         call create_(X_atom%pADP_vector,n)
         call set_pADP_vector_to_(X_atom,self%xray_data%dX(uf:ul))

         ! Change ufrag atom shifts -> asym atom shifts
         call transform_pADP_vector_with_(X_atom,seitz(:,:,s))

         ! Store asym shifts in position a0 for asym atom "a"
         call put_pADP_vector_to_(X_atom,dA(af:al))

         ! Clean
         call destroy_ptr_part_(X_atom)

         ! Increment u-base
         ub = ub+n
      end do

      ! Replace .dX
      self%xray_data%dX = dA

      ! Clean
      call destroy_(dA)
      call destroy_(seitz)
   end subroutine

   PURE subroutine stabilize_asym_atom_shifts(self)
   ! Stabilize ".dX", parameter vector for asymmetric atom *shifts*
   ! in positions and ADP's, removing any symmetry breaking.
      type(CRYSTAL_TYPE), INOUT :: self
      INT :: a,n,ab,af,al,n_stab,s,t
      VEC(INT,:), allocatable :: stabilizer
      MAT3(REAL,:,:,:), allocatable :: seitz
      type(ATOM_TYPE) :: X_atom,Y_atom
      REAL :: fac
   ENSURE(self%fragment_info_made,"CRYSTAL:stabilize_asym_atom_shifts ... no fragment info")
   ENSURE(allocated(self%xray_data),"CRYSTAL:stabilize_asym_atom_shifts ... no data")
   ENSURE(allocated(self%xray_data%dX),"CRYSTAL:stabilize_asym_atom_shifts ... no data.dX")

      ! Get symops
      seitz = xyz_seitz_matrices_(self)

      ! Base for asym atoms
      ab = 0

      ! Symmetrize the asym atom positions/ADP's
      ! We can use the asym atom stabilizers for the ufrag
      do a = 1,self%n_asymmetric_unit_atoms

         ! Block limits
         n = no_of_pADPs_(self%asymmetric_unit_atom(a))

         ! List of stabilizer symops
         stabilizer = self%asym_atom_stabilizer(a)%element

         ! Any stabilizers?
         n_stab = size(stabilizer)
         if (n_stab<=1) then  ! Kang debug on 2025.Mar.27 
            ab = ab+n  ! Shift to the next block
            cycle
         end if

         ! Block limits
         af = ab+1
         al = ab+n

         ! Set pADPs (leaky)
         call create_(X_atom%pADP_vector,n)
         call create_(Y_atom%pADP_vector,n)
         ! X_atom.set_pADP_vector_to(.xray_data.dX(af:al))!-Not necessary to set X_atom's pADP here. It will be done in the do s=2,n_stab loop
         call set_pADP_vector_to_(Y_atom,self%xray_data%dX(af:al))  !-Y_atom has the contribution from the identity operator (s=1) 
                                                         ! for the cumulative sum over all the stabilizer symops. (Kang's note)
         ! Apply symops and accumulate
         do s = 2,n_stab
            t = stabilizer(s)
            call set_pADP_vector_to_(X_atom,self%xray_data%dX(af:al))  ! Reset X_atom's pADP vector before transformation 
            call transform_pADP_vector_with_(X_atom,seitz(:,:,t))
            call set_pADP_vector_to_(Y_atom,Y_atom%pADP_vector+X_atom%pADP_vector)
         end do

         ! Stabilize
         fac = ONE/REALIFY(n_stab)
         call set_pADP_vector_to_(Y_atom,fac*Y_atom%pADP_vector)

         ! Put back shifts
         call put_pADP_vector_to_(Y_atom,self%xray_data%dX(af:al))

         ! Clean
         call destroy_ptr_part_(Y_atom)
         call destroy_ptr_part_(X_atom)

         ! Increment a-base
         ab = ab+n
      end do

      ! Clean
      call destroy_(seitz)
   end subroutine

   PURE subroutine update_fit_esds(self)
   ! Set the fit errors e.g. maximum shift on esd and the positional
   ! and ADP errors for the asym unit.
      type(CRYSTAL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
   ENSURE(allocated(self%xray_data),"CRYSTAL:update_fit_esds ... no data")

      ! Reset the esd's and maximum shifts on esd
      call update_fit_esds_(self%xray_data)

      ! Set asym atom par label value corresponding to the max shift/esd
      if (self%xray_data%max_shift_on_esd_ind/=DIFFRACTION_DATA_INDMAX_INIT) then
         val = tag_pADP_label_(self%asymmetric_unit_atom,self%xray_data%max_shift_on_esd_ind)
         call set_max_shift_on_esd_par_(self%xray_data,val)
      else
         call set_max_shift_on_esd_par_(self%xray_data,"--")
      end if
   end subroutine

   PURE subroutine update_refinement_ESDs(self)
   ! Update the refinement errors e.g. maximum shift on esd and the
   ! positional and ADP errors for the asym unit.
      type(CRYSTAL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
   ENSURE(allocated(self%xray_data),"CRYSTAL:update_refinement_ESDs ... no data")

      ! Set the refinement errors
      call update_refinement_ESDs_(self%xray_data)

      ! Set asym atom max refinement shift on easd par value
      if (self%xray_data%max_refshift_on_esd_ind/=DIFFRACTION_DATA_INDMAX_INIT) then
         val = tag_pADP_label_(self%asymmetric_unit_atom,self%xray_data%max_refshift_on_esd_ind)
         call set_max_refshift_on_esd_par_(self%xray_data,val)
      else
         call set_max_refshift_on_esd_par_(self%xray_data,"--")
      end if
   end subroutine


!  Refinement: transform covariance matrix

   PURE subroutine set_asym_from_ufrag_cov_mx(self)
   ! Set the asymmetric atom covariance matrix from the unique
   ! fragment atom covariance_mx (produced from refinement)
   ! Note: .covariance_mx is over-written.
      type(CRYSTAL_TYPE), INOUT :: self
      INT :: u1,f1,a1,s1,n1,u1b,u1f,u1l,a1b,a1f,a1l
      INT :: u2,f2,a2,s2,n2,u2b,u2f,u2l,a2b,a2f,a2l
      INT :: dim
      MAT(REAL,:,:), allocatable :: T1,T2
      MAT(REAL,:,:), allocatable :: acov
      MAT3(REAL,:,:,:), allocatable :: seitz
   ENSURE(self%fragment_info_made,"CRYSTAL:set_asym_from_ufrag_cov_mx ... no fragment info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:set_asym_from_ufrag_cov_mx ... no asymmetric_unit_atom")
   ENSURE(allocated(self%xray_data),"CRYSTAL:set_asym_from_ufrag_cov_mx ... no data")
   ENSURE(allocated(self%xray_data%covariance_mx),"CRYSTAL:set_asym_from_ufrag_cov_mx ... no covariance matrix")

      ! *Inverse* symops (see below)
      seitz = transposed_xyz_seitz_matrices_(self)

      ! Create asym atom frag covariance matrix
      dim = size(self%xray_data%covariance_mx,1)
      call create_(acov,dim,dim)
      acov = ZERO

      ! Transform the unique .fragment_atom to the asym unit covariances
      u1b = 0
      do u1 = 1,self%n_unique_frag_atoms

         ! s1 maps: asym atom a1 -> frag atom f1, but use transpose
         f1 = self%unique_frag_atom(u1)
         a1 = self%asym_atom_for_frag_atom(f1)
         s1 = self%asym_symop_for_frag_atom(f1)

         ! Block limits
         n1 = no_of_pADPs_(self%asymmetric_unit_atom(a1))
         a1b = no_of_pADPs_up_to_atom_(self%asymmetric_unit_atom,a1)
         a1f = a1b+1
         a1l = a1b+n1
         u1f = u1b+1
         u1l = u1b+n1

         ! Transformation matrix
         call create_(T1,n1,n1)
         call symmetric_tensor_product_mx_(T1,seitz(:,:,s1))
         u2b = 0
         do u2 = 1,self%n_unique_frag_atoms

            ! s2 maps: asym atom a2 -> frag atom f2, but use transpose
            f2 = self%unique_frag_atom(u2)
            a2 = self%asym_atom_for_frag_atom(f2)
            s2 = self%asym_symop_for_frag_atom(f2)

            ! Block limits
            n2 = no_of_pADPs_(self%asymmetric_unit_atom(a2))
            a2b = no_of_pADPs_up_to_atom_(self%asymmetric_unit_atom,a2)
            a2f = a2b+1
            a2l = a2b+n2
            u2f = u2b+1
            u2l = u2b+n2

            ! Transform covariances: ufrag -> asym atom
            call create_(T2,n2,n2)
            call symmetric_tensor_product_mx_(T2,seitz(:,:,s2))
            call back_transform_to_(self%xray_data%covariance_mx(u1f:u1l,u2f:u2l),acov(a1f:a1l,a2f:a2l),T1,T2)
            call destroy_(T2)

            ! Increment
            u2b = u2b+n2
         end do

         ! Increment
         u1b = u1b+n1
      end do

      ! Overwrite ufrag-atom cov mx with asym-atom cov mx
      self%xray_data%covariance_mx = acov  ! <<<<<<

      ! Clean
      call destroy_(acov)
      call destroy_(seitz)
   end subroutine

   PURE subroutine set_frag_from_asym_cov_mx(self)
   ! Set the total frag atom covariance matrix from the asym atom
   ! covariance matrix. Seems correct 12/9/11
      type(CRYSTAL_TYPE), INOUT :: self
      INT :: f1,a1,s1,n1,f1b,f1f,f1l,a1b,a1f,a1l
      INT :: f2,a2,s2,n2,f2b,f2f,f2l,a2b,a2f,a2l
      MAT(REAL,:,:), allocatable :: T1,T2
      MAT3(REAL,:,:,:), allocatable :: seitz
   ENSURE(self%fragment_info_made,"CRYSTAL:set_frag_from_asym_cov_mx ... no fragment info")
   ENSURE(allocated(self%xray_data),"CRYSTAL:set_frag_from_asym_cov_mx ... no data")
   ENSURE(allocated(self%xray_data%covariance_mx),"CRYSTAL:set_frag_from_asym_cov_mx ... no covariance matrix")
   ENSURE(allocated(self%xray_data%fragment_covariance_mx),"CRYSTAL:set_frag_from_asym_cov_mx ... no fragment covariance matrix")

      ! Symops
      seitz = xyz_seitz_matrices_(self)

      ! Copy the asymmetric atom covariances to *all* .fragment_atom ADPs
      f1b = 0
      do f1 = 1,self%n_fragment_atoms

         ! s1 maps: asym atom a1 -> frag atom f1
         a1 = self%asym_atom_for_frag_atom(f1)
         s1 = self%asym_symop_for_frag_atom(f1)

         ! Block limits
         n1 = no_of_pADPs_(self%asymmetric_unit_atom(a1))
         a1b = no_of_pADPs_up_to_atom_(self%asymmetric_unit_atom,a1)
         a1f = a1b+1
         a1l = a1b+n1
         f1f = f1b+1
         f1l = f1b+n1

         ! Transformation matrix
         call create_(T1,n1,n1)
         call symmetric_tensor_product_mx_(T1,seitz(:,:,s1))
         f2b = 0
         do f2 = 1,self%n_fragment_atoms

            ! s2 maps: asym atom a2 -> frag atom f2
            a2 = self%asym_atom_for_frag_atom(f2)
            s2 = self%asym_symop_for_frag_atom(f2)

            ! Block limits
            n2 = no_of_pADPs_(self%asymmetric_unit_atom(a2))
            a2b = no_of_pADPs_up_to_atom_(self%asymmetric_unit_atom,a2)
            a2f = a2b+1
            a2l = a2b+n2
            f2f = f2b+1
            f2l = f2b+n2

            ! Transformation matrix
            call create_(T2,n2,n2)
            call symmetric_tensor_product_mx_(T2,seitz(:,:,s2))
            call back_transform_to_(self%xray_data%covariance_mx(a1f:a1l,a2f:a2l),self%xray_data%fragment_covariance_mx(f1f:f1l,f2f:f2l),T1,T2)
            call destroy_(T2)
            f2b = f2b+n2
         end do

         ! Clean, increment
         call destroy_(T1)
         f1b = f1b+n1
      end do

      ! Clean
      call destroy_(seitz)
   end subroutine


!  Structure factors only

   NON_MPI_PURE subroutine make_unique_sf_0(self,sf_e,ff)
   ! Make the *temperature dependent* unique structure factors and
   ! their derivatives "sf_e" corresponding to the unique_SF_k_pts
   ! from the temperature independent atomic form factors, "ff", and
   ! atom parameters in .fragment_atom.
      type(CRYSTAL_TYPE), IN :: self
      VEC(CPX,:), OUT :: sf_e
      MAT(CPX,:,:), IN :: ff
      MAT(REAL,3,3) :: adp2
      MAT3(REAL,3,3,3) :: adp3
      MAT4(REAL,3,3,3,3) :: adp4
      MAT(REAL,:,:), allocatable :: k_pt
      VEC(REAL,3) :: kv
      REAL :: c2,c3,c4
      INT :: n_k,f,u,k
      BIN :: do_ADP3s,do_ADP4s
      CPX :: II,sf2,sf3,sf4
   ENSURE(size(sf_e)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_sf_0 ... sf_e, wrong dim")
   ENSURE(size(ff,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_sf_0 ... ff, wrong dim1")
   ENSURE(size(ff,2)==size(self%asymmetric_unit_atom),"CRYSTAL:make_unique_sf_0 ... ff, wrong dim2")
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_unique_sf_0 ... no x-ray reflection data")

      ! Constants
      II = IMAGIFY(ONE)
      n_k = size(ff,1)

      ! Symmetry generated k points
      call create_(k_pt,n_k,3)
      call make_unique_SF_k_pts_(self%xray_data%reflections,k_pt,self%spacegroup,self%unit_cell)

      ! Initialize
      sf_e = ZERO

      ! Make the structure factors
      do u = PARALLEL_DO_START(1,1),self%n_unique_frag_atoms,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("CRYSTAL:make_unique_sf_0")

         ! Fragment atom "F"
         f = self%unique_frag_atom(u)

         ! Doing anharmomic terms?
         do_ADP4s = has_only_ADP4s_and_errors_(self%fragment_atom(f))
         do_ADP3s = has_only_ADP3s_and_errors_(self%fragment_atom(f))

      ! stdout.text("pADPs:")
      ! stdout.put(.fragment_atom(f).pADP_vector)

         ! Quartic terms (if any)
         if (do_ADP4s) then
            call put_ADP2_vector_to_(self%fragment_atom(f),adp2)
            call put_ADP3_vector_to_(self%fragment_atom(f),adp3)
            call put_ADP4_vector_to_(self%fragment_atom(f),adp4)
            do k = 1,n_k
               kv = k_pt(k,:)

               ! ADP2s & ADP4s
               c2 = contract_with_(adp2,kv)*HALF
               c3 = contract_with_(adp3,kv)*SIXTH
               c4 = contract_with_(adp4,kv)*SIXTH*QUARTER

               ! Quartic term only
               sf2 = ff(k,u)*exp(-c2)
               sf4 = sf2*(ONE-II*c3+c4)

               ! SF contribution
               sf_e(k) = sf_e(k)+sf4
            end do
         else if (do_ADP3s) then
            call put_ADP2_vector_to_(self%fragment_atom(f),adp2)
            call put_ADP3_vector_to_(self%fragment_atom(f),adp3)
            do k = 1,n_k
               kv = k_pt(k,:)

               ! ADP2s & ADP4s
               c2 = contract_with_(adp2,kv)*HALF
               c3 = contract_with_(adp3,kv)*SIXTH

               ! Quartic term only
               sf2 = ff(k,u)*exp(-c2)
               sf3 = sf2*(ONE-II*c3)

               ! SF contribution
               sf_e(k) = sf_e(k)+sf3
            end do
         else

            ! ADP2s
            call put_ADP2_vector_to_(self%fragment_atom(f),adp2)
            do k = 1,n_k
               kv = k_pt(k,:)

               ! U tensor contracted with k vector
               c2 = contract_with_(adp2,kv)*HALF

               ! Static structure factor x TF
               ! i.e. dynamic structure factor,
               ! in the harmonic approximation
               sf2 = ff(k,u)*exp(-c2)

               ! SF contribution
               sf_e(k) = sf_e(k)+sf2
            end do
         end if
      end do  ! loop over unqique/asym atoms "u"
      UNLOCK_PARALLEL_DO("CRYSTAL:make_unique_sf_0")
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(sf_e)
      end if

      ! Clean
      call destroy_(k_pt)
   end subroutine

   subroutine make_unique_sf_1(self,sf_e)
   ! Make the *temperature dependent* unique structure factors and
   ! their derivatives "sf_e" corresponding to the unique_SF_k_pts
   ! and from atom parameters in .fragment_atom.
   ! NOTE: this version reads SFs from the disk
      type(CRYSTAL_TYPE), IN :: self
      VEC(CPX,:), OUT :: sf_e
      VEC(CPX,:), allocatable :: sf_u
      type(ARCHIVE_TYPE), allocatable :: arch
      MAT(REAL,3,3) :: adp2
      MAT3(REAL,3,3,3) :: adp3
      MAT4(REAL,3,3,3,3) :: adp4
      MAT(REAL,:,:), allocatable :: k_pt
      VEC(REAL,3) :: kv
      REAL :: c2,c3,c4
      INT :: n_k,f,u,k
      BIN :: do_ADP3s,do_ADP4s
      CPX :: II,sf2,sf3,sf4
   ENSURE(size(sf_e)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_sf_1 ... sf_e, wrong dim")
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_unique_sf_1 ... no x-ray reflection data")

      ! Constants
      II = IMAGIFY(ONE)
      n_k = n_unique_SF_k_pts_(self)

      ! Symmetry generated k points
      call create_(k_pt,n_k,3)
      call make_unique_SF_k_pts_(self%xray_data%reflections,k_pt,self%spacegroup,self%unit_cell)

      ! K times thermal tensors
      call create_(sf_u,n_k)

      ! Initialize
      sf_e = ZERO

      ! Make the structure factors
      do u = PARALLEL_DO_START(1,1),self%n_unique_frag_atoms,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("CRYSTAL:make_unique_sf_1")

         ! Fragment atom "F"
         f = self%unique_frag_atom(u)

         ! Doing anharmomic terms?
         do_ADP4s = has_only_ADP4s_and_errors_(self%fragment_atom(f))
         do_ADP3s = has_only_ADP3s_and_errors_(self%fragment_atom(f))

         ! Read stored SFS
         call create_(arch,trim(self%fragment_atom(f)%tag)//"-SFs")
         if (self%xray_data%use_text_SFs) call set_format_(arch,"ascii")
         call read_(arch,sf_u)
         call destroy_(arch)

         ! Quartic terms (if any)
         if (do_ADP4s) then
            call put_ADP2_vector_to_(self%fragment_atom(f),adp2)
            call put_ADP3_vector_to_(self%fragment_atom(f),adp3)
            call put_ADP4_vector_to_(self%fragment_atom(f),adp4)
            do k = 1,n_k
               kv = k_pt(k,:)

               ! ADP2s & ADP4s
               c2 = contract_with_(adp2,kv)*HALF
               c3 = contract_with_(adp3,kv)*SIXTH
               c4 = contract_with_(adp4,kv)*SIXTH*QUARTER

               ! Quartic term only
               sf2 = sf_u(k)*exp(-c2)
               sf4 = sf2*(ONE-II*c3+c4)

               ! SF contribution
               sf_e(k) = sf_e(k)+sf4
            end do
         else if (do_ADP3s) then
            call put_ADP2_vector_to_(self%fragment_atom(f),adp2)
            call put_ADP3_vector_to_(self%fragment_atom(f),adp3)
            do k = 1,n_k
               kv = k_pt(k,:)

               ! ADP2s & ADP4s
               c2 = contract_with_(adp2,kv)*HALF
               c3 = contract_with_(adp3,kv)*SIXTH

               ! Quartic term only
               sf2 = sf_u(k)*exp(-c2)
               sf3 = sf2*(ONE-II*c3)

               ! SF contribution
               sf_e(k) = sf_e(k)+sf3
            end do
         else

            ! ADP2s
            call put_ADP2_vector_to_(self%fragment_atom(f),adp2)
            do k = 1,n_k
               kv = k_pt(k,:)

               ! U tensor contracted with k vector
               c2 = contract_with_(adp2,kv)*HALF

               ! Static structure factor x TF
               ! i.e. dynamic structure factor,
               ! in the harmonic approximation
               sf2 = sf_u(k)*exp(-c2)

               ! SF contribution
               sf_e(k) = sf_e(k)+sf2
            end do
         end if
      end do  ! loop over unqique/asym atoms "u"
      UNLOCK_PARALLEL_DO("CRYSTAL:make_unique_sf_1")
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(sf_e)
      end if

      ! Clean
      call destroy_(sf_u)
      call destroy_(k_pt)
   end subroutine


!  SF derivatives 

   NON_MPI_PURE subroutine make_unique_sf_derivs_0(self,sf_e,sf_d,ff)
   ! Make the unique structure factors and their derivatives "sf_e"
   ! and "sf_d" corresponding to the unique_SF_k_pts from the given
   ! set of temperature-independent atomic form factors "ff", 

   !   - refining U_iso for atoms in .refine_U_iso_for_atom list
   !   - The rest are refined *An*isotropically unless
   !     you specify refine_H_positions or refine_H_ADPs FALSE
   ! Atom parameters are those in .fragment_atom.
      type(CRYSTAL_TYPE), IN :: self
      VEC(CPX,:), OUT :: sf_e
      MAT(CPX,:,:), OUT :: sf_d
      MAT(CPX,:,:), IN :: ff
      VEC(STR(len=STR_SIZE),:), allocatable :: list
      VEC(STR(len=STR_SIZE),:), allocatable :: tags
      VEC(CPX,:), allocatable :: sf_u
      VEC(REAL,3) :: kv
      MAT(REAL,:,:), allocatable :: k_pt
      MAT(REAL,3,3) :: adp2
      MAT3(REAL,3,3,3) :: adp3
      MAT4(REAL,3,3,3,3) :: adp4
      REAL :: k1,k2,k3,c2,c3,c4
      REAL :: k1_k1,k2_k2,k3_k3,k1_k2,k1_k3,k2_k3
      REAL :: zero3
      INT :: n_k,f,u,k,base
      BIN :: has_skip,has_Uiso,is_H_atom
      BIN :: refine_positions,refine_Uiso
      BIN :: refine_positions_only,refine_ADPs_only
      BIN :: refine_ADPs,refine_ADP4s,refine_ADP3s
      CPX :: II,sfh,sf1,sf2,sf4,sf6,sf24
      STR(len=STR_SIZE) :: tag
   ENSURE(size(sf_e)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_sf_derivs_0 ... sf_e, wrong dim")
   ENSURE(size(sf_d,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_sf_derivs_0 ... sf_d, wrong dim1")
   ENSURE(size(sf_d,2)==no_of_pADPs_(self%asymmetric_unit_atom),"CRYSTAL:make_unique_sf_derivs_0 ... sf_d, wrong dim2")
   ENSURE(size(ff,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_sf_derivs_0 ... ff, wrong dim1")
   ENSURE(size(ff,2)==size(self%asymmetric_unit_atom),"CRYSTAL:make_unique_sf_derivs_0 ... ff, wrong dim2")
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_unique_sf_derivs_0 ... no x-ray reflection data")

      ! Is there a nothing list?
      has_skip = allocated(self%xray_data%refine_nothing_for_atom)
      if (has_skip) then
         tags = self%fragment_atom(:)%tag
         list = self%xray_data%refine_nothing_for_atom
         ENSURE(has_all_elements_common_with_(list,tags),"CRYSTAL:make_unique_sf_derivs_0 ... trying to refine nothing for an atom that is not in your list")
      end if

      ! Are there atoms to be refined isotropically?
      has_Uiso = allocated(self%xray_data%refine_U_iso_for_atom)
      if (has_Uiso) then
         tags = self%fragment_atom(:)%tag
         list = self%xray_data%refine_U_iso_for_atom
         ENSURE(has_all_elements_common_with_(list,tags),"CRYSTAL:make_unique_sf_derivs_0 ... trying to refine an atom isotropically that is not in your list")
      end if

      ! Constants
      II = IMAGIFY(ONE)
      n_k = n_unique_SF_k_pts_(self)

      ! Symmetry generated k points
      call create_(k_pt,n_k,3)
      call make_unique_SF_k_pts_(self%xray_data%reflections,k_pt,self%spacegroup,self%unit_cell)

      ! Refine only 4th's?
      zero3 = ONE
      if (self%xray_data%refine_4th_order_only) zero3 = ZERO

      ! Initialize
      sf_e = ZERO
      sf_d = ZERO

      ! Make the structure factors and derivatives
      do u = PARALLEL_DO_START(1,1),self%n_unique_frag_atoms,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("CRYSTAL:make_unique_sf_derivs_0")

         ! Fragment atom
         f = self%unique_frag_atom(u)

         ! This atom's label
         tag = self%fragment_atom(f)%tag

         ! Structure factor, unique frag atom "u"
         sf_u = ff(:,u)

         ! Base index for unique frag atom
         !base = .fragment_atom(.unique_frag_atom).no_of_pADPs_up_to_atom(u)
         base = no_of_pADPs_up_to_atom_(self%fragment_atom,u,self%unique_frag_atom)

         ! Which of this atom's parameters are refined
         call refinement_flags_for_frag_atom_(self,f,refine_positions,refine_ADPs,refine_Uiso)
         refine_ADP4s = has_only_ADP4s_and_errors_(self%fragment_atom(f))
         refine_ADP3s = has_only_ADP3s_and_errors_(self%fragment_atom(f))

         ! Get ADP2s
         call put_ADP2_vector_to_(self%fragment_atom(f),adp2)

         ! Sanity check
         ENSURE(equals_(adp2,self%fragment_atom(f)%ADP_tensor),"CRYSTAL:make_unique_sf_derivs_0 ... ADP2 not equal to ADP_tensor")

         ! ADP4's !!!!!!!!!!!!!
         if (refine_ADP4s) then

         !!!!!!!!!!!!!!!!!!!!!!

            ! Get ADP tensors
            call put_ADP3_vector_to_(self%fragment_atom(f),adp3)
            call put_ADP4_vector_to_(self%fragment_atom(f),adp4)
            do k = 1,n_k

               ! Scattering vector
               k1 = k_pt(k,1)
               k2 = k_pt(k,2)
               k3 = k_pt(k,3)
               kv = [k1,k2,k3]

               ! Component products
               k1_k1 = k1*k1
               k2_k2 = k2*k2
               k3_k3 = k3*k3
               k1_k2 = k1*k2
               k1_k3 = k1*k3
               k2_k3 = k2*k3

               ! U tensor contracted with k vector
               c2 = contract_with_(adp2,kv)*HALF
               c3 = contract_with_(adp3,kv)*SIXTH
               c4 = contract_with_(adp4,kv)*SIXTH*QUARTER

               ! Static structure factor x TF
               ! i.e. dynamic structure factor,
               ! in the harmonic approximation
               sfh = sf_u(k)*exp(-c2)

               ! Multiples of *anharmonic* structure factor
               sf1 = sfh*(ONE-II*c3+c4)
               sf2 = sf1*HALF

               ! SF contribution (anharmonic!)
               sf_e(k) = sf_e(k)+sf1
               if (refine_positions) then
                  sf_d(k,base+1) = II*k1*sf1
                  sf_d(k,base+2) = II*k2*sf1
                  sf_d(k,base+3) = II*k3*sf1
               end if
               if (refine_ADPs) then

                  ! U2 derivatives
                  sf_d(k,base+4) = -k1_k1*sf2
                  sf_d(k,base+5) = -k2_k2*sf2
                  sf_d(k,base+6) = -k3_k3*sf2
                  sf_d(k,base+7) = -k1_k2*sf1
                  sf_d(k,base+8) = -k1_k3*sf1
                  sf_d(k,base+9) = -k2_k3*sf1

                  ! Structure factor (harmonic)
                  sf1 = sfh

                  ! Multiples of *harmonic* sf
                  sf2 = sf1*HALF
                  sf4 = sf1*QUARTER
                  sf6 = sf1*SIXTH
                  sf24 = sf1*SIXTH*QUARTER

                  ! U3 derivatives
                  sf_d(k,base+10) = -II*k1_k1*k1*sf6*zero3
                  sf_d(k,base+11) = -II*k2_k2*k2*sf6*zero3
                  sf_d(k,base+12) = -II*k3_k3*k3*sf6*zero3
                  sf_d(k,base+13) = -II*k1_k1*k2*sf2*zero3
                  sf_d(k,base+14) = -II*k1_k1*k3*sf2*zero3
                  sf_d(k,base+15) = -II*k2_k2*k1*sf2*zero3
                  sf_d(k,base+16) = -II*k2_k2*k3*sf2*zero3
                  sf_d(k,base+17) = -II*k3_k3*k1*sf2*zero3
                  sf_d(k,base+18) = -II*k3_k3*k2*sf2*zero3
                  sf_d(k,base+19) = -II*k1*k2*k3*sf1*zero3

                  ! U4 derivatives
                  sf_d(k,base+20) = k1_k1*k1_k1*sf24
                  sf_d(k,base+21) = k2_k2*k2_k2*sf24
                  sf_d(k,base+22) = k3_k3*k3_k3*sf24
                  sf_d(k,base+23) = k1_k1*k1_k2*sf6
                  sf_d(k,base+24) = k1_k1*k1_k3*sf6
                  sf_d(k,base+25) = k2_k2*k1_k2*sf6
                  sf_d(k,base+26) = k2_k2*k2_k3*sf6
                  sf_d(k,base+27) = k3_k3*k1_k3*sf6
                  sf_d(k,base+28) = k3_k3*k2_k3*sf6
                  sf_d(k,base+29) = k1_k1*k2_k2*sf4
                  sf_d(k,base+30) = k1_k1*k3_k3*sf4
                  sf_d(k,base+31) = k2_k2*k3_k3*sf4
                  sf_d(k,base+32) = k1_k1*k2_k3*sf2
                  sf_d(k,base+33) = k2_k2*k1_k3*sf2
                  sf_d(k,base+34) = k3_k3*k1_k2*sf2
               end if
            end do

         ! ADP3's !!!!!!!!!!!!!!!!!!
         else if (refine_ADP3s) then

         !!!!!!!!!!!!!!!!!!!!!!!!!!!

            ! Get ADP tensors
            call put_ADP3_vector_to_(self%fragment_atom(f),adp3)
            do k = 1,n_k

               ! Scattering vector
               k1 = k_pt(k,1)
               k2 = k_pt(k,2)
               k3 = k_pt(k,3)
               kv = [k1,k2,k3]

               ! Component products
               k1_k1 = k1*k1
               k2_k2 = k2*k2
               k3_k3 = k3*k3
               k1_k2 = k1*k2
               k1_k3 = k1*k3
               k2_k3 = k2*k3

               ! U tensor contracted with k vector
               c2 = contract_with_(adp2,kv)*HALF
               c3 = contract_with_(adp3,kv)*SIXTH

               ! Static structure factor x TF
               ! i.e. dynamic structure factor,
               ! in the harmonic approximation
               sfh = sf_u(k)*exp(-c2)

               ! Multiples of *anharmonic* structure factor
               ! -- 3rd order only.
               sf1 = sfh*(ONE-II*c3)
               sf2 = sf1*HALF

               ! SF contribution (anharmonic!)
               sf_e(k) = sf_e(k)+sf1

               !test refine_H_positions Lorraine
               if (refine_positions) then

                  ! Pos derivatives
                  sf_d(k,base+1) = II*k1*sf1
                  sf_d(k,base+2) = II*k2*sf1
                  sf_d(k,base+3) = II*k3*sf1
               end if
               if (refine_ADPs) then

                  ! U2 derivatives
                  sf_d(k,base+4) = -k1_k1*sf2
                  sf_d(k,base+5) = -k2_k2*sf2
                  sf_d(k,base+6) = -k3_k3*sf2
                  sf_d(k,base+7) = -k1_k2*sf1
                  sf_d(k,base+8) = -k1_k3*sf1
                  sf_d(k,base+9) = -k2_k3*sf1

                  ! Structure factor (harmonic!)
                  sf1 = sfh

                  ! Multiples of *harmonic* sf
                  sf2 = sf1*HALF
                  sf6 = sf1*SIXTH

                  ! U3 derivatives
                  sf_d(k,base+10) = -II*k1_k1*k1*sf6
                  sf_d(k,base+11) = -II*k2_k2*k2*sf6
                  sf_d(k,base+12) = -II*k3_k3*k3*sf6
                  sf_d(k,base+13) = -II*k1_k1*k2*sf2
                  sf_d(k,base+14) = -II*k1_k1*k3*sf2
                  sf_d(k,base+15) = -II*k2_k2*k1*sf2
                  sf_d(k,base+16) = -II*k2_k2*k3*sf2
                  sf_d(k,base+17) = -II*k3_k3*k1*sf2
                  sf_d(k,base+18) = -II*k3_k3*k2*sf2
                  sf_d(k,base+19) = -II*k1*k2*k3*sf1
               end if
            end do

         ! ADP2's !!!!!!!!!!!!!!
         else  ! NO anharmonicity

         !!!!!!!!!!!!!!!!!!!!!!!
            do k = 1,n_k

               ! Scattering vector
               k1 = k_pt(k,1)
               k2 = k_pt(k,2)
               k3 = k_pt(k,3)
               kv = [k1,k2,k3]

               ! Component products
               k1_k1 = k1*k1
               k2_k2 = k2*k2
               k3_k3 = k3*k3
               k1_k2 = k1*k2
               k1_k3 = k1*k3
               k2_k3 = k2*k3

               ! U tensor contracted with k vector
               c2 = contract_with_(adp2,kv)*HALF

               ! Static structure factor x TF
               ! i.e. dynamic structure factor,
               ! in the harmonic approximation
               sfh = sf_u(k)*exp(-c2)

               ! Multiples *harmonic* structure factor
               sf1 = sfh
               sf2 = sf1*HALF

               ! SF contribution (harmonic)
               sf_e(k) = sf_e(k)+sf1
               if (refine_positions) then

                  ! Pos derivatives
                  sf_d(k,base+1) = II*k1*sf1
                  sf_d(k,base+2) = II*k2*sf1
                  sf_d(k,base+3) = II*k3*sf1
               end if
               if (refine_ADPs) then

                  ! U2 derivatives
                  sf_d(k,base+4) = -k1_k1*sf2
                  sf_d(k,base+5) = -k2_k2*sf2
                  sf_d(k,base+6) = -k3_k3*sf2
                  sf_d(k,base+7) = -k1_k2*sf1
                  sf_d(k,base+8) = -k1_k3*sf1
                  sf_d(k,base+9) = -k2_k3*sf1
               end if
            end do
         end if
      end do
      UNLOCK_PARALLEL_DO("CRYSTAL:make_unique_sf_derivs_0")

      ! Clean
      call destroy_(k_pt)
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(sf_e)
         PARALLEL_SUM(sf_d)
      end if
   end subroutine

   subroutine make_unique_sf_derivs_1(self,sf_e,sf_d)
   ! Make the unique structure factors and their derivatives "sf_e"
   ! and "sf_d" corresponding to the unique_SF_k_pts from the given
   ! set of temperature-independent form factors, on disk.

   !   - refining U_iso for atoms in .refine_U_iso_for_atom list
   !   - The rest are refined *An*isotropically unless
   !     you specify refine_H_positions or refine_H_ADPs FALSE
   ! Atom parameters are those in .fragment_atom.
      type(CRYSTAL_TYPE), IN :: self
      VEC(CPX,:), OUT :: sf_e
      MAT(CPX,:,:), OUT :: sf_d
      VEC(STR(len=STR_SIZE),:), allocatable :: list
      VEC(STR(len=STR_SIZE),:), allocatable :: tags
      VEC(CPX,:), allocatable :: sf_u
      VEC(REAL,3) :: kv
      MAT(REAL,:,:), allocatable :: k_pt
      MAT(REAL,3,3) :: adp2
      MAT3(REAL,3,3,3) :: adp3
      MAT4(REAL,3,3,3,3) :: adp4
      REAL :: k1,k2,k3,c2,c3,c4
      REAL :: k1_k1,k2_k2,k3_k3,k1_k2,k1_k3,k2_k3
      REAL :: zero3
      INT :: n_k,f,u,k,base
      BIN :: has_skip,has_Uiso,is_H_atom
      BIN :: refine_positions,refine_Uiso
      BIN :: refine_positions_only,refine_ADPs_only
      BIN :: refine_ADPs,refine_ADP4s,refine_ADP3s
      CPX :: II,sfh,sf1,sf2,sf4,sf6,sf24
      STR(len=STR_SIZE) :: tag
      type(ARCHIVE_TYPE), allocatable :: arch
   ENSURE(size(sf_e)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_sf_derivs_1 ... sf_e, wrong dim")
   ENSURE(size(sf_d,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_sf_derivs_1 ... sf_d, wrong dim1")
   ENSURE(size(sf_d,2)==no_of_pADPs_(self%asymmetric_unit_atom),"CRYSTAL:make_unique_sf_derivs_1 ... sf_d, wrong dim2")
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_unique_sf_derivs_1 ... no x-ray reflection data")

      ! Is there a nothing list?
      has_skip = allocated(self%xray_data%refine_nothing_for_atom)
      if (has_skip) then
         tags = self%fragment_atom(:)%tag
         list = self%xray_data%refine_nothing_for_atom
         ENSURE(has_all_elements_common_with_(list,tags),"CRYSTAL:make_unique_sf_derivs_1 ... trying to refine nothing for an atom that is not in your list")
      end if

      ! Are there atoms to be refined isotropically?
      has_Uiso = allocated(self%xray_data%refine_U_iso_for_atom)
      if (has_Uiso) then
         tags = self%fragment_atom(:)%tag
         list = self%xray_data%refine_U_iso_for_atom
         ENSURE(has_all_elements_common_with_(list,tags),"CRYSTAL:make_unique_sf_derivs_1 ... trying to refine an atom isotropically that is not in your list")
      end if

      ! Constants
      II = IMAGIFY(ONE)
      n_k = n_unique_SF_k_pts_(self)

      ! Aspherical temperature independent SFs

      ! Symmetry generated k points
      call create_(k_pt,n_k,3)
      call make_unique_SF_k_pts_(self%xray_data%reflections,k_pt,self%spacegroup,self%unit_cell)

      ! Refine only 4th's?
      zero3 = ONE
      if (self%xray_data%refine_4th_order_only) zero3 = ZERO

      ! Initialize
      sf_e = ZERO
      sf_d = ZERO

      ! Make the structure factors and derivatives
      do u = 1,self%n_unique_frag_atoms
         call create_(sf_u,n_k)

         ! Fragment atom
         f = self%unique_frag_atom(u)

         ! This atom's label
         tag = self%fragment_atom(f)%tag

         ! Read stored SFS !!!!!!!!!!!!
         call create_(arch,trim(tag)//"-SFs")
         if (self%xray_data%use_text_SFs) call set_format_(arch,"ascii")
         call read_(arch,sf_u)
         call destroy_(arch)
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         ! Base index for unique frag atom
      ! base = .fragment_atom(.unique_frag_atom).no_of_pADPs_up_to_atom(u)
         base = no_of_pADPs_up_to_atom_(self%fragment_atom,u,self%unique_frag_atom)

         ! Which of this atom's parameters are refined
         call refinement_flags_for_frag_atom_(self,f,refine_positions,refine_ADPs,refine_Uiso)
         refine_ADP4s = has_only_ADP4s_and_errors_(self%fragment_atom(f))
         refine_ADP3s = has_only_ADP3s_and_errors_(self%fragment_atom(f))

         ! Get ADP2s
         call put_ADP2_vector_to_(self%fragment_atom(f),adp2)

         ! Sanity check
         ENSURE(equals_(adp2,self%fragment_atom(f)%ADP_tensor),"CRYSTAL:make_unique_sf_derivs_1 ... ADP2 not equal to ADP_tensor")

         ! ADP4's !!!!!!!!!!!!!
         if (refine_ADP4s) then

         !!!!!!!!!!!!!!!!!!!!!!

            ! Get ADP tensors
            call put_ADP3_vector_to_(self%fragment_atom(f),adp3)
            call put_ADP4_vector_to_(self%fragment_atom(f),adp4)
            do k = 1,n_k

               ! Scattering vector
               k1 = k_pt(k,1)
               k2 = k_pt(k,2)
               k3 = k_pt(k,3)
               kv = [k1,k2,k3]

               ! Component products
               k1_k1 = k1*k1
               k2_k2 = k2*k2
               k3_k3 = k3*k3
               k1_k2 = k1*k2
               k1_k3 = k1*k3
               k2_k3 = k2*k3

               ! U tensor contracted with k vector
               c2 = contract_with_(adp2,kv)*HALF
               c3 = contract_with_(adp3,kv)*SIXTH
               c4 = contract_with_(adp4,kv)*SIXTH*QUARTER

               ! Static structure factor x TF
               ! i.e. dynamic structure factor,
               ! in the harmonic approximation
               sfh = sf_u(k)*exp(-c2)

               ! Multiples of *anharmonic* structure factor
               sf1 = sfh*(ONE-II*c3+c4)
               sf2 = sf1*HALF

               ! SF contribution (anharmonic!)
               sf_e(k) = sf_e(k)+sf1
               if (refine_positions) then
                  sf_d(k,base+1) = II*k1*sf1
                  sf_d(k,base+2) = II*k2*sf1
                  sf_d(k,base+3) = II*k3*sf1
               end if
               if (refine_ADPs) then

                  ! U2 derivatives
                  sf_d(k,base+4) = -k1_k1*sf2
                  sf_d(k,base+5) = -k2_k2*sf2
                  sf_d(k,base+6) = -k3_k3*sf2
                  sf_d(k,base+7) = -k1_k2*sf1
                  sf_d(k,base+8) = -k1_k3*sf1
                  sf_d(k,base+9) = -k2_k3*sf1

                  ! Structure factor (harmonic)
                  sf1 = sfh

                  ! Multiples of *harmonic* sf
                  sf2 = sf1*HALF
                  sf4 = sf1*QUARTER
                  sf6 = sf1*SIXTH
                  sf24 = sf1*SIXTH*QUARTER

                  ! U3 derivatives
                  sf_d(k,base+10) = -II*k1_k1*k1*sf6*zero3
                  sf_d(k,base+11) = -II*k2_k2*k2*sf6*zero3
                  sf_d(k,base+12) = -II*k3_k3*k3*sf6*zero3
                  sf_d(k,base+13) = -II*k1_k1*k2*sf2*zero3
                  sf_d(k,base+14) = -II*k1_k1*k3*sf2*zero3
                  sf_d(k,base+15) = -II*k2_k2*k1*sf2*zero3
                  sf_d(k,base+16) = -II*k2_k2*k3*sf2*zero3
                  sf_d(k,base+17) = -II*k3_k3*k1*sf2*zero3
                  sf_d(k,base+18) = -II*k3_k3*k2*sf2*zero3
                  sf_d(k,base+19) = -II*k1*k2*k3*sf1*zero3

                  ! U4 derivatives
                  sf_d(k,base+20) = k1_k1*k1_k1*sf24
                  sf_d(k,base+21) = k2_k2*k2_k2*sf24
                  sf_d(k,base+22) = k3_k3*k3_k3*sf24
                  sf_d(k,base+23) = k1_k1*k1_k2*sf6
                  sf_d(k,base+24) = k1_k1*k1_k3*sf6
                  sf_d(k,base+25) = k2_k2*k1_k2*sf6
                  sf_d(k,base+26) = k2_k2*k2_k3*sf6
                  sf_d(k,base+27) = k3_k3*k1_k3*sf6
                  sf_d(k,base+28) = k3_k3*k2_k3*sf6
                  sf_d(k,base+29) = k1_k1*k2_k2*sf4
                  sf_d(k,base+30) = k1_k1*k3_k3*sf4
                  sf_d(k,base+31) = k2_k2*k3_k3*sf4
                  sf_d(k,base+32) = k1_k1*k2_k3*sf2
                  sf_d(k,base+33) = k2_k2*k1_k3*sf2
                  sf_d(k,base+34) = k3_k3*k1_k2*sf2
               end if
            end do

         ! ADP3's !!!!!!!!!!!!!!!!!!
         else if (refine_ADP3s) then

         !!!!!!!!!!!!!!!!!!!!!!!!!!!

            ! Get ADP tensors
            call put_ADP3_vector_to_(self%fragment_atom(f),adp3)
            do k = 1,n_k

               ! Scattering vector
               k1 = k_pt(k,1)
               k2 = k_pt(k,2)
               k3 = k_pt(k,3)
               kv = [k1,k2,k3]

               ! Component products
               k1_k1 = k1*k1
               k2_k2 = k2*k2
               k3_k3 = k3*k3
               k1_k2 = k1*k2
               k1_k3 = k1*k3
               k2_k3 = k2*k3

               ! U tensor contracted with k vector
               c2 = contract_with_(adp2,kv)*HALF
               c3 = contract_with_(adp3,kv)*SIXTH

               ! Static structure factor x TF
               ! i.e. dynamic structure factor,
               ! in the harmonic approximation
               sfh = sf_u(k)*exp(-c2)

               ! Multiples of *anharmonic* structure factor
               ! -- 3rd order only.
               sf1 = sfh*(ONE-II*c3)
               sf2 = sf1*HALF

               ! SF contribution (anharmonic!)
               sf_e(k) = sf_e(k)+sf1

               !test refine_H_positions Lorraine
               if (refine_positions) then

                  ! Pos derivatives
                  sf_d(k,base+1) = II*k1*sf1
                  sf_d(k,base+2) = II*k2*sf1
                  sf_d(k,base+3) = II*k3*sf1
               end if
               if (refine_ADPs) then

                  ! U2 derivatives
                  sf_d(k,base+4) = -k1_k1*sf2
                  sf_d(k,base+5) = -k2_k2*sf2
                  sf_d(k,base+6) = -k3_k3*sf2
                  sf_d(k,base+7) = -k1_k2*sf1
                  sf_d(k,base+8) = -k1_k3*sf1
                  sf_d(k,base+9) = -k2_k3*sf1

                  ! Structure factor (harmonic!)
                  sf1 = sfh

                  ! Multiples of *harmonic* sf
                  sf2 = sf1*HALF
                  sf6 = sf1*SIXTH

                  ! U3 derivatives
                  sf_d(k,base+10) = -II*k1_k1*k1*sf6
                  sf_d(k,base+11) = -II*k2_k2*k2*sf6
                  sf_d(k,base+12) = -II*k3_k3*k3*sf6
                  sf_d(k,base+13) = -II*k1_k1*k2*sf2
                  sf_d(k,base+14) = -II*k1_k1*k3*sf2
                  sf_d(k,base+15) = -II*k2_k2*k1*sf2
                  sf_d(k,base+16) = -II*k2_k2*k3*sf2
                  sf_d(k,base+17) = -II*k3_k3*k1*sf2
                  sf_d(k,base+18) = -II*k3_k3*k2*sf2
                  sf_d(k,base+19) = -II*k1*k2*k3*sf1
               end if
            end do

         ! ADP2's !!!!!!!!!!!!!!
         else  ! NO anharmonicity

         !!!!!!!!!!!!!!!!!!!!!!!
            do k = 1,n_k

               ! Scattering vector
               k1 = k_pt(k,1)
               k2 = k_pt(k,2)
               k3 = k_pt(k,3)
               kv = [k1,k2,k3]

               ! Component products
               k1_k1 = k1*k1
               k2_k2 = k2*k2
               k3_k3 = k3*k3
               k1_k2 = k1*k2
               k1_k3 = k1*k3
               k2_k3 = k2*k3

               ! U tensor contracted with k vector
               c2 = contract_with_(adp2,kv)*HALF

               ! Static structure factor x TF
               ! i.e. dynamic structure factor,
               ! in the harmonic approximation
               sfh = sf_u(k)*exp(-c2)

               ! Multiples *harmonic* structure factor
               sf1 = sfh
               sf2 = sf1*HALF

               ! SF contribution (harmonic)
               sf_e(k) = sf_e(k)+sf1
               if (refine_positions) then

                  ! Pos derivatives
                  sf_d(k,base+1) = II*k1*sf1
                  sf_d(k,base+2) = II*k2*sf1
                  sf_d(k,base+3) = II*k3*sf1
               end if
               if (refine_ADPs) then

                  ! U2 derivatives
                  sf_d(k,base+4) = -k1_k1*sf2
                  sf_d(k,base+5) = -k2_k2*sf2
                  sf_d(k,base+6) = -k3_k3*sf2
                  sf_d(k,base+7) = -k1_k2*sf1
                  sf_d(k,base+8) = -k1_k3*sf1
                  sf_d(k,base+9) = -k2_k3*sf1
               end if
            end do
         end if
         call destroy_(sf_u)
      end do

      ! Clean
      call destroy_(k_pt)
   end subroutine

   PURE subroutine make_unique_sf_derivs_for_atom(self,a,sf_e,sf_d,ff)
   ! Make the unique structure factors and their derivatives "sf_e"
   ! for the fragment atom correspondihnbg to asymm unit atom "a".
      type(CRYSTAL_TYPE), IN :: self
      INT, IN :: a
      VEC(CPX,:), OUT :: sf_e
      MAT(CPX,:,:), OUT :: sf_d
      VEC(CPX,:), IN :: ff
      VEC(REAL,3) :: kv
      MAT(REAL,:,:), allocatable :: k_pt
      MAT(REAL,3,3) :: adp2
      MAT3(REAL,3,3,3) :: adp3
      MAT4(REAL,3,3,3,3) :: adp4
      REAL :: k1,k2,k3,c2,c3,c4
      REAL :: k1_k1,k2_k2,k3_k3,k1_k2,k1_k3,k2_k3
      INT :: n_k,f,k,base
      BIN :: do_ADP3s,do_ADP4s
      CPX :: II,sfh,sf1,sf2,sf4,sf6,sf24
      STR(len=STR_SIZE) :: tag
   ENSURE(size(sf_e)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_sf_derivs_for_atom ... sf_e, wrong dim")
   ENSURE(size(sf_d,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_sf_derivs_for_atom ... sf_d, wrong dim1")
   ENSURE(size(sf_d,2)==no_of_pADPs_(self%asymmetric_unit_atom(a)),"CRYSTAL:make_unique_sf_derivs_for_atom ... sf_d, wrong dim2")
   ENSURE(size(ff,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_sf_derivs_for_atom ... ff, wrong dim1")
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_unique_sf_derivs_for_atom ... no x-ray reflection data")

      ! Constants
      II = IMAGIFY(ONE)
      n_k = n_unique_SF_k_pts_(self)

      ! Symmetry generated k points
      call create_(k_pt,n_k,3)
      call make_unique_SF_k_pts_(self%xray_data%reflections,k_pt,self%spacegroup,self%unit_cell)

      ! Initialize
      sf_e = ZERO
      sf_d = ZERO

      ! Fragment atom
      f = self%unique_frag_atom(a)

      ! This atom's label
      tag = self%fragment_atom(f)%tag

      ! Base index for unique frag atom
      base = 0

      ! Get ADP2s
      call put_ADP2_vector_to_(self%fragment_atom(f),adp2)

      ! Sanity check
      ENSURE(equals_(adp2,self%fragment_atom(f)%ADP_tensor),"CRYSTAL:make_unique_sf_derivs_for_atom ... ADP2 not equal to ADP_tensor")

      ! Doing anharmomic terms?
      do_ADP4s = has_only_ADP4s_and_errors_(self%fragment_atom(f))
      do_ADP3s = has_only_ADP3s_and_errors_(self%fragment_atom(f))

      ! ADP4's !!!!!!!!!!!!!
      if (do_ADP4s) then

      !!!!!!!!!!!!!!!!!!!!!!

         ! Get ADP tensors
         call put_ADP3_vector_to_(self%fragment_atom(f),adp3)
         call put_ADP4_vector_to_(self%fragment_atom(f),adp4)
         do k = 1,n_k

            ! Scattering vector
            k1 = k_pt(k,1)
            k2 = k_pt(k,2)
            k3 = k_pt(k,3)
            kv = [k1,k2,k3]

            ! Component products
            k1_k1 = k1*k1
            k2_k2 = k2*k2
            k3_k3 = k3*k3
            k1_k2 = k1*k2
            k1_k3 = k1*k3
            k2_k3 = k2*k3

            ! U tensor contracted with k vector
            c2 = contract_with_(adp2,kv)*HALF
            c3 = contract_with_(adp3,kv)*SIXTH
            c4 = contract_with_(adp4,kv)*SIXTH*QUARTER

            ! Static structure factor x TF
            ! i.e. dynamic structure factor,
            ! in the harmonic approximation
            sfh = ff(k)*exp(-c2)

            ! Multiples of *anharmonic* structure factor
            sf1 = sfh*(ONE-II*c3+c4)
            sf2 = sf1*HALF

            ! SF contribution (anharmonic!)
            sf_e(k) = sf_e(k)+sf1
            sf_d(k,base+1) = II*k1*sf1
            sf_d(k,base+2) = II*k2*sf1
            sf_d(k,base+3) = II*k3*sf1

            ! U2 derivatives
            sf_d(k,base+4) = -k1_k1*sf2
            sf_d(k,base+5) = -k2_k2*sf2
            sf_d(k,base+6) = -k3_k3*sf2
            sf_d(k,base+7) = -k1_k2*sf1
            sf_d(k,base+8) = -k1_k3*sf1
            sf_d(k,base+9) = -k2_k3*sf1

            ! Structure factor (harmonic)
            sf1 = sfh

            ! Multiples of *harmonic* sf
            sf2 = sf1*HALF
            sf4 = sf1*QUARTER
            sf6 = sf1*SIXTH
            sf24 = sf1*SIXTH*QUARTER

            ! U3 derivatives
            sf_d(k,base+10) = -II*k1_k1*k1*sf6
            sf_d(k,base+11) = -II*k2_k2*k2*sf6
            sf_d(k,base+12) = -II*k3_k3*k3*sf6
            sf_d(k,base+13) = -II*k1_k1*k2*sf2
            sf_d(k,base+14) = -II*k1_k1*k3*sf2
            sf_d(k,base+15) = -II*k2_k2*k1*sf2
            sf_d(k,base+16) = -II*k2_k2*k3*sf2
            sf_d(k,base+17) = -II*k3_k3*k1*sf2
            sf_d(k,base+18) = -II*k3_k3*k2*sf2
            sf_d(k,base+19) = -II*k1*k2*k3*sf1

            ! U4 derivatives
            sf_d(k,base+20) = k1_k1*k1_k1*sf24
            sf_d(k,base+21) = k2_k2*k2_k2*sf24
            sf_d(k,base+22) = k3_k3*k3_k3*sf24
            sf_d(k,base+23) = k1_k1*k1_k2*sf6
            sf_d(k,base+24) = k1_k1*k1_k3*sf6
            sf_d(k,base+25) = k2_k2*k1_k2*sf6
            sf_d(k,base+26) = k2_k2*k2_k3*sf6
            sf_d(k,base+27) = k3_k3*k1_k3*sf6
            sf_d(k,base+28) = k3_k3*k2_k3*sf6
            sf_d(k,base+29) = k1_k1*k2_k2*sf4
            sf_d(k,base+30) = k1_k1*k3_k3*sf4
            sf_d(k,base+31) = k2_k2*k3_k3*sf4
            sf_d(k,base+32) = k1_k1*k2_k3*sf2
            sf_d(k,base+33) = k2_k2*k1_k3*sf2
            sf_d(k,base+34) = k3_k3*k1_k2*sf2
         end do

      ! ADP3's !!!!!!!!!!!!!!!!!!
      else if (do_ADP3s) then

      !!!!!!!!!!!!!!!!!!!!!!!!!!!

         ! Get ADP tensors
         call put_ADP3_vector_to_(self%fragment_atom(f),adp3)
         do k = 1,n_k

            ! Scattering vector
            k1 = k_pt(k,1)
            k2 = k_pt(k,2)
            k3 = k_pt(k,3)
            kv = [k1,k2,k3]

            ! Component products
            k1_k1 = k1*k1
            k2_k2 = k2*k2
            k3_k3 = k3*k3
            k1_k2 = k1*k2
            k1_k3 = k1*k3
            k2_k3 = k2*k3

            ! U tensor contracted with k vector
            c2 = contract_with_(adp2,kv)*HALF
            c3 = contract_with_(adp3,kv)*SIXTH

            ! Static structure factor x TF
            ! i.e. dynamic structure factor,
            ! in the harmonic approximation
            sfh = ff(k)*exp(-c2)

            ! Multiples of *anharmonic* structure factor
            ! -- 3rd order only.
            sf1 = sfh*(ONE-II*c3)
            sf2 = sf1*HALF

            ! SF contribution (anharmonic!)
            sf_e(k) = sf_e(k)+sf1

            ! Pos derivatives
            sf_d(k,base+1) = II*k1*sf1
            sf_d(k,base+2) = II*k2*sf1
            sf_d(k,base+3) = II*k3*sf1

            ! U2 derivatives
            sf_d(k,base+4) = -k1_k1*sf2
            sf_d(k,base+5) = -k2_k2*sf2
            sf_d(k,base+6) = -k3_k3*sf2
            sf_d(k,base+7) = -k1_k2*sf1
            sf_d(k,base+8) = -k1_k3*sf1
            sf_d(k,base+9) = -k2_k3*sf1

            ! Structure factor (harmonic!)
            sf1 = sfh

            ! Multiples of *harmonic* sf
            sf2 = sf1*HALF
            sf6 = sf1*SIXTH

            ! U3 derivatives
            sf_d(k,base+10) = -II*k1_k1*k1*sf6
            sf_d(k,base+11) = -II*k2_k2*k2*sf6
            sf_d(k,base+12) = -II*k3_k3*k3*sf6
            sf_d(k,base+13) = -II*k1_k1*k2*sf2
            sf_d(k,base+14) = -II*k1_k1*k3*sf2
            sf_d(k,base+15) = -II*k2_k2*k1*sf2
            sf_d(k,base+16) = -II*k2_k2*k3*sf2
            sf_d(k,base+17) = -II*k3_k3*k1*sf2
            sf_d(k,base+18) = -II*k3_k3*k2*sf2
            sf_d(k,base+19) = -II*k1*k2*k3*sf1
         end do

      ! ADP2's !!!!!!!!!!!!!!
      else  ! NO anharmonicity

      !!!!!!!!!!!!!!!!!!!!!!!
         do k = 1,n_k

            ! Scattering vector
            k1 = k_pt(k,1)
            k2 = k_pt(k,2)
            k3 = k_pt(k,3)
            kv = [k1,k2,k3]

            ! Component products
            k1_k1 = k1*k1
            k2_k2 = k2*k2
            k3_k3 = k3*k3
            k1_k2 = k1*k2
            k1_k3 = k1*k3
            k2_k3 = k2*k3

            ! U tensor contracted with k vector
            c2 = contract_with_(adp2,kv)*HALF

            ! Static structure factor x TF
            ! i.e. dynamic structure factor,
            ! in the harmonic approximation
            sfh = ff(k)*exp(-c2)

            ! Multiples *harmonic* structure factor
            sf1 = sfh
            sf2 = sf1*HALF

            ! SF contribution (harmonic)
            sf_e(k) = sf_e(k)+sf1

            ! Pos derivatives
            sf_d(k,base+1) = II*k1*sf1
            sf_d(k,base+2) = II*k2*sf1
            sf_d(k,base+3) = II*k3*sf1

            ! U2 derivatives
            sf_d(k,base+4) = -k1_k1*sf2
            sf_d(k,base+5) = -k2_k2*sf2
            sf_d(k,base+6) = -k3_k3*sf2
            sf_d(k,base+7) = -k1_k2*sf1
            sf_d(k,base+8) = -k1_k3*sf1
            sf_d(k,base+9) = -k2_k3*sf1
         end do
      end if

      ! Clean
      call destroy_(k_pt)
   end subroutine


!   d_F_abs_dX(res,dFdX)
!   ! Evaluate "res", the derivative of the F_abs = |F_calc| with
!   ! respect to some parameters X (e.g. thermal parameters) given
!   ! "dFdX", the derivatives of the complex structure factors  w.r.t.
!   ! the X parameters.
!      self :: INOUT
!      res :: MAT{REAL}, OUT
!      dFdX :: MAT{CPX}, IN
!
!   ENSURE(.xray_data_exists,"no diffraction data")
!
!      .xray_data.d_F_abs_dX(res,dFdX)
!
!   end

!   d_F_abs_2_dX(res,dFdX)
!   ! Evaluate "res", the derivative of the F_abs with respect to some
!   ! parameters X (e.g. thermal parameters) given "dFdX", the
!   ! derivatives of the complex structure factors  w.r.t. the X
!   ! parameters.
!      res :: MAT{REAL}, OUT
!      dFdX :: MAT{CPX}, IN
!
!   ENSURE(.xray_data_exists,"no diffraction data")
!
!      .xray_data.d_F_abs_2_dX(res,dFdX)
!
!   end

!  Normal equations

!   solve_normal_equations(dFdX) :: leaky
!   ! Get the ".dX" in the parameters by solving the normal equations,
!   ! given "dFdX", the derivatives of the *absolute* *uscaled*
!   ! structure factors |F_calc| w.r.t. those parameters.
!      self :: INOUT
!      dFdX :: MAT{REAL}, INOUT
!
!   ENSURE(.xray_data_exists,"no diffraction data")
!
!      .xray_data.solve_normal_equations(dFdX)
!
!   end

!  =================
!  Refinement output
!  =================

   subroutine put_fit_header(self)
   ! Put out the fit header
      type(CRYSTAL_TYPE), IN :: self
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_fit_header ... no diffraction data")
      call put_fit_header_info_(self)
   end subroutine

   subroutine put_fit_header_info(self)
   ! Put out the fit header
      type(CRYSTAL_TYPE), IN :: self
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_fit_header_info ... no diffraction data")

      ! Print header
      call put_fit_header_(self%xray_data)

      ! Put the fit control switches
      call put_refinement_switches_(self%xray_data)

      ! Put the initial parameters
      call flush_(stdout,2)
      call text_(stdout,"Initial parameter values:")
      call flush_(stdout)
      call put_parameters_(self)

      ! Put the fit table header
      call put_fit_table_headings_(self%xray_data)
   end subroutine

   subroutine put_refinement_header_info(self)
   ! Put out the fit header
      type(CRYSTAL_TYPE), IN :: self
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_refinement_header_info ... no diffraction data")

      ! Print header
      call put_refinement_header_

      ! Put the refinement control switches
      call put_refinement_switches_(self%xray_data)

      ! Put the initial parameters
      call flush_(stdout)
      call text_(stdout,"Initial asymmetric unit parameter values:")
      call flush_(stdout)
      call put_parameters_(self)

      ! Put the refinement header info
      call put_refinement_table_headings_(self%xray_data)
   end subroutine

   subroutine put_parameters(self,covariance)
   ! Write refinement results
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), optional, IN :: covariance
      INT :: n_p
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_parameters ... no diffraction data")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:put_parameters ... no .asymmetric_unit_atom's")

      ! Put positions and ADPs
      call put_coordinates_(self%asymmetric_unit_atom,no_header=TRUE)
      call put_ADPs_(self%asymmetric_unit_atom,no_header=TRUE,covariance=covariance)

      ! Put zero eigenvalue information
      if (self%xray_data%show_near_0_eigenvectors) then
         if (self%xray_data%fit_finished) then
            n_p = no_of_pADPs_(self%asymmetric_unit_atom)
            call put_near_0_evecs_(self%xray_data,n_p)
         end if
      end if
   end subroutine

   subroutine put_correlations(self,name)
   ! Put the correlation matrix
      type(CRYSTAL_TYPE), IN :: self
      STR(len=*), optional, IN :: name
      VEC(REAL,:), allocatable :: val
      VEC(INT,:), allocatable :: row,col,ind
      VEC(STR(len=STR_SIZE),:), allocatable :: labr,labc,dsh
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      REAL :: min_corr
      INT :: n_corr
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_correlations ... no diffraction data")
   ENSURE(allocated(self%xray_data%correlation_mx),"CRYSTAL:put_correlations ... no data.correlation_mx")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:put_correlations ... no .asymmetric_unit_atom's")

      ! How many large correlations?
      min_corr = self%xray_data%min_correlation
      n_corr = number_in_range_(self%xray_data%correlation_mx,[min_corr,0.9999d0],TRUE,TRUE)

      ! If no large correlations, return
      if (n_corr<=0) return

      ! Change to a new file
      if (present(name)) call redirect_(stdout,name)
      call flush_(stdout)
      call text_(stdout,"============================")
      call text_(stdout,"Large parameter correlations")
      call text_(stdout,"============================")
      call flush_(stdout)
      call show_(stdout,"Smallest absolute correlation to show    =",min_corr)
      call show_(stdout,"No. pairs with at least this correlation =",n_corr)
      call create_(val,n_corr)
      call create_(ind,n_corr)
      call create_(row,n_corr)
      call create_(col,n_corr)
      call create_(dsh,n_corr)
      dsh = "--"

      ! Get row and column indices
      call get_indices_in_range_(self%xray_data%correlation_mx,[min_corr,0.9999d0],row,col,val,TRUE,TRUE)

      ! Construct the row labels
      labr = tag_pADP_labels_(self%asymmetric_unit_atom,row)
      labc = tag_pADP_labels_(self%asymmetric_unit_atom,col)

      ! Sort
      call quick_sort_(val,ind,decreasing_order=TRUE)
      val = val(ind)
      labr = labr(ind)
      labc = labc(ind)

      ! Set the table
      call create_(table,4)
      call set_heading_(table(1),"Parameter")
      call set_subhead_(table(1),"1")
      call set_width_from_(table(1),labr)
      call set_values_(table(1),labr)
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"--")
      call set_width_(table(2),2)
      call set_values_(table(2),dsh)
      call set_heading_(table(3),"Parameter")
      call set_subhead_(table(3),"2")
      call set_width_from_(table(3),labc)
      call set_left_justify_(table(3),TRUE)
      call set_values_(table(3),labc)
      call set_heading_(table(4),"Correlation")
      call set_subhead_(table(4),"Coefficeint")
      call set_values_(table(4),val)

      ! Put body
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(labc); call destroy_(labr)
      call destroy_(dsh); call destroy_(col); call destroy_(row)
      call destroy_(ind); call destroy_(val)

      ! Change back
      if (present(name)) call revert_(stdout)
   end subroutine

   subroutine put_correlations_in_cif(self,name)
   ! Put the correlation matrix
      type(CRYSTAL_TYPE), IN :: self
      STR(len=*), optional, IN :: name
      VEC(REAL,:), allocatable :: val
      VEC(INT,:), allocatable :: row,col,ind
      VEC(STR(len=STR_SIZE),:), allocatable :: labr,labc,dsh
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      REAL :: min_corr
      INT :: n_corr
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_correlations_in_cif ... no diffraction data")
   ENSURE(allocated(self%xray_data%correlation_mx),"CRYSTAL:put_correlations_in_cif ... no data.correlation_mx")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:put_correlations_in_cif ... no .asymmetric_unit_atom's")

      ! How many large correlations?
      min_corr = self%xray_data%min_correlation
      n_corr = number_in_range_(self%xray_data%correlation_mx,[min_corr,0.9999d0],TRUE,TRUE)

      ! If no large correlations, return
      if (n_corr<=0) return

      ! Change to a new file
      if (present(name)) call redirect_(stdout,name)
      call flush_(stdout)
      call text_(stdout,"# ============================")
      call text_(stdout,"# Large parameter correlations")
      call text_(stdout,"# ============================")
      call flush_(stdout)

      ! Loop 
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout," _geom_correlation_atom_site_label_1")
      call text_(stdout," _geom_correlation_param_site_label_1")
      call text_(stdout," _geom_correlation_atom_site_label_2")
      call text_(stdout," _geom_correlation_param_site_label_2")
      call text_(stdout," _geom_correlation_coef")
      call create_(val,n_corr)
      call create_(ind,n_corr)
      call create_(row,n_corr)
      call create_(col,n_corr)
      call create_(dsh,n_corr)
      dsh = " "

      ! Get row and column indices
      call get_indices_in_range_(self%xray_data%correlation_mx,[min_corr,0.9999d0],row,col,val,TRUE,TRUE)

      ! Construct the row labels
      labr = tag_pADP_labels_(self%asymmetric_unit_atom,row)
      labc = tag_pADP_labels_(self%asymmetric_unit_atom,col)

      ! Sort
      call quick_sort_(val,ind,decreasing_order=TRUE)
      val = val(ind)
      labr = labr(ind)
      labc = labc(ind)

      ! Set the table
      call create_(table,4)
      call set_left_justify_(table(1),TRUE)
      call set_width_from_(table(1),labr)
      call set_values_(table(1),labr)
      call set_column_spacing_(table(2),0)
      call set_width_(table(2),1)
      call set_values_(table(2),dsh)
      call set_left_justify_(table(3),TRUE)
      call set_width_from_(table(3),labc)
      call set_values_(table(3),labc)
      call set_left_justify_(table(4),FALSE)
      call set_values_(table(4),val)

      ! Put body
      call put_body_(table)

      ! Clean
      call destroy_(table)
      call destroy_(labc); call destroy_(labr)
      call destroy_(dsh); call destroy_(col); call destroy_(row)
      call destroy_(ind); call destroy_(val)

      ! Change back
      if (present(name)) call revert_(stdout)
   end subroutine

   subroutine put_fit_results(self)
   ! Finalise fit, write results
      type(CRYSTAL_TYPE), IN :: self
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_fit_results ... no diffraction data")

      ! Iteration footer
      call put_fit_results_(self%xray_data)

      ! Put the parameter values
      call flush_(stdout,2)
      call text_(stdout,"Final asymmetric unit parameter values:")
      call flush_(stdout)
      if (allocated(self%xray_data%covariance_mx)) then; call put_parameters_(self,self%xray_data%covariance_mx)
      else; call put_parameters_(self)
      end if
      if (self%xray_data%ADP_model=="tls") call put_TLS_results_(self)

      ! Form factor asymmetry, as put_refinement_results does. A single
      ! rigid-atom fit (ha_fit, which is how pHAR runs) ends here rather
      ! than there, and without this the symmetrization it performed is
      ! never reported.
      if (allocated(self%asymmetric_FF_symmetrization_rss)) then
         call put_asymmetric_FF_symmetrization_rss_(self)
      end if
   end subroutine

   subroutine put_refinement_results(self)
   ! Finalise refinement, write results
      type(CRYSTAL_TYPE), IN :: self
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_refinement_results ... no diffraction data")

      ! Iteration footer
      call put_refinement_results_(self%xray_data)

      ! Put the final parameters
      call flush_(stdout,2)
      call text_(stdout,"Final asymmetric unit parameter values:")
      call flush_(stdout)
      if (allocated(self%xray_data%covariance_mx)) then
         call put_parameters_(self,self%xray_data%covariance_mx)
      else
         call put_parameters_(self)
      end if
      if (self%xray_data%ADP_model=="tls") call put_TLS_results_(self)
      if (allocated(self%asymmetric_FF_symmetrization_rss)) then
         call put_asymmetric_FF_symmetrization_rss_(self)
      end if

      ! Worst reflections
      call put_worst_reflections_(self%xray_data)
   end subroutine

   subroutine put_bond_info_with_errors(self)
   ! Put out the bond-length, bond-angle and torsion-angle info with
   ! errors in brackets
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: cx,cm
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_bond_info_with_errors ... no data")
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:put_bond_info_with_errors ... no fragment_atoms")
   ENSURE(allocated(self%xray_data%fragment_covariance_mx),"CRYSTAL:put_bond_info_with_errors ... no fragment_covariance_mx")

      ! Get the pos (cx) and pADP (cm) covariance matrices
      call create_(cx,3*self%n_fragment_atoms,3*self%n_fragment_atoms)
      call make_pos_covariance_mx_(cx,self%fragment_atom,self%xray_data%fragment_covariance_mx)
      call create_(cm,9*self%n_fragment_atoms,9*self%n_fragment_atoms)
      call make_pADP2_covariance_mx_(cm,self%fragment_atom,self%xray_data%fragment_covariance_mx)

      ! ADP3s in principal axis frame
      if (has_any_ADP3s_and_errors_(self%fragment_atom)) then
         call put_ADP3s_in_ADP2_principal_axes_(self%fragment_atom,self%xray_data%fragment_covariance_mx)
      end if

      ! ADP4s in principal axis frame
      if (has_any_ADP4s_and_errors_(self%fragment_atom)) then
         call put_ADP4s_in_ADP2_principal_axes_(self%fragment_atom,self%xray_data%fragment_covariance_mx)
      end if

      ! Put fragment_atom's coordinate on the top. !  // Kang added
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"=======================================")
      call text_(stdout,"Fragment atom coordinates for bond info")
      call text_(stdout,"=======================================")
      call put_coordinates_(self%fragment_atom,no_header=TRUE)
      ! Bond length tables with Hirshfeld test
      call put_bonds_and_Hirshfeld_test_(self%fragment_atom,cx,cm)

      ! Bond angle tables
   ! .fragment_atom.put_bond_length_table(cx)
      call put_bond_angle_table_(self%fragment_atom,cx)
      call put_torsion_angle_table_(self%fragment_atom,cx)
      if (self%xray_data%show_correlation_mx) then
         call flush_(stdout)
         call text_(stdout,"Covariance matrix:")
         call put_(stdout,self%xray_data%fragment_covariance_mx)
      end if

      ! Clean
      call destroy_(cm)
      call destroy_(cx)
   end subroutine

   subroutine put_CIF_bond_info_tables(atom,covariance)
   ! Put out the CIF bond lengths for an "atom" list with an
   ! allocated "covariance_mx"
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), IN :: covariance
      MAT(REAL,:,:), allocatable :: cx

      ! Get the pos covariance matrices
      call create_(cx,3*size(atom),3*size(atom))
      call make_pos_covariance_mx_(cx,atom,covariance)

      ! Print out the *CIF* tables with errors
      call put_CIF_bond_length_table_(atom,cx)
      call put_CIF_bond_angle_table_(atom,cx)
      call put_CIF_bond_torsion_angle_table_(atom,cx,parentheses=TRUE)
      call destroy_(cx)
   end subroutine

   subroutine put_CIF_bond_Hirshfeld_tables(atom,covariance_mx)
   ! Put out the CIF bond lengths for an "atom" list with an
   ! allocated "covariance_mx"
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), IN :: covariance_mx
      MAT(REAL,:,:), allocatable :: cx,cm

      ! Get the pos (cx) and pADP (cm) covariance matrices
      call create_(cx,3*size(atom),3*size(atom))
      call make_pos_covariance_mx_(cx,atom,covariance_mx)
      call create_(cm,9*size(atom),9*size(atom))
      call make_pADP2_covariance_mx_(cm,atom,covariance_mx)

      ! Print out the *CIF* tables with errors
      call put_CIF_bond_Hirshfeld_table_(atom,cx,cm)
      call put_CIF_bond_angle_table_HBB_(atom,cx)
      call put_CIF_bond_torsion_angle_table_(atom,cx,parentheses=FALSE)
      call destroy_(cm)
      call destroy_(cx)
   end subroutine

   PURE subroutine make_pos_covariance_mx_0(self,cm)
   ! Make the positional covarinace matrix "cm".
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: cm
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_pos_covariance_mx_0 ... no data")
   ENSURE(allocated(self%xray_data%fragment_covariance_mx),"CRYSTAL:make_pos_covariance_mx_0 ... no fragment_covariance_mx")
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:make_pos_covariance_mx_0 ... no fragment_atoms")
      call make_pos_covariance_mx_(cm,self%fragment_atom,self%xray_data%fragment_covariance_mx)
   end subroutine

   PURE subroutine make_pos_covariance_mx_1(cm,atom,covariance)
   ! Make the positional covarinace matrix "cm".
      MAT(REAL,:,:), OUT :: cm
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), IN :: covariance
      VEC(INT,:), allocatable :: n_pADPs
      INT :: dim
   ENSURE(is_square_(cm),"CRYSTAL:make_pos_covariance_mx_1 ... cm is not square")
   ENSURE(size(cm,1)==3*size(atom),"CRYSTAL:make_pos_covariance_mx_1 ... cm is not square")

      ! Get block dimensions
      dim = size(atom)
      call create_(n_pADPs,dim)
      call put_no_of_pADPs_per_atom_to_(atom,n_pADPs)

      ! Get it
      call put_sub_blocks_to_(covariance,cm,dim=3,block_dims=n_pADPs)

      ! Clean
      call destroy_(n_pADPs)
   end subroutine

   PURE subroutine make_pADP2_covariance_mx_0(self,cm)
   ! Make the pADP2 covarinace matrix "cm".
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: cm
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_pADP2_covariance_mx_0 ... no data")
   ENSURE(allocated(self%xray_data%fragment_covariance_mx),"CRYSTAL:make_pADP2_covariance_mx_0 ... no fragment_covariance_mx")
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:make_pADP2_covariance_mx_0 ... no fragment_atoms")
      call make_pADP2_covariance_mx_(cm,self%fragment_atom,self%xray_data%fragment_covariance_mx)
   end subroutine

   PURE subroutine make_pADP2_covariance_mx_1(cm,atom,covariance)
   ! Make the pADP2 covarinace matrix "cm".
      MAT(REAL,:,:), OUT :: cm
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), IN :: covariance
      VEC(INT,:), allocatable :: n_pADPs
      INT :: dim
   ENSURE(is_square_(cm),"CRYSTAL:make_pADP2_covariance_mx_1 ... cm is not square")
   ENSURE(size(cm,1)==9*size(atom),"CRYSTAL:make_pADP2_covariance_mx_1 ... cm is not square")

      ! Get block dimensions
      dim = size(atom)
      call create_(n_pADPs,dim)
      call put_no_of_pADPs_per_atom_to_(atom,n_pADPs)

      ! Get it
      call put_sub_blocks_to_(covariance,cm,dim=9,block_dims=n_pADPs)

      ! Clean
      call destroy_(n_pADPs)
   end subroutine

   subroutine put_bond_info_with_errors_to(self)
   ! Put out the bond-length, bond-angle and torsion-angle info with
   ! errors with *no* brackets to a file, name read from stdin
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: cm
      STR(len=STR_SIZE) :: name
      type(TEXTFILE_TYPE), allocatable :: save,file
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_bond_info_with_errors_to ... no data")
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:put_bond_info_with_errors_to ... no fragment_atoms")
   ENSURE(allocated(self%xray_data%fragment_covariance_mx),"CRYSTAL:put_bond_info_with_errors_to ... no fragment_covariance_mx")

      ! Read the name
      call read_(stdin,name)

      ! Create the new file
      call create_(file,trim(name))
      call open_for_(file,"write")

      ! Positional covariance matrix
      call create_(cm,3*self%n_fragment_atoms,3*self%n_fragment_atoms)
      call make_pos_covariance_mx_(self,cm)

      ! Print out the tables with errors
      save = stdout
      stdout = file
      call put_bond_length_table_(self%fragment_atom,cm,parentheses=FALSE)
      call put_bond_angle_table_(self%fragment_atom,cm,parentheses=FALSE)
      call put_torsion_angle_table_(self%fragment_atom,cm,parentheses=FALSE)
      stdout = save

      ! Clean
      call destroy_(cm)
      call close_(file)
      call destroy_(file)
   end subroutine


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump object data as text
      type(CRYSTAL_TYPE), IN :: self
      call dump_(stdout,"CIF_chemical_formula",self%CIF_chemical_formula)
      call dump_(self%spacegroup,"spacegroup")
      call dump_(self%unit_cell,"unit_cell")
      call dump_(stdout,"n_fragment_atoms",self%n_fragment_atoms)
      call dmpa_(self%fragment_atom,"fragment_atom")
      call dump_(stdout,"destroy_fragment_atom",self%destroy_fragment_atom)
      call dmpa_(stdout,"fragment_geometry",self%fragment_geometry)
      call dump_(stdout,"fragment_offset",self%fragment_offset)
      call dump_(stdout,"asymmetric_unit_source",self%asymmetric_unit_source)
      call dump_(stdout,"n_asymmetric_unit_atoms",self%n_asymmetric_unit_atoms)
      call dmpa_(self%asymmetric_unit_atom,"asymmetric_unit_atom")
      call dmpa_(stdout,"asymmetric_unit_geometry",self%asymmetric_unit_geometry)
      call dump_(stdout,"prune_asymmetric_unit",self%prune_asymmetric_unit)
      call dump_(stdout,"n_unit_cell_atoms",self%n_unit_cell_atoms)
      call dmpa_(stdout,"unit_cell_geometry",self%unit_cell_geometry)
      call dmpa_(stdout,"unique_unit_cell_atom",self%unique_unit_cell_atom)
   ! stdout.dmpa("unique_uc_atom_offset",.unique_uc_atom_offset)
      call dmpa_(stdout,"is_asym_unit_cell_atom",self%is_asym_unit_cell_atom)
      call dump_(stdout,"fragment_info_made",self%fragment_info_made)
      call dmpa_(stdout,"asym_atom_for_unit_cell_atom",self%asym_atom_for_unit_cell_atom)
      call dmpa_(stdout,"asym_symop_for_unit_cell_atom",self%asym_symop_for_unit_cell_atom)
      call dmpa_(stdout,"frag_atom_for_unit_cell_atom",self%frag_atom_for_unit_cell_atom)
      call dmpa_(stdout,"unit_cell_atom_for_frag_atom",self%unit_cell_atom_for_frag_atom)
      call dmpa_(stdout,"unit_cell_shft_for_frag_atom",self%unit_cell_shft_for_frag_atom)
      call dmpa_(stdout,"frag_atom_for_asym_atom",self%frag_atom_for_asym_atom)
      call dmpa_(stdout,"asym_atom_for_frag_atom",self%asym_atom_for_frag_atom)
      call dmpa_(stdout,"frag_parent_for_cell_atom",self%frag_parent_for_cell_atom)
      call dump_(stdout,"n_unique_frag_atoms",self%n_unique_frag_atoms)
      call dmpa_(stdout,"unique_frag_atom",self%unique_frag_atom)
      call dmpa_(stdout,"unique_atom_for_frag_atom",self%unique_atom_for_frag_atom)
      call dmpa_(stdout,"unique_symop_for_frag_atom",self%unique_symop_for_frag_atom)
      call dmpa_(stdout,"asym_symop_for_frag_atom",self%asym_symop_for_frag_atom)
      call dmpa_(stdout,"asym_shift_for_frag_atom",self%asym_shift_for_frag_atom)
      call dump_(stdout,"Z",self%Z)
      call dump_(stdout,"use_manual_repetition_factors",self%use_manual_repetition_factors)
      call dmpa_(stdout,"repetition_factor",self%repetition_factor)
      call dmpa_(stdout,"asym_atom_sym_factor",self%asym_atom_sym_factor)
   ! stdout.dmpa("asym_atom_stabilizer",.asym_atom_stabilizer)
   ! stdout.dmpa("unit_cell_connection",.unit_cell_connection)
   ! stdout.dmpa("uc_vdw_connection",.uc_vdw_connection)
      call dmpa_(stdout,"unit_cell_mol_for_atom",self%unit_cell_mol_for_atom)
      call dump_(stdout,"n_unit_cell_mols",self%n_unit_cell_mols)
      call dump_(stdout,"n_stabilizer_symops",self%n_stabilizer_symops)
   ! stdout.dmpa("stabilizer_symop",.stabilizer_symop)
      call dmpa_(stdout,"str",self%str)
      call dmpa_(stdout,"ptr",self%ptr)
      call dmpa_(stdout,"dtr",self%dtr)
      call dmpa_(stdout,"ftr",self%ftr)
      call dmpa_(stdout,"gtr",self%gtr)
   ! stdout.dump("data_kind",.data_kind)
   ! stdout.dmpa("data",.xray_data)
   ! stdout.dmpa("xray_data",.xray_data)
   ! stdout.dmpa("pnd_data",.pnd_data)
      call dump_(stdout,"r_free_percentage",self%r_free_percentage)
   ! stdout.dmpa("xray_r_free_data",.xray_r_free_data)
   ! stdout.dmpa("pnd_r_free_data",.pnd_r_free_data)
      call dump_(stdout,"finalize",self%finalize)
      call dump_(stdout,"radius",self%radius)
      call dump_(stdout,"atom_density_cutoff",self%atom_density_cutoff)
      call dump_(stdout,"generation_method",self%generation_method)
      call dump_(stdout,"defragment",self%defragment)
      call dump_(stdout,"n_atoms",self%n_atoms)
      call dmpa_(stdout,"geometry",self%geometry)
      call dump_(stdout,"unit_cell_offset",self%unit_cell_offset)
      call dump_(stdout,"h_min",self%h_min)
      call dump_(stdout,"h_max",self%h_max)
      call dump_(stdout,"n_shift",self%n_shift)
      call dmpa_(stdout,"shift",self%shift)
      call dmpa_(stdout,"shift_for_atom",self%shift_for_atom)
      call dmpa_(stdout,"is_fragment_atom",self%is_fragment_atom)
      call dmpa_(stdout,"occupation_list",self%occupation_list)
      call dump_(stdout,"n_molecules",self%n_molecules)
   ! stdout.dmpa("molecule_for_atom",.molecule_for_atom)
   ! stdout.dmpa("molecule",.molecule)
      call dmpa_(stdout,"molecule_centroid",self%molecule_centroid)
      call dmpa_(stdout,"unique_molecule_for",self%unique_molecule_for)
      call dmpa_(stdout,"unique_sycode_for",self%unique_symcode_for)
   ! stdout.dmpa("atom_connection",.atom_connection)
      call dmpa_(stdout,"unit_cell_atoms_to_suppress",self%unit_cell_atoms_to_suppress)
      call dump_(stdout,"cluster_info_made",self%cluster_info_made)
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(CRYSTAL_TYPE), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: all
      BIN :: is_all

      ! The following code is inherited from OBJECT
      is_all = FALSE
      if (present(all)) is_all = all
      if (is_all) then; call text_(stdout,trim(object_name)//":CRYSTAL@ = { ")
      else; call text_(stdout,trim(object_name)//":CRYSTAL = { ")
      end if
      call increment_margin_width_(stdout,1)
      call do_dump_(self)
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump allocatable object data as text
      type(CRYSTAL_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":CRYSTAL@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


!  ===========
!  Put methods
!  ===========

   subroutine put(self)
   ! Put out the crystal data to file "out"
      type(CRYSTAL_TYPE), INOUT :: self
      call put_(self%unit_cell)
      call put_(self%spacegroup)
   ! if (NOT tonto.low_verbosity) .put_xyz_seitz_matrices
      if (allocated(self%asymmetric_unit_geometry)) call put_asymmetric_unit_geometry_(self)
      if (tonto%low_verbosity) then
         if (allocated(self%fragment_geometry)) then
            if (size(self%fragment_geometry)>100) return
         end if
      end if
      if (allocated(self%fragment_geometry)) call put_fragment_data_(self)
      if (allocated(self%unit_cell_geometry)) call put_unit_cell_geometry_(self)

   ! if (.unit_cell_connection.allocated)     .put_uc_connection_table
   ! if (.uc_vdw_connection.allocated)        .put_uc_vdw_connection_table

   ! .put_reflection_data
   end subroutine

   subroutine put_xyz_seitz_matrices(self)
   ! Put out the cartesian seitz matrices.
      type(CRYSTAL_TYPE), IN :: self
      MAT3(REAL,:,:,:), allocatable :: symop
      call save_style_(stdout)
      call set_using_array_labels_(stdout,FALSE)
      call set_real_precision_(stdout,3)
      symop = xyz_seitz_matrices_(self)
      call flush_(stdout)
      call text_(stdout,"========================")
      call text_(stdout,"Cartesian Seitz matrices")
      call text_(stdout,"========================")
      call put_(stdout,symop,auto_width=TRUE)

      ! Clean
      call destroy_(symop)
      call unsave_style_(stdout)
   end subroutine

   subroutine put_asymmetric_unit_geometry(self)
   ! Put out the asymmetric unit geometry.
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
   ENSURE(allocated(self%asymmetric_unit_geometry),"CRYSTAL:put_asymmetric_unit_geometry ... no asymmetric unit geometry")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:put_asymmetric_unit_geometry ... no asymmetric unit atoms")
   ENSURE(allocated(self%asym_atom_sym_factor),"CRYSTAL:put_asymmetric_unit_geometry ... no asym_atom_sym_factor")
   ENSURE(allocated(self%asym_atom_stabilizer),"CRYSTAL:put_asymmetric_unit_geometry ... no asym_atom_stabilizer")
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"===============")
      call text_(stdout,"Asymmetric unit")
      call text_(stdout,"===============")
      call flush_(stdout)
      call show_(stdout,"Asymmetric unit source       =",self%asymmetric_unit_source)
      call show_(stdout,"No. of asymmetric unit atoms =",self%n_asymmetric_unit_atoms)
      call flush_(stdout)

      ! Table
      call create_(table,6)
      call set_heading_(table(1),"ID"); call set_values_(table(1),self%asymmetric_unit_atom(:)%tag)
      call set_heading_(table(2),"x"); call set_values_(table(2),self%asymmetric_unit_geometry(1,:))
      call set_heading_(table(3),"y"); call set_values_(table(3),self%asymmetric_unit_geometry(2,:))
      call set_heading_(table(4),"z"); call set_values_(table(4),self%asymmetric_unit_geometry(3,:))
      call set_sb3head_(table(3),"/frac")
      call set_heading_(table(5),"Site"); call set_values_(table(5),int(self%asym_atom_sym_factor))
      call set_subhead_(table(5),"symm")
      call set_sb3head_(table(5),"fact")
      call set_item_width_(table(6),2)
      call set_heading_(table(6),"Stabilizer"); call set_values_(table(6),self%asym_atom_stabilizer)
      call set_subhead_(table(6),"symops")
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine put_asymmetric_FF_symmetrization_rss(self)
   ! Put the residual square sums of asymmetric unit atom's Form Factors from the form factor symmetrization
   ! By Kang. 10.Mar.2025
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: rms,rmx
      MAT(INT,:,:), allocatable :: hkl
      INT :: n_refl,a,n_atom
   ENSURE(allocated(self%asymmetric_FF_symmetrization_rss),"CRYSTAL:put_asymmetric_FF_symmetrization_rss ... No asymmetric atom Form Factor symmetrization residuals")
   ENSURE(allocated(self%asymmetric_FF_symmetrization_rmax),"CRYSTAL:put_asymmetric_FF_symmetrization_rss ... No asymmetric atom Form Factor maximum residuals")
   ENSURE(allocated(self%asymmetric_FF_symmetrization_rmax_hkl),"CRYSTAL:put_asymmetric_FF_symmetrization_rss ... No reflections for the maximum residuals")
   ENSURE(allocated(self%asym_atom_stabilizer),"CRYSTAL:put_asymmetric_FF_symmetrization_rss ... No stabilizer symmetry operators found")
      n_refl = size(self%xray_data%reflections)
      n_atom = size(self%asymmetric_FF_symmetrization_rss)

      ! Below the tolerance the asymmetry is rounding noise, whose size and
      ! argmax hkl differ between compilers: print an exact zero and no hkl.
      ! Real asymmetry (pHAR, 1e-9 to 5e-8) is three orders above it. The
      ! measurements are in TASKS_AND_HISTORY.md (the CI red of 2026-08-18).
      call create_(rms,n_atom)
      call create_(rmx,n_atom)
      call create_(hkl,n_atom,3)
      rms = sqrt(self%asymmetric_FF_symmetrization_rss/n_refl)
      rmx = self%asymmetric_FF_symmetrization_rmax
      hkl = self%asymmetric_FF_symmetrization_rmax_hkl
      do a = 1,n_atom
         if (rmx(a)<FF_ASYMMETRY_TOL) then
            rms(a) = ZERO
            rmx(a) = ZERO
            hkl(a,:) = 0
         end if
      end do
      call flush_(stdout)
      call text_(stdout,"=====================")
      call text_(stdout,"Form factor asymmetry")
      call text_(stdout,"=====================")
      call flush_(stdout)
      call text_(stdout,". Only a finite part of the crystal ED is used to make")
      call text_(stdout,"  the aspherical atomic form factors. So it may occur")
      call text_(stdout,"  that the FFs are not symmetric under the space group.")
      call text_(stdout,"  Therefore symmetrization is performed.")
      call flush_(stdout)
      call text_(stdout,". The root-mean squared asymmetry in the form factors")
      call text_(stdout,"  per reflection) is reported, for each asymm. unit atom.")
      call flush_(stdout)
      call text_(stdout,"   (RMS asymmetry)^2 = (1/n_refl) Sum_k |F_k-sym(F_k)|^2")
      call text_(stdout,"    Max asymmetry    = Max_k |F_k-sym(F_k)|, at (h,k,l)")
      call flush_(stdout)
      call text_(stdout,"  It should be zero, to at least 5 d.p's.")
      call create_(table,8)
      call set_heading_(table(1),"ID");
      call set_values_(table(1),self%asymmetric_unit_atom(:)%tag)

      ! One significant digit: the order of magnitude is the diagnostic, and
      ! more digits would only report bits that differ between compilers.
      call set_heading_(table(2),"RMS");
      call set_subhead_(table(2),"asymmetry")
      call set_width_(table(2),9)
      call set_real_precision_(table(2),1)
      call set_real_fortran_fmt_(table(2),"e")  ! First e format in column !!!
      call set_values_(table(2),rms)
      call set_heading_(table(3),"Max")
      call set_subhead_(table(3),"asymmetry")
      call set_width_(table(3),9)
      call set_real_precision_(table(3),1)
      call set_real_fortran_fmt_(table(3),"e")
      call set_values_(table(3),rmx)
      call set_heading_(table(4),"-")
      call set_subhead_(table(4),"h")
      call set_values_(table(4),hkl(:,1))
      call set_heading_(table(5),"-")
      call set_subhead_(table(5),"k")
      call set_values_(table(5),hkl(:,2))
      call set_heading_(table(6),"-")
      call set_subhead_(table(6),"l")
      call set_values_(table(6),hkl(:,3))
      call set_heading_(table(7),"Site symm.")
      call set_subhead_(table(7),"factor")
      call set_values_(table(7),int(self%asym_atom_sym_factor))
      call set_heading_(table(8),"Stabilizer")
      call set_subhead_(table(8),"symops")
      call set_values_(table(8),self%asym_atom_stabilizer)
      call put_(table)
      call destroy_(table)
      call destroy_(hkl)
      call destroy_(rmx)
      call destroy_(rms)
   end subroutine

   subroutine put_unit_cell_geometry(self)
   ! Put out the unit_cell_geometry.
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:put_unit_cell_geometry ... no unit_cell_geometry")
   ENSURE(allocated(self%asym_atom_for_unit_cell_atom),"CRYSTAL:put_unit_cell_geometry ... no asym-atom_for_cell_atom")
   ENSURE(allocated(self%asym_symop_for_unit_cell_atom),"CRYSTAL:put_unit_cell_geometry ... no asym_symop_for_cell_atom")
   ENSURE(allocated(self%frag_atom_for_unit_cell_atom),"CRYSTAL:put_unit_cell_geometry ... no frag_atom_for_cell_atom")
   ENSURE(allocated(self%frag_parent_for_cell_atom),"CRYSTAL:put_unit_cell_geometry ... no frag_parent_for_cell_atom")

      ! Header
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"==================")
      call text_(stdout,"Unit cell geometry")
      call text_(stdout,"==================")
      if (self%spacegroup%n_seitz==1) then
         call flush_(stdout)
         call text_(stdout,". The spacegroup is P1. The unit cell")
         call text_(stdout,"  is the same as the asymmetric unit.")
      end if
      if (any(self%frag_parent_for_cell_atom==0)) then
         call flush_(stdout)
         call text_(stdout,". The fragment does encompass the whole")
         call text_(stdout,"  unit cell - there are 0 indices.")
      end if
      call flush_(stdout)
      call show_(stdout,"No. of unit cell atoms =",self%n_unit_cell_atoms)
      call flush_(stdout)

      ! Table
      call create_(table,10)
      call set_heading_(table(1),"ID"); call set_values_(table(1),self%asymmetric_unit_atom(self%asym_atom_for_unit_cell_atom)%tag)
      call set_heading_(table(3),"Coord")
      call set_subhead_(table(2),"- x -"); call set_values_(table(2),self%unit_cell_geometry(1,:))
      call set_subhead_(table(3),"- y -"); call set_values_(table(3),self%unit_cell_geometry(2,:))
      call set_subhead_(table(4),"- z -"); call set_values_(table(4),self%unit_cell_geometry(3,:))
      call set_sb3head_(table(3),"/frac")
      call set_heading_(table(5),"From"); call set_values_(table(5),self%asym_atom_for_unit_cell_atom)
      call set_subhead_(table(5),"asym")
      call set_sb3head_(table(5),"atom")
      call set_heading_(table(6),"Using"); call set_values_(table(6),self%asym_symop_for_unit_cell_atom)
      call set_subhead_(table(6),"symop")
      call set_sb3head_(table(6),"    #")
      call set_heading_(table(7),"Asym"); call set_values_(table(7),self%is_asym_unit_cell_atom)
      call set_subhead_(table(7),"atom")
      call set_sb3head_(table(7),"   ?")
      call set_heading_(table(8),"Made by a"); call set_values_(table(8),self%frag_atom_for_unit_cell_atom)
      call set_subhead_(table(8),"shift frm")
      call set_sb3head_(table(8),"frag atom")
      call set_heading_(table(9),"Made by a"); call set_values_(table(9),self%frag_parent_for_cell_atom)
      call set_subhead_(table(9),"symop frm")
      call set_sb3head_(table(9),"frag atom")
      call set_heading_(table(10),"Unit"); call set_values_(table(10),self%unit_cell_mol_for_atom)
      call set_subhead_(table(10),"cell")
      call set_sb3head_(table(10),"Mol #")
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine put_unit_cell_geometry_cartesian(self)
   ! Put out the unit_cell_geometry, in cartesian coordinates and
   ! Angstrom, rather than the fractional coordinates used by
   ! .put_unit_cell_geometry.
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      MAT(REAL,3,3) :: dcm
      MAT(REAL,:,:), allocatable :: ucc
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:put_unit_cell_geometry_cartesian ... no unit_cell_geometry")
   ENSURE(allocated(self%asym_atom_for_unit_cell_atom),"CRYSTAL:put_unit_cell_geometry_cartesian ... no asym-atom_for_cell_atom")
   ENSURE(allocated(self%asym_symop_for_unit_cell_atom),"CRYSTAL:put_unit_cell_geometry_cartesian ... no asym_symop_for_cell_atom")
   ENSURE(allocated(self%frag_atom_for_unit_cell_atom),"CRYSTAL:put_unit_cell_geometry_cartesian ... no frag_atom_for_cell_atom")
   ENSURE(allocated(self%frag_parent_for_cell_atom),"CRYSTAL:put_unit_cell_geometry_cartesian ... no frag_parent_for_cell_atom")

      ! Convert the fractional unit cell geometry to cartesians
      dcm = self%unit_cell%direct_mx
      ucc = matmul(dcm,self%unit_cell_geometry)  ! This is (3 x N)

      ! Header
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"===========================================")
      call text_(stdout,"Unit cell geometry in cartesian coordinates")
      call text_(stdout,"===========================================")
      if (self%spacegroup%n_seitz==1) then
         call flush_(stdout)
         call text_(stdout,". The spacegroup is P1. The unit cell")
         call text_(stdout,"  is the same as the asymmetric unit.")
      end if
      if (any(self%frag_parent_for_cell_atom==0)) then
         call flush_(stdout)
         call text_(stdout,". The fragment does encompass the whole")
         call text_(stdout,"  unit cell - there are 0 indices.")
      end if
      call flush_(stdout)
      call show_(stdout,"No. of unit cell atoms =",self%n_unit_cell_atoms)
      call flush_(stdout)

      ! Table
      call create_(table,10)
      call set_heading_(table(1),"ID"); call set_values_(table(1),self%asymmetric_unit_atom(self%asym_atom_for_unit_cell_atom)%tag)
      call set_heading_(table(3),"Coord")
      call set_subhead_(table(2),"- x -"); call set_values_(table(2),ucc(1,:)*ANGSTROM_PER_BOHR)
      call set_subhead_(table(3),"- y -"); call set_values_(table(3),ucc(2,:)*ANGSTROM_PER_BOHR)
      call set_subhead_(table(4),"- z -"); call set_values_(table(4),ucc(3,:)*ANGSTROM_PER_BOHR)
      call set_sb3head_(table(3),"/Angstrom")
      call set_heading_(table(5),"From"); call set_values_(table(5),self%asym_atom_for_unit_cell_atom)
      call set_subhead_(table(5),"asym")
      call set_sb3head_(table(5),"atom")
      call set_heading_(table(6),"Using"); call set_values_(table(6),self%asym_symop_for_unit_cell_atom)
      call set_subhead_(table(6),"symop")
      call set_sb3head_(table(6),"    #")
      call set_heading_(table(7),"Asym"); call set_values_(table(7),self%is_asym_unit_cell_atom)
      call set_subhead_(table(7),"atom")
      call set_sb3head_(table(7),"   ?")
      call set_heading_(table(8),"Made by a"); call set_values_(table(8),self%frag_atom_for_unit_cell_atom)
      call set_subhead_(table(8),"shift frm")
      call set_sb3head_(table(8),"frag atom")
      call set_heading_(table(9),"Made by a"); call set_values_(table(9),self%frag_parent_for_cell_atom)
      call set_subhead_(table(9),"symop frm")
      call set_sb3head_(table(9),"frag atom")
      call set_heading_(table(10),"Unit"); call set_values_(table(10),self%unit_cell_mol_for_atom)
      call set_subhead_(table(10),"cell")
      call set_sb3head_(table(10),"Mol #")
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(ucc)
   end subroutine

   subroutine put_pdb(self,pdbfile)
   ! pdb file header generation
   ! Put unit cell information
   ! CRYST1   31.595   32.369   24.219  90.00  90.00  90.00 P 21 21 21    8
      type(CRYSTAL_TYPE), IN :: self
      type(TEXTFILE_TYPE), allocatable :: pdbfile
      REAL :: factor
      call save_style_(pdbfile)
      call set_margin_width_(pdbfile,0)
      call put_(pdbfile,"CRYST1",width=6)
      call set_real_precision_(pdbfile,3)
      call set_real_width_(pdbfile,9)
      call put_(pdbfile,to_units_(self%unit_cell%length(1),"angstrom"))
      call put_(pdbfile,to_units_(self%unit_cell%length(2),"angstrom"))
      call put_(pdbfile,to_units_(self%unit_cell%length(3),"angstrom"))
      call set_real_precision_(pdbfile,2)
      call set_real_width_(pdbfile,7)
      call put_(pdbfile,to_units_(self%unit_cell%angle(1),"degree"))
      call put_(pdbfile,to_units_(self%unit_cell%angle(2),"degree"))
      call put_(pdbfile,to_units_(self%unit_cell%angle(3),"degree"))
      call put_(pdbfile,self%spacegroup%HM_symbol,width=10)
      call put_(pdbfile,self%Z,width=5)
      call flush_(pdbfile)
      call put_(pdbfile,"SCALE1",width=6)
      call tab_(pdbfile,width=6)
      call set_real_precision_(pdbfile,7)
      call set_real_width_(pdbfile,10)
      factor = 1.0d0
      call convert_to_(factor,"angstrom^-1")
      call put_(pdbfile,self%unit_cell%inverse_mx(1,1)*factor)
      call put_(pdbfile,self%unit_cell%inverse_mx(1,2)*factor)
      call put_(pdbfile,self%unit_cell%inverse_mx(1,3)*factor)
      call flush_(pdbfile)
      call put_(pdbfile,"SCALE2",width=6)
      call tab_(pdbfile,width=6)
      call put_(pdbfile,self%unit_cell%inverse_mx(2,1)*factor)
      call put_(pdbfile,self%unit_cell%inverse_mx(2,2)*factor)
      call put_(pdbfile,self%unit_cell%inverse_mx(2,3)*factor)
      call flush_(pdbfile)
      call put_(pdbfile,"SCALE3",width=6)
      call tab_(pdbfile,width=6)
      call put_(pdbfile,self%unit_cell%inverse_mx(3,1)*factor)
      call put_(pdbfile,self%unit_cell%inverse_mx(3,2)*factor)
      call put_(pdbfile,self%unit_cell%inverse_mx(3,3)*factor)
      call flush_(pdbfile)

      !SCALE1     0.1053741 0.0175390 0.0172991      0.0000000
      !SCALE2     0.0000000 0.0601636 0.0076747      0.0000000
      !SCALE3     0.0000000 0.0000000 0.0321397      0.0000000
      call unsave_style_(pdbfile)
   end subroutine


! CIF

   subroutine put_asymm_unit_CIF(self,cif)
   ! Put out an asymmetric-unit "cif" file
      type(CRYSTAL_TYPE), INOUT :: self
      type(CIF_TYPE), IN :: cif
      BIN :: has_covariances
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:put_asymm_unit_CIF ... no asymmetric_unit_atom list")
      has_covariances = has_covariance_mx_(self)
      if (has_covariances) then
         call put_CIF_(self,cif,self%asymmetric_unit_atom,self%xray_data%covariance_mx)
      else
         call put_CIF_(self,cif,self%asymmetric_unit_atom)
      end if
   end subroutine

   subroutine put_fragment_CIF(self,cif)
   ! Put out a fragment_atom "cif" file
      type(CRYSTAL_TYPE), INOUT :: self
      type(CIF_TYPE), IN :: cif
      BIN :: has_covariances
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:put_fragment_CIF ... no fragment_atom list")
      has_covariances = has_fragment_covariance_mx_(self)
      if (has_covariances) then
         call put_CIF_(self,cif,self%fragment_atom,self%xray_data%fragment_covariance_mx)
      else
         call put_CIF_(self,cif,self%fragment_atom)
      end if
   end subroutine

   subroutine put_fragment_CIF_HBB(self,cif)
   ! Put out a fragment_atom "cif" file
      type(CRYSTAL_TYPE), INOUT :: self
      type(CIF_TYPE), IN :: cif
      BIN :: has_covariances
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:put_fragment_CIF_HBB ... no fragment_atom list")
      has_covariances = has_fragment_covariance_mx_(self)
      if (has_covariances) then
         call put_CIF_HBB_(self,cif,self%fragment_atom,self%xray_data%fragment_covariance_mx)
      end if
   end subroutine

   subroutine put_asymm_unit_CIF_bond_info(self)
   ! Put out an asymmetric-unit "cif" file, bond info only
      type(CRYSTAL_TYPE), INOUT :: self
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:put_asymm_unit_CIF_bond_info ... no asymmetric_unit_atom list")
   ENSURE(self%asymmetric_unit_atom(1)%pos_axis_system=="cartesian","CRYSTAL:put_asymm_unit_CIF_bond_info ... not cartesian axes!")
      if (NOT has_covariance_mx_(self)) return
      call put_CIF_bond_info_tables(self%asymmetric_unit_atom,self%xray_data%covariance_mx)
   end subroutine

   subroutine put_fragment_CIF_bond_info(self)
   ! Put out a fragment_atom "cif" file, bond info only
      type(CRYSTAL_TYPE), IN :: self
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:put_fragment_CIF_bond_info ... no fragment_atom list")
   ENSURE(self%fragment_atom(1)%pos_axis_system=="cartesian","CRYSTAL:put_fragment_CIF_bond_info ... not cartesian axes!")
      if (NOT has_fragment_covariance_mx_(self)) return
      call put_CIF_bond_info_tables(self%fragment_atom,self%xray_data%fragment_covariance_mx)
   end subroutine

   PURE function has_covariances(self) result (res)
   ! Does the covariance matrix exist?
      type(CRYSTAL_TYPE), IN :: self
      BIN :: res
      res = has_covariance_mx_(self) AND has_fragment_covariance_mx_(self)
   end function

   PURE function has_covariance_mx(self) result (res)
   ! Does the covariance matrix exist?
      type(CRYSTAL_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%covariance_mx)) then
            res = TRUE
         end if
      end if
   end function

   PURE function has_fragment_covariance_mx(self) result (res)
   ! Does the fragment_covariance matrix exist?
      type(CRYSTAL_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%fragment_covariance_mx)) then
            res = TRUE
         end if
      end if
   end function

   subroutine put_CIF_0(self,cif,atom)
   ! Put an "atom" cif file without errors.
      type(CRYSTAL_TYPE), INOUT :: self
      type(CIF_TYPE), IN :: cif
      VEC(type(ATOM_TYPE),:), IN :: atom
      STR(len=STR_SIZE) :: thresh
      REAL :: lambda
      BIN :: refine_H_U_iso,cart,har
      VEC(REAL,:), allocatable :: esd

      ! Placeholders
      call put_CIF_crystal_data

      ! Spacegroup information
      call put_CIF_(self%spacegroup)

      ! Unit cell information; INOUT
      cart = atom(1)%pos_axis_system=="cartesian"
      har = FALSE
      if (allocated(self%xray_data)) har = self%xray_data%refine_structure OR self%xray_data%fit_structure
      call put_CIF_(self%unit_cell,self%Z,cart,har)

      ! Any data?
      refine_H_U_iso = TRUE
      if (allocated(self%xray_data)) refine_H_U_iso = self%xray_data%refine_H_U_iso

      ! Put the info.
      ! If the atoms carry their own covariance matrices -- as they do for
      ! a structure read from a CIF, where they were derived from the
      ! reported uncertainties -- the esds are made from those, in whatever
      ! axis system the atoms are currently in. Otherwise there is nothing
      ! to make them from and the writers omit them, saying why.
      if (has_covariance_mxs_(atom)) then
         call create_(esd,no_of_pADPs_(atom))
         call make_CIF_esds_from_atom_covariances(self,atom,esd)
         call put_CIF_coords_(atom,refine_H_U_iso,esd)
         call put_CIF_ADP2_(atom,refine_H_U_iso,esd)
         call destroy_(esd)
      else
         call put_CIF_coords_(atom,refine_H_U_iso)
         call put_CIF_ADP2_(atom,refine_H_U_iso)
      end if
      call put_CIF_ADPn_(atom,cif)

      ! Put covariance matrix  
      if (cif%use_CIF2) then
         if (atom(1)%pos_axis_system=="cartesian") then
            if (allocated(self%xray_data)) then
               if (allocated(self%xray_data%reflections)) then
                  call put_CIF_covariance_matrices_(self%xray_data,cif)
               end if
            end if
         end if
      end if

      ! Put wavelength and other refl data
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%reflections)) then
            thresh = CIF_threshold_expression_(self%xray_data)
            lambda = self%xray_data%wavelength
            call put_CIF_refln_data_(self%xray_data%reflections,thresh,lambda)
         end if
      end if

      ! Put the reflection data
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%reflections)) then

      ! .put_xd_fco
         end if
      end if
   end subroutine

   subroutine put_CIF_1(self,cif,atom,covariance_mx)
   ! Put a *molecular fragment* cif file with errors, using "atom"
   ! list.  ESDs are produced from .xray_data.fragment_covariance_mx
   ! and NOTE: must have the correct units on entry.
      type(CRYSTAL_TYPE), INOUT :: self
      type(CIF_TYPE), IN :: cif
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), IN :: covariance_mx
      INT :: n_pADPs
      STR(len=STR_SIZE) :: thresh
      REAL :: lambda
      BIN :: cart,har
      VEC(REAL,:), allocatable :: esd
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_CIF_1 ... no diffraction data!")

      ! Placeholders
      call put_CIF_crystal_data

      ! Spacegroup information
      call put_CIF_(self%spacegroup)

      ! Unit cell information; INOUT
      cart = atom(1)%pos_axis_system=="cartesian"
      har = FALSE
      if (allocated(self%xray_data)) har = self%xray_data%refine_structure OR self%xray_data%fit_structure
      call put_CIF_(self%unit_cell,self%Z,cart,har)

      ! Put refinement special info
      call put_CIF_refinement_(self%xray_data)

      ! Errors
      n_pADPs = no_of_pADPs_(atom)
      call create_(esd,n_pADPs)
      call make_CIF_esds(self,atom,covariance_mx,esd)
      esd = sqrt(esd)

      ! Put the info
      call put_CIF_coords_(atom,self%xray_data%refine_H_U_iso,esd)
      call put_CIF_ADP2_(atom,self%xray_data%refine_H_U_iso,esd)
      call put_CIF_ADPn_(atom,cif,esd)

      ! Put bond & angle info
      if (atom(1)%pos_axis_system=="cartesian") then
         call put_CIF_bond_info_tables(atom,covariance_mx)
      end if

      ! Clean
      call destroy_(esd)

      ! Put covariance matrix  
      if (cif%use_CIF2) then
         if (includes_(stdout%name,"cif2")) then
            if (atom(1)%pos_axis_system=="cartesian") then
               if (allocated(self%xray_data)) then
                  if (allocated(self%xray_data%reflections)) then
                     call put_CIF_covariance_matrices_(self%xray_data,cif)
                  end if
               end if
            end if
         end if
      end if

      ! Put wavelength and other refl data
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%reflections)) then
            thresh = CIF_threshold_expression_(self%xray_data)
            lambda = self%xray_data%wavelength
            call put_CIF_refln_data_(self%xray_data%reflections,thresh,lambda)
         end if
      end if

      ! Put the reflection data
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%reflections)) then

      ! .put_xd_fco
         end if
      end if
   end subroutine

   subroutine put_CIF_HBB(self,cif,atom,covariance_mx)
   ! Put a *molecular fragment* cif file with errors, using "atom"
   ! list.  ESDs are produced from .xray_data.fragment_covariance_mx
   ! and NOTE: must have the correct units on entry.
      type(CRYSTAL_TYPE), INOUT :: self
      type(CIF_TYPE), IN :: cif
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), IN :: covariance_mx
      INT :: n_pADPs
      BIN :: cart,har
      VEC(REAL,:), allocatable :: esd
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_CIF_HBB ... no diffraction data!")

      ! Placeholders
      call put_CIF_crystal_data

      ! Spacegroup information
      call put_CIF_(self%spacegroup)

      ! Unit cell information; INOUT
      cart = atom(1)%pos_axis_system=="cartesian"
      har = FALSE
      if (allocated(self%xray_data)) har = self%xray_data%refine_structure OR self%xray_data%fit_structure
      call put_CIF_(self%unit_cell,self%Z,cart,har)

      ! Put refinement special info
      call put_CIF_refinement_(self%xray_data)

      ! Errors
      n_pADPs = no_of_pADPs_(atom)
      call create_(esd,n_pADPs)
      call make_CIF_esds(self,atom,covariance_mx,esd)
      esd = sqrt(esd)

      ! Put the info
      call put_CIF_coords_cartn_HBB_(atom,self%xray_data%refine_H_U_iso,esd)
      call put_CIF_ADP2_cartn_HBB_(atom,self%xray_data%refine_H_U_iso,esd)
      call put_CIF_ADP2_in_inertial_axes_cartn_HBB_(atom,covariance_mx,self%xray_data%refine_H_U_iso)
      call put_CIF_ADP2_in_ADP2_principal_axes_cartn_HBB_(atom,covariance_mx,self%xray_data%refine_H_U_iso)
      call put_CIF_ADP3_cartn_HBB_(atom,esd,cif%GC3_scale_factor)
      call put_CIF_ADP3_in_inertial_axes_cartn_HBB_(atom,covariance_mx,cif%GC3_scale_factor)
      call put_CIF_ADP3_in_ADP2_principal_axes_cartn_HBB_(atom,covariance_mx,cif%GC3_scale_factor)
      call put_CIF_ADP4_cartn_HBB_(atom,esd,cif%GC4_scale_factor)
      call put_CIF_ADP4_in_inertial_axes_cartn_HBB_(atom,covariance_mx,cif%GC4_scale_factor)
      call put_CIF_ADP4_in_ADP2_principal_axes_cartn_HBB_(atom,covariance_mx,cif%GC4_scale_factor)

      ! Put bond & angle info
      if (atom(1)%pos_axis_system=="cartesian") then
         call put_CIF_bond_Hirshfeld_tables(atom,covariance_mx)
      end if

      ! Correlations
      call flush_(stdout)
      call put_correlations_in_cif_(self)

      ! Clean
      call destroy_(esd)
   end subroutine

   subroutine put_CIF_crystal_data()
   ! Put placeholders forCIF crystal data
      call flush_(stdout)
      call text_(stdout,"# =======")
      call text_(stdout,"# Crystal")
      call text_(stdout,"# =======")
      call flush_(stdout)
      call show_(stdout,"_exptl_crystal_description         ","?",dots=FALSE)
      call show_(stdout,"_exptl_crystal_colour              ","?",dots=FALSE)
      call show_(stdout,"_exptl_crystal_size_max            ","?",dots=FALSE)
      call show_(stdout,"_exptl_crystal_size_mid            ","?",dots=FALSE)
      call show_(stdout,"_exptl_crystal_size_min            ","?",dots=FALSE)
      call show_(stdout,"_exptl_crystal_density_diffrn      ","?",dots=FALSE)
      call flush_(stdout)
      call text_(stdout,"# ==============")
      call text_(stdout,"# Absortion data")
      call text_(stdout,"# ==============")
      call flush_(stdout)
      call show_(stdout,"_exptl_absorpt_coefficient_mu      ","?",dots=FALSE)
      call show_(stdout,"_exptl_absorpt_correction_type     ","'none'",dots=FALSE)
      call show_(stdout,"_exptl_absorpt_process_details     ","?",dots=FALSE)
      call show_(stdout,"_exptl_absorpt_correction_T_min    ","?",dots=FALSE)
      call show_(stdout,"_exptl_absorpt_correction_T_max    ","?",dots=FALSE)
   end subroutine

   subroutine make_CIF_esds_from_atom_covariances(self,atom,esd)
   ! Make the "esd"s in the pADP layout for the "atom" list from each
   ! ATOM's OWN cartesian covariance matrix, rather than from a
   ! whole-structure one. Used for a structure read from a CIF, whose
   ! covariances were derived from the reported uncertainties rather than
   ! measured by a refinement.
   ! The esds come out in the axis system the atoms are currently in, so
   ! the crystal-frame writers get back exactly the numbers the CIF
   ! supplied, and the cartesian writers get genuine cartesian ones.
   ! NOTE: deliberately NOT done by assembling a block-diagonal
   ! whole-structure covariance and reusing make_CIF_esds. That matrix is
   ! passed on to put_CIF_bond_info_tables, which genuinely needs the
   ! inter-atom off-diagonal blocks; zeros there would report bond esds
   ! computed as though atomic positions were uncorrelated. See TASKS_AND_HISTORY.md.
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      VEC(REAL,:), OUT :: esd
      MAT(REAL,:,:), allocatable :: T,C
      INT :: a,n,ab,af,al
   ENSURE(has_covariance_mxs_(atom),"CRYSTAL:make_CIF_esds_from_atom_covariances ... an atom has no covariance matrix")
   ENSURE(size(esd)==no_of_pADPs_(atom),"CRYSTAL:make_CIF_esds_from_atom_covariances ... wrong size, esd")
      call create_(T,34,34)
      call make_pADP_frame_map(self,atom,T)
      esd = ZERO
      do a = 1,size(atom)
         n = no_of_pADPs_(atom(a))
         ab = no_of_pADPs_up_to_atom_(atom,a)
         af = ab+1
         al = ab+n
         call create_(C,n,n)
         call back_transform_to_(atom(a)%covariance_mx,C,T(1:n,1:n))
         call put_diagonal_to_(C,esd(af:al))
         call destroy_(C)
      end do
      call destroy_(T)

      ! Variances -> esds. T V T^T is positive semi-definite whenever V is,
      ! so this cannot produce a NaN; max() guards only against round-off
      ! on a component whose true variance is exactly zero.
      esd = sqrt(max(esd,ZERO))
   end subroutine

   subroutine make_pADP_frame_map(self,atom,T)
   ! Set "T", the 34x34 map taking a CARTESIAN pADP covariance matrix into
   ! the axis system the "atom" list is currently in. The unit matrix if
   ! they are already cartesian.
   ! NOTE the transpose on the ADP2 block. symmetric_tensor_2_product_mx
   ! (res,R) returns the induced map for U -> R U R^T, but the ADP *values*
   ! go to the crystal frame by MAT:change_basis_using(reciprocal_U_mx),
   ! which is B^T U B. The esds must follow the same map as the values, so
   ! B^T is what goes in. Passing B is silent whenever B is symmetric --
   ! i.e. for every orthogonal cell, which is every CIF-writing test in the
   ! suite -- and wrong by up to 37% on a hexagonal one. This is the one
   ! place that transpose is written down; see TASKS_AND_HISTORY.md.
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), INOUT :: T
      T = ZERO
      if (atom(1)%ADP_axis_system=="cartesian") then

         ! Do nothing
         call to_unit_mx_(T)
      else

         ! Usual ADPs & dimensionless positions and (GC) anharmonics
         T(1:3,1:3) = self%unit_cell%inverse_mx  ! Reset positional transform
         call symmetric_tensor_2_product_mx_(T(4:9,4:9),transpose(self%unit_cell%reciprocal_U_mx))
         call symmetric_tensor_3_product_mx_(T(10:19,10:19),transpose(self%unit_cell%reciprocal_mx))
         call symmetric_tensor_4_product_mx_(T(20:34,20:34),transpose(self%unit_cell%reciprocal_mx))
      end if
   end subroutine

   subroutine make_CIF_esds(self,atom,covariance_mx,esd)
   ! Make the "esd"s in the pADPs for the "atom" list from the
   ! "covariance_mx", required for the "cif" file output.  if
   ! "atom.axis_system" is "cartesian", only the diagonal covariances
   ! are extracted. For "crystal" axis system, positions are unitless
   ! and ADP's are in bohr^2, and anharmonic constants are
   ! dimensionless Gram-Charlier.
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), IN :: covariance_mx
      VEC(REAL,:), OUT :: esd
      MAT(REAL,:,:), allocatable :: T,C
      INT :: n_pADPs
      INT :: a,n,ab,af,al
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_CIF_esds ... no data")
   ENSURE(atom(1)%pos_axis_system==atom(1)%ADP_axis_system,"CRYSTAL:make_CIF_esds ... inconsistent pos and ADP axis systems")
   ENSURE(atom(1)%pos_axis_system==atom(1)%ADPn_axis_system,"CRYSTAL:make_CIF_esds ... inconsistent pos and ADPn axis systems")

      ! No of pADPs
      n_pADPs = no_of_pADPs_(atom)
      ENSURE(size(esd)==n_pADPs,"CRYSTAL:make_CIF_esds ... wrong size, esd")

      ! Get standard deviations and covariances
      ! ... in crystal coordinates
      call create_(C,n_pADPs,n_pADPs)
      C = ZERO

      ! The covariance matrix transformation matrix
      ! This is only applied to the diagonal blocks.
      call create_(T,34,34)
      call make_pADP_frame_map(self,atom,T)
      do a = 1,size(atom)

         ! Block limits
         n = no_of_pADPs_(atom(a))
         ab = no_of_pADPs_up_to_atom_(atom,a)
         af = ab+1
         al = ab+n

         ! Transform ...
         call back_transform_to_(covariance_mx(af:al,af:al),C(af:al,af:al),T(1:n,1:n))
      end do

      ! Get the esd's ... only diagonal (blocks) needed
      call put_diagonal_to_(C,esd)

      ! Clean
      call destroy_(C)
      call destroy_(T)
   end subroutine

   subroutine put_CIF_reflections_head(self)
   ! Put reflection information in CIF format *on an absolute scale*
      type(CRYSTAL_TYPE), IN :: self
      REAL :: lambda
      REAL :: thmax
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_CIF_reflections_head ... no diffraction data!")
      call flush_(stdout)
      call text_(stdout,"# ================")
      call text_(stdout,"# Diffraction data")
      call text_(stdout,"# ================")
      lambda = self%xray_data%wavelength
      call convert_to_(lambda,"angstrom")
      call flush_(stdout)
      call show_(stdout,"_diffrn_radiation_type             ","?",dots=FALSE)
      call show_(stdout,"_diffrn_radiation_wavelength       ",lambda,dots=FALSE)
      call flush_(stdout)
      call show_(stdout,"_diffrn_measurement_method         ","?",dots=FALSE)
      call show_(stdout,"_diffrn_measurement_device_type    ","?",dots=FALSE)
      thmax = maxval(abs(self%xray_data%reflections(:)%theta))*DEGREE_PER_RADIAN
      call flush_(stdout)
      call show_(stdout,"_diffrn_reflns_number              ","?",dots=FALSE)
      call show_(stdout,"_diffrn_reflns_theta_max           ",thmax,dots=FALSE)
   end subroutine

   subroutine put_CIF_reflections_rlim(self,threshold)
   ! Put reflection information in CIF format *on an absolute scale*
      type(CRYSTAL_TYPE), IN :: self
      STR(len=*), IN :: threshold
      INT :: n_refl
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_CIF_reflections_rlim ... no diffraction data!")

      ! Constants
      n_refl = size(self%xray_data%reflections)
      call flush_(stdout)
      call text_(stdout,"# ===========")
      call text_(stdout,"# Reflections")
      call text_(stdout,"# ===========")
      call flush_(stdout)
      call show_(stdout,"_reflns_d_resolution_low           ",d_max_(self%xray_data%reflections(:))*ANGSTROM_PER_BOHR,dots=FALSE)
      call show_(stdout,"_reflns_d_resolution_high          ",d_min_(self%xray_data%reflections(:))*ANGSTROM_PER_BOHR,dots=FALSE)
      call show_(stdout,"_reflns_limit_h_min                ",minval(self%xray_data%reflections(:)%h1),dots=FALSE)
      call show_(stdout,"_reflns_limit_h_max                ",maxval(self%xray_data%reflections(:)%h1),dots=FALSE)
      call show_(stdout,"_reflns_limit_k_min                ",minval(self%xray_data%reflections(:)%h2),dots=FALSE)
      call show_(stdout,"_reflns_limit_k_max                ",maxval(self%xray_data%reflections(:)%h2),dots=FALSE)
      call show_(stdout,"_reflns_limit_l_min                ",minval(self%xray_data%reflections(:)%h3),dots=FALSE)
      call show_(stdout,"_reflns_limit_l_max                ",maxval(self%xray_data%reflections(:)%h3),dots=FALSE)
      call show_(stdout,"_reflns_number_gt                  ",n_refl,dots=FALSE)
      call show_(stdout,"_reflns_number_total               ",n_refl,dots=FALSE)
      call show_(stdout,"_reflns_threshold_expression       ",threshold,dots=FALSE)
      call flush_(stdout)
      call text_(stdout,"_reflns_special_details            ")
      call text_(stdout,";")
      call text_(stdout,"* Specify symmetry and Friedel pair averaging *")
      call flush_(stdout)
      call text_(stdout,". Only.data reflections which satisfy the threshold expression")
      call text_(stdout,"  are listed below, and only they are considered observed,")
      call text_(stdout,"  thus the *_gt, *_all, & *_total data are always the same")
      call text_(stdout,";")

   ! fcmax = maxval(abs(.xray_data.reflections(:).F_calc))
   ! stdout.flush
   ! stdout.show("_shelx_F_calc_maximum              ", fcmax,dots=FALSE)
   end subroutine

   subroutine put_CIF_reflections(self,threshold)
   ! Put reflection information in CIF format *on an absolute scale*
      type(CRYSTAL_TYPE), IN :: self
      STR(len=*), IN :: threshold
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: stl,d,fac
      VEC(CPX,:), allocatable :: F_p
      VEC(REAL,:), allocatable :: F_cr,F_ci,F_ph
      VEC(REAL,:), allocatable :: F_pred,F_exp,F_sigma,F_z
      INT :: n_refl
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_CIF_reflections ... no diffraction data!")

      ! Constants
      n_refl = size(self%xray_data%reflections)
      call put_CIF_reflections_head_(self)
      call put_CIF_reflections_rlim_(self,threshold)
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"    _refln_index_h")
      call text_(stdout,"    _refln_index_k")
      call text_(stdout,"    _refln_index_l")
      call text_(stdout,"    _refln_index_group")
      call text_(stdout,"    _refln_stl")
      call text_(stdout,"    _refln_d_spacing")
      call text_(stdout,"    _refln_A_calc")
      call text_(stdout,"    _refln_B_calc")
      call text_(stdout,"    _refln_phase_calc")
      call text_(stdout,"    _refln_F_calc")
      call text_(stdout,"    _refln_F_meas")
      call text_(stdout,"    _refln_F_sigma")
      call text_(stdout,"    _refln_delta_F_on_sigma")

      ! Get data
      stl = self%xray_data%reflections(:)%stl*BOHR_PER_ANGSTROM
      d = self%xray_data%reflections(:)%d*ANGSTROM_PER_BOHR
      F_p = F_pred_cpx_(self%xray_data)
      call create_(F_ph,n_refl)
      call create_(F_exp,n_refl)
      call create_(F_pred,n_refl)
      call create_(F_cr,n_refl)
      call create_(F_ci,n_refl)
      call create_(F_z,n_refl)
      call create_(F_sigma,n_refl)
      F_ph = F_phase_angle_in_degrees_(self%xray_data%reflections(:))
      F_exp = F_exp_scaled_corrected_(self)
      fac = ONE/F_scale_and_extn_correction_(self%xray_data)
   ! fac     = sqrt(fac) ! ??? CHECK
      F_pred = abs(F_p)*fac  ! Warning ... this may include dispersion !
      F_cr = RE(F_p)*fac
      F_ci = IM(F_p)*fac
      F_sigma = self%xray_data%reflections%F_sigma*fac
      F_z = F_z_(self%xray_data%reflections(:))

      ! Make table
      call create_(table,13)
      call set_values_(table(1),self%xray_data%reflections(:)%h1)
      call set_values_(table(2),self%xray_data%reflections(:)%h2)
      call set_values_(table(3),self%xray_data%reflections(:)%h3)
      call set_values_(table(4),self%xray_data%reflections(:)%group)
      call set_values_(table(5),stl)
      call set_values_(table(6),d)
      call set_values_(table(7),F_cr)
      call set_values_(table(8),F_ci)
      call set_values_(table(9),F_ph)
      call set_values_(table(10),F_pred)
      call set_values_(table(11),F_exp)
      call set_values_(table(12),F_sigma)
      call set_values_(table(13),F_z)

      ! Make table
      call set_width_(table(1),4)
      call set_width_(table(2),4)
      call set_width_(table(3),4)
      call set_width_(table(4),3)
      call set_width_(table(5),8)
      call set_real_precision_(table(5),3)
      call set_width_(table(6),8)
      call set_real_precision_(table(6),3)
      call set_width_(table(7),8)
      call set_real_precision_(table(7),3)
      call set_width_(table(8),8)
      call set_real_precision_(table(8),3)
      call set_width_(table(9),8)
      call set_real_precision_(table(9),3)
      call set_width_(table(10),8)
      call set_real_precision_(table(10),3)
      call set_width_(table(11),8)
      call set_real_precision_(table(11),3)
      call set_width_(table(12),8)
      call set_real_precision_(table(12),3)
      call set_width_(table(13),8)
      call set_real_precision_(table(13),3)

      ! Body
      call put_body_(table)

      ! Clean
      call destroy_(table)
      call destroy_(F_ph)
      call destroy_(F_exp)
      call destroy_(F_pred)
      call destroy_(F_cr)
      call destroy_(F_ci)
      call destroy_(F_z)
      call destroy_(F_sigma)
   end subroutine


! fcf files

   subroutine put_Olex2_fcf(self,threshold)
   ! Put reflection information in CIF format *on an absolute scale*
   ! in the Olex2 way ... ?
      type(CRYSTAL_TYPE), IN :: self
      STR(len=*), IN :: threshold
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: stl,d,fac
      VEC(CPX,:), allocatable :: F_p
      VEC(REAL,:), allocatable :: F_cr,F_ci,F_ph,F2m,F2s,F2c
      VEC(REAL,:), allocatable :: F_pred,F_exp,F_sigma,F_z
      INT :: n_refl
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_Olex2_fcf ... no diffraction data!")

      ! Constants
      n_refl = size(self%xray_data%reflections)
      call put_CIF_reflections_head_(self)
      call put_CIF_reflections_rlim_(self,threshold)
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"    _refln_index_h")
      call text_(stdout,"    _refln_index_k")
      call text_(stdout,"    _refln_index_l")
      call text_(stdout,"    _refln_index_group")
      call text_(stdout,"    _refln_stl")
      call text_(stdout,"    _refln_d_spacing")
      call text_(stdout,"    _refln_A_calc")
      call text_(stdout,"    _refln_B_calc")
      call text_(stdout,"    _refln_phase_calc")
      call text_(stdout,"    _refln_F_calc")
      call text_(stdout,"    _refln_F_meas")
      call text_(stdout,"    _refln_F_sigma")
      call text_(stdout,"    _refln_delta_F_on_sigma")
      call text_(stdout,"    _refln_F_squared_meas")
      call text_(stdout,"    _refln_F_squared_sigma")
      call text_(stdout,"    _refln_F_squared_calc")

      ! Get data
      stl = self%xray_data%reflections(:)%stl*BOHR_PER_ANGSTROM
      d = self%xray_data%reflections(:)%d*ANGSTROM_PER_BOHR
      F_p = F_pred_cpx_(self%xray_data)
      call create_(F_ph,n_refl)
      call create_(F2m,n_refl)
      call create_(F2c,n_refl)
      call create_(F2s,n_refl)
      call create_(F_exp,n_refl)
      call create_(F_pred,n_refl)
      call create_(F_cr,n_refl)
      call create_(F_ci,n_refl)
      call create_(F_z,n_refl)
      call create_(F_sigma,n_refl)
      F2m = F2_exp_scaled_corrected_(self)
      fac = F2m/F2_exp_(self%xray_data%reflections(:))
      F2s = F2_sigma_(self%xray_data%reflections(:))*fac
      F2c = F2_calc_abs_(self%xray_data%reflections(:))
      F_ph = F_phase_angle_in_degrees_(self%xray_data%reflections(:))
      F_exp = F_exp_scaled_corrected_(self)
      fac = ONE/F_scale_and_extn_correction_(self%xray_data)
      F_pred = abs(F_p)*fac  ! Warning ... this may include dispersion !
      F_cr = RE(F_p)
      F_cr = F_cr*fac
      F_ci = IM(F_p)
      F_ci = F_ci*fac
      F_sigma = self%xray_data%reflections%F_sigma*fac
      F_z = F_z_(self%xray_data%reflections(:))

      ! Make table
      call create_(table,16)
      call set_values_(table(1),self%xray_data%reflections(:)%h1)
      call set_values_(table(2),self%xray_data%reflections(:)%h2)
      call set_values_(table(3),self%xray_data%reflections(:)%h3)
      call set_values_(table(4),self%xray_data%reflections(:)%group)
      call set_values_(table(5),stl)
      call set_values_(table(6),d)
      call set_values_(table(7),F_cr)
      call set_values_(table(8),F_ci)
      call set_values_(table(9),F_ph)
      call set_values_(table(10),F_pred)
      call set_values_(table(11),F_exp)
      call set_values_(table(12),F_sigma)
      call set_values_(table(13),F_z)
      call set_values_(table(14),F2m)
      call set_values_(table(15),F2s)
      call set_values_(table(16),F2c)

      ! Body
      call put_body_(table)

      ! Clean
      call destroy_(table)
      call destroy_(F_ph)
      call destroy_(F_exp)
      call destroy_(F_pred)
      call destroy_(F_cr)
      call destroy_(F_ci)
      call destroy_(F_z)
      call destroy_(F_sigma)
      call destroy_(F2m)
      call destroy_(F2c)
      call destroy_(F2s)
   end subroutine

   subroutine put_tonto_fcf(self)
   ! Put a tonto fcf file with all the info and scaled so that
   ! Olex2 can make difference density maps.
      type(CRYSTAL_TYPE), IN :: self
      STR(len=STR_SIZE) :: thresh
      if (NOT allocated(self%xray_data)) return
      if (NOT allocated(self%xray_data%reflections)) return
      thresh = CIF_threshold_expression_(self%xray_data)
      call put_CIF_reflections_(self,thresh)
   end subroutine

   subroutine put_shelx_fcf5(self)
   ! Put shelx fcf LIST=5 reflection file.
      type(CRYSTAL_TYPE), INOUT :: self
      REAL :: F000
      REAL :: fcmax
      VEC(REAL,:), allocatable :: F_corr
      INT :: i

      ! Spacegroup information
      call put_CIF_(self%spacegroup)

      ! Unit cell information; INOUT
      call put_CIF_(self%unit_cell,self%Z,cart=FALSE,har=FALSE)

      ! Put the reflection data
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%reflections)) then
            F000 = no_of_unit_cell_electrons_(self)
            call flush_(stdout)
            call text_(stdout,"# The value below is just the no. of unit cell electrons")
            call flush_(stdout)
            call show_(stdout,"_exptl_crystal_F_000               ",F000,dots=FALSE)
            call flush_(stdout)
            call text_(stdout,"# ===============")
            call text_(stdout,"# Reflection data")
            call text_(stdout,"# ===============")
            call flush_(stdout)
            call show_(stdout,"_reflns_d_resolution_low           ",d_max_(self%xray_data%reflections)*ANGSTROM_PER_BOHR,dots=FALSE)
            call show_(stdout,"_reflns_d_resolution_high          ",d_min_(self%xray_data%reflections)*ANGSTROM_PER_BOHR,dots=FALSE)
            call show_(stdout,"_reflns_limit_h_min                ",minval(self%xray_data%reflections(:)%h1),dots=FALSE)
            call show_(stdout,"_reflns_limit_h_max                ",maxval(self%xray_data%reflections(:)%h1),dots=FALSE)
            call show_(stdout,"_reflns_limit_k_min                ",minval(self%xray_data%reflections(:)%h2),dots=FALSE)
            call show_(stdout,"_reflns_limit_k_max                ",maxval(self%xray_data%reflections(:)%h2),dots=FALSE)
            call show_(stdout,"_reflns_limit_l_min                ",minval(self%xray_data%reflections(:)%h3),dots=FALSE)
            call show_(stdout,"_reflns_limit_l_max                ",maxval(self%xray_data%reflections(:)%h3),dots=FALSE)
            if (NOT allocated(self%xray_data%scale_factors)) then
               fcmax = self%xray_data%scale_factor
               call flush_(stdout)
               call show_(stdout,"_tonto_F_scale_factor              ",fcmax,dots=FALSE)
            end if
            fcmax = maxval(F_calc_abs_(self%xray_data%reflections))
            call flush_(stdout)
            call show_(stdout,"_shelx_refln_list_code             ",5,dots=FALSE)
            call show_(stdout,"_shelx_F_calc_maximum              ",fcmax,dots=FALSE)
            F_corr = F_exp_scaled_corrected_(self)
            call flush_(stdout)
            call text_(stdout,"loop_")
            call text_(stdout,"    _refln_index_h")
            call text_(stdout,"    _refln_index_k")
            call text_(stdout,"    _refln_index_l")
            call text_(stdout,"    _refln_F_meas")
            call text_(stdout,"    _refln_F_calc")
            call text_(stdout,"    _refln_phase_calc")
            do i = 1,size(self%xray_data%reflections)
               call put_(stdout,self%xray_data%reflections(i)%h1,width=4)
               call put_(stdout,self%xray_data%reflections(i)%h2,width=4)
               call put_(stdout,self%xray_data%reflections(i)%h3,width=4)
               call put_(stdout,F_corr(i),width=10,precision=2)
               call put_(stdout,F_calc_abs_(self%xray_data%reflections(i)),width=10,precision=2)
               call put_(stdout,F_phase_angle_in_degrees_(self%xray_data%reflections(i)),width=7,precision=2)
               call flush_(stdout)
            end do
         end if
      end if
   end subroutine

   subroutine put_shelx_fcf6(self)
   ! Put shelx fcf LIST=6 reflection file
      type(CRYSTAL_TYPE), INOUT :: self
      REAL :: F000
      REAL :: fcmax
      VEC(REAL,:), allocatable :: F2_cor,factor
      INT :: i

      ! Spacegroup information
      call put_CIF_(self%spacegroup)

      ! Unit cell information; INOUT
      call put_CIF_(self%unit_cell,self%Z,cart=FALSE,har=FALSE)

      ! Put the reflection data
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%reflections)) then
            F000 = no_of_unit_cell_electrons_(self)
            call flush_(stdout)
            call text_(stdout,"# The value below is just the no. of unit cell electrons")
            call flush_(stdout)
            call show_(stdout,"_exptl_crystal_F_000               ",F000,dots=FALSE)
            call flush_(stdout)
            call flush_(stdout)
            call text_(stdout,"# ===============")
            call text_(stdout,"# Reflection data")
            call text_(stdout,"# ===============")
            call flush_(stdout)
            call show_(stdout,"_reflns_d_resolution_low           ",d_max_(self%xray_data%reflections)*ANGSTROM_PER_BOHR,dots=FALSE)
            call show_(stdout,"_reflns_d_resolution_high          ",d_min_(self%xray_data%reflections)*ANGSTROM_PER_BOHR,dots=FALSE)
            call show_(stdout,"_reflns_limit_h_min                ",minval(self%xray_data%reflections(:)%h1),dots=FALSE)
            call show_(stdout,"_reflns_limit_h_max                ",maxval(self%xray_data%reflections(:)%h1),dots=FALSE)
            call show_(stdout,"_reflns_limit_k_min                ",minval(self%xray_data%reflections(:)%h2),dots=FALSE)
            call show_(stdout,"_reflns_limit_k_max                ",maxval(self%xray_data%reflections(:)%h2),dots=FALSE)
            call show_(stdout,"_reflns_limit_l_min                ",minval(self%xray_data%reflections(:)%h3),dots=FALSE)
            call show_(stdout,"_reflns_limit_l_max                ",maxval(self%xray_data%reflections(:)%h3),dots=FALSE)
            if (NOT allocated(self%xray_data%scale_factors)) then
               fcmax = self%xray_data%scale_factor
               fcmax = fcmax*fcmax
               call flush_(stdout)
               call show_(stdout,"_tonto_F2_scale_factor             ",fcmax,dots=FALSE)
            end if
            fcmax = maxval(F_calc_abs_(self%xray_data%reflections))
            call flush_(stdout)
            call show_(stdout,"_shelx_refln_list_code             ",6,dots=FALSE)
            call show_(stdout,"_shelx_F_calc_maximum              ",fcmax,dots=FALSE)
            call flush_(stdout)
            call text_(stdout,"loop_")
            call text_(stdout,"    _refln_index_h")
            call text_(stdout,"    _refln_index_k")
            call text_(stdout,"    _refln_index_l")
            call text_(stdout,"    _refln_F_squared_meas")
            call text_(stdout,"    _refln_F_squared_sigma")
            call text_(stdout,"    _refln_F_squared_calc")
            call text_(stdout,"    _refln_phase_calc")
            F2_cor = F2_exp_scaled_corrected_(self)
            factor = F2_cor/F2_exp_(self%xray_data%reflections(:))
            do i = 1,size(self%xray_data%reflections)
               call put_(stdout,self%xray_data%reflections(i)%h1,width=4)
               call put_(stdout,self%xray_data%reflections(i)%h2,width=4)
               call put_(stdout,self%xray_data%reflections(i)%h3,width=4)
               call put_(stdout,F2_cor(i))
               call put_(stdout,F2_sigma_(self%xray_data%reflections(i))*factor(i))
               call put_(stdout,F2_calc_abs_(self%xray_data%reflections(i)))
               call put_(stdout,F_phase_angle_in_degrees_(self%xray_data%reflections(i)))
               call flush_(stdout)
            end do
         end if
      end if
   end subroutine


! XD fco file

   subroutine put_xd_fco(self)
   ! Put XD fco reflection file
      type(CRYSTAL_TYPE), INOUT :: self
      REAL :: F000,F2_cal,F2_exp,F2_sig
      VEC(CPX,:), allocatable :: F_p
      VEC(REAL,:), allocatable :: F_exp,fac,F_sigma
      REAL :: d_max,d_min
      VEC(INT,3) :: h_max,h_min
      type(REFLECTION_TYPE) :: ref
      INT :: i,n_refl

      !Get Number of reflections
      n_refl = size(self%xray_data%reflections)

      ! Spacegroup information
      call put_CIF_(self%spacegroup)

      ! Unit cell information; INOUT
      call put_CIF_(self%unit_cell,self%Z,cart=FALSE,har=FALSE)

      ! Put the reflection data
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%reflections)) then
            F000 = no_of_unit_cell_electrons_(self)
            F000 = F000*F000
            call flush_(stdout)
            call show_(stdout,"_exptl_crystal_F_squared_000        ",F000,dots=FALSE)
            call flush_(stdout)
            call flush_(stdout)
            call text_(stdout,"# ======================")
            call text_(stdout,"# XD fco reflection data")
            call text_(stdout,"# ======================")
            call flush_(stdout)
            call text_(stdout,"# . checkCIF also accepts this as fcf format")
            call flush_(stdout)
            call show_(stdout,"_shelx_refln_list_code             ",4,dots=FALSE)
            call flush_(stdout)
            d_max = d_max_(self%xray_data%reflections)*ANGSTROM_PER_BOHR
            d_min = d_min_(self%xray_data%reflections)*ANGSTROM_PER_BOHR
            call show_(stdout,"_reflns_d_resolution_high          ",d_min,dots=FALSE)
            call show_(stdout,"_reflns_d_resolution_low           ",d_max,dots=FALSE)
            h_max = h_max_(self%xray_data%reflections)
            h_min = h_min_(self%xray_data%reflections)
            call show_(stdout,"_reflns_limit_h_max                ",h_max(1),dots=FALSE)
            call show_(stdout,"_reflns_limit_h_min                ",h_min(1),dots=FALSE)
            call show_(stdout,"_reflns_limit_k_max                ",h_max(2),dots=FALSE)
            call show_(stdout,"_reflns_limit_k_min                ",h_min(2),dots=FALSE)
            call show_(stdout,"_reflns_limit_l_max                ",h_max(3),dots=FALSE)
            call show_(stdout,"_reflns_limit_l_min                ",h_min(3),dots=FALSE)
            call flush_(stdout)
            call text_(stdout,"# NOTE: F_squared_meas is scaled to F_squared_calc")
            call text_(stdout,"#       and so it is on an absolute scale")
            if (NOT use_multiple_scale_factors_(self%xray_data)) then
               call show_(stdout,"_reflns_tonto_F_scale_factor       ",self%xray_data%scale_factor,dots=FALSE)
            end if
            call flush_(stdout)
            call text_(stdout,"loop_")
            call text_(stdout,"    _refln_index_h")
            call text_(stdout,"    _refln_index_k")
            call text_(stdout,"    _refln_index_l")
            call text_(stdout,"    _refln_F_squared_calc")
            call text_(stdout,"    _refln_F_squared_meas")
            call text_(stdout,"    _refln_F_squared_sigma")
            call text_(stdout,"    _refln_observed_status")
            F_exp = F_exp_on_absolute_scale_(self%xray_data)  ! We can just multiply F_exp b y fac below!
            F_p = F_pred_cpx_(self%xray_data)
            fac = ONE/F_scale_and_extn_correction_(self%xray_data)
            F_sigma = self%xray_data%reflections(:)%F_sigma*fac
            do i = 1,size(self%xray_data%reflections)

            ! The reflection
               ref = self%xray_data%reflections(i)

            ! The numbers
         ! F2_cal = ref.F2_calc_abs
               F2_exp = F_exp(i)*F_exp(i)
         ! factor = F_exp(i)/ref.F_exp ! scale factor
         ! F2_sig = fac(i)*fac(i) * F_exp(i) * ref.F_sigma * TWO ! Lorraine, CHECK !!!!
         ! Lorraine here: Checked, and its wrong, corrected equation
         ! below. Tested against alanine dataset with scale factor 4.85
               F2_sig = F_exp(i)*F_sigma(i)*TWO
               F_p(i) = F_p(i)*fac(i)
               F2_cal = RE(F_p(i))*RE(F_p(i))+IM(F_p(i))*IM(F_p(i))
               if (IO_IS_ALLOWED) then
                  write(stdout%unit,"(3i4,2f12.2,1f10.2,1x,a1)") ref%h1,ref%h2,ref%h3,F2_cal,F2_exp,F2_sig,"o"
               end if
            end do
         end if
      end if
   end subroutine

   subroutine put_NKA_cartesian(self)
   ! Put out the NKA information in cartesian coordinates
      type(CRYSTAL_TYPE), IN :: self
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_NKA_cartesian ... no diffraction data")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:put_NKA_cartesian ... no asymmetric_unit_atom list")

      ! Cell line
      call flush_(stdout)
      call put_(stdout,"TCEL",width=4)
      call put_(stdout,self%xray_data%NKA_T_sequence_no,width=3)
      call put_(stdout,self%xray_data%temperature,width=6,precision=1)
      call put_(stdout,to_units_(self%unit_cell%length(1),"angstrom"))
      call put_(stdout,to_units_(self%unit_cell%length(2),"angstrom"))
      call put_(stdout,to_units_(self%unit_cell%length(3),"angstrom"))
      call put_(stdout,to_units_(self%unit_cell%angle(1),"degree"))
      call put_(stdout,to_units_(self%unit_cell%angle(2),"degree"))
      call put_(stdout,to_units_(self%unit_cell%angle(3),"degree"))
      call flush_(stdout)

      ! Coordinate information
      call put_NKA_with_errors_(self%asymmetric_unit_atom,self%xray_data%NKA_T_sequence_no)
   end subroutine

   subroutine put_excel_crystal(self)
   ! Put out the coordinate and ADP information in Excel format
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: esd
      INT :: n_pADPs
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_excel_crystal ... no diffraction data")
   ENSURE(allocated(self%xray_data%covariance_mx),"CRYSTAL:put_excel_crystal ... no covariance_mx")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:put_excel_crystal ... no asymmetric_unit_atom list")

      ! Cell line
      call flush_(stdout)
      call put_(stdout,"TCEL",width=4)
      call put_(stdout,self%xray_data%NKA_T_sequence_no,width=3)
      call put_(stdout,self%xray_data%temperature,width=6,precision=1)
      call put_(stdout,to_units_(self%unit_cell%length(1),"angstrom"))
      call put_(stdout,to_units_(self%unit_cell%length(2),"angstrom"))
      call put_(stdout,to_units_(self%unit_cell%length(3),"angstrom"))
      call put_(stdout,to_units_(self%unit_cell%angle(1),"degree"))
      call put_(stdout,to_units_(self%unit_cell%angle(2),"degree"))
      call put_(stdout,to_units_(self%unit_cell%angle(3),"degree"))
      call flush_(stdout)

      ! Get errors in crystal coordinates
      n_pADPs = no_of_pADPs_(self%asymmetric_unit_atom)
      call create_(esd,n_pADPs)
      call make_CIF_esds_(self,self%asymmetric_unit_atom,self%xray_data%covariance_mx,esd)
      esd = sqrt(esd)

      ! Change axis system to crystal
      call change_axis_system_to_(self%asymmetric_unit_atom,"crystal",self%unit_cell)

      ! Dump the data
      call put_excel_with_errors_(self%asymmetric_unit_atom,esd,self%xray_data%temperature)

      ! Change axes back to cartesian
      call change_axis_system_to_(self%asymmetric_unit_atom,"cartesian",self%unit_cell)

      ! Clean up
      call destroy_(esd)
   end subroutine

   subroutine put_uc_connection_table(self)
   ! Put the .unit_cell_connection table
      type(CRYSTAL_TYPE), IN :: self
      INT :: m,ma,a,i,b
      BIN :: first
   ENSURE(allocated(self%unit_cell_connection),"CRYSTAL:put_uc_connection_table ... no unit_cell_connection")
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:put_uc_connection_table ... no unit_cell_mol_for_atom")
      call flush_(stdout)
      call text_(stdout,"Unit cell atom connection table:")
      call flush_(stdout)
      call show_(stdout,"n_unit_cell_mols =",self%n_unit_cell_mols)
      call flush_(stdout)
      call dash_(stdout,int_fields=6)
      call put_(stdout,"UnitCell",int_width=TRUE)
      call tab_(stdout,int_fields=1)
      call put_(stdout,"Joins",int_width=TRUE)
      call tab_(stdout,int_fields=1)
      call put_(stdout,"In Cell",int_width=TRUE)
      call flush_(stdout)
      call put_(stdout,"Mol.",int_width=TRUE)
      call put_(stdout,"Atom 1",int_width=TRUE)
      call put_(stdout,"Atom 2",int_width=TRUE)
      call put_(stdout,"h",int_width=TRUE)
      call put_(stdout,"k",int_width=TRUE)
      call put_(stdout,"l",int_width=TRUE)
      call flush_(stdout)
      call dash_(stdout,int_fields=6)
      do m = 1,self%n_unit_cell_mols
         call put_(stdout,m)
         first = TRUE
         do a = 1,size(self%unit_cell_connection)
            ma = self%unit_cell_mol_for_atom(a)
            if (ma/=m) cycle
            if (NOT first) call tab_(stdout,int_fields=1)
            call put_(stdout,a)
            first = FALSE
            if (size(self%unit_cell_connection(a)%element)==0) then
               call flush_(stdout)
            else
               do i = 1,size(self%unit_cell_connection(a)%element)
                  if (i>1) call tab_(stdout,int_fields=2)
                  b = self%unit_cell_connection(a)%element(i)%element(4)
                  call put_(stdout,b)
                  call put_(stdout,self%unit_cell_connection(a)%element(i)%element(1))
                  call put_(stdout,self%unit_cell_connection(a)%element(i)%element(2))
                  call put_(stdout,self%unit_cell_connection(a)%element(i)%element(3))
                  call flush_(stdout)
               end do
            end if
         end do
      end do
      call dash_(stdout,int_fields=6)
   end subroutine

   subroutine put_uc_vdw_connection_table(self)
   ! Put the .unit_cell_connection table
      type(CRYSTAL_TYPE), IN :: self
      INT :: m,ma,a,i,b
      BIN :: first
   ENSURE(allocated(self%uc_vdw_connection),"CRYSTAL:put_uc_vdw_connection_table ... no uc_vdw_connection")
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:put_uc_vdw_connection_table ... no unit_cell_mol_for_atom")
      call flush_(stdout)
      call text_(stdout,"Cell VDW atom connection table:")
      call flush_(stdout)
      call show_(stdout,"n_unit_cell_mols =",self%n_unit_cell_mols)
      call flush_(stdout)
      call dash_(stdout,int_fields=6)
      call put_(stdout,"UnitCell",int_width=TRUE)
      call tab_(stdout,int_fields=1)
      call put_(stdout,"Joins",int_width=TRUE)
      call tab_(stdout,int_fields=1)
      call put_(stdout,"In Cell",int_width=TRUE)
      call flush_(stdout)
      call put_(stdout,"Mol.",int_width=TRUE)
      call put_(stdout,"Atom 1",int_width=TRUE)
      call put_(stdout,"Atom 2",int_width=TRUE)
      call put_(stdout,"h",int_width=TRUE)
      call put_(stdout,"k",int_width=TRUE)
      call put_(stdout,"l",int_width=TRUE)
      call flush_(stdout)
      call dash_(stdout,int_fields=6)
      do m = 1,self%n_unit_cell_mols
         call put_(stdout,m)
         first = TRUE
         do a = 1,size(self%uc_vdw_connection)
            ma = self%unit_cell_mol_for_atom(a)
            if (ma/=m) cycle
            if (NOT first) call tab_(stdout,int_fields=1)
            call put_(stdout,a)
            first = FALSE
            if (size(self%uc_vdw_connection(a)%element)==0) then
               call flush_(stdout)
            else
               do i = 1,size(self%uc_vdw_connection(a)%element)
                  if (i>1) call tab_(stdout,int_fields=2)
                  b = self%uc_vdw_connection(a)%element(i)%element(4)
                  call put_(stdout,b)
                  call put_(stdout,self%uc_vdw_connection(a)%element(i)%element(1))
                  call put_(stdout,self%uc_vdw_connection(a)%element(i)%element(2))
                  call put_(stdout,self%uc_vdw_connection(a)%element(i)%element(3))
                  call flush_(stdout)
               end do
            end if
         end do
      end do
      call dash_(stdout,int_fields=6)
   end subroutine


!  ================================
!  Put Fragment related information
!  ================================

   subroutine put_fragment_data(self)
   ! Put fragment information to file "out". Optional "atom" list may
   ! be used to enhace output.
      type(CRYSTAL_TYPE), IN :: self
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"================")
      call text_(stdout,"Crystal fragment")
      call text_(stdout,"================")
      call flush_(stdout)
      call text_(stdout,". Ab initio calculations are performed on the fragment")
      call text_(stdout,"  to obtain structure factors, charges, etc.")
      call flush_(stdout)
      call text_(stdout,". The fragment need not be an asymmetric unit.")
      call flush_(stdout)
      call text_(stdout,". Repeated atoms are assigned a 'repetition factor' which")
      call text_(stdout,"  is used to scale the structure factor contribution from")
      call text_(stdout,"  that atom. You can manually reset this.")
      call flush_(stdout)
      call text_(stdout,". Only the unique atoms in the fragment have their ")
      call text_(stdout,"  positions and ADPs refined.")
      call flush_(stdout)
      call text_(stdout,". The Z factor for the crystal is defined with respect to")
      call text_(stdout,"  the no. of electrons in the fragment")
      call flush_(stdout)
      call flush_(stdout)
      call show_(stdout,"No. of fragment atoms        =",self%n_fragment_atoms)
      call show_(stdout,"No. of unique fragment atoms =",self%n_unique_frag_atoms)
      call show_(stdout,"Z factor                     =",self%Z)
      if (partition_model_(self)/=" ") then
         call flush_(stdout)
         call show_(stdout,"Density partition model      =",partition_model_(self))
         call show_(stdout,"Is one-center TF model?      =",using_one_center_TF_model_(self))
      end if
      call put_fragment_geometry_(self)
      call put_stabilizer_symops_(self)
   end subroutine

   subroutine put_stabilizer_symops(self)
   ! Put out the stabilizer symop data
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
   ENSURE(allocated(self%stabilizer_symop),"CRYSTAL:put_stabilizer_symops ... no stabilizer_symop")
      call flush_(stdout)
      call text_(stdout,"=========================")
      call text_(stdout,"Crystal stabilizer symops")
      call text_(stdout,"=========================")
      call flush_(stdout)
      call text_(stdout,". The stablizer is the subgroup of symops which")
      call text_(stdout,"  map the fragment_geometry into itself.")
      call flush_(stdout)
      call show_(stdout,"n_stabilizer_symops =",self%n_stabilizer_symops)
      call flush_(stdout)

      ! Table
      call create_(table,1)
      call set_heading_(table(1),"Stabilizer")  ! typo found Kang ;)
      call set_subhead_(table(1),"symops")
      call set_values_(table(1),self%stabilizer_symop(:))
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine put_fragment_geometry(self)
   ! Put out the fragment_geometry information
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
   ENSURE(self%fragment_info_made,"CRYSTAL:put_fragment_geometry ... no fragment info")
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"QM fragment geometry:")
      call flush_(stdout)

      ! Table
      call create_(table,13)
      call set_heading_(table(1),"ID"); call set_values_(table(1),self%fragment_atom%tag)
      call set_column_spacing_(table(2),2)
      call set_heading_(table(2),"Rep"); call set_values_(table(2),int(self%repetition_factor))
      call set_subhead_(table(2),"fac")
      call set_sb3head_(table(2),"tor")
      call set_column_spacing_(table(3),2)
      call set_heading_(table(3),"Site"); call set_values_(table(3),self%frag_atom_sym_factor)
      call set_subhead_(table(3),"symm")
      call set_sb3head_(table(3),"fact")
      call set_column_spacing_(table(4),2)
      call set_heading_(table(4),"Uniq"); call set_values_(table(4),self%unique_index_for_frag_atom)
      call set_subhead_(table(4),"atom")
      call set_column_spacing_(table(5),3)
      call set_heading_(table(5),"Uniq"); call set_values_(table(5),self%unique_atom_for_frag_atom)
      call set_subhead_(table(5),"atom")
      call set_sb3head_(table(5),"ID")
      call set_heading_(table(7),"Coord")
      call set_subhead_(table(6),"- x -"); call set_values_(table(6),self%fragment_geometry(1,:))
      call set_subhead_(table(7),"- y -"); call set_values_(table(7),self%fragment_geometry(2,:))
      call set_subhead_(table(8),"- z -"); call set_values_(table(8),self%fragment_geometry(3,:))
      call set_sb3head_(table(7),"/frac")
      call set_heading_(table(9),"From"); call set_values_(table(9),self%asym_atom_for_frag_atom)
      call set_subhead_(table(9),"asym")
      call set_sb3head_(table(9),"atom")
      call set_column_spacing_(table(10),2)
      call set_heading_(table(10),"Using"); call set_values_(table(10),self%asym_symop_for_frag_atom)
      call set_subhead_(table(10),"symop")
      call set_column_spacing_(table(11),2)
      call set_real_precision_(table(11),1)
      call set_heading_(table(11),"     "); call set_values_(table(11),self%asym_shift_for_frag_atom(1,:))
      call set_subhead_(table(11),"- x -")
      call set_column_spacing_(table(12),2)
      call set_real_precision_(table(12),1)
      call set_heading_(table(12),"Shift"); call set_values_(table(12),self%asym_shift_for_frag_atom(2,:))
      call set_subhead_(table(12),"- y -")
      call set_column_spacing_(table(13),2)
      call set_real_precision_(table(13),1)
      call set_heading_(table(13),"     "); call set_values_(table(13),self%asym_shift_for_frag_atom(3,:))
      call set_subhead_(table(13),"- z -")
      call set_sb3head_(table(12),"/frac")
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine


!  ==================================
!  Put reflection related information
!  ==================================

   subroutine put_X_data_analysis(self,output,job)
   ! Put out analysis data for the reflections after a fit/refinement.
   ! "job" is passed on to .put_fitting_plots, which names the plot files
   ! after it; see there.
      type(CRYSTAL_TYPE), INOUT :: self
      BIN, optional, IN :: output
      STR(len=*), optional, IN :: job
      BIN :: show
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_X_data_analysis ... no diffraction data")
      show = TRUE
      if (present(output)) show = output
      if (NOT show) return
      call put_GOF_vs_STL_table_(self%xray_data)
      call put_GOF_vs_F_exp_table_(self%xray_data)
      call put_N_worst_reflections_(self%xray_data)
      call put_fitting_plots_(self,job)
   end subroutine

   subroutine put_reflection_data(self)
   ! Put out the reflection data to file "out".  Both Neutron and
   ! X-ray will be put if present.
      type(CRYSTAL_TYPE), IN :: self
      call put_X_reflection_data_(self)
   ! .put_N_reflection_data
   end subroutine

   subroutine put_X_reflection_data(self)
   ! Put out the X-ray reflection data.
      type(CRYSTAL_TYPE), IN :: self
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%reflections)) then
            call put_reflections_(self%xray_data)
         end if
      end if
      if (allocated(self%xray_r_free_data)) then
         if (allocated(self%xray_r_free_data%reflections)) then
            call put_reflections_(self%xray_r_free_data)
         end if
      end if
   end subroutine


!   put_N_reflection_data
!   ! Put out the PND reflection data
!      self :: IN
!      if (.pnd_data.allocated) then
!      if (.pnd_data.reflections.allocated) then
!         .pnd_data.PUT:put_reflections
!      end
!      end
!   end

   subroutine put_all_reflection_data(self)
   ! Put out all the X-ray reflection data.
      type(CRYSTAL_TYPE), IN :: self
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%reflections)) then
            call put_F_all_data_(self%xray_data%reflections)
         end if
      end if
      if (allocated(self%xray_r_free_data)) then
         if (allocated(self%xray_r_free_data%reflections)) then
            call put_F_all_data_(self%xray_r_free_data%reflections)
         end if
      end if
   end subroutine

   subroutine put_reflection_intensities(self)
   ! Put out the reflection data to file "out".  Both Neutron and
   ! X-ray will be put if present.
      type(CRYSTAL_TYPE), IN :: self
      call put_X_reflection_intensities_(self)
   end subroutine

   subroutine put_X_reflection_intensities(self)
   ! Put out the X-ray reflection data.
      type(CRYSTAL_TYPE), IN :: self
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%reflections)) then
            call put_reflection_intensities_(self%xray_data)
         end if
      end if
      if (allocated(self%xray_r_free_data)) then
         if (allocated(self%xray_r_free_data%reflections)) then
            call put_reflection_intensities_(self%xray_r_free_data)
         end if
      end if
   end subroutine

   subroutine put_reflection_intensities_XD(self)
   ! Put out the reflection data to file "out".  Both Neutron and
   ! X-ray will be put if present.
      type(CRYSTAL_TYPE), IN :: self
      call put_X_reflection_intensities_XD_(self)
   end subroutine

   subroutine put_X_reflection_intensities_XD(self)
   ! Put out the X-ray reflection data.
      type(CRYSTAL_TYPE), IN :: self
      if (allocated(self%xray_data)) then
         if (allocated(self%xray_data%reflections)) then
            call put_reflection_intensities_XD_(self%xray_data)
         end if
      end if
      if (allocated(self%xray_r_free_data)) then
         if (allocated(self%xray_r_free_data%reflections)) then
            call put_reflection_intensities_XD_(self%xray_r_free_data)
         end if
      end if
   end subroutine


!   put_PND_sf(name)
!   ! outputs the polarised neutron structure factors
!      self :: IN
!      name :: STR, IN
!
!   ENSURE(.xray_data.allocated, "no diffraction data")
!
!      .xray_data.PUT:put_PND_sf(name)
!
!   end

   subroutine put_correction_data(self)
   ! Output the correction data
      type(CRYSTAL_TYPE), IN :: self
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_correction_data ... no diffraction data")
      call put_correction_data_(self%xray_data)
      if (has_any_ADP3s_and_errors_(self%asymmetric_unit_atom)) then
         call flush_(stdout)
         call show_(stdout,"Using ADP3's?                  =",TRUE)
         if (has_any_ADP4s_and_errors_(self%asymmetric_unit_atom)) then
            call show_(stdout,"Using ADP4's?                  =",TRUE)
         end if
      end if
   end subroutine


!  ================
!  Crystal Explorer
!  ================

   subroutine put_CX(self,label,formula,spacegroup)
   ! Output some information for the Crystal Explorer program.
      type(CRYSTAL_TYPE), INOUT :: self
      STR(len=*), IN :: label,formula,spacegroup
      call put_CX_(self%unit_cell,label,formula,spacegroup)  ! INOUT - change
   end subroutine


!  ==================
!  Put plots and maps
!  ==================

   subroutine put_fitting_plots(self,job)
   ! "job" names the job, and heads every plot file written below. MOLECULE
   ! passes its own .name, which is what "name=" in a job file sets and what
   ! `hart --job` sets. Without it every plot was called stdout.*, so two jobs
   ! run in one directory overwrote each other's pictures.
   !
   ! Optional, and it falls back to "stdout", because two call sites are inside
   ! CRYSTAL itself (the "put_data_analysis" keyword and .put_X_data_analysis
   ! at the end of a fit) where there is no MOLECULE to ask.
   !
   ! Output the fit analysis: several plots, each as four files headed "job".
      type(CRYSTAL_TYPE), INOUT :: self
      STR(len=*), IN, optional :: job
      STR(len=STR_SIZE) :: head
   ENSURE(allocated(self%xray_data),"CRYSTAL:put_fitting_plots ... no reflection data")

      ! MOLECULE.name defaults to "unknown", which would make a worse file name
      ! than the one every existing job already produces.
      head = "stdout"
      if (present(job)) then
         if (job/=" " AND job/="unknown") head = job
      end if

      ! Fit analysis
      call flush_(stdout)
      call text_(stdout,"==================================")
      call text_(stdout,"Analysis of the Hirshfeld atom fit")
      call text_(stdout,"==================================")
      call flush_(stdout)
      call text_(stdout,"The plots are:")
      call text_(stdout,". "//trim(head)//".QQ_plot")
      call text_(stdout,"     a normal QQ plot, each datum labelled by (h k l)")
      call text_(stdout,". "//trim(head)//".F_z_vs_stl")
      call text_(stdout,"     F_z = (F_exp-F_pred)/F_sigma vs sin(theta)/lambda")
      call text_(stdout,". "//trim(head)//".F_z_vs_F_exp")
      call text_(stdout,"     F_z = (F_exp-F_pred)/F_sigma vs F_exp")
      call text_(stdout,". "//trim(head)//".Delta_F_vs_stl")
      call text_(stdout,"     Delta F = (F_exp-F_pred) vs sin(theta)/lambda")
      call flush_(stdout)
      call text_(stdout,"If you used an extinction correction, you also get:")
      call text_(stdout,". "//trim(head)//".Delta_F_pred_z_vs_stl")
      call text_(stdout,"     (F_pred(no ext)-F_pred(ext))/F_sigma vs sin(theta)/lambda")
      call text_(stdout,". "//trim(head)//".Delta_F_pred_z_vs_F_pred")
      call text_(stdout,"     (F_pred(no ext)-F_pred(ext))/F_sigma vs F_pred")
      call flush_(stdout)
      call text_(stdout,"Each of those is four files: the data itself, then")
      call text_(stdout,". <plot>.labels    where the (h k l) labels go, and their leaders")
      call text_(stdout,". <plot>.gnuplot   the gnuplot script that draws it")
      call text_(stdout,". <plot>.png       the picture, if gnuplot is installed")
      call text_(stdout,"The QQ plot's data file keeps its older name, "//trim(head)//".QQ_plot_with_hkl.")
      call flush_(stdout)
      call text_(stdout,"All of the above are repeated for the reserved (free) data,")
      call text_(stdout,"headed '"//trim(head)//".free', if you used that option. Phew.")
      call flush_(stdout)
      call text_(stdout,"The pictures are drawn for you if gnuplot is on your PATH; install")
      call text_(stdout,"it if a .png is missing. To draw one yourself, run the script --")
      call text_(stdout,"unchanged, it draws to your screen:")
      call text_(stdout,"   gnuplot -persist '"//trim(head)//".QQ_plot.gnuplot'")

      ! Data analysis
      if (allocated(self%xray_data)) then

      !  stdout.flush
      !  stdout.text("====================================")
      !  stdout.text("Plots based on data used for fitting")
      !  stdout.text("====================================")

      !  .put_GOF_vs_STL_plot(.xray_data)
      !  .put_GOF_vs_F_exp_plot(.xray_data)
      !  .put_ratio_vs_STL_plot(.xray_data)
      !  .put_ratio_vs_F_exp_plot(.xray_data)
      !  .put_qq_plot(.xray_data)
         call put_labelled_qq_plot_(self%xray_data,head)
         call put_F_calc_plots_(self%xray_data,head)
      end if

      ! Reserved data analysis
      if (allocated(self%xray_r_free_data)) then

      !  stdout.flush
      !  stdout.text("===================================")
      !  stdout.text("Plots based on free (reserved) data")
      !  stdout.text("===================================")

      !  .put_GOF_vs_STL_plot(.xray_data)
      !  .put_GOF_vs_F_exp_plot(.xray_data)
      !  .put_ratio_vs_STL_plot(.xray_data)
      !  .put_ratio_vs_F_exp_plot(.xray_data)
      !  .put_qq_plot(.xray_data)
         call put_labelled_qq_plot_(self%xray_r_free_data,trim(head)//".free")
         call put_F_calc_plots_(self%xray_r_free_data,trim(head)//".free")
      end if
   end subroutine


!   put_GOF_vs_STL_plot(data)
!   ! Output the GOF vs. sin(theta)/angle bins
!      self :: INOUT
!      data :: DIFFRACTION_DATA
!      data.PUT:put_GOF_vs_STL_plot
!   end

!   put_GOF_vs_F_exp_plot(data)
!   ! Output the GOF vs. F_pred bins
!      self :: INOUT
!      data :: DIFFRACTION_DATA
!      data.PUT:put_GOF_vs_F_exp_plot
!   end

!   put_ratio_vs_STL_plot(data)
!   ! Output the mean ratio F_obs/F_pred vs. sin(theta)/angle bins
!      self :: INOUT
!      data :: DIFFRACTION_DATA
!      data.put_ratio_vs_STL_plot
!   end

!   put_ratio_vs_F_exp_plot(data)
!   ! Output the GOF vs. F_pred bins
!      self :: INOUT
!      data :: DIFFRACTION_DATA
!      data.put_ratio_vs_F_exp_plot
!   end

!   put_labelled_qq_plot(data,tag)
!   ! Output a qq plot to the text file.
!   ! It is a plot of the experimental quantile vs expected quantile.
!      self :: INOUT
!      data :: DIFFRACTION_DATA
!      tag :: STR, optional, IN
!      data.put_labelled_qq_plot(tag)
!   end

!   put_fcalc_plots(data,tag)
!   ! Output some different plots about the calculated structure factors.
!      self :: INOUT
!      data :: DIFFRACTION_DATA
!      tag :: STR, optional, IN
!      data.put_fcalc_plots(tag)
!   end

!  ====================
!  Residual density map
!  ====================

!   make_inverse_FT_F_calc_ED_grid(map,pts) :: PURE
!   ! Make the electron density "map" values using an inverse Fourier
!   ! synthesis F_calc values, removing corrections for dispersion,
!   ! corrections for extinction. Friedel pairs are accounted for.
!   ! The supplied points "pts" are in a cartesian system (n_pts x 3)
!      self :: IN
!      map  :: VEC{REAL}, OUT
!      pts  :: MAT{REAL}, IN
!
!   ENSURE(map.dim ==pts.dim1,"wrong dim: map")
!   ENSURE(pts.dim2==3,       "wrong dim2: pts")
!   ENSURE(.xray_data.allocated, "no diffraction data")
!   ENSURE(.xray_data.reflections.allocated, "no structure factors")
!   ENSURE(.xray_data.INQ:have_F_calc, "no calculated structure factors")
!   ENSURE(.xray_data.INQ:have_F_pred, "no predicted structure factors")
!   ENSURE(.xray_data.INQ:have_F_exp, "no experimental structure factors")
!
!      frac  :: MAT{REAL}@
!      Fc,Fe :: VEC{CPX}@
!      gx,gy,gz :: VEC{INT}@
!      n_pts,n_refl, n,p :: INT
!      xi,yi,zi,den :: REAL
!      ci,val :: CPX
!
!      ! Constants
!      n_pts  = pts.dim1
!      n_refl = .xray_data.reflections.dim*.spacegroup.n_seitz
!      ci     = -2*PI * IMAGIFY(ONE)
!
!      ! Get phased structure factor differences, absolute scale
!      ! And the symmetry generated Miller indices, the g's.
!      gx.create(n_refl)
!      gy.create(n_refl)
!      gz.create(n_refl)
!      Fc.create(n_refl)
!      Fe.create(n_refl)
!      .xray_data.make_symmetry_equivalents(Fc,Fe,gx,gy,gz,.spacegroup)
!      Fe.destroy
!
!      ! Volume factor
!      Fc  = Fc/.unit_cell.volume
!
!      ! Get the fractional coordinates. Hard coded !
!      frac = matmul(pts,.unit_cell.reciprocal_mx)
!
!      ! Make the phases x del F
!      do p = 1,n_pts
!
!         xi = frac(p,1)
!         yi = frac(p,2)
!         zi = frac(p,3)
!
!         ! Assign value
!         den = ZERO
!         do n = 1,n_refl
!            val = ci*(gx(n)*xi + gy(n)*yi + gz(n)*zi)
!            den = den + REALIFY(exp(val)*Fc(n)) ! <--
!         end
!         map(p) = den
!
!      end
!
!      ! Clean
!      gz.destroy
!      gy.destroy
!      gx.destroy
!      Fc.destroy
!
!   end

   PURE subroutine set_Fourier_multiplicities(self,mult)
   ! Set the multiplicity each reflection carries in the residual-density
   ! Fourier synthesis: 2 when the conjugate half of reciprocal space is
   ! absent from the data, 1 when another reflection already supplies it.
   !
   ! The conjugate of h is supplied by any reflection whose symmetry
   ! equivalents include -h. That is the BIJVOET mate, which need not be the
   ! Friedel mate: in P2(1)2(1)2(1), (h,k,l) and (h,k,-l) are neither
   ! symmetry equivalent nor Friedel mates, yet each generates the other's
   ! conjugate half. Testing only for -h itself doubles every such pair.
   !
   ! A reflection mapped onto -h by one of its own symops is NOT set to 1
   ! here: make_symop_generated_coefficients already divides that case by its site
   ! symmetry factor.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), OUT :: mult
      VEC(INT,:), allocatable :: key,sorted_key
      MAT(REAL,3,3) :: R
      VEC(INT,3) :: h,g
      INT :: i,s,n_refl,n_seitz
      BIN :: found
   ENSURE(allocated(self%xray_data),"CRYSTAL:set_Fourier_multiplicities ... no diffraction data")
   ENSURE(allocated(self%xray_data%reflections),"CRYSTAL:set_Fourier_multiplicities ... no structure factors")
   ENSURE(size(mult)==size(self%xray_data%reflections),"CRYSTAL:set_Fourier_multiplicities ... mult: wrong dim")
      n_refl = size(self%xray_data%reflections)
      n_seitz = self%spacegroup%n_seitz
      mult = 2

      ! Key each reflection by one integer so the lookup below is a bisection.
      ! The naive test is O(n_refl^2 . n_seitz), which is minutes on a large
      ! data set.
      call create_(key,n_refl)
      do i = 1,n_refl
         key(i) = Miller_index_key_(self,Miller_indices_(self%xray_data%reflections(i)))
      end do
      call create_(sorted_key,n_refl)
      sorted_key = key
      call sort_(sorted_key)
      do i = 1,n_refl
         h = Miller_indices_(self%xray_data%reflections(i))

         ! Is some symmetry equivalent of -h in the data?
         do s = 1,n_seitz
            R = transpose(self%spacegroup%seitz(1:3,1:3,s))
            g = -nint(matmul(R,h))

            ! Skip -h generated from h by a symop of its own: that is the site
            ! symmetry factor's business, not this one's.
            if (all(g==h)) cycle
            found = sorted_key_has_(self,sorted_key,Miller_index_key_(self,g))
            if (found) then
               mult(i) = 1
               exit
            end if
         end do
      end do
      call destroy_(sorted_key)
      call destroy_(key)
   end subroutine

   PURE function sorted_key_has(self,key,val) result (res)
   ! Return TRUE if the increasing-order list "key" contains "val".
   ! Bisection: "key" must be sorted.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), IN :: key
      INT, IN :: val
      BIN :: res
      INT :: lo,hi,mid
      res = FALSE
      lo = 1
      hi = size(key)
      do
         if (lo>hi) exit
         mid = (lo+hi)/2
         if (key(mid)==val) then
            res = TRUE
            exit
         else if (key(mid)<val) then
            lo = mid+1
         else
            hi = mid-1
         end if
      end do
   end function

   PURE function Miller_index_key(self,h) result (res)
   ! Pack Miller indices into a single integer so a set of reflections can be
   ! searched by bisection. Base 1024 keeps the product inside a 32-bit INT.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,3), IN :: h
      INT :: res
   ENSURE(maxval(abs(h))<512,"CRYSTAL:Miller_index_key ... Miller index too large to pack")
      res = ((h(1)+512)*1024+(h(2)+512))*1024+(h(3)+512)
   end function

   PURE subroutine make_cell_map(self,map)
   ! Fill "map" with the Fourier coefficients of the density of the unit
   ! cell of its kind: "residual", the difference density F_exp - F_calc
   ! with the model's phases; "f_exp", the observed density; "f_calc", the
   ! model's. The coefficients are on absolute scale and expanded over the
   ! whole sphere of reciprocal space with the Friedel multiplicities.
   ! For the two total densities the term F(000), the number of electrons
   ! in the cell, is added, so that those maps integrate to the electron
   ! count; the cell is taken as neutral, F(000) being the sum of the
   ! atomic numbers of the unit cell atoms times their site occupancies.
   ! The kinds "deformation_calc" and "deformation_exp" subtract the
   ! promolecule, the spherical atoms at the model's positions and ADPs,
   ! whose structure factors the map must hold (set_promolecule_F); their
   ! F(000) cancels for a neutral cell and is not added.
      type(CRYSTAL_TYPE), IN :: self
      type(CELL_MAP_TYPE), INOUT :: map
      VEC(INT,:), allocatable :: g1,g2,g3,mult
      VEC(CPX,:), allocatable :: f,coeff
      INT :: n_refl,n_sym,n_all,a
      REAL :: f000
      BIN :: total
   ENSURE(allocated(self%xray_data),"CRYSTAL:make_cell_map ... no diffraction data")
   ENSURE(allocated(self%xray_data%reflections),"CRYSTAL:make_cell_map ... no structure factors")
   ENSURE(have_F_calc_(self%xray_data),"CRYSTAL:make_cell_map ... no calculated structure factors")
   ENSURE(have_F_pred_(self%xray_data),"CRYSTAL:make_cell_map ... no predicted structure factors")
   ENSURE(have_F_exp_(self%xray_data),"CRYSTAL:make_cell_map ... no experimental structure factors")
   ENSURE(kind_is_known_(map),"CRYSTAL:make_cell_map ... unknown kind of cell map: "//trim(map%kind))
   ENSURE(allocated(map%F_pro) OR NOT is_deformation_kind_(map),"CRYSTAL:make_cell_map ... a deformation kind needs the promolecule structure factors")
      n_refl = size(self%xray_data%reflections)
      n_sym = n_refl*self%spacegroup%n_seitz
      total = map%kind=="f_exp" OR map%kind=="f_calc"
      n_all = n_sym
      if (total) n_all = n_all+1

      ! Multiplicity factor for the conjugate half of reciprocal space.
      ! For a centrosymmetric group every reflection's own symops generate -h,
      ! so the site symmetry factor handles it and 2 is right throughout.
      call create_(mult,n_refl)
      if (self%spacegroup%centrosymmetric) then
         mult = 2
      else
         call set_Fourier_multiplicities_(self,mult)
      end if

      ! The coefficient of each unique reflection, then of every symmetry
      ! generated one
      call create_(f,n_refl)
      if (is_deformation_kind_(map)) then
         ENSURE(size(map%F_pro)==n_refl,"CRYSTAL:make_cell_map ... F_pro: wrong dim")
         call make_Fourier_coefficients_(self%xray_data,f,map%kind,map%F_pro,wiener=map%wiener_weight)
      else
         call make_Fourier_coefficients_(self%xray_data,f,map%kind,wiener=map%wiener_weight)
      end if
      call create_(coeff,n_all)
      call create_(g1,n_all)
      call create_(g2,n_all)
      call create_(g3,n_all)
      call make_symop_generated_coefficients_(self%xray_data,coeff(1:n_sym),g1(1:n_sym),g2(1:n_sym),g3(1:n_sym),self%spacegroup,mult,f)

      ! F(000) for a total density
      if (total) then
         ENSURE(allocated(self%unit_cell_atom),"CRYSTAL:make_cell_map ... no unit cell atoms")
         f000 = ZERO
         do a = 1,self%n_unit_cell_atoms
            f000 = f000+self%unit_cell_atom(a)%atomic_number*self%unit_cell_atom(a)%site_occupancy
         end do
         g1(n_all) = 0
         g2(n_all) = 0
         g3(n_all) = 0
         coeff(n_all) = f000
      end if
      call set_cell_(map,self%unit_cell)
      call set_coefficients_(map,g1,g2,g3,coeff)
      call destroy_(g3)
      call destroy_(g2)
      call destroy_(g1)
      call destroy_(coeff)
      call destroy_(f)
      call destroy_(mult)
   end subroutine

   PURE subroutine make_residual_density_grid(self,grid,pts)
   ! Make the residual density "grid" at the cartesian points "pts", one
   ! row per point. For a grid of points over the cell
   ! make_residual_density_cell is faster.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
      type(CELL_MAP_TYPE), allocatable :: map
   ENSURE(size(grid)==size(pts,1),"CRYSTAL:make_residual_density_grid ... wrong dim, grid")
   ENSURE(size(pts,2)==3,"CRYSTAL:make_residual_density_grid ... wrong dim2, pts")
      call create_(map)
      call set_kind_(map,"residual")
      call make_cell_map_(self,map)
      call make_values_at_(map,grid,pts)
      call destroy_(map)
   end subroutine

   PURE subroutine make_residual_density_cell(self,map,nx,ny,nz)
   ! Make the residual density "map" on a grid of "nx", "ny", "nz" points
   ! along the cell axes, both ends included, incrementing first on x,
   ! then y, then z, as PLOT_GRID:make_points does.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,:), OUT :: map
      INT, IN :: nx,ny,nz
      type(CELL_MAP_TYPE), allocatable :: cell_map
      call create_(cell_map)
      call set_kind_(cell_map,"residual")
      call make_cell_map_(self,cell_map)
      call make_values_on_cell_(cell_map,map,nx,ny,nz)
      call destroy_(cell_map)
   end subroutine


!  ===========================
!  Cluster generation routines
!  ===========================

   subroutine make_asym_unit_HS_cluster(self,HS_atoms,tol)
   ! Generating a "HS_atoms" cluster from information useful for
   ! calculating the HS of the asymmetric unit, to an accuracy "tol".
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: HS_atoms
      REAL, IN :: tol
      call set_generation_method_(self,"for_asymm_unit_hs")
      call set_atom_density_cutoff_(self,tol)
      call set_defragment_(self,FALSE)
      call make_cluster_info_(self)
      call create_atom_list_(self,HS_atoms)
   end subroutine


!  ===========================
!  Cluster generation routines
!  ===========================

   subroutine make_cluster_info(self)
   ! Make all the cluster information from an arbitrary .fragment_geometry.
   ! NOTE: .set_defaults must be called before this.
      type(CRYSTAL_TYPE), INOUT :: self
   ENSURE(allocated(self%unit_cell_connection),"CRYSTAL:make_cluster_info ... no unit cell connection table")
   ENSURE(allocated(self%fragment_geometry),"CRYSTAL:make_cluster_info ... no fragment_geometry")
      if (self%cluster_info_made) return

      ! Initial cluster in .fragment_geometry 
      ! for .generation_method
      call make_occupation_list_(self)

      ! Make the cluster from the fragment
      select case (self%generation_method)

      case ("within_radius")
         call make_within_radius_(self)

      case ("for_asymm_unit_hs","for_hirshfeld_surface")
         call make_HS_radius_(self)  ! NOT PURE
         call make_within_radius_(self)
         self%defragment = FALSE

      case ("for_unit_cell_density")
      ! .do_defragment
         call make_HS_radius_(self)  ! NOT PURE
         call make_within_radius_(self)
         self%defragment = TRUE

      case ("offset_unit_cell_density")
         call make_HS_radius_(self)  ! NOT PURE
         call make_within_radius_(self)
         self%defragment = FALSE
      end select

      ! Defragment/find molecules in the cluster
      if (self%defragment) then; call do_defragment_(self)
      else; call do_connectify_(self)
      end if

      ! Extract cordinates from occupation list
      if (self%reorder_in_mols) then

         ! In molecule order
         call extract_geometry_in_mols_(self,self%occupation_list,self%geometry)
      else

         ! The fragment atoms come first ...
         call extract_geometry_no_mols_(self,self%occupation_list,self%geometry)
      end if

      ! Finished ...
      self%cluster_info_made = TRUE
   end subroutine

   PURE subroutine make_occupation_list(self)
   ! Codify which atoms in the crystal are fragment atoms i.e. make
   ! the ".occupation_list". If atom "u" in the unit cell "(h1,h2,h3)"
   ! is occupied then the macro CLUSTER_AT_CODE(h1,h2,h3,u) appears in
   ! the array "list". The maximum value allowed for any h is
   ! CLUSTER_H_MAX i.e. we can't consider fragments atoms which lie in
   ! cells outside this range. This list allows easy comparison of
   ! which atoms are in the fragment by an integer equality test.
   ! NOTE: Also make the ".molecule_for_atom" list which tells which
   ! molecule the atom belongs to.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(INT,3) :: h
      VEC(REAL,3) :: pos
      INT :: f,u,h000
      MAT(REAL,:,:), allocatable :: fragment_geometry
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:make_occupation_list ... no crystal unit cell")
   ENSURE(allocated(self%fragment_geometry),"CRYSTAL:make_occupation_list ... no fragment_geometry")
      select case (self%generation_method)

      case ("for_asymm_unit_hs")
         ENSURE(self%fragment_info_made,"CRYSTAL:make_occupation_list ... no crystal fragment info")

         ! Set the asymmetric unit as the starting fragment
         self%fragment_geometry = self%asymmetric_unit_geometry
         self%n_fragment_atoms = self%n_asymmetric_unit_atoms

         ! Get fragment offset i.e. center
         pos = mean_column_vector_(self%fragment_geometry)
         self%fragment_offset = int(pos*(ONE-TOL(8)))  ! shrink toward zero: FP-robust at cell boundary

         ! Create occupation list
         call destroy_(self%occupation_list)
         call create_(self%occupation_list,self%n_fragment_atoms)

         ! Loop on fragment atom
         do f = 1,self%n_fragment_atoms

            ! Get unit cell atom & shift
            u = self%unit_cell_atom_for_frag_atom(f)
            h = self%unit_cell_shft_for_frag_atom(:,f)

            ! Shift relative to recentered unit cell
            h = h+self%fragment_offset

            ! Offset h to apply to "u" to get frag atom "f".
            ! Must reverse h. See above.
            h = -h

            ! Set occupation code ...
            self%occupation_list(f) = CLUSTER_AT_CODE(h(1),h(2),h(3),u)
         end do

      case ("within_radius","for_hirshfeld_surface","fragment")
         ENSURE(allocated(self%fragment_geometry),"CRYSTAL:make_occupation_list ... no fragment_geometry")
         ENSURE(self%fragment_info_made,"CRYSTAL:make_occupation_list ... no crystal fragment info")
         ENSURE(allocated(self%unit_cell_atom_for_frag_atom),"CRYSTAL:make_occupation_list ... no cell-frag_atom array")
         ENSURE(allocated(self%unit_cell_shft_for_frag_atom),"CRYSTAL:make_occupation_list ... no cell-frag_atom array")

         ! Get fragment offset i.e. center
         pos = mean_column_vector_(self%fragment_geometry)
         self%fragment_offset = int(pos*(ONE-TOL(8)))  ! shrink toward zero: FP-robust at cell boundary

         ! Create occupation list
         call destroy_(self%occupation_list)
         call create_(self%occupation_list,self%n_fragment_atoms)

         ! Loop on fragment atom
         do f = 1,self%n_fragment_atoms

            ! Get unit cell atom & shift
            u = self%unit_cell_atom_for_frag_atom(f)
            h = self%unit_cell_shft_for_frag_atom(:,f)

            ! Shift relative to recentered unit cell
            h = h+self%fragment_offset

            ! Check if we can store these h's in a bit string
            ENSURE(all(h>=-CLUSTER_H_MAX),"CRYSTAL:make_occupation_list ... frag too wide, h = "//trim(to_concatenated_str_(h)))
            ENSURE(all(h<=CLUSTER_H_MAX),"CRYSTAL:make_occupation_list ... frag too wide, h = "//trim(to_concatenated_str_(h)))

            ! Sanity check
            ENSURE(u/=0,"CRYSTAL:make_occupation_list ... position of fragment atom "//trim(to_str_(f))//" not found in unit cell")
         ! if (u==0) then
         !    WARN("position of fragment atom "//f.to_str.trim//" not found in unit cell")
         !    stdout.show("fragment pos =",pos)
         !    stdout.text("unit cell geometry:")
         !    stdout.put(transpose(.unit_cell_geometry))
         !    DIE("position of fragment atom "//f.to_str.trim//" not found in unit cell")
         ! end

            ! Offset h to apply to "u" to get frag atom "f".
            ! Must reverse h. See above.
            h = -h

            ! Get occ code ...
            self%occupation_list(f) = CLUSTER_AT_CODE(h(1),h(2),h(3),u)
         end do
         self%molecule_for_atom = self%fragment_atom%group  ! molecule_for_atom follows fragment_atom.group. 

      case ("unit_cell","for_unit_cell_density")

         ! Create occupation list
         call destroy_(self%occupation_list)
         call create_(self%occupation_list,self%n_unit_cell_atoms)

         ! Temporary
         call create_(fragment_geometry,3,self%n_unit_cell_atoms)

         ! Set central cell
         h000 = CLUSTER_H_CODE(0,0,0)
         do u = 1,self%n_unit_cell_atoms

            ! Encode central cell
            self%occupation_list(u) = h000+CLUSTER_UA_CODE(u)

            ! Set fragment to unit cell
            pos = self%unit_cell_geometry(:,u)
            fragment_geometry(:,u) = pos
         end do

         ! Set fragment and make sure there is no offset
         call set_fragment_geometry_(self,fragment_geometry)
         self%fragment_offset = 0

         ! Clean
         call destroy_(fragment_geometry)
         self%molecule_for_atom = self%unit_cell_mol_for_atom  ! Update fragment atom's group index from .unit_cell_atom
      end select
      ! update (crystal).n_atoms
      self%n_atoms = size(self%occupation_list)  ! (crystal).n_atoms /==Cluster-related==/ The number of atoms in the cluster
   end subroutine

   PURE subroutine make_plot_info(self,pt,parent,symop,shifts)
   ! Make ".occupation_list" i.e. the CLUSTER_AT_CODE(h1,h2,h3,u)'s
   ! for the list of atoms in the crystal within a distance ".radius"
   ! of the set of points in "pt" (usually generated from "plot_grid"
   ! via the "make_points(pt)" routine, but not necessarily.
   ! NOTE: Also make the ".molecule_for_atom" list which tells which
   ! molecule the atom belongs to.
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: pt
      VEC(INT,:), allocatable, OUT :: parent
      VEC(INT,:), allocatable, OUT :: symop
      MAT(INT,:,:), allocatable, OUT :: shifts
      MAT(REAL,3,3) :: dcm
      MAT(REAL,:,:), allocatable :: ucc
      VEC(INT,3) :: h
      VEC(INT,:), allocatable :: uc_mol
      VEC(REAL,3) :: pos,shift
      VEC(REAL,:), allocatable :: vdw_radius
      REAL :: vdw,d2
      INT :: p,i,j,k,u,code,s,f
      BIN :: found
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:make_plot_info ... no crystal unit cell")
   ENSURE(allocated(self%fragment_geometry),"CRYSTAL:make_plot_info ... no fragment_geometry")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_plot_info ... no crystal fragment info")
   ENSURE(allocated(self%unit_cell_atom_for_frag_atom),"CRYSTAL:make_plot_info ... no cell-frag_atom array")
   ENSURE(allocated(self%unit_cell_shft_for_frag_atom),"CRYSTAL:make_plot_info ... no cell-frag_atom array")

      ! Create occupation list
      call destroy_(self%occupation_list)
      call create_(self%occupation_list,0)

      ! Set up the van der Waals radii, for the limits of
      ! interaction.
      call create_(vdw_radius,self%n_unit_cell_atoms)
      call get_vdw_radii_ccdc_(self%unit_cell_atom,vdw_radius)

      ! Have a cartesian unit cell, and add the unit cell length to
      ! each direction before each iteration.
      dcm = self%unit_cell%direct_mx
      ucc = matmul(dcm,self%unit_cell_geometry)  ! This is (3 x N)

      ! Loop over 27 unit cells
      do k = -1,1
         do j = -1,1
            do i = -1,1
               h = [i,j,k]

         ! Create the unit cell molecule list for this shift
               call create_(uc_mol,0)
               uc_mol = ZERO

         ! Shift the unit cell geometry to a different unit cell
         ! This should shift the atomic cartesian coordinates to
         ! the relevant unit cell
               shift = matmul(dcm,h)
               do u = 1,size(ucc,2)
                  ucc(:,u) = ucc(:,u)+shift
               end do

         ! Cycle through every coordinate in the cartesian unit
         ! cell
               do u = 1,size(ucc,2)
                  f = self%frag_parent_for_cell_atom(u)

            ! Check to see if that point has already been found
                  found = FALSE
                  if (any(self%unit_cell_mol_for_atom(u)==uc_mol)) then
                     found = TRUE
                     exit
                  end if
                  if (found) cycle
                  vdw = vdw_radius(u)
                  pos = ucc(:,u)
                  do p = 1,size(pt,1)

               ! Cycle if it is outside the range of the point
                     if (abs(pos(1)-pt(p,1))>vdw) cycle
                     if (abs(pos(2)-pt(p,2))>vdw) cycle
                     if (abs(pos(3)-pt(p,3))>vdw) cycle

               ! Final check - actual distance
                     d2 = (pos(3)-pt(p,3))**2+(pos(2)-pt(p,2))**2+(pos(1)-pt(p,1))**2
                     if (d2>vdw*vdw) cycle

               ! If you reach this point, your point should be added
               ! to the occupation list. Must first convert back to
               ! bitwise h code.  h(1) = i, h(2) = j and h(3) = k.
               ! NOTE: u should be the atom code, but it is possible
               ! this will be incorrect

               ! Get occ code ...
                     code = CLUSTER_AT_CODE(i,j,k,u)
                     call append_(self%occupation_list,code)

               ! If it is in the occupation list, then it is done, and
               ! the next atom can be checked
                     found = TRUE
                     exit
                  end do
                  if (found) then
                     f = self%frag_parent_for_cell_atom(u)
                     s = self%asym_symop_for_unit_cell_atom(u)
               ! Symop type info here? Somehow?
                     call append_(parent,f)
                     call append_(symop,s)
                     call append_column_(shifts,h)
                     call append_(uc_mol,self%unit_cell_mol_for_atom(u))
                  end if
               end do

         ! Undoes the unit cell geometry to a different unit cell
         ! This should shift the atomic cartesian coordinates back
         ! to the initial unit cell coordinates
               do u = 1,size(ucc,2)
                  ucc(:,u) = ucc(:,u)-shift
               end do

         ! Destroy the unit cell molecules, so a fresh list is
         ! available for the next cell
               call destroy_(uc_mol)
            end do
         end do
      end do
      call destroy_(ucc)
      call destroy_(vdw_radius)
   end subroutine

   subroutine make_HS_radius(self)
   ! This routine makes an appropriate value for the cluster .radius
   ! in the case where the .generation_method is "for_hirshfeld_surface". 
   ! The radius is made so that the accuracy of any atomic term
   ! neglected is smaller than .atom_density_cutoff. The radius is
   ! defined to the nearest 0.02 bohr (or whatever is set as the
   ! default for the interpolator table spacing). Likewise, we can't
   ! cope with atom density cutoffs where the distances are larger
   ! than 30 bohr (or whatever is set as the default interpolator
   ! table length). To do this properly we'd have to solve numerical
   ! equations for exactly where the atom density equals the desired
   ! cutoff; that seems like too much effort.
      type(CRYSTAL_TYPE), INOUT :: self
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:make_HS_radius ... no asymmetric cell atoms")
   ENSURE(has_all_bases_(self%asymmetric_unit_atom),"CRYSTAL:make_HS_radius ... incomplete atom bases")
   ENSURE(self%atom_density_cutoff>ZERO,"CRYSTAL:make_HS_radius ... atom density cutoff must be positve")
      self%radius = max_interpolator_table_length_(self%asymmetric_unit_atom,self%atom_density_cutoff)
   end subroutine

   PURE subroutine make_within_radius(self)
   ! Make the maximum unit cell offsets so that we get all atoms within a
   ! certain radius of the fragment geometry.
      type(CRYSTAL_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: geom0,frag
      MAT(REAL,3,3) :: rcm
      VEC(REAL,3) :: pos,off,d,frac_radius
      VEC(INT,:), allocatable :: hlist,ulist,match
      VEC(BIN,:), allocatable :: uskip
      REAL :: d2,radius2
      BIN :: within_radius,has_frag_atoms,offset
      BIN :: has_atoms_to_suppress
      INT :: n_unit_cell_atoms,k,u
      INT :: h0,h1,h2,h3,hcode,code
   ENSURE(allocated(self%occupation_list),"CRYSTAL:make_within_radius ... no occupation_list")
   ENSURE(allocated(self%fragment_geometry),"CRYSTAL:make_within_radius ... no fragment_geometry")
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:make_within_radius ... no crystal unit_cell_geometry")

      ! Constants
      n_unit_cell_atoms = self%n_unit_cell_atoms
      radius2 = self%radius*self%radius
      rcm = self%unit_cell%direct_mx

      ! Suppres some atoms?
      has_atoms_to_suppress = allocated(self%unit_cell_atoms_to_suppress)
      if (has_atoms_to_suppress) has_atoms_to_suppress = size(self%unit_cell_atoms_to_suppress)>0

      ! Cartesian fragment geometry
      call create_(frag,3,self%n_fragment_atoms)
      call to_product_of_(frag,rcm,self%fragment_geometry)

      ! Calculate offset to fragment
      offset = any(self%fragment_offset/=0)
      if (offset) then
         call to_product_of_(off,rcm,REALIFY(self%fragment_offset))
      end if

      ! Look this many cells away for cluster atoms ...
      frac_radius = self%radius/self%unit_cell%length
      self%h_max = ceiling(frac_radius+maxval(self%fragment_geometry,dim=2)-self%fragment_offset)
      self%h_min = floor(-frac_radius+minval(self%fragment_geometry,dim=2)-self%fragment_offset)

      ! Look at least one cell away ...
      self%h_max = max(self%h_max,[1,1,1])
      self%h_min = min(self%h_min,[-1,-1,-1])

      ! Maximum value of unit cell index
      ! and offset for unit cell coding
      h0 = CLUSTER_H_MAX
      ENSURE(all(self%h_min>=-h0),"CRYSTAL:make_within_radius ... search too low,  h_min = "//trim(to_concatenated_str_(self%h_min)))
      ENSURE(all(self%h_max<=h0),"CRYSTAL:make_within_radius ... search too high, h_max = "//trim(to_concatenated_str_(self%h_max)))

      ! Get the (0,0,0) unit cell offset by .fragment_offset in cartesians
      call create_(geom0,3,n_unit_cell_atoms)
      call to_product_of_(geom0,rcm,self%unit_cell_geometry)
      if (offset) geom0 = geom0+spread(off,2,n_unit_cell_atoms)

      ! Cells which fragment occupies
      call create_(match,self%n_fragment_atoms)
      call create_(hlist,self%n_fragment_atoms)
      hlist = CLUSTER_HC_FROM_CODE(self%occupation_list)

      ! Unit cell atoms which correspond to the fragment atoms
      call create_(ulist,self%n_fragment_atoms)
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)

      ! Skip fragment atoms in a particular unit cell
      call create_(uskip,n_unit_cell_atoms)

      ! Loop over allowed cells
      do h1 = self%h_min(1),self%h_max(1)
         do h2 = self%h_min(2),self%h_max(2)
            do h3 = self%h_min(3),self%h_max(3)

         ! Get cell displacement vector if needed
               off = matmul(rcm,REALIFY(([h1,h2,h3])))

         ! Get list of atoms in "match" which are in this cell
               hcode = CLUSTER_H_CODE(h1,h2,h3)
               match = indices_of_elements_matching_(hlist,hcode)

         ! Are there any fragment atoms in this cell?
         ! Set the unit cell atoms corresponding to them
               has_frag_atoms = size(match)>0
               if (has_frag_atoms) then
                  uskip = FALSE
                  do k = 1,size(match)
                     uskip(ulist(match(k))) = TRUE
                  end do
               end if

         ! Loop over unit cell atoms 
               do u = 1,n_unit_cell_atoms

            ! Skip fragment atoms?
                  if (has_frag_atoms) then
                     if (uskip(u)) cycle
                  end if
                  if (has_atoms_to_suppress) then
                     if (any(self%unit_cell_atoms_to_suppress==u)) cycle
                  end if

            ! Position of cell atom u in [h1,h2,h3] cell
                  pos = geom0(:,u)+off

            ! Test if this atom pos is within .radius
                  within_radius = FALSE
                  do k = 1,self%n_fragment_atoms
                     d = frag(:,k)-pos

               ! Sequentially eliminate far points
                     if (abs(d(1))>self%radius) cycle
                     if (abs(d(2))>self%radius) cycle
                     if (abs(d(3))>self%radius) cycle

               ! Don't use square root
                     d2 = d(1)*d(1)+d(2)*d(2)+d(3)*d(3)
                     if (d2>radius2) cycle
                     within_radius = TRUE
                     exit
                  end do
                  if (NOT within_radius) cycle

            ! New atom ... add it in ...
                  code = hcode+CLUSTER_UA_CODE(u)
                  call append_(self%occupation_list,code)
               end do

         ! Clean
               call destroy_(match)
            end do
         end do
      end do

      ! Clean
      call destroy_(uskip)
      call destroy_(ulist)
      call destroy_(hlist)
      call destroy_(geom0)
      call destroy_(frag)
   end subroutine

   PURE subroutine do_connectify(self)
   ! Find the connected molecules in the "occupation_list" from the
   ! .unit_cell_connection table.
      type(CRYSTAL_TYPE), INOUT :: self
      INT :: i,c,code,h1,h2,h3,u,k1,k2,k3,t,old,mi,mo
      VEC(INT,:), allocatable :: molecule_for_atom
      VEC(INT,:), allocatable :: con
   ENSURE(allocated(self%unit_cell_connection),"CRYSTAL:do_connectify ... no unit cell connection table")

      ! No. of atoms
      self%n_atoms = size(self%occupation_list)

      ! Clear info arrays
      call destroy_(self%atom_connection)
      call create_(self%atom_connection,self%n_atoms)
      call destroy_(self%molecule_for_atom)

      ! Assume every atom it's own molecule
      call create_(molecule_for_atom,self%n_atoms)
      do i = 1,self%n_atoms
         molecule_for_atom(i) = i
      end do

      ! Loop on all atoms "i"
      i = 0
      do
         i = i+1

         ! Finished atoms "i"?
         if (i>self%n_atoms) exit

         ! Info about atom "i"
         mi = molecule_for_atom(i)  ! Molecule "mi" for i
         code = self%occupation_list(i)  ! Occupation code
         u = CLUSTER_UA_FROM_CODE(code)  ! Unit cell atom

         ! No connections? next ...
         if (size(self%unit_cell_connection(u)%element)==0) cycle

         ! Get unit cell atom offset for atom "i"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)

         ! Find atoms which connect to u
         do c = 1,size(self%unit_cell_connection(u)%element)

            ! List of connections
            con = self%unit_cell_connection(u)%element(c)%element

            ! Get code for connected atom
            k1 = h1+con(1)
            k2 = h2+con(2)
            k3 = h3+con(3)
            t = con(4)
            code = CLUSTER_AT_CODE(k1,k2,k3,t)

            ! Is it new in the occupation list?
            old = index_of_value_(self%occupation_list,code)
            if (old>0) then

               ! This is an old (found) atom. Replace the molecule
               ! index by the lower of  "mi" and "mo" everywhere.
               mo = molecule_for_atom(old)
               if (mo<mi) then
                  where (molecule_for_atom==mi) molecule_for_atom = mo
                  mi = mo
               else if (mo>mi) then
                  where (molecule_for_atom==mo) molecule_for_atom = mi
               end if

               ! Atom "old" is connection to "i"
               call append_(self%atom_connection(i)%element,old)
            end if
         end do  ! connections to u
      end do  ! atoms "i"

      ! Set (sorted) molecule index array
      self%molecule_for_atom = molecule_for_atom

      ! Make molecule info
      call make_molecule_info_(self)
   end subroutine

   PURE subroutine do_defragment_0(self)
   ! Defragment the generated cluster and find the connected molecules
   ! using the .unit_cell_connection table.
      type(CRYSTAL_TYPE), INOUT :: self
      INT :: i,c,code,h1,h2,h3,u,k1,k2,k3,t,old,mi,mo
      VEC(INT,:), allocatable :: molecule_for_atom
      VEC(INT,:), allocatable :: con
   ENSURE(allocated(self%occupation_list),"CRYSTAL:do_defragment_0 ... no occupation_list")
   ENSURE(allocated(self%unit_cell_connection),"CRYSTAL:do_defragment_0 ... no unit cell connection table")

      ! Initial no. of atoms
      self%n_atoms = size(self%occupation_list)

      ! Clear info arrays
      call destroy_(self%atom_connection)
      call create_(self%atom_connection,self%n_atoms)
      call destroy_(self%molecule_for_atom)

      ! Assume every atom it's own molecule
      call create_(molecule_for_atom,self%n_atoms)
      do i = 1,self%n_atoms
         molecule_for_atom(i) = i
      end do

      ! Loop on all atoms "i"
      i = 0
      do
         i = i+1

         ! Any more atoms to connect?
         if (i>self%n_atoms) exit

         ! Info about atom "i"
         mi = molecule_for_atom(i)  ! Molecule "mi" for i
         code = self%occupation_list(i)  ! Occupation code
         u = CLUSTER_UA_FROM_CODE(code)  ! Unit cell atom

         ! No connections to this atom? next ...
         if (size(self%unit_cell_connection(u)%element)==0) cycle

         ! Get unit cell atom offset for atom "i"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)

         ! Loop on atoms "c" connected to "u"
         do c = 1,size(self%unit_cell_connection(u)%element)

            ! List of connections
            con = self%unit_cell_connection(u)%element(c)%element

            ! Get code for connected atom
            k1 = h1+con(1)
            k2 = h2+con(2)
            k3 = h3+con(3)
            t = con(4)
            code = CLUSTER_AT_CODE(k1,k2,k3,t)

            ! Is it new in the occupation list?
            old = index_of_value_(self%occupation_list,code)
            if (old>0) then

               ! This is an old (found) atom. Replace the molecule
               ! index by the lower of  "mi" and "mo" everywhere.
               mo = molecule_for_atom(old)
               if (mo<mi) then
                  where (molecule_for_atom==mi) molecule_for_atom = mo
                  mi = mo
               else if (mo>mi) then
                  where (molecule_for_atom==mo) molecule_for_atom = mi
               end if
               call append_(self%atom_connection(i)%element,old)
            else

               ! This is a new atom. Increment no. of atoms,
               ! store in the occ list and add new connection.
               call append_(self%occupation_list,code)
               call append_(molecule_for_atom,mi)
               self%n_atoms = self%n_atoms+1
               call expand_(self%atom_connection,self%n_atoms)
               call append_(self%atom_connection(i)%element,self%n_atoms)
            end if
         end do
      end do

      ! Set (sorted) molecule index array
      self%molecule_for_atom = molecule_for_atom

      ! Make molecule info
      call make_molecule_info_(self)
   end subroutine

   PURE subroutine do_defragment_v2(self)
   ! Defragment the generated cluster and find the connected molecules
   ! using the .unit_cell_connection table. It can handle network compound without blowing up
   ! It overwrites .defragment = TRUE
      type(CRYSTAL_TYPE), INOUT :: self
      INT :: i,v,c,h1,h2,h3,u,k1,k2,k3,t
      INT :: mi,mo,code,old,new
      INT :: safetyCounter
      VEC(INT,:), allocatable :: molecule_for_atom
      VEC(INT,:), allocatable :: con
      VEC(BIN,:), allocatable :: stop_search  ! Bin array. size = n_atoms. Add elements along with .occupation_list
      VEC(type(EVEC_BIN_TYPE),:), allocatable :: root_uc_atom  ! n_atoms by .n_unit_cell_atoms. Add rows along with .occupation_list
      VEC(BIN,:), allocatable :: tmp1,tmp2,ind1,ind2  ! tmp1&2 are for i-th, old-th in the root_uc_atom nested array.
   ENSURE(allocated(self%occupation_list),"CRYSTAL:do_defragment_v2 ... no occupation_list")
   ENSURE(allocated(self%unit_cell_connection),"CRYSTAL:do_defragment_v2 ... no unit cell connection table")
                                          ! ind1&2 are for index of .occupation_list
      ! Initial no. of atoms
      self%n_atoms = size(self%occupation_list)

      ! Clear info arrays
      call destroy_(self%atom_connection)
      call create_(self%atom_connection,self%n_atoms)
      call destroy_(self%molecule_for_atom)
      call create_(stop_search,self%n_atoms)  ! Initialize "stop_search" for the initial atoms.
      stop_search = FALSE  ! Get ready for finding bonded atoms for the initial atom. 
      call create_(root_uc_atom,self%n_atoms)
      call create_(tmp1,self%n_unit_cell_atoms)
      call create_(tmp2,self%n_unit_cell_atoms)

      ! Assume every atom it's own molecule
      call create_(molecule_for_atom,self%n_atoms)
      do i = 1,self%n_atoms  ! initial occupation_list => fragment atom order = xml atom order
         molecule_for_atom(i) = i
         call create_(root_uc_atom(i)%element,self%n_unit_cell_atoms)
         root_uc_atom(i)%element = FALSE
         root_uc_atom(i)%element(self%unit_cell_atom_for_frag_atom(i)) = TRUE  ! Every frag atom is its own root by itself
      end do

      ! Loop on all atoms "i"
      i = 1
      safetyCounter = 0  ! Kang. Initialize Forced-stop counter
      do
         if (safetyCounter>3000) exit  ! Forced termination if total attempts to find connected atoms exceeds 1000          

         ! Any more atoms to connect?
         if (i>self%n_atoms) exit

         ! Info about atom "i"
         mi = molecule_for_atom(i)  ! Molecule "mi" for i
         code = self%occupation_list(i)  ! Occupation code
         u = CLUSTER_UA_FROM_CODE(code)  ! Unit cell atom

         ! No connections to this atom? next ...
         if (size(self%unit_cell_connection(u)%element)==0) then  ! .unit_cell_connection :: VEC{EVEC{EVEC{INT}}}@ 
            i = i+1  ! ._for u-th UC atom___(u)[i][1:3] h1,h2,h3 
            cycle  ! .____________________(u)[i][4]   connected UC atom 
         end if

         ! Get unit cell atom offset for atom "i"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)

         ! Loop on atoms "c" connected to "u"
         do c = 1,size(self%unit_cell_connection(u)%element)
            safetyCounter = safetyCounter+1

            ! List of connections
            con = self%unit_cell_connection(u)%element(c)%element

            ! Get code for connected atom
            k1 = h1+con(1)
            k2 = h2+con(2)
            k3 = h3+con(3)
            t = con(4)
            code = CLUSTER_AT_CODE(k1,k2,k3,t)

            ! Is it new in the occupation list?
            old = index_of_value_(self%occupation_list,code)
            if (old>0) then

               ! This is an old (found) atom. Replace the molecule
               ! index by the lower of  "mi" and "mo" everywhere.
               mo = molecule_for_atom(old)
               if (mo<mi) then
                  where (molecule_for_atom==mi) molecule_for_atom = mo
                  mi = mo
               else if (mo>mi) then
                  where (molecule_for_atom==mo) molecule_for_atom = mi
               end if
               call append_(self%atom_connection(i)%element,old)

               ! Kang's major modification here 
               !  Unification of two roots (familty trees)
               tmp1 = root_uc_atom(old)%element  ! tmp1,tmp2: Family labels 
               tmp2 = root_uc_atom(i)%element  ! note: tmp1.dim = .n_unit_cell_atoms
               call destroy_(ind1)
               call destroy_(ind2)
               call create_(ind1,self%n_atoms)
               call create_(ind2,self%n_atoms)
               do v = 1,self%n_atoms  ! Find the family members of each root
                  ind1(v) = same_as_(tmp1,root_uc_atom(v)%element)  ! members from family 1
                  ind2(v) = same_as_(tmp2,root_uc_atom(v)%element)  !         "           2
               end do
               ind1 = ind1 OR ind2  ! merged family members
               tmp1 = tmp1 OR tmp2  ! New family name
               do v = 1,self%n_atoms  ! Update the new family name to the merged family members
                  if (ind1(v)) then
                     root_uc_atom(v)%element = tmp1
                  end if
               end do
            else
               if (stop_search(i)) cycle  ! Do not append new atom if stop_search(i) is true.
                                          ! Note: this "cycle" is for "do c=1,.unit_cell_connection(u).element.dim"
                                          ! i=i+1 will be done after quitting this "do c=.." loop

               ! This is a new atom. Increment no. of atoms,
               ! store in the occ list and add new connection.
               call append_(self%occupation_list,code)
               call append_(molecule_for_atom,mi)
               new = self%n_atoms+1  ! Kang's comment: I introduced 'new' for readablity by distinguishing
                                 !                 the (increaded) array size from the last index
               call expand_(self%atom_connection,self%n_atoms+1)  ! nested-array size increased
               call append_(self%atom_connection(i)%element,new)  ! put 'new' to 'i'

               ! Kang's majore modification 
               !  Assign the family name and stop flag for the newly found atom.
               call append_(stop_search,root_uc_atom(i)%element(t))  ! Reminder: t = con(4) = unit cell atom code
                  ! Above line askes whether t-unit_cell_atom is already in the i-atom's family. if YES, no more search from this newly found atom
               tmp1 = root_uc_atom(i)%element
               do v = 1,size(root_uc_atom)  ! find the all preexisting family members sharing root_uc_atom(i)

                                          ! Maybe it is ok to "do v = 1,i" no need for looking beyond 'i', will be revisited in the later time
                  if (same_as_(tmp1,root_uc_atom(v)%element)) then
                     if (molecule_for_atom(v)==mi) then

                     !  Note that, two completed molecules in translation symmetry have the same root_uc_atom
                     !  So molecule_for_atom should be checked for distinguishing translated molecules
                        root_uc_atom(v)%element(t) = TRUE  ! Let them know that 't' is now in the family 
                  ! It was WRONG idea // molecule_for_atom(v) = mi     ! same family book --> same family members!
                     end if  ! / == mi  ?
                  end if  ! / ==tmp1 ?
               end do
               call expand_(root_uc_atom,self%n_atoms+1)
               ! award the updated i-family book to 't' 
               call append_(root_uc_atom(new)%element,root_uc_atom(i)%element)  ! Note: root_uc_atom(i) is updated. so root_uc_atom(i).element(t) = TRUE now.
               self%n_atoms = self%n_atoms+1  ! All done for this round, .n_atoms is updated.
            end if
         end do  ! do c = 1,.unit_cell_connection ... 
         i = i+1  ! move on to the next atom in the .occupation_list   
      end do  ! do loop (equivalent to while ( i <=.n_atoms 'AND' safetyCounter < "Threshold value" )

      !! In most cases, it is ok to remove the atoms whose stop_search = TRUE, unless systems of a few atoms per unit cell
      !! The following contents may join in the next commit
      ! if (.occupation_list.dim > 27) then ! 3x3x3 cell
      ! do i = stop_search.dim,1,-1 ! reverse order for pruning
      !    if (stop_search(i)) then
      !       .occupation_list.prune_element(i)
      !       molecule_for_atom.prune_element(i)
      !       root_uc_atom.prune_element(i)
      !       atom_connection.prune_element(i)
      !
      !       stop_search.prune_element(i)
      !    end
      ! end
      ! end
      ! .n_atoms = .occupation_list.dim 

      ! make molecule_for_atom consecutive  Abusing variables
      c = maxval(molecule_for_atom)
      molecule_for_atom = -molecule_for_atom
      t = 0
      do i = -1,-c,-1
         if (any(molecule_for_atom==i)) then
            t = t+1
            where (molecule_for_atom==i) molecule_for_atom = t
         end if
      end do

      ! Set (sorted) molecule index array
      self%molecule_for_atom = molecule_for_atom

      ! Make molecule info
      self%defragment = TRUE  ! it activates ".make_unique_molecule_info" at the end of ".make_molecule_info"  
      call make_molecule_info_(self)
   end subroutine

   PURE subroutine do_defragment_1(self,occupation_list)
   ! Defragment the "occupation_list"
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), allocatable, INOUT :: occupation_list
      INT :: n_atoms,i,c,code,h1,h2,h3,u,k1,k2,k3,t,old
      VEC(INT,:), allocatable :: con
   ENSURE(allocated(occupation_list),"CRYSTAL:do_defragment_1 ... no occupation_list")
   ENSURE(allocated(self%unit_cell_connection),"CRYSTAL:do_defragment_1 ... no unit cell connection table")

      ! Initial no. of atoms
      n_atoms = size(occupation_list)

      ! Loop on all atoms "i"
      i = 0
      do
         i = i+1

         ! Any more atoms to connect?
         if (i>n_atoms) exit

         ! Info for atom "i"
         code = occupation_list(i)  ! Occupation code
         u = CLUSTER_UA_FROM_CODE(code)  ! Unit cell atom

         ! No connections to this atom? next ...
         if (size(self%unit_cell_connection(u)%element)==0) cycle

         ! Get unit cell atom offset "h"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)

         ! Loop on atoms "c" connected to "u"
         do c = 1,size(self%unit_cell_connection(u)%element)

            ! List of connections
            con = self%unit_cell_connection(u)%element(c)%element

            ! Get code for connected atom
            k1 = h1+con(1)
            k2 = h2+con(2)
            k3 = h3+con(3)
            t = con(4)
            code = CLUSTER_AT_CODE(k1,k2,k3,t)

            ! Is it new in the occupation list?
            old = index_of_value_(occupation_list,code)
            if (old>0) cycle  ! if old

            ! This is a new atom. Increment no. of atoms,
            ! store in the occ list.
            call append_(occupation_list,code)
            n_atoms = n_atoms+1
         end do
      end do
   end subroutine

   PURE subroutine do_defragment_uc_atom_0(self,u,geometry)
   ! Defragment a single atom "u" in the unit cell and produce a
   ! "geometry" in crystal coordinates.
      type(CRYSTAL_TYPE), IN :: self
      INT, IN :: u
      MAT(REAL,:,:), allocatable, OUT :: geometry
      VEC(INT,:), allocatable :: occupation_list

      ! Creat one-atom occ list
      call create_(occupation_list,1)
      occupation_list(1) = CLUSTER_AT_CODE(0,0,0,u)

      ! Defragment and extract geometry
      call do_defragment_(self,occupation_list)
      call extract_geometry_(self,occupation_list,geometry)

      ! Clean
      call destroy_(occupation_list)
   end subroutine

   PURE subroutine do_defragment_uc_atom_1(self,u,symop,par,pos,mass)
   ! Defragment a single atom "u" in the unit cell and produce the
   ! "symop" (index) which maps the asymmetric parent-atom "par" into
   ! "u", the *list* of cartesian positions in "pos",  and the list of
   ! corresponding atomic masses in "mass".  Needed for LFF refractive
   ! index calculations.
      type(CRYSTAL_TYPE), IN :: self
      INT, IN :: u
      INT, OUT :: symop
      VEC(INT,:), OUT :: par
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: mass
      VEC(INT,:), allocatable :: occupation_list

      ! Get symop for cell atom "u"
      symop = self%asym_symop_for_unit_cell_atom(u)

      ! Make the coded occupation_list for this atom
      call create_(occupation_list,1)
      occupation_list(1) = CLUSTER_AT_CODE(0,0,0,u)

      ! Defragment this atom and get the info
      call do_defragment_(self,occupation_list)
      call extract_atom_info_(self,occupation_list,par,pos,mass)

      ! Clean up
      call destroy_(occupation_list)
   end subroutine

   PURE subroutine do_defragment_uc_atom_2(self,u,pos,charge,dipole,frag_atom)
   ! Defragment a single atom "u" in the unit cell and extract
   ! *cartesian* atom positions "pos", the atomic charges and dipoles
   ! "charge" and "dipole" in a *flat* array, and the frag atom for
   ! the atoms.  These properties are requited for LFF refractive
   ! index calculations.
      type(CRYSTAL_TYPE), IN :: self
      INT, IN :: u
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: charge,dipole
      VEC(INT,:), optional, OUT :: frag_atom
      VEC(INT,:), allocatable :: occupation_list

      ! Make the coded occupation_list for this atom
      call create_(occupation_list,1)
      occupation_list(1) = CLUSTER_AT_CODE(0,0,0,u)

      ! Defragment this atom and get the info
      call do_defragment_(self,occupation_list)
      call extract_atom_info_(self,occupation_list,pos,charge,dipole,frag_atom)

      ! Clean up
      call destroy_(occupation_list)
   end subroutine

   PURE subroutine do_defragment_uc_atom_3(self,u,symop,par,pos,charge,mass,dipole,alpha_q,alpha_m)
   ! Defragment a single atom "u" in the unit cell and for the
   ! *molecule* produce the "symop" (3x3 matrix) which maps "u" into
   ! the *last* asymmetric parent-atom "par" in the *list* of
   ! defragged cartesian positions in "pos", the atomic "charge",
   ! "mass", and "dipole", and the atomic charge and dipole
   ! polarizabilities "alpha_q" and "alpha_m".
   ! NOTE: symop matrix seems strange cf. routine above.
      type(CRYSTAL_TYPE), IN :: self
      INT, IN :: u
      MAT(REAL,:,:), OUT :: symop
      VEC(INT,:), OUT :: par
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: charge
      VEC(REAL,:), OUT :: mass
      MAT(REAL,:,:), OUT :: dipole
      MAT(REAL,:,:), OUT :: alpha_q
      MAT3(REAL,:,:,:), OUT :: alpha_m
      VEC(INT,:), allocatable :: occupation_list

      ! Make the coded occupation_list for this atom
      call create_(occupation_list,1)
      occupation_list(1) = CLUSTER_AT_CODE(0,0,0,u)

      ! Defragment this atom and get the *molecule* info
      call do_defragment_(self,occupation_list)
      call extract_molecule_atom_info_(self,occupation_list,symop,par,pos,charge,mass,dipole,alpha_q,alpha_m)

      ! Clean up
      call destroy_(occupation_list)
   end subroutine

   PURE subroutine make_molecule_info(self)
   ! Sort the cluster into molecules whose indices appear in ascending
   ! order; also store the atom indices in each molecule.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: unique
      INT :: mi,mj,a,m
   ENSURE(allocated(self%molecule_for_atom),"CRYSTAL:make_molecule_info ... no molecule_for_atom")

      ! Get molecule indices & sort ascdending
      call to_unique_elements_of_(unique,self%molecule_for_atom)
      call sort_(unique)

      ! Number of molecules in cluster
      self%n_molecules = size(unique)  !Kang note: ".n_molecules" is from # of unique elements in ".molecule_for_atom"

      ! Put .molecule_for_atom into ascending order
      do mi = 1,self%n_molecules
         mj = unique(mi)
         if (mj==mi) cycle
         where (self%molecule_for_atom==mj) self%molecule_for_atom = mi
      end do

      ! Clean
      call destroy_(unique)

      ! Now make the molecule list
      call create_(self%molecule,self%n_molecules)
      do a = 1,self%n_atoms
         m = self%molecule_for_atom(a)
         call append_(self%molecule(m)%element,a)
      end do

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (self%defragment) call make_unique_molecule_info_(self)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   end subroutine

   PURE subroutine make_unique_molecule_info(self)
   ! Sort the cluster molecules so that the atom order in
   ! each is consistent. This only works for defragmented
   ! clusters.
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: li,lj
      VEC(INT,:), allocatable :: ci,cj,ui,uj,ai,aj
      VEC(INT,:), allocatable :: permutation,new
      INT :: mi,mj,ni,s,si,sj
      INT :: ic,i0,i1,i2,i3
      INT :: jc,j0,j1,j2,j3
      VEC(REAL,3) :: pi,pj,ps
      VEC(INT,3) :: shift
      BIN :: related
   ENSURE(allocated(self%molecule_for_atom),"CRYSTAL:make_unique_molecule_info ... no molecule_for_atom")

      ! Clean & get ready
      call destroy_(self%unique_molecule_for)
      call create_(self%unique_molecule_for,self%n_molecules)
      call destroy_(self%unique_symcode_for)
      call create_(self%unique_symcode_for,self%n_molecules)

      ! List of unique molecules for each cluster atom
      ! and their relating symops
      self%n_unique_molecules = 0
      self%unique_molecule_for = 0
      self%unique_symcode_for = 0

      ! Make atom-order in each molecule consistent
      do mi = 1,self%n_molecules

         ! Processed?
         if (self%unique_molecule_for(mi)>0) then  ! Kang corrected
            cycle  ! Processed before hand -> skip to the next molecule
         else  ! new unique Molecule found
            self%n_unique_molecules = self%n_unique_molecules+1
         end if
         li = self%molecule(mi)%element  ! Knag note: li::VEC{INT} = mi-molecule's atom index (for .occupation_list ) (1~.occupation_list.dim)
         ni = size(li)
         call create_(ai,ni)
         call create_(ui,ni)
         call create_(ci,ni)
         ci = self%occupation_list(li)  ! Kang note: ci::VEC{INT} = atom CODEs of the mi-molecule (555xx)
         ui = CLUSTER_UA_FROM_CODE(ci)  !            ui::VEC{INT} = corresponding unit cell atom index (1~.n_unit_cell_atoms)
         ai = self%asym_atom_for_unit_cell_atom(ui)  ! 
         si = self%asym_symop_for_unit_cell_atom(ui(1))  !          
         si = self%spacegroup%inverse_of_symops(si)  !            si::INT      = inverse of the corrsponding symop for the first atom in mi-molecule
         ic = ci(1)
         call destroy_(ci)
         call destroy_(ui)
                                                      ! Kang note: ic::INT (= ci(1)) = code for the first atom in the mi-th molecule
         i0 = CLUSTER_UA_FROM_CODE(ic)  !            i0::INT  = corresponding unit cell atom index of ic-atom 
         i1 = CLUSTER_H1_FROM_CODE(ic)
         i2 = CLUSTER_H2_FROM_CODE(ic)
         i3 = CLUSTER_H3_FROM_CODE(ic)
         pi = self%unit_cell_geometry(:,i0)+[i1,i2,i3]+self%fragment_offset  !.
                                                      !            pi::VEC{REAL}(3) unbounded fractional(crystal) coordinate of ci-atom 
         ! Find matching molecules
         do mj = mi+1,self%n_molecules
            lj = self%molecule(mj)%element  ! Kang note: lj::VEC{INT} = atom codes of mj-th molecule

            ! Found?
            if (self%unique_molecule_for(mj)>0) cycle

            ! Same # on atoms?
            if (size(lj)/=ni) cycle
            call create_(aj,ni)
            call create_(uj,ni)
            call create_(cj,ni)
            cj = self%occupation_list(lj)  ! Kang note: cj::VEC{INT} = atom CODEs of the mj-molecule
            uj = CLUSTER_UA_FROM_CODE(cj)  !            uj = corresponding unit cell atoms
            aj = self%asym_atom_for_unit_cell_atom(uj)  !            aj = corresponding corresponding asymmetric atoms
            sj = self%asym_symop_for_unit_cell_atom(uj(1))  !         sj::INT = symop index for the first atom in mj-molecule
            call destroy_(cj)
            call destroy_(uj)

            ! Match asym atoms
            call create_(permutation,ni)
            call is_permutation_of_(ai,aj,related,permutation)  ! Are atoms from mi- & mj-molecule from the same asymmetric unit atoms?
            if (related) then  ! mj-molecule is related to mi-molecule

               ! New order
               new = lj(permutation)

               ! First atom in mj (matched to mi)
               jc = self%occupation_list(new(1))

               ! Position of first atom in mj
               j0 = CLUSTER_UA_FROM_CODE(jc)
               j1 = CLUSTER_H1_FROM_CODE(jc)
               j2 = CLUSTER_H2_FROM_CODE(jc)
               j3 = CLUSTER_H3_FROM_CODE(jc)
               pj = self%unit_cell_geometry(:,j0)+[j1,j2,j3]+self%fragment_offset

               ! Symop which maps mi->mj
               s = self%spacegroup%product_of_symops(sj,si)

               ! Get shift
               ps = pi
               call transform_position_(self%spacegroup,ps,s)
               ps = pj-ps
               shift = nint(ps)  ! Kang note: shift::VEC{INT}(3) integer lattice translation vector

               ! Unique molecule
               self%unique_molecule_for(mj) = mi

               ! New symcode
               self%unique_symcode_for(mj) = CLUSTER_AT_CODE(shift(1),shift(2),shift(3),s)  ! Kang note: 's' is spacegroup index not unit_cell_atom index
               ! Kang's memo: The symmetry operator 's' can be wrong if the first atom in the molecule has another atom of the same kind. example B2H6

               ! Reorder atoms in mj to be consistent with mi
               self%occupation_list(lj) = self%occupation_list(new)

               ! Reset
               self%molecule(mj)%element = new
               call destroy_(new)
            end if
            call destroy_(permutation)
            call destroy_(aj)
         end do
         call destroy_(ai)
      end do
   end subroutine


!  ==================================================
!  Extract geometry & info from coded occupation list
!  ==================================================

   PURE subroutine extract_geometry_in_mols(self,occupation_list,geometry)
   ! Extract the cluster "geometry" (fractional) from an "occupation
   ! list" and place atoms in order of connected molecules.
   ! . Set the .is_fragment_atom flag array
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(INT,:), INOUT :: occupation_list
      MAT(REAL,:,:), allocatable, OUT :: geometry
      INT :: a,m,i,code,h1,h2,h3,u
      VEC(type(EVEC_INT_TYPE),:), allocatable :: atom_connection
      VEC(INT,:), allocatable :: new_order
      BIN :: connected
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:extract_geometry_in_mols ... no crystal unit cell geometry")

      ! No of atoms to extract
      ! Warning
      self%n_atoms = size(occupation_list)

      ! Extracted geometry
      call create_(geometry,3,self%n_atoms)

      ! Clear flags telling if fragment atom
      call destroy_(self%is_fragment_atom)
      call create_(self%is_fragment_atom,self%n_atoms)
   ! .new_frag_atom_ind.destroy
   ! .new_frag_atom_ind.create(.n_atoms)
   ! .new_frag_atom_ind = 0
   ! .new_frag_atom_ind = [(i,i=1,.n_fragment_atoms)]

      ! New ordering
      call create_(new_order,self%n_atoms)
      a = 0

      ! Loop over molecules
      do m = 1,self%n_molecules

         ! Loop over atoms "i"
         do i = 1,self%n_atoms
            if (self%molecule_for_atom(i)/=m) cycle

            ! Atom index
            a = a+1
            new_order(i) = a

            ! Get occ code for this atom
            code = occupation_list(i)

            ! Get unit cell atom "u" and cell offset "h"
            h1 = CLUSTER_H1_FROM_CODE(code)
            h2 = CLUSTER_H2_FROM_CODE(code)
            h3 = CLUSTER_H3_FROM_CODE(code)
            u = CLUSTER_UA_FROM_CODE(code)

            ! Get the cluster atom coordinate !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            geometry(:,i) = self%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset

            ! Set flag if fragment atom
            if (i<=self%n_fragment_atoms) then; self%is_fragment_atom(i) = TRUE
            else; self%is_fragment_atom(i) = FALSE
            end if
         end do
      end do

      ! Now reorder
      occupation_list(new_order) = occupation_list
      geometry(:,new_order) = geometry
      self%molecule_for_atom(new_order) = self%molecule_for_atom
      self%is_fragment_atom(new_order) = self%is_fragment_atom
   ! .new_frag_atom_ind(new_order) = .new_frag_atom_ind !<< Kang's note : Bug here xxxxxxxxxxxxxx

      ! This needs to be here, we must update .molecule to
      ! be in a consistent state with the rest of the object
      call destroy_(self%molecule)
      call create_(self%molecule,self%n_molecules)
      do a = 1,self%n_atoms
         m = self%molecule_for_atom(a)
         call append_(self%molecule(m)%element,a)
      end do

      ! Reorder connection table
      call create_(atom_connection,self%n_atoms)
      do i = 1,self%n_atoms
         connected = allocated(self%atom_connection(i)%element)
         if (NOT connected) cycle
         atom_connection(new_order(i))%element = new_order(self%atom_connection(i)%element(:))
      end do
      call destroy_(self%atom_connection)
      self%atom_connection = atom_connection

      ! Clean
      call destroy_(new_order)
   end subroutine

   PURE subroutine extract_geometry_no_mols(self,occupation_list,geometry)
   ! Extract the cluster "geometry" (fractional) from an "occupation
   ! list" and place atoms in order of connected molecules.
   ! . Set the .is_fragment_atom flag array
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(INT,:), INOUT :: occupation_list
      MAT(REAL,:,:), allocatable, OUT :: geometry
      INT :: i,code,h1,h2,h3,u
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:extract_geometry_no_mols ... no crystal unit cell geometry")

      ! No of atoms to extract
      self%n_atoms = size(occupation_list)

      ! Extracted geometry
      call create_(geometry,3,self%n_atoms)

      ! Clear flags telling if fragment atom
      call destroy_(self%is_fragment_atom)
      call create_(self%is_fragment_atom,self%n_atoms)

      ! Loop over atoms "i"
      do i = 1,self%n_atoms

         ! Get occ code for this atom
         code = occupation_list(i)

         ! Get unit cell atom "u" and cell offset "h"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)
         u = CLUSTER_UA_FROM_CODE(code)

         ! Get the cluster atom coordinate !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         geometry(:,i) = self%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset

         ! Set flag if fragment atom
         if (i<=self%n_fragment_atoms) then; self%is_fragment_atom(i) = TRUE
         else; self%is_fragment_atom(i) = FALSE
         end if
      end do
   end subroutine

   PURE subroutine extract_geometry(self,occupation_list,geometry)
   ! Extract the geometry (fractional) from an "occupation_list".
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), IN :: occupation_list
      MAT(REAL,:,:), allocatable, OUT :: geometry
      INT :: n_atoms,i,code
      INT :: h1,h2,h3,u
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:extract_geometry ... no crystal unit cell geometry")

      ! No. of atoms to extract
      n_atoms = size(occupation_list)

      ! Extracted geometry
      call create_(geometry,3,n_atoms)

      ! Loop over atoms "i"
      do i = 1,n_atoms

         ! Get occ code for this atom
         code = occupation_list(i)

         ! Get unit cell atom "u" and cell offset "h"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)
         u = CLUSTER_UA_FROM_CODE(code)

         ! Get the cluster atom coordinate
         geometry(:,i) = self%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset
      end do
   end subroutine

   PURE subroutine extract_atom_info_0(self,occupation_list,par,pos,mass)
   ! From a "occupation_list", extract the *cartesian* atom positions
   ! "pos", the index of the asymmetric-parent-atom in "par" and the
   ! atomic "mass" for each atom.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), IN :: occupation_list
      VEC(INT,:), OUT :: par
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: mass
      INT :: i,code,u,h1,h2,h3,p
      VEC(REAL,3) :: q
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:extract_atom_info_0 ... no crystal unit cell geometry")
   ENSURE(allocated(self%asym_atom_for_unit_cell_atom),"CRYSTAL:extract_atom_info_0 ... no asym_atom_for_unit_cell_atom")
   ENSURE(allocated(self%frag_atom_for_unit_cell_atom),"CRYSTAL:extract_atom_info_0 ... no frag_atom_for_unit_cell_atom")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:extract_atom_info_0 ... no asymmetric_cell_atom data")
   ENSURE(size(mass)==size(occupation_list),"CRYSTAL:extract_atom_info_0 ... mass, wrong size")
   ENSURE(size(pos,1)==3 AND size(pos,2)==size(occupation_list),"CRYSTAL:extract_atom_info_0 ... pos, wrong size")

      ! Loop over atoms "i"
      do i = 1,size(occupation_list)

         ! Get occ code for this atom
         code = occupation_list(i)

         ! Get unit cell atom "u" and cell offset "h"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)
         u = CLUSTER_UA_FROM_CODE(code)

         ! Get the cluster atom position (cartesian)
         q = self%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset
         call to_product_of_(pos(:,i),self%unit_cell%direct_mx,q)

         ! Get parent asymmetric atom and symop
         p = self%asym_atom_for_unit_cell_atom(u)
         par(i) = p

         ! Atomic mass
         mass(i) = self%asymmetric_unit_atom(p)%mass
      end do
   end subroutine

   PURE subroutine extract_atom_info_1(self,occupation_list,pos,charge,dipole,frag_atom)
   ! From the "occupation_list", extract the *cartesian* atom
   ! positions "pos", the atomic charges and dipoles "charge" and
   ! "dipole" in a *flat* array, and the frag atom for the atoms.
   ! NOTE:
   ! . All the quantities are available and non-zero.
   ! . "pos" and "dipole" are in the cartesian frame.
   ! WARNING: the asymmetric_unit_atom's should be generated from
   ! the fragment on which the charge and dipole polarisability
   ! calculations (see below) have been done.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), IN :: occupation_list
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: charge,dipole
      VEC(INT,:), optional, OUT :: frag_atom
      MAT3(REAL,:,:,:), allocatable :: seitz
      INT :: i,l,code,h1,h2,h3,u,p,s
      VEC(REAL,3) :: q
   ENSURE(self%fragment_info_made,"CRYSTAL:extract_atom_info_1 ... no crystal fragment_info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:extract_atom_info_1 ... no asymmetric_cell_atom data")
   ENSURE(has_dipoles_(self%asymmetric_unit_atom),"CRYSTAL:extract_atom_info_1 ... no atomic dipoles, did you calc them?")
   ENSURE(size(pos,1)==3 AND size(pos,2)==size(occupation_list),"CRYSTAL:extract_atom_info_1 ... pos, wrong size")
   ENSURE(size(charge)==size(occupation_list),"CRYSTAL:extract_atom_info_1 ... charge, wrong size")
   ENSURE(size(dipole)==3*size(occupation_list),"CRYSTAL:extract_atom_info_1 ... dipole, wrong size")

      ! Symops
      seitz = xyz_seitz_matrices_(self)

      ! Counter, dipole array
      l = 0

      ! Loop over atoms "i"
      do i = 1,size(occupation_list)

         ! Get occ code for this atom
         code = occupation_list(i)

         ! Get unit cell atom "u" and cell offset "h"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)
         u = CLUSTER_UA_FROM_CODE(code)

         ! Get the cluster atom position (cartesian)
         q = self%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset
         call to_product_of_(pos(:,i),self%unit_cell%direct_mx,q)

         ! Parent asymmetric atom and symop
         p = self%asym_atom_for_unit_cell_atom(u)
         s = self%asym_symop_for_unit_cell_atom(u)

         ! Parent and symop
         ! par(i) = p
         ! symop(i) = s

         ! Warning, the asymmetric_unit_atom's should be generated from
         ! the fragment on which the charge and dipole polarisability
         ! calculations (see below) have been done.

         ! Set the charges and dipoles
         charge(i) = self%asymmetric_unit_atom(p)%charge
         call to_product_of_(dipole(l+1:l+3),seitz(:,:,s),self%asymmetric_unit_atom(p)%dipole)
         l = l+3

         ! Fragment atom list
         if (NOT present(frag_atom)) cycle
         frag_atom(i) = self%frag_atom_for_unit_cell_atom(u)
      end do

      ! Clean
      call destroy_(seitz)
   end subroutine

   PURE subroutine extract_molecule_atom_info(self,occupation_list,symop,par,pos,charge,mass,dipole,alpha_q,alpha_m)
   ! From a *molecule* "occupation_list", extract the *cartesian* atom
   ! positions "pos", the indiex of the asymmetric-parent-atom in
   ! "par", the corresponding 3x3 symop matrix which maps the unit
   ! cell atom into the asymmetric-parent-atom, the atomic charges and
   ! masses "charge" and "mass", atomic dipoles "dipole", charge
   ! polarisabilities "alpha_q" and dipole polarisabilities "alpha_m".
   ! NOTE:
   ! . This assumes that the quantities are available and non-zero.
   ! . "pos" and everything else are in *cartesians*.
   ! . The symop returned is the last one for "occupation_list"
   !   This is OK for a whole molecule
   ! WARNING: the asymmetric_unit_atom's should be generated from
   ! the fragment on which the charge and dipole polarisability
   ! calculations (see below) have been done.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), IN :: occupation_list
      MAT(REAL,:,:), OUT :: symop
      VEC(INT,:), OUT :: par
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: charge
      VEC(REAL,:), OUT :: mass
      MAT(REAL,:,:), OUT :: dipole
      MAT(REAL,:,:), OUT :: alpha_q
      MAT3(REAL,:,:,:), OUT :: alpha_m
      MAT(REAL,:,:), allocatable :: T
      MAT3(REAL,:,:,:), allocatable :: inverse_seitz
      INT :: i,code,h1,h2,h3,u,p,s
      VEC(REAL,3) :: q
   ENSURE(self%fragment_info_made,"CRYSTAL:extract_molecule_atom_info ... no crystal fragment info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:extract_molecule_atom_info ... no asymmetric_cell_atom data")
   ENSURE(has_dipoles_(self%asymmetric_unit_atom),"CRYSTAL:extract_molecule_atom_info ... no atomic dipoles, did you calc them?")
   ENSURE(has_polarisabilities_(self%asymmetric_unit_atom),"CRYSTAL:extract_molecule_atom_info ... no atomic polarisabilities")
   ENSURE(size(charge)==size(occupation_list),"CRYSTAL:extract_molecule_atom_info ... charge, wrong size")
   ENSURE(size(mass)==size(occupation_list),"CRYSTAL:extract_molecule_atom_info ... mass, wrong size")
   ENSURE(all(shape(pos)==[3,size(occupation_list)]),"CRYSTAL:extract_molecule_atom_info ... pos, wrong size")
   ENSURE(all(shape(dipole)==[3,size(occupation_list)]),"CRYSTAL:extract_molecule_atom_info ... dipole, wrong size")
   ENSURE(all(shape(alpha_q)==[3,size(occupation_list)]),"CRYSTAL:extract_molecule_atom_info ... alpha_q, wrong size")
   ENSURE(all(shape(alpha_m)==[3,3,size(occupation_list)]),"CRYSTAL:extract_molecule_atom_info ... alpha_m, wrong size")

      ! Transposed/Inverse symops
      inverse_seitz = transposed_xyz_seitz_matrices_(self)

      ! Loop over atoms "i"
      do i = 1,size(occupation_list)

         ! Get occ code for this atom
         code = occupation_list(i)

         ! Get unit cell atom "u" and cell offset "h"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)
         u = CLUSTER_UA_FROM_CODE(code)

         ! Set the cluster atom position (cartesian)
         q = self%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset
         call to_product_of_(pos(:,i),self%unit_cell%direct_mx,q)

         ! Set parent asymmetric atom and symop
         p = self%asym_atom_for_unit_cell_atom(u)
         s = self%asym_symop_for_unit_cell_atom(u)
         T = inverse_seitz(:,:,s)
         par(i) = p

         ! Set the inverse 3x3 symop
         symop = T

         ! Warning, the asymmetric_unit_atom's should be generated from
         ! the fragment on which the charge and dipole polarisability
         ! calculations (see below) have been done.

         ! Set atomic electronic charge.
         charge(i) = self%asymmetric_unit_atom(p)%charge-self%asymmetric_unit_atom(p)%atomic_number
         mass(i) = self%asymmetric_unit_atom(p)%mass

         ! Set atomic dipole
         call to_product_of_(dipole(:,i),T,self%asymmetric_unit_atom(p)%dipole,transpose_a=TRUE)

         ! Set atomic charge polarisability
         call to_product_of_(alpha_q(:,i),T,self%asymmetric_unit_atom(p)%charge_polarisability,transpose_a=TRUE)

         ! Set atomic dipole polarisability
         alpha_m(:,:,i) = self%asymmetric_unit_atom(p)%dipole_polarisability
         call change_basis_using_(alpha_m(:,:,i),T)
      end do

      ! Clean
      call destroy_(inverse_seitz)
   end subroutine


!  ===================
!  Cluster information
!  ===================

   PURE function cluster_width(self) result (res)
   ! Return the width "res" of the cluster in each of the 3 axis directions.
   ! NOTE: using crystal axis system.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,3) :: res
   ENSURE(allocated(self%geometry),"CRYSTAL:cluster_width ... no fragment geometry")
      res = max_abs_column_difference_(self%geometry)
   end function

   PURE subroutine create_fragment_atom_list(self,fragment_atom)
   ! Create the "fragment_atom" list.
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: fragment_atom
   ENSURE(self%n_fragment_atoms>0,"CRYSTAL:create_fragment_atom_list ... no fragment atoms")
   ENSURE(allocated(self%is_fragment_atom),"CRYSTAL:create_fragment_atom_list ... no fragment atoms")
      call create_(fragment_atom,self%n_fragment_atoms)
      call make_fragment_atom_list_(self,fragment_atom)
   end subroutine

   PURE subroutine make_fragment_atom_list(self,fragment_atom)
   ! Make the "fragment_atom" list, a VEC{ATOM}
   ! WARNING: set info_made to FALSE in "fragment_atom" parent
   !          molecule, if there is one.
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), OUT :: fragment_atom
      VEC(INT,:), allocatable :: ulist
      MAT3(REAL,:,:,:), allocatable :: seitz
      INT :: a,f,u,p,s
   ENSURE(self%n_atoms>0,"CRYSTAL:make_fragment_atom_list ... no atoms")
   ENSURE(self%n_fragment_atoms>0,"CRYSTAL:make_fragment_atom_list ... no fragment atoms")
   ENSURE(allocated(self%is_fragment_atom),"CRYSTAL:make_fragment_atom_list ... no is_fragment_atom")
   ENSURE(count(self%is_fragment_atom)==self%n_fragment_atoms,"CRYSTAL:make_fragment_atom_list ... inconsistent is_fragment_atom")
   ENSURE(allocated(self%geometry),"CRYSTAL:make_fragment_atom_list ... no geometry")
   ENSURE(allocated(self%occupation_list),"CRYSTAL:make_fragment_atom_list ... no occupation_list")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:make_fragment_atom_list ... no asymmetric_cell_atom")
   ENSURE(allocated(self%asym_atom_for_unit_cell_atom),"CRYSTAL:make_fragment_atom_list ... no asym_atom-cell_atom")
   ENSURE(has_cartesian_axes_(self%asymmetric_unit_atom),"CRYSTAL:make_fragment_atom_list ... pADPs must be in cartn axis system")

      ! Unit cell atoms which correspond to the fragment atoms
      call create_(ulist,self%n_atoms)
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)

      ! Seitz matrices
      seitz = xyz_seitz_matrices_(self)

      ! Fragment atom
      f = 0

      ! Loop over cluster atoms "a"
      do a = 1,self%n_atoms
         if (NOT self%is_fragment_atom(a)) cycle

         ! Fragment atom
         f = f+1

         ! Extract unit cell atom and asymmetric parent
         u = ulist(a)
         p = self%asym_atom_for_unit_cell_atom(u)
         s = self%asym_symop_for_unit_cell_atom(u)

         ! Copy the parent atom
         fragment_atom(f) = self%asymmetric_unit_atom(p)

         ! Transform the pADPs (which must be in cartesians)
         call transform_pADP_vector_with_(fragment_atom(f),seitz(:,:,s))

         ! Reset cluster atom pos from .geometry(:,a)
         call set_position_(fragment_atom(f),self%geometry(:,a))
         call set_axis_system_to_(fragment_atom(f),"crystal")
      end do

      ! Clean
      call destroy_(ulist)

      ! Change coordinates to cartesian
      ! Errors (if any) are changed straightforwardly
      ! WARNING: set info_made to FALSE in parent molecule, if any
      call change_axis_system_to_(fragment_atom,"cartesian",self%unit_cell)
   end subroutine

   PURE subroutine create_mol_atom_list(self,mol_atom,m)
   ! Create the "mol_atom" ATOM list for molecule "m".
      type(CRYSTAL_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: mol_atom
      INT, IN :: m
   ENSURE(allocated(self%molecule),"CRYSTAL:create_mol_atom_list ... no molecule list")
   ENSURE(size(self%molecule)>=m,"CRYSTAL:create_mol_atom_list ... molecule m too large")
   ENSURE(allocated(self%molecule(m)%element),"CRYSTAL:create_mol_atom_list ... no molecule m atom indices")
      call create_(mol_atom,size(self%molecule(m)%element))
      call make_mol_atom_list_(self,mol_atom,m)
   end subroutine

   PURE subroutine make_mol_atom_list(self,mol_atom,m)
   ! Make the "fragment_atom" list, a VEC{ATOM}
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), OUT :: mol_atom
      INT, IN :: m
      VEC(INT,:), allocatable :: ulist
      MAT3(REAL,:,:,:), allocatable :: seitz
      INT :: f,a,u,p,s
   ENSURE(allocated(self%geometry),"CRYSTAL:make_mol_atom_list ... no geometry")
   ENSURE(allocated(self%occupation_list),"CRYSTAL:make_mol_atom_list ... no occupation_list")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:make_mol_atom_list ... no asymmetric_cell_atom")
   ENSURE(allocated(self%asym_atom_for_unit_cell_atom),"CRYSTAL:make_mol_atom_list ... no asym_atom-cell_atom")
   ENSURE(has_cartesian_axes_(self%asymmetric_unit_atom),"CRYSTAL:make_mol_atom_list ... pADPs must be in cartn axis system")

      ! Unit cell atoms which correspond to the molecule's atoms
      call create_(ulist,self%n_atoms)
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)

      ! Seitz matrices
      seitz = xyz_seitz_matrices_(self)

      ! Loop over molecule m atoms "f"
      do f = 1,size(self%molecule(m)%element)
         a = self%molecule(m)%element(f)

         ! Extract unit cell atom and asymmetric parent
         u = ulist(a)
         p = self%asym_atom_for_unit_cell_atom(u)
         s = self%asym_symop_for_unit_cell_atom(u)

         ! Copy the parent atom
         ! NOTE: .basis is a ptr copy
         mol_atom(f) = self%asymmetric_unit_atom(p)

         ! Transform the pADPs (which must be in cartesians)
         call transform_pADP_vector_with_(mol_atom(f),seitz(:,:,s))

         ! Reset cluster atom pos from .geometry(:,a)
         call set_position_(mol_atom(f),self%geometry(:,a))
         call set_axis_system_to_(mol_atom(f),"crystal")
      end do

      ! Clean
      call destroy_(ulist)

      ! Change coordinates to cartesian
      ! Errors (if any) are changed straightforwardly
      call change_axis_system_to_(mol_atom,"cartesian",self%unit_cell)
   end subroutine

   PURE function fragment_atom_indices(self) result (res)
   ! Return the indices of the fragment atoms in the cluster.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,self%n_fragment_atoms) :: res
      INT :: a,f
   ENSURE(self%n_fragment_atoms>0,"CRYSTAL:fragment_atom_indices ... no fragment atoms")
   ENSURE(allocated(self%is_fragment_atom),"CRYSTAL:fragment_atom_indices ... no fragment atoms")

      ! Loop over cluster atoms "a"
      f = 0
      do a = 1,self%n_atoms
         if (NOT self%is_fragment_atom(a)) cycle
         ! Set the fragment atom index
         f = f+1  ! Frag atom "f"
         res(f) = a
      end do
   end function

   PURE function nonfragment_atom_indices(self) result (res)
   ! Return the indices of the nonfragment atoms in the cluster.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,self%n_atoms-self%n_fragment_atoms) :: res
      INT :: a,f
   ENSURE(self%n_atoms>=self%n_fragment_atoms,"CRYSTAL:nonfragment_atom_indices ... no nonfragment atoms")
   ENSURE(allocated(self%is_fragment_atom),"CRYSTAL:nonfragment_atom_indices ... no fragment atoms")

      ! Loop over cluster atoms "a"
      f = 0
      do a = 1,self%n_atoms
         if (self%is_fragment_atom(a)) cycle
         ! Set the fragment atom index
         f = f+1  ! Frag atom "f"
         res(f) = a
      end do
   end function

   PURE function crystal_asym_unit_atom_indices(self) result (res)
   ! Return the indices of the crystal asymmetric unit cell atoms
   ! corresponding to the fragment atoms of the *cluster*.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,self%n_fragment_atoms) :: res
      INT :: f,i,code,u,a
   ENSURE(self%n_fragment_atoms>0,"CRYSTAL:crystal_asym_unit_atom_indices ... no fragment atoms")
   ENSURE(allocated(self%is_fragment_atom),"CRYSTAL:crystal_asym_unit_atom_indices ... no is_fragment_atom")
   ENSURE(allocated(self%occupation_list),"CRYSTAL:crystal_asym_unit_atom_indices ... no occupation_list")

      ! Loop over atoms
      f = 0
      do i = 1,self%n_atoms
         if (NOT self%is_fragment_atom(i)) cycle
         code = self%occupation_list(i)
         u = CLUSTER_UA_FROM_CODE(code)
         a = self%asym_atom_for_unit_cell_atom(u)
         f = f+1
         res(f) = a
      end do
   end function

   PURE function crystal_frag_atom_indices(self) result (res)
   ! Return the *crystal* fragment atom indices of the fragment atoms
   ! in the *cluster* (the two are not necessarily the same!)
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,self%n_fragment_atoms) :: res
      INT :: f,i,code,u,a
   ENSURE(self%n_fragment_atoms>0,"CRYSTAL:crystal_frag_atom_indices ... no fragment atoms")
   ENSURE(allocated(self%is_fragment_atom),"CRYSTAL:crystal_frag_atom_indices ... no is_fragment_atom")
   ENSURE(allocated(self%occupation_list),"CRYSTAL:crystal_frag_atom_indices ... no occupation_list")

      ! Loop over atoms
      f = 0
      do i = 1,self%n_atoms
         if (NOT self%is_fragment_atom(i)) cycle
         code = self%occupation_list(i)
         u = CLUSTER_UA_FROM_CODE(code)
         a = self%frag_atom_for_unit_cell_atom(u)
         f = f+1
         res(f) = a
      end do
   end function


!  =================
!  Cluster atom list
!  =================

   PURE subroutine create_atom_list(self,atom)
   ! Create and make a new atom list for the cluster
   ! NOTE: basis sets are pointer copied!
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: atom
   ENSURE(allocated(self%fragment_geometry),"CRYSTAL:create_atom_list ... no crystal fragment geometry")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:create_atom_list ... no atom data")
   ENSURE(self%n_atoms>0,"CRYSTAL:create_atom_list ... no atoms in cluster")
      call create_(atom,self%n_atoms)
      call make_atom_list_(self,atom)
   end subroutine

   PURE subroutine make_atom_list_0(self,atom)
   ! Kang's version of make_atom_list(2025.Mar.20) Make a new atom list from
   !  .occupation_list & .molecule_for_atom by using .unit_cell_atom. 
   ! unit_cell_atom should have ADP (thermal) tensors (in the cartesian axis system)
   ! No need for (.crystal).geometry
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), OUT :: atom
      VEC(INT,:), allocatable :: ulist
      INT :: a
      INT :: h1,h2,h3
      VEC(REAL,3) :: pos
   ENSURE(allocated(self%occupation_list),"CRYSTAL:make_atom_list_0 ... no occupation_list")
   ENSURE(self%n_atoms==size(self%occupation_list),"CRYSTAL:make_atom_list_0 ... disagreement in the number of cluster atoms")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_atom_list_0 ... no crystal fragment info")
   ENSURE(allocated(self%unit_cell_atom),"CRYSTAL:make_atom_list_0 ... no unit_cell_atom data")
   ENSURE(has_cartesian_axes_(self%unit_cell_atom),"CRYSTAL:make_atom_list_0 ... unit_cell_atom not in cartesian axes")
   ENSURE(allocated(self%molecule_for_atom),"CRYSTAL:make_atom_list_0 ... no molecule_for_atom info")

      ! Unit cell atoms which correspond to the fragment atoms
      call create_(ulist,self%n_atoms)
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)
      atom = self%unit_cell_atom(ulist)  ! Preliminary initialization. It is segmentation-fault-free !

      ! Loop over cluster atoms "a"
      do a = 1,self%n_atoms

         !> NOTE: 
         !>> atom(a)=.asymmetric_unit_atom(p) syntax causes Segmentation fault, so I did atom=.unit_cell_atom(ulist)

         ! copy from (.crystal).unit_cell_atom 
         ! .fragment_offset ::VEC{INT}(3) is made from make_occupation_list
         h1 = CLUSTER_H1_FROM_CODE(self%occupation_list(a))+self%fragment_offset(1)
         h2 = CLUSTER_H2_FROM_CODE(self%occupation_list(a))+self%fragment_offset(2)
         h3 = CLUSTER_H3_FROM_CODE(self%occupation_list(a))+self%fragment_offset(3)
         pos = atom(a)%position
         pos = pos+matmul(self%unit_cell%direct_mx,[h1,h2,h3])
         call set_position_(atom(a),pos)
         call set_group_(atom(a),self%molecule_for_atom(a))
      end do

      ! Update labels/groups
      call set_unique_tags_(atom)  ! forced label update
      call update_(atom)

      ! Free memory
      call destroy_(ulist)
   end subroutine


!   make_atom_list(atom) :: PURE  !! OLD version of make_atom_list commented out by Kang. 2025.Mar.20
!   ! Make a new atom list for the cluster. Note that the
!   ! .asymmetric_unit_atom's must have ADP (thermal) tensors in the
!   ! cartesian axis system; this will normally be the case for Tonto's
!   ! internal representation ....
!      self :: IN
!      atom :: VEC{ATOM}, OUT
!
!   ENSURE(.n_atoms>0,"no atoms in cluster")
!   ENSURE(atom.dim==.n_atoms,"wrong size, atoms")
!   ENSURE(.geometry.allocated,"no geometry")
!   ENSURE(.occupation_list.allocated,"no occupation_list")
!   ENSURE(.fragment_info_made,"no crystal fragment info")
!   ENSURE(.asymmetric_unit_atom.allocated, "no asymmetric_unit_atom data")
!   ENSURE(.asymmetric_unit_atom.has_cartesian_axes, "asymmetric_unit_atom not in cartesian axes")
!   ENSURE(.molecule_for_atom.allocated, "no molecule_for_atom info")
!
!      ulist :: VEC{INT}@
!      seitz :: MAT3{REAL}@
!      a,u,p,s :: INT
!      pos :: VEC{REAL}(3)
!
!      ! Unit cell atoms which correspond to the fragment atoms
!      ulist.create(.n_atoms)
!      ulist = CLUSTER_UA_FROM_CODE(.occupation_list)
!
!      ! Seitz matrices (cartesian!)
!      seitz = .xyz_seitz_matrices
!
!      ! Loop over cluster atoms "a"
!      do a = 1,.n_atoms
!
!         ! Extract unit cell atom, asymmetric parent and symop
!         u = ulist(a)
!         p = .asym_atom_for_unit_cell_atom(u)
!         s = .asym_symop_for_unit_cell_atom(u)
!
!         ! Copy the parent atom
!         ! NOTE: .basis is a ptr copy
!         atom(a) = .asymmetric_unit_atom(p)
!
!         ! Transform the pADPs (which must be in cartesians)
!         atom(a).transform_pADP_vector_with(seitz(:,:,s))
!
!         ! Reset cluster atom pos from .geometry(:,a) (in fractional)
!         pos = matmul(.unit_cell.direct_mx,.geometry(:,a))      ! Kang's comment (.crystal).geometry is possibly not allocated
!         atom(a).set_position(pos) ! Set new cartesian position 
!         atom(a).set_group(.molecule_for_atom(a))
!
!      end
!
!      ! Update labels/groups
!      atom.update
!
!      ! Free memory
!      seitz.destroy
!      ulist.destroy
!
!   end

   PURE subroutine make_atom_list_1(self,atom,list)
   ! Make a new atom list for the cluster from the indices in "list".
   ! Note that the .asymmetric_unit_atom's must have ADP tensors
   ! in the cartesian axis system; this will normally be the case for
   ! Tonto's internal representation ....
   ! NOTE: basis sets are pointer copied!
      type(CRYSTAL_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), OUT :: atom
      VEC(INT,:), IN :: list
      VEC(INT,:), allocatable :: ulist
      MAT3(REAL,:,:,:), allocatable :: seitz
      INT :: a,u,p,s
   ENSURE(size(atom)==size(list),"CRYSTAL:make_atom_list_1 ... wrong size, atomsr")
   ENSURE(allocated(self%geometry),"CRYSTAL:make_atom_list_1 ... no geometry")
   ENSURE(allocated(self%occupation_list),"CRYSTAL:make_atom_list_1 ... no occupation_list")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_atom_list_1 ... no crystal fragment info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:make_atom_list_1 ... no asymmetric_cell_atom data")
   ENSURE(has_cartesian_axes_(self%asymmetric_unit_atom),"CRYSTAL:make_atom_list_1 ... pADPs must be in cartn axis system")

      ! Unit cell atoms which correspond to the *list* fragment atoms
      call create_(ulist,size(list))
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list(list))

      ! Seitz matrices
      seitz = xyz_seitz_matrices_(self)

      ! Loop over cluster atoms "a"
      do a = 1,size(ulist)

         ! Extract unit cell atom, asymmetric parent and symop
         u = ulist(a)
         p = self%asym_atom_for_unit_cell_atom(u)
         s = self%asym_symop_for_unit_cell_atom(u)

         ! Copy the parent atom
         ! NOTE: .basis is a ptr copy
         atom(a) = self%asymmetric_unit_atom(p)

         ! Transform the pADPs (which must be in cartesians)
         call transform_pADP_vector_with_(atom(a),seitz(:,:,s))

         ! Reset cluster atom pos from .geometry(:,a)
         ! NOTE: .basis is a ptr copy
         call set_position_(atom(a),self%geometry(:,a))
         call set_axis_system_to_(atom(a),"crystal")
      end do

      ! Clean
      call destroy_(seitz)
      call destroy_(ulist)

      ! Change pos to cartesian
      ! Errors (if any) are changed straightforwardly
      call change_axis_system_to_(atom,"cartesian",self%unit_cell)

      ! Update labels/groups
      call update_(atom)
   end subroutine


!  ===============
!  Cluster charges
!  ===============

   PURE subroutine make_non_fragment_mu_charges(self,pos,charge)
   ! Make a list of the non-fragment atom positions and point charges
   ! derived from the atomic charges and dipoles only.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: charge
      MAT3(REAL,:,:,:), allocatable :: seitz
      VEC(REAL,3) :: dipole
      REAL :: d,mu,q
      INT :: a,i,code,h1,h2,h3,u,p,s
      VEC(REAL,3) :: r
   ENSURE(self%n_atoms>0,"CRYSTAL:make_non_fragment_mu_charges ... no atoms in cluster")
   ENSURE(size(pos,2)==3*(self%n_atoms-self%n_fragment_atoms),"CRYSTAL:make_non_fragment_mu_charges ... wrong dim2, pos")
   ENSURE(size(charge)==size(pos,2),"CRYSTAL:make_non_fragment_mu_charges ... wrong dim, charge")
   ENSURE(allocated(self%occupation_list),"CRYSTAL:make_non_fragment_mu_charges ... no occupation_list")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_non_fragment_mu_charges ... no crystal fragment info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:make_non_fragment_mu_charges ... no asymmetric_cell_atom data")

      ! Normal XYZ symops
      seitz = xyz_seitz_matrices_(self)
      i = -2

      ! Loop over cluster atoms "a"
      do a = 1,self%n_atoms
         if (self%is_fragment_atom(a)) cycle

         ! Get the occ code
         code = self%occupation_list(a)

         ! Get unit cell atom "u" and cell offset "h"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)
         u = CLUSTER_UA_FROM_CODE(code)

         ! Counter for 3 charges
         i = i+3

         ! Get the cluster atom position
         r = self%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset
         call to_product_of_(pos(:,i),self%unit_cell%direct_mx,r)

         ! Get parent asymmetric atom and symop
         p = self%asym_atom_for_unit_cell_atom(u)
         s = self%asym_symop_for_unit_cell_atom(u)

         ! Atomic charge
         charge(i) = self%asymmetric_unit_atom(p)%charge

         ! Atomic dipole
         call to_product_of_(dipole,seitz(:,:,s),self%asymmetric_unit_atom(p)%dipole)

         ! Make the two charges
         ! Twice the shift along dipole direction
         d = TOL(2)
         mu = norm_(dipole)
         q = mu/d
         dipole = dipole/mu*d*HALF
         pos(:,i+1) = pos(:,i)+dipole
         pos(:,i+2) = pos(:,i)-dipole
         charge(i+1) = q
         charge(i+2) = -q
      end do

      ! Clean
      call destroy_(seitz)
   end subroutine

   PURE subroutine make_non_fragment_qq_charges(self,pos,charge)
   ! Make a list of the non-fragment atom positions and charges
   ! from the atomic charges, dipoles, and *quadrupoles*
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: charge
      MAT3(REAL,:,:,:), allocatable :: seitz
      VEC(REAL,3) :: dipole,eval
      MAT(REAL,3,3) :: quadrupole,evec
      REAL :: d,mu,q
      INT :: a,i,code,h1,h2,h3,u,p,s
      VEC(REAL,3) :: r
   ENSURE(self%n_atoms>0,"CRYSTAL:make_non_fragment_qq_charges ... no atoms in cluster")
   ENSURE(size(pos,2)==9*(self%n_atoms-self%n_fragment_atoms),"CRYSTAL:make_non_fragment_qq_charges ... wrong dim2, pos")
   ENSURE(size(charge)==size(pos,2),"CRYSTAL:make_non_fragment_qq_charges ... wrong dim, charge")
   ENSURE(allocated(self%occupation_list),"CRYSTAL:make_non_fragment_qq_charges ... no occupation_list")
   ENSURE(self%fragment_info_made,"CRYSTAL:make_non_fragment_qq_charges ... no crystal fragment info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:make_non_fragment_qq_charges ... no asymmetric_cell_atom data")

      ! NOrmal XYZ symops
      seitz = xyz_seitz_matrices_(self)

      ! Counter for charges
      i = 1
      do a = 1,self%n_atoms
         if (self%is_fragment_atom(a)) cycle

         ! The occ code
         code = self%occupation_list(a)

         ! Get unit cell atom "u" and cell offset "h"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)
         u = CLUSTER_UA_FROM_CODE(code)

         ! Get the cluster atom position
         r = self%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset
         call to_product_of_(pos(:,i),self%unit_cell%direct_mx,r)

         ! Get parent asymmetric atom and symop
         p = self%asym_atom_for_unit_cell_atom(u)
         s = self%asym_symop_for_unit_cell_atom(u)

         ! Atomic charge
         charge(i) = self%asymmetric_unit_atom(p)%charge

         ! Atomic dipole
         call to_product_of_(dipole,seitz(:,:,s),self%asymmetric_unit_atom(p)%dipole)

         ! Atomic quadrupole
         call back_transform_to_(self%asymmetric_unit_atom(p)%quadrupole,quadrupole,seitz(:,:,s))

         ! Make the two charges
         d = TOL(2)  ! Twice the shift along dipole direction
         mu = norm_(dipole)
         q = mu/d
         dipole = dipole/mu*d*HALF
         pos(:,i+1) = pos(:,i)+dipole
         pos(:,i+2) = pos(:,i)-dipole
         charge(i+1) = q
         charge(i+2) = -q

         ! Make the diagonal quadrupoles
         ! Charge at d/2 along eigenvectors
         ! Should be x 4 for single charge
      ! quadrupole.solve_symmetric_eigenproblem(eval,evec)
         call diagonalize_Jacobi_(quadrupole,eval,evec)
         evec = d*evec/TWO
         eval = eval/(d*d)

         ! Two charges on axis at +d/2 and -d/2
         pos(:,i+3) = pos(:,i)+evec(:,1)
         pos(:,i+4) = pos(:,i)-evec(:,1)
         pos(:,i+5) = pos(:,i)+evec(:,2)
         pos(:,i+6) = pos(:,i)-evec(:,2)
         pos(:,i+7) = pos(:,i)+evec(:,3)
         pos(:,i+8) = pos(:,i)-evec(:,3)
         charge(i+3) = TWO*eval(1)
         charge(i+4) = TWO*eval(1)
         charge(i+5) = TWO*eval(2)
         charge(i+6) = TWO*eval(2)
         charge(i+7) = TWO*eval(3)
         charge(i+8) = TWO*eval(3)
         charge(i) = charge(i)-FOUR*eval(1)-FOUR*eval(2)-FOUR*eval(3)

!        ! Four charges in the plane at
!        ! (+d/2,+d/2), (-d/2,-d/2)
!        ! (+d/2,-d/2), (-d/2,+d/2)
!        pos(:,i+9 )  = pos(:,i) + unit(:,1) + unit(:,2)
!        pos(:,i+10)  = pos(:,i) - unit(:,1) - unit(:,2)
!        pos(:,i+11)  = pos(:,i) + unit(:,1) - unit(:,2)
!        pos(:,i+12)  = pos(:,i) - unit(:,1) + unit(:,2)
!        charge(i+9 ) =  quadrupole(1,2)
!        charge(i+10) =  quadrupole(1,2)
!        charge(i+11) = -quadrupole(1,2)
!        charge(i+12) = -quadrupole(1,2)
!        pos(:,i+13)  = pos(:,i) + unit(:,1) + unit(:,3)
!        pos(:,i+14)  = pos(:,i) - unit(:,1) - unit(:,3)
!        pos(:,i+15)  = pos(:,i) + unit(:,1) - unit(:,3)
!        pos(:,i+16)  = pos(:,i) - unit(:,1) + unit(:,3)
!        charge(i+13) =  quadrupole(1,3)
!        charge(i+14) =  quadrupole(1,3)
!        charge(i+15) = -quadrupole(1,3)
!        charge(i+16) = -quadrupole(1,3)
!        pos(:,i+17)  = pos(:,i) + unit(:,2) + unit(:,3)
!        pos(:,i+18)  = pos(:,i) - unit(:,2) - unit(:,3)
!        pos(:,i+19)  = pos(:,i) + unit(:,2) - unit(:,3)
!        pos(:,i+20)  = pos(:,i) - unit(:,2) + unit(:,3)
!        charge(i+17) =  quadrupole(2,3)
!        charge(i+18) =  quadrupole(2,3)
!        charge(i+19) = -quadrupole(2,3)
!        charge(i+20) = -quadrupole(2,3)

         ! Increment charge counter
         i = i+9
      end do

      ! Clean
      call destroy_(seitz)
   end subroutine

!  ===========
!  Add tensors
!  ===========

   PURE subroutine add_UC_tensors_0(self,tensor)
   ! For a unit cell, add up a "tensor" property for one molecule, assumed to be
   ! the fragment_geometry.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,3,3), OUT :: tensor
      VEC(INT,:), allocatable :: ulist
      MAT3(REAL,:,:,:), allocatable :: seitz
      MAT(REAL,3,3) :: tensor0,tensor1
      INT :: m,a,u,s
   ENSURE(self%generation_method=="unit_cell","CRYSTAL:add_UC_tensors_0 ... only for unit_cell clusters")
   ENSURE(self%n_atoms>0,"CRYSTAL:add_UC_tensors_0 ... no atoms in cluster")
   ENSURE(allocated(self%occupation_list),"CRYSTAL:add_UC_tensors_0 ... no occupation_list")
   ENSURE(self%fragment_info_made,"CRYSTAL:add_UC_tensors_0 ... no crystal fragment info")
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:add_UC_tensors_0 ... no cell_mol_for_atom")

      ! The following code is inherited from CRYSTAL
      tensor0 = tensor
      tensor = ZERO

      ! Unit cell atoms which correspond to the fragment atoms
      call create_(ulist,self%n_atoms)
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)

      ! Inverse XYZ symops
      seitz = transposed_xyz_seitz_matrices_(self)

      ! stdout.text("Making unit cell tensor")
      ! stdout.text("original tensor")
      ! stdout.put(tensor0)
      do m = 1,self%n_unit_cell_mols
         do a = 1,self%n_atoms
            if (self%unit_cell_mol_for_atom(a)/=m) cycle

         ! Get symop for this molecule
            u = ulist(a)
            s = self%asym_symop_for_unit_cell_atom(u)

         ! Use inverse symop  S^T to change U tensor basis
            call change_basis_to_(tensor0,tensor1,seitz(:,:,s))
            tensor = tensor+tensor1

         ! stdout.show("molecule =",m)
         ! stdout.show("symop    =",s)
         ! stdout.put(seitz(:,:,s))
         ! stdout.text("transformed tensor")
         ! stdout.put(tensor1)
            exit
         end do
      end do

      ! Clean
      call destroy_(seitz)
      call destroy_(ulist)
   end subroutine

   PURE subroutine add_UC_tensors_1(self,tensor)
   ! For a unit cell, add up a "tensor" property for one molecule, assumed to be
   ! the fragment_geometry.
      type(CRYSTAL_TYPE), IN :: self
      MAT3(REAL,3,3,3), OUT :: tensor
      VEC(INT,:), allocatable :: ulist
      MAT3(REAL,:,:,:), allocatable :: seitz
      MAT3(REAL,3,3,3) :: tensor0,tensor1
      INT :: m,a,u,s
   ENSURE(self%generation_method=="unit_cell","CRYSTAL:add_UC_tensors_1 ... only for unit_cell clusters")
   ENSURE(self%n_atoms>0,"CRYSTAL:add_UC_tensors_1 ... no atoms in cluster")
   ENSURE(allocated(self%occupation_list),"CRYSTAL:add_UC_tensors_1 ... no occupation_list")
   ENSURE(self%fragment_info_made,"CRYSTAL:add_UC_tensors_1 ... no crystal fragment info")
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:add_UC_tensors_1 ... no cell_mol_for_atom")

      ! The following code is inherited from CRYSTAL
      tensor0 = tensor
      tensor = ZERO

      ! Unit cell atoms which correspond to the fragment atoms
      call create_(ulist,self%n_atoms)
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)

      ! Inverse XYZ symops
      seitz = transposed_xyz_seitz_matrices_(self)

      ! stdout.text("Making unit cell tensor")
      ! stdout.text("original tensor")
      ! stdout.put(tensor0)
      do m = 1,self%n_unit_cell_mols
         do a = 1,self%n_atoms
            if (self%unit_cell_mol_for_atom(a)/=m) cycle

         ! Get symop for this molecule
            u = ulist(a)
            s = self%asym_symop_for_unit_cell_atom(u)

         ! Use inverse symop  S^T to change U tensor basis
            call change_basis_to_(tensor0,tensor1,seitz(:,:,s))
            tensor = tensor+tensor1

         ! stdout.show("molecule =",m)
         ! stdout.show("symop    =",s)
         ! stdout.put(seitz(:,:,s))
         ! stdout.text("transformed tensor")
         ! stdout.put(tensor1)
            exit
         end do
      end do

      ! Clean
      call destroy_(seitz)
      call destroy_(ulist)
   end subroutine


!  ==========================
!  Local field factor tensors
!  ==========================

   subroutine put_averaged_LFF_tensors(self)
   ! Put the heavy-atom averaged local field factor dipole L tensors
   ! for the molecules in the unit cell. This uses the Reis formula.
      type(CRYSTAL_TYPE), IN :: self
      INT :: n_mol,n_H,n,a,i1,i2,j1,j2,k1,k2,m1,m2
      MAT(REAL,:,:), allocatable :: pos,pos0,com,L,L0,L1
      VEC(INT,:), allocatable :: symop,non_H,par,mol,mol0
      VEC(STR(len=STR_SIZE),:), allocatable :: label0
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:put_averaged_LFF_tensors ... no cell_mol_for_atom")
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:put_averaged_LFF_tensors ... no asymetric cell atoms")

      ! Header
      call flush_(stdout)
      call text_(stdout,"Molecule-averaged Dipole L(1) tensor:")

      ! Get the positions, COM's for the molecules in the unit cell.
      ! By construction, the atoms in each molecule occur consecutively.
      call get_uc_molecule_info_(self,mol,symop,par,pos,com)

      ! Get indices of non-Hydrogen-atom positions.
      n_mol = self%n_unit_cell_mols
      n_H = count(self%asymmetric_unit_atom(par)%atomic_number>1)
      call create_(non_H,n_H)
      non_H = pack([(a,a = 1,size(par))],self%asymmetric_unit_atom(par)%atomic_number>1)

      ! Set the non-Hydrogen-atom positions, molecules.
      call create_(label0,n_H)
      call create_(pos0,3,n_H)
      call create_(mol0,n_H)
      label0 = self%asymmetric_unit_atom(par(non_H))%label
      pos0 = pos(:,non_H)
      mol0 = mol(non_H)
      call destroy_(non_H)
      call destroy_(com); call destroy_(pos); call destroy_(par); call destroy_(symop); call destroy_(mol)

      ! Make the non-Hydrogen-atom Lorentz factor tensors
      call create_(L,3*n_mol,3*n_mol)
      call create_(L0,3*n_H,n_H)  ! charge L tensor
      call create_(L1,3*n_H,3*n_H)  ! dipole L tensor
      call make_LFF_tensors_(self%unit_cell,L0,L1,pos0,mol0)
      call destroy_(mol0); call destroy_(pos0)

      ! Print monopole and dipole L tensor out
      n = n_H/n_mol
      call flush_(stdout)
      call text_(stdout,"Heavy-atom monopole L(0) tensor and Dipole L(1) tensor:")
      call flush_(stdout)
      call dash_(stdout,int_fields=4,real_fields=4)
      call put_(stdout,"Mol. 1",int_width=TRUE)
      call put_(stdout,"Mol. 2",int_width=TRUE)
      call put_(stdout,"Atom 1",int_width=TRUE)
      call put_(stdout,"Atom 2",int_width=TRUE)
      call put_(stdout,"L(0)")
      call tab_(stdout,real_fields=1)
      call put_(stdout,"L(1)")
      call flush_(stdout)
      call dash_(stdout,int_fields=4,real_fields=4)
      do i1 = 1,n_mol
         do i2 = 1,n_mol
            do j1 = 1,n
               do j2 = 1,n
                  k1 = n*(i1-1)+j1
                  k2 = n*(i2-1)+j2
                  m1 = 3*(k1-1)
                  m2 = 3*(k2-1)
                  call put_(stdout,i1)
                  call put_(stdout,i2)
                  call put_(stdout,trim(to_str_(j1))//" "//trim(label0(k1)),int_width=TRUE)
                  call put_(stdout,trim(to_str_(j2))//" "//trim(label0(k2)),int_width=TRUE)
                  call put_(stdout,L0(m1+1,k2))
                  call put_(stdout,L1(m1+1,m2+1))
                  call put_(stdout,L1(m1+1,m2+2))
                  call put_(stdout,L1(m1+1,m2+3))
                  call flush_(stdout); call tab_(stdout,int_fields=4)
                  call put_(stdout,L0(m1+2,k2))
                  call put_(stdout,L1(m1+2,m2+1))
                  call put_(stdout,L1(m1+2,m2+2))
                  call put_(stdout,L1(m1+2,m2+3))
                  call flush_(stdout); call tab_(stdout,int_fields=4)
                  call put_(stdout,L0(m1+3,k2))
                  call put_(stdout,L1(m1+3,m2+1))
                  call put_(stdout,L1(m1+3,m2+2))
                  call put_(stdout,L1(m1+3,m2+3))
                  call flush_(stdout)
               end do
            end do
         end do
      end do
      call dash_(stdout,int_fields=4,real_fields=4)
      call destroy_(label0)

      ! Average the L(1) tensors over molecules
      L = ZERO
      do i1 = 1,n_mol
         do i2 = 1,n_mol
            m1 = 3*(i1-1)
            m2 = 3*(i2-1)
            do j1 = 1,n
               do j2 = 1,n
                  k1 = 3*(n*(i1-1)+j1-1)
                  k2 = 3*(n*(i2-1)+j2-1)
                  L(m1+1:m1+3,m2+1:m2+3) = L(m1+1:m1+3,m2+1:m2+3)+L1(k1+1:k1+3,k2+1:k2+3)
               end do
            end do
         end do
      end do
      L = L/real(n*n,kind=REAL_KIND)

      ! Print averaged dipole L tensor
      call flush_(stdout)
      call text_(stdout,"Spackman's molecule-averaged Dipole L(1) tensor:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      call put_(stdout,"Mol. 1",int_width=TRUE)
      call put_(stdout,"Mol. 2",int_width=TRUE)
      call tab_(stdout,real_fields=1)
      call put_(stdout,"L(1)")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      do k1 = 1,n_mol
         do k2 = 1,n_mol
            m1 = 3*(k1-1)
            m2 = 3*(k2-1)
            call put_(stdout,k1)
            call put_(stdout,k2)
            call put_(stdout,L(m1+1,m2+1))
            call put_(stdout,L(m1+1,m2+2))
            call put_(stdout,L(m1+1,m2+3))
            call flush_(stdout); call tab_(stdout,int_fields=2)
            call put_(stdout,L(m1+2,m2+1))
            call put_(stdout,L(m1+2,m2+2))
            call put_(stdout,L(m1+2,m2+3))
            call flush_(stdout); call tab_(stdout,int_fields=2)
            call put_(stdout,L(m1+3,m2+1))
            call put_(stdout,L(m1+3,m2+2))
            call put_(stdout,L(m1+3,m2+3))
            call flush_(stdout)
         end do
      end do
      call dash_(stdout,int_fields=2,real_fields=3)

      ! Clean up
      call destroy_(L1)
      call destroy_(L0)
      call destroy_(L)
   end subroutine

   subroutine put_centroid_LFF_tensors(self)
   ! Put the local field factor L tensors at the centroids of all
   ! molecules in the unit cell. This uses the Reis formula.
      type(CRYSTAL_TYPE), IN :: self
      INT :: n,i,k1,k2,m1,m2
      MAT(REAL,:,:), allocatable :: L0,L1
      MAT(REAL,:,:), allocatable :: centroid,W
      VEC(INT,:), allocatable :: mol_for
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:put_centroid_LFF_tensors ... no cell_mol_for_atom")

      ! Header
      call flush_(stdout)
      call text_(stdout,"Local Field Factor L tensors:")
      call flush_(stdout)
      call show_(stdout,"n_unit_cell_mols =",self%n_unit_cell_mols)

      ! Allocate
      n = self%n_unit_cell_mols
      call create_(L0,3*n,n)
      call create_(L1,3*n,3*n)

      ! Get centroids
      call create_(centroid,3,n)
      call get_uc_molecule_centroids_(self,centroid)
      call flush_(stdout)
      call text_(stdout,"Molecule centroids (cell coordinates):")
      call flush_(stdout)
      call put_(stdout,transpose(centroid))

      ! Change centroids into cartesian
      W = centroid
      call to_product_of_(centroid,self%unit_cell%direct_mx,W)
      call destroy_(W)
      call flush_(stdout)
      call text_(stdout,"Molecule centroids (cartesian/Bohr):")
      call flush_(stdout)
      call put_(stdout,transpose(centroid))

      ! Make L tensor
      call create_(mol_for,n); mol_for = [(i,i = 1,n)]
      call make_LFF_tensors_(self%unit_cell,L0,L1,centroid,mol_for)
      call destroy_(mol_for)
      call destroy_(centroid)

      ! Print monopole and dipole L tensor out
      call flush_(stdout)
      call text_(stdout,"Monopole L(0) tensor and Dipole L(1) tensor:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=4)
      call put_(stdout,"k1",int_width=TRUE)
      call put_(stdout,"k2",int_width=TRUE)
      call put_(stdout,"L(0)")
      call tab_(stdout,real_fields=1)
      call put_(stdout,"L(1)")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=4)
      do k1 = 1,n
         do k2 = 1,k1
            m1 = 3*(k1-1)
            m2 = 3*(k2-1)
            call put_(stdout,k1)
            call put_(stdout,k2)
            call put_(stdout,L0(m1+1,k2))
            call put_(stdout,L1(m1+1,m2+1))
            call put_(stdout,L1(m1+1,m2+2))
            call put_(stdout,L1(m1+1,m2+3))
            call flush_(stdout); call tab_(stdout,int_fields=2)
            call put_(stdout,L0(m1+2,k2))
            call put_(stdout,L1(m1+2,m2+1))
            call put_(stdout,L1(m1+2,m2+2))
            call put_(stdout,L1(m1+2,m2+3))
            call flush_(stdout); call tab_(stdout,int_fields=2)
            call put_(stdout,L0(m1+3,k2))
            call put_(stdout,L1(m1+3,m2+1))
            call put_(stdout,L1(m1+3,m2+2))
            call put_(stdout,L1(m1+3,m2+3))
            call flush_(stdout)
         end do
      end do
      call dash_(stdout,int_fields=2,real_fields=4)

      ! Clean up
      call destroy_(L1)
      call destroy_(L0)
   end subroutine

   subroutine put_centroid_LFF_tensors_old(self)
   ! Put the local field factor L tensors at the centroids of all
   ! molecules in the unit cell. This uses the Cummins code.
      type(CRYSTAL_TYPE), IN :: self
      INT :: n,k,k1,k2
      MAT3(REAL,:,:,:), allocatable :: L2
      MAT(REAL,:,:), allocatable :: centroid,W
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:put_centroid_LFF_tensors_old ... no cell_mol_for_atom")

      ! Header
      call flush_(stdout)
      call text_(stdout,"Local Field Factor L tensors:")
      call flush_(stdout)
      call show_(stdout,"n_unit_cell_mols =",self%n_unit_cell_mols)

      ! Allocate space
      n = self%n_unit_cell_mols
      call create_(L2,3,3,triangle_(n))
      call create_(centroid,3,n)

      ! Get unit cell molecule centroids
      call get_uc_molecule_centroids_(self,centroid)
      call flush_(stdout)
      call text_(stdout,"Molecule centroids (cell coordinates):")
      call flush_(stdout)
      call put_(stdout,transpose(centroid))

      ! Change centroids into cartesian
      W = centroid
      call to_product_of_(centroid,self%unit_cell%direct_mx,W)
      call destroy_(W)
      call flush_(stdout)
      call text_(stdout,"Molecule centroids (cartesian/Bohr):")
      call flush_(stdout)
      call put_(stdout,transpose(centroid))

      ! Make L tensor
      call make_LFF_tensors_(self%unit_cell,L2,centroid)

      ! Print L tensor out
      call flush_(stdout)
      call text_(stdout,"L tensors:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=6)
      call put_(stdout,"k1",int_width=TRUE)
      call put_(stdout,"k2",int_width=TRUE)
      call put_(stdout,"L(xx)")
      call put_(stdout,"L(yy)")
      call put_(stdout,"L(zz)")
      call put_(stdout,"L(xy)")
      call put_(stdout,"L(xz)")
      call put_(stdout,"L(yz)")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=6)
      k = 0
      do k1 = 1,n
         do k2 = 1,k1
            k = k+1
            call put_(stdout,k1)
            call put_(stdout,k2)
            call put_(stdout,L2(1,1,k))
            call put_(stdout,L2(2,2,k))
            call put_(stdout,L2(3,3,k))
            call put_(stdout,L2(1,2,k))
            call put_(stdout,L2(1,3,k))
            call put_(stdout,L2(2,3,k))
            call flush_(stdout)
         end do
      end do
      call dash_(stdout,int_fields=2,real_fields=6)

      ! Clean up
      call destroy_(centroid)
      call destroy_(L2)
   end subroutine

   PURE subroutine get_uc_molecule_centroids(self,centroid)
   ! Get the unit cell molecule centroids; they are transformed back
   ! into the unit cell if the defragmented molecule lies outside the
   ! unit cell.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: centroid
      INT :: m,u
      MAT(REAL,:,:), allocatable :: geometry
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:get_uc_molecule_centroids ... no cell_mol_for_atom")
   ENSURE(all(shape(centroid)==[3,self%n_unit_cell_mols]),"CRYSTAL:get_uc_molecule_centroids ... wrong shape, centroid")
      centroid = ZERO
      do m = 1,self%n_unit_cell_mols
         do u = 1,self%n_unit_cell_atoms
            if (self%unit_cell_mol_for_atom(u)/=m) cycle
            call do_defragment_uc_atom_(self,u,geometry)
            exit
         end do
         centroid(:,m) = sum_column_vectors_(geometry)/size(geometry,2)
         call put_to_unit_cell_(self,centroid(:,m))

         ! Clean
         call destroy_(geometry)
      end do
   end subroutine

   subroutine put_LFF_info(self)
   ! Put the local field factor L tensors at the centroids of all
   ! molecules in the unit cell.
      type(CRYSTAL_TYPE), IN :: self
      INT :: n,k1,k2,m1,m2
      MAT(REAL,:,:), allocatable :: L0,L1
      VEC(INT,:), allocatable :: mol,par
      MAT(REAL,:,:), allocatable :: pos,coc,com,alpha_q
      MAT3(REAL,:,:,:), allocatable :: symop,alpha_m
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:put_LFF_info ... no cell_mol_for_atom")

      ! Get the unit cell molecule atom info
      call get_uc_molecule_info_(self,mol,symop,par,pos,coc,com,alpha_q,alpha_m)

      ! Make the charge and dipole Lorentz factor tensors
      n = size(par)
      call create_(L0,3*n,n)
      call create_(L1,3*n,3*n)
      call make_LFF_tensors_(self%unit_cell,L0,L1,pos,mol)

      ! Output
      call flush_(stdout)
      call text_(stdout,"Local Field Factor L tensors:")
      call flush_(stdout)
      call show_(stdout,"n_unit_cell_atoms =",size(par))
      call show_(stdout,"n_unit_cell_mols  =",size(coc,2))
      call flush_(stdout)
      call text_(stdout,"Molecule atom positions:")
      call flush_(stdout)
      call put_(stdout,transpose(pos))
      call flush_(stdout)
      call text_(stdout,"L tensors:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=6)
      call put_(stdout,"k1",int_width=TRUE)
      call put_(stdout,"k2",int_width=TRUE)
      call put_(stdout,"L(xx)")
      call put_(stdout,"L(yy)")
      call put_(stdout,"L(zz)")
      call put_(stdout,"L(xy)")
      call put_(stdout,"L(xz)")
      call put_(stdout,"L(yz)")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=6)
      do k1 = 1,n
         do k2 = 1,k1
            m1 = 3*(k1-1)
            m2 = 3*(k2-1)
            call put_(stdout,k1)
            call put_(stdout,k2)
            call put_(stdout,L1(m1+1,m2+1))
            call put_(stdout,L1(m1+2,m2+2))
            call put_(stdout,L1(m1+3,m2+3))
            call put_(stdout,L1(m1+1,m2+2))
            call put_(stdout,L1(m1+1,m2+3))
            call put_(stdout,L1(m1+2,m2+3))
            call flush_(stdout)
         end do
      end do
      call dash_(stdout,int_fields=2,real_fields=6)

      ! Clean up
      call destroy_(L1)
      call destroy_(L0)
      call destroy_(alpha_m)
      call destroy_(alpha_q)
      call destroy_(com)
      call destroy_(coc)
      call destroy_(par)
      call destroy_(symop)
      call destroy_(mol)
   end subroutine


!  ======================
!  Susceptibility tensors
!  ======================

   subroutine make_chi1_naive(self,chi1,alpha_mol)
   ! Make the naive second order susceptibility from the
   ! polarisability of the molecule.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,3,3), OUT :: chi1  ! Clausius-Mossotti, L=1/3
      MAT(REAL,3,3), IN :: alpha_mol
      MAT(REAL,3,3) :: alpha_uc
      MAT(REAL,:,:), allocatable :: D
      REAL :: fac
      call make_alpha_uc_(self,alpha_uc,alpha_mol)

      ! fac = factor to reduce polarisabilities = 1/(eps_0 V) (in au)
      fac = FOUR*PI/self%unit_cell%volume
      call create_(D,3,3)
      call to_unit_mx_(D)
      D = D-fac*THIRD*alpha_uc
      call to_inverse_of_(D,D)  ! NOT PURE
      call to_scaled_product_of_(chi1,alpha_uc,D,fac)
      call destroy_(D)
   end subroutine

   PURE subroutine make_alpha_uc(self,alpha_uc,alpha_mol)
   ! Make the unit cell alpha.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,3,3), OUT :: alpha_uc
      MAT(REAL,3,3), IN :: alpha_mol
      MAT(REAL,3,3) :: alpha
      VEC(INT,:), allocatable :: mol,symop,par
      MAT(REAL,:,:), allocatable :: pos,com
      MAT3(REAL,:,:,:), allocatable :: inverse_seitz
      INT :: n_mol,m

      ! Get the unit cell molecule atom info
      call get_uc_molecule_info_(self,mol,symop,par,pos,com)
      call destroy_(com)
      call destroy_(pos)
      call destroy_(par)
      call destroy_(mol)

      ! No of molecules
      n_mol = size(symop)

      ! Transposed/Inverse symops
      inverse_seitz = transposed_xyz_seitz_matrices_(self)

      ! Add up the polarisabilities
      alpha_uc = ZERO
      do m = 1,n_mol
         call change_basis_to_(alpha_mol,alpha,inverse_seitz(:,:,symop(m)))
         alpha_uc = alpha_uc+alpha
      end do

      ! Clean
      call destroy_(inverse_seitz)
      call destroy_(symop)
   end subroutine

   subroutine make_chi1_ALFFA(self,chi1,alpha_mol)
   ! Make the ALFFA "chi1" matrix: chi = Tr_block a^1(1 - L^1 a^1)^-1
   ! where L^1 is the dipole L tensor, a^1 is the (symmetry
   ! transformed) molecular dipole polarisability "alpha_mol". The L
   ! tensors are evaluated at the molecular center-of_masses.
   ! The trace is over 3x3 blocks.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,3,3), OUT :: chi1
      MAT(REAL,3,3), IN :: alpha_mol
      VEC(INT,:), allocatable :: mol,mol0,symop,par
      MAT3(REAL,:,:,:), allocatable :: inverse_seitz
      MAT3(REAL,:,:,:), allocatable :: alpha
      MAT(REAL,:,:), allocatable :: pos,com,L0,L1,D,Df
      MAT(REAL,3,3) :: alph
      INT :: n_mol,dim,a,b,i,j,m
      REAL :: fac

      ! No. of molecules
      n_mol = self%n_unit_cell_mols

      ! Molecular polarisabilities
      call create_(alpha,3,3,n_mol)

      ! D matrix: D = (1 - aL)^-1
      dim = 3*n_mol
      call create_(D,dim,dim)

      ! Get the unit cell molecule atom info
      call get_uc_molecule_info_(self,mol,symop,par,pos,com)

      ! Get the molecule indices (they are consecutive by construction)
      call create_(mol0,n_mol)
      mol0 = [(m,m = 1,n_mol)]

      ! Print out centroid info ... important
      call flush_(stdout)
      call text_(stdout,"Center of mass:")
      call put_(stdout,com(:,1))

      ! Make the centroid charge and dipole Lorentz factor tensors
      call create_(L0,dim,n_mol)
      call create_(L1,dim,dim)
      call make_LFF_tensors_(self%unit_cell,L0,L1,com,mol0)

      ! Get the reduced molecular polarisabilities
      ! fac = factor to reduce polarisabilities = 1/(eps_0 V) (in au)
      inverse_seitz = transposed_xyz_seitz_matrices_(self)
      fac = FOUR*PI/self%unit_cell%volume
      alph = fac*alpha_mol
      do m = 1,n_mol
         call change_basis_to_(alph,alpha(:,:,m),inverse_seitz(:,:,symop(m)))
      end do
      call destroy_(inverse_seitz)

      ! Make the (inverse) D matrix
      call to_unit_mx_(D)
      i = 0
      do a = 1,n_mol
         j = 0
         do b = 1,n_mol
            call plus_scaled_product_of_(D(i+1:i+3,j+1:j+3),L1(i+1:i+3,j+1:j+3),alpha(:,:,b),-ONE)
            j = j+3
         end do
         i = i+3
      end do

      ! Clean up
      call destroy_(L1)
      call destroy_(L0)
      call destroy_(mol0)
      call destroy_(com)
      call destroy_(pos)
      call destroy_(par)
      call destroy_(symop)
      call destroy_(mol)

      ! Invert
      call to_inverse_of_(D,D)  ! NOT PURE

      ! Make the local field factors
      call create_(Df,dim,3)
      call make_local_field_factors(Df,D)

      ! Make the chi1 (ALFFA) susceptibility
      chi1 = ZERO
      i = 0
      do a = 1,n_mol
         call plus_product_of_(chi1,alpha(:,:,a),Df(i+1:i+3,:))
         i = i+3
      end do

      ! Clean up
      call destroy_(Df)
      call destroy_(D)
      call destroy_(alpha)
   end subroutine

   subroutine make_chi1_RLFTn(self,chi1,alpha_mol)
   ! Make the RLFTn "chi1" matrix: chi1 = Tr_block a^1(1 - L^1 a^1)^-1
   ! where L^1 is the dipole L tensor, a^1 is the (symmetry
   ! transformed) molecular dipole polarisability "alpha_mol" divided
   ! by the number of non-Hydrogen atoms in a molecule.  The L tensors
   ! are evaluated at these non H atom sites. The trace is over 3x3 blocks
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,3,3), OUT :: chi1
      MAT(REAL,3,3), IN :: alpha_mol
      VEC(INT,:), allocatable :: mol,mol0,symop,non_H,par
      MAT3(REAL,:,:,:), allocatable :: inverse_seitz
      MAT3(REAL,:,:,:), allocatable :: alpha
      MAT(REAL,:,:), allocatable :: pos,pos0,com,L0,L1,D,Df
      MAT(REAL,3,3) :: alph
      INT :: n_mol,n_non_H,n_non_H_per_mol
      INT :: n,dim,a,b,i,j,m
      REAL :: fac
      n = self%n_unit_cell_atoms  ! No. of atoms in unit cell
      n_mol = self%n_unit_cell_mols  ! No. of molecules

      ! No of non-hydrogen atoms in unit cell
      n_non_H = count(self%asymmetric_unit_atom(self%asym_atom_for_unit_cell_atom)%atomic_number>1)

      ! No. of non-H hydrogen atoms per molecule
      ENSURE(mod(n_non_H,n_mol)==0,"CRYSTAL:make_chi1_RLFTn ... no of non-Hydrogen atoms wrong")
      n_non_H_per_mol = n_non_H/n_mol

      ! Molecular polarisabilities
      call create_(alpha,3,3,n_mol)

      ! D matrix: D = (1 - aL)^-1
      dim = 3*n_non_H
      call create_(D,dim,dim)

      ! Get the unit cell molecule atom info
      call get_uc_molecule_info_(self,mol,symop,par,pos,com)

      ! Get the reduced averaged site polarisabilities
      ! fac = factor to reduce polarisabilities = 1/(eps_0 V) (in au)
      inverse_seitz = transposed_xyz_seitz_matrices_(self)
      fac = FOUR*PI/self%unit_cell%volume
      alph = fac*alpha_mol/n_non_H_per_mol
      do m = 1,n_mol
         call change_basis_to_(alph,alpha(:,:,m),inverse_seitz(:,:,symop(m)))
      end do
      call destroy_(inverse_seitz)

      ! Get *indices* for non-H positions
      call create_(non_H,n_non_H)
      non_H = pack([(a,a = 1,n)],self%asymmetric_unit_atom(par)%atomic_number>1)

      ! Get the non-H positions and molecules
      call create_(pos0,3,n_non_H)
      call create_(mol0,n_non_H)
      pos0 = pos(:,non_H)
      mol0 = mol(non_H)

      ! Make the non H charge and dipole Lorentz factor tensors
      call create_(L0,dim,n_non_H)  ! charge L tensor
      call create_(L1,dim,dim)  ! dipole L tensor
      call make_LFF_tensors_(self%unit_cell,L0,L1,pos0,mol0)

      ! Make the (inverse) D matrix: D = (1 - aL)^-1
      call to_unit_mx_(D)
      i = 0
      do a = 1,n_non_H
         j = 0
         do b = 1,n_non_H
            m = (b-1)/n_non_H_per_mol+1
            call plus_scaled_product_of_(D(i+1:i+3,j+1:j+3),L1(i+1:i+3,j+1:j+3),alpha(:,:,m),-ONE)
            j = j+3
         end do
         i = i+3
      end do

      ! Clean up
      call destroy_(L1)
      call destroy_(L0)
      call destroy_(mol0)
      call destroy_(pos0)
      call destroy_(non_H)
      call destroy_(com)
      call destroy_(pos)
      call destroy_(par)
      call destroy_(symop)
      call destroy_(mol)

      ! Invert
      call to_inverse_of_(D,D)  ! NOT PURE

      ! Make the local field factors
      call create_(Df,dim,3)
      call make_local_field_factors(Df,D)

      ! Make the chi1 (RLFTn) susceptibility
      chi1 = ZERO
      i = 0
      do a = 1,n_non_H
         m = (a-1)/n_non_H_per_mol+1
         call plus_product_of_(chi1,alpha(:,:,m),Df(i+1:i+3,:))
         i = i+3
      end do

      ! Clean up
      call destroy_(Df)
      call destroy_(D)
      call destroy_(alpha)
   end subroutine

   subroutine make_chi1_RLFTn_av_L(self,chi1,alpha_mol)
   ! Make the RLFTn "chi1" matrix: chi1 = Tr_block a^1(1 - L^1 a^1)^-1
   ! where L^1 is the dipole L tensor, a^1 is the (symmetry
   ! transformed) molecular dipole polarisability "alpha_mol" divided
   ! by the number of non-Hydrogen atoms in a molecule.  The L tensors
   ! are evaluated at these non H atom sites. The trace is over 3x3 blocks
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,3,3), OUT :: chi1
      MAT(REAL,3,3), IN :: alpha_mol
      VEC(INT,:), allocatable :: mol,mol0,symop,par,non_H
      MAT3(REAL,:,:,:), allocatable :: inverse_seitz
      MAT3(REAL,:,:,:), allocatable :: alpha
      MAT(REAL,:,:), allocatable :: pos,pos0,com,L,L0,L1,D,Df
      MAT(REAL,3,3) :: alph
      INT :: n,n_mol,n_non_H,n_non_H_per_mol,dim
      INT :: i1,i2,j1,j2,k1,k2,m1,m2,a,b,i,j,m
      REAL :: fac
      n = self%n_unit_cell_atoms  ! No. of atoms in unit cell
      n_mol = self%n_unit_cell_mols  ! No. of molecules

      ! Molecular polarisabilities
      call create_(alpha,3,3,n_mol)

      ! Molecule averaged L1
      dim = 3*n_mol
      call create_(L,dim,dim)

      ! Get the unit cell molecule atom info
      call get_uc_molecule_info_(self,mol,symop,par,pos,com)

      ! Get *indices* for non-H positions
      n_non_H = count(self%asymmetric_unit_atom(par)%atomic_number>1)
      call create_(non_H,n_non_H)
      non_H = pack([(a,a = 1,n)],self%asymmetric_unit_atom(par)%atomic_number>1)

      ! No. of non-H hydrogen atoms per molecule
      ENSURE(mod(n_non_H,n_mol)==0,"CRYSTAL:make_chi1_RLFTn_av_L ... no of non-Hydrogen atoms wrong")
      n_non_H_per_mol = n_non_H/n_mol

      ! Get the non-H positions and molecules
      call create_(pos0,3,n_non_H)
      call create_(mol0,n_non_H)
      pos0 = pos(:,non_H)
      mol0 = mol(non_H)

      ! Get the reduced molecular polarisabilities
      ! fac = factor to reduce polarisabilities = 1/(eps_0 V) (in au)
      inverse_seitz = transposed_xyz_seitz_matrices_(self)
      fac = FOUR*PI/self%unit_cell%volume
      alph = fac*alpha_mol
      do m = 1,n_mol
         call change_basis_to_(alph,alpha(:,:,m),inverse_seitz(:,:,symop(m)))
      end do
      call destroy_(inverse_seitz)

      ! Make the non H charge and dipole Lorentz factor tensors
      dim = 3*n_non_H
      call create_(L0,dim,n_non_H)  ! charge L tensor
      call create_(L1,dim,dim)  ! dipole L tensor
      call make_LFF_tensors_(self%unit_cell,L0,L1,pos0,mol0)

      ! Average the L(1) tensors over molecules
      L = ZERO
      do i1 = 1,n_mol
         do i2 = 1,n_mol
            m1 = 3*(i1-1)
            m2 = 3*(i2-1)
            do j1 = 1,n_non_H_per_mol
               do j2 = 1,n_non_H_per_mol
                  k1 = 3*(n_non_H_per_mol*(i1-1)+j1-1)
                  k2 = 3*(n_non_H_per_mol*(i2-1)+j2-1)
                  L(m1+1:m1+3,m2+1:m2+3) = L(m1+1:m1+3,m2+1:m2+3)+L1(k1+1:k1+3,k2+1:k2+3)
               end do
            end do
         end do
      end do
      fac = ONE/(n_non_H_per_mol*n_non_H_per_mol)
      L = fac*L

      ! Mini Clean up
      call destroy_(L1); call destroy_(L0)
      call destroy_(mol0); call destroy_(pos0); call destroy_(non_H)
      call destroy_(com); call destroy_(pos); call destroy_(par); call destroy_(symop); call destroy_(mol)

      ! Make the (inverse) D matrix: D = (1 - aL)^-1
      dim = 3*n_mol
      call create_(D,dim,dim)
      call to_unit_mx_(D)
      i = 0
      do a = 1,n_mol
         j = 0
         do b = 1,n_mol
            call plus_scaled_product_of_(D(i+1:i+3,j+1:j+3),L(i+1:i+3,j+1:j+3),alpha(:,:,b),-ONE)
            j = j+3
         end do
         i = i+3
      end do

      ! Invert
      call to_inverse_of_(D,D)  ! NOT PURE

      ! Make the local field factors
      call create_(Df,dim,3)
      call make_local_field_factors(Df,D)

      ! Make the chi1 (ALFFA-like) susceptibility
      chi1 = ZERO
      i = 0
      do a = 1,n_mol
         call plus_product_of_(chi1,alpha(:,:,a),Df(i+1:i+3,:))
         i = i+3
      end do

      ! Clean up
      call destroy_(Df); call destroy_(D)
      call destroy_(L); call destroy_(alpha)
   end subroutine

   subroutine make_chi2_RLFTn_av_L(self,chi2,beta_mol,alpha_mol)
   ! Make the RLFTn "chi2" matrix:
   !   chi2_ijk = \sum_S \beta^S_abc d^S_ai d^S_bj d^S_ck
   ! where d^S are the local field factor tensors and \beta^S are the (symmetry
   ! transformed) molecular dipole site-hyperpolarisability "beta_mol" divided
   ! by the number of non-Hydrogen atoms in a molecule.  The L tensors
   ! are evaluated at these non H atom sites, "S".
      type(CRYSTAL_TYPE), IN :: self
      MAT3(REAL,3,3,3), OUT :: chi2
      MAT3(REAL,3,3,3), IN :: beta_mol
      MAT(REAL,3,3), IN :: alpha_mol
      MAT(REAL,3,3) :: alph0
      MAT3(REAL,3,3,3) :: beta0
      MAT3(REAL,:,:,:), allocatable :: inverse_seitz
      MAT3(REAL,:,:,:), allocatable :: alpha
      MAT4(REAL,:,:,:,:), allocatable :: beta
      MAT(REAL,:,:), allocatable :: pos,pos0,com,L,L0,L1,D,Df
      VEC(INT,:), allocatable :: mol,mol0,symop,par,non_H
      INT :: n,n_mol,n_non_H,n_non_H_per_mol,dim
      INT :: i1,i2,j1,j2,k1,k2,m1,m2,a,b,i,j,m
      REAL :: fac
      n = self%n_unit_cell_atoms  ! No. of atoms in unit cell
      n_mol = self%n_unit_cell_mols  ! No. of molecules

      ! Molecular polarisabilities
      call create_(alpha,3,3,n_mol)
      call create_(beta,3,3,3,n_mol)

      ! Molecule averaged L1
      dim = 3*n_mol
      call create_(L,dim,dim)

      ! Get the unit cell molecule atom info
      call get_uc_molecule_info_(self,mol,symop,par,pos,com)

      ! Get *indices* for non-H positions
      n_non_H = count(self%asymmetric_unit_atom(par)%atomic_number>1)
      call create_(non_H,n_non_H)
      non_H = pack([(a,a = 1,n)],self%asymmetric_unit_atom(par)%atomic_number>1)

      ! No. of non-H hydrogen atoms per molecule
      ENSURE(mod(n_non_H,n_mol)==0,"CRYSTAL:make_chi2_RLFTn_av_L ... no of non-Hydrogen atoms wrong")
      n_non_H_per_mol = n_non_H/n_mol

      ! Get the non-H positions and molecules
      call create_(pos0,3,n_non_H)
      call create_(mol0,n_non_H)
      pos0 = pos(:,non_H)
      mol0 = mol(non_H)

      ! Get the reduced molecular polarisabilities
      ! fac = factor to reduce polarisabilities = 1/(2 eps_0 V)
      !     = (2 pi/V) ... in au since eps_0 = (4 pi)^{-1}.
      inverse_seitz = transposed_xyz_seitz_matrices_(self)
      fac = TWO*PI/self%unit_cell%volume
      alph0 = TWO*fac*alpha_mol
      beta0 = fac*beta_mol
      do m = 1,n_mol
         call change_basis_to_(alph0,alpha(:,:,m),inverse_seitz(:,:,symop(m)))
         call change_basis_to_(beta0,beta(:,:,:,m),inverse_seitz(:,:,symop(m)))
      end do
      call destroy_(inverse_seitz)

      ! Make the non H charge and dipole Lorentz factor tensors
      dim = 3*n_non_H
      call create_(L0,dim,n_non_H)  ! charge L tensor
      call create_(L1,dim,dim)  ! dipole L tensor
      call make_LFF_tensors_(self%unit_cell,L0,L1,pos0,mol0)

      ! Average the L(1) tensors over molecules
      L = ZERO
      do i1 = 1,n_mol
         do i2 = 1,n_mol
            m1 = 3*(i1-1)
            m2 = 3*(i2-1)
            do j1 = 1,n_non_H_per_mol
               do j2 = 1,n_non_H_per_mol
                  k1 = 3*(n_non_H_per_mol*(i1-1)+j1-1)
                  k2 = 3*(n_non_H_per_mol*(i2-1)+j2-1)
                  L(m1+1:m1+3,m2+1:m2+3) = L(m1+1:m1+3,m2+1:m2+3)+L1(k1+1:k1+3,k2+1:k2+3)
               end do
            end do
         end do
      end do
      fac = ONE/(n_non_H_per_mol*n_non_H_per_mol)
      L = fac*L

      ! Mini Clean up
      call destroy_(L1)
      call destroy_(L0)
      call destroy_(mol0)
      call destroy_(pos0)
      call destroy_(non_H)
      call destroy_(com)
      call destroy_(pos)
      call destroy_(par)
      call destroy_(symop)
      call destroy_(mol)

      ! Make the (inverse) D matrix: D = (1 - aL)^-1
      dim = 3*n_mol
      call create_(D,dim,dim)
      call to_unit_mx_(D)
      i = 0
      do a = 1,n_mol
         j = 0
         do b = 1,n_mol
            call plus_scaled_product_of_(D(i+1:i+3,j+1:j+3),L(i+1:i+3,j+1:j+3),alpha(:,:,b),-ONE)
            j = j+3
         end do
         i = i+3
      end do

      ! Invert
      call to_inverse_of_(D,D)  ! NOT PURE

      ! Make the local field factors
      call create_(Df,dim,3)
      call make_local_field_factors(Df,D)

      ! Make the chi2 susceptibility
      chi2 = ZERO
      i = 0
      do a = 1,n_mol
         call change_basis_to_(beta(:,:,:,a),beta0,Df(i+1:i+3,:))
         chi2 = chi2+beta0
         i = i+3
      end do

      ! Clean up
      call destroy_(Df)
      call destroy_(D)
      call destroy_(L)
      call destroy_(beta)
      call destroy_(alpha)
   end subroutine


! Old distributed polarisability code
! This code does the chi2 from hyperpolarizability

   subroutine make_susceptibility2(self,alpha_mol,beta_mol,uc_alpha,chi1_naive,chi1_centr,chi1_nonH,chi2_nonH,chi1_nc,chi1_do,chi1)
   ! Make the second ortder susceptibility from distributed
   ! polarisabilities.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,3,3), IN :: alpha_mol
      MAT3(REAL,3,3,3), IN :: beta_mol
      MAT(REAL,3,3), OUT :: uc_alpha  ! Unit cell alpha
      MAT(REAL,3,3), OUT :: chi1_naive  ! Clausius-Mossotti, L=1/3
      MAT(REAL,3,3), OUT :: chi1_centr  ! Dipoles at the COM
      MAT(REAL,3,3), OUT :: chi1_nonH  ! Non-hydrogen model, RLFTn
      MAT(REAL,3,3), OUT :: chi1_nc  ! No charge contributons to polarisability
      MAT(REAL,3,3), OUT :: chi1_do  ! Atomic alpha used
      MAT(REAL,3,3), OUT :: chi1  ! Distributed polarisability
      MAT3(REAL,3,3,3) :: chi2_nonH
      VEC(REAL,3) :: r
      MAT(REAL,3,3) :: AA
      MAT(REAL,3,3) :: alph,al
      MAT3(REAL,3,3,3) :: beta,be
      MAT(REAL,:,:), allocatable, target :: L0,L1
      VEC(REAL,:), PTR :: Lq
      MAT(REAL,:,:), PTR :: Lm
      MAT(REAL,:,:), allocatable :: df
      MAT(REAL,:,:), allocatable, target :: D,Dnc,Ddo
      MAT(REAL,:,:), PTR :: Dd
      MAT(REAL,:,:), allocatable :: pos,pos0,coc,com,coc0,alpha_q
      VEC(INT,:), allocatable :: mol,mol0,par,non_H
      MAT3(REAL,:,:,:), allocatable :: symop,alpha_m,alpha
      INT :: n,n_m,n_a,n_H,nH,a,b,i,j,m
      REAL :: fac
   ENSURE(allocated(self%asymmetric_unit_atom),"CRYSTAL:make_susceptibility2 ... no asymetric cell atoms")

      ! Get the unit cell electrical properties for each atom
      call get_uc_molecule_info_(self,mol,symop,par,pos,coc,com,alpha_q,alpha_m)

      ! Get no of atoms in unit cell
      n = size(par)

      ! Factor to reduce polarisabilities (in au)
      fac = FOUR*PI/self%unit_cell%volume

      ! Make the charge and dipole Lorentz factor tensors
      call create_(D,3*n,3*n)
      call create_(Dnc,3*n,3*n)
      call create_(Ddo,3*n,3*n)
      call create_(alpha,3,3,n)
      call create_(L0,3*n,n)
      call create_(L1,3*n,3*n)
      call make_LFF_tensors_(self%unit_cell,L0,L1,pos,mol)

      ! Get the atomic alpha's
      alpha = ZERO
      do a = 1,n
         r = pos(:,a)-coc(:,a)
         AA = outer_product_with_(r,alpha_q(:,a))+alpha_m(:,:,a)
         alpha(:,:,a) = alpha(:,:,a)+AA
      end do

      ! Make the D matrix
      call to_unit_mx_(D)
      call to_unit_mx_(Dnc)
      call to_unit_mx_(Ddo)
      i = 0
      do a = 1,n
         j = 0
         do b = 1,n
            Lq => L0(i+1:i+3,b)
            Lm => L1(i+1:i+3,j+1:j+3)
            Dd => D(i+1:i+3,j+1:j+3)
            Dd = Dd-fac*outer_product_with_(Lq,alpha_q(:,b))
            Dd = Dd-fac*matmul(Lm,alpha_m(:,:,b))
            Dd => Dnc(i+1:i+3,j+1:j+3)
            Dd = Dd-fac*matmul(Lm,alpha_m(:,:,b))
            Dd => Ddo(i+1:i+3,j+1:j+3)
            Dd = Dd-fac*matmul(Lm,alpha(:,:,b))
            j = j+3
         end do
         i = i+3
      end do
      call to_inverse_of_(D,D)
      call to_inverse_of_(Dnc,Dnc)
      call to_inverse_of_(Ddo,Ddo)
      call destroy_(L1)
      call destroy_(L0)

      ! Make the distributed polarisability chi1
      chi1 = ZERO
      chi1_nc = ZERO
      chi1_do = ZERO
      uc_alpha = ZERO
      i = 0
      do a = 1,n
         r = pos(:,a)-coc(:,a)
         AA = outer_product_with_(r,alpha_q(:,a))+alpha_m(:,:,a)
         uc_alpha = uc_alpha+AA
         AA = fac*AA
         j = 0
         do b = 1,n
            Dd => D(i+1:i+3,j+1:j+3)
            chi1 = chi1+matmul(AA,Dd)
            Dd => Dnc(i+1:i+3,j+1:j+3)
            chi1_nc = chi1_nc+matmul(AA,Dd)
            Dd => Ddo(i+1:i+3,j+1:j+3)
            chi1_do = chi1_do+matmul(AA,Dd)
            j = j+3
         end do
         i = i+3
      end do

      ! Clean up
      call destroy_(alpha)
      call destroy_(Ddo); call destroy_(Dnc); call destroy_(D)

      ! ================================================

      ! Make chi1_naive
      call create_(D,3,3)
      call to_unit_mx_(D)
      D = D-fac*THIRD*uc_alpha
      call to_inverse_of_(D,D)
      chi1_naive = fac*matmul(uc_alpha,D)
      call destroy_(D)

      ! ================================================

      ! Get the no of molecules
      n_m = maxval(mol)
      ENSURE(mod(n,n_m)==0,"CRYSTAL:make_susceptibility2 ... no of atoms in a molecule is not integral")

      ! Get the no of atoms per molecule
      n_a = n/n_m

      ! Get the molecular alpha's
      call create_(alpha,3,3,n_m)
      alpha = ZERO
      a = 0
      do m = 1,n_m
         do i = 1,n_a
            a = a+1
            r = pos(:,a)-coc(:,a)
            AA = outer_product_with_(r,alpha_q(:,a))+alpha_m(:,:,a)
            alpha(:,:,m) = alpha(:,:,m)+AA
         end do
      end do

      ! Get the centroids, use COM
      call create_(coc0,3,n_m)
      call create_(mol0,n_m)
      coc0 = com(:,1:(n_m-1)*n_a+1:n_a)
      mol0 = mol(1:(n_m-1)*n_a+1:n_a)

      ! Print out centroid info ... important
   ! stdout.flush
   ! stdout.show("Center of mass =",coc0(:,1))

      ! Make the centroid charge and dipole Lorentz factor tensors
      call create_(D,3*n_m,3*n_m)
      call create_(L0,3*n_m,n_m)
      call create_(L1,3*n_m,3*n_m)
      call make_LFF_tensors_(self%unit_cell,L0,L1,coc0,mol0)

      ! Make the D matrix
      call to_unit_mx_(D)
      i = 0
      do a = 1,n_m
         j = 0
         do b = 1,n_m
            Dd => D(i+1:i+3,j+1:j+3)
            Lm => L1(i+1:i+3,j+1:j+3)
            Dd = Dd-fac*matmul(Lm,alpha(:,:,b))
            j = j+3
         end do
         i = i+3
      end do
      call to_inverse_of_(D,D)
      call destroy_(L1)
      call destroy_(L0)

      ! Make the chi1_centr susceptibility
      chi1_centr = ZERO
      i = 0
      do a = 1,n_m
         AA = alpha(:,:,a)
         AA = fac*AA
         j = 0
         do b = 1,n_m
            Dd => D(i+1:i+3,j+1:j+3)
            chi1_centr = chi1_centr+matmul(AA,Dd)
            j = j+3
         end do
         i = i+3
      end do

      ! Clean up
      call destroy_(D)
      call destroy_(mol0)
      call destroy_(coc0)

      ! ================================================
      ! RLFTn
      ! ================================================

      ! Get non H positions.
      n_H = count(self%asymmetric_unit_atom(par)%atomic_number>1)
      call create_(non_H,n_H)
      non_H = pack([(a,a = 1,n)],self%asymmetric_unit_atom(par)%atomic_number>1)

      ! Get the non H positions, indices, molecules.
      call create_(pos0,3,n_H)
      call create_(mol0,n_H)
      pos0 = pos(:,non_H)
      mol0 = mol(non_H)

      ! Make the non H charge and dipole Lorentz factor tensors
      call create_(D,3*n_H,3*n_H)  ! (1 - aL)^-1
      call create_(df,3*n_H,3)  ! local field factor tensor
      call create_(L0,3*n_H,n_H)  ! charge L tensor
      call create_(L1,3*n_H,3*n_H)  ! dipole L tensor
      call make_LFF_tensors_(self%unit_cell,L0,L1,pos0,mol0)

      ! Get number of Non-H hydrogen atoms per molecule
      ENSURE(mod(n_H,n_m)==0,"CRYSTAL:make_susceptibility2 ... no of non-Hydrogen atoms wrong")
      nH = n_H/n_m

      ! Average alpha over non-H sites
      alpha = alpha/nH

      ! Get the reduced average molecular polarisabilities
      alph = fac*alpha_mol/nH
      beta = fac*beta_mol/nH

      ! Make the D matrix
      call to_unit_mx_(D)
      i = 0
      do a = 1,n_H
         j = 0
         do b = 1,n_H
            Dd => D(i+1:i+3,j+1:j+3)
            Lm => L1(i+1:i+3,j+1:j+3)
            m = (b-1)/nH+1

            ! Old way
         !  Dd = Dd - fac*matmul(Lm,alpha(:,:,m))

            ! New way -- should be better -- check
            call change_basis_to_(alph,al,symop(:,:,m))
            Dd = Dd-matmul(Lm,al)
            j = j+3
         end do
         i = i+3
      end do
      call to_inverse_of_(D,D)  ! NOT PURE
      call destroy_(L1)
      call destroy_(L0)

      ! Make the chi1_nonH susceptibility
      chi1_nonH = ZERO
      i = 0
      do a = 1,n_H
         m = (a-1)/nH+1
         AA = alpha(:,:,m)
         AA = fac*AA
         j = 0
         do b = 1,n_H
            Dd => D(i+1:i+3,j+1:j+3)
            chi1_nonH = chi1_nonH+matmul(AA,Dd)
            j = j+3
         end do
         i = i+3
      end do

      ! Local field factors
      df = ZERO
      i = 0
      do a = 1,n_H
         j = 0
         do b = 1,n_H
            df(i+1:i+3,:) = df(i+1:i+3,:)+D(i+1:i+3,j+1:j+3)
            j = j+3
         end do
         i = i+3
      end do

      ! Make the chi1_nonH and chi2_nonH susceptibility
      chi1_nonH = ZERO
      chi2_nonH = ZERO
      i = 0
      do a = 1,n_H
         m = (a-1)/nH+1
         call change_basis_to_(alph,al,symop(:,:,m))
         chi1_nonH = chi1_nonH+matmul(al,df(i+1:i+3,:))
         call change_basis_to_(beta,be,symop(:,:,m))
         call change_basis_using_(be,df(i+1:i+3,:))
         chi2_nonH = chi2_nonH+be
         i = i+3
      end do
      chi2_nonH = HALF*chi2_nonH

      ! Clean up
      call destroy_(df); call destroy_(D)
      call destroy_(mol0); call destroy_(pos0)
      call destroy_(non_H)
      call destroy_(alpha)
      call destroy_(alpha_m); call destroy_(alpha_q)
      call destroy_(com); call destroy_(coc); call destroy_(pos)
      call destroy_(par); call destroy_(symop); call destroy_(mol)
   end subroutine

   PURE subroutine make_local_field_factors(Df,D)
   ! Make the second ortder susceptibility from distributed
   ! polarisabilities.
      MAT(REAL,:,:), OUT :: Df
      MAT(REAL,:,:), IN :: D
      INT :: n,a,b,i,j
   ENSURE(is_square_(D),"CRYSTAL:make_local_field_factors ... non square D")
   ENSURE(mod(size(D,1),3)==0,"CRYSTAL:make_local_field_factors ... D dim1 is not divisible by 3")
   ENSURE(size(Df,1)==size(D,1),"CRYSTAL:make_local_field_factors ... wrong dim1, Df")
   ENSURE(size(Df,2)==3,"CRYSTAL:make_local_field_factors ... wrong dim2, Df")
      n = size(D,1)/3
      Df = ZERO
      i = 0
      do a = 1,n
         j = 0
         do b = 1,n
            Df(i+1:i+3,:) = Df(i+1:i+3,:)+D(i+1:i+3,j+1:j+3)
            j = j+3
         end do
         i = i+3
      end do
   end subroutine


! These are for more complicated calculations

   subroutine make_D_mx(self,D)
   ! Make the D matrix: D = (1 - L^0 a^0 - L^1 a^1)^-1
   ! where L^0 and L^1 are the monopole and dipole L tensors and
   ! where a^0 and a^1 are the monopole and dipole polarisabilities
   ! for every atom site at which the L tensors are evaluated.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: D
      VEC(INT,:), allocatable :: mol,par
      MAT3(REAL,:,:,:), allocatable :: symop,alpha_m
      MAT(REAL,:,:), allocatable :: pos,coc,com,alpha_q
      MAT(REAL,:,:), allocatable, target :: L0,L1
      MAT(REAL,:,:), PTR :: Lm
      VEC(REAL,:), PTR :: Lq
      INT :: n,dim,a,b,i,j
      REAL :: fac
   ENSURE(is_square_(D),"CRYSTAL:make_D_mx ... non square D")

      ! Factor to reduce polarisabilities = 1/(eps_0 V) (in au)
      fac = FOUR*PI/self%unit_cell%volume

      ! Get the electrical properties for each atom
      call get_uc_molecule_info_(self,mol,symop,par,pos,coc,com,alpha_q,alpha_m)
      call destroy_(com)
      call destroy_(coc)
      call destroy_(pos)
      call destroy_(par)
      call destroy_(symop)
      call destroy_(mol)

      ! Get no of atoms in unit cell
      n = size(par)
      dim = size(D,1)
      ENSURE(dim==3*n,"CRYSTAL:make_D_mx ... wrong size, par array")

      ! Make the charge and dipole Lorentz factor tensors
      call create_(L0,dim,n)
      call create_(L1,dim,dim)
      call make_LFF_tensors_(self%unit_cell,L0,L1,pos,mol)

      ! Make the (inverse) D matrix
      call to_unit_mx_(D)
      i = 0
      do a = 1,n
         j = 0
         do b = 1,n
            Lq => L0(i+1:i+3,b)
            Lm => L1(i+1:i+3,j+1:j+3)
            D(i+1:i+3,j+1:j+3) = D(i+1:i+3,j+1:j+3)-fac*outer_product_with_(Lq,alpha_q(:,b))
            call plus_scaled_product_of_(D(i+1:i+3,j+1:j+3),Lm,alpha_m(:,:,b),-fac)
            j = j+3
         end do
         i = i+3
      end do

      ! Clean up
      call destroy_(alpha_m)
      call destroy_(alpha_q)
      call destroy_(L1)
      call destroy_(L0)

      ! Invert
      call to_inverse_of_(D,D)  ! NOT PURE
   end subroutine

   subroutine make_Dnc_mx(self,Dnc)
   ! Make the Dnc matrix: Dnc = (1 - L^1 a^1)^-1
   ! where L^1 is the dipole L tensors and where a^1 is the dipole
   ! polarisability for every atom site at which the L tensors are
   ! evaluated.  NOTE: Dnc means "no charge dipole terms"
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Dnc
      VEC(INT,:), allocatable :: mol,par
      MAT3(REAL,:,:,:), allocatable :: symop,alpha_m
      MAT(REAL,:,:), allocatable :: pos,coc,com,alpha_q
      MAT(REAL,:,:), allocatable, target :: L0,L1
      MAT(REAL,:,:), PTR :: Lm
      INT :: n,dim,a,b,i,j
      REAL :: fac
   ENSURE(is_square_(Dnc),"CRYSTAL:make_Dnc_mx ... non square Dnc")

      ! Factor to reduce polarisabilities = 1/(eps_0 V) (in au)
      fac = FOUR*PI/self%unit_cell%volume

      ! Get the unit cell electrical properties for each atom
      call get_uc_molecule_info_(self,mol,symop,par,pos,coc,com,alpha_q,alpha_m)
      call destroy_(alpha_q)
      call destroy_(com)
      call destroy_(coc)
      call destroy_(pos)
      call destroy_(par)
      call destroy_(symop)
      call destroy_(mol)

      ! Get no of atoms in unit cell
      n = size(par)
      dim = size(Dnc,1)
      ENSURE(dim==3*n,"CRYSTAL:make_Dnc_mx ... wrong size, par array")

      ! Make the charge and dipole Lorentz factor tensors
      call create_(L0,dim,n)
      call create_(L1,dim,dim)
      call make_LFF_tensors_(self%unit_cell,L0,L1,pos,mol)

      ! Make the (inverse) D matrix
      call to_unit_mx_(Dnc)
      i = 0
      do a = 1,n
         j = 0
         do b = 1,n
            Lm => L1(i+1:i+3,j+1:j+3)
            call plus_scaled_product_of_(Dnc(i+1:i+3,j+1:j+3),Lm,alpha_m(:,:,b),-fac)
            j = j+3
         end do
         i = i+3
      end do

      ! Clean up
      call destroy_(L1)
      call destroy_(L0)

      ! Invert
      call to_inverse_of_(Dnc,Dnc)  ! NOT PURE
   end subroutine


!  ===============
!  Electric fields
!  ===============

   PURE subroutine make_Lorentz_fields(self,F,sphere_sum)
   ! Make the Lorentz electric fields at the unit cell atom sites.
   ! Warning: the asymmetric_unit_atom's should be generated from the
   ! fragment where catomic harges and dipoles have been done.
      type(CRYSTAL_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: F
      BIN, optional, IN :: sphere_sum
      VEC(INT,:), allocatable :: mol,frag_atom
      VEC(REAL,:), allocatable :: charge,dipole
      MAT(REAL,:,:), allocatable :: pos,L0,L1
      REAL :: fac
      INT :: n_atom,dim,u,a,fu,lu

      ! Get the unit cell molecule atom info
      call get_uc_molecule_info_(self,mol,pos,charge,dipole,frag_atom)

      ! Make the Lorentz factor tensors L0, L1
      n_atom = size(charge)
      dim = size(dipole)
      call create_(L0,dim,n_atom)  ! charge L0 tensor
      call create_(L1,dim,dim)  ! dipole L1 tensor
      call make_LFF_tensors_(self%unit_cell,L0,L1,pos,mol,sphere_sum)
      fac = FOUR*PI/self%unit_cell%volume
      L0 = fac*L0
      L1 = fac*L1

      ! Assign electric fields
      do a = 1,self%n_fragment_atoms
         u = index_of_value_(frag_atom,a)  ! map to unit cell atom
         ENSURE(u/=0,"CRYSTAL:make_Lorentz_fields ... fragment atom "//trim(to_str_(a))//" not found in cell")
         lu = 3*(u-1)
         fu = lu+1
         lu = lu+3
         call to_product_of_(F(:,a),L0(fu:lu,:),charge)
         call plus_product_of_(F(:,a),L1(fu:lu,:),dipole)
      end do

      ! Clean up
      call destroy_(L1)
      call destroy_(L0)
      call destroy_(frag_atom)
      call destroy_(dipole)
      call destroy_(charge)
      call destroy_(pos)
   end subroutine

   PURE subroutine make_Lorentz_interactions(self,I0,I1,sphere_sum)
   ! Make the Lorentz interactions with charges and dipoles due to
   ! dipoles ONLY at the unit cell atom sites. The charge-charge term
   ! is not calculated -- it does not affect the susceptibilities.
   ! Warning: the asymmetric_unit_atom's should be generated from the
   ! fragment where atomic charges and dipoles have been done.
      type(CRYSTAL_TYPE), IN :: self
      VEC(REAL,:), OUT :: I0
      MAT(REAL,:,:), OUT :: I1
      BIN, optional, IN :: sphere_sum
      VEC(INT,:), allocatable :: mol,frag_atom
      VEC(REAL,:), allocatable :: charge,dipole
      MAT(REAL,:,:), allocatable :: pos,L0,L1
      REAL :: fac
      INT :: n_atom,dim,u,a,fu,lu

      ! Get the unit cell molecule atom info
      call get_uc_molecule_info_(self,mol,pos,charge,dipole,frag_atom)

      ! Create L tensors
      n_atom = size(charge)  ! No of unit cell atoms
      dim = size(dipole)  ! 3 x the above
      call create_(L0,dim,n_atom)  ! charge L0 tensor
      call create_(L1,dim,dim)  ! dipole L1 tensor

      ! Make the Lorentz factor tensors L0, L1
      call make_LFF_tensors_(self%unit_cell,L0,L1,pos,mol,sphere_sum)
      fac = FOUR*PI/self%unit_cell%volume
      L0 = fac*L0
      L1 = fac*L1

      ! Assign charge and dipole interactions
      ! Needs a minus sign when combined
      do a = 1,self%n_fragment_atoms
         u = index_of_value_(frag_atom,a)  ! map to unit cell atom
         ENSURE(u/=0,"CRYSTAL:make_Lorentz_interactions ... fragment atom "//trim(to_str_(a))//" not found in cell")
         lu = 3*(u-1)
         fu = lu+1
         lu = lu+3
         I0(a) = dot_product(L0(:,u),dipole)
         call to_product_of_(I1(:,a),L1(:,fu:lu),dipole,transpose_a=TRUE)
      end do

      ! Clean up
      call destroy_(L1)
      call destroy_(L0)
      call destroy_(frag_atom)
      call destroy_(dipole)
      call destroy_(charge)
      call destroy_(pos)
   end subroutine


!  =====================================================
!  Cluster positions, charges, dipoles, polarisabilities
!  =====================================================

   PURE subroutine get_uc_molecule_info_0(self,mol,symop,par,pos,com)
   ! Get the unit cell molecule information for a distributed local
   ! field factor (LFF) calculation. "mol" is the molecule index for
   ! an atom in the unit cell. "pos" are the positions. "par" is the
   ! asymmetric unit atom parent. "com" are the molecular center of
   ! masses.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), allocatable, OUT :: mol
      VEC(INT,:), allocatable, OUT :: symop
      VEC(INT,:), allocatable, OUT :: par
      MAT(REAL,:,:), allocatable, OUT :: pos
      MAT(REAL,:,:), allocatable, OUT :: com
      INT :: n_atom,n_mol,n,m,u,a
      VEC(REAL,:), allocatable :: mass
      VEC(REAL,3) :: cm
      REAL :: fac
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:get_uc_molecule_info_0 ... no cell_mol_for_atom")

      ! Create the info arrays
      n_mol = self%n_unit_cell_mols
      n_atom = self%n_unit_cell_atoms
      call create_(mol,n_atom)
      call create_(symop,n_mol)
      call create_(par,n_atom)
      call create_(pos,3,n_atom)
      call create_(com,3,n_mol)
      call create_(mass,n_atom)

      ! Set n_atom to the # of atoms in one molecule
      ENSURE(mod(n_atom,n_mol)==0,"CRYSTAL:get_uc_molecule_info_0 ... # of unit cell atoms is not divisible by number of molecules")
      n_atom = n_atom/n_mol
      n = 0
      do m = 1,n_mol

         ! Find atom "u" on molecule "m", defragment, get the info
         do u = 1,self%n_unit_cell_atoms
            if (self%unit_cell_mol_for_atom(u)/=m) cycle
            mol(n+1:n+n_atom) = m
            call do_defragment_uc_atom_(self,u,symop(m),par(n+1:n+n_atom),pos(:,n+1:n+n_atom),mass(n+1:n+n_atom))
            exit
         end do

         ! Calculate the center of mass (COM)
         cm = ZERO
         do a = 1,n_atom
            cm = cm+mass(n+a)*pos(:,n+a)
         end do
         fac = sum(mass(n+1:n+n_atom))
         fac = ONE/fac
         cm = fac*cm
         com(:,m) = cm

         ! Next
         n = n+n_atom
      end do
      call destroy_(mass)
   end subroutine

   PURE subroutine get_uc_molecule_info_1(self,mol,symop,par,pos,coc,com,alpha_q,alpha_m)
   ! Get the unit cell molecule information for a distributed local field factor
   ! (LFF) calculation. "coc" are the molecular center of charges.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), allocatable, OUT :: mol
      MAT3(REAL,:,:,:), allocatable, OUT :: symop
      VEC(INT,:), allocatable, OUT :: par
      MAT(REAL,:,:), allocatable, OUT :: pos
      MAT(REAL,:,:), allocatable, OUT :: coc
      MAT(REAL,:,:), allocatable, OUT :: com
      MAT(REAL,:,:), allocatable, OUT :: alpha_q
      MAT3(REAL,:,:,:), allocatable, OUT :: alpha_m
      INT :: n_atom,n_mol,n,m,u,a
      VEC(REAL,:), allocatable :: charge,mass
      VEC(REAL,3) :: e_dipole,cm
      MAT(REAL,:,:), allocatable :: dipole
      REAL :: fac
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:get_uc_molecule_info_1 ... no cell_mol_for_atom")

      ! Sizes
      n_mol = self%n_unit_cell_mols
      n_atom = self%n_unit_cell_atoms

      ! Create the info arrays
      call create_(mol,n_atom)
      call create_(symop,3,3,n_mol)
      call create_(par,n_atom)
      call create_(pos,3,n_atom)
      call create_(coc,3,n_atom)
      call create_(com,3,n_atom)
      call create_(alpha_q,3,n_atom)
      call create_(alpha_m,3,3,n_atom)
      call create_(charge,n_atom)
      call create_(mass,n_atom)
      call create_(dipole,3,n_atom)

      ! Set n_atom to the # of atoms in one molecule
      ENSURE(mod(n_atom,n_mol)==0,"CRYSTAL:get_uc_molecule_info_1 ... # of unit cell atoms is not divisible by number of molecules")
      n_atom = n_atom/n_mol
      n = 0
      do m = 1,n_mol

         ! Find atom "u" on molecule "m", defragment, get the info
         do u = 1,self%n_unit_cell_atoms
            if (self%unit_cell_mol_for_atom(u)/=m) cycle
            mol(n+1:n+n_atom) = m
            call do_defragment_uc_atom_(self,u,symop(:,:,m),par(n+1:n+n_atom),pos(:,n+1:n+n_atom),charge(n+1:n+n_atom),mass(n+1:n+n_atom),dipole(:,n+1:n+n_atom),alpha_q(:,n+1:n+n_atom),alpha_m(:,:,n+1:n+n_atom))
            exit
         end do

         ! Calculate electronic dipole and center of charge
         e_dipole = ZERO
         do a = 1,n_atom
            e_dipole = e_dipole+dipole(:,n+a)
            e_dipole = e_dipole+charge(n+a)*pos(:,n+a)
         end do
         fac = sum(charge(n+1:n+n_atom))
         fac = ONE/fac
         e_dipole = fac*e_dipole
         coc(:,n+1:n+n_atom) = spread(e_dipole,2,n_atom)

         ! Calculate the center of mass (COM): make coc the COM
         cm = ZERO
         do a = 1,n_atom
            cm = cm+mass(n+a)*pos(:,n+a)
         end do
         fac = sum(mass(n+1:n+n_atom))
         fac = ONE/fac
         cm = fac*cm
         com(:,n+1:n+n_atom) = spread(cm,2,n_atom)

         ! Next
         n = n+n_atom
      end do
      call destroy_(dipole)
      call destroy_(mass)
      call destroy_(charge)
   end subroutine

   PURE subroutine get_uc_molecule_info_2(self,mol,pos,charge,dipole,frag_atom)
   ! Get the unit cell molecule information for a distributed local field factor
   ! (LFF) calculation. "coc" are the molecular center of charges.
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), allocatable, OUT :: mol
      MAT(REAL,:,:), allocatable, OUT :: pos
      VEC(REAL,:), allocatable, OUT :: charge
      VEC(REAL,:), allocatable, OUT :: dipole
      VEC(INT,:), allocatable, OUT :: frag_atom
      INT :: n_atom,n_mol,n,n3,m,u
      VEC(INT,:), allocatable :: occupation_list
   ENSURE(allocated(self%unit_cell_mol_for_atom),"CRYSTAL:get_uc_molecule_info_2 ... no cell_mol_for_atom")

      ! Sizes
      n_mol = self%n_unit_cell_mols
      n_atom = self%n_unit_cell_atoms

      ! Create the info arrays
      call create_(mol,n_atom)
      call create_(pos,3,n_atom)
      call create_(charge,n_atom)
      call create_(dipole,3*n_atom)
      call create_(frag_atom,n_atom)

      ! Counters
      n = 0
      n3 = 0

      ! Loop over molecules m
      do m = 1,n_mol

         ! Find atom "u" on molecule "m", defragment, get the info
         do u = 1,self%n_unit_cell_atoms
            if (self%unit_cell_mol_for_atom(u)/=m) cycle

            ! Get the occ list for molecule m
            call create_(occupation_list,1)
            occupation_list(1) = CLUSTER_AT_CODE(0,0,0,u)
            call do_defragment_(self,occupation_list)

            ! No of atoms in molecule m
            n_atom = size(occupation_list)

            ! Assign
            mol(n+1:n+n_atom) = m

            ! Get the info
            call extract_atom_info_(self,occupation_list,pos(:,n+1:n+n_atom),charge(n+1:n+n_atom),dipole(n3+1:n3+3*n_atom),frag_atom(n+1:n+n_atom))

            ! Clean
            call destroy_(occupation_list)
            exit
         end do

         ! Increment
         n = n+n_atom
         n3 = n3+3*n_atom
      end do
   end subroutine


!  ==============
!  Cluster output
!  ==============

   subroutine put_cluster_info(self)
   ! Put the list of vertices for the object
      type(CRYSTAL_TYPE), IN :: self
   ENSURE(self%cluster_info_made,"CRYSTAL:put_cluster_info ... call make_cluster_info first")
   ENSURE(allocated(tonto),"CRYSTAL:put_cluster_info ... no tonto variable")
      call flush_(stdout)
      call text_(stdout,"==============================")
      call text_(stdout,"Cluster generation information")
      call text_(stdout,"==============================")
      call flush_(stdout)
      call show_(stdout,"Cluster generation method       =",self%generation_method)
      call show_(stdout,"Complete connected fragments?   =",self%defragment)
      call show_(stdout,"Radius of cluster               =",self%radius)
      call flush_(stdout)
      call show_(stdout,"No. of molecules                =",self%n_molecules)
      call show_(stdout,"No. of atoms                    =",self%n_atoms)
      call show_(stdout,"No. of fragment atoms           =",self%n_fragment_atoms)
      call flush_(stdout)
      call show_(stdout,"Fragment offset                 =",self%fragment_offset)
      call show_(stdout,"Minimum cell shift for search   =",self%h_min)
      call show_(stdout,"Maximum cell shift for search   =",self%h_max)
      if (allocated(self%fragment_geometry)) call put_central_fragment_coords_(self)
      if (allocated(self%occupation_list)) call put_occupation_list_(self)
   !   if (.atom_connection.allocated)      .put_atom_connection_table
   end subroutine

   subroutine put_central_fragment_coords(self)
   ! Put the initial fragment geometry information
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), allocatable :: ind
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
   ENSURE(allocated(self%fragment_geometry),"CRYSTAL:put_central_fragment_coords ... no fragment_geometry")
   ENSURE(allocated(self%fragment_atom),"CRYSTAL:put_central_fragment_coords ... no fragment_atom")
      call flush_(stdout)
      call text_(stdout,"=======================================")
      call text_(stdout,"Central fragment fractional coordinates")
      call text_(stdout,"=======================================")
      call flush_(stdout)
      call show_(stdout,"No. of fragment atoms =",self%n_fragment_atoms)
      call flush_(stdout)

      ! Unique ID tags
      ind = crystal_frag_atom_indices_(self)

      ! Table
      call create_(table,4)
      call set_heading_(table(1),"ID"); call set_values_(table(1),self%fragment_atom(ind)%tag)
      call set_heading_(table(3),"Coord")
      call set_subhead_(table(2),"- x -"); call set_values_(table(2),self%fragment_geometry(1,:))
      call set_subhead_(table(3),"- y -"); call set_values_(table(3),self%fragment_geometry(2,:))
      call set_subhead_(table(4),"- z -"); call set_values_(table(4),self%fragment_geometry(3,:))
      call set_sb3head_(table(3),"/frac")
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(ind)
   end subroutine

   subroutine put_occupation_list(self)
   ! Put the .occupation_list table
      type(CRYSTAL_TYPE), IN :: self
      VEC(INT,:), allocatable :: ua,aa,h1,h2,h3,sy,s1,s2,s3,ac,ao
      VEC(STR(len=STR_SIZE),:), allocatable :: ut,at
      VEC(REAL,:), allocatable :: r1,r2,r3
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
   ENSURE(allocated(self%occupation_list),"CRYSTAL:put_occupation_list ... no occupation_list")
   ENSURE(allocated(self%unit_cell_geometry),"CRYSTAL:put_occupation_list ... no crystal unit cell geometry")
   ENSURE(allocated(self%asym_symop_for_unit_cell_atom),"CRYSTAL:put_occupation_list ... no asym_symop_for_unit_cell_atom")
   ENSURE(allocated(self%asym_shift_for_unit_cell_atom),"CRYSTAL:put_occupation_list ... no asym_shift_for_unit_cell_atom")
      call flush_(stdout)
      call text_(stdout,"=============")
      call text_(stdout,"Cluster atoms")
      call text_(stdout,"=============")
      call flush_(stdout)
      call show_(stdout,"No. of atoms =",self%n_atoms)
   !> stdout.show("No. of atoms =",.occupation_list.dim) ! Kang. For Debug 
      call flush_(stdout)
      call text_(stdout,"The table below is understood as follows:")
      call flush_(stdout)
      call text_(stdout,"  #          = Cluster atom index")
      call text_(stdout,"  (xyz)      = Cluster atom coord (fractional)")
      call text_(stdout,"  UA         = Unit cell atom index which generates (xyz)")
      call text_(stdout,"  Mol        = Molecule index")
      call flush_(stdout)
      call text_(stdout,"where")
      call flush_(stdout)
      call text_(stdout,"  (xyz)      = (UA xyz) + (h1 h2 h3)")
      call text_(stdout,"  (UA xyz)   = Unit cell atom coord")
      call text_(stdout,"  (h1 h2 h3) = Unit cell atom shift = US")
      call flush_(stdout)
      call text_(stdout,"Also")
      call flush_(stdout)
      call text_(stdout,"  (xyz)      = Op(AA xyz) + (s1 s2 s3) + (h1 h2 h3)")
      call text_(stdout,"  (UA xyz)   = Op(AA xyz) + (s1 s2 s3)")
      call flush_(stdout)
      call text_(stdout,"where")
      call flush_(stdout)
      call text_(stdout,"  Op         = Seitz symmetry operator")
      call text_(stdout,"  (AA xyz)   = Asym cell atom coord")
      call text_(stdout,"  (s1 s2 s3) = Asym cell atom shift = AS")
      call text_(stdout,"  AA         = Asym cell atom index which generates (xyz)")
      call text_(stdout,"  AA code    = Asym atom code to make (xyz) from AA")
      call text_(stdout,"             = Op | h1+s1+5 | h2+s2+5 | h3+s3+5")
      call text_(stdout,"  AO code    = AA | Op")
      call flush_(stdout)

      ! Data
      ua = CLUSTER_UA_FROM_CODE(self%occupation_list)
      ut = self%unit_cell_atom(ua)%tag
      aa = self%asym_atom_for_unit_cell_atom(ua)
      at = self%asymmetric_unit_atom(aa)%tag
      h1 = CLUSTER_H1_FROM_CODE(self%occupation_list)+self%fragment_offset(1)
      h2 = CLUSTER_H2_FROM_CODE(self%occupation_list)+self%fragment_offset(2)
      h3 = CLUSTER_H3_FROM_CODE(self%occupation_list)+self%fragment_offset(3)
      r1 = self%unit_cell_geometry(1,ua)+REALIFY(h1)
      r2 = self%unit_cell_geometry(2,ua)+REALIFY(h2)
      r3 = self%unit_cell_geometry(3,ua)+REALIFY(h3)
      sy = self%asym_symop_for_unit_cell_atom(ua)
      s1 = self%asym_shift_for_unit_cell_atom(1,ua)
      s2 = self%asym_shift_for_unit_cell_atom(2,ua)
      s3 = self%asym_shift_for_unit_cell_atom(3,ua)
      ac = 1000*sy
      ac = 100*(h1+5+s1)+ac
      ac = 10*(h2+5+s2)+ac
      ac = (h3+5+s3)+ac
      ao = 100*aa
      ao = sy+ao

      ! Table
      call create_(table,17)
      call set_heading_(table(1)," UA")
      call set_values_(table(1),ua)
      call set_heading_(table(2)," UA")
      call set_subhead_(table(2),"Tag")
      call set_values_(table(2),ut)
      call set_heading_(table(3),"Mol")
      call set_values_(table(3),self%molecule_for_atom)
      call set_heading_(table(5),"(xyz)")
      call set_subhead_(table(4),"- x -")
      call set_subhead_(table(5),"- y -")
      call set_subhead_(table(6),"- z -")
      call set_values_(table(4),r1)
      call set_values_(table(5),r2)
      call set_values_(table(6),r3)
      call set_heading_(table(8),"US")
      call set_subhead_(table(7),"h1")
      call set_subhead_(table(8),"h2")
      call set_subhead_(table(9),"h3")
      call set_values_(table(7),h1)
      call set_values_(table(8),h2)
      call set_values_(table(9),h3)
      call set_heading_(table(10),"AA")
      call set_values_(table(10),aa)
      call set_heading_(table(11)," AA")
      call set_subhead_(table(11),"Tag")
      call set_values_(table(11),at)
      call set_heading_(table(12),"Op")
      call set_heading_(table(14),"AS")
      call set_subhead_(table(13),"s1")
      call set_subhead_(table(14),"s2")
      call set_subhead_(table(15),"s3")
      call set_values_(table(12),sy)
      call set_values_(table(13),s1)
      call set_values_(table(14),s2)
      call set_values_(table(15),s3)
      call set_heading_(table(16)," AA ")
      call set_subhead_(table(16),"code")
      call set_values_(table(16),ac)
      call set_heading_(table(17)," AO ")
      call set_subhead_(table(17),"code")
      call set_values_(table(17),ao)
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(ao)
      call destroy_(ac)
      call destroy_(r3); call destroy_(r2); call destroy_(r1)
      call destroy_(s3); call destroy_(s2); call destroy_(s1)
      call destroy_(sy)
      call destroy_(h3); call destroy_(h2); call destroy_(h1)
      call destroy_(at)
      call destroy_(aa)
      call destroy_(ut)
      call destroy_(ua)
   end subroutine

   subroutine put_atom_connection_table(self)
   ! Put the .atom_connection table
      type(CRYSTAL_TYPE), IN :: self
      INT :: m,a,i
      BIN :: first
   ENSURE(allocated(self%atom_connection),"CRYSTAL:put_atom_connection_table ... no atom_connection")
   ENSURE(allocated(self%molecule_for_atom),"CRYSTAL:put_atom_connection_table ... no molecule_for_atom")
      call save_style_(stdout)
      call set_int_width_(stdout,5)
      call flush_(stdout)
      call text_(stdout,"Atom connection table:")
      call flush_(stdout)
      call show_(stdout,"n_molecules = ",self%n_molecules)
      call flush_(stdout)
      call dash_(stdout,int_fields=8)
      call put_(stdout,"Cluster",int_width=TRUE)
      call tab_(stdout,int_fields=1)
      call put_(stdout,"Joins",int_width=TRUE)
      call flush_(stdout)
      call put_(stdout,"Mol",int_width=TRUE)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Atom(s)",int_width=TRUE)
      call flush_(stdout)
      call dash_(stdout,int_fields=8)
      do m = 1,self%n_molecules

         ! Molecule label
         call put_(stdout,m)
         first = TRUE
         do a = 1,self%n_atoms
            if (self%molecule_for_atom(a)/=m) cycle
            if (NOT first) call tab_(stdout,int_fields=1)
            call put_(stdout,a)
            first = FALSE
            if (allocated(self%atom_connection(a)%element)) then
               do i = 1,size(self%atom_connection(a)%element)
                  call put_(stdout,self%atom_connection(a)%element(i))
               end do
               call flush_(stdout)
            else
               call flush_(stdout)
            end if
         end do
      end do
      call dash_(stdout,int_fields=8)
      call unsave_style_(stdout)
   end subroutine

end module
