!====================================================================
!
! CLUSTER:
!
! An object to store information pertaining to a cluster of atoms or
! molecules generated from an underlying crystal structure.
!
! A lot of information regarding the cluster is generated -- like the
! unit cell connection table, the atom connection table, the
! occupation list (unit cell atom numbers plus cell translation
! vectors), and the unit cell atom numbers.  The symmetry operators
! for each atom are also available from the unit cell numbers if the
! crystal information is available (it should be, otherwise we can't
! generate a cluster!).
!
! NOTE: an allocated crystal and atom list should be supplied. It is
! intended that these will come from the molecule which generates the
! cluster (perhaps a cluster should contain a molecule?).
!
! Copyright (C) Dylan Jayatilaka, 2002
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
module CLUSTER_MODULE

#  include "macros"

   include "cluster.use"

   implicit none

   include "cluster.int"


contains

!  ===========
!  Allocatiion
!  ===========

   PURE subroutine create(self)
   ! Allocate an object
      type(CLUSTER_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"CLUSTER:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(CLUSTER_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"CLUSTER:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
      type(CLUSTER_TYPE), INOUT :: self
      call destroy_(self%crystal)
      call destroy_(self%asymmetric_unit_atom)
      call destroy_(self%fragment_geometry)
      call destroy_(self%geometry)
      call destroy_(self%shift)
      call destroy_(self%shift_for_atom)
      call destroy_(self%is_fragment_atom)
      call destroy_(self%occupation_list)
      call destroy_(self%molecule_for_atom)
      call destroy_(self%molecule)
      call destroy_(self%molecule_centroid)
      call destroy_(self%unique_molecule_for)
      call destroy_(self%unique_symcode_for)
      call destroy_(self%atom_connection)
   end subroutine

   PURE subroutine destroy_cluster_info_ptr_part(self)
   ! Destroy the non-symop informational ptr parts. These incclude the actual
   ! .geometry of the cluster as wellas information relating to the .symop's
   ! used to generate the cluster.
      type(CLUSTER_TYPE), INOUT :: self
      call destroy_(self%geometry)
      call destroy_(self%shift)
      call destroy_(self%shift_for_atom)
      call destroy_(self%is_fragment_atom)
      call destroy_(self%occupation_list)
      call destroy_(self%molecule_for_atom)
      call destroy_(self%molecule)
      call destroy_(self%molecule_centroid)
      call destroy_(self%unique_molecule_for)
      call destroy_(self%unique_symcode_for)
      call destroy_(self%atom_connection)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self,crystal)
   ! Set up defaults. The "crystal" is pointer assigned. The cluster
   ! is essentially a crystal with additional operations based on the
   ! .fragment_geometry. Thus we must keep the crystal.fragment_geometry
   ! in sync with the cluster.fragment geometry.
      type(CLUSTER_TYPE), INOUT :: self
      type(CRYSTAL_TYPE), IN :: crystal
      type(CLUSTER_TYPE) :: cluster
   ENSURE(allocated(crystal%asymmetric_unit_atom),"CLUSTER:set_defaults ... no crystal asymmetric unit atoms!")
   ENSURE(allocated(crystal%fragment_geometry),"CLUSTER:set_defaults ... no crystal fragment geometry!")
      self = cluster
      call destroy_ptr_part_(self)

      ! Deep copy
      self%crystal = crystal
      self%asymmetric_unit_atom = self%crystal%asymmetric_unit_atom

      ! Set basis: slater trumps gaussians for cluster generation ...
      if (has_all_slater_bases_(self%asymmetric_unit_atom)) then; call set_basis_kind_(self%asymmetric_unit_atom,"slater")
      else if (has_all_coppens_bases_(self%asymmetric_unit_atom)) then; call set_basis_kind_(self%asymmetric_unit_atom,"coppens")
      else if (has_all_gaussian_bases_(self%asymmetric_unit_atom)) then; call set_basis_kind_(self%asymmetric_unit_atom,"gaussian")
      end if

      ! Set the fragment geometry in fractionals & offset from the supplied crystal
      self%fragment_geometry = crystal%fragment_geometry
      self%n_fragment_atoms = size(self%fragment_geometry,2)
      self%fragment_offset = int(mean_column_vector_(crystal%fragment_geometry)*(ONE-TOL(8)))  ! shrink toward zero: FP-robust at cell boundary
   end subroutine

   PURE subroutine set_defragment(self,val)
   ! Generic set
      type(CLUSTER_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%defragment = val
   end subroutine

   PURE subroutine set_radius(self,val)
   ! Generic set
      type(CLUSTER_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%radius = val
   end subroutine

   PURE subroutine set_fragment_geometry(self,fragment_geometry,cartesian)
   ! Point assign the fragment_geometry in sync with the crystal one
   ! Set "cartesian" TRUE if the fragment atoms are in xyz coord frame
   ! WARNING: doesn't seem in sync to me -- dylan
      type(CLUSTER_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: fragment_geometry
      BIN, optional, IN :: cartesian
   ENSURE(allocated(self%crystal),"CLUSTER:set_fragment_geometry ... no crystal created!")
   ENSURE(allocated(self%crystal%asymmetric_unit_atom),"CLUSTER:set_fragment_geometry ... no crystal asymmetric unit atoms!")

      ! Set crystal fragment geometry (must be fractional)
      call set_fragment_geometry_(self%crystal,fragment_geometry,cartesian)

      ! Point to crystal fragment geometry
      self%fragment_geometry = self%crystal%fragment_geometry
      self%n_fragment_atoms = size(self%fragment_geometry,2)

      ! Set fragment offset (center) from the supplied crystal
      self%fragment_offset = int(mean_column_vector_(self%crystal%fragment_geometry)*(ONE-TOL(8)))  ! shrink toward zero: FP-robust at cell boundary
   end subroutine

   PURE subroutine set_generation_method(self,method)
   ! Set the generation method, which tells how to generate the cluster e.g.
   ! whether to add atoms to the cluster within a certain radius of the starting
   ! fragment, or whether to use the unit cell ad the cluster, or just the
   ! initial starting fragment (i.e. the null cluster) or the initial fragment
   ! and the unit cell.
      type(CLUSTER_TYPE), INOUT :: self
      STR(len=*), IN :: method
      self%generation_method = method
      call to_lower_case_(self%generation_method)
      select case (self%generation_method)
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

   PURE subroutine set_unit_cell_offset(self,offset)
   ! Set the unit_cell_offset to use when making an offset unit cell.
      type(CLUSTER_TYPE), INOUT :: self
      VEC(REAL,3), IN :: offset
   ENSURE(all(-HALF<=offset) AND all(offset<=HALF),"CLUSTER:set_unit_cell_offset ... offset must be between -1/2 and +1/2")
      self%unit_cell_offset = offset
   end subroutine


!  =============
!  Input methods
!  =============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(CLUSTER_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","CLUSTER:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"CLUSTER:read_keywords ... unexpected end of file while reading list keywords")
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
      type(CLUSTER_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                             ")  ! exit case
      case ("add_criteria=                 "); call read_generation_method_(self)
      case ("atoms=                        "); call read_atoms_(self)
      case ("atom_density_cutoff=          "); call read_atom_density_cutoff_(self)
      case ("defragment=                   "); call read_defragment_(self)
      case ("fractional_fragment=          "); call read_fractional_fragment_(self)
      case ("generation_method=            "); call read_generation_method_(self)
      case ("make_info                     "); call update_info_(self)
!     case ("make_nn_info                  "); .make_NN_info
      case ("unit_cell_atoms_to_suppress=  "); call read_unit_cell_atoms_to_suppress_(self)
      case ("unit_cell_offset=             "); call read_unit_cell_offset_(self)
      case ("put                           "); call put_(self)
      case ("put_averaged_lff_l_tensors    "); call put_averaged_LFF_tensors_(self)
      case ("put_centroid_lff_l_tensors    "); call put_centroid_LFF_tensors_(self)
      case ("put_centroid_lff_l_tensors_old"); call put_centroid_LFF_tensors_old_(self)
      case ("put_lff_info                  "); call put_LFF_info_(self)
      case ("put_tonto_input               "); call put_tonto_input_(self)
      case ("radius=                       "); call read_radius_(self)
      case ("reset_site_occupancies        "); call reset_site_occupancies_(self)
      case ("units=                        "); call read_units_(self)
      case ("update_info                   "); call update_info_(self)
      case default; allocate(tonto%known_keywords(20))
      tonto%known_keywords(1) = "}                             "
      tonto%known_keywords(2) = "add_criteria=                 "
      tonto%known_keywords(3) = "atoms=                        "
      tonto%known_keywords(4) = "atom_density_cutoff=          "
      tonto%known_keywords(5) = "defragment=                   "
      tonto%known_keywords(6) = "fractional_fragment=          "
      tonto%known_keywords(7) = "generation_method=            "
      tonto%known_keywords(8) = "make_info                     "
      tonto%known_keywords(9) = "unit_cell_atoms_to_suppress=  "
      tonto%known_keywords(10) = "unit_cell_offset=             "
      tonto%known_keywords(11) = "put                           "
      tonto%known_keywords(12) = "put_averaged_lff_l_tensors    "
      tonto%known_keywords(13) = "put_centroid_lff_l_tensors    "
      tonto%known_keywords(14) = "put_centroid_lff_l_tensors_old"
      tonto%known_keywords(15) = "put_lff_info                  "
      tonto%known_keywords(16) = "put_tonto_input               "
      tonto%known_keywords(17) = "radius=                       "
      tonto%known_keywords(18) = "reset_site_occupancies        "
      tonto%known_keywords(19) = "units=                        "
      tonto%known_keywords(20) = "update_info                   "
      call unknown_(tonto,word,"CLUSTER:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine reset_site_occupancies(self)
   ! Sets all the site occupancies of asymmetric to 1.0
      type(CLUSTER_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: occ
   ENSURE(allocated(self%crystal),"CLUSTER:reset_site_occupancies ... no crystal defined")
   ENSURE(allocated(self%crystal%asymmetric_unit_atom),"CLUSTER:reset_site_occupancies ... no asymmetric atoms!")
      call create_(occ,size(self%asymmetric_unit_atom))
      occ = 1.0
      call set_site_occupancies_(self%asymmetric_unit_atom,occ)
      call destroy_(occ)
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(CLUSTER_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_unit_cell_atoms_to_suppress(self)
   ! Read in a list of indices into the unit cell atom list
   ! In make_info these atoms are removed from unit_cell_geometry
   ! thus suppressing them from cluster generation methods
   ! that use the unit cell geometry
      type(CLUSTER_TYPE), INOUT :: self
      call read_all_(stdin,self%unit_cell_atoms_to_suppress)
   end subroutine

   subroutine read_unit_cell_offset(self)
   ! Read the add criteria, whether to add atoms by whole clusters
   ! within a certain radius of the starting fragment, or by
   ! individual atoms within a certain distance of the starting
   ! fragment.
      type(CLUSTER_TYPE), INOUT :: self
      VEC(REAL,3) :: offset
      call read_(stdin,offset)
      call set_unit_cell_offset_(self,offset)
   end subroutine

   subroutine read_generation_method(self)
   ! Read the add criteria, whether to add atoms by whole clusters
   ! within a certain radius of the starting fragment, or by
   ! individual atoms within a certain distance of the starting
   ! fragment.
      type(CLUSTER_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: generation_method
      call read_(stdin,generation_method)
      call set_generation_method_(self,generation_method)
   end subroutine

   subroutine read_defragment(self)
   ! Read whether to defragment the cluster at the boundaries. If set
   ! TRUE, the cluster ends are defragmented, i.e. any atoms which are
   ! bonded at the ends of the cluster are included into the cluster.
      type(CLUSTER_TYPE), INOUT :: self
      call read_(stdin,self%defragment)
   end subroutine

   subroutine read_radius(self)
   ! Read the radius of the cluster. Usually, all atoms with this distance of
   ! any atom in the fragment_geometry will appear in the cluster.
      type(CLUSTER_TYPE), INOUT :: self
      call read_quantity_(stdin,self%radius)
   end subroutine

   subroutine read_atom_density_cutoff(self)
   ! This number is used to define a cluster radius (see above). The
   ! radius is defined from it, as the *largest* distance from any
   ! atom in the asymmetric_cell_atom list where the atomic electron
   ! density becomes smaller than this number. It requires that slater
   ! or coppens bases are defined for the asymmetric_cell_atom's, and
   ! it requires that the generation method be set to
   ! "for_hirshfeld_surface". It is primarily used for clusters
   ! generated from a fragment, which will correctly produce the
   ! Hirshfeld surface.
      type(CLUSTER_TYPE), INOUT :: self
      call read_quantity_(stdin,self%atom_density_cutoff)
   end subroutine

   subroutine read_fractional_fragment(self)
   ! Read in the fragment geometry, in fractional coordinates.
   ! NOTE: All fragment atom positions must correspond to the
   ! positions of actual atoms in the crystal lattice.
      type(CLUSTER_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: fragment_geometry
      VEC(REAL,:), allocatable :: tmp
      INT :: n
   ENSURE(allocated(self%crystal),"CLUSTER:read_fractional_fragment ... no crystal defined")
      call read_all_(stdin,tmp)
      ENSURE(mod(size(tmp),3)==0,"CLUSTER:read_fractional_fragment ... # of elements not divisible by 3")
      n = size(tmp)/3
      fragment_geometry = reshape(tmp,[3,n])
      call set_fragment_geometry_(self,fragment_geometry)
      call destroy_(fragment_geometry)
      call destroy_(tmp)
   end subroutine

   subroutine read_atoms(self)
   ! Modify the atom list information
      type(CLUSTER_TYPE), INOUT :: self
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:read_atoms ... atom list not created!")
      call read_list_keywords_(self%asymmetric_unit_atom)

      ! Change coordinates to cartesian
      ! Errors (if any) are changed straightforwardly
      if (allocated(self%crystal)) call change_axis_system_to_(self%asymmetric_unit_atom,"cartesian",self%crystal%unit_cell)
   end subroutine


!  ===========================
!  Cluster generation routines
!  ===========================

   subroutine update_info(self)
   ! Make all the cluster information from an arbitrary
   ! .fragment_geometry.  NOTE: .set_defaults must be called 
   ! before this.
      type(CLUSTER_TYPE), INOUT :: self
   ENSURE(allocated(self%crystal),"CLUSTER:update_info ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_connection),"CLUSTER:update_info ... no unit cell connection table")
   ENSURE(allocated(self%fragment_geometry),"CLUSTER:update_info ... no fragment_geometry")
      if (self%info_made) return

      ! Depending on the generation_method, make starting cluster
      call make_occupation_list_(self)

      ! Generate the cluster
      select case (self%generation_method)

      case ("within_radius")
         call make_within_radius_(self)

      case ("for_hirshfeld_surface")
         call make_HS_radius_(self)
         call make_within_radius_(self)
         self%defragment = FALSE

      case ("for_unit_cell_density")
      ! .do_defragment
         call make_HS_radius_(self)
         call make_within_radius_(self)
         self%defragment = FALSE

      case ("offset_unit_cell_density")
         call make_HS_radius_(self)
         call make_within_radius_(self)
         self%defragment = FALSE
      end select

      ! Defragment/find molecules in the cluster
      if (self%defragment) then; call do_defragment_(self)
      else; call do_connectify_(self)
      end if

      ! Extract cordinates from occupation list
      call extract_geometry_(self)

      ! Finished ...
      self%info_made = TRUE
   end subroutine


!   make_NN_info :: leaky
!   ! Make all the cluster information from an arbitrary
!   ! .fragment_geometry for a periodic network solid.
!   ! NOTE: .set_defaults must be called before this.
!      self :: INOUT
!
!   ENSURE(.crystal.allocated,"no crystal")
!   ENSURE(.crystal.unit_cell_connection.allocated,"no unit cell connection table")
!   ENSURE(.fragment_geometry.allocated,"no fragment_geometry")
!      
!      H_n :: INT
!
!      if (.info_made) return
!
!      ! Depending on the generation_method, make starting cluster.
!      ! Should be one for each atom.
!      .make_occupation_list
!
!      ! Defragment/find molecules in the cluster
!      .do_NN_defragment(.occupation_list,1,H_n)
!
!      ! Extract cordinates from occupation list
!      stdout.show("Number of first hydrogen: ",H_n)
!      .put_occupation_list
!      stop
!      .extract_geometry
!
!      ! Finished ...
!      .info_made = TRUE
!
!   end
!
   PURE subroutine make_asym_occupation_list(self,a)
   ! Codify which atoms in the crystal are fragment atoms i.e. make
   ! the ".occupation_list". If atom "u" in the unit cell "(h1,h2,h3)"
   ! is occupied then the macro CLUSTER_AT_CODE(h1,h2,h3,u) appears in
   ! the array "list". The maximum value allowed for any h is
   ! CLUSTER_H_MAX i.e. we can't consider fragments atoms which lie in
   ! cells outside this range. This list allows easy comparison of
   ! which atoms are in the fragment by an integer equality test.
   ! NOTE: Also make the ".molecule_for_atom" list which tells which
   ! molecule the atom belongs to.
      type(CLUSTER_TYPE), INOUT :: self
      INT, IN :: a
      INT :: f,u,h000
   ENSURE(allocated(self%crystal),"CLUSTER:make_asym_occupation_list ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_geometry),"CLUSTER:make_asym_occupation_list ... no crystal unit cell")

      ! Create occupation list
      call destroy_(self%occupation_list)
      call create_(self%occupation_list,1)

      ! Set central cell
      h000 = CLUSTER_H_CODE(0,0,0)
      f = self%crystal%frag_atom_for_asym_atom(a)
      u = self%crystal%unit_cell_atom_for_frag_atom(f)

      ! Encode central cell
      ! NOTE: the list has exactly one element, for asymmetric unit
      ! atom "a"; indexing it by the unit cell atom "u" was an
      ! out-of-bounds write whenever "u" was not 1.
      self%occupation_list(1) = h000+CLUSTER_UA_CODE(u)
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
      type(CLUSTER_TYPE), INOUT :: self
      VEC(INT,3) :: h
      REAL :: radius,two_angstrom
      VEC(REAL,3) :: pos,b,t
      INT :: f,u,h000,uc
      MAT(REAL,:,:), allocatable :: fragment_geometry
   ENSURE(allocated(self%crystal),"CLUSTER:make_occupation_list ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_geometry),"CLUSTER:make_occupation_list ... no crystal unit cell")
      select case (self%generation_method)

      case ("within_radius","for_hirshfeld_surface","fragment")
         ENSURE(allocated(self%fragment_geometry),"CLUSTER:make_occupation_list ... no fragment_geometry")
         ENSURE(self%crystal%fragment_info_made,"CLUSTER:make_occupation_list ... no crystal fragment info")
         ENSURE(allocated(self%crystal%unit_cell_atom_for_frag_atom),"CLUSTER:make_occupation_list ... no cell-frag_atom array")
         ENSURE(allocated(self%crystal%unit_cell_shft_for_frag_atom),"CLUSTER:make_occupation_list ... no cell-frag_atom array")

         ! Get fragment offset i.e. center
         pos = mean_column_vector_(self%fragment_geometry)
         self%fragment_offset = int(pos*(ONE-TOL(8)))  ! shrink toward zero: FP-robust at cell boundary

         ! Create occupation list
         call destroy_(self%occupation_list)
         call create_(self%occupation_list,size(self%fragment_geometry,2))

         ! Loop on fragment atom
         do f = 1,size(self%fragment_geometry,2)

            ! Get unit cell atom & shift
            u = self%crystal%unit_cell_atom_for_frag_atom(f)
            h = self%crystal%unit_cell_shft_for_frag_atom(:,f)

            ! Shift relative to recentered unit cell
            h = h+self%fragment_offset
            ENSURE(all(h>=-CLUSTER_H_MAX),"CLUSTER:make_occupation_list ... frag too wide, h = "//trim(to_concatenated_str_(h)))
            ENSURE(all(h<=CLUSTER_H_MAX),"CLUSTER:make_occupation_list ... frag too wide, h = "//trim(to_concatenated_str_(h)))

            ! Sanity check
            ENSURE(u/=0,"CLUSTER:make_occupation_list ... position of fragment atom "//trim(to_str_(f))//" not found in unit cell")
         ! if (u==0) then
         !    WARN("position of fragment atom "//f.to_str.trim//" not found in unit cell")
         !    stdout.show("fragment pos =",pos)
         !    stdout.text("unit cell geometry:")
         !    stdout.put(transpose(.crystal.unit_cell_geometry))
         !    DIE("position of fragment atom "//f.to_str.trim//" not found in unit cell")
         ! end

            ! Offset h to apply to "u" to get frag atom "f".
            ! Must reverse h. See above.
            h = -h

            ! Get occ code ...
            self%occupation_list(f) = CLUSTER_AT_CODE(h(1),h(2),h(3),u)
         end do

      case ("unit_cell")

         ! Create occupation list
         call destroy_(self%occupation_list)
         call create_(self%occupation_list,self%crystal%n_unit_cell_atoms)

         ! Temporary
         call destroy_(fragment_geometry)
         call create_(fragment_geometry,3,self%crystal%n_unit_cell_atoms)

         ! Set central cell
         h000 = CLUSTER_H_CODE(0,0,0)
         do u = 1,self%crystal%n_unit_cell_atoms

            ! Encode central cell
            self%occupation_list(u) = h000+CLUSTER_UA_CODE(u)

            ! Set fragment to unit cell
            pos = self%crystal%unit_cell_geometry(:,u)
            fragment_geometry(:,u) = pos
         end do

         ! Set fragment and make sure there is no offset
         call set_fragment_geometry_(self,fragment_geometry)
         self%fragment_offset = 0

         ! Clean
         call destroy_(fragment_geometry)

      case ("unit_cell_plus","for_unit_cell_density")

         ! Create occupation list
         call destroy_(self%occupation_list)
         call create_(self%occupation_list,self%crystal%n_unit_cell_atoms)

         ! Temporary
         call destroy_(fragment_geometry)
         call create_(fragment_geometry,3,self%crystal%n_unit_cell_atoms)

         ! Set central cell
         h000 = CLUSTER_H_CODE(0,0,0)
         do u = 1,self%crystal%n_unit_cell_atoms

            ! Encode central cell
            self%occupation_list(u) = h000+CLUSTER_UA_CODE(u)

            ! Set fragment to unit cell
            pos = self%crystal%unit_cell_geometry(:,u)
            fragment_geometry(:,u) = pos
         end do

         ! Set fragment and make sure there is no offset
         call set_fragment_geometry_(self,fragment_geometry)
         self%fragment_offset = 0

         ! Clean
         call destroy_(fragment_geometry)

         ! Go two angstrom from unit cell edges for unit cell densities
         ! if the setting is "for_unit_cell_density"
         two_angstrom = TWO
         two_angstrom = from_units_(two_angstrom,"angstrom")
         if (self%generation_method=="unit_cell_plus") then; radius = self%radius
         else; radius = max(TWO,self%radius)
         end if

         ! Fractions "b" along cell axes; and complements "t"
         b = radius/self%crystal%unit_cell%length
         ENSURE(all(b<=ONE),"CLUSTER:make_occupation_list ... radius around unit cell exceeds at least one cell dimension")
         t = ONE-b

         ! Unit cell code
         uc = 0
         do u = 1,self%crystal%n_unit_cell_atoms
            pos = self%crystal%unit_cell_geometry(:,u)
            uc = CLUSTER_UA_CODE(u)

            ! FIX h1
            if (pos(1)<b(1)) call append_(self%occupation_list,CLUSTER_H_CODE(1,0,0)+uc)
            if (pos(2)<b(2)) call append_(self%occupation_list,CLUSTER_H_CODE(0,1,0)+uc)
            if (pos(3)<b(3)) call append_(self%occupation_list,CLUSTER_H_CODE(0,0,1)+uc)
            if (pos(1)>t(1)) call append_(self%occupation_list,CLUSTER_H_CODE(-1,0,0)+uc)
            if (pos(2)>t(2)) call append_(self%occupation_list,CLUSTER_H_CODE(0,-1,0)+uc)
            if (pos(3)>t(3)) call append_(self%occupation_list,CLUSTER_H_CODE(0,0,-1)+uc)
            if (pos(1)<b(1) AND pos(2)<b(2)) call append_(self%occupation_list,CLUSTER_H_CODE(1,1,0)+uc)
            if (pos(1)<b(1) AND pos(3)<b(3)) call append_(self%occupation_list,CLUSTER_H_CODE(1,0,1)+uc)
            if (pos(2)<b(2) AND pos(3)<b(3)) call append_(self%occupation_list,CLUSTER_H_CODE(0,1,1)+uc)
            if (pos(1)>t(1) AND pos(2)<b(2)) call append_(self%occupation_list,CLUSTER_H_CODE(-1,1,0)+uc)
            if (pos(1)>t(1) AND pos(3)<b(3)) call append_(self%occupation_list,CLUSTER_H_CODE(-1,0,1)+uc)
            if (pos(2)>t(2) AND pos(3)<b(3)) call append_(self%occupation_list,CLUSTER_H_CODE(0,-1,1)+uc)
            if (pos(1)<b(1) AND pos(2)>t(2)) call append_(self%occupation_list,CLUSTER_H_CODE(1,-1,0)+uc)
            if (pos(1)<b(1) AND pos(3)>t(3)) call append_(self%occupation_list,CLUSTER_H_CODE(1,0,-1)+uc)
            if (pos(2)<b(2) AND pos(3)>t(3)) call append_(self%occupation_list,CLUSTER_H_CODE(0,1,-1)+uc)
            if (pos(1)>t(1) AND pos(2)>t(2)) call append_(self%occupation_list,CLUSTER_H_CODE(-1,-1,0)+uc)
            if (pos(1)>t(1) AND pos(3)>t(3)) call append_(self%occupation_list,CLUSTER_H_CODE(-1,0,-1)+uc)
            if (pos(2)>t(2) AND pos(3)>t(3)) call append_(self%occupation_list,CLUSTER_H_CODE(0,-1,-1)+uc)
            if (pos(1)<b(1) AND pos(2)<b(2) AND pos(3)<b(3)) call append_(self%occupation_list,CLUSTER_H_CODE(1,1,1)+uc)
            if (pos(1)>t(1) AND pos(2)<b(2) AND pos(3)<b(3)) call append_(self%occupation_list,CLUSTER_H_CODE(-1,1,1)+uc)
            if (pos(1)<b(1) AND pos(2)>t(2) AND pos(3)<b(3)) call append_(self%occupation_list,CLUSTER_H_CODE(1,-1,1)+uc)
            if (pos(1)>t(1) AND pos(2)>t(2) AND pos(3)<b(3)) call append_(self%occupation_list,CLUSTER_H_CODE(-1,-1,1)+uc)
            if (pos(1)<b(1) AND pos(2)<b(2) AND pos(3)>t(3)) call append_(self%occupation_list,CLUSTER_H_CODE(1,1,-1)+uc)
            if (pos(1)>t(1) AND pos(2)<b(2) AND pos(3)>t(3)) call append_(self%occupation_list,CLUSTER_H_CODE(-1,1,-1)+uc)
            if (pos(1)<b(1) AND pos(2)>t(2) AND pos(3)>t(3)) call append_(self%occupation_list,CLUSTER_H_CODE(1,-1,-1)+uc)
            if (pos(1)>t(1) AND pos(2)>t(2) AND pos(3)>t(3)) call append_(self%occupation_list,CLUSTER_H_CODE(-1,-1,-1)+uc)
         end do

      case ("offset_unit_cell","offset_unit_cell_density")

         ! Create occupation list
         call destroy_(self%occupation_list)
         call create_(self%occupation_list,self%crystal%n_unit_cell_atoms)

         ! Temporary
         call destroy_(fragment_geometry)
         call create_(fragment_geometry,3,self%crystal%n_unit_cell_atoms)
         do u = 1,self%crystal%n_unit_cell_atoms

            ! Unit cell position
            pos = self%crystal%unit_cell_geometry(:,u)

            ! Set central cell
            h = 0

            ! Apply the offset
            if (pos(1)<self%unit_cell_offset(1)) h(1) = +1
            if (pos(2)<self%unit_cell_offset(2)) h(2) = +1
            if (pos(3)<self%unit_cell_offset(3)) h(3) = +1
            if (pos(1)>ONE+self%unit_cell_offset(1)) h(1) = -1
            if (pos(2)>ONE+self%unit_cell_offset(2)) h(2) = -1
            if (pos(3)>ONE+self%unit_cell_offset(3)) h(3) = -1

            ! Get occ code ...
            self%occupation_list(u) = CLUSTER_AT_CODE(h(1),h(2),h(3),u)

            ! Set frag geometry with offset
            fragment_geometry(:,u) = pos+h
         end do

         ! Set fragment and make sure there is no offset
         call set_fragment_geometry_(self,fragment_geometry)
         self%fragment_offset = 0

         ! Clean
         call destroy_(fragment_geometry)
      end select
   end subroutine

   subroutine make_HS_radius(self)
   ! This routine makes an appropriate value for the cluster .radius in the
   ! case where the .generation_method is "for_hirshfeld_surface". The radius is
   ! made so that the accuracy of any atomic term neglected is smaller than
   ! .atom_density_cutoff. The radius is defined to the nearest 0.02 bohr (or
   ! whatever is set as the default for the interpolator table spacing). Likewise,
   ! we can't cope with atom density cutoffs where the distances are larger than
   ! 30 bohr (or whatever is set as the default interpolator table length). To
   ! do this properly we'd have to solve numerical equations for exactly where
   ! the atom density equals the desired cutoff; that seems like too much
   ! effort.
      type(CLUSTER_TYPE), INOUT :: self
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:make_HS_radius ... no asymmetric cell atoms")
   ENSURE(self%atom_density_cutoff>ZERO,"CLUSTER:make_HS_radius ... atom density cutoff must be positve")
      self%radius = max_interpolator_table_length_(self%asymmetric_unit_atom,self%atom_density_cutoff)
   end subroutine

   PURE subroutine make_within_radius(self)
   ! Make the maximum unit cell offsets so that we get all atoms
   ! within a certain radius of the fragment geometry.
      type(CLUSTER_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: geom0,frag
      MAT(REAL,3,3) :: rcm
      VEC(REAL,3) :: pos,off,d,frac_radius
      VEC(INT,:), allocatable :: hlist,ulist,match
      VEC(BIN,:), allocatable :: uskip
      REAL :: d2,radius2
      BIN :: within_radius,has_frag_atoms,not000,offset,has_atoms_to_suppress
      INT :: n_unit_cell_atoms,h0,h1,h2,h3,hcode,code,k,u
   ENSURE(allocated(self%occupation_list),"CLUSTER:make_within_radius ... no occupation_list")
   ENSURE(allocated(self%fragment_geometry),"CLUSTER:make_within_radius ... no fragment_geometry")
   ENSURE(allocated(self%crystal),"CLUSTER:make_within_radius ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_geometry),"CLUSTER:make_within_radius ... no crystal unit_cell_geometry")

      ! Constants
      n_unit_cell_atoms = self%crystal%n_unit_cell_atoms
      radius2 = self%radius*self%radius
      rcm = self%crystal%unit_cell%direct_mx
      has_atoms_to_suppress = allocated(self%unit_cell_atoms_to_suppress)
      if (has_atoms_to_suppress) has_atoms_to_suppress = size(self%unit_cell_atoms_to_suppress)>0

      ! Cartesian fragment geometry
      call create_(frag,3,size(self%fragment_geometry,2))
      call to_product_of_(frag,rcm,self%fragment_geometry)

      ! Calculate offset to fragment
      offset = any(self%fragment_offset/=0)
      if (offset) then
         call to_product_of_(off,rcm,REALIFY(self%fragment_offset))
      end if

      ! Look this many cells away for cluster atoms ...
      frac_radius = self%radius/self%crystal%unit_cell%length
      self%h_max = ceiling(frac_radius+maxval(self%fragment_geometry,dim=2)-self%fragment_offset)
      self%h_min = floor(-frac_radius+minval(self%fragment_geometry,dim=2)-self%fragment_offset)

      ! Look at least one cell away ...
      self%h_max = max(self%h_max,[1,1,1])
      self%h_min = min(self%h_min,[-1,-1,-1])

      ! Maximum value of unit cell index
      ! and offset for unit cell coding
      h0 = CLUSTER_H_MAX
      ENSURE(all(self%h_min>=-h0),"CLUSTER:make_within_radius ... search too low,  h_min = "//trim(to_concatenated_str_(self%h_min)))
      ENSURE(all(self%h_max<=h0),"CLUSTER:make_within_radius ... search too high, h_max = "//trim(to_concatenated_str_(self%h_max)))

      ! Get the (0,0,0) unit cell offset by .fragment_offset in cartesians
      call create_(geom0,3,n_unit_cell_atoms)
      call to_product_of_(geom0,rcm,self%crystal%unit_cell_geometry)
      if (offset) geom0 = geom0+spread(off,2,n_unit_cell_atoms)

      ! Cells which fragment occupies
      call create_(match,size(self%fragment_geometry,2))
      call create_(hlist,size(self%fragment_geometry,2))
      hlist = CLUSTER_HC_FROM_CODE(self%occupation_list)

      ! Unit cell atoms which correspond to the fragment atoms
      call create_(ulist,size(self%fragment_geometry,2))
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)

      ! Skip fragment atoms in a particular unit cell
      call create_(uskip,n_unit_cell_atoms)

      ! Loop over allowed cells
      do h1 = self%h_min(1),self%h_max(1)
         do h2 = self%h_min(2),self%h_max(2)
            do h3 = self%h_min(3),self%h_max(3)

         ! Get cell displacement vector if needed
               not000 = h1/=0 OR h2/=0 OR h3/=0
               if (not000) call to_product_of_(off,rcm,REALIFY(([h1,h2,h3])))

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

         ! Loop over unit cell atoms in this cell
               do u = 1,n_unit_cell_atoms

            ! If there are fragment atoms here, skip them
                  if (has_frag_atoms) then
                     if (uskip(u)) cycle
                  end if
                  if (has_atoms_to_suppress) then
                     if (any(self%unit_cell_atoms_to_suppress==u)) cycle
                  end if
                  pos = geom0(:,u)
                  if (not000) pos = pos+off

            ! Test if this atom pos is within .radius
                  within_radius = FALSE
                  do k = 1,size(self%fragment_geometry,2)
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
   ! .crystal.unit_cell_connection table.
      type(CLUSTER_TYPE), INOUT :: self
      INT :: i,c,code,h1,h2,h3,u,k1,k2,k3,t,old,mi,mo
      VEC(INT,:), allocatable :: molecule_for_atom
      VEC(INT,:), allocatable :: con
   ENSURE(allocated(self%crystal),"CLUSTER:do_connectify ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_connection),"CLUSTER:do_connectify ... no unit cell connection table")

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
         if (size(self%crystal%unit_cell_connection(u)%element)==0) cycle

         ! Get unit cell atom offset for atom "i"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)

         ! Find atoms which connect to u
         do c = 1,size(self%crystal%unit_cell_connection(u)%element)

            ! List of connections
            con = self%crystal%unit_cell_connection(u)%element(c)%element

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
      call update_molecule_info_(self)
   end subroutine

   PURE subroutine do_defragment_0(self)
   ! Defragment the generated cluster and find the connected molecules
   ! using the .unit_cell_connection table.
      type(CLUSTER_TYPE), INOUT :: self
      INT :: i,c,code,h1,h2,h3,u,k1,k2,k3,t,old,mi,mo
      VEC(INT,:), allocatable :: molecule_for_atom
      VEC(INT,:), allocatable :: con
   ENSURE(allocated(self%occupation_list),"CLUSTER:do_defragment_0 ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:do_defragment_0 ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_connection),"CLUSTER:do_defragment_0 ... no unit cell connection table")

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
         if (size(self%crystal%unit_cell_connection(u)%element)==0) cycle

         ! Get unit cell atom offset for atom "i"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)

         ! Loop on atoms "c" connected to "u"
         do c = 1,size(self%crystal%unit_cell_connection(u)%element)

            ! List of connections
            con = self%crystal%unit_cell_connection(u)%element(c)%element

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
      call update_molecule_info_(self)
   end subroutine

   PURE subroutine do_defragment_1(self,occupation_list)
   ! Defragment the "occupation_list"
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,:), allocatable, INOUT :: occupation_list
      INT :: n_atoms,i,c,code
      INT :: h1,h2,h3,u
      INT :: k1,k2,k3,t,old
      VEC(INT,:), allocatable :: con
   ENSURE(allocated(occupation_list),"CLUSTER:do_defragment_1 ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:do_defragment_1 ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_connection),"CLUSTER:do_defragment_1 ... no unit cell connection table")

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
         if (size(self%crystal%unit_cell_connection(u)%element)==0) cycle

         ! Get unit cell atom offset "h"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)

         ! Loop on atoms "c" connected to "u"
         do c = 1,size(self%crystal%unit_cell_connection(u)%element)

            ! List of connections
            con = self%crystal%unit_cell_connection(u)%element(c)%element

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

   PURE subroutine do_NN_defragment(self,occupation_list,level,nc_atoms)
   ! Defragment the "occupation_list" for each atom, modelling nearest
   ! "level" neighbours with atoms, and the next layer with hydrogen
   ! atoms, nc_atoms is the number of non-hydrogenic capping atoms.
      type(CLUSTER_TYPE), INOUT :: self
      VEC(INT,:), allocatable, INOUT :: occupation_list
      INT, IN :: level
      INT, OUT :: nc_atoms
      INT :: n_cluster_atoms,i,c
      INT :: code,u,l,t,old
      INT :: h1,h2,h3
      INT :: k1,k2,k3
      VEC(INT,:), allocatable :: con
   ENSURE(allocated(occupation_list),"CLUSTER:do_NN_defragment ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:do_NN_defragment ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_connection),"CLUSTER:do_NN_defragment ... no unit cell connection table")

      ! Initial no. of atoms
      nc_atoms = 1
      n_cluster_atoms = 1

      ! Loop on all atoms "i"
      i = 0
      l = 0
      do

         ! Stop once "level" shells have been added; the shell after
         ! that is what gets replaced by capping hydrogens.
         if (l>level) exit
         i = i+1

         ! Loop for atoms and nearest "l" atoms.
         if (i<=nc_atoms) then

            ! Info for atom "i"
            code = occupation_list(i)  ! Occupation code
            u = CLUSTER_UA_FROM_CODE(code)  ! Unit cell atom

            ! No connections to this atom? next ...
            if (size(self%crystal%unit_cell_connection(u)%element)==0) cycle

            ! Get unit cell atom offset "h"
            h1 = CLUSTER_H1_FROM_CODE(code)
            h2 = CLUSTER_H2_FROM_CODE(code)
            h3 = CLUSTER_H3_FROM_CODE(code)

            ! Loop on atoms "c" connected to "u"
            do c = 1,size(self%crystal%unit_cell_connection(u)%element)

               ! List of connections
               con = self%crystal%unit_cell_connection(u)%element(c)%element

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
            end do
            if (i>=nc_atoms) then
               l = l+1
               !check if "level" atom level has been reached
               if (l<=level) then
                  nc_atoms = size(occupation_list)
               end if
            end if
         end if
      end do

      ! The cluster is the central atom, its "level" shells of
      ! neighbours, and the capping shell beyond them. Treat it as a
      ! single molecule whose only fragment atom is the central one --
      ! that is the atom whose form factor is wanted.
      self%n_atoms = size(occupation_list)
      call destroy_(self%molecule_for_atom)
      call destroy_(self%is_fragment_atom)
      call create_(self%is_fragment_atom,self%n_atoms)
      call create_(self%molecule_for_atom,self%n_atoms)
      self%molecule_for_atom = 1
      self%is_fragment_atom = FALSE
      self%is_fragment_atom(1) = TRUE
      self%info_made = TRUE
   end subroutine

   PURE subroutine do_defragment_uc_atom_0(self,u,geometry)
   ! Defragment a single atom "u" in the unit cell and produce a
   ! "geometry" in crystal coordinates.
      type(CLUSTER_TYPE), IN :: self
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
      type(CLUSTER_TYPE), IN :: self
      INT, IN :: u
      INT, OUT :: symop
      VEC(INT,:), OUT :: par
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: mass
      VEC(INT,:), allocatable :: occupation_list

      ! Get symop for cell atom "u"
      symop = self%crystal%asym_symop_for_unit_cell_atom(u)

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
      type(CLUSTER_TYPE), IN :: self
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
      type(CLUSTER_TYPE), IN :: self
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

   PURE subroutine update_molecule_info(self)
   ! Sort the cluster into molecules whose indices appear in ascending
   ! order; also store the atom indices in each molecule.
      type(CLUSTER_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: unique
      INT :: mi,mj,a,m
   ENSURE(allocated(self%molecule_for_atom),"CLUSTER:update_molecule_info ... no molecule_for_atom")

      ! Get molecule indices & sort ascdending
      call to_unique_elements_of_(unique,self%molecule_for_atom)
      call sort_(unique)

      ! Number of molecules in cluster
      self%n_molecules = size(unique)

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
      type(CLUSTER_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: li,lj
      VEC(INT,:), allocatable :: ci,cj,ui,uj,ai,aj
      VEC(INT,:), allocatable :: permutation,new
      INT :: mi,mj,ni,s,si,sj
      INT :: ic,i0,i1,i2,i3
      INT :: jc,j0,j1,j2,j3
      VEC(REAL,3) :: pi,pj,ps
      VEC(INT,3) :: shift
      BIN :: related
   ENSURE(allocated(self%molecule_for_atom),"CLUSTER:make_unique_molecule_info ... no molecule_for_atom")

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
         if (self%unique_molecule_for(mi)>0) cycle
         li = self%molecule(mi)%element
         ni = size(li)
         call create_(ai,ni)
         call create_(ui,ni)
         call create_(ci,ni)
         ci = self%occupation_list(li)
         ui = CLUSTER_UA_FROM_CODE(ci)
         ai = self%crystal%asym_atom_for_unit_cell_atom(ui)
         si = self%crystal%asym_symop_for_unit_cell_atom(ui(1))
         si = self%crystal%spacegroup%inverse_of_symops(si)
         ic = ci(1)
         call destroy_(ci)
         call destroy_(ui)
         i0 = CLUSTER_UA_FROM_CODE(ic)
         i1 = CLUSTER_H1_FROM_CODE(ic)
         i2 = CLUSTER_H2_FROM_CODE(ic)
         i3 = CLUSTER_H3_FROM_CODE(ic)
         pi = self%crystal%unit_cell_geometry(:,i0)+[i1,i2,i3]+self%fragment_offset

         ! Find matching molecules
         do mj = mi+1,self%n_molecules
            lj = self%molecule(mj)%element

            ! Found?
            if (self%unique_molecule_for(mj)>0) cycle

            ! Same # on atoms?
            if (size(lj)/=ni) cycle
            call create_(aj,ni)
            call create_(uj,ni)
            call create_(cj,ni)
            cj = self%occupation_list(lj)
            uj = CLUSTER_UA_FROM_CODE(cj)
            aj = self%crystal%asym_atom_for_unit_cell_atom(uj)
            sj = self%crystal%asym_symop_for_unit_cell_atom(uj(1))
            call destroy_(cj)
            call destroy_(uj)

            ! Match asym atoms
            call create_(permutation,ni)
            call is_permutation_of_(ai,aj,related,permutation)
            if (related) then

               ! New order
               new = lj(permutation)

               ! First atom in mj (matched to mi)
               jc = self%occupation_list(new(1))

               ! Position of first atom in mj
               j0 = CLUSTER_UA_FROM_CODE(jc)
               j1 = CLUSTER_H1_FROM_CODE(jc)
               j2 = CLUSTER_H2_FROM_CODE(jc)
               j3 = CLUSTER_H3_FROM_CODE(jc)
               pj = self%crystal%unit_cell_geometry(:,j0)+[j1,j2,j3]+self%fragment_offset

               ! Symop which maps mi->mj
               s = self%crystal%spacegroup%product_of_symops(sj,si)

               ! Get shift
               ps = pi
               call transform_position_(self%crystal%spacegroup,ps,s)
               ps = pj-ps
               shift = nint(ps)

               ! Unique molecule
               self%unique_molecule_for(mj) = mi

               ! New symcode
               self%unique_symcode_for(mj) = CLUSTER_AT_CODE(shift(1),shift(2),shift(3),s)

               ! Reorder atoms in mj to be consistent with mi
               self%occupation_list(lj) = self%occupation_list(new)

               ! Reset
               self%molecule(mj)%element = new
               call destroy_(new)
            else

               ! Symmetry unique molecule
               self%n_unique_molecules = self%n_unique_molecules+1
               self%unique_molecule_for(mi) = self%n_unique_molecules
            end if
            call destroy_(permutation)
            call destroy_(aj)
         end do
         call destroy_(ai)
      end do

   ! stdout.text("=============================")
   ! stdout.text("Occupation list AFTER  unique")
   ! stdout.text("=============================")
   ! .put_occupation_list
   end subroutine


!  ==================================================
!  Extract geometry & info from coded occupation list
!  ==================================================

   PURE subroutine extract_geometry_0(self)
   ! Extract the .geometry (fractional) from the final .occupation list.
   ! . Place connected molecules in order
   ! . Set the .is_fragment_atom flag array
      type(CLUSTER_TYPE), INOUT :: self
      INT :: a,m,i,code,h1,h2,h3,u
      VEC(type(EVEC_INT_TYPE),:), allocatable :: atom_connection
      VEC(INT,:), allocatable :: new_order
      BIN :: connected
   ENSURE(allocated(self%occupation_list),"CLUSTER:extract_geometry_0 ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:extract_geometry_0 ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_geometry),"CLUSTER:extract_geometry_0 ... no crystal unit cell geometry")

      ! No of atoms to extract
      self%n_atoms = size(self%occupation_list)

      ! Extracted geometry
      call destroy_(self%geometry)
      call create_(self%geometry,3,self%n_atoms)

      ! Clear flags telling if fragment atom
      call destroy_(self%is_fragment_atom)
      call create_(self%is_fragment_atom,self%n_atoms)

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
            code = self%occupation_list(i)

            ! Get unit cell atom "u" and cell offset "h"
            h1 = CLUSTER_H1_FROM_CODE(code)
            h2 = CLUSTER_H2_FROM_CODE(code)
            h3 = CLUSTER_H3_FROM_CODE(code)
            u = CLUSTER_UA_FROM_CODE(code)

            ! Get the cluster atom coordinate
            self%geometry(:,i) = self%crystal%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset

            ! Set flag if fragment atom
            if (i<=size(self%fragment_geometry,2)) then; self%is_fragment_atom(i) = TRUE
            else; self%is_fragment_atom(i) = FALSE
            end if
         end do
      end do

      ! Now reorder
      self%occupation_list(new_order) = self%occupation_list
      self%geometry(:,new_order) = self%geometry
      self%molecule_for_atom(new_order) = self%molecule_for_atom
      self%is_fragment_atom(new_order) = self%is_fragment_atom

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

   PURE subroutine extract_geometry_1(self,occupation_list,geometry)
   ! Extract the geometry (fractional) from an "occupation_list".
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,:), IN :: occupation_list
      MAT(REAL,:,:), allocatable, OUT :: geometry
      INT :: n_atoms,i,code,h1,h2,h3,u
   ENSURE(allocated(self%crystal),"CLUSTER:extract_geometry_1 ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_geometry),"CLUSTER:extract_geometry_1 ... no crystal unit cell geometry")

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
         geometry(:,i) = self%crystal%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset
      end do
   end subroutine

   PURE subroutine extract_atom_info_0(self,occupation_list,par,pos,mass)
   ! From a "occupation_list", extract the *cartesian* atom positions
   ! "pos", the index of the asymmetric-parent-atom in "par" and the
   ! atomic "mass" for each atom.
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,:), IN :: occupation_list
      VEC(INT,:), OUT :: par
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: mass
      INT :: i,code,u,h1,h2,h3,p
      VEC(REAL,3) :: q
   ENSURE(allocated(self%crystal),"CLUSTER:extract_atom_info_0 ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_geometry),"CLUSTER:extract_atom_info_0 ... no crystal unit cell geometry")
   ENSURE(allocated(self%crystal%asym_atom_for_unit_cell_atom),"CLUSTER:extract_atom_info_0 ... no asym_atom_for_unit_cell_atom")
   ENSURE(allocated(self%crystal%frag_atom_for_unit_cell_atom),"CLUSTER:extract_atom_info_0 ... no frag_atom_for_unit_cell_atom")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:extract_atom_info_0 ... no asymmetric_cell_atom data")
   ENSURE(size(mass)==size(occupation_list),"CLUSTER:extract_atom_info_0 ... mass, wrong size")
   ENSURE(size(pos,1)==3 AND size(pos,2)==size(occupation_list),"CLUSTER:extract_atom_info_0 ... pos, wrong size")

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
         q = self%crystal%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset
         call to_product_of_(pos(:,i),self%crystal%unit_cell%direct_mx,q)

         ! Get parent asymmetric atom and symop
         p = self%crystal%asym_atom_for_unit_cell_atom(u)
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
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,:), IN :: occupation_list
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: charge,dipole
      VEC(INT,:), optional, OUT :: frag_atom
      MAT3(REAL,:,:,:), allocatable :: seitz
      INT :: i,l,code,h1,h2,h3,u,p,s
      VEC(REAL,3) :: q
   ENSURE(allocated(self%crystal),"CLUSTER:extract_atom_info_1 ... no crystal")
   ENSURE(self%crystal%fragment_info_made,"CLUSTER:extract_atom_info_1 ... no crystal fragment_info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:extract_atom_info_1 ... no asymmetric_cell_atom data")
   ENSURE(has_dipoles_(self%asymmetric_unit_atom),"CLUSTER:extract_atom_info_1 ... no atomic dipoles, did you calc them?")
   ENSURE(size(pos,1)==3 AND size(pos,2)==size(occupation_list),"CLUSTER:extract_atom_info_1 ... pos, wrong size")
   ENSURE(size(charge)==size(occupation_list),"CLUSTER:extract_atom_info_1 ... charge, wrong size")
   ENSURE(size(dipole)==3*size(occupation_list),"CLUSTER:extract_atom_info_1 ... dipole, wrong size")

      ! Symops
      seitz = xyz_seitz_matrices_(self%crystal)

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
         q = self%crystal%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset
         call to_product_of_(pos(:,i),self%crystal%unit_cell%direct_mx,q)

         ! Parent asymmetric atom and symop
         p = self%crystal%asym_atom_for_unit_cell_atom(u)
         s = self%crystal%asym_symop_for_unit_cell_atom(u)

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
         frag_atom(i) = self%crystal%frag_atom_for_unit_cell_atom(u)
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
      type(CLUSTER_TYPE), IN :: self
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
   ENSURE(allocated(self%crystal),"CLUSTER:extract_molecule_atom_info ... no crystal")
   ENSURE(self%crystal%fragment_info_made,"CLUSTER:extract_molecule_atom_info ... no crystal fragment info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:extract_molecule_atom_info ... no asymmetric_cell_atom data")
   ENSURE(has_dipoles_(self%asymmetric_unit_atom),"CLUSTER:extract_molecule_atom_info ... no atomic dipoles, did you calc them?")
   ENSURE(has_polarisabilities_(self%asymmetric_unit_atom),"CLUSTER:extract_molecule_atom_info ... no atomic polarisabilities")
   ENSURE(size(charge)==size(occupation_list),"CLUSTER:extract_molecule_atom_info ... charge, wrong size")
   ENSURE(size(mass)==size(occupation_list),"CLUSTER:extract_molecule_atom_info ... mass, wrong size")
   ENSURE(all(shape(pos)==[3,size(occupation_list)]),"CLUSTER:extract_molecule_atom_info ... pos, wrong size")
   ENSURE(all(shape(dipole)==[3,size(occupation_list)]),"CLUSTER:extract_molecule_atom_info ... dipole, wrong size")
   ENSURE(all(shape(alpha_q)==[3,size(occupation_list)]),"CLUSTER:extract_molecule_atom_info ... alpha_q, wrong size")
   ENSURE(all(shape(alpha_m)==[3,3,size(occupation_list)]),"CLUSTER:extract_molecule_atom_info ... alpha_m, wrong size")

      ! Transposed/Inverse symops
      inverse_seitz = transposed_xyz_seitz_matrices_(self%crystal)

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
         q = self%crystal%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset
         call to_product_of_(pos(:,i),self%crystal%unit_cell%direct_mx,q)

         ! Set parent asymmetric atom and symop
         p = self%crystal%asym_atom_for_unit_cell_atom(u)
         s = self%crystal%asym_symop_for_unit_cell_atom(u)
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

         ! stdout.show("dipole parent =",.asymmetric_unit_atom(p).dipole)
         ! stdout.show("dipole        =",dipole(:,i))

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
      type(CLUSTER_TYPE), IN :: self
      VEC(REAL,3) :: res
   ENSURE(allocated(self%geometry),"CLUSTER:cluster_width ... no fragment geometry")
      res = max_abs_column_difference_(self%geometry)
   end function

   PURE subroutine create_fragment_atom_list(self,fragment_atom)
   ! Create the "fragment_atom" list.
      type(CLUSTER_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: fragment_atom
   ENSURE(allocated(self%fragment_geometry),"CLUSTER:create_fragment_atom_list ... no fragment atoms")
   ENSURE(allocated(self%is_fragment_atom),"CLUSTER:create_fragment_atom_list ... no fragment atoms")
   ENSURE(allocated(self%crystal),"CLUSTER:create_fragment_atom_list ... no crystal info")
      call create_(fragment_atom,size(self%fragment_geometry,2))
      call make_fragment_atom_list_(self,fragment_atom)
   end subroutine

   PURE subroutine make_fragment_atom_list(self,fragment_atom)
   ! Make the "fragment_atom" list, a VEC{ATOM}
   ! WARNING: set info_made to FALSE in "fragment_atom" parent
   !          molecule, if there is one.
      type(CLUSTER_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), OUT :: fragment_atom
      VEC(INT,:), allocatable :: ulist
      MAT3(REAL,:,:,:), allocatable :: seitz
      INT :: a,f,u,p,s
   ENSURE(self%n_atoms>0,"CLUSTER:make_fragment_atom_list ... no atoms")
   ENSURE(allocated(self%fragment_geometry),"CLUSTER:make_fragment_atom_list ... no fragment atoms")
   ENSURE(allocated(self%is_fragment_atom),"CLUSTER:make_fragment_atom_list ... no is_fragment_atom")
   ENSURE(count(self%is_fragment_atom)==size(self%fragment_geometry,2),"CLUSTER:make_fragment_atom_list ... inconsistent is_fragment_atom")
   ENSURE(allocated(self%geometry),"CLUSTER:make_fragment_atom_list ... no geometry")
   ENSURE(allocated(self%occupation_list),"CLUSTER:make_fragment_atom_list ... no occupation_list")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:make_fragment_atom_list ... no asymmetric_cell_atom")
   ENSURE(allocated(self%crystal),"CLUSTER:make_fragment_atom_list ... no crystal info")
   ENSURE(allocated(self%crystal%asym_atom_for_unit_cell_atom),"CLUSTER:make_fragment_atom_list ... no asym_atom-cell_atom")
   ENSURE(has_cartesian_axes_(self%asymmetric_unit_atom),"CLUSTER:make_fragment_atom_list ... pADPs must be in cartn axis system")

      ! Unit cell atoms which correspond to the fragment atoms
      call create_(ulist,self%n_atoms)
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)

      ! Seitz matrices
      seitz = xyz_seitz_matrices_(self%crystal)

      ! Fragment atom
      f = 0

      ! Loop over cluster atoms "a"
      do a = 1,self%n_atoms
         if (NOT self%is_fragment_atom(a)) cycle

         ! Fragment atom
         f = f+1

         ! Extract unit cell atom and asymmetric parent
         u = ulist(a)
         p = self%crystal%asym_atom_for_unit_cell_atom(u)
         s = self%crystal%asym_symop_for_unit_cell_atom(u)

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
      call change_axis_system_to_(fragment_atom,"cartesian",self%crystal%unit_cell)
   end subroutine

   PURE subroutine create_mol_atom_list(self,mol_atom,m)
   ! Create the "mol_atom" ATOM list for molecule "m".
      type(CLUSTER_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: mol_atom
      INT, IN :: m
   ENSURE(allocated(self%molecule),"CLUSTER:create_mol_atom_list ... no molecule list")
   ENSURE(size(self%molecule)>=m,"CLUSTER:create_mol_atom_list ... molecule m too large")
   ENSURE(allocated(self%molecule(m)%element),"CLUSTER:create_mol_atom_list ... no molecule m atom indices")
      call create_(mol_atom,size(self%molecule(m)%element))
      call make_mol_atom_list_(self,mol_atom,m)
   end subroutine

   PURE subroutine make_mol_atom_list(self,mol_atom,m)
   ! Make the "fragment_atom" list, a VEC{ATOM}
      type(CLUSTER_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), OUT :: mol_atom
      INT, IN :: m
      VEC(INT,:), allocatable :: ulist
      MAT3(REAL,:,:,:), allocatable :: seitz
      INT :: f,a,u,p,s
   ENSURE(allocated(self%geometry),"CLUSTER:make_mol_atom_list ... no geometry")
   ENSURE(allocated(self%occupation_list),"CLUSTER:make_mol_atom_list ... no occupation_list")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:make_mol_atom_list ... no asymmetric_cell_atom")
   ENSURE(allocated(self%crystal),"CLUSTER:make_mol_atom_list ... no crystal info")
   ENSURE(allocated(self%crystal%asym_atom_for_unit_cell_atom),"CLUSTER:make_mol_atom_list ... no asym_atom-cell_atom")
   ENSURE(has_cartesian_axes_(self%asymmetric_unit_atom),"CLUSTER:make_mol_atom_list ... pADPs must be in cartn axis system")

      ! Unit cell atoms which correspond to the molecule's atoms
      call create_(ulist,self%n_atoms)
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)

      ! Seitz matrices
      seitz = xyz_seitz_matrices_(self%crystal)

      ! Loop over molecule m atoms "f"
      do f = 1,size(self%molecule(m)%element)
         a = self%molecule(m)%element(f)

         ! Extract unit cell atom and asymmetric parent
         u = ulist(a)
         p = self%crystal%asym_atom_for_unit_cell_atom(u)
         s = self%crystal%asym_symop_for_unit_cell_atom(u)

         ! Copy the parent atom
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
      call change_axis_system_to_(mol_atom,"cartesian",self%crystal%unit_cell)
   end subroutine

   PURE function fragment_atom_indices(self) result (res)
   ! Return the indices of the fragment atoms in the cluster.
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,size(self%fragment_geometry,2)) :: res
      INT :: a,f
   ENSURE(allocated(self%is_fragment_atom),"CLUSTER:fragment_atom_indices ... no fragment atoms")

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
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,self%n_atoms-size(self%fragment_geometry,2)) :: res
      INT :: a,f
   ENSURE(self%n_atoms>=size(self%fragment_geometry,2),"CLUSTER:nonfragment_atom_indices ... no nonfragment atoms")
   ENSURE(allocated(self%is_fragment_atom),"CLUSTER:nonfragment_atom_indices ... no fragment atoms")

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
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,size(self%fragment_geometry,2)) :: res
      INT :: f,i,code,u,a
   ENSURE(size(self%fragment_geometry,2)>0,"CLUSTER:crystal_asym_unit_atom_indices ... no fragment atoms")
   ENSURE(allocated(self%is_fragment_atom),"CLUSTER:crystal_asym_unit_atom_indices ... no is_fragment_atom")
   ENSURE(allocated(self%occupation_list),"CLUSTER:crystal_asym_unit_atom_indices ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:crystal_asym_unit_atom_indices ... no crystal")

      ! Loop over atoms
      f = 0
      do i = 1,self%n_atoms
         if (NOT self%is_fragment_atom(i)) cycle
         code = self%occupation_list(i)
         u = CLUSTER_UA_FROM_CODE(code)
         a = self%crystal%asym_atom_for_unit_cell_atom(u)
         f = f+1
         res(f) = a
      end do
   end function

   PURE function crystal_frag_atom_indices(self) result (res)
   ! Return the *crystal* fragment atom indices of the fragment atoms
   ! in the *cluster* (the two are not necessarily the same!)
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,size(self%fragment_geometry,2)) :: res
      INT :: f,i,code,u,a
   ENSURE(allocated(self%is_fragment_atom),"CLUSTER:crystal_frag_atom_indices ... no is_fragment_atom")
   ENSURE(allocated(self%occupation_list),"CLUSTER:crystal_frag_atom_indices ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:crystal_frag_atom_indices ... no crystal")

      ! Loop over atoms
      f = 0
      do i = 1,self%n_atoms
         if (NOT self%is_fragment_atom(i)) cycle
         code = self%occupation_list(i)
         u = CLUSTER_UA_FROM_CODE(code)
         a = self%crystal%frag_atom_for_unit_cell_atom(u)
         f = f+1
         res(f) = a
      end do
   end function

   PURE subroutine create_atom_list(self,atom)
   ! Create and make a new atom list for the cluster
   ! NOTE: basis sets are pointer copied!
      type(CLUSTER_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: atom
   ENSURE(allocated(self%fragment_geometry),"CLUSTER:create_atom_list ... no crystal fragment geometry")
   ENSURE(allocated(self%crystal),"CLUSTER:create_atom_list ... no crystal data")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:create_atom_list ... no atom data")
   ENSURE(self%n_atoms>0,"CLUSTER:create_atom_list ... no atoms in cluster")
      call create_(atom,self%n_atoms)
      call make_atom_list_(self,atom)
   end subroutine

   PURE subroutine make_atom_list_0(self,atom)
   ! Make a new atom list for the cluster. Note that the .asymmetric_unit_atom's
   ! must have ADP (thermal) tensors in the cartesian axis system; this will normally
   ! be the case for Tonto's internal representation ....
   ! NOTE: basis sets are pointer copied!
      type(CLUSTER_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), OUT :: atom
      VEC(INT,:), allocatable :: ulist
      MAT3(REAL,:,:,:), allocatable :: seitz
      INT :: a,u,p,s
   ENSURE(self%n_atoms>0,"CLUSTER:make_atom_list_0 ... no atoms in cluster")
   ENSURE(size(atom)==self%n_atoms,"CLUSTER:make_atom_list_0 ... wrong size, atomsr")
   ENSURE(allocated(self%geometry),"CLUSTER:make_atom_list_0 ... no geometry")
   ENSURE(allocated(self%occupation_list),"CLUSTER:make_atom_list_0 ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:make_atom_list_0 ... no crystal data")
   ENSURE(self%crystal%fragment_info_made,"CLUSTER:make_atom_list_0 ... no crystal fragment info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:make_atom_list_0 ... no asymmetric_unit_atom data")
   ENSURE(has_cartesian_axes_(self%asymmetric_unit_atom),"CLUSTER:make_atom_list_0 ... asymmetric_unit_atom not in cartesian axes")
   ENSURE(allocated(self%molecule_for_atom),"CLUSTER:make_atom_list_0 ... no molecule_for_atom info")

      ! Unit cell atoms which correspond to the fragment atoms
      call create_(ulist,size(self%occupation_list))
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)

      ! Seitz matrices
      seitz = xyz_seitz_matrices_(self%crystal)

      ! Loop over cluster atoms "a"
      do a = 1,self%n_atoms

         ! Extract unit cell atom, asymmetric parent and symop
         u = ulist(a)
         p = self%crystal%asym_atom_for_unit_cell_atom(u)
         s = self%crystal%asym_symop_for_unit_cell_atom(u)

         ! Copy the parent atom
         ! NOTE: .basis is a ptr copy
         atom(a) = self%asymmetric_unit_atom(p)

         ! Transform the pADPs (which must be in cartesians)
         call transform_pADP_vector_with_(atom(a),seitz(:,:,s))

         ! Reset cluster atom pos from .geometry(:,a)
         call set_position_(atom(a),self%geometry(:,a))
         call set_axis_system_to_(atom(a),"crystal")
         call set_group_(atom(a),self%molecule_for_atom(a))
      end do

      ! Clean
      call destroy_(seitz)
      call destroy_(ulist)

      ! Change pos to cartesian
      ! Errors (if any) are changed straightforwardly
      call change_axis_system_to_(atom,"cartesian",self%crystal%unit_cell)

      ! Update labels/groups
      call update_(atom)
   end subroutine

   PURE subroutine make_atom_list_1(self,atom,list)
   ! Make a new atom list for the cluster from the indices in "list".
   ! Note that the .asymmetric_unit_atom's must have ADP tensors
   ! in the cartesian axis system; this will normally be the case for
   ! Tonto's internal representation ....
   ! NOTE: basis sets are pointer copied!
      type(CLUSTER_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), OUT :: atom
      VEC(INT,:), IN :: list
      VEC(INT,:), allocatable :: ulist
      MAT3(REAL,:,:,:), allocatable :: seitz
      INT :: a,u,p,s
   ENSURE(size(atom)==size(list),"CLUSTER:make_atom_list_1 ... wrong size, atomsr")
   ENSURE(allocated(self%geometry),"CLUSTER:make_atom_list_1 ... no geometry")
   ENSURE(allocated(self%occupation_list),"CLUSTER:make_atom_list_1 ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:make_atom_list_1 ... no crystal data")
   ENSURE(self%crystal%fragment_info_made,"CLUSTER:make_atom_list_1 ... no crystal fragment info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:make_atom_list_1 ... no asymmetric_cell_atom data")
   ENSURE(has_cartesian_axes_(self%asymmetric_unit_atom),"CLUSTER:make_atom_list_1 ... pADPs must be in cartn axis system")

      ! Unit cell atoms which correspond to the *list* fragment atoms
      call create_(ulist,size(list))
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list(list))

      ! Seitz matrices
      seitz = xyz_seitz_matrices_(self%crystal)

      ! Loop over cluster atoms "a"
      do a = 1,size(ulist)

         ! Extract unit cell atom, asymmetric parent and symop
         u = ulist(a)
         p = self%crystal%asym_atom_for_unit_cell_atom(u)
         s = self%crystal%asym_symop_for_unit_cell_atom(u)

         ! Copy the parent atom
         atom(a) = self%asymmetric_unit_atom(p)

         ! Transform the pADPs (which must be in cartesians)
         call transform_pADP_vector_with_(atom(a),seitz(:,:,s))

         ! Reset cluster atom pos from .geometry(:,a)
         call set_position_(atom(a),self%geometry(:,a))
         call set_axis_system_to_(atom(a),"crystal")
      end do

      ! Clean
      call destroy_(seitz)
      call destroy_(ulist)

      ! Change pos to cartesian
      ! Errors (if any) are changed straightforwardly
      call change_axis_system_to_(atom,"cartesian",self%crystal%unit_cell)

      ! Update labels/groups
      call update_(atom)
   end subroutine

   PURE subroutine make_non_fragment_mu_charges(self,pos,charge)
   ! Make a list of the non-fragment atom positions and point charges
   ! derived from the atomic charges and dipoles only.
      type(CLUSTER_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: charge
      MAT3(REAL,:,:,:), allocatable :: seitz
      VEC(REAL,3) :: dipole
      REAL :: d,mu,q
      INT :: a,i,code,p,s
      INT :: h1,h2,h3,u
      VEC(REAL,3) :: r
   ENSURE(self%n_atoms>0,"CLUSTER:make_non_fragment_mu_charges ... no atoms in cluster")
   ENSURE(size(pos,2)==3*(self%n_atoms-size(self%fragment_geometry,2)),"CLUSTER:make_non_fragment_mu_charges ... wrong dim2, pos")
   ENSURE(size(charge)==size(pos,2),"CLUSTER:make_non_fragment_mu_charges ... wrong dim, charge")
   ENSURE(allocated(self%occupation_list),"CLUSTER:make_non_fragment_mu_charges ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:make_non_fragment_mu_charges ... no crystal data")
   ENSURE(self%crystal%fragment_info_made,"CLUSTER:make_non_fragment_mu_charges ... no crystal fragment info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:make_non_fragment_mu_charges ... no asymmetric_cell_atom data")

      ! Normal XYZ symops
      seitz = xyz_seitz_matrices_(self%crystal)
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
         r = self%crystal%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset
         pos(:,i) = matmul(self%crystal%unit_cell%direct_mx,r)

         ! Get parent asymmetric atom and symop
         p = self%crystal%asym_atom_for_unit_cell_atom(u)
         s = self%crystal%asym_symop_for_unit_cell_atom(u)

         ! Atomic charge
         charge(i) = self%asymmetric_unit_atom(p)%charge

         ! Atomic dipole
         dipole = matmul(seitz(:,:,s),self%asymmetric_unit_atom(p)%dipole)

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
      type(CLUSTER_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: pos
      VEC(REAL,:), OUT :: charge
      MAT3(REAL,:,:,:), allocatable :: seitz
      VEC(REAL,3) :: dipole,eval
      MAT(REAL,3,3) :: quadrupole,evec
      REAL :: d,mu,q
      INT :: a,i,code,h1,h2,h3,u,p,s
      VEC(REAL,3) :: r
   ENSURE(self%n_atoms>0,"CLUSTER:make_non_fragment_qq_charges ... no atoms in cluster")
   ENSURE(size(pos,2)==9*(self%n_atoms-size(self%fragment_geometry,2)),"CLUSTER:make_non_fragment_qq_charges ... wrong dim2, pos")
   ENSURE(size(charge)==size(pos,2),"CLUSTER:make_non_fragment_qq_charges ... wrong dim, charge")
   ENSURE(allocated(self%occupation_list),"CLUSTER:make_non_fragment_qq_charges ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:make_non_fragment_qq_charges ... no crystal data")
   ENSURE(self%crystal%fragment_info_made,"CLUSTER:make_non_fragment_qq_charges ... no crystal fragment info")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:make_non_fragment_qq_charges ... no asymmetric_cell_atom data")

      ! NOrmal XYZ symops
      seitz = xyz_seitz_matrices_(self%crystal)

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
         r = self%crystal%unit_cell_geometry(:,u)+[h1,h2,h3]+self%fragment_offset
         call to_product_of_(pos(:,i),self%crystal%unit_cell%direct_mx,r)

         ! Get parent asymmetric atom and symop
         p = self%crystal%asym_atom_for_unit_cell_atom(u)
         s = self%crystal%asym_symop_for_unit_cell_atom(u)

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
      type(CLUSTER_TYPE), IN :: self
      MAT(REAL,3,3), INOUT :: tensor
      VEC(INT,:), allocatable :: ulist
      MAT3(REAL,:,:,:), allocatable :: seitz
      MAT(REAL,3,3) :: tensor0
      MAT(REAL,3,3) :: tensor1
      INT :: m,a,u,s
   ENSURE(self%generation_method=="unit_cell","CLUSTER:add_UC_tensors_0 ... only for unit_cell clusters")
   ENSURE(self%n_atoms>0,"CLUSTER:add_UC_tensors_0 ... no atoms in cluster")
   ENSURE(allocated(self%occupation_list),"CLUSTER:add_UC_tensors_0 ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:add_UC_tensors_0 ... no crystal data")
   ENSURE(self%crystal%fragment_info_made,"CLUSTER:add_UC_tensors_0 ... no crystal fragment info")
   ENSURE(allocated(self%crystal%unit_cell_mol_for_atom),"CLUSTER:add_UC_tensors_0 ... no cell_mol_for_atom")

      ! The following code is inherited from CLUSTER
      tensor0 = tensor
      tensor = ZERO

      ! Unit cell atoms which correspond to the fragment atoms
      call create_(ulist,self%n_atoms)
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)

      ! Inverse XYZ symops
      seitz = transposed_xyz_seitz_matrices_(self%crystal)
      do m = 1,self%crystal%n_unit_cell_mols
         do a = 1,self%n_atoms
            if (self%crystal%unit_cell_mol_for_atom(a)/=m) cycle

         ! Get symop for this molecule
            u = ulist(a)
            s = self%crystal%asym_symop_for_unit_cell_atom(u)

         ! Use inverse symop  S^T to change U tensor basis
            call change_basis_to_(tensor0,tensor1,seitz(:,:,s))
            tensor = tensor+tensor1
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
      type(CLUSTER_TYPE), IN :: self
      MAT3(REAL,3,3,3), INOUT :: tensor
      VEC(INT,:), allocatable :: ulist
      MAT3(REAL,:,:,:), allocatable :: seitz
      MAT3(REAL,3,3,3) :: tensor0
      MAT3(REAL,3,3,3) :: tensor1
      INT :: m,a,u,s
   ENSURE(self%generation_method=="unit_cell","CLUSTER:add_UC_tensors_1 ... only for unit_cell clusters")
   ENSURE(self%n_atoms>0,"CLUSTER:add_UC_tensors_1 ... no atoms in cluster")
   ENSURE(allocated(self%occupation_list),"CLUSTER:add_UC_tensors_1 ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:add_UC_tensors_1 ... no crystal data")
   ENSURE(self%crystal%fragment_info_made,"CLUSTER:add_UC_tensors_1 ... no crystal fragment info")
   ENSURE(allocated(self%crystal%unit_cell_mol_for_atom),"CLUSTER:add_UC_tensors_1 ... no cell_mol_for_atom")

      ! The following code is inherited from CLUSTER
      tensor0 = tensor
      tensor = ZERO

      ! Unit cell atoms which correspond to the fragment atoms
      call create_(ulist,self%n_atoms)
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)

      ! Inverse XYZ symops
      seitz = transposed_xyz_seitz_matrices_(self%crystal)
      do m = 1,self%crystal%n_unit_cell_mols
         do a = 1,self%n_atoms
            if (self%crystal%unit_cell_mol_for_atom(a)/=m) cycle

         ! Get symop for this molecule
            u = ulist(a)
            s = self%crystal%asym_symop_for_unit_cell_atom(u)

         ! Use inverse symop  S^T to change U tensor basis
            call change_basis_to_(tensor0,tensor1,seitz(:,:,s))
            tensor = tensor+tensor1
            exit
         end do
      end do

      ! Clean
      call destroy_(seitz)
      call destroy_(ulist)
   end subroutine


!  ========================================
!  Cluster transformations on matrices, etc
!  ========================================

!   read_partition_factors :: leaky
!   ! Read the partition factors to be used. Note that the length of this array
!   ! must correspond to the number of atoms in the generated cluster in order to
!   ! be used. This cannot be checked at this point in the code.
!      stdin.read_ptr(.partition_factor)
!   end

!   make_partition_factors(matrix) :: leaky
!   ! Make the partition factors from the cluster-fragment mapping information.
!     matrix :: MAT{REAL}
!   ENSURE(.asymmetric_unit_atom.allocated, "no atom data")
!     n_atom,a1,a2,f1,l1,f2,l2 :: INT
!     first_basis_fn_for_atom,last_basis_fn_for_atom :: VEC{INT}*
!     factor :: REAL
!     .asymmetric_unit_atom.make_atom_basis_fn_limits(first_basis_fn_for_atom,last_basis_fn_for_atom)
!     n_atom = .asymmetric_unit_atom.n_atom
!     do a1 = 1,n_atom
!       f1 = first_basis_fn_for_atom(a1)
!       l1 = last_basis_fn_for_atom(a1)
!       do a2 = 1,n_atom
!         f2 = first_basis_fn_for_atom(a2)
!         l2 = last_basis_fn_for_atom(a2)
!         ! Mulliken partitioning
!         factor = (.partition_factor(a1)+.partition_factor(a2))/TWO
!         matrix(f1:l1,f2:l2) = factor * matrix(f1:l1,f2:l2)
!       end
!     end
!     last_basis_fn_for_atom.destroy
!     first_basis_fn_for_atom.destroy
!   end

!   partition_density(matrix) :: leaky
!   ! Applies atomic partition factors to the density matrix, useful for zeroing
!   ! out certain atoms.
!     matrix :: MAT{REAL}
!   ENSURE(.asymmetric_unit_atom.allocated, "no atom data")
!     n_atom,a1,a2,f1,l1,f2,l2 :: INT
!     first_basis_fn_for_atom,last_basis_fn_for_atom :: VEC{INT}*
!     factor :: REAL
!     .asymmetric_unit_atom.make_atom_basis_fn_limits(first_basis_fn_for_atom,last_basis_fn_for_atom)
!     n_atom = .asymmetric_unit_atom.n_atom
!     do a1 = 1,n_atom
!       f1 = first_basis_fn_for_atom(a1)
!       l1 = last_basis_fn_for_atom(a1)
!       do a2 = 1,n_atom
!         f2 = first_basis_fn_for_atom(a2)
!         l2 = last_basis_fn_for_atom(a2)
!         ! Mulliken partitioning
!         factor = (.partition_factor(a1)+.partition_factor(a2))/TWO
!         matrix(f1:l1,f2:l2) = factor * matrix(f1:l1,f2:l2)
!       end
!     end
!     last_basis_fn_for_atom.destroy
!     first_basis_fn_for_atom.destroy
!   end

!   make_density_mx(P,D,atom)
!   ! Make a cluster density matrix "P" from a fragment density matrix "D", given
!   ! a new cluster "atom" list (see routine create_atom_list).
!     P,D :: MAT{REAL}
!     atom :: VEC{ATOM}
!   ENSURE(.crystal.allocated, "no crystal data")
!   ENSURE(.asymmetric_unit_atom.allocated, "no atom data")
!   ENSURE(.asymmetric_unit_atom.bases_are_all_labeled,"unlabelled bases!")
!   ENSURE(D.is_square AND D.dim1==.asymmetric_unit_atom.n_bf,"D wrong shape")
!   ENSURE(P.is_square AND P.dim1==atom.n_bf,"P wrong shape")
!   ENSURE(.partition_factor.allocated,"no partition factors")
!     ptr :: MAT3{REAL}*
!     tr :: VEC{MAT3_{REAL}}*
!     tr1,tr2,pc, W :: MAT{REAL}*
!     f1,l1,s1,n1,m1,a1,c1,f2,l2,s2,n2,m2,a2,c2 :: INT
!     n_shell, q,s,f3,l3,f4,l4 :: INT
!     atom_for_shell, first,last, first_fn,last_fn :: VEC{INT}*
!     n_shell = .asymmetric_unit_atom.n_shell
!     atom_for_shell => .asymmetric_unit_atom.atom_for_shell
!     .asymmetric_unit_atom.make_atom_basis_fn_limits(first,last)
!      atom.make_atom_basis_fn_limits(first_fn,last_fn)
!     ptr => .crystal.transposed_xyz_seitz_matrices ! transposes here
!     ptr.transpose_12
!     tr.make_gaussian_xyz_matrices(ptr)
!     ptr.destroy
!     pc => .atom_pair_parent_count
!     do q = 1,.n_symop
!       s = .symop(1,q)
!       ! Transform each shell pair by symop "s"
!       do s1 = 1,n_shell
!         f1 = first(s1); l1 = last(s1); n1 = (l1-f1+1)
!         a1 = atom_for_shell(s1)
!         c1 = .atom_for_cell_atom(a1,q) ! cluster atom index
!         f3 = first_fn(c1); l3 = last_fn(c1)
!         m1 = n1.inverse_triangle_number - 1
!         tr1  => tr(m1)[:,:,s]
!         ! Transform 1st index of density matrix
!         W.create(n1,n2)
!         W = matmul(tr1,D(f1:l1,f2:l2))
!         do s2 = 1, n_shell
!           f2 = first(s2); l2 = last(s2); n2 = (l2-f2+2)
!           a2 = atom_for_shell(s2)
!           c2 = .atom_for_cell_atom(a2,q) ! cluster atom index
!           f4 = first_fn(c2); l4 = last_fn(c2)
!           m2 = n2.inverse_triangle_number - 1
!           ! Transform 2nd index of density matrix
!           tr2  => tr(m2)[:,:,s]

!                          + matmul(W,transpose(tr2))/pc(c1,c2)
!         end
!         W.destroy
!       end
!     end
!     pc.destroy
!     last_fn.destroy; first_fn.destroy
!     last.destroy; first.destroy
!     tr.destroy
!     atom_for_shell.destroy
!   end

!   atom_pair_parent_count result (n2) :: leaky
!   ! Make the atom pair parent count, i.e. the number of times n2(i,j) a
!   ! particular atom pair (i,j) is *generated from* a fragment atom pair (i',j')
!   ! by the symmerty operations whose indices are stored in .symop. We divide
!   ! by this factor to ensure that the pair effectively appears as being
!   ! generated once, as an average of all the symmetry operations.  This is
!   ! quite similar to the n2 factor in the Dacre-Elder-Dupuis-King symmetry
!   ! method. For insight see:
!   ! P.D. Dacre, CPL (1970) 7, 47
!   ! M. Elder, IJQC (1973) 7, 75
!   ! M. Dupuis and H.F> King, IJQC (1977) 11, 613
!     n2 :: MAT{REAL}*
!   ENSURE(.asymmetric_unit_atom.allocated, "no atom data")
!   ENSURE(.atom_for_cell_atom.allocated, "no atom_for_cell_atom data")
!     s1,a1,c1,s2,a2,c2 :: INT
!     n_shell,q :: INT
!     atom_for_shell :: VEC{INT}*
!     n2.create(.n_atoms,.n_atoms)
!     n_shell = .asymmetric_unit_atom.n_shell
!     atom_for_shell => .asymmetric_unit_atom.atom_for_shell
!     do q = 1,.n_symop
!       ! Transform each shell pair by symop "s"
!       do s1 = 1,n_shell
!         a1 = atom_for_shell(s1)
!         c1 = .atom_for_cell_atom(a1,q)   ! cluster atom index
!         do s2 = 1, n_shell
!           a2 = atom_for_shell(s2)
!           c2 = .atom_for_cell_atom(a2,q) ! cluster atom index
!           n2(c1,c2) = n2(c1,c2) + 1
!         end
!       end
!     end
!     atom_for_shell.destroy
!   end

!  ==========================
!  Local field factor tensors
!  ==========================

   subroutine put_averaged_LFF_tensors(self)
   ! Put the heavy-atom averaged local field factor dipole L tensors
   ! for the molecules in the unit cell. This uses the Reis formula.
      type(CLUSTER_TYPE), IN :: self
      INT :: n_mol,n_H,n,a,i1,i2,j1,j2,k1,k2,m1,m2
      MAT(REAL,:,:), allocatable :: pos,pos0,com,L,L0,L1
      VEC(INT,:), allocatable :: symop,non_H,par,mol,mol0
      VEC(STR(len=STR_SIZE),:), allocatable :: label0
   ENSURE(allocated(self%crystal),"CLUSTER:put_averaged_LFF_tensors ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_mol_for_atom),"CLUSTER:put_averaged_LFF_tensors ... no cell_mol_for_atom")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:put_averaged_LFF_tensors ... no asymetric cell atoms")

      ! Header
      call flush_(stdout)
      call text_(stdout,"Molecule-averaged Dipole L(1) tensor:")

      ! Get the positions, COM's for the molecules in the unit cell.
      ! By construction, the atoms in each molecule occur consecutively.
      call get_uc_molecule_info_(self,mol,symop,par,pos,com)

      ! Get indices of non-Hydrogen-atom positions.
      n_mol = self%crystal%n_unit_cell_mols
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
      call destroy_(com)
      call destroy_(pos)
      call destroy_(par)
      call destroy_(symop)
      call destroy_(mol)

      ! Make the non-Hydrogen-atom Lorentz factor tensors
      call create_(L,3*n_mol,3*n_mol)
      call create_(L0,3*n_H,n_H)  ! charge L tensor
      call create_(L1,3*n_H,3*n_H)  ! dipole L tensor
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,pos0,mol0)
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
      type(CLUSTER_TYPE), IN :: self
      INT :: n,i,k1,k2,m1,m2
      MAT(REAL,:,:), allocatable :: L0,L1
      MAT(REAL,:,:), allocatable :: centroid,W
      VEC(INT,:), allocatable :: mol_for
   ENSURE(allocated(self%crystal),"CLUSTER:put_centroid_LFF_tensors ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_mol_for_atom),"CLUSTER:put_centroid_LFF_tensors ... no cell_mol_for_atom")

      ! Header
      call flush_(stdout)
      call text_(stdout,"Local Field Factor L tensors:")
      call flush_(stdout)
      call show_(stdout,"n_unit_cell_mols =",self%crystal%n_unit_cell_mols)

      ! Allocate
      n = self%crystal%n_unit_cell_mols
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
      call to_product_of_(centroid,self%crystal%unit_cell%direct_mx,W)
      call destroy_(W)
      call flush_(stdout)
      call text_(stdout,"Molecule centroids (cartesian/Bohr):")
      call flush_(stdout)
      call put_(stdout,transpose(centroid))

      ! Make L tensor
      call create_(mol_for,n); mol_for = [(i,i = 1,n)]
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,centroid,mol_for)
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
      type(CLUSTER_TYPE), IN :: self
      INT :: n,k,k1,k2
      MAT3(REAL,:,:,:), allocatable :: L2
      MAT(REAL,:,:), allocatable :: centroid,W
   ENSURE(allocated(self%crystal),"CLUSTER:put_centroid_LFF_tensors_old ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_mol_for_atom),"CLUSTER:put_centroid_LFF_tensors_old ... no cell_mol_for_atom")

      ! Header
      call flush_(stdout)
      call text_(stdout,"Local Field Factor L tensors:")
      call flush_(stdout)
      call show_(stdout,"n_unit_cell_mols =",self%crystal%n_unit_cell_mols)

      ! Allocate space
      n = self%crystal%n_unit_cell_mols
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
      call to_product_of_(centroid,self%crystal%unit_cell%direct_mx,W)
      call destroy_(W)
      call flush_(stdout)
      call text_(stdout,"Molecule centroids (cartesian/Bohr):")
      call flush_(stdout)
      call put_(stdout,transpose(centroid))

      ! Make L tensor
      call make_LFF_tensors_(self%crystal%unit_cell,L2,centroid)

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
      type(CLUSTER_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: centroid
      INT :: m,u
      MAT(REAL,:,:), allocatable :: geometry
   ENSURE(allocated(self%crystal),"CLUSTER:get_uc_molecule_centroids ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_mol_for_atom),"CLUSTER:get_uc_molecule_centroids ... no cell_mol_for_atom")
   ENSURE(all(shape(centroid)==[3,self%crystal%n_unit_cell_mols]),"CLUSTER:get_uc_molecule_centroids ... wrong shape, centroid")
      centroid = ZERO
      do m = 1,self%crystal%n_unit_cell_mols
         do u = 1,self%crystal%n_unit_cell_atoms
            if (self%crystal%unit_cell_mol_for_atom(u)/=m) cycle
            call do_defragment_uc_atom_(self,u,geometry)
            exit
         end do
         centroid(:,m) = sum_column_vectors_(geometry)/size(geometry,2)
         call put_to_unit_cell_(self%crystal,centroid(:,m))

         ! Clean
         call destroy_(geometry)
      end do
   end subroutine

   subroutine put_LFF_info(self)
   ! Put the local field factor L tensors at the centroids of all
   ! molecules in the unit cell.
      type(CLUSTER_TYPE), IN :: self
      INT :: n,k1,k2,m1,m2
      MAT(REAL,:,:), allocatable :: L0,L1
      VEC(INT,:), allocatable :: mol,par
      MAT(REAL,:,:), allocatable :: pos,coc,com,alpha_q
      MAT3(REAL,:,:,:), allocatable :: symop,alpha_m
   ENSURE(allocated(self%crystal),"CLUSTER:put_LFF_info ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_mol_for_atom),"CLUSTER:put_LFF_info ... no cell_mol_for_atom")

      ! Get the unit cell molecule atom info
      call get_uc_molecule_info_(self,mol,symop,par,pos,coc,com,alpha_q,alpha_m)

      ! Make the charge and dipole Lorentz factor tensors
      n = size(par)
      call create_(L0,3*n,n)
      call create_(L1,3*n,3*n)
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,pos,mol)

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
      type(CLUSTER_TYPE), IN :: self
      MAT(REAL,3,3), OUT :: chi1  ! Clausius-Mossotti, L=1/3
      MAT(REAL,3,3), IN :: alpha_mol
      MAT(REAL,3,3) :: alpha_uc
      MAT(REAL,:,:), allocatable :: D
      REAL :: fac
      call make_alpha_uc_(self,alpha_uc,alpha_mol)

      ! fac = factor to reduce polarisabilities = 1/(eps_0 V) (in au)
      fac = FOUR*PI/self%crystal%unit_cell%volume
      call create_(D,3,3)
      call to_unit_mx_(D)
      D = D-fac*THIRD*alpha_uc
      call to_inverse_of_(D,D)  ! NOT PURE
      call to_scaled_product_of_(chi1,alpha_uc,D,fac)
      call destroy_(D)
   end subroutine

   PURE subroutine make_alpha_uc(self,alpha_uc,alpha_mol)
   ! Make the unit cell alpha.
      type(CLUSTER_TYPE), IN :: self
      MAT(REAL,3,3), OUT :: alpha_uc
      MAT(REAL,3,3), IN :: alpha_mol
      MAT(REAL,3,3) :: alpha
      VEC(INT,:), allocatable :: mol,symop,par
      MAT(REAL,:,:), allocatable :: pos,com
      MAT3(REAL,:,:,:), allocatable :: inverse_seitz
      INT :: n_mol,m
   ENSURE(allocated(self%crystal),"CLUSTER:make_alpha_uc ... no crystal")

      ! Get the unit cell molecule atom info
      call get_uc_molecule_info_(self,mol,symop,par,pos,com)
      call destroy_(com)
      call destroy_(pos)
      call destroy_(par)
      call destroy_(mol)

      ! Transposed/Inverse symops
      inverse_seitz = transposed_xyz_seitz_matrices_(self%crystal)

      ! No of molecules
      n_mol = size(symop)

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
      type(CLUSTER_TYPE), IN :: self
      MAT(REAL,3,3), OUT :: chi1
      MAT(REAL,3,3), IN :: alpha_mol
      VEC(INT,:), allocatable :: mol,mol0,symop,par
      MAT3(REAL,:,:,:), allocatable :: inverse_seitz,alpha
      MAT(REAL,:,:), allocatable :: pos,com,L0,L1,D,Df
      MAT(REAL,3,3) :: alph
      INT :: n_mol,dim,a,b,i,j,m
      REAL :: fac

      ! No. of molecules
      n_mol = self%crystal%n_unit_cell_mols

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
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,com,mol0)

      ! Get the reduced molecular polarisabilities
      ! fac = factor to reduce polarisabilities = 1/(eps_0 V) (in au)
      inverse_seitz = transposed_xyz_seitz_matrices_(self%crystal)
      fac = FOUR*PI/self%crystal%unit_cell%volume
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
   ! are evaluated at the non H atom sites. Trace is over 3x3 blocks
      type(CLUSTER_TYPE), IN :: self
      MAT(REAL,3,3), OUT :: chi1
      MAT(REAL,3,3), IN :: alpha_mol
      VEC(INT,:), allocatable :: mol,mol0,symop,par,non_H
      MAT3(REAL,:,:,:), allocatable :: inverse_seitz,alpha
      MAT(REAL,:,:), allocatable :: pos,pos0,com,L0,L1,D,Df
      MAT(REAL,3,3) :: alph
      INT :: n_mol,n_non_H,n_non_H_per_mol
      INT :: n,dim,a,b,i,j,m
      REAL :: fac
      n = self%crystal%n_unit_cell_atoms  ! No. of atoms in unit cell
      n_mol = self%crystal%n_unit_cell_mols  ! No. of molecules

      ! No of non-hydrogen atoms in unit cell
      par = self%crystal%asym_atom_for_unit_cell_atom
      n_non_H = count(self%asymmetric_unit_atom(par)%atomic_number>1)

      ! No. of non-H hydrogen atoms per molecule
      ENSURE(mod(n_non_H,n_mol)==0,"CLUSTER:make_chi1_RLFTn ... no of non-Hydrogen atoms wrong")
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
      inverse_seitz = transposed_xyz_seitz_matrices_(self%crystal)
      fac = FOUR*PI/self%crystal%unit_cell%volume
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
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,pos0,mol0)

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
      call destroy_(L1); call destroy_(L0)
      call destroy_(mol0); call destroy_(pos0); call destroy_(non_H)
      call destroy_(com); call destroy_(pos); call destroy_(par); call destroy_(symop); call destroy_(mol)

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
   ! are evaluated at the non H atom sites. Trace is over 3x3 blocks
      type(CLUSTER_TYPE), IN :: self
      MAT(REAL,3,3), OUT :: chi1
      MAT(REAL,3,3), IN :: alpha_mol
      VEC(INT,:), allocatable :: mol,mol0,symop,par,non_H
      MAT3(REAL,:,:,:), allocatable :: inverse_seitz,alpha
      MAT(REAL,:,:), allocatable :: pos,pos0,com,L,L0,L1,D,Df
      MAT(REAL,3,3) :: alph
      INT :: n,n_mol,n_non_H,n_non_H_per_mol,dim
      INT :: i1,i2,j1,j2,k1,k2,m1,m2,a,b,i,j,m
      REAL :: fac
      n = self%crystal%n_unit_cell_atoms  ! No. of atoms in unit cell
      n_mol = self%crystal%n_unit_cell_mols  ! No. of molecules

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
      ENSURE(mod(n_non_H,n_mol)==0,"CLUSTER:make_chi1_RLFTn_av_L ... no of non-Hydrogen atoms wrong")
      n_non_H_per_mol = n_non_H/n_mol

      ! Get the non-H positions and molecules
      call create_(pos0,3,n_non_H)
      call create_(mol0,n_non_H)
      pos0 = pos(:,non_H)
      mol0 = mol(non_H)

      ! Get the reduced molecular polarisabilities
      ! fac = factor to reduce polarisabilities = 1/(eps_0 V) (in au)
      inverse_seitz = transposed_xyz_seitz_matrices_(self%crystal)
      fac = FOUR*PI/self%crystal%unit_cell%volume
      alph = fac*alpha_mol
      do m = 1,n_mol
         call change_basis_to_(alph,alpha(:,:,m),inverse_seitz(:,:,symop(m)))
      end do
      call destroy_(inverse_seitz)

      ! Make the non H charge and dipole Lorentz factor tensors
      dim = 3*n_non_H
      call create_(L0,dim,n_non_H)  ! charge L tensor
      call create_(L1,dim,dim)  ! dipole L tensor
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,pos0,mol0)

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

      ! Mini clean up
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

      ! Make the chi1 (ALFFA-like) susceptibility
      chi1 = ZERO
      i = 0
      do a = 1,n_mol
         call plus_product_of_(chi1,alpha(:,:,a),Df(i+1:i+3,:))
         i = i+3
      end do

      ! Clean up
      call destroy_(Df)
      call destroy_(D)
      call destroy_(L)
      call destroy_(alpha)
   end subroutine

   subroutine make_chi2_RLFTn_av_L(self,chi2,beta_mol,alpha_mol)
   ! Make the RLFTn "chi2" matrix:
   !   chi2_ijk = \sum_S \beta^S_abc d^S_ai d^S_bj d^S_ck
   ! where d^S are the local field factor tensors and \beta^S are the
   ! (symmetry transformed) molecular dipole site-hyperpolarisability
   ! "beta_mol" divided by the number of non-Hydrogen atoms in a
   ! molecule.  The L tensors are evaluated at non H atom sites.
      type(CLUSTER_TYPE), IN :: self
      MAT3(REAL,3,3,3), OUT :: chi2
      MAT3(REAL,3,3,3), IN :: beta_mol
      MAT(REAL,3,3), IN :: alpha_mol
      MAT(REAL,3,3) :: alph0
      MAT3(REAL,3,3,3) :: beta0
      MAT3(REAL,:,:,:), allocatable :: inverse_seitz,alpha
      MAT4(REAL,:,:,:,:), allocatable :: beta
      MAT(REAL,:,:), allocatable :: pos,pos0,com,L,L0,L1,D,Df
      VEC(INT,:), allocatable :: mol,mol0,symop,par,non_H
      INT :: n,n_mol,n_non_H,n_non_H_per_mol,dim
      INT :: i1,i2,j1,j2,k1,k2,m1,m2,a,b,i,j,m
      REAL :: fac
      n = self%crystal%n_unit_cell_atoms  ! No. of atoms in unit cell
      n_mol = self%crystal%n_unit_cell_mols  ! No. of molecules

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
      ENSURE(mod(n_non_H,n_mol)==0,"CLUSTER:make_chi2_RLFTn_av_L ... no of non-Hydrogen atoms wrong")
      n_non_H_per_mol = n_non_H/n_mol

      ! Get the non-H positions and molecules
      call create_(pos0,3,n_non_H)
      call create_(mol0,n_non_H)
      pos0 = pos(:,non_H)
      mol0 = mol(non_H)

      ! Get the reduced molecular polarisabilities
      ! fac = factor to reduce polarisabilities = 1/(2 eps_0 V)
      !     = (2 pi/V) ... in au since eps_0 = (4 pi)^{-1}.
      inverse_seitz = transposed_xyz_seitz_matrices_(self%crystal)
      fac = TWO*PI/self%crystal%unit_cell%volume
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
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,pos0,mol0)

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
      type(CLUSTER_TYPE), IN :: self
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
      MAT(REAL,:,:), PTR :: Lm
      VEC(REAL,:), PTR :: Lq
      MAT(REAL,:,:), allocatable :: df
      MAT(REAL,:,:), allocatable, target :: D,Dnc,Ddo
      MAT(REAL,:,:), PTR :: Dd
      MAT(REAL,:,:), allocatable :: pos,pos0,coc,com,coc0,alpha_q
      VEC(INT,:), allocatable :: mol,mol0,par,non_H
      MAT3(REAL,:,:,:), allocatable :: symop,alpha_m,alpha
      INT :: n,n_m,n_a,n_H,nH,a,b,i,j,m
      REAL :: fac
   ENSURE(allocated(self%crystal),"CLUSTER:make_susceptibility2 ... no crystal")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:make_susceptibility2 ... no asymetric cell atoms")

      ! Get the unit cell electrical properties for each atom
      call get_uc_molecule_info_(self,mol,symop,par,pos,coc,com,alpha_q,alpha_m)

      ! Get no of atoms in unit cell
      n = size(par)

      ! Factor to reduce polarisabilities (in au)
      fac = FOUR*PI/self%crystal%unit_cell%volume

      ! Make the charge and dipole Lorentz factor tensors
      call create_(D,3*n,3*n)
      call create_(Dnc,3*n,3*n)
      call create_(Ddo,3*n,3*n)
      call create_(alpha,3,3,n)
      call create_(L0,3*n,n)
      call create_(L1,3*n,3*n)
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,pos,mol)

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
      call to_inverse_of_(D,D)  ! NOT PURE
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
      call destroy_(Ddo)
      call destroy_(Dnc)
      call destroy_(D)

      ! ================================================

      ! Make chi1_naive
      call create_(D,3,3)
      call to_unit_mx_(D)
      D = D-fac*THIRD*uc_alpha
      call to_inverse_of_(D,D)  ! NOT PURE
      chi1_naive = fac*matmul(uc_alpha,D)
      call destroy_(D)

      ! ================================================

      ! Get the no of molecules
      n_m = maxval(mol)
      ENSURE(mod(n,n_m)==0,"CLUSTER:make_susceptibility2 ... no of atoms in a molecule is not integral")

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
      call flush_(stdout)
      call show_(stdout,"Center of mass =",coc0(:,1))

      ! Make the centroid charge and dipole Lorentz factor tensors
      call create_(D,3*n_m,3*n_m)
      call create_(L0,3*n_m,n_m)
      call create_(L1,3*n_m,3*n_m)
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,coc0,mol0)

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
      call to_inverse_of_(D,D)  ! NOT PURE
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
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,pos0,mol0)

      ! Get number of Non-H hydrogen atoms per molecule
      ENSURE(mod(n_H,n_m)==0,"CLUSTER:make_susceptibility2 ... no of non-Hydrogen atoms wrong")
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
      call destroy_(df)
      call destroy_(D)
      call destroy_(mol0)
      call destroy_(pos0)
      call destroy_(non_H)
      call destroy_(alpha)
      call destroy_(alpha_m)
      call destroy_(alpha_q)
      call destroy_(com)
      call destroy_(coc)
      call destroy_(pos)
      call destroy_(par)
      call destroy_(symop)
      call destroy_(mol)
   end subroutine

   PURE subroutine make_local_field_factors(Df,D)
   ! Make the second ortder susceptibility from distributed
   ! polarisabilities.
      MAT(REAL,:,:), OUT :: Df
      MAT(REAL,:,:), IN :: D
      INT :: n,a,b,i,j
   ENSURE(is_square_(D),"CLUSTER:make_local_field_factors ... non square D")
   ENSURE(mod(size(D,1),3)==0,"CLUSTER:make_local_field_factors ... D dim1 is not divisible by 3")
   ENSURE(size(Df,1)==size(D,1),"CLUSTER:make_local_field_factors ... wrong dim1, Df")
   ENSURE(size(Df,2)==3,"CLUSTER:make_local_field_factors ... wrong dim2, Df")
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
      type(CLUSTER_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: D
      VEC(INT,:), allocatable :: mol,par
      MAT3(REAL,:,:,:), allocatable :: symop,alpha_m
      MAT(REAL,:,:), allocatable :: pos,coc,com,alpha_q
      MAT(REAL,:,:), allocatable, target :: L0,L1
      MAT(REAL,:,:), PTR :: Lm
      VEC(REAL,:), PTR :: Lq
      INT :: n,dim,a,b,i,j
      REAL :: fac
   ENSURE(is_square_(D),"CLUSTER:make_D_mx ... non square D")
   ENSURE(allocated(self%crystal),"CLUSTER:make_D_mx ... no crystal")

      ! Factor to reduce polarisabilities = 1/(eps_0 V) (in au)
      fac = FOUR*PI/self%crystal%unit_cell%volume

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
      ENSURE(dim==3*n,"CLUSTER:make_D_mx ... wrong size, par array")

      ! Make the charge and dipole Lorentz factor tensors
      call create_(L0,dim,n)
      call create_(L1,dim,dim)
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,pos,mol)

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
      type(CLUSTER_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Dnc
      VEC(INT,:), allocatable :: mol,par
      MAT3(REAL,:,:,:), allocatable :: symop,alpha_m
      MAT(REAL,:,:), allocatable :: pos,coc,com,alpha_q
      MAT(REAL,:,:), allocatable, target :: L0,L1
      MAT(REAL,:,:), PTR :: Lm
      INT :: n,dim,a,b,i,j
      REAL :: fac
   ENSURE(is_square_(Dnc),"CLUSTER:make_Dnc_mx ... non square Dnc")
   ENSURE(allocated(self%crystal),"CLUSTER:make_Dnc_mx ... no crystal")

      ! Factor to reduce polarisabilities = 1/(eps_0 V) (in au)
      fac = FOUR*PI/self%crystal%unit_cell%volume

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
      ENSURE(dim==3*n,"CLUSTER:make_Dnc_mx ... wrong size, par array")

      ! Make the charge and dipole Lorentz factor tensors
      call create_(L0,dim,n)
      call create_(L1,dim,dim)
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,pos,mol)

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
   ! fragment where atomic charges and dipoles have been done.
      type(CLUSTER_TYPE), IN :: self
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
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,pos,mol,sphere_sum)
      fac = FOUR*PI/self%crystal%unit_cell%volume
      L0 = fac*L0
      L1 = fac*L1

      ! Assign electric fields
      do a = 1,size(self%fragment_geometry,2)
         u = index_of_value_(frag_atom,a)  ! map to unit cell atom
         ENSURE(u/=0,"CLUSTER:make_Lorentz_fields ... fragment atom "//trim(to_str_(a))//" not found in cell")
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
      type(CLUSTER_TYPE), IN :: self
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
      call make_LFF_tensors_(self%crystal%unit_cell,L0,L1,pos,mol,sphere_sum)
      fac = FOUR*PI/self%crystal%unit_cell%volume
      L0 = fac*L0
      L1 = fac*L1

      ! Assign charge and dipole interactions
      ! Needs a minus sign when combined
      do a = 1,size(self%fragment_geometry,2)
         u = index_of_value_(frag_atom,a)  ! map to unit cell atom
         ENSURE(u/=0,"CLUSTER:make_Lorentz_interactions ... fragment atom "//trim(to_str_(a))//" not found in cell")
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
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,:), allocatable, OUT :: mol
      VEC(INT,:), allocatable, OUT :: symop
      VEC(INT,:), allocatable, OUT :: par
      MAT(REAL,:,:), allocatable, OUT :: pos
      MAT(REAL,:,:), allocatable, OUT :: com
      INT :: n_atom,n_mol,n,m,u,a
      VEC(REAL,:), allocatable :: mass
      VEC(REAL,3) :: cm
      REAL :: fac
   ENSURE(allocated(self%crystal),"CLUSTER:get_uc_molecule_info_0 ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_mol_for_atom),"CLUSTER:get_uc_molecule_info_0 ... no cell_mol_for_atom")

      ! Create the info arrays
      n_mol = self%crystal%n_unit_cell_mols
      n_atom = self%crystal%n_unit_cell_atoms
      call create_(mol,n_atom)
      call create_(symop,n_mol)
      call create_(par,n_atom)
      call create_(pos,3,n_atom)
      call create_(com,3,n_mol)
      call create_(mass,n_atom)

      ! Set n_atom to the # of atoms in one molecule
      ENSURE(mod(n_atom,n_mol)==0,"CLUSTER:get_uc_molecule_info_0 ... # of unit cell atoms is not divisible by number of molecules")
      n_atom = n_atom/n_mol
      n = 0
      do m = 1,n_mol

         ! Find atom "u" on molecule "m", defragment, get the info
         do u = 1,self%crystal%n_unit_cell_atoms
            if (self%crystal%unit_cell_mol_for_atom(u)/=m) cycle
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
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,:), allocatable, OUT :: mol
      VEC(INT,:), allocatable, OUT :: par
      MAT3(REAL,:,:,:), allocatable, OUT :: symop
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
   ENSURE(allocated(self%crystal),"CLUSTER:get_uc_molecule_info_1 ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_mol_for_atom),"CLUSTER:get_uc_molecule_info_1 ... no cell_mol_for_atom")

      ! Sizes
      n_mol = self%crystal%n_unit_cell_mols
      n_atom = self%crystal%n_unit_cell_atoms

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
      ENSURE(mod(n_atom,n_mol)==0,"CLUSTER:get_uc_molecule_info_1 ... # of unit cell atoms is not divisible by number of molecules")
      n_atom = n_atom/n_mol
      n = 0
      do m = 1,n_mol

         ! Find atom "u" on molecule "m", defragment, get the info
         do u = 1,self%crystal%n_unit_cell_atoms
            if (self%crystal%unit_cell_mol_for_atom(u)/=m) cycle
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
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,:), allocatable, OUT :: mol
      MAT(REAL,:,:), allocatable, OUT :: pos
      VEC(REAL,:), allocatable, OUT :: charge
      VEC(REAL,:), allocatable, OUT :: dipole
      VEC(INT,:), allocatable, OUT :: frag_atom
      INT :: n_atom,n_mol,n,n3,m,u
      VEC(INT,:), allocatable :: occupation_list
   ENSURE(allocated(self%crystal),"CLUSTER:get_uc_molecule_info_2 ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_mol_for_atom),"CLUSTER:get_uc_molecule_info_2 ... no cell_mol_for_atom")

      ! Sizes
      n_mol = self%crystal%n_unit_cell_mols
      n_atom = self%crystal%n_unit_cell_atoms

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
         do u = 1,self%crystal%n_unit_cell_atoms
            if (self%crystal%unit_cell_mol_for_atom(u)/=m) cycle

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


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump object data as text
      type(CLUSTER_TYPE), IN :: self
      call dump_(stdout,"radius",self%radius)
      call dump_(stdout,"atom_density_cutoff",self%atom_density_cutoff)
      call dump_(stdout,"generation_method",self%generation_method)
      call dump_(stdout,"defragment",self%defragment)
      call dump_(stdout,"n_atoms",self%n_atoms)
      call dmpa_(stdout,"geometry",self%geometry)
      call dmpa_(self%crystal,"crystal")
      call dmpa_(self%asymmetric_unit_atom,"asymmetric_unit_atom")
      call dmpa_(stdout,"fragment_geometry",self%fragment_geometry)
      call dump_(stdout,"fragment_offset",self%fragment_offset)
      call dump_(stdout,"unit_cell_offset",self%unit_cell_offset)
      call dump_(stdout,"h_min",self%h_min)
      call dump_(stdout,"h_max",self%h_max)
      call dump_(stdout,"n_shift",self%n_shift)
      call dmpa_(stdout,"shift",self%shift)
      call dmpa_(stdout,"shift_for_atom",self%shift_for_atom)
      call dmpa_(stdout,"is_fragment_atom",self%is_fragment_atom)
      call dmpa_(stdout,"occupation_list",self%occupation_list)
   ! stdout.dmpa("molecule_for_atom",.molecule_for_atom)
   ! stdout.dmpa("molecule",.molecule)
      call dmpa_(stdout,"molecule_centroid",self%molecule_centroid)
      call dmpa_(stdout,"unique_molecule_for",self%unique_molecule_for)
      call dmpa_(stdout,"unique_sycode_for",self%unique_symcode_for)
   ! stdout.dmpa("atom_connection",.atom_connection)
      call dump_(stdout,"n_molecules",self%n_molecules)
      call dump_(stdout,"info_made",self%info_made)
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(CLUSTER_TYPE), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: all
      BIN :: is_all

      ! The following code is inherited from OBJECT
      is_all = FALSE
      if (present(all)) is_all = all
      if (is_all) then; call text_(stdout,trim(object_name)//":CLUSTER@ = { ")
      else; call text_(stdout,trim(object_name)//":CLUSTER = { ")
      end if
      call increment_margin_width_(stdout,1)
      call do_dump_(self)
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump allocatable object data as text
      type(CLUSTER_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":CLUSTER@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


!  ==============
!  Output methods
!  ==============

   subroutine put(self)
   ! Put the list of vertices for the object
      type(CLUSTER_TYPE), IN :: self
   ENSURE(self%info_made,"CLUSTER:put ... call update_info first")
   ENSURE(allocated(tonto),"CLUSTER:put ... no tonto variable")
      call flush_(stdout)
      call text_(stdout,"==============================")
      call text_(stdout,"Cluster generation information")
      call text_(stdout,"==============================")
      call flush_(stdout)
      call show_(stdout,"Cluster generation method       =",self%generation_method)
      call show_(stdout,"Complete connected fragments    =",self%defragment)
      call show_(stdout,"Radius of cluster               =",self%radius)
      call show_(stdout,"No. of molecules                =",self%n_molecules)
      call show_(stdout,"No. of atoms                    =",self%n_atoms)
      call show_(stdout,"No. of fragment atoms           =",size(self%fragment_geometry,2))
      call show_(stdout,"Fragment offset                 =",self%fragment_offset)
      call show_(stdout,"Minimum cell shift for search   =",self%h_min)
      call show_(stdout,"Maximum cell shift for search   =",self%h_max)
      if (tonto%low_verbosity) then
         if (allocated(self%fragment_geometry)) then
            if (size(self%fragment_geometry)>100) return
         end if
      end if
      if (allocated(self%fragment_geometry)) call put_fragment_geometry_(self)
      if (allocated(self%occupation_list)) call put_occupation_list_(self)
      if (allocated(self%atom_connection)) call put_atom_connection_table_(self)
      if (allocated(self%occupation_list)) call put_atom_symmetry_table_(self)
   end subroutine

   subroutine put_fragment_geometry(self)
   ! Put the initial fragment geometry information
      type(CLUSTER_TYPE), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: ID
      VEC(INT,:), allocatable :: ind
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
   ENSURE(allocated(self%crystal),"CLUSTER:put_fragment_geometry ... no crystal")
   ENSURE(allocated(self%fragment_geometry),"CLUSTER:put_fragment_geometry ... no fragment_geometry")
      call flush_(stdout)
      call text_(stdout,"=======================================")
      call text_(stdout,"Central fragment fractional coordinates")
      call text_(stdout,"=======================================")
      call flush_(stdout)
      call show_(stdout,"No. of fragment atoms           =",size(self%fragment_geometry,2))
      call flush_(stdout)

      ! Unique ID tags
      ind = crystal_frag_atom_indices_(self)
      ID = self%crystal%fragment_atom(ind)%tag

      ! Table
      call create_(table,4)
      call set_heading_(table(1),"ID"); call set_values_(table(1),ID)
      call set_heading_(table(3),"Coord")
      call set_subhead_(table(2),"- x -"); call set_values_(table(2),self%fragment_geometry(1,:))
      call set_subhead_(table(3),"- y -"); call set_values_(table(3),self%fragment_geometry(2,:))
      call set_subhead_(table(4),"- z -"); call set_values_(table(4),self%fragment_geometry(3,:))
      call set_sb3head_(table(3),"/frac")
      call put_(table)

      ! Clean
      call clear_columns_(table)
      call destroy_(table)
      call destroy_(ID)
      call destroy_(ind)
   end subroutine

   subroutine put_occupation_list(self)
   ! Put the .occupation_list table
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,:), allocatable :: ui,hc
      VEC(INT,:), allocatable :: h1,h2,h3
      VEC(REAL,:), allocatable :: r1,r2,r3
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
   ENSURE(allocated(self%occupation_list),"CLUSTER:put_occupation_list ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:put_occupation_list ... no crystal")
   ENSURE(allocated(self%crystal%unit_cell_geometry),"CLUSTER:put_occupation_list ... no crystal unit cell geometry")
      call flush_(stdout)
      call text_(stdout,"=============")
      call text_(stdout,"Cluster atoms")
      call text_(stdout,"=============")
      call flush_(stdout)
      call show_(stdout,"Cluster generation method       =",self%generation_method)
      call show_(stdout,"Complete connected fragments    =",self%defragment)
      call show_(stdout,"Radius of cluster               =",self%radius)
      call show_(stdout,"No. of molecules                =",self%n_molecules)
      call show_(stdout,"No. of atoms                    =",self%n_atoms)
      call show_(stdout,"No. of fragment atoms           =",size(self%fragment_geometry,2))
      call show_(stdout,"Fragment offset                 =",self%fragment_offset)
      call show_(stdout,"Minimum cell shift for search   =",self%h_min)
      call show_(stdout,"Maximum cell shift for search   =",self%h_max)

      ! Data
      ui = CLUSTER_UA_FROM_CODE(self%occupation_list)
      h1 = CLUSTER_H1_FROM_CODE(self%occupation_list)
      h2 = CLUSTER_H2_FROM_CODE(self%occupation_list)
      h3 = CLUSTER_H3_FROM_CODE(self%occupation_list)
      r1 = self%crystal%unit_cell_geometry(1,ui)+h1
      r2 = self%crystal%unit_cell_geometry(2,ui)+h2
      r3 = self%crystal%unit_cell_geometry(3,ui)+h3
      hc = 1000*self%crystal%asym_symop_for_unit_cell_atom(ui)
      hc = 100*(h1+5+self%crystal%asym_shift_for_unit_cell_atom(1,ui))+hc
      hc = 10*(h2+5+self%crystal%asym_shift_for_unit_cell_atom(2,ui))+hc
      hc = (h3+5+self%crystal%asym_shift_for_unit_cell_atom(3,ui))+hc

      ! Table
      call create_(table,9)
      call set_heading_(table(1),"Cell"); call set_values_(table(1),ui)
      call set_subhead_(table(1),"atom")
      call set_heading_(table(2),"Group"); call set_values_(table(2),self%molecule_for_atom)
      call set_subhead_(table(2),"(Mol)")
      call set_heading_(table(4),"Coord")
      call set_subhead_(table(3),"- x -"); call set_values_(table(3),r1)
      call set_subhead_(table(4),"- y -"); call set_values_(table(4),r2)
      call set_subhead_(table(5),"- z -"); call set_values_(table(5),r3)
      call set_sb3head_(table(4),"/frac")
      call set_heading_(table(7),"UC")
      call set_subhead_(table(6),"h1"); call set_values_(table(6),h1)
      call set_subhead_(table(7),"h2"); call set_values_(table(7),h2)
      call set_subhead_(table(8),"h3"); call set_values_(table(8),h3)
      call set_subhead_(table(9),"code"); call set_values_(table(9),hc)
      call put_(table)

      ! Clean
      call clear_columns_(table)
      call destroy_(table)
      call destroy_(hc)
      call destroy_(r3); call destroy_(r2); call destroy_(r1)
      call destroy_(h3); call destroy_(h2); call destroy_(h1)
   end subroutine

   subroutine put_atom_connection_table(self)
   ! Put the .atom_connection table
      type(CLUSTER_TYPE), IN :: self
      INT :: m,a,i
      BIN :: first
   ENSURE(allocated(self%atom_connection),"CLUSTER:put_atom_connection_table ... no atom_connection")
   ENSURE(allocated(self%molecule_for_atom),"CLUSTER:put_atom_connection_table ... no molecule_for_atom")
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
            if (NOT allocated(self%atom_connection(a)%element)) then
               call flush_(stdout)
            else
               do i = 1,size(self%atom_connection(a)%element)
                  call put_(stdout,self%atom_connection(a)%element(i))
               end do
               call flush_(stdout)
            end if
         end do
      end do
      call dash_(stdout,int_fields=8)
      call unsave_style_(stdout)
   end subroutine

   subroutine put_atom_symmetry_table(self)
   ! Put the atom symmetry table i.e. those atoms in the cluste
   ! related by symmetry.
      type(CLUSTER_TYPE), IN :: self
      VEC(INT,:), allocatable :: asym_atom_for_atom
      INT :: a,u
   ENSURE(allocated(self%occupation_list),"CLUSTER:put_atom_symmetry_table ... no occupation_list")
   ENSURE(allocated(self%crystal),"CLUSTER:put_atom_symmetry_table ... no crystal")
   ENSURE(self%crystal%fragment_info_made,"CLUSTER:put_atom_symmetry_table ... no crystal fragment info")
      call create_(asym_atom_for_atom,self%n_atoms)
      do a = 1,self%n_atoms
         u = CLUSTER_UA_FROM_CODE(self%occupation_list(a))
         asym_atom_for_atom(a) = self%crystal%asym_atom_for_unit_cell_atom(u)
      end do
      call flush_(stdout)
      call text_(stdout,"Asymmetric (unique) atoms for each atom in the cluster, and allocated symop:")
      call flush_(stdout)
      call dash_(stdout,int_fields=3)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Unique",int_width=TRUE)
      call put_(stdout,"Symop",int_width=TRUE)
      call flush_(stdout)
      call dash_(stdout,int_fields=3)
      do a = 1,self%n_atoms
         u = CLUSTER_UA_FROM_CODE(self%occupation_list(a))
         call put_(stdout,a)
         call put_(stdout,asym_atom_for_atom(a))
         call put_(stdout,self%crystal%asym_symop_for_unit_cell_atom(u))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=3)

      ! Clean
      call destroy_(asym_atom_for_atom)
   end subroutine

   subroutine put_tonto_input(self)
   ! Outputs the tonto input file for the cluster, given additionally the list
   ! of atoms which was used to generate the fragment_geometry in crystal.
   ! (See routine make_fragment_data).
      type(CLUSTER_TYPE), IN :: self
      MAT3(REAL,:,:,:), allocatable :: seitz
      MAT(REAL,3,3) :: therm
      INT :: a,u,p,s
   ENSURE(self%info_made,"CLUSTER:put_tonto_input ... call update_info first")
   ENSURE(allocated(self%occupation_list),"CLUSTER:put_tonto_input ... no occupation_list")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:put_tonto_input ... no atom data")
   ENSURE(allocated(self%crystal),"CLUSTER:put_tonto_input ... no crystal data")
      call text_(stdout,"   atoms= {")
      call flush_(stdout)
      call text_(stdout,"      keys= { label= ")
      call text_(stdout,"              { axis_system= crystal } pos=")
      call text_(stdout,"              { units= angstrom^2 } ADP_tensor= }")
      call flush_(stdout)
      call text_(stdout,"      data= {")
      seitz = transposed_xyz_seitz_matrices_(self%crystal)  ! transposed !
      do a = 1,self%n_atoms
         u = CLUSTER_UA_FROM_CODE(self%occupation_list(a))
         p = self%crystal%asym_atom_for_unit_cell_atom(u)
         s = self%crystal%asym_symop_for_unit_cell_atom(u)
         call put_(stdout,trim(self%asymmetric_unit_atom(p)%label),int_width=TRUE)
         call put_(stdout,self%geometry(1,a))
         call put_(stdout,self%geometry(2,a))
         call put_(stdout,self%geometry(3,a))
!        stdout.put(.asymmetric_unit_atom(p).basis.label.trim)
         therm = self%asymmetric_unit_atom(p)%ADP_tensor  ! in cartesians?
         call change_basis_using_(therm,seitz(:,:,s))
         call convert_to_(therm,"angstrom^2")
         call put_(stdout,therm(1,1))
         call put_(stdout,therm(2,2))
         call put_(stdout,therm(3,3))
         call put_(stdout,therm(1,2))
         call put_(stdout,therm(1,3))
         call put_(stdout,therm(2,3))
         call flush_(stdout)
      end do
      call destroy_(seitz)
      call text_(stdout,"      }")
      call text_(stdout,"   }")
      call flush_(stdout)
   end subroutine

   subroutine put_CX(self,angstrom)
   ! Outputs some information for the Crystal Explorer program: the list of atoms in
   ! the cluster, their positions, and whether they are part of the generating
   ! fragment or not.
      type(CLUSTER_TYPE), IN :: self
      BIN, optional, IN :: angstrom
      INT :: u,a,s,n,i,j
      MAT(REAL,:,:), allocatable :: geometry
      VEC(INT,:), allocatable :: ulist
      MAT(REAL,3,3) :: therm
      MAT3(REAL,:,:,:), allocatable :: seitz
      BIN :: angst
   ENSURE(allocated(self%geometry),"CLUSTER:put_CX ... no cluster geometry")
   ENSURE(allocated(self%occupation_list),"CLUSTER:put_CX ... no occupation_list")
   ENSURE(allocated(self%is_fragment_atom),"CLUSTER:put_CX ... no is_fragment_atom")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:put_CX ... no atom data")
   ENSURE(allocated(self%crystal),"CLUSTER:put_CX ... no crystal data")
   ENSURE(self%crystal%fragment_info_made,"CLUSTER:put_CX ... no crystal fragment_info")
      angst = FALSE
      if (present(angstrom)) angst = angstrom

      ! Transposed seutz matrices
      seitz = transposed_xyz_seitz_matrices_(self%crystal)

      ! Get uniot cell atom indices
      call create_(ulist,self%n_atoms)
      ulist = CLUSTER_UA_FROM_CODE(self%occupation_list)

      ! Get unit cell geometry - keep in fractional coordinates
      geometry = self%crystal%unit_cell_geometry

      ! Print symops
      call flush_(stdout)
      call show_(stdout,"begin seitz_matrices ",self%crystal%spacegroup%n_seitz,dots=FALSE)
      do n = 1,self%crystal%spacegroup%n_seitz
         do i = 1,size(self%crystal%spacegroup%seitz,1)
            do j = 1,size(self%crystal%spacegroup%seitz,2)
               call put_(stdout,self%crystal%spacegroup%seitz(i,j,n))
            end do
         end do
         call flush_(stdout)
      end do
      call text_(stdout,"end seitz_matrices")

      ! Print inverse symop indices
      call flush_(stdout)
      call show_(stdout,"begin inverse_symops ",self%crystal%spacegroup%n_seitz,dots=FALSE)
      do n = 1,self%crystal%spacegroup%n_seitz
         call put_(stdout,self%crystal%spacegroup%inverse_of_symops(n))
         call flush_(stdout)
      end do
      call text_(stdout,"end inverse_symops")

      ! Print symop produce indices
      call flush_(stdout)
      call show_(stdout,"begin symop_products ",self%crystal%spacegroup%n_seitz,dots=FALSE)
      call put_(stdout,self%crystal%spacegroup%product_of_symops(:,:))
      call text_(stdout,"end symop_products")

      ! Print unit_cell
      call flush_(stdout)
      call show_(stdout,"begin unit_cell ",self%crystal%n_unit_cell_atoms,dots=FALSE)
      do u = 1,self%crystal%n_unit_cell_atoms
         a = self%crystal%asym_atom_for_unit_cell_atom(u)
         call put_(stdout,self%asymmetric_unit_atom(a)%label)
         call put_(stdout,chemical_symbol_(self%asymmetric_unit_atom(a)))
         call put_(stdout,geometry(1,u))
         call put_(stdout,geometry(2,u))
         call put_(stdout,geometry(3,u))
         call put_(stdout,self%asymmetric_unit_atom(a)%site_disorder_group)
         call put_(stdout,self%asymmetric_unit_atom(a)%site_occupancy)
         call flush_(stdout)
      end do
      call text_(stdout,"end unit_cell")

      ! Print symops for unit cell atoms
      ! This is printed after unit_cell because CrystalExplorer likes
      ! to check the number of symops matches the number of unit cell
      ! atoms given
      call flush_(stdout)
      call show_(stdout,"begin symops_for_unit_cell_atoms ",self%crystal%n_unit_cell_atoms,dots=FALSE)
      do n = 1,self%crystal%n_unit_cell_atoms
         call put_(stdout,self%crystal%asym_symop_for_unit_cell_atom(n))
         call flush_(stdout)
      end do
      call text_(stdout,"end symops_for_unit_cell_atoms")

      ! Print unit_cell
      call flush_(stdout)
      call show_(stdout,"begin asymmetric_unit_atom_indices ",self%crystal%n_asymmetric_unit_atoms,dots=FALSE)
      do a = 1,size(self%crystal%unique_unit_cell_atom)
         call put_(stdout,self%crystal%unique_unit_cell_atom(a))
         call put_(stdout,-self%crystal%unique_uc_atom_offset(a)%element(1))
         call put_(stdout,-self%crystal%unique_uc_atom_offset(a)%element(2))
         call put_(stdout,-self%crystal%unique_uc_atom_offset(a)%element(3))
         call flush_(stdout)
      end do
      call text_(stdout,"end asymmetric_unit_atom_indices")

      ! Print ADP's
      if (has_nonzero_ADP_tensors_(self%asymmetric_unit_atom)) then
         call flush_(stdout)
         call text_(stdout,"begin adp ")
         do u = 1,self%crystal%n_unit_cell_atoms
            a = self%crystal%asym_atom_for_unit_cell_atom(u)
            s = self%crystal%asym_symop_for_unit_cell_atom(u)
            call put_(stdout,chemical_symbol_(self%asymmetric_unit_atom(a)))
            therm = self%asymmetric_unit_atom(a)%ADP_tensor  ! This must be in cartesian axes
            call change_basis_using_(therm,seitz(:,:,s))  ! Use R^T to change U tensor basis
            if (angst) therm = ANGSTROM_PER_BOHR*ANGSTROM_PER_BOHR*therm
            call put_(stdout,therm(1,1))
            call put_(stdout,therm(2,2))
            call put_(stdout,therm(3,3))
            call put_(stdout,therm(1,2))
            call put_(stdout,therm(1,3))
            call put_(stdout,therm(2,3))
            call flush_(stdout)
         end do
         call text_(stdout,"end adp")
      end if

      ! Cleanup
      call destroy_(geometry)
      call destroy_(ulist)
      call destroy_(seitz)
   end subroutine

   subroutine make_CX_atoms(self,inside,outside)
   ! Outputs the cluster information, atoms inside and outside the
   ! surface with their unit cell index and offset, for the Crystal
   ! Explorer program. Should be called only when there is a saved
   ! surface.
      type(CLUSTER_TYPE), IN :: self
      MAT(INT,:,:), allocatable, OUT :: inside
      MAT(INT,:,:), allocatable, OUT :: outside
      INT :: a,i,code,u,h1,h2,h3
   ENSURE(allocated(self%occupation_list),"CLUSTER:make_CX_atoms ... no occupation_list")
   ENSURE(self%n_atoms>=size(self%fragment_geometry,2),"CLUSTER:make_CX_atoms ... no atoms outside the surface!")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:make_CX_atoms ... no atom data")

      ! Print out atoms and shifts for atoms inside surface
      call create_(inside,size(self%fragment_geometry,2),4)
      do a = 1,size(self%fragment_geometry,2)

         ! Occupation code
         code = self%occupation_list(a)

         ! Extract cell and unit cell ato "u"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)
         u = CLUSTER_UA_FROM_CODE(code)

         ! Add overall offset
         h1 = h1+self%fragment_offset(1)
         h2 = h2+self%fragment_offset(2)
         h3 = h3+self%fragment_offset(3)
         inside(a,1) = u
         inside(a,2) = h1
         inside(a,3) = h2
         inside(a,4) = h3
      end do

      ! Print out atoms and shifts for atoms outside surface
      if ((self%n_atoms-size(self%fragment_geometry,2))==0) return
      call create_(outside,self%n_atoms-size(self%fragment_geometry,2),4)
      do a = size(self%fragment_geometry,2)+1,self%n_atoms
         i = a-size(self%fragment_geometry,2)

         ! Occupation code
         code = self%occupation_list(a)

         ! Extract cell and unit cell ato "u"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)
         u = CLUSTER_UA_FROM_CODE(code)

         ! Add overall offset
         h1 = h1+self%fragment_offset(1)
         h2 = h2+self%fragment_offset(2)
         h3 = h3+self%fragment_offset(3)
         outside(i,1) = u
         outside(i,2) = h1
         outside(i,3) = h2
         outside(i,4) = h3
      end do
   end subroutine

   subroutine put_CX_atoms(self)
   ! Outputs the cluster information, atoms inside and outside the
   ! surface with their unit cell index and offset, for the Crystal
   ! Explorer program. Should be called only when there is a saved
   ! surface.
      type(CLUSTER_TYPE), IN :: self
      INT :: a,code,u,h1,h2,h3
   ENSURE(allocated(self%occupation_list),"CLUSTER:put_CX_atoms ... no occupation_list")
   ENSURE(self%n_atoms>=size(self%fragment_geometry,2),"CLUSTER:put_CX_atoms ... no atoms outside the surface!")
   ENSURE(allocated(self%asymmetric_unit_atom),"CLUSTER:put_CX_atoms ... no atom data")

      ! Print out atoms and shifts for atoms inside surface
      call flush_(stdout)
      call show_(stdout,"begin atoms_inside_surface",size(self%fragment_geometry,2),dots=FALSE)
      do a = 1,size(self%fragment_geometry,2)

         ! Occupation code
         code = self%occupation_list(a)

         ! Extract cell and unit cell ato "u"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)
         u = CLUSTER_UA_FROM_CODE(code)

         ! Add overall offset
         h1 = h1+self%fragment_offset(1)
         h2 = h2+self%fragment_offset(2)
         h3 = h3+self%fragment_offset(3)
         call put_(stdout,u)
         call put_(stdout,h1)
         call put_(stdout,h2)
         call put_(stdout,h3)
         call flush_(stdout)
      end do
      call text_(stdout,"end atoms_inside_surface")

      ! Print out atoms and shifts for atoms outside surface
      call flush_(stdout)
      call show_(stdout,"begin atoms_outside_surface",self%n_atoms-size(self%fragment_geometry,2),dots=FALSE)
      do a = size(self%fragment_geometry,2)+1,self%n_atoms

         ! Occupation code
         code = self%occupation_list(a)

         ! Extract cell and unit cell ato "u"
         h1 = CLUSTER_H1_FROM_CODE(code)
         h2 = CLUSTER_H2_FROM_CODE(code)
         h3 = CLUSTER_H3_FROM_CODE(code)
         u = CLUSTER_UA_FROM_CODE(code)

         ! Add overall offset
         h1 = h1+self%fragment_offset(1)
         h2 = h2+self%fragment_offset(2)
         h3 = h3+self%fragment_offset(3)
         call put_(stdout,u)
         call put_(stdout,h1)
         call put_(stdout,h2)
         call put_(stdout,h3)
         call flush_(stdout)
      end do
      call text_(stdout,"end atoms_outside_surface")
   end subroutine

end module
