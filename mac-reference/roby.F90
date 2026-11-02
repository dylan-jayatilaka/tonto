!========================================================================
!
!  ROBY:
!
!  Read and evaluate Roby atom data for population and bond analysis.
!  This module has been completely rewritten from Chris Taylor's version,
!  based on Steve Wolff's checked version.
!
! Copyright (C) Chris Taylor, UWA, 1999.
! Copyright (C) Stephen Wolff, UWA, 2000-2001.
! Copyright (C) Dylan Jayatilaka, 2001
! Copyright (C) Dylan Jayatilaka, 2021
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
!========================================================================
module ROBY_MODULE

#  include "macros"

   include "roby.use"

   implicit none

   include "roby.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self)
   ! Allocate an object
      type(ROBY_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"ROBY:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(ROBY_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"ROBY:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine create_1(self,mol)
   ! Create space for the object
      type(ROBY_TYPE), allocatable, OUT :: self
      type(MOLECULE_TYPE), IN :: mol
      call create_(self)
      call set_defaults_(self,mol)
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      type(ROBY_TYPE), INOUT :: self
      call destroy_(self%n1)
      call destroy_(self%n2)
      call destroy_(self%bnd_index)
      call destroy_(self%cov_index)
      call destroy_(self%ion_index)
      call destroy_(self%percent_covalency_P)
      call destroy_(self%percent_covalency_A)
      call destroy_(self%gould_charge)
      call destroy_(self%cruickshank_charge)
      call destroy_(self%summed_n2)
      call destroy_(self%summed_n3)
      call destroy_(self%subgroup_pop)
      call destroy_(self%atom_list)
      call destroy_(self%atom_group)
      call destroy_(self%atom_a)
      call destroy_(self%atom_b)
      call destroy_(self%atom_ab)
      call destroy_(self%theta_C)
      call destroy_(self%eigen_C)
      call destroy_(self%theta_I)
      call destroy_(self%eigen_I)
      call destroy_(self%theta_pop_C)
      call destroy_(self%theta_pop_C_A)
      call destroy_(self%theta_pop_C_B)
      call destroy_(self%theta_pop_I)
      call destroy_(self%theta_pop_I_A)
      call destroy_(self%theta_pop_I_B)
      call destroy_(self%theta_cov_index)
      call destroy_(self%theta_ion_index)
      call destroy_(self%proportion_a)
      call destroy_(self%pair)

      ! Energies
      call destroy_(self%E1)
      call destroy_(self%E2)
      call destroy_(self%E1_cov)
      call destroy_(self%E1_ion)
      call destroy_(self%E1_bond)
   end subroutine

   PURE subroutine destroy_theta_info(self)
   ! Delete all the temporary theta subspace information created by
   ! various bond index routines
      type(ROBY_TYPE), INOUT :: self

      ! Theta population indices
      call destroy_(self%theta_ion_index)
      call destroy_(self%theta_cov_index)
      call destroy_(self%theta_pop_I_B)
      call destroy_(self%theta_pop_I_A)
      call destroy_(self%theta_pop_I)
      call destroy_(self%theta_pop_C_B)
      call destroy_(self%theta_pop_C_A)
      call destroy_(self%theta_pop_C)

      ! Theta energy indiced
      call destroy_(self%theta_E_I_B)
      call destroy_(self%theta_E_I_A)
      call destroy_(self%theta_E_I)
      call destroy_(self%theta_E_C_B)
      call destroy_(self%theta_E_C_A)
      call destroy_(self%theta_E_C)

      ! Theta eigenstates
      call destroy_(self%eigen_I)
      call destroy_(self%theta_I)
      call destroy_(self%pair)
      call destroy_(self%theta_angle)
      call destroy_(self%eigen_C)
      call destroy_(self%theta_C)
   end subroutine

   PURE subroutine destroy_bond_info(self)
   ! Delete all the bond index/bond energy info
      type(ROBY_TYPE), INOUT :: self
      call destroy_(self%n1)
      call destroy_(self%n2)
      call destroy_(self%percent_covalency_A)
      call destroy_(self%percent_covalency_P)
      call destroy_(self%ion_index)
      call destroy_(self%cov_index)
      call destroy_(self%bnd_index)
      call destroy_(self%E1_cov)
      call destroy_(self%E1_ion)
      call destroy_(self%E1_bond)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults_0(self)
   ! Set default SCF data values
      type(ROBY_TYPE), INOUT :: self

      ! Important
      self%roby_kind = " "
      self%zero_angle_cutoff = TOL(2)*RADIAN_PER_DEGREE
      call set_use_NAOs_(self,FALSE)

      ! Other rubbish
      self%output_theta_info = TRUE
      self%n_dial_columns = 4
      self%analyze_all_atom_pairs = FALSE
      self%analyze_vdw_atom_pairs = FALSE
      self%bond_range_factor = 0.4*BOHR_PER_ANGSTROM
   end subroutine

   PURE subroutine set_defaults_1(self,mol)
   ! Set only the defaults derivable from molecule "mol"
      type(ROBY_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
   ENSURE(allocated(mol%atom),"ROBY:set_defaults_1 ... no atom list")
   ENSURE(allocated(mol%density_mx),"ROBY:set_defaults_1 ... no density matrix")
   ENSURE(allocated(mol%overlap_mx),"ROBY:set_defaults_1 ... no overlap matrix")
      self%charge = mol%charge
      self%spin_multiplicity = mol%spin_multiplicity

      ! Note: NOT pointer assignment
      self%atom = mol%atom
      self%density_mx = mol%density_mx
      self%overlap_mx = mol%overlap_mx
      self%core_mx = mol%core_mx
      if (allocated(mol%fock_mx)) self%fock_mx = mol%fock_mx
   end subroutine

   PURE subroutine set_roby_kind(self,val)
   ! Set default SCF data values
      type(ROBY_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%roby_kind = val
      call to_lower_case_(self%roby_kind)
      select case (self%roby_kind)
      case ("atom_bond_analysis       ")
      case ("atom_e1_exact_analysis   ")
      case ("atom_e1_krogel_analysis  ")
   ! case("atom_energy_bond_analysis")
      case ("atom_hyper_analysis      ")
      case ("group_bond_analysis      ")
      case ("group_hyper_analysis     ")
      end select
   end subroutine

   PURE subroutine set_show_calcs(self,val)
   ! Set whether to show detailed RGBI calcs.
      type(ROBY_TYPE), INOUT :: self
      BIN, IN :: val
      self%show_calcs = val
   end subroutine

   PURE subroutine set_use_spherical_averaging(self,val)
   ! Set whether to use spherical averaging for the NAOs.
   ! Spherical averaging is always used for ANOs.
      type(ROBY_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_spherical_averaging = val
   end subroutine

   PURE subroutine set_use_NAOs(self,val)
   ! Set whether to use NAOs as atomic projectors instead of ANOs as
   ! in the original 2008 paper.
      type(ROBY_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_NAOs = val
      if (val) then
         self%occupied_ANO_cutoff = ROBY_OCCUPIED_NAO_CUTOFF
      else
         self%occupied_ANO_cutoff = ROBY_OCCUPIED_ANO_CUTOFF
      end if
   end subroutine

   PURE subroutine set_use_projected_NAOs(self,val)
   ! Set whether to use NAOs as atomic projectors instead of ANOs as
   ! in the original 2008 paper.
      type(ROBY_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_projected_NAOs = val

   ! if (val) then
   !    .use_NAOs = FALSE
   !    .occupied_ANO_cutoff = ROBY_OCCUPIED_NAO_CUTOFF
   ! else
   !    .occupied_ANO_cutoff = ROBY_OCCUPIED_ANO_CUTOFF
   ! end
   end subroutine

   PURE subroutine set_output_ci_latex_labels(self,val)
   ! Set whether to output c & i labels above latex structure in 
   ! RGBI plots
      type(ROBY_TYPE), INOUT :: self
      BIN, IN :: val
      self%output_ci_latex_labels = val
   end subroutine


!  =============
!  Input methods
!  =============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(ROBY_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","ROBY:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"ROBY:read_keywords ... unexpected end of file while reading list keywords")
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
      type(ROBY_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)

      case ("}                       ")  ! exit case

      case ("analyze_all_atom_pairs= "); call read_analyze_all_atom_pairs_(self)
      case ("analyze_vdw_atom_pairs= "); call read_analyze_VDW_atom_pairs_(self)
      case ("analyze_xh_bonds=       "); call read_analyze_XH_bonds_(self)
      case ("atom_groups=            "); call read_atom_groups_(self)
      case ("atom_list=              "); call read_atom_list_(self)

      case ("bond_scale_factors=     "); call read_bond_scale_factor_(self)

      case ("kind=                   "); call read_kind_(self)

      case ("occupied_ano_cutoff=    "); call read_occupied_ANO_cutoff_(self)
      case ("output_ci_latex_labels= "); call read_output_ci_labels_(self)
      case ("n_dial_columns=         "); call read_n_dial_columns_(self)
      case ("output_theta_info=      "); call read_output_theta_info_(self)

   ! case ("put_core_mx             "); stdout.put(.core_mx)
   ! case ("put_density_mx          "); stdout.put(.density_mx)
   ! case ("put_fock_mx             "); stdout.put(.fock_mx)
   ! case ("put_overlap_mx          "); stdout.put(.overlap_mx)

      case ("show_calcs=             "); call read_show_calcs_(self)
      case ("show_calc_groups=       "); call read_show_calc_groups_(self)

      case ("use_naos=               "); call read_use_NAOs_(self)
      case ("use_pnaos=              "); call read_use_projected_NAOs_(self)
      case ("use_projected_naos=     "); call read_use_projected_NAOs_(self)
      case ("use_spherical_averaging="); call read_use_spherical_averaging_(self)

      case ("zero_angle_cutoff=      "); call read_zero_angle_cutoff_(self)

      case default; allocate(tonto%known_keywords(19))
      tonto%known_keywords(1) = "}                       "
      tonto%known_keywords(2) = "analyze_all_atom_pairs= "
      tonto%known_keywords(3) = "analyze_vdw_atom_pairs= "
      tonto%known_keywords(4) = "analyze_xh_bonds=       "
      tonto%known_keywords(5) = "atom_groups=            "
      tonto%known_keywords(6) = "atom_list=              "
      tonto%known_keywords(7) = "bond_scale_factors=     "
      tonto%known_keywords(8) = "kind=                   "
      tonto%known_keywords(9) = "occupied_ano_cutoff=    "
      tonto%known_keywords(10) = "output_ci_latex_labels= "
      tonto%known_keywords(11) = "n_dial_columns=         "
      tonto%known_keywords(12) = "output_theta_info=      "
      tonto%known_keywords(13) = "show_calcs=             "
      tonto%known_keywords(14) = "show_calc_groups=       "
      tonto%known_keywords(15) = "use_naos=               "
      tonto%known_keywords(16) = "use_pnaos=              "
      tonto%known_keywords(17) = "use_projected_naos=     "
      tonto%known_keywords(18) = "use_spherical_averaging="
      tonto%known_keywords(19) = "zero_angle_cutoff=      "
      call unknown_(tonto,word,"ROBY:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine update(self)
   ! Make sure the input satisfies sanity checks and generate any
   ! other missing data
      type(ROBY_TYPE), INOUT :: self
   ENSURE(self%roby_kind/=" ","ROBY:update ... no calculation kind specified")
   ENSURE(allocated(self%atom),"ROBY:update ... no atom data supplied")
! ENSURE(.atom_list.allocated NEQV .atom_group.allocated,"atom_list and atom_group both used!")
      if (FALSE) self = self
   end subroutine

   subroutine read_kind(self)
   ! Read the SCF type
      type(ROBY_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      call set_roby_kind_(self,val)
   end subroutine

   subroutine read_atom_list(self)
   ! Read the list of atoms indices defining a Roby group
      type(ROBY_TYPE), INOUT :: self
      call read_all_(stdin,self%atom_list)
   end subroutine

   subroutine read_atom_groups(self)
   ! Read the list of atoms indices defining a Roby group
      type(ROBY_TYPE), INOUT :: self
      call read_all_(stdin,self%atom_group)
   end subroutine

   subroutine read_occupied_ANO_cutoff(self)
   ! Read the cutoff to decide occupied ANOs or NAOs
      type(ROBY_TYPE), INOUT :: self
      call read_quantity_(stdin,self%occupied_ANO_cutoff)
   end subroutine

   subroutine read_zero_angle_cutoff(self)
   ! Angles (in radians) *less* than this one inputted are regareded as zero
      type(ROBY_TYPE), INOUT :: self
      call read_quantity_(stdin,self%zero_angle_cutoff)
   end subroutine

   subroutine read_output_ci_labels(self)
   ! Read whether to output c/i labels above/below the bond line in
   ! the latex structure figures - for use with make-rgbi-pic script.
      type(ROBY_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_output_ci_latex_labels_(self,val)
   end subroutine

   subroutine read_output_theta_info(self)
   ! Read in a switch which tells whether to output detailed
   ! theta-subspace bond information
      type(ROBY_TYPE), INOUT :: self
      call read_(stdin,self%output_theta_info)
   end subroutine

   subroutine read_n_dial_columns(self)
   ! Read the number of dial diagrams per row on the survey page
      type(ROBY_TYPE), INOUT :: self
      call read_(stdin,self%n_dial_columns)
      ENSURE(self%n_dial_columns>0,"ROBY:read_n_dial_columns ... n_dial_columns= must be positive")
   end subroutine

   subroutine read_analyze_all_atom_pairs(self)
   ! Read in a switch which tells whether to bond-analyze all atoms
   ! pairs or not - actually this just changes the bond_range_factor
   ! by an order of magnitude
      type(ROBY_TYPE), INOUT :: self
      call read_(stdin,self%analyze_all_atom_pairs)
      if (self%analyze_all_atom_pairs) then
         self%bond_range_factor = FIVE*BOHR_PER_ANGSTROM
      end if
   end subroutine

   subroutine read_analyze_VDW_atom_pairs(self)
   ! Read in a switch which tells whether to bond-analyze all atoms
   ! pairs or not - actually this just changes the bond_range_factor
   ! by an order of magnitude
      type(ROBY_TYPE), INOUT :: self
      call read_(stdin,self%analyze_vdw_atom_pairs)
   end subroutine

   subroutine read_analyze_XH_bonds(self)
   ! Read whether to bond-analyze X-H bonds.
   ! NOTE: functionality not used ...
      type(ROBY_TYPE), INOUT :: self
      call read_(stdin,self%analyze_XH_bonds)
   end subroutine

   subroutine read_show_calcs(self)
   ! Read whether to show detailed RGBI calcs for each bond.
      type(ROBY_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_show_calcs_(self,val)
   end subroutine

   subroutine read_show_calc_groups(self)
   ! Read whether to show detailed RGBI calcs for each bond.
      type(ROBY_TYPE), INOUT :: self
      BIN :: val
      call read_all_(stdin,self%show_calc_groups)
      call set_show_calcs_(self,val)
   end subroutine

   subroutine read_use_NAOs(self)
   ! Read whether to use NAOs as atomic projectors instead of ANOs as
   ! in the original 2008 paper.
      type(ROBY_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_NAOs_(self,val)
   end subroutine

   subroutine read_use_projected_NAOs(self)
   ! Read whether to use projected NAOs instead of NAOs
      type(ROBY_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_projected_NAOs_(self,val)
   end subroutine

   subroutine read_use_spherical_averaging(self)
   ! Read whether to use sperhically averaged NAOs.
      type(ROBY_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_spherical_averaging_(self,val)
   end subroutine

   subroutine read_bond_scale_factor(self)
   ! Read in a bond scale factor used to multiply the sum of the
   ! Bragg-Slater radii for two atoms, to determine a distance cutoff
   ! within which atoms are regarded to be bonded
      type(ROBY_TYPE), INOUT :: self
      call read_(stdin,self%bond_range_factor)
   end subroutine


!  ===================
!  Information methods
!  ===================

   pure function is_homoleptic(self) result (res)
   ! Returns TRUE if the calculation data is "homoleptic"
   ! i.e. involving groups of atoms without single-atom groups.
      type(ROBY_TYPE), IN :: self
      BIN :: res

      ! This isn't the best test
      res = size(self%atom_group)/=size(self%atom_list)
   end function

   function do_pair(self,a,b) result (res)
   ! Returns TRUE if the bond index calculation should include the
   ! pair of groups (a,b): every pair for a homoleptic calculation or
   ! when .analyze_all_atom_pairs is set; only vdW-contact pairs when
   ! .analyze_vdw_atom_pairs is set; otherwise only pairs bonded by the
   ! .bond_range_factor distance criterion. This is the same selection
   ! the bond tables print, so no pair is computed and then unused.
      type(ROBY_TYPE), IN :: self
      INT, IN :: a,b
      BIN :: res
      if (is_homoleptic_(self)) then

         ! Do all
         res = TRUE
      else if (self%analyze_all_atom_pairs) then

         ! Do all
         res = TRUE
      else if (self%analyze_vdw_atom_pairs) then

         ! Do only vdw pairs
         res = vdw_bonded_(self%atom,a,b,self%bond_range_factor)
      else

         ! Do only bonded pairs
         res = bonded_(self%atom,a,b,self%bond_range_factor)
      end if
   end function

   PURE function n_bf(self,group) result (res)
   ! Return the number of basis functions in the atom group
   ! whose indices are given in "group"; but if "group" is
   ! not present return the dimension of the full overlap matrix.
      type(ROBY_TYPE), IN :: self
      VEC(INT,:), optional, IN :: group
      INT :: res
   ENSURE(allocated(self%atom),"ROBY:n_bf ... No atom information")
   ENSURE(allocated(self%overlap_mx),"ROBY:n_bf ... No overlap matrix")
      if (present(group)) then; res = no_of_basis_functions_(self%atom(group))
      else; res = size(self%overlap_mx,1)
      end if
   end function

   PURE function n_bf_a(self) result (res)
   ! Return the number of basis functions in the .atom_a group
      type(ROBY_TYPE), IN :: self
      INT :: res
   ENSURE(allocated(self%atom),"ROBY:n_bf_a ... No atom infpormation")
   ENSURE(allocated(self%atom_a),"ROBY:n_bf_a ... No atom A group")
      res = n_bf_(self,self%atom_a)
   end function

   PURE function n_bf_b(self) result (res)
   ! Return the number of basis functions in the .atom_b group
      type(ROBY_TYPE), IN :: self
      INT :: res
   ENSURE(allocated(self%atom),"ROBY:n_bf_b ... No atom infpormation")
   ENSURE(allocated(self%atom_b),"ROBY:n_bf_b ... No atom B group")
      res = n_bf_(self,self%atom_b)
   end function

   PURE function n_bf_ab(self) result (res)
   ! Return the number of basis functions in the .atom_b group
      type(ROBY_TYPE), IN :: self
      INT :: res
   ENSURE(allocated(self%atom),"ROBY:n_bf_ab ... No atom infpormation")
   ENSURE(allocated(self%atom_ab),"ROBY:n_bf_ab ... No atom AB group")
      res = n_bf_(self,self%atom_ab)
   end function

   PURE function n_group(self) result (res)
   ! Return the number of roby atom groups
      type(ROBY_TYPE), IN :: self
      INT :: res
   ENSURE(allocated(self%atom_group),"ROBY:n_group ... no atom groups!")
      res = size(self%atom_group)
   end function

   PURE function no_of_occupied_NAOs(self,group,tol) result (res)
   ! Return the number of occupied NAOs for the group of atoms whose
   ! indices are in "group"
      type(ROBY_TYPE), IN :: self
      VEC(INT,:), IN :: group
      REAL, optional, IN :: tol
      INT :: res
      REAL :: eps
   ENSURE(allocated(self%atom),"ROBY:no_of_occupied_NAOs ... no atom information")
      eps = self%occupied_ANO_cutoff
      if (present(tol)) eps = tol
      res = no_of_occupied_NAOs_(self%atom(group),tol=eps)
   end function

   PURE function no_of_occupied_ANOs(self,group,tol) result (res)
   ! Return the number of occupied atomic natural orbitals for
   ! the group of atoms whose indices are in "group"
      type(ROBY_TYPE), IN :: self
      VEC(INT,:), IN :: group
      REAL, optional, IN :: tol
      INT :: res
      REAL :: eps
   ENSURE(allocated(self%atom),"ROBY:no_of_occupied_ANOs ... no atom information")
      eps = self%occupied_ANO_cutoff
      if (present(tol)) eps = tol
      res = no_of_occupied_ANOs_(self%atom(group),tol=eps)
   end function

   PURE function unique_tags(self) result (res)
   ! Return a list of unique tags for the atom groups
      type(ROBY_TYPE), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      VEC(STR(len=STR_SIZE),:), allocatable :: symbols
      INT :: g
      if (is_homoleptic_(self)) then
         call create_(res,size(self%atom_group))
         do g = 1,size(self%atom_group)
            symbols = self%atom(self%atom_group(g)%element)%tag
            res(g) = to_concatenated_str_(symbols)
            call destroy_(symbols)
         end do
      else
         res = self%atom(self%atom_list)%tag
      end if
   end function

   PURE function numbered_tag(self,group) result (res)
   ! Output the atom labels with their numbers for the atom indices
   ! given in "group"
      type(ROBY_TYPE), IN :: self
      VEC(INT,:), IN :: group
      STR(len=STR_SIZE) :: res
      INT :: g

      ! First atom
      res = trim(chemical_symbol_(self%atom(group(1))))//trim(to_str_(group(1)))

      ! The rest, if any
      do g = 2,size(group)
         res = trim(res)//"-"//trim(chemical_symbol_(self%atom(group(1))))//trim(to_str_(group(g)))
      end do
   end function

   pure function show_calc(self) result (res)
   ! Returns TRUE if debug calc in formation should be shown.
      type(ROBY_TYPE), IN :: self
      BIN :: res

   ! ENSURE(.atom_group.allocated,"no atom_group")
      INT :: i
      VEC(INT,:), allocatable :: show
      res = self%show_calcs
      if (allocated(self%show_calc_groups)) then
         if (NOT res) then
            do i = 1,size(self%show_calc_groups)
               show = self%show_calc_groups(i)%element
               res = all(show==self%atom_ab)
               if (res) exit
            end do
         end if
      end if
   end function


! ============
! Main methods
! ============

   subroutine atom_bond_analysis(self)
   ! Do a Roby bond and Gould charge analysis.
      type(ROBY_TYPE), INOUT :: self
      INT :: i,n_atom
   ENSURE(allocated(self%atom),"ROBY:atom_bond_analysis ... no atoms")

      ! Make atom list if it doesn't exist
      if (NOT allocated(self%atom_list)) then
         n_atom = size(self%atom)
         self%atom_list = [(i,i = 1,n_atom)]
      end if

      ! Each atom is a Roby subspace
      if (NOT allocated(self%atom_group)) then
         call make_single_atom_groups_(self)
      end if

      ! Bond orders
      select case (self%roby_kind)

      case ("atom_bond_analysis       ")
         self%is_hyper_analysis = FALSE
         call bond_analysis_(self)
   !    .charge_analysis

      case ("atom_hyper_analysis      ")
         self%is_hyper_analysis = TRUE
         call hyper_analysis_(self)

      case ("group_bond_analysis      ")
         self%is_hyper_analysis = FALSE
         call bond_analysis_(self)

      case ("group_hyper_analysis      ")
         self%is_hyper_analysis = TRUE
         call hyper_analysis_(self)

      case default
         allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "atom_bond_analysis       "
         tonto%known_keywords(2) = "atom_hyper_analysis      "
         tonto%known_keywords(3) = "group_bond_analysis      "
         tonto%known_keywords(4) = "group_hyper_analysis      "
         call unknown_(tonto,self%roby_kind,"ROBY:atom_bond_analysis")
         deallocate(tonto%known_keywords)
      end select

      ! Results
      call put_basics_(self)
      call put_populations_(self)
      call put_bond_indices_(self)

      ! Draw the pictures, if the tools are installed. It must be HERE and not
      ! at the end of .bond_analysis: that routine only computes the indices,
      ! while the rgbi-bondtable*.tex and rgbi-atom-labels*.tex fragments the
      ! picture is made from are written by .put_bond_indices, just above.
      call draw_pictures_(self)
   end subroutine

   subroutine bond_analysis(self)
   ! Do a Roby-Gould bond and charge analysis for the 
   ! atom groups given in .atom_groups
      type(ROBY_TYPE), INOUT :: self
      INT :: n_group,a,b,n_bond_do_H,n_bond_no_H
      REAL :: pop,ind,cov,ion,pccp,pcca
      type(TEXTFILE_TYPE), allocatable :: dial
   ENSURE(allocated(self%atom_group),"ROBY:bond_analysis ... no atom group defined")

      ! Initialize
      n_group = n_group_(self)
      call destroy_bond_info_(self)

      ! Population of all atoms
      call to_sequence_(self%atom_ab,1,size(self%atom))
      self%n_all = population_(self,self%atom_ab)
      call destroy_(self%atom_ab)

      ! Populations of groups
      call create_(self%n1,n_group)
      do a = 1,n_group
         self%atom_a = self%atom_group(a)%element
         self%n1(a) = population_(self,self%atom_a)
      end do

      ! Dial diagrams
      if (self%output_theta_info) then
         call create_(dial,"rgbi-dial-figures.tex")
         call open_for_(dial,"writing")
      end if

      ! Clean
      call create_(self%n2,n_group,n_group); self%n2 = ZERO
      call create_(self%bnd_index,n_group,n_group)
      call create_(self%cov_index,n_group,n_group)
      call create_(self%ion_index,n_group,n_group)
      call create_(self%percent_covalency_P,n_group,n_group)
      call create_(self%percent_covalency_A,n_group,n_group)

      ! Bonds for pairs of groups
      n_bond_do_H = 0
      n_bond_no_H = 0
      do a = 1,n_group
         do b = a+1,n_group
            if (NOT do_pair_(self,a,b)) cycle

         ! Atom groups
            self%atom_a = self%atom_group(a)%element
            self%atom_b = self%atom_group(b)%element
            self%atom_ab = [self%atom_a,self%atom_b]

         ! Pair populations !!!!!!!!
            pop = population_(self,self%atom_ab)
            self%n2(a,b) = pop
            self%n2(b,a) = pop
         !!!!!!!!!!!!!!!!!!!!!!!!!!!

         !!!!!!!!!!!!!!!!
            call make_theta_info_(self)
            ind = gould_bond_index_(self,cov,ion,pccp,pcca)
         !!!!!!!!!!!!!!!!
            self%bnd_index(a,b) = ind
            self%cov_index(a,b) = cov
            self%ion_index(a,b) = ion
            self%percent_covalency_P(a,b) = pccp
            self%percent_covalency_A(a,b) = pcca
            self%bnd_index(b,a) = ind
            self%cov_index(b,a) = cov
            self%ion_index(b,a) = -ion
            self%percent_covalency_P(b,a) = pccp
            self%percent_covalency_A(b,a) = pcca
            if (self%output_theta_info) then
               call put_theta_info_(self)
               call put_theta_BI_info_(self)
               n_bond_do_H = n_bond_do_H+1
               call put_dial_BI_info_do_H_(self,a,b,dial,n_bond_do_H)
               if (is_not_a_H_atom_(self%atom(a))) then
                  if (is_not_a_H_atom_(self%atom(b))) then
                     n_bond_no_H = n_bond_no_H+1
            ! Keep original atom label
                     call put_dial_BI_info_no_H_(self,a,b,dial,n_bond_no_H)
                  end if
               end if
            end if
         end do
      end do
      if (self%output_theta_info) then
         call close_(dial)
         call put_dial_table_do_H(n_bond_do_H,self%n_dial_columns)
         call put_dial_table_no_H(n_bond_no_H,self%n_dial_columns)
      end if
      call put_xyz_file_(self%atom)
   end subroutine

   subroutine draw_pictures(self)
   ! Draw the RGBI pictures -- the labelled molecular structure and the page of
   ! dial diagrams -- by running the make-rgbi-pic script on the .tex fragments
   ! just written. This is what makes the pictures appear at the end of a job
   ! without the user having to do anything.
   !
   ! SILENT NO-OP IF THE SCRIPT IS NOT INSTALLED, deliberately. The brief was
   ! "if the components are there, the plots are built automatically", and the
   ! converse matters just as much: a machine without them must behave exactly
   ! as it did before, saying nothing. Otherwise every tests/rgbi job would
   ! start printing a warning into the output it is compared against, and 13
   ! blessed references would have to be reissued to say "you have no LaTeX".
   !
   ! Failure of an INSTALLED script is different, and is reported -- but never
   ! fatal. The .tex fragments and geometry.xyz are on disk either way, so
   ! nothing is lost but the convenience, and the user can run the script by
   ! hand. Same contract as SYSTEM:call_gnuplot for the HAR plots.
   !
   ! MPI: guarded on the master rank, because .execute_command is NOT
   ! collective -- without the guard every rank spawns its own copy of the
   ! pipeline onto the same output files. The reporting below is therefore a
   ! raw write to .stdout_unit rather than stdout.text: TEXTFILE bookkeeping
   ! IS collective, so printing on master alone through stdout would desync the
   ! ranks (pitfall 8, docs/TONTO_DEVELOPER_INFO.md §1a).
      type(ROBY_TYPE), IN :: self
      BIN :: has_H
      INT :: a
      if (NOT is_master_processor_(tonto)) return

      ! Is the script installed? If not, do nothing and say nothing.
      call execute_(tonto,"command -v make-rgbi-pic >/dev/null 2>&1")
      if (command_failed_(tonto)) return

      ! Are there any hydrogens to keep? If not, the +H and -H fragments are
      ! identical and one drawing is enough.
      has_H = FALSE
      do a = 1,size(self%atom)
         if (NOT is_not_a_H_atom_(self%atom(a))) has_H = TRUE
      end do
      call run_picture_script_(self,"--no-H")
      if (has_H) call run_picture_script_(self,"--do-H")
   end subroutine

   subroutine run_picture_script(self,hopt)
   ! Run make-rgbi-pic with the hydrogen option "hopt", and report a failure
   ! without making it fatal. See .draw_pictures for the reasoning.
      type(ROBY_TYPE), IN :: self
      STR(len=*), IN :: hopt
      STR(len=STR_SIZE) :: cmd

      ! Built in a STR and passed with trim: BUFFER:put_str copies the DECLARED
      ! length, so an over-long literal corrupts memory in a release build.
      cmd = "make-rgbi-pic --quiet "//trim(hopt)
      if (self%output_ci_latex_labels) cmd = trim(cmd)//" --do-ci"
      call execute_(tonto,trim(cmd))
      if (command_failed_(tonto)) then
         if (IO_is_allowed_(tonto)) then

            ! To std_err, NOT stdout. A picture is a convenience; stdout is the
            ! scientific result and is what every blessed reference is compared
            ! against. Writing here would mean that ANY user with a broken or
            ! stale make-rgbi-pic on their PATH silently fails 12 of the 13
            ! tests/rgbi jobs -- which is exactly what happened when this was
            ! first written, because ~/bin held a copy older than the repo's.
            ! The message still reaches the terminal and the job's stderr file.
            write(tonto%std_err_unit,*)
            write(tonto%std_err_unit,"(a)") "WARNING: could not draw the RGBI pictures with:"
            write(tonto%std_err_unit,"(a)") "   "//trim(cmd)
            write(tonto%std_err_unit,"(a)") "The rgbi-*.tex files and geometry.xyz were written anyway,"
            write(tonto%std_err_unit,"(a)") "so you can do it yourself. To find out what is missing, run:"
            write(tonto%std_err_unit,"(a)") "   rgbi_doctor.sh"
            write(tonto%std_err_unit,*)
         end if
      end if
   end subroutine

   subroutine hyper_analysis(self)
   ! Do a Roby-Gould hypervalent bond analysis for the 
   ! atom groups given in .atom_groups
      type(ROBY_TYPE), INOUT :: self
      INT :: n_group,a
      REAL :: ind,cov,ion,pccp,pcca
   ENSURE(allocated(self%atom_group),"ROBY:hyper_analysis ... no atom group defined")
      n_group = n_group_(self)

      ! Populations
      call make_populations_(self)
      call put_populations_(self)

      ! This is actually M, not ~M
      call to_sequence_(self%atom_b,1,size(self%atom))
      call to_sequence_(self%atom_ab,1,size(self%atom))

      ! Clean
      call destroy_bond_info_(self)
      call create_(self%bnd_index,n_group,n_group)
      call create_(self%cov_index,n_group,n_group)
      call create_(self%ion_index,n_group,n_group)
      call create_(self%percent_covalency_P,n_group,n_group)
      call create_(self%percent_covalency_A,n_group,n_group)

      ! Make bonds for A to ~M
      do a = 1,n_group

         ! A group
         self%atom_a = self%atom_group(a)%element

         !!!!!!!!!!!!!!!!
         call make_hyper_info_(self)
         !!!!!!!!!!!!!!!!
         ind = gould_bond_index_(self,cov,ion,pccp,pcca)
         self%bnd_index(a,a) = ind
         self%cov_index(a,a) = cov
         self%ion_index(a,a) = ion
         self%percent_covalency_P(a,a) = pccp
         self%percent_covalency_A(a,a) = pcca
         if (self%output_theta_info) then
            call put_theta_info_(self)
            call put_theta_BI_info_(self)
            call put_theta_AB_pops_(self)
         end if
      end do
   end subroutine


! =================================
! Roby population analysis routines
! =================================

   subroutine make_single_atom_groups(self)
   ! Make each Roby atom to be equal to a single atom formed from
   ! each element in the ".atom_list".
      type(ROBY_TYPE), INOUT :: self
      INT :: n_atom,a
   ENSURE(allocated(self%atom_list),"ROBY:make_single_atom_groups ... no atom list")
      n_atom = size(self%atom_list)
      call destroy_(self%atom_group)
      call create_(self%atom_group,n_atom)
      do a = 1,n_atom
         call create_(self%atom_group(a)%element,1)
         self%atom_group(a)%element(1) = self%atom_list(a)
      end do
   end subroutine

   subroutine make_populations(self)
   ! Make the Roby populations for the defined atom groups, for a
   ! given .density_mx 
      type(ROBY_TYPE), INOUT :: self
      INT :: a
   ENSURE(allocated(self%atom_group),"ROBY:make_populations ... no atom groups")
   ENSURE(allocated(self%density_mx),"ROBY:make_populations ... no rho matrix")

      ! Clean
      call destroy_(self%n1)
      call create_(self%n1,n_group_(self))

      ! Population of all atoms
      call to_sequence_(self%atom_ab,1,size(self%atom))
      self%n_all = population_(self,self%atom_ab)
      call destroy_(self%atom_ab)

      ! Make populations
      do a = 1,n_group_(self)
         self%atom_a = self%atom_group(a)%element
         self%n1(a) = population_(self,self%atom_a)
      end do
   end subroutine


!   make_pair_populations :: leaky, private
!   ! Make the pair Roby populations
!      self :: INOUT
!
!      n_group,a,b :: INT
!      pop :: REAL
!
!      n_group = .n_group
!
!      ! Clean
!      .n2.destroy
!      .n2.create(n_group,n_group)
!      .n2 = ZERO
!
!      ! Make pair populations
!      do a = 1,n_group
!      do b = a+1,n_group
!
!         if (.skip_pair(a,b)) cycle
!
!         .atom_a = .atom_group(a).element
!         .atom_b = .atom_group(b).element
!
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         .atom_ab = [.atom_a, .atom_b]
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!         pop = .population(.atom_ab)
!
!         .n2(a,b) = pop
!         .n2(b,a) = pop
!
!      end
!      end
!
!   end

   function population(self,group,ANO,reverse,rho) result (res)
   ! Return the total population for the projector P associated with
   ! the group of atoms whose indices are in "group", i.e
   !
   !   res = Tr (P_{group;ANO} .density_mx )
   !
   ! . If "rho" is present then it is used for the "density_mx".
   ! . If present, the columns of the "ANO" matrix are used to define
   !   the Roby projector instead of the atomic natural orbitals.
   ! . If "reverse" is present and TRUE, the orbitals used to make the
   !   projection operator are the *unoccupied* ANOs.
      type(ROBY_TYPE), INOUT :: self
      VEC(INT,:), IN :: group
      MAT(REAL,:,:), optional, IN :: ANO
      BIN, optional, IN :: reverse
      type(OPMATRIX_TYPE), allocatable, optional, IN :: rho
      REAL :: res
      MAT(REAL,:,:), allocatable :: P
      VEC(INT,:), allocatable :: gr
      INT :: n_gr
      BIN :: rev
   ENSURE(allocated(self%density_mx),"ROBY:population ... no density matrix")
   ENSURE(allocated(self%overlap_mx),"ROBY:population ... no overlap matrix")

      ! Reverse
      rev = FALSE
      if (present(reverse)) rev = reverse

      ! Group atom indices
      if (rev) then
         call to_sequence_(gr,1,size(self%atom))
         call prune_(gr,group)
      else
         gr = group
      end if

      ! No. of groups
      n_gr = n_bf_(self,gr)

      ! Make small P matrix
      call create_(P,n_gr,n_gr)
      call make_projection_mx_(self,P,gr,ANO)

   ! stdout.text("gr:")
   ! stdout.put(gr)
   ! stdout.text("P:")
   ! stdout.put(P)
   ! if (present(ANO)) then
   ! stdout.text("ANO:")
   ! stdout.put(ANO)
   ! end

      ! Evaluate
      res = expectation_(self,P,gr,rho)
   ! stdout.show("res = ",res)

      ! Clean
      call destroy_(P)
      call destroy_(gr)
   end function


!   subpopulation(subgroup,group,ANO,rho) result (res) :: private
!   ! Return the Roby subpopulation for the subgroup of atoms whose
!   ! indices are given in "subgroup", in the whole group of atoms
!   ! whose indices are are given in "group", i.e
!   !    res = Tr ( P_{subgroup} P_{group;ANO} P_{subgroup} 
!   !               density_mx)
!   ! If present, the columns of the "ANO" matrix are used to define
!   ! the middle Roby projector instead of the atomic natural orbitals.
!   ! Note that the real atomic natural orbitals are always used for
!   ! the P_{subgroup} projector.  This routine is useful for getting
!   ! theta orbital populations.
!      self :: INOUT
!      subgroup :: VEC{INT}, IN
!      group    :: VEC{INT}, IN
!      ANO :: MAT{REAL}, optional, IN
!      rho :: OPMATRIX@, optional, IN
!      res :: REAL
!
!   ENSURE(.density_mx.allocated,"no density matrix")
!   ENSURE(.overlap_mx.allocated,"no overlap matrix")
!
!      P,X,P_sub :: MAT{REAL}@
!      n_gr,n_sub :: INT
!
!      n_gr  = .n_bf(group)
!      n_sub = .n_bf(subgroup)
!
!      X.create(n_sub,n_sub)
!      P.create(n_gr,n_gr)
!      P_sub.create(n_sub,n_sub)
!
!      .make_projection_mx(P,group,ANO)
!      .make_projection_mx(P_sub,subgroup)
!      .project(P,P_sub,X,group,subgroup)
!
!
!      res = .expectation(X,subgroup,rho)
!
!      P_sub.destroy
!      P.destroy
!      X.destroy
!
!   end

! ================================
! Roby multiple shared populations
! ================================

!   make_shared_population :: leaky, private
!   ! Evaluate the Roby shared population
!
!   ENSURE(.atom_group.allocated,"no atom group supplied")
!
!      n_group,i :: INT
!
!      n_group = .n_group
!
!      .subgroup_pop.destroy
!      .subgroup_pop.create(n_group)
!
!      i = 0
!      .n_shared = .shared_population([(i,i=1,n_group)], .subgroup_pop)
!
!   end

!   shared_population(group_list,pop_groups) result (res) :: private
!   ! Returns the Roby shared population of the group of .atom_groups whose
!   ! indices are in "group". If "pop_groups" is present, the total
!   ! sub-group populations of every size from 1 to size(group) is returned
!      group_list :: VEC{INT}
!      pop_groups :: VEC{REAL}, optional
!      res :: REAL
!
!   ENSURE(.atom_group.allocated,"no atom groups")
!
!      pop_k :: REAL
!      m, k, n_k, n :: INT
!      comb_mat :: MAT{INT}@
!      pop_group :: VEC{REAL}@
!
!      n = size(group_list)
!
!      pop_group.create(n)
!
!      res = ZERO
!      do k = 1,n
!
!         n_k = int(n.choose(k))
!
!         ! Make "comb_mat", the matrix of all combinations of the groups
!         ! of size k, where the groups are taken from .atom_group
!         comb_mat.create(k,n_k)
!         group_list.make_combinations_of_length(k,comb_mat)
!
!         pop_k = 0
!         do m = 1,n_k
!            .atom_group = .append_listed_to(.atom_ab,list=comb_mat(:,m))
!            pop_k = pop_k + .population(.atom_ab)
!            .atom_ab.destroy
!         end
!         pop_group(k) = pop_k
!
!         comb_mat.destroy
!
!         res = res - ((-1)**k)*pop_k
!
!      end
!
!      if (present(pop_groups)) pop_groups = pop_group
!
!      pop_group.destroy
!
!   end

!   make_summed_pair_pops :: leaky, private
!   ! Evaluate the SUMMED Roby shared populations for groups of 2 atoms.
!   ! On return, .summed_n2(a) = \sum_{b} s_{ab}, where s_{ab} is the pairwise
!   ! shared population for atoms a, b.
!
!   ENSURE(.n1.allocated,"no group populations")
!   ENSURE(.n2.allocated,"no group pair populations")
!
!      shared_population :: REAL
!      n_group,a,b :: INT
!
!      n_group = .n_group
!      .summed_n2.create(n_group)
!
!      .summed_n2 = ZERO
!
!      do a = 1,n_group
!      do b = a+1,n_group
!         shared_population = .n1(a) + .n1(b) - .n2(a,b)
!         .summed_n2(a)     = .summed_n2(a) + shared_population
!         .summed_n2(b)     = .summed_n2(b) + shared_population
!      end
!      end
!
!   end

!   make_summed_triple_pops :: leaky, private
!   ! Evaluate the SUMMED Roby shared populations for groups of 3 atoms.
!   ! On return, .summed_n3(a) = \sum_{bc} s_{abc}, where s_{abc} is the triple
!   ! shared population for atoms a, b, and c.
!
!      shared_population :: REAL
!      n_group,a,b,c :: INT
!
!      n_group = .n_group
!      .summed_n3.create(n_group)
!
!      .summed_n3 = ZERO
!
!      do a = 1  ,n_group
!      do b = a+1,n_group
!      do c = b+1,n_group
!         shared_population = .shared_population([a,b,c])
!         .summed_n3(a)     = .summed_n3(a) + shared_population
!         .summed_n3(b)     = .summed_n3(b) + shared_population
!         .summed_n3(c)     = .summed_n3(c) + shared_population
!      end
!      end
!      end
!
!   end

! ==========
! Bond index
! ==========

   function gould_bond_index(self,c_ab,i_ab,pccp,pcca) result (bond_index)
   ! Calculates the Roby-Gould "bond_index", Pythagorean percentage
   ! covalent character "pcc", Araki percentage covalency "pcca", for
   ! atom groups ".atom_a" and ".atom_b".
      type(ROBY_TYPE), INOUT :: self
      REAL, OUT :: c_ab,i_ab,pccp,pcca
      REAL :: bond_index
      INT :: n0,i
   ENSURE(allocated(self%theta_pop_C),"ROBY:gould_bond_index ... no covalent theta populations")
   ENSURE(allocated(self%theta_pop_I),"ROBY:gould_bond_index ... no ionic theta populations")
   ENSURE(allocated(self%pair),"ROBY:gould_bond_index ... no pair array")
      n0 = size(self%eigen_I)
      call create_(self%theta_cov_index,n0)
      call create_(self%theta_ion_index,n0)
      self%theta_cov_index = ZERO
      self%theta_ion_index = ZERO
      pccp = ZERO
      pcca = ZERO
      c_ab = ZERO
      i_ab = ZERO
      bond_index = ZERO
      do i = 1,n0

         ! Skip lone pairs & skip antibonding 
         if (self%pair(i)<1) cycle  ! nonsense
         if (self%pair(i)==i) cycle
         if (self%eigen_I(i)<self%eigen_I(self%pair(i))) cycle
         if (self%pair(i)/=i) then
            self%theta_cov_index(i) = HALF*(self%theta_pop_C(i)-self%theta_pop_C(self%pair(i)))
            self%theta_ion_index(i) = HALF*(self%theta_pop_I(i)-self%theta_pop_I(self%pair(i)))
            c_ab = c_ab+self%theta_cov_index(i)
            i_ab = i_ab+self%theta_ion_index(i)
         else if (self%eigen_I(i)>ZERO) then

            ! Store bond index, do not add into i_ab
            self%theta_ion_index(i) = HALF*(self%theta_pop_I(i))
         else if (self%eigen_I(i)<ZERO) then

            ! Store bond index, do not add into i_ab
            self%theta_ion_index(i) = -HALF*(self%theta_pop_I(i))
         end if
      end do
      bond_index = c_ab**2+i_ab**2
      pccp = 100*(c_ab**2/bond_index)
      bond_index = sqrt(bond_index)
      pcca = 100*TWO*abs(asin(c_ab/bond_index))/PI
   end function


! ====================
! Angle-space analysis
! ====================

   subroutine make_theta_info(self)
   ! Calculates the Roby-Gould theta subspace information for atom groups
   ! ".atom_a" and ".atom_b", including theta subspace populations
      type(ROBY_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: I
      MAT(REAL,:,:), allocatable :: theta_I
      VEC(REAL,:), allocatable :: eigen_I
      VEC(INT,:), allocatable :: ind
      INT :: n_a,n_b,n_ab,n0
   ENSURE(allocated(self%atom_a),"ROBY:make_theta_info ... no atom A group")
   ENSURE(allocated(self%atom_b),"ROBY:make_theta_info ... no atom B group")
   ENSURE(allocated(self%atom_ab),"ROBY:make_theta_info ... no atom AB group")

      ! Clean up
      call destroy_theta_info_(self)

      ! Sizes
      n_a = n_bf_a_(self)
      n_b = n_bf_b_(self)
      n_ab = n_a+n_b

      ! Make the ionic operator & ionic states
      call create_(theta_I,n_ab,n_ab)
      call create_(eigen_I,n_ab)

      ! Make I
      call create_(I,n_ab,n_ab)
      call make_ionic_operator_(self,I)

      ! Diagonalize I
      call diagonalize_V_AB_operator_(self,I,theta_I,eigen_I)
      if (self%show_calcs) then
         call text_(stdout,"I:")
         call put_(stdout,I)
         call text_(stdout,"eigen_I:")
         call put_(stdout,eigen_I)
         call text_(stdout,"theta_I:")
         call put_(stdout,theta_I)
      end if
      call destroy_(I)

      ! Prune zero eigenvalues
      n0 = no_of_abs_vals_larger_than_(eigen_I,self%zero_angle_cutoff)
      if (show_calc_(self)) then
         call show_(stdout,"No of non-zero eigenvalues, n0 = ",n0)
      end if

      ! Get the indices of non-zero angles
      call create_(ind,n0)
      ind = indices_of_abs_vals_larger_than_(eigen_I,self%zero_angle_cutoff)
      self%theta_I = theta_I(:,ind)
      self%eigen_I = eigen_I(ind)
      if (show_calc_(self)) then
         call text_(stdout,"ind")
         call put_(stdout,ind)
      end if
      call destroy_(ind)
      call destroy_(eigen_I)
      call destroy_(theta_I)

      ! Find +/- ionic pairs
      call create_(self%pair,n0)
      call find_non_unit_pairs_(self%eigen_I,self%pair,match_pair,tol=TOL(4))

      ! Clean up |ion> states
      call make_gould_ionic_orbitals_(self)

      ! Make |cov> pairs from |ion> pairs
      call create_(self%theta_angle,n0)
      call create_(self%theta_C,n_ab,n0)
      call create_(self%eigen_C,n0)
      call make_cov_from_ion_orbitals_(self)
      if (show_calc_(self)) then
         call text_(stdout,"========")
         call text_(stdout,"atom_ab:")
         call put_(stdout,self%atom_ab)
         call text_(stdout,"pair:")
         call put_(stdout,self%pair)
         call show_(stdout,"n_bf_a = ",n_bf_a_(self))
         call show_(stdout,"n_bf_b = ",n_bf_b_(self))
         call text_(stdout,"theta_angle:")
         call put_(stdout,self%theta_angle)
         call text_(stdout,"eigen_I:")
         call put_(stdout,self%eigen_I)
         call text_(stdout,"eigen_C:")
         call put_(stdout,self%eigen_C)
         call text_(stdout,"theta_C:")
         call put_(stdout,self%theta_C)
         call text_(stdout,"theta_I:")
         call put_(stdout,self%theta_I)
      end if

      ! Covalent theta populations
      call create_(self%theta_pop_C,n0)
      call make_theta_populations_(self,self%theta_pop_C,self%theta_C)

      ! Ionic theta populations
      call create_(self%theta_pop_I,n0)
      call make_theta_populations_(self,self%theta_pop_I,self%theta_I)
   end subroutine

   subroutine make_hyper_info(self)
   ! Calculates the Roby-Gould theta subspace information for atom
   ! groups ".atom_a" and everything not in ".atom".
      type(ROBY_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: I
      MAT(REAL,:,:), allocatable :: theta_I
      VEC(REAL,:), allocatable :: eigen_I
      VEC(INT,:), allocatable :: ind
      INT :: n_bf,n0
   ENSURE(allocated(self%atom_a),"ROBY:make_hyper_info ... no atom A group")
   ENSURE(allocated(self%atom_b),"ROBY:make_hyper_info ... no atom B group")
   ENSURE(allocated(self%atom_ab),"ROBY:make_hyper_info ... no atom AB group")

      ! Clean up
      call destroy_theta_info_(self)

      ! Size
      n_bf = n_bf_(self)

      ! Make the ionic operator & ionic states
      call create_(theta_I,n_bf,n_bf)
      call create_(eigen_I,n_bf)
      call create_(I,n_bf,n_bf)
      call make_hyper_operator_(self,I)
      call diagonalize_full_operator_(self,I,theta_I,eigen_I)

   ! stdout.text("I")
   ! stdout.put(I)
      call destroy_(I)

   ! stdout.text("eigen_I")
   ! stdout.put(eigen_I)

      ! Prune zero eigenvalues
      n0 = no_of_abs_vals_larger_than_(eigen_I,TOL(8))

   ! stdout.show("n0 = ",n0)
      call create_(ind,n0)
      ind = indices_of_abs_vals_larger_than_(eigen_I,TOL(8))
      call create_(self%theta_I,n_bf,n0)
      call create_(self%eigen_I,n0)
      self%theta_I = theta_I(:,ind)
      self%eigen_I = eigen_I(ind)
      call destroy_(ind)
      call destroy_(eigen_I)
      call destroy_(theta_I)

   ! stdout.text(".theta_I")
   ! stdout.put(.theta_I)

   ! stdout.text(".eigen_I")
   ! stdout.put(.eigen_I)

      ! Find +/- ionic pairs
      call create_(self%pair,n0)
      call find_non_unit_pairs_(self%eigen_I,self%pair,match_pair,tol=TOL(8))

   ! stdout.text("pair")
   ! stdout.put(.pair)

      ! Make theta angles
   ! .make_theta_angles

   ! stdout.text("theta_angle")
   ! stdout.put(.theta_angle)

      ! Clean up |ion> states
      call make_gould_ionic_orbitals_(self)

   ! stdout.text(".theta_I")
   ! stdout.put(.theta_I)
   ! stdout.text(".eigen_I")
   ! stdout.put(.eigen_I)

      ! Make |cov> pairs from |ion> pairs
      call create_(self%theta_angle,n0)
      call create_(self%theta_C,n_bf,n0)
      call create_(self%eigen_C,n0)
      call make_cov_from_ion_orbitals_(self)

   ! stdout.text(".theta_C")
   ! stdout.put(.theta_C)
   ! stdout.text(".eigen_C")
   ! stdout.put(.eigen_C)

      ! Covalent theta populations
      call create_(self%theta_pop_C,n0)
   ! .theta_pop_C_A.create(n0)
   ! .theta_pop_C_B.create(n0)
      call make_theta_populations_(self,self%theta_pop_C,self%theta_C)

      ! Ionic theta populations
      call create_(self%theta_pop_I,n0)
   ! .theta_pop_I_A.create(n0)
   ! .theta_pop_I_B.create(n0)
      call make_theta_populations_(self,self%theta_pop_I,self%theta_I)
   end subroutine


! Low level helper routines

   function match_pair(arg1,arg2) result (res)
   ! Function which returns zero if "arg1" and "arg2" are opposite.
   ! Used for matching pairs purposes.
      REAL, IN :: arg1,arg2
      REAL :: res
      res = abs(arg1+arg2)
   end function

   subroutine make_theta_populations(self,pop,theta)
   ! Make "pop", an array of "theta" populations for a pair of Roby
   ! atoms defined by indices in ".atom_a" and ".atom_b". "theta"
   ! itself is a matrix whose columns are coefficients on V_AB.
   ! NOTE: not all these population will be used in making the
   ! bond indices, but why not calculate them all?
      type(ROBY_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: pop
      MAT(REAL,:,:), IN :: theta
      INT :: i
   ENSURE(allocated(self%atom_ab),"ROBY:make_theta_populations ... No roby atom group AB")
   ENSURE(size(pop)==size(theta,2),"ROBY:make_theta_populations ... wrong size, pop")
      do i = 1,size(theta,2)
         pop(i) = population_(self,self%atom_ab,ANO=theta(:,i:i))
      end do
   end subroutine


!   make_theta_angles :: private
!   ! Make the ".theta_angle" array from the Roby eigenvalues
!   ! ".eigen_I". Angles are in degrees.
!   ! WARNING: some crazy mistake here !!!! <<<<<<
!      self :: INOUT
!
!   ENSURE(.eigen_I.allocated,"No roby eigenvalues")
!   ENSURE(.theta_angle.allocated,"No theta_angle array")
!
!      i :: INT
!      value :: REAL
!
!      do i = 1,.eigen_I.dim
!
!         value = .eigen_I(i)
!         value = value.arcsin
!         .theta_angle(i) = value*DEGREE_PER_RADIAN
!
!      end
!
!   end

   subroutine make_gould_ionic_orbitals(self)
   ! Generate the gould ionic orbitals ".theta_I" in such a way that
   ! the negative eigenvector of each pair is generated explicitly
   ! from the positive eigenvector by constructing the |a> and |b>
   ! parts using projectors P_A and P_B. ".eigen_I" are the
   ! eigenvalues corresponding to ".theta_I". ".pair" is defined so
   ! that the i-th eigenvector .theta_I(:,i) with +ve value is paired
   ! with theta_I(:,pair(i)) with negative eigenvalue. The spaces V_A,
   ! V_B and V_AB are defined by ".atom_a", ".atom_b", and ".atom_ab".
   ! This routine is necessary in cases where there are degeneracies.
      type(ROBY_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: PAS,PBS,PA,PB
      VEC(REAL,:), allocatable :: A,B
      INT :: n_a,n_b,n_ab,i
      REAL :: fp,fm,fa,fb,s,c,s2
   ENSURE(allocated(self%atom_a),"ROBY:make_gould_ionic_orbitals ... No roby atom A")
   ENSURE(allocated(self%atom_b),"ROBY:make_gould_ionic_orbitals ... No roby atom B")
   ENSURE(allocated(self%atom_ab),"ROBY:make_gould_ionic_orbitals ... No roby atom AB")
   ENSURE(allocated(self%atom),"ROBY:make_gould_ionic_orbitals ... No atom information")
   ENSURE(allocated(self%theta_I),"ROBY:make_gould_ionic_orbitals ... No theta_I array")
   ENSURE(size(self%theta_I,1)==n_bf_ab_(self),"ROBY:make_gould_ionic_orbitals ... wrong shape, theta_I")
   ENSURE(allocated(self%eigen_I),"ROBY:make_gould_ionic_orbitals ... No eval_I array")
   ENSURE(size(self%eigen_I)==size(self%theta_I,2),"ROBY:make_gould_ionic_orbitals ... eigen_I & theta_I, inconsistent")
   ENSURE(allocated(self%pair),"ROBY:make_gould_ionic_orbitals ... No pair array")
   ENSURE(size(self%pair)==size(self%theta_I,2),"ROBY:make_gould_ionic_orbitals ... pair & theta_I, inconsistent")
      n_a = n_bf_a_(self)
      n_b = n_bf_b_(self)
      n_ab = n_bf_ab_(self)
      call create_(PAS,n_a,n_ab)
      call create_(PBS,n_b,n_ab)
      call create_(A,n_a)
      call create_(B,n_b)
      call create_(PA,n_a,n_a)
      call create_(PB,n_b,n_b)
      call make_projection_mx_(self,PA,self%atom_a)
      call make_projection_mx_(self,PB,self%atom_b)
      call right_overlap_transform_(self,PA,PAS,self%atom_a,self%atom_ab)
      call right_overlap_transform_(self,PB,PBS,self%atom_b,self%atom_ab)
      call destroy_(PB)
      call destroy_(PA)
      do i = 1,size(self%eigen_I)
         if (self%pair(i)<1) cycle
         if (self%pair(i)==i) cycle
         if (self%eigen_I(i)<self%eigen_I(self%pair(i))) cycle

         ! Bonding state (.eigen_I +ve)
         s = self%eigen_I(i)
         s2 = s*s
         if (equals_(s2,ONE,TOL(8))) then; c = ZERO
         else; c = sqrt(ONE-s2)
         end if

         ! Find |a> and |b> from +ve bonding state
         fm = sqrt(ONE-c)/s
         fp = sqrt(ONE+c)/s
         fa = HALF*((fm+fp)+c*(fm-fp))
         fb = HALF*(c*(fm+fp)+(fm-fp))
         A = ZERO
         B = ZERO
         if (NOT equals_(fa,ONE,TOL(8))) then
            call to_product_of_(A,PAS,self%theta_I(:,i)); A = A/fa
         end if
         if (NOT equals_(fb,ONE,TOL(8))) then
            call to_product_of_(B,PBS,self%theta_I(:,i)); B = B/fb
         end if

         ! Construct -ve antibonding state in .pair(i)
         fa = HALF*(fm-fp)
         fb = HALF*(fm+fp)
         self%theta_I(1:n_a,self%pair(i)) = fa*A
         self%theta_I(n_a+1:n_ab,self%pair(i)) = fb*B
      end do

      ! Clean
      call destroy_(B)
      call destroy_(A)
      call destroy_(PBS)
      call destroy_(PAS)
   end subroutine

   subroutine make_cov_from_ion_orbitals(self)
   ! Make ".theta_C" and ".eigen_C" from ".theta_I" and ".eigen_I".
      type(ROBY_TYPE), INOUT :: self
      REAL :: fac,c,s
      INT :: i,j
   ENSURE(allocated(self%eigen_I),"ROBY:make_cov_from_ion_orbitals ... no eval_I array")
   ENSURE(allocated(self%eigen_C),"ROBY:make_cov_from_ion_orbitals ... no eigen_C array")
   ENSURE(size(self%eigen_C)==size(self%eigen_I),"ROBY:make_cov_from_ion_orbitals ... inconsistent eigen_C & eigen_I arrays")
   ENSURE(allocated(self%theta_I),"ROBY:make_cov_from_ion_orbitals ... no theta_I array")
   ENSURE(size(self%theta_I,1)==n_bf_ab_(self),"ROBY:make_cov_from_ion_orbitals ... wrong dim1, theta_I")
   ENSURE(size(self%theta_I,2)==size(self%eigen_C),"ROBY:make_cov_from_ion_orbitals ... wrong shape, theta_I")
   ENSURE(allocated(self%theta_C),"ROBY:make_cov_from_ion_orbitals ... no theta_C array")
   ENSURE(size(self%theta_C,1)==n_bf_ab_(self),"ROBY:make_cov_from_ion_orbitals ... wrong dim1, theta_C")
   ENSURE(size(self%theta_C,2)==size(self%eigen_C),"ROBY:make_cov_from_ion_orbitals ... wrong shape, theta_I")
   ENSURE(allocated(self%pair),"ROBY:make_cov_from_ion_orbitals ... no pair array")
   ENSURE(size(self%pair)==size(self%eigen_C),"ROBY:make_cov_from_ion_orbitals ... wrong shape, pair")
   !  eval :: MAT{REAL}(1,1)
   !  RR,SS :: MAT{REAL}@

   !  n_ab = .n_bf_ab

   !  RR.create(n_ab,n_ab)
   !  SS.create(n_ab,n_ab)

   !  stdout.text("1")
   !  .make_shared_operator(SS)
   !  stdout.text("2")
   !  .overlap_transform(SS,RR,.atom_ab,.atom_ab)
   !  stdout.text("3")

   !  SS.destroy
      self%theta_angle = 90.0d0
      self%theta_C = ZERO
      self%eigen_C = ZERO
      do i = 1,size(self%eigen_I)
         if (self%pair(i)<1) cycle
         if (self%pair(i)==i) cycle
         if (self%eigen_I(i)<self%eigen_I(self%pair(i))) cycle

         ! Index j of -ve antibonding orbital
         j = self%pair(i)

         ! pi/4 rotation: eqn (64) in Gould et al (2008)
         fac = ONE/sqrt(TWO)
         self%theta_C(:,i) = fac*(self%theta_I(:,i)+self%theta_I(:,j))  ! C+
         self%theta_C(:,j) = fac*(self%theta_I(:,i)-self%theta_I(:,j))  ! C-

         ! Diagonalize I
      ! RR.change_basis_to(eval,.theta_C(:,i:i)); .eigen_C(i) = eval(1,1)
      ! RR.change_basis_to(eval,.theta_C(:,j:j)); .eigen_C(j) = eval(1,1)

         ! Set C eigenvalues
         s = self%eigen_I(i)
         c = sqrt(ONE-s*s)
         self%eigen_C(i) = c
         self%eigen_C(j) = -c

         ! Set angle
         self%theta_angle(i) = atan2(s,c)*DEGREE_PER_RADIAN
      end do

   ! RR.destroy
   end subroutine

   pure function group_nuclear_charges(self) result (res)
   ! Sum of the nuclear charges in each group.
      type(ROBY_TYPE), IN :: self
      VEC(REAL,size(self%atom_group)) :: res
      INT :: g
      do g = 1,size(self%atom_group)
         res(g) = sum_of_nuclear_charges_(self%atom(self%atom_group(g)%element))-sum_of_core_electrons_(self%atom(self%atom_group(g)%element))
      end do
   end function


!   make_theta_E1_Krogel_info :: leaky, private
!   ! Calculates the Roby-Gould theta subspace information for atom
!   ! groups ".atom_a" and ".atom_b", including theta subspace
!   ! energies and subenergies via the Krogel approach.
!      self :: INOUT
!
!   ENSURE(.atom_a.allocated,"No roby atom A")
!   ENSURE(.atom_b.allocated,"No roby atom B")
!   ENSURE(.atom_ab.allocated,"No roby atom AB")
!
!      I :: MAT{REAL}@
!      theta_I :: MAT{REAL}@
!      eigen_I :: VEC{REAL}@
!      ind :: VEC{INT}@
!      n_a,n_b,n_ab, n0 :: INT
!
!      n_a  = .n_bf_a
!      n_b  = .n_bf_b
!      n_ab = n_a + n_b
!
!      ! Make the ionic operator & ionic states
!      theta_I.create(n_ab,n_ab)
!      eigen_I.create(n_ab)
!
!      ! Make I
!      I.create(n_ab,n_ab)
!      .make_ionic_operator(I)
!
!      ! Diagonalize I
!      .diagonalize_V_AB_operator(I,theta_I,eigen_I)
!
!      if (.show_calcs) then
!         stdout.text("I:")
!         stdout.put(I)
!         stdout.text("eigen_I:")
!         stdout.put(eigen_I)
!         stdout.text("theta_I:")
!         stdout.put(theta_I)
!      end
!
!      I.destroy
!
!      ! Prune zero eigenvalues
!      n0 = eigen_I.no_of_abs_vals_larger_than(.zero_angle_cutoff)
!
!      if (.show_calcs) then
!         stdout.show("No of non-zero eigenvalues, n0 = ",n0)
!      end
!
!      ind.create(n0)
!      ind = eigen_I.indices_of_abs_vals_larger_than(.zero_angle_cutoff)
!
!      if (.show_calcs) then
!         stdout.text("ind")
!         stdout.put(ind)
!      end
!
!      .theta_I.create(n_ab,n0)
!      .eigen_I.create(n0)
!
!      .theta_I = theta_I(:,ind)
!      .eigen_I = eigen_I(ind)
!
!      ind.destroy
!      eigen_I.destroy
!      theta_I.destroy
!
!      ! Find +/- ionic pairs
!      .pair.create(n0)
!      .eigen_I.find_non_unit_pairs(.pair,ROBY::match_pair,tol=TOL(5))
!
!      ! Make theta angles
!    ! .make_theta_angles
!
!    ! if (.show_calcs) then
!         stdout.text("pair array:")
!         stdout.put(.pair)
!         stdout.text("eigen_I:")
!         stdout.put(.eigen_I)
!    ! end
!
!      ! Clean up |ion> states
!      .make_gould_ionic_orbitals
!
!      ! Make |cov> pairs from |ion> pairs
!      .theta_angle.create(n0)
!      .theta_C.create(n_ab,n0)
!      .eigen_C.create(n0)
!      .make_cov_from_ion_orbitals
!
!      if (.show_calcs) then
!         stdout.show("n_bf_a = ",.n_bf_a)
!         stdout.show("n_bf_b = ",.n_bf_b)
!         stdout.text("eigen_C:")
!         stdout.put(.eigen_C)
!         stdout.text("theta_C:")
!         stdout.put(.theta_C)
!         stdout.text("theta_I:")
!         stdout.put(.theta_I)
!      end
!
!      ! Covalent theta populations
!      .theta_E_C.create(n0)
!      .theta_E_C_A.create(n0)
!      .theta_E_C_B.create(n0)
!      .make_theta_E1_Krogel_Es(.theta_E_C,.theta_E_C_A,.theta_E_C_B,.theta_C)
!
!      ! Ionic theta populations
!      .theta_E_I.create(n0)
!      .theta_E_I_A.create(n0)
!      .theta_E_I_B.create(n0)
!      .make_theta_E1_Krogel_Es(.theta_E_I,.theta_E_I_A,.theta_E_I_B,.theta_I)
!
!   end
!
!   make_theta_E1_Krogel_Es(E,Ea,Eb,theta) :: private
!   ! Make "pop", an array of "theta" populations for a pair of Roby
!   ! atoms defined by indices in ".atom_a" and ".atom_b". "theta"
!   ! itself is a matrix whose columns are orbital coefficients on
!   ! V_AB corresponding to an ionic or covalent or some other state.
!      self :: INOUT
!      E,Ea,Eb :: VEC{REAL}, OUT
!      theta :: MAT{REAL}, IN
!
!   ENSURE(.atom_ab.allocated,"No roby atom AB")
!   ENSURE(.E_density_mx.allocated,"No Krogel E matrix")
!   ENSURE(E.dim==theta.dim2,"wrong size, pop")
!
!      i :: INT
!
!      do i = 1,theta.dim2
!          E(i)  =    .population(       .atom_ab,ANO=theta(:,i:i),rho=.E_density_mx)
!         Ea(i) = .subpopulation(.atom_a,.atom_ab,ANO=theta(:,i:i),rho=.E_density_mx)
!         Eb(i) = .subpopulation(.atom_b,.atom_ab,ANO=theta(:,i:i),rho=.E_density_mx)
!      end
!
!   end

! =================
! Energy bond index
! =================

!   atom_E1_Krogel_analysis :: leaky
!   ! Do a E^1_Krogel energy-bond analysis.
!   ! Only works for RHF case for now.
!      self :: INOUT
!
!   ENSURE(.core_mx.allocated,"no core_mx!")
!   ENSURE(.core_mx.r.allocated,"no restricted core_mx!")
!   ENSURE(.fock_mx.allocated,"no fock_mx!")
!   ENSURE(.fock_mx.r.allocated,"no restricted fock_mx!")
!
!      i,n_atom :: INT
!      n_group,a,b :: INT
!
!      ! Make atom list if it doesn't exist
!      if (.atom_list.deallocated) then
!         n_atom = .atom.dim
!         .atom_list = [ (i, i=1,n_atom) ]
!      end
!
!      ! Each atom is a Roby subspace
!      .make_single_atom_groups
!
!      ! Make E_density_mx, E_scf, E_non_atomic
!      .make_E1_Krogel_mx 
!
!      ! Each atom is a Roby subspace
!      .make_single_atom_groups
!      n_group = .n_group
!
!      ! Get group energies
!      .make_E1_Krogel_A
!      .make_E1_Krogel_AB
!
!      ! Clean
!      .destroy_bond_info
!      .E1_cov.create(n_group,n_group)
!      .E1_ion.create(n_group,n_group)
!      .E1_bond.create(n_group,n_group)
!
!      ! File name & redirect
!      ! Get Roby subspace analysis
!      do a = 1,.n_group
!      do b = a+1,.n_group
!
!         if (.skip_pair(a,b)) cycle
!
!         .atom_a = .atom_group(a).element
!         .atom_b = .atom_group(b).element
!
!         .atom_ab = [.atom_a, .atom_b ]
!
!         ! Make theta-subspace energies
!         .destroy_theta_info
!         .make_theta_E1_Krogel_info
!
!         if (.output_theta_info) then
!            .put_theta_info
!            .put_theta_E_info
!         end
!
!
!      end
!      end
!
!      ! Output
!      .put_basics
!      .put_E_As
!      .put_E_ABs
!
!   end
!
!   make_E1_Krogel_mx :: leaky
!   ! Make the E^1_Krogel matrix which substitutes for the density
!   ! matrix .rho in the RGBI population code.
!   ! Only works for UHF/RHF cases for now.
!      self :: INOUT
!
!   ENSURE(.core_mx.allocated,"no core_mx!")
!   ENSURE(.core_mx.r.allocated,"no restricted core_mx!")
!   ENSURE(.fock_mx.allocated,"no fock_mx!")
!   ENSURE(.fock_mx.r.allocated,"no restricted fock_mx!")
!
!      tmp :: OPMATRIX@
!      Sm1 :: MAT{REAL}@
!      all_atoms :: VEC{INT}@
!      i,n_bf :: INT
!       
!      ! Make E_density_mx without S^-1 prefactor
!      .E_density_mx = .fock_mx
!      .E_density_mx.plus(.core_mx)
!      .E_density_mx.scale_by(HALF)
!
!      ! SCF energy
!      .E_scf = .E_density_mx.expectation(.density_mx)
!
!      ! Make S^-1 
!      n_bf = .core_mx.n_bf
!      Sm1.create(n_bf,n_bf) ! NOTE: RHF/UHF cases here only
!      Sm1.to_pseudo_inverse_of(.overlap_mx,TOL(5))
!
!      ! Do S^-1 . 1/2 [ h + F ] . D
!      tmp = .E_density_mx
!      tmp.to_product_of(Sm1,.E_density_mx)
!      .E_density_mx.to_product_of(X=tmp,Y=.density_mx)
!
!      ! Non-atomic energy, E_{na}
!      all_atoms = [ (i, i=1,.atom.dim) ]
!      .E_atomic     = .population(all_atoms,rho=.E_density_mx)
!      .E_non_atomic = .E_scf - .E_atomic
!
!      ! Clean
!      all_atoms.destroy
!      tmp.destroy
!      Sm1.destroy
!
!   end
!
!   make_E1_Krogel_A :: leaky, private
!   ! Make the Krogel atom energies for the defined atom groups.
!      self :: INOUT
!
!   ENSURE(.atom_group.allocated,"no atom groups")
!   ENSURE(.density_mx.allocated,"no density matrix")
!   ENSURE(.E_density_mx.allocated,"no density matrix")
!
!      a :: INT
!
!      .E1.destroy
!      .E1.create(.n_group)
!
!      .n1.destroy
!      .n1.create(.n_group)
!
!      do a = 1,.n_group
!
!         .atom_a = .atom_group(a).element
!
!         .n1(a) =  .population(.atom_a)
!         .E1(a) =  .population(.atom_a,rho=.E_density_mx)
!
!      end
!
!   end
!
!   make_E1_Krogel_AB :: leaky, private
!   ! Make the Krogel atom pair energies for the defined atom groups.
!      self :: INOUT
!
!   ENSURE(.atom_group.allocated,"no atom groups")
!   ENSURE(.density_mx.allocated,"no density matrix")
!   ENSURE(.E_density_mx.allocated,"no density matrix")
!
!      n_group,a,b :: INT
!      val :: REAL
!
!      n_group = .n_group
!
!      .n2.destroy
!      .n2.create(n_group,n_group)
!      .n2 = ZERO
!
!      .E2.destroy
!      .E2.create(n_group,n_group)
!      .E2 = ZERO
!
!      do a = 1,n_group
!      do b = a+1,n_group
!
!         if (.skip_pair(a,b)) cycle
!
!         .atom_a = .atom_group(a).element
!         .atom_b = .atom_group(b).element
!
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         .atom_ab = [.atom_a, .atom_b]
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!         val = .population(.atom_ab)
!        
!         .n2(a,b) = val
!         .n2(b,a) = val
!
!          val = .population(.atom_ab,rho=.E_density_mx)
!
!         .E2(a,b) = val
!         .E2(b,a) = val
!
!      end
!      end
!
!   end
!
!
!   atom_E1_exact_analysis :: leaky
!   ! Do a Roby energy-bond and Gould charge analysis.
!   ! Only works for RHF case for now.
!      self :: INOUT
!
!   ENSURE(.core_mx.allocated,"no core_mx!")
!   ENSURE(.core_mx.r.allocated,"no restricted core_mx!")
!   ENSURE(.fock_mx.allocated,"no fock_mx!")
!   ENSURE(.fock_mx.r.allocated,"no restricted fock_mx!")
!
!      i,n_atom :: INT
!      n_group,a,b :: INT
!      ind,cov,ion,pccp,pcca :: REAL
!       
!
!      ! Make atom list if it doesn't exist
!      if (.atom_list.deallocated) then
!         n_atom = .atom.dim
!         .atom_list = [ (i, i=1,n_atom) ]
!      end
!
!      ! Each atom is a Roby subspace
!      .make_single_atom_groups
!
!      ! Make unprojected E_density_mx
!      .E_density_mx = .fock_mx
!      .E_density_mx.plus(.core_mx)
!      .E_density_mx.scale_by(HALF)
!
!      ! Print out SCF energy
!      .E_scf = .E_density_mx.r.trace_product_with(.density_mx.r)
!      stdout.show("E_scf = ",.E_scf)
!
!      ! Each atom is a Roby subspace
!      .make_single_atom_groups
!      n_group = .n_group
!
!      ! Get group energies
!      .make_E_exact_A
!      .make_E_exact_AB
!
!      ! Clean
!      .destroy_bond_info
!      .bnd_index.create(n_group,n_group)
!      .cov_index.create(n_group,n_group)
!      .ion_index.create(n_group,n_group)
!      .percent_covalency_P.create(n_group,n_group)
!      .percent_covalency_A.create(n_group,n_group)
!
!      ! File name & redirect
!      ! Get Roby subspace analysis
!      do a = 1,.n_group
!      do b = a+1,.n_group
!
!         if (.skip_pair(a,b)) cycle
!
!         .atom_a = .atom_group(a).element
!         .atom_b = .atom_group(b).element
!
!         .atom_ab = [.atom_a, .atom_b]
!
!       ! .make_theta_energy_info
!         .make_theta_info
!         
!         ind = .gould_bond_index(cov,ion,pccp,pcca)
!
!         .bnd_index(a,b)           =  ind
!         .cov_index(a,b)           =  cov
!         .ion_index(a,b)           =  ion
!         .percent_covalency_P(a,b) =  pccp
!         .percent_covalency_A(a,b) =  pcca
!
!         .bnd_index(b,a)           =  ind
!         .cov_index(b,a)           =  cov
!         .ion_index(b,a)           = -ion
!         .percent_covalency_P(b,a) =  pccp
!         .percent_covalency_A(b,a) =  pcca
!
!         if (.output_theta_info) then
!            .put_theta_info
!            .put_theta_BI_info
!         end
!
!      end
!      end
!
!      ! Output
!      .put_basics
!      .put_E_As
!      .put_E_ABs
!
!   end
!
!   make_E_exact_A :: leaky, private
!   ! Make the Roby energies for the defined atom groups
!      self :: INOUT
!
!   ENSURE(.atom_group.allocated,"no atom groups")
!   ENSURE(.density_mx.allocated,"no density matrix")
!   ENSURE(.E_density_mx.allocated,"no density matrix")
!
!      a :: INT
!      fac :: REAL
!
!      fac = .E_scf/.atom.no_of_electrons
!
!      .E1.destroy
!      .E1.create(.n_group)
!
!      .n1.destroy
!      .n1.create(.n_group)
!
!      do a = 1,.n_group
!
!         .atom_a = .atom_group(a).element
!
!         .n1(a) =  .population(.atom_a)
!         .E1(a) =  .n1(a) * fac
!
!      end
!
!   end
!
!   make_E_exact_AB :: leaky, private
!   ! Make the pair Roby energies
!      self :: INOUT
!
!   ENSURE(.atom_group.allocated,"no atom groups")
!   ENSURE(.density_mx.allocated,"no density matrix")
!   ENSURE(.E_density_mx.allocated,"no density matrix")
!
!      n_group,a,b :: INT
!      fac :: REAL
!      pop :: REAL
!
!      fac = .E_scf/.atom.no_of_electrons
!      n_group = .n_group
!
!      .n2.destroy
!      .n2.create(n_group,n_group)
!      .n2 = ZERO
!
!      .E2.destroy
!      .E2.create(n_group,n_group)
!      .E2 = ZERO
!
!      do a = 1,n_group
!      do b = a+1,n_group
!
!         if (.skip_pair(a,b)) cycle
!
!         .atom_a = .atom_group(a).element
!         .atom_b = .atom_group(b).element
!
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         .atom_ab = [.atom_a, .atom_b]
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!         pop = .population(.atom_ab)
!        
!         .n2(a,b) = pop
!         .n2(b,a) = pop 
!
!         .E2(a,b) = pop * fac
!         .E2(b,a) = pop * fac
!
!      end
!      end
!
!   end
!   
!   put_E_As
!   ! Put to stdout the only Roby atom energies
!      self :: IN
!
!   ENSURE(.E1.allocated,"no atom energies exist")
!   ENSURE(.atom.allocated,"no atom data")
!
!      table :: VEC{TABLE_COLUMN}@
!      symbols :: VEC{STR}@
!      charge :: VEC{REAL}@
!      labels :: VEC{INT}@
!      n_e,f :: INT
!
!      ! Redo decimal places
!      stdout.save_style
!      stdout.set_real_precision(2)
!
!      stdout.flush
!      stdout.text("==================")
!      stdout.text("Roby atom energies")
!      stdout.text("==================")
!
!      n_e = .atom.no_of_electrons
!
!      stdout.flush
!      stdout.show("No. of electrons in mol =",n_e)
!      stdout.show("No. of electrons in P_M =",.n_all)
!      stdout.show("No. of hypervalent e's  =",n_e-.n_all)
!      stdout.flush
!
!      ! Create table
!      table.create(3)
!
!      ! Set the table headings
!      table(1).set_heading("Atom")
!      table(1).set_width_from(.atom.chemical_symbols)
!      if (.is_homoleptic) then
!         symbols.create(.n_group)
!         labels.create(.n_group)
!         f = 1
!         labels.to_sequence(f,.n_group)
!         symbols = labels.to_str
!         labels.destroy
!      else
!         symbols = .unique_tags
!      end
!      table(1).set_values(symbols)
!
!      table(2).set_heading("Projected energies")
!      table(2).set_subhead("of atoms")
!      table(2).set_values(.E1)
!
!      charge = -.n1 + .group_nuclear_charges
!      table(3).set_heading("Charge")
!      table(3).set_values(charge)
!
!      ! Put table
!      table.put(label_rows=FALSE)
!
!      ! Clean
!      table.clear_columns
!      charge.destroy
!      symbols.destroy
!
!      ! Unsave
!      stdout.unsave_style
!
!   end
!
!   put_E_ABs
!   ! Output pair and shared bond exact bond energies
!      self :: IN
!
!   ENSURE(NOT .is_homoleptic,"job is homoleptic")
!
!      table :: VEC{TABLE_COLUMN}@
!      Z_list :: VEC{EVEC{INT}}@
!      no_of_bonds, n_kind,k1,k2,n_bonds, i :: INT
!      pair :: MAT{INT}@
!      symbols,labels :: MAT{STR}@
!      dash,dsh1 :: VEC{STR}@
!      dist,sAB,E_A,E_B,EAB :: VEC{REAL}@
!      has_unique_labels :: BIN
!
!      ! Any atoms to do?
!      if (.atom.dim<2) return
!
!      ! Any bonded atoms to do?
!      no_of_bonds = .atom.no_of_bonds(.bond_range_factor)
!      if (no_of_bonds<1) return
!
!      ! Has unique labels?
!      has_unique_labels = .atom.has_unique_labels
!      DIE_IF(NOT has_unique_labels,"atoms are not uniquely labelled!")
!
!      ! Redo decimal places
!      stdout.save_style
!      stdout.set_real_precision(4)
!
!      ! Title
!      if (.analyze_vdw_atom_pairs) then
!      stdout.flush
!      stdout.text("==============================================")
!      stdout.text("Roby-Gould bond energies: VDW interactions only")
!      stdout.text("==============================================")
!      else
!      stdout.flush
!      stdout.text("=======================")
!      stdout.text("Roby-Gould bond energies")
!      stdout.text("=======================")
!      end
!
!      stdout.flush
!      if (.analyze_vdw_atom_pairs) then
!      stdout.text("Indices appear only for VDW bonded atom pairs and")
!      stdout.text("NOT normally bonded by CSD criterion")
!      stdout.flush
!      stdout.show("No. of 'bonds'          =",no_of_bonds)
!      else if (.analyze_all_atom_pairs) then
!      stdout.text("Indices appear only for atom pairs which are connected")
!      stdout.text("according to the Cambridge Structural Database (CSD)")
!      stdout.text("AND if they are within a minimum bond_range_factor")
!      stdout.flush
!      stdout.show("Bond range factor /Angs =",.bond_range_factor.to_units("angstrom"))
!      stdout.show("No. of 'bonds'          =",no_of_bonds)
!      else
!      stdout.text("Indices appear only for atom pairs which are connected")
!      stdout.text("according to the Cambridge Structural Database (CSD).")
!      stdout.flush
!      stdout.show("No. of bonds            =",no_of_bonds)
!      end
!      stdout.flush
!
!      stdout.show("E_scf                   =",.E_scf)
!      stdout.show("E_non_atomic            =",.E_non_atomic)
!      stdout.flush
!
!      ! Get atom Z list
!      .atom.make_atom_Z_list(Z_list)
!
!      ! Skip dummy atoms
!      n_kind = Z_list.dim
!      if (.atom(Z_list(n_kind)[1]).atomic_number==0) n_kind = n_kind - 1
!
!      ! Create table
!      table.create(11)
!
!      ! Set the table headings
!      table( 1).set_heading("A")
!      table( 1).set_width_from(.atom.chemical_symbols)
!
!      table( 2).set_column_spacing(0)
!      table( 2).set_heading("--")
!      table( 2).set_width(2)
!
!      table( 3).set_heading("B")
!      table( 3).set_width(table(1).width)
!      table( 3).set_left_justify(TRUE)
!
!      table( 4).set_heading("A")
!      table( 4).set_width_from(.atom(:).tag)
!
!      table( 5).set_column_spacing(0)
!      table( 5).set_heading("--")
!      table( 5).set_width(2)
!
!      table( 6).set_heading("B")
!      table( 6).set_width(table(4).width)
!      table( 6).set_left_justify(TRUE)
!
!      table( 7).set_heading("R(A--B)")
!      table( 7).set_subheading("/Angs.")
!
!      table( 8).set_heading("E_A")
!      table( 9).set_heading("E_B")
!      table(10).set_heading("E_AB")
!      table(11).set_heading("   E_AB")
!      table(11).set_subhead("(shared)")
!
!      ! Put out the headings
!      table.put_headings(label_rows=FALSE,no_flush=TRUE)
!
!      ! Loop over atom kind classes
!      do k1 = 1,n_kind
!      do k2 = k1,n_kind
!
!         ! Get the bonded atoms per kind
!         if (.analyze_vdw_atom_pairs) then
!         .atom.get_vdw_bonded_atoms(pair,dist,Z_list,k1,k2,.bond_range_factor)
!         else
!         .atom.get_bonded_atoms(pair,dist,Z_list,k1,k2,.bond_range_factor)
!         end
!         dist = dist*ANGSTROM_PER_BOHR
!
!         ! No of bonds in this class
!         n_bonds = dist.dim
!         if (n_bonds==0) then
!            dist.destroy
!            pair.destroy
!            cycle
!         end
!
!         ! Element symbols
!         symbols.create(n_bonds,2)
!         symbols = " "
!         symbols(1,1) = .atom( Z_list(k1)[1]).chemical_symbol
!         symbols(1,2) = .atom(Z_list(k2)[1]).chemical_symbol
!
!         ! Dashes
!         dash.create(n_bonds)
!         dsh1.create(n_bonds)
!         dash    = "--"
!         dsh1    = "  "
!         dsh1(1) = "--"
!
!         ! Labels
!         labels.create(n_bonds,2)
!         labels(:,1) = .atom(pair(:,1)).tag
!         labels(:,2) = .atom(pair(:,2)).tag
!
!         ! Populations & indices
!         E_A.create(n_bonds)
!         E_B.create(n_bonds)
!         EAB.create(n_bonds)
!         sAB.create(n_bonds)
!
!         E_A = .E1(pair(:,1))
!         E_B = .E1(pair(:,2))
!         forall (i=1:n_bonds)
!         EAB(i) = .E2(pair(i,1),pair(i,2))
!         sAB(i) = E_A(i) + E_B(i) - EAB(i)
!         end
!
!         ! Set table data
!         table( 1).set_values(symbols(:,1))
!         table( 2).set_values(dsh1)
!         table( 3).set_values(symbols(:,2))
!         table( 4).set_values(labels(:,1))
!         table( 5).set_values(dash)
!         table( 6).set_values(labels(:,2))
!         table( 7).set_values(dist)
!         table( 8).set_values(E_A)
!         table( 9).set_values(E_B)
!         table(10).set_values(EAB)
!         table(11).set_values(sAB)
!
!         ! Put body
!         stdout.flush
!         table.put_body(label_rows=FALSE)
!
!         ! Clean
!         table.clear_columns
!
!         sAB.destroy
!         E_A.destroy
!         E_B.destroy
!         EAB.destroy
!         labels.destroy
!         dsh1.destroy
!         dash.destroy
!         symbols.destroy
!
!         dist.destroy
!         pair.destroy
!
!      end
!      end
!
!      ! Table footer
!      table.put_footer(label_rows=FALSE)
!
!      ! Clean
!      table.destroy
!      Z_list.destroy
!
!      ! Unsave settings
!      stdout.unsave_style
!
!   end   
!
!
!   put_theta_E_info
!   ! Output theta-space bond index information
!      self :: IN
!
!      n,i,j,k,p :: INT
!      par :: VEC{STR}@
!      ind :: VEC{INT}@
!      ect_p,ect_m :: VEC{REAL}@
!      eit_p,eit_m :: VEC{REAL}@
!    ! eca_p,eca_m,ecb_p,ecb_m :: VEC{REAL}@
!    ! eia_p,eia_m,eib_p,eib_m :: VEC{REAL}@
!      ang :: VEC{REAL}@
!      table :: VEC{TABLE_COLUMN}@
!
!      ! Columns
!      n = .eigen_I.dim
!      par.create(n)
!      ang.create(n)
!      ect_p.create(n)
!      ect_m.create(n)
!      eit_p.create(n)
!      eit_m.create(n)
!
!      ! Sort decreasing covalent index
!      ind.create(n)
!      .theta_E_C.quick_sort(ind)
!
!      ! Set column data
!      j = 0
!      do k = 1,n
!
!         ! Reordered index and its pair
!         i = ind(k)
!         p = .pair(i)
!
!         if (p<1) cycle
!         if (.eigen_I(i)<.eigen_I(p)) cycle
!
!         j = j + 1
!
!         if (p/=i) then
!               par(j) = "("//i.to_str.trim//","//p.to_str.trim//")"
!               eit_p(j) = .theta_E_I(i)
!               eit_m(j) = .theta_E_I(p)
!         else
!               par(j) = "("//i.to_str.trim//")"
!            if      (.eigen_I(i)>ZERO) then
!               eit_p(j) = .theta_E_I(i)
!               eit_m(j) = ZERO
!            else if (.eigen_I(i)<ZERO) then
!               eit_p(j) = ZERO
!               eit_m(j) = .theta_E_I(i)
!            end
!         end
!
!         ect_p(j) = .theta_E_C(i)
!         ect_m(j) = .theta_E_C(p)
!         ang(j) = atan2(.eigen_I(i),.eigen_C(i))*DEGREE_PER_RADIAN
!
!      end
!
!      n = j
!
!      ! Change decimal places
!      stdout.save_style
!      stdout.set_real_precision(3)
!
!      stdout.flush
!      stdout.flush
!      stdout.text("========================================")
!      stdout.text("Covalent/ionic bond indices per subspace")
!      stdout.text("========================================")
!      stdout.flush
!      stdout.text(". +/- means: bonding /antibonding RG bond orbitals.")
!      stdout.text("  C/I means: covalent/ionic       RG bond orbitals.")
!      stdout.text("  Thus I- represents an ionic antibonding orbital.")
!      stdout.flush
!      stdout.text(". A +ve value for the ionic bond index in bond AB")
!      stdout.text("  means a polarity toward atom (or group) A.")
!      stdout.flush
!      stdout.text(". Araki angles are associated with maximum overlap on")
!      stdout.text("  the two centres (or groups). Thus angles close to 0")
!      stdout.text("  have large overlap and are highly 'covalent' bonds,")
!      stdout.text("  while angles near 90 represent highly 'ionic' bonds.")
!      stdout.text("  They are not the angles formed in the dial diagram!")
!      stdout.flush
!
!      ! Do table
!      table.create(6)
!
!      table(1).set_heading("Paired")
!      table(1).set_subhead(" space")
!      table(1).set_values(par(1:n))
!
!      table(2).set_heading("Araki")
!      table(2).set_subhead("angle")
!      table(2).set_sb3head(" /deg")
!      table(2).set_values(ang(1:n))
!
!      table(3).set_heading("E_C+")
!      table(3).set_values(ect_p(1:n))
!
!      table(4).set_heading("E_C-")
!      table(4).set_values(ect_m(1:n))
!
!      table(5).set_heading("E_I+")
!      table(5).set_values(eit_p(1:n))
!
!      table(6).set_heading("E_I-")
!      table(6).set_values(eit_m(1:n))
!
!      ! Write it
!      table.put(label_rows=FALSE)
!
!      ! Clean
!      table.destroy
!      ind.destroy
!      eit_m.destroy; eit_p.destroy
!      ect_m.destroy; ect_p.destroy
!      ang.destroy;   par.destroy
!
!      ! Change decimal places
!      stdout.unsave_style
!
!   end

! Careful, old one below

!   put_theta_E_info
!   ! Output theta-space E bond energy information
!      self :: IN
!
!   ENSURE(.atom_a.allocated,"No roby atom A")
!   ENSURE(.atom_b.allocated,"No roby atom B")
!   ENSURE(.atom_ab.allocated,"No roby atom AB")
!
!      n,i,j,p :: INT
!      cst,snt,ang :: VEC{REAL}@
!      ect,eca,ecb :: VEC{REAL}@
!      eit,eia,eib :: VEC{REAL}@
!      emt :: VEC{REAL}@
!      evn,par :: VEC{INT}@
!      table :: VEC{TABLE_COLUMN}@
!
!      stdout.flush
!      stdout.text("===========================================")
!      stdout.text("Roby-Gould theta (bond) eigenspace energies")
!      stdout.text("===========================================")
!
!      stdout.flush
!      if (NOT .is_hyper_analysis) then
!      stdout.text(". Analysis is for atoms:")
!      stdout.flush
!      .put_unique_tags(.atom_a,"(A)")
!      .put_unique_tags(.atom_b,"(B)")
!      else
!      stdout.text(". This is a hypervalent analysis")
!      stdout.flush
!      stdout.text(". Analysis is for transfer into atom A from everything not in M")
!      stdout.flush
!      .put_unique_tags(.atom_a,"(A)")
!      .put_unique_tags(.atom_b,"(M)")
!      end
!
!      stdout.flush
!      if (NOT .is_hyper_analysis) then
!      stdout.text(". States come in bonding-antibonding pairs classified by an angle.")
!      stdout.text("  Negative high covalency have high overlap and low angle.")
!      stdout.text("  States of high ionicity have low overlap and high angle.")
!      else
!      stdout.text(". There are no covalent states, only pure ionic")
!      end
!
!      stdout.flush
!      stdout.text(". For more on this bonding analysis please cite:")
!      stdout.text("  M.D. Gould et al. (2008) Theor. Chem. Acc. 119 pp. 275-290")
!
!      n = .eigen_I.dim
!      stdout.flush
!      stdout.show("No. of states =",n)
!
!      ! Columns
!      evn.create(n)
!      par.create(n)
!      ang.create(n); cst.create(n); snt.create(n)
!      ect.create(n); eca.create(n); ecb.create(n)
!      eit.create(n); eia.create(n); eib.create(n)
!      emt.create(n)
!
!      ! Set column data
!      j = 0
!      do i = 1,n
!
!         if (.pair(i)<1) cycle
!         if (.eigen_I(i)<.eigen_I(.pair(i))) cycle
!
!         ! Index of the paired orbital
!         p = .pair(i)
!
!         j = j + 1
!         evn(j) = i
!         par(j) = p
!         cst(j) = .eigen_C(i)
!         snt(j) = .eigen_I(i)
!         ang(j) = atan2(.eigen_I(i),.eigen_C(i))*DEGREE_PER_RADIAN
!
!         eat(j) = .E_A(i)
!
!      end
!
!      n = j
!
!      stdout.flush
!      stdout.flush
!      stdout.text("=====================")
!      stdout.text("Araki angle subspaces")
!      stdout.text("=====================")
!      stdout.flush
!
!      ! Do table
!      table.create(12)
!
!      table( 1).set_heading("Bond")
!      table( 1).set_subhead("orb.")
!      table( 1).set_values(evn(1:n))
!
!      table( 2).set_heading("Anti")
!      table( 2).set_subhead("bond")
!      table( 2).set_values(par(1:n))
!
!      table( 3).set_heading("theta")
!      table( 3).set_subhead(" /deg")
!      table( 3).set_values(ang(1:n))
!
!      table( 4).set_heading("  cos")
!      table( 4).set_subhead("theta")
!      table( 4).set_values(cst(1:n))
!
!      table( 5).set_heading("  sin")
!      table( 5).set_subhead("theta")
!      table( 5).set_values(snt(1:n))
!
!      table( 6).set_heading("E_C")
!      table( 6).set_values(ect(1:n))
!
!      table( 7).set_heading("E_C")
!      table( 7).set_subhead("A")
!      table( 7).set_values(eca(1:n))
!
!      table( 8).set_heading("E_C")
!      table( 8).set_subhead("B")
!      table( 8).set_values(ecb(1:n))
!
!      table( 9).set_heading("E_I")
!      table( 9).set_values(eit(1:n))
!
!      table(10).set_heading("E_I")
!      table(10).set_subhead("A")
!      table(10).set_values(eia(1:n))
!
!      table(11).set_heading("E_I")
!      table(11).set_subhead("B")
!      table(11).set_values(eib(1:n))
!
!      table(12).set_heading("|E|")
!      table(12).set_values(emt(1:n))
!
!      ! Put table out
!      table.put(label_rows=FALSE)
!
!      ! Clean
!      table.destroy
!      emt.destroy
!      ecb.destroy; eca.destroy; ect.destroy
!      eib.destroy; eia.destroy; eit.destroy
!      snt.destroy; cst.destroy; ang.destroy
!      par.destroy; evn.destroy
!
!   end

! ===================================
! Make and diagonalise Roby operators
! ===================================

   subroutine make_full_complement(self,Q,group,ANO)
   ! Make "Q" the matrix of the complement projection operator for the
   ! atoms whose indices are in "group" in the full AO basis of
   ! .atom's. If present, use the columns of "ANO" as atomic orbital
   ! coefficients to form the projection operator instead of the
   ! actual atomic natural orbitals.
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Q
      VEC(INT,:), optional, IN :: group
      MAT(REAL,:,:), optional, IN :: ANO
      MAT(REAL,:,:), allocatable :: SI
      INT :: n_bf
   ENSURE(size(Q,1)==n_bf_(self),"ROBY:make_full_complement ... wrong dimension, Q")
   ENSURE(is_square_(Q),"ROBY:make_full_complement ... Q is incorrectly dimensioned")

      ! Make the full projection matrix
      call make_full_projection_(self,Q,group,ANO)

      ! Make unit operator "1", just S^-1, in full basis
      n_bf = n_bf_(self)
      call create_(SI,n_bf,n_bf)
      call to_pseudo_inverse_of_(SI,self%overlap_mx,TOL(5))

      ! Make complement
      Q = SI-Q
   end subroutine

   subroutine make_full_projection(self,P,group,ANO)
   ! Make "P" the matrix of the Roby projection operator for the atoms
   ! whose indices are in "group" in the full AO basis of .atom's. If
   ! present, use the columns of "ANO" as atomic orbital coefficients
   ! to form the projection operator instead of the actual atomic
   ! natural orbitals.
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: P
      VEC(INT,:), IN :: group
      MAT(REAL,:,:), optional, IN :: ANO
      MAT(REAL,:,:), allocatable :: W
      INT :: n_bf
   ENSURE(size(P,1)==n_bf_(self),"ROBY:make_full_projection ... wrong dimension, P")
   ENSURE(is_square_(P),"ROBY:make_full_projection ... P is incorrectly dimensioned")

      ! Small group
      n_bf = n_bf_(self,group)

      ! Make the "small" projector (reuse code)
      call create_(W,n_bf,n_bf)
      call make_projection_mx_(self,W,group,ANO)

      ! Copy into full basis
      call AO_subspace_set_(self,P,W,A_row_atom=group,A_col_atom=group)

      ! Clean
      call destroy_(W)
   end subroutine

   subroutine make_projection_mx(self,P,group,ANO)
   ! Make "P", the matrix of the Roby projection *operator in the
   ! (nonorthgogonal) AO basis. The AO basis is the concatenated set
   ! of basis functions for each atom in "group". If present, use the
   ! columns of "ANO" as atomic orbital coefficients to form the
   ! projection operator instead of the actual atomic natural orbitals.
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: P
      VEC(INT,:), IN :: group
      MAT(REAL,:,:), optional, IN :: ANO
      MAT(REAL,:,:), allocatable :: W,X,Y
      INT :: n_occ,n_bf
   ENSURE(size(P,1)==n_bf_(self,group),"ROBY:make_projection_mx ... wrong dimension, P ")
   ENSURE(is_square_(P),"ROBY:make_projection_mx ... P is incorrectly dimensioned")
      n_bf = n_bf_(self,group)

      ! W = columns of ANO's
      if (present(ANO)) then
         ENSURE(size(ANO,1)==n_bf_(self,group),"ROBY:make_projection_mx ... wrong dimension, ANO")
         n_occ = size(ANO,2)
         call create_(W,n_bf,n_occ)
         W = ANO
      else if (self%use_NAOs OR self%use_projected_NAOs) then
         n_occ = no_of_occupied_NAOs_(self,group)
         call create_(W,n_bf,n_occ)
         call make_NAO_mx_(self,W,group)
         if (self%show_calcs) then
            call text_(stdout,"NAO:")
            call put_(stdout,W)
         end if
      else
         n_occ = no_of_occupied_ANOs_(self,group)
         call create_(W,n_bf,n_occ)
         call make_ANO_mx_(self,W,group)
         if (self%show_calcs) then
            call text_(stdout,"ANO:")
            call put_(stdout,W)
         end if
      end if

      ! Y = overlap matrix for group(:) basis
      call create_(Y,n_bf,n_bf)
      call make_overlap_mx_(self,Y,group,group)

      ! X = S in the ANO basis
      call create_(X,n_occ,n_occ)
      call change_basis_to_(Y,X,W)
      call destroy_(Y)

      ! Y = (ANO overlap matrix)^{-1}
      call create_(Y,n_occ,n_occ)
      call to_pseudo_inverse_of_(Y,X,TOL(5))
      call destroy_(X)

      ! P = Y in the group(:) AO basis
      call back_transform_to_(Y,P,W)

      ! Clean
      call destroy_(Y)
      call destroy_(W)
   end subroutine

   PURE subroutine make_ANO_mx(self,ANO,group,tol)
   ! Make the "ANO" matrix, comprised of columns of the occupied
   ! atomic natural orbitals, for each atom whose index appears in
   ! "group".  If "tol" is present, use this cutoff to determine what
   ! is an occupied natural orbital.
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: ANO
      VEC(INT,:), IN :: group
      REAL, optional, IN :: tol
      REAL :: eps
      INT :: a,aa,b,n,n_bf,n_occ,n_cor
   ENSURE(size(ANO,1)==n_bf_(self,group),"ROBY:make_ANO_mx ... wrong shape, ANO")
   ENSURE(size(ANO,2)==no_of_occupied_ANOs_(self,group,tol),"ROBY:make_ANO_mx ... wrong shape, ANO")
      eps = self%occupied_ANO_cutoff
      if (present(tol)) eps = tol
      ANO = ZERO
      b = 0
      n = 0
      do a = 1,size(group)
         aa = group(a)
         n_bf = self%atom(aa)%basis%n_bf
         n_occ = no_of_occupied_NOs_(self%atom(aa),tol=eps)
         n_cor = self%atom(aa)%n_core_electrons
         ANO(b+1:b+n_bf,n+1:n+n_occ) = self%atom(aa)%NOs%r(:,n_cor+1:n_occ)
         b = b+n_bf
         n = n+n_occ
      end do
   end subroutine

   PURE subroutine make_NAO_mx(self,NAO,group,tol)
   ! Make the "NAO" matrix, comprised of columns of the occupied
   ! natural atomic orbitals, for each atom whose index appears in
   ! "group".  If "tol" is present, use this cutoff to determine what
   ! is an occupied natural orbital.
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: NAO
      VEC(INT,:), IN :: group
      REAL, optional, IN :: tol
      REAL :: eps
      INT :: a,aa,b,n,n_bf,n_occ
   ENSURE(size(NAO,1)==n_bf_(self,group),"ROBY:make_NAO_mx ... wrong shape, NAO")
   ENSURE(size(NAO,2)==no_of_occupied_NAOs_(self,group,tol),"ROBY:make_NAO_mx ... wrong shape, NAO")

      ! Same cutoff as for ANO's
      eps = self%occupied_ANO_cutoff
      if (present(tol)) eps = tol
      NAO = ZERO
      b = 0; n = 0
      do a = 1,size(group)
         aa = group(a)
         n_bf = self%atom(aa)%basis%n_bf
         n_occ = no_of_occupied_NAOs_(self%atom(aa),eps)
         NAO(b+1:b+n_bf,n+1:n+n_occ) = self%atom(aa)%NAOs(:,1:n_occ)
         b = b+n_bf
         n = n+n_occ
      end do
   end subroutine


!   make_NAO_mx(NAO,occupation,group,tol) :: leaky
!   ! Make the natural atomic orbitals "NAO" and their "occupation"
!   ! numbers for a "group" of atoms, from the density matrix 
!   ! ".density_mx"; "tol" is used to decide when an NAO is
!   ! occupied or not.
!      self :: IN
!      NAO :: MAT{REAL}@
!      occupation :: VEC{REAL}@
!      group :: VEC{INT}, IN
!      tol :: REAL, optional, IN
!
!   ENSURE(.overlap_mx.allocated,"no overlap matrix")
!   ENSURE(.density_mx.allocated,"no density_mx")
!   ENSURE(.density_mx.r.allocated,"no density_mx.r")
!
!      S,V,X,P :: MAT{REAL}@
!      n_bf,n_occ :: INT
!      eps :: REAL
!
!      ! No. of basis functions in the group
!      n_bf = .n_bf(group)
!
!      ! Create the (full) NAO and occupation matrix
!      NAO.create(n_bf,n_bf)
!      occupation.create(n_bf)
!
!      ! S = overlap matrix for group(:) basis
!      S.create(n_bf,n_bf)
!      .make_overlap_mx(S,group,group)
!
!      ! Get V = S^(1/2)
!      V.create(n_bf,n_bf)
!      V.to_sqrt_of(S)
!
!      ! Diagonal part of P matrix
!      P.create(n_bf,n_bf)
!      .AO_subspace_set(P,.density_mx.r,B_row_atom=group,B_col_atom=group)
!
!      ! Transform P -> S^(1/2) P S^(1/2)
!      X.create(n_bf,n_bf)
!      P.change_basis_to(X,V)
!
!      ! Diagonalize
!      X.solve_symmetric_eigenproblem(occupation,P)
!
!      ! Get V = S^(-1/2)
!      V.to_inverse_sqrt_of(S)
!
!      ! Back transform
!      NAO.to_product_of(V,P)
!
!      ! Clean
!      X.destroy
!      P.destroy
!      V.destroy
!      S.destroy
!
!      ! Debug
!      if (TRUE) then
!         stdout.flush
!         stdout.text("occupation:")
!         stdout.put(occupation)
!         stdout.text("NAO:")
!         stdout.put(NAO)
!      end
!
!      ! Reverse order
!      occupation = occupation(n_bf:1:-1)
!      NAO        =      NAO(:,n_bf:1:-1)
!
!      ! Cutoff
!      eps = .occupied_ANO_cutoff
!      if (present(tol)) eps = tol
!
!      ! Count
!      n_occ = count(occupation>=eps)
!
!      ! Shrink
!      occupation.shrink(n_occ)
!      NAO.shrink_columns(n_occ)
!
!      ! Debug
!      if (TRUE) then
!         stdout.flush
!         stdout.text("occupation:")
!         stdout.put(occupation)
!         stdout.text("NAO:")
!         stdout.put(NAO)
!      end
!
!   end

!   make_shared_operator(R) :: private
!   ! constructs the roby_shared_operator R_AB = P_A + P_B - P_AB
!   ! if spin_case is supplied then either the alpha or beta
!   ! operator is constructed, depending on the value of spin_case
!      self :: IN
!      R :: MAT{REAL}, OUT
!
!   ENSURE(.atom_a.allocated,"No roby atom A")
!   ENSURE(.atom_b.allocated,"No roby atom B")
!   ENSURE(.atom_ab.allocated,"No roby atom AB")
!
!      P_A,P_B,P_AB :: MAT{REAL}@
!      n_a, n_b, n_ab :: INT
!
!      n_a  = .n_bf_a
!      n_b  = .n_bf_b
!      n_ab = .n_bf_ab
!
!      P_A.create(n_a,n_a)
!      P_B.create(n_b,n_b)
!      P_AB.create(n_ab,n_ab)
!
!      .make_projection_mx(P_A ,.atom_a)
!      .make_projection_mx(P_B ,.atom_b)
!      .make_projection_mx(P_AB,.atom_ab)
!
!      R = ZERO
!      R(    1:n_a ,     1:n_a ) =  P_A
!      R(n_a+1:n_ab, n_a+1:n_ab) =  P_B
!      R = R - P_AB
!
!      P_AB.destroy
!      P_B.destroy
!      P_A.destroy
!
!   end

   subroutine make_ionic_operator(self,I)
   ! constructs the roby/gould ionic operator I_AB = P_A - P_B
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: I
      MAT(REAL,:,:), allocatable :: P_B,P_A
      INT :: n_a,n_b,n_ab
   ENSURE(allocated(self%atom_a),"ROBY:make_ionic_operator ... No roby atom A")
   ENSURE(allocated(self%atom_b),"ROBY:make_ionic_operator ... No roby atom B")
   ENSURE(allocated(self%atom_ab),"ROBY:make_ionic_operator ... No roby atom AB")
      n_a = n_bf_a_(self)
      n_b = n_bf_b_(self)
      n_ab = n_bf_ab_(self)
      call create_(P_A,n_a,n_a)
      call create_(P_B,n_b,n_b)
      call make_projection_mx_(self,P_A,self%atom_a)
      call make_projection_mx_(self,P_B,self%atom_b)
      I = ZERO
      I(1:n_a,1:n_a) = P_A
      I(n_a+1:n_ab,n_a+1:n_ab) = -P_B
      call destroy_(P_B)
      call destroy_(P_A)
   end subroutine

   subroutine make_hyper_operator(self,H)
   ! constructs the Roby/Gould hyper operator H_A = P_A - Q_M
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: H
      MAT(REAL,:,:), allocatable :: Q_M,P_A
      INT :: n_bf
   ENSURE(size(H,1)==n_bf_(self),"ROBY:make_hyper_operator ... Wrong dim1, H")
   ENSURE(is_square_(H),"ROBY:make_hyper_operator ... H is noyt square")
      n_bf = n_bf_(self)
      call create_(Q_M,n_bf,n_bf)
      call create_(P_A,n_bf,n_bf)
      call make_full_complement_(self,Q_M,self%atom_ab)
      call make_full_projection_(self,P_A,self%atom_a)
      H = P_A-Q_M
      call destroy_(P_A)
      call destroy_(Q_M)
   end subroutine

   subroutine diagonalize_V_AB_operator(self,X,eigenvectors,eigenvalues)
   ! For operator "X" (for example, C = cos theta or S= sin theta) in
   ! the concatenated AO basis of the atoms in .roby.atom_group,
   !    X = \sum_{i,j} |i> X_{ij} <j|, |i>,|j> in V_{AB},
   ! diagonalise and return the "eigenvectors" and "eigenvalues".
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), IN :: X
      MAT(REAL,:,:), OUT :: eigenvectors
      VEC(REAL,:), OUT :: eigenvalues
      MAT(REAL,:,:), allocatable :: XX,XV,SS,SI,SH
      INT :: n_ab
   ENSURE(allocated(self%atom_a),"ROBY:diagonalize_V_AB_operator ... No roby atom A")
   ENSURE(allocated(self%atom_b),"ROBY:diagonalize_V_AB_operator ... No roby atom B")
   ENSURE(allocated(self%atom_ab),"ROBY:diagonalize_V_AB_operator ... No roby atom AB")
      n_ab = n_bf_ab_(self)
      call create_(XX,n_ab,n_ab)
      call create_(XV,n_ab,n_ab)

      ! Overlap matrices
      call create_(SI,n_ab,n_ab)
      call create_(SH,n_ab,n_ab)
      call create_(SS,n_ab,n_ab)
      call make_overlap_mx_(self,SS,self%atom_ab,self%atom_ab)

      ! Inverse overlap
      call to_sqrt_of_(SH,SS,TOL(5))
      call to_pseudo_inverse_of_(SI,SH,TOL(5))

      ! Eigenproblem
      call back_transform_to_(X,XX,SH)
      call solve_symmetric_eigenproblem_(XX,eigenvalues,XV)
      call to_product_of_(eigenvectors,SI,XV)

      ! Clean
      call destroy_(SS)
      call destroy_(SH)
      call destroy_(SI)
      call destroy_(XV)
      call destroy_(XX)
   end subroutine

   subroutine diagonalize_full_operator(self,X,eigenvectors,eigenvalues)
   ! For operator "X" (for example, C = cos theta or S= sin theta) in
   ! the concatenated AO basis of the atoms in .roby.atom_group,
   !    X = \sum_{i,j} |i> X_{ij} <j|, |i>,|j> in V_{AB},
   ! diagonalise and return the "eigenvectors" and "eigenvalues".
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), IN :: X
      MAT(REAL,:,:), OUT :: eigenvectors
      VEC(REAL,:), OUT :: eigenvalues
! ENSURE(.atom_ab.allocated,"No roby atom AB")
      MAT(REAL,:,:), allocatable :: XX,XV,SS,SI,SH
      INT :: n_ab
   ENSURE(allocated(self%atom_a),"ROBY:diagonalize_full_operator ... No roby atom A")
   ENSURE(allocated(self%atom_b),"ROBY:diagonalize_full_operator ... No roby atom B")
      n_ab = n_bf_(self)
      call create_(XX,n_ab,n_ab)
      call create_(XV,n_ab,n_ab)

      ! Overlap matrices
      call create_(SI,n_ab,n_ab)
      call create_(SH,n_ab,n_ab)
      call create_(SS,n_ab,n_ab)
      call make_overlap_mx_(self,SS,self%atom_b,self%atom_b)

      ! Inverse overlap
      call to_sqrt_of_(SH,SS,TOL(5))
      call to_pseudo_inverse_of_(SI,SH,TOL(5))

      ! Eigenproblem
      call back_transform_to_(X,XX,SH)
      call solve_symmetric_eigenproblem_(XX,eigenvalues,XV)
      call to_product_of_(eigenvectors,SI,XV)

      ! Clean
      call destroy_(SS)
      call destroy_(SH)
      call destroy_(SI)
      call destroy_(XV)
      call destroy_(XX)
   end subroutine


! ===========================
! Make roby projected density
! ===========================

!   make_projected_density(rho,density,ANO) :: private
!   ! Make the Roby-projected density matrix in the concatenated basis
!   ! of AO functions of the atoms in .atom_ab and copy it into "rho",
!   ! a .n_bf x .n_bf matrix where all other matrix elements not corresponding
!   ! to basis functions in .atom_ab are made zero. If present, "density" is
!   ! used instead of the restricted density matrix. If present, "ANO" are used
!   ! for the Roby projection.
!      rho,density :: MAT{REAL}
!      ANO :: MAT{REAL}, optional
!   ENSURE(rho.dim1==.n_bf,"rho has wrong shape")
!   ENSURE(rho.is_square,"rho has wrong shape")
!   ENSURE(density.dim1==.n_bf,"wrong shape, density")
!   ENSURE(density.is_square,"wrong shape, density")
!   ENSURE(.atom_ab.allocated,"No roby atom AB")
!      D,P,rho_P :: MAT{REAL}@
!      n_bf :: INT
!      n_bf = .n_bf_ab
!      D.create(n_bf,n_bf)
!      .overlap_transform(density,D,col_atom=.atom_ab)
!      P.create(n_bf,n_bf)
!      .make_projection_mx(P,.atom_ab,ANO)
!      rho_P.create(n_bf,n_bf)
!      D.change_basis_to(rho_P,P) ! rho_P(small) = P_W(small) D(small) P_W(small)
!      .AO_subspace_set(rho,rho_P,A_row_atom=.atom_ab,A_col_atom=.atom_ab)
!      rho_P.destroy
!      P.destroy
!      D.destroy
!   end

! ===============================================
! Low level Roby routines used all over the place
! ===============================================

!   E_expectation result (res) :: private
!   ! Get the expectation value of the density matrix.
!      self :: IN
!      res :: REAL
!
!      E_density_mx :: MAT{REAL}@
!
!      ! Make unprojected E_density_mx
!      ! Warning: restricted case only
!      E_density_mx = HALF*(.core_mx.r + .fock_mx.r)
!
!      ! Do it
!      res = .density_mx.r.trace_product_with(E_mx)
!
!   end

!   E_expectation(P,group) result (res) :: private
!   ! Get the expectation value of the projection operator matrix "P"
!   ! over a certain "group" of atoms, i.e:
!   !    res = Trace ( S(:,group) P S(group,:) . density_mx )
!   ! Note that "group" are the indices of the atoms whose basis functions
!   ! are used to define the matrix "P".
!      self :: IN
!      P :: MAT{REAL}, IN
!      group :: VEC{INT}, optional, IN
!      res :: REAL
!
!   ENSURE(P.dim1==.n_bf(group),"wrong P dimension")
!   ENSURE(P.is_square,"P is not square")
!
!      W :: MAT{REAL}@
!      grp :: VEC{INT}@
!      n_bf :: INT
!
!      n_bf = .n_bf
!
!      if (present(group)) then; grp = group
!      else;                     grp.to_sequence(1,n_group)
!      end
!
!      ! Make W = S P S
!      W.create(n_bf,n_bf)
!      .overlap_transform(P,W,grp)
!
!      ! Make Tr ( rho . S P S )
!      ! Warning: restricted case only so far
!      res = .density_mx.r.trace_product_with(W)
!
!      ! Clean
!      W.destroy
!      grp.destroy
!
!   end

   function expectation(self,P,group,rho) result (res)
   ! Get the expectation value of the projection operator matrix "P"
   ! over a certain "group" of atoms, i.e:
   !    res = Trace ( S(:,group) P S(group,:) rho )
   ! where "rho" is by default the .density_mx. Note that "group"
   ! are the indices of the atoms whose basis functions are used to
   ! define the matrix "P".
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), IN :: P
      VEC(INT,:), optional, IN :: group
      type(OPMATRIX_TYPE), allocatable, optional, IN :: rho
      REAL :: res
      type(OPMATRIX_TYPE), allocatable :: density
      VEC(INT,:), allocatable :: grp
      MAT(REAL,:,:), allocatable :: W
      INT :: n_bf
   ENSURE(size(P,1)==n_bf_(self,group),"ROBY:expectation ... wrong P dimension")
   ENSURE(is_square_(P),"ROBY:expectation ... P is not square")
      n_bf = n_bf_(self)
      density = self%density_mx
      if (present(rho)) density = rho

      ! Make all groups?
      if (present(group)) then; grp = group
      else; call to_sequence_(grp,1,n_group_(self))
      end if

      ! Make W = S P S
      call create_(W,n_bf,n_bf)
      call overlap_transform_(self,P,W,grp)

      ! Make Tr ( S P S rho)
   ! if (.spin_multiplicity/=1) then

    !        + density.b.trace_product_with(W)
   ! else
      res = trace_product_with_(density%r,W)
   ! end
      call destroy_(W)
      call destroy_(grp)
   end function


!   project(X,P,Y,row_atom,col_atom) :: private
!   ! Do: Y = P^T S(col_atom,row_atom) X S(row_atom,col_atom) P where
!   ! S(col_atom,row_atom) is the AO subspace section of the full
!   ! overlap matrix specified by the basis functions on the atom
!   ! indices in "row_atom" and "col_atom", and "P" is a matrix
!      X,P,Y :: MAT{REAL}
!      row_atom,col_atom :: VEC{INT}
!
!   ENSURE(X.dim1==.n_bf(row_atom),"wrong X dimension")
!   ENSURE(Y.dim1==.n_bf(col_atom),"wrong Y dimension")
!   ENSURE(Y.dim1==P.dim1,"wrong P dimension")
!   ENSURE(X.is_square,"X is not square")
!   ENSURE(Y.is_square,"Y is not square")
!   ENSURE(P.is_square,"P is not square")
!
!      W :: MAT{REAL}@
!
!      W.create(Y.dim1,Y.dim2)
!      .overlap_transform(X,W,row_atom,col_atom)
!      W.change_basis_to(Y,P)
!      W.destroy
!
!   end

   subroutine overlap_transform(self,X,Y,row_atom,col_atom)
   ! Do: Y = S(col_atom,row_atom) X S(row_atom,col_atom)
   ! where S(col_atom,row_atom) is the section of the full overlap
   ! matrix specified by the basis functions on the atom indices in
   ! "row_atom" and "col_atom".
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), IN :: X
      MAT(REAL,:,:), OUT :: Y
      VEC(INT,:), optional, IN :: row_atom,col_atom
      MAT(REAL,:,:), allocatable :: SS
      INT :: n_row,n_col
      n_row = n_bf_(self,row_atom)
      n_col = n_bf_(self,col_atom)

      ! SS = small S
      call create_(SS,n_row,n_col)
      call make_overlap_mx_(self,SS,row_atom,col_atom)
      call change_basis_to_(X,Y,SS)
      call destroy_(SS)
   end subroutine

   subroutine right_overlap_transform(self,X,Y,row_atom,col_atom)
   ! Do: Y = X S(row_atom,col_atom), where S(row_atom,col_atom) is the
   ! AO subspace section of the full overlap matrix specified by the
   ! basis functions on the atom indices in "row_atom" and "col_atom".
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), IN :: X
      MAT(REAL,:,:), OUT :: Y
      VEC(INT,:), optional, IN :: row_atom,col_atom
      MAT(REAL,:,:), allocatable :: SS
      INT :: n_row,n_col
      n_row = n_bf_(self,row_atom)
      n_col = n_bf_(self,col_atom)
      call create_(SS,n_row,n_col)
      call make_overlap_mx_(self,SS,row_atom,col_atom)
      call to_product_of_(Y,X,SS)
      call destroy_(SS)
   end subroutine

   PURE subroutine make_overlap_mx(self,SS,row_atom,col_atom)
   ! Make an overlap section "SS" from the basis functions of the atoms
   ! specified in "row_atom" and "col_atom".
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: SS
      VEC(INT,:), optional, IN :: row_atom,col_atom
   ENSURE(allocated(self%overlap_mx),"ROBY:make_overlap_mx ... no overlap matrix")
   ENSURE(size(SS,1)==n_bf_(self,row_atom),"ROBY:make_overlap_mx ... wrong shape, SS")
   ENSURE(size(SS,2)==n_bf_(self,col_atom),"ROBY:make_overlap_mx ... wrong shape, SS")
      call AO_subspace_set_(self,SS,self%overlap_mx,B_row_atom=row_atom,B_col_atom=col_atom)
   end subroutine

   PURE subroutine AO_subspace_set(self,A,B,A_row_atom,A_col_atom,B_row_atom,B_col_atom)
   ! If either "B_row_atom" or "B_col_atom" is present, then set "A"
   ! equal to the AO subspace blocks of "B" specified by the atom
   ! indices in "B_row_atom" and "B_col_atom". If either is missing,
   ! then copy the entire row or col, i.e.
   !    A(small) = B(B_row_atom,B_col_atom)
   ! If either "A_row_atom" or "A_col_atom" is present, then set the
   ! AO subspace blocks of "A" specified by the atom indices in
   ! "A_row_atom" and "A_col_atom" equal to "B". If either is missing
   ! then copy the entire row or column. Uncopied blocks are set to
   ! zero, i.e.
   !    A(A_row_atom,A_col_atom) = B(small)
      type(ROBY_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: A
      MAT(REAL,:,:), IN :: B
      VEC(INT,:), optional, IN :: A_row_atom,A_col_atom
      VEC(INT,:), optional, IN :: B_row_atom,B_col_atom
      INT :: n_row_atoms,n_col_atoms
      INT :: i,b_i,f_i,l_i,n_i,j,b_j,f_j,l_j,n_j
      VEC(INT,:), allocatable :: first_basis_fn_for,last_basis_fn_for
      BIN :: Achk,Bchk
      Achk = present(A_row_atom) OR present(A_col_atom)
      Bchk = present(B_row_atom) OR present(B_col_atom)
      ENSURE(Achk NEQV Bchk,"ROBY:AO_subspace_set ... missing A/B row or col atoms")
      call make_atom_basis_fn_limits_(self%atom,first_basis_fn_for,last_basis_fn_for)
      if (present(B_row_atom) OR present(B_col_atom)) then
         ENSURE(size(B,1)==n_bf_(self),"ROBY:AO_subspace_set ... B has wrong shape")
         ENSURE(size(B,2)==n_bf_(self),"ROBY:AO_subspace_set ... B has wrong shape")
         if (present(B_row_atom) AND present(B_col_atom)) then
            n_row_atoms = size(B_row_atom)
            n_col_atoms = size(B_col_atom)
            ENSURE(size(A,1)==no_of_basis_functions_(self%atom(B_row_atom)),"ROBY:AO_subspace_set ... A has wrong shape")
            ENSURE(size(A,2)==no_of_basis_functions_(self%atom(B_col_atom)),"ROBY:AO_subspace_set ... A has wrong shape")
            b_i = 0
            do i = 1,n_row_atoms
               f_i = first_basis_fn_for(B_row_atom(i))
               l_i = last_basis_fn_for(B_row_atom(i))
               n_i = l_i-f_i+1
               b_j = 0
               do j = 1,n_col_atoms
                  f_j = first_basis_fn_for(B_col_atom(j))
                  l_j = last_basis_fn_for(B_col_atom(j))
                  n_j = l_j-f_j+1
                  A(b_i+1:b_i+n_i,b_j+1:b_j+n_j) = B(f_i:l_i,f_j:l_j)
                  b_j = b_j+n_j
               end do
               b_i = b_i+n_i
            end do
         else if (present(B_row_atom)) then
            n_row_atoms = size(B_row_atom)
            ENSURE(size(A,1)==no_of_basis_functions_(self%atom(B_row_atom)),"ROBY:AO_subspace_set ... A has wrong shape")
            ENSURE(size(A,2)==n_bf_(self),"ROBY:AO_subspace_set ... A has wrong shape")
            b_i = 0
            do i = 1,n_row_atoms
               f_i = first_basis_fn_for(B_row_atom(i));
               l_i = last_basis_fn_for(B_row_atom(i))
               n_i = l_i-f_i+1
               A(b_i+1:b_i+n_i,:) = B(f_i:l_i,:)
               b_i = b_i+n_i
            end do
         else if (present(B_col_atom)) then
            n_col_atoms = size(B_col_atom)
            ENSURE(size(A,1)==n_bf_(self),"ROBY:AO_subspace_set ... A has wrong shape")
            ENSURE(size(A,2)==no_of_basis_functions_(self%atom(B_col_atom)),"ROBY:AO_subspace_set ... A has wrong shape")
            b_j = 0
            do j = 1,n_col_atoms
               f_j = first_basis_fn_for(B_col_atom(j));
               l_j = last_basis_fn_for(B_col_atom(j))
               n_j = l_j-f_j+1
               A(:,b_j+1:b_j+n_j) = B(:,f_j:l_j)
               b_j = b_j+n_j
            end do
         end if

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      else if (present(A_row_atom) OR present(A_col_atom)) then
         ENSURE(size(A,1)==n_bf_(self),"ROBY:AO_subspace_set ... A has wrong shape")
         ENSURE(size(A,2)==n_bf_(self),"ROBY:AO_subspace_set ... A has wrong shape")
         A = ZERO
         if (present(A_row_atom) AND present(A_col_atom)) then
            n_row_atoms = size(A_row_atom)
            n_col_atoms = size(A_col_atom)
            ENSURE(size(B,1)==no_of_basis_functions_(self%atom(A_row_atom)),"ROBY:AO_subspace_set ... B has wrong shape")
            ENSURE(size(B,2)==no_of_basis_functions_(self%atom(A_col_atom)),"ROBY:AO_subspace_set ... B has wrong shape")
            b_i = 0
            do i = 1,n_row_atoms
               f_i = first_basis_fn_for(A_row_atom(i))
               l_i = last_basis_fn_for(A_row_atom(i))
               n_i = l_i-f_i+1
               b_j = 0
               do j = 1,n_col_atoms
                  f_j = first_basis_fn_for(A_col_atom(j))
                  l_j = last_basis_fn_for(A_col_atom(j))
                  n_j = l_j-f_j+1
                  A(f_i:l_i,f_j:l_j) = B(b_i+1:b_i+n_i,b_j+1:b_j+n_j)
                  b_j = b_j+n_j
               end do
               b_i = b_i+n_i
            end do
         else if (present(A_row_atom)) then
            n_row_atoms = size(A_row_atom)
            ENSURE(size(B,1)==no_of_basis_functions_(self%atom(A_row_atom)),"ROBY:AO_subspace_set ... B has wrong shape")
            ENSURE(size(B,2)==n_bf_(self),"ROBY:AO_subspace_set ... B has wrong shape")
            b_i = 0
            do i = 1,n_row_atoms
               f_i = first_basis_fn_for(A_row_atom(i));
               l_i = last_basis_fn_for(A_row_atom(i))
               n_i = l_i-f_i+1
               A(f_i:l_i,:) = B(b_i+1:b_i+n_i,:)
               b_i = b_i+n_i
            end do
         else if (present(A_col_atom)) then
            n_col_atoms = size(A_col_atom)
            ENSURE(size(B,1)==no_of_basis_functions_(self%atom(A_col_atom)),"ROBY:AO_subspace_set ... B has wrong shape")
            ENSURE(size(B,2)==n_bf_(self),"ROBY:AO_subspace_set ... B has wrong shape")
            b_j = 0
            do j = 1,n_col_atoms
               f_j = first_basis_fn_for(A_col_atom(j));
               l_j = last_basis_fn_for(A_col_atom(j))
               n_j = l_j-f_j+1
               A(:,f_j:l_j) = B(:,b_j+1:b_j+n_j)
               b_j = b_j+n_j
            end do
         end if
      end if

      ! Clean
      call destroy_(last_basis_fn_for)
      call destroy_(first_basis_fn_for)
   end subroutine


! ==============
! Output methods
! ==============

   subroutine put_basics(self)
   ! Put to stdout the Roby atom data
      type(ROBY_TYPE), IN :: self
      INT :: i
      VEC(STR(len=STR_SIZE),:), allocatable :: symbol
   ENSURE(allocated(self%atom),"ROBY:put_basics ... no atom data")

      ! Redo decimal places
      call save_style_(stdout)
      call set_real_precision_(stdout,2)
      call flush_(stdout)
      call text_(stdout,"==============================")
      call text_(stdout,"Roby-Gould population analysis")
      call text_(stdout,"==============================")
      call flush_(stdout)
      call show_(stdout,"Molecule charge          =",self%charge)
      call show_(stdout,"Molecule multiplicity    =",self%spin_multiplicity)
      call flush_(stdout)
      call show_(stdout,"Kind of calculation      =",self%roby_kind)
      call show_(stdout,"Use NAOs?                =",self%use_NAOs)
      call show_(stdout,"Use projected NAOs?      =",self%use_projected_NAOs)
      call show_(stdout,"Use spherical averaging? =",self%use_spherical_averaging)
      call show_(stdout,"Homoleptic calculation?  =",is_homoleptic_(self))
      call show_(stdout,"Occupied ANO cutoff      =",self%occupied_ANO_cutoff)
      call show_(stdout,"Output theta info?       =",self%output_theta_info)
      call flush_(stdout)
      if (is_homoleptic_(self)) then
         symbol = unique_tags_(self)
         do i = 1,n_group_(self)
            call show_(stdout,"Atom group ="//trim(to_str_(i))//"            =",symbol(i))
         end do
         call flush_(stdout)
      else if (allocated(self%atom_list) AND NOT self%analyze_all_atom_pairs) then
         symbol = unique_tags_(self%atom,self%atom_list)
         call show_(stdout,"Atom list                =",symbol)
         call destroy_(symbol)
      end if
      call show_(stdout,"No. of atoms             =",size(self%atom))
      call show_(stdout,"Analyze all atom pairs?  =",self%analyze_all_atom_pairs)
      call show_(stdout,"Analyze VDW atom pairs?  =",self%analyze_vdw_atom_pairs)
      call show_(stdout,"Bond range factor /Angs  =",to_units_(self%bond_range_factor,"angstrom"))
      call flush_(stdout)
      call show_(stdout,"Zero angle cutoff /Deg   =",to_units_(self%zero_angle_cutoff,"degree"))

      ! Unsave
      call unsave_style_(stdout)
   end subroutine

   subroutine put_populations(self)
   ! Put to stdout the only Roby atom populations
      type(ROBY_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(INT,:), allocatable :: group
      VEC(STR(len=STR_SIZE),:), allocatable :: symbols
      VEC(REAL,:), allocatable :: vals
      INT :: n_e,i
   ENSURE(allocated(self%n1),"ROBY:put_populations ... no atom populations exist")
   ENSURE(allocated(self%atom),"ROBY:put_populations ... no atom data")

      ! Redo decimal places
      call save_style_(stdout)
      call set_real_precision_(stdout,2)
      call flush_(stdout)
      call text_(stdout,"================")
      call text_(stdout,"Roby populations")
      call text_(stdout,"================")
      call flush_(stdout)
      call text_(stdout,". Populations are the expectation of the atomic projection operators.")
      call text_(stdout,". Non-molecular electrons are not describably by any of the ANOs.")
      call text_(stdout,". Population charges are the atomic number minus the # no. electrons.")
      call text_(stdout,". Scaled charges are population charges minus their mean value.")
      n_e = no_of_electrons_(self%atom)
      call flush_(stdout)
      call show_(stdout,"No. of electrons          =",n_e)
      call show_(stdout,"No. of e's in P_{A1...An} =",self%n_all)
      call show_(stdout,"No. of non-molecular e's  =",n_e-self%n_all)
      call flush_(stdout)

      ! Table
      call create_(symbols,n_group_(self))
      if (is_homoleptic_(self)) then

         ! Group atom indices
         do i = 1,n_group_(self)
            group = self%atom_group(i)%element
            if (size(group)==1) then; symbols(i) = self%atom(group(1))%tag
            else; symbols(i) = chemical_formula_(self%atom(group))
            end if
         end do
         call create_(table,4)
         call set_heading_(table(1),"Group")
      else

         ! Atom symbols
         symbols = unique_tags_(self)
         call create_(table,5)
         call set_heading_(table(1),"Atom")
      end if
      call set_width_from_(table(1),symbols)
      call set_values_(table(1),symbols)
      call destroy_(symbols)
      call set_heading_(table(2),"No. of")
      call set_subhead_(table(2),"e's")
      call set_values_(table(2),self%n1)

      ! Pop charges
      call create_(vals,n_group_(self))
      vals = -self%n1+group_nuclear_charges_(self)
      call set_heading_(table(3),"Pop.")
      call set_subhead_(table(3),"charge")
      call set_values_(table(3),vals)

      ! Scaled charges
      vals = vals-mean_(vals)
      call set_heading_(table(4),"Scaled")
      call set_subhead_(table(4),"charge")
      call set_values_(table(4),vals)
      call destroy_(vals)
      if (NOT is_homoleptic_(self)) then
         call create_(vals,n_group_(self))
         vals = self%n1-n_core_electrons_(self%atom)
         call set_heading_(table(5),"No. of")
         call set_subhead_(table(5),"val e's")
         call set_values_(table(5),vals)
         call destroy_(vals)
      end if

      ! Put table
      call put_(table,label_rows=FALSE)

      ! Unsave
      call unsave_style_(stdout)
   end subroutine

   subroutine put_bond_indices(self)
   ! Put to stdout the Roby bond indices, including the shared populations
      type(ROBY_TYPE), IN :: self
      if (is_homoleptic_(self)) then
         call put_bond_indices_group_(self)
      else
         call put_bond_indices_atoms_(self)
         call put_bond_indices_sortd_(self)

         ! Separate data tables
         call put_file_indices_atoms_(self)

         ! LaTeX tables
         call put_bond_indices_do_H_(self)
         call put_bond_indices_do_H_vdw_(self)
         call put_bond_indices_no_H_(self)
      ! .put_bond_indices_CC
      end if
   end subroutine

   subroutine put_bond_indices_group(self)
   ! Output the bond indices table for connected atoms.
      type(ROBY_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: symbol
      VEC(INT,:), allocatable :: g_A,g_B
      VEC(INT,:), allocatable :: grA,grB
      VEC(REAL,:), allocatable :: n_A,n_B
      VEC(REAL,:), allocatable :: rAB,nAB,sAB,cAB,iAB
      VEC(REAL,:), allocatable :: tAB,pAB,aAB
      INT :: n_group,n_pair,i,a,b
      REAL :: distAB
   ENSURE(is_homoleptic_(self),"ROBY:put_bond_indices_group ... job is not homoleptic")
   ENSURE(n_group_(self)>1,"ROBY:put_bond_indices_group ... must be at least two groups")
      n_group = n_group_(self)

      ! Redo decimal places
      call save_style_(stdout)
      call set_real_precision_(stdout,2)

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"=======================")
      call text_(stdout,"Roby-Gould bond indices")
      call text_(stdout,"=======================")
      call flush_(stdout)
      call text_(stdout,"Bond indices appear only between atom groups.")
      call flush_(stdout)
      if (is_homoleptic_(self)) then
         symbol = unique_tags_(self)
         do i = 1,n_group
            call show_(stdout,"Group ="//trim(to_str_(i,fmt="i2"))//" = ",symbol(i))
         end do
         call destroy_(symbol)
         call flush_(stdout)
      end if

      ! Create table
      call create_(table,12)

      ! Set the table headings
      call set_heading_(table(1),"Grp")
      call set_subhead_(table(1),"  A")
      call set_heading_(table(2),"Grp")
      call set_subhead_(table(2),"  B")
      call set_heading_(table(3),"min r(AB)")
      call set_subheading_(table(3),"/Angs.")
      call set_width_from_(table(3),ONE)
      call set_heading_(table(4),"n_A")
      call set_width_from_(table(4),TEN)
      call set_heading_(table(5),"n_B")
      call set_width_from_(table(5),TEN)
      call set_heading_(table(6),"n_AB")
      call set_width_from_(table(6),TEN)
      call set_heading_(table(7),"s_AB")
      call set_width_from_(table(7),TEN)
      call set_heading_(table(8)," Cov.")
      call set_subhead_(table(8),"index")
      call set_width_from_(table(8),-TEN)
      call set_heading_(table(9),"Ionic")
      call set_subhead_(table(9),"index")
      call set_width_from_(table(9),-TEN)
      call set_heading_(table(10)," Bond")
      call set_subhead_(table(10),"index")
      call set_width_from_(table(10),ONE)
      call set_heading_(table(11),"% Cov")
      call set_subheading_(table(11),"Pythag.")
      call set_width_from_(table(11),100d0)
      call set_heading_(table(12),"% Cov")
      call set_subheading_(table(12),"Araki")
      call set_width_from_(table(12),100d0)

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)

      ! Populations & indices
      n_pair = n_group-1
      n_pair = triangle_number_(n_pair)
      call create_(g_A,n_pair)
      call create_(g_B,n_pair)
      call create_(rAB,n_pair)
      call create_(n_A,n_pair)
      call create_(n_B,n_pair)
      call create_(nAB,n_pair)
      call create_(sAB,n_pair)
      call create_(cAB,n_pair)
      call create_(iAB,n_pair)
      call create_(tAB,n_pair)
      call create_(pAB,n_pair)
      call create_(aAB,n_pair)
      i = 0
      do a = 1,n_group_(self)
         do b = a+1,n_group_(self)
            i = i+1
            g_A(i) = a
            g_B(i) = b
            grA = self%atom_group(a)%element(:)
            grB = self%atom_group(b)%element(:)
            distAB = min_distance_between_(self%atom,grA,grB)
            rAB(i) = distAB*ANGSTROM_PER_BOHR
            n_A(i) = self%n1(a)
            n_B(i) = self%n1(b)
            nAB(i) = self%n2(a,b)
            sAB(i) = self%n1(a)+self%n1(b)-self%n2(a,b)
            cAB(i) = self%cov_index(a,b)
            iAB(i) = self%ion_index(a,b)
            tAB(i) = self%bnd_index(a,b)
            pAB(i) = self%percent_covalency_P(a,b)
            aAB(i) = self%percent_covalency_A(a,b)
         end do
      end do

      ! Set table data
      call set_values_(table(1),g_A)
      call set_values_(table(2),g_B)
      call set_values_(table(3),rAB)
      call set_values_(table(4),n_A)
      call set_values_(table(5),n_B)
      call set_values_(table(6),nAB)
      call set_values_(table(7),sAB)
      call set_values_(table(8),cAB)
      call set_values_(table(9),iAB)
      call set_values_(table(10),tAB)
      call set_values_(table(11),pAB)
      call set_values_(table(12),aAB)

      ! Put body
      call flush_(stdout)
      call put_body_(table,label_rows=FALSE)

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(aAB)
      call destroy_(pAB)
      call destroy_(tAB)
      call destroy_(iAB)
      call destroy_(cAB)
      call destroy_(sAB)
      call destroy_(nAB)
      call destroy_(rAB)
      call destroy_(n_B)
      call destroy_(n_A)
      call destroy_(g_B)
      call destroy_(g_A)

      ! Unsave
      call unsave_style_(stdout)
   end subroutine

   subroutine put_bond_indices_hyper(self)
   ! Output the bond indices table for connected atoms.
      type(ROBY_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: symbol
      VEC(INT,:), allocatable :: g_A
      VEC(REAL,:), allocatable :: n_A,cAB,iAB,tAB,pAB,aAB
      INT :: n_group,i,a
   ENSURE(self%is_hyper_analysis,"ROBY:put_bond_indices_hyper ... must be at hypervalent analysis")
   ENSURE(n_group_(self)>1,"ROBY:put_bond_indices_hyper ... must be at leat two groups")
      n_group = n_group_(self)

      ! Redo decimal places
      call save_style_(stdout)
      call set_real_precision_(stdout,2)

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"===================================")
      call text_(stdout,"Hypervalent Roby-Gould bond indices")
      call text_(stdout,"===================================")
      call flush_(stdout)
      call text_(stdout,"Bond indices appear between atom groups and everything")
      call text_(stdout,"*not* in the space of the atoms.")
      call flush_(stdout)
      call text_(stdout,"There should only appear ionic transfer, no covalency.")
      call flush_(stdout)
      symbol = unique_tags_(self)
      do i = 1,n_group
         call show_(stdout,"Group ="//trim(to_str_(i,fmt="i2"))//" = ",symbol(i))
      end do
      call destroy_(symbol)
      call flush_(stdout)

      ! Create table
      call create_(table,7)

      ! Set the table headings
      call set_heading_(table(1),"Grp")
      call set_subhead_(table(1),"  A")
      call set_heading_(table(2),"n_A")
      call set_width_from_(table(2),TEN)
      call set_heading_(table(3)," Cov.")
      call set_subhead_(table(3),"index")
      call set_width_from_(table(3),-TEN)
      call set_heading_(table(4),"Ionic")
      call set_subhead_(table(4),"index")
      call set_width_from_(table(4),-TEN)
      call set_heading_(table(5)," Bond")
      call set_subhead_(table(5),"index")
      call set_width_from_(table(5),ONE)
      call set_heading_(table(6),"% Cov")
      call set_subhead_(table(6),"Pythag.")
      call set_width_from_(table(6),100d0)
      call set_heading_(table(7),"% Cov")
      call set_subhead_(table(7),"Araki")
      call set_width_from_(table(7),100d0)

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)

      ! Populations & indices
      call create_(g_A,n_group)
      call create_(n_A,n_group)
      call create_(cAB,n_group)
      call create_(iAB,n_group)
      call create_(tAB,n_group)
      call create_(pAB,n_group)
      call create_(aAB,n_group)
      i = 0
      do a = 1,n_group_(self)
         i = i+1
         g_A(i) = a
         n_A(i) = self%n1(a)
         cAB(i) = self%cov_index(a,a)
         iAB(i) = self%ion_index(a,a)
         tAB(i) = self%bnd_index(a,a)
         pAB(i) = self%percent_covalency_P(a,a)
         aAB(i) = self%percent_covalency_A(a,a)
      end do

      ! Set table data
      call set_values_(table(1),g_A)
      call set_values_(table(2),n_A)
      call set_values_(table(3),cAB)
      call set_values_(table(4),iAB)
      call set_values_(table(5),tAB)
      call set_values_(table(6),pAB)
      call set_values_(table(7),aAB)

      ! Put body
      call flush_(stdout)
      call put_body_(table,label_rows=FALSE)

      ! Clean
      call clear_columns_(table)
      call destroy_(aAB)
      call destroy_(pAB)
      call destroy_(tAB)
      call destroy_(iAB)
      call destroy_(cAB)
      call destroy_(n_A)
      call destroy_(g_A)

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)

      ! Unsave
      call unsave_style_(stdout)
   end subroutine

   subroutine put_bond_indices_atoms(self)
   ! Output the bond indices table for connected atoms.
      type(ROBY_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: no_of_bonds,n_kind,k1,k2,n_bonds,i
      MAT(INT,:,:), allocatable :: pair
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols,labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash,dsh1
      VEC(REAL,:), allocatable :: dist,n_A,n_B,nAB,sAB,cAB,iAB,tAB,pAB,aAB
      BIN :: has_unique_labels
   ENSURE(NOT is_homoleptic_(self),"ROBY:put_bond_indices_atoms ... job is homoleptic")

      ! Anything to do?
      if (size(self%atom)<2) return
      no_of_bonds = no_of_bonds_(self%atom,self%bond_range_factor)
      if (no_of_bonds<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self%atom)
      DIE_IF(NOT has_unique_labels,"ROBY:put_bond_indices_atoms ... atoms are not uniquely labelled!")

      ! Redo decimal places
      call save_style_(stdout)
      call set_real_precision_(stdout,2)

      ! Title
      if (self%analyze_vdw_atom_pairs) then
         call flush_(stdout)
         call text_(stdout,"==============================================")
         call text_(stdout,"Roby-Gould bond indices: VDW interactions only")
         call text_(stdout,"==============================================")
      else
         call flush_(stdout)
         call text_(stdout,"=======================")
         call text_(stdout,"Roby-Gould bond indices")
         call text_(stdout,"=======================")
      end if
      call flush_(stdout)
      if (self%analyze_vdw_atom_pairs) then
         call text_(stdout,"Indices appear only for VDW bonded atom pairs and")
         call text_(stdout,"NOT normally bonded by CSD criterion")
         call flush_(stdout)
         call show_(stdout,"No. of 'bonds'          =",no_of_bonds)
      else if (self%analyze_all_atom_pairs) then
         call text_(stdout,"Indices appear only for atom pairs which are connected")
         call text_(stdout,"according to the Cambridge Structural Database (CSD)")
         call text_(stdout,"AND if they are within a minimum bond_range_factor")
         call flush_(stdout)
         call show_(stdout,"Bond range factor /Angs =",to_units_(self%bond_range_factor,"angstrom"))
         call show_(stdout,"No. of 'bonds'          =",no_of_bonds)
      else
         call text_(stdout,"Indices appear only for atom pairs which are connected")
         call text_(stdout,"according to the Cambridge Structural Database (CSD).")
         call flush_(stdout)
         call show_(stdout,"No. of bonds =",no_of_bonds)
      end if
      call flush_(stdout)

      ! Get atom Z list
      call make_atom_Z_list_(self%atom,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self%atom(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      call create_(table,16)

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),chemical_symbols_(self%atom))
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"--")
      call set_width_(table(2),2)
      call set_heading_(table(3),"B")
      call set_width_(table(3),table(1)%width)
      call set_left_justify_(table(3),TRUE)
      call set_heading_(table(4),"A")
      call set_width_from_(table(4),self%atom(:)%tag)
      call set_column_spacing_(table(5),0)
      call set_heading_(table(5),"--")
      call set_width_(table(5),2)
      call set_heading_(table(6),"B")
      call set_width_(table(6),table(4)%width)
      call set_left_justify_(table(6),TRUE)
      call set_heading_(table(7),"R(A--B)")
      call set_subheading_(table(7),"/Angs.")
      call set_width_from_(table(7),ONE)
      call set_heading_(table(8),"n_A")
      call set_width_from_(table(8),TEN)
      call set_heading_(table(9),"n_B")
      call set_width_from_(table(9),TEN)
      call set_heading_(table(10),"n_AB")
      call set_width_from_(table(10),TEN)
      call set_heading_(table(11),"s_AB")
      call set_width_from_(table(11),TEN)
      call set_heading_(table(12)," Cov.")
      call set_subhead_(table(12),"index")
      call set_width_from_(table(12),-TEN)
      call set_heading_(table(13),"Ionic")
      call set_subhead_(table(13),"index")
      call set_width_from_(table(13),-TEN)
      call set_heading_(table(14)," Bond")
      call set_subhead_(table(14),"index")
      call set_width_from_(table(14),ONE)
      call set_heading_(table(15),"% Cov")
      call set_subhead_(table(15),"Pythag.")
      call set_width_from_(table(15),100d0)
      call set_heading_(table(16),"% Cov")
      call set_subhead_(table(16),"Araki")
      call set_width_from_(table(16),100d0)

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = k1,n_kind

         ! Get the bonded atoms per kind
            if (self%analyze_vdw_atom_pairs) then
               call get_vdw_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,self%bond_range_factor)
            else
               call get_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,self%bond_range_factor)
            end if
            dist = dist*ANGSTROM_PER_BOHR

         ! No of bonds in this class
            n_bonds = size(dist)
            if (n_bonds==0) then
               call destroy_(dist)
               call destroy_(pair)
               cycle
            end if

         ! Element symbols
            call create_(symbols,n_bonds,2)
            symbols = " "
            symbols(1,1) = chemical_symbol_(self%atom(Z_list(k1)%element(1)))
            symbols(1,2) = chemical_symbol_(self%atom(Z_list(k2)%element(1)))

         ! Dashes
            call create_(dash,n_bonds)
            call create_(dsh1,n_bonds)
            dash = "--"
            dsh1 = "  "
            dsh1(1) = "--"

         ! Labels
            call create_(labels,n_bonds,2)
            labels(:,1) = self%atom(pair(:,1))%tag
            labels(:,2) = self%atom(pair(:,2))%tag

         ! Populations & indices
            call create_(n_A,n_bonds)
            call create_(n_B,n_bonds)
            call create_(nAB,n_bonds)
            call create_(sAB,n_bonds)
            call create_(cAB,n_bonds)
            call create_(iAB,n_bonds)
            call create_(tAB,n_bonds)
            call create_(pAB,n_bonds)
            call create_(aAB,n_bonds)
            n_A = self%n1(pair(:,1))
            n_B = self%n1(pair(:,2))
            forall (i=1:n_bonds)
               nAB(i) = self%n2(pair(i,1),pair(i,2))
               sAB(i) = n_A(i)+n_B(i)-nAB(i)
               cAB(i) = self%cov_index(pair(i,1),pair(i,2))
               iAB(i) = self%ion_index(pair(i,1),pair(i,2))
               tAB(i) = self%bnd_index(pair(i,1),pair(i,2))
               pAB(i) = self%percent_covalency_P(pair(i,1),pair(i,2))
               aAB(i) = self%percent_covalency_A(pair(i,1),pair(i,2))
            end forall

         ! Set table data
            call set_values_(table(1),symbols(:,1))
            call set_values_(table(2),dsh1)
            call set_values_(table(3),symbols(:,2))
            call set_values_(table(4),labels(:,1))
            call set_values_(table(5),dash)
            call set_values_(table(6),labels(:,2))
            call set_values_(table(7),dist)
            call set_values_(table(8),n_A)
            call set_values_(table(9),n_B)
            call set_values_(table(10),nAB)
            call set_values_(table(11),sAB)
            call set_values_(table(12),cAB)
            call set_values_(table(13),iAB)
            call set_values_(table(14),tAB)
            call set_values_(table(15),pAB)
            call set_values_(table(16),aAB)

         ! Put body
            call flush_(stdout)
            call put_body_(table,label_rows=FALSE)

         ! Clean
            call clear_columns_(table)
            call destroy_(aAB)
            call destroy_(pAB)
            call destroy_(tAB)
            call destroy_(iAB)
            call destroy_(cAB)
            call destroy_(sAB)
            call destroy_(nAB)
            call destroy_(n_B)
            call destroy_(n_A)
            call destroy_(labels)
            call destroy_(dsh1)
            call destroy_(dash)
            call destroy_(symbols)
            call destroy_(dist)
            call destroy_(pair)
         end do
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)

      ! Unsave
      call unsave_style_(stdout)
   end subroutine

   subroutine put_bond_indices_sortd(self)
   ! Output the bond indices table for connected atoms.
      type(ROBY_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: no_of_bonds,n_kind,k1,k2,n_bonds,i
      MAT(INT,:,:), allocatable :: pair
      MAT(STR(len=STR_SIZE),:,:), allocatable :: labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash
      VEC(REAL,:), allocatable :: dist,cAB,iAB,tAB,pAB,aAB
      VEC(INT,:), allocatable :: ind
      BIN :: has_unique_labels
   ENSURE(NOT is_homoleptic_(self),"ROBY:put_bond_indices_sortd ... job is homoleptic")

      ! Anything to do?
      if (size(self%atom)<2) return
      no_of_bonds = no_of_bonds_(self%atom,self%bond_range_factor)
      if (no_of_bonds<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self%atom)
      DIE_IF(NOT has_unique_labels,"ROBY:put_bond_indices_sortd ... atoms are not uniquely labelled!")

      ! Redo decimal places
      call save_style_(stdout)
      call set_real_precision_(stdout,2)
      call redirect_(stdout,"rgbi-bondtable+H+vdw.tex")

      ! Title
      if (self%analyze_vdw_atom_pairs) then
         call flush_(stdout)
         call text_(stdout,"=====================================================")
         call text_(stdout,"Sorted Roby-Gould bond indices: VDW interactions only")
         call text_(stdout,"=====================================================")
      else
         call flush_(stdout)
         call text_(stdout,"==============================")
         call text_(stdout,"Sorted Roby-Gould bond indices")
         call text_(stdout,"==============================")
      end if
      call flush_(stdout)
      call text_(stdout,". Indices are sorted from lowest to highest")
      call flush_(stdout)
      if (self%analyze_vdw_atom_pairs) then
         call text_(stdout,"Indices appear only for VDW bonded atom pairs and")
         call text_(stdout,"NOT normally bonded by CSD criterion")
         call flush_(stdout)
         call show_(stdout,"No. of 'bonds'          =",no_of_bonds)
      else if (self%analyze_all_atom_pairs) then
         call text_(stdout,". Indices appear only for atom pairs which are connected")
         call text_(stdout,"  according to the Cambridge Structural Database (CSD)")
         call text_(stdout,"  AND if they are within a minimum bond_range_factor")
         call flush_(stdout)
         call show_(stdout,"Bond range factor /Angs =",to_units_(self%bond_range_factor,"angstrom"))
         call show_(stdout,"No. of 'bonds'          =",no_of_bonds)
      else
         call text_(stdout,". Indices appear only for atom pairs which are connected")
         call text_(stdout,"  according to the Cambridge Structural Database (CSD).")
         call flush_(stdout)
         call show_(stdout,"No. of bonds =",no_of_bonds)
      end if
      call flush_(stdout)

      ! Get atom Z list
      call make_atom_Z_list_(self%atom,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self%atom(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      call create_(table,9)

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),self%atom(:)%tag)
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"--")
      call set_width_(table(2),2)
      call set_heading_(table(3),"B")
      call set_width_(table(3),table(1)%width)
      call set_left_justify_(table(3),TRUE)
      call set_heading_(table(4),"R(A--B)")
      call set_subheading_(table(4),"/Angs.")
      call set_width_from_(table(4),ONE)
      call set_heading_(table(5)," Cov.")
      call set_subhead_(table(5),"index")
      call set_width_from_(table(5),-TEN)
      call set_heading_(table(6),"Ionic")
      call set_subhead_(table(6),"index")
      call set_width_from_(table(6),-TEN)
      call set_heading_(table(7)," Bond")
      call set_subhead_(table(7),"index")
      call set_width_from_(table(7),ONE)
      call set_heading_(table(8),"% Cov")
      call set_subhead_(table(8),"Pythag.")
      call set_width_from_(table(8),100d0)
      call set_heading_(table(9),"% Cov")
      call set_subhead_(table(9),"Araki")
      call set_width_from_(table(9),100d0)

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = k1,n_kind

         ! Get the bonded atoms per kind
            if (self%analyze_vdw_atom_pairs) then
               call get_vdw_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,self%bond_range_factor)
            else
               call get_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,self%bond_range_factor)
            end if
            dist = dist*ANGSTROM_PER_BOHR

         ! No of bonds in this class
            n_bonds = size(dist)
            if (n_bonds==0) then
               call destroy_(dist)
               call destroy_(pair)
               cycle
            end if

         ! Dashes
            call create_(dash,n_bonds)
            dash = "--"

         ! Labels
            call create_(labels,n_bonds,2)
            labels(:,1) = self%atom(pair(:,1))%tag
            labels(:,2) = self%atom(pair(:,2))%tag

         ! Populations & indices
            call create_(cAB,n_bonds)
            call create_(iAB,n_bonds)
            call create_(tAB,n_bonds)
            call create_(pAB,n_bonds)
            call create_(aAB,n_bonds)
            do i = 1,n_bonds
               cAB(i) = self%cov_index(pair(i,1),pair(i,2))
               iAB(i) = self%ion_index(pair(i,1),pair(i,2))
               tAB(i) = self%bnd_index(pair(i,1),pair(i,2))
               pAB(i) = self%percent_covalency_P(pair(i,1),pair(i,2))
               aAB(i) = self%percent_covalency_A(pair(i,1),pair(i,2))
            end do

         ! Sort & rearrange
            call create_(ind,n_bonds)
            call quick_sort_(tAB,ind,decreasing_order=TRUE)
            labels(:,1) = labels(ind,1)
            labels(:,2) = labels(ind,2)
            dist = dist(ind)
            cAB = cAB(ind)
            iAB = iAB(ind)
            tAB = tAB(ind)
            pAB = pAB(ind)
            aAB = aAB(ind)

         ! Set table data
            call set_values_(table(1),labels(:,1))
            call set_values_(table(2),dash)
            call set_values_(table(3),labels(:,2))
            call set_values_(table(4),dist)
            call set_values_(table(5),cAB)
            call set_values_(table(6),iAB)
            call set_values_(table(7),tAB)
            call set_values_(table(8),pAB)
            call set_values_(table(9),aAB)

         ! Put body
            call flush_(stdout)
            call put_body_(table,label_rows=FALSE)

         ! Clean
            call clear_columns_(table)
            call destroy_(ind)
            call destroy_(aAB)
            call destroy_(pAB)
            call destroy_(tAB)
            call destroy_(iAB)
            call destroy_(cAB)
            call destroy_(labels)
            call destroy_(dash)
            call destroy_(dist)
            call destroy_(pair)
         end do
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)

      ! Revert & unsave
      call revert_(stdout)
      call unsave_style_(stdout)
   end subroutine

   subroutine put_file_indices_atoms(self)
   ! Output bond indices in separate files "rgbi-A-B" where 
   ! A and B are the element labels.
      type(ROBY_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: no_of_bonds,n_kind,k1,k2,n_bonds,i
      MAT(INT,:,:), allocatable :: pair
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols,labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash,dsh1
      VEC(REAL,:), allocatable :: dist,n_A,n_B,nAB,sAB,cAB,iAB,tAB,pAB,aAB
      BIN :: has_unique_labels
   ENSURE(NOT is_homoleptic_(self),"ROBY:put_file_indices_atoms ... job is homoleptic")

      ! Anything to do?
      if (size(self%atom)<2) return
      no_of_bonds = no_of_bonds_(self%atom,self%bond_range_factor)
      if (no_of_bonds<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self%atom)
      DIE_IF(NOT has_unique_labels,"ROBY:put_file_indices_atoms ... atoms are not uniquely labelled!")

      ! Redo decimal places
      call save_style_(stdout)
      call set_real_precision_(stdout,2)

      ! Get atom Z list
      call make_atom_Z_list_(self%atom,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self%atom(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      call create_(table,14)

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),chemical_symbols_(self%atom))
      call set_heading_(table(2),"B")
      call set_width_(table(2),table(1)%width)
      call set_heading_(table(3),"A")
      call set_width_from_(table(3),self%atom(:)%tag)
      call set_heading_(table(4),"B")
      call set_width_(table(4),table(3)%width)
      call set_heading_(table(5),"R(A--B)")
      call set_subheading_(table(5),"/Angs.")
      call set_width_from_(table(5),ONE)
      call set_heading_(table(6),"n_A")
      call set_width_from_(table(6),TEN)
      call set_heading_(table(7),"n_B")
      call set_width_from_(table(7),TEN)
      call set_heading_(table(8),"n_AB")
      call set_width_from_(table(8),TEN)
      call set_heading_(table(9),"s_AB")
      call set_width_from_(table(9),TEN)
      call set_heading_(table(10)," Cov.")
      call set_subhead_(table(10),"index")
      call set_width_from_(table(10),-TEN)
      call set_heading_(table(11),"Ionic")
      call set_subhead_(table(11),"index")
      call set_width_from_(table(11),-TEN)
      call set_heading_(table(12)," Bond")
      call set_subhead_(table(12),"index")
      call set_width_from_(table(12),ONE)
      call set_heading_(table(13),"% Cov")
      call set_subhead_(table(13),"Pythag.")
      call set_width_from_(table(13),100d0)
      call set_heading_(table(14),"% Cov")
      call set_subhead_(table(14),"Araki")
      call set_width_from_(table(14),100d0)

      ! Put out the headings
   ! table.put_headings(label_rows=FALSE,no_flush=TRUE)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = k1,n_kind

         ! Get the bonded atoms per kind
            if (self%analyze_vdw_atom_pairs) then
               call get_vdw_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,self%bond_range_factor)
            else
               call get_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,self%bond_range_factor)
            end if
            dist = dist*ANGSTROM_PER_BOHR

         ! No of bonds in this class
            n_bonds = size(dist)
            if (n_bonds==0) then
               call destroy_(dist)
               call destroy_(pair)
               cycle
            end if

         ! Element symbols
            call create_(symbols,n_bonds,2)
            symbols(:,1) = chemical_symbol_(self%atom(Z_list(k1)%element(1)))
            symbols(:,2) = chemical_symbol_(self%atom(Z_list(k2)%element(1)))

         ! Dashes
            call create_(dash,n_bonds)
            call create_(dsh1,n_bonds)
            dash = "--"
            dsh1 = "  "
            dsh1(1) = "--"

         ! Labels
            call create_(labels,n_bonds,2)
            labels(:,1) = self%atom(pair(:,1))%tag
            labels(:,2) = self%atom(pair(:,2))%tag

         ! Populations & indices
            call create_(n_A,n_bonds)
            call create_(n_B,n_bonds)
            call create_(nAB,n_bonds)
            call create_(sAB,n_bonds)
            call create_(cAB,n_bonds)
            call create_(iAB,n_bonds)
            call create_(tAB,n_bonds)
            call create_(pAB,n_bonds)
            call create_(aAB,n_bonds)
            n_A = self%n1(pair(:,1))
            n_B = self%n1(pair(:,2))
            forall (i=1:n_bonds)
               nAB(i) = self%n2(pair(i,1),pair(i,2))
               sAB(i) = n_A(i)+n_B(i)-nAB(i)
               cAB(i) = self%cov_index(pair(i,1),pair(i,2))
               iAB(i) = self%ion_index(pair(i,1),pair(i,2))
               tAB(i) = self%bnd_index(pair(i,1),pair(i,2))
               pAB(i) = self%percent_covalency_P(pair(i,1),pair(i,2))
               aAB(i) = self%percent_covalency_A(pair(i,1),pair(i,2))
            end forall

         ! Set table data
            call set_values_(table(1),symbols(:,1))
            call set_values_(table(2),symbols(:,2))
            call set_values_(table(3),labels(:,1))
            call set_values_(table(4),labels(:,2))
            call set_values_(table(5),dist)
            call set_values_(table(6),n_A)
            call set_values_(table(7),n_B)
            call set_values_(table(8),nAB)
            call set_values_(table(9),sAB)
            call set_values_(table(10),cAB)
            call set_values_(table(11),iAB)
            call set_values_(table(12),tAB)
            call set_values_(table(13),pAB)
            call set_values_(table(14),aAB)

         ! Put body
            call flush_(stdout)
            call redirect_(stdout,"rgbi-data-"//trim(symbols(1,1))//"-"//trim(symbols(1,2)))
            call put_body_(table,label_rows=FALSE)
            call revert_(stdout)

         ! Clean
            call clear_columns_(table)
            call destroy_(aAB)
            call destroy_(pAB)
            call destroy_(tAB)
            call destroy_(iAB)
            call destroy_(cAB)
            call destroy_(sAB)
            call destroy_(nAB)
            call destroy_(n_B)
            call destroy_(n_A)
            call destroy_(labels)
            call destroy_(dsh1)
            call destroy_(dash)
            call destroy_(symbols)
            call destroy_(dist)
            call destroy_(pair)
         end do
      end do

      ! Table footer
   ! table.put_footer(label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)

      ! Unsave
      call unsave_style_(stdout)
   end subroutine

   subroutine put_bond_indices_do_H(self)
   ! Output bond indices including H's in LaTeX "rgbi-bondtable+H.tex".
      type(ROBY_TYPE), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: no_of_bonds,n_kind,k1,k2,n_bonds,i,a,n
      MAT(INT,:,:), allocatable :: pair
      MAT(STR(len=STR_SIZE),:,:), allocatable :: labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash
      STR(len=STR_SIZE) :: s1,s2,val
      VEC(REAL,:), allocatable :: dist,cAB,iAB,tAB,pAB,aAB
      BIN :: has_unique_labels
   ENSURE(NOT is_homoleptic_(self),"ROBY:put_bond_indices_do_H ... job is homoleptic")

      ! Anything to do?
      if (size(self%atom)<2) return
      no_of_bonds = no_of_bonds_(self%atom,self%bond_range_factor)
      if (no_of_bonds<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self%atom)
      DIE_IF(NOT has_unique_labels,"ROBY:put_bond_indices_do_H ... atoms are not uniquely labelled!")

      ! Redo decimal places
      call save_style_(stdout)
      call set_real_precision_(stdout,2)
      call redirect_(stdout,"rgbi-bondtable+H.tex")

      ! Get atom Z list
      call make_atom_Z_list_(self%atom,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self%atom(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = k1,n_kind

         ! Get the bonded atoms per kind
            if (self%analyze_vdw_atom_pairs) then
               call get_vdw_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,self%bond_range_factor)
            else
               call get_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,self%bond_range_factor)
            end if
            dist = dist*ANGSTROM_PER_BOHR

         ! No of bonds in this class
            n_bonds = size(dist)
            if (n_bonds==0) then
               call destroy_(dist)
               call destroy_(pair)
               cycle
            end if

         ! Dashes
            call create_(dash,n_bonds)
            dash = "--"

         ! Labels
            call create_(labels,n_bonds,2)
            labels(:,1) = self%atom(pair(:,1))%tag
            labels(:,2) = self%atom(pair(:,2))%tag

         ! Populations & indices
            call create_(cAB,n_bonds)
            call create_(iAB,n_bonds)
            call create_(tAB,n_bonds)
            call create_(pAB,n_bonds)
            call create_(aAB,n_bonds)
            forall (i=1:n_bonds)
               cAB(i) = self%cov_index(pair(i,1),pair(i,2))
               iAB(i) = self%ion_index(pair(i,1),pair(i,2))
               tAB(i) = self%bnd_index(pair(i,1),pair(i,2))
               pAB(i) = self%percent_covalency_P(pair(i,1),pair(i,2))
               aAB(i) = self%percent_covalency_A(pair(i,1),pair(i,2))
            end forall

         ! Put latex table
            s1 = chemical_symbol_(self%atom(pair(1,1)))
            s2 = chemical_symbol_(self%atom(pair(1,2)))
            call text_(stdout,"% "//trim(s1)//"--"//trim(s2)//" bonds")
            do a = 1,size(pair,1)
               call put_(stdout,"\namebond{a",width=11)
               val = to_str_(pair(a,1))
               call put_(stdout,val,width=len_trim(val))
               call put_(stdout,"}{a",width=3)
               val = to_str_(pair(a,2))
               call put_(stdout,val,width=len_trim(val))
               call put_(stdout,"}{",width=2)
               if (self%output_ci_latex_labels) then
                  val = to_str_(iAB(a),"f10.2",left_justify=TRUE)
               else
                  val = to_str_(tAB(a),"f10.2",left_justify=TRUE)
               end if
               call put_(stdout,val,width=len_trim(val))
               call put_(stdout,"}{",width=2)
               if (self%output_ci_latex_labels) then
                  val = to_str_(cAB(a),"f10.2",left_justify=TRUE)
               else
                  val = to_str_(nint(aAB(a)),"i3",left_justify=TRUE)
               end if
               call put_(stdout,val,width=len_trim(val))
               call put_(stdout,"}",width=1)
               call flush_(stdout)
            end do

         ! Clean
            call destroy_(aAB)
            call destroy_(pAB)
            call destroy_(tAB)
            call destroy_(iAB)
            call destroy_(cAB)
            call destroy_(labels)
            call destroy_(dash)
            call destroy_(dist)
            call destroy_(pair)
         end do
      end do

      ! Clean
      call destroy_(Z_list)

      ! Revert & unsave
      call revert_(stdout)
      call unsave_style_(stdout)

      ! New atom labels    
      call redirect_(stdout,"rgbi-atom-labels+H.tex")
      n = size(self%atom)
      call text_(stdout,"% New atom labels")
      call text_(stdout,"\pgfkeys{%")
      do a = 1,n
         call put_(stdout,"/atom no/"//trim(to_str_(a))//"/.code="//trim(to_str_(a)))
         if (a<n) then
            call put_(stdout,",",width=1)
         else
            call put_(stdout,"}",width=1)
         end if
         call flush_(stdout)
      end do
      call flush_(stdout)
      call text_(stdout,"% New atom label command")
      call text_(stdout,"\let\oldmcfatomno\mcfatomno")
      call text_(stdout,"\renewcommand{\mcfatomno}[1]{\oldmcfatomno{\pgfkeys{/atom no/#1}}}")

      ! Revert & unsave
      call revert_(stdout)
   end subroutine

   subroutine put_bond_indices_do_H_vdw(self)
   ! Output bond indices including H's and shortest vdw bond in LaTeX
   ! script "rgbi-bondtable+H+vdw.tex".
      type(ROBY_TYPE), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: no_of_bonds,n_kind,k1,k2,n_bonds,i,a
      MAT(INT,:,:), allocatable :: pair
      MAT(STR(len=STR_SIZE),:,:), allocatable :: labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash
      VEC(INT,2) :: vdw
      STR(len=STR_SIZE) :: s1,s2,val
      VEC(REAL,:), allocatable :: dist,cAB,iAB,tAB,pAB,aAB
      BIN :: has_unique_labels
   ENSURE(NOT is_homoleptic_(self),"ROBY:put_bond_indices_do_H_vdw ... job is homoleptic")

      ! Anything to do?
      if (size(self%atom)<2) return
      if (NOT has_vdw_bond_(self%atom)) return
      no_of_bonds = no_of_bonds_(self%atom,self%bond_range_factor)
      if (no_of_bonds<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self%atom)
      DIE_IF(NOT has_unique_labels,"ROBY:put_bond_indices_do_H_vdw ... atoms are not uniquely labelled!")

      ! Redo decimal places
      call save_style_(stdout)
      call set_real_precision_(stdout,2)
      call redirect_(stdout,"rgbi-bondtable+H+vdw.tex")

      ! Get atom Z list
      call make_atom_Z_list_(self%atom,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self%atom(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = k1,n_kind

         ! Get the bonded atoms per kind
            if (self%analyze_vdw_atom_pairs) then
               call get_vdw_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,self%bond_range_factor)
            else
               call get_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,self%bond_range_factor)
            end if
            dist = dist*ANGSTROM_PER_BOHR

         ! No of bonds in this class
            n_bonds = size(dist)
            if (n_bonds==0) then
               call destroy_(dist)
               call destroy_(pair)
               cycle
            end if

         ! Dashes
            call create_(dash,n_bonds)
            dash = "--"

         ! Labels
            call create_(labels,n_bonds,2)
            labels(:,1) = self%atom(pair(:,1))%tag
            labels(:,2) = self%atom(pair(:,2))%tag

         ! Populations & indices
            call create_(cAB,n_bonds)
            call create_(iAB,n_bonds)
            call create_(tAB,n_bonds)
            call create_(pAB,n_bonds)
            call create_(aAB,n_bonds)
            forall (i=1:n_bonds)
               cAB(i) = self%cov_index(pair(i,1),pair(i,2))
               iAB(i) = self%ion_index(pair(i,1),pair(i,2))
               tAB(i) = self%bnd_index(pair(i,1),pair(i,2))
               pAB(i) = self%percent_covalency_P(pair(i,1),pair(i,2))
               aAB(i) = self%percent_covalency_A(pair(i,1),pair(i,2))
            end forall

         ! Put latex table
            s1 = chemical_symbol_(self%atom(pair(1,1)))
            s2 = chemical_symbol_(self%atom(pair(1,2)))
            call text_(stdout,"% "//trim(s1)//"--"//trim(s2)//" bonds")
            do a = 1,size(pair,1)
               call put_(stdout,"\namebond{a",width=11)
               val = to_str_(pair(a,1))
               call put_(stdout,val,width=len_trim(val))
               call put_(stdout,"}{a",width=3)
               val = to_str_(pair(a,2))
               call put_(stdout,val,width=len_trim(val))
               call put_(stdout,"}{",width=2)
               val = to_str_(tAB(a),"f10.2",left_justify=TRUE)
               call put_(stdout,val,width=len_trim(val))
               call put_(stdout,"}{",width=2)
               val = to_str_(nint(aAB(a)),"i3",left_justify=TRUE)
               call put_(stdout,val,width=len_trim(val))
               call put_(stdout,"}",width=1)
               call flush_(stdout)
            end do

         ! Clean
            call destroy_(aAB)
            call destroy_(pAB)
            call destroy_(tAB)
            call destroy_(iAB)
            call destroy_(cAB)
            call destroy_(labels)
            call destroy_(dash)
            call destroy_(dist)
            call destroy_(pair)
         end do
      end do

      ! Clean
      call destroy_(Z_list)

      ! Now put the shortest vdw bond
      call text_(stdout,"% Shortest vdw bond")

      ! Get shortest vdw bond
      vdw = shortest_vdw_bond_(self%atom)

         ! Allocate & get
      call create_(tAB,1)
      call create_(aAB,1)
      tAB(1) = self%bnd_index(vdw(1),vdw(2))
      aAB(1) = self%percent_covalency_A(vdw(1),vdw(2))

            ! Print tabele
      call put_(stdout,"\namebond{a",width=11)
      val = to_str_(vdw(1))
      call put_(stdout,val,width=len_trim(val))
      call put_(stdout,"}{a",width=3)
      val = to_str_(vdw(2))
      call put_(stdout,val,width=len_trim(val))
      call put_(stdout,"}{",width=2)
      val = to_str_(tAB(1),"f10.2",left_justify=TRUE)
      call put_(stdout,val,width=len_trim(val))
      call put_(stdout,"}{",width=2)
      val = to_str_(nint(aAB(1)),"i3",left_justify=TRUE)
      call put_(stdout,val,width=len_trim(val))
      call put_(stdout,"}",width=1)
      call flush_(stdout)

         ! Clean
      call destroy_(aAB)
      call destroy_(tAB)

      ! Revert & unsave
      call revert_(stdout)
      call unsave_style_(stdout)
   end subroutine

   subroutine put_bond_indices_no_H(self)
   ! Output the non-H bond indices in LaTex "rgbi-bondtable-H.tex".
      type(ROBY_TYPE), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: no_of_bonds,n_kind,k1,k2,n_bonds,i,a,b,n,m
      MAT(INT,:,:), allocatable :: pair
      MAT(STR(len=STR_SIZE),:,:), allocatable :: labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash
      STR(len=STR_SIZE) :: s1,s2,val
      VEC(REAL,:), allocatable :: dist,cAB,iAB,tAB,pAB,aAB
      VEC(INT,:), allocatable :: non_H_atom_for_atom,non_H_atom
      BIN :: has_unique_labels
   ENSURE(NOT is_homoleptic_(self),"ROBY:put_bond_indices_no_H ... job is homoleptic")

      ! Anything to do?
      if (size(self%atom)<2) return
      no_of_bonds = no_of_bonds_(self%atom,self%bond_range_factor)
      if (no_of_bonds<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self%atom)
      DIE_IF(NOT has_unique_labels,"ROBY:put_bond_indices_no_H ... atoms are not uniquely labelled!")

      ! Redo decimal places
      call save_style_(stdout)
      call set_real_precision_(stdout,2)
      call redirect_(stdout,"rgbi-bondtable-H.tex")

      ! Get atom Z list
      call make_atom_Z_list_(self%atom,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self%atom(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Non-H atom indices - openbabel renumbers indices
      non_H_atom_for_atom = indices_of_non_H_atom_for_atom_(self%atom)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = k1,n_kind
            if (self%atom(Z_list(k1)%element(1))%atomic_number==1) cycle
            if (self%atom(Z_list(k2)%element(1))%atomic_number==1) cycle

         ! Get the bonded atoms per kind
            if (self%analyze_vdw_atom_pairs) then
               call get_vdw_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,self%bond_range_factor)
            else
               call get_bonded_atoms_(self%atom,pair,dist,Z_list,k1,k2,self%bond_range_factor)
            end if
            dist = dist*ANGSTROM_PER_BOHR

         ! No of bonds in this class
            n_bonds = size(dist)
            if (n_bonds==0) then
               call destroy_(dist)
               call destroy_(pair)
               cycle
            end if

         ! Dashes
            call create_(dash,n_bonds)
            dash = "--"

         ! Labels
            call create_(labels,n_bonds,2)
            labels(:,1) = self%atom(pair(:,1))%tag
            labels(:,2) = self%atom(pair(:,2))%tag

         ! Populations & indices
            call create_(cAB,n_bonds)
            call create_(iAB,n_bonds)
            call create_(tAB,n_bonds)
            call create_(pAB,n_bonds)
            call create_(aAB,n_bonds)
            forall (i=1:n_bonds)
               cAB(i) = self%cov_index(pair(i,1),pair(i,2))
               iAB(i) = self%ion_index(pair(i,1),pair(i,2))
               tAB(i) = self%bnd_index(pair(i,1),pair(i,2))
               pAB(i) = self%percent_covalency_P(pair(i,1),pair(i,2))
               aAB(i) = self%percent_covalency_A(pair(i,1),pair(i,2))
            end forall

         ! Put latex table
            s1 = chemical_symbol_(self%atom(pair(1,1)))
            s2 = chemical_symbol_(self%atom(pair(1,2)))
            call text_(stdout,"% "//trim(s1)//"--"//trim(s2)//" bonds")
            do a = 1,size(pair,1)
               call put_(stdout,"\namebond{a",width=11)
               b = non_H_atom_for_atom(pair(a,1))
               val = to_str_(b)
               call put_(stdout,val,width=len_trim(val))
               call put_(stdout,"}{a",width=3)
               b = non_H_atom_for_atom(pair(a,2))
               val = to_str_(b)
               call put_(stdout,val,width=len_trim(val))
               call put_(stdout,"}{",width=2)
               if (self%output_ci_latex_labels) then
                  val = to_str_(iAB(a),"f10.2",left_justify=TRUE)
               else
                  val = to_str_(tAB(a),"f10.2",left_justify=TRUE)
               end if
               call put_(stdout,val,width=len_trim(val))
               call put_(stdout,"}{",width=2)
               if (self%output_ci_latex_labels) then
                  val = to_str_(cAB(a),"f10.2",left_justify=TRUE)
               else
                  val = to_str_(nint(aAB(a)),"i3",left_justify=TRUE)
               end if
               call put_(stdout,val,width=len_trim(val))
               call put_(stdout,"}",width=1)
               call flush_(stdout)
            end do

         ! Clean
            call destroy_(aAB)
            call destroy_(pAB)
            call destroy_(tAB)
            call destroy_(iAB)
            call destroy_(cAB)
            call destroy_(labels)
            call destroy_(dash)
            call destroy_(dist)
            call destroy_(pair)
         end do
      end do

      ! Clean
      call destroy_(Z_list)

      ! Revert & unsave
      call revert_(stdout)
      call unsave_style_(stdout)

      ! New atom labels    
      non_H_atom = list_of_non_H_atoms_(self%atom)
      call redirect_(stdout,"rgbi-atom-labels-H.tex")
      n = size(self%atom)
      m = size(non_H_atom)
      call text_(stdout,"% New atom labels")
      call text_(stdout,"\pgfkeys{%")
      do b = 1,m
         a = non_H_atom(b)
         call put_(stdout,"/atom no/"//trim(to_str_(b))//"/.code="//trim(to_str_(a)))
         if (b<m) then
            call put_(stdout,",",width=1)
         else
            call put_(stdout,"}",width=1)
         end if
         call flush_(stdout)
      end do
      call flush_(stdout)
      call text_(stdout,"% New atom label command")
      call text_(stdout,"\let\oldmcfatomno\mcfatomno")
      call text_(stdout,"\renewcommand{\mcfatomno}[1]{\oldmcfatomno{\pgfkeys{/atom no/#1}}}")

      ! Revert & unsave
      call revert_(stdout)
   end subroutine


!   put_bond_indices_CC
!   ! Output the only CC bond indices table in file "bondtable-cc".
!      self :: IN
!
!   ENSURE(NOT .is_homoleptic,"job is homoleptic")
!
!      table :: VEC{TABLE_COLUMN}@
!      Z_list :: VEC{EVEC{INT}}@
!      no_of_bonds, n_kind,k1,k2,n_bonds, i :: INT
!      pair :: MAT{INT}@
!      labels :: MAT{STR}@
!      dist,cAB,iAB,tAB :: VEC{REAL}@
!      has_unique_labels :: BIN
!
!      ! Anything to do?
!      if (.atom.dim<2) return
!
!      no_of_bonds = .atom.no_of_bonds(.bond_range_factor)
!      if (no_of_bonds<1) return
!
!      ! Has unique labels?
!      has_unique_labels = .atom.has_unique_labels
!      DIE_IF(NOT has_unique_labels,"atoms are not uniquely labelled!")
!
!      ! Redo decimal places
!      stdout.save_style
!      stdout.set_real_precision(2)
!      stdout.redirect("bondtable-CC")
!
!      ! Get atom Z list
!      .atom.make_atom_Z_list(Z_list)
!
!      ! Skip dummy atoms
!      n_kind = Z_list.dim
!      if (.atom(Z_list(n_kind)[1]).atomic_number==0) n_kind = n_kind - 1
!
!      ! Create table
!      table.create(6)
!
!      ! Set the table headings
!      table(1).set_heading("A")
!      table(1).set_width_from(.atom(:).tag)
!
!      table(2).set_heading("B")
!      table(2).set_width(table(1).width)
!
!      table(3).set_heading(" Cov.")
!      table(3).set_subhead("index")
!      table(3).set_width_from(-TEN)
!
!      table(4).set_heading("Ionic")
!      table(4).set_subhead("index")
!      table(4).set_width_from(-TEN)
!
!      table(5).set_heading(" Bond")
!      table(5).set_subhead("index")
!      table(5).set_width_from(ONE)
!
!      ! Loop over atom kind classes
!      do k1 = 1,n_kind
!      do k2 = k1,n_kind
!
!         ! Skip non-C atoms
!         if (.atom(Z_list(k1)[1]).atomic_number/=6) cycle
!         if (.atom(Z_list(k2)[1]).atomic_number/=6) cycle
!
!         ! Get the bonded atoms per kind
!         .atom.get_bonded_atoms(pair,dist,Z_list,k1,k2,.bond_range_factor)
!         dist = dist*ANGSTROM_PER_BOHR
!
!         ! No of bonds in this class
!         n_bonds = dist.dim
!         if (n_bonds==0) then
!            dist.destroy
!            pair.destroy
!            cycle
!         end
!
!         ! Labels
!         labels.create(n_bonds,2)
!         labels(:,1) = .atom(pair(:,1)).tag
!         labels(:,2) = .atom(pair(:,2)).tag
!
!         ! Extract number-part only
!         labels(:,1) = labels(:,1)(2:)
!         labels(:,2) = labels(:,2)(2:)
!
!         ! Populations & indices
!         cAB.create(n_bonds)
!         iAB.create(n_bonds)
!         tAB.create(n_bonds)
!
!         forall (i=1:n_bonds)
!         cAB(i) = .cov_index(pair(i,1),pair(i,2))
!         iAB(i) = .ion_index(pair(i,1),pair(i,2))
!         tAB(i) = .bnd_index(pair(i,1),pair(i,2))
!         end
!
!         ! Set table data
!         table(1).create_values(labels(:,1))
!         table(2).create_values(labels(:,2))
!         table(3).create_values(cAB)
!         table(4).create_values(iAB)
!         table(5).create_values(tAB)
!
!         ! Put info
!         stdout.put(.atom.molecular_weight)
!         stdout.flush
!         stdout.put(tAB.dim)
!         stdout.flush
!         table.put_body(label_rows=FALSE)
!
!         ! Clean
!         table.clear_columns
!
!         tAB.destroy
!         iAB.destroy
!         cAB.destroy
!         labels.destroy
!
!         dist.destroy
!         pair.destroy
!
!      end
!      end
!
!      ! Clean
!      table.destroy
!      Z_list.destroy
!
!      ! Revert & unsave
!      stdout.revert
!      stdout.unsave_style
!
!   end

! ===============
! Output energies
! ===============

!   put_all_energy_info
!   ! Put to stdout the energy bond analysis
!      self :: IN
!
!      .put_basics
!      .put_energies
!     ! .put_ci_energies
!
!   end

!   put_energies
!   ! Put to stdout the only Roby atom energies
!      self :: IN
!
!   ENSURE(.atom.allocated,"no atom data")
!   ENSURE(.E1.allocated,"no atom energies exist")
!
!      table :: VEC{TABLE_COLUMN}@
!      symbols :: VEC{STR}@
!      labels  :: VEC{INT}@
!      e_scf,e_nuc,e_tot,e_prj :: REAL
!      P :: MAT{REAL}@
!      f,n_gr :: INT
!
!
!      stdout.flush
!      stdout.text("==================")
!      stdout.text("Roby atom energies")
!      stdout.text("==================")
!    ! stdout.flush
!    ! stdout.text(". Half of the nuclear-nuclear repulsion energies")
!    ! stdout.text("  for all other atoms are included in the total")
!      stdout.flush
!      stdout.text(". Energies are in atomic units")
!      stdout.flush
!
!      ! Get SCF energy from Krogel E_density_mx without projection
!      ! WARNING: restricted case only so far
!      e_scf = .E_expectation
!      e_prj = .atom_energy
!      e_nuc = .atom.nuclear_repulsion_energy
!      e_tot = e_scf + e_nuc
!
!      ! Make small P matrix
!    ! f = 1
!    ! labels => f.sequence_up_to(.n_group)
!    ! n_gr = .n_bf(labels)
!    ! P.create(n_gr,n_gr)
!    ! .make_projection_mx(P,labels)
!    ! e_prj = .E_expectation(P,labels)
!    ! labels.destroy
!    ! P.destroy
!
!    ! stdout.show("Total SCF energy          = ",e_tot)
!      stdout.show("SCF electronic energy       = ",e_scf)
!      stdout.show("Projected electronic energy = ",e_prj)
!      stdout.show("Nuclear repulsion energy    = ",e_nuc)
!      stdout.flush
!
!      ! Create table
!      table.create(2)
!
!      ! Set the table headings
!      table(1).set_heading("Atom")
!      table(1).set_width_from(.atom.chemical_symbols)
!
!      if (.is_homoleptic) then
!         symbols.create(.n_group)
!         labels.create(.n_group)
!         f = 1
!         labels => f.sequence_up_to(.n_group)
!         symbols = labels.to_str
!         labels.destroy
!      else
!         symbols => .unique_tags
!      end
!
!      ! Datas
!      table(1).set_values(symbols)
!      table(2).set_heading(" Energy")
!      table(2).set_subhead("    /au")
!      table(2).set_values(.E1)
!
!      ! Put table
!      table.put(label_rows=FALSE)
!
!      ! Clean
!      table.clear_columns
!      symbols.destroy
!
!   end

!   put_ci_energies
!   ! Output the covalent (shared) and ionic (transferred) energies.
!      self :: IN
!
!   ENSURE(NOT .is_homoleptic,"job is homoleptic")
!   ENSURE(.E1.allocated,"no atom energies exist")
!   ENSURE(.E2.allocated,"no atom pair energies exist")
!
!      table :: VEC{TABLE_COLUMN}@
!      Z_list :: VEC{EVEC{INT}}@
!      no_of_bonds, n_kind,k1,k2,n_bonds, i :: INT
!      pair :: MAT{INT}@
!      symbols,labels :: MAT{STR}@
!      dash,dsh1 :: VEC{STR}@
!      dist,n_A,n_B,nAB,sAB,cAB,iAB,tAB,pAB :: VEC{REAL}@
!      has_unique_labels :: BIN
!
!      ! Anything to do?
!      if (.atom.dim<2) return
!
!      no_of_bonds = .atom.no_of_bonds(.bond_range_factor)
!      if (no_of_bonds<1) return
!
!      ! Has unique labels?
!      has_unique_labels = .atom.has_unique_labels
!      DIE_IF(NOT has_unique_labels,"atoms are not uniquely labelled!")
!
!      ! Title
!      if (.analyze_vdw_atom_pairs) then
!      stdout.flush
!      stdout.text("==============================================")
!      stdout.text("Roby-Gould covalent & ionic energies: VDW only")
!      stdout.text("==============================================")
!      else
!      stdout.flush
!      stdout.text("====================================")
!      stdout.text("Roby-Gould covalent & ionic energies")
!      stdout.text("====================================")
!      end
!
!      stdout.flush
!      if (.analyze_vdw_atom_pairs) then
!      stdout.text("Energy bond indices appear only for VDW bonded atom")
!      stdout.text("pairs NOT normally bonded by the CSD criterion")
!      stdout.flush
!      stdout.show("No. of 'bonds'          =",no_of_bonds)
!      else if (.analyze_all_atom_pairs) then
!      stdout.text("Energy bond indices appear only for atom pairs connected")
!      stdout.text("according to the Cambridge Structural Database (CSD)")
!      stdout.text("AND if they are within a minimum bond_range_factor")
!      stdout.flush
!      stdout.show("Bond range factor /Angs =",.bond_range_factor.to_units("angstrom"))
!      stdout.show("No. of 'bonds'          =",no_of_bonds)
!      else
!      stdout.text("Energy bond indices appear only for atom pairs connected")
!      stdout.text("according to the Cambridge Structural Database (CSD).")
!      stdout.flush
!      stdout.show("No. of bonds =",no_of_bonds)
!      end
!      stdout.flush
!
!      ! Get atom Z list
!      .atom.make_atom_Z_list(Z_list)
!
!      ! Skip dummy atoms
!      n_kind = Z_list.dim
!      if (.atom(Z_list(n_kind)[1]).atomic_number==0) n_kind = n_kind - 1
!
!      ! Create table
!      table.create(11)
!
!      ! Set the table headings
!      table( 1).set_heading("A")
!      table( 1).set_width_from(.atom.chemical_symbols)
!
!      table( 2).set_spacing(0)
!      table( 2).set_heading("--")
!      table( 2).set_width(2)
!
!      table( 3).set_heading("B")
!      table( 3).set_width(table(1).width)
!      table( 3).set_left_justify(TRUE)
!
!      table( 4).set_heading("A")
!      table( 4).set_width_from(.atom(:).tag)
!
!      table( 5).set_spacing(0)
!      table( 5).set_heading("--")
!      table( 5).set_width(2)
!
!      table( 6).set_heading("B")
!      table( 6).set_width(table(4).width)
!      table( 6).set_left_justify(TRUE)
!
!      table( 7).set_heading("R(A--B)")
!      table( 7).set_subheading("/Angs.")
!      table( 7).set_width_from(ONE)
!
!      table( 8).set_heading("E_A")
!      table( 8).set_width_from(-100d0)
!      table( 9).set_heading("E_B")
!      table( 9).set_width_from(-100d0)
!      table(10).set_heading("E_AB")
!      table(10).set_width_from(-100d0)
!      table(11).set_heading("E^s_AB")
!      table(11).set_width_from(-100d0)
!
!      ! Put out the headings
!      table.put_headings(label_rows=FALSE,no_flush=TRUE)
!
!      ! Loop over atom kind classes
!      do k1 = 1,n_kind
!      do k2 = k1,n_kind
!
!         ! Get the bonded atoms per kind
!         if (.analyze_vdw_atom_pairs) then
!         .atom.get_vdw_bonded_atoms(pair,dist,Z_list,k1,k2,.bond_range_factor)
!         else
!         .atom.get_bonded_atoms(pair,dist,Z_list,k1,k2,.bond_range_factor)
!         end
!         dist = dist*ANGSTROM_PER_BOHR
!
!         ! No of bonds in this class
!         n_bonds = dist.dim
!         if (n_bonds==0) then
!            dist.destroy
!            pair.destroy
!            cycle
!         end
!
!         ! Element symbols
!         symbols.create(n_bonds,2)
!         symbols = " "
!         symbols(1,1) = .atom(Z_list(k1)[1]).chemical_symbol
!         symbols(1,2) = .atom(Z_list(k2)[1]).chemical_symbol
!
!         ! Dashes
!         dash.create(n_bonds)
!         dsh1.create(n_bonds)
!         dash    = "--"
!         dsh1    = "  "
!         dsh1(1) = "--"
!
!         ! Labels
!         labels.create(n_bonds,2)
!         labels(:,1) = .atom(pair(:,1)).tag
!         labels(:,2) = .atom(pair(:,2)).tag
!
!         ! Populations & indices
!         n_A.create(n_bonds)
!         n_B.create(n_bonds)
!         nAB.create(n_bonds)
!         sAB.create(n_bonds)
!
!         n_A = .E1(pair(:,1))
!         n_B = .E1(pair(:,2))
!         do i = 1,n_bonds
!         nAB(i) = .E2(pair(i,1),pair(i,2))
!         sAB(i) = n_A(i) + n_B(i) - nAB(i)
!         end
!
!         ! Set table data
!         table( 1).set_values(symbols(:,1))
!         table( 2).set_values(dsh1)
!         table( 3).set_values(symbols(:,2))
!         table( 4).set_values(labels(:,1))
!         table( 5).set_values(dash)
!         table( 6).set_values(labels(:,2))
!         table( 7).set_values(dist)
!         table( 8).set_values(n_A)
!         table( 9).set_values(n_B)
!         table(10).set_values(nAB)
!         table(11).set_values(sAB)
!
!         ! Put body
!         stdout.flush
!         table.put_body(label_rows=FALSE)
!
!         ! Clean
!         table.clear_columns
!
!         sAB.destroy
!         nAB.destroy
!         n_B.destroy
!         n_A.destroy
!         labels.destroy
!         dsh1.destroy
!         dash.destroy
!         symbols.destroy
!
!         dist.destroy
!         pair.destroy
!
!      end
!      end
!
!      ! Table footer
!      table.put_footer(label_rows=FALSE)
!
!      ! Clean
!      table.clear_columns
!      table.destroy
!      Z_list.destroy
!
!   end

!   put_energy_indices
!   ! Put to stdout the Roby bond indices, including the shared populations
!      self :: IN
!
!      if (.is_homoleptic) then
!
!         .put_energy_indices_group
!
!      else
!
!         .put_energy_indices_atoms
!         .put_energy_indices_sorted
!
!      end
!
!   end
!
!   put_energy_indices_group
!   ! Output the energy bond indices table for connected atoms.
!      self :: IN
!
!   ENSURE(.is_homoleptic,"job is not homoleptic")
!   ENSURE(.n_group>1,"must be at leat two groups")
!   ENSURE(.E1.allocated,"no atom energies exist")
!   ENSURE(.E2.allocated,"no atom pair energies exist")
!
!      table :: VEC{TABLE_COLUMN}@
!      symbol :: VEC{STR}@
!      g_A,g_B :: VEC{INT}@
!      n_A,n_B,nAB,sAB,cAB,iAB,tAB,pAB :: VEC{REAL}@
!      n_group, n_pair,i,a,b :: INT
!
!      n_group = .n_group
!
!      ! Title
!      stdout.flush
!      stdout.flush
!      stdout.text("==============================")
!      stdout.text("Roby-Gould energy bond indices")
!      stdout.text("==============================")
!      stdout.flush
!      stdout.text("Energy bond indices appear only between atom groups.")
!      stdout.flush
!      symbol => .unique_tags
!      do i = 1,n_group
!      stdout.show("Group "//trim(i.to_str(fmt="i2"))//" = ",symbol(i))
!      end
!      symbol.destroy
!      stdout.flush
!
!      ! Create table
!      table.create(10)
!
!      ! Set the table headings
!      table( 1).set_heading("Grp")
!      table( 1).set_subhead("  A")
!      table( 2).set_heading("Grp")
!      table( 2).set_subhead("  B")
!      table( 3).set_heading("E_A")
!      table( 3).set_width_from(TEN)
!      table( 4).set_heading("E_B")
!      table( 4).set_width_from(TEN)
!      table( 5).set_heading("E_AB")
!      table( 5).set_width_from(TEN)
!      table( 6).set_heading("E^s_AB")
!      table( 6).set_width_from(TEN)
!      table( 7).set_heading("E Cov")
!      table( 7).set_subhead("index")
!      table( 7).set_width_from(-TEN)
!      table( 8).set_heading("E Ion")
!      table( 8).set_subhead("index")
!      table( 8).set_width_from(-TEN)
!      table( 9).set_heading("E Bond")
!      table( 9).set_subhead(" index")
!      table( 9).set_width_from(ONE)
!      table(10).set_heading("% E")
!      table(10).set_subhead("Cov")
!      table(10).set_width_from(100d0)
!
!      ! Put out the headings
!      table.put_headings(label_rows=FALSE,no_flush=TRUE)
!
!      ! Populations & indices
!      n_pair = n_group - 1
!      n_pair = n_pair.triangle_number
!      g_A.create(n_pair)
!      g_B.create(n_pair)
!      n_A.create(n_pair)
!      n_B.create(n_pair)
!      nAB.create(n_pair)
!      sAB.create(n_pair)
!      cAB.create(n_pair)
!      iAB.create(n_pair)
!      tAB.create(n_pair)
!      pAB.create(n_pair)
!
!      i = 0
!      do a = 1,.n_group
!      do b = a+1,.n_group
!         i = i + 1
!         g_A(i) = a
!         g_B(i) = b
!         n_A(i) = .E1(a)
!         n_B(i) = .E1(b)
!         nAB(i) = .E2(a,b)
!         sAB(i) = .E1(a) + .E1(b) - .E2(a,b)
!         cAB(i) =  .E_cov_index(a,b)
!         iAB(i) =  .E_ion_index(a,b)
!         tAB(i) = .E_bond_index(a,b)
!         pAB(i) = .E_percent_covalency(a,b)
!      end
!      end
!
!      ! Set table data
!      table( 1).set_values(g_A)
!      table( 2).set_values(g_B)
!      table( 3).set_values(n_A)
!      table( 4).set_values(n_B)
!      table( 5).set_values(nAB)
!      table( 6).set_values(sAB)
!      table( 7).set_values(cAB)
!      table( 8).set_values(iAB)
!      table( 9).set_values(tAB)
!      table(10).set_values(pAB)
!
!      ! Put body
!      stdout.flush
!      table.put_body(label_rows=FALSE)
!
!      ! Clean
!      table.clear_columns
!
!      pAB.destroy
!      tAB.destroy
!      iAB.destroy
!      cAB.destroy
!      sAB.destroy
!      nAB.destroy
!      n_B.destroy
!      n_A.destroy
!
!      ! Table footer
!      table.put_footer(label_rows=FALSE)
!
!      ! Clean
!      table.clear_columns
!      table.destroy
!
!   end
!
!   put_energy_indices_atoms
!   ! Output the energy bond indices table for connected atoms.
!      self :: IN
!
!   ENSURE(NOT .is_homoleptic,"job is homoleptic")
!   ENSURE(.E1.allocated,"no atom energies exist")
!   ENSURE(.E2.allocated,"no atom pair energies exist")
!
!      table :: VEC{TABLE_COLUMN}@
!      Z_list :: VEC{EVEC{INT}}@
!      no_of_bonds, n_kind,k1,k2,n_bonds, i :: INT
!      pair :: MAT{INT}@
!      symbols,labels :: MAT{STR}@
!      dash,dsh1 :: VEC{STR}@
!      dist,n_A,n_B,nAB,sAB,cAB,iAB,tAB,pAB :: VEC{REAL}@
!      has_unique_labels :: BIN
!
!      ! Anything to do?
!      if (.atom.dim<2) return
!
!      no_of_bonds = .atom.no_of_bonds(.bond_range_factor)
!      if (no_of_bonds<1) return
!
!      ! Has unique labels?
!      has_unique_labels = .atom.has_unique_labels
!      DIE_IF(NOT has_unique_labels,"atoms are not uniquely labelled!")
!
!      ! Title
!      if (.analyze_vdw_atom_pairs) then
!      stdout.flush
!      stdout.text("========================================")
!      stdout.text("Roby-Gould energy bond indices: VDW only")
!      stdout.text("========================================")
!      else
!      stdout.flush
!      stdout.text("==============================")
!      stdout.text("Roby-Gould energy bond indices")
!      stdout.text("==============================")
!      end
!
!      stdout.flush
!      if (.analyze_vdw_atom_pairs) then
!      stdout.text("Energy bond indices appear only for VDW bonded atom")
!      stdout.text("pairs NOT normally bonded by the CSD criterion")
!      stdout.flush
!      stdout.show("No. of 'bonds'          =",no_of_bonds)
!      else if (.analyze_all_atom_pairs) then
!      stdout.text("Energy bond indices appear only for atom pairs connected")
!      stdout.text("according to the Cambridge Structural Database (CSD)")
!      stdout.text("AND if they are within a minimum bond_range_factor")
!      stdout.flush
!      stdout.show("Bond range factor /Angs =",.bond_range_factor.to_units("angstrom"))
!      stdout.show("No. of 'bonds'          =",no_of_bonds)
!      else
!      stdout.text("Energy bond indices appear only for atom pairs connected")
!      stdout.text("according to the Cambridge Structural Database (CSD).")
!      stdout.flush
!      stdout.show("No. of bonds =",no_of_bonds)
!      end
!      stdout.flush
!
!      ! Get atom Z list
!      .atom.make_atom_Z_list(Z_list)
!
!      ! Skip dummy atoms
!      n_kind = Z_list.dim
!      if (.atom(Z_list(n_kind)[1]).atomic_number==0) n_kind = n_kind - 1
!
!      ! Create table
!      table.create(15)
!
!      ! Set the table headings
!      table( 1).set_heading("A")
!      table( 1).set_width_from(.atom.chemical_symbols)
!
!      table( 2).set_column_spacing(0)
!      table( 2).set_heading("--")
!      table( 2).set_width(2)
!
!      table( 3).set_heading("B")
!      table( 3).set_width(table(1).width)
!      table( 3).set_left_justify(TRUE)
!
!      table( 4).set_heading("A")
!      table( 4).set_width_from(.atom(:).tag)
!
!      table( 5).set_column_spacing(0)
!      table( 5).set_heading("--")
!      table( 5).set_width(2)
!
!      table( 6).set_heading("B")
!      table( 6).set_width(table(4).width)
!      table( 6).set_left_justify(TRUE)
!
!      table( 7).set_heading("R(A--B)")
!      table( 7).set_subheading("/Angs.")
!      table( 7).set_width_from(ONE)
!
!      table( 8).set_heading("E_A")
!      table( 8).set_width_from(-100d0)
!      table( 9).set_heading("E_B")
!      table( 9).set_width_from(-100d0)
!      table(10).set_heading("E_AB")
!      table(10).set_width_from(-100d0)
!      table(11).set_heading("E^s_AB")
!      table(11).set_width_from(-100d0)
!      table(12).set_heading("E Cov")
!      table(12).set_subhead("index")
!      table(12).set_width_from(-100d0)
!      table(13).set_heading("E Ion")
!      table(13).set_subhead("index")
!      table(13).set_width_from(-100d0)
!      table(14).set_heading("E Bond")
!      table(14).set_subhead(" index")
!      table(14).set_width_from(-100d0)
!      table(15).set_heading("% E")
!      table(15).set_subhead("Cov")
!      table(15).set_width_from(100d0)
!
!      ! Put out the headings
!      table.put_headings(label_rows=FALSE,no_flush=TRUE)
!
!      ! Loop over atom kind classes
!      do k1 = 1,n_kind
!      do k2 = k1,n_kind
!
!         ! Get the bonded atoms per kind
!         if (.analyze_vdw_atom_pairs) then
!         .atom.get_vdw_bonded_atoms(pair,dist,Z_list,k1,k2,.bond_range_factor)
!         else
!         .atom.get_bonded_atoms(pair,dist,Z_list,k1,k2,.bond_range_factor)
!         end
!         dist = dist*ANGSTROM_PER_BOHR
!
!         ! No of bonds in this class
!         n_bonds = dist.dim
!         if (n_bonds==0) then
!            dist.destroy
!            pair.destroy
!            cycle
!         end
!
!         ! Element symbols
!         symbols.create(n_bonds,2)
!         symbols = " "
!         symbols(1,1) = .atom(Z_list(k1)[1]).chemical_symbol
!         symbols(1,2) = .atom(Z_list(k2)[1]).chemical_symbol
!
!         ! Dashes
!         dash.create(n_bonds)
!         dsh1.create(n_bonds)
!         dash    = "--"
!         dsh1    = "  "
!         dsh1(1) = "--"
!
!         ! Labels
!         labels.create(n_bonds,2)
!         labels(:,1) = .atom(pair(:,1)).tag
!         labels(:,2) = .atom(pair(:,2)).tag
!
!         ! Populations & indices
!         n_A.create(n_bonds)
!         n_B.create(n_bonds)
!         nAB.create(n_bonds)
!         sAB.create(n_bonds)
!         cAB.create(n_bonds)
!         iAB.create(n_bonds)
!         tAB.create(n_bonds)
!         pAB.create(n_bonds)
!
!         n_A = .E1(pair(:,1))
!         n_B = .E1(pair(:,2))
!         do i = 1,n_bonds
!         nAB(i) = .E2(pair(i,1),pair(i,2))
!         sAB(i) = n_A(i) + n_B(i) - nAB(i)
!         cAB(i) =  .E_cov_index(pair(i,1),pair(i,2))
!         iAB(i) =  .E_ion_index(pair(i,1),pair(i,2))
!         tAB(i) = .E_bond_index(pair(i,1),pair(i,2))
!         pAB(i) = .E_percent_covalency(pair(i,1),pair(i,2))
!         end
!
!         ! Set table data
!         table( 1).set_values(symbols(:,1))
!         table( 2).set_values(dsh1)
!         table( 3).set_values(symbols(:,2))
!         table( 4).set_values(labels(:,1))
!         table( 5).set_values(dash)
!         table( 6).set_values(labels(:,2))
!         table( 7).set_values(dist)
!         table( 8).set_values(n_A)
!         table( 9).set_values(n_B)
!         table(10).set_values(nAB)
!         table(11).set_values(sAB)
!         table(12).set_values(cAB)
!         table(13).set_values(iAB)
!         table(14).set_values(tAB)
!         table(15).set_values(pAB)
!
!         ! Put body
!         stdout.flush
!         table.put_body(label_rows=FALSE)
!
!         ! Clean
!         table.clear_columns
!
!         pAB.destroy
!         tAB.destroy
!         iAB.destroy
!         cAB.destroy
!         sAB.destroy
!         nAB.destroy
!         n_B.destroy
!         n_A.destroy
!         labels.destroy
!         dsh1.destroy
!         dash.destroy
!         symbols.destroy
!
!         dist.destroy
!         pair.destroy
!
!      end
!      end
!
!      ! Table footer
!      table.put_footer(label_rows=FALSE)
!
!      ! Clean
!      table.clear_columns
!      table.destroy
!      Z_list.destroy
!
!   end
!
!   put_energy_indices_sorted
!   ! Output the energy bond indices table for connected atoms.
!      self :: IN
!
!   ENSURE(NOT .is_homoleptic,"job is homoleptic")
!
!      table :: VEC{TABLE_COLUMN}@
!      Z_list :: VEC{EVEC{INT}}@
!      no_of_bonds, n_kind,k1,k2,n_bonds, i :: INT
!      pair :: MAT{INT}@
!      labels :: MAT{STR}@
!      dash :: VEC{STR}@
!      dist,cAB,iAB,tAB,pAB :: VEC{REAL}@
!      ind :: VEC{INT}@
!      has_unique_labels :: BIN
!
!      ! Anything to do?
!      if (.atom.dim<2) return
!
!      no_of_bonds = .atom.no_of_bonds(.bond_range_factor)
!      if (no_of_bonds<1) return
!
!      ! Has unique labels?
!      has_unique_labels = .atom.has_unique_labels
!      DIE_IF(NOT has_unique_labels,"atoms are not uniquely labelled!")
!
!      ! Title
!      if (.analyze_vdw_atom_pairs) then
!      stdout.flush
!      stdout.text("===============================================")
!      stdout.text("Sorted Roby-Gould energy bond indices: VDW only")
!      stdout.text("===============================================")
!      else
!      stdout.flush
!      stdout.text("=====================================")
!      stdout.text("Sorted Roby-Gould energy bond indices")
!      stdout.text("=====================================")
!      end
!
!      stdout.flush
!      stdout.text(". Energy bond indices are sorted from lowest to highest")
!      stdout.flush
!      if (.analyze_vdw_atom_pairs) then
!      stdout.text("Energy bond indices appear only for VDW bonded atom pairs")
!      stdout.text("and NOT normally bonded by the CSD criterion")
!      stdout.flush
!      stdout.show("No. of 'bonds'          =",no_of_bonds)
!      else if (.analyze_all_atom_pairs) then
!      stdout.text(". Energy bond indices appear only for atom pairs which connected")
!      stdout.text("  according to the Cambridge Structural Database (CSD)")
!      stdout.text("  AND if they are within a minimum bond_range_factor")
!      stdout.flush
!      stdout.show("Bond range factor /Angs =",.bond_range_factor.to_units("angstrom"))
!      stdout.show("No. of 'bonds'          =",no_of_bonds)
!      else
!      stdout.text(". Energy bond indices appear only for atom pairs which are connected")
!      stdout.text("  according to the Cambridge Structural Database (CSD).")
!      stdout.flush
!      stdout.show("No. of bonds =",no_of_bonds)
!      end
!      stdout.flush
!
!      ! Get atom Z list
!      .atom.make_atom_Z_list(Z_list)
!
!      ! Skip dummy atoms
!      n_kind = Z_list.dim
!      if (.atom(Z_list(n_kind)[1]).atomic_number==0) n_kind = n_kind - 1
!
!      ! Create table
!      table.create(8)
!
!      ! Set the table headings
!      table(1).set_heading("A")
!      table(1).set_width_from(.atom(:).tag)
!
!      table(2).set_column_spacing(0)
!      table(2).set_heading("--")
!      table(2).set_width(2)
!
!      table(3).set_heading("B")
!      table(3).set_width(table(1).width)
!      table(3).set_left_justify(TRUE)
!
!      table(4).set_heading("R(A--B)")
!      table(4).set_subheading("/Angs.")
!      table(4).set_width_from(ONE)
!
!      table(5).set_heading("E Cov")
!      table(5).set_subhead("index")
!      table(5).set_width_from(-TEN)
!      table(6).set_heading("E Ion")
!      table(6).set_subhead("index")
!      table(6).set_width_from(-TEN)
!      table(7).set_heading("E Bond")
!      table(7).set_subhead(" index")
!      table(7).set_width_from(ONE)
!      table(8).set_heading("% E")
!      table(8).set_subhead("Cov")
!      table(8).set_width_from(100d0)
!
!      ! Put out the headings
!      table.put_headings(label_rows=FALSE,no_flush=TRUE)
!
!      ! Loop over atom kind classes
!      do k1 = 1,n_kind
!      do k2 = k1,n_kind
!
!         ! Get the bonded atoms per kind
!         if (.analyze_vdw_atom_pairs) then
!         .atom.get_vdw_bonded_atoms(pair,dist,Z_list,k1,k2,.bond_range_factor)
!         else
!         .atom.get_bonded_atoms(pair,dist,Z_list,k1,k2,.bond_range_factor)
!         end
!         dist = dist*ANGSTROM_PER_BOHR
!
!         ! No of bonds in this class
!         n_bonds = dist.dim
!         if (n_bonds==0) then
!            dist.destroy
!            pair.destroy
!            cycle
!         end
!
!         ! Dashes
!         dash.create(n_bonds)
!         dash    = "--"
!
!         ! Labels
!         labels.create(n_bonds,2)
!         labels(:,1) = .atom(pair(:,1)).tag
!         labels(:,2) = .atom(pair(:,2)).tag
!
!         ! Populations & indices
!         cAB.create(n_bonds)
!         iAB.create(n_bonds)
!         tAB.create(n_bonds)
!         pAB.create(n_bonds)
!
!         do i  = 1,n_bonds
!         cAB(i) =  .E_cov_index(pair(i,1),pair(i,2))
!         iAB(i) =  .E_ion_index(pair(i,1),pair(i,2))
!         tAB(i) = .E_bond_index(pair(i,1),pair(i,2))
!         pAB(i) = .E_percent_covalency(pair(i,1),pair(i,2))
!         end
!
!         ! Sort & rearrange
!         ind.create(n_bonds)
!         tAB.quick_sort(ind,decreasing_order=TRUE)
!         labels(:,1) = labels(ind,1)
!         labels(:,2) = labels(ind,2)
!         dist = dist(ind)
!         cAB  = cAB(ind)
!         iAB  = iAB(ind)
!         tAB  = tAB(ind)
!         pAB  = pAB(ind)
!
!         ! Set table data
!         table(1).set_values(labels(:,1))
!         table(2).set_values(dash)
!         table(3).set_values(labels(:,2))
!         table(4).set_values(dist)
!         table(5).set_values(cAB)
!         table(6).set_values(iAB)
!         table(7).set_values(tAB)
!         table(8).set_values(pAB)
!
!         ! Put body
!         stdout.flush
!         table.put_body(label_rows=FALSE)
!
!         ! Clean
!         table.clear_columns
!
!         ind.destroy
!         pAB.destroy
!         tAB.destroy
!         iAB.destroy
!         cAB.destroy
!         labels.destroy
!         dash.destroy
!
!         dist.destroy
!         pair.destroy
!
!      end
!      end
!
!      ! Table footer
!      table.put_footer(label_rows=FALSE)
!
!      ! Clean
!      table.clear_columns
!      table.destroy
!      Z_list.destroy
!
!   end

! ==============
! Output charges
! ==============

   subroutine put_charges(self)
   ! Put to stdout the group charges.
      type(ROBY_TYPE), IN :: self
      INT :: a
      BIN :: homoleptic
      STR(len=STR_SIZE) :: symb
      VEC(STR(len=STR_SIZE),:), allocatable :: symbol
   ENSURE(allocated(self%gould_charge),"ROBY:put_charges ... no Roby-Gould charges")
   ENSURE(allocated(self%cruickshank_charge),"ROBY:put_charges ... no Cruickshank charges")
   ENSURE(allocated(self%atom),"ROBY:put_charges ... no atom data")
   ENSURE(allocated(self%atom_group),"ROBY:put_charges ... no atom group data")
      homoleptic = is_homoleptic_(self)
      if (NOT homoleptic) symbol = unique_tags_(self)
      call flush_(stdout)
      call text_(stdout,"Group charges:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Roby-Gould")
      call put_(stdout,"Cruickshank")
      call put_(stdout,"Sum_b  Sab/2")
      call put_(stdout,"Sum_bc Sabc/3")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      do a = 1,n_group_(self)
         if (homoleptic) then; symb = to_str_(a)
         else; symb = symbol(a)
         end if
         call put_(stdout,symb,int_width=TRUE)
         call put_(stdout,self%gould_charge(a))
         call put_(stdout,self%cruickshank_charge(a))
         call put_(stdout,self%summed_n2(a)/TWO)
         call put_(stdout,self%summed_n3(a)/THREE)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=4)
      if (NOT homoleptic) call destroy_(symbol)
   end subroutine

   subroutine put_dipole_moments(self)
   ! Dipole moments are calculated from the charges
      type(ROBY_TYPE), IN :: self
      INT :: a
      VEC(REAL,3) :: gd,cd
   ENSURE(allocated(self%gould_charge),"ROBY:put_dipole_moments ... no Roby-Gould charges")
   ENSURE(allocated(self%cruickshank_charge),"ROBY:put_dipole_moments ... no Cruickshank charges")
   ENSURE(allocated(self%atom),"ROBY:put_dipole_moments ... no atom data")
      gd = ZERO
      cd = ZERO
      do a = 1,n_group_(self)
         gd(:) = gd(:)+self%gould_charge(a)*self%atom(a)%position(:)
         cd(:) = cd(:)+self%cruickshank_charge(a)*self%atom(a)%position(:)
      end do

      ! Convert from AU to Debyes
      gd = gd*conversion_factor_(("debye"))
      cd = cd*conversion_factor_(("debye"))
      call flush_(stdout)
      call text_(stdout,"Dipole moments (in DEBYE):")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=5)
      call put_(stdout,"Charge type")
      call put_(stdout,"X")
      call put_(stdout,"Y")
      call put_(stdout,"Z")
      call put_(stdout,"Magnitude")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=5)
      call put_(stdout,"Roby-Gould")
      call put_(stdout,gd(1))
      call put_(stdout,gd(2))
      call put_(stdout,gd(3))
      call put_(stdout,sqrt(gd(1)**2+gd(2)**2+gd(3)**2))
      call flush_(stdout)
      call put_(stdout,"Cruickshank")
      call put_(stdout,cd(1))
      call put_(stdout,cd(2))
      call put_(stdout,cd(3))
      call put_(stdout,sqrt(cd(1)**2+cd(2)**2+cd(3)**2))
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=5)
   end subroutine

   subroutine put_unique_tags(self,group,name)
   ! Output atom labels for the atom indices given in "group"
      type(ROBY_TYPE), IN :: self
      VEC(INT,:), IN :: group
      STR(len=*), optional, IN :: name
      STR(len=STR_SIZE) :: symbol
      STR(len=STR_SIZE) :: label
      INT :: g
      label = " "
      if (present(name)) label = name
      call put_(stdout,"Atom "//trim(label)//" =")
      do g = 1,size(group)
         symbol = self%atom(group(g))%tag
         call put_(stdout,trim(symbol),width=len_trim(symbol)+1)
      end do
      call flush_(stdout)
   end subroutine


! =================
! Output theta info
! =================

   subroutine put_theta_info(self)
   ! Output Theta-space information
      type(ROBY_TYPE), IN :: self
      INT :: n,i,j
      VEC(REAL,:), allocatable :: cst,snt,ang
      VEC(INT,:), allocatable :: evn,par
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
   ENSURE(allocated(self%atom_a),"ROBY:put_theta_info ... No roby atom A")
   ENSURE(allocated(self%atom_b),"ROBY:put_theta_info ... No roby atom B")
   ENSURE(allocated(self%atom_ab),"ROBY:put_theta_info ... No roby atom AB")
      call flush_(stdout)
      call text_(stdout,"============================")
      call text_(stdout,"Roby-Gould angle eigenspaces")
      call text_(stdout,"============================")
      call flush_(stdout)
      if (NOT self%is_hyper_analysis) then
         call text_(stdout,". Analysis is for atoms:")
         call flush_(stdout)
         call put_unique_tags_(self,self%atom_a,"(A)")
         call put_unique_tags_(self,self%atom_b,"(B)")
      else
         call text_(stdout,". This is a hypervalent analysis")
         call flush_(stdout)
         call text_(stdout,". Analysis is for transfer into atom A from everything not in M")
         call flush_(stdout)
         call put_unique_tags_(self,self%atom_a,"(A)")
         call put_unique_tags_(self,self%atom_b,"(M)")
      end if
      call flush_(stdout)
      if (NOT self%is_hyper_analysis) then
         call text_(stdout,". States come in bonding-antibonding pairs classified by an angle.")
         call text_(stdout,"  Negativef high covalency have high overlap and low angle.")
         call text_(stdout,"  States of high ionicity have low overlap and high angle.")
      else
         call text_(stdout,". There are no covalent states, only pure ionic")
      end if
      call flush_(stdout)
      call text_(stdout,". For more on this bonding analysis please cite:")
      call text_(stdout,"  M.D. Gould et al. (2008) Theor. Chem. Acc. 119 pp. 275-290")
      n = size(self%eigen_I)
      call flush_(stdout)
      call show_(stdout,"No. of states =",n)

      ! Columns
      call create_(evn,n)
      call create_(par,n)
      call create_(ang,n)
      call create_(cst,n)
      call create_(snt,n)

      ! Set column data
      j = 0
      do i = 1,n
         if (self%pair(i)<1) cycle
         if (self%eigen_I(i)<self%eigen_I(self%pair(i))) cycle
         j = j+1
         evn(j) = i
         par(j) = self%pair(i)
         cst(j) = self%eigen_C(i)
         snt(j) = self%eigen_I(i)
      ! ang(j) = atan2(.eigen_I(i),.eigen_C(i))*DEGREE_PER_RADIAN
         ang(j) = self%theta_angle(i)
      end do
      n = j
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"=====================")
      call text_(stdout,"Araki angle subspaces")
      call text_(stdout,"=====================")
      call flush_(stdout)

      ! Do table
      call create_(table,5)
      call set_heading_(table(1),"Bond")
      call set_subhead_(table(1),"orb.")
      call set_values_(table(1),evn(1:n))
      call set_heading_(table(2),"Anti")
      call set_subhead_(table(2),"bond")
      call set_values_(table(2),par(1:n))
      call set_heading_(table(3),"theta")
      call set_subhead_(table(3)," /deg")
      call set_values_(table(3),ang(1:n))
      call set_heading_(table(4),"  cos")
      call set_subhead_(table(4),"theta")
      call set_values_(table(4),cst(1:n))
      call set_heading_(table(5),"  sin")
      call set_subhead_(table(5),"theta")
      call set_values_(table(5),snt(1:n))
      call put_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(snt)
      call destroy_(cst)
      call destroy_(ang)
      call destroy_(par)
      call destroy_(evn)
   end subroutine

   subroutine put_theta_BI_info(self)
   ! Output theta-space bond index information
      type(ROBY_TYPE), IN :: self
      INT :: n,i,j,k
      VEC(REAL,:), allocatable :: ang
      VEC(STR(len=STR_SIZE),:), allocatable :: par
      VEC(INT,:), allocatable :: ind
      VEC(REAL,:), allocatable :: c_p,c_m,cov,i_p,i_m,ion,bi
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table

      ! Columns
      n = size(self%eigen_I)
      call create_(par,n)
      call create_(ang,n)
      call create_(c_p,n)
      call create_(c_m,n)
      call create_(cov,n)
      call create_(i_p,n)
      call create_(i_m,n)
      call create_(ion,n)
      call create_(bi,n)

      ! Sort decreasing covalent index
      call create_(ind,n)
      call quick_sort_(self%theta_cov_index,ind,decreasing_order=TRUE)

      ! Set column data
      j = 0
      do k = 1,n

         ! Reordered index
         i = ind(k)
         if (self%pair(i)<1) cycle
         if (self%eigen_I(i)<self%eigen_I(self%pair(i))) cycle
         j = j+1
         if (self%pair(i)/=i) then
            par(j) = "("//trim(to_str_(i))//","//trim(to_str_(self%pair(i)))//")"
            i_p(j) = self%theta_pop_I(i)
            i_m(j) = self%theta_pop_I(self%pair(i))
         else
            par(j) = "("//trim(to_str_(i))//")"
            if (self%eigen_I(i)>ZERO) then
               i_p(j) = self%theta_pop_I(i)
               i_m(j) = ZERO
            else if (self%eigen_I(i)<ZERO) then
               i_p(j) = ZERO
               i_m(j) = self%theta_pop_I(i)
            end if
         end if
         c_p(j) = self%theta_pop_C(i)
         c_m(j) = self%theta_pop_C(self%pair(i))
         cov(j) = self%theta_cov_index(i)
         ion(j) = self%theta_ion_index(i)
         ang(j) = self%theta_angle(i)
      ! ang(j) = atan2(.eigen_I(i),.eigen_C(i))*DEGREE_PER_RADIAN
         bi(j) = sqrt(cov(j)*cov(j)+ion(j)*ion(j))
      end do
      n = j

      ! Change decimal places
      call save_style_(stdout)
      call set_real_precision_(stdout,3)
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"========================================")
      call text_(stdout,"Covalent/ionic bond indices per subspace")
      call text_(stdout,"========================================")
      call flush_(stdout)
      call text_(stdout,". +/- means: bonding /antibonding RG bond orbitals.")
      call text_(stdout,"  C/I means: covalent/ionic       RG bond orbitals.")
      call text_(stdout,"  Thus I- represents an ionic antibonding orbital.")
      call flush_(stdout)
      call text_(stdout,". A +ve value for the ionic bond index in bond AB")
      call text_(stdout,"  means a polarity toward atom (or group) A.")
      call flush_(stdout)
      call text_(stdout,". Araki angles are associated with maximum overlap on")
      call text_(stdout,"  the two centres (or groups). Thus angles close to 0")
      call text_(stdout,"  have large overlap and are highly 'covalent' bonds,")
      call text_(stdout,"  while angles near 90 represent highly 'ionic' bonds.")
      call text_(stdout,"  They are not the angles formed in the dial diagram!")
      call flush_(stdout)

      ! Do table
      call create_(table,9)
      call set_heading_(table(1),"Paired")
      call set_subhead_(table(1)," space")
      call set_values_(table(1),par(1:n))
      call set_heading_(table(2),"Araki")
      call set_subhead_(table(2),"angle")
      call set_sb3head_(table(2)," /deg")
      call set_values_(table(2),ang(1:n))
      call set_heading_(table(3)," C+")
      call set_subhead_(table(3),"pop")
      call set_values_(table(3),c_p(1:n))
      call set_heading_(table(4)," C-")
      call set_subhead_(table(4),"pop")
      call set_values_(table(4),c_m(1:n))
      call set_heading_(table(5)," Cov.")
      call set_subhead_(table(5),"index")
      call set_values_(table(5),cov(1:n))
      call set_heading_(table(6)," I+")
      call set_subhead_(table(6),"pop")
      call set_values_(table(6),i_p(1:n))
      call set_heading_(table(7)," I-")
      call set_subhead_(table(7),"pop")
      call set_values_(table(7),i_m(1:n))
      call set_heading_(table(8)," Ion.")
      call set_subhead_(table(8),"index")
      call set_values_(table(8),ion(1:n))
      call set_heading_(table(9),"Bond")
      call set_subhead_(table(9),"index")
      call set_values_(table(9),bi(1:n))
      call put_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(ind)
      call destroy_(bi)
      call destroy_(ion)
      call destroy_(i_m)
      call destroy_(i_p)
      call destroy_(cov)
      call destroy_(c_m)
      call destroy_(c_p)
      call destroy_(ang)
      call destroy_(par)

      ! Print theta_C if only two groups e.g. for plots
   ! if (.n_group==2) then

   !    stdout.flush
   !    stdout.flush
   !    stdout.text("Roby-Gould covalent orbitals:")
   !    stdout.flush
   !    .atom(.atom_ab).put_orbitals(.theta_C(:,[1,.pair(1)]),pair=TRUE)
   !
   !    stdout.flush
   !    stdout.flush
   !    stdout.text("Roby-Gould ionic orbitals:")
   !    stdout.flush
   !    .atom(.atom_ab).put_orbitals(.theta_I(:,[1,.pair(1)]),pair=TRUE)

   ! end

      ! Change decimal places
      call unsave_style_(stdout)
   end subroutine

   subroutine put_dial_BI_info_do_H(self,a,b,file,num,reverse)
   ! Output the dial diagram tex script.
   ! "num" in the incrementing number of dial diagrams.
   ! "reverse" is set true to reverse the atom order.
   ! Note: the radius of the dial is normalized to unity.
      type(ROBY_TYPE), IN :: self
      INT, IN :: a,b,num
      type(TEXTFILE_TYPE), INOUT :: file
      BIN, optional, IN :: reverse
      INT :: n,i,j,k
      STR(len=1) :: space
      VEC(INT,:), allocatable :: ind
      STR(len=STR_SIZE) :: filename,atom_a,atom_b,ci_list,dial
      REAL :: tau,cov,ion,bc,bi
      BIN :: swap

      ! Get indices
      tau = self%bnd_index(a,b)
      cov = self%cov_index(a,b)
      ion = self%ion_index(a,b)

      ! Swap?
      swap = FALSE
      if (present(reverse)) swap = reverse
      if (ion<-0.1) swap = TRUE
      if (swap) ion = -ion

      ! Columns
      n = size(self%eigen_I)

      ! Sort decreasing covalent index
      call create_(ind,n)
      call quick_sort_(self%theta_cov_index,ind,decreasing_order=TRUE)

      ! Make the list part of the dial diagram
      j = 0
      space = " "
      ci_list = " "
      do k = 1,n
         i = ind(k)

         ! Skip lone pairs & skip antibonding 
         if (self%pair(i)<1) cycle  ! nonsense
         if (self%pair(i)==i) cycle
         if (self%eigen_I(i)<self%eigen_I(self%pair(i))) cycle
         j = j+1

         ! Theta bond indices; make bi +ve
         bc = self%theta_cov_index(i)/tau
         bi = self%theta_ion_index(i)/tau
         if (swap) bi = -bi

         ! List part of dial diagram
         if (j>1) then
            ci_list = trim(ci_list)//space//trim(to_str_(bc,"f7.3",left_justify=TRUE))//"/"//trim(to_str_(bi,"f7.3",left_justify=TRUE))//","
         else
            ci_list = trim(ci_list)//trim(to_str_(bc,"f7.3",left_justify=TRUE))//"/"//trim(to_str_(bi,"f7.3",left_justify=TRUE))//","
         end if
      end do
      ci_list = ci_list(1:len_trim(ci_list)-1)
      call destroy_(ind)

      ! Atom tags
      if (NOT swap) then
         atom_a = numbered_tag_(self,self%atom_a)
         atom_b = numbered_tag_(self,self%atom_b)
      else
         atom_b = numbered_tag_(self,self%atom_a)
         atom_a = numbered_tag_(self,self%atom_b)
      end if

      ! Write the dial diagram
      dial = "\dialdiagram"
      dial = trim(dial)//"{"//trim(atom_a)//"}"
      dial = trim(dial)//"{"//trim(atom_b)//"}"
      dial = trim(dial)//"{"//trim(to_str_(tau,"f7.2",left_justify=TRUE))//"}"
      dial = trim(dial)//"{"//trim(to_str_(cov,"f7.2",left_justify=TRUE))//"}"
      dial = trim(dial)//"{"//trim(to_str_(ion,"f7.2",left_justify=TRUE))//"}"
      dial = trim(dial)//"{"//trim(ci_list)//"}"
      dial = trim(dial)//"{2}"
      call text_(file,"\dialbox{"//trim(dial)//"}\hspace{0pt}%")

      ! Individual dial file ... +H case
      filename = adjustl(to_str_(num))//"+H.tex"
      call redirect_(stdout,filename)
      call text_(stdout,trim(dial))
      call revert_(stdout)
   end subroutine

   subroutine put_dial_BI_info_no_H(self,a,b,file,num,reverse)
   ! Output the dial diagram tex script.
   ! "num" in the incrementing number of dial diagrams.
   ! "reverse" is set true to reverse the atom order.
   ! Note: the radius of the dial is normalized to unity.
      type(ROBY_TYPE), IN :: self
      INT, IN :: a,b,num
      type(TEXTFILE_TYPE), INOUT :: file
      BIN, optional, IN :: reverse
      INT :: n,i,j,k
      STR(len=1) :: space
      VEC(INT,:), allocatable :: ind
      STR(len=STR_SIZE) :: filename,atom_a,atom_b,ci_list,dial
      REAL :: tau,cov,ion,bc,bi
      BIN :: swap

      ! Skip
      if (is_a_H_atom_(self%atom(a))) return
      if (is_a_H_atom_(self%atom(b))) return

      ! Get indices
      tau = self%bnd_index(a,b)
      cov = self%cov_index(a,b)
      ion = self%ion_index(a,b)

      ! Swap?
      swap = FALSE
      if (present(reverse)) swap = reverse
      if (ion<-0.1) swap = TRUE
      if (swap) ion = -ion

      ! Columns
      n = size(self%eigen_I)

      ! Sort decreasing covalent index
      call create_(ind,n)
      call quick_sort_(self%theta_cov_index,ind,decreasing_order=TRUE)

      ! Make the list part of the dial diagram
      j = 0
      space = " "
      ci_list = " "
      do k = 1,n
         i = ind(k)

         ! Skip lone pairs & skip antibonding 
         if (self%pair(i)<1) cycle  ! nonsense
         if (self%pair(i)==i) cycle
         if (self%eigen_I(i)<self%eigen_I(self%pair(i))) cycle
         j = j+1

         ! Theta bond indices; make bi +ve
         bc = self%theta_cov_index(i)/tau
         bi = self%theta_ion_index(i)/tau
         if (swap) bi = -bi

         ! List part of dial diagram
         if (j>1) then
            ci_list = trim(ci_list)//space//trim(to_str_(bc,"f7.3",left_justify=TRUE))//"/"//trim(to_str_(bi,"f7.3",left_justify=TRUE))//","
         else
            ci_list = trim(ci_list)//trim(to_str_(bc,"f7.3",left_justify=TRUE))//"/"//trim(to_str_(bi,"f7.3",left_justify=TRUE))//","
         end if
      end do
      ci_list = ci_list(1:len_trim(ci_list)-1)
      call destroy_(ind)

      ! Atom tags
      if (NOT swap) then
         atom_a = numbered_tag_(self,self%atom_a)
         atom_b = numbered_tag_(self,self%atom_b)
      else
         atom_b = numbered_tag_(self,self%atom_a)
         atom_a = numbered_tag_(self,self%atom_b)
      end if

      ! Write the dial diagram
      dial = "\dialdiagram"
      dial = trim(dial)//"{"//trim(atom_a)//"}"
      dial = trim(dial)//"{"//trim(atom_b)//"}"
      dial = trim(dial)//"{"//trim(to_str_(tau,"f7.2",left_justify=TRUE))//"}"
      dial = trim(dial)//"{"//trim(to_str_(cov,"f7.2",left_justify=TRUE))//"}"
      dial = trim(dial)//"{"//trim(to_str_(ion,"f7.2",left_justify=TRUE))//"}"
      dial = trim(dial)//"{"//trim(ci_list)//"}"
      dial = trim(dial)//"{2}"
      call text_(file,"\dialbox{"//trim(dial)//"}\hspace{0pt}%")

      ! Individual dial file ... +H case
      filename = adjustl(to_str_(num))//"-H.tex"
      call redirect_(stdout,filename)
      call text_(stdout,trim(dial))
      call revert_(stdout)
   end subroutine

   subroutine put_dial_table_do_H(n_bonds,n_columns)
   ! Write rgbi-dial-table+H.tex, the survey page of dial diagrams for the
   ! "n_bonds" bonds, "n_columns" to a row. The page is a flow of fixed-width
   ! boxes, one per bond, and rgbi-dial-header.tex makes the page exactly
   ! "n_columns" boxes wide, so LaTeX breaks the rows itself; nothing here
   ! counts rows or columns.
      INT, IN :: n_bonds,n_columns
      INT :: c
      call redirect_(stdout,"rgbi-dial-table+H.tex")
      call text_(stdout,"\def\ndialcolumns{"//trim(to_str_(n_columns))//"}")
      call text_(stdout,"\input{rgbi-dial-header}")
      call text_(stdout,"\begin{document}")
      call text_(stdout,"\noindent")
      do c = 1,n_bonds
         call text_(stdout,"\dialbox{\input{"//trim(to_str_(c))//"+H}}\hspace{0pt}%")
      end do
      call text_(stdout,"\end{document}")
      call revert_(stdout)
   end subroutine

   subroutine put_dial_table_no_H(n_bonds,n_columns)
   ! Write rgbi-dial-table-H.tex, the survey page of dial diagrams for the
   ! "n_bonds" non-hydrogen bonds, "n_columns" to a row. The page is a flow of fixed-width
   ! boxes, one per bond, and rgbi-dial-header.tex makes the page exactly
   ! "n_columns" boxes wide, so LaTeX breaks the rows itself; nothing here
   ! counts rows or columns.
      INT, IN :: n_bonds,n_columns
      INT :: c
      call redirect_(stdout,"rgbi-dial-table-H.tex")
      call text_(stdout,"\def\ndialcolumns{"//trim(to_str_(n_columns))//"}")
      call text_(stdout,"\input{rgbi-dial-header}")
      call text_(stdout,"\begin{document}")
      call text_(stdout,"\noindent")
      do c = 1,n_bonds
         call text_(stdout,"\dialbox{\input{"//trim(to_str_(c))//"-H}}\hspace{0pt}%")
      end do
      call text_(stdout,"\end{document}")
      call revert_(stdout)
   end subroutine

   subroutine put_theta_AB_pops(self)
   ! Output theta-space A/B populations information
   ! NOTE: functionality removed ...
      type(ROBY_TYPE), IN :: self
      INT :: n,i,j,k
      VEC(REAL,:), allocatable :: ang
      VEC(STR(len=STR_SIZE),:), allocatable :: par
      VEC(INT,:), allocatable :: ind
      VEC(REAL,:), allocatable :: cpa,cpb,cma,cmb
      VEC(REAL,:), allocatable :: ipa,ipb,ima,imb
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table

      ! Columns
      n = size(self%eigen_I)
      call create_(par,n)
      call create_(ang,n)
      call create_(cpa,n); cpa = ZERO
      call create_(cpb,n); cpb = ZERO
      call create_(cma,n); cma = ZERO
      call create_(cmb,n); cmb = ZERO
      call create_(ipa,n); ipa = ZERO
      call create_(ipb,n); ipb = ZERO
      call create_(ima,n); ima = ZERO
      call create_(imb,n); imb = ZERO
      call create_(ind,n)

      ! Sort decreasing covalent index
      call create_(ind,n)
      call quick_sort_(self%theta_cov_index,ind,decreasing_order=TRUE)

      ! Set column data
      j = 0
      do k = 1,n
         i = ind(k)
         if (self%pair(i)<1) cycle
         if (self%eigen_I(i)<self%eigen_I(self%pair(i))) cycle
         j = j+1
         if (self%pair(i)/=i) then
            par(j) = "("//trim(to_str_(i))//","//trim(to_str_(self%pair(i)))//")"
            ipa(j) = self%theta_pop_I_A(i)
            ipb(j) = self%theta_pop_I_B(i)
            ima(j) = self%theta_pop_I_A(self%pair(i))
            imb(j) = self%theta_pop_I_B(self%pair(i))
         else
            par(j) = "("//trim(to_str_(i))//")"
            if (self%eigen_I(i)>ZERO) then
               ipa(j) = self%theta_pop_I_A(i)
               ipb(j) = self%theta_pop_I_B(i)
            else if (self%eigen_I(i)<ZERO) then
               ima(j) = self%theta_pop_I_A(i)
               imb(j) = self%theta_pop_I_B(i)
            end if
         end if
         ang(j) = self%theta_angle(i)
         cpa(j) = self%theta_pop_C_A(i)
         cpb(j) = self%theta_pop_C_B(i)
         cma(j) = self%theta_pop_C_A(self%pair(i))
         cmb(j) = self%theta_pop_C_B(self%pair(i))
      ! ang(j) = atan2(.theta_ion_index(i),.theta_cov_index(i))*DEGREE_PER_RADIAN
         ang(j) = self%theta_angle(i)
      end do
      n = j

      ! Change decimal places
      call save_style_(stdout)
      call set_real_precision_(stdout,3)
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"Araki angle subspace A & B populations:")
      call flush_(stdout)

      ! Do table
      call create_(table,10)
      call set_heading_(table(1),"Paired")
      call set_subhead_(table(1)," space")
      call set_values_(table(1),par(1:n))
      call set_heading_(table(2),"theta")
      call set_subhead_(table(2)," /deg")
      call set_values_(table(2),ang(1:n))
      call set_heading_(table(3),"C+a")
      call set_subhead_(table(3),"pop")
      call set_values_(table(3),cpa(1:n))
      call set_heading_(table(4),"C+b")
      call set_subhead_(table(4),"pop")
      call set_values_(table(4),cpb(1:n))
      call set_heading_(table(5),"C-a")
      call set_subhead_(table(5),"pop")
      call set_values_(table(5),cma(1:n))
      call set_heading_(table(6),"C-b")
      call set_subhead_(table(6),"pop")
      call set_values_(table(6),cmb(1:n))
      call set_heading_(table(7),"I+a")
      call set_subhead_(table(7),"pop")
      call set_values_(table(7),ipa(1:n))
      call set_heading_(table(8),"I+b")
      call set_subhead_(table(8),"pop")
      call set_values_(table(8),ipb(1:n))
      call set_heading_(table(9),"I-a")
      call set_subhead_(table(9),"pop")
      call set_values_(table(9),ima(1:n))
      call set_heading_(table(10),"I-b")
      call set_subhead_(table(10),"pop")
      call set_values_(table(10),imb(1:n))
      call put_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(ind)
      call destroy_(imb)
      call destroy_(ima)
      call destroy_(ipb)
      call destroy_(ipa)
      call destroy_(cmb)
      call destroy_(cma)
      call destroy_(cpb)
      call destroy_(cpa)
      call destroy_(ang)
      call destroy_(par)

      ! Change decimal places
      call unsave_style_(stdout)
   end subroutine

   PURE function n_paired_angles(self) result (res)
   ! Output Roby angles and populations
      type(ROBY_TYPE), IN :: self
      INT :: res
      INT :: i
   ENSURE(allocated(self%pair),"ROBY:n_paired_angles ... No pair info")
   ENSURE(allocated(self%eigen_C),"ROBY:n_paired_angles ... No eigenvalues")
   ENSURE(allocated(self%theta_angle),"ROBY:n_paired_angles ... No theta angles")
      res = 0
      do i = 1,n_bf_ab_(self)
         if (self%pair(i)<1) cycle
         if (self%pair(i)==i) cycle
         res = res+1
      end do
   end function

   subroutine put_shared_population(self)
   ! Put to stdout the multiple shared Roby atom populations
      type(ROBY_TYPE), IN :: self
      INT :: a
      VEC(STR(len=STR_SIZE),:), allocatable :: symbol
   ENSURE(allocated(self%subgroup_pop),"ROBY:put_shared_population ... no subgroup population totals exist")
   ENSURE(allocated(self%atom),"ROBY:put_shared_population ... no atom data")
   ENSURE(allocated(self%atom_group),"ROBY:put_shared_population ... no atom group data")
      call flush_(stdout)
      call text_(stdout,"===============================")
      call text_(stdout,"Roby multiple shared population")
      call text_(stdout,"===============================")
      call flush_(stdout)
      call show_(stdout,"Multiple shared population =",self%n_shared)
      call text_(stdout,"Contributions by subgroup size:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"Subgrp size",int_width=TRUE)
      call put_(stdout,"Subgrp Pop.")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      do a = 1,n_group_(self)
         call put_(stdout,symbol(a),int_width=TRUE)
         call put_(stdout,self%subgroup_pop(a))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=1)
      call destroy_(symbol)
   end subroutine

end module
