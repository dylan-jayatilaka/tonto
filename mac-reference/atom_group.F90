!=====================================================================
!
! ATOM_GROUP:
!
! For storing the lists of integers which make up separate atom 
! groups within a list of atoms.
!
! Copyright (C) Dylan Jayatilaka, June 2011
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
!=====================================================================
module ATOM_GROUP_MODULE

#  include "macros"

   include "atom_group.use"

   implicit none

   include "atom_group.int"

   VEC(STR(len=STR_SIZE),:), allocatable, private :: keys

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(ATOM_GROUP_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"ATOM_GROUP:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(ATOM_GROUP_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"ATOM_GROUP:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   subroutine update(self,atom)
   ! Do all the sanity checks and default settings
      type(ATOM_GROUP_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
   DIE_IF(size(atom)==0,"ATOM_GROUP:update ... no atoms")

      ! Set the atom information
      call set_atom_(self,atom)

      ! Ensure .atom_index is defined and OK
      DIE_IF(NOT allocated(self%atom_index),"ATOM_GROUP:update ... no atom_index in the group")
      DIE_IF(size(self%atom_index)>size(self%atom),"ATOM_GROUP:update ... more atom_index than # of atoms!")
      DIE_IF(has_repetitions_(self%atom_index),"ATOM_GROUP:update ... there are repeated atom_index!")
      DIE_IF(any(self%atom_index<=0),"ATOM_GROUP:update ... non-positive atom atom_index!")
      DIE_IF(any(self%atom_index>size(self%atom)),"ATOM_GROUP:update ... index too large")
   end subroutine


!  ===
!  Set
!  ===

   pure subroutine set_defaults(self)
   ! Set up defaults
      type(ATOM_GROUP_TYPE), INOUT :: self
      self%name = " "
      self%n_atoms = 0
      self%charge = 0
      self%MO_file_name = " "
      self%transform_group = FALSE
      self%rotation_mx = ZERO
      self%shift = ZERO
      self%n_MOs = 0
   end subroutine

   pure subroutine set_atom(self,atom)
   ! Set the .atom's to which .atom_index refers.
   ! NOTE: this is a copy, it may go out of date.
      type(ATOM_GROUP_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      self%atom = atom
   end subroutine

   pure subroutine set_atom_index(self,val)
   ! Generic set
      type(ATOM_GROUP_TYPE), INOUT :: self
      VEC(INT,:), IN :: val
      self%atom_index = val
      self%n_atoms = size(val)
   end subroutine

   pure subroutine set_charge(self,val)
   ! Generic set
      type(ATOM_GROUP_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%charge = val
   end subroutine

   PURE subroutine set_FChk_file_name(self,val)
   ! Generic set
      type(ATOM_GROUP_TYPE), INOUT :: self
      STR(len=*), IN :: val
   ENSURE(ends_in_(val,"fchk"),"ATOM_GROUP:set_FChk_file_name ... file name does not end in .FChk")
      self%FChk_file_name = val
   end subroutine

   pure subroutine set_MO_file_name(self,val)
   ! Generic set
      type(ATOM_GROUP_TYPE), INOUT :: self
      STR(len=*), IN :: val

      ! The following code is inherited from OBJECT
      self%MO_file_name = val
   end subroutine


!   set_mol(mol)
!   ! Set the molecule corresponding to .atom_index
!      self :: INOUT
!      mol :: MOLECULE, target, IN
!      .mol => mol
!   end

   PURE subroutine set_molden_file_name(self,val)
   ! Generic set
      type(ATOM_GROUP_TYPE), INOUT :: self
      STR(len=*), IN :: val
   ENSURE(ends_in_(val,"molden"),"ATOM_GROUP:set_molden_file_name ... file name does not end in .molden")
      self%molden_file_name = val
   end subroutine

   pure subroutine set_n_atoms(self,val)
   ! Generic set
      type(ATOM_GROUP_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%n_atoms = val
   end subroutine

   pure subroutine set_n_MOs(self,val)
   ! Generic set
      type(ATOM_GROUP_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%n_MOs = val
   end subroutine

   pure subroutine set_name(self,val)
   ! Generic set
      type(ATOM_GROUP_TYPE), INOUT :: self
      STR(len=*), IN :: val

      ! The following code is inherited from OBJECT
      self%name = val
   end subroutine

   PURE subroutine set_one_group(self)
   ! Set up one group comprising all the atoms
      type(ATOM_GROUP_TYPE), INOUT :: self
      INT :: i
   ENSURE(allocated(self%atom),"ATOM_GROUP:set_one_group ... atom list not created")
      self%atom_index = [(i,i = 1,size(self%atom))]
   end subroutine

   pure subroutine set_rotation_mx(self,val)
   ! Generic set
      type(ATOM_GROUP_TYPE), INOUT :: self
      MAT(REAL,3,3), IN :: val

      ! The following code is inherited from OBJECT
      self%rotation_mx = val
   end subroutine

   PURE subroutine set_spin_multiplicity(self,val)
   ! Set the spin multiplicity for the group
      type(ATOM_GROUP_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val/=0,"ATOM_GROUP:set_spin_multiplicity ... cannot have zero multiplicity")
      self%spin_multiplicity = val
      self%spin_multiplicity_set = TRUE
   end subroutine

   pure subroutine set_shift(self,val)
   ! Generic set
      type(ATOM_GROUP_TYPE), INOUT :: self
      VEC(REAL,3), IN :: val

      ! The following code is inherited from OBJECT
      self%shift = val
   end subroutine

   pure subroutine set_transform_group(self,val)
   ! Generic set
      type(ATOM_GROUP_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%transform_group = val
   end subroutine


!  =======
!  Inquiry
!  =======

   elemental function has_wfn_file(self) result (res)
   ! Return TRUE if there is a wavefunction file of some type set.
      type(ATOM_GROUP_TYPE), IN :: self
      BIN :: res
      res = self%FChk_file_name/=" " OR self%molden_file_name/=" "
   end function

   pure function wfn_file(self) result (res)
   ! Return TRUE if there is a wavefunction file of some type set.
      type(ATOM_GROUP_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      res = " "
      if (self%FChk_file_name/=" ") then; res = self%FChk_file_name
      else if (self%molden_file_name/=" ") then; res = self%molden_file_name
      end if
   end function


!  ============
!  Read methods
!  ============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(ATOM_GROUP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","ATOM_GROUP:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"ATOM_GROUP:read_keywords ... unexpected end of file while reading list keywords")
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
   ! Process command "keyword". Any further required data needed 
   ! by the "keyword" is inputted from "stdin".
      type(ATOM_GROUP_TYPE), INOUT :: self
      STR(len=*) :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                 ")  ! exit read_loop
      case ("atom_indices=     "); call read_atom_indices_(self)
      case ("charge=           "); call read_charge_(self)
      case ("fchk_file=        "); call read_FChk_file_name_(self)
      case ("fchk_file_name=   "); call read_FChk_file_name_(self)
      case ("mo_file=          "); call read_MO_file_name_(self)
      case ("mo_file_name=     "); call read_MO_file_name_(self)
      case ("molden_file=      "); call read_molden_file_name_(self)
      case ("molden_file_name= "); call read_molden_file_name_(self)
      case ("multiplicity=     "); call read_multiplicity_(self)
      case ("n_mos=            "); call read_n_MOs_(self)
      case ("name=             "); call read_name_(self)
      case ("rotation=         "); call read_rotation_(self)
      case ("shift=            "); call read_shift_(self)
      case ("spin_multiplicity="); call read_multiplicity_(self)
      case ("transform_group=  "); call read_transform_group_(self)
      case default; allocate(tonto%known_keywords(16))
      tonto%known_keywords(1) = "}                 "
      tonto%known_keywords(2) = "atom_indices=     "
      tonto%known_keywords(3) = "charge=           "
      tonto%known_keywords(4) = "fchk_file=        "
      tonto%known_keywords(5) = "fchk_file_name=   "
      tonto%known_keywords(6) = "mo_file=          "
      tonto%known_keywords(7) = "mo_file_name=     "
      tonto%known_keywords(8) = "molden_file=      "
      tonto%known_keywords(9) = "molden_file_name= "
      tonto%known_keywords(10) = "multiplicity=     "
      tonto%known_keywords(11) = "n_mos=            "
      tonto%known_keywords(12) = "name=             "
      tonto%known_keywords(13) = "rotation=         "
      tonto%known_keywords(14) = "shift=            "
      tonto%known_keywords(15) = "spin_multiplicity="
      tonto%known_keywords(16) = "transform_group=  "
      call unknown_(tonto,word,"ATOM_GROUP:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine


! A

   subroutine docu_atom_indices(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"read_atom_indices= VEC{INT}@ ")
      call flush_(stdout)
      call text_(stdout,". The indices of the atoms in the molecule to which this")
      call text_(stdout,"  object belongs, which define this atom group.")
      if (present(die)) stop
   end subroutine

   subroutine read_atom_indices(self)
   ! Read the atom atom_indices in the group
      type(ATOM_GROUP_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: val
      call read_all_(stdin,val)
      call set_atom_index_(self,val)
   end subroutine


! C

   subroutine docu_charge(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"charge= INT ")
      call flush_(stdout)
      call text_(stdout,". The charge in a.u. associated to this atom group.")
      if (present(die)) stop
   end subroutine

   subroutine read_charge(self)
   ! Read the charges on the group
      type(ATOM_GROUP_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      call set_charge_(self,val)
   end subroutine


! F

   subroutine docu_FChk_file_name(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"fchk_file=      STR")
      call text_(stdout,"fchk_file_name= STR")
      call flush_(stdout)
      call text_(stdout,". The name of the Gaussian FChk file associated with the group of")
      call text_(stdout,"  atoms, which contains electronic structure information, perhaps")
      call text_(stdout,"  needed for further calculations.")
      call flush_(stdout)
      call text_(stdout,". Any file suffix must also be included.")
      if (present(die)) stop
   end subroutine

   subroutine read_FChk_file_name(self)
   ! Read the name of gaussian FChk file for group
      type(ATOM_GROUP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      call set_FChk_file_name_(self,val)
   end subroutine


! M

   subroutine docu_MO_file_name(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"mo_file=      STR")
      call text_(stdout,"mo_file_name= STR")
      call flush_(stdout)
      call text_(stdout,". The name of the Tonto file containing the MOs associated with")
      call text_(stdout,"  this group of atoms of atoms, containing electronic structure")
      call text_(stdout,"  information, perhaps needed for further calculations.")
      call flush_(stdout)
      call text_(stdout,". Any file suffix must also be included.")
      if (present(die)) stop
   end subroutine

   subroutine read_MO_file_name(self)
   ! Read the name of tonto MO file for group
      type(ATOM_GROUP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      call set_MO_file_name_(self,val)
   end subroutine

   subroutine docu_molden_file_name(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"molden_file=      STR")
      call text_(stdout,"molden_file_name= STR")
      call flush_(stdout)
      call text_(stdout,". The name of the Molden formatted file associated with this")
      call text_(stdout,"  group of atoms, containing electronic structure information")
      call text_(stdout,"  perhaps needed for further calculations.")
      call flush_(stdout)
      call text_(stdout,". Information concerning the format can be found at: ")
      call text_(stdout,"  www.theochem.ru.nl/molden/molden_format.html")
      if (present(die)) stop
   end subroutine

   subroutine read_molden_file_name(self)
   ! Read the name of .molden file for the atom group
      type(ATOM_GROUP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      call set_molden_file_name_(self,val)
   end subroutine

   subroutine docu_multiplicity(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"multiplicity= INT ")
      call flush_(stdout)
      call text_(stdout,". The spin multiplicity 2*S+1 associated to this atom group")
      call text_(stdout,"  and which may be needed for electronic structure calcs.")
      if (present(die)) stop
   end subroutine

   subroutine read_multiplicity(self)
   ! Read the spin multiplicity on the group
      type(ATOM_GROUP_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      call set_spin_multiplicity_(self,val)
   end subroutine


! R

   subroutine docu_rotation(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"rotation= MAT{REAL}(3,3)")
      call flush_(stdout)
      call text_(stdout,". A 3x3 rotation matrix, comprising nine numbers entered in")
      call text_(stdout,"  column major order, which is applied to rotate the geometry")
      call text_(stdout,"  orbitals, and density matrix for this atom group before use.")
      call flush_(stdout)
      call text_(stdout,"* Note: the matrix is not allocatable, braces are not needed.")
      if (present(die)) stop
   end subroutine

   subroutine read_rotation(self)
   ! Read a 3x3 rotation matrix
      type(ATOM_GROUP_TYPE), INOUT :: self
      MAT(REAL,3,3) :: val
      call read_(stdin,val)
      call set_rotation_mx_(self,val)
   end subroutine


! S

   subroutine docu_shift(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"shift= VEC{REAL}(3)")
      call text_(stdout,"deserialize= STR")
      call flush_(stdout)
      call text_(stdout,". A 3 vector which is used to translate the geometry of")
      call text_(stdout,"  this atom group before use.")
      call flush_(stdout)
      call text_(stdout,"* Note: the units are assumed in Angstrom.")
      call flush_(stdout)
      call text_(stdout,"* Note: Braces are not needed.")
      if (present(die)) stop
   end subroutine

   subroutine read_shift(self)
   ! Read shift vector ***in angstrom units***
      type(ATOM_GROUP_TYPE), INOUT :: self
      VEC(REAL,3) :: val
      call read_(stdin,val)
      val = val*BOHR_PER_ANGSTROM
      call set_shift_(self,val)
   end subroutine


! N

   subroutine docu_n_MOs(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"n_mos= INT")
      call flush_(stdout)
      call text_(stdout,". An integer describing the number of MOs to use")
      call text_(stdout,"  to make the density matrix")
      call flush_(stdout)
      if (present(die)) stop
   end subroutine

   subroutine read_n_MOs(self)
   ! Read the number of MOs to use to make the density matrix
      type(ATOM_GROUP_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      call set_n_MOs_(self,val)
   end subroutine

   subroutine docu_name(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"name= STR")
      call flush_(stdout)
      call text_(stdout,". A STR used to describe this group of atoms.")
      call flush_(stdout)
      call text_(stdout,". If not set it may default to group-<formula>,")
      call text_(stdout,"  where <formula> is the chemical formula.")
      call flush_(stdout)
      if (present(die)) stop
   end subroutine

   subroutine read_name(self)
   ! Read the name for the groups
      type(ATOM_GROUP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      call set_name_(self,val)
   end subroutine


! T

   subroutine docu_transform_group(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"transform_group= BIN")
      call flush_(stdout)
      call text_(stdout,". Set TRUE if this atom group should be transformed by a")
      call text_(stdout,"  (spacegroup) symmetry operator, in a later calculation.")
      call flush_(stdout)
      if (present(die)) stop
   end subroutine

   subroutine read_transform_group(self)
   ! Read whether to symmetry-transform a group
      type(ATOM_GROUP_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_transform_group_(self,val)
   end subroutine


!  ====================
!  Key related routines
!  ====================

   subroutine read_keys(self)
   ! Read the "keys".
      type(ATOM_GROUP_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call read_all_(stdin,keys)
   end subroutine

   subroutine process_keys(self)
   ! Process each of the words in the "keys" list.
      type(ATOM_GROUP_TYPE), INOUT :: self
      INT :: k,l,n_key
      STR(len=STR_SIZE) :: keyword
      BIN :: keyword_echo
   ENSURE(allocated(keys),"ATOM_GROUP:process_keys ... no keys")

      ! The number of keys

      ! The following code is inherited from OBJECT
      n_key = size(keys)

      ! Keyword counter
      k = 0
      do  ! Loop over keywords in "keys"

         ! Get the next keyword
         k = k+1
         keyword = keys(k)

         ! Exit if close brace
         if (keyword=="}") exit
         if (keyword=="{") then  ! embedded keyword

            ! Turn off keyword echo
            keyword_echo = tonto%keyword_echo
            call set_keyword_echo_(tonto,FALSE)

            ! Find closing brace of embedded section
            l = index_of_matching_bracket_(keys(k:),"{")
            DIE_IF(l==0,"ATOM_GROUP:process_keys ... no matching closing brace in embedded keyword")

            ! Process internalized keywords
            call redirect_(stdin,keys(k:k+l-1))
            call read_keywords_(self)
            call revert_(stdin)

            ! Reset key counter
            k = k+l-1

            ! Reset keyword echo
            call set_keyword_echo_(tonto,keyword_echo)
         else  ! plain-old keyword

            ! Process plain-old keyword
            call process_keyword_(self,keyword)
         end if

         ! Exit if no keys left
         if (k==n_key) exit
      end do
   end subroutine

   function keys_created(self) result (res)
   ! Return TRUE if the list-element keys are created.
      type(ATOM_GROUP_TYPE), INOUT :: self
      BIN :: res

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      res = allocated(keys)
      if (res) res = size(keys)>0
   end function

   subroutine set_keys(self,the_keys)
   ! This is for setting the "keys" externally.
      type(ATOM_GROUP_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: the_keys

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      keys = the_keys
   end subroutine

   subroutine clear_keys(self)
   ! This is for destroying the "keys" externally.
      type(ATOM_GROUP_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call create_(keys,0)
   end subroutine

end module
