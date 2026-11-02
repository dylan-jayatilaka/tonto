!========================================================================
!
! CIF: An object for processing Crystallographic Information Files (CIF).
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
!========================================================================
module CIF_MODULE

#  include "macros"

   include "cif.use"

   implicit none

   include "cif.int"


contains

!  ==================
!  Allocation methods
!  ==================

   PURE subroutine create_0(self)
   ! Allocate an object
      type(CIF_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"CIF:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine


!  WARNING: destroy may not be pure due to .file

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(CIF_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"CIF:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   subroutine create_1(self,name)
   ! Create a a cif with .file_name to be "name".
      type(CIF_TYPE), allocatable, OUT :: self
      STR(len=*), IN :: name
      call create_(self)
      call set_file_name_(self,name)  ! NOT PURE
   end subroutine

   subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
      type(CIF_TYPE), INOUT :: self
      call destroy_(self%file)
   end subroutine


!  ============
!  Set routines
!  ============

   subroutine set_defaults(self,name)
   ! Set up defaults
      type(CIF_TYPE), INOUT :: self
      STR(len=*), optional, IN :: name
      self%file_name = " "
      self%use_CIF2 = FALSE
      self%data_block_name = " "
      self%start_of_data = 1
      self%end_of_data = 0
      self%data_block_found = FALSE
      self%BH_bond_length = -CIF_BH_BOND_LENGTH*BOHR_PER_ANGSTROM
      self%CH_bond_length = -CIF_CH_BOND_LENGTH*BOHR_PER_ANGSTROM
      self%NH_bond_length = -CIF_NH_BOND_LENGTH*BOHR_PER_ANGSTROM
      self%OH_bond_length = -CIF_OH_BOND_LENGTH*BOHR_PER_ANGSTROM
      self%U_iso_is_B_iso = FALSE
      self%U_is_B = FALSE
      self%U_is_beta = FALSE
      self%GC3_scale_factor = CIF_GC3_SCALE_FACTOR
      self%GC4_scale_factor = CIF_GC4_SCALE_FACTOR
      if (present(name)) call set_file_name_(self,name)  ! NOT PURE
   end subroutine

   subroutine set_file_name(self,name)
   ! Set the default .file_name to be "name".
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: name

! ENSURE(file.exists(name),"no CIF file exists named: "//name.trim)
! ENSURE(.file_name==" ","a file name already exists")
      self%file_name = name
      call create_(self%file,name)  ! ignore unmatched quotes for CIF's
      call set_ignore_unmatched_quotes_(self%file,TRUE)
      if (filename_tail_(self%file_name)=="cif2") then
         call set_use_CIF2_(self,TRUE)
      end if
   end subroutine

   pure subroutine set_use_CIF2(self,val)
   ! Set whether to use CIF output conventions
      type(CIF_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_CIF2 = val
   end subroutine

   PURE subroutine set_data_block_name(self,name)
   ! Set the default .data_block_name to be "name".
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: name
   ENSURE(self%file_name/=" ","CIF:set_data_block_name ... specify data_block_name= after the file_name=")
! ENSURE(.data_block_name==" ","a data block name already exists")
      self%data_block_name = name
   end subroutine

   PURE subroutine set_GC_format(self,fmt)
   ! Set the Gram-Charlier format
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: fmt
      self%GC_format = fmt
      call to_lower_case_(self%GC_format)
      select case (self%GC_format)

      case ("crystal  ","g")
         self%GC3_label = "_atom_site_anharm_GC_C_"
         self%GC4_label = "_atom_site_anharm_GC_D_"

      case ("cartesian","u")
         self%GC3_label = "_atom_site_Cartn_ADP3_U_"
         self%GC4_label = "_atom_site_Cartn_ADP4_U_"
      end select

      ! Set defualt in user did not enter
      if (self%GC3_scale_factor<ZERO) call set_GC3_scale_factor_(self,CIF_GC3_SCALE_FACTOR)
      if (self%GC4_scale_factor<ZERO) call set_GC4_scale_factor_(self,CIF_GC4_SCALE_FACTOR)
   end subroutine

   PURE subroutine set_GC3_scale_factor(self,val)
   ! Set the 3rd order Gram-Charlier scale factor
      type(CIF_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>ZERO,"CIF:set_GC3_scale_factor ... use +ve value!")
      self%GC3_scale_factor = val
   end subroutine

   PURE subroutine set_GC4_scale_factor(self,val)
   ! Set the 4th order Gram-Charlier scale factor
      type(CIF_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>ZERO,"CIF:set_GC4_scale_factor ... use +ve value!")
      self%GC4_scale_factor = val
   end subroutine

   pure subroutine set_GC3_label(self,lab)
   ! Set the 3rd order Gram-Charlier cif-label
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: lab
      self%GC3_label = lab
   end subroutine

   pure subroutine set_GC4_label(self,lab)
   ! Set the 4th order Gram-Charlier cif-label
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: lab
      self%GC4_label = lab
   end subroutine

   PURE subroutine set_is_mmCIF(self,val)
   ! Set whether we have a macromolecular CIF format.
      type(CIF_TYPE), INOUT :: self
      BIN, IN :: val
      self%is_mmCIF = val
   end subroutine

   PURE subroutine set_start_of_data(self,val)
   ! Set the staring line to search for something in the CIF.
      type(CIF_TYPE), INOUT :: self
      INT, IN :: val
      self%start_of_data = val
   end subroutine

   PURE subroutine set_use_fragments(self,val)
   ! Set whether we want to make fragments to split protein up.
      type(CIF_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_fragments = val
   end subroutine


!  ============
!  Open & close
!  ============

   subroutine open(self)
   ! Create and open the CIF
      type(CIF_TYPE), INOUT :: self
   ENSURE(allocated(self%file),"CIF:open ... file not created")
      call open_for_(self%file,"read")
   end subroutine

   subroutine close(self)
   ! Close the CIF, and destroy it
      type(CIF_TYPE), INOUT :: self
   ENSURE(allocated(self%file),"CIF:close ... file not created")
      call close_(self%file)
   end subroutine


!  =============
!  Input methods
!  =============

   ! NOTE : this must be finalized after the call.

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(CIF_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","CIF:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"CIF:read_keywords ... unexpected end of file while reading list keywords")
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
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                     ")  ! exit read_loop
      case ("apply_bh_bond_length= "); call read_apply_BH_bond_length_(self)
      case ("apply_ch_bond_length= "); call read_apply_CH_bond_length_(self)
      case ("apply_nh_bond_length= "); call read_apply_NH_bond_length_(self)
      case ("apply_oh_bond_length= "); call read_apply_OH_bond_length_(self)
      case ("data_block_name=      "); call read_data_block_name_(self)
      case ("file_name=            "); call read_file_name_(self)
      case ("gc_format=            "); call read_GC_format_(self)
      case ("gc3_label=            "); call read_GC3_label_(self)
      case ("gc4_label=            "); call read_GC4_label_(self)
      case ("gc3_scale_factor=     "); call read_GC3_scale_factor_(self)
      case ("gc4_scale_factor=     "); call read_GC4_scale_factor_(self)
      case ("is_mmcif=             "); call read_is_mmCIF_(self)
      case ("bh_bond_length=       "); call read_BH_bond_length_(self)
      case ("ch_bond_length=       "); call read_CH_bond_length_(self)
      case ("nh_bond_length=       "); call read_NH_bond_length_(self)
      case ("oh_bond_length=       "); call read_OH_bond_length_(self)
      case ("mmcif=                "); call read_is_mmCIF_(self)
      case ("mmcif_format=         "); call read_is_mmCIF_(self)
      case ("u_iso_is_b_iso=       "); call read_U_iso_is_B_iso_(self)
      case ("u_is_b=               "); call read_U_is_B_(self)
      case ("u_is_beta=            "); call read_U_is_beta_(self)
      case ("use_fragments=        "); call read_use_fragments_(self)
      case default; allocate(tonto%known_keywords(23))
      tonto%known_keywords(1) = "}                     "
      tonto%known_keywords(2) = "apply_bh_bond_length= "
      tonto%known_keywords(3) = "apply_ch_bond_length= "
      tonto%known_keywords(4) = "apply_nh_bond_length= "
      tonto%known_keywords(5) = "apply_oh_bond_length= "
      tonto%known_keywords(6) = "data_block_name=      "
      tonto%known_keywords(7) = "file_name=            "
      tonto%known_keywords(8) = "gc_format=            "
      tonto%known_keywords(9) = "gc3_label=            "
      tonto%known_keywords(10) = "gc4_label=            "
      tonto%known_keywords(11) = "gc3_scale_factor=     "
      tonto%known_keywords(12) = "gc4_scale_factor=     "
      tonto%known_keywords(13) = "is_mmcif=             "
      tonto%known_keywords(14) = "bh_bond_length=       "
      tonto%known_keywords(15) = "ch_bond_length=       "
      tonto%known_keywords(16) = "nh_bond_length=       "
      tonto%known_keywords(17) = "oh_bond_length=       "
      tonto%known_keywords(18) = "mmcif=                "
      tonto%known_keywords(19) = "mmcif_format=         "
      tonto%known_keywords(20) = "u_iso_is_b_iso=       "
      tonto%known_keywords(21) = "u_is_b=               "
      tonto%known_keywords(22) = "u_is_beta=            "
      tonto%known_keywords(23) = "use_fragments=        "
      call unknown_(tonto,word,"CIF:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   pure subroutine update(self)
   ! Make sure the input satisfies sanity checks.
      type(CIF_TYPE), INOUT :: self
      if (self%GC3_scale_factor<ONE) self%GC3_scale_factor = CIF_GC3_SCALE_FACTOR
      if (self%GC4_scale_factor<ONE) self%GC4_scale_factor = CIF_GC4_SCALE_FACTOR
   end subroutine


!  ============
!  Read methods
!  ============

   subroutine read_data_block_name(self)
   ! Read the default data_block_name to look for.
      type(CIF_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name
      call read_(stdin,name)
      call set_data_block_name_(self,name)
   end subroutine

   subroutine read_file_name(self)
   ! Read the file name of the CIF file to open, then create the .file object
      type(CIF_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name
      call read_(stdin,name)
      call set_file_name_(self,name)  ! leaky here
   end subroutine

   subroutine read_GC3_scale_factor(self)
   ! Read the scaling for 3rd order Gram-Charlier coefficients.
      type(CIF_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_GC3_scale_factor_(self,val)
   end subroutine

   subroutine read_GC4_scale_factor(self)
   ! Read the scaling for 4th order Gram-Charlier coefficients.
      type(CIF_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_GC4_scale_factor_(self,val)
   end subroutine

   subroutine read_GC_format(self)
   ! Read the format for the Gram-Charlier coefficients.
      type(CIF_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: fmt
      call read_(stdin,fmt)
      call set_GC_format_(self,fmt)
   end subroutine

   subroutine read_GC3_label(self)
   ! Read the cif-file-label for the 3rd order Gram-Charlier coefficients.
      type(CIF_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: lab
      call read_(stdin,lab)
      call set_GC3_label_(self,lab)
   end subroutine

   subroutine read_GC4_label(self)
   ! Read the cif-file-label for the 4th order Gram-Charlier coefficients.
      type(CIF_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: lab
      call read_(stdin,lab)
      call set_GC4_label_(self,lab)
   end subroutine

   subroutine read_apply_BH_bond_length(self)
   ! Read whether to apply the BH bond length factor.
      type(CIF_TYPE), INOUT :: self
      BIN :: apply
      call read_(stdin,apply)
      if (apply) then; self%BH_bond_length = abs(self%BH_bond_length)
      else; self%BH_bond_length = -abs(self%BH_bond_length)
      end if
   end subroutine

   subroutine read_apply_CH_bond_length(self)
   ! Read whether to apply the CH bond length factor.
      type(CIF_TYPE), INOUT :: self
      BIN :: apply
      call read_(stdin,apply)
      if (apply) then; self%CH_bond_length = abs(self%CH_bond_length)
      else; self%CH_bond_length = -abs(self%CH_bond_length)
      end if
   end subroutine

   subroutine read_apply_NH_bond_length(self)
   ! Read whether to apply the NH bond length factor.
      type(CIF_TYPE), INOUT :: self
      BIN :: apply
      call read_(stdin,apply)
      if (apply) then; self%NH_bond_length = abs(self%NH_bond_length)
      else; self%NH_bond_length = -abs(self%NH_bond_length)
      end if
   end subroutine

   subroutine read_apply_OH_bond_length(self)
   ! Read whether to apply the OH bond length factor.
      type(CIF_TYPE), INOUT :: self
      BIN :: apply
      call read_(stdin,apply)
      if (apply) then; self%OH_bond_length = abs(self%OH_bond_length)
      else; self%OH_bond_length = -abs(self%OH_bond_length)
      end if
   end subroutine

   subroutine read_BH_bond_length(self)
   ! Read the BH bond length factor.
      type(CIF_TYPE), INOUT :: self
      call read_quantity_(stdin,self%BH_bond_length)
      ENSURE(self%BH_bond_length>ZERO,"CIF:read_BH_bond_length ... length must be +ve to be applied")
   end subroutine

   subroutine read_CH_bond_length(self)
   ! Read the CH bond length factor.
      type(CIF_TYPE), INOUT :: self
      call read_quantity_(stdin,self%CH_bond_length)
      ENSURE(self%CH_bond_length>ZERO,"CIF:read_CH_bond_length ... length must be +ve to be applied")
   end subroutine

   subroutine read_NH_bond_length(self)
   ! Read the NH bond length factor.
      type(CIF_TYPE), INOUT :: self
      call read_quantity_(stdin,self%NH_bond_length)
      ENSURE(self%NH_bond_length>ZERO,"CIF:read_NH_bond_length ... length must be +ve to be applied")
   end subroutine

   subroutine read_OH_bond_length(self)
   ! Read the OH bond length factor.
      type(CIF_TYPE), INOUT :: self
      call read_quantity_(stdin,self%OH_bond_length)
      ENSURE(self%OH_bond_length>ZERO,"CIF:read_OH_bond_length ... length must be +ve to be applied")
   end subroutine

   subroutine read_is_mmCIF(self)
   ! Read if the format is macromolecular CIF style.
      type(CIF_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_is_mmCIF_(self,val)
   end subroutine

   subroutine read_U_iso_is_B_iso(self)
   ! If TRUE, then any U_iso values (i.e. ADP's) in the CIF file are interpreted
   ! as B_iso values. This is a hack that should not be used except when the CIF
   ! file is hand made using B_iso values.
      type(CIF_TYPE), INOUT :: self
      call read_(stdin,self%U_iso_is_B_iso)
   end subroutine

   subroutine read_U_is_B(self)
   ! If TRUE, then any U values (i.e. ADP's) in the CIF file are interpreted as B
   ! values. This is a hack that should not be used except when the CIF file is
   ! hand made using B values.
      type(CIF_TYPE), INOUT :: self
      call read_(stdin,self%U_is_B)
   end subroutine

   subroutine read_U_is_beta(self)
   ! If TRUE, then any U values (i.e. ADP's) in the CIF file are interpreted as
   ! beta values. This is a hack that should not be used except when the CIF
   ! file is hand made using B values.
      type(CIF_TYPE), INOUT :: self
      call read_(stdin,self%U_is_beta)
   end subroutine

   subroutine read_use_fragments(self)
   ! Read whether to create fragments to split protein.
   ! NOTE: The CIF must be mmCIF format.
      type(CIF_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_use_fragments_(self,val)
   end subroutine


!  =======
!  Inquiry
!  =======

   pure function apply_bond_length(self) result (apply)
   ! Return whether to apply the any bond length factor.
      type(CIF_TYPE), IN :: self
      BIN :: apply
      apply = self%BH_bond_length>ZERO OR self%CH_bond_length>ZERO OR self%NH_bond_length>ZERO OR self%OH_bond_length>ZERO
   end function

   pure function apply_BH_bond_length(self) result (apply)
   ! Return whether to apply the BH bond length factor.
      type(CIF_TYPE), IN :: self
      BIN :: apply
      apply = self%BH_bond_length>ZERO
   end function

   pure function apply_CH_bond_length(self) result (apply)
   ! Return whether to apply the CH bond length factor.
      type(CIF_TYPE), IN :: self
      BIN :: apply
      apply = self%CH_bond_length>ZERO
   end function

   pure function apply_NH_bond_length(self) result (apply)
   ! Return whether to apply the NH bond length factor.
      type(CIF_TYPE), IN :: self
      BIN :: apply
      apply = self%NH_bond_length>ZERO
   end function

   pure function apply_OH_bond_length(self) result (apply)
   ! Return whether to apply the OH bond length factor.
      type(CIF_TYPE), IN :: self
      BIN :: apply
      apply = self%OH_bond_length>ZERO
   end function


!  ===============
!  Find data block
!  ===============

   subroutine find_data_block(self,found)
   ! Find the start of a data block "data_<name>" in the cif file.
   ! . Start from line .start_of_data = 1
   ! . If present and the block is found, "found" is set TRUE.
   ! . If not present and the block is not found, it is an error. 
   ! . If found, the ".start_of_data", ".end_of_data" records 
   !   are stored, and the "<name>" in ".data_block_name".
      type(CIF_TYPE), INOUT :: self
      BIN, optional, OUT :: found
   ENSURE(allocated(self%file),"CIF:find_data_block ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:find_data_block ... CIF file has not been opened")

      ! From line 1
      self%start_of_data = 1

      ! Find the start
      call find_start_of_data_block_(self)

      ! Found?
      if (present(found)) then
         found = self%data_block_found
      end if
   end subroutine

   subroutine find_data_block_called(self,name,found)
   ! Find the start of a *specific* data block "data_<name>" 
   ! with "<name>" e.g. starting with "data_name" in the cif file.
   ! . Start from line .start_of_data = 1
   ! . If present and the block is found, "found" is set TRUE.
   ! . If not present and the block is not found, it is an error. 
   ! . If found, the ".start_of_data", ".end_of_data" records 
   !   are stored, and the "<name>" in ".data_block_name".
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: name
      BIN, optional, OUT :: found
   ENSURE(allocated(self%file),"CIF:find_data_block_called ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:find_data_block_called ... CIF file has not been opened")

      ! From line 1
      self%start_of_data = 1
      do

         ! Find the start
         call find_start_of_data_block_(self)

         ! Found?
         if (present(found)) then
            found = self%data_block_found
            if (NOT found) return
         else
            ENSURE(self%data_block_found,"CIF:find_data_block_called ... cant find data block with name "//trim(name))
         end if
         if (self%data_block_name==name) exit
         self%start_of_data = self%end_of_data
      end do
   end subroutine

   subroutine find_start_of_data_block(self)
   ! Find a data block "data_<name>" 
   ! . Start from line ".start_of_data"
   ! . Store its <name> in ".data_block_name"
   ! . Store the record where it occurs in ".start_of_data"
   ! . If found, set ".data_block_found" TRUE, else FALSE
   ! . Cursor is left before the data block, or at the EOF 
   !   if nothing was found.
      type(CIF_TYPE), INOUT :: self
   ENSURE(self%start_of_data>0,"CIF:find_start_of_data_block ... .start_of_data is 0")
   ENSURE(allocated(self%file),"CIF:find_start_of_data_block ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:find_start_of_data_block ... CIF file has not been opened")
      self%data_block_name = " "
      call look_for_item_(self%file,"data_",head_match=TRUE,from=self%start_of_data,found=self%data_block_found)
      if (self%data_block_found) then

         ! Get data block name
         call move_to_previous_item_(self%file)
         call read_(self%file,self%data_block_name)

         ! Name without data_ at start
         self%data_block_name = self%data_block_name(6:)
         self%start_of_data = line_number_(self%file)

         ! Find the end, from the start
         call find_end_of_data_block_(self)

         ! Move back to the start
         call move_to_line_(self%file,self%start_of_data)
      end if
   end subroutine

   subroutine find_end_of_data_block(self)
   ! Find the end of a data block, starting from the first line
   ! after ".start_of_data"
   ! . Store the record where it occurs in ".end_of_data". 
   ! . Cursor is left at the end of the data block, possibly at the
   !   end of the file if no block terminator was found. 
      type(CIF_TYPE), INOUT :: self
   ENSURE(allocated(self%file),"CIF:find_end_of_data_block ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:find_end_of_data_block ... CIF file has not been opened")

      ! Find EOD
      call look_for_item_(self%file,"_eof",exact_match=TRUE,from=self%start_of_data+1,end_tokens=["data_"])

      ! Set the line number for EOD
      self%end_of_data = self%file%record
   end subroutine

   subroutine find_crystal_data_block(self,found)
   ! Find the first acceptable small-molecule data block
   ! . Starting from ".start_of_data" 
   ! . Store its name in ".data_block_name".
   ! . If present "found" is set TRUE
   ! . If not present, it is an error
      type(CIF_TYPE), INOUT :: self
      BIN, optional, OUT :: found
      if (NOT self%is_mmCIF) then
         call find_sm_data_block_(self,found)
      else
         call find_mm_data_block_(self,found)
      end if
   end subroutine

   subroutine find_sm_data_block(self,found)
   ! Find the first acceptable small-molecule crystal data block 
   ! . Starting from ".start_of_data" 
   ! . Store its name in ".data_block_name".
   ! . If present "found" is set TRUE
   ! . If not present, it is an error
      type(CIF_TYPE), INOUT :: self
      BIN, optional, OUT :: found
      VEC(BIN,4) :: there
      INT :: f,l
   ENSURE(allocated(self%file),"CIF:find_sm_data_block ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:find_sm_data_block ... CIF file has not been opened")
      do

         ! Find block from .start_of_data ...
         call find_start_of_data_block_(self)

         ! Set found or DIE
         if (present(found)) then
            found = self%data_block_found
            if (NOT found) return
         else
            ENSURE(self%data_block_found,"CIF:find_sm_data_block ... no valid data block in file: "//trim(self%file%name))
         end if

         ! OK, we found a data block
         f = self%start_of_data
         l = self%end_of_data

         ! Look for some kind of space group information
         call look_for_item_(self%file,"_symmetry_space_group_name_Hall",from=f,until=l,found=there(1))
         if (NOT there(1)) call look_for_item_(self%file,"_symmetry_equiv_pos_as_xyz",from=f,until=l,found=there(1))
         if (NOT there(1)) call look_for_item_(self%file,"_space_group_name_Hall",from=f,until=l,found=there(1))
         if (NOT there(1)) call look_for_item_(self%file,"_space_group_symop_operation_xyz",from=f,until=l,found=there(1))
         if (NOT there(1)) call look_for_item_(self%file,"_symmetry_space_group_name_H-M",from=f,until=l,found=there(1))
         if (NOT there(1)) call look_for_item_(self%file,"_space_group_name_H-M_alt",from=f,until=l,found=there(1))
         if (NOT there(1)) call look_for_item_(self%file,"_symmetry_Int_Tables_number",from=f,until=l,found=there(1))

         ! Look for cell data
         call look_for_item_(self%file,"_cell_length",from=f,until=l,found=there(2))

         ! Look for atomic labels
         call look_for_item_(self%file,"_atom_site_label",from=f,until=l,found=there(3))
         if (NOT there(3)) call look_for_item_(self%file,"_atom_site_type_symbol",from=f,until=l,found=there(3))
         if (NOT there(3)) call look_for_item_(self%file,"_atom_site_Cartn_label",from=f,until=l,found=there(3))
         if (NOT there(3)) call look_for_item_(self%file,"_atom_site_Cryst_label",from=f,until=l,found=there(3))

         ! Look for atomic coordinates
         call look_for_item_(self%file,"_atom_site_fract",from=f,until=l,found=there(4))
         if (NOT there(4)) call look_for_item_(self%file,"_atom_site_Cartn_x",from=f,until=l,found=there(4))
         if (NOT there(4)) call look_for_item_(self%file,"_atom_site_Cryst_x",from=f,until=l,found=there(4))

         ! All data there? 
         ! Then its really a crystal data block
         if (all(there)) exit

         ! Keep looking, from current end-of-data
         self%start_of_data = self%end_of_data
      end do

      ! Success
      if (present(found)) found = TRUE
   end subroutine

   subroutine find_mm_data_block(self,found)
   ! Find the first acceptable macro-molecular data block
   ! . Starting from ".start_of_data" 
   ! . Store its name in ".data_block_name".
   ! . If present "found" is set TRUE
   ! . If not present, it is an error
      type(CIF_TYPE), INOUT :: self
      BIN, OUT, optional :: found
      VEC(BIN,12) :: there
      INT :: f,l
   ENSURE(allocated(self%file),"CIF:find_mm_data_block ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:find_mm_data_block ... CIF file has not been opened")
      do

         ! Find block from .start_of_data ...
         call find_start_of_data_block_(self)

         ! Set found or DIE
         if (present(found)) then
            found = self%data_block_found
            if (NOT found) return
         else
            ENSURE(self%data_block_found,"CIF:find_mm_data_block ... no valid data block in file: "//trim(self%file%name))
         end if

         ! OK, we found a data block
         f = self%start_of_data
         l = self%end_of_data

         ! Look for some kind of space group information
         call look_for_item_(self%file,"_symmetry.space_group_name_Hall",from=f,until=l,found=there(1))
         if (NOT there(1)) call look_for_item_(self%file,"_symmetry_equiv.pos_as_xyz",from=f,until=l,found=there(1))
         if (NOT there(1)) call look_for_item_(self%file,"_space_group.name_Hall",from=f,until=l,found=there(1))
         if (NOT there(1)) call look_for_item_(self%file,"_space_group_symop.operation_xyz",from=f,until=l,found=there(1))
         if (NOT there(1)) call look_for_item_(self%file,"_symmetry.space_group_name_H-M",from=f,until=l,found=there(1))
         if (NOT there(1)) call look_for_item_(self%file,"_space_group.name_H-M_alt",from=f,until=l,found=there(1))
         if (NOT there(1)) call look_for_item_(self%file,"_symmetry.Int_Tables_number",from=f,until=l,found=there(1))

         ! Look for cell data
         call look_for_item_(self%file,"_cell.length",from=f,until=l,found=there(2))

         ! Look for atomic labels
         call look_for_item_(self%file,"_atom_site.type_symbol",from=f,until=l,found=there(3))
         call look_for_item_(self%file,"_atom_site.label_atom_id",from=f,until=l,found=there(4))
         call look_for_item_(self%file,"_atom_site.label_comp_id",from=f,until=l,found=there(5))
         call look_for_item_(self%file,"_atom_site.label_seq_id",from=f,until=l,found=there(6))
         call look_for_item_(self%file,"_atom_site.label_asym_id",from=f,until=l,found=there(7))
         call look_for_item_(self%file,"_atom_site.label_entity_id",from=f,until=l,found=there(8))
         call look_for_item_(self%file,"_atom_site.Cartn_x",from=f,until=l,found=there(9))
         call look_for_item_(self%file,"_atom_site.Cartn_y",from=f,until=l,found=there(10))
         call look_for_item_(self%file,"_atom_site.Cartn_z",from=f,until=l,found=there(11))
         call look_for_item_(self%file,"_atom_site.occupancy",from=f,until=l,found=there(12))

         ! All data there? 
         ! Then its really a crystal data block
         if (all(there)) exit

         ! Keep looking, from current end-of-data
         self%start_of_data = self%end_of_data
      end do

      ! Success
      if (present(found)) found = TRUE
   end subroutine

   subroutine move_to_end_of_data(self)
   ! Move to the (previously found) end of a data block.
      type(CIF_TYPE), INOUT :: self
      INT :: eod
   ENSURE(allocated(self%file),"CIF:move_to_end_of_data ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:move_to_end_of_data ... CIF file has not been opened")
   ENSURE(self%end_of_data>0,"CIF:move_to_end_of_data ... end_of_data is not set")
      eod = self%end_of_data

      ! Find EOD
      call move_to_line_(self%file,eod)

      ! Set the line number for EOD
      self%start_of_data = eod
   end subroutine


!  =================
!  Find/read methods
!  =================

! Find single basic item

   subroutine find_item(self,ID,found)
   ! Find a (non-looped) data item with identifier "ID" and leave the
   ! cursor positioned just after it. If present, "found" is set TRUE
   ! if "ID" is found, else it is set FALSE and the routine returns
   ! without an error. 
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: ID
      BIN, optional, OUT :: found
      BIN :: fnd
   ENSURE(allocated(self%file),"CIF:find_item ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:find_item ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:find_item ... no data block found")
   ENSURE(self%end_of_data>self%start_of_data,"CIF:find_item ... no data block found")

      ! Find the "ID"
      call look_for_item_(self%file,ID,from=self%start_of_data,until=self%end_of_data,found=fnd,exact_match=TRUE)
      if (present(found)) then
         found = fnd
         if (NOT found) return
      else if (NOT fnd) then
         DIE("CIF:find_item ... data item "//trim(ID)//" not found")
      end if
   end subroutine

! Read single basic item

   subroutine read_item_0(self,ID,item,found)
   ! Read a data "item" with string identifier "ID". If present,
   ! "found" is set TRUE if "ID" is found, otherwise it is set FALSE
   ! and there is no error.
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: ID
      STR(len=*), OUT :: item
      BIN, optional, OUT :: found
   ENSURE(allocated(self%file),"CIF:read_item_0 ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_item_0 ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_item_0 ... no data block found")

      ! Find "ID"

      ! The following code is inherited from CIF
      call find_item_(self,ID,found)

      ! Found?
      if (present(found)) then
         if (NOT found) return
      end if

      ! Read the data
      call read_(self%file,item)
   end subroutine

   subroutine read_item_1(self,ID,item,found)
   ! Read a data "item" with string identifier "ID". If present,
   ! "found" is set TRUE if "ID" is found, otherwise it is set FALSE
   ! and there is no error.
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: ID
      INT, OUT :: item
      BIN, optional, OUT :: found
   ENSURE(allocated(self%file),"CIF:read_item_1 ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_item_1 ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_item_1 ... no data block found")

      ! Find "ID"

      ! The following code is inherited from CIF
      call find_item_(self,ID,found)

      ! Found?
      if (present(found)) then
         if (NOT found) return
      end if

      ! Read the data
      call read_(self%file,item)
   end subroutine


! Read single STR item, multiline allowed

   subroutine read_item_2(self,ID,item,itemvec,found)
   ! Read a STR data "item" with string identifier "ID". If the data
   ! is a multi-line semicolon delimited paragraph the lines are
   ! placed in "itemvec". If present, "found" is set TRUE if "ID" is 
   ! found, otherwise it is set FALSE and there is no error.
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: ID
      STR(len=*), OUT :: item
      VEC(STR(len=*),:), allocatable :: itemvec
      BIN, optional, OUT :: found
      STR(len=STR_SIZE) :: semicolon
   ENSURE(allocated(self%file),"CIF:read_item_2 ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_item_2 ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_item_2 ... no data block found")

      ! Find "ID"
      call find_item_(self,ID,found)

      ! Found?
      if (present(found)) then
         if (NOT found) return
      end if

      ! Read the item
      call read_(self%file,item)

      ! Multi-line data?
      semicolon = ";"
      if (item==semicolon AND self%file%buffer%item_start==1) then

         ! Append rest of line
         item = rest_of_line_(self%file)
         if (item/=" ") call append_(itemvec,item)

         ! Read lines and append
         do
            call read_(self%file,item)
            if (item==semicolon AND self%file%buffer%item_start==1) exit
            call append_(itemvec,self%file%buffer%string)
            call read_line_(self%file)
         end do
         ENSURE(size(itemvec)>0,"CIF:read_item_2 ... zero length string")

         ! If only one line, store it as an item
         if (size(itemvec)==1) then
            item = itemvec(1)
            call destroy_(itemvec)
         else
            item = " "
         end if
      end if
   end subroutine


! Read single REAL item, with (error)

   subroutine read_item_3(self,ID,item,error,found)
   ! Read a REAL data "item" with string identifier "ID". If present,
   ! any "error" associated with "item" is also returned. If present,
   ! "found" is set TRUE if "ID" is found, otherwise it is set FALSE
   ! and there is no error.
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: ID
      REAL, OUT :: item
      REAL, optional, OUT :: error
      BIN, optional, OUT :: found
      REAL :: err
   ENSURE(allocated(self%file),"CIF:read_item_3 ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_item_3 ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_item_3 ... no data block found")

      ! Find "ID"
      call find_item_(self,ID,found)

      ! Found?
      if (present(found)) then
         if (NOT found) return
      end if

      ! Read the data
      call read_(self%file,item,err)
      if (present(error)) error = err
   end subroutine

! Read single list item

   subroutine read_item_4(self,ID,item,found)
   ! Read an array data "item" with string identifier "ID". If
   ! present, "found" is set TRUE if "ID" is found, otherwise it is
   ! set FALSE and there is no error.
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: ID
      VEC(REAL,:), OUT :: item
      BIN, optional, OUT :: found
      STR(len=STR_SIZE) :: delimiter
   ENSURE(allocated(self%file),"CIF:read_item_4 ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_item_4 ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_item_4 ... no data block found")

      ! Find "ID"

      ! The following code is inherited from CIF
      call find_item_(self,ID,found)

      ! Found?
      if (present(found)) then
         if (NOT found) return
      end if

      ! Open, works for CIF1 & CIF2
      call read_(self%file,delimiter)
      ENSURE(is_one_of_(delimiter,["[",";"]),"CIF:read_item_4 ... opening delimiter must be present for list item")

      ! Read the data
      call read_(self%file,item)

      ! Close, works for CIF1 & CIF2
      call read_(self%file,delimiter)
      ENSURE(is_one_of_(delimiter,["]",";"]),"CIF:read_item_4 ... opening delimiter must be present for list item")
   end subroutine

   subroutine read_item_5(self,ID,item,found)
   ! Read an array data "item" with string identifier "ID". If
   ! present, "found" is set TRUE if "ID" is found, otherwise it is
   ! set FALSE and there is no error.
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: ID
      MAT(REAL,:,:), OUT :: item
      BIN, optional, OUT :: found
      STR(len=STR_SIZE) :: delimiter
   ENSURE(allocated(self%file),"CIF:read_item_5 ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_item_5 ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_item_5 ... no data block found")

      ! Find "ID"

      ! The following code is inherited from CIF
      call find_item_(self,ID,found)

      ! Found?
      if (present(found)) then
         if (NOT found) return
      end if

      ! Open, works for CIF1 & CIF2
      call read_(self%file,delimiter)
      ENSURE(is_one_of_(delimiter,["[",";"]),"CIF:read_item_5 ... opening delimiter must be present for list item")

      ! Read the data
      call read_(self%file,item)

      ! Close, works for CIF1 & CIF2
      call read_(self%file,delimiter)
      ENSURE(is_one_of_(delimiter,["]",";"]),"CIF:read_item_5 ... opening delimiter must be present for list item")
   end subroutine

   subroutine read_all_item(self,ID,item,op,cl,found)
   ! Read an allocatable array data "item" with string identifier
   ! "ID" which is delimited by "op" and "cl" delimiters. If present,
   ! "found" is set TRUE if "ID" is found, otherwise it is set FALSE
   ! and there is no error.
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: ID
      VEC(REAL,:), allocatable, OUT :: item
      STR(len=*), IN :: op
      STR(len=*), IN :: cl
      BIN, optional, OUT :: found
   ENSURE(allocated(self%file),"CIF:read_all_item ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_all_item ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_all_item ... no data block found")

      ! Find "ID"

      ! The following code is inherited from CIF
      call find_item_(self,ID,found)

      ! Found?
      if (present(found)) then
         if (NOT found) return
      end if

      ! Read the data
      call read_all_(self%file,item,op,cl)
   end subroutine


! Find single looped basic item

   subroutine find_looped_item(self,ID,found,ID_pos,n_labels)
   ! In a loop_, find a list of _data identifiers "ID", and leave the
   ! file cursor just before the actual looped data. If any "ID" is
   ! not found the routine fails -- unless "found" is present. In that
   ! case "found" is set TRUE/FALSE for (respectively) success/failure.
   ! If present: 
   ! . ID_pos   is the position no. of ID in the _data of the loop_
   ! . n_labels is the total number of looped data labels
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: ID
      BIN, optional, OUT :: found
      INT, optional, OUT :: ID_pos,n_labels
      BIN :: fnd
      STR(len=STR_SIZE) :: word
      INT :: label,item
   ENSURE(allocated(self%file),"CIF:find_looped_item ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:find_looped_item ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:find_looped_item ... no data block found")
   ENSURE(ID(1:1)=="_","CIF:find_looped_item ... ID list is not a looped datum")

      ! Look for the ID
      call look_for_item_(self%file,ID,from=self%start_of_data,until=self%end_of_data,found=fnd,exact_match=TRUE)

      ! Found?
      if (present(found)) then
         found = fnd
         if (NOT found) return
      else if (NOT fnd) then
         DIE("CIF:find_looped_item ... data item "//trim(ID)//" not found")
      end if

      ! Is it in a loop? Look backwards for loop_
      call look_backwards_for_item_(self%file,"loop_",until=self%start_of_data,found=fnd)

      ! Found?
      if (present(found)) then
         found = fnd
         if (NOT found) return
      else if (NOT fnd) then
         DIE("CIF:find_looped_item ... data item "//trim(ID)//" is not looped")
      end if

      ! Item no. in ID
      item = 0

      ! The _data label no.
      label = 0

      ! Loop over ID items / _data labels 
      do

         ! Anything to read?
         if (at_EOF_(self%file)) then
            DIE_IF(label==0,"CIF:find_looped_item ... EOF, no _data labels!")
            DIE_IF(label>0,"CIF:find_looped_item ... EOF after _data labels i.e. no loop data!")
            exit
         end if

         ! A looped data item
         call read_(self%file,word)
         if (ID==word) then

            ! A matching ID.
            item = item+1
            label = label+1
            if (present(ID_pos)) ID_pos = label
         else if (word(1:1)=="_") then

            ! A looped _data descriptor
            label = label+1
         else

            ! Not a loop descriptor
            exit
         end if
      end do

      ! Move before first datum
      call move_to_previous_item_(self%file)

      ! Return n_labels
      if (present(n_labels)) n_labels = label
   end subroutine

! Read single looped basic item

   subroutine read_looped_item_0(self,ID,item,found)
   ! Read a looped/vector data "item" with string identifier "ID".
   ! If present, "found" is set TRUE if "ID" is found, else it is set
   ! FALSE and the routine returns without an error.
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: ID
      VEC(INT,:), allocatable, OUT :: item
      BIN, optional, OUT :: found
      INT :: n_labels,ID_pos
      STR(len=STR_SIZE) :: word
      BIN :: fnd
      INT :: val
   ENSURE(allocated(self%file),"CIF:read_looped_item_0 ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_looped_item_0 ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_looped_item_0 ... no data block found")
   ENSURE(ID(1:1)=="_","CIF:read_looped_item_0 ... ID list is not a looped datum")

      ! Find the looped item

      ! The following code is inherited from CIF
      call find_looped_item_(self,ID,fnd,ID_pos,n_labels)

      ! Return if not found
      if (present(found)) then
         found = fnd
         if (NOT found) return
      end if

      ! Loop over ID-type lines
      do

         ! Skip to just before ID
         call skip_next_items_(self%file,ID_pos-1)

         ! Read the ID-item
         call read_(self%file,val)
         call append_(item,val)

         ! Skip to end of ID list
         call skip_next_items_(self%file,n_labels-ID_pos)

         ! EOF? End of loop
         if (at_EOF_(self%file)) exit

         ! A reserved word also end loop
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:1)==";") exit  ! a text field ends the loop
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit
      end do
   end subroutine

   subroutine read_looped_item_1(self,ID,item,found)
   ! Read a looped/vector data "item" with string identifier "ID".
   ! If present, "found" is set TRUE if "ID" is found, else it is set
   ! FALSE and the routine returns without an error.
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: ID
      VEC(STR(len=*),:), allocatable, OUT :: item
      BIN, optional, OUT :: found
      INT :: n_labels,ID_pos
      STR(len=STR_SIZE) :: word
      BIN :: fnd
      STR(len=STR_SIZE) :: val
   ENSURE(allocated(self%file),"CIF:read_looped_item_1 ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_looped_item_1 ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_looped_item_1 ... no data block found")
   ENSURE(ID(1:1)=="_","CIF:read_looped_item_1 ... ID list is not a looped datum")

      ! Find the looped item

      ! The following code is inherited from CIF
      call find_looped_item_(self,ID,fnd,ID_pos,n_labels)

      ! Return if not found
      if (present(found)) then
         found = fnd
         if (NOT found) return
      end if

      ! Loop over ID-type lines
      do

         ! Skip to just before ID
         call skip_next_items_(self%file,ID_pos-1)

         ! Read the ID-item
         call read_(self%file,val)
         call append_(item,val)

         ! Skip to end of ID list
         call skip_next_items_(self%file,n_labels-ID_pos)

         ! EOF? End of loop
         if (at_EOF_(self%file)) exit

         ! A reserved word also end loop
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:1)==";") exit  ! a text field ends the loop
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit
      end do
   end subroutine


! Read single looped REAL item, with (error)

   subroutine read_looped_item_2(self,ID,item,error,found)
   ! Read a looped/vector data "item" with string identifier "ID".
   ! Also, read any "error" associated with the "item" (if present).
   ! If present, "found" is set TRUE if "ID" is found, else it is set
   ! FALSE and the routine returns without an error.
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: ID
      VEC(REAL,:), allocatable, OUT :: item
      VEC(REAL,:), allocatable, optional, OUT :: error
      BIN, optional, OUT :: found
      INT :: n_labels,ID_pos
      STR(len=STR_SIZE) :: word
      REAL :: val,err
   ENSURE(allocated(self%file),"CIF:read_looped_item_2 ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_looped_item_2 ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_looped_item_2 ... no data block found")
   ENSURE(ID(1:1)=="_","CIF:read_looped_item_2 ... ID list is not a looped datum")

      ! Find the looped item
      call find_looped_item_(self,ID,found,ID_pos,n_labels)

      ! Return if not found
      if (present(found)) then
         if (NOT found) return
      end if

      ! Loop over ID-type lines
      do

         ! Skip to just before ID
         call skip_next_items_(self%file,ID_pos-1)

         ! Read the ID-item (and error)
         call read_(self%file,val,err)
         call append_(item,val)
         if (present(error)) call append_(error,err)

         ! Skip to end of ID list
         call skip_next_items_(self%file,n_labels-ID_pos)

         ! EOF? End of loop
         if (at_EOF_(self%file)) exit

         ! A reserved word also end loop
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:1)==";") exit  ! a text field ends the loop
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit
      end do
   end subroutine


! Find multiple looped item

   subroutine find_looped_items(self,ID,found,ID_pos,ID_ind,n_labels)
   ! In a loop_, find a list of _data identifiers "ID", and leave the
   ! file cursor just before the actual looped data. If any "ID" is
   ! not found the routine fails -- unless "found" is present. In that
   ! case "found" is set TRUE/FALSE for (respectively) success/failure.
   ! If present: 
   ! . ID_pos(item) is the position of the _data label in the loop_
   !                that is the item-th matching label in ID.
   ! . ID_ind(item) is the index of the above label in ID
   ! . n_labels     is the total number of looped data labels, 
   !                ID.dim<=n_labels.
   ! NOTE: The ID's _data labels are processed in the order that they 
   ! occur in the loop_ i.e. ID_pos(item) is monotone increasing and
   ! in particular ID_pos(item) <= label
      type(CIF_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: ID
      BIN, optional, OUT :: found
      VEC(INT,:), optional, OUT :: ID_pos
      VEC(INT,:), optional, OUT :: ID_ind
      INT, optional, OUT :: n_labels
      BIN :: fnd
      STR(len=STR_SIZE) :: word
      INT :: label,item
   ENSURE(allocated(self%file),"CIF:find_looped_items ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:find_looped_items ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:find_looped_items ... no data block found")
   ENSURE(size(ID)>0,"CIF:find_looped_items ... no items in ID list")
   ENSURE(all(ID(:)(1:1)=="_"),"CIF:find_looped_items ... ID list does not have a looped datum")
   ENSURE(NOT has_repetitions_(ID),"CIF:find_looped_items ... ID list has repetitions")

      ! Sanity check
      if (present(ID_pos)) then
         ENSURE(size(ID)==size(ID_pos),"CIF:find_looped_items ... ID and ID_pos are inconsistent")
      end if
      if (present(ID_ind)) then
         ENSURE(size(ID)==size(ID_ind),"CIF:find_looped_items ... ID and ID_ind are inconsistent")
      end if

      ! Look for any of the ID's
      call look_for_any_item_(self%file,ID,from=self%start_of_data,until=self%end_of_data,found=fnd,exact_match=TRUE)

      ! Found?
      if (present(found)) then
         found = fnd
         if (NOT found) return
      else if (NOT fnd) then
         DIE("CIF:find_looped_items ... no data items found")
      end if

      ! Look backwards for the loop_ start
      call look_backwards_for_item_(self%file,"loop_",until=self%start_of_data,found=fnd)

      ! Found?
      if (present(found)) then
         found = fnd
         if (NOT found) return
      else if (NOT fnd) then
         DIE("CIF:find_looped_items ... at least one data item is not looped")
      end if

      ! Item no. in ID
      item = 0

      ! The _data label no.
      label = 0

      ! Loop over ID items / _data labels 
      do

         ! Anything to read?
         if (at_EOF_(self%file)) then
            DIE_IF(label==0,"CIF:find_looped_items ... EOF, no _data labels!")
            DIE_IF(label>0,"CIF:find_looped_items ... EOF after _data labels i.e. no loop data!")
            exit
         end if

         ! Read looped _data item
         call read_(self%file,word)
         if (any(ID==word)) then

            ! A matching ID.
            item = item+1
            label = label+1
            if (present(ID_pos)) ID_pos(item) = label
            if (present(ID_ind)) ID_ind(item) = index_of_(ID,word)
         else if (word(1:1)=="_") then

            ! A looped _data descriptor
            label = label+1
         else

            ! Not a loop descriptor
            exit
         end if
      end do

      ! Found?
      fnd = item==size(ID) AND NOT has_repetitions_(ID_ind)
      if (present(found)) then; found = fnd
      else if (NOT fnd) then; DIE("CIF:find_looped_items ... not all data items found")
      end if

      ! Leave cursor before first data item
      call move_to_previous_item_(self%file)

      ! Return no. of _data labels
      if (present(n_labels)) n_labels = label
   end subroutine


! Read multiple looped STR item

   subroutine read_looped_items_0(self,ID,mat,found)
   ! In a loop_, read items with string identifiers "ID" into rows
   ! of "mat", with columns equal to the number of loops. "mat" is
   ! created. If present, "found" is set TRUE if all the "ID" are
   ! found; else it is set FALSE and routine returns without error.
      type(CIF_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: ID
      MAT(STR(len=*),:,:), allocatable, OUT :: mat
      BIN, optional, OUT :: found
      VEC(INT,size(ID)) :: ID_pos,ID_ind
      INT :: n_labels,n,item,label,ind
      STR(len=STR_SIZE) :: word,val
   ENSURE(allocated(self%file),"CIF:read_looped_items_0 ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_looped_items_0 ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_looped_items_0 ... no data block found")

      ! Find looped ID's, their position, no. of looped labels
      call find_looped_items_(self,ID,found,ID_pos,ID_ind,n_labels)

      ! Return if not found
      if (present(found)) then
         if (NOT found) return
      end if

      ! Create the first column
      call create_(mat,size(ID),1)

      ! The ID data column
      n = 1

      ! Loop on all ID data columns
      do
         item = 1

         ! Go through all _data labels
         do label = 1,n_labels

            ! Anything to read?
            if (at_EOF_(self%file)) then
               DIE_IF(item==1,"CIF:read_looped_items_0 ... EOF before reading first ID!")
               DIE_IF(item/=size(ID),"CIF:read_looped_items_0 ... EOF before all ID's were preocessed!")
               exit
            end if
            if (item>size(ID)) then

               ! Skip any remaing labels
               call skip_next_item_(self%file)
            else if (label/=ID_pos(item)) then

               ! Skip unwanted labels
               ! NOTE: ID_pos is monotone increasing
               call skip_next_item_(self%file)
            else

               ! Label found; get it's ID index
               ind = ID_ind(item)

               ! Read the _data into "mat"
               call read_(self%file,val)
               mat(ind,n) = val

               ! Next ID label
               item = item+1
            end if
         end do

         ! Anything to read?
         if (at_EOF_(self%file)) exit

         ! End of loop?
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:1)==";") exit  ! a text field ends the loop
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit

         ! Must be another round of ID's
         n = n+1
         call expand_columns_(mat,n)
      end do
   end subroutine


! Read multiple looped REAL item with (error)

   subroutine read_looped_items_1(self,ID,mat,error,found,transpose)
   ! In a loop_, read items with string identifiers "ID" into rows
   ! of "mat", with columns equal to the number of loops. "mat" is
   ! created. Assoaciated errors are placed in "error". If present,
   ! "found" is set TRUE if all the "ID" are found; else it is set
   ! FALSE and routine returns without error.
      type(CIF_TYPE), INOUT :: self
      VEC(STR(len=*),:), OUT :: ID
      MAT(REAL,:,:), allocatable, OUT :: mat
      MAT(REAL,:,:), allocatable, optional, OUT :: error
      BIN, optional, OUT :: found
      BIN, optional, IN :: transpose
      VEC(INT,size(ID)) :: ID_pos,ID_ind
      INT :: n_labels,n,item,label,ind
      STR(len=STR_SIZE) :: word
      REAL :: val,err
      BIN :: flip
   ENSURE(allocated(self%file),"CIF:read_looped_items_1 ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_looped_items_1 ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_looped_items_1 ... no data block found")
      flip = FALSE
      if (present(transpose)) flip = TRUE

      ! Find looped ID's, their position, no. of looped labels
      call find_looped_items_(self,ID,found,ID_pos,ID_ind,n_labels)

      ! Return if not found
      if (present(found)) then
         if (NOT found) return
      end if

      ! Create the first column
      if (flip) then
         call create_(mat,1,size(ID))
         if (present(error)) then
            call create_(error,1,size(ID))
            error = ZERO
         end if
      else
         call create_(mat,size(ID),1)
         if (present(error)) then
            call create_(error,size(ID),1)
            error = ZERO
         end if
      end if

      ! The ID data column
      n = 1

      ! Loop on all ID data columns
      do
         item = 1

         ! Go through all _data labels
         do label = 1,n_labels

            ! Anything to read?
            if (at_EOF_(self%file)) then
               DIE_IF(item==1,"CIF:read_looped_items_1 ... EOF before reading first item in ID!")
               DIE_IF(item/=size(ID),"CIF:read_looped_items_1 ... EOF before all ID's were preocessed!")
               exit
            end if
            if (item>size(ID)) then

               ! Skip any remaing labels
               call skip_next_item_(self%file)
            else if (label/=ID_pos(item)) then

               ! Skip unwanted labels
               ! NOTE: ID_pos is monotone increasing
               call skip_next_item_(self%file)
            else

               ! Label found; get it's ID index
               ind = ID_ind(item)

               ! Read the _data into "mat" & "error"
               call read_(self%file,val,err)
               if (flip) then
                  mat(n,ind) = val
                  if (present(error)) error(n,ind) = err
               else
                  mat(ind,n) = val
                  if (present(error)) error(ind,n) = err
               end if

               ! Next ID label
               item = item+1
            end if
         end do

         ! Anything to read?
         if (at_EOF_(self%file)) exit

         ! End of loop?
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:1)==";") exit  ! a text field ends the loop
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit

         ! Must be another round of ID's
         n = n+1
         if (flip) then
            call expand_rows_(mat,n)
            if (present(error)) call expand_rows_(error,n)
         else
            call expand_columns_(mat,n)
            if (present(error)) call expand_columns_(error,n)
         end if
      end do
   end subroutine


!  ======================
!  Read CIF covariance mx
!  ======================

   subroutine read_CIF_covariance(self,kind,covariance_mx)
   ! Read information from a Crystallographic Information File "cif".
      type(CIF_TYPE), INOUT :: self
      STR(len=*), IN :: kind
      MAT(REAL,:,:), allocatable, OUT :: covariance_mx
      VEC(REAL,:), allocatable :: cov
      INT :: np
      BIN :: there

      ! Find appropriate kind of covariance matrix
      call find_item_(self,trim(kind)//"_covariance_mx",there)
      if (there) then

         ! Read and set it
         call read_all_item_(self,trim(kind)//"_covariance_mx",cov,"[","]")
         np = nint(sqrt(REALIFY(size(cov))))
         covariance_mx = reshape(cov,[np,np])
         call destroy_(cov)
      end if
   end subroutine


!  ===
!  Put
!  ===

   subroutine put(self)
   ! Put the isosurface data
      type(CIF_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"CIF data:")
      call flush_(stdout)
      call show_(stdout,"File name                =",self%file_name)
      call show_(stdout,"Data block name          =",self%data_block_name)
      call show_(stdout,"Start of data            =",self%start_of_data)
      call show_(stdout,"Data block found         =",self%data_block_found)
      call show_(stdout,"BH_bond_length           =",self%BH_bond_length)
      call show_(stdout,"CH_bond_length           =",self%CH_bond_length)
      call show_(stdout,"NH_bond_length           =",self%NH_bond_length)
      call show_(stdout,"OH_bond_length           =",self%OH_bond_length)
   end subroutine

end module
