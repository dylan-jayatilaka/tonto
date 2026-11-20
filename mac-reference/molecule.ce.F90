!==============================================================================-
!
! MOLECULE.CE: CrystalExplorer related routines
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
!
!==============================================================================-
module MOLECULE_CE_MODULE

#  include "macros"

   include "molecule.ce.use"

   implicit none

   include "molecule.ce.int"


contains

!  ======================
!  Read CIF block options
!  ======================

   subroutine read_CX_file_name(self)
   ! Read the CX file name
      type(MOLECULE_TYPE), INOUT :: self
      call read_(stdin,self%CX_file_name)
   end subroutine

   subroutine read_CX_uses_angstrom(self)
   ! Read whether CX should use angstroms
      type(MOLECULE_TYPE), INOUT :: self
      call read_(stdin,self%CX_uses_angstrom)
   end subroutine

   subroutine read_CX_isosurface(self)
   ! Read in special crystalexplorer isosurface data. This only works
   ! if there is a .saved.cluster created . This CX isosurface differs
   ! from a normal isosurface in that the default box sizes for the
   ! isosurface are set from the cluster fragment (as opposed to the
   ! .atom list for normal isosurfaces), and there is a default
   ! isosurface kind, which is the "stockholder_density".  
   ! NOTE: the isosurface has its own plotgrid, different from that
   ! used in normal density plots, but .plot_grid is assigned to it.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%saved),"MOLECULE.CE:read_CX_isosurface ... no saved molecule")
   ENSURE(allocated(self%saved%cluster),"MOLECULE.CE:read_CX_isosurface ... no saved cluster data")
   ENSURE(self%saved%cluster%info_made,"MOLECULE.CE:read_CX_isosurface ... no saved cluster info")
   ENSURE(allocated(self%saved%atom),"MOLECULE.CE:read_CX_isosurface ... no saved atom list")
   ENSURE(allocated(self%atom),"MOLECULE.CE:read_CX_isosurface ... no atom list")
      if (NOT allocated(self%isosurface)) call create_(self%isosurface,self%atom)
      call set_defaults_(self%isosurface%plot_grid,self%saved%atom)
      self%isosurface%plot_grid%n_x = 2**ISOSURFACE_MINIMUM_SCAN_LEVEL+1
      call set_points_widths_origin_(self%isosurface%plot_grid)
      self%isosurface%property = "stockholder_weight"

      !!!!!!!!!!!!!!!!!!!!!!!!!
      call read_keywords_(self%isosurface)
      !!!!!!!!!!!!!!!!!!!!!!!!!

      ! Use unit cell as bbox if requested
      if (self%isosurface%plot_grid%use_unit_cell_as_bbox) then
         WARN("MOLECULE.CE:read_CX_isosurface ... resetting plot axes, plot widths from unit cell")
         call set_bbox_from_unit_cell_(self%isosurface%plot_grid,self%saved%cluster%crystal%unit_cell)
      end if
   end subroutine


!  =========================
!  Process CIF block options
!  =========================

   subroutine process_CIF_for_CX(self)
   ! Process the entire .cif file by reading *all* acceptable data
   ! blocks (i.e. those with crystal and atom information) and then
   ! writing this information to a CE information file. 
   ! NOTE: on termination, this does NOT produce a valid atom list.
   !       For that, use process_CIF
      type(MOLECULE_TYPE), allocatable, INOUT :: self
      BIN :: found,CX_uses_angstrom
      type(TEXTFILE_TYPE), allocatable :: CX_file
      type(CIF_TYPE), allocatable :: cif
   ENSURE(self%CX_file_name/=" ","MOLECULE.CE:process_CIF_for_CX ... no CX file name")
   ENSURE(allocated(self%cif),"MOLECULE.CE:process_CIF_for_CX ... no CIF object created")
   ENSURE(self%cif%file_name/=" ","MOLECULE.CE:process_CIF_for_CX ... no CIF file name specified")
      call create_(CX_file,self%CX_file_name)
      call open_for_(CX_file,"write")

      ! Save .cif outside of self, since it gets destroyed
      call move_alloc(self%cif,cif)
      do

         ! Find next data block
         call find_CIF_crystal_data_block_(self,cif,found)

         ! Any data left?
         if (NOT found) exit

         ! Read and create in from cif (leaky)
         call read_CIF_atoms_(self,cif)
         call read_CIF_crystal_(self,cif)

         ! Create the cluster
         call create_(self%cluster)
         call set_defaults_(self%cluster,self%crystal)
         call set_radius_(self%cluster,ZERO)
         call set_generation_method_(self%cluster,"fragment")
         call set_defragment_(self%cluster,FALSE)
         call update_info_(self%cluster)
         call put_(self%cluster)

         ! Write out 
         call put_CX_data_(self,CX_file,cif)

         ! Preserve
         CX_uses_angstrom = self%CX_uses_angstrom

         !!!!!!!!!!!!!!
         ! Destroy self
         call destroy_(self)
         call create_(self)
         !!!!!!!!!!!!!!

         ! Next 
         self%CX_uses_angstrom = CX_uses_angstrom
         call move_to_end_of_data_(cif)
      end do

      ! Clean
      call destroy_(cif)
      call destroy_(CX_file)
   end subroutine

   subroutine process_CIF_cell_and_symmetry(self)
   ! Process only the cell and spacegroup from the CIF file.
   ! If ".cif.data_block_name" is not defined, use the first
   ! acceptable data block.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%cif),"MOLECULE.CE:process_CIF_cell_and_symmetry ... no CIF object created")

      ! Find a data block name
      if (self%cif%data_block_name==" ") then
         WARN("MOLECULE.CE:process_CIF_cell_and_symmetry ... No CIF data block as specified")
         WARN("MOLECULE.CE:process_CIF_cell_and_symmetry ... Searching for first acceptable data block")
         call find_CIF_crystal_data_block_(self,self%cif)
      end if

      ! Find the data block
      call open_CIF_at_data_block(self%cif)

      ! Read crystal info
      call read_CIF_crystal_(self,self%cif)
   end subroutine

   subroutine process_CIF(self,skip_HA_inputs)
   ! Process a CIF file: read atom and crystal information.
   ! If ".cif.data_block_name" is not defined, use the first
   ! acceptable data block.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: skip_HA_inputs
      BIN :: do_HA_inputs
   ENSURE(allocated(self%cif),"MOLECULE.CE:process_CIF ... no CIF object created")
      do_HA_inputs = TRUE
      if (present(skip_HA_inputs)) do_HA_inputs = NOT skip_HA_inputs

      ! Find a data block name
      if (self%cif%data_block_name==" ") then
         call find_CIF_crystal_data_block_(self,self%cif)
      end if

      ! Find the data block
      call open_CIF_at_data_block(self%cif)

      ! Read crystal info, ensure .atom's are cartesian
      call read_CIF_crystal_only_(self,self%cif)

      ! Assume there is X-ray data
      if (allocated(self%crystal)) then
         call read_CIF_xray_data_(self%crystal,self%cif)
      end if

      ! Read in atoms of asymmetric unit
      ! If no .crystal.unit_cell, .atom's remain fractional
      call read_CIF_atoms_only_(self,self%cif)

      ! Update any crystal fragment_atom info, positions.
      call update_CIF_crystal_atoms_(self,self%cif)

      ! Make Hirshfeld info
      if (self%basis_info_made) then
         if (allocated(self%SCF_data)) then
            if (allocated(self%atom)) then
               if (allocated(self%crystal%xray_data)) then
                  if (using_one_center_TF_model_(self%crystal)) then
                     if (do_HA_inputs) then
                        call make_ANOs_and_interpolators_(self)
                     end if
                  end if
               end if
            end if
         end if
      end if

      ! Reset to finalize next time
      self%crystal%finalize = TRUE
   end subroutine

   subroutine find_CIF_crystal_data_block(self,cif,found)
   ! Find the *first* crystal data block in the "cif" file
   ! starting from line cif.start_of_data, and set the locator
   ! "cif.data_block_name". The name of the molecule is set to 
   ! the name of the cif file concatenated with the block name.  
      type(MOLECULE_TYPE), INOUT :: self
      type(CIF_TYPE) :: cif
      BIN, optional, OUT :: found
      BIN :: fnd
   ENSURE(allocated(cif%file),"MOLECULE.CE:find_CIF_crystal_data_block ... no CIF file name")
   ENSURE(cif%file%name/=" ","MOLECULE.CE:find_CIF_crystal_data_block ... no CIF file name")

      ! Set name
      if (self%name==" " OR self%name=="unknown") then
         self%name = trim(filename_head_(cif%file%name))
      end if
      if (NOT is_open_(cif%file)) call open_(cif)
      call find_crystal_data_block_(cif,fnd)
      if (present(found)) then
         found = fnd
         if (NOT found) return
      else
         DIE_IF(NOT fnd,"MOLECULE.CE:find_CIF_crystal_data_block ... couldn't find crystal data block")
      end if
   end subroutine

   subroutine open_CIF_at_data_block(cif)
   ! Open the "cif" file at the block with name "cif.data_block_name".
      type(CIF_TYPE), INOUT :: cif
      STR(len=STR_SIZE) :: name
   ENSURE(cif%file_name/=" ","MOLECULE.CE:open_CIF_at_data_block ... no CIF file name")
   ENSURE(cif%data_block_name/=" ","MOLECULE.CE:open_CIF_at_data_block ... no CIF data block name")

      ! Open?
      if (NOT is_open_(cif%file)) call open_(cif)

      ! Store: cif.data_block_name is trashed
      name = cif%data_block_name
      call find_data_block_called_(cif,name)
   end subroutine


!  ======================
!  New cluster operations
!  ======================

   subroutine defragment(self)
   ! Defragment the current atom list, assuming it is read in from CIF
   ! file and may consist of just an asymmetric unit.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%cif),"MOLECULE.CE:defragment ... no CIF info")
   ENSURE(allocated(self%crystal),"MOLECULE.CE:defragment ... no crystal")

      ! Defragment any molecules in the CIF
      call set_generation_method_(self%crystal,"fragment")
      call set_radius_(self%crystal,ZERO)
      call set_defragment_(self%crystal,TRUE)
      call make_cluster_info_(self%crystal)

      ! Reset .atom list to the .crystal.geometry
      call destroy_(self%atom)
      call create_atom_list_(self%crystal,self%atom)

      ! Finalize atom info/bases & atom agroups
      self%atom_info_made = FALSE
      call destroy_atom_group_(self)
      call update_atom_basis_group_info_(self)

      ! Redine crystal fragment
      call destroy_asymmetric_unit_(self%crystal)
      call update_fragment_info_(self%crystal,self%atom)
   ! .update_atom_basis_group_info ! Why a 2nd time?

      ! Make Hirshfeld info
      if (self%basis_info_made) then
         if (allocated(self%SCF_data)) then
            if (allocated(self%crystal)) then
               if (allocated(self%crystal%xray_data)) then
                  if (using_one_center_TF_model_(self%crystal)) then
                     call make_ANOs_and_interpolators_(self)
                  end if
               end if
            end if
         end if
      end if
   end subroutine

   subroutine phar_defragment(self)
   ! For periodic HAR. Find atoms for the bond analysis
   ! This updates .crystal.fragment_atom for further geometry analysis, while keeps 
   ! (.molecule).atom for the DFT calculation.
   ! Make sure that the updated crystal has the same ordred (.crystal).asymmetric_unit_atom 
   ! and they should have the identical counter part in (molecule).atom(:)
   ! It is recommended to put this keyword between "crystal= { ... }" and "ha_fit" keywords.
   ! Otherwise, "crystal= {" will reset the defragmented atom groups.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), allocatable :: tmp_Aatom
      VEC(type(ATOM_TYPE),:), allocatable :: umol_atom  ! new source for fragment_atom from unqiue molecules
      INT :: i,oi,m,um
      VEC(INT,:), allocatable :: prune_ind,Z,Zs,perm
      BIN :: b
   ENSURE(allocated(self%crystal),"MOLECULE.CE:phar_defragment ... no crystal")
      call create_(tmp_Aatom,self%crystal%n_asymmetric_unit_atoms)
      tmp_Aatom = self%crystal%asymmetric_unit_atom

      !stdout.text("Kang Debug: before defragment: .crystal.fragment_atom")
      !.crystal.fragment_atom.put_coordinates(no_header=TRUE)
      call set_generation_method_(self%crystal,"fragment")
      call make_occupation_list_(self%crystal)  ! ".crystal.occupation_list" is set.
                                    ! Note: .crystal.unit_cell_atom_for_frag_atom(f) is .crystal.unit_cell_atom(u) 
      call do_defragment_v2_(self%crystal)  ! Grow fragment atoms by "do_defragment_v2" 
                                 ! - The first ".n_fragment_atoms" in the ".occupied_list" is the same to ".crystal.frag
                                 ! - The followings are available
                                 !   (.crystal).molecule_for_atom :: VEC{INT}@         group indices. 
                                 !             .molecule          :: VEC{EVEC{INT}}@  
                                 !             .n_molecules
                                 !             .n_unique_molecules
                                 !             .unique_molecule_for :: VEC{INT}@     unique molecules have value 0

      ! extract the atoms from the unique molecules from the occupation list
      call create_(prune_ind,0)

      ! Prune occupation list for new fragment atom
      do oi = 1,size(self%crystal%occupation_list)
         m = self%crystal%molecule_for_atom(oi)
         um = self%crystal%unique_molecule_for(m)
         if (NOT um==0) call append_(prune_ind,oi)  ! If oi-atom is not unique molecule, append "oi" to prune_ind(:)
      end do

      ! Prune occupation list. leaving atoms belonging to the unique molecules.
      do i = size(prune_ind),1,-1  ! Reverse order is important, from tail to head pruning
         call prune_element_(self%crystal%occupation_list,prune_ind(i))
         call prune_element_(self%crystal%molecule_for_atom,prune_ind(i))
      end do
      self%crystal%n_atoms = size(self%crystal%occupation_list)  ! Manually update .crystal.n_atoms 
      call destroy_(prune_ind)  ! > It should be same to ".crystal.occupation_list.dim"

      !! Sort occupation_list based on atomic_number, so the .crystal.fragment_atom will be sorted
!>DEBUG      .crystal.put_occupation_list  ! Note: .crystal.n_atom = .crystal.occupation_list.dim
      call create_(Z,self%crystal%n_atoms)
      call create_(Zs,self%crystal%n_atoms)
      call create_(perm,self%crystal%n_atoms)
      Z = CLUSTER_UA_FROM_CODE(self%crystal%occupation_list)
      Z = self%crystal%unit_cell_atom(Z)%atomic_number
      Zs = Z
      call sort_(Zs,decreasing_order=TRUE)
      call is_permutation_of_(Zs,Z,b,perm)  ! this is a way to get the index ordering from the sorting, b::BIN is dummy
      self%crystal%occupation_list = self%crystal%occupation_list(perm)
      self%crystal%molecule_for_atom = self%crystal%molecule_for_atom(perm)
!>DEBUG      .crystal.put_occupation_list ! occupation list after sorting

      !! Kang's note.
      ! xml_atom's and cif_atom's ADPs are saved at (.molecule).atom 
      ! The ".crystal" object is created by ".update_fragment_info( .atom )"
      ! So,>  i) "crystal.asymmetric_unit_atom:: VEC{ATOM}@" id selected from xml_atom.
      !      ii) "crystal.fragment_atom       :: VEC{ATOM}@" is the same with "xml_atom" from CRYSTAL23
      !     iii) "crystal.occupation_list     :: VEC{INT}@"  is created from ".crystal.fragment_atom", then "do_defragment_v2"ed
      ! until now.
      ! Now, we are going to separate (.molecule).atom and .crystal.fragment_atom. Sorry Dylan ;) 
      ! .atom                        is for set_shell1_from(...) 
      ! .crystal.fragment_atom       is for geometry analysis
      ! .crystal.asymmetric_unit_atom will be found agian from xml_atom(= .atom )
      call create_atom_list_(self%crystal,umol_atom)  ! unique molecule atom is ready.
      call update_(umol_atom)  ! How can I redefine label or tags?
      call destroy_asymmetric_unit_(self%crystal)
      call update_fragment_info_(self%crystal,umol_atom)  ! now crystal.fragment_atom are defragmented

!>DEBUG  Show NEW .asymmetric_unit_atom ")
!>      .crystal.asymmetric_unit_atom.put_coordinates(no_header=TRUE)
!>      .crystal.asymmetric_unit_atom.put_ADPs(no_header=TRUE) ! These new asymmetric atom should be consistent with .beck_grid.atom & .becke_grid.unique_atom_for
   end subroutine


!  ===
!  Put
!  ===

   subroutine put_CX_data_0(self)
   ! Output information that Crystal Explorer requires to the file 
   ! with name ".CX_file_name".
      type(MOLECULE_TYPE), INOUT :: self
      call put_CX_data_(self,self%cif)
   end subroutine

   subroutine put_CX_data_1(self,cif)
   ! Output information that Crystal Explorer requires to the file 
   ! with name ".CX_file_name".
      type(MOLECULE_TYPE), INOUT :: self
      type(CIF_TYPE), allocatable :: cif
      type(TEXTFILE_TYPE), allocatable :: CX_file
      if (self%CX_file_name/=" ") then
         call create_(CX_file,self%CX_file_name)
         call open_for_(CX_file,"write")
         call put_CX_data_(self,CX_file,cif)
         call close_(CX_file)
         call destroy_(CX_file)
      else
         call put_CX_data_(self,stdout,cif)
      end if
   end subroutine

   subroutine put_CX_data_2(self,CX_file,cif)
   ! Output information that Crystal Explorer requires to file "CX_file".
   ! NOTE: this requires a "cif" file to have been read, and a valid data block
   ! name to be stored in the "cif" object.
      type(MOLECULE_TYPE), INOUT :: self
      type(TEXTFILE_TYPE), IN :: CX_file
      type(CIF_TYPE), allocatable, IN :: cif

! ENSURE(cif.allocated,"no CIF object created")
! ENSURE(cif.data_block_name/=" ","no CIF data block name")
      type(TEXTFILE_TYPE), allocatable :: save
      STR(len=STR_SIZE) :: CX_label,CX_name,formula,spacegroup
      VEC(INT,:), allocatable :: in,out
      BIN :: done
      if (allocated(cif)) then
         CX_name = cif%file_name
         CX_label = cif%data_block_name
      else
         CX_name = "unknown"
         CX_label = "unknown"
      end if
      call flush_(stdout)
      call text_(stdout,"Writing Crystal Explorer data file: "//trim(CX_file%name))
      call flush_(stdout)

      ! Redirect stdout to out
      save = stdout
      stdout = CX_file

      ! Reset output format
      call save_style_(stdout)
      call set_int_width_(stdout,7)
      call set_real_fortran_fmt_(stdout,"f")
      call set_real_precision_(stdout,5)
      call set_real_width_(stdout,12)
      call set_using_fields_(stdout,FALSE)
      call flush_(stdout)
      call text_(stdout,"Crystal Explorer data")
      call flush_(stdout)
      call show_(stdout,"begin CIF ",CX_label,dots=FALSE)
      call show_(stdout,"file_name =",CX_name,dots=FALSE)
      call show_(stdout,"data_block_name =",CX_label,dots=FALSE)
      call text_(stdout,"end CIF")
      call flush_(stdout)

      ! Crystal data and cluster/fragment positions
      ! Try saved cluster first
      done = FALSE
      if (allocated(self%saved)) then
         if (allocated(self%saved%cluster)) then
            ENSURE(self%saved%cluster%info_made,"MOLECULE.CE:put_CX_data_2 ... no cluster information")
            ENSURE(allocated(self%saved%cluster%crystal),"MOLECULE.CE:put_CX_data_2 ... no cluster crystal data")
            ENSURE(self%saved%cluster%n_fragment_atoms>0,"MOLECULE.CE:put_CX_data_2 ... no fragment size")
            formula = self%saved%cluster%crystal%CIF_chemical_formula
            spacegroup = self%saved%cluster%crystal%spacegroup%HM_symbol
            call show_(stdout,"begin crystal ",CX_label,dots=FALSE)
            call put_CX_(self%saved%cluster%crystal,CX_label,formula,spacegroup)
            call put_CX_(self%saved%cluster,angstrom=self%CX_uses_angstrom)
            call flush_(stdout)
            call text_(stdout,"end crystal")
            done = TRUE
         end if
      end if

      ! Try normal cluster
      if (NOT done) then
         if (allocated(self%cluster)) then
            ENSURE(self%cluster%info_made,"MOLECULE.CE:put_CX_data_2 ... no cluster information")
            ENSURE(allocated(self%cluster%crystal),"MOLECULE.CE:put_CX_data_2 ... no cluster crystal data")
            ENSURE(self%cluster%n_fragment_atoms>0,"MOLECULE.CE:put_CX_data_2 ... no fragment size")
            formula = self%cluster%crystal%CIF_chemical_formula
            spacegroup = self%cluster%crystal%spacegroup%HM_symbol
            call show_(stdout,"begin crystal ",CX_label,dots=FALSE)
            call put_CX_(self%cluster%crystal,CX_label,formula,spacegroup)
            call put_CX_(self%cluster,angstrom=self%CX_uses_angstrom)
            call flush_(stdout)
            call text_(stdout,"end crystal")
            done = TRUE
         end if
      end if

      ! Finally try .crystal and .atom
      if (NOT done) then
         if (allocated(self%crystal)) then
            if (allocated(self%atom)) then
               formula = self%crystal%CIF_chemical_formula
               spacegroup = self%crystal%spacegroup%HM_symbol
               call show_(stdout,"begin crystal =",CX_label)
               call put_CX_(self%crystal,CX_label,formula,spacegroup)  ! INOUT
               call put_CX_(self%atom,CX_label,angstrom=self%CX_uses_angstrom)
               call flush_(stdout)
               call text_(stdout,"end crystal")
               done = TRUE
            end if
         end if
      end if

      ! Isosurface information
      if (allocated(self%isosurface)) then
         if (allocated(self%isosurface%atom)) then

         ! Saved cluster must exist
            done = FALSE
            if (allocated(self%saved)) then
               if (allocated(self%saved%cluster)) then

            ! Surface name
                  call flush_(stdout)
                  if (allocated(self%saved%isosurface)) then

               ! saved_isosurface_plot
                     call show_(stdout,"begin surface ",trim(surface_label_(self%saved%isosurface)),dots=FALSE)
                  else

               ! Normal isosurface_plot
                     call show_(stdout,"begin surface ",trim(surface_label_(self%isosurface)),dots=FALSE)
                  end if

            ! Surface vertices with IN and OUT atoms
                  in = fragment_atom_indices_(self%saved%cluster)
                  out = nonfragment_atom_indices_(self%saved%cluster)
                  if (allocated(self%saved%isosurface)) then

               ! saved_isosurface_plot
                     call put_CX_(self%saved%isosurface,in,out,self%atom,angstrom=self%CX_uses_angstrom)
                  else

               ! Normal isosurface_plot
                     call put_CX_(self%isosurface,in,out,self%atom,angstrom=self%CX_uses_angstrom)
                  end if

            ! Cluster atoms
                  call put_CX_atoms_(self%saved%cluster)

            ! Fingerprint data
                  if (allocated(self%saved%isosurface)) then

               ! saved_isosurface_plot
                     call put_fingerprint_face_atoms_(self%saved%isosurface,in,out)
                  else

               ! Normal isosurface_plot
                     call put_fingerprint_face_atoms_(self%isosurface,in,out)
                  end if
                  call text_(stdout,"end surface")
                  done = TRUE
               end if
            end if

         ! Try normal cluster
            if (NOT done) then
               if (allocated(self%cluster)) then
                  DIE("MOLECULE.CE:put_CX_data_2 ... should we be here?")
                  done = TRUE
               end if
            end if

         ! Finally try isosurface only
            if (NOT done) then
               DIE("MOLECULE.CE:put_CX_data_2 ... should we be here?")
               done = TRUE
            end if
         end if
      end if

      ! Put back stdout
      call unsave_style_(stdout)
      stdout = save
   end subroutine

end module
