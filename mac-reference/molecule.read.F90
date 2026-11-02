!===============================================================================
!
! MOLECULE.READ
!
! An object representation of a molecule.
!
! These are base methods needed by all other submodules.
!
! The submodule hierarchy is shown below.
! . The numbers (if any) indicate (roughly) the number of used
!   procedures from the module at the head of the column. This is
!   useful if you want to decouple the modules even further.
! . The type of routines in each submodule is more-or-less
!   straightforward to understand from the three- or four-letter
!   pneumonic e.g. REL is for relativistic routines.
!
! Updated 2014-12-01
!
! .BASE
!     3 .REL
!    16---24 .GRID
!    14---12---- 1 .XTAL
!     2--- |---- |---- 4 .CE =
!     7--- |---- 1---- |-- | .INTS
!    14--- |---- 5---- |-- |---- 2 .FOCK
!    30--- 5---- 4---- 5-- |----10---- 5 .SCF
!     2--- |---- |---- |-- |---- 1---- 5--- 1 .GEM =     =     =
!    11--- |---- |---- 1-- |----15---- 5---13--- | .PROP =     =
!     4--- |---- 6---- |-- |---- 3---- |--- 2--- |---- | .PLOT =
!     4--- |---- 5---- |-- |---- 4---- 2--- 1--- |---- |---- | .CP
!    12--- |---- 7---- |-- |---- |---- |--- 1--- |---- |---- 1-- | .TAD
!    15--- |---- 5---- 1-- |---- 5---- 6--- 5--- |---- |---- |-- |--- 2 .MISC
!    76--- 3---- 3----13-- 7---- 1---- |---13--- 3----19---- 6-- 9---11----24 .MAIN
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1996
! Copyright (C) Dylan Jayatilaka 2012-
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
!===============================================================================
module MOLECULE_READ_MODULE

#  include "macros"

   include "molecule.read.use"

   implicit none

   include "molecule.read.int"


contains

!  ================
!  Molden interface
!  ================

   subroutine read_molden_file(self,file_name,print_output)
   ! Read a Molden MOs file
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), optional, IN :: file_name
      BIN, optional, IN :: print_output
      type(TEXTFILE_TYPE), allocatable :: molden
      type(TEXTFILE_TYPE), allocatable :: save
      type(ARCHIVE_TYPE) :: archive
      VEC(REAL,3) :: pos
      VEC(REAL,:), allocatable :: orb
      VEC(INT,:), allocatable :: fbf,lbf,ang
      REAL :: fac,n_e,n_a,n_b
      REAL :: val,diff,energy,occ
      STR(len=STR_SIZE) :: name,units,item,label,spin
      STR(len=1) :: op,cl
      BIN :: output,found
      BIN :: is_unrestricted
      BIN :: is_correlated
      INT :: line,a,i,f,Z

      ! Get file name
      if (present(file_name)) then; name = file_name
      else if (NOT exhausted_(stdin%buffer)) then; call read_(stdin,name)
      else; name = self%name
      end if
      if (present(print_output)) then; output = print_output
      else; output = FALSE
      end if

      ! Constants
      op = achar(91)
      cl = achar(93)

      ! Open Molden file
      call create_(molden,name)
      call open_for_(molden,"read")

      ! Find out if this file is using a spherical basis
      line = line_number_(molden)
      call look_for_(molden,op//"5D"//cl,found=found)
      if (found) self%use_spherical_basis = TRUE
      call move_to_line_(molden,line)

      ! Find [ATOMS] block
      ! or [Atoms]
      call look_for_(molden,op//"Atoms"//cl,found=found)
      if (NOT found) call look_for_(molden,op//"ATOMS"//cl,found=found)
      DIE_IF(NOT found,"MOLECULE.READ:read_molden_file ... No ATOMS line")
      DIE_IF(n_line_items_(molden)/=2,"MOLECULE.READ:read_molden_file ... Expecting two items on ATOMS line")

      ! Read units
      call skip_next_item_(molden)
      call read_(molden,units)
      fac = ONE
      if (units=="Angs") fac = BOHR_PER_ANGSTROM

      ! Store start of atoms list
      call read_line_(molden)
      line = line_number_(molden)

      ! Get no. of atoms ...
      self%n_atom = 0
      do
         call read_(molden,item)
         if (item(1:1)==op) exit
         DIE_IF(molden%IO_status/=0,"MOLECULE.READ:read_molden_file ... file ended unexpectedly")
         self%n_atom = self%n_atom+1
         call read_line_(molden)
      end do

      ! Create atom list (leaky)
      if (allocated(self%atom)) call destroy_(self%atom)
      call create_(self%atom,self%n_atom)
      self%atom_info_made = FALSE

      ! Create basis -- Different one for every atom (leaky)
      self%basis_name = "molden"
      if (allocated(self%basis)) call destroy_(self%basis)
      call create_(self%basis,self%n_atom)
      if (self%use_spherical_basis) call set_spherical_(self%basis,TRUE)

      ! Move back to atoms line
      call move_to_line_(molden,line)

      ! Read atom label, Z, coordinates
      do a = 1,self%n_atom
         call read_(molden,label)  ! Atom label
         call skip_next_item_(molden)
         call read_(molden,Z)  ! Read Z
         call set_atomic_number_(self%atom(a),Z)
         call set_atom_data_from_label_(self%atom(a),label)
         call read_(molden,pos)  ! Position: change units
         pos = fac*pos
         call set_position_(self%atom(a),pos)
         ! Set basis label
         call set_basis_label_(self%atom(a),trim(generic_chemical_symbol_(self%atom(a)))//":molden")
      end do

      ! Read basis set
      call look_for_(molden,op//"GTO"//cl,found=found)
      DIE_IF(NOT found,"MOLECULE.READ:read_molden_file ... No GTO line")
      DIE_IF(n_line_items_(molden)/=1,"MOLECULE.READ:read_molden_file ... Expecting only one item on GTO line")
      call read_line_(molden)

      ! Read basis
      if (allocated(stdin)) save = stdin
      stdin = molden
      do a = 1,self%n_atom
         call read_molden_(self%basis(a))
         call set_label_(self%basis(a),self%atom(a)%basis_label)
      end do
      if (allocated(save)) stdin = save

      ! Remove repetitions
      call remove_repetitions_(self%basis)

      ! Find [MO] tag
      call look_for_(molden,op//"MO"//cl,found=found)
      DIE_IF(NOT found,"MOLECULE.READ:read_molden_file ... No MO line")
      DIE_IF(n_line_items_(molden)/=1,"MOLECULE.READ:read_molden_file ... Expecting only one item on MO line")
      line = line_number_(molden)+1

      ! Any Beta tag?
      call look_for_(molden,"Beta",found=is_unrestricted)

      ! Move back to [MO]'s
      call move_to_line_(molden,line)

      ! Read occupations & get no. of electrons
      is_correlated = FALSE
      n_e = ZERO
      do
         call look_for_(molden,"Occup=",found=found)
         if (NOT found) exit
         call move_to_line_item_(molden,2)
         call read_(molden,val)
         call read_line_(molden)
         n_e = n_e+val
         diff = val-nint(val)
         if (diff>TOL(4)) is_correlated = TRUE
      end do

      ! Now set charge
      self%n_e = nint(n_e)
      self%charge = no_of_electrons_(self%atom)-self%n_e

      ! Set multiplicity
      self%spin_multiplicity = default_spin_multiplicity_(self)

      ! Finalize atom info/basis sets 
      ! And default multiplicity ...
      call update_atom_basis_group_info_(self)

      ! Clean up (leaky)
      call destroy_(self%SCF_data)
      call destroy_(self%density_mx)
      call destroy_(self%MO_energies)
      call destroy_(self%MOs)
      call destroy_(self%NO_occ_nos)
      call destroy_(self%NOs)

      ! Read the MO or NO coefficients.
      ! Read the NO occupation numbers always.
      if (NOT is_correlated) then

         ! Reset multiplicity?
         if (is_unrestricted) then
            call move_to_line_(molden,line)
            n_a = ZERO
            do
               call look_for_(molden,"Alpha",found=found)
               if (NOT found) exit
               call look_for_(molden,"Occup=",found=found)
               if (NOT found) exit
               call move_to_line_item_(molden,2)
               call read_(molden,val)
               call read_line_(molden)
               n_a = n_a+val
            end do
            n_b = n_e-n_a  ! Set no. of beta electrons
            call set_n_a_n_b_and_multiplicity_(self,nint(n_a),nint(n_b))
         end if

         ! Orbital method
         call create_(self%SCF_data)
         if (is_unrestricted) then

            ! Unrestricted
            call set_SCF_kind_(self%SCF_data,"uhf")
            call create_(self%NO_occ_nos,self%n_bf,"u")
            self%NO_occ_nos%a = ZERO
            self%NO_occ_nos%b = ZERO
            call create_(self%MO_energies,self%n_bf,"u")
            self%MO_energies%a = ZERO
            self%MO_energies%b = ZERO
            call create_(self%MOs,self%n_bf,"u")
            self%MOs%a = ZERO
            self%MOs%b = ZERO
         else

            ! Restricted
            call set_SCF_kind_(self%SCF_data,"rhf")
            call create_(self%NO_occ_nos,self%n_bf,"r")
            self%NO_occ_nos%r = ZERO
            call create_(self%MO_energies,self%n_bf,"r")
            self%MO_energies%r = ZERO
            call create_(self%MOs,self%n_bf,"r")
            self%MOs%r = ZERO
         end if
      else

         ! Correlated method
         if (is_unrestricted) then

            ! Unrestricted
            call create_(self%NO_occ_nos,self%n_bf,"u")
            self%NO_occ_nos%a = ZERO
            self%NO_occ_nos%b = ZERO
            call create_(self%NOs,self%n_bf,"u")
            self%NOs%a = ZERO
            self%NOs%b = ZERO
         else

            ! Restricted
            call create_(self%NO_occ_nos,self%n_bf,"r")
            self%NO_occ_nos%r = ZERO
            call create_(self%NOs,self%n_bf,"r")
            self%NOs%r = ZERO
         end if
      end if

      ! Move back to [MO]'s
      call move_to_line_(molden,line)

      ! Read in orbitals and energies.
      call create_(orb,self%n_bf)
      i = 0
      do  ! over orbitals
         call look_for_(molden,"Ene=",found=found)
         if (NOT found) exit
         call skip_next_item_(molden)
         call read_(molden,energy)  ! Read energy
         call skip_next_item_(molden)
         call read_(molden,spin)  ! Read spin
         call skip_next_item_(molden)
         call read_(molden,occ)  ! Read occupation

         ! Read MO coefficients
         do f = 1,self%n_bf
            call skip_next_item_(molden)
            call read_(molden,orb(f))
         end do

         ! Increment orbital
         i = i+1
         if (NOT is_correlated) then

            ! Assign MO's
            if (is_unrestricted) then
               DIE_IF(i>2*self%n_bf,"MOLECULE.READ:read_molden_file ... too many orbitals specified")
               select case (spin)
               case ("Alpha")
                  self%NO_occ_nos%a(i) = occ
                  self%MO_energies%a(i) = energy
                  self%MOs%a(:,i) = orb
               case ("Beta")
                  self%NO_occ_nos%b(i-self%n_bf) = occ
                  self%MO_energies%b(i-self%n_bf) = energy
                  self%MOs%b(:,i-self%n_bf) = orb
               case default
                  DIE("MOLECULE.READ:read_molden_file ... unknown spin: "//trim(spin))
               end select
            else
               DIE_IF(i>self%n_bf,"MOLECULE.READ:read_molden_file ... too many orbitals specified")
               select case (spin)
               case ("Alpha")
                  self%NO_occ_nos%r(i) = occ
                  self%MO_energies%r(i) = energy
                  self%MOs%r(:,i) = orb
               case default
                  DIE("MOLECULE.READ:read_molden_file ... unexpected spin: "//trim(spin))
               end select
            end if
         else

            ! Assign NO's
            if (is_unrestricted) then
               select case (spin)
               case ("Alpha")
                  self%NO_occ_nos%a(i) = occ
                  self%NOs%a(:,i) = orb
               case ("Beta")
                  self%NO_occ_nos%b(i-self%n_bf) = occ
                  self%NOs%b(:,i-self%n_bf) = orb
               case default
                  DIE("MOLECULE.READ:read_molden_file ... unknown spin: "//trim(spin))
               end select
            else
               select case (spin)
               case ("Alpha")
                  self%NO_occ_nos%r(i) = occ
                  self%NOs%r(:,i) = orb
               case default
                  DIE("MOLECULE.READ:read_molden_file ... unexpected spin: "//trim(spin))
               end select
            end if
         end if
      end do

      ! Clean
      call destroy_(orb)
      fbf = self%first_basis_fn_for_shell
      lbf = self%last_basis_fn_for_shell
      ang = self%angular_moment_for_shell

      ! Swap orbital order
      ! Spherical basis should be in correct order
      ! Save data in archive files
      if (NOT is_correlated) then
         if (NOT self%use_spherical_basis) then
            if (is_unrestricted) then
               call swap_from_Molden_C_order_(self%MOs%a,"1",fbf,lbf,ang)
               call swap_from_Molden_C_order_(self%MOs%b,"1",fbf,lbf,ang)
            else
               call swap_from_Molden_C_order_(self%MOs%r,"1",fbf,lbf,ang)
            end if
         end if
      else
         if (NOT self%use_spherical_basis) then
            if (is_unrestricted) then
               call swap_from_Molden_C_order_(self%NOs%a,"1",fbf,lbf,ang)
               call swap_from_Molden_C_order_(self%NOs%b,"1",fbf,lbf,ang)
            else
               call swap_from_Molden_C_order_(self%NOs%r,"1",fbf,lbf,ang)
            end if
         end if
      end if
      call destroy_(ang)
      call destroy_(lbf)
      call destroy_(fbf)

      ! Make restricted density matrix
      ! Under assumption an RGBI is going to be done
      call destroy_(self%density_mx)
      call create_(self%density_mx,self%n_bf)
      if (NOT allocated(self%NOs)) self%NOs = self%MOs
      call make_density_mx_from_NOs_(self)
      if (NOT allocated(self%density_mx%r)) then
         call make_r_density_mx_(self)
      ! .make_r_NOs
      end if

      ! Write MO archives
      call set_(archive,self%name,"MOs")
      call write_(archive,self%MOs)
      call set_(archive,self%name,"MO_energies")
      call write_(archive,self%MO_energies)

      ! Write NO archive
      call set_(archive,self%name,"NOs")
      call write_(archive,self%NOs)
      call set_(archive,self%name,"NO_occ_nos")
      call write_(archive,self%NO_occ_nos)

      ! Set crystal data
      if (allocated(self%crystal)) then
         call resolve_axis_system_(self)
         call update_fragment_info_(self%crystal,self%atom)
      end if

      ! Clean
      call destroy_(molden)
   end subroutine


! WARNING: need to harmonise with above

!   read_molden_NOs(file_name) :: leaky
!   ! Read a Molden natural orbitals file.
!   ! Here the occupation numbers are not integers.
!      self :: INOUT
!      file_name :: STR, optional, IN
!
!   ENSURE(NOT .use_spherical_basis,"not implemented")
!
!      molden_file, save :: TEXTFILE@
!      archive :: ARCHIVE
!      pos :: VEC{REAL}(3)
!      orb :: VEC{REAL}@
!      fbf,lbf,ang :: VEC{INT}@
!      fac,n_e,val,energy,occ :: REAL
!      name,units,item,label,spin :: STR
!      op,cl :: STR(len=1)
!      found,is_unrestricted :: BIN
!      line, a,i,f,Z :: INT
!
!      ! Get file name
!      if (present(file_name)) then;              name = file_name
!      else if (NOT stdin.buffer.exhausted) then; stdin.read(name)
!      else;                                      name = .name
!      end
!
!      ! Constants
!      op = achar(91)
!      cl = achar(93)
!
!      ! Open Molden file
!      molden_file.create(name)
!      molden_file.open_for("read")
!
!      ! Find [ATOMS] block
!      molden_file.look_for(op//"ATOMS"//cl,found=found)
!      DIE_IF(NOT found, "No ATOMS line")
!      DIE_IF(molden_file.n_line_items/=2, "Expecting two items on ATOMS line")
!
!      ! Read units
!      molden_file.read(units)
!      fac = ONE
!      if (units=="Angs") fac = BOHR_PER_ANGSTROM
!
!      ! Store start of atoms list
!      molden_file.read_line
!      line = molden_file.line_number
!
!      ! Get no. of atoms ...
!      .n_atom = 0
!      do
!         molden_file.read(item)
!         if (item==op//"Molden") exit
!         DIE_IF(molden_file.IO_status/=0,"file ended unexpectedly")
!         .n_atom = .n_atom + 1
!         molden_file.read_line
!      end
!
!      ! Create atom list (leaky)
!      if (.atom.allocated)  .atom.destroy
!      .atom.create(.n_atom)
!      .atom_info_made = FALSE
!
!      ! Create basis -- Different one for every atom (leaky)
!      .basis_name = "molden"
!      if (.basis.allocated) .basis.destroy
!      .basis.create(.n_atom)
!
!      ! Move back to atoms line
!      molden_file.move_to_line(line)
!
!      ! For each atom a: read label, Z, coordinates
!      do a = 1,.n_atom
!
!         ! Read label
!         molden_file.read(label)
!
!         ! Read Z
!         molden_file.skip_next_item
!         molden_file.read(Z)
!
!         ! Set
!         .atom(a).set_atomic_number(Z)
!         .atom(a).set_atom_data_from_label(label)
!
!         ! Read position, change units
!         molden_file.read(pos)
!         pos = fac*pos
!         .atom(a).set_position(pos)
!
!         ! Set basis label
!         .atom(a).set_basis_label("molden-"//trim(.atom(a).generic_chemical_symbol))
!
!      end
!
!      ! Read basis set
!      molden_file.look_for(op//"GTO"//cl,found=found)
!      DIE_IF(NOT found, "No GTO line")
!      DIE_IF(molden_file.n_line_items/=1, "Expecting only one item on GTO line")
!      molden_file.read_line
!
!      ! Read basis
!      save  = stdin
!      stdin = molden_file
!      do a = 1,.n_atom
!         .basis(a).read_molden
!         .basis(a).put
!         .basis(a).set_label(.atom(a).basis_label)
!      end
!      stdin = save
!
!      ! Find [MO] tag
!      molden_file.look_for(op//"MO"//cl,found=found)
!      DIE_IF(NOT found, "No MO line")
!      DIE_IF(molden_file.n_line_items/=1, "Expecting only one item on MO line")
!      line = molden_file.line_number + 1
!
!      ! Any Beta tag?
!      molden_file.look_for("Beta",found=is_unrestricted)
!
!      ! Move back to [MO]'s
!      molden_file.move_to_line(line)
!
!      ! Get no. of electrons
!      n_e = ZERO
!      do
!         molden_file.look_for("Occup=",found=found)
!         if (NOT found) exit
!         molden_file.move_to_line_item(2)
!         molden_file.read(val)
!         molden_file.read_line
!         n_e = n_e + val
!      end
!
!      ! Now set charge
!      .charge = .atom.no_of_electrons - nint(n_e)
!
!      ! Set multiplicity
!      .spin_multiplicity = .default_spin_multiplicity
!
!      ! Finalize atom info/basis sets
!      .update_atom_basis_group_info
!
!      ! Create MO arrays (leaky)
!      .density_mx.destroy
!      .NO_occ_nos.destroy
!      .NOs.destroy
!
!      if (is_unrestricted) then
!
!         .NO_occ_nos.create(.n_bf,"u ")
!         .NO_occ_nos.a = ZERO
!         .NO_occ_nos.b = ZERO
!
!         .density_mx.create(.n_bf,"u ")
!         .NOs.create(.n_bf,"u ")
!         .NOs.a = ZERO
!         .NOs.b = ZERO
!
!      else
!
!         .NO_occ_nos.create(.n_bf,"r ")
!         .NO_occ_nos.r = ZERO
!
!         .density_mx.create(.n_bf,"u ")
!         .NOs.create(.n_bf,"r ")
!         .NOs.r = ZERO
!
!      end
!
!      ! Move back to [MO]'s
!      molden_file.move_to_line(line)
!      orb.create(.n_bf)
!
!      ! Read in orbitals and energies.
!      i = 0
!      do
!
!         molden_file.look_for("Ene=",found=found)
!
!         if (NOT found) exit
!
!         ! Read energy
!         molden_file.skip_next_item
!         molden_file.read(energy)
!
!         ! Read spin
!         molden_file.skip_next_item
!         molden_file.read(spin)
!
!         ! Read occupation
!         molden_file.skip_next_item
!         molden_file.read(occ)
!
!         ! Read orbital
!         do f = 1,.n_bf
!            molden_file.skip_next_item
!            molden_file.read(orb(f))
!         end
!
!         ! Increment orbital
!         if (spin=="Alpha") i = i + 1
!         DIE_IF(i>.n_bf,"too many orbitals specified")
!
!         ! Assign MO's
!         if (is_unrestricted) then
!
!            select case (spin)
!
!            case ("Alpha")
!               .NO_occ_nos.a(i) = occ
!               .NOs.a(:,i) = orb
!
!            case ("Beta")
!               .NO_occ_nos.b(i) = occ
!               .NOs.b(:,i) = orb
!
!            case default
!               DIE("unknown spin: "//trim(spin))
!
!            end
!
!         else
!
!            select case (spin)
!
!            case ("Alpha")
!               .NO_occ_nos.r(i) = occ
!               .NOs.r(:,i) = orb
!
!            case default
!               DIE("unexpected spin: "//trim(spin))
!
!            end
!
!         end
!
!      end
!
!      ! Clean
!      orb.destroy
!
!      ! Swap orbital order
!      fbf = .first_basis_fn_for_shell
!      lbf =  .last_basis_fn_for_shell
!      ang = .angular_moment_for_shell
!
!      if (is_unrestricted) then
!         GAUSSIAN_DATA:swap_from_Molden_C_order(.NOs.a,"1",fbf,lbf,ang)
!         GAUSSIAN_DATA:swap_from_Molden_C_order(.NOs.b,"1",fbf,lbf,ang)
!      else
!         GAUSSIAN_DATA:swap_from_Molden_C_order(.NOs.r,"1",fbf,lbf,ang)
!      end
!
!
!      ! Make density matrix
!    ! if (is_unrestricted) then
!    !    .make_density_mx_from(.NOs,.NO_occ_nos,"u ")
!    ! else
!    !    .make_density_mx_from(.NOs,.NO_occ_nos,"r ")
!    ! end
!
!      ! Print out data
!    ! stdout.flush
!    ! stdout.text("===========")
!    ! stdout.text("Molden data")
!    ! stdout.text("===========")
!    ! stdout.flush
!    ! stdout.text("Below are the data read from the molden file:")
!    ! stdout.text(". Atom coordinates and bases")
!    ! stdout.text(". Natural orbitals and their occupancies")
!    ! stdout.flush
!    ! if (is_unrestricted) then; stdout.text("The data is for *restricted* orbitals")
!    ! else;                      stdout.text("The data is for *unrestricted* orbitals")
!    ! end
!
!    ! stdout.flush
!    ! stdout.text("Molden atom coordinates and bases:")
!    ! .put_atoms
!
!    ! stdout.flush
!    ! stdout.text("Occupation numbers:")
!    ! stdout.flush
!    ! stdout.put(.NO_occ_nos)
!
!    ! stdout.flush
!    ! stdout.text("Natural orbitals:")
!    ! stdout.flush
!    ! stdout.put(.NOs)
!
!    ! ! Save data in archive files
!    ! archive.set(.name,"density_mx")
!    ! archive.write(.density_mx)
!
!      archive.set(.name,"NOs")
!      archive.write(.NOs)
!
!      archive.set(.name,"NO_occ_nos")
!      archive.write(.NO_occ_nos)
!
!      ! Set crystal data
!      if (.crystal.allocated) then
!         .resolve_axis_system
!         .crystal.update_fragment_info(.atom)
!      end
!
!      ! Clean
!      molden_file.destroy
!
!   end

!   swap_molden_orbital_order(X,swap) :: private, PURE
!   ! Swap the order of orbitals on matrix "X" after reading a
!   ! molden input file, for "swap" equal to "row" or "1", or
!   ! "coloumn" or "2".  This is nearly the same as gaussian.
!   !
!   ! The molden orderring is as follows:
!   !  5D: D 0, D+1, D-1, D+2, D-2
!   !  6D: xx, yy, zz, xy, xz, yz
!   !  7F: F 0, F+1, F-1, F+2, F-2, F+3, F-3
!   ! 10F: xxx, yyy, zzz, xyy, xxy, xxz, xzz, yzz, yyz, xyz
!   !  9G: G 0, G+1, G-1, G+2, G-2, G+3, G-3, G+4, G-4
!   ! 15G: xxxx yyyy zzzz xxxy xxxz yyyx yyyz zzzx zzzy,
!   !      xxyy xxzz yyzz xxyz yyxz zzxy
!      self :: IN
!      X :: MAT{REAL}, INOUT
!      swap :: STR, IN
!
!      s,f,l,ls,fg,lg,i,j :: INT
!      fac :: VEC{REAL}@
!      ff  :: VEC{INT}(10)
!      sph_reorder :: VEC{INT}@
!
!      ff = [1, 2, 3, 5, 6, 4, 9, 7, 8, 10]
!
!      select case (swap)
!
!      case("row","1")
!         do s = 1,.n_shell
!            f  = .first_basis_fn_for_shell(s)
!            l  = .last_basis_fn_for_shell(s)
!            ls = .angular_moment_for_shell(s)
!            if (.atom(.atom_for_shell(s)).basis.is_spherical) then
!               ! do spherical test
!               sph_reorder = GAUSSIAN_DATA:tonto_to_gaussian_spherical_indices(ls)
!               if (ls>=1) then ! swap f functions
!                  X(f:l,:) = X(f-1+sph_reorder,:)
!               end
!               if (ls>1) then  ! normalisation factors
!                  lg  = GAUSSIAN_DATA:n_comp_up_to(ls-1)
!                  fg  = lg + 1
!                  lg  = lg + GAUSSIAN_DATA:n_comp(ls)
!                  fac = GAUSSIAN_DATA::unnormalizing_factors(fg:lg)
!                  do i = f,l
!                     X(i,:) = X(i,:)*fac(i-f+1)
!                  end
!               end
!            else
!               ENSURE(ls<=4,"cannot yet convert order for h shells")
!               ! fix for nwchem molden MOs, PGTO normalization for xy, xy, yz
!               if (ls == 2) then
!                  X(f+3:l,:) = X(f+3:l,:) / sqrt(3d0)
!               end
!               if (ls==3) then ! swap f functions
!                  X(f:l,:) = X(f-1+ff,:)
!                  ! if the same trend from D is true for F above these may be needed
!                  ! X(f+3:l-1,:) = X(f+3:l-1,:) / sqrt(3d0)
!                  ! X(l-1:l,:) = X(l-1:l,:) / sqrt(15d0)
!               end
!               if (ls>1) then  ! normalisation factors
!                  lg  = GAUSSIAN_DATA:n_comp_up_to(ls-1)
!                  fg  = lg + 1
!                  lg  = lg + GAUSSIAN_DATA:n_comp(ls)
!                  fac = GAUSSIAN_DATA::unnormalizing_factors(fg:lg)
!                  do i = f,l
!                     X(i,:) = X(i,:)*fac(i-f+1)
!                  end
!               end
!            end
!         end
!
!      case("column","2")
!         do s = 1,.n_shell
!            f  = .first_basis_fn_for_shell(s)
!            l  = .last_basis_fn_for_shell(s)
!            ls = .angular_moment_for_shell(s)
!            ENSURE(ls<=4,"cannot yet convert order for h shells")
!            if (ls==3) then ! swap f functions
!               X(:,f:l) = X(:,f-1+ff)
!            end
!            if (ls>1) then  ! normalisation factors
!               lg  = GAUSSIAN_DATA:n_comp_up_to(ls-1)
!               fg  = lg + 1
!               lg  = lg + GAUSSIAN_DATA:n_comp(ls)
!               fac = GAUSSIAN_DATA::unnormalizing_factors(fg:lg)
!               do j = f,l
!                  X(:,j) = X(:,j)*fac(j-f+1)
!               end
!            end
!         end
!
!    ! case default
!    !    DIE("unknown swap kind, "//trim(swap))
!
!      end
!
!   end

!   swap_from_molden_order(X,swap) :: private, PURE
!   ! Swap the order of orbitals on matrix "X" after reading a
!   ! molden input file, for "swap" equal to "row" or "1", or
!   ! "coloumn" or "2". The molden orderring is as follows:
!   !  5D: D 0, D+1, D-1, D+2, D-2
!   !  6D: xx, yy, zz, xy, xz, yz
!   !  7F: F 0, F+1, F-1, F+2, F-2, F+3, F-3
!   ! 10F: xxx, yyy, zzz, xyy, xxy, xxz, xzz, yzz, yyz, xyz
!   !  9G: G 0, G+1, G-1, G+2, G-2, G+3, G-3, G+4, G-4
!   ! 15G: xxxx yyyy zzzz xxxy xxxz yyyx yyyz zzzx zzzy,
!   !      xxyy xxzz yyzz xxyz yyxz zzxy
!      self :: IN
!      X :: MAT{REAL}, INOUT
!      swap :: STR, IN
!
!      s,i,f,l,ls,fg,lg :: INT
!      fac :: VEC{REAL}@
!      reorder :: VEC{INT}@
!
!      ff = [1, 2, 3, 5, 6, 4, 9, 7, 8, 10]
!
!      select case (swap)
!
!      case("row","1")
!
!         do s = 1,.n_shell
!
!            f  = .first_basis_fn_for_shell(s)
!            l  = .last_basis_fn_for_shell(s)
!            ls = .angular_moment_for_shell(s)
!
!            if (.atom(.atom_for_shell(s)).basis.is_spherical) then
!               reorder = GAUSSIAN_DATA::molden_spherical_order(ls)
!               X(f:l,:) = X(f-1+reorder,:)
!               if (ls>1) then  ! normalisation factors
!                  lg  = GAUSSIAN_DATA:n_comp_up_to(ls-1)
!                  fg  = lg + 1
!                  lg  = lg + GAUSSIAN_DATA:n_comp(ls)
!                  fac = GAUSSIAN_DATA::unnormalizing_factors(fg:lg)
!                  do i = f,l
!                     X(i,:) = X(i,:)*fac(i-f+1)
!                  end
!               end
!
!            else
!
!               ! Cartesian
!               ENSURE(ls<=4,"cannot yet convert order for h shells")
!               ! fix for nwchem molden MOs, PGTO normalization for xy, xy, yz
!               if (ls == 2) then
!                  X(f+3:l,:) = X(f+3:l,:) / sqrt(3d0)
!               end
!               if (ls==3) then ! swap f functions
!                  X(f:l,:) = X(f-1+ff,:)
!                  ! if the same trend from D is true for F above these may be needed
!                  ! X(f+3:l-1,:) = X(f+3:l-1,:) / sqrt(3d0)
!                  ! X(l-1:l,:) = X(l-1:l,:) / sqrt(15d0)
!               end
!               if (ls>1) then  ! normalisation factors
!                  lg  = GAUSSIAN_DATA:n_comp_up_to(ls-1)
!                  fg  = lg + 1
!                  lg  = lg + GAUSSIAN_DATA:n_comp(ls)
!                  fac = GAUSSIAN_DATA::unnormalizing_factors(fg:lg)
!                  do i = f,l
!                     X(i,:) = X(i,:)*fac(i-f+1)
!                  end
!               end
!            end
!         end
!
!      case("column","2")
!
!         do s = 1,.n_shell
!
!            f  = .first_basis_fn_for_shell(s)
!            l  = .last_basis_fn_for_shell(s)
!            ls = .angular_moment_for_shell(s)
!
!            ENSURE(ls<=4,"cannot yet convert order for h shells")
!            if (ls==3) then ! swap f functions
!               X(:,f:l) = X(:,f-1+ff)
!            end
!            if (ls>1) then  ! normalisation factors
!               lg  = GAUSSIAN_DATA:n_comp_up_to(ls-1)
!               fg  = lg + 1
!               lg  = lg + GAUSSIAN_DATA:n_comp(ls)
!               fac = GAUSSIAN_DATA::unnormalizing_factors(fg:lg)
!               do j = f,l
!                  X(:,j) = X(:,j)*fac(j-f+1)
!               end
!            end
!
!         end
!
!    ! case default
!    !    DIE("unknown swap kind, "//trim(swap))
!
!      end
!
!   end

!  ======================
!  Crystal XML etc. files
!  ======================

   subroutine process_CIF_AND_C23_XML(self)
   ! Process both a CIF and an XML output from C23 to
   ! read the density matrix, and unit cell.
   ! NOTE: the CIF should have been processed beforehand!
      type(MOLECULE_TYPE), INOUT :: self
   ! ENSURE(.atom.allocated,"Oi! Start with input CIF")
   ! ENSURE(.atom.dim==.crystal.n_unit_cell_atoms,"Oi!")
      STR(len=STR_SIZE) :: base
      MAT(REAL,3,3) :: xuc,cuc,dm,cuc_prm
      MAT(REAL,3,3) :: W
      MAT(REAL,3,3) :: W_inv
      VEC(type(ATOM_TYPE),:), allocatable :: xml_atom,cif_atom
      REAL :: vol_ratio_m1
      INT :: a,f,s,i,j
      BIN :: same
      VEC(REAL,3) :: cif_pos,xml_pos,tmp_vec
      MAT(REAL,3,3) :: adp2,cif_adp2,tmp_adp2  ! Kang
      MAT3(REAL,:,:,:), allocatable :: seitz  ! Kang for ADP transform
   ENSURE(allocated(self%crystal23),"MOLECULE.READ:process_CIF_AND_C23_XML ... There is no C23 xml file name!")
   ENSURE(self%crystal23%xml_file_name/=" ","MOLECULE.READ:process_CIF_AND_C23_XML ... There is no C23 xml file name!")

! Kang's note. 2024 Jun 19
! " cif_atom " contains the information of cif atoms
!             - It has ADP data.             
! 
! " xml_atom " will update "(molecule).atom" 
!             - All atoms in the PRIMITIVE unit cell defined in the xml file.
!             - W (X_to_P) matrix provide the conversion between the primitive cell and 
!               the crystallographic cell. ( See Crystal23 manual chapter A.5 )           
!             - "(molecule).atom" is used for ED calculation (.make_CRYSTAL23_ED_grid_v1 )

      ! Periodic HAR has a reported, unconfirmed defect: say so first.
      call flush_(stdout)
      call text_(stdout,"============================================================")
      call text_(stdout,"WARNING: periodic HAR may have errors - use at your own risk")
      call text_(stdout,"============================================================")
      call flush_(stdout)

   !***** << Stage 1/4 : Cell comparison: Crystallographic cell vs Primitive cell, are they agree? >>
      if (NOT allocated(self%crystal)) call create_(self%crystal)

      ! Read cif and update .crystal object
      call open_(self%cif)
      call find_data_block_(self%cif)
      call read_smCIF_(self%crystal%unit_cell,self%cif)
      call read_smCIF_(self%crystal%spacegroup,self%cif)  ! ".crystal.spacegroup" & ".crystal.unit_cell" are updated
      call read_smCIF_(cif_atom,self%cif)  ! "cif_atom" is updated. i
      call close_(self%cif)
      ! The (temporary) Cartesian lattice vectors created from the cif file.
      !   "cuc" and ".crystal.unit_cell" will be updated after the comparison of METRIC TENSORs (dm)
      !   based on the lattice vectors provided by the XML file.
      !   It is important to keep our crystal's orientation be consistent with 
      !   the cartesian coordinate of XML file, but at this stage, "cuc" may not be consistent
      cuc = self%crystal%unit_cell%direct_mx

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Read xml and update .CRYSTAL23 object !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Get C23 XML file name
      base = self%crystal23%xml_file_name
      call read_CRYSTAL23_xml_(self,base,xml_atom)  ! This read in xml atom's "Cartesian coordinate"
                                                ! and ".crystal23.Primitive_unit_cell" is set
      ! "xuc" is Cartesian Lattice vectors (or the primitive cell) from the xml file.       
      xuc = self%crystal23%Primitive_unit_cell%direct_mx

      ! Kang 2024.10.23 comment
      !     If lamaGOET makes Crystal23 to keep the conventional cell
      !   using "KEEPSYMM" and "SUPERCON" \n 1 0 0 \n 0 1 0 \n 0 0 1
      !   then .XML contains a conventional information.

      ! Lattice consistency checking 1): "cell volume ratio" minus ONE = should be ZERO
      vol_ratio_m1 = (determinant_(xuc))/(determinant_(cuc))-ONE
      if (is_zero_(vol_ratio_m1,TOL(4))) then

         ! volume test pass. Orientation checking is coming
         do i = 1,3
            do j = 1,3
               dm(i,j) = dot_product(xuc(:,i),xuc(:,j))-dot_product(cuc(:,i),cuc(:,j))
            end do
         end do
         DIE_IF(NOT is_ZERO_(dm,TOL(3)),"MOLECULE.READ:process_CIF_AND_C23_XML ... Inconsistent lattice vector Ordering between the xml cell and the cif cell")
         cuc = xuc  ! All test passed. overwrite cif cell lattice vectors from the xml cell
         W = ZERO  ! W and W_inv are initialized to be Identity for later use (Stage 2/4)
         do i = 1,3
            W(i,i) = ONE  ! Kang's memo: Is there a better way to set W and W_inv as 3x3 Identity matrices?
         end do
         W_inv = W  ! W_inv is also identity matrix
      else

         !--- If you enter here, there is high chance of xml cell is a primitive cell and 
         !    cif cell is a conventional cell. Or really working on inconsistent cells
         !
         ! Conversion matrix and its inverse between crystallogrphic and primitive cells
         ! provided by Crystal23  (See Crystal23 manual)
         call WC23_to_(self%crystal%spacegroup%lattice_symbol,W)
         call to_inverse_of_(W_inv,W)
         W_inv = nint(W_inv)  ! W^-1 always has integer elements only
         if (self%crystal%spacegroup%lattice_symbol/="P") then
            call text_(stdout,"  Info: Centering detected.")
            call text_(stdout,"  |  Conversion matrix W (Crystallographic to Primitive) matrix")
            call put_(stdout,W)
            call text_(stdout,"  |")
            call text_(stdout,"  |  Inverse conversion matrix")
            call put_(stdout,W_inv)
            call text_(stdout,"  |- Refer to Crystal23 manual chapter A5 for more details ")
            call flush_(stdout)
         else  ! Kang comment 24.Oct 2024 It is unlikely to enter here. Shall I remove?
            call text_(stdout," Info: The XML unit cell and the cif unit cell are the same. ")
            call flush_(stdout)
         end if

         ! Primitive lattice vectors of the (temporary) unit cell created from .cif
         cuc_prm = matmul(cuc,W)

         ! Lattice consistency checking 1*) (primitive) cell volume comparison. If both cells are consistent, then "volume_ratio_minus_ONE" is ZERO
         vol_ratio_m1 = (determinant_(xuc))/(determinant_(cuc_prm))-ONE
         DIE_IF(NOT is_zero_(vol_ratio_m1,TOL(4)),"MOLECULE.READ:process_CIF_AND_C23_XML ... Inconsistent cell volumes between the .XML and .cif cells")

         ! Lattice consistency checking 2) dm = Difference in two metric tensors of primitive cells
         do i = 1,3
            do j = 1,3
               dm(i,j) = dot_product(xuc(:,i),xuc(:,j))-dot_product(cuc_prm(:,i),cuc_prm(:,j))
            end do
         end do
         DIE_IF(NOT is_ZERO_(dm,TOL(3)),"MOLECULE.READ:process_CIF_AND_C23_XML ... Inconsistent lattice vector Ordering between the primitive xml cell and the cif cell")
         cuc = matmul(xuc,W_inv)  ! All test passed. make cif cell lattice vectors from the primitive XML cell
      end if

   !****** << Stage 2/4 : Create a unit cell that is consistent with the XML unit cell >
   !        <             Find the atom pair list of  cif atoms and the XML atoms      >>

      ! Update crsytallographic cell lattice vectors from the Crystal23 .xml primitive cell
      ! k-points of reflection data is constructed based on this.
      call make_direct_mx_from_(self%crystal%unit_cell,cuc(:,1),cuc(:,2),cuc(:,3))  ! Updated "cuc" is re-oriented based on "xuc"
      !! Kang's note
      ! At this stage, cif_atom(a).pADP_vector has 
      ! - positions in fractional coordinates 
      ! -      ADPs in crystal coordinate in bohr^2 unit 
      ! Don't invoke .update_CIF_crystal_atoms(.cif) here
      ! This make positions in cartesian coord. Keep them in fractional coordinate for now
      ! > Kang's 2nd note
      ! >   The metric tensor comparison is made for robustness, but maybe someone can simplify
      ! >   the above part by starting from reading in "xuc" and create corresponding "cuc"
      call create_(self%crystal23%xml_atom_for_cif_atom,size(cif_atom))
      self%crystal23%xml_atom_for_cif_atom = 0
      call create_(self%crystal23%cif_atom_for_xml_atom,size(xml_atom))
      self%crystal23%cif_atom_for_xml_atom = 0

      !!
      ! Search the same atom pairs from
      !   1) cif cell (crysallographic)   
      !   2) xml cell (primitive)          
      !  
      ! Each atom's fractional coordinate satisfies the following relation
      !   
      !   W * x_primitive = x_crystallographic  <==>  x_prm   = W^-1 * x_cry
      !        (xml cell)       (cif cell)          (xml cell)       (cif cell)
      !   
      do a = 1,size(cif_atom)  ! loop over cif atoms

         !
         cif_pos = cif_atom(a)%position  !      cif_pos in the Crystallographic crystal coordinate
         cif_pos = matmul(W_inv,cif_pos)  ! Now, cif_pos in the    PRIMITIVE     crystal coordinate
         !
         do f = 1,size(xml_atom)  ! loop over xml_atom (future fragment_atom)

            ! 
            xml_pos = xml_atom(f)%position  ! xml_pos is in Cartesian coordinate
            xml_pos = matmul(self%crystal23%Primitive_unit_cell%inverse_mx,xml_pos)  ! xml_pos in fractional coordinate of the Primitive cell (from the Crystal23 XML file)
            !
            tmp_vec = cif_pos-xml_pos  ! tmp_vec is difference in PRIMITIVE crystal coordinate
            tmp_vec = MOD(tmp_vec+HALF+ONE,ONE)-HALF  ! Stable "modulo 1" function whose range is [-0.5, 0.5)
            tmp_vec = matmul(xuc,tmp_vec)  ! Now, tmp_vec in Cartensian coordinate ([Bohr] unit)
            !
            same = is_zero_(tmp_vec,0.1d0)  ! same atom criteria: distance < 0.1 Bohr
            !
            if (same) then

               !  Assign information arrays       
               self%crystal23%xml_atom_for_cif_atom(a) = f
               self%crystal23%cif_atom_for_xml_atom(f) = a

               ! - Overwrite the fractional coordinate of cif atom (.atom) by using xml_atom.
               ! - By this way, we can use more precise numbers written in the XML file,
               !   and prevent precision-originated coordinate discrepancy between cif atoms and xml atom.
               ! - It may happen that, the modulo 1 operation in the PRIMITIVE fractional coordinate results in 
               !   different but symmetrically equivalent number in the Crystallographic fractional coordinate.
               ! - To prevent the confusion, we need some additional steps of tracking boundary crossing of modulo 1 operation
               tmp_vec = matmul(W_inv,cif_atom(a)%position)-xml_pos  ! Abusing variable "tmp_vec" to track boundary crossing
               tmp_vec = nint(tmp_vec)
               !
               cif_pos = matmul(W,xml_pos+tmp_vec)  ! this "cif_pos" is more consistent with the xml file
               !
               call set_position_(cif_atom(a),cif_pos)
               exit
            end if  ! end if (same) then
         end do  ! end do f=1,xml_atom.dim
         !
         DIE_IF(NOT same,"MOLECULE.READ:process_CIF_AND_C23_XML ... A cif atom is not found among the XML atoms ")
      end do  ! end do a=1,cif_atom.dim

   ! **** << Stage 3/4 : Symmetrizing ADPs of cif atoms using the stabilizers of each unique atom >
   !       <              which are obtained by a FALSE update of ".fragment_atom"                >>
      do f = 1,size(xml_atom)  ! Change each xml_atom coordinate system (Primitive cell)
         call change_axis_system_to_(xml_atom(f),"cartesian",self%crystal23%Primitive_unit_cell)
      end do

! stdout.text("(molecule)-crystal23-xml_atom_for_cif_atom")
! stdout.put( .crystal23.xml_atom_for_cif_atom )
! stdout.put( .crystal23.cif_atom_for_xml_atom )

   !! Preliminary RUN updata_fragment_info(xml_atom) 
   !!    > This step is needed to find the stabilizers or relavant symmetry operators of each atom
   !!    > Using those information, we will update the ADPs of "xml_atom" 
   !! 
      call update_fragment_info_(self%crystal,xml_atom)  ! ".atom" has ADPs but not "xml_atom" at this stage
                                             ! .crystal.fragment_atom order is the same to xml_atom
   !! First, symmetrize the ADPs of each cif atom (=".atom")
      ! Symmetrize ADP tensor : First written Kang 16.May. 2024 
      !                         Updated            24.Jun. 2024 
      seitz = xyz_seitz_matrices_(self%crystal)  ! cartesian sym ops- Kang
      do a = 1,size(cif_atom)  ! cif_atom(a) (a-th cif atom) 
         call change_axis_system_to_(cif_atom(a),"cartesian",self%crystal%unit_cell)
         cif_adp2 = cif_atom(a)%ADP_tensor
         tmp_adp2 = ZERO
         f = self%crystal23%xml_atom_for_cif_atom(a)  ! f is xml_atom index corresponding to cif_atom(i)
         do i = 1,self%crystal%frag_atom_sym_factor(f)
            s = self%crystal%frag_atom_stabilizer(f)%element(i)  ! Transform by s-th seitz matrix 
            adp2 = matmul(seitz(:,:,s),cif_adp2)
            adp2 = matmul(adp2,transpose(seitz(:,:,s)))
               !   new cartesian ADP = Rotation_mx * old cartesian ADP * tanspose( Rot._mx )
            tmp_adp2 = tmp_adp2+adp2
         end do  !  do a=1,#stabilizer
         adp2 = tmp_adp2/self%crystal%frag_atom_sym_factor(f)
         call set_ADP_tensor_(cif_atom(a),adp2)  ! i-th cif atom's ADP is symmetrized
      end do  ! cif atom ADP symmetrization completed. now move on to the xml_atom

   !<< Stage 4/4 : True update of ".atom" & ".fragment_atom" with symmetrized ADPs  >>

      ! Now we are going to update ADPs "xml_atom" based on cif atom stored in ".atom"
      do f = 1,size(xml_atom)  ! Index example for NH3 f = 14 Nitrogen atom

         ! f-th xml_atom(=frag._atom)-> find its unique_atom (Let say the i-th frag.atom) 
         !                                   its relavant sym op (Let say the s-th symop)
         ! for that unique_atom      -> find cif_atom (=.atom)             (mostly they are the same) 
         i = self%crystal%unique_atom_for_frag_atom(f)  ! i = 13
         s = self%crystal%asym_symop_for_frag_atom(f)  ! s = 2
         a = self%crystal23%cif_atom_for_xml_atom(i)  ! find the cif atom "a" for a unique atom "i" 
                                                ! a = cry23.cafxa(i=13) = 2
         DIE_IF(a==0,"MOLECULE.READ:process_CIF_AND_C23_XML ... Fail to find the cif atom for a unique atom")
         ! Kang's note. 
         !  In fact this is not an actual problem for the most cases,
         !  If there is exceptional cases that the unique atoms cannot found 
         !  in the cif atom list, overwriting ADP to the xml_atom will be a bit more tricky but doable
         !  because it would require a step for transforming cif atom's ADP into unique atom's ADP
         !  Then rest of the other xml_atom(or .fragment_atom)'s ADPs are updated using the unique atom's ADP
         adp2 = cif_atom(a)%ADP_tensor
         adp2 = matmul(seitz(:,:,s),adp2)
         adp2 = matmul(adp2,transpose(seitz(:,:,s)))  ! ADP transformed
         call set_ADP_tensor_(xml_atom(f),adp2)  ! adp2 is cartesian ADP tensor with Bohr^2 unit        
      end do
      ! xml_atom is ready for updating .fragment_atom

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   !! TRUE Update .atom & .fragment_atom with symmetrized ADPs for all atoms !
   !!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      self%n_atom = size(xml_atom)
      call create_(self%atom,self%n_atom)  !Kang's note : (molecule).atom(:) will be used for the shells(basis) and Electron Density calculation
      do f = 1,self%n_atom
         call set_axis_system_to_(self%atom(f),"cartesian")
         call set_label_(self%atom(f),xml_atom(f)%label)  ! what about tags?
         call set_position_(self%atom(f),xml_atom(f)%position)
         call set_ADP_tensor_(self%atom(f),xml_atom(f)%ADP_tensor)
         call set_group_(self%atom(f),0)  ! No group info. Reset to 0
      end do
      ! Destroy existing basis, and atom list
      if (allocated(self%basis)) call destroy_(self%basis)

      ! Make sure basis set is spherical etc.
      self%use_spherical_basis = TRUE
      self%use_CRYSTAL23_order = TRUE

      ! Finalize atom info and esnsure it has basis set
      ! (Assuming basis set was set before this)
      call update_atom_basis_group_info_(self)  ! It uses .atom.group updated  
                                          ! Kang. Caution: (molecule).atom.group may not be correct when a molecule crosses the unit cell boundary
      call destroy_(self%crystal%asymmetric_unit_atom)
      call destroy_(self%crystal%asymmetric_unit_geometry)
      call update_fragment_info_(self%crystal,self%atom)  ! ".atom" = "xml_atom" has ADP data now
                                          ! .crystal.fragment_atom is now .atom (=xml_atom)

      !!!! Put Crystal !!!!!!!!!!
      !!  .crystal.put         !!
      call put_(self%crystal%unit_cell)  !!
      call put_(self%crystal%spacegroup)  !!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! Show xml&cif-integrated .atom(f). They have xml's coordinates and (symmetry transformed) cif's ADPs.
      call text_(stdout," =======================================")
      call text_(stdout," Integrated CIF and Crystal23's XML data")  ! Note: Cry23 xml atoms belong to (-1/2,1/2)cell
      call text_(stdout," =======================================")
! Kang debug: Following 3 lines will be reactivated when the periodic fragment is fully available
!      do f = 1,.crystal.n_fragment_atoms          ! Delete fragment_atom.group
!         .crystal.fragment_atom(f).set_group( 0 ) !  Group indices may be incorrect, when a molecule crosses the unit cell boundary)
!      end                                         ! So erase ".crystal.fragment_atom.group". Will be updated after defragment
      call put_coordinates_(self%crystal%fragment_atom,no_header=TRUE)  ! Put ".fragment_atom" (w/o group info)
      call put_ADPs_(self%crystal%fragment_atom,no_header=TRUE)
      self%crystal%finalize = TRUE
      call destroy_(xml_atom)
      call destroy_(cif_atom)
   end subroutine

   subroutine read_CRYSTAL23_xml(self,base,xml_atom)
   ! Read the xml file and update .crystal23 object
   ! Kang 2024.Jun.11
   ! Testing
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: base
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: xml_atom
      type(TEXTFILE_TYPE), allocatable :: XMLfile
      STR(len=STR_SIZE) :: tmp
      MAT(STR(len=STR_SIZE),:,:), allocatable :: replace
      MAT(REAL,:,:), allocatable :: W
      VEC(REAL,3) :: av,bv,cv
      VEC(INT,3) :: igv
      VEC(REAL,3) :: pos
      INT :: n_atoms,n_bf,n_g
      INT :: a,g
   ENSURE(allocated(self%crystal23),"MOLECULE.READ:read_CRYSTAL23_xml ... crystal23 object is not allocated!")
      if (allocated(self%crystal%fragment_atom)) call destroy_(self%crystal%fragment_atom)

      ! Open XML file
      call create_(XMLfile,base)
      call create_(replace,3,2)
      replace(:,1) = ["<",">","="]
      replace(:,2) = ['" "','" "','" "']
      call set_replacement_list_(XMLfile,replace)
      call open_for_(XMLfile,"read")
      call move_to_line_(XMLfile,1)

      ! Read in unit cell
      ! IMPORTANT - could be different to standard one!
      call look_for_(XMLfile,"CELL_VECTOR_A")
      call read_line_(XMLfile)
      call move_to_line_item_(XMLfile,1)
      call read_(XMLfile,av)
      call look_for_(XMLfile,"CELL_VECTOR_B")
      call read_line_(XMLfile)
      call move_to_line_item_(XMLfile,1)
      call read_(XMLfile,bv)
      call look_for_(XMLfile,"CELL_VECTOR_C")
      call read_line_(XMLfile)
      call move_to_line_item_(XMLfile,1)
      call read_(XMLfile,cv)
      call make_direct_mx_from_(self%crystal23%Primitive_unit_cell,av,bv,cv)

      ! Read in atoms from XML
      call look_for_(XMLfile,"NUMBER_OF_ATOMS")
      call move_to_line_item_(XMLfile,2)
      call read_(XMLfile,n_atoms)
      ! update number of xml atoms (= number of atoms in a primitive cell)
      !  Store the xml atom list in xml_atom. xml_atom will update ".atom" object.
      !  (.atom are used for the electron density(ED) calculation in .make_CRYSTAL23_ED_grid_v1)

      ! Read atomic numbers & set
      ! These are a different order to how LamaGOET had them
      call create_(xml_atom,n_atoms)

!     XMLfile.look_for("FRACTIONARY_COORDINATES") ! Find the fractional coordinate block in the XML file
!     xml_atom.set_axis_system_to("crystal")
      call look_for_(XMLfile,"CARTESIAN_COORDINATES")
      call set_axis_system_to_(xml_atom,"cartesian")
      call look_for_(XMLfile,"ATOM.1")
      do a = 1,n_atoms
         call move_to_line_item_(XMLfile,3)
         call read_(XMLfile,tmp)
         call set_label_(xml_atom(a),tmp)
         call read_line_(XMLfile)
         call read_(XMLfile,pos)
         call set_position_(xml_atom(a),pos)  ! pos is in fractional coordinate
         call read_line_(XMLfile)
         call read_line_(XMLfile)
      end do

      ! Read number of atomic orbitals (# of basis function PER primitive cell)
      call look_for_(XMLfile,"NUMBER_OF_ATOMIC_ORBITALS")
      call move_to_line_item_(XMLfile,2)
      call read_(XMLfile,n_bf)
      self%crystal23%n_bf = n_bf
      call flush_(stdout)
      call text_(stdout,"=============")
      call text_(stdout,"Crystal23 XML")
      call text_(stdout,"=============")
      call show_(stdout,"  Number of basis function per unit cell      n_bf=",n_bf)

      ! Read number of IVDL (Integer Vectors Direct Lattice) (List of the relavant neighboring cells)
      call look_for_(XMLfile,"INTEGER_VECTORS_INFO")
      call move_to_line_item_(XMLfile,3)
      call read_(XMLfile,n_g)
      call show_(stdout,"  Number of neighbor cells for ED calculation n_g =",n_g)
      self%crystal23%n_g = n_g

      ! Read the neighbor cell information
      call create_(self%crystal23%g_latt,3,n_g)
      call look_for_(XMLfile,"IVDL.1")
      do g = 1,n_g
         call move_to_line_item_(XMLfile,2)
         call read_(XMLfile,igv)
         self%crystal23%g_latt(:,g) = igv
         call read_line_(XMLfile)
      end do

      ! Read overlap matrices
   ! n_tri = n_bf*(n_bf+1)/2
   ! S.create(n_tri,n_g)
   ! do g = 1,n_g
   !    tmp = g.to_str
   !    XMLfile.look_for("DIRECT_OVERLAP_MATRIX__IVDL."//trim(tmp))
   !    XMLfile.read_line
   !    XMLfile.move_to_line_item(1)
   !    XMLfile.read(S(:,g)) ! <-- ignore new lines
   !  ! stdout.text("Stri"//trim(tmp)//":")
   !  ! stdout.put(S(1:4,g))
   ! end

   ! ! Copy into full S matrices
   ! .crystal23.overlap_mx.create(n_bf,n_bf,n_g)
   ! .crystal23.overlap_mx(:,:,1).symmetric_unzip_triangle(S(:,1))
   ! do g = 2,n_g,2
   !    .crystal23.overlap_mx(:,:,g  ).unzip_triangles(S(:,g+1),S(:,g  ))
   !    .crystal23.overlap_mx(:,:,g+1).unzip_triangles(S(:,g  ),S(:,g+1))
   !  ! stdout.text("Check S:")
   !  ! stdout.text("S"//trim(g.to_str))
   !  ! stdout.put(.crystal23.overlap_mx(:,:,g  ))
   !  ! g1 = g + 1
   !  ! stdout.text("S"//trim(g1.to_str))
   !  ! stdout.put(.crystal23.overlap_mx(:,:,g1 ))
   ! end

      ! Read full density matrices
      call create_(self%crystal23%density_mx,n_bf,n_bf,n_g)
      call create_(W,n_bf,n_bf)
      do g = 1,n_g
         tmp = to_str_(g)
         call look_for_(XMLfile,"DIRECT_DENSITY_MATRIX__IVDL."//trim(tmp))
         call read_line_(XMLfile)
         call move_to_line_item_(XMLfile,1)
         call read_(XMLfile,W)
         self%crystal23%density_mx(:,:,g) = W
      end do
      call text_(stdout,"  Reading Crystal23 Density matrix complete")
      call flush_(stdout)
   ! do g = 2,n_g,2
   !    stdout.text("Check:")
   !    stdout.text("P"//trim(g.to_str))
   !    stdout.put(.crystal23.density_mx(:,:,g  ))
   !    g1 = g + 1
   !    stdout.text("P"//trim(g1.to_str))
   !    stdout.put(.crystal23.density_mx(:,:,g1 ))
   ! end

      ! Check number of electrons
   ! val = ZERO
   ! do g = 1,n_g
   !    S = .crystal23.overlap_mx(:,:,g)
   !    W = .crystal23.density_mx(:,:,g)
   !    zmp = S.trace_product_with(W)
   !    val = val + zmp
   !    stdout.show("g   = ",g)
   !    stdout.show("tmp = ",zmp)
   ! end
   ! stdout.show("N_e = ",val)
      call close_(XMLfile)
      call destroy_(XMLfile)
   end subroutine


!  ======================
!  Gaussian chk interface
!  ======================

! Read *only* Tonto MO's and density from g09

   subroutine read_tonto_FChk_file(self,name)
   ! Read the MO's and density matrix from a g09 checkpoint file
   ! as if these were just placed there in TONTO format.
   ! WARNING: only works for restricted
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), optional, IN :: name
      type(TEXTFILE_TYPE), allocatable :: chkfile
      STR(len=STR_SIZE) :: fchk
   DIE_IF(NOT self%basis_info_made,"MOLECULE.READ:read_tonto_FChk_file ... must be a tonto basis set!")

      ! Get FChk file name
      if (present(name)) then
         fchk = name
      else if (NOT exhausted_(stdin%buffer)) then
         call read_(stdin,fchk)  ! specified on stdin
      else
         fchk = self%name
      end if

      ! Create FChk file
      call create_(chkfile,name)

   ! ! Create orbital energies.
   ! .MO_energies.destroy
   ! .MO_energies.create(.n_bf,"r ")

   ! ! Find & read orbital energies.
   ! chkfile.look_for("Alpha Orbital Energies",from=1)
   ! chkfile.read(.MO_energies)

      ! Create MOs
      call destroy_(self%MOs)
      call create_(self%MOs,self%n_bf,"r ")

      ! Find & read in orbitals
      call look_for_(chkfile,"Alpha MO",from=1)
      call read_(chkfile,self%MOs,by_column=TRUE)

      ! Create DM 
      call destroy_(self%density_mx)
      call create_(self%density_mx,self%n_bf,"r ")

      ! Find & read in DM
      call look_for_(chkfile,"SCF density",from=1)
      call read_(chkfile,self%density_mx,by_column=TRUE)
   end subroutine


! Can thesed be condensed into one?

   subroutine read_g09_FChk_file(self,name)
   ! Read a g09 checkpoint file (after fchk conversion to ASCII) into TONTO
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), optional, IN :: name
      STR(len=STR_SIZE) :: fchk,label
      type(TEXTFILE_TYPE), allocatable :: chkfile
      VEC(INT,:), allocatable :: fbf,lbf,ang
      VEC(INT,:), allocatable :: shell_l,n_prim_for_shell,atom_for_shell,atom_p
      VEC(INT,:), allocatable :: G_n_prim_for_shell,G_atom_for_shell,G_shell_type
      VEC(REAL,:), allocatable :: G_ex,G_cc,G_cp,ex,cc,orb_energy,fc
      VEC(REAL,3) :: pos
      MAT(REAL,:,:), allocatable :: temp_mat
      INT :: Z,a,s,t,f,l,g,h,n3n,ntr
      INT :: n_prim_shells,n_shell,n_prim,n_independent
      BIN :: restricted
      type(ARCHIVE_TYPE) :: archive
      REAL :: Nc
   DIE_IF(self%use_spherical_basis,"MOLECULE.READ:read_g09_FChk_file ... spherical basis not implemented")

      ! Destroy existing basis, and atom list
      if (allocated(self%basis)) call destroy_(self%basis)
      if (allocated(self%atom)) call destroy_(self%atom)

      ! Create SCF_data
      call destroy_(self%SCF_data)
      call create_(self%SCF_data)
      call set_output_(self%SCF_data,FALSE)

      ! Get FChk file name
      if (present(name)) then
         fchk = name
      else if (NOT exhausted_(stdin%buffer)) then
         call read_(stdin,fchk)  ! specified on stdin
      else
         fchk = self%name
      end if

      ! Open FChk file
      call create_(chkfile,fchk)
      call open_for_(chkfile,"read")
      call move_to_line_(chkfile,1)

      ! Read in basic stuff.
      call look_for_(chkfile,"Number of atoms",from=1)
      call move_to_line_item_(chkfile,5); call read_(chkfile,self%n_atom)
      call look_for_(chkfile,"Charge",from=1)
      call move_to_line_item_(chkfile,3); call read_(chkfile,self%charge)
      call look_for_(chkfile,"Multiplicity",from=1)
      call move_to_line_item_(chkfile,3); call read_(chkfile,self%spin_multiplicity)
      call look_for_(chkfile,"Number of electrons",from=1)
      call move_to_line_item_(chkfile,5); call read_(chkfile,self%n_e)
      call look_for_(chkfile,"Number of alpha electrons",from=1)
      call move_to_line_item_(chkfile,6); call read_(chkfile,self%n_a)
      call look_for_(chkfile,"Number of beta electrons",from=1)
      call move_to_line_item_(chkfile,6); call read_(chkfile,self%n_b)
      call look_for_(chkfile,"Number of basis functions",from=1)
      call move_to_line_item_(chkfile,6); call read_(chkfile,self%n_bf)
      n_independent = self%n_bf
      call look_for_(chkfile,"Number of independ",from=1)
      call move_to_line_item_(chkfile,6); call read_(chkfile,n_independent)
      WARN_IF(self%n_bf/=n_independent,"MOLECULE.READ:read_g09_FChk_file ... No. of basis fns /= no. of independent fns")
      WARN_IF(self%n_bf/=n_independent,"MOLECULE.READ:read_g09_FChk_file ... MO's are wrong, but density matrix is correct")

      ! Read atomic numbers & set
      call look_for_(chkfile,"Atomic numbers",from=1)
      call read_line_(chkfile)
      call create_(self%atom,self%n_atom)
      do a = 1,self%n_atom
         call read_(chkfile,Z)
         call set_atomic_number_(self%atom(a),Z)
         call set_nuclear_charge_(self%atom(a),REALIFY(Z))
      end do

      ! Assign core electrons if any
      call look_for_(chkfile,"Nuclear charges",from=1)
      call read_line_(chkfile)
      do a = 1,self%n_atom
         call read_(chkfile,Nc)
         Z = self%atom(a)%atomic_number
         if (nint(Nc)/=Z) then
            Z = Z-nint(Nc)
            call set_n_core_electrons_(self%atom(a),Z)
         end if
      end do

      ! Read in coordinates
      call look_for_(chkfile,"Current cartesian coordinates",from=1)
      call read_line_(chkfile)
      do a = 1,self%n_atom
         call read_(chkfile,pos)
         call set_position_(self%atom(a),pos)
      end do

      ! Set a name?
      if (self%name=="unknown") self%name = chemical_formula_(self%atom)

      ! Read gaussian shell info.
      label = "Number of contracted shells"
      call look_for_(chkfile,label,from=1)
      call move_to_line_item_(chkfile,6); call read_(chkfile,n_shell)
      call look_for_(chkfile,"Number of primitive shells",from=1)
      call move_to_line_item_(chkfile,6); call read_(chkfile,n_prim_shells)
      call create_(G_shell_type,n_shell)
      call create_(G_n_prim_for_shell,n_shell)
      call create_(G_atom_for_shell,n_shell)
      call create_(G_ex,n_prim_shells)
      call create_(G_cc,n_prim_shells)
      call look_for_(chkfile,"Shell types",from=1); call read_line_(chkfile)
      call read_(chkfile,G_shell_type)
      DIE_IF(any(G_shell_type<-1),"MOLECULE.READ:read_g09_FChk_file ... unknown shell types exist")
      call look_for_(chkfile,"Number of primitives per shell",from=1); call read_line_(chkfile)
      call read_(chkfile,G_n_prim_for_shell)
      call look_for_(chkfile,"Shell to atom map",from=1); call read_line_(chkfile)
      call read_(chkfile,G_atom_for_shell)
      call look_for_(chkfile,"Primitive exponents",from=1); call read_line_(chkfile)
      call read_(chkfile,G_ex)
      call look_for_(chkfile,"Contraction coefficients",from=1); call read_line_(chkfile)
      call read_(chkfile,G_cc)
      call look_for_(chkfile,"Total Energy",from=1)
      call move_to_line_item_(chkfile,4); call read_(chkfile,self%SCF_data%energy)

      ! SP type shells
      call create_(G_cp,n_prim_shells); G_cp = 0
      call create_(atom_p,self%n_atom); atom_p = 0
      if (any(G_shell_type==-1)) then
         do s = 1,n_shell
            if (G_shell_type(s)/=-1) cycle
            a = G_atom_for_shell(s)
            atom_p(a) = atom_p(a)+1
            n_prim_shells = n_prim_shells+G_n_prim_for_shell(s)
         end do
         call look_for_(chkfile,"P(S=P)",from=1); call read_line_(chkfile)
         call read_(chkfile,G_cp)
      end if

      ! Get correct number of shells
      self%n_shell = n_shell+sum_elements_(atom_p)
      call destroy_(atom_p)

      ! Create basis set arrays
      call create_(shell_l,self%n_shell)
      call create_(n_prim_for_shell,self%n_shell)
      call create_(atom_for_shell,self%n_shell)
      call create_(ex,n_prim_shells)
      call create_(cc,n_prim_shells)

   ! stdout.show(" n_shell =", n_shell)
   ! stdout.show(".n_shell =",.n_shell)

   ! stdout.text("Shell types:")
   ! stdout.put(G_shell_type)

   ! stdout.text("Number of prims for:")
   ! stdout.put(G_n_prim_for_shell)

   ! stdout.text("Atom for shell:")
   ! stdout.put(G_atom_for_shell)

   ! stdout.text("Primitive exponents:")
   ! stdout.put(G_ex)

   ! stdout.text("Contractions:")
   ! stdout.put(G_cc)

   ! stdout.text("P(SP) contractions:")
   ! stdout.put(G_cp)

      ! Copy the basis. Expand out L shells.
      t = 0
      l = 0; h = 0
      do s = 1,n_shell
         t = t+1
         n_prim = G_n_prim_for_shell(s)
         f = l+1; l = l+n_prim
         g = h+1; h = h+n_prim
         if (G_shell_type(s)==-1) then
            shell_l(t) = 0  ! S part of L shell
            n_prim_for_shell(t) = n_prim
            atom_for_shell(t) = G_atom_for_shell(s)
            ex(f:l) = G_ex(g:h)
            cc(f:l) = G_cc(g:h)
            t = t+1
            f = l+1
            l = l+n_prim
            shell_l(t) = 1  ! P part of L shell
            n_prim_for_shell(t) = n_prim
            atom_for_shell(t) = G_atom_for_shell(s)
            ex(f:l) = G_ex(g:h)
            cc(f:l) = G_cp(g:h)
         else
            ex(f:l) = G_ex(g:h)
            cc(f:l) = G_cc(g:h)
            shell_l(t) = abs(G_shell_type(s))  ! WARNING: sometimes this can be -2
            n_prim_for_shell(t) = n_prim
            atom_for_shell(t) = G_atom_for_shell(s)
         end if
      end do
      call destroy_(G_cp)
      call destroy_(G_shell_type)
      call destroy_(G_atom_for_shell)
      call destroy_(G_n_prim_for_shell)
      call destroy_(G_ex)
      call destroy_(G_cc)
      ENSURE(self%n_shell==t,"MOLECULE.READ:read_g09_FChk_file ... wrong number of shells")

      ! Set basis
      call set_basis_from_gX_data_(self,shell_l,n_prim_for_shell,atom_for_shell,ex,cc)
      call destroy_(cc)
      call destroy_(ex)
      call destroy_(atom_for_shell)
      call destroy_(n_prim_for_shell)
      call destroy_(shell_l)

      ! Finalize atom info/basis sets
      call update_atom_basis_group_info_(self)

      ! Read in orbital energies.
      call create_(orb_energy,n_independent)
      call look_for_(chkfile,"Alpha Orbital Energies",from=1)
      call read_line_(chkfile); call read_(chkfile,orb_energy)
      call destroy_(self%MO_energies)
      if (trim(next_str_(chkfile))=="Beta") then
         restricted = FALSE
         call set_SCF_kind_(self%SCF_data,"uhf")
         call create_(self%MO_energies,self%n_bf,"u ")
         self%MO_energies%a = ZERO
         self%MO_energies%a(1:n_independent) = orb_energy
         call read_line_(chkfile); call read_(chkfile,orb_energy)
         self%MO_energies%b = ZERO
         self%MO_energies%b(1:n_independent) = orb_energy
         call destroy_(orb_energy)
      else
         restricted = TRUE
         call set_SCF_kind_(self%SCF_data,"rhf")
         call create_(self%MO_energies,self%n_bf,"r ")
         self%MO_energies%r = ZERO
         self%MO_energies%r(1:n_independent) = orb_energy
         call destroy_(orb_energy)
      end if

      ! Read in molecular orbitals.
      call create_(temp_mat,n_independent,self%n_bf)
      call look_for_(chkfile,"Alpha MO",from=1)
      call read_line_(chkfile)
      call read_(chkfile,temp_mat)
      fbf = self%first_basis_fn_for_shell
      lbf = self%last_basis_fn_for_shell
      ang = self%angular_moment_for_shell
      call destroy_(self%MOs)
      if (trim(next_str_(chkfile))=="Beta") then
         call create_(self%MOs,self%n_bf,"u ")
         self%MOs%a = ZERO
         self%MOs%a(1:self%n_bf,1:n_independent) = transpose(temp_mat)
         call read_line_(chkfile); call read_(chkfile,temp_mat)
         self%MOs%b = ZERO
         self%MOs%b(1:self%n_bf,1:n_independent) = transpose(temp_mat)
         call destroy_(temp_mat)
         call swap_from_Gaussian_C_order_(self%MOs%a,"row",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(self%MOs%b,"row",fbf,lbf,ang)
      else
         call create_(self%MOs,self%n_bf,"r ")
         self%MOs%r = ZERO
         self%MOs%r(1:self%n_bf,1:n_independent) = transpose(temp_mat)
         call destroy_(temp_mat)
         call swap_from_Gaussian_C_order_(self%MOs%r,"row",fbf,lbf,ang)
      end if

      ! Read in density matrix.
      call read_gX_SCF_dm_(self,chkfile,restricted,self%density_mx)

      ! Extract MP2 density matrix to file.  UHF untested
      call read_gX_MP2_dm_(self,chkfile,restricted,self%density_mx)

      ! Extract MP3 density matrix to file.  UHF untested
      call read_gX_MP3_dm_(self,chkfile,restricted,self%density_mx)

      ! Extract CC density matrix to file.  UHF untested
      call read_gX_CC_dm_(self,chkfile,restricted,self%density_mx)

      ! Read in force constants
      if (has_string_(chkfile,"Cartesian Force Constants")) then
         n3n = 3*size(self%atom)
         ntr = triangle_number_(n3n)
         call create_(self%force_constants,n3n,n3n)
         call create_(fc,ntr)
         call look_for_(chkfile,"Cartesian Force Constants",from=1)
         call read_line_(chkfile)
         call read_(chkfile,fc)
         call symmetric_unzip_triangle_(self%force_constants,fc)
         call destroy_(fc)
         self%FD_hessian_scale = -ONE
      end if

      ! Close chkfile
      call close_(chkfile); call destroy_(chkfile)

      ! Save data in archive files
      ! NOTE: MO's & density matrix are wrt unnormalized basis fn's
      call set_(archive,self%name,"density_mx")
      call write_(archive,self%density_mx)
      call set_(archive,self%name,"MOs")
      call write_(archive,self%MOs)
      call set_(archive,self%name,"MO_energies")
      call write_(archive,self%MO_energies)
      if (allocated(self%crystal)) then
         call resolve_axis_system_(self)
         call update_fragment_info_(self%crystal,self%atom)
      end if
   end subroutine

   subroutine read_gX_SCF_dm(self,chkfile,restricted,dm)
   ! Read a gaussian density matrix with TAG? which may be
   ! "r " (or not) from "chkfile". Archive it as NAME?.
   ! If "dm" is present it is created to be the density matrix.
      type(MOLECULE_TYPE), INOUT :: self
      type(TEXTFILE_TYPE), INOUT :: chkfile
      BIN, IN :: restricted
      type(OPMATRIX_TYPE), allocatable, optional :: dm
      type(OPMATRIX_TYPE), allocatable :: D,spin,total
      VEC(INT,:), allocatable :: fbf,lbf,ang
      type(ARCHIVE_TYPE) :: archive
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.READ:read_gX_SCF_dm ... not implemented")
   ENSURE(self%n_bf>0,"MOLECULE.READ:read_gX_SCF_dm ... no basis functions")

      ! Extract density matrix

      ! The following code is inherited from MOLECULE.READ
      if (NOT has_string_(chkfile,"SCF Density")) return
      fbf = self%first_basis_fn_for_shell
      lbf = self%last_basis_fn_for_shell
      ang = self%angular_moment_for_shell
      if (restricted) then

         ! Read restricted density (triangle) "name" into "D"
         call create_(D,self%n_bf,"r ")
         call create_(D%tri,l_compress_(D,"r "))
         call look_for_(chkfile,"Total SCF Density")
         call read_line_(chkfile)
         call read_(chkfile,D%tri)
         call uncompress_(D)

         ! Swap orbital order
         call swap_from_Gaussian_C_order_(D%r,"1",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%r,"2",fbf,lbf,ang)
      else

         ! Create
         call create_(D,self%n_bf,"u ")
         call create_(total,self%n_bf,"r ")
         call create_(spin,self%n_bf,"r ")

         ! Read total density (triangle) TAG? into "total"
         call create_(total%tri,l_compress_(total,"r "))
         call look_for_(chkfile,"Total SCF Density")
         call read_line_(chkfile)
         call read_(chkfile,total%tri)
         call uncompress_(total)

         ! Read spin density (triangle) trim(tag) into "spin"
         call create_(spin%tri,l_compress_(spin,"r "))
         call look_for_(chkfile,"Spin SCF Density")
         call read_line_(chkfile)
         call read_(chkfile,spin%tri)
         call uncompress_(spin)

         ! Define alpha and beta densities
         D%b = (total%r-spin%r)/2
         D%a = (spin%r+total%r)/2

         ! Clean
         call destroy_(spin)
         call destroy_(total)

         ! Swap orbital order
         call swap_from_Gaussian_C_order_(D%a,"1",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%a,"2",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%b,"1",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%b,"2",fbf,lbf,ang)
      end if

      ! Archive the density "D"
      call set_(archive,self%name,"SCF_density_mx")
      call write_(archive,D)

      ! Clean
      if (present(dm)) then
         call destroy_(dm)
         dm = D
      end if
      call destroy_(D)
   end subroutine

   subroutine read_gX_MP2_dm(self,chkfile,restricted,dm)
   ! Read a gaussian density matrix with TAG? which may be
   ! "r " (or not) from "chkfile". Archive it as NAME?.
   ! If "dm" is present it is created to be the density matrix.
      type(MOLECULE_TYPE), INOUT :: self
      type(TEXTFILE_TYPE), INOUT :: chkfile
      BIN, IN :: restricted
      type(OPMATRIX_TYPE), allocatable, optional :: dm
      type(OPMATRIX_TYPE), allocatable :: D,spin,total
      VEC(INT,:), allocatable :: fbf,lbf,ang
      type(ARCHIVE_TYPE) :: archive
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.READ:read_gX_MP2_dm ... not implemented")
   ENSURE(self%n_bf>0,"MOLECULE.READ:read_gX_MP2_dm ... no basis functions")

      ! Extract density matrix

      ! The following code is inherited from MOLECULE.READ
      if (NOT has_string_(chkfile,"MP2 Density")) return
      fbf = self%first_basis_fn_for_shell
      lbf = self%last_basis_fn_for_shell
      ang = self%angular_moment_for_shell
      if (restricted) then

         ! Read restricted density (triangle) "name" into "D"
         call create_(D,self%n_bf,"r ")
         call create_(D%tri,l_compress_(D,"r "))
         call look_for_(chkfile,"Total MP2 Density")
         call read_line_(chkfile)
         call read_(chkfile,D%tri)
         call uncompress_(D)

         ! Swap orbital order
         call swap_from_Gaussian_C_order_(D%r,"1",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%r,"2",fbf,lbf,ang)
      else

         ! Create
         call create_(D,self%n_bf,"u ")
         call create_(total,self%n_bf,"r ")
         call create_(spin,self%n_bf,"r ")

         ! Read total density (triangle) TAG? into "total"
         call create_(total%tri,l_compress_(total,"r "))
         call look_for_(chkfile,"Total MP2 Density")
         call read_line_(chkfile)
         call read_(chkfile,total%tri)
         call uncompress_(total)

         ! Read spin density (triangle) trim(tag) into "spin"
         call create_(spin%tri,l_compress_(spin,"r "))
         call look_for_(chkfile,"Spin MP2 Density")
         call read_line_(chkfile)
         call read_(chkfile,spin%tri)
         call uncompress_(spin)

         ! Define alpha and beta densities
         D%b = (total%r-spin%r)/2
         D%a = (spin%r+total%r)/2

         ! Clean
         call destroy_(spin)
         call destroy_(total)

         ! Swap orbital order
         call swap_from_Gaussian_C_order_(D%a,"1",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%a,"2",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%b,"1",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%b,"2",fbf,lbf,ang)
      end if

      ! Archive the density "D"
      call set_(archive,self%name,"MP2_density_mx")
      call write_(archive,D)

      ! Clean
      if (present(dm)) then
         call destroy_(dm)
         dm = D
      end if
      call destroy_(D)
   end subroutine

   subroutine read_gX_MP3_dm(self,chkfile,restricted,dm)
   ! Read a gaussian density matrix with TAG? which may be
   ! "r " (or not) from "chkfile". Archive it as NAME?.
   ! If "dm" is present it is created to be the density matrix.
      type(MOLECULE_TYPE), INOUT :: self
      type(TEXTFILE_TYPE), INOUT :: chkfile
      BIN, IN :: restricted
      type(OPMATRIX_TYPE), allocatable, optional :: dm
      type(OPMATRIX_TYPE), allocatable :: D,spin,total
      VEC(INT,:), allocatable :: fbf,lbf,ang
      type(ARCHIVE_TYPE) :: archive
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.READ:read_gX_MP3_dm ... not implemented")
   ENSURE(self%n_bf>0,"MOLECULE.READ:read_gX_MP3_dm ... no basis functions")

      ! Extract density matrix

      ! The following code is inherited from MOLECULE.READ
      if (NOT has_string_(chkfile,"MP3 Density")) return
      fbf = self%first_basis_fn_for_shell
      lbf = self%last_basis_fn_for_shell
      ang = self%angular_moment_for_shell
      if (restricted) then

         ! Read restricted density (triangle) "name" into "D"
         call create_(D,self%n_bf,"r ")
         call create_(D%tri,l_compress_(D,"r "))
         call look_for_(chkfile,"Total MP3 Density")
         call read_line_(chkfile)
         call read_(chkfile,D%tri)
         call uncompress_(D)

         ! Swap orbital order
         call swap_from_Gaussian_C_order_(D%r,"1",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%r,"2",fbf,lbf,ang)
      else

         ! Create
         call create_(D,self%n_bf,"u ")
         call create_(total,self%n_bf,"r ")
         call create_(spin,self%n_bf,"r ")

         ! Read total density (triangle) TAG? into "total"
         call create_(total%tri,l_compress_(total,"r "))
         call look_for_(chkfile,"Total MP3 Density")
         call read_line_(chkfile)
         call read_(chkfile,total%tri)
         call uncompress_(total)

         ! Read spin density (triangle) trim(tag) into "spin"
         call create_(spin%tri,l_compress_(spin,"r "))
         call look_for_(chkfile,"Spin MP3 Density")
         call read_line_(chkfile)
         call read_(chkfile,spin%tri)
         call uncompress_(spin)

         ! Define alpha and beta densities
         D%b = (total%r-spin%r)/2
         D%a = (spin%r+total%r)/2

         ! Clean
         call destroy_(spin)
         call destroy_(total)

         ! Swap orbital order
         call swap_from_Gaussian_C_order_(D%a,"1",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%a,"2",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%b,"1",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%b,"2",fbf,lbf,ang)
      end if

      ! Archive the density "D"
      call set_(archive,self%name,"MP3_density_mx")
      call write_(archive,D)

      ! Clean
      if (present(dm)) then
         call destroy_(dm)
         dm = D
      end if
      call destroy_(D)
   end subroutine

   subroutine read_gX_CC_dm(self,chkfile,restricted,dm)
   ! Read a gaussian density matrix with TAG? which may be
   ! "r " (or not) from "chkfile". Archive it as NAME?.
   ! If "dm" is present it is created to be the density matrix.
      type(MOLECULE_TYPE), INOUT :: self
      type(TEXTFILE_TYPE), INOUT :: chkfile
      BIN, IN :: restricted
      type(OPMATRIX_TYPE), allocatable, optional :: dm
      type(OPMATRIX_TYPE), allocatable :: D,spin,total
      VEC(INT,:), allocatable :: fbf,lbf,ang
      type(ARCHIVE_TYPE) :: archive
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.READ:read_gX_CC_dm ... not implemented")
   ENSURE(self%n_bf>0,"MOLECULE.READ:read_gX_CC_dm ... no basis functions")

      ! Extract density matrix

      ! The following code is inherited from MOLECULE.READ
      if (NOT has_string_(chkfile,"CC Density")) return
      fbf = self%first_basis_fn_for_shell
      lbf = self%last_basis_fn_for_shell
      ang = self%angular_moment_for_shell
      if (restricted) then

         ! Read restricted density (triangle) "name" into "D"
         call create_(D,self%n_bf,"r ")
         call create_(D%tri,l_compress_(D,"r "))
         call look_for_(chkfile,"Total CC Density")
         call read_line_(chkfile)
         call read_(chkfile,D%tri)
         call uncompress_(D)

         ! Swap orbital order
         call swap_from_Gaussian_C_order_(D%r,"1",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%r,"2",fbf,lbf,ang)
      else

         ! Create
         call create_(D,self%n_bf,"u ")
         call create_(total,self%n_bf,"r ")
         call create_(spin,self%n_bf,"r ")

         ! Read total density (triangle) TAG? into "total"
         call create_(total%tri,l_compress_(total,"r "))
         call look_for_(chkfile,"Total CC Density")
         call read_line_(chkfile)
         call read_(chkfile,total%tri)
         call uncompress_(total)

         ! Read spin density (triangle) trim(tag) into "spin"
         call create_(spin%tri,l_compress_(spin,"r "))
         call look_for_(chkfile,"Spin CC Density")
         call read_line_(chkfile)
         call read_(chkfile,spin%tri)
         call uncompress_(spin)

         ! Define alpha and beta densities
         D%b = (total%r-spin%r)/2
         D%a = (spin%r+total%r)/2

         ! Clean
         call destroy_(spin)
         call destroy_(total)

         ! Swap orbital order
         call swap_from_Gaussian_C_order_(D%a,"1",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%a,"2",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%b,"1",fbf,lbf,ang)
         call swap_from_Gaussian_C_order_(D%b,"2",fbf,lbf,ang)
      end if

      ! Archive the density "D"
      call set_(archive,self%name,"CC_density_mx")
      call write_(archive,D)

      ! Clean
      if (present(dm)) then
         call destroy_(dm)
         dm = D
      end if
      call destroy_(D)
   end subroutine


!  ========
!  WFN file
!  ========

   subroutine read_wfn_file(self,filename)
   ! Open and read a file named "name", either a 
   ! . gaussian .FChk file
   ! . A .molden file
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: filename
      if (ends_in_(filename,"fchk")) then; call read_g09_FChk_file_(self,filename)
      else if (ends_in_(filename,"molden")) then; call read_molden_file_(self,filename)
      else
         DIE("MOLECULE.READ:read_wfn_file ... unknown file format for file: '"//trim(filename)//"'")
      end if
   end subroutine


!  ============
!  Read archive
!  ============

   subroutine read_archive_0(self)
   ! Read the archive called "name" with "genre", e.g.  "r ".
   ! For use with TONTO generated archives.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name,genre
   ENSURE(n_line_items_(stdin)==3,"MOLECULE.READ:read_archive_0 ... must specify an archive and a kind")
   ENSURE(self%n_bf>0,"MOLECULE.READ:read_archive_0 ... need to already know the number of basis functions")
      call read_(stdin,name)
      call read_(stdin,genre)
      call text_(stdout,"reading archive : "//trim(name))
      call read_archive_(self,name,genre)
   end subroutine

   subroutine read_archive_and_normalise(self)
   ! Read the archive called "name" with "genre", e.g. "r ", and unnormalize
   ! the orbitals after reading. For use with TONTO generated archives.
   !
   ! This is a separate keyword because the parsing belongs here, in the
   ! keyword layer, not in the library routine it calls. It also replaces a
   ! trailing "normalise" qualifier on the read_archive line, which could
   ! never fire -- read_archive ENSUREs three items on the line and the
   ! qualifier was only looked for on a line of two.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name,genre
   ENSURE(n_line_items_(stdin)==3,"MOLECULE.READ:read_archive_and_normalise ... must specify an archive and a kind")
   ENSURE(self%n_bf>0,"MOLECULE.READ:read_archive_and_normalise ... need to already know the number of basis functions")
      call read_(stdin,name)
      call read_(stdin,genre)
      call text_(stdout,"reading archive : "//trim(name))
      call read_archive_(self,name,genre,normalise=TRUE)
   end subroutine

   subroutine read_archive_1(self,name,genre,normalise)
   ! Read the archive called "name" with "genre", e.g. "r ".
   ! For use with TONTO generated archives. Handle specific
   ! lambda's in the name (if the name is of the form ....,lambda=...)
   ! Set "normalise" to unnormalize the orbitals after reading.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: name
      STR(len=*), IN :: genre
      BIN, optional, IN :: normalise
      INT :: i
      STR(len=STR_SIZE) :: nm
      VEC(INT,:), allocatable :: fbf,lbf,ang
      BIN :: do_norm
      type(ARCHIVE_TYPE) :: arch
   ENSURE(self%n_bf>0,"MOLECULE.READ:read_archive_1 ... need to already know the number of basis functions")

      ! Set archive "name", "genre"
      call set_(arch,self%name,name,genre)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! Now remove any lambda in the "name"
      nm = name
      i = index_of_character_in_(nm,",")
      if (i>0) nm = name(1:i-1)

      ! Normalize? This is an ARGUMENT, not something read from stdin.
      !
      ! It used to be "if (stdin.buffer.n_items==2) stdin.read(normalize)",
      ! which was wrong twice over. This is a library routine -- it takes its
      ! operands as arguments and is called from MOLECULE.SCF, MOLECULE.MAIN
      ! and run_sf_derivs -- so reaching for the global stdin made it depend
      ! on whatever line the job file happened to be on, and made it segfault
      ! outright in a program that has no stdin at all (hart). It was also
      ! dead: the keyword handler above ENSUREs n_line_items==3, which can
      ! never equal 2, so no job file could ever switch it on. Use the
      ! read_archive_and_normalise keyword for that. See TASKS_AND_HISTORY.md.
      do_norm = FALSE
      if (present(normalise)) do_norm = normalise

      ! Tell them
      if (do_norm) then
         call flush_(stdout)
         call text_(stdout,"Reading normalized "//trim(nm))
         call flush_(stdout)
      end if
      fbf = self%first_basis_fn_for_shell
      lbf = self%last_basis_fn_for_shell
      ang = self%angular_moment_for_shell

      ! Read the archive
      select case (nm)

      case ("covalent_orbitals ","RG_COs            ")
         if (NOT allocated(self%NOs)) call create_(self%NOs,self%n_bf)
         call read_(arch,self%NOs)
         if (do_norm) call unnormalize_(self%NOs,"1",fbf,lbf,ang)

      case ("ionic_orbitals    ","RG_IOs            ")
         if (NOT allocated(self%NOs)) call create_(self%NOs,self%n_bf)
         call read_(arch,self%NOs)
         if (do_norm) call unnormalize_(self%NOs,"1",fbf,lbf,ang)

      case ("molecular_orbitals","MOs               ","HF_MOs            ","KS_MOs            ")
         if (NOT allocated(self%MOs)) call create_(self%MOs,self%n_bf)
         call read_(arch,self%MOs)
         if (do_norm) call unnormalize_(self%MOs,"1",fbf,lbf,ang)

         ! Read in lambda=0 M0s in there.
         call set_(arch,self%name,"MOs,lambda=0.000000",genre)
         if (exists_(arch)) then
            if (NOT allocated(self%M0s)) call create_(self%M0s,self%n_bf)
            call read_(arch,self%M0s)
            if (do_norm) call unnormalize_(self%M0s,"1",fbf,lbf,ang)
         end if

      case ("M0s               ")
         ! lambda=0.00 MOs.
         if (NOT allocated(self%MOs)) call create_(self%MOs,self%n_bf)

      case ("MO_energies       ","HF_MO_energies    ","KS_MO_energies    ")
         if (NOT allocated(self%MO_energies)) call create_(self%MO_energies,self%n_bf)
         call read_(arch,self%MO_energies)

      case ("natural_orbitals  ","NOs               ")
         if (NOT allocated(self%NOs)) call create_(self%NOs,self%n_bf)
         call read_(arch,self%NOs)
         if (do_norm) call unnormalize_(self%NOs,"1",fbf,lbf,ang)

      case ("occupation_nos    ","NO_occ_nos        ")
         if (NOT allocated(self%NO_occ_nos)) call create_(self%NO_occ_nos,self%n_bf)
         call read_(arch,self%NO_occ_nos)

      case ("density_matrix    ","density_mx        ","hf_density_matrix ","HF_density_mx     ","ks_density_matrix ","KS_density_mx     ","mp2_density_matrix","MP2_density_mx    ","mp3_density_matrix","MP3_density_mx    ")
         if (NOT allocated(self%density_mx)) call create_(self%density_mx,self%n_bf)
         call read_(arch,self%density_mx)
         if (do_norm) then
            call unnormalize_(self%density_mx,self%first_basis_fn_for_shell,self%last_basis_fn_for_shell,self%angular_moment_for_shell)
         end if

      case ("fock_matrix       ","Fock_mx           ","Kohn-Sham_mx      ")
         if (NOT allocated(self%fock_mx)) call create_(self%fock_mx,self%n_bf)
         call read_(arch,self%fock_mx)
         if (do_norm) call unnormalize_(self%fock_mx,fbf,lbf,ang)

      case ("U_electric_dipole ")
         if (NOT allocated(self%U_electric_dipole)) call create_(self%U_electric_dipole,self%n_bf,self%n_bf,3)
         call read_(arch,self%U_electric_dipole)

      case ("U2_electric_dipole ")
         if (NOT allocated(self%U2_electric_dipole)) call create_(self%U2_electric_dipole,self%n_bf,self%n_bf,6)
         call read_(arch,self%U2_electric_dipole)

      case default
         allocate(tonto%known_keywords(31))
         tonto%known_keywords(1) = "covalent_orbitals "
         tonto%known_keywords(2) = "RG_COs            "
         tonto%known_keywords(3) = "ionic_orbitals    "
         tonto%known_keywords(4) = "RG_IOs            "
         tonto%known_keywords(5) = "molecular_orbitals"
         tonto%known_keywords(6) = "MOs               "
         tonto%known_keywords(7) = "HF_MOs            "
         tonto%known_keywords(8) = "KS_MOs            "
         tonto%known_keywords(9) = "M0s               "
         tonto%known_keywords(10) = "MO_energies       "
         tonto%known_keywords(11) = "HF_MO_energies    "
         tonto%known_keywords(12) = "KS_MO_energies    "
         tonto%known_keywords(13) = "natural_orbitals  "
         tonto%known_keywords(14) = "NOs               "
         tonto%known_keywords(15) = "occupation_nos    "
         tonto%known_keywords(16) = "NO_occ_nos        "
         tonto%known_keywords(17) = "density_matrix    "
         tonto%known_keywords(18) = "density_mx        "
         tonto%known_keywords(19) = "hf_density_matrix "
         tonto%known_keywords(20) = "HF_density_mx     "
         tonto%known_keywords(21) = "ks_density_matrix "
         tonto%known_keywords(22) = "KS_density_mx     "
         tonto%known_keywords(23) = "mp2_density_matrix"
         tonto%known_keywords(24) = "MP2_density_mx    "
         tonto%known_keywords(25) = "mp3_density_matrix"
         tonto%known_keywords(26) = "MP3_density_mx    "
         tonto%known_keywords(27) = "fock_matrix       "
         tonto%known_keywords(28) = "Fock_mx           "
         tonto%known_keywords(29) = "Kohn-Sham_mx      "
         tonto%known_keywords(30) = "U_electric_dipole "
         tonto%known_keywords(31) = "U2_electric_dipole "
         call unknown_(tonto,nm,"MOLECULE.READ:read_archive")
         deallocate(tonto%known_keywords)
      end select

      ! Clean up
      call close_(arch)
   end subroutine

   subroutine read_ascii_archive(self)
   ! Read the archive called "name" with "genre", e.g. "r ".
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name,genre
      type(ARCHIVE_TYPE) :: arch
   ENSURE(stdin%buffer%n_items==3,"MOLECULE.READ:read_ascii_archive ... must specify an archive and a kind")
   ENSURE(self%n_bf>0,"MOLECULE.READ:read_ascii_archive ... need to already know the number of basis functions")

      ! Read archive name & genre
      call read_(stdin,name)
      call read_(stdin,genre)

      ! Set archive "name", "genre" & ascii format
      call set_(arch,self%name,name,genre=genre,format="ascii")

      ! Read the archive
      select case (name)

      case ("covalent_orbitals ","RG_COs            ")
         if (NOT allocated(self%NOs)) call create_(self%NOs,self%n_bf)
         call read_(arch,self%NOs,by_column=TRUE)

      case ("ionic_orbitals    ","RG_IOs            ")
         if (NOT allocated(self%NOs)) call create_(self%NOs,self%n_bf)
         call read_(arch,self%NOs,by_column=TRUE)

      case ("molecular_orbitals","MOs               ","HF_MOs            ","KS_MOs            ")
         if (NOT allocated(self%MOs)) call create_(self%MOs,self%n_bf)
         call read_(arch,self%MOs,by_column=TRUE)

      case ("MO_energies       ","HF_MO_energies    ","KS_MO_energies    ")
         if (NOT allocated(self%MO_energies)) call create_(self%MO_energies,self%n_bf)
         call read_(arch,self%MO_energies)

      case ("natural_orbitals  ","NOs               ")
         if (NOT allocated(self%NOs)) call create_(self%NOs,self%n_bf)
         call read_(arch,self%NOs,by_column=TRUE)

      case ("no_occupation_nos ","NO_occ_nos        ")
         if (NOT allocated(self%NO_occ_nos)) call create_(self%NO_occ_nos,self%n_bf)
         call read_(arch,self%NO_occ_nos)

      case ("density_matrix    ","density_mx        ","hf_density_matrix ","HF_density_mx     ","ks_density_matrix ","KS_density_mx     ","mp2_density_matrix","MP2_density_mx    ","mp3_density_matrix","MP3_density_mx    ")
         if (NOT allocated(self%density_mx)) call create_(self%density_mx,self%n_bf)
         call read_(arch,self%density_mx,by_column=TRUE)

      case ("fock_matrix       ","Fock_mx           ","Kohn-Sham_mx      ")
         if (NOT allocated(self%fock_mx)) call create_(self%fock_mx,self%n_bf)
         call read_(arch,self%fock_mx,by_column=TRUE)

      case default
         allocate(tonto%known_keywords(28))
         tonto%known_keywords(1) = "covalent_orbitals "
         tonto%known_keywords(2) = "RG_COs            "
         tonto%known_keywords(3) = "ionic_orbitals    "
         tonto%known_keywords(4) = "RG_IOs            "
         tonto%known_keywords(5) = "molecular_orbitals"
         tonto%known_keywords(6) = "MOs               "
         tonto%known_keywords(7) = "HF_MOs            "
         tonto%known_keywords(8) = "KS_MOs            "
         tonto%known_keywords(9) = "MO_energies       "
         tonto%known_keywords(10) = "HF_MO_energies    "
         tonto%known_keywords(11) = "KS_MO_energies    "
         tonto%known_keywords(12) = "natural_orbitals  "
         tonto%known_keywords(13) = "NOs               "
         tonto%known_keywords(14) = "no_occupation_nos "
         tonto%known_keywords(15) = "NO_occ_nos        "
         tonto%known_keywords(16) = "density_matrix    "
         tonto%known_keywords(17) = "density_mx        "
         tonto%known_keywords(18) = "hf_density_matrix "
         tonto%known_keywords(19) = "HF_density_mx     "
         tonto%known_keywords(20) = "ks_density_matrix "
         tonto%known_keywords(21) = "KS_density_mx     "
         tonto%known_keywords(22) = "mp2_density_matrix"
         tonto%known_keywords(23) = "MP2_density_mx    "
         tonto%known_keywords(24) = "mp3_density_matrix"
         tonto%known_keywords(25) = "MP3_density_mx    "
         tonto%known_keywords(26) = "fock_matrix       "
         tonto%known_keywords(27) = "Fock_mx           "
         tonto%known_keywords(28) = "Kohn-Sham_mx      "
         call unknown_(tonto,name,"MOLECULE.READ:read_ascii_archive")
         deallocate(tonto%known_keywords)
      end select

      ! Clean up
      call close_(arch)
   end subroutine

   subroutine read_archive_2(self,opmatrix,archive_name,genre)
   ! Recover the "opmatrix" from an archive on disk with name "archive_name".
   ! The kind is guessed from "opmatrix".
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), allocatable, OUT :: opmatrix
      STR(len=*), IN :: archive_name
      STR(len=*), optional, IN :: genre
      type(ARCHIVE_TYPE) :: archive
   ENSURE(has_any_genre_(opmatrix),"MOLECULE.READ:read_archive_2 ... no opmatrix genre created")
      call set_(archive,self%name,archive_name)
      call read_(archive,opmatrix,genre)
   end subroutine

   subroutine read_archive_3(self,opvector,archive_name,genre)
   ! Recover the "opvector" from an archive on disk with name "archive_name".
   ! The kind is guessed from "opvector".
      type(MOLECULE_TYPE), INOUT :: self
      type(OPVECTOR_TYPE), OUT :: opvector
      STR(len=*), IN :: archive_name
      STR(len=*), optional, IN :: genre
      type(ARCHIVE_TYPE) :: archive
   ENSURE(has_any_genre_(opvector),"MOLECULE.READ:read_archive_3 ... no opvector genre created")
      call set_(archive,self%name,archive_name)
      call read_(archive,opvector,genre)
   end subroutine

   subroutine read_archive_4(self,item,archive_name,genre)
   ! Recover the "item" from an archive on disk with name "archive_name".
      type(MOLECULE_TYPE), INOUT :: self
      REAL, OUT :: item
      STR(len=*), IN :: archive_name
      STR(len=*), optional, IN :: genre
      type(ARCHIVE_TYPE) :: archive
      call set_(archive,self%name,archive_name)
      call read_(archive,item,genre)
   end subroutine

   subroutine read_SCF_archives(self)
   ! Copy the natural orbitals to be the MOs.
   ! If thw MO's are not there, try and read them from disk.
   ! The occupation numbers are the MO ones.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: genre
      type(ARCHIVE_TYPE) :: arch

      ! Get MO's 
      if (allocated(self%MOs)) then

         ! Already there
         genre = genre_(self%MOs)
      else if (allocated(self%SCF_data)) then

         ! Does it exist as on disk?
         genre = spinorbital_genre_(self%SCF_data)
         call set_(arch,self%name,"MOs",genre)
         if (exists_(arch)) then
            call create_(self%MOs,self%n_bf)
            call read_(arch,self%MOs)
            call close_(arch)
         end if
      end if

      ! Get density matrix
      if (allocated(self%density_mx)) then

         ! Already there
         genre = genre_(self%density_mx)
      else if (allocated(self%SCF_data)) then

         ! Does it exist on disk?
         genre = spinorbital_genre_(self%SCF_data)
         call set_(arch,self%name,"density_mx",genre)
         if (exists_(arch)) then
            call create_(self%density_mx,self%n_bf)
            call read_(arch,self%density_mx)
            call close_(arch)
         end if
      end if

      ! Get orbital energies
      if (NOT allocated(self%MO_energies)) then
         if (allocated(self%SCF_data)) then
            genre = spinorbital_genre_(self%SCF_data)
            call create_(self%MO_energies,self%n_bf,genre)
         ! Does it exist on disk?
            call set_(arch,self%name,"MO_energies",genre)
            if (exists_(arch)) then
               call read_(arch,self%MO_energies)
               call close_(arch)
            end if
         end if
      end if

      ! Clone the MO's
      if (allocated(self%MOs)) then
         self%NOs = self%MOs
         call create_(self%NO_occ_nos,self%n_bf,genre)
         call set_to_zero_(self%NO_occ_nos)
         call set_SCF_occupations_for_NOs_(self,genre)
      end if
   end subroutine


!  ===================
!  Redirect and revert
!  ===================

   subroutine redirect(self)
   ! Redirect input. (This code is for non-pointer self objects).
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: filename

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      filename = next_str_(stdin)
      call redirect_(stdin,trim(filename))
   end subroutine

   subroutine redirect_stdout(self)
   ! Redirect stdout to the file whose name is the following string
   ! in the current input file.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name
      if (FALSE) self = self
      call read_(stdin,name)
      call redirect_(stdout,name)
   end subroutine

   subroutine revert(self)
   ! Revert back to previous stdin file. (This code is for non-pointer self
   ! objects).
      type(MOLECULE_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call revert_(stdin)
   end subroutine

   subroutine revert_stdout(self)
   ! Revert stdout back to the previously stored input file
      type(MOLECULE_TYPE), INOUT :: self
      if (FALSE) self = self
      call revert_(stdout)
   end subroutine

end module
