!===========================================================
!
! MOLECULE.PLOT: An object representation of a molecule.
!
! These are plot-controlling procedures. Most of the actual 
! grids are made in the .GRID submodule.
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1996-2012
! Copyright (C) Dylan Jayatilaka 2012-
! Copyright (C) Dylan Jayatilaka 2021
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
!===========================================================
module MOLECULE_PLOT_MODULE

#  include "macros"

   include "molecule.plot.use"

   implicit none

   include "molecule.plot.int"


contains

!  =================================
!  2D plane or 3D cube or cell plots
!  =================================

   subroutine plot(self)
   ! Do one of the many kinds of plot calculations
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%plot_grid),"MOLECULE.PLOT:plot ... no grid")

      ! Set up
      call set_up_for_plot_(self,self%plot_grid%plot_kind)

      ! Use unit cell as bbox if requested
      if (self%plot_grid%use_unit_cell_as_bbox) then
         WARN("MOLECULE.PLOT:plot ... resetting plot axes, plot widths from unit cell")
         ENSURE(allocated(self%crystal),"MOLECULE.PLOT:plot ... no crystal or unit cell for plot!")
         call set_box_scale_factor_(self%plot_grid,ONE)
         call set_bbox_from_unit_cell_(self%plot_grid,self%crystal%unit_cell)
      end if

      ! Use unit cell as bbox if requested
      if (self%plot_grid%plot_kind=="crystal23_ed_v1") then
         WARN("MOLECULE.PLOT:plot ... resetting plot axes, plot widths from unit cell")
         ENSURE(allocated(self%crystal),"MOLECULE.PLOT:plot ... no crystal or unit cell for plot!")
         call set_box_scale_factor_(self%plot_grid,ONE)
      !  USER MUST SET either desired_separation or n_x, n_y, n_z
      ! .plot_grid.set_desired_separation(0.1*BOHR_PER_ANGSTROM)
         call set_bbox_from_unit_cell_(self%plot_grid,self%crystal%unit_cell,midpt_is_center=TRUE)
      end if

      ! See plot_grid parameters
      call put_(self%plot_grid)

      ! Do the plot
      call do_plot_(self)
   end subroutine

   subroutine do_plot_0(self)
   ! Do one of the many kinds of plot calculations and write out the
   ! plot property into a file. These are usually 2D plots but they
   ! can be rectilinear volumetric plots, depending on how .plot_grid
   ! was defined by the user.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name
      VEC(REAL,:), allocatable :: grid
   ENSURE(allocated(self%plot_grid),"MOLECULE.PLOT:do_plot_0 ... no grid")

      ! Property values.
      call create_(grid,self%plot_grid%n_pt)

      ! Do the plot
      call do_plot_(self,grid)

      ! Print out
      name = plot_file_name_(self%plot_grid,self%n_a)
      call dump_plot_grid_(self,grid,name)

      ! Clean up
      call destroy_(grid)
   end subroutine

   subroutine do_plot_1(self,grid)
   ! Do one of the many kinds of plot calculations and store the
   ! property values in "grid". These are usually 2D plots but they
   ! can be rectilinear 3D volumetric plots, depending on how
   ! .plot_grid was defined by the user.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), allocatable :: pt
   ENSURE(allocated(self%plot_grid),"MOLECULE.PLOT:do_plot_1 ... no grid")
   ENSURE(size(grid)==self%plot_grid%n_pt,"MOLECULE.PLOT:do_plot_1 ... no grid")

      ! Make the grid points
      call create_(pt,self%plot_grid%n_pt,3)
      if (self%plot_grid%plot_kind/="crystal23_ed_v1") then

         ! Standard cell
         call make_points_(self%plot_grid,pt)
      else

         ! Standard grid mapped to CRYSTAL23 [-1/2,+1/2] cell
         call make_01_uc_pts_as_mhph_pts_(self%plot_grid,pt)
      end if

      ! Make the property values.
      call make_prop_values_(self,grid,pt)

      ! Clean up
      call destroy_(pt)
   end subroutine

   subroutine plots(self)
   ! Do one of the many kinds of plot calculations
      type(MOLECULE_TYPE), INOUT :: self
      INT :: i
   ENSURE(allocated(self%plot_grid),"MOLECULE.PLOT:plots ... no grid")
   ENSURE(allocated(self%plot_grid%plot_kinds),"MOLECULE.PLOT:plots ... no multiple plot_kinds!")

      ! Loop over plots and do each one
      do i = 1,size(self%plot_grid%plot_kinds)
         call set_plot_kind_(self%plot_grid,self%plot_grid%plot_kinds(i))
         call set_plot_units_(self%plot_grid,self%plot_grid%plot_kinds_units(i))
         call plot_(self)
      end do

      ! Reset to blank
      call set_plot_kind_(self%plot_grid," ")
      call set_plot_units_(self%plot_grid," ")
   end subroutine

   subroutine plot_fragments(self)
   ! Do the summed-fragment property density on ".plot_grid".
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%plot_grid),"MOLECULE.PLOT:plot_fragments ... no grid")
   ENSURE(allocated(self%atom_group),"MOLECULE.PLOT:plot_fragments ... no atom_group info")
   ENSURE(allocated(self%crystal),"MOLECULE.PLOT:plot_fragments ... no crystal")

      ! Use unit cell as bbox if requested
      if (self%plot_grid%use_unit_cell_as_bbox) then
         WARN("MOLECULE.PLOT:plot_fragments ... resetting plot axes, plot widths from unit cell")
         ENSURE(allocated(self%crystal),"MOLECULE.PLOT:plot_fragments ... no crystal or unit cell for plot!")
         call set_bbox_from_unit_cell_(self%plot_grid,self%crystal%unit_cell)
      end if

      ! See plot_grid parameters
      call put_(self%plot_grid)

      ! Do the plot
      call do_fragment_plot_(self)
   end subroutine

   subroutine do_fragment_plot(self)
   ! Do the summed-fragmened  property density on ".plot_grid" using
   ! the molecules in .atom_group(:).mol.
      type(MOLECULE_TYPE), INOUT :: self

   ! seitz :: MAT3{REAL}@
   ! shift :: MAT{INT}@
      MAT(REAL,:,:), allocatable :: pt
   ! mx,dcm,coords :: MAT{REAL}@
   ! grid,frag :: VEC{REAL}@
      VEC(REAL,3) :: coa
   ! trans :: VEC{REAL}(3)
   ! parent,symop :: VEC{INT}@
   ! s,n_mols, p :: INT
      INT :: g
   ENSURE(allocated(self%plot_grid),"MOLECULE.PLOT:do_fragment_plot ... no grid")
   ENSURE(allocated(self%atom_group),"MOLECULE.PLOT:do_fragment_plot ... no atom_group info")
   ENSURE(allocated(self%crystal),"MOLECULE.PLOT:do_fragment_plot ... no crystal")
   ! name :: STR
   ! mol :: MOLECULE@
   ! same :: BIN

      ! Reset plot grid to group center?
      g = self%plot_grid%group
      if (g>0) then
         coa = center_of_atoms_(self%mol(g)%atom)
         call destroy_(self%plot_grid)
         call create_(self%plot_grid,self%mol(g)%atom)
         call set_center_(self%plot_grid,coa)
         call use_bbox_with_shape_axes_(self%plot_grid)
      end if

      ! Make the grid points
      call create_(pt,self%plot_grid%n_pt,3)
      call make_points_(self%plot_grid,pt)

      ! Make cluster (molecules) around plot grid
      call destroy_(self%cluster)
      call create_(self%cluster)
      call set_defaults_(self%cluster,self%crystal)
      call set_generation_method_(self%cluster,"fragment")  ! ?
      call set_defragment_(self%cluster,TRUE)
      call update_info_(self%cluster)
      call put_(self%cluster)
   ! .create_cluster

   ! .crystal.make_plot_info(pt,parent,symop,shift) 
   ! 
   ! DIE_IF(parent.deallocated,"no molecules inthe plot!")

   ! ! No. of molecules near the plot grid
   ! n_mols = parent.dim

   ! ! Cartesian rotation matrices
   ! seitz = .crystal.xyz_seitz_matrices

   ! ! Direct space unit cell matrix
   ! dcm = .crystal.unit_cell.direct_mx

   ! ! Make space for grid values and grid points
   ! grid.create(.plot_grid.n_pt)
   ! frag.create(.plot_grid.n_pt)
   ! coords.create(3,0)

   ! ! Initialise
   ! grid = ZERO

   ! do p = 1,n_mols
   ! 
   !    ! Which group does p belong to?
   !    g = .atom_group.group_for_atom(p)

   !    ! Clone molecule
   !    mol.create
   !    .set_molecule_from_atom_group(g,mol,FALSE)

   !    ! Set to be group name (for reading archive)
   !    mol.set_name(.atom_group(g).name)

   !    ! Set up plot - maybe read archive for MO's?
   !    mol.plot_grid => .plot_grid
   !    mol.set_up_for_fragment_plot(.plot_grid.plot_kind)
   ! 
   !    ! Rotate it
   !    s = symop(p)
   !    mol.rotate_by(seitz(1:3,1:3,s))

   !    ! Translate it
   !    trans = matmul(dcm,shift(:,p))
   !    mol.atom.translate_by(trans)

   !    ! CHeck here if the atoms are in the same space NOTE this may
   !    ! not work for disordered molecules
   !    same = FALSE
   !    mx.create(3,mol.atom.dim)
   !    mol.atom.put_coordinates_to(mx,positions_as_rows=FALSE)
   !    
   !    same = mx.has_any_column_from(coords)
   !    
   !    ! If it isn't the same, add it to the list
   !    if (NOT same) coords.append_column(mx(:,1))
   !    
   !    ! Clean up in each iteration
   !    mx.destroy
   !    
   !    ! Don't proceed if molecule is already plotted
   !    if (same) cycle

   !    ! Skip SCF for atoms without electrons
   !    ! Question: any plots which work with no electrons?
   !    if (mol.no_of_electrons < 1) cycle

   !    ! Do the plot for this fragment
   !    ! Use the parent .plot_grid
   !    mol.make_prop_values(frag,pt)
   !    nullify(mol.plot_grid)

   !    ! Add up this contribution
   !    grid = grid + frag

   !    ! Clean up mol
   !    mol.atom.nullify_ptr_part ! do not destroy these
   !    nullify(mol.basis)        ! do not destroy this
   !    nullify(mol.slaterbasis)  ! do not destroy this
   !    nullify(mol.coppensbasis) ! do not destroy this
   !    mol.destroy

   ! end

   ! ! Print out
   ! name = .plot_grid.plot_file_name(.n_a)
   ! .dump_plot_grid(grid,name)

      ! Clean up
   ! seitz.destroy
   ! grid.destroy
      call destroy_(pt)
   end subroutine

   subroutine test_plot_info(self)
   ! Test tlot the unit cell density
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: pt
      MAT(INT,:,:), allocatable :: shift
      VEC(INT,:), allocatable :: parent,symop
      INT :: i,p,g
   ENSURE(allocated(self%atom),"MOLECULE.PLOT:test_plot_info ... no atom info")
   ENSURE(allocated(self%crystal),"MOLECULE.PLOT:test_plot_info ... no crystal info")
   ENSURE(NOT self%crystal%cluster_info_made,"MOLECULE.PLOT:test_plot_info ... cluster info already made!")

   ! .crystal.destroy_fragment_data
   ! .crystal.make_fragment_data(.atom,assign_atom=TRUE)

   ! .crystal.set_generation_method("for_unit_cell_density") 
      call create_(pt,self%plot_grid%n_pt,3)
      call make_points_(self%plot_grid,pt)
      call put_(self%plot_grid)
      call set_atom_density_cutoff_(self%crystal,1.0d-3)
      call make_plot_info_(self%crystal,pt,parent,symop,shift)
      if (allocated(parent)) then
         do i = 1,size(parent)
            p = parent(i)
            g = group_for_atom_(self%atom_group,p)
            call text_(stdout,"The group for atom "//trim(to_str_(p))//" is "//trim(to_str_(g)))
            call show_(stdout,"parent     =",p)
            call show_(stdout,"symop      =",symop(i))
            call show_(stdout,"With shift =",shift(:,i))
         end do
      end if

   ! .crystal.put_cluster_info
   end subroutine


!  ===============
!  Isosurface plot
!  ===============

   subroutine isosurface_plot(self)
   ! Do one of the many kinds of isosurface plot calculations
      type(MOLECULE_TYPE), target, INOUT :: self
   ENSURE(allocated(self%isosurface),"MOLECULE.PLOT:isosurface_plot ... no isosurface")

      ! Set up
      call set_up_for_plot_(self,self%isosurface%property)

      ! Save self for selfless function calls
   ! .set_saved_self

      ! Do the plot
      call do_isosurface_plot_(self)
   end subroutine

   subroutine saved_isosurface_plot(self)
   ! Do an isosurface plot using the CURRENT .isosurface information
   ! *AND* using the .saved molecule, density functions, MOs, etc.
   ! Results placed in .saved.isosurface.
      type(MOLECULE_TYPE), target, INOUT :: self
   ENSURE(allocated(self%isosurface),"MOLECULE.PLOT:saved_isosurface_plot ... no isosurface")
   ENSURE(allocated(self%saved),"MOLECULE.PLOT:saved_isosurface_plot ... no saved molecule")

   ! keep :: ISOSURFACE@
   ! has_saved_iso :: BIN

      ! Keep any previous .saved.isosurface
   ! has_saved_iso = allocated(.saved.isosurface)
   ! if (has_saved_iso) then
   ! call move_alloc(.saved.isosurface,keep)
   ! end

      ! Set .saved.isosurface to be current .isosurface
      self%saved%isosurface = self%isosurface

      ! Set up
      call set_up_for_plot_(self%saved,self%isosurface%property)

      ! Set saved_self to be .saved molecule
   ! .saved.set_saved_self

      ! Do the plot -- current molecule isosurface
      call do_isosurface_plot_(self%saved)

      ! Put back the .saved.isosurface the way it was
   ! if (has_saved_iso) then
   ! call move_alloc(keep,.saved.isosurface)
   ! end
   end subroutine


!  ==============================
!  Plot property on an isosurface
!  ==============================

   subroutine plot_on_isosurface(self)
   ! Plot one of the many kinds of density functions on an existing
   ! isosurface - useful for mapping properties on a surface.
      type(MOLECULE_TYPE), target, INOUT :: self
   ENSURE(allocated(self%isosurface),"MOLECULE.PLOT:plot_on_isosurface ... no isosurface")
   ENSURE(self%isosurface%surface_property/=" ","MOLECULE.PLOT:plot_on_isosurface ... no surface_property given!")

      ! Set up
      call set_up_for_plot_(self,self%isosurface%surface_property)

      ! Save self for selfless function calls
   ! .set_saved_self

      ! Do the plot
      call do_plot_property_on_isosurface_(self)
   end subroutine

   subroutine saved_plot_on_isosurface(self)
   ! Plot a density function on an existing isosurface - for mapping
   ! properties on a surface - but using the CURRENT .isosurface *AND*
   ! using information from the .saved molecule atom list, density
   ! functions, MO's. Results placed in .saved.isosurface.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%isosurface),"MOLECULE.PLOT:saved_plot_on_isosurface ... no isosurface")
   ENSURE(self%isosurface%surface_property/=" ","MOLECULE.PLOT:saved_plot_on_isosurface ... no surface_property given!")
   ENSURE(allocated(self%saved),"MOLECULE.PLOT:saved_plot_on_isosurface ... no saved molecule")

   ! keep :: ISOSURFACE@
   ! has_saved_iso :: BIN

      ! Keep any previous .saved.isosurface
   ! has_saved_iso = allocated(.saved.isosurface)
   ! if (has_saved_iso) then
   ! call move_alloc(.saved.isosurface,keep)
   ! end

      ! Set .saved.isosurface to be current .isosurface
      self%saved%isosurface = self%isosurface

      ! Set up
      call set_up_for_plot_(self%saved,self%isosurface%surface_property)

   ! ! Set saved_self to be .saved molecule
   ! .saved.set_saved_self

      ! Do the plot -- current molecule isosurface
      call do_plot_property_on_isosurface_(self%saved)

      ! Put back the saved isosurface the way it was
   ! if (has_saved_iso) then
   ! call move_alloc(keep,.saved.isosurface)
   ! end
   end subroutine


!  ====================
!  Set-up info for plot
!  ====================

   subroutine set_up_for_plot(self,plot_kind)
   ! Do plot-specific initialization
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: plot_kind

      ! Make the becke grids & overlapping atoms
      call set_becke_grid_for_plot_(self,plot_kind)

      ! Set atom plot info for deformation densities
      call set_ANOs_for_plot_(self,plot_kind)

      ! Set interpolators for stockholder or promolecule plots
      call set_interpolators_for_plot_(self,plot_kind)

      ! Set up densities & integrals for plot
      call set_integrals_for_plot_(self,plot_kind)

      ! Hirshfeld weights for the rebuilt-atoms plots
      if (plot_kind=="angular_hirshfeld" OR plot_kind=="hirshfeld_amplitude") then
         call make_atom_partition_info_(self)
      end if

      ! Make sure group densities & MOs are there
      ! Warining: not for summed fragments plots
      call set_group_densities_for_plot_(self,plot_kind)

      ! Assign natural orbitals
      if (allocated(self%MOs)) then
         call assign_NOs_to_MOs_(self)
      end if
   end subroutine

   subroutine set_up_for_fragment_plot(self,plot_kind)
   ! Do plot-specific initialization
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: plot_kind

      ! Make the becke grids & overlapping atoms
      call set_becke_grid_for_plot_(self,plot_kind)

      ! Set atom plot info for deformation densities
      call set_ANOs_for_plot_(self,plot_kind)

      ! Set interpolators for stockholder or promolecule plots
      call set_interpolators_for_plot_(self,plot_kind)

      ! Set up densities & integrals for plot
      call set_integrals_for_plot_(self,plot_kind)

      ! Make sure group densities & MOs are there
      ! Warining: not for summed fragments plots
      call set_group_densities_for_plot_(self,plot_kind)

      ! Assign natural orbitals
      call read_SCF_archives_(self)  ! Note may cause issues, but probably not
   end subroutine

   subroutine set_interpolators_for_plot(self,plot_kind)
   ! Set the interpolator info for all the atoms if the "plot_kind" is such
   ! that interpolation tables could be beneficial. This should be called just
   ! prior to doing the plot to take advantage of speed-ups.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: plot_kind
      BIN :: do_interpolator
      do_interpolator = plot_kind=="promolecule_density" OR plot_kind(1:11)=="stockholder" OR plot_kind(1:11)=="deformation"
      if (NOT do_interpolator) return
      ENSURE(allocated(self%atom),"MOLECULE.PLOT:set_interpolators_for_plot ... no atom list")

      ! Get interpolators.
      select case (self%atom(1)%basis_kind)

      case ("slater")
         ENSURE(allocated(self%slaterbasis),"MOLECULE.PLOT:set_interpolators_for_plot ... no slater bases")
         call make_slater_interpolators_(self)

      case ("coppens")
         ENSURE(allocated(self%coppensbasis),"MOLECULE.PLOT:set_interpolators_for_plot ... no coppens bases")
         call make_coppens_interpolators_(self)

      case ("gaussian")
         ENSURE(allocated(self%basis),"MOLECULE.PLOT:set_interpolators_for_plot ... no gaussian bases")
         call make_ANOs_and_interpolators_(self)
      end select
   end subroutine

   subroutine set_ANOs_for_plot(self,plot_kind)
   ! Set the ANO info for all the atoms if the "plot_kind" is such
   ! that the ANO data must be made.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: plot_kind
      if (plot_kind(1:11)=="deformation" OR plot_kind(1:4)=="e_hf") then

         ! If we get here, the ANO information will be used
         call make_ANOs_and_interpolators_(self)
      else if (plot_kind(1:5)=="bond_") then
         call make_ANOs_and_interpolators_(self)

      ! ! If we get here, the spherically averaged atom information will be used
      ! .make_spherical_atoms
      else if (plot_kind=="hirshfeld_atom_grid") then

         ! should be in set_up becke grid?
         call set_up_becke_grid_(self)  ! NOT PURE
      end if
   end subroutine

   subroutine set_becke_grid_for_plot(self,plot_kind)
   ! Set up the Becke grid info only for those plots which actually
   ! need it.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: plot_kind
      BIN :: do_it
      do_it = FALSE
      select case (plot_kind)
      case ("adp                                 "); do_it = FALSE
      case ("adp_probability_grid                "); do_it = FALSE
      case ("anharmonic_adp_correction           "); do_it = FALSE
      case ("b_field                             "); do_it = FALSE
      case ("b_l_field                           "); do_it = FALSE
      case ("b_s_field                           "); do_it = FALSE
      case ("binding_function_wang_peng          "); do_it = FALSE
      case ("bond_electron_density               "); do_it = TRUE
      case ("bond_weight                         "); do_it = FALSE
      case ("crystal_error_map                   "); do_it = FALSE
      case ("crystal23_ed                        "); do_it = FALSE
      case ("crystal23_ed_v1                     "); do_it = FALSE
      case ("crystal23_electron_density          "); do_it = FALSE
      case ("current_density                     "); do_it = FALSE
      case ("deformation_bond_density            "); do_it = TRUE
      case ("deformation_density                 "); do_it = TRUE
      case ("deformation_dft_eff_potential       "); do_it = TRUE
      case ("deformation_dft_energy              "); do_it = TRUE
      case ("deformation_dft_xc_potential        "); do_it = TRUE
      case ("deformation_electric_potential      "); do_it = TRUE
      case ("deformation_g_kinetic_energy        "); do_it = TRUE
      case ("deformation_g_kirzhnits             "); do_it = TRUE
      case ("deformation_h_dft_energy            "); do_it = TRUE
      case ("deformation_h_from_virial           "); do_it = TRUE
      case ("deformation_h_kirzhnits             "); do_it = TRUE
      case ("deformation_k_kinetic_energy        "); do_it = TRUE
      case ("deformation_laplacian               "); do_it = TRUE
      case ("deformation_v_from_virial           "); do_it = TRUE
      case ("deformation_v_kirzhnits             "); do_it = TRUE
      case ("deformation_reduced_g_kinetic_energy"); do_it = TRUE
      case ("deformation_reduced_h_from_virial   "); do_it = TRUE
      case ("deformation_reduced_k_kinetic_energy"); do_it = TRUE
      case ("deformation_reduced_v_from_virial   "); do_it = TRUE
      case ("dft_eff_potential                   "); do_it = TRUE
      case ("dft_energy                          "); do_it = TRUE
      case ("dft_xc_potential                    "); do_it = TRUE
      case ("div_jp                              "); do_it = FALSE
      case ("electric_potential                  "); do_it = FALSE
      case ("multipole_electric_potential        "); do_it = FALSE
      case ("electric_polarisation_density       "); do_it = TRUE
      case ("electric_polarization_density       "); do_it = TRUE
      case ("electron_density                    "); do_it = FALSE
      case ("electron_density_v2                 "); do_it = FALSE
      case ("elf                                 "); do_it = FALSE
      case ("e_field_magnitude                   "); do_it = FALSE
      case ("e_hf_density_from_rho               "); do_it = FALSE
      case ("eli_d                               "); do_it = FALSE
   ! case("energy_density_from_rho             "); do_it = TRUE
      case ("fermi_mobility                      "); do_it = FALSE
      case ("g_kinetic_energy                    "); do_it = FALSE
      case ("g_kirzhnits                         "); do_it = FALSE
      case ("grad_rho_on_rho                     "); do_it = FALSE
      case ("group_product_electric_field_grid   "); do_it = FALSE
      case ("group_product_esp_grid              "); do_it = FALSE
      case ("h_dft_energy                        "); do_it = FALSE
      case ("h_kirzhnits                         "); do_it = FALSE
      case ("h_from_virial                       "); do_it = FALSE
      case ("hirshfeld_atom_grid                 "); do_it = FALSE
      case ("hirshfeld_density                   "); do_it = FALSE
      case ("hirshfeld_weight                    "); do_it = FALSE
      case ("interaction_density                 "); do_it = FALSE
      case ("j                                   "); do_it = FALSE
      case ("jd                                  "); do_it = FALSE
      case ("jp                                  "); do_it = FALSE
      case ("k_kinetic_energy                    "); do_it = FALSE
      case ("kramers_spin_density                "); do_it = FALSE
      case ("laplacian                           "); do_it = FALSE
      case ("local_ionisation_energy             "); do_it = FALSE
      case ("local_virial_ratio                  "); do_it = FALSE
      case ("lol                                 "); do_it = FALSE
      case ("negative_laplacian                  "); do_it = FALSE
      case ("noncollinear_spin_density           "); do_it = FALSE
      case ("normalized_g_from_virial            "); do_it = FALSE
      case ("normalized_h_from_virial            "); do_it = FALSE
      case ("normalized_v_from_virial            "); do_it = FALSE
   ! case("orbital_current_density             "); do_it = FALSE
      case ("orbital_density                     "); do_it = FALSE
      case ("orbital                             "); do_it = FALSE
      case ("oscillator_orbital                  "); do_it = FALSE
      case ("promolecule_density                 "); do_it = FALSE
      case ("product_electric_field_grid         "); do_it = FALSE
      case ("product_esp_grid                    "); do_it = FALSE
      case ("reduced_g_kinetic_energy            "); do_it = TRUE
      case ("reduced_h_from_virial               "); do_it = TRUE
      case ("reduced_k_kinetic_energy            "); do_it = TRUE
      case ("reduced_v_from_virial               "); do_it = TRUE
      case ("angular_hirshfeld                   "); do_it = FALSE
      case ("cell_map                            "); do_it = FALSE
      case ("hirshfeld_amplitude                 "); do_it = FALSE
      case ("residual_density                    "); do_it = FALSE
      case ("roby-gould_ano_density              "); do_it = FALSE
      case ("roby-gould_covalent_orbital         "); do_it = FALSE
      case ("roby-gould_ionic_orbital            "); do_it = FALSE
      case ("roby-gould_nao_density              "); do_it = FALSE
      case ("solenoidal_jp                       "); do_it = FALSE
   ! case("spin_current_density                "); do_it = FALSE
      case ("spin_density                        "); do_it = FALSE
      case ("spherical_atom_density              "); do_it = FALSE
      case ("stockholder_density                 "); do_it = FALSE
      case ("stockholder_weight                  "); do_it = FALSE
      case ("test_sphere_on_atom_1               "); do_it = FALSE
      case ("true_fermi_mobility                 "); do_it = FALSE
      case ("tsirelson_elf                       "); do_it = FALSE
      case ("v_kirzhnits                         "); do_it = FALSE
      case ("v_from_virial                       "); do_it = FALSE
      case default; allocate(tonto%known_keywords(102))
      tonto%known_keywords(1) = "adp                                 "
      tonto%known_keywords(2) = "adp_probability_grid                "
      tonto%known_keywords(3) = "anharmonic_adp_correction           "
      tonto%known_keywords(4) = "b_field                             "
      tonto%known_keywords(5) = "b_l_field                           "
      tonto%known_keywords(6) = "b_s_field                           "
      tonto%known_keywords(7) = "binding_function_wang_peng          "
      tonto%known_keywords(8) = "bond_electron_density               "
      tonto%known_keywords(9) = "bond_weight                         "
      tonto%known_keywords(10) = "crystal_error_map                   "
      tonto%known_keywords(11) = "crystal23_ed                        "
      tonto%known_keywords(12) = "crystal23_ed_v1                     "
      tonto%known_keywords(13) = "crystal23_electron_density          "
      tonto%known_keywords(14) = "current_density                     "
      tonto%known_keywords(15) = "deformation_bond_density            "
      tonto%known_keywords(16) = "deformation_density                 "
      tonto%known_keywords(17) = "deformation_dft_eff_potential       "
      tonto%known_keywords(18) = "deformation_dft_energy              "
      tonto%known_keywords(19) = "deformation_dft_xc_potential        "
      tonto%known_keywords(20) = "deformation_electric_potential      "
      tonto%known_keywords(21) = "deformation_g_kinetic_energy        "
      tonto%known_keywords(22) = "deformation_g_kirzhnits             "
      tonto%known_keywords(23) = "deformation_h_dft_energy            "
      tonto%known_keywords(24) = "deformation_h_from_virial           "
      tonto%known_keywords(25) = "deformation_h_kirzhnits             "
      tonto%known_keywords(26) = "deformation_k_kinetic_energy        "
      tonto%known_keywords(27) = "deformation_laplacian               "
      tonto%known_keywords(28) = "deformation_v_from_virial           "
      tonto%known_keywords(29) = "deformation_v_kirzhnits             "
      tonto%known_keywords(30) = "deformation_reduced_g_kinetic_energy"
      tonto%known_keywords(31) = "deformation_reduced_h_from_virial   "
      tonto%known_keywords(32) = "deformation_reduced_k_kinetic_energy"
      tonto%known_keywords(33) = "deformation_reduced_v_from_virial   "
      tonto%known_keywords(34) = "dft_eff_potential                   "
      tonto%known_keywords(35) = "dft_energy                          "
      tonto%known_keywords(36) = "dft_xc_potential                    "
      tonto%known_keywords(37) = "div_jp                              "
      tonto%known_keywords(38) = "electric_potential                  "
      tonto%known_keywords(39) = "multipole_electric_potential        "
      tonto%known_keywords(40) = "electric_polarisation_density       "
      tonto%known_keywords(41) = "electric_polarization_density       "
      tonto%known_keywords(42) = "electron_density                    "
      tonto%known_keywords(43) = "electron_density_v2                 "
      tonto%known_keywords(44) = "elf                                 "
      tonto%known_keywords(45) = "e_field_magnitude                   "
      tonto%known_keywords(46) = "e_hf_density_from_rho               "
      tonto%known_keywords(47) = "eli_d                               "
      tonto%known_keywords(48) = "fermi_mobility                      "
      tonto%known_keywords(49) = "g_kinetic_energy                    "
      tonto%known_keywords(50) = "g_kirzhnits                         "
      tonto%known_keywords(51) = "grad_rho_on_rho                     "
      tonto%known_keywords(52) = "group_product_electric_field_grid   "
      tonto%known_keywords(53) = "group_product_esp_grid              "
      tonto%known_keywords(54) = "h_dft_energy                        "
      tonto%known_keywords(55) = "h_kirzhnits                         "
      tonto%known_keywords(56) = "h_from_virial                       "
      tonto%known_keywords(57) = "hirshfeld_atom_grid                 "
      tonto%known_keywords(58) = "hirshfeld_density                   "
      tonto%known_keywords(59) = "hirshfeld_weight                    "
      tonto%known_keywords(60) = "interaction_density                 "
      tonto%known_keywords(61) = "j                                   "
      tonto%known_keywords(62) = "jd                                  "
      tonto%known_keywords(63) = "jp                                  "
      tonto%known_keywords(64) = "k_kinetic_energy                    "
      tonto%known_keywords(65) = "kramers_spin_density                "
      tonto%known_keywords(66) = "laplacian                           "
      tonto%known_keywords(67) = "local_ionisation_energy             "
      tonto%known_keywords(68) = "local_virial_ratio                  "
      tonto%known_keywords(69) = "lol                                 "
      tonto%known_keywords(70) = "negative_laplacian                  "
      tonto%known_keywords(71) = "noncollinear_spin_density           "
      tonto%known_keywords(72) = "normalized_g_from_virial            "
      tonto%known_keywords(73) = "normalized_h_from_virial            "
      tonto%known_keywords(74) = "normalized_v_from_virial            "
      tonto%known_keywords(75) = "orbital_density                     "
      tonto%known_keywords(76) = "orbital                             "
      tonto%known_keywords(77) = "oscillator_orbital                  "
      tonto%known_keywords(78) = "promolecule_density                 "
      tonto%known_keywords(79) = "product_electric_field_grid         "
      tonto%known_keywords(80) = "product_esp_grid                    "
      tonto%known_keywords(81) = "reduced_g_kinetic_energy            "
      tonto%known_keywords(82) = "reduced_h_from_virial               "
      tonto%known_keywords(83) = "reduced_k_kinetic_energy            "
      tonto%known_keywords(84) = "reduced_v_from_virial               "
      tonto%known_keywords(85) = "angular_hirshfeld                   "
      tonto%known_keywords(86) = "cell_map                            "
      tonto%known_keywords(87) = "hirshfeld_amplitude                 "
      tonto%known_keywords(88) = "residual_density                    "
      tonto%known_keywords(89) = "roby-gould_ano_density              "
      tonto%known_keywords(90) = "roby-gould_covalent_orbital         "
      tonto%known_keywords(91) = "roby-gould_ionic_orbital            "
      tonto%known_keywords(92) = "roby-gould_nao_density              "
      tonto%known_keywords(93) = "solenoidal_jp                       "
      tonto%known_keywords(94) = "spin_density                        "
      tonto%known_keywords(95) = "spherical_atom_density              "
      tonto%known_keywords(96) = "stockholder_density                 "
      tonto%known_keywords(97) = "stockholder_weight                  "
      tonto%known_keywords(98) = "test_sphere_on_atom_1               "
      tonto%known_keywords(99) = "true_fermi_mobility                 "
      tonto%known_keywords(100) = "tsirelson_elf                       "
      tonto%known_keywords(101) = "v_kirzhnits                         "
      tonto%known_keywords(102) = "v_from_virial                       "
      call unknown_(tonto,plot_kind,"MOLECULE.PLOT:set_becke_grid_for_plot")
      deallocate(tonto%known_keywords)
      end select
      if (do_it) call set_up_becke_grid_(self)
   end subroutine

   subroutine set_integrals_for_plot(self,plot_kind)
   ! Set the required integrals for the "plot_kind".
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: plot_kind
      select case (plot_kind)

      case ("true_fermi_mobility_grid")
         call make_kinetic_energy_mx_(self)
         call make_nuclear_attraction_mx_(self)

   ! case ("jp    ", "div_jp" )
   !    .make_r_density_mx
   !    .make_overlap_mx
   !    .make_restricted_complex_NOs

   ! case ("jp_irrotational", "jp_solendoidal")
   !    .make_r_density_mx
   !    ENSURE(.density_mx.is_allocated_with_genre("restricted_complex"),"no restrtd cmplx DM")

      case ("electric_potential","e_field_magnitude ","deformation_electric_potential")
         call make_r_density_mx_(self)
      end select
   end subroutine

   subroutine set_group_densities_for_plot(self,plot_kind)
   ! Set the fragment group densities for the plot.
   ! This does an SCF for each fragment - expensive!
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=*), IN :: plot_kind
      select case (plot_kind)

      case ("interaction_density")
         call fragment_SCF_(self)
         call assign_NOs_to_MOs_(self)
      end select
   end subroutine

end module
