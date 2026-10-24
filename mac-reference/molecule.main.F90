!==================================================================
!
! MOLECULE: An object repxresentation of a molecule.
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
!==================================================================
module MOLECULE_MAIN_MODULE

#  include "macros"

   include "molecule.main.use"

! The version, commit and build date (GIT_VERSION ...), remade on every build
#  include "build_stamp"

   implicit none

   include "molecule.main.int"


#  undef  ENSURE
#  define ENSURE(X,Y)       ENSURE0(X,Y)

contains

!  ================
!  Main controllers
!  ================

   subroutine run(self,basis_library_dir)
   ! Main molecule run-loop; assumes stdin and stdout have been defined.
      type(MOLECULE_TYPE), allocatable, INOUT :: self
      STR(len=*), optional, IN :: basis_library_dir

      ! Create & set up molecule
      call setup_(self,basis_library_dir)

      ! Banner
      call put_banner_

      ! Read & process keywords
      call read_keywords_(self)

      ! Finish timing
      call stop_timing_(std_time)
      call flush_(stdout)
      call text_(stdout,elapsed_time_message_(std_time,"job "//'"'//trim(self%name)//'"'))
      call text_(stdout,elapsed_cpu_time_message_(std_time,"job "//'"'//trim(self%name)//'"'))

      ! Warnings
      if (tonto%warnings_issued) then
         call flush_(stdout)
         call text_(stdout,"WARNINGS were issued in this job.")
         call text_(stdout,"Look above, or in the std_err file.")
      end if
   end subroutine

   subroutine setup(self,basis_library_dir)
   ! Set up a molecule ready to do something.
      type(MOLECULE_TYPE), allocatable, INOUT :: self
      STR(len=*), optional, IN :: basis_library_dir

      ! Start timer
      call start_timing_(std_time)

      ! Create (leaky)
      call create_(self)
      if (present(basis_library_dir)) then
         if (basis_library_dir/=" ") then
            call set_basis_directory_(self,basis_library_dir)
         end if
      end if

      ! Gaussian indices (leaky)
      call set_indices_(GAUSSIAN_DATA_L_MAX)

      ! Put errors under value
      call create_std_table_column_
   ! std_table_column.set_e_under_r(TRUE)
   end subroutine

   subroutine put_banner()
   ! Put to stdout the current TONTO banner
      call text_(stdout,"_______________________________________________________________")
      call text_(stdout,"                                                               ")
      call text_(stdout," T   O   N   T   O                                             ")
      call text_(stdout,"                                                               ")
      call text_(stdout," A free object-based system for computational chemistry        ")
      call text_(stdout,"                                                               ")
      call text_(stdout," Version: "//TONTO_VERSION//" v. "//GIT_VERSION)
      call text_(stdout," Platform: "//TONTO_PLATFORM)
      call text_(stdout," Build-date: "//TONTO_BUILD_DATE)
      call text_(stdout," Build-type: "//TONTO_BUILD_TYPE)
      call text_(stdout," Compiler: "//TONTO_COMPILER)
      call text_(stdout," LAPACK: "//TONTO_LAPACK)
      call text_(stdout,"                                                               ")
      call text_(stdout," For conditions of use, on-line documentation, and contributor ")
      call text_(stdout," and developer information see :-                              ")
      call text_(stdout,"                                                               ")
      call text_(stdout," https://"//"github."//"com/"//"dylan-jayatilaka/tonto         ")
      call text_(stdout,"                                                               ")
      call text_(stdout," Dylan Jayatilaka                                              ")
      call text_(stdout," Daniel Grimwood                                               ")
      call text_(stdout," Peter Spackman                                                ")
      call text_(stdout,"_______________________________________________________________")
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,start_time_message_(std_time))
      call flush_(stdout)
   end subroutine

   subroutine cleanup()
   ! Clean up all undeleted memory in all modules.
      type(ATOM_TYPE) :: atom
      type(BASIS_TYPE) :: basis
      type(SHELL_TYPE) :: shell
      type(COPPENSBASIS_TYPE) :: coppensbasis
      type(COPPENSORBITAL_TYPE) :: coppensorbital
      type(SLATERBASIS_TYPE) :: slaterbasis
      type(SLATERSHELL_TYPE) :: slatershell
      type(REFLECTION_TYPE) :: reflection

      ! Clean self
      ! Causes problems on gfortran.
      ! Leave the compiler to do it
   ! .destroy

      ! Gaussian indices
      call destroy_data_

      ! Clear all keys
      call clear_keys_(atom)
      call clear_keys_(basis)
      call clear_keys_(shell)
      call clear_keys_(coppensbasis)
      call clear_keys_(coppensorbital)
      call clear_keys_(slaterbasis)
      call clear_keys_(slatershell)
      call clear_keys_(reflection)
   end subroutine

   subroutine reset_molecule(self)
   ! Resets the mol data type and reruns the read routine
      type(MOLECULE_TYPE), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word
      call destroy_(self)
      call create_(self)

      ! First keyword must be "name"
      call read_(stdin,word)
      call to_lower_case_(word)
      ENSURE(word=="name=","MOLECULE.MAIN:reset_molecule ... first keyword must be name")
      call read_(stdin,self%name)
   end subroutine


!  =============
!  Read routines
!  =============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
   ! Allocatable version.
      type(MOLECULE_TYPE), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","MOLECULE.MAIN:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"MOLECULE.MAIN:read_keywords ... unexpected end of file while reading list keywords")
         call to_lower_case_(word)

         ! Append any lonely = sign
         if (NOT exhausted_(stdin%buffer)) then
            if (next_item_(stdin)=="=") then  ! assume this is an attached = sign
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

   recursive subroutine process_keyword(self,keyword)
   ! Process a keyword "word". Data is inputted from "stdin", unless
   ! "word" is a sequence of blank separated strings. In this case,
   ! the sequence is processed as if it were a separate file.
   ! NOTE : this routine reuires a MOLECULE pointer self variable
      type(MOLECULE_TYPE), allocatable, INOUT :: self
      STR(len=*), IN :: keyword
      select case (keyword(1:30))

      case ("}                             ")  ! exit case

      case ("assign_mos_to_nos             "); call assign_MOs_to_NOs_(self)
      case ("assign_nos_to_mos             "); call assign_NOs_to_MOs_(self)
      case ("atom_groups=                  "); call read_atom_groups_(self)
      case ("atom_group_charges=           "); call read_all_(stdin,self%atom_group_charges)
      case ("atom_partition_model=         "); call read_atom_partition_model_(self)
      case ("atomic_partition_model=       "); call read_atom_partition_model_(self)
      case ("atoms=                        "); call read_atoms_(self)
      case ("auxiliary_basis_name=         "); call read_auxiliary_basis_name_(self)

      case ("b_field=                      "); call read_B_field_(self)
      case ("basis_directory=              "); call read_basis_directory_(self)
      case ("basis_library=                "); call read_basis_directory_(self)
      case ("basis_name=                   "); call read_basis_name_(self)
      case ("basis_set=                    "); call read_basis_name_(self)
      case ("basis_sets=                   "); call read_basis_sets_(self)
   ! case ("batch_surfaces                "); .batch_HS
   ! case ("generate_wavefunctions        "); .generate_all_wavefunctions
      case ("becke_grid=                   "); call read_becke_grid_(self)

      case ("c23_xml_file_name=            "); call read_C23_XML_file_name_(self)
      case ("cp2k_periodic_file_name=      "); call read_C23_XML_file_name_(self)
      case ("canonicalise_occupied_mos     "); call canonicalize_occupied_MOs_(self)
      case ("canonicalize_occupied_mos     "); call canonicalize_occupied_MOs_(self)
      case ("canonicalise_omos             "); call canonicalize_occupied_MOs_(self)
      case ("change_axis_system_to         "); call change_axis_system_to_(self)
      case ("change_to_principal_axes      "); call change_to_principal_axes_(self%atom)
      case ("change_to_principal_top_axes  "); call change_to_principal_top_axes_(self%atom)
      case ("cell_map=                     "); call read_cell_map_(self)
      case ("charge=                       "); call read_charge_(self)
      case ("cif=                          "); call read_CIF_(self)
      case ("cleanup_scf                   "); call cleanup_scf_(self)
      case ("cluster=                      "); call read_cluster_(self)
      case ("coppensbasis_name=            "); call read_coppensbasis_name_(self)
      case ("create_cluster                "); call create_cluster_(self)
   ! case ("create_cluster_mol            "); .create_cluster_mol
   ! case ("create_g09_wfn_from_crystal   "); .generate_g09_input
   ! case ("create_hs_cluster             "); .create_HS_cluster
      case ("crystal=                      "); call read_crystal_(self)

      case ("defragment                    "); call defragment_(self)
      case ("phar_defragment               "); call phar_defragment_(self)  ! Kang added 2025.Mar.13
      case ("delete_plot_grid              "); call delete_plot_grid_(self)
      case ("delete_scf_archives           "); call delete_SCF_archives_(self)
      case ("delete_scf_integrals          "); call destroy_SCF_integrals_(self)
      case ("delta_lambda_plot             "); call delta_lambda_plot_(self)
   ! case ("deserialize=                  "); .read_deserialize_filename
      case ("destroy_atom_groups           "); call destroy_atom_group_(self)
      case ("destroy_cluster               "); call destroy_cluster_(self)
      case ("dispersion_coefficients=      "); call read_dispersion_coefficients_(self)
   ! case ("dump_xyz_files                "); .dump_xyz_files
   ! case ("dvpt_scf                      "); .MISC.DVPT_scf

      case ("e_field=                      "); call read_E_field_(self)

   ! case ("find_d_norm_contacts          "); .find_d_norm_contacts
      case ("fit_hirshfeld_atoms           "); call HA_fit_(self)
   ! case ("fit_hirshfeld_atom_kappas     "); .fit_Hirshfeld_atom_kappas
      case ("force_constants=              "); call read_force_constants_(self)
      case ("fraghar                       "); call fragHAR_refinement_(self)
      case ("fraghar_refinement            "); call fragHAR_refinement_(self)
      case ("fragment_plot                 "); call plot_fragments_(self)

      case ("gauge_origin=                 "); call read_gauge_origin_(self)
      case ("get_bader_basins              "); call get_Bader_basins_(self)
      case ("get_bader_regions             "); call get_Bader_regions_(self)
   ! case ("get_dedr_iotc                 "); .REL:get_dEdR_IOTC
   ! case ("get_g_overlap_nuc             "); .REL:get_g_overlap_nuc
   ! case ("get_gchf_jz                   "); .get_gchf_jz
   ! case ("get_gchf_kramers              "); .get_gchf_kramers
   ! case ("get_gchf_kramers_2            "); .get_gchf_kramers_2
   ! case ("get_gchf_na_nb                "); .get_gchf_na_nb
   ! case ("get_gchf_sx_sy_sz             "); .get_gchf_sx_sy_sz
   ! case ("get_gchf_s2                   "); .get_gchf_s2
   ! case ("get_gchf_s2_orig              "); .get_gchf_s2_orig
   ! case ("get_gchf_s2_contributions     "); .get_gchf_s2_contributions
   ! case ("get_uhf_kramers               "); .get_uhf_kramers
   ! case ("get_uhf_kramers_2             "); .get_uhf_kramers_2
   ! case ("get_uhf_sx_sy_sz              "); .get_uhf_sx_sy_sz
   ! case ("get_uhf_s2                    "); .get_uhf_s2
   ! case ("get_uhf_s2_orig               "); .get_uhf_s2_orig
   ! case ("get_x_negative_laplacian_grid "); .get_X_negative_laplacian_grid
   ! case ("get_x_laplacian_grid          "); .get_X_laplacian_grid
   ! case ("put_pce_x_structure_factors   "); .put_PCE_X_structure_factors
   ! case ("get_x_structure_factors_test  "); .get_X_structure_factors_test

      case ("ha_fit                        "); call HA_fit_(self)
      case ("ha_refinement                 "); call HAR_refinement_(self)
      case ("har_refinement                "); call HAR_refinement_(self)
      case ("har                           "); call HAR_refinement_(self)

      case ("iam_refinement                "); call IAM_refinement_(self)
      case ("integrate_ed_numerically      "); call integrate_ED_numerically_(self)
   ! case ("integrate_ku_spin_numerically "); .integrate_ku_spin_numerically
   ! case ("integrate_nc_spin_numerically "); .integrate_nc_spin_numerically
   ! case ("integrate_spin_numerically    "); .integrate_spin_numerically
      case ("interpolator=                 "); call read_interpolator_(self)
      case ("isosurface=                   "); call read_isosurface_(self)
      case ("isosurface_plot               "); call isosurface_plot_(self)

      case ("keyword_echo_on               "); call set_keyword_echo_on_

      case ("localise_mos=                 "); call localize_MOs_(self)
      case ("localize_mos=                 "); call localize_MOs_(self)
      case ("low_verbosity_on              "); call set_low_verbosity_on_

      case ("make_anos                     "); call make_ANOs_(self,output=TRUE)
      case ("make_ano_data                 "); call make_ANOs_and_interpolators_(self)  ! deprecated
      case ("make_anos_and_interpolators   "); call make_ANOs_and_interpolators_(self)
      case ("make_ao_density_matrix        "); call make_r_density_mx_(self)
      case ("make_ao_density_mx            "); call make_r_density_mx_(self)
      case ("make_atom_group_density_mx    "); call make_atom_group_density_mx_(self)
      case ("make_ao_sz_density_matrix     "); call make_r_Sz_density_mx_(self)
      case ("make_ba_info                  "); call make_BA_info_(self)
   ! case ("make_core_structure_factors   "); .make_core_structure_factors
      case ("make_cluster_charges          "); call make_cluster_charges_(self)
   ! case ("make_dkh_density_norm         "); .REL:make_DKH_density_norm
      case ("make_f_predicted              "); call make_F_predicted_(self)
   ! case ("make_fermi_contact_terms      "); .make_fermi_contact_terms
   ! case ("make_file_delta_density_grid  "); .make_file_delta_density_grid
   ! case ("make_file_delta_spin_grid     "); .make_file_delta_spin_grid
      case ("make_fock_guess               "); call make_Fock_mx_guess_MOs_(self)
      case ("make_fock_matrix              "); call make_scf_Fock_mx_(self)
      case ("make_fock_mx                  "); call make_scf_Fock_mx_(self)
      case ("make_ha_info                  "); call make_HA_info_(self)
      case ("make_ha_inputs                "); call make_HA_info_(self)
      case ("make_hirshfeld_inputs         "); call make_HA_info_(self)
      case ("make_iterative_has            "); call make_iterative_HAs_(self)
      case ("make_monomer_mos              "); call make_monomer_MOs_(self)
   ! case ("make_n_structure_factors      "); .make_N_SFs
      case ("make_natural_orbitals         "); call make_NOs_(self)
      case ("make_nos                      "); call make_NOs_(self)
      case ("make_overlap_matrix           "); call make_overlap_mx_(self)
      case ("make_overlap_mx               "); call make_overlap_mx_(self)
      case ("make_progroup_density_matrix  "); call make_progroup_density_mx_(self)
      case ("make_progroup_density_mx      "); call make_progroup_density_mx_(self)
      case ("make_promolecule_density      "); call make_promolecule_density_mx_(self)
      case ("make_promol_density_matrix    "); call make_promolecule_density_mx_(self)
      case ("make_promol_density_mx        "); call make_promolecule_density_mx_(self)
      case ("make_promol_mos               "); call make_promol_MOs_(self)
      case ("make_r_density_mx             "); call make_r_density_mx_(self)
      case ("make_r_sz_density_mx          "); call make_r_Sz_density_mx_(self)
      case ("make_sa_info                  "); call make_SA_info_(self)
      case ("make_salvador_info            "); call make_SA_info_(self)
      case ("make_salvador_radii           "); call make_Salvador_radii_(self)
      case ("make_salvador_radii_lmin      "); call make_Salvador_radii_lmin_(self)
      case ("make_ss_info                  "); call make_SS_info_(self)
      case ("make_scf_density_matrix       "); call make_SCF_density_mx_(self,damp=FALSE)
      case ("make_scf_density_mx           "); call make_SCF_density_mx_(self,damp=FALSE)
      case ("make_spherically_averaged_bdas"); call make_averaged_BDAs_(self)
      case ("make_spherically_averaged_has "); call make_spherically_averaged_HAs_(self)
      case ("make_structure_factors        "); call make_X_SFs_(self,output=TRUE)
   ! case ("make_weak_force_energy_shift  "); .make_weak_force_energy_shift
   ! case ("make_symortho_density_matrix  "); .make_group_density_matrix
      case ("make_vanfleteren_atoms        "); call make_Vanfleteren_atoms_(self)
      case ("make_x_structure_factors      "); call make_X_SFs_(self,output=TRUE)
      case ("move_origin                   "); call move_origin_(self)
      case ("move_origin_to_center_of_atoms"); call move_origin_to_center_of_atoms_(self)
      case ("move_origin_to_center_of_mass "); call move_origin_to_center_of_mass_(self)
      case ("mp2                           "); call MP2_(self)
      case ("multiplicity=                 "); call read_spin_multiplicity_(self)  ! deprecated

      case ("name=                         "); call read_name_(self)
   ! case ("no_scf                        "); .NOMO:no_scf
      case ("normal_mode_analysis          "); call normal_mode_analysis_(self)
      case ("make_internal_adps            "); call make_internal_ADPs_(self)
      case ("make_fd_hessian               "); call make_FD_hessian_(self)
      case ("fd_hessian_step=              "); call read_(stdin,self%FD_hessian_step)
      case ("internal_adp_temperature=     "); call read_(stdin,self%internal_ADP_temperature)
      case ("soft_mode_cutoff=             "); call read_(stdin,self%soft_mode_cutoff)
      case ("frequency_scale_factor=       "); call read_(stdin,self%frequency_scale_factor)
      case ("nucleus_model=                "); call read_nucleus_model_(self)

   ! case ("optimise_geometry             "); .optimise_geometry
   ! case ("optimise_orbitals             "); .optimize_orbitals
   ! case ("optimize_orbitals             "); .optimize_orbitals
      case ("output_style_options=         "); call read_output_style_options_

      case ("phi_step_size                 "); call read_phi_step_size_(self)
      case ("plot                          "); call plot_(self)
      case ("plot_fragments                "); call plot_fragments_(self)
      case ("plots                         "); call plots_(self)
      case ("plot_grid=                    "); call read_plot_grid_(self)
      case ("plot_on_isosurface            "); call plot_on_isosurface_(self)
      case ("plot_uc_density               "); call plot_uc_density_(self)
      case ("pointgroup=                   "); call read_pointgroup_(self)
      case ("process_cif_and_c23_xml       "); call process_CIF_AND_C23_XML_(self)
      case ("process_cif_and_cp2k_data     "); call process_CIF_AND_C23_XML_(self)
      case ("put                           "); call put_(self)
      case ("put_3_center_overlaps_check   "); call put_3_center_overlaps_check_(self)
      case ("put_1e_properties             "); call put_1e_properties_(self)
      case ("put_1e_properties_for_groups  "); call put_1e_properties_for_groups_(self)
      case ("put_atom_coordinates          "); call put_atom_coordinates_(self)
      case ("put_adps                      "); call put_ADPs_(self)
      case ("put_anos                      "); call put_ANOs_(self)
      case ("put_ao_energy_partition       "); call put_AO_energy_partition_(self)
      case ("put_atom_groups               "); call put_atom_groups_(self)
      case ("put_atomic_form_factors       "); call put_atomic_form_factors_(self)
      case ("put_ba_moments                "); call put_BA_moments_(self)
      case ("put_bda_charges               "); call put_BDA_charges_(self)
      case ("put_becke_atom_moments        "); call put_BA_moments_(self)
      case ("put_cosx_shell_errors         "); call put_COSX_shell_errors_(self)
      case ("put_grid_shell_errors         "); call put_grid_shell_errors_(self)
      case ("put_xc_batch_statistics       "); call put_XC_batch_statistics_(self)
   ! case ("put_becke_grid_atom_info      "); .becke_grid.put_atom_info
      case ("put_bond_length_with_error    "); call put_bond_length_with_error_(self)
      case ("put_bond_angle_with_error     "); call put_bond_angle_with_error_(self)
      case ("put_torsion_angle_with_error  "); call put_torsion_angle_with_error_(self)
      case ("put_ccdc_bonded_neighbours    "); call put_CCDC_bonded_neighbours_(self)
      case ("put_cif                       "); call put_CIFs_(self)
   ! case ("put_coppens_interpolators     "); .put_coppens_interpolators
      case ("put_coulomb_energy            "); call put_Coulomb_energy_(self)
      case ("put_crystal                   "); call put_crystal_(self)
      case ("put_density_matrix            "); call put_density_mx_(self)
      case ("put_density_mx                "); call put_density_mx_(self)
   ! case ("put_diff_coppens_slater_ints  "); .put_diff_coppens_slater_ints
   ! case ("put_diff_slater_interpolators "); .put_diff_slater_interpolators
      case ("put_dnc_displaced_geometries  "); call put_dnc_displaced_geometries_(self)
      case ("put_ed_between_atoms          "); call put_ED_between_atoms_(self)
!     case ("put_energies_breakdown=       "); .put_energies_breakdown 
   ! case ("put_excel_crystal             "); .put_excel_crystal
      case ("put_fitting_plots             "); call put_fitting_plots_(self%crystal,self%name)  ! <--
      case ("put_fock_matrix               "); call put_Fock_mx_(self)
      case ("put_fock_mx                   "); call put_Fock_mx_(self)
   ! case ("put_g_tensor_information      "); .put_g_tensor_information
      case ("put_group_12_energies         "); call put_group_12_energies_(self)
      case ("put_group_12_multipole_energy "); call put_group_12_multipole_energies_(self)
      case ("put_group_12_spackman1986_ener"); call put_group_12_Spackman1986_energies_(self)
      case ("put_group_12_grimme2006_energy"); call put_group_12_Grimme2006_energy_(self)
      case ("put_group_12_polarization_ener"); call put_group_12_polarization_energy_(self)
      case ("put_grown_cif                 "); call put_grown_CIF_(self)
      case ("put_ha_moments                "); call put_HA_moments_(self)
   ! case ("put_hirshfeld_g_and_k_energy  "); .put_Hirshfeld_G_and_K_energy
   ! case ("put_hirshfeld_polarisabilities"); .put_H_polarisabilities
   ! case ("put_hirshfeld_polarizabilities"); .put_H_polarisabilities
      case ("put_hirshfeld_moments         "); call put_HA_moments_(self)
      case ("put_hirshfeld_test            "); call put_Hirshfeld_test_(self)
      case ("put_ri_ff_check               "); call put_RI_FF_check_(self)
      case ("fit_sph_atom_ffs              "); call fit_sph_atom_FFs_(self)
   ! case ("put_interpolator_list         "); .put_interpolator_list
   ! case ("put_km_1e_properties          "); .put_1e_properties_with_KM
      case ("put_mayer_bond_indices        "); call put_Mayer_bond_indices_(self)
      case ("put_cell_map                  "); call put_cell_map_(self)
      case ("put_cell_map_radial_functions"); call put_cell_map_radial_functions_(self)
      case ("put_angular_hirshfeld_atoms   "); call put_angular_Hirshfeld_atoms_(self,amplitude=FALSE)
      case ("put_ha_populations            "); call put_angular_Hirshfeld_atoms_(self,amplitude=TRUE)
      case ("put_minmax_residual_density   "); call get_minmax_residual_density_(self,output=TRUE)
      case ("put_molecular_orbitals        "); call put_MOs_(self)
      case ("put_mo_energies               "); call put_MO_energies_(self)
      case ("put_mo_energy_partition       "); call put_MO_energy_partition_(self)
      case ("put_mos                       "); call put_MOs_(self)
      case ("put_mos_and_energies          "); call put_MOs_and_energies_(self)
      case ("put_mulliken_populations      "); call put_Mulliken_populations_(self)
   ! case ("put_mulliken_ku_populations   "); .put_mulliken_ku_populations
   ! case ("put_mulliken_sf_populations   "); .put_mulliken_sf_populations
      case ("put_natural_orbitals          "); call put_NOs_(self)
      case ("put_nbo_file_47               "); call put_NBO_file_47_(self)
   ! case ("put_nka_cartesian             "); .put_NKA_cartesian
   ! case ("put_nka_crystal               "); .put_NKA_crystal
   ! case ("put_n_structure_factors       "); .put_N_SFs
      case ("put_normal_modes              "); call put_normal_modes_(self)
      case ("put_internal_adps             "); call put_internal_ADPs_(self)
      case ("put_nos                       "); call put_NOs_(self)
   ! case ("put_oscillator_overlaps       "); .put_oscillator_overlaps
      case ("put_overlap_matrix            "); call put_overlap_mx_(self)
      case ("put_overlap_mx                "); call put_overlap_mx_(self)
      case ("put_overlap_mx_evals          "); call put_overlap_mx_evals_(self)
      case ("put_overlapping_atoms         "); call put_overlapping_atoms_for_atom_(self)
      case ("put_pdb                       "); call put_pdb_(self)
      case ("put_plot_grid                 "); call put_plot_grid_(self)
   ! case ("put_pnd_sf                    "); .put_PND_sf
      case ("put_pointgroup                "); call put_pointgroup_(self)
      case ("put_principal_axes            "); call put_principal_axes_(self)
      case ("put_reflection_data           "); call put_reflection_data_(self%crystal)
      case ("put_ri_metric_timings         "); call put_RI_metric_timings_(self)
      case ("put_roby_mmo_energy_partition "); call put_Roby_mmo_energy_partition_(self)
      case ("put_roby_smo_energy_partition "); call put_Roby_smo_energy_partition_(self)
      case ("put_sa_cell_fn_between_atoms  "); call put_SA_cell_fn_between_atoms_(self)
      case ("put_sa_ed_between_atoms       "); call put_SA_ED_between_atoms_(self)
      case ("put_sa_moments                "); call put_SA_moments_(self)
      case ("put_salvador_moments          "); call put_SA_moments_(self)
      case ("put_salvador_radii            "); call put_Salvador_radii_(self)
      case ("put_ss_moments                "); call put_SS_moments_(self)
      case ("put_sao_energy_partition      "); call put_SAO_energy_partition_(self)
      case ("put_scf_energy_in_mo_pairs    "); call put_SCF_energy_in_MO_pairs_(self)
      case ("put_scf_energy_as_mo_parts    "); call put_SCF_energy_as_MO_parts_(self)
      case ("put_scf_atomic_polarisabilitie"); call put_SCF_atomic_polarisability_(self)
      case ("put_scf_atomic_polarizabilitie"); call put_SCF_atomic_polarisability_(self)
      case ("put_scf_dipole_hyperpolarisabi"); call put_SCF_dipole_hyperpolarisa_(self)
      case ("put_scf_dipole_hyperpolarizabi"); call put_SCF_dipole_hyperpolarisa_(self)
      case ("put_scf_dipole_polarisability "); call put_SCF_dipole_polarisability_(self)
      case ("put_scf_dipole_polarizability "); call put_SCF_dipole_polarisability_(self)
      case ("put_shelx_fcf5                "); call put_shelx_fcf5_(self)
      case ("put_shelx_fcf6                "); call put_shelx_fcf6_(self)
      case ("put_shelx_hkl3                "); call put_shelx_hkl3_(self)
      case ("put_shelx_hkl4                "); call put_shelx_hkl4_(self)
   ! case ("put_slater_interpolators      "); .put_slater_interpolators
      case ("put_sos_dipole_polarisability "); call put_SOS_dipole_polarisability_(self)
      case ("put_spherical_sa_ed_from_atom "); call put_spherical_SA_ED_from_atom_(self)
      case ("put_sylvain_csizmadia_tensors "); call put_Sylvain_Csizmadia_tensors_(self)
      case ("put_structure_factor_stats    "); call put_X_SF_statistics_(self)
      case ("put_structure_factors         "); call put_X_SFs_(self)
      case ("put_submolecule1              "); call put_submolecule_(self,1)
      case ("put_submolecule2              "); call put_submolecule_(self,2)
      case ("put_uc_dipole_polarisability  "); call put_UC_dipole_polarisability_(self)
      case ("put_uc_dipole_polarizability  "); call put_UC_dipole_polarisability_(self)
      case ("put_uc_dipole_hyperpolarisabil"); call put_UC_dipole_hyperpolar_(self)
      case ("put_uc_dipole_hyperpolarizabil"); call put_UC_dipole_hyperpolar_(self)
      case ("put_uc_chi2_old               "); call put_UC_chi2_(self)
      case ("put_uc_refractive_index       "); call put_UC_refractive_index_(self)
      case ("put_vrml                      "); call put_vrml_(self)
      case ("put_xyz_file                  "); call put_xyz_file_(self%atom)

      case ("read_archive                  "); call read_archive_(self)
      case ("read_archive_and_normalise    "); call read_archive_and_normalise_(self)
      case ("read_archive_and_normalize    "); call read_archive_and_normalise_(self)
      case ("read_ascii_archive            "); call read_ascii_archive_(self)
      case ("read_fchk_file                "); call read_g09_FChk_file_(self)
      case ("read_g09_fchk_file            "); call read_g09_FChk_file_(self)
      case ("read_gaussian_fchk_file       "); call read_g09_FChk_file_(self)
      case ("read_molden_file              "); call read_molden_file_(self)
      case ("read_molden_mos               "); call read_molden_file_(self)
      case ("read_molden_nos               "); call read_molden_file_(self)
      case ("read_tonto_fchk_file          "); call read_tonto_FChk_file_(self)
      case ("redirect                      "); call redirect_(self)
      case ("redirect_stdout               "); call redirect_stdout_(self)
      case ("refine_hirshfeld_atoms        "); call HAR_refinement_(self)
   ! case ("refine_tanaka_atoms           "); .TAR_refinement
      case ("reset_molecule                "); call reset_molecule_(self)
      case ("revert                        "); call revert_(self)
      case ("revert_stdout                 "); call revert_stdout_(self)
      case ("rmp2                          "); call make_chem_mp2_(self)
      case ("roby_analysis                 "); call roby_analysis_(self)
      case ("robydata=                     "); call read_robydata_(self)
      case ("rotate                        "); call read_rotate_(self)
      case ("rotate_group                  "); call read_rotate_group_(self)
      case ("rotate_with_matrix            "); call read_rotate_mx_(self)
      case ("rotate_with_mx                "); call read_rotate_mx_(self)

      case ("saved_basis_name=             "); call read_saved_basis_name_(self)
      case ("saved_coppensbasis_name=      "); call read_saved_coppensbasis_name_(self)
      case ("saved_isosurface_plot         "); call saved_isosurface_plot_(self)
      case ("saved_slaterbasis_name=       "); call read_saved_slaterbasis_name_(self)
      case ("saved_plot_on_isosurface      "); call saved_plot_on_isosurface_(self)
      case ("scf                           "); call SCF_(self)
      case ("scf_data=                     "); call read_SCF_data_(self)
      case ("scfdata=                      "); call read_SCF_data_(self)
   ! case ("serialize=                    "); .read_serialize_filename
      case ("set_isotropic_adps            "); call set_isotropic_ADPs_(self)
      case ("set_isotropic_h_adps          "); call set_isotropic_H_ADPs_(self)
      case ("shorten_vdw_bond              "); call shorten_shortest_vdw_bond_(self%atom)
   ! case ("simulate_new_f_exp            "); .crystal.simulate_new_F_exp ! use F_sigma_noise
      case ("slaterbasis_name=             "); call read_slaterbasis_name_(self)
      case ("spin_contamination            "); call spin_contamination_(self)
      case ("spin_multiplicity=            "); call read_spin_multiplicity_(self)

      case ("table_column=                 "); call read_table_column_

      case ("unsave                        "); call unsave_(self)
      case ("use_crystal23_order=          "); call read_use_CRYSTAL23_order_(self)
      case ("use_interpolators=            "); call read_use_interpolators_(self)
      case ("use_spherical_basis=          "); call read_use_spherical_basis_(self)

      case ("write_aim2000_wfn_file        "); call put_aim2000_wfn_file_(self)
      case ("write_florian_wfn_file        "); call put_florian_wfn_file_(self)
      case ("write_archive                 "); call put_archive_(self)
      case ("write_ascii_archive           "); call put_ascii_archive_(self)
      case ("write_tonto_fchk_mo           "); call put_tonto_FChk_MO_(self)
      case ("write_tonto_fchk_dm           "); call put_tonto_FChk_DM_(self)
      case ("write_morphy98_wfn_file       "); call put_morphy98_wfn_file_(self)
      case ("write_xyz_file                "); call put_xyz_file_(self)
      case ("write_fragment_xyz_file       "); call put_xyz_frag_file_(self)
      case ("write_xtal23_xyz_file         "); call put_xtal23_xyz_file_(self)

   ! case ("zero_off_atom_density         "); .zero_off_atom_density
   ! case ("zero_overlap_density          "); .zero_overlap_density

      ! Crystal Explorer commands ...

      case ("cx_file_name=                 "); call read_CX_file_name_(self)
      case ("cx_isosurface=                "); call read_CX_isosurface_(self)
      case ("cx_surface=                   "); call read_CX_isosurface_(self)
      case ("cx_uses_angstrom=             "); call read_CX_uses_angstrom_(self)

      case ("process_cif                   "); call process_CIF_(self)
      case ("process_cif_cell_and_symmetry "); call process_CIF_cell_and_symmetry_(self)
      case ("process_cif_for_cx            "); call process_CIF_for_CX_(self)
      case ("put_cx_data                   "); call put_CX_data_(self)

      ! Invarioms commands ...

      case ("put_invariom_label            "); call put_invariom_label_(self)
      case ("put_invariom_labels           "); call put_invariom_labels_(self)
      case ("put_next_neighbour_handedness "); call put_next_neighbour_handedness_(self)
      case ("put_smiles_invariom_labels    "); call put_SMILES_invariom_labels_(self)

      case ("td_data=                      "); call read_TD_data_(self)
      case ("td_excited_states             "); call TD_excited_states_(self)

      ! Test commands ...

   ! case ("spherically_averaged_sf       "); .spherically_averaged_sf
   ! case ("test_dump_file                "); .test_dump_file
   ! case ("test_plot_info                "); .test_plot_info
      case ("test_vdw_bonding              "); call test_vdw_bonding_(self)
      case ("test_vovo                     "); call test_VOVO_(self)
      case ("test_mgs_qr                   "); call test_MGS_QR_
      case ("test_davidson_cps_6           "); call test_Davidson_CPS_6_

      case default; allocate(tonto%known_keywords(305))
      tonto%known_keywords(1) = "}                             "
      tonto%known_keywords(2) = "assign_mos_to_nos             "
      tonto%known_keywords(3) = "assign_nos_to_mos             "
      tonto%known_keywords(4) = "atom_groups=                  "
      tonto%known_keywords(5) = "atom_group_charges=           "
      tonto%known_keywords(6) = "atom_partition_model=         "
      tonto%known_keywords(7) = "atomic_partition_model=       "
      tonto%known_keywords(8) = "atoms=                        "
      tonto%known_keywords(9) = "auxiliary_basis_name=         "
      tonto%known_keywords(10) = "b_field=                      "
      tonto%known_keywords(11) = "basis_directory=              "
      tonto%known_keywords(12) = "basis_library=                "
      tonto%known_keywords(13) = "basis_name=                   "
      tonto%known_keywords(14) = "basis_set=                    "
      tonto%known_keywords(15) = "basis_sets=                   "
      tonto%known_keywords(16) = "becke_grid=                   "
      tonto%known_keywords(17) = "c23_xml_file_name=            "
      tonto%known_keywords(18) = "cp2k_periodic_file_name=      "
      tonto%known_keywords(19) = "canonicalise_occupied_mos     "
      tonto%known_keywords(20) = "canonicalize_occupied_mos     "
      tonto%known_keywords(21) = "canonicalise_omos             "
      tonto%known_keywords(22) = "change_axis_system_to         "
      tonto%known_keywords(23) = "change_to_principal_axes      "
      tonto%known_keywords(24) = "change_to_principal_top_axes  "
      tonto%known_keywords(25) = "cell_map=                     "
      tonto%known_keywords(26) = "charge=                       "
      tonto%known_keywords(27) = "cif=                          "
      tonto%known_keywords(28) = "cleanup_scf                   "
      tonto%known_keywords(29) = "cluster=                      "
      tonto%known_keywords(30) = "coppensbasis_name=            "
      tonto%known_keywords(31) = "create_cluster                "
      tonto%known_keywords(32) = "crystal=                      "
      tonto%known_keywords(33) = "defragment                    "
      tonto%known_keywords(34) = "phar_defragment               "
      tonto%known_keywords(35) = "delete_plot_grid              "
      tonto%known_keywords(36) = "delete_scf_archives           "
      tonto%known_keywords(37) = "delete_scf_integrals          "
      tonto%known_keywords(38) = "delta_lambda_plot             "
      tonto%known_keywords(39) = "destroy_atom_groups           "
      tonto%known_keywords(40) = "destroy_cluster               "
      tonto%known_keywords(41) = "dispersion_coefficients=      "
      tonto%known_keywords(42) = "e_field=                      "
      tonto%known_keywords(43) = "fit_hirshfeld_atoms           "
      tonto%known_keywords(44) = "force_constants=              "
      tonto%known_keywords(45) = "fraghar                       "
      tonto%known_keywords(46) = "fraghar_refinement            "
      tonto%known_keywords(47) = "fragment_plot                 "
      tonto%known_keywords(48) = "gauge_origin=                 "
      tonto%known_keywords(49) = "get_bader_basins              "
      tonto%known_keywords(50) = "get_bader_regions             "
      tonto%known_keywords(51) = "ha_fit                        "
      tonto%known_keywords(52) = "ha_refinement                 "
      tonto%known_keywords(53) = "har_refinement                "
      tonto%known_keywords(54) = "har                           "
      tonto%known_keywords(55) = "iam_refinement                "
      tonto%known_keywords(56) = "integrate_ed_numerically      "
      tonto%known_keywords(57) = "interpolator=                 "
      tonto%known_keywords(58) = "isosurface=                   "
      tonto%known_keywords(59) = "isosurface_plot               "
      tonto%known_keywords(60) = "keyword_echo_on               "
      tonto%known_keywords(61) = "localise_mos=                 "
      tonto%known_keywords(62) = "localize_mos=                 "
      tonto%known_keywords(63) = "low_verbosity_on              "
      tonto%known_keywords(64) = "make_anos                     "
      tonto%known_keywords(65) = "make_ano_data                 "
      tonto%known_keywords(66) = "make_anos_and_interpolators   "
      tonto%known_keywords(67) = "make_ao_density_matrix        "
      tonto%known_keywords(68) = "make_ao_density_mx            "
      tonto%known_keywords(69) = "make_atom_group_density_mx    "
      tonto%known_keywords(70) = "make_ao_sz_density_matrix     "
      tonto%known_keywords(71) = "make_ba_info                  "
      tonto%known_keywords(72) = "make_cluster_charges          "
      tonto%known_keywords(73) = "make_f_predicted              "
      tonto%known_keywords(74) = "make_fock_guess               "
      tonto%known_keywords(75) = "make_fock_matrix              "
      tonto%known_keywords(76) = "make_fock_mx                  "
      tonto%known_keywords(77) = "make_ha_info                  "
      tonto%known_keywords(78) = "make_ha_inputs                "
      tonto%known_keywords(79) = "make_hirshfeld_inputs         "
      tonto%known_keywords(80) = "make_iterative_has            "
      tonto%known_keywords(81) = "make_monomer_mos              "
      tonto%known_keywords(82) = "make_natural_orbitals         "
      tonto%known_keywords(83) = "make_nos                      "
      tonto%known_keywords(84) = "make_overlap_matrix           "
      tonto%known_keywords(85) = "make_overlap_mx               "
      tonto%known_keywords(86) = "make_progroup_density_matrix  "
      tonto%known_keywords(87) = "make_progroup_density_mx      "
      tonto%known_keywords(88) = "make_promolecule_density      "
      tonto%known_keywords(89) = "make_promol_density_matrix    "
      tonto%known_keywords(90) = "make_promol_density_mx        "
      tonto%known_keywords(91) = "make_promol_mos               "
      tonto%known_keywords(92) = "make_r_density_mx             "
      tonto%known_keywords(93) = "make_r_sz_density_mx          "
      tonto%known_keywords(94) = "make_sa_info                  "
      tonto%known_keywords(95) = "make_salvador_info            "
      tonto%known_keywords(96) = "make_salvador_radii           "
      tonto%known_keywords(97) = "make_salvador_radii_lmin      "
      tonto%known_keywords(98) = "make_ss_info                  "
      tonto%known_keywords(99) = "make_scf_density_matrix       "
      tonto%known_keywords(100) = "make_scf_density_mx           "
      tonto%known_keywords(101) = "make_spherically_averaged_bdas"
      tonto%known_keywords(102) = "make_spherically_averaged_has "
      tonto%known_keywords(103) = "make_structure_factors        "
      tonto%known_keywords(104) = "make_vanfleteren_atoms        "
      tonto%known_keywords(105) = "make_x_structure_factors      "
      tonto%known_keywords(106) = "move_origin                   "
      tonto%known_keywords(107) = "move_origin_to_center_of_atoms"
      tonto%known_keywords(108) = "move_origin_to_center_of_mass "
      tonto%known_keywords(109) = "mp2                           "
      tonto%known_keywords(110) = "multiplicity=                 "
      tonto%known_keywords(111) = "name=                         "
      tonto%known_keywords(112) = "normal_mode_analysis          "
      tonto%known_keywords(113) = "make_internal_adps            "
      tonto%known_keywords(114) = "make_fd_hessian               "
      tonto%known_keywords(115) = "fd_hessian_step=              "
      tonto%known_keywords(116) = "internal_adp_temperature=     "
      tonto%known_keywords(117) = "soft_mode_cutoff=             "
      tonto%known_keywords(118) = "frequency_scale_factor=       "
      tonto%known_keywords(119) = "nucleus_model=                "
      tonto%known_keywords(120) = "output_style_options=         "
      tonto%known_keywords(121) = "phi_step_size                 "
      tonto%known_keywords(122) = "plot                          "
      tonto%known_keywords(123) = "plot_fragments                "
      tonto%known_keywords(124) = "plots                         "
      tonto%known_keywords(125) = "plot_grid=                    "
      tonto%known_keywords(126) = "plot_on_isosurface            "
      tonto%known_keywords(127) = "plot_uc_density               "
      tonto%known_keywords(128) = "pointgroup=                   "
      tonto%known_keywords(129) = "process_cif_and_c23_xml       "
      tonto%known_keywords(130) = "process_cif_and_cp2k_data     "
      tonto%known_keywords(131) = "put                           "
      tonto%known_keywords(132) = "put_3_center_overlaps_check   "
      tonto%known_keywords(133) = "put_1e_properties             "
      tonto%known_keywords(134) = "put_1e_properties_for_groups  "
      tonto%known_keywords(135) = "put_atom_coordinates          "
      tonto%known_keywords(136) = "put_adps                      "
      tonto%known_keywords(137) = "put_anos                      "
      tonto%known_keywords(138) = "put_ao_energy_partition       "
      tonto%known_keywords(139) = "put_atom_groups               "
      tonto%known_keywords(140) = "put_atomic_form_factors       "
      tonto%known_keywords(141) = "put_ba_moments                "
      tonto%known_keywords(142) = "put_bda_charges               "
      tonto%known_keywords(143) = "put_becke_atom_moments        "
      tonto%known_keywords(144) = "put_cosx_shell_errors         "
      tonto%known_keywords(145) = "put_grid_shell_errors         "
      tonto%known_keywords(146) = "put_xc_batch_statistics       "
      tonto%known_keywords(147) = "put_bond_length_with_error    "
      tonto%known_keywords(148) = "put_bond_angle_with_error     "
      tonto%known_keywords(149) = "put_torsion_angle_with_error  "
      tonto%known_keywords(150) = "put_ccdc_bonded_neighbours    "
      tonto%known_keywords(151) = "put_cif                       "
      tonto%known_keywords(152) = "put_coulomb_energy            "
      tonto%known_keywords(153) = "put_crystal                   "
      tonto%known_keywords(154) = "put_density_matrix            "
      tonto%known_keywords(155) = "put_density_mx                "
      tonto%known_keywords(156) = "put_dnc_displaced_geometries  "
      tonto%known_keywords(157) = "put_ed_between_atoms          "
      tonto%known_keywords(158) = "put_fitting_plots             "
      tonto%known_keywords(159) = "put_fock_matrix               "
      tonto%known_keywords(160) = "put_fock_mx                   "
      tonto%known_keywords(161) = "put_group_12_energies         "
      tonto%known_keywords(162) = "put_group_12_multipole_energy "
      tonto%known_keywords(163) = "put_group_12_spackman1986_ener"
      tonto%known_keywords(164) = "put_group_12_grimme2006_energy"
      tonto%known_keywords(165) = "put_group_12_polarization_ener"
      tonto%known_keywords(166) = "put_grown_cif                 "
      tonto%known_keywords(167) = "put_ha_moments                "
      tonto%known_keywords(168) = "put_hirshfeld_moments         "
      tonto%known_keywords(169) = "put_hirshfeld_test            "
      tonto%known_keywords(170) = "put_ri_ff_check               "
      tonto%known_keywords(171) = "fit_sph_atom_ffs              "
      tonto%known_keywords(172) = "put_mayer_bond_indices        "
      tonto%known_keywords(173) = "put_cell_map                  "
      tonto%known_keywords(174) = "put_cell_map_radial_functions"
      tonto%known_keywords(175) = "put_angular_hirshfeld_atoms   "
      tonto%known_keywords(176) = "put_ha_populations            "
      tonto%known_keywords(177) = "put_minmax_residual_density   "
      tonto%known_keywords(178) = "put_molecular_orbitals        "
      tonto%known_keywords(179) = "put_mo_energies               "
      tonto%known_keywords(180) = "put_mo_energy_partition       "
      tonto%known_keywords(181) = "put_mos                       "
      tonto%known_keywords(182) = "put_mos_and_energies          "
      tonto%known_keywords(183) = "put_mulliken_populations      "
      tonto%known_keywords(184) = "put_natural_orbitals          "
      tonto%known_keywords(185) = "put_nbo_file_47               "
      tonto%known_keywords(186) = "put_normal_modes              "
      tonto%known_keywords(187) = "put_internal_adps             "
      tonto%known_keywords(188) = "put_nos                       "
      tonto%known_keywords(189) = "put_overlap_matrix            "
      tonto%known_keywords(190) = "put_overlap_mx                "
      tonto%known_keywords(191) = "put_overlap_mx_evals          "
      tonto%known_keywords(192) = "put_overlapping_atoms         "
      tonto%known_keywords(193) = "put_pdb                       "
      tonto%known_keywords(194) = "put_plot_grid                 "
      tonto%known_keywords(195) = "put_pointgroup                "
      tonto%known_keywords(196) = "put_principal_axes            "
      tonto%known_keywords(197) = "put_reflection_data           "
      tonto%known_keywords(198) = "put_ri_metric_timings         "
      tonto%known_keywords(199) = "put_roby_mmo_energy_partition "
      tonto%known_keywords(200) = "put_roby_smo_energy_partition "
      tonto%known_keywords(201) = "put_sa_cell_fn_between_atoms  "
      tonto%known_keywords(202) = "put_sa_ed_between_atoms       "
      tonto%known_keywords(203) = "put_sa_moments                "
      tonto%known_keywords(204) = "put_salvador_moments          "
      tonto%known_keywords(205) = "put_salvador_radii            "
      tonto%known_keywords(206) = "put_ss_moments                "
      tonto%known_keywords(207) = "put_sao_energy_partition      "
      tonto%known_keywords(208) = "put_scf_energy_in_mo_pairs    "
      tonto%known_keywords(209) = "put_scf_energy_as_mo_parts    "
      tonto%known_keywords(210) = "put_scf_atomic_polarisabilitie"
      tonto%known_keywords(211) = "put_scf_atomic_polarizabilitie"
      tonto%known_keywords(212) = "put_scf_dipole_hyperpolarisabi"
      tonto%known_keywords(213) = "put_scf_dipole_hyperpolarizabi"
      tonto%known_keywords(214) = "put_scf_dipole_polarisability "
      tonto%known_keywords(215) = "put_scf_dipole_polarizability "
      tonto%known_keywords(216) = "put_shelx_fcf5                "
      tonto%known_keywords(217) = "put_shelx_fcf6                "
      tonto%known_keywords(218) = "put_shelx_hkl3                "
      tonto%known_keywords(219) = "put_shelx_hkl4                "
      tonto%known_keywords(220) = "put_sos_dipole_polarisability "
      tonto%known_keywords(221) = "put_spherical_sa_ed_from_atom "
      tonto%known_keywords(222) = "put_sylvain_csizmadia_tensors "
      tonto%known_keywords(223) = "put_structure_factor_stats    "
      tonto%known_keywords(224) = "put_structure_factors         "
      tonto%known_keywords(225) = "put_submolecule1              "
      tonto%known_keywords(226) = "put_submolecule2              "
      tonto%known_keywords(227) = "put_uc_dipole_polarisability  "
      tonto%known_keywords(228) = "put_uc_dipole_polarizability  "
      tonto%known_keywords(229) = "put_uc_dipole_hyperpolarisabil"
      tonto%known_keywords(230) = "put_uc_dipole_hyperpolarizabil"
      tonto%known_keywords(231) = "put_uc_chi2_old               "
      tonto%known_keywords(232) = "put_uc_refractive_index       "
      tonto%known_keywords(233) = "put_vrml                      "
      tonto%known_keywords(234) = "put_xyz_file                  "
      tonto%known_keywords(235) = "read_archive                  "
      tonto%known_keywords(236) = "read_archive_and_normalise    "
      tonto%known_keywords(237) = "read_archive_and_normalize    "
      tonto%known_keywords(238) = "read_ascii_archive            "
      tonto%known_keywords(239) = "read_fchk_file                "
      tonto%known_keywords(240) = "read_g09_fchk_file            "
      tonto%known_keywords(241) = "read_gaussian_fchk_file       "
      tonto%known_keywords(242) = "read_molden_file              "
      tonto%known_keywords(243) = "read_molden_mos               "
      tonto%known_keywords(244) = "read_molden_nos               "
      tonto%known_keywords(245) = "read_tonto_fchk_file          "
      tonto%known_keywords(246) = "redirect                      "
      tonto%known_keywords(247) = "redirect_stdout               "
      tonto%known_keywords(248) = "refine_hirshfeld_atoms        "
      tonto%known_keywords(249) = "reset_molecule                "
      tonto%known_keywords(250) = "revert                        "
      tonto%known_keywords(251) = "revert_stdout                 "
      tonto%known_keywords(252) = "rmp2                          "
      tonto%known_keywords(253) = "roby_analysis                 "
      tonto%known_keywords(254) = "robydata=                     "
      tonto%known_keywords(255) = "rotate                        "
      tonto%known_keywords(256) = "rotate_group                  "
      tonto%known_keywords(257) = "rotate_with_matrix            "
      tonto%known_keywords(258) = "rotate_with_mx                "
      tonto%known_keywords(259) = "saved_basis_name=             "
      tonto%known_keywords(260) = "saved_coppensbasis_name=      "
      tonto%known_keywords(261) = "saved_isosurface_plot         "
      tonto%known_keywords(262) = "saved_slaterbasis_name=       "
      tonto%known_keywords(263) = "saved_plot_on_isosurface      "
      tonto%known_keywords(264) = "scf                           "
      tonto%known_keywords(265) = "scf_data=                     "
      tonto%known_keywords(266) = "scfdata=                      "
      tonto%known_keywords(267) = "set_isotropic_adps            "
      tonto%known_keywords(268) = "set_isotropic_h_adps          "
      tonto%known_keywords(269) = "shorten_vdw_bond              "
      tonto%known_keywords(270) = "slaterbasis_name=             "
      tonto%known_keywords(271) = "spin_contamination            "
      tonto%known_keywords(272) = "spin_multiplicity=            "
      tonto%known_keywords(273) = "table_column=                 "
      tonto%known_keywords(274) = "unsave                        "
      tonto%known_keywords(275) = "use_crystal23_order=          "
      tonto%known_keywords(276) = "use_interpolators=            "
      tonto%known_keywords(277) = "use_spherical_basis=          "
      tonto%known_keywords(278) = "write_aim2000_wfn_file        "
      tonto%known_keywords(279) = "write_florian_wfn_file        "
      tonto%known_keywords(280) = "write_archive                 "
      tonto%known_keywords(281) = "write_ascii_archive           "
      tonto%known_keywords(282) = "write_tonto_fchk_mo           "
      tonto%known_keywords(283) = "write_tonto_fchk_dm           "
      tonto%known_keywords(284) = "write_morphy98_wfn_file       "
      tonto%known_keywords(285) = "write_xyz_file                "
      tonto%known_keywords(286) = "write_fragment_xyz_file       "
      tonto%known_keywords(287) = "write_xtal23_xyz_file         "
      tonto%known_keywords(288) = "cx_file_name=                 "
      tonto%known_keywords(289) = "cx_isosurface=                "
      tonto%known_keywords(290) = "cx_surface=                   "
      tonto%known_keywords(291) = "cx_uses_angstrom=             "
      tonto%known_keywords(292) = "process_cif                   "
      tonto%known_keywords(293) = "process_cif_cell_and_symmetry "
      tonto%known_keywords(294) = "process_cif_for_cx            "
      tonto%known_keywords(295) = "put_cx_data                   "
      tonto%known_keywords(296) = "put_invariom_label            "
      tonto%known_keywords(297) = "put_invariom_labels           "
      tonto%known_keywords(298) = "put_next_neighbour_handedness "
      tonto%known_keywords(299) = "put_smiles_invariom_labels    "
      tonto%known_keywords(300) = "td_data=                      "
      tonto%known_keywords(301) = "td_excited_states             "
      tonto%known_keywords(302) = "test_vdw_bonding              "
      tonto%known_keywords(303) = "test_vovo                     "
      tonto%known_keywords(304) = "test_mgs_qr                   "
      tonto%known_keywords(305) = "test_davidson_cps_6           "
      call unknown_(tonto,keyword,"MOLECULE.MAIN:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine


!  A

!   docu_read_atoms(die) :: selfless, private
!   ! Documentation.
!      die :: BIN, optional, IN
!      stdout.flush
!      stdout.flush
!      stdout.text("read_atoms= VEC{ATOM}@ ")
!      stdout.flush
!      stdout.text(". The list of atom data")
!      stdout.flush
!      stdout.text(". Read the documentation to know how to input")
!      stdout.text("  a list of complex data.")
!      if (present(die)) stop
!   end

!   docu_read_atom_groups(die) :: selfless, private
!   ! Documentation.
!      die :: BIN, optional, IN
!      stdout.flush
!      stdout.flush
!      stdout.text("read_atom_groups= VEC{ATOM_GROUP}@ ")
!      stdout.flush
!      stdout.text(". The list of ATOM_GROUP data")
!      stdout.flush
!      stdout.text(". Read documentation to know how to input a list of data")
!      if (present(die)) stop
!   end

   subroutine read_atom_groups(self)
   ! Read in the .atom_group information
      type(MOLECULE_TYPE), INOUT :: self

! WARN_IF(.atom.deallocated,"normally, atoms= must be defined before atom_groups=")
      BIN :: done

      ! Read the atom groups
      call read_list_keywords_(self%atom_group)

      ! CE hack to bypass Tonto group finery
      done = FALSE
      if (NOT allocated(self%atom)) then
         if (has_wfn_files_(self%atom_group)) then

            ! Make the atoms from FChk files?
            call read_groups_from_wfn_files_(self)
            done = TRUE
         end if
      end if
      if (NOT done) then
         call update_atom_basis_group_info_(self)
      end if
   end subroutine

   subroutine read_groups_from_wfn_files(self)
   ! Make the atom groups by read in Gaussian Fchk files in they are
   ! defined for the groups.
   ! NOTE: this creates a default SCF_data since the gaussian FChk more
   ! than likely has some MOs and a density mx inside it.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name
      INT :: g
   DIE_IF(allocated(self%atom),"MOLECULE.MAIN:read_groups_from_wfn_files ... atoms already exists!")
   DIE_IF(NOT allocated(self%atom_group),"MOLECULE.MAIN:read_groups_from_wfn_files ... no atom groups")
   DIE_IF(NOT has_wfn_files_(self%atom_group),"MOLECULE.MAIN:read_groups_from_wfn_files ... no wfn files")
      if (NOT allocated(self%SCF_data)) then
         call create_(self%SCF_data)
         call set_defaults_(self%SCF_data)
         call set_SCF_kind_(self%SCF_data,"rhf")
      end if
      allocate(self%mol(size(self%atom_group)))
      do g = 1,size(self%atom_group)

         ! Set name
         if (self%atom_group(g)%name/=" ") then
            call set_name_(self%mol(g),self%atom_group(g)%name)
         end if

         ! Define mol from FChk file
         name = wfn_file_(self%atom_group(g))
         if (name/=" ") call read_wfn_file_(self%mol(g),name)

         ! Set kind of SCF of joined groups!
         if (is_unrestricted_(self%mol(g)%SCF_data)) then
            call set_SCF_kind_(self%SCF_data,"uhf")  ! <== HERE
         end if

         ! Rotate and translate it
         call rotate_by_(self%mol(g),self%atom_group(g)%rotation_mx)
         call translate_by_(self%mol(g)%atom,self%atom_group(g)%shift)
         call set_group_to_(self%mol(g)%atom,g)
      end do

      ! Define .atom list
      call join_atom_groups_(self)

      ! See NOTE in comments
      if (NOT allocated(self%SCF_data)) then
         call set_SCF_defaults_(self)  ! NOT PURE
         call set_output_(self%SCF_data,FALSE)
      end if

      ! Update group info from self
      call update_group_crystal_and_ANOs_(self)
   end subroutine

   subroutine read_atom_partition_model(self)
   ! Read in the .atom_partition_model
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      call to_lower_case_(val)
      call set_atom_partition_model_(self,val)
   end subroutine

   subroutine read_atoms(self)
   ! Read in the atom list information
      type(MOLECULE_TYPE), INOUT :: self
   DIE_IF(allocated(self%cluster),"MOLECULE.MAIN:read_atoms ... cluster already defined! swap cluster= and atoms= ?")
   WARN_IF(allocated(self%atom),"MOLECULE.MAIN:read_atoms ... atom list already defined!")

      ! Destroy atom_groups & reset group indices
      call destroy_(self%atom_group)

      ! Force update
      self%atom_info_made = FALSE

      ! Read list keywords
      call read_list_keywords_(self%atom)

      ! Finalize
      call update_atom_basis_group_info_(self)

      ! Make Hirshfeld info
      if (allocated(self%SCF_data)) then
         if (self%basis_info_made) then
            if (allocated(self%crystal)) then
               call update_crystal_atom_info_(self)
               if (allocated(self%crystal%xray_data)) then
                  if (uses_numerical_FFs_(self%crystal)) then
                     call make_ANOs_and_interpolators_(self)
                  end if
               end if
            end if
         end if
      end if
   end subroutine


!  B

   subroutine read_B_field(self)
   ! Read in the applied magnetic B field
      type(MOLECULE_TYPE), INOUT :: self
      call read_quantity_(stdin,self%B_field)
   end subroutine

   subroutine read_basis_directory(self)
   ! Read in the basis directory (for all kinds of bases)
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=PATH_SIZE) :: directory  ! a path
      call read_(stdin,directory)
      call set_basis_directory_(self,directory)
   end subroutine

   subroutine read_basis_name(self)
   ! Read in the *gaussian* basis type to be used for the atoms.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name
      call read_(stdin,name)
      call set_basis_name_(self,name)
      if (allocated(self%atom)) then
         call update_atom_basis_group_info_(self)
      end if
   end subroutine

   subroutine read_basis_sets(self)
   ! Read in a list of atomic basis sets for the molecule
      type(MOLECULE_TYPE), INOUT :: self
   WARN_IF(allocated(self%basis),"MOLECULE.MAIN:read_basis_sets ... basis sets already defined!")

      ! Copy bases
      if (allocated(self%atom)) call destroy_basis_part_(self%atom)

      ! Basis sets are read in normalized, but internally
      ! are used unnormalized.
      if (allocated(self%basis)) call renormalize_(self%basis)
      call read_list_keywords_(self%basis)
      if (allocated(self%basis)) call unnormalize_(self%basis)

      ! Finalize
      call update_atom_basis_group_info_(self)

      ! Make Hirshfeld info
      if (allocated(self%SCF_data)) then
         if (allocated(self%atom)) then
            if (allocated(self%crystal)) then
               if (allocated(self%crystal%xray_data)) then
                  if (uses_numerical_FFs_(self%crystal)) then
                     call make_ANOs_and_interpolators_(self)
                  end if
               end if
            end if
         end if
      end if
   end subroutine


!  Below should be in some other module?

!   batch_HS :: leaky
!   ! Make a HS for every symmetry-unique molecule in a CIF file
!      self :: INOUT
!
!      i, n_molecules :: INT
!      name :: STR
!
!      .cluster.create(.crystal) 
!      .cluster.set_generation_method("fragment")
!      .cluster.set_radius(ZERO)
!      .cluster.set_defragment(TRUE)
!      .cluster.update_info
!      n_molecules = self.cluster.n_molecules
!
!      name = .name
!
!      do i = 1, n_molecules
!         .make_HS_for_group(i)
!         .set_from_saved_self
!         .cluster.create(.crystal) 
!         .cluster.set_generation_method("fragment")
!         .cluster.set_radius(ZERO)
!         .cluster.set_defragment(TRUE)
!         .cluster.update_info
!         .name = name
!      end
!
!   end

!   make_HS_for_group(i) :: private, leaky
!   ! Make the wavefunction, then HS, then the ESP mapped on it
!   ! for the i-th molecule in the cluster.
!      self :: allocatable, OUT
!
!      i :: INT
!      mol :: MOLECULE@
!      formula, name, filename :: STR
!
!      mol.create
!      .create_cluster_mol(i, mol)
!
!      formula = mol.atom.chemical_formula(FALSE)
!      name = trim(.name)//"-"//trim(formula)//"_"//trim(i.to_str)
!      stdout.show("Name ", name)
!      stdout.show("Molecular formula ", formula)
!      mol.atom.put_xyz_file(trim(name))
!      stdout.show("Creating Hirshfeld surface m =", i)
!      call make_hirshfeld_surface(mol, 0.5d0) ! Where is this routine?
!      stdout.text("Hirshfeld surface done.")
!
!   end

!   make_hirshfeld_surface(m, res) :: selfless, private, leaky
!   ! Make the HS for molecule "m" with desired resolution "res".
!   ! Also do the ESP on the HS of "m" afterwards.
!      m :: MOLECULE*
!      res :: REAL, IN
!
!      ! Create HS cluster for molecule "m"
!      m.cluster.create(m.crystal)
!      m.cluster.set_generation_method("for_hirshfeld_surface")
!      m.cluster.set_atom_density_cutoff(TOL(8))
!      m.cluster.set_defragment(FALSE)
!      m.cluster.update_info
!      m.create_cluster
!
!      ! Use Slater-type HS
!      m.atom.set_basis_kind("slater")
!
!      ! Set up interpolator type
!      m.interpolator.create
!      m.interpolator.set_interpolation_method("linear")
!      m.interpolator.set_domain_mapping("sqrt")
!      m.interpolator.set_table_eps(1.0d-10)
!      m.interpolator.set_table_spacing(1.0d-1)
!      m.destroy_interpolators
!
!      ! Set up CX_isosurface
!      m.isosurface.create(m.atom)
!      m.isosurface.property = "stockholder_weight"
!      m.isosurface.triangulation_method = "recursive_marching_cube"
!      m.isosurface.iso_value = 0.5
!      m.isosurface.minimum_scan_division = 1
!      m.isosurface.voxel_proximity_factor = 5
!      m.isosurface.CX_output_distance_properties = TRUE
!      m.isosurface.CX_output_shape_properties = TRUE
!
!      ! Initialize CX_isosurface.plot_grid
!      m.isosurface.plot_grid.use_bcube_with_shape_axes
!      m.isosurface.plot_grid.set_cube_scale_factor(1.0d0)
!
!      ! Desired separation is essentially the resolution of the calculated surface
!      m.isosurface.plot_grid.set_desired_separation(res)
!
!      ! Make isosurface
!      m.isosurface_plot
!
!      ! Do the ESP on surface
!      if (FALSE) then
!         m.isosurface.plot_grid.electric_potential_method = "mixed"
!         m.isosurface.surface_property = "electric_potential"
!         m.atom.set_basis_kind("gaussian")
!         m.saved_plot_on_isosurface
!      end
!      m.cluster.destroy
!   end

   subroutine read_becke_grid(self)
   ! Read in the Becke integration grid data
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.MAIN:read_becke_grid ... input atom information before becke_grid information")
      if (NOT allocated(self%becke_grid)) call create_(self%becke_grid)

      ! Includes
      call read_keywords_(self%becke_grid)

      ! Finalize
      call set_atom_info_(self%becke_grid,self%atom)
      call set_grid_data_(self%becke_grid)
   end subroutine


!  C

   subroutine read_C23_XML_file_name(self)
   ! Read in the Becke integration grid data
      type(MOLECULE_TYPE), INOUT :: self
      if (NOT allocated(self%crystal23)) call create_(self%crystal23)
      call read_(stdin,self%crystal23%xml_file_name)
   end subroutine

   subroutine read_charge(self)
   ! Read in the total molecular charge
      type(MOLECULE_TYPE), INOUT :: self
      call read_(stdin,self%charge)

      ! Set the no. of electons
      if (allocated(self%atom)) self%n_e = no_of_electrons_(self)
   end subroutine

   subroutine read_cluster(self)
   ! Read in the crystal cluster data. NOTE: the .atom list must correspond to
   ! the (unpruned or pruned) asymmetric unit cell geometry of the .crystal.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.MAIN:read_cluster ... no atom data")
   ENSURE(allocated(self%crystal),"MOLECULE.MAIN:read_cluster ... no crystal data")
      call update_fragment_info_(self%crystal,self%atom)
      DIE_IF(NOT allocated(self%crystal%asymmetric_unit_atom),"MOLECULE.MAIN:read_cluster ... atom list missing asym atoms")
      call destroy_(self%cluster)
      call create_(self%cluster)
      call set_defaults_(self%cluster,self%crystal)
      call read_keywords_(self%cluster)
   end subroutine

   subroutine read_auxiliary_basis_name(self)
   ! Read in the auxiliary (fitting) basis type to be used for the atoms.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: this
      call read_(stdin,this)
      call set_auxiliary_basis_name_(self,this)
   end subroutine

   subroutine read_coppensbasis_name(self)
   ! Read in the *coppens* basis type to be used for the atoms.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: this
      call read_(stdin,this)
      call set_coppensbasis_name_(self,this)
   end subroutine

   subroutine read_coppensbasis_sets(self)
   ! Read in a list of atomic Coppens basis sets for the molecule
      type(MOLECULE_TYPE), INOUT :: self
   WARN_IF(allocated(self%coppensbasis),"MOLECULE.MAIN:read_coppensbasis_sets ... coppensbasis sets already defined!")
      if (allocated(self%atom)) call destroy_coppensbasis_part_(self%atom)
      if (allocated(self%coppensbasis)) call renormalize_(self%coppensbasis)
      call read_list_keywords_(self%coppensbasis)
      if (allocated(self%coppensbasis)) call unnormalize_(self%coppensbasis)
      call update_atom_basis_group_info_(self)
   end subroutine

   subroutine create_g09_wfn_from_crystal(self)
   ! Write g09 input file for what kinmd of job?
      type(MOLECULE_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), allocatable :: atoms
      call create_(self%cluster)
      call set_defaults_(self%cluster,self%crystal)
      call set_generation_method_(self%cluster,"fragment")
      call set_defragment_(self%cluster,TRUE)
      call update_info_(self%cluster)
      call create_mol_atom_list_(self%cluster,atoms,1)
      call put_g09_input_(atoms,trim(self%name))
   end subroutine

   subroutine read_cell_map(self)
   ! Read the settings of a map of the density of the unit cell
      type(MOLECULE_TYPE), INOUT :: self
      if (NOT allocated(self%cell_map)) call create_(self%cell_map)

      ! The cell first, so that centre_fractional= can be converted
      if (allocated(self%crystal)) call set_cell_(self%cell_map,self%crystal%unit_cell)
      call read_keywords_(self%cell_map)

      ! The promolecule, for a deformation kind
      if (is_deformation_kind_(self%cell_map)) call set_cell_map_promolecule_(self)
   end subroutine

   subroutine set_cell_map_promolecule(self)
   ! Give the cell map the promolecule structure factors: the spherical
   ! atoms of the current method and basis (or the International Tables
   ! form factors, if use_IAM_ITC_FFs= is set) at the model's positions
   ! and ADPs, assembled as F_calc is, so that the deformation kinds
   ! subtract them on the same footing. Made from the structure as it is
   ! when the cell_map= block is read; read the block again after a
   ! refinement.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable :: ff
      VEC(CPX,:), allocatable :: F
   DIE_IF(NOT allocated(self%crystal),"MOLECULE.MAIN:set_cell_map_promolecule ... no crystal")
   DIE_IF(NOT allocated(self%crystal%xray_data),"MOLECULE.MAIN:set_cell_map_promolecule ... no diffraction data")
   DIE_IF(NOT self%crystal%fragment_info_made,"MOLECULE.MAIN:set_cell_map_promolecule ... no crystal fragment info")
      if (NOT self%crystal%xray_data%use_IAM_ITC_FFs) then
         call make_ANOs_and_interpolators_(self)
         call set_use_IAM_ITC_FFs_(self%atom,FALSE)
      end if
      call make_unique_IAM_atom_FFs_(self,ff)
      call create_(F,size(self%crystal%xray_data%reflections))
      call make_F_calc_(self%crystal,F,ff)
      call set_promolecule_F_(self%cell_map,F)
      call destroy_(F)
      call destroy_(ff)
   end subroutine

   subroutine put_cell_map(self)
   ! Fill the cell map from the diffraction data and put its settings, the
   ! number of reflections and the electron count
      type(MOLECULE_TYPE), INOUT :: self
   DIE_IF(NOT allocated(self%crystal),"MOLECULE.MAIN:put_cell_map ... no crystal")
      if (NOT allocated(self%cell_map)) call create_(self%cell_map)
      if (is_deformation_kind_(self%cell_map)) call set_cell_map_promolecule_(self)
      call make_cell_map_(self%crystal,self%cell_map)
      call put_(self%cell_map)
   end subroutine

   subroutine put_angular_Hirshfeld_atoms(self,amplitude)
   ! For each unique fragment atom, put the radial functions of its
   ! Hirshfeld atom on its Becke radial shells, l = 0 to the cell_map
   ! block's l_max, and the power of each l integrated over s, which does
   ! not depend on the axes: for the density that is the l-resolved
   ! electron count of the atom, zero for l > 0; with "amplitude" TRUE
   ! the square root of the atom is expanded and the integrals are the
   ! populations n^A_l, which sum to the atom's electron count. The cell_map
   ! block's density_source= and atom_weight= say which density and
   ! whether the Hirshfeld weight is applied.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, IN :: amplitude
      MAT(REAL,:,:), allocatable :: f
      VEC(REAL,:), allocatable :: radii,wts,power
      type(QUADRATURE_TYPE), allocatable :: rgrid
      REAL :: total
      REAL :: fac,fac3
      INT :: n_u,u,a,n_rad,i,lv,m,lm,n_lm,l_max,l_leb
      BIN :: weighted,use_cell_map
   ENSURE(allocated(self%atom),"MOLECULE.MAIN:put_angular_Hirshfeld_atoms ... no atoms")
   ENSURE(allocated(self%density_mx),"MOLECULE.MAIN:put_angular_Hirshfeld_atoms ... no density matrix")
      if (NOT allocated(self%cell_map)) call create_(self%cell_map)
      l_max = self%cell_map%l_max
      call set_indices_(l_max)
      weighted = self%cell_map%atom_weight=="hirshfeld"
      use_cell_map = self%cell_map%density_source=="cell_map"
      if (use_cell_map) then
         DIE_IF(NOT allocated(self%crystal),"MOLECULE.MAIN:put_angular_Hirshfeld_atoms ... density_source= cell_map needs a crystal")
         call make_cell_map_(self%crystal,self%cell_map)
      end if

      ! Hirshfeld weights, interpolators and the Becke grid
      call make_atom_partition_info_(self)

      ! The Lebedev order: enough for Y_lm times a density of like angular
      ! content, and never a pruned grid
      l_leb = max(2*l_max+2,29)
      fac = ANGSTROM_PER_BOHR
      fac3 = BOHR_PER_ANGSTROM**3
      n_lm = (l_max+1)*(l_max+1)
      call flush_(stdout)
      if (amplitude) then
         call text_(stdout,"=================================================")
         call text_(stdout,"Angular decomposition of the Hirshfeld amplitudes")
         call text_(stdout,"=================================================")
      else
         call text_(stdout,"============================================")
         call text_(stdout,"Angular decomposition of the Hirshfeld atoms")
         call text_(stdout,"============================================")
      end if
      call flush_(stdout)
      call show_(stdout,"l_max                   =",l_max)
      call show_(stdout,"Lebedev order           =",l_leb)
      call show_(stdout,"Density source          =",self%cell_map%density_source)
      call show_(stdout,"Atom weight             =",self%cell_map%atom_weight)
      call text_(stdout,". f_lm(s) in e/Angstrom^3 (its square root for the amplitude), s in Angstrom;")
      call text_(stdout,"  the atom on the sphere of radius s is the sum over l, m of f_lm(s) Y_lm,")
      call text_(stdout,"  with Y_lm real harmonics of unit integral; m = -1, 0, 1 are y, z, x.")
      call text_(stdout,". P_l = sum over m of the integral of f_lm(s)^2 s^2 ds: for the density")
      call text_(stdout,"  its l = 0 value is 1/(4 pi) times (electron count)^2 only if the atom is")
      call text_(stdout,"  spherical, so P_l are compared between atoms, not with a count; for the")
      call text_(stdout,"  amplitude P_l = n_l, the electrons of angular character l, summing to")
      call text_(stdout,"  the atom's electron count. A free atom in a closed shell has n_l = 0 for")
      call text_(stdout,"  l > 0 whatever its orbitals; n_l are not orbital occupations.")
      n_u = self%n_atom
      if (allocated(self%crystal)) then
         if (self%crystal%fragment_info_made) n_u = self%crystal%n_unique_frag_atoms
      end if
      do u = 1,n_u
         a = u
         if (allocated(self%crystal)) then
            if (self%crystal%fragment_info_made) a = self%crystal%unique_frag_atom(u)
         end if

         ! The atom's radial shells, and their weights 4 pi s^2 ds
         rgrid = radial_grid_for_atom_(self%becke_grid,a)
         n_rad = rgrid%n_pts
         call destroy_(rgrid)
         call create_(radii,n_rad)
         call create_(wts,n_rad)
         do i = 1,n_rad
            call radial_shell_for_atom_(self%becke_grid,a,i,radii(i),wts(i))
         end do
         call create_(f,n_rad,n_lm)
         call make_atom_lm_radial_functions_(self,f,radii,a,l_max,l_leb,amplitude,weighted,use_cell_map)
         call flush_(stdout)
         call text_(stdout,"Atom "//trim(self%atom(a)%label)//", f_lm(s), columns (l,m):")
         call flush_(stdout)
         do i = 1,n_rad
            call put_(stdout,radii(i)*fac,width=10,precision=5)
            do lm = 1,n_lm
               call put_(stdout,f(i,lm)*fac3,width=12,precision=5)
            end do
            call flush_(stdout)
         end do

         ! The power of each l over s; wts carry 4 pi s^2 ds
         call create_(power,l_max+1)
         power = ZERO
         do lv = 0,l_max
            do m = -lv,lv
               lm = lv*lv+lv+m+1
               power(lv+1) = power(lv+1)+sum(wts*f(:,lm)**2)/(4*PI)
            end do
         end do
         total = sum(power)
         call flush_(stdout)
         if (amplitude) then
            call text_(stdout,"Atom "//trim(self%atom(a)%label)//", populations n_l, l = 0 to l_max, and their sum:")
         else
            call text_(stdout,"Atom "//trim(self%atom(a)%label)//", power P_l, l = 0 to l_max, and the sum:")
         end if
         do lv = 0,l_max
            call put_(stdout,power(lv+1),width=12,precision=5)
         end do
         call put_(stdout,total,width=12,precision=5)
         call flush_(stdout)

         ! The electron count, for the density: sqrt(4 pi) times the integral
         ! of f_00 s^2 ds, which the populations of the amplitude add up to;
         ! and the multipole moments, the integrals of f_lm s^(l+2) ds, in
         ! e Angstrom^l, the Hirshfeld atomic multipoles
         if (NOT amplitude) then
            total = sqrt(4*PI)*sum(wts*f(:,1))/(4*PI)
            call show_(stdout,"Electron count          =",total)
            call text_(stdout,"Atom "//trim(self%atom(a)%label)//", multipole moments mu_lm = integral f_lm s^(l+2) ds, e Angstrom^l, columns (l,m):")
            do lv = 0,l_max
               do m = -lv,lv
                  lm = lv*lv+lv+m+1
                  call put_(stdout,sum(wts*f(:,lm)*radii**lv)/(4*PI)*fac**lv,width=12,precision=5)
               end do
            end do
            call flush_(stdout)
         end if
         call destroy_(power)
         call destroy_(f)
         call destroy_(wts)
         call destroy_(radii)
      end do
   end subroutine

   subroutine put_cell_map_radial_functions(self)
   ! Fill the cell map from the diffraction data and put the radial
   ! functions of its density about the centre in the cell_map= block
      type(MOLECULE_TYPE), INOUT :: self
   DIE_IF(NOT allocated(self%crystal),"MOLECULE.MAIN:put_cell_map_radial_functions ... no crystal")
      if (NOT allocated(self%cell_map)) call create_(self%cell_map)
      if (is_deformation_kind_(self%cell_map)) call set_cell_map_promolecule_(self)
      call make_cell_map_(self%crystal,self%cell_map)
      call put_radial_functions_(self%cell_map)
   end subroutine

   subroutine read_crystal(self)
   ! Read in the crystal data. If the ".atom" list data is available,
   ! its coordinates will be resolved into cartesians at this point.
   ! IMPORTANT NOTE: normally the crystal information will be inputted
   ! from a CIF file.  However, if the crystal information is inputted
   ! explicitly, and the asymmetric unit is not specified, then the
   ! symmetry unique atoms in the ".atom" list are used to define the
   ! crystal asymmetric unit. Once defined, the asymmetric unit in the
   ! crystal is never destroyed. Thus, in this case, you must ensure
   ! that your atom list has enough atoms to generate the entire
   ! crystal; for co-crystals this may be a problem. Use the CIF file
   ! method for input in this case, or be careful.
      type(MOLECULE_TYPE), INOUT :: self
   WARN_IF(allocated(self%crystal),"MOLECULE.MAIN:read_crystal ... crystal data already defined!")

      ! Create?
      if (NOT allocated(self%crystal)) call create_(self%crystal)

      ! Read
      call read_keywords_(self%crystal)

      ! Finalize
      if (self%crystal%finalize) then

         ! Copy atom partition model
         if (allocated(self%crystal%xray_data)) then
            if (self%crystal%xray_data%partition_model/=" ") then

      !    .set_atom_partition_model(.crystal.xray_data.partition_model)
            end if
         end if
         if (allocated(self%atom)) then

            ! Finalize.
            ! May be needed for fragment capping
            call update_atom_basis_group_info_(self)
            call update_crystal_atom_info_(self)
         end if

         ! Update
         call update_diffraction_data_(self%crystal)
      end if

      ! Set dispersion coefficients?
   ! if (.atom.allocated) then
   ! if (.crystal.xray_data.allocated) then
   ! if (.crystal.xray_data.have_set_wavelength) then
   !    .atom.set_dispersion_coefficients(.crystal.xray_data.wavelength)
   ! end
   ! end
   ! end

      ! Make Hirshfeld info
      if (allocated(self%SCF_data)) then
         if (self%basis_info_made) then
            if (allocated(self%atom)) then
               if (allocated(self%crystal%xray_data)) then
                  if (uses_numerical_FFs_(self%crystal)) then
                     call make_ANOs_and_interpolators_(self)
                  end if
               end if
            end if
         end if
      end if

      ! Reset to finalize next time
      self%crystal%finalize = TRUE
   end subroutine

   subroutine update_crystal_atom_info(self)
   ! Update the crystal info if new atom info has been read in.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: tags

      ! Finalize crystal
      self%crystal%finalize = TRUE

      ! Set H U iso
      if (allocated(self%crystal%xray_data)) then
         if (self%crystal%xray_data%refine_H_U_iso) then
            call set_isotropic_H_ADP_(self%atom)
         end if
      end if

      ! Set U iso's
      if (allocated(self%crystal%xray_data)) then
         if (allocated(self%crystal%xray_data%refine_U_iso_for_atom)) then
            tags = self%crystal%xray_data%refine_U_iso_for_atom
            call set_isotropic_ADPs_(self%atom,tags)
         end if
      end if

      ! Cartesian axes
      call resolve_axis_system_(self)
      call update_fragment_info_(self%crystal,self%atom)

      ! Set H U iso & really do it this time
      if (allocated(self%crystal%xray_data)) then
         if (self%crystal%xray_data%refine_H_U_iso) then
            call set_isotropic_H_ADP_(self%crystal%fragment_atom)
            call set_isotropic_H_ADP_(self%crystal%asymmetric_unit_atom)
            call set_isotropic_H_ADP_(self%crystal%unit_cell_atom)
         end if
      end if

      ! Set U iso & really do it this time
      if (allocated(self%crystal%xray_data)) then
         if (allocated(self%crystal%xray_data%refine_U_iso_for_atom)) then
            tags = self%crystal%xray_data%refine_U_iso_for_atom
            call set_isotropic_ADPs_(self%crystal%fragment_atom,tags)
            call set_isotropic_ADPs_(self%crystal%asymmetric_unit_atom,tags)
            call set_isotropic_ADPs_(self%crystal%unit_cell_atom,tags)
         end if
      end if

      ! Set dispersion coefficients?
   ! if (.atom.allocated) then
   ! if (.crystal.xray_data.allocated) then
   ! if (.crystal.xray_data.have_set_wavelength) then
   !    .atom.set_dispersion_coefficients(.crystal.xray_data.wavelength)
   ! end
   ! end
   ! end

      ! Make Hirshfeld info
      if (allocated(self%SCF_data)) then
         if (self%basis_info_made) then
            if (allocated(self%atom)) then
               if (allocated(self%crystal%xray_data)) then
                  if (uses_numerical_FFs_(self%crystal)) then
                     call make_ANOs_and_interpolators_(self)
                  end if
               end if
            end if
         end if
      end if
   end subroutine


!  D

!   read_deserialize_filename :: leaky
!   ! Read in the *gaussian* basis type to be used for the atoms.
!      self :: INOUT
!
!      name :: STR
!
!      stdin.read(name)
!      .deserialize(name)
!
!   end

   subroutine read_dispersion_coefficients(self)
   ! Read the anaomalous dispersion coefficients f' and f''
   ! and set the atom list
      type(MOLECULE_TYPE), INOUT :: self
   DIE_IF(NOT allocated(self%atom),"MOLECULE.MAIN:read_dispersion_coefficients ... please enter dispersion after atom list")
      call read_dispersion_coefficients_
      call set_tabular_dispersion_(self%atom)
   end subroutine


!   dump_xyz_files :: leaky
!   ! Generate all wavefunctions from the cluster
!   ! NOTE: Broken, need to create SCF_data etc.
!   ! Very bad name! Fix! -- dylan
!      self :: INOUT
!
!      mol :: MOLECULE@
!      formula :: STR
!      filename :: STR
!      i :: INT
!
!      ! Defragment the cluster
!      .cluster.create(.crystal) 
!      .cluster.set_generation_method("fragment")
!      .cluster.set_radius(ZERO)
!      .cluster.set_defragment(TRUE)
!      .cluster.update_info
!
!      ! One molecule
!      i = 1
!      if (.cluster.n_molecules==1) then
!         formula  = .atom.chemical_formula(FALSE)
!         filename = trim(.name)//"-"//trim(formula)//"_"//trim(i.to_str)
!         stdout.show("Writing xyz file", trim(filename))
!         .atom.put_xyz_file(trim(filename))
!         return
!      end
!
!      ! Several mols?
!      do i = 1,.cluster.n_molecules
!         ! Make the i-th molecule
!         mol.create
!         .create_cluster_mol(i, mol)
!         stdout.show("Writing xyz file", trim(filename))
!         formula = mol.atom.chemical_formula(FALSE)
!         filename = trim(.name)//"-"//trim(formula)//"_"//trim(i.to_str)
!         mol.atom.put_xyz_file(trim(filename))
!      end
!
!   end

!  E

   subroutine read_E_field(self)
   ! Read in the applied electric E field
      type(MOLECULE_TYPE), INOUT :: self
      call read_quantity_(stdin,self%E_field)
   end subroutine


!  F

!   find_d_norm_contacts :: leaky
!   ! Find the close d_norm values and store the list of indices for
!   ! the corresponding closest atoms inside and outside the HS.
!      self :: PTR
!
!   ENSURE(.cif.allocated,"no CIF file")
!   ENSURE(.isosurface.allocated,"no isosurface")
!
!      i :: INT
!      ub :: REAL
!      formula, name :: STR
!      CX_file :: TEXTFILE@
!
!      ! Defragment any molecules in the CIF
!      .cluster.create(.crystal) 
!      .cluster.set_generation_method("fragment")
!      .cluster.set_radius(ZERO)
!      .cluster.set_defragment(TRUE)
!      .cluster.update_info
!
!      ub = .isosurface.surface_property_ub
!
!      if (.cluster.n_molecules == 1) then
!
!         i = 1
!
!         ! Set up name
!         formula = .atom.chemical_formula(FALSE)
!         stdout.show("formula ", formula)
!         name = trim(.name)//"-"//trim(formula)//"_"//trim(i.to_str)
!
!         ! Make the HS
!         stdout.flush
!         stdout.show("Creating Hirshfeld surface m =", i)
!         call make_HS(self,ub)
!         stdout.flush
!         stdout.text("Hirshfeld surface done.")
!
!         ! Print out cxs file
!         CX_file.create(trim(name)//".cxs")
!         CX_file.open_for("write")
!         .put_CX_data(CX_file,.cif)       
!         CX_file.close
!         CX_file.destroy
!
!         return
!
!      end
!
!      ! Make all HSs for i-th cluster mol
!      do i = 1, .cluster.n_molecules
!         .make_HS_for_cluster_mol(i,ub)
!      end
!
!   end

!   make_HS(m,ub) :: selfless, private, leaky
!   ! Make the HS for molecule "m" with desired resolution "res".
!      m  :: MOLECULE, INOUT
!      ub :: REAL, IN
!
!      res :: REAL
!
!      res = 0.2d0
!
!      ! Create HS cluster for molecule "m"
!      m.cluster.create(m.crystal)
!      m.cluster.set_generation_method("for_hirshfeld_surface")
!      m.cluster.set_atom_density_cutoff(TOL(8))
!      m.cluster.set_defragment(FALSE)
!      m.cluster.update_info
!
!      !!!!!!!!!!!!!!!!!!!!!
!      m.create_cluster
!      !!!!!!!!!!!!!!!!!!!!!
!
!      ! Use Slater-type HS
!      m.atom.set_basis_kind("slater")
!
!      ! Set up interpolator type
!      m.interpolator.create
!      m.interpolator.set_interpolation_method("linear")
!      m.interpolator.set_domain_mapping("sqrt")
!      m.interpolator.set_table_eps(1.0d-10)
!      m.interpolator.set_table_spacing(1.0d-1)
!      m.destroy_interpolators
!
!      ! Set up CX_isosurface
!      m.isosurface.destroy
!      m.isosurface.create(m.atom)
!      m.isosurface.property = "stockholder_weight"
!      m.isosurface.triangulation_method = "recursive_marching_cube"
!      m.isosurface.iso_value = 0.5
!      m.isosurface.minimum_scan_division = 1
!      m.isosurface.voxel_proximity_factor = 5
!      m.isosurface.CX_output_distance_properties = TRUE
!      m.isosurface.CX_output_shape_properties = TRUE
!
!      ! Initialize CX_isosurface.plot_grid
!      m.isosurface.plot_grid.use_bcube_with_shape_axes
!      m.isosurface.plot_grid.set_cube_scale_factor(1.0d0)
!
!      ! Desired separation is essentially the resolution of the calculated surface
!      m.isosurface.plot_grid.set_desired_separation(res)
!
!      ! Make HS isosurface
!      m.isosurface_plot
!
!      ! Do the d_norm close contact table
!      if (m.isosurface.allocated) then
!      if (m.isosurface.atom.allocated) then 
!      if (m.saved.allocated) then
!      if (m.saved.cluster.allocated) then
!         m.isosurface.set_surface_property_ub(ub)

!                                     ,m.atom)
!      end
!      end
!      end
!      end
!
!   end

!   make_HS_for_cluster_mol(i,ub) :: private, leaky
!   ! Make the HS for cluster molecule i.
!      self :: INOUT
!      i :: INT, IN
!      ub :: REAL, IN
!
!      mol :: MOLECULE@
!      formula, name :: STR
!      CX_file :: TEXTFILE@
!
!      ! Make the i-th cluster molecule "mol"
!      mol.create
!      .create_cluster_mol(i, mol)
!
!      ! Set molecule name
!      formula = mol.atom.chemical_formula(FALSE)
!      stdout.show("formula ", formula)
!      name = trim(.name)//"-"//trim(formula)//"_"//trim(i.to_str)
!      mol.set_name(trim(name))
!
!      ! Set basis set
!      mol.basis_name = .basis_name
!      .update_atom_basis_group_info
!
!      ! Make the HS
!      stdout.flush
!      stdout.show("Creating Hirshfeld surface m =", i)
!      call make_HS(mol,ub)
!      stdout.flush
!      stdout.text("Hirshfeld surface done.")
!
!      ! Deposit cxs file
!      CX_file.create(trim(name)//".cxs")
!      CX_file.open_for("write")
!      mol.put_CX_data(CX_file,.cif)       
!      CX_file.close
!      CX_file.destroy
!
!      ! Clean
!      mol.destroy
!
!   end

!  G

   subroutine read_gauge_origin(self)
   ! Read in the gauge origin vector for an applied external 
   ! magnetic field
      type(MOLECULE_TYPE), INOUT :: self
      call read_quantity_(stdin,self%gauge_origin)
   end subroutine


!   generate_all_wavefunctions :: leaky
!   ! Generate all wavefunctions from the cluster
!   ! NOTE: Broken, need to create SCF_data etc.
!      self :: INOUT
!
!      mol :: MOLECULE@
!      i :: INT
!      formula :: STR
!      filename, name :: STR
!
!      ! Defragment the cluster
!      .cluster.create(.crystal) 
!      .cluster.set_generation_method("fragment")
!      .cluster.set_radius(ZERO)
!      .cluster.set_defragment(TRUE)
!      .cluster.update_info
!
!
!      ! Make a single wavefunction
!      if (.cluster.n_molecules == 1) then
!
!         formula = .atom.chemical_formula(FALSE)
!         .make_ANOs_and_interpolators
!         .scf
!
!
!         return
!
!      end
!
!      ! Make several wavefunctions
!      do i = 1, .cluster.n_molecules
!
!         ! Make the i-th molecule
!         mol.create
!         .create_cluster_mol(i, mol) ! THIS IS STRANGE - FIX
!
!         ! Set up the name of the molecule
!         formula = mol.atom.chemical_formula(FALSE)
!         name = trim(.name)//"-"//trim(formula)//"_"//trim(i.to_str)
!         mol.set_name(trim(name))
!         mol.basis_name = .basis_name
!         mol.SCF_data.copy(.SCF_data)
!
!         ! Make the wavefunction
!         mol.make_ANOs_and_interpolators
!         mol.scf
!
!
!      end
!
!   end

!  I

   subroutine read_interpolator(self)
   ! Read template interpolators
      type(MOLECULE_TYPE), INOUT :: self
      call destroy_(self%interpolator)
      call create_(self%interpolator)
      call read_keywords_(self%interpolator)
      call destroy_interpolators_(self)
   end subroutine

   subroutine read_isosurface(self)
   ! Read in the isosurface data. 
   ! NOTE: the isosurface has its own plotgrid, different from that
   ! used in normal density plots, but .plot_grid is assigned to it.
   ! This will be a nightmare for memory leaks!
      type(MOLECULE_TYPE), INOUT :: self

      ! Destroy
      if (NOT allocated(self%isosurface)) call create_(self%isosurface,self%atom)

      ! Read
      call read_keywords_(self%isosurface)
   end subroutine

   subroutine set_isotropic_ADPs(self)
   ! As it says
      type(MOLECULE_TYPE), INOUT :: self

      ! Make isotropic
      if (allocated(self%crystal)) then
         if (allocated(self%crystal%xray_data)) then
            if (allocated(self%crystal%xray_data%refine_U_iso_for_atom)) then
               call set_isotropic_ADPs_(self%atom,self%crystal%xray_data%refine_U_iso_for_atom)
               call resolve_axis_system_(self)
               call destroy_asymmetric_unit_(self%crystal)
               call update_fragment_info_(self%crystal,self%atom)
            end if
         end if
      end if
   end subroutine

   subroutine set_isotropic_H_ADPs(self)
   ! As it says
      type(MOLECULE_TYPE), INOUT :: self

      ! Make isotropic
      call set_isotropic_H_ADP_(self%atom)

      ! Finalize crystal
      if (allocated(self%crystal)) then
         call resolve_axis_system_(self)
         call destroy_asymmetric_unit_(self%crystal)
         call update_fragment_info_(self%crystal,self%atom)
      end if
   end subroutine


!  K

   subroutine set_keyword_echo_on()
   ! Echo keywords into the stdout file
      call set_keyword_echo_(tonto,TRUE)
   end subroutine


!  L

   subroutine set_low_verbosity_on()
   ! Set low output verbosity
      call set_low_verbosity_(tonto,TRUE)
   end subroutine


!  M

!  N 

   subroutine read_name(self)
   ! Read in the name of the molecule. This must always be
   ! the first keyword inputted.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val
      call read_(stdin,val)
      call set_name_(self,val)
   end subroutine

   subroutine read_nucleus_model(self)
   ! Read the nucleus model: "point", or "gaussian" for the gaussian
   ! charges of Visscher and Dyall, At. Data Nucl. Data Tables 67, 207
   ! (1997). The second is available only with relativity_kind= x2c.
      type(MOLECULE_TYPE), INOUT :: self
      call read_(stdin,self%nucleus_model)
      call to_lower_case_(self%nucleus_model)
      if (self%nucleus_model=="finite") self%nucleus_model = "gaussian"
      DIE_IF(self%nucleus_model/="point" AND self%nucleus_model/="gaussian","MOLECULE.MAIN:read_nucleus_model ... nucleus_model= must be point or gaussian")
   end subroutine


!  O

   subroutine read_output_style_options()
   ! Read new output style options, for example change the
   ! width and number of field, or precision of output.
      call read_keywords_(stdout)
   end subroutine


!  P

   subroutine fit_sph_atom_FFs(self)
   ! For every unique fragment atom, fit its spherical form factor (the
   ! sph- partition model in force) to FF_fit_n_gaussians Gaussians and a
   ! constant on [0,FF_fit_s_max] through gnuplot (GAUSSIAN_FF_FIT), from
   ! the International Tables coefficients of its element; print the
   ! electron count and the fit deviations; and write the coefficients
   ! with their esds to <name>.sph_ff.<FF_fit_format>, one atom per entry,
   ! headed by its label. The fit files of atom X are <name>.X.sph_ff.*.
   ! An atom whose fit fails keeps the International Tables coefficients,
   ! and the entry says so. gnuplot must be installed.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: f,s,a,b,ae,be,Cs,Cse,rmss,maxs
      MAT(REAL,:,:), allocatable :: A_all,B_all,Ae_all,Be_all
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      VEC(BIN,:), allocatable :: fitted
      STR(len=STR_SIZE) :: label,stem,out,fmt,comment,line
      REAL :: c0,ce,s_max,rms,max_dev,n_e
      INT :: n_g,n_u,u,c,i,Z
      BIN :: ok
   ENSURE(allocated(self%atom),"MOLECULE.MAIN:fit_sph_atom_FFs ... no atoms")
   ENSURE(allocated(self%crystal),"MOLECULE.MAIN:fit_sph_atom_FFs ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.MAIN:fit_sph_atom_FFs ... no x-ray data")
      n_g = self%crystal%xray_data%FF_fit_n_gaussians
      s_max = self%crystal%xray_data%FF_fit_s_max
      fmt = self%crystal%xray_data%FF_fit_format
      DIE_IF(n_g/=4,"MOLECULE.MAIN:fit_sph_atom_FFs ... only 4 Gaussians so far: the start comes from the International Tables")
      call execute_(tonto,"command -v gnuplot > /dev/null 2>&1")
      DIE_IF(command_failed_(tonto),"MOLECULE.MAIN:fit_sph_atom_FFs ... fit_sph_atom_FFs needs gnuplot, which is not on the PATH")
      call make_atom_partition_info_(self)
      call make_r_density_mx_(self)
      out = trim(self%name)//".sph_ff."//trim(fmt)
      n_u = self%crystal%n_unique_frag_atoms
      call flush_(stdout)
      call text_(stdout,"==========================================")
      call text_(stdout,"Spherical form factors fitted to Gaussians")
      call text_(stdout,"==========================================")
      call flush_(stdout)
      call show_(stdout,"Partition model           =",self%crystal%xray_data%partition_model)
      call show_(stdout,"Fit range, s_max          =",s_max)
      call show_(stdout,"No. of Gaussians          =",n_g)
      call show_(stdout,"No. of s points           =",self%crystal%xray_data%FF_fit_n_points)
      call show_(stdout,"Coefficients written to   =",out)
      call flush_(stdout)
      call create_(a,n_g); call create_(b,n_g); call create_(ae,n_g); call create_(be,n_g)
      call create_(A_all,n_u,n_g); call create_(B_all,n_u,n_g); call create_(Ae_all,n_u,n_g); call create_(Be_all,n_u,n_g)
      call create_(Cs,n_u); call create_(Cse,n_u); call create_(rmss,n_u); call create_(maxs,n_u)
      call create_(labels,n_u); call create_(fitted,n_u)
      do u = 1,n_u
         c = self%crystal%unique_frag_atom(u)
         label = self%atom(c)%label
         Z = self%atom(c)%atomic_number
         stem = trim(self%name)//"."//trim(label)//".sph_ff"

         ! The curve, and its electron count
         call make_sph_atom_FF_curve_(self,f,s,c)
         n_e = f(1)

         ! Start: the International Tables coefficients of the element
         call IT_form_factor_coeffs_(Z,a,b,c0)
         ae = ZERO; be = ZERO; ce = ZERO
         call fit_(s,f,a,b,c0,s_max,stem,ok,ae,be,ce)
         call deviations_(s,f,a,b,c0,s_max,rms,max_dev)

         ! The coefficients themselves are in the output file: a degenerate
         ! pair of Gaussians can split differently from one machine to the
         ! next, and the fit's quality is what is worth comparing here.
         call flush_(stdout)
         call text_(stdout,"Atom "//trim(label))
         call show_(stdout,"Electrons, f(0)           =",n_e)
         call show_(stdout,"Fitted f(0) = sum a + c   =",sum(a)+c0)
         call show_(stdout,"Fit succeeded?            =",ok)
         call show_(stdout,"rms deviation             =",rms)
         call show_(stdout,"largest deviation         =",max_dev)
         if (NOT ok) call text_(stdout,"The International Tables coefficients are kept for this atom.")
         call flush_(stdout)
         A_all(u,:) = a; Ae_all(u,:) = ae
         B_all(u,:) = b; Be_all(u,:) = be
         Cs(u) = c0; Cse(u) = ce
         rmss(u) = rms; maxs(u) = max_dev
         labels(u) = label
         fitted(u) = ok
         call destroy_(s)
         call destroy_(f)
      end do

      ! The output file, one entry per atom: a comment with the atom, the
      ! fit's quality and the esds, then the coefficients in the format
      call redirect_(stdout,out)
      if (fmt=="table") call text_(stdout,"# label n  a_1..a_n  b_1..b_n  c")
      do u = 1,n_u
         a = A_all(u,:)
         b = B_all(u,:)
         c = self%crystal%unique_frag_atom(u)
         comment = trim(labels(u))//" Z="//trim(to_str_(self%atom(c)%atomic_number))//" "//trim(self%crystal%xray_data%partition_model)//" fit to s_max="//trim(adjustl(to_str_(s_max,"f6.3")))//" from "//trim(self%name)
         if (NOT fitted(u)) comment = trim(comment)//" FIT FAILED: International Tables coefficients"
         line = "rms "//trim(adjustl(to_str_(rmss(u),"f8.4")))//" max "//trim(adjustl(to_str_(maxs(u),"f8.4")))//" e; esd(a) "
         do i = 1,n_g
            line = trim(line)//" "//trim(adjustl(to_str_(Ae_all(u,i),"es9.2")))
         end do
         line = trim(line)//"; esd(b) "
         do i = 1,n_g
            line = trim(line)//" "//trim(adjustl(to_str_(Be_all(u,i),"es9.2")))
         end do
         line = trim(line)//"; esd(c) "//trim(adjustl(to_str_(Cse(u),"es9.2")))
         select case (fmt)
         case ("shelx"); call text_(stdout,"REM "//trim(line))
            call put_SHELX_SFAC_(labels(u),a,b,Cs(u),comment)
         case ("table"); call text_(stdout,"# "//trim(comment))
            call text_(stdout,"# "//trim(line))
            call put_table_line_(labels(u),a,b,Cs(u))
         end select
      end do
      call revert_(stdout)
      call destroy_(fitted); call destroy_(labels)
      call destroy_(maxs); call destroy_(rmss); call destroy_(Cse); call destroy_(Cs)
      call destroy_(Be_all); call destroy_(Ae_all); call destroy_(B_all); call destroy_(A_all)
      call destroy_(be); call destroy_(ae)
      call destroy_(b); call destroy_(a)
   end subroutine

   subroutine put_RI_FF_check(self)
   ! For each unique fragment atom, compare the Hirshfeld atom form factors
   ! from the sum over its grid points with those from its oc-ri fit, by
   ! sin(theta)/lambda shell, and time both. Uses RI_l_max= and
   ! RI_exponent_ratio= whatever the partition model. Also checks the fit
   ! two ways: its residual in real space, |rho_a - fit|/|rho_a| on the
   ! grid; and its analytic transform against a sum of it over the grid.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      MAT(REAL,:,:), allocatable :: k_pts,pt
      VEC(REAL,:), allocatable :: rho,stl,alpha,d2,f2,wt,wa,rt
      VEC(CPX,:), allocatable :: fq,fr,fg
      VEC(STR(len=STR_SIZE),:), allocatable :: label
      VEC(INT,:), allocatable :: n_pt,n_al
      VEC(REAL,:), allocatable :: rms_f,rms_d,max_d,t_sum,t_fit,res_r,chk_t
      VEC(REAL,7) :: edge
      VEC(INT,6) :: b_n
      VEC(REAL,6) :: b_f2,b_d2,b_max
      VEC(REAL,:), allocatable :: b_lo,b_hi,b_rf,b_rd,b_rel
      REAL :: t0,t1
      INT :: n_u,n_k,u,c,k,b,l_max
   ENSURE(allocated(self%atom),"MOLECULE.MAIN:put_RI_FF_check ... no atoms")
   ENSURE(allocated(self%density_mx),"MOLECULE.MAIN:put_RI_FF_check ... no density matrix")
   ENSURE(allocated(self%crystal),"MOLECULE.MAIN:put_RI_FF_check ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.MAIN:put_RI_FF_check ... no reflection data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.MAIN:put_RI_FF_check ... no crystal fragment info")
      l_max = self%crystal%xray_data%RI_l_max
      call set_indices_(l_max)
      call make_atom_partition_info_(self)
      call make_r_density_mx_(self)

      ! k points, and sin(theta)/lambda in 1/Angstrom
      n_k = n_unique_SF_k_pts_(self%crystal)
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)
      call create_(stl,n_k)
      stl = sqrt(k_pts(:,1)**2+k_pts(:,2)**2+k_pts(:,3)**2)/(FOUR*PI)*BOHR_PER_ANGSTROM

      ! Resolution shells
      edge = [ZERO,0.3d0,0.5d0,0.7d0,0.9d0,1.1d0,huge(ONE)]
      b_n = 0; b_f2 = ZERO; b_d2 = ZERO; b_max = ZERO
      n_u = self%crystal%n_unique_frag_atoms
      call create_(label,n_u); call create_(n_pt,n_u); call create_(n_al,n_u)
      call create_(rms_f,n_u); call create_(rms_d,n_u); call create_(max_d,n_u)
      call create_(t_sum,n_u); call create_(t_fit,n_u); call create_(res_r,n_u); call create_(chk_t,n_u)
      call create_(fq,n_k); call create_(fr,n_k); call create_(fg,n_k); call create_(d2,n_k); call create_(f2,n_k)
      do u = 1,n_u
         c = self%crystal%unique_frag_atom(u)
         label(u) = self%atom(c)%label

         ! Grid weights, stockholder weights and density, kept apart
         call make_atom_grid_(self%becke_grid,pt,wt,c)
         call create_(wa,size(pt,1))
         call make_stockholder_atom_weight_(self,wa,c,pt)
         call create_(rt,size(pt,1))
         call make_ED_grid_r_v2_(self,rt,pt,self%density_mx%r,self%becke_grid%basis_fn_cutoff)
         rt = wa*rt  ! rho_a
         call create_(rho,size(pt,1))
         rho = wt*rt  ! rho_a times the grid weight
         call make_RI_exponents_(self,alpha,c)
         n_pt(u) = size(pt,1)
         n_al(u) = size(alpha)
         t0 = current_cpu_time()
         call exp_ikr_sums_(fq,k_pts,pt,rho)
         t1 = current_cpu_time()
         call fitted_exp_ikr_sums_(fr,k_pts,pt,rho,self%atom(c)%position,alpha,l_max,fit=wa)
         t_fit(u) = current_cpu_time()-t1
         t_sum(u) = t1-t0

         ! wa now holds the fit. Its residual, and its transform by a sum
         res_r(u) = sqrt(sum(wt*(rt-wa)**2)/sum(wt*rt**2))
         wa = wt*wa
         call exp_ikr_sums_(fg,k_pts,pt,wa)
         chk_t(u) = sqrt(sum(abs(fr-fg)**2)/sum(abs(fg)**2))
         d2 = abs(fr-fq)**2
         f2 = abs(fq)**2
         rms_f(u) = sqrt(sum(f2)/n_k)
         rms_d(u) = sqrt(sum(d2)/n_k)
         max_d(u) = sqrt(maxval(d2))
         do k = 1,n_k
            do b = 1,6
               if (stl(k)>=edge(b) AND stl(k)<edge(b+1)) exit
            end do
            b_n(b) = b_n(b)+1
            b_f2(b) = b_f2(b)+f2(k)
            b_d2(b) = b_d2(b)+d2(k)
            b_max(b) = max(b_max(b),sqrt(d2(k)))
         end do
         call destroy_(alpha)
         call destroy_(rho)
         call destroy_(rt)
         call destroy_(wa)
         call destroy_(wt)
         call destroy_(pt)
      end do
      call flush_(stdout)
      call text_(stdout,"====================================================")
      call text_(stdout,"Hirshfeld atom form factors: fit (oc-ri) against sum")
      call text_(stdout,"====================================================")
      call flush_(stdout)
      call show_(stdout,"RI_l_max                    =",l_max)
      call show_(stdout,"RI_exponent_ratio           =",self%crystal%xray_data%RI_exponent_ratio)
      call show_(stdout,"Number of k points          =",n_k)
      call show_(stdout,"CPU time, sum over points   =",sum(t_sum))
      call show_(stdout,"CPU time, fit               =",sum(t_fit))
      call flush_(stdout)
      call text_(stdout,". |f| and |df| in electrons; df = fit - sum. Times in s.")
      call text_(stdout,". res: |rho_a - fit|/|rho_a| on the grid. ft chk: rms")
      call text_(stdout,"  relative difference, analytic transform of the fit")
      call text_(stdout,"  against its sum over the grid.")
      call flush_(stdout)
      call create_(table,11)
      call set_heading_(table(1),"Atom"); call set_values_(table(1),label)
      call set_heading_(table(2),"n_pt"); call set_values_(table(2),n_pt)
      call set_heading_(table(3),"n_exp"); call set_values_(table(3),n_al)
      call set_heading_(table(4),"rms |f|"); call set_values_(table(4),rms_f)
      call set_heading_(table(5),"rms |df|"); call set_values_(table(5),rms_d)
      call set_heading_(table(6),"max |df|"); call set_values_(table(6),max_d)
      call set_heading_(table(7),"rms/rms"); call set_values_(table(7),rms_d/rms_f)
      call set_heading_(table(8),"t sum"); call set_values_(table(8),t_sum)
      call set_heading_(table(9),"t fit"); call set_values_(table(9),t_fit)
      call set_heading_(table(10),"res"); call set_values_(table(10),res_r)
      call set_heading_(table(11),"ft chk"); call set_values_(table(11),chk_t)
      call put_(table,label_rows=FALSE)
      call destroy_(table)
      call flush_(stdout)
      call text_(stdout,". By sin(theta)/lambda shell, all atoms, in 1/Angstrom.")
      call flush_(stdout)
      b_lo = edge(1:6)
      b_hi = edge(2:7)
      b_hi(6) = maxval(stl)
      b_rf = sqrt(b_f2/max(b_n,1))
      b_rd = sqrt(b_d2/max(b_n,1))
      b_rel = b_rd/max(b_rf,tiny(ONE))
      call create_(table,7)
      call set_heading_(table(1),"stl from"); call set_values_(table(1),b_lo)
      call set_heading_(table(2),"stl to"); call set_values_(table(2),b_hi)
      call set_heading_(table(3),"n"); call set_values_(table(3),b_n)
      call set_heading_(table(4),"rms |f|"); call set_values_(table(4),b_rf)
      call set_heading_(table(5),"rms |df|"); call set_values_(table(5),b_rd)
      call set_heading_(table(6),"max |df|"); call set_values_(table(6),b_max)
      call set_heading_(table(7),"rms/rms"); call set_values_(table(7),b_rel)
      call put_(table,label_rows=FALSE)
      call destroy_(table)
      call destroy_(f2); call destroy_(d2); call destroy_(fg); call destroy_(fr); call destroy_(fq)
      call destroy_(chk_t); call destroy_(res_r)
      call destroy_(t_fit); call destroy_(t_sum); call destroy_(max_d); call destroy_(rms_d); call destroy_(rms_f)
      call destroy_(n_al); call destroy_(n_pt); call destroy_(label)
      call destroy_(stl); call destroy_(k_pts)
   end subroutine

   subroutine put_Hirshfeld_test(self)
   ! Set pair-energy model
      type(MOLECULE_TYPE), IN :: self
   DIE_IF(NOT allocated(self%atom),"MOLECULE.MAIN:put_Hirshfeld_test ... there are no atoms!")
      call put_Hirshfeld_test_(self%atom)
   end subroutine

   subroutine read_phi_step_size(self)
   ! Read in the dimensionless normal coordinate step size.
      type(MOLECULE_TYPE), INOUT :: self
      call read_(stdin,self%phi_step_size)
      WARN_IF(self%phi_step_size<TOL(2),"MOLECULE.MAIN:read_phi_step_size ... phi_step_size may be too small")
   end subroutine

   subroutine read_plot_grid(self)
   ! Read in the plot grid data
      type(MOLECULE_TYPE), INOUT :: self

      ! Set up a defualt grid
      if (NOT allocated(self%plot_grid)) call create_(self%plot_grid,self%atom)

      ! Read options
      call read_keywords_(self%plot_grid)
      if (self%plot_grid%use_unit_cell_as_bbox) then
         DIE_IF(NOT allocated(self%crystal),"MOLECULE.MAIN:read_plot_grid ... unit_cell_as_bbox set, but no crystal= info given! Fair go!")
         call set_bbox_from_unit_cell_(self%plot_grid,self%crystal%unit_cell,midpt_is_center=FALSE)
      end if
      DIE_IF(self%plot_grid%plot_format==" ","MOLECULE.MAIN:read_plot_grid ... plot_format not specified (try all)")
   end subroutine

   subroutine plot_uc_density(self)
   ! Plot the unit cell density
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.MAIN:plot_uc_density ... no atom info")
   ENSURE(allocated(self%crystal),"MOLECULE.MAIN:plot_uc_density ... no crystal info")
   ENSURE(NOT self%crystal%cluster_info_made,"MOLECULE.MAIN:plot_uc_density ... cluster info already made!")
      call make_ANOs_and_interpolators_(self)
      call destroy_fragment_data_(self%crystal)
      call update_fragment_info_(self%crystal,self%atom)
      call set_generation_method_(self%crystal,"for_unit_cell_density")
      call set_atom_density_cutoff_(self%crystal,1.0d-3)
      call make_cluster_info_(self%crystal)
      call put_cluster_info_(self%crystal)
   end subroutine

   subroutine read_pointgroup(self)
   ! Read in the pointgroup data
      type(MOLECULE_TYPE), INOUT :: self
      if (NOT allocated(self%pointgroup)) call create_(self%pointgroup)
      call read_keywords_(self%pointgroup)
   end subroutine


!  R

   subroutine read_robydata(self)
   ! Read in the Roby data. NOTE: this keyword must appear after a basis set
   ! has been defined
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(self%basis_info_made,"MOLECULE.MAIN:read_robydata ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.MAIN:read_robydata ... no atom list")

      ! Make the Becke grid
      if (NOT allocated(self%becke_grid)) call create_(self%becke_grid)
      call set_atom_info_(self%becke_grid,self%atom)
      call set_grid_data_(self%becke_grid)
      call make_overlap_mx_(self)
      call make_core_mx_(self)
   ! if (.SCF_data.allocated) then
   !    .make_scf_Fock_mx
   ! end
      if (allocated(self%roby)) call destroy_(self%roby)
      call create_(self%roby,self)
      call read_keywords_(self%roby)
      call update_(self%roby)
   end subroutine

   subroutine read_rotate(self)
   ! Rotate the whole molecule using matrix defined by its rotation
   ! "axis" and the "angle" of rotation around that axis, which are
   ! both inputted.  Checked, 7/2/06; it needed the archives to be
   ! deleted to work
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,3,3) :: R
      VEC(REAL,3) :: axis
      REAL :: angle

      ! Read axis
      call read_(stdin,axis)
      call read_(stdin,angle)
      ENSURE(NOT is_zero_(axis),"MOLECULE.MAIN:read_rotate ... axis must not be zero")

      ! Delete archives
      call delete_SCF_archives_(self)

      ! Get the rotation matrix from the axis
      call to_3x3_rotation_mx_(R,axis,angle)

      ! Rotate
      call rotate_by_(self,R)

      ! Remake the Becke grid (leaky)
      if (allocated(self%becke_grid)) then
         call set_atom_info_(self%becke_grid,self%atom)
         call set_grid_data_(self%becke_grid)
      end if
   end subroutine

   subroutine read_rotate_group(self)
   ! Rotate a group "g" of atoms in the molecule using a matrix
   ! defined by its rotation "axis" and the "angle" of rotation around
   ! that axis, which are all inputted on the command line. The
   ! rotation is performed on the coordinates and the group-block
   ! diagonal of the density matrix. NOTE: this can be a problem for
   ! group off-diagonal blocks; this routine works for progroup
   ! densities only.
   ! Checked, 7/2/06; the energy decomposition was checked by manual
   ! rotation
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,3) :: axis
      REAL :: angle
      INT :: g
   ENSURE(allocated(self%atom_group),"MOLECULE.MAIN:read_rotate_group ... no atom group information")
      call read_(stdin,g)  ! read group
      call read_(stdin,axis)  ! read axis
      call read_quantity_(stdin,angle)  ! read angle
      ENSURE(g>0,"MOLECULE.MAIN:read_rotate_group ... group number must be positive")
      ENSURE(g<=size(self%atom_group),"MOLECULE.MAIN:read_rotate_group ... group number exceeds number of groups")
      ENSURE(NOT is_zero_(axis),"MOLECULE.MAIN:read_rotate_group ... axis must not be zero")
      call rotate_group_(self,g,axis,angle)
   end subroutine

   subroutine read_rotate_mx(self)
   ! Same as read_rotate except reads the rotation matrix from 
   ! the stdin file
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,3,3) :: R

      ! Read matrix
      call read_(stdin,R)

      ! Delete archives
      call delete_SCF_archives_(self)

      ! Rotate
      call rotate_by_(self,R)

      ! Remake the Becke grid (leaky)
      if (allocated(self%becke_grid)) then
         call set_atom_info_(self%becke_grid,self%atom)
         call set_grid_data_(self%becke_grid)
      end if
   end subroutine


!  S

   subroutine read_saved_basis_name(self)
   ! Read in the *gaussian* basis type to be used for the atoms in the
   ! *saved* molecule.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: this
   ENSURE(allocated(self%saved),"MOLECULE.MAIN:read_saved_basis_name ... there is no saved molecule")
      call read_(stdin,this)
      call set_basis_name_(self%saved,this)
   end subroutine

   subroutine read_saved_coppensbasis_name(self)
   ! Read in the *coppens* basis type to be used for the atoms in the
   ! *saved* molecule.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: this
   ENSURE(allocated(self%saved),"MOLECULE.MAIN:read_saved_coppensbasis_name ... there is no saved molecule")
      call read_(stdin,this)
      call set_coppensbasis_name_(self%saved,this)
   end subroutine

   subroutine read_saved_slaterbasis_name(self)
   ! Read in the *slater* basis type to be used for the atoms in the
   ! *saved* molecule.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: this
   ENSURE(allocated(self%saved),"MOLECULE.MAIN:read_saved_slaterbasis_name ... there is no saved molecule")
      call read_(stdin,this)
      call set_slaterbasis_name_(self%saved,this)
   end subroutine

   subroutine read_SCF_data(self)
   ! Read in the scf data
      type(MOLECULE_TYPE), INOUT :: self
      if (NOT allocated(self%SCF_data)) call create_(self%SCF_data)

      ! Read options
      call read_keywords_(self%SCF_data)

      ! Internal post-process options
      call update_(self%SCF_data)

      ! Make Hirshfeld info
      if (allocated(self%crystal)) then
         if (uses_numerical_FFs_(self%crystal)) then
            if (allocated(self%crystal%xray_data)) then
               call make_ANOs_and_interpolators_(self)
            end if
         end if
      end if

      ! Each atom_group has up-to-date SCF_data options
      ! Do the whole kaboodle again.
      if (allocated(self%atom_group)) then
         call update_atom_basis_group_info_(self)
      end if
   end subroutine


!   read_serialize_filename :: leaky
!   ! Read in the *gaussian* basis type to be used for the atoms.
!      self :: IN
!
!      name :: STR
!
!      stdin.read(name)
!      .serialize(name)
!
!   end

   subroutine read_slaterbasis_name(self)
   ! Read in the *slater* basis type to be used for the atoms.
      type(MOLECULE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: name
      call read_(stdin,name)
      call set_slaterbasis_name_(self,name)

      ! Finalize crystal - remake basis kinds !
   ! if (.crystal.allocated) then
   !    .crystal.destroy_asymmetric_unit
   !    .crystal.update_fragment_info(.atom)
   ! end
   end subroutine

   subroutine read_slaterbasis_sets(self)
   ! Read in a list of atomic Coppens basis sets for the molecule
      type(MOLECULE_TYPE), INOUT :: self
   WARN_IF(allocated(self%slaterbasis),"MOLECULE.MAIN:read_slaterbasis_sets ... slaterbasis sets already defined!")
      if (allocated(self%atom)) call destroy_slaterbasis_part_(self%atom)
      if (allocated(self%slaterbasis)) call renormalize_(self%slaterbasis)
      call read_list_keywords_(self%slaterbasis)
      if (allocated(self%slaterbasis)) call unnormalize_(self%slaterbasis)
      call update_atom_basis_group_info_(self)
   end subroutine

   subroutine read_spin_multiplicity(self)
   ! Read in the spin multiplicity
      type(MOLECULE_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      call set_spin_multiplicity_(self,val)
   end subroutine

   subroutine delta_lambda_plot(self)
   ! Do a plot of the difference between a property
   ! ".plot_grid.plot_kind" using molecular orbitals or density
   ! matrices for .SCF_data.lambda_max and .SCF_data.initial_lambda, 
   ! and write out the file.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: gr1,gr2
      STR(len=STR_SIZE) :: nm1,nm2
      STR(len=STR_SIZE) :: kind
   ENSURE(allocated(self%plot_grid),"MOLECULE.MAIN:delta_lambda_plot ... no plot_grid!")
   ENSURE(allocated(self%SCF_data),"MOLECULE.MAIN:delta_lambda_plot ... no SCF_data!")

      ! Get lambda values
      nm1 = "MOs,lambda="//trim(to_str_(self%SCF_data%lambda_max,"f9.6"))
      nm2 = "MOs,lambda="//trim(to_str_(self%SCF_data%initial_lambda,"f9.6"))
      kind = spinorbital_genre_(self%SCF_data)

      ! Property values.
      call create_(gr1,self%plot_grid%n_pt)
      call create_(gr2,self%plot_grid%n_pt)

      ! Read the lambda_max
      call read_archive_(self,nm1,kind)
      call assign_NOs_to_MOs_(self)
      call make_SCF_density_mx_(self)

      ! Plot max
      call do_plot_(self,gr1)

      ! Read the initial_lambd
      call read_archive_(self,nm2,kind)
      call assign_NOs_to_MOs_(self)
      call make_SCF_density_mx_(self)

      ! Plot initial_lamba
      call do_plot_(self,gr2)

      ! Subtract
      gr1 = gr1-gr2

      ! Print out
      call dump_plot_grid_(self,gr1,"subtracted_plot")

      ! Clean
      call destroy_(gr2)
      call destroy_(gr1)
   end subroutine


!  T

   subroutine read_table_column()
   ! Read in default settings for table_column output
      call create_std_table_column_
      call read_keywords_(std_table_column)
   end subroutine

   subroutine read_TD_data(self)
   ! Read in the time-dependent data
      type(MOLECULE_TYPE), INOUT :: self
      if (allocated(self%atom)) then
         call set_defaults_(self%TD_data,self%n_bf,self%n_a,self%n_b)
         if (allocated(self%MOs)) then
            if (allocated(self%MOs%r)) then
               if (allocated(self%SCF_data)) then
                  call set_defaults_(self%TD_data,self%MOs%r,self%MO_energies%r,self%SCF_data%VMO_energies)
               else
                  call set_defaults_(self%TD_data,self%MOs%r,self%MO_energies%r)
               end if
            end if
         end if
      end if

      ! Read options
      call read_keywords_(self%TD_data)
      DIE_IF(self%TD_data%method==" ","MOLECULE.MAIN:read_TD_data ... No method specified")
      DIE_IF(self%TD_data%implementation==" ","MOLECULE.MAIN:read_TD_data ... No implementation specified")
   end subroutine


!  U

   subroutine read_use_interpolators(self)
   ! Read whether to use interpolators
      type(MOLECULE_TYPE), INOUT :: self
      call read_(stdin,self%use_interpolators)
      if (allocated(self%atom)) call set_use_interpolators_(self%atom,self%use_interpolators)
   end subroutine

   subroutine read_use_CRYSTAL23_order(self)
   ! Read whether to use CRYSTAL23 spherical basis
   ! function order which hasw P orbitals as px, py, pz.
      type(MOLECULE_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      self%use_CRYSTAL23_order = val
      if (self%use_CRYSTAL23_order) then
         call destroy_data_
         call set_CRYSTAL23_ordering_(TRUE)
         call set_indices_(GAUSSIAN_DATA_L_MAX)
      end if
      if (allocated(self%atom)) then
         call update_atom_basis_group_info_(self)
      end if
   end subroutine

   subroutine read_use_spherical_basis(self)
   ! Read whether to use a spherical basis
   ! NOTE: must be set before the basis_name or .atom bases are specified.
   ! That precondition is now ENFORCED. It used to be merely documented:
   ! the bases are resolved when the atoms are read (MOLECULE.READ, ".basis
   ! .set_spherical(TRUE)"), so setting this keyword after the "atoms=" block
   ! had no effect at all -- accepted, exit 0, no diagnostic. Measured
   ! 2026-08-13 on h2o/cc-pVDZ: before "atoms=" gives 24 basis functions
   ! (spherical), after it gives 25 (cartesian) and an energy 1.6e-3 Hartree
   ! away -- about 1 kcal/mol -- with nothing said. The bottom of a job file
   ! is a natural place to put a switch like this.
   ! Dying is preferred to re-resolving the basis here: silently rebuilding
   ! a basis that other objects may already have been built against would
   ! trade a quiet wrong answer for a harder one.
      type(MOLECULE_TYPE), INOUT :: self
      call read_(stdin,self%use_spherical_basis)

      ! NOTE: one line. A Fortran string continuation ("... &" / "& ...") does
      ! NOT survive inside a DIE/ENSURE macro argument -- the macro is expanded
      ! by the C preprocessor after translation, so the continuation is never
      ! joined and the "&" and its padding appear verbatim in the message.
      DIE_IF(allocated(self%atom),"MOLECULE.MAIN:read_use_spherical_basis ... use_spherical_basis= must be set BEFORE the atoms= block; the bases are already resolved by then")
   end subroutine

end module
