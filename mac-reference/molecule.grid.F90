!==================================================================
!
! MOLECULE.GRID: 
! 
! An object representation of a molecule, plotting procedures.
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1996-2012
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
!==================================================================
module MOLECULE_GRID_MODULE

#  include "macros"

   include "molecule.grid.use"

   implicit none

   include "molecule.grid.int"


contains

   subroutine do_isosurface_plot(self)
   ! Do one of the many kinds of isosurface plot calculations
   ! NOTE: cubify_m passes MOLECULE "self" !
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%isosurface),"MOLECULE.GRID:do_isosurface_plot ... no isosurface")
      select case (self%isosurface%property)
      case ("adp                              "); call cubify_m_(self%isosurface,self,make_ADP_exponent_grid)
      case ("adp2                             "); call cubify_m_(self%isosurface,self,make_ADP2_exponent_grid)
      case ("anharmonic_adp_correction        "); call cubify_m_(self%isosurface,self,make_AADP_correction_grid)
   ! case("bond_density_atom                "); .isosurface.cubify_m(self,::make_bond_density_atom_grid)
   ! case("bond_electron_density            "); .isosurface.cubify_m(self,::make_bond_ED_grid)
   ! case("bond_weight                      "); .isosurface.cubify_m(self,::make_bond_weight_grid)
   ! case("deformation_bond_density         "); .isosurface.cubify_m(self,::make_deformation_bond_density_grid)
      case ("deformation_density              "); call cubify_m_(self%isosurface,self,make_def_ED_grid)
      case ("deformation_ed                   "); call cubify_m_(self%isosurface,self,make_def_ED_grid)
      case ("deformation_electron_density     "); call cubify_m_(self%isosurface,self,make_def_ED_grid)
      case ("deformation_dft_eff_potential    "); call cubify_m_(self%isosurface,self,make_def_DFT_eff_pot_grid)
      case ("deformation_dft_energy           "); call cubify_m_(self%isosurface,self,make_def_DFT_energy_grid)
      case ("deformation_dft_xc_potential     "); call cubify_m_(self%isosurface,self,make_def_DFT_xc_pot_grid)
      case ("deformation_electric_potential   "); call cubify_m_(self%isosurface,self,make_def_e_pot_grid)
      case ("deformation_g_kinetic_energy     "); call cubify_m_(self%isosurface,self,make_def_G_kinetic_grid)
      case ("deformation_h_dft_energy         "); call cubify_m_(self%isosurface,self,make_def_H_DFT_energy_grid)
      case ("deformation_k_kinetic_energy     "); call cubify_m_(self%isosurface,self,make_def_K_kinetic_grid)
      case ("deformation_laplacian            "); call cubify_m_(self%isosurface,self,make_def_laplacian_grid)
      case ("dft_eff_potential                "); call cubify_m_(self%isosurface,self,make_DFT_eff_pot_grid)
      case ("dft_energy                       "); call cubify_m_(self%isosurface,self,make_DFT_energy_grid)
      case ("dft_xc_potential                 "); call cubify_m_(self%isosurface,self,make_DFT_xc_pot_grid)
      case ("e_field_magnitude                "); call cubify_m_(self%isosurface,self,make_E_field_magnitude)
      case ("electric_potential               "); call cubify_m_(self%isosurface,self,make_EP_grid)
      case ("electron_density                 "); call cubify_m_(self%isosurface,self,make_ED_grid)
      case ("elf                              "); call cubify_m_(self%isosurface,self,make_ELF_grid)
      case ("eli_d                            "); call cubify_m_(self%isosurface,self,make_ELI_D_grid)
      case ("fermi_mobility                   "); call cubify_m_(self%isosurface,self,make_fermi_mobility_grid)
      case ("grad_rho_on_rho                  "); call cubify_m_(self%isosurface,self,make_grad_rho_on_rho_grid)
      case ("g_kinetic_energy                 "); call cubify_m_(self%isosurface,self,make_G_kinetic_energy_grid)
      case ("h_dft_energy                     "); call cubify_m_(self%isosurface,self,make_H_DFT_energy_grid)
      case ("hirshfeld_density                "); call cubify_m_(self%isosurface,self,make_Hirshfeld_density_grid)
      case ("hirshfeld_weight                 "); call cubify_m_(self%isosurface,self,make_Hirshfeld_density_grid)
      case ("k_kinetic_energy                 "); call cubify_m_(self%isosurface,self,make_K_kinetic_energy_grid)
      case ("laplacian                        "); call cubify_m_(self%isosurface,self,make_laplacian_ED_grid)
      case ("local_ionisation_energy          "); call cubify_m_(self%isosurface,self,make_local_ionisation_grid)
      case ("lol                              "); call cubify_m_(self%isosurface,self,make_LOL_grid)
      case ("orbital                          "); call cubify_m_(self%isosurface,self,make_orbital_grid)
   ! case("oscillator_orbital               "); .isosurface.cubify_m(self,::make_oscillator_orbital_grid)
      case ("orbital_density                  "); call cubify_m_(self%isosurface,self,make_orbital_density_grid)
      case ("promolecule_density              "); call cubify_m_(self%isosurface,self,make_promolecule_density_grid)
   ! case("roby_ano_density                 "); .isosurface.cubify_m(self,::make_Roby_ANO_density_grid)
   ! case("roby_nao_density                 "); .isosurface.cubify_m(self,::make_Roby_NAO_density_grid)
      case ("roby-gould_covalent_orbital      "); call cubify_m_(self%isosurface,self,make_RG_cov_orbital_grid)
      case ("roby-gould_ionic_orbital         "); call cubify_m_(self%isosurface,self,make_RG_ion_orbital_grid)
      case ("spin_density                     "); call cubify_m_(self%isosurface,self,make_SD_grid)
      case ("stockholder_density              "); call cubify_m_(self%isosurface,self,make_stockholder_density_grid)
      case ("stockholder_weight               "); call cubify_m_(self%isosurface,self,make_Hirshfeld_density_grid)
      case ("test_sphere_on_atom_1            "); call cubify_m_(self%isosurface,self,make_test_sphere_on_atom_1)
      case ("true_fermi_mobility              "); call cubify_m_(self%isosurface,self,make_true_fermi_mobility_grid)
      case ("tsirelson_elf                    "); call cubify_m_(self%isosurface,self,make_Tsirelson_ELF_grid)
      case default; allocate(tonto%known_keywords(43))
      tonto%known_keywords(1) = "adp                              "
      tonto%known_keywords(2) = "adp2                             "
      tonto%known_keywords(3) = "anharmonic_adp_correction        "
      tonto%known_keywords(4) = "deformation_density              "
      tonto%known_keywords(5) = "deformation_ed                   "
      tonto%known_keywords(6) = "deformation_electron_density     "
      tonto%known_keywords(7) = "deformation_dft_eff_potential    "
      tonto%known_keywords(8) = "deformation_dft_energy           "
      tonto%known_keywords(9) = "deformation_dft_xc_potential     "
      tonto%known_keywords(10) = "deformation_electric_potential   "
      tonto%known_keywords(11) = "deformation_g_kinetic_energy     "
      tonto%known_keywords(12) = "deformation_h_dft_energy         "
      tonto%known_keywords(13) = "deformation_k_kinetic_energy     "
      tonto%known_keywords(14) = "deformation_laplacian            "
      tonto%known_keywords(15) = "dft_eff_potential                "
      tonto%known_keywords(16) = "dft_energy                       "
      tonto%known_keywords(17) = "dft_xc_potential                 "
      tonto%known_keywords(18) = "e_field_magnitude                "
      tonto%known_keywords(19) = "electric_potential               "
      tonto%known_keywords(20) = "electron_density                 "
      tonto%known_keywords(21) = "elf                              "
      tonto%known_keywords(22) = "eli_d                            "
      tonto%known_keywords(23) = "fermi_mobility                   "
      tonto%known_keywords(24) = "grad_rho_on_rho                  "
      tonto%known_keywords(25) = "g_kinetic_energy                 "
      tonto%known_keywords(26) = "h_dft_energy                     "
      tonto%known_keywords(27) = "hirshfeld_density                "
      tonto%known_keywords(28) = "hirshfeld_weight                 "
      tonto%known_keywords(29) = "k_kinetic_energy                 "
      tonto%known_keywords(30) = "laplacian                        "
      tonto%known_keywords(31) = "local_ionisation_energy          "
      tonto%known_keywords(32) = "lol                              "
      tonto%known_keywords(33) = "orbital                          "
      tonto%known_keywords(34) = "orbital_density                  "
      tonto%known_keywords(35) = "promolecule_density              "
      tonto%known_keywords(36) = "roby-gould_covalent_orbital      "
      tonto%known_keywords(37) = "roby-gould_ionic_orbital         "
      tonto%known_keywords(38) = "spin_density                     "
      tonto%known_keywords(39) = "stockholder_density              "
      tonto%known_keywords(40) = "stockholder_weight               "
      tonto%known_keywords(41) = "test_sphere_on_atom_1            "
      tonto%known_keywords(42) = "true_fermi_mobility              "
      tonto%known_keywords(43) = "tsirelson_elf                    "
      call unknown_(tonto,self%isosurface%property,"MOLECULE.GRID:do_isosurface_plot")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine do_plot_property_on_isosurface(self)
   ! Plot one of the many kinds of density functions on an existing
   ! isosurface --- useful for mapping properties on a surface.
   ! NOTE: plot_function_m passes MOLECULE "self" !
      type(MOLECULE_TYPE), INOUT :: self
      select case (self%isosurface%surface_property)
      case ("adp                             "); call plot_function_m_(self%isosurface,self,make_ADP_exponent_grid)
      case ("adp2                            "); call plot_function_m_(self%isosurface,self,make_ADP2_exponent_grid)
      case ("anharmonic_adp_correction       "); call plot_function_m_(self%isosurface,self,make_AADP_correction_grid)
   ! case("bond_density_atom               "); .isosurface.plot_function_m(self,::make_bond_density_atom_grid)
   ! case("bond_electron_density           "); .isosurface.plot_function_m(self,::make_bond_ED_grid)
   ! case("deformation_bond_density        "); .isosurface.plot_function_m(self,::make_deformation_bond_density_grid)
      case ("deformation_density             "); call plot_function_m_(self%isosurface,self,make_def_ED_grid)
      case ("deformation_ed                  "); call plot_function_m_(self%isosurface,self,make_def_ED_grid)
      case ("deformation_electron_density    "); call plot_function_m_(self%isosurface,self,make_def_ED_grid)
      case ("deformation_dft_eff_potential   "); call plot_function_m_(self%isosurface,self,make_def_DFT_eff_pot_grid)
      case ("deformation_dft_energy          "); call plot_function_m_(self%isosurface,self,make_def_DFT_energy_grid)
      case ("deformation_dft_xc_potential    "); call plot_function_m_(self%isosurface,self,make_def_DFT_xc_pot_grid)
      case ("deformation_electric_potential  "); call plot_function_m_(self%isosurface,self,make_def_e_pot_grid)
      case ("deformation_g_kinetic_energy    "); call plot_function_m_(self%isosurface,self,make_def_G_kinetic_grid)
      case ("deformation_h_dft_energy        "); call plot_function_m_(self%isosurface,self,make_def_H_DFT_energy_grid)
      case ("deformation_k_kinetic_energy    "); call plot_function_m_(self%isosurface,self,make_def_K_kinetic_grid)
      case ("deformation_laplacian           "); call plot_function_m_(self%isosurface,self,make_def_laplacian_grid)
      case ("dft_eff_potential               "); call plot_function_m_(self%isosurface,self,make_DFT_eff_pot_grid)
      case ("dft_energy                      "); call plot_function_m_(self%isosurface,self,make_DFT_energy_grid)
      case ("dft_xc_potential                "); call plot_function_m_(self%isosurface,self,make_DFT_xc_pot_grid)
      case ("e_field_magnitude               "); call plot_function_m_(self%isosurface,self,make_E_field_magnitude)
      case ("electric_potential              "); call plot_function_m_(self%isosurface,self,make_EP_grid)
      case ("electron_density                "); call plot_function_m_(self%isosurface,self,make_ED_grid)
      case ("elf                             "); call plot_function_m_(self%isosurface,self,make_ELF_grid)
      case ("eli_d                           "); call plot_function_m_(self%isosurface,self,make_ELI_D_grid)
      case ("fermi_mobility                  "); call plot_function_m_(self%isosurface,self,make_fermi_mobility_grid)
      case ("g_kinetic_energy                "); call plot_function_m_(self%isosurface,self,make_G_kinetic_energy_grid)
      case ("group_product_e_field_grid      "); call plot_function_m_(self%isosurface,self,make_group_product_E_field_grid)
      case ("group_product_esp_grid          "); call plot_function_m_(self%isosurface,self,make_group_product_ESP_grid)
      case ("grad_rho_on_rho                 "); call plot_function_m_(self%isosurface,self,make_grad_rho_on_rho_grid)
      case ("h_dft_energy                    "); call plot_function_m_(self%isosurface,self,make_H_DFT_energy_grid)
      case ("hirshfeld_density               "); call plot_function_m_(self%isosurface,self,make_Hirshfeld_density_grid)
      case ("hirshfeld_weight                "); call plot_function_m_(self%isosurface,self,make_Hirshfeld_density_grid)
      case ("k_kinetic_energy                "); call plot_function_m_(self%isosurface,self,make_K_kinetic_energy_grid)
      case ("laplacian                       "); call plot_function_m_(self%isosurface,self,make_laplacian_ED_grid)
      case ("local_ionisation_energy         "); call plot_function_m_(self%isosurface,self,make_local_ionisation_grid)
      case ("lol                             "); call plot_function_m_(self%isosurface,self,make_LOL_grid)
      case ("orbital                         "); call plot_function_m_(self%isosurface,self,make_orbital_grid)
   ! case("oscillator_orbital              "); .isosurface.plot_function_m(self,::make_oscillator_orbital_grid)
      case ("orbital_density                 "); call plot_function_m_(self%isosurface,self,make_orbital_density_grid)
      case ("promolecule_density             "); call plot_function_m_(self%isosurface,self,make_promolecule_density_grid)
      case ("product_e_field_grid            "); call plot_function_m_(self%isosurface,self,make_product_E_field_grid)
      case ("product_esp_grid                "); call plot_function_m_(self%isosurface,self,make_product_ESP_grid)
   ! case("roby_ano_density                "); .isosurface.plot_function_m(self,::make_Roby_ANO_density_grid)
   ! case("roby_nao_density                "); .isosurface.plot_function_m(self,::make_Roby_NAO_density_grid)
      case ("roby-gould_covalent_orbital     "); call plot_function_m_(self%isosurface,self,make_RG_cov_orbital_grid)
      case ("roby-gould_ionic_orbital        "); call plot_function_m_(self%isosurface,self,make_RG_ion_orbital_grid)
      case ("spin_density                    "); call plot_function_m_(self%isosurface,self,make_SD_grid)
      case ("stockholder_density             "); call plot_function_m_(self%isosurface,self,make_stockholder_density_grid)
      case ("stockholder_weight              "); call plot_function_m_(self%isosurface,self,make_Hirshfeld_density_grid)
      case ("test_sphere_on_atom_1           "); call plot_function_m_(self%isosurface,self,make_test_sphere_on_atom_1)
      case ("true_fermi_mobility             "); call plot_function_m_(self%isosurface,self,make_true_fermi_mobility_grid)
      case ("tsirelson_elf                   "); call plot_function_m_(self%isosurface,self,make_Tsirelson_ELF_grid)
      case default; allocate(tonto%known_keywords(47))
      tonto%known_keywords(1) = "adp                             "
      tonto%known_keywords(2) = "adp2                            "
      tonto%known_keywords(3) = "anharmonic_adp_correction       "
      tonto%known_keywords(4) = "deformation_density             "
      tonto%known_keywords(5) = "deformation_ed                  "
      tonto%known_keywords(6) = "deformation_electron_density    "
      tonto%known_keywords(7) = "deformation_dft_eff_potential   "
      tonto%known_keywords(8) = "deformation_dft_energy          "
      tonto%known_keywords(9) = "deformation_dft_xc_potential    "
      tonto%known_keywords(10) = "deformation_electric_potential  "
      tonto%known_keywords(11) = "deformation_g_kinetic_energy    "
      tonto%known_keywords(12) = "deformation_h_dft_energy        "
      tonto%known_keywords(13) = "deformation_k_kinetic_energy    "
      tonto%known_keywords(14) = "deformation_laplacian           "
      tonto%known_keywords(15) = "dft_eff_potential               "
      tonto%known_keywords(16) = "dft_energy                      "
      tonto%known_keywords(17) = "dft_xc_potential                "
      tonto%known_keywords(18) = "e_field_magnitude               "
      tonto%known_keywords(19) = "electric_potential              "
      tonto%known_keywords(20) = "electron_density                "
      tonto%known_keywords(21) = "elf                             "
      tonto%known_keywords(22) = "eli_d                           "
      tonto%known_keywords(23) = "fermi_mobility                  "
      tonto%known_keywords(24) = "g_kinetic_energy                "
      tonto%known_keywords(25) = "group_product_e_field_grid      "
      tonto%known_keywords(26) = "group_product_esp_grid          "
      tonto%known_keywords(27) = "grad_rho_on_rho                 "
      tonto%known_keywords(28) = "h_dft_energy                    "
      tonto%known_keywords(29) = "hirshfeld_density               "
      tonto%known_keywords(30) = "hirshfeld_weight                "
      tonto%known_keywords(31) = "k_kinetic_energy                "
      tonto%known_keywords(32) = "laplacian                       "
      tonto%known_keywords(33) = "local_ionisation_energy         "
      tonto%known_keywords(34) = "lol                             "
      tonto%known_keywords(35) = "orbital                         "
      tonto%known_keywords(36) = "orbital_density                 "
      tonto%known_keywords(37) = "promolecule_density             "
      tonto%known_keywords(38) = "product_e_field_grid            "
      tonto%known_keywords(39) = "product_esp_grid                "
      tonto%known_keywords(40) = "roby-gould_covalent_orbital     "
      tonto%known_keywords(41) = "roby-gould_ionic_orbital        "
      tonto%known_keywords(42) = "spin_density                    "
      tonto%known_keywords(43) = "stockholder_density             "
      tonto%known_keywords(44) = "stockholder_weight              "
      tonto%known_keywords(45) = "test_sphere_on_atom_1           "
      tonto%known_keywords(46) = "true_fermi_mobility             "
      tonto%known_keywords(47) = "tsirelson_elf                   "
      call unknown_(tonto,self%isosurface%surface_property,"MOLECULE.GRID:do_plot_property_on_isosurface")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine make_prop_values(self,grid,pt)
   ! Make the plot values in "grid" for a given set of points "pt" for
   ! the ".plot.plot_kind" of property. These are usually 2D plot, but
   ! they can be rectilinear volumetric plots, depending on how
   ! .plot_grid was defined by the user.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_prop_values ... no grid")

      ! NOTE: Some of these were commented out during a major change
      ! and need to be reinstated using the new calling method
      ! pattern.

      ! Do the plot
      select case (self%plot_grid%plot_kind)
      case ("adp                                 "); call make_ADP_grid_(self,grid,pt)
      case ("adp2                                "); call make_ADP2_exponent_grid_(self,grid,pt)
      case ("anharmonic_adp_correction           "); call make_AADP_correction_grid_(self,grid,pt)
      case ("b_field                             "); call make_B_field_grid_(self,grid,pt)
   ! case("b_l_field                           "); .make_B_L_field_grid(grid,pt)
   ! case("b_s_field                           "); .make_B_S_field_grid(grid,pt)
      case ("binding_function_wang_peng          "); call make_binding_function_WP_grid_(self,grid,pt)
   ! case("bond_density_atom                   "); .make_bond_density_atom_grid(grid,pt)
   ! case("bond_electron_density               "); .make_bond_ED_grid(grid,pt)
   ! case("bond_weight                         "); .make_bond_weight_grid(grid,pt)
   ! case("current_density                     "); .make_j_grid(grid,pt)
   ! case("deformation_bond_density            "); .make_def_bond_density_grid(grid,pt)
      case ("crystal23_ed_v1                     "); call make_CRYSTAL23_ED_grid_v1_(self,grid,pt)
      case ("crystal23_electron_density          "); call make_CRYSTAL23_ED_grid_v1_(self,grid,pt)
      case ("deformation_density                 "); call make_def_ED_grid_(self,grid,pt)
      case ("deformation_ed                      "); call make_def_ED_grid_(self,grid,pt)
      case ("deformation_electron_density        "); call make_def_ED_grid_(self,grid,pt)
      case ("deformation_dft_eff_potential       "); call make_def_DFT_eff_pot_grid_(self,grid,pt)
      case ("deformation_dft_energy              "); call make_def_DFT_energy_grid_(self,grid,pt)
      case ("deformation_dft_xc_potential        "); call make_def_DFT_xc_pot_grid_(self,grid,pt)
      case ("deformation_electric_potential      "); call make_def_e_pot_grid_(self,grid,pt)
      case ("deformation_g_kinetic_energy        "); call make_def_G_kinetic_grid_(self,grid,pt)
      case ("deformation_g_kirzhnits             "); call make_def_G_Kirzhnits_grid_(self,grid,pt)
      case ("deformation_h_dft_energy            "); call make_def_H_DFT_energy_grid_(self,grid,pt)
      case ("deformation_h_from_virial           "); call make_def_H_from_virial_grid_(self,grid,pt)
      case ("deformation_h_kirzhnits             "); call make_def_H_Kirzhnits_grid_(self,grid,pt)
      case ("deformation_k_kinetic_energy        "); call make_def_K_kinetic_grid_(self,grid,pt)
      case ("deformation_laplacian               "); call make_def_laplacian_grid_(self,grid,pt)
      case ("deformation_v_from_virial           "); call make_def_V_from_virial_grid_(self,grid,pt)
      case ("deformation_v_kirzhnits             "); call make_def_V_Kirzhnits_grid_(self,grid,pt)
      case ("deformation_reduced_g_kinetic_energy"); call make_def_rG_kinetic_grid_(self,grid,pt)
      case ("deformation_reduced_h_from_virial   "); call make_def_rH_from_virial_grid_(self,grid,pt)
      case ("deformation_reduced_k_kinetic_energy"); call make_def_rK_kinetic_grid_(self,grid,pt)
      case ("deformation_reduced_v_from_virial   "); call make_def_rV_from_virial_grid_(self,grid,pt)
      case ("dft_eff_potential                   "); call make_DFT_eff_pot_grid_(self,grid,pt)
      case ("dft_energy                          "); call make_DFT_energy_grid_(self,grid,pt)
      case ("dft_xc_potential                    "); call make_DFT_xc_pot_grid_(self,grid,pt)
   ! case("div_jp                              "); .make_div_jp_grid(grid,pt)
      case ("e_field_magnitude                   "); call make_E_field_magnitude_(self,grid,pt)
      case ("electric_potential                  "); call make_EP_grid_(self,grid,pt)
      case ("electric_polarisation_density       "); call make_E_polarization_grid_(self,grid,pt)
      case ("electric_polarization_density       "); call make_E_polarization_grid_(self,grid,pt)
      case ("electron_density                    "); call make_ED_grid_(self,grid,pt)
      case ("electron_density_v2                 "); call make_ED_grid_r_v2_(self,grid,pt)
      case ("elf                                 "); call make_ELF_grid_(self,grid,pt)
      case ("e_hf_density_from_rho               "); call make_E_hf_dens_from_rho_grid_(self,grid,pt)
      case ("eli_d                               "); call make_ELI_D_grid_(self,grid,pt)
      case ("fermi_mobility                      "); call make_fermi_mobility_grid_(self,grid,pt)
      case ("g_kinetic_energy                    "); call make_G_kinetic_energy_grid_(self,grid,pt)
      case ("g_kirzhnits                         "); call make_G_Kirzhnits_grid_(self,grid,pt)
      case ("grad_rho_on_rho                     "); call make_grad_rho_on_rho_grid_(self,grid,pt)
      case ("h_dft_energy                        "); call make_H_DFT_energy_grid_(self,grid,pt)
      case ("h_kirzhnits                         "); call make_H_Kirzhnits_grid_(self,grid,pt)
      case ("h_from_virial                       "); call make_H_from_virial_grid_(self,grid,pt)
   ! case("hirshfeld_atom_grid                 "); .make_Hirshfeld_atom_grid(grid,pt)
      case ("hirshfeld_density                   "); call make_Hirshfeld_density_grid_(self,grid,pt)
      case ("hirshfeld_weight                    "); call make_Hirshfeld_density_grid_(self,grid,pt)
      case ("interaction_density                 "); call make_interaction_density_grid_(self,grid,pt)
   ! case("j                                   "); .make_j_grid(grid,pt)
   ! case("jd                                  "); .make_jd_grid(grid,pt)
   ! case("jp                                  "); .make_jp_grid(grid,pt)
   ! case("jp_irrotational                     "); .make_irrotational_jp_grid(grid,pt)
   ! case("jp_solenoidal                       "); .make_solenoidal_jp_grid(grid,pt)
   ! case("jayatilaka_atom_grid                "); .make_Jayatilaka_atom_grid(grid,pt)
      case ("k_kinetic_energy                    "); call make_K_kinetic_energy_grid_(self,grid,pt)
      case ("laplacian                           "); call make_laplacian_ED_grid_(self,grid,pt)
      case ("local_ionisation_energy             "); call make_local_ionisation_grid_(self,grid,pt)
      case ("local_virial_ratio                  "); call make_local_virial_ratio_grid_(self,grid,pt)
      case ("lol                                 "); call make_LOL_grid_(self,grid,pt)
      case ("negative_laplacian                  "); call make_neg_laplacian_grid_(self,grid,pt)
      case ("normalized_g_from_virial            "); call make_norm_G_from_virial_grid_(self,grid,pt)
      case ("normalized_h_from_virial            "); call make_norm_H_from_virial_grid_(self,grid,pt)
      case ("normalized_v_from_virial            "); call make_norm_V_from_virial_grid_(self,grid,pt)
   ! case("orbital_current_density             "); .make_orbital_j_grid(grid,pt)
      case ("orbital_density                     "); call make_orbital_density_grid_(self,grid,pt)
   ! case("orbital                             "); .make_orbital_grid(grid,pt)
   ! case("oscillator_orbital                  "); .make_oscillator_orbital_grid(grid,pt)
      case ("promolecule_density                 "); call make_promolecule_density_grid_(self,grid,pt)
      case ("reduced_g_kinetic_energy            "); call make_rG_kinetic_energy_grid_(self,grid,pt)
      case ("reduced_h_from_virial               "); call make_rH_from_virial_grid_(self,grid,pt)
      case ("reduced_k_kinetic_energy            "); call make_rK_kinetic_energy_grid_(self,grid,pt)
      case ("reduced_v_from_virial               "); call make_rV_from_virial_grid_(self,grid,pt)
      case ("angular_hirshfeld                   "); call make_angular_Hirshfeld_grid_(self,grid,pt)
      case ("cell_map                            "); call make_cell_map_grid_(self,grid,pt)
      case ("hirshfeld_amplitude                 "); call make_Hirshfeld_amplitude_grid_(self,grid,pt)
      case ("residual_density                    "); call make_residual_density_grid_(self,grid,pt)
   ! case("roby_ano_density                    "); .make_Roby_ANO_density_grid(grid,pt)
   ! case("roby_nao_density                    "); .make_Roby_NAO_density_grid(grid,pt)
      case ("roby-gould_covalent_orbital         "); call make_RG_cov_orbital_grid_(self,grid,pt)
      case ("roby-gould_ionic_orbital            "); call make_RG_ion_orbital_grid_(self,grid,pt)
   ! case("spin_current_density                "); .make_spin_j_grid(grid,pt)
      case ("spin_density                        "); call make_SD_grid_(self,grid,pt)
      case ("spherical_atom_density              "); call make_spherical_atom_grid_(self,grid,pt)
      case ("stockholder_density                 "); call make_stockholder_density_grid_(self,grid,pt)
      case ("stockholder_weight                  "); call make_Hirshfeld_density_grid_(self,grid,pt)
      case ("test_sphere_on_atom_1               "); call make_test_sphere_on_atom_1_(self,grid,pt)
      case ("true_fermi_mobility                 "); call make_true_fermi_mobility_grid_(self,grid,pt)
      case ("tsirelson_elf                       "); call make_Tsirelson_ELF_grid_(self,grid,pt)
      case ("v_kirzhnits                         "); call make_V_Kirzhnits_grid_(self,grid,pt)
      case ("v_from_virial                       "); call make_V_from_virial_grid_(self,grid,pt)
      case default; allocate(tonto%known_keywords(79))
      tonto%known_keywords(1) = "adp                                 "
      tonto%known_keywords(2) = "adp2                                "
      tonto%known_keywords(3) = "anharmonic_adp_correction           "
      tonto%known_keywords(4) = "b_field                             "
      tonto%known_keywords(5) = "binding_function_wang_peng          "
      tonto%known_keywords(6) = "crystal23_ed_v1                     "
      tonto%known_keywords(7) = "crystal23_electron_density          "
      tonto%known_keywords(8) = "deformation_density                 "
      tonto%known_keywords(9) = "deformation_ed                      "
      tonto%known_keywords(10) = "deformation_electron_density        "
      tonto%known_keywords(11) = "deformation_dft_eff_potential       "
      tonto%known_keywords(12) = "deformation_dft_energy              "
      tonto%known_keywords(13) = "deformation_dft_xc_potential        "
      tonto%known_keywords(14) = "deformation_electric_potential      "
      tonto%known_keywords(15) = "deformation_g_kinetic_energy        "
      tonto%known_keywords(16) = "deformation_g_kirzhnits             "
      tonto%known_keywords(17) = "deformation_h_dft_energy            "
      tonto%known_keywords(18) = "deformation_h_from_virial           "
      tonto%known_keywords(19) = "deformation_h_kirzhnits             "
      tonto%known_keywords(20) = "deformation_k_kinetic_energy        "
      tonto%known_keywords(21) = "deformation_laplacian               "
      tonto%known_keywords(22) = "deformation_v_from_virial           "
      tonto%known_keywords(23) = "deformation_v_kirzhnits             "
      tonto%known_keywords(24) = "deformation_reduced_g_kinetic_energy"
      tonto%known_keywords(25) = "deformation_reduced_h_from_virial   "
      tonto%known_keywords(26) = "deformation_reduced_k_kinetic_energy"
      tonto%known_keywords(27) = "deformation_reduced_v_from_virial   "
      tonto%known_keywords(28) = "dft_eff_potential                   "
      tonto%known_keywords(29) = "dft_energy                          "
      tonto%known_keywords(30) = "dft_xc_potential                    "
      tonto%known_keywords(31) = "e_field_magnitude                   "
      tonto%known_keywords(32) = "electric_potential                  "
      tonto%known_keywords(33) = "electric_polarisation_density       "
      tonto%known_keywords(34) = "electric_polarization_density       "
      tonto%known_keywords(35) = "electron_density                    "
      tonto%known_keywords(36) = "electron_density_v2                 "
      tonto%known_keywords(37) = "elf                                 "
      tonto%known_keywords(38) = "e_hf_density_from_rho               "
      tonto%known_keywords(39) = "eli_d                               "
      tonto%known_keywords(40) = "fermi_mobility                      "
      tonto%known_keywords(41) = "g_kinetic_energy                    "
      tonto%known_keywords(42) = "g_kirzhnits                         "
      tonto%known_keywords(43) = "grad_rho_on_rho                     "
      tonto%known_keywords(44) = "h_dft_energy                        "
      tonto%known_keywords(45) = "h_kirzhnits                         "
      tonto%known_keywords(46) = "h_from_virial                       "
      tonto%known_keywords(47) = "hirshfeld_density                   "
      tonto%known_keywords(48) = "hirshfeld_weight                    "
      tonto%known_keywords(49) = "interaction_density                 "
      tonto%known_keywords(50) = "k_kinetic_energy                    "
      tonto%known_keywords(51) = "laplacian                           "
      tonto%known_keywords(52) = "local_ionisation_energy             "
      tonto%known_keywords(53) = "local_virial_ratio                  "
      tonto%known_keywords(54) = "lol                                 "
      tonto%known_keywords(55) = "negative_laplacian                  "
      tonto%known_keywords(56) = "normalized_g_from_virial            "
      tonto%known_keywords(57) = "normalized_h_from_virial            "
      tonto%known_keywords(58) = "normalized_v_from_virial            "
      tonto%known_keywords(59) = "orbital_density                     "
      tonto%known_keywords(60) = "promolecule_density                 "
      tonto%known_keywords(61) = "reduced_g_kinetic_energy            "
      tonto%known_keywords(62) = "reduced_h_from_virial               "
      tonto%known_keywords(63) = "reduced_k_kinetic_energy            "
      tonto%known_keywords(64) = "reduced_v_from_virial               "
      tonto%known_keywords(65) = "angular_hirshfeld                   "
      tonto%known_keywords(66) = "cell_map                            "
      tonto%known_keywords(67) = "hirshfeld_amplitude                 "
      tonto%known_keywords(68) = "residual_density                    "
      tonto%known_keywords(69) = "roby-gould_covalent_orbital         "
      tonto%known_keywords(70) = "roby-gould_ionic_orbital            "
      tonto%known_keywords(71) = "spin_density                        "
      tonto%known_keywords(72) = "spherical_atom_density              "
      tonto%known_keywords(73) = "stockholder_density                 "
      tonto%known_keywords(74) = "stockholder_weight                  "
      tonto%known_keywords(75) = "test_sphere_on_atom_1               "
      tonto%known_keywords(76) = "true_fermi_mobility                 "
      tonto%known_keywords(77) = "tsirelson_elf                       "
      tonto%known_keywords(78) = "v_kirzhnits                         "
      tonto%known_keywords(79) = "v_from_virial                       "
      call unknown_(tonto,self%plot_grid%plot_kind,"MOLECULE.GRID:make_prop_values")
      deallocate(tonto%known_keywords)
      end select
   end subroutine


!  ===========
!  Test sphere
!  ===========

   PURE subroutine make_test_sphere_on_atom_1(self,f,pt)
   ! Work out the test sphere on atom 1 with value 1 at radius 1 bohr
   ! increasing linearly with distance. Can use to check units if you
   ! know bond distances. Results in "f" for points "pts".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: f
      MAT(REAL,:,:), IN :: pt
      INT :: i
      VEC(REAL,3) :: pos,r
   ENSURE(self%atom_info_made,"MOLECULE.GRID:make_test_sphere_on_atom_1 ... no atoms")
      pos = self%atom(1)%position

      ! Do it
      do i = 1,size(pt,1)
         r = pt(i,:)
         r = r-pos
         f(i) = norm_(r)
      end do
   end subroutine

!  ==================================
!  Property density plots (templates)
!  ==================================


!  ============
!  Orbital grid
!  ============

   subroutine make_orbital_grid(self,grid,pt)
   ! Work out the orbital "grid" for a series of points "pt" for using
   ! the ".natural orbitals", for orbital.isosurface.plot_grid.orbital. 
   ! NOTE: for complex orbitals, the absolute value times the sign of
   ! the complex part
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      INT :: n_pt,orb
   ENSURE(size(pt,1)==size(grid,1),"MOLECULE.GRID:make_orbital_grid ... incompatible # of points, g and pt arrays")
   ENSURE(size(pt,2)==3,"MOLECULE.GRID:make_orbital_grid ... wrong shape, pt array")
   ENSURE(allocated(self%isosurface),"MOLECULE.GRID:make_orbital_grid ... no isosurface")
   ENSURE(self%isosurface%plot_grid%orbital>=0,"MOLECULE.GRID:make_orbital_grid ... non-positive grid orbital")
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_orbital_grid ... no natural orbitals")
      n_pt = size(pt,1)

      ! Get orbital to plot
      orb = self%isosurface%plot_grid%orbital

      ! Zero value indicates HOMO ...
      if (orb==0) then
         orb = self%n_a
         if (self%isosurface%plot_grid%HOMO_orbital/=0) then
            orb = orb+self%isosurface%plot_grid%HOMO_orbital
         end if
      end if
      if (trim(number_kind_(self%NOs))=="real") then
         call make_orbital_grid_r_(self,grid,self%NOs%r(:,orb),pt)
      end if
   end subroutine


!  ====================
!  Orbital density grid
!  ====================

   subroutine make_orbital_density_grid(self,gr,pt)
   ! Work out the orbital on ".plot_grid.orbital" of ".NOs" 
   ! on a set of points "pt" returning the values in "gr".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: gr
      MAT(REAL,:,:), IN :: pt
      INT :: orb
      STR(len=STR_SIZE) :: relativity,okind
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_orbital_density_grid ... no grid")
   ENSURE(self%plot_grid%orbital>=0,"MOLECULE.GRID:make_orbital_density_grid ... non-positive grid orbital")
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_orbital_density_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_orbital_density_grid ... no natural orbitals")

      ! Restricted or general complex?
      okind = genre_(self%NOs)

      ! Orbital to be plotted
      orb = self%plot_grid%orbital

      ! Kind of relativity to use
      relativity = "none"
      if (allocated(self%SCF_data)) then
         relativity = self%SCF_data%relativity_kind
         if (NOT self%SCF_data%PCE_CORR) relativity = "none"
      end if
      select case (relativity)

   ! case ("douglas-kroll-hess","dkh")

   !    select case (okind)

   !    case("r ")
   !       .make_DKH_orbital_grid_r(gr,orb,pt,square=TRUE)

   !    case("u ")
   !       stdout.flush
   !       stdout.text("NOTE:")
   !       stdout.text("You can copy either alpha or beta density_mx and MOs")
   !       stdout.text("into the appropriate restricted file(s) and plot in the rhf regime!")
   !       DIE("UHF/UKS unimplemented")

   !    case("gc")
   !       gc.create(.plot_grid.n_pt)
   !       .make_DKH_orb_dens_grid_gc(gc,orb,pt)
   !       gr = RE(gc)
   !       gc.destroy

   !    case default
   !       DIE("unimplemented")

   !    end

   ! case ("iotc")

   !    select case (okind)

   !    case("r ")
   !       .make_IOTC_orbital_grid_r(gr,orb,pt,square=TRUE)

   !    case("u ")
   !       stdout.flush
   !       stdout.text("NOTE:")
   !       stdout.text("You can copy either alpha or beta density_mx and MOs")
   !       stdout.text("into the appropriate restricted file(s) and plot in the rhf regime!")
   !       DIE("UHF/UKS unimplemented")

   !    case("gc")
   !       gc.create(.plot_grid.n_pt)
   !       .make_IOTC_orb_dens_grid_gc(gc,orb,pt)
   !       gr = RE(gc)
   !       gc.destroy

   !    case default
   !       DIE("unimplemented")

   !    end

      case ("none")
         select case (okind)

         case ("r ")
            call make_orbital_grid_r_(self,gr,self%NOs%r(:,orb),pt,square=TRUE)

         case ("u ")
            call flush_(stdout)
            call text_(stdout,"NOTE:")
            call text_(stdout,"You can copy either alpha or beta density_mx and MOs")
            call text_(stdout,"into the appropriate restricted file(s) and plot in the rhf regime!")
            DIE("MOLECULE.GRID:make_orbital_density_grid ... UHF/UKS unimplemented")

      ! case("gc")
      !    gc.create(.plot_grid.n_pt)
      !    .make_orbital_grid_gc(gc,.MOs.gc(:,orb),pt)
      !    gr = RE(gc)
      !    gc.destroy

         case default
            DIE("MOLECULE.GRID:make_orbital_density_grid ... unimplemented")
         end select

      case default
         DIE("MOLECULE.GRID:make_orbital_density_grid ... Unimplemented")
      end select
   end subroutine


!  ========================
!  Roby-Gould orbital grids
!  ========================

!   make_Roby_ANO_density_grid
!   ! Evaluate the Roby ANO density grid for the atom with 
!   ! index .roby.center_atom. Assumes double occupancy.
!      self :: IN
!
!   ENSURE(.roby.allocated, "no roby data -- do one group analysis")
!   ENSURE(.plot_grid.allocated, "no plot_grid")
!   ENSURE(.plot_grid.center_atom>0, "plot_grid center_atom not specified!")
!   ENSURE(.atom(.plot_grid.center_atom).NOs.allocated, "no natural orbitals!")
!   ENSURE(.atom(.plot_grid.center_atom).NOs.r.allocated, "no restricted natural orbitals!")
!
!      pt, NO :: MAT{REAL}@
!      rho, occ :: VEC{REAL}@
!      a :: INT
!
!      ! Do this atom
!      a = .plot_grid.center_atom
!
!      ! Points to plot
!      pt.create(.plot_grid.n_pt,3)
!      .plot_grid.make_points(pt)
!
!      ! Make the grid, rho
!      rho.create(.plot_grid.n_pt)  
!      NO  = .atom(a).NOs.r
!      occ = .atom(a).NO_occ_nos.r
!      .make_atom_density_grid_r(rho,NO,occ,pt,a)
!
!      ! Write grid, rho
!      .dump_plot_grid(rho,"Roby-Gould_ANO_density_atom_"//trim(a.to_str)//"_grid")
!
!      ! Clean
!      rho.destroy
!      pt.destroy
!
!   end

!   make_Roby_NAO_density_grid
!   ! Evaluate the Roby NAO density grid for the atom with index
!   ! .roby.center_atom. Assumes double occupancy.
!      self :: IN
!
!   ENSURE(.roby.allocated, "no roby data -- do one group analysis")
!   ENSURE(.plot_grid.allocated, "no plot_grid")
!   ENSURE(.plot_grid.center_atom>0, "plot_grid center_atom not specified!")
!   ENSURE(.atom(.plot_grid.center_atom).NAOs.allocated, "no natural orbitals!")
!   ENSURE(.atom(.plot_grid.center_atom).NAO_occupations.allocated, "no restricted natural orbitals!")
!
!      pt :: MAT{REAL}@
!      rho :: VEC{REAL}@
!      a :: INT
!
!      ! Do this atom
!      a = .plot_grid.center_atom
!
!      ! Points to plot
!      pt.create(.plot_grid.n_pt,3)
!      .plot_grid.make_points(pt)
!
!      ! Make the grid, rho
!      rho.create(.plot_grid.n_pt)
!      .make_atom_density_grid_r(rho,.atom(a).NAOs,.atom(a).NAO_occupations,pt,a)
!
!      ! Write grid, rho
!      .dump_plot_grid(rho,"Roby-Gould_NAO_density_atom_"//trim(a.to_str)//"_grid")
!
!      ! Clean
!      rho.destroy
!      pt.destroy
!
!   end

!   make_atom_density_grid_r(grid,NO,occ,pt,atom) :: PURE
!   ! Evaluate the atom density "grid" for all "NO"s and occupation
!   ! numbers "occ" for the atom with index "atom" on a set of grid
!   ! points "pt"
!      self :: IN
!      grid :: VEC{REAL}, OUT
!      NO   :: MAT{REAL}, IN
!      occ  :: VEC{REAL}, IN
!      pt   :: MAT{REAL}, IN
!      atom :: INT, IN
!
!   ENSURE(.basis_info_made, "no basis info")
!   ENSURE(.atom.allocated,  "no atom list")
!   ENSURE(NO.dim2==occ.dim, "incompatible dim2, NO & occ")
!   ENSURE(NO.dim1==.atom(atom).basis.no_of_basis_functions, "wrong dim1, NO")
!   ENSURE(pt.dim2==3, "pt dim2 is not 3")
!
!      n :: INT
!      g :: VEC{REAL}@
!
!      grid = ZERO
!
!      g.create(pt.dim1)
!
!      parallel do n = 1,NO.dim2
!         .atom(atom).make_orbital_grid_r(g,NO(:,n),pt)
!         grid = grid + occ(n)*g*g
!      end
!
!      if (WORK_IS_SHARED) then
!         PARALLEL_SUM(grid)
!      end
!
!      ! Clean
!      g.destroy
!
!   end

   subroutine make_RG_cov_orbital_grid(self,gr,pt)
   ! Evaluate the Roby-Gould covalent orbital ".plot_grid.orb" and
   ! write it out to a file.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: gr
      MAT(REAL,:,:), IN :: pt
      INT :: orb
   DIE_IF(NOT allocated(self%roby),"MOLECULE.GRID:make_RG_cov_orbital_grid ... no roby data -- do one group analysis")
   DIE_IF(NOT self%roby%roby_kind=="group_bond_analysis","MOLECULE.GRID:make_RG_cov_orbital_grid ... must do previous group_bond_analysis")
   DIE_IF(NOT n_group_(self%roby)==2,"MOLECULE.GRID:make_RG_cov_orbital_grid ... must have only two groups")
   ENSURE(allocated(self%roby%theta_C),"MOLECULE.GRID:make_RG_cov_orbital_grid ... no Roby-Gould covalent orbitals")
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_RG_cov_orbital_grid ... no plot_grid")
   ENSURE(self%plot_grid%orbital>=0,"MOLECULE.GRID:make_RG_cov_orbital_grid ... non-positive theta orbital")
   ENSURE(self%plot_grid%orbital<=size(self%roby%theta_C,2),"MOLECULE.GRID:make_RG_cov_orbital_grid ... orbital out of range, "//trim(to_str_(self%plot_grid%orbital)))

      ! Orbital to be plotted
      orb = self%plot_grid%orbital
      call flush_(stdout)
      call text_(stdout,"============================")
      call text_(stdout,"Roby-Gould covalent orbitals")
      call text_(stdout,"============================")
      call flush_(stdout)
      call text_(stdout,"For atoms:")
      call put_(stdout,self%roby%atom_ab)
      call flush_(stdout)
      call text_(stdout,"Orbital coefficients:")
      call put_(stdout,self%roby%theta_C(:,orb))

      ! Make the grid, gr
      call make_ELMO_grid_r_(self,gr,self%roby%theta_C(:,orb),pt,self%roby%atom_ab)

      ! Write grid, gr
   ! .dump_plot_grid(gr,"Roby-Gould_covalent_orbital_"//trim(orb.to_str)//"_grid")
   end subroutine

   subroutine make_RG_ion_orbital_grid(self,gr,pt)
   ! Evaluate the Roby-Gould ionic orbital ".plot_grid.orb" and write
   ! it out to a file.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: gr
      MAT(REAL,:,:), IN :: pt
      INT :: orb
   DIE_IF(NOT allocated(self%roby),"MOLECULE.GRID:make_RG_ion_orbital_grid ... no roby data -- do one group analysis")
   DIE_IF(NOT self%roby%roby_kind=="group_bond_analysis","MOLECULE.GRID:make_RG_ion_orbital_grid ... must do previous group_bond_analysis")
   DIE_IF(NOT n_group_(self%roby)==2,"MOLECULE.GRID:make_RG_ion_orbital_grid ... must have only two groups")
   ENSURE(allocated(self%roby%theta_I),"MOLECULE.GRID:make_RG_ion_orbital_grid ... no Roby-Gould ionic orbitals")
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_RG_ion_orbital_grid ... no plot_grid")
   ENSURE(self%plot_grid%orbital>=0,"MOLECULE.GRID:make_RG_ion_orbital_grid ... non-positive theta orbital")
   ENSURE(self%plot_grid%orbital<=size(self%roby%theta_I,2),"MOLECULE.GRID:make_RG_ion_orbital_grid ... orbital out of range, "//trim(to_str_(self%plot_grid%orbital)))

      ! Orbital to be plotted
      orb = self%plot_grid%orbital
      call flush_(stdout)
      call text_(stdout,"=========================")
      call text_(stdout,"Roby-Gould ionic orbitals")
      call text_(stdout,"=========================")
      call flush_(stdout)
      call text_(stdout,"For atoms:")
      call put_(stdout,self%roby%atom_ab)
      call flush_(stdout)
      call text_(stdout,"Orbital coefficients:")
      call put_(stdout,self%roby%theta_I(:,orb))

      ! Make the grid, gr
      call make_ELMO_grid_r_(self,gr,self%roby%theta_I(:,orb),pt,self%roby%atom_ab)

      ! Write grid, gr
      call dump_plot_grid_(self,gr,"Roby-Gould_ionic_orbital_"//trim(to_str_(orb))//"_grid")
   end subroutine

   PURE subroutine make_ELMO_grid_r(self,grid,orb,pt,atom_indices,square)
   ! Evaluate the ELMO "grid" for *one* AO-basis coefficient orbital
   ! vector "orb" on a set of grid points "pt"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      VEC(REAL,:), IN :: orb
      MAT(REAL,:,:), IN :: pt
      VEC(INT,:), IN :: atom_indices
      BIN, optional, IN :: square
      INT :: n_pt,i,a,na,fa,la
      BIN :: sq
      VEC(REAL,:), allocatable :: ga
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_ELMO_grid_r ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_ELMO_grid_r ... no atom list")
   ENSURE(size(orb)==no_of_basis_functions_(self%atom(atom_indices)),"MOLECULE.GRID:make_ELMO_grid_r ... wrong dim2, orbitals")
      sq = FALSE
      if (present(square)) sq = square
      n_pt = size(pt,1)
      la = 0
      grid = ZERO
      do i = 1,size(atom_indices)

         ! Atom index a
         a = atom_indices(i)

         ! Basis fn limits
         na = self%atom(a)%basis%n_bf
         fa = la+1
         la = la+na

         ! Atom a grid, ga
         call create_(ga,n_pt)
         call make_orbital_grid_r_(self%atom(a),ga,orb(fa:la),pt)

         ! Add ga to grid
         grid = grid+ga

         ! Clean
         call destroy_(ga)
      end do
      if (sq) grid = grid*grid
   end subroutine


!  =========================
!  Electron density routines
!  =========================

   PURE subroutine make_ED_grid(self,grid,pt)
   ! Work out the electron "grid" on "pt" using ".natural orbitals" and
   ! the ".NO_occ_nos" vector. 
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      STR(len=STR_SIZE) :: relativity,okind
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_ED_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_ED_grid ... no natural orbitals")
      okind = genre_(self%NOs)
      relativity = "none"
      if (allocated(self%SCF_data)) then
         relativity = self%SCF_data%relativity_kind
         if (NOT self%SCF_data%PCE_CORR) relativity = "none"
      end if
      select case (relativity)

      case ("none")
         select case (okind)
         case ("r "); call make_ED_grid_r_(self,grid,pt)
         case ("u "); call make_ED_grid_u_(self,grid,pt)
         end select
      end select
   end subroutine


!  ==================
!  Spin density grids
!  ==================

   PURE subroutine make_SD_grid(self,grid,pt)
   ! Work out the electron spin density "grid" on "pt" using the
   ! ".NOs" and the ".NO_occ_nos" vector. 
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      STR(len=STR_SIZE) :: relativity,okind
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_SD_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_SD_grid ... no natural orbitals")
      okind = genre_(self%NOs)
      relativity = "none"
      if (allocated(self%SCF_data)) then
         relativity = self%SCF_data%relativity_kind
         if (NOT self%SCF_data%PCE_CORR) relativity = "none"
      end if
      select case (relativity)

      case ("none")
         select case (okind)
         case ("u "); call make_ED_grid_u_(self,grid,pt,sgn=-1)
         end select
      end select
   end subroutine


!  ======================
!  Efficient bond ED grid
!  ======================

   PURE subroutine make_bond_ED_grid_0(self,g,pt)
   ! Work out the bond electron density grid "g" for points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: g
      MAT(REAL,:,:), IN :: pt
      STR(len=STR_SIZE) :: okind
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_bond_ED_grid_0 ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_bond_ED_grid_0 ... no natural orbitals")
      okind = genre_(self%NOs)
      select case (okind)
      case ("r "); call make_bond_ED_grid_r_(self,g,pt)
      end select
   end subroutine

   PURE subroutine make_bond_ED_grid_r(self,g,pt)
   ! Make the bond electron density grid "g" for points "pt" from the
   ! restricted real density matrix.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: g
      MAT(REAL,:,:), IN :: pt
   ENSURE(is_allocated_with_genre_(self%density_mx,"r "),"MOLECULE.GRID:make_bond_ED_grid_r ... no restricted density matrix")
      call make_bond_ED_grid_(self,g,pt,self%density_mx%r)
   end subroutine

   PURE subroutine make_bond_ED_grid_1(self,N0,pt,D)
   ! Make the bond electron density grid "N0" for points "pt"
   ! skipping as many points as possible.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: N0
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), IN :: D
      VEC(type(EVEC_INT_TYPE),:), allocatable :: bf_lgval
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: bf_grid0
      VEC(REAL,3) :: ra,rb,pc,pa,pb
      VEC(REAL,:), allocatable :: xa,ya,za,a2
      VEC(REAL,:), allocatable :: ga0,gb0
      VEC(INT,:), allocatable :: overlapping_atom,lga,lgb,sp
      MAT(REAL,:,:), allocatable :: DD
      type(SHELL1_TYPE) :: sha
      type(SHELL2_TYPE) :: sh
      REAL :: Dab,fac,val,ga,gb
      INT :: atom_o,cutoff,n_pt,n_keep,bs,p,n
      INT :: atom_a,fsa,lsa,sa,fa,la,na,a
      INT :: atom_b,fsb,lsb,sb,fb,lb,nb,b
      INT :: sab
   ENSURE(size(N0)==size(pt,1),"MOLECULE.GRID:make_bond_ED_grid_1 ... N0 and pt: incompatible sizes")
   ENSURE(3==size(pt,2),"MOLECULE.GRID:make_bond_ED_grid_1 ... pt: wrong dim2")
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_bond_ED_grid_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_bond_ED_grid_1 ... no atom list")
   ENSURE(allocated(self%becke_grid),"MOLECULE.GRID:make_bond_ED_grid_1 ... no becke_grid")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.GRID:make_bond_ED_grid_1 ... no overlapping_atoms")

      ! Basis function cutoff
      cutoff = floor(log10(self%becke_grid%basis_fn_cutoff))

      ! Saved basis function grid and skip list
      call create_(bf_lgval,self%n_shell)
      call create_(bf_grid0,self%n_shell)

      ! Atom "a" displaced points and skip switches
      n_pt = size(pt,1)
      call create_(xa,n_pt)
      call create_(ya,n_pt)
      call create_(za,n_pt)
      call create_(a2,n_pt)
      call create_(sp,n_pt)

      ! Create N0
      N0 = ZERO

      ! Loop atom_a
      do atom_a = 1,self%n_atom
         fsa = self%first_shell_for_atom(atom_a)
         lsa = self%last_shell_for_atom(atom_a)

         ! Make shell atom displaced points
         call make_displaced_pts_(self%atom(atom_a),xa,ya,za,a2,pt,n_pt)

         ! Atom a position
         ra = self%atom(atom_a)%position

         ! Overlapping atoms
         overlapping_atom = self%overlapping_atoms_for_atom(atom_a)%element

         ! Loop overlapping atom_b
         do atom_o = 1,size(overlapping_atom)
            atom_b = overlapping_atom(atom_o)

            ! Skip if atoms are equal
            if (atom_b>=atom_a) cycle
            fsb = self%first_shell_for_atom(atom_b)
            lsb = min(self%last_shell_for_atom(atom_b),lsa)

            ! Atom a position
            rb = self%atom(atom_b)%position

            ! Loop shells on atom a
            do sa = fsa,lsa
               fa = self%first_basis_fn_for_shell(sa)
               la = self%last_basis_fn_for_shell(sa)
               na = la-fa+1

               ! Set shell for index "sa"
               bs = self%basis_shell_for_shell(sa)
               sha = self%basis_shell1pair(bs,1)%a
               sha%position = ra

               ! Make shell "sa" basis function grid and lga values
               call create_(bf_grid0(sa)%element,n_pt,na)
               call create_(lga,n_pt)
               call make_grid_(sha,bf_grid0(sa)%element,n_pt,xa,ya,za,a2,lga)

               ! Save the values
               bf_lgval(sa)%element = lga

               ! Loop shells on atom b
               do sb = fsb,min(lsb,sa)
                  fb = self%first_basis_fn_for_shell(sb)
                  lb = self%last_basis_fn_for_shell(sb)
                  nb = lb-fb+1

                  ! Get shell-pair
                  call set_shell2_from_(self,sab,sh)

                  ! TEST: Pair-center distance
                  pc = sh%av_pair_center
                  pa = pc-ra
                  pb = pc-rb
                  pc = pc-HALF*(ra+rb)
                  if (norm_(pa)<norm_(pc)) cycle
                  if (norm_(pb)<norm_(pc)) cycle

                  ! Get the "sb" basis function grid
                  lgb = bf_lgval(sb)%element

                  ! How many points in common?
                  n_keep = count(lga+lgb>=cutoff)
                  if (n_keep==0) cycle

                  ! Make significant points
                  sp(1:n_keep) = pack([(n,n = 1,n_pt)],mask=lga+lgb>=cutoff)

                  ! Factor
                  fac = TWO
                  if (sa==sb) fac = ONE

                  ! Density block
                  DD = D(fa:la,fb:lb)
                  if (na*nb==1) then
                     ga0 = bf_grid0(sa)%element(:,1)
                     gb0 = bf_grid0(sb)%element(:,1)
                     Dab = fac*DD(1,1)
                     do p = 1,n_keep
                        n = sp(p)
                        ga = ga0(n)
                        gb = gb0(n)
                        val = ga*gb*Dab
                        N0(n) = N0(n)+val
                     end do
                  else

                     ! Add density contribution to rho
                     do a = 1,na
                        ga0 = bf_grid0(sa)%element(:,a)
                        do b = 1,nb
                           gb0 = bf_grid0(sb)%element(:,b)
                           Dab = fac*DD(a,b)
                           do p = 1,n_keep
                              n = sp(p)
                              ga = ga0(n)
                              gb = gb0(n)
                              val = ga*gb*Dab
                              N0(n) = N0(n)+val
                           end do
                        end do
                     end do
                  end if  ! if ss or not ss
               end do  ! -- loop sb
            end do  ! -- loop sa
         end do  ! -- atom_b
      end do  ! -- atom_a

      ! Clean up
      call destroy_(sp)
      call destroy_(a2)
      call destroy_(za)
      call destroy_(ya)
      call destroy_(xa)
      call destroy_(bf_grid0)
      call destroy_(bf_lgval)
   end subroutine


!  =========================
!  Interaction density grids
!  =========================

   PURE subroutine make_interaction_density_grid(self,grid,pt)
   ! Make the interaction density grid.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: prop,prop_grid
      INT :: g
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_interaction_density_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_interaction_density_grid ... no natural orbitals")
   ENSURE(allocated(self%atom_group),"MOLECULE.GRID:make_interaction_density_grid ... no atom_group")
   ENSURE(allocated(self%mol),"MOLECULE.GRID:make_interaction_density_grid ... no .mol")
   ENSURE(size(self%mol)==size(self%atom_group),"MOLECULE.GRID:make_interaction_density_grid ... no .mol")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_ED_grid_(self,grid,pt)

      ! Allocate grids
      call create_(prop_grid,size(grid))
      call create_(prop,size(grid))
      prop_grid = ZERO

      ! Add up sub grid values
      do g = 1,size(self%atom_group)
         call make_ED_grid_(self%mol(g),prop,pt)
         prop_grid = prop_grid+prop
      end do

      ! Subtract to form deformation grid
      grid = grid-prop_grid
   end subroutine


!  =========================
!  Deformation density grids
!  =========================

   NON_MPI_PURE subroutine make_def_ED_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_ED_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_ED_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_ED_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_promolecule_density_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine


!  =========
!  ADP grids
!  =========

   subroutine make_ADP_grid(self,g,pt)
   ! Work out the ADP density grid "g" for points "pt".
   ! For the first atom, only
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: g
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%crystal),"MOLECULE.GRID:make_ADP_grid ... no crystal")
   ENSURE(allocated(self%isosurface),"MOLECULE.GRID:make_ADP_grid ... no isosurface")
   ENSURE(self%isosurface%plot_grid%center_atom>0,"MOLECULE.GRID:make_ADP_grid ... no center_atom")
      call make_ADP_grid_(self%atom(1),g,pt,self%crystal%unit_cell)
   end subroutine

   subroutine make_ADP2_exponent_grid(self,g,pt)
   ! Work out the ADP2 PDF exponent grid "g" for points "pt".
   ! For the first atom, only
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: g
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%isosurface),"MOLECULE.GRID:make_ADP2_exponent_grid ... no isosurface")
   ENSURE(self%isosurface%plot_grid%center_atom>0,"MOLECULE.GRID:make_ADP2_exponent_grid ... no center_atom")
      call make_ADP2_exponent_grid_(self%atom(1),g,pt)
   end subroutine

   subroutine make_ADP_exponent_grid(self,g,pt)
   ! Work out the ADP density grid "g" for points "pt".
   ! For the first atom, only
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: g
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%crystal),"MOLECULE.GRID:make_ADP_exponent_grid ... no crystal")
   ENSURE(allocated(self%isosurface),"MOLECULE.GRID:make_ADP_exponent_grid ... no isosurface")
   ENSURE(self%isosurface%plot_grid%center_atom>0,"MOLECULE.GRID:make_ADP_exponent_grid ... no center_atom")
      call make_ADP_exponent_grid_(self%atom(1),g,pt,self%crystal%unit_cell)
   end subroutine

   subroutine make_AADP_correction_grid(self,g,pt)
   ! Work out the anharmonic ADP PDF correction density grid "g" for points "pt".
   ! For the first atom, only
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: g
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%crystal),"MOLECULE.GRID:make_AADP_correction_grid ... no crystal")
   ENSURE(allocated(self%isosurface),"MOLECULE.GRID:make_AADP_correction_grid ... no isosurface")
   ENSURE(self%isosurface%plot_grid%center_atom>0,"MOLECULE.GRID:make_AADP_correction_grid ... no center_atom")
      call make_AADP_PDF_correction_grid_(self%atom(1),g,pt)
   end subroutine


!  ==================
!  Bond density grids
!  ==================

   subroutine make_def_bond_density_grid_0(self)
   ! Work out the property density on ".plot_grid" using ".natural
   ! orbitals" and the ".NO_occ_nos" vector. A Gnuplot ascii
   ! file is generated.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), allocatable :: grid
      MAT(REAL,:,:), allocatable :: pt
      STR(len=STR_SIZE) :: label
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_def_bond_density_grid_0 ... no grid")
   ENSURE(allocated(self%plot_grid%bond_density_atom),"MOLECULE.GRID:make_def_bond_density_grid_0 ... no bond_density_atoms")

      ! Make space for grid values and grid points
      call create_(grid,self%plot_grid%n_pt)
      call create_(pt,self%plot_grid%n_pt,3)

      ! Make the grid points
      call make_points_(self%plot_grid,pt)

      ! Make the density
      call make_def_bond_density_grid_(self,grid,pt,self%plot_grid%bond_density_atom)

      ! Make the plot label
      label = "deformation_bond_("
      label = trim(label)//trim(to_str_(self%plot_grid%bond_density_atom(1)))//","
      label = trim(label)//trim(to_str_(self%plot_grid%bond_density_atom(2)))
      if (size(self%plot_grid%bond_density_atom)==3) label = trim(label)//":"//trim(to_str_(self%plot_grid%bond_density_atom(3)))
      label = trim(label)//")_density_grid"

      ! Print out
      call dump_plot_grid_(self,grid,trim(label))

      ! Clean up
      call destroy_(pt)
      call destroy_(grid)
   end subroutine

   NON_MPI_PURE subroutine make_def_bond_density_grid_1(self,density_grid,pt,bond_density_atom)
   ! Make the bond deformation "density_grid" for the points "pt".
   ! If there are two atoms then the whole bond density is made; if
   ! there are three atoms then the third atom specifies which of the
   ! Hirshfeld-split bond density is produced.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(INT,:), IN :: bond_density_atom
      INT :: n_atoms,n_pt,a,b,c,n
      REAL :: x,y,z
      VEC(REAL,3) :: r_ab
      VEC(REAL,:), allocatable :: rho,rho_a,rho_b
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_def_bond_density_grid_1 ... no atoms")
   ENSURE(self%n_atom>1,"MOLECULE.GRID:make_def_bond_density_grid_1 ... must have at least two atoms")

      ! Make the deformation ED grid
      call make_def_ED_grid_(self,density_grid,pt)

      ! No of bond density atoms
      n_atoms = size(bond_density_atom)

      ! No of grid points
      n_pt = size(density_grid)

      ! Create space for pair density
      call create_(rho,n_pt)
      rho = ZERO

      ! Get the atom pair density for whole molecule
      r_ab = ZERO
      do a = 2,self%n_atom
         do b = 1,a-1
            if (NOT bonded_(self%atom,a,b)) cycle
            r_ab = HALF*(self%atom(a)%position+self%atom(b)%position)
            do n = 1,n_pt
               x = pt(n,1)-r_ab(1)
               y = pt(n,2)-r_ab(2)
               z = pt(n,3)-r_ab(3)
               rho(n) = rho(n)+exp(-(x*x+y*y+z*z))
            end do
         end do
      end do

      ! Make the density grid
      select case (n_atoms)

      case (2:3)  ! Two or three atoms in the list

         ! Indices of bonded pair
         a = bond_density_atom(1)
         b = bond_density_atom(2)

         ! Make the deformation bond pair density
         r_ab = HALF*(self%atom(a)%position+self%atom(b)%position)
         do n = 1,n_pt
            if (rho(n)>epsilon(ONE)) then
               x = pt(n,1)-r_ab(1)
               y = pt(n,2)-r_ab(2)
               z = pt(n,3)-r_ab(3)
               density_grid(n) = density_grid(n)*exp(-(x*x+y*y+z*z))/rho(n)
            else
               density_grid(n) = ZERO
            end if
         end do

         ! Make the (third) atom contribution === if required
         if (n_atoms==3) then
            call create_(rho_a,n_pt)
            call create_(rho_b,n_pt)
            call make_ED_grid_(self%atom(a),rho_a,pt)
            call make_ED_grid_(self%atom(b),rho_b,pt)
            rho = rho_a+rho_b
            c = bond_density_atom(3)
            if (c==a) then
               where (rho>epsilon(ONE)); density_grid = (rho_a/rho)*density_grid
               elsewhere; density_grid = ZERO
               end where
            else if (c==b) then
               where (rho>epsilon(ONE)); density_grid = (rho_b/rho)*density_grid
               elsewhere; density_grid = ZERO
               end where
         ! else
         !    DIE("atom is not one of those in the bond")
            end if
            call destroy_(rho_b)
            call destroy_(rho_a)
         end if
      end select

      ! Clean up
      call destroy_(rho)
   end subroutine

   NON_MPI_PURE subroutine make_def_bond_density_grid_2(self,density_grid,pt,bond_density_atom,def_density_grid)
   ! Make the bond deformation "density_grid" for the points "pt".
   ! If there are two atoms then the whole bond density is made; if
   ! there are three atoms then the third atom specifies which of the
   ! Hirshfeld-split bond density is produced.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(INT,:), IN :: bond_density_atom
      VEC(REAL,:), IN :: def_density_grid
      INT :: n_atoms,n_pt,a,b,c,n
      REAL :: x,y,z,r_inv
      VEC(REAL,3) :: r_ab
      VEC(REAL,:), allocatable :: rho,rho_a,rho_b
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_def_bond_density_grid_2 ... no atoms")
   ENSURE(self%n_atom>1,"MOLECULE.GRID:make_def_bond_density_grid_2 ... must have at least two atoms")

      ! No of bond density atoms
      n_atoms = size(bond_density_atom)

      ! No of grid points
      n_pt = size(density_grid)

      ! Create space for pair density
      call create_(rho,n_pt)
      rho = ZERO

      ! Get the atom pair density for whole molecule
      r_ab = ZERO
      do a = 2,self%n_atom
         do b = 1,a-1
            if (NOT bonded_(self%atom,a,b)) cycle
            r_ab = HALF*(self%atom(a)%position+self%atom(b)%position)
            r_inv = ONE/norm_(r_ab)
            do n = 1,n_pt
               x = pt(n,1)-r_ab(1)
               y = pt(n,2)-r_ab(2)
               z = pt(n,3)-r_ab(3)
               rho(n) = rho(n)+exp(-(x*x+y*y+z*z)*r_inv)
            end do
         end do
      end do

      ! Make the density grid
      select case (n_atoms)

      case (2:3)  ! Two or three atoms in the list

         ! Indices of bonded pair
         a = bond_density_atom(1)
         b = bond_density_atom(2)

         ! Make the deformation bond pair density
         r_ab = HALF*(self%atom(a)%position+self%atom(b)%position)
         r_inv = ONE/norm_(r_ab)
         do n = 1,n_pt
            if (rho(n)>epsilon(ONE)) then
               x = pt(n,1)-r_ab(1)
               y = pt(n,2)-r_ab(2)
               z = pt(n,3)-r_ab(3)
               density_grid(n) = def_density_grid(n)*exp(-(x*x+y*y+z*z)*r_inv)/rho(n)
            else
               density_grid(n) = ZERO
            end if
         end do

         ! Make the (third) atom contribution === if required
         if (n_atoms==3) then
            call create_(rho_a,n_pt)
            call create_(rho_b,n_pt)
            call make_ED_grid_(self%atom(a),rho_a,pt)
            call make_ED_grid_(self%atom(b),rho_b,pt)
            rho = rho_a+rho_b
            c = bond_density_atom(3)
            if (c==a) then
               where (rho>epsilon(ONE)); density_grid = (rho_a/rho)*density_grid
               elsewhere; density_grid = ZERO
               end where
            else if (c==b) then
               where (rho>epsilon(ONE)); density_grid = (rho_b/rho)*density_grid
               elsewhere; density_grid = ZERO
               end where
         ! else
         !    DIE("atom is not one of those in the bond")
            end if
            call destroy_(rho_b)
            call destroy_(rho_a)
         end if
      end select

      ! Clean up
      call destroy_(rho)
   end subroutine

   subroutine make_bond_weight_grid_0(self)
   ! Work out the property density on ".plot_grid" using ".natural
   ! orbitals" and the ".NO_occ_nos" vector. A Gnuplot ascii
   ! file is generated.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: grid
      MAT(REAL,:,:), allocatable :: pt
      STR(len=STR_SIZE) :: label
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_bond_weight_grid_0 ... no grid")
   ENSURE(allocated(self%plot_grid%bond_density_atom),"MOLECULE.GRID:make_bond_weight_grid_0 ... no bond_density_atoms")

      ! Make space for grid values and grid points
      call create_(grid,self%plot_grid%n_pt)
      call create_(pt,self%plot_grid%n_pt,3)

      ! Make the grid points
      call make_points_(self%plot_grid,pt)

      ! Make the density
      call make_bond_weight_grid_(self,grid,pt,self%plot_grid%bond_density_atom)

      ! Make the plot label
      label = "bond_("
      label = trim(label)//trim(to_str_(self%plot_grid%bond_density_atom(1)))//","
      label = trim(label)//trim(to_str_(self%plot_grid%bond_density_atom(2)))
      if (size(self%plot_grid%bond_density_atom)==3) label = trim(label)//":"//trim(to_str_(self%plot_grid%bond_density_atom(3)))
      label = trim(label)//")_weight_grid"

      ! Print out
      call dump_plot_grid_(self,grid,trim(label))

      ! Clean up
      call destroy_(pt)
      call destroy_(grid)
   end subroutine

   PURE subroutine make_bond_weight_grid_1(self,density_grid,pt,bond_density_atom)
   ! Make the bond deformation "density_grid" for the points "pt".
   ! If there are two atoms then the whole bond density is made; if
   ! there are three atoms then the third atom specifies which of the
   ! Hirshfeld-split bond density is produced.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(INT,:), IN :: bond_density_atom
      INT :: n_atoms,n_pt,a,b,c,n
      REAL :: x,y,z
      VEC(REAL,3) :: r_ab
      VEC(REAL,:), allocatable :: rho,rho_a,rho_b
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_bond_weight_grid_1 ... no atoms")
   ENSURE(self%n_atom>1,"MOLECULE.GRID:make_bond_weight_grid_1 ... must have at least two atoms")

      ! No of bond density atoms
      n_atoms = size(bond_density_atom)

      ! No of grid points
      n_pt = size(density_grid)

      ! Create space for pair density
      call create_(rho,n_pt)
      rho = ZERO

      ! Get the atom pair density for whole molecule
      r_ab = ZERO
      do a = 2,self%n_atom
         do b = 1,a-1
            if (NOT bonded_(self%atom,a,b)) cycle
            r_ab = HALF*(self%atom(a)%position+self%atom(b)%position)
            do n = 1,n_pt
               x = pt(n,1)-r_ab(1)
               y = pt(n,2)-r_ab(2)
               z = pt(n,3)-r_ab(3)
               rho(n) = rho(n)+exp(-(x*x+y*y+z*z))
            end do
         end do
      end do

      ! Make the density grid
      select case (n_atoms)

      case (2:3)  ! Two or three atoms in the list

         ! Indices of bonded pair
         a = bond_density_atom(1)
         b = bond_density_atom(2)

         ! Make the deformation bond pair density
         r_ab = HALF*(self%atom(a)%position+self%atom(b)%position)
         do n = 1,n_pt
            if (rho(n)>epsilon(ONE)) then
               x = pt(n,1)-r_ab(1)
               y = pt(n,2)-r_ab(2)
               z = pt(n,3)-r_ab(3)
               density_grid(n) = exp(-(x*x+y*y+z*z))/rho(n)
            else
               density_grid(n) = ZERO
            end if
         end do

         ! Make the (third) atom contribution === if required
         if (n_atoms==3) then
            call create_(rho_a,n_pt)
            call create_(rho_b,n_pt)
            call make_ED_grid_(self%atom(a),rho_a,pt)
            call make_ED_grid_(self%atom(b),rho_b,pt)
            rho = rho_a+rho_b
            c = bond_density_atom(3)
            if (c==a) then
               where (rho>epsilon(ONE)); density_grid = (rho_a/rho)*density_grid
               elsewhere; density_grid = ZERO
               end where
            else if (c==b) then
               where (rho>epsilon(ONE)); density_grid = (rho_b/rho)*density_grid
               elsewhere; density_grid = ZERO
               end where
         ! else
         !    DIE("atom is not one of those in the bond")
            end if
            call destroy_(rho_b)
            call destroy_(rho_a)
         end if
      end select

      ! Clean up
      call destroy_(rho)
   end subroutine

   subroutine make_bond_density_atom_grid_0(self)
   ! Work out the bond density atom on ".plot_grid"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), allocatable :: grid
      MAT(REAL,:,:), allocatable :: pt
      STR(len=STR_SIZE) :: label
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_bond_density_atom_grid_0 ... no grid")
   ENSURE(allocated(self%plot_grid%bond_density_atom),"MOLECULE.GRID:make_bond_density_atom_grid_0 ... no bond_density_atoms")

      ! Make space for grid values and grid points
      call create_(grid,self%plot_grid%n_pt)
      call create_(pt,self%plot_grid%n_pt,3)

      ! Make the grid points
      call make_points_(self%plot_grid,pt)

      ! Make the density
      call make_bond_density_atom_grid_(self,grid,pt,self%plot_grid%bond_density_atom(1))

      ! Make the plot label
      label = "bond_density_atom_"
      label = trim(label)//trim(to_str_(self%plot_grid%bond_density_atom(1)))
      label = trim(label)//"_grid"

      ! Print out
      call dump_plot_grid_(self,grid,trim(label))

      ! Clean up
      call destroy_(pt)
      call destroy_(grid)
   end subroutine

   NON_MPI_PURE subroutine make_bond_density_atom_grid_1(self,density_grid,pt,a)
   ! Make the bond density atom grid "density_grid" for the points
   ! "pt" for atom with index "a".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      INT, IN :: a
      INT :: n_pt,i
      VEC(REAL,:), allocatable :: rho_bond,def_density_grid
      VEC(INT,:), allocatable :: bond_density_atom
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_bond_density_atom_grid_1 ... no atoms")

      ! No of grid points
      n_pt = size(density_grid)

      ! Do only the spherical part if a<0
      if (a<0) then
         call make_ED_grid_(self%atom(-a),density_grid,pt)

      ! Do the whole BDA
      else

         ! Allocate
         call create_(def_density_grid,n_pt)
         call create_(rho_bond,n_pt)

         ! Space for bond density atoms
         call create_(bond_density_atom,3)
         bond_density_atom(1) = a
         bond_density_atom(3) = a

         ! Make the deformation density
         call make_def_ED_grid_(self,def_density_grid,pt)

         ! Add spherical part
         call make_ED_grid_(self%atom(a),density_grid,pt)

         ! Get bond density contributions
         do i = 1,self%n_atom
            if (i==a) cycle
            if (NOT bonded_(self%atom,i,a)) cycle
            bond_density_atom(2) = i
            call make_def_bond_density_grid_(self,rho_bond,pt,bond_density_atom,def_density_grid)
            density_grid = density_grid+rho_bond
         end do

         ! Clean up
         call destroy_(bond_density_atom)
         call destroy_(rho_bond)
         call destroy_(def_density_grid)
      end if
   end subroutine


!  ============================
!  Local ionisation energy grid
!  ============================

   NON_MPI_PURE subroutine make_local_ionisation_grid(self,grid,pt)
   ! Work out Politzer's local ionisation energy "grid" using points "pt"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%MOs),"MOLECULE.GRID:make_local_ionisation_grid ... no molecular orbitals")
   ENSURE(has_any_genre_(self%MOs),"MOLECULE.GRID:make_local_ionisation_grid ... no molecular orbitals")
      if (trim(number_kind_(self%MOs))=="real") then
         call make_local_ionisation_grid_r_(self,grid,pt)
   ! else
   !    DIE("code not written yet")
      end if
   end subroutine

   NON_MPI_PURE subroutine make_local_ionisation_grid_r(self,grid,pt)
   ! Work out Politzer's local ionisation energy "grid" using points "pt"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: rho,MO
      INT :: n_occ,n,n_pt
   ENSURE(size(pt,2)==3,"MOLECULE.GRID:make_local_ionisation_grid_r ... wrong dimension for pt array")
   ENSURE(is_allocated_with_genre_(self%MOs,"r "),"MOLECULE.GRID:make_local_ionisation_grid_r ... no restricted MO's")
   ENSURE(is_allocated_with_genre_(self%MO_energies,"r "),"MOLECULE.GRID:make_local_ionisation_grid_r ... no restricted MO energies")
      n_pt = size(pt,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(rho,n_pt)
      call create_(MO,n_pt)
      rho = ZERO
      grid = ZERO
      do n = PARALLEL_DO_START(1,1),n_occ,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_local_ionisation_grid_r")
         call make_orbital_grid_r_(self,MO,self%MOs%r(:,n),pt,square=TRUE)
         rho = rho+MO
         grid = grid-MO*self%MO_energies%r(n)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_local_ionisation_grid_r")
      PARALLEL_SUM(rho)
      PARALLEL_SUM(grid)
      grid = grid/rho
      call destroy_(MO)
      call destroy_(rho)
   end subroutine


!  =======================
!  Local virial ratio grid
!  =======================

   NON_MPI_PURE subroutine make_local_virial_ratio_grid(self,grid,pt)
   ! Work out local virial ratio "grid" using points "pt"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%MOs),"MOLECULE.GRID:make_local_virial_ratio_grid ... no molecular orbitals")
   ENSURE(has_any_genre_(self%MOs),"MOLECULE.GRID:make_local_virial_ratio_grid ... no molecular orbitals")
      if (trim(number_kind_(self%MOs))=="real") then
         call make_local_virial_ratio_grid_r_(self,grid,pt)
   ! else
   !    DIE("code not written yet")
      end if
   end subroutine

   NON_MPI_PURE subroutine make_local_virial_ratio_grid_r(self,grid,pt)
   ! Work out local virial ratio  "grid" using points "pt"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: L,G
      INT :: n_pt
   ENSURE(size(pt,2)==3,"MOLECULE.GRID:make_local_virial_ratio_grid_r ... wrong dimension for pt array")
      n_pt = size(pt,1)
      call create_(L,n_pt)
      call create_(G,n_pt)
      call make_laplacian_ED_grid_r_(self,L,pt)
      call make_G_kinetic_density_grid_r_(self,G,pt)
      grid = TWO-QUARTER*L/G
      call destroy_(G)
      call destroy_(L)
   end subroutine


! ==========================
! Laplacian density routines
! ==========================

   NON_MPI_PURE subroutine make_laplacian_ED_grid(self,Lgrid,pts)
   ! Work out the electron density laplacian "Lgrid" on points "pts"
   ! using ".natural orbitals" and  the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: Lgrid
      MAT(REAL,:,:), IN :: pts
      STR(len=STR_SIZE) :: NO_kind
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_laplacian_ED_grid ... no natural orbitals")
   ENSURE(has_real_genre_(self%NOs),"MOLECULE.GRID:make_laplacian_ED_grid ... no real NOs")
      NO_kind = genre_(self%NOs)
      select case (NO_kind)
      case ("r "); call make_laplacian_ED_grid_r_(self,Lgrid,pts)
      case ("u "); call make_laplacian_ED_grid_u_(self,Lgrid,pts)
      end select
   end subroutine

   subroutine make_def_laplacian_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_laplacian_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_laplacian_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_laplacian_ED_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_laplacian_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_laplacian_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_laplacian_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_laplacian_grid")
         call make_laplacian_ED_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_laplacian_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine

   NON_MPI_PURE subroutine make_neg_laplacian_grid(self,laplacian_grid,pt)
   ! Work out the electron "laplacian_grid" on "pt" using ".natural orbitals"
   ! and  the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: laplacian_grid
      MAT(REAL,:,:), IN :: pt
      STR(len=STR_SIZE) :: NO_kind
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_neg_laplacian_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_neg_laplacian_grid ... no natural orbitals")
      NO_kind = genre_(self%NOs)
      if (trim(number_kind_(self%NOs))=="real") then
         select case (NO_kind)
         case ("r ")
            call make_laplacian_ED_grid_r_(self,laplacian_grid,pt)
            laplacian_grid = -laplacian_grid
         case ("u ")
            call make_laplacian_ED_grid_u_(self,laplacian_grid,pt)
            laplacian_grid = -laplacian_grid
         end select
      end if
   end subroutine

   PURE subroutine make_rL_grid(self,laplacian_grid,pt)
   ! Work out the reduced "laplacian_grid" on "pt" using ".natural
   ! orbitals" and  the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: laplacian_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_rL_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_rL_grid ... no natural orbitals")
      if (trim(number_kind_(self%NOs))=="real") then
         call make_rL_grid_r_(self,laplacian_grid,pt)
   ! else
   !    DIE("code not written yet")
      end if
   end subroutine

   PURE subroutine make_rL_grid_r(self,laplacian_grid,pt)
   ! Make the reduced "laplacian_grid" for the supplied points "pt" from
   ! restricted real natural orbitals
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: laplacian_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: rho,NO
      MAT(REAL,:,:), allocatable :: P,D
      INT :: n_occ,n,n_pt
      REAL :: occ
   ENSURE(size(pt,2)==3,"MOLECULE.GRID:make_rL_grid_r ... wrong dimension for pt array")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.GRID:make_rL_grid_r ... no restricted NO's")
   ENSURE(allocated(self%NO_occ_nos%r),"MOLECULE.GRID:make_rL_grid_r ... no occupation #'s")
      n_pt = size(pt,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(rho,n_pt)
      call create_(NO,n_pt)
      call create_(P,n_pt,3)
      call create_(D,n_pt,3)
      laplacian_grid = ZERO
      rho = ZERO
      do n = 1,n_occ
         call make_laplacian_orbital_grid_r_(self,D,P,NO,self%NOs%r(:,n),pt)
         occ = self%NO_occ_nos%r(n)
         laplacian_grid = laplacian_grid+occ*(NO*(D(:,1)+D(:,2)+D(:,3))+P(:,1)*P(:,1)+P(:,2)*P(:,2)+P(:,3)*P(:,3))
         rho = rho+occ*NO*NO
      end do
      laplacian_grid = TWO*laplacian_grid/rho
      call destroy_(D)
      call destroy_(P)
      call destroy_(NO)
      call destroy_(rho)
   end subroutine


!  ===============================
!  K-type kinetic density routines
!  ===============================

   PURE subroutine make_K_kinetic_energy_grid(self,K_grid,pt)
   ! Work out the K-type kinetic energy density (using the nabla^2) on "pt" using
   ! ".natural orbitals" and  the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: K_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_K_kinetic_energy_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_K_kinetic_energy_grid ... no natural orbitals")
      if (number_kind_(self%NOs)=="real") then
         call make_K_kinetic_density_grid_r_(self,K_grid,pt)
   ! else
   !    DIE("code not written yet")
      end if
   end subroutine

   PURE subroutine make_K_kinetic_density_grid_r(self,K_grid,pt)
   ! Make the K-type kinetic energy density "K_grid" for the supplied points
   ! "pt" from restricted real natural orbitals
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: K_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: NO
      MAT(REAL,:,:), allocatable :: P,D
      INT :: n_occ,n,n_pt
   ENSURE(size(pt,2)==3,"MOLECULE.GRID:make_K_kinetic_density_grid_r ... wrong dimension for pt array")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.GRID:make_K_kinetic_density_grid_r ... no restricted NO's")
   ENSURE(allocated(self%NO_occ_nos%r),"MOLECULE.GRID:make_K_kinetic_density_grid_r ... no occupation #'s")
      n_pt = size(pt,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(NO,n_pt)
      call create_(P,n_pt,3)
      call create_(D,n_pt,3)
      K_grid = ZERO
      do n = 1,n_occ
         call make_laplacian_orbital_grid_r_(self,D,P,NO,self%NOs%r(:,n),pt)
         K_grid = K_grid+self%NO_occ_nos%r(n)*NO*(D(:,1)+D(:,2)+D(:,3))
      end do
      K_grid = -HALF*K_grid
      call destroy_(D)
      call destroy_(P)
      call destroy_(NO)
   end subroutine

   subroutine make_def_K_kinetic_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_K_kinetic_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_K_kinetic_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_K_kinetic_energy_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_K_kinetic_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_K_kinetic_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_K_kinetic_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_K_kinetic_grid")
         call make_K_kinetic_energy_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_K_kinetic_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine

   PURE subroutine make_rK_kinetic_energy_grid(self,K_grid,pt)
   ! Work out the K-type reduced kinetic energy density (using the nabla^2) on
   ! "pt" using ".natural orbitals" and  the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: K_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_rK_kinetic_energy_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_rK_kinetic_energy_grid ... no natural orbitals")
      if (number_kind_(self%NOs)=="real") then
         call make_rK_kinetic_density_grid_r_(self,K_grid,pt)
   ! else
   !    DIE("code not written yet")
      end if
   end subroutine

   PURE subroutine make_rK_kinetic_density_grid_r(self,K_grid,pt)
   ! Make the K-type reduced kinetic energy density "K_grid" for the supplied
   ! points "pt" from restricted real natural orbitals
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: K_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: NO,rho
      MAT(REAL,:,:), allocatable :: P,D
      INT :: n_occ,n,n_pt
      REAL :: occ
   ENSURE(size(pt,2)==3,"MOLECULE.GRID:make_rK_kinetic_density_grid_r ... wrong dimension for pt array")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.GRID:make_rK_kinetic_density_grid_r ... no restricted NO's")
   ENSURE(allocated(self%NO_occ_nos%r),"MOLECULE.GRID:make_rK_kinetic_density_grid_r ... no occupation #'s")
      n_pt = size(pt,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(rho,n_pt)
      call create_(NO,n_pt)
      call create_(P,n_pt,3)
      call create_(D,n_pt,3)
      K_grid = ZERO
      rho = ZERO
      do n = 1,n_occ
         call make_laplacian_orbital_grid_r_(self,D,P,NO,self%NOs%r(:,n),pt)
         occ = self%NO_occ_nos%r(n)
         K_grid = K_grid+occ*NO*(D(:,1)+D(:,2)+D(:,3))
         rho = rho+occ*NO*NO
      end do
      K_grid = -HALF*K_grid/rho
      call destroy_(D)
      call destroy_(P)
      call destroy_(NO)
      call destroy_(rho)
   end subroutine

   subroutine make_def_rK_kinetic_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_rK_kinetic_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_rK_kinetic_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_rK_kinetic_energy_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_rK_kinetic_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_rK_kinetic_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_rK_kinetic_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_rK_kinetic_grid")
         call make_rK_kinetic_energy_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_rK_kinetic_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine


!  ===============================
!  G-type kinetic density routines
!  ===============================

   PURE subroutine make_norm_G_from_virial_grid(self,G_grid,pt)
   ! Work out the normalized G kinetic energy density
   ! This has the scf energy subtracted.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: G_grid
      MAT(REAL,:,:), IN :: pt
      call make_G_kinetic_energy_grid_(self,G_grid,pt)
      G_grid = G_grid+self%SCF_data%energy
   end subroutine

   PURE subroutine make_G_kinetic_energy_grid(self,G_grid,pt)
   ! Work out the G-type kinetic density (using nabla phi.nabla phi) on "pt"
   ! using ".natural orbitals" and  the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: G_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_G_kinetic_energy_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_G_kinetic_energy_grid ... no natural orbitals")
      if (number_kind_(self%NOs)=="real") then
         call make_G_kinetic_density_grid_r_(self,G_grid,pt)
   ! else
   !    DIE("code not written yet")
      end if
   end subroutine

   PURE subroutine make_G_kinetic_density_grid_r(self,G_grid,pt)
   ! Make the G-type kinetic energy density "G_grid" for the supplied points
   ! "pt" from restricted real natural orbitals
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: G_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: NO,orb
      MAT(REAL,:,:), allocatable :: P
      INT :: n_occ,n,n_pt
   ENSURE(size(pt,2)==3,"MOLECULE.GRID:make_G_kinetic_density_grid_r ... wrong dimension for pt array")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.GRID:make_G_kinetic_density_grid_r ... no restricted NO's")
   ENSURE(allocated(self%NO_occ_nos%r),"MOLECULE.GRID:make_G_kinetic_density_grid_r ... no occupation #'s")
      n_pt = size(pt,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(NO,n_pt)
      call create_(P,n_pt,3)
      G_grid = ZERO
      do n = 1,n_occ
         orb = self%NOs%r(:,n)
         call make_nabla_orbital_grid_r_(self,P,NO,orb,pt)
         G_grid(:) = G_grid(:)+self%NO_occ_nos%r(n)*(P(:,1)*P(:,1)+P(:,2)*P(:,2)+P(:,3)*P(:,3))
      end do
      G_grid(:) = HALF*G_grid(:)
      call destroy_(P)
      call destroy_(NO)
   end subroutine

   PURE subroutine make_G_Kirzhnits_grid(self,G_grid,pt)
   ! Work out the G-type Kirzhnits kinetic density (using nabla phi.nabla phi) on "pt"
   ! using ".natural orbitals" and  the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: G_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_G_Kirzhnits_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_G_Kirzhnits_grid ... no natural orbitals")
      if (number_kind_(self%NOs)=="real") then
         call make_G_Kirzhnits_grid_r_(self,G_grid,pt)
   ! else
   !    DIE("code not written yet")
      end if
   end subroutine

   PURE subroutine make_G_Kirzhnits_grid_r(self,G_grid,pt)
   ! Make the G-type Kirzhnits kinetic energy density "G_grid" for the supplied points
   ! "pt" from restricted real natural orbitals
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: G_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: NO,rho,nab_ro2,lap_rho,orb
      MAT(REAL,:,:), allocatable :: G,L,nab_rho
      REAL :: lambda,k2,fac,occ,occ2
      INT :: n_occ,n,n_pt
   ENSURE(size(pt,2)==3,"MOLECULE.GRID:make_G_Kirzhnits_grid_r ... wrong dimension for pt array")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.GRID:make_G_Kirzhnits_grid_r ... no restricted NO's")
   ENSURE(allocated(self%NO_occ_nos%r),"MOLECULE.GRID:make_G_Kirzhnits_grid_r ... no occupation #'s")
      n_pt = size(pt,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(rho,n_pt)
      call create_(lap_rho,n_pt)
      call create_(nab_ro2,n_pt)
      call create_(nab_rho,n_pt,3)
      call create_(NO,n_pt)
      call create_(G,n_pt,3)
      call create_(L,n_pt,3)
      rho = ZERO
      lap_rho = ZERO
      nab_rho = ZERO
      do n = 1,n_occ
         orb = self%NOs%r(:,n)
         call make_laplacian_orbital_grid_r_(self,L,G,NO,orb,pt)
         occ = self%NO_occ_nos%r(n)
         occ2 = TWO*occ
         rho = rho+occ*NO*NO
         nab_rho(:,1) = nab_rho(:,1)+occ2*NO*G(:,1)
         nab_rho(:,2) = nab_rho(:,2)+occ2*NO*G(:,2)
         nab_rho(:,3) = nab_rho(:,3)+occ2*NO*G(:,3)
         lap_rho = lap_rho+occ2*(NO*(L(:,1)+L(:,2)+L(:,3)))+occ2*(G(:,1)*G(:,1)+G(:,2)*G(:,2)+G(:,3)*G(:,3))
      end do
      call destroy_(L)
      call destroy_(G)
      call destroy_(NO)
      nab_ro2 = nab_rho(:,1)*nab_rho(:,1)+nab_rho(:,2)*nab_rho(:,2)+nab_rho(:,3)*nab_rho(:,3)
      call destroy_(nab_rho)
      lambda = ONE/72.0d0
      k2 = HALF/SIX
      fac = 0.3d0*(THREE*PI*PI)**(TWOTHIRD)
      G_grid = fac*rho**(FIVE/THREE)+lambda*nab_ro2/rho+k2*lap_rho
      call destroy_(nab_ro2)
      call destroy_(lap_rho)
      call destroy_(rho)
   end subroutine

   subroutine make_def_G_kinetic_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_G_kinetic_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_G_kinetic_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_G_kinetic_energy_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_G_kinetic_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_G_kinetic_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_G_kinetic_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_G_kinetic_grid")
         call make_G_kinetic_energy_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_G_kinetic_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine

   subroutine make_def_G_Kirzhnits_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_G_Kirzhnits_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_G_Kirzhnits_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_G_Kirzhnits_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_G_Kirzhnits_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_G_Kirzhnits_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_G_Kirzhnits_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_G_Kirzhnits_grid")
         call make_G_Kirzhnits_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_G_Kirzhnits_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine

   PURE subroutine make_rG_kinetic_energy_grid(self,G_grid,pt)
   ! Work out the reduced G-type kinetic density (using nabla
   ! phi.nabla phi) on "pt" using ".natural orbitals" and  the
   ! ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: G_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_rG_kinetic_energy_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_rG_kinetic_energy_grid ... no natural orbitals")
      if (number_kind_(self%NOs)=="real") then
         call make_rG_kinetic_density_grid_r_(self,G_grid,pt)
   ! else
   !    DIE("code not written yet")
      end if
   end subroutine

   PURE subroutine make_rG_kinetic_density_grid_r(self,G_grid,pt)
   ! Make the G-type kinetic energy density "G_grid" for the supplied points
   ! "pt" from restricted real natural orbitals
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: G_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: rho,NO,orb
      MAT(REAL,:,:), allocatable :: P
      INT :: n_occ,n,n_pt
      REAL :: occ
   ENSURE(size(pt,2)==3,"MOLECULE.GRID:make_rG_kinetic_density_grid_r ... wrong dimension for pt array")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.GRID:make_rG_kinetic_density_grid_r ... no restricted NO's")
   ENSURE(allocated(self%NO_occ_nos%r),"MOLECULE.GRID:make_rG_kinetic_density_grid_r ... no occupation #'s")
      n_pt = size(pt,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(rho,n_pt)
      call create_(NO,n_pt)
      call create_(P,n_pt,3)
      G_grid = ZERO
      rho = ZERO
      do n = 1,n_occ
         orb = self%NOs%r(:,n)
         call make_nabla_orbital_grid_r_(self,P,NO,orb,pt)
         occ = self%NO_occ_nos%r(n)
         G_grid = G_grid+occ*(P(:,1)*P(:,1)+P(:,2)*P(:,2)+P(:,3)*P(:,3))
         rho = rho+occ*NO*NO
      end do
      G_grid = HALF*G_grid/rho
      call destroy_(P)
      call destroy_(NO)
      call destroy_(rho)
   end subroutine

   subroutine make_def_rG_kinetic_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_rG_kinetic_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_rG_kinetic_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_rG_kinetic_energy_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_rG_kinetic_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_rG_kinetic_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_rG_kinetic_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_rG_kinetic_grid")
         call make_rG_kinetic_energy_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_rG_kinetic_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine


!  ===============================
!  G and K-type kinetic densities
!  ===============================

   PURE subroutine make_G_and_K_kinetic_grids(self,G_grid,K_grid,pt)
   ! Work out the G and K-type kinetic energy density on "pt" using
   ! ".natural orbitals" and  the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: G_grid
      VEC(REAL,:), OUT :: K_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_G_and_K_kinetic_grids ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_G_and_K_kinetic_grids ... no natural orbitals")
      if (number_kind_(self%NOs)=="real") then
         call make_G_and_K_kinetic_grids_r_(self,G_grid,K_grid,pt)
   ! else
   !    DIE("code not written yet")
      end if
   end subroutine

   PURE subroutine make_G_and_K_kinetic_grids_r(self,G_grid,K_grid,pt)
   ! Make the G and K-type kinetic energy grids for the supplied points
   ! "pt" from restricted real natural orbitals
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: G_grid
      VEC(REAL,:), OUT :: K_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: NO,orb
      MAT(REAL,:,:), allocatable :: P,D
      INT :: n_occ,n,n_pt
      REAL :: occ
   ENSURE(size(pt,2)==3,"MOLECULE.GRID:make_G_and_K_kinetic_grids_r ... wrong dimension for pt array")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.GRID:make_G_and_K_kinetic_grids_r ... no restricted NO's")
   ENSURE(allocated(self%NO_occ_nos%r),"MOLECULE.GRID:make_G_and_K_kinetic_grids_r ... no occupation #'s")
      n_occ = no_of_occupied_NOs_(self)
      n_pt = size(pt,1)
      call create_(NO,n_pt)
      call create_(P,n_pt,3)
      call create_(D,n_pt,3)
      G_grid = ZERO
      K_grid = ZERO
      do n = 1,n_occ
         orb = self%NOs%r(:,n)
         call make_laplacian_orbital_grid_r_(self,D,P,NO,orb,pt)
         occ = self%NO_occ_nos%r(n)
         G_grid = G_grid+occ*(P(:,1)*P(:,1)+P(:,2)*P(:,2)+P(:,3)*P(:,3))
         K_grid = K_grid+occ*NO*(D(:,1)+D(:,2)+D(:,3))
      end do
      G_grid = HALF*G_grid
      K_grid = -HALF*K_grid
      call destroy_(D)
      call destroy_(P)
      call destroy_(NO)
   end subroutine


!  =================
!  LOL from Schmider
!  =================

   PURE subroutine make_LOL_grid(self,LOL_grid,pt)
   ! Work out the LOL density (using nabla phi.nabla phi) on "pt"
   ! using ".natural orbitals" and  the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: LOL_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_LOL_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_LOL_grid ... no natural orbitals")
      if (number_kind_(self%NOs)=="real") then
         call make_LOL_grid_r_(self,LOL_grid,pt)
   ! else
   !    DIE("code not written yet")
      end if
   end subroutine

   PURE subroutine make_LOL_grid_r(self,LOL_grid,pt)
   ! Make the "LOL_grid" for the supplied points
   ! "pt" from restricted real natural orbitals.
   ! See Schmider & Becke (2000) J. Mol. STruct 527 p. 51-61.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: LOL_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: tau,tau0
      VEC(REAL,:), allocatable :: NO,orb
      MAT(REAL,:,:), allocatable :: P
      INT :: n_occ,n,n_pt
      REAL :: occ
   ENSURE(size(pt,2)==3,"MOLECULE.GRID:make_LOL_grid_r ... wrong dimension for pt array")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.GRID:make_LOL_grid_r ... no restricted NO's")
   ENSURE(allocated(self%NO_occ_nos%r),"MOLECULE.GRID:make_LOL_grid_r ... no occupation #'s")
      n_pt = size(pt,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(tau0,n_pt)
      call create_(tau,n_pt)
      call create_(NO,n_pt)
      call create_(P,n_pt,3)
      LOL_grid = ZERO
      do n = 1,n_occ
         orb = self%NOs%r(:,n)
         call make_nabla_orbital_grid_r_(self,P,NO,orb,pt)
         occ = self%NO_occ_nos%r(n)
         if (is_zero_(occ,TOL(8))) cycle
         tau = tau+occ*(P(:,1)*P(:,1)+P(:,2)*P(:,2)+P(:,3)*P(:,3))
         tau0 = tau0+occ*NO*NO
      end do
      tau = HALF*tau
      tau0 = HALF*tau0

      ! Make tau0
      tau0 = 0.6d0*(SIX*PI*PI)**(TWO/THREE)*(tau0)**(FIVE/THREE)

      ! Make t(sigma)
      where (abs(tau0)<TOL(8))
         tau = tau/tau0
      elsewhere
         tau = ZERO
      end where

      ! Make LOL
      LOL_grid = tau/(ONE+tau)

      ! Clean
      call destroy_(P)
      call destroy_(NO)
      call destroy_(tau)
      call destroy_(tau0)
   end subroutine


!  =============================
!  Energy density from rho grids
!  =============================

   PURE subroutine make_E_hf_dens_from_rho_grid(self,E_grid,pt)
   ! Work out the energy density as the scaled electron density.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: E_grid
      MAT(REAL,:,:), IN :: pt
      REAL :: fac
   ENSURE(allocated(self%SCF_data),"MOLECULE.GRID:make_E_hf_dens_from_rho_grid ... no SCF_data")
   ENSURE(is_one_of_(self%SCF_data%SCF_kind,["rhf","uhf"]),"MOLECULE.GRID:make_E_hf_dens_from_rho_grid ... scf is not RHF/UHF")
      call make_ED_grid_(self,E_grid,pt)
      fac = self%SCF_data%energy/self%n_e
      E_grid = fac*E_grid
   end subroutine

   subroutine make_def_E_hf_from_rho_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_E_hf_from_rho_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_E_hf_from_rho_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_E_hf_dens_from_rho_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_E_hf_dens_from_rho_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine


!  ========================================================
!  H-type total energy density obtained from virial theorem
!  ========================================================

   NON_MPI_PURE subroutine make_norm_H_from_virial_grid(self,H_grid,pt)
   ! Work out the normalized energy density using the virial
   ! relationsip 2G + V = (1/4) L, or H = G + V = (1/4)L - G
   ! This has the scf energy subtracted.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: H_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: G_grid
      call create_(G_grid,self%plot_grid%n_pt)
      call make_G_kinetic_energy_grid_(self,G_grid,pt)
      call make_laplacian_ED_grid_(self,H_grid,pt)
      H_grid = QUARTER*H_grid-G_grid-self%SCF_data%energy
      call destroy_(G_grid)
   end subroutine

   NON_MPI_PURE subroutine make_H_from_virial_grid(self,H_grid,pt)
   ! Work out the energy density using the virial relationsip 2G + V = (1/4) L,
   ! or H = G + V = (1/4)L - G
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: H_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: G_grid
      call create_(G_grid,self%plot_grid%n_pt)
      call make_G_kinetic_energy_grid_(self,G_grid,pt)
      call make_laplacian_ED_grid_(self,H_grid,pt)
      H_grid = QUARTER*H_grid-G_grid
      call destroy_(G_grid)
   end subroutine

   subroutine make_def_H_from_virial_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_H_from_virial_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_H_from_virial_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_H_from_virial_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_H_from_virial_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_H_from_virial_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_H_from_virial_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_H_from_virial_grid")
         call make_H_from_virial_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_H_from_virial_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine

   NON_MPI_PURE subroutine make_H_Kirzhnits_grid(self,H_grid,pt)
   ! Work out the energy density using the Kirzhnits relationsip 2G +
   ! V = (1/4) L, or H = G + V = (1/4)L - G
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: H_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: G_grid
      call create_(G_grid,self%plot_grid%n_pt)
      call make_G_Kirzhnits_grid_(self,G_grid,pt)
      call make_laplacian_ED_grid_(self,H_grid,pt)
      H_grid = QUARTER*H_grid-G_grid
      call destroy_(G_grid)
   end subroutine

   subroutine make_def_H_Kirzhnits_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_H_Kirzhnits_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_H_Kirzhnits_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_H_Kirzhnits_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_H_Kirzhnits_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_H_Kirzhnits_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_H_Kirzhnits_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_H_Kirzhnits_grid")
         call make_H_Kirzhnits_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_H_Kirzhnits_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine

   PURE subroutine make_rH_from_virial_grid(self,H_grid,pt)
   ! Work out the reduced energy density using the virial relationsip
   ! 2G + V = (1/4) L, or H = G + V = (1/4)L - G
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: H_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: G_grid
      call create_(G_grid,self%plot_grid%n_pt)
      call make_rG_kinetic_energy_grid_(self,G_grid,pt)
      call make_rL_grid_(self,H_grid,pt)
      H_grid = QUARTER*H_grid-G_grid
      call destroy_(G_grid)
   end subroutine

   NON_MPI_PURE subroutine make_def_rH_from_virial_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_rH_from_virial_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_rH_from_virial_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_rH_from_virial_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_rH_from_virial_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_rH_from_virial_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_rH_from_virial_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_rH_from_virial_grid")
         call make_rH_from_virial_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_rH_from_virial_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine


!  =====================================================
!  Potential energy density obtained from virial theorem
!  =====================================================

   NON_MPI_PURE subroutine make_V_from_virial_grid(self,V,pt)
   ! Work out the energy density from usiang the virial theorem: 
   ! V = (1/4) L - 2G
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: V
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: G
      call create_(G,self%plot_grid%n_pt)
      call make_G_kinetic_energy_grid_(self,G,pt)
      call make_laplacian_ED_grid_(self,V,pt)
      V = QUARTER*V-TWO*G
      call destroy_(G)
   end subroutine

   subroutine make_def_V_from_virial_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_V_from_virial_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_V_from_virial_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_V_from_virial_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_V_from_virial_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_V_from_virial_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_V_from_virial_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_V_from_virial_grid")
         call make_V_from_virial_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_V_from_virial_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine

   NON_MPI_PURE subroutine make_V_Kirzhnits_grid(self,V,pt)
   ! Work out the energy density from usiang the Kirzhnits theorem: 
   ! V = (1/4) L - 2G
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: V
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: G
      call create_(G,self%plot_grid%n_pt)
      call make_G_Kirzhnits_grid_(self,G,pt)
      call make_laplacian_ED_grid_(self,V,pt)
      V = QUARTER*V-TWO*G
      call destroy_(G)
   end subroutine

   subroutine make_def_V_Kirzhnits_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_V_Kirzhnits_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_V_Kirzhnits_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_V_Kirzhnits_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_V_Kirzhnits_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_V_Kirzhnits_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_V_Kirzhnits_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_V_Kirzhnits_grid")
         call make_V_Kirzhnits_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_V_Kirzhnits_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine

   PURE subroutine make_rV_from_virial_grid(self,V,pt)
   ! Work out the energy density from usiang the virial theorem: 
   ! V = (1/4) L - 2G
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: V
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: G
      call create_(G,self%plot_grid%n_pt)
      call make_rG_kinetic_energy_grid_(self,G,pt)
      call make_rL_grid_(self,V,pt)
      V = QUARTER*V-TWO*G
      call destroy_(G)
   end subroutine

   subroutine make_def_rV_from_virial_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_rV_from_virial_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_rV_from_virial_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_rV_from_virial_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_rV_from_virial_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_rV_from_virial_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_rV_from_virial_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_rV_from_virial_grid")
         call make_rV_from_virial_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_rV_from_virial_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine

   NON_MPI_PURE subroutine make_norm_V_from_virial_grid(self,V,pt)
   ! Work out the Wang and Peng's generalization of Berlin's 
   ! binding energy density
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: V
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: G
   ENSURE(allocated(self%SCF_data),"MOLECULE.GRID:make_norm_V_from_virial_grid ... no SCF_data!")
   ENSURE(NOT is_zero_(self%SCF_data%energy),"MOLECULE.GRID:make_norm_V_from_virial_grid ... no scf energy!")
      call create_(G,self%plot_grid%n_pt)
      call make_G_kinetic_energy_grid_(self,G,pt)
      call make_laplacian_ED_grid_(self,V,pt)
      V = QUARTER*V-TWO*G-TWO*self%SCF_data%energy
      call destroy_(G)
   end subroutine

   PURE subroutine make_binding_function_WP_grid(self,V,pt)
   ! Work out the Wang and Peng's generalization of Berlin's binding
   ! energy density using *their* formula (47)
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: V
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3) :: Ra,r,center
      REAL :: l,d
      INT :: a,i,Za
   ENSURE(allocated(self%SCF_data),"MOLECULE.GRID:make_binding_function_WP_grid ... no SCF_data!")
   ENSURE(NOT is_zero_(self%SCF_data%energy),"MOLECULE.GRID:make_binding_function_WP_grid ... no scf energy!")
      V = ZERO
      center = center_of_atoms_(self%atom)
      do a = 1,self%n_atom
         Ra = self%atom(a)%position-center
         Za = self%atom(a)%atomic_number
         do i = 1,size(V)
            r(1) = pt(i,1)-center(1)-Ra(1)
            r(2) = pt(i,2)-center(2)-Ra(2)
            r(3) = pt(i,3)-center(3)-Ra(3)
            l = sqrt(r(1)*r(1)+r(2)*r(2)+r(3)*r(3))
            d = Ra(1)*r(1)+Ra(2)*r(2)+Ra(3)*r(3)
            V(i) = V(i)-Za*d/(l*l*l)
         end do
      end do
   end subroutine


!  ==================
!  Electric potential
!  ==================

   NON_MPI_PURE subroutine make_EP_grid(self,pot_grid,pt)
   ! Make the electric potential "pot_grid" on a series of points "pt"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: pot_grid
      MAT(REAL,:,:), IN :: pt
      STR(len=STR_SIZE) :: method
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_EP_grid ... no basis info")
      method = "exact"

      ! The below is to avoid crashing if we are making an isosurface
      ! and .plot_grid is not created; then .isosurface.plot_grid will
      ! be automatically created if we are doing an isosurface. This
      ! is an ugly hack, hopefully temporary
      if (allocated(self%isosurface)) then
         method = self%isosurface%plot_grid%electric_potential_method
      else if (allocated(self%plot_grid)) then
         method = self%plot_grid%electric_potential_method
      end if
      select case (method)

      case ("exact")
         if (trim(number_kind_(self%density_mx))=="real") then
            call make_EP_grid_r_(self,pot_grid,pt,self%density_mx%r)
         end if
      end select
   end subroutine

   NON_MPI_PURE subroutine make_multipole_EP_grid(self,pot_grid,pt)
   ! Make the electric potential "pot_grid" on a series of points "pt"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: pot_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: ESP_grid
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_multipole_EP_grid ... no basis info")
      call create_(ESP_grid,size(pot_grid))
      call nuclear_potential_(self,pot_grid,pt)
      call make_multipole_ESP_grid_r_(self,ESP_grid,pt)
      pot_grid = pot_grid+ESP_grid
      call destroy_(ESP_grid)
   end subroutine

   subroutine make_mixed_EP_grid(self,grid,pt)
   ! Make the electric potential "pot_grid" on a series of points "pt"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: ESP_grid
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_mixed_EP_grid ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.GRID:make_mixed_EP_grid ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.GRID:make_mixed_EP_grid ... no density matrix")
   ENSURE(trim(number_kind_(self%density_mx))=="real","MOLECULE.GRID:make_mixed_EP_grid ... must be a real density matrix")
      call create_(ESP_grid,size(grid))
      call nuclear_potential_(self,grid,pt)
      call make_mixed_ESP_grid_r_(self,ESP_grid,pt,self%density_mx%r)
      grid = grid+ESP_grid
      call destroy_(ESP_grid)
   end subroutine

   subroutine make_scm_EP_grid(self,grid,pt)
   ! Make the electric potential "grid" on a series of points "pt"
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: ESP_grid
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_scm_EP_grid ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.GRID:make_scm_EP_grid ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.GRID:make_scm_EP_grid ... no density matrix")
   ENSURE(trim(number_kind_(self%density_mx))=="real","MOLECULE.GRID:make_scm_EP_grid ... must be a real density matrix")
      call create_(ESP_grid,size(grid))
      call nuclear_potential_(self,grid,pt)
      call make_scm_ESP_grid_r_(self,ESP_grid,pt,self%density_mx%r)
      grid = grid+ESP_grid
      call destroy_(ESP_grid)
   end subroutine

   NON_MPI_PURE subroutine make_EP_grid_r(self,grid,pt,dens)
   ! Make the total electric potential "grid" on a series of grid points
   ! "pt" using AO density matrix "dens"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), IN :: dens
      VEC(REAL,:), allocatable :: ESP_grid
      call create_(ESP_grid,size(grid))
      call nuclear_potential_(self,grid,pt)
      call make_electronic_pot_grid_r_(self,ESP_grid,pt,dens)
      grid = grid+ESP_grid
      call destroy_(ESP_grid)
   end subroutine


!  This is written twice!
!  As make_EP_grid

   NON_MPI_PURE subroutine make_electronic_pot_grid(self,pot_grid,pt)
   ! Make the electronic potential "pot_grid" on a series of points "pt"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: pot_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_electronic_pot_grid ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.GRID:make_electronic_pot_grid ... no density matrix")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.GRID:make_electronic_pot_grid ... no density matrix")
      if (trim(number_kind_(self%density_mx))=="real") then
         call make_electronic_pot_grid_r_(self,pot_grid,pt,self%density_mx%r)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_electronic_pot_grid_r(self,grid,pt,dens)
   ! Make the electronic potential "grid" on a series of grid points "pt"
   ! using AO density matrix "dens"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), INOUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), target, IN :: dens
      MAT(REAL,:,:), allocatable :: Vab
      MAT(REAL,:,:), allocatable :: Dba
      INT :: n_pt,q,fa,fb,la,lb,na,nb,k
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_electronic_pot_grid_r ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_electronic_pot_grid_r ... no atom list")
      n_pt = size(pt,1)
      grid = ZERO
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_electronic_pot_grid_r")
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         Dba = dens(fa:la,fb:lb)
         call create_(Vab,na,nb)
         if (fa/=fb) then
            do k = 1,n_pt
               call get_nuc_(sh,Vab,ONE,pt(k,:))
               grid(k) = grid(k)-sum(Vab*Dba)*TWO
            end do
         else
            do k = 1,n_pt
               call get_nuc_(sh,Vab,ONE,pt(k,:))
               grid(k) = grid(k)-sum(Vab*Dba)
            end do
         end if
         call destroy_(Vab)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_electronic_pot_grid_r")

      ! NOTE: this reduction must be OUTSIDE the parallel do. The translator
      ! emits LOCK_PARALLEL_DO as the first statement inside the loop, and
      ! WORK_IS_SHARED is `.is_parallel AND .parallel_do_lock==" "`, so inside
      ! the body the guard is always FALSE -- a PARALLEL_SUM written there is
      ! dead code, and every rank kept only its own 1/n_ranks of the terms.
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(grid)
      end if
   end subroutine


!  ===============
!  Multipole stuff
!  ===============

   NON_MPI_PURE subroutine make_multipole_ESP_grid_r(self,grid,pt)
   ! Make the multipole ESP grid 
   ! potential "grid" on a series of grid points "pt" using AO density
   ! matrix "dens"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), INOUT :: grid
      MAT(REAL,:,:), IN :: pt
      INT :: q
      type(T_TENSOR_TYPE) :: T
      VEC(REAL,:), allocatable :: charges
      MAT(REAL,:,:), allocatable :: dipoles,quadrupoles,octupoles
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_multipole_ESP_grid_r ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_multipole_ESP_grid_r ... no atom list")
      grid = ZERO
      call create_(charges,self%n_atom)
      call create_(dipoles,self%n_atom,3)
      call create_(quadrupoles,self%n_atom,6)
      call create_(octupoles,self%n_atom,10)
      call make_atomic_multipoles_(self,charges,dipoles,quadrupoles,octupoles,od=FALSE)
      do q = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_multipole_ESP_grid_r")
         call electrostatic_potential_grid_(T,charges(q),dipoles(q,:),quadrupoles(q,:),octupoles(q,:),self%atom(q)%position,pt,grid)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_multipole_ESP_grid_r")

      ! The atom loop above is rank-partitioned, so without this each rank kept
      ! only its own 1/n_ranks of the atomic multipole contributions. There was
      ! no reduction here at all.
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(grid)
      end if
   end subroutine

   subroutine make_mixed_ESP_grid_r(self,grid,pt,dens)
   ! Make the multipole ESP grid (off diagonal) using the exact for
   ! the one center terms potential "grid" on a series of grid points
   ! "pt" using AO density matrix "dens"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), INOUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), IN :: dens
      MAT(REAL,:,:), allocatable :: Vab
      MAT(REAL,:,:), allocatable :: Dba
      INT :: n_pt,q,fa,fb,la,lb,na,nb,k
      INT :: atom_a,atom_b
      type(SHELL2_TYPE) :: sh
      type(T_TENSOR_TYPE) :: T
      VEC(REAL,:), allocatable :: charges
      MAT(REAL,:,:), allocatable :: dipoles,quadrupoles,octupoles
      VEC(REAL,4) :: V
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_mixed_ESP_grid_r ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_mixed_ESP_grid_r ... no atom list")
      n_pt = size(pt,1)
      grid = ZERO
      call create_(charges,self%n_atom)
      call create_(dipoles,self%n_atom,3)
      call create_(quadrupoles,self%n_atom,6)
      call create_(octupoles,self%n_atom,10)
      call make_connection_table_(self%atom)
      call make_atomic_multipoles_(self,charges,dipoles,quadrupoles,octupoles,oc=TRUE)

      ! NOTE: this loop is NOT rank-partitioned -- every rank would compute the
      ! whole multipole contribution. Since the shell-pair loop below is
      ! rank-partitioned and reduced with PARALLEL_SUM(grid), an unguarded
      ! contribution here would be counted n_ranks times by that reduction.
      ! Restricting it to the master rank makes it enter the sum exactly once.
      ! In a serial build is_master_processor is TRUE and PARALLEL_SUM is empty,
      ! so this is byte-identical to the unguarded form.
      if (is_master_processor_(tonto)) then
         do q = 1,self%n_atom
            do k = 1,n_pt
               call create_fast_(T,self%atom(q)%position,pt(k,:))
               V = electrostatic_potential_(T,charges(q),dipoles(q,:),quadrupoles(q,:),octupoles(q,:))
               grid(k) = grid(k)-sum(V)
            end do
         end do
      end if
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_mixed_ESP_grid_r")
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (has_a_connection_(atom_a,atom_b) OR (atom_a==atom_b)) then
            Dba = dens(fa:la,fb:lb)
            call create_(Vab,na,nb)
            if (fa/=fb) then
               do k = 1,n_pt
                  call get_nuc_(sh,Vab,ONE,pt(k,:))
                  grid(k) = grid(k)-sum(Vab*Dba)*TWO
               end do
            else
               do k = 1,n_pt
                  call get_nuc_(sh,Vab,ONE,pt(k,:))
                  grid(k) = grid(k)-sum(Vab*Dba)
               end do
            end if
            call destroy_(Vab)
         end if
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_mixed_ESP_grid_r")

      ! NOTE: must be OUTSIDE the parallel do -- see make_electronic_pot_grid_r.
      ! It was previously inside the loop AND inside the has_a_connection test,
      ! so it was dead code; had it ever run it would have been a collective
      ! inside a rank-dependent conditional, i.e. a guaranteed hang.
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(grid)
      end if
   end subroutine

   subroutine make_scm_ESP_grid_r(self,grid,pt,dens)
   ! Make the multipole ESP "grid" (off diagonal) using the exact for
   ! the one center terms on a series of grid points "pt" using AO
   ! density matrix "dens"
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), INOUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), IN :: dens
      MAT(REAL,:,:), allocatable :: Vab
      MAT(REAL,:,:), allocatable :: Dba
      INT :: n_pt,q,fa,fb,la,lb,na,nb,k
      INT :: atom_a,atom_b
      type(SHELL2_TYPE) :: sh
      type(T_TENSOR_TYPE) :: T
      VEC(REAL,:), allocatable :: charges
      MAT(REAL,:,:), allocatable :: dipoles,quadrupoles,octupoles
      VEC(REAL,4) :: V
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_scm_ESP_grid_r ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_scm_ESP_grid_r ... no atom list")
      n_pt = size(pt,1)
      grid = ZERO
      call create_(charges,self%n_atom)
      call create_(dipoles,self%n_atom,3)
      call create_(quadrupoles,self%n_atom,6)
      call create_(octupoles,self%n_atom,10)
      call make_connection_table_(self%atom)
      call make_atomic_multipoles_(self,charges,dipoles,quadrupoles,octupoles,no_s=TRUE)

      ! Master-only: replicated contribution, must enter the PARALLEL_SUM below
      ! exactly once. Byte-identical in a serial build. See make_mixed_ESP_grid_r.
      if (is_master_processor_(tonto)) then
         do q = 1,self%n_atom
            charges(q) = charges(q)
            do k = 1,n_pt
               call create_fast_(T,self%atom(q)%position,pt(k,:))
               V = electrostatic_potential_(T,charges(q),dipoles(q,:),quadrupoles(q,:),octupoles(q,:))
               grid(k) = grid(k)-sum(V)
            end do
         end do
      end if
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_scm_ESP_grid_r")
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if ((sh%a%l==0) AND (sh%b%l==0) AND atom_a==atom_b) then
            Dba = dens(fa:la,fb:lb)
            call create_(Vab,na,nb)
            if (fa/=fb) then
               do k = 1,n_pt
                  call get_nuc_(sh,Vab,ONE,pt(k,:))
                  grid(k) = grid(k)-sum(Vab*Dba)*TWO
               end do
            else
               do k = 1,n_pt
                  call get_nuc_(sh,Vab,ONE,pt(k,:))
                  grid(k) = grid(k)-sum(Vab*Dba)
               end do
            end if
            call destroy_(Vab)
         end if
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_scm_ESP_grid_r")

      ! NOTE: must be OUTSIDE the parallel do -- see make_electronic_pot_grid_r.
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(grid)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_mixed_ESP_grid_r_v2(self,grid,pt,P)
   ! Make the multipole ESP grid (off diagonal) using the exact for
   ! the one center terms potential "grid" on a series of grid points
   ! "pt" using AO density matrix "P"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), INOUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), IN :: P
      VEC(type(EMAT3_REAL_TYPE),:), allocatable :: VD
      MAT(REAL,:,:), allocatable :: Vab
      MAT(REAL,:,:), allocatable :: Pba
      REAL :: P_max,tol,Va,Vb
      INT :: n_pt,q,a,b,fa,fb,la,lb,na,nb,k
      INT :: atom_a,atom_b
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_mixed_ESP_grid_r_v2 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_mixed_ESP_grid_r_v2 ... no atom list")
   ! pole0 :: VEC{REAL}@
   ! pole2,pole4,pole8 :: MAT{REAL}@
   ! V :: VEC{REAL}(4)
   ! T :: T_TENSOR
      n_pt = size(pt,1)
      grid = ZERO

      ! Make atomic multipoles
   ! pole0.create(.n_atom)
   ! pole2.create(.n_atom, 3)
   ! pole4.create(.n_atom, 6)
   ! pole8.create(.n_atom, 10)

   ! .make_atomic_multipoles(pole0,pole2,pole4,pole8,oc=TRUE)

   ! ! Make mutipole potential
   ! do q = 1, .n_atom
   ! do k = 1, n_pt
   !    T.create_fast(.atom(q).position, pt(k,:))
   !    V = T.electrostatic_potential(pole0(q),pole2(q,:),pole4(q,:),pole8(q,:))
   !    grid(k) = grid(k) - sum(V)
   ! end do
   ! end do

   ! pole8.destroy
   ! pole4.destroy
   ! pole2.destroy
   ! pole0.destroy

      ! Diagonal blocks for schwarz test
      call make_diagonal_block_V_ints_(self,VD,pt)

      ! Schwarz test tolerance
      tol = TOL(6)

      ! Make exact potential
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_mixed_ESP_grid_r_v2")
         call set_shell2_from_(self,q,sh,a,fa,la,na,b,fb,lb,nb,atom_a,atom_b)

         ! Density matrix
         Pba = P(fa:la,fb:lb)
         P_max = maxval(abs(Pba))
         if (fa==fb) then

            ! Do basis-fn diagonal exactly
            do k = 1,n_pt
               grid(k) = grid(k)-sum(VD(a)%element(:,:,k)*Pba)
            end do
         else if (P_max>tol) then

            ! Schwarz test for off diagonals & large P's
            allocate(Vab(na,nb))
            do k = 1,n_pt
               Va = sqrt(maxval(abs(VD(a)%element(:,:,k))))
               Vb = sqrt(maxval(abs(VD(b)%element(:,:,k))))
               if (P_max*Va*Vb>tol) then
                  call get_nuc_(sh,Vab,ZERO,pt(k,:))
                  grid(k) = grid(k)-sum(Vab*Pba)*TWO
               end if
            end do
            deallocate(Vab)
         end if
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_mixed_ESP_grid_r_v2")

      ! Clean
      call destroy_(VD)

      ! Reduce
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(grid)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_diagonal_block_V_ints(self,V,pt)
   ! Make "V" the diagonal block nuclear attraction integrals 
   ! (a| |r_k - r|^-1 |a) over a shell q, a in q, for each 
   ! point r_k = "pt(k,1:3)".
      type(MOLECULE_TYPE), IN :: self
      VEC(type(EMAT3_REAL_TYPE),:), allocatable, OUT :: V
      MAT(REAL,:,:), IN :: pt
      INT :: n_pt,q,a,b,fa,fb,la,lb,na,nb,k
      INT :: atom_a,atom_b
      type(SHELL2_TYPE) :: sh
   ENSURE(3==size(pt,2),"MOLECULE.GRID:make_diagonal_block_V_ints ... wrong dim2, pt")
      n_pt = size(pt,1)
      call create_(V,self%n_shell)
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_diagonal_block_V_ints")
         call set_shell2_from_(self,q,sh,a,fa,la,na,b,fb,lb,nb,atom_a,atom_b)
         if (a==b) then
            call create_(V(a)%element,na,na,n_pt)
            do k = 1,n_pt
               call get_nuc_(sh,V(a)%element(:,:,k),ZERO,pt(k,:))
            end do
         end if
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_diagonal_block_V_ints")
   end subroutine

   subroutine make_atomic_multipoles_0(self,od,oc,no_s)
   ! Make the atomic multipoles using the tanaka partitioing
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: od,oc,no_s
      BIN :: off_diagonal,only_disconnected,no_s_functions
      MAT(REAL,:,:), allocatable :: S,Sa,Sb,part
      MAT(REAL,:,:), allocatable :: Ax,Ay,Az
      MAT(REAL,:,:), allocatable :: Bx,By,Bz
      MAT(REAL,:,:), allocatable :: Axx,Ayy,Azz,Axy,Axz,Ayz
      MAT(REAL,:,:), allocatable :: Bxx,Byy,Bzz,Bxy,Bxz,Byz
      MAT(REAL,:,:), allocatable :: Axxx,Ayyy,Azzz,Axxy,Axxz,Ayyx,Ayyz,Azzx,Azzy,Axyz
      MAT(REAL,:,:), allocatable :: Bxxx,Byyy,Bzzz,Bxxy,Bxxz,Byyx,Byyz,Bzzx,Bzzy,Bxyz
      INT :: q,fa,la,na,fb,lb,nb,atom_a,atom_b
      type(SHELL2_TYPE) :: sh
      REAL :: fac
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_atomic_multipoles_0 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_atomic_multipoles_0 ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.GRID:make_atomic_multipoles_0 ... no density_mx")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.GRID:make_atomic_multipoles_0 ... no restricted density_mx")
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.GRID:make_atomic_multipoles_0 ... not implememnted for spherical")
      off_diagonal = FALSE
      only_disconnected = FALSE
      no_s_functions = FALSE
      if (present(od)) off_diagonal = od
      if (present(oc)) only_disconnected = oc
      if (present(no_s)) no_s_functions = no_s
      call create_(self%charges,self%n_atom)
      call create_(self%dipoles,self%n_atom,3)
      call create_(self%quadrupoles,self%n_atom,6)
      call create_(self%octupoles,self%n_atom,10)
      self%charges = ZERO
      self%dipoles = ZERO
      self%quadrupoles = ZERO
      self%octupoles = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (atom_a==atom_b) then
            if (no_s_functions AND sh%a%l==0 AND sh%b%l==0) cycle
            if (off_diagonal) cycle
            if (only_disconnected) cycle
            fac = TWO
            if (fa==fb) fac = ONE
            call create_(S,na,nb)
            call create_(Ax,na,nb); call create_(Ay,na,nb); call create_(Az,na,nb)
            call create_(Axx,na,nb); call create_(Ayy,na,nb); call create_(Azz,na,nb)
            call create_(Axy,na,nb); call create_(Axz,na,nb); call create_(Ayz,na,nb)
            call create_(Axxx,na,nb); call create_(Ayyy,na,nb); call create_(Azzz,na,nb)
            call create_(Axxy,na,nb); call create_(Axxz,na,nb); call create_(Ayyx,na,nb)
            call create_(Ayyz,na,nb); call create_(Azzx,na,nb); call create_(Azzy,na,nb)
            call create_(Axyz,na,nb);
            call make_overlap_ints_(sh,S)
            call make_dipole_ints_(sh,Ax,Ay,Az,sh%a%position)
            call make_quadrupole_ints_(sh,Axx,Ayy,Azz,Axy,Axz,Ayz,sh%a%position)
            call make_octupole_ints_(sh,Axxx,Ayyy,Azzz,Axxy,Axxz,Ayyx,Ayyz,Azzx,Azzy,Axyz,sh%a%position)
            self%charges(atom_a) = self%charges(atom_a)+fac*sum(S*self%density_mx%r(fa:la,fb:lb))
            self%dipoles(atom_a,1) = self%dipoles(atom_a,1)+fac*sum(Ax*self%density_mx%r(fa:la,fb:lb))
            self%dipoles(atom_a,2) = self%dipoles(atom_a,2)+fac*sum(Ay*self%density_mx%r(fa:la,fb:lb))
            self%dipoles(atom_a,3) = self%dipoles(atom_a,3)+fac*sum(Az*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_a,1) = self%quadrupoles(atom_a,1)+fac*sum(Axx*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_a,2) = self%quadrupoles(atom_a,2)+fac*sum(Ayy*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_a,3) = self%quadrupoles(atom_a,3)+fac*sum(Azz*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_a,4) = self%quadrupoles(atom_a,4)+fac*sum(Axy*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_a,5) = self%quadrupoles(atom_a,5)+fac*sum(Axz*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_a,6) = self%quadrupoles(atom_a,6)+fac*sum(Ayz*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,1) = self%octupoles(atom_a,1)+fac*sum(Axxx*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,2) = self%octupoles(atom_a,2)+fac*sum(Ayyy*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,3) = self%octupoles(atom_a,3)+fac*sum(Azzz*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,4) = self%octupoles(atom_a,4)+fac*sum(Axxy*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,5) = self%octupoles(atom_a,5)+fac*sum(Axxz*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,6) = self%octupoles(atom_a,6)+fac*sum(Ayyx*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,7) = self%octupoles(atom_a,7)+fac*sum(Ayyz*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,8) = self%octupoles(atom_a,8)+fac*sum(Azzx*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,9) = self%octupoles(atom_a,9)+fac*sum(Azzy*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,10) = self%octupoles(atom_a,10)+fac*sum(Axyz*self%density_mx%r(fa:la,fb:lb))
            call destroy_(S)
            call destroy_(Ax); call destroy_(Ay); call destroy_(Az)
            call destroy_(Axx); call destroy_(Ayy); call destroy_(Azz)
            call destroy_(Axy); call destroy_(Axz); call destroy_(Ayz)
            call destroy_(Axxx); call destroy_(Ayyy); call destroy_(Azzz)
            call destroy_(Axxy); call destroy_(Axxz); call destroy_(Ayyx)
            call destroy_(Ayyz); call destroy_(Azzx); call destroy_(Azzy)
            call destroy_(Axyz);
         else
            if (only_disconnected) then
               if (has_a_connection_(atom_a,atom_b)) cycle
            end if

            ! Partition factors
            call create_(part,sh%a%n_cc,sh%b%n_cc)
            call make_KM_partition_(sh,part)

            ! Get weighted overlap integrals
            call create_(Sa,na,nb); call create_(Sb,na,nb)
            call make_weighted_overlap_ints_(sh,Sa,Sb,part)
            self%charges(atom_a) = self%charges(atom_a)+TWO*sum(Sa*self%density_mx%r(fa:la,fb:lb))
            self%charges(atom_b) = self%charges(atom_b)+TWO*sum(Sb*self%density_mx%r(fa:la,fb:lb))
            call destroy_(Sb); call destroy_(Sa)

            ! .dipoless
            call create_(Ax,na,nb); call create_(Ay,na,nb); call create_(Az,na,nb)
            call create_(Bx,na,nb); call create_(By,na,nb); call create_(Bz,na,nb)
            call make_weighted_dipole_ints_(sh,Ax,Ay,Az,Bx,By,Bz,part)
            self%dipoles(atom_a,1) = self%dipoles(atom_a,1)+TWO*sum(Ax*self%density_mx%r(fa:la,fb:lb))
            self%dipoles(atom_a,2) = self%dipoles(atom_a,2)+TWO*sum(Ay*self%density_mx%r(fa:la,fb:lb))
            self%dipoles(atom_a,3) = self%dipoles(atom_a,3)+TWO*sum(Az*self%density_mx%r(fa:la,fb:lb))
            self%dipoles(atom_b,1) = self%dipoles(atom_b,1)+TWO*sum(Bx*self%density_mx%r(fa:la,fb:lb))
            self%dipoles(atom_b,2) = self%dipoles(atom_b,2)+TWO*sum(By*self%density_mx%r(fa:la,fb:lb))
            self%dipoles(atom_b,3) = self%dipoles(atom_b,3)+TWO*sum(Bz*self%density_mx%r(fa:la,fb:lb))
            call destroy_(Ax); call destroy_(Ay); call destroy_(Az)
            call destroy_(Bx); call destroy_(By); call destroy_(Bz)

            ! .quadrupoless
            call create_(Axx,na,nb); call create_(Ayy,na,nb); call create_(Azz,na,nb)
            call create_(Axy,na,nb); call create_(Axz,na,nb); call create_(Ayz,na,nb)
            call create_(Bxx,na,nb); call create_(Byy,na,nb); call create_(Bzz,na,nb)
            call create_(Bxy,na,nb); call create_(Bxz,na,nb); call create_(Byz,na,nb)
            call make_weighted_quadrupole_ints_(sh,Axx,Ayy,Azz,Axy,Axz,Ayz,Bxx,Byy,Bzz,Bxy,Bxz,Byz,part)
            self%quadrupoles(atom_a,1) = self%quadrupoles(atom_a,1)+TWO*sum(Axx*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_a,2) = self%quadrupoles(atom_a,2)+TWO*sum(Ayy*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_a,3) = self%quadrupoles(atom_a,3)+TWO*sum(Azz*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_a,4) = self%quadrupoles(atom_a,4)+TWO*sum(Axy*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_a,5) = self%quadrupoles(atom_a,5)+TWO*sum(Axz*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_a,6) = self%quadrupoles(atom_a,6)+TWO*sum(Ayz*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_b,1) = self%quadrupoles(atom_b,1)+TWO*sum(Bxx*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_b,2) = self%quadrupoles(atom_b,2)+TWO*sum(Byy*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_b,3) = self%quadrupoles(atom_b,3)+TWO*sum(Bzz*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_b,4) = self%quadrupoles(atom_b,4)+TWO*sum(Bxy*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_b,5) = self%quadrupoles(atom_b,5)+TWO*sum(Bxz*self%density_mx%r(fa:la,fb:lb))
            self%quadrupoles(atom_b,6) = self%quadrupoles(atom_b,6)+TWO*sum(Byz*self%density_mx%r(fa:la,fb:lb))
            call destroy_(Axx); call destroy_(Ayy); call destroy_(Azz)
            call destroy_(Axy); call destroy_(Axz); call destroy_(Ayz)
            call destroy_(Bxx); call destroy_(Byy); call destroy_(Bzz)
            call destroy_(Bxy); call destroy_(Bxz); call destroy_(Byz)

            ! .octupoless
            call create_(Axxx,na,nb); call create_(Ayyy,na,nb); call create_(Azzz,na,nb)
            call create_(Axxy,na,nb); call create_(Axxz,na,nb); call create_(Ayyx,na,nb)
            call create_(Ayyz,na,nb); call create_(Azzx,na,nb); call create_(Azzy,na,nb)
            call create_(Axyz,na,nb);
            call create_(Bxxx,na,nb); call create_(Byyy,na,nb); call create_(Bzzz,na,nb)
            call create_(Bxxy,na,nb); call create_(Bxxz,na,nb); call create_(Byyx,na,nb)
            call create_(Byyz,na,nb); call create_(Bzzx,na,nb); call create_(Bzzy,na,nb)
            call create_(Bxyz,na,nb);
            call make_weighted_octupole_ints_(sh,Axxx,Ayyy,Azzz,Axxy,Axxz,Ayyx,Ayyz,Azzx,Azzy,Axyz,Bxxx,Byyy,Bzzz,Bxxy,Bxxz,Byyx,Byyz,Bzzx,Bzzy,Bxyz,part)
            self%octupoles(atom_a,1) = self%octupoles(atom_a,1)+TWO*sum(Axxx*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,2) = self%octupoles(atom_a,2)+TWO*sum(Ayyy*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,3) = self%octupoles(atom_a,3)+TWO*sum(Azzz*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,4) = self%octupoles(atom_a,4)+TWO*sum(Axxy*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,5) = self%octupoles(atom_a,5)+TWO*sum(Axxz*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,6) = self%octupoles(atom_a,6)+TWO*sum(Ayyx*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,7) = self%octupoles(atom_a,7)+TWO*sum(Ayyz*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,8) = self%octupoles(atom_a,8)+TWO*sum(Azzx*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,9) = self%octupoles(atom_a,9)+TWO*sum(Azzy*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_a,10) = self%octupoles(atom_a,10)+TWO*sum(Axyz*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_b,1) = self%octupoles(atom_b,1)+TWO*sum(Bxxx*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_b,2) = self%octupoles(atom_b,2)+TWO*sum(Byyy*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_b,3) = self%octupoles(atom_b,3)+TWO*sum(Bzzz*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_b,4) = self%octupoles(atom_b,4)+TWO*sum(Bxxy*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_b,5) = self%octupoles(atom_b,5)+TWO*sum(Bxxz*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_b,6) = self%octupoles(atom_b,6)+TWO*sum(Byyx*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_b,7) = self%octupoles(atom_b,7)+TWO*sum(Byyz*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_b,8) = self%octupoles(atom_b,8)+TWO*sum(Bzzx*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_b,9) = self%octupoles(atom_b,9)+TWO*sum(Bzzy*self%density_mx%r(fa:la,fb:lb))
            self%octupoles(atom_b,10) = self%octupoles(atom_b,10)+TWO*sum(Bxyz*self%density_mx%r(fa:la,fb:lb))
            call destroy_(Axxx); call destroy_(Ayyy); call destroy_(Azzz)
            call destroy_(Axxy); call destroy_(Axxz); call destroy_(Ayyx)
            call destroy_(Ayyz); call destroy_(Azzx); call destroy_(Azzy)
            call destroy_(Axyz);
            call destroy_(Bxxx); call destroy_(Byyy); call destroy_(Bzzz)
            call destroy_(Bxxy); call destroy_(Bxxz); call destroy_(Byyx)
            call destroy_(Byyz); call destroy_(Bzzx); call destroy_(Bzzy)
            call destroy_(Bxyz);
            call destroy_(part)
         end if
      end do

      ! Electron .charges
      self%dipoles = -ONE*self%dipoles
      self%quadrupoles = -ONE*self%quadrupoles
      self%octupoles = -ONE*self%octupoles
   end subroutine

   PURE subroutine make_atomic_multipoles_1(self,charge,dipole,quadrupole,octupole,od,oc,no_s)
   ! Make the atomic multipoles using the tanaka partitioing
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: charge
      MAT(REAL,:,:), OUT :: dipole,quadrupole,octupole
      BIN, optional, IN :: od,oc,no_s
      BIN :: off_diagonal,only_disconnected,no_s_functions
      MAT(REAL,:,:), allocatable :: S,Sa,Sb,part
      MAT(REAL,:,:), allocatable :: Ax,Ay,Az
      MAT(REAL,:,:), allocatable :: Bx,By,Bz
      MAT(REAL,:,:), allocatable :: Axx,Ayy,Azz,Axy,Axz,Ayz
      MAT(REAL,:,:), allocatable :: Bxx,Byy,Bzz,Bxy,Bxz,Byz
      MAT(REAL,:,:), allocatable :: Axxx,Ayyy,Azzz,Axxy,Axxz,Ayyx,Ayyz,Azzx,Azzy,Axyz
      MAT(REAL,:,:), allocatable :: Bxxx,Byyy,Bzzz,Bxxy,Bxxz,Byyx,Byyz,Bzzx,Bzzy,Bxyz
      INT :: q,fa,la,na,fb,lb,nb,atom_a,atom_b
      type(SHELL2_TYPE) :: sh
      REAL :: fac
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_atomic_multipoles_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_atomic_multipoles_1 ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.GRID:make_atomic_multipoles_1 ... no density_mx")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.GRID:make_atomic_multipoles_1 ... no restricted density_mx")
   ENSURE(NOT self%use_spherical_basis,"MOLECULE.GRID:make_atomic_multipoles_1 ... not implememnted for spherical")
      off_diagonal = FALSE
      only_disconnected = FALSE
      no_s_functions = FALSE
      if (present(od)) off_diagonal = od
      if (present(oc)) only_disconnected = oc
      if (present(no_s)) no_s_functions = no_s
      charge = ZERO
      dipole = ZERO
      quadrupole = ZERO
      octupole = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (atom_a==atom_b) then
            if (no_s_functions AND sh%a%l==0 AND sh%b%l==0) cycle
            if (off_diagonal) cycle
            if (only_disconnected) cycle
            fac = TWO
            if (fa==fb) fac = ONE
            call create_(S,na,nb)
            call create_(Ax,na,nb); call create_(Ay,na,nb); call create_(Az,na,nb)
            call create_(Axx,na,nb); call create_(Ayy,na,nb); call create_(Azz,na,nb)
            call create_(Axy,na,nb); call create_(Axz,na,nb); call create_(Ayz,na,nb)
            call create_(Axxx,na,nb); call create_(Ayyy,na,nb); call create_(Azzz,na,nb)
            call create_(Axxy,na,nb); call create_(Axxz,na,nb); call create_(Ayyx,na,nb)
            call create_(Ayyz,na,nb); call create_(Azzx,na,nb); call create_(Azzy,na,nb)
            call create_(Axyz,na,nb);
            call make_overlap_ints_(sh,S)
            call make_dipole_ints_(sh,Ax,Ay,Az,sh%a%position)
            call make_quadrupole_ints_(sh,Axx,Ayy,Azz,Axy,Axz,Ayz,sh%a%position)
            call make_octupole_ints_(sh,Axxx,Ayyy,Azzz,Axxy,Axxz,Ayyx,Ayyz,Azzx,Azzy,Axyz,sh%a%position)
            charge(atom_a) = charge(atom_a)+fac*sum(S*self%density_mx%r(fa:la,fb:lb))
            dipole(atom_a,1) = dipole(atom_a,1)+fac*sum(Ax*self%density_mx%r(fa:la,fb:lb))
            dipole(atom_a,2) = dipole(atom_a,2)+fac*sum(Ay*self%density_mx%r(fa:la,fb:lb))
            dipole(atom_a,3) = dipole(atom_a,3)+fac*sum(Az*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_a,1) = quadrupole(atom_a,1)+fac*sum(Axx*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_a,2) = quadrupole(atom_a,2)+fac*sum(Ayy*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_a,3) = quadrupole(atom_a,3)+fac*sum(Azz*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_a,4) = quadrupole(atom_a,4)+fac*sum(Axy*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_a,5) = quadrupole(atom_a,5)+fac*sum(Axz*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_a,6) = quadrupole(atom_a,6)+fac*sum(Ayz*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,1) = octupole(atom_a,1)+fac*sum(Axxx*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,2) = octupole(atom_a,2)+fac*sum(Ayyy*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,3) = octupole(atom_a,3)+fac*sum(Azzz*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,4) = octupole(atom_a,4)+fac*sum(Axxy*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,5) = octupole(atom_a,5)+fac*sum(Axxz*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,6) = octupole(atom_a,6)+fac*sum(Ayyx*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,7) = octupole(atom_a,7)+fac*sum(Ayyz*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,8) = octupole(atom_a,8)+fac*sum(Azzx*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,9) = octupole(atom_a,9)+fac*sum(Azzy*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,10) = octupole(atom_a,10)+fac*sum(Axyz*self%density_mx%r(fa:la,fb:lb))
            call destroy_(S)
            call destroy_(Ax); call destroy_(Ay); call destroy_(Az)
            call destroy_(Axx); call destroy_(Ayy); call destroy_(Azz)
            call destroy_(Axy); call destroy_(Axz); call destroy_(Ayz)
            call destroy_(Axxx); call destroy_(Ayyy); call destroy_(Azzz)
            call destroy_(Axxy); call destroy_(Axxz); call destroy_(Ayyx)
            call destroy_(Ayyz); call destroy_(Azzx); call destroy_(Azzy)
            call destroy_(Axyz)
         else
            if (only_disconnected) then
               if (has_a_connection_(atom_a,atom_b)) cycle
            end if

            ! Partition factors
            call create_(part,sh%a%n_cc,sh%b%n_cc)
            call make_KM_partition_(sh,part)

            ! Get weighted overlap integrals
            call create_(Sa,na,nb); call create_(Sb,na,nb)
            call make_weighted_overlap_ints_(sh,Sa,Sb,part)
            charge(atom_a) = charge(atom_a)+TWO*sum(Sa*self%density_mx%r(fa:la,fb:lb))
            charge(atom_b) = charge(atom_b)+TWO*sum(Sb*self%density_mx%r(fa:la,fb:lb))
            call destroy_(Sb); call destroy_(Sa)

            ! Dipoles
            call create_(Ax,na,nb); call create_(Ay,na,nb); call create_(Az,na,nb)
            call create_(Bx,na,nb); call create_(By,na,nb); call create_(Bz,na,nb)
            call make_weighted_dipole_ints_(sh,Ax,Ay,Az,Bx,By,Bz,part)
            dipole(atom_a,1) = dipole(atom_a,1)+TWO*sum(Ax*self%density_mx%r(fa:la,fb:lb))
            dipole(atom_a,2) = dipole(atom_a,2)+TWO*sum(Ay*self%density_mx%r(fa:la,fb:lb))
            dipole(atom_a,3) = dipole(atom_a,3)+TWO*sum(Az*self%density_mx%r(fa:la,fb:lb))
            dipole(atom_b,1) = dipole(atom_b,1)+TWO*sum(Bx*self%density_mx%r(fa:la,fb:lb))
            dipole(atom_b,2) = dipole(atom_b,2)+TWO*sum(By*self%density_mx%r(fa:la,fb:lb))
            dipole(atom_b,3) = dipole(atom_b,3)+TWO*sum(Bz*self%density_mx%r(fa:la,fb:lb))
            call destroy_(Ax); call destroy_(Ay); call destroy_(Az)
            call destroy_(Bx); call destroy_(By); call destroy_(Bz)

            ! Quadrupoles
            call create_(Axx,na,nb); call create_(Ayy,na,nb); call create_(Azz,na,nb)
            call create_(Axy,na,nb); call create_(Axz,na,nb); call create_(Ayz,na,nb)
            call create_(Bxx,na,nb); call create_(Byy,na,nb); call create_(Bzz,na,nb)
            call create_(Bxy,na,nb); call create_(Bxz,na,nb); call create_(Byz,na,nb)
            call make_weighted_quadrupole_ints_(sh,Axx,Ayy,Azz,Axy,Axz,Ayz,Bxx,Byy,Bzz,Bxy,Bxz,Byz,part)
            quadrupole(atom_a,1) = quadrupole(atom_a,1)+TWO*sum(Axx*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_a,2) = quadrupole(atom_a,2)+TWO*sum(Ayy*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_a,3) = quadrupole(atom_a,3)+TWO*sum(Azz*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_a,4) = quadrupole(atom_a,4)+TWO*sum(Axy*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_a,5) = quadrupole(atom_a,5)+TWO*sum(Axz*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_a,6) = quadrupole(atom_a,6)+TWO*sum(Ayz*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_b,1) = quadrupole(atom_b,1)+TWO*sum(Bxx*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_b,2) = quadrupole(atom_b,2)+TWO*sum(Byy*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_b,3) = quadrupole(atom_b,3)+TWO*sum(Bzz*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_b,4) = quadrupole(atom_b,4)+TWO*sum(Bxy*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_b,5) = quadrupole(atom_b,5)+TWO*sum(Bxz*self%density_mx%r(fa:la,fb:lb))
            quadrupole(atom_b,6) = quadrupole(atom_b,6)+TWO*sum(Byz*self%density_mx%r(fa:la,fb:lb))
            call destroy_(Axx); call destroy_(Ayy); call destroy_(Azz)
            call destroy_(Axy); call destroy_(Axz); call destroy_(Ayz)
            call destroy_(Bxx); call destroy_(Byy); call destroy_(Bzz)
            call destroy_(Bxy); call destroy_(Bxz); call destroy_(Byz)

            ! Octupoles
            call create_(Axxx,na,nb); call create_(Ayyy,na,nb); call create_(Azzz,na,nb)
            call create_(Axxy,na,nb); call create_(Axxz,na,nb); call create_(Ayyx,na,nb)
            call create_(Ayyz,na,nb); call create_(Azzx,na,nb); call create_(Azzy,na,nb)
            call create_(Axyz,na,nb);
            call create_(Bxxx,na,nb); call create_(Byyy,na,nb); call create_(Bzzz,na,nb)
            call create_(Bxxy,na,nb); call create_(Bxxz,na,nb); call create_(Byyx,na,nb)
            call create_(Byyz,na,nb); call create_(Bzzx,na,nb); call create_(Bzzy,na,nb)
            call create_(Bxyz,na,nb);
            call make_weighted_octupole_ints_(sh,Axxx,Ayyy,Azzz,Axxy,Axxz,Ayyx,Ayyz,Azzx,Azzy,Axyz,Bxxx,Byyy,Bzzz,Bxxy,Bxxz,Byyx,Byyz,Bzzx,Bzzy,Bxyz,part)
            octupole(atom_a,1) = octupole(atom_a,1)+TWO*sum(Axxx*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,2) = octupole(atom_a,2)+TWO*sum(Ayyy*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,3) = octupole(atom_a,3)+TWO*sum(Azzz*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,4) = octupole(atom_a,4)+TWO*sum(Axxy*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,5) = octupole(atom_a,5)+TWO*sum(Axxz*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,6) = octupole(atom_a,6)+TWO*sum(Ayyx*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,7) = octupole(atom_a,7)+TWO*sum(Ayyz*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,8) = octupole(atom_a,8)+TWO*sum(Azzx*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,9) = octupole(atom_a,9)+TWO*sum(Azzy*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_a,10) = octupole(atom_a,10)+TWO*sum(Axyz*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_b,1) = octupole(atom_b,1)+TWO*sum(Bxxx*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_b,2) = octupole(atom_b,2)+TWO*sum(Byyy*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_b,3) = octupole(atom_b,3)+TWO*sum(Bzzz*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_b,4) = octupole(atom_b,4)+TWO*sum(Bxxy*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_b,5) = octupole(atom_b,5)+TWO*sum(Bxxz*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_b,6) = octupole(atom_b,6)+TWO*sum(Byyx*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_b,7) = octupole(atom_b,7)+TWO*sum(Byyz*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_b,8) = octupole(atom_b,8)+TWO*sum(Bzzx*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_b,9) = octupole(atom_b,9)+TWO*sum(Bzzy*self%density_mx%r(fa:la,fb:lb))
            octupole(atom_b,10) = octupole(atom_b,10)+TWO*sum(Bxyz*self%density_mx%r(fa:la,fb:lb))
            call destroy_(Axxx); call destroy_(Ayyy); call destroy_(Azzz)
            call destroy_(Axxy); call destroy_(Axxz); call destroy_(Ayyx)
            call destroy_(Ayyz); call destroy_(Azzx); call destroy_(Azzy)
            call destroy_(Axyz);
            call destroy_(Bxxx); call destroy_(Byyy); call destroy_(Bzzz)
            call destroy_(Bxxy); call destroy_(Bxxz); call destroy_(Byyx)
            call destroy_(Byyz); call destroy_(Bzzx); call destroy_(Bzzy)
            call destroy_(Bxyz);
            call destroy_(part)
         end if
      end do

      ! Electron charge
      dipole = -ONE*dipole
      quadrupole = -ONE*quadrupole
      octupole = -ONE*octupole
   end subroutine

   subroutine make_def_e_pot_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_e_pot_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_e_pot_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_EP_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_e_pot_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   NON_MPI_PURE subroutine make_pro_e_pot_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_e_pot_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_e_pot_grid")
         call make_EP_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_pro_e_pot_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine


!  ==========================================
!  ESPs from center and surrounding molecules
!  ===========================================

   NON_MPI_PURE subroutine make_the_center_ESP_grid(self,grid,pt)
   ! Make the central neighboring-molecule ESP "grid",
   ! i.e. for atom groups>1, on a series of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_the_center_ESP_grid ... no basis info")
      if (trim(number_kind_(self%density_mx))=="real") then
         call make_the_center_ESP_grid_r_(self,grid,pt)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_the_center_ESP_grid_r(self,grid,pt)
   ! Make the central neighboring-molecule ESP "grid",
   ! i.e. for atom groups>1, on a series of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%atom_group),"MOLECULE.GRID:make_the_center_ESP_grid_r ... no atom group information")
   ENSURE(size(self%atom_group)>=2,"MOLECULE.GRID:make_the_center_ESP_grid_r ... must have more than 2 groups")
   ENSURE(allocated(self%mol(1)%density_mx),"MOLECULE.GRID:make_the_center_ESP_grid_r ... no mol 1 DM!")
   ENSURE(allocated(self%mol(2)%density_mx),"MOLECULE.GRID:make_the_center_ESP_grid_r ... no mol 2 DM!")

      ! ESP for central molecule
      call make_EP_grid_(self%mol(1),grid,pt)
   end subroutine

   NON_MPI_PURE subroutine make_non_center_ESP_grid(self,grid,pt)
   ! Make the non-central neighboring-molecule ESP "grid",
   ! i.e. for atom groups>1, on a series of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_non_center_ESP_grid ... no basis info")
      if (trim(number_kind_(self%density_mx))=="real") then
         call make_non_center_ESP_grid_r_(self,grid,pt)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_non_center_ESP_grid_r(self,grid,pt)
   ! Make the non-central neighboring-molecule ESP "grid",
   ! i.e. for atom groups>1, on a series of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: ESP
      INT :: m
   ENSURE(allocated(self%atom_group),"MOLECULE.GRID:make_non_center_ESP_grid_r ... no atom group information")
   ENSURE(size(self%atom_group)>=2,"MOLECULE.GRID:make_non_center_ESP_grid_r ... must have more than 2 groups")
   ENSURE(allocated(self%mol(1)%density_mx),"MOLECULE.GRID:make_non_center_ESP_grid_r ... no mol 1 DM!")
   ENSURE(allocated(self%mol(2)%density_mx),"MOLECULE.GRID:make_non_center_ESP_grid_r ... no mol 2 DM!")
      call create_(ESP,size(grid))
      grid = ZERO

      ! Loop over non-central molecules
      do m = 2,size(self%atom_group)

         ! Make ESP for molecule m
         call make_EP_grid_(self%mol(m),ESP,pt)
         grid = grid+ESP
      end do
      call destroy_(ESP)
   end subroutine


!  ==========================================
!  Product of ESPs from surrounding molecules
!  ==========================================

   NON_MPI_PURE subroutine make_product_ESP_grid(self,grid,pt)
   ! Make the product of the central and  neighboring-molecule
   ! ESP "grid", on a series of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_product_ESP_grid ... no basis info")
      call make_product_ESP_grid_r_(self,grid,pt)
   end subroutine

   NON_MPI_PURE subroutine make_product_ESP_grid_r(self,grid,pt)
   ! Make the product of the central and  neighboring-molecule
   ! ESP "grid", on a series of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: ESP
      INT :: m
   ENSURE(allocated(self%atom_group),"MOLECULE.GRID:make_product_ESP_grid_r ... no atom group information")
   ENSURE(size(self%atom_group)>=2,"MOLECULE.GRID:make_product_ESP_grid_r ... must have more than 2 groups")
   ENSURE(allocated(self%mol(1)%density_mx),"MOLECULE.GRID:make_product_ESP_grid_r ... no mol 1 DM!")
   ENSURE(allocated(self%mol(2)%density_mx),"MOLECULE.GRID:make_product_ESP_grid_r ... no mol 2 DM!")
      call create_(ESP,size(grid))

      ! Loop over non-central molecules
      grid = ZERO
      do m = 2,size(self%atom_group)

         ! Make ESP for molecule m
         call make_EP_grid_(self%mol(m),ESP,pt)
         grid = grid+ESP
      end do

      ! Central molecule ESP
      call make_EP_grid_(self%mol(1),ESP,pt)

      ! Make product
      grid = grid*ESP
      call destroy_(ESP)
   end subroutine

   NON_MPI_PURE subroutine make_group_product_ESP_grid(self,grid,pt)
   ! Make the product of the central and all neighboring-molecule
   ! ESP "grid"s, pairwise on each fragment patch, on a series of
   ! points "pt". The points are divided into fragment patches first.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_group_product_ESP_grid ... no basis info")
      call make_group_product_ESP_grid_r_(self,grid,pt)
   end subroutine

   NON_MPI_PURE subroutine make_group_product_ESP_grid_r(self,grid,pt)
   ! Make the product of the central and all neighboring-molecule
   ! ESP "grid"s, pairwise on each fragment patch, on a series of
   ! points "pt". The points are divided into fragment patches first.
   ! This is for surface properties only, not isosurfaces!
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(type(EVEC_INT_TYPE),:), allocatable :: list
      VEC(REAL,:), allocatable :: ESP
      MAT(REAL,:,:), allocatable :: ptg
      INT :: g
   ENSURE(allocated(self%atom_group),"MOLECULE.GRID:make_group_product_ESP_grid_r ... no atom group information")
   ENSURE(allocated(self%isosurface),"MOLECULE.GRID:make_group_product_ESP_grid_r ... no isosurface")
      call create_(list,size(pt,2))
      call make_pts_list_for_group_(self%isosurface,list)
      grid = ZERO

      ! Loop over non-central molecules
      do g = 2,size(self%atom_group)
         if (size(list(g)%element)==0) cycle

         ! Get the fragment g patch
         ptg = pt(list(g)%element,:)

         ! Make ESP on patch g for molecule m
         call create_(ESP,size(list(g)%element))
         call make_EP_grid_(self%mol(g),ESP,ptg)
         grid(list(g)%element(:)) = ESP

         ! Clean
         call destroy_(ESP)
         call destroy_(ptg)
      end do

      ! Central molecule ESP; use all points pt
      call create_(ESP,size(pt))
      call make_EP_grid_(self%mol(1),ESP,pt)

      ! Make product
      grid = grid*ESP

      ! Clean
      call destroy_(ESP)
      call destroy_(list)
   end subroutine


!  ==========================================
!  Product E field grid surrounding molecules
!  ==========================================

   NON_MPI_PURE subroutine make_product_E_field_grid(self,grid,pt)
   ! Make the product of the central and neighboring-molecule
   ! E fields in "grid", on a series of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_product_E_field_grid ... no basis info")
      call make_product_E_field_grid_r_(self,grid,pt)
   end subroutine

   NON_MPI_PURE subroutine make_product_E_field_grid_r(self,grid,pt)
   ! Make the product of the central and neighboring-molecule
   ! densities and ESPs on "grid", on a series of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: E1,E2
      INT :: m,n_pt
   ENSURE(allocated(self%atom_group),"MOLECULE.GRID:make_product_E_field_grid_r ... no atom group information")
   ENSURE(size(self%atom_group)>=2,"MOLECULE.GRID:make_product_E_field_grid_r ... must have more than 2 groups")
   ENSURE(allocated(self%mol(1)%density_mx),"MOLECULE.GRID:make_product_E_field_grid_r ... no mol 1 DM!")
   ENSURE(allocated(self%mol(2)%density_mx),"MOLECULE.GRID:make_product_E_field_grid_r ... no mol 2 DM!")
      n_pt = size(grid)

      ! E fields for first and surrounding molecules
      call create_(E1,n_pt,3)
      call create_(E2,n_pt,3)

      ! Loop over non-central molecules
      E2 = ZERO
      do m = 2,size(self%atom_group)

         ! Make E field for molecules m>2
         call make_E_field_grid_(self%mol(m),E1,pt)
         E2 = E2+E1
      end do

      ! Central molecule E-field
      call make_E_field_grid_(self%mol(1),E1,pt)

      ! Product of E fields
      grid = E1(:,1)*E2(:,1)+E1(:,2)*E2(:,2)+E1(:,3)*E2(:,3)

      ! Clean
      call destroy_(E2)
      call destroy_(E1)
   end subroutine

   NON_MPI_PURE subroutine make_group_product_E_field_grid(self,grid,pt)
   ! Make the product of the central and neighboring-molecule
   ! E fields in "grid", on a series of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_group_product_E_field_grid ... no basis info")
      call make_group_product_E_field_grid_r_(self,grid,pt)
   end subroutine

   NON_MPI_PURE subroutine make_group_product_E_field_grid_r(self,grid,pt)
   ! Make the product of the central and all neighboring-molecule
   ! E-field "grid"s, pairwise on each fragment patch, on a series of
   ! points "pt". The points are divided into fragment patches first.
   ! This is for surface properties only, not isosurfaces!
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(type(EVEC_INT_TYPE),:), allocatable :: list
      MAT(REAL,:,:), allocatable :: E1,E2
      MAT(REAL,:,:), allocatable :: ptg
      INT :: g,n_pt
   ENSURE(allocated(self%atom_group),"MOLECULE.GRID:make_group_product_E_field_grid_r ... no atom group information")
   ENSURE(allocated(self%isosurface),"MOLECULE.GRID:make_group_product_E_field_grid_r ... no isosurface")
      n_pt = size(grid)
      call create_(list,n_pt)
      call make_pts_list_for_group_(self%isosurface,list)

      ! E fields for surrounding molecules
      allocate(E2(n_pt,3))
      E2 = ZERO

      ! Loop over non-central molecules
      do g = 2,size(self%atom_group)

         ! Get the fragment g patch
         ptg = pt(:,list(g)%element)

         ! Make ESP on patch g for molecule m
         allocate(E1(size(list(g)%element),3))
         call make_E_field_grid_(self%mol(g),E1,ptg)
         E2(list(g)%element(:),:) = E1

         ! Clean
         deallocate(E1)
         call destroy_(ptg)
      end do

      ! Central molecule E-field; use all points pt
      allocate(E1(n_pt,3))
      call make_E_field_grid_(self%mol(1),E1,pt)

      ! Product of E fields
      grid = E1(:,1)*E2(:,1)+E1(:,2)*E2(:,2)+E1(:,3)*E2(:,3)

      ! Clean
      deallocate(E1)
      deallocate(E2)
      call destroy_(list)
   end subroutine


!  ================
!  DFT xc potential
!  ================

   PURE subroutine make_DFT_xc_pot_grid(self,V,pt)
   ! Work out the DFT potential on ".plot_grid" using ".natural orbitals" and the
   ! ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: V
      MAT(REAL,:,:), IN :: pt
      type(DFT_FUNCTIONAL_TYPE) :: dft
      type(XC_POTENTIAL_TYPE) :: pot
      VEC(REAL,:), allocatable :: rho
      MAT(REAL,:,:), allocatable :: grad_rho
      STR(len=STR_SIZE) :: exch,corr
      BIN :: is_GGA
   ENSURE(allocated(self%SCF_data),"MOLECULE.GRID:make_DFT_xc_pot_grid ... no scf data, for DFT functional")
   ENSURE(is_DFT_calculation_(self%SCF_data),"MOLECULE.GRID:make_DFT_xc_pot_grid ... so far, must be a DFT calculation")
      call create_(rho,size(V))
      call make_ED_grid_(self,rho,pt)
      call set_defaults_(dft)
      exch = self%SCF_data%DFT_exchange_functional
      corr = self%SCF_data%DFT_correlation_functional
      is_GGA = is_GGA_functional_(dft,exch)
      is_GGA = is_GGA OR is_GGA_functional_(dft,corr)
      if (is_GGA) then
         call create_(grad_rho,size(V),3)
         call make_nabla_ED_grid_(self,grad_rho,pt)
         call make_r_energy_and_potential_(dft,pot,exch,corr,rho,grad_rho)
         call destroy_(grad_rho)
      else
         call make_r_energy_and_potential_(dft,pot,exch,corr,rho)
      end if
      V = pot%v_rho(:,1)
      call destroy_ptr_part_(pot)
      call destroy_(rho)
   end subroutine

   PURE subroutine make_def_DFT_xc_pot_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_DFT_xc_pot_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_DFT_xc_pot_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_DFT_xc_pot_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_DFT_xc_pot_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   PURE subroutine make_pro_DFT_xc_pot_grid(self,density_grid,pt)
   ! Return promolecule DFT "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      STR(len=STR_SIZE) :: exch,corr
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_DFT_xc_pot_grid ... no atoms")
   ENSURE(has_all_density_data_(self%atom),"MOLECULE.GRID:make_pro_DFT_xc_pot_grid ... no atomic orbitals")
   ENSURE(allocated(self%SCF_data),"MOLECULE.GRID:make_pro_DFT_xc_pot_grid ... no scf data, for DFT functional")
   ENSURE(is_DFT_calculation_(self%SCF_data),"MOLECULE.GRID:make_pro_DFT_xc_pot_grid ... so far, must be a DFT calculation")

      ! Create

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Make the grid of values
      exch = self%SCF_data%DFT_exchange_functional
      corr = self%SCF_data%DFT_correlation_functional
      density_grid = ZERO
      do a = 1,self%n_atom
         call make_DFT_xc_pot_grid_(self%atom(a),grid,pt,exch,corr)
         density_grid = density_grid+grid
      end do

      ! Clean
      call destroy_(grid)
   end subroutine


!  =======================
!  DFT effective potential
!  =======================

! The effective potential includes the classical electrostatic potential

   NON_MPI_PURE subroutine make_DFT_eff_pot_grid(self,V,pt)
   ! Work out the Slater DFT potential "V" (i.e. the Slater potential plus the
   ! classical electric potential) on a grid of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: V
      MAT(REAL,:,:), IN :: pt
      type(DFT_FUNCTIONAL_TYPE) :: dft
      type(XC_POTENTIAL_TYPE) :: pot
      STR(len=STR_SIZE) :: exch,corr
      VEC(REAL,:), allocatable :: rho
   ENSURE(allocated(self%SCF_data),"MOLECULE.GRID:make_DFT_eff_pot_grid ... no scf data, for DFT functional")
   ENSURE(is_DFT_calculation_(self%SCF_data),"MOLECULE.GRID:make_DFT_eff_pot_grid ... so far, must be a DFT calculation")
   ENSURE(NOT using_GGA_functional_(self%SCF_data),"MOLECULE.GRID:make_DFT_eff_pot_grid ... so far, must not be a GGA functional")
      call create_(rho,size(V))
      call make_EP_grid_(self,V,pt)
      call make_ED_grid_(self,rho,pt)
      exch = self%SCF_data%DFT_exchange_functional
      corr = self%SCF_data%DFT_correlation_functional
      call set_defaults_(dft)
      call make_r_energy_and_potential_(dft,pot,exch,corr,rho)
      V = V+pot%v_rho(:,1)
      call destroy_ptr_part_(pot)
      call destroy_(rho)
   end subroutine

   NON_MPI_PURE subroutine make_def_DFT_eff_pot_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_DFT_eff_pot_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_DFT_eff_pot_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_DFT_eff_pot_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_DFT_eff_pot_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   PURE subroutine make_pro_DFT_eff_pot_grid(self,density_grid,pt)
   ! Return promolecule DFT "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      STR(len=STR_SIZE) :: exch,corr
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_DFT_eff_pot_grid ... no atoms")
   ENSURE(has_all_density_data_(self%atom),"MOLECULE.GRID:make_pro_DFT_eff_pot_grid ... no atomic orbitals")
   ENSURE(allocated(self%SCF_data),"MOLECULE.GRID:make_pro_DFT_eff_pot_grid ... no scf data, for DFT functional")
   ENSURE(is_DFT_calculation_(self%SCF_data),"MOLECULE.GRID:make_pro_DFT_eff_pot_grid ... so far, must be a DFT calculation")

      ! Create

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Make the grid of values
      exch = self%SCF_data%DFT_exchange_functional
      corr = self%SCF_data%DFT_correlation_functional
      density_grid = ZERO
      do a = 1,self%n_atom
         call make_DFT_eff_pot_grid_(self%atom(a),grid,pt,exch,corr)
         density_grid = density_grid+grid
      end do

      ! Clean
      call destroy_(grid)
   end subroutine


!  ==================
!  DFT energy density
!  ==================

! The energy density includes the classical electrostatic potential, but no
! kinetic energy density terms

   NON_MPI_PURE subroutine make_DFT_energy_grid(self,V,pt)
   ! Work out the Slater DFT energy "V" (i.e. the Slater potential times the
   ! density plus the classical electric energy) on a grid of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: V
      MAT(REAL,:,:), IN :: pt
      type(DFT_FUNCTIONAL_TYPE) :: dft
      type(XC_POTENTIAL_TYPE) :: pot
      STR(len=STR_SIZE) :: exch,corr
      VEC(REAL,:), allocatable :: Ve,rho
   ENSURE(allocated(self%SCF_data),"MOLECULE.GRID:make_DFT_energy_grid ... no scf data, for DFT functional")
   ENSURE(is_DFT_calculation_(self%SCF_data),"MOLECULE.GRID:make_DFT_energy_grid ... so far, must be a DFT calculation")
      call create_(Ve,size(V))
      call create_(rho,size(V))
      call nuclear_potential_(self,V,pt)
      call make_electronic_pot_grid_(self,Ve,pt)
      V = -V-HALF*Ve
      call make_ED_grid_(self,rho,pt)
      exch = self%SCF_data%DFT_exchange_functional
      corr = self%SCF_data%DFT_correlation_functional
      call set_defaults_(dft)
      call make_r_energy_and_potential_(dft,pot,exch,corr,rho)
      V = V+pot%energy
      V = V*rho
      call destroy_ptr_part_(pot)
      call destroy_(Ve)
      call destroy_(rho)
   end subroutine

   NON_MPI_PURE subroutine make_def_DFT_energy_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_DFT_energy_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_DFT_energy_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_DFT_energy_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_DFT_energy_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   PURE subroutine make_pro_DFT_energy_grid(self,density_grid,pt)
   ! Return promolecule DFT "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      STR(len=STR_SIZE) :: exch,corr
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_DFT_energy_grid ... no atoms")
   ENSURE(has_all_density_data_(self%atom),"MOLECULE.GRID:make_pro_DFT_energy_grid ... no atomic orbitals")
   ENSURE(allocated(self%SCF_data),"MOLECULE.GRID:make_pro_DFT_energy_grid ... no scf data, for DFT functional")
   ENSURE(is_DFT_calculation_(self%SCF_data),"MOLECULE.GRID:make_pro_DFT_energy_grid ... so far, must be a DFT calculation")

      ! Create

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Make the grid of values
      exch = self%SCF_data%DFT_exchange_functional
      corr = self%SCF_data%DFT_correlation_functional
      density_grid = ZERO
      do a = 1,self%n_atom
         call make_DFT_energy_grid_(self%atom(a),grid,pt,exch,corr)
         density_grid = density_grid+grid
      end do

      ! Clean
      call destroy_(grid)
   end subroutine


!  =========================
!  H-type DFT energy density
!  =========================

! H = G + V(DFT)

   NON_MPI_PURE subroutine make_H_DFT_energy_grid(self,H_grid,pt)
   ! Work out the property density on ".plot_grid" using ".natural
   ! orbitals" and the ".NO_occ_nos" vector. A Gnuplot ascii
   ! file is generated.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: H_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: G_grid
   ENSURE(allocated(self%SCF_data),"MOLECULE.GRID:make_H_DFT_energy_grid ... no scf data, for DFT functional")
   ENSURE(is_DFT_calculation_(self%SCF_data),"MOLECULE.GRID:make_H_DFT_energy_grid ... must be a DFT calculation")
      call create_(G_grid,self%plot_grid%n_pt)
      call make_G_kinetic_energy_grid_(self,G_grid,pt)
      call make_DFT_energy_grid_(self,H_grid,pt)
      H_grid = H_grid+G_grid
      call destroy_(G_grid)
   end subroutine

   NON_MPI_PURE subroutine make_def_H_DFT_energy_grid(self,grid,pt)
   ! Make deformation density "grid" on a set of points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: promol_grid
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_def_H_DFT_energy_grid ... no natural orbitals")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.GRID:make_def_H_DFT_energy_grid ... no natural orbitals")

      ! Make the normal grid values

      ! The following code is inherited from MOLECULE.GRID
      call make_H_DFT_energy_grid_(self,grid,pt)

      ! Make the promolecule grid values
      call create_(promol_grid,size(grid))
      call make_pro_H_DFT_energy_grid_(self,promol_grid,pt)

      ! Subtract to form deformation grid
      grid = grid-promol_grid

      ! Clean up
      call destroy_(promol_grid)
   end subroutine

   PURE subroutine make_pro_H_DFT_energy_grid(self,density_grid,pt)
   ! Return promolecule DFT "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      STR(len=STR_SIZE) :: exch,corr
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_pro_H_DFT_energy_grid ... no atoms")
   ENSURE(has_all_density_data_(self%atom),"MOLECULE.GRID:make_pro_H_DFT_energy_grid ... no atomic orbitals")
   ENSURE(allocated(self%SCF_data),"MOLECULE.GRID:make_pro_H_DFT_energy_grid ... no scf data, for DFT functional")
   ENSURE(is_DFT_calculation_(self%SCF_data),"MOLECULE.GRID:make_pro_H_DFT_energy_grid ... so far, must be a DFT calculation")

      ! Create

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Make the grid of values
      exch = self%SCF_data%DFT_exchange_functional
      corr = self%SCF_data%DFT_correlation_functional
      density_grid = ZERO
      do a = 1,self%n_atom
         call make_H_DFT_energy_grid_(self%atom(a),grid,pt,exch,corr)
         density_grid = density_grid+grid
      end do

      ! Clean
      call destroy_(grid)
   end subroutine


!  =======================
!  Hirshfeld (slater) grid
!  =======================

! This is the fast one

   PURE subroutine make_Hirshfeld_density_grid(self,density_grid,pt)
   ! WARNING: this version calls directly the interpolated slaterbasis
   ! routine.  Make the Hirshfeld stockholder "density_grid" for the
   ! supplied points "pt". The promolecule electron density used to
   ! define the Hirshfeld surface is calculated using a tabulated
   ! slater basis set.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt

!  ENSURE(.saved.allocated, "no saved molecule")
!  ENSURE(.saved.cluster.allocated, "no saved cluster data")
!  ENSURE(.saved.cluster.info_made, "no saved cluster info")
!  ENSURE(.saved.cluster.radius>ZERO, "saved cluster radius is zero")
!  ENSURE(.saved.cluster.n_atoms==.atom.dim,"inconsistent saved cluster and atom list")
!  ENSURE(.atom.allocated,"no atoms")
!  ENSURE(.atom.has_all_density_data,"no atomic orbitals")
      VEC(REAL,:), allocatable :: rho_fragment
      VEC(BIN,:), allocatable :: is_fragment_atom
      INT :: n_atoms,n_pt,a
      n_pt = size(density_grid)
      n_atoms = size(self%atom)
      call create_(rho_fragment,n_pt)
      rho_fragment = ZERO
      is_fragment_atom = self%saved%cluster%is_fragment_atom
      if (NOT has_disorder_(self%atom)) then
         do a = 1,n_atoms
            if (NOT is_fragment_atom(a)) cycle
            call add_ED_grid_(self%atom(a)%slaterbasis,rho_fragment,pt,self%atom(a)%position)
         end do
         density_grid = rho_fragment
         do a = 1,n_atoms
            if (is_fragment_atom(a)) cycle
            call add_ED_grid_(self%atom(a)%slaterbasis,density_grid,pt,self%atom(a)%position)
         end do
      else
         do a = 1,n_atoms
            if (NOT is_fragment_atom(a)) cycle
            call add_ED_grid_(self%atom(a)%slaterbasis,rho_fragment,pt,self%atom(a)%position,self%atom(a)%site_occupancy)
         end do
         density_grid = rho_fragment
         do a = 1,n_atoms
            if (is_fragment_atom(a)) cycle
            call add_ED_grid_(self%atom(a)%slaterbasis,density_grid,pt,self%atom(a)%position,self%atom(a)%site_occupancy)
         end do
      end if
      where (density_grid>ZERO)
         density_grid = rho_fragment/density_grid
      elsewhere
         density_grid = ZERO
      end where
      call destroy_(rho_fragment)
   end subroutine


!  ===========
!  Stockholder
!  ===========

   PURE subroutine make_stockholder_density_grid(self,density_grid,pt)
   ! Make the Hirschfeld stockholder "density_grid" for the supplied
   ! points "pt". The promolecule electron density used to define the
   ! Hirshfeld surface is calculated as follows: if a slater basis set
   ! is available, then that is used to calculate the promolecule
   ! electron density; if a coppens basis set is available, then that
   ! is used to get the promolecule density; otherwise, if restricted
   ! real natural orbitals are available for all atoms (in terms of
   ! gaussian basis functions), then those are used to calculate the
   ! promolecule densisties. In principle, we can caculate Hirshfeld
   ! surfaces for *molecular* fragment densities, rather than
   ! promolecule densities as done here. That would require a
   ! different keyword and different code.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: rho_fragment
      VEC(BIN,:), allocatable :: is_fragment_atom
      INT :: n_atoms,n_pt,a
   ENSURE(allocated(self%saved),"MOLECULE.GRID:make_stockholder_density_grid ... no saved molecule")
   ENSURE(allocated(self%saved%cluster),"MOLECULE.GRID:make_stockholder_density_grid ... no saved cluster data")
   ENSURE(self%saved%cluster%info_made,"MOLECULE.GRID:make_stockholder_density_grid ... no saved cluster info")
   ENSURE(self%saved%cluster%radius>ZERO,"MOLECULE.GRID:make_stockholder_density_grid ... saved cluster radius is zero")
   ENSURE(self%saved%cluster%n_atoms==size(self%atom),"MOLECULE.GRID:make_stockholder_density_grid ... saved cluster/=atom list")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_stockholder_density_grid ... no atoms")
   ENSURE(has_all_density_data_(self%atom),"MOLECULE.GRID:make_stockholder_density_grid ... no atomic orbitals")
      n_atoms = size(self%atom)
      n_pt = size(density_grid)

      ! Locate fragment atoms in cluster
      is_fragment_atom = self%saved%cluster%is_fragment_atom

      ! Fragment density grid
      call create_(rho_fragment,n_pt)
      rho_fragment = ZERO

      ! Make fragment density
      do a = 1,n_atoms
         if (NOT is_fragment_atom(a)) cycle
         call add_ED_grid_(self%atom(a),rho_fragment,pt)
      end do
      density_grid = rho_fragment
      do a = 1,n_atoms
         if (is_fragment_atom(a)) cycle
         call add_ED_grid_(self%atom(a),density_grid,pt)
      end do

      ! density_grid = rho_fragment/density_grid
      do a = 1,size(density_grid)
         if (density_grid(a)>epsilon(ONE)) then
            density_grid(a) = rho_fragment(a)/density_grid(a)
         else
            density_grid(a) = ZERO
         end if
      end do

      ! Clean up
      call destroy_(rho_fragment)
   end subroutine


!   make_stockholder_weight_d_grid(w0,w1,w2,pt)
!   ! Make the Hirschfeld stockholder derivative density grid "d1_grid"
!   ! for the supplied points "pt". The promolecule electron density
!   ! used to define the Hirshfeld surface is calculated as follows: if
!   ! a slater basis set is available, then that is used to calculate
!   ! the promolecule electron density; if a coppens basis set is
!   ! available, then that is used to get the promolecule density;
!   ! otherwise, if restricted real natural orbitals are available for
!   ! all atoms (in terms of gaussian basis functions), then those are
!   ! used to calculate the promolecule densisties. In principle, we
!   ! can caculate Hirshfeld surfaces for *molecular* fragment
!   ! densities, rather than promolecule densities as done here. That
!   ! would require a different keyword and different code.
!     self :: IN
!     w0 :: VEC{REAL}, OUT
!     w1 :: MAT{REAL}, OUT
!     w2 :: MAT3{REAL}, OUT
!     pt :: MAT{REAL}, IN
!   ENSURE(w1.dim1==3, "wrong size w1")
!   ENSURE(w2.dim1==3, "wrong size w2")
!   ENSURE(w2.dim2==3, "wrong size w2")
!   ENSURE(w0.dim1==w1.dim2, "incompatible w0 and w1")
!   ENSURE(w0.dim1==w2.dim3, "incompatible w0 and w2")
!   ENSURE(.cluster.allocated, "no cluster data")
!   ENSURE(.cluster.info_made, "no cluster info")
!   ENSURE(.cluster.radius>ZERO, "cluster radius is zero")
!   ENSURE(.atom.allocated,"no atoms")
!   ENSURE(.atom.has_all_density_data,"no atomic orbitals")
!     d0,rho0f,rho0a :: VEC{REAL}@
!     d1,rho1f,rho1a :: MAT{REAL}@
!     d2,rho2f,rho2a :: MAT3{REAL}@
!     f1,a1 :: VEC{REAL}(3)
!     f0,bot,bot2,bot3,fac :: REAL
!     n_pt,a,b,i :: INT
!   ! w  = (sum_IN rho_IN) /(sum_ALL rho_ALL)
!   ! w' = (sum_IN rho'_IN)/(sum_ALL rho_ALL)
!   !    - (sum_IN rho_IN)(sum_ALL rho'_ALL)/(sum_ALL rho_ALL)^2
!   ! w^ab = (sum_IN rho^ab_IN)/(sum_ALL rho_ALL)
!   !      - (sum_IN rho^a_IN)(sum_ALL rho^b_ALL)/(sum_ALL rho_ALL)^2
!   !      - (sum_IN rho^b_IN)(sum_ALL rho^a_ALL)/(sum_ALL rho_ALL)^2
!   !      - (sum_IN rho_IN)(sum_ALL rho^ab_ALL)/(sum_ALL rho_ALL)^2
!   !    + 2 (sum_IN rho_IN)(sum_ALL rho^a_ALL)(sum_ALL rho^b_ALL)/(sum_ALL rho_ALL)^3
!     n_pt = w0.dim1
!     d0.create(n_pt);     rho0f.create(n_pt);     rho0a.create(n_pt)
!     d1.create(3,n_pt);   rho1f.create(3,n_pt);   rho1a.create(3,n_pt)
!     d2.create(3,3,n_pt); rho2f.create(3,3,n_pt); rho2a.create(3,3,n_pt)
!     rho0f = ZERO; rho0a = ZERO
!     rho1f = ZERO; rho1a = ZERO
!     rho2f = ZERO; rho2a = ZERO
!     do a = 1,.cluster.n_atoms
!        if (NOT .cluster.is_fragment_atom(a)) cycle
!        .atom(a).make_d_density_grid(d0,d1,d2,pt) ! this incorporates a skip step
!        rho0f = rho0f + d0
!        rho1f = rho1f + d1
!        rho2f = rho2f + d2
!     end
!     rho0a = rho0f
!     rho1a = rho1f
!     rho2a = rho2f
!     do a = 1,.cluster.n_atoms
!        if (.cluster.is_fragment_atom(a)) cycle
!        .atom(a).make_d_density_grid(d0,d1,d2,pt) ! this incorporates a skip step
!        rho0a = rho0a + d0
!        rho1a = rho1a + d1
!        rho2a = rho2a + d2
!     end
!   ! w  = (sum_IN rho_IN) /(sum_ALL rho_ALL)
!   ! w' = (sum_IN rho'_IN)/(sum_ALL rho_ALL)
!   !    - (sum_IN rho_IN)(sum_ALL rho'_ALL)/(sum_ALL rho_ALL)^2
!   ! w^ab = (sum_IN rho^ab_IN)/(sum_ALL rho_ALL)
!   !      - (sum_IN rho^a_IN)(sum_ALL rho^b_ALL)/(sum_ALL rho_ALL)^2
!   !      - (sum_IN rho^b_IN)(sum_ALL rho^a_ALL)/(sum_ALL rho_ALL)^2
!   !      - (sum_IN rho_IN)(sum_ALL rho^ab_ALL)/(sum_ALL rho_ALL)^2
!   !    + 2 (sum_IN rho_IN)(sum_ALL rho^a_ALL)(sum_ALL rho^b_ALL)/(sum_ALL rho_ALL)^3
!     do i = 1,rho0a.dim
!       if (rho0a(a)>epsilon(ONE)) then
!         bot = ONE/rho0a(i)
!         bot2 = bot*bot
!         bot3 = TWO*bot2*bot
!         f0   = rho0f(i)
!         w0(i) = f0*bot
!         f1 = rho1f(:,i)
!         a1 = rho1a(:,i)
!         fac = f0*bot2
!         w1(:,i) = f1*bot - a1*fac
!         do a = 1,3
!         do b = 1,a

!                      + f0*a1(a)*a1(b)*bot3
!            if (a==b) cycle
!            w2(b,a,i) = w2(a,b,i)
!         end
!         end
!       else
!         w0(i) = ZERO
!         w1(:,i) = ZERO
!         w2(:,:,i) = ZERO
!       end
!     end
!     rho2a.destroy; rho2f.destroy; d2.destroy
!     rho1a.destroy; rho1f.destroy; d1.destroy
!     rho0a.destroy; rho0f.destroy; d0.destroy
!   end

!  ===================
!  Hirshfeld atom grid
!  ===================

   subroutine make_Hirshfeld_atom_grid_0(self)
   ! Work out the Hirshfeld atom density density on ".plot_grid" 
   ! for ".plot_grid.center_atom"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), allocatable :: grid
      MAT(REAL,:,:), allocatable :: pt
      MAT(REAL,:,:), allocatable :: D
      STR(len=STR_SIZE) :: label
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_Hirshfeld_atom_grid_0 ... no grid")
   ENSURE(self%plot_grid%center_atom>0,"MOLECULE.GRID:make_Hirshfeld_atom_grid_0 ... no center_atom to plot")
   ENSURE(is_allocated_with_genre_(self%density_mx,"r "),"MOLECULE.GRID:make_Hirshfeld_atom_grid_0 ... no restricted density matrix")

      ! Make space for grid values and grid points
      call create_(grid,self%plot_grid%n_pt)
      call create_(pt,self%plot_grid%n_pt,3)

      ! Make the grid points
      call make_points_(self%plot_grid,pt)

      ! Make the hirshfeld atom density
      D = self%density_mx%r
      call make_Hirshfeld_atom_grid_(self,grid,pt,self%plot_grid%center_atom,D)

      ! Make the plot label
      label = "hirshfeld_atom_"
      label = trim(label)//trim(to_str_(self%plot_grid%center_atom))
      label = trim(label)//"_density_grid"

      ! Print out
      call dump_plot_grid_(self,grid,trim(label))

      ! Clean up
      call destroy_(pt)
      call destroy_(grid)
   end subroutine

   PURE subroutine make_Hirshfeld_atom_grid_1(self,N0,pt,c,D)
   ! Make the hirshfeld atom density grid "N0" for the grid "pt" for
   ! atom "c" given AO density matrix "D". Density "N0" weighted by
   ! Hirshfeld partition weight for atom "c".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: N0
      MAT(REAL,:,:), IN :: pt
      INT, IN :: c
      MAT(REAL,:,:), IN :: D
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_Hirshfeld_atom_grid_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_Hirshfeld_atom_grid_1 ... no atom list")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.GRID:make_Hirshfeld_atom_grid_1 ... no overlapping_atoms")

      ! Overlapping atom electron density
      call make_overlapping_atom_density_(self,N0,pt,c,D)

      ! Apply stockholder weight function for atom "c" to "N0".
      ! If using interpolatators, then overlapping_atoms
      ! are not needed ... otherwise nothing is skipped
      call apply_stockholder_atom_weight_(self,N0,c,pt)
   end subroutine

   PURE subroutine make_overlapping_atom_density(self,N0,pt,c,D)
   ! Make the overlapping atom density grid "N0" for the grid "pt" for
   ! atom "c" given AO density matrix "D". 
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: N0
      MAT(REAL,:,:), IN :: pt
      INT, IN :: c
      MAT(REAL,:,:), IN :: D
      VEC(REAL,:), allocatable :: xa,ya,za,a2
      VEC(REAL,:), allocatable :: ga0,gb0
      VEC(REAL,3) :: ra
      VEC(INT,:), allocatable :: pi,pj,pn
      VEC(BIN,:), allocatable :: skipa0,skipab,skipa,skipb
      MAT(REAL,:,:), allocatable :: DD
      type(SHELL1_TYPE) :: sha
      VEC(type(EVEC_BIN_TYPE),:), allocatable :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: bf_grid0
      REAL :: cutoff,Dab,fac,val,ga,gb
      INT :: n_pt,n_keep,ca,cb,oa,fsa,lsa,ob,fsb,lsb
      INT :: sa,fa,la,na,ba,sb,fb,lb,nb,a,b,i,j,p,n
      type(BECKE_GRID_TYPE) :: becke_grid
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_overlapping_atom_density ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_overlapping_atom_density ... no atom list")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.GRID:make_overlapping_atom_density ... no overlapping_atoms")

      ! Grid size
      n_pt = size(pt,1)

      ! Initialise N0
      N0 = ZERO

      ! Basis function cutoff
      if (allocated(self%becke_grid)) then; cutoff = self%becke_grid%basis_fn_cutoff
      else; cutoff = becke_grid%basis_fn_cutoff
      end if

      ! Atom "a" displaced points (from "c") and skip switches
      call create_(xa,n_pt)
      call create_(ya,n_pt)
      call create_(za,n_pt)
      call create_(a2,n_pt)
      call create_(skipa0,n_pt)
      call create_(skipab,n_pt)

      ! Basis function grid and skip list
      call create_(bf_skip,self%n_shell)
      call create_(bf_grid0,self%n_shell)

      ! Loop c-overlapping atoms "oa"
      do ca = 1,size(self%overlapping_atoms_for_atom(c)%element)
         oa = self%overlapping_atoms_for_atom(c)%element(ca)
         fsa = self%first_shell_for_atom(oa)
         lsa = self%last_shell_for_atom(oa)
         ra = self%atom(oa)%position

         ! Make the displaced points for atom "oa"
         call make_displaced_skip_pts_(self%atom(oa),xa,ya,za,a2,skipa0,pt,n_pt,cutoff)

         ! Loop shells on atom a
         do sa = fsa,lsa
            fa = self%first_basis_fn_for_shell(sa)
            la = self%last_basis_fn_for_shell(sa)
            na = la-fa+1

            ! Create shell "sa"
            ba = self%basis_shell_for_shell(sa)
            sha = self%basis_shell1pair(ba,1)%a
            sha%position = ra

            ! Make shell "sa" skipa list
            call create_(skipa,n_pt)
            call make_skip_list_(sha,skipa,n_keep,a2,skipa0,n_pt,cutoff)

            ! Next shell "sa" if no sig. points
            if (n_keep==0) then
               call destroy_(skipa)
               cycle
            end if

            ! Assign the "sa" skipa list
            bf_skip(sa)%element = skipa

            ! Make/copy the "sa" basis function grid
            ! This must be copied since skipa could be TRUE
            call create_(bf_grid0(sa)%element,n_keep,na)
            call make_skip_grid_(sha,bf_grid0(sa)%element,n_keep,xa,ya,za,a2,skipa,n_pt)

            ! Loop c-overlapping atoms b
            do cb = 1,ca
               ob = self%overlapping_atoms_for_atom(c)%element(cb)
               fsb = self%first_shell_for_atom(ob)
               lsb = self%last_shell_for_atom(ob)

               ! Loop shells on atom b
               do sb = fsb,min(lsb,sa)
                  if (NOT allocated(bf_skip(sb)%element)) cycle
                  fb = self%first_basis_fn_for_shell(sb)
                  lb = self%last_basis_fn_for_shell(sb)
                  nb = lb-fb+1

                  ! Set the "sb" basis function grid
                  skipb = bf_skip(sb)%element

                  ! How many points in common?
                  skipab(1:n_pt) = skipa OR skipb
                  n_keep = count(NOT skipab(1:n_pt))
                  if (n_keep==0) cycle

                  ! Factor
                  fac = TWO
                  if (sa==sb) fac = ONE

                  ! Density block
                  DD = D(fa:la,fb:lb)
                  if (na*nb==1) then
                     ga0 = bf_grid0(sa)%element(:,1)
                     gb0 = bf_grid0(sb)%element(:,1)
                     Dab = fac*DD(1,1)
                     i = 0
                     j = 0
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        ga = ga0(i)
                        gb = gb0(j)
                        val = ga*gb*Dab
                        N0(n) = N0(n)+val
                     end do
                  else

                     ! Create space for gathered index info
                     call create_(pi,n_keep)
                     call create_(pj,n_keep)
                     call create_(pn,n_keep)

                     ! Make mapper arrays
                     p = 0
                     i = 0
                     j = 0
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        p = p+1
                        pi(p) = i
                        pj(p) = j
                        pn(p) = n
                     end do

                     ! Add density contribution to rho
                     do a = 1,na
                        ga0 = bf_grid0(sa)%element(:,a)
                        do b = 1,nb
                           gb0 = bf_grid0(sb)%element(:,b)
                           Dab = fac*DD(a,b)
                           do p = 1,n_keep
                              i = pi(p)
                              j = pj(p)
                              n = pn(p)
                              ga = ga0(i)
                              gb = gb0(j)
                              val = ga*gb*Dab
                              N0(n) = N0(n)+val
                           end do
                        end do
                     end do

                     ! Clean up
                     call destroy_(pn)
                     call destroy_(pj)
                     call destroy_(pi)
                  end if
               end do  ! -- loops sa,sb
            end do
         end do  ! -- lops ca,cb
      end do

      ! Clean up
      call destroy_(bf_grid0)
      call destroy_(bf_skip)
      call destroy_(skipab)
      call destroy_(skipa0)
      call destroy_(a2)
      call destroy_(za)
      call destroy_(ya)
      call destroy_(xa)
   end subroutine


!  ====================
!  Jayatilaka atom grid
!  ====================

   subroutine make_Jayatilaka_atom_grid_0(self)
   ! Work out the Jayatilaka atom density density on ".plot_grid" 
   ! for ".plot_grid.center_atom"
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: grid
      MAT(REAL,:,:), allocatable :: pt
      STR(len=STR_SIZE) :: label
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_Jayatilaka_atom_grid_0 ... no grid")
   ENSURE(self%plot_grid%center_atom>0,"MOLECULE.GRID:make_Jayatilaka_atom_grid_0 ... no center_atom to plot")
   ENSURE(is_allocated_with_genre_(self%density_mx,"r "),"MOLECULE.GRID:make_Jayatilaka_atom_grid_0 ... no restricted density matrix")

      ! Make space for grid values and grid points
      call create_(grid,self%plot_grid%n_pt)
      call create_(pt,self%plot_grid%n_pt,3)

      ! Make the grid points
      call make_points_(self%plot_grid,pt)

      ! Make the hirshfeld atom density
      call make_Jayatilaka_atom_grid_(self,grid,pt,self%plot_grid%center_atom,self%density_mx%r)

      ! Make the plot label
      label = "jayatilaka_atom_"
      label = trim(label)//trim(to_str_(self%plot_grid%center_atom))
      label = trim(label)//"_density_grid"

      ! Print out
      call dump_plot_grid_(self,grid,trim(label))

      ! Clean up
      call destroy_(pt)
      call destroy_(grid)
   end subroutine

   PURE subroutine make_Jayatilaka_atom_grid_1(self,N0,pt,c,D)
   ! Make the Jayatilaka atom density grid "N0" for the grid "pt" for
   ! atom "c" given AO density matrix "D". Density "N0" weighted by
   ! Jayatilaka partition weight for atom "c" which is
   !    w(r)   = y_c(r)/sum_a y_a
   !    y_c(r) = [rho_c(r) - rho_c(0)]^{-1}
   !  rho_c(r) = rho(0) exp(- 2 Z_c r)
   ! where rho(0) is the molecular density at the position of atom c.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: N0
      MAT(REAL,:,:), IN :: pt
      INT, IN :: c
      MAT(REAL,:,:), IN :: D
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_Jayatilaka_atom_grid_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_Jayatilaka_atom_grid_1 ... no atom list")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.GRID:make_Jayatilaka_atom_grid_1 ... no overlapping_atoms")

      ! Overlapping atom electron density
      call make_overlapping_atom_density_(self,N0,pt,c,D)

      ! Apply weight
      call apply_Jayatilaka_atom_weight_(self,N0,c,pt)
   end subroutine

   PURE subroutine apply_Jayatilaka_atom_weight(self,grid,a,pt)
   ! Apply i.e. multiply the Jayatilaka weight to "grid" for the atom
   ! with index "a" using supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      INT, IN :: a
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: Wa
      INT :: n_pt
   ENSURE(allocated(self%atom),"MOLECULE.GRID:apply_Jayatilaka_atom_weight ... no atoms")
   ENSURE(has_interpolators_any_kind_(self%atom),"MOLECULE.GRID:apply_Jayatilaka_atom_weight ... missing atomic interpolators")
   ENSURE(size(grid)==size(pt,1),"MOLECULE.GRID:apply_Jayatilaka_atom_weight ... wrong size, grid")

      ! Only needed for more than one atom
      if (self%n_atom>1) then

         ! Get weights
         n_pt = size(pt,1)
         call create_(Wa,n_pt)
         call make_Jayatilaka_atom_weight_(self,Wa,a,pt)

         ! Apply them
         grid = grid*Wa

         ! Clean
         call destroy_(Wa)
      end if
   end subroutine

   PURE subroutine make_Jayatilaka_atom_weight(self,wt,a,pt)
   ! Make the Jayatilaka grid "g" for the atom with index "a" using
   ! supplied points "pt".
   !    w(r)   = y_c(r)/sum_a y_a
   !    y_c(r) = [rho_c(r) - rho_c(0)]^{-1}
   !  rho_c(r) = rho(0) exp(- 2 Z_c r)
   ! where rho(0) is the molecular density at the position of atom c.
   ! NOTE: This DOES NOT USE an interpolated density for each atom
   ! UNLESS use_interpolators is explicitly set. In this case you
   ! should probably use the next routine which uses the
   ! overlapping_atoms.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: wt
      INT, IN :: a
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: pos
      VEC(REAL,:), allocatable :: rho_0,rho_a
      INT :: n_pt,i
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_Jayatilaka_atom_weight ... no atoms")
   ENSURE(has_interpolators_any_kind_(self%atom),"MOLECULE.GRID:make_Jayatilaka_atom_weight ... missing atomic interpolators")
   ENSURE(size(wt)==size(pt,1),"MOLECULE.GRID:make_Jayatilaka_atom_weight ... wrong size, wt")

      ! No. of points
      n_pt = size(wt)

      ! Get electron densities at each atom nucleus
      call create_(rho_0,n_pt)
      call create_(pos,size(self%atom),3)
      call put_coordinates_to_(self%atom,pos,positions_as_rows=TRUE)
      call make_ED_grid_(self,rho_0,pos)
      call destroy_(pos)

      ! Make density "rho_a" for atom "a"
      call create_(rho_a,n_pt)
      call make_exponential_densities_(self%atom(a),rho_a,pt,rho_0(a))

      ! Sum remaining into "wt"
      wt = rho_a
      do i = 1,self%n_atom
         if (i==a) cycle
         call add_exponential_densities_(self%atom(i),wt,pt,rho_0(i))
      end do

      ! Make the stockholder weight
      do i = 1,n_pt
         if (wt(i)>epsilon(ONE)) then; wt(i) = rho_a(i)/wt(i)
         else; wt(i) = ZERO
         end if
      end do
      call destroy_(rho_a)
      call destroy_(rho_0)
   end subroutine


!  ================
!  Promolecule grid
!  ================

   NON_MPI_PURE subroutine make_promolecule_density_grid(self,density_grid,pt)
   ! Return promolecule "density_grid" from supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_promolecule_density_grid ... no atoms")

      ! Create space for an atom's grid values

      ! The following code is inherited from MOLECULE.GRID
      call create_(grid,size(density_grid))

      ! Sum up the atom grid values
      density_grid = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_promolecule_density_grid")
         call make_ED_grid_(self%atom(a),grid,pt)
         density_grid = density_grid+grid
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_promolecule_density_grid")
      PARALLEL_SUM(density_grid)

      ! Clean up
      call destroy_(grid)
   end subroutine

   PURE subroutine make_spherical_atom_grid(self,density_grid,pt)
   ! Return the promolecule "density_grid" from the supplied points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%atom),"MOLECULE.GRID:make_spherical_atom_grid ... no atoms")
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_spherical_atom_grid ... no plot_grid")
   ENSURE(self%plot_grid%center_atom>0,"MOLECULE.GRID:make_spherical_atom_grid ... no plot_grid center atom")
      call make_ED_grid_(self%atom(self%plot_grid%center_atom),density_grid,pt)
   end subroutine


!  =======================
!  Grad rho on rho density
!  =======================

   PURE subroutine make_grad_rho_on_rho_grid(self,grid,pt)
   ! Work out abs(grad(rho))/rho on a set of grid points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: rho
      MAT(REAL,:,:), allocatable :: grad_rho
      INT :: n_pt,i
   ENSURE(size(pt,1)==size(grid),"MOLECULE.GRID:make_grad_rho_on_rho_grid ... array dimensions do not match")
      n_pt = size(pt,1)
      call create_(rho,n_pt)
      call create_(grad_rho,n_pt,3)
      rho = ZERO
      grid = ZERO
      call make_ED_grid_(self,rho,pt)
      call make_nabla_ED_grid_(self,grad_rho,pt)
      do i = 1,n_pt
         if (rho(i)>ZERO) then
            grid(i) = sqrt(dot_product(grad_rho(i,:),grad_rho(i,:)))/rho(i)
         else
            grid(i) = ZERO
         end if
      end do
      call destroy_(grad_rho)
      call destroy_(rho)
   end subroutine


!  =============
!  ELI-D density
!  =============

   NON_MPI_PURE subroutine make_ELI_D_grid(self,ELI_grid,pt)
   ! Work out the Electron Localisation Indicator (D) (ELI) density
   ! "ELI_grid" on a set of grid points "pt", using ".natural
   ! orbitals" and the ".occupation" number vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: ELI_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_ELI_D_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_ELI_D_grid ... no natural orbitals")
      if (trim(number_kind_(self%NOs))=="real") then
         call make_ELI_D_grid_r_(self,ELI_grid,pt)
   ! else
   !    DIE("no complex implementation")
      end if
   end subroutine

   NON_MPI_PURE subroutine make_ELI_D_grid_r(self,ELI_grid,pt)
   ! Work out the Electron Localisation Indicator (D) grid (ELI_D)
   ! density on a set of grid points "pt", using ".natural orbitals"
   ! and the ".NO_occ_nos" vector.  Grid, basis set, and
   ! natural orbitals are required to exist.  Reference: Wagner et al
   ! (2007), Chem Eur. J. 13, p. 5724 (Look at appendix for definition
   ! of g!)
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: ELI_grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: DD
      VEC(REAL,:), allocatable :: gx,gy,gz
      VEC(REAL,:), allocatable :: g
      VEC(REAL,:), allocatable :: rho_s,tau,NO
      VEC(REAL,:), allocatable :: rhox,rhoy,rhoz
      INT :: i,n_pt
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_ELI_D_grid_r ... no basis info")
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_ELI_D_grid_r ... no natural orbitals")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.GRID:make_ELI_D_grid_r ... natural orbitals must be restricted")

      ! No of grid points
      n_pt = size(pt,1)
      call create_(rho_s,n_pt)
      rho_s = ZERO

      ! Make the density of alpha electrons
      call make_ED_grid_(self,rho_s,pt)
      rho_s = HALF*rho_s

      ! Temporaries
      call create_(g,n_pt); g = ZERO
      call create_(tau,n_pt); tau = ZERO
      call create_(rhox,n_pt); rhox = ZERO
      call create_(rhoy,n_pt); rhoy = ZERO
      call create_(rhoz,n_pt); rhoz = ZERO
      call create_(NO,n_pt)
      call create_(DD,n_pt,3)

      ! Sum the orbital gradient and density gradient bit
      do i = PARALLEL_DO_START(1,1),no_of_occupied_NOs_(self),PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_ELI_D_grid_r")
         call make_nabla_orbital_grid_r_(self,DD,NO,self%NOs%r(:,i),pt)

         ! Orbital gradients
         gx = DD(:,1)
         gy = DD(:,2)
         gz = DD(:,3)

         ! Fermi hole curvature
         g = g+rho_s*(gx*gx+gy*gy+gz*gz)

         ! This is only the orbital gradient part
         rhox = rhox+gx*NO
         rhoy = rhoy+gy*NO
         rhoz = rhoz+gz*NO
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_ELI_D_grid_r")
      call destroy_(DD)
      call destroy_(NO)
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(g)
         PARALLEL_SUM(tau)
         PARALLEL_SUM(rhox)
         PARALLEL_SUM(rhoy)
         PARALLEL_SUM(rhoz)
      end if
      g = g-rhox*rhox-rhoy*rhoy-rhoz*rhoz
      ELI_grid = rho_s*(12.0d0/g)**(THREE/EIGHT)

      ! Clean up
      call destroy_(rhoz)
      call destroy_(rhoy)
      call destroy_(rhox)
      call destroy_(tau)
      call destroy_(g)
      call destroy_(rho_s)
   end subroutine


!  ===========
!  ELF density
!  ===========

   PURE subroutine make_ELF_grid(self,ELF_grid,pt)
   ! Work out the Electron Localisation Function (ELF) density "g" on
   ! a set of grid points "pt", using ".natural orbitals" and the
   ! ".occupation" number vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: ELF_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_ELF_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_ELF_grid ... no natural orbitals")
      if (trim(number_kind_(self%NOs))=="real") then
         call make_ELF_grid_r_(self,ELF_grid,pt)
      end if
   end subroutine

   PURE subroutine make_ELF_grid_r(self,ELF_grid,pt)
   ! Work out the Electron Localisation Function (ELF) density on a
   ! set of grid points "pt", using ".natural orbitals" and the
   ! ".NO_occ_nos" vector.  Grid, basis set, and natural
   ! orbitals are required to exist.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: ELF_grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: DD
      VEC(REAL,:), allocatable :: NO
      VEC(REAL,:), allocatable :: gx,gy,gz,mo_val
      VEC(REAL,:), allocatable :: rho_sx,rho_sy,rho_sz
      VEC(REAL,:), allocatable :: rho_s,tau,d_s,d0_s
      REAL :: occu,factor,mo_val_n
      REAL :: gx_n,gy_n,gz_n,occ_mo
      INT :: i,n_occ,n,n_pt
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_ELF_grid_r ... no basis info")
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_ELF_grid_r ... no natural orbitals")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.GRID:make_ELF_grid_r ... natural orbitals must be restricted")
      ELF_grid = ZERO
      n_pt = size(pt,1)
      call create_(rho_s,n_pt); rho_s = ZERO
      call create_(tau,n_pt); tau = ZERO
      call create_(rho_sx,n_pt); rho_sx = ZERO
      call create_(rho_sy,n_pt); rho_sy = ZERO
      call create_(rho_sz,n_pt); rho_sz = ZERO
      call create_(NO,n_pt)
      call create_(DD,n_pt,3)
      n_occ = no_of_occupied_NOs_(self)
      do i = 1,n_occ
         call make_nabla_orbital_grid_r_(self,DD,NO,self%NOs%r(:,i),pt)
         mo_val = NO(:)
         gx = DD(:,1)
         gy = DD(:,2)
         gz = DD(:,3)
         occu = HALF*self%NO_occ_nos%r(i)
         do n = 1,n_pt
            mo_val_n = mo_val(n)
            gx_n = gx(n)
            gy_n = gy(n)
            gz_n = gz(n)
            occ_mo = occu*mo_val_n
            rho_s(n) = rho_s(n)+occ_mo*mo_val_n
            tau(n) = tau(n)+occu*(gx_n*gx_n+gy_n*gy_n+gz_n*gz_n)
            rho_sx(n) = rho_sx(n)+occ_mo*gx_n  ! this is half grad_x rho_s
            rho_sy(n) = rho_sy(n)+occ_mo*gy_n
            rho_sz(n) = rho_sz(n)+occ_mo*gz_n
         end do
      end do
      call destroy_(DD)
      call destroy_(NO)
      call create_(d_s,n_pt)
      d_s = tau-(rho_sx*rho_sx+rho_sy*rho_sy+rho_sz*rho_sz)/rho_s
      call destroy_(rho_sz)
      call destroy_(rho_sy)
      call destroy_(rho_sx)
      call destroy_(tau)
      call create_(d0_s,n_pt)
      factor = 0.6d0*(SIX*PI*PI)**(TWO/THREE)
      d0_s = factor*rho_s**(FIVE/THREE)
      call destroy_(rho_s)
      ELF_grid = ONE/(ONE+(d_s*d_s)/(d0_s*d0_s))

      ! this could be more efficient if the d0_s*d0_s is written
      ! factor2*rho_s**(TWO+FIVE/THREE)
      call destroy_(d0_s); call destroy_(d_s)
   end subroutine


!  =====================
!  Tsirelson ELF density
!  =====================

   PURE subroutine make_Tsirelson_ELF_grid(self,ELF_grid,pt)
   ! Work out the Tsirelson-Stash Electron Localisation Function (ELF)
   ! density on a series of grid points "pt" ".natural orbitals" and
   ! the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: ELF_grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_Tsirelson_ELF_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_Tsirelson_ELF_grid ... no natural orbitals")
      if (trim(number_kind_(self%NOs))=="real") then
         call make_Tsirelson_ELF_grid_r_(self,ELF_grid,pt)
      end if
   end subroutine

   PURE subroutine make_Tsirelson_ELF_grid_r(self,ELF_grid,pt)
   ! Work out the Tsirelson-Stash Electron Localisation Function (ELF)
   ! density on a series of grid points "pt" ".natural orbitals" and
   ! the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: ELF_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: NO
      MAT(REAL,:,:), allocatable :: DD,LL
      VEC(REAL,:), allocatable :: mo_val,gx,gy,gz,lx,ly,lz
      VEC(REAL,:), allocatable :: rho,rho_x,rho_y,rho_z
      VEC(REAL,:), allocatable :: lap,d_p,d_0
      REAL :: occu,factor,mo_val_n,occ_mo
      REAL :: gx_n,gy_n,gz_n
      REAL :: lx_n,ly_n,lz_n
      INT :: i,n_occ,n,n_pt
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_Tsirelson_ELF_grid_r ... no basis info")
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_Tsirelson_ELF_grid_r ... no natural orbitals")
      ELF_grid = ZERO
      n_pt = size(pt,1)
      call create_(rho,n_pt); rho = ZERO
      call create_(rho_x,n_pt); rho_x = ZERO
      call create_(rho_y,n_pt); rho_y = ZERO
      call create_(rho_z,n_pt); rho_z = ZERO
      call create_(lap,n_pt); lap = ZERO
      call create_(NO,n_pt)
      call create_(DD,n_pt,3)
      call create_(LL,n_pt,3)
      n_occ = no_of_occupied_NOs_(self)
      do i = 1,n_occ
         call make_laplacian_orbital_grid_r_(self,LL,DD,NO,self%NOs%r(:,i),pt)
         mo_val = NO(:)
         gx = DD(:,1); gy = DD(:,2); gz = DD(:,3)
         lx = LL(:,1); ly = LL(:,2); lz = LL(:,3)
         occu = self%NO_occ_nos%r(i)
         do n = 1,n_pt
            mo_val_n = mo_val(n)
            gx_n = gx(n); gy_n = gy(n); gz_n = gz(n)
            lx_n = lx(n); ly_n = ly(n); lz_n = lz(n)
            occ_mo = occu*mo_val_n  ! this is n_i phi_i
            rho(n) = rho(n)+occ_mo*mo_val_n  ! this is rho
            rho_x(n) = rho_x(n)+occ_mo*gx_n  ! this is half grad_x rho
            rho_y(n) = rho_y(n)+occ_mo*gy_n
            rho_z(n) = rho_z(n)+occ_mo*gz_n
            lap(n) = lap(n)+occ_mo*(lx_n+ly_n+lz_n)+occu*(gx_n*gx_n+gy_n*gy_n+gz_n*gz_n)
         end do
      end do
      call destroy_(LL)
      call destroy_(DD)
      call destroy_(NO)
      call create_(d_0,n_pt)
      factor = 0.3d0*(THREE*PI*PI)**(TWO/THREE)
      d_0 = factor*rho**(FIVE/THREE)
      call create_(d_p,n_pt)
      factor = FOUR/NINE
      d_p = d_0-factor*(rho_x*rho_x+rho_y*rho_y+rho_z*rho_z)/rho+lap/THREE
      call destroy_(lap)
      call destroy_(rho_z)
      call destroy_(rho_y)
      call destroy_(rho_x)
      call destroy_(rho)
      ELF_grid = ONE/(ONE+(d_p*d_p)/(d_0*d_0))
      call destroy_(d_p); call destroy_(d_0)
   end subroutine


!  ======================
!  Fermi mobility density
!  ======================

   PURE subroutine make_fermi_mobility_grid(self,f,pt)
   ! Evaluate the Luken's Fermi hole mobility grid "f" on a set of grid points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: f
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_fermi_mobility_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_fermi_mobility_grid ... no natural orbitals")
      if (trim(number_kind_(self%NOs))=="real") then
         call make_fermi_mobility_grid_r_(self,f,pt)
   ! else
   !    DIE("code not written yet")
      end if
   end subroutine

   PURE subroutine make_fermi_mobility_grid_r(self,f,pt)
   ! Evaluate the Luken's Fermi hole mobility grid "f" on a set of grid points "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: f
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: del_gi
      VEC(REAL,:), allocatable :: dg,gi,rho
      VEC(REAL,:), allocatable :: dgx,dgy,dgz
      VEC(REAL,:), allocatable :: orb
      INT :: n,n_pt,i,n_occ
      REAL :: delx,dely,delz,fac
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_fermi_mobility_grid_r ... no basis info")
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_fermi_mobility_grid_r ... no natural orbitals")
      n_pt = size(pt,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(dg,n_pt); dg = ZERO
      call create_(dgx,n_pt); dgx = ZERO
      call create_(dgy,n_pt); dgy = ZERO
      call create_(dgz,n_pt); dgz = ZERO
      call create_(gi,n_pt)
      call create_(del_gi,n_pt,3)
      do i = 1,n_occ
         orb = self%NOs%r(:,i)
         call make_nabla_orbital_grid_r_(self,del_gi,gi,orb,pt)
         do n = 1,n_pt
            delx = del_gi(n,1)
            dely = del_gi(n,2)
            delz = del_gi(n,3)
            dg(n) = dg(n)+delx*delx+dely*dely+delz*delz
            dgx(n) = dgx(n)+delx*gi(n)
            dgy(n) = dgy(n)+dely*gi(n)
            dgz(n) = dgz(n)+delz*gi(n)
         end do
      end do
      call destroy_(del_gi)
      call destroy_(gi)
      call create_(rho,n_pt)
      call make_ED_grid_(self,rho,pt)
      fac = (THREE*PI/FOUR)*HALF**TWOTHIRD
      f = ONE/rho*(dg-TWO*(dgx*dgx+dgy*dgy+dgz*dgz)/rho)-fac*rho**TWOTHIRD
      call destroy_(rho)
      call destroy_(dgz)
      call destroy_(dgy)
      call destroy_(dgx)
      call destroy_(dg)
   end subroutine


!  ===========================
!  True Fermi mobility density
!  ===========================

   PURE subroutine make_true_fermi_mobility_grid(self,f,pt)
   ! Work out the the true fermi mobility density "f" on a series of
   ! grid points "pt" using ".natural orbitals" and the
   ! ".NO_occ_nos" vector. The true function differs from that
   ! above in that the kinetic energy density is evaluated exactly.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: f
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_true_fermi_mobility_grid ... no natural orbitals")
   ENSURE(has_any_genre_(self%NOs),"MOLECULE.GRID:make_true_fermi_mobility_grid ... no natural orbitals")
      if (trim(number_kind_(self%NOs))=="real") then
         call make_true_fermi_mobil_grid_r_(self,f,pt)
   ! else
   !    DIE("code not written yet")
      end if
   end subroutine

   PURE subroutine make_true_fermi_mobil_grid_r(self,f,pt)
   ! Evaluate the true Fermi hole mobility grid "f" on a set of grid points
   ! "pt".  The true function differs from that above in that the kinetic energy
   ! density is evaluated exactly.
   ! NOTE: this routine is only valid for single determinant wavefunctions.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: f
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: dg,rho,K
      VEC(REAL,:), allocatable :: dgx,dgy,dgz
      INT :: n,n_pt,i,j,n_occ
      REAL :: delx,dely,delz
      MAT(REAL,:,:), allocatable :: del_gi,g,T
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_true_fermi_mobil_grid_r ... no basis info")
   ENSURE(allocated(self%NOs),"MOLECULE.GRID:make_true_fermi_mobil_grid_r ... no natural orbitals")
   ENSURE(allocated(self%kinetic_energy_mx),"MOLECULE.GRID:make_true_fermi_mobil_grid_r ... no kinetic energy matrix")
      n_pt = size(pt,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(dg,n_pt); dg = ZERO
      call create_(dgx,n_pt); dgx = ZERO
      call create_(dgy,n_pt); dgy = ZERO
      call create_(dgz,n_pt); dgz = ZERO
      call create_(g,n_pt,n_occ)
      call create_(del_gi,n_pt,3)

      ! Assemble intermediate terms
      do i = 1,n_occ
         call make_nabla_orbital_grid_r_(self,del_gi,g(:,i),self%NOs%r(:,i),pt)
         do n = 1,n_pt
            delx = del_gi(n,1)
            dely = del_gi(n,2)
            delz = del_gi(n,3)
            dg(n) = dg(n)+delx*delx+dely*dely+delz*delz
            dgx(n) = dgx(n)+delx*g(n,i)
            dgy(n) = dgy(n)+dely*g(n,i)
            dgz(n) = dgz(n)+delz*g(n,i)
         end do
      end do
      call destroy_(del_gi)

      ! Make kinetic energy matrix part
      call create_(K,n_pt)
      K = ZERO
      call create_(T,self%n_bf,self%n_bf)
      T = self%kinetic_energy_mx%r
      call change_basis_using_(T,self%NOs%r)
      do j = 1,n_occ
         do i = 1,n_occ
            K = K+g(:,i)*g(:,j)*T(i,j)
         end do
      end do
      call destroy_(T)

      ! Now do the final assembly
      call create_(rho,n_pt)
      call make_ED_grid_(self,rho,pt)
      f = ONE/rho*(dg-TWO*(dgx*dgx+dgy*dgy+dgz*dgz)/rho-TWO*K)

      ! Clean
      call destroy_(rho)
      call destroy_(K)
      call destroy_(g)
      call destroy_(dgz)
      call destroy_(dgy)
      call destroy_(dgx)
      call destroy_(dg)
   end subroutine


!  ==============
!  Electric Field
!  ==============

   subroutine make_E_field_grid_0(self,grid,pt)
   ! Work out the field density on ".plot_grid" and dump output
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: field
      STR(len=STR_SIZE) :: name
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_E_field_grid_0 ... no grid")

      ! Make space for the field and grid points

      ! The following code is inherited from MOLECULE.GRID
      call create_(field,self%plot_grid%n_pt,3)

      ! Make the grid of field values
      call make_E_field_grid_(self,field,pt)

      ! Print out components as well
      name = plot_file_name_(self%plot_grid,self%n_a)
      call dump_plot_grid_(self,field(:,1),trim(name)//".x")  ! NOT PURE
      call dump_plot_grid_(self,field(:,2),trim(name)//".y")
      call dump_plot_grid_(self,field(:,3),trim(name)//".z")

      ! Get the magnitude
      grid = sqrt(field(:,1)*field(:,1)+field(:,2)*field(:,2)+field(:,3)*field(:,3))

      ! Clean up
      call destroy_(field)
   end subroutine

   NON_MPI_PURE subroutine make_E_field_grid_1(self,E,pt)
   ! Make the electric field "E" at a series of points "pt", nuclei
   ! included, from the restricted density matrix
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: E
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: Dab,Ex,Ey,Ez
      INT :: q,fa,fb,la,lb,na,nb,p,n_pt
      REAL :: fac
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_E_field_grid_1 ... no basis set")
   ENSURE(is_allocated_with_genre_(self%density_mx,"r "),"MOLECULE.GRID:make_E_field_grid_1 ... no restricted density matrix")
   ENSURE(size(E,1)==size(pt,1),"MOLECULE.GRID:make_E_field_grid_1 ... wrong dim1, E & pt")
   ENSURE(size(E,2)==3,"MOLECULE.GRID:make_E_field_grid_1 ... wrong dim2, E")
      n_pt = size(pt,1)
      E = ZERO
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_E_field_grid_1")
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)

         ! Off-diagonal blocks stand for their transpose too
         Dab = self%density_mx%r(fa:la,fb:lb)
         fac = TWO
         if (fa==fb) fac = ONE
         call create_(Ex,na,nb)
         call create_(Ey,na,nb)
         call create_(Ez,na,nb)

         ! Minus sign for electron charge
         do p = 1,n_pt
            call make_E_field_ints_(sh,Ex,Ey,Ez,pt(p,:))
            E(p,1) = E(p,1)-fac*sum(Ex*Dab)
            E(p,2) = E(p,2)-fac*sum(Ey*Dab)
            E(p,3) = E(p,3)-fac*sum(Ez*Dab)
         end do
         call destroy_(Ez)
         call destroy_(Ey)
         call destroy_(Ex)
         call destroy_(Dab)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_E_field_grid_1")
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(E)
      end if
      call add_nuclear_E_field_at_points_to_(self%atom,E,pt)
   end subroutine

   NON_MPI_PURE subroutine make_E_field_magnitude(self,grid,pt)
   ! Evaluate the E field magnitude on a set of grid points "pt". 
   ! Includes the atom nuclear charge contribution.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: field
      VEC(REAL,:), allocatable :: fx,fy,fz

      ! Make the grid of field values
      call create_(field,self%plot_grid%n_pt,3)
      call make_E_field_grid_(self,field,pt)

      ! Assign field components
      fx = field(:,1)
      fy = field(:,2)
      fz = field(:,3)

      ! Get the magnitude
      grid = sqrt(fx*fx+fy*fy+fz*fz)

      ! Clean up
      call destroy_(field)
   end subroutine


!  ==============
!  Magnetic Field
!  ==============

   subroutine make_B_field_grid(self,grid,pt)
   ! Work out the field density magnitude on ".plot_grid" and dump output
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: B,B_L,B_S
      VEC(REAL,:), allocatable :: mag
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_B_field_grid ... no grid")
      if (self%plot_grid%points_filename/="no_made-up_points") then
         call text_(stdout,"=====================================")
         call text_(stdout,"use of read-in grid to be implemented")
         call text_(stdout,"TONTO will stop here ...")
         call text_(stdout,"=====================================")
         DIE("MOLECULE.GRID:make_B_field_grid ... die")
      else

         ! Make space for the field and grid points
         call create_(B,self%plot_grid%n_pt,3)
         call create_(B_L,self%plot_grid%n_pt,3)
         call create_(B_S,self%plot_grid%n_pt,3)
         call create_(mag,self%plot_grid%n_pt)

         ! Make the grid of field values
         call make_B_L_field_grid_(self,B_L,pt)
         call make_B_S_field_grid_(self,B_S,pt)

         ! Print B_L field components
         call dump_plot_grid_(self,B_L(:,1),"B_L_field.x")
         call dump_plot_grid_(self,B_L(:,2),"B_L_field.y")
         call dump_plot_grid_(self,B_L(:,3),"B_L_field.z")
         mag = sqrt(B_L(:,1)*B_L(:,1)+B_L(:,2)*B_L(:,2)+B_L(:,3)*B_L(:,3))
         call dump_plot_grid_(self,mag,"B_L_field.magnitude")

         ! Print B_S field components
         call dump_plot_grid_(self,B_S(:,1),"B_S_field.x")
         call dump_plot_grid_(self,B_S(:,2),"B_S_field.y")
         call dump_plot_grid_(self,B_S(:,3),"B_S_field.z")
         mag = sqrt(B_S(:,1)*B_S(:,1)+B_S(:,2)*B_S(:,2)+B_S(:,3)*B_S(:,3))
         call dump_plot_grid_(self,mag,"B_S_field.magnitude")

         ! Print B field components
         B = B_L+B_S
         call dump_plot_grid_(self,B(:,1),"B_field.x")
         call dump_plot_grid_(self,B(:,2),"B_field.y")
         call dump_plot_grid_(self,B(:,3),"B_field.z")

         ! Magnitude (returned)
         grid = sqrt(B(:,1)*B(:,1)+B(:,2)*B(:,2)+B(:,3)*B(:,3))
         call dump_plot_grid_(self,grid,"B_field.magnitude")
      end if

      ! Clean up
      call destroy_(mag)
      call destroy_(B_S)
      call destroy_(B_L)
      call destroy_(B)
   end subroutine

   subroutine make_B_L_field_grid_0(self,grid,pt)
   ! Work out the field density on ".plot_grid" and dump output
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: field
      STR(len=STR_SIZE) :: name
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_B_L_field_grid_0 ... no grid")

      ! Make space for the field and grid points

      ! The following code is inherited from MOLECULE.GRID
      call create_(field,self%plot_grid%n_pt,3)

      ! Make the grid of field values
      call make_B_L_field_grid_(self,field,pt)

      ! Print out components as well
      name = plot_file_name_(self%plot_grid,self%n_a)
      call dump_plot_grid_(self,field(:,1),trim(name)//".x")  ! NOT PURE
      call dump_plot_grid_(self,field(:,2),trim(name)//".y")
      call dump_plot_grid_(self,field(:,3),trim(name)//".z")

      ! Get the magnitude
      grid = sqrt(field(:,1)*field(:,1)+field(:,2)*field(:,2)+field(:,3)*field(:,3))

      ! Clean up
      call destroy_(field)
   end subroutine

   NON_MPI_PURE subroutine make_B_L_field_grid_1(self,B,pt)
   ! Make the magnetic field "B" field due to the linear current on a
   ! series of points "pt"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: B
      MAT(REAL,:,:), IN :: pt
      MAT4(REAL,:,:,:,:), allocatable, target :: Mleft,Mright
      MAT(REAL,:,:), PTR :: J1,J2,J3
      MAT(REAL,:,:), PTR :: J4,J5,J6
      MAT(REAL,:,:), allocatable :: D,Dba,Dab
      INT :: q,fa,fb,la,lb,na,nb,p,n_pt
      REAL :: fac
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_B_L_field_grid_1 ... no basis set")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.GRID:make_B_L_field_grid_1 ... no density matrix")

      ! No of grid points
      n_pt = size(pt,1)

      ! The imaginary part of AO density matrix
      call create_(D,self%n_bf,self%n_bf)
      call make_ao_IM_density_mx_(self,D)

      ! Initialize
      B = ZERO

      ! Loop over integral blocks
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_B_L_field_grid_1")

         ! Get shell indices (leaky)
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)

         ! Get the density matrix
         call create_(Dab,na,nb)
         Dab = D(fa:la,fb:lb)
         call create_(Dba,nb,na)
         Dba = D(fb:lb,fa:la)

         ! Space for integrals
         call create_(Mleft,nb,na,3,3)
         call create_(Mright,na,nb,3,3)
         J1 => Mright(:,:,1,1)
         J2 => Mright(:,:,2,2)
         J3 => Mright(:,:,3,3)
         J4 => Mleft(:,:,1,1)
         J5 => Mleft(:,:,2,2)
         J6 => Mleft(:,:,3,3)

         ! Loop over points
         do p = 1,n_pt
            if (fa==fb) then

               ! Make the B-field integrals for point k
               call make_nabla_field_ints_(sh,Mright,pt(p,:))

               ! Cross product part
               J1 = Mright(:,:,2,3)-Mright(:,:,3,2)
               J2 = Mright(:,:,3,1)-Mright(:,:,1,3)
               J3 = Mright(:,:,1,2)-Mright(:,:,2,1)

               ! Get magnetic field contribution
               B(p,1) = B(p,1)+trace_product_with_(J1,Dba)
               B(p,2) = B(p,2)+trace_product_with_(J2,Dba)
               B(p,3) = B(p,3)+trace_product_with_(J3,Dba)
            else

               ! Make the B-field integrals for point k
               call make_nabla_field_ints_(sh,Mright,Mleft,pt(p,:))

               ! Cross product part
               J1 = Mright(:,:,2,3)-Mright(:,:,3,2)
               J2 = Mright(:,:,3,1)-Mright(:,:,1,3)
               J3 = Mright(:,:,1,2)-Mright(:,:,2,1)
               J4 = Mleft(:,:,2,3)-Mleft(:,:,3,2)
               J5 = Mleft(:,:,3,1)-Mleft(:,:,1,3)
               J6 = Mleft(:,:,1,2)-Mleft(:,:,2,1)

               ! Get magnetic field contribution
               B(p,1) = B(p,1)+trace_product_with_(J1,Dba)
               B(p,2) = B(p,2)+trace_product_with_(J2,Dba)
               B(p,3) = B(p,3)+trace_product_with_(J3,Dba)
               B(p,1) = B(p,1)+trace_product_with_(J4,Dab)
               B(p,2) = B(p,2)+trace_product_with_(J5,Dab)
               B(p,3) = B(p,3)+trace_product_with_(J6,Dab)
            end if
         end do

         ! Clean
         call destroy_(Mleft)
         call destroy_(Mright)
         call destroy_(Dba)
         call destroy_(Dab)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_B_L_field_grid_1")

      ! Clean
      call destroy_(D)
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(B)
      end if

      ! Minus sign because the nabla field ints in the code are defined as minus
      ! the I-integrals of the paper at PCCP
      fac = -ONE/(SPEED_OF_LIGHT_AU*SPEED_OF_LIGHT_AU)
      B = fac*B
   end subroutine


!   make_B_L_field_grid_checked(B,pt)
!   ! Make the magnetic field "B" field due to the linear current on a
!   ! series of points "pt"
!      B :: MAT{REAL}, OUT
!      pt :: MAT{REAL}, IN
!
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.density_mx.has_any_genre,"no density matrix")
!
!      D :: MAT{REAL}*
!      Dba :: MAT{REAL}*
!      Jx,Jy,Jz :: MAT{REAL}*
!      q,fa,fb,la,lb,na,nb, k,n_pt :: INT
!      fac :: REAL
!      sh :: SHELL2
!
!      ! No of grid points
!      n_pt = pt.dim1
!
!      ! Make the AO density matrix
!      D.create(.n_bf,.n_bf)
!      .BASEmake_ao_IM_density_mx(D)
!
!      ! Initialize
!      B = ZERO
!
!      ! Loop over integral blocks
!      parallel do q = 1,.n_shell_pairs
!
!        ! Get shell indices (leaky)
!        .set_shell2_from(q,sh,fa,la,na,fb,lb,nb)
!        fac = TWO
!        if (fa==fb) fac = ONE
!
!        ! Get the density matrix
!        Dba.create(nb,na)
!        Dba = D(fb:lb,fa:la)
!
!        ! Space for integrals
!        Jx.create(na,nb)
!        Jy.create(na,nb)
!        Jz.create(na,nb)
!
!        ! Loop over points
!        do k = 1,n_pt
!
!           ! Make the integrals for point k
!           sh.make_magnetic_jp_ints(Jx,Jy,Jz,pt(k,:))
!
!           ! Get magnetic field contribution
!           B(k,1) = B(k,1) + fac*Jx.trace_product_with(Dba)
!           B(k,2) = B(k,2) + fac*Jy.trace_product_with(Dba)
!           B(k,3) = B(k,3) + fac*Jz.trace_product_with(Dba)
!
!        end
!
!        ! Clean
!        Jz.destroy; Jy.destroy; Jx.destroy
!        Dba.destroy
!
!      end
!
!      ! Clean
!      D.destroy
!
!      if (WORK_IS_SHARED) then
!         PARALLEL_SUM(B)
!      end
!
!      ! Minus sign not necessary? CHECK
!      fac = -HALF/(SPEED_OF_LIGHT_AU*SPEED_OF_LIGHT_AU)
!      B   = fac*B
!
!   end

   subroutine make_B_S_field_grid_0(self,grid,pt)
   ! Work out the field density on ".plot_grid" and dump output
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: field
      STR(len=STR_SIZE) :: name
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_B_S_field_grid_0 ... no grid")

      ! Make space for the field and grid points

      ! The following code is inherited from MOLECULE.GRID
      call create_(field,self%plot_grid%n_pt,3)

      ! Make the grid of field values
      call make_B_S_field_grid_(self,field,pt)

      ! Print out components as well
      name = plot_file_name_(self%plot_grid,self%n_a)
      call dump_plot_grid_(self,field(:,1),trim(name)//".x")  ! NOT PURE
      call dump_plot_grid_(self,field(:,2),trim(name)//".y")
      call dump_plot_grid_(self,field(:,3),trim(name)//".z")

      ! Get the magnitude
      grid = sqrt(field(:,1)*field(:,1)+field(:,2)*field(:,2)+field(:,3)*field(:,3))

      ! Clean up
      call destroy_(field)
   end subroutine

   NON_MPI_PURE subroutine make_B_S_field_grid_1(self,B,pt)
   ! Make the magnetic field "B" field due to the linear current on a
   ! series of points "pt"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: B
      MAT(REAL,:,:), IN :: pt
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_B_S_field_grid_1 ... no basis set")
   ENSURE(has_any_genre_(self%density_mx),"MOLECULE.GRID:make_B_S_field_grid_1 ... no density matrix")

      ! Not the most efficient, but should work ...
      if (allocated(self%density_mx%a)) then; call make_B_S_field_grid_u_(self,B,pt)
      else; call make_B_S_field_grid_gc_(self,B,pt)
      end if
   end subroutine

   NON_MPI_PURE subroutine make_B_S_field_grid_u(self,B,pt)
   ! Make the magnetic field "B" field due to the spin current on a
   ! series of points "pt"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: B
      MAT(REAL,:,:), IN :: pt
      MAT4(REAL,:,:,:,:), allocatable :: Mright,Mleft
      MAT(REAL,:,:), allocatable :: Sz,S3,S6
      INT :: q,fa,fb,la,lb,na,nb,p,n_pt
      REAL :: fac
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_B_S_field_grid_u ... no basis set")
   ENSURE(allocated(self%density_mx%a),"MOLECULE.GRID:make_B_S_field_grid_u ... no alpha density matrix")
   ENSURE(allocated(self%density_mx%b),"MOLECULE.GRID:make_B_S_field_grid_u ... no beta density matrix")

      ! No of grid points
      n_pt = size(pt,1)

      ! Make the AO Sz density matrix
      call create_(Sz,self%n_bf,self%n_bf)
      call make_r_Sz_density_mx_(self,Sz)

      ! Initialize
      B = ZERO

      ! Loop over integral blocks
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_B_S_field_grid_u")

         ! Get shell indices (leaky)
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)

         ! Get the z spin density matrix block
         call create_(S3,nb,na); S3 = Sz(fb:lb,fa:la)
         call create_(S6,na,nb); S6 = Sz(fa:la,fb:lb)

         ! Space for integrals
         call create_(Mright,na,nb,3,3)
         call create_(Mleft,nb,na,3,3)

         ! Loop over points
         do p = 1,n_pt
            if (fa==fb) then

               ! Make the B-field integrals for point k
               call make_nabla_field_ints_(sh,Mright,pt(p,:))

               ! Sum over the spin density matrices
               B(p,1) = B(p,1)+trace_product_with_(Mright(:,:,1,3),S3)
               B(p,2) = B(p,2)+trace_product_with_(Mright(:,:,2,3),S3)
               B(p,3) = B(p,3)-trace_product_with_(Mright(:,:,1,1),S3)
               B(p,3) = B(p,3)-trace_product_with_(Mright(:,:,2,2),S3)
            else

               ! Make the B-field integrals for point k
               call make_nabla_field_ints_(sh,Mright,Mleft,pt(p,:))
               B(p,1) = B(p,1)+trace_product_with_(Mright(:,:,1,3),S3)
               B(p,2) = B(p,2)+trace_product_with_(Mright(:,:,2,3),S3)
               B(p,3) = B(p,3)-trace_product_with_(Mright(:,:,1,1),S3)
               B(p,3) = B(p,3)-trace_product_with_(Mright(:,:,2,2),S3)
               B(p,1) = B(p,1)+trace_product_with_(Mleft(:,:,1,3),S6)
               B(p,2) = B(p,2)+trace_product_with_(Mleft(:,:,2,3),S6)
               B(p,3) = B(p,3)-trace_product_with_(Mleft(:,:,1,1),S6)
               B(p,3) = B(p,3)-trace_product_with_(Mleft(:,:,2,2),S6)

            ! To add to make it work even if S is not Hermitian
            ! (useful to test a single matrix element)
            ! but then a factor HALF should be inserted

!             B(p,1) = B(p,1) + Mright(:,:,1,3).trace_product_with(S6,TRUE)
!             B(p,2) = B(p,2) + Mright(:,:,2,3).trace_product_with(S6,TRUE)
!             B(p,3) = B(p,3) - Mright(:,:,1,1).trace_product_with(S6,TRUE)
!             B(p,3) = B(p,3) - Mright(:,:,2,2).trace_product_with(S6,TRUE)
!         
!             B(p,1) = B(p,1) + Mleft(:,:,1,3).trace_product_with(S3,TRUE)
!             B(p,2) = B(p,2) + Mleft(:,:,2,3).trace_product_with(S3,TRUE)
!             B(p,3) = B(p,3) - Mleft(:,:,1,1).trace_product_with(S3,TRUE)
!             B(p,3) = B(p,3) - Mleft(:,:,2,2).trace_product_with(S3,TRUE)
            end if
         end do

         ! Clean
         call destroy_(Mright)
         call destroy_(Mleft)
         call destroy_(S3)
         call destroy_(S6)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_B_S_field_grid_u")

      ! Clean
      call destroy_(Sz)
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(B)
      end if

      ! Spin Factor
      ! Plus sign because the nabla field ints in the code are defined as minus
      ! the I-integrals of the paper at PCCP
      fac = G_FACTOR/(SPEED_OF_LIGHT_AU*SPEED_OF_LIGHT_AU)
      B = fac*B
   end subroutine

   NON_MPI_PURE subroutine make_B_S_field_grid_gc(self,B,pt)
   ! Make the magnetic field "B" field due to the linear current on a
   ! series of points "pt"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: B
      MAT(REAL,:,:), IN :: pt
      MAT3(REAL,:,:,:), allocatable :: S
      MAT4(REAL,:,:,:,:), allocatable :: Mleft
      MAT4(REAL,:,:,:,:), allocatable :: Mright
      MAT(REAL,:,:), allocatable :: S1,S2,S3
      MAT(REAL,:,:), allocatable :: S4,S5,S6
      INT :: q,fa,fb,la,lb,na,nb,p,n_pt
      REAL :: fac
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_B_S_field_grid_gc ... no basis set")

      ! No of grid points
      n_pt = size(pt,1)

      ! Make the AO density matrix
      call create_(S,self%n_bf,self%n_bf,3)
      call make_RE_c_S_densities_(self,S)

      ! Initialize
      B = ZERO

      ! Loop over integral blocks
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_B_S_field_grid_gc")

         ! Get shell indices (leaky)
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)

         ! Get the spin density matrix
         call create_(S1,nb,na); S1 = S(fb:lb,fa:la,1)
         call create_(S2,nb,na); S2 = S(fb:lb,fa:la,2)
         call create_(S3,nb,na); S3 = S(fb:lb,fa:la,3)
         call create_(S4,na,nb); S4 = S(fa:la,fb:lb,1)
         call create_(S5,na,nb); S5 = S(fa:la,fb:lb,2)
         call create_(S6,na,nb); S6 = S(fa:la,fb:lb,3)

         ! Space for integrals
         call create_(Mright,na,nb,3,3)
         call create_(Mleft,nb,na,3,3)

         ! Loop over points
         do p = 1,n_pt
            if (fa==fb) then

               ! Make the B-field integrals for point k
               call make_nabla_field_ints_(sh,Mright,pt(p,:))

               ! Sum over the spin density matrices
               B(p,2) = B(p,2)+trace_product_with_(Mright(:,:,2,1),S1)
               B(p,3) = B(p,3)+trace_product_with_(Mright(:,:,3,1),S1)
               B(p,1) = B(p,1)-trace_product_with_(Mright(:,:,2,2),S1)
               B(p,1) = B(p,1)-trace_product_with_(Mright(:,:,3,3),S1)
               B(p,1) = B(p,1)+trace_product_with_(Mright(:,:,1,2),S2)
               B(p,3) = B(p,3)+trace_product_with_(Mright(:,:,3,2),S2)
               B(p,2) = B(p,2)-trace_product_with_(Mright(:,:,1,1),S2)
               B(p,2) = B(p,2)-trace_product_with_(Mright(:,:,3,3),S2)
               B(p,1) = B(p,1)+trace_product_with_(Mright(:,:,1,3),S3)
               B(p,2) = B(p,2)+trace_product_with_(Mright(:,:,2,3),S3)
               B(p,3) = B(p,3)-trace_product_with_(Mright(:,:,1,1),S3)
               B(p,3) = B(p,3)-trace_product_with_(Mright(:,:,2,2),S3)
            else

               ! Make the B-field integrals for point k
               call make_nabla_field_ints_(sh,Mright,Mleft,pt(p,:))
               B(p,2) = B(p,2)+trace_product_with_(Mright(:,:,2,1),S1)
               B(p,3) = B(p,3)+trace_product_with_(Mright(:,:,3,1),S1)
               B(p,1) = B(p,1)-trace_product_with_(Mright(:,:,2,2),S1)
               B(p,1) = B(p,1)-trace_product_with_(Mright(:,:,3,3),S1)
               B(p,1) = B(p,1)+trace_product_with_(Mright(:,:,1,2),S2)
               B(p,3) = B(p,3)+trace_product_with_(Mright(:,:,3,2),S2)
               B(p,2) = B(p,2)-trace_product_with_(Mright(:,:,1,1),S2)
               B(p,2) = B(p,2)-trace_product_with_(Mright(:,:,3,3),S2)
               B(p,1) = B(p,1)+trace_product_with_(Mright(:,:,1,3),S3)
               B(p,2) = B(p,2)+trace_product_with_(Mright(:,:,2,3),S3)
               B(p,3) = B(p,3)-trace_product_with_(Mright(:,:,1,1),S3)
               B(p,3) = B(p,3)-trace_product_with_(Mright(:,:,2,2),S3)
               B(p,2) = B(p,2)+trace_product_with_(Mleft(:,:,2,1),S4)
               B(p,3) = B(p,3)+trace_product_with_(Mleft(:,:,3,1),S4)
               B(p,1) = B(p,1)-trace_product_with_(Mleft(:,:,2,2),S4)
               B(p,1) = B(p,1)-trace_product_with_(Mleft(:,:,3,3),S4)
               B(p,1) = B(p,1)+trace_product_with_(Mleft(:,:,1,2),S5)
               B(p,3) = B(p,3)+trace_product_with_(Mleft(:,:,3,2),S5)
               B(p,2) = B(p,2)-trace_product_with_(Mleft(:,:,1,1),S5)
               B(p,2) = B(p,2)-trace_product_with_(Mleft(:,:,3,3),S5)
               B(p,1) = B(p,1)+trace_product_with_(Mleft(:,:,1,3),S6)
               B(p,2) = B(p,2)+trace_product_with_(Mleft(:,:,2,3),S6)
               B(p,3) = B(p,3)-trace_product_with_(Mleft(:,:,1,1),S6)
               B(p,3) = B(p,3)-trace_product_with_(Mleft(:,:,2,2),S6)

            ! To add to make it work even if S is not Hermitian
            ! (useful to test a single matrix element)
            ! but then a factor HALF should be inserted

!             B(p,2) = B(p,2) + Mright(:,:,2,1).trace_product_with(S4,TRUE)
!             B(p,3) = B(p,3) + Mright(:,:,3,1).trace_product_with(S4,TRUE)
!             B(p,1) = B(p,1) - Mright(:,:,2,2).trace_product_with(S4,TRUE)
!             B(p,1) = B(p,1) - Mright(:,:,3,3).trace_product_with(S4,TRUE)
!        
!             B(p,1) = B(p,1) + Mright(:,:,1,2).trace_product_with(S5,TRUE)
!             B(p,3) = B(p,3) + Mright(:,:,3,2).trace_product_with(S5,TRUE)
!             B(p,2) = B(p,2) - Mright(:,:,1,1).trace_product_with(S5,TRUE)
!             B(p,2) = B(p,2) - Mright(:,:,3,3).trace_product_with(S5,TRUE)
!        
!             B(p,1) = B(p,1) + Mright(:,:,1,3).trace_product_with(S6,TRUE)
!             B(p,2) = B(p,2) + Mright(:,:,2,3).trace_product_with(S6,TRUE)
!             B(p,3) = B(p,3) - Mright(:,:,1,1).trace_product_with(S6,TRUE)
!             B(p,3) = B(p,3) - Mright(:,:,2,2).trace_product_with(S6,TRUE)
!        
!        
!             B(p,2) = B(p,2) + Mleft(:,:,2,1).trace_product_with(S1,TRUE)
!             B(p,3) = B(p,3) + Mleft(:,:,3,1).trace_product_with(S1,TRUE)
!             B(p,1) = B(p,1) - Mleft(:,:,2,2).trace_product_with(S1,TRUE)
!             B(p,1) = B(p,1) - Mleft(:,:,3,3).trace_product_with(S1,TRUE)
!        
!             B(p,1) = B(p,1) + Mleft(:,:,1,2).trace_product_with(S2,TRUE)
!             B(p,3) = B(p,3) + Mleft(:,:,3,2).trace_product_with(S2,TRUE)
!             B(p,2) = B(p,2) - Mleft(:,:,1,1).trace_product_with(S2,TRUE)
!             B(p,2) = B(p,2) - Mleft(:,:,3,3).trace_product_with(S2,TRUE)
!        
!             B(p,1) = B(p,1) + Mleft(:,:,1,3).trace_product_with(S3,TRUE)
!             B(p,2) = B(p,2) + Mleft(:,:,2,3).trace_product_with(S3,TRUE)
!             B(p,3) = B(p,3) - Mleft(:,:,1,1).trace_product_with(S3,TRUE)
!             B(p,3) = B(p,3) - Mleft(:,:,2,2).trace_product_with(S3,TRUE)
            end if
         end do

         ! Clean
         call destroy_(Mright)
         call destroy_(Mleft)
         call destroy_(S3)
         call destroy_(S2)
         call destroy_(S1)
         call destroy_(S6)
         call destroy_(S5)
         call destroy_(S4)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_B_S_field_grid_gc")

      ! Clean
      call destroy_(S)
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(B)
      end if

      ! Spin Factor.
      ! Plus sign because the nabla field ints in the code are 
      ! defined as minus the I-integrals of the paper at PCCP
      fac = G_FACTOR/(SPEED_OF_LIGHT_AU*SPEED_OF_LIGHT_AU)
      B = fac*B
   end subroutine

   NON_MPI_PURE subroutine make_B_L_matrices(self,Bx,By,Bz,c)
   ! Make the magnetic field "B" field due to the linear current on a
   ! series of points "pt"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Bx,By,Bz
      VEC(REAL,3), IN :: c
      INT :: q,fa,fb,la,lb,na,nb
      type(SHELL2_TYPE) :: sh

      ! Initialize
      Bx = ZERO
      By = ZERO
      Bz = ZERO

      ! Loop over integral blocks
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.GRID:make_B_L_matrices")

         ! shell indices (leaky)
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)

         ! Make the integrals, point c
         call make_magnetic_jp_ints_(sh,Bx(fa:la,fb:lb),By(fa:la,fb:lb),Bz(fa:la,fb:lb),c)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.GRID:make_B_L_matrices")

      ! Symmetrize
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(Bx)
         PARALLEL_SYMMETRIC_SUM(By)
         PARALLEL_SYMMETRIC_SUM(Bz)
      else
         call symmetric_reflect_(Bx)
         call symmetric_reflect_(By)
         call symmetric_reflect_(Bz)
      end if
   end subroutine


!  ======================
!  Polarizability density
!  ======================

   subroutine make_E_polarization_grid(self,grid,pt)
   ! Work out the electric polarization grid, either along a
   ! projection or all three directions and magnitude.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_E_polarization_grid ... no grid")
      if (is_zero_(self%plot_grid%projection_direction)) then
         call make_E_polarization_vgrid_(self,grid,pt)
      else
         call make_E_polarization_pgrid_(self,grid,pt)
      end if
   end subroutine

   subroutine make_E_polarization_vgrid_0(self,grid,pt)
   ! Work out the field density on ".plot_grid" and dump output
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: field
      STR(len=STR_SIZE) :: name
   ENSURE(allocated(self%plot_grid),"MOLECULE.GRID:make_E_polarization_vgrid_0 ... no grid")

      ! Make space for the field and grid points

      ! The following code is inherited from MOLECULE.GRID
      call create_(field,self%plot_grid%n_pt,3)

      ! Make the grid of field values
      call make_E_polarization_vgrid_(self,field,pt)

      ! Print out components as well
      name = plot_file_name_(self%plot_grid,self%n_a)
      call dump_plot_grid_(self,field(:,1),trim(name)//".x")  ! NOT PURE
      call dump_plot_grid_(self,field(:,2),trim(name)//".y")
      call dump_plot_grid_(self,field(:,3),trim(name)//".z")

      ! Get the magnitude
      grid = sqrt(field(:,1)*field(:,1)+field(:,2)*field(:,2)+field(:,3)*field(:,3))

      ! Clean up
      call destroy_(field)
   end subroutine

   PURE subroutine make_E_polarization_vgrid_1(self,E,pt)
   ! Make the (vector as opposed to projected) electric polarization
   ! density "E" on a series of grid points "pt"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: E
      MAT(REAL,:,:), IN :: pt
      MAT3(REAL,:,:,:), allocatable :: D
      MAT(REAL,:,:), allocatable :: MO
      INT :: n_occ,i
   ENSURE(self%basis_info_made,"MOLECULE.GRID:make_E_polarization_vgrid_1 ... no basis set")
   ENSURE(allocated(self%MOs),"MOLECULE.GRID:make_E_polarization_vgrid_1 ... no MO's")
   ENSURE(allocated(self%U_electric_dipole),"MOLECULE.GRID:make_E_polarization_vgrid_1 ... no U matrices?")
   ENSURE(size(E,1)==size(pt,1),"MOLECULE.GRID:make_E_polarization_vgrid_1 ... E and pt: inconsistent no. of points")
   ENSURE(size(E,2)==3,"MOLECULE.GRID:make_E_polarization_vgrid_1 ... E : wrong dim2")

      ! Dimensions
      n_occ = self%n_a

      ! Get derivative density matrices (restricted only)
      call create_(D,self%n_bf,self%n_bf,3)
      MO = self%MOs%r
      do i = 1,3  ! x,y,z electric fields
         call back_transform_to_(self%U_electric_dipole(:,:,1),D(:,:,1),MO(:,n_occ+1:),MO(:,1:n_occ))
         call symmetrize_(D(:,:,i))
      end do
      D = FOUR*D

      ! Make the derivative rho grid
      call make_derivative_rho_grid_(self,E,pt,D)

      ! Clean
      call destroy_(D)
   end subroutine

   PURE subroutine make_E_polarization_pgrid(self,grid,pt)
   ! Make the (vector as opposed to projected) electric polarization
   ! density "E" on a series of grid points "pt"
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: E
      VEC(REAL,3) :: n
      INT :: n_pt
   ENSURE(size(grid)==size(pt,1),"MOLECULE.GRID:make_E_polarization_pgrid ... grid and pt: inconsistent no. of points")

      ! Dimensions
      n_pt = size(pt,1)

      ! Space
      call create_(E,n_pt,3)

      ! Make electric polarization density
      call make_E_polarization_vgrid_(self,E,pt)

      ! Do the projection
      n = self%plot_grid%projection_direction
      grid = E(:,1)*n(1)+E(:,2)*n(2)+E(:,3)*n(3)

      ! Clean
      call destroy_(E)
   end subroutine

end module
