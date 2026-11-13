!=====================================================================
! BECKE_GRID
!
! This module provides a grid and weights for 3D numerical quadrature,
! where the 3D integrand is comprised of peaks which are centered at
! points in space which are known beforehand (they may be the
! positions of the atoms in a molecule, for example, and the integrand
! may be the density).
!
! These grids were first designed by Axel Becke. See the references:
!    A. D. Becke, J. Chem. Phys. 88 (1988) 2547.
!    O. Treutler and R. Ahlrichs, J. Chem. Phys. 102 (1995) 346.
!    M. Mura and P. Knowles, J. Chem. Phys. 104 (1996) p 9848.
!
! The 3-D grid is a direct-product combination of a radial grid and a
! spherical grid. The radial grids are standard one-dimensional types
! (Chebyshev, Euler-Maclaurin, etc) while the spherical grids are
! Lebedev-Laikov grids with octahedral symmetry designed to integrate
! exactly spherical harmonics up to angular momentum L=53 or higher. A
! minimum L=11 is recommended. The number of radial points for each
! atom is defined by reference to the number of points for Hydrogen,
! according to Becke's rule of thumb (5 points more for each atomic
! n-shell).
!
! The grid is essentially composed of a superposition of spherical
! grids, which have been "translated", "scaled", and "partitioned" to
! take into account that the grids are overlapping and
! interpenetrating.
!
! This version is essentially completely rewritten compared to Steve
! Wolff's original version. Some code remains from the original
! elliptic partitioning.
!
! Copyright (C) S. K. Wolff, 1999
! Copyright (C) D. Jayatilaka, 2005
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
module BECKE_GRID_MODULE

#  include "macros"

   include "becke_grid.use"

   implicit none

   include "becke_grid.int"


   ! These are the Bragg slater radii.
   ! WARNING: These are in Angstroms.
   VEC(REAL,89), private :: bragg_slater_radius
   data bragg_slater_radius/ &
      0.35d0,0.35d0,                                           & ! 1s
      1.45d0,1.05d0,0.85d0,0.70d0,0.65d0,0.60d0,0.50d0,0.45d0, & ! 2s+2p
      1.80d0,1.50d0,1.25d0,1.10d0,1.00d0,1.00d0,1.00d0,1.00d0, & ! 3s+3p
      2.20d0,1.80d0,                                           & ! 4s
      1.60d0,1.40d0,1.35d0,1.40d0,1.40d0,                      & ! 3d: 1st transition
      1.40d0,1.35d0,1.35d0,1.35d0,1.35d0,                      &
                    1.30d0,1.25d0,1.15d0,1.15d0,1.15d0,1.15d0, & ! 4p
      1.30d0,1.30d0,                                           & ! 5s
      1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,                      & ! 4d: 2nd transition
      1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,                      &
                    1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,1.30d0, & ! 5p
      1.30d0,1.30d0,                                           & ! 6s
      1.30d0,                                                  & ! Lanthanum
      1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,        & ! 4f: 1st Lanthanide
      1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,        &
             1.30d0,1.30d0,1.30d0,1.30d0,                      & ! 5d: 3rd transition
      1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,                      &
                    1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,1.30d0, & ! 6p
      1.30d0,1.30d0,                                           & ! 7s
      1.30d0                                                   /

   ! Treutler and Ahlrichs recommended zeta values
   ! (Table I, JCP 102 p346), in atomic units: the M4 mapping puts the
   ! centre of the x interval at r = zeta bohr.
   VEC(REAL,89), private :: TA_zeta
   data TA_zeta/ &
      0.80d0,0.90d0,                                           & ! 1s
      1.80d0,1.40d0,1.30d0,1.10d0,0.90d0,0.90d0,0.90d0,0.90d0, & ! 2s+2p
      1.40d0,1.30d0,1.30d0,1.20d0,1.10d0,1.00d0,1.00d0,1.00d0, & ! 3s+3p
      1.50d0,1.40d0,                                           & ! 4s
      1.30d0,1.20d0,1.20d0,1.20d0,1.20d0,                      & ! 3d: 1st transition
      1.20d0,1.20d0,1.10d0,1.10d0,1.10d0,                      &
                    1.10d0,1.00d0,0.90d0,0.90d0,0.90d0,0.90d0, & ! 4p: values after here are guessed
      1.50d0,1.40d0,                                           & ! 5s
      1.30d0,1.20d0,1.20d0,1.20d0,1.20d0,                      & ! 4d: 2nd transition
      1.20d0,1.20d0,1.10d0,1.10d0,1.10d0,                      &
                    1.10d0,1.00d0,0.90d0,0.90d0,0.90d0,0.90d0, & ! 5p
      1.50d0,1.40d0,                                           & ! 6s
      1.30d0,                                                  & ! Lanthanum
      1.00d0,1.00d0,1.00d0,1.00d0,1.00d0,1.00d0,1.00d0,        & ! 4f: 1st Lanthanide
      1.00d0,1.00d0,1.00d0,1.00d0,1.00d0,1.00d0,1.00d0,        &
             1.00d0,1.00d0,1.00d0,1.00d0,                      & ! 5d: 3rd transition
      1.00d0,1.00d0,1.00d0,1.00d0,1.00d0,                      &
                    1.00d0,1.00d0,1.00d0,1.00d0,1.00d0,1.00d0, & ! 6p
      1.50d0,1.40d0,                                           & ! 7s
      1.30d0                                                   /

   ! Mura and Knowles recommended alpha values
   ! These are in atomic units
   VEC(REAL,89), private :: MK_zeta
   data MK_zeta/ &
      5.00d0,5.00d0,                                           & ! 1s
      7.00d0,7.00d0,5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,5.00d0, & ! 2s+2p
      7.00d0,7.00d0,5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,5.00d0, & ! 3s+3p
      7.00d0,7.00d0,                                           & ! 4s
      5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,                      & ! 3d: 1st transition
      5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,                      &
                    5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,5.00d0, & ! 4p: values after here are guessed
      7.00d0,7.00d0,                                           & ! 5s
      5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,                      & ! 4d: 2nd transition
      5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,                      &
                    5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,5.00d0, & ! 5p
      7.00d0,7.00d0,                                           & ! 6s
      5.00d0,                                                  & ! Lanthanum
      5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,        & ! 4f: 1st Lanthanide
      5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,        &
             5.00d0,5.00d0,5.00d0,5.00d0,                      & ! 5d: 3rd transition
      5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,                      &
                    5.00d0,5.00d0,5.00d0,5.00d0,5.00d0,5.00d0, & ! 6p
      7.00d0,7.00d0,                                           & ! 7s
      5.00d0                                                   /

   ! The period of the atom. Needed because the number of radial points
   ! depends on the atom's period number.
   VEC(INT,89), private :: period
   data period/ &
           1,     1,                                           & ! 1s
            2,     2,     2,     2,     2,     2,     2,     2, & ! 2s+2p
            3,     3,     3,     3,     3,     3,     3,     3, & ! 3s+3p
            4,     4,                                           & ! 4s
            4,     4,     4,     4,     4,                      & ! 3d: 1st transition
            4,     4,     4,     4,     4,                      &
                         4,     4,     4,     4,     4,     4, & ! 4p
            5,     5,                                           & ! 5s
            5,     5,     5,     5,     5,                      & ! 4d: 2nd transition
            5,     5,     5,     5,     5,                      &
                         5,     5,     5,     5,     5,     5, & ! 5p
            6,     6,                                           & ! 6s
            6,                                                  & ! Lanthanum
            6,     6,     6,     6,     6,     6,     6,        & ! 4f: 6st Lanthanide
            6,     6,     6,     6,     6,     6,     6,        &
                  6,     6,     6,     6,                      & ! 5d: 3rd transition
            6,     6,     6,     6,     6,                      &
                         6,     6,     6,     6,     6,     6, & ! 6p
            7,     7,                                           & ! 7s
            7                                                   /

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object and set defaults
      type(BECKE_GRID_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"BECKE_GRID:create ... error in allocating memory")
#else
      allocate(self)
#endif
      call set_defaults_(self)
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(BECKE_GRID_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"BECKE_GRID:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
   !  .atom.destroy ! Never destroy this, just nullify
      type(BECKE_GRID_TYPE), INOUT :: self

      !!!!!!!!!!!!!
      call destroy_(self%atom)
      !!!!!!!!!!!!!
      call destroy_(self%atom_zeta)
      call destroy_(self%atom_min_distance)
      call destroy_(self%atom_n1_SS)
      call destroy_(self%unique_atom)
      call destroy_(self%unique_atom_for)
      call destroy_(self%n_pts_for_row)
      call destroy_(self%radial_grid)
      call destroy_(self%lebedev_grid)
      call destroy_(self%lebedev_H_grid)
      call destroy_(self%lebedev_L3_grid)
      call destroy_(self%lebedev_L5_grid)
      call destroy_(self%lebedev_L7_grid)
      call destroy_(self%lebedev_L9_grid)
      call destroy_(self%lebedev_L11_grid)
      call destroy_(self%unscaled_pts)
      call destroy_(self%unscaled_wts)
      call destroy_(self%atom_pts)
      call destroy_(self%atom_wts)
      call destroy_(self%atom_grid0)
      call destroy_(self%atom_grid1)
      call destroy_(self%wt_is_0)
   end subroutine

   PURE subroutine destroy_grid_part(self)
   ! Destroy all the quadrature grid pointer parts
      type(BECKE_GRID_TYPE), INOUT :: self
      call destroy_(self%n_pts_for_row)
      call destroy_(self%radial_grid)
      call destroy_(self%lebedev_grid)
      call destroy_(self%lebedev_H_grid)
      call destroy_(self%lebedev_L3_grid)
      call destroy_(self%lebedev_L5_grid)
      call destroy_(self%lebedev_L7_grid)
      call destroy_(self%lebedev_L9_grid)
      call destroy_(self%lebedev_L11_grid)
      call destroy_(self%unscaled_pts)
      call destroy_(self%unscaled_wts)
      call destroy_(self%atom_pts)
      call destroy_(self%atom_wts)
      call destroy_(self%atom_grid0)
      call destroy_(self%atom_grid1)
      call destroy_(self%wt_is_0)
   end subroutine

   PURE subroutine destroy_unscaled_grid(self)
   ! Destroy the quadrature grid pointer parts
      type(BECKE_GRID_TYPE), INOUT :: self
      call destroy_(self%unscaled_wts)
      call destroy_(self%unscaled_pts)
   end subroutine

   PURE subroutine destroy_atom_info(self)
   ! Destroy atoms and positions
      type(BECKE_GRID_TYPE), INOUT :: self

   ! .atom.destroy ! Never destroy this, just nullify
      call destroy_(self%atom_min_distance)
      call destroy_(self%atom_n1_SS)
      call destroy_(self%unique_atom)
      call destroy_(self%unique_atom_for)
      call destroy_(self%atom_pts)
      call destroy_(self%atom_wts)
      call destroy_(self%atom_grid0)
      call destroy_(self%atom_grid1)
      call destroy_(self%wt_is_0)
   end subroutine


!  ===
!  Set
!  ===

   PURE subroutine set_defaults(self)
   ! Set up a default dftgrid object
      type(BECKE_GRID_TYPE), INOUT :: self
      type(BECKE_GRID_TYPE) :: std

      ! Leaky, because of .set_zeta
      call set_kind_(self,std%kind)

      ! Set default no. of points, etc.
      call set_accuracy_(self,std%accuracy)

      ! Below are not really necessary?
      self%pruning_scheme = std%pruning_scheme
      self%basis_fn_cutoff = std%basis_fn_cutoff
      self%basis_fn_pair_cutoff = std%basis_fn_pair_cutoff
      self%partition_cutoff = std%partition_cutoff
      self%rho_cutoff = std%rho_cutoff
      self%prune_rho_cutoff = std%prune_rho_cutoff
      self%XC_batch_max_pts = std%XC_batch_max_pts
      self%XC_batch_box_edge = std%XC_batch_box_edge
      self%n_extra_pts_per_shell = std%n_extra_pts_per_shell
      self%reduce_H_angular_grid = std%reduce_H_angular_grid
      self%scale_atomic_grids = std%scale_atomic_grids
      self%partition_power = std%partition_power
      self%partition_scheme = std%partition_scheme
      self%partition_scaling_scheme = std%partition_scaling_scheme
      self%finalized = FALSE
      self%n_unscaled_pts = 0
      self%n_points = 0
   end subroutine

   PURE subroutine set_settings_from(self,grid)
   ! Copy every user-settable grid parameter from "grid", and nothing which
   ! depends on the atoms the grid is for. A fragment molecule needs this: it
   ! builds its own grid for its own atoms, but must integrate on the settings
   ! the user gave the parent. Keep the list in step with .set_defaults above,
   ! which sets the same parameters from the type defaults.
      type(BECKE_GRID_TYPE), INOUT :: self
      type(BECKE_GRID_TYPE), IN :: grid

      ! Leaky, because of .set_zeta
      call set_kind_(self,grid%kind)

      ! Sets .n_radial_pts and the three angular orders from the named level
      call set_accuracy_(self,grid%accuracy)

      ! ... any of which a keyword may have overridden, so take the four as
      ! they stand in "grid" rather than leaving .set_accuracy's values
      self%n_radial_pts = grid%n_radial_pts
      self%l_angular_grid = grid%l_angular_grid
      self%l_H_angular_grid = grid%l_H_angular_grid
      self%l_bonding_angular_grid = grid%l_bonding_angular_grid
      self%pruning_scheme = grid%pruning_scheme
      self%basis_fn_cutoff = grid%basis_fn_cutoff
      self%basis_fn_pair_cutoff = grid%basis_fn_pair_cutoff
      self%partition_cutoff = grid%partition_cutoff
      self%rho_cutoff = grid%rho_cutoff
      self%prune_rho_cutoff = grid%prune_rho_cutoff
      self%XC_batch_max_pts = grid%XC_batch_max_pts
      self%XC_batch_box_edge = grid%XC_batch_box_edge
      self%n_extra_pts_per_shell = grid%n_extra_pts_per_shell
      self%reduce_H_angular_grid = grid%reduce_H_angular_grid
      self%scale_atomic_grids = grid%scale_atomic_grids
      self%partition_power = grid%partition_power
      self%partition_scheme = grid%partition_scheme
      self%partition_scaling_scheme = grid%partition_scaling_scheme
      self%stratmann_scuseria_A = grid%stratmann_scuseria_A

      ! Whatever was built for the other molecule's atoms is not this
      ! molecule's grid: set_grid_data must run again
      self%finalized = FALSE
   end subroutine

   pure subroutine set_finalized(self,val)
   ! Generic set
      type(BECKE_GRID_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%finalized = val
   end subroutine

   PURE subroutine set_kind(self,kind)
   ! Set the kind of interpolation to use.
      type(BECKE_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: kind
   ENSURE(is_one_of_(kind,["becke            ","treutler_ahlrichs","mura_knowles     ","sg-0             ","sg-1             ","sg-2             ","sg-3             "]),"BECKE_GRID:set_kind ... unknown grid kind")
      self%kind = kind
      call to_lower_case_(self%kind)

      ! The SG kinds are whole recipes: their own radial grid and pruning
      ! for each element, and no atomic scale factor.
      if (is_SG_kind_(self)) then
         self%pruning_scheme = self%kind
         self%scale_atomic_grids = FALSE
         self%reduce_H_angular_grid = FALSE
      end if
      call set_zeta_(self)  ! leaky here
   end subroutine

   subroutine set_partition_scheme(self,scheme)
   ! Set the scheme used to partition a density-like function into
   ! "atomic" regions. Not PURE: the live default is an UNKNOWN, because
   ! the partition dispatcher is PURE and cannot carry one itself.
      type(BECKE_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: scheme
      self%partition_scheme = scheme
      call to_lower_case_(self%partition_scheme)
      select case (self%partition_scheme)

      case ("becke             ")

      case ("stratmann_scuseria")
         call set_partition_scaling_scheme_(self,"none")

      case default; allocate(tonto%known_keywords(2))
      tonto%known_keywords(1) = "becke             "
      tonto%known_keywords(2) = "stratmann_scuseria"
      call unknown_(tonto,self%partition_scheme,"BECKE_GRID:set_partition_scheme")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine set_partition_scaling_scheme(self,scheme)
   ! Set the atomic-size adjustment applied by a Becke partition.
   ! Not PURE, for the same reason as set_partition_scheme.
      type(BECKE_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: scheme
      self%partition_scaling_scheme = scheme
      call to_lower_case_(self%partition_scaling_scheme)
      select case (self%partition_scaling_scheme)
      case ("none             ")
      case ("becke            ")
      case ("treutler_ahlrichs")
      case default; allocate(tonto%known_keywords(3))
      tonto%known_keywords(1) = "none             "
      tonto%known_keywords(2) = "becke            "
      tonto%known_keywords(3) = "treutler_ahlrichs"
      call unknown_(tonto,self%partition_scaling_scheme,"BECKE_GRID:set_partition_scaling_scheme")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine set_pruning_scheme(self,scheme)
   ! Set the pruning scheme to reduce angular momentum of the angular grids near
   ! atomic centers.
      type(BECKE_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: scheme

   ! NOTE "treutler_ahlrichs" was spelled "truetler" here while the select case
   ! below has it right, so in a DEBUG build this ENSURE rejected the CORRECT
   ! name and accepted the misspelled one (which then matched no case and did
   ! nothing). Release never saw it, ENSURE being compiled out there.
   ! "jayatilaka2" was removed 2026-08-14 -- see apply_pruning_scheme.
   ENSURE(is_one_of_(scheme,["none             ","jayatilaka0      ","jayatilaka1      ","treutler_ahlrichs","adaptive         "]),"BECKE_GRID:set_pruning_scheme ... unknown pruning scheme")
      self%pruning_scheme = scheme
      call to_lower_case_(self%pruning_scheme)

      ! A LIVE default, not just the ENSURE above: ENSURE is compiled out in a
      ! release build, and apply_pruning_scheme has no default either, so an
      ! unrecognised scheme would silently apply NO PRUNING AT ALL rather than
      ! failing. That matters now that "jayatilaka2" has been removed -- an old
      ! job file still naming it must say so, not quietly change grids.
      ! NOT PURE for this reason: UNKNOWN expands to an allocate, which is
      ! illegal in a pure procedure. The only caller is read_pruning_scheme.
      select case (self%pruning_scheme)
      case ("none             ")
      case ("jayatilaka0      ")
      case ("jayatilaka1      ")
      case ("treutler_ahlrichs")
      case ("adaptive         ")
      case default; allocate(tonto%known_keywords(5))
      tonto%known_keywords(1) = "none             "
      tonto%known_keywords(2) = "jayatilaka0      "
      tonto%known_keywords(3) = "jayatilaka1      "
      tonto%known_keywords(4) = "treutler_ahlrichs"
      tonto%known_keywords(5) = "adaptive         "
      call unknown_(tonto,self%pruning_scheme,"BECKE_GRID:set_pruning_scheme")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   PURE subroutine set_Salvador_bonds(self,bonds)
   ! Set the Salvador bonds for each atom's neighbours.
      type(BECKE_GRID_TYPE), INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), IN :: bonds
      self%Salvador_bonds_for = bonds
   end subroutine

   PURE subroutine set_Salvador_radii(self,radii)
   ! Set the Salvador radii for each atom's neighbours.
      type(BECKE_GRID_TYPE), INOUT :: self
      VEC(type(EVEC_REAL_TYPE),:), IN :: radii
      self%Salvador_radii_for = radii
   end subroutine

   PURE subroutine set_Salvador_chi(self,ratio)
   ! Set the Salvador chi ratios for each atom's neighbours.
      type(BECKE_GRID_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: ratio
      self%Salvador_chi = ratio
   end subroutine

   PURE subroutine set_zeta_0(self)
   ! Set the atom_zeta scaling parameters. We store them in case they need to be
   ! manually changed ...
      type(BECKE_GRID_TYPE), INOUT :: self
      REAL :: fac
   ENSURE(is_one_of_(self%kind,["becke            ","treutler_ahlrichs","mura_knowles     ","sg-0             ","sg-1             ","sg-2             ","sg-3             "]),"BECKE_GRID:set_zeta_0 ... unknown grid kind")
      call destroy_(self%atom_zeta)
      if (self%scale_atomic_grids) then
         select case (self%kind)

         case ("becke                    ")
            self%atom_zeta = bragg_slater_radius
            fac = HALF*BOHR_PER_ANGSTROM
            self%atom_zeta = fac*self%atom_zeta
            ! H and He are the Bragg radius
            self%atom_zeta(1) = TWO*self%atom_zeta(1)
            self%atom_zeta(2) = TWO*self%atom_zeta(2)

         case ("treutler_ahlrichs        ")
            self%atom_zeta = TA_zeta  ! atomic units, Table I of the paper

         case ("mura_knowles             ")
            self%atom_zeta = MK_zeta
         end select
      else
         call create_(self%atom_zeta,89)
         self%atom_zeta = ONE
      end if
   end subroutine

   PURE subroutine set_zeta_1(self,i,zeta)
   ! Set the zeta value for element "i" to be "zeta".
   ! NOTE: This is ain atomic units
      type(BECKE_GRID_TYPE), INOUT :: self
      INT, IN :: i
      REAL, IN :: zeta
   ENSURE(allocated(self%atom_zeta),"BECKE_GRID:set_zeta_1 ... no atom_zeta array, set atom positions first")
   ENSURE(is_in_range_(i,[1,89]),"BECKE_GRID:set_zeta_1 ... i is not in the allowed range")
      self%atom_zeta(i) = zeta
   end subroutine

   PURE subroutine set_accuracy(self,acc)
   ! Set the accuracy. NOTE: the actual integration accuracy is affected not
   ! only by this keyword, but also by the switch .reduce_H_angular_grid. It
   ! will also depend on .n_extra_pts_per_shell.
      type(BECKE_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: acc
   ENSURE(is_one_of_(acc,["very_low ","low      ","medium   ","high     ","very_high","extreme  ","best     "]),"BECKE_GRID:set_accuracy ... unknown accuracy")
      self%accuracy = acc
      call to_lower_case_(self%accuracy)
      select case (self%accuracy)

      case ("very_low")
         ! These are Treutler-Ahlrichs settings
         call set_l_H_angular_grid_(self,11)
         call set_l_angular_grid_(self,17)
         call set_n_radial_pts_(self,20)
         call set_l_bonding_angular_grid_(self,17)

      case ("low")
         call set_l_H_angular_grid_(self,17)
         call set_l_angular_grid_(self,23)
         call set_n_radial_pts_(self,25)
         call set_l_bonding_angular_grid_(self,23)

      case ("medium")
         call set_l_H_angular_grid_(self,23)
         call set_l_angular_grid_(self,29)
         call set_n_radial_pts_(self,30)
         call set_l_bonding_angular_grid_(self,29)

      case ("high")
         call set_l_H_angular_grid_(self,29)
         call set_l_angular_grid_(self,35)
         call set_n_radial_pts_(self,35)
         call set_l_bonding_angular_grid_(self,59)

      case ("very_high")
         call set_l_H_angular_grid_(self,35)
         call set_l_angular_grid_(self,47)
         call set_n_radial_pts_(self,45)
         call set_l_bonding_angular_grid_(self,59)

      case ("extreme")
         ! These are better than the Mura-Knowles settings
         call set_l_H_angular_grid_(self,47)
         call set_l_angular_grid_(self,59)
         call set_n_radial_pts_(self,55)
         call set_l_bonding_angular_grid_(self,71)

      case ("best")
         call set_l_H_angular_grid_(self,59)
         call set_l_angular_grid_(self,71)
         call set_n_radial_pts_(self,65)
         call set_l_bonding_angular_grid_(self,83)
      end select
   end subroutine

   PURE subroutine set_atom_info(self,atom)
   ! Set the list of "atoms" ... used e.g. to estimate when grid
   ! points too far away from an atom become insignificant
      type(BECKE_GRID_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
   ENSURE(self%kind/=" ","BECKE_GRID:set_atom_info ... must set kind of quadrature first")
      call destroy_atom_info_(self)
      self%atom = atom
   end subroutine

   PURE subroutine set_basis_fn_cutoff(self,cutoff)
   ! Set .basis_fn_cutoff, the value below which function values
   ! are assumed to be zero, so as to eliminate *atom* grid points.
      type(BECKE_GRID_TYPE), INOUT :: self
      REAL, IN :: cutoff
   ENSURE(cutoff>=ZERO,"BECKE_GRID:set_basis_fn_cutoff ... cutoff must be non-negative")
      self%basis_fn_cutoff = cutoff
   end subroutine

   PURE subroutine set_partition_cutoff(self,cutoff)
   ! Set .partition_cutoff, the value below which the partition function values
   ! are assumed to be zero, so as to eliminate (full) grid points *after*
   ! partitioning.
      type(BECKE_GRID_TYPE), INOUT :: self
      REAL, IN :: cutoff
   ENSURE(cutoff>=ZERO,"BECKE_GRID:set_partition_cutoff ... cutoff must be non-negative")
      self%partition_cutoff = cutoff
   end subroutine

   PURE subroutine set_rho_cutoff(self,cutoff)
   ! If the density goes below ".rho_cutoff" then the functional, its energy
   ! density, and its potential are all set to zero.
      type(BECKE_GRID_TYPE), INOUT :: self
      REAL, IN :: cutoff
      self%rho_cutoff = cutoff
   end subroutine

   PURE subroutine set_n_radial_pts(self,n)
   ! Set the number of radial points wanted for the Hydrogen atom.
   ! The number of points used for other atoms is more: 5 more points are used
   ! for every atomic shell that the atom has more than Hydrogen.
      type(BECKE_GRID_TYPE), INOUT :: self
      INT, IN :: n
   ENSURE(n>1,"BECKE_GRID:set_n_radial_pts ... n must be greater than one")
      self%n_radial_pts = n
   end subroutine

   PURE subroutine set_n_extra_pts_per_shell(self,n)
   ! Set the number of extra radial points to use per n-shell over and above the
   ! default number .n_radial_pts wanted for the Hydrogen atom. Normally this
   ! is set to 5 according to Becke's rule of thumb. You might want to set it to
   ! zero to get uniform grids for all the atoms.
      type(BECKE_GRID_TYPE), INOUT :: self
      INT, IN :: n
   ENSURE(n>=0,"BECKE_GRID:set_n_extra_pts_per_shell ... n must be non-negative")
      self%n_extra_pts_per_shell = n
   end subroutine

   PURE subroutine set_l_angular_grid(self,l)
   ! Set the angular quantum number "l" which is to be integrated
   ! exactly by the angular grid.
      type(BECKE_GRID_TYPE), INOUT :: self
      INT, IN :: l
      self%l_angular_grid = l
   end subroutine

   PURE subroutine set_l_H_angular_grid(self,l)
   ! Set the angular quantum number "l" which is to be integrated
   ! exactly by the angular grid for the Hydrogen and Helium atoms only.
      type(BECKE_GRID_TYPE), INOUT :: self
      INT, IN :: l
      self%l_H_angular_grid = l
   end subroutine

   PURE subroutine set_l_bonding_angular_grid(self,l)
   ! Set the Lebedev order "l" for the bonding zone of non-H atoms, used
   ! only by the "adaptive" pruning scheme.
      type(BECKE_GRID_TYPE), INOUT :: self
      INT, IN :: l
      self%l_bonding_angular_grid = l
   end subroutine

   PURE subroutine set_reduce_H_angular_grid(self,reduce)
   ! If set TRUE the order of the angular grid for Hydrogen and Helium
   ! are reduced to .l_H_angular_grid.
      type(BECKE_GRID_TYPE), INOUT :: self
      BIN, IN :: reduce
      self%reduce_H_angular_grid = reduce
   end subroutine

   PURE subroutine set_scale_atomic_grids(self,scale)
   ! If set TRUE, the atomic grids are scaled according to the authors
   ! recommended scaling factors. This does *not* mean that the
   ! partition function is also scaled to account for varying atomic
   ! sizes.
      type(BECKE_GRID_TYPE), INOUT :: self
      BIN, IN :: scale
      self%scale_atomic_grids = scale
      call set_zeta_(self)
   end subroutine


!  ====
!  Read
!  ====

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(BECKE_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","BECKE_GRID:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"BECKE_GRID:read_keywords ... unexpected end of file while reading list keywords")
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
      type(BECKE_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                         ")  ! exit case
      case ("accuracy=                 "); call read_accuracy_(self)
      case ("basis_fn_cutoff=          "); call read_basis_fn_cutoff_(self)
      case ("basis_function_cutoff=    "); call read_basis_fn_cutoff_(self)
      case ("finalize                  "); call set_grid_data_(self)
      case ("kind=                     "); call read_kind_(self)
      case ("l_angular_grid=           "); call read_l_angular_grid_(self)
      case ("l_bonding_angular_grid=   "); call read_l_bonding_angular_grid_(self)
      case ("l_h_angular_grid=         "); call read_l_H_angular_grid_(self)
      case ("n_extra_pts_per_shell=    "); call read_n_extra_pts_per_shell_(self)
      case ("n_radial_pts=             "); call read_n_radial_pts_(self)
      case ("reduce_h_angular_grid=    "); call read_reduce_H_angular_grid_(self)
      case ("scale_atomic_grids=       "); call read_scale_atomic_grids_(self)
      case ("partition_cutoff=         "); call read_partition_cutoff_(self)
      case ("partition_scheme=         "); call read_partition_scheme_(self)
      case ("partition_scaling_scheme= "); call read_partition_scaling_scheme_(self)
      case ("pruning_scheme=           "); call read_pruning_scheme_(self)
      case ("put                       "); call put_(self)
      case ("put_basics                "); call put_basics_(self)
      case ("put_atom_info             "); call put_atom_info_(self)
      case ("put_radial_angular_grids  "); call put_radial_angular_grids_(self)
      case ("put_unique_atom_grids     "); call put_unique_atom_grids_(self)
      case ("prune_rho_cutoff=         "); call read_prune_rho_cutoff_(self)
      case ("rho_cutoff=               "); call read_rho_cutoff_(self)
      case ("set_grid_data             "); call set_grid_data_(self)
      case ("set_defaults              "); call set_defaults_(self)
      case ("xc_batch_box_edge=        "); call read_XC_batch_box_edge_(self)
      case ("xc_batch_max_pts=         "); call read_XC_batch_max_pts_(self)
      case default; allocate(tonto%known_keywords(28))
      tonto%known_keywords(1) = "}                         "
      tonto%known_keywords(2) = "accuracy=                 "
      tonto%known_keywords(3) = "basis_fn_cutoff=          "
      tonto%known_keywords(4) = "basis_function_cutoff=    "
      tonto%known_keywords(5) = "finalize                  "
      tonto%known_keywords(6) = "kind=                     "
      tonto%known_keywords(7) = "l_angular_grid=           "
      tonto%known_keywords(8) = "l_bonding_angular_grid=   "
      tonto%known_keywords(9) = "l_h_angular_grid=         "
      tonto%known_keywords(10) = "n_extra_pts_per_shell=    "
      tonto%known_keywords(11) = "n_radial_pts=             "
      tonto%known_keywords(12) = "reduce_h_angular_grid=    "
      tonto%known_keywords(13) = "scale_atomic_grids=       "
      tonto%known_keywords(14) = "partition_cutoff=         "
      tonto%known_keywords(15) = "partition_scheme=         "
      tonto%known_keywords(16) = "partition_scaling_scheme= "
      tonto%known_keywords(17) = "pruning_scheme=           "
      tonto%known_keywords(18) = "put                       "
      tonto%known_keywords(19) = "put_basics                "
      tonto%known_keywords(20) = "put_atom_info             "
      tonto%known_keywords(21) = "put_radial_angular_grids  "
      tonto%known_keywords(22) = "put_unique_atom_grids     "
      tonto%known_keywords(23) = "prune_rho_cutoff=         "
      tonto%known_keywords(24) = "rho_cutoff=               "
      tonto%known_keywords(25) = "set_grid_data             "
      tonto%known_keywords(26) = "set_defaults              "
      tonto%known_keywords(27) = "xc_batch_box_edge=        "
      tonto%known_keywords(28) = "xc_batch_max_pts=         "
      call unknown_(tonto,word,"BECKE_GRID:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_accuracy(self)
   ! Read the accuracy of the numerical integration
      type(BECKE_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
      call read_(stdin,word)
      call set_accuracy_(self,word)
      self%finalized = FALSE
   end subroutine

   subroutine read_kind(self)
   ! Read the .kind of quadrature desired
      type(BECKE_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
      call read_(stdin,word)
      call to_lower_case_(word)
      call set_kind_(self,word)
      self%finalized = FALSE
   end subroutine

   subroutine read_partition_scheme(self)
   ! Set the partition method to use. Normally it is "becke".
      type(BECKE_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: s
      call read_(stdin,s)
      call set_partition_scheme_(self,s)
      self%finalized = FALSE
   end subroutine

   subroutine read_partition_scaling_scheme(self)
   ! Set the partition scaling method to use. Normally "treutler_ahlrichs".
      type(BECKE_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: s
      call read_(stdin,s)
      call set_partition_scaling_scheme_(self,s)
      self%finalized = FALSE
   end subroutine

   subroutine read_pruning_scheme(self)
   ! Set the pruning scheme to reduce angular momentum of the angular grids near
   ! atomic centers.
      type(BECKE_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: s
      call read_(stdin,s)
      call set_pruning_scheme_(self,s)
      self%finalized = FALSE
   end subroutine

   subroutine read_l_angular_grid(self)
   ! Read .l_angular_grid, the angular momentum used for the angular grid
      type(BECKE_GRID_TYPE), INOUT :: self
      INT :: l
      call read_(stdin,l)
      call set_l_angular_grid_(self,l)
      self%finalized = FALSE
   end subroutine

   subroutine read_l_H_angular_grid(self)
   ! Read .l_H_angular_grid, the angular momentum used for the angular grid for
   ! the Hydrogen atom.
      type(BECKE_GRID_TYPE), INOUT :: self
      INT :: l
      call read_(stdin,l)
      call set_l_H_angular_grid_(self,l)
      self%finalized = FALSE
   end subroutine

   subroutine read_l_bonding_angular_grid(self)
   ! Read .l_bonding_angular_grid, the Lebedev order in the bonding zone of
   ! non-H atoms for the "adaptive" pruning scheme.
      type(BECKE_GRID_TYPE), INOUT :: self
      INT :: l
      call read_(stdin,l)
      call set_l_bonding_angular_grid_(self,l)
      self%finalized = FALSE
   end subroutine

   subroutine read_n_extra_pts_per_shell(self)
   ! Read .n_extra_pts_per_shell,
      type(BECKE_GRID_TYPE), INOUT :: self
      INT :: n
      call read_(stdin,n)
      call set_n_extra_pts_per_shell_(self,n)
      self%finalized = FALSE
   end subroutine

   subroutine read_n_radial_pts(self)
   ! Read .n_radial_pnts, required to defined the radial quadrature
      type(BECKE_GRID_TYPE), INOUT :: self
      INT :: n
      call read_(stdin,n)
      call set_n_radial_pts_(self,n)
      self%finalized = FALSE
   end subroutine

   subroutine read_reduce_H_angular_grid(self)
   ! Read whether to reduce the angular momentum of the angular grid near the
   ! core of a *hydrogen* atom
      type(BECKE_GRID_TYPE), INOUT :: self
      BIN :: l
      call read_(stdin,l)
      call set_reduce_H_angular_grid_(self,l)
      self%finalized = FALSE
   end subroutine

   subroutine read_scale_atomic_grids(self)
   ! Read whether to scale the sizes of the atomic grids to account for
   ! different atomic sizes. NOTE: this does not also scale the partitioning
   ! scheme, for this set the .partition_scaling_scheme.
      type(BECKE_GRID_TYPE), INOUT :: self
      BIN :: l
      call read_(stdin,l)
      call set_scale_atomic_grids_(self,l)
      self%finalized = FALSE
   end subroutine

   subroutine read_basis_fn_cutoff(self)
   ! Read .basis_fn_cutoff, the value below which function values are
   ! assumed to be zero, so as to eliminate *atom* grid points.
      type(BECKE_GRID_TYPE), INOUT :: self
      REAL :: cutoff
      call read_(stdin,cutoff)
      call set_basis_fn_cutoff_(self,cutoff)
      self%finalized = FALSE
   end subroutine

   subroutine read_partition_cutoff(self)
   ! Read .partition_cutoff, the value below which the partition function values
   ! are assumed to be zero, so as to eliminate (full) grid points *after*
   ! partitioning.
      type(BECKE_GRID_TYPE), INOUT :: self
      REAL :: cutoff
      call read_(stdin,cutoff)
      call set_partition_cutoff_(self,cutoff)
      self%finalized = FALSE
   end subroutine

   subroutine read_prune_rho_cutoff(self)
   ! Read the promolecule density below which DFT grid points are dropped.
      type(BECKE_GRID_TYPE), INOUT :: self
      call read_quantity_(stdin,self%prune_rho_cutoff)
      self%finalized = FALSE
   end subroutine

   subroutine read_XC_batch_max_pts(self)
   ! Read the most points in one batch of the molecular DFT grid.
      type(BECKE_GRID_TYPE), INOUT :: self
      call read_(stdin,self%XC_batch_max_pts)
      ENSURE(self%XC_batch_max_pts>0,"BECKE_GRID:read_XC_batch_max_pts ... XC_batch_max_pts must be positive")
   end subroutine

   subroutine read_XC_batch_box_edge(self)
   ! Read the edge of the cubes the molecular DFT grid is first cut into.
      type(BECKE_GRID_TYPE), INOUT :: self
      call read_quantity_(stdin,self%XC_batch_box_edge)
      ENSURE(self%XC_batch_box_edge>ZERO,"BECKE_GRID:read_XC_batch_box_edge ... XC_batch_box_edge must be positive")
   end subroutine

   subroutine read_rho_cutoff(self)
   ! If the density goes below ".rho_cutoff" then the functional, its energy
   ! density, and its potential are all set to zero.
      type(BECKE_GRID_TYPE), INOUT :: self
      call read_quantity_(stdin,self%rho_cutoff)
      self%finalized = FALSE
   end subroutine


!  ============================================
!  Radial and angular grids & unique atom grids
!  ============================================

!  This is the main finalization ...

   PURE subroutine update(self)
   ! Set all the radial and angular grid data required to construct
   ! the product grids for any atom in the periodic table. Then define
   ! the unique atomic grids for the current set of atoms.
      type(BECKE_GRID_TYPE), INOUT :: self
      call set_grid_data_(self)
   end subroutine

   PURE subroutine set_grid_data(self)
   ! Set all the radial and angular grid data required to construct
   ! the product grids for any atom in the periodic table. Then define
   ! the unique atomic grids for the current set of atoms.
      type(BECKE_GRID_TYPE), INOUT :: self
      if (NOT allocated(self%atom)) return

      ! Radial and angular grids come first
      call set_radial_grids_(self)
      call set_angular_grids_(self)

      ! Point counting routines next, for dimensioning
      call set_n_pts_for_row_(self)

      ! Finally, make the unique atomic grids
      call set_unique_atoms_(self)
      call set_atom_grids_(self)
      call set_atom_density_grids_(self)
      call set_atom_min_distance_(self)
      call set_atom_n1_SS_(self)
      self%finalized = TRUE
   end subroutine

   PURE subroutine set_one_atom_grid_data(self)
   ! Set all the radial and angular grid data required to construct the product
   ! grids for any atom in the periodic table. Then define the unique atomic
   ! grids for the current set of atoms.
      type(BECKE_GRID_TYPE), INOUT :: self
      if (NOT allocated(self%atom)) return

      ! Radial and angular grids come first
      call set_radial_grids_(self)
      call set_angular_grids_(self)

      ! Point counting routines next, for dimensioning
      call set_n_pts_for_row_(self)

      ! Finally, make the unique atomic grids
      call set_unique_atoms_(self)
      call set_atom_grids_(self)
      self%finalized = TRUE
   end subroutine


!  The following are private helper routines

   PURE subroutine set_radial_grids(self)
   ! Set all possible .radial_grid's that could be needed. Since a
   ! different number of radial points is used for atoms in different
   ! rows of the periodic table, this required a loop over the rows of
   ! the periodic table.
      type(BECKE_GRID_TYPE), INOUT :: self
      INT :: p,n
   ENSURE(self%kind/=" ","BECKE_GRID:set_radial_grids ... no grid kind set")

      ! One per row, or one per element for the SG kinds
      n = 7
      if (is_SG_kind_(self)) n = 103
      call create_(self%radial_grid,n)
      do p = 1,n
         call set_radial_grid_(self,self%radial_grid(p),p)
      end do
   end subroutine

   PURE subroutine set_radial_grid(self,radial,p)
   ! Set the "radial" grid data for an atom on period "p" of the
   ! periodic table.  The standard radial grid points are modified
   ! using the mappings recommended by the authors. The jacoboian of
   ! the transformation is included back into the weight. The zeta
   ! scale factor is *not* included at this stage, only at the final
   ! stage when the full grid is generated for the entire atom list.
      type(BECKE_GRID_TYPE), INOUT :: self
      type(QUADRATURE_TYPE), INOUT :: radial
      INT, IN :: p
      INT :: nr
      STR(len=STR_SIZE) :: scheme
      REAL :: length
   ENSURE(self%kind/=" ","BECKE_GRID:set_radial_grid ... no grid kind set")
      nr = no_of_radial_pts_(self,p)
      select case (self%kind)
      case ("becke            "); call set_B_radial_grid(radial,nr)
      case ("treutler_ahlrichs"); call set_TA_radial_grid(radial,nr)
      case ("mura_knowles     "); call set_MK_radial_grid(radial,nr)  ! default
      case ("sg-0","sg-1","sg-2","sg-3")
         ! "p" is the atomic number
         call SG_radial_scheme(self%kind,p,scheme,nr,length)
         select case (scheme)
         case ("multiexp       "); call set_MultiExp_radial_grid(radial,nr,length)
         case ("euler_maclaurin"); call set_EM_radial_grid(radial,nr,length)
         case ("de2            "); call set_DE2_radial_grid(radial,nr,length,SG_radius(p))
         end select
      end select
   end subroutine

   PURE function period_of(self,a) result (res)
   ! The index of atom "a"'s radial grid in .radial_grid: its
   ! periodic-table row, or its atomic number for the SG kinds, which
   ! have one radial grid per element.
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: a
      INT :: res
      if (is_SG_kind_(self)) then; res = self%atom(a)%atomic_number
      else; res = period(self%atom(a)%atomic_number)
      end if
   end function

   pure function is_SG_kind(self) result (res)
   ! Whether the grid is one of the published standard grids, SG-0 to
   ! SG-3. See docs/TASK_COSX_GRIDS.md.
      type(BECKE_GRID_TYPE), IN :: self
      BIN :: res
      res = self%kind=="sg-0" OR self%kind=="sg-1" OR self%kind=="sg-2" OR self%kind=="sg-3"
   end function

   PURE function n_radial_pts_for_atom(self,a) result (res)
   ! The number of radial shells in the grid of atom "a".
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: a
      INT :: res
      res = no_of_radial_pts_(self,period_of_(self,a))
   end function

   PURE subroutine radial_shell_for_atom(self,a,i,r,w)
   ! The radius "r" (bohr, scaled) of radial shell "i" of atom "a" and
   ! its radial weight "w", 4 pi r^2 dr with the atomic scale factor
   ! folded in, as set_unscaled_grid and set_atom_grids apply them.
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: a,i
      REAL, OUT :: r,w
      INT :: p
      REAL :: s,r0,w0
      p = period_of_(self,a)
      r0 = self%radial_grid(p)%pts(i)
      w0 = self%radial_grid(p)%wts(i)
      s = ONE
      if (self%scale_atomic_grids) s = self%atom_zeta(self%atom(a)%atomic_number)
      r = s*r0
      w = FOUR*PI*r0*r0*w0*s*s*s
   end subroutine

   PURE function angular_order_for_shell(self,a,i) result (res)
   ! The Lebedev order the pruning scheme assigns to radial shell "i" of
   ! atom "a".
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: a,i
      INT :: res
      type(LEBEDEV_TYPE), allocatable :: lebedev_grid
      INT :: p,nr
      REAL :: r,w
      p = period_of_(self,a)
      nr = no_of_radial_pts_(self,p)
      call radial_shell_for_atom_(self,a,i,r,w)
      call apply_pruning_scheme_(self,lebedev_grid,p,i,nr,r)
      res = lebedev_grid%l
   end function

   PURE function no_of_radial_pts(self,p) result (res)
   ! Return the number of radial points in the grid for an atom in period "p" of
   ! the periodic table: .n_radial_pts for H and He, and for every further
   ! period .n_extra_pts_per_shell more, up to Ar. From K on the core needs
   ! many more than that rule gives: 9, 20 and 32 times .n_extra_pts_per_shell
   ! are added in periods 4, 5 and 6, so that a DFT energy is good to about
   ! 1e-5 hartree at the default accuracy, as it is for the lighter atoms.
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: p
      INT :: res
      STR(len=STR_SIZE) :: scheme
      REAL :: length
      INT :: n_extra
      if (is_SG_kind_(self)) then
         call SG_radial_scheme(self%kind,p,scheme,res,length)  ! "p" is the atomic number
      else
         select case (p)
         case (:3); n_extra = p-1
         case (4); n_extra = 9
         case (5); n_extra = 20
         case default; n_extra = 32
         end select
         res = self%n_radial_pts+self%n_extra_pts_per_shell*n_extra
      end if
   end function

   PURE subroutine set_B_radial_grid(radial_grid,nr)
   ! Set the Becke "radial_grid" to have "nr" points. The standard grid points
   ! are modified using the recommended mappings. The jacoboian of the mapping
   ! is included back into the weight. The zeta scale factor is *not* included
   ! at this stage, only at the final stage when the full grid is generated for
   ! the entire atom list.
      type(QUADRATURE_TYPE), INOUT :: radial_grid
      INT, IN :: nr
      INT :: i
      REAL :: r,w,rr,ww,rp1,rm1
      call set_n_pts_(radial_grid,nr)
      call set_kind_(radial_grid,"gauss_chebyshev_2nd")
   ! radial_grid.set_kind("gauss_chebyshev_1st")
      call set_quadrature_points_(radial_grid)  ! leaky here

      ! Apply the Becke mapping r = (1+x)/(1-x), for x in [-1,1]
      do i = 1,radial_grid%n_pts
         r = radial_grid%pts(i)
         w = radial_grid%wts(i)
         rp1 = ONE+r
         rm1 = ONE-r
         rr = rp1/rm1
         ww = TWO*w/(rm1*rm1)
         radial_grid%pts(i) = rr
         radial_grid%wts(i) = ww
      end do
      call reverse_order_(radial_grid%pts)
      call reverse_order_(radial_grid%wts)
   end subroutine

   PURE subroutine set_TA_radial_grid(radial_grid,nr)
   ! Set the Treutler-Ahlrichs "radial_grid" to have "nr" points.
   ! The standard grid points are modified using the recommended
   ! mappings. The jacobian of the mapping is included back into the
   ! weight. The zeta scale factor is *not* included at this stage,
   ! only at the final stage when the full grid is generated for the
   ! entire atom list.
      type(QUADRATURE_TYPE), INOUT :: radial_grid
      INT, IN :: nr
      INT :: i
      REAL :: r,w,rr,ww,rp1,rm1,ra1,ra,ln2,a,a1,tmp
      call set_n_pts_(radial_grid,nr)
      call set_kind_(radial_grid,"gauss_chebyshev_2nd")
      call set_quadrature_points_(radial_grid)  ! leaky here

      ! Apply the Treutler-Ahlrichs mapping
      ! r = (ln 2)^-1 (1+x)^0.6 ln(2/(1-x)), for x in [-1,1]
      ln2 = ONE/log(TWO)
      a = 0.6d0
      a1 = a-ONE
      do i = 1,radial_grid%n_pts
         r = radial_grid%pts(i)
         w = radial_grid%wts(i)
         rp1 = ONE+r
         rm1 = ONE-r
         ra1 = rp1**a1
         ra = ra1*rp1
         tmp = ONE-ln2*log(rm1)
         rr = ra*tmp
         ww = ra1*(a*tmp+ln2*rp1/rm1)*w
         radial_grid%pts(i) = rr
         radial_grid%wts(i) = ww
      end do
      call reverse_order_(radial_grid%pts)
      call reverse_order_(radial_grid%wts)
   end subroutine

   PURE subroutine set_MK_radial_grid(radial_grid,nr)
   ! Set the Mura-Knowles "radial_grid" to have "nr" points.
   ! The standard grid points are modified using the recommended
   ! mappings. The jacoboian of the mapping is included back into the
   ! weight. The zeta scale factor is *not* included at this stage,
   ! only at the final stage when the full grid is generated for the
   ! entire atom list.
      type(QUADRATURE_TYPE), INOUT :: radial_grid
      INT, IN :: nr
      INT :: i,m,m1
      REAL :: r,w,rr,ww,rm1,rm,tmp
      call set_n_pts_(radial_grid,nr)
   ! radial_grid.set_kind("open_rectangle") ! eliminate end points
   ! radial_grid.set_kind("open_extended_simpson") ! seems not to work
      call set_kind_(radial_grid,"open_extended_trapezoid")  ! eliminate end points
   ! radial_grid.set_kind("gauss_legendre")
      call set_quadrature_points_(radial_grid)  ! leaky here

      ! Apply the Mura-Knowles log 3 mapping
      ! r = -ln (1-x^3), for x in [0,1]
      m = 3
      m1 = m-1
      do i = 1,radial_grid%n_pts
         r = radial_grid%pts(i)
         w = radial_grid%wts(i)
         rm1 = r**m1
         rm = r*rm1
         tmp = ONE-rm
         rr = -log(tmp)
         ww = (ONE/tmp)*m*rm1*w
         radial_grid%pts(i) = rr
         radial_grid%wts(i) = ww
      end do
   end subroutine

   PURE subroutine set_angular_grids(self)
   ! Set all possible angular grid's that might be needed. At the moment only
   ! spherical Lebedev grids are available, but they are probably the best
   ! anyway ...
      type(BECKE_GRID_TYPE), INOUT :: self
   ENSURE(self%l_angular_grid>=11,"BECKE_GRID:set_angular_grids ... l recommended greater than 11")
   ENSURE(self%l_H_angular_grid>=11,"BECKE_GRID:set_angular_grids ... l_H recommended greater than 11")
      call create_(self%lebedev_grid)  ! Default grid
      call set_l_(self%lebedev_grid,self%l_angular_grid)
      call create_(self%lebedev_H_grid)  ! Needed for H atoms
      call set_l_(self%lebedev_H_grid,self%l_H_angular_grid)
      call create_(self%lebedev_L3_grid)  ! Needed for reduced grids near atom core
      call set_l_(self%lebedev_L3_grid,3)
      call create_(self%lebedev_L5_grid)
      call set_l_(self%lebedev_L5_grid,5)
      call create_(self%lebedev_L7_grid)
      call set_l_(self%lebedev_L7_grid,7)
      call create_(self%lebedev_L9_grid)
      call set_l_(self%lebedev_L9_grid,9)
      call create_(self%lebedev_L11_grid)
      call set_l_(self%lebedev_L11_grid,11)
   end subroutine

   PURE subroutine set_n_pts_for_row(self)
   ! Set the number of points per row of the periodic table.
      type(BECKE_GRID_TYPE), INOUT :: self
      INT :: r
      call destroy_(self%n_pts_for_row)
      call create_(self%n_pts_for_row,size(self%radial_grid))
      do r = 1,size(self%radial_grid)  ! Set no. of points for each row
         self%n_pts_for_row(r) = no_of_pts_for_row_(self,r)
      end do
   end subroutine

   PURE subroutine set_unique_atoms(self)
   ! Make the list of unique_atoms, and a map array to the unique list of atoms.
      type(BECKE_GRID_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: an
   ENSURE(allocated(self%atom),"BECKE_GRID:set_unique_atoms ... no atom array")
      an = self%atom(:)%atomic_number
      call make_unique_element_maps_(an,self%unique_atom,self%unique_atom_for)
      self%n_unique_atoms = size(self%unique_atom)
   end subroutine

   PURE subroutine set_atom_grids(self)
   ! Make all the atom integration grids. These grids use ".atom"
   ! exponent information to decide when the basis functions for
   ! an atom will have small values, to decide when to prune the
   ! radial grid. The pruning of points with low partition weight
   ! factors is done at a later stage when the atoms in the partition
   ! are known. See the .prune_grid routines.
      type(BECKE_GRID_TYPE), INOUT :: self
      INT :: u,a,np
      REAL :: s,s3
   ENSURE(allocated(self%atom),"BECKE_GRID:set_atom_grids ... no atom list")
   ENSURE(allocated(self%unique_atom),"BECKE_GRID:set_atom_grids ... no unique_atom array")

      ! Create unique-atom points and weights
      call destroy_(self%atom_wts)
      call destroy_(self%atom_pts)
      call create_(self%atom_pts,self%n_unique_atoms)
      call create_(self%atom_wts,self%n_unique_atoms)

      ! Loop for each *unique* atom
      do u = 1,self%n_unique_atoms
         a = self%unique_atom(u)

         ! Set unscaled reference grid for atom "a"
         call set_unscaled_grid_(self,a)

         ! Get "np" points where BF's are large enough
         np = no_of_pts_for_atom_(self,a)

         ! Copy only "np" points, weights, and radii
         call create_(self%atom_pts(u)%element,np,3)
         call create_(self%atom_wts(u)%element,np)
         self%atom_pts(u)%element = self%unscaled_pts(1:np,:)  ! Copy here
         self%atom_wts(u)%element = self%unscaled_wts(1:np)

         ! Do scaling if needed
         if (self%scale_atomic_grids) then
            s = self%atom_zeta(self%atom(a)%atomic_number)
            s3 = s*s*s
            self%atom_pts(u)%element = s*self%atom_pts(u)%element
            self%atom_wts(u)%element = s3*self%atom_wts(u)%element
         end if

         ! Clean
         call destroy_unscaled_grid_(self)
      end do
   end subroutine

   PURE subroutine set_unscaled_grid(self,a)
   ! Set the *unscaled* spherical quadrature grid points and weights
   ! for atom "a". The unscaled grid is a direct product of the
   ! ".radial_grid(p)" for its row "p" and the ".lebedev" angular
   ! grids. Based on the .pruning_scheme this routine may reduce the
   ! order of the angular grid per radial shell. The "adaptive" scheme
   ! does so by the *scaled* radius, so the grid is made per atom.
      type(BECKE_GRID_TYPE), INOUT :: self
      INT, IN :: a
      INT :: i,j,k,np,nr,p
      REAL :: pi4,r,w,s,zeta
      type(LEBEDEV_TYPE), allocatable :: lebedev_grid
   ENSURE(allocated(self%radial_grid),"BECKE_GRID:set_unscaled_grid ... no radial_grid")
   ENSURE(allocated(self%lebedev_grid),"BECKE_GRID:set_unscaled_grid ... no lebedev_grid")
      p = period_of_(self,a)
      nr = self%radial_grid(p)%n_pts
      zeta = ONE
      if (self%scale_atomic_grids) zeta = self%atom_zeta(self%atom(a)%atomic_number)

      ! Count the points
      np = 0
      do i = 1,nr
         call apply_pruning_scheme_(self,lebedev_grid,p,i,nr,zeta*self%radial_grid(p)%pts(i))
         np = np+lebedev_grid%n_pts
      end do
      self%n_unscaled_pts = np
      call destroy_unscaled_grid_(self)
      call create_(self%unscaled_pts,np,3)
      call create_(self%unscaled_wts,np)

      ! Factor 4*pi*r^2 included into weights.
      pi4 = FOUR*PI
      k = 0

      ! Loop on radial grid points
      do i = 1,nr
         r = self%radial_grid(p)%pts(i)
         w = self%radial_grid(p)%wts(i)

      ! ! WARNING. Is below right?
      ! if (r<TOL(6)) then; s = pi4*r*r*r*w*THIRD
      ! else;               s = pi4*r*r*w
      ! end
         s = pi4*r*r*w

         ! Set the lebedev grid
         call apply_pruning_scheme_(self,lebedev_grid,p,i,nr,zeta*r)
         do j = 1,lebedev_grid%n_pts  ! <-- Loop on angular grid
            k = k+1
            self%unscaled_pts(k,1) = r*lebedev_grid%pts(j,1)
            self%unscaled_pts(k,2) = r*lebedev_grid%pts(j,2)
            self%unscaled_pts(k,3) = r*lebedev_grid%pts(j,3)
            self%unscaled_wts(k) = s*lebedev_grid%wts(j)
         end do
      end do
   end subroutine

   PURE subroutine set_atom_density_grids(self)
   ! Make all the atom basis function grids. This will use up a large slab of
   ! memory, but hopefully not too much, since it only depends on the number of
   ! different atoms in the molecule.
      type(BECKE_GRID_TYPE), INOUT :: self
! ENSURE(.atom.basis.allocated,"no atom basis!")
      INT :: u,a,np,nb
      VEC(REAL,3) :: pos
   ENSURE(allocated(self%atom),"BECKE_GRID:set_atom_density_grids ... no atom list")
   ENSURE(allocated(self%unique_atom),"BECKE_GRID:set_atom_density_grids ... no unique_atom array")
   ENSURE(allocated(self%atom_pts),"BECKE_GRID:set_atom_density_grids ... no atom_pts array")
      call destroy_(self%atom_grid0)
      call destroy_(self%atom_grid1)
      call create_(self%atom_grid0,self%n_unique_atoms)
      call create_(self%atom_grid1,self%n_unique_atoms)

      ! For each *unique* atom ...
      do u = 1,self%n_unique_atoms

         ! Create atom BF grids
         a = self%unique_atom(u)
         np = size(self%atom_pts(u)%element,1)
         nb = self%atom(a)%basis%n_bf
         call create_(self%atom_grid0(u)%element,np,nb)
         call create_(self%atom_grid1(u)%element,np,nb,3)

         ! Now make the atom BF grids
         pos = self%atom(a)%position
         call set_position_(self%atom(a),[ZERO,ZERO,ZERO])  ! Reset atom to origin temporarily
         call make_nabla_bf_grid_(self%atom(a),self%atom_grid1(u)%element,self%atom_grid0(u)%element,self%atom_pts(u)%element)
         call set_position_(self%atom(a),pos)
      end do
   end subroutine

   PURE subroutine set_atom_min_distance(self)
   ! Make the minimum distances squared between atoms.
   ! Required for the Stratmann-Scuseria partitioning.
      type(BECKE_GRID_TYPE), INOUT :: self
      INT :: n_atom,i,j
      REAL :: px,py,pz,dx,dy,dz,d2min,d2
   ENSURE(allocated(self%atom),"BECKE_GRID:set_atom_min_distance ... no atom list")

      ! No. of atoms
      n_atom = size(self%atom,1)

      ! Create minimum distance *squared*
      call destroy_(self%atom_min_distance)
      call create_(self%atom_min_distance,n_atom)

      ! Find minimum distance squared to atom i
      do i = 1,n_atom

         ! Atom i position
         px = self%atom(i)%position(1)
         py = self%atom(i)%position(2)
         pz = self%atom(i)%position(3)

         ! Minimum distance squared starts huge
         d2min = huge(ONE)

         ! Which of the atoms j is minimum?
         do j = 1,n_atom
            if (i==j) cycle

            ! x2 distance
            dx = px-self%atom(j)%position(1); dx = dx*dx
            if (dx>=d2min) cycle

            ! y2 distance
            dy = py-self%atom(j)%position(2); dy = dy*dy
            if (dy>=d2min) cycle

            ! z2 distance
            dz = pz-self%atom(j)%position(3); dz = dz*dz
            if (dz>=d2min) cycle
            d2 = dx+dy+dz
            if (d2>=d2min) cycle
            d2min = d2
         end do

         ! Store minimum distance
         self%atom_min_distance(i) = sqrt(d2min)
      end do
   end subroutine

   PURE subroutine set_atom_n1_SS(self)
   ! Set the n1_SS values for all atoms
      type(BECKE_GRID_TYPE), INOUT :: self
      INT :: n_atom,a
   ENSURE(allocated(self%atom),"BECKE_GRID:set_atom_n1_SS ... no atom list")
   ENSURE(allocated(self%unique_atom),"BECKE_GRID:set_atom_n1_SS ... no unique_atom array")
   ENSURE(allocated(self%atom_pts),"BECKE_GRID:set_atom_n1_SS ... no atom_pts array")
      n_atom = size(self%atom)
      call destroy_(self%atom_n1_SS)
      call create_(self%atom_n1_SS,n_atom)

      ! For each *unique* atom ...
      do a = 1,n_atom
         self%atom_n1_SS(a) = n1_SS_for_atom_(self,a)
      end do
   end subroutine


!  ===============================
!  Separate radial & angular grids
!  ===============================

! For spherical averaging

   PURE function radial_grid_for_atom(self,a) result (res)
   ! Return the radial integration grid for atom "a".
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: a
      type(QUADRATURE_TYPE), allocatable :: res
      INT :: p
      REAL :: s
   ENSURE(self%finalized,"BECKE_GRID:radial_grid_for_atom ... not finalized; use set_grid_data")
   ENSURE(allocated(self%radial_grid),"BECKE_GRID:radial_grid_for_atom ... no radial_grid")

      ! Period for atom "a"
      p = period_of_(self,a)
      res = self%radial_grid(p)

      ! Do scaling if needed
      if (self%scale_atomic_grids) then
         s = self%atom_zeta(self%atom(a)%atomic_number)
         res%pts = s*res%pts
      end if
   end function

   PURE function angular_grid_for_atom(self,a,i) result (res)
   ! Return the lebedev grid for atom "a" and "i"-th radial point
   ! NOTE: no factor 4 PI r^2 w_r, where w_r is the radial wt, is applied.
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: a
      INT, IN :: i
      type(LEBEDEV_TYPE), allocatable :: res
      INT :: p,nr
      REAL :: r,w
   ENSURE(self%finalized,"BECKE_GRID:angular_grid_for_atom ... not finalized; use set_grid_data")
   ENSURE(allocated(self%radial_grid),"BECKE_GRID:angular_grid_for_atom ... no radial_grid")
   ENSURE(allocated(self%lebedev_grid),"BECKE_GRID:angular_grid_for_atom ... no lebedev_grid")
   ENSURE(allocated(self%n_pts_for_row),"BECKE_GRID:angular_grid_for_atom ... no n_pts_for_row")

      ! Period for atom "a"
      p = period_of_(self,a)
      nr = self%radial_grid(p)%n_pts

      ! Get grid
      call radial_shell_for_atom_(self,a,i,r,w)
      call apply_pruning_scheme_(self,res,p,i,nr,r)
   end function


!  =======================
!  Pruning & no. of points
!  =======================

   PURE function no_of_pts_for_row(self,p) result (res)
   ! Return the number of points in the (radial x spherical) grid,
   ! assuming that the atom is in period "p". For "adaptive" pruning the
   ! radii are taken unscaled, so this is indicative only.
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: p
      INT :: res
      type(LEBEDEV_TYPE), allocatable :: lebedev_grid
      INT :: nr,i
   ENSURE(allocated(self%radial_grid),"BECKE_GRID:no_of_pts_for_row ... no radial_grid")
   ENSURE(p>=1 AND p<=size(self%radial_grid),"BECKE_GRID:no_of_pts_for_row ... p out of range")
   ENSURE(allocated(self%lebedev_grid),"BECKE_GRID:no_of_pts_for_row ... no lebedev_grid")
      nr = self%radial_grid(p)%n_pts
      res = 0
      do i = 1,nr

         ! Set lebedev grid
         call apply_pruning_scheme_(self,lebedev_grid,p,i,nr,self%radial_grid(p)%pts(i))
         res = res+lebedev_grid%n_pts
      end do
   end function

   PURE function no_of_pts_for_atom(self,a) result (res)
   ! Calculate the total number of points in the grid for atom "a".
   ! These exponents are used to define a corresponding gaussian;
   ! when that gaussian has value below .basis_fn_cutoff, the
   ! contribution of the radial point is assumed to be zero.
   ! NOTE: this routine assumes that
   ! . grid points are to be used for integrating products of basis
   !   functions on *all* atoms (may be at most two atoms).
   ! . radial grid points are ordered smallest to largest distance
   !   from the nucleus.
   ! . this routine depends on the atom scale factor, and the kind
   !   of radial grid.
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: a
      INT :: res
      type(LEBEDEV_TYPE), allocatable :: lebedev_grid
      INT :: p,nr,i
      REAL :: r_max,s,r
   ENSURE(allocated(self%radial_grid),"BECKE_GRID:no_of_pts_for_atom ... no radial_grid")
   ENSURE(allocated(self%lebedev_grid),"BECKE_GRID:no_of_pts_for_atom ... no lebedev_grid")
   ENSURE(allocated(self%atom),"BECKE_GRID:no_of_pts_for_atom ... no atom list")
   ENSURE(allocated(self%atom_zeta),"BECKE_GRID:no_of_pts_for_atom ... no atom_zeta scale factors")

      ! Basis function cutoff
      r_max = r_max_for_atom_(self,a)

      ! Radial grid and no of radial points
      p = period_of_(self,a)
      nr = self%radial_grid(p)%n_pts

      ! Scale factor
      s = ONE
      if (self%scale_atomic_grids) s = self%atom_zeta(self%atom(a)%atomic_number)

      ! Make the no. of points
      res = 0
      do i = 1,nr
         r = s*self%radial_grid(p)%pts(i)

         ! Eliminate later points in the list
         if (r>r_max) exit

         ! Set default lebedev grid
         call apply_pruning_scheme_(self,lebedev_grid,p,i,nr,r)
         res = res+lebedev_grid%n_pts
      end do
   end function

   PURE subroutine apply_pruning_scheme(self,lebedev_grid,p,i,nr,r,scheme)
   ! Set the angular pruning scheme for "lebedev_grid" for an atom in
   ! row "p", for a radial point "i" in a set of "nr" radial points
   ! arranged in increasing distance, at scaled radius "r" (bohr). The
   ! default grid is .lebedev_grid. "scheme" overrides the default scheme.
      type(BECKE_GRID_TYPE), IN :: self
      type(LEBEDEV_TYPE), allocatable, OUT :: lebedev_grid
      INT, IN :: p,i,nr
      REAL, IN :: r
      STR(len=*), optional, IN :: scheme
      STR(len=STR_SIZE) :: pruning_scheme
   ENSURE(allocated(self%lebedev_grid),"BECKE_GRID:apply_pruning_scheme ... no lebedev_grid")
      pruning_scheme = self%pruning_scheme
      if (present(scheme)) pruning_scheme = scheme
      lebedev_grid = self%lebedev_grid

      ! Reset lebedev grid for H atoms
      if (self%reduce_H_angular_grid AND p==1 AND NOT is_SG_kind_(self)) lebedev_grid = self%lebedev_H_grid
      select case (pruning_scheme)
      case ("none             ")
      case ("jayatilaka0      "); call apply_pruning_scheme_J0_(self,lebedev_grid,i,nr)
      case ("jayatilaka1      "); call apply_pruning_scheme_J1_(self,lebedev_grid,i,nr)
      ! "jayatilaka2" was REMOVED 2026-08-14: too rough. It dropped to Lebedev
      ! L3/L5/L5/L7 over the inner half of the radial range, and measured
      ! against g09 on H2O+ (cc-pVDZ, UKS slater, accuracy= best) it was
      ! -1.54e-05 where treutler_ahlrichs, jayatilaka0 and jayatilaka1 all
      ! landed within ~1.6e-06. See docs/TASK_DFT_STANDARDISATION.md section 6a.
      case ("treutler_ahlrichs")
         ! For the SG kinds "p" is the atomic number
         if (is_SG_kind_(self)) then; call apply_pruning_scheme_TA_(self,lebedev_grid,period(p),i,nr)
         else; call apply_pruning_scheme_TA_(self,lebedev_grid,p,i,nr)
         end if
      case ("adaptive         "); call apply_pruning_scheme_adaptive_(self,lebedev_grid,p,r)
      case ("sg-0","sg-1","sg-2","sg-3"); call apply_pruning_scheme_SG_(self,lebedev_grid,p,i)
      end select
   end subroutine

   PURE subroutine apply_pruning_scheme_SG(self,lebedev_grid,Z,i)
   ! Set "lebedev_grid" for radial shell "i", counted from the nucleus,
   ! of the element with atomic number "Z" in the standard grid .kind.
   ! An element with a published partition uses it. Otherwise: SG-0
   ! falls back to SG-1; SG-1 uses the five zones of Gill, Johnson and
   ! Pople to Ar and 194 points on every shell beyond; SG-2 and SG-3
   ! use 302 and 590 points on every shell.
      type(BECKE_GRID_TYPE), IN :: self
      type(LEBEDEV_TYPE), allocatable, INOUT :: lebedev_grid
      INT, IN :: Z,i
      VEC(INT,:), allocatable :: n_ang,n_shell
      INT :: k,last,n,nr
      STR(len=STR_SIZE) :: scheme
      REAL :: length,t
      VEC(REAL,4) :: zone
      call SG_partition(self%kind,Z,n_ang,n_shell)
      if (allocated(n_ang)) then
         last = 0
         do k = 1,size(n_ang)
            last = last+n_shell(k)
            if (i<=last) exit
         end do
         ENSURE(k<=size(n_ang),"BECKE_GRID:apply_pruning_scheme_SG ... radial shell beyond the SG partition")
         n = n_ang(min(k,size(n_ang)))
         call destroy_(n_shell); call destroy_(n_ang)
      else if (self%kind=="sg-2") then
         n = 302
      else if (self%kind=="sg-3") then
         n = 590
      else if (Z>18) then
         n = 194
      else

         ! SG-1: the zone boundaries are multiples of the atomic radius,
         ! so r/R = (i/(nr+1-i))^2 places the shell
         if (Z<=2) then; zone = [0.25d0,0.5d0,1.0d0,4.5d0]
         else if (Z<=10) then; zone = [0.1667d0,0.5d0,0.9d0,3.5d0]
         else; zone = [0.1d0,0.4d0,0.8d0,2.5d0]
         end if
         call SG_radial_scheme(self%kind,Z,scheme,nr,length)
         t = i
         t = t/(nr+1-i)
         t = t*t+TOL(10)  ! a shell on a boundary belongs to the outer zone
         if (t<zone(1)) then; n = 6
         else if (t<zone(2)) then; n = 38
         else if (t<zone(3)) then; n = 86
         else if (t<zone(4)) then; n = 194
         else; n = 86
         end if
      end if

      ! The 18-point rule of SG-0 is not a Lebedev grid
      if (n==18) then; call as0018_(lebedev_grid)
      else; call set_n_pts_(lebedev_grid,n)
      end if
   end subroutine

   PURE subroutine SG_radial_scheme(kind,Z,scheme,nr,length)
   ! The radial quadrature of element "Z" in the standard grid "kind":
   ! its "scheme" (multiexp, euler_maclaurin or de2), its number of
   ! points "nr", and its "length" parameter -- the atomic radius in
   ! bohr, or for de2 the scaling factor alpha.
   ! . SG-0 (Chien and Gill, J. Comput. Chem. 27, 730 (2006)): MultiExp
   !   for H to Cl without the rare gases; SG-1 for the rest.
   ! . SG-1 (Gill, Johnson and Pople, Chem. Phys. Lett. 209, 506
   !   (1993)): 50-point Euler-Maclaurin.
   ! . SG-2, SG-3 (Dasgupta and Herbert, J. Comput. Chem. 38, 869
   !   (2017)): 75- and 99-point DE2 for the same elements as SG-0;
   !   Euler-Maclaurin with as many points for the rest.
      STR(len=*), IN :: kind
      INT, IN :: Z
      STR(len=*), OUT :: scheme
      INT, OUT :: nr
      REAL, OUT :: length
      scheme = "euler_maclaurin"
      nr = 50
      length = Slater_radius(Z)
      select case (kind)

      case ("sg-0")
         if (SG0_radius(Z)>ZERO) then
            scheme = "multiexp"
            nr = 23
            if (Z>10) nr = 26
            length = SG0_radius(Z)
         end if

      case ("sg-2","sg-3")
         nr = 75
         if (kind=="sg-3") nr = 99
         if (SG_alpha(kind,Z)>ZERO) then
            scheme = "de2"
            length = SG_alpha(kind,Z)
         end if
      end select
   end subroutine

   PURE function SG0_radius(Z) result (res)
   ! The MultiExp radius (bohr) of element "Z" in the SG-0 grid (Chien
   ! and Gill 2006, Table 1); zero where SG-0 has no data.
      INT, IN :: Z
      REAL :: res
      VEC(REAL,18) :: rad
      rad = [1.30d0,ZERO,1.95d0,2.20d0,1.45d0,1.20d0,1.10d0,1.10d0,1.20d0,ZERO,2.30d0,2.20d0,2.10d0,1.30d0,1.30d0,1.10d0,1.45d0,ZERO]
      res = ZERO
      if (Z>=1 AND Z<=18) res = rad(Z)
   end function

   PURE function Slater_radius(Z) result (res)
   ! The atomic radius (bohr) of element "Z" of the SG-1 grid: where the
   ! radial density of the outermost s or p orbital peaks by Slater's
   ! rules, n*^2/Z_eff. To Ar these are the values of Gill, Johnson and
   ! Pople, Table 1; beyond, the same rule with the subshells filled in
   ! Madelung order. Slater gave no effective quantum number for the
   ! seventh shell; that of the sixth is used.
      INT, IN :: Z
      REAL :: res
      VEC(REAL,18) :: gill
      VEC(INT,18) :: pqn,cap
      VEC(INT,7) :: n_sp,n_all
      VEC(REAL,7) :: n_star
      INT :: left,k,e,v
      REAL :: sigma
      gill = [1.0000d0,0.5882d0,3.0769d0,2.0513d0,1.5385d0,1.2308d0,1.0256d0,0.8791d0,0.7692d0,0.6838d0,4.0909d0,3.1579d0,2.5714d0,2.1687d0,1.8750d0,1.6514d0,1.4754d0,1.3333d0]
      res = ZERO
      if (Z<1 OR Z>103) return
      if (Z<=18) then
         res = gill(Z)
         return
      end if

      ! The subshells in filling order: principal number and capacity
      pqn = [1,2,2,3,3,4,3,4,5,4,5,6,4,5,6,7,5,6]
      cap = [2,2,6,2,6,2,10,6,2,10,6,2,14,10,6,2,14,10]
      n_star = [ONE,TWO,THREE,3.7d0,FOUR,4.2d0,4.2d0]

      ! Electrons in s and p subshells, and in all subshells, by shell
      n_sp = 0
      n_all = 0
      left = Z
      do k = 1,18
         e = min(left,cap(k))
         left = left-e
         n_all(pqn(k)) = n_all(pqn(k))+e
         if (cap(k)<=6) n_sp(pqn(k)) = n_sp(pqn(k))+e
      end do

      ! The valence shell "v", and the screening of one of its electrons
      v = 1
      do k = 1,7
         if (n_sp(k)>0) v = k
      end do
      sigma = 0.35d0*(n_sp(v)-1)+0.85d0*n_all(v-1)+sum(n_all(1:v-2))
      res = n_star(v)*n_star(v)/(Z-sigma)
   end function

   PURE function SG_alpha(kind,Z) result (res)
   ! The DE2 radial scaling factor of element "Z" in the SG-2 or SG-3
   ! grid "kind" (Dasgupta and Herbert 2017, Table 1); zero where the
   ! grids have no data.
      STR(len=*), IN :: kind
      INT, IN :: Z
      REAL :: res
      VEC(REAL,17) :: a2,a3

      !        H     He    Li    Be    B     C     N     O     F     Ne    Na    Mg    Al    Si    P     S     Cl
      a2 = [2.6d0,ZERO,3.2d0,2.4d0,2.4d0,2.2d0,2.2d0,2.2d0,2.2d0,ZERO,3.2d0,2.4d0,2.5d0,2.3d0,2.5d0,2.5d0,2.5d0]
      a3 = [2.7d0,ZERO,3.0d0,2.4d0,2.4d0,2.4d0,2.4d0,2.6d0,2.1d0,ZERO,3.2d0,2.6d0,2.6d0,2.8d0,2.4d0,2.4d0,2.6d0]
      res = ZERO
      if (Z<1 OR Z>17) return
      if (kind=="sg-2") res = a2(Z)
      if (kind=="sg-3") res = a3(Z)
   end function

   PURE function SG_radius(Z) result (res)
   ! The radius (bohr) that sets the outer limit, ten times it, of the
   ! DE2 radial grid of element "Z": the Hartree-Fock mean radius of its
   ! outermost valence orbital (Mitani, Theor. Chem. Acc. 130, 645 (2011)).
      INT, IN :: Z
      REAL :: res
      VEC(REAL,17) :: rad

      !         H        He       Li         Be       B        C        N        O        F
      !         Ne          Na          Mg        Al        Si        P         S         Cl
      rad = [1.5d0,0.927272d0,3.873661d0,2.6494d0,2.2048d0,1.7145d0,1.4096d0,1.2322d0,1.084786d0,0.965273d0,4.208762d0,3.2529d0,3.4339d0,2.7521d0,2.3225d0,2.0607d0,1.842024d0]
      res = ZERO
      if (Z>=1 AND Z<=17) res = rad(Z)
   end function

   PURE subroutine SG_partition(kind,Z,n_ang,n_shell)
   ! The published angular partition of element "Z" in the standard grid
   ! "kind": "n_ang(k)" angular points on each of the next "n_shell(k)"
   ! radial shells, from the nucleus outward. From Chien and Gill 2006,
   ! Table 1, for SG-0 and Dasgupta and Herbert 2017, Table 1, for SG-2
   ! and SG-3. Left unallocated where there is none, as for all of SG-1.
      STR(len=*), IN :: kind
      INT, IN :: Z
      VEC(INT,:), allocatable, OUT :: n_ang,n_shell
      if (kind=="sg-0") then
         select case (Z)
         case (1,3)
            n_ang = [6,18,26,38,74,110,146,86,50,38,18]
            n_shell = [6,3,1,1,1,1,6,1,1,1,1]
         case (4)
            n_ang = [6,18,26,38,74,86,110,146,50,38,18,6]
            n_shell = [4,2,1,2,1,1,2,5,1,1,1,2]
         case (5)
            n_ang = [6,26,38,86,146,38,6]
            n_shell = [4,4,3,3,6,1,2]
         case (6)
            n_ang = [6,18,26,38,50,86,110,146,170,146,86,38,18]
            n_shell = [6,2,1,2,2,1,1,1,2,2,1,1,1]
         case (7)
            n_ang = [6,18,26,38,74,110,170,146,86,50]
            n_shell = [6,3,1,2,2,1,2,3,1,2]
         case (8)
            n_ang = [6,18,26,38,50,86,110,86,50,38,6]
            n_shell = [5,1,2,1,4,1,5,1,1,1,1]
         case (9)
            n_ang = [6,38,50,74,110,146,110,86,50,6]
            n_shell = [4,2,4,2,2,2,2,3,1,1]
         case (11)
            n_ang = [6,18,26,38,50,110,74,6]
            n_shell = [6,2,3,1,2,8,2,2]
         case (12)
            n_ang = [6,18,26,38,50,74,110,146,110,86,38,18,6]
            n_shell = [5,2,2,2,2,1,2,4,1,1,2,1,1]
         case (13)
            n_ang = [6,18,26,38,50,74,86,146,170,110,86,74,26,18,6]
            n_shell = [6,2,1,2,2,1,1,2,2,2,1,1,1,1,1]
         case (14,15)
            n_ang = [6,18,38,50,74,110,146,170,86,50,6]
            n_shell = [5,4,4,3,1,2,1,3,1,1,1]
         case (16)
            n_ang = [6,18,26,38,50,74,110,170,146,110,50,6]
            n_shell = [4,1,8,2,1,2,1,3,1,1,1,1]
         case (17)
            n_ang = [6,18,26,38,50,74,110,170,146,110,86,6]
            n_shell = [4,7,2,2,1,1,2,3,1,1,1,1]
         end select
      else if (kind=="sg-2") then
         select case (Z)
         case (1)
            n_ang = [6,110,302,86,26]
            n_shell = [35,12,16,7,5]
         case (3,4,11,12)
            n_ang = [6,110,302,86,50]
            n_shell = [35,12,17,7,4]
         case (5,6)
            n_ang = [6,110,302,146,26]
            n_shell = [35,12,17,7,4]
         case (7)
            n_ang = [6,110,302,86,26]
            n_shell = [35,12,17,7,4]
         case (8)
            n_ang = [6,110,302,146,50]
            n_shell = [30,14,18,8,5]
         case (9,17)
            n_ang = [6,110,302,110,50]
            n_shell = [26,16,19,8,6]
         case (13)
            n_ang = [6,110,302,146,86]
            n_shell = [32,15,17,7,4]
         case (14)
            n_ang = [6,110,302,146,50]
            n_shell = [32,15,17,7,4]
         case (15,16)
            n_ang = [6,110,302,146,38]
            n_shell = [30,14,17,7,7]
         end select
      else if (kind=="sg-3") then
         select case (Z)
         case (1)
            n_ang = [6,110,590,194,50]
            n_shell = [45,16,21,10,7]
         case (3,11)
            n_ang = [6,110,590,146,50]
            n_shell = [46,16,22,9,6]
         case (4,13)
            n_ang = [6,86,110,590,194,146,50]
            n_shell = [42,6,14,22,3,6,6]
         case (5,14)
            n_ang = [6,86,110,590,194,50]
            n_shell = [42,6,14,22,9,6]
         case (6)
            n_ang = [6,146,590,302,194,146,86]
            n_shell = [46,16,22,1,2,6,6]
         case (7)
            n_ang = [6,110,590,146,50]
            n_shell = [40,18,24,11,6]
         case (8)
            n_ang = [6,110,194,302,590,302,194,146,50]
            n_shell = [40,14,2,2,24,1,1,8,7]
         case (9,17)
            n_ang = [6,110,194,590,194,110,50]
            n_shell = [35,17,4,25,2,8,8]
         case (12)
            n_ang = [6,110,590,146,50]
            n_shell = [48,15,20,7,9]
         case (15,16)
            n_ang = [6,86,110,194,590,194,146,50]
            n_shell = [35,1,18,4,25,2,8,6]
         end select
      end if
   end subroutine

   PURE subroutine set_EM_radial_grid(radial_grid,nr,rad)
   ! Set "radial_grid" to the "nr"-point Euler-Maclaurin grid of Murray,
   ! Handy and Laming, Mol. Phys. 78, 997 (1993), with atomic radius "rad":
   !    r_i = rad i^2/(nr+1-i)^2 .
   ! The weight is for an integral over r; 4 pi r^2 is applied when the
   ! atom grid is made.
      type(QUADRATURE_TYPE), INOUT :: radial_grid
      INT, IN :: nr
      REAL, IN :: rad
      INT :: i
      REAL :: t,u
      call set_n_pts_(radial_grid,nr)
      call set_kind_(radial_grid,"open_extended_trapezoid")
      call set_quadrature_points_(radial_grid)  ! leaky here: allocates pts and wts
      do i = 1,nr
         t = i
         u = nr+1-i
         radial_grid%pts(i) = rad*t*t/(u*u)
         radial_grid%wts(i) = TWO*rad*(nr+1)*t/(u*u*u)
      end do
   end subroutine

   PURE subroutine set_MultiExp_radial_grid(radial_grid,nr,rad)
   ! Set "radial_grid" to the "nr"-point MultiExp grid of Gill and Chien,
   ! J. Comput. Chem. 24, 732 (2003), with atomic radius "rad":
   !    r_i = -rad ln x_i ,
   ! with x_i the points of the Gaussian quadrature on [0,1] for the
   ! weight function ln^2 x. Only the 23- and 26-point rules of SG-0 are
   ! stored. The weight is for an integral over r; 4 pi r^2 is applied
   ! when the atom grid is made.
      type(QUADRATURE_TYPE), INOUT :: radial_grid
      INT, IN :: nr
      REAL, IN :: rad
      VEC(REAL,26) :: x,a
      INT :: i,k
      REAL :: lx
   ENSURE(nr==23 OR nr==26,"BECKE_GRID:set_MultiExp_radial_grid ... only the 23- and 26-point MultiExp grids are stored")
      if (nr==23) then
         x(1:23) = [0.0015058924745840717d0,0.0099491128468611853d0,0.026212787562513862d0,0.050215014094683999d0,0.081660512821455929d0,0.12009269427713703d0,0.1649157971660048d0,0.21541139722601381d0,0.27075407329850232d0,0.33002759271630692d0,0.39224199352412352d0,0.45635157223202704d0,0.52127362052638902d0,0.58590767062942112d0,0.6491549639117992d0,0.7099378339431055d0,0.7672186862847633d0,0.82001826057974256d0,0.86743287798275914d0,0.90865042304171716d0,0.94296495577173701d0,0.96979055758591744d0,0.98868121412417917d0]
         a(1:23) = [0.19397997519811811d0,0.2623639636596476d0,0.26742812676397013d0,0.24866202472840894d0,0.21982849560935217d0,0.18749582392684797d0,0.15523427291530433d0,0.12506617470764026d0,0.098100418872550932d0,0.074860364849398577d0,0.055477247414684803d0,0.039817159733692048d0,0.027571576765107484d0,0.018325604666401505d0,0.011611019656801442d0,0.0069477499303844705d0,0.0038757714610574311d0,0.001978563506849731d0,0.00089889253062980984d0,0.0003475578897577002d0,0.00010572868056731378d0,2.15659539392613d-5,1.9205788879728201d-6]
      else
         x(1:26) = [0.0012118959531442052d0,0.0079508350834211655d0,0.020911970701014455d0,0.040062890303615981d0,0.065227756094948038d0,0.096124873966380717d0,0.13238114512422221d0,0.17354177117918734d0,0.21907886291165968d0,0.2684000493196846d0,0.32085745807052075d0,0.37575717144832967d0,0.43236914470191774d0,0.48993751506621983d0,0.547691197495453d0,0.60485464446878273d0,0.66065863649024736d0,0.71435096447486728d0,0.76520686385149628d0,0.81253906250257684d0,0.85570731112497829d0,0.8941272781391899d0,0.9272787239389384d0,0.9547129784471566d0,0.97606029628496512d0,0.99104255389177476d0]
         a(1:26) = [0.16590959790074127d0,0.2299690509487479d0,0.24044079379163951d0,0.22975862025927898d0,0.20930163443099709d0,0.18456387321242483d0,0.15860582735470473d0,0.13324603342450455d0,0.10957766175447101d0,0.088230076743000778d0,0.069518256800206744d0,0.053537150852983894d0,0.040226455838504916d0,0.029418139625084388d0,0.020873023457481862d0,0.014309796296644065d0,0.0094283196275203881d0,0.005928277523598967d0,0.0035237959667716519d0,0.0019544295871422983d0,0.00099280471150256854d0,0.00044916603235275513d0,0.00017307202304493877d0,5.25042596156813d-5,1.0687185195322948d-5,9.503918389326711d-7]
      end if
      call set_n_pts_(radial_grid,nr)
      call set_kind_(radial_grid,"open_extended_trapezoid")
      call set_quadrature_points_(radial_grid)  ! leaky here: allocates pts and wts

      ! The largest x is nearest the nucleus
      do i = 1,nr
         k = nr+1-i
         lx = log(x(k))
         radial_grid%pts(i) = -rad*lx
         radial_grid%wts(i) = rad*a(k)/(x(k)*lx*lx)
      end do
   end subroutine

   PURE subroutine set_DE2_radial_grid(radial_grid,nr,alpha,R)
   ! Set "radial_grid" to the "nr"-point double-exponential DE2 grid of
   ! Mitani, Theor. Chem. Acc. 130, 645 (2011), with scaling factor "alpha":
   !    r_i = exp(alpha x_i - exp(-x_i)),   x_i equally spaced,
   ! from r = 1e-7 to r = 10 R bohr. The weight is for an integral over r,
   ! dr/dx times the spacing; 4 pi r^2 is applied when the atom grid is made.
      type(QUADRATURE_TYPE), INOUT :: radial_grid
      INT, IN :: nr
      REAL, IN :: alpha,R
      REAL :: x_lo,x_hi,h,x,e
      INT :: i
      x_lo = DE2_x_for(log(TOL(7)),alpha)
      x_hi = DE2_x_for(log(TEN*R),alpha)
      h = (x_hi-x_lo)/(nr-1)
      call set_n_pts_(radial_grid,nr)
      call set_kind_(radial_grid,"open_extended_trapezoid")
      call set_quadrature_points_(radial_grid)  ! leaky here: allocates pts and wts
      do i = 1,nr
         x = x_lo+(i-1)*h
         e = exp(-x)
         radial_grid%pts(i) = exp(alpha*x-e)
         radial_grid%wts(i) = h*(alpha+e)*radial_grid%pts(i)
      end do
   end subroutine

   PURE function DE2_x_for(ln_r,alpha) result (res)
   ! The x for which alpha x - exp(-x) = "ln_r", by bisection; the left
   ! side rises steadily with x.
      REAL, IN :: ln_r,alpha
      REAL :: res
      REAL :: lo,hi,mid
      INT :: i
      lo = -TEN
      hi = 50d0
      do i = 1,200
         mid = HALF*(lo+hi)
         if (alpha*mid-exp(-mid)<ln_r) then; lo = mid
         else; hi = mid
         end if
      end do
      res = HALF*(lo+hi)
   end function

   PURE subroutine apply_pruning_scheme_adaptive(self,lebedev_grid,p,r)
   ! Set the Lebedev order of "lebedev_grid" from the scaled radius "r"
   ! (bohr) of a radial shell of an atom in row "p":
   !
   !    r <  0.2  0.7  1.0  1.4  4.5  6.5  7.5  beyond
   !    L    5    11   17   23   Lb   29   17   5
   !
   ! Lb is .l_bonding_angular_grid, set by the accuracy, and caps every
   ! zone. For H and He (with .reduce_H_angular_grid) Lb is
   ! .l_H_angular_grid and the 4.5-6.5 zone is L17, the 6.5-7.5 zone L5.
   ! The zones are calibrated on per-shell errors for water and
   ! karrikinolide; see docs/TASK_DFT_STANDARDISATION.md section 6c.
      type(BECKE_GRID_TYPE), IN :: self
      type(LEBEDEV_TYPE), allocatable, INOUT :: lebedev_grid
      INT, IN :: p
      REAL, IN :: r
      INT :: L,Lb
      BIN :: H_like
      H_like = p==1 AND self%reduce_H_angular_grid
      Lb = self%l_bonding_angular_grid
      if (H_like) Lb = self%l_H_angular_grid
      if (r<0.2d0) then; L = 5
      else if (r<0.7d0) then; L = 11
      else if (r<1.0d0) then; L = 17
      else if (r<1.4d0) then; L = 23
      else if (r<4.5d0) then; L = Lb
      else if (H_like) then
         L = 5
         if (r<6.5d0) L = 17
      else
         L = 5
         if (r<7.5d0) L = 17
         if (r<6.5d0) L = 29
      end if
      L = min(L,Lb)
      call destroy_(lebedev_grid)
      call create_(lebedev_grid)
      call set_l_(lebedev_grid,L)
   end subroutine

   PURE subroutine apply_pruning_scheme_J0(self,lebedev_grid,i,nr)
   ! Set Jayatilaka's angular pruning scheme for "lebedev_grid" given a radial
   ! point "i" out of a set of "nr" radial points arranged in increasing order.
      type(BECKE_GRID_TYPE), IN :: self
      type(LEBEDEV_TYPE), allocatable, INOUT :: lebedev_grid
      INT, IN :: i,nr
      INT :: n6,n3,n5,n2
      n6 = nr/6
      n3 = nr/3
      n5 = 5*nr/12
      n2 = nr/2
      if (i<=n6) then; lebedev_grid = self%lebedev_L3_grid
      else if (n6<i AND i<=n3) then; lebedev_grid = self%lebedev_L5_grid
      else if (n3<i AND i<=n5) then; lebedev_grid = self%lebedev_L9_grid
      else if (n5<i AND i<=n2) then; lebedev_grid = self%lebedev_L11_grid
      end if
   end subroutine

   PURE subroutine apply_pruning_scheme_J1(self,lebedev_grid,i,nr)
   ! Set Jayatilaka's angular pruning scheme for "lebedev_grid" given a radial
   ! point "i" out of a set of "nr" radial points arranged in increasing order.
      type(BECKE_GRID_TYPE), IN :: self
      type(LEBEDEV_TYPE), allocatable, INOUT :: lebedev_grid
      INT, IN :: i,nr
      INT :: n2,n4,n5,n6
      n2 = 2*nr/12
      n4 = 4*nr/12
      n5 = 5*nr/12
      n6 = 6*nr/12
      if (i<=n2) then; lebedev_grid = self%lebedev_L3_grid
      else if (n2<i AND i<=n4) then; lebedev_grid = self%lebedev_L5_grid
      else if (n4<i AND i<=n5) then; lebedev_grid = self%lebedev_L7_grid
      else if (n5<i AND i<=n6) then; lebedev_grid = self%lebedev_L9_grid
      end if
   end subroutine

   PURE subroutine apply_pruning_scheme_TA(self,lebedev_grid,p,i,nr)
   ! Set Truetler-Ahlrichs angular pruning scheme for "lebedev_grid"
   ! given a radial point "i" out of a set of "nr" radial points
   ! arranged in increasing order, for an atom in period "p". From
   ! period 6 on the innermost third has order 7, not 5: a product of
   ! two f functions, of the filled 4f shell, has degree 6.
      type(BECKE_GRID_TYPE), IN :: self
      type(LEBEDEV_TYPE), allocatable, INOUT :: lebedev_grid
      INT, IN :: p,i,nr
      INT :: n3,n2
      n3 = nr/3
      n2 = nr/2
      if (i<=n3) then
         if (p>=6) then; lebedev_grid = self%lebedev_L7_grid
         else; lebedev_grid = self%lebedev_L5_grid
         end if
      else if (n3<i AND i<=n2) then
         lebedev_grid = self%lebedev_L11_grid
      end if
   end subroutine


!  ===========================================
!  No. of points (after scaled grids are made)
!  ===========================================

   PURE function no_of_points_0(self) result (res)
   ! Calculate the total number of points over all atoms stored in the
   ! Becke grid. This number of points does *not* take into account
   ! grid compression but it *does* take into account each atom's
   ! lowest exponent. We can't know the number of compressed points
   ! before the partition weights are made!
      type(BECKE_GRID_TYPE), IN :: self
      INT :: res
      INT :: a,u
   ENSURE(self%finalized,"BECKE_GRID:no_of_points_0 ... not finalized; use set_grid_data")
   ENSURE(allocated(self%unique_atom_for),"BECKE_GRID:no_of_points_0 ... no unique_atom_for array")
   ENSURE(allocated(self%atom),"BECKE_GRID:no_of_points_0 ... no atom list")
   ENSURE(allocated(self%atom_wts),"BECKE_GRID:no_of_points_0 ... no atom_wts array")
      res = 0
      do a = 1,size(self%atom)
         u = self%unique_atom_for(a)
         res = res+size(self%atom_wts(u)%element)
      end do
   end function

   PURE function no_of_points_1(self,atom) result (res)
   ! Return the number of points in the DFT integration grid required
   ! to integrate "atom". This number of points does *not* take into
   ! account grid compression, but it *does* take into account the
   ! atoms lowest exponent.
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: atom
      INT :: res
      INT :: u
   ENSURE(self%finalized,"BECKE_GRID:no_of_points_1 ... not finalized; use set_grid_data")
   ENSURE(allocated(self%unique_atom_for),"BECKE_GRID:no_of_points_1 ... no unique_atom_for array")
   ENSURE(allocated(self%atom_wts),"BECKE_GRID:no_of_points_1 ... no atom_wts array")
      u = self%unique_atom_for(atom)
      res = size(self%atom_wts(u)%element)
   end function

   PURE function no_of_points_2(self,atoms) result (res)
   ! Return the number of points in the DFT integration grid required to
   ! integrate those atoms whose indices appear in "atoms". This number of
   ! points does *not* take into account grid compression, but it *does* take
   ! into account the atoms lowest exponent.
      type(BECKE_GRID_TYPE), IN :: self
      VEC(INT,:), IN :: atoms
      INT :: res
      INT :: a,u
   ENSURE(self%finalized,"BECKE_GRID:no_of_points_2 ... not finalized; use set_grid_data")
   ENSURE(allocated(self%unique_atom_for),"BECKE_GRID:no_of_points_2 ... no unique_atom_for array")
   ENSURE(allocated(self%atom_wts),"BECKE_GRID:no_of_points_2 ... no atom_wts array")
      res = 0
      do a = 1,size(atoms)
         u = self%unique_atom_for(atoms(a))
         res = res+size(self%atom_wts(u)%element)
      end do
   end function

   PURE function max_no_of_pts_per_atom(self) result (res)
   ! Return the maximum number of points in the DFT integration grid
   ! required to integrate any one atom. This number of points does
   ! *not* take into account grid compression, but it *does* take into
   ! account the atoms lowest exponent.
      type(BECKE_GRID_TYPE), IN :: self
      INT :: res
      INT :: a,u
   ENSURE(self%finalized,"BECKE_GRID:max_no_of_pts_per_atom ... not finalized; use set_grid_data")
   ENSURE(allocated(self%atom),"BECKE_GRID:max_no_of_pts_per_atom ... no atom array")
   ENSURE(allocated(self%unique_atom_for),"BECKE_GRID:max_no_of_pts_per_atom ... no unique_atom_for array")
   ENSURE(allocated(self%atom_wts),"BECKE_GRID:max_no_of_pts_per_atom ... no atom_wts array")
      res = 0
      do a = 1,size(self%atom)
         u = self%unique_atom_for(a)
         res = max(res,size(self%atom_wts(u)%element))
      end do
   end function

   PURE function r_max_for_atom(self,a) result (r_max)
   ! Calculate the distance for atom "a" where the basis functions are all
   ! negligible in value.
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: a
      REAL :: r_max
      r_max = r_max_(self%atom(a),self%basis_fn_cutoff)
   end function

   PURE function r_max_SS_for_atom(self,a) result (r_max)
   ! Distance from atom "a" *below which* Stratmann-Scuseria weight is one.
   ! See equation (15), CPL 257 p. 213-23 (1996).
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: a
      REAL :: r_max
      REAL :: a1,a2
   ENSURE(allocated(self%atom_min_distance),"BECKE_GRID:r_max_SS_for_atom ... no atom_min_distance array")
      a1 = ONE-(self%stratmann_scuseria_A)
      a2 = HALF*a1
      r_max = a2*self%atom_min_distance(a)
   end function

   PURE function n1_SS_for_atom(self,a) result (res)
   ! Return: the number of points in the grid for atom "a" which have
   ! unit weight from the Stratmann-Scuseria scheme. Or are too tiny.
      type(BECKE_GRID_TYPE), IN :: self
      INT, IN :: a
      INT :: res
      type(LEBEDEV_TYPE), allocatable :: lebedev_grid
      INT :: p,nr,i
      REAL :: r_max,r_max_SS,s,r
   ENSURE(allocated(self%radial_grid),"BECKE_GRID:n1_SS_for_atom ... no radial_grid")
   ENSURE(allocated(self%lebedev_grid),"BECKE_GRID:n1_SS_for_atom ... no lebedev_grid")
   ENSURE(allocated(self%atom),"BECKE_GRID:n1_SS_for_atom ... no atom list")
   ENSURE(allocated(self%atom_zeta),"BECKE_GRID:n1_SS_for_atom ... no atom_zeta scale factors")

      ! Basis function cutoff
      r_max = r_max_for_atom_(self,a)

      ! SS cutoff
      r_max_SS = r_max_SS_for_atom_(self,a)

      ! Radial grid and no of radial points
      p = period_of_(self,a)
      nr = self%radial_grid(p)%n_pts

      ! Scale factor
      s = ONE
      if (self%scale_atomic_grids) s = self%atom_zeta(self%atom(a)%atomic_number)

      ! Make the no of points
      res = 0
      do i = 1,nr
         r = s*self%radial_grid(p)%pts(i)
         if (r>r_max) exit  ! Too tiny
         if (r>r_max_SS) exit  ! Not unit weight

         ! Set lebedev grid
         call apply_pruning_scheme_(self,lebedev_grid,p,i,nr,r)
         res = res+lebedev_grid%n_pts
      end do
   end function


!  ===========
!  Prune grids
!  ===========

   PURE subroutine prune_grid_0(self,pts,wts)
   ! Prune an integration grid ("pts","wts") by removing all points
   ! whose weight is exactly zero (killed by the partition scheme).
   ! Never threshold a weight against .basis_fn_cutoff: a weight is a
   ! volume element, and doing so discarded the innermost radial shells
   ! of every heavy atom. See docs/TASK_DFT_STANDARDISATION.md section 6b.
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, INOUT :: pts
      VEC(REAL,:), allocatable, INOUT :: wts
      INT :: i,k

      ! Copy only significant points
      k = 0
      do i = 1,size(pts,1)
         if (wts(i)==ZERO) cycle
         k = k+1
         pts(k,1) = pts(i,1)
         pts(k,2) = pts(i,2)
         pts(k,3) = pts(i,3)
         wts(k) = wts(i)
      end do

      ! Shrink the grid
      call shrink_(pts,k,3)
      call shrink_(wts,k)
   end subroutine

   PURE subroutine prune_grid_1(self,pt,wt,f0)
   ! Prune an integration grid (pt,wt) by removing all points
   ! whose weight is exactly zero (see prune_grid(pts,wts)).
   ! Also return the corresponding basis function grid "f0".
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, INOUT :: pt
      VEC(REAL,:), allocatable, INOUT :: wt
      MAT(REAL,:,:), allocatable, INOUT :: f0
      VEC(INT,:), allocatable :: map
      INT :: n_pt,n_bf,i,k,f

      ! Map array
      n_pt = size(wt)
      call create_(map,n_pt)

      ! Copy only significant points
      k = 0
      do i = 1,n_pt
         if (wt(i)==ZERO) cycle
         k = k+1
         map(k) = i
         pt(k,1) = pt(i,1)
         pt(k,2) = pt(i,2)
         pt(k,3) = pt(i,3)
         wt(k) = wt(i)
      end do

      ! New no. of points
      n_pt = k

      ! Copy the bf grid
      n_bf = size(f0,2)
      do f = 1,n_bf
         do k = 1,n_pt
            i = map(k)
            f0(k,f) = f0(i,f)
         end do
      end do

      ! Shrink the grid
      call shrink_(pt,n_pt,3)
      call shrink_(wt,n_pt)
      call shrink_(f0,n_pt,n_bf)

      ! Clean
      call destroy_(map)
   end subroutine

   PURE subroutine prune_grid_2(self,pt,wt,f0,gx,gy,gz)
   ! Prune an integration grid (pt,wt) by removing all points
   ! whose weight is exactly zero (see prune_grid(pts,wts)).
   ! Also return the corresponding basis function grid "f0" and its
   ! derivatives "gx", "gy", "gz".
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, INOUT :: pt
      VEC(REAL,:), allocatable, INOUT :: wt
      MAT(REAL,:,:), allocatable, INOUT :: f0
      MAT(REAL,:,:), allocatable, INOUT :: gx,gy,gz
      VEC(INT,:), allocatable :: map
      INT :: n_pt,n_bf,i,k,f

      ! Map array
      n_pt = size(wt)
      call create_(map,n_pt)

      ! Copy only significant points
      k = 0
      do i = 1,n_pt
         if (wt(i)==ZERO) cycle
         k = k+1
         map(k) = i
         pt(k,1) = pt(i,1)
         pt(k,2) = pt(i,2)
         pt(k,3) = pt(i,3)
         wt(k) = wt(i)
      end do

      ! New no. of points
      n_pt = k

      ! Copy the bf grid
      n_bf = size(f0,2)
      do f = 1,n_bf
         do k = 1,n_pt
            i = map(k)
            f0(k,f) = f0(i,f)
            gx(k,f) = gx(i,f)
            gy(k,f) = gy(i,f)
            gz(k,f) = gz(i,f)
         end do
      end do

      ! Shrink the grid
      call shrink_(pt,n_pt,3)
      call shrink_(wt,n_pt)
      call shrink_(f0,n_pt,n_bf)
      call shrink_(gx,n_pt,n_bf)
      call shrink_(gy,n_pt,n_bf)
      call shrink_(gz,n_pt,n_bf)

      ! Clean
      call destroy_(map)
   end subroutine

   PURE subroutine make_wt_is_0(self,pt,atoms)
   ! Prune a two-atom grid of points which are too small when the
   ! basis functions on each atom are multiplied together. These small
   ! points are given zero weights.
      type(BECKE_GRID_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: pt
      VEC(INT,:), IN :: atoms
      VEC(REAL,3) :: ra,rb,p,da,db
      REAL :: a,b,cutoff,ra2,rb2
      INT :: i
   ENSURE(self%finalized,"BECKE_GRID:make_wt_is_0 ... radial and angular grids are unmade; use set_grid_data")
   ENSURE(allocated(self%atom),"BECKE_GRID:make_wt_is_0 ... no atom list")
   ENSURE(allocated(self%wt_is_0),"BECKE_GRID:make_wt_is_0 ... the wt_is_0 array does not exist!")
   ENSURE(NOT has_repetitions_(atoms),"BECKE_GRID:make_wt_is_0 ... the atoms array has repetitions!")
      if (size(atoms)/=2) return
      a = minimum_basis_exponent_(self%atom(atoms(1)))
      b = minimum_basis_exponent_(self%atom(atoms(2)))
      ra = self%atom(atoms(1))%position
      rb = self%atom(atoms(2))%position
      cutoff = -log(self%basis_fn_pair_cutoff)
      do i = 1,size(pt,1)
         if (self%wt_is_0(i)) cycle
         p = pt(i,:)
         da = ra-p
         db = rb-p
         ra2 = dot_product(da,da)
         rb2 = dot_product(db,db)
         if (a*ra2+b*rb2>cutoff) self%wt_is_0(i) = TRUE
      end do
   end subroutine


!  =======================
!  Unnormalized atom grids
!  =======================

   PURE subroutine make_atom_grid_0(self,pts,wts,a)
   ! Make the *unnormalised* integration grid ("pts","wts") *only* for
   ! atom "a". Grids must have been made by calling .set_grid_data.
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pts
      VEC(REAL,:), allocatable, OUT :: wts
      INT, IN :: a
      INT :: u,n_pts,n
      REAL :: a1,a2,a3
   ENSURE(self%finalized,"BECKE_GRID:make_atom_grid_0 ... radial and angular grids are unmade; use set_grid_data")
   ENSURE(allocated(self%atom_pts),"BECKE_GRID:make_atom_grid_0 ... no atom list")

      ! Unique atom for atom "a"
      u = self%unique_atom_for(a)

      ! No. of points in the grid
      n_pts = size(self%atom_wts(u)%element)

      ! Create pts and wts - LEAKY
      call create_(pts,n_pts,3)
      call create_(wts,n_pts)

      ! Set
      pts = self%atom_pts(u)%element
      wts = self%atom_wts(u)%element

      ! Displace pt
      a1 = self%atom(a)%position(1)
      a2 = self%atom(a)%position(2)
      a3 = self%atom(a)%position(3)
      do n = 1,n_pts
         pts(n,1) = pts(n,1)+a1
         pts(n,2) = pts(n,2)+a2
         pts(n,3) = pts(n,3)+a3
      end do
   end subroutine

   PURE subroutine make_atom_grid_1(self,pts,wts,f0,a,index)
   ! Make the *unnormalised* integration grid ("pts","wts") *only* for
   ! atom "a". Grids must have been made by calling .set_grid_data.
   ! . Also return the basis function grid "f0" -- or, if "index" is
   !   present and TRUE, a one-column "f0" holding each point's row in
   !   the unique-atom grid, so a caller that prunes the grid can still
   !   find the cached basis values with get_atom_bf_grid.
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pts
      VEC(REAL,:), allocatable, OUT :: wts
      MAT(REAL,:,:), allocatable, OUT :: f0
      INT, IN :: a
      BIN, optional, IN :: index
      INT :: u,n_pts,n_bf,n,f,k
      REAL :: a1,a2,a3
   ENSURE(self%finalized,"BECKE_GRID:make_atom_grid_1 ... radial and angular grids are unmade; use set_grid_data")
   ENSURE(allocated(self%atom),"BECKE_GRID:make_atom_grid_1 ... no atom list")

      ! Unique atom for atom "a"
      u = self%unique_atom_for(a)

      ! No. of points in the grid
      n_pts = size(self%atom_wts(u)%element)

      ! Create
      call create_(pts,n_pts,3)
      call create_(wts,n_pts)

      ! Set
      pts = self%atom_pts(u)%element  ! ... leaky !
      wts = self%atom_wts(u)%element  ! ... leaky !

      ! Displace pt
      a1 = self%atom(a)%position(1)
      a2 = self%atom(a)%position(2)
      a3 = self%atom(a)%position(3)
      do n = 1,n_pts
         pts(n,1) = pts(n,1)+a1
         pts(n,2) = pts(n,2)+a2
         pts(n,3) = pts(n,3)+a3
      end do

      ! Row indices instead of basis values?
      if (present(index)) then
         if (index) then
            call create_(f0,n_pts,1)
            do k = 1,n_pts
               f0(k,1) = k
            end do
            return
         end if
      end if

      ! No. of basis functions
      n_bf = self%atom(a)%basis%n_bf

      ! Copy the bf grid
      call create_(f0,n_pts,n_bf)
      do f = 1,n_bf
         do k = 1,n_pts
            f0(k,f) = self%atom_grid0(u)%element(k,f)
         end do
      end do
   end subroutine

   PURE subroutine get_atom_bf_grid_0(self,f0,a,index)
   ! Return the basis functions of atom "a" on the rows "index" of its
   ! unique-atom grid, from the cache made by set_atom_density_grids.
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: f0
      INT, IN :: a
      VEC(INT,:), IN :: index
      INT :: u,n_pts,n_bf,f,k
      u = self%unique_atom_for(a)
      n_pts = size(index)
      n_bf = self%atom(a)%basis%n_bf
      call create_(f0,n_pts,n_bf)
      do f = 1,n_bf
         do k = 1,n_pts
            f0(k,f) = self%atom_grid0(u)%element(index(k),f)
         end do
      end do
   end subroutine

   PURE subroutine get_atom_bf_grid_1(self,f0,gx,gy,gz,a,index)
   ! Return the basis functions of atom "a" and their gradients on the
   ! rows "index" of its unique-atom grid, from the caches.
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: f0
      MAT(REAL,:,:), allocatable, OUT :: gx,gy,gz
      INT, IN :: a
      VEC(INT,:), IN :: index
      INT :: u,n_pts,n_bf,f,k,i
      u = self%unique_atom_for(a)
      n_pts = size(index)
      n_bf = self%atom(a)%basis%n_bf
      call create_(f0,n_pts,n_bf)
      call create_(gx,n_pts,n_bf)
      call create_(gy,n_pts,n_bf)
      call create_(gz,n_pts,n_bf)
      do f = 1,n_bf
         do k = 1,n_pts
            i = index(k)
            f0(k,f) = self%atom_grid0(u)%element(i,f)
            gx(k,f) = self%atom_grid1(u)%element(i,f,1)
            gy(k,f) = self%atom_grid1(u)%element(i,f,2)
            gz(k,f) = self%atom_grid1(u)%element(i,f,3)
         end do
      end do
   end subroutine

   PURE subroutine make_atom_grid_2(self,pts,wts,f0,gx,gy,gz,a)
   ! Make the *unnormalised* integration grid ("pts","wts") *only* for
   ! atom "a". Grids must have been made by calling .set_grid_data.
   ! . Also return the basis function grid "f0".
   ! . Also return basis function gradients "gx", "gy", "gz".
   ! Atom grids must have been made by calling .set_grid_data
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pts
      VEC(REAL,:), allocatable, OUT :: wts
      MAT(REAL,:,:), allocatable, OUT :: f0
      MAT(REAL,:,:), allocatable, OUT :: gx,gy,gz
      INT, IN :: a
      INT :: u,n_pts,n_bf,n,f,k
      REAL :: a1,a2,a3
   ENSURE(self%finalized,"BECKE_GRID:make_atom_grid_2 ... radial and angular grids are unmade; use set_grid_data")
   ENSURE(allocated(self%atom),"BECKE_GRID:make_atom_grid_2 ... no atom list")

      ! Unique atom for atom "a"
      u = self%unique_atom_for(a)

      ! No. of points in the grid
      n_pts = size(self%atom_wts(u)%element)

      ! Create
      call create_(pts,n_pts,3)
      call create_(wts,n_pts)

      ! Set
      pts = self%atom_pts(u)%element  ! ... leaky !
      wts = self%atom_wts(u)%element  ! ... leaky !

      ! Displace pt
      a1 = self%atom(a)%position(1)
      a2 = self%atom(a)%position(2)
      a3 = self%atom(a)%position(3)
      do n = 1,n_pts
         pts(n,1) = pts(n,1)+a1
         pts(n,2) = pts(n,2)+a2
         pts(n,3) = pts(n,3)+a3
      end do

      ! No. of basis functions
      n_bf = self%atom(a)%basis%n_bf

      ! Copy the bf grid
      call create_(f0,n_pts,n_bf)
      call create_(gx,n_pts,n_bf)
      call create_(gy,n_pts,n_bf)
      call create_(gz,n_pts,n_bf)
      u = self%unique_atom_for(a)
      do f = 1,n_bf
         do k = 1,n_pts
            f0(k,f) = self%atom_grid0(u)%element(k,f)
            gx(k,f) = self%atom_grid1(u)%element(k,f,1)
            gy(k,f) = self%atom_grid1(u)%element(k,f,2)
            gz(k,f) = self%atom_grid1(u)%element(k,f,3)
         end do
      end do
   end subroutine


!  =====================
!  Normalized atom grids
!  =====================

   PURE subroutine make_Becke_atom_grid(self,pts,wts,a)
   ! Make the *normalised* Becke integration grid ("pts","wts")
   ! *only* for atom "a". Atom size adjustments are made.
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pts
      VEC(REAL,:), allocatable, OUT :: wts
      INT, IN :: a
      VEC(REAL,:), allocatable :: Pa,Pi,Ps
      VEC(REAL,3) :: ri,rj,rg
      REAL :: bi,bj,chi
      REAL :: dij,dgi,dgj
      REAL :: mu,s
      INT :: n_atoms,i,j
      INT :: n_pts,g
   ENSURE(self%finalized,"BECKE_GRID:make_Becke_atom_grid ... call the set_grid_data routine")
   ENSURE(allocated(self%atom),"BECKE_GRID:make_Becke_atom_grid ... no atom list")
   ENSURE(is_in_range_(a,[1,size(self%atom)]),"BECKE_GRID:make_Becke_atom_grid ... `a' argument out of range")

      ! Unnormalized grid for atom "a"
      call make_atom_grid_(self,pts,wts,a)

      ! Sizes
      n_atoms = size(self%atom)
      n_pts = size(pts,1)

      ! Cell functions
      call create_(Ps,n_pts)  ! Sum_i P_i
      call create_(Pa,n_pts)  ! a-th cell fn
      call create_(Pi,n_pts)  ! i-th cell fn
      Ps = ZERO  ! <--- NOTE

      ! Sum cell fns. Pi
      do i = 1,n_atoms
         bi = bragg_slater_radius(self%atom(i)%atomic_number)
         ri = self%atom(i)%position
         Pi = ONE  ! <--- NOTE

         ! Product over step fns.
         ! Pi = Prod_j s(mu_ij)
         do j = 1,n_atoms
            if (i==j) cycle
            bj = bragg_slater_radius(self%atom(j)%atomic_number)
            rj = self%atom(j)%position
            dij = ONE/norm2(ri-rj)

            ! Becke size adjustment
            chi = bi/bj  ! ratio
            chi = (chi-ONE)/(chi+ONE)
            chi = chi/(chi*chi-ONE)  ! a
            chi = min(chi,HALF)  ! Becke (A5)
            chi = max(chi,-HALF)

            ! Grid points "g"
            do g = 1,n_pts
               rg = pts(g,:)
               dgi = norm2(ri-rg)
               dgj = norm2(rj-rg)
               mu = (dgi-dgj)*dij
               mu = mu+chi*(ONE-mu*mu)  ! nu
               mu = 1.5*mu-0.5*mu*mu*mu
               mu = 1.5*mu-0.5*mu*mu*mu
               mu = 1.5*mu-0.5*mu*mu*mu
               s = 0.5*(ONE-mu)  ! Eq. (8)
               Pi(g) = Pi(g)*s  ! Eq. (9)
            end do
         end do

         ! Add & store
         Ps = Ps+Pi
         if (i==a) Pa = Pi
      end do

      ! Normalize/modify wts
      wts = wts*Pa/Ps

      ! Clean
      call destroy_(Pi)
      call destroy_(Pa)
      call destroy_(Ps)
   end subroutine

   PURE subroutine make_SS1_atom_grid(self,pts,wts,a)
   ! Make the *normalised* Stratmann-Scuseria integration grid
   ! ("pts","wts") *only* for atom "a".
   ! NOTE: Actually it's the Stratmann-Scuseria grid with a=1.
   ! NOTE: Becke atom size adjustment is made to the step function.
   ! See Eqs. (11)-(14) in Stratmann et al (1996) CPL 257 p. 213.
   ! So that means there is no grid pruning or any fancy stuff.
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pts
      VEC(REAL,:), allocatable, OUT :: wts
      INT, IN :: a
      MAT(REAL,:,:), allocatable :: dg
      VEC(REAL,:), allocatable :: Pa,Pi,Ps
      VEC(REAL,3) :: ri,rj,rg
      REAL :: bi,bj,chi
      REAL :: mu,mu2,mu3
      REAL :: mu5,mu7,dij
      REAL :: c5,c21,c35,s
      INT :: n_atoms,i,j
      INT :: n_pts,g
   ENSURE(self%finalized,"BECKE_GRID:make_SS1_atom_grid ... call the set_grid_data routine")
   ENSURE(size(wts)==size(pts,1),"BECKE_GRID:make_SS1_atom_grid ... inconsistent dim1, wts and pts")
   ENSURE(size(pts,2)==3,"BECKE_GRID:make_SS1_atom_grid ... wrong dim2, pts")
   ENSURE(allocated(self%atom),"BECKE_GRID:make_SS1_atom_grid ... no atom list")
   ENSURE(is_in_range_(a,[1,size(self%atom)]),"BECKE_GRID:make_SS1_atom_grid ... `a' argument out of range")

      ! Unnormalized grid for atom "a"
      call make_atom_grid_(self,pts,wts,a)

      ! Sizes
      n_atoms = size(self%atom)
      n_pts = size(pts,1)

      ! Distances to grid points
      ! Does this speed things up?
      call create_(dg,n_pts,n_atoms)
      do i = 1,n_atoms
         ri = self%atom(i)%position
         do g = 1,n_pts
            rg = pts(g,:)
            dg(g,i) = norm2(rg-ri)
         end do
      end do

      ! Cell functions
      call create_(Ps,n_pts)  ! Sum_i P_i
      call create_(Pa,n_pts)  ! a-th cell fn
      call create_(Pi,n_pts)  ! i-th cell fn
      Ps = ZERO  ! <--- NOTE

      ! Stratmann-Scuseria eq. (14)
      c5 = FIVE
      c21 = SEVEN*THREE
      c35 = SEVEN*FIVE

      ! Sum cell fns. Pi
      do i = 1,n_atoms
         bi = bragg_slater_radius(self%atom(i)%atomic_number)
         ri = self%atom(i)%position
         Pi = ONE  ! <--- NOTE

         ! Product over step fns.
         ! Pi = Prod_j s(mu_ij)
         do j = 1,n_atoms
            if (i==j) cycle
            bj = bragg_slater_radius(self%atom(j)%atomic_number)
            rj = self%atom(j)%position
            dij = ONE/norm2(ri-rj)

               ! Becke size adjustment
            chi = bi/bj  ! ratio
            chi = (chi-ONE)/(chi+ONE)
            chi = chi/(chi*chi-ONE)  ! a
            chi = min(chi,HALF)  ! Becke (A5)
            chi = max(chi,-HALF)

               ! Grid points "g"
            do g = 1,n_pts
               mu = (dg(g,i)-dg(g,j))*dij  ! Speeded up?
               mu = mu+chi*(ONE-mu*mu)  ! nu
               mu2 = mu*mu
               mu3 = mu2*mu
               mu5 = mu3*mu2
               mu7 = mu5*mu2  ! Eq. (14)
               s = SIXTEENTH*(c35*(mu-mu3)+c21*mu5-c5*mu7)
               s = HALF*(ONE-s)  ! Eq. (8)
               Pi(g) = Pi(g)*s  ! Eq. (9)
            end do
         end do

         ! Add & store
         Ps = Ps+Pi
         if (i==a) Pa = Pi
      end do

      ! Normalize & modify grid wts
      do g = 1,n_pts
         wts(g) = wts(g)*Pa(g)/Ps(g)
      end do

      ! Clean
      call destroy_(Pi)
      call destroy_(Pa)
      call destroy_(Ps)
      call destroy_(dg)
   end subroutine

   PURE subroutine make_Salvador_atom_grid_v0(self,pts,wts,a)
   ! Make the *normalised* Salvador integration grid ("pts","wts")
   ! *only* for atom "a". 
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pts
      VEC(REAL,:), allocatable, OUT :: wts
      INT, IN :: a
      VEC(REAL,:), allocatable :: Wa
   ENSURE(self%finalized,"BECKE_GRID:make_Salvador_atom_grid_v0 ... call the set_grid_data routine")
   ENSURE(allocated(self%atom),"BECKE_GRID:make_Salvador_atom_grid_v0 ... no atom list")
   ENSURE(is_in_range_(a,[1,size(self%atom)]),"BECKE_GRID:make_Salvador_atom_grid_v0 ... `a' argument out of range")
   ENSURE(allocated(self%Salvador_chi),"BECKE_GRID:make_Salvador_atom_grid_v0 ... no Salvador_chi.")

      ! Unnormalized grid for atom "a"
      call make_atom_grid_(self,pts,wts,a)

      ! Salvador weights for atom "a"
      call create_(Wa,size(pts,1))
      call make_Salvador_cell_fn_(self,Wa,pts,a)
      wts = wts*Wa
      call destroy_(Wa)
   end subroutine


! Not working

!   make_Salvador_atom_grid_v1(pts,wts,a) :: PURE
!   ! Make the *normalised* Salvador integration grid ("pts","wts")
!   ! *only* for atom "a". See Salvador (2013) JCP 139 p 071103.
!      self :: IN
!      pts  :: MAT{REAL}@, OUT
!      wts  :: VEC{REAL}@, OUT
!      a    :: INT, IN
!
!   ENSURE(.finalized,"call the set_grid_data routine")
!   ENSURE(.atom.allocated,"no atom list")
!   ENSURE(a.is_in_range([1,.atom.dim]),"`a' argument out of range")
!
!      atoms        :: VEC{INT}@
!      Pa, Pi, Ps   :: VEC{REAL}@
!      ri, rj, rg   :: VEC{REAL}(3)
!      dgi,dgj, chi :: REAL
!      dij, mu, s   :: REAL
!      n_atoms, i,j :: INT
!      n_pts,   g   :: INT
!
!      ! Unnormalized grid for atom "a"
!      .:make_atom_grid(pts,wts,a)
!
!      ! Create a & neighbours, n=2
!      ! The 1st atom is "a"
!    ! atoms   = .Salvador_n_neighbours_of(a,n=2)
!      atoms   = [ (i,i=1,.atom.dim) ]
!      n_atoms = atoms.dim
!
!      ! Cell functions
!      Ps.create(n_pts) ! Sum_i P_i
!      Pa.create(n_pts) ! a-th cell fn
!      Pi.create(n_pts) ! i-th cell fn
!
!      ! Sum cell fns. Pi
!      Ps = ZERO  ! <--- NOTE
!      do i = 1,n_atoms
!
!         ri = .atom(atoms(i)).position
!
!         Pi = ONE ! <--- NOTE
!
!         ! Product over step fns.
!         ! Pi = Prod_j s(mu_ij)
!         do j = 1,n_atoms
!
!            if (i==j) cycle
!
!            rj  = .atom(atoms(j)).position
!            dij = ONE/norm2(rj-ri)
!
!            ! Becke size adjustment
!            chi = .Salvador_chi(atoms(i),atoms(j))
!
!            ! Grid points "g"
!            do g = 1,n_pts
!               rg  = pts(g,:)
!               dgi = norm2(ri-rg)
!               dgj = norm2(rj-rg)
!               mu  = (dgi-dgj)*dij 

!                     (ONE+mu+chi*(ONE-mu))  ! Eq. (9) nu'
!               mu  = HALF*mu*(THREE-mu*mu)  ! Stiffness 4
!               mu  = HALF*mu*(THREE-mu*mu)
!               mu  = HALF*mu*(THREE-mu*mu)
!               mu  = HALF*mu*(THREE-mu*mu)
!               s   = HALF*(ONE-mu)          ! Eq. (5)
!               Pi(g) = Pi(g) * s            ! Eq. (4) RHS
!            end
!
!         end
!
!         ! Keep Pa
!         if (atoms(i)==a) Pa = Pi
!
!         ! Add & store
!         Ps = Ps + Pi
!
!      end
!
!      ! Normalize/modify wts
!      wts = wts * Pa/Ps
!
!      ! Clean
!      Pi.destroy
!      Pa.destroy
!      Ps.destroy
!
!   end

!   Salvador_n_neighbours_of(a,n) result (res) :: leaky, PURE
!   ! Get the "n"-th next-nearest Salvador neighbours of at atom "a".
!   ! The first atom is always "a". Works only for n=1,2.
!      self :: IN
!      a    :: INT, IN
!      n    :: INT, IN
!      res  :: VEC{INT}@
!
!      n_atoms,x :: INT
!
!      n_atoms = 1 + .Salvador_bonds_for(a).element.dim
!      res.create(n_atoms)
!      res(1 ) = a ! Atom a is 1st one
!      res(2:) = .Salvador_bonds_for(a).element
!
!      if (n==2) then
!         do x = 1,.Salvador_bonds_for(a).element.dim
!            res = [ res, .Salvador_bonds_for(x).element ]
!         end
!         res.remove_repetitions
!      end
!
!   end

!  =================
!  Atom weights only
!  =================

   PURE subroutine make_Salvador_cell_fn(self,Wa,pts,a)
   ! Make the *normalised* Salvador atom weights "Wa" for atom "a"
   ! evaluated on the set of points "pts". 
   ! See Eqn. (4) in Salvador (2013) JCP 139 p 071103.
   ! At each point, atom i's cell function P_i is a product of step
   ! factors, each <= 1, so it is at most the factor for the atom
   ! nearest the point. Where that factor is below 1e-15, P_i is left
   ! out; that makes the cost N^2 rather than N^3 for N atoms. The kept
   ! P_i are computed exactly as without the test.
      type(BECKE_GRID_TYPE), IN :: self
      VEC(REAL,:), OUT :: Wa
      MAT(REAL,:,:), IN :: pts
      INT, IN :: a
      MAT(REAL,:,:), allocatable :: D,Dk,inv_R
      VEC(REAL,:), allocatable :: Pa,Pi,Ps
      VEC(INT,:), allocatable :: near,idx
      VEC(BIN,:), allocatable :: keep
      VEC(REAL,3) :: ri,rj
      REAL :: mu,eps
      REAL :: rij,chi
      INT :: n_atoms,i,j
      INT :: n_pts,g,k,q
      INT :: blk,b0,b1,nb
      INT :: m
   ENSURE(self%finalized,"BECKE_GRID:make_Salvador_cell_fn ... call the set_grid_data routine")
   ENSURE(size(Wa)==size(pts,1),"BECKE_GRID:make_Salvador_cell_fn ... incompatble dim1, Wa and pts")
   ENSURE(3==size(pts,2),"BECKE_GRID:make_Salvador_cell_fn ... wrong dim2, pts")
   ENSURE(allocated(self%atom),"BECKE_GRID:make_Salvador_cell_fn ... no atom list")
   ENSURE(is_in_range_(a,[1,size(self%atom)]),"BECKE_GRID:make_Salvador_cell_fn ... `a' argument out of range")
      eps = TOL(15)
      blk = 2048

      ! Sizes
      n_atoms = size(self%atom)
      n_pts = size(pts,1)

      ! Inverse interatomic distances
      call create_(inv_R,n_atoms,n_atoms)
      inv_R = ZERO
      do i = 1,n_atoms
         ri = self%atom(i)%position
         do j = 1,n_atoms
            if (i==j) cycle
            rj = self%atom(j)%position
            inv_R(i,j) = ONE/norm2(rj-ri)
         end do
      end do
      nb = min(blk,n_pts)
      call create_(D,nb,n_atoms)  ! Point-atom distances
      call create_(Dk,nb,n_atoms)  ! The same, for the kept points only
      call create_(near,nb)  ! Nearest atom to each point
      call create_(keep,nb)
      call create_(Ps,nb)  ! Sum_i P_i
      call create_(Pa,nb)  ! a-th cell fn
      call create_(Pi,nb)  ! i-th cell fn, on the kept points

      ! Blocks of points
      do b0 = 1,n_pts,blk
         b1 = min(b0+blk-1,n_pts)
         nb = b1-b0+1
         do i = 1,n_atoms
            ri = self%atom(i)%position
            do g = 1,nb
               D(g,i) = norm2(ri-pts(b0+g-1,:))
            end do
         end do
         do g = 1,nb
            near(g) = minloc(D(g,:),1)
         end do
         Ps(1:nb) = ZERO
         Pa(1:nb) = ZERO
         do i = 1,n_atoms

            ! Points where P_i can matter: bounded by the nearest atom
            do g = 1,nb
               k = near(g)
               if (k==i) then
                  keep(g) = TRUE
               else
                  mu = (D(g,i)-D(g,k))*inv_R(i,k)
                  keep(g) = Salvador_step_(mu,self%Salvador_chi(i,k))>eps
               end if
            end do
            m = count(keep(1:nb))
            if (m==0) cycle
            idx = pack([(g,g = 1,nb)],keep(1:nb))

            ! The kept points' distances, contiguous, so the loop below
            ! runs over consecutive memory and the compiler can vectorise it
            do j = 1,n_atoms
               do q = 1,m
                  Dk(q,j) = D(idx(q),j)
               end do
            end do

            ! P_i = Prod_j s(mu_ij), Eq. (4) RHS
            Pi(1:m) = ONE
            do j = 1,n_atoms
               if (i==j) cycle
               rij = inv_R(i,j)
               chi = self%Salvador_chi(i,j)
               do q = 1,m
                  mu = (Dk(q,i)-Dk(q,j))*rij
                  Pi(q) = Pi(q)*Salvador_step_(mu,chi)
               end do
            end do

            ! Keep Pa; add to Ps
            if (i==a) Pa(idx) = Pi(1:m)
            Ps(idx) = Ps(idx)+Pi(1:m)
         end do

         ! Normalize
         Wa(b0:b1) = Pa(1:nb)/Ps(1:nb)
      end do

      ! Clean
      call destroy_(idx)
      call destroy_(Pi)
      call destroy_(Pa)
      call destroy_(Ps)
      call destroy_(keep)
      call destroy_(near)
      call destroy_(Dk)
      call destroy_(D)
      call destroy_(inv_R)
   end subroutine

   PURE function Salvador_step(mu,chi) result (res)
   ! The Becke step factor s for the Salvador cell function, for
   ! elliptical coordinate "mu" and size adjustment "chi". Eqs. (5), (9)
   ! in Salvador (2013) JCP 139 p 071103, with stiffness 4.
      REAL, IN :: mu,chi
      REAL :: res
      REAL :: nu
      nu = (ONE+mu-chi*(ONE-mu))/(ONE+mu+chi*(ONE-mu))  ! Eq. (9) nu'
      nu = HALF*nu*(THREE-nu*nu)  ! Stiffness 4
      nu = HALF*nu*(THREE-nu*nu)
      nu = HALF*nu*(THREE-nu*nu)
      nu = HALF*nu*(THREE-nu*nu)
      res = HALF*(ONE-nu)  ! Eq. (5)
   end function


! Not working

!   make_Salvador_cell_fn(Wa,pts,a) :: PURE
!   ! Make the *normalised* Salvador atom weights "Wa" for atom "a"
!   ! evaluated on the set of points "pts". 
!   ! See Eqn. (4) in Salvador (2013) JCP 139 p 071103.
!      self :: IN
!      Wa   :: VEC{REAL}, OUT
!      pts  :: MAT{REAL}, IN
!      a    :: INT, IN
!
!   ENSURE(.finalized,"call the set_grid_data routine")
!   ENSURE(Wa.dim==pts.dim1,"incompatble dim1, Wa and pts")
!   ENSURE(3==pts.dim2,"wrong dim2, pts")
!   ENSURE(.atom.allocated,"no atom list")
!   ENSURE(a.is_in_range([1,.atom.dim]),"`a' argument out of range")
!
!      atoms        :: VEC{INT}@
!      Pa, Pi, Ps   :: VEC{REAL}@
!      ri, rj, rg   :: VEC{REAL}(3)
!      dgi,dgj, chi :: REAL
!      dij, mu, s   :: REAL
!      n_atoms, i,j :: INT
!      n_pts,   g   :: INT
!
!      ! Sizes
!      n_atoms = 1 + .Salvador_bonds_for(a).element.dim
!      n_pts   = pts.dim1
!
!      ! Create a & neighbours
!      atoms.create(n_atoms)
!      atoms(1 ) = a
!      atoms(2:) = .Salvador_bonds_for(a).element
!
!      ! Cell functions
!      Ps.create(n_pts) ! Sum_i P_i
!      Pa.create(n_pts) ! a-th cell fn
!      Pi.create(n_pts) ! i-th cell fn
!      Ps = ZERO        ! <--- NOTE
!
!      ! Sum cell fns. Pi
!      do i = 1,n_atoms
!
!         ri = .atom(atoms(i)).position
!
!         Pi = ONE ! <--- NOTE
!
!         ! Product over step fns.
!         ! Pi = Prod_j s(mu_ij)
!         do j = 1,n_atoms
!
!            if (i==j) cycle
!
!            rj  = .atom(atoms(j)).position
!            dij = ONE/norm2(rj-ri)
!
!            ! Becke size adjustment
!            chi = .Salvador_chi(i,j)
!
!            ! Grid points "g"
!            do g = 1,n_pts
!               rg  = pts(g,:)
!               dgi = norm2(ri-rg)
!               dgj = norm2(rj-rg)
!               mu  = (dgi-dgj)*dij 

!                     (ONE+mu+chi*(ONE-mu))  ! Eq. (9) nu'
!               mu  = HALF*mu*(THREE-mu*mu)  ! Stiffness 4
!               mu  = HALF*mu*(THREE-mu*mu)
!               mu  = HALF*mu*(THREE-mu*mu)
!               mu  = HALF*mu*(THREE-mu*mu)
!               s   = HALF*(ONE-mu)          ! Eq. (5)
!               Pi(g) = Pi(g) * s            ! Eq. (4) RHS
!            end
!
!         end
!
!         ! Add & store
!         Ps = Ps + Pi
!         if (i==a) Pa = Pi
!
!      end
!
!      ! Normalize
!      Wa = Pa/Ps
!
!      ! Clean
!      Pi.destroy
!      Pa.destroy
!      Ps.destroy
!
!   end

! Evaluated on a set of pts, no grid wts

!  =============================
!  Full grid generation routines
!  =============================

! These below are older (deprecated?)
! NO! - 12/05/2023 in molecule.cp

   PURE subroutine make_grid_0(self,pt,wt,compress,wt_is_0)
   ! Make the DFT integration grid ("pt","wt") for a given list of "atoms".
   ! If you don't want a compressed grid set "compress" to FALSE. Here
   ! compression refers to removal of points whose partition weight is small.
   ! Thus, if "atoms" is comprised of one atom only, no compression is allowed.
   ! If you need the .wt_is_0 array (to save work when making basis function
   ! grids) then set "wt_is_0" to TRUE. NOTE: the "atoms" array must have no
   ! repetitions. NOTE: The precalculated atom grids must have been made by
   ! calling .set_grid_data. NOTE: For linear scaling calculations, don't use this
   ! routine: instead use the routine based on atom pairs.
      type(BECKE_GRID_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable, OUT :: pt
      VEC(REAL,:), allocatable, OUT :: wt
      BIN, optional, IN :: compress
      BIN, optional, IN :: wt_is_0
      INT :: i
      i = 0
      call make_grid_(self,pt,wt,[(i,i = 1,size(self%atom))],compress,wt_is_0)
   end subroutine

   PURE subroutine make_grid_1(self,pt,wt,atoms,compress,wt_is_0)
   ! Make the DFT integration grid ("pt","wt") for a given list of
   ! "atoms".  The precalculated atom grids must have been made by
   ! calling .set_grid_data If you don't want a compressed grid set
   ! "compress" to FALSE. Here compression refers to removal of points
   ! whose partition weight is small.  Thus, if "atoms" is comprised
   ! of one atom only, no compression is allowed.  If you need the
   ! .wt_is_0 array (to save work when making basis function grids)
   ! then set "wt_is_0" to TRUE. NOTE: the "atoms" array must
   ! have no repetitions.NOTE: The precalculated atom grids must
   ! have been made by calling .set_grid_data.
      type(BECKE_GRID_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable, OUT :: pt
      VEC(REAL,:), allocatable, OUT :: wt
      VEC(INT,:), IN :: atoms
      BIN, optional, IN :: compress
      BIN, optional, IN :: wt_is_0
      INT :: n_pt,a,u,f,l,i,k
      BIN :: keep,kompress
      MAT(REAL,:,:), allocatable :: pt0
      VEC(REAL,:), allocatable :: wt0
   ENSURE(self%finalized,"BECKE_GRID:make_grid_1 ... radial and angular grids are unmade; use set_grid_data")
   ENSURE(allocated(self%atom_pts),"BECKE_GRID:make_grid_1 ... no atom_pts")
   ENSURE(NOT allocated(self%wt_is_0),"BECKE_GRID:make_grid_1 ... the wt_is_0 array exists!")
   ENSURE(NOT has_repetitions_(atoms),"BECKE_GRID:make_grid_1 ... the atoms array has repetitions!")

      ! kompress & keep logic a bit messy here
      ! --dylan
      kompress = TRUE
      if (present(compress)) kompress = compress
      if (size(atoms)==1) kompress = FALSE
      keep = FALSE
      if (present(wt_is_0)) keep = wt_is_0
      n_pt = no_of_points_(self,atoms)
      call create_(pt,n_pt,3)  ! leaky
      call create_(wt,n_pt)

      ! For compression
      call create_(self%wt_is_0,n_pt)  ! leaky
      self%wt_is_0 = FALSE
      l = 0
      do a = 1,size(atoms)
         u = self%unique_atom_for(atoms(a))
         f = l+1
         l = l+size(self%atom_wts(u)%element)
         pt(f:l,:) = self%atom_pts(u)%element
         wt(f:l) = self%atom_wts(u)%element
         call displace_(self,pt(f:l,:),atoms(a))

         ! No partition needed for one atom ...
         if (NOT kompress) cycle
         call partition_(self,wt(f:l),pt(f:l,:),a,atoms,self%wt_is_0(f:l))
      end do
      if (kompress) then

         ! Prune atom-pair grids
         call make_wt_is_0_(self,pt,atoms)

         ! How many non-zero weights?
         n_pt = count(NOT self%wt_is_0)
         if (n_pt==size(wt)) then

            ! None? Don't need .wt_is_0
            keep = FALSE
         else

            ! Compress the grid here
            call create_(pt0,n_pt,3)
            call create_(wt0,n_pt)
            k = 0
            do i = 1,size(wt)
               if (self%wt_is_0(i)) cycle
               k = k+1
               pt0(k,:) = pt(i,:)
               wt0(k) = wt(i)
            end do
            wt = wt0
            pt = pt0
         end if
      end if

      ! Clean - leaky!
      if (NOT keep) call destroy_(self%wt_is_0)
   end subroutine


!  ==============
!  Displace grids
!  ==============

   PURE subroutine make_displacements(self,dx,dy,dz,d2,pt,overlapping_atom)
   ! Make the displacements "dx", "dy", "dz" and squared distances
   ! "d2" to the points "pt" from the "overlapping_atoms"
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: dx,dy,dz,d2
      MAT(REAL,:,:), IN :: pt
      VEC(INT,:), IN :: overlapping_atom
      INT :: n_pt,n_ov,b,ob,n
      REAL :: b1,b2,b3,x,y,z,r
   ENSURE(size(dx,1)==size(pt,1),"BECKE_GRID:make_displacements ... wrong dim1, dx")
   ENSURE(size(dx,2)==size(overlapping_atom),"BECKE_GRID:make_displacements ... wrong dim1, dx")
   ENSURE(is_same_shape_as_(dx,dx),"BECKE_GRID:make_displacements ... dy inconsistent shape")
   ENSURE(is_same_shape_as_(dy,dx),"BECKE_GRID:make_displacements ... dz inconsistent shape")
   ENSURE(is_same_shape_as_(d2,dx),"BECKE_GRID:make_displacements ... d2 inconsistent shape")

      ! No. of grid points
      n_pt = size(pt,1)

      ! No of overlapping atoms
      n_ov = size(overlapping_atom)

      ! Overlapping atoms "b"
      do b = 1,n_ov

         ! Coordinates of "b"
         ob = overlapping_atom(b)
         b1 = self%atom(ob)%position(1)
         b2 = self%atom(ob)%position(2)
         b3 = self%atom(ob)%position(3)

         ! Displacements from  "b"
         do n = 1,n_pt
            x = pt(n,1)-b1
            y = pt(n,2)-b2
            z = pt(n,3)-b3
            r = x*x+y*y+z*z
            dx(n,b) = x
            dy(n,b) = y
            dz(n,b) = z
            d2(n,b) = r
         end do
      end do
   end subroutine

   PURE subroutine displace(self,pt,a)
   ! Displace the atomic grid from the origin to the position of atom "a".
   ! Schematically:
   !           ("pt","wt") <-- ("pt" + ".atom(a).position", "wt").
   ! References: A. D. Becke, J. Chem. Phys. 88 (1988) 2547.
   !             O. Treutler and R. Ahlrichs, J. Chem. Phys. 102 (1995) 346.
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: pt
      INT, IN :: a
      INT :: n
      do n = 1,size(pt,1)
         pt(n,:) = pt(n,:)+self%atom(a)%position
      end do
   end subroutine


!  =======================
!  Stratmann-Scuseria grid
!  =======================

   PURE subroutine make_partitioned_grid_0(self,pt,wt,f0,atom_a,overlapping_atom,index)
   ! Make the integration grid ("pt","wt") for "atom_a", with the
   ! .partition_scheme weights for the "overlapping_atom" list folded
   ! into "wt", and the basis function grid "f0" for "atom_a".
   ! Stratmann-Scuseria has its own fast path; the other schemes go
   ! through the "partition" dispatcher. With "index" TRUE, "f0" is the
   ! one-column row map described at make_atom_grid.
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pt
      VEC(REAL,:), allocatable, OUT :: wt
      MAT(REAL,:,:), allocatable, OUT :: f0
      INT, IN :: atom_a
      VEC(INT,:), IN :: overlapping_atom
      BIN, optional, IN :: index
      select case (self%partition_scheme)
      case ("stratmann_scuseria")
         call make_unpruned_SS_grid_(self,pt,wt,f0,atom_a,overlapping_atom,index)
      case default
         call make_atom_grid_(self,pt,wt,f0,atom_a,index)
         call apply_partition_(self,pt,wt,atom_a,overlapping_atom)
      end select
      call prune_grid_(self,pt,wt,f0)
   end subroutine

   PURE subroutine make_partitioned_grid_1(self,pt,wt,f0,gx,gy,gz,atom_a,overlapping_atom)
   ! Make the integration grid ("pt","wt") for "atom_a", with the
   ! .partition_scheme weights for the "overlapping_atom" list folded
   ! into "wt", the basis function grid "f0" for "atom_a", and its
   ! derivatives "gx", "gy", "gz".
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pt
      VEC(REAL,:), allocatable, OUT :: wt
      MAT(REAL,:,:), allocatable, OUT :: f0
      MAT(REAL,:,:), allocatable, OUT :: gx,gy,gz
      INT, IN :: atom_a
      VEC(INT,:), IN :: overlapping_atom
      select case (self%partition_scheme)
      case ("stratmann_scuseria")
         call make_unpruned_SS_grid_(self,pt,wt,f0,gx,gy,gz,atom_a,overlapping_atom)
      case default
         call make_atom_grid_(self,pt,wt,f0,gx,gy,gz,atom_a)
         call apply_partition_(self,pt,wt,atom_a,overlapping_atom)
      end select
      call prune_grid_(self,pt,wt,f0,gx,gy,gz)
   end subroutine

   PURE subroutine apply_partition(self,pt,wt,atom_a,overlapping_atom)
   ! Multiply the atom_a grid weights "wt" by the normalised
   ! .partition_scheme weight, using the "overlapping_atom" list as the
   ! integration centres. No point is dropped: .partition_cutoff
   ! applies only to the make_grid path.
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), INOUT :: wt
      INT, IN :: atom_a
      VEC(INT,:), IN :: overlapping_atom
      VEC(BIN,:), allocatable :: wt_is_0
      INT :: a,i
      if (size(overlapping_atom)==1) return

      ! Position of atom_a in the list
      a = 0
      do i = 1,size(overlapping_atom)
         if (overlapping_atom(i)/=atom_a) cycle
         a = i
         exit
      end do
      ENSURE(a>0,"BECKE_GRID:apply_partition ... atom_a is not in the overlapping_atom list")
      call create_(wt_is_0,size(wt))
      call partition_(self,wt,pt,a,overlapping_atom,wt_is_0)
      call destroy_(wt_is_0)
   end subroutine

   PURE subroutine make_unpruned_SS_grid_0(self,pt,wt,f0,atom_a,overlapping_atom,index)
   ! Make the unpruned Stratman-Scuseria integration grid ("pt","wt")
   ! for the "atom_a". Weights "wt" incorporate grid integration
   ! weights and the SS partition weight for "overlapping_atoms"
   ! Return also the basis function grid "f0" for "atom_a".
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pt
      MAT(REAL,:,:), allocatable, OUT :: f0
      VEC(REAL,:), allocatable, OUT :: wt
      INT, IN :: atom_a
      VEC(INT,:), IN :: overlapping_atom
      BIN, optional, IN :: index
      MAT(REAL,:,:), allocatable :: dist
      VEC(BIN,:), allocatable :: zero
      VEC(REAL,:), allocatable :: ww
      INT :: n_pt,n1_SS,n_ov,n_atom

      ! Make the basic grid points
      call make_atom_grid_(self,pt,wt,f0,atom_a,index)

      ! Sizes
      n_pt = size(pt,1)
      n1_SS = self%atom_n1_SS(atom_a)
      n_ov = size(overlapping_atom)
      n_atom = size(self%atom)

      ! Distances to overlapping atoms
      call create_(dist,n_pt,n_ov)

      ! Stratmann-Scuseria zero partition values
      call create_(zero,n1_SS+1,n_pt)  ! Note lbound

      ! Make the distances and zero partition weights
      call make_distances_(self,dist,zero,pt,atom_a,overlapping_atom)

      ! Compress out the zero partition weights
      call compress_zeros_(self,pt,wt,f0,dist,zero,atom_a)
      call destroy_(zero)
      if (n_atom>1) then

         ! Make the Stratman-Scuseria weights
         n_pt = size(wt)
         call create_(ww,n1_SS+1,n_pt)
         call make_SS_wts_(self,ww,dist,atom_a,overlapping_atom)

         ! Incorporate SS weights into grid weights
         wt(n1_SS+1:) = ww(n1_SS+1:)*wt(n1_SS+1:)
         call destroy_(ww)
      end if

      ! Clean
      call destroy_(dist)
   end subroutine

   PURE subroutine make_unpruned_SS_grid_1(self,pt,wt,f0,gx,gy,gz,atom_a,overlapping_atom)
   ! Make the unpruned Stratman-Scuseria integration grid ("pt","wt")
   ! for the "atom_a". Weights "wt" incorporate grid integration
   ! weights and the SS partition weight for "overlapping_atoms"
   ! Return also the basis function grid "f0" and the basis function
   ! derivatives "gx", "gy", "gz" for "atom_a"
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pt
      VEC(REAL,:), allocatable, OUT :: wt
      MAT(REAL,:,:), allocatable, OUT :: f0
      MAT(REAL,:,:), allocatable, OUT :: gx,gy,gz
      INT, IN :: atom_a
      VEC(INT,:), IN :: overlapping_atom
      MAT(REAL,:,:), allocatable :: dist
      VEC(BIN,:), allocatable :: zero
      VEC(REAL,:), allocatable :: ww
      INT :: n_pt,n1_SS,n_ov,n_atom

      ! Make the basic grid points
      call make_atom_grid_(self,pt,wt,f0,gx,gy,gz,atom_a)

      ! Sizes
      n_pt = size(pt,1)
      n1_SS = self%atom_n1_SS(atom_a)
      n_ov = size(overlapping_atom)
      n_atom = size(self%atom)

      ! Distances to overlapping atoms
      call create_(dist,n_pt,n_ov)

      ! Stratmann-Scuseria zero partition values
      call create_(zero,n1_SS+1,n_pt)  ! Note lbound

      ! Make the distances and zero partition weights
      call make_distances_(self,dist,zero,pt,atom_a,overlapping_atom)

      ! Compress out the zero partition weights
      call compress_zeros_(self,pt,wt,f0,gx,gy,gz,dist,zero,atom_a)
      call destroy_(zero)
      if (n_atom>1) then

         ! Make the Stratman-Scuseria weights
         n_pt = size(wt)
         call create_(ww,n1_SS+1,n_pt)
         call make_SS_wts_(self,ww,dist,atom_a,overlapping_atom)

         ! Incorporate SS weights into grid weights
         wt(n1_SS+1:) = ww(n1_SS+1:)*wt(n1_SS+1:)
         call destroy_(ww)
      end if

      ! Clean
      call destroy_(dist)
   end subroutine

   PURE subroutine compress_zeros_0(self,pt,wt,f0,dist,zero,atom_a)
   ! Compress the zero values out of the grid and displacements
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, INOUT :: pt
      VEC(REAL,:), allocatable, INOUT :: wt
      MAT(REAL,:,:), allocatable, INOUT :: f0
      MAT(REAL,:,:), allocatable, INOUT :: dist
      VEC(BIN,:), allocatable, IN :: zero
      INT, IN :: atom_a
      VEC(INT,:), allocatable :: map
      INT :: n_pt,n1_SS,n_ov,n_bf,n,k,b,f
   ENSURE(allocated(self%atom_n1_SS),"BECKE_GRID:compress_zeros_0 ... no atom_n1_SS")
      if (NOT any(zero)) return

      ! Sizes
      n_pt = size(pt,1)
      n1_SS = self%atom_n1_SS(atom_a)
      n_ov = size(dist,2)
      n_bf = size(f0,2)

      ! Creat map array
      call create_(map,n1_SS+1,n_pt)

      ! Compress out the zero pts & weights
      k = n1_SS
      do n = n1_SS+1,n_pt
         if (zero(n)) cycle
         k = k+1
         map(k) = n
         pt(k,1) = pt(n,1)
         pt(k,2) = pt(n,2)
         pt(k,3) = pt(n,3)
         wt(k) = wt(n)  ! grid weight
      end do

      ! New no of points
      n_pt = k

      ! Compress out the distances
      do b = 1,n_ov
         do k = n1_SS+1,n_pt
            n = map(k)
            dist(k,b) = dist(n,b)
         end do
      end do

      ! Compress out the basis functions
      do f = 1,n_bf
         do k = n1_SS+1,n_pt
            n = map(k)
            f0(k,f) = f0(n,f)
         end do
      end do

      ! Clean
      call destroy_(map)

      ! Shrink
      call shrink_(pt,n_pt,3)
      call shrink_(wt,n_pt)
      call shrink_(f0,n_pt,n_bf)
      call shrink_(dist,n_pt,n_ov)
   end subroutine

   PURE subroutine compress_zeros_1(self,pt,wt,f0,gx,gy,gz,dist,zero,atom_a)
   ! Compress the zero values out of the grid and displacements
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, INOUT :: pt
      VEC(REAL,:), allocatable, INOUT :: wt
      MAT(REAL,:,:), allocatable, INOUT :: f0
      MAT(REAL,:,:), allocatable, INOUT :: gx,gy,gz
      MAT(REAL,:,:), allocatable, INOUT :: dist
      VEC(BIN,:), allocatable, IN :: zero
      INT, IN :: atom_a
      VEC(INT,:), allocatable :: map
      INT :: n_pt,n1_SS,n_ov,n_bf,n,k,b,f
      if (NOT any(zero)) return

      ! Sizes
      n_pt = size(pt,1)
      n1_SS = self%atom_n1_SS(atom_a)
      n_ov = size(dist,2)
      n_bf = size(f0,2)

      ! Creat map array
      call create_(map,n1_SS+1,n_pt)

      ! Compress out the zero pts & weights
      k = n1_SS
      do n = n1_SS+1,n_pt
         if (zero(n)) cycle
         k = k+1
         map(k) = n
         pt(k,1) = pt(n,1)
         pt(k,2) = pt(n,2)
         pt(k,3) = pt(n,3)
         wt(k) = wt(n)  ! grid weight
      end do

      ! New no of points
      n_pt = k

      ! Compress out the distances
      do b = 1,n_ov
         do k = n1_SS+1,n_pt
            n = map(k)
            dist(k,b) = dist(n,b)
         end do
      end do

      ! Compress out the basis functions
      do f = 1,n_bf
         do k = n1_SS+1,n_pt
            n = map(k)
            f0(k,f) = f0(n,f)
            gx(k,f) = gx(n,f)
            gy(k,f) = gy(n,f)
            gz(k,f) = gz(n,f)
         end do
      end do

      ! Clean
      call destroy_(map)

      ! Shrink
      call shrink_(pt,n_pt,3)
      call shrink_(wt,n_pt)
      call shrink_(f0,n_pt,n_bf)
      call shrink_(gx,n_pt,n_bf)
      call shrink_(gy,n_pt,n_bf)
      call shrink_(gz,n_pt,n_bf)
      call shrink_(dist,n_pt,n_ov)
   end subroutine

   PURE subroutine make_distances(self,dist,zero,pt,atom_a,overlapping_atom)
   ! Make the distances "dist" to the grid points "pt" for "atom_a"
   ! from the "overlapping_atoms" .  Also return "zero(:,b)" which is
   ! TRUE for grid points (:) which have zero SS partiton weight.
      type(BECKE_GRID_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: dist
      VEC(BIN,:), allocatable, INOUT :: zero  ! Note lbound
      MAT(REAL,:,:), IN :: pt
      INT, IN :: atom_a
      VEC(INT,:), IN :: overlapping_atom
      INT :: n_pt,n1_SS,n_ov,n,b,ob
      REAL :: b1,b2,b3,x,y,z,r,r_max_SS

      ! Sizes
      n_pt = size(pt,1)
      n1_SS = self%atom_n1_SS(atom_a)
      n_ov = size(overlapping_atom)

      ! Initially no zero weights
      zero = FALSE

      ! Distances to the overlapping atoms
      do b = 1,n_ov
         ob = overlapping_atom(b)
         b1 = self%atom(ob)%position(1)
         b2 = self%atom(ob)%position(2)
         b3 = self%atom(ob)%position(3)

         ! These pts can't have zero weight
         do n = 1,n1_SS
            x = pt(n,1)-b1
            y = pt(n,2)-b2
            z = pt(n,3)-b3
            r = sqrt(x*x+y*y+z*z)
            dist(n,b) = r
         end do

         ! Zero weights are near non atom_a
         if (atom_a==ob) then
            do n = n1_SS+1,n_pt
               x = pt(n,1)-b1
               y = pt(n,2)-b2
               z = pt(n,3)-b3
               r = sqrt(x*x+y*y+z*z)
               dist(n,b) = r
            end do
         else
            r_max_SS = r_max_SS_for_atom_(self,ob)
            do n = n1_SS+1,n_pt
               if (zero(n)) cycle
               x = pt(n,1)-b1
               y = pt(n,2)-b2
               z = pt(n,3)-b3
               r = sqrt(x*x+y*y+z*z)
               dist(n,b) = r
               zero(n) = r<=r_max_SS  ! near ob /= atom_a
            end do
         end if
      end do
   end subroutine

   PURE subroutine make_SS_wts(self,wt,dist,atom_a,overlapping_atom)
   ! Make the Stratman-Scuseria partition weight "wt" for "atom_a"
   ! given distances "dist" from the grid points to all relevant
   ! "overlapping_atoms". Stratmann et al, CPL 257 p. 213-23 (1996).
      type(BECKE_GRID_TYPE), IN :: self
      VEC(REAL,:), allocatable, INOUT :: wt  ! Note lbound
      MAT(REAL,:,:), IN :: dist
      INT, IN :: atom_a
      VEC(INT,:), IN :: overlapping_atom
      VEC(REAL,:), allocatable :: ws,wb,ra,rb
      VEC(BIN,:), allocatable :: zero
      INT :: n_pt,n1_SS,n_ov,n,a,b,oa,ob
      REAL :: a1,a2,a3,b1,b2,b3,x,y,z,r
      REAL :: aa,c5,c21,c35,mu,mu1,mu2,mu3,mu5,mu7

      ! Sizes
      n_pt = ubound(wt,1)
      n1_SS = self%atom_n1_SS(atom_a)
      n_ov = size(overlapping_atom)

      ! Stratmann-Scuseria constants
      aa = self%stratmann_scuseria_A
      c5 = FIVE
      c21 = SEVEN*THREE
      c35 = SEVEN*FIVE

      ! Partition weights sum ws
      call create_(ws,[n1_SS+1,n_pt])
      ws = ZERO

      ! Unnormalized weight for atom "b"
      call create_(wb,[n1_SS+1,n_pt])

      ! Zero weights for atom a
      call create_(zero,[n1_SS+1,n_pt])

      ! Make "wb" the SS weight for atom "b" and
      ! make "ws" the sum of the SS weights for each atom
      do b = 1,n_ov
         ob = overlapping_atom(b)

         ! Atom "b" position
         b1 = self%atom(ob)%position(1)
         b2 = self%atom(ob)%position(2)
         b3 = self%atom(ob)%position(3)

         ! Set unit weights
         ! wb = Product over atoms a of s(mu) factor
         ! Left hand side of eq. (9)
         wb = ONE
         rb = dist(:,b)

         ! Initially, no weight wb is zero
         zero = FALSE
         do a = 1,n_ov
            if (a==b) cycle
            oa = overlapping_atom(a)
            a1 = self%atom(oa)%position(1)
            a2 = self%atom(oa)%position(2)
            a3 = self%atom(oa)%position(3)

            ! Distance between the atoms
            x = b1-a1
            y = b2-a2
            z = b3-a3
            r = sqrt(x*x+y*y+z*z)
            ra = dist(:,a)

            ! Make the mu and SS weight for atom b
            do n = n1_SS+1,n_pt

               ! Is this point killed off?
               if (zero(n)) cycle
               mu = (rb(n)-ra(n))/r
               if (mu<=-aa) then

                  ! Weight is 1. Should never occur since
                  ! we loop over points outside eq. (15).
                  cycle
               else if (mu>=aa) then

                  ! Kill grid point n. Once killed for atom "a"
                  ! the pt can be skipped forever. See eq. (9).
                  wb(n) = ZERO
                  zero(n) = TRUE
               else

                  ! Eq. (14)
                  mu1 = mu/aa
                  mu2 = mu1*mu1
                  mu3 = mu2*mu1
                  mu5 = mu3*mu2
                  mu7 = mu5*mu2
                  z = SIXTEENTH*(c35*mu1-c35*mu3+c21*mu5-c5*mu7)
                  wb(n) = wb(n)*HALF*(ONE-z)
               end if
            end do
         end do

         ! Sum weights, denominator of eq. (10)
         ws = ws+wb

         ! Store atom_a wt in "wt"
         if (atom_a==ob) wt = wb
      end do

      ! Normalise the weight, eq. (10)
      wt = wt/ws

      ! Clean
      call destroy_(zero)
      call destroy_(wb)
      call destroy_(ws)
   end subroutine


!  ==================================
!  Partition the grids for each atoms
!  ==================================

   PURE subroutine partition(self,wt,pt,a,atoms,wt_is_0)
   ! Partition the atomic grid ("pt","wt") for the atom "a" in the list of
   ! "atoms" so that the weights work with all the other atomic positions
   ! Schematically:
   !         ("pt","wt") <-- ("pt", partition*"wt").
   ! You can choose if you want the atom sizes to be taken account in
   ! the construction of the partiton.
   ! References: A. D. Becke, J. Chem. Phys. 88 (1988) 2547.
   !             O. Treutler and R. Ahlrichs, J. Chem. Phys. 102 (1995) 346.
      type(BECKE_GRID_TYPE), IN :: self
      VEC(REAL,:), INOUT :: wt
      MAT(REAL,:,:), IN :: pt
      INT, IN :: a
      VEC(INT,:), IN :: atoms
      VEC(BIN,:), OUT :: wt_is_0
      select case (self%partition_scheme)

      case ("becke            ")
         select case (self%partition_scaling_scheme)
         case ("none             "); call partition_B_no_adj_(self,wt,pt,a,atoms,wt_is_0)
         case ("becke            "); call partition_B_B_adj_(self,wt,pt,a,atoms,wt_is_0)
         case ("treutler_ahlrichs"); call partition_B_TA_adj_(self,wt,pt,a,atoms,wt_is_0)
         end select

      case ("stratmann_scuseria")
         select case (self%partition_scaling_scheme)
         case ("none"); call partition_SS_(self,wt,pt,a,atoms,wt_is_0)
         end select
      end select
   end subroutine

   PURE subroutine partition_B_no_adj(self,wt,pt,a,atoms,wt_is_0)
   ! Using Becke's partition function, partition the atomic grid ("pt","wt") for
   ! the atom "a" in the list of "atoms" so that the weights work with all the
   ! other atomic positions. Schematically:
   !         ("pt","wt") <-- ("pt", partition*"wt").
   ! In this version no atomic size adjustments is made.
   ! References: A. D. Becke, J. Chem. Phys. 88 (1988) 2547.
   !             O. Treutler and R. Ahlrichs, J. Chem. Phys. 102 (1995) 346.
      type(BECKE_GRID_TYPE), IN :: self
      VEC(REAL,:), INOUT :: wt
      MAT(REAL,:,:), IN :: pt
      INT, IN :: a
      VEC(INT,:), IN :: atoms
      VEC(BIN,:), OUT :: wt_is_0
      INT :: n,n_pts,n_centers,i,j,m
      VEC(REAL,3) :: rij,rni,rnj
      REAL :: dij,dni,dnj,uij,h,s
      VEC(REAL,3) :: posi,posj
      MAT(REAL,:,:), allocatable :: partition
   ENSURE(self%finalized,"BECKE_GRID:partition_B_no_adj ... call the set_grid_data routine")
   ENSURE(size(pt,2)==3,"BECKE_GRID:partition_B_no_adj ... pt incorrectly dimensioned")
   ENSURE(size(pt,1)==size(wt),"BECKE_GRID:partition_B_no_adj ... pt and wt incompatible")
   ENSURE(allocated(self%atom),"BECKE_GRID:partition_B_no_adj ... no atom list")
   ENSURE(is_in_range_(a,[1,size(self%atom)]),"BECKE_GRID:partition_B_no_adj ... `a' argument out of range")
      n_centers = size(atoms)  ! Number of integration (atom) centers
      n_pts = size(wt)  ! Number of points for this atom "a"
      call create_(partition,n_pts,n_centers)  ! Create partition array
      partition = ONE
      do i = 1,n_centers
         posi = self%atom(atoms(i))%position
         do j = 1,n_centers
            if (i==j) cycle
            posj = self%atom(atoms(j))%position
            rij = posi-posj
            dij = sqrt(dot_product(rij,rij))
            do n = 1,n_pts
               rni = pt(n,:)-posi
               rnj = pt(n,:)-posj
               dni = sqrt(dot_product(rni,rni))
               dnj = sqrt(dot_product(rnj,rnj))
               uij = (dni-dnj)/dij
               h = uij
               do m = 1,self%partition_power
                  h = 1.5d0*h-HALF*h*h*h
               end do
               s = HALF*(ONE-h)
                  ! ===========================================
               partition(n,i) = partition(n,i)*s
            end do
         end do
      end do

      ! Normalize the partitioning function and modify the grid weights
      call partition_wt_(self,wt,partition,a,wt_is_0)
      call destroy_(partition)
   end subroutine

   PURE subroutine partition_B_B_adj(self,wt,pt,a,atoms,wt_is_0)
   ! Using Becke's partition function, partition the atomic grid ("pt","wt") for
   ! the atom "a" in the list of "atoms" so that the weights work with all the
   ! other atomic positions. Schematically:
   !         ("pt","wt") <-- ("pt", partition*"wt").
   ! This version uses Becke's scheme for atomic size adjusments.
   ! References: A. D. Becke, J. Chem. Phys. 88 (1988) 2547.
   !             O. Treutler and R. Ahlrichs, J. Chem. Phys. 102 (1995) 346.
      type(BECKE_GRID_TYPE), IN :: self
      VEC(REAL,:), INOUT :: wt
      MAT(REAL,:,:), IN :: pt
      INT, IN :: a
      VEC(INT,:), IN :: atoms
      VEC(BIN,:), OUT :: wt_is_0
      INT :: n,n_pts,n_centers,i,j,ai,aj,m
      VEC(REAL,3) :: rij,rni,rnj
      REAL :: dij,dni,dnj,uij,vij,bsri,bsrj,chi,wij,aij,h,s
      VEC(REAL,3) :: posi,posj
      MAT(REAL,:,:), allocatable :: partition
   ENSURE(self%finalized,"BECKE_GRID:partition_B_B_adj ... call the set_grid_data routine")
   ENSURE(size(pt,2)==3,"BECKE_GRID:partition_B_B_adj ... pt incorrectly dimensioned")
   ENSURE(size(pt,1)==size(wt),"BECKE_GRID:partition_B_B_adj ... pt and wt incompatible")
   ENSURE(allocated(self%atom),"BECKE_GRID:partition_B_B_adj ... no atom list")
   ENSURE(is_in_range_(a,[1,size(self%atom)]),"BECKE_GRID:partition_B_B_adj ... `a' argument out of range")
      n_centers = size(atoms)  ! Number of integration (atom) centers
      n_pts = size(wt)  ! Number of points for this atom "a"
      call create_(partition,n_pts,n_centers)  ! Create partition array
      partition = ONE
      do i = 1,n_centers
         ai = atoms(i)
         bsri = bragg_slater_radius(self%atom(ai)%atomic_number)
         posi = self%atom(ai)%position
         do j = 1,n_centers
            if (i==j) cycle
            aj = atoms(j)
            bsrj = bragg_slater_radius(self%atom(aj)%atomic_number)
            chi = bsri/bsrj  ! Becke size adjustment
            wij = (chi-ONE)/(chi+ONE)
            aij = wij/(wij*wij-ONE)
            aij = min(aij,HALF)  ! Becke (A5): keep the boundary
            aij = max(aij,-HALF)  ! between the nuclei
            posj = self%atom(aj)%position
            rij = posi-posj
            dij = sqrt(dot_product(rij,rij))
            do n = 1,n_pts
               rni = pt(n,:)-posi
               rnj = pt(n,:)-posj
               dni = sqrt(dot_product(rni,rni))
               dnj = sqrt(dot_product(rnj,rnj))
               uij = (dni-dnj)/dij
               vij = uij+aij*(ONE-uij*uij)
               h = vij
               do m = 1,self%partition_power
                  h = 1.5d0*h-HALF*h*h*h
               end do
               s = HALF*(ONE-h)
                  ! ===========================================
               partition(n,i) = partition(n,i)*s
            end do
         end do
      end do

      ! Normalize the partitioning function and modify the grid weights
      call partition_wt_(self,wt,partition,a,wt_is_0)
      call destroy_(partition)
   end subroutine

   PURE subroutine partition_B_TA_adj(self,wt,pt,a,atoms,wt_is_0)
   ! Using Becke's partition function, partition the atomic grid ("pt","wt") for
   ! the atom "a" in the list of "atoms" so that the weights work with all the
   ! other atomic positions. Schematically:
   !         ("pt","wt") <-- ("pt", partition*"wt").
   ! This version uses Treutler and Ahlrichs scheme for atomic size adjusments.
   ! References: A. D. Becke, J. Chem. Phys. 88 (1988) 2547.
   !             O. Treutler and R. Ahlrichs, J. Chem. Phys. 102 (1995) 346.
      type(BECKE_GRID_TYPE), IN :: self
      VEC(REAL,:), INOUT :: wt
      MAT(REAL,:,:), IN :: pt
      INT, IN :: a
      VEC(INT,:), IN :: atoms
      VEC(BIN,:), OUT :: wt_is_0
      INT :: n,n_pts,n_centers,i,j,ai,aj,m
      VEC(REAL,3) :: rij,rni,rnj
      REAL :: dij,dni,dnj,uij,vij,bsri,bsrj,chi,wij,aij,h,s
      VEC(REAL,3) :: posi,posj
      MAT(REAL,:,:), allocatable :: partition
   ENSURE(self%finalized,"BECKE_GRID:partition_B_TA_adj ... call the set_grid_data routine")
   ENSURE(size(pt,2)==3,"BECKE_GRID:partition_B_TA_adj ... pt incorrectly dimensioned")
   ENSURE(size(pt,1)==size(wt),"BECKE_GRID:partition_B_TA_adj ... pt and wt incompatible")
   ENSURE(allocated(self%atom),"BECKE_GRID:partition_B_TA_adj ... no atom list")
   ENSURE(is_in_range_(a,[1,size(self%atom)]),"BECKE_GRID:partition_B_TA_adj ... `a' argument out of range")
      n_centers = size(atoms)  ! Number of integration (atom) centers
      n_pts = size(wt)  ! Number of points for this atom "a"
      call create_(partition,n_pts,n_centers)  ! Create partition array
      partition = ONE
      do i = 1,n_centers
         ai = atoms(i)
         bsri = bragg_slater_radius(self%atom(ai)%atomic_number)
         posi = self%atom(ai)%position
         do j = 1,n_centers
            if (i==j) cycle
            aj = atoms(j)
            bsrj = bragg_slater_radius(self%atom(aj)%atomic_number)
            chi = sqrt(bsri/bsrj)  ! Treutler-Ahlrcihs size adjustment
            wij = (chi-ONE)/(chi+ONE)
            aij = wij/(wij*wij-ONE)
            aij = min(aij,HALF)  ! Becke (A5): keep the boundary
            aij = max(aij,-HALF)  ! between the nuclei
            posj = self%atom(aj)%position
            rij = posi-posj
            dij = sqrt(dot_product(rij,rij))
            do n = 1,n_pts
               rni = pt(n,:)-posi
               rnj = pt(n,:)-posj
               dni = sqrt(dot_product(rni,rni))
               dnj = sqrt(dot_product(rnj,rnj))
               uij = (dni-dnj)/dij
               vij = uij+aij*(ONE-uij*uij)
               h = vij
               do m = 1,self%partition_power
                  h = 1.5d0*h-HALF*h*h*h
               end do
               s = HALF*(ONE-h)
                  ! ===========================================
               partition(n,i) = partition(n,i)*s
            end do
         end do
      end do

      ! Normalize the partitioning function and modify the grid weights
      call partition_wt_(self,wt,partition,a,wt_is_0)
      call destroy_(partition)
   end subroutine


!  Normalize the weights

   PURE subroutine partition_wt(self,wt,partition,a,wt_is_0)
   ! Partition the weights "wt" using the unnormalized "partition" for atom "a".
   ! If present the zero weights are recorded in "wt_is_0".
      type(BECKE_GRID_TYPE), IN :: self
      VEC(REAL,:), INOUT :: wt
      MAT(REAL,:,:), IN :: partition
      INT, IN :: a
      VEC(BIN,:), OUT :: wt_is_0
      INT :: n
      REAL :: s
      wt_is_0 = FALSE
      do n = 1,size(wt)
         s = sum(partition(n,:))
         s = partition(n,a)/s
         wt(n) = wt(n)*s
         if (s>self%partition_cutoff) cycle
         wt_is_0(n) = TRUE
      end do
   end subroutine


!  SS partition accounts for zero weights

   PURE subroutine partition_SS(self,wt,pt,a,atoms,wt_is_0)
   ! Using Stratmann and Scuseria's partition function, partition the
   ! atomic grid ("pt","wt") for the atom "a" in the list of "atoms"
   ! so that the weights work with all the other atomic positions
   ! given in "atoms". Schematically:
   !         ("pt","wt") <-- ("pt", partition*"wt").
   ! NOTE: the "pt" and "wt" are those for atom "atoms(a)" only.
   ! NOTE: In this version no atomic size adjustments is made.
   ! References: R.E. Stratmann, G E. Scuseria and M.E. Frisch, 1996 CPL 257 p. 213
      type(BECKE_GRID_TYPE), IN :: self
      VEC(REAL,:), INOUT :: wt
      MAT(REAL,:,:), IN :: pt
      INT, IN :: a
      VEC(INT,:), IN :: atoms
      VEC(BIN,:), OUT :: wt_is_0
      if (size(atoms)==2) then; call partition_SS_pair_(self,wt,pt,a,atoms,wt_is_0)
      else; call partition_SS_gnrl_(self,wt,pt,a,atoms,wt_is_0)
      end if
   end subroutine

   PURE subroutine partition_SS_pair(self,wt,pt,a,atoms,wt_is_0)
   ! Using Stratmann and Scuseria's partition function, partition the
   ! atomic grid ("pt","wt") for the atom "a" in the list of "atoms"
   ! so that the weights work with all the other atomic positions
   ! given in "atoms". Schematically:
   !         ("pt","wt") <-- ("pt", partition*"wt").
   ! NOTE: the "pt" and "wt" are those for atom "atoms(a)" only.
   ! NOTE: In this version no atomic size adjustments is made.
   ! References: R.E. Stratmann, G E. Scuseria and M.E. Frisch, 1996 CPL 257 p. 213
      type(BECKE_GRID_TYPE), IN :: self
      VEC(REAL,:), INOUT :: wt
      MAT(REAL,:,:), IN :: pt
      INT, IN :: a
      VEC(INT,:), IN :: atoms
      VEC(BIN,:), OUT :: wt_is_0
      INT :: n_centers,n_pts,i,j,k,n,m,ai,aj
      VEC(REAL,3) :: rij,rni,rnj
      REAL :: r_min,r_max,dij,dni,dnj,uij,uij1,uij2,uij3,uij5,uij7,h,s,aa,c5,c21,c35
      VEC(REAL,3) :: posi,posj
      MAT(REAL,:,:), allocatable :: partition
      VEC(BIN,:), allocatable :: wt_is_1
      VEC(INT,:), allocatable :: list
   ENSURE(self%finalized,"BECKE_GRID:partition_SS_pair ... call the set_grid_data routine")
   ENSURE(size(pt,2)==3,"BECKE_GRID:partition_SS_pair ... pt incorrectly dimensioned")
   ENSURE(size(pt,1)==size(wt),"BECKE_GRID:partition_SS_pair ... pt and wt incompatible")
   ENSURE(allocated(self%atom),"BECKE_GRID:partition_SS_pair ... no atom list")
   ENSURE(is_in_range_(a,[1,size(self%atom)]),"BECKE_GRID:partition_SS_pair ... `a' argument out of range")
      aa = self%stratmann_scuseria_A
      c5 = FIVE
      c21 = SEVEN*THREE
      c35 = SEVEN*FIVE
      n_centers = size(atoms)  ! Number of integration (atom) centers
      n_pts = size(wt)  ! Number of points for this atom "a"
      call create_(wt_is_1,n_pts)  ! Tells where weights are 1 or zero
      wt_is_1 = FALSE  ! ... Do this when making atom grids!
      i = a
      ai = atoms(i)
      posi = self%atom(ai)%position
      r_min = self%atom_min_distance(ai)
      do n = 1,n_pts  ! Predetermine which weights are one
         rni = pt(n,:)-posi
         dni = sqrt(dot_product(rni,rni))
         if (dni>HALF*(ONE-aa)*r_min) cycle
         wt_is_1(n) = TRUE
      end do
      k = 0  ! Make the list of non-unit weights
      call create_(list,count(NOT wt_is_1))
      do n = 1,n_pts
         if (wt_is_1(n)) cycle
         k = k+1
         list(k) = n
      end do
      call create_(partition,n_pts,n_centers)
      partition = ONE
      wt_is_0 = FALSE
      i = a  ! Do atom "a" partition & set wt_is_0
      ai = atoms(i)
      posi = self%atom(ai)%position
      do j = 1,n_centers
         if (i==j) cycle
         aj = atoms(j)
         posj = self%atom(aj)%position
         r_max = r_max_for_atom_(self,aj)
         r_max = r_max*r_max  ! Square it to save square roots
         rij = posi-posj
         dij = sqrt(dot_product(rij,rij))
         do m = 1,size(list)  ! Loop over non-unit weight points
            n = list(m)
            rni = pt(n,:)-posi
            rnj = pt(n,:)-posj
            dni = dot_product(rni,rni)
            dnj = dot_product(rnj,rnj)
            if (dnj>r_max) then  ! WARNING: only valid for basis function on aj
               partition(n,i) = ZERO
               wt_is_0(n) = TRUE
               wt(n) = ZERO  ! <<< This is zero by cutoff ...
            else
               dni = sqrt(dni)
               dnj = sqrt(dnj)
               uij = (dni-dnj)/dij
               if (uij>aa) then
                  partition(n,i) = ZERO
                  wt_is_0(n) = TRUE
                  wt(n) = ZERO  ! <<< This is rigorously zero ...
               else if (uij>-aa) then
                  uij1 = uij/aa
                  uij2 = uij1*uij1
                  uij3 = uij2*uij1
                  uij5 = uij3*uij2
                  uij7 = uij5*uij2
                  h = SIXTEENTH*(c35*uij1-c35*uij3+c21*uij5-c5*uij7)
                  s = HALF*(ONE-h)
                  partition(n,i) = partition(n,i)*s
               end if
            end if
         end do
      end do
      k = 0  ! Make the list of non-unit & non-zero weights
      call destroy_(list)
      call create_(list,count(NOT (wt_is_1 OR wt_is_0)))
      do n = 1,n_pts
         if (wt_is_1(n) OR wt_is_0(n)) cycle
         k = k+1
         list(k) = n
      end do
      call destroy_(wt_is_1)
      do i = 1,n_centers  ! Now partition those non-unit weights
         if (i==a) cycle  ! Do atoms /= a ... for normalisation
         ai = atoms(i)
         posi = self%atom(ai)%position
         r_min = self%atom_min_distance(ai)
         do j = 1,n_centers
            if (i==j) cycle
            posj = self%atom(atoms(j))%position
            rij = posi-posj
            dij = sqrt(dot_product(rij,rij))
            do m = 1,size(list)  ! Loop over non-unit non-zero weight points
               n = list(m)
               rni = pt(n,:)-posi
               rnj = pt(n,:)-posj
               dni = sqrt(dot_product(rni,rni))
               dnj = sqrt(dot_product(rnj,rnj))
               uij = (dni-dnj)/dij
               if (uij>aa) then
                  partition(n,i) = ZERO
               else if (uij>-aa) then
                  uij1 = uij/aa
                  uij2 = uij1*uij1
                  uij3 = uij2*uij1
                  uij5 = uij3*uij2
                  uij7 = uij5*uij2
                  h = SIXTEENTH*(c35*uij1-c35*uij3+c21*uij5-c5*uij7)
                  s = HALF*(ONE-h)
                  partition(n,i) = partition(n,i)*s
               end if
            end do
         end do
      end do

      ! Normalize the partitioning function and modify the grid weights
      do m = 1,size(list)
         n = list(m)
         s = sum(partition(n,:))
         s = partition(n,a)/s
         wt(n) = wt(n)*s
         if (s<=self%partition_cutoff) wt_is_0(n) = TRUE
      end do
      call destroy_(partition)
      call destroy_(list)
   end subroutine

   PURE subroutine partition_SS_gnrl(self,wt,pt,a,atoms,wt_is_0)
   ! Using Stratmann and Scuseria's partition function, partition the
   ! atomic grid ("pt","wt") for the atom "a" in the list of "atoms"
   ! so that the weights work with all the other atomic positions
   ! given in "atoms". Schematically:
   !         ("pt","wt") <-- ("pt", partition*"wt").
   ! NOTE: the "pt" and "wt" are those for atom "atoms(a)" only.
   ! NOTE: In this version no atomic size adjustments is made.
   ! References: R.E. Stratmann, G E. Scuseria and M.E. Frisch, 1996 CPL 257 p. 213
      type(BECKE_GRID_TYPE), IN :: self
      VEC(REAL,:), INOUT :: wt
      MAT(REAL,:,:), IN :: pt
      INT, IN :: a
      VEC(INT,:), IN :: atoms
      VEC(BIN,:), OUT :: wt_is_0
      INT :: n_centers,n_pts,i,j,k,n,m,ai,aj
      VEC(REAL,3) :: rij,rni,rnj
      REAL :: r_min,r_max,dij,dni,dnj,uij,uij1,uij2,uij3,uij5,uij7,h,s,aa,c5,c21,c35
      VEC(REAL,3) :: posi,posj
      MAT(REAL,:,:), allocatable :: partition
      VEC(BIN,:), allocatable :: wt_is_1
      VEC(INT,:), allocatable :: list
   ENSURE(self%finalized,"BECKE_GRID:partition_SS_gnrl ... call the set_grid_data routine")
   ENSURE(size(pt,2)==3,"BECKE_GRID:partition_SS_gnrl ... pt incorrectly dimensioned")
   ENSURE(size(pt,1)==size(wt),"BECKE_GRID:partition_SS_gnrl ... pt and wt incompatible")
   ENSURE(allocated(self%atom),"BECKE_GRID:partition_SS_gnrl ... no atom list")
   ENSURE(is_in_range_(a,[1,size(self%atom)]),"BECKE_GRID:partition_SS_gnrl ... `a' argument out of range")
      aa = self%stratmann_scuseria_A
      c5 = FIVE
      c21 = SEVEN*THREE
      c35 = SEVEN*FIVE

      ! Number of integration (atom) centers
      n_centers = size(atoms)

      ! Number of points for this atom "a"
      n_pts = size(wt)

      ! Tells where weights are 1 or zero
      ! ... Do this when making atom grids!
      call create_(wt_is_1,n_pts)
      wt_is_1 = FALSE
      i = a
      ai = atoms(i)
      posi = self%atom(ai)%position
      r_min = self%atom_min_distance(ai)

         ! Predetermine which weights are one
      do n = 1,n_pts
         rni = pt(n,:)-posi
         dni = sqrt(dot_product(rni,rni))
         if (dni>HALF*(ONE-aa)*r_min) cycle
         wt_is_1(n) = TRUE
      end do

      ! Make the list of non-unit weights
      k = 0
      call create_(list,count(NOT wt_is_1))
      do n = 1,n_pts
         if (wt_is_1(n)) cycle
         k = k+1
         list(k) = n
      end do
      call create_(partition,n_pts,n_centers)
      partition = ONE
      wt_is_0 = FALSE

         ! Do atom "a" partition & set wt_is_0
      i = a
      ai = atoms(i)
      posi = self%atom(ai)%position
      do j = 1,n_centers
         if (i==j) cycle
         aj = atoms(j)
         posj = self%atom(aj)%position
         r_max = r_max_for_atom_(self,aj)
         r_max = r_max*r_max  ! Square it to save square roots
         rij = posi-posj
         dij = sqrt(dot_product(rij,rij))

               ! Loop over non-unit weight points
         do m = 1,size(list)
            n = list(m)
            rni = pt(n,:)-posi
            rnj = pt(n,:)-posj
            dni = dot_product(rni,rni)
            dnj = dot_product(rnj,rnj)
               ! if (dnj>r_max) then   ! WARNING: only valid for basis function on aj
               !    partition(n,i) = ZERO
               !    wt_is_0(n) = TRUE
               !    wt(n) = ZERO       ! <<< This is zero by cutoff ...
               ! else
            dni = sqrt(dni)
            dnj = sqrt(dnj)
            uij = (dni-dnj)/dij
            if (uij>aa) then
               partition(n,i) = ZERO
               wt_is_0(n) = TRUE
               wt(n) = ZERO  ! <<< This is rigorously zero ...
            else if (uij>-aa) then
               uij1 = uij/aa
               uij2 = uij1*uij1
               uij3 = uij2*uij1
               uij5 = uij3*uij2
               uij7 = uij5*uij2
               h = SIXTEENTH*(c35*uij1-c35*uij3+c21*uij5-c5*uij7)
               s = HALF*(ONE-h)
               partition(n,i) = partition(n,i)*s
            end if
         end do
      end do

      ! Make the list of non-unit & non-zero weights
      k = 0
      call destroy_(list)
      call create_(list,count(NOT (wt_is_1 OR wt_is_0)))
      do n = 1,n_pts
         if (wt_is_1(n) OR wt_is_0(n)) cycle
         k = k+1
         list(k) = n
      end do
      call destroy_(wt_is_1)

      ! Now partition those non-unit weights
      do i = 1,n_centers
         if (i==a) cycle

         ! Do atoms /= a ... for normalisation
         ai = atoms(i)
         posi = self%atom(ai)%position
         r_min = self%atom_min_distance(ai)
         do j = 1,n_centers
            if (i==j) cycle
            posj = self%atom(atoms(j))%position
            rij = posi-posj
            dij = sqrt(dot_product(rij,rij))

               ! Loop over non-unit non-zero weight points
            do m = 1,size(list)
               n = list(m)
               rni = pt(n,:)-posi
               rnj = pt(n,:)-posj
               dni = sqrt(dot_product(rni,rni))
               dnj = sqrt(dot_product(rnj,rnj))
               uij = (dni-dnj)/dij
               if (uij>aa) then
                  partition(n,i) = ZERO
               else if (uij>-aa) then
                  uij1 = uij/aa
                  uij2 = uij1*uij1
                  uij3 = uij2*uij1
                  uij5 = uij3*uij2
                  uij7 = uij5*uij2
                  h = SIXTEENTH*(c35*uij1-c35*uij3+c21*uij5-c5*uij7)
                  s = HALF*(ONE-h)
                  partition(n,i) = partition(n,i)*s
               end if
            end do
         end do
      end do

      ! Normalize the partitioning function and modify the grid weights
      do m = 1,size(list)
         n = list(m)
         s = sum(partition(n,:))
         s = partition(n,a)/s
         wt(n) = wt(n)*s
         if (s<=self%partition_cutoff) wt_is_0(n) = TRUE
      end do
      call destroy_(partition)
      call destroy_(list)
   end subroutine


!  =======================================
!  Basis function grid generation routines
!  =======================================

   PURE subroutine make_bf_grids_0(self,a0,b0,ma,mb,pt,atoms)
   ! Make the basis function grid and basis function gradient grid, "a0", "a1"
   ! and "b0", "b1" respectively, on the set of points "pt", for a pair of
   ! "atoms" (a,b) in the complete atom list. NOTE: remember *not* to destroy
   ! "b0" and "b1" sometimes. NOTE: .wt_is_0 is destroyed at the end of
   ! this routine, since I assume that this expensive routine is called only
   ! once for a particular pair of atoms. "ma" amd "mb" are the number of
   ! non-zero points on atoms a and b in "atoms". NOTE: "mb" may not be defined!
      type(BECKE_GRID_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable, OUT :: a0,b0
      INT, OUT :: ma,mb
      MAT(REAL,:,:), IN :: pt
      VEC(INT,:), IN :: atoms
      INT :: a,b,ua,ub,na,nb,n,fa,fb,i,k,m
   ENSURE(self%finalized,"BECKE_GRID:make_bf_grids_0 ... radial and angular grids are unmade; use set_grid_data")
   ENSURE(allocated(self%atom),"BECKE_GRID:make_bf_grids_0 ... no atom list array")
   ENSURE(allocated(self%atom_grid0),"BECKE_GRID:make_bf_grids_0 ... no atom_grid0 array")
   ENSURE(size(atoms)<=2,"BECKE_GRID:make_bf_grids_0 ... only valid for at most two atoms")
      if (size(atoms)==1) then
         a = atoms(1)
         ua = self%unique_atom_for(a)
         a0 = self%atom_grid0(ua)%element  ! Leaky
         b0 = a0
         ma = size(a0,1)
         mb = 0
      else
         a = atoms(1)
         b = atoms(2)
         ua = self%unique_atom_for(a)
         ub = self%unique_atom_for(b)
         na = size(self%atom_grid0(ua)%element,1)
         nb = size(self%atom_grid0(ub)%element,1)
         fa = size(self%atom_grid0(ua)%element,2)
         fb = size(self%atom_grid0(ub)%element,2)
         n = na+nb

         ! WARNING: be careful here
         if (NOT allocated(self%wt_is_0)) then

            ! Leaky
            call create_(a0,n,fa)
            call create_(b0,n,fb)
            a0(1:na,:) = self%atom_grid0(ua)%element  ! a's basis functions on a's points
            b0(na+1:n,:) = self%atom_grid0(ub)%element  ! b's basis functions on b's points
            call make_bf_grid_(self%atom(a),a0(na+1:n,:),pt(na+1:n,:),self%rho_cutoff)  ! a's basis functions on b's point
            call make_bf_grid_(self%atom(b),b0(1:na,:),pt(1:na,:),self%rho_cutoff)  ! b's basis functions on a's points
         else
            m = count(NOT self%wt_is_0)
            ENSURE(size(pt,1)==m,"BECKE_GRID:make_bf_grids_0 ... wrong size for pt array")

            ! Leaky
            call create_(a0,m,fa)
            call create_(b0,m,fb)

            ! a's basis functions on a's points
            k = 0
            do i = 1,na
               if (self%wt_is_0(i)) cycle
               k = k+1
               a0(k,:) = self%atom_grid0(ua)%element(i,:)
            end do

            ! b's basis functions on b's points
            ma = k
            mb = m-ma
            do i = 1,nb
               if (self%wt_is_0(na+i)) cycle
               k = k+1
               b0(k,:) = self%atom_grid0(ub)%element(i,:)
            end do

            ! a's basis functions on b's point
            call make_bf_grid_(self%atom(a),a0(ma+1:m,:),pt(ma+1:m,:),self%rho_cutoff)

            ! b's basis functions on a's points
            call make_bf_grid_(self%atom(b),b0(1:ma,:),pt(1:ma,:),self%rho_cutoff)

            ! <<< Destroy .wt_is_0
            call destroy_(self%wt_is_0)
         end if
      end if
   end subroutine

   PURE subroutine make_bf_grids_1(self,a1,b1,ma,mb,pt,atoms)
   ! Make the basis function *gradient* grids, "a1" and "b1"
   ! respectively, on the set of points "pt", for a pair of "atoms"
   ! (a,b) in the complete atom list.  "ma" and "mb" are the number of
   ! non-zero points on atoms a and b in "atoms".
   ! NOTE: remember *not* to destroy "b1" sometimes.
   ! NOTE: "mb" may not be defined!
      type(BECKE_GRID_TYPE), IN :: self
      MAT3(REAL,:,:,:), allocatable, OUT :: a1,b1
      INT, OUT :: ma,mb
      MAT(REAL,:,:), IN :: pt
      VEC(INT,:), IN :: atoms
      INT :: a,b,ua,ub,na,nb,n,fa,fb,i,k,m
   ENSURE(self%finalized,"BECKE_GRID:make_bf_grids_1 ... radial and angular grids are unmade; use set_grid_data")
   ENSURE(allocated(self%atom),"BECKE_GRID:make_bf_grids_1 ... no atom list array")
   ENSURE(allocated(self%atom_grid1),"BECKE_GRID:make_bf_grids_1 ... no atom_grid1 array")
   ENSURE(size(atoms)<=2,"BECKE_GRID:make_bf_grids_1 ... only valid for at most two atoms")
      if (size(atoms)==1) then
         a = atoms(1)
         ua = self%unique_atom_for(a)
         a1 = self%atom_grid1(ua)%element
         b1 = a1
         ma = size(a1,1)
         mb = 0
      else
         a = atoms(1)
         b = atoms(2)
         ua = self%unique_atom_for(a)
         ub = self%unique_atom_for(b)
         na = size(self%atom_grid1(ua)%element,1)
         nb = size(self%atom_grid1(ub)%element,1)
         fa = size(self%atom_grid1(ua)%element,2)
         fb = size(self%atom_grid1(ub)%element,2)
         n = na+nb
         if (NOT allocated(self%wt_is_0)) then  ! WARNING: be careful here
            call create_(a1,n,fa,3)  ! Leaky
            call create_(b1,n,fb,3)
            a1(1:na,:,:) = self%atom_grid1(ua)%element
            b1(na+1:n,:,:) = self%atom_grid1(ub)%element
            call make_nabla_bf_grid_(self%atom(a),a1(na+1:n,:,:),pt(na+1:n,:),self%rho_cutoff)  ! a's basis functions on b's point
            call make_nabla_bf_grid_(self%atom(b),b1(1:na,:,:),pt(1:na,:),self%rho_cutoff)  ! b's basis functions on a's points
         else
            m = count(NOT self%wt_is_0)
            ENSURE(size(pt,1)==m,"BECKE_GRID:make_bf_grids_1 ... wrong size for pt array")
            call create_(a1,m,fa,3)  ! Leaky
            call create_(b1,m,fb,3)
            k = 0
            do i = 1,na
               if (self%wt_is_0(i)) cycle
               k = k+1
               a1(k,:,:) = self%atom_grid1(ua)%element(i,:,:)  ! a's basis functions on a's points
            end do
            ma = k
            mb = m-ma
            do i = 1,nb
               if (self%wt_is_0(na+i)) cycle
               k = k+1
               b1(k,:,:) = self%atom_grid1(ub)%element(i,:,:)  ! b's basis functions on b's points
            end do
            call make_nabla_bf_grid_(self%atom(a),a1(ma+1:m,:,:),pt(ma+1:m,:),self%rho_cutoff)  ! a's basis functions on b's point
            call make_nabla_bf_grid_(self%atom(b),b1(1:ma,:,:),pt(1:ma,:),self%rho_cutoff)  ! b's basis functions on a's points
         ! .wt_is_0.destroy                              ! <<< Destroy .wt_is_0
         end if
      end if
   end subroutine

   PURE subroutine make_bf_grids_2(self,a0,a1,b0,b1,ma,mb,pt,atoms)
   ! Make the basis function grid and basis function gradient grid, "a0", "a1"
   ! and "b0", "b1" respectively, on the set of points "pt", for a pair of
   ! "atoms" (a,b) in the complete atom list. NOTE: remember *not* to destroy
   ! "b0" and "b1" sometimes. NOTE: .wt_is_0 is destroyed at the end of this
   ! routine, since I assume that this expensive routine is called only once for
   ! a particular pair of atoms. "ma" amd "mb" are the number of non-zero points
   ! on atoms a and b in "atoms". NOTE: "mb" may not be defined!
      type(BECKE_GRID_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable, OUT :: a0,b0
      MAT3(REAL,:,:,:), allocatable, OUT :: a1,b1
      INT, OUT :: ma,mb
      MAT(REAL,:,:), IN :: pt
      VEC(INT,:), IN :: atoms
      INT :: a,b,ua,ub,na,nb,n,fa,fb,i,k,m
   ENSURE(self%finalized,"BECKE_GRID:make_bf_grids_2 ... radial and angular grids are unmade; use set_grid_data")
   ENSURE(allocated(self%atom),"BECKE_GRID:make_bf_grids_2 ... no atom list array")
   ENSURE(allocated(self%atom_grid0),"BECKE_GRID:make_bf_grids_2 ... no atom_grid0 array")
   ENSURE(allocated(self%atom_grid1),"BECKE_GRID:make_bf_grids_2 ... no atom_grid1 array")
   ENSURE(size(atoms)<=2,"BECKE_GRID:make_bf_grids_2 ... only valid for at most two atoms")
      if (size(atoms)==1) then
         a = atoms(1)
         ua = self%unique_atom_for(a)
         a0 = self%atom_grid0(ua)%element  ! Leaky
         a1 = self%atom_grid1(ua)%element
         b0 = a0
         b1 = a1
         ma = size(a0,1)
         mb = 0
      else
         a = atoms(1)
         b = atoms(2)
         ua = self%unique_atom_for(a)
         ub = self%unique_atom_for(b)
         na = size(self%atom_grid0(ua)%element,1)
         nb = size(self%atom_grid0(ub)%element,1)
         fa = size(self%atom_grid0(ua)%element,2)
         fb = size(self%atom_grid0(ub)%element,2)
         n = na+nb
         if (NOT allocated(self%wt_is_0)) then  ! WARNING: be careful here
            call create_(a0,n,fa); call create_(a1,n,fa,3)  ! Leaky
            call create_(b0,n,fb); call create_(b1,n,fb,3)
            a0(1:na,:) = self%atom_grid0(ua)%element  ! a's basis functions on a's points
            a1(1:na,:,:) = self%atom_grid1(ua)%element
            b0(na+1:n,:) = self%atom_grid0(ub)%element  ! b's basis functions on b's points
            b1(na+1:n,:,:) = self%atom_grid1(ub)%element
            call make_nabla_bf_grid_(self%atom(a),a1(na+1:n,:,:),a0(na+1:n,:),pt(na+1:n,:),self%rho_cutoff)  ! a's basis functions on b's point
            call make_nabla_bf_grid_(self%atom(b),b1(1:na,:,:),b0(1:na,:),pt(1:na,:),self%rho_cutoff)  ! b's basis functions on a's points
         else
            m = count(NOT self%wt_is_0)
            ENSURE(size(pt,1)==m,"BECKE_GRID:make_bf_grids_2 ... wrong size for pt array")
            call create_(a0,m,fa); call create_(a1,m,fa,3)  ! Leaky
            call create_(b0,m,fb); call create_(b1,m,fb,3)
            k = 0
            do i = 1,na
               if (self%wt_is_0(i)) cycle
               k = k+1
               a0(k,:) = self%atom_grid0(ua)%element(i,:)  ! a's basis functions on a's points
               a1(k,:,:) = self%atom_grid1(ua)%element(i,:,:)
            end do
            ma = k
            mb = m-ma
            do i = 1,nb
               if (self%wt_is_0(na+i)) cycle
               k = k+1
               b0(k,:) = self%atom_grid0(ub)%element(i,:)  ! b's basis functions on b's points
               b1(k,:,:) = self%atom_grid1(ub)%element(i,:,:)
            end do
            call make_nabla_bf_grid_(self%atom(a),a1(ma+1:m,:,:),a0(ma+1:m,:),pt(ma+1:m,:),self%rho_cutoff)  ! a's basis functions on b's point
            call make_nabla_bf_grid_(self%atom(b),b1(1:ma,:,:),b0(1:ma,:),pt(1:ma,:),self%rho_cutoff)  ! b's basis functions on a's points
            call destroy_(self%wt_is_0)  ! <<< Destroy .wt_is_0
         end if
      end if
   end subroutine


!  =====================
!  Numerical integration
!  =====================

!   integrate_function(f,ans)
!   ! Integrate the function "f" which is sharply peaked at positions "pos" in 3D
!   ! space, where the peaks resemble those in the electron density of atoms with
!   ! charge "Z". The integrand is returned as "ans".
!      self :: INOUT
!      interface
!         f(f_grid,pts)
!            f_grid :: VEC{REAL}, OUT
!            pts :: MAT{REAL}, IN
!         end
!      end
!      ans :: REAL
!
!   ENSURE(.finalized,"call the set_grid_data routine")
!
!      n_pt :: INT
!      pt :: MAT{REAL}@
!      wt,f_grid :: VEC{REAL}@
!
!      n_pt = .no_of_points
!      f_grid.create(n_pt)
!      .make_grid(pt,wt) ! leaky
!
!
!      ! Do the integration
!      call f(f_grid,pt)
!      ans = sum(f_grid*wt)
!
!      wt.destroy
!      pt.destroy
!      f_grid.destroy
!
!
!   end

!  ======
!  Output
!  ======

   subroutine put(self)
   ! Put out to file "out"
      type(BECKE_GRID_TYPE), IN :: self
      call put_basics_(self)
   end subroutine

   subroutine put_all(self)
   ! Put out to file "out"
      type(BECKE_GRID_TYPE), IN :: self
      call put_basics_(self)
      call put_atom_info_(self)
      call put_radial_angular_grids_(self)
      call put_unique_atom_grids_(self)
   end subroutine

   subroutine put_basics(self)
   ! Put out to file "out"
      type(BECKE_GRID_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"======================")
      call text_(stdout,"Becke integration grid")
      call text_(stdout,"======================")
      call flush_(stdout)
      call show_(stdout,"Kind of grid              =",self%kind)
      call show_(stdout,"Accuracy                  =",self%accuracy)
      call show_(stdout,"Pruning scheme            =",self%pruning_scheme)
      call show_(stdout,"Partition scheme          =",self%partition_scheme)
      call show_(stdout,"Partition scaling scheme  =",self%partition_scaling_scheme)
      call show_(stdout,"Partition cutoff          =",self%partition_cutoff)
      call show_(stdout,"Basis function cutoff     =",self%basis_fn_cutoff)
      call show_(stdout,"XC batch max points       =",self%XC_batch_max_pts)
      call show_(stdout,"XC batch box edge         =",self%XC_batch_box_edge)
      call show_(stdout,"Rho cutoff                =",self%rho_cutoff)
      call show_(stdout,"Becke partition_power     =",self%partition_power)
      call show_(stdout,"Scale atomic grids?       =",self%scale_atomic_grids)
      if (allocated(self%radial_grid) AND allocated(self%lebedev_grid)) then
         call show_(stdout,"# of points (row=1)       =",no_of_pts_for_row_(self,1))
         call show_(stdout,"# of points (row=2)       =",no_of_pts_for_row_(self,2))
         call show_(stdout,"# of points (row=3)       =",no_of_pts_for_row_(self,3))
      end if
      call flush_(stdout)
      call text_(stdout,"Radial grid:")
      call flush_(stdout)
      call show_(stdout,"n_radial_pts (for H)      =",self%n_radial_pts)
      call show_(stdout,"n_extra_pts_per_shell     =",self%n_extra_pts_per_shell)
      call flush_(stdout)
      call text_(stdout,"Angular grid:")
      call flush_(stdout)
      call show_(stdout,"l_angular_grid            =",self%l_angular_grid)
      call show_(stdout,"l_H_angular_grid          =",self%l_H_angular_grid)
      if (self%pruning_scheme=="adaptive") then
         call show_(stdout,"l_bonding_angular_grid    =",self%l_bonding_angular_grid)
      end if
      call show_(stdout,"reduce_H_angular_grid?    =",self%reduce_H_angular_grid)
   end subroutine

   subroutine put_atom_info(self)
   ! Put out the atom info to file "out"
      type(BECKE_GRID_TYPE), IN :: self
      INT :: i,Z,np
      INT :: fields
   ENSURE(allocated(self%atom),"BECKE_GRID:put_atom_info ... no atom list")
      if (NOT self%finalized) return
      fields = 5
      call flush_(stdout)
      call show_(stdout,"Total n_points            =",no_of_points_(self))
      call flush_(stdout)
      call text_(stdout,"Atom center information:")
      call flush_(stdout)
      call dash_(stdout,int_fields=fields+1,real_fields=fields)
      call put_(stdout,"Atom #",int_width=TRUE)
      call put_(stdout,"Uniq #",int_width=TRUE)
      call put_(stdout,"Z",int_width=TRUE)
      call put_(stdout,"Row",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call put_(stdout,"Scale factor")
      call put_(stdout,"Min exponent")
      call put_(stdout,"n_pts",int_width=TRUE)
      call put_(stdout,"n_max",int_width=TRUE)
      call flush_(stdout)
      call dash_(stdout,int_fields=fields+1,real_fields=fields)
      do i = 1,size(self%atom)
         call put_(stdout,i)
         call put_(stdout,self%unique_atom_for(i))
         Z = self%atom(i)%atomic_number
         call put_(stdout,Z)
         call put_(stdout,period(Z))
         call put_(stdout,self%atom(i)%position(1))
         call put_(stdout,self%atom(i)%position(2))
         call put_(stdout,self%atom(i)%position(3))
         call put_(stdout,self%atom_zeta(Z))
         np = no_of_pts_for_atom_(self,i)
         call put_(stdout,minimum_basis_exponent_(self%atom(i)))
         call put_(stdout,np)
         call put_(stdout,self%n_pts_for_row(period_of_(self,i)))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=fields+1,real_fields=fields)
   end subroutine

   subroutine put_radial_angular_grids(self)
   ! Put out the radial and angular grid info to file "out"
      type(BECKE_GRID_TYPE), IN :: self
      INT :: i,p
      if (NOT self%finalized) return
      do p = 1,1
         call flush_(stdout)
         call show_(stdout,"Radial grid, period = ",p)
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=2)
         call put_(stdout,"pt #",int_width=TRUE)
         call put_(stdout,"pt")
         call put_(stdout,"wt")
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=2)
         do i = 1,self%radial_grid(p)%n_pts
            call put_(stdout,i)
            call put_(stdout,self%radial_grid(p)%pts(i))
            call put_(stdout,self%radial_grid(p)%wts(i))
            call flush_(stdout)
         end do
      end do
      call flush_(stdout)
      call text_(stdout,"Angular grid:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      call put_(stdout,"pt #",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call put_(stdout,"wt")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      do i = 1,self%lebedev_grid%n_pts
         call put_(stdout,i)
         call put_(stdout,self%lebedev_grid%pts(i,1))
         call put_(stdout,self%lebedev_grid%pts(i,2))
         call put_(stdout,self%lebedev_grid%pts(i,3))
         call put_(stdout,self%lebedev_grid%wts(i))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=4)
   end subroutine

   subroutine put_unique_atom_grids(self)
   ! Put out the unique atom grid info to file "out"
      type(BECKE_GRID_TYPE), IN :: self
      INT :: u,a,p,np
   ENSURE(allocated(self%unique_atom),"BECKE_GRID:put_unique_atom_grids ... no .unique_atom")
   ENSURE(allocated(self%atom_pts),"BECKE_GRID:put_unique_atom_grids ... no .atom_pts")
   ENSURE(allocated(self%atom_wts),"BECKE_GRID:put_unique_atom_grids ... no .atom_wts")
      if (NOT self%finalized) return
      call flush_(stdout)
      call text_(stdout,"Atom grids:")
      call flush_(stdout)
      call show_(stdout,"No of atom grids          =",self%n_unique_atoms)
      call flush_(stdout)
      do u = 1,self%n_unique_atoms
         a = self%unique_atom(u)
         np = size(self%atom_wts(u)%element)
         p = period_of_(self,a)
         call flush_(stdout)
         call show_(stdout,"Unique atom number        =",u)
         call show_(stdout,"Atom number               =",a)
         call show_(stdout,"Atomic number             =",self%atom(a)%atomic_number)
         call show_(stdout,"No of points              =",np)
         call show_(stdout,"Max no of points for row  =",self%n_pts_for_row(p))
         call flush_(stdout)
         call text_(stdout,"Grid points:")
         call put_(stdout,self%atom_pts(u)%element)
         call flush_(stdout)
         call text_(stdout,"Grid weights:")
         call put_(stdout,self%atom_wts(u)%element)
         call text_(stdout,"Atom densities:")
         call put_(stdout,self%atom_grid0(u)%element)
         call text_(stdout,"Atom density gradients:")
         call put_(stdout,self%atom_grid1(u)%element)
      end do
   end subroutine

end module
