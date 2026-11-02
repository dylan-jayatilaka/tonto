!=====================================================================
!
! ISOSURFACE:
!
! For generating triangulated iso-surfaces for display. An isosurface
! is just a list of point, and a list of integer triples describing
! each triangular face of the object.
!
! You can use the "recursive_marching_cube" or noremal "marching_cube"
! method to get the surface. The former is faster for single
! non-reentrant surfaces.
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
!=====================================================================
module ISOSURFACE_MODULE

#  include "macros"

   include "isosurface.use"

   implicit none

   include "isosurface.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self)
   ! Allocate an object
      type(ISOSURFACE_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"ISOSURFACE:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine create_1(self,atom)
   ! Create a grid object
      type(ISOSURFACE_TYPE), allocatable, OUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      call create_(self)
      call set_atom_defaults_(self,atom)
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(ISOSURFACE_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"ISOSURFACE:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
      type(ISOSURFACE_TYPE), INOUT :: self
      call destroy_(self%iso_values)
      call destroy_(self%point)
      call destroy_(self%face)
      call destroy_(self%neighbour)
      call destroy_(self%cap)
      call destroy_(self%point_gradient)
      call destroy_(self%point_mean_curvature)
      call destroy_(self%point_gaussian_curvature)
      call destroy_(self%map)
      call destroy_(self%surface_property_values)
      call destroy_(self%surface_iso_values)
      call destroy_(self%colour)
      call destroy_(self%atom)
   end subroutine

   PURE subroutine destroy_info_arrays(self)
   ! Destroy the isosurface information arrays
      type(ISOSURFACE_TYPE), INOUT :: self
      self%n_pt = 0
      self%n_face = 0
      self%n_cap = 0
      call destroy_(self%point)
      call destroy_(self%point_gradient)
      call destroy_(self%point_mean_curvature)
      call destroy_(self%point_gaussian_curvature)
      call destroy_(self%face)
      call destroy_(self%neighbour)
      call destroy_(self%cap)
      call destroy_(self%surface_property_values)
      call destroy_(self%surface_iso_values)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self,atom)
   ! Set up defaults
      type(ISOSURFACE_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), optional, IN :: atom
      self%property = " "
      self%triangulation_method = " "
      self%iso_value = ZERO
      self%n_pt = 0
      self%n_face = 0
      self%n_cap = 0
      self%area = ZERO
      self%volume = ZERO
      self%an_volume = ZERO
      self%volume_min = ZERO
      self%volume_max = ZERO
      self%n_skip = 0
      self%level = 0
      self%final_level = 0
      self%scan_level = 0
      self%minimum_scan_level = ISOSURFACE_MINIMUM_SCAN_LEVEL
      self%minimum_scan_division = ZERO
      self%voxel_proximity_factor = ISOSURFACE_VOXEL_PROXIMITY_FACTOR
      self%del_x = ONE
      self%del_y = ONE
      self%del_z = ONE
      self%x = 0
      self%y = 0
      self%z = 0
      self%small_map = FALSE
      self%big_interior = FALSE
      self%cap_ends = 0
      self%suppress_caps = FALSE
      self%surface_property = " "
      self%surface_point = ZERO
      self%chop_surface_property = FALSE
      self%surface_property_lb = ISOSURFACE_PROPERTY_LB
      self%surface_property_ub = ISOSURFACE_PROPERTY_UB
      self%surface_prop_lb_abs = ISOSURFACE_PROPERTY_LB_ABS
      self%surface_prop_ub_abs = ISOSURFACE_PROPERTY_UB_ABS
      self%CX_output_fingerprint = FALSE
      self%CX_fingerprint_grid = 2
      self%CX_output_distance_properties = FALSE
      self%CX_output_shape_properties = FALSE
      self%hirshfeld_power_factor = 1

      ! Special
      call create_(self%colour)
      call set_reverse_defaults_(self%colour)

      ! Copy atom
      if (present(atom)) then
         call set_atom_defaults_(self,atom)
      end if
   end subroutine

   PURE subroutine set_atom_defaults(self,atom)
   ! Set up atom defaults
      type(ISOSURFACE_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      self%atom = atom
      call set_defaults_(self%plot_grid,atom)
   end subroutine

   pure subroutine set_surface_property_ub(self,val)
   ! Generic set
      type(ISOSURFACE_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%surface_property_ub = val
   end subroutine


!   default_big_interior result (res) :: PURE
!   ! Return TRUE if the interior of the isosurface is bigger than the exterior,
!   ! i.e. gradients for the isosurface plot are to be reversed by default, so that
!   ! they point outside the isosurface. NOTE: gradients should be reversed if the
!   ! function has bigger values *inside* the isosurface.
!      self :: IN
!      res :: BIN
!
!      def :: BIN
!
!      def = TRUE
!
!      if (.iso_value<ZERO) def = FALSE
!      res = def
!
!      select case (.property)
!      case("delta_density      "); res = def ! only for +ve densities ...
!      case("electric_potential "); res = def
!      case("electron_density   "); res = def
!      case("elf                "); res = def
!      case("fermi_mobility     "); res = def
!      case("grad_rho_on_rho    "); res = def ! a guess
!      case("hirshfeld_density  "); res = def
!      case("laplacian_density  "); res = def
!      case("orbital            "); res = def
!      case("orbital_density    "); res = def
!      case("promolecule_density"); res = def
!      case("spin_density       "); res = def ! only for +ve densities
!      case("stockholder_density"); res = def
!      case("true_fermi_mobility"); res = def
!      case("tsirelson_elf      "); res = def
!      case default;      ! don't know what it should be ...
!      end
!   end

   PURE subroutine set_isosurface_info_arrays(self)
   ! Destroy the isosurface information and set the informations arrays to nice
   ! big sizes to begin with.
      type(ISOSURFACE_TYPE), INOUT :: self
      call destroy_info_arrays_(self)
      call create_(self%point,3,ISOSURFACE_N_POINT)
      call create_(self%point_gradient,3,ISOSURFACE_N_POINT)
      call create_(self%point_mean_curvature,ISOSURFACE_N_POINT)
      call create_(self%point_gaussian_curvature,ISOSURFACE_N_POINT)
      call create_(self%face,3,ISOSURFACE_N_FACE)
      call create_(self%cap,3,ISOSURFACE_N_CAP)
   end subroutine

   subroutine set_default_cube(self,cube)
   ! Initialise marching "cube" to the settings required for the isosurface.
   ! NOTE: .iso_value, .del, .big_interior must all be set.
      type(ISOSURFACE_TYPE), IN :: self
      type(MARCHINGCUBE_TYPE), INOUT :: cube
      call set_defaults_(cube)
      call set_iso_value_(cube,self%iso_value)
      call set_side_length_(cube,self%del_x,self%del_y,self%del_z)
      call set_axes_(cube,self%plot_grid%box_axes)
   !  cube.set_accuracy(.accuracy)
   !  cube.set_big_interior(.big_interior)
      call set_big_interior_(cube,TRUE)
   end subroutine


!  ===============
!  Inquiry methods
!  ===============

   pure function property_bounds_set(self) result (res)
   ! Return TRUE if the property bounds have been set
      type(ISOSURFACE_TYPE), IN :: self
      BIN :: res
      res = NOT same_as_(self%surface_property_lb,self%surface_property_ub)
   end function

   pure function surface_property_lb_set(self) result (res)
   ! Return TRUE if the surface property lower bound has been set.
      type(ISOSURFACE_TYPE), IN :: self
      BIN :: res
      res = self%surface_property_lb/=ISOSURFACE_PROPERTY_LB
   end function

   pure function surface_property_ub_set(self) result (res)
   ! Return TRUE if the surface property lower bound has been set.
      type(ISOSURFACE_TYPE), IN :: self
      BIN :: res
      res = self%surface_property_ub/=ISOSURFACE_PROPERTY_UB
   end function

   pure function surface_property_ub_abs_set(self) result (res)
   ! Return TRUE if the surface property upper bound for absolute values has
   ! been set.
      type(ISOSURFACE_TYPE), IN :: self
      BIN :: res
      res = self%surface_prop_ub_abs/=ISOSURFACE_PROPERTY_UB_ABS
   end function

   pure function surface_point_set(self) result (res)
   ! Return TRUE if the surface point has been set
      type(ISOSURFACE_TYPE), IN :: self
      BIN :: res
      res = any(self%surface_point/=ISOSURFACE_SURFACE_POINT)
   end function

   pure function is_hirshfeld_surface(self) result (res)
   ! Return TRUE if the isosurface settings are for a Hirshfeld surface.
      type(ISOSURFACE_TYPE), IN :: self
      BIN :: res
      res = (self%property=="stockholder_density" OR self%property=="stockholder_weight") AND equals_(self%iso_value,HALF)
   end function

   pure function surface_label(self) result (res)
   ! Return a string which describes the isosurface, including the isovalue.
      type(ISOSURFACE_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      STR(len=STR_SIZE) :: prop
      if (is_hirshfeld_surface_(self)) then
         res = "Hirshfeld_surface"
      else if (self%property=="orbital") then
         prop = orbital_label_(self)
         res = trim(prop)//",isovalue="//to_str_(self%iso_value,"f10.5")
      else
         prop = self%property
         call to_upper_case_(prop(1:1))
         res = trim(prop)//",isovalue="//to_str_(self%iso_value,"f10.5")
      end if
      if (self%surface_property=="orbital") then
         prop = orbital_label_(self)
         res = trim(prop)//"-on-"//trim(res)
      else if (self%surface_property/=" " AND self%surface_property/="none") then
         prop = self%surface_property
         call to_upper_case_(prop(1:1))
         res = trim(prop)//"-on-"//trim(res)
      end if
   end function

   pure function orbital_label(self) result (res)
   ! Return a string which describes the orbital being plotted
   ! This uses the fact that a non-negative value of 
   ! .plot_grid.LUMO_orbital means that this value was set.
      type(ISOSURFACE_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      INT :: homo,lumo
      homo = self%plot_grid%HOMO_orbital
      lumo = self%plot_grid%LUMO_orbital
      if (lumo/=-1) then

         ! The LUMO value was set
         if (lumo==0) then
            res = "LUMO"
         else
            res = "LUMO-plus-"//trim(to_str_(lumo))
         end if
      else

         ! The HOMO was set or is default
         if (homo==0) then
            res = "HOMO"
         else if (homo>0) then
            res = "HOMO-plus-"//trim(to_str_(homo))
         else if (homo<0) then
            homo = -homo
            res = "HOMO-minus-"//trim(to_str_(homo))
         end if
      end if
   end function


!  =============
!  Input methods
!  =============

   subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(ISOSURFACE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","ISOSURFACE:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"ISOSURFACE:read_keywords ... unexpected end of file while reading list keywords")
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
   ! Process a comand "keyword". Data is inputted from "stdin", unless
   ! "word" is a sequence of blank separated strings. In this case,
   ! the sequence is processed as if it were a separate file.
      type(ISOSURFACE_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      select case (keyword)
      case ("}                             ")  ! exit case
      case ("big_interior=                 "); call read_big_interior_(self)
      case ("cap_ends=                     "); call read_cap_ends_(self)
      case ("chop_surface_propertye=       "); call read_(stdin,self%chop_surface_property)
      case ("colour_function=              "); call read_colour_function_(self)
      case ("cx_fingerprint_grid=          "); call read_CX_fingerprint_grid_(self)
      case ("cx_output_fingerprint=        "); call read_CX_output_fingerprint_(self)
      case ("cx_output_distance_properties="); call read_CX_output_distance_properties_(self)
      case ("cx_output_shape_properties=   "); call read_CX_output_shape_properties_(self)
      case ("hirshfeld_power_factor=       "); call read_hirshfeld_power_factor_(self)
      case ("iso_value=                    "); call read_iso_value_(self)
      case ("iso_values=                   "); call read_iso_values_(self)
      case ("kind=                         "); call read_property_(self)  ! deprecated
      case ("minimum_scan_division=        "); call read_minimum_scan_division_(self)
      case ("minimum_scan_level=           "); call read_minimum_scan_level_(self)
      case ("plot_grid=                    "); call read_plot_grid_(self)
      case ("property=                     "); call read_property_(self)
      case ("put                           "); call put_(self)
      case ("put_colour_function           "); call put_(self%colour)
      case ("put_connected_area            "); call put_connected_area_(self)
      case ("put_faces                     "); call put_faces_(self)
      case ("put_face_normals              "); call put_face_normals_(self)
      case ("put_points                    "); call put_points_(self)
      case ("put_vertex_gradients          "); call put_vertex_gradients_(self)
      case ("put_vertex_normals            "); call put_vertex_normals_(self)
      case ("small_map=                    "); call read_small_map_(self)
      case ("suppress_caps=                "); call read_suppress_caps_(self)
      case ("surface_point=                "); call read_surface_point_(self)
      case ("surface_property=             "); call read_surface_property_(self)
      case ("surface_property_lower_bound= "); call read_surface_property_lb_(self)
      case ("surface_property_upper_bound= "); call read_surface_property_ub_(self)
      case ("surface_prop_lower_bound_abs= "); call read_surface_property_lb_abs_(self)
      case ("surface_prop_upper_bound_abs= "); call read_surface_property_ub_abs_(self)
   ! case ("test_cubify                   "); .test_cubify
      case ("triangulation_method=         "); call read_triangulation_method_(self)
      case ("units=                        "); call read_units_(self)
      case ("voxel_proximity_factor=       "); call read_voxel_proximity_factor_(self)
      case default; allocate(tonto%known_keywords(36))
      tonto%known_keywords(1) = "}                             "
      tonto%known_keywords(2) = "big_interior=                 "
      tonto%known_keywords(3) = "cap_ends=                     "
      tonto%known_keywords(4) = "chop_surface_propertye=       "
      tonto%known_keywords(5) = "colour_function=              "
      tonto%known_keywords(6) = "cx_fingerprint_grid=          "
      tonto%known_keywords(7) = "cx_output_fingerprint=        "
      tonto%known_keywords(8) = "cx_output_distance_properties="
      tonto%known_keywords(9) = "cx_output_shape_properties=   "
      tonto%known_keywords(10) = "hirshfeld_power_factor=       "
      tonto%known_keywords(11) = "iso_value=                    "
      tonto%known_keywords(12) = "iso_values=                   "
      tonto%known_keywords(13) = "kind=                         "
      tonto%known_keywords(14) = "minimum_scan_division=        "
      tonto%known_keywords(15) = "minimum_scan_level=           "
      tonto%known_keywords(16) = "plot_grid=                    "
      tonto%known_keywords(17) = "property=                     "
      tonto%known_keywords(18) = "put                           "
      tonto%known_keywords(19) = "put_colour_function           "
      tonto%known_keywords(20) = "put_connected_area            "
      tonto%known_keywords(21) = "put_faces                     "
      tonto%known_keywords(22) = "put_face_normals              "
      tonto%known_keywords(23) = "put_points                    "
      tonto%known_keywords(24) = "put_vertex_gradients          "
      tonto%known_keywords(25) = "put_vertex_normals            "
      tonto%known_keywords(26) = "small_map=                    "
      tonto%known_keywords(27) = "suppress_caps=                "
      tonto%known_keywords(28) = "surface_point=                "
      tonto%known_keywords(29) = "surface_property=             "
      tonto%known_keywords(30) = "surface_property_lower_bound= "
      tonto%known_keywords(31) = "surface_property_upper_bound= "
      tonto%known_keywords(32) = "surface_prop_lower_bound_abs= "
      tonto%known_keywords(33) = "surface_prop_upper_bound_abs= "
      tonto%known_keywords(34) = "triangulation_method=         "
      tonto%known_keywords(35) = "units=                        "
      tonto%known_keywords(36) = "voxel_proximity_factor=       "
      call unknown_(tonto,keyword,"ISOSURFACE:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_hirshfeld_power_factor(self)
   ! Read the hirshfeld power factor.
   ! Densities are raise to this factor when generating
   ! Hirshfeld surfaces
   ! The number of points is adjusted to be no more than this.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%hirshfeld_power_factor)
   end subroutine

   subroutine read_CX_output_distance_properties(self)
   ! Read whether to output di, de and dnorm in put_CX routine
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%CX_output_distance_properties)
   end subroutine

   subroutine read_CX_output_shape_properties(self)
   ! Read whether to output shape index and curvedness in put_CX routine
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%CX_output_shape_properties)
   end subroutine

   subroutine read_CX_output_fingerprint(self)
   ! Read whether the fingerprint data should be output in put_CX routine
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%CX_output_fingerprint)
   end subroutine

   subroutine read_CX_fingerprint_grid(self)
   ! Read the resolution of the fingerprint grid; affects resolution of the outputted fingerprint data
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%CX_fingerprint_grid)
   end subroutine

   subroutine read_small_map(self)
   ! Read a switch which tells whether to delete dead elements from
   ! the map table.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%small_map)
   end subroutine

   subroutine read_big_interior(self)
   ! Read a switch which tells if the interior of the isosurface is
   ! bigger than the exterior i.e. whether to reverse the surface
   ! gradients or not.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%big_interior)
   end subroutine

   subroutine read_cap_ends(self)
   ! Set non-zero if you want the ends of the isosurface on the
   ! bounding cube to be capped. Set less than one if you want values
   ! less than the isosurface capped, or greater than one if you want
   ! values greater than the iso value capped.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%cap_ends)
   end subroutine

   subroutine read_suppress_caps(self)
   ! Only applies if .cap_ends is /= 0
   ! If suppress_caps = TRUE then caps are generated but no put out
   ! when put_faces is reached
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%suppress_caps)
   end subroutine

   subroutine read_colour_function(self)
   ! Read the details of the colourfunction to be used
      type(ISOSURFACE_TYPE), INOUT :: self
   ! .colour.read_data_and_colour
      call read_data_and_RGB_(self%colour)
   end subroutine

   subroutine read_iso_value(self)
   ! Read the defining iso_value for the isosurface
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_quantity_(stdin,self%iso_value)
   end subroutine

   subroutine read_iso_values(self)
   ! Read the defining iso_values for the isosurface
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_all_quantity_(stdin,self%iso_values)
   end subroutine

   subroutine read_property(self)
   ! Read the kind of isosurface to plot.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%property)
      call to_lower_case_(self%property)
      call set_plot_kind_(self%plot_grid,self%property)
   end subroutine

   subroutine read_triangulation_method(self)
   ! Read the triangulation method a number whose smallness correlates with how
   ! small each face of the generated isosurface is.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%triangulation_method)
      call to_lower_case_(self%triangulation_method)
      select case (self%triangulation_method)
      case ("marching_cube          ")
      case ("recursive_marching_cube")
      case default; allocate(tonto%known_keywords(2))
      tonto%known_keywords(1) = "marching_cube          "
      tonto%known_keywords(2) = "recursive_marching_cube"
      call unknown_(tonto,self%triangulation_method,"ISOSURFACE:read_triangulation_method")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_plot_grid(self)
   ! Read in the plot grid data
      type(ISOSURFACE_TYPE), INOUT :: self
      call reset_defaults_(self%plot_grid)  ! don't reset bounding box or axes
      call read_keywords_(self%plot_grid)
   end subroutine

   subroutine read_surface_point(self)
   ! Read a surface point which lies on, or near the surface.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_quantity_(stdin,self%surface_point)
   end subroutine

   subroutine read_surface_property(self)
   ! Read surface properties to calculate
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%surface_property)
      call to_lower_case_(self%surface_property)
   end subroutine

   subroutine read_surface_property_lb(self)
   ! Read the value of a property lower bound.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_quantity_(stdin,self%surface_property_lb)
   end subroutine

   subroutine read_surface_property_ub(self)
   ! Read the value of a property upper bound.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_quantity_(stdin,self%surface_property_ub)
   end subroutine

   subroutine read_surface_property_lb_abs(self)
   ! Read the value of the lower bound for absolute values of the property.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_quantity_(stdin,self%surface_prop_lb_abs)
   end subroutine

   subroutine read_surface_property_ub_abs(self)
   ! Read the value of the upper bound for absolute values of the property.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_quantity_(stdin,self%surface_prop_ub_abs)
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(ISOSURFACE_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_minimum_scan_division(self)
   ! Read the minimum acceptable size for the marching cube, in order
   ! to resolve all important isosurface features, when using the
   ! recursive marching cubes method.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_quantity_(stdin,self%minimum_scan_division)
      ENSURE(self%minimum_scan_division>ZERO,"ISOSURFACE:read_minimum_scan_division ... value must be positive")
   end subroutine

   subroutine read_minimum_scan_level(self)
   ! Read the minimum number of scan divisions when using the
   ! recursive marching cubes method. It must be large enough to
   ! resolve all isosurface features.  It is probably better to set
   ! the minimum_scan_division above, which is a length. However, the
   ! number entered here is used as a sanity check to override the
   ! number of divisions obtained from the minimum_scan_division if it
   ! is smaller than this number.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%minimum_scan_level)
      ENSURE(self%minimum_scan_level>0,"ISOSURFACE:read_minimum_scan_level ... value must be positive")
   end subroutine

   subroutine read_voxel_proximity_factor(self)
   ! Read the voxel proximity factor, which determies whn a voxel or marching
   ! cube is close to the isosurface. It works by checking if the .iso_value is
   ! within the entered factor times the difference between the maximum and
   ! minimum function values on the voxel eges.
      type(ISOSURFACE_TYPE), INOUT :: self
      call read_(stdin,self%voxel_proximity_factor)
      ENSURE(self%voxel_proximity_factor>ONE,"ISOSURFACE:read_voxel_proximity_factor ... value must be greater than one")
   end subroutine


!  =====================
!  Marching cube methods
!  =====================

!   cubify(func) :: leaky
!   ! Generate the isosurface using the marching cube algorithm
!   ! for a cubic region with an equally spaced grid.
!   ! NOTE: Recently, only the recursive marching cube routine
!   ! has been exhaustively tested.
!      self :: INOUT
!      interface
!         func(values,pt)
!            values :: VEC{REAL}, OUT
!            pt :: MAT{REAL}, IN
!         end
!      end
!
!      .prepare_grid
!
!      select case (.triangulation_method)
!      case("marching_cube          "); .cubify_normally(func) !  Warning: see note
!      case("recursive_marching_cube"); .cubify_recursively(func)
!      case default;        UNKNOWN(.triangulation_method)
!      end
!
!      DIE_IF(.n_pt==0,"No isosurface points found.")
!
!      ! Report
!      .put
!
!   end

   subroutine cubify_m(self,mol,func_m)
   ! Generate the isosurface using the marching cube algorithm
   ! for a cubic region with an equally spaced grid.
   ! NOTE: Recently, only the recursive marching cube routine
   ! has been exhaustively tested.
      type(ISOSURFACE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      interface
         subroutine func_m(mol,values,pt)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: mol
            VEC(REAL,:), OUT :: values
            MAT(REAL,:,:), IN :: pt
         end subroutine
      end interface
      call prepare_grid_(self)
      select case (self%triangulation_method)
      case ("marching_cube          "); call cubify_normally_m_(self,mol,func_m)  !  Warning: see note
      case ("recursive_marching_cube")
         call cubify_recursively_m_(self,mol,func_m)
      case default; allocate(tonto%known_keywords(2))
      tonto%known_keywords(1) = "marching_cube          "
      tonto%known_keywords(2) = "recursive_marching_cube"
      call unknown_(tonto,self%triangulation_method,"ISOSURFACE:cubify_m")
      deallocate(tonto%known_keywords)
      end select
      DIE_IF(self%n_pt==0,"ISOSURFACE:cubify_m ... No isosurface points found.")

      ! Report
      call put_(self)
   end subroutine

   subroutine prepare_grid(self)
   ! Prepare the rhomboid grid for the different kinds of isosurface
   ! alogorithms.  Particularly, for the recursive method, set the
   ! .final_level and the .scan_level. Then output the grid
   ! information before doing anything, so the user can check
   ! everything is OK.
      type(ISOSURFACE_TYPE), INOUT :: self
      INT :: n_div,n_pt,fl,ml
      REAL :: scale
      BIN :: oblique

      ! Set the plot kind
      call set_plot_kind_(self%plot_grid,self%property)
      oblique = self%plot_grid%use_unit_cell_as_bbox
      if (NOT oblique) then
         ENSURE(has_all_equal_(self%plot_grid%width),"ISOSURFACE:prepare_grid ... grid widths must be all equal")
      end if
      select case (self%triangulation_method)

      case ("marching_cube          ")
         call set_for_marching_cubes_(self%plot_grid)
         self%del_x = self%plot_grid%del_x
         self%del_y = self%plot_grid%del_y
         self%del_z = self%plot_grid%del_x
         call set_default_cube_(self,self%cube)
         call set_defaults_(self%cap_square)

      case ("recursive_marching_cube")
         if (equals_(self%minimum_scan_division,ZERO)) then
            if (self%property=="stockholder_density") then
               self%minimum_scan_division = TWO*ISOSURFACE_MINIMUM_SCAN_DIVISION
            else
               self%minimum_scan_division = ISOSURFACE_MINIMUM_SCAN_DIVISION
            end if
         end if

         ! First find the number of divisions required
         n_div = no_of_divisions_(self,self%minimum_scan_division)
         n_div = max(n_div,self%minimum_scan_level)
         n_pt = 2**n_div

         ! The user might have requested too small a number of points ...
         ! We must have at least the minimum number of divisions
         if (n_pt>(max(self%plot_grid%n_x,self%plot_grid%n_y,self%plot_grid%n_z)-1)) then
            WARN("ISOSURFACE:prepare_grid ... No. of points not sufficient, adjusting to be larger")
            self%plot_grid%n_x = n_pt+1
         end if

         ! Find out what the final level is that we really need.
         self%final_level = ceiling(log(real(self%plot_grid%n_x-2))/log(TWO))
         fl = self%final_level
         ml = self%minimum_scan_level
         ENSURE(fl<=32,"ISOSURFACE:prepare_grid ... too many grid points: # of divisions = "//trim(to_str_(fl)))
         ENSURE(fl>=ml,"ISOSURFACE:prepare_grid ... too few grid points: # of divisions = "//trim(to_str_(fl)))
         self%scan_level = -int(log(self%minimum_scan_division/maxval(self%plot_grid%width))/log(TWO))
         self%scan_level = min(self%final_level,self%scan_level)
         self%scan_level = max(self%scan_level,self%minimum_scan_level)

         ! We have the right level, now adjust the # of points to be a power
         ! of two (plus one to be odd), in order to apply the telescoping
         ! cubes algorithm.  Any points outside the grid required for
         ! gradients or hessians are calculated explicitly; this grid is
         ! really the true bounding region of the isosurface plot.
         ! WARN("Adjusting grid points higher to the nearest power of 2")
         self%plot_grid%n_x = 2**self%final_level+1
         if (NOT oblique) then
            call set_points_widths_origin_(self%plot_grid)
            ! If requested, adjust the bounding box width to be larger so that
            ! we get *exactly* the desired separation. Otherwise, the grid
            ! separation will be the smallest that can be used with the current
            ! bounding box to achieve *at least* the number of user requested points.
            if (self%plot_grid%desired_separation>ZERO) then
               scale = self%plot_grid%desired_separation/self%plot_grid%del_x
               self%plot_grid%width = self%plot_grid%width*scale
               call set_points_widths_origin_(self%plot_grid)
            end if
         else

            ! Do not change widths -- they are taken from unit cell
            call set_cube_from_widths_(self%plot_grid)
         end if
         self%del_x = self%plot_grid%del_x
         self%del_y = self%plot_grid%del_y
         self%del_z = self%plot_grid%del_z
         call set_default_cube_(self,self%cube)
         self%del_x = self%plot_grid%width(1)
         self%del_y = self%plot_grid%width(2)
         self%del_z = self%plot_grid%width(3)

      case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "marching_cube          "
         tonto%known_keywords(2) = "recursive_marching_cube"
         call unknown_(tonto,self%triangulation_method,"ISOSURFACE:prepare_grid")
         deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine prepare_Bader_grid(self)
   ! Prepare the rhomboid grid for triangulating Bader basin
   ! boundaries. Unlike .prepare_grid this does *not* call
   ! .plot_grid.set_for_marching_cubes: the grid on which the basins
   ! were assigned is the grid to be triangulated, and adding two
   ! points per axis would invalidate the basin index array.
      type(ISOSURFACE_TYPE), INOUT :: self

      ! Set the plot kind
      call set_plot_kind_(self%plot_grid,self%property)
      self%del_x = self%plot_grid%del_x
      self%del_y = self%plot_grid%del_y
      self%del_z = self%plot_grid%del_z
      call set_default_cube_(self,self%cube)
      call set_defaults_(self%cap_square)
   end subroutine

   PURE function capping_number(self,x,y,z) result (res)
   ! Identify which faces of a given cube denoted by x,y and z are on the boundary of the plot grid
      type(ISOSURFACE_TYPE), IN :: self
      INT, IN :: x,y,z
      INT :: res
      res = 0
      if (z==2) res = ibset(res,0)
      if (z==self%plot_grid%n_z-2) res = ibset(res,1)
      if (x==2) res = ibset(res,2)
      if (x==self%plot_grid%n_x-2) res = ibset(res,3)
      if (y==2) res = ibset(res,4)
      if (y==self%plot_grid%n_y-2) res = ibset(res,5)
   end function

   PURE function on_boundary(self,x,y,z) result (res)
   ! Return true if x,y,z refers to a cube on the boundary of the plot
   ! grid
      type(ISOSURFACE_TYPE), IN :: self
      INT, IN :: x,y,z
      BIN :: res
      res = (NOT (capping_number_(self,x,y,z)==0))
   end function


!   cubify_normally(func) :: leaky, private
!   ! Generate the isosurface using the standard marching cube algorithm
!   ! *without* any recursion.
!      self :: INOUT
!      interface
!         func(values,pt)
!            values :: VEC{REAL}, OUT
!            pt :: MAT{REAL}, IN
!         end
!      end
!
!      x,y,z, capping_number, face_number, b :: INT
!      cube :: MARCHINGCUBE
!      cap_square :: CAPPING_SQUARE
!      func_slab :: MAT3{REAL}@
!      info :: MAT3{EVEC{INT}}@
!      x_cap_info, y_cap_info, z_cap_info :: MAT3{EVEC{INT}}@
!      point_slab :: MAT4{REAL}@
!      oblique, do_capping :: BIN
!
!      ! Set isosurface info defaults
!      .set_isosurface_info_arrays
!
!      ! .cube contains the isovalue and size/axes of grid
!      cube = .cube
!      cap_square = .cap_square
!
!      oblique = .plot_grid.use_unit_cell_as_bbox
!      if (oblique) cube.set_axes(.plot_grid.box_axes)
!
!      ! Make the 4-slab of data
!      point_slab.create(3,.plot_grid.n_x,.plot_grid.n_y,4)         ! slab of points
!      func_slab.create(.plot_grid.n_x,.plot_grid.n_y,4)           ! function values
!
!      info.create(.plot_grid.n_x,.plot_grid.n_y,2,[0,11]) ! Saved vertex index info
!      info.zero
!
!      x_cap_info.create(.plot_grid.n_y,.plot_grid.n_z,2,[0,7])
!      x_cap_info.zero
!      y_cap_info.create(.plot_grid.n_x,.plot_grid.n_z,2,[0,7])
!      y_cap_info.zero
!      z_cap_info.create(.plot_grid.n_x,.plot_grid.n_y,2,[0,7])
!      z_cap_info.zero
!
!      ! Find all the marching cubes. Loop bottom to top.
!      ! Do two layers at a time. End points are not looped,
!      ! they are only to get the normals and curvatures.
!      do z = 2,.plot_grid.n_z-2
!
!         ! Get a new z-slab of function values
!         .update_4_slab(point_slab, func_slab, z, func)
!
!         ! Save previous edge info; zero current
!         info(:,:,1:1).set_to(info(:,:,2:2))
!         info(:,:,2:2).zero
!
!         ! Loop over the slab;
!         ! x = left,  right
!         ! y = front, back
!         do y = 2,.plot_grid.n_y-2
!         do x = 2,.plot_grid.n_x-2
!
!            ! Set cube points and function values
!            cube.set_vertex_info( point_slab(:,x:x+1,y:y+1,2:3), func_slab(x:x+1,y:y+1,2:3) )
!
!            ! Set cube edge intersections
!            cube.set_case_info
!
!            ! If not on surface, do next cube except if we are on the boundary and doing caps
!            do_capping = (.cap_ends /= 0) AND .on_boundary(x,y,z)
!            if (NOT (cube.is_on_surface OR do_capping)) cycle
!
!            ! Set up cube edge triangulation
!            cube.set_triangulation_info
!
!            ! Use previous edge info, if available.
!            ! left = previous cube in x-direction
!            ! front = previous cube in y-direction
!            ! bottom - previous cube in z-direction
!            if (x>2) cube.set_left_info(left  =info(x-1,y  ,2).element)
!            if (y>2) cube.set_front_info(front=info(x  ,y-1,2).element)
!            if (z>2) cube.set_below_info(below=info(x  ,y  ,1).element)
!
!            ! Get curvatures
!            cube.set_hessian_info( func_slab(x-1:x+2,y-1:y+2,1:4) )
!
!            ! Now do the interpolation
!            if (oblique) then; cube.interpolate_oblique_faces
!            else;              cube.interpolate_faces
!            end
!
!            ! Store surface info
!            .append_new_face_info(cube)
!
!            ! Determine which caps to add
!            if (do_capping) then
!
!               capping_number = .capping_number(x,y,z)
!
!               ! Six possible faces to test
!               do b = 0,5
!
!                  ! Find which faces to do (up to three)
!                  if (NOT BIT_SET_AT_POS_IN(capping_number,b)) cycle 
!
!                  ! face_number starts at 1 not 0
!                  face_number = b + 1 
!
!                  ! initialize the capping square from the corresponding marching cube
!                  cube.set_capping_square(face_number,cap_square)
!                  .set_edge_vertices(cap_square) ! Get vertices created for the cube
!                  cap_square.set_n_pt(.n_pt)
!
!                  ! Depending on the face we are on, we initialize the
!                  ! cap to use vertices already generated for other
!                  ! surface caps. We do this by using the capping
!                  ! square to the "left" and the capping square
!                  ! "below" the capping square we are currently
!                  ! working on.  The tricky part is the final argument
!                  ! to set_left_info or set_bottom_info - this boolean
!                  ! determines whether we need to flip the capping
!                  ! square.  set_left_info(...,TRUE) flips the left
!                  ! and right edges set_bottom_info(...,TRUE) flips
!                  ! the top and bottom edges The capping square of 3
!                  ! of the plotgrids faces always need flipping
!                  ! Because of the (complicated) interplay between the
!                  ! cubes coordinate system and that of the capping
!                  ! square, it turns out we need to flip...
!                  ! (i)   bottom-top mapping for z=0 faces (b==0)
!                  ! (ii)  left-right mapping for x=0 faces (b==2)
!                  ! (iii) left-right mapping for  +y faces (b==5)
!                  if (b == 0 OR b ==1) then ! 0,+z faces
!                     if (x>2) cap_square.set_left_info(z_cap_info(x-1,y,b+1).element, FALSE)
!                     if (y>2) cap_square.set_bottom_info(z_cap_info(x,y-1,b+1).element, b==0)
!                  else if (b == 2 OR b == 3) then ! 0,+x faces
!                     if (y>2) cap_square.set_left_info(x_cap_info(y-1,z,b-1).element, b==2)
!                     if (z>2) cap_square.set_bottom_info(x_cap_info(y,z-1,b-1).element, FALSE)
!                  else if (b == 4 OR b == 5) then ! 0,+y faces
!                     if (x>2) cap_square.set_left_info(y_cap_info(x-1,z,b-3).element, b==5)
!                     if (z>2) cap_square.set_bottom_info(y_cap_info(x,z-1,b-3).element, FALSE)
!                  end
!
!                  cap_square.triangulate
!                  .append_new_cap_info(cap_square)
!
!                  if (b == 0 OR b == 1) then ! 0,+z faces
!                     z_cap_info(x,y,b+1)[4:7] = cap_square.edge_vertex_index(4:7)
!                  else if (b == 2 OR b == 3) then ! 0,+x faces
!                     x_cap_info(y,z,b-1)[4:7] = cap_square.edge_vertex_index(4:7)
!                  else ! 0 (b=4), +y (b=5) faces
!                     y_cap_info(x,z,b-3)[4:7] = cap_square.edge_vertex_index(4:7)
!                  end
!
!                  cap_square.reset
!               end
!
!            end
!
!            ! Keep interpolated vertex indices
!            info(x,y,2)[:] = cube.edge_vertex_index
!
!            ! Reset cube, reset # of points
!            cube.reset
!            cube.set_n_pt(.n_pt)
!
!         end
!         end
!
!      end
!
!      ! Clean
!      info.destroy
!      point_slab.destroy
!      func_slab.destroy
!      x_cap_info.destroy
!      y_cap_info.destroy
!      z_cap_info.destroy
!
!      ! Compactify
!      .point.shrink_columns(.n_pt)
!      .point_gradient.shrink_columns(.n_pt)
!      .point_mean_curvature.shrink(.n_pt)
!      .point_gaussian_curvature.shrink(.n_pt)
!      .face.shrink_columns(.n_face)
!      .cap.shrink_columns(.n_cap)
!
!      ! Rotate
!      .rotate_gradients
!
!   end

   subroutine cubify_normally_m(self,mol,func_m)
   ! Generate the isosurface using the standard marching cube algorithm
   ! *without* any recursion.
      type(ISOSURFACE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      interface
         subroutine func_m(mol,values,pt)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: mol
            VEC(REAL,:), OUT :: values
            MAT(REAL,:,:), IN :: pt
         end subroutine
      end interface
      INT :: x,y,z,capping_number,face_number,b
      type(MARCHINGCUBE_TYPE) :: cube
      type(CAPPING_SQUARE_TYPE) :: cap_square
      MAT3(REAL,:,:,:), allocatable :: func_slab
      MAT3(type(EVEC_INT_TYPE),:,:,:), allocatable :: info
      MAT3(type(EVEC_INT_TYPE),:,:,:), allocatable :: x_cap_info,y_cap_info,z_cap_info
      MAT4(REAL,:,:,:,:), allocatable :: point_slab
      BIN :: oblique,do_capping

      ! Set isosurface info defaults
      call set_isosurface_info_arrays_(self)

      ! .cube contains the isovalue and size/axes of grid
      cube = self%cube
      cap_square = self%cap_square
      oblique = self%plot_grid%use_unit_cell_as_bbox
      if (oblique) call set_axes_(cube,self%plot_grid%box_axes)

      ! Make the 4-slab of data
      call create_(point_slab,3,self%plot_grid%n_x,self%plot_grid%n_y,4)  ! slab of points
      call create_(func_slab,self%plot_grid%n_x,self%plot_grid%n_y,4)  ! function values
      call create_(info,self%plot_grid%n_x,self%plot_grid%n_y,2,[0,11])  ! Saved vertex index info
      call zero_(info)
      call create_(x_cap_info,self%plot_grid%n_y,self%plot_grid%n_z,2,[0,7])
      call zero_(x_cap_info)
      call create_(y_cap_info,self%plot_grid%n_x,self%plot_grid%n_z,2,[0,7])
      call zero_(y_cap_info)
      call create_(z_cap_info,self%plot_grid%n_x,self%plot_grid%n_y,2,[0,7])
      call zero_(z_cap_info)

      ! Find all the marching cubes. Loop bottom to top.
      ! Do two layers at a time. End points are not looped,
      ! they are only to get the normals and curvatures.
      do z = 2,self%plot_grid%n_z-2

         ! Get a new z-slab of function values
         call update_4_slab_m_(self,mol,point_slab,func_slab,z,func_m)

         ! Save previous edge info; zero current
         call set_to_(info(:,:,1:1),info(:,:,2:2))
         call zero_(info(:,:,2:2))

         ! Loop over the slab;
         ! x = left,  right
         ! y = front, back
         do y = 2,self%plot_grid%n_y-2
            do x = 2,self%plot_grid%n_x-2

            ! Set cube points and function values
               call set_vertex_info_(cube,point_slab(:,x:x+1,y:y+1,2:3),func_slab(x:x+1,y:y+1,2:3))

            ! Set cube edge intersections
               call set_case_info_(cube)

            ! If not on surface, do next cube except if we are on the boundary and doing caps
               do_capping = (self%cap_ends/=0) AND on_boundary_(self,x,y,z)
               if (NOT (is_on_surface_(cube) OR do_capping)) cycle

            ! Set up cube edge triangulation
               call set_triangulation_info_(cube)

            ! Use previous edge info, if available.
            ! left = previous cube in x-direction
            ! front = previous cube in y-direction
            ! bottom - previous cube in z-direction
               if (x>2) call set_left_info_(cube,left=info(x-1,y,2)%element)
               if (y>2) call set_front_info_(cube,front=info(x,y-1,2)%element)
               if (z>2) call set_below_info_(cube,below=info(x,y,1)%element)

            ! Get curvatures
               call set_hessian_info_(cube,func_slab(x-1:x+2,y-1:y+2,1:4))

            ! Now do the interpolation
               if (oblique) then; call interpolate_oblique_faces_(cube)
               else; call interpolate_faces_(cube)
               end if

            ! Store surface info
               call append_new_face_info_(self,cube)

            ! Determine which caps to add
               if (do_capping) then
                  capping_number = capping_number_(self,x,y,z)

               ! Six possible faces to test
                  do b = 0,5

                  ! Find which faces to do (up to three)
                     if (NOT BIT_SET_AT_POS_IN(capping_number,b)) cycle

                  ! face_number starts at 1 not 0
                     face_number = b+1

                  ! initialize the capping square from the corresponding marching cube
                     call set_capping_square_(cube,face_number,cap_square)
                     call set_edge_vertices_(self,cap_square)  ! Get vertices created for the cube
                     call set_n_pt_(cap_square,self%n_pt)

                  ! Depending on the face we are on, we initialize the
                  ! cap to use vertices already generated for other
                  ! surface caps. We do this by using the capping
                  ! square to the "left" and the capping square
                  ! "below" the capping square we are currently
                  ! working on.  The tricky part is the final argument
                  ! to set_left_info or set_bottom_info - this boolean
                  ! determines whether we need to flip the capping
                  ! square.  set_left_info(...,TRUE) flips the left
                  ! and right edges set_bottom_info(...,TRUE) flips
                  ! the top and bottom edges The capping square of 3
                  ! of the plotgrids faces always need flipping
                  ! Because of the (complicated) interplay between the
                  ! cubes coordinate system and that of the capping
                  ! square, it turns out we need to flip...
                  ! (i)   bottom-top mapping for z=0 faces (b==0)
                  ! (ii)  left-right mapping for x=0 faces (b==2)
                  ! (iii) left-right mapping for  +y faces (b==5)
                     if (b==0 OR b==1) then  ! 0,+z faces
                        if (x>2) call set_left_info_(cap_square,z_cap_info(x-1,y,b+1)%element,FALSE)
                        if (y>2) call set_bottom_info_(cap_square,z_cap_info(x,y-1,b+1)%element,b==0)
                     else if (b==2 OR b==3) then  ! 0,+x faces
                        if (y>2) call set_left_info_(cap_square,x_cap_info(y-1,z,b-1)%element,b==2)
                        if (z>2) call set_bottom_info_(cap_square,x_cap_info(y,z-1,b-1)%element,FALSE)
                     else if (b==4 OR b==5) then  ! 0,+y faces
                        if (x>2) call set_left_info_(cap_square,y_cap_info(x-1,z,b-3)%element,b==5)
                        if (z>2) call set_bottom_info_(cap_square,y_cap_info(x,z-1,b-3)%element,FALSE)
                     end if
                     call triangulate_(cap_square)
                     call append_new_cap_info_(self,cap_square)
                     if (b==0 OR b==1) then  ! 0,+z faces
                        z_cap_info(x,y,b+1)%element(4:7) = cap_square%edge_vertex_index(4:7)
                     else if (b==2 OR b==3) then  ! 0,+x faces
                        x_cap_info(y,z,b-1)%element(4:7) = cap_square%edge_vertex_index(4:7)
                     else  ! 0 (b=4), +y (b=5) faces
                        y_cap_info(x,z,b-3)%element(4:7) = cap_square%edge_vertex_index(4:7)
                     end if
                     call reset_(cap_square)
                  end do
               end if

            ! Keep interpolated vertex indices
               info(x,y,2)%element = cube%edge_vertex_index(0:11)

            ! Reset cube, reset # of points
               call reset_(cube)
               call set_n_pt_(cube,self%n_pt)
            end do
         end do
      end do

      ! Clean
      call destroy_(info)
      call destroy_(point_slab)
      call destroy_(func_slab)
      call destroy_(x_cap_info)
      call destroy_(y_cap_info)
      call destroy_(z_cap_info)

      ! Compactify
      call shrink_columns_(self%point,self%n_pt)
      call shrink_columns_(self%point_gradient,self%n_pt)
      call shrink_(self%point_mean_curvature,self%n_pt)
      call shrink_(self%point_gaussian_curvature,self%n_pt)
      call shrink_columns_(self%face,self%n_face)
      call shrink_columns_(self%cap,self%n_cap)

      ! Rotate
      call rotate_gradients_(self)
   end subroutine

   subroutine cubify_Bader(self,basin,n_basin)
   ! Triangulate the boundary of every Bader basin using the standard
   ! marching cube algorithm *without* recursion, assuming the whole
   ! grid is available in memory with each grid point assigned a
   ! "basin" index in the range 1 ... "n_basin". The vertex function
   ! value is ZERO inside the basin of interest and ONE outside it, and
   ! the isovalue is one half, so the surface falls on the voxel
   ! boundary. NOTE: capping is not done, so a basin cut by the edge of
   ! the plot grid gives an open surface.
      type(ISOSURFACE_TYPE), INOUT :: self
      MAT3(INT,:,:,:), IN :: basin
      INT, IN :: n_basin
      INT :: x,y,z,r
      type(MARCHINGCUBE_TYPE) :: cube
      MAT3(type(EVEC_INT_TYPE),:,:,:), allocatable :: info
      MAT3(INT,:,:,:), allocatable :: func_slab
      MAT4(REAL,:,:,:,:), allocatable :: point_slab
   ENSURE(size(basin,1)==self%plot_grid%n_x,"ISOSURFACE:cubify_Bader ... wrong dim1, basin")
   ENSURE(size(basin,2)==self%plot_grid%n_y,"ISOSURFACE:cubify_Bader ... wrong dim2, basin")
   ENSURE(size(basin,3)==self%plot_grid%n_z,"ISOSURFACE:cubify_Bader ... wrong dim3, basin")

      ! Set isosurface info defaults
      call set_isosurface_info_arrays_(self)

      ! One surface per basin, all appended to the same point/face lists
      do r = 1,n_basin

         ! .cube contains the size/axes of the grid; the basin flag is
         ! ZERO inside and ONE outside, so the surface is at one half
         cube = self%cube
         call set_iso_value_(cube,HALF)

         ! Make the 4-slab of data
         call create_(point_slab,3,self%plot_grid%n_x,self%plot_grid%n_y,4)  ! slab of points
         call create_(func_slab,self%plot_grid%n_x,self%plot_grid%n_y,4)  ! basin indices
         call create_(info,self%plot_grid%n_x,self%plot_grid%n_y,2,[0,11])  ! Saved vertex index info
         call zero_(info)

         ! Find all the marching cubes. Loop bottom to top,
         ! two layers at a time.
         do z = 2,self%plot_grid%n_z-2

            ! Get a new z-slab of basin indices
            call update_4_slab_(self,point_slab,func_slab,z,basin)

            ! Save previous edge info; zero current
            call set_to_(info(:,:,1:1),info(:,:,2:2))
            call zero_(info(:,:,2:2))

            ! Loop over the slab;
            ! x = left,  right
            ! y = front, back
            do y = 2,self%plot_grid%n_y-2
               do x = 2,self%plot_grid%n_x-2

               ! Set cube points and basin membership flags
                  call set_vertex_info_(cube,point_slab(:,x:x+1,y:y+1,2:3),func_slab(x:x+1,y:y+1,2:3),r)

               ! Which vertices are inside the basin?
                  call set_case_info_(cube)

               ! If not on the surface, do the next cube
                  if (NOT is_on_surface_(cube)) cycle

               ! Set up cube edge triangulation
                  call set_triangulation_info_(cube)

               ! Use previous edge info, if available.
               ! left  = previous cube in the x-direction
               ! front = previous cube in the y-direction
               ! below = previous cube in the z-direction
                  if (x>2) call set_left_info_(cube,left=info(x-1,y,2)%element)
                  if (y>2) call set_front_info_(cube,front=info(x,y-1,2)%element)
                  if (z>2) call set_below_info_(cube,below=info(x,y,1)%element)

               ! Now do the interpolation. Positions only: a basin
               ! index has no gradient
                  call interpolate_Bader_faces_(cube)

               ! Store surface info
                  call append_new_face_info_(self,cube)

               ! Keep interpolated vertex indices
                  info(x,y,2)%element = cube%edge_vertex_index(0:11)

               ! Reset cube, reset # of points
                  call reset_(cube)
                  call set_n_pt_(cube,self%n_pt)
               end do
            end do
         end do

         ! Clean
         call destroy_(info)
         call destroy_(point_slab)
         call destroy_(func_slab)
      end do

      ! Compactify
      call shrink_columns_(self%point,self%n_pt)
      call shrink_columns_(self%point_gradient,self%n_pt)
      call shrink_(self%point_mean_curvature,self%n_pt)
      call shrink_(self%point_gaussian_curvature,self%n_pt)
      call shrink_columns_(self%face,self%n_face)
      call shrink_columns_(self%cap,self%n_cap)

      ! Rotate
      call rotate_gradients_(self)
   end subroutine

   PURE subroutine set_edge_vertices(self,cap_square)
   ! copy the edge vertex positions for the marching cube into the capping square
      type(ISOSURFACE_TYPE), IN :: self
      type(CAPPING_SQUARE_TYPE), INOUT :: cap_square
      INT :: i,j
      do i = 0,3
         j = cap_square%edge_vertex_index(i)
         if (j==0) cycle
         cap_square%edge_positions(:,i) = self%point(:,j)
      end do
   end subroutine


!   cubify_recursively(func) :: leaky, private
!   ! Generate the isosurface using the recursive marching cube
!   ! algorithm.  Multiple isosurface values are merged into one
!   ! isosurface.
!      self :: INOUT
!      interface
!         func(values,pt)
!            values :: VEC{REAL}, OUT
!            pt :: MAT{REAL}, IN
!         end
!      end
!      i :: INT
!      isosurface :: ISOSURFACE@
!
!      if (.iso_values.allocated) then
!
!         ! Multiple iso values
!         ! Set self isovalue, and copy self
!         .iso_value = .iso_values(1)
!         .cube.iso_value = .iso_values(1)
!
!         isosurface = self
!
!         ! Loop over isovalues and merge surfaces
!         do i = 1,.iso_values.dim
!            isosurface.iso_value = isosurface.iso_values(i)
!            isosurface.cube.iso_value = isosurface.iso_values(i)
!            isosurface.cubify_recursively_this(func)
!            isosurface.surface_iso_values.create(isosurface.n_pt)
!            isosurface.surface_iso_values = isosurface.iso_value
!            self.merge_with(isosurface)
!            isosurface.destroy_info_arrays
!         end
!
!         isosurface.destroy
!
!      else
!
!         ! Single iso value ...
!         .cubify_recursively_this(func)
!
!      end
!
!   end

   subroutine cubify_recursively_m(self,mol,func_m)
   ! Generate the isosurface using the recursive marching cube
   ! algorithm.  Multiple isosurface values are merged into one
   ! isosurface.
      type(ISOSURFACE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      interface
         subroutine func_m(mol,values,pt)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: mol
            VEC(REAL,:), OUT :: values
            MAT(REAL,:,:), IN :: pt
         end subroutine
      end interface
      INT :: i
      type(ISOSURFACE_TYPE), allocatable :: isosurface
      if (allocated(self%iso_values)) then

         ! Multiple iso values
         ! Set self isovalue, and copy self
         self%iso_value = self%iso_values(1)
         self%cube%iso_value = self%iso_values(1)
         isosurface = self

         ! Loop over isovalues and merge surfaces
         do i = 1,size(self%iso_values)
            isosurface%iso_value = isosurface%iso_values(i)
            isosurface%cube%iso_value = isosurface%iso_values(i)
            call cubify_recursively_this_m_(isosurface,mol,func_m)
            call create_(isosurface%surface_iso_values,isosurface%n_pt)
            isosurface%surface_iso_values = isosurface%iso_value
            call merge_with_(self,isosurface)
            call destroy_info_arrays_(isosurface)
         end do
         call destroy_(isosurface)
      else

         ! Single iso value ...
         call cubify_recursively_this_m_(self,mol,func_m)
      end if
   end subroutine


!   cubify_recursively_this(func) :: leaky, private
!   ! Generate the isosurface using the recursive marching cube algorithm.
!      self :: INOUT
!      interface
!         func(values,pt)
!            values :: VEC{REAL}, OUT
!            pt :: MAT{REAL}, IN
!         end
!
!      end
!
!   ENSURE(.plot_grid.width.has_all_equal,"grid widths must be all equal")
!
!      p2 :: MAT{REAL}(3,8)
!      f2 :: VEC{REAL}(8)
!
!      .set_isosurface_info_arrays
!
!      .volume_min = ZERO
!      .volume_max = ZERO
!      .n_skip = (.plot_grid.n_x+2)**3 ! including edges
!      .level = 0
!      .x = 0; .y = 0; .z = 0
!
!      .map.create(256,3,13)
!      .map.set_reverse_search(TRUE)
!
!      PLOT_GRID::make_cube_of_points(.plot_grid,p2,0,1,.del_x,.del_y,.del_z,0,0,0)
!      ISOSURFACE::make_2_cube_of_values(self,f2,func,p2)
!
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      if (.small_map) then; .divide_cubes_small_map(func,f2)
!      else;                 .divide_cubes(func,f2)
!      end
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!      .point.shrink_columns(.n_pt)
!      .point_gradient.shrink_columns(.n_pt)
!      .point_mean_curvature.shrink(.n_pt)
!      .point_gaussian_curvature.shrink(.n_pt)
!      .face.shrink_columns(.n_face)
!      .rotate_gradients
!
!    ! stdout.flush
!    ! stdout.show("Final no. of map keys =",.map.n_keys)
!      .map.destroy
!
!      .set_area
!      .set_volume
!
!   end

   subroutine cubify_recursively_this_m(self,mol,func_m)
   ! Generate the isosurface using the recursive marching cube algorithm.
      type(ISOSURFACE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      interface
         subroutine func_m(mol,values,pt)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: mol
            VEC(REAL,:), OUT :: values
            MAT(REAL,:,:), IN :: pt
         end subroutine
      end interface
      MAT(REAL,3,8) :: p2
      VEC(REAL,8) :: f2
   ENSURE(has_all_equal_(self%plot_grid%width),"ISOSURFACE:cubify_recursively_this_m ... grid widths must be all equal")
      call set_isosurface_info_arrays_(self)
      self%volume_min = ZERO
      self%volume_max = ZERO
      self%n_skip = (self%plot_grid%n_x+2)**3  ! including edges
      self%level = 0
      self%x = 0; self%y = 0; self%z = 0
      call create_(self%map,256,3,13)
      call set_reverse_search_(self%map,TRUE)
      call make_cube_of_points(self%plot_grid,p2,0,1,self%del_x,self%del_y,self%del_z,0,0,0)
      call make_2_cube_of_values_m(self,mol,f2,func_m,p2)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (self%small_map) then; call divide_cubes_small_map_m_(self,mol,func_m,f2)
      else; call divide_cubes_m_(self,mol,func_m,f2)
      end if
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call shrink_columns_(self%point,self%n_pt)
      call shrink_columns_(self%point_gradient,self%n_pt)
      call shrink_(self%point_mean_curvature,self%n_pt)
      call shrink_(self%point_gaussian_curvature,self%n_pt)
      call shrink_columns_(self%face,self%n_face)
      call rotate_gradients_(self)

   ! stdout.flush
   ! stdout.show("Final no. of map keys =",.map.n_keys)
      call destroy_(self%map)
      call set_area_(self)
      call set_volume_(self)
   end subroutine


!   divide_cubes(func,corner_values) :: recursive, private, leaky
!   ! Generate the isosurface for "func" using a recursive marching
!   ! cube algorithm. "parent_cube" is the enclosing parent marching
!   ! cube from which the current cube was generated by a bisection
!   ! method. We keep the "parent_cube" to save on function evaluations
!   ! on the corners.
!      self :: INOUT
!      interface
!         func(values,pt)                       ! The function whose isosurface we make
!            values :: VEC{REAL}, OUT
!            pt :: MAT{REAL}, IN
!         end
!      end
!      corner_values :: VEC{REAL}(8), IN        ! Parent cube corner values
!
!      cube :: MARCHINGCUBE                     ! One of the 8 child cube; becomes next parent
!      kube :: VEC{MARCHINGCUBE}(8)             ! The group of final 8 child "cubes"
!      p3   :: MAT{REAL}(3,27)                  ! The points of all child "cubes"
!      p5   :: MAT{REAL}(3,125)                 ! The points surrounding the final 8-"kube"
!      f3   :: VEC{REAL}(27)                    ! The function values used to make all child "cubes"
!      f5   :: VEC{REAL}(125)                   ! The function values for surrounds of the final 8-"kube"
!      skip :: VEC{BIN}(8)                      ! Tells if we can skip any of the final 8-"kubes"
!      eval :: VEC{BIN}(125)                    ! Tells which of the "p5" points need to be
!                                               ! evaluated using "func".
!      left  :: VEC{INT}(8) = [0,1,0,3,0,5,0,7] ! The lexical index of the left-cube
!      front :: VEC{INT}(8) = [0,0,1,2,0,0,5,6] ! The lexical index of the front-cube
!      below :: VEC{INT}(8) = [0,0,0,0,1,2,3,4] ! The lexical index of the bottom cube
!      ix,iy,iz :: VEC{INT}(8)
!      bit,x,y,z,k :: INT
!      vol :: REAL
!      do_divide,oblique :: BIN
!
!      ! Which cube division level we are at
!      .level = .level + 1
!
!      ! Which bit to be set which describes the cube axis positions
!      bit = .final_level - .level
!
!      ! Oblique axis systems require special interpolation
!      oblique = .plot_grid.use_unit_cell_as_bbox
!
!      ! Oblique axes have different spacings on each axis
!      .del_x = .del_x/TWO
!      if (oblique) then; .del_y = .del_y/TWO; .del_z = .del_z/TWO
!      else;              .del_y = .del_x;     .del_z = .del_x
!      end
!      vol = .del_x*.del_y*.del_z
!
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      ! Divide into 8 cubes and do again
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      if (.level<.final_level) then
!
!         ! Evaluate the parent  points and function values to pass down
!         PLOT_GRID::make_cube_of_points(.plot_grid,p3,0,2,.del_x,.del_y,.del_z,.x,.y,.z)
!         ISOSURFACE::make_3_cube_of_values(self,f3,func,p3,corner_values)
!
!         ! Set the default cube
!         cube = .cube
!       ! .set_default_cube(cube)
!
!         ! Loop over the 8 subcubes
!         do z = 0,1
!         do y = 0,1
!         do x = 0,1
!
!            ! Set "cube" to be one of the child cubes

!          !                        f3(x:x+1,y:y+1,z:z+1))
!            MARCHINGCUBE::set_vertex_info3(cube,p3,f3,x,y,z)
!
!            ! Which vertices are inside or outside isosurface?
!            cube.set_case_info
!
!            ! Determine of we must divide this child further
!            do_divide = (.level<.scan_level) OR cube.is_nearly_on_surface(.voxel_proximity_factor)
!
!            ! If dividing, set the child cube coordinates and do it
!            if (do_divide) then
!               .x = ibclr(.x,bit); if (x==1) .x = ibset(.x,bit) ! Set cube coords
!               .y = ibclr(.y,bit); if (y==1) .y = ibset(.y,bit)
!               .z = ibclr(.z,bit); if (z==1) .z = ibset(.z,bit)
!               .divide_cubes(func,cube.vertex_fn_value)         ! <<<<<< recursive here
!               .x = ibclr(.x,bit)                               ! Clear the cube coords
!               .y = ibclr(.y,bit)
!               .z = ibclr(.z,bit)
!            ! If not dividing get a rough estimate of the volume
!            else
!               if (cube.is_inside_surface) then
!                  .volume_min = .volume_min + vol
!                  .volume_max = .volume_max + vol
!               end
!            end
!
!         end
!         end
!         end
!
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      ! Divide by 8 the last time, and triangulate
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      else
!
!         ! A 5-cube of points needed for gradients and hessians
!         ! Start with a 3-cube of values for now
!         PLOT_GRID::make_cube_of_points(.plot_grid,p3,0,2,.del_x,.del_y,.del_z,.x,.y,.z)
!         ISOSURFACE::make_3_cube_of_values(self,f3,func,p3,corner_values)
!
!         ! Set the default cube, for interpolations
!         cube = .cube
!       ! .set_default_cube(cube)
!
!         ! Tells which of the points in the 5-cube must be evaluated
!         eval = FALSE
!
!         ! List of cubes which are not triangulated.
!         skip = TRUE
!
!         ! Indices of previous cubes in the map table which have been
!         ! triangulated; no need to re-triangulate shared faces.
!         ix = 0; iy = 0; iz = 0
!
!         ! The child cube index, in range 1 ... 8
!         k = 0
!
!         ! Loop over the child cubes the first time; triangulate later
!         ! when we know which cubes to do, which helps avoid function
!         ! evaluations on the 5 x 5 x 5 cube
!         do z = 0,1
!         do y = 0,1
!         do x = 0,1
!
!            k = k + 1
!
!            ! Set child cube info; store only needed parts in kube(k)
!            MARCHINGCUBE::set_vertex_info3(cube,p3,f3,x,y,z)
!
!            ! Which cube vertices are inside or outside isosurface?
!            cube.set_case_info
!            if (cube.is_inside_surface)      .volume_min = .volume_min + vol
!            if (NOT cube.is_outside_surface) .volume_max = .volume_max + vol
!
!            ! Is the cube on the surface?
!            if (NOT cube.is_on_surface) cycle
!
!            ! We are on the surface: set the cube coords
!            .x = .x + x
!            .y = .y + y
!            .z = .z + z
!
!            ! Don't skip this cube
!            skip(k) = FALSE
!
!            ! Which cube edges cut the surface? Hence, which cube vertices are
!            ! needed? Hence, what triangles are to be made?
!            cube.skip_bit_string = 0
!            cube.set_triangulation_info
!
!            ! Find out which faces of the cube should have been triangulated
!            ! already, and store in ix, iy, iz arrays, and store in the
!            ! skip_bit_string for hessian evaluation
!            if (cube.has_left_face_on_surface) then
!               if (x>0) then
!                  ix(k) = -1
!                  cube.set_left_skip_bit_string  ! Left child cube exists
!               else if (.x>0 AND .map.has_key([.x-1,.y,.z],index=ix(k))) then
!                  cube.set_left_skip_bit_string  ! Left map cube exists
!               end
!            end
!            if (cube.has_front_face_on_surface) then
!               if (y>0) then
!                  iy(k) = -1
!                  cube.set_front_skip_bit_string ! Front child cube exists
!               else if (.y>0 AND .map.has_key([.x,.y-1,.z],index=iy(k))) then
!                  cube.set_front_skip_bit_string ! Front map cube exists
!               end
!            end
!            if (cube.has_lower_face_on_surface) then
!               if (z>0) then
!                  iz(k) = -1
!                  cube.set_below_skip_bit_string ! Lower child cube exists
!               else if (.z>0 AND .map.has_key([.x,.y,.z-1],index=iz(k))) then
!                  cube.set_below_skip_bit_string ! Lower map cube exists
!               end
!            end
!
!            ! Set which edges to interpolate, accounting for those
!            ! already done above in skip_bit_string
!            cube.set_cube_bit_string
!
!            ! Set which of the 5 x 5 x 5 points are needed for hessian
!            ! and gradient.
!            if (NOT oblique) then;
!               MARCHINGCUBE::set_hessian_eval_array(cube,eval,x,y,z)
!            else
!               MARCHINGCUBE::set_gradient_eval_array(cube,eval,x,y,z)
!            end
!
!            ! Store the needed parts of cube for later interpolation
!            kube(k) = cube
!          ! kube(k).cube_bit_string = cube.cube_bit_string
!          ! kube(k).edge_bit_string = cube.edge_bit_string
!          ! kube(k).skip_bit_string = cube.skip_bit_string
!
!
!            ! Clear the cube coords
!            .x = .x - x
!            .y = .y - y
!            .z = .z - z
!         end
!         end
!         end
!
!         ! Don't evaluate the 3 x 3 x 3 points/values -- already done
!       ! eval(2:4,2:4,2:4) = FALSE
!       ! f5(1:3,1:3,1:3) = f3
!         ISOSURFACE::initialize_eval_f5(eval,f5,f3)
!
!         ! Now evaluate the function only at the needed points. The
!         ! previous loops over the 8 kubes was all for this.
!         PLOT_GRID::make_cube_of_points(.plot_grid,p5,-1,3,.del_x,.del_y,.del_z,.x,.y,.z,eval)
!         ISOSURFACE::make_5_cube_of_values(self,f5,func,p5,eval)
!
!         ! Loop over the child cubes the second time, and do the
!         ! triangulation.
!         k = 0
!         do z = 0,1
!         do y = 0,1
!         do x = 0,1
!
!            k = k + 1
!            if (skip(k)) cycle
!
!            ! Get stored cobe
!            cube = kube(k)
!
!            ! Evaluate the gradient and hessian; skips in cube_bit_string
!          ! cube.set_hessian_info(f5(x:x+3,y:y+3,z:z+3))
!            MARCHINGCUBE::set_hessian_info5(cube,f5,x,y,z)
!
!            ! Set the cube coordinates; for storing in map table
!            .x = .x + x
!            .y = .y + y
!            .z = .z + z
!
!            ! Use previously triangulated cubes if possible, avoid
!            ! looking in the map table
!            if (ix(k)/=0) then
!               if (ix(k)<0) then; cube.set_left_info(kube(left(k)).edge_vertex_index)
!               else;              cube.set_left_info(.map.values(:,ix(k)))
!               end
!            end
!            if (iy(k)/=0) then
!               if (iy(k)<0) then; cube.set_front_info(kube(front(k)).edge_vertex_index)
!               else;              cube.set_front_info(.map.values(:,iy(k)))
!               end
!            end
!            if (iz(k)/=0) then
!               if (iz(k)<0) then; cube.set_below_info(kube(below(k)).edge_vertex_index)
!               else;              cube.set_below_info(.map.values(:,iz(k)))
!               end
!            end
!
!            ! Set the cube vertex index to the last vertex
!            cube.set_n_pt(.n_pt)
!
!            ! Finally, do the interpolation
!            if (oblique) then; cube.interpolate_oblique_faces
!            else;              cube.interpolate_faces
!            end
!
!            ! Copy the cube interpolation info to the isosurface
!            .append_new_face_info(cube)
!
!            ! Append the cube vertex info to the map only if needed

!               .map.append_pair([.x,.y,.z],cube.edge_vertex_index)
!
!
!            ! Might need to keep this, it may share a face

!               kube(k).edge_vertex_index = cube.edge_vertex_index
!
!            ! Clear the cube coords
!            .x = .x - x
!            .y = .y - y
!            .z = .z - z
!         end
!         end
!         end
!
!      end
!
!      ! Go up to the next level: double side lengths, decrease level
!      .del_x = TWO*.del_x
!      if (oblique) then; .del_y = TWO*.del_y; .del_z = TWO*.del_z
!      else;              .del_y = .del_x;     .del_z = .del_x
!      end
!
!      .level = .level - 1
!
!   end

   recursive subroutine divide_cubes_m(self,mol,func_m,corner_values)
   ! Generate the isosurface for "func" using a recursive marching
   ! cube algorithm. "parent_cube" is the enclosing parent marching
   ! cube from which the current cube was generated by a bisection
   ! method. We keep the "parent_cube" to save on function evaluations
   ! on the corners.
      type(ISOSURFACE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      interface
         subroutine func_m(mol,values,pt)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: mol
            VEC(REAL,:), OUT :: values
            MAT(REAL,:,:), IN :: pt
         end subroutine
      end interface
      VEC(REAL,8), IN :: corner_values  ! Parent cube corner values
      type(MARCHINGCUBE_TYPE) :: cube  ! One of the 8 child cube; becomes next parent
      VEC(type(MARCHINGCUBE_TYPE),8) :: kube  ! The group of final 8 child "cubes"
      MAT(REAL,3,27) :: p3  ! The points of all child "cubes"
      MAT(REAL,3,125) :: p5  ! The points surrounding the final 8-"kube"
      VEC(REAL,27) :: f3  ! The function values used to make all child "cubes"
      VEC(REAL,125) :: f5  ! The function values for surrounds of the final 8-"kube"
      VEC(BIN,8) :: skip  ! Tells if we can skip any of the final 8-"kubes"
      VEC(BIN,125) :: eval  ! Tells which of the "p5" points need to be
                                                ! evaluated using "func".
      VEC(INT,8) :: left = [0,1,0,3,0,5,0,7]  ! The lexical index of the left-cube
      VEC(INT,8) :: front = [0,0,1,2,0,0,5,6]  ! The lexical index of the front-cube
      VEC(INT,8) :: below = [0,0,0,0,1,2,3,4]  ! The lexical index of the bottom cube
      VEC(INT,8) :: ix,iy,iz
      INT :: bit,x,y,z,k
      REAL :: vol
      BIN :: do_divide,oblique

      ! Which cube division level we are at
      self%level = self%level+1

      ! Which bit to be set which describes the cube axis positions
      bit = self%final_level-self%level

      ! Oblique axis systems require special interpolation
      oblique = self%plot_grid%use_unit_cell_as_bbox

      ! Oblique axes have different spacings on each axis
      self%del_x = self%del_x/TWO
      if (oblique) then; self%del_y = self%del_y/TWO; self%del_z = self%del_z/TWO
      else; self%del_y = self%del_x; self%del_z = self%del_x
      end if
      vol = self%del_x*self%del_y*self%del_z

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Divide into 8 cubes and do again
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (self%level<self%final_level) then

         ! Evaluate the parent  points and function values to pass down
         call make_cube_of_points(self%plot_grid,p3,0,2,self%del_x,self%del_y,self%del_z,self%x,self%y,self%z)
         call make_3_cube_of_values_m(self,mol,f3,func_m,p3,corner_values)

         ! Set the default cube
         cube = self%cube
      ! .set_default_cube(cube)

         ! Loop over the 8 subcubes
         do z = 0,1
            do y = 0,1
               do x = 0,1

            ! Set "cube" to be one of the child cubes
            ! cube.set_vertex_info(

            !    f3(x:x+1,y:y+1,z:z+1)  )
                  call set_vertex_info3(cube,p3,f3,x,y,z)

            ! Which vertices are inside or outside isosurface?
                  call set_case_info_(cube)

            ! Determine of we must divide this child further
                  do_divide = self%level<self%scan_level OR is_nearly_on_surface_(cube,self%voxel_proximity_factor)

            ! If dividing, set the child cube coordinates and do it
                  if (do_divide) then
                     self%x = ibclr(self%x,bit); if (x==1) self%x = ibset(self%x,bit)  ! Set cube coords
                     self%y = ibclr(self%y,bit); if (y==1) self%y = ibset(self%y,bit)
                     self%z = ibclr(self%z,bit); if (z==1) self%z = ibset(self%z,bit)
                     call divide_cubes_m_(self,mol,func_m,cube%vertex_fn_value)  ! <<<<<< recursive here
                     self%x = ibclr(self%x,bit)  ! Clear the cube coords
                     self%y = ibclr(self%y,bit)
                     self%z = ibclr(self%z,bit)
            ! If not dividing get a rough estimate of the volume
                  else
                     if (is_inside_surface_(cube)) then
                        self%volume_min = self%volume_min+vol
                        self%volume_max = self%volume_max+vol
                     end if
                  end if
               end do
            end do
         end do

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Divide by 8 the last time, and triangulate
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      else

         ! A 5-cube of points needed for gradients and hessians
         ! Start with a 3-cube of values for now
         call make_cube_of_points(self%plot_grid,p3,0,2,self%del_x,self%del_y,self%del_z,self%x,self%y,self%z)
         call make_3_cube_of_values_m(self,mol,f3,func_m,p3,corner_values)

         ! Set the default cube, for interpolations
         cube = self%cube

         ! Tells which of the points in the 5-cube must be evaluated
         eval = FALSE

         ! List of cubes which are not triangulated.
         skip = TRUE

         ! Indices of previous cubes in the map table which have been
         ! triangulated; no need to re-triangulate shared faces.
         ix = 0; iy = 0; iz = 0

         ! The child cube index, in range 1 ... 8
         k = 0

         ! Loop over the child cubes the first time; triangulate later
         ! when we know which cubes to do, which helps avoid function
         ! evaluations on the 5 x 5 x 5 cube
         do z = 0,1
            do y = 0,1
               do x = 0,1
                  k = k+1

            ! Set child cube info; store only needed parts in kube(k)
                  call set_vertex_info3(cube,p3,f3,x,y,z)

            ! Which cube vertices are inside or outside isosurface?
                  call set_case_info_(cube)
                  if (is_inside_surface_(cube)) self%volume_min = self%volume_min+vol
                  if (NOT is_outside_surface_(cube)) self%volume_max = self%volume_max+vol

            ! Is the cube on the surface?
                  if (NOT is_on_surface_(cube)) cycle

            ! We are on the surface: set the cube coords
                  self%x = self%x+x
                  self%y = self%y+y
                  self%z = self%z+z

            ! Don't skip this cube
                  skip(k) = FALSE

            ! Which cube edges cut the surface? Hence, which cube vertices are
            ! needed? Hence, what triangles are to be made?
                  cube%skip_bit_string = 0
                  call set_triangulation_info_(cube)

            ! Find out which faces of the cube should have been triangulated
            ! already, and store in ix, iy, iz arrays, and store in the
            ! skip_bit_string for hessian evaluation
                  if (has_left_face_on_surface_(cube)) then
                     if (x>0) then
                        ix(k) = -1
                        call set_left_skip_bit_string_(cube)  ! Left child cube exists
                     else if (self%x>0 AND has_key_(self%map,[self%x-1,self%y,self%z],index=ix(k))) then
                        call set_left_skip_bit_string_(cube)  ! Left map cube exists
                     end if
                  end if
                  if (has_front_face_on_surface_(cube)) then
                     if (y>0) then
                        iy(k) = -1
                        call set_front_skip_bit_string_(cube)  ! Front child cube exists
                     else if (self%y>0 AND has_key_(self%map,[self%x,self%y-1,self%z],index=iy(k))) then
                        call set_front_skip_bit_string_(cube)  ! Front map cube exists
                     end if
                  end if
                  if (has_lower_face_on_surface_(cube)) then
                     if (z>0) then
                        iz(k) = -1
                        call set_below_skip_bit_string_(cube)  ! Lower child cube exists
                     else if (self%z>0 AND has_key_(self%map,[self%x,self%y,self%z-1],index=iz(k))) then
                        call set_below_skip_bit_string_(cube)  ! Lower map cube exists
                     end if
                  end if

            ! Set which edges to interpolate, accounting for those
            ! already done above in skip_bit_string
                  call set_cube_bit_string_(cube)

            ! Set which of the 5 x 5 x 5 points are needed for hessian
            ! and gradient.
                  if (NOT oblique) then
                     call set_hessian_eval_array(cube,eval,x,y,z)
                  else
                     call set_gradient_eval_array(cube,eval,x,y,z)
                  end if

            ! Store the needed parts of cube for later interpolation
                  kube(k) = cube

            ! Clear the cube coords
                  self%x = self%x-x
                  self%y = self%y-y
                  self%z = self%z-z
               end do
            end do
         end do

         ! Don't evaluate the 3 x 3 x 3 points/values -- already done
      ! eval(2:4,2:4,2:4) = FALSE
      ! f5(1:3,1:3,1:3)   = f3
         call initialize_eval_f5(eval,f5,f3)

         ! Now evaluate the function only at the needed points. The
         ! previous loops over the 8 kubes was all for this.
         call make_cube_of_points(self%plot_grid,p5,-1,3,self%del_x,self%del_y,self%del_z,self%x,self%y,self%z,eval)
         call make_5_cube_of_values_m(self,mol,f5,func_m,p5,eval)

         ! Loop over the child cubes the second time, and do the
         ! triangulation.
         k = 0
         do z = 0,1
            do y = 0,1
               do x = 0,1
                  k = k+1
                  if (skip(k)) cycle

            ! Get stored cobe
                  cube = kube(k)

            ! Evaluate the gradient and hessian; skips in cube_bit_string
         ! cube.set_hessian_info(f5(x:x+3,y:y+3,z:z+3))
                  call set_hessian_info5(cube,f5,x,y,z)

            ! Set the cube coordinates; for storing in map table
                  self%x = self%x+x
                  self%y = self%y+y
                  self%z = self%z+z

            ! Use previously triangulated cubes if possible, avoid
            ! looking in the map table
                  if (ix(k)/=0) then
                     if (ix(k)<0) then; call set_left_info_(cube,kube(left(k))%edge_vertex_index)
                     else; call set_left_info_(cube,self%map%values(:,ix(k)))
                     end if
                  end if
                  if (iy(k)/=0) then
                     if (iy(k)<0) then; call set_front_info_(cube,kube(front(k))%edge_vertex_index)
                     else; call set_front_info_(cube,self%map%values(:,iy(k)))
                     end if
                  end if
                  if (iz(k)/=0) then
                     if (iz(k)<0) then; call set_below_info_(cube,kube(below(k))%edge_vertex_index)
                     else; call set_below_info_(cube,self%map%values(:,iz(k)))
                     end if
                  end if

            ! Set the cube vertex index to the last vertex
                  call set_n_pt_(cube,self%n_pt)

            ! Finally, do the interpolation
                  if (oblique) then; call interpolate_oblique_faces_(cube)
                  else; call interpolate_faces_(cube)
                  end if

            ! Copy the cube interpolation info to the isosurface
                  call append_new_face_info_(self,cube)

            ! Append the cube vertex info to the map only if needed
                  if (has_reusable_edge_vertices_(cube)) call append_pair_(self%map,[self%x,self%y,self%z],cube%edge_vertex_index)

            ! Might need to keep this, it may share a face
                  if (x==0 OR y==0 OR z==0) kube(k)%edge_vertex_index = cube%edge_vertex_index

            ! Clear the cube coords
                  self%x = self%x-x
                  self%y = self%y-y
                  self%z = self%z-z
               end do
            end do
         end do
      end if

      ! Go up to the next level: double side lengths, decrease level
      self%del_x = TWO*self%del_x
      if (oblique) then; self%del_y = TWO*self%del_y; self%del_z = TWO*self%del_z
      else; self%del_y = self%del_x; self%del_z = self%del_x
      end if
      self%level = self%level-1
   end subroutine


!   divide_cubes_small_map(func,corner_values) :: recursive, private, leaky
!   ! Generate the isosurface for "func" using a recursive marching
!   ! cube algorithm. "parent_cube" is the enclosing parent marching
!   ! cube from which the current cube was generated by a bisection
!   ! method. We keep the "parent_cube" to save on function evaluations
!   ! on the corners.  NOTE: this version keeps a small map table but
!   ! it makes no difference to timings on linux
!      self :: INOUT
!      interface
!         func(values,pt)                       ! The function whose isosurface we make
!            values :: VEC{REAL}, OUT
!            pt :: MAT{REAL}, IN
!         end
!      end
!      corner_values :: VEC{REAL}(8), IN        ! Parent cube corner values
!
!      cube :: MARCHINGCUBE                     ! One of the 8 child cube; becomes next parent
!      kube :: VEC{MARCHINGCUBE}(8)             ! The group of final 8 child "cubes"
!      p3   :: MAT{REAL}(3,27)                  ! The points of all child "cubes"
!      p5   :: MAT{REAL}(3,125)                 ! The interior and surrounds of the final 8-"kube"
!      f3   :: MAT3{REAL}(0:2,0:2,0:2)          ! The interior values used to make all child "cubes"
!      f5   :: MAT3{REAL}(0:4,0:4,0:4)          ! The interior and surrounding values of the final 8-"kube"
!      skip :: VEC{BIN}(8)                      ! Tells if we can skip any of the final 8-"kubes"
!      eval :: MAT3{BIN}(5,5,5)                 ! Tells which of the "p5" points need to be
!                                               ! evaluated using "func".
!      left  :: VEC{INT}(8) = [0,1,0,3,0,5,0,7] ! The lexical index of the left-cube
!      front :: VEC{INT}(8) = [0,0,1,2,0,0,5,6] ! The lexical index of the front-cube
!      below :: VEC{INT}(8) = [0,0,0,0,1,2,3,4] ! The lexical index of the bottom cube
!      ix,iy,iz,ik :: VEC{INT}(8)
!      dead_list :: VEC{INT}(24)
!      bit,x,y,z,k,kk,c,n :: INT
!      vol :: REAL
!      do_divide,oblique :: BIN
!
!      ! Which cube division level we are at
!      .level = .level + 1
!
!      ! Which bit to be set which describes the cube axis positions
!      bit = .final_level - .level
!
!      ! Oblique axis systems require special interpolation
!      oblique = .plot_grid.use_unit_cell_as_bbox
!
!      ! Oblique axes have different spacings on each axis
!      .del_x = .del_x/TWO
!      if (oblique) then; .del_y = .del_y/TWO; .del_z = .del_z/TWO
!      else;              .del_y = .del_x;     .del_z = .del_x
!      end
!      vol = .del_x*.del_y*.del_z
!
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      ! Divide into 8 cubes and do again
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      if (.level<.final_level) then
!
!         ! Evaluate the parent  points and function values to pass down
!         PLOT_GRID::make_cube_of_points(.plot_grid,p3,0,2,.del_x,.del_y,.del_z,.x,.y,.z)
!         ISOSURFACE::make_3_cube_of_values(self,f3,func,p3,corner_values)
!
!         ! Set the default cube
!         cube = .cube
!       ! .set_default_cube(cube)
!
!         ! Loop over the 8 subcubes
!         do z = 0,1
!         do y = 0,1
!         do x = 0,1
!
!            ! Set "cube" to be one of the child cubes

!          !                        f3(x:x+1,y:y+1,z:z+1))
!            MARCHINGCUBE::set_vertex_info3(cube,p3,f3,x,y,z)
!
!            ! Which vertices are inside or outside isosurface?
!            cube.set_case_info
!
!            ! Determine of we must divide this child further
!            do_divide = (.level<.scan_level) OR cube.is_nearly_on_surface(.voxel_proximity_factor)
!
!            ! If dividing, set the child cube coordinates and do it
!            if (do_divide) then
!               .x = ibclr(.x,bit); if (x==1) .x = ibset(.x,bit) ! Set cube coords
!               .y = ibclr(.y,bit); if (y==1) .y = ibset(.y,bit)
!               .z = ibclr(.z,bit); if (z==1) .z = ibset(.z,bit)
!               .divide_cubes_small_map(func,cube.vertex_fn_value) ! <<<<<< recursive here
!               .x = ibclr(.x,bit)                                 ! Clear the cube coords
!               .y = ibclr(.y,bit)
!               .z = ibclr(.z,bit)
!            ! If not dividing get a rough estimate of the volume
!            else
!               if (cube.is_inside_surface) then
!                  .volume_min = .volume_min + vol
!                  .volume_max = .volume_max + vol
!               end
!            end
!
!         end
!         end
!         end
!
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      ! Divide by 8 the last time, and triangulate
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      else
!
!         ! A 5-cube of points needed for gradients and hessians
!         ! Start with a 3-cube of values for now
!         PLOT_GRID::make_cube_of_points(.plot_grid,p3,0,2,.del_x,.del_y,.del_z,.x,.y,.z)
!         ISOSURFACE::make_3_cube_of_values(self,f3,func,p3,corner_values)
!
!         ! Set the default cube, for interpolations
!         cube = .cube
!       ! .set_default_cube(cube)
!
!         ! Tells which of the points in the 5-cube must be evaluated
!         eval = FALSE
!
!         ! List of cubes which are not triangulated.
!         skip = TRUE
!
!         ! Indices of previous cubes in the map table which have been
!         ! triangulated; no need to re-triangulate shared faces.
!         ix = 0; iy = 0; iz = 0; ik = 0
!
!         ! The child cube index, in range 1 ... 8
!         k = 0
!
!
!         ! Loop over the child cubes the first time; triangulate later
!         ! when we know which cubes to do, which helps avoid function
!         ! evaluations on the 5 x 5 x 5 cube
!         do z = 0,1
!         do y = 0,1
!         do x = 0,1
!
!            k = k + 1
!
!            ! Set child cube info; store only needed parts in kube(k)

!          !                        f5(x+1:x+2,y+1:y+2,z+1:z+2))
!            MARCHINGCUBE::set_vertex_info3(cube,p3,f3,x,y,z)
!
!            ! Which cube vertices are inside or outside isosurface?
!            cube.set_case_info
!            if (cube.is_inside_surface)      .volume_min = .volume_min + vol
!            if (NOT cube.is_outside_surface) .volume_max = .volume_max + vol
!
!            ! Is the cube on the surface?
!            if (NOT cube.is_on_surface) cycle
!
!            ! We are on the surface: set the cube coords
!            .x = .x + x
!            .y = .y + y
!            .z = .z + z
!
!            ! Don't skip this cube
!            skip(k) = FALSE
!
!            ! Which cube edges cut the surface? Hence, which cube
!            ! vertices are needed? Hence, what triangles are to be
!            ! made?
!            cube.skip_bit_string = 0
!            cube.set_triangulation_info
!
!            ! The number of times that the cube will be re-used in
!            ! the map table
!            c = 0
!            if (cube.has_right_face_on_surface) then
!               if (x>0) then; c = c + 1
!               else;          ik(k) = 1
!               end
!            end
!            if (cube.has_back_face_on_surface) then
!               if (y>0) then; c = c + 1
!               else;          ik(k) = 1
!               end
!            end
!            if (cube.has_upper_face_on_surface) then
!               if (z>0) then; c = c + 1
!               else;          ik(k) = 1
!               end
!            end
!            cube.edge_vertex_index(12) = c
!
!            ! Find out which faces of the cube should have been triangulated
!            ! already, and store in ix, iy, iz arrays, and store in the
!            ! skip_bit_string for hessian evaluation
!            if (cube.has_left_face_on_surface) then
!               if (x>0) then
!                  ix(k) = -1
!                  cube.set_left_skip_bit_string  ! Left child cube exists
!               else if (.x>0 AND .map.has_key([.x-1,.y,.z],index=ix(k))) then
!                  cube.set_left_skip_bit_string  ! Left map cube exists
!               end
!               ENSURE(ix(k)/=0,"no left face indices found")
!            end
!            if (cube.has_front_face_on_surface) then
!               if (y>0) then
!                  iy(k) = -1
!                  cube.set_front_skip_bit_string ! Front child cube exists
!               else if (.y>0 AND .map.has_key([.x,.y-1,.z],index=iy(k))) then
!                  cube.set_front_skip_bit_string ! Front map cube exists
!               end
!               ENSURE(iy(k)/=0,"no front face indices found")
!            end
!            if (cube.has_lower_face_on_surface) then
!               if (z>0) then
!                  iz(k) = -1
!                  cube.set_below_skip_bit_string ! Lower child cube exists
!               else if (.z>0 AND .map.has_key([.x,.y,.z-1],index=iz(k))) then
!                  cube.set_below_skip_bit_string ! Lower map cube exists
!               end
!               ENSURE(iz(k)/=0,"no lower face indices found")
!            end
!
!            ! Set which edges to interpolate, accounting for those
!            ! already done above in skip_bit_string
!            cube.set_cube_bit_string
!
!            ! Set which of the 5 x 5 x 5 points are needed for hessian
!            ! and gradient.
!            MARCHINGCUBE::set_hessian_eval_array(cube,eval,x,y,z)
!
!            ! Store the needed parts of cube for later interpolation
!            kube(k) = cube
!
!            ! Clear the cube coords
!            .x = .x - x
!            .y = .y - y
!            .z = .z - z
!         end
!         end
!         end
!
!         ! Don't evaluate the 3 x 3 x 3 points/values -- already done
!         eval(2:4,2:4,2:4) = FALSE
!         f5(1:3,1:3,1:3) = f3
!
!         ! Now evaluate the function only at the needed points. The
!         ! previous loops over the 8 kubes was all for this.
!         PLOT_GRID::make_cube_of_points(.plot_grid,p5,-1,3,.del_x,.del_y,.del_z,.x,.y,.z,eval)
!         ISOSURFACE::make_5_cube_of_values(self,f5,func,p5,eval)
!
!         ! Loop over the child cubes the second time, and do the
!         ! triangulation.
!         k = 0
!         do z = 0,1
!         do y = 0,1
!         do x = 0,1
!
!            k = k + 1
!            if (skip(k)) cycle
!
!            ! Get stored cobe
!            cube = kube(k)
!
!            ! Evaluate the gradient and hessian
!            cube.set_hessian_info(f5(x:x+3,y:y+3,z:z+3))
!
!            ! Set the cube coordinates; for storing in map table
!            .x = .x + x
!            .y = .y + y
!            .z = .z + z
!
!            ! Use previously triangulated cubes if possible, avoid
!            ! looking in the map table
!            kk = ix(k)
!            if (kk/=0) then
!               if (kk<0) then
!                  kk = left(k)
!                  cube.set_left_info(kube(kk).edge_vertex_index)
!               else
!                  cube.set_left_info(.map.values(:,kk))
!                  .map.values(13,kk) = .map.values(13,kk) - 1
!               end
!            end
!            kk = iy(k)
!            if (kk/=0) then
!               if (kk<0) then
!                  kk = front(k)
!                  cube.set_front_info(kube(kk).edge_vertex_index)
!               else
!                  cube.set_front_info(.map.values(:,kk))
!                  .map.values(13,kk) = .map.values(13,kk) - 1
!               end
!            end
!            kk = iz(k)
!            if (kk/=0) then
!               if (kk<0) then
!                  kk = below(k)
!                  cube.set_below_info(kube(kk).edge_vertex_index)
!               else
!                  cube.set_below_info(.map.values(:,kk))
!                  .map.values(13,kk) = .map.values(13,kk) - 1
!               end
!            end
!
!            ! Set the cube vertex index to the last vertex
!            cube.set_n_pt(.n_pt)
!
!            ! Finally, do the interpolation
!            if (oblique) then; cube.interpolate_oblique_faces
!            else;              cube.interpolate_faces
!            end
!
!            ! Copy the cube interpolation info to the isosurface
!            .append_new_face_info(cube)
!
!            ! Append the cube vertex info to the map only if needed

!               .map.append_pair([.x,.y,.z],cube.edge_vertex_index)
!
!            ! Might need to keep this from interpolation, it may share a face

!               kube(k).edge_vertex_index = cube.edge_vertex_index
!
!            ! Clear the cube coords
!            .x = .x - x
!            .y = .y - y
!            .z = .z - z
!         end
!         end
!         end
!
!         ! Remove dead edge_vertex_indices from map table
!         n = 0
!         do k = 1,8
!            kk = ix(k)
!            if (kk>0 AND .map.values(13,kk)==0) then
!               n = n + 1
!               dead_list(n) = kk
!            end
!            kk = iy(k)
!            if (kk>0 AND .map.values(13,kk)==0) then
!               n = n + 1
!               dead_list(n) = kk
!            end
!            kk = iz(k)
!            if (kk>0 AND .map.values(13,kk)==0) then
!               n = n + 1
!               dead_list(n) = kk
!            end
!         end
!         if (n>0) .map.delete_items(dead_list(1:n))
!
!      end
!
!      ! Go up to the next level: double side lengths, decrease level
!      .del_x = TWO*.del_x
!      if (oblique) then; .del_y = TWO*.del_y; .del_z = TWO*.del_z
!      else;              .del_y = .del_x;     .del_z = .del_x
!      end
!      .level = .level - 1
!   end

   recursive subroutine divide_cubes_small_map_m(self,mol,func_m,corner_values)
   ! Generate the isosurface for "func" using a recursive marching
   ! cube algorithm. "parent_cube" is the enclosing parent marching
   ! cube from which the current cube was generated by a bisection
   ! method. We keep the "parent_cube" to save on function evaluations
   ! on the corners.  NOTE: this version keeps a small map table but
   ! it makes no difference to timings on linux
      type(ISOSURFACE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      interface
         subroutine func_m(mol,values,pt)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: mol
            VEC(REAL,:), OUT :: values
            MAT(REAL,:,:), IN :: pt
         end subroutine
      end interface
      VEC(REAL,8), IN :: corner_values  ! Parent cube corner values
      type(MARCHINGCUBE_TYPE) :: cube  ! One of the 8 child cube; becomes next parent
      VEC(type(MARCHINGCUBE_TYPE),8) :: kube  ! The group of final 8 child "cubes"
      MAT(REAL,3,27) :: p3  ! The points of all child "cubes"
      MAT(REAL,3,125) :: p5  ! The interior and surrounds of the final 8-"kube"
      MAT3(REAL,0:2,0:2,0:2) :: f3  ! The interior values used to make all child "cubes"
      MAT3(REAL,0:4,0:4,0:4) :: f5  ! The interior and surrounding values of the final 8-"kube"
      VEC(BIN,8) :: skip  ! Tells if we can skip any of the final 8-"kubes"
      MAT3(BIN,5,5,5) :: eval  ! Tells which of the "p5" points need to be
                                                ! evaluated using "func".
      VEC(INT,8) :: left = [0,1,0,3,0,5,0,7]  ! The lexical index of the left-cube
      VEC(INT,8) :: front = [0,0,1,2,0,0,5,6]  ! The lexical index of the front-cube
      VEC(INT,8) :: below = [0,0,0,0,1,2,3,4]  ! The lexical index of the bottom cube
      VEC(INT,8) :: ix,iy,iz,ik
      VEC(INT,24) :: dead_list
      INT :: bit,x,y,z,k,kk,c,n
      REAL :: vol
      BIN :: do_divide,oblique

      ! Which cube division level we are at
      self%level = self%level+1

      ! Which bit to be set which describes the cube axis positions
      bit = self%final_level-self%level

      ! Oblique axis systems require special interpolation
      oblique = self%plot_grid%use_unit_cell_as_bbox

      ! Oblique axes have different spacings on each axis
      self%del_x = self%del_x/TWO
      if (oblique) then; self%del_y = self%del_y/TWO; self%del_z = self%del_z/TWO
      else; self%del_y = self%del_x; self%del_z = self%del_x
      end if
      vol = self%del_x*self%del_y*self%del_z

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Divide into 8 cubes and do again
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (self%level<self%final_level) then

         ! Evaluate the parent  points and function values to pass down
         call make_cube_of_points(self%plot_grid,p3,0,2,self%del_x,self%del_y,self%del_z,self%x,self%y,self%z)
         call make_3_cube_of_values_m(self,mol,f3,func_m,p3,corner_values)

         ! Set the default cube
         cube = self%cube
      ! .set_default_cube(cube)

         ! Loop over the 8 subcubes
         do z = 0,1
            do y = 0,1
               do x = 0,1

            ! Set "cube" to be one of the child cubes

          !                        f3(x:x+1,y:y+1,z:z+1))
                  call set_vertex_info3(cube,p3,f3,x,y,z)

            ! Which vertices are inside or outside isosurface?
                  call set_case_info_(cube)

            ! Determine of we must divide this child further
                  do_divide = (self%level<self%scan_level) OR is_nearly_on_surface_(cube,self%voxel_proximity_factor)

            ! If dividing, set the child cube coordinates and do it
                  if (do_divide) then
                     self%x = ibclr(self%x,bit); if (x==1) self%x = ibset(self%x,bit)  ! Set cube coords
                     self%y = ibclr(self%y,bit); if (y==1) self%y = ibset(self%y,bit)
                     self%z = ibclr(self%z,bit); if (z==1) self%z = ibset(self%z,bit)
                     call divide_cubes_small_map_m_(self,mol,func_m,cube%vertex_fn_value)  ! <<<<<< recursive here
                     self%x = ibclr(self%x,bit)  ! Clear the cube coords
                     self%y = ibclr(self%y,bit)
                     self%z = ibclr(self%z,bit)
            ! If not dividing get a rough estimate of the volume
                  else
                     if (is_inside_surface_(cube)) then
                        self%volume_min = self%volume_min+vol
                        self%volume_max = self%volume_max+vol
                     end if
                  end if
               end do
            end do
         end do

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Divide by 8 the last time, and triangulate
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      else

         ! A 5-cube of points needed for gradients and hessians
         ! Start with a 3-cube of values for now
         call make_cube_of_points(self%plot_grid,p3,0,2,self%del_x,self%del_y,self%del_z,self%x,self%y,self%z)
         call make_3_cube_of_values_m(self,mol,f3,func_m,p3,corner_values)

         ! Set the default cube, for interpolations
         cube = self%cube
      ! .set_default_cube(cube)

         ! Tells which of the points in the 5-cube must be evaluated
         eval = FALSE

         ! List of cubes which are not triangulated.
         skip = TRUE

         ! Indices of previous cubes in the map table which have been
         ! triangulated; no need to re-triangulate shared faces.
         ix = 0; iy = 0; iz = 0; ik = 0

         ! The child cube index, in range 1 ... 8
         k = 0

         ! Loop over the child cubes the first time; triangulate later
         ! when we know which cubes to do, which helps avoid function
         ! evaluations on the 5 x 5 x 5 cube
         do z = 0,1
            do y = 0,1
               do x = 0,1
                  k = k+1

            ! Set child cube info; store only needed parts in kube(k)

          !                        f5(x+1:x+2,y+1:y+2,z+1:z+2))
                  call set_vertex_info3(cube,p3,f3,x,y,z)

            ! Which cube vertices are inside or outside isosurface?
                  call set_case_info_(cube)
                  if (is_inside_surface_(cube)) self%volume_min = self%volume_min+vol
                  if (NOT is_outside_surface_(cube)) self%volume_max = self%volume_max+vol

            ! Is the cube on the surface?
                  if (NOT is_on_surface_(cube)) cycle

            ! We are on the surface: set the cube coords
                  self%x = self%x+x
                  self%y = self%y+y
                  self%z = self%z+z

            ! Don't skip this cube
                  skip(k) = FALSE

            ! Which cube edges cut the surface? Hence, which cube
            ! vertices are needed? Hence, what triangles are to be
            ! made?
                  cube%skip_bit_string = 0
                  call set_triangulation_info_(cube)

            ! The number of times that the cube will be re-used in
            ! the map table
                  c = 0
                  if (has_right_face_on_surface_(cube)) then
                     if (x>0) then; c = c+1
                     else; ik(k) = 1
                     end if
                  end if
                  if (has_back_face_on_surface_(cube)) then
                     if (y>0) then; c = c+1
                     else; ik(k) = 1
                     end if
                  end if
                  if (has_upper_face_on_surface_(cube)) then
                     if (z>0) then; c = c+1
                     else; ik(k) = 1
                     end if
                  end if
                  cube%edge_vertex_index(12) = c

            ! Find out which faces of the cube should have been triangulated
            ! already, and store in ix, iy, iz arrays, and store in the
            ! skip_bit_string for hessian evaluation
                  if (has_left_face_on_surface_(cube)) then
                     if (x>0) then
                        ix(k) = -1
                        call set_left_skip_bit_string_(cube)  ! Left child cube exists
                     else if (self%x>0 AND has_key_(self%map,[self%x-1,self%y,self%z],index=ix(k))) then
                        call set_left_skip_bit_string_(cube)  ! Left map cube exists
                     end if
                     ENSURE(ix(k)/=0,"ISOSURFACE:divide_cubes_small_map_m ... no left face indices found")
                  end if
                  if (has_front_face_on_surface_(cube)) then
                     if (y>0) then
                        iy(k) = -1
                        call set_front_skip_bit_string_(cube)  ! Front child cube exists
                     else if (self%y>0 AND has_key_(self%map,[self%x,self%y-1,self%z],index=iy(k))) then
                        call set_front_skip_bit_string_(cube)  ! Front map cube exists
                     end if
                     ENSURE(iy(k)/=0,"ISOSURFACE:divide_cubes_small_map_m ... no front face indices found")
                  end if
                  if (has_lower_face_on_surface_(cube)) then
                     if (z>0) then
                        iz(k) = -1
                        call set_below_skip_bit_string_(cube)  ! Lower child cube exists
                     else if (self%z>0 AND has_key_(self%map,[self%x,self%y,self%z-1],index=iz(k))) then
                        call set_below_skip_bit_string_(cube)  ! Lower map cube exists
                     end if
                     ENSURE(iz(k)/=0,"ISOSURFACE:divide_cubes_small_map_m ... no lower face indices found")
                  end if

            ! Set which edges to interpolate, accounting for those
            ! already done above in skip_bit_string
                  call set_cube_bit_string_(cube)

            ! Set which of the 5 x 5 x 5 points are needed for hessian
            ! and gradient.
                  call set_hessian_eval_array(cube,eval,x,y,z)

            ! Store the needed parts of cube for later interpolation
                  kube(k) = cube

            ! Clear the cube coords
                  self%x = self%x-x
                  self%y = self%y-y
                  self%z = self%z-z
               end do
            end do
         end do

         ! Don't evaluate the 3 x 3 x 3 points/values -- already done
         eval(2:4,2:4,2:4) = FALSE
         f5(1:3,1:3,1:3) = f3

         ! Now evaluate the function only at the needed points. The
         ! previous loops over the 8 kubes was all for this.
         call make_cube_of_points(self%plot_grid,p5,-1,3,self%del_x,self%del_y,self%del_z,self%x,self%y,self%z,eval)
         call make_5_cube_of_values_m(self,mol,f5,func_m,p5,eval)

         ! Loop over the child cubes the second time, and do the
         ! triangulation.
         k = 0
         do z = 0,1
            do y = 0,1
               do x = 0,1
                  k = k+1
                  if (skip(k)) cycle

            ! Get stored cobe
                  cube = kube(k)

            ! Evaluate the gradient and hessian
                  call set_hessian_info_(cube,f5(x:x+3,y:y+3,z:z+3))

            ! Set the cube coordinates; for storing in map table
                  self%x = self%x+x
                  self%y = self%y+y
                  self%z = self%z+z

            ! Use previously triangulated cubes if possible, avoid
            ! looking in the map table
                  kk = ix(k)
                  if (kk/=0) then
                     if (kk<0) then
                        kk = left(k)
                        call set_left_info_(cube,kube(kk)%edge_vertex_index)
                     else
                        call set_left_info_(cube,self%map%values(:,kk))
                        self%map%values(13,kk) = self%map%values(13,kk)-1
                     end if
                  end if
                  kk = iy(k)
                  if (kk/=0) then
                     if (kk<0) then
                        kk = front(k)
                        call set_front_info_(cube,kube(kk)%edge_vertex_index)
                     else
                        call set_front_info_(cube,self%map%values(:,kk))
                        self%map%values(13,kk) = self%map%values(13,kk)-1
                     end if
                  end if
                  kk = iz(k)
                  if (kk/=0) then
                     if (kk<0) then
                        kk = below(k)
                        call set_below_info_(cube,kube(kk)%edge_vertex_index)
                     else
                        call set_below_info_(cube,self%map%values(:,kk))
                        self%map%values(13,kk) = self%map%values(13,kk)-1
                     end if
                  end if

            ! Set the cube vertex index to the last vertex
                  call set_n_pt_(cube,self%n_pt)

            ! Finally, do the interpolation
                  if (oblique) then; call interpolate_oblique_faces_(cube)
                  else; call interpolate_faces_(cube)
                  end if

            ! Copy the cube interpolation info to the isosurface
                  call append_new_face_info_(self,cube)

            ! Append the cube vertex info to the map only if needed
                  if (cube%edge_vertex_index(12)>0) call append_pair_(self%map,[self%x,self%y,self%z],cube%edge_vertex_index)

            ! Might need to keep this from interpolation, it may share a face
                  if (ik(k)>0) kube(k)%edge_vertex_index = cube%edge_vertex_index

            ! Clear the cube coords
                  self%x = self%x-x
                  self%y = self%y-y
                  self%z = self%z-z
               end do
            end do
         end do

         ! Remove dead edge_vertex_indices from map table
         n = 0
         do k = 1,8
            kk = ix(k)
            if (kk>0 AND self%map%values(13,kk)==0) then
               n = n+1
               dead_list(n) = kk
            end if
            kk = iy(k)
            if (kk>0 AND self%map%values(13,kk)==0) then
               n = n+1
               dead_list(n) = kk
            end if
            kk = iz(k)
            if (kk>0 AND self%map%values(13,kk)==0) then
               n = n+1
               dead_list(n) = kk
            end if
         end do
         if (n>0) call delete_items_(self%map,dead_list(1:n))
      end if

      ! Go up to the next level: double side lengths, decrease level
      self%del_x = TWO*self%del_x
      if (oblique) then; self%del_y = TWO*self%del_y; self%del_z = TWO*self%del_z
      else; self%del_y = self%del_x; self%del_z = self%del_x
      end if
      self%level = self%level-1
   end subroutine


!   update_4_slab(p,f,slice,func) :: private
!   ! Make or update a "slice" of an array "f", a 4-slab of "func" function
!   ! values which are evaluated at the slab points "p".  The slab involves .plot_grid
!   ! points whose z values are in the range slice-1:slice+2.  The "p" points and
!   ! the function values f(:,:,2:3) correspond to the bottom and top of the
!   ! slice.  The other slabs f(:,:,1) and f(:,:,4) are used for finite
!   ! differences to get vertex gradients.
!      p :: MAT4{REAL}, INOUT
!      f :: MAT3{REAL}, INOUT
!      slice :: INT
!      interface
!         func(values,pt)
!            values :: VEC{REAL}, OUT
!            pt :: MAT{REAL}, IN
!         end
!      end
!
!   ENSURE(slice>=2 AND slice<=.plot_grid.n_z-2,"slice out f range")
!   ENSURE(p.dim1==3,"p must have dim1=3")
!   ENSURE(p.dim2==.plot_grid.n_x,"wrong size, p")
!   ENSURE(p.dim3==.plot_grid.n_y,"wrong size, p")
!   ENSURE(p.dim4==4,"p must have 4 slabs")
!   ENSURE(f.dim1==.plot_grid.n_x,"wrong size, f")
!   ENSURE(f.dim2==.plot_grid.n_y,"wrong size, f")
!   ENSURE(f.dim3==4,"f must have 4 slabs")
!
!      fz,lz,n_pt, i,x,y,z :: INT
!      values :: VEC{REAL}@
!      pt :: MAT{REAL}@
!
!      ! Get slab ranges
!      if (slice == 2) then
!         ! These are the first 4 slices
!         fz = 1; lz = 4
!      else
!         ! In between: only calculate one slice
!         p(:,:,:,1:3) = p(:,:,:,2:4)
!         f(:,:,1:3) = f(:,:,2:4)
!         fz = 4; lz = 4
!      end
!
!      ! Now calculate the slab between the right limits
!      n_pt = .plot_grid.n_x*.plot_grid.n_y*(lz-fz+1)
!      values.create(n_pt)
!      pt.create(n_pt,3)
!
!      ! Set the slab
!      .plot_grid.make_points(pt,1,.plot_grid.n_x,1,.plot_grid.n_y,slice-2+fz,slice-2+lz)
!      call func(values,pt)
!      f(:,:,fz:lz).set_to(values)
!
!      ! Copy the points
!      i = 0
!      do z = fz,lz
!      do y = 1,.plot_grid.n_y
!      do x = 1,.plot_grid.n_x
!         i = i + 1
!         p(:,x,y,z) = pt(i,:)
!      end
!      end
!      end
!
!      pt.destroy
!      values.destroy
!
!   end

   subroutine update_4_slab_m(self,mol,p,f,slice,func_m)
   ! Make or update a "slice" of an array "f", a 4-slab of "func" function
   ! values which are evaluated at the slab points "p".  The slab involves .plot_grid
   ! points whose z values are in the range slice-1:slice+2.  The "p" points and
   ! the function values f(:,:,2:3) correspond to the bottom and top of the
   ! slice.  The other slabs f(:,:,1) and f(:,:,4) are used for finite
   ! differences to get vertex gradients.
      type(ISOSURFACE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      MAT4(REAL,:,:,:,:), INOUT :: p
      MAT3(REAL,:,:,:), INOUT :: f
      INT :: slice
      interface
         subroutine func_m(mol,values,pt)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: mol
            VEC(REAL,:), OUT :: values
            MAT(REAL,:,:), IN :: pt
         end subroutine
      end interface
      INT :: fz,lz,n_pt,i,x,y,z
      VEC(REAL,:), allocatable :: values
      MAT(REAL,:,:), allocatable :: pt
   ENSURE(slice>=2 AND slice<=self%plot_grid%n_z-2,"ISOSURFACE:update_4_slab_m ... slice out f range")
   ENSURE(size(p,1)==3,"ISOSURFACE:update_4_slab_m ... p must have dim1=3")
   ENSURE(size(p,2)==self%plot_grid%n_x,"ISOSURFACE:update_4_slab_m ... wrong size, p")
   ENSURE(size(p,3)==self%plot_grid%n_y,"ISOSURFACE:update_4_slab_m ... wrong size, p")
   ENSURE(size(p,4)==4,"ISOSURFACE:update_4_slab_m ... p must have 4 slabs")
   ENSURE(size(f,1)==self%plot_grid%n_x,"ISOSURFACE:update_4_slab_m ... wrong size, f")
   ENSURE(size(f,2)==self%plot_grid%n_y,"ISOSURFACE:update_4_slab_m ... wrong size, f")
   ENSURE(size(f,3)==4,"ISOSURFACE:update_4_slab_m ... f must have 4 slabs")

      ! Get slab ranges
      if (slice==2) then

         ! These are the first 4 slices
         fz = 1; lz = 4
      else

         ! In between: only calculate one slice
         p(:,:,:,1:3) = p(:,:,:,2:4)
         f(:,:,1:3) = f(:,:,2:4)
         fz = 4; lz = 4
      end if

      ! Now calculate the slab between the right limits
      n_pt = self%plot_grid%n_x*self%plot_grid%n_y*(lz-fz+1)
      call create_(values,n_pt)
      call create_(pt,n_pt,3)

      ! Set the slab
      call make_points_(self%plot_grid,pt,1,self%plot_grid%n_x,1,self%plot_grid%n_y,slice-2+fz,slice-2+lz)
      call func_m(mol,values,pt)
      call set_to_(f(:,:,fz:lz),values)

      ! Copy the points
      i = 0
      do z = fz,lz
         do y = 1,self%plot_grid%n_y
            do x = 1,self%plot_grid%n_x
               i = i+1
               p(:,x,y,z) = pt(i,:)
            end do
         end do
      end do
      call destroy_(pt)
      call destroy_(values)
   end subroutine

   subroutine update_4_slab(self,p,f,slice,basin)
   ! Make or update a "slice" of an array "f", a 4-slab of Bader basin
   ! indices taken from "basin", at the slab points "p". The slab
   ! involves .plot_grid points whose z values are in the range
   ! slice-1:slice+2. The "p" points and the values f(:,:,2:3)
   ! correspond to the bottom and top of the slice.
      type(ISOSURFACE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), INOUT :: p
      MAT3(INT,:,:,:), INOUT :: f
      INT, IN :: slice
      MAT3(INT,:,:,:), IN :: basin
      INT :: fz,lz,n_pt,n_x,n_y,i,x,y,z
      MAT(REAL,:,:), allocatable :: pt
   ENSURE(slice>=2 AND slice<=self%plot_grid%n_z-2,"ISOSURFACE:update_4_slab ... slice out f range")
   ENSURE(size(p,1)==3,"ISOSURFACE:update_4_slab ... p must have dim1=3")
   ENSURE(size(p,2)==self%plot_grid%n_x,"ISOSURFACE:update_4_slab ... wrong size, p")
   ENSURE(size(p,3)==self%plot_grid%n_y,"ISOSURFACE:update_4_slab ... wrong size, p")
   ENSURE(size(p,4)==4,"ISOSURFACE:update_4_slab ... p must have 4 slabs")
   ENSURE(size(f,1)==self%plot_grid%n_x,"ISOSURFACE:update_4_slab ... wrong size, f")
   ENSURE(size(f,2)==self%plot_grid%n_y,"ISOSURFACE:update_4_slab ... wrong size, f")
   ENSURE(size(f,3)==4,"ISOSURFACE:update_4_slab ... f must have 4 slabs")
   ENSURE(size(basin,1)==self%plot_grid%n_x,"ISOSURFACE:update_4_slab ... wrong dim1, basin")
   ENSURE(size(basin,2)==self%plot_grid%n_y,"ISOSURFACE:update_4_slab ... wrong dim2, basin")
   ENSURE(size(basin,3)==self%plot_grid%n_z,"ISOSURFACE:update_4_slab ... wrong dim3, basin")

      ! Get slab ranges
      if (slice==2) then

         ! These are the first 4 slices
         fz = 1; lz = 4
      else

         ! In between: only calculate one slice
         p(:,:,:,1:3) = p(:,:,:,2:4)
         f(:,:,1:3) = f(:,:,2:4)
         fz = 4; lz = 4
      end if

      ! Now calculate the slab between the right limits
      n_x = self%plot_grid%n_x
      n_y = self%plot_grid%n_y
      n_pt = n_x*n_y*(lz-fz+1)
      call create_(pt,n_pt,3)

      ! Set the slab of points & transfer the basin indices
      call make_points_(self%plot_grid,pt,1,n_x,1,n_y,slice-2+fz,slice-2+lz)
      f(:,:,fz:lz) = basin(:,:,slice-2+fz:slice-2+lz)

      ! Copy the points; reshape & transpose
      i = 0
      do z = fz,lz
         do y = 1,n_y
            do x = 1,n_x
               i = i+1
               p(:,x,y,z) = pt(i,:)
            end do
         end do
      end do
      call destroy_(pt)
   end subroutine

   PURE subroutine append_new_face_info(self,cube)
   ! Generate the isosurface using the marching cube algorithm.
      type(ISOSURFACE_TYPE), INOUT :: self
      type(MARCHINGCUBE_TYPE), IN :: cube
      INT :: n_pt,n_face,n_col
      n_pt = no_of_active_edges_(cube)
      n_face = cube%n_triangle

      ! Expand info arrays if required
      n_col = size(self%point,2)
      if ((self%n_pt+n_pt)>n_col) then
         call expand_columns_(self%point,2*n_col)
         call expand_columns_(self%point_gradient,2*n_col)
         call expand_(self%point_mean_curvature,2*n_col)
         call expand_(self%point_gaussian_curvature,2*n_col)
      end if
      n_col = size(self%face,2)
      if ((self%n_face+n_face)>n_col) then
         call expand_columns_(self%face,2*n_col)
      end if

      ! Store isosurface info.
      call get_edge_vertex_positions_(cube,self%point(:,self%n_pt+1:self%n_pt+n_pt))
      call get_edge_vertex_gradients_(cube,self%point_gradient(:,self%n_pt+1:self%n_pt+n_pt))
      call get_edge_mean_curvatures_(cube,self%point_mean_curvature(self%n_pt+1:self%n_pt+n_pt))
      call get_edge_gaussian_curvatures_(cube,self%point_gaussian_curvature(self%n_pt+1:self%n_pt+n_pt))
      call get_triangle_vertex_indices_(cube,self%face(:,self%n_face+1:self%n_face+n_face))

      ! Update
      self%n_pt = self%n_pt+n_pt
      self%n_face = self%n_face+n_face
   end subroutine

   PURE subroutine append_new_cap_info(self,cap_square)
   ! Add the caps to the isosurface from the cap_square
      type(ISOSURFACE_TYPE), INOUT :: self
      type(CAPPING_SQUARE_TYPE), IN :: cap_square
      INT :: n_pt,n_cap,n_col
      n_pt = no_of_new_vertices_(cap_square)
      n_cap = cap_square%n_triangle

      ! Expand info arrays if required
      n_col = size(self%point,2)
      if ((self%n_pt+n_pt)>n_col) then
         call expand_columns_(self%point,2*n_col)
         call expand_columns_(self%point_gradient,2*n_col)
         call expand_(self%point_mean_curvature,2*n_col)
         call expand_(self%point_gaussian_curvature,2*n_col)
      end if
      n_col = size(self%cap,2)
      if ((self%n_cap+n_cap)>n_col) then
         call expand_columns_(self%cap,2*n_col)
      end if

      ! Store cap info.
      call get_new_vertex_positions_(cap_square,self%point(:,self%n_pt+1:self%n_pt+n_pt))
      call get_new_vertex_gradients_(cap_square,self%point_gradient(:,self%n_pt+1:self%n_pt+n_pt))
      call get_new_mean_curvatures_(cap_square,self%point_mean_curvature(self%n_pt+1:self%n_pt+n_pt))
      call get_new_gaussian_curvatures_(cap_square,self%point_gaussian_curvature(self%n_pt+1:self%n_pt+n_pt))
      call get_triangle_vertex_indices_(cap_square,self%cap(:,self%n_cap+1:self%n_cap+n_cap))

      ! Update
      self%n_pt = self%n_pt+n_pt
      self%n_cap = self%n_cap+n_cap
   end subroutine

   PURE subroutine rotate_gradients(self)
   ! Rotate the gradients by the box axes. This needs to be done
   ! because box axes may not be the same as the natural x-y-z axes.
      type(ISOSURFACE_TYPE), INOUT :: self
      MAT(REAL,3,3) :: axes
      MAT(REAL,:,:), allocatable :: old
      axes = plot_axes_(self%plot_grid)
      call create_(old,3,self%n_pt)
      old = self%point_gradient
      self%point_gradient = matmul(axes,old)
      call destroy_(old)
   end subroutine

   PURE function no_of_divisions(self,side_length) result (res)
   ! Return the number of binary divisions before the .plot_grid side length
   ! becomes less than a given "side_length".
      type(ISOSURFACE_TYPE), IN :: self
      REAL, IN :: side_length
      INT :: res
      REAL :: width
      INT :: i
      width = maxval(self%plot_grid%width)
      i = 0
      do
         i = i+1
         width = width/TWO
         if (width>side_length) cycle
         res = i
         exit
      end do
   end function

   PURE subroutine initialize_eval_f5(eval,f5,f3)
   ! Initialize the 2:4^3 block of "eval" and the "f5" array,
   ! the latter from "f3".
      MAT3(BIN,5,5,5), OUT :: eval
      MAT3(REAL,5,5,5), OUT :: f5
      VEC(REAL,27), IN :: f3
      INT :: i,j,k,n
      n = 0
      do k = 2,4
         do j = 2,4
            do i = 2,4
               n = n+1
               eval(i,j,k) = FALSE
               f5(i,j,k) = f3(n)
            end do
         end do
      end do
   end subroutine


!   make_2_cube_of_values(f,func,p) :: private
!   ! Evaluate a cube of values "f", with two points per side (starting at point
!   ! 0). The values in "f" are those of the "func" function evaluated at
!   ! positions "p" which are supposed to be the cube corners.
!      self :: INOUT
!      f :: VEC{REAL}(8), OUT
!      p :: MAT{REAL}(3,8), INOUT
!      interface
!         func(values,pt)
!            values :: VEC{REAL}, OUT
!            pt :: MAT{REAL}, IN
!         end
!      end
!
!      i :: INT
!      fac,s :: REAL
!
!      ! Swap these around to match marchingcube
!      p(:,3:4) = p(:,4:3:-1)    
!      p(:,7:8) = p(:,8:7:-1)
!
!      ! Evaluate the function at pt
!      call func(f,transpose(p)) 
!
!      ! Remove 8 evaluations for this
!      .n_skip = .n_skip - 8     
!
!      ! Cap ends after here
!      if (.cap_ends==0) return  
!
!      if (.cap_ends>ZERO) then; fac = ONE - TOL(3); s =  ONE
!      else;                     fac = ONE + TOL(3); s = -ONE
!      end
!
!      do i = 1,8
!         if (s*f(i)>s*.iso_value) f(i) = .iso_value*fac
!      end
!
!   end

   subroutine make_2_cube_of_values_m(self,mol,f,func_m,p)
   ! Evaluate a cube of values "f", with two points per side (starting at point
   ! 0). The values in "f" are those of the "func" function evaluated at
   ! positions "p" which are supposed to be the cube corners.
      type(ISOSURFACE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      VEC(REAL,8), OUT :: f
      MAT(REAL,3,8), INOUT :: p
      interface
         subroutine func_m(mol,values,pt)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: mol
            VEC(REAL,:), OUT :: values
            MAT(REAL,:,:), IN :: pt
         end subroutine
      end interface
      INT :: i
      REAL :: fac,s

      ! Swap these around to match marchingcube
      p(:,3:4) = p(:,4:3:-1)
      p(:,7:8) = p(:,8:7:-1)

      ! Evaluate the function at pt
      call func_m(mol,f,transpose(p))

      ! Remove 8 evaluations for this
      self%n_skip = self%n_skip-8

      ! Cap ends after here
      if (self%cap_ends==0) return
      if (self%cap_ends>ZERO) then; fac = ONE-TOL(3); s = ONE
      else; fac = ONE+TOL(3); s = -ONE
      end if
      do i = 1,8
         if (s*f(i)>s*self%iso_value) f(i) = self%iso_value*fac
      end do
   end subroutine


!   make_3_cube_of_values(f,func,p,corner_value) :: private
!   ! Evaluate a cube of values "f", with three points per side (starting at
!   ! point 0). The values in "f" are those of the "func" function evaluated at
!   ! cube point positions "p". The cube points are assumed to be ordered along
!   ! firstly the x-grid axis, then y-axis, then z-axis. The corner positions are
!   ! not evaluated but taken from the "parent_cube" corner vertex values.
!      self :: INOUT
!      f :: VEC{REAL}(27), OUT
!      p :: MAT{REAL}(3,27), IN
!      corner_value :: VEC{REAL}(8), IN
!      interface
!         func(values,pt)
!            values :: VEC{REAL}, OUT
!            pt :: MAT{REAL}, IN
!         end
!      end
!
!      vals :: VEC{REAL}(19)
!      pt :: MAT{REAL}(19,3)
!      edge :: VEC{INT}(19) = [2,4,5,6,8,10,11,12,13,14,15,16,17,18,20,22,23,24,26]
!      i,last :: INT
!      fac,s :: REAL
!
!      vals = ZERO
!      pt = transpose(p(:,edge))       ! Skip edge points
!      call func(vals,pt)              ! Evaluate the function at pt
!      .n_skip = .n_skip - 19          ! Remove 19=27-8 evaluations for this
!      f(edge) = vals                  ! Store values in full 3^3 array
!
!      f(1)  = corner_value(1)         ! Get the parent cube values
!      f(3)  = corner_value(2)
!      f(9)  = corner_value(3)
!      f(7)  = corner_value(4)
!      f(19) = corner_value(5)
!      f(21) = corner_value(6)
!      f(27) = corner_value(7)
!      f(25) = corner_value(8)
!
!      if (.cap_ends==0) return        ! Cap ends after here
!
!      if (.cap_ends>ZERO) then; fac = ONE - TOL(3); s =  ONE
!      else;                     fac = ONE + TOL(3); s = -ONE
!      end
!
!      last = 2**.final_level - 2
!
!      if (.x==0) then
!         do i = 1,25,3
!            if (s*f(i)>s*.iso_value) f(i) = .iso_value*fac
!         end
!      else if (.x==last) then
!         do i = 3,27,3
!            if (s*f(i)>s*.iso_value) f(i) = .iso_value*fac
!         end
!      end
!
!      if (.y==0) then
!         do i = 1,19,9
!            if (s*f(i)>s*.iso_value) f(i) = .iso_value*fac
!         end
!         do i = 2,20,9
!            if (s*f(i)>s*.iso_value) f(i) = .iso_value*fac
!         end
!         do i = 3,21,9
!            if (s*f(i)>s*.iso_value) f(i) = .iso_value*fac
!         end
!      else if (.y==last) then
!         do i = 7,25,9
!            if (s*f(i)>s*.iso_value) f(i) = .iso_value*fac
!         end
!         do i = 8,26,9
!            if (s*f(i)>s*.iso_value) f(i) = .iso_value*fac
!         end
!         do i = 9,27,9
!            if (s*f(i)>s*.iso_value) f(i) = .iso_value*fac
!         end
!      end
!
!      if (.z==0) then
!         do i = 1,9
!            if (s*f(i)>s*.iso_value) f(i) = .iso_value*fac
!         end
!      else if (.z==last) then
!         do i = 19,27
!            if (s*f(i)>s*.iso_value) f(i) = .iso_value*fac
!         end
!      end
!   end

   subroutine make_3_cube_of_values_m(self,mol,f,func_m,p,corner_value)
   ! Evaluate a cube of values "f", with three points per side (starting at
   ! point 0). The values in "f" are those of the "func" function evaluated at
   ! cube point positions "p". The cube points are assumed to be ordered along
   ! firstly the x-grid axis, then y-axis, then z-axis. The corner positions are
   ! not evaluated but taken from the "parent_cube" corner vertex values.
      type(ISOSURFACE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      VEC(REAL,27), OUT :: f
      MAT(REAL,3,27), IN :: p
      VEC(REAL,8), IN :: corner_value
      interface
         subroutine func_m(mol,values,pt)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: mol
            VEC(REAL,:), OUT :: values
            MAT(REAL,:,:), IN :: pt
         end subroutine
      end interface
      VEC(REAL,19) :: vals
      MAT(REAL,19,3) :: pt
      VEC(INT,19) :: edge = [2,4,5,6,8,10,11,12,13,14,15,16,17,18,20,22,23,24,26]
      INT :: i,last
      REAL :: fac,s
      vals = ZERO
      pt = transpose(p(:,edge))  ! Skip edge points
      call func_m(mol,vals,pt)  ! Evaluate the function at pt
      self%n_skip = self%n_skip-19  ! Remove 19=27-8 evaluations for this
      f(edge) = vals  ! Store values in full 3^3 array
      f(1) = corner_value(1)  ! Get the parent cube values
      f(3) = corner_value(2)
      f(9) = corner_value(3)
      f(7) = corner_value(4)
      f(19) = corner_value(5)
      f(21) = corner_value(6)
      f(27) = corner_value(7)
      f(25) = corner_value(8)
      if (self%cap_ends==0) return  ! Cap ends after here
      if (self%cap_ends>ZERO) then; fac = ONE-TOL(3); s = ONE
      else; fac = ONE+TOL(3); s = -ONE
      end if
      last = 2**self%final_level-2
      if (self%x==0) then
         do i = 1,25,3
            if (s*f(i)>s*self%iso_value) f(i) = self%iso_value*fac
         end do
      else if (self%x==last) then
         do i = 3,27,3
            if (s*f(i)>s*self%iso_value) f(i) = self%iso_value*fac
         end do
      end if
      if (self%y==0) then
         do i = 1,19,9
            if (s*f(i)>s*self%iso_value) f(i) = self%iso_value*fac
         end do
         do i = 2,20,9
            if (s*f(i)>s*self%iso_value) f(i) = self%iso_value*fac
         end do
         do i = 3,21,9
            if (s*f(i)>s*self%iso_value) f(i) = self%iso_value*fac
         end do
      else if (self%y==last) then
         do i = 7,25,9
            if (s*f(i)>s*self%iso_value) f(i) = self%iso_value*fac
         end do
         do i = 8,26,9
            if (s*f(i)>s*self%iso_value) f(i) = self%iso_value*fac
         end do
         do i = 9,27,9
            if (s*f(i)>s*self%iso_value) f(i) = self%iso_value*fac
         end do
      end if
      if (self%z==0) then
         do i = 1,9
            if (s*f(i)>s*self%iso_value) f(i) = self%iso_value*fac
         end do
      else if (self%z==last) then
         do i = 19,27
            if (s*f(i)>s*self%iso_value) f(i) = self%iso_value*fac
         end do
      end if
   end subroutine


!   make_5_cube_of_values(f,func,p,eval) :: private
!   ! Evaluate a 5 cube of values "f", with five points per side (starting at
!   ! point 0). The values in "f" are those of the "func" function evaluated at
!   ! cube point positions "p", except that only the points corresponding to the
!   ! mask "eval" being TRUE are evaluated.
!      self :: INOUT
!      f :: VEC{REAL}(125), OUT
!      p :: MAT{REAL}(3,125), IN
!      eval :: VEC{BIN}(125), IN
!      interface
!         func(values,pt)
!            values :: VEC{REAL}, OUT
!            pt :: MAT{REAL}, IN
!         end
!      end
!
!      values :: VEC{REAL}@
!      pt :: MAT{REAL}@
!      n_pt,n,i :: INT
!
!      n_pt = count(eval)
!      values.create(n_pt)
!      pt.create(n_pt,3)
!      n = 0
!      do i = 1,125 ! Pack the points "p" into "pt"
!         if (NOT eval(i)) cycle
!         n = n + 1
!         pt(n,1) = p(1,i)
!         pt(n,2) = p(2,i)
!         pt(n,3) = p(3,i)
!      end
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      call func(values,pt)
!      .n_skip = .n_skip - n_pt
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      f = unpack(values,eval,f)
!      pt.destroy
!      values.destroy
!   end

   subroutine make_5_cube_of_values_m(self,mol,f,func_m,p,eval)
   ! Evaluate a 5 cube of values "f", with five points per side (starting at
   ! point 0). The values in "f" are those of the "func" function evaluated at
   ! cube point positions "p", except that only the points corresponding to the
   ! mask "eval" being TRUE are evaluated.
      type(ISOSURFACE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      VEC(REAL,125), OUT :: f
      MAT(REAL,3,125), INOUT :: p
      VEC(BIN,125), IN :: eval
      interface
         subroutine func_m(mol,values,pt)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: mol
            VEC(REAL,:), OUT :: values
            MAT(REAL,:,:), IN :: pt
         end subroutine
      end interface
      VEC(REAL,:), allocatable :: values
      MAT(REAL,:,:), allocatable :: pt
      INT :: n_pt,n,i
      n_pt = count(eval)
      call create_(values,n_pt)
      call create_(pt,n_pt,3)
      n = 0
      do i = 1,125  ! Pack the points "p" into "pt"
         if (NOT eval(i)) cycle
         n = n+1
         pt(n,1) = p(1,i)
         pt(n,2) = p(2,i)
         pt(n,3) = p(3,i)
      end do
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call func_m(mol,values,pt)
      self%n_skip = self%n_skip-n_pt
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      f = unpack(values,eval,f)
      call destroy_(pt)
      call destroy_(values)
   end subroutine


!  ================
!  Plotting Methods
!  ================

!   plot_function(func) :: leaky
!   ! Evaluate "func" on the surface of the isosurface points ".point".
!   ! The answers are stored in surface_property_values.
!      self :: INOUT
!      interface
!         func(values,pt)
!            values :: VEC{REAL}, OUT
!            pt :: MAT{REAL}, IN
!         end
!      end
!
!   ENSURE(.point.allocated,"no isosurface points!")
!
!      .surface_property_values.create(.point.dim2)
!
!      call func(.surface_property_values,transpose(.point))
!
!   end

   subroutine plot_function_m(self,mol,func_m)
   ! Evaluate "func" on the surface of the isosurface points ".point".
   ! The answers are stored in surface_property_values. This versionb
   ! does not need to use a MOLECULE saved_self.
      type(ISOSURFACE_TYPE), INOUT :: self
      type(MOLECULE_TYPE), IN :: mol
      interface
         subroutine func_m(mol,values,pt)
         use TYPES_MODULE
            type(MOLECULE_TYPE), IN :: mol
            VEC(REAL,:), OUT :: values
            MAT(REAL,:,:), IN :: pt
         end subroutine
      end interface
   ENSURE(allocated(self%point),"ISOSURFACE:plot_function_m ... no isosurface points!")
      call create_(self%surface_property_values,size(self%point,2))
      call func_m(mol,self%surface_property_values,transpose(self%point))
   end subroutine


!  ===============
!  Connected Areas
!  ===============

   PURE subroutine make_neighbour_list(self)
   ! Make the .neighbour list for the isosurface
      type(ISOSURFACE_TYPE), INOUT :: self
      INT :: f,v1,v2,v3
   ENSURE(allocated(self%point),"ISOSURFACE:make_neighbour_list ... no points")
   ENSURE(allocated(self%face),"ISOSURFACE:make_neighbour_list ... no faces")
      call create_(self%neighbour,self%n_pt)
      do f = 1,self%n_face
         v1 = self%face(1,f)
         v2 = self%face(2,f)
         v3 = self%face(3,f)
         call append_only_if_unique_(self%neighbour(v1)%element,v2)
         call append_only_if_unique_(self%neighbour(v1)%element,v3)
         call append_only_if_unique_(self%neighbour(v2)%element,v1)
         call append_only_if_unique_(self%neighbour(v2)%element,v3)
         call append_only_if_unique_(self%neighbour(v3)%element,v1)
         call append_only_if_unique_(self%neighbour(v3)%element,v2)
      end do
   end subroutine

   PURE subroutine get_minimal_blobs(self,blob,property,limit)
   ! Returns the indices of set(s) of vertices/points comprising a
   ! *connected* area (aka a "red spot") where the "property" values
   ! are all smaller than "limit", otherwise known as a "blob".
      type(ISOSURFACE_TYPE), INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable, OUT :: blob
      VEC(REAL,:), IN :: property
      REAL, IN :: limit
      VEC(INT,:), allocatable :: this
      VEC(BIN,:), allocatable :: mask
      INT :: b,ind
   ENSURE(self%n_pt>0,"ISOSURFACE:get_minimal_blobs ... there are no isosurface points")
   ENSURE(size(property)==self%n_pt,"ISOSURFACE:get_minimal_blobs ... wrong size, property array")

      ! Make neighbour list (self is INOUT)
      if (NOT allocated(self%neighbour)) call make_neighbour_list_(self)

      ! Mask for searchable points
      call create_(mask,self%n_pt)
      mask = TRUE

      ! Loop repeatedly over smallest
      ! starting points on the surface
      b = 0
      call create_(blob,0)
      do

         ! This blob's indices
         call create_(this,0)

         ! Index of the minimum property value
         ind = minval(minloc(property,mask))

         ! Any more starting indices?
         if (property(ind)>limit) exit

         ! Add points to blob(b)
         call find_minimal_blob_from_(self,ind,this,property,limit)

         ! Remove found blob from prop
         mask(this) = FALSE

         ! Store this blob
         call append_(blob,this)

         ! Clean
         call destroy_(this)
      end do
   end subroutine

   PURE recursive subroutine find_minimal_blob_from(self,ind,blob,property,limit)
   ! Starting from vertex "ind" look at all neighbours to see if their
   ! property values are also less than "limit", and if so, add then
   ! to the "blob" list. NOTE: blob must be initialised size=0.
      type(ISOSURFACE_TYPE), IN :: self
      INT, IN :: ind
      VEC(INT,:), allocatable, OUT :: blob
      VEC(REAL,:), IN :: property
      REAL, IN :: limit
      VEC(INT,:), allocatable :: neighbours
      INT :: i,n
   ENSURE(allocated(self%neighbour),"ISOSURFACE:find_minimal_blob_from ... no neighbor list!")
   ENSURE(allocated(blob),"ISOSURFACE:find_minimal_blob_from ... blob not initialized")
   ENSURE(size(property)==self%n_pt,"ISOSURFACE:find_minimal_blob_from ... wrong size, property array")
   ENSURE(is_in_range_(ind,[1,size(property)]),"ISOSURFACE:find_minimal_blob_from ... ind out of range")

      ! Neighbours of point "ind"
      neighbours = self%neighbour(ind)%element(:)

      ! Find no. of elements in this blob
      do i = 1,size(neighbours)

         ! The i-th neighbour index
         n = neighbours(i)

         ! Is the neighbour in the blob?
         if (property(n)>limit) cycle

         ! Is the point unique?
         if (size(blob)==0 OR all(n/=blob)) then

            ! Add point to blob ...
            call append_(blob,n)

            ! Add any connected points to the blob
            call find_minimal_blob_from_(self,n,blob,property,limit)
         end if
      end do
   end subroutine

   subroutine put_d_norm_info(self,in,out,atom)
   ! Summarize d_norm contact/spot information for the current
   ! isosurface & surface property, assuming the surface has
   ! associated "in" and "out" "atoms", as for e.g. Hirshfeld
   ! surfaces.
      type(ISOSURFACE_TYPE), INOUT :: self
      VEC(INT,:), IN :: in,out
      VEC(type(ATOM_TYPE),:), IN :: atom
      VEC(REAL,:), allocatable :: d_norm,d_e,d_i
      VEC(REAL,:), allocatable :: bd_norm,bd_e,bd_i
      VEC(REAL,:), allocatable :: bd_area
      VEC(INT,:), allocatable :: a_e,a_i
      VEC(INT,:), allocatable :: ba_e,ba_i,bb,b1
      VEC(STR(len=STR_SIZE),:), allocatable :: bt_e,bt_i
      VEC(type(EVEC_INT_TYPE),:), allocatable :: blob,face
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: n_blob,b
      REAL :: fac,fac2
   ENSURE(allocated(self%atom),"ISOSURFACE:put_d_norm_info ... no cluster atom's")

      ! Get the surface distance info
      call create_(d_norm,self%n_pt)
      call create_(d_e,self%n_pt)
      call create_(d_i,self%n_pt)
      call create_(a_e,self%n_pt)
      call create_(a_i,self%n_pt)
      call make_surface_d_info_(self,d_norm,d_e,d_i,a_e,a_i,in,out,atom)

      ! Get the blob information
      call get_minimal_blobs_(self,blob,d_norm,self%surface_property_ub)
      n_blob = size(blob)

      ! Get faces in this blob
      call facify_(self,blob,face)

      ! Output the table
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"=======================")
      call text_(stdout,"d_norm blob information")
      call text_(stdout,"=======================")
      call flush_(stdout)
      call text_(stdout,". Minimal d_norm blobs are connected areas of the isosurface")
      call text_(stdout,"  where d_norm has a value *less than* a certain upper bound.")
      call text_(stdout,"  These areas are useful for characterising e.g. the weakly")
      call text_(stdout,"  interacting contacts.")
      call flush_(stdout)
      call text_(stdout,". Blobs are sorted in increasing order of their minimal value")
      call flush_(stdout)
      call text_(stdout,". Distances to blob centers, blob areas, reported in Angstrom")
      call flush_(stdout)
      call show_(stdout,"Surface property upper bound   =",self%surface_property_ub)
      call show_(stdout,"No. of blobs                   =",n_blob)
      call flush_(stdout)

      ! Anything to print?
      if (n_blob==0) then
         call destroy_(blob)
         call destroy_(a_i)
         call destroy_(a_e)
         call destroy_(d_i)
         call destroy_(d_e)
         call destroy_(d_norm)
         return
      end if

      ! Table
      call create_(table,9)
      call set_heading_(table(1),"Blob")
      call set_subhead_(table(1),"  # ")
      call set_heading_(table(2),"  In")
      call set_subhead_(table(2),"atom")
      call set_heading_(table(3)," Tag")
      call set_heading_(table(4)," Out")
      call set_subhead_(table(4),"atom")
      call set_heading_(table(5)," Tag")
      call set_heading_(table(6)," d_i")
      call set_subhead_(table(6),"  /A")
      call set_heading_(table(7)," d_e")
      call set_subhead_(table(7),"  /A")
      call set_heading_(table(8),"d_norm")
      call set_heading_(table(9)," Area")
      call set_subhead_(table(9)," /A^2")

      ! Get blob data
      call create_(bb,n_blob)
      call create_(b1,n_blob)
      call create_(ba_i,n_blob)
      call create_(bt_i,n_blob)
      call create_(ba_e,n_blob)
      call create_(bt_e,n_blob)
      call create_(bd_i,n_blob)
      call create_(bd_e,n_blob)
      call create_(bd_norm,n_blob)
      call create_(bd_area,n_blob)
      bb = [(b,b = 1,n_blob)]
      do b = 1,n_blob
         b1(b) = blob(b)%element(1)
      end do
      fac = ANGSTROM_PER_BOHR
      fac2 = fac*fac
      bd_i = d_i(b1)*fac
      bd_e = d_e(b1)*fac
      ba_i = a_i(b1)
      bt_i = atom(ba_i)%tag
      ba_e = a_e(b1)
      bt_e = atom(ba_e)%tag
      bd_norm = d_norm(b1)
      do b = 1,n_blob
         bd_area(b) = total_area_(self,face(b)%element)*fac2
      end do

      ! Set table values
      call set_values_(table(1),bb)
      call set_values_(table(2),ba_i)
      call set_values_(table(3),bt_i)
      call set_values_(table(4),ba_e)
      call set_values_(table(5),bt_e)
      call set_values_(table(6),bd_i)
      call set_values_(table(7),bd_e)
      call set_values_(table(8),bd_norm)
      call set_values_(table(9),bd_area)

      ! Put
      call put_(table,label_rows=FALSE)

      ! Clean up!
      call destroy_(bd_area)
      call destroy_(bd_norm)
      call destroy_(bd_e)
      call destroy_(bd_i)
      call destroy_(bt_e)
      call destroy_(ba_e)
      call destroy_(bt_i)
      call destroy_(ba_i)
      call destroy_(b1)
      call destroy_(bb)
      call destroy_(table)
      call destroy_(face)
      call destroy_(blob)
      call destroy_(a_i)
      call destroy_(a_e)
      call destroy_(d_i)
      call destroy_(d_e)
      call destroy_(d_norm)
   end subroutine

   PURE subroutine facify(self,blob,face)
   ! Given set(s) of vertices in a "blob" return the indices of the
   ! corresponding "face"s. Note that all three vertices of a face
   ! need to be in the vertex list "blob" before a face is added to
   ! the "face" blob.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(type(EVEC_INT_TYPE),:), IN :: blob
      VEC(type(EVEC_INT_TYPE),:), allocatable, OUT :: face
      INT :: n_blob,b,f,v1,v2,v3
   ENSURE(self%n_face>0,"ISOSURFACE:facify ... there are no isosurface faces")
      n_blob = size(blob)
      call create_(face,n_blob)
      do b = 1,n_blob
         call create_(face(b)%element,0)
         do f = 1,self%n_face
            v1 = self%face(1,f)
            v2 = self%face(2,f)
            v3 = self%face(3,f)

            ! Is this face in blob(b) ?
            if (any(v1==blob(b)%element)) then
               if (any(v2==blob(b)%element)) then
                  if (any(v3==blob(b)%element)) then
                     call append_(face(b)%element,f)
                  end if
               end if
            end if
         end do
      end do
   end subroutine

   subroutine put_connected_area(self)
   ! Put the connected area for a given ".surface_property" out.
      type(ISOSURFACE_TYPE), IN :: self
      REAL :: area
      BIN :: lb_set,ub_set
   ENSURE(property_bounds_set_(self),"ISOSURFACE:put_connected_area ... unacceptable property bounds")
   ENSURE(self%surface_property_lb<self%surface_property_ub,"ISOSURFACE:put_connected_area ... unacceptable property bounds")
   ENSURE(surface_point_set_(self),"ISOSURFACE:put_connected_area ... surface_point not set")
      lb_set = surface_property_ub_set_(self)
      ub_set = surface_property_ub_set_(self)
      if (lb_set AND ub_set) then
         area = connected_property_area_(self,self%surface_property,self%surface_property_lb,self%surface_property_ub)
      else if (lb_set) then
         area = connected_property_area_(self,self%surface_property,lower=self%surface_property_lb)
      else if (ub_set) then
         area = connected_property_area_(self,self%surface_property,upper=self%surface_property_lb)
      end if
      call flush_(stdout)
      call text_(stdout,"ISOSURFACE Property area")
      call flush_(stdout)
      call show_(stdout,"Surface property               =",self%surface_property)
      call show_(stdout,"Surface point                  =",self%surface_point)
      call show_(stdout,"Index of nearest surface point =",index_of_nearest_point_(self))
      if (lb_set) call show_(stdout,"Property lower bound           =",self%surface_property_lb)
      if (ub_set) call show_(stdout,"Property upper bound           =",self%surface_property_ub)
      call show_(stdout,"Connected area                 =",area)
   end subroutine

   PURE function index_of_nearest_point(self) result (res)
   ! Returns the index of the nearest isosurface point to .surface_point.
      type(ISOSURFACE_TYPE), IN :: self
      INT :: res
      MAT(REAL,:,:), allocatable :: point
   ENSURE(self%n_pt>0,"ISOSURFACE:index_of_nearest_point ... there are no isosurface points")

      ! Get vectors to .surface_point
      point = self%point-spread(self%surface_point,dim=2,ncopies=size(self%point,2))
      res = index_of_minimum_column_norm_(point)
   end function

   PURE function connected_property_area_0(self,property,lower,upper) result (res)
   ! Returns the *connected* area of isosurface triangles where the "property"
   ! values of all vertices of a triangles connected to a "point" which lies on
   ! or near the surface is between "lower" and "upper", if present.
      type(ISOSURFACE_TYPE), IN :: self
      STR(len=*), IN :: property
      REAL, optional, IN :: lower,upper
      REAL :: res
      VEC(REAL,:), allocatable :: W
   ENSURE(self%n_pt>0,"ISOSURFACE:connected_property_area_0 ... there are no isosurface points")
   ENSURE(present(lower) OR present(upper),"ISOSURFACE:connected_property_area_0 ... no bounds specified")
      res = ZERO
      select case (property)

      case ("mean_curvature")
         res = connected_property_area_(self,self%point_mean_curvature,lower,upper)

      case ("gaussian_curvature")
         res = connected_property_area_(self,self%point_gaussian_curvature,lower,upper)

      case ("rms_curvature")
         call create_(W,self%n_pt)
         call make_vertex_RMS_curvature_(self,W)
         res = connected_property_area_(self,W,lower,upper)
         call destroy_(W)

      case ("curvedness")
         call create_(W,self%n_pt)
         call make_vertex_curvedness_(self,W)
         res = connected_property_area_(self,W,lower,upper)
         call destroy_(W)

      case ("shape_index")
         call create_(W,self%n_pt)
         call make_vertex_shape_index_(self,W)
         res = connected_property_area_(self,W,lower,upper)
         call destroy_(W)
      end select
   end function

   PURE function connected_property_area_1(self,property,lower,upper) result (res)
   ! Returns the *connected* area of isosurface triangles where the "property"
   ! values of all vertices of a triangles connected to a "point" which lies on
   ! or near the surface is between "lower" and "upper", if present.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), IN :: property
      REAL, optional, IN :: lower,upper
      REAL :: res
      INT :: ind
      VEC(INT,2) :: loc
   ENSURE(self%n_pt>0,"ISOSURFACE:connected_property_area_1 ... there are no isosurface points")
   ENSURE(size(property)==self%n_pt,"ISOSURFACE:connected_property_area_1 ... wrong size, property array")
   ENSURE(present(lower) OR present(upper),"ISOSURFACE:connected_property_area_1 ... no bounds specified")
      ind = index_of_nearest_point_(self)
      loc = minloc(self%face-ind)
      res = connected_property_area_(self,property,loc(2),lower,upper)
   end function

   PURE function connected_property_area_2(self,property,ind,lower,upper) result (res)
   ! Returns the *connected* area of isosurface triangles where the
   ! "property" values of all vertices of triangles connected to a
   ! face with index "ind" on the surface is between "lower" and
   ! "upper", if present.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), IN :: property
      INT, IN :: ind
      REAL, optional, IN :: lower,upper
      REAL :: res
      INT :: n_vertex,i,v,p
      VEC(BIN,3) :: add_face
      VEC(INT,:), allocatable :: face,vertex
   ENSURE(self%n_pt>0,"ISOSURFACE:connected_property_area_2 ... there are no isosurface points")
   ENSURE(self%n_face>0,"ISOSURFACE:connected_property_area_2 ... there are no isosurface faces")
   ENSURE(size(property)==self%n_pt,"ISOSURFACE:connected_property_area_2 ... wrong size, property array")
   ENSURE(present(lower) OR present(upper),"ISOSURFACE:connected_property_area_2 ... no bounds specified")

      ! Connected faces & vertices
      call append_(face,ind)
      call append_(vertex,self%face(:,ind))

      ! Loop over new 
      ! connected vertices
      do
         n_vertex = size(vertex)

         ! Loop over faces
         do i = 1,self%n_face

            ! Ignore faces already found
            if (any(i==face)) cycle

            ! Do any vertices in vertex match this face?
            if (all(vertex/=self%face(1,i)) AND all(vertex/=self%face(2,i)) AND all(vertex/=self%face(3,i))) cycle

            ! Are vertex properties of this 
            ! face between property limits?
            add_face = FALSE
            do v = 1,3
               p = self%face(v,i)
               if (present(lower)) then
                  if (property(p)<lower) cycle
               end if
               if (present(upper)) then
                  if (property(p)>upper) cycle
               end if
               add_face(v) = TRUE
            end do

            ! This face passes limits
            ! Add it and its vertices
            if (all(add_face)) then
               call append_(face,i)
               call append_only_if_unique_(vertex,self%face(1,i))
               call append_only_if_unique_(vertex,self%face(2,i))
               call append_only_if_unique_(vertex,self%face(3,i))
            end if
         end do
         if (size(vertex)==n_vertex) exit
      end do
      res = total_area_(self,face)
      call destroy_(vertex)
      call destroy_(face)
   end function


!  ===============
!  Area and volume
!  ===============

   PURE function total_area(self,faces) result (res)
   ! Returns the *total* area of the list of "faces".
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,:), IN :: faces
      REAL :: res
      INT :: i
   ENSURE(size(faces)>0,"ISOSURFACE:total_area ... faces array, zero size")
   ENSURE(maxval(faces)<=self%n_face,"ISOSURFACE:total_area ... faces array, value too large")
   ENSURE(minval(faces)>0,"ISOSURFACE:total_area ... faces array, nonpositive value")
   ENSURE(self%n_face>0,"ISOSURFACE:total_area ... there are no isosurface points")
      res = ZERO
      do i = 1,size(faces)
         res = res+face_area_(self,faces(i))
      end do
   end function

   PURE function face_area(self,face) result (res)
   ! Returns the face area for the face with index "face".
      type(ISOSURFACE_TYPE), IN :: self
      INT, IN :: face
      REAL :: res
      VEC(REAL,3) :: a,b,c
   ENSURE(face>0,"ISOSURFACE:face_area ... face, nonpositive value")
   ENSURE(face<=self%n_face,"ISOSURFACE:face_area ... face, value too large")
   ENSURE(self%n_face>0,"ISOSURFACE:face_area ... there are no isosurface points")
      a = self%point(:,self%face(2,face))-self%point(:,self%face(1,face))
      b = self%point(:,self%face(3,face))-self%point(:,self%face(1,face))
      c = cross_(a,b)
      res = HALF*norm_(c)
   end function

   PURE function face_normal(self,face) result (res)
   ! Returns the face normal for the "face". This is not determined within a
   ! sign (I think) since we don't know what is the inside or outside. (However,
   ! the algorithm may generate the face points in the clockwise order so that
   ! the normal calculated below *is* pointing outwards -- I should check that).
      type(ISOSURFACE_TYPE), IN :: self
      INT, IN :: face
      VEC(REAL,3) :: res
      VEC(REAL,3) :: a,b
   ENSURE(face>0,"ISOSURFACE:face_normal ... face, nonpositive value")
   ENSURE(face<=self%n_face,"ISOSURFACE:face_normal ... face, value too large")
   ENSURE(self%n_face>0,"ISOSURFACE:face_normal ... there are no isosurface points")
      a = self%point(:,self%face(2,face))-self%point(:,self%face(1,face))
      b = self%point(:,self%face(3,face))-self%point(:,self%face(1,face))
      res = cross_(a,b)
      call normalize_(res)
   end function

   PURE function face_midpoint(self,face) result (res)
   ! Returns the face midpoint for the "face".
      type(ISOSURFACE_TYPE), IN :: self
      INT, IN :: face
      VEC(REAL,3) :: res
      VEC(REAL,3) :: a,b,c
   ENSURE(face>0,"ISOSURFACE:face_midpoint ... face, nonpositive value")
   ENSURE(face<=self%n_face,"ISOSURFACE:face_midpoint ... face, value too large")
   ENSURE(self%n_face>0,"ISOSURFACE:face_midpoint ... there are no isosurface points")
      a = self%point(:,self%face(1,face))
      b = self%point(:,self%face(2,face))
      c = self%point(:,self%face(3,face))
      res = THIRD*(a+b+c)
   end function

   PURE function average_face_normal(self,n) result (res)
   ! Returns the average normal for face "n", calculated as the
   ! average of the gradient normals at each vertex.
      type(ISOSURFACE_TYPE), IN :: self
      INT, IN :: n
      VEC(REAL,3) :: res
      VEC(REAL,3) :: a,b,c
   ENSURE(allocated(self%point_gradient),"ISOSURFACE:average_face_normal ... no point_gradient array")
   ENSURE(allocated(self%face),"ISOSURFACE:average_face_normal ... no face array")
   ENSURE(n<=size(self%face,2),"ISOSURFACE:average_face_normal ... n too large")
      a = self%point_gradient(:,self%face(1,n)); call normalize_(a)
      b = self%point_gradient(:,self%face(2,n)); call normalize_(b)
      c = self%point_gradient(:,self%face(3,n)); call normalize_(c)
      res = (a+b+c)/THREE
      call normalize_(res)
   end function

   PURE function average_face_gradient(self,n) result (res)
   ! Returns the average gradient for face "n"
      type(ISOSURFACE_TYPE), IN :: self
      INT, IN :: n
      VEC(REAL,3) :: res
      VEC(REAL,3) :: a,b,c
   ENSURE(allocated(self%point_gradient),"ISOSURFACE:average_face_gradient ... no point_gradient array")
   ENSURE(allocated(self%face),"ISOSURFACE:average_face_gradient ... no face array")
   ENSURE(n<=size(self%face,2),"ISOSURFACE:average_face_gradient ... n too large")
      a = self%point_gradient(:,self%face(1,n))
      b = self%point_gradient(:,self%face(2,n))
      c = self%point_gradient(:,self%face(3,n))
      res = (a+b+c)/THREE
   end function

   PURE function point_normal(self,n) result (res)
   ! Returns the point normal for point "n", calculated as the norm of the
   ! point_gradient for than point
      type(ISOSURFACE_TYPE), IN :: self
      INT, IN :: n
      VEC(REAL,3) :: res
   ENSURE(allocated(self%point_gradient),"ISOSURFACE:point_normal ... no point_gradient array")
   ENSURE(n<=self%n_pt,"ISOSURFACE:point_normal ... n too large")
   ENSURE(n>0,"ISOSURFACE:point_normal ... n must be positive")
      res = self%point_gradient(:,n)
      call normalize_(res)
   end function

   PURE subroutine set_area(self)
   ! Set the *total* area of the isosurface
      type(ISOSURFACE_TYPE), INOUT :: self
      INT :: i
   ENSURE(allocated(self%face),"ISOSURFACE:set_area ... no isosurface faces created")
   ENSURE(self%n_face>0,"ISOSURFACE:set_area ... no isosurface faces")
      self%area = ZERO
      do i = 1,self%n_face
         self%area = self%area+face_area_(self,i)
      end do
   end subroutine

   PURE subroutine set_volume(self)
   ! Set the *total* volume of the isosurface. This is set in two
   ! ways: from the face normals, and from the average of the vertex
   ! normals, calculated from the .point_gradients.
      type(ISOSURFACE_TYPE), INOUT :: self
      INT :: f,i1,i2,i3
      REAL :: base,height,h1,h2,h3
   ENSURE(allocated(self%point),"ISOSURFACE:set_volume ... no isosurface points created")
   ENSURE(self%n_pt>0,"ISOSURFACE:set_volume ... no isosurface points")
   ENSURE(allocated(self%face),"ISOSURFACE:set_volume ... no isosurface faces created")
   ENSURE(self%n_face>0,"ISOSURFACE:set_volume ... no isosurface faces")
      self%volume = ZERO
      self%an_volume = ZERO
      do f = 1,self%n_face
         i1 = self%face(1,f)
         i2 = self%face(2,f)
         i3 = self%face(3,f)
         base = face_area_(self,f)
         height = dot_product(face_normal_(self,f),self%point(:,i1))
         self%volume = self%volume+base*height
         h1 = dot_product(point_normal_(self,i1),self%point(:,i1))
         h2 = dot_product(point_normal_(self,i2),self%point(:,i2))
         h3 = dot_product(point_normal_(self,i3),self%point(:,i3))
         self%an_volume = self%an_volume+base*(h1+h2+h3)
      end do
      self%volume = THIRD*self%volume
      self%an_volume = NINTH*self%an_volume
   end subroutine


!  ==================
!  Surface properties
!  ==================

   PURE subroutine make_vertex_RMS_curvature(self,RMS)
   ! Get the "RMS" curvature values for each canonical point
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), OUT :: RMS
      VEC(REAL,:), allocatable :: k1,k2
   ENSURE(self%n_pt>0,"ISOSURFACE:make_vertex_RMS_curvature ... no isosurface points")
   ENSURE(size(RMS)==self%n_pt,"ISOSURFACE:make_vertex_RMS_curvature ... wrong size, RMS")
      call create_(k1,self%n_pt)
      call create_(k2,self%n_pt)
      call make_principal_curvatures_(self,k1,k2)
      RMS = sqrt((k1*k1+k2*k2)/TWO)
      call destroy_(k2)
      call destroy_(k1)
   end subroutine

   PURE subroutine make_principal_curvatures(self,k1,k2)
   ! Get the principal curvatures "k1" and "k2" for each canonical point.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), OUT :: k1,k2
      REAL :: m,g,val
      INT :: i
   ENSURE(allocated(self%point_mean_curvature),"ISOSURFACE:make_principal_curvatures ... no mean curvatures")
   ENSURE(allocated(self%point_gaussian_curvature),"ISOSURFACE:make_principal_curvatures ... no gaussian curvatures")
   ENSURE(self%n_pt>0,"ISOSURFACE:make_principal_curvatures ... no isosurface points")
   ENSURE(size(k1)==self%n_pt,"ISOSURFACE:make_principal_curvatures ... wrong size, k1")
   ENSURE(size(k2)==self%n_pt,"ISOSURFACE:make_principal_curvatures ... wrong size, k1")
      do i = 1,size(k1)
         m = self%point_mean_curvature(i)
         g = self%point_gaussian_curvature(i)
         val = m*m-g
         if (val<ZERO) then
            k1(i) = m
            k2(i) = m
         else
            val = sqrt(val)
            k1(i) = m+val
            k2(i) = m-val
         end if
      end do
   end subroutine

   PURE subroutine make_vertex_curvedness_0(self,c)
   ! Get the list of Koenderinks curvedness values "c" for each point
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), OUT :: c
      VEC(REAL,:), allocatable :: k1,k2
   ENSURE(self%n_pt>0,"ISOSURFACE:make_vertex_curvedness_0 ... no isosurface points")
   ENSURE(size(c)==self%n_pt,"ISOSURFACE:make_vertex_curvedness_0 ... wrong size, c")

      ! Get curvatures
      call create_(k1,self%n_pt)
      call create_(k2,self%n_pt)
      call make_principal_curvatures_(self,k1,k2)

      ! Do it
      call make_vertex_curvedness_(self,c,k1,k2)

      ! Clean
      call destroy_(k2)
      call destroy_(k1)
   end subroutine

   PURE subroutine make_vertex_curvedness_1(self,c,k1,k2)
   ! Get the list of Koenderinks curvedness values "c" for each
   ! vertex point from the principal curvatures "k1" and "k2"
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), OUT :: c
      VEC(REAL,:), IN :: k1,k2
      REAL :: fac,small
      INT :: i
   ENSURE(self%n_pt>0,"ISOSURFACE:make_vertex_curvedness_1 ... no isosurface points")
   ENSURE(size(C)==self%n_pt,"ISOSURFACE:make_vertex_curvedness_1 ... wrong size, C")
   ENSURE(size(k1)==self%n_pt,"ISOSURFACE:make_vertex_curvedness_1 ... wrong size, k1")
   ENSURE(size(k2)==self%n_pt,"ISOSURFACE:make_vertex_curvedness_1 ... wrong size, k2")
      C = sqrt((k1*k1+k2*k2)/TWO)

      ! First we need to set zero values to something small
      ! I choose to set them to the smallest non-zero value
      small = huge(ONE)
      do i = 1,self%n_pt
         if (C(i)==ZERO) cycle
         if (C(i)<small) small = C(i)
      end do
      do i = 1,self%n_pt
         if (C(i)/=ZERO) cycle
         C(i) = small
      end do
      fac = TWO/PI
      C = fac*log(C)
   end subroutine

   PURE subroutine make_vertex_shape_index_0(self,si)
   ! Make the list of Koenderinks shape index values "si"
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), OUT :: si
      VEC(REAL,:), allocatable :: k1,k2
   ENSURE(self%n_pt>0,"ISOSURFACE:make_vertex_shape_index_0 ... no isosurface points")
   ENSURE(size(si)==self%n_pt,"ISOSURFACE:make_vertex_shape_index_0 ... wrong size, SI")

      ! Get curvatures
      call create_(k1,self%n_pt)
      call create_(k2,self%n_pt)
      call make_principal_curvatures_(self,k1,k2)

      ! Do it
      call make_vertex_shape_index_(self,si,k1,k2)

      ! Clean
      call destroy_(k2)
      call destroy_(k1)
   end subroutine

   PURE subroutine make_vertex_shape_index_1(self,SI,k1,k2)
   ! Make the list of Koenderinks shape index values "si" from the
   ! principal curvatures "k1" and "k2" at each point.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), OUT :: SI
      VEC(REAL,:), IN :: k1,k2
      REAL :: c1,c2,fac,mx,mn,sm
      INT :: i
   ENSURE(self%n_pt>0,"ISOSURFACE:make_vertex_shape_index_1 ... no isosurface points")
   ENSURE(size(SI)==self%n_pt,"ISOSURFACE:make_vertex_shape_index_1 ... wrong size, SI")
   ENSURE(size(k1)==self%n_pt,"ISOSURFACE:make_vertex_shape_index_1 ... wrong size, k1")
   ENSURE(size(k2)==self%n_pt,"ISOSURFACE:make_vertex_shape_index_1 ... wrong size, k2")
      fac = -TWO/PI
      do i = 1,self%n_pt
         c1 = k1(i)
         c2 = k2(i)
         if (c1/=c2) then
            mx = max(c1,c2)
            mn = min(c1,c2)
            sm = c1+c2
            SI(i) = fac*atan(sm/(mx-mn))
         else
            SI(i) = -sign(ONE,c1)
         end if
      end do
   end subroutine

   PURE subroutine make_vertex_SI_and_curvedness(self,si,cn)
   ! Make the list of Koenderinks shape index values "SI" and the
   ! curvedness C from the principal curvatures.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), OUT :: si,cn
      VEC(REAL,:), allocatable :: k1,k2
   ENSURE(self%n_pt>0,"ISOSURFACE:make_vertex_SI_and_curvedness ... no isosurface points")
   ENSURE(size(si)==self%n_pt,"ISOSURFACE:make_vertex_SI_and_curvedness ... wrong size, si")
   ENSURE(size(cn)==self%n_pt,"ISOSURFACE:make_vertex_SI_and_curvedness ... wrong size, cn")

      ! Make the curvatures
      call create_(k1,self%n_pt)
      call create_(k2,self%n_pt)
      call make_principal_curvatures_(self,k1,k2)

      ! Do it
      call make_vertex_shape_index_(self,si,k1,k2)
      call make_vertex_curvedness_(self,cn,k1,k2)

      ! Clean
      call destroy_(k2)
      call destroy_(k1)
   end subroutine

   pure subroutine make_vertex_normals(self,normal)
   ! Make the list of normals for each canonical point
      type(ISOSURFACE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: normal
      VEC(REAL,3) :: n
      INT :: i
      do i = 1,self%n_pt
         n = self%point_gradient(:,i)
         call normalize_(n)
         normal(:,i) = n
      end do
   end subroutine

   PURE subroutine make_average_face_normals(self,normal)
   ! Put the list of average face normals
      type(ISOSURFACE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: normal
      VEC(REAL,3) :: v
      INT :: i
      do i = 1,self%n_face
         v = average_face_normal_(self,i)
         if (is_zero_(v)) v = [ONE,ZERO,ZERO]
         call normalize_(v)
         normal(:,i) = v
      end do
   end subroutine

   PURE function d_min_to(self,list) result (res)
   ! Put out the minimum distances from each point on the
   ! isosurface to the nearest .atom in the "list"
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,:), IN :: list
      VEC(REAL,:), allocatable :: res
      VEC(REAL,3) :: dist
      REAL :: dmin,norm
      INT :: n_atom,i,a
   ENSURE(allocated(self%point),"ISOSURFACE:d_min_to ... no points")
   ENSURE(allocated(self%atom),"ISOSURFACE:d_min_to ... no atom list")
   ENSURE(all_in_range_(list,[1,size(self%atom)]),"ISOSURFACE:d_min_to ... in list out of bounds")
      n_atom = size(list)
      call create_(res,self%n_pt)
      do i = 1,self%n_pt
         dmin = HUGE(ONE)
         do a = 1,n_atom
            dist = self%atom(list(a))%position-self%point(:,i)
            norm = norm_(dist)
            if (norm>=dmin) cycle
            dmin = norm
         end do
         res(i) = dmin
      end do
   end function

   PURE subroutine d_e_d_i_areas(self,listin,listout,res_in,res_out,areas)
   ! Calculates the minimum distance for each point on the isosurface
   ! to the nearest atom in the "list", grouped in the three points
   ! relevant to each face of the isosurface polygon, and then
   ! averages this over the area of that face.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,:), IN :: listin,listout
      VEC(REAL,self%n_face), OUT :: areas,res_in,res_out
      VEC(REAL,3) :: dist,distances_in,distances_out,vec_of_ones
      REAL :: dmin,norm,area
      INT :: n_atom_internal,n_atom_external,i,j,a
      MAT(REAL,3,3) :: tripoint
      VEC(REAL,3) :: v_1,v_2,v_3
   ENSURE(allocated(self%point),"ISOSURFACE:d_e_d_i_areas ... no points")
   ENSURE(allocated(self%atom),"ISOSURFACE:d_e_d_i_areas ... no atom list")
   ENSURE(allocated(self%face),"ISOSURFACE:d_e_d_i_areas ... no faces list")
   ENSURE(all_in_range_(listin,[1,size(self%atom)]),"ISOSURFACE:d_e_d_i_areas ... in list out of bounds")
   ENSURE(all_in_range_(listout,[1,size(self%atom)]),"ISOSURFACE:d_e_d_i_areas ... in list out of bounds")
      n_atom_external = size(listout)
      n_atom_internal = size(listin)
      vec_of_ones = ONE

   ! stdout.text("triangle areas:")
      do i = 1,self%n_face
         do j = 1,3

            ! Store the three coordinates for each point of the triangle
            tripoint(:,j) = self%point(:,self%face(j,i))
            dmin = HUGE(ONE)
            do a = 1,n_atom_external
               dist = self%atom(listout(a))%position-tripoint(:,j)
               norm = norm_(dist)
               if (norm<=dmin) then
                  dmin = norm
               end if
            end do
            distances_out(j) = dmin
            dmin = HUGE(ONE)
            do a = 1,n_atom_internal
               dist = self%atom(listin(a))%position-tripoint(:,j)
               norm = norm_(dist)
               if (norm<=dmin) then
                  dmin = norm
               end if
            end do
            distances_in(j) = dmin
         end do
         v_1 = tripoint(:,2)-tripoint(:,1)
         v_2 = tripoint(:,3)-tripoint(:,1)
         call to_cross_product_of_(v_3,v_1,v_2)
         area = HALF*norm_(v_3)

      ! stdout.text(" ")
      ! stdout.show("area =", area)
         res_in(i) = THIRD*sum(distances_in)
         res_out(i) = THIRD*sum(distances_out)
      ! stdout.show("res_in", res_in(i))
      ! stdout.show("res_out", res_out(i))
         areas(i) = area
      end do

   ! stdout.text("end triangle areas")
   end subroutine

   PURE subroutine make_d_min_d_norm_to(self,list,atom,d_min,d_norm)
   ! Put out the normalized distances from each point on the
   ! isosurface to the nearest .atom's given in the "list".
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,:), IN :: list
      VEC(type(ATOM_TYPE),:), IN :: atom
      VEC(REAL,self%n_pt), OUT :: d_min,d_norm
      MAT(REAL,:,:), allocatable :: pos
      VEC(REAL,:), allocatable :: rvdw
      VEC(REAL,3) :: dist,pt
      REAL :: dmin,dnorm,d,dn
      INT :: n_atom,i,a
   ENSURE(allocated(self%point),"ISOSURFACE:make_d_min_d_norm_to ... no points")
   ENSURE(all_in_range_(list,[1,size(atom)]),"ISOSURFACE:make_d_min_d_norm_to ... in list out of bounds")

      ! No of atoms in the list
      n_atom = size(list)

      ! The vdw radii
      call create_(rvdw,n_atom)
      call get_vdw_radii_ccdc_(atom(list),rvdw)

      ! The atom positions
      call create_(pos,3,n_atom)
      call put_coordinates_to_(atom(list),pos,positions_as_rows=FALSE)

      ! Get d_min and d_norm
      do i = 1,self%n_pt
         pt = self%point(:,i)
         dmin = HUGE(ONE)
         dnorm = HUGE(ONE)
         do a = 1,n_atom
            dist = pos(:,a)-pt
            d = norm_(dist)
            if (d<dmin) dmin = d
            dn = (d-rvdw(a))/rvdw(a)
            if (dn<dnorm) dnorm = dn
         end do
         d_min(i) = dmin
         d_norm(i) = dnorm
      end do

      ! Clean
      call destroy_(rvdw)
      call destroy_(pos)
   end subroutine

   PURE subroutine make_fingerprint_distances(self,d_e,d_i,d_norm_e,d_norm_i,d_norm,in,out,atom,angstrom)
   ! Put out lists of vertex surface curvature properties.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,self%n_pt), OUT :: d_e,d_i,d_norm_e,d_norm_i,d_norm
      VEC(INT,:), IN :: in,out
      VEC(type(ATOM_TYPE),:), IN :: atom
      BIN, optional, IN :: angstrom
      BIN :: angst
   ENSURE(all_in_range_(in,[1,size(atom)]),"ISOSURFACE:make_fingerprint_distances ... in list out of bounds")
   ENSURE(all_in_range_(out,[1,size(atom)]),"ISOSURFACE:make_fingerprint_distances ... in list out of bounds")
   ENSURE(has_no_elements_common_with_(in,out),"ISOSURFACE:make_fingerprint_distances ... in and out lists overlap!")

      ! Use angstroms?
      angst = FALSE
      if (present(angstrom)) angst = angstrom

      ! d_i, d_norm_i, d_e, d_norm_e and d_norm
      call make_d_min_d_norm_to_(self,in,atom,d_i,d_norm_i)
      call make_d_min_d_norm_to_(self,out,atom,d_e,d_norm_e)
      d_norm = d_norm_i+d_norm_e

      ! Convert if requested ...
      if (angst) then
         d_i = d_i*ANGSTROM_PER_BOHR
         d_e = d_e*ANGSTROM_PER_BOHR
      end if
   end subroutine

   PURE subroutine make_surface_d_info_0(self,d_norm,d_min,a_min,list,atom)
   ! Make the "d_norm" values for every point on the isosurface for
   ! the given "list" of "atoms". Also return "a_min" the list of
   ! closest atoms to a given point.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,self%n_pt), OUT :: d_norm
      VEC(REAL,self%n_pt), OUT :: d_min
      VEC(INT,self%n_pt), OUT :: a_min
      VEC(INT,:), IN :: list
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), allocatable :: pos
      VEC(REAL,:), allocatable :: rvdw
      VEC(REAL,3) :: dist,pt
      REAL :: dnorm,dmini,d,dn
      INT :: n_atom,i,a,amini
   ENSURE(allocated(self%point),"ISOSURFACE:make_surface_d_info_0 ... no points")
   ENSURE(all_in_range_(list,[1,size(atom)]),"ISOSURFACE:make_surface_d_info_0 ... in list out of bounds")

      ! No of atoms in the list
      n_atom = size(list)

      ! The vdw radii
      call create_(rvdw,n_atom)
      call get_vdw_radii_ccdc_(atom(list),rvdw)

      ! The atom positions
      call create_(pos,3,n_atom)
      call put_coordinates_to_(atom(list),pos,positions_as_rows=FALSE)

      ! Get d_norm
      do i = 1,self%n_pt

         ! Surface point
         pt = self%point(:,i)

         ! Loop over atoms
         dnorm = HUGE(ONE)
         dmini = HUGE(ONE)
         amini = 0
         do a = 1,n_atom

            ! Get current d_norm in "dn"
            dist = pos(:,a)-pt
            d = norm_(dist)
            dn = (d-rvdw(a))/rvdw(a)

            ! Store if smaller
            if (dn<dnorm) then
               dnorm = dn
            end if

            ! Store nearest atom
            if (d<dmini) then
               dmini = d
               amini = list(a)
            end if
         end do

         ! Set
         d_norm(i) = dnorm
         d_min(i) = dmini
         a_min(i) = amini
      end do

      ! Clean
      call destroy_(rvdw)
      call destroy_(pos)
   end subroutine

   PURE subroutine make_surface_d_info_1(self,d_norm,d_e,d_i,a_e,a_i,in,out,atom)
   ! Get the complete "d_norm" values for every point on the
   ! isosurface, the "d_e" and "d_i" values, as well as the nearest
   ! atoms exterior and interior the surface, "a_e" and "a_i", where
   ! the indices of the "atom"s "in" and "out" of the surface are
   ! provided externally. 
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,self%n_pt), OUT :: d_norm
      VEC(REAL,self%n_pt), OUT :: d_e
      VEC(REAL,self%n_pt), OUT :: d_i
      VEC(INT,self%n_pt), OUT :: a_e
      VEC(INT,self%n_pt), OUT :: a_i
      VEC(INT,:), IN :: in,out
      VEC(type(ATOM_TYPE),:), IN :: atom
      VEC(REAL,:), allocatable :: d_norm_e,d_norm_i
   ENSURE(all_in_range_(in,[1,size(atom)]),"ISOSURFACE:make_surface_d_info_1 ... in list out of bounds")
   ENSURE(all_in_range_(out,[1,size(atom)]),"ISOSURFACE:make_surface_d_info_1 ... in list out of bounds")
   ENSURE(has_no_elements_common_with_(in,out),"ISOSURFACE:make_surface_d_info_1 ... in and out lists overlap!")

      ! Temporaries
      call create_(d_norm_i,self%n_pt)
      call create_(d_norm_e,self%n_pt)

      ! Get the two d_norm's
      call make_surface_d_info_(self,d_norm_e,d_e,a_e,out,atom)
      call make_surface_d_info_(self,d_norm_i,d_i,a_i,in,atom)

      ! Make d_norm from the formula
      d_norm = d_norm_i+d_norm_e

      ! Clean
      call destroy_(d_norm_e)
      call destroy_(d_norm_i)
   end subroutine

   PURE subroutine make_fingerprint_atoms(self,d_e_atoms,d_i_atoms,in,out)
   ! Put out lists of atoms in "atom" which are closest to a given
   ! vertex of the surface.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,self%n_pt), OUT :: d_e_atoms,d_i_atoms
      VEC(INT,:), IN :: in,out
      call make_d_min_atoms_to_(self,in,d_i_atoms)
      call make_d_min_atoms_to_(self,out,d_e_atoms)
   end subroutine

   PURE subroutine make_fingerprint_face_atoms(self,d_e_atoms,d_i_atoms,in,out)
   ! Put out lists of atoms in "atom" which are closest to a given
   ! face of the surface.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,self%n_face), OUT :: d_e_atoms,d_i_atoms
      VEC(INT,:), IN :: in,out
      call make_d_min_atoms_to_face_(self,in,d_i_atoms)
      call make_d_min_atoms_to_face_(self,out,d_e_atoms)

      ! Subtract off the # of d_i_atoms since the
      ! "out" array points into the whole "atom" list
      ! and the first few are the "inside" atoms
      d_e_atoms = d_e_atoms-size(in)
   end subroutine

   PURE subroutine make_d_min_atoms_to(self,list,d_min_list)
   ! Make the atom indices corresponding to the minimum distances
   ! from each point on the isosurface to the nearest .atom in the "list"
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,:), IN :: list
      VEC(INT,size(list)), OUT :: d_min_list
      VEC(REAL,3) :: dist
      REAL :: dmin,norm
      INT :: n_atom,i,a,amin
   ENSURE(allocated(self%point),"ISOSURFACE:make_d_min_atoms_to ... no points")
   ENSURE(allocated(self%atom),"ISOSURFACE:make_d_min_atoms_to ... no atom list")
   ENSURE(size(list)>=0,"ISOSURFACE:make_d_min_atoms_to ... no list")
   ENSURE(all_in_range_(list,[1,size(self%atom)]),"ISOSURFACE:make_d_min_atoms_to ... in list out of bounds")
      n_atom = size(list)
      do i = 1,self%n_pt
         dmin = HUGE(ONE)
         amin = 0
         do a = 1,n_atom
            dist = self%atom(list(a))%position-self%point(:,i)
            norm = norm_(dist)
            if (norm>=dmin) cycle
            dmin = norm
            amin = list(a)
         end do
         d_min_list(i) = amin
      end do
   end subroutine

   PURE subroutine make_d_min_atoms_to_face(self,list,d_min_list)
   ! Make the atom indices corresponding to the minimum distances
   ! from each face of the isosurface to the nearest .atom in the "list"
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,:), IN :: list
      VEC(INT,self%n_face), OUT :: d_min_list
      VEC(REAL,3) :: dist,pt
      REAL :: d2min,d2
      INT :: n_atom,f,f1,f2,f3,a,amin
   ENSURE(allocated(self%point),"ISOSURFACE:make_d_min_atoms_to_face ... no points")
   ENSURE(allocated(self%atom),"ISOSURFACE:make_d_min_atoms_to_face ... no atom list")
   ENSURE(size(list)>=0,"ISOSURFACE:make_d_min_atoms_to_face ... no list")
   ENSURE(all_in_range_(list,[1,size(self%atom)]),"ISOSURFACE:make_d_min_atoms_to_face ... in list out of bounds")
      n_atom = size(list)
      do f = 1,self%n_face
         f1 = self%face(1,f)
         f2 = self%face(2,f)
         f3 = self%face(3,f)
         pt = THIRD*(self%point(:,f1)+self%point(:,f2)+self%point(:,f3))
         d2min = HUGE(ONE)
         amin = 0
         do a = 1,n_atom
            dist = self%atom(list(a))%position-pt
            d2 = dist(1)*dist(1)+dist(2)*dist(2)+dist(3)*dist(3)
            if (d2>=d2min) cycle
            d2min = d2
            amin = list(a)
         end do
         d_min_list(f) = amin
      end do
   end subroutine

   PURE function surface_group_atom_indices(self,atom) result (res)
   ! Return a list of atom indices which represent a molecule-group
   ! which touches the isosurface.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      VEC(INT,:), allocatable :: res
      VEC(REAL,3) :: pos,apos,d
      VEC(REAL,:), allocatable :: group_for_pt
      REAL :: d2min,d2
      INT :: p,a,agrp,g,ga
   ENSURE(allocated(self%point),"ISOSURFACE:surface_group_atom_indices ... no points")

      ! Create the index list
      call create_(res,0)

      ! Assign each pt to a group
      call create_(group_for_pt,self%n_pt)
      group_for_pt = 0
      do p = 1,self%n_pt
         pos = self%point(:,p)
         d2min = HUGE(ONE)
         g = 0
         do a = 1,size(atom)
            agrp = atom(a)%group
            apos = atom(a)%position
            d = apos-pos
            d2 = d(1)*d(1)+d(2)*d(2)+d(3)*d(3)
            if (d2>=d2min) cycle

            ! New minimal-distance atom
            d2min = d2
            g = agrp
            ga = a  ! Atom for this group
         end do
         group_for_pt(p) = g

         ! New group? Then store atom.
         if (p==1) then
            call append_(res,ga)
         else if (all(group_for_pt(1:p-1)/=g)) then
            call append_(res,ga)
         end if
      end do

      ! Clean
      call destroy_(group_for_pt)
   end function

   PURE subroutine make_pts_list_for_group(self,pts_for_group)
   ! Return a "list" of indices for .point's which are closest to any
   ! atoms belonging to group "g" (not group 1, which assumed to be
   ! the central molecule).
      type(ISOSURFACE_TYPE), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable, OUT :: pts_for_group
      REAL :: d2min,d2
      VEC(REAL,3) :: pos,apos,d
      INT :: p,a,agrp,g,n_group,n,i
      VEC(INT,:), allocatable :: group_for_pt,group
   ENSURE(allocated(self%point),"ISOSURFACE:make_pts_list_for_group ... no points")
   ENSURE(allocated(self%atom),"ISOSURFACE:make_pts_list_for_group ... no atom list")
   ENSURE(maxval(self%atom%group)>1,"ISOSURFACE:make_pts_list_for_group ... not enough groups")

      ! Assign each pt to a group
      call create_(group_for_pt,self%n_pt)
      group_for_pt = 0
      do p = 1,self%n_pt
         pos = self%point(:,p)
         d2min = HUGE(ONE)
         g = 0
         do a = 1,size(self%atom)
            agrp = self%atom(a)%group
            apos = self%atom(a)%position
            if (agrp<=1) cycle
            d = apos-pos
            d2 = d(1)*d(1)+d(2)*d(2)+d(3)*d(3)
            if (d2>=d2min) cycle
            d2min = d2
            g = agrp
         end do
         group_for_pt(p) = g
      end do
      ENSURE(all(group_for_pt>0),"ISOSURFACE:make_pts_list_for_group ... not all points have a group")

      ! Now make the list. Group 1 not there.
      n_group = maxval(self%atom(:)%group)
      call create_(pts_for_group,n_group)
      do g = 2,n_group
         n = count(group_for_pt==g)
         call create_(pts_for_group(g)%element,n)
         i = 0
         do p = 1,self%n_pt
            if (group_for_pt(p)==g) then
               i = i+1
               pts_for_group(g)%element(i) = p
            end if
         end do
      end do

      ! Clean
      call destroy_(group)
      call destroy_(group_for_pt)
   end subroutine


!  ===================
!  Merge an isosurface
!  ===================

   PURE subroutine merge_with(self,other)
   ! This routine takes the "other" isosurface and merges it into the
   ! current isosurface. Of course, the settings may differ between
   ! the two surfaces: the settings for self are retained. It is
   ! intended that the surfaces to be merged will be of the same kind,
   ! only different for the defining iso_values.
      type(ISOSURFACE_TYPE), INOUT :: self
      type(ISOSURFACE_TYPE), IN :: other
      if (allocated(other%point)) call append_columns_(self%point,other%point)
      if (allocated(other%point_gradient)) call append_columns_(self%point_gradient,other%point_gradient)
      if (allocated(other%point_mean_curvature)) call append_(self%point_mean_curvature,other%point_mean_curvature)
      if (allocated(other%point_gaussian_curvature)) call append_(self%point_gaussian_curvature,other%point_gaussian_curvature)
      if (allocated(other%face)) call append_columns_(self%face,other%face+self%n_pt)
      if (allocated(other%surface_property_values)) call append_(self%surface_property_values,other%surface_property_values)
      if (allocated(other%surface_iso_values)) call append_(self%surface_iso_values,other%surface_iso_values)
      self%n_pt = self%n_pt+other%n_pt
      self%n_face = self%n_face+other%n_face
      self%area = self%area+other%area
      self%volume = self%volume+other%volume
      self%an_volume = self%an_volume+other%an_volume
   end subroutine


!  ==============
!  Output methods
!  ==============

   subroutine put(self)
   ! Put the isosurface data
      type(ISOSURFACE_TYPE), IN :: self
      REAL :: mean_vol
      INT :: n
      call flush_(stdout)
      call text_(stdout,"===============")
      call text_(stdout,"Isosurface data")
      call text_(stdout,"===============")
      call flush_(stdout)
      call show_(stdout,"Kind of surface          =",self%property)
      call show_(stdout,"Triangulation method     =",self%triangulation_method)
      call show_(stdout,"Iso value                =",self%iso_value)
      if (allocated(self%iso_values)) call show_(stdout,"Iso values               =",self%iso_values)
      call show_(stdout,"No. of isosurface points =",self%n_pt)
      call show_(stdout,"No. of isosurface faces  =",self%n_face)
      if (self%cap_ends/=0) then
         call show_(stdout,"Cap ends?                =",self%cap_ends)
         call show_(stdout,"Suppress caps?           =",self%suppress_caps)
         call show_(stdout,"No. of caps              =",self%n_cap)
      end if
   ! stdout.show("Big interior?            =",.big_interior)
   ! stdout.show("Reverse surface normals? =",.big_interior)

      ! These statistics include the points used for gradient
      ! and hessian evaluation just outside the grid perimeter.
      if (self%triangulation_method=="recursive_marching_cube") then
         n = (self%plot_grid%n_x+2)**3
         call flush_(stdout)
         call show_(stdout,"Minimum scan division    =",self%minimum_scan_division)
         call show_(stdout,"Minimum scan level       =",self%minimum_scan_level)
         call show_(stdout,"Voxel proximity factor   =",self%voxel_proximity_factor)
         call show_(stdout,"# of divisions           =",self%final_level)
         call show_(stdout,"# of scan divisions      =",self%scan_level)
         call show_(stdout,"# of func. evals         =",n-self%n_skip)
         call show_(stdout,"# of func. evals skipped =",self%n_skip)
         call show_(stdout,"% skipped                =",(1.0d2*self%n_skip)/n)
      end if
      if (self%n_pt>0) then
         mean_vol = HALF*(self%volume_min+self%volume_max)
         call flush_(stdout)
         call text_(stdout,"Volumes and areas ...")
         call flush_(stdout)
         call show_(stdout,"Surface area             =",self%area)
         call show_(stdout,"Interior volume          =",self%volume)
         call show_(stdout,"Average normal volume    =",self%an_volume)
         call show_(stdout,"Volume lower bound       =",self%volume_min)
         call show_(stdout,"Volume upper bound       =",self%volume_max)
         call show_(stdout,"Average of volume bounds =",mean_vol)
         call flush_(stdout)
         call text_(stdout,"Volumes and areas in Angstrom units ...")
         call flush_(stdout)
         call show_(stdout,"Surface area             =",to_units_(self%area,"angstrom^2"))
         call show_(stdout,"Interior volume          =",to_units_(self%volume,"angstrom^3"))
         call show_(stdout,"Average normal volume    =",to_units_(self%an_volume,"angstrom^3"))
         call show_(stdout,"Volume lower bound       =",to_units_(self%volume_min,"angstrom^3"))
         call show_(stdout,"Volume upper bound       =",to_units_(self%volume_max,"angstrom^3"))
         call show_(stdout,"Average of volume bounds =",to_units_(mean_vol,"angstrom^3"))
      end if
      call put_(self%plot_grid,"Isosurface plot grid data")
   end subroutine


! CX related output

   subroutine put_CX(self,in,out,atom,angstrom)
   ! Put the isosurface data in a form that the Crystal Explorer
   ! program can read it.  "in" and "out" are the indices of the atoms
   ! inside and outside the surface.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,:), IN :: in,out
      VEC(type(ATOM_TYPE),:), IN :: atom
      BIN, optional, IN :: angstrom
      BIN :: has_curvatures

      ! Basic surface data
      call put_points_(self,angstrom)
      call put_faces_(self)
      call put_vertex_normals_(self)
      call flush_(stdout)
      call text_(stdout,"begin vertex_properties ")

      ! Put standard CX fingerprint properties
      ! (de, di, dnorme, dnormi, dnorm)
      if (self%CX_output_distance_properties) call put_fingerprint_properties_(self,in,out,atom,angstrom)

      ! Shape index and curvedness
      has_curvatures = allocated(self%point_mean_curvature) AND allocated(self%point_gaussian_curvature)
      if (self%CX_output_shape_properties AND has_curvatures) call put_vertex_SI_and_curvedness_(self)

      ! User-requested property?
      if (allocated(self%surface_property_values)) call put_surface_property_values_(self)  ! up to 2
      if (allocated(self%surface_iso_values)) call put_surface_iso_values_(self)  ! up to 2

      ! Binned fingerprints
      if (self%CX_output_fingerprint) call put_binned_d_i_d_e_(self,in,out)
   ! .put_nearest_internal_atom_RGBs(in)
   ! .put_nearest_external_atom_RGBs(out)
   ! .put_binned_d_i_d_e_RGBs(in,out)
   ! .put_d_i_d_e_RGBs(in,out)
      call text_(stdout,"end vertex_properties ")
   end subroutine

   subroutine put_points(self,angstrom)
   ! Put the canonically indexed list of vertices for the object
      type(ISOSURFACE_TYPE), IN :: self
      BIN, optional, IN :: angstrom
      BIN :: angst
      MAT(REAL,:,:), allocatable :: point
      angst = FALSE
      if (present(angstrom)) angst = angstrom
      point = self%point
      if (angst) then
         point = point*ANGSTROM_PER_BOHR
      end if
      call put_vertex_property_(point,"vertices",self%n_pt)
      if (angst) call destroy_(point)
   end subroutine

   subroutine put_faces(self)
   ! Put the list of canonical indices for each triangular face
      type(ISOSURFACE_TYPE), IN :: self
      INT :: n_cap
      MAT(INT,:,:), allocatable :: face,cap
      n_cap = self%n_cap
      if (self%suppress_caps) n_cap = 0

      ! Put faces with caps, or not ...
      ! Subtract one from all indices
      if (self%suppress_caps OR self%n_cap==0) then
         call create_(face,3,self%n_face)
         face = self%face-1
         call put_vertex_property_(face,"indices",self%n_face)
         call destroy_(face)
      else
         call create_(face,3,self%n_face)
         call create_(cap,3,n_cap)
         face = self%face-1
         cap = self%cap-1
         call put_2_vertex_properties_(face,cap,"indices",self%n_face,n_cap)
         call destroy_(cap)
         call destroy_(face)
      end if
   end subroutine

   subroutine put_vertex_gradients(self)
   ! Put the list of gradients for each canonical point
      type(ISOSURFACE_TYPE), IN :: self
      call put_vertex_property_(self%point_gradient,"vertex_gradients",self%n_pt)
   end subroutine

   subroutine put_vertex_normals(self)
   ! Put the list of normals for each canonical point
      type(ISOSURFACE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: normal
      call create_(normal,3,self%n_pt)
      call make_vertex_normals_(self,normal)
      call put_vertex_property_(normal,"vertex_normals",self%n_pt)
      call destroy_(normal)
   end subroutine

   subroutine put_face_normals(self)
   ! Put the list of average face normals
      type(ISOSURFACE_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: normal
      call create_(normal,3,self%n_face)
      call make_average_face_normals_(self,normal)
      call put_vertex_property_(normal,"average_face_normal",self%n_face)
      call destroy_(normal)
   end subroutine

   subroutine put_fingerprint_properties(self,in,out,atom,angstrom)
   ! Put out the fingerprint properties.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,:), IN :: in,out
      VEC(type(ATOM_TYPE),:), IN :: atom
      BIN, optional :: angstrom
      VEC(REAL,:), allocatable :: d_e,d_i,d_norm_e,d_norm_i,d_norm
   ENSURE(maxval(in)<=size(atom),"ISOSURFACE:put_fingerprint_properties ... in atoms out of range")
   ENSURE(maxval(out)<=size(atom),"ISOSURFACE:put_fingerprint_properties ... out atoms out of range")
   ENSURE(minval(in)>0,"ISOSURFACE:put_fingerprint_properties ... in atoms out of range")
   ENSURE(minval(out)>0,"ISOSURFACE:put_fingerprint_properties ... out atoms out of range")

      ! d_i, d_norm_i, d_e, d_norm_e and d_norm
      call create_(d_i,self%n_pt)
      call create_(d_e,self%n_pt)
      call create_(d_norm_i,self%n_pt)
      call create_(d_norm_e,self%n_pt)
      call create_(d_norm,self%n_pt)
      call make_fingerprint_distances_(self,d_e,d_i,d_norm_e,d_norm_i,d_norm,in,out,atom,angstrom)

      ! Print it out
      call put_vertex_property_(d_i,"d_i",self%n_pt)
      call put_vertex_property_(d_e,"d_e",self%n_pt)
      call put_vertex_property_(d_norm_i,"d_norm_i",self%n_pt)
      call put_vertex_property_(d_norm_e,"d_norm_e",self%n_pt)
      call put_vertex_property_(d_norm,"d_norm",self%n_pt)

      ! Clean
      call destroy_(d_norm)
      call destroy_(d_norm_e)
      call destroy_(d_norm_i)
      call destroy_(d_e)
      call destroy_(d_i)
   end subroutine

   subroutine put_fingerprint_atoms(self,in,out)
   ! Put out the fingerprint properties.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,:), IN :: in,out
      VEC(INT,:), allocatable :: d_e_atoms,d_i_atoms

      ! d_i, d_norm_i, d_e, d_norm_e and d_norm
      call create_(d_i_atoms,self%n_pt)
      call create_(d_e_atoms,self%n_pt)
      call make_fingerprint_atoms_(self,d_e_atoms,d_i_atoms,in,out)

      ! Print it out
      call put_vertex_property_(d_i_atoms,"d_i_atoms",self%n_pt)
      call put_vertex_property_(d_e_atoms,"d_e_atoms",self%n_pt)

      ! Clean
      call destroy_(d_e_atoms)
      call destroy_(d_i_atoms)
   end subroutine

   subroutine put_fingerprint_face_atoms(self,in,out)
   ! Put out the fingerprint properties.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,:), IN :: in,out
      VEC(INT,:), allocatable :: d_e_atoms,d_i_atoms

      ! d_i, d_norm_i, d_e, d_norm_e and d_norm
      call create_(d_i_atoms,self%n_face)
      call create_(d_e_atoms,self%n_face)
      call make_fingerprint_face_atoms_(self,d_e_atoms,d_i_atoms,in,out)

      ! Print it out
      call put_vertex_property_(d_i_atoms,"d_i_face_atoms",self%n_face)
      call put_vertex_property_(d_e_atoms,"d_e_face_atoms",self%n_face)

      ! Clean
      call destroy_(d_e_atoms)
      call destroy_(d_i_atoms)
   end subroutine

   subroutine put_vertex_SI_and_curvedness(self)
   ! Put out the shape index.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), allocatable :: si,cn
      call create_(si,self%n_pt)
      call create_(cn,self%n_pt)
      call make_vertex_SI_and_curvedness_(self,si,cn)
      call put_vertex_property_(si,"shape_index",self%n_pt)
      call put_vertex_property_(cn,"curvedness",self%n_pt)
      call destroy_(cn)
      call destroy_(si)
   end subroutine

   subroutine put_surface_property_values(self)
   ! Put out the surface_property_values
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), allocatable :: values
   ENSURE(self%surface_property/=" ","ISOSURFACE:put_surface_property_values ... no surface property set")
   ENSURE(allocated(self%surface_property_values),"ISOSURFACE:put_surface_property_values ... no surface property values")

      ! Create a copy of property
      values = self%surface_property_values

      ! Chop it if requested
      if (surface_property_lb_set_(self)) call chop_small_values_(values,self%surface_property_lb)
      if (surface_property_ub_set_(self)) call chop_large_values_(values,self%surface_property_ub)

      ! Put out the surface property values
      call put_vertex_property_(values,self%surface_property,self%n_pt)

      ! Clean
      call destroy_(values)
   end subroutine

   subroutine put_surface_iso_values_0(self)
   ! Put out the surface_iso_values array
      type(ISOSURFACE_TYPE), IN :: self
   ENSURE(allocated(self%surface_iso_values),"ISOSURFACE:put_surface_iso_values_0 ... no surfacevalues")

      ! Orbital is a special case because normally orbitals
      ! are plotted with two iso_values, one +ve and the other -ve
      select case (self%property)
      case ("orbital            "); call put_surface_iso_values_(self,"orbital")
      case ("spin_density       "); call put_surface_iso_values_(self,"spin_density")
      case ("deformation_density"); call put_surface_iso_values_(self,"deformation_density")
      case ("electric_potential "); call put_surface_iso_values_(self,"electric_potential")
      case default; call put_surface_iso_values_(self,"iso_values")
      end select
   end subroutine

   subroutine put_surface_iso_values_1(self,label)
   ! Put out the .surface_iso_values for surfaces with multiple
   ! iso values ...
      type(ISOSURFACE_TYPE), IN :: self
      STR(len=*), IN :: label
   ENSURE(allocated(self%surface_iso_values),"ISOSURFACE:put_surface_iso_values_1 ... no surface iso values")
      call save_style_(stdout)
      call set_real_fortran_fmt_(stdout,"e")
      call put_vertex_property_(self%surface_iso_values,label,size(self%surface_iso_values))
      call unsave_style_(stdout)
   end subroutine


! These are not used by CX

   subroutine put_vertex_mean_curvatures(self)
   ! Put the list of mean curvatures for each canonical point
      type(ISOSURFACE_TYPE), IN :: self
      call put_vertex_property_(self%point_mean_curvature,"mean_curvature",self%n_pt)
   end subroutine

   subroutine put_vertex_gaussian_curvatures(self)
   ! Put the list of gaussian curvatures for each canonical point
      type(ISOSURFACE_TYPE), IN :: self
      call put_vertex_property_(self%point_gaussian_curvature,"gaussian_curvature",self%n_pt)
   end subroutine

   subroutine put_vertex_RMS_curvature(self)
   ! Put the list of RMS curvature values for each canonical point
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), allocatable :: RMS
   ENSURE(self%n_pt>0,"ISOSURFACE:put_vertex_RMS_curvature ... no isosurface points")
      call create_(RMS,self%n_pt)
      call make_vertex_RMS_curvature_(self,RMS)
      call put_vertex_property_(RMS,"RMS_curvature",self%n_pt)
      call destroy_(RMS)
   end subroutine

! Put vertex property routines

   subroutine put_vertex_property_0(prop,prop_name,n_size)
   ! Put a generic property out
      VEC(INT,:), IN :: prop
      STR(len=*), IN :: prop_name
      INT, IN :: n_size

      ! We don't want to use labels on vectors
      ! We don't want blocking arrays

      ! The following code is inherited from ISOSURFACE
      call save_style_(stdout)
      call set_using_array_labels_(stdout,FALSE)
      call set_using_fields_(stdout,FALSE)

      ! Print out
      call flush_(stdout)
      call text_(stdout,"begin "//trim(prop_name)//" "//trim(to_str_(n_size)))
      call put_(stdout,prop)
      call text_(stdout,"end "//trim(prop_name))

      ! Restore
      call unsave_style_(stdout)
   end subroutine

   subroutine put_vertex_property_1(prop,prop_name,n_size)
   ! Put a generic property out
      VEC(REAL,:), IN :: prop
      STR(len=*), IN :: prop_name
      INT, IN :: n_size

      ! We don't want to use labels on vectors
      ! We don't want blocking arrays

      ! The following code is inherited from ISOSURFACE
      call save_style_(stdout)
      call set_using_array_labels_(stdout,FALSE)
      call set_using_fields_(stdout,FALSE)

      ! Print out
      call flush_(stdout)
      call text_(stdout,"begin "//trim(prop_name)//" "//trim(to_str_(n_size)))
      call put_(stdout,prop)
      call text_(stdout,"end "//trim(prop_name))

      ! Restore
      call unsave_style_(stdout)
   end subroutine


   ! NB: the MAT{INT}/MAT{REAL} versions below have explicit bodies rather than a
   ! get_from, because the property is stored (3,N) and must be written TRANSPOSED
   ! (one point per line). A get_from substitution value cannot contain a comma —
   ! "PUT?=>prop,transpose=TRUE" is mis-split by both foo.pl and antlr4, which drop
   ! transpose and then write N-wide rows that overflow the BUFFER (256).

   subroutine put_vertex_property_2(prop,prop_name,n_size)
   ! Put a generic MAT{INT} property out, one point per line (transposed).
      MAT(INT,:,:), IN :: prop
      STR(len=*), IN :: prop_name
      INT, IN :: n_size

      ! We don't want array labels or blocking
      call save_style_(stdout)
      call set_using_array_labels_(stdout,FALSE)
      call set_using_fields_(stdout,FALSE)
      call flush_(stdout)
      call text_(stdout,"begin "//trim(prop_name)//" "//trim(to_str_(n_size)))
      call put_(stdout,prop,transpose=TRUE)
      call text_(stdout,"end "//trim(prop_name))
      call unsave_style_(stdout)
   end subroutine

   subroutine put_vertex_property_3(prop,prop_name,n_size)
   ! Put a generic MAT{REAL} property out, one point per line (transposed).
      MAT(REAL,:,:), IN :: prop
      STR(len=*), IN :: prop_name
      INT, IN :: n_size

      ! We don't want array labels or blocking
      call save_style_(stdout)
      call set_using_array_labels_(stdout,FALSE)
      call set_using_fields_(stdout,FALSE)
      call flush_(stdout)
      call text_(stdout,"begin "//trim(prop_name)//" "//trim(to_str_(n_size)))
      call put_(stdout,prop,transpose=TRUE)
      call text_(stdout,"end "//trim(prop_name))
      call unsave_style_(stdout)
   end subroutine

   subroutine put_2_vertex_properties(prop1,prop2,prop_name,n_size1,n_size2)
   ! Put a generic property out
      MAT(INT,:,:), IN :: prop1,prop2
      STR(len=*), IN :: prop_name
      INT, IN :: n_size1,n_size2

      ! We don't want to use labels on vectors
      call save_style_(stdout)
      call set_using_array_labels_(stdout,FALSE)
      call set_using_fields_(stdout,FALSE)

      ! Print out
      call flush_(stdout)
      call text_(stdout,"begin "//trim(prop_name)//" "//trim(to_str_(n_size1))//" "//trim(to_str_(n_size2)))
      call put_(stdout,prop1,transpose=TRUE)
      call put_(stdout,prop2,transpose=TRUE)
      call text_(stdout,"end "//trim(prop_name))

      ! Restore
      call unsave_style_(stdout)
   end subroutine


! Fingerprints and colours

   subroutine put_d_e_colors(self,out)
   ! Put the nearest external atom isosurface distances, as RGB
   ! colours. "out" are the indices of the atoms outside the surface.
      type(ISOSURFACE_TYPE), INOUT :: self
      VEC(INT,:), IN :: out
      VEC(REAL,:), allocatable :: dist
      MAT(REAL,:,:), allocatable :: RGB
   ENSURE(allocated(self%atom),"ISOSURFACE:put_d_e_colors ... no atom data")
   ENSURE(allocated(self%colour),"ISOSURFACE:put_d_e_colors ... no colour function")
      call create_(RGB,3,self%n_pt)
      dist = d_min_to_(self,out)
      call rescale_data_(self%colour,element_range_(dist))
      call get_RGB_for_(self%colour,dist,RGB)
      call put_vertex_property_(RGB,"d_e_colors",self%n_pt)
      call destroy_(dist)
      call destroy_(RGB)
   end subroutine

   subroutine put_d_i_colors(self,in)
   ! Put the nearest internal atom isosurface distances, as RGB colours. "in"
   ! are the indices of the atoms inside the surface.
      type(ISOSURFACE_TYPE), INOUT :: self
      VEC(INT,:), IN :: in
      VEC(REAL,:), allocatable :: dist
      MAT(REAL,:,:), allocatable :: RGB
   ENSURE(allocated(self%atom),"ISOSURFACE:put_d_i_colors ... no atom data")
   ENSURE(allocated(self%colour),"ISOSURFACE:put_d_i_colors ... no colour function")
      call create_(RGB,3,self%n_pt)
      dist = d_min_to_(self,in)
      call rescale_data_(self%colour,element_range_(dist))
      call get_RGB_for_(self%colour,dist,RGB)
      call put_vertex_property_(RGB,"d_i_colors",self%n_pt)
      call destroy_(dist)
      call destroy_(RGB)
   end subroutine

   subroutine put_binned_d_i_d_e(self,in,out)
   ! Put a binned representation of the surface, d_e vs. d_i, as RGBs.
   ! "in" and "out" are (respectively) the indices of the atoms inside
   ! and outside the surface. This version does not convert the binned
   ! vale to a colour.
      type(ISOSURFACE_TYPE), IN :: self
      VEC(INT,:), IN :: in,out
      MAT(REAL,:,:), allocatable :: binned_areas
      VEC(REAL,self%n_face) :: areas,d_i_res,d_e_res
   ENSURE(allocated(self%atom),"ISOSURFACE:put_binned_d_i_d_e ... no atom data")
      call create_(binned_areas,self%n_face,self%n_face)
      call d_e_d_i_areas_(self,in,out,d_i_res,d_e_res,areas)
      call bin_areas_(self,TOL(2),d_i_res,d_e_res,areas,binned_areas)
      call put_(stdout,d_i_res)
      call text_(stdout,"begin binned d_i and d_e data")
      call show_(stdout,"bin size  =",TOL(2))
      call show_(stdout,"d_i range =",element_range_(d_i_res))
      call show_(stdout,"d_e range =",element_range_(d_e_res))
      call text_(stdout,"begin binned areas")
      call put_(stdout,binned_areas)
      call flush_(stdout)
      call text_(stdout,"end binned areas")
      call text_(stdout,"end binned d_i and d_e data")
      call destroy_(binned_areas)
   end subroutine

   PURE subroutine bin_areas(self,bin_size,d_i_res,d_e_res,areas,area_mx)
   ! No comment? Bah! --dylan
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,self%n_face), IN :: d_i_res,d_e_res,areas
      REAL, IN :: bin_size
      MAT(REAL,:,:), allocatable, OUT :: area_mx
      REAL :: d_e_range,d_i_range,fac
      VEC(REAL,2) :: d_e_range_vec,d_i_range_vec
      VEC(REAL,2) :: inspect_row_range,inspect_col_range
      VEC(INT,:), allocatable :: area_indices,d_i_indices,d_e_indices
      INT :: area_mx_cols,area_mx_rows
      INT :: i,j
      d_e_range_vec = element_range_(d_e_res)
      d_i_range_vec = element_range_(d_i_res)
      d_e_range = d_e_range_vec(2)-d_e_range_vec(1)
      d_i_range = d_i_range_vec(2)-d_i_range_vec(1)
      area_mx_cols = ceiling(d_i_range/bin_size)
      area_mx_rows = ceiling(d_e_range/bin_size)
      call create_(area_mx,area_mx_cols,area_mx_rows)
      do i = 1,area_mx_cols
         inspect_col_range(1) = d_i_range_vec(1)+((i-1)*bin_size)
         inspect_col_range(2) = inspect_col_range(1)+bin_size
         d_i_indices = indices_in_range_(d_i_res,inspect_col_range)
         do j = 1,area_mx_rows
            inspect_row_range(1) = d_e_range_vec(1)+((i-1)*bin_size)
            inspect_row_range(2) = inspect_row_range(1)+bin_size
            d_e_indices = indices_in_range_(d_e_res,inspect_row_range)
            area_indices = elements_common_with_(d_e_indices,d_i_indices)
            area_mx(i,j) = sum(areas(area_indices))
            call destroy_(area_indices)
            call destroy_(d_e_indices)
         end do
         call destroy_(d_i_indices)
      end do
      fac = sum(area_mx)
      fac = ONE/fac
      area_mx = fac*area_mx
   end subroutine

   subroutine put_binned_d_i_d_e_RGBs(self,in,out)
   ! Put a binned representation of the surface, d_e vs. d_i, as RGBs.
   ! "in" and "out" are (respectively) the indices of the atoms inside
   ! and outside the surface.
      type(ISOSURFACE_TYPE), INOUT :: self
      VEC(INT,:), IN :: in,out
      VEC(REAL,:), allocatable :: d_e,d_i
      MAT(INT,:,:), allocatable :: bin_count
      VEC(INT,3) :: RGB255
      INT :: n_e,n_i,e,i
      STR(len=STR_SIZE) :: val
   ENSURE(allocated(self%atom),"ISOSURFACE:put_binned_d_i_d_e_RGBs ... no atom data")
   ENSURE(allocated(self%colour),"ISOSURFACE:put_binned_d_i_d_e_RGBs ... no colour function")
      d_i = d_min_to_(self,in)
      d_e = d_min_to_(self,out)

      ! Bin the counts
      call bin_XY_data_(bin_count,d_i,d_e,0.2d0)

      ! Make the color function from rangs
      call rescale_data_(self%colour,real(element_range_(bin_count),kind=REAL_KIND))

      ! Clean
      call destroy_(d_e)
      call destroy_(d_i)

      ! Print the bins
      call flush_(stdout)

      ! Banner
      n_i = size(bin_count,1)
      n_e = size(bin_count,2)
      val = trim(to_str_(n_i))//" "//trim(to_str_(n_e))
      call show_(stdout,"begin binned_d_i_d_e_colors ",trim(val),dots=FALSE)

      ! Print the bins
      do i = 1,n_i
         do e = 1,n_e
            call put_(stdout,i)
            call put_(stdout,e)
            RGB255 = RGB255_for_(self%colour,real(bin_count(i,e),kind=REAL_KIND))
            call put_(stdout,RGB255(1))
            call put_(stdout,RGB255(2))
            call put_(stdout,RGB255(3))
            call flush_(stdout)
         end do
      end do
      call text_(stdout,"end binned_d_i_d_e_colors")

      ! Clean
      call destroy_(bin_count)
   end subroutine


!   put_d_i_d_e_RGBs(in,out)
!   ! Put a binned representation of the surface, d_e vs. d_i, as RGBs.  "in" and
!   ! "out" are (respectively) the indices of the atoms inside and outside the
!   ! surface.  This differs from the put_binned_d_i_d_e_RGBs routine above in
!   ! that the actual data values are outputted with the count next to them
!   ! (converted to a RGB colour).
!      self :: IN
!      in,out :: VEC{INT}
!   ENSURE(.atom.allocated,"no atom data")
!   ENSURE(.colour.allocated,"no colour function")
!
!      d_e,d_i :: VEC{REAL}*
!      bin_count :: VEC{INT}*
!      RGB255 :: VEC{INT}(3)
!      range :: VEC{REAL}(2)
!      k :: INT
!
!      d_i => .d_min_to(in)
!      d_e => .d_min_to(out)
!
!      ! Make the bins
!      bin_count.create(.n_pt)
!      bin_count.bin_XY_data(d_i,d_e,0.2d0)
!
!      ! Make the color function from rangs
!      range = bin_count.element_range
!      .colour.rescale_data(range)
!
!      ! Print the d_e and d_i
!      stdout.flush
!      stdout.show("begin d_i_d_e_colors ",.n_pt.to_str.trim,dots=FALSE)
!      do k = 1,.n_pt
!         stdout.put(d_i(k))
!         stdout.put(d_e(k))
!         RGB255 = .colour.RGB255_for(real(bin_count(k),kind=REAL_KIND))
!         stdout.put(RGB255(1))
!         stdout.put(RGB255(2))
!         stdout.put(RGB255(3))
!         stdout.flush
!      end
!      stdout.text("end d_i_d_e_colors")
!      bin_count.destroy
!
!      ! Clean
!      d_e.destroy
!      d_i.destroy
!
!   end

   subroutine put_vrml(self,out)
   ! Put the isosurface data into VRML format to the file "out".
      type(ISOSURFACE_TYPE), INOUT :: self
      type(TEXTFILE_TYPE), INOUT :: out
      VEC(REAL,3) :: n
      MAT(REAL,:,:), allocatable :: RGB
      INT :: i
      REAL :: lb,ub
   ENSURE(self%n_pt>0,"ISOSURFACE:put_vrml ... no isosurface points")
      call text_(stdout,"Generating VRML isosurface")
      call text_(out,"Shape {")
      call text_(out,"  appearance Appearance {")
      call text_(out,"    material Material {")
      call text_(out,"      diffuseColor 0.5 0.5 0.5")
      call text_(out,"      ambientIntensity 0.5")
      call text_(out,"      emissiveColor 0.1 0.1 0.1")
      call text_(out,"    }")
      call text_(out,"  }")
      call text_(out,"  geometry IndexedFaceSet {")

      ! Output the list of vertices.
      call text_(out,"    coord Coordinate {")
      call text_(out,"      point "//achar(91))
      do i = 1,self%n_pt
         call put_(out,"      ")
         call put_(out,self%point(1,i))
         call put_(out,self%point(2,i))
         call put_(out,self%point(3,i))
         if (i==self%n_pt) then
            call put_(out," "//achar(93))
            call flush_(out)
         else
            call put_(out,",")
            call flush_(out)
         end if
      end do
      call text_(out,"    }")

      ! Output the list of vertices for each face.  Each face is ended
      ! with index -1, since in VRML we are not restricted to
      ! triangles.
      call text_(out,"    coordIndex "//achar(91))
      do i = 1,self%n_face
         call put_(out,"      ")
         call put_(out,self%face(1,i)-1)
         call put_(out,self%face(2,i)-1)
         call put_(out,self%face(3,i)-1)
         call put_(out,-1)
         call flush_(out)
      end do
      call text_(out,"    "//achar(93))
      call text_(out,"    solid FALSE")
      call text_(out,"    creaseAngle 2")

      ! Output the list of normals corresponding to the vertices.
      call text_(out,"    normal Normal {")
      call text_(out,"      vector "//achar(91))
      do i = 1,self%n_pt
         n = self%point_gradient(:,i)
         call normalize_(n)
         call put_(out,n(1))
         call put_(out,n(2))
         call put_(out,n(3))
         if (i==self%n_pt) then
            call put_(out," "//achar(93))
            call flush_(out)
         else
            call put_(out,",")
            call flush_(out)
         end if
      end do
      call text_(out,"    }")
      if (allocated(self%surface_property_values)) then

         ! Output the colour of each vertex, if applicable.
         call create_(RGB,3,self%n_pt)
         call text_(stdout,"Scaling isosurface property values for colouring...")
         if (self%chop_surface_property) then
            lb = self%surface_property_lb
            ub = self%surface_property_ub
            call show_(stdout,"Min value used is ",lb,dots=FALSE)
            call show_(stdout,"Max value used is ",ub,dots=FALSE)
            call rescale_data_(self%colour,[lb,ub])  ! INOUT
         else
            call show_(stdout,"Min value used is ",minval(self%surface_property_values),dots=FALSE)
            call show_(stdout,"Max value used is ",maxval(self%surface_property_values),dots=FALSE)
            call rescale_data_(self%colour,element_range_(self%surface_property_values))
         end if
         call get_RGB_for_(self%colour,self%surface_property_values,RGB)

         ! We should not rescale colours for many properties!!!
         call text_(out,"    colorPerVertex TRUE")
         call text_(out,"    color Color {")
         call text_(out,"      color "//achar(91))
         do i = 1,self%n_pt
            call put_(out,RGB(1,i))
            call put_(out,RGB(2,i))
            call put_(out,RGB(3,i))
            if (i==self%n_pt) then
               call put_(out," "//achar(93))
               call flush_(out)
            else
               call put_(out,",")
               call flush_(out)
            end if
         end do
         call text_(out,"    }")
         call destroy_(RGB)
      end if
      call text_(out,"  }")
      call text_(out,"}")
      call text_(stdout,"done VRML isosurface")
   end subroutine

   PURE subroutine make_surface_multipole_moments(self,qc,qs,in,out,l_max,property)
   ! Like it says ...
      type(ISOSURFACE_TYPE), IN :: self
      VEC(REAL,:), allocatable, OUT :: qc,qs
      VEC(INT,:), IN :: in,out
      INT, IN :: l_max
      STR(len=*), IN :: property
      VEC(type(ATOM_TYPE),:), allocatable :: atom
      MAT(REAL,:,:), allocatable :: iso_points
      MAT(REAL,:,:), allocatable :: face_point
      VEC(REAL,:), allocatable :: face_value
      VEC(REAL,:), allocatable :: d_e,d_i,d_norm_e,d_norm_i,d_norm,vals
      INT :: N,x,y,z
      REAL :: tmp,area
      VEC(REAL,3) :: centre,midpoint,px,py,pz
      call create_(d_i,self%n_pt)
      call create_(d_e,self%n_pt)
      call create_(d_norm_i,self%n_pt)
      call create_(d_norm_e,self%n_pt)
      call create_(d_norm,self%n_pt)
      call create_(vals,self%n_pt)
      call create_(iso_points,self%n_pt,3)
      call create_(face_point,self%n_face,3)
      call create_(face_value,self%n_face)
      atom = self%atom

      ! we need to transpose iso_points because multipole method
      ! expects a Nx3 rather than a 3xN array
      iso_points = transpose(self%point)
      call make_fingerprint_distances_(self,d_e,d_i,d_norm_e,d_norm_i,d_norm,in,out,atom,angstrom=TRUE)

      ! interpolate the centre of the sphere, c, by finding midpoint of
      ! all points
      centre(1) = sum(iso_points(:,1))/self%n_pt
      centre(2) = sum(iso_points(:,2))/self%n_pt
      centre(3) = sum(iso_points(:,3))/self%n_pt
      select case (property)
      case ("d_e"); vals = d_e
      case ("d_i"); vals = d_i
      case ("d_norm_e"); vals = d_norm_e
      case ("d_norm_i"); vals = d_norm_i
      case ("shape")
         do N = 1,self%n_pt
            x = self%face(1,N); y = self%face(2,N); z = self%face(3,N)
            px = iso_points(x,:); py = iso_points(y,:); pz = iso_points(z,:)
            midpoint = THIRD*(px+py+pz)
            midpoint = midpoint-centre
            vals(N) = sqrt(midpoint(1)**2+midpoint(2)**2+midpoint(3)**2)
         end do
      case ("ones"); vals = 1.0
      case default; vals = d_norm
      end select
      do N = 1,self%n_face

         ! x, y, z are the indices of each vertex on the triangle
         x = self%face(1,N); y = self%face(2,N); z = self%face(3,N)

         ! px, py, pz are the vertices on the triangle
         px = iso_points(x,:); py = iso_points(y,:); pz = iso_points(z,:)
         ! Normalise the points on the surface to be on a sphere of radius 1
         ! with centre at C by making a vector from the centre of the sphere
         ! to px, giving it unit length
         ! at the end px, py, pz will lie on the surface of a sphere of unit
         ! radius, centre C
         px = px-centre; call normalize_(px)
         py = py-centre; call normalize_(py)
         pz = pz-centre; call normalize_(pz)

         ! Assign the value of the point on the sphere
         midpoint = THIRD*(px+py+pz)
         call set_to_(face_point(N,:),midpoint)

         ! reuse midpoint variable to calculate area of triangle that
         ! has been projected onto the sphere
         call to_cross_product_of_(midpoint,pz-px,pz-py)
         area = 0.5*norm_(midpoint)  ! A = |V1xV2|/2

         ! face value weighted by area, and interpolated from values at
         ! vertices of triangle
         tmp = area*ONE/(THIRD*(vals(x)+vals(y)+vals(z)))
         face_value(N) = tmp
      end do

      ! Now that we have normalized points to lie on the surface of the sphere
      ! and weighted vals by the area of triangles on the surface of the sphere
      ! we can make the multipole moments of vals
      call make_R_multipoles_(qc,qs,face_value,face_point,l_max)

      ! CLEANUP
      call destroy_(d_i)
      call destroy_(d_e)
      call destroy_(iso_points)
      call destroy_(d_norm)
      call destroy_(d_norm_i)
      call destroy_(d_norm_e)
      call destroy_(face_value)
      call destroy_(face_point)
      call destroy_(vals)
   end subroutine

end module
