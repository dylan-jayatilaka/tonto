!======================================================================
!
! PLOT_GRID: 
!
! For cartesian rectilinear grid specification and generation, e.g.
! for plots or surfaces. The strategy is to update the object as soon
! as possible, rather than to wait and finalize it once, at the end,
! with a call to an .update procedure.
!
! Copyright (C) Dylan Jayatilaka, 1997
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
!======================================================================
module PLOT_GRID_MODULE

#  include "macros"

   include "plot_grid.use"

   implicit none

   include "plot_grid.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self)
   ! Allocate an object
      type(PLOT_GRID_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"PLOT_GRID:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(PLOT_GRID_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"PLOT_GRID:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine create_1(self,atom)
   ! Create object
      type(PLOT_GRID_TYPE), allocatable, OUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      call create_(self)
      call set_atom_defaults_(self,atom)
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
      type(PLOT_GRID_TYPE), INOUT :: self
      call destroy_(self%plot_kinds)
      call destroy_(self%plot_kinds_units)
      call destroy_(self%atom)
      call destroy_(self%center_atoms)
      call destroy_(self%bond_density_atom)
   end subroutine


!  ===
!  Set
!  ===

   PURE subroutine set_defaults_0(self)
   ! Set up a default grid.
      type(PLOT_GRID_TYPE), INOUT :: self
      self%plot_kind = " "
      self%orbital = 0
      self%HOMO_orbital = 0
      self%LUMO_orbital = -1
      self%projection_direction = [ZERO,ZERO,ZERO]
      self%center = ZERO
      self%offset = ZERO
      self%center_atom = 0
      self%x_atom_1 = 0
      self%x_atom_2 = 0
      self%y_atom_1 = 0
      self%y_atom_2 = 0
      self%x_axis(:) = [ONE,ZERO,ZERO]
      self%y_axis(:) = [ZERO,ONE,ZERO]
      self%z_axis(:) = [ZERO,ZERO,ONE]
      self%n_x = PLOT_GRID_NX
      self%n_y = PLOT_GRID_NY
      self%n_z = PLOT_GRID_NZ
      self%width(:) = PLOT_GRID_WIDTH
      self%width(3) = ZERO  ! assume a planar plot
      self%del_x = self%width(1)/(PLOT_GRID_NX-1)
      self%del_y = self%del_x
      self%del_z = self%del_x
      self%x_width_set = FALSE  ! These are set false only if the widths
      self%y_width_set = FALSE  ! are explicitly read in
      self%z_width_set = FALSE
      self%use_bounding_box = FALSE
      self%box_center = ZERO
      self%box_widths = PLOT_GRID_WIDTH
      self%box_scale_factor = ONE
      self%box_axes(:,1) = [ONE,ZERO,ZERO]
      self%box_axes(:,2) = [ZERO,ONE,ZERO]
      self%box_axes(:,3) = [ZERO,ZERO,ONE]
      self%use_unit_cell_as_bbox = FALSE
      self%origin_atom = FALSE
      self%x_axis_defined = FALSE
      self%y_axis_defined = FALSE
      self%z_axis_defined = FALSE
      self%desired_separation = ZERO
      self%points_filename = "no_made-up_points"
      self%plot_units = " "
      self%plot_format = " "
      self%use_plot_max = FALSE
      self%use_plot_min = FALSE
      self%plot_max = ZERO
      self%plot_min = ZERO
      self%contour_min_value = ZERO
      self%contour_max_value = ZERO
      call set_(self%contour_min_color,[253,114,121])
      call set_(self%contour_mid_color,[255,255,255])
      call set_(self%contour_max_color,[125,114,216])
      self%contour_color_scale = " "
      self%contour_scale = "log"
      self%contour_increment = ZERO
      self%contour_line_width = 1.25d0
      self%contour_fade_factor = 0.8d0
      self%contour_tic_dps = 2
      self%contour_font_size = 9
      self%use_colorbox_scale = TRUE
      self%use_ruler_scale = TRUE
   end subroutine

   PURE subroutine set_defaults_1(self,atom)
   ! Set up atom defaults. "atom" is used to define an xyz bounding
   ! box for the molecule.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      call set_defaults_(self)
      call set_atom_defaults_(self,atom)
   end subroutine

   PURE subroutine set_atom_defaults(self,atom)
   ! Set up atom defaults. "atom" is used to define an xyz bounding
   ! box for the molecule.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      self%atom = atom
      call set_bbox_with_shape_axes_(self,atom)
   end subroutine

   PURE subroutine reset_defaults(self)
   ! Reset a default grid. The only difference to set_defaults is that
   ! the bounding box and box_axes are not set again.
      type(PLOT_GRID_TYPE), INOUT :: self
      self%plot_kind = " "
      self%orbital = 0
      self%HOMO_orbital = 0
      self%LUMO_orbital = -1
      self%center = ZERO
      self%offset = ZERO
      self%center_atom = 0
      self%x_atom_1 = 0
      self%x_atom_2 = 0
      self%y_atom_1 = 0
      self%y_atom_2 = 0
      self%n_x = PLOT_GRID_NX
      self%n_y = PLOT_GRID_NY
      self%n_z = PLOT_GRID_NZ
      self%width = PLOT_GRID_WIDTH
      self%width(3) = ZERO  ! assume a planar plot
      self%del_x = self%width(1)/(PLOT_GRID_NX-1)
      self%del_y = self%del_x
      self%del_z = self%del_x
      self%x_width_set = FALSE  ! These are set TRUE only if the widths
      self%y_width_set = FALSE  ! are explicitly read in
      self%z_width_set = FALSE
      self%x_axis_defined = FALSE
      self%y_axis_defined = FALSE
      self%z_axis_defined = FALSE
      self%desired_separation = ZERO
      self%origin_atom = FALSE
      self%plot_units = " "
      self%plot_format = " "
      self%points_filename = "no_made-up_points"
   end subroutine

   PURE subroutine set_for_marching_cubes(self)
   ! Update and set the grid data to be consistent with the
   ! (non-recursive) marching cubes algorithm. Essentially, a
   ! gridpoint is added along all dimensions.
      type(PLOT_GRID_TYPE), INOUT :: self
      self%n_x = self%n_x+2
      self%n_y = self%n_y+2
      self%n_z = self%n_z+2
      ENSURE(self%n_x>=4,"PLOT_GRID:set_for_marching_cubes ... not enough x points for marching cube isosurface algorithm")
      ENSURE(self%n_y>=4,"PLOT_GRID:set_for_marching_cubes ... not enough y points for marching cube isosurface algorithm")
      ENSURE(self%n_z>=4,"PLOT_GRID:set_for_marching_cubes ... not enough z points for marching cube isosurface algorithm")

      ! center remains the same, extra points added around edges
      self%width(1) = self%width(1)+2*self%del_x
      self%width(2) = self%width(2)+2*self%del_y
      self%width(3) = self%width(3)+2*self%del_z
      self%origin = self%origin-self%del_x*self%x_axis
      self%origin = self%origin-self%del_y*self%y_axis
      self%origin = self%origin-self%del_z*self%z_axis
      self%n_pt = self%n_x*self%n_y*self%n_z
   end subroutine

   pure subroutine set_plot_label(self,label)
   ! Set the plot label.
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: label
      self%plot_label = label
   end subroutine

   PURE subroutine set_plot_kind(self,kind)
   ! Set the plot kind to be a certain "kind"
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: kind
      self%plot_kind = kind
      call to_lower_case_(self%plot_kind)
   end subroutine

   subroutine set_plot_format(self,plot_format)
   ! Set the plot kind to be a certain "kind"
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: plot_format
      self%plot_format = plot_format
      call to_lower_case_(self%plot_format)
      select case (self%plot_format)
      case ("all")
      case ("contour")
      case ("cube")
      case ("drishti")
      case ("gaussian")
      case ("gaussian.cube")
      case ("cell.cube")
      case ("gnuplot")
      case ("gnuplot.contour")
      case ("gnuplot.log_contour")
      case ("gnuplot.pos_log_contour")
      case ("mathematica")
      case ("spackman")
      case ("vapor")
      case ("vtk")
      case ("xcrysden")
      case ("xdgraph")
      case default; allocate(tonto%known_keywords(17))
      tonto%known_keywords(1) = "all"
      tonto%known_keywords(2) = "contour"
      tonto%known_keywords(3) = "cube"
      tonto%known_keywords(4) = "drishti"
      tonto%known_keywords(5) = "gaussian"
      tonto%known_keywords(6) = "gaussian.cube"
      tonto%known_keywords(7) = "cell.cube"
      tonto%known_keywords(8) = "gnuplot"
      tonto%known_keywords(9) = "gnuplot.contour"
      tonto%known_keywords(10) = "gnuplot.log_contour"
      tonto%known_keywords(11) = "gnuplot.pos_log_contour"
      tonto%known_keywords(12) = "mathematica"
      tonto%known_keywords(13) = "spackman"
      tonto%known_keywords(14) = "vapor"
      tonto%known_keywords(15) = "vtk"
      tonto%known_keywords(16) = "xcrysden"
      tonto%known_keywords(17) = "xdgraph"
      call unknown_(tonto,self%plot_format,"PLOT_GRID:set_plot_format")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   PURE subroutine set_plot_units(self,units)
   ! Set the plot "units".
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: units
      self%plot_units = units
      call to_lower_case_(self%plot_units)
      ENSURE(is_known_unit_(self%plot_units),"PLOT_GRID:set_plot_units ... unknown unit specified")
   end subroutine

   PURE subroutine set_bbox_with_current_axes(self,atom)
   ! Set a bounding box to the plot axes, with the current center,
   ! with widths based on the "atom" list. NOTE: This is activated
   ! with the use_bounding_box_and_axes= option.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,3,3) :: sa
   ENSURE(x_or_y_or_z_axes_defined_(self),"PLOT_GRID:set_bbox_with_current_axes ... you must have already defined some axes!")
      self%use_bounding_box = TRUE

      ! Make shape moments and axes; keep order
      sa = plot_axes_(self)

      ! Set box axes/center
      self%box_axes = sa
      self%box_center = self%center

      ! Set bounding box
      self%box_widths = bounding_box_widths_(atom,sa)
      self%box_widths = self%box_scale_factor*self%box_widths
      self%use_unit_cell_as_bbox = FALSE
   end subroutine

   PURE subroutine set_bbox_with_shape_axes(self,atom)
   ! Set up a bounding box based on the "atom" list and ensure that
   ! the x-y-z axis widths x-y-z go longest to shortest.
   ! NOTE: This is set with a use_bounding_box_and_axes= option.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      VEC(REAL,3) :: sm
      MAT(REAL,3,3) :: sa
      VEC(INT,3) :: order
   ENSURE(NOT x_or_y_or_z_axes_defined_(self),"PLOT_GRID:set_bbox_with_shape_axes ... you already defined some axes!")
      self%use_bounding_box = TRUE  ! <<<< Careful

      ! Make shape moments and axes; keep order
      call make_shape_moments_(atom,sm,sa)

      ! Set bounding box
      self%box_widths = bounding_box_widths_(atom,sa)
      self%box_widths = self%box_scale_factor*self%box_widths

      ! Make the x-axis the longest
      call quick_sort_(self%box_widths,order,decreasing_order=TRUE)

      ! Swap
      sm = sm(order)
      sa = sa(:,order)
      if (determinant_(sa)<ZERO) sa(:,3) = -sa(:,3)

      ! Set (swap) box axes/center 
      self%box_widths = self%box_widths(order)
      self%box_axes = sa
      self%box_center = center_of_atoms_(atom)
      self%use_unit_cell_as_bbox = FALSE  ! <<<< Careful
   end subroutine

   pure subroutine set_points_widths_origin(self)
   ! Set the number of points along the x axis to be odd. Evaluate the
   ! grid point separation .del_x from the current x_width, .width(1);
   ! from this separation, evaluate the number of points along the y
   ! and z axes; adjust the y and z widths to be an exact multiple of
   ! .del_x; finally, set the origin once the widths are known.
      type(PLOT_GRID_TYPE), INOUT :: self

      ! Make # of x axis points odd ...
      ! MUST BE IN INPUT
      self%n_x = 2*(self%n_x/2)+1

      ! Calculate del_x
      if (self%n_x>1) then
         self%del_x = self%width(1)/(self%n_x-1)
         self%del_y = self%del_x
         self%del_z = self%del_x
      else if (self%n_x==1) then
         self%width = ZERO
      end if

      ! Adjust y width to the nearest grid point
      self%n_y = nint(self%width(2)/self%del_x)
      self%n_y = 2*(self%n_y/2)+1
      self%width(2) = self%del_x*(self%n_y-1)

      ! Adjust z width to the nearest grid point
      self%n_z = nint(self%width(3)/self%del_x)
      self%n_z = 2*(self%n_z/2)+1
      self%width(3) = self%del_x*(self%n_z-1)
      self%n_pt = self%n_x*self%n_y*self%n_z
      call set_origin_(self)
   end subroutine

   PURE subroutine set_desired_separation(self,del)
   ! Set the (approximate) desired separation "del" between grid
   ! points along an axis direction.  The *actual* separation used may
   ! not be the inputted separation, but may be slightly smaller: the
   ! current .x_width is used to work out the number of points along
   ! the x_axis which is then used to work out the actual .del.  
   ! NOTE: After this routine the y and z widths are changed to be the
   ! nearest multiple of "del" (actually, ".del") that exceeds their
   ! current value.
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL, IN :: del
   ENSURE(del>0,"PLOT_GRID:set_desired_separation ... del must be positive")
      self%desired_separation = del
      self%n_x = ceiling(self%width(1)/del)
      self%n_y = ceiling(self%width(2)/del)
      self%n_z = ceiling(self%width(3)/del)
      call set_points_widths_origin_(self)
   end subroutine

   PURE subroutine set_center(self,pos)
   ! Set the .center of the grid data to be "pos".
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(REAL,3), IN :: pos
      self%center = pos
      call set_origin_(self)
   end subroutine

   PURE subroutine set_center_from_atom_list(self,atom)
   ! Set the .center of the grid data from an "atom" list. The .origin of the
   ! plot isd also set using the current plot widths and plot center.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
   ENSURE(self%center_atom/=0,"PLOT_GRID:set_center_from_atom_list ... no center_atom")
      self%center = atom(self%center_atom)%position
      call set_origin_(self)
   end subroutine

   PURE subroutine set_offset_to_center_of(self,atom)
   ! Set the .offset to the center of an "atom" list. The .origin of
   ! the plot is also set updated.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      VEC(REAL,3) :: val
      val = center_of_atoms_(atom)
      call set_offset_(self,val)
   end subroutine

   pure subroutine set_offset(self,val)
   ! Offset the .center by .offset along the current axes.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(REAL,3), IN :: val
      self%offset = val
      call set_center_from_offset_(self)
   end subroutine

   pure subroutine set_offset_cube(self,val)
   ! Offset the .center by .offset along the current axes.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(REAL,3), IN :: val
      self%offset = val
      call set_center_cube_from_offset_(self)
   end subroutine

   pure subroutine set_center_from_offset(self)
   ! Offset the .center by .offset along the current axes.
      type(PLOT_GRID_TYPE), INOUT :: self
      self%center = self%center+self%offset(1)*self%x_axis
      self%center = self%center+self%offset(2)*self%y_axis
      self%center = self%center+self%offset(3)*self%z_axis
      call set_origin_(self)
   end subroutine

   pure subroutine set_center_cube_from_offset(self)
   ! Offset the .center by .offset along the current axes.
      type(PLOT_GRID_TYPE), INOUT :: self
      self%center = self%center+self%offset(1)*self%x_axis*BOHR_PER_ANGSTROM
      self%center = self%center+self%offset(2)*self%y_axis*BOHR_PER_ANGSTROM
      self%center = self%center+self%offset(3)*self%z_axis*BOHR_PER_ANGSTROM
      call set_origin_(self)
   end subroutine

   PURE subroutine set_center_to_center_of(self,atom)
   ! Set the .center to the center of an "atom" list. The .origin of
   ! the plot is also set updated.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      self%center = center_of_atoms_(atom)
      call set_origin_(self)
   end subroutine

   pure subroutine set_origin(self)
   ! Set the origin of the plot (the bottom, front, left-hand corner).
   ! It is equal to the .center of the plot minus half the (current)
   ! .widths along the (current) x, y, and z axes.
      type(PLOT_GRID_TYPE), INOUT :: self
      if (self%origin_atom) then
         self%origin = self%center
      else
         self%origin = self%center
         self%origin = self%origin-self%width(1)*HALF*self%x_axis
         self%origin = self%origin-self%width(2)*HALF*self%y_axis
         self%origin = self%origin-self%width(3)*HALF*self%z_axis
      end if
   end subroutine

   PURE subroutine set_x_axis_from_atom_list(self,atom)
   ! Set the x-axis of the grid data from an "atom" list. If the .x_width has
   ! not been explicitly set, it is set to twice the length between the atom
   ! separation (because probably the user wants to see at least those atoms in
   ! his plot!).
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
   ENSURE(self%x_atom_1/=0,"PLOT_GRID:set_x_axis_from_atom_list ... no x_atom_1")
   ENSURE(self%x_atom_2/=0,"PLOT_GRID:set_x_axis_from_atom_list ... no x_atom_2")
      self%x_axis = atom(self%x_atom_2)%position-atom(self%x_atom_1)%position
      if (NOT self%x_width_set) then
         self%width(1) = FOUR*norm_(self%x_axis)
         if (NOT self%y_width_set) self%width(2) = self%width(1)
         call set_points_widths_origin_(self)
      end if
      call set_xyz_axes_from_x_axis_(self)
   end subroutine

   PURE subroutine set_y_axis_from_atom_list(self,atom)
   ! Set the y-axis of the grid data from an "atom" list. If the .y_width has
   ! not been explicitly set, it is set to twice the length between the atom
   ! separation (because probably the user wants to see at least those atoms in
   ! his plot!).
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
   ENSURE(self%y_atom_1/=0,"PLOT_GRID:set_y_axis_from_atom_list ... no y_atom_1")
   ENSURE(self%y_atom_2/=0,"PLOT_GRID:set_y_axis_from_atom_list ... no y_atom_2")
      self%y_axis = atom(self%y_atom_2)%position-atom(self%y_atom_1)%position
      if (NOT self%y_width_set) then
         if (self%x_axis_defined) then; self%width(2) = self%width(1)
         else; self%width(2) = FOUR*norm_(self%y_axis)
         end if
         call set_points_widths_origin_(self)
      end if
      call set_xyz_axes_from_y_axis_(self)
   end subroutine

   PURE subroutine set_z_axis_from_atom_list(self,atom)
   ! Set the z-axis of the grid data from an "atom" list. If the .z_width has
   ! not been explicitly set, it is set to twice the length between the atom
   ! separation (because probably the user wants to see at least those atoms in
   ! his plot!).
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
   ENSURE(self%z_atom_1/=0,"PLOT_GRID:set_z_axis_from_atom_list ... no z_atom_1")
   ENSURE(self%z_atom_2/=0,"PLOT_GRID:set_z_axis_from_atom_list ... no z_atom_2")
      self%z_axis = atom(self%z_atom_2)%position-atom(self%z_atom_1)%position
      if (NOT self%z_width_set) then
         if (self%x_axis_defined) then; self%width(3) = self%width(1)
         else; self%width(3) = FOUR*norm_(self%z_axis)
         end if
         call set_points_widths_origin_(self)
      end if
      call set_xyz_axes_from_z_axis_(self)
   end subroutine

   PURE subroutine set_xyz_axes_from_x_axis(self)
   ! Set the x,y,z axes of the grid given a new x_axis vector. Also set the
   ! origin since that is dependent on the axes.
      type(PLOT_GRID_TYPE), INOUT :: self
      call normalize_(self%x_axis)
      if (self%z_axis_defined) then

      ! WARN_IF(.x_axis_defined,"orthonormalizing x axis to z, but x_axis was defined!")
      ! WARN_IF(.y_axis_defined,"orthonormalizing y axis to z, but y_axis was defined!")
         call orthonormalize_x_y_to_z_axis_(self)
      else

      ! WARN_IF(.y_axis_defined,"orthonormalizing y axis to x, but y_axis was defined!")
         call orthonormalize_y_z_to_x_axis_(self)
      end if
      call set_origin_(self)
   end subroutine

   PURE subroutine set_xyz_axes_from_y_axis(self)
   ! Set the x,y,z axes of the grid given a new y_axis vector. Also set the
   ! origin since that is dependent on the axes.
      type(PLOT_GRID_TYPE), INOUT :: self
      call normalize_(self%y_axis)
      ENSURE(NOT same_as_(self%y_axis,self%x_axis),"PLOT_GRID:set_xyz_axes_from_y_axis ... y_axis is the same as x_axis")
! WARN_IF(.y_axis_defined,"orthonormalizing y axis to x, but y_axis was defined!")
! WARN_IF(.z_axis_defined,"orthonormalizing z axis to x, but z_axis was defined!")
      call orthonormalize_y_z_to_x_axis_(self)
      call set_origin_(self)
   end subroutine

   PURE subroutine set_xyz_axes_from_z_axis(self)
   ! Set the x,y,z axes of the grid given a new z_axis vector. Also set the
   ! origin since that is dependent on the axes.
      type(PLOT_GRID_TYPE), INOUT :: self
   ENSURE(NOT self%z_axis_defined,"PLOT_GRID:set_xyz_axes_from_z_axis ... z_axis already explicitly defined")
! WARN_IF(.x_or_y_axes_defined,"x/y axis to be orthonormalized to z axis")
      call normalize_(self%z_axis)
      call orthonormalize_x_y_to_z_axis_(self)
      call set_origin_(self)
   end subroutine

   PURE subroutine orthonormalize_x_y_to_z_axis(self)
   ! Orthonormalize the x and y axes to the z axis.
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL :: dot

      ! Make sure x and z are different
      if (same_as_(self%x_axis,self%z_axis)) self%x_axis = self%y_axis

      ! Orthonormalize x_axis to z_axis
      dot = dot_product(self%z_axis,self%x_axis)
      self%x_axis = self%x_axis-dot*self%z_axis
      call normalize_(self%x_axis)
      ENSURE(NOT is_zero_(self%x_axis),"PLOT_GRID:orthonormalize_x_y_to_z_axis ... x_axis is same as z_axis!")

      ! Now define y axis
      call to_cross_product_of_(self%y_axis,self%z_axis,self%x_axis)
   end subroutine

   PURE subroutine orthonormalize_y_z_to_x_axis(self)
   ! Orthonormalize the x and y axes to the z axis.
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL :: dot

      ! Make sure y and x are different
      if (same_as_(self%y_axis,self%x_axis)) self%y_axis = self%z_axis

      ! Orthogonalise y_axis to x_axis
      dot = dot_product(self%x_axis,self%y_axis)
      self%y_axis = self%y_axis-dot*self%x_axis
      call normalize_(self%y_axis)
      ENSURE(NOT is_zero_(self%y_axis),"PLOT_GRID:orthonormalize_y_z_to_x_axis ... y_axis is same as x_axis!")

      ! Now define z axis
      call to_cross_product_of_(self%z_axis,self%x_axis,self%y_axis)
   end subroutine

   PURE subroutine set_bbox_from_unit_cell(self,unit_cell,midpt_is_center)
   ! Use the "unit_cell" to set the bounding box. This bounding box
   ! must have a desired_separation to ensure even point density and
   ! sampling. The unit cell midpoint 1/2*[1,1,1] (plus any offset) is
   ! the plot center *unless* "midpt_is_center" is FALSE, in which
   ! case any user-defined box_center is used; if this is not set, it
   ! is the origin [0,0,0].
      type(PLOT_GRID_TYPE), INOUT :: self
      type(UNIT_CELL_TYPE), IN :: unit_cell
      BIN, optional, IN :: midpt_is_center

! ENSURE(.desired_separation>ZERO,"must have non-zero desired_separation")
      BIN :: mid_is_center
      self%box_axes = unit_cell%direct_mx
      self%use_unit_cell_as_bbox = TRUE
      mid_is_center = TRUE
      if (present(midpt_is_center)) mid_is_center = midpt_is_center
      if (mid_is_center) then

         ! Midpoint (offset) of unit cell
         self%center = matmul(self%box_axes,[HALF,HALF,HALF]+self%offset)
      else

         ! Default: plot box_center is [0,0,0]
         self%center = self%box_center
      end if

      ! Widths are unit cell side lenths
      self%width(1) = norm_(self%box_axes(:,1))
      self%width(2) = norm_(self%box_axes(:,2))
      self%width(3) = norm_(self%box_axes(:,3))

      ! Normalise the box axes
      self%box_axes(:,1) = self%box_axes(:,1)/self%width(1)
      self%box_axes(:,2) = self%box_axes(:,2)/self%width(2)
      self%box_axes(:,3) = self%box_axes(:,3)/self%width(3)

      ! Set plot axes
      self%x_axis = self%box_axes(:,1)
      self%y_axis = self%box_axes(:,2)
      self%z_axis = self%box_axes(:,3)

      ! Scale by requested factor
      self%width = self%box_scale_factor*self%width

      ! Set odd points
      if (self%desired_separation>ZERO) then
         self%n_x = ceiling(self%width(1)/self%desired_separation)
         self%n_y = ceiling(self%width(2)/self%desired_separation)
         self%n_z = ceiling(self%width(3)/self%desired_separation)
         ! Make # of axis points odd ...
         self%n_x = 2*(self%n_x/2)+1
         self%n_y = 2*(self%n_y/2)+1
         self%n_z = 2*(self%n_z/2)+1
      else
         self%n_x = 2*(self%n_x/2)+1
         self%n_y = 2*(self%n_y/2)+1
         self%n_z = 2*(self%n_z/2)+1
      end if
      self%n_pt = self%n_x*self%n_y*self%n_z
      self%del_x = self%width(1)/(self%n_x-1)
      self%del_y = self%width(2)/(self%n_y-1)
      self%del_z = self%width(3)/(self%n_z-1)
      call set_origin_(self)
   end subroutine

   pure subroutine set_box_from_widths(self)
   ! Forces the number of points in the three directions to be odd
   ! Calculates the separations from these.
      type(PLOT_GRID_TYPE), INOUT :: self
      self%n_pt = self%n_x*self%n_y*self%n_z
      if (self%n_x>1) then; self%del_x = self%width(1)/(self%n_x)
      else if (self%n_x==1) then; self%width = ZERO
      end if
      if (self%n_y>1) then; self%del_y = self%width(2)/(self%n_y)
      else if (self%n_y==1) then; self%width = ZERO
      end if
      if (self%n_z>1) then; self%del_z = self%width(3)/(self%n_z)
      else if (self%n_z==1) then; self%width = ZERO
      end if
      call set_origin_(self)
   end subroutine

   pure subroutine set_cube_from_widths(self)
   ! Set a plot grid which is a cube from the different widths for each
   ! axis (as specified). These means that the separations on each
   ! axis will be different. NOTE: that the axes need not be orthogonal.
      type(PLOT_GRID_TYPE), INOUT :: self
      self%n_x = 2*(self%n_x/2)+1  ! Make # of axis points odd ...
      self%n_y = self%n_x  ! Set other points from x
      self%n_z = self%n_x  ! Equal points in all directions
      self%n_pt = self%n_x*self%n_y*self%n_z
      if (self%n_x>1) then
         self%del_x = self%width(1)/(self%n_x-1)
         self%del_y = self%width(2)/(self%n_y-1)
         self%del_z = self%width(3)/(self%n_z-1)
      else if (self%n_x==1) then
         self%width = ZERO
      end if
      call set_origin_(self)
   end subroutine

   PURE subroutine set_contour_scale(self,val)
   ! Set the conour range scale mapping.
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: val
   ENSURE(is_one_of_(val,["linear      ","log         ","negative-log","positive-log"]),"PLOT_GRID:set_contour_scale ... unknown option: "//trim(val))
      self%contour_scale = val
   end subroutine

   pure subroutine set_contour_min_value(self,val)
   ! Generic set
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%contour_min_value = val
   end subroutine

   pure subroutine set_contour_max_value(self,val)
   ! Generic set
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%contour_max_value = val
   end subroutine

   pure subroutine set_contour_increment(self,val)
   ! Generic set
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%contour_increment = val
   end subroutine

   pure subroutine set_contour_line_width(self,val)
   ! Generic set
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%contour_line_width = val
   end subroutine

   pure subroutine set_contour_fade_factor(self,val)
   ! Generic set
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL, IN :: val

      ! The following code is inherited from OBJECT
      self%contour_fade_factor = val
   end subroutine

   pure subroutine set_contour_tic_dps(self,val)
   ! Generic set
      type(PLOT_GRID_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%contour_tic_dps = val
   end subroutine

   pure subroutine set_contour_font_size(self,val)
   ! Generic set
      type(PLOT_GRID_TYPE), INOUT :: self
      INT, IN :: val

      ! The following code is inherited from OBJECT
      self%contour_font_size = val
   end subroutine

   pure subroutine set_contour_color_scale(self,val)
   ! Generic set
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: val

      ! The following code is inherited from OBJECT
      self%contour_scale = val
   end subroutine

   pure subroutine set_use_colorbox_scale(self,val)
   ! Generic set
      type(PLOT_GRID_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_colorbox_scale = val
   end subroutine

   pure subroutine set_use_ruler_scale(self,val)
   ! Generic set
      type(PLOT_GRID_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%use_ruler_scale = val
   end subroutine


!  ==============
!  Bond alignment
!  ==============

   subroutine use_bond_axis_as_z_axis(self)
   ! Align the bond axis (given in .bond_density_atoms) onto the z
   ! axis. Also print the positions of the bond axis atoms B and A and
   ! the neighbouring atoms of atom A in anticlockwise direction,
   ! along with the atomic numbers and rotation angles.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: Z
      MAT(REAL,:,:), allocatable :: pos
      VEC(REAL,:), allocatable :: theta
      INT :: i

      ! Get the positions
      call get_bond_atoms_on_z_axis_(self,pos,Z,theta)

      ! Print the positions of the atoms A, B and
      ! neighbour atoms n1,n2, .... in anticlockwise order
      call text_(stdout,"Rotated positions of atoms B, A, n1, n2 ...")
      call text_(stdout,"relative to the plot origin")
      do i = 1,size(Z)
         call put_(stdout,pos(1,i))
         call put_(stdout,pos(2,i))
         call put_(stdout,pos(3,i))
         call put_(stdout,Z(i))
         call put_(stdout,theta(i))
         call flush_(stdout)
      end do

      ! Clean up
      call destroy_(theta)
      call destroy_(Z)
      call destroy_(pos)
   end subroutine

   PURE subroutine get_bond_atoms_on_z_axis(self,pos,Z,theta)
   ! Get "pos", the positions of the atoms on the bond axis, aligned
   ! so that the bond axis is on the z axis. The atom positions are in
   ! the order B, A, followed by the neighbours of atom A, in
   ! anticlockwise order. Also get the atomic numbers "Z"
   ! corresponding to the atom positions in "pos", and the
   ! corresponding rotation angles "theta" around the bond axis
   ! NOTE: the routine sets the plot width to be the AB bond length
   ! times the .box_scale_factor
      type(PLOT_GRID_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable, OUT :: pos
      VEC(INT,:), allocatable, OUT :: Z
      VEC(REAL,:), allocatable, OUT :: theta
      INT :: a,b,c,n,i
      REAL :: x,y,d_max
      VEC(INT,:), allocatable :: na,order
      VEC(REAL,3) :: v
      MAT(REAL,3,3) :: R
   ENSURE(allocated(self%bond_density_atom),"PLOT_GRID:get_bond_atoms_on_z_axis ... no bond_density_atoms")
   ENSURE(size(self%bond_density_atom)==3,"PLOT_GRID:get_bond_atoms_on_z_axis ... must have 3 bond_density_atoms")
   ENSURE(self%x_atom_1/=0 AND self%x_atom_2/=0,"PLOT_GRID:get_bond_atoms_on_z_axis ... must have x axis atoms")
   ENSURE(allocated(self%atom),"PLOT_GRID:get_bond_atoms_on_z_axis ... must have a list of atoms")

      ! Get a, b, c atoms
      a = self%bond_density_atom(1)
      b = self%bond_density_atom(2)
      c = self%bond_density_atom(3)  ! this must be either a or b

      ! If the third bond atom c is equal to b, swap
      ! a and b so that atom a has neighbours.
      if (c==b) then
         b = a
         a = c
      end if

      ! Get the neighbours of A
      na = neighbours_of_(self%atom,a)

      ! Get the plot axes. First make the z axis
      self%z_axis = self%atom(b)%position-self%atom(a)%position
      call normalize_(self%z_axis)

      ! Make the x axis orthogonal to z axis
      self%x_axis = self%atom(self%x_atom_2)%position-self%atom(self%x_atom_1)%position
      self%x_axis = self%x_axis-dot_product(self%x_axis,self%z_axis)*self%z_axis
      call normalize_(self%x_axis)

      ! Now make the y axis
      self%y_axis = cross_(self%z_axis,self%x_axis)

      ! Set the center of the plot to be atom a position
      self%center = self%atom(a)%position

      ! Get the plot with as 2 times the largest bond
      ! length to the central atom a
   ! n = na.dim
   ! d_max = ZERO
   ! do i = 1,n
   !    d_max = max(d_max,.atom.bond_distance(a,na(i)))
   ! end
   ! .width(1) = 2*d_max
   ! .width(2) = 2*d_max
   ! .width(3) = 2*d_max

      ! Make the plot size 2 times the bond length
      d_max = bond_distance_(self%atom,a,b)
      self%width(1) = self%box_scale_factor*d_max
      self%width(2) = self%box_scale_factor*d_max
      self%width(3) = self%box_scale_factor*d_max
      call set_points_widths_origin_(self)
      self%x_width_set = TRUE
      self%y_width_set = TRUE
      self%z_width_set = TRUE

      ! Remove atom B from the neighbours of A
      call prune_(na,b)

      ! Allocate space for positions, atomic numbers, angles
      n = size(na)
      call create_(pos,3,n+2)
      call create_(Z,n+2)
      call create_(theta,n)

      ! Get the cos and sin of the neighbours
      ! and sort into anticlockwise order
      call create_(order,n)
      if (n>0) then
         do i = 1,n
            v = self%atom(na(i))%position-self%atom(a)%position
            call normalize_(v)
            x = dot_product(v,self%x_axis)
            y = dot_product(v,self%y_axis)
            theta(i) = atan2(y,x)*DEGREE_PER_RADIAN
         end do
         call quick_sort_(theta,order)
         theta = theta(order)
         na = na(order)
      end if
      call destroy_(order)

      ! Make the rotation matrix which rotates the bond
      ! axis to the z axis
      R(1,:) = self%x_axis
      R(2,:) = self%y_axis
      R(3,:) = self%z_axis

      ! Get the rotated position of atom B
      v = self%atom(b)%position-self%center
      call to_product_of_(pos(:,1),R,v)
      Z(1) = self%atom(b)%atomic_number

      ! Get the rotated position of atom A
      v = self%atom(a)%position-self%center
      call to_product_of_(pos(:,2),R,v)
      Z(2) = self%atom(a)%atomic_number

      ! Get the rotated position of neighbours of atom A
      if (n>0) then
         do i = 1,n
            v = self%atom(na(i))%position-self%center
            call to_product_of_(pos(:,i+2),R,v)
            Z(i+2) = self%atom(na(i))%atomic_number
         end do
      end if

      ! Add the theta angles for the B and A atoms
      call prepend_(theta,[ZERO,ZERO])

      ! Clean up
      call destroy_(na)
   end subroutine

   subroutine align_to_bond(self)
   ! Make the rotation matrix R which aligns the bond axis given in
   ! .bond_density_atoms along the z axis
   ! WARNING: do not set n_points in the input after this routine
      type(PLOT_GRID_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: pos,pos0,pos0r,pos_keep,p
      VEC(INT,:), allocatable :: Z,Z0
      VEC(REAL,:), allocatable :: theta,theta0
      VEC(REAL,3) :: x,y
      MAT(REAL,3,3) :: R
      BIN :: found
      STR(len=STR_SIZE) :: word
      REAL :: fac_z,fac_r,r_ab,r_ab0,d2,angle,a,b,c,dist2,an,top,bot
      INT :: n,i,s

      ! Get the positions
      call get_bond_atoms_on_z_axis_(self,pos,Z,theta)

      ! Print out again the bond atom positions
      call text_(stdout,"Rotated positions of bond atoms B, A, n1, n2 ...(N')")
      do i = 1,size(Z)
         call put_(stdout,pos(1,i))
         call put_(stdout,pos(2,i))
         call put_(stdout,pos(3,i))
         call put_(stdout,Z(i))
         call put_(stdout,theta(i))
         call flush_(stdout)
      end do

      ! Get the number of atoms including A and B
      n = size(Z)
      ENSURE(n>1,"PLOT_GRID:align_to_bond ... must have at least two atoms!")

      ! Create space for reference positions, atomic numbers,
      ! and angles
      call create_(pos0,3,n)
      call create_(Z0,n)
      call create_(theta0,n)

      ! Read in the reference positions we align to
      call read_(stdin,word)
      ENSURE(word=="{","PLOT_GRID:align_to_bond ... missing open curly bracket?")
      do i = 1,n
         call read_(stdin,pos0(:,i))
         call read_(stdin,Z0(i))
         call read_(stdin,theta0(i))
      end do
      call read_(stdin,word)
      ENSURE(word=="}","PLOT_GRID:align_to_bond ... missing close curly bracket?")

      ! Print out again the reference positions
      call text_(stdout,"Rotated positions of REFERENCE bond atoms ... (N0)")
      do i = 1,size(Z0)
         call put_(stdout,pos0(1,i))
         call put_(stdout,pos0(2,i))
         call put_(stdout,pos0(3,i))
         call put_(stdout,Z0(i))
         call put_(stdout,theta0(i))
         call flush_(stdout)
      end do

      ! Find the cyclic permutation shift that causes the atomic
      ! numbers Z to match the reference ones in Z0
      found = FALSE
      do s = 0,n-3
         if (any(cshift(Z(3:),s)/=Z0(3:))) cycle
         found = TRUE
         exit
      end do
      DIE_IF(NOT found,"PLOT_GRID:align_to_bond ... a matching shift was not found!")
      call show_(stdout,"shift left/up      =",s)
      Z(3:) = cshift(Z(3:),s)
      theta(3:) = cshift(theta(3:),s)
      pos(:,3:) = cshift(pos(:,3:),s,dim=2)
      pos_keep = pos

      ! Print out again the bond atom positions
      call text_(stdout,"Cyclic shifted neighbour positions ...")
      do i = 1,size(Z)
         call put_(stdout,pos(1,i))
         call put_(stdout,pos(2,i))
         call put_(stdout,pos(3,i))
         call put_(stdout,Z(i))
         call put_(stdout,theta(i))
         call flush_(stdout)
      end do

      ! Work out at the optimum rotation angle which rotates N0 to N'
      a = sum((pos0(1,3:)*pos0(1,3:)+pos0(2,3:)*pos0(2,3:)+pos0(3,3:)*pos0(3,3:)))+sum((pos(1,3:)*pos(1,3:)+pos(2,3:)*pos(2,3:)+pos(3,3:)*pos(3,3:)))-TWO*sum(pos(3,3:)*pos0(3,3:))
      b = -TWO*sum(pos(1,3:)*pos0(1,3:))-TWO*sum_elements_(pos(2,3:)*pos0(2,3:))
      c = -TWO*sum(pos(2,3:)*pos0(1,3:))+TWO*sum_elements_(pos(1,3:)*pos0(2,3:))
      angle = atan(c/b)
      call show_(stdout,"a =",a)
      call show_(stdout,"b =",b)
      call show_(stdout,"c =",c)

      ! Does this have a negative second derivative?
      ! If not, add pi to the solution
      d2 = -b*cos(angle)-c*sin(angle)
      if (d2<ZERO) angle = angle+PI
      dist2 = a+b*cos(angle)+c*sin(angle)
      call show_(stdout,"rotation angle converting reference to this geometry (degrees) =",angle*DEGREE_PER_RADIAN)
      call show_(stdout,"distance2                                                      =",dist2)

      ! Now, rotate the x and y plot axes by the angle
      x = cos(angle)*self%x_axis+sin(angle)*self%y_axis
      y = -sin(angle)*self%x_axis+cos(angle)*self%y_axis
      self%x_axis = x
      self%y_axis = y

      ! Get the matrix which rotates reference (N0)
      ! to this position (N')
      R(:,3) = [ZERO,ZERO,ONE]
      R(3,:) = [ZERO,ZERO,ONE]
      R(1,1) = cos(angle); R(1,2) = -sin(angle)
      R(2,1) = sin(angle); R(2,2) = cos(angle)
      call text_(stdout,"Matrix which rotates reference (N0) to this position (N'):")
      call put_(stdout,R)

      ! Print out again the bond atom positions
      ! and check squared distance
      call text_(stdout,"Rotated neighbour positions (R^T N') which match scaled reference (N0)")
      p = pos
      call to_product_of_(pos,R,p,transpose_a=TRUE)
      call destroy_(p)
      dist2 = ZERO
      do i = 1,size(Z)
         call put_(stdout,pos(1,i))
         call put_(stdout,pos(2,i))
         call put_(stdout,pos(3,i))
         call put_(stdout,Z(i))
         if (i>2) then
            call put_(stdout,theta(i)-angle*DEGREE_PER_RADIAN)
            dist2 = dist2+sum_elements_((pos(:,i)-pos0(:,i))*(pos(:,i)-pos0(:,i)))
         else
            call put_(stdout,theta(i))
         end if
         call flush_(stdout)
      end do
      call show_(stdout,"distance2 (should be same as above)                            =",dist2)

      ! Get the z scale factor which scales reference N0
      ! to *this* geonmetry N'
      r_ab = pos(3,1)-pos(3,2)
      r_ab0 = pos0(3,1)-pos0(3,2)
      fac_z = r_ab/r_ab0
      call show_(stdout,"Z scale factor scaling N0 to N1 =",fac_z)

      ! Get the radial scale factor from the
      ! rotated reference atom positions
      if (n==2) then
         fac_r = ONE  ! no neighbours
      else
         top = ZERO
         bot = ZERO
         do i = 3,n
            top = top+dot_product(pos0(1:2,i),pos(1:2,i))
            bot = bot+dot_product(pos0(1:2,i),pos0(1:2,i))
         end do
         fac_r = top/bot
      end if
      call show_(stdout,"radial scale factor =",fac_r)

      ! Rotate and stretch reference positions "pos0" so they
      ! match this set of positions "pos"
      ! No need to rotate as the plot axes are rotated
      ! to simulate a rotation of this molecule
      call create_(pos0r,3,size(pos0,2))
      call to_product_of_(pos0r,R,pos0)

      ! Scale pos0
      pos0(3,:) = pos0(3,:)*fac_z
      pos0(1:2,:) = pos0(1:2,:)*fac_r

      ! Scale pos0r
      pos0r(3,:) = pos0r(3,:)*fac_z
      pos0r(1:2,:) = pos0r(1:2,:)*fac_r

      ! Now, scale the radial plot widths to match
      ! the reference positions
      self%width(1) = self%width(1)*fac_r
      self%width(2) = self%width(2)*fac_r
      self%width(3) = self%width(3)
      self%x_width_set = TRUE
      self%y_width_set = TRUE
      self%z_width_set = TRUE

      ! Make sure axes widths, etc, are OK. Checked.
      ! This grid has unequal spacings in z, y and x
      ! WARNING: do not set n_points after this routine
   ! .set_points_widths_origin
      self%n_x = 2*(self%n_x/2)+1
      if (self%n_x>1) then
         self%del_x = self%width(1)/(self%n_x-1)
         self%del_y = self%width(2)/(self%n_y-1)
         self%del_z = self%width(3)/(self%n_z-1)
      else if (self%n_x==1) then
         self%width = ZERO
      end if
      self%n_pt = self%n_x*self%n_y*self%n_z
      call set_origin_(self)

      ! Print out the scaled reference atom positions
      ! "pos0" in cube file format ...
      call text_(stdout,"Scaled, rotated reference positions (R N0) which match original positions (N')")
      call text_(stdout,"Place these in the cube file")
      call save_style_(stdout)  ! Save settings
      call set_real_fortran_fmt_(stdout,"e")  ! Gaussian format
      call set_real_width_(stdout,13)
      call set_real_precision_(stdout,5)
      dist2 = ZERO
      do i = 1,size(Z)
         call put_(stdout,Z0(i))  ! Z
         an = Z0(i)
         call put_(stdout,an,precision=1)  ! Charge
         call put_(stdout,pos0(1,i))  ! Pos in plot grid axis system
         call put_(stdout,pos0(2,i))
         call put_(stdout,pos0(3,i))
         call flush_(stdout)
         if (i>2) then
            dist2 = dist2+sum_elements_((pos0r(:,i)-pos_keep(:,i))*(pos0r(:,i)-pos_keep(:,i)))
         end if
      end do
      call unsave_style_(stdout)  ! Put back settings
      call show_(stdout,"distance2 (should be smaller than above)                       =",dist2)

      ! Clean up
      call destroy_(theta0); call destroy_(Z0); call destroy_(pos0); call destroy_(pos0r)
      call destroy_(theta); call destroy_(Z); call destroy_(pos); call destroy_(pos_keep)
   end subroutine


!  ==============
!  Atom positions
!  ==============

   PURE subroutine get_rotated_atom_positions(self,pos)
   ! Calculate the atom positions "pos" which have been shifted to the
   ! grid center and rotated to the grid axes.
      type(PLOT_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pos
      MAT(REAL,:,:), allocatable :: p
      MAT(REAL,3,3) :: axes
      VEC(REAL,3) :: limits
      INT :: a,n
   ENSURE(allocated(self%atom),"PLOT_GRID:get_rotated_atom_positions ... no atoms!")

      ! No of atoms
      n = size(self%atom)

      ! Create positions (leaky)
      call create_(pos,3,n)

      ! Shift atom positions
      do a = 1,n
         pos(:,a) = self%atom(a)%position-self%center
      end do

      ! Rotate positions into plot grid axis
      axes = transposed_plot_axes_(self)
      call create_(p,3,n)
      call to_product_of_(p,axes,pos)
      pos = p
      call destroy_(p)

      ! Adjust positions to be within plot box width
      limits = self%width*HALF
      do a = 1,n
         where (pos(:,a)>+limits) pos(:,a) = +limits
         where (pos(:,a)<-limits) pos(:,a) = -limits
      end do
   end subroutine

   PURE subroutine get_rotoranged_atom_positions(self,math_pos,z_range,valid)
   ! Calculate the positions "math_pos" of the valid atoms which have
   ! been rotated and shifted and scaled into grid point coordinates.
   ! Valid atoms are those which are within "z_range" of the plot
   ! plane; the indices of the valid atoms are stored in "valid"
      type(PLOT_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: math_pos
      REAL, IN :: z_range
      VEC(INT,:), allocatable, OUT :: valid
      VEC(REAL,3) :: scale_factor,p
      MAT(REAL,3,3) :: tr
      MAT(REAL,:,:), allocatable :: pos
      INT :: n,nv,a,i
   ENSURE(allocated(self%atom),"PLOT_GRID:get_rotoranged_atom_positions ... no atoms!")
   ENSURE(z_range>ZERO,"PLOT_GRID:get_rotoranged_atom_positions ... z_range must be non-negative")
   ENSURE(NOT allocated(math_pos),"PLOT_GRID:get_rotoranged_atom_positions ... math_pos, already allocated")
   ENSURE(NOT allocated(valid),"PLOT_GRID:get_rotoranged_atom_positions ... valid, already allocated")

      ! Get shifted atom positions
      n = size(self%atom)
      call create_(pos,n,3)
      do a = 1,n
         pos(a,:) = self%atom(a)%position-self%origin  !shift
      end do

      ! Rotate positions into plot grid axis
      tr = transposed_plot_axes_(self)
      do a = 1,n
         p = pos(a,:)
         call to_product_of_(pos(a,:),tr,p)
      end do

      ! Reject atoms far from plot plane, get valid atoms
      call create_(valid,0)
      do a = 1,n

         ! if ( abs(pos(a,z)) > z_range) cycle
         if (abs(pos(a,3))>z_range) cycle
         call append_(valid,a)
      end do

      ! Create list of valid atoms positions
      nv = size(valid)
      call create_(math_pos,nv,3)
      i = 0
      do a = 1,n
         if (abs(pos(a,3))>z_range) cycle
         i = i+1
         math_pos(i,:) = pos(a,:)
      end do

      ! Finally, scale positions
      scale_factor(1) = (self%n_x-ONE)/self%width(1)
      scale_factor(2) = (self%n_y-ONE)/self%width(2)
      scale_factor(3) = ZERO
      do a = 1,nv
         math_pos(a,:) = scale_factor*math_pos(a,:)+ONE  ! since plot begins at 1
      end do
      call destroy_(pos)
   end subroutine


!  =======
!  Inquiry
!  =======

   pure function atom_data_used(self) result (res)
   ! Return TRUE if atom-based data was used to specify plot axes.
      type(PLOT_GRID_TYPE), IN :: self
      BIN :: res
      res = allocated(self%center_atoms) OR self%x_atom_1/=0 OR self%y_atom_1/=0
   end function

   pure function x_or_y_axes_defined(self) result (res)
   ! Return TRUE if the x or y axes have been explicitly inputted.
      type(PLOT_GRID_TYPE), IN :: self
      BIN :: res
      res = self%x_axis_defined OR self%y_axis_defined
   end function

   pure function x_or_y_or_z_axes_defined(self) result (res)
   ! Return TRUE if any of the x or y or z axes have been explicitly
   ! inputted.
      type(PLOT_GRID_TYPE), IN :: self
      BIN :: res
      res = self%x_axis_defined OR self%y_axis_defined OR self%z_axis_defined
   end function

   pure function has_orthogonal_plot_axes(self) result (res)
   ! Return TRUE if the plot axes are orthogonal
      type(PLOT_GRID_TYPE), IN :: self
      BIN :: res
      REAL :: dot
      dot = dot_product(self%x_axis,self%y_axis)
      res = is_zero_(dot)
      if (NOT res) return
      dot = dot_product(self%x_axis,self%z_axis)
      res = is_zero_(dot)
      if (NOT res) return
      dot = dot_product(self%y_axis,self%z_axis)
      res = is_zero_(dot)
   end function

   pure function has_equal_spacing(self) result (res)
   ! Return TRUE if the plot spacings are all equal
      type(PLOT_GRID_TYPE), IN :: self
      BIN :: res
      res = equals_(self%del_x,self%del_y)
      if (NOT res) return
      res = equals_(self%del_x,self%del_z)
      if (NOT res) return
      res = equals_(self%del_y,self%del_z)
   end function

   pure function plot_axes(self) result (res)
   ! Return the plot axes as a matrix.
      type(PLOT_GRID_TYPE), IN :: self
      MAT(REAL,3,3) :: res
      res(:,1) = self%x_axis
      res(:,2) = self%y_axis
      res(:,3) = self%z_axis
   end function

   PURE function volume(self) result (res)
   ! Return the volume of the plot box.
   ! PURE (the macro), not `pure` (the keyword): it calls .determinant, which
   ! is itself PURE, and PURE expands to nothing in a debug build. A lower-case
   ! `pure` caller keeps its purity while its callee loses it, which is a
   ! compile error in every debug build -- see CLAUDE.md on PURE vs pure.
      type(PLOT_GRID_TYPE), IN :: self
      REAL :: res
      MAT(REAL,3,3) :: axes
      axes = plot_axes_(self)
      res = abs(determinant_(axes))*product(self%width)
   end function

   PURE function pixel_volume(self) result (res)
   ! Return the volume of one voxel of the plot grid.
      type(PLOT_GRID_TYPE), IN :: self
      REAL :: res
      INT :: n
      n = (self%n_x-1)*(self%n_y-1)*(self%n_z-1)
      res = volume_(self)/n
   end function

   pure function transposed_plot_axes(self) result (res)
   ! Return the transposed plot axis matrix.
      type(PLOT_GRID_TYPE), IN :: self
      MAT(REAL,3,3) :: res
      res(1,:) = self%x_axis
      res(2,:) = self%y_axis
      res(3,:) = self%z_axis
   end function

   PURE function plot_file_name_0(self,n_a) result (res)
   ! Return the name of the file in which the plot values are stored.
   ! "n_a" is the number of alpha electrons used to obtain the actual
   ! orbital index as an offset from the HOMO/LUMO index.
      type(PLOT_GRID_TYPE), IN :: self
      INT, optional, IN :: n_a
      STR(len=STR_SIZE) :: res
      res = plot_file_name_(self,self%plot_kind,n_a)
   end function

   PURE function plot_file_name_1(self,plot_kind,n_a) result (res)
   ! Return the name of the file corresponding to "plot_kind" in which
   ! the plot values are stored.  "n_a" is the number of alpha
   ! electrons used to obtain the actual orbital index as an offset
   ! from the HOMO/LUMO index.
      type(PLOT_GRID_TYPE), IN :: self
      STR(len=*), IN :: plot_kind
      INT, optional, IN :: n_a
      STR(len=STR_SIZE) :: res
      INT :: orb
      res = plot_kind
      res = trim_from_end_(res,"_grid")

      ! Fix up special file names
      select case (res)

      case ("orbital_density                     ")

         ! Sanity test!
         ENSURE(present(n_a),"PLOT_GRID:plot_file_name_1 ... must supply n_a argument!")

         ! Get the orbital index
         orb = self%orbital
         if (orb==0) then
            if (self%HOMO_orbital/=0) then; orb = n_a+self%HOMO_orbital
            else if (self%LUMO_orbital/=-1) then; orb = n_a+self%LUMO_orbital  ! -1 Error?
            else; orb = n_a
            end if
            ENSURE(orb>0,"PLOT_GRID:plot_file_name_1 ... non positive orbital")
         end if
         res = "orbital_"//trim(to_str_(orb))//"_density"
      end select

      ! Add _grid to the end
      res = trim(res)//trim("_grid")
   end function


!  =====
!  Input
!  =====

   subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","PLOT_GRID:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"PLOT_GRID:read_keywords ... unexpected end of file while reading list keywords")
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
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                         ")  ! exit case
      case ("align_to_bond=            "); call align_to_bond_(self)
      case ("bond_density_atoms=       "); call read_bond_density_atoms_(self)
      case ("box_scale_factor=         "); call read_box_scale_factor_(self)
      case ("center=                   "); call read_center_(self)
      case ("centre=                   "); call read_center_(self)
      case ("center_atom=              "); call read_center_atom_(self)
      case ("center_atoms=             "); call read_center_atoms_(self)
      case ("centre_atom=              "); call read_center_atom_(self)
      case ("centre_atoms=             "); call read_center_atoms_(self)
      case ("contour_color_scale=      "); call read_contour_color_scale_(self)
      case ("contour_tic_dps=          "); call read_contour_tic_dps_(self)
      case ("contour_fade_factor=      "); call read_contour_fade_factor_(self)
      case ("contour_font_size=        "); call read_contour_font_size_(self)
      case ("contour_increment=        "); call read_contour_increment_(self)
      case ("contour_line_width=       "); call read_contour_line_width_(self)
      case ("contour_max_color=        "); call read_contour_max_color_(self)
      case ("contour_max_value=        "); call read_contour_max_value_(self)
      case ("contour_mid_color=        "); call read_contour_mid_color_(self)
      case ("contour_min_color=        "); call read_contour_min_color_(self)
      case ("contour_min_value=        "); call read_contour_min_value_(self)
      case ("contour_scale=            "); call read_contour_scale_(self)
      case ("crystal_plane=            "); call read_z_axis_(self)
      case ("cube_scale_factor=        "); call read_cube_scale_factor_(self)
      case ("desired_separation=       "); call read_desired_separation_(self)
      case ("group=                    "); call read_group_(self)
      case ("electric_potential_method="); call read_electric_potential_method_(self)
      case ("homo_orbital_plus=        "); call read_HOMO_orbital_plus_(self)
      case ("homo_orbital_minus=       "); call read_HOMO_orbital_minus_(self)
      case ("kind=                     "); call read_plot_kind_(self)
      case ("kinds=                    "); call read_plot_kinds_(self)
      case ("lumo_orbital_plus=        "); call read_LUMO_orbital_plus_(self)
      case ("max_x_points=             "); call read_max_x_points_(self)
      case ("min_x_points=             "); call read_min_x_points_(self)
      case ("n_all_points=             "); call read_all_points_(self)
      case ("n_points=                 "); call read_x_points_(self)
      case ("n_x=                      "); call read_n_x_(self)
      case ("n_y=                      "); call read_n_y_(self)
      case ("n_z=                      "); call read_n_z_(self)
      case ("normal=                   "); call read_z_axis_(self)
      case ("offset=                   "); call read_offset_(self)
      case ("offset_cube=              "); call read_offset_cube_(self)
      case ("orbital=                  "); call read_orbital_(self)
      case ("origin_atom=              "); call read_origin_atom_(self)
      case ("plot_format=              "); call read_plot_format_(self)
      case ("plot_kind=                "); call read_plot_kind_(self)
      case ("plot_kinds=               "); call read_plot_kinds_(self)
      case ("plot_kinds_units=         "); call read_plot_kinds_units_(self)
      case ("plot_label=               "); call read_plot_label_(self)
      case ("plot_max=                 "); call read_plot_max_(self)
      case ("plot_min=                 "); call read_plot_min_(self)
      case ("plot_units=               "); call read_plot_units_(self)
      case ("projection_direction=     "); call read_projection_direction_(self)
      case ("put                       "); call put_(self)
      case ("units=                    "); call read_plot_units_(self)
      case ("use_bbox                  "); call use_bbox_with_current_axes_(self)
      case ("use_bbox_with_shape_axes  "); call use_bbox_with_shape_axes_(self)
      case ("use_bcube                 "); call use_bcube_with_current_axes_(self)
      case ("use_bcube_with_shape_axes "); call use_bcube_with_shape_axes_(self)
      case ("use_bond_axis_as_z_axis   "); call use_bond_axis_as_z_axis_(self)
      case ("use_bounding_box          "); call use_bbox_with_current_axes_(self)
      case ("use_bounding_box_and_axes "); call use_bbox_with_shape_axes_(self)
      case ("use_bounding_cube         "); call use_bcube_with_current_axes_(self)
      case ("use_bounding_cube_and_axes"); call use_bcube_with_shape_axes_(self)
      case ("use_colorbox_scale=       "); call read_use_colorbox_scale_(self)
      case ("use_plot_max=             "); call read_use_plot_max_(self)
      case ("use_plot_min=             "); call read_use_plot_min_(self)
      case ("use_ruler_scale=          "); call read_use_ruler_scale_(self)
      case ("use_unit_cell_as_bbox     "); self%use_unit_cell_as_bbox = TRUE
      case ("x_axis=                   "); call read_x_axis_(self)
      case ("x_axis_atoms=             "); call read_x_axis_atoms_(self)
      case ("x_axis_atom_list=         "); call read_x_axis_atom_list_(self)
      case ("x_points=                 "); call read_x_points_(self)
      case ("x_width=                  "); call read_x_width_(self)
      case ("y_axis=                   "); call read_y_axis_(self)
      case ("y_axis_atoms=             "); call read_y_axis_atoms_(self)
      case ("y_axis_atom_list=         "); call read_y_axis_atom_list_(self)
      case ("y_width=                  "); call read_y_width_(self)
      case ("z_axis=                   "); call read_z_axis_(self)
      case ("z_width=                  "); call read_z_width_(self)
      case ("3d_points_filename=       "); call read_points_filename_(self)
      case default; allocate(tonto%known_keywords(81))
      tonto%known_keywords(1) = "}                         "
      tonto%known_keywords(2) = "align_to_bond=            "
      tonto%known_keywords(3) = "bond_density_atoms=       "
      tonto%known_keywords(4) = "box_scale_factor=         "
      tonto%known_keywords(5) = "center=                   "
      tonto%known_keywords(6) = "centre=                   "
      tonto%known_keywords(7) = "center_atom=              "
      tonto%known_keywords(8) = "center_atoms=             "
      tonto%known_keywords(9) = "centre_atom=              "
      tonto%known_keywords(10) = "centre_atoms=             "
      tonto%known_keywords(11) = "contour_color_scale=      "
      tonto%known_keywords(12) = "contour_tic_dps=          "
      tonto%known_keywords(13) = "contour_fade_factor=      "
      tonto%known_keywords(14) = "contour_font_size=        "
      tonto%known_keywords(15) = "contour_increment=        "
      tonto%known_keywords(16) = "contour_line_width=       "
      tonto%known_keywords(17) = "contour_max_color=        "
      tonto%known_keywords(18) = "contour_max_value=        "
      tonto%known_keywords(19) = "contour_mid_color=        "
      tonto%known_keywords(20) = "contour_min_color=        "
      tonto%known_keywords(21) = "contour_min_value=        "
      tonto%known_keywords(22) = "contour_scale=            "
      tonto%known_keywords(23) = "crystal_plane=            "
      tonto%known_keywords(24) = "cube_scale_factor=        "
      tonto%known_keywords(25) = "desired_separation=       "
      tonto%known_keywords(26) = "group=                    "
      tonto%known_keywords(27) = "electric_potential_method="
      tonto%known_keywords(28) = "homo_orbital_plus=        "
      tonto%known_keywords(29) = "homo_orbital_minus=       "
      tonto%known_keywords(30) = "kind=                     "
      tonto%known_keywords(31) = "kinds=                    "
      tonto%known_keywords(32) = "lumo_orbital_plus=        "
      tonto%known_keywords(33) = "max_x_points=             "
      tonto%known_keywords(34) = "min_x_points=             "
      tonto%known_keywords(35) = "n_all_points=             "
      tonto%known_keywords(36) = "n_points=                 "
      tonto%known_keywords(37) = "n_x=                      "
      tonto%known_keywords(38) = "n_y=                      "
      tonto%known_keywords(39) = "n_z=                      "
      tonto%known_keywords(40) = "normal=                   "
      tonto%known_keywords(41) = "offset=                   "
      tonto%known_keywords(42) = "offset_cube=              "
      tonto%known_keywords(43) = "orbital=                  "
      tonto%known_keywords(44) = "origin_atom=              "
      tonto%known_keywords(45) = "plot_format=              "
      tonto%known_keywords(46) = "plot_kind=                "
      tonto%known_keywords(47) = "plot_kinds=               "
      tonto%known_keywords(48) = "plot_kinds_units=         "
      tonto%known_keywords(49) = "plot_label=               "
      tonto%known_keywords(50) = "plot_max=                 "
      tonto%known_keywords(51) = "plot_min=                 "
      tonto%known_keywords(52) = "plot_units=               "
      tonto%known_keywords(53) = "projection_direction=     "
      tonto%known_keywords(54) = "put                       "
      tonto%known_keywords(55) = "units=                    "
      tonto%known_keywords(56) = "use_bbox                  "
      tonto%known_keywords(57) = "use_bbox_with_shape_axes  "
      tonto%known_keywords(58) = "use_bcube                 "
      tonto%known_keywords(59) = "use_bcube_with_shape_axes "
      tonto%known_keywords(60) = "use_bond_axis_as_z_axis   "
      tonto%known_keywords(61) = "use_bounding_box          "
      tonto%known_keywords(62) = "use_bounding_box_and_axes "
      tonto%known_keywords(63) = "use_bounding_cube         "
      tonto%known_keywords(64) = "use_bounding_cube_and_axes"
      tonto%known_keywords(65) = "use_colorbox_scale=       "
      tonto%known_keywords(66) = "use_plot_max=             "
      tonto%known_keywords(67) = "use_plot_min=             "
      tonto%known_keywords(68) = "use_ruler_scale=          "
      tonto%known_keywords(69) = "use_unit_cell_as_bbox     "
      tonto%known_keywords(70) = "x_axis=                   "
      tonto%known_keywords(71) = "x_axis_atoms=             "
      tonto%known_keywords(72) = "x_axis_atom_list=         "
      tonto%known_keywords(73) = "x_points=                 "
      tonto%known_keywords(74) = "x_width=                  "
      tonto%known_keywords(75) = "y_axis=                   "
      tonto%known_keywords(76) = "y_axis_atoms=             "
      tonto%known_keywords(77) = "y_axis_atom_list=         "
      tonto%known_keywords(78) = "y_width=                  "
      tonto%known_keywords(79) = "z_axis=                   "
      tonto%known_keywords(80) = "z_width=                  "
      tonto%known_keywords(81) = "3d_points_filename=       "
      call unknown_(tonto,word,"PLOT_GRID:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_plot_units(self)
   ! Read the units to which the plot should be converted to.
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: units
      call read_(stdin,units)
      call set_plot_units_(self,units)
   end subroutine

   subroutine read_plot_kinds_units(self)
   ! Read a string vector which describes the units to be used
   ! for each of the plot_kinds_units
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_all_(stdin,self%plot_kinds_units)
      call to_lower_case_(self%plot_kinds_units)
   end subroutine

   subroutine read_plot_format(self)
   ! Read the units to which the plot should be converted to.
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: plot_format
      call read_(stdin,plot_format)
      call set_plot_format_(self,plot_format)
   end subroutine

   subroutine read_plot_max(self)
   ! Read the plot maximum to use
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_quantity_(stdin,self%plot_max)
      self%use_plot_max = TRUE
   end subroutine

   subroutine read_plot_min(self)
   ! Read the plot minimum to use
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_quantity_(stdin,self%plot_min)
      self%use_plot_min = TRUE
   end subroutine

   subroutine read_use_plot_max(self)
   ! Read whether to use the plot maximum
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%use_plot_max)
   end subroutine

   subroutine read_use_plot_min(self)
   ! Read whether to use the plot minimum
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%use_plot_min)
   end subroutine

   subroutine read_plot_kind(self)
   ! Read in the plot kind
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: kind
      call read_(stdin,kind)
      call set_plot_kind_(self,kind)
   end subroutine

   subroutine read_plot_kinds(self)
   ! Read in the plot kind
      type(PLOT_GRID_TYPE), INOUT :: self
      INT :: n
      call read_all_(stdin,self%plot_kinds)
      call to_lower_case_(self%plot_kinds)
      n = size(self%plot_kinds)
      call create_(self%plot_kinds_units,n)
      self%plot_kinds_units = " "
   end subroutine

   subroutine read_plot_label(self)
   ! Read in the plot label
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: label
      call read_(stdin,label)
      call set_plot_label_(self,label)
   end subroutine

   subroutine read_desired_separation(self)
   ! Read the (approximate) desired separation between grid points along an axis
   ! direction.  The number of x points for the plot is adjusted so that the
   ! *actual* separation is less than the inputted separation. NOTE: the current
   ! .x_width is used to calculate the number of points.
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL :: del
   ENSURE(self%n_x>1,"PLOT_GRID:read_desired_separation ... # of x_points must be greater than 1")
   WARN_IF(NOT self%x_width_set,"PLOT_GRID:read_desired_separation ... default x_width used to calculate # of x_points")
      call read_quantity_(stdin,del)
      call set_desired_separation_(self,del)
   end subroutine

   subroutine read_group(self)
   ! Read the molecular group to plot in a fragment_plot.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%group)
   end subroutine

   subroutine read_electric_potential_method(self)
   ! Read a string representing which method to use when calculating
   ! electric potential on the generated isosurface
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%electric_potential_method)
   end subroutine

   subroutine read_max_x_points(self)
   ! Read the *maximum number* of x points to be used for the plot.
   ! The number of points is adjusted to be no more than this.
      type(PLOT_GRID_TYPE), INOUT :: self
      INT :: max_n_x
      call read_(stdin,max_n_x)
      if (self%n_x<=max_n_x) return
      self%n_x = max_n_x
      WARN_IF(is_even_(self%n_x),"PLOT_GRID:read_max_x_points ... resetting to nearest odd number")
      call set_points_widths_origin_(self)
   end subroutine

   subroutine read_min_x_points(self)
   ! Read the *minimum number* of x points to be used for the plot.
      type(PLOT_GRID_TYPE), INOUT :: self
      INT :: min_n_x
      call read_(stdin,min_n_x)
      if (min_n_x<=self%n_x) return
      self%n_x = min_n_x
      WARN_IF(is_even_(self%n_x),"PLOT_GRID:read_min_x_points ... resetting to nearest odd number")
      call set_points_widths_origin_(self)
   end subroutine

   subroutine read_all_points(self)
   ! Read the number of x y and z points for the plot. This is the precision
   ! of the plot.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(INT,3) :: grid_points
      call read_(stdin,grid_points)
      self%n_x = grid_points(1)
      self%n_y = grid_points(2)
      self%n_z = grid_points(3)
      ENSURE(self%n_x>0,"PLOT_GRID:read_all_points ... number of x_points must be positive")
      ENSURE(self%n_y>0,"PLOT_GRID:read_all_points ... number of y_points must be positive")
      ENSURE(self%n_z>0,"PLOT_GRID:read_all_points ... number of z_points must be positive")
   end subroutine

   subroutine read_x_points(self)
   ! Read the number of x points for the plot. This is the precision
   ! of the plot.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%n_x)
      ENSURE(self%n_x>0,"PLOT_GRID:read_x_points ... number of x_points must be positive")
      WARN_IF(is_even_(self%n_x),"PLOT_GRID:read_x_points ... resetting to nearest odd number")
      call set_points_widths_origin_(self)
   end subroutine

   subroutine read_n_x(self)
   ! Read the value of n_x and nothing else.
   ! WARNING: use only for unit_cell as bbox.
   ! of the plot.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%n_x)
      ENSURE(self%n_x>0,"PLOT_GRID:read_n_x ... n_x must be positive")
   end subroutine

   subroutine read_n_y(self)
   ! Read the value of n_y and nothing else.
   ! WARNING: use only for unit_cell as bbox.
   ! of the plot.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%n_y)
      ENSURE(self%n_y>0,"PLOT_GRID:read_n_y ... number n_y must be positive")
   end subroutine

   subroutine read_n_z(self)
   ! Read the value of n_z and nothing else.
   ! WARNING: use only for unit_cell as bbox.  of the plot.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%n_z)
      ENSURE(self%n_z>0,"PLOT_GRID:read_n_z ... number n_z must be positive")
   end subroutine

   subroutine read_offset(self)
   ! Read a 3-vector, used to offset the plot along.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(REAL,3) :: val
      call read_quantity_(stdin,val)
      call set_offset_(self,val)
   end subroutine

   subroutine read_offset_cube(self)
   ! Read a 3-vector, used to offset the plot along.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(REAL,3) :: val
      call read_quantity_(stdin,val)
      call set_offset_cube_(self,val)
   end subroutine

   subroutine read_orbital(self)
   ! Read the orbital to plot out
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%orbital)
   end subroutine

   subroutine read_origin_atom(self)
   ! Read whether the centre atom should be the origin of the plot
   ! default FALSE
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%origin_atom)
   end subroutine

   subroutine read_HOMO_orbital_plus(self)
   ! Read the HOMO orbital to plot out. This is actually the index "n", where
   ! the orbital plotted out is "HOMO+n". The default value for n is 0, i.e. the
   ! HOMO is plotted out.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%HOMO_orbital)
      ENSURE(self%HOMO_orbital<=0,"PLOT_GRID:read_HOMO_orbital_plus ... HOMO orbital must be less than or equal to zero")
   end subroutine

   subroutine read_HOMO_orbital_minus(self)
   ! Read the minus HOMO orbital to plot out. This actually the index "n" where
   ! the orbital to be plotted out is "HOMO-n". The default value for n is 0,
   ! i.e. the HOMO is plotted out.
      type(PLOT_GRID_TYPE), INOUT :: self
      INT :: orb
      call read_(stdin,orb)
      self%HOMO_orbital = -orb
      ENSURE(self%HOMO_orbital<=0,"PLOT_GRID:read_HOMO_orbital_minus ... HOMO orbital must be less than or equal to zero")
   end subroutine

   subroutine read_LUMO_orbital_plus(self)
   ! Read the LUMO orbital to plot out. This is actually the index "n" where the
   ! orbital to be plotted out is "LUMO+n". The default value of n is -1, and
   ! the HOMO is plotted out by default; if thgis routine is called then the
   ! value must be non-zero and we know that the LUMO value was set.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%LUMO_orbital)
      ENSURE(self%LUMO_orbital>=0,"PLOT_GRID:read_LUMO_orbital_plus ... LUMO orbital must be greater than or equal to zero")
      self%HOMO_orbital = self%LUMO_orbital+1
   end subroutine

   subroutine read_center_atom(self)
   ! Read the center atom of the plot
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%center_atom)
      call set_center_from_atom_list_(self,self%atom)
   end subroutine

   subroutine read_center_atoms(self)
   ! Make the center of the plot to be the center of the list of atom
   ! indices.
      type(PLOT_GRID_TYPE), INOUT :: self
      INT :: i,n
   ENSURE(allocated(self%atom),"PLOT_GRID:read_center_atoms ... atom list not present")

      ! Read the center atoms list (leaky)
      call destroy_(self%center_atoms)
      call read_all_(stdin,self%center_atoms)
      ENSURE(size(self%center_atoms)>0,"PLOT_GRID:read_center_atoms ... no atoms specified in input")

      ! Get the center
      self%center = ZERO
      n = size(self%center_atoms)
      do i = 1,n
         self%center = self%center+self%atom(self%center_atoms(i))%position
      end do
      self%center = self%center/REALIFY(n)

      ! Set the origin
      call set_origin_(self)
   end subroutine

   subroutine read_center(self)
   ! Read the center position of the plot. Also adjust the origin of
   ! the plot.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(REAL,3) :: pos
      call read_quantity_(stdin,pos)
      call set_center_(self,pos)
   end subroutine

   subroutine read_x_axis(self)
   ! Read the x axis vector of the plot. Normally, the y and z axes
   ! are defined to be orthogonal to the inputted axes, based on the
   ! defualt axis settings.  However, if the z axis has been
   ! explicitly defined before hand, then the x and y axes are
   ! orthogonalised to it.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%x_axis)
      self%x_axis_defined = TRUE
      call set_xyz_axes_from_x_axis_(self)
   end subroutine

   subroutine read_x_axis_atoms(self)
   ! Read the atoms which define the x axis vector of the plot. If the
   ! z axis has been defined, then the x_axis is orthogonalised to it.
   ! Otherwise the y and z axes are orthogonalised to this vector.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%x_atom_1)
      call read_(stdin,self%x_atom_2)
      DIE_IF(self%x_atom_1==self%x_atom_2,"PLOT_GRID:read_x_axis_atoms ... cannot specify same x axis atoms")
      self%x_axis_defined = TRUE
      call set_x_axis_from_atom_list_(self,self%atom)
   end subroutine

   subroutine read_x_axis_atom_list(self)
   ! Make the x-axis along the mean of two lists of atoms.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: atom_list
      VEC(REAL,3) :: pos1,pos2
      INT :: i
   ENSURE(allocated(self%atom),"PLOT_GRID:read_x_axis_atom_list ... atom list not present")
      call read_all_(stdin,atom_list)
      ENSURE(size(atom_list)>0,"PLOT_GRID:read_x_axis_atom_list ... no atoms specified in input")
      pos1 = ZERO
      do i = 1,size(atom_list)
         pos1 = pos1+self%atom(atom_list(i))%position
      end do
      pos1 = pos1/size(atom_list)
      call destroy_(atom_list)
      call read_all_(stdin,atom_list)
      ENSURE(size(atom_list)>0,"PLOT_GRID:read_x_axis_atom_list ... no atoms specified in input")
      pos2 = ZERO
      do i = 1,size(atom_list)
         pos2 = pos2+self%atom(atom_list(i))%position
      end do
      pos2 = pos2/size(atom_list)
      call destroy_(atom_list)
      self%x_axis = pos2-pos1
      self%x_axis_defined = TRUE
      if (NOT self%x_width_set) then
         self%width(1) = FOUR*norm_(self%x_axis)
         call set_points_widths_origin_(self)
      end if
      call set_xyz_axes_from_x_axis_(self)
   end subroutine

   subroutine read_y_axis(self)
   ! Read the y axis vector of the plot. It is an error to use this
   ! command if the .z_axis has already been inputted, because the
   ! .y_axis is made orthogonal to the x_axis and z_axis. Even if the
   ! z_axis has not been defined, the y_axis is made orthogonal to the
   ! x_axis.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%y_axis)
      self%y_axis_defined = TRUE
      call set_xyz_axes_from_y_axis_(self)
   end subroutine

   subroutine read_y_axis_atoms(self)
   ! Read the atoms which define the y axis vector of the plot. It is an error
   ! to use this command if the .z_axis has already been inputted, because the
   ! .y_axis is made orthogonal to the x_axis and z_axis. Even if the z_axis has
   ! not been
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%y_atom_1)
      call read_(stdin,self%y_atom_2)
      DIE_IF(self%y_atom_1==self%y_atom_2,"PLOT_GRID:read_y_axis_atoms ... cannot specify same y axis atoms")
      self%y_axis_defined = TRUE
      call set_y_axis_from_atom_list_(self,self%atom)
   end subroutine

   subroutine read_y_axis_atom_list(self)
   ! Make the y-axis along the mean of two lists of atoms.
      type(PLOT_GRID_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: atom_list
      VEC(REAL,3) :: pos1,pos2
      INT :: i
   ENSURE(allocated(self%atom),"PLOT_GRID:read_y_axis_atom_list ... atom list not present")
      call read_all_(stdin,atom_list)
      ENSURE(size(atom_list)>0,"PLOT_GRID:read_y_axis_atom_list ... no atoms specified in input")
      pos1 = ZERO
      do i = 1,size(atom_list)
         pos1 = pos1+self%atom(atom_list(i))%position
      end do
      pos1 = pos1/size(atom_list)
      call destroy_(atom_list)
      call read_all_(stdin,atom_list)
      ENSURE(size(atom_list)>0,"PLOT_GRID:read_y_axis_atom_list ... no atoms specified in input")
      pos2 = ZERO
      do i = 1,size(atom_list)
         pos2 = pos2+self%atom(atom_list(i))%position
      end do
      pos2 = pos2/size(atom_list)
      call destroy_(atom_list)
      self%y_axis = pos2-pos1
      self%y_axis_defined = TRUE
      if (NOT self%y_width_set) then
         self%width(2) = FOUR*norm_(self%y_axis)
         call set_points_widths_origin_(self)
      end if
      call set_xyz_axes_from_y_axis_(self)
   end subroutine

   subroutine read_z_axis(self)
   ! Read the z axis of the plot. NOTE: If it is inputted, the current x and y
   ! axes are made orthogonal to it.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%z_axis)
      self%z_axis_defined = TRUE
      call set_xyz_axes_from_z_axis_(self)
   end subroutine

   subroutine read_z_axis_atoms(self)
   ! Read the atoms which define the z axis vector of the plot.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%z_atom_1)
      call read_(stdin,self%z_atom_2)
      DIE_IF(self%z_atom_1==self%z_atom_2,"PLOT_GRID:read_z_axis_atoms ... cannot specify same z axis atoms")
      self%z_axis_defined = TRUE
      call set_z_axis_from_atom_list_(self,self%atom)
   end subroutine

   subroutine read_x_width(self)
   ! Read the x width of the plot
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_quantity_(stdin,self%width(1))
      call set_points_widths_origin_(self)
      self%x_width_set = TRUE
   end subroutine

   subroutine read_y_width(self)
   ! Read the y width of the plot
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_quantity_(stdin,self%width(2))
      call set_points_widths_origin_(self)
      self%y_width_set = TRUE
   end subroutine

   subroutine read_z_width(self)
   ! Read the z width of the plot. This is normally 0.
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_quantity_(stdin,self%width(3))
      call set_points_widths_origin_(self)
      self%z_width_set = TRUE
   end subroutine

   subroutine read_bond_density_atoms(self)
   ! Read bond density atoms
      type(PLOT_GRID_TYPE), INOUT :: self
      INT :: n_atom
      call destroy_(self%bond_density_atom)
      call read_all_(stdin,self%bond_density_atom)

      ! Check
      n_atom = size(self%bond_density_atom)
      ENSURE(n_atom>=1 AND n_atom<=3,"PLOT_GRID:read_bond_density_atoms ... wrong length for bond_density_atoms")
      if (n_atom==3) then
         ENSURE(any(self%bond_density_atom(1:2)==self%bond_density_atom(3)),"PLOT_GRID:read_bond_density_atoms ... atom 3 unmatching")
      end if
   end subroutine

   subroutine read_box_scale_factor(self)
   ! Read the bounding box scale factor and apply it immediately.
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_box_scale_factor_(self,val)
      call set_box_axes_(self)
   end subroutine

   subroutine read_cube_scale_factor(self)
   ! Read the bounding box scale factor and apply it immediately.
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_box_scale_factor_(self,val)
      call set_cube_scale_factor_(self,val)
   end subroutine

   subroutine read_projection_direction(self)
   ! Read the directionb to prject the vectors onto
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%projection_direction)
      call normalize_(self%projection_direction)
   end subroutine

   subroutine read_points_filename(self)
   ! Read 3D points grid filename
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(stdin,self%points_filename)
   end subroutine

   subroutine read_3D_grid(self,pt)
   ! Read Cassam-Chenai's 3D grid
      type(PLOT_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pt
      type(TEXTFILE_TYPE), allocatable :: textfile
      INT :: i

      ! Open the file for reading
      call create_(textfile,self%points_filename)
      call open_for_(textfile,"read")

      ! Read them in.
      ! NOTE: this is a RAW Fortran read on textfile.unit, bypassing the TEXTFILE
      ! API and its IO_IS_ALLOWED guarding. Under MPI only the master actually
      ! opens the file (TEXTFILE.open is guarded) and its unit is then broadcast,
      ! so every non-master rank was reading from a unit it had never opened.
      ! Unlike the write-only cases, this one needs BOTH halves: guard the read
      ! so only the master does it, then broadcast the DATA -- a unit number
      ! cannot be shared between processes, only its contents.
      call create_(pt,self%n_pt,3)
      if (IO_IS_ALLOWED) then
         do i = 1,self%n_pt
            read(textfile%unit,*) pt(i,:)
         end do
      end if
      PARALLEL_BROADCAST(pt,master_processor_(tonto))
      call close_(textfile)
      call destroy_(textfile)
   end subroutine

   subroutine read_contour_min_value(self)
   ! Generic read and set a quantity with units
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_contour_min_value_(self,val)
   end subroutine

   subroutine read_contour_max_value(self)
   ! Generic read and set a quantity with units
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_contour_max_value_(self,val)
   end subroutine

   subroutine read_contour_increment(self)
   ! Generic read and set a quantity with units
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_contour_increment_(self,val)
   end subroutine

   subroutine read_contour_min_color(self)
   ! Read the color used for the minimum-value contour
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(self%contour_min_color)
   end subroutine

   subroutine read_contour_max_color(self)
   ! Read the color used for the maximum-value contour
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(self%contour_max_color)
   end subroutine

   subroutine read_contour_mid_color(self)
   ! Read the color used for the mid-value contour, usually the
   ! zero-value contour
      type(PLOT_GRID_TYPE), INOUT :: self
      call read_(self%contour_mid_color)
   end subroutine

   subroutine read_contour_color_scale(self)
   ! Generic read and set
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_contour_color_scale_(self,val)
   end subroutine

   subroutine read_contour_line_width(self)
   ! Generic read and set a quantity with units
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_contour_line_width_(self,val)
   end subroutine

   subroutine read_contour_tic_dps(self)
   ! Generic read and set
      type(PLOT_GRID_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_contour_tic_dps_(self,val)
   end subroutine

   subroutine read_contour_font_size(self)
   ! Generic read and set
      type(PLOT_GRID_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_contour_font_size_(self,val)
   end subroutine

   subroutine read_contour_fade_factor(self)
   ! Generic read and set a quantity with units
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_contour_fade_factor_(self,val)
   end subroutine

   subroutine read_use_colorbox_scale(self)
   ! Generic read and set
      type(PLOT_GRID_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_colorbox_scale_(self,val)
   end subroutine

   subroutine read_use_ruler_scale(self)
   ! Generic read and set
      type(PLOT_GRID_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_ruler_scale_(self,val)
   end subroutine

   subroutine read_contour_scale(self)
   ! Generic read and set
      type(PLOT_GRID_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_contour_scale_(self,val)
   end subroutine


!  ============
!  Bounding box
!  ============

   PURE subroutine set_box_scale_factor(self,factor)
   ! Set the bounding box scale factor
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL, IN :: factor
      self%box_scale_factor = factor
      ENSURE(self%box_scale_factor>ZERO,"PLOT_GRID:set_box_scale_factor ... scale factor not positive")
   end subroutine

   PURE subroutine set_box_axes(self)
   ! Set box axes -- they might have different widths.
      type(PLOT_GRID_TYPE), INOUT :: self
      if (NOT self%use_unit_cell_as_bbox) then
         if (x_or_y_or_z_axes_defined_(self)) then; call use_bbox_with_current_axes_(self)
         else; call use_bbox_with_shape_axes_(self)
         end if
      end if
   end subroutine

   PURE subroutine set_cube_scale_factor(self,factor)
   ! Set the bounding cube scale factor
      type(PLOT_GRID_TYPE), INOUT :: self
      REAL, IN :: factor
      call set_box_scale_factor_(self,factor)
      call set_cube_axes_(self)
   end subroutine

   PURE subroutine set_cube_axes(self)
   ! Set cube axes
      type(PLOT_GRID_TYPE), INOUT :: self
      if (x_or_y_or_z_axes_defined_(self)) then; call use_bcube_with_current_axes_(self)
      else; call use_bcube_with_shape_axes_(self)
      end if
   end subroutine

   PURE subroutine use_bbox_with_current_axes(self)
   ! Use a bounding box axes and widths coming from the current
   ! user-specified axes
      type(PLOT_GRID_TYPE), INOUT :: self
      call set_bbox_with_current_axes_(self,self%atom)
      self%center = self%box_center
      self%width = self%box_widths
      self%x_axis = self%box_axes(:,1)
      self%y_axis = self%box_axes(:,2)
      self%z_axis = self%box_axes(:,3)
      call set_points_widths_origin_(self)
   end subroutine

   PURE subroutine use_bbox_with_shape_axes(self)
   ! Use a bounding box axes and widths coming from the principal axes
   ! of the shape tensor (the inertia tensor with unit atomic masses).
      type(PLOT_GRID_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"PLOT_GRID:use_bbox_with_shape_axes ... no atom list")
      call set_bbox_with_shape_axes_(self,self%atom)
      self%center = self%box_center
      self%width = self%box_widths
      self%x_axis = self%box_axes(:,1)
      self%y_axis = self%box_axes(:,2)
      self%z_axis = self%box_axes(:,3)
      call set_points_widths_origin_(self)
   end subroutine

   PURE subroutine use_bcube_with_current_axes(self)
   ! Use a bounding cube from the current axes. The box width are
   ! taken to be the maximum of the three widths along current axes.
      type(PLOT_GRID_TYPE), INOUT :: self
      call set_bbox_with_current_axes_(self,self%atom)
      self%center = self%box_center
      self%width = maxval(self%box_widths)
      self%n_x = maxval([self%n_x,self%n_y,self%n_z])
      self%n_y = self%n_x
      self%n_z = self%n_x
      self%x_axis = self%box_axes(:,1)
      self%y_axis = self%box_axes(:,2)
      self%z_axis = self%box_axes(:,3)
      call set_points_widths_origin_(self)
   end subroutine

   PURE subroutine use_bcube_with_shape_axes(self)
   ! Use a bounding cube from the current shape tensor. The box width
   ! is taken to be the maximum of the three widths.
      type(PLOT_GRID_TYPE), INOUT :: self
      call set_bbox_with_shape_axes_(self,self%atom)
      self%center = self%box_center
      self%width = maxval(self%box_widths)
      self%x_axis = self%box_axes(:,1)
      self%y_axis = self%box_axes(:,2)
      self%z_axis = self%box_axes(:,3)
      call set_points_widths_origin_(self)
   end subroutine


!   use_unit_cell_as_bbox
!   ! Use the unit cell as the bounding box.  This sets a switch and
!   ! the axes must be obtained afterwards from a supplied cell.
!       .use_unit_cell_as_bbox = TRUE
!   end

!   widths_were_set result (res)
!   ! Return TRUE if any of the widths were inputted.
!      res :: BIN

!          OR .z_width_set
!   end

!   read_crystal_plane(in,unit_cell)
!   ! To read in a vector which describes a plane in the crystal.
!   ! The vector is the normal of the plane.
!     in :: INPUT
!     unit_cell :: UNIT_CELL, IN
!     in.read( .z_axis )
!     .z_axis.rotate(unit_cell.cell_mx)
!     .z_axis.normalize
!     .x_axis.normalize
!     .y_axis.to_cross_product_of( .z_axis, .x_axis)
!     .y_axis.normalize
!     .x_axis.to_cross_product_of( .y_axis, .z_axis)
!     .x_axis.normalize
!   end

!  ================
!  Make grid points
!  ================

   PURE subroutine make_points_0(self,x_pt,y_pt,z_pt)
   ! Make a list of the grid points
      type(PLOT_GRID_TYPE), IN :: self
      VEC(REAL,:), OUT :: x_pt,y_pt,z_pt
      REAL :: x1,x2,x3,y1,y2,y3,z1,z2,z3
      REAL :: ox,oy,oz
      INT :: ix,iy,iz,i,t2,t3
   ENSURE(size(x_pt)==self%n_pt,"PLOT_GRID:make_points_0 ... x_pt not correct size")
   ENSURE(size(y_pt)==self%n_pt,"PLOT_GRID:make_points_0 ... y_pt not correct size")
   ENSURE(size(z_pt)==self%n_pt,"PLOT_GRID:make_points_0 ... z_pt not correct size")
      ox = self%origin(1)
      oy = self%origin(2)
      oz = self%origin(3)
      x1 = self%del_x*self%x_axis(1)
      x2 = self%del_x*self%x_axis(2)
      x3 = self%del_x*self%x_axis(3)
      y1 = self%del_y*self%y_axis(1)
      y2 = self%del_y*self%y_axis(2)
      y3 = self%del_y*self%y_axis(3)
      z1 = self%del_z*self%z_axis(1)
      z2 = self%del_z*self%z_axis(2)
      z3 = self%del_z*self%z_axis(3)
      t2 = self%n_x*self%n_y
      do i = 1,self%n_pt
         iz = (i-1)/t2
         t3 = i-iz*t2
         iy = (t3-1)/self%n_x
         ix = t3-iy*self%n_x-1
         x_pt(i) = ox+ix*x1+iy*y1+iz*z1
         y_pt(i) = oy+ix*x2+iy*y2+iz*z2
         z_pt(i) = oz+ix*x3+iy*y3+iz*z3
      end do
   end subroutine

   PURE subroutine make_points_1(self,pt)
   ! Return a list of the grid points in "pt". 
      type(PLOT_GRID_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: pt
      VEC(REAL,3) :: x,y,z
      VEC(INT,3) :: v
      INT :: i,t2,t3
   ENSURE(self%n_x/=0,"PLOT_GRID:make_points_1 ... zero number of x points")
   ENSURE(self%n_y/=0,"PLOT_GRID:make_points_1 ... zero number of y points")
   ENSURE(size(pt,1)==self%n_pt,"PLOT_GRID:make_points_1 ... wrong 1st dimension, pt")
   ENSURE(size(pt,2)==3,"PLOT_GRID:make_points_1 ... wrong 2nd dimension, pt")

      ! Voxel vectors
      x = self%del_x*self%x_axis
      y = self%del_y*self%y_axis
      z = self%del_z*self%z_axis

      ! No. of points in base plane
      t2 = self%n_x*self%n_y
      do i = 1,self%n_pt

         ! Voxel coordinates "v'
         v(3) = (i-1)/t2
         t3 = i-v(3)*t2
         v(2) = (t3-1)/self%n_x
         v(1) = t3-v(2)*self%n_x-1

         ! Get actual cartesian coords of voxel "v"
         pt(i,1) = self%origin(1)+v(1)*x(1)+v(2)*y(1)+v(3)*z(1)
         pt(i,2) = self%origin(2)+v(1)*x(2)+v(2)*y(2)+v(3)*z(2)
         pt(i,3) = self%origin(3)+v(1)*x(3)+v(2)*y(3)+v(3)*z(3)
      end do
   end subroutine

   PURE subroutine make_points_2(self,pt,first,last)
   ! Return a list of the grid points in "pt". Will return a subset of
   ! the grid points from "first" to "last", if these options
   ! are provided.
      type(PLOT_GRID_TYPE), IN :: self
      INT, IN :: first,last
      MAT(REAL,:,:), OUT :: pt
      REAL :: x1,x2,x3,y1,y2,y3,z1,z2,z3
      REAL :: ox,oy,oz
      INT :: ix,iy,iz,i,f,l,t2,t3
   ENSURE(self%n_x/=0,"PLOT_GRID:make_points_2 ... zero number of x points")
   ENSURE(self%n_y/=0,"PLOT_GRID:make_points_2 ... zero number of y points")
      f = first
      l = last
      ox = self%origin(1)
      oy = self%origin(2)
      oz = self%origin(3)
      x1 = self%del_x*self%x_axis(1)
      x2 = self%del_x*self%x_axis(2)
      x3 = self%del_x*self%x_axis(3)
      y1 = self%del_y*self%y_axis(1)
      y2 = self%del_y*self%y_axis(2)
      y3 = self%del_y*self%y_axis(3)
      z1 = self%del_z*self%z_axis(1)
      z2 = self%del_z*self%z_axis(2)
      z3 = self%del_z*self%z_axis(3)
      t2 = self%n_x*self%n_y
      do i = f,l
         iz = (i-1)/t2
         t3 = i-iz*t2
         iy = (t3-1)/self%n_x
         ix = t3-iy*self%n_x-1
         pt(i,1) = ox+ix*x1+iy*y1+iz*z1
         pt(i,2) = oy+ix*x2+iy*y2+iz*z2
         pt(i,3) = oz+ix*x3+iy*y3+iz*z3
      end do
   end subroutine

   PURE subroutine make_points_3(self,pt,f,l)
   ! Return a list of the grid points in "pt" in grid xyz order, starting from
   ! the first and last x, y, and z points: "fx", "lx", "fy", "ly", "fz", "lz"
   ! respectively
      type(PLOT_GRID_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: pt
      VEC(INT,3), IN :: f,l
      call make_points_(self,pt,f(1),l(1),f(2),l(2),f(3),l(3))
   end subroutine

   PURE subroutine make_points_4(self,pt,fx,lx,fy,ly,fz,lz)
   ! Return a list of the grid points in "pt" in grid xyz order, starting from
   ! the first and last x, y, and z points: "fx", "lx", "fy", "ly", "fz", "lz"
   ! respectively
      type(PLOT_GRID_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: pt
      INT, IN :: fx,lx
      INT, IN :: fy,ly
      INT, IN :: fz,lz
      REAL :: x1,x2,x3
      REAL :: y1,y2,y3
      REAL :: z1,z2,z3
      REAL :: ox,oy,oz
      INT :: x,y,z
      INT :: ix,iy,iz,i
   ENSURE(size(pt,1)==(lx-fx+1)*(ly-fy+1)*(lz-fz+1),"PLOT_GRID:make_points_4 ... wrong 1st dimension, pt")
   ENSURE(size(pt,2)==3,"PLOT_GRID:make_points_4 ... wrong 2nd dimension, pt")
      ox = self%origin(1)
      oy = self%origin(2)
      oz = self%origin(3)
      x1 = self%del_x*self%x_axis(1)
      x2 = self%del_x*self%x_axis(2)
      x3 = self%del_x*self%x_axis(3)
      y1 = self%del_y*self%y_axis(1)
      y2 = self%del_y*self%y_axis(2)
      y3 = self%del_y*self%y_axis(3)
      z1 = self%del_z*self%z_axis(1)
      z2 = self%del_z*self%z_axis(2)
      z3 = self%del_z*self%z_axis(3)
      i = 0
      do z = fz,lz
         do y = fy,ly
            do x = fx,lx
               ix = x-1
               iy = y-1
               iz = z-1
               i = i+1
               pt(i,1) = ox+ix*x1+iy*y1+iz*z1
               pt(i,2) = oy+ix*x2+iy*y2+iz*z2
               pt(i,3) = oz+ix*x3+iy*y3+iz*z3
            end do
         end do
      end do
   end subroutine

   PURE function point(self,x,y,z) result (res)
   ! Return one particular point "res" of the grid, given by grid coordinates
   ! "x", "y", and "z". (1,1,1) is the bottom (front) left hand corner.
      type(PLOT_GRID_TYPE), IN :: self
      INT, IN :: x,y,z
      VEC(REAL,3) :: res
      REAL :: x1,x2,x3,y1,y2,y3,z1,z2,z3,ox,oy,oz
      INT :: ix,iy,iz
      ox = self%origin(1)
      oy = self%origin(2)
      oz = self%origin(3)
      x1 = self%del_x*self%x_axis(1)
      x2 = self%del_x*self%x_axis(2)
      x3 = self%del_x*self%x_axis(3)
      y1 = self%del_y*self%y_axis(1)
      y2 = self%del_y*self%y_axis(2)
      y3 = self%del_y*self%y_axis(3)
      z1 = self%del_z*self%z_axis(1)
      z2 = self%del_z*self%z_axis(2)
      z3 = self%del_z*self%z_axis(3)
      ix = x-1
      iy = y-1
      iz = z-1
      res(1) = ox+ix*x1+iy*y1+iz*z1
      res(2) = oy+ix*x2+iy*y2+iz*z2
      res(3) = oz+ix*x3+iy*y3+iz*z3
   end function

   PURE subroutine make_cube_of_points(self,p,f,l,del_x,del_y,del_z,x,y,z,eval)
   ! Return a cube of points "p" where the cube edge points start from
   ! index "f" and end at index "l", and each point separated by
   ! distance "del_x", "del_y", "del_z" along a grid axis direction
   ! (these are *not* the same as the grid .del's).  The (0,0,0) point
   ! of the cube "p" corresponds to the plot grid coordinates "x", "y",
   ! "z", which are numbered from 0 (normally the first point is
   ! numbered from 1; these are grid coords in .del_x, .dely, .del_z).
   ! The axes of the cube are given by the plot grid axes.
      type(PLOT_GRID_TYPE), IN :: self
      INT, IN :: f,l,x,y,z
      MAT(REAL,3,*), OUT :: p
      REAL, IN :: del_x,del_y,del_z
      VEC(BIN,*), optional, IN :: eval
      REAL :: x1,x2,x3,y1,y2,y3,z1,z2,z3,ox,oy,oz
      INT :: ix,iy,iz,k

      ! Plot grid axes times plot grid del's
      x1 = self%del_x*self%x_axis(1); x2 = self%del_x*self%x_axis(2); x3 = self%del_x*self%x_axis(3)
      y1 = self%del_y*self%y_axis(1); y2 = self%del_y*self%y_axis(2); y3 = self%del_y*self%y_axis(3)
      z1 = self%del_z*self%z_axis(1); z2 = self%del_z*self%z_axis(2); z3 = self%del_z*self%z_axis(3)

      ! Cube origin in plotrgrid coords
      ox = self%origin(1)+x*x1+y*y1+z*z1
      oy = self%origin(2)+x*x2+y*y2+z*z2
      oz = self%origin(3)+x*x3+y*y3+z*z3

      ! Plotgrid axes times cube del's
      x1 = del_x*self%x_axis(1); x2 = del_x*self%x_axis(2); x3 = del_x*self%x_axis(3)  !
      y1 = del_y*self%y_axis(1); y2 = del_y*self%y_axis(2); y3 = del_y*self%y_axis(3)
      z1 = del_z*self%z_axis(1); z2 = del_z*self%z_axis(2); z3 = del_z*self%z_axis(3)
      if (NOT present(eval)) then
         k = 0
         do iz = f,l
            do iy = f,l
               do ix = f,l
                  k = k+1
                  p(1,k) = ox+ix*x1+iy*y1+iz*z1
                  p(2,k) = oy+ix*x2+iy*y2+iz*z2
                  p(3,k) = oz+ix*x3+iy*y3+iz*z3
               end do
            end do
         end do
      else
         k = 0
         do iz = f,l
            do iy = f,l
               do ix = f,l
                  k = k+1
                  if (NOT eval(k)) cycle
                  p(1,k) = ox+ix*x1+iy*y1+iz*z1
                  p(2,k) = oy+ix*x2+iy*y2+iz*z2
                  p(3,k) = oz+ix*x3+iy*y3+iz*z3
               end do
            end do
         end do
      end if
   end subroutine

   PURE subroutine make_math_pos(self,math_pos,z_range,valid)
   ! Calculate the positions "math_pos" of the valid atoms which have
   ! been rotated and shifted and scaled into grid point coordinates.
   ! Valid atoms are those which are within "z_range" of the plot
   ! plane; the indices of the valid atoms are stored in "valid"
      type(PLOT_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: math_pos
      REAL, IN :: z_range
      VEC(INT,:), allocatable, OUT :: valid
      VEC(REAL,3) :: scale_factor,p
      MAT(REAL,3,3) :: tr
      MAT(REAL,:,:), allocatable :: pos
      INT :: n,nv,a,i
   ENSURE(allocated(self%atom),"PLOT_GRID:make_math_pos ... no atoms!")
   ENSURE(z_range>ZERO,"PLOT_GRID:make_math_pos ... z_range must be non-negative")
   ENSURE(NOT allocated(valid),"PLOT_GRID:make_math_pos ... valid array already allocated")

      ! Get shifted atom positions
      n = size(self%atom)
      call create_(pos,n,3)
      do a = 1,n
         pos(a,:) = self%atom(a)%position-self%origin  !shift
      end do

      ! Rotate positions into plot grid axis
      tr = transposed_plot_axes_(self)
      do a = 1,n
         p = pos(a,:)
         call to_product_of_(pos(a,:),tr,p)
      end do

      ! Reject atoms far from plot plane, get valid atoms
      call create_(valid,0)
      do a = 1,n

         ! if ( abs(pos(a,z)) > z_range) cycle
         if (abs(pos(a,3))>z_range) cycle
         call append_(valid,a)
      end do

      ! Create list of valid atoms positions
      nv = size(valid)
      call create_(math_pos,nv,3)
      i = 0
      do a = 1,n
         if (abs(pos(a,3))>z_range) cycle
         i = i+1
         math_pos(i,:) = pos(a,:)
      end do

      ! Finally, scale positions
      scale_factor(1) = (self%n_x-ONE)/self%width(1)
      scale_factor(2) = (self%n_y-ONE)/self%width(2)
      scale_factor(3) = ZERO
      do a = 1,nv
         math_pos(a,:) = scale_factor*math_pos(a,:)+ONE  ! since plot begins at 1
      end do
      call destroy_(pos)
   end subroutine


!  ===================
!  Batched grid points
!  ===================

   PURE subroutine make_01_uc_pts_as_mhph_pts(self,pt)
   ! Make the [0,1] unit-cell points (with the first and last poinyts
   ! on each cell representing the same plane) transformed into a
   ! [-1/2,+1/2) cell, for the CRYTSTAL23 program. 
   ! NOTE: plot axes should be set to unit cell already!
      type(PLOT_GRID_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: pt
      VEC(REAL,3) :: da,db,dc
      INT :: j1,j2,j3
      INT :: i1,i2,i3
      INT :: hx,hy,hz
      INT :: n
   ENSURE(self%n_x/=0,"PLOT_GRID:make_01_uc_pts_as_mhph_pts ... zero number of x points")
   ENSURE(self%n_y/=0,"PLOT_GRID:make_01_uc_pts_as_mhph_pts ... zero number of y points")
   ENSURE(is_odd_(self%n_x),"PLOT_GRID:make_01_uc_pts_as_mhph_pts ... n_x is not odd!")
   ENSURE(is_odd_(self%n_y),"PLOT_GRID:make_01_uc_pts_as_mhph_pts ... n_x is not odd!")
   ENSURE(is_odd_(self%n_z),"PLOT_GRID:make_01_uc_pts_as_mhph_pts ... n_x is not odd!")
   ENSURE(size(pt,1)==self%n_pt,"PLOT_GRID:make_01_uc_pts_as_mhph_pts ... wrong 1st dimension, pt")
   ENSURE(size(pt,2)==3,"PLOT_GRID:make_01_uc_pts_as_mhph_pts ... wrong 2nd dimension, pt")

      ! Delta times unit cell axes
      da = self%del_x*self%x_axis
      db = self%del_y*self%y_axis
      dc = self%del_z*self%z_axis

      ! The is the half-periodicity
      hx = (self%n_x-1)/2
      hy = (self%n_y-1)/2
      hz = (self%n_z-1)/2
      n = 0
      ! Ordering is standard [0,1] unit cell
      ! with positive tic marks per point
      do i3 = 0,self%n_z-1
         do i2 = 0,self%n_y-1
            do i1 = 0,self%n_x-1  ! i1=1 origin (x = 0) ! n_x=5 :: i1 = 1,2,3,4,5 

                                          !               :       :
                                          !             ::  repeated
         ! CRYSTAL23 [-1/2,+1/2] points
               !       = 2                                   org    hf
               ! mod(1-1+2=2,4) - 2 =  0   mod(x  ,4)   =   0   1   2   3   4   5
               ! mod(2-1+2=3,4) - 2 =  1   mod(x  ,4)-2 =  -2  -1   0   1 
               ! mod(3-1+2=4,4) - 2 = +-2  mod(x+2,4)-2 =   0   1  -2  -1
               ! mod(4-1+2=5,4) - 2 = -1
               ! mod(5-1+2=6,4) - 2 =  0
               !
               ! target j1 = 0 ,"1/4", "1/2", "-1/2" ,"-1/4"
               !           = 0 1 2 -2 -1    
               j1 = mod(i1+hx,2*hx)-hx
               j2 = mod(i2+hy,2*hy)-hy
               j3 = mod(i3+hz,2*hz)-hz
               n = n+1
               pt(n,1) = j1*da(1)+j2*db(1)+j3*dc(1)
               pt(n,2) = j1*da(2)+j2*db(2)+j3*dc(2)
               pt(n,3) = j1*da(3)+j2*db(3)+j3*dc(3)
            end do
         end do
      end do
   end subroutine


!  ===================
!  Batched grid points
!  ===================

   PURE subroutine make_batched_points(self,pt,batch)
   ! Return a list of the grid points "pt" for a given "batch" index.
   ! These are a little rectlinear part of the big grid.
      type(PLOT_GRID_TYPE), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: pt
      INT, IN :: batch
      VEC(INT,3) :: f,l
      INT :: np
      call set_fl_for_batch_(self,f,l,batch)
      np = product(l-f+1)
      call create_(pt,np,3)
      call make_points_(self,pt,f,l)
   end subroutine

   PURE function n_batches(self) result (res)
   ! Return the number of batches for the given ".min_batch_size". 
      type(PLOT_GRID_TYPE), IN :: self
      INT :: res
      INT :: mx,my,mz
      INT :: np
      np = n_pts_per_axis_(self)
      mx = ceiling(REALIFY(self%n_x/np))
      my = ceiling(REALIFY(self%n_y/np))
      mz = ceiling(REALIFY(self%n_z/np))
      res = mx*my*mz
   end function

   PURE function n_pts_for_batch(self,batch) result (res)
   ! Return the number of points for batch "batch".
      type(PLOT_GRID_TYPE), IN :: self
      INT, IN :: batch
      INT :: res
      VEC(INT,3) :: f,l
      call set_fl_for_batch_(self,f,l,batch)
      res = product(l-f+1)
   end function

   PURE subroutine set_fl_for_batch(self,f,l,batch)
   ! Set the fitst and last starting tics on each axis, "f" and "l",
   ! for a given "batch" index.
      type(PLOT_GRID_TYPE), IN :: self
      VEC(INT,3), OUT :: f,l
      INT, IN :: batch
      INT :: mx,my,mz
      INT :: bx,by,bz
      INT :: bb,np
      np = n_pts_per_axis_(self)
      mx = ceiling(REALIFY(self%n_x/np))
      my = ceiling(REALIFY(self%n_y/np))
      mz = ceiling(REALIFY(self%n_z/np))
      bb = 0
      outer: do bz = 1,mz
         do by = 1,my
            do bx = 1,mx
               bb = bb+1

         ! X, Y, Z axis poinht limits
               f(1) = (bx-1)*np+1
               l(1) = min(bx*np,self%n_x)
               f(2) = (by-1)*np+1
               l(2) = min(by*np,self%n_y)
               f(3) = (bz-1)*np+1
               l(3) = min(bz*np,self%n_z)
               if (bb==batch) exit outer
            end do
         end do
      end do outer
   end subroutine

   PURE function n_pts_per_axis(self) result (res)
   ! Return the number of points per axis so that the number of points
   ! is just above ".min_batch_size". This assumes that points per axis
   ! are separated by equal divisions and groups them by cubes.
      type(PLOT_GRID_TYPE), IN :: self
      INT :: res
      if (self%n_z>1) then
         res = ceiling((REALIFY(self%min_batch_size))**(THIRD))
      else
         res = ceiling((REALIFY(self%min_batch_size))**(HALF))
      end if
   end function


!  ======
!  Output
!  ======

   subroutine put(self,title)
   ! Put the grid data to "stdout"
      type(PLOT_GRID_TYPE), IN :: self
      STR(len=*), optional, IN :: title
      MAT(REAL,3,3) :: axes
      INT :: length,wid
      STR(len=STR_SIZE) :: heading
      heading = "Plot grid data"
      if (present(title)) heading = title
      length = len_trim(heading)
      call flush_(stdout)
      call text_(stdout,repeat("=",length))
      call text_(stdout,trim(heading))
      call text_(stdout,repeat("=",length))
      call flush_(stdout)
      call text_(stdout,". Usually, the plot center, plot widths, and plot axes are specified.")
      call text_(stdout,"  Unless specified otherwise the z-axis width is assumed to be zero")
      call text_(stdout,"  i.e. it is assumed we are constructing a 2D plot grid.")
      call flush_(stdout)
      call text_(stdout,". The origin is calculated as the front-left-bottom point.")
      call flush_(stdout)
      call text_(stdout,". An atom or atoms may be used to specify the plot center.")
      call flush_(stdout)
      call text_(stdout,". Atoms may also be used to specify the x and y axes; and the y and z")
      call text_(stdout,"  axes are orthogonalized to the x and y axes, respectively.")
      call flush_(stdout)
      call text_(stdout,". Alternatively, a a bonding box around the molecule may be used.")
      call text_(stdout,"  The axes diagonalised the moment of inertia tensor using unit")
      call text_(stdout,"  weight atoms, so-called shape axes.")
      call flush_(stdout)
      call text_(stdout,". For crystals, the unit cell axes may be chosen to be the plot axes.")
      call text_(stdout,"  In most cases the midpoint of the unit cell is the plot center.")
      call text_(stdout,"  For CRYSTAL23, the origin is chosen as the plot center")
      call flush_(stdout)
      call text_(stdout,". Normally only the no. of points on the x axis is specified, the no.")
      call text_(stdout,"  of points on the other axes is adjusted to have the same linear")
      call text_(stdout,"  point density, roughly.")
      call flush_(stdout)
      call text_(stdout,". Alternatively, a desired point separation is specified. Then the")
      call text_(stdout,"  no. of points on each axis is calculated to just exceed it.")
      call flush_(stdout)
      call text_(stdout,". All coordinates are in au.")
      call flush_(stdout)

      ! What is it?
      call show_(stdout,"Kind of plot            =",self%plot_kind)
      call show_(stdout,"Plot orbital no. ?      =",self%orbital)

      ! Point info ...
      call flush_(stdout)
      call text_(stdout,"No. of points:")
      call show_(stdout,"Number of X grid points =",self%n_x)
      call show_(stdout,"Number of Y grid points =",self%n_y)
      call show_(stdout,"Number of Z grid points =",self%n_z)
      call show_(stdout,"Total number of points  =",self%n_pt)
      call flush_(stdout)
      call text_(stdout,"Point separations:")
      call show_(stdout,"Point separation (x)    =",self%del_x)
      call show_(stdout,"Point separation (y)    =",self%del_y)
      call show_(stdout,"Point separation (z)    =",self%del_z)

      ! Nice width
      axes = plot_axes_(self)
      wid = maxval(len_trim(to_str_(axes,"f",50,stdout%style%real_precision)))+1
      axes = plot_axes_(self)

      ! Centering
      call flush_(stdout)
      call text_(stdout,"Plot centering:")
      call show_(stdout,"Plot center point       =",self%center,width=wid)
      call show_(stdout,"Center offset           =",self%offset,width=wid)
      call show_(stdout,"Origin                  =",self%origin,width=wid)

      ! Plot bounding boxes
      call flush_(stdout)
      call text_(stdout,"Plot bounding box info:")
      call show_(stdout,"Box center              =",self%box_center,wid)
      call show_(stdout,"Bounding box used?      =",self%use_bounding_box)
      call show_(stdout,"Unit cell as is bbox?   =",self%use_unit_cell_as_bbox)

      ! Plot widths & axes
      call flush_(stdout)
      call text_(stdout,"Plot widths & axes:")
      call show_(stdout,"Widths                  =",self%width,wid)
      call show_(stdout,"Axis vector columns     =",axes(1,:),width=wid)
      call show_(stdout,"                        =",axes(2,:),width=wid)
      call show_(stdout,"                        =",axes(3,:),width=wid)

      ! Atom-based data
      call flush_(stdout)
      call text_(stdout,"Atom-based center:")
      call show_(stdout,"Center atom             =",self%center_atom)
      call show_(stdout,"Center atoms list used? =",allocated(self%center_atoms))
      if (allocated(self%center_atoms)) then
         call show_(stdout,"Center atoms list       =",to_concatenated_str_(self%center_atoms))
      end if
      call show_(stdout,"X axis atoms?           =",[self%x_atom_1,self%x_atom_2])
      call show_(stdout,"Y axis atoms?           =",[self%y_atom_1,self%y_atom_2])
   end subroutine

   subroutine put_minimal(self)
   ! Put minimal *angstrom*
      type(PLOT_GRID_TYPE), IN :: self
      REAL :: fac
      fac = ANGSTROM_PER_BOHR
      call show_(stdout,"No. of X grid points     =",self%n_x)
      call show_(stdout,"No. of Y grid points     =",self%n_y)
      call show_(stdout,"No. of Z grid points     =",self%n_z)
      call show_(stdout,"Total no. of points      =",self%n_pt)
      call show_(stdout,"X point separation       =",self%del_x*fac)
      call show_(stdout,"Y point separation       =",self%del_y*fac)
      call show_(stdout,"Z point separation       =",self%del_z*fac)
   end subroutine

end module
