!==================================================================
!
! CAPPING_SQUARE: 
!
! For generating triangulated caps (on boundary of plot grid).
!
! (C) Mike Turner, Dylan Jayatilaka, 2009
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
module CAPPING_SQUARE_MODULE

#  include "macros"

   include "capping_square.use"

   implicit none

   include "capping_square.int"


!  ===============
!  Data Statements
!  ===============

! This is a capping square
!        V3      e2       V2
!          +-------------+
!          |             |
!          |             |
!        e3|             |e1
!          |             |
!          |             |
!          +-------------+
!        V0      e0       V1
!
!   +Y
!   ^
!   |
!   +--> +X
!
! Every square has 4 vertices. "square" lists the four vertices of a
! square in terms of x and y. The vertices indices are in the range 0:3
   MAT(INT,1:2,0:3), private :: square
!                        V0   V1   V2   V3
   data square(1:2,0:3)/0,0, 1,0, 1,1, 0,1/

! There are 4 square edges which can be defined in a standard order.
! "vertex_1_of_edge(e)" gives the 1st vertex index of the e-th square edge.
! "vertex_2_of_edge(e)" gives the 2nd vertex index of the e-th square edge.
! The edge indices are in the range 0:3
   VEC(INT,0:3), private :: vertex_1_of_edge
   VEC(INT,0:3), private :: vertex_2_of_edge
!                                  e0  e1  e2  e3
   data vertex_1_of_edge(0:3)/      0,  1,  2,  3/
   data vertex_2_of_edge(0:3)/      1,  2,  3,  0/

! The "xy_axis_of_edge(e)" array defines which axis (1-2 or x-y) is changing
! for every edge "e" for the 4 standard square edges.
   VEC(INT,0:3), private :: xy_axis_of_edge
!                                 e0  e1  e2  e3
   data xy_axis_of_edge(0:3)/      1,  2,  1,  2/

!  No edge table included at present

!  The "triangle_table(n)" lists the edges used to form each triangle,
!  for the n-th possible case number. They come in groups of three,
!  and -1 is the null value indicating no edge, e.g. the fourth data 
!  item below is:

!  This contains two triangles.  The first is formed from points on 
!  edges 4, 3, 1, while the second is formed from points on edges 4, 1, 5.
!
!  Unlike Marching Cubes, the capping square requires not only the edges
!  (of the capping square or marching cube) but also the vertices as well
!  when triangulating. To do this the vertices are mapped to "edges":
!  V0 -> e4, V1 -> e5, V2 -> e6, V3 -> e7.
   MAT(INT,0:11,0:15), private :: triangle_table

! Maximum of 4 triangles with 3 indices = 12 values (0:11)
! 16 possible cases for triangulation (0:15)
   data triangle_table(0:11, 0:15)/          &
        -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
         4, 3, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
         0, 1, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
         4, 3, 1, 4, 1, 5,-1,-1,-1,-1,-1,-1, &
         1, 2, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
         5, 0, 1, 0, 2, 1, 0, 3, 2, 3, 7, 2, &
         0, 2, 6, 0, 6, 5,-1,-1,-1,-1,-1,-1, &
         5, 4, 3, 5, 3, 2, 5, 2, 6,-1,-1,-1, &
         2, 3, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
         2, 0, 4, 2, 4, 7,-1,-1,-1,-1,-1,-1, &
         4, 3, 0, 3, 1, 0, 3, 2, 1, 2, 6, 1, &
         4, 7, 2, 4, 2, 1, 4, 1, 5,-1,-1,-1, &
         1, 3, 7, 1, 7, 6,-1,-1,-1,-1,-1,-1, &
         7, 6, 1, 7, 1, 0, 7, 0, 4,-1,-1,-1, &
         6, 5, 0, 6, 0, 3, 6, 3, 7,-1,-1,-1, &
         4, 7, 6, 4, 6, 5,-1,-1,-1,-1,-1,-1 /

! Normals for each of the 6 possible faces (i.e 6 orientations of the capping square)
   MAT(REAL,1:3,1:6) :: face_normal_table
!                                        f1 (z=0)          f2(+z)         f3(x=0)         f4(+x)
   data face_normal_table(1:3,1:6) /ZERO,ZERO,ONE, ZERO,ZERO,-ONE, ONE,ZERO,ZERO, -ONE,ZERO,ZERO, &
                                    ZERO,ONE,ZERO, ZERO,-ONE,ZERO /
!                                         f5(y=0)          f6(+y)

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(CAPPING_SQUARE_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"CAPPING_SQUARE:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(CAPPING_SQUARE_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"CAPPING_SQUARE:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine set_defaults(self)
   ! Set up default information.
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      self%face_number = 0
      self%iso_value = ONE
      self%skip_bit_string = 0
      self%vertex_positions = ZERO
      self%vertex_fn_value = ZERO
      self%cap_vertex_flag = FALSE
      self%case = -1  ! This is not a real case
      self%n_triangle = 0
      self%n_pt = 0
      self%triangle_edge_index = 0
      self%triangle_vertex_index = 0
      self%edge_vertex_index = 0
      self%edge_vertex_positions = ZERO
      self%edge_vertex_gradient = ZERO
      self%edge_mean_curvature = ZERO
      self%edge_gaussian_curvature = ZERO
      self%new_vertex_index = 0
      self%no_new_vertex = 0
   end subroutine

   PURE subroutine reset(self)
   ! Reset defaults. 
   ! Compared to set_defaults, somethings can remain unchanged e.g.
   ! iso_value But at present everything is reset
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      self%face_number = 0
      self%iso_value = ONE
      self%skip_bit_string = 0
      self%vertex_positions = ZERO
      self%vertex_fn_value = ZERO
      self%cap_vertex_flag = FALSE
      self%case = -1  ! This is not a real case
      self%n_triangle = 0
      self%n_pt = 0
      self%triangle_edge_index = 0
      self%triangle_vertex_index = 0
      self%edge_vertex_index = 0
      self%edge_vertex_positions = ZERO
      self%edge_vertex_gradient = ZERO
      self%edge_mean_curvature = ZERO
      self%edge_gaussian_curvature = ZERO
      self%new_vertex_index = 0
      self%no_new_vertex = 0
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_iso_value(self,iso_value)
   ! Set the iso_value for the capping square
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      REAL, IN :: iso_value
      self%iso_value = iso_value
   end subroutine

   PURE subroutine set_n_pt(self,n_pt)
   ! Sets the current number of points using the triangulation of the
   ! surface
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      INT, IN :: n_pt
      self%n_pt = n_pt
   end subroutine

   PURE subroutine set_face(self,face_number)
   ! Set the face_number for the capping square (i.e. it's
   ! orientation)
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      INT, IN :: face_number
      self%face_number = face_number
   end subroutine

   PURE subroutine set_vertex_positions(self,pos)
   ! Set the vertex positions of each vertex of the capping square
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      MAT(REAL,1:3,1:4), IN :: pos
      self%vertex_positions = pos
   end subroutine

   PURE subroutine set_vertex_values(self,values)
   ! Set the values at the vertices of the capping square
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      VEC(REAL,1:4), IN :: values
      self%vertex_fn_value = values
   end subroutine

   PURE subroutine set_edge_vertex_index(self,edge_index)
   ! Set the vertex indices that appear on the edges of the capping
   ! square.  These come from the previously done triangulation of the
   ! corresponding marching cube.
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      VEC(INT,1:4), IN :: edge_index
      self%edge_vertex_index(0:3) = edge_index(1:4)
   end subroutine

   PURE subroutine set_edge_vertex_positions(self,edge_pos)
   ! Set the interpolated edge positions from the marching cube for use by the capping square
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      MAT(REAL,1:3,1:4), IN :: edge_pos
      self%edge_positions = edge_pos
   end subroutine

   PURE subroutine set_cap_vertex_flags(self)
   ! Using the vertex values and the iso_value to determine which
   ! vertices of the square are capping vertices.  This information is
   ! used to determine the "case" of the capping square.
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      self%cap_vertex_flag(0) = self%vertex_fn_value(0)<self%iso_value
      self%cap_vertex_flag(1) = self%vertex_fn_value(1)<self%iso_value
      self%cap_vertex_flag(2) = self%vertex_fn_value(2)<self%iso_value
      self%cap_vertex_flag(3) = self%vertex_fn_value(3)<self%iso_value
   end subroutine

   PURE subroutine set_case_info(self)
   ! Set the capping square case information.
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      self%case = case_number_(self)
   end subroutine

   PURE subroutine set_triangulation_info(self)
   ! Set the capping square triangulation information i.e. which edges of
   ! the square are involved in triangulation (remember the vertices of the capping
   ! square are also encoded as "edges" - see comments at triangulation table).
   ! Also sets how many triangles are needed.
   ! NOTE: the case number is assumed to be correctly set by a call to
   ! .set_case_info
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      INT :: i,e
      self%n_triangle = no_of_triangles_(self,self%case)
      self%triangle_edge_index = reshape(triangle_table(0:11,self%case),[3,4])

      ! Reverse triangle order
      if (self%iso_value<ZERO) then
         do i = 1,self%n_triangle
            e = self%triangle_edge_index(1,i)
            self%triangle_edge_index(1,i) = self%triangle_edge_index(3,i)
            self%triangle_edge_index(3,i) = e
         end do
      end if
      self%triangle_vertex_index = 0
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine get_new_vertex_positions(self,pos)
   ! Get the positions found on this capping square for the new vertices (see above).
      type(CAPPING_SQUARE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: pos
      INT :: i
   ENSURE(size(pos,1)==3,"CAPPING_SQUARE:get_new_vertex_positions ... wrong 1st dimension, pos")
   ENSURE(size(pos,2)>=no_of_new_vertices_(self),"CAPPING_SQUARE:get_new_vertex_positions ... pos too small")
      do i = 1,no_of_new_vertices_(self)
         pos(:,i) = self%edge_vertex_positions(:,self%new_vertex_index(i))
      end do
   end subroutine

   PURE subroutine get_new_vertex_gradients(self,g)
   ! Get the gradient vectors "g" on this capping square for the new vertices.
      type(CAPPING_SQUARE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: g
      INT :: i
   ENSURE(size(g,1)==3,"CAPPING_SQUARE:get_new_vertex_gradients ... wrong 1st dimension, g")
   ENSURE(size(g,2)>=no_of_new_vertices_(self),"CAPPING_SQUARE:get_new_vertex_gradients ... g array too small")
      do i = 1,no_of_new_vertices_(self)
         g(:,i) = self%edge_vertex_gradient(:,self%new_vertex_index(i))
      end do
   end subroutine

   PURE subroutine get_new_mean_curvatures(self,c)
   ! Get the mean curvatures "c" on this capping square for the new vertices.
      type(CAPPING_SQUARE_TYPE), IN :: self
      VEC(REAL,:), INOUT :: c
      INT :: i
   ENSURE(size(c)>=no_of_new_vertices_(self),"CAPPING_SQUARE:get_new_mean_curvatures ... c array too small")
      do i = 1,no_of_new_vertices_(self)
         c(i) = self%edge_mean_curvature(self%new_vertex_index(i))
      end do
   end subroutine

   PURE subroutine get_new_gaussian_curvatures(self,c)
   ! Get the gaussian curvatures "c" on this capping square for the new vertices.
      type(CAPPING_SQUARE_TYPE), IN :: self
      VEC(REAL,:), INOUT :: c
      INT :: i
   ENSURE(size(c)>=no_of_new_vertices_(self),"CAPPING_SQUARE:get_new_gaussian_curvatures ... c array too small")
      do i = 1,no_of_new_vertices_(self)
         c(i) = self%edge_gaussian_curvature(self%new_vertex_index(i))
      end do
   end subroutine

   PURE subroutine get_triangle_vertex_indices(self,ind)
   ! Set the vertex positions found on this capping square for the new vertices.
      type(CAPPING_SQUARE_TYPE), IN :: self
      MAT(INT,:,:), INOUT :: ind
   ENSURE(size(ind,1)==3,"CAPPING_SQUARE:get_triangle_vertex_indices ... wrong 1st dimension, ind")
   ENSURE(size(ind,2)==self%n_triangle,"CAPPING_SQUARE:get_triangle_vertex_indices ... wrong 2nd dimension, ind")
      ind = self%triangle_vertex_index(:,1:self%n_triangle)
   end subroutine


!  ==============================
!  Triangulation: the main thing!
!  ==============================

   PURE subroutine triangulate(self)
   ! Do all the steps required to determine all the triangles for a given
   ! capping square.
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      call set_cap_vertex_flags_(self)
      call set_case_info_(self)
      call set_triangulation_info_(self)
      call set_triangle_vertex_info_(self)
   end subroutine

   PURE subroutine set_triangle_vertex_info(self)
   ! Generates all the information required for each triangle (vertex
   ! indices, positions, gradients and curvatures)
   !
   ! There are two cases to handle depending on the triangles edge
   ! index, e:
   !
   ! Case 1: if 0<=e<=3 then the corresponding edge vertex has already
   !         been created by the marching cubes routine.
   !
   ! Case 2: if 4<=e<=7 then we need to use the corresponding capping
   !         square vertex (see comments in triangle table) to
   !         generate a new edge vertex (index, position, gradient and
   !         curvature).  The gradient comes from the
   !         face_normal_table and since there is no curvature for the
   !         capping faces the curvatures are both zero.
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      INT :: n,v,e,i,done_edge,cube_done_edge
      done_edge = 0
      cube_done_edge = 0
      i = 0
      do n = 1,self%n_triangle
         do v = 1,3
            e = self%triangle_edge_index(v,n)
            if (BIT_SET_AT_POS_IN(self%skip_bit_string,e)) then

            ! Edge vertex info exists, so use it
               self%triangle_vertex_index(v,n) = self%edge_vertex_index(e)
               ENSURE(self%edge_vertex_index(e)/=0,"CAPPING_SQUARE:set_triangle_vertex_info ... zero index, edge:"//trim(to_str_(e)))
               cycle
            end if
            if (e<4) then

            ! This is a real edge i.e. a vertex already determined
            ! from the marching cube Previously we shared the edge but
            ! this results in having to share the normals which in
            ! turn introduces a graphics artifacts when OpenGL color
            ! blending is used.  To solve this, the vertex is
            ! duplicated but the face normal comes from the capping
            ! squares face_normal_table.
               if (BIT_SET_AT_POS_IN(cube_done_edge,e)) then
                  self%triangle_vertex_index(v,n) = self%edge_vertex_index(e)
                  cycle
               end if
               i = i+1
               self%n_pt = self%n_pt+1
               self%triangle_vertex_index(v,n) = self%n_pt
               self%edge_vertex_index(e) = self%n_pt
               self%new_vertex_index(i) = e
               self%edge_vertex_positions(:,e) = self%edge_positions(:,e)
               self%edge_vertex_gradient(:,e) = face_normal_table(:,self%face_number)
               self%edge_mean_curvature(e) = ZERO
               self%edge_gaussian_curvature(e) = ZERO
               cube_done_edge = SET_BIT_AT_POS_IN(cube_done_edge,e)
            else

            ! Convert pretend edge to corresponding vertex of square
               if (BIT_SET_AT_POS_IN(done_edge,e)) then
                  self%triangle_vertex_index(v,n) = self%edge_vertex_index(e)
                  cycle
               end if
               i = i+1
               self%n_pt = self%n_pt+1
               self%triangle_vertex_index(v,n) = self%n_pt
               self%edge_vertex_index(e) = self%n_pt
               self%new_vertex_index(i) = e
               self%edge_vertex_positions(:,e) = self%vertex_positions(:,e-4)
               self%edge_vertex_gradient(:,e) = face_normal_table(:,self%face_number)
               self%edge_mean_curvature(e) = ZERO
               self%edge_gaussian_curvature(e) = ZERO
               done_edge = SET_BIT_AT_POS_IN(done_edge,e)
            end if
         end do
      end do
      self%no_new_vertex = i
   end subroutine

   PURE subroutine set_bottom_info(self,info,flip)
   ! Set the .skip_bit_string based on when there is information from the
   ! adjoining capping square.
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      VEC(INT,0:7), IN :: info
      BIN, IN :: flip
      INT :: ind
      if (flip) then

         ! set top of square to bottom of info
         ind = info(4)
         if (ind>0) then
            self%edge_vertex_index(7) = ind
            self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,7)
         end if
         ind = info(5)
         if (ind>0) then
            self%edge_vertex_index(6) = ind
            self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,6)
         end if
      else

         ! set bottom of square to top of info
         ind = info(7)
         if (ind>0) then
            self%edge_vertex_index(4) = ind
            self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,4)
         end if
         ind = info(6)
         if (ind>0) then
            self%edge_vertex_index(5) = ind
            self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,5)
         end if
      end if
   end subroutine

   PURE subroutine set_left_info(self,info,flip)
   ! Set the .skip_bit_string based on when there is information from the
   ! adjoining capping square.
      type(CAPPING_SQUARE_TYPE), INOUT :: self
      VEC(INT,0:7), IN :: info
      BIN, IN :: flip
      INT :: ind

      ! Reverse triangle order
      if (flip) then

         ! set right side of square to left of info
         ind = info(4)
         if (ind>0) then
            self%edge_vertex_index(5) = ind
            self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,5)
         end if
         ind = info(7)
         if (ind>0) then
            self%edge_vertex_index(6) = ind
            self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,6)
         end if
      else

         ! set left side of square to right of info
         ind = info(5)
         if (ind>0) then
            self%edge_vertex_index(4) = ind
            self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,4)
         end if
         ind = info(6)
         if (ind>0) then
            self%edge_vertex_index(7) = ind
            self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,7)
         end if
      end if
   end subroutine


!  =======
!  Inquiry
!  =======

   PURE function case_number(self) result (res)
   ! From the vertex cap flags on the capping square evaluate and
   ! return the case number (this tells us which vertices are larger
   ! than the .iso_value).
      type(CAPPING_SQUARE_TYPE), IN :: self
      INT :: res
      res = 0
      if (self%cap_vertex_flag(0)) res = SET_BIT_AT_POS_IN(res,0)
      if (self%cap_vertex_flag(1)) res = SET_BIT_AT_POS_IN(res,1)
      if (self%cap_vertex_flag(2)) res = SET_BIT_AT_POS_IN(res,2)
      if (self%cap_vertex_flag(3)) res = SET_BIT_AT_POS_IN(res,3)
   end function

   PURE function no_of_triangles(self,case) result (res)
   ! Return the number of triangles for a capping square with a
   ! particular "case" number.
      type(CAPPING_SQUARE_TYPE), IN :: self
      INT, optional, IN :: case
      INT :: res
      INT :: e,c
      VEC(INT,0:11) :: triangle_edge_index
      c = self%case
      if (present(case)) c = case
      if (c<0 OR c>15) then

         ! Outside acceptable range
         res = 0
      else

         ! Loop over triangle edge triples
         triangle_edge_index = triangle_table(:,c)
         res = 0
         do e = 0,11,3
            if (triangle_edge_index(e)==-1) exit
            res = res+1
         end do
      end if
   end function

   PURE function no_of_new_vertices(self) result (res)
   ! Returns the number of new vertices that capping has introduced.
   ! These only occur at the vertices of the square and don't include
   ! the edge vertices acquired from the marching cube and which we
   ! "stitch" to.
      type(CAPPING_SQUARE_TYPE), IN :: self
      INT :: res
      res = self%no_new_vertex
   end function


!  ======
!  Output
!  ======

   subroutine put_case_info(self)
   ! Prints out the case number for the capping square
      type(CAPPING_SQUARE_TYPE), IN :: self
      call show_(stdout,"Capping case number =",self%case)
   end subroutine

end module
